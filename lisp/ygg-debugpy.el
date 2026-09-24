;;; ygg-debugpy.el --- Python debugging on dape's debugpy configs -*- lexical-binding: t; -*-

;; The adapter may run on any debugpy; the launch request's python decides the debuggee's.

;;; Code:

(require 'seq)
(require 'map)
(require 'cl-lib)
(require 'subr-x)
(require 'treesit)
(require 'compile)
(require 'ygg-kernel-picker)

(defvar dape-configs)
(defvar dape-history)
(declare-function dape "dape")
(declare-function dape-buffer-default "dape")
(declare-function dape-config-get "dape")
(declare-function dape--config-eval "dape")
(declare-function dape--config-eval-1 "dape")
(declare-function dape--live-connections-root "dape")
(declare-function dape--kill-busy-wait "dape")

(defgroup ygg-debugpy nil
  "Python debugging with debugpy through dape."
  :group 'tools
  :prefix "ygg-debugpy-")

(defcustom ygg-debugpy-adapter-pythons
  '("~/.local/share/uv/tools/debugpy/bin/python"
    "~/.local/share/jupyter-python/bin/python")
  "Interpreters tried in order to run the adapter when the project has no debugpy."
  :type '(repeat file))

(defcustom ygg-debugpy-just-my-code t
  "Non-nil steps and stops only in the project's own code, not in libraries."
  :type 'boolean)

(defvar ygg-debugpy-importable-function #'ygg-debugpy--importable-p
  "Called with an interpreter path; non-nil when it can import the adapter.")

(defvar ygg-debugpy--importable (make-hash-table :test #'equal)
  "Interpreter to the debugpy directory it imported, trusted while it exists.")

(defun ygg-debugpy--importable-p (python)
  "Whether PYTHON can import the debugpy adapter."
  (let ((known (gethash python ygg-debugpy--importable)))
    (if (and known (file-directory-p known))
        t
      (remhash python ygg-debugpy--importable)
      (when (file-executable-p python)
        (with-temp-buffer
          (when (zerop (call-process python nil t nil "-c"
                                     "import debugpy.adapter, os; print(os.path.dirname(debugpy.__file__))"))
            (puthash python (string-trim (buffer-string)) ygg-debugpy--importable)
            t))))))

;;; The project's interpreter

(defun ygg-debugpy-venv ()
  "The project's venv directory, or nil."
  (car (ygg-kernel-picker--project-venvs)))

(defun ygg-debugpy--venv-python (venv)
  "VENV's interpreter, unresolved so the venv stays active."
  (expand-file-name "bin/python" venv))

(defun ygg-debugpy--python (venv)
  "The interpreter the program runs on: VENV's, else python3 on PATH."
  (if venv
      (ygg-debugpy--venv-python venv)
    (or (executable-find "python3")
        (user-error "No project venv and no python3 on PATH"))))

(defun ygg-debugpy-adapter-python (python)
  "An interpreter that runs the adapter: PYTHON itself if it has debugpy."
  (or (seq-find (lambda (candidate)
                  (funcall ygg-debugpy-importable-function candidate))
                (cons python (mapcar #'expand-file-name ygg-debugpy-adapter-pythons)))
      (user-error "No debugpy found; run ygg-debugpy-add-to-project or uv tool install debugpy")))

;;; What to run

(defun ygg-debugpy-module-name ()
  "The dotted module the visited file is, as python -m names it."
  (let* ((path (file-name-sans-extension (dape-buffer-default)))
         (parts (split-string (string-remove-prefix "src/" path) "/")))
    (when (equal (car (last parts)) "__main__")
      (unless (cdr parts)
        (user-error "A top-level __main__.py has no module name; debug it as a file"))
      (setq parts (butlast parts)))
    (string-join parts ".")))

(defun ygg-debugpy--definition (node)
  "NODE as a class or function definition, seeing through a decorator wrapper."
  (pcase (treesit-node-type node)
    ((or "class_definition" "function_definition") node)
    ("decorated_definition" (treesit-node-child-by-field-name node "definition"))))

(defun ygg-debugpy-test-names ()
  "Classes and the test enclosing point, outermost first.
Stops at the outermost function, since pytest collects nothing inside one."
  (unless (treesit-language-available-p 'python)
    (user-error "No python tree-sitter grammar"))
  (let* ((parser (treesit-parser-create 'python))
         (pos (save-excursion (skip-chars-forward " \t") (point)))
         (node (treesit-node-at pos parser))
         definitions)
    (while node
      (when-let* ((definition (ygg-debugpy--definition node)))
        (unless (and definitions (treesit-node-eq definition (car definitions)))
          (push definition definitions)))
      (setq node (treesit-node-parent node)))
    (let (names)
      (catch 'done
        (dolist (definition definitions)
          (push (treesit-node-text (treesit-node-child-by-field-name definition "name") t)
                names)
          (when (equal (treesit-node-type definition) "function_definition")
            (throw 'done nil))))
      (nreverse names))))

(defun ygg-debugpy-test-node-id ()
  "The pytest node id of the test or class at point, else of the file."
  (mapconcat #'identity (cons (dape-buffer-default) (ygg-debugpy-test-names)) "::"))

(defun ygg-debugpy-test-args ()
  (vector (ygg-debugpy-test-node-id)))

;;; Resolving a config at launch

(defconst ygg-debugpy--launch-only '(:program :module :code :args :console)
  "Keys a launch base carries that an attach request must not.")

(defun ygg-debugpy--settle (config)
  "CONFIG evaluated as dape's prompt would, keeping its most specific target.
A launch.json base reaches dape unevaluated, and an attach from it
drops the launch keys it inherited."
  (let* ((attach (equal (plist-get config :request) "attach"))
         (targets '(:code :module :program))
         (keep (seq-find (lambda (key) (plist-get config key)) targets)))
    (dape--config-eval-1
     (cl-loop for (key value) on config by #'cddr
              unless (or (and attach (memq key ygg-debugpy--launch-only))
                         (and (memq key targets) (not (eq key keep))))
              append (list key value)))))

(defun ygg-debugpy--given-p (config key)
  "Whether CONFIG sets KEY to something other than an unexpanded variable."
  (let ((value (plist-get config key)))
    (and value (not (and (stringp value) (string-match-p "\\${" value))))))

(defun ygg-debugpy--venv-env (venv env)
  "ENV with VENV active, so the program's subprocesses stay in it."
  (append (unless (plist-member env :VIRTUAL_ENV)
            (list :VIRTUAL_ENV (directory-file-name venv)))
          (unless (plist-member env :PATH)
            (list :PATH (concat (expand-file-name "bin" venv) path-separator (getenv "PATH"))))
          env))

(defun ygg-debugpy-resolve (config)
  "CONFIG with the adapter picked and the program run on the project venv."
  (let* ((config (ygg-debugpy--settle config))
         (launch (not (equal (plist-get config :request) "attach")))
         (local (or launch (plist-member config 'command))))
    (when (and local (file-remote-p default-directory))
      (user-error "Remote Python debugging is not supported; attach to debugpy --listen instead"))
    (when local
      (let* ((venv (ygg-debugpy-venv))
             (python (ygg-debugpy--python venv)))
        (when (equal (plist-get config 'command) "python")
          (setq config (plist-put config 'command (ygg-debugpy-adapter-python python)))
          ;; Dape listens on all interfaces for tramp, which is refused above.
          (setq config (plist-put config 'command-args
                                  (cl-substitute "127.0.0.1" "0.0.0.0"
                                                 (plist-get config 'command-args)
                                                 :test #'equal))))
        (when (and launch (not (ygg-debugpy--given-p config :python)))
          (setq config (plist-put config :python python))
          (when venv
            (setq config (plist-put config :env
                                    (ygg-debugpy--venv-env venv (plist-get config :env))))))))
    config))

;;; Extending dape's entries

(defvar ygg-debugpy--dape-ensure nil
  "The ensure dape's debugpy entries shipped with.")

(defun ygg-debugpy-ensure (config)
  "Dape's debugpy check, run only on an adapter command someone chose.
Dape's check probes its placeholder python, and runs in the prompt."
  (unless (equal (dape-config-get config 'command) "python")
    (funcall ygg-debugpy--dape-ensure config)))

(defun ygg-debugpy--extend (plist)
  "Dape's debugpy PLIST with our fn after its own, our ensure and our justMyCode."
  (let* ((fn (plist-get plist 'fn))
         (fns (if (functionp fn) (list fn) fn)))
    (unless (eq (plist-get plist 'ensure) #'ygg-debugpy-ensure)
      (setq ygg-debugpy--dape-ensure (plist-get plist 'ensure)))
    (map-merge 'plist plist
               `(fn ,(seq-uniq (append fns (list #'ygg-debugpy-resolve)))
                 ensure ygg-debugpy-ensure
                 :justMyCode ygg-debugpy-just-my-code))))

(defun ygg-debugpy-install ()
  "Extend dape's debugpy entries, and derive test and attach entries from dape's."
  (dolist (name '(debugpy debugpy-module))
    (when-let* ((plist (alist-get name dape-configs)))
      (setf (alist-get name dape-configs) (ygg-debugpy--extend plist))))
  (when-let* ((module (alist-get 'debugpy-module dape-configs)))
    (setf (alist-get 'debugpy-module dape-configs)
          (map-merge 'plist module '(:module ygg-debugpy-module-name))))
  (when-let* ((debugpy (alist-get 'debugpy dape-configs)))
    (setf (alist-get 'debugpy-test dape-configs)
          (map-merge 'plist (map-delete (copy-sequence debugpy) :program)
                     '(:module "pytest" :args ygg-debugpy-test-args))))
  (when-let* ((attach (alist-get 'attach dape-configs)))
    (setf (alist-get 'debugpy-attach dape-configs)
          (map-merge 'plist attach
                     '(modes (python-mode python-ts-mode) fn ygg-debugpy-resolve port 5678
                       :type "python" :cwd dape-cwd :justMyCode ygg-debugpy-just-my-code)))))

(with-eval-after-load 'dape
  (ygg-debugpy-install))

;;; Commands

(defun ygg-debugpy-toggle-just-my-code ()
  "Switch between stepping only project code and stepping into libraries."
  (interactive)
  (setq ygg-debugpy-just-my-code (not ygg-debugpy-just-my-code))
  (message "debugpy justMyCode %s" (if ygg-debugpy-just-my-code "on" "off")))

(defun ygg-debugpy-add-to-project ()
  "Install debugpy into the project venv with uv, after asking."
  (interactive)
  (let* ((venv (or (ygg-debugpy-venv) (user-error "No project venv here")))
         (default-directory (file-name-directory (directory-file-name venv)))
         (command (if (and (file-exists-p "pyproject.toml")
                           (equal (file-name-nondirectory (directory-file-name venv)) ".venv"))
                      "uv add --dev debugpy"
                    (format "uv pip install --python %s debugpy"
                            (shell-quote-argument (ygg-debugpy--venv-python venv))))))
    (when (yes-or-no-p (format "Run %s in %s? " command default-directory))
      (compilation-start command))))

(provide 'ygg-debugpy)
;;; ygg-debugpy.el ends here
