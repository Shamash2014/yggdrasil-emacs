;;; layer-rass.el --- every eglot server beside harper, through rass -*- lexical-binding: t; -*-

;;; Commentary:
;; Every eglot server runs through rassumfrassum (rass,
;; https://github.com/joaotavora/rassumfrassum) with harper-ls beside it, so
;; comments and docstrings are grammar-checked in every language.  The wrap
;; happens when eglot resolves a contact, so entries registered in any order
;; are covered.  TCP servers, remote buffers, and harper itself stay direct.
;; ygg-rass-disable turns it off live; restart the server to apply either way.

;;; Code:

(require 'cl-lib)
(defvar eglot-server-programs)
(declare-function eglot--major-modes "eglot")

(defconst ygg-rass-harper-preset
  (expand-file-name "../etc/rass-harper.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The rass preset that sends commands it cannot place to the language server.")

(defvar ygg-rass--wrapped-contacts nil
  "Each contact function wrapped through rass, paired with the original.")

(defun ygg-rass--unwrap (cmd)
  "Strip a rass wrapping from CMD, recovering the original server command."
  (let* ((after (cdr (member "--" cmd)))
         (end (cl-position "--" after :test #'equal)))
    (if end (cl-subseq after 0 end) cmd)))

(defun ygg-rass--command (contact)
  (seq-take contact (or (cl-position-if #'keywordp contact) (length contact))))

(defun ygg-rass--options (contact)
  (nthcdr (length (ygg-rass--command contact)) contact))

(defun ygg-rass--wrappable-p (contact)
  "Whether CONTACT is a local stdio command rass may lead."
  (let ((command (ygg-rass--command contact)))
    (and command
         (cl-every #'stringp command)
         (not (plist-member (ygg-rass--options contact) :autoport))
         (not (member (file-name-nondirectory (car command)) '("rass" "harper-ls")))
         (not (file-remote-p default-directory))
         (executable-find "rass")
         t)))

(defun ygg-rass--harper-ready-p ()
  (and (executable-find "harper-ls") (file-exists-p ygg-rass-harper-preset)))

(defun ygg-rass-with-harper (contact)
  "CONTACT, a resolved eglot contact, running through rass beside harper-ls.
Left as is when it is not a local stdio command or rass is missing.
Keyword options such as :initializationOptions stay at the end."
  (if (and (ygg-rass--wrappable-p contact) (ygg-rass--harper-ready-p))
      (ygg-rass--join ygg-rass-harper-preset contact
                      (and (member (file-name-nondirectory (car contact))
                                   '("vscode-html-language-server" "vscode-css-language-server"))
                           (ygg-rass--tailwind-companions default-directory)))
    contact))

(defun ygg-rass--wrap-guess (guess)
  "GUESS, what eglot--guess-contact returns, with its contact beside harper."
  (if (consp (nth 3 guess))
      (append (seq-take guess 3) (list (ygg-rass-with-harper (nth 3 guess))) (nthcdr 4 guess))
    guess))

(defun ygg-rass--guess-contact (fn &optional interactive)
  ;; a command typed at C-u M-x eglot runs as typed
  (let ((guess (funcall fn interactive)))
    (if (and interactive current-prefix-arg) guess (ygg-rass--wrap-guess guess))))

;;;###autoload
(defun ygg-rass-enable ()
  "Run every eglot server through rass beside harper-ls.
A running server keeps its command until it is shut down and started again."
  (interactive)
  (advice-add 'eglot--guess-contact :around #'ygg-rass--guess-contact)
  (ygg-rass-companions-enable))

;;;###autoload
(defun ygg-rass-disable ()
  "Run eglot servers directly again.
A running server keeps its command until it is shut down and started again."
  (interactive)
  (advice-remove 'eglot--guess-contact #'ygg-rass--guess-contact)
  (dolist (entry eglot-server-programs)
    (let ((cmd (cdr entry)))
      (cond ((and (consp cmd) (equal (car cmd) "rass"))
             (setf (cdr entry) (ygg-rass--unwrap cmd)))
            ((assq cmd ygg-rass--wrapped-contacts)
             (setf (cdr entry) (cdr (assq cmd ygg-rass--wrapped-contacts)))))))
  (setq ygg-rass--wrapped-contacts nil)
  (when (called-interactively-p 'interactive)
    (message "layer-rass: servers run directly")))

;;; The project's ESLint beside the TypeScript server

(defconst ygg-rass--eslint-flat-re "\\`eslint\\.config\\.[cm]?[jt]s\\'")
(defconst ygg-rass--eslint-legacy-re "\\`\\.eslintrc\\(?:\\.[a-z]+\\)?\\'")

(defun ygg-rass--package-eslint-p (directory)
  (let ((file (expand-file-name "package.json" directory)))
    (and (file-exists-p file)
         (ignore-errors
           (with-temp-buffer
             (insert-file-contents file)
             (assq 'eslintConfig (json-parse-buffer :object-type 'alist)))))))

(defun ygg-rass-eslint-config (directory)
  "The ESLint config nearest DIRECTORY: flat, legacy, or nil when there is none."
  (let ((kind nil))
    (locate-dominating-file
     directory
     (lambda (dir)
       (setq kind (cond ((directory-files dir nil ygg-rass--eslint-flat-re t) 'flat)
                        ((or (directory-files dir nil ygg-rass--eslint-legacy-re t)
                             (ygg-rass--package-eslint-p dir))
                         'legacy)))))
    kind))

(defun ygg-rass-eslint-p (directory)
  "Whether the project at DIRECTORY lints with an ESLint of its own."
  (and (not (file-remote-p directory))
       (ygg-rass-eslint-config directory)
       (locate-dominating-file directory "node_modules/.bin/eslint")
       t))

(defconst ygg-rass-typescript-preset
  (expand-file-name "../etc/rass-typescript.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The rass preset pairing a TypeScript server with ESLint.")

(defun ygg-rass--resolve (name directory)
  "NAME's binary within the project at DIRECTORY, else NAME itself when on PATH."
  (let ((project (let ((default-directory directory))
                   (and (fboundp 'ygg-format--project-bin) (ygg-format--project-bin name)))))
    (cond ((stringp project) project)
          ((executable-find name) name))))

(defun ygg-rass--biome-command (directory)
  (and (not (file-remote-p directory))
       (locate-dominating-file
        directory
        (lambda (dir) (or (file-exists-p (expand-file-name "biome.json" dir))
                          (file-exists-p (expand-file-name "biome.jsonc" dir)))))
       (and-let* ((biome (ygg-rass--resolve "biome" directory)))
         (list biome "lsp-proxy"))))

(defconst ygg-rass--tailwind-config-re "\\`tailwind\\.config\\.[cm]?[jt]s\\'")

(defun ygg-rass--package-tailwind-p (directory)
  (let ((file (expand-file-name "package.json" directory)))
    (and (file-exists-p file)
         (ignore-errors
           (with-temp-buffer
             (insert-file-contents file)
             (let ((json (json-parse-buffer :object-type 'alist)))
               (seq-some (lambda (key)
                           (seq-some (lambda (dep) (assq dep (alist-get key json)))
                                     '(tailwindcss nativewind)))
                         '(dependencies devDependencies))))))))

(defun ygg-rass--tailwind-p (directory)
  (and (not (file-remote-p directory))
       (executable-find "tailwindcss-language-server")
       (locate-dominating-file
        directory
        (lambda (dir) (or (directory-files dir nil ygg-rass--tailwind-config-re t)
                          (ygg-rass--package-tailwind-p dir))))
       t))

(defun ygg-rass--tailwind-companions (directory)
  (and (ygg-rass--tailwind-p directory) '(("tailwindcss-language-server" "--stdio"))))

(defun ygg-rass--typescript-companions (directory)
  "The commands of the linters and tooling servers the project at DIRECTORY uses."
  (append
   (and (ygg-rass-eslint-p directory)
        (executable-find "vscode-eslint-language-server")
        '(("vscode-eslint-language-server" "--stdio")))
   (and-let* ((biome (ygg-rass--biome-command directory))) (list biome))
   (ygg-rass--tailwind-companions directory)))

(defun ygg-rass--join (preset contact companions)
  "CONTACT through rass with PRESET, COMPANIONS, then harper-ls, options last."
  `("rass" "--no-stream-diagnostics" "--log-level" "warn" ,preset
    "--" ,@(ygg-rass--command contact)
    ,@(mapcan (lambda (companion) (cons "--" (copy-sequence companion))) companions)
    ,@(and (executable-find "harper-ls") '("--" "harper-ls" "--stdio"))
    ,@(ygg-rass--options contact)))

(defun ygg-rass-with-eslint (base)
  "BASE, a TypeScript contact, paired with the project's ESLint, Biome, Tailwind.
Its keyword options, such as :initializationOptions, stay at the end,
where eglot reads them.  TypeScript 7's tsc is never paired."
  (let ((companions (and (ygg-rass--wrappable-p base)
                         (not (equal (file-name-nondirectory (car base)) "tsc"))
                         (ygg-rass--typescript-companions default-directory))))
    (if companions
        (ygg-rass--join (if (assoc "vscode-eslint-language-server" companions)
                            ygg-rass-typescript-preset
                          ygg-rass-harper-preset)
                        base companions)
      base)))

(defun ygg-rass-with-ruff (base)
  "BASE, a Python server contact, paired with `ruff server' when ruff runs."
  (let ((ruff (and (ygg-rass--wrappable-p base)
                   (not (equal (file-name-nondirectory (car base)) "ruff"))
                   (ygg-rass--harper-ready-p)
                   (ygg-rass--resolve "ruff" default-directory))))
    (if ruff
        (ygg-rass--join ygg-rass-harper-preset base (list (list ruff "server")))
      base)))

(defun ygg-rass--typescript-entry-p (entry)
  (and (consp (car entry))
       (cl-some (lambda (mode) (eq (if (consp mode) (car mode) mode) 'tsx-ts-mode))
                (car entry))))

(defun ygg-rass--python-entry-p (entry)
  (and (consp (car entry)) (memq 'python-mode (car entry)) t))

(defun ygg-rass--wrap-entry (entry wrapper)
  (when-let* ((base (cdr entry))
              ((not (assq base ygg-rass--wrapped-contacts)))
              ((or (functionp base) (and (consp base) (stringp (car base))))))
    (let ((wrapped (lambda (&rest args)
                     (funcall wrapper (if (functionp base) (apply base args) base)))))
      (push (cons wrapped base) ygg-rass--wrapped-contacts)
      (setf (cdr entry) wrapped))))

(defun ygg-rass-companions-enable ()
  "Let the TypeScript and Python entries bring companion servers along."
  (when-let* ((entry (cl-find-if #'ygg-rass--typescript-entry-p eglot-server-programs)))
    (ygg-rass--wrap-entry entry #'ygg-rass-with-eslint))
  (when-let* ((entry (cl-find-if #'ygg-rass--python-entry-p eglot-server-programs)))
    (ygg-rass--wrap-entry entry #'ygg-rass-with-ruff)))

(define-obsolete-function-alias 'ygg-rass-eslint-enable #'ygg-rass-companions-enable "2026-10")

(defun ygg-rass-typescript-configuration (server)
  "ESLint settings for SERVER when it serves TypeScript.
ESLint reads an eslintrc only when told, which ESLint 9 no longer does
by itself."
  (when (and (memq 'tsx-ts-mode (eglot--major-modes server))
             (eq (ygg-rass-eslint-config default-directory) 'legacy))
    (list (intern ":") '(:useFlatConfig :json-false))))

(defun ygg-rass--with-typescript-configuration (configuration server)
  (append (funcall configuration server) (ygg-rass-typescript-configuration server)))

(defun ygg-rass--runs-harper-p (server)
  (when-let* ((proc (ignore-errors (jsonrpc--process server))))
    (seq-some (lambda (arg) (equal (file-name-nondirectory arg) "harper-ls"))
              (process-command proc))))

(defun ygg-rass--with-harper-configuration (configuration server)
  "CONFIGURATION for SERVER, with harper-ls's empty settings beside its own.
Harper behind rass is asked by section, and the language server leading the
process is what picks the settings otherwise."
  (let ((own (funcall configuration server)))
    (if (and (ygg-rass--runs-harper-p server) (listp own) (not (plist-member own :harper-ls)))
        (append own (list :harper-ls (make-hash-table :test 'equal)))
      own)))

(with-eval-after-load 'layer-lsp
  (advice-add 'ygg-lsp-workspace-configuration :around
              #'ygg-rass--with-typescript-configuration)
  (advice-add 'ygg-lsp-workspace-configuration :around
              #'ygg-rass--with-harper-configuration '((depth . -100))))

;; run after layer-lsp has registered the base entries (require it later)
(with-eval-after-load 'eglot
  (ygg-rass-enable))

(provide 'layer-rass)
;;; layer-rass.el ends here
