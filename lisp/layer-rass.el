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

(defun ygg-rass-with-harper (contact)
  "CONTACT, a resolved eglot contact, running through rass beside harper-ls.
Left as is when it is not a local stdio command or rass is missing.
Keyword options such as :initializationOptions stay at the end."
  (let* ((split (or (cl-position-if #'keywordp contact) (length contact)))
         (command (seq-take contact split))
         (options (nthcdr split contact)))
    (if (and command
             (cl-every #'stringp command)
             (not (plist-member options :autoport))
             (not (member (file-name-nondirectory (car command)) '("rass" "harper-ls")))
             (not (file-remote-p default-directory))
             (executable-find "rass")
             (executable-find "harper-ls")
             (file-exists-p ygg-rass-harper-preset))
        `("rass" "--no-stream-diagnostics" "--log-level" "warn" ,ygg-rass-harper-preset
          "--" ,@command "--" "harper-ls" "--stdio" ,@options)
      contact)))

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
  (ygg-rass-eslint-enable))

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

(defun ygg-rass-with-eslint (base)
  "BASE, a TypeScript server contact, paired with the project's ESLint.
Its keyword options, such as :initializationOptions, stay at the end,
where eglot reads them.  TypeScript 7's tsc is never paired."
  (let* ((split (or (cl-position-if #'keywordp base) (length base)))
         (command (seq-take base split))
         (options (nthcdr split base)))
    (if (and command
             (not (equal (file-name-nondirectory (car command)) "tsc"))
             (ygg-rass-eslint-p default-directory)
             (executable-find "rass")
             (executable-find "vscode-eslint-language-server"))
        `("rass" "--no-stream-diagnostics" "--log-level" "warn" ,ygg-rass-typescript-preset
          "--" ,@command "--" "vscode-eslint-language-server" "--stdio"
          ,@(and (executable-find "harper-ls") '("--" "harper-ls" "--stdio"))
          ,@options)
      base)))

(defun ygg-rass--typescript-entry-p (entry)
  (and (consp (car entry))
       (cl-some (lambda (mode) (eq (if (consp mode) (car mode) mode) 'tsx-ts-mode))
                (car entry))))

(defun ygg-rass-eslint-enable ()
  "Let the TypeScript server entry bring the project's ESLint along, in place."
  (when-let* ((entry (cl-find-if #'ygg-rass--typescript-entry-p eglot-server-programs))
              (base (cdr entry))
              ((not (assq base ygg-rass--wrapped-contacts)))
              ((or (functionp base) (and (consp base) (stringp (car base))))))
    (let ((wrapped (lambda (&rest args)
                     (ygg-rass-with-eslint (if (functionp base) (apply base args) base)))))
      (push (cons wrapped base) ygg-rass--wrapped-contacts)
      (setf (cdr entry) wrapped))))

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
