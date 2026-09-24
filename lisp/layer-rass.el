;;; layer-rass.el --- multiplex eglot servers with harper via rass -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in multi-LSP.  Eglot is one-server-per-buffer; rassumfrassum (`rass',
;; https://github.com/joaotavora/rassumfrassum) presents several servers as
;; one, so harper-ls grammar-checks comments/docstrings alongside the real
;; language server in the same buffer.
;;
;; Enable/disable: add or remove `(require 'layer-rass)' in init.el (then
;; restart), or toggle live with `ygg-rass-enable' / `ygg-rass-disable'.
;; rass is 0.3.x and no-warranty, and sits between eglot and every routed
;; server, so `ygg-rass-modes' is kept to a few well-behaved single-binary
;; servers — JS-land servers need rass hooking presets and are left out.

;;; Code:

(require 'cl-lib)
(defvar eglot-server-programs)
(declare-function eglot--major-modes "eglot")

(defcustom ygg-rass-modes
  '(python-ts-mode python-mode elixir-ts-mode elixir-mode heex-ts-mode
    dart-mode dart-ts-mode kotlin-mode kotlin-ts-mode swift-mode swift-ts-mode)
  "Major modes whose eglot server is multiplexed with harper-ls via rass."
  :type '(repeat symbol) :group 'eglot)

(defun ygg-rass--modes (entry)
  (let ((k (car entry))) (if (listp k) k (list k))))

(defun ygg-rass--wrap (cmd)
  "Wrap plain server command CMD through rass + harper-ls.
Eglot 1.24.31, in Emacs 31, advertises and merges $/streamDiagnostics,
and rass 0.3.4 streams whenever the client advertises it, so
`--no-stream-diagnostics' only keeps an eglot without that handler on
standard `textDocument/publishDiagnostics'."
  (append '("rass" "--no-stream-diagnostics" "--") cmd '("--" "harper-ls" "--stdio")))

(defvar ygg-rass--wrapped-contacts nil
  "Each contact function wrapped through rass, paired with the original.")

(defun ygg-rass--wrap-contact (contact)
  "CONTACT, a function eglot asks for a command, answering through rass."
  (let ((wrapped (lambda (&rest args)
                   (let ((cmd (apply contact args)))
                     (if (and (consp cmd) (cl-every #'stringp cmd)
                              (not (member "rass" cmd)))
                         (ygg-rass--wrap cmd)
                       cmd)))))
    (push (cons wrapped contact) ygg-rass--wrapped-contacts)
    wrapped))

(defun ygg-rass--unwrap (cmd)
  "Strip a rass wrapping from CMD, recovering the original server command."
  (let* ((after (cdr (member "--" cmd)))
         (end (cl-position "--" after :test #'equal)))
    (if end (cl-subseq after 0 end) cmd)))

;;;###autoload
(defun ygg-rass-enable ()
  "Route `ygg-rass-modes' servers through rass + harper-ls, in place.
Only wraps an entry when rass, harper-ls, and the base server are all on
PATH and the entry is a plain command list, or a named contact function
whose command is wrapped when eglot asks for it; otherwise it is left
as-is, so a missing binary never kills a language's LSP.
Reconnect eglot to apply."
  (interactive)
  (if (not (and (executable-find "rass") (executable-find "harper-ls")))
      (when (called-interactively-p 'interactive)
        (message "layer-rass: rass or harper-ls not on PATH — nothing wrapped"))
    (let ((n 0))
      (dolist (entry eglot-server-programs)
        (let ((cmd (cdr entry)))
          (when (cl-intersection (ygg-rass--modes entry) ygg-rass-modes)
            (cond ((and (consp cmd) (cl-every #'stringp cmd)
                        (not (member "rass" cmd))
                        (executable-find (car cmd)))
                   (setf (cdr entry) (ygg-rass--wrap cmd))
                   (cl-incf n))
                  ((and (symbolp cmd) (fboundp cmd))
                   (setf (cdr entry) (ygg-rass--wrap-contact cmd))
                   (cl-incf n))))))
      (when (called-interactively-p 'interactive)
        (message "layer-rass: wrapped %d server(s)" n))
      n)))

;;;###autoload
(defun ygg-rass-disable ()
  "Restore the un-wrapped server commands.  Reconnect eglot to apply."
  (interactive)
  (dolist (entry eglot-server-programs)
    (let ((cmd (cdr entry)))
      (cond ((and (consp cmd) (equal (car cmd) "rass"))
             (setf (cdr entry) (ygg-rass--unwrap cmd)))
            ((assq cmd ygg-rass--wrapped-contacts)
             (setf (cdr entry) (cdr (assq cmd ygg-rass--wrapped-contacts)))))))
  (setq ygg-rass--wrapped-contacts nil)
  (when (called-interactively-p 'interactive)
    (message "layer-rass: servers un-wrapped")))

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
        `("rass" "--no-stream-diagnostics" ,ygg-rass-typescript-preset
          "--" ,@command "--" "vscode-eslint-language-server" "--stdio"
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

(with-eval-after-load 'layer-lsp
  (advice-add 'ygg-lsp-workspace-configuration :around
              #'ygg-rass--with-typescript-configuration))

;; run after layer-lsp has registered the base entries (require it later)
(with-eval-after-load 'eglot
  (ygg-rass-enable)
  (ygg-rass-eslint-enable))

(provide 'layer-rass)
;;; layer-rass.el ends here
