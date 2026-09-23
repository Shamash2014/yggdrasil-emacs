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

(defcustom ygg-rass-modes
  '(python-ts-mode python-mode elixir-ts-mode elixir-mode heex-ts-mode
    dart-mode dart-ts-mode kotlin-mode kotlin-ts-mode swift-mode swift-ts-mode)
  "Major modes whose eglot server is multiplexed with harper-ls via rass."
  :type '(repeat symbol) :group 'eglot)

(defun ygg-rass--modes (entry)
  (let ((k (car entry))) (if (listp k) k (list k))))

(defun ygg-rass--wrap (cmd)
  "Wrap plain server command CMD through rass + harper-ls.
`--no-stream-diagnostics' because stock eglot has no $/streamDiagnostics
handler, so rass must send standard `textDocument/publishDiagnostics'."
  (append '("rass" "--no-stream-diagnostics" "--") cmd '("--" "harper-ls" "--stdio")))

(defun ygg-rass--unwrap (cmd)
  "Strip a rass wrapping from CMD, recovering the original server command."
  (let* ((after (cdr (member "--" cmd)))
         (end (cl-position "--" after :test #'equal)))
    (if end (cl-subseq after 0 end) cmd)))

;;;###autoload
(defun ygg-rass-enable ()
  "Route `ygg-rass-modes' servers through rass + harper-ls, in place.
Only wraps an entry when rass, harper-ls, and the base server are all on
PATH and the entry is a plain command list; otherwise it is left as-is, so
a missing binary never kills a language's LSP.  Reconnect eglot to apply."
  (interactive)
  (if (not (and (executable-find "rass") (executable-find "harper-ls")))
      (when (called-interactively-p 'interactive)
        (message "layer-rass: rass or harper-ls not on PATH — nothing wrapped"))
    (let ((n 0))
      (dolist (entry eglot-server-programs)
        (let ((cmd (cdr entry)))
          (when (and (cl-intersection (ygg-rass--modes entry) ygg-rass-modes)
                     (consp cmd) (cl-every #'stringp cmd)
                     (not (member "rass" cmd))
                     (executable-find (car cmd)))
            (setf (cdr entry) (ygg-rass--wrap cmd))
            (cl-incf n))))
      (when (called-interactively-p 'interactive)
        (message "layer-rass: wrapped %d server(s)" n))
      n)))

;;;###autoload
(defun ygg-rass-disable ()
  "Restore the un-wrapped server commands.  Reconnect eglot to apply."
  (interactive)
  (dolist (entry eglot-server-programs)
    (let ((cmd (cdr entry)))
      (when (and (consp cmd) (equal (car cmd) "rass"))
        (setf (cdr entry) (ygg-rass--unwrap cmd)))))
  (when (called-interactively-p 'interactive)
    (message "layer-rass: servers un-wrapped")))

;; run after layer-lsp has registered the base entries (require it later)
(with-eval-after-load 'eglot (ygg-rass-enable))

(provide 'layer-rass)
;;; layer-rass.el ends here
