;;; ygg-special-modes-modal-tests.el --- Special-mode buffers that gain the modal layer -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(setq user-emacs-directory (file-name-as-directory (make-temp-file "ygg-special" t)))
(require 'yggdrasil)
(require 'aob-btw)
(require 'aob-bang)
(require 'ygg-compose-preview)
(require 'layer-ui)
(setq kill-emacs-hook nil)
(yggdrasil-global-mode 1)

(defun ygg-special-modes-tests--cmd (key)
  "The command KEY runs here, menu-item and label wrappers taken off."
  (pcase (key-binding (kbd key) t)
    (`(menu-item ,_ ,def . ,_) def)
    ((and `(,label . ,def) (guard (stringp label))) def)
    (def def)))

(defmacro ygg-special-modes-tests--in (mode &rest body)
  "Run BODY in a MODE buffer after the activation the global mode would run."
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     (let ((inhibit-read-only t)) (insert "one\ntwo\n"))
     (goto-char (point-min))
     (ygg--maybe-activate)
     ,@body))

(ert-deftest ygg-special-modes-get-the-modal-layer ()
  (let ((motion (with-temp-buffer
                  (text-mode)
                  (yggdrasil-local-mode 1)
                  (ygg-special-modes-tests--cmd "j"))))
    (should (commandp motion))
    (should-not (eq motion 'next-line))
    (dolist (mode '(aob-btw-mode aob-bang-mode ygg-compose-preview-mode
                    ygg-notify-history-mode))
      (ygg-special-modes-tests--in mode
        (should yggdrasil-local-mode)
        (should (eq ygg--state 'normal))
        (should (eq (ygg-special-modes-tests--cmd "j") motion))
        (should (eq (ygg-special-modes-tests--cmd "q") (lookup-key special-mode-map "q")))))))

(ert-deftest ygg-compose-preview-localleader-reaches-rerender ()
  (ygg-special-modes-tests--in 'ygg-compose-preview-mode
    (should (eq (key-binding (kbd "\\ r") t) 'ygg-compose-preview-rerender))))

(provide 'ygg-special-modes-modal-tests)
;;; ygg-special-modes-modal-tests.el ends here
