;;; ygg-steal-leader-tests.el --- Buffer, insert and help leader maps, vertico-directory, dabbrev guard -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'vertico)
(require 'vertico-directory)
(require 'dabbrev)
(require 'layer-completion)

(defun ygg-steal-leader-tests--cmd (keys)
  (let ((def (lookup-key ygg-leader-map (kbd keys))))
    (if (and (consp def) (stringp (car def))) (cdr def) def)))

(defconst ygg-steal-leader-tests--keys
  '(("b d" . yggdrasil-leader--kill-buffer)
    ("b k" . yggdrasil-leader--kill-choose)
    ("b n" . next-buffer)
    ("b p" . previous-buffer)
    ("b s" . save-buffer)
    ("b S" . save-some-buffers)
    ("b r" . revert-buffer)
    ("b R" . rename-buffer)
    ("b l" . mode-line-other-buffer)
    ("b x" . scratch-buffer)
    ("b z" . bury-buffer)
    ("b m" . bookmark-set)
    ("b M" . ygg-leader--bookmark-jump)
    ("i s" . ygg-tempel-insert)
    ("i i" . ygg-tempel-insert)
    ("i f" . ygg-insert-path-relative)
    ("i F" . ygg-insert-path-absolute)
    ("i u" . insert-char)
    ("i y" . consult-yank-pop)
    ("i r" . consult-register-load)
    ("h f" . describe-function)
    ("h v" . describe-variable)
    ("h k" . describe-key)
    ("h F" . describe-face)
    ("h m" . describe-mode)
    ("h t" . load-theme)
    ("h p" . describe-package)
    ("h a" . apropos)
    ("h i" . info)
    ("h B b" . which-key-show-top-level)
    ("h B m" . which-key-show-major-mode)
    ("h B k" . which-key-show-keymap)
    ("h o" . describe-symbol)
    ("h b" . describe-bindings)
    ("?" . ygg-keys)))

(ert-deftest ygg-steal-leader-keys-resolve ()
  (dolist (entry ygg-steal-leader-tests--keys)
    (should (eq (cdr entry) (ygg-steal-leader-tests--cmd (car entry))))))

(ert-deftest ygg-steal-leader-prefixes-are-maps ()
  (dolist (key '("b" "i" "h" "h B"))
    (should (keymapp (ygg-steal-leader-tests--cmd key)))))

(ert-deftest ygg-steal-vertico-directory-keys ()
  (should (eq (lookup-key vertico-map (kbd "DEL")) #'vertico-directory-delete-char))
  (should (eq (lookup-key vertico-map (kbd "M-DEL")) #'vertico-directory-delete-word))
  (should (memq #'vertico-directory-tidy rfn-eshadow-update-overlay-hook)))

(ert-deftest ygg-steal-dabbrev-guard ()
  (let ((small (generate-new-buffer " small"))
        (big (generate-new-buffer " big"))
        (other (generate-new-buffer " other"))
        (same (generate-new-buffer " same")))
    (unwind-protect
        (progn
          (with-current-buffer big
            (insert (make-string (* 2 1024 1024) ?a)))
          (should (funcall dabbrev-friend-buffer-function small))
          (should-not (funcall dabbrev-friend-buffer-function big))
          (should (ygg-dabbrev-friend-buffer-p small))
          (should-not (ygg-dabbrev-friend-buffer-p big))
          (with-current-buffer other
            (emacs-lisp-mode))
          (with-current-buffer small
            (text-mode))
          (with-current-buffer big
            (text-mode))
          (with-current-buffer same
            (text-mode))
          (with-current-buffer small
            (should-not (ygg-dabbrev-friend-buffer-p other))
            (should (ygg-dabbrev-friend-buffer-p same))
            (should-not (ygg-dabbrev-friend-buffer-p big))))
      (mapc #'kill-buffer (list small big other same)))))

(provide 'ygg-steal-leader-tests)
;;; ygg-steal-leader-tests.el ends here
