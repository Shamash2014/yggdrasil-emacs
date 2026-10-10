;;; ygg-steal-core-tests.el --- Startup, so-long and platform defaults -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'saveplace)

(defconst ygg-steal-core-tests--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun ygg-steal-core-tests--forms (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file ygg-steal-core-tests--root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun ygg-steal-core-tests--eval-named (file names)
  (dolist (form (ygg-steal-core-tests--forms file))
    (when (and (memq (car-safe form) '(defun defvar-local defcustom defvar))
               (memq (cadr form) names))
      (eval form t))))

(ert-deftest ygg-steal-core-parse ()
  (dolist (f '("early-init.el" "init.el"))
    (should (ygg-steal-core-tests--forms f))))

(ert-deftest ygg-steal-core-handler-merge ()
  (ygg-steal-core-tests--eval-named "early-init.el" '(ygg--merge-file-name-handlers))
  (let* ((saved '(("\\.gz\\'" . jka-handler) ("\\`/ssh:" . tramp-handler)))
         (current '(("\\.late\\'" . late-handler) ("\\.gz\\'" . jka-handler)))
         (merged (ygg--merge-file-name-handlers current saved)))
    (should (assoc "\\.late\\'" merged))
    (should (assoc "\\`/ssh:" merged))
    (should (= 1 (cl-count "\\.gz\\'" merged :key #'car :test #'equal)))))

(ert-deftest ygg-steal-core-so-long-predicate ()
  (require 'so-long)
  (ygg-steal-core-tests--eval-named
   "init.el" '(ygg-so-long-line-threshold ygg--so-long-fns ygg-so-long-p))
  (with-temp-buffer
    (rename-buffer "long-line" t)
    (insert (make-string 20000 ?x))
    (should (ygg-so-long-p))
    (should (eq (car ygg--so-long-fns) 'so-long-minor-mode)))
  (with-temp-buffer
    (rename-buffer "many-short" t)
    (dotimes (_ 40000) (insert "short line\n"))
    (should-not (ygg-so-long-p)))
  (with-temp-buffer
    (rename-buffer "huge-line" t)
    (insert (make-string 60000 ?x))
    (should (ygg-so-long-p))
    (should (eq (car ygg--so-long-fns) 'so-long-mode))))

(ert-deftest ygg-steal-core-so-long-choice-survives-the-mode-switch ()
  (ygg-steal-core-tests--eval-named "init.el" '(ygg--so-long-fns))
  (dolist (form (ygg-steal-core-tests--forms "init.el"))
    (when (equal form '(put 'ygg--so-long-fns 'permanent-local t))
      (eval form t)))
  (with-temp-buffer
    (setq ygg--so-long-fns '(so-long-mode . so-long-mode-revert))
    (fundamental-mode)
    (should (equal ygg--so-long-fns '(so-long-mode . so-long-mode-revert)))))

(ert-deftest ygg-steal-core-save-place-file-readable ()
  (let* ((save-place-file (make-temp-file "ygg-saveplace"))
         (save-place-alist '(("/tmp/a.el" . 10) ("/tmp/b.el" . 20)))
         (save-place-forget-unreadable-files nil))
    (unwind-protect
        (progn
          (save-place-alist-to-file)
          (with-temp-buffer
            (insert-file-contents save-place-file)
            (should (equal (read (current-buffer)) save-place-alist))))
      (delete-file save-place-file))))

(provide 'ygg-steal-core-tests)
