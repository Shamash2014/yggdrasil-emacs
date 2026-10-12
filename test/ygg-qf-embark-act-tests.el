;;; ygg-qf-embark-act-tests.el --- , on a quickfix row acts through embark -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'grep)
(yggdrasil-global-mode 1)
(require 'layer-quickfix)
(require 'embark)
(require 'ygg-embark)

(defmacro ygg-qf-embark-act-tests--with (&rest body)
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "qfact-" t)))
          (default-directory dir)
          (buf (generate-new-buffer "*qfact*")))
     (dolist (f '("a.txt" "b.txt" "c.txt"))
       (write-region "x\n" nil (expand-file-name f dir)))
     (unwind-protect
         (progn
           (set-window-buffer (selected-window) buf)
           (with-current-buffer buf
             (grep-mode)
             (let ((inhibit-read-only t))
               (dolist (f '("a.txt" "b.txt" "c.txt"))
                 (insert (propertize (format "%s:1:x\n" f) 'ygg-qf-row t))))
             (goto-char (point-min))
             (ygg-normal-state)
             ,@body))
       (kill-buffer buf)
       (delete-directory dir t))))

(ert-deftest ygg-qf-embark-act-comma-on-a-row-acts-on-that-row ()
  (ygg-qf-embark-act-tests--with
    (forward-line 1)
    (let (seen)
      (cl-letf (((symbol-function 'embark-act)
                 (lambda (&optional _) (setq seen (embark--targets)))))
        (execute-kbd-macro (kbd ",")))
      (should (eq 'ygg-qf-row (plist-get (car seen) :type)))
      (should (string-suffix-p "b.txt:1" (plist-get (car seen) :target))))))

(ert-deftest ygg-qf-embark-act-comma-in-visual-reaches-every-selected-row ()
  (ygg-qf-embark-act-tests--with
    (let (hit)
      (defalias 'ygg-qf-embark-act-tests--verb
        (lambda () (interactive) (push (line-number-at-pos) hit)))
      (cl-letf (((symbol-function 'embark--prompt)
                 (lambda (&rest _) #'ygg-qf-embark-act-tests--verb))
                ((symbol-function 'embark--confirm) #'ignore)
                (embark-confirm-act-all nil))
        (ygg-visual-state)
        (forward-line 1)
        (execute-kbd-macro (kbd ",")))
      (should (equal '(1 2) (sort hit #'<))))))

(provide 'ygg-qf-embark-act-tests)
;;; ygg-qf-embark-act-tests.el ends here
