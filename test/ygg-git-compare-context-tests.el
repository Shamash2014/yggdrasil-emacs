;;; ygg-git-compare-context-tests.el --- more context around a compare's hunks -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(let ((builds (expand-file-name "../elpaca/builds/"
                                (file-name-directory
                                 (or load-file-name buffer-file-name)))))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds))))

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-compare-comments)
(require 'ygg-git-compare-context)

(defun ygg-git-compare-context-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-compare-context-tests--lines (&optional changed)
  (mapconcat (lambda (n) (if (and changed (= n 20)) "twenty\n" (format "l%d\n" n)))
             (number-sequence 1 40) ""))

(defmacro ygg-git-compare-context-tests--with-compare (root &rest body)
  "BODY in a compare of main and feature, which rewrites line 20 of a 40 line a.txt."
  (declare (indent 1))
  `(let* ((process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (,root (file-name-as-directory
                  (file-truename (make-temp-file "ygg-git-compare-context-" t))))
          (default-directory ,root)
          (magit-refresh-verbose nil)
          (transient-mark-mode t))
     (unwind-protect
         (cl-flet ((git (&rest args) (apply #'ygg-git-compare-context-tests--git ,root args)))
           (git "init" "-q" "-b" "main")
           (git "config" "user.name" "Context Test")
           (git "config" "user.email" "context@example.invalid")
           (git "config" "commit.gpgsign" "false")
           (with-temp-file (expand-file-name "a.txt" ,root)
             (insert (ygg-git-compare-context-tests--lines)))
           (git "add" ".")
           (git "commit" "-q" "-m" "base")
           (git "checkout" "-q" "-b" "feature")
           (with-temp-file (expand-file-name "a.txt" ,root)
             (insert (ygg-git-compare-context-tests--lines t)))
           (git "add" ".")
           (git "commit" "-q" "-m" "feature")
           (git "checkout" "-q" "main")
           (with-current-buffer (ygg-git-compare-buffer ,root '(rev . "main") '(rev . "feature"))
             (magit-section-show-level-4-all)
             ,@body))
       (dolist (b (buffer-list))
         (when (or (string-prefix-p "compose:review:" (buffer-name b))
                   (and (with-current-buffer b (derived-mode-p 'magit-mode))
                        (file-in-directory-p (buffer-local-value 'default-directory b)
                                             ,root)))
           (kill-buffer b)))
       (delete-directory ,root t))))

(defun ygg-git-compare-context-tests--drawn-p (text)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote text) "$") nil t)))

(defun ygg-git-compare-context-tests--goto (text)
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote text) "$"))
  (beginning-of-line))

(ert-deftest ygg-git-compare-context-expanding-draws-a-hidden-line ()
  (ygg-git-compare-context-tests--with-compare root
    (should-not (ygg-git-compare-context-tests--drawn-p " l10"))
    (ygg-git-compare-expand-context)
    (magit-section-show-level-4-all)
    (should (ygg-git-compare-context-tests--drawn-p " l10"))
    (should-not (ygg-git-compare-context-tests--drawn-p " l1"))
    (ygg-git-compare-shrink-context)
    (magit-section-show-level-4-all)
    (should-not (ygg-git-compare-context-tests--drawn-p " l10"))
    (should (ygg-git-compare-context-tests--drawn-p " l18"))))

(ert-deftest ygg-git-compare-context-point-stays-on-its-file-line ()
  (ygg-git-compare-context-tests--with-compare root
    (ygg-git-compare-context-tests--goto " l22")
    (ygg-git-compare-expand-context)
    (should (looking-at-p " l22$"))
    (ygg-git-compare-context-tests--goto "+twenty")
    (ygg-git-compare-expand-context)
    (should (looking-at-p "\\+twenty$"))
    (ygg-git-compare-shrink-context)
    (should (looking-at-p "\\+twenty$"))))

(ert-deftest ygg-git-compare-context-an-expanded-line-takes-a-comment ()
  (ygg-git-compare-context-tests--with-compare root
    (ygg-git-compare-expand-context)
    (magit-section-show-level-4-all)
    (ygg-git-compare-context-tests--goto " l12")
    (let ((anchor (ygg-git-compare--anchor)))
      (should (equal (list (plist-get anchor :file) (plist-get anchor :side)
                           (plist-get anchor :line) (plist-get anchor :old-line))
                     '("a.txt" new 12 12))))
    (let ((buffer (ygg-git-compare-comment)))
      (with-current-buffer buffer
        (insert "far from the change")
        (aob-compose-send)))
    (let ((comment (car (ygg-git-compare-comments-list))))
      (should (eq (plist-get comment :level) 'line))
      (should (equal (list (plist-get comment :side) (plist-get comment :line)) '(new 12)))
      (should (equal (plist-get comment :text) "far from the change")))))

(ert-deftest ygg-git-compare-context-show-line-draws-just-enough ()
  (ygg-git-compare-context-tests--with-compare root
    (let ((marker (ygg-git-compare-show-line "a.txt" 'new 14)))
      (should (markerp marker))
      (goto-char marker)
      (should (looking-at-p " l14$"))
      (should (= ygg-git-compare--context 6))
      (should-not (ygg-git-compare-context-tests--drawn-p " l13x")))
    (should-not (ygg-git-compare-context-tests--drawn-p " l13x"))
    (let ((marker (ygg-git-compare-show-line "a.txt" 'old 20)))
      (goto-char marker)
      (should (looking-at-p "-l20$")))
    (let ((marker (ygg-git-compare-show-line "a.txt" 'old 40)))
      (goto-char marker)
      (should (looking-at-p " l40$")))
    (should-not (ygg-git-compare-show-line "a.txt" 'new 400))))

(provide 'ygg-git-compare-context-tests)
;;; ygg-git-compare-context-tests.el ends here
