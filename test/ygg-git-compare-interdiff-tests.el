;;; ygg-git-compare-interdiff-tests.el --- what B changed since it was last seen -*- lexical-binding: t; -*-

;;; Code:

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
(require 'ygg-git-compare-interdiff)

(defun ygg-git-compare-interdiff-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defmacro ygg-git-compare-interdiff-tests--with-repo (root &rest body)
  "A repo at ROOT on branch feature, one commit past main, which holds f."
  (declare (indent 1))
  `(let* ((process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (,root (file-name-as-directory
                  (file-truename (make-temp-file "ygg-git-compare-interdiff-" t))))
          (default-directory ,root)
          (magit-refresh-verbose nil))
     (unwind-protect
         (cl-flet ((git (&rest args) (apply #'ygg-git-compare-interdiff-tests--git ,root args)))
           (git "init" "-q" "-b" "feature")
           (git "config" "user.name" "Interdiff Test")
           (git "config" "user.email" "interdiff@example.invalid")
           (git "config" "commit.gpgsign" "false")
           (with-temp-file (expand-file-name "f" ,root) (insert "a\n"))
           (git "add" "f")
           (git "commit" "-q" "-m" "base")
           (git "branch" "main")
           (with-temp-file (expand-file-name "f" ,root) (insert "a\nb\n"))
           (git "commit" "-q" "-am" "feature one")
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-live-p b)
                    (file-in-directory-p (buffer-local-value 'default-directory b) ,root))
           (kill-buffer b)))
       (delete-directory ,root t))))

(defun ygg-git-compare-interdiff-tests--seen ()
  (mapcar #'car (ygg-git-compare-interdiff-seen "feature")))

(ert-deftest ygg-git-compare-interdiff-shows-what-an-amend-changed ()
  (ygg-git-compare-interdiff-tests--with-repo root
    (let ((old (git "rev-parse" "HEAD")))
      (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "feature"))
        (should (equal (ygg-git-compare-interdiff-tests--seen) (list old)))
        (with-temp-file (expand-file-name "f" root) (insert "a\nb\nc\n"))
        (git "commit" "-q" "--amend" "-am" "feature one, reworked")
        (let ((new (git "rev-parse" "HEAD")))
          (ygg-git-compare-refresh)
          (should (equal (ygg-git-compare-interdiff-tests--seen) (list new old)))
          (let (offered meta)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt table &rest args)
                         (setq offered (all-completions "" table)
                               meta (cdr (funcall table "" nil 'metadata)))
                         (nth 4 args))))
              (with-current-buffer (ygg-git-compare-interdiff)
                (should (equal (length offered) 1))
                (should (string-prefix-p (substring old 0 7) (car offered)))
                (should (eq (alist-get 'category meta) 'ygg-review-sha))
                (should (string-match-p " ago  feature one\\'"
                                        (funcall (alist-get 'annotation-function meta)
                                                 (car offered))))
                (should (string-prefix-p "*range-diff: " (buffer-name)))
                (should ygg-git-compare-mode)
                (should (buffer-live-p ygg-git-compare--list-buffer))
                (let ((text (buffer-string)))
                  (should (string-match-p "1: +[0-9a-f]+ ! 1: +[0-9a-f]+ feature one" text))
                  (should (string-match-p "^ *-    feature one$" text))
                  (should (string-match-p "^ *\\+    feature one, reworked$" text))
                  (should (string-match-p "^ *\\+\\+c$" text)))))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt _table &rest args) (nth 4 args))))
              (with-current-buffer (ygg-git-compare-interdiff t)
                (should (equal magit-buffer-diff-range (concat old ".." new)))
                (should (equal magit-buffer-diff-files '("f")))
                (should (string-match-p "^\\+c$" (buffer-string)))))))))))

(ert-deftest ygg-git-compare-interdiff-after-a-rebase-leaves-out-a-s-commits ()
  (ygg-git-compare-interdiff-tests--with-repo root
    (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "feature"))
      (git "update-ref" "refs/heads/main"
           (git "commit-tree" "-p" "main" "-m" "main moves on" "main^{tree}"))
      (git "rebase" "-q" "main")
      (ygg-git-compare-refresh)
      (should (= (length (ygg-git-compare-interdiff-tests--seen)) 2))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _table &rest args) (nth 4 args))))
        (with-current-buffer (ygg-git-compare-interdiff)
          (should (string-match-p "1: +[0-9a-f]+ = 1: +[0-9a-f]+ feature one" (buffer-string)))
          (should-not (string-search "main moves on" (buffer-string))))))))

(ert-deftest ygg-git-compare-interdiff-without-an-earlier-version-says-so ()
  (ygg-git-compare-interdiff-tests--with-repo root
    (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "feature"))
      (should (equal (cadr (should-error (ygg-git-compare-interdiff) :type 'user-error))
                     "No earlier version of feature seen")))))

(ert-deftest ygg-git-compare-interdiff-history-is-capped-and-deduplicated ()
  (ygg-git-compare-interdiff-tests--with-repo root
    (let ((ygg-git-compare-interdiff-keep 3))
      (dolist (sha '("s1" "s2" "s3" "s4"))
        (ygg-git-compare-interdiff-remember "feature" sha))
      (should (equal (ygg-git-compare-interdiff-tests--seen) '("s4" "s3" "s2")))
      (ygg-git-compare-interdiff-remember "feature" "s2")
      (should (equal (ygg-git-compare-interdiff-tests--seen) '("s2" "s4" "s3")))
      (let ((time (cdar (ygg-git-compare-interdiff-seen "feature"))))
        (ygg-git-compare-interdiff-remember "feature" "s2")
        (should (equal (cdar (ygg-git-compare-interdiff-seen "feature")) time)))
      (ygg-git-compare-interdiff-remember "other" "o1")
      (should (equal (mapcar #'car (ygg-git-compare-interdiff-seen "other")) '("o1")))
      (should (equal (ygg-git-compare-interdiff-tests--seen) '("s2" "s4" "s3"))))))

(ert-deftest ygg-git-compare-interdiff-keys-name-the-side ()
  (should (equal (ygg-git-compare-interdiff-key '(worktree . "/r/wt/")) "/r/wt"))
  (should (equal (ygg-git-compare-interdiff-key '(pr :number 12 :sha "x")) "#12"))
  (should (equal (ygg-git-compare-interdiff-key '(rev . "feature")) "feature")))

(provide 'ygg-git-compare-interdiff-tests)
;;; ygg-git-compare-interdiff-tests.el ends here
