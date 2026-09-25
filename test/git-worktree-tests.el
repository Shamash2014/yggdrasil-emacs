;;; git-worktree-tests.el --- Worktrees in magit and SPC g -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-git)
(require 'magit)

(defmacro git-worktree-tests--in-repo (var &rest body)
  "Run BODY in a fresh repository on branch trunk, VAR bound to its root."
  (declare (indent 1))
  `(let* ((outer (file-name-as-directory (file-truename (make-temp-file "ygg-wt" t))))
          (,var (file-name-as-directory (expand-file-name "main" outer)))
          (default-directory (progn (make-directory ,var) ,var))
          (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment)))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "init" "-q" "-b" "trunk")
           (call-process "git" nil nil nil "-c" "user.email=t@t" "-c" "user.name=t"
                         "commit" "-q" "--allow-empty" "-m" "0")
           ,@body)
       (delete-directory outer t))))

(ert-deftest ygg-git-status-shows-worktrees-after-the-headers ()
  (let ((hook (default-value 'magit-status-sections-hook)))
    (should (memq #'magit-insert-worktrees hook))
    (should (eq (cadr (memq #'magit-insert-status-headers hook))
                #'magit-insert-worktrees))))

(ert-deftest ygg-git-status-hides-worktrees-in-a-single-tree-repo ()
  (git-worktree-tests--in-repo root
    (with-temp-buffer
      (magit-insert-worktrees)
      (should (= (buffer-size) 0)))
    (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "feat/x"
                  (expand-file-name "../feat-x" root))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (status)
          (magit-insert-worktrees)))
      (should (string-match-p "Worktrees" (buffer-string)))
      (should (string-match-p "feat/x" (buffer-string))))))

(ert-deftest ygg-git-worktree-choices-name-the-others-by-name-branch-and-path ()
  (git-worktree-tests--in-repo root
    (let ((tree (file-name-as-directory (expand-file-name "../feat-x" root))))
      (should-not (ygg-git--worktree-choices))
      (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "feat/x" tree)
      (let ((choices (ygg-git--worktree-choices)))
        (should (= (length choices) 1))
        (should (equal (cdar choices) tree))
        (should (string-match-p "\\`feat-x  feat/x  " (caar choices)))
        (should (string-suffix-p (abbreviate-file-name tree) (caar choices))))
      (let ((default-directory tree))
        (should (equal (mapcar #'cdr (ygg-git--worktree-choices)) (list root))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p choices &rest _) (caar choices)))
                ((symbol-function 'magit-status)
                 (lambda (dir &rest _) (should (equal dir tree)) 'opened)))
        (should (eq (ygg-git-worktree-status) 'opened))))))

(ert-deftest ygg-git-worktree-choices-leave-out-the-current-one-reached-by-a-link ()
  (git-worktree-tests--in-repo root
    (let ((tree (file-name-as-directory (expand-file-name "../feat-x" root)))
          (link (expand-file-name "../link" root)))
      (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "feat/x" tree)
      (make-symbolic-link root link)
      (let ((default-directory (file-name-as-directory link)))
        (should (equal (mapcar #'cdr (ygg-git--worktree-choices)) (list tree)))))))

(ert-deftest ygg-git-worktree-status-is-on-the-leader ()
  (should (eq (keymap-lookup ygg-leader-git-map "W") #'ygg-git-worktree-status)))

;;; git-worktree-tests.el ends here
