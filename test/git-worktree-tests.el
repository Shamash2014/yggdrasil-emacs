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

(defun git-worktree-tests--write (file text)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun git-worktree-tests--link (tree branch)
  "Lay out TREE as a linked worktree of this repository on a new BRANCH,
the files git itself leaves on disk for one, without asking git to make it."
  (let* ((tree (directory-file-name (expand-file-name tree)))
         (admin (expand-file-name (concat ".git/worktrees/" (file-name-nondirectory tree)))))
    (git-worktree-tests--write (expand-file-name (concat ".git/refs/heads/" branch))
                               (with-temp-buffer
                                 (call-process "git" nil t nil "rev-parse" "HEAD")
                                 (buffer-string)))
    (git-worktree-tests--write (expand-file-name "gitdir" admin)
                               (concat tree "/.git\n"))
    (git-worktree-tests--write (expand-file-name "commondir" admin) "../..\n")
    (git-worktree-tests--write (expand-file-name "HEAD" admin)
                               (format "ref: refs/heads/%s\n" branch))
    (git-worktree-tests--write (expand-file-name ".git" tree)
                               (format "gitdir: %s\n" admin))))

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
    (git-worktree-tests--link (expand-file-name "../feat-x" root) "feat/x")
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
      (git-worktree-tests--link tree "feat/x")
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
      (git-worktree-tests--link tree "feat/x")
      (make-symbolic-link root link)
      (let ((default-directory (file-name-as-directory link)))
        (should (equal (mapcar #'cdr (ygg-git--worktree-choices)) (list tree)))))))

(ert-deftest ygg-git-worktree-status-is-on-the-leader ()
  (should (eq (keymap-lookup ygg-leader-git-map "W") #'ygg-git-worktree-status)))

;;; git-worktree-tests.el ends here
