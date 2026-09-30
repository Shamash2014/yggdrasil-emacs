;;; ygg-project-umbrella-tests.el --- A plain folder of repos as a project -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ygg-project-scan)

(defmacro ygg-umbrella-tests--with (umbrella imported &rest body)
  "Run BODY with UMBRELLA a temp folder of two repos and a plain folder.
The project list is empty and private to BODY, and IMPORTED collects
every root `ygg-project-import' is asked for."
  (declare (indent 2))
  `(let* ((,umbrella (file-name-as-directory
                      (file-truename (make-temp-file "ygg-umbrella" t))))
          (,imported nil)
          (project--list nil)
          (project-list-file (expand-file-name "projects.eld" ,umbrella))
          (ygg-project--children (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (dolist (child '("alpha" "beta"))
             (let ((default-directory (expand-file-name child ,umbrella)))
               (make-directory default-directory)
               (call-process "git" nil nil nil "init" "-q")))
           (make-directory (expand-file-name "plain" ,umbrella))
           (cl-letf (((symbol-function 'ygg-project-import)
                      (lambda (root &optional callback _extras)
                        (push (ygg-project--key root) ,imported)
                        (when callback (funcall callback root)))))
             ,@body))
       (delete-directory ,umbrella t))))

(ert-deftest ygg-umbrella-add-imports-umbrella-and-children ()
  (ygg-umbrella-tests--with dir imported
    (should (equal (ygg-project-add dir) dir))
    (should (equal (sort (copy-sequence imported) #'string<)
                   (sort (list dir (concat dir "alpha/") (concat dir "beta/"))
                         #'string<)))
    (should (member (concat dir "alpha/") (ygg-project-roots)))
    (should (member dir (ygg-project-roots)))))

(ert-deftest ygg-umbrella-project-current-by-place ()
  (ygg-umbrella-tests--with dir _imported
    (ygg-project-add dir)
    (should (equal (project-root (project-current nil (concat dir "plain/")))
                   dir))
    (let ((child (project-current nil (concat dir "alpha/"))))
      (should (eq (car child) 'vc))
      (should (equal (ygg-project--key (project-root child))
                     (concat dir "alpha/"))))))

(ert-deftest ygg-umbrella-children-are-the-repos ()
  (ygg-umbrella-tests--with dir _imported
    (ygg-project-add dir)
    (should (equal (sort (ygg-project-children dir) #'string<)
                   (list (concat dir "alpha/") (concat dir "beta/"))))
    (should-not (ygg-project-children (concat dir "alpha/")))))

(ert-deftest ygg-umbrella-remove-unregisters-but-keeps-children ()
  (ygg-umbrella-tests--with dir _imported
    (ygg-project-add dir)
    (ygg-project-remove dir)
    (should-not (ygg-project-children dir))
    (should-not (project-current nil (concat dir "plain/")))
    (should (member (concat dir "beta/") (ygg-project-roots)))))

(ert-deftest ygg-umbrella-folder-without-repos-still-errors ()
  (ygg-umbrella-tests--with dir imported
    (should-error (ygg-project-add (concat dir "plain/")) :type 'user-error)
    (should-not imported)))

;;; In the sidebar: repositories under their umbrella, worktrees under theirs

(require 'ygg-projects)
(require 'aob-subagent)

(defun ygg-umbrella-tests--git (dir &rest args)
  (let ((default-directory dir))
    (apply #'call-process "git" nil nil nil
           "-c" "user.name=t" "-c" "user.email=t@t" args)))

(defun ygg-umbrella-tests--write (file text)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun ygg-umbrella-tests--link-worktree (main tree branch)
  "Lay out TREE as a linked worktree of MAIN on BRANCH, the files
git itself leaves on disk for one, without asking git to make it."
  (let ((admin (expand-file-name (concat ".git/worktrees/"
                                         (file-name-nondirectory tree))
                                 main)))
    (ygg-umbrella-tests--write (expand-file-name "gitdir" admin)
                               (concat (expand-file-name ".git" tree) "\n"))
    (ygg-umbrella-tests--write (expand-file-name "commondir" admin) "../..\n")
    (ygg-umbrella-tests--write (expand-file-name "HEAD" admin)
                               (format "ref: refs/heads/%s\n" branch))
    (ygg-umbrella-tests--write (expand-file-name ".git" tree)
                               (format "gitdir: %s\n" admin))))

(defun ygg-umbrella-tests--porcelain (&rest trees)
  "git worktree list --porcelain for TREES, each (DIR . BRANCH)."
  (mapconcat (pcase-lambda (`(,dir . ,branch))
               (format "worktree %s\nHEAD %s\nbranch refs/heads/%s\n\n"
                       dir (make-string 40 ?0) branch))
             trees ""))

(defvar ygg-umbrella-tests--worktree-lists nil
  "Each checkout's git worktree list --porcelain, as (DIR . OUT).")

(defun ygg-umbrella-tests--git-now (dir args callback)
  "`ygg-git-async' answering before it returns, so no test waits on git.
Only a worktree list is asked for, read from
`ygg-umbrella-tests--worktree-lists'."
  (should (equal args '("worktree" "list" "--porcelain")))
  (let ((out (cdr (assoc (file-name-as-directory (expand-file-name dir))
                         ygg-umbrella-tests--worktree-lists))))
    (if out
        (funcall callback out 0)
      (funcall callback "fatal: not a git repository\n" 128)))
  t)

(defmacro ygg-umbrella-tests--with-sidebar (umbrella &rest body)
  "Run BODY with UMBRELLA taken in: repos a and b, and a-wt, a linked
worktree of a laid out inside the umbrella.  The sidebar's caches, the
saved order and `custom-file' are private to BODY, and a worktree list
answers at once from `ygg-umbrella-tests--worktree-lists'."
  (declare (indent 1))
  `(let* ((,umbrella (file-name-as-directory
                      (file-truename (make-temp-file "ygg-umbrella" t))))
          (project--list nil)
          (project-list-file (make-temp-file "ygg-umbrella-projects"))
          (custom-file (make-temp-file "ygg-umbrella-custom" nil ".el"))
          (user-init-file custom-file)
          (ygg-umbrella-tests--order ygg-project-order)
          (ygg-umbrella-tests--plist (symbol-plist 'ygg-project-order))
          (ygg-project--children (make-hash-table :test 'equal))
          (ygg-projects--here nil) (ygg-projects--open nil)
          (ygg-projects--folder-flipped nil)
          (ygg-projects-show-past nil)
          (ygg-projects--worktrees-cache (make-hash-table :test #'equal))
          (ygg-projects--worktrees-pending (make-hash-table :test #'equal))
          (ygg-projects--tree-notes (make-hash-table :test #'equal))
          (ygg-projects--tree-notes-pending (make-hash-table :test #'equal))
          (ygg-projects--tree-mains (make-hash-table :test #'equal))
          (ygg-umbrella-tests--worktree-lists
           (let ((a (concat ,umbrella "a")) (wt (concat ,umbrella "a-wt"))
                 (b (concat ,umbrella "b")))
             (list (cons (concat a "/") (ygg-umbrella-tests--porcelain
                                         (cons a "main") (cons wt "a-wt")))
                   (cons (concat wt "/") (ygg-umbrella-tests--porcelain
                                          (cons a "main") (cons wt "a-wt")))
                   (cons (concat b "/") (ygg-umbrella-tests--porcelain
                                         (cons b "main")))))))
     ;; set, not bound: a reload through Custom does not reach a let
     (setq ygg-project-order nil)
     (unwind-protect
         (progn
           (dolist (child '("a" "b"))
             (let ((dir (expand-file-name child ,umbrella)))
               (make-directory dir)
               (ygg-umbrella-tests--git dir "init" "-q")
               (ygg-umbrella-tests--git dir "commit" "-q" "--allow-empty" "-m" "i")))
           (ygg-umbrella-tests--link-worktree (expand-file-name "a" ,umbrella)
                                              (expand-file-name "a-wt" ,umbrella)
                                              "a-wt")
           (cl-letf (((symbol-function 'ygg-project-import) #'ignore)
                     ((symbol-function 'ygg-git-async) #'ygg-umbrella-tests--git-now)
                     ((symbol-function 'ygg-projects--past) #'ignore))
             (ygg-project-add ,umbrella)
             ,@body))
       (setq ygg-project-order ygg-umbrella-tests--order)
       (setplist 'ygg-project-order ygg-umbrella-tests--plist)
       (delete-directory ,umbrella t)
       (delete-file project-list-file)
       (delete-file custom-file))))

;; `ygg-projects-refresh' reads trace buffers this suite does not load
(defmacro ygg-umbrella-tests--drawing (&rest body)
  "Run BODY with vui's text node the string itself and no redraw."
  `(cl-letf (((symbol-function 'vui-text) #'identity)
             ((symbol-function 'ygg-projects-refresh) #'ignore))
     ,@body))

(defun ygg-umbrella-tests--lines (root)
  "ROOT's Folders row as its lines, without their faces."
  (mapcar (lambda (line) (string-trim (substring-no-properties line)))
          (ygg-projects--entry-nodes root 'folders)))

(ert-deftest ygg-umbrella-sidebar-nests-repos-under-the-umbrella ()
  "One card, the umbrella's, and its repositories as its folders."
  (ygg-umbrella-tests--with-sidebar u
    (let ((a (concat u "a/")) (b (concat u "b/")))
      (should (member a (ygg-project-roots)))
      (should (equal (ygg-project-top-roots) (list u)))
      (should (equal (ygg-projects--shown) (list u)))
      (should (equal (ygg-projects--roots) (list u a b)))
      (should (equal (mapcar #'cdr (ygg-projects--entries u 'folders))
                     (list u a b))))))

(ert-deftest ygg-umbrella-worktree-is-no-repo-and-sits-under-its-own ()
  (ygg-umbrella-tests--with-sidebar u
    (let ((a (concat u "a/")) (wt (concat u "a-wt")))
      (should (equal (ygg-project-children u) (list a (concat u "b/"))))
      (should-not (ygg-project-umbrella-of (concat wt "/")))
      (ygg-projects--scan-worktrees)
      (should (equal (ygg-projects--worktree-entries a)
                     (list (cons "a-wt (a-wt)" wt))))
      (should-not (ygg-projects--worktree-entries u)))))

(ert-deftest ygg-umbrella-shows-repos-as-folders-and-no-worktree-row ()
  (ygg-umbrella-tests--with-sidebar u
    (ygg-projects--scan-worktrees)
    (ygg-umbrella-tests--drawing
      (should-not (assq 'worktrees (ygg-projects--row-specs u)))
      (should (equal (cdr (ygg-umbrella-tests--lines u)) '("│ · ▸ a ⌥1" "│ · ▸ b")))
      (should (equal (substring-no-properties (ygg-projects--badge (concat u "a/") 30))
                     "⌥1")))))

(ert-deftest ygg-umbrella-tab-opens-a-repos-worktrees-and-sessions ()
  (ygg-umbrella-tests--with-sidebar u
    (ygg-projects--scan-worktrees)
    (let* ((a (concat u "a/"))
           (wt (concat u "a-wt/"))
           (s (aob-create-session :id "umbrella-tab" :backend 'acp :name "fix"
                                  :project wt :dir wt :state 'working)))
      (unwind-protect
          (ygg-umbrella-tests--drawing
            (ygg-projects--session-main s)
            (should (string-search "●1" (nth 1 (ygg-umbrella-tests--lines u))))
            (with-temp-buffer
              (let ((draw (lambda ()
                            (erase-buffer)
                            (dolist (line (ygg-projects--entry-nodes u 'folders))
                              (insert line "\n")))))
                (funcall draw)
                (goto-char (point-min))
                (forward-line 1)
                (should (equal (get-text-property (point) 'ygg-entry) a))
                (ygg-projects-toggle)
                (funcall draw)
                (let ((lines (ygg-umbrella-tests--lines u)))
                  (should (string-prefix-p "│ · ▾ a" (nth 1 lines)))
                  (should (string-prefix-p "│ · ⌥ a-wt (a-wt)" (nth 2 lines)))
                  (should (string-prefix-p "│ · fix" (nth 3 lines)))
                  (should (string-prefix-p "│   ⌥ a-wt · a-wt" (nth 4 lines)))
                  (should (string-prefix-p "│ · ▸ b" (nth 5 lines))))
                (goto-char (point-min))
                (forward-line 3)
                (should (eq (get-text-property (point) 'ygg-entry) s))
                (should (equal (get-text-property (point) 'ygg-project) a))
                (should (eq (get-text-property (point) 'ygg-row) 'agents))
                (ygg-projects-toggle)
                (should (= (line-number-at-pos) 2))
                (should (equal (cdr (ygg-umbrella-tests--lines u))
                               '("│ · ▸ a ⌥1 ●1" "│ · ▸ b"))))))
        (aob-remove-session s)))))

(ert-deftest ygg-umbrella-opening-a-repo-keeps-its-umbrella-card-open ()
  (ygg-umbrella-tests--with-sidebar u
    (let ((a (concat u "a/")) (ygg-projects--open-row nil) (opened nil))
      (ygg-umbrella-tests--drawing
        (cl-letf (((symbol-function 'ygg-space-open) (lambda (d) (push d opened))))
          (ygg-projects--open-root a)))
      (should (equal opened (list (directory-file-name a))))
      (should (equal ygg-projects--open u))
      (should (member (cons u 'folders) ygg-projects--open-row))
      (should (ygg-projects--folder-open-p a)))))

(ert-deftest ygg-umbrella-repo-order-survives-a-reload ()
  (ygg-umbrella-tests--with-sidebar u
    (let ((a (concat u "a/")) (b (concat u "b/")))
      (ygg-project-move-child b -1)
      (should (equal (ygg-project-children u) (list b a)))
      (should (equal (ygg-projects--roots) (list u b a)))
      (setq ygg-project-order nil)
      (should (equal (ygg-project-children u) (list a b)))
      (load custom-file nil t)
      (should (equal (ygg-project-children u) (list b a))))))

(ert-deftest ygg-umbrella-session-in-a-repos-worktree-files-under-the-repo ()
  (ygg-umbrella-tests--with-sidebar u
    (let* ((a (concat u "a/"))
           (wt (concat u "a-wt/"))
           (s (aob-create-session :id "umbrella-wt" :backend 'acp :name "wt"
                                  :project wt :dir wt :state 'working)))
      (unwind-protect
          (progn
            (ygg-projects--session-main s)
            (should (memq s (ygg-projects--sessions a)))
            (should-not (memq s (ygg-projects--sessions u))))
        (aob-remove-session s)))))

(ert-deftest ygg-umbrella-top-level-move-keeps-the-order ()
  (ygg-umbrella-tests--with-sidebar u
    (let ((other (file-name-as-directory
                  (file-truename (make-temp-file "ygg-umbrella-other" t)))))
      (unwind-protect
          (progn
            (ygg-umbrella-tests--git other "init" "-q")
            (let ((ygg-project--importing t))
              (project-remember-project (project-current nil other)))
            (should (equal (ygg-project-top-roots) (list other u)))
            (ygg-project-move u -1)
            (should (equal (ygg-project-top-roots) (list u other)))
            (setq project--list nil)
            (project--read-project-list)
            (should (equal (ygg-project-top-roots) (list u other))))
        (delete-directory other t)))))

;;; ygg-project-umbrella-tests.el ends here
