;;; ygg-git-compare-explain-tests.el --- asking about a compare from inside it -*- lexical-binding: t; -*-

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (builds (expand-file-name "../elpaca/builds/" here)))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds)))
  (add-to-list 'load-path (expand-file-name "../lisp/agent-objects" here)))

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-compare-explain)

(setq aob-acp-persist-file (make-temp-file "compare-explain-sessions-" nil ".eld"))
(setq aob-acp-show-trace nil)

(defun ygg-git-compare-explain-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-compare-explain-tests--write (file text)
  (with-temp-file file (insert text)))

(defmacro ygg-git-compare-explain-tests--with-repo (vars &rest body)
  "A repo on main with branch feature checked out in a second worktree.
VARS binds (ROOT TREE).  Feature turns a.txt's second line into \"two\"."
  (declare (indent 1))
  (let ((root (nth 0 vars)) (tree (nth 1 vars)))
    `(let* ((process-environment
             (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                     process-environment))
            (outer (file-name-as-directory
                    (file-truename (make-temp-file "ygg-compare-explain-" t))))
            (,root (file-name-as-directory (expand-file-name "main" outer)))
            (,tree (file-name-as-directory (expand-file-name "wt" outer)))
            (default-directory (progn (make-directory ,root) ,root))
            (magit-refresh-verbose nil))
       (unwind-protect
           (cl-flet ((git (dir &rest args)
                       (apply #'ygg-git-compare-explain-tests--git dir args)))
             (git ,root "init" "-q" "-b" "main")
             (git ,root "config" "user.name" "Compare Test")
             (git ,root "config" "user.email" "compare@example.invalid")
             (git ,root "config" "commit.gpgsign" "false")
             (ygg-git-compare-explain-tests--write
              (expand-file-name "a.txt" ,root) "1\n2\n3\n")
             (git ,root "add" ".")
             (git ,root "commit" "-q" "-m" "base")
             (git ,root "branch" "feature")
             (git ,root "worktree" "add" "-q" ,tree "feature")
             (ygg-git-compare-explain-tests--write
              (expand-file-name "a.txt" ,tree) "1\ntwo\n3\n")
             (git ,tree "commit" "-q" "-am" "fix a")
             (ignore ,root ,tree)
             ,@body)
         (dolist (b (buffer-list))
           (when (file-in-directory-p (buffer-local-value 'default-directory b) outer)
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (delete-directory outer t)))))

(defun ygg-git-compare-explain-tests--right (list)
  (window-buffer (buffer-local-value 'ygg-git-compare--file-window list)))

(defun ygg-git-compare-explain-tests--on (list text)
  "Point in LIST's right pane on TEXT in a diff line, that pane selected."
  (let ((window (buffer-local-value 'ygg-git-compare--file-window list)))
    (select-window window)
    (goto-char (point-min))
    (search-forward text)
    (goto-char (match-beginning 0))))

(ert-deftest ygg-git-compare-explain-b-position-from-diff-line ()
  (ygg-git-compare-explain-tests--with-repo (root tree)
    (let ((list (ygg-git-compare-open '(rev . "main") (cons 'worktree tree))))
      (with-current-buffer (ygg-git-compare-explain-tests--right list)
        (ygg-git-compare-explain-tests--on list "+two")
        (should (equal (ygg-git-compare-explain-b-position)
                       (list (expand-file-name "a.txt" tree) 2 0)))
        (forward-char 2)
        (should (equal (ygg-git-compare-explain-b-position)
                       (list (expand-file-name "a.txt" tree) 2 1)))
        (ygg-git-compare-explain-tests--on list "-2")
        (should (equal (ygg-git-compare-explain-b-position)
                       (list (expand-file-name "a.txt" tree) 2 0)))
        (ygg-git-compare-explain-tests--on list " 3")
        (should (equal (nth 1 (ygg-git-compare-explain-b-position)) 3))))))

(ert-deftest ygg-git-compare-explain-b-position-needs-a-worktree ()
  (ygg-git-compare-explain-tests--with-repo (root tree)
    (let ((list (ygg-git-compare-open '(rev . "main") '(rev . "feature"))))
      (with-current-buffer (ygg-git-compare-explain-tests--right list)
        (ygg-git-compare-explain-tests--on list "+two")
        (should (string-match-p
                 "B is feature, not a worktree"
                 (cadr (should-error (ygg-git-compare-explain-b-position)
                                     :type 'user-error))))))))

(ert-deftest ygg-git-compare-explain-visit-b-opens-the-real-file-in-the-pane ()
  (ygg-git-compare-explain-tests--with-repo (root tree)
    (let* ((list (ygg-git-compare-open '(rev . "main") (cons 'worktree tree)))
           (window (buffer-local-value 'ygg-git-compare--file-window list)))
      (ygg-git-compare-explain-tests--on list "+two")
      (ygg-git-compare-visit-b)
      (should (eq (selected-window) window))
      (should (equal (buffer-file-name) (expand-file-name "a.txt" tree)))
      (should (= (line-number-at-pos) 2))
      (should (looking-at "two"))
      (should (= (length (window-list)) 2))
      (should-not (buffer-local-value 'ygg-git-compare--shown list)))))

(defmacro ygg-git-compare-explain-tests--spawning (spawned &rest body)
  "Run BODY with aob-acp-spawn recorded into SPAWNED, no agent started.
Each record is a plist of what the spawn was handed and where it starts."
  (declare (indent 1))
  `(let ((trace (generate-new-buffer " *compare-explain-trace*")))
     (unwind-protect
         (cl-letf (((symbol-function 'aob-acp-spawn)
                    (lambda (agent &optional intent atts name tree)
                      (push (list :agent agent :intent intent :atts atts :name name
                                  :tree tree :dir aob-acp-start-dir
                                  :show-trace aob-acp-show-trace)
                            ,spawned)
                      (aob-create-session :id (concat "acp:" name) :backend 'acp
                                          :name name :project aob-acp-start-dir
                                          :state 'starting)))
                   ((symbol-function 'aob-trace-buffer) (lambda (_s) trace)))
           ,@body)
       (dolist (s (aob-sessions)) (aob-remove-session s))
       (when (buffer-live-p trace) (kill-buffer trace)))))

(ert-deftest ygg-git-compare-explain-sends-range-dir-and-prompt ()
  (ygg-git-compare-explain-tests--with-repo (root tree)
    (ygg-git-compare-explain-tests--write (expand-file-name "a.txt" tree)
                                          "1\ntwo\n3\ndirty\n")
    (let* ((aob-acp-default-agent "claude")
           (main (ygg-git-compare-explain-tests--git root "rev-parse" "main"))
           (head (ygg-git-compare-explain-tests--git tree "rev-parse" "HEAD"))
           (list (ygg-git-compare-open '(rev . "main") (cons 'worktree tree)))
           (window (buffer-local-value 'ygg-git-compare--file-window list))
           spawned)
      (ygg-git-compare-explain-tests--spawning spawned
        (with-current-buffer list (ygg-git-compare-explain))
        (let* ((call (car spawned))
               (intent (plist-get call :intent)))
          (should (= (length spawned) 1))
          (should (equal (plist-get call :agent) "claude"))
          (should (equal (plist-get call :dir) tree))
          (should-not (plist-get call :show-trace))
          (should (equal (plist-get call :name) "explain main…feature"))
          (should (string-search (format "The change, run in %s: git diff %s for" tree main)
                                 intent))
          (should (string-search "git ls-files --others --exclude-standard" intent))
          (should (string-search "what its worktree has not committed" intent))
          (should (string-search (format "git log --left-right %s...%s" main head)
                                 intent))
          (should (string-search "the calls the change touches" intent))
          (should (string-search "Read only" intent))
          (should (string-search ygg-git-compare-explain-instructions intent))
          (should (eq (window-buffer window) (get-buffer " *compare-explain-trace*")))
          (should (eq (selected-window) window))
          (should (= (length (window-list)) 2))
          (ygg-git-compare-quit)
          (should (get-buffer " *compare-explain-trace*"))
          (should (aob-session-get "acp:explain main…feature")))))))

(ert-deftest ygg-git-compare-explain-follows-dots-and-a-branch-b ()
  (ygg-git-compare-explain-tests--with-repo (root tree)
    (let* ((main (ygg-git-compare-explain-tests--git root "rev-parse" "main"))
           (feature (ygg-git-compare-explain-tests--git root "rev-parse" "feature"))
           (list (ygg-git-compare-open '(rev . "main") '(rev . "feature")))
           spawned)
      (with-current-buffer list (ygg-git-compare-toggle-dots))
      (ygg-git-compare-explain-tests--spawning spawned
        (with-current-buffer list (ygg-git-compare-explain))
        (let ((call (car spawned)))
          (should (equal (plist-get call :dir) root))
          (should (string-search (format "git diff %s..%s\n" main feature)
                                 (plist-get call :intent))))))))

(ert-deftest ygg-git-compare-explain-refuses-a-preset-with-its-own-worktree ()
  (ygg-git-compare-explain-tests--with-repo (root tree)
    (let ((list (ygg-git-compare-open '(rev . "main") (cons 'worktree tree)))
          (ygg-git-compare-explain-preset "claude-isolated")
          spawned)
      (ygg-git-compare-explain-tests--spawning spawned
        (with-current-buffer list
          (should-error (ygg-git-compare-explain) :type 'user-error))
        (should-not spawned)))))

(ert-deftest ygg-git-compare-explain-names-a-remote-checkout-as-its-agent-sees-it ()
  (let ((remote "/ssh:host:/srv/repo/"))
    (with-temp-buffer
      (setq-local ygg-git-compare--a '(:label "main" :diff "aaa" :log "aaa"))
      (setq-local ygg-git-compare--b (list :label "repo [x]" :diff "bbb" :log "bbb"
                                           :dir remote :uncommitted t))
      (setq-local ygg-git-compare--b-spec (cons 'worktree remote))
      (setq-local ygg-git-compare--plan (list :work ygg-git-compare--b :dir remote
                                              :range "aaa"))
      (setq-local ygg-git-compare--dots "...")
      (let ((context (ygg-git-compare-explain-context (current-buffer))))
        (should (string-search "Repository: /srv/repo/\n" context))
        (should (string-search "run in /srv/repo/: git diff aaa" context))
        (should-not (string-search "/ssh:" context))))))

(provide 'ygg-git-compare-explain-tests)
;;; ygg-git-compare-explain-tests.el ends here
