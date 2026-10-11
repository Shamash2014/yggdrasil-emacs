;;; aob-gc-tests.el --- ERT tests for aob-gc -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(load (expand-file-name "test-elpaca" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'aob-gc)

(defvar gc-tests--base nil)
(defvar gc-tests--procs nil)

(defun gc-tests--git (dir &rest args)
  "Run git ARGS in DIR and return its output, failing the test on error."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory dir)))
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "git %s: %s" args (buffer-string)))
      (buffer-string))))

(defun gc-tests--commit (dir file text)
  "Commit FILE holding TEXT in DIR."
  (with-temp-file (expand-file-name file dir) (insert text))
  (gc-tests--git dir "add" file)
  (gc-tests--git dir "commit" "-q" "-m" file))

(defun gc-tests--wait (pred)
  "Pump the event loop until PRED holds, up to 20 seconds."
  (let ((deadline (+ (float-time) 20)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall pred))))

(defmacro gc-tests--with-repo (&rest body)
  "Run BODY with PROJ, ROOT and a registry-free environment bound."
  (declare (indent 0))
  `(let* ((gc-tests--base (file-name-as-directory
                          (file-truename (make-temp-file "aob-gc-" t))))
          (process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null"
                     "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                     "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                   process-environment))
          (origin (expand-file-name "origin.git" gc-tests--base))
          (proj (expand-file-name "proj" gc-tests--base))
          (root (file-name-as-directory (expand-file-name "wt" gc-tests--base)))
          (aob-acp-worktree-root root)
          (aob-gc-branches 'aob)
          (aob--sessions (make-hash-table :test #'equal))
          (aob--order nil)
          (aob-gc--verdicts (make-hash-table :test #'equal))
          (aob-gc--prs (make-hash-table :test #'equal))
          (aob-gc--busy (make-hash-table :test #'equal))
          (aob-acp-persist-file nil)
          (gc-tests--procs (process-list))
          (messages nil))
     (unwind-protect
         (progn
           (make-directory root t)
           (gc-tests--git gc-tests--base "init" "-q" "--bare" "-b" "main" origin)
           (gc-tests--git gc-tests--base "clone" "-q" origin proj)
           (gc-tests--git proj "checkout" "-q" "-B" "main")
           (gc-tests--commit proj "a" "a")
           (gc-tests--git proj "push" "-q" "-u" "origin" "main")
           (gc-tests--git proj "remote" "set-head" "origin" "main")
           (cl-letf (((symbol-function 'aob-gc--projects) (lambda () (list proj)))
                     ((symbol-function 'aob--dirty) #'ignore)
                     ((symbol-function 'message)
                      (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
             ,@body))
       (dolist (p (process-list))
         (unless (memq p gc-tests--procs)
           (delete-process p)))
       (delete-directory gc-tests--base t))))

(defun gc-tests--worktree (proj dir branch &optional text)
  "Add worktree DIR on new BRANCH off PROJ, committing TEXT in it when given."
  (gc-tests--git proj "worktree" "add" "-q" "-b" branch dir)
  (when text (gc-tests--commit dir "f.txt" text)))

(defun gc-tests--merge-ff (proj branch)
  "Fast-forward PROJ's main to BRANCH and publish it."
  (gc-tests--git proj "merge" "-q" "--ff-only" branch)
  (gc-tests--git proj "push" "-q" "origin" "main"))

(defun gc-tests--branches (proj)
  "The local branch names of PROJ."
  (mapcar (lambda (r) (string-remove-prefix "refs/heads/" r))
          (split-string (gc-tests--git proj "for-each-ref" "--format=%(refname)"
                                       "refs/heads")
                        "\n" t)))

(defun gc-tests--sweep (&optional dry)
  "Run a sweep and return its results once finished."
  (let (out finished)
    (aob-gc--sweep (aob-gc--sweep-mode) dry
                   (lambda (r) (setq out r finished t)))
    (gc-tests--wait (lambda () finished))
    out))

(ert-deftest aob-gc-ff-merged-worktree-and-branch-go ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root)))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--sweep)
      (should-not (file-directory-p dir))
      (should-not (member "aob/ff" (gc-tests--branches proj))))))

(ert-deftest aob-gc-squash-merged-worktree-and-branch-go ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "sq" root)))
      (gc-tests--worktree proj dir "aob/sq" "one")
      (gc-tests--commit dir "g.txt" "two")
      (gc-tests--commit proj "other" "unrelated")
      (gc-tests--git proj "merge" "-q" "--squash" "aob/sq")
      (gc-tests--git proj "commit" "-q" "-m" "squashed")
      (gc-tests--git proj "push" "-q" "origin" "main")
      (should-not (string-empty-p (gc-tests--git proj "cherry" "main" "aob/sq")))
      (gc-tests--sweep)
      (should-not (file-directory-p dir))
      (should-not (member "aob/sq" (gc-tests--branches proj))))))

(ert-deftest aob-gc-unmerged-is-kept ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "un" root)))
      (gc-tests--worktree proj dir "aob/un" "one")
      (gc-tests--sweep)
      (should (file-directory-p dir))
      (should (member "aob/un" (gc-tests--branches proj))))))

(ert-deftest aob-gc-merged-but-dirty-is-kept ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "dirty" root)))
      (gc-tests--worktree proj dir "aob/dirty" "one")
      (gc-tests--merge-ff proj "aob/dirty")
      (with-temp-file (expand-file-name "scratch" dir) (insert "x"))
      (gc-tests--sweep)
      (should (file-directory-p dir))
      (should (member "aob/dirty" (gc-tests--branches proj))))))

(ert-deftest aob-gc-any-session-inside-protects ()
  (gc-tests--with-repo
    (let ((busy (expand-file-name "busy" root))
          (idle (expand-file-name "idle" root))
          (proc (start-process "gc-tests-agent" nil "cat")))
      (gc-tests--worktree proj busy "aob/busy" "one")
      (gc-tests--merge-ff proj "aob/busy")
      (gc-tests--worktree proj idle "aob/idle")
      (aob-create-session :id "acp:gc-live" :backend 'acp :name "live"
                          :project proj :dir (file-name-as-directory busy)
                          :state 'idle :conn proc)
      (aob-create-session :id "acp:gc-asleep" :backend 'acp :name "asleep"
                          :project proj :dir (file-name-as-directory idle)
                          :state 'idle)
      (gc-tests--sweep)
      (should (file-directory-p busy))
      (should (member "aob/busy" (gc-tests--branches proj)))
      (should (file-directory-p idle))
      (should (member "aob/idle" (gc-tests--branches proj))))))

(ert-deftest aob-gc-non-aob-branches-follow-the-scope ()
  (gc-tests--with-repo
    (let ((other (expand-file-name "other" gc-tests--base)))
      (gc-tests--git proj "branch" "feature/done")
      (gc-tests--worktree proj other "feature/open")
      (gc-tests--sweep)
      (should (member "feature/done" (gc-tests--branches proj)))
      (let ((aob-gc-branches 'all-merged))
        (gc-tests--sweep))
      (should-not (member "feature/done" (gc-tests--branches proj)))
      (should (member "feature/open" (gc-tests--branches proj)))
      (should (member "main" (gc-tests--branches proj)))
      (should (file-directory-p other)))))

(ert-deftest aob-gc-preview-removes-nothing ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root)))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (let ((results (gc-tests--sweep t)))
        (should (equal '("aob/ff")
                       (mapcar (lambda (i) (plist-get i :branch))
                               (seq-remove (lambda (i) (plist-get i :skip))
                                           (cdar results))))))
      (should (file-directory-p dir))
      (should (member "aob/ff" (gc-tests--branches proj))))))

(ert-deftest aob-gc-sweep-reports-removals-in-one-message ()
  (gc-tests--with-repo
    (dolist (n '("a1" "a2"))
      (gc-tests--worktree proj (expand-file-name n root) (concat "aob/" n)))
    (aob-gc--run t)
    (gc-tests--wait (lambda () messages))
    (should (= 1 (length messages)))
    (should (string-match-p "removed" (car messages)))))

(defmacro gc-tests--with-project-command (answer &rest body)
  "Run BODY in a repo with a merged worktree outside the root, answering ANSWER."
  (declare (indent 1))
  `(gc-tests--with-repo
     (let ((other (expand-file-name "other" gc-tests--base))
           (default-directory (file-name-as-directory proj)))
       (gc-tests--git proj "branch" "feature/plain")
       (gc-tests--worktree proj other "feature/wt")
       (gc-tests--worktree proj (expand-file-name "keep" gc-tests--base) "feature/unmerged" "x")
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) ,answer)))
         ,@body))))

(ert-deftest aob-gc-project-removes-merged-after-confirmation ()
  (gc-tests--with-project-command t
    (aob-gc-project)
    (gc-tests--wait (lambda () (seq-some (lambda (m) (string-match-p "removed" m)) messages)))
    (should-not (file-directory-p other))
    (should-not (member "feature/wt" (gc-tests--branches proj)))
    (should-not (member "feature/plain" (gc-tests--branches proj)))
    (should (member "feature/unmerged" (gc-tests--branches proj)))
    (should (member "main" (gc-tests--branches proj)))))

(ert-deftest aob-gc-project-removes-nothing-on-no ()
  (gc-tests--with-project-command nil
    (aob-gc-project)
    (gc-tests--wait (lambda () (seq-some (lambda (m) (string-match-p "kept everything" m)) messages)))
    (should (file-directory-p other))
    (should (member "feature/wt" (gc-tests--branches proj)))
    (should (member "feature/plain" (gc-tests--branches proj)))))

(ert-deftest aob-gc-project-spares-dirty-and-live-worktrees ()
  (gc-tests--with-project-command t
    (let ((proc (start-process "gc-tests-agent" nil "cat"))
          (dirty (expand-file-name "dirty" gc-tests--base)))
      (gc-tests--worktree proj dirty "feature/dirty")
      (with-temp-file (expand-file-name "scratch" dirty) (insert "x"))
      (aob-create-session :id "acp:gc-live" :backend 'acp :name "live"
                          :project proj :dir (file-name-as-directory other)
                          :state 'idle :conn proc)
      (aob-gc-project)
      (gc-tests--wait (lambda () (seq-some (lambda (m) (string-match-p "removed" m)) messages)))
      (should (file-directory-p other))
      (should (file-directory-p dirty))
      (should-not (member "feature/plain" (gc-tests--branches proj))))))

(defun gc-tests--plan (proj mode)
  "Plan a MODE sweep of PROJ and return its items."
  (let (out finished)
    (aob-gc--plan proj mode nil (lambda (items) (setq out items finished t)))
    (gc-tests--wait (lambda () finished))
    out))

(defun gc-tests--remove (proj items)
  "Run the removal in PROJ of ITEMS to its end and return the branches removed."
  (let (out finished)
    (aob-gc--remove proj items nil (lambda (r) (setq out r finished t)))
    (gc-tests--wait (lambda () finished))
    out))

(defun gc-tests--sha (dir ref)
  "The commit REF names in DIR."
  (string-trim (gc-tests--git dir "rev-parse" ref)))

(defun gc-tests--fake-gh (script)
  "A directory holding a gh that runs the shell SCRIPT."
  (let* ((bin (make-temp-file "aob-gc-bin" t))
         (gh (expand-file-name "gh" bin)))
    (with-temp-file gh (insert "#!/bin/sh\n" script "\n"))
    (set-file-modes gh #o755)
    bin))

(defun gc-tests--diverge (proj root name)
  "Add worktree NAME under ROOT of PROJ, with work main lacks and main moved on."
  (let ((wt (expand-file-name name root)))
    (gc-tests--worktree proj wt (concat "aob/" name) name)
    (gc-tests--commit proj (concat "other-" name) "unrelated")
    wt))

(ert-deftest aob-gc-merged-pr-needs-this-branch-tip-and-this-repository ()
  (gc-tests--with-repo
    (let* ((aob-gc-fetch nil)
           (same (gc-tests--diverge proj root "same"))
           (moved (gc-tests--diverge proj root "moved"))
           (fork (gc-tests--diverge proj root "fork"))
           (calls (expand-file-name "calls" gc-tests--base))
           (json (lambda (name sha cross)
                   (format "{\"headRefName\":\"%s\",\"headRefOid\":\"%s\",\"isCrossRepository\":%s}"
                           name sha cross)))
           (bin (gc-tests--fake-gh
                 (format "echo x >> %s\necho '[%s,%s,%s]'" calls
                         (funcall json "aob/same" (gc-tests--sha same "HEAD") "false")
                         (funcall json "aob/moved" (gc-tests--sha proj "main") "false")
                         (funcall json "aob/fork" (gc-tests--sha fork "HEAD") "true"))))
           (exec-path (cons bin exec-path))
           (process-environment (cons (concat "PATH=" bin ":" (getenv "PATH"))
                                      process-environment)))
      (gc-tests--git proj "remote" "set-url" "origin" "https://github.com/x/y.git")
      (gc-tests--sweep)
      (should-not (file-directory-p same))
      (should (file-directory-p moved))
      (should (file-directory-p fork))
      (should-not (member "aob/same" (gc-tests--branches proj)))
      (should (member "aob/moved" (gc-tests--branches proj)))
      (should (member "aob/fork" (gc-tests--branches proj)))
      (should (= 1 (length (split-string (with-temp-buffer (insert-file-contents calls)
                                                           (buffer-string))
                                         "\n" t)))))))

(ert-deftest aob-gc-branch-that-moves-after-the-plan-is-kept ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root))
          (aob-gc-branches 'all-merged))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--git proj "branch" "feature/done")
      (let* ((items (gc-tests--plan proj 'all-merged))
             (old (gc-tests--sha proj "feature/done"))
             (new (string-trim (gc-tests--git proj "commit-tree" "main^{tree}" "-p" "main"
                                              "-m" "n"))))
        (should (= 2 (length (seq-remove (lambda (i) (plist-get i :skip)) items))))
        (gc-tests--commit dir "late" "late")
        (gc-tests--git proj "update-ref" "refs/heads/feature/done" new)
        (should-not (gc-tests--remove proj items))
        (should (file-directory-p dir))
        (should (member "aob/ff" (gc-tests--branches proj)))
        (should (member "feature/done" (gc-tests--branches proj)))
        (should-error (gc-tests--git proj "update-ref" "-d" "refs/heads/feature/done" old))))))

(defun gc-tests--persist (entries)
  "Write ENTRIES as the persisted conversations."
  (with-temp-file aob-acp-persist-file (prin1 entries (current-buffer)))
  (setq aob-acp--persisted-cache nil))

(ert-deftest aob-gc-saved-conversation-in-a-removed-worktree-moves-to-main ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root))
          (aob-acp-persist-file (expand-file-name "persist.eld" gc-tests--base))
          (aob-acp--persisted-cache nil))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--persist
       (list (list :acp-id "x" :dir (file-name-as-directory dir) :project proj
                   :task (expand-file-name "t.md" dir))
             (list :acp-id "y" :dir "/elsewhere/" :project proj)))
      (let ((item (seq-find (lambda (i) (equal (plist-get i :branch) "aob/ff"))
                            (gc-tests--plan proj 'aob))))
        (should (= 1 (plist-get item :saved)))
        (should (string-match-p "1 saved conversation move to" (aob-gc--moves item))))
      (gc-tests--sweep)
      (should-not (file-directory-p dir))
      (let ((moved (aob-acp-persisted-entry "x")))
        (should (equal (plist-get moved :dir) (file-name-as-directory proj)))
        (should (equal (plist-get moved :task) (expand-file-name "t.md" proj))))
      (should (equal (plist-get (aob-acp-persisted-entry "y") :dir) "/elsewhere/")))))

(ert-deftest aob-gc-asleep-session-protects-its-folder ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root)))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (aob-create-session :id "acp:gc-asleep" :backend 'acp :name "asleep"
                          :project proj :dir (file-name-as-directory dir)
                          :state 'asleep)
      (gc-tests--sweep)
      (should (file-directory-p dir))
      (should (member "aob/ff" (gc-tests--branches proj))))))

(defun gc-tests--sync-git ()
  "A `ygg-git-async' that runs git to its end before returning."
  (lambda (root args callback)
    (let ((default-directory (file-name-as-directory root)))
      (with-temp-buffer
        (let ((code (apply #'call-process "git" nil t nil args)))
          (funcall callback (buffer-string) code))))))

(ert-deftest aob-gc-a-failing-step-frees-only-its-own-lock ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root))
          (aob-gc-fetch nil)
          out)
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (puthash "elsewhere" 'token aob-gc--busy)
      (cl-letf (((symbol-function 'ygg-git-async) (gc-tests--sync-git))
                ((symbol-function 'aob-gc--tip)
                 (lambda (&rest _) (error "boom"))))
        (aob-gc--sweep 'aob nil (lambda (r) (setq out r))))
      (should (equal "boom" (plist-get (cadar out) :error)))
      (should (equal '("elsewhere") (hash-table-keys aob-gc--busy))))))

(ert-deftest aob-gc-an-erroring-project-does-not-stop-the-others ()
  (gc-tests--with-repo
    (let* ((proj2 (expand-file-name "proj2" gc-tests--base))
           (dir (expand-file-name "ff" root))
           (dir2 (expand-file-name "gg" root))
           (aob-gc-fetch nil)
           (orig (symbol-function 'aob-gc--tip))
           out)
      (gc-tests--git gc-tests--base "clone" "-q" origin proj2)
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--git proj2 "pull" "-q" "--ff-only")
      (gc-tests--worktree proj2 dir2 "aob/gg" "two")
      (gc-tests--merge-ff proj2 "aob/gg")
      (cl-letf (((symbol-function 'aob-gc--projects) (lambda () (list proj proj2)))
                ((symbol-function 'aob-gc--tip)
                 (lambda (root &rest args)
                   (if (equal root proj) (error "boom") (apply orig root args)))))
        (aob-gc--sweep 'aob nil (lambda (r) (setq out r)))
        (gc-tests--wait (lambda () out)))
      (should (= 2 (length out)))
      (should (plist-get (cadr (assoc proj out)) :error))
      (should (file-directory-p dir))
      (should-not (file-directory-p dir2))
      (should (zerop (hash-table-count aob-gc--busy)))
      (should (string-match-p "failed: .*boom"
                              (string-join (aob-gc--refused out) "\n"))))))

(ert-deftest aob-gc-a-visited-file-protects-the-worktree-in-the-sweep ()
  (gc-tests--with-repo
    (let* ((dir (expand-file-name "ff" root))
           buffer)
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (setq buffer (find-file-noselect (expand-file-name "f.txt" dir)))
      (unwind-protect
          (progn
            (gc-tests--sweep)
            (should (file-directory-p dir))
            (should (member "aob/ff" (gc-tests--branches proj))))
        (kill-buffer buffer))
      (gc-tests--sweep)
      (should-not (file-directory-p dir)))))

(ert-deftest aob-gc-precious-ignored-files-keep-the-worktree-but-plain-ones-do-not ()
  (gc-tests--with-repo
    (let ((pre (expand-file-name "pre" root))
          (plain (expand-file-name "plain" root)))
      (gc-tests--worktree proj pre "aob/pre")
      (gc-tests--commit pre ".gitignore" ".env\nbuild/\n")
      (gc-tests--merge-ff proj "aob/pre")
      (gc-tests--worktree proj plain "aob/plain")
      (gc-tests--merge-ff proj "aob/plain")
      (with-temp-file (expand-file-name ".env" pre) (insert "K=1"))
      (make-directory (expand-file-name "build" plain))
      (with-temp-file (expand-file-name "build/out" plain) (insert "x"))
      (let ((kept (seq-find (lambda (i) (equal (plist-get i :branch) "aob/pre"))
                            (gc-tests--plan proj 'aob))))
        (should (string-match-p "\\.env" (plist-get kept :skip))))
      (gc-tests--sweep)
      (should (file-directory-p pre))
      (should (member "aob/pre" (gc-tests--branches proj)))
      (should-not (file-directory-p plain))
      (should-not (member "aob/plain" (gc-tests--branches proj))))))

(ert-deftest aob-gc-a-tag-named-like-the-branch-does-not-fool-it ()
  (gc-tests--with-repo
    (let ((dir (gc-tests--diverge proj root "t")))
      (gc-tests--git proj "tag" "aob/t" "main")
      (gc-tests--sweep)
      (should (file-directory-p dir))
      (should (member "aob/t" (gc-tests--branches proj))))))

(ert-deftest aob-gc-no-default-branch-removes-nothing ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root)))
      (gc-tests--git proj "remote" "set-head" "origin" "-d")
      (gc-tests--git proj "branch" "-m" "main" "trunk")
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--git proj "merge" "-q" "--ff-only" "aob/ff")
      (gc-tests--sweep)
      (should (file-directory-p dir))
      (should (member "aob/ff" (gc-tests--branches proj))))))

(ert-deftest aob-gc-reap-keeps-a-worktree-whose-branch-is-missing ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root))
          (s nil))
      (gc-tests--worktree proj dir "aob/other" "one")
      (gc-tests--merge-ff proj "aob/other")
      (setq s (aob-create-session :id "acp:gc-reap" :backend 'acp :name "reap"
                                  :project proj :dir (file-name-as-directory dir)
                                  :state 'dead))
      (aob-acp--reap-worktree s)
      (gc-tests--wait (lambda () (seq-some (lambda (m) (string-match-p "kept" m)) messages)))
      (should (file-directory-p dir))
      (should (member "aob/other" (gc-tests--branches proj))))))

(ert-deftest aob-gc-a-precious-file-inside-an-ignored-folder-keeps-the-worktree ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "pre" root)))
      (gc-tests--worktree proj dir "aob/pre")
      (gc-tests--commit dir ".gitignore" "data/\n")
      (gc-tests--merge-ff proj "aob/pre")
      (make-directory (expand-file-name "data" dir))
      (with-temp-file (expand-file-name "data/app.sqlite" dir) (insert "x"))
      (gc-tests--sweep)
      (should (file-directory-p dir))
      (should (member "aob/pre" (gc-tests--branches proj))))))

(defun gc-tests--submodule-worktree (proj dir)
  "Add a merged worktree DIR of PROJ whose submodule mod is checked out."
  (let ((inner (expand-file-name "inner" gc-tests--base)))
    (gc-tests--git gc-tests--base "init" "-q" "-b" "main" inner)
    (gc-tests--commit inner "i" "i")
    (gc-tests--git proj "-c" "protocol.file.allow=always" "submodule" "add" "-q" inner "mod")
    (gc-tests--git proj "commit" "-q" "-m" "sub")
    (gc-tests--git proj "push" "-q" "origin" "main")
    (gc-tests--git proj "branch" "aob/sub")
    (gc-tests--git proj "worktree" "add" "-q" dir "aob/sub")
    (gc-tests--git dir "-c" "protocol.file.allow=always" "submodule" "update" "-q" "--init")))

(ert-deftest aob-gc-a-worktree-with-a-clean-submodule-is-removed ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "sub" root)))
      (gc-tests--submodule-worktree proj dir)
      (gc-tests--sweep)
      (should-not (file-directory-p dir))
      (should-not (member "aob/sub" (gc-tests--branches proj))))))

(ert-deftest aob-gc-a-dirty-submodule-keeps-the-worktree ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "sub" root)))
      (gc-tests--submodule-worktree proj dir)
      (with-temp-file (expand-file-name "mod/scratch" dir) (insert "x"))
      (gc-tests--sweep)
      (should (file-directory-p dir))
      (should (member "aob/sub" (gc-tests--branches proj))))))

(ert-deftest aob-gc-a-locked-worktree-is-kept-quietly-and-shown-as-locked ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root))
          (aob-gc-fetch nil))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--git proj "worktree" "lock" dir)
      (let ((items (cdar (gc-tests--sweep t))))
        (should (equal "locked" (plist-get (car items) :skip))))
      (aob-gc--run t)
      (gc-tests--wait (lambda () messages))
      (should (file-directory-p dir))
      (should (equal '("aob-gc: nothing to remove") messages))
      (should (member "aob/ff" (gc-tests--branches proj))))))

(ert-deftest aob-gc-background-sweeps-leave-out-refused-removals ()
  (let ((results '(("p" (:branch "b" :skip "boom" :failed t) (:error "bad")))))
    (should (equal '("failed: p: bad") (aob-gc--refused results)))
    (should (= 2 (length (aob-gc--refused results t))))))

(ert-deftest aob-gc-an-errored-project-is-not-counted-as-removed ()
  (should-not (aob-gc--removed '(("p" (:error "boom")))))
  (should (equal '("branch b") (aob-gc--removed '(("p" (:error "x") (:branch "b")))))))

(ert-deftest aob-gc-a-deleted-branch-loses-its-config-section ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root)))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--git proj "config" "branch.aob/ff.remote" "origin")
      (gc-tests--sweep)
      (should-not (member "aob/ff" (gc-tests--branches proj)))
      (should-error (gc-tests--git proj "config" "--get" "branch.aob/ff.remote")))))

(ert-deftest aob-gc-dependency-dirs-and-env-templates-are-not-precious ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "dep" root))
          (tpl (expand-file-name "tpl" root)))
      (gc-tests--worktree proj dir "aob/dep")
      (gc-tests--commit dir ".gitignore" "node_modules/\n")
      (gc-tests--merge-ff proj "aob/dep")
      (make-directory (expand-file-name "node_modules/@aws-sdk/client-secrets-manager" dir) t)
      (with-temp-file (expand-file-name "node_modules/@aws-sdk/client-secrets-manager/x.js" dir)
        (insert "x"))
      (with-temp-file (expand-file-name "node_modules/.env" dir) (insert "x"))
      (gc-tests--worktree proj tpl "aob/tpl")
      (gc-tests--commit tpl ".gitignore" ".env.example\n")
      (gc-tests--merge-ff proj "aob/tpl")
      (with-temp-file (expand-file-name ".env.example" tpl) (insert "x"))
      (gc-tests--sweep)
      (should-not (file-directory-p dir))
      (should-not (file-directory-p tpl)))))

(ert-deftest aob-gc-precious-matches-the-base-name-only ()
  (should (aob-gc--precious-p ".env"))
  (should (aob-gc--precious-p "data/app.sqlite"))
  (should (aob-gc--precious-p "cfg/credentials.json"))
  (should-not (aob-gc--precious-p ".env.example"))
  (should-not (aob-gc--precious-p "a/.env.template"))
  (should-not (aob-gc--precious-p "secrets-dir/readme.txt"))
  (should-not (aob-gc--precious-p "x.env/y.txt")))

(ert-deftest aob-gc-a-modified-buffer-without-a-file-does-not-protect ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root)))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (with-temp-buffer
        (setq default-directory (file-name-as-directory dir))
        (insert "x")
        (should (buffer-modified-p))
        (should-not (aob-gc--visited-p dir)))
      (let ((buffer (find-file-noselect (expand-file-name "f.txt" dir))))
        (unwind-protect
            (with-current-buffer buffer
              (insert "x")
              (should (aob-gc--visited-p dir)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest aob-gc-a-hanging-gh-is-killed-and-the-lock-released ()
  (gc-tests--with-repo
    (let* ((aob-gc-fetch nil)
           (aob-gc-fetch-timeout 1)
           (dir (gc-tests--diverge proj root "slow"))
           (bin (gc-tests--fake-gh "exec sleep 30"))
           (exec-path (cons bin exec-path))
           (process-environment (cons (concat "PATH=" bin ":" (getenv "PATH"))
                                      process-environment))
           (start (float-time)))
      (gc-tests--git proj "remote" "set-url" "origin" "https://github.com/x/y.git")
      (gc-tests--sweep)
      (should (< (- (float-time) start) 15))
      (should (file-directory-p dir))
      (should (zerop (hash-table-count aob-gc--busy)))
      (should-not (seq-some (lambda (p) (and (process-live-p p)
                                             (string-match-p "forge" (process-name p))))
                            (process-list))))))

(ert-deftest aob-gc-preview-does-not-fetch-but-a-sweep-does ()
  (gc-tests--with-repo
    (let ((other (expand-file-name "other" gc-tests--base))
          (before (gc-tests--sha proj "origin/main")))
      (gc-tests--git gc-tests--base "clone" "-q" origin other)
      (gc-tests--commit other "n" "n")
      (gc-tests--git other "push" "-q" "origin" "main")
      (gc-tests--sweep t)
      (should (equal before (gc-tests--sha proj "origin/main")))
      (gc-tests--sweep)
      (should-not (equal before (gc-tests--sha proj "origin/main"))))))

(ert-deftest aob-gc-a-hanging-fetch-is-killed-and-local-refs-are-used ()
  (gc-tests--with-repo
    (let ((dir (expand-file-name "ff" root))
          (aob-gc-fetch-timeout 1))
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--git proj "config" "remote.origin.uploadpack" "sleep 5; git-upload-pack")
      (gc-tests--sweep)
      (should-not (file-directory-p dir)))))

(defun gc-tests--ssh-script ()
  "A fake ssh that logs its arguments; returns (SCRIPT . LOG)."
  (let ((log (expand-file-name "ssh.log" gc-tests--base))
        (script (expand-file-name "ssh.sh" gc-tests--base)))
    (with-temp-file script (insert (format "#!/bin/sh\necho \"$@\" > %s\nexit 1\n" log)))
    (set-file-modes script #o755)
    (cons script log)))

(defun gc-tests--logged (log)
  "The contents of LOG."
  (with-temp-buffer (insert-file-contents log) (buffer-string)))

(ert-deftest aob-gc-fetch-ssh-never-asks-and-keeps-the-configured-command ()
  (gc-tests--with-repo
    (let* ((ssh (gc-tests--ssh-script))
           (process-environment (cons (format "GIT_SSH_COMMAND=%s" (car ssh))
                                      process-environment))
           done)
      (gc-tests--git proj "remote" "set-url" "origin" "ssh://host/repo.git")
      (aob-gc--fetch proj (lambda () (setq done t)))
      (gc-tests--wait (lambda () done))
      (should (string-match-p "-o BatchMode=yes" (gc-tests--logged (cdr ssh)))))))

(ert-deftest aob-gc-fetch-honours-core-ssh-command ()
  (gc-tests--with-repo
    (let* ((ssh (gc-tests--ssh-script))
           (process-environment (cons "GIT_SSH_COMMAND" process-environment))
           done)
      (gc-tests--git proj "config" "core.sshCommand" (car ssh))
      (gc-tests--git proj "remote" "set-url" "origin" "ssh://host/repo.git")
      (aob-gc--fetch proj (lambda () (setq done t)))
      (gc-tests--wait (lambda () done))
      (should (string-match-p "-o BatchMode=yes" (gc-tests--logged (cdr ssh)))))))

(ert-deftest aob-gc-an-entry-under-a-symlink-moves-when-git-reports-the-real-path ()
  (gc-tests--with-repo
    (let* ((dir (expand-file-name "ff" root))
           (link (expand-file-name "link" gc-tests--base))
           (aob-acp-persist-file (expand-file-name "persist.eld" gc-tests--base))
           (aob-acp--persisted-cache nil))
      (make-symbolic-link (directory-file-name root) link)
      (gc-tests--worktree proj dir "aob/ff" "one")
      (gc-tests--merge-ff proj "aob/ff")
      (gc-tests--persist
       (list (list :acp-id "x" :dir (file-name-as-directory (expand-file-name "ff" link))
                   :project proj :task (expand-file-name "ff/t.md" link))))
      (gc-tests--sweep)
      (should-not (file-directory-p dir))
      (let ((moved (aob-acp-persisted-entry "x")))
        (should (equal (plist-get moved :dir) (file-name-as-directory proj)))
        (should (equal (plist-get moved :task) (expand-file-name "t.md" proj)))))))

(ert-deftest aob-gc-a-grace-longer-than-twice-the-interval-still-sweeps ()
  (let ((aob-gc-interval 60)
        (aob-gc-grace 300)
        (aob-gc--timer nil)
        (aob-gc--expected nil)
        (aob-gc--last-tick nil)
        (aob-gc--idle nil)
        (runs 0))
    (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) 'timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'current-idle-time) (lambda () '(1 0 0 0)))
              ((symbol-function 'aob-gc--run) (lambda (_) (cl-incf runs))))
      (aob-gc--arm aob-gc-grace)
      (setq aob-gc--last-tick (- (float-time) 300))
      (aob-gc--tick)
      (should (= 1 runs))
      (setq aob-gc--last-tick (- (float-time) 60))
      (aob-gc--tick)
      (should (= 2 runs))
      (setq aob-gc--last-tick (- (float-time) 500))
      (aob-gc--tick)
      (should (= 2 runs)))))

(ert-deftest aob-gc-busy-ticks-keep-one-pending-idle-timer ()
  (let ((aob-gc--last-tick (float-time))
        (aob-gc-mode t)
        (aob-gc--idle nil))
    (unwind-protect
        (cl-letf (((symbol-function 'current-idle-time) #'ignore))
          (aob-gc--tick)
          (aob-gc--tick)
          (should (= 1 (seq-count (lambda (tm) (eq (timer--function tm) #'aob-gc--when-idle))
                                  timer-idle-list))))
      (when aob-gc--idle (cancel-timer aob-gc--idle)))))

(provide 'aob-gc-tests)
;;; aob-gc-tests.el ends here
