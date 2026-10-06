;;; ygg-git-compare-async-tests.el --- nothing in a compare waits for the network -*- lexical-binding: t; -*-

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

(defconst ygg-git-compare-async-tests--budget 0.1
  "Seconds a call has to return in, besides the time its local git runs take.")

(defconst ygg-git-compare-async-tests--real-git (executable-find "git"))

(defconst ygg-git-compare-async-tests--scripts
  '(("gh" . "#!/bin/sh
echo \"$*\" >> \"$FAKE_DIR/gh.log\"
env | grep -E '^(GH_PROMPT_DISABLED|GIT_TERMINAL_PROMPT)=' >> \"$FAKE_DIR/gh.env\"
[ -f \"$FAKE_DIR/gh.delay\" ] && sleep \"$(cat \"$FAKE_DIR/gh.delay\")\"
case \"$1 $2\" in
  \"pr list\") cat \"$FAKE_DIR/pulls.json\";;
  \"pr view\") cat \"$FAKE_DIR/pr-view.json\";;
  \"repo view\") cat \"$FAKE_DIR/repo-url.txt\";;
  *) echo \"unexpected: $*\" >&2; exit 2;;
esac
")
    ("glab" . "#!/bin/sh
echo \"$*\" >> \"$FAKE_DIR/glab.log\"
[ -f \"$FAKE_DIR/gh.delay\" ] && sleep \"$(cat \"$FAKE_DIR/gh.delay\")\"
cat \"$FAKE_DIR/mrs.json\"
")
    ("git" . "#!/bin/sh
case \"$1\" in fetch)
  echo \"$*\" >> \"$FAKE_DIR/git.log\"
  env | grep -E '^(GIT_TERMINAL_PROMPT|GH_PROMPT_DISABLED|GIT_SSH_COMMAND)=' >> \"$FAKE_DIR/git.env\"
  [ -f \"$FAKE_DIR/git.delay\" ] && sleep \"$(cat \"$FAKE_DIR/git.delay\")\";;
esac
exec \"$REAL_GIT\" \"$@\"
")
    ("both" . "#!/bin/sh
echo out
echo err >&2
exit 3
")
    ("slow" . "#!/bin/sh
sleep 5
"))
  "The programs a test puts first on PATH, by name.")

(defun ygg-git-compare-async-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process ygg-git-compare-async-tests--real-git
                            nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-compare-async-tests--put (file text)
  (with-temp-file file (insert text)))

(defun ygg-git-compare-async-tests--read (file)
  (when (file-exists-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun ygg-git-compare-async-tests--wait (pred &optional seconds)
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall pred))))

(defun ygg-git-compare-async-tests--await (start)
  "What START, given the callback it is to call, answers it."
  (let (result)
    (funcall start (lambda (&rest answer) (setq result (or answer '(none)))))
    (ygg-git-compare-async-tests--wait (lambda () result))
    result))

(defmacro ygg-git-compare-async-tests--with-world (vars &rest body)
  "A repository whose origin is github.com/o/r, served from a local bare
repository that has pull request 7, with fake gh, glab and git first on
PATH.  VARS binds (ROOT FAKE BASE HEAD); HEAD is the pull request's head,
a commit ROOT does not have."
  (declare (indent 1))
  `(let* ((outer (file-name-as-directory
                  (file-truename (make-temp-file "ygg-git-compare-async-" t))))
          (,(nth 0 vars) (file-name-as-directory (expand-file-name "work" outer)))
          (,(nth 1 vars) (file-name-as-directory (expand-file-name "fake" outer)))
          (remote (expand-file-name "remote.git" outer))
          (clone (expand-file-name "clone" outer))
          (path (concat ,(nth 1 vars) ":" (getenv "PATH")))
          (process-environment
           (append (list "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                         (concat "PATH=" path) (concat "FAKE_DIR=" ,(nth 1 vars))
                         (concat "REAL_GIT=" ygg-git-compare-async-tests--real-git)
                         "GIT_SSH_COMMAND" "GIT_SSH" "LAB_HOST")
                   process-environment))
          (exec-path (cons (directory-file-name ,(nth 1 vars)) exec-path))
          (magit-git-executable ygg-git-compare-async-tests--real-git)
          (ygg-git-compare--ssh-hostnames
           (let ((hostnames (make-hash-table :test #'equal)))
             (puthash "github.com" "github.com" hostnames)
             hostnames))
          (magit-refresh-verbose nil)
          (default-directory (progn (make-directory ,(nth 0 vars) t)
                                    (make-directory ,(nth 1 vars) t)
                                    ,(nth 0 vars)))
          ,(nth 2 vars) ,(nth 3 vars))
     (unwind-protect
         (cl-flet ((git (dir &rest args)
                     (apply #'ygg-git-compare-async-tests--git dir args)))
           (pcase-dolist (`(,name . ,text) ygg-git-compare-async-tests--scripts)
             (let ((file (expand-file-name name ,(nth 1 vars))))
               (ygg-git-compare-async-tests--put file text)
               (set-file-modes file #o755)))
           (git ,(nth 0 vars) "init" "-q" "-b" "main")
           (git ,(nth 0 vars) "config" "user.name" "Async Test")
           (git ,(nth 0 vars) "config" "user.email" "async@example.invalid")
           (git ,(nth 0 vars) "config" "commit.gpgsign" "false")
           (ygg-git-compare-async-tests--put (expand-file-name "a.txt" ,(nth 0 vars)) "1\n")
           (git ,(nth 0 vars) "add" ".")
           (git ,(nth 0 vars) "commit" "-q" "-m" "base")
           (setq ,(nth 2 vars) (git ,(nth 0 vars) "rev-parse" "HEAD"))
           (git outer "init" "-q" "--bare" remote)
           (git outer "clone" "-q" ,(nth 0 vars) clone)
           (git clone "config" "user.name" "Async Test")
           (git clone "config" "user.email" "async@example.invalid")
           (git clone "config" "commit.gpgsign" "false")
           (ygg-git-compare-async-tests--put (expand-file-name "a.txt" clone) "1\n2\n")
           (git clone "commit" "-q" "-am" "work")
           (setq ,(nth 3 vars) (git clone "rev-parse" "HEAD"))
           (git clone "push" "-q" remote "HEAD:refs/pull/7/head"
                (concat ,(nth 2 vars) ":refs/heads/main"))
           (git ,(nth 0 vars) "remote" "add" "origin" "git@github.com:o/r.git")
           (git ,(nth 0 vars) "config" (concat "url." remote ".insteadOf") "git@github.com:o/r.git")
           (ygg-git-compare-async-tests--put
            (expand-file-name "pulls.json" ,(nth 1 vars))
            "[{\"number\":7,\"title\":\"Add two\",\"headRefName\":\"feature\",\"baseRefName\":\"main\"}]")
           (ygg-git-compare-async-tests--put
            (expand-file-name "pr-view.json" ,(nth 1 vars))
            (format "{\"number\":7,\"state\":\"OPEN\",\"baseRefOid\":\"%s\",\"headRefOid\":\"%s\",\"baseRefName\":\"main\",\"url\":\"https://github.com/o/r/pull/7\"}"
                    ,(nth 2 vars) ,(nth 3 vars)))
           (ygg-git-compare-async-tests--put
            (expand-file-name "repo-url.txt" ,(nth 1 vars)) "https://github.com/o/r\n")
           (ygg-git-compare-async-tests--put
            (expand-file-name "mrs.json" ,(nth 1 vars)) "[]")
           ,@body)
       (clrhash ygg-git-compare--inflight)
       (setq ygg-git-compare--fetching nil ygg-git-compare--fetch-queue nil)
       (dolist (buffer (buffer-list))
         (when (and (buffer-live-p buffer)
                    (buffer-local-value 'ygg-git-compare--a-spec buffer))
           (ygg-git-compare--close buffer)))
       (delete-directory outer t))))

(defun ygg-git-compare-async-tests--seed (key value &optional age)
  "Keep VALUE under KEY here as found AGE seconds ago."
  (puthash key (list :value value :time (- (float-time) (or age 100000)))
           (ygg-git-compare--cache-table (magit-gitdir))))

(defun ygg-git-compare-async-tests--seed-all (base head)
  "Every forge answer a compare asks for, long stale."
  (let ((repo '(github "github.com" "o/r")))
    (ygg-git-compare-async-tests--seed '(gh-url) "https://github.com/o/r")
    (ygg-git-compare-async-tests--seed
     (list 'pulls repo)
     '((:number 7 :title "Add two" :headRefName "feature" :baseRefName "main")))
    (ygg-git-compare-async-tests--seed
     (ygg-git-compare--pr-key repo 7)
     (list :forge 'github :host "github.com" :path "o/r" :number 7 :head head
           :start base :base-ref "main" :url "https://github.com/o/r/pull/7"))))

(defun ygg-git-compare-async-tests--local-git-p (program args)
  (and (equal (file-name-nondirectory program) "git")
       (not (seq-some (lambda (arg) (member arg '("fetch" "pull" "push" "clone" "ls-remote")))
                      args))))

(defmacro ygg-git-compare-async-tests--timed (total &rest body)
  "Run BODY, adding the seconds it took to the variable TOTAL."
  (declare (indent 1))
  `(let ((begun (float-time)))
     (unwind-protect (progn ,@body)
       (cl-incf ,total (- (float-time) begun)))))

(defmacro ygg-git-compare-async-tests--unblocked (&rest body)
  "Run BODY, failing when it waits on a process, sleeps, runs anything but a
local git to completion, or takes longer than the budget once the time
those git runs took is taken off."
  (declare (indent 0))
  `(let ((call-process-real (symbol-function 'call-process))
         (process-file-real (symbol-function 'process-file))
         (spent 0.0)
         (started (float-time)))
     (cl-letf (((symbol-function 'accept-process-output)
                (lambda (&rest _) (error "Waited on a process")))
               ((symbol-function 'sleep-for)
                (lambda (&rest _) (error "Slept")))
               ((symbol-function 'call-process)
                (lambda (program &rest args)
                  (unless (ygg-git-compare-async-tests--local-git-p program args)
                    (error "%s run to completion" program))
                  (ygg-git-compare-async-tests--timed spent
                    (apply call-process-real program args))))
               ((symbol-function 'process-file)
                (lambda (program &rest args)
                  (unless (ygg-git-compare-async-tests--local-git-p program (nthcdr 3 args))
                    (error "%s run to completion" program))
                  (ygg-git-compare-async-tests--timed spent
                    (apply process-file-real program args)))))
       ,@body)
     (should (< (- (float-time) started spent) ygg-git-compare-async-tests--budget))))

(defun ygg-git-compare-async-tests--install-trampolines ()
  (dolist (primitive '(call-process sleep-for accept-process-output))
    (let ((real (symbol-function primitive)))
      (cl-letf (((symbol-function primitive) (lambda (&rest args) (apply real args)))))))) 

(ygg-git-compare-async-tests--install-trampolines)

(defun ygg-git-compare-async-tests--idle-p ()
  (zerop (hash-table-count ygg-git-compare--inflight)))

(ert-deftest ygg-git-compare-async-keeps-both-streams-and-the-status ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (should (equal (ygg-git-compare-async-tests--await
                    (lambda (done) (ygg-git-compare--forge-async "both" nil done)))
                   '(3 "out\n" "err")))))

(ert-deftest ygg-git-compare-async-reports-a-program-that-will-not-start ()
  (should (equal (ygg-git-compare-async-tests--await
                  (lambda (done)
                    (ygg-git-compare--forge-async "ygg-no-such-program" nil done)))
                 '(nil "" ""))))

(ert-deftest ygg-git-compare-async-kills-what-outlasts-its-timeout ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (let ((started (float-time)))
      (should (equal (car (ygg-git-compare-async-tests--await
                           (lambda (done) (ygg-git-compare--forge-async "slow" nil done 0.3))))
                     'timeout))
      (should (< (- (float-time) started) 3))
      (should-not (seq-some (lambda (p) (string-prefix-p "ygg-git-compare-forge" (process-name p)))
                            (process-list))))))

(ert-deftest ygg-git-compare-async-nothing-asks-a-question ()
  (ygg-git-compare-async-tests--with-world (root fake _base _head)
    (ygg-git-compare-async-tests--await
     (lambda (done) (ygg-git-compare--forge-async "git" '("fetch" "--quiet" "origin" "main") done)))
    (let ((env (ygg-git-compare-async-tests--read (expand-file-name "git.env" fake))))
      (should (string-search "GIT_TERMINAL_PROMPT=0" env))
      (should (string-search "GH_PROMPT_DISABLED=1" env))
      (should (string-search "GIT_SSH_COMMAND=ssh -o BatchMode=yes" env)))
    (let ((default-directory "/ssh:nowhere:/tmp/"))
      (ygg-git-compare-async-tests--await
       (lambda (done) (ygg-git-compare--forge-async "gh" '("repo" "view") done))))
    (let ((env (ygg-git-compare-async-tests--read (expand-file-name "gh.env" fake))))
      (should (string-search "GIT_TERMINAL_PROMPT=0" env))
      (should (string-search "GH_PROMPT_DISABLED=1" env)))
    (should (file-directory-p root))))

(ert-deftest ygg-git-compare-async-fetch-keeps-a-ssh-command-the-user-set ()
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
    (let ((process-environment (cons "GIT_SSH_COMMAND=ssh -i key" process-environment)))
      (ygg-git-compare-async-tests--await
       (lambda (done) (ygg-git-compare--forge-async "git" '("fetch" "origin" "main") done))))
    (should (string-search "GIT_SSH_COMMAND=ssh -i key -o BatchMode=yes"
                           (ygg-git-compare-async-tests--read (expand-file-name "git.env" fake))))))

(ert-deftest ygg-git-compare-async-cache-refreshes-once-for-everyone-who-asks ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (ygg-git-compare-async-tests--seed '(k) "old")
    (let ((fetches 0) land (heard nil))
      (cl-flet ((ask (tag)
                  (ygg-git-compare--cached
                   '(k) 60 (lambda (done) (cl-incf fetches) (setq land done))
                   (lambda (value err) (push (list tag value err) heard)))))
        (should (equal (ask 'a) "old"))
        (should (equal (ask 'b) "old"))
        (should (equal (ask 'c) "old"))
        (should (= fetches 1))
        (should (ygg-git-compare--refreshing-p 'k))
        (funcall land "new")
        (should (equal (nreverse heard) '((a "new" nil) (b "new" nil) (c "new" nil))))
        (should-not (ygg-git-compare--refreshing-p 'k))
        (should (equal (ask 'd) "new"))
        (should (= fetches 1))))))

(ert-deftest ygg-git-compare-async-cache-keeps-the-old-answer-through-a-failure ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (ygg-git-compare-async-tests--seed '(k) "old")
    (let ((fetches 0) land (heard nil)
          (ygg-git-compare-forge-retry 1000))
      (cl-flet ((ask ()
                  (ygg-git-compare--cached
                   '(k) 60 (lambda (done) (cl-incf fetches) (setq land done))
                   (lambda (value err) (push (list value err) heard)))))
        (should (equal (ask) "old"))
        (funcall land nil "gh timed out")
        (should (equal heard '((nil "gh timed out"))))
        (should (equal (ask) "old"))
        (should (= fetches 1))
        (should (equal (plist-get (ygg-git-compare--cache-entry '(k)) :error) "gh timed out"))
        (let ((ygg-git-compare-forge-retry 0))
          (sleep-for 0.01)
          (should (equal (ask) "old"))
          (should (= fetches 2)))))))

(ert-deftest ygg-git-compare-async-a-timeout-is-an-error-and-is-tried-again ()
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
    (let ((ygg-git-compare-forge-timeout 0.3)
          (ygg-git-compare-forge-retry 0))
      (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "5")
      (let ((landed nil))
        (ygg-git-compare--cached
         '(k) 60
         (lambda (done)
           (ygg-git-compare--forge-async
            "gh" '("repo" "view")
            (lambda (status text err)
              (ygg-git-compare--answer done "gh" status text err #'string-trim))))
         (lambda (value err) (setq landed (list value err))))
        (ygg-git-compare-async-tests--wait (lambda () landed))
        (should (equal landed '(nil "gh timed out"))))
      (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0")
      (sleep-for 0.01)
      (let ((landed nil))
        (ygg-git-compare--cached
         '(k) 60
         (lambda (done)
           (ygg-git-compare--forge-async
            "gh" '("repo" "view")
            (lambda (status text err)
              (ygg-git-compare--answer done "gh" status text err #'string-trim))))
         (lambda (value err) (setq landed (list value err))))
        (ygg-git-compare-async-tests--wait (lambda () landed))
        (should (equal landed '("https://github.com/o/r" nil)))))))

(ert-deftest ygg-git-compare-async-cache-is-kept-on-disk-for-a-cold-start ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (ygg-git-compare--cached '(k) 60 (lambda (done) (funcall done '(:x "kept"))) nil)
    (should (file-exists-p (expand-file-name "ygg-forge-cache.eld" (magit-gitdir))))
    (clrhash ygg-git-compare--caches)
    (let ((fetches 0))
      (should (equal (ygg-git-compare--cached
                      '(k) 60 (lambda (_done) (cl-incf fetches)) nil)
                     '(:x "kept")))
      (should (= fetches 0))
      (should (equal (ygg-git-compare--cached
                      '(k) -1 (lambda (_done) (cl-incf fetches)) nil)
                     '(:x "kept")))
      (should (= fetches 1)))
    (should-not (directory-files (magit-gitdir) nil "\\`ygg-forge-cache.eld."))))

(ert-deftest ygg-git-compare-async-a-forge-repo-is-worked-out-once ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (let ((asked 0)
          (hostname (symbol-function 'ygg-git-compare--ssh-hostname)))
      (clrhash ygg-git-compare--forge-repos)
      (cl-letf (((symbol-function 'ygg-git-compare--ssh-hostname)
                 (lambda (alias) (cl-incf asked) (funcall hostname alias))))
        (should (equal (ygg-git-compare--forge-repo "origin") '(github "github.com" "o/r")))
        (should (equal (ygg-git-compare--forge-repo "origin") '(github "github.com" "o/r")))
        (should (= asked 1))))))

(ert-deftest ygg-git-compare-async-the-pr-remote-needs-no-gh-round-trip ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (ygg-git-compare-async-tests--unblocked
      (should (equal (ygg-git-compare--pr-remote) "origin")))))

(ert-deftest ygg-git-compare-async-opening-a-picker-waits-for-nothing ()
  (ygg-git-compare-async-tests--with-world (root fake base head)
    (ygg-git-compare-async-tests--seed-all base head)
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "1")
    (let ((tables nil) (prompts nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt table &rest _) (push prompt prompts) (push table tables) "main")))
        (ygg-git-compare-async-tests--unblocked (ygg-git-compare--read-sides))
        (ygg-git-compare-async-tests--unblocked (ygg-git-compare--review-targets))
        (ygg-git-compare-async-tests--unblocked
          (ygg-git-compare--read "Review" #'ygg-git-compare--review-targets nil))
        (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "main"))
          (ygg-git-compare-async-tests--unblocked (ygg-git-compare-switch-base)))
        (when (require 'ygg-git-worktree nil t)
          (with-temp-buffer
            (ygg-git-compare-async-tests--unblocked
              (ygg-git-worktree--spec-at-point)))))
      (should (seq-some (lambda (p) (string-search "(refreshing PRs…)" p)) prompts))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p))))

(ert-deftest ygg-git-compare-async-a-worktree-for-an-unfetched-pull-request-says-so ()
  (skip-unless (require 'ygg-git-worktree nil t))
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
    (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "0.3")
    (let ((spec (cons 'pr (list :number 7 :head "feature" :remote "origin"))))
      (should (string-search "fetching PR #7"
                             (cadr (should-error (ygg-git-worktree-run spec) :type 'user-error))))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p))))

(ert-deftest ygg-git-compare-async-pull-requests-reach-an-open-picker ()
  (ygg-git-compare-async-tests--with-world (_root fake base head)
    (ygg-git-compare-async-tests--seed-all base head)
    (puthash (list 'pulls '(github "github.com" "o/r")) (list :value nil :time 0)
             (ygg-git-compare--cache-table (magit-gitdir)))
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0.3")
    (let (table nudged)
      (cl-letf (((symbol-function 'ygg-git-compare--nudge)
                 (lambda (tbl) (setq nudged (eq tbl table))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt tbl &rest _)
                   (setq table tbl)
                   (should-not (seq-find (lambda (c) (string-prefix-p "#7" c))
                                         (all-completions "" tbl)))
                   (ygg-git-compare-async-tests--wait (lambda () nudged))
                   (should (seq-find (lambda (c) (string-prefix-p "#7" c))
                                     (all-completions "" tbl)))
                   "main")))
        (ygg-git-compare--read "Compare" (apply-partially #'ygg-git-compare-candidates t) nil)))))

(ert-deftest ygg-git-compare-async-a-pull-request-window-opens-first-and-draws-again-once ()
  (ygg-git-compare-async-tests--with-world (root fake _base head)
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0.3")
    (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "0.3")
    (let ((redraws 0) (notes nil) buffer)
      (advice-add 'ygg-git-compare--redraw :before (lambda (&rest _) (cl-incf redraws))
                  '((name . count)))
      (advice-add 'ygg-git-compare--review-note :before (lambda (_buffer note) (push note notes))
                  '((name . note)))
      (unwind-protect
          (progn
            (ygg-git-compare-async-tests--unblocked
              (setq buffer (ygg-git-compare-review-branch
                            (cons 'pr (list :number 7 :head "feature")))))
            (should (buffer-live-p buffer))
            (should (= redraws 0))
            (should (string-search "looking up" (format "%s" (buffer-local-value 'header-line-format buffer))))
            (ygg-git-compare-async-tests--wait
             (lambda () (eq (car-safe (buffer-local-value 'ygg-git-compare--b-spec buffer)) 'pr)))
            (ygg-git-compare-async-tests--wait
             (lambda () (null (buffer-local-value 'ygg-git-compare--note buffer))))
            (should (= redraws 1))
            (should (member "fetching PR #7…" notes))
            (should (equal (plist-get (buffer-local-value 'ygg-git-compare--b buffer) :diff) head))
            (should-not (string-search "fetching" (format "%s" (buffer-local-value 'header-line-format buffer))))
            (should (equal (ygg-git-compare-async-tests--git root "for-each-ref" "refs/pull") "")))
        (advice-remove 'ygg-git-compare--redraw 'count)
        (advice-remove 'ygg-git-compare--review-note 'note)))))

(ert-deftest ygg-git-compare-async-a-cached-pull-request-opens-in-full-at-once ()
  (ygg-git-compare-async-tests--with-world (root _fake base head)
    (ygg-git-compare-async-tests--git root "fetch" "-q" "origin" "refs/pull/7/head")
    (ygg-git-compare-async-tests--seed-all base head)
    (let (buffer)
      (ygg-git-compare-async-tests--unblocked
        (setq buffer (ygg-git-compare-review-branch (cons 'pr (list :number 7 :head "feature")))))
      (should (equal (plist-get (buffer-local-value 'ygg-git-compare--b buffer) :diff) head))
      (should (equal (cdr (buffer-local-value 'ygg-git-compare--a-spec buffer)) base))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p))))

(ert-deftest ygg-git-compare-async-redrawing-an-unfetched-pull-request-fetches-it-once ()
  (ygg-git-compare-async-tests--with-world (root fake _base head)
    (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "0.3")
    (let ((spec (cons 'pr (list :number 7 :head "feature" :base "main" :remote "origin")))
          buffer)
      (setq buffer (ygg-git-compare-buffer root '(rev . "main") spec))
      (with-current-buffer buffer
        (should (string-search "fetching PR #7…" (format "%s" header-line-format)))
        (ygg-git-compare-async-tests--unblocked
          (dotimes (_ 3) (ygg-git-compare--redraw))))
      (ygg-git-compare-async-tests--wait
       (lambda () (not (plist-get (buffer-local-value 'ygg-git-compare--b buffer) :pending))))
      (should (equal (plist-get (buffer-local-value 'ygg-git-compare--b buffer) :diff) head))
      (should (= (length (split-string (ygg-git-compare-async-tests--read
                                        (expand-file-name "git.log" fake))
                                       "\n" t))
                 1)))))

(ert-deftest ygg-git-compare-async-a-failed-fetch-is-said-in-the-header ()
  (ygg-git-compare-async-tests--with-world (root _fake _base _head)
    (ygg-git-compare-async-tests--git root "remote" "set-url" "origin" "/nonexistent/remote.git")
    (let ((buffer (ygg-git-compare-buffer
                   root '(rev . "main")
                   (cons 'pr (list :number 7 :head "feature" :remote "origin")))))
      (ygg-git-compare-async-tests--wait
       (lambda ()
         (string-search "not fetched" (format "%s" (buffer-local-value 'header-line-format
                                                                       buffer))))))))

(ert-deftest ygg-git-compare-async-this-pr-reads-the-cache-first ()
  (ygg-git-compare-async-tests--with-world (root fake base head)
    (ygg-git-compare-async-tests--git root "fetch" "-q" "origin" "refs/pull/7/head")
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "1")
    (with-current-buffer (ygg-git-compare-buffer
                          root (cons 'rev base)
                          (cons 'pr (list :number 7 :sha head :head "feature" :base "main"
                                          :remote "origin")))
      (let ((error nil))
        (ygg-git-compare-async-tests--unblocked
          (setq error (cadr (should-error (ygg-git-compare--this-pr) :type 'user-error))))
        (should (string-search "Looking up #7" error)))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p)
      (ygg-git-compare-async-tests--seed (ygg-git-compare--pr-key '(github "github.com" "o/r") 7)
                                         (ygg-git-compare--cache-value
                                          (ygg-git-compare--pr-key '(github "github.com" "o/r") 7)))
      (let (pr)
        (ygg-git-compare-async-tests--unblocked
          (setq pr (ygg-git-compare--this-pr)))
        (should (equal (plist-get pr :number) 7))
        (should (equal (plist-get pr :head) head)))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p))))

(defun ygg-git-compare-async-tests--header (buffer)
  (format "%s" (buffer-local-value 'header-line-format buffer)))

(defun ygg-git-compare-async-tests--cache-file ()
  (ygg-git-compare--cache-file (magit-gitdir)))

(defun ygg-git-compare-async-tests--cache-rows ()
  (with-temp-buffer
    (insert-file-contents (ygg-git-compare-async-tests--cache-file))
    (read (current-buffer))))

(defun ygg-git-compare-async-tests--reload (data)
  "Have the cache read DATA, written as the file's text, afresh."
  (with-temp-file (ygg-git-compare-async-tests--cache-file)
    (if (stringp data) (insert data) (prin1 data (current-buffer))))
  (clrhash ygg-git-compare--caches)
  (ygg-git-compare--cache-table (magit-gitdir)))

(ert-deftest ygg-git-compare-async-a-picker-open-makes-few-git-calls ()
  (ygg-git-compare-async-tests--with-world (_root _fake base head)
    (ygg-git-compare-async-tests--seed-all base head)
    (let ((calls 0) (listed 0))
      (advice-add 'process-file :before (lambda (&rest _) (cl-incf calls)) '((name . count-git)))
      (advice-add 'ygg-git-compare-candidates :before (lambda (&rest _) (cl-incf listed))
                  '((name . count-listing)))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "main")))
            (ygg-git-compare--read-sides)
            (should (= listed 1))
            (should (<= calls 40)))
        (advice-remove 'process-file 'count-git)
        (advice-remove 'ygg-git-compare-candidates 'count-listing)))))

(ert-deftest ygg-git-compare-async-an-open-compare-follows-a-pull-request-head-that-moved ()
  (ygg-git-compare-async-tests--with-world (root fake base head)
    (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "0.3")
    (ygg-git-compare-async-tests--seed (ygg-git-compare--head-key "origin" 7) base 100000)
    (let ((buffer (ygg-git-compare-buffer
                   root '(rev . "main")
                   (cons 'pr (list :number 7 :head "feature" :remote "origin")))))
      (should (equal (plist-get (buffer-local-value 'ygg-git-compare--b buffer) :diff) base))
      (should (string-search "head as of 27h; refreshing…"
                             (ygg-git-compare-async-tests--header buffer)))
      (ygg-git-compare-async-tests--wait
       (lambda ()
         (equal (plist-get (buffer-local-value 'ygg-git-compare--b buffer) :diff) head)))
      (should-not (string-search "head as of" (ygg-git-compare-async-tests--header buffer))))))

(ert-deftest ygg-git-compare-async-a-fresh-pull-request-head-says-nothing-of-its-age ()
  (ygg-git-compare-async-tests--with-world (root _fake base _head)
    (ygg-git-compare-async-tests--seed (ygg-git-compare--head-key "origin" 7) base 5)
    (let ((buffer (ygg-git-compare-buffer
                   root '(rev . "main")
                   (cons 'pr (list :number 7 :head "feature" :remote "origin")))))
      (should-not (string-search "head as of" (ygg-git-compare-async-tests--header buffer)))
      (should (ygg-git-compare-async-tests--idle-p)))))

(ert-deftest ygg-git-compare-async-a-head-that-could-not-be-refreshed-says-so ()
  (ygg-git-compare-async-tests--with-world (root _fake base _head)
    (ygg-git-compare-async-tests--git root "remote" "set-url" "origin" "/nonexistent/remote.git")
    (ygg-git-compare-async-tests--seed (ygg-git-compare--head-key "origin" 7) base 100000)
    (let ((buffer (ygg-git-compare-buffer
                   root '(rev . "main")
                   (cons 'pr (list :number 7 :head "feature" :remote "origin")))))
      (ygg-git-compare-async-tests--wait
       (lambda () (string-search "not refreshed" (ygg-git-compare-async-tests--header buffer))))
      (should-not (string-search "refreshing" (ygg-git-compare-async-tests--header buffer))))))

(defconst ygg-git-compare-async-tests--two-prs-gh "#!/bin/sh
case \"$1 $2\" in
  \"pr view\")
    [ -f \"$FAKE_DIR/delay-$3\" ] && sleep \"$(cat \"$FAKE_DIR/delay-$3\")\"
    if [ -f \"$FAKE_DIR/pr-view-$3.json\" ]; then cat \"$FAKE_DIR/pr-view-$3.json\"
    else echo \"GraphQL: Could not resolve to a PullRequest with the number of $3.\" >&2; exit 1; fi;;
  *) exit 2;;
esac
")

(defun ygg-git-compare-async-tests--two-prs (fake base head-7 head-8)
  (let ((gh (expand-file-name "gh" fake)))
    (ygg-git-compare-async-tests--put gh ygg-git-compare-async-tests--two-prs-gh)
    (set-file-modes gh #o755))
  (dolist (pair (list (cons 7 head-7) (cons 8 head-8)))
    (ygg-git-compare-async-tests--put
     (expand-file-name (format "pr-view-%d.json" (car pair)) fake)
     (format "{\"number\":%d,\"state\":\"OPEN\",\"baseRefOid\":\"%s\",\"headRefOid\":\"%s\",\"baseRefName\":\"main\",\"url\":\"https://github.com/o/r/pull/%d\"}"
             (car pair) base (cdr pair) (car pair)))))

(ert-deftest ygg-git-compare-async-the-last-review-request-wins ()
  (ygg-git-compare-async-tests--with-world (root fake base head-7)
    (ygg-git-compare-async-tests--put (expand-file-name "ignored" fake) "")
    (ygg-git-compare-async-tests--put (expand-file-name "a.txt" clone) "1\n2\n3\n")
    (ygg-git-compare-async-tests--git clone "commit" "-q" "-am" "more")
    (let ((head-8 (ygg-git-compare-async-tests--git clone "rev-parse" "HEAD")))
      (ygg-git-compare-async-tests--git clone "push" "-q" remote "HEAD:refs/pull/8/head")
      (ygg-git-compare-async-tests--two-prs fake base head-7 head-8)
      (ygg-git-compare-async-tests--put (expand-file-name "delay-8" fake) "1")
      (let (buffer)
        (setq buffer (ygg-git-compare-review-branch (cons 'pr (list :number 7 :head "a"))))
        (should (eq buffer (ygg-git-compare-review-branch
                            (cons 'pr (list :number 8 :head "b")))))
        (ygg-git-compare-async-tests--wait
         (lambda () (equal (plist-get (buffer-local-value 'ygg-git-compare--b buffer) :diff)
                           head-8)))
        (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p)
        (should (equal (plist-get (cdr (buffer-local-value 'ygg-git-compare--b-spec buffer))
                                  :number)
                       8))))))

(ert-deftest ygg-git-compare-async-a-stale-review-answer-is-dropped ()
  (ygg-git-compare-async-tests--with-world (root _fake base head)
    (let ((buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "main")))
          (current (list nil)) (old (list nil)))
      (with-current-buffer buffer
        (setq ygg-git-compare--review-token current
              ygg-git-compare--note "looking up the pull request…"))
      (let ((pr (list :number 7 :head head :start base :base-ref "main")))
        (ygg-git-compare--review-settle buffer old root "feature" "origin" 7 pr nil)
        (should-not (eq (car-safe (buffer-local-value 'ygg-git-compare--b-spec buffer)) 'pr))
        (ygg-git-compare--review-settle buffer old root "feature" "origin" 7 nil "gh failed")
        (should (equal (buffer-local-value 'ygg-git-compare--note buffer)
                       "looking up the pull request…"))))))

(ert-deftest ygg-git-compare-async-a-pull-request-that-does-not-exist-is-said-in-the-header ()
  (ygg-git-compare-async-tests--with-world (root fake base head)
    (ygg-git-compare-async-tests--two-prs fake base head head)
    (let ((buffer (ygg-git-compare-review-branch (cons 'pr (list :number 99 :head "gone")))))
      (ygg-git-compare-async-tests--wait
       (lambda () (string-search "PR #99 not found"
                                 (ygg-git-compare-async-tests--header buffer))))
      (should-not (string-search "looking up" (ygg-git-compare-async-tests--header buffer)))
      (should (file-directory-p root)))))

(ert-deftest ygg-git-compare-async-a-malformed-cache-file-is-ignored ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (let ((now (float-time)))
      (dolist (data (list "(((" "42" "\"text\"" "nil" "(1 2 3)" "((k))" "(((k) :time))"
                          (list (list '(k) :value "v" :time "yesterday"))
                          (list (list '(k) :value "v"))
                          (list (list '(k) :value "v" :time 0.0e+NaN))
                          (list (list "k" :value "v" :time now))
                          (list (list '(pulls r) :value "oops" :time now))
                          (list (list '(pulls r) :value '(1 2) :time now))
                          (list (list '(head "origin" 7) :value 42 :time now))
                          (list (list '(pr r 7) :value "text" :time now))
                          (list (list '(gh-url) :value '(a) :time now))
                          (list '(k :value "v" :time . 5))
                          (list 'atom)))
        (let ((table (ygg-git-compare-async-tests--reload data)))
          (should (zerop (hash-table-count table))))
        (should-not (ygg-git-compare--cache-value '(k)))
        (should (ygg-git-compare--cache-stale-p (ygg-git-compare--cache-entry '(k)) 60))))))

(ert-deftest ygg-git-compare-async-a-sound-cache-row-is-kept-among-malformed-ones ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (let* ((now (float-time))
           (table (ygg-git-compare-async-tests--reload
                   (list 'junk
                         (list '(head "origin" 7) :value "abc" :time (- now 10))
                         (list '(pulls r) :value '((:number 7 :title "t")) :time (- now 10))
                         (list '(pulls q) :value "oops" :time now)
                         (list '(gh-url) :value "https://x" :time 9e12)
                         (list '(mrs r) :value nil :time now)))))
      (should (= (hash-table-count table) 4))
      (should (equal (ygg-git-compare--cache-value '(head "origin" 7)) "abc"))
      (should (<= (plist-get (ygg-git-compare--cache-entry '(gh-url)) :time) (float-time)))
      (should-not (ygg-git-compare--cache-stale-p (ygg-git-compare--cache-entry '(gh-url)) 60)))))

(ert-deftest ygg-git-compare-async-ssh-keeps-the-users-own-choice-of-ssh ()
  (ygg-git-compare-async-tests--with-world (root _fake _base _head)
    (cl-flet ((env (&rest vars)
                (clrhash ygg-git-compare--ssh-commands)
                (let ((process-environment (append vars process-environment)))
                  (ygg-git-compare--ssh-environment))))
      (should (equal (env) '("GIT_SSH_COMMAND=ssh -o BatchMode=yes")))
      (should (equal (env "GIT_SSH_COMMAND=ssh -i key")
                     '("GIT_SSH_COMMAND=ssh -i key -o BatchMode=yes")))
      (should (equal (env "GIT_SSH=/usr/local/bin/my-ssh") nil))
      (ygg-git-compare-async-tests--git root "config" "core.sshCommand" "ssh -F conf")
      (should (equal (env) '("GIT_SSH_COMMAND=ssh -F conf -o BatchMode=yes")))
      (should (equal (env "GIT_SSH_COMMAND=ssh -i key")
                     '("GIT_SSH_COMMAND=ssh -i key -o BatchMode=yes")))
      (should (equal (env "GIT_SSH=/usr/local/bin/my-ssh")
                     '("GIT_SSH_COMMAND=ssh -F conf -o BatchMode=yes")))
      (ygg-git-compare-async-tests--git root "config" "core.sshCommand" "ssh -F other")
      (let ((process-environment process-environment))
        (should (equal (ygg-git-compare--ssh-environment)
                       '("GIT_SSH_COMMAND=ssh -F conf -o BatchMode=yes"))))
      (let ((default-directory "/ssh:nowhere:/tmp/"))
        (should (equal (env) nil))))))

(ert-deftest ygg-git-compare-async-a-fetch-has-its-own-timeout ()
  (ygg-git-compare-async-tests--with-world (root fake _base _head)
    (should (= ygg-git-compare-fetch-timeout 300))
    (let ((ygg-git-compare-fetch-timeout 0.3)
          (started (float-time))
          (landed nil))
      (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "5")
      (ygg-git-compare--fetch-ref "origin" "pull/7/head"
                                  (lambda (&rest answer) (setq landed (or answer '(none)))))
      (ygg-git-compare-async-tests--wait (lambda () landed))
      (should (equal landed '(nil "git timed out")))
      (should (< (- (float-time) started) 3)))
    (let ((ygg-git-compare-forge-timeout 0.3)
          (landed nil))
      (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "1")
      (ygg-git-compare--fetch-ref "origin" "pull/7/head"
                                  (lambda (&rest answer) (setq landed (or answer '(none)))))
      (ygg-git-compare-async-tests--wait (lambda () landed))
      (should (stringp (car landed))))))

(ert-deftest ygg-git-compare-async-the-refreshing-words-leave-the-prompt-once-pull-requests-land ()
  (let ((table (lambda (&rest _) nil))
        (other (lambda (&rest _) nil)))
    (with-temp-buffer
      (insert "Compare (B)" ygg-git-compare--refreshing-suffix " (default x): " "input")
      (put-text-property 1 (- (point-max) 5) 'read-only t)
      (setq-local minibuffer-completion-table table)
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (cl-letf (((symbol-function 'active-minibuffer-window) #'selected-window))
          (ygg-git-compare--clear-refreshing other)
          (should (string-search "refreshing" (buffer-string)))
          (ygg-git-compare--clear-refreshing table)
          (should (equal (buffer-string) "Compare (B) (default x): input")))))))

(ert-deftest ygg-git-compare-async-the-prompt-is-cleared-when-the-refresh-lands ()
  (ygg-git-compare-async-tests--with-world (_root fake base head)
    (ygg-git-compare-async-tests--seed-all base head)
    (puthash (list 'pulls '(github "github.com" "o/r")) (list :value nil :time 0)
             (ygg-git-compare--cache-table (magit-gitdir)))
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0.3")
    (let (table cleared)
      (cl-letf (((symbol-function 'ygg-git-compare--nudge) #'ignore)
                ((symbol-function 'ygg-git-compare--clear-refreshing)
                 (lambda (tbl) (setq cleared (eq tbl table))))
                ((symbol-function 'completing-read)
                 (lambda (prompt tbl &rest _)
                   (setq table tbl)
                   (should (string-search "(refreshing PRs…)" prompt))
                   (ygg-git-compare-async-tests--wait (lambda () cleared))
                   "main")))
        (ygg-git-compare--read "Compare" (apply-partially #'ygg-git-compare-candidates t) nil)))))

(ert-deftest ygg-git-compare-async-a-cache-save-keeps-what-another-session-wrote ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (ygg-git-compare--cached '(head "o" 1) 60 (lambda (done) (funcall done "aaa")) nil)
    (let ((rows (ygg-git-compare-async-tests--cache-rows)))
      (with-temp-file (ygg-git-compare-async-tests--cache-file)
        (prin1 (cons (list '(head "o" 2) :value "bbb" :time (float-time)) rows)
               (current-buffer))))
    (ygg-git-compare--cached '(head "o" 3) 60 (lambda (done) (funcall done "ccc")) nil)
    (let ((rows (ygg-git-compare-async-tests--cache-rows)))
      (should (equal (sort (mapcar (lambda (row) (plist-get (cdr row) :value)) rows) #'string<)
                     '("aaa" "bbb" "ccc"))))
    (should (equal (ygg-git-compare--cache-value '(head "o" 2)) "bbb"))
    (should-not (file-exists-p (concat (ygg-git-compare-async-tests--cache-file) ".lock")))))

(ert-deftest ygg-git-compare-async-a-cache-file-is-kept-small ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (let ((table (ygg-git-compare--cache-table (magit-gitdir)))
          (now (float-time)))
      (dotimes (i 600)
        (puthash (list 'head "o" i) (list :value (format "%d" i) :time (- now i)) table))
      (puthash '(head "o" "old") (list :value "old" :time (- now (* 8 86400))) table)
      (ygg-git-compare--cache-save (magit-gitdir) table)
      (let ((rows (ygg-git-compare-async-tests--cache-rows)))
        (should (= (length rows) 500))
        (should-not (assoc '(head "o" "old") rows))
        (should (assoc '(head "o" 0) rows))
        (should-not (assoc '(head "o" 599) rows))))))

(ert-deftest ygg-git-compare-async-gh-is-not-asked-about-a-gitlab-repository ()
  (ygg-git-compare-async-tests--with-world (root fake _base _head)
    (ygg-git-compare-async-tests--git root "remote" "set-url" "origin" "git@gitlab.com:o/r.git")
    (puthash "gitlab.com" "gitlab.com" ygg-git-compare--ssh-hostnames)
    (ygg-git-compare--refine-pr-remote)
    (should (ygg-git-compare-async-tests--idle-p))
    (should-not (file-exists-p (expand-file-name "gh.log" fake)))))

(ert-deftest ygg-git-compare-async-a-gh-failure-on-a-remote-directory-is-kept-for-the-ttl ()
  (let (retry)
    (cl-letf (((symbol-function 'ygg-git-compare--forge-repo)
               (lambda (&rest _) '(github "github.com" "o/r")))
              ((symbol-function 'ygg-git-compare--remote) (lambda () "origin"))
              ((symbol-function 'executable-find) (lambda (&rest _) "/bin/gh"))
              ((symbol-function 'ygg-git-compare--cached)
               (lambda (_key _ttl _fetch _fresh &optional r) (setq retry (or r 'default)))))
      (ygg-git-compare--refine-pr-remote)
      (should (eq retry 'default))
      (let ((default-directory "/ssh:nowhere:/tmp/"))
        (ygg-git-compare--refine-pr-remote))
      (should (= retry 86400))))
  (should-not (ygg-git-compare--cache-stale-p (list :value "u" :time 0 :failed (- (float-time) 100))
                                              86400 86400))
  (should (ygg-git-compare--cache-stale-p (list :value "u" :time 0 :failed (- (float-time) 100))
                                          86400)))


;;; ygg-git-compare-async-tests.el ends here
