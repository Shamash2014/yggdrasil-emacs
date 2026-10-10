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

(declare-function ygg-git-worktree-run "ygg-git-worktree")
(declare-function ygg-git-worktree--spec-at-point "ygg-git-worktree")

(defconst ygg-git-compare-async-tests--budget 0.1
  "Seconds a call has to return in, besides the time its local git runs take.")

(defconst ygg-git-compare-async-tests--real-git (executable-find "git"))

(defconst ygg-git-compare-async-tests--scripts
  '(("gh" . "#!/bin/sh
echo \"$*\" >> \"$FAKE_DIR/gh.log\"
env | grep -E '^(GH_PROMPT_DISABLED|GIT_TERMINAL_PROMPT)=' >> \"$FAKE_DIR/gh.env\"
[ -f \"$FAKE_DIR/gh.delay\" ] && sleep \"$(cat \"$FAKE_DIR/gh.delay\")\"
if [ \"$1\" = api ]; then
  for a in \"$@\"; do [ \"$prev\" = --input ] && body=$(cat \"$a\"); prev=$a; done
  echo \"$body\" >> \"$FAKE_DIR/api.log\"
  case \"$body\" in *FAILME*) echo \"boom: refused\" >&2; exit 1;; esac
  echo '{\"id\":1}'; exit 0
fi
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
for a in \"$@\"; do
  [ \"$prev\" = --input ] && body=$(cat \"$a\")
  [ \"$a\" = --method ] && post=1
  prev=$a
done
if [ -n \"$post\" ]; then
  echo \"$body\" >> \"$FAKE_DIR/api.log\"
  case \"$body\" in *FAILME*) echo \"boom: refused\" >&2; exit 1;; esac
  echo '{\"id\":1}'; exit 0
fi
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
  (let ((root (make-symbol "root")) (fake (make-symbol "fake"))
        (base (make-symbol "base")) (head (make-symbol "head")))
  `(let* ((outer (file-name-as-directory
                  (file-truename (make-temp-file "ygg-git-compare-async-" t))))
          (,root (file-name-as-directory (expand-file-name "work" outer)))
          (,fake (file-name-as-directory (expand-file-name "fake" outer)))
          (remote (expand-file-name "remote.git" outer))
          (clone (expand-file-name "clone" outer))
          (path (concat ,fake ":" (getenv "PATH")))
          (process-environment
           (append (list "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                         (concat "PATH=" path) (concat "FAKE_DIR=" ,fake)
                         (concat "REAL_GIT=" ygg-git-compare-async-tests--real-git)
                         "GIT_SSH_COMMAND" "GIT_SSH" "LAB_HOST")
                   process-environment))
          (exec-path (cons (directory-file-name ,fake) exec-path))
          (magit-git-executable ygg-git-compare-async-tests--real-git)
          (ygg-git-compare--ssh-hostnames
           (let ((hostnames (make-hash-table :test #'equal)))
             (puthash "github.com" "github.com" hostnames)
             hostnames))
          (magit-refresh-verbose nil)
          (default-directory (progn (make-directory ,root t)
                                    (make-directory ,fake t)
                                    ,root))
          ,base ,head)
     (unwind-protect
         (cl-flet ((git (dir &rest args)
                     (apply #'ygg-git-compare-async-tests--git dir args)))
           (pcase-dolist (`(,name . ,text) ygg-git-compare-async-tests--scripts)
             (let ((file (expand-file-name name ,fake)))
               (ygg-git-compare-async-tests--put file text)
               (set-file-modes file #o755)))
           (git ,root "init" "-q" "-b" "main")
           (git ,root "config" "user.name" "Async Test")
           (git ,root "config" "user.email" "async@example.invalid")
           (git ,root "config" "commit.gpgsign" "false")
           (ygg-git-compare-async-tests--put (expand-file-name "a.txt" ,root) "1\n")
           (git ,root "add" ".")
           (git ,root "commit" "-q" "-m" "base")
           (setq ,base (git ,root "rev-parse" "HEAD"))
           (git outer "init" "-q" "--bare" remote)
           (git outer "clone" "-q" ,root clone)
           (git clone "config" "user.name" "Async Test")
           (git clone "config" "user.email" "async@example.invalid")
           (git clone "config" "commit.gpgsign" "false")
           (ygg-git-compare-async-tests--put (expand-file-name "a.txt" clone) "1\n2\n")
           (git clone "commit" "-q" "-am" "work")
           (setq ,head (git clone "rev-parse" "HEAD"))
           (git clone "push" "-q" remote "HEAD:refs/pull/7/head"
                (concat ,base ":refs/heads/main"))
           (git ,root "remote" "add" "origin" "git@github.com:o/r.git")
           (git ,root "config" (concat "url." remote ".insteadOf") "git@github.com:o/r.git")
           (ygg-git-compare-async-tests--put
            (expand-file-name "pulls.json" ,fake)
            "[{\"number\":7,\"title\":\"Add two\",\"headRefName\":\"feature\",\"baseRefName\":\"main\"}]")
           (ygg-git-compare-async-tests--put
            (expand-file-name "pr-view.json" ,fake)
            (format "{\"number\":7,\"state\":\"OPEN\",\"baseRefOid\":\"%s\",\"headRefOid\":\"%s\",\"baseRefName\":\"main\",\"url\":\"https://github.com/o/r/pull/7\"}"
                    ,base ,head))
           (ygg-git-compare-async-tests--put
            (expand-file-name "repo-url.txt" ,fake) "https://github.com/o/r\n")
           (ygg-git-compare-async-tests--put
            (expand-file-name "mrs.json" ,fake) "[]")
           (let ((,(nth 0 vars) ,root) (,(nth 1 vars) ,fake)
                 (,(nth 2 vars) ,base) (,(nth 3 vars) ,head))
             ,@body))
       (clrhash ygg-git-compare--inflight)
       (clrhash ygg-git-compare--fetch-lanes)
       (dolist (buffer (buffer-list))
         (when (and (buffer-live-p buffer)
                    (buffer-local-value 'ygg-git-compare--a-spec buffer))
           (ygg-git-compare--close buffer)))
       (delete-directory outer t)))))

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

(defvar ygg-git-compare-async-tests--said nil)

(defmacro ygg-git-compare-async-tests--saying (&rest body)
  "BODY with every message collected, newest first, in `--said'."
  `(let ((ygg-git-compare-async-tests--said nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args)
                  (when format
                    (push (apply #'format-message format args)
                          ygg-git-compare-async-tests--said)))))
       ,@body)))

(ert-deftest ygg-git-compare-async-a-worktree-for-an-unfetched-pull-request-continues-when-it-lands ()
  (skip-unless (require 'ygg-git-worktree nil t))
  (ygg-git-compare-async-tests--with-world (_root fake _base head)
    (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "0.3")
    (let ((spec (cons 'pr (list :number 7 :head "feature" :remote "origin")))
          (added nil))
      (ygg-git-compare-async-tests--saying
        (cl-letf (((symbol-function 'ygg-git-worktree--git)
                   (lambda (args _on-success) (setq added args))))
          (ygg-git-compare-async-tests--unblocked (ygg-git-worktree-run spec))
          (should-not added)
          (should (member "Fetching PR #7…" ygg-git-compare-async-tests--said))
          (ygg-git-compare-async-tests--wait (lambda () added))))
      (should (equal (list (nth 0 added) (nth 1 added) (nth 2 added) (nth 4 added))
                     (list "worktree" "add" "--detach" head)))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p))))

(ert-deftest ygg-git-compare-async-a-worktree-for-a-pull-request-that-will-not-fetch-says-why ()
  (skip-unless (require 'ygg-git-worktree nil t))
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (let ((spec (cons 'pr (list :number 99 :head "feature" :remote "origin")))
          (added nil))
      (ygg-git-compare-async-tests--saying
        (cl-letf (((symbol-function 'ygg-git-worktree--git)
                   (lambda (args _on-success) (setq added args))))
          (ygg-git-compare-async-tests--unblocked (ygg-git-worktree-run spec))
          (ygg-git-compare-async-tests--wait
           (lambda () (seq-some (lambda (m) (string-search "PR #99 not fetched" m))
                                ygg-git-compare-async-tests--said)))
          (should-not added)
          (should-not (seq-some (lambda (m) (string-search "try again" m))
                                ygg-git-compare-async-tests--said))))
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
        (should (string-search "Looking up PR #7" error)))
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

(defun ygg-git-compare-async-tests--add-worktrees (root count)
  "Give ROOT's repository COUNT linked worktrees, written as git keeps them."
  (let ((head (ygg-git-compare-async-tests--git root "rev-parse" "HEAD"))
        (outer (file-name-directory (directory-file-name root))))
    (dotimes (i count)
      (let* ((name (format "wt%d" (1+ i)))
             (dir (expand-file-name (concat "linked-" name) outer))
             (meta (expand-file-name (concat ".git/worktrees/" name) root)))
        (make-directory dir t)
        (make-directory meta t)
        (ygg-git-compare-async-tests--put (expand-file-name ".git" dir)
                                          (format "gitdir: %s\n" meta))
        (ygg-git-compare-async-tests--put (expand-file-name "HEAD" meta) (concat head "\n"))
        (ygg-git-compare-async-tests--put (expand-file-name "gitdir" meta)
                                          (concat dir "/.git\n"))
        (ygg-git-compare-async-tests--put (expand-file-name "commondir" meta) "../..\n")))))

(defun ygg-git-compare-async-tests--picker-calls ()
  (let ((calls 0))
    (advice-add 'process-file :before (lambda (&rest _) (cl-incf calls)) '((name . count-git)))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "main")))
          (ygg-git-compare--read-sides)
          calls)
      (advice-remove 'process-file 'count-git))))

(ert-deftest ygg-git-compare-async-a-picker-open-costs-no-more-git-calls-per-worktree ()
  (ygg-git-compare-async-tests--with-world (root _fake base head)
    (ygg-git-compare-async-tests--seed-all base head)
    (ygg-git-compare-async-tests--add-worktrees root 15)
    (let ((labels (mapcar #'car (ygg-git-compare-candidates))))
      (should (= (seq-count (lambda (label)
                              (equal (get-text-property 0 'ygg-git-compare-group label)
                                     "Worktrees"))
                            labels)
                 16)))
    (should (<= (ygg-git-compare-async-tests--picker-calls) 25))
    (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p)))

(ert-deftest ygg-git-compare-async-a-failing-fetch-start-frees-the-queue ()
  (dolist (signal-as '(wrong-type-argument quit))
    (ygg-git-compare-async-tests--with-world (root fake _base head)
      (let ((boom t) (landed nil))
        (advice-add 'make-process :around
                    (lambda (original &rest args)
                      (if (and boom (member "fetch" (plist-get args :command)))
                          (progn (setq boom nil) (signal signal-as '(boom)))
                        (apply original args)))
                    '((name . boom)))
        (unwind-protect
            (progn
              (condition-case nil
                  (ygg-git-compare-fetch-pr 7 "origin")
                (quit nil))
              (should-not (ygg-git-compare--refreshing-p 'head))
              (ygg-git-compare--fetch-ref "origin" "pull/7/head"
                                          (lambda (&rest answer) (setq landed (or answer '(none)))))
              (ygg-git-compare-async-tests--wait (lambda () landed))
              (should (equal (car landed) head))
              (when (eq signal-as 'wrong-type-argument)
                (ygg-git-compare--cache-drop (ygg-git-compare--head-key "origin" 7)))
              (ygg-git-compare-fetch-pr 7 "origin")
              (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p)
              (should (equal (ygg-git-compare--cache-value (ygg-git-compare--head-key "origin" 7))
                             head)))
          (advice-remove 'make-process 'boom))
        (ignore root fake)))))

(ert-deftest ygg-git-compare-async-one-repository-does-not-queue-behind-another ()
  (ygg-git-compare-async-tests--with-world (root fake _base head)
    (let ((other (file-name-as-directory (expand-file-name "other" (file-name-directory
                                                                       (directory-file-name root)))))
          (landed nil))
      (ygg-git-compare-async-tests--git root "clone" "-q" (directory-file-name root) (directory-file-name other))
      (ygg-git-compare-async-tests--git other "remote" "set-url" "origin"
                                        (ygg-git-compare-async-tests--git
                                         root "remote" "get-url" "origin"))
      (ygg-git-compare-async-tests--git other "config" (concat "url." (expand-file-name "../remote.git" root)
                                                               ".insteadOf")
                                        "git@github.com:o/r.git")
      (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "2")
      (ygg-git-compare--fetch-ref "origin" "pull/7/head" #'ignore)
      (let ((default-directory other)
            (started (float-time)))
        (ygg-git-compare--fetch-ref "origin" "pull/7/head"
                                    (lambda (&rest answer) (setq landed (or answer '(none)))))
        (ygg-git-compare-async-tests--wait (lambda () landed) 10)
        (should (< (- (float-time) started) 3.5))
        (should (equal (car landed) head))))
    (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p)))

(ert-deftest ygg-git-compare-async-a-pull-request-row-of-the-wrong-types-is-dropped ()
  (dolist (value '((:head 5) (:start (1 2)) (:number "x") (:base 'a) (:url 3) (:base-ref (a))))
    (should-not (ygg-git-compare--value-valid-p '(pr (github "h" "o/r") 7) value)))
  (should (ygg-git-compare--value-valid-p
           '(pr (github "h" "o/r") 7)
           '(:forge github :number 7 :head "a" :start "b" :base-ref "main" :url "u")))
  (should (equal (ygg-git-compare--with-base '(:head 5 :start (1 2))) '(:head 5 :start (1 2))))
  (should (equal (ygg-git-compare--with-base '(:head "a")) '(:head "a"))))

(ert-deftest ygg-git-compare-async-a-pull-request-that-closed-is-said-in-an-open-compare ()
  (ygg-git-compare-async-tests--with-world (root _fake _base head)
    (ygg-git-compare-async-tests--git root "fetch" "-q" "origin" "refs/pull/7/head")
    (let* ((token (list nil))
           (buffer (ygg-git-compare-buffer
                    root '(rev . "main")
                    (cons 'pr (list :number 7 :head "feature" :sha head :remote "origin")))))
      (with-current-buffer buffer
        (setq ygg-git-compare--review-token token ygg-git-compare--note nil))
      (ygg-git-compare--review-settle buffer token root "feature" "origin" 7 nil nil)
      (should (string-search "PR #7 is closed or gone"
                             (ygg-git-compare-async-tests--header buffer))))))

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
  (ygg-git-compare-async-tests--with-world (_root fake base head-7)
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
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
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

(ert-deftest ygg-git-compare-async-one-repository-by-two-paths-is-one-lane ()
  (ygg-git-compare-async-tests--with-world (root fake base head-7)
    (ygg-git-compare-async-tests--put (expand-file-name "a.txt" clone) "1\n2\n3\n")
    (ygg-git-compare-async-tests--git clone "commit" "-q" "-am" "more")
    (let* ((head-8 (ygg-git-compare-async-tests--git clone "rev-parse" "HEAD"))
           (link (expand-file-name "link" (file-name-directory (directory-file-name root))))
           (key-7 (ygg-git-compare--head-key "origin" 7))
           (key-8 (ygg-git-compare--head-key "origin" 8)))
      (ignore base)
      (ygg-git-compare-async-tests--git clone "push" "-q" remote "HEAD:refs/pull/8/head")
      (make-symbolic-link (directory-file-name root) link)
      (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "0.3")
      (let ((default-directory root))
        (ygg-git-compare-fetch-pr 7 "origin"))
      (let ((default-directory (file-name-as-directory link)))
        (ygg-git-compare-fetch-pr 8 "origin"))
      (should (= (hash-table-count ygg-git-compare--fetch-lanes) 1))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p)
      (should (equal (ygg-git-compare--cache-value key-7) head-7))
      (should (equal (ygg-git-compare--cache-value key-8) head-8))
      (should (= (hash-table-count ygg-git-compare--fetch-lanes) 0)))))

(ert-deftest ygg-git-compare-async-a-queued-job-that-fails-to-start-lands-and-the-lane-goes-on ()
  (ygg-git-compare-async-tests--with-world (_root _fake _base _head)
    (let (release-1 (lane "lane"))
      (ygg-git-compare--cached
       '(head "o" 1) 60
       (lambda (_land)
         (ygg-git-compare--serially lane (lambda (release) (setq release-1 release))))
       nil)
      (ygg-git-compare--cached
       '(head "o" 2) 60
       (lambda (_land)
         (ygg-git-compare--serially
          lane (lambda (_release) (signal 'wrong-type-argument '(boom)))))
       nil)
      (ygg-git-compare--cached
       '(head "o" 3) 60
       (lambda (land)
         (ygg-git-compare--serially
          lane (lambda (release) (funcall land "third") (funcall release))))
       nil)
      (funcall release-1)
      (should (equal (ygg-git-compare--cache-value '(head "o" 3)) "third"))
      (should (plist-get (ygg-git-compare--cache-entry '(head "o" 2)) :error))
      (should (= (hash-table-count ygg-git-compare--inflight) 1))
      (should (= (hash-table-count ygg-git-compare--fetch-lanes) 0)))))

(ert-deftest ygg-git-compare-async-idle-lanes-are-dropped ()
  (let ((ygg-git-compare--fetch-lanes (make-hash-table :test #'equal))
        releases)
    (dotimes (i 100)
      (ygg-git-compare--serially (format "repo-%d" i) (lambda (release) (funcall release))))
    (should (= (hash-table-count ygg-git-compare--fetch-lanes) 0))
    (dotimes (i 100)
      (ygg-git-compare--serially (format "repo-%d" i) (lambda (release) (push release releases)))
      (ygg-git-compare--serially (format "repo-%d" i) #'funcall))
    (should (= (hash-table-count ygg-git-compare--fetch-lanes) 100))
    (mapc #'funcall releases)
    (should (= (hash-table-count ygg-git-compare--fetch-lanes) 0))))

(ert-deftest ygg-git-compare-async-a-worktree-path-with-a-newline-is-one-path ()
  (ygg-git-compare-async-tests--with-world (root _fake _base _head)
    (let* ((outer (file-name-directory (directory-file-name root)))
           (dir (expand-file-name "linked\nnl" outer))
           (meta (expand-file-name ".git/worktrees/wtnl" root)))
      (make-directory dir t)
      (make-directory meta t)
      (ygg-git-compare-async-tests--put (expand-file-name ".git" dir) (format "gitdir: %s\n" meta))
      (ygg-git-compare-async-tests--put (expand-file-name "HEAD" meta)
                                        (concat (ygg-git-compare-async-tests--git root "rev-parse" "HEAD") "\n"))
      (ygg-git-compare-async-tests--put (expand-file-name "gitdir" meta) (concat dir "/.git\n"))
      (ygg-git-compare-async-tests--put (expand-file-name "commondir" meta) "../..\n")
      (should (member dir (mapcar (lambda (row) (directory-file-name (car row)))
                                  (ygg-git-compare--worktrees)))))))

(ert-deftest ygg-git-compare-async-a-worktree-note-ends-in-a-slash ()
  (ygg-git-compare-async-tests--with-world (root _fake _base _head)
    (let ((note (get-text-property 0 'ygg-git-compare-note
                                   (car (seq-find (lambda (cand)
                                                    (equal (get-text-property 0 'ygg-git-compare-group (car cand))
                                                           "Worktrees"))
                                                  (ygg-git-compare-candidates))))))
      (should (string-suffix-p "/  (here)" note))
      (ignore root))))

(ert-deftest ygg-git-compare-async-a-pull-request-gone-twice-keeps-its-note-quietly ()
  (ygg-git-compare-async-tests--with-world (root _fake _base head)
    (ygg-git-compare-async-tests--git root "fetch" "-q" "origin" "refs/pull/7/head")
    (let* ((token (list nil))
           (messages nil)
           (buffer (ygg-git-compare-buffer
                    root '(rev . "main")
                    (cons 'pr (list :number 7 :head "feature" :sha head :remote "origin")))))
      (with-current-buffer buffer
        (setq ygg-git-compare--review-token token ygg-git-compare--note nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args) (push args messages))))
        (ygg-git-compare--review-settle buffer token root "feature" "origin" 7 nil nil)
        (ygg-git-compare--review-settle buffer token root "feature" "origin" 7 nil nil))
      (should (string-search "PR #7 is closed or gone" (ygg-git-compare-async-tests--header buffer)))
      (should-not (string-search "not found" (ygg-git-compare-async-tests--header buffer)))
      (should-not messages))))

(ert-deftest ygg-git-compare-async-this-pr-continues-when-a-cold-lookup-lands ()
  (ygg-git-compare-async-tests--with-world (root fake _base head)
    (ygg-git-compare-async-tests--git root "fetch" "-q" "origin" "refs/pull/7/head")
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0.3")
    (with-current-buffer (ygg-git-compare-buffer
                          root (cons 'rev _base)
                          (cons 'pr (list :number 7 :sha head :head "feature" :base "main"
                                          :remote "origin")))
      (let (got)
        (ygg-git-compare-async-tests--saying
          (ygg-git-compare-async-tests--unblocked
            (ygg-git-compare--this-pr (lambda (pr) (setq got pr))))
          (should-not got)
          (should (member "Looking up PR #7…" ygg-git-compare-async-tests--said))
          (ygg-git-compare-async-tests--wait (lambda () got)))
        (should (equal (plist-get got :number) 7))
        (should (equal (plist-get got :head) head)))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p))))

(ert-deftest ygg-git-compare-async-this-pr-says-why-a-lookup-failed-without-asking-to-retry ()
  (ygg-git-compare-async-tests--with-world (root fake base head)
    (ygg-git-compare-async-tests--git root "fetch" "-q" "origin" "refs/pull/7/head")
    (delete-file (expand-file-name "pr-view.json" fake))
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0.2")
    (with-current-buffer (ygg-git-compare-buffer
                          root (cons 'rev base)
                          (cons 'pr (list :number 7 :sha head :head "feature" :base "main"
                                          :remote "origin")))
      (let (got)
        (ygg-git-compare-async-tests--saying
          (ygg-git-compare--this-pr (lambda (pr) (setq got pr)))
          (ygg-git-compare-async-tests--wait
           (lambda () (seq-some (lambda (m) (string-search "Could not read PR #7" m))
                                ygg-git-compare-async-tests--said)))
          (should-not got)
          (should-not (seq-some (lambda (m) (string-search "try again" m))
                                ygg-git-compare-async-tests--said))))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p))))

(ert-deftest ygg-git-compare-async-a-queued-fetch-whose-start-fails-lands-its-error ()
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
    (ygg-git-compare-async-tests--put (expand-file-name "git.delay" fake) "0.3")
    (let ((landed nil))
      (ygg-git-compare--fetch-ref "origin" "pull/7/head" #'ignore)
      (advice-add 'make-process :around
                  (lambda (original &rest args)
                    (if (member "fetch" (plist-get args :command))
                        (signal 'wrong-type-argument '(boom))
                      (apply original args)))
                  '((name . boom)))
      (unwind-protect
          (progn
            (ygg-git-compare--fetch-ref "origin" "pull/7/head"
                                        (lambda (&rest answer) (setq landed (or answer '(none)))))
            (should-not landed)
            (ygg-git-compare-async-tests--wait (lambda () landed))
            (should-not (car landed))
            (should (string-search "boom" (cadr landed))))
        (advice-remove 'make-process 'boom))
      (ygg-git-compare-async-tests--wait
       (lambda () (zerop (hash-table-count ygg-git-compare--fetch-lanes)))))))

(ert-deftest ygg-git-compare-async-a-remote-git-dir-is-not-resolved-on-the-way ()
  (let ((resolved nil))
    (cl-letf (((symbol-function 'magit-gitdir) (lambda (&rest _) "/ssh:host:/repo/.git"))
              ((symbol-function 'file-truename)
               (lambda (file &rest _) (setq resolved t) file)))
      (should (equal (ygg-git-compare--gitdir) "/ssh:host:/repo/.git"))
      (should-not resolved))))

(ert-deftest ygg-git-compare-async-a-running-job-lands-on-its-own-key ()
  (clrhash ygg-git-compare--fetch-lanes)
  (let ((release-first nil)
        (own (lambda (&rest _)))
        (other (lambda (&rest _)))
        (seen 'unset))
    (let ((ygg-git-compare--landing nil))
      (ygg-git-compare--serially "lane" (lambda (release) (setq release-first release))))
    (let ((ygg-git-compare--landing own))
      (ygg-git-compare--serially "lane" (lambda (_release) (setq seen ygg-git-compare--landing))))
    (let ((ygg-git-compare--landing other))
      (funcall release-first))
    (should (eq seen own))
    (clrhash ygg-git-compare--fetch-lanes)))

(defvar ygg-git-compare-async-tests--held nil)
(defvar ygg-git-compare-async-tests--dropped nil)

(defun ygg-git-compare-async-tests--comment (id &rest props)
  (append props (list :id id :range "R" :text id)))

(defmacro ygg-git-compare-async-tests--submitting (comments pr &rest body)
  "BODY in a compare holding COMMENTS for PR, the forge being the fake gh and
glab, every message collected in `--said'."
  (declare (indent 2))
  `(let ((ygg-git-compare-async-tests--held (copy-tree ,comments))
         (ygg-git-compare-async-tests--dropped nil))
     (ygg-git-compare-async-tests--saying
       (with-temp-buffer
         (cl-letf (((symbol-function 'ygg-git-compare-comments-list)
                    (lambda (&rest _) ygg-git-compare-async-tests--held))
                   ((symbol-function 'ygg-git-compare-comments-drop)
                    (lambda (ids)
                      (setq ygg-git-compare-async-tests--dropped
                            (append ygg-git-compare-async-tests--dropped ids)
                            ygg-git-compare-async-tests--held
                            (seq-remove (lambda (c) (member (plist-get c :id) ids))
                                        ygg-git-compare-async-tests--held))))
                   ((symbol-function 'ygg-git-compare--list) #'current-buffer)
                   ((symbol-function 'ygg-git-compare--range-label) (lambda () "R"))
                   ((symbol-function 'ygg-git-compare--this-pr)
                    (lambda (&optional continue) (funcall continue ,pr)))
                   ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
           ,@body)))))

(defun ygg-git-compare-async-tests--posted-p ()
  (seq-some (lambda (m) (string-prefix-p "Posted" m)) ygg-git-compare-async-tests--said))

(ert-deftest ygg-git-compare-async-submit-to-github-leaves-emacs-free-and-refuses-a-second ()
  (skip-unless (and (require 'ygg-git-compare-submit nil t)
                    (require 'ygg-git-compare-comments nil t)))
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0.4")
    (ygg-git-compare-async-tests--submitting
        (list (ygg-git-compare-async-tests--comment "a" :level 'line :file "a.txt"
                                                    :new-path "a.txt" :side 'new :line 2)
              (ygg-git-compare-async-tests--comment "b" :level 'review))
        '(:forge github :host "github.com" :path "o/r" :number 7 :head "h" :base "b"
          :start "s" :url "https://github.com/o/r/pull/7")
      (ygg-git-compare-async-tests--unblocked (ygg-git-compare-submit-forge 'comment))
      (should (equal (reverse ygg-git-compare-async-tests--said)
                     '("Posting 2 comments to github PR #7…")))
      (should-not ygg-git-compare-async-tests--dropped)
      (should (string-search "already being posted"
                             (cadr (should-error (ygg-git-compare-submit-forge 'comment)
                                                 :type 'user-error))))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--posted-p)
      (should (equal (sort (copy-sequence ygg-git-compare-async-tests--dropped) #'string<)
                     '("a" "b")))
      (should (string-prefix-p "Posted 2 comments to github PR #7"
                               (car ygg-git-compare-async-tests--said)))
      (should (= (length (split-string
                          (ygg-git-compare-async-tests--read (expand-file-name "api.log" fake))
                          "\n" t))
                 1))
      (should (string-search "No comments for the pull request"
                             (cadr (should-error (ygg-git-compare-submit-forge 'comment)
                                                 :type 'user-error)))))))

(ert-deftest ygg-git-compare-async-submit-to-gitlab-reports-progress-and-keeps-what-failed ()
  (skip-unless (and (require 'ygg-git-compare-submit nil t)
                    (require 'ygg-git-compare-comments nil t)))
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
    (ygg-git-compare-async-tests--put (expand-file-name "gh.delay" fake) "0.2")
    (ygg-git-compare-async-tests--submitting
        (cl-loop for id in '("one" "two" "three")
                 collect (ygg-git-compare-async-tests--comment
                          id :level 'line :file "a.txt" :new-path "a.txt" :old-path "a.txt"
                          :side 'old :line 4 :text (if (equal id "two") "FAILME" id)))
        '(:forge gitlab :host "gitlab.com" :path "g/p" :number 7 :head "h" :base "b"
          :start "s" :url "https://gitlab.com/g/p/-/merge_requests/7")
      (ygg-git-compare-async-tests--unblocked (ygg-git-compare-submit-forge 'comment))
      (should (equal (reverse ygg-git-compare-async-tests--said) '("Posting 1/3…")))
      (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--posted-p 15)
      (should (equal (reverse ygg-git-compare-async-tests--said)
                     '("Posting 1/3…" "Posting 2/3…" "Posting 3/3…"
                       "Posted 2 of 3; 1 kept: boom: refused")))
      (should (equal (sort (copy-sequence ygg-git-compare-async-tests--dropped) #'string<)
                     '("one" "three")))
      (should (equal (mapcar (lambda (c) (plist-get c :id)) ygg-git-compare-async-tests--held)
                     '("two"))))))

(defun ygg-git-compare-async-tests--repo ()
  (let ((dir (file-name-as-directory (file-truename (make-temp-file "ygg-gitdir-" t)))))
    (ygg-git-compare-async-tests--git dir "init" "-q" "-b" "main")
    (ygg-git-compare-async-tests--git dir "config" "user.name" "x")
    (ygg-git-compare-async-tests--git dir "config" "user.email" "x@x.i")
    (ygg-git-compare-async-tests--put (expand-file-name "f" dir) "x")
    (ygg-git-compare-async-tests--git dir "add" ".")
    (ygg-git-compare-async-tests--git dir "commit" "-q" "-m" "c")
    dir))

(defun ygg-git-compare-async-tests--gitdir (dir)
  (let ((default-directory dir)) (ygg-git-compare--gitdir)))

(ert-deftest ygg-git-compare-async-gitdir-follows-a-git-init-in-a-subdirectory ()
  (let* ((root (ygg-git-compare-async-tests--repo))
         (sub (file-name-as-directory (expand-file-name "sub" root))))
    (make-directory sub)
    (should (equal (ygg-git-compare-async-tests--gitdir sub)
                   (file-name-as-directory (file-truename (expand-file-name ".git" root)))))
    (ygg-git-compare-async-tests--git sub "init" "-q" "-b" "main")
    (should (equal (ygg-git-compare-async-tests--gitdir sub)
                   (file-name-as-directory (file-truename (expand-file-name ".git" sub)))))))

(ert-deftest ygg-git-compare-async-gitdir-follows-a-removed-nested-repository ()
  (let* ((root (ygg-git-compare-async-tests--repo))
         (sub (file-name-as-directory (expand-file-name "sub" root))))
    (make-directory sub)
    (ygg-git-compare-async-tests--git sub "init" "-q")
    (should (equal (ygg-git-compare-async-tests--gitdir sub)
                   (file-name-as-directory (file-truename (expand-file-name ".git" sub)))))
    (delete-directory (expand-file-name ".git" sub) t)
    (should (equal (ygg-git-compare-async-tests--gitdir sub)
                   (file-name-as-directory (file-truename (expand-file-name ".git" root)))))))

(ert-deftest ygg-git-compare-async-gitdir-follows-a-worktree-path-another-repository-reuses ()
  (let* ((a (ygg-git-compare-async-tests--repo))
         (b (ygg-git-compare-async-tests--repo))
         (wt (concat (file-name-as-directory (make-temp-file "ygg-wt-" t)) "w/")))
    (ygg-git-compare-async-tests--git a "worktree" "add" "-q" "-b" "w" (directory-file-name wt))
    (let ((first (ygg-git-compare-async-tests--gitdir wt)))
      (should (string-prefix-p (file-truename (expand-file-name ".git/worktrees" a)) first))
      (ygg-git-compare-async-tests--git a "worktree" "remove" "--force" (directory-file-name wt))
      (ygg-git-compare-async-tests--git b "worktree" "add" "-q" "-b" "w" (directory-file-name wt))
      (let ((second (ygg-git-compare-async-tests--gitdir wt)))
        (should (string-prefix-p (file-truename (expand-file-name ".git/worktrees" b)) second))
        (should-not (equal first second))))))

(ert-deftest ygg-git-compare-async-a-warm-gitdir-runs-no-process ()
  (let ((root (ygg-git-compare-async-tests--repo)))
    (ygg-git-compare-async-tests--gitdir root)
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (error "process")))
              ((symbol-function 'process-file) (lambda (&rest _) (error "process")))
              ((symbol-function 'make-process) (lambda (&rest _) (error "process"))))
      (should (ygg-git-compare-async-tests--gitdir root)))))

(ert-deftest ygg-git-compare-async-a-submit-makes-the-next-redraw-fetch-the-threads-once ()
  (skip-unless (and (require 'ygg-git-compare-submit nil t)
                    (require 'ygg-git-compare-comments nil t)
                    (require 'ygg-git-compare-threads nil t)))
  (ygg-git-compare-async-tests--with-world (_root fake _base _head)
    (let* ((pr '(:forge github :host "github.com" :path "o/r" :number 7 :head "h" :base "b"
                 :start "s" :url "https://github.com/o/r/pull/7"))
           (key (ygg-git-compare--remote-key pr))
           (fetches (lambda ()
                      (length (seq-filter
                               (lambda (l) (string-search "graphql" l))
                               (split-string
                                (or (ygg-git-compare-async-tests--read (expand-file-name "gh.log" fake)) "")
                                "\n" t))))))
      (ygg-git-compare-async-tests--seed key (list :comments nil) 1)
      (ygg-git-compare-async-tests--submitting
          (list (ygg-git-compare-async-tests--comment "a" :level 'review))
          pr
        (ygg-git-compare--remote-start (current-buffer) pr key)
        (should (= (funcall fetches) 0))
        (ygg-git-compare-submit-forge 'comment)
        (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--posted-p)
        (ygg-git-compare--remote-start (current-buffer) pr key)
        (ygg-git-compare-async-tests--wait #'ygg-git-compare-async-tests--idle-p)
        (ygg-git-compare--remote-start (current-buffer) pr key)
        (should (= (funcall fetches) 1))))))
