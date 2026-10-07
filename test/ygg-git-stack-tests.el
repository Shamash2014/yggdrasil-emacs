;;; ygg-git-stack-tests.el --- stacked pull requests in magit status -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-stack)

(defvar ygg-git-stack-tests--dir nil)
(defvar ygg-git-stack-tests--prompts nil)
(defvar ygg-git-stack-tests--messages nil)
(defvar ygg-git-stack-tests--answer t)

(defconst ygg-git-stack-tests--repo '(github "github.com" "o/r"))

(defconst ygg-git-stack-tests--fake
  "#!/bin/sh
echo \"$(basename \"$0\") $*\" >> \"$FAKE_DIR/log\"
exit 0
")

(defun ygg-git-stack-tests--git (dir &rest args)
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-stack-tests--work () (expand-file-name "work" ygg-git-stack-tests--dir))
(defun ygg-git-stack-tests--origin () (expand-file-name "origin.git" ygg-git-stack-tests--dir))

(defun ygg-git-stack-tests--w (&rest args)
  (apply #'ygg-git-stack-tests--git (ygg-git-stack-tests--work) args))

(defun ygg-git-stack-tests--commit (dir file text message)
  (with-temp-file (expand-file-name file dir) (insert text))
  (ygg-git-stack-tests--git dir "add" file)
  (ygg-git-stack-tests--git dir "commit" "-q" "-m" message))

(defun ygg-git-stack-tests--origin-tip (branch)
  (ygg-git-stack-tests--git (ygg-git-stack-tests--origin) "rev-parse" (concat "refs/heads/" branch)))

(defun ygg-git-stack-tests--tip (branch)
  (ygg-git-stack-tests--w "rev-parse" (concat "refs/heads/" branch)))

(defun ygg-git-stack-tests--descends (branch ancestor)
  (zerop (let ((default-directory (ygg-git-stack-tests--work)))
           (call-process "git" nil nil nil "merge-base" "--is-ancestor" ancestor branch))))

(defun ygg-git-stack-tests--setup ()
  (let ((work (ygg-git-stack-tests--work)))
    (ygg-git-stack-tests--git ygg-git-stack-tests--dir "init" "-q" "--bare" "-b" "main" "origin.git")
    (ygg-git-stack-tests--git ygg-git-stack-tests--dir "clone" "-q" "origin.git" "work")
    (ygg-git-stack-tests--git work "config" "user.name" "T")
    (ygg-git-stack-tests--git work "config" "user.email" "t@example.invalid")
    (ygg-git-stack-tests--git work "checkout" "-q" "-B" "main")
    (ygg-git-stack-tests--commit work "base.txt" "base\n" "base")
    (ygg-git-stack-tests--git work "push" "-q" "-u" "origin" "main")
    (let ((parent "main"))
      (dolist (name '("feat-a" "feat-b" "feat-c"))
        (ygg-git-stack-tests--git work "checkout" "-q" "-b" name)
        (ygg-git-stack-tests--commit work (concat name ".txt") (concat name "\n") name)
        (ygg-git-stack-tests--git work "push" "-q" "-u" "origin" name)
        (ygg-git-stack-tests--git work "config" (format "branch.%s.ygg-parent" name) parent)
        (setq parent name)))))

(defun ygg-git-stack-tests--seed (rows)
  (puthash ygg-git-stack-tests--repo (cons (float-time) (list :rows rows))
           ygg-git-review-requests--cache))

(defun ygg-git-stack-tests--wait (pred)
  (let ((deadline (+ (float-time) 15)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should (funcall pred))))

(defun ygg-git-stack-tests--said (regexp)
  (seq-find (lambda (m) (string-match-p regexp m)) ygg-git-stack-tests--messages))

(defun ygg-git-stack-tests--log ()
  (let ((file (expand-file-name "log" ygg-git-stack-tests--dir)))
    (and (file-exists-p file)
         (split-string (with-temp-buffer (insert-file-contents file) (buffer-string)) "\n" t))))

(defmacro ygg-git-stack-tests--deftest (name &rest body)
  (declare (indent 1))
  `(ert-deftest ,name ()
     (let* ((ygg-git-stack-tests--dir (file-truename (make-temp-file "ygg-stack" t)))
            (exec-path (cons ygg-git-stack-tests--dir exec-path))
            (process-environment
             (append (list (concat "PATH=" ygg-git-stack-tests--dir ":" (getenv "PATH"))
                           (concat "FAKE_DIR=" ygg-git-stack-tests--dir)
                           "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                           "GIT_TERMINAL_PROMPT=0")
                     process-environment))
            (ygg-git-review-requests--cache (make-hash-table :test 'equal))
            (ygg-git-review-requests--loaded t)
            (ygg-git-stack--pending nil)
            (ygg-git-stack--asked nil)
            (ygg-git-stack--checking nil)
            (ygg-git-stack--merged (make-hash-table :test 'equal))
            (ygg-git-stack--info-cache (make-hash-table :test 'equal))
            (ygg-git-stack--seen (make-hash-table :test 'equal))
            (ygg-git-stack-tests--prompts nil)
            (ygg-git-stack-tests--messages nil)
            (ygg-git-stack-tests--answer t))
       (unwind-protect
           (progn
             (dolist (program '("gh" "glab"))
               (let ((script (expand-file-name program ygg-git-stack-tests--dir)))
                 (with-temp-file script (insert ygg-git-stack-tests--fake))
                 (set-file-modes script #o755)))
             (ygg-git-stack-tests--setup)
             (let ((default-directory (file-name-as-directory (ygg-git-stack-tests--work))))
               (cl-letf (((symbol-function 'y-or-n-p)
                          (lambda (prompt)
                            (push prompt ygg-git-stack-tests--prompts)
                            ygg-git-stack-tests--answer))
                         ((symbol-function 'message)
                          (lambda (fmt &rest args)
                            (when fmt (push (apply #'format fmt args) ygg-git-stack-tests--messages))))
                         ((symbol-function 'magit-status-setup-buffer) #'ignore)
                         ((symbol-function 'ygg-git-review-requests--repo)
                          (lambda () ygg-git-stack-tests--repo)))
                 ,@body)))
         (delete-directory ygg-git-stack-tests--dir t)))))

(defun ygg-git-stack-tests--amend (branch file text)
  (ygg-git-stack-tests--w "checkout" "-q" branch)
  (with-temp-file (expand-file-name file (ygg-git-stack-tests--work)) (insert text))
  (ygg-git-stack-tests--w "add" file)
  (ygg-git-stack-tests--w "commit" "-q" "--amend" "--no-edit"))

(ygg-git-stack-tests--deftest ygg-git-stack-parent-config-first-then-pr-base
  (ygg-git-stack-tests--w "config" "--unset" "branch.feat-c.ygg-parent")
  (ygg-git-stack-tests--seed
   (list (list :number 102 :head "feat-b" :base "other")
         (list :number 103 :head "feat-c" :base "feat-b")))
  (should (equal (ygg-git-stack--parent "feat-b") "feat-a"))
  (should (equal (ygg-git-stack--parent "feat-c") "feat-b"))
  (should-not (ygg-git-stack--parent "main")))

(ygg-git-stack-tests--deftest ygg-git-stack-chain-runs-bottom-to-top-from-any-branch
  (let ((parents (ygg-git-stack--parents (magit-list-local-branch-names) nil)))
    (dolist (start '("feat-a" "feat-b" "feat-c"))
      (should (equal (ygg-git-stack--chain start parents) '("feat-a" "feat-b" "feat-c"))))))

(ygg-git-stack-tests--deftest ygg-git-stack-branch-records-its-parent
  (ygg-git-stack-branch "feat-d")
  (ygg-git-stack-tests--wait (lambda () (equal (magit-get-current-branch) "feat-d")))
  (should (equal (ygg-git-stack--config-parent "feat-d") "feat-c"))
  (should (equal (ygg-git-stack-tests--tip "feat-d") (ygg-git-stack-tests--tip "feat-c"))))

(ygg-git-stack-tests--deftest ygg-git-stack-parses-review-and-checks
  (let ((row (car (ygg-git-review-requests--parse
                   'github
                   "[{\"number\":1,\"title\":\"t\",\"author\":{\"login\":\"x\"},\"reviewRequests\":[],\"headRefName\":\"h\",\"baseRefName\":\"b\",\"url\":\"u\",\"isDraft\":false,\"updatedAt\":\"2026-01-01T00:00:00Z\",\"reviewDecision\":\"APPROVED\",\"statusCheckRollup\":[{\"conclusion\":\"FAILURE\",\"status\":\"COMPLETED\"}]}]"))))
    (should (equal (plist-get row :review) "approved"))
    (should (eq (plist-get row :checks) 'failing))))

(ygg-git-stack-tests--deftest ygg-git-stack-section-lists-the-stack-bottom-to-top
  (ygg-git-stack-tests--w "config" "--unset" "branch.feat-c.ygg-parent")
  (ygg-git-stack-tests--seed
   (list (list :number 101 :head "feat-a" :base "main" :review "approved" :checks 'passing)
         (list :number 102 :head "feat-b" :base "feat-a" :review nil :checks 'failing)
         (list :number 103 :head "feat-c" :base "feat-b" :draft t :checks 'passing)))
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (status)
        (ygg-git-stack-insert-section)))
    (let ((text (buffer-string)))
      (should (string-match-p "Stack (3)" text))
      (should (string-match-p "feat-a  #101  approved  CI ok  \\+1 on main" text))
      (should (string-match-p "feat-b  #102  review  CI failed  \\+1 on feat-a" text))
      (should (string-match-p "feat-c  #103  draft  CI ok  \\+1 on feat-b" text))
      (should (< (string-match "#101" text) (string-match "#102" text) (string-match "#103" text))))))

(ygg-git-stack-tests--deftest ygg-git-stack-section-shows-behind-the-parent
  (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (status)
        (ygg-git-stack-insert-section)))
    (should (string-match-p "feat-b  \\+2 on feat-a  behind 1" (buffer-string)))))

(ygg-git-stack-tests--deftest ygg-git-stack-section-hidden-for-a-lone-branch
  (ygg-git-stack-tests--w "checkout" "-q" "main")
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (status)
        (ygg-git-stack-insert-section)))
    (should (string-empty-p (buffer-string)))))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-moves-the-stack-and-pushes-changed-only
  (let ((old-a (ygg-git-stack-tests--origin-tip "feat-a"))
        (old-b (ygg-git-stack-tests--origin-tip "feat-b"))
        (old-c (ygg-git-stack-tests--origin-tip "feat-c")))
    (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait
     (lambda () (not (equal (ygg-git-stack-tests--origin-tip "feat-c") old-c))))
    (should (ygg-git-stack-tests--descends "feat-b" (ygg-git-stack-tests--tip "feat-a")))
    (should (ygg-git-stack-tests--descends "feat-c" (ygg-git-stack-tests--tip "feat-b")))
    (should (equal (ygg-git-stack-tests--origin-tip "feat-b") (ygg-git-stack-tests--tip "feat-b")))
    (should (equal (ygg-git-stack-tests--origin-tip "feat-c") (ygg-git-stack-tests--tip "feat-c")))
    (should-not (equal (ygg-git-stack-tests--origin-tip "feat-b") old-b))
    (should (equal (ygg-git-stack-tests--origin-tip "feat-a") (ygg-git-stack-tests--tip "feat-a")))
    (should-not (equal (ygg-git-stack-tests--origin-tip "feat-a") old-a))
    (should (equal (magit-get-current-branch) "feat-c"))
    (should (= 1 (length ygg-git-stack-tests--prompts)))
    (let ((prompt (car ygg-git-stack-tests--prompts)))
      (should (string-match-p "origin/feat-a  [0-9a-f]+ -> [0-9a-f]+ (amended)" prompt))
      (should (string-match-p "origin/feat-b  [0-9a-f]+ -> [0-9a-f]+\\(\n\\|$\\)" prompt))
      (should (string-match-p "origin/feat-c" prompt))
      (should-not (string-match-p "origin/main" prompt)))))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-from-the-middle-keeps-the-checked-out-branch
  (let ((old-b (ygg-git-stack-tests--origin-tip "feat-b")))
    (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-b")
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait
     (lambda () (not (equal (ygg-git-stack-tests--origin-tip "feat-b") old-b))))
    (should (equal (magit-get-current-branch) "feat-b"))
    (should (ygg-git-stack-tests--descends "feat-c" (ygg-git-stack-tests--tip "feat-b")))))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-pushes-nothing-when-declined
  (let ((old-b (ygg-git-stack-tests--origin-tip "feat-b")))
    (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
    (setq ygg-git-stack-tests--answer nil)
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait (lambda () (ygg-git-stack-tests--said "nothing pushed")))
    (should (equal (ygg-git-stack-tests--origin-tip "feat-b") old-b))
    (should-not (equal (ygg-git-stack-tests--tip "feat-b") old-b))))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-lease-refuses-a-moved-remote-branch
  (let ((other (expand-file-name "other" ygg-git-stack-tests--dir)))
    (ygg-git-stack-tests--git ygg-git-stack-tests--dir "clone" "-q" "origin.git" "other")
    (ygg-git-stack-tests--git other "config" "user.name" "O")
    (ygg-git-stack-tests--git other "config" "user.email" "o@example.invalid")
    (ygg-git-stack-tests--git other "checkout" "-q" "feat-b")
    (ygg-git-stack-tests--commit other "theirs.txt" "x\n" "theirs")
    (ygg-git-stack-tests--git other "push" "-q" "origin" "feat-b")
    (let ((theirs (ygg-git-stack-tests--origin-tip "feat-b")))
      (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
      (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
      (ygg-git-stack-restack)
      (ygg-git-stack-tests--wait (lambda () (ygg-git-stack-tests--said "Push to origin failed")))
      (should (equal (ygg-git-stack-tests--origin-tip "feat-b") theirs)))))

(defun ygg-git-stack-tests--teammate-push (branch)
  (let ((other (expand-file-name "other" ygg-git-stack-tests--dir)))
    (ygg-git-stack-tests--git ygg-git-stack-tests--dir "clone" "-q" "origin.git" "other")
    (ygg-git-stack-tests--git other "config" "user.name" "O")
    (ygg-git-stack-tests--git other "config" "user.email" "o@example.invalid")
    (ygg-git-stack-tests--git other "checkout" "-q" branch)
    (ygg-git-stack-tests--commit other "theirs.txt" "x\n" "theirs")
    (ygg-git-stack-tests--git other "push" "-q" "origin" branch)
    (ygg-git-stack-tests--w "fetch" "-q" "origin")
    (ygg-git-stack-tests--origin-tip branch)))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-does-not-offer-an-amended-branch-with-a-teammates-commit
  (let ((theirs (ygg-git-stack-tests--teammate-push "feat-a")))
    (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait (lambda () ygg-git-stack-tests--prompts))
    (let ((prompt (car ygg-git-stack-tests--prompts)))
      (should-not (string-match-p "origin/feat-a  " prompt))
      (should (string-match-p "origin/feat-b  " prompt))
      (should (string-match-p "not pushed: origin/feat-a has commits you never had locally" prompt)))
    (ygg-git-stack-tests--settle)
    (should (equal (ygg-git-stack-tests--origin-tip "feat-a") theirs))))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-does-not-offer-a-rebased-branch-someone-else-pushed-to
  (ygg-git-stack-tests--teammate-push "feat-b")
  (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (ygg-git-stack-restack)
  (ygg-git-stack-tests--wait (lambda () ygg-git-stack-tests--prompts))
  (let ((prompt (car ygg-git-stack-tests--prompts)))
    (should-not (string-match-p "origin/feat-b  " prompt))
    (should (string-match-p "origin/feat-a  .*(amended)" prompt))
    (should (string-match-p "origin/feat-c  " prompt))
    (should (string-match-p "not pushed: origin/feat-b has commits you never had locally" prompt))))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-offers-nothing-when-the-reflog-is-gone
  (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (cl-letf (((symbol-function 'ygg-git-stack--had-locally-p) #'ignore))
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait (lambda () (ygg-git-stack-tests--said "nothing to push")))
    (should (ygg-git-stack-tests--said "not pushed: origin/feat-b"))
    (should-not ygg-git-stack-tests--prompts)))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-refuses-a-dirty-worktree
  (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (with-temp-file (expand-file-name "feat-c.txt" (ygg-git-stack-tests--work)) (insert "dirty\n"))
  (let ((before (ygg-git-stack-tests--tip "feat-b")))
    (should-error (ygg-git-stack-restack) :type 'user-error)
    (should (equal (ygg-git-stack-tests--tip "feat-b") before))
    (should-not ygg-git-stack--pending)))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-with-nothing-to-do-says-so
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (ygg-git-stack-restack)
  (should (ygg-git-stack-tests--said "already in order"))
  (should-not ygg-git-stack--pending))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-conflict-stops-then-resumes
  (let ((work (magit-toplevel))
        old-b)
    (ygg-git-stack-tests--w "checkout" "-q" "feat-a")
    (ygg-git-stack-tests--commit work "shared.txt" "one\n" "shared")
    (ygg-git-stack-tests--w "push" "-q" "origin" "feat-a")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-b")
    (ygg-git-stack-tests--w "rebase" "-q" "feat-a")
    (ygg-git-stack-tests--commit work "shared.txt" "two\n" "edit in b")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
    (ygg-git-stack-tests--w "rebase" "-q" "feat-b")
    (ygg-git-stack-tests--w "push" "-q" "-f" "origin" "feat-a" "feat-b" "feat-c")
    (setq old-b (ygg-git-stack-tests--origin-tip "feat-b"))
    (ygg-git-stack-tests--w "checkout" "-q" "feat-a")
    (with-temp-file (expand-file-name "shared.txt" work) (insert "three\n"))
    (ygg-git-stack-tests--w "add" "shared.txt")
    (ygg-git-stack-tests--w "commit" "-q" "--amend" "--no-edit")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait (lambda () (ygg-git-stack-tests--said "stopped on a conflict")))
    (should (magit-rebase-in-progress-p))
    (should (assoc work ygg-git-stack--pending))
    (should-not ygg-git-stack-tests--prompts)
    (should (equal (ygg-git-stack-tests--origin-tip "feat-b") old-b))
    (ygg-git-stack--after-rebase work)
    (should (assoc work ygg-git-stack--pending))
    (with-temp-file (expand-file-name "shared.txt" work) (insert "resolved\n"))
    (ygg-git-stack-tests--w "add" "shared.txt")
    (let ((process-environment (cons "GIT_EDITOR=true" process-environment)))
      (ygg-git-stack-tests--w "rebase" "--continue"))
    (ygg-git-stack--after-rebase work)
    (ygg-git-stack-tests--wait
     (lambda () (equal (ygg-git-stack-tests--origin-tip "feat-c") (ygg-git-stack-tests--tip "feat-c"))))
    (should-not (equal (ygg-git-stack-tests--origin-tip "feat-b") old-b))
    (should-not ygg-git-stack--pending)))

(ygg-git-stack-tests--deftest ygg-git-stack-compare-base-is-the-parent
  (ygg-git-stack-tests--seed nil)
  (should (equal (ygg-git-compare-default-a
                  (list 'pr :number 102 :head "feat-b" :base "main" :remote "origin"))
                 '(rev . "origin/feat-a")))
  (should (equal (ygg-git-compare-default-a
                  (list 'pr :number 101 :head "feat-a" :base "main" :remote "origin"))
                 '(rev . "origin/main")))
  (ygg-git-stack-tests--w "config" "--unset" "branch.feat-b.ygg-parent")
  (should (equal (ygg-git-compare-default-a
                  (list 'pr :number 102 :head "feat-b" :base "main" :remote "origin"))
                 '(rev . "origin/main"))))

(defun ygg-git-stack-tests--squash-merge-a ()
  (let ((other (expand-file-name "other" ygg-git-stack-tests--dir)))
    (ygg-git-stack-tests--git ygg-git-stack-tests--dir "clone" "-q" "origin.git" "other")
    (ygg-git-stack-tests--git other "config" "user.name" "O")
    (ygg-git-stack-tests--git other "config" "user.email" "o@example.invalid")
    (ygg-git-stack-tests--git other "merge" "-q" "--squash" "origin/feat-a")
    (ygg-git-stack-tests--git other "commit" "-q" "-m" "squash a")
    (ygg-git-stack-tests--git other "push" "-q" "origin" "main")))

(ygg-git-stack-tests--deftest ygg-git-stack-merge-of-the-bottom-asks-once-retargets-and-restacks
  (ygg-git-stack-tests--seed
   (list (list :number 102 :head "feat-b" :base "feat-a")
         (list :number 103 :head "feat-c" :base "feat-b")))
  (ygg-git-stack-tests--squash-merge-a)
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (let ((old-b (ygg-git-stack-tests--origin-tip "feat-b"))
        (origin (current-buffer)))
    (cl-letf (((symbol-function 'ygg-git-pr-merge--refetch) #'ignore))
      (ygg-git-pr-merge--report ygg-git-stack-tests--repo
                                '(:number 101 :head "feat-a" :base "main")
                                '(:target "main" :method squash) origin 'merged nil))
    (ygg-git-stack-tests--wait
     (lambda () (not (equal (ygg-git-stack-tests--origin-tip "feat-b") old-b))))
    (should (equal (car (last ygg-git-stack-tests--prompts))
                   "Retarget #102 to main and restack? "))
    (should (member "gh pr edit 102 --repo github.com/o/r --base main" (ygg-git-stack-tests--log)))
    (should (equal (ygg-git-stack--config-parent "feat-b") "main"))
    (should (ygg-git-stack-tests--descends "feat-b" "origin/main"))
    (should (ygg-git-stack-tests--descends "feat-c" (ygg-git-stack-tests--tip "feat-b")))
    (should (equal (ygg-git-stack-tests--origin-tip "feat-b") (ygg-git-stack-tests--tip "feat-b")))
    (should (equal (ygg-git-stack-tests--origin-tip "feat-c") (ygg-git-stack-tests--tip "feat-c")))
    (let ((count (length ygg-git-stack-tests--prompts)))
      (ygg-git-stack--after-merge ygg-git-stack-tests--repo "feat-a" "main")
      (should (= count (length ygg-git-stack-tests--prompts))))))

(ygg-git-stack-tests--deftest ygg-git-stack-merge-declined-changes-nothing
  (ygg-git-stack-tests--seed (list (list :number 102 :head "feat-b" :base "feat-a")))
  (setq ygg-git-stack-tests--answer nil)
  (ygg-git-stack--after-merge ygg-git-stack-tests--repo "feat-a" "main")
  (should (equal ygg-git-stack-tests--prompts '("Retarget #102 to main and restack? ")))
  (should-not (ygg-git-stack-tests--log))
  (should (equal (ygg-git-stack--config-parent "feat-b") "feat-a")))

(ygg-git-stack-tests--deftest ygg-git-stack-vanished-request-is-checked-with-the-forge
  (ygg-git-stack-tests--seed (list (list :number 101 :head "feat-a" :base "main")))
  (let ((root (magit-toplevel)))
    (puthash (list root "feat-a") '(101 . "main") ygg-git-stack--seen)
    (cl-letf (((symbol-function 'ygg-git-stack--verify-merged)
               (lambda (_repo branch _key seen)
                 (push (list branch (car seen)) ygg-git-stack-tests--messages))))
      (ygg-git-stack--state ygg-git-stack-tests--repo (list :rows nil) "feat-a" nil)
      (ygg-git-stack--state ygg-git-stack-tests--repo (list :error "x") "feat-a" nil))
    (should (equal ygg-git-stack-tests--messages '(("feat-a" 101))))))

(defun ygg-git-stack-tests--conflict (start)
  "Stop a restack from START on a conflict; return feat-b's old origin tip."
  (let ((work (magit-toplevel)) old-b)
    (ygg-git-stack-tests--w "checkout" "-q" "feat-a")
    (ygg-git-stack-tests--commit work "shared.txt" "one\n" "shared")
    (ygg-git-stack-tests--w "push" "-q" "origin" "feat-a")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-b")
    (ygg-git-stack-tests--w "rebase" "-q" "feat-a")
    (ygg-git-stack-tests--commit work "shared.txt" "two\n" "edit in b")
    (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
    (ygg-git-stack-tests--w "rebase" "-q" "feat-b")
    (ygg-git-stack-tests--w "push" "-q" "-f" "origin" "feat-a" "feat-b" "feat-c")
    (setq old-b (ygg-git-stack-tests--origin-tip "feat-b"))
    (ygg-git-stack-tests--w "checkout" "-q" "feat-a")
    (with-temp-file (expand-file-name "shared.txt" work) (insert "three\n"))
    (ygg-git-stack-tests--w "add" "shared.txt")
    (ygg-git-stack-tests--w "commit" "-q" "--amend" "--no-edit")
    (ygg-git-stack-tests--w "checkout" "-q" start)
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait (lambda () (ygg-git-stack-tests--said "stopped on a conflict")))
    old-b))

(defun ygg-git-stack-tests--settle ()
  (let ((deadline (+ (float-time) 0.5)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

(defvar ygg-git-stack-tests--procs nil)

(defun ygg-git-stack-tests--count-procs (thunk)
  (setq ygg-git-stack-tests--procs 0)
  (let ((counter (lambda (_program _in _out _display &rest args)
                   (unless (or (member "--show-toplevel" args) (member "--show-cdup" args))
                     (cl-incf ygg-git-stack-tests--procs)))))
    (advice-add 'call-process :before counter)
    (unwind-protect
        (let ((magit--refresh-cache (list (cons 0 0))))
          (funcall thunk))
      (advice-remove 'call-process counter)))
  ygg-git-stack-tests--procs)

(defun ygg-git-stack-tests--render ()
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (status)
        (ygg-git-stack-insert-section)))
    (buffer-string)))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-refuses-a-branch-checked-out-in-another-worktree
  (ygg-git-stack-tests--amend "feat-a" "a.txt" "amended\n")
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (let ((wt (expand-file-name "wt" ygg-git-stack-tests--dir))
        (before (ygg-git-stack-tests--tip "feat-b")))
    (ygg-git-stack-tests--w "worktree" "add" "-q" wt "feat-b")
    (let ((err (should-error (ygg-git-stack-restack) :type 'user-error)))
      (should (string-match-p "feat-b" (cadr err)))
      (should (string-match-p "wt" (cadr err))))
    (should (equal (ygg-git-stack-tests--tip "feat-b") before))
    (should (equal (magit-get-current-branch) "feat-c"))
    (should-not ygg-git-stack--pending)))

(ygg-git-stack-tests--deftest ygg-git-stack-refresh-costs-few-git-calls
  (ygg-git-stack-tests--seed
   (list (list :number 105 :head "solo" :base "main")))
  (ygg-git-stack-tests--w "checkout" "-q" "-b" "solo" "main")
  (ygg-git-stack-tests--render)
  (should (<= (ygg-git-stack-tests--count-procs #'ygg-git-stack-tests--render) 2))
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (ygg-git-stack-tests--render)
  (let ((text nil))
    (should (<= (ygg-git-stack-tests--count-procs
                 (lambda () (setq text (ygg-git-stack-tests--render))))
                (+ 3 3)))
    (should (string-match-p "Stack (3)" text))))

(ygg-git-stack-tests--deftest ygg-git-stack-failed-rebase-returns-to-the-branch-you-were-on
  (ygg-git-stack-tests--w "checkout" "-q" "feat-b")
  (ygg-git-stack--run (magit-toplevel) "feat-b"
                      (list :branches '("feat-c") :stack '("feat-a" "feat-b" "feat-c")
                            :onto "origin/main" :old-base "deadbeef" :top "feat-c"))
  (ygg-git-stack-tests--wait (lambda () (ygg-git-stack-tests--said "Restack failed")))
  (ygg-git-stack-tests--wait (lambda () (equal (magit-get-current-branch) "feat-b")))
  (should-not ygg-git-stack--pending))

(ygg-git-stack-tests--deftest ygg-git-stack-aborted-rebase-resumes-and-returns-to-the-origin-branch
  (ygg-git-stack-tests--conflict "feat-b")
  (should (eq (plist-get (cdr (car ygg-git-stack--pending)) :state) 'stopped))
  (ygg-git-stack-tests--w "rebase" "--abort")
  (ygg-git-stack--resume)
  (ygg-git-stack-tests--wait (lambda () (null ygg-git-stack--pending)))
  (should (equal (magit-get-current-branch) "feat-b"))
  (should (ygg-git-stack-tests--said "Restack aborted"))
  (should-not ygg-git-stack-tests--prompts))

(ygg-git-stack-tests--deftest ygg-git-stack-refresh-ignores-a-restack-whose-rebase-has-not-begun
  (ygg-git-stack-tests--conflict "feat-b")
  (ygg-git-stack-tests--w "rebase" "--abort")
  (setcdr (car ygg-git-stack--pending)
          (plist-put (cdr (car ygg-git-stack--pending)) :state 'running))
  (ygg-git-stack--resume)
  (ygg-git-stack-tests--settle)
  (should ygg-git-stack--pending)
  (should (equal (magit-get-current-branch) "feat-c")))

(ygg-git-stack-tests--deftest ygg-git-stack-stale-or-unrelated-pending-never-switches-branches
  (ygg-git-stack-tests--conflict "feat-b")
  (ygg-git-stack-tests--w "rebase" "--abort")
  (setcdr (car ygg-git-stack--pending)
          (plist-put (cdr (car ygg-git-stack--pending)) :started 0))
  (ygg-git-stack--resume)
  (ygg-git-stack-tests--wait (lambda () (null ygg-git-stack--pending)))
  (should (equal (magit-get-current-branch) "feat-c"))
  (let ((root (magit-toplevel)))
    (ygg-git-stack-tests--w "checkout" "-q" "main")
    (push (cons root (list :token (list 'x) :state 'stopped :started (float-time) :orig "feat-b"
                           :branches '("feat-b" "feat-c") :before nil))
          ygg-git-stack--pending)
    (ygg-git-stack--resume)
    (ygg-git-stack-tests--wait (lambda () (null ygg-git-stack--pending)))
    (should (equal (magit-get-current-branch) "main"))
    (should-not ygg-git-stack-tests--prompts)))

(ygg-git-stack-tests--deftest ygg-git-stack-restack-after-the-bottom-was-merged-skips-it
  (ygg-git-stack-tests--seed
   (list (list :number 102 :head "feat-b" :base "feat-a")
         (list :number 103 :head "feat-c" :base "feat-b")))
  (ygg-git-stack-tests--squash-merge-a)
  (ygg-git-stack-tests--w "fetch" "-q" "origin")
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (setq ygg-git-stack-tests--answer nil)
  (ygg-git-stack--after-merge ygg-git-stack-tests--repo "feat-a" "main")
  (setq ygg-git-stack-tests--answer t)
  (let ((old-b (ygg-git-stack-tests--origin-tip "feat-b"))
        (tip-a (ygg-git-stack-tests--tip "feat-a")))
    (ygg-git-stack-restack)
    (ygg-git-stack-tests--wait
     (lambda () (not (equal (ygg-git-stack-tests--origin-tip "feat-b") old-b))))
    (should (ygg-git-stack-tests--descends "feat-b" "origin/main"))
    (should-not (ygg-git-stack-tests--descends "feat-b" tip-a))
    (should (ygg-git-stack-tests--descends "feat-c" (ygg-git-stack-tests--tip "feat-b")))
    (should (equal (ygg-git-stack--config-parent "feat-b") "main"))
    (should (equal (ygg-git-stack-tests--w "log" "--format=%s" "origin/main..feat-c")
                   "feat-c\nfeat-b"))))

(ygg-git-stack-tests--deftest ygg-git-stack-branch-refuses-an-existing-name
  (ygg-git-stack-tests--w "checkout" "-q" "feat-c")
  (should-error (ygg-git-stack-branch "feat-b") :type 'user-error)
  (should (equal (ygg-git-stack--config-parent "feat-b") "feat-a"))
  (should (equal (magit-get-current-branch) "feat-c")))

(ygg-git-stack-tests--deftest ygg-git-stack-retargets-to-the-base-the-merged-request-had
  (ygg-git-stack-tests--seed (list (list :number 101 :head "feat-a" :base "release")))
  (let ((root (magit-toplevel)) targets)
    (ygg-git-stack--state ygg-git-stack-tests--repo (list :rows nil) "other" nil)
    (puthash (list root "feat-a") '(101 . "release") ygg-git-stack--seen)
    (cl-letf (((symbol-function 'ygg-git-pr-merge--run)
               (lambda (_program _args done) (funcall done 0 "{\"state\":\"MERGED\"}" "")))
              ((symbol-function 'ygg-git-stack--after-merge)
               (lambda (_repo _branch target) (push target targets))))
      (ygg-git-stack--verify-merged ygg-git-stack-tests--repo "feat-a" (list root "feat-a")
                                    '(101 . "release"))
      (ygg-git-stack-tests--wait (lambda () targets)))
    (should (equal targets '("release")))))

(ygg-git-stack-tests--deftest ygg-git-stack-fork-follows-the-branch-you-are-on-and-hints-at-siblings
  (ygg-git-stack-tests--w "checkout" "-q" "-b" "feat-b2" "feat-a")
  (ygg-git-stack-tests--commit (magit-toplevel) "b2.txt" "x\n" "b2")
  (ygg-git-stack-tests--w "config" "branch.feat-b2.ygg-parent" "feat-a")
  (let ((text (ygg-git-stack-tests--render)))
    (should (string-match-p "Stack (2)" text))
    (should (string-match-p "feat-b2" text))
    (should-not (string-match-p "feat-b  " text)))
  (ygg-git-stack-tests--w "checkout" "-q" "feat-a")
  (let ((text (ygg-git-stack-tests--render)))
    (should (string-match-p "Stack (3)" text))
    (should (string-match-p "feat-a .*\\+1 other" text))
    (should-not (string-match-p "feat-b2" text))))

(provide 'ygg-git-stack-tests)
;;; ygg-git-stack-tests.el ends here
