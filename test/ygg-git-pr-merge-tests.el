;;; ygg-git-pr-merge-tests.el --- merging a pull request from magit -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(load (expand-file-name "test-elpaca" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-pr-merge)

(defvar ygg-git-pr-merge-tests--dir nil)

(defconst ygg-git-pr-merge-tests--fake
  "#!/bin/sh
echo \"$(basename \"$0\") $*\" >> \"$FAKE_DIR/log\"
case \"$*\" in
  \"pr merge\"*|\"mr merge\"*)
    touch \"$FAKE_DIR/did-merge\"
    if [ -f \"$FAKE_DIR/failmerge\" ]; then cat \"$FAKE_DIR/failmerge\" >&2; exit 1; fi
    exit 0;;
  \"pr view\"*\"state,autoMergeRequest\") cat \"$FAKE_DIR/post.json\"; exit 0;;
  \"pr view\"*)
    if [ -f \"$FAKE_DIR/prview2.json\" ] && [ \"$(grep -c '^gh pr view' \"$FAKE_DIR/log\")\" -ge 2 ]; then
      cat \"$FAKE_DIR/prview2.json\"; exit 0; fi
    cat \"$FAKE_DIR/prview.json\"; exit 0;;
  \"repo view\"*) cat \"$FAKE_DIR/repoview.json\"; exit 0;;
  *merge_requests/7)
    if [ -f \"$FAKE_DIR/did-merge\" ]; then cat \"$FAKE_DIR/post-mr.json\"; exit 0; fi
    cat \"$FAKE_DIR/mr.json\"; exit 0;;
  *DELETE*)
    if [ -f \"$FAKE_DIR/failrm\" ]; then cat \"$FAKE_DIR/failrm\" >&2; exit 1; fi
    exit 0;;
  api*) cat \"$FAKE_DIR/project.json\"; exit 0;;
esac
if [ -f \"$FAKE_DIR/fail\" ]; then cat \"$FAKE_DIR/fail\" >&2; exit 1; fi
exit 0
")

(defun ygg-git-pr-merge-tests--put (name text)
  (with-temp-file (expand-file-name name ygg-git-pr-merge-tests--dir)
    (insert text)))

(defun ygg-git-pr-merge-tests--log ()
  (let ((file (expand-file-name "log" ygg-git-pr-merge-tests--dir)))
    (and (file-exists-p file)
         (split-string (with-temp-buffer (insert-file-contents file) (buffer-string)) "\n" t))))

(defconst ygg-git-pr-merge-tests--gh-pr
  "{\"number\":5,\"headRefOid\":\"abc1234567890def\",\"isCrossRepository\":false,\"title\":\"Add thing\",\"state\":\"OPEN\",\"isDraft\":false,\"mergeable\":\"MERGEABLE\",\"mergeStateStatus\":\"CLEAN\",\"baseRefName\":\"main\",\"headRefName\":\"feat\",\"reviewDecision\":\"APPROVED\",\"statusCheckRollup\":[{\"conclusion\":\"SUCCESS\",\"status\":\"COMPLETED\"}]}")

(defconst ygg-git-pr-merge-tests--gh-repo
  "{\"defaultBranchRef\":{\"name\":\"develop\"},\"deleteBranchOnMerge\":true,\"mergeCommitAllowed\":true,\"squashMergeAllowed\":true,\"rebaseMergeAllowed\":false}")

(defconst ygg-git-pr-merge-tests--gl-mr
  "{\"iid\":7,\"sha\":\"def4567890123abc\",\"title\":\"Fix it\",\"state\":\"opened\",\"draft\":false,\"has_conflicts\":false,\"detailed_merge_status\":\"mergeable\",\"target_branch\":\"main\",\"source_branch\":\"fix\",\"head_pipeline\":{\"status\":\"failed\"}}")

(defconst ygg-git-pr-merge-tests--gl-project
  "{\"default_branch\":\"develop\",\"merge_method\":\"merge\",\"squash_option\":\"default_off\",\"remove_source_branch_after_merge\":false}")

(defvar ygg-git-pr-merge-tests--asked nil)
(defvar ygg-git-pr-merge-tests--messages nil)
(defvar ygg-git-pr-merge-tests--confirm nil)
(defvar ygg-git-pr-merge-tests--refetched nil)
(defvar ygg-git-pr-merge-tests--scope nil)
(defvar ygg-git-pr-merge-tests--defaults nil)
(defvar ygg-git-pr-merge-tests--header nil)
(defvar ygg-git-pr-merge-tests--after-start nil)
(defvar ygg-git-pr-merge-tests--local-tip nil)
(defvar ygg-git-pr-merge-tests--worktrees nil)
(defvar ygg-git-pr-merge-tests--ancestor t)
(defvar ygg-git-pr-merge-tests--local-deleted nil)

(defun ygg-git-pr-merge-tests--set (args prefix value)
  (let ((rest (seq-remove (lambda (a) (string-prefix-p prefix a)) args)))
    (if value (cons (concat prefix value) rest) rest)))

(defun ygg-git-pr-merge-tests--args (answers)
  (let ((args ygg-git-pr-merge-tests--defaults))
    (dolist (answer answers)
      (pcase (car answer)
        ("target" (setq args (ygg-git-pr-merge-tests--set args "--target=" (cdr answer))))
        ("method" (setq args (ygg-git-pr-merge-tests--set args "--method=" (cdr answer))))
        ("auto" (setq args (ygg-git-pr-merge-tests--set args "--auto" (and (equal (cdr answer) "yes") ""))))
        ("approve" (setq args (ygg-git-pr-merge-tests--set args "--approve" (and (equal (cdr answer) "yes") ""))))
        ("delete" (setq args (ygg-git-pr-merge-tests--set args "--delete" (and (equal (cdr answer) "yes") ""))))))
    args))

(defun ygg-git-pr-merge-tests--run (repo number &rest answers)
  "Drive the merge of NUMBER in REPO by setting the menu's arguments from
ANSWERS, an alist of argument name to value, then invoking Merge; return
once it says its last word."
  (ignore-errors (delete-file (expand-file-name "did-merge" ygg-git-pr-merge-tests--dir)))
  (setq ygg-git-pr-merge-tests--asked nil
        ygg-git-pr-merge-tests--messages nil
        ygg-git-pr-merge-tests--refetched nil
        ygg-git-pr-merge-tests--scope nil
        ygg-git-pr-merge-tests--defaults nil
        ygg-git-pr-merge-tests--header nil)
  (cl-letf (((symbol-function 'ygg-git-pr-merge--present)
             (lambda (scope)
               (setq ygg-git-pr-merge-tests--scope scope
                     ygg-git-pr-merge-tests--defaults (ygg-git-pr-merge--default-args scope))
               (unless (assq 'quit answers)
                 (let ((args (ygg-git-pr-merge-tests--args answers)))
                   (setq ygg-git-pr-merge-tests--header (ygg-git-pr-merge--header scope args))
                   (run-at-time 0 nil #'ygg-git-pr-merge--submit scope args)))))
            ((symbol-function 'y-or-n-p)
             (lambda (prompt)
               (push (cons 'confirm prompt) ygg-git-pr-merge-tests--asked)
               ygg-git-pr-merge-tests--confirm))
            ((symbol-function 'message)
             (lambda (fmt &rest args)
               (push (apply #'format fmt args) ygg-git-pr-merge-tests--messages)))
            ((symbol-function 'ygg-git-pr-merge--remote-branches)
             (lambda () '("main" "release")))
            ((symbol-function 'ygg-git-review-requests--ensure)
             (lambda (repo &optional force)
               (push (list repo force) ygg-git-pr-merge-tests--refetched))))
    (with-temp-buffer
      (ygg-git-pr-merge--start repo number)
      (when ygg-git-pr-merge-tests--after-start
        (funcall ygg-git-pr-merge-tests--after-start))
      (condition-case nil
          (let ((deadline (+ (float-time) 10)))
            (while (and (< (float-time) deadline)
                        (not (and (assq 'quit answers) ygg-git-pr-merge-tests--scope))
                        (not (seq-find (lambda (m) (string-match-p "\\`\\(Merged\\|Approved\\|Approval\\|Auto-merge\\|Queued\\|Merge of\\|Merge cancelled\\|Cannot\\|#[0-9]+ \\|The repo\\|The forge\\|Another\\)" m))
                                       ygg-git-pr-merge-tests--messages)))
              (accept-process-output nil 0.05)))
        (quit nil)))))

(defmacro ygg-git-pr-merge-tests--deftest (name &rest body)
  (declare (indent 1))
  `(ert-deftest ,name ()
     (let* ((ygg-git-pr-merge-tests--dir (make-temp-file "ygg-pm" t))
            (exec-path (cons ygg-git-pr-merge-tests--dir exec-path))
            (process-environment
             (append (list (concat "PATH=" ygg-git-pr-merge-tests--dir ":" (getenv "PATH"))
                           (concat "FAKE_DIR=" ygg-git-pr-merge-tests--dir))
                     process-environment))
            (ygg-git-pr-merge-tests--confirm t)
            (ygg-git-pr-merge-retry-delay 0)
            (ygg-git-pr-merge--active nil)
            (ygg-git-pr-merge-tests--after-start nil)
            (ygg-git-pr-merge-tests--local-tip nil)
            (ygg-git-pr-merge-tests--worktrees nil)
            (ygg-git-pr-merge-tests--ancestor t)
            (ygg-git-pr-merge-tests--local-deleted nil))
       (unwind-protect
           (cl-letf (((symbol-function 'ygg-git-pr-merge--local-tip)
                      (lambda (_branch) ygg-git-pr-merge-tests--local-tip))
                     ((symbol-function 'ygg-git-pr-merge--worktree-branches)
                      (lambda () ygg-git-pr-merge-tests--worktrees))
                     ((symbol-function 'ygg-git-pr-merge--ancestor-p)
                      (lambda (_tip _sha) ygg-git-pr-merge-tests--ancestor))
                     ((symbol-function 'ygg-git-pr-merge--delete-local)
                      (lambda (branch) (push branch ygg-git-pr-merge-tests--local-deleted) t)))
             (dolist (name '("gh" "glab"))
               (let ((script (expand-file-name name ygg-git-pr-merge-tests--dir)))
                 (with-temp-file script (insert ygg-git-pr-merge-tests--fake))
                 (set-file-modes script #o755)))
             (ygg-git-pr-merge-tests--put "prview.json" ygg-git-pr-merge-tests--gh-pr)
             (ygg-git-pr-merge-tests--put "repoview.json" ygg-git-pr-merge-tests--gh-repo)
             (ygg-git-pr-merge-tests--put "mr.json" ygg-git-pr-merge-tests--gl-mr)
             (ygg-git-pr-merge-tests--put "project.json" ygg-git-pr-merge-tests--gl-project)
             (ygg-git-pr-merge-tests--put "post.json" "{\"state\":\"MERGED\",\"autoMergeRequest\":null}")
             (ygg-git-pr-merge-tests--put "post-mr.json" "{\"state\":\"merged\"}")
             ,@body)
         (delete-directory ygg-git-pr-merge-tests--dir t)))))

(defconst ygg-git-pr-merge-tests--gh '(github "github.com" "o/r"))
(defconst ygg-git-pr-merge-tests--gl '(gitlab "gl.example" "o/r"))

(defun ygg-git-pr-merge-tests--deletes ()
  (seq-filter (lambda (l) (string-match-p " -X DELETE " l)) (ygg-git-pr-merge-tests--log)))

(defun ygg-git-pr-merge-tests--merge-calls ()
  (seq-filter (lambda (l) (string-match-p " \\(pr merge\\|pr edit\\|mr merge\\|mr update\\) " l))
              (ygg-git-pr-merge-tests--log)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-default-target-is-the-base
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should (equal (ygg-git-pr-merge-tests--merge-calls)
                 '("gh pr merge 5 --repo github.com/o/r --squash --match-head-commit abc1234567890def")))
  (should (member "--target=main" ygg-git-pr-merge-tests--defaults))
  (should (member "Merged #5 into main (squash), deleted feat"
                  ygg-git-pr-merge-tests--messages))
  (should (equal (ygg-git-pr-merge-tests--deletes)
                 '("gh api --hostname github.com -X DELETE repos/o/r/git/refs/heads/feat"))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-default-branch-retargets-first
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "develop") '("method" . "merge") '("delete" . "no"))
  (should (equal (ygg-git-pr-merge-tests--merge-calls)
                 '("gh pr edit 5 --repo github.com/o/r --base develop"
                   "gh pr merge 5 --repo github.com/o/r --merge --match-head-commit abc1234567890def")))
  (should (string-match-p "feat -> develop (retargeted from main first)"
                          ygg-git-pr-merge-tests--header)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-specific-branch-retargets-first
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "release") '("method" . "squash") '("delete" . "no"))
  (should (equal (seq-take (ygg-git-pr-merge-tests--merge-calls) 1)
                 '("gh pr edit 5 --repo github.com/o/r --base release")))
  (should (equal (ygg-git-pr-merge--target-choices
                  (plist-get ygg-git-pr-merge-tests--scope :info)
                  (plist-get ygg-git-pr-merge-tests--scope :remotes))
                 '("main" "develop" "release"))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-offers-only-allowed-methods
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should (equal (ygg-git-pr-merge--method-choices (plist-get ygg-git-pr-merge-tests--scope :info))
                 '("squash" "merge"))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-delete-defaults-from-the-repository
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should (member "--delete" ygg-git-pr-merge-tests--defaults))
  (ygg-git-pr-merge-tests--put "repoview.json"
                               (replace-regexp-in-string "\"deleteBranchOnMerge\":true" "\"deleteBranchOnMerge\":false"
                                                         ygg-git-pr-merge-tests--gh-repo))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not (member "--delete" ygg-git-pr-merge-tests--defaults))
  (should (ygg-git-pr-merge--delete-offered-p
           ygg-git-pr-merge-tests--gh (plist-get ygg-git-pr-merge-tests--scope :info) nil)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-menu-defaults-are-base-squash-and-checks
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should (equal ygg-git-pr-merge-tests--defaults
                 '("--target=main" "--method=squash" "--delete")))
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"conclusion\":\"SUCCESS\",\"status\":\"COMPLETED\""
                                                         "\"status\":\"IN_PROGRESS\"" ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should (equal ygg-git-pr-merge-tests--defaults
                 '("--target=main" "--method=squash" "--auto")))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7)
  (should (equal ygg-git-pr-merge-tests--defaults '("--target=main" "--method=squash")))
  (should (equal (ygg-git-pr-merge--method-choices (plist-get ygg-git-pr-merge-tests--scope :info))
                 '("squash" "merge"))))

(defun ygg-git-pr-merge-tests--open-menu ()
  (let ((scope ygg-git-pr-merge-tests--scope))
    (ygg-git-pr-merge--present scope)
    (prog1 (list (oref transient--prefix scope)
                 (transient-get-value)
                 (ygg-git-pr-merge--describe)
                 (delq nil (mapcar (lambda (o) (and (not (oref o inactive)) (ignore-errors (oref o argument))))
                                   transient--suffixes)))
      (transient--emergency-exit))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-menu-opens-scoped-with-its-defaults
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '(quit))
  (pcase-let ((`(,scope ,value ,header ,arguments) (ygg-git-pr-merge-tests--open-menu)))
    (should (equal scope ygg-git-pr-merge-tests--scope))
    (should (equal (sort value #'string<) '("--delete" "--method=squash" "--target=main")))
    (should (string-match-p "\\`Merge PR #5 \"Add thing\" at abc12345  feat -> main\n" header))
    (should (equal (sort arguments #'string<) '("--approve" "--auto" "--delete" "--method=" "--target="))))
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"conclusion\":\"SUCCESS\",\"status\":\"COMPLETED\""
                                                         "\"status\":\"IN_PROGRESS\"" ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '(quit))
  (ygg-git-pr-merge--present ygg-git-pr-merge-tests--scope)
  (should (string-search "the repository's setting decides"
                         (ygg-git-pr-merge--delete-description)))
  (transient--emergency-exit)
  (pcase-let ((`(,_ ,value ,_ ,arguments) (ygg-git-pr-merge-tests--open-menu)))
    (should (equal (sort value #'string<) '("--auto" "--method=squash" "--target=main")))
    (should (equal (sort arguments #'string<) '("--approve" "--auto" "--delete" "--method=" "--target=")))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-choice-is-read-from-the-menu-arguments
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '(quit))
  (let ((scope ygg-git-pr-merge-tests--scope))
    (should (equal (ygg-git-pr-merge--choice scope '("--target=release" "--method=merge" "--auto" "--delete"))
                   '(:target "release" :method merge :auto t :approve nil :delete nil)))
    (should (equal (ygg-git-pr-merge--choice scope '("--target=main" "--method=squash" "--delete"))
                   '(:target "main" :method squash :auto nil :approve nil :delete t)))
    (should-not (ygg-git-pr-merge--choice scope '("--target=main" "--method=rebase")))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-refuses-a-draft
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"isDraft\":false" "\"isDraft\":true"
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should-not ygg-git-pr-merge-tests--scope)
  (should (string-match-p "draft" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-refuses-a-conflict
  (ygg-git-pr-merge-tests--put "prview.json"
                               (let ((case-fold-search nil))
                                 (replace-regexp-in-string ":\"MERGEABLE" ":\"CONFLICTING"
                                                           ygg-git-pr-merge-tests--gh-pr)))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should (string-match-p "conflicts" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-declined-confirm-merges-nothing
  (let ((ygg-git-pr-merge-tests--confirm nil))
    (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                                 '("target" . "develop") '("method" . "squash") '("delete" . "no")))
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should-not ygg-git-pr-merge-tests--refetched)
  (should (member "Merge cancelled" ygg-git-pr-merge-tests--messages)))

(defun ygg-git-pr-merge-tests--confirm-text ()
  (cdr (assq 'confirm ygg-git-pr-merge-tests--asked)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-header-and-confirm-describe-the-merge
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (dolist (part '("Merge PR #5 \"Add thing\" at abc12345  feat -> main" "checks: passing"
                  "review: approved" "mergeable: mergeable"))
    (should (string-match-p (regexp-quote part) ygg-git-pr-merge-tests--header)))
  (should (equal (ygg-git-pr-merge-tests--confirm-text) "Squash-merge #5 into main and delete feat? ")))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-failure-surfaces-the-forge-text
  (ygg-git-pr-merge-tests--put "failmerge" "X Pull request is not mergeable: ghp_abcDEF123 base branch policy prohibits the merge\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "no"))
  (let ((said (car ygg-git-pr-merge-tests--messages)))
    (should (string-match-p "base branch policy prohibits the merge" said))
    (should-not (string-match-p "ghp_" said)))
  (should-not ygg-git-pr-merge-tests--refetched))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-failed-retarget-stops-before-merge
  (ygg-git-pr-merge-tests--put "fail" "no such branch\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "release") '("method" . "squash") '("delete" . "no"))
  (should (equal (length (ygg-git-pr-merge-tests--merge-calls)) 1))
  (should (string-match-p "no such branch" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-success-refetches-the-requests
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "no"))
  (should (equal ygg-git-pr-merge-tests--refetched
                 (list (list ygg-git-pr-merge-tests--gh t)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-merges-and-retargets
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "develop") '("method" . "squash") '("delete" . "yes"))
  (should (equal (ygg-git-pr-merge-tests--merge-calls)
                 '("glab mr update 7 -R gl.example/o/r --target-branch develop"
                   "glab mr merge 7 -R gl.example/o/r --squash --sha def4567890123abc --auto-merge=false --yes --remove-source-branch")))
  (should (string-match-p "checks: failing" ygg-git-pr-merge-tests--header))
  (should (string-match-p "\\`Merge MR #7 " ygg-git-pr-merge-tests--header))
  (should (equal (ygg-git-pr-merge--method-choices (plist-get ygg-git-pr-merge-tests--scope :info))
                 '("squash" "merge")))
  (should ygg-git-pr-merge-tests--refetched))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-plain-merge-has-no-method-flag
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "main") '("method" . "merge") '("delete" . "no"))
  (should (equal (ygg-git-pr-merge-tests--merge-calls)
                 '("glab mr merge 7 -R gl.example/o/r --sha def4567890123abc --auto-merge=false --yes"))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-refuses-a-conflict
  (ygg-git-pr-merge-tests--put "mr.json"
                               (replace-regexp-in-string "\"has_conflicts\":false" "\"has_conflicts\":true"
                                                         ygg-git-pr-merge-tests--gl-mr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7)
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should (string-match-p "conflicts" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-failure-surfaces-the-forge-text
  (ygg-git-pr-merge-tests--put "failmerge" "{\"message\":\"405 Method Not Allowed\"}")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "main") '("method" . "merge") '("delete" . "no"))
  (should (string-match-p "glab: \\|Method Not Allowed" (car ygg-git-pr-merge-tests--messages))))

(defun ygg-git-pr-merge-tests--answers (&rest extra)
  (append extra '(("target" . "main") ("method" . "squash") ("delete" . "no"))))

(defun ygg-git-pr-merge-tests--summary ()
  ygg-git-pr-merge-tests--header)

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-every-write-names-its-repository
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "release") '("method" . "squash") '("delete" . "no"))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "release") '("method" . "squash") '("delete" . "no"))
  (let ((calls (ygg-git-pr-merge-tests--merge-calls)))
    (should (= (length calls) 4))
    (dolist (call calls)
      (should (string-match-p " \\(--repo\\|-R\\) " call)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-pins-the-reviewed-head
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers))
  (should (string-match-p "--match-head-commit abc1234567890def"
                          (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "at abc12345  feat" (ygg-git-pr-merge-tests--summary)))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "main") '("method" . "merge"))
  (should (string-match-p "--sha def4567890123abc" (cadr (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "at def45678  fix" (ygg-git-pr-merge-tests--summary))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-refuses-without-a-head-commit
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"headRefOid\":\"[a-f0-9]+\"," ""
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should (string-match-p "no head commit" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-auto-merge-is-always-explicit
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
         (ygg-git-pr-merge-tests--answers '("auto" . "yes")))
  (should (string-match-p " --auto-merge=true " (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "when the pipeline passes" (ygg-git-pr-merge-tests--confirm-text)))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
         (ygg-git-pr-merge-tests--answers '("auto" . "no")))
  (should (string-match-p " --auto-merge=false " (cadr (ygg-git-pr-merge-tests--merge-calls))))
  (should-not (string-match-p "when the pipeline passes" (ygg-git-pr-merge-tests--confirm-text))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-github-auto-passes-auto
  (ygg-git-pr-merge-tests--put "post.json" "{\"state\":\"OPEN\",\"autoMergeRequest\":{\"enabledAt\":\"now\"}}")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash")
                               '("auto" . "yes") '("delete" . "yes"))
  (should (string-match-p " --auto$" (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (member "Auto-merge enabled for #5 (merges when the checks pass)"
                  ygg-git-pr-merge-tests--messages))
  (should-not (ygg-git-pr-merge--delete-offered-p
               ygg-git-pr-merge-tests--gh (plist-get ygg-git-pr-merge-tests--scope :info) t))
  (should-not (ygg-git-pr-merge-tests--deletes))
  (should (string-match-p "when the checks pass" (ygg-git-pr-merge-tests--confirm-text))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-github-merge-now-has-no-auto-flag
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("auto" . "no")))
  (should-not (string-match-p "--auto" (car (ygg-git-pr-merge-tests--merge-calls)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-auto-merge-defaults-follow-the-checks
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("auto" . "no")))
  (should-not (member "--auto" ygg-git-pr-merge-tests--defaults))
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"conclusion\":\"SUCCESS\",\"status\":\"COMPLETED\""
                                                         "\"status\":\"IN_PROGRESS\"" ygg-git-pr-merge-tests--gh-pr))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("auto" . "no")))
  (should (member "--auto" ygg-git-pr-merge-tests--defaults))
  (should (string-match-p "merging now will be queued or refused"
                          (ygg-git-pr-merge--header ygg-git-pr-merge-tests--scope
                                                    '("--target=main" "--method=squash")))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-says-merge-now-is-refused-when-a-pipeline-is-required
  (ygg-git-pr-merge-tests--put "project.json"
                               (replace-regexp-in-string "\"merge_method\"" "\"only_allow_merge_if_pipeline_succeeds\":true,\"merge_method\""
                                                         ygg-git-pr-merge-tests--gl-project))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7 (ygg-git-pr-merge-tests--answers))
  (should (string-match-p "pipeline must succeed: yes" (ygg-git-pr-merge-tests--summary)))
  (should (string-match-p "merging now will be refused" (ygg-git-pr-merge-tests--summary))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-success-wording-follows-the-forge
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (should (string-match-p "\\`Merged #5" (car ygg-git-pr-merge-tests--messages)))
  (ygg-git-pr-merge-tests--put "post.json" "{\"state\":\"OPEN\",\"autoMergeRequest\":null}")
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (should (equal (car ygg-git-pr-merge-tests--messages) "Queued #5"))
  (ygg-git-pr-merge-tests--put "post-mr.json" "{\"state\":\"opened\",\"merge_when_pipeline_succeeds\":true}")
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
         (ygg-git-pr-merge-tests--answers '("auto" . "yes")))
  (should (equal (car ygg-git-pr-merge-tests--messages)
                 "Auto-merge enabled for #7 (merges when the pipeline passes)")))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-guard-refuses-a-second-call
  (let (second)
    (setq ygg-git-pr-merge-tests--after-start
          (lambda ()
            (setq second (should-error (ygg-git-pr-merge--start ygg-git-pr-merge-tests--gh 5)
                                       :type 'user-error))))
    (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
    (should (string-match-p "already in progress" (cadr second)))
    (should-not ygg-git-pr-merge--active)
    (should (string-match-p "\\`Merged" (car ygg-git-pr-merge-tests--messages)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-guard-clears-when-the-menu-is-closed
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '(quit))
  (should ygg-git-pr-merge-tests--scope)
  (should-not ygg-git-pr-merge--active)
  (should-not (ygg-git-pr-merge-tests--merge-calls)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-guard-refuses-a-second-submit
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '(quit))
  (let ((scope ygg-git-pr-merge-tests--scope)
        (args ygg-git-pr-merge-tests--defaults))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'message)
               (lambda (fmt &rest rest)
                 (push (apply #'format fmt rest) ygg-git-pr-merge-tests--messages)))
              ((symbol-function 'ygg-git-review-requests--ensure) #'ignore))
      (setq ygg-git-pr-merge-tests--messages nil)
      (ygg-git-pr-merge--submit scope args)
      (ygg-git-pr-merge--submit scope args)
      (should (member "A merge of #5 is already in progress" ygg-git-pr-merge-tests--messages))
      (let ((deadline (+ (float-time) 10)))
        (while (and ygg-git-pr-merge--active (< (float-time) deadline))
          (accept-process-output nil 0.05))))
    (should (= 1 (length (seq-filter (lambda (l) (string-match-p "^gh pr merge " l))
                                     (ygg-git-pr-merge-tests--log)))))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-guard-clears-after-a-refusal-and-a-failed-read
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"isDraft\":false" "\"isDraft\":true"
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not ygg-git-pr-merge--active)
  (ygg-git-pr-merge-tests--put "prview.json" "not json")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not ygg-git-pr-merge--active)
  (should (string-match-p "Cannot read" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-guard-clears-after-failure-and-success
  (ygg-git-pr-merge-tests--put "failmerge" "boom\n")
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (should-not ygg-git-pr-merge--active)
  (delete-file (expand-file-name "failmerge" ygg-git-pr-merge-tests--dir))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (should-not ygg-git-pr-merge--active))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-unknown-mergeability-is-reread
  (let ((unknown (replace-regexp-in-string "\"MERGEABLE\"" "\"UNKNOWN\"" ygg-git-pr-merge-tests--gh-pr)))
    (ygg-git-pr-merge-tests--put "prview.json" unknown)
    (ygg-git-pr-merge-tests--put "prview2.json" ygg-git-pr-merge-tests--gh-pr)
    (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
    (should (= 2 (seq-count (lambda (l) (string-match-p "^gh pr view 5 .*number,title" l))
                            (ygg-git-pr-merge-tests--log))))
    (should (ygg-git-pr-merge-tests--merge-calls))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-unknown-mergeability-gives-up-after-three-rereads
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"MERGEABLE\"" "\"UNKNOWN\"" ygg-git-pr-merge-tests--gh-pr))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (should (= 4 (seq-count (lambda (l) (string-match-p "^gh pr view 5 .*number,title" l))
                          (ygg-git-pr-merge-tests--log))))
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should (string-match-p "still computing mergeability; try again" (car ygg-git-pr-merge-tests--messages)))
  (should-not ygg-git-pr-merge--active))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-summary-shows-a-blocked-state
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"CLEAN\"" "\"BLOCKED\"" ygg-git-pr-merge-tests--gh-pr))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (should (string-match-p "  state: blocked" (ygg-git-pr-merge-tests--summary)))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (ygg-git-pr-merge-tests--put "prview.json" ygg-git-pr-merge-tests--gh-pr)
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (should-not (string-match-p "state:" (ygg-git-pr-merge-tests--summary))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-nil-number-is-a-user-error
  (should-error (ygg-git-pr-merge--start ygg-git-pr-merge-tests--gh nil) :type 'user-error)
  (cl-letf (((symbol-function 'ygg-git-pr-merge--locate)
             (lambda () (cons ygg-git-pr-merge-tests--gh nil))))
    (should-error (ygg-git-pr-merge--context) :type 'user-error))
  (should-not ygg-git-pr-merge--active))

(defun ygg-git-pr-merge-tests--gl-methods (method squash)
  (ygg-git-pr-merge-tests--put
   "project.json"
   (format "{\"default_branch\":\"develop\",\"merge_method\":\"%s\",\"squash_option\":\"%s\"}" method squash))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7 '("target" . "main"))
  (ygg-git-pr-merge--method-choices (plist-get ygg-git-pr-merge-tests--scope :info)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-ff-offers-only-rebase
  (should (equal (ygg-git-pr-merge-tests--gl-methods "ff" "never") '("fast-forward")))
  (should (member "--method=fast-forward" ygg-git-pr-merge-tests--defaults))
  (should (string-match-p " --rebase " (car (ygg-git-pr-merge-tests--merge-calls))))
  (should-not (string-match-p "--squash" (car (ygg-git-pr-merge-tests--merge-calls)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-rebase-merge-is-semi-linear
  (should (equal (ygg-git-pr-merge-tests--gl-methods "rebase_merge" "never") '("semi-linear merge")))
  (should (member "--method=semi-linear merge" ygg-git-pr-merge-tests--defaults))
  (should-not (string-match-p "--rebase\\|--squash" (car (ygg-git-pr-merge-tests--merge-calls)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-merge-method-and-squash-options
  (should (equal (ygg-git-pr-merge-tests--gl-methods "merge" "never") '("merge")))
  (should (member "--method=merge" ygg-git-pr-merge-tests--defaults))
  (should (equal (ygg-git-pr-merge-tests--gl-methods "merge" "default_off") '("squash" "merge")))
  (should (equal (ygg-git-pr-merge-tests--gl-methods "merge" "always") '("squash")))
  (should (string-match-p " --squash " (car (last (ygg-git-pr-merge-tests--merge-calls))))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-remote-branch-is-deleted-only-after-the-merge
  (ygg-git-pr-merge-tests--put "failmerge" "no\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should-not (ygg-git-pr-merge-tests--deletes))
  (should-not (seq-find (lambda (l) (string-match-p "--delete-branch" l)) (ygg-git-pr-merge-tests--log))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-remote-branch-delete-failure-is-reported
  (ygg-git-pr-merge-tests--put "failrm" "Reference does not exist\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should (string-match-p "\\`Merged #5.*deleting remote branch feat failed: .*Reference does not exist"
                          (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-cross-repository-branch-is-never-deleted
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"isCrossRepository\":false" "\"isCrossRepository\":true"
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should-not (ygg-git-pr-merge--delete-offered-p
               ygg-git-pr-merge-tests--gh (plist-get ygg-git-pr-merge-tests--scope :info) nil))
  (should-not (ygg-git-pr-merge-tests--deletes)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-merge-failure-after-retarget-says-so
  (ygg-git-pr-merge-tests--put "failmerge" "not mergeable\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "release") '("method" . "squash") '("delete" . "no"))
  (should (= 2 (length (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "failed after retargeting it to release (its base is now release): .*not mergeable"
                          (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-target-must-be-an-offered-branch
  (dolist (target '("" "nowhere" "feat" nil))
    (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                                 (cons "target" target) '("method" . "squash"))
    (should-not (ygg-git-pr-merge-tests--merge-calls))
    (should-not (assq 'confirm ygg-git-pr-merge-tests--asked))
    (should (string-match-p "Merge cancelled" (car ygg-git-pr-merge-tests--messages)))
    (should-not ygg-git-pr-merge--active))
  (should (equal (ygg-git-pr-merge--target-choices
                  (plist-get ygg-git-pr-merge-tests--scope :info)
                  (plist-get ygg-git-pr-merge-tests--scope :remotes))
                 '("main" "develop" "release"))))

(ert-deftest ygg-git-pr-merge-scrub-removes-forge-tokens ()
  (dolist (token '("ghp_abc123" "github_pat_AB12_cd" "glpat-abc_DEF-1" "gloas-abc123" "glptt-0123abcd"
                   "glcbt-64_abcDEF" "Bearer eyJhbGciOi.abc" "CI_JOB_TOKEN=abcd1234" "JOB-TOKEN: abcd1234"))
    (let ((said (ygg-git-pr-merge--scrub (format "bad credentials %s here" token))))
      (should (string-match-p "bad credentials <token> here" said)))))


(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-ask-yes-adds-the-flag-for-gitlab
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "main") '("method" . "merge") '("delete" . "yes"))
  (should (string-match-p " --remove-source-branch$" (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "and delete fix\\? " (ygg-git-pr-merge-tests--confirm-text)))
  (should (member "Merged #7 into main (merge), deleted fix" ygg-git-pr-merge-tests--messages)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-ask-yes-deletes-on-github-after-the-merge
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should (= 1 (length (ygg-git-pr-merge-tests--deletes))))
  (should (string-match-p "and delete feat\\? " (ygg-git-pr-merge-tests--confirm-text))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-never-offers-and-flags-nothing
  (let ((ygg-git-pr-merge-delete-branch 'never))
    (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                                 '("target" . "main") '("method" . "merge") '("delete" . "yes"))
    (should-not (member "--delete" ygg-git-pr-merge-tests--defaults))
    (should-not (ygg-git-pr-merge--delete-offered-p
                 ygg-git-pr-merge-tests--gl (plist-get ygg-git-pr-merge-tests--scope :info) nil))
    (should-not (string-match-p "remove-source-branch" (car (ygg-git-pr-merge-tests--merge-calls))))
    (should-not (string-match-p "delete" (ygg-git-pr-merge-tests--confirm-text)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-always-starts-with-the-deletion-on
  (let ((ygg-git-pr-merge-delete-branch 'always))
    (ygg-git-pr-merge-tests--put "repoview.json"
                                 (replace-regexp-in-string "\"deleteBranchOnMerge\":true" "\"deleteBranchOnMerge\":false"
                                                           ygg-git-pr-merge-tests--gh-repo))
    (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '("target" . "main"))
    (should (member "--delete" ygg-git-pr-merge-tests--defaults))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-mr-setting-defaults-to-delete
  (ygg-git-pr-merge-tests--put "mr.json"
                               (replace-regexp-in-string "\"source_branch\"" "\"force_remove_source_branch\":true,\"source_branch\""
                                                         ygg-git-pr-merge-tests--gl-mr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7 '("target" . "main"))
  (should (member "--delete" ygg-git-pr-merge-tests--defaults)))

(defvar aob-gc-kept-branches)

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-kept-and-base-branches-are-never-offered
  (dolist (head '("main" "stage" "release/1.2" "backup/x"))
   (let ((aob-gc-kept-branches "\\`\\(?:main\\|stage\\|release/.*\\|backup/.*\\)\\'"))
    (ygg-git-pr-merge-tests--put "prview.json"
                                 (replace-regexp-in-string "\"headRefName\":\"feat\""
                                                           (format "\"headRefName\":\"%s\"" head)
                                                           ygg-git-pr-merge-tests--gh-pr))
    (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                                 '("target" . "main") '("method" . "squash") '("delete" . "yes"))
    (should-not (plist-get (plist-get ygg-git-pr-merge-tests--scope :info) :deletable))
    (should-not (member "--delete" ygg-git-pr-merge-tests--defaults))
    (should-not (ygg-git-pr-merge-tests--deletes))))
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"baseRefName\":\"main\",\"headRefName\":\"feat\""
                                                         "\"baseRefName\":\"feat\",\"headRefName\":\"feat\""
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '("target" . "main"))
  (should-not (plist-get (plist-get ygg-git-pr-merge-tests--scope :info) :deletable)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-auto-merge-passes-the-flag
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "main") '("method" . "merge")
                               '("auto" . "yes") '("delete" . "yes"))
  (should (string-match-p "--auto-merge=true .*--remove-source-branch$"
                          (car (ygg-git-pr-merge-tests--merge-calls)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-local-branch-is-deleted-when-free-and-its-tip-matches
  (setq ygg-git-pr-merge-tests--local-tip "abc1234567890def")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should (equal ygg-git-pr-merge-tests--local-deleted '("feat")))
  (should (member "Merged #5 into main (squash), deleted feat (remote and local)"
                  ygg-git-pr-merge-tests--messages)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-local-branch-with-other-commits-is-kept
  (setq ygg-git-pr-merge-tests--local-tip "ffff" ygg-git-pr-merge-tests--ancestor nil)
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "main") '("method" . "squash") '("delete" . "yes"))
  (should-not ygg-git-pr-merge-tests--local-deleted)
  (should (string-match-p "kept local feat: it has commits the request does not"
                          (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-checked-out-local-branch-is-kept-with-a-note
  (setq ygg-git-pr-merge-tests--local-tip "abc1234567890def"
        ygg-git-pr-merge-tests--worktrees '("fix"))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "main") '("method" . "merge") '("delete" . "yes"))
  (should-not ygg-git-pr-merge-tests--local-deleted)
  (should (member "Merged #7 into main (merge), deleted fix; kept local fix: it is checked out in a worktree"
                  ygg-git-pr-merge-tests--messages)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-local-branch-is-untouched-when-the-merge-is-queued
  (setq ygg-git-pr-merge-tests--local-tip "abc1234567890def")
  (ygg-git-pr-merge-tests--put "post-mr.json" "{\"state\":\"opened\",\"merge_when_pipeline_succeeds\":true}")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("target" . "main") '("method" . "merge")
                               '("auto" . "yes") '("delete" . "yes"))
  (should-not ygg-git-pr-merge-tests--local-deleted))

(defun ygg-git-pr-merge-tests--approve-calls ()
  (seq-filter (lambda (l) (string-match-p " \\(pr review\\|mr approve\\) " l))
              (ygg-git-pr-merge-tests--log)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-approve-command-comes-after-the-retarget-on-both-forges
  (let ((gh-info (list :number 5 :sha "abc" :base "main"))
        (gl-info (list :number 7 :sha "def" :base "main")))
    (should (equal (ygg-git-pr-merge--commands ygg-git-pr-merge-tests--gh gh-info "release" 'squash t nil t)
                   '(("gh" "pr" "edit" "5" "--repo" "github.com/o/r" "--base" "release")
                     ("gh" "pr" "review" "5" "--repo" "github.com/o/r" "--approve")
                     ("gh" "pr" "merge" "5" "--repo" "github.com/o/r" "--squash"
                      "--match-head-commit" "abc" "--auto"))))
    (should (equal (ygg-git-pr-merge--commands ygg-git-pr-merge-tests--gl gl-info "main" 'merge t nil t)
                   '(("glab" "mr" "approve" "7" "-R" "gl.example/o/r")
                     ("glab" "mr" "merge" "7" "-R" "gl.example/o/r" "--sha" "def"
                      "--auto-merge=true" "--yes"))))
    (should (equal (ygg-git-pr-merge--commands ygg-git-pr-merge-tests--gl gl-info "develop" 'merge t nil t)
                   '(("glab" "mr" "update" "7" "-R" "gl.example/o/r" "--target-branch" "develop")
                     ("glab" "mr" "approve" "7" "-R" "gl.example/o/r")
                     ("glab" "mr" "merge" "7" "-R" "gl.example/o/r" "--sha" "def"
                      "--auto-merge=true" "--yes"))))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-no-approve-command-without-the-switch
  (let ((info (list :number 5 :sha "abc" :base "main")))
    (dolist (repo (list ygg-git-pr-merge-tests--gh ygg-git-pr-merge-tests--gl))
      (should-not (seq-find (lambda (c) (member (nth 2 c) '("review" "approve")))
                            (ygg-git-pr-merge--commands repo info "release" 'merge nil nil)))))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 (ygg-git-pr-merge-tests--answers))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7 (ygg-git-pr-merge-tests--answers))
  (should-not (ygg-git-pr-merge-tests--approve-calls)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-approve-then-merge-on-github
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("approve" . "yes")))
  (should (equal (seq-take (ygg-git-pr-merge-tests--log) 4)
                 (list (nth 0 (ygg-git-pr-merge-tests--log))
                       (nth 1 (ygg-git-pr-merge-tests--log))
                       "gh pr review 5 --repo github.com/o/r --approve"
                       "gh pr merge 5 --repo github.com/o/r --squash --match-head-commit abc1234567890def")))
  (should (string-match-p "\\`Approve, then squash-merge #5" (ygg-git-pr-merge-tests--confirm-text)))
  (should (string-match-p "\\`Approved, merged #5 into main (squash)" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-approve-then-merge-on-gitlab
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
         (ygg-git-pr-merge-tests--answers '("approve" . "yes")))
  (should (equal (ygg-git-pr-merge-tests--approve-calls) '("glab mr approve 7 -R gl.example/o/r")))
  (should (string-match-p "\\`Approved, merged #7" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-approve-works-with-auto-merge
  (ygg-git-pr-merge-tests--put "post.json" "{\"state\":\"OPEN\",\"autoMergeRequest\":{\"enabledAt\":\"now\"}}")
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("approve" . "yes") '("auto" . "yes")))
  (should (equal (ygg-git-pr-merge-tests--approve-calls)
                 '("gh pr review 5 --repo github.com/o/r --approve")))
  (should (string-match-p " --auto$" (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (member "Approved, auto-merge enabled for #5 (merges when the checks pass)"
                  ygg-git-pr-merge-tests--messages)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-failed-approval-runs-no-merge
  (ygg-git-pr-merge-tests--put "fail" "failed to create review: Can not approve your own pull request ghp_abcDEF123\n")
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("approve" . "yes")))
  (should (ygg-git-pr-merge-tests--approve-calls))
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (let ((said (car ygg-git-pr-merge-tests--messages)))
    (should (string-match-p "\\`Approval of #5 failed, nothing merged: .*Can not approve your own" said))
    (should-not (string-match-p "ghp_" said)))
  (should-not ygg-git-pr-merge-tests--refetched)
  (should-not ygg-git-pr-merge--active)
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
         (ygg-git-pr-merge-tests--answers '("approve" . "yes")))
  (should-not (seq-find (lambda (l) (string-prefix-p "glab mr merge" l)) (ygg-git-pr-merge-tests--log)))
  (should (string-match-p "\\`Approval of #7 failed" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-merge-failing-after-approval-says-it-was-approved
  (ygg-git-pr-merge-tests--put "failmerge" "not mergeable\n")
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("approve" . "yes")))
  (should (ygg-git-pr-merge-tests--approve-calls))
  (should (string-match-p "\\`Approved #5, but merge failed: .*not mergeable"
                          (car ygg-git-pr-merge-tests--messages)))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "release") '("method" . "squash") '("delete" . "no")
                               '("approve" . "yes"))
  (should (string-match-p "\\`Approved #5, but merge failed after retargeting it to release.*not mergeable"
                          (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-failed-retarget-leaves-no-approval-behind
  (ygg-git-pr-merge-tests--put "fail" "no such branch\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("target" . "release") '("method" . "squash") '("delete" . "no")
                               '("approve" . "yes"))
  (should-not (ygg-git-pr-merge-tests--approve-calls))
  (should (equal (length (ygg-git-pr-merge-tests--merge-calls)) 1))
  (should (string-match-p "no such branch" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-menu-offers-the-approve-switch
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '(quit))
  (pcase-let ((`(,_ ,value ,_ ,arguments) (ygg-git-pr-merge-tests--open-menu)))
    (should (member "--approve" arguments))
    (should-not (member "--approve" value)))
  (ygg-git-pr-merge--present ygg-git-pr-merge-tests--scope)
  (should (string-match-p "already approved" (ygg-git-pr-merge--approve-description)))
  (transient--emergency-exit)
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "APPROVED" "REVIEW_REQUIRED" ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '(quit))
  (ygg-git-pr-merge--present ygg-git-pr-merge-tests--scope)
  (should-not (string-match-p "already approved" (ygg-git-pr-merge--approve-description)))
  (transient--emergency-exit)
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7 '(quit))
  (should (member "--approve" (cadddr (ygg-git-pr-merge-tests--open-menu)))))

(provide 'ygg-git-pr-merge-tests)
;;; ygg-git-pr-merge-tests.el ends here
