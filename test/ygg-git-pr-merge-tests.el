;;; ygg-git-pr-merge-tests.el --- merging a pull request from magit -*- lexical-binding: t; -*-

;;; Code:

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
(defvar ygg-git-pr-merge-tests--answers nil)
(defvar ygg-git-pr-merge-tests--refetched nil)
(defvar ygg-git-pr-merge-tests--defaults nil)
(defvar ygg-git-pr-merge-tests--after-start nil)

(defun ygg-git-pr-merge-tests--run (repo number &rest answers)
  "Drive the merge of NUMBER in REPO with ANSWERS, an alist of prompt prefix
to the reply; return once it says its last word."
  (ignore-errors (delete-file (expand-file-name "did-merge" ygg-git-pr-merge-tests--dir)))
  (setq ygg-git-pr-merge-tests--asked nil
        ygg-git-pr-merge-tests--messages nil
        ygg-git-pr-merge-tests--refetched nil
        ygg-git-pr-merge-tests--defaults nil
        ygg-git-pr-merge-tests--answers answers)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt choices _pred _req _init _hist def)
               (let ((hit (seq-find (lambda (a) (string-prefix-p (car a) prompt))
                                    ygg-git-pr-merge-tests--answers)))
                 (push (cons (car hit) choices) ygg-git-pr-merge-tests--asked)
                 (push (cons (car hit) def) ygg-git-pr-merge-tests--defaults)
                 (if (eq (cdr hit) 'quit) (signal 'quit nil) (cdr hit)))))
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
                        (not (seq-find (lambda (m) (string-match-p "\\`\\(Merged\\|Auto-merge\\|Queued\\|Merge of\\|Merge cancelled\\|Cannot\\|#[0-9]+ \\|The repo\\|The forge\\|Another\\)" m))
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
            (ygg-git-pr-merge--asking nil)
            (ygg-git-pr-merge-tests--after-start nil))
       (unwind-protect
           (progn
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
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "yes"))
  (should (equal (ygg-git-pr-merge-tests--merge-calls)
                 '("gh pr merge 5 --repo github.com/o/r --squash --match-head-commit abc1234567890def")))
  (should (equal (car (alist-get "Merge #" ygg-git-pr-merge-tests--asked nil nil #'equal))
                 "main"))
  (should (member "Merged #5 into main (squash); remote branch feat deleted"
                  ygg-git-pr-merge-tests--messages))
  (should (equal (ygg-git-pr-merge-tests--deletes)
                 '("gh api --hostname github.com -X DELETE repos/o/r/git/refs/heads/feat"))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-default-branch-retargets-first
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "develop") '("Merge method" . "merge") '("Delete" . "no"))
  (should (equal (ygg-git-pr-merge-tests--merge-calls)
                 '("gh pr edit 5 --repo github.com/o/r --base develop"
                   "gh pr merge 5 --repo github.com/o/r --merge --match-head-commit abc1234567890def")))
  (should (string-match-p "retargeted from main"
                          (cdr (assq 'confirm ygg-git-pr-merge-tests--asked)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-specific-branch-retargets-first
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "release") '("Merge method" . "squash") '("Delete" . "no"))
  (should (equal (seq-take (ygg-git-pr-merge-tests--merge-calls) 1)
                 '("gh pr edit 5 --repo github.com/o/r --base release")))
  (should (equal (cdr (assoc "Merge #" ygg-git-pr-merge-tests--asked))
                 '("main" "develop" "release"))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-offers-only-allowed-methods
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "yes"))
  (should (equal (cdr (assoc "Merge method" ygg-git-pr-merge-tests--asked))
                 '("squash" "merge"))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-delete-defaults-from-the-repository
  (let (default)
    (cl-letf (((symbol-function 'ygg-git-pr-merge--read-delete)
               (lambda (info) (setq default (plist-get info :delete)) nil)))
      (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                                   '("Merge #" . "main") '("Merge method" . "squash")))
    (should (eq default t))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-refuses-a-draft
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"isDraft\":false" "\"isDraft\":true"
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should-not ygg-git-pr-merge-tests--asked)
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
                                 '("Merge #" . "develop") '("Merge method" . "squash") '("Delete" . "no")))
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should-not ygg-git-pr-merge-tests--refetched)
  (should (member "Merge cancelled" ygg-git-pr-merge-tests--messages)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-confirm-summarises-the-merge
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "yes"))
  (let ((text (cdr (assq 'confirm ygg-git-pr-merge-tests--asked))))
    (dolist (part '("#5" "Add thing" "abc12345" "feat -> main" "method: squash" "delete remote branch: yes"
                    "auto-merge: no" "checks: passing" "review: approved" "mergeable: mergeable"))
      (should (string-match-p (regexp-quote part) text)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-failure-surfaces-the-forge-text
  (ygg-git-pr-merge-tests--put "failmerge" "X Pull request is not mergeable: ghp_abcDEF123 base branch policy prohibits the merge\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "no"))
  (let ((said (car ygg-git-pr-merge-tests--messages)))
    (should (string-match-p "base branch policy prohibits the merge" said))
    (should-not (string-match-p "ghp_" said)))
  (should-not ygg-git-pr-merge-tests--refetched))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-failed-retarget-stops-before-merge
  (ygg-git-pr-merge-tests--put "fail" "no such branch\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "release") '("Merge method" . "squash") '("Delete" . "no"))
  (should (equal (length (ygg-git-pr-merge-tests--merge-calls)) 1))
  (should (string-match-p "no such branch" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-success-refetches-the-requests
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "no"))
  (should (equal ygg-git-pr-merge-tests--refetched
                 (list (list ygg-git-pr-merge-tests--gh t)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-merges-and-retargets
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("Merge #" . "develop") '("Merge method" . "squash") '("Delete" . "yes"))
  (should (equal (ygg-git-pr-merge-tests--merge-calls)
                 '("glab mr update 7 -R gl.example/o/r --target-branch develop"
                   "glab mr merge 7 -R gl.example/o/r --squash --sha def4567890123abc --auto-merge=false --yes --remove-source-branch")))
  (should (string-match-p "checks: failing" (cdr (assq 'confirm ygg-git-pr-merge-tests--asked))))
  (should (equal (cdr (assoc "Merge method" ygg-git-pr-merge-tests--asked))
                 '("squash" "merge")))
  (should ygg-git-pr-merge-tests--refetched))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-plain-merge-has-no-method-flag
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("Merge #" . "main") '("Merge method" . "merge") '("Delete" . "no"))
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
                               '("Merge #" . "main") '("Merge method" . "merge") '("Delete" . "no"))
  (should (string-match-p "glab: \\|Method Not Allowed" (car ygg-git-pr-merge-tests--messages))))

(defun ygg-git-pr-merge-tests--answers (&rest extra)
  (append extra '(("Merge #" . "main") ("Merge method" . "squash") ("Delete" . "no"))))

(defun ygg-git-pr-merge-tests--summary ()
  (cdr (assq 'confirm ygg-git-pr-merge-tests--asked)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-every-write-names-its-repository
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "release") '("Merge method" . "squash") '("Delete" . "no"))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("Merge #" . "release") '("Merge method" . "squash") '("Delete" . "no"))
  (let ((calls (ygg-git-pr-merge-tests--merge-calls)))
    (should (= (length calls) 4))
    (dolist (call calls)
      (should (string-match-p " \\(--repo\\|-R\\) " call)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-pins-the-reviewed-head
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers))
  (should (string-match-p "--match-head-commit abc1234567890def"
                          (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "at abc12345\n" (ygg-git-pr-merge-tests--summary)))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("Merge #" . "main") '("Merge method" . "merge"))
  (should (string-match-p "--sha def4567890123abc" (cadr (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "at def45678\n" (ygg-git-pr-merge-tests--summary))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-refuses-without-a-head-commit
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"headRefOid\":\"[a-f0-9]+\"," ""
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5)
  (should-not (ygg-git-pr-merge-tests--merge-calls))
  (should (string-match-p "no head commit" (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-auto-merge-is-always-explicit
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
         (ygg-git-pr-merge-tests--answers '("Auto-merge" . "yes")))
  (should (string-match-p " --auto-merge=true " (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "auto-merge: yes" (ygg-git-pr-merge-tests--summary)))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
         (ygg-git-pr-merge-tests--answers '("Auto-merge" . "no")))
  (should (string-match-p " --auto-merge=false " (cadr (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "auto-merge: no" (ygg-git-pr-merge-tests--summary))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-github-auto-passes-auto
  (ygg-git-pr-merge-tests--put "post.json" "{\"state\":\"OPEN\",\"autoMergeRequest\":{\"enabledAt\":\"now\"}}")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash")
                               '("Auto-merge" . "yes") '("Delete" . "yes"))
  (should (string-match-p " --auto$" (car (ygg-git-pr-merge-tests--merge-calls))))
  (should (member "Auto-merge enabled for #5 (merges when the checks pass)"
                  ygg-git-pr-merge-tests--messages))
  (should-not (assoc "Delete" ygg-git-pr-merge-tests--asked))
  (should-not (ygg-git-pr-merge-tests--deletes))
  (should (string-match-p "delete-branch-on-merge setting" (ygg-git-pr-merge-tests--summary))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-github-merge-now-has-no-auto-flag
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("Auto-merge" . "no")))
  (should-not (string-match-p "--auto" (car (ygg-git-pr-merge-tests--merge-calls)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-auto-merge-defaults-follow-the-checks
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("Auto-merge" . "no")))
  (should (equal (cdr (assoc "Auto-merge" ygg-git-pr-merge-tests--defaults)) "no"))
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"conclusion\":\"SUCCESS\",\"status\":\"COMPLETED\""
                                                         "\"status\":\"IN_PROGRESS\"" ygg-git-pr-merge-tests--gh-pr))
  (apply #'ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
         (ygg-git-pr-merge-tests--answers '("Auto-merge" . "no")))
  (should (equal (cdr (assoc "Auto-merge" ygg-git-pr-merge-tests--defaults)) "yes"))
  (should (string-match-p "merging now will be queued or refused" (ygg-git-pr-merge-tests--summary))))

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
         (ygg-git-pr-merge-tests--answers '("Auto-merge" . "yes")))
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

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-guard-clears-after-quit
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5 '("Merge #" . quit))
  (should-not ygg-git-pr-merge--active)
  (should-not ygg-git-pr-merge--asking)
  (should-not (ygg-git-pr-merge-tests--merge-calls)))

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
  (should (string-match-p "^  state: blocked$" (ygg-git-pr-merge-tests--summary)))
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
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gl 7
                               '("Merge #" . "main") '("Merge method" . "squash"))
  (cdr (assoc "Merge method" ygg-git-pr-merge-tests--asked)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-ff-offers-only-rebase
  (should-not (ygg-git-pr-merge-tests--gl-methods "ff" "never"))
  (should (string-match-p "method: fast-forward" (ygg-git-pr-merge-tests--summary)))
  (should (string-match-p " --rebase " (car (ygg-git-pr-merge-tests--merge-calls))))
  (should-not (string-match-p "--squash" (car (ygg-git-pr-merge-tests--merge-calls)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-rebase-merge-is-semi-linear
  (should-not (ygg-git-pr-merge-tests--gl-methods "rebase_merge" "never"))
  (should (string-match-p "method: semi-linear merge" (ygg-git-pr-merge-tests--summary)))
  (should-not (string-match-p "--rebase\\|--squash" (car (ygg-git-pr-merge-tests--merge-calls)))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-gitlab-merge-method-and-squash-options
  (should-not (ygg-git-pr-merge-tests--gl-methods "merge" "never"))
  (should (string-match-p "method: merge" (ygg-git-pr-merge-tests--summary)))
  (should (equal (ygg-git-pr-merge-tests--gl-methods "merge" "default_off") '("squash" "merge")))
  (should-not (ygg-git-pr-merge-tests--gl-methods "merge" "always"))
  (should (string-match-p " --squash " (car (last (ygg-git-pr-merge-tests--merge-calls))))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-remote-branch-is-deleted-only-after-the-merge
  (ygg-git-pr-merge-tests--put "failmerge" "no\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "yes"))
  (should-not (ygg-git-pr-merge-tests--deletes))
  (should-not (seq-find (lambda (l) (string-match-p "--delete-branch" l)) (ygg-git-pr-merge-tests--log))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-remote-branch-delete-failure-is-reported
  (ygg-git-pr-merge-tests--put "failrm" "Reference does not exist\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "yes"))
  (should (string-match-p "\\`Merged #5.*deleting remote branch feat failed: .*Reference does not exist"
                          (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-cross-repository-branch-is-never-deleted
  (ygg-git-pr-merge-tests--put "prview.json"
                               (replace-regexp-in-string "\"isCrossRepository\":false" "\"isCrossRepository\":true"
                                                         ygg-git-pr-merge-tests--gh-pr))
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "main") '("Merge method" . "squash") '("Delete" . "yes"))
  (should-not (assoc "Delete" ygg-git-pr-merge-tests--asked))
  (should-not (ygg-git-pr-merge-tests--deletes)))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-merge-failure-after-retarget-says-so
  (ygg-git-pr-merge-tests--put "failmerge" "not mergeable\n")
  (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                               '("Merge #" . "release") '("Merge method" . "squash") '("Delete" . "no"))
  (should (= 2 (length (ygg-git-pr-merge-tests--merge-calls))))
  (should (string-match-p "failed after retargeting it to release (its base is now release): .*not mergeable"
                          (car ygg-git-pr-merge-tests--messages))))

(ygg-git-pr-merge-tests--deftest ygg-git-pr-merge-target-must-be-an-offered-branch
  (dolist (target '("" "nowhere" "feat" nil))
    (ygg-git-pr-merge-tests--run ygg-git-pr-merge-tests--gh 5
                                 (cons "Merge #" target) '("Merge method" . "squash"))
    (should-not (ygg-git-pr-merge-tests--merge-calls))
    (should-not (assq 'confirm ygg-git-pr-merge-tests--asked))
    (should (string-match-p "Merge cancelled" (car ygg-git-pr-merge-tests--messages)))
    (should-not ygg-git-pr-merge--active))
  (should (equal (cdr (assoc "Merge #" ygg-git-pr-merge-tests--asked)) '("main" "develop" "release"))))

(ert-deftest ygg-git-pr-merge-scrub-removes-forge-tokens ()
  (dolist (token '("ghp_abc123" "github_pat_AB12_cd" "glpat-abc_DEF-1" "gloas-abc123" "glptt-0123abcd"
                   "glcbt-64_abcDEF" "Bearer eyJhbGciOi.abc" "CI_JOB_TOKEN=abcd1234" "JOB-TOKEN: abcd1234"))
    (let ((said (ygg-git-pr-merge--scrub (format "bad credentials %s here" token))))
      (should (string-match-p "bad credentials <token> here" said)))))

(provide 'ygg-git-pr-merge-tests)
;;; ygg-git-pr-merge-tests.el ends here
