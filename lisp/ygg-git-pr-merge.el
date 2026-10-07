;;; ygg-git-pr-merge.el --- merge a pull or merge request from magit -*- lexical-binding: t; -*-

;;; Commentary:
;; Merge the request at point, from its row in the status section or from
;; its compare: the forge is asked in the background for the request and the
;; repository's settings, then the target, the method and the branch's fate
;; are chosen and one question confirms the lot.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'ygg-git-review-requests)

(declare-function ygg-git-compare--forge-repo "ygg-git-compare" (&optional remote))
(declare-function ygg-git-compare--known-pr "ygg-git-compare" (repo selector what))
(declare-function ygg-git-compare--pr-remote "ygg-git-compare" ())
(declare-function ygg-git-compare--b-branch "ygg-git-compare" ())
(declare-function ygg-git-compare-refresh "ygg-git-compare" ())
(defvar ygg-git-compare--b-spec)
(defvar ygg-git-compare-mode)
(defvar ygg-git-compare-mode-map)

(defun ygg-git-pr-merge--program (repo)
  (if (eq (car repo) 'github) "gh" "glab"))

(defun ygg-git-pr-merge--slug (repo)
  (format "%s/%s" (nth 1 repo) (nth 2 repo)))

(defun ygg-git-pr-merge--json (text)
  (json-parse-string text :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defconst ygg-git-pr-merge--token-regexp
  (concat "\\(?:gh[pousr]_[A-Za-z0-9]+\\|github_pat_[A-Za-z0-9_]+"
          "\\|\\(?:glpat\\|gloas\\|glptt\\|glcbt\\|glrt\\|gldt\\|glft\\|glimt\\|glagent\\|glsoat\\)-[A-Za-z0-9_.-]+"
          "\\|Bearer[ \t]+[^ \t\n]+"
          "\\|\\(?:PRIVATE-TOKEN\\|JOB-TOKEN\\|CI_JOB_TOKEN\\)[:=][ \t]*[^ \t\n]+\\)"))

(defvar ygg-git-pr-merge-retry-delay 2)
(defvar ygg-git-pr-merge--active nil)
(defvar ygg-git-pr-merge--asking nil)

(defun ygg-git-pr-merge--key (repo number)
  (format "%s#%s" (ygg-git-pr-merge--slug repo) number))

(defun ygg-git-pr-merge--clear (key)
  (setq ygg-git-pr-merge--active (delete key ygg-git-pr-merge--active)))

(defun ygg-git-pr-merge--scrub (text)
  (replace-regexp-in-string ygg-git-pr-merge--token-regexp "<token>" text t t))

(defun ygg-git-pr-merge--said (text)
  (let ((json (ignore-errors (ygg-git-pr-merge--json (or text "")))))
    (and (proper-list-p json)
         (cl-evenp (length json))
         (let ((m (or (plist-get json :message) (plist-get json :error))))
           (and (stringp m) m)))))

(defun ygg-git-pr-merge--failure (program status text stderr)
  "The forge's own words for why PROGRAM failed, tokens removed."
  (let ((said (or (ygg-git-pr-merge--said text) (ygg-git-pr-merge--said stderr)))
        (lines (split-string (or stderr "") "\n" t "[ \t\r]+")))
    (ygg-git-pr-merge--scrub
     (cond (said (format "%s: %s" program said))
           ((null status) (format "%s not found" program))
           (lines (format "%s: %s" program
                          (truncate-string-to-width (string-join lines " ") 300)))
           (t (format "%s failed (exit %s)" program status))))))

(defun ygg-git-pr-merge--run (program args callback)
  (ygg-git-review-requests--spawn (cons program args) callback))

(defun ygg-git-pr-merge--checks (rollup)
  (if (null rollup)
      'none
    (let (failing pending)
      (dolist (check rollup)
        (let ((conclusion (or (plist-get check :conclusion) (plist-get check :state)))
              (status (plist-get check :status)))
          (cond ((member conclusion '("FAILURE" "TIMED_OUT" "CANCELLED" "ACTION_REQUIRED"
                                      "STARTUP_FAILURE" "ERROR"))
                 (setq failing t))
                ((or (member conclusion '("PENDING" "EXPECTED"))
                     (and status (not (equal status "COMPLETED")) (null conclusion))
                     (and (null conclusion) (null status)))
                 (setq pending t)))))
      (cond (failing 'failing) (pending 'pending) (t 'passing)))))

(defun ygg-git-pr-merge--github-info (pr repo-json)
  (let ((mergeable (plist-get pr :mergeable))
        (status (plist-get pr :mergeStateStatus))
        (head (plist-get pr :headRefName))
        (base (plist-get pr :baseRefName))
        (default (plist-get (plist-get repo-json :defaultBranchRef) :name)))
    (list :number (plist-get pr :number)
          :title (plist-get pr :title)
          :sha (plist-get pr :headRefOid)
          :open (equal (plist-get pr :state) "OPEN")
          :draft (plist-get pr :isDraft)
          :conflicting (equal mergeable "CONFLICTING")
          :unknown (or (null mergeable) (equal mergeable "UNKNOWN"))
          :checks (ygg-git-pr-merge--checks (plist-get pr :statusCheckRollup))
          :review (let ((d (plist-get pr :reviewDecision)))
                    (if (or (null d) (string-empty-p d)) "none required" (downcase d)))
          :merge-state (downcase (or mergeable "unknown"))
          :state (and status (not (member status '("CLEAN" "UNKNOWN"))) (downcase status))
          :base base
          :head head
          :default default
          :methods (append (and (plist-get repo-json :squashMergeAllowed) '(squash))
                           (and (plist-get repo-json :mergeCommitAllowed) '(merge))
                           (and (plist-get repo-json :rebaseMergeAllowed) '(rebase)))
          :deletable (and (not (plist-get pr :isCrossRepository))
                          (not (member head (list base default))))
          :delete (and (plist-get repo-json :deleteBranchOnMerge) t))))

(defun ygg-git-pr-merge--gitlab-info (mr project)
  (let* ((pipeline (plist-get (plist-get mr :head_pipeline) :status))
         (method (plist-get project :merge_method))
         (squash (plist-get project :squash_option))
         (detail (plist-get mr :detailed_merge_status))
         (head (plist-get mr :source_branch))
         (base (plist-get mr :target_branch))
         (default (plist-get project :default_branch)))
    (list :number (plist-get mr :iid)
          :title (plist-get mr :title)
          :sha (plist-get mr :sha)
          :open (equal (plist-get mr :state) "opened")
          :draft (or (plist-get mr :draft) (plist-get mr :work_in_progress))
          :conflicting (and (plist-get mr :has_conflicts) t)
          :checks (cond ((null pipeline) 'none)
                        ((member pipeline '("success")) 'passing)
                        ((member pipeline '("failed" "canceled")) 'failing)
                        (t 'pending))
          :review (if (equal detail "not_approved") "approval required" "none required")
          :merge-state (or detail "unknown")
          :pipeline-required (and (plist-get project :only_allow_merge_if_pipeline_succeeds) t)
          :base base
          :head head
          :default default
          :methods (delq nil
                         (list (and (not (equal squash "never")) 'squash)
                               (and (not (equal squash "always"))
                                    (if (equal method "ff") 'rebase 'merge))))
          :labels (pcase method
                    ("ff" '((rebase . "fast-forward")))
                    ("rebase_merge" '((merge . "semi-linear merge"))))
          :deletable (not (member head (list base default)))
          :delete (and (plist-get project :remove_source_branch_after_merge) t))))

(defun ygg-git-pr-merge--fetch (repo number callback)
  "Ask the forge for request NUMBER and REPO's settings, then call CALLBACK
with the request's facts as a plist, or with nil and why."
  (let* ((github (eq (car repo) 'github))
         (program (ygg-git-pr-merge--program repo))
         (slug (ygg-git-pr-merge--slug repo))
         (host (nth 1 repo))
         (project (concat "projects/" (url-hexify-string (nth 2 repo))))
         (jobs (if github
                   (list (list "pr" "view" (number-to-string number) "--repo" slug
                               "--json" "number,title,state,isDraft,isCrossRepository,mergeable,mergeStateStatus,baseRefName,headRefName,headRefOid,reviewDecision,statusCheckRollup")
                         (list "repo" "view" slug "--json"
                               "defaultBranchRef,mergeCommitAllowed,squashMergeAllowed,rebaseMergeAllowed,deleteBranchOnMerge"))
                 (list (list "api" "--hostname" host
                             (format "%s/merge_requests/%d" project number))
                       (list "api" "--hostname" host project))))
         (answers (make-vector 2 nil))
         (left 2)
         (failed nil)
         (index 0))
    (dolist (job jobs)
      (let ((slot index))
        (ygg-git-pr-merge--run
         program job
         (lambda (status text stderr)
           (if (eql status 0)
               (aset answers slot text)
             (unless failed
               (setq failed (ygg-git-pr-merge--failure program status text stderr))))
           (when (zerop (cl-decf left))
             (if failed
                 (funcall callback nil failed)
               (condition-case nil
                   (let ((a (ygg-git-pr-merge--json (aref answers 0)))
                         (b (ygg-git-pr-merge--json (aref answers 1))))
                     (funcall callback
                              (if github
                                  (ygg-git-pr-merge--github-info a b)
                                (ygg-git-pr-merge--gitlab-info a b))
                              nil))
                 (error (funcall callback nil (format "%s answered nothing readable" program)))))))))
      (cl-incf index))))

(defun ygg-git-pr-merge--read (repo number retries callback)
  "Like `ygg-git-pr-merge--fetch', but re-read up to RETRIES times while
GitHub is still computing mergeability."
  (ygg-git-pr-merge--fetch
   repo number
   (lambda (info failure)
     (cond (failure (funcall callback nil failure))
           ((not (plist-get info :unknown)) (funcall callback info nil))
           ((> retries 0)
            (run-at-time ygg-git-pr-merge-retry-delay nil
                         #'ygg-git-pr-merge--read repo number (1- retries) callback))
           (t (funcall callback nil "GitHub is still computing mergeability; try again"))))))

(defun ygg-git-pr-merge--commands (repo info target method auto delete)
  "The commands that retarget when needed, then merge, as (PROGRAM . ARGS)."
  (let* ((number (number-to-string (plist-get info :number)))
         (slug (ygg-git-pr-merge--slug repo))
         (sha (plist-get info :sha))
         (retarget (not (equal target (plist-get info :base)))))
    (if (eq (car repo) 'github)
        (append
         (and retarget
              (list (list "gh" "pr" "edit" number "--repo" slug "--base" target)))
         (list (append (list "gh" "pr" "merge" number "--repo" slug
                             (format "--%s" method)
                             "--match-head-commit" sha)
                       (and auto '("--auto")))))
      (append
       (and retarget
            (list (list "glab" "mr" "update" number "-R" slug "--target-branch" target)))
       (list (append (list "glab" "mr" "merge" number "-R" slug)
                     (pcase method ('squash '("--squash")) ('rebase '("--rebase")))
                     (list "--sha" sha
                           (if auto "--auto-merge=true" "--auto-merge=false")
                           "--yes")
                     (and delete '("--remove-source-branch"))))))))

(defun ygg-git-pr-merge--delete-command (repo info)
  (list "gh" "api" "--hostname" (nth 1 repo) "-X" "DELETE"
        (format "repos/%s/git/refs/heads/%s" (nth 2 repo)
                (mapconcat #'url-hexify-string (split-string (plist-get info :head) "/") "/"))))

(defun ygg-git-pr-merge--run-all (commands callback)
  "Run COMMANDS in turn, stopping at the first failure; CALLBACK gets nil or
the failure's words, the last command's output and the command that failed."
  (if (null commands)
      (funcall callback nil "" nil)
    (ygg-git-pr-merge--run
     (caar commands) (cdar commands)
     (lambda (status text stderr)
       (cond ((not (eql status 0))
              (funcall callback (ygg-git-pr-merge--failure (caar commands) status text stderr)
                       "" (car commands)))
             ((cdr commands)
              (ygg-git-pr-merge--run-all (cdr commands) callback))
             (t (funcall callback nil (concat text "\n" stderr) nil)))))))

(defun ygg-git-pr-merge--outcome (repo info auto output callback)
  "Call CALLBACK with merged, auto or queued: the forge's state, else OUTPUT."
  (let* ((github (eq (car repo) 'github))
         (number (plist-get info :number))
         (program (ygg-git-pr-merge--program repo))
         (project (concat "projects/" (url-hexify-string (nth 2 repo))))
         (args (if github
                   (list "pr" "view" (number-to-string number) "--repo"
                         (ygg-git-pr-merge--slug repo) "--json" "state,autoMergeRequest")
                 (list "api" "--hostname" (nth 1 repo)
                       (format "%s/merge_requests/%d" project number)))))
    (ygg-git-pr-merge--run
     program args
     (lambda (status text _stderr)
       (let ((json (and (eql status 0) (ignore-errors (ygg-git-pr-merge--json text)))))
         (funcall callback
                  (cond ((and json github)
                         (cond ((equal (plist-get json :state) "MERGED") 'merged)
                               ((plist-get json :autoMergeRequest) 'auto)
                               (t 'queued)))
                        (json
                         (cond ((equal (plist-get json :state) "merged") 'merged)
                               ((plist-get json :merge_when_pipeline_succeeds) 'auto)
                               (t 'queued)))
                        ((string-match-p "queue" output) 'queued)
                        (auto 'auto)
                        (t 'merged))))))))

(defun ygg-git-pr-merge--refetch (repo origin)
  (let ((status-buffer (or (and (buffer-live-p origin)
                                (with-current-buffer origin (derived-mode-p 'magit-status-mode))
                                origin)
                           (and (buffer-live-p origin)
                                (with-current-buffer origin
                                  (ignore-errors (magit-get-mode-buffer 'magit-status-mode))))
                           origin)))
    (when (buffer-live-p status-buffer)
      (with-current-buffer status-buffer
        (ygg-git-review-requests--ensure repo t)
        (when (derived-mode-p 'magit-status-mode)
          (magit-refresh-buffer))))
    (when (and (buffer-live-p origin)
               (buffer-local-value 'ygg-git-compare-mode origin)
               (fboundp 'ygg-git-compare-refresh))
      (with-current-buffer origin
        (ignore-errors (ygg-git-compare-refresh))))))

(defun ygg-git-pr-merge--read-target (info remotes)
  (let* ((base (plist-get info :base))
         (default (plist-get info :default))
         (choices (delete (plist-get info :head)
                          (delete-dups (delq nil (append (list base default) remotes)))))
         (target (completing-read
                  (format "Merge #%s into (%s is its base, %s the default): "
                          (plist-get info :number) base default)
                  choices nil t nil nil base)))
    (and (stringp target) (member target choices) target)))

(defun ygg-git-pr-merge--method-label (info method)
  (or (alist-get method (plist-get info :labels)) (symbol-name method)))

(defun ygg-git-pr-merge--read-method (info)
  (let* ((allowed (plist-get info :methods))
         (labels (mapcar (lambda (m) (cons (ygg-git-pr-merge--method-label info m) m)) allowed)))
    (if (cdr labels)
        (let ((choice (completing-read "Merge method: " (mapcar #'car labels) nil t nil nil
                                       (ygg-git-pr-merge--method-label
                                        info (if (memq 'squash allowed) 'squash (car allowed))))))
          (cdr (assoc choice labels)))
      (cdar labels))))

(defun ygg-git-pr-merge--passes (repo)
  (if (eq (car repo) 'github) "the checks pass" "the pipeline passes"))

(defun ygg-git-pr-merge--read-auto (repo info)
  (let ((pending (eq (plist-get info :checks) 'pending)))
    (equal "yes" (completing-read
                  (format "Auto-merge #%s when %s instead of merging now (%s)? "
                          (plist-get info :number) (ygg-git-pr-merge--passes repo)
                          (if pending "default yes, they are still running" "default no, merge now"))
                  '("yes" "no") nil t nil nil (if pending "yes" "no")))))

(defun ygg-git-pr-merge--read-delete (info)
  (equal "yes" (completing-read (format "Delete remote branch %s after merging? " (plist-get info :head))
                                '("yes" "no") nil t nil nil
                                (if (plist-get info :delete) "yes" "no"))))

(defun ygg-git-pr-merge--short (sha)
  (substring sha 0 (min 8 (length sha))))

(defun ygg-git-pr-merge--summary (repo info target method auto delete)
  (let* ((github (eq (car repo) 'github))
         (checks (plist-get info :checks))
         (required (plist-get info :pipeline-required)))
    (concat
     (format "Merge %s #%s \"%s\" at %s\n  %s -> %s%s\n  method: %s, delete remote branch: %s\n  auto-merge: %s\n  checks: %s, review: %s, mergeable: %s\n"
             (if github "PR" "MR")
             (plist-get info :number) (plist-get info :title)
             (ygg-git-pr-merge--short (plist-get info :sha))
             (plist-get info :head) target
             (if (equal target (plist-get info :base)) ""
               (format " (retargeted from %s first)" (plist-get info :base)))
             (ygg-git-pr-merge--method-label info method)
             (cond (delete "yes")
                   ((and github auto) "no (the repository's delete-branch-on-merge setting decides)")
                   (t "no"))
             (if auto "yes" "no")
             checks (plist-get info :review) (plist-get info :merge-state))
     (and (plist-get info :state) (format "  state: %s\n" (plist-get info :state)))
     (and (not github) (format "  pipeline must succeed: %s\n" (if required "yes" "no")))
     (cond (auto nil)
           ((and required (not (eq checks 'passing)))
            "  merging now will be refused until the pipeline passes\n")
           ((and github (eq checks 'pending))
            "  merging now will be queued or refused until required checks pass\n"))
     "Proceed? ")))

(defun ygg-git-pr-merge--ask (repo info remotes)
  "Ask for the target, method, auto-merge and branch fate, then confirm;
return them as a plist, or nil after saying why not."
  (let* ((github (eq (car repo) 'github))
         (target (ygg-git-pr-merge--read-target info remotes))
         (method (and target (ygg-git-pr-merge--read-method info)))
         (auto (and method (ygg-git-pr-merge--read-auto repo info)))
         (delete (and method
                      (plist-get info :deletable)
                      (not (and github auto))
                      (ygg-git-pr-merge--read-delete info))))
    (cond ((null target) (message "Merge cancelled: pick one of the offered branches") nil)
          ((null method) (message "Merge cancelled: no merge method chosen") nil)
          ((not (y-or-n-p (ygg-git-pr-merge--summary repo info target method auto delete)))
           (message "Merge cancelled") nil)
          (t (list :target target :method method :auto auto :delete delete)))))

(defun ygg-git-pr-merge--report (repo info choice origin outcome note)
  (let ((number (plist-get info :number)))
    (message "%s"
             (pcase outcome
               ('merged (format "Merged #%s into %s (%s)%s" number (plist-get choice :target)
                                (ygg-git-pr-merge--method-label info (plist-get choice :method))
                                (or note "")))
               ('auto (format "Auto-merge enabled for #%s (merges when %s)" number
                              (ygg-git-pr-merge--passes repo)))
               (_ (format "Queued #%s" number))))
    (ygg-git-pr-merge--refetch repo origin)))

(defun ygg-git-pr-merge--execute (repo info choice origin key)
  (let* ((target (plist-get choice :target))
         (number (plist-get info :number))
         (retargeting (not (equal target (plist-get info :base))))
         (github (eq (car repo) 'github)))
    (ygg-git-pr-merge--run-all
     (ygg-git-pr-merge--commands repo info target (plist-get choice :method)
                                 (plist-get choice :auto) (plist-get choice :delete))
     (lambda (failure output failed)
       (if failure
           (progn
             (ygg-git-pr-merge--clear key)
             (if (and retargeting (equal (nth 2 failed) "merge"))
                 (message "Merge of #%s failed after retargeting it to %s (its base is now %s): %s"
                          number target target failure)
               (message "Merge of #%s failed: %s" number failure)))
         (ygg-git-pr-merge--outcome
          repo info (plist-get choice :auto) output
          (lambda (outcome)
            (if (and (eq outcome 'merged) github (plist-get choice :delete))
                (ygg-git-pr-merge--run-all
                 (list (ygg-git-pr-merge--delete-command repo info))
                 (lambda (delete-failure _output _failed)
                   (ygg-git-pr-merge--clear key)
                   (ygg-git-pr-merge--report
                    repo info choice origin outcome
                    (if delete-failure
                        (format "; deleting remote branch %s failed: %s"
                                (plist-get info :head) delete-failure)
                      (format "; remote branch %s deleted" (plist-get info :head))))))
              (ygg-git-pr-merge--clear key)
              (ygg-git-pr-merge--report repo info choice origin outcome nil)))))))))

(defun ygg-git-pr-merge--decide (repo info origin remotes key)
  (let ((handed nil)
        (number (plist-get info :number)))
    (unwind-protect
        (cond ((not (plist-get info :open))
               (message "#%s is not open" number))
              ((plist-get info :draft)
               (message "#%s is a draft; mark it ready on the forge first" number))
              ((plist-get info :conflicting)
               (message "#%s has conflicts with its base; resolve them first" number))
              ((null (plist-get info :methods))
               (message "The repository allows no merge method"))
              ((not (plist-get info :sha))
               (message "The forge gave no head commit for #%s" number))
              (ygg-git-pr-merge--asking
               (message "Another merge question is open; answer it first"))
              (t
               (let ((choice (let ((ygg-git-pr-merge--asking t))
                               (ygg-git-pr-merge--ask repo info remotes))))
                 (when choice
                   (setq handed t)
                   (ygg-git-pr-merge--execute repo info choice origin key)))))
      (unless handed
        (ygg-git-pr-merge--clear key)))))

(defun ygg-git-pr-merge--remote-branches ()
  (ignore-errors
    (let ((remote (and (fboundp 'ygg-git-compare--pr-remote) (ygg-git-compare--pr-remote))))
      (when remote
        (delq nil
              (mapcar (lambda (name)
                        (and (string-prefix-p (concat remote "/") name)
                             (not (string-suffix-p "/HEAD" name))
                             (substring name (1+ (length remote)))))
                      (magit-list-remote-branch-names)))))))

(defun ygg-git-pr-merge--start (repo number)
  (unless (integerp number)
    (user-error "No number for this pull request"))
  (let ((key (ygg-git-pr-merge--key repo number))
        (origin (current-buffer))
        (remotes (ygg-git-pr-merge--remote-branches))
        (dir default-directory))
    (when (member key ygg-git-pr-merge--active)
      (user-error "A merge of #%s is already in progress" number))
    (push key ygg-git-pr-merge--active)
    (message "Reading #%s…" number)
    (ygg-git-pr-merge--read
     repo number 3
     (lambda (info failure)
       (if failure
           (progn
             (ygg-git-pr-merge--clear key)
             (message "Cannot read #%s: %s" number failure))
         (run-at-time 0 nil
                      (lambda ()
                        (let ((default-directory dir))
                          (ygg-git-pr-merge--decide repo info origin remotes key)))))))))

(defun ygg-git-pr-merge--context ()
  "The request here as (REPO . NUMBER), from a status row or a compare."
  (let ((found (ygg-git-pr-merge--locate)))
    (unless (integerp (cdr found))
      (user-error "No pull request number here"))
    found))

(defun ygg-git-pr-merge--locate ()
  (cond ((magit-section-value-if 'review-request)
         (cons (or (ygg-git-review-requests--repo)
                   (user-error "Not a forge remote"))
               (plist-get (ygg-git-review-requests--at-point) :number)))
        ((bound-and-true-p ygg-git-compare-mode)
         (require 'ygg-git-compare)
         (let* ((spec (and (eq (car-safe ygg-git-compare--b-spec) 'pr)
                           (cdr ygg-git-compare--b-spec)))
                (repo (ygg-git-compare--forge-repo (and spec (plist-get spec :remote))))
                (selector (if spec
                              (plist-get spec :number)
                            (or (ygg-git-compare--b-branch)
                                (user-error "B is not a branch, so it has no pull request")))))
           (cons repo (if (integerp selector)
                          selector
                        (plist-get (ygg-git-compare--known-pr repo selector selector) :number)))))
        (t (user-error "No pull request at point"))))

;;;###autoload
(defun ygg-git-pr-merge ()
  "Merge the pull or merge request at point, after one confirmation."
  (interactive)
  (pcase-let ((`(,repo . ,number) (ygg-git-pr-merge--context)))
    (ygg-git-pr-merge--start repo number)))

(keymap-set magit-review-request-section-map "m" #'ygg-git-pr-merge)

(with-eval-after-load 'ygg-git-compare
  (keymap-set ygg-git-compare-mode-map "P" #'ygg-git-pr-merge))

(provide 'ygg-git-pr-merge)
;;; ygg-git-pr-merge.el ends here
