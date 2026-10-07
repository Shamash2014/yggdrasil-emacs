;;; ygg-git-stack.el --- stacked pull requests in magit status -*- lexical-binding: t; -*-

;;; Commentary:
;; The branch you are on, with the branches under and over it that each
;; build on the last, listed bottom to top in magit status.  A branch's
;; parent is `branch.X.ygg-parent' in git config, else its pull request's
;; base.  One key rebases the stack where a parent has moved and pushes the
;; branches that changed, with a lease, after one question.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'ygg-git-review-requests)
(require 'ygg-git-pr-merge)

(declare-function ygg-git-compare--remote "ygg-git-compare" ())
(declare-function ygg-git-compare--default-branch "ygg-git-compare" ())

(defgroup ygg-git-stack nil
  "Stacked pull requests in magit status."
  :group 'magit)

(defcustom ygg-git-stack t
  "Whether magit status lists the current branch's stack."
  :type 'boolean)

(defvar ygg-git-stack--pending nil)
(defvar ygg-git-stack--seen (make-hash-table :test 'equal))
(defvar ygg-git-stack--asked nil)
(defvar ygg-git-stack--checking nil)
(defvar ygg-git-stack--merged (make-hash-table :test 'equal))
(defvar ygg-git-stack--snap nil)
(defvar ygg-git-stack--info-cache (make-hash-table :test 'equal))

(defconst ygg-git-stack--info-ttl 30)
(defconst ygg-git-stack--pending-ttl 86400)

(defun ygg-git-stack--compute-info ()
  (require 'ygg-git-compare)
  (let* ((remote (ygg-git-compare--remote))
         (name (ygg-git-compare--default-branch))
         (trunk (if (and remote (string-prefix-p (concat remote "/") name))
                    (substring name (1+ (length remote)))
                  name)))
    (list :remote remote :trunk trunk
          :protected (delq nil (list trunk (magit-main-branch)))
          :repo (ignore-errors (ygg-git-review-requests--repo)))))

(defun ygg-git-stack--info (cached)
  (let* ((key (magit-toplevel))
         (hit (and cached (gethash key ygg-git-stack--info-cache))))
    (if (and hit (< (- (float-time) (car hit)) ygg-git-stack--info-ttl))
        (cdr hit)
      (let ((info (ygg-git-stack--compute-info)))
        (puthash key (cons (float-time) info) ygg-git-stack--info-cache)
        info))))

(defun ygg-git-stack--read-refs ()
  (let ((refs (make-hash-table :test 'equal)) locals)
    (dolist (line (magit-git-lines "for-each-ref" "--format=%(refname)\t%(objectname)"
                                   "refs/heads" "refs/remotes"))
      (when (string-match "\\`\\(.+\\)\t\\(.+\\)\\'" line)
        (puthash (match-string 1 line) (match-string 2 line) refs)
        (when (string-prefix-p "refs/heads/" (match-string 1 line))
          (push (substring (match-string 1 line) 11) locals))))
    (list :refs refs :locals (nreverse locals))))

(defmacro ygg-git-stack--with-snapshot (cached &rest body)
  (declare (indent 1))
  `(let ((ygg-git-stack--snap (append (ygg-git-stack--info ,cached) (ygg-git-stack--read-refs))))
     ,@body))

(defun ygg-git-stack--info-get (key)
  (plist-get (or ygg-git-stack--snap (ygg-git-stack--compute-info)) key))

(defun ygg-git-stack--ref-sha (refname)
  (if-let* ((refs (plist-get ygg-git-stack--snap :refs)))
      (gethash refname refs)
    (magit-rev-verify refname)))

(defun ygg-git-stack--remote ()
  (ygg-git-stack--info-get :remote))

(defun ygg-git-stack--trunk ()
  (ygg-git-stack--info-get :trunk))

(defun ygg-git-stack--protected ()
  (ygg-git-stack--info-get :protected))

(defun ygg-git-stack--prs ()
  "The open requests last cached for this repository, without asking again."
  (when-let* ((repo (if ygg-git-stack--snap
                        (plist-get ygg-git-stack--snap :repo)
                      (ygg-git-review-requests--repo))))
    (ygg-git-review-requests--load)
    (plist-get (cdr (gethash repo ygg-git-review-requests--cache)) :rows)))

(defun ygg-git-stack--pr (branch prs)
  (seq-find (lambda (row) (equal (plist-get row :head) branch)) prs))

(defun ygg-git-stack--config-parent (branch)
  (let ((parent (magit-get "branch" branch "ygg-parent")))
    (and parent (not (string-empty-p parent)) parent)))

(defun ygg-git-stack--configured ()
  (let (configured)
    (dolist (line (magit-git-lines "config" "--get-regexp" "^branch\\..*\\.ygg-parent$"))
      (when (string-match "\\`branch\\.\\(.*\\)\\.ygg-parent \\(.+\\)\\'" line)
        (push (cons (match-string 1 line) (match-string 2 line)) configured)))
    configured))

(defun ygg-git-stack--parent (branch &optional prs)
  "BRANCH's parent: its config first, else the base of its request."
  (or (ygg-git-stack--config-parent branch)
      (plist-get (ygg-git-stack--pr branch (or prs (ygg-git-stack--prs))) :base)))

(defun ygg-git-stack--parents (locals prs &optional configured)
  "An alist of each of LOCALS and its parent, config before request base."
  (let ((configured (or configured (ygg-git-stack--configured))))
    (mapcar (lambda (branch)
              (cons branch (or (cdr (assoc branch configured))
                               (plist-get (ygg-git-stack--pr branch prs) :base))))
            locals)))

(defun ygg-git-stack--linked-p (branch configured prs protected)
  "Whether BRANCH has a neighbour in CONFIGURED or PRS that is not PROTECTED."
  (seq-some (lambda (edge)
              (let ((other (cond ((equal (car edge) branch) (cdr edge))
                                 ((equal (cdr edge) branch) (car edge)))))
                (and other (not (member other protected)))))
            (append configured
                    (mapcar (lambda (row) (cons (plist-get row :head) (plist-get row :base)))
                            prs))))

(defun ygg-git-stack--children (branch parents)
  (sort (mapcar #'car (seq-filter (lambda (cell) (equal (cdr cell) branch)) parents))
        #'string<))

(defun ygg-git-stack--chain (branch parents)
  "The local branches of BRANCH's stack, bottom first.
The path down to BRANCH is kept; above it a fork follows its first child."
  (let ((protected (ygg-git-stack--protected))
        (chain (list branch)))
    (unless (member branch protected)
      (let ((up (cdr (assoc branch parents))))
        (while (and up (assoc up parents) (not (member up protected)) (not (member up chain)))
          (push up chain)
          (setq up (cdr (assoc up parents)))))
      (let ((top (car (last chain))))
        (while-let ((child (seq-find (lambda (c) (not (or (member c chain) (member c protected))))
                                     (ygg-git-stack--children top parents))))
          (setq chain (append chain (list child))
                top child)))
      chain)))

(defun ygg-git-stack--ref (name)
  "The ref to compare with or rebase onto for parent NAME, or nil."
  (let ((remote (ygg-git-stack--remote)))
    (cond ((and (member name (ygg-git-stack--protected)) remote
                (ygg-git-stack--ref-sha (format "refs/remotes/%s/%s" remote name)))
           (concat remote "/" name))
          ((ygg-git-stack--ref-sha (concat "refs/heads/" name)) name)
          ((and remote (ygg-git-stack--ref-sha (format "refs/remotes/%s/%s" remote name)))
           (concat remote "/" name)))))

(defun ygg-git-stack--tip (ref)
  (ygg-git-stack--ref-sha (concat "refs/heads/" ref)))

(defun ygg-git-stack--counts (parent-ref branch)
  "(AHEAD . BEHIND) of BRANCH against PARENT-REF."
  (when-let* ((line (and parent-ref
                         (magit-git-string "rev-list" "--left-right" "--count"
                                           (concat parent-ref "..." branch))))
              ((string-match "\\`\\([0-9]+\\)[ \t]+\\([0-9]+\\)" line)))
    (cons (string-to-number (match-string 2 line))
          (string-to-number (match-string 1 line)))))

(defun ygg-git-stack--fork-point (onto branch)
  (or (magit-git-string "merge-base" "--fork-point" onto branch)
      (magit-git-string "merge-base" onto branch)))

(defun ygg-git-stack--plan (chain parents)
  "What restacking CHAIN rebases: :branches from the first one that no longer
contains its parent, :onto that parent, :old-base the commit it parted at, :top."
  (let ((index 0) found)
    (while (and (not found) (< index (length chain)))
      (let* ((branch (nth index chain))
             (parent (if (zerop index)
                         (or (cdr (assoc branch parents)) (ygg-git-stack--trunk))
                       (nth (1- index) chain)))
             (onto (ygg-git-stack--ref parent)))
        (if (and onto (not (magit-git-success "merge-base" "--is-ancestor" onto branch)))
            (setq found (list :branches (nthcdr index chain)
                              :stack chain
                              :onto onto
                              :old-base (ygg-git-stack--fork-point onto branch)
                              :top (car (last chain))))
          (cl-incf index))))
    found))

(defun ygg-git-stack--restack-plan (branch parents)
  "The plan for BRANCH's stack, leaving out its bottom branches already merged
and parting the rest at the merged branch's old tip."
  (let* ((chain (ygg-git-stack--chain branch parents))
         (root (magit-toplevel))
         (gone (seq-take-while (lambda (name) (gethash (list root name) ygg-git-stack--merged))
                               chain)))
    (if (null gone)
        (ygg-git-stack--plan chain parents)
      (let* ((record (gethash (list root (car (last gone))) ygg-git-stack--merged))
             (rest (nthcdr (length gone) chain))
             (target (plist-get record :target))
             (plan (and rest (ygg-git-stack--plan
                              rest (cons (cons (car rest) target) parents)))))
        (when plan
          (when (and (equal (car (plist-get plan :branches)) (car rest)) (plist-get record :tip))
            (setq plan (plist-put plan :old-base (plist-get record :tip))))
          (plist-put plan :reparent (cons target (car rest))))))))

(defun ygg-git-stack--push-remote (branch)
  (or (magit-get-push-remote branch) (magit-get-remote branch) (ygg-git-stack--remote)))

(defun ygg-git-stack--diverged-p (new expected)
  (not (or (magit-git-success "merge-base" "--is-ancestor" expected new)
           (magit-git-success "merge-base" "--is-ancestor" new expected))))

(defun ygg-git-stack--had-locally-p (branch sha)
  (member sha (magit-git-lines "reflog" "show" "--format=%H" (concat "refs/heads/" branch))))

(defun ygg-git-stack--pushes (branches before &optional stack)
  "(PUSHES . REFUSED): PUSHES the (BRANCH REMOTE EXPECTED NEW AMENDED) of the
branches to push, those of BRANCHES whose tip moved since BEFORE and those of
STACK rewritten on their own, that the remote already has and the local branch
once pointed at; REFUSED the (BRANCH . REMOTE) of those the remote has moved on."
  (let ((protected (ygg-git-stack--protected)) pushes refused)
    (dolist (branch (delete-dups (append stack branches)))
      (let* ((new (ygg-git-stack--tip branch))
             (remote (ygg-git-stack--push-remote branch))
             (expected (and remote (ygg-git-stack--ref-sha
                                    (format "refs/remotes/%s/%s" remote branch))))
             (rebased (assoc branch before))
             (amended (and (not rebased) new expected (ygg-git-stack--diverged-p new expected))))
        (when (and new expected (not (member branch protected))
                   (not (equal new expected))
                   (or amended (and rebased (not (equal new (cdr rebased))))))
          (if (ygg-git-stack--had-locally-p branch expected)
              (push (list branch remote expected new amended) pushes)
            (push (cons branch remote) refused)))))
    (cons (nreverse pushes) (nreverse refused))))

(defun ygg-git-stack--refused-note (refused)
  (mapconcat (lambda (entry)
               (format "\nnot pushed: %s/%s has commits you never had locally - pull or rebase first"
                       (cdr entry) (car entry)))
             refused ""))

(defun ygg-git-stack--short (sha)
  (substring sha 0 (min 8 (length sha))))

(defun ygg-git-stack--confirm (pushes &optional note)
  (y-or-n-p
   (concat "Force-push with lease:\n"
           (mapconcat (lambda (entry)
                        (pcase-let ((`(,branch ,remote ,expected ,new ,amended) entry))
                          (format "  %s/%s  %s -> %s%s" remote branch
                                  (ygg-git-stack--short expected) (ygg-git-stack--short new)
                                  (if amended " (amended)" ""))))
                      pushes "\n")
           note
           "\nProceed? ")))

(defun ygg-git-stack--push-command (remote pushes)
  (append (list "push" remote)
          (mapcar (lambda (entry)
                    (format "--force-with-lease=refs/heads/%s:%s" (nth 0 entry) (nth 2 entry)))
                  pushes)
          (mapcar (lambda (entry)
                    (format "refs/heads/%s:refs/heads/%s" (nth 0 entry) (nth 0 entry)))
                  pushes)))

(defun ygg-git-stack--push (root pushes)
  (let ((remotes (delete-dups (mapcar #'cadr pushes))))
    (ygg-git-stack--push-next root remotes pushes)))

(defun ygg-git-stack--push-next (root remotes pushes)
  (if (null remotes)
      (message "Pushed %d branch%s" (length pushes) (if (cdr pushes) "es" ""))
    (let ((default-directory root)
          (mine (seq-filter (lambda (entry) (equal (cadr entry) (car remotes))) pushes)))
      (ygg-git-review-requests--spawn
       (cons "git" (ygg-git-stack--push-command (car remotes) mine))
       (lambda (status _text stderr)
         (if (eql status 0)
             (ygg-git-stack--push-next root (cdr remotes) pushes)
           (message "Push to %s failed: %s" (car remotes)
                    (string-trim (or stderr "")))))))))

(defun ygg-git-stack--entry (root)
  (cdr (assoc root ygg-git-stack--pending)))

(defun ygg-git-stack--forget (root)
  (setq ygg-git-stack--pending (assoc-delete-all root ygg-git-stack--pending)))

(defun ygg-git-stack--switch-back (orig)
  (when (and orig (magit-branch-p orig) (not (equal (magit-get-current-branch) orig)))
    (magit-git-success "switch" orig)))

(defun ygg-git-stack--after-rebase (root)
  "Once the rebase begun for ROOT is over, return to the branch left and offer
to push what moved."
  (when-let* ((pending (ygg-git-stack--entry root))
              (default-directory root)
              ((not (magit-rebase-in-progress-p))))
    (ygg-git-stack--forget root)
    (let* ((branches (plist-get pending :branches))
           (top (car (last branches))))
      (cond
       ((>= (- (float-time) (plist-get pending :started)) ygg-git-stack--pending-ttl)
        (message "Restack finished after its 24h window; nothing pushed"))
       ((not (equal (magit-get-current-branch) top))
        (message "Restack ended away from %s; nothing pushed" top))
       (t
        (ygg-git-stack--switch-back (plist-get pending :orig))
        (pcase-let ((`(,moved . ,result)
                     (ygg-git-stack--with-snapshot nil
                       (let ((before (plist-get pending :before)))
                         (when-let* ((moved (seq-some
                                             (lambda (cell)
                                               (not (equal (ygg-git-stack--tip (car cell)) (cdr cell))))
                                             before)))
                           (cons moved (ygg-git-stack--pushes branches before
                                                              (plist-get pending :stack))))))))
          (let ((pushes (car result))
                (note (ygg-git-stack--refused-note (cdr result))))
            (cond ((not moved)
                   (message (if (eq (plist-get pending :state) 'stopped)
                                "Restack aborted"
                              "Restack made no changes")))
                  ((null pushes) (message "Restacked; nothing to push%s" note))
                  ((ygg-git-stack--confirm pushes note) (ygg-git-stack--push root pushes))
                  (t (message "Restacked; nothing pushed%s" note))))))))))

(defun ygg-git-stack--resume ()
  (when-let* ((root (ignore-errors (magit-toplevel)))
              ((eq (plist-get (ygg-git-stack--entry root) :state) 'stopped))
              ((not (magit-rebase-in-progress-p))))
    (run-at-time 0 nil #'ygg-git-stack--after-rebase root)))

(add-hook 'magit-post-refresh-hook #'ygg-git-stack--resume)

(defun ygg-git-stack--norm (dir)
  (directory-file-name (file-truename (expand-file-name dir))))

(defun ygg-git-stack--refuse-elsewhere (root branches)
  "Signal when any of BRANCHES is checked out in a worktree other than ROOT's."
  (let ((here (ygg-git-stack--norm root)) found)
    (dolist (worktree (magit-list-worktrees))
      (let ((branch (nth 2 worktree)))
        (when (and branch (member branch branches)
                   (not (equal (ygg-git-stack--norm (car worktree)) here)))
          (push (format "%s in %s" branch (car worktree)) found))))
    (when found
      (user-error "Restack would rewrite a branch checked out in another worktree: %s"
                  (string-join (nreverse found) ", ")))))

(defun ygg-git-stack--run (root orig plan)
  "Rebase PLAN in ROOT in the background; a conflict hands over to magit."
  (let* ((default-directory root)
         (process-environment (append (list "GIT_EDITOR=true" "GIT_SEQUENCE_EDITOR=true")
                                      process-environment))
         (branches (plist-get plan :branches))
         (top (plist-get plan :top))
         (token (list 'restack)))
    (ygg-git-stack--refuse-elsewhere root branches)
    (when-let* ((reparent (plist-get plan :reparent)))
      (magit-set (car reparent) "branch" (cdr reparent) "ygg-parent"))
    (setq ygg-git-stack--pending
          (cons (cons root (list :token token :state 'running :started (float-time)
                                 :orig orig :branches branches :stack (plist-get plan :stack)
                                 :before (mapcar (lambda (b) (cons b (ygg-git-stack--tip b)))
                                                 branches)))
                (assoc-delete-all root ygg-git-stack--pending)))
    (unless (or (equal (magit-get-current-branch) top)
                (magit-git-success "switch" top))
      (ygg-git-stack--forget root)
      (user-error "Could not switch to %s" top))
    (message "Restacking %s onto %s…" (string-join branches ", ") (plist-get plan :onto))
    (ygg-git-review-requests--spawn
     (list "git" "rebase" "--update-refs" "--onto" (plist-get plan :onto)
           (plist-get plan :old-base) top)
     (lambda (status _text stderr)
       (let ((default-directory root))
         (when (eq token (plist-get (ygg-git-stack--entry root) :token))
           (cond ((eql status 0) (ygg-git-stack--after-rebase root))
                 ((magit-rebase-in-progress-p)
                  (setcdr (assoc root ygg-git-stack--pending)
                          (plist-put (ygg-git-stack--entry root) :state 'stopped))
                  (message "Restack stopped on a conflict; finish the rebase in magit and the push follows")
                  (magit-status-setup-buffer root))
                 (t (ygg-git-stack--forget root)
                    (unwind-protect
                        (progn
                          (message "Restack failed: %s" (string-trim (or stderr "")))
                          (ignore-errors (magit-git-success "rebase" "--abort")))
                      (ignore-errors (ygg-git-stack--switch-back orig)))))))))))

(defun ygg-git-stack--dirty-p ()
  (magit-git-lines "status" "--porcelain" "--untracked-files=no"))

(defun ygg-git-stack--ready ()
  (cond ((ygg-git-stack--dirty-p)
         (user-error "Worktree has uncommitted changes; commit or stash them before restacking"))
        ((magit-rebase-in-progress-p)
         (user-error "A rebase is already in progress"))
        ((not (magit-get-current-branch))
         (user-error "Not on a branch"))))

;;;###autoload
(defun ygg-git-stack-restack ()
  "Rebase the stack onto its moved parents, then offer to push what changed."
  (interactive)
  (let ((root (magit-toplevel)))
    (ygg-git-stack--ready)
    (let* ((branch (magit-get-current-branch))
           (plan (ygg-git-stack--with-snapshot nil
                   (ygg-git-stack--restack-plan
                    branch (ygg-git-stack--parents (plist-get ygg-git-stack--snap :locals)
                                                   (ygg-git-stack--prs))))))
      (if plan
          (ygg-git-stack--run root branch plan)
        (message "Stack is already in order")))))

;;;###autoload
(defun ygg-git-stack-branch (name)
  "Create and check out NAME on top of the current branch, recording its parent."
  (interactive (list (read-string "Branch on top of the current one: ")))
  (let ((parent (or (magit-get-current-branch) (user-error "Not on a branch"))))
    (when (string-empty-p name)
      (user-error "No branch name"))
    (when (magit-local-branch-p name)
      (user-error "Branch %s already exists" name))
    (when (zerop (magit-call-git "checkout" "-b" name parent))
      (magit-set parent "branch" name "ygg-parent"))
    (magit-refresh)))

(defun ygg-git-stack--retarget-then (repo number target next)
  (if (null number)
      (funcall next)
    (ygg-git-pr-merge--run-all
     (list (ygg-git-pr-merge--retarget-command repo number target))
     (lambda (failure _output _failed)
       (if failure
           (message "Retarget of #%s failed: %s" number failure)
         (funcall next))))))

(defun ygg-git-stack--restack-after-merge (root head child chain target)
  (let* ((default-directory root)
         (remote (ygg-git-stack--remote))
         (old-base (or (ygg-git-stack--tip head)
                       (magit-rev-verify (format "refs/remotes/%s/%s" remote head)))))
    (magit-set target "branch" child "ygg-parent")
    (ygg-git-review-requests--spawn
     (list "git" "fetch" remote)
     (lambda (_status _text _stderr)
       (let ((default-directory root))
         (condition-case failure
             (progn
               (ygg-git-stack--ready)
               (let* ((onto (ygg-git-stack--with-snapshot nil
                              (or (ygg-git-stack--ref target) target)))
                      (plan (list :branches chain :stack chain :onto onto
                                  :old-base (or old-base (ygg-git-stack--fork-point onto child))
                                  :top (car (last chain)))))
                 (ygg-git-stack--run root (magit-get-current-branch) plan)))
           (user-error (message "%s" (cadr failure)))))))))

(defun ygg-git-stack--after-merge (repo head target)
  "Ask once whether to retarget the request above HEAD to TARGET and restack."
  (let* ((root (magit-toplevel))
         (key (list root head)))
    (pcase-let ((`(,prs ,child ,chain)
                 (ygg-git-stack--with-snapshot nil
                   (let* ((prs (ygg-git-stack--prs))
                          (parents (ygg-git-stack--parents (plist-get ygg-git-stack--snap :locals) prs))
                          (child (car (ygg-git-stack--children head parents))))
                     (puthash key (list :target target
                                        :tip (or (ygg-git-stack--tip head)
                                                 (ygg-git-stack--ref-sha
                                                  (format "refs/remotes/%s/%s"
                                                          (ygg-git-stack--remote) head))))
                              ygg-git-stack--merged)
                     (list prs child (and child (member child (ygg-git-stack--chain child parents))))))))
      (when (and child (not (member key ygg-git-stack--asked)))
        (push key ygg-git-stack--asked)
        (let ((number (plist-get (ygg-git-stack--pr child prs) :number)))
          (when (y-or-n-p (if number
                              (format "Retarget #%s to %s and restack? " number target)
                            (format "Restack %s onto %s? " child target)))
            (ygg-git-stack--retarget-then
             repo number target
             (lambda () (ygg-git-stack--restack-after-merge root head child chain target)))))))))

(defun ygg-git-stack--on-merged (repo info choice origin)
  (let ((head (plist-get info :head))
        (default-directory (if (buffer-live-p origin)
                               (buffer-local-value 'default-directory origin)
                             default-directory)))
    (when (and head (magit-toplevel))
      (run-at-time 0 nil
                   (let ((dir default-directory))
                     (lambda ()
                       (let ((default-directory dir))
                         (ygg-git-stack--after-merge repo head (plist-get choice :target)))))))))

(add-hook 'ygg-git-pr-merge-merged-hook #'ygg-git-stack--on-merged)

(defun ygg-git-stack--state (repo answer branch row)
  "Remember BRANCH's open request, and when it has vanished ask the forge
whether it was merged."
  (let ((key (list (magit-toplevel) branch)))
    (cond (row (puthash key (cons (plist-get row :number) (plist-get row :base)) ygg-git-stack--seen))
          ((and answer (not (plist-get answer :error)) (gethash key ygg-git-stack--seen)
                (not (member key ygg-git-stack--checking)))
           (push key ygg-git-stack--checking)
           (ygg-git-stack--verify-merged repo branch key (gethash key ygg-git-stack--seen))))))

(defun ygg-git-stack--verify-merged (repo branch key seen)
  (let* ((github (eq (car repo) 'github))
         (dir default-directory)
         (number (car seen))
         (target (or (cdr seen) (ygg-git-stack--trunk))))
    (ygg-git-pr-merge--run
     (ygg-git-pr-merge--program repo)
     (if github
         (list "pr" "view" (number-to-string number) "--repo" (ygg-git-pr-merge--slug repo)
               "--json" "state")
       (list "api" "--hostname" (nth 1 repo)
             (format "projects/%s/merge_requests/%d" (url-hexify-string (nth 2 repo)) number)))
     (lambda (status text _stderr)
       (let ((state (and (eql status 0)
                         (plist-get (ignore-errors (ygg-git-pr-merge--json text)) :state))))
         (setq ygg-git-stack--checking (delete key ygg-git-stack--checking))
         (if (member state '("MERGED" "merged"))
             (run-at-time 0 nil
                          (lambda ()
                            (let ((default-directory dir))
                              (ygg-git-stack--after-merge repo branch target))))
           (remhash key ygg-git-stack--seen)))))))

(defun ygg-git-stack--review (row)
  (let ((review (plist-get row :review)))
    (cond ((plist-get row :draft) "draft")
          ((equal review "approved") "approved")
          ((equal review "changes_requested") "changes")
          (t "review"))))

(defun ygg-git-stack--checks (row)
  (pcase (plist-get row :checks)
    ('passing "CI ok") ('failing "CI failed") ('pending "CI running") (_ nil)))

(defun ygg-git-stack--line (branch row parent counts current others)
  (let* ((face (if current 'magit-branch-current 'magit-branch-local)))
    (string-join
     (delq nil
           (list (propertize branch 'font-lock-face face)
                 (and row (format "#%s" (plist-get row :number)))
                 (and row (ygg-git-stack--review row))
                 (and row (ygg-git-stack--checks row))
                 (and counts (format "+%d on %s" (car counts) parent))
                 (and counts (> (cdr counts) 0) (format "behind %d" (cdr counts)))
                 (and (> others 0)
                      (propertize (format "+%d other" others) 'font-lock-face 'magit-dimmed))))
     "  ")))

;;;###autoload
(defun ygg-git-stack-insert-section ()
  "Insert the current branch's stack, nothing when it stands alone."
  (when-let* ((ygg-git-stack)
              (branch (magit-get-current-branch)))
    (let* ((info (ygg-git-stack--info t))
           (repo (plist-get info :repo))
           (answer (and repo (progn (ygg-git-review-requests--load)
                                    (cdr (gethash repo ygg-git-review-requests--cache)))))
           (prs (plist-get answer :rows))
           (configured (ygg-git-stack--configured))
           (protected (plist-get info :protected)))
      (when (and (not (member branch protected))
                 (ygg-git-stack--linked-p branch configured prs protected))
        (let* ((ygg-git-stack--snap (append info (ygg-git-stack--read-refs)))
               (parents (ygg-git-stack--parents (plist-get ygg-git-stack--snap :locals) prs configured))
               (chain (ygg-git-stack--chain branch parents)))
          (when (cdr chain)
            (magit-insert-section (stack)
              (magit-insert-heading (format "Stack (%d)" (length chain)))
              (dolist (name chain)
                (let* ((row (ygg-git-stack--pr name prs))
                       (parent (or (cdr (assoc name parents)) (ygg-git-stack--trunk)))
                       (parent (if (assoc parent parents) parent (ygg-git-stack--trunk)))
                       (counts (ygg-git-stack--counts (ygg-git-stack--ref parent) name))
                       (others (length (seq-remove (lambda (c) (member c chain))
                                                   (ygg-git-stack--children name parents)))))
                  (when repo
                    (ygg-git-stack--state repo answer name row))
                  (magit-insert-section (stack-branch (list :branch name :row row :repo repo))
                    (insert (ygg-git-stack--line name row parent counts (equal name branch) others)
                            "\n"))))
              (insert ?\n))))))))

(defun ygg-git-stack--at-point ()
  (or (magit-section-value-if 'stack-branch)
      (user-error "No stack branch at point")))

(defun ygg-git-stack-visit ()
  "Check out the stack branch at point."
  (interactive)
  (magit-call-git "checkout" (plist-get (ygg-git-stack--at-point) :branch))
  (magit-refresh))

(defun ygg-git-stack-merge ()
  "Merge the request of the stack branch at point."
  (interactive)
  (let ((at (ygg-git-stack--at-point)))
    (ygg-git-pr-merge--start (or (plist-get at :repo) (user-error "Not a forge remote"))
                             (or (plist-get (plist-get at :row) :number)
                                 (user-error "No pull request for this branch")))))

(defvar-keymap magit-stack-section-map
  "R" #'ygg-git-stack-restack
  "b" #'ygg-git-stack-branch)

(defvar-keymap magit-stack-branch-section-map
  "RET" #'ygg-git-stack-visit
  "R" #'ygg-git-stack-restack
  "b" #'ygg-git-stack-branch
  "m" #'ygg-git-stack-merge)

(provide 'ygg-git-stack)
;;; ygg-git-stack.el ends here
