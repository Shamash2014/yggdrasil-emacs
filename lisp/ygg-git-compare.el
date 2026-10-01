;;; ygg-git-compare.el --- compare any two sides of a repository -*- lexical-binding: t; -*-

;;; Commentary:
;; Two sides, each a worktree (with what it has not committed), a branch,
;; a commit or a pull request, compared in the whole frame: magit's own
;; diff of the two on the left, folded to its files, and the diff of the
;; file at point on the right.  No checkout, branch or stash entry is made
;; to get there.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)

(defvar ygg-magit-diff-line-limit)

(defgroup ygg-git-compare nil
  "Compare two sides of a repository."
  :group 'magit-extensions :prefix "ygg-git-compare-")

(defcustom ygg-git-compare-list-width 0.38
  "The share of the frame's width the diff of every file takes."
  :type 'number)

(defcustom ygg-git-compare-follow-delay 0.15
  "Idle seconds after a move before the right pane follows the file at point."
  :type 'number)

(defcustom ygg-git-compare-gh-timeout 4
  "Seconds to wait for gh to list pull requests before going on without them."
  :type 'number)

(defvar-local ygg-git-compare--a-spec nil "Side A as picked: (KIND . WHAT).")
(defvar-local ygg-git-compare--b-spec nil "Side B as picked: (KIND . WHAT).")
(defvar-local ygg-git-compare--a nil "Side A as resolved, a plist.")
(defvar-local ygg-git-compare--b nil "Side B as resolved, a plist.")
(defvar-local ygg-git-compare--dots "..."
  "\"...\" diffs from where the sides parted, \"..\" diffs them directly.")
(defvar-local ygg-git-compare--window-config nil)
(defvar-local ygg-git-compare--file-window nil)
(defvar-local ygg-git-compare--shown nil "(RANGE . FILE) the right pane shows.")
(defvar-local ygg-git-compare--timer nil)
(defvar-local ygg-git-compare--list-buffer nil
  "In the right pane, the diff of every file it belongs to.")

;;; Git

(defun ygg-git-compare--git (&rest args)
  "What git ARGS prints, trimmed; a user error naming git's complaint on failure."
  (with-temp-buffer
    (let* ((err (make-temp-file "ygg-git-compare-"))
           (status (apply #'process-file "git" nil (list t err) nil args)))
      (unwind-protect
          (unless (eql status 0)
            (user-error "git %s: %s" (car args)
                        (string-trim (with-temp-buffer
                                       (insert-file-contents err)
                                       (buffer-string)))))
        (delete-file err)))
    (string-trim (buffer-string))))

(defun ygg-git-compare--commit (rev)
  (ygg-git-compare--git "rev-parse" "--verify" "--quiet" (concat rev "^{commit}")))

(defun ygg-git-compare--remote ()
  (or (magit-primary-remote) (car (magit-list-remotes))))

(defun ygg-git-compare--default-branch ()
  "The branch the repository's work merges into, local when there is one."
  (or (magit-main-branch)
      (when-let* ((remote (ygg-git-compare--remote)))
        (magit-git-string "symbolic-ref" "--short"
                          (format "refs/remotes/%s/HEAD" remote)))
      (magit-get-current-branch)
      "HEAD"))

(defun ygg-git-compare--pr-base (base)
  "BASE, a pull request's target branch, as the remote has it when it can."
  (let ((remote-base (when-let* ((remote (ygg-git-compare--remote)))
                       (concat remote "/" base))))
    (if (and remote-base (magit-rev-verify remote-base)) remote-base base)))

;;; Sides

(defun ygg-git-compare-fetch-pr (number)
  "Fetch pull request NUMBER's head without a branch for it; return its commit."
  (let ((remote (or (ygg-git-compare--remote) (user-error "No remote to fetch from"))))
    (message "Fetching pull request #%s from %s…" number remote)
    (ygg-git-compare--git "fetch" "--quiet" "--no-tags" remote
                          (format "pull/%s/head" number))
    (prog1 (ygg-git-compare--commit "FETCH_HEAD")
      (message nil))))

(defun ygg-git-compare--prepare (spec)
  "SPEC with its network work done: a pull request carries its fetched commit."
  (if (and (eq (car spec) 'pr) (not (plist-get (cdr spec) :sha)))
      (cons 'pr (append (list :sha (ygg-git-compare-fetch-pr
                                    (plist-get (cdr spec) :number)))
                        (cdr spec)))
    spec))

(defun ygg-git-compare-resolve (spec)
  "SPEC as the commits it stands for, without touching any checkout or ref.
A plist: :label, :diff the commit diffs are taken from, :log the commit
history is read from, and :uncommitted when :diff is a snapshot of a
worktree's changes over its HEAD.  The snapshot is what `git stash create'
makes, a commit no ref or stash entry points at; untracked files are not
in it."
  (pcase spec
    (`(worktree . ,dir)
     (let* ((default-directory (file-name-as-directory dir))
            (head (ygg-git-compare--commit "HEAD"))
            (snapshot (ygg-git-compare--git "stash" "create"))
            (branch (magit-get-current-branch)))
       (list :label (format "%s [%s]"
                            (file-name-nondirectory (directory-file-name dir))
                            (or branch (magit-rev-abbrev head)))
             :diff (if (string-empty-p snapshot) head snapshot)
             :log head
             :uncommitted (not (string-empty-p snapshot)))))
    (`(pr . ,pr)
     (let ((sha (or (plist-get pr :sha)
                    (ygg-git-compare-fetch-pr (plist-get pr :number)))))
       (list :label (format "#%s %s" (plist-get pr :number)
                            (or (plist-get pr :head) ""))
             :diff sha :log sha)))
    (`(rev . ,rev)
     (let ((sha (or (ignore-errors (ygg-git-compare--commit rev))
                    (user-error "No commit called %s" rev))))
       (list :label rev :diff sha :log sha)))
    (_ (error "Not a side: %S" spec))))

;;; Picking sides

(defun ygg-git-compare--gh-pulls ()
  "Open pull requests as gh lists them, or nil without gh or within its timeout."
  (when (executable-find "gh")
    (message "Listing open pull requests (gh)…")
    (with-temp-buffer
      (let* ((out (current-buffer))
             (err (generate-new-buffer " *ygg-git-compare-gh*"))
             (proc (make-process
                    :name "ygg-git-compare-gh" :buffer out :stderr err
                    :noquery t :connection-type 'pipe
                    :command '("gh" "pr" "list" "--json"
                               "number,title,headRefName,baseRefName")))
             (deadline (+ (float-time) ygg-git-compare-gh-timeout)))
        (unwind-protect
            (progn
              (while (and (process-live-p proc) (< (float-time) deadline))
                (accept-process-output proc 0.1))
              (unless (process-live-p proc)
                (while (accept-process-output proc 0)))
              (cond ((process-live-p proc)
                     (delete-process proc)
                     (message "gh took too long; pull requests left out")
                     nil)
                    ((zerop (process-exit-status proc))
                     (message nil)
                     (goto-char (point-min))
                     (ignore-errors
                       (json-parse-buffer :object-type 'plist :array-type 'list)))
                    (t (message "gh pr list failed; pull requests left out")
                       nil)))
          (kill-buffer err))))))

(defun ygg-git-compare--group (label group &optional note)
  (propertize label 'ygg-git-compare-group group 'ygg-git-compare-note note))

(defun ygg-git-compare-candidates (&optional with-prs)
  "Every side this repository offers, as (LABEL . SPEC), grouped by kind.
WITH-PRS asks gh for open pull requests too."
  (let ((here (magit-toplevel))
        (cands nil))
    (cl-flet ((add (label group spec &optional note)
                (while (assoc label cands) (setq label (concat label "'")))
                (push (cons (ygg-git-compare--group label group note) spec) cands)))
      (pcase-dolist (`(,path ,commit ,branch ,bare ,_detached ,_locked ,prunable)
                     (magit-list-worktrees))
        (unless (or bare prunable)
          (add (format "%s [%s]" (file-name-nondirectory (directory-file-name path))
                       (or branch (magit-rev-abbrev commit)))
               "Worktrees" (cons 'worktree (file-name-as-directory path))
               (concat (abbreviate-file-name path)
                       (when (and here (file-equal-p path here)) "  (here)")))))
      (dolist (b (magit-list-local-branch-names))
        (add b "Branches" (cons 'rev b)))
      (dolist (b (magit-list-remote-branch-names))
        (unless (string-suffix-p "/HEAD" b)
          (add b "Remote branches" (cons 'rev b))))
      (dolist (pr (and with-prs (ygg-git-compare--gh-pulls)))
        (add (format "#%s %s" (plist-get pr :number) (plist-get pr :title))
             "Pull requests"
             (cons 'pr (list :number (plist-get pr :number)
                             :title (plist-get pr :title)
                             :head (plist-get pr :headRefName)
                             :base (plist-get pr :baseRefName)))
             (format "%s → %s" (plist-get pr :headRefName)
                     (plist-get pr :baseRefName))))
      (dolist (line (magit-git-lines "log" "--format=%h %s" "-n" "30"))
        (add line "Recent commits" (cons 'rev (car (split-string line " "))))))
    (nreverse cands)))

(defun ygg-git-compare--label (spec cands)
  "The label of SPEC among CANDS, or a string that names it when it is not one."
  (or (car (seq-find (lambda (c) (equal (cdr c) spec)) cands))
      (pcase spec
        (`(rev . ,rev) rev)
        (`(worktree . ,dir) dir))))

(defun ygg-git-compare--here-spec ()
  (cons 'worktree (file-name-as-directory
                   (or (magit-toplevel) (user-error "Not in a git repository")))))

(defun ygg-git-compare-default-a (b)
  "The side A defaults to when B is picked: a pull request's base, else the
repository's default branch."
  (if (eq (car b) 'pr)
      (cons 'rev (ygg-git-compare--pr-base (plist-get (cdr b) :base)))
    (cons 'rev (ygg-git-compare--default-branch))))

(defun ygg-git-compare--read (prompt cands default)
  "Read a side with PROMPT among CANDS, DEFAULT the spec RET takes."
  (let* ((default-label (ygg-git-compare--label default cands))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (group-function
                         . ,(lambda (cand transform)
                              (if transform cand
                                (get-text-property 0 'ygg-git-compare-group cand))))
                        (annotation-function
                         . ,(lambda (cand)
                              (when-let* ((note (get-text-property
                                                 0 'ygg-git-compare-note cand)))
                                (concat "  " (propertize note 'face 'shadow)))))
                        (display-sort-function . identity)
                        (cycle-sort-function . identity))
                    (complete-with-action action cands str pred))))
         (choice (completing-read (format-prompt prompt default-label)
                                  table nil nil nil nil default-label)))
    (or (cdr (assoc choice cands))
        (and (equal choice default-label) default)
        (cons 'rev choice))))

(defun ygg-git-compare--read-sides ()
  "Read side B, then side A defaulting to what B is compared against."
  (let* ((cands (ygg-git-compare-candidates t))
         (b (ygg-git-compare--read "Compare (B)" cands (ygg-git-compare--here-spec)))
         (a (ygg-git-compare--read (format "Against (A) for %s"
                                           (ygg-git-compare--label b cands))
                                   cands (ygg-git-compare-default-a b))))
    (list a b)))

;;; The two panes

(defun ygg-git-compare--range ()
  (concat (plist-get ygg-git-compare--a :diff) ygg-git-compare--dots
          (plist-get ygg-git-compare--b :diff)))

(defun ygg-git-compare--large-p (range)
  "Whether RANGE changes more lines than a status buffer would wash unasked."
  (let ((counts (mapcar #'string-to-number
                        (split-string (or (magit-git-string "diff" "--shortstat" range) "")
                                      "[^0-9]+" t))))
    (> (apply #'+ (cdr counts))
       (if (boundp 'ygg-magit-diff-line-limit) ygg-magit-diff-line-limit 2000))))

(defun ygg-git-compare--args (range)
  "Magit's diff arguments for RANGE with its diffstat; only the diffstat when
RANGE is too large to wash, since --no-patch cancels what comes before it."
  (let ((args (cl-set-difference (car (magit-diff-arguments 'magit-diff-mode))
                                 '("--stat" "--numstat" "--no-patch")
                                 :test #'equal)))
    (if (ygg-git-compare--large-p range)
        (append '("--no-patch" "--numstat" "--stat") args)
      (cons "--stat" args))))

(defun ygg-git-compare--display-in (window)
  (lambda (buffer) (set-window-buffer window buffer) window))

(defmacro ygg-git-compare--display (window &rest body)
  "Run BODY with magit showing its buffer in WINDOW, or nowhere when nil.
Nothing is selected and no window layout is remembered for magit's q."
  (declare (indent 1))
  `(let ((magit-display-buffer-noselect t)
         (magit-pre-display-buffer-hook nil)
         (magit-post-display-buffer-hook nil)
         (magit-display-buffer-function
          (if (window-live-p ,window) (ygg-git-compare--display-in ,window) #'ignore)))
     ,@body))

(defun ygg-git-compare--side-label (side)
  (concat (plist-get side :label) (when (plist-get side :uncommitted) " + uncommitted")))

(defun ygg-git-compare--header ()
  (magit-set-header-line-format
   (concat "A: " (ygg-git-compare--side-label ygg-git-compare--a)
           " ↔ B: " (ygg-git-compare--side-label ygg-git-compare--b)
           (when (or (plist-get ygg-git-compare--a :uncommitted)
                     (plist-get ygg-git-compare--b :uncommitted))
             " (untracked files left out)")
           (if (equal ygg-git-compare--dots "...") "  A...B" "  A..B"))))

(defun ygg-git-compare--fold ()
  "Fold every file to its heading, the diffstat left open, point on its first file."
  (magit-section-show-level-1-all)
  (when-let* ((stat (seq-find (lambda (s) (eq (oref s type) 'diffstat))
                              (oref magit-root-section children))))
    (magit-section-show stat)
    (goto-char (if-let* ((file (car (oref stat children))))
                   (oref file start)
                 (point-min)))))

(defun ygg-git-compare--redraw ()
  "Resolve both sides afresh, a worktree snapshot again, and diff them here."
  (setq ygg-git-compare--a (ygg-git-compare-resolve ygg-git-compare--a-spec)
        ygg-git-compare--b (ygg-git-compare-resolve ygg-git-compare--b-spec)
        ygg-git-compare--shown nil)
  (let ((range (ygg-git-compare--range)))
    (setq magit-buffer-diff-range range
          magit-buffer-diff-args (ygg-git-compare--args range)))
  (magit-refresh-buffer)
  (ygg-git-compare--fold)
  (ygg-git-compare--follow (current-buffer)))

(defun ygg-git-compare--file-buffer-name ()
  (format "magit-diff: compare file of %s" (buffer-name)))

(defun ygg-git-compare--show-file (file)
  "Show FILE's diff alone in the right pane."
  (let ((list (current-buffer))
        (range magit-buffer-diff-range)
        (args (cl-set-difference magit-buffer-diff-args
                                 '("--stat" "--numstat" "--no-patch")
                                 :test #'equal))
        (window ygg-git-compare--file-window))
    (setq ygg-git-compare--shown (cons range file))
    (with-current-buffer
        (ygg-git-compare--display window
          (magit-setup-buffer #'magit-diff-mode nil
            :buffer (ygg-git-compare--file-buffer-name)
            :directory default-directory
            (magit-buffer-diff-range range)
            (magit-buffer-diff-typearg nil)
            (magit-buffer-diff-type 'committed)
            (magit-buffer-diff-args args)
            (magit-buffer-diff-files (list file))
            (magit-buffer-diff-files-suspended nil)))
      (setq magit-buffer-locked-p t
            ygg-git-compare--list-buffer list)
      (ygg-git-compare-mode 1)
      (current-buffer))))

(defun ygg-git-compare--follow (buffer)
  "Make BUFFER's right pane show the file at point, unless it already does."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ygg-git-compare--timer nil)
      (when-let* ((file (magit-file-at-point))
                  ((window-live-p ygg-git-compare--file-window))
                  ((not (equal ygg-git-compare--shown
                               (cons magit-buffer-diff-range file)))))
        (ygg-git-compare--show-file file)))))

(defun ygg-git-compare--schedule ()
  (when ygg-git-compare--timer (cancel-timer ygg-git-compare--timer))
  (setq ygg-git-compare--timer
        (run-with-idle-timer ygg-git-compare-follow-delay nil
                             #'ygg-git-compare--follow (current-buffer))))

;;; What magit lacks

(defun ygg-git-compare--list ()
  "The diff of every file this command acts on, from either pane."
  (or (and ygg-git-compare--a-spec (current-buffer))
      (and (buffer-live-p ygg-git-compare--list-buffer) ygg-git-compare--list-buffer)
      (user-error "Not in a compare")))

(defun ygg-git-compare-refresh ()
  "Resolve both sides again, snapshotting a worktree's changes afresh."
  (interactive)
  (with-current-buffer (ygg-git-compare--list) (ygg-git-compare--redraw)))

(defun ygg-git-compare-swap ()
  "Swap sides A and B."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (cl-rotatef ygg-git-compare--a-spec ygg-git-compare--b-spec)
    (ygg-git-compare--redraw)))

(defun ygg-git-compare-toggle-dots ()
  "Diff A...B, from where the sides parted, or A..B, one against the other."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (setq ygg-git-compare--dots (if (equal ygg-git-compare--dots "...") ".." "..."))
    (ygg-git-compare--redraw)))

(defun ygg-git-compare-log ()
  "Show the commits only in A and only in B, marked < and >, in the right pane."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (let ((window ygg-git-compare--file-window)
          (list (current-buffer)))
      (setq ygg-git-compare--shown nil)
      (with-current-buffer
          (ygg-git-compare--display window
            (magit-log-setup-buffer
             (list (concat (plist-get ygg-git-compare--a :log) "..."
                           (plist-get ygg-git-compare--b :log)))
             (cons "--left-right" (car (magit-log-arguments 'magit-log-mode)))
             nil t))
        (setq ygg-git-compare--list-buffer list)
        (ygg-git-compare-mode 1)
        (when (window-live-p window) (select-window window))))))

(defun ygg-git-compare--close (list)
  "Kill LIST and the panes it opened; return the windows from before it."
  (let ((config (buffer-local-value 'ygg-git-compare--window-config list))
        (timer (buffer-local-value 'ygg-git-compare--timer list)))
    (when timer (cancel-timer timer))
    (dolist (b (buffer-list))
      (when (or (eq b list)
                (eq (buffer-local-value 'ygg-git-compare--list-buffer b) list))
        (kill-buffer b)))
    config))

(defun ygg-git-compare-quit ()
  "Leave the compare and bring back the windows it replaced."
  (interactive)
  (when-let* ((config (ygg-git-compare--close (ygg-git-compare--list))))
    (set-window-configuration config)))

(defvar-keymap ygg-git-compare-mode-map
  "~" #'ygg-git-compare-swap
  "." #'ygg-git-compare-toggle-dots
  "#" #'ygg-git-compare-log
  "q" #'ygg-git-compare-quit
  "<remap> <magit-refresh>" #'ygg-git-compare-refresh)

(define-minor-mode ygg-git-compare-mode
  "A magit buffer that is part of a compare of two sides.
\\<ygg-git-compare-mode-map>
\\[ygg-git-compare-swap] swaps the sides.
\\[ygg-git-compare-toggle-dots] switches between A...B and A..B.
\\[ygg-git-compare-log] shows the commits only in A and only in B.
\\[ygg-git-compare-refresh] snapshots worktrees again.
\\[ygg-git-compare-quit] leaves, bringing back the windows from before."
  :lighter " Compare")

;;; Opening

(defun ygg-git-compare-buffer (root a b)
  "Magit's diff of sides A and B in ROOT, folded to its files, not yet shown."
  (let* ((default-directory root)
         (a (ygg-git-compare--prepare a))
         (b (ygg-git-compare--prepare b))
         (a-side (ygg-git-compare-resolve a))
         (b-side (ygg-git-compare-resolve b))
         (dots (if (magit-git-string "merge-base" (plist-get a-side :diff)
                                    (plist-get b-side :diff))
                   "..." ".."))
         (range (concat (plist-get a-side :diff) dots (plist-get b-side :diff))))
    (with-current-buffer
        (ygg-git-compare--display nil
          (magit-diff-setup-buffer range nil (ygg-git-compare--args range) nil
                                   'committed t))
      (setq ygg-git-compare--a-spec a ygg-git-compare--b-spec b
            ygg-git-compare--a a-side ygg-git-compare--b b-side
            ygg-git-compare--dots dots)
      (ygg-git-compare-mode 1)
      (add-hook 'magit-refresh-buffer-hook #'ygg-git-compare--header nil t)
      (add-hook 'post-command-hook #'ygg-git-compare--schedule nil t)
      (ygg-git-compare--header)
      (ygg-git-compare--fold)
      (current-buffer))))

(defun ygg-git-compare-open (a b)
  "Compare sides A and B of this repository in the whole frame."
  (let* ((root (or (magit-toplevel) (user-error "Not in a git repository")))
         (old (and (bound-and-true-p ygg-git-compare-mode) (ygg-git-compare--list)))
         (config (if old
                     (buffer-local-value 'ygg-git-compare--window-config old)
                   (current-window-configuration)))
         (buffer (ygg-git-compare-buffer root a b)))
    (when (and old (buffer-live-p old) (not (eq old buffer)))
      (ygg-git-compare--close old))
    (delete-other-windows)
    (switch-to-buffer buffer)
    (let ((right (split-window nil (round (* ygg-git-compare-list-width
                                             (window-total-width)))
                               'right)))
      (setq ygg-git-compare--window-config config
            ygg-git-compare--file-window right)
      (if-let* ((file (magit-file-at-point)))
          (ygg-git-compare--show-file file)
        (save-selected-window (ygg-git-compare-log))))
    buffer))

;;;###autoload
(defun ygg-git-compare (a b)
  "Compare side A with side B of this repository in the whole frame.
A side is a worktree with what it has not committed, a branch, a commit
or an open pull request.  B is read first and defaults to this worktree;
A defaults to a pull request's base, else the repository's default branch."
  (interactive (progn (require 'magit) (ygg-git-compare--read-sides)))
  (ygg-git-compare-open a b))

;;;###autoload
(defun ygg-git-compare-at-point ()
  "Compare the worktree, branch or commit at point against a side read for A."
  (interactive)
  (require 'magit)
  (let ((b (if-let* ((dir (magit-section-value-if 'worktree)))
               (cons 'worktree (file-name-as-directory dir))
             (when-let* ((rev (magit-branch-or-commit-at-point)))
               (cons 'rev rev)))))
    (if (not b)
        (call-interactively #'ygg-git-compare)
      (let* ((cands (ygg-git-compare-candidates))
             (a (ygg-git-compare--read (format "Compare %s against (A)"
                                               (ygg-git-compare--label b cands))
                                       cands (ygg-git-compare-default-a b))))
        (ygg-git-compare-open a b)))))

(provide 'ygg-git-compare)
;;; ygg-git-compare.el ends here
