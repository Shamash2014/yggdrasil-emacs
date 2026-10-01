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
(defvar aob-acp-agents)
(defvar aob-acp-start-dir)
(defvar aob-prompt-typed)
(defvar aob-compose--dir)
(defvar aob-compose-spawn-function)
(defvar ygg-aob--draft-tree)
(declare-function aob-live-sessions "aob")
(declare-function aob-session-name "aob")
(declare-function aob-session-state "aob")
(declare-function aob-session-cwd "aob")
(declare-function aob-prompt "aob")
(declare-function aob-trace "aob-trace")
(declare-function aob-acp-spawn "aob-acp")
(declare-function ygg-aob--expand-presets "layer-aob" (text))

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

(defcustom ygg-git-compare-untracked-max-size 1000000
  "Bytes past which an untracked file is shown by its diffstat alone and
left out of a review."
  :type 'natnum)

(defvar-local ygg-git-compare--root nil "The checkout the compare was opened from.")
(defvar-local ygg-git-compare--plan nil
  "How the sides are diffed, as `ygg-git-compare--planned' answers.")
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
(defvar-local ygg-git-compare--comments nil
  "Review comments held on this compare until it is sent, newest first.")
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

(defun ygg-git-compare--url-key (url)
  "URL as host/owner/name, however git or gh spells it."
  (thread-last url
               (replace-regexp-in-string "\\`[a-z+]+://" "")
               (replace-regexp-in-string "\\`[^@/]+@" "")
               (replace-regexp-in-string "\\`\\([^/:]+\\):" "\\1/")
               (replace-regexp-in-string "\\(\\.git\\)?/*\\'" "")
               downcase))

(defun ygg-git-compare--pr-remote ()
  "The remote of the repository gh lists pull requests for, else the primary one."
  (or (when-let* ((url (ygg-git-compare--gh-repo-url))
                  (key (ygg-git-compare--url-key url)))
        (seq-find (lambda (remote)
                    (when-let* ((remote-url (magit-get "remote" remote "url")))
                      (equal (ygg-git-compare--url-key remote-url) key)))
                  (magit-list-remotes)))
      (ygg-git-compare--remote)))

(defun ygg-git-compare--pr-base (base &optional remote)
  "BASE, a pull request's target branch, as REMOTE has it when it can."
  (let ((remote-base (when-let* ((remote (or remote (ygg-git-compare--remote))))
                       (concat remote "/" base))))
    (if (and remote-base (magit-rev-verify remote-base)) remote-base base)))

;;; Sides

(defun ygg-git-compare-fetch-pr (number &optional remote)
  "Fetch pull request NUMBER's head from REMOTE without a branch for it;
return its commit."
  (let ((remote (or remote (ygg-git-compare--remote)
                    (user-error "No remote to fetch from"))))
    (message "Fetching pull request #%s from %s…" number remote)
    (ygg-git-compare--git "fetch" "--quiet" "--no-tags" remote
                          (format "pull/%s/head" number))
    (prog1 (ygg-git-compare--commit "FETCH_HEAD")
      (message nil))))

(defun ygg-git-compare--prepare (spec)
  "SPEC with its network work done: a pull request carries its fetched commit."
  (if (and (eq (car spec) 'pr) (not (plist-get (cdr spec) :sha)))
      (cons 'pr (append (list :sha (ygg-git-compare-fetch-pr
                                    (plist-get (cdr spec) :number)
                                    (plist-get (cdr spec) :remote)))
                        (cdr spec)))
    spec))

(defun ygg-git-compare--dirty-p (dir)
  (let ((default-directory dir))
    (not (string-empty-p (ygg-git-compare--git "--no-optional-locks" "status"
                                               "--porcelain")))))

(defun ygg-git-compare-resolve (spec)
  "SPEC as the commits it stands for, without touching any checkout or ref.
A plist: :label, :diff the commit diffs are taken from, :log the commit
history is read from, and for a worktree :dir, its checkout, with
:uncommitted when it has changes over its HEAD, untracked files included."
  (pcase spec
    (`(worktree . ,dir)
     (let* ((default-directory (file-name-as-directory dir))
            (head (ygg-git-compare--commit "HEAD"))
            (branch (magit-get-current-branch)))
       (list :label (format "%s [%s]"
                            (file-name-nondirectory (directory-file-name dir))
                            (or branch (magit-rev-abbrev head)))
             :diff head
             :log head
             :dir default-directory
             :uncommitted (ygg-git-compare--dirty-p default-directory))))
    (`(pr . ,pr)
     (let ((sha (or (plist-get pr :sha)
                    (ygg-git-compare-fetch-pr (plist-get pr :number)
                                              (plist-get pr :remote)))))
       (list :label (format "#%s %s" (plist-get pr :number)
                            (or (plist-get pr :head) ""))
             :diff sha :log sha)))
    (`(rev . ,rev)
     (let ((sha (or (ignore-errors (ygg-git-compare--commit rev))
                    (user-error "No commit called %s" rev))))
       (list :label rev :diff sha :log sha)))
    (_ (error "Not a side: %S" spec))))

;;; Picking sides

(defun ygg-git-compare--gh (what &rest args)
  "What gh ARGS prints, or nil without gh, on failure or past its timeout,
saying WHAT is left out."
  (when (executable-find "gh")
    (with-temp-buffer
      (let* ((out (current-buffer))
             (err (generate-new-buffer " *ygg-git-compare-gh*"))
             (proc (make-process
                    :name "ygg-git-compare-gh" :buffer out :stderr err
                    :noquery t :connection-type 'pipe
                    :command (cons "gh" args)))
             (deadline (+ (float-time) ygg-git-compare-gh-timeout)))
        (unwind-protect
            (progn
              (while (and (process-live-p proc) (< (float-time) deadline))
                (accept-process-output proc 0.1))
              (unless (process-live-p proc)
                (while (accept-process-output proc 0)))
              (cond ((process-live-p proc)
                     (delete-process proc)
                     (message "gh took too long; %s left out" what)
                     nil)
                    ((zerop (process-exit-status proc))
                     (buffer-string))
                    (t (message "gh %s failed; %s left out" (car args) what)
                       nil)))
          (kill-buffer err))))))

(defun ygg-git-compare--gh-pulls ()
  "Open pull requests as gh lists them, or nil without gh or within its timeout."
  (message "Listing open pull requests (gh)…")
  (when-let* ((json (ygg-git-compare--gh "pull requests" "pr" "list" "--json"
                                         "number,title,headRefName,baseRefName")))
    (message nil)
    (ignore-errors (json-parse-string json :object-type 'plist :array-type 'list))))

(defun ygg-git-compare--gh-repo-url ()
  "The URL of the repository gh takes pull requests from, or nil."
  (when-let* ((url (ygg-git-compare--gh "its remote" "repo" "view" "--json" "url"
                                        "--jq" ".url")))
    (string-trim url)))

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
      (let* ((pulls (and with-prs (ygg-git-compare--gh-pulls)))
             (remote (and pulls (ygg-git-compare--pr-remote))))
        (dolist (pr pulls)
          (add (format "#%s %s" (plist-get pr :number) (plist-get pr :title))
               "Pull requests"
               (cons 'pr (list :number (plist-get pr :number)
                               :title (plist-get pr :title)
                               :head (plist-get pr :headRefName)
                               :base (plist-get pr :baseRefName)
                               :remote remote))
               (format "%s → %s" (plist-get pr :headRefName)
                       (plist-get pr :baseRefName)))))
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
  "The side A defaults to when B is picked: a pull request's base, HEAD for
this worktree when it has uncommitted changes, else the repository's
default branch."
  (cond ((eq (car b) 'pr)
         (cons 'rev (ygg-git-compare--pr-base (plist-get (cdr b) :base)
                                              (plist-get (cdr b) :remote))))
        ((and (eq (car b) 'worktree)
              (file-equal-p (cdr b) (cdr (ygg-git-compare--here-spec)))
              (ygg-git-compare--dirty-p (cdr b)))
         '(rev . "HEAD"))
        (t (cons 'rev (ygg-git-compare--default-branch)))))

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

(defconst ygg-git-compare--quiet-args
  '("--no-optional-locks" "-c" "diff.autoRefreshIndex=false")
  "Git's arguments that leave a worktree's index unwritten, stat cache and all.")

(defun ygg-git-compare--global-args ()
  "Magit's global git arguments, kept from refreshing a worktree's index."
  (append ygg-git-compare--quiet-args (default-value 'magit-git-global-arguments)))

(defun ygg-git-compare--planned (a b dots root)
  "How sides A and B are diffed with DOTS, as a plist: :work the side whose
working tree stands in for its commit, :reverse when that is A, :dir git
runs in, its checkout or ROOT, :range magit diffs there, and :untracked
the files it adds that git does not ignore.  B's working tree is taken
when it has uncommitted changes, else A's when DOTS is \"..\": A...B
leaves A's tip out, and with both dirty A stands at its HEAD."
  (let* ((work (cond ((plist-get b :uncommitted) b)
                     ((and (plist-get a :uncommitted) (equal dots "..")) a)))
         (dir (or (plist-get work :dir) root))
         (default-directory dir)
         (magit-git-global-arguments (ygg-git-compare--global-args)))
    (list :work work
          :reverse (and work (eq work a))
          :dir dir
          :range (cond ((not work)
                        (concat (plist-get a :diff) dots (plist-get b :diff)))
                       ((eq work a) (plist-get b :diff))
                       ((equal dots "...")
                        (or (magit-git-string "merge-base" (plist-get a :diff)
                                              (plist-get b :diff))
                            (user-error "%s and %s share no history"
                                        (plist-get a :label) (plist-get b :label))))
                       (t (plist-get a :diff)))
          :untracked (and work (magit-git-items "ls-files" "-z" "--others"
                                                "--exclude-standard")))))

(defun ygg-git-compare-agent-path (dir)
  "DIR as an agent running where the repository lives reads it: over TRAMP
without the method and host, which name nothing on the far side."
  (if (file-remote-p dir) (file-local-name dir) (abbreviate-file-name dir)))

(defun ygg-git-compare--range ()
  "The compared range for a reader: a side diffed with what it has not
committed is named by its checkout."
  (cl-flet ((end (side)
              (if (eq side (plist-get ygg-git-compare--plan :work))
                  (ygg-git-compare-agent-path (plist-get side :dir))
                (plist-get side :diff))))
    (concat (end ygg-git-compare--a) ygg-git-compare--dots (end ygg-git-compare--b))))

(defun ygg-git-compare--diff-args (plan)
  "What git diff takes for PLAN's tracked changes, before any option of magit's."
  (cons (plist-get plan :range) (and (plist-get plan :reverse) '("-R"))))

(defun ygg-git-compare--large-p (plan)
  "Whether PLAN changes more lines than a status buffer would wash unasked."
  (let* ((default-directory (plist-get plan :dir))
         (magit-git-global-arguments (ygg-git-compare--global-args))
         (counts (mapcar #'string-to-number
                         (split-string (or (magit-git-string "diff" "--shortstat"
                                                             (plist-get plan :range))
                                           "")
                                       "[^0-9]+" t))))
    (> (apply #'+ (cdr counts))
       (if (boundp 'ygg-magit-diff-line-limit) ygg-magit-diff-line-limit 2000))))

(defun ygg-git-compare--args (plan)
  "Magit's diff arguments for PLAN with its diffstat; only the diffstat when
PLAN is too large to wash, since --no-patch cancels what comes before it."
  (let ((args (append (cl-set-difference (car (magit-diff-arguments 'magit-diff-mode))
                                         '("--stat" "--numstat" "--no-patch" "-R")
                                         :test #'equal)
                      (cdr (ygg-git-compare--diff-args plan)))))
    (if (ygg-git-compare--large-p plan)
        (append '("--no-patch" "--numstat" "--stat") args)
      (cons "--stat" args))))

(defun ygg-git-compare--insert-untracked ()
  "The files the plan's worktree adds untracked, each a file section to follow."
  (when-let* ((files (plist-get ygg-git-compare--plan :untracked)))
    (magit-insert-section (ygg-git-compare-untracked nil)
      (magit-insert-heading (length files) "Untracked files")
      (dolist (file files)
        (if (ygg-git-compare--nested-p file)
            (magit-insert-section (ygg-git-compare-nested file)
              (insert (propertize file 'font-lock-face 'magit-filename)
                      (propertize "  nested repository, not diffed" 'font-lock-face 'shadow)
                      "\n"))
          (magit-insert-section (file file)
            (insert (propertize file 'font-lock-face 'magit-filename) "\n"))))
      (insert "\n"))))

(defun ygg-git-compare--nested-p (file)
  "Whether untracked FILE is a repository of its own, listed as a directory."
  (string-suffix-p "/" file))

(defun ygg-git-compare--sections-hook ()
  (append (default-value 'magit-diff-sections-hook)
          (list #'ygg-git-compare--insert-untracked)))

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
  (concat (plist-get side :label)
          (cond ((eq side (plist-get ygg-git-compare--plan :work))
                 " + uncommitted (incl. untracked)")
                ((plist-get side :uncommitted) " (uncommitted not shown)"))))

(defun ygg-git-compare--header ()
  (magit-set-header-line-format
   (concat "A: " (ygg-git-compare--side-label ygg-git-compare--a)
           " ↔ B: " (ygg-git-compare--side-label ygg-git-compare--b)
           (if (equal ygg-git-compare--dots "...") "  A...B" "  A..B"))))

(defun ygg-git-compare--fold ()
  "Fold every file to its heading, the diffstat and untracked files left open,
point on the first file."
  (magit-section-show-level-1-all)
  (let* ((top (oref magit-root-section children))
         (open (seq-filter (lambda (s) (memq (oref s type)
                                             '(diffstat ygg-git-compare-untracked)))
                           top)))
    (mapc #'magit-section-show open)
    (goto-char (if-let* ((file (seq-some (lambda (s) (car (oref s children))) open)))
                   (oref file start)
                 (point-min)))))

(defun ygg-git-compare--redraw (&optional a-spec b-spec dots)
  "Resolve sides A-SPEC and B-SPEC afresh, worktrees as they stand, and diff
them here with DOTS; each nil keeps the compare's own.  When they cannot
be diffed, the compare stays as it was."
  (pcase-let* ((a-spec (or a-spec ygg-git-compare--a-spec))
               (b-spec (or b-spec ygg-git-compare--b-spec))
               (dots (or dots ygg-git-compare--dots))
               (`(,a ,b) (let ((default-directory ygg-git-compare--root))
                           (list (ygg-git-compare-resolve a-spec)
                                 (ygg-git-compare-resolve b-spec))))
               (plan (ygg-git-compare--planned a b dots ygg-git-compare--root))
               (args (ygg-git-compare--args plan)))
    (setq ygg-git-compare--a-spec a-spec
          ygg-git-compare--b-spec b-spec
          ygg-git-compare--dots dots
          ygg-git-compare--a a
          ygg-git-compare--b b
          ygg-git-compare--plan plan
          ygg-git-compare--shown nil
          default-directory (plist-get plan :dir)
          magit-buffer-diff-range (plist-get plan :range)
          magit-buffer-diff-args args))
  (magit-refresh-buffer)
  (ygg-git-compare--fold)
  (ygg-git-compare--follow (current-buffer)))

(defun ygg-git-compare--file-buffer-name ()
  (format "magit-diff: compare file of %s" (buffer-name)))

(defun ygg-git-compare--untracked-pair (file reverse)
  "FILE added from nothing, or the reverse when REVERSE, for git diff --no-index."
  (if reverse (list file "/dev/null") (list "/dev/null" file)))

(defun ygg-git-compare--oversized-p (file)
  (> (or (file-attribute-size (file-attributes (expand-file-name file))) 0)
     ygg-git-compare-untracked-max-size))

(defun ygg-git-compare--renamed-from (file)
  "What FILE was called on the old side when the compare renames or copies
it, else nil.  Read from its diff, or from git when only the diffstat shows."
  (let ((section (seq-find (lambda (s) (and (eq (oref s type) 'file)
                                            (equal (oref s value) file)))
                           (oref magit-root-section children))))
    (if section
        (oref section source)
      (let ((items (apply #'magit-git-items "diff" "-z" "--name-status" "-M"
                          (ygg-git-compare--diff-args ygg-git-compare--plan)))
            found)
        (while (and items (not found))
          (let ((status (pop items)))
            (if (string-match-p "\\`[RC]" status)
                (let ((old (pop items)) (new (pop items)))
                  (when (equal new file) (setq found old)))
              (pop items))))
        found))))

(defun ygg-git-compare--show-file (file)
  "Show FILE's diff alone in the right pane, from its old name when renamed;
an untracked one against nothing."
  (let* ((list (current-buffer))
         (range magit-buffer-diff-range)
         (untracked (member file (plist-get ygg-git-compare--plan :untracked)))
         (source (and (not untracked) (ygg-git-compare--renamed-from file)))
         (args (cl-set-difference magit-buffer-diff-args
                                  '("--stat" "--numstat" "--no-patch")
                                  :test #'equal))
         (window ygg-git-compare--file-window))
    (when untracked
      (setq args (remove "-R" args))
      (when (ygg-git-compare--oversized-p file)
        (setq args (append '("--no-patch" "--numstat" "--stat") args))))
    (setq ygg-git-compare--shown (cons range file))
    (with-current-buffer
        (ygg-git-compare--display window
          (magit-setup-buffer #'magit-diff-mode nil
            :buffer (ygg-git-compare--file-buffer-name)
            :directory default-directory
            (magit-buffer-diff-range (and (not untracked) range))
            (magit-buffer-diff-typearg (and untracked "--no-index"))
            (magit-buffer-diff-type 'committed)
            (magit-buffer-diff-args args)
            (magit-buffer-diff-files
             (if untracked
                 (ygg-git-compare--untracked-pair
                  file (plist-get ygg-git-compare--plan :reverse))
               (if source (list source file) (list file))))
            (magit-buffer-diff-files-suspended nil)
            (magit-git-global-arguments (ygg-git-compare--global-args))))
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
  "Resolve both sides again, a worktree's changes as they stand now."
  (interactive)
  (with-current-buffer (ygg-git-compare--list) (ygg-git-compare--redraw)))

(defun ygg-git-compare-swap ()
  "Swap sides A and B."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (ygg-git-compare--redraw ygg-git-compare--b-spec ygg-git-compare--a-spec)))

(defun ygg-git-compare-toggle-dots ()
  "Diff A...B, from where the sides parted, or A..B, one against the other."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (ygg-git-compare--redraw nil nil (if (equal ygg-git-compare--dots "...") ".." "..."))))

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

(defun ygg-git-compare--drop-comments (list)
  "Ask before LIST's held review comments go unsent; a user error keeps them."
  (when-let* ((n (length (buffer-local-value 'ygg-git-compare--comments list)))
              ((> n 0)))
    (unless (y-or-n-p (format "Drop %d unsent review comment%s? " n (if (= n 1) "" "s")))
      (user-error "%s" (substitute-command-keys
                        "Comments kept; \\<ygg-git-compare-mode-map>\\[ygg-git-compare-review] sends them")))
    (with-current-buffer list (setq ygg-git-compare--comments nil))))

(defun ygg-git-compare-quit ()
  "Leave the compare and bring back the windows it replaced, asking first
when review comments are held unsent."
  (interactive)
  (let ((list (ygg-git-compare--list)))
    (ygg-git-compare--drop-comments list)
    (when-let* ((config (ygg-git-compare--close list)))
      (set-window-configuration config))))

;;; Review

(defcustom ygg-git-compare-review-instructions
  "Review the change from A to B below as a strict reviewer.  List each
finding as path:line, a severity (blocker, major, minor, nit), what is
wrong and a concrete fix.  My comments, each anchored to a file and line
with the lines around it, are instructions, not suggestions: work out the
change each asks for, and ask me where one leaves a choice open.  Do not
change any files."
  "What an agent is asked to do with a compare it is sent."
  :type 'string)

(defcustom ygg-git-compare-review-max-chars 60000
  "How much of the compared log and diff a review carries before it is cut."
  :type 'natnum)

(defconst ygg-git-compare-review-preset "review"
  "The preset a new session that plans from a review runs under.")

(defun ygg-git-compare--hunk-lines (hunk)
  "Each diff line of HUNK as (POS SIDE LINE); a removed line is on the old side."
  (save-excursion
    (goto-char (oref hunk content))
    (let ((old (car (oref hunk from-range)))
          (new (car (oref hunk to-range)))
          lines)
      (while (< (point) (oref hunk end))
        (pcase (char-after)
          (?- (push (list (point) 'old old) lines) (cl-incf old))
          (?+ (push (list (point) 'new new) lines) (cl-incf new))
          (?\s (push (list (point) 'new new) lines) (cl-incf old) (cl-incf new)))
        (forward-line))
      (nreverse lines))))

(defun ygg-git-compare--anchor ()
  "The diff line at point as a comment's place: file, side, line and quote."
  (let* ((hunk (magit-current-section))
         (lines (and hunk (magit-section-match 'hunk hunk)
                     (oref hunk from-range) (oref hunk to-range)
                     (ygg-git-compare--hunk-lines hunk)))
         (at (or (cl-position (line-beginning-position) lines :key #'car)
                 (user-error "Not on a diff line")))
         (side (nth 1 (nth at lines)))
         (file (oref hunk parent)))
    (list :file (or (and (eq side 'old) (oref file source)) (oref file value))
          :side side
          :line (nth 2 (nth at lines))
          :quote (buffer-substring-no-properties
                  (car (nth (max 0 (- at 2)) lines))
                  (save-excursion
                    (goto-char (car (nth (min (1- (length lines)) (+ at 2)) lines)))
                    (line-end-position))))))

(defun ygg-git-compare--where (comment)
  (format "%s:%d%s" (plist-get comment :file) (plist-get comment :line)
          (if (eq (plist-get comment :side) 'old) " (removed line)" "")))

(defun ygg-git-compare--read-comment (where &optional initial)
  (let ((text (string-trim (read-string (format "Comment on %s: " where) initial))))
    (if (string-empty-p text) (user-error "Empty comment") text)))

(defun ygg-git-compare-comment ()
  "Hold a review comment on the diff line at point until the compare is sent."
  (interactive)
  (let* ((anchor (ygg-git-compare--anchor))
         (comment (cons :text (cons (ygg-git-compare--read-comment
                                     (ygg-git-compare--where anchor))
                                    anchor))))
    (with-current-buffer (ygg-git-compare--list)
      (push (append comment (list :range (ygg-git-compare--range-label)))
            ygg-git-compare--comments))))

(defun ygg-git-compare-comments ()
  "Pick a held review comment to edit or drop."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (let* ((rows (or (mapcar (lambda (c)
                               (cons (format "%s  %s" (ygg-git-compare--where c)
                                             (plist-get c :text))
                                     c))
                             (reverse ygg-git-compare--comments))
                     (user-error "No review comments held")))
           (c (cdr (assoc (completing-read "Review comment: " rows nil t) rows))))
      (pcase (car (read-multiple-choice (ygg-git-compare--where c)
                                        '((?e "edit") (?d "drop"))))
        (?e (plist-put c :text (ygg-git-compare--read-comment
                                (ygg-git-compare--where c) (plist-get c :text))))
        (?d (setq ygg-git-compare--comments (delq c ygg-git-compare--comments)))))))

(defun ygg-git-compare--capped (text)
  (if (> (length text) ygg-git-compare-review-max-chars)
      (concat (substring text 0 ygg-git-compare-review-max-chars)
              (format "\n… %d more characters left out"
                      (- (length text) ygg-git-compare-review-max-chars)))
    text))

(defun ygg-git-compare--range-label ()
  (format "A: %s ↔ B: %s  %s" (ygg-git-compare--side-label ygg-git-compare--a)
          (ygg-git-compare--side-label ygg-git-compare--b) (ygg-git-compare--range)))

(defun ygg-git-compare-review-prompt ()
  "This compare's held comments, its commits and its diff, as one message.
A comment made on another range than the one sent says which."
  (concat
   ygg-git-compare-review-instructions
   (when ygg-git-compare--comments
     (concat "\n\n<review-comments>\n"
             (mapconcat (lambda (c)
                          (concat (ygg-git-compare--where c)
                                  (unless (equal (plist-get c :range)
                                                 (ygg-git-compare--range-label))
                                    (concat " · made on " (plist-get c :range)))
                                  "\n"
                                  (replace-regexp-in-string
                                   "^" "> " (plist-get c :quote))
                                  "\n" (plist-get c :text)))
                        (reverse ygg-git-compare--comments) "\n\n")
             "\n</review-comments>"))
   "\n\n<compare>\nrepository " (ygg-git-compare-agent-path default-directory)
   "\n" (ygg-git-compare--range-label) "\n\n"
   (ygg-git-compare--capped
    (concat (ygg-git-compare--git "log" "--no-color" "--format=%h %an: %s" "-n" "200"
                                  (concat (plist-get ygg-git-compare--a :log) ".."
                                          (plist-get ygg-git-compare--b :log)))
            "\n\n"
            (apply #'ygg-git-compare--git
                   (append ygg-git-compare--quiet-args
                           (list "diff" "--no-ext-diff" "--no-color")
                           (ygg-git-compare--diff-args ygg-git-compare--plan)))
            (mapconcat (lambda (file)
                         (concat "\n" (ygg-git-compare--untracked-patch
                                       file (plist-get ygg-git-compare--plan :reverse))))
                       (plist-get ygg-git-compare--plan :untracked))))
   "\n</compare>"))

(defun ygg-git-compare--untracked-patch (file reverse)
  "Untracked FILE as a patch adding it, or removing it when REVERSE; a note
in its place when it is a nested repository or too large to send."
  (cond
   ((ygg-git-compare--nested-p file)
    (format "untracked %s: a nested repository, not diffed" file))
   ((ygg-git-compare--oversized-p file)
    (format "untracked %s: %d bytes, left out"
            file (file-attribute-size (file-attributes (expand-file-name file)))))
   (t
    (with-temp-buffer
      (apply #'process-file "git" nil '(t nil) nil
             "diff" "--no-index" "--no-ext-diff" "--no-color" "--"
             (ygg-git-compare--untracked-pair file reverse))
      (string-trim-right (buffer-string))))))

(defun ygg-git-compare--reviewers ()
  "Labels to what a review goes to: a new session under the review preset,
a live session, or a new agent as (new . NAME)."
  (append
   (list (cons (format "new: review plan (preset %s)" ygg-git-compare-review-preset)
               'review))
   (mapcar (lambda (s) (cons (format "%s · %s · %s" (aob-session-name s)
                                     (aob-session-state s)
                                     (or (aob-session-cwd s) "?"))
                             s))
           (aob-live-sessions))
   (mapcar (lambda (a) (cons (concat "new: " (car a)) (cons 'new (car a))))
           aob-acp-agents)))

(defun ygg-git-compare--spawn-review (root text)
  "A new session in ROOT under the review preset, TEXT its first turn.
Only the preset is expanded, never a name TEXT happens to mention."
  (unless (and (bound-and-true-p aob-compose-spawn-function)
               (fboundp 'ygg-aob--expand-presets))
    (user-error "The review preset needs layer-aob"))
  (let* ((aob-compose--dir root)
         (ygg-aob--draft-tree nil)
         (default-directory root)
         (at (concat "@" ygg-git-compare-review-preset))
         (preset (ygg-aob--expand-presets at)))
    (unless (and preset (string-search (format "<preset name=\"%s\">"
                                               ygg-git-compare-review-preset)
                                       preset))
      (user-error "No %s preset here" ygg-git-compare-review-preset))
    (or (let ((aob-prompt-typed t))
          (funcall aob-compose-spawn-function
                   (concat at " " text (substring preset (length at)))))
        (user-error "Could not start a review session"))))

(defun ygg-git-compare-review ()
  "Send the held review comments and the compared range to an agent."
  (interactive)
  (require 'aob)
  (require 'aob-acp)
  (with-current-buffer (ygg-git-compare--list)
    (let* ((root default-directory)
           (text (ygg-git-compare-review-prompt))
           (reviewers (ygg-git-compare--reviewers))
           (table (lambda (str pred action)
                    (if (eq action 'metadata)
                        '(metadata (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action reviewers str pred))))
           (choice (cdr (assoc (completing-read "Review by: " table nil t) reviewers)))
           (session
            (pcase choice
              ('nil (user-error "No reviewer picked"))
              ('review (ygg-git-compare--spawn-review root text))
              (`(new . ,agent)
               (let ((aob-acp-start-dir root))
                 (or (aob-acp-spawn agent) (user-error "Could not start %s" agent))))
              (_ choice))))
      (unless (eq choice 'review)
        (let ((aob-prompt-typed t))
          (aob-prompt session text)))
      (setq ygg-git-compare--comments nil)
      (aob-trace session))))

(autoload 'ygg-git-compare-visit-b "ygg-git-compare-explain" nil t)
(autoload 'ygg-git-compare-explain "ygg-git-compare-explain" nil t)

(defvar-keymap ygg-git-compare-mode-map
  "~" #'ygg-git-compare-swap
  "." #'ygg-git-compare-toggle-dots
  "#" #'ygg-git-compare-log
  "q" #'ygg-git-compare-quit
  "C" #'ygg-git-compare-comment
  "<remap> <magit-commit-add-log>" #'ygg-git-compare-comment
  ";" #'ygg-git-compare-comments
  "@" #'ygg-git-compare-review
  "'" #'ygg-git-compare-visit-b
  "N" #'ygg-git-compare-explain
  "<remap> <magit-refresh>" #'ygg-git-compare-refresh)

(define-minor-mode ygg-git-compare-mode
  "A magit buffer that is part of a compare of two sides.
\\<ygg-git-compare-mode-map>
\\[ygg-git-compare-swap] swaps the sides.
\\[ygg-git-compare-toggle-dots] switches between A...B and A..B.
\\[ygg-git-compare-log] shows the commits only in A and only in B.
\\[ygg-git-compare-refresh] diffs worktrees as they stand now.
\\[ygg-git-compare-comment] holds a review comment on the diff line at point.
\\[ygg-git-compare-comments] edits or drops a held comment.
\\[ygg-git-compare-review] sends the held comments and the compare to an agent.
\\[ygg-git-compare-visit-b] opens B's own file at the line, for the language server.
\\[ygg-git-compare-explain] starts a session explaining the change.
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
         (plan (ygg-git-compare--planned a-side b-side dots root))
         (default-directory (plist-get plan :dir)))
    (with-current-buffer
        (ygg-git-compare--display nil
          (magit-setup-buffer #'magit-diff-mode t
            (magit-buffer-diff-range (plist-get plan :range))
            (magit-buffer-diff-typearg nil)
            (magit-buffer-diff-type 'committed)
            (magit-buffer-diff-args (ygg-git-compare--args plan))
            (magit-buffer-diff-files nil)
            (magit-buffer-diff-files-suspended nil)
            (magit-git-global-arguments (ygg-git-compare--global-args))
            (magit-diff-sections-hook (ygg-git-compare--sections-hook))
            (ygg-git-compare--root root)
            (ygg-git-compare--plan plan)
            (ygg-git-compare--a-spec a)
            (ygg-git-compare--b-spec b)
            (ygg-git-compare--a a-side)
            (ygg-git-compare--b b-side)
            (ygg-git-compare--dots dots)))
      (ygg-git-compare-mode 1)
      (add-hook 'magit-refresh-buffer-hook #'ygg-git-compare--header nil t)
      (add-hook 'post-command-hook #'ygg-git-compare--schedule nil t)
      (ygg-git-compare--header)
      (ygg-git-compare--fold)
      (current-buffer))))

(defun ygg-git-compare-open (a b)
  "Compare sides A and B of this repository in the whole frame.
From inside a compare of the same sides its held review comments carry
over; of other sides, dropping them is asked first."
  (let* ((old (and (bound-and-true-p ygg-git-compare-mode) (ygg-git-compare--list)))
         (comments (and old
                        (equal a (buffer-local-value 'ygg-git-compare--a-spec old))
                        (equal b (buffer-local-value 'ygg-git-compare--b-spec old))
                        (buffer-local-value 'ygg-git-compare--comments old)))
         (_ (when (and old (not comments)) (ygg-git-compare--drop-comments old)))
         (root (or (and old (buffer-local-value 'ygg-git-compare--root old))
                   (magit-toplevel)
                   (user-error "Not in a git repository")))
         (config (if old
                     (buffer-local-value 'ygg-git-compare--window-config old)
                   (current-window-configuration)))
         (buffer (ygg-git-compare-buffer root a b)))
    (when (and old (buffer-live-p old) (not (eq old buffer)))
      (ygg-git-compare--close old))
    (with-current-buffer buffer (setq ygg-git-compare--comments comments))
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
A defaults to a pull request's base, HEAD when B is this worktree with
uncommitted changes, else the repository's default branch."
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
