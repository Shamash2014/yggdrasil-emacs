;;; ygg-git-worktree.el --- run a branch, commit or pull request in a worktree -*- lexical-binding: t; -*-

;;; Commentary:
;; From magit or a compare: check what is at point out in a worktree,
;; reusing one that already holds it, and open it as a space to run the
;; code there; and take worktrees away again with their spaces and
;; buffers, asking twice before losing changes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'magit-worktree)

(defvar ygg-git-compare--a-spec)
(defvar ygg-git-compare--b-spec)
(defvar ygg-git-compare--inflight)
(defvar ygg-git-compare-pr-ttl)
(declare-function ygg-git-compare--cached "ygg-git-compare"
                  (key ttl fetch-fn on-fresh &optional retry))
(declare-function ygg-git-compare--fetch-ref "ygg-git-compare" (remote refspec done))
(declare-function ygg-git-compare--gitdir "ygg-git-compare" ())
(declare-function ygg-git-compare--head-key "ygg-git-compare" (remote number))
(declare-function ygg-git-compare--pending-note "ygg-git-compare" (number remote))
(declare-function ygg-git-compare--remote "ygg-git-compare" ())
(declare-function ygg-git-compare-resolve "ygg-git-compare" (spec))
(declare-function ygg-git-compare--list "ygg-git-compare" ())
(declare-function ygg-git-compare--read "ygg-git-compare" (prompt cands default))
(declare-function ygg-git-compare-candidates "ygg-git-compare" (&optional with-prs))
(declare-function ygg-git-compare-open "ygg-git-compare" (a b))
(declare-function ygg-git-compare--default-branch "ygg-git-compare" ())

(declare-function ygg-wt--run "layer-git" (args &optional on-success))
(declare-function ygg-space-new-on "yggdrasil-spacetree" (dir))
(declare-function ygg-space--for-dir "yggdrasil-spacetree" (dir))
(declare-function ygg-space--goto-id "yggdrasil-spacetree" (id))
(declare-function ygg-space--id-of "yggdrasil-spacetree" (tab))
(declare-function ygg-space-close "yggdrasil-spacetree" ())
(declare-function ygg-task--collect "layer-tasks" (&optional start))
(declare-function ygg-task-run "layer-tasks" ())
(declare-function aob-live-sessions "aob" ())
(declare-function aob-session-dir "aob" (s))
(declare-function aob-session-project "aob" (s))
(declare-function aob-session-name "aob" (s))
(declare-function aob-session-state "aob" (s))
(declare-function aob-read-session "aob" (prompt &optional sessions))
(declare-function aob-trace "aob-trace" (s))

(defun ygg-git-worktree--wt-p ()
  (and (executable-find "wt") (fboundp 'ygg-wt--run)))

(defun ygg-git-worktree--norm (dir)
  (directory-file-name (file-truename (expand-file-name dir))))

;;; Worktrees this made

(defun ygg-git-worktree--file ()
  (expand-file-name "ygg-compare-worktrees.eld" (magit-gitdir nil t)))

(defun ygg-git-worktree--made ()
  "The worktrees `ygg-git-worktree-run' made in this repository, as paths."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents (ygg-git-worktree--file))
      (read (current-buffer)))))

(defun ygg-git-worktree--remember (paths)
  (with-temp-file (ygg-git-worktree--file)
    (prin1 paths (current-buffer))))

;;; Spinning one up

(defun ygg-git-worktree--main ()
  (car (car (magit-list-worktrees))))

(defun ygg-git-worktree--existing (branch sha)
  "The worktree on BRANCH, or with no BRANCH a detached one at SHA, or nil."
  (car (seq-find (pcase-lambda (`(,_path ,commit ,wt-branch ,_bare ,detached ,_l ,_p))
                   (if branch (equal wt-branch branch)
                     (and detached (equal commit sha))))
                 (if branch (magit-list-worktrees)
                   (cdr (magit-list-worktrees))))))

(defun ygg-git-worktree--path (name)
  "Where worktrunk would put the worktree NAME: beside the checkout, as REPO.NAME."
  (concat (directory-file-name (ygg-git-worktree--main))
          "." (replace-regexp-in-string "[/\\]" "-" name)))

(defun ygg-git-worktree--at-point ()
  (or (magit-section-value-if 'worktree) (user-error "No worktree at point")))

(defun ygg-git-worktree-space (dir)
  "Go to the space on the worktree DIR, opening one when there is none."
  (interactive (list (ygg-git-worktree--at-point)))
  (setq dir (ygg-git-worktree--norm dir))
  (if-let* ((tab (and (fboundp 'ygg-space--for-dir) (ygg-space--for-dir dir))))
      (ygg-space--goto-id (ygg-space--id-of tab))
    (ygg-space-new-on dir))
  dir)

(defun ygg-git-worktree--open (dir)
  "Go to DIR's space, opening one when there is none, and offer to run a task."
  (setq dir (ygg-git-worktree-space dir))
  (let ((default-directory (file-name-as-directory dir)))
    (when (and (fboundp 'ygg-task--collect)
               (ygg-task--collect)
               (y-or-n-p (format "Run a task in %s? " (abbreviate-file-name dir))))
      (ygg-task-run)))
  dir)

(defun ygg-git-worktree--made-at (dir)
  (let ((dir (ygg-git-worktree--norm dir)))
    (ygg-git-worktree--remember
     (cons dir (delete dir (ygg-git-worktree--made))))
    (ygg-git-worktree--open dir)))

(defun ygg-git-worktree--git (args on-success)
  "Run git ARGS without a refresh of the buffer; ON-SUCCESS when it exits 0."
  (let* ((magit-inhibit-refresh t)
         (proc (magit-run-git-async args)))
    (set-process-sentinel
     proc (lambda (p event)
            (magit-process-sentinel p event)
            (when (and (eq (process-status p) 'exit) (zerop (process-exit-status p)))
              (funcall on-success))))
    proc))

(defun ygg-git-worktree--here (fn)
  "FN, to be called in this repository from wherever it is called."
  (let ((root default-directory))
    (lambda () (let ((default-directory root)) (funcall fn)))))

;;;###autoload
(defun ygg-git-worktree-run (spec)
  "Check SPEC out in a worktree and open it as a space, offering a task.
At point: a compare's side B, else the worktree, pull request, branch or
commit there, else one read.  A worktree that already holds it is
reused; a branch gets a worktree on that branch, anything else one
detached at its commit."
  (interactive (list (ygg-git-worktree--spec-at-point)))
  (require 'ygg-git-compare)
  (let ((default-directory (or (magit-toplevel) (user-error "Not in a git repository"))))
    (if (eq (car spec) 'worktree)
        (ygg-git-worktree--place spec nil)
      (if-let* ((sha (plist-get (ygg-git-compare-resolve spec) :diff)))
          (ygg-git-worktree--place spec sha)
        (ygg-git-worktree--when-fetched spec)))))

(defun ygg-git-worktree--when-fetched (spec)
  "Place SPEC, a pull request whose commit is being fetched, once it is here;
say why when it is not."
  (let* ((pr (cdr spec))
         (number (plist-get pr :number))
         (remote (or (plist-get pr :remote) (ygg-git-compare--remote)))
         (key (ygg-git-compare--head-key remote number))
         (dir default-directory)
         (called nil)
         (settle (lambda (sha err)
                   (unless called
                     (setq called t)
                     (run-at-time
                      0 nil
                      (lambda ()
                        (let ((default-directory dir))
                          (condition-case failure
                              (if (and sha (magit-commit-p sha))
                                  (ygg-git-worktree--place spec sha)
                                (user-error "PR #%s not fetched: %s" number
                                            (or err "its commit is not here")))
                            (error (message "%s" (error-message-string failure))))))))))
         (sha (ygg-git-compare--cached
               key ygg-git-compare-pr-ttl
               (lambda (done)
                 (ygg-git-compare--fetch-ref remote (format "pull/%s/head" number) done))
               settle)))
    (cond ((and sha (magit-commit-p sha))
           (setq called t)
           (ygg-git-worktree--place spec sha))
          (called)
          ((gethash (cons (ygg-git-compare--gitdir) key) ygg-git-compare--inflight)
           (message "Fetching PR #%s…" number))
          (t (user-error "%s" (ygg-git-compare--pending-note number remote))))))

(defun ygg-git-worktree--place (spec sha)
  "Open SPEC in a worktree, SHA being its commit unless it is a worktree."
  (let* ((branch (pcase spec
                   (`(rev . ,rev) (and (magit-local-branch-p rev) rev))))
         (found (if (eq (car spec) 'worktree) (cdr spec)
                  (ygg-git-worktree--existing branch sha))))
    (cond
     (found (ygg-git-worktree--open found) nil)
     ((and branch (ygg-git-worktree--wt-p))
      (ygg-wt--run (list "switch" branch)
                   (ygg-git-worktree--here
                    (lambda ()
                      (when-let* ((dir (ygg-git-worktree--existing branch nil)))
                        (ygg-git-worktree--made-at dir))))))
     (t
      (let ((path (ygg-git-worktree--path
                   (pcase spec
                     (`(pr . ,pr) (format "pr-%s" (plist-get pr :number)))
                     (_ (or branch (magit-rev-abbrev sha)))))))
        (ygg-git-worktree--git
         (if branch (list "worktree" "add" path branch)
           (list "worktree" "add" "--detach" path sha))
         (ygg-git-worktree--here
          (lambda () (ygg-git-worktree--made-at path)))))))))

(defun ygg-git-worktree--spec-at-point ()
  "The side to run, as (KIND . WHAT): see `ygg-git-worktree-run'."
  (require 'ygg-git-compare)
  (or (when-let* ((list (ignore-errors (ygg-git-compare--list))))
        (buffer-local-value 'ygg-git-compare--b-spec list))
      (when-let* ((dir (magit-section-value-if 'worktree)))
        (cons 'worktree dir))
      (when-let* ((rev (magit-branch-or-commit-at-point)))
        (cons 'rev rev))
      (ygg-git-compare--read "Run in a worktree"
                             (apply-partially #'ygg-git-compare-candidates t) nil)))

;;; Taking one away

(defun ygg-git-worktree--compared-in (dir)
  "A live compare with DIR as one of its sides, or nil."
  (let ((dir (ygg-git-worktree--norm dir)))
    (and (featurep 'ygg-git-compare)
         (seq-find (lambda (b)
                     (seq-some (lambda (spec)
                                 (and (eq (car spec) 'worktree)
                                      (equal (ygg-git-worktree--norm (cdr spec)) dir)))
                               (list (buffer-local-value 'ygg-git-compare--a-spec b)
                                     (buffer-local-value 'ygg-git-compare--b-spec b))))
                   (buffer-list)))))

(defun ygg-git-worktree--read-removable ()
  "Pick one of this repository's worktrees, not the checkout nor this one;
those `ygg-git-worktree-run' made come first, the newest as the default."
  (let* ((here (ygg-git-worktree--norm (or (magit-toplevel) default-directory)))
         (paths (seq-keep (pcase-lambda (`(,path ,_c ,_b ,bare ,_d ,_l ,prunable))
                            (unless (or bare prunable
                                        (equal (ygg-git-worktree--norm path) here))
                              (ygg-git-worktree--norm path)))
                          (cdr (magit-list-worktrees))))
         (made (seq-filter (lambda (p) (member p paths))
                           (ygg-git-worktree--made)))
         (ordered (append made (seq-difference paths made)))
         (branches (mapcar (lambda (w) (cons (ygg-git-worktree--norm (car w)) (nth 2 w)))
                           (magit-list-worktrees)))
         (annotations nil)
         (annotate (lambda (path)
                     (with-memoization (alist-get path annotations nil nil #'equal)
                       (concat "  " (or (cdr (assoc path branches)) "detached")
                               (cond ((not (file-directory-p path))
                                      (propertize " missing" 'face 'warning))
                                     ((let ((default-directory (file-name-as-directory path)))
                                        (magit-git-string "--no-optional-locks" "status" "--porcelain"))
                                      (propertize " *" 'face 'warning))))))))
    (unless ordered (user-error "No worktree to remove"))
    (completing-read (format-prompt "Remove worktree" (car made))
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata (category . file)
                                      (annotation-function . ,annotate)
                                      (display-sort-function . identity))
                         (complete-with-action action ordered str pred)))
                     nil t nil nil (car made))))

(defun ygg-git-worktree--confirm (dir)
  "Ask before DIR goes; return non-nil when its changes are to be discarded.
A user error keeps it."
  (let ((changes (if (file-directory-p dir)
                     (length (let ((default-directory (file-name-as-directory dir)))
                               (magit-git-lines "--no-optional-locks" "status" "--porcelain")))
                   0))
        (name (abbreviate-file-name dir)))
    (unless (if (zerop changes)
                (y-or-n-p (format (if (file-directory-p dir)
                                      "Remove worktree %s? "
                                    "Worktree %s is missing; forget it? ")
                                  name))
              (and (y-or-n-p (format "%s has %d uncommitted change%s; remove it? "
                                     name changes (if (= changes 1) "" "s")))
                   (y-or-n-p (format "Discard those %d change%s for good? "
                                     changes (if (= changes 1) "" "s")))))
      (user-error "Kept %s" name))
    (> changes 0)))

(defun ygg-git-worktree--close (dir)
  "Close DIR's space and kill the buffers visiting its files."
  (when-let* ((tab (and (fboundp 'ygg-space--for-dir) (ygg-space--for-dir dir))))
    (ygg-space--goto-id (ygg-space--id-of tab))
    (ygg-space-close))
  (dolist (b (buffer-list))
    (when-let* ((file (buffer-file-name b)))
      (when (file-in-directory-p file dir)
        (kill-buffer b)))))

;;;###autoload
(defun ygg-git-worktree-remove (dir)
  "Remove the worktree DIR with its space and buffers, after asking.
One with uncommitted changes is asked about twice; one whose directory
is gone is forgotten, even when locked.  The space and buffers stay when
git refuses."
  (interactive (list (or (magit-section-value-if 'worktree)
                         (ygg-git-worktree--read-removable))))
  (setq dir (ygg-git-worktree--norm dir))
  (when (member dir (mapcar #'ygg-git-worktree--norm
                            (list (ygg-git-worktree--main)
                                  (or (magit-toplevel) default-directory))))
    (user-error "%s is the checkout or the worktree you are in"
                (abbreviate-file-name dir)))
  (when (ygg-git-worktree--compared-in dir)
    (user-error "%s is a side of a compare; quit the compare first"
                (abbreviate-file-name dir)))
  (let* ((force (ygg-git-worktree--confirm dir))
         (default-directory (file-name-as-directory (ygg-git-worktree--main)))
         (missing (not (file-directory-p dir)))
         (forget (ygg-git-worktree--here
                  (lambda ()
                    (ygg-git-worktree--close dir)
                    (ygg-git-worktree--remember
                     (delete dir (ygg-git-worktree--made)))))))
    (if (and (ygg-git-worktree--wt-p) (not missing))
        (ygg-wt--run (append '("remove" "--foreground" "--no-delete-branch")
                             (and force '("--force"))
                             (list dir))
                     forget)
      (ygg-git-worktree--git
       (append '("worktree" "remove")
               (cond (missing '("-f" "-f")) (force '("--force")))
               (list dir))
       forget))))

;;; In magit status

(defcustom ygg-git-worktree-status-limit 20
  "Worktrees past this many are listed in magit status without their state."
  :type 'natnum
  :group 'magit-status)

(defun ygg-git-worktree--parse-counts (lines)
  "(STAGED UNSTAGED UNTRACKED AHEAD BEHIND) from porcelain v2 LINES, AHEAD nil with no upstream."
  (let ((staged 0) (unstaged 0) (untracked 0) ahead behind)
    (dolist (line lines)
      (cond ((string-match "\\`# branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" line)
             (setq ahead (string-to-number (match-string 1 line))
                   behind (string-to-number (match-string 2 line))))
            ((string-prefix-p "? " line) (cl-incf untracked))
            ((string-prefix-p "u " line) (cl-incf unstaged))
            ((string-match "\\`[12] \\(.\\)\\(.\\)" line)
             (unless (equal (match-string 1 line) ".") (cl-incf staged))
             (unless (equal (match-string 2 line) ".") (cl-incf unstaged)))))
    (list staged unstaged untracked ahead behind)))

(defvar ygg-git-worktree--cache (make-hash-table :test #'equal))
(defvar ygg-git-worktree--commits (make-hash-table :test #'equal))
(defvar ygg-git-worktree--queue nil)
(defvar ygg-git-worktree--inflight (make-hash-table :test #'equal))
(defvar ygg-git-worktree--running 0)
(defvar-local ygg-git-worktree--renderers nil)

(defcustom ygg-git-worktree-status-ttl 30
  "Seconds a worktree's counts are trusted when its HEAD and index are unchanged."
  :type 'natnum
  :group 'magit-status)

(defcustom ygg-git-worktree-status-timeout 20
  "Seconds a worktree's git status may run before it is killed and shown unknown."
  :type 'number
  :group 'magit-status)

(defcustom ygg-git-worktree-status-jobs 3
  "How many worktrees' git status run at once."
  :type 'natnum
  :group 'magit-status)

(defun ygg-git-worktree--gitdir (dir)
  (let ((dot (expand-file-name ".git" dir)))
    (if (file-directory-p dot)
        dot
      (or (ignore-errors
            (with-temp-buffer
              (insert-file-contents dot nil 0 512)
              (when (looking-at "gitdir: \\(.+\\)$")
                (expand-file-name (match-string 1) dir))))
          dot))))

(defun ygg-git-worktree--fingerprint (dir commit)
  (if (file-remote-p dir)
      (list commit)
    (let ((gitdir (ygg-git-worktree--gitdir dir)))
      (list commit
            (file-attribute-modification-time (file-attributes (expand-file-name "HEAD" gitdir)))
            (file-attribute-modification-time (file-attributes (expand-file-name "index" gitdir)))))))

(defun ygg-git-worktree--stale-p (entry fingerprint)
  (or (null entry)
      (not (equal (plist-get entry :fp) fingerprint))
      (> (- (float-time) (plist-get entry :at)) ygg-git-worktree-status-ttl)))

(defun ygg-git-worktree--land (dir fingerprint counts)
  (let ((wanted (cdr (gethash dir ygg-git-worktree--inflight))))
    (remhash dir ygg-git-worktree--inflight)
    (puthash dir (list :fp fingerprint :counts counts :at (float-time)) ygg-git-worktree--cache)
    (run-at-time 0 nil #'ygg-git-worktree--redraw dir)
    (when (and wanted (not (equal wanted fingerprint)))
      (ygg-git-worktree--request dir wanted))))

(defun ygg-git-worktree--abandon (dir)
  (remhash dir ygg-git-worktree--inflight)
  (unless (gethash dir ygg-git-worktree--cache)
    (puthash dir (list :fp nil :counts :unknown :at (float-time)) ygg-git-worktree--cache)
    (run-at-time 0 nil #'ygg-git-worktree--redraw dir)))

(defun ygg-git-worktree--redraw (dir)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when-let* ((render (cdr (assoc dir ygg-git-worktree--renderers)))
                    (section (ygg-git-worktree--section dir)))
          (ygg-git-worktree--replace-heading section
                                             (funcall render (plist-get (gethash dir ygg-git-worktree--cache) :counts))))))))

(defun ygg-git-worktree--section (dir)
  (and (derived-mode-p 'magit-status-mode)
       magit-root-section
       (when-let* ((parent (seq-find (lambda (s) (eq (oref s type) 'worktrees))
                                     (oref magit-root-section children))))
         (seq-find (lambda (s) (equal (oref s value) dir)) (oref parent children)))))

(defun ygg-git-worktree--replace-heading (section line)
  (let* ((inhibit-read-only t)
         (start (marker-position (oref section start)))
         (props (list 'magit-section section))
         (old-end (save-excursion (goto-char start) (line-end-position)))
         (offset (and (<= start (point) old-end) (- (point) start))))
    (when-let* ((map (get-text-property start 'keymap)))
      (setq props (append props (list 'keymap map))))
    (save-excursion
      (goto-char (1+ start))
      (insert (apply #'propertize line props))
      (delete-region (point) (+ old-end (length line)))
      (delete-region start (1+ start)))
    (when offset
      (goto-char (+ start (min offset (length line)))))))

(defun ygg-git-worktree--spawn (dir fingerprint)
  (let* ((default-directory (file-name-as-directory dir))
         (out (generate-new-buffer " *ygg-wt-status*"))
         (process-environment (magit-process-environment))
         (done nil)
         (timer nil)
         (finish (lambda (counts timed-out)
                   (unless done
                     (setq done t)
                     (when timer (cancel-timer timer))
                     (cl-decf ygg-git-worktree--running)
                     (when (buffer-live-p out) (kill-buffer out))
                     (if timed-out
                         (ygg-git-worktree--abandon dir)
                       (ygg-git-worktree--land dir fingerprint counts))
                     (ygg-git-worktree--pump)))))
    (cl-incf ygg-git-worktree--running)
    (condition-case nil
        (let ((process
               (make-process
                :name "ygg-wt-status" :buffer out :noquery t :connection-type 'pipe
                :file-handler t
                :command (list (magit-git-executable) "--no-optional-locks" "status"
                               "--porcelain=v2" "--branch")
                :sentinel
                (lambda (process _event)
                  (unless (or done (process-live-p process))
                    (funcall finish
                             (and (zerop (process-exit-status process))
                                  (buffer-live-p out)
                                  (with-current-buffer out
                                    (ygg-git-worktree--parse-counts
                                     (split-string (buffer-string) "\n" t))))
                             nil))))))
          (when (and process (> ygg-git-worktree-status-timeout 0))
            (setq timer (run-at-time ygg-git-worktree-status-timeout nil
                                     (lambda ()
                                       (unless done
                                         (funcall finish nil t)
                                         (delete-process process)))))))
      (error (funcall finish nil nil)))))

(defun ygg-git-worktree--pump ()
  (while (and ygg-git-worktree--queue
              (< ygg-git-worktree--running (max 1 ygg-git-worktree-status-jobs)))
    (pcase-let ((`(,dir . ,fingerprint) (pop ygg-git-worktree--queue)))
      (ygg-git-worktree--spawn dir fingerprint))))

(defun ygg-git-worktree--request (dir fingerprint)
  (if-let* ((job (gethash dir ygg-git-worktree--inflight)))
      (setcdr job fingerprint)
    (puthash dir (cons fingerprint fingerprint) ygg-git-worktree--inflight)
    (setq ygg-git-worktree--queue (nconc ygg-git-worktree--queue (list (cons dir fingerprint))))))

(defun ygg-git-worktree--state (counts)
  (pcase-let ((`(,staged ,unstaged ,untracked ,ahead ,behind) counts))
    (string-join
     (delq nil (list (and (> staged 0)
                          (propertize (format "+%d" staged) 'font-lock-face 'success))
                     (and (> unstaged 0)
                          (propertize (format "~%d" unstaged) 'font-lock-face 'warning))
                     (and (> untracked 0)
                          (propertize (format "?%d" untracked) 'font-lock-face 'magit-dimmed))
                     (and ahead (> ahead 0) (format "↑%d" ahead))
                     (and behind (> behind 0) (format "↓%d" behind))))
     " ")))

(defun ygg-git-worktree--agents (paths)
  "Live agents as (PATH . SESSIONS), each under the deepest of PATHS holding it."
  (when (fboundp 'aob-live-sessions)
    (let (found)
      (dolist (s (aob-live-sessions))
        (when-let* ((dir (or (aob-session-dir s) (aob-session-project s)))
                    ((not (file-remote-p dir)))
                    (dir (file-name-as-directory (ygg-git-worktree--norm dir)))
                    (home (car (sort (seq-filter (lambda (p)
                                                   (string-prefix-p (file-name-as-directory p) dir))
                                                 paths)
                                     (lambda (a b) (> (length a) (length b)))))))
          (push s (alist-get home found nil nil #'equal))))
      found)))

(defun ygg-git-worktree--agent-label (s)
  (concat (aob-session-name s) " "
          (pcase (aob-session-state s)
            ('blocked (propertize "■" 'font-lock-face 'error))
            ((or 'working 'starting) (propertize "●" 'font-lock-face 'warning))
            (_ (propertize "○" 'font-lock-face 'success)))))

(defun ygg-git-worktree--parse-list (items)
  "Worktrees from `worktree list --porcelain -z' ITEMS, nil when one is bare."
  (let (worktrees worktree bare)
    (dolist (line items)
      (cond ((string-prefix-p "worktree" line)
             (setq worktree (list (substring line 9) nil nil nil nil nil nil))
             (push worktree worktrees))
            ((string-prefix-p "HEAD" line) (setf (nth 1 worktree) (substring line 5)))
            ((string-prefix-p "branch" line) (setf (nth 2 worktree) (substring line 18)))
            ((string-equal line "bare") (setq bare t))
            ((string-equal "detached" line) (setf (nth 4 worktree) t))
            ((string-prefix-p "locked" line)
             (setf (nth 5 worktree) (if (> (length line) 6) (substring line 7) t)))
            ((string-prefix-p "prunable" line)
             (setf (nth 6 worktree) (if (> (length line) 8) (substring line 9) t)))))
    (unless bare (nreverse worktrees))))

(defun ygg-git-worktree--list ()
  "Like `magit-list-worktrees', without a toplevel lookup per worktree."
  (or (and (not (file-remote-p default-directory))
           (magit-git-version>= "2.36")
           (ygg-git-worktree--parse-list
            (magit-git-items "worktree" "list" "--porcelain" "-z")))
      (magit-list-worktrees)))

(defun ygg-git-worktree--log (commits)
  (dolist (line (apply #'magit-git-lines "log" "--no-walk=unsorted"
                       "--format=%H%x1f%h%x1f%s%x1f%ct" commits))
    (pcase (split-string line "\x1f")
      (`(,full ,abbrev ,subject ,time)
       (puthash full (list abbrev subject (string-to-number time))
                ygg-git-worktree--commits)))))

(defun ygg-git-worktree--load-commits (commits)
  (when-let* ((missing (seq-filter (lambda (c) (and c (not (gethash c ygg-git-worktree--commits))))
                                   (delete-dups (copy-sequence commits)))))
    (ygg-git-worktree--log missing)
    (when-let* ((lost (seq-remove (lambda (c) (gethash c ygg-git-worktree--commits)) missing)))
      (dolist (c lost)
        (ygg-git-worktree--log (list c))
        (unless (gethash c ygg-git-worktree--commits)
          (puthash c 'unreachable ygg-git-worktree--commits))))))

(defun ygg-git-worktree--head (config here)
  (pcase-let ((`(,path ,commit ,branch ,bare) config))
    (cond (branch (propertize branch 'font-lock-face
                              (if (equal path here) 'magit-branch-current 'magit-branch-local)))
          (commit (concat (propertize "detached " 'font-lock-face 'magit-dimmed)
                          (propertize (or (car-safe (gethash commit ygg-git-worktree--commits))
                                          (substring commit 0 (min 7 (length commit))))
                                      'font-lock-face 'magit-hash)))
          (bare "(bare)")
          (t ""))))

(defun ygg-git-worktree--commit (commit)
  (pcase (gethash commit ygg-git-worktree--commits)
    (`(,hash ,subject ,time)
     (concat "    " (propertize hash 'font-lock-face 'magit-hash) " " subject "  "
             (propertize (pcase-let ((`(,cnt ,unit) (magit--age time)))
                           (format "%d %s ago" cnt unit))
                         'font-lock-face 'magit-dimmed)
             "\n"))))

(defun ygg-git-worktree--line (config head align path ctx counts)
  (pcase-let* ((`(,dir ,_commit ,_branch ,bare ,_detached ,_locked ,prunable) config)
               (`(,paths ,here ,made ,agents ,known) ctx)
               (missing (not (file-directory-p dir))))
    (concat
     head (make-string (- align (string-width head)) ?\s)
     (string-join
      (delete
       "" (list (propertize (abbreviate-file-name (directory-file-name dir)) 'font-lock-face 'shadow)
                (cond ((or bare prunable missing (not known)) "")
                      ((eq counts :unknown) (propertize "…" 'font-lock-face 'magit-dimmed))
                      (counts (ygg-git-worktree--state counts))
                      (t ""))
                (propertize
                 (string-join
                  (delq nil (list (and (equal path (car paths)) "main")
                                  (and (equal path here) "here")
                                  (and (member path made) "made")
                                  (and prunable "prunable")
                                  (and missing (not prunable) "missing")
                                  (and (fboundp 'ygg-space--for-dir)
                                       (ygg-space--for-dir path)
                                       "space")))
                  " ")
                 'font-lock-face 'magit-dimmed)
                (mapconcat #'ygg-git-worktree--agent-label
                           (cdr (assoc path agents)) " ")))
      "  "))))

;;;###autoload
(defun ygg-git-worktree-insert-section ()
  "Insert the worktrees with their state; nothing when there is only one.
Counts come from a cache and are refreshed in the background."
  (let ((worktrees (ygg-git-worktree--list)))
    (when (length> worktrees 1)
      (ygg-git-worktree--load-commits (mapcar #'cadr worktrees))
      (magit-insert-section (worktrees)
        (magit-insert-heading t "Worktrees")
        (let* ((paths (mapcar (lambda (w) (ygg-git-worktree--norm (car w))) worktrees))
               (here (ygg-git-worktree--norm (or (magit-toplevel) default-directory)))
               (made (ygg-git-worktree--made))
               (agents (ygg-git-worktree--agents paths))
               (heads (mapcar (lambda (w) (ygg-git-worktree--head
                                           (cons (ygg-git-worktree--norm (car w)) (cdr w))
                                           here))
                              worktrees))
               (align (1+ (apply #'max (mapcar #'string-width heads))))
               (shown 0))
          (setq ygg-git-worktree--renderers nil)
          (cl-mapc
           (lambda (config head path)
             (pcase-let* ((`(,dir ,commit ,_branch ,bare ,_detached ,_locked ,prunable) config)
                          (known (not (or bare prunable (not (file-directory-p dir))
                                          (> (cl-incf shown) ygg-git-worktree-status-limit))))
                          (fingerprint (and known (ygg-git-worktree--fingerprint dir commit)))
                          (entry (and known (gethash dir ygg-git-worktree--cache)))
                          (ctx (list paths here made agents known))
                          (render (lambda (counts)
                                    (ygg-git-worktree--line config head align path ctx counts))))
               (when (and known (ygg-git-worktree--stale-p entry fingerprint))
                 (ygg-git-worktree--request dir fingerprint))
               (push (cons dir render) ygg-git-worktree--renderers)
               (magit-insert-section (worktree dir t)
                 (insert (funcall render (if entry (plist-get entry :counts) :unknown)) "\n")
                 (magit-insert-heading)
                 (when (and commit (not prunable))
                   (magit-insert-section-body
                     (insert (or (ygg-git-worktree--commit commit) "")))))))
           worktrees heads paths))
        (insert ?\n)
        (ygg-git-worktree--pump)))))

(defun ygg-git-worktree-compare (dir)
  "Compare the worktree DIR against the repository's default branch."
  (interactive (list (ygg-git-worktree--at-point)))
  (require 'ygg-git-compare)
  (ygg-git-compare-open (cons 'rev (ygg-git-compare--default-branch))
                        (cons 'worktree (file-name-as-directory dir))))

(defun ygg-git-worktree-agent (dir)
  "Open the trace of an agent working in the worktree DIR."
  (interactive (list (ygg-git-worktree--at-point)))
  (let* ((dir (ygg-git-worktree--norm dir))
         (sessions (cdr (assoc dir (ygg-git-worktree--agents
                                    (mapcar (lambda (w) (ygg-git-worktree--norm (car w)))
                                            (magit-list-worktrees)))))))
    (cond ((null sessions)
           (user-error "No agent works in %s" (abbreviate-file-name dir)))
          ((cdr sessions) (aob-trace (aob-read-session "Agent: " sessions)))
          (t (aob-trace (car sessions))))))

(keymap-set magit-worktree-section-map "r" #'ygg-git-worktree-run)
(keymap-set magit-worktree-section-map "x" #'ygg-git-worktree-remove)
(keymap-set magit-worktree-section-map "=" #'ygg-git-worktree-compare)
(keymap-set magit-worktree-section-map "s" #'ygg-git-worktree-space)
(keymap-set magit-worktree-section-map "a" #'ygg-git-worktree-agent)

(provide 'ygg-git-worktree)
;;; ygg-git-worktree.el ends here
