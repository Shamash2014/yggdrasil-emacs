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
  (let* ((default-directory (or (magit-toplevel) (user-error "Not in a git repository")))
         (branch (pcase spec
                   (`(rev . ,rev) (and (magit-local-branch-p rev) rev))))
         (sha (unless (eq (car spec) 'worktree)
                (plist-get (ygg-git-compare-resolve spec) :diff)))
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
      (ygg-git-compare--read "Run in a worktree" (ygg-git-compare-candidates t) nil)))

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

(defun ygg-git-worktree--counts (dir)
  "DIR's (STAGED UNSTAGED UNTRACKED AHEAD BEHIND), AHEAD nil with no upstream."
  (let ((default-directory (file-name-as-directory dir))
        (staged 0) (unstaged 0) (untracked 0) ahead behind)
    (dolist (line (magit-git-lines "--no-optional-locks" "status"
                                   "--porcelain=v2" "--branch"))
      (cond ((string-match "\\`# branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" line)
             (setq ahead (string-to-number (match-string 1 line))
                   behind (string-to-number (match-string 2 line))))
            ((string-prefix-p "? " line) (cl-incf untracked))
            ((string-prefix-p "u " line) (cl-incf unstaged))
            ((string-match "\\`[12] \\(.\\)\\(.\\)" line)
             (unless (equal (match-string 1 line) ".") (cl-incf staged))
             (unless (equal (match-string 2 line) ".") (cl-incf unstaged)))))
    (list staged unstaged untracked ahead behind)))

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

(defun ygg-git-worktree--head (config here)
  (pcase-let ((`(,path ,commit ,branch ,bare) config))
    (cond (branch (propertize branch 'font-lock-face
                              (if (equal path here) 'magit-branch-current 'magit-branch-local)))
          (commit (concat (propertize "detached " 'font-lock-face 'magit-dimmed)
                          (propertize (magit-rev-abbrev commit) 'font-lock-face 'magit-hash)))
          (bare "(bare)")
          (t ""))))

(defun ygg-git-worktree--commit (commit)
  (pcase-let ((`(,hash ,subject ,age)
               (split-string (or (magit-git-string "log" "-1" "--format=%h%x1f%s%x1f%cr" commit)
                                 "")
                             "\x1f")))
    (when age
      (concat "    " (propertize hash 'font-lock-face 'magit-hash) " " subject "  "
              (propertize age 'font-lock-face 'magit-dimmed) "\n"))))

;;;###autoload
(defun ygg-git-worktree-insert-section ()
  "Insert the worktrees with their state; nothing when there is only one."
  (let ((worktrees (magit-list-worktrees)))
    (when (length> worktrees 1)
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
          (cl-mapc
           (lambda (config head path)
             (pcase-let* ((`(,dir ,commit ,_branch ,bare ,_detached ,_locked ,prunable) config)
                          (missing (not (file-directory-p dir))))
               (magit-insert-section (worktree dir t)
                 (insert
                  head (make-string (- align (string-width head)) ?\s)
                  (string-join
                   (delete
                    "" (list (propertize (abbreviate-file-name (directory-file-name dir)) 'font-lock-face 'shadow)
                             (if (or bare prunable missing
                                     (> (cl-incf shown) ygg-git-worktree-status-limit))
                                 ""
                               (ygg-git-worktree--state (ygg-git-worktree--counts dir)))
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
                   "  ")
                  "\n")
                 (magit-insert-heading)
                 (when (and commit (not prunable))
                   (magit-insert-section-body
                     (insert (or (ygg-git-worktree--commit commit) "")))))))
           worktrees heads paths))
        (insert ?\n)))))

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
