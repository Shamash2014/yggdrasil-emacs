;;; ygg-projects.el --- the projects sidebar, drawn with vui -*- lexical-binding: t; -*-

;;; Commentary:
;; One side window listing the projects this config knows, the selected one
;; opened to show what it is running now: agents, their subagents, the
;; commands its justfile offers, its terminals, its worktrees.
;;
;; The tree is a vui component, so a refresh is a props update that vui
;; reconciles, rather than the buffer being erased and rebuilt under the
;; cursor.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'hl-line)
(require 'project)
(require 'ygg-ui)
(require 'ygg-project-commands nil t)
(require 'ygg-git nil t)
(require 'vui nil t)

(declare-function ygg-project-roots "ygg-project-scan" (&optional refresh))
(declare-function ygg-project-candidates "ygg-project-scan" (&optional refresh))
(declare-function ygg-project-import-mark "ygg-project-scan" (root))
(defvar ygg-project-import-hook)
(declare-function aob-sessions "aob")
(declare-function aob-session-project "aob" (s))
(declare-function aob-session-state "aob" (s))
(declare-function aob-session-p "aob" (x))
(declare-function ygg-todo-session-file "ygg-todo" (s))
(declare-function ygg-todo-progress "ygg-todo" (file))
(declare-function ygg-aob-session-subagents "layer-aob" (s))
(declare-function ygg-aob-goto-space "layer-aob" (s))
(declare-function aob-session-id "aob" (s))
(declare-function aob-session-events "aob" (s))
(declare-function aob-session-started "aob" (s))
(declare-function aob-session-clock "aob" (s))
(declare-function aob-session-spend "aob" (s))
(defvar aob-trace--session-id)

(defvar ygg-projects--on-screen nil
  "Ids of the sessions a window was showing when the sidebar last drew.")

(defun ygg-projects--traced-ids ()
  "Ids of the sessions whose traces are on screen."
  (delete-dups
   (delq nil (mapcar (lambda (win)
                       (buffer-local-value 'aob-trace--session-id (window-buffer win)))
                     ;; every visible frame, not the selected one: a redraw
                     ;; from a timer must not decide the answer by which
                     ;; frame happened to be current
                     (window-list-1 nil nil 'visible)))))
(declare-function aob-acp-resumable-entries "aob-acp" ())
(declare-function aob-transcript-file "aob-transcript" (entry))
(declare-function aob-acp-resume-entry "aob-acp" (e &optional pref))
(declare-function aob-acp-archive-entry "aob-acp" (e))
(declare-function aob--call "aob" (s verb &rest args))
(declare-function aob-remove-session "aob" (s))
(declare-function aob-acp-forget-entry "aob-acp" (e))
(declare-function aob-acp-delete-session "aob-acp" (s))
(declare-function aob-transcript-view "aob-transcript" (entry))
(declare-function ygg-task--locate-justfile "layer-tasks" (start))
(declare-function ygg-task--justfile-recipes-text "layer-tasks" (file))
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function ygg-space-open "yggdrasil-spacetree" (uri))
(declare-function vui-component "vui")
(declare-function vui-mount "vui")
(declare-function vui-update-props "vui")
(declare-function vui-vstack "vui")
(declare-function vui-region "vui")
(declare-function vui-text "vui")
(declare-function yggdrasil-local-mode "yggdrasil-core")

(defgroup ygg-projects nil
  "The projects sidebar."
  :group 'tools :prefix "ygg-projects-")

(defcustom ygg-projects-width 40
  "Columns the projects sidebar takes."
  :type 'natnum :group 'ygg-projects)

(defcustom ygg-projects-limit 8
  "How many projects the sidebar lists: the busy ones, then the rest."
  :type 'natnum :group 'ygg-projects)

(defcustom ygg-projects-buffer-name "*projects*"
  "Name of the sidebar buffer."
  :type 'string :group 'ygg-projects)

(defcustom ygg-projects-gutter 10
  "Pixels of dark set between the sidebar and the window beside it."
  :type 'natnum :group 'ygg-projects)

(defface ygg-projects-base
  '((t :height 1.0))
  "Face sizing the whole sidebar and lifting it off the ground."
  :group 'ygg-projects)

(defface ygg-projects-name '((t :weight bold))
  "Face for a project name." :group 'ygg-projects)

(defface ygg-projects-path '((t :inherit shadow))
  "Face for a project path." :group 'ygg-projects)

(defface ygg-projects-count '((t :inherit shadow))
  "Face for a row's count." :group 'ygg-projects)

(defface ygg-projects-live
  '((((background dark)) :foreground "#98BB6C")
    (t :foreground "#3f6f2a"))
  "Face for the dot of a session at work, or a project holding one.
Not `success\=': the theme reads that as blue, and a dot whose whole job
is to say something is running should be green at a glance."
  :group 'ygg-projects)

(defface ygg-projects-waiting '((t :inherit warning))
  "Face for the dot of a session that is up but waiting on you."
  :group 'ygg-projects)

(defface ygg-projects-idle '((t :inherit shadow))
  "Face for the dot of a quiet project." :group 'ygg-projects)

(defface ygg-projects-card '((t :extend t))
  "The open project's extent.
Carries no fill: a surface is told apart by what it holds and by the
quiet around it, and the one filled thing on screen should be the line
you are on." :group 'ygg-projects)

(defface ygg-projects-current
  '((((background dark)) :background "#1c1c1c" :extend t)
    (t :background "#e4e4e4" :extend t))
  "Face behind the line point is on: the only fill in the sidebar.
Untinted, and a full step off the ground — a grey nudged by less than
#0a reads as a rendering artefact rather than a choice."
  :group 'ygg-projects)

(defface ygg-projects-on-screen
  '((((background dark)) :foreground "#7E9CD8" :weight bold)
    (t :foreground "#4C6FA6" :weight bold))
  "Face for the conversation whose trace is on screen.
Colour, not fill: the one filled line is the line point is on, and a
second fill beside it reads as the cursor having moved."
  :group 'ygg-projects)

(defface ygg-projects-label '((t :inherit default))
  "Face for a row's name." :group 'ygg-projects)

(defface ygg-projects-entry '((t :inherit shadow :slant italic))
  "Face for one thing a row stands for." :group 'ygg-projects)

(defface ygg-projects-accent '((t :inherit success))
  "Face of the bar marking the open project." :group 'ygg-projects)

(defface ygg-projects-gutter
  '((((background dark)) :background "#000000")
    (t :background "#dcdee3"))
  "Face of the dark run between the sidebar and its neighbour."
  :group 'ygg-projects)

(defvar ygg-projects--open nil
  "Root of the project currently expanded, or nil.")

(defvar ygg-projects--here nil
  "Root of the project the sidebar was opened from.
Held because `project-current' answers about whichever buffer a
refresh runs in, and a refresh runs in the sidebar's own.")

(defvar-local ygg-projects--instance nil
  "The mounted vui root of this sidebar.")

(defcustom ygg-projects-show-archived nil
  "Whether conversations put away are listed with the rest."
  :type 'boolean :group 'ygg-projects)

(defvar ygg-projects--open-row nil
  "Conses of (ROOT . KIND) whose entries are listed.
A set, not one at a time: opening the commands of a project is not a
reason to put its sessions away, and a row that closed itself because
you looked at another is a row you have to open twice.")

;;; What each project is running

(declare-function aob-trace--work-root "aob-trace" (s))
(declare-function aob-session-put "aob" (s key val))

(defun ygg-projects--root-of (dir roots)
  "The one of ROOTS DIR is, or is inside of, the deepest when several are."
  (when dir
    (let ((dir (file-name-as-directory
                (if (file-remote-p dir) (expand-file-name dir) (file-truename dir)))))
      (car (sort (seq-filter (lambda (r) (string-prefix-p r dir)) roots)
                 (lambda (a b) (> (length a) (length b))))))))

(defun ygg-projects--session-root (s roots)
  "The one of ROOTS session S belongs under, or nil.
Its own project first; an agent started in a folder that is no project
goes under the one it has been working in, as its tools last said."
  (or (ygg-projects--root-of (aob-session-project s) roots)
      (when (fboundp 'aob-trace--work-root)
        (let ((newest (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'tool))
                                           (aob-session-events s))
                                 :seq))
              (known (aob-session-ref s :sidebar-root)))
          (if (and known (equal (car known) newest))
              (cdr known)
            (let ((root (ygg-projects--root-of (aob-trace--work-root s) roots)))
              (aob-session-put s :sidebar-root (cons newest root))
              root))))))

(defun ygg-projects--sessions (root)
  (when (fboundp 'aob-sessions)
    (let ((roots (mapcar (lambda (r) (file-name-as-directory
                                      (expand-file-name (if (consp r) (car r) r))))
                         (ygg-projects--roots))))
      (seq-filter (lambda (s) (equal (ygg-projects--session-root s roots) root))
                  (aob-sessions)))))

(defun ygg-projects--past (root)
  "Conversations in ROOT that ended but can be picked up again.
The ones this Emacs started, and the ones the CLI left on disk before
it ever did — a project you have just taken in has a history whether
or not this Emacs was there for it."
  (let* ((hidden (and (fboundp 'aob-acp-archived-entries)
                      (mapcar (lambda (e) (plist-get e :acp-id))
                              (ignore-errors (aob-acp-archived-entries)))))
         (known (and (fboundp 'aob-acp-resumable-entries)
                     (seq-filter
                      (lambda (e)
                        (equal root (file-name-as-directory
                                     (expand-file-name (or (plist-get e :project)
                                                           (plist-get e :dir) "/")))))
                      (ignore-errors (aob-acp-resumable-entries)))))
         (ids (mapcar (lambda (e) (plist-get e :acp-id)) known))
         (awake (delq nil (mapcar (lambda (s) (aob-session-ref s :acp-id))
                                  (and (fboundp 'aob-sessions) (aob-sessions)))))
         (found (and (fboundp 'aob-transcript-found)
                     (seq-remove (lambda (e)
                                   (or (member (plist-get e :acp-id) ids)
                                       ;; put away as a persisted entry, but
                                       ;; its file is still where it was
                                       (member (plist-get e :acp-id) hidden)
                                       ;; woken: the file it was found in is
                                       ;; the file the live session is
                                       ;; writing, and one conversation is
                                       ;; one row
                                       (member (plist-get e :acp-id) awake)))
                                 (ignore-errors (aob-transcript-found root)))))
         (put-away (when ygg-projects-show-archived
                     (append
                      (seq-filter
                       (lambda (e)
                         (equal root (file-name-as-directory
                                      (expand-file-name (or (plist-get e :project)
                                                            (plist-get e :dir) "/")))))
                       (and (fboundp 'aob-acp-archived-entries)
                            (ignore-errors (aob-acp-archived-entries))))
                      (and (fboundp 'aob-transcript-found)
                           (ignore-errors
                             (aob-transcript-found root nil "archive")))))))
    (setq found (append found put-away))
    ;; newest first, whichever list it came from: a conversation is
    ;; found again by when it happened
    (sort (append known found)
          (lambda (a b) (> (or (ygg-projects--entry-ts a) 0)
                           (or (ygg-projects--entry-ts b) 0))))))

(defun ygg-projects--entry-ts (entry)
  "When ENTRY was last written to, as far as the disk knows."
  (or (plist-get entry :ts)
      (when-let* ((file (and (fboundp 'aob-transcript-file)
                             (ignore-errors (aob-transcript-file entry)))))
        (float-time (file-attribute-modification-time (file-attributes file))))))

(defun ygg-projects--agents (root)
  "What ROOT has going, and everything it could go back to.
A subagent counts as work in flight; a conversation that ended still
counts as one the project has, since it can be resumed."
  (let ((all (ygg-projects--sessions root))
        (subs (ygg-projects--subagents root))
        (past (length (ygg-projects--past root))))
    (cons (+ (seq-count (lambda (s) (not (memq (aob-session-state s) '(dead done))))
                        all)
             (car subs))
          (+ (length all) (cdr subs) past))))

(defconst ygg-projects-busy-states '(working starting)
  "States in which a session is doing something of its own.")

(defconst ygg-projects-over-states '(dead done failed)
  "States after which a session does nothing more.")

(defun ygg-projects--session-dot (payload)
  "The dot PAYLOAD gets: green at work, orange waiting, grey when over.
Only a session has a state; a subagent the agent merely reported, a
folder or a command has none and keeps the quiet dot."
  (let ((state (and (fboundp 'aob-session-p) (aob-session-p payload)
                    (aob-session-state payload))))
    (cond ((memq state ygg-projects-busy-states) 'ygg-projects-live)
          ((null state) 'ygg-projects-idle)
          ((memq state ygg-projects-over-states) 'ygg-projects-idle)
          (t 'ygg-projects-waiting))))

(defun ygg-projects--dot (root)
  "ROOT\='s own dot, the loudest of its sessions\=' dots.
Green while any of them works, orange while one is up and waiting, grey
when the project has nothing of its own running."
  (let ((states (mapcar #'aob-session-state (ygg-projects--sessions root))))
    (cond ((seq-some (lambda (st) (memq st ygg-projects-busy-states)) states)
           'ygg-projects-live)
          ((seq-some (lambda (st) (not (memq st ygg-projects-over-states))) states)
           'ygg-projects-waiting)
          (t 'ygg-projects-idle))))

(defun ygg-projects--roots ()
  "Every project root worth listing, in an order that does not move.
Neither opening a project nor an agent starting in one reorders the
list: a row that changes place under the hand that reached for it is
worse than a row in an inconvenient place.  The open project and the
one the sidebar was opened from are kept whatever the cap, since a
list that can drop the thing it is showing is a list that shows
nothing."
  (let* ((scanned (seq-uniq (mapcar #'file-name-as-directory
                                    (delq nil (ignore-errors (ygg-project-roots))))
                            #'equal))
         ;; only ones you imported: standing in a folder is not importing
         ;; it, and a row that appears because you opened a file there is
         ;; the row you removed yesterday coming back
         (pinned (seq-filter (lambda (r) (member r scanned))
                             (delq nil (list ygg-projects--here
                                             ygg-projects--open))))
         (all (append scanned (seq-remove (lambda (r) (member r scanned)) pinned)))
         (picked (seq-take all (max 1 ygg-projects-limit))))
    (dolist (r pinned)
      (unless (member r picked) (setq picked (append picked (list r)))))
    picked))

(defun ygg-projects--subagents (root)
  (if (not (fboundp 'ygg-aob-session-subagents))
      (cons 0 0)
    (let ((n (apply #'+ (mapcar (lambda (s) (length (ygg-aob-session-subagents s)))
                                (ygg-projects--sessions root)))))
      (cons n n))))

(defvar ygg-projects--buffers nil
  "Each root's buffers as (ROOT COMMANDS . TERMINALS), for one redraw.")

(defun ygg-projects--forget-buffers ()
  (setq ygg-projects--buffers nil))

(defun ygg-projects--root-buffers (root)
  "ROOT's running commands and its terminals, as one walk of the buffer list.
Three rows of every card ask the same question; the list is long, and
`expand-file-name' per buffer per row is what makes a redraw felt."
  (or (cdr (assoc root ygg-projects--buffers))
      (let (cmds terms)
        (dolist (b (buffer-list))
          (when (string-prefix-p root (expand-file-name
                                       (buffer-local-value 'default-directory b)))
            (when (provided-mode-derived-p
                   (buffer-local-value 'major-mode b) 'ghostel-mode)
              (push b terms))
            (when (and (process-live-p (get-buffer-process b))
                       (provided-mode-derived-p
                        (buffer-local-value 'major-mode b)
                        '(compilation-mode comint-mode)))
              (push b cmds))))
        (let ((cell (cons (nreverse cmds) (nreverse terms))))
          (push (cons root cell) ygg-projects--buffers)
          cell))))

(defun ygg-projects--commands (root)
  "Commands running in ROOT now, out of what its build systems offer.
The offer is read from a cache that never blocks; a scan is asked for
in the background and redraws this when it settles."
  (let ((total (length (and (fboundp 'ygg-project-commands)
                            (ygg-project-commands root))))
        (running (length (car (ygg-projects--root-buffers root)))))
    (cons running (max total running))))

(defun ygg-projects--scan-commands ()
  "Ask for a command scan of the projects on show.
Never from the render: the callback redraws, the redraw counts the
commands, and counting them would ask for another scan."
  (when (fboundp 'ygg-project-commands-refresh)
    (let ((done nil))
      (dolist (root (ygg-projects--roots))
        (ygg-project-commands-refresh
         root (lambda (_root)
                (unless done
                  (setq done t)
                  (run-at-time 0 nil #'ygg-projects-refresh))))))))

(defvar ygg-projects--docker-cache (make-hash-table :test #'equal)
  "Root to the containers docker last said were running for it.")

(defvar ygg-projects--docker-pending (make-hash-table :test #'equal))

(defun ygg-projects--docker-p (root)
  "Whether ROOT is a project docker would have anything to say about."
  (and (executable-find "docker")
       (seq-some (lambda (name) (file-exists-p (expand-file-name name root)))
                 '("docker-compose.yml" "docker-compose.yaml"
                   "compose.yml" "compose.yaml" "Dockerfile"))))

(defun ygg-projects--docker-key (name)
  (downcase (replace-regexp-in-string "[^a-z0-9]" "" (downcase (or name "")))))

(defun ygg-projects--docker-names (root)
  "What compose might have called ROOT\='s project.
The folder is one answer and often the wrong one: a worktree is named
for its branch, and the containers were started under the name of the
repository it came out of."
  (delete-dups
   (delq nil
         (list (ygg-projects--docker-key
                (file-name-nondirectory (directory-file-name root)))
               (when-let* ((common (ignore-errors
                                     (with-temp-buffer
                                       (let ((default-directory root))
                                         (when (zerop (call-process
                                                       "git" nil t nil "rev-parse"
                                                       "--path-format=absolute"
                                                       "--git-common-dir"))
                                           (string-trim (buffer-string))))))))
                 (ygg-projects--docker-key
                  (file-name-base (directory-file-name
                                   (file-name-directory
                                    (directory-file-name common))))))))))

(declare-function docker-run-docker-async-with-buffer-noninteractive "docker-core" (&rest args))
(declare-function docker-compose "docker-compose" ())

(defun ygg-projects--docker-logs (root name)
  "Follow container NAME\='s log, through docker.el where it is there.
Its plumbing knows which docker to call and how to reach a remote
host; this only says which container and how much of it."
  (let ((default-directory root))
    (require 'docker-core nil t)
    (if (fboundp 'docker-run-docker-async-with-buffer-noninteractive)
        (docker-run-docker-async-with-buffer-noninteractive
         "logs" "-f" "--tail" "200" name)
      (async-shell-command (format "docker logs --tail 200 -f %s"
                                   (shell-quote-argument name))
                           (format "*docker: %s*" name)))))

(defun ygg-projects--containers (root)
  "The containers docker last reported for ROOT."
  (gethash root ygg-projects--docker-cache))

(defun ygg-projects--scan-docker ()
  "Ask docker what is running for the projects on show, without waiting.
Only where the project has something docker-shaped in it: `docker ps\='
on every row of a list of thirty is a second of nothing."
  (dolist (root (ygg-projects--roots))
    (when (and (ygg-projects--docker-p root)
               (not (gethash root ygg-projects--docker-pending)))
      (puthash root t ygg-projects--docker-pending)
      (let ((buf (generate-new-buffer " *ygg-docker*"))
            (want (ygg-projects--docker-names root)))
        (condition-case nil
            (make-process
             :name "ygg-docker" :buffer buf :noquery t
             ;; -a: a stack that is down is still the project's, and
             ;; the row is where it is brought back up
             :command '("docker" "ps" "-a" "--format"
                        "{{.Names}}\t{{.Label \"com.docker.compose.project\"}}\t{{.Status}}\t{{.Label \"com.docker.compose.project.working_dir\"}}")
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (remhash root ygg-projects--docker-pending)
                 (let* ((out (and (buffer-live-p buf)
                                  (with-current-buffer buf (buffer-string))))
                        (was (gethash root ygg-projects--docker-cache))
                        (rows (delq nil
                                    (mapcar
                                     (lambda (line)
                                       (let* ((cols (split-string line "\t"))
                                              (name (nth 0 cols))
                                              (project (nth 1 cols))
                                              (status (nth 2 cols))
                                              (home (nth 3 cols)))
                                         (when (and name
                                                    (or (and home
                                                             (not (string-empty-p home))
                                                             (string-prefix-p
                                                              (expand-file-name root)
                                                              (file-name-as-directory
                                                               (expand-file-name home))))
                                                        (member (ygg-projects--docker-key
                                                                 (or project name))
                                                                want)))
                                           (list :name name :status status))))
                                     (split-string (or out "") "\n" t)))))
                   (when (buffer-live-p buf) (kill-buffer buf))
                   (puthash root rows ygg-projects--docker-cache)
                   (unless (equal was rows) (ygg-projects-refresh))))))
          (error (remhash root ygg-projects--docker-pending)
                 (when (buffer-live-p buf) (kill-buffer buf))))))))

(defun ygg-projects--processes (root)
  "What ROOT has running, of all it has that you can go and look at."
  (let* ((containers (ygg-projects--containers root))
         (buffers (length (cdr (ygg-projects--root-buffers root))))
         (up (seq-count (lambda (c) (string-prefix-p "Up" (or (plist-get c :status) "")))
                        containers)))
    (cons (+ buffers up) (+ buffers (length containers)))))

(defvar ygg-projects--docker-timer nil)

(defun ygg-projects--docker-tick ()
  "Ask docker again while the sidebar is on screen: stacks come and go."
  (when (ygg-projects--window)
    (ygg-projects--scan-docker)))

(unless (timerp ygg-projects--docker-timer)
  (setq ygg-projects--docker-timer
        (run-with-timer 20 20 #'ygg-projects--docker-tick)))

(defun ygg-projects--folders (root)
  "Every folder ROOT covers, the checkout itself first.
The root is where its agents already stand; the rest is what they were
additionally given to see."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (delete-dups
     (append (list root)
             (mapcar #'file-name-as-directory
                     (ignore-errors (ygg-project-folders root)))
             ;; a monorepo's members are folders of the project whether or
             ;; not anybody listed them by hand
             (mapcar #'file-name-as-directory
                     (and (fboundp 'ygg-project-workspaces)
                          (ignore-errors (ygg-project-workspaces root))))))))

(defvar ygg-projects--worktrees-cache (make-hash-table :test #'equal)
  "Each root's other worktrees as (NAME . DIR), as the last scan found them.")

(defvar ygg-projects--worktrees-pending (make-hash-table :test #'equal)
  "Roots with a worktree scan already out.")

(defun ygg-projects--worktrees-parse (out root)
  "The worktrees OUT names, ROOT's own checkout left out."
  (let ((own (file-name-as-directory (expand-file-name root)))
        (start 0)
        dirs)
    (while (string-match "^worktree \\(.*\\)$" out start)
      (setq start (match-end 0))
      (let ((dir (match-string 1 out)))
        (unless (equal (file-name-as-directory (expand-file-name dir)) own)
          (push (cons (file-name-nondirectory dir) dir) dirs))))
    (nreverse dirs)))

(defun ygg-projects--worktree-entries (root)
  "ROOT's other worktrees, from the cache only — safe on a drawing path."
  (gethash root ygg-projects--worktrees-cache))

(defun ygg-projects--scan-worktrees ()
  "Ask git what worktrees the projects on show have, without waiting.
Never from the render: a fork per card per redraw is what the command
scan already learned not to do."
  (when (fboundp 'ygg-git-async)
    (dolist (root (ygg-projects--roots))
      (unless (gethash root ygg-projects--worktrees-pending)
        (when (ignore-errors
                (ygg-git-async
                 root '("worktree" "list" "--porcelain")
                 (lambda (out exit)
                   (remhash root ygg-projects--worktrees-pending)
                   (let ((was (gethash root ygg-projects--worktrees-cache))
                         (now (and (zerop exit)
                                   (ygg-projects--worktrees-parse out root))))
                     (puthash root now ygg-projects--worktrees-cache)
                     (unless (equal was now) (ygg-projects-refresh))))))
          (puthash root t ygg-projects--worktrees-pending))))))

(defun ygg-projects--worktrees (root)
  "Worktrees ROOT has besides the checkout itself, as last scanned."
  (let ((n (length (ygg-projects--worktree-entries root))))
    (cons n n)))


;;; What a row holds, when you open it

(defun ygg-projects--session-ts (s)
  "When S last had something to say, as a number."
  (float-time (or (plist-get (car (aob-session-events s)) :ts)
                  (ignore-errors (aob-session-started s)))))

(defun ygg-projects--entries (root kind)
  "The things ROOT's KIND row stands for: (LABEL . PAYLOAD) each."
  (pcase kind
    ('agents
     (let* ((live (seq-remove (lambda (x)
                                (and (fboundp 'aob-subagent-p)
                                     (aob-subagent-p x)))
                              (ygg-projects--sessions root)))
            (groups
             (mapcar
              (lambda (s)
                ;; what it sent goes under it, marked rather than
                ;; indented: a row this narrow has no columns to spare
                (cons (ygg-projects--session-ts s)
                      (append
                       (list (cons (aob-session-name s) s))
                       (mapcar (lambda (kid)
                                 (cons (format "└ %s" (aob-session-name kid)) kid))
                               (and (fboundp 'aob-subagent-children)
                                    (aob-subagent-children s))))))
              live))
            ;; ended, but the conversation is still there to pick up
            (past (and ygg-projects-show-past
                  (mapcar (lambda (e)
                            (cons (or (ygg-projects--entry-ts e) 0)
                                  (list (cons (or (plist-get e :name)
                                                  (plist-get e :agent)
                                                  "session")
                                              e))))
                          (ygg-projects--past root)))))
       ;; running first and then ended, each newest first, with air
       ;; between: what is alive is told from what is kept without
       ;; reading a single badge, and a session's own rows go with it
       (let ((newest (lambda (cells)
                       (apply #'append
                              (mapcar #'cdr (sort cells (lambda (a b) (> (car a) (car b)))))))))
         (append (funcall newest groups)
                 (and groups past (list (cons "" 'ygg-projects-gap)))
                 (funcall newest past)))))
    ('commands (mapcar (lambda (c)
                         (cons (format "%s  %s" (plist-get c :name)
                                       (propertize (format "%s" (plist-get c :source))
                                                   'font-lock-face 'ygg-projects-count))
                               c))
                       (and (fboundp 'ygg-project-commands)
                            (ygg-project-commands root))))
    ('processes
     (append
      (mapcar (lambda (b) (cons (buffer-name b) b))
              (cdr (ygg-projects--root-buffers root)))
      (mapcar (lambda (c)
                (cons (plist-get c :name)
                      (list 'docker :name (plist-get c :name)
                            :status (plist-get c :status))))
              (ygg-projects--containers root))))
    ('folders (mapcar (lambda (d) (cons (abbreviate-file-name
                                        (directory-file-name d))
                                       d))
                      (ygg-projects--folders root)))
    ('worktrees (ygg-projects--worktree-entries root))
    (_ nil)))

(defun ygg-projects--ago (ts)
  "TS as how long ago it was, in one or two characters and a unit."
  (when ts
    (let ((secs (max 0 (- (float-time) ts))))
      (cond ((< secs 90) "now")
            ((< secs 3600) (format "%dm" (round secs 60)))
            ((< secs 86400) (format "%dh" (round secs 3600)))
            ((< secs (* 7 86400)) (format "%dd" (round secs 86400)))
            (t (format-time-string "%b %-d" ts))))))

(defun ygg-projects--docker-status (status)
  "STATUS as a badge: \"Up 21 hours (healthy)\" is a sentence, 21h is a badge."
  (let ((status (or status "")))
    (if (string-match "\\`Up \\([0-9]+\\) \\([a-z]\\)" status)
        (concat (match-string 1 status) (match-string 2 status))
      (downcase (or (car (split-string status " " t)) "")))))

(defcustom ygg-projects-entry-spacing 0.4
  "Extra line height under an entry, as a share of the line.
A sublist is a list of things, not a paragraph: what tells one row
from the next is the space around it."
  :type 'number :group 'ygg-projects)


(defun ygg-projects--entry-badge (payload)
  "What PAYLOAD has to say for itself at the right edge.
A running conversation says what it is doing; one that ended says how
long ago, since a list of six conversations from today is told apart
by when, not by that they were all today."
  (cond ((and (fboundp 'aob-session-p) (aob-session-p payload))
         (if-let* (((not (memq (aob-session-state payload) '(dead failed))))
                   ((fboundp 'ygg-todo-session-file))
                   (file (ygg-todo-session-file payload))
                   (progress (ygg-todo-progress file)))
             (format "%d/%d %s" (car progress) (cdr progress)
                     (aob-session-state payload))
           (format "%s" (aob-session-state payload))))
        ((and (consp payload) (proper-list-p payload) (plist-get payload :archived))
         "archived")
        ((and (consp payload) (eq (car payload) 'docker))
         (ygg-projects--docker-status (plist-get (cdr payload) :status)))
        ((and (consp payload) (proper-list-p payload)
              (plist-member payload :acp-id))
         (or (ygg-projects--ago (plist-get payload :ts))
             (when-let* ((file (and (fboundp 'aob-transcript-file)
                                    (ignore-errors (aob-transcript-file payload)))))
               (ygg-projects--ago
                (float-time (file-attribute-modification-time
                             (file-attributes file)))))
             "ended"))
        (t "")))

(defun ygg-projects--session-meter (s)
  "S's clock and spend, or nil before it has either."
  (when (fboundp 'aob-session-clock)
    (let ((parts (delq nil (list (aob-session-clock s) (aob-session-spend s)))))
      (and parts (mapconcat #'identity parts " ")))))

(defun ygg-projects--badge (payload)
  "PAYLOAD's badge, drawn: a session's meter muted beside its state.
The state is the colour of the state: green at work, orange waiting on
you, grey once there is nothing to wait for.  Working and idle are said
by the dot and the running clock already, so a meter stands in for them."
  (let ((state (ygg-projects--entry-badge payload)))
    (if (not (and (fboundp 'aob-session-p) (aob-session-p payload)))
        (propertize state 'font-lock-face 'ygg-projects-count)
      (let ((meter (ygg-projects--session-meter payload))
            (said (propertize state 'font-lock-face
                              (ygg-projects--session-dot payload))))
        (cond ((null meter) said)
              ((member state '("working" "idle"))
               (propertize meter 'font-lock-face 'ygg-projects-count))
              (t (concat (propertize meter 'font-lock-face 'ygg-projects-count)
                         " " said)))))))

(defcustom ygg-projects-entry-indent 4
  "Columns an entry is set in from the left.
A panel is narrow: every column spent on indentation is a column the
name does not get."
  :type 'natnum :group 'ygg-projects)

(defun ygg-projects--entry-text (label root kind payload)
  "LABEL as a row, and a second line where it does not fit.
The badge is the width the name cannot have, so the name is measured
against what is left and carries on underneath rather than being cut
where nothing can be read."
  (let* ((badge (ygg-projects--badge payload))
         ;; a rail down the indent, the way a tree says depth without
         ;; spending a column on saying nothing
         (head (concat (make-string (max 0 (- ygg-projects-entry-indent 2)) ?\s)
                       (propertize "│" 'font-lock-face 'ygg-projects-idle)
                       " "
                       (propertize "·" 'font-lock-face
                                   (ygg-projects--session-dot payload))
                       " "))
         (indent (+ ygg-projects-entry-indent 2))
         (room (max 4 (- (ygg-projects--width) indent (string-width badge) 1)))
         (fits (<= (string-width label) room))
         (cut (unless fits
                (let* ((head (truncate-string-to-width label room))
                       (space (string-match "[ /:_-][^ /:_-]*\\'" head)))
                  ;; break where the words break, unless that throws most
                  ;; of the line away
                  (if (and space (> space (* 0.5 (length head))))
                      (1+ space)
                    (length head)))))
         (first (if fits label (string-trim-right (substring label 0 cut))))
         (rest (unless fits (string-trim-left (substring label cut))))
         (wrap (- (ygg-projects--width) indent)))
    (concat
     (propertize (concat head
                         (propertize first 'font-lock-face 'ygg-projects-entry)
                         (ygg-projects--right
                          badge
                          ;; the row is as tall as it needs, unless its
                          ;; name carries on below, where the air belongs
                          (unless (and rest (not (string-empty-p rest)))
                            (+ 1.0 ygg-projects-entry-spacing))))
                 'ygg-project root 'ygg-row kind 'ygg-entry payload)
     (when (and rest (not (string-empty-p rest)))
       (concat
        "\n"
        (propertize
         (concat (make-string (max 0 (- ygg-projects-entry-indent 2)) ?\s)
                 (propertize "│" 'font-lock-face 'ygg-projects-idle)
                 (make-string (max 0 (- indent (- ygg-projects-entry-indent 1))) ?\s)
                 ;; clipped, not elided: an ellipsis is a column spent
                 ;; saying there was another column
                 (propertize (truncate-string-to-width rest wrap)
                             'font-lock-face 'ygg-projects-entry)
                 (ygg-projects--right "" (+ 1.0 ygg-projects-entry-spacing)))
         'ygg-project root 'ygg-row kind 'ygg-entry payload 'ygg-cont t))))))

(defun ygg-projects--on-screen-p (payload)
  "Non-nil when PAYLOAD is a session a window is showing."
  (and (fboundp 'aob-session-p) (aob-session-p payload)
       (member (aob-session-id payload) ygg-projects--on-screen)
       t))

(defun ygg-projects--mark-row (text face)
  "Put FACE over everything TEXT already wears.
The row is dressed in `font-lock-face\=', and a `face\=' laid over it is
not merged with it but hides it — so the mark joins the same property,
first, where what it sets wins and the rest is still read from behind
it."
  (let ((i 0) (len (length text)))
    (while (< i len)
      (let* ((end (next-single-property-change i 'font-lock-face text len))
             (old (get-text-property i 'font-lock-face text)))
        (put-text-property i end 'font-lock-face
                           (cons face (cond ((null old) nil)
                                            ((listp old) old)
                                            (t (list old))))
                           text)
        (setq i end))))
  text)

(defun ygg-projects--entry-nodes (root kind)
  (mapcar (lambda (cell)
            (if (eq (cdr cell) 'ygg-projects-gap)
                (vui-text " ")
            (let ((text (ygg-projects--entry-text (car cell) root kind (cdr cell))))
              (when (ygg-projects--on-screen-p (cdr cell))
                (ygg-projects--mark-row text 'ygg-projects-on-screen))
              (vui-text text))))
          (or (ygg-projects--entries root kind)
              (list (cons "— none —" nil)))))

;;; Drawing

(defun ygg-projects--icon (name fallback)
  "NAME as a nerd-icon, or FALLBACK.
No :face is passed: nerd-icons carries its own font family there, and
overriding it hands the glyph to a font that has no such character."
  (or (and (or (featurep 'nerd-icons) (require 'nerd-icons nil t))
           (ignore-errors (nerd-icons-mdicon name)))
      fallback))

(defun ygg-projects--width ()
  (- ygg-projects-width 4))

(defun ygg-projects--right (text &optional height)
  "TEXT pushed to the right edge, with the line padded out behind it.
The target is the window edge, not a column count: a nerd-icon glyph is
drawn wider than the one column it measures, so a numeric `align-to'
leaves icon rows long and iconless rows short, and the card behind them
ends in a ragged edge.  Two columns are held back so the last glyph
cannot spill past the text area and mark every line truncated."
  (concat (propertize " " 'display
                      `(space :align-to (- right ,(+ 2 (string-width text)))))
          text
          ;; HEIGHT makes the last glyph of the row taller than the text,
          ;; which is the only thing that opens a line up here: a
          ;; line-spacing property on the newline is ignored in this
          ;; buffer, whatever the manual says it does elsewhere
          (propertize " " 'display (if height
                                       `(space :align-to right :height ,height)
                                     '(space :align-to right)))))

(defun ygg-projects--pad ()
  "A blank line as wide as a card."
  (ygg-projects--right ""))

(defun ygg-projects--counts (live total)
  (cond ((and (zerop live) (zerop total)) "0")
        ((= live total) (format "%d" total))
        (t (format "%d/%d" live total))))

(defun ygg-projects--bar ()
  (propertize "▌" 'font-lock-face 'ygg-projects-accent))

(defun ygg-projects--row-text (icon label count root kind)
  "One row of an open project, carrying what it stands for."
  (propertize (concat "    " icon "   "
                      (propertize label 'font-lock-face 'ygg-projects-label)
                      (ygg-projects--right
                       (propertize count 'font-lock-face 'ygg-projects-count)))
              'ygg-project root 'ygg-row kind))

(defun ygg-projects--head-text (root)
  "ROOT's own line."
  (let* ((raw (file-name-nondirectory (directory-file-name root)))
         (dot (propertize "●" 'font-lock-face (ygg-projects--dot root)))
         ;; the basename is the name already: what is worth saying is
         ;; which folder it sits in
         (full (abbreviate-file-name
                (directory-file-name
                 (file-name-directory (directory-file-name root)))))
         ;; the name is never sacrificed for the path: what is left after
         ;; it decides whether the folder is spelled out, shortened to its
         ;; own basename, or left off
         (avail (- (ygg-projects--width) 7))
         (name (if (<= (string-width raw) avail) raw
                 (truncate-string-to-width raw avail nil nil t)))
         (room (- avail (string-width name) 2))
         (mark (and (fboundp 'ygg-project-import-mark)
                    (ygg-project-import-mark root)))
         (path (cond (mark (propertize mark 'font-lock-face 'ygg-projects-waiting))
                     ((<= (string-width full) room) full)
                     ((let ((short (concat "…/" (file-name-nondirectory full))))
                        (and (<= (string-width short) room) short)))
                     (t ""))))
    (propertize (concat " " dot "  "
                        (propertize name 'font-lock-face 'ygg-projects-name)
                        (ygg-projects--right
                         (propertize path 'font-lock-face 'ygg-projects-path)))
                'ygg-project root 'ygg-row 'project)))

(defun ygg-projects--row-specs (root)
  "ROOT's rows as (KIND ICON LABEL COUNT)."
  (let ((agents (ygg-projects--agents root))
        (cmds (ygg-projects--commands root))
        (terms (ygg-projects--processes root))
        (wts (ygg-projects--worktrees root)))
    ;; the same family as the rows under it: one text glyph among four
    ;; icons is the one that looks wrong, whatever its width says
    (list (list 'agents (ygg-projects--icon "nf-md-triangle_outline" "▲")
                "Sessions" (ygg-projects--counts (car agents) (cdr agents)))
          (list 'commands (ygg-projects--icon "nf-md-console" ">")
                "Commands" (ygg-projects--counts (car cmds) (cdr cmds)))
          (list 'processes (ygg-projects--icon "nf-md-console_line" "T")
                "Processes" (ygg-projects--counts (car terms) (cdr terms)))
          (list 'worktrees (ygg-projects--icon "nf-md-source_branch" "W")
                "Worktrees" (ygg-projects--counts (car wts) (cdr wts)))
          (let ((n (length (ygg-projects--folders root))))
            (list 'folders (ygg-projects--icon "nf-md-folder_multiple_outline" "F")
                  "Folders" (ygg-projects--counts n n))))))

(defun ygg-projects--rows (root)
  "ROOT's rows, the opened one followed by what it holds."
  (let ((out nil))
    (pcase-dolist (`(,kind ,icon ,label ,count) (ygg-projects--row-specs root))
      (push (vui-text (ygg-projects--row-text icon label count root kind)) out)
      (when (member (cons root kind) ygg-projects--open-row)
        (dolist (node (ygg-projects--entry-nodes root kind)) (push node out))))
    (nreverse out)))

(with-eval-after-load 'vui
  ;; expanded where vui is loaded, never before: byte-compiled in a
  ;; session that has not loaded it, `vui-defcomponent' is not yet a
  ;; macro, compiles to a function call, and the file signals on load
  (eval '(progn
    (vui-defcomponent ygg-projects-card (root open)
        "One project: a line in the list, or an opened block when OPEN.
    Nothing is drawn above the head line: a row that slid down as it
    opened would take the hand that pressed TAB with it."
        :render
        (if (not open)
            (vui-text (ygg-projects--head-text root))
          (vui-region
           :face 'ygg-projects-card
           (apply #'vui-vstack
                  (cons (vui-text (ygg-projects--head-text root))
                        (ygg-projects--rows root))))))

      (vui-defcomponent ygg-projects-view (roots open)
        "The projects as a list, the open one lifted out of it as a card."
        :render
        (apply #'vui-vstack
               (apply #'append
                      (mapcar (lambda (root)
                                (let ((card (vui-component 'ygg-projects-card
                                                           :key root :root root
                                                           :open (equal root open))))
                                  (list card)))
                              roots)))))
        t))

(defun ygg-projects--entry-key (entry)
  "Something ENTRY can be found again by after a redraw."
  (cond ((null entry) nil)
        ((and (fboundp 'aob-session-p) (aob-session-p entry))
         (aob-session-id entry))
        ((and (consp entry) (proper-list-p entry) (plist-get entry :acp-id))
         (plist-get entry :acp-id))
        ((and (consp entry) (eq (car entry) 'docker))
         (plist-get (cdr entry) :name))
        ((bufferp entry) (buffer-name entry))
        ((stringp entry) entry)
        (t (format "%s" entry))))

(defun ygg-projects--row-at-point ()
  "What the line point is on stands for: its project, row and entry.
The entry as well as the row: forty-nine conversations are forty-nine
lines of one row, and a redraw that only knows the row puts the
cursor back on the first of them."
  (list (get-text-property (line-beginning-position) 'ygg-project)
        (get-text-property (line-beginning-position) 'ygg-row)
        (ygg-projects--entry-key
         (get-text-property (line-beginning-position) 'ygg-entry))))

(defun ygg-projects--goto-row (cell)
  "Put point back on what CELL names, if it is still drawn.
The line that holds the same entry, else the first line of the same
row, else nowhere."
  (when (car cell)
    (let ((exact nil) (loose nil))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((here (line-beginning-position)))
            (when (and (equal (get-text-property here 'ygg-project) (nth 0 cell))
                       (eq (get-text-property here 'ygg-row) (nth 1 cell))
                       (not (get-text-property here 'ygg-cont)))
              (unless loose (setq loose here))
              (when (and (nth 2 cell)
                         (equal (ygg-projects--entry-key
                                 (get-text-property here 'ygg-entry))
                                (nth 2 cell))
                         (not exact))
                (setq exact here))))
          (forward-line 1)))
      (when-let* ((target (or exact loose))) (goto-char target)))))

(defun ygg-projects-refresh ()
  "Redraw the sidebar from what the projects are running now.
Point is kept on the row it was on rather than at the offset that row
used to occupy: a redraw that lands the cursor somewhere else reads as
the sidebar moving on its own."
  (interactive)
  (setq ygg-projects--on-screen (ygg-projects--traced-ids))
  (when-let* ((buf (get-buffer ygg-projects-buffer-name)))
    (with-current-buffer buf
      (when ygg-projects--instance
        (let* ((row (ygg-projects--row-at-point))
               (win (get-buffer-window buf 'visible))
               (start (and (window-live-p win) (window-start win))))
          (ygg-projects--forget-buffers)
          (vui-update-props ygg-projects--instance
                            (list :roots (ygg-projects--roots)
                                  :open ygg-projects--open))
          (ygg-projects--goto-row row)
          (ygg-projects--follow-point)
          (when (and (window-live-p win) start (<= start (point-max)))
            (set-window-start win start t)))))))

;;; Moving and acting

(defun ygg-projects--goto (dir &optional project-only)
  "Move DIR rows, stopping only on rows, or only on project heads."
  (let ((moved 0))
    (while (and (zerop moved)
                (zerop (forward-line dir))
                (not (eobp)))
      (when (and (get-text-property (line-beginning-position) 'ygg-project)
                 ;; the second line of a name is the same row, not the next
                 (not (get-text-property (line-beginning-position) 'ygg-cont))
                 (or (not project-only)
                     (eq (get-text-property (line-beginning-position) 'ygg-row)
                         'project)))
        (setq moved 1)))
    (beginning-of-line)))

(declare-function ygg-git-async "ygg-git" (root args callback))
(declare-function ygg-project-commands "ygg-project-commands" (root))
(declare-function ygg-project-commands-refresh "ygg-project-commands" (root &optional cb))
(declare-function ygg-project-commands-run "ygg-project-commands" (command))
(declare-function ygg-project-workspaces "ygg-project-commands" (root))
(declare-function ygg-project-folders "ygg-project-scan" (root))
(declare-function ygg-project-add-folder "ygg-project-scan" (root dir))
(declare-function ygg-project-add "ygg-project-scan" (dir))
(declare-function ygg-project-remove "ygg-project-scan" (dir))

(defun ygg-projects-add (dir)
  "Import DIR and show it in the sidebar.
Repositories the scan found but nobody has imported are offered by
name; anything else, and the folder picker takes over."
  (interactive
   (let* ((found (mapcar #'abbreviate-file-name
                         (and (fboundp 'ygg-project-candidates)
                              (ignore-errors (ygg-project-candidates)))))
          (pick (string-trim
                 (completing-read
                  (if found "Import (found on disk, or a folder): " "Import: ")
                  found nil nil))))
     (list (if (and (not (string-empty-p pick))
                    (file-directory-p (expand-file-name pick)))
               (expand-file-name pick)
             (read-directory-name "Project: " nil nil t)))))
  (ygg-project-add dir)
  (ygg-projects-refresh))

(defun ygg-projects-remove (&optional root)
  "Stop offering ROOT, the project on this line by default."
  (interactive)
  (let ((root (or root
                  (get-text-property (line-beginning-position) 'ygg-project)
                  (completing-read "Forget project: "
                                   (mapcar #'abbreviate-file-name
                                           (ygg-projects--roots))
                                   nil t))))
    (when (yes-or-no-p (format "Stop offering %s? "
                               (abbreviate-file-name root)))
      (ygg-project-remove root)
      (when (equal (file-name-as-directory (expand-file-name root))
                   ygg-projects--open)
        (setq ygg-projects--open nil
              ygg-projects--open-row (ygg-projects--forget-rows root)))
      (ygg-projects-refresh))))

(defun ygg-projects--forget-rows (root)
  "The open rows, less those of ROOT."
  (seq-remove (lambda (cell) (equal (car cell) root)) ygg-projects--open-row))

(defun ygg-projects--entry-at-point ()
  (get-text-property (line-beginning-position) 'ygg-entry))

(defun ygg-projects-delete ()
  "Remove what this line stands for: a session for good, or a project.
A session that ran here is a conversation, so deleting it is deleting
that; a project row is only the list this sidebar keeps."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (cond
     ((and (consp entry) (plist-member entry :acp-id))
      (aob-acp-forget-entry entry)
      (ygg-projects-refresh))
     ((aob-session-p entry)
      (aob-acp-delete-session entry)
      (ygg-projects-refresh))
     (t (ygg-projects-remove)))))

(defun ygg-projects-resume ()
  "Start the conversation on this line up again."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (unless (and (consp entry) (plist-member entry :acp-id))
      (user-error "projects: no ended conversation on this line"))
    (when (y-or-n-p (format "Resume %s? " (or (plist-get entry :name)
                                              (plist-get entry :agent))))
      (aob-acp-resume-entry entry)
      (ygg-projects-refresh))))

(defun ygg-projects--put-away (entry)
  "Put ENTRY away, by whichever record it is kept in.
A conversation this Emacs started is marked archived in the file it
keeps; one found on disk is not in that file at all, so marking it
there archives nothing and the row comes back.  Its own file moves
instead."
  (when (and (fboundp 'aob-acp-archive-entry)
             (not (plist-get entry :found)))
    (ignore-errors (aob-acp-archive-entry entry)))
  (when (fboundp 'aob-transcript-move)
    (ignore-errors (aob-transcript-move entry "archive")))
  (when (fboundp 'aob-transcript-forget) (aob-transcript-forget)))

(defun ygg-projects-archive ()
  "Put the conversation on this line away, keeping it resumable."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (cond
     ((and (consp entry) (plist-member entry :acp-id))
      (ygg-projects--put-away entry)
      (ygg-projects-refresh))
     ((aob-session-p entry)
      (user-error "projects: that one is still running — end it first"))
     (t (user-error "projects: no conversation on this line")))))

(defun ygg-projects-archive-ask ()
  "Put the conversation on this line away, after asking.
One still running is ended first and archived on the next press: an
agent holding a conversation open is the reason it cannot be filed."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (cond
     ((and (consp entry) (plist-member entry :acp-id))
      (when (y-or-n-p (format "Archive %s? "
                              (or (plist-get entry :name) "this conversation")))
        (ygg-projects--put-away entry)
        (ygg-projects-refresh)))
     ;; a conversation opened for reading is a session object with no
     ;; agent behind it: there is nothing to end, only something to file
     ((and (aob-session-p entry)
           (or (aob-session-ref entry :asleep)
               (memq (aob-session-state entry) '(done dead failed))))
      (when (y-or-n-p (format "Archive %s? " (aob-session-name entry)))
        (when-let* ((past (aob-session-ref entry :asleep)))
          (ygg-projects--put-away past))
        (aob-remove-session entry)
        (ygg-projects-refresh)))
     ((aob-session-p entry)
      (when (y-or-n-p (format "End %s? " (aob-session-name entry)))
        (ignore-errors (aob--call entry :kill))
        (ygg-projects-refresh)))
     (t (user-error "projects: no conversation on this line")))))

(defun ygg-projects--on-import ()
  "Redraw while a project is being taken in, if anyone is watching."
  (when (get-buffer-window ygg-projects-buffer-name 'visible)
    (ygg-projects-refresh)))

(add-hook 'ygg-project-import-hook #'ygg-projects--on-import)

;; a conversation's opening line arrives after the list it belongs to
(defvar aob-transcript-titles-hook)
(with-eval-after-load 'aob-transcript
  (add-hook 'aob-transcript-titles-hook #'ygg-projects--on-import))

(defun ygg-projects-forget-root (root)
  "Drop what the sidebar has cached about ROOT."
  (remhash root ygg-projects--worktrees-cache)
  (remhash root ygg-projects--docker-cache))

(defun ygg-projects-import ()
  "Take the project on this line in again.
The same import a project gets when it first arrives: for when the
answers have changed underneath, or one of them was wrong."
  (interactive)
  (let ((root (or (get-text-property (line-beginning-position) 'ygg-project)
                  ygg-projects--open
                  (user-error "projects: no project on this line"))))
    (if (fboundp 'ygg-project-import)
        (ygg-project-import root)
      (user-error "projects: nothing to import with"))))

(defvar ygg-conversations--index (make-hash-table :test #'equal)
  "Candidate string to the conversation it stands for.")

(declare-function aob-transcript-move "aob-transcript" (entry where))
(declare-function aob-acp-archive-entry "aob-acp" (e))

(defun ygg-conversations--label (entry root)
  (format "%s  %s  %s"
          (or (plist-get entry :name) (plist-get entry :agent) "session")
          (or (ygg-projects--ago (ygg-projects--entry-ts entry)) "")
          (abbreviate-file-name (directory-file-name root))))

(defun ygg-conversations--entry (candidate)
  (or (gethash candidate ygg-conversations--index)
      (user-error "projects: no conversation called that")))

(defun ygg-conversations--roots ()
  (or (and ygg-projects--open (list ygg-projects--open))
      (ygg-projects--roots)))

;;;###autoload
(defun ygg-conversations (&optional all)
  "Pick among the conversations of the open project, or ALL projects.
The point of a list is what can be done to several of it at once:
this is a completion category, so `embark-act-all\=' archives or
discards everything the filter left."
  (interactive "P")
  (clrhash ygg-conversations--index)
  (let (rows)
    (dolist (root (if all (ygg-projects--roots) (ygg-conversations--roots)))
      (dolist (entry (ygg-projects--past root))
        (let ((label (ygg-conversations--label entry root)))
          (puthash label entry ygg-conversations--index)
          (push label rows))))
    (unless rows (user-error "projects: no conversations"))
    (let ((pick (completing-read
                 "Conversation: "
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       '(metadata (category . ygg-conversation))
                     (complete-with-action action (nreverse rows) string predicate)))
                 nil t)))
      (ygg-conversation-open pick))))

(defun ygg-conversation-open (candidate)
  "Read CANDIDATE."
  (interactive "sConversation: ")
  (let ((entry (ygg-conversations--entry candidate)))
    (cond ((and (fboundp 'aob-session-p) (aob-session-p entry)) (aob-trace entry))
          ((fboundp 'aob-transcript-view) (aob-transcript-view entry))
          (t (user-error "projects: nothing to read it with")))))

(defun ygg-conversation-archive (candidate)
  "Put CANDIDATE away: kept, and out of the list."
  (interactive "sConversation: ")
  (let ((entry (ygg-conversations--entry candidate)))
    (ygg-projects--put-away entry)
    (ygg-projects-refresh)))

(defun ygg-conversation-discard (candidate)
  "Move CANDIDATE out of the way, into a folder nothing reads."
  (interactive "sConversation: ")
  (let ((entry (ygg-conversations--entry candidate)))
    (when (fboundp 'aob-transcript-move) (aob-transcript-move entry "discarded"))
    (ygg-projects-refresh)))

(defcustom ygg-projects-show-past t
  "Whether the sessions row lists ended conversations under the live ones.
Off, it lists only what is running or waiting: the row is then what is
going on, not the history of the project."
  :type 'boolean :group 'ygg-projects)

(defun ygg-projects-toggle-past ()
  "List ended conversations under the live sessions, or stop listing them."
  (interactive)
  (setq ygg-projects-show-past (not ygg-projects-show-past))
  (ygg-projects-refresh)
  (message "projects: old sessions %s" (if ygg-projects-show-past "shown" "hidden")))

(defun ygg-projects-toggle-archived ()
  "Show the conversations put away, or stop showing them."
  (interactive)
  (setq ygg-projects-show-archived (not ygg-projects-show-archived))
  (when (fboundp 'aob-transcript-forget) (aob-transcript-forget))
  (ygg-projects-refresh)
  (message "projects: archived conversations %s"
           (if ygg-projects-show-archived "shown" "hidden")))

(defun ygg-projects-rescan ()
  "Redraw, and look again for what the projects can run."
  (interactive)
  (when (fboundp 'aob-transcript-forget) (aob-transcript-forget))
  (ygg-projects-refresh)
  (ygg-projects--scan-commands)
  (ygg-projects--scan-worktrees)
  (ygg-projects--scan-docker))

(defun ygg-projects-first ()
  "Go to the first row."
  (interactive)
  (goto-char (point-min))
  (unless (and (get-text-property (line-beginning-position) 'ygg-project)
               (not (get-text-property (line-beginning-position) 'ygg-cont)))
    (ygg-projects--goto 1)))

(defun ygg-projects-last ()
  "Go to the last row."
  (interactive)
  (goto-char (point-max))
  (ygg-projects--goto -1))

(defun ygg-projects--open-p ()
  "Whether the row point is on is showing what it stands for."
  (let ((root (get-text-property (line-beginning-position) 'ygg-project))
        (kind (get-text-property (line-beginning-position) 'ygg-row)))
    (cond ((null root) nil)
          ((get-text-property (line-beginning-position) 'ygg-entry) nil)
          ((eq kind 'project) (equal root ygg-projects--open))
          (t (and (member (cons root kind) ygg-projects--open-row) t)))))

(defun ygg-projects-open-row ()
  "Open what this line stands for, or go into it when it is a thing."
  (interactive)
  (cond ((get-text-property (line-beginning-position) 'ygg-entry)
         (ygg-projects-visit))
        ((not (ygg-projects--open-p)) (ygg-projects-toggle))))

(defun ygg-projects-close-row ()
  "Close what this line stands for, else go up to the project above it."
  (interactive)
  (if (ygg-projects--open-p)
      (ygg-projects-toggle)
    (ygg-projects--goto -1 t)))

(defun ygg-projects-forward-rows (n)
  "Move N rows, the way a half page moves a list."
  (dotimes (_ (abs n)) (ygg-projects--goto (if (< n 0) -1 1))))

(defun ygg-projects-down-half () (interactive) (ygg-projects-forward-rows 5))
(defun ygg-projects-up-half () (interactive) (ygg-projects-forward-rows -5))

(defun ygg-projects-next () (interactive) (ygg-projects--goto 1))
(defun ygg-projects-prev () (interactive) (ygg-projects--goto -1))
(defun ygg-projects-next-project () (interactive) (ygg-projects--goto 1 t))
(defun ygg-projects-prev-project () (interactive) (ygg-projects--goto -1 t))

(defun ygg-projects-toggle ()
  "Open what this line stands for: a project's rows, or a row's entries.
The line keeps its place on screen; what opens, opens below it."
  (interactive)
  (let* ((root (get-text-property (line-beginning-position) 'ygg-project))
         (kind (get-text-property (line-beginning-position) 'ygg-row))
         (win (get-buffer-window (current-buffer) 'visible))
         (above (and (window-live-p win)
                     (- (line-number-at-pos (point))
                        (line-number-at-pos (window-start win))))))
    (cond
     ((null root) nil)
     ((eq kind 'project)
      (setq ygg-projects--open (unless (equal root ygg-projects--open) root)))
     (t (let ((cell (cons root kind)))
          (setq ygg-projects--open-row
                (if (member cell ygg-projects--open-row)
                    (remove cell ygg-projects--open-row)
                  (cons cell ygg-projects--open-row))))))
    (ygg-projects-refresh)
    ;; and when the list is long enough to scroll, hold the row against
    ;; the same screen line rather than letting the view slide
    (when (and (window-live-p win) above)
      (save-excursion
        (forward-line (- above))
        (set-window-start win (line-beginning-position) t)))))

;; defined above its uses: a macro the compiler has not seen yet is
;; compiled as a function call, and answers one with "invalid function"
(defmacro ygg-projects--keeping (&rest body)
  "Run BODY, and put the sidebar back if BODY took it away.
Opening a space restores a window configuration recorded without this
side window, so acting on a row would otherwise close the sidebar the
row was picked from."
  (declare (indent 0) (debug t))
  `(let ((had (and (get-buffer-window ygg-projects-buffer-name 'visible) t)))
     (prog1 (progn ,@body)
       (when-let* ((had)
                   (buf (get-buffer ygg-projects-buffer-name))
                   ((not (get-buffer-window buf 'visible)))
                   (win (ygg-projects--display buf)))
         (set-window-dedicated-p win t)
         (with-current-buffer buf (ygg-projects--trim-window))))))

(defun ygg-projects-open ()
  "Open the project on this line as its own space."
  (interactive)
  (let ((root (get-text-property (line-beginning-position) 'ygg-project)))
    (unless root (user-error "projects: no project on this line"))
    (setq ygg-projects--open root)
    (ygg-projects-refresh)
    (ygg-projects--keeping
      (if (fboundp 'ygg-space-open)
          (ygg-space-open (directory-file-name root))
        (dired root)))))

(defun ygg-projects-visit ()
  "Act on the row under point."
  (interactive)
  (let* ((root (get-text-property (line-beginning-position) 'ygg-project))
         (row (get-text-property (line-beginning-position) 'ygg-row))
         (entry (get-text-property (line-beginning-position) 'ygg-entry)))
    (unless root (user-error "projects: nothing on this line"))
    (ygg-projects--keeping
    (if entry
        (pcase row
          ('agents
           (cond
            ;; a persisted conversation is a plist; a live one a struct.
            ;; reading what was said costs nothing; resuming starts an
            ;; agent, so that is a different key
            ((and (consp entry) (plist-member entry :acp-id))
             (aob-transcript-view entry))
            (t (when (fboundp 'ygg-aob-goto-space) (ygg-aob-goto-space entry))
               (aob-trace entry))))
          ('commands (if (fboundp 'ygg-project-commands-run)
                         (ygg-project-commands-run entry)
                       (let ((default-directory root))
                         (compile (format "just %s" entry)))))
          ('folders (dired entry))
          ('processes
           (if (and (consp entry) (eq (car entry) 'docker))
               (ygg-projects--docker-logs root (plist-get (cdr entry) :name))
             (pop-to-buffer entry)))
          ('worktrees (if (fboundp 'ygg-space-open)
                          (ygg-space-open entry)
                        (dired entry))))
      (pcase row
      ('project (ygg-projects-open))
      ('agents
       ;; the picker where there is something to pick, the list
       ;; otherwise: a row that answers a keypress with an error is a
       ;; row that looks broken
       (condition-case nil
           (if (and (fboundp 'ygg-aob-pick)
                    (fboundp 'aob-live-sessions) (aob-live-sessions))
               (ygg-aob-pick)
             (ygg-projects-toggle))
         (error (ygg-projects-toggle))))
      ('commands (let ((default-directory root))
                   (if (fboundp 'ygg-task-run) (call-interactively #'ygg-task-run)
                     (user-error "projects: no task runner"))))
      ('folders (call-interactively #'ygg-project-add-folder))
      ('processes
       (let ((default-directory root))
         ;; where there is a stack, the stack is what the row is about
         (cond ((and (ygg-projects--docker-p root) (fboundp 'docker-compose))
                (call-interactively #'docker-compose))
               ((fboundp 'ghostel) (call-interactively #'ghostel))
               (t (user-error "projects: no terminal")))))
      ('worktrees (let ((default-directory root))
                    (cond ((fboundp 'ygg-wt-list) (call-interactively #'ygg-wt-list))
                          ((fboundp 'magit-worktree)
                           (call-interactively #'magit-worktree))
                          (t (user-error "projects: no worktree list"))))))))))

(defvar ygg-projects-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ygg-projects-visit)
    (define-key map (kbd "<return>") #'ygg-projects-visit)
    (define-key map (kbd "TAB") #'ygg-projects-toggle)
    ;; a GUI frame sends <tab>, and vui-mode binds it to vui-forward:
    ;; without this the disclosure key never reaches us
    (define-key map (kbd "<tab>") #'ygg-projects-toggle)
    (define-key map (kbd "<backtab>") #'ygg-projects-toggle)
    (define-key map (kbd "S-<tab>") #'ygg-projects-toggle)
    (define-key map "j" #'ygg-projects-next)
    (define-key map "k" #'ygg-projects-prev)
    (define-key map "J" #'ygg-projects-next-project)
    (define-key map "K" #'ygg-projects-prev-project)
    ;; g is the goto prefix everywhere else, so it is one here too
    (define-key map "g g" #'ygg-projects-first)
    (define-key map "g r" #'ygg-projects-rescan)
    (define-key map "G" #'ygg-projects-last)
    ;; the movements normal state has, answering in rows: a panel one
    ;; column wide has nothing to say to a character left or right, so
    ;; they close and open instead, the way a tree does
    (define-key map "h" #'ygg-projects-close-row)
    (define-key map "l" #'ygg-projects-open-row)
    (define-key map (kbd "C-d") #'ygg-projects-down-half)
    (define-key map (kbd "C-u") #'ygg-projects-up-half)
    (define-key map "}" #'ygg-projects-next-project)
    (define-key map "{" #'ygg-projects-prev-project)
    (define-key map "+" #'project-switch-project)
    (define-key map "A" #'ygg-projects-add)
    (define-key map "I" #'ygg-projects-import)
    (define-key map "z" #'ygg-projects-toggle-past)
    (define-key map "Z" #'ygg-projects-toggle-archived)
    (define-key map "D" #'ygg-projects-delete)
    (define-key map "-" #'ygg-projects-archive)
    (define-key map "x" #'ygg-projects-archive-ask)
    (define-key map "R" #'ygg-projects-resume)
    (define-key map "q" #'ygg-projects-close)
    map)
  "The sidebar's own verbs, ahead of yggdrasil's normal state.")

(defvar-local ygg-projects--modal nil)
(defvar ygg-projects--emulation-alist
  (list (cons 'ygg-projects--modal ygg-projects-map)))
(add-to-list 'emulation-mode-map-alists 'ygg-projects--emulation-alist)

(defun ygg-projects--follow-point ()
  "Stand the sidebar in the project point is on.
Whatever is started from here — magit, a terminal, a compose, a find —
starts in that project, not in the folder the sidebar was made in."
  (when-let* ((root (get-text-property (line-beginning-position) 'ygg-project))
              ((stringp root))
              ((file-directory-p root)))
    (setq default-directory (file-name-as-directory root))))

(defun ygg-projects--drop-selection ()
  "Moving is not selecting: normal state leaves no region behind,
and no state gets to put its cursor back."
  (setq cursor-type nil)
  (when (and (bound-and-true-p ygg--normal-p) mark-active)
    (set-mark (point))
    (deactivate-mark)))

(defun ygg-projects--window (&optional frame)
  "The sidebar\='s own window on FRAME: the side window down the left.
A copy of the buffer in an ordinary window — a restored configuration,
a stray `switch-to-buffer\=' — is not the sidebar, whatever it shows."
  (when-let* ((buf (get-buffer ygg-projects-buffer-name)))
    (seq-find (lambda (w) (eq (window-parameter w 'window-side) 'left))
              (get-buffer-window-list buf nil (or frame (selected-frame))))))

(defun ygg-projects--display (buf)
  "Put BUF in the sidebar's own window and return it.
The width is preserved once set: `balance-windows\=' counts a side
window as one more pane to share the frame out between, and hands a
panel a third of the screen."
  (dolist (w (get-buffer-window-list buf nil t))
    (unless (window-parameter w 'window-side)
      (ignore-errors (ygg-projects--dismiss w))))
  (let ((win (display-buffer buf `((display-buffer-in-side-window)
                                   (side . left) (slot . 0)
                                   (window-width . ,ygg-projects-width)
                                   (window-parameters
                                    . ((no-delete-other-windows . t)))))))
    (when (window-live-p win) (window-preserve-size win t t))
    win))

(defun ygg-projects-keep-open (fn &rest args)
  "Call FN with ARGS and put the sidebar back if it went away.
Usable as :around advice on anything that rearranges windows."
  (ygg-projects--keeping (apply fn args)))

(defun ygg-projects--dismiss (win)
  "Take the sidebar off WIN.
A window that is its frame's last cannot be deleted, and a dedicated one
refuses to change buffer, so it is undedicated and handed back what it
showed before.  Its side parameters go too, or the next open reclaims
the whole frame instead of a side window."
  (when (window-live-p win)
    (if (not (eq win (frame-root-window win)))
        (delete-window win)
      (set-window-dedicated-p win nil)
      (dolist (p '(window-side window-slot no-delete-other-windows))
        (set-window-parameter win p nil))
      (switch-to-prev-buffer win)
      (when (and (window-live-p win)
                 (eq (window-buffer win) (get-buffer ygg-projects-buffer-name)))
        (set-window-buffer win (other-buffer (window-buffer win)))))))

(defvar ygg-projects--sizing nil
  "Non-nil while the sidebar is putting its own width back.")

(defvar ygg-projects--wanted nil
  "Non-nil while the sidebar is meant to be on screen.
Set when you open it, cleared only when you close it yourself.  Nothing
else decides that a panel you asked for is gone: a space restoring a
window configuration, a session load, a compose box making room.")

(defvar ygg-projects--restoring nil
  "Non-nil while the sidebar is putting itself back, to not recurse.")

(defun ygg-projects--restore (&rest _)
  "Put the sidebar back on a frame that lost it without being asked."
  (unless (or ygg-projects--restoring
              (not ygg-projects--wanted)
              (frame-parent)
              (minibufferp))
    (when-let* ((buf (get-buffer ygg-projects-buffer-name))
                ((not (ygg-projects--window))))
      (let ((ygg-projects--restoring t))
        (when-let* ((win (ignore-errors (ygg-projects--display buf))))
          (set-window-dedicated-p win t)
          (with-current-buffer buf (ygg-projects--trim-window)))))))

(add-hook 'window-configuration-change-hook #'ygg-projects--restore)

(defun ygg-projects--follow-trace (&rest _)
  "Redraw when another conversation comes on screen, so the fill follows it."
  (unless (equal (ygg-projects--traced-ids) ygg-projects--on-screen)
    (ygg-projects-refresh)))

(add-hook 'window-configuration-change-hook #'ygg-projects--follow-trace)

(defvar ygg-projects--redraw-timer nil)

(defun ygg-projects--redraw-soon (&rest _)
  "Redraw shortly: a burst of session changes is one redraw, not fifty.
A plain timer and not an idle one — an idle timer made while Emacs is
already idle waits for the next keystroke, and agents finish their
turns while nobody is typing."
  (unless (timerp ygg-projects--redraw-timer)
    (setq ygg-projects--redraw-timer
          (run-with-timer 0.3 nil
                          (lambda ()
                            (setq ygg-projects--redraw-timer nil)
                            (ygg-projects-refresh))))))

(defvar aob-session-created-hook)
(defvar aob-session-removed-hook)
(defvar aob-state-change-hook)
(defvar aob-meter-change-hook)
(with-eval-after-load 'aob
  (add-hook 'aob-session-created-hook #'ygg-projects--redraw-soon)
  (add-hook 'aob-session-removed-hook #'ygg-projects--redraw-soon)
  (add-hook 'aob-state-change-hook #'ygg-projects--redraw-soon)
  (add-hook 'aob-meter-change-hook #'ygg-projects--redraw-soon))

(defun ygg-projects-close ()
  "Close the sidebar, and mean it: it stays closed until you open it."
  (interactive)
  (setq ygg-projects--wanted nil)
  (when-let* ((win (get-buffer-window ygg-projects-buffer-name 'visible)))
    (ygg-projects--dismiss win)))

(defun ygg-projects--trim-window ()
  "Nothing to the left of a card, a dark run to its right, and the width it was given.
`balance-windows\=' counts the sidebar as one more pane to share the
frame out between, and a panel is not a pane: whatever moves the
windows around, the width it was opened at is the width it keeps."
  (unless ygg-projects--sizing
    (let ((ygg-projects--sizing t))
      (dolist (win (get-buffer-window-list (current-buffer) nil t))
        (set-window-fringes win 0 ygg-projects-gutter)
        (unless (zerop (window-hscroll win)) (set-window-hscroll win 0))
        (when (window-parameter win 'window-side)
          (let ((delta (- ygg-projects-width (window-total-width win))))
            (unless (zerop delta)
              (ignore-errors (window-resize win delta t t))))
          (window-preserve-size win t t))))))

(defun ygg-projects--setup (buf)
  "Make BUF read like a sidebar and answer to the modal layer."
  (with-current-buffer buf
    (setq truncate-lines t)
    ;; a panel has no columns off to the right: point reaching the end of
    ;; a truncated row must not slide the whole sidebar sideways
    (setq-local auto-hscroll-mode nil)
    (setq-local cursor-type nil)
    (setq-local left-margin-width 0)
    (setq-local line-spacing 0)
    (setq-local fringe-indicator-alist (cons '(truncation nil nil)
                                             fringe-indicator-alist))
    (setq ygg-projects--modal t)
    ;; a panel is not a document: it has no position, no encoding and no
    ;; name worth repeating under itself
    (setq-local mode-line-format nil)
    (buffer-face-set 'ygg-projects-base)
    ;; nothing paints a selection here.  remapping these to a flat colour
    ;; punches that colour through whatever card the line sits on, so they
    ;; are cut off from their global definitions and contribute nothing
    (dolist (f '(region ygg-secondary-selection ygg-secondary-cursor
                        ygg-sel ygg-fake-cursor))
      (when (facep f) (face-remap-set-base f nil)))
    (face-remap-add-relative 'fringe 'ygg-projects-gutter)
    (when (fboundp 'yggdrasil-local-mode) (yggdrasil-local-mode 1))
    ;; after the modal layer, which sets a cursor per state
    (setq-local cursor-type nil)
    (setq-local hl-line-face 'ygg-projects-current)
    (hl-line-mode 1)
    (add-hook 'post-command-hook #'ygg-projects--drop-selection 90 t)
    (add-hook 'post-command-hook #'ygg-projects--follow-point nil t)
    (add-hook 'window-configuration-change-hook #'ygg-projects--trim-window nil t)))

;;;###autoload
(defun ygg-projects-sidebar ()
  "Show the projects sidebar, or close it when it is already up."
  (interactive)
  (unless (or (featurep 'vui) (require 'vui nil t))
    (user-error "projects: vui is not installed"))
  (if-let* ((win (get-buffer-window ygg-projects-buffer-name 'visible)))
      ;; already up, on this frame or another: the key closes it rather
      ;; than opening a second one
      (ygg-projects-close)
    (when-let* ((pr (project-current nil)))
      (setq ygg-projects--here
            (file-name-as-directory (expand-file-name (project-root pr)))))
    (unless ygg-projects--open (setq ygg-projects--open ygg-projects--here))
    (let ((buf (get-buffer ygg-projects-buffer-name)))
      (unless (and buf (buffer-local-value 'ygg-projects--instance buf))
        ;; vui-mount ends in `switch-to-buffer', which would leave the
        ;; sidebar showing in the main window as well as its own
        (ygg-projects--forget-buffers)
        (let ((inst (save-window-excursion
                      (vui-mount (vui-component 'ygg-projects-view
                                                :roots (ygg-projects--roots)
                                                :open ygg-projects--open)
                                 ygg-projects-buffer-name))))
          (setq buf (get-buffer ygg-projects-buffer-name))
          (with-current-buffer buf (setq ygg-projects--instance inst))))
      (ygg-projects--setup buf)
      (ygg-projects-refresh)
      (ygg-projects--scan-commands)
      (ygg-projects--scan-worktrees)
      (ygg-projects--scan-docker)
      (setq ygg-projects--wanted t)
      (let ((win (ygg-projects--display buf)))
        ;; dedicated: whatever the sidebar opens goes to the main area,
        ;; never into the sidebar's own window
        (when (window-live-p win)
          (set-window-dedicated-p win t)
          ;; one sidebar: any other window that ended up on this buffer goes
          (dolist (other (get-buffer-window-list buf nil 'visible))
            (unless (eq other win) (ygg-projects--dismiss other)))
          (select-window win)))
      (ygg-projects--trim-window))))

;;; Sessions — the sidebar is laid over a layout, never saved inside one

(defvar easysession-before-save-hook)
(defvar easysession-after-save-hook)
(defvar easysession-after-load-hook)
(declare-function easysession-add-save-handler "easysession" (handler-fn))
(declare-function easysession-add-load-handler "easysession" (handler-fn))

(defun ygg-projects--session-save (buffers)
  "The sidebar's state as easysession keeps it; BUFFERS go on untouched.
The next handler is handed what this one leaves, so all of them are left."
  `((key . "ygg-projects")
    (value . ((wanted . ,(and ygg-projects--wanted t))
              (open . ,ygg-projects--open)
              (open-row . ,ygg-projects--open-row)
              (archived . ,ygg-projects-show-archived)
              (past . ,ygg-projects-show-past)))
    (remaining-buffers . ,buffers)))

(defun ygg-projects--session-load (session-data)
  "Take the sidebar's state back from SESSION-DATA.
A session saved before the sidebar was kept has nothing to say, and the
sidebar stays as it is."
  (when-let* ((state (assoc-default "ygg-projects" session-data)))
    (setq ygg-projects--wanted (alist-get 'wanted state)
          ygg-projects--open (alist-get 'open state)
          ygg-projects--open-row (alist-get 'open-row state)
          ygg-projects-show-archived (alist-get 'archived state)
          ygg-projects-show-past (alist-get 'past state t))))

(defun ygg-projects--stray-window-p (win)
  "Non-nil when WIN shows the sidebar but is not the sidebar\='s own window.
A layout saved with the sidebar in it brings the buffer back in an
ordinary window, or a placeholder of it when the buffer is gone."
  (let ((name (buffer-name (window-buffer win))))
    (and (string-match-p (concat "\\`[ *]*\\(Old buffer \\)?"
                                 (regexp-quote ygg-projects-buffer-name))
                         name)
         (not (eq (window-parameter win 'window-side) 'left)))))

(defvar ygg-projects--saved-frames nil
  "Frames the sidebar was taken out of for a save, to be put back in.")

(defun ygg-projects--before-session-save ()
  "Take the sidebar out of every frame, so no saved layout holds it.
Runs after the layer that keeps each frame\='s configuration to put back
once the save is written, so the live frame never loses it."
  (setq ygg-projects--saved-frames nil)
  (let ((ygg-projects--restoring t))
    (dolist (frame (frame-list))
      (when-let* ((win (ygg-projects--window frame))
                  ((not (eq win (frame-root-window frame)))))
        (push frame ygg-projects--saved-frames)
        (ignore-errors (delete-window win))))))

(defun ygg-projects--after-session-save ()
  "Put the sidebar back in the frames it was taken from, if nothing else did."
  (dolist (frame ygg-projects--saved-frames)
    (when (frame-live-p frame)
      (with-selected-frame frame (ygg-projects--restore))))
  (setq ygg-projects--saved-frames nil))

(defun ygg-projects--after-session-load ()
  "Clear what a stale layout restored of the sidebar, then show it as saved."
  (dolist (frame (frame-list))
    (dolist (win (window-list frame 'no-minibuf))
      (when (and (window-live-p win)
                 (not (eq win (frame-root-window frame)))
                 (ygg-projects--stray-window-p win))
        (ignore-errors (delete-window win)))))
  (ygg-projects--restore)
  (ygg-projects-refresh))

(with-eval-after-load 'easysession
  (easysession-add-save-handler #'ygg-projects--session-save)
  (easysession-add-load-handler #'ygg-projects--session-load)
  (add-hook 'easysession-before-save-hook #'ygg-projects--before-session-save 90)
  (add-hook 'easysession-after-save-hook #'ygg-projects--after-session-save 90)
  (add-hook 'easysession-after-load-hook #'ygg-projects--after-session-load 90))

(provide 'ygg-projects)
;;; ygg-projects.el ends here
