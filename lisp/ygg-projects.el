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
(require 'cl-lib)
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
(declare-function ygg-project-top-roots "ygg-project-scan" ())
(declare-function ygg-project-children "ygg-project-scan" (root))
(declare-function ygg-project-umbrella-of "ygg-project-scan" (root))
(declare-function ygg-project-try-umbrella "ygg-project-scan" (dir))
(declare-function ygg-project-move "ygg-project-scan" (root n))
(declare-function ygg-project-move-child "ygg-project-scan" (child n))
(defvar ygg-project-import-hook)
(declare-function aob-sessions "aob")
(declare-function aob-session-project "aob" (s))
(declare-function aob-session-state "aob" (s))
(declare-function aob-session-p "aob" (x))
(declare-function aob-session-dir "aob" (s))
(declare-function aob-subagent-p "aob-subagent" (s))
(declare-function ygg-todo-session-file "ygg-todo" (s))
(declare-function ygg-todo-progress "ygg-todo" (file))
(declare-function ygg-aob-goto-space "layer-aob" (s))
(declare-function aob-session-id "aob" (s))
(declare-function aob-session-events "aob" (s))
(declare-function aob-session-started "aob" (s))
(declare-function aob-session-clock "aob" (s))
(declare-function aob-transcript-found "aob-transcript" (project &optional agent where))
(declare-function aob-session-quiet "aob" (s))
(declare-function aob-session-spend "aob" (s))
(declare-function aob-schedule-for "aob-schedule" (acp-id))
(declare-function aob-schedule-for-project "aob-schedule" (root))
(declare-function aob-schedule-read "aob-schedule" (s))
(declare-function aob-schedule "aob-schedule" (s prompt when))
(declare-function aob-schedule-list "aob-schedule" ())
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
(declare-function aob-transcript--title-cached "aob-transcript" (file))
(declare-function aob-acp-list-sessions "aob-acp" (agent project then))
(declare-function aob-acp-merge-listed "aob-acp" (entries listed agent project))
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
  '((((background dark)) :background "#332f29" :extend t)
    (t :background "#d5cebe" :extend t))
  "Face behind the line point is on: the only fill in the sidebar.
The paper's own hue, two full steps off the ground — one step read as a
faint wash rather than the line you are on."
  :group 'ygg-projects)

(defface ygg-projects-on-screen '((t :inherit default :weight bold))
  "Face for the conversation whose trace is on screen.
Weight, not fill: the one filled line is the line point is on, and a
second fill beside it reads as the cursor having moved."
  :group 'ygg-projects)

(defface ygg-projects-label '((t :inherit default))
  "Face for a row's name." :group 'ygg-projects)

(defface ygg-projects-entry '((t :inherit shadow :slant italic))
  "Face for one thing a row stands for." :group 'ygg-projects)

(defface ygg-projects-accent '((t :inherit default))
  "Face of the bar marking the open project." :group 'ygg-projects)

(defface ygg-projects-gutter
  '((((background dark)) :background "#000000")
    (t :inherit default))
  "Face of the run between the sidebar and its neighbour.
On paper it is paper, and the window divider draws the rule."
  :group 'ygg-projects)

(defvar ygg-projects--open nil
  "Root of the project currently expanded, or nil.")

(defvar ygg-projects--here nil
  "Root of the project the sidebar was opened from.
Held because `project-current' answers about whichever buffer a
refresh runs in, and a refresh runs in the sidebar's own.")

(defvar-local ygg-projects--instance nil
  "The mounted vui root of this sidebar.")

(defvar-local ygg-projects--drawn nil
  "The cards this sidebar last drew, as `ygg-projects--picture\=' made them.")

(defvar ygg-projects--stale nil
  "Non-nil when a redraw was due while no window showed the sidebar.")

(defvar aob-transcript--stat-memo)

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

(defvar ygg-projects--drawing nil
  "What the redraw under way has already worked out, by what asked it.
Nil between redraws, so nothing outside one is answered from it.")

(defmacro ygg-projects--once (key &rest body)
  "BODY's value, worked out once per redraw under KEY."
  (declare (indent 1))
  (let ((k (make-symbol "key")) (seen (make-symbol "seen")))
    `(if (not ygg-projects--drawing)
         (progn ,@body)
       (let* ((,k ,key)
              (,seen (gethash ,k ygg-projects--drawing 'ygg-projects--unseen)))
         (if (eq ,seen 'ygg-projects--unseen)
             (puthash ,k (progn ,@body) ygg-projects--drawing)
           ,seen)))))

(defvar ygg-projects--true-dirs (make-hash-table :test #'equal)
  "Each folder a session was placed by, to its true name as a directory.")

(defvar ygg-projects--true-dirs-roots nil
  "The roots on show when `ygg-projects--true-dirs\=' was last emptied.")

(defun ygg-projects--true-dir (dir)
  "DIR's true name as a directory, the disk asked once per spelling."
  (let ((true (lambda ()
                (file-name-as-directory
                 (if (file-remote-p dir) (expand-file-name dir) (file-truename dir))))))
    (if (file-name-absolute-p dir)
        (with-memoization (gethash dir ygg-projects--true-dirs) (funcall true))
      (funcall true))))

(defun ygg-projects--forget-true-dirs (&optional roots)
  "Forget the true names, unless ROOTS are the roots they were found under.
A link re-pointed is seen again when a project comes or goes, or on a
rescan."
  (unless (and roots (equal roots ygg-projects--true-dirs-roots))
    (clrhash ygg-projects--true-dirs)
    (setq ygg-projects--true-dirs-roots roots)))

(defun ygg-projects--root-of (dir roots)
  "The one of ROOTS DIR is, or is inside of, the deepest when several are."
  (when dir
    (let ((dir (ygg-projects--true-dir dir)))
      (car (sort (seq-filter (lambda (r) (string-prefix-p r dir)) roots)
                 (lambda (a b) (> (length a) (length b))))))))

(defun ygg-projects--session-root (s roots)
  "The one of ROOTS session S belongs under, or nil.
A linked worktree's session goes under its repository's main checkout,
there only even when the worktree is a project of its own: one
conversation is one row.  Then its own project; an agent started in a
folder that is no project goes under the one it has been working in,
as its tools last said."
  (or (when-let* ((main (ygg-projects--session-main s)))
        (ygg-projects--root-of main roots))
      (ygg-projects--root-of (aob-session-project s) roots)
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

(defun ygg-projects--ended-subagent-p (s)
  "Non-nil when S is a subagent that has finished, failed or been stopped."
  (and (fboundp 'aob-subagent-p) (aob-subagent-p s)
       (memq (aob-session-state s) '(dead done failed))))

(defun ygg-projects--root-dirs ()
  "The roots on show, spelled as folders, the way a session is matched to one."
  (ygg-projects--once 'root-dirs
    (mapcar (lambda (r) (file-name-as-directory
                         (expand-file-name (if (consp r) (car r) r))))
            (ygg-projects--roots))))

(defun ygg-projects--by-root ()
  "Every root's sessions, in their own order, from one pass over them all."
  (ygg-projects--once 'by-root
    (let ((roots (ygg-projects--root-dirs))
          (table (make-hash-table :test #'equal)))
      (dolist (s (reverse (aob-sessions)))
        (unless (or (ygg-projects--ended-subagent-p s) (aob-session-ref s :hidden))
          (push s (gethash (ygg-projects--session-root s roots) table))))
      table)))

(defun ygg-projects--sessions (root)
  "ROOT's sessions, a subagent only while it runs."
  (when (fboundp 'aob-sessions)
    (gethash root (ygg-projects--by-root))))

(defun ygg-projects--past (root)
  "Conversations in ROOT that ended but can be picked up again.
The ones this Emacs started, and the ones the CLI left on disk before
it ever did — a project you have just taken in has a history whether
or not this Emacs was there for it."
  (ygg-projects--once (list 'past root)
    (let* ((hidden (mapcar (lambda (e) (plist-get e :acp-id))
                           (ygg-projects--archived)))
           (known (and (fboundp 'aob-acp-resumable-entries)
                       (seq-filter
                        (lambda (e)
                          (equal root (file-name-as-directory
                                       (expand-file-name (or (plist-get e :project)
                                                             (plist-get e :dir) "/")))))
                        (ygg-projects--resumable))))
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
                                   (ygg-projects--found root))))
           (put-away (when ygg-projects-show-archived
                       (append
                        (seq-filter
                         (lambda (e)
                           (equal root (file-name-as-directory
                                        (expand-file-name (or (plist-get e :project)
                                                              (plist-get e :dir) "/")))))
                         (ygg-projects--archived))
                        (and (fboundp 'aob-transcript-found)
                             (ygg-projects--found root "archive"))))))
      (setq found (append found put-away))
      ;; newest first, whichever list it came from: a conversation is
      ;; found again by when it happened
      (sort (append known found)
            :key (lambda (e) (or (ygg-projects--entry-ts e) 0))
            :reverse t :in-place t))))

(defun ygg-projects--resumable ()
  "The conversations this Emacs ended and can pick up again."
  (ygg-projects--once 'resumable
    (and (fboundp 'aob-acp-resumable-entries)
         (ignore-errors (aob-acp-resumable-entries)))))

(defun ygg-projects--archived ()
  "The conversations put away here."
  (ygg-projects--once 'archived
    (and (fboundp 'aob-acp-archived-entries)
         (ignore-errors (aob-acp-archived-entries)))))

(defun ygg-projects--found (root &optional where)
  "What the default agent and Codex left on disk for ROOT, put away in WHERE.
Codex keeps its own history whichever agent sessions here start with.
Outside WHERE, what a running agent lists as its own is merged in."
  (seq-mapcat (lambda (agent)
                (let ((found (ignore-errors (aob-transcript-found root agent where))))
                  (if where found (ygg-projects--with-listed found agent root))))
              (delete-dups (list (or (bound-and-true-p aob-acp-default-agent) "claude")
                                 "codex"))))

(defvar ygg-projects--listed (make-hash-table :test #'equal)
  "The sessions each (AGENT . ROOT) last listed as its own.")

(defvar ygg-projects--listed-asked (make-hash-table :test #'equal)
  "When each (AGENT . ROOT) was last asked for its sessions.")

(defconst ygg-projects--listed-every 30
  "Seconds before an agent is asked for its session list again.")

(defun ygg-projects--listed (agent root)
  "The sessions AGENT last listed for ROOT, from the cache only.
It is asked again at most every `ygg-projects--listed-every\=' seconds,
without waiting; an answer that differs redraws once."
  (let ((key (cons agent root)))
    (when (and (fboundp 'aob-acp-list-sessions)
               (> (- (float-time) (gethash key ygg-projects--listed-asked 0))
                  ygg-projects--listed-every))
      (puthash key (float-time) ygg-projects--listed-asked)
      (ignore-errors
        (aob-acp-list-sessions
         agent root
         (lambda (sessions)
           (unless (equal sessions (gethash key ygg-projects--listed))
             (puthash key sessions ygg-projects--listed)
             (ygg-projects--redraw-soon))))))
    (gethash key ygg-projects--listed)))

(defvar ygg-projects--discarded (make-hash-table :test #'equal)
  "Ids discarded here that only their agent kept a file of.
Nothing on disk says they are gone, and the agent's list says so only
once it is asked again.")

(defun ygg-projects--with-listed (found agent root)
  "FOUND, with what AGENT lists for ROOT merged in by id.
One put away here stays away however stale the list; one with no
opening line read yet goes by the title the agent gave it."
  (if-let* ((listed (ygg-projects--listed agent root))
            ((fboundp 'aob-acp-merge-listed)))
      (let ((away (append (hash-table-keys ygg-projects--discarded)
                          (mapcar (lambda (e) (plist-get e :acp-id))
                                  (append (ignore-errors
                                            (aob-transcript-found root agent "archive"))
                                          (ignore-errors
                                            (aob-transcript-found root agent "discarded")))))))
        (delq nil
              (mapcar (lambda (e)
                        (cond ((plist-get e :file)
                               (if (and (plist-get e :listed-title)
                                        (not (aob-transcript--title-cached
                                              (plist-get e :file))))
                                   (plist-put e :name (plist-get e :listed-title))
                                 e))
                              ((not (member (plist-get e :acp-id) away)) e)))
                      (aob-acp-merge-listed found listed agent root))))
    found))

(defun ygg-projects--entry-ts (entry)
  "When ENTRY was last written to, as far as the disk knows.
One only its agent keeps goes by when the agent says it last moved."
  (or (plist-get entry :ts)
      (when-let* ((file (and (fboundp 'aob-transcript-file)
                             (ignore-errors (aob-transcript-file entry)))))
        (float-time (file-attribute-modification-time (file-attributes file))))
      (when-let* ((at (plist-get entry :updated-at)))
        (ignore-errors (float-time (date-to-time at))))))

(defun ygg-projects--agents (root)
  "What ROOT has going, and everything it could go back to.
A running subagent counts as work in flight, once; a conversation that
ended still counts as one the project has, since it can be resumed."
  (let ((all (ygg-projects--sessions root))
        (past (length (ygg-projects--past root))))
    (cons (seq-count (lambda (s) (not (memq (aob-session-state s) '(dead done))))
                     all)
          (+ (length all) past))))

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
  "The projects on show, each umbrella followed by its repositories.
A repository is drawn in its umbrella's Folders row, not as a card, but
its sessions, commands and worktrees are its own all the same."
  (ygg-projects--once 'roots
    (seq-mapcat (lambda (r) (cons r (ignore-errors (ygg-project-children r))))
                (ygg-projects--shown))))

(defun ygg-projects--shown ()
  "Every project card worth listing, in an order that does not move.
Neither opening a project nor an agent starting in one reorders the
list: a row that changes place under the hand that reached for it is
worse than a row in an inconvenient place.  The open project and the
one the sidebar was opened from are kept whatever the cap, since a
list that can drop the thing it is showing is a list that shows
nothing.  An umbrella's repositories are not cards of their own."
  (ygg-projects--once 'shown
    (let* ((scanned (seq-uniq (mapcar #'file-name-as-directory
                                      (delq nil (ignore-errors (ygg-project-top-roots))))
                              #'equal))
           ;; only ones you imported: standing in a folder is not importing
           ;; it, and a row that appears because you opened a file there is
           ;; the row you removed yesterday coming back
           (pinned (seq-filter (lambda (r) (member r scanned))
                               (mapcar (lambda (r) (or (ygg-projects--umbrella-of r) r))
                                       (delq nil (list ygg-projects--here
                                                       ygg-projects--open)))))
           (all (append scanned (seq-remove (lambda (r) (member r scanned)) pinned)))
           (picked (seq-take all (max 1 ygg-projects-limit))))
      (dolist (r pinned)
        (unless (member r picked) (setq picked (append picked (list r)))))
      picked)))

(defun ygg-projects--umbrella-of (root)
  "The umbrella ROOT is a repository of, else nil."
  (ygg-projects--once (list 'umbrella root)
    (ignore-errors (ygg-project-umbrella-of root))))

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
    (dolist (root (ygg-projects--roots))
      (ygg-project-commands-refresh root #'ygg-projects--redraw-soon))))

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
                   (unless (equal was rows) (ygg-projects--redraw-soon))))))
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
    (ygg-projects--scan-docker)
    (ygg-projects--scan-context)))

(unless (timerp ygg-projects--docker-timer)
  (setq ygg-projects--docker-timer
        (run-with-timer 20 20 #'ygg-projects--docker-tick)))

(defun ygg-projects--folders (root)
  "Every folder ROOT covers, the checkout itself first.
The root is where its agents already stand; the rest is what they were
additionally given to see."
  (ygg-projects--once (list 'folders root)
    (let ((root (file-name-as-directory (expand-file-name root))))
      (delete-dups
       (append (list root)
               (ignore-errors (ygg-project-children root))
               (mapcar #'file-name-as-directory
                       (ignore-errors (ygg-project-folders root)))
               ;; a monorepo's members are folders of the project whether or
               ;; not anybody listed them by hand
               (mapcar #'file-name-as-directory
                       (and (fboundp 'ygg-project-workspaces)
                            (ignore-errors (ygg-project-workspaces root)))))))))

(defvar ygg-projects--worktrees-cache (make-hash-table :test #'equal)
  "Each root's other worktrees as (NAME . DIR), as the last scan found them.")

(defvar ygg-projects--worktrees-pending (make-hash-table :test #'equal)
  "Roots with a worktree scan already out.")

(defun ygg-projects--worktree-branch (block)
  "The branch a git worktree list --porcelain BLOCK has out, a detached
one its short sha."
  (cond ((string-match "^branch \\(?:refs/heads/\\)?\\(.*\\)$" block)
         (match-string 1 block))
        ((string-match "^HEAD \\([0-9a-f]\\{7\\}\\)" block)
         (match-string 1 block))
        (t "detached")))

(defun ygg-projects--worktrees-parse (out root)
  "The worktrees OUT names as (LABEL . DIR), ROOT's own checkout left out.
LABEL is the worktree's folder and, in brackets, its branch."
  (let ((own (file-name-as-directory (expand-file-name root)))
        dirs)
    (dolist (block (split-string out "\n\n" t))
      (when (string-match "^worktree \\(.*\\)$" block)
        (let ((dir (match-string 1 block)))
          (unless (equal (file-name-as-directory (expand-file-name dir)) own)
            (push (cons (format "%s (%s)" (file-name-nondirectory dir)
                                (ygg-projects--worktree-branch block))
                        dir)
                  dirs)))))
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
                     (unless (equal was now) (ygg-projects--redraw-soon))))))
          (puthash root t ygg-projects--worktrees-pending))))))

(declare-function ygg-ice-context-scan "ygg-ice" (root))
(declare-function ygg-ice-context-present-p "ygg-ice" (root))
(declare-function ygg-ice-context-count "ygg-ice" (root))
(declare-function ygg-ice-context-entries "ygg-ice" (root))
(declare-function ygg-ice-visit-item "ygg-ice" (item))
(declare-function ygg-ice-changes-list "ygg-ice" ())
(declare-function ygg-ice-send-quickfix "ygg-ice" (items))
(declare-function ygg-ice-send-context "ygg-ice" (items))

(defun ygg-projects--scan-context ()
  "Ask for the projects' ICE docs to be read again when stat says they moved.
The read is queued on a timer and starts no process; the cache it fills
is all the drawing ever looks at."
  (when (fboundp 'ygg-ice-context-scan)
    (dolist (root (ygg-projects--roots))
      (unless (file-remote-p root)
        (ygg-ice-context-scan root)))))

(defun ygg-projects--context-spec (root)
  "ROOT's Context row, or nil where it has no changes, lat.md, ADRs,
glossary or C4 model: a row of zeros in every project says nothing."
  (when (and (fboundp 'ygg-ice-context-present-p)
             (not (file-remote-p root))
             (ygg-ice-context-present-p root))
    (let ((n (ygg-ice-context-count root)))
      (list 'context (ygg-projects--icon "nf-md-book_open_outline" "C")
            "Context" (ygg-projects--counts n n)))))

(defvar ygg-projects--folder-flipped nil
  "Folders in a Folders row shown the other way from their default.
A checkout opens by default, so a project's worktrees are there the way
they always were; an umbrella's repository starts folded.")

(defun ygg-projects--folder-open-p (folder)
  "Whether FOLDER, in a Folders row, shows what it holds."
  (xor (not (ygg-projects--umbrella-of folder))
       (member folder ygg-projects--folder-flipped)))

(defun ygg-projects--folder-foldable-p (folder)
  "Whether FOLDER opens: it has worktrees, or it is an umbrella's
repository, which holds its sessions."
  (or (ygg-projects--worktree-entries folder)
      (ygg-projects--umbrella-of folder)))

(defun ygg-projects--toggle-folder (entry)
  "Fold or unfold the folder ENTRY stands for; nil when ENTRY is no
folder with anything in it, so TAB keeps its other use."
  (when (and (stringp entry) (ygg-projects--folder-foldable-p entry))
    (setq ygg-projects--folder-flipped
          (if (member entry ygg-projects--folder-flipped)
              (remove entry ygg-projects--folder-flipped)
            (cons entry ygg-projects--folder-flipped)))
    t))

(defvar ygg-projects--tree-notes (make-hash-table :test #'equal)
  "Each session folder's worktree line as (WHEN . NOTE), NOTE nil for none.")

(defvar ygg-projects--tree-notes-pending (make-hash-table :test #'equal)
  "Session folders with a worktree lookup already out.")

(defvar ygg-projects--tree-mains (make-hash-table :test #'equal)
  "Each linked-worktree session folder's main checkout, as git last named it.")

(defconst ygg-projects--tree-note-ttl 300
  "Seconds a session folder's worktree line is trusted before git is asked again.")

(defun ygg-projects--tree-note (out dir)
  "The line git worktree list --porcelain OUT earns DIR, a true name.
Its worktree's folder and branch, a detached one its short sha; nil in
the repository's first worktree, the main checkout, or outside them all."
  (let ((dir (file-name-as-directory dir)) (i 0) best)
    (dolist (block (split-string out "\n\n" t))
      (when (string-match "^worktree \\(.*\\)$" block)
        (let ((tree (file-name-as-directory (match-string 1 block))))
          (when (and (string-prefix-p tree dir)
                     (or (null best) (> (length tree) (length (nth 1 best)))))
            (setq best (list i tree block)))))
      (setq i (1+ i)))
    (when (and best (> (car best) 0))
      (let ((block (nth 2 best)))
        (format "⌥ %s · %s"
                (file-name-nondirectory (directory-file-name (nth 1 best)))
                (ygg-projects--worktree-branch block))))))

(defun ygg-projects--main-worktree (out)
  "The main checkout: git worktree list --porcelain OUT lists it first."
  (when (string-match "^worktree \\(.*\\)$" out)
    (file-name-as-directory (match-string 1 out))))

(defun ygg-projects--session-tree (dir)
  "DIR's worktree line from the cache, asking git without waiting when
it has none or an old one — safe on a drawing path."
  (when (and dir (not (file-remote-p dir)))
    (let* ((dir (file-name-as-directory (expand-file-name dir)))
           (seen (gethash dir ygg-projects--tree-notes)))
      (when (and (or (null seen)
                     (> (- (float-time) (car seen)) ygg-projects--tree-note-ttl))
                 (not (gethash dir ygg-projects--tree-notes-pending))
                 (file-directory-p dir))
        (if (ignore-errors
              (ygg-git-async
               dir '("worktree" "list" "--porcelain")
               (lambda (out exit)
                 (remhash dir ygg-projects--tree-notes-pending)
                 (let ((note (and (zerop exit)
                                  (ygg-projects--tree-note out (file-truename dir)))))
                   (if note
                       (puthash dir (ygg-projects--main-worktree out)
                                ygg-projects--tree-mains)
                     (remhash dir ygg-projects--tree-mains))
                   (puthash dir (cons (float-time) note) ygg-projects--tree-notes)
                   (unless (equal note (cdr seen)) (ygg-projects--redraw-soon))))))
            (puthash dir t ygg-projects--tree-notes-pending)
          (puthash dir (cons (float-time) nil) ygg-projects--tree-notes)))
      (cdr seen))))

(defun ygg-projects--session-main (s)
  "The main checkout of the linked worktree S works in, else nil.
From the cache, asking git without waiting — safe on a drawing path."
  (when-let* ((dir (or (aob-session-dir s) (aob-session-project s)))
              ((not (file-remote-p dir))))
    (ygg-projects--session-tree dir)
    (gethash (file-name-as-directory (expand-file-name dir))
             ygg-projects--tree-mains)))

(defun ygg-projects--entry-root (e roots)
  "The one of ROOTS ended conversation E goes under, found the way a
running session's is: a linked worktree under its main checkout."
  (let ((dir (or (plist-get e :dir) (plist-get e :project))))
    (or (when-let* ((dir) ((not (file-remote-p dir))))
          (ygg-projects--session-tree dir)
          (ygg-projects--root-of
           (gethash (file-name-as-directory (expand-file-name dir))
                    ygg-projects--tree-mains)
           roots))
        (ygg-projects--root-of (or (plist-get e :project) dir) roots))))

(defun ygg-projects--pinned-ended (root)
  "ROOT's pinned conversations no session is holding, from any of its
worktrees: a pin outlasts the session, so the row does too."
  (when-let* ((pins (ygg-projects--pins))
              ((fboundp 'aob-acp-resumable-entries)))
    (let ((roots (ygg-projects--root-dirs)))
      (seq-filter (lambda (e)
                    (and (member (plist-get e :acp-id) pins)
                         (equal (ygg-projects--entry-root e roots) root)))
                  (ygg-projects--resumable)))))

(declare-function aob-subagent-live-count "aob-subagent" (s))
(declare-function aob-subagent-parent "aob-subagent" (s))

(defcustom ygg-projects-show-subagents nil
  "Non-nil shows every lead's subagents; nil shows only the leads opened with TAB."
  :type 'boolean :group 'ygg-projects)

(defvar ygg-projects--expanded nil
  "Ids of the leads whose subagents are on show.")

(defun ygg-projects--expanded-p (s)
  (or ygg-projects-show-subagents
      (member (aob-session-id s) ygg-projects--expanded)))

(defun ygg-projects--hidden-count (s)
  "How many live subagents S keeps folded away, zero when on show."
  (if (or (ygg-projects--expanded-p s) (not (fboundp 'aob-subagent-children)))
      0
    (seq-count (lambda (k) (not (ygg-projects--ended-subagent-p k)))
               (aob-subagent-children s))))

(defun ygg-projects--entry-tree (payload)
  "The grey line under PAYLOAD's row: the agent running it, then a
subagent's kind, or a lead's live subagents against its cap and the
worktree of a session outside its repository's main checkout; a
subagent stands where its sender does, so its worktree is never said
twice."
  (when (and (fboundp 'aob-session-p) (aob-session-p payload))
    (let* ((agent (aob-session-ref payload :agent))
           (sub (and (fboundp 'aob-subagent-p) (aob-subagent-p payload)))
           (cap (and (not sub) (fboundp 'aob-subagent-live-count)
                     (aob-session-ref payload :subagent-cap)))
           (more (if sub
                     (let ((kind (aob-session-ref payload :subagent-type)))
                       (unless (equal kind agent) kind))
                   (ygg-projects--session-tree (or (aob-session-dir payload)
                                                   (aob-session-project payload)))))
           (hidden (if sub 0 (ygg-projects--hidden-count payload)))
           (parts (delq nil (list (and (stringp agent) agent)
                                  (and cap (format "%s/%s"
                                                   (aob-subagent-live-count payload)
                                                   cap))
                                  more
                                  (and (> hidden 0) (format "▸ %d" hidden))))))
      (and parts (string-join parts " · ")))))


;;; What a row holds, when you open it

(defun ygg-projects--session-ts (s)
  "When S started, as a number: a row keeps its place while it streams.
Ordering by the last event made every row trade places each second."
  (float-time (or (ignore-errors (aob-session-started s))
                  (plist-get (car (last (aob-session-events s))) :ts)
                  0)))

(defcustom ygg-projects-pins-file (locate-user-emacs-file "var/projects-pins.eld")
  "Where the pinned sessions are kept, first pinned first."
  :type 'file :group 'ygg-projects)

(defvar ygg-projects--pin-list 'unread
  "The pinned sessions' keys in pin order, or unread before the file is.")

(defun ygg-projects--pins ()
  (when (eq ygg-projects--pin-list 'unread)
    (setq ygg-projects--pin-list
          (and ygg-projects-pins-file (file-readable-p ygg-projects-pins-file)
               (ignore-errors
                 (with-temp-buffer
                   (insert-file-contents ygg-projects-pins-file)
                   (seq-filter #'stringp (read (current-buffer))))))))
  ygg-projects--pin-list)

(defun ygg-projects--save-pins (pins)
  "Keep PINS; the file gets only conversation keys, since session ids
are handed out again from 1 after a restart and would pin a stranger."
  (setq ygg-projects--pin-list pins)
  (when ygg-projects-pins-file
    (make-directory (file-name-directory ygg-projects-pins-file) t)
    (with-temp-file ygg-projects-pins-file
      (prin1 (seq-remove (lambda (k) (string-prefix-p "acp:" k)) pins)
             (current-buffer)))))

(defun ygg-projects--pin-key (s)
  "What S is pinned by: its conversation, which outlives a resume's new id."
  (or (aob-session-ref s :acp-id) (aob-session-id s)))

(defun ygg-projects--pin-keys (s)
  "Every key S may have been pinned under: one pinned while starting has
no conversation yet."
  (if (ygg-projects--ended-p s)
      (list (plist-get s :acp-id))
    (delq nil (list (aob-session-ref s :acp-id) (aob-session-id s)))))

(defun ygg-projects--pin-rank (s)
  "S's place among the pins, or nil when it is not pinned.
A pin taken before S had a conversation moves onto it once it has one.
S may be an ended conversation, kept pinned across a restart."
  (when-let* ((pins (ygg-projects--pins))
              ((ygg-projects--conversation-p s))
              (ranks (delq nil (mapcar (lambda (k) (seq-position pins k))
                                       (ygg-projects--pin-keys s)))))
    (let ((rank (apply #'min ranks))
          (conv (and (ygg-projects--session-p s) (aob-session-ref s :acp-id))))
      (when (and conv (equal (nth rank pins) (aob-session-id s)))
        (ygg-projects--save-pins
         (seq-uniq (mapcar (lambda (k) (if (equal k (aob-session-id s)) conv k)) pins))))
      rank)))

(defun ygg-projects--pinned-p (s)
  (and (ygg-projects--pin-rank s) t))

(defun ygg-projects--sender (s)
  "The top-level session S was sent by, S itself when nobody sent it."
  (let ((seen (list s)))
    (while-let ((up (and (fboundp 'aob-subagent-parent) (aob-subagent-parent s)))
                ((not (memq up seen))))
      (push up seen)
      (setq s up))
    s))

(defun ygg-projects--depth (s)
  (let ((top (ygg-projects--sender s)) (n 0) (seen nil))
    (while (and (not (eq s top)) (not (memq s seen)))
      (push s seen)
      (setq s (aob-subagent-parent s) n (1+ n)))
    n))

(defun ygg-projects--by-start (sessions)
  (sort (copy-sequence sessions)
        (lambda (a b) (> (ygg-projects--session-ts a) (ygg-projects--session-ts b)))))

(defun ygg-projects--session-label (s)
  "S's name as its row says it: without the agent in front of the task,
which the grey line under the row already names."
  (let ((name (aob-session-name s))
        (agent (aob-session-ref s :agent)))
    (if (and (stringp name) (stringp agent)
             (string-prefix-p (concat agent ": ") name))
        (substring name (+ 2 (length agent)))
      name)))

(defun ygg-projects--descendant-rows (s depth seen)
  "The rows of what S sent, and what they sent, DEPTH levels in.
SEEN holds the sessions already drawn, so a loop in the refs ends."
  (when (fboundp 'aob-subagent-children)
    (mapcan (lambda (kid)
              (unless (memq kid seen)
                (push kid seen)
                (cons (cons (concat (make-string (* 2 depth) ?\s) "└ "
                                    (ygg-projects--session-label kid))
                            kid)
                      (ygg-projects--descendant-rows kid (1+ depth) seen))))
            (ygg-projects--by-start
             (seq-remove #'ygg-projects--ended-subagent-p
                         (aob-subagent-children s))))))

(defun ygg-projects--entries (root kind)
  "The things ROOT's KIND row stands for: (LABEL . PAYLOAD) each."
  (pcase kind
    ('agents
     (let* ((live (seq-remove (lambda (x)
                                (and (fboundp 'aob-subagent-p)
                                     (aob-subagent-p x)))
                              (ygg-projects--sessions root)))
            (ended (ygg-projects--past root))
            (kept (ygg-projects--pinned-ended root))
            (kept-ids (mapcar (lambda (e) (plist-get e :acp-id)) kept))
            (ended (seq-remove (lambda (e) (member (plist-get e :acp-id) kept-ids))
                               ended))
            (pinned (sort (append (seq-filter #'ygg-projects--pinned-p live)
                                  kept
                                  (seq-filter #'ygg-projects--pinned-p ended))
                          (lambda (a b)
                            (< (ygg-projects--pin-rank a) (ygg-projects--pin-rank b)))))
            (label (lambda (s)
                     (if (ygg-projects--ended-p s)
                         (or (plist-get s :name) (plist-get s :agent) "session")
                       (ygg-projects--session-label s))))
            (rows (lambda (s)
                    ;; what it sent goes under it, two columns a level:
                    ;; a row this narrow has no more to spare
                    (cons (cons (funcall label s) s)
                          (and (ygg-projects--session-p s)
                               (ygg-projects--expanded-p s)
                               (ygg-projects--descendant-rows s 0 (list s))))))
            (groups (mapcar (lambda (s) (cons (ygg-projects--session-ts s)
                                              (funcall rows s)))
                            (seq-difference live pinned #'eq)))
            ;; ended, but the conversation is still there to pick up
            (past (and ygg-projects-show-past
                  (mapcar (lambda (e)
                            (cons (or (ygg-projects--entry-ts e) 0)
                                  (list (cons (or (plist-get e :name)
                                                  (plist-get e :agent)
                                                  "session")
                                              e))))
                          (seq-difference ended pinned #'eq)))))
       ;; running first, newest started first, then ended, with air
       ;; between: what is alive is told from what is kept without
       ;; reading a single badge, and a session's own rows go with it
       (let ((newest (lambda (cells)
                       (apply #'append
                              (mapcar #'cdr (sort cells (lambda (a b) (> (car a) (car b)))))))))
         (append (mapcan rows pinned)
                 (funcall newest groups)
                 (and (or live pinned) past (list (cons "" 'ygg-projects-gap)))
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
    ('folders (mapcar (lambda (d) (cons (ygg-projects--folder-label d) d))
                      (ygg-projects--folders root)))
    ('context (and (fboundp 'ygg-ice-context-entries)
                   (ygg-ice-context-entries root)))
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
         (let ((progress (ygg-projects--session-progress payload)))
           (format "%s%s" (if progress (concat progress " ") "")
                   (aob-session-state payload))))
        ((and (consp payload) (proper-list-p payload) (plist-get payload :archived))
         "archived")
        ((and (consp payload) (eq (car payload) 'docker))
         (ygg-projects--docker-status (plist-get (cdr payload) :status)))
        ((and (consp payload) (proper-list-p payload) (plist-get payload :ice))
         (or (plist-get payload :badge) ""))
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

(defun ygg-projects--session-progress (s)
  "S's todo list as done/total, or nil when it keeps none or has stopped.
A subagent keeps no list, only its own plan, counted into :plan-progress."
  (when-let* (((not (memq (aob-session-state s) '(dead failed))))
              (progress (or (when-let* (((fboundp 'ygg-todo-session-file))
                                        (file (ygg-todo-session-file s)))
                              (ygg-todo-progress file))
                            (aob-session-ref s :plan-progress))))
    (format "%d/%d" (car progress) (cdr progress))))

(defun ygg-projects--due (ts)
  "TS, a time to come, said in a few columns.
Within the hour in minutes, later today in hours, by tomorrow at its
clock time, within the week by its day, and past that by its date."
  (let ((secs (- ts (float-time))))
    (cond ((<= secs 0) "due")
          ((< secs 3600) (format "%dm" (max 1 (floor secs 60))))
          ((equal (format-time-string "%F" ts) (format-time-string "%F"))
           (format "%dh" (round secs 3600)))
          ((< secs 86400) (format-time-string "%H:%M" ts))
          ((< secs (* 6 86400)) (format-time-string "%a" ts))
          (t (format-time-string "%b %-d" ts)))))

(defun ygg-projects--schedule-face (schedules)
  (if (seq-some (lambda (s) (plist-get s :error)) schedules)
      'ygg-projects-waiting
    'shadow))

(defun ygg-projects--schedule-mark (acp-id)
  "What is scheduled for the conversation ACP-ID, as (TEXT . FACE), or nil.
The soonest that will run, and when; a failed one is said before it,
and a paused mark only when nothing else is left to run."
  (when-let* ((acp-id)
              ((fboundp 'aob-schedule-for))
              (all (aob-schedule-for acp-id)))
    (let ((failed (seq-find (lambda (s) (plist-get s :error)) all))
          (active (seq-remove (lambda (s) (plist-get s :paused)) all)))
      (cons (cond (failed "◷ failed")
                  (active (concat "◷ " (ygg-projects--due (plist-get (car active) :next))))
                  (t (concat "⏸ " (ygg-projects--due (plist-get (car all) :next)))))
            (ygg-projects--schedule-face all)))))

(defun ygg-projects--acp-id (payload)
  "The conversation id of PAYLOAD, a session or an ended conversation's plist."
  (cond ((ygg-projects--session-p payload) (aob-session-ref payload :acp-id))
        ((ygg-projects--ended-p payload) (plist-get payload :acp-id))))

(defun ygg-projects--badge (payload &optional room)
  "PAYLOAD's badge, drawn: a session's meter muted beside its state.
The state is the colour of the state: green at work, orange waiting on
you, grey once there is nothing to wait for.  Working and idle are said
by the dot and the running clock already, so a meter stands in for them.
Given ROOM columns, a session's badge sheds parts until it fits: its
spend first, then its clock, then its progress, and its state last,
since a session waiting on you is the one thing a glance must catch.
What is scheduled for it goes before any of them, shed after the spend.
A folder's badge is its own, `ygg-projects--folder-badge'."
  (cond
   ((stringp payload) (ygg-projects--folder-badge payload room))
   ((not (and (fboundp 'aob-session-p) (aob-session-p payload)))
      (let ((badge (propertize (ygg-projects--entry-badge payload)
                               'font-lock-face 'ygg-projects-count))
            (mark (ygg-projects--schedule-mark (ygg-projects--acp-id payload))))
        (if (and mark (or (not room)
                          (<= (+ (string-width (car mark)) 1 (string-width badge)) room)))
            (concat (propertize (car mark) 'font-lock-face (cdr mark)) " " badge)
          badge)))
   (t
    (let* ((mark (ygg-projects--schedule-mark (ygg-projects--acp-id payload)))
           (state (format "%s" (aob-session-state payload)))
           (clock (and (fboundp 'aob-session-clock) (aob-session-clock payload)))
           (spend (and (fboundp 'aob-session-spend) (aob-session-spend payload)))
           (quiet (and (or clock spend) (member state '("working" "idle"))))
           (parts (seq-filter
                   #'car
                   (list (list (car mark) (cdr mark) 1.5)
                         (list (and (fboundp 'aob-session-quiet)
                                    (aob-session-quiet payload))
                               'shadow 3.5)
                         (list clock 'ygg-projects-count 2)
                         (list spend 'ygg-projects-count 1)
                         (list (ygg-projects--session-progress payload)
                               'ygg-projects-count 3)
                         (list (unless quiet state)
                               (ygg-projects--session-dot payload) 4)))))
      (ygg-projects--fit parts room)))))

(defun ygg-projects--fit (parts room)
  "PARTS, each (TEXT FACE RANK), drawn in ROOM columns.
The lowest rank goes first until the rest fit, the last one never."
  (let ((width (lambda () (string-width (mapconcat #'car parts " ")))))
    (while (and room (cdr parts) (> (funcall width) room))
      (setq parts (delq (car (seq-sort-by (lambda (p) (nth 2 p)) #'< parts))
                        parts)))
    (mapconcat (lambda (p) (propertize (car p) 'font-lock-face (nth 1 p)))
               parts " ")))

(defun ygg-projects--folder-badge (folder room)
  "FOLDER's worktrees, and as an umbrella's repository its live sessions,
theirs counted in, and what is scheduled there, in ROOM columns.
The schedule goes first, then the sessions, the worktrees last."
  (let* ((child (ygg-projects--umbrella-of folder))
         (wts (length (ygg-projects--worktree-entries folder)))
         (live (and child
                    (seq-count (lambda (s) (not (memq (aob-session-state s)
                                                      ygg-projects-over-states)))
                               (ygg-projects--sessions folder))))
         (scheduled (and child (ygg-projects--project-schedules folder))))
    (ygg-projects--fit
     (seq-filter #'car
                 (list (list (and (> wts 0) (format "⌥%d" wts)) 'ygg-projects-count 3)
                       (list (and live (> live 0) (format "●%d" live))
                             (ygg-projects--dot folder) 2)
                       (list (and scheduled (format "◷%d" (length scheduled)))
                             (ygg-projects--schedule-face scheduled) 1)))
     room)))

(defcustom ygg-projects-entry-indent 4
  "Columns an entry is set in from the left.
A panel is narrow: every column spent on indentation is a column the
name does not get."
  :type 'natnum :group 'ygg-projects)

(defcustom ygg-projects-entry-min-name 10
  "Columns of an entry's name kept before its badge gives up a part."
  :type 'natnum :group 'ygg-projects)

(defun ygg-projects--entry-text (label root kind payload)
  "LABEL as one row, its badge at the right edge.
A row never wraps: the badge sheds its least parts until the name has
the columns ygg-projects-entry-min-name asks for, and the name is cut
to what is left after that; a path loses its head, not its name."
  (let* ((indent (+ ygg-projects-entry-indent 2))
         (pin (if (ygg-projects--pinned-p payload)
                  (propertize "⊤ " 'font-lock-face 'ygg-projects-count)
                ""))
         (line (- (ygg-projects--width) indent 1 (string-width pin)))
         (badge (ygg-projects--badge
                 payload (- line (min (string-width label) ygg-projects-entry-min-name))))
         ;; a rail down the indent, the way a tree says depth without
         ;; spending a column on saying nothing
         (head (concat (make-string (max 0 (- ygg-projects-entry-indent 2)) ?\s)
                       (propertize "│" 'font-lock-face 'ygg-projects-idle)
                       " "
                       (propertize "·" 'font-lock-face
                                   (ygg-projects--session-dot payload))
                       " "))
         (room (max 1 (- line (string-width badge))))
         (name (cond ((<= (string-width label) room) label)
                     ((string-search "/" label)
                      (let ((tail label))
                        (while (> (string-width tail) (1- room))
                          (setq tail (substring tail 1)))
                        (concat "…" tail)))
                     (t (truncate-string-to-width label room nil nil "…")))))
    (propertize (concat head pin
                        (propertize name 'font-lock-face 'ygg-projects-entry)
                        (ygg-projects--right
                         badge (+ 1.0 ygg-projects-entry-spacing)))
                'ygg-project root 'ygg-row kind 'ygg-entry payload)))

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

(defun ygg-projects--tree-text (note root kind payload)
  "NOTE as the grey line under PAYLOAD's row, set in under its name.
The same row to every command, but never a line point stops on."
  (let* ((depth (if (ygg-projects--session-p payload)
                    (* 2 (ygg-projects--depth payload))
                  0))
         (line (- (ygg-projects--width) ygg-projects-entry-indent 3 depth)))
    (propertize (concat (make-string (max 0 (- ygg-projects-entry-indent 2)) ?\s)
                        (propertize "│" 'font-lock-face 'ygg-projects-idle)
                        "   "
                        (make-string depth ?\s)
                        (propertize (truncate-string-to-width note line nil nil "…")
                                    'font-lock-face 'ygg-projects-count)
                        (ygg-projects--pad))
                'ygg-project root 'ygg-row kind 'ygg-entry payload 'ygg-cont t)))

(defun ygg-projects--folder-label (folder)
  "FOLDER as its line in a Folders row: an umbrella's repository by its
name, anything else by its path, each marked open or shut when it opens."
  (concat (cond ((not (ygg-projects--folder-foldable-p folder)) "")
                ((ygg-projects--folder-open-p folder) "▾ ")
                (t "▸ "))
          (if (ygg-projects--umbrella-of folder)
              (file-name-nondirectory (directory-file-name folder))
            (abbreviate-file-name (directory-file-name folder)))))

(defun ygg-projects--folder-nodes (root)
  "ROOT's Folders row: each folder, and under one that is open its
worktrees and, for an umbrella's repository, its sessions, set in a
step.  What is under a folder carries that folder as its project."
  (mapcan (lambda (d)
            (cons (ygg-projects--entry-text
                   (ygg-projects--folder-label d) root 'folders d)
                  (when (ygg-projects--folder-open-p d)
                    (let ((ygg-projects-entry-indent (+ 2 ygg-projects-entry-indent)))
                      (append
                       (mapcar (lambda (wt)
                                 (ygg-projects--entry-text
                                  (concat "⌥ " (car wt)) d 'folders
                                  (cons 'worktree (cdr wt))))
                               (ygg-projects--worktree-entries d))
                       (when-let* (((ygg-projects--umbrella-of d))
                                   (cells (ygg-projects--entries d 'agents)))
                         (ygg-projects--entry-nodes d 'agents cells)))))))
          (ygg-projects--folders root)))

(defun ygg-projects--entry-nodes (root kind &optional cells)
  "The lines of ROOT's KIND row, out of CELLS when they are known."
  (if (and (eq kind 'folders) (not cells))
      (ygg-projects--folder-nodes root)
    (ygg-projects--cell-nodes root kind cells)))

(defun ygg-projects--cell-nodes (root kind cells)
  (mapcan (lambda (cell)
            (if (eq (cdr cell) 'ygg-projects-gap)
                (list " ")
              (let* ((payload (cdr cell))
                     (note (and (eq kind 'agents) (ygg-projects--entry-tree payload)))
                     (texts (cons (ygg-projects--entry-text (car cell) root kind payload)
                                  (and note (list (ygg-projects--tree-text
                                                   note root kind payload))))))
                (when (ygg-projects--on-screen-p payload)
                  (dolist (text texts)
                    (ygg-projects--mark-row text 'ygg-projects-on-screen)))
                texts)))
          (or cells
              (ygg-projects--entries root kind)
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

(defun ygg-projects--project-schedules (root)
  "The schedules that work in ROOT, and not in a project nested in it.
Each goes under the deepest project holding its folder, the way a
session does, so a parent does not count what its child already shows."
  (when (fboundp 'aob-schedule-for-project)
    (ygg-projects--once (list 'schedules root)
      (let ((roots (ygg-projects--root-dirs)))
        (seq-filter (lambda (s)
                      (equal (ygg-projects--root-of
                              (plist-get (plist-get s :target) :project) roots)
                             root))
                    (aob-schedule-for-project root))))))

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
         (mark (and (fboundp 'ygg-project-import-mark)
                    (ygg-project-import-mark root)))
         (scheduled (ygg-projects--project-schedules root))
         (count (and scheduled
                     (<= (+ 3 (string-width (number-to-string (length scheduled)))
                            (if mark (1+ (string-width mark)) 0))
                         (- avail (string-width name) 2))
                     (propertize (format "◷ %d" (length scheduled))
                                 'font-lock-face (ygg-projects--schedule-face scheduled))))
         (room (- avail (string-width name) 2 (if count (1+ (string-width count)) 0)))
         (path (cond (mark (propertize mark 'font-lock-face 'ygg-projects-waiting))
                     ((<= (string-width full) room) full)
                     ((let ((short (concat "…/" (file-name-nondirectory full))))
                        (and (<= (string-width short) room) short)))
                     (t ""))))
    (propertize (concat " " dot "  "
                        (propertize name 'font-lock-face 'ygg-projects-name)
                        (ygg-projects--right
                         (concat count (and count (not (string-empty-p path)) " ")
                                 (propertize path 'font-lock-face 'ygg-projects-path))))
                'ygg-project root 'ygg-row 'project)))

(defun ygg-projects--row-specs (root)
  "ROOT's rows as (KIND ICON LABEL COUNT)."
  (let ((agents (ygg-projects--agents root))
        (cmds (ygg-projects--commands root))
        (terms (ygg-projects--processes root)))
    ;; the same family as the rows under it: one text glyph among four
    ;; icons is the one that looks wrong, whatever its width says
    (delq nil
    (list (list 'agents (ygg-projects--icon "nf-md-triangle_outline" "▲")
                "Sessions" (ygg-projects--counts (car agents) (cdr agents)))
          (ygg-projects--context-spec root)
          (list 'commands (ygg-projects--icon "nf-md-console" ">")
                "Commands" (ygg-projects--counts (car cmds) (cdr cmds)))
          (list 'processes (ygg-projects--icon "nf-md-console_line" "T")
                "Processes" (ygg-projects--counts (car terms) (cdr terms)))
          (let ((n (length (ygg-projects--folders root))))
            (list 'folders (ygg-projects--icon "nf-md-folder_multiple_outline" "F")
                  "Folders" (ygg-projects--counts n n)))))))

(defun ygg-projects--rows (root)
  "ROOT's rows, the opened one followed by what it holds."
  (let ((out nil))
    (pcase-dolist (`(,kind ,icon ,label ,count) (ygg-projects--row-specs root))
      (push (ygg-projects--row-text icon label count root kind) out)
      (when (member (cons root kind) ygg-projects--open-row)
        (dolist (node (ygg-projects--entry-nodes root kind)) (push node out))))
    (nreverse out)))

(defun ygg-projects--picture ()
  "Every card on show as (ROOT OPEN LINES), LINES its drawn text."
  (mapcar (lambda (root)
            (let ((open (equal root ygg-projects--open)))
              (list root open (cons (ygg-projects--head-text root)
                                    (and open (ygg-projects--rows root))))))
          (ygg-projects--shown)))

(defun ygg-projects--fresh-picture ()
  "The cards as they stand now, each thing a redraw asks worked out once."
  (let ((ygg-projects--drawing (make-hash-table :test #'equal))
        (aob-transcript--stat-memo (make-hash-table :test #'equal)))
    (ygg-projects--forget-true-dirs (ygg-projects--root-dirs))
    (ygg-projects--forget-buffers)
    (ygg-projects--picture)))

(defun ygg-projects--same-p (a b)
  "Whether A and B draw alike: text by its characters and properties, a
session or a buffer by identity, anything else by value.  A session is
never compared by value: that walks its whole history."
  (while (and (consp a) (consp b) (ygg-projects--same-p (car a) (car b)))
    (setq a (cdr a) b (cdr b)))
  (cond ((eq a b) t)
        ((or (consp a) (consp b)) nil)
        ((stringp a)
         (and (stringp b) (string= a b)
              (ygg-projects--same-p (object-intervals a) (object-intervals b))))
        ((or (recordp a) (recordp b)) nil)
        (t (equal a b))))

(with-eval-after-load 'vui
  ;; expanded where vui is loaded, never before: byte-compiled in a
  ;; session that has not loaded it, `vui-defcomponent' is not yet a
  ;; macro, compiles to a function call, and the file signals on load
  (eval '(progn
    (vui-defcomponent ygg-projects-card (open lines)
        "One project: a line in the list, or an opened block when OPEN.
    Nothing is drawn above the head line: a row that slid down as it
    opened would take the hand that pressed TAB with it."
        :render
        (if (not open)
            (vui-text (car lines))
          (vui-region
           :face 'ygg-projects-card
           (apply #'vui-vstack (mapcar #'vui-text lines)))))

      (vui-defcomponent ygg-projects-view (cards)
        "The projects as a list, the open one lifted out of it as a card."
        :render
        (apply #'vui-vstack
               (mapcar (lambda (card)
                         (vui-component 'ygg-projects-card
                                        :key (nth 0 card) :open (nth 1 card)
                                        :lines (nth 2 card)))
                       cards))))
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
        (if (not (get-buffer-window buf t))
            (setq ygg-projects--stale t)
          (setq ygg-projects--stale nil)
          (let ((cards (ygg-projects--fresh-picture)))
            (unless (ygg-projects--same-p cards ygg-projects--drawn)
              (let* ((row (ygg-projects--row-at-point))
                     (win (get-buffer-window buf 'visible))
                     (start (and (window-live-p win) (window-start win))))
                (vui-update-props ygg-projects--instance (list :cards cards))
                (setq ygg-projects--drawn cards)
                (ygg-projects--goto-row row)
                (ygg-projects--follow-point)
                (when (and (window-live-p win) start (<= start (point-max)))
                  (set-window-start win start t))))))))))

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

(declare-function ygg-normal-state "yggdrasil-core" ())
(declare-function ygg-toggle-visual "yggdrasil-core" ())
(declare-function aob-session-name "aob" (s))
(declare-function aob-session-ref "aob" (s key))
(declare-function aob-prompt "aob" (s text &optional attachments))
(defvar ygg--visual-p)
(defvar aob-prompt-typed)

(defun ygg-projects--selecting-p ()
  (and (bound-and-true-p ygg--visual-p) (mark t) t))

(defun ygg-projects--selected-entries (&optional kind)
  "The session rows between mark and point, each once, top first.
A name's second line and a worktree note carry their row's entry, and
project heads, row titles and the other rows carry none of a session's.
With KIND, that row's entries instead of the sessions'."
  (let ((last (save-excursion (goto-char (max (point) (mark t)))
                              (line-beginning-position)))
        (out nil))
    (save-excursion
      (goto-char (min (point) (mark t)))
      (beginning-of-line)
      (while (and (<= (point) last) (not (eobp)))
        (when-let* (((eq (get-text-property (point) 'ygg-row) (or kind 'agents)))
                    ((not (get-text-property (point) 'ygg-cont)))
                    (entry (get-text-property (point) 'ygg-entry))
                    ((not (symbolp entry))))
          (unless (memq entry out) (push entry out)))
        (forward-line 1)))
    (nreverse out)))

(defun ygg-projects--session-p (entry)
  (and (fboundp 'aob-session-p) (aob-session-p entry)))

(defun ygg-projects--subagent-entry-p (entry)
  (and (ygg-projects--session-p entry)
       (fboundp 'aob-subagent-p) (aob-subagent-p entry)))

(defun ygg-projects--conversation-p (entry)
  "Non-nil when ENTRY is a session, running or ended."
  (or (ygg-projects--session-p entry) (ygg-projects--ended-p entry)))

(defun ygg-projects--running-p (entry)
  (and (ygg-projects--session-p entry)
       (not (aob-session-ref entry :asleep))
       (not (memq (aob-session-state entry) ygg-projects-over-states))))

(defun ygg-projects--entry-name (entry)
  (if (ygg-projects--session-p entry)
      (aob-session-name entry)
    (or (plist-get entry :name) (plist-get entry :agent) "session")))

(defun ygg-projects--leave-selection ()
  "Back to normal state on the first line the selection held."
  (when (ygg-projects--selecting-p)
    (goto-char (min (point) (mark t)))
    (beginning-of-line)
    (when (fboundp 'ygg-normal-state) (ygg-normal-state))))

(defun ygg-projects--act (verb targets act &optional ask)
  "Call ACT on each of TARGETS, having asked once whether to VERB them.
Without ASK nothing is asked.  In visual state the selection's session
rows are the targets TARGETS keeps, subagents left out since they are
their sender's to steer; otherwise it is the row at point."
  (let* ((all (if (ygg-projects--selecting-p)
                  (ygg-projects--selected-entries)
                (delq nil (list (ygg-projects--entry-at-point)))))
         (subs (seq-filter #'ygg-projects--subagent-entry-p all))
         (picked (seq-filter targets (seq-difference all subs #'eq)))
         (skipped (if subs (format "; %d subagent%s skipped (read-only)"
                                   (length subs) (if (cdr subs) "s" ""))
                    "")))
    (unless picked
      (ygg-projects--leave-selection)
      (user-error "projects: nothing here to %s%s" verb skipped))
    (when (or (not ask)
              (y-or-n-p (format "%s%s %s? " (upcase (substring verb 0 1))
                                (substring verb 1)
                                (mapconcat #'ygg-projects--entry-name picked ", "))))
      (ygg-projects--leave-selection)
      ;; the one question has been answered for all of them
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (mapc act picked))
      (ygg-projects-refresh)
      (message "projects: %s %d%s" verb (length picked) skipped))))

(defun ygg-projects-cancel ()
  "Stop the turn of the session on this line, or of each one selected."
  (interactive)
  (ygg-projects--act "cancel" #'ygg-projects--running-p
                     (lambda (s) (aob--call s :cancel nil))))

(defun ygg-projects-say (text)
  "Send TEXT to the session on this line, or to each one selected."
  (interactive
   (let* ((all (if (ygg-projects--selecting-p)
                   (ygg-projects--selected-entries)
                 (list (ygg-projects--entry-at-point))))
          (names (mapcar #'ygg-projects--entry-name
                         (seq-filter #'ygg-projects--running-p
                                     (seq-remove #'ygg-projects--subagent-entry-p all)))))
     (unless names (user-error "projects: no running session to say anything to"))
     (list (read-string (format "%s » " (string-join names ", "))))))
  (ygg-projects--act "say to" #'ygg-projects--running-p
                     (lambda (s) (let ((aob-prompt-typed t)) (aob-prompt s text)))))

(defvar aob-buffer-session-id)
(declare-function aob-session-get "aob" (id))

(defun ygg-projects--in-sidebar-p ()
  (or (bound-and-true-p ygg-projects--modal)
      (equal (buffer-name) ygg-projects-buffer-name)))

(defun ygg-projects--pin-targets ()
  "The top-level sessions pinning acts on here, each once, top first.
In the sidebar, the row at point or every row selected; elsewhere, the
session the buffer shows."
  (let ((picked
         (if (ygg-projects--in-sidebar-p)
             (if (ygg-projects--selecting-p)
                 (ygg-projects--selected-entries)
               (list (ygg-projects--entry-at-point)))
           (list (and (bound-and-true-p aob-buffer-session-id)
                      (fboundp 'aob-session-get)
                      (aob-session-get aob-buffer-session-id)))))
        (out nil))
    (dolist (entry picked)
      (when-let* ((top (cond ((ygg-projects--session-p entry)
                              (ygg-projects--sender entry))
                             ((ygg-projects--ended-p entry) entry))))
        (unless (memq top out) (push top out))))
    (nreverse out)))

(defun ygg-projects-toggle-pin ()
  "Pin the session here above the rest of its project, or unpin it.
A subagent pins the session that sent it.  Across a visual selection,
every one is unpinned when all of them already are pinned, else the
rest are pinned after them."
  (interactive)
  (let ((targets (ygg-projects--pin-targets))
        (pins (ygg-projects--pins)))
    (unless targets
      (when (ygg-projects--in-sidebar-p) (ygg-projects--leave-selection))
      (user-error "projects: no session here to pin"))
    (let ((unpin (seq-every-p #'ygg-projects--pinned-p targets)))
      (ygg-projects--save-pins
       (if unpin
           (seq-difference pins (mapcan #'ygg-projects--pin-keys targets))
         (append pins (mapcar #'ygg-projects--pin-key
                              (seq-remove #'ygg-projects--pinned-p targets)))))
      (when (ygg-projects--in-sidebar-p) (ygg-projects--leave-selection))
      (ygg-projects-refresh)
      (message "projects: %s %s" (if unpin "unpinned" "pinned")
               (mapconcat (lambda (s)
                            (if (ygg-projects--ended-p s)
                                (or (plist-get s :name) (plist-get s :agent) "session")
                              (aob-session-name s)))
                          targets ", ")))))

(defun ygg-projects--context-targets ()
  "The Context entries selected, or the one on this line."
  (if (ygg-projects--selecting-p)
      (ygg-projects--selected-entries 'context)
    (when-let* (((eq (get-text-property (line-beginning-position) 'ygg-row) 'context))
                (entry (get-text-property (line-beginning-position) 'ygg-entry))
                ((consp entry)))
      (list entry))))

(defun ygg-projects--send-context (send what)
  "Hand the Context entries selected, or the one here, to SEND.
WHAT names where they go, for when there is nothing to hand over."
  (let ((items (ygg-projects--context-targets)))
    (when (ygg-projects--in-sidebar-p) (ygg-projects--leave-selection))
    (unless items (user-error "projects: no Context entry here for %s" what))
    (funcall send items)))

(defun ygg-projects-context-quickfix ()
  "Send the Context entries selected, or the one here, to the quickfix."
  (interactive)
  (ygg-projects--send-context #'ygg-ice-send-quickfix "the quickfix"))

(defun ygg-projects-context-to-agent ()
  "Add the Context entries selected, or the one here, to the agent context."
  (interactive)
  (ygg-projects--send-context #'ygg-ice-send-context "the agent context"))

(defun ygg-projects-delete ()
  "Remove what this line stands for: a session for good, or a project.
A session that ran here is a conversation, so deleting it is deleting
that; a project row is only the list this sidebar keeps.  In visual
state, every session selected, after one question."
  (interactive)
  (if (ygg-projects--selecting-p)
      (ygg-projects--act "delete for good" #'ygg-projects--conversation-p
                         #'ygg-projects--delete-entry t)
    (ygg-projects--delete-entry (ygg-projects--entry-at-point))))

(defun ygg-projects--delete-entry (entry)
  (cond
   ((and (consp entry) (plist-member entry :acp-id))
    (aob-acp-forget-entry entry)
    (ygg-projects-refresh))
   ((aob-session-p entry)
    (aob-acp-delete-session entry)
    (ygg-projects-refresh))
   (t (ygg-projects-remove))))

(defun ygg-projects--ended-p (entry)
  (and (consp entry) (proper-list-p entry) (plist-member entry :acp-id) t))

(defun ygg-projects-resume ()
  "Start the conversation on this line up again, or each one selected."
  (interactive)
  (if (ygg-projects--selecting-p)
      (ygg-projects--act "resume" #'ygg-projects--ended-p
                         #'aob-acp-resume-entry t)
    (let ((entry (ygg-projects--entry-at-point)))
      (unless (ygg-projects--ended-p entry)
        (user-error "projects: no ended conversation on this line"))
      (when (y-or-n-p (format "Resume %s? " (or (plist-get entry :name)
                                                (plist-get entry :agent))))
        (aob-acp-resume-entry entry)
        (ygg-projects-refresh)))))

(defun ygg-projects--put-away (entry)
  "Put ENTRY away, by whichever record it is kept in.
A conversation this Emacs started is marked archived in the file it
keeps; one found on disk is not in that file at all, so marking it
there archives nothing and the row comes back.  Its own file moves
instead.  A pin goes with it: what is filed away no longer leads."
  (when-let* ((pins (ygg-projects--pins))
              ((member (plist-get entry :acp-id) pins)))
    (ygg-projects--save-pins (remove (plist-get entry :acp-id) pins)))
  (when (and (fboundp 'aob-acp-archive-entry)
             (not (plist-get entry :found)))
    (ignore-errors (aob-acp-archive-entry entry)))
  (when (fboundp 'aob-transcript-move)
    (ignore-errors (aob-transcript-move entry "archive")))
  (when (fboundp 'aob-transcript-forget) (aob-transcript-forget)))

(defun ygg-projects-archive ()
  "Put the conversation on this line away, keeping it resumable.
In visual state, every ended one selected."
  (interactive)
  (if (ygg-projects--selecting-p)
      (ygg-projects--act "archive" #'ygg-projects--ended-p #'ygg-projects--put-away)
    (let ((entry (ygg-projects--entry-at-point)))
      (cond
     ((and (consp entry) (plist-member entry :acp-id))
        (ygg-projects--put-away entry)
        (ygg-projects-refresh))
       ((aob-session-p entry)
        (user-error "projects: that one is still running — end it first"))
       (t (user-error "projects: no conversation on this line"))))))

(defun ygg-projects-archive-ask ()
  "Put the conversation on this line away, after asking.
One still running is ended first and archived on the next press: an
agent holding a conversation open is the reason it cannot be filed.
In visual state, every session selected, after one question."
  (interactive)
  (if (ygg-projects--selecting-p)
      (ygg-projects--act "end or archive" #'ygg-projects--conversation-p
                         #'ygg-projects--archive-entry t)
    (ygg-projects--archive-entry (ygg-projects--entry-at-point))))

(defun ygg-projects--archive-entry (entry)
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
   (t (user-error "projects: no conversation on this line"))))

(defun ygg-projects--on-import ()
  "Redraw while a project is being taken in, if anyone is watching."
  (when (get-buffer-window ygg-projects-buffer-name 'visible)
    (ygg-projects-refresh)))

(add-hook 'ygg-project-import-hook #'ygg-projects--on-import)

;; a conversation's opening line arrives after the list it belongs to
(defvar aob-transcript-titles-hook)
(defun ygg-projects--on-titles ()
  "Redraw once for a burst of titles read, if anyone is watching."
  (when (get-buffer-window ygg-projects-buffer-name 'visible)
    (ygg-projects--redraw-soon)))

(with-eval-after-load 'aob-transcript
  (add-hook 'aob-transcript-titles-hook #'ygg-projects--on-titles))

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

(defun ygg-projects-schedule ()
  "Schedule a prompt for the conversation on this line."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (unless (fboundp 'aob-schedule-read)
      (user-error "projects: nothing to schedule with"))
    (unless (and (ygg-projects--conversation-p entry)
                 (not (ygg-projects--subagent-entry-p entry)))
      (user-error "projects: no conversation here to schedule"))
    (apply #'aob-schedule (aob-schedule-read entry))))

(defun ygg-projects-schedules ()
  "List every scheduled prompt."
  (interactive)
  (if (fboundp 'aob-schedule-list)
      (aob-schedule-list)
    (user-error "projects: nothing to schedule with")))

(defvar ygg-conversations--index (make-hash-table :test #'equal)
  "Candidate string to the conversation it stands for.")

(declare-function aob-transcript-move "aob-transcript" (entry where &optional then))
(declare-function aob-acp-delete-entry "aob-acp" (entry &optional then))
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
          ((fboundp 'aob-transcript-view) (ygg-projects--open-ended entry))
          (t (user-error "projects: nothing to read it with")))))

(defun ygg-projects--open-ended (entry)
  "Read ENTRY's conversation, or resume it when only its agent keeps it.
A session the agent lists with no file here has nothing to read."
  (if (and (plist-get entry :listed)
           (not (ignore-errors (aob-transcript-file entry))))
      (aob-acp-resume-entry entry)
    (aob-transcript-view entry)))

(defun ygg-conversation-archive (candidate)
  "Put CANDIDATE away: kept, and out of the list."
  (interactive "sConversation: ")
  (let ((entry (ygg-conversations--entry candidate)))
    (ygg-projects--put-away entry)
    (ygg-projects-refresh)))

(defun ygg-conversation-discard (candidate &optional ask)
  "Move CANDIDATE out of the way, into a folder nothing reads.
ASK, as when called by hand, names it and asks first; under
`embark-act-all\=' the one question for all of them was already asked."
  (interactive (list (read-string "Conversation: ")
                     ;; act-all stubs its confirm out around each action it runs
                     (not (eq (symbol-function 'embark--confirm) #'ignore))))
  (let ((entry (ygg-conversations--entry candidate)))
    (when (or (not ask)
              (y-or-n-p (format "Discard “%s”? "
                                (or (plist-get entry :name)
                                    (plist-get entry :listed-title)
                                    (plist-get entry :acp-id)))))
      (cond ((ignore-errors (aob-transcript-file entry))
             (when (fboundp 'aob-transcript-move)
               (aob-transcript-move entry "discarded" #'ygg-projects-refresh)))
            ((and (fboundp 'aob-acp-delete-entry) (aob-acp-delete-entry entry))
             (puthash (plist-get entry :acp-id) t ygg-projects--discarded)))
      (ygg-projects-refresh))))

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
  (ygg-projects--forget-true-dirs)
  (ygg-projects-refresh)
  (ygg-projects--scan-commands)
  (ygg-projects--scan-worktrees)
  (ygg-projects--scan-docker)
  (ygg-projects--scan-context))

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

(defun ygg-projects--toggle-subagents (entry)
  "Fold or unfold the subagents of the lead ENTRY stands under; nil when
ENTRY is no session, or a lead with none, so TAB keeps its other use."
  (when-let* (((and (fboundp 'aob-session-p) (aob-session-p entry)))
              (lead (ygg-projects--sender entry))
              ((or (not (eq lead entry))
                   (and (fboundp 'aob-subagent-children)
                        (seq-some (lambda (k) (not (ygg-projects--ended-subagent-p k)))
                                  (aob-subagent-children lead))))))
    (let ((id (aob-session-id lead)))
      (setq ygg-projects--expanded
            (if (member id ygg-projects--expanded)
                (delete id ygg-projects--expanded)
              (cons id ygg-projects--expanded))))
    ;; a folded subagent's line is gone; its lead is where point belongs
    (unless (eq lead entry)
      (let ((pos (point-min)) found)
        (while (and (not found) (setq pos (next-single-property-change pos 'ygg-entry)))
          (when (eq (get-text-property pos 'ygg-entry) lead) (setq found pos)))
        (when found (goto-char found))))
    t))

(defun ygg-projects-toggle ()
  "Open what this line stands for: a project's rows, a row's entries, or
what a folder holds.
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
     ((ygg-projects--toggle-subagents
       (get-text-property (line-beginning-position) 'ygg-entry)))
     ((ygg-projects--toggle-folder
       (get-text-property (line-beginning-position) 'ygg-entry)))
     ;; a line under an umbrella's repository folds that repository, and
     ;; point goes back to its line, the one that stays
     ((and (get-text-property (line-beginning-position) 'ygg-entry)
           (ygg-projects--umbrella-of root)
           (ygg-projects--toggle-folder root))
      (when-let* ((pos (text-property-search-backward 'ygg-entry root #'equal)))
        (goto-char (prop-match-beginning pos))))
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
    (ygg-projects--open-root root)))

(defun ygg-projects--open-root (root)
  "Open ROOT as its own space, and in the sidebar: an umbrella's
repository opens its umbrella's card at its line."
  (let ((umbrella (ygg-projects--umbrella-of root)))
    (setq ygg-projects--open (or umbrella root))
    (when umbrella
      (cl-pushnew (cons umbrella 'folders) ygg-projects--open-row :test #'equal)
      (unless (ygg-projects--folder-open-p root)
        (ygg-projects--toggle-folder root))))
  (ygg-projects-refresh)
  (ygg-projects--keeping
    (if (fboundp 'ygg-space-open)
        (ygg-space-open (directory-file-name root))
      (dired root))))

;;;###autoload
(defun ygg-project-switch-child (child)
  "Open CHILD, one of the repositories of the umbrella you are in."
  (interactive
   (let* ((umbrella (or (cdr (ygg-project-try-umbrella default-directory))
                        (user-error "projects: not inside an umbrella")))
          (names (mapcar (lambda (c) (cons (directory-file-name
                                            (file-relative-name c umbrella))
                                           c))
                         (ygg-project-children umbrella))))
     (list (cdr (assoc (completing-read "Repository: " names nil t) names)))))
  (ygg-projects--open-root child))

(defun ygg-projects--move (n)
  "Move the project on this line N places, among its umbrella's repositories
when it is one of them, else down the list.  A repository's line in its
umbrella's Folders row moves that repository."
  (let* ((entry (ygg-projects--entry-at-point))
         (root (or (and (stringp entry) (ygg-projects--umbrella-of entry) entry)
                   (get-text-property (line-beginning-position) 'ygg-project)
                   (user-error "projects: no project on this line"))))
    (if (ygg-projects--umbrella-of root)
        (ygg-project-move-child root n)
      (ygg-project-move root n))
    (ygg-projects-refresh)))

(defun ygg-projects-move-down ()
  "Move the project on this line one place down."
  (interactive)
  (ygg-projects--move 1))

(defun ygg-projects-move-up ()
  "Move the project on this line one place up."
  (interactive)
  (ygg-projects--move -1))

(defun ygg-projects-visit ()
  "Act on the row under point, the one row even in visual state."
  (interactive)
  (when (and (ygg-projects--selecting-p) (fboundp 'ygg-normal-state))
    (ygg-normal-state))
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
             (ygg-projects--open-ended entry))
            (t (when (fboundp 'ygg-aob-goto-space) (ygg-aob-goto-space entry))
               (aob-trace entry))))
          ('commands (if (fboundp 'ygg-project-commands-run)
                         (ygg-project-commands-run entry)
                       (let ((default-directory root))
                         (compile (format "just %s" entry)))))
          ('folders
           (cond ((eq (car-safe entry) 'worktree)
                  (if (fboundp 'ygg-space-open)
                      (ygg-space-open (cdr entry))
                    (dired (cdr entry))))
                 ((ygg-projects--umbrella-of entry) (ygg-projects--open-root entry))
                 (t (dired entry))))
          ('processes
           (if (and (consp entry) (eq (car entry) 'docker))
               (ygg-projects--docker-logs root (plist-get (cdr entry) :name))
             (pop-to-buffer entry)))
          ('context (ygg-ice-visit-item entry)))
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
      ('context (let ((default-directory root)) (ygg-ice-changes-list))))))))

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
    (define-key map (kbd "M-j") #'ygg-projects-move-down)
    (define-key map (kbd "M-k") #'ygg-projects-move-up)
    (define-key map "u" #'ygg-project-switch-child)
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
    (define-key map "C" #'ygg-projects-cancel)
    (define-key map "a" #'ygg-projects-say)
    (define-key map "p" #'ygg-projects-toggle-pin)
    (define-key map "s" #'ygg-projects-schedule)
    (define-key map "S" #'ygg-projects-schedules)
    (define-key map "V" #'ygg-toggle-visual)
    (define-key map "Q" #'ygg-projects-context-quickfix)
    (define-key map "c" #'ygg-projects-context-to-agent)
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

(defun ygg-projects--anchor ()
  "Begin a selection of rows on the row it was started from."
  (set-marker (mark-marker) (line-beginning-position)))

(defvar-local ygg-projects--selection-overlay nil)

(defun ygg-projects--paint-selection ()
  "Light the rows a visual selection holds, the region face being off here."
  (if (ygg-projects--selecting-p)
      (let ((beg (save-excursion (goto-char (min (point) (mark t)))
                                 (line-beginning-position)))
            (end (save-excursion (goto-char (max (point) (mark t)))
                                 (line-beginning-position 2))))
        (unless ygg-projects--selection-overlay
          (setq ygg-projects--selection-overlay (make-overlay beg end))
          (overlay-put ygg-projects--selection-overlay 'face 'ygg-projects-current))
        (move-overlay ygg-projects--selection-overlay beg end))
    (when ygg-projects--selection-overlay
      (delete-overlay ygg-projects--selection-overlay)
      (setq ygg-projects--selection-overlay nil))))

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
          (with-current-buffer buf (ygg-projects--trim-window))
          (when ygg-projects--stale (ygg-projects-refresh)))))))

(add-hook 'window-configuration-change-hook #'ygg-projects--restore)

(defun ygg-projects--follow-trace (&rest _)
  "Redraw when another conversation comes on screen, so the fill follows it,
or when the sidebar comes back with a redraw it missed while away."
  (when (or ygg-projects--stale
            (not (equal (ygg-projects--traced-ids) ygg-projects--on-screen)))
    (ygg-projects-refresh)))

(add-hook 'window-configuration-change-hook #'ygg-projects--follow-trace)

(defvar ygg-projects--redraw-timer nil)

(defun ygg-projects--redraw-soon (&rest _)
  "Redraw shortly: a burst of session changes is one redraw, not fifty.
A plain timer and not an idle one — an idle timer made while Emacs is
already idle waits for the next keystroke, and agents finish their
turns while nobody is typing.  Out of sight, it only notes one is due."
  (cond
   ((not (get-buffer-window ygg-projects-buffer-name t))
    (setq ygg-projects--stale t))
   ((not (timerp ygg-projects--redraw-timer))
    (setq ygg-projects--redraw-timer
          (run-with-timer 0.3 nil
                          (lambda ()
                            (setq ygg-projects--redraw-timer nil)
                            (ygg-projects-refresh)))))))

(defvar aob-session-created-hook)
(defvar aob-session-removed-hook)
(defvar aob-state-change-hook)
(defvar aob-meter-change-hook)
(with-eval-after-load 'aob
  (add-hook 'aob-session-created-hook #'ygg-projects--redraw-soon)
  (add-hook 'aob-session-removed-hook #'ygg-projects--redraw-soon)
  (add-hook 'aob-state-change-hook #'ygg-projects--redraw-soon)
  (add-hook 'aob-meter-change-hook #'ygg-projects--redraw-soon))

;; a list ticking changes a badge without any session event the hooks above see
(defvar aob-subagent-progress-functions)
(defvar ygg-todo-changed-functions)
(with-eval-after-load 'aob-subagent
  (add-hook 'aob-subagent-progress-functions #'ygg-projects--redraw-soon))
(with-eval-after-load 'ygg-todo
  (add-hook 'ygg-todo-changed-functions #'ygg-projects--redraw-soon))
(defvar ygg-ice-context-changed-functions)
(with-eval-after-load 'ygg-ice
  (add-hook 'ygg-ice-context-changed-functions #'ygg-projects--redraw-soon))
(defvar aob-schedule-changed-hook)
(with-eval-after-load 'aob-schedule
  (add-hook 'aob-schedule-changed-hook #'ygg-projects--redraw-soon))

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
    (add-hook 'post-command-hook #'ygg-projects--paint-selection 91 t)
    (add-hook 'ygg-visual-entry-hook #'ygg-projects--anchor nil t)
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
      (let ((root (file-name-as-directory (expand-file-name (project-root pr)))))
        (setq ygg-projects--here (or (ygg-projects--umbrella-of root) root))))
    (unless ygg-projects--open (setq ygg-projects--open ygg-projects--here))
    (let ((buf (get-buffer ygg-projects-buffer-name)))
      (unless (and buf (buffer-local-value 'ygg-projects--instance buf))
        ;; vui-mount ends in `switch-to-buffer', which would leave the
        ;; sidebar showing in the main window as well as its own
        (let* ((cards (ygg-projects--fresh-picture))
               (inst (save-window-excursion
                       (vui-mount (vui-component 'ygg-projects-view :cards cards)
                                  ygg-projects-buffer-name))))
          (setq buf (get-buffer ygg-projects-buffer-name))
          (with-current-buffer buf
            (setq ygg-projects--instance inst
                  ygg-projects--drawn cards))))
      (ygg-projects--setup buf)
      (ygg-projects--scan-commands)
      (ygg-projects--scan-worktrees)
      (ygg-projects--scan-docker)
      (ygg-projects--scan-context)
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
      (ygg-projects-refresh)
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
