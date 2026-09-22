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
(declare-function ygg-aob-session-subagents "layer-aob" (s))
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

(defcustom ygg-projects-width 34
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

(defvar ygg-projects--open-row nil
  "Conses of (ROOT . KIND) whose entries are listed.
A set, not one at a time: opening the commands of a project is not a
reason to put its sessions away, and a row that closed itself because
you looked at another is a row you have to open twice.")

;;; What each project is running

(defun ygg-projects--sessions (root)
  (when (fboundp 'aob-sessions)
    (seq-filter (lambda (s)
                  (when-let* ((p (aob-session-project s)))
                    (equal (file-name-as-directory (expand-file-name p)) root)))
                (aob-sessions))))

(defun ygg-projects--past (root)
  "Conversations in ROOT that ended but can be picked up again.
The ones this Emacs started, and the ones the CLI left on disk before
it ever did — a project you have just taken in has a history whether
or not this Emacs was there for it."
  (let* ((known (and (fboundp 'aob-acp-resumable-entries)
                     (seq-filter
                      (lambda (e)
                        (equal root (file-name-as-directory
                                     (expand-file-name (or (plist-get e :project)
                                                           (plist-get e :dir) "/")))))
                      (ignore-errors (aob-acp-resumable-entries)))))
         (ids (mapcar (lambda (e) (plist-get e :acp-id)) known))
         (found (and (fboundp 'aob-transcript-found)
                     (seq-remove (lambda (e) (member (plist-get e :acp-id) ids))
                                 (ignore-errors (aob-transcript-found root))))))
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
             :command '("docker" "ps" "--format"
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
  "What ROOT has running that you can go and look at."
  (let ((n (+ (length (cdr (ygg-projects--root-buffers root)))
              (length (ygg-projects--containers root)))))
    (cons n n)))

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

(defun ygg-projects--entries (root kind)
  "The things ROOT's KIND row stands for: (LABEL . PAYLOAD) each."
  (pcase kind
    ('agents (let ((all (ygg-projects--sessions root))
                   out)
               ;; no result form here: `nreverse' would rewire the list
               ;; and leave OUT pointing at what is now its last cell,
               ;; which is one session however many are running
               (dolist (s (seq-remove (lambda (x)
                                        (and (fboundp 'aob-subagent-p)
                                             (aob-subagent-p x)))
                                      all))
                 (push (cons (aob-session-name s) s) out)
                 ;; what it sent, under it, marked rather than indented: a
                 ;; row this narrow has no columns to spare.  A spawned one
                 ;; is a session of its own; a reported one is a plist the
                 ;; agent mentioned and nothing can be done with
                 (dolist (kid (and (fboundp 'aob-subagent-children)
                                   (aob-subagent-children s)))
                   (push (cons (format "└ %s" (aob-session-name kid)) kid) out))
                 (dolist (sub (and (fboundp 'ygg-aob-session-subagents)
                                   (ygg-aob-session-subagents s)))
                   (push (cons (format "└ %s" (or (plist-get sub :title)
                                                  (plist-get sub :name)
                                                  "subagent"))
                               (cons s sub))
                         out)))
               ;; ended, but the conversation is still there to pick up
               (dolist (e (ygg-projects--past root))
                 (push (cons (or (plist-get e :name) (plist-get e :agent) "session")
                             e)
                       out))
               (setq out (nreverse out))))
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

(defun ygg-projects--entry-badge (payload)
  "What PAYLOAD has to say for itself at the right edge.
A running conversation says what it is doing; one that ended says how
long ago, since a list of six conversations from today is told apart
by when, not by that they were all today."
  (cond ((and (fboundp 'aob-session-p) (aob-session-p payload))
         (format "%s" (aob-session-state payload)))
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
  (let* ((badge (ygg-projects--entry-badge payload))
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
                          (propertize badge 'font-lock-face 'ygg-projects-count)))
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
                 (ygg-projects--right ""))
         'ygg-project root 'ygg-row kind 'ygg-entry payload 'ygg-cont t))))))

(defun ygg-projects--entry-nodes (root kind)
  (mapcar (lambda (cell)
            (vui-text (ygg-projects--entry-text (car cell) root kind (cdr cell))))
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

(defun ygg-projects--right (text)
  "TEXT pushed to the right edge, with the line padded out behind it.
The target is the window edge, not a column count: a nerd-icon glyph is
drawn wider than the one column it measures, so a numeric `align-to'
leaves icon rows long and iconless rows short, and the card behind them
ends in a ragged edge.  Two columns are held back so the last glyph
cannot spill past the text area and mark every line truncated."
  (concat (propertize " " 'display
                      `(space :align-to (- right ,(+ 2 (string-width text)))))
          text
          (propertize " " 'display '(space :align-to right))))

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

(defun ygg-projects-archive ()
  "Put the conversation on this line away, keeping it resumable."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (cond
     ((and (consp entry) (plist-member entry :acp-id))
      (aob-acp-archive-entry entry)
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
        (aob-acp-archive-entry entry)
        (ygg-projects-refresh)))
     ;; a conversation opened for reading is a session object with no
     ;; agent behind it: there is nothing to end, only something to file
     ((and (aob-session-p entry)
           (or (aob-session-ref entry :asleep)
               (memq (aob-session-state entry) '(done dead failed))))
      (when (y-or-n-p (format "Archive %s? " (aob-session-name entry)))
        (when-let* ((past (aob-session-ref entry :asleep)))
          (aob-acp-archive-entry past))
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

(defun ygg-projects-rescan ()
  "Redraw, and look again for what the projects can run."
  (interactive)
  (when (fboundp 'aob-transcript-forget) (aob-transcript-forget))
  (ygg-projects-refresh)
  (ygg-projects--scan-commands)
  (ygg-projects--scan-worktrees)
  (ygg-projects--scan-docker))

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
          ;; a session is a struct, a delegation the pair it came from
          ('agents
           (cond
            ;; a persisted conversation is a plist; a live one a struct,
            ;; and a delegation the pair it came from
            ;; reading what was said costs nothing; resuming starts an
            ;; agent, so that is a different key
            ((and (consp entry) (plist-member entry :acp-id))
             (aob-transcript-view entry))
            ((consp entry) (aob-subagents (car entry)))
            (t (aob-trace entry))))
          ('commands (if (fboundp 'ygg-project-commands-run)
                         (ygg-project-commands-run entry)
                       (let ((default-directory root))
                         (compile (format "just %s" entry)))))
          ('folders (dired entry))
          ('processes
           (if (and (consp entry) (eq (car entry) 'docker))
               (let ((default-directory root)
                     (name (plist-get (cdr entry) :name)))
                 (async-shell-command
                  (format "docker logs --tail 200 -f %s" (shell-quote-argument name))
                  (format "*docker: %s*" name)))
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
      ('processes (let ((default-directory root))
                    (if (fboundp 'ghostel) (call-interactively #'ghostel)
                      (user-error "projects: no terminal"))))
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
    (define-key map "n" #'ygg-projects-next)
    (define-key map "p" #'ygg-projects-prev)
    (define-key map "g" #'ygg-projects-rescan)
    (define-key map "+" #'project-switch-project)
    (define-key map "A" #'ygg-projects-add)
    (define-key map "I" #'ygg-projects-import)
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

(provide 'ygg-projects)
;;; ygg-projects.el ends here
