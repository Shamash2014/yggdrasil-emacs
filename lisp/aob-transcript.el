;;; aob-transcript.el --- a conversation that ended, opened as itself -*- lexical-binding: t; -*-

;;; Commentary:
;; A session that ended is still a session.  It has a name, a folder, a
;; conversation and an id that can bring its agent back — everything a
;; running one has except the process.  So it opens as one: the same
;; object, the same trace, the same keys.  There is no second kind of
;; window for old work.
;;
;; Nothing is started by opening it.  The turns are read from the file
;; the CLI wrote as they happened, under the config home the session ran
;; with.  The agent comes back when you write to it, and not before.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob)
(require 'aob-trace)

(declare-function ygg-agent--config-env "ygg-agent-conf" (preset cmd project &optional isolate))
(declare-function ygg-agent--own-home "ygg-agent-conf" (kind repo &optional isolate))
(declare-function ygg-agent--repo-home "ygg-agent-conf" (project))
(declare-function aob-acp-resume-entry "aob-acp" (e &optional pref))

(defgroup aob-transcript nil
  "Conversations that already happened."
  :group 'aob :prefix "aob-transcript-")

(defvar aob-transcript--homes (make-hash-table :test 'equal)
  "(AGENT . DIR) to config home: working it out reads settings and
keychains, and every row of the sidebar asks for it.")

(defun aob-transcript--home (agent dir)
  "The config home AGENT used in DIR, or its default one."
  (let ((key (cons agent dir)))
    (or (gethash key aob-transcript--homes)
        (puthash key (aob-transcript--home-1 agent dir) aob-transcript--homes))))

(defun aob-transcript--home-1 (agent dir)
  "Work out AGENT's config home in DIR from its configuration."
  (or (when (fboundp 'ygg-agent--config-env)
        (when-let* ((entry (ygg-agent--config-env agent agent dir))
                    ((string-match "=\\(.*\\)\\'" entry)))
          (match-string 1 entry)))
      (expand-file-name (if (equal agent "codex") "~/.codex" "~/.claude"))))

(defun aob-transcript--slug (dir)
  "DIR as the CLI spells it when naming a folder.
Every character that is not a letter or a digit becomes a dash — the
dots included, which is how a worktree of justfin.git is filed under
justfin-git and not under justfin.git."
  (replace-regexp-in-string "[^A-Za-z0-9]" "-"
                            (directory-file-name (expand-file-name dir))))

(defun aob-transcript--own-home (agent dir)
  "The home this config keeps for DIR, whether or not sessions use it.
A home whose login has lapsed is passed over when a session starts, and
still holds everything it was written before that."
  (let ((key (list 'own agent dir)))
    (let ((known (gethash key aob-transcript--homes)))
      (unless known
        (setq known (or (and (fboundp 'ygg-agent--own-home)
                             (bound-and-true-p ygg-agent-conf-root)
                             (ignore-errors
                               (ygg-agent--own-home
                                agent (ygg-agent--repo-home dir))))
                        'none))
        (puthash key known aob-transcript--homes))
      (unless (eq known 'none) known))))

(defun aob-transcript--homes (agent dir)
  "Every config home AGENT may have written DIR\='s conversations under.
The one a session from here runs with, the one this config keeps for
the project, and the one the CLI uses when you start it yourself — a
project has a history from all three."
  (delete-dups
   (delq nil (list (aob-transcript--home agent dir)
                   (aob-transcript--own-home agent dir)
                   (expand-file-name
                    (if (equal agent "codex") "~/.codex" "~/.claude"))))))

(defvar aob-transcript--codex-files (make-hash-table :test 'equal)
  "Codex session id to its file: finding one walks a folder per day.")

(defun aob-transcript--codex-file (id homes)
  "The file Codex wrote session ID to under one of HOMES.
Codex files by the day it started, not by project, as
sessions/YYYY/MM/DD/rollout-<time>-ID.jsonl."
  (let ((known (gethash id aob-transcript--codex-files)))
    (if (and known (file-readable-p known))
        known
      (when-let* ((file (seq-some
                         (lambda (home)
                           (car (append
                                 (file-expand-wildcards
                                  (expand-file-name
                                   (format "sessions/*/*/*/rollout-*-%s.jsonl" id) home))
                                 (file-expand-wildcards
                                  (expand-file-name
                                   (format "archived_sessions/rollout-*-%s.jsonl" id) home)))))
                         homes)))
        (puthash id file aob-transcript--codex-files)))))

(defun aob-transcript-file (entry)
  "Where ENTRY's conversation was written, if it is still there."
  (or (when-let* ((file (plist-get entry :file)) ((file-readable-p file))) file)
      (when-let* ((id (plist-get entry :acp-id))
                  (dir (or (plist-get entry :dir) (plist-get entry :project))))
        (let* ((agent (or (plist-get entry :agent) "claude"))
               (homes (aob-transcript--homes agent dir)))
          (if (equal agent "codex")
              (aob-transcript--codex-file id homes)
            (seq-some
             (lambda (home)
               (let ((file (expand-file-name
                            (format "projects/%s/%s.jsonl" (aob-transcript--slug dir) id)
                            home)))
                 (and (file-readable-p file) file)))
             homes))))))

(defvar aob-transcript--titles (make-hash-table :test 'equal)
  "File to (MTIME . TITLE): reading the head of one is not free.")

(defvar aob-transcript--found (make-hash-table :test 'equal)
  "Project to (MTIME . ENTRIES): the listing is redone when the folder moves.")

(defun aob-transcript--mtime (file)
  (float-time (file-attribute-modification-time (file-attributes file))))

(defvar aob-transcript-titles-hook nil
  "Run with no arguments when titles read in the background land.")

(defvar aob-transcript--queue nil
  "Files whose opening line is still to be read.")

(defvar aob-transcript--timer nil)

(defcustom aob-transcript-titles-per-tick 6
  "Conversations whose opening line is read in one go of the idle timer."
  :type 'natnum :group 'aob-transcript)

(defun aob-transcript--title-cached (file)
  "FILE\='s opening line if it has already been read, else nil."
  (let ((cell (gethash file aob-transcript--titles)))
    (when (and cell (equal (car cell) (aob-transcript--mtime file)))
      (cdr cell))))

(defun aob-transcript--title (file)
  "What the conversation in FILE opened with, reading it if need be.
Cached against the file\='s own clock: this runs for every row of the
sidebar, and a row is drawn whenever anything moves."
  (or (aob-transcript--title-cached file)
      (let ((title (aob-transcript--title-1 file)))
        (puthash file (cons (aob-transcript--mtime file) title)
                 aob-transcript--titles)
        title)))

(defun aob-transcript--want-title (file)
  "Ask for FILE\='s opening line to be read when there is a moment."
  (unless (member file aob-transcript--queue)
    (setq aob-transcript--queue (append aob-transcript--queue (list file))))
  (unless aob-transcript--timer
    (setq aob-transcript--timer
          (run-with-idle-timer 0.2 t #'aob-transcript--read-some))))

(defun aob-transcript--read-some ()
  "Read the next few queued openings, then let whoever draws know."
  (let ((n aob-transcript-titles-per-tick)
        (any nil))
    (while (and (> n 0) aob-transcript--queue)
      (let ((file (pop aob-transcript--queue)))
        (when (file-readable-p file)
          (aob-transcript--title file)
          (setq any t)))
      (setq n (1- n)))
    (when any
      ;; the listing was built from what was cached at the time
      (clrhash aob-transcript--found)
      (run-hooks 'aob-transcript-titles-hook))
    (unless aob-transcript--queue
      (when aob-transcript--timer
        (cancel-timer aob-transcript--timer)
        (setq aob-transcript--timer nil)))))

(defun aob-transcript--title-1 (file)
  "Read FILE\='s opening line off the disk, a little of it at a time."
  (or (aob-transcript--title-in file 16384)
      (and (> (or (file-attribute-size (file-attributes file)) 0) 16384)
           (aob-transcript--title-in file 262144))))

(defun aob-transcript--title-in (file bytes)
  "FILE\='s opening line, looking only at its first BYTES."
  (with-temp-buffer
    ;; the opening line is at the top: a small read first, and the
    ;; larger one only where a preamble pushed it down
    (ignore-errors (insert-file-contents file nil 0 bytes))
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (rec (and (not (string-empty-p line))
                         (ignore-errors
                           (json-parse-string line :object-type 'alist
                                              :null-object nil
                                              :false-object nil)))))
          (when-let* ((turn (and rec (aob-transcript--turn rec)))
                      ((equal (car turn) "user")))
            (when-let* ((text (cdr turn))
                        ;; the harness writes its own preamble in as a
                        ;; user turn; the first thing a person said is
                        ;; what this is after
                        ((not (string-prefix-p "<" text)))
                        ((not (string-prefix-p "Caveat:" text)))
                        ((not (string-prefix-p "# AGENTS.md" text))))
              (throw 'found
                     (truncate-string-to-width
                      (car (split-string text "\n" t)) 44 nil nil t)))))
        (forward-line 1))
      nil)))

;;;###autoload
(defun aob-transcript-found (project &optional agent where)
  "Conversations AGENT left on disk for PROJECT, newest first.
The CLI writes one file per conversation under its config home.  What
this Emacs knows about is what it started itself, which for a project
you have only just taken in is none of them."
  (let* ((agent (or agent (bound-and-true-p aob-acp-default-agent) "claude"))
         (dirs (seq-filter
                #'file-directory-p
                (mapcar (lambda (home)
                          (expand-file-name
                           (format "projects/%s%s" (aob-transcript--slug project)
                                   (if where (concat "/" where) ""))
                           home))
                        (aob-transcript--homes agent project))))
         (key (list agent dirs where)))
    (when dirs
      ;; the folder's own clock says when a conversation was added to it
      ;; or written to; until it moves, the listing stands
      (let ((stamp (mapcar #'aob-transcript--mtime dirs))
            (cell (gethash key aob-transcript--found)))
        (if (and cell (equal (car cell) stamp))
            (cdr cell)
          (let ((entries (aob-transcript--found-1 project agent dirs where)))
            (puthash key (cons stamp entries) aob-transcript--found)
            entries))))))

(defun aob-transcript--found-1 (project agent dirs &optional where)
  "Read DIRS, which hold AGENT\='s conversations about PROJECT.
WHERE, when given, is the folder they were put away in."
  (let ((files (sort (seq-mapcat (lambda (dir) (directory-files dir t "\\.jsonl\\'"))
                                 dirs)
                         (lambda (a b)
                           (time-less-p
                            (file-attribute-modification-time (file-attributes b))
                            (file-attribute-modification-time (file-attributes a)))))))
        (let (seen)
          (mapcar (lambda (file)
                    (let* ((entry (list :agent agent
                                        :acp-id (file-name-base file)
                                        :project (file-name-as-directory
                                                  (expand-file-name project))
                                        :dir (file-name-as-directory
                                              (expand-file-name project))
                                        :ts (float-time
                                             (file-attribute-modification-time
                                              (file-attributes file)))
                                        ;; which home it came out of is
                                        ;; not derivable from the entry
                                        :file file
                                        :found t
                                        :archived (and where t)))
                           ;; the list first: an opening line is a read,
                           ;; and a hundred reads is not a listing.  What
                           ;; has been read is used, the rest is asked
                           ;; for and arrives on a later draw
                           (name (or (aob-transcript--title-cached file)
                                     (progn (aob-transcript--want-title file)
                                            (aob-transcript--name entry file))))
                           (name (if (member name seen)
                                     (format "%s %s" name
                                             (substring (plist-get entry :acp-id) 0 4))
                                   name)))
                      (push name seen)
                      (plist-put entry :name name)))
                  files))))

(defun aob-transcript-move (entry where)
  "Move ENTRY\='s conversation into the WHERE folder beside it.
Nothing is destroyed: archiving and discarding are both a move, and a
folder the listing does not read is what \"gone\" means here."
  (when-let* ((file (aob-transcript-file entry)))
    (let* ((dir (expand-file-name where (file-name-directory file)))
           (to (expand-file-name (file-name-nondirectory file) dir)))
      (make-directory dir t)
      (rename-file file to t)
      (aob-transcript-forget)
      to)))

;;;###autoload
(defun aob-transcript-forget ()
  "Drop what was read from disk, so the next look reads it again.
The folder\='s clock catches a conversation being written; a config
home moving is a change of mind, and nothing on disk says when."
  (interactive)
  (clrhash aob-transcript--homes)
  (clrhash aob-transcript--found)
  (clrhash aob-transcript--titles)
  (clrhash aob-transcript--codex-files)
  (setq aob-transcript--queue nil))

(defun aob-transcript--text (content)
  "The words in CONTENT, whatever shape the record used for it."
  (cond
   ((stringp content) content)
   ;; a json array parses to a vector, not a list
   ((seqp content)
    (string-join
     (append (delq nil
                   (seq-map (lambda (part)
                              (let ((type (alist-get 'type part)))
                                (cond
                                 ((member type '("text" "input_text" "output_text"))
                                  (alist-get 'text part))
                                 ((equal type "tool_use")
                                  (format "· %s" (or (alist-get 'name part) "tool")))
                                 (t nil))))
                            content))
             nil)
     "\n"))
   (t nil)))

(defun aob-transcript--insert-tail (file lines)
  "Insert the last LINES lines of FILE into the current buffer.
A conversation's log is written whole — tool results and all — and runs
to tens of megabytes; only its end is ever shown, so only its end is
read.  The window is widened until it holds enough lines or reaches the
start of the file."
  (let* ((size (file-attribute-size (file-attributes file)))
         (span (min size 262144))
         (whole nil))
    (while (progn
             (erase-buffer)
             (setq whole (>= span size))
             (insert-file-contents file nil (- size span) size)
             (and (not whole)
                  (< (count-lines (point-min) (point-max)) (1+ lines))
                  (setq span (min size (* span 8))))))
    (goto-char (point-min))
    ;; the first line of a window that starts mid-file is half a record
    (unless whole
      (forward-line 1)
      (delete-region (point-min) (point)))
    (let ((extra (- (count-lines (point-min) (point-max)) lines)))
      (when (> extra 0)
        (goto-char (point-min))
        (forward-line extra)
        (delete-region (point-min) (point))))))

(defun aob-transcript-turns (file)
  "FILE as a list of (WHO . TEXT), oldest first.
Only its last records: a session keeps `aob-event-cap' events and drops
the older half past that, so reading further back is work thrown away."
  (let (out)
    (with-temp-buffer
      (aob-transcript--insert-tail file aob-event-cap)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (rec (and (not (string-empty-p line))
                         (ignore-errors
                           (json-parse-string line :object-type 'alist
                                              :null-object nil :false-object nil))))
               (turn (and rec (aob-transcript--turn rec))))
          (when turn (push turn out)))
        (forward-line 1)))
    (nreverse out)))

(defun aob-transcript--harness-part-p (part)
  "Whether PART of a user turn was written in by the harness, not typed."
  (when-let* ((text (alist-get 'text part)))
    (string-match-p "\\`\\(?:<\\|\\[workspace: \\|# AGENTS\\.md\\)" text)))

(defun aob-transcript--turn (rec)
  "REC as (WHO . TEXT) when it is something said, else nil.
Claude writes a turn as {type: user|assistant, message: {content}};
Codex as {type: response_item, payload: {type: message, role, content}}."
  (let* ((codex (equal (alist-get 'type rec) "response_item"))
         (body (alist-get (if codex 'payload 'message) rec))
         (who (if codex
                  (and (equal (alist-get 'type body) "message")
                       (alist-get 'role body))
                (alist-get 'type rec))))
    (when-let* (((member who '("user" "assistant")))
                (content (alist-get 'content body))
                (content (if (and (equal who "user") (vectorp content))
                             (seq-remove #'aob-transcript--harness-part-p content)
                           content))
                (text (aob-transcript--text content))
                (text (string-trim text))
                ((not (string-empty-p text))))
      (cons who text))))

;;; Opening one

(defun aob-transcript--name (entry file)
  "What to call ENTRY's session, told apart from the others.
Every conversation with one agent is called the same thing, and a trace
is named after its session — so without this, opening a second one
walks into the first one's buffer."
  (let* ((base (or (plist-get entry :name) (plist-get entry :agent) "session"))
         (day (and file (format-time-string
                         "%b %-d"
                         (file-attribute-modification-time
                          (file-attributes file)))))
         (name (if day (format "%s · %s" base day) base)))
    ;; two on the same day still need telling apart
    (if (seq-find (lambda (s) (equal (aob-session-name s) name)) (aob-sessions))
        (format "%s %s" name (substring (or (plist-get entry :acp-id) "") 0 4))
      name)))

(defun aob-transcript--session (entry file)
  "ENTRY as a session object read from FILE, its turns already in it.
Asleep: it carries the id its agent answers to, and no process."
  (let* ((id (concat "acp:" (plist-get entry :acp-id)))
         (existing (aob-session-get id)))
    (or existing
        (let ((s (aob-create-session
                  :id id :backend 'acp
                  :name (if (or (plist-get entry :found)
                                ;; already named for its day when it was
                                ;; found, or named by you: no day added
                                (plist-get entry :named-by-user))
                            (plist-get entry :name)
                          (aob-transcript--name entry file))
                  :project (or (plist-get entry :project) (plist-get entry :dir))
                  :dir (or (plist-get entry :dir) (plist-get entry :project))
                  :state 'done)))
          (aob-session-put s :agent (plist-get entry :agent))
          (aob-session-put s :acp-id (plist-get entry :acp-id))
          (aob-session-put s :model-id (plist-get entry :model))
          (aob-session-put s :mode-id (plist-get entry :mode))
          (aob-session-put s :asleep entry)
          (aob-session-put s :named-by-user (plist-get entry :named-by-user))
          (aob-session-put s :auto-named (plist-get entry :auto-named))
          (dolist (turn (aob-transcript-turns file))
            (aob-event s (if (equal (car turn) "user") 'prompt 'message)
                       :text (cdr turn)
                       ;; the user turns of a written conversation are yours
                       :typed (equal (car turn) "user")))
          s))))

;;;###autoload
(defun aob-transcript-view (entry)
  "Open ENTRY's conversation as the session it was.
Nothing is started: writing to it is what brings its agent back."
  (interactive
   (list (let* ((entries (append (and (fboundp 'aob-acp-resumable-entries)
                                      (aob-acp-resumable-entries))
                                 (and (fboundp 'aob-acp-archived-entries)
                                      (aob-acp-archived-entries))))
                (rows (mapcar (lambda (e)
                                (cons (format "%s · %s"
                                              (or (plist-get e :name) "session")
                                              (abbreviate-file-name
                                               (or (plist-get e :dir) "")))
                                      e))
                              entries)))
           (unless rows (user-error "aob: no past conversations"))
           (cdr (assoc (completing-read "Open: " (mapcar #'car rows) nil t) rows)))))
  (let* ((file (or (aob-transcript-file entry)
                   (user-error "aob: no transcript on disk for %s"
                               (or (plist-get entry :name) "that session"))))
         (s (aob-transcript--session entry file))
         (buf (aob-trace-buffer s))
         ;; creating the session already put its trace on screen; showing
         ;; it again is how one conversation ends up in two windows
         (win (or (get-buffer-window buf 'visible)
                  (display-buffer buf (or (bound-and-true-p ygg-aob-trace-action)
                                          t)))))
    (when (window-live-p win) (select-window win))
    s))

(defun aob-transcript-asleep-p (s)
  "Whether S is a conversation whose agent is not running."
  (and (aob-session-ref s :asleep) t))

(declare-function aob-trace--name "aob-trace" (s))
(declare-function aob-trace--render "aob-trace" (&optional full))
(defvar aob-trace--blocks)
(defvar aob-trace--session-id)
(defvar aob-buffer-session-id)

(defun aob-transcript--hand-over (old live)
  "Give OLD\='s trace buffer to LIVE, and its windows with it.
A resumed conversation is a new session object under a new name, and
a new name is a new buffer: what you were reading would be left in a
buffer nothing writes to, beside a fresh one that looks empty."
  (when-let* ((buf (get-buffer (aob-trace--name old))))
    (let* ((wanted (aob-trace--name live))
           (clash (unless (equal wanted (buffer-name buf)) (get-buffer wanted)))
           (wins (and clash (get-buffer-window-list clash nil t))))
      (when clash (kill-buffer clash))
      (with-current-buffer buf
        (unless (equal (buffer-name) wanted) (rename-buffer wanted))
        (setq aob-trace--session-id (aob-session-id live))
        (setq aob-buffer-session-id (aob-session-id live))
        (setq aob-trace--blocks nil)
        (let ((inhibit-read-only t)) (erase-buffer))
        (aob-trace--render t))
      (setf (aob-session-buffer live) buf)
      ;; the window the resume opened shows what we already have open:
      ;; one conversation, one window, rather than the same trace twice
      (dolist (win wins)
        (when (window-live-p win)
          (if (get-buffer-window buf (window-frame win))
              (unless (eq win (frame-root-window (window-frame win)))
                (ignore-errors (delete-window win)))
            (set-window-buffer win buf))))
      buf)))

(defun aob-transcript--wake (fn s &rest args)
  "Bring S's agent back before sending to it, if it is asleep.
The session the resume makes is the live one; this one has served its
purpose and would otherwise sit in every list beside it."
  (if-let* ((entry (aob-session-ref s :asleep)))
      (let ((live (aob-acp-resume-entry entry)))
        (unless live
          (user-error "aob: %s would not come back" (aob-session-name s)))
        (aob-session-put s :asleep nil)
        (aob-transcript--hand-over s live)
        (ignore-errors (aob-remove-session s))
        (apply fn live args))
    (apply fn s args)))

(advice-add 'aob-prompt :around #'aob-transcript--wake)

;;;###autoload
(defun aob-transcript-wake (s)
  "Bring S's agent back without saying anything to it.
Writing to a sleeping conversation wakes it anyway; this is for when
you want it awake first — to set a mode or a model, or just to have it
there."
  (interactive (list (aob-target)))
  (let ((entry (or (aob-session-ref s :asleep)
                   (user-error "aob: %s is already awake" (aob-session-name s)))))
    (let ((live (aob-acp-resume-entry entry)))
      (unless live
        (user-error "aob: %s would not come back" (aob-session-name s)))
      (aob-session-put s :asleep nil)
      (aob-transcript--hand-over s live)
      (ignore-errors (aob-remove-session s))
      (when (fboundp 'aob-trace) (aob-trace live))
      live)))

(provide 'aob-transcript)
;;; aob-transcript.el ends here
