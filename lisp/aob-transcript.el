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
  "DIR as the CLI spells it when naming a folder: every slash a dash."
  (replace-regexp-in-string "/" "-" (directory-file-name (expand-file-name dir))))

(defun aob-transcript-file (entry)
  "Where ENTRY's conversation was written, if it is still there."
  (when-let* ((id (plist-get entry :acp-id))
              (dir (or (plist-get entry :dir) (plist-get entry :project)))
              (home (aob-transcript--home (or (plist-get entry :agent) "claude") dir))
              (file (expand-file-name
                     (format "projects/%s/%s.jsonl" (aob-transcript--slug dir) id)
                     home))
              ((file-readable-p file)))
    file))

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
          (when (and rec (equal (alist-get 'type rec) "user"))
            (when-let* ((msg (alist-get 'message rec))
                        (text (aob-transcript--text (alist-get 'content msg)))
                        (text (string-trim text))
                        ((not (string-empty-p text)))
                        ;; the harness writes its own preamble in as a
                        ;; user turn; the first thing a person said is
                        ;; what this is after
                        ((not (string-prefix-p "<" text)))
                        ((not (string-prefix-p "Caveat:" text))))
              (throw 'found
                     (truncate-string-to-width
                      (car (split-string text "\n" t)) 44 nil nil t)))))
        (forward-line 1))
      nil)))

;;;###autoload
(defun aob-transcript-found (project &optional agent)
  "Conversations AGENT left on disk for PROJECT, newest first.
The CLI writes one file per conversation under its config home.  What
this Emacs knows about is what it started itself, which for a project
you have only just taken in is none of them."
  (let* ((agent (or agent (bound-and-true-p aob-acp-default-agent) "claude"))
         (dir (expand-file-name
               (format "projects/%s" (aob-transcript--slug project))
               (aob-transcript--home agent project)))
         (key (cons agent dir)))
    (when (file-directory-p dir)
      ;; the folder's own clock says when a conversation was added to it
      ;; or written to; until it moves, the listing stands
      (let ((stamp (aob-transcript--mtime dir))
            (cell (gethash key aob-transcript--found)))
        (if (and cell (equal (car cell) stamp))
            (cdr cell)
          (let ((entries (aob-transcript--found-1 project agent dir)))
            (puthash key (cons stamp entries) aob-transcript--found)
            entries))))))

(defun aob-transcript--found-1 (project agent dir)
  "Read DIR, which holds AGENT\='s conversations about PROJECT."
  (let ((files (sort (directory-files dir t "\\.jsonl\\'")
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
                                        :found t))
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

;;;###autoload
(defun aob-transcript-forget ()
  "Drop what was read from disk, so the next look reads it again.
The folder\='s clock catches a conversation being written; a config
home moving is a change of mind, and nothing on disk says when."
  (interactive)
  (clrhash aob-transcript--homes)
  (clrhash aob-transcript--found)
  (clrhash aob-transcript--titles)
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
                                 ((equal type "text") (alist-get 'text part))
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
               (kind (and rec (alist-get 'type rec)))
               (msg (and rec (alist-get 'message rec))))
          (when (member kind '("user" "assistant"))
            (when-let* ((text (aob-transcript--text (alist-get 'content msg)))
                        ((not (string-empty-p (string-trim text)))))
              (push (cons kind (string-trim text)) out))))
        (forward-line 1)))
    (nreverse out)))

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
                  :name (if (plist-get entry :found)
                            ;; already named for its day when it was found
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
          (dolist (turn (aob-transcript-turns file))
            (aob-event s (if (equal (car turn) "user") 'prompt 'message)
                       :text (cdr turn)))
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
