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

(defun aob-transcript--home (agent dir)
  "The config home AGENT used in DIR, or its default one."
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

(defun aob-transcript-turns (file)
  "FILE as a list of (WHO . TEXT), oldest first."
  (let (out)
    (with-temp-buffer
      (insert-file-contents file)
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

(defun aob-transcript--name (entry)
  "What to call ENTRY's session, told apart from the others.
Every conversation with one agent is called the same thing, and a trace
is named after its session — so without this, opening a second one
walks into the first one's buffer."
  (let* ((base (or (plist-get entry :name) (plist-get entry :agent) "session"))
         (file (aob-transcript-file entry))
         (day (and file (format-time-string
                         "%b %-d"
                         (file-attribute-modification-time
                          (file-attributes file)))))
         (name (if day (format "%s · %s" base day) base)))
    ;; two on the same day still need telling apart
    (if (seq-find (lambda (s) (equal (aob-session-name s) name)) (aob-sessions))
        (format "%s %s" name (substring (or (plist-get entry :acp-id) "") 0 4))
      name)))

(defun aob-transcript--session (entry)
  "ENTRY as a session object, its turns already in it.
Asleep: it carries the id its agent answers to, and no process."
  (let* ((id (concat "acp:" (plist-get entry :acp-id)))
         (existing (aob-session-get id)))
    (or existing
        (let ((s (aob-create-session
                  :id id :backend 'acp
                  :name (aob-transcript--name entry)
                  :project (or (plist-get entry :project) (plist-get entry :dir))
                  :dir (or (plist-get entry :dir) (plist-get entry :project))
                  :state 'done)))
          (aob-session-put s :agent (plist-get entry :agent))
          (aob-session-put s :acp-id (plist-get entry :acp-id))
          (aob-session-put s :model-id (plist-get entry :model))
          (aob-session-put s :mode-id (plist-get entry :mode))
          (aob-session-put s :asleep entry)
          (dolist (turn (aob-transcript-turns (aob-transcript-file entry)))
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
  (unless (aob-transcript-file entry)
    (user-error "aob: no transcript on disk for %s"
                (or (plist-get entry :name) "that session")))
  (let* ((s (aob-transcript--session entry))
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

(defun aob-transcript--wake (fn s &rest args)
  "Bring S's agent back before sending to it, if it is asleep.
The session the resume makes is the live one; this one has served its
purpose and would otherwise sit in every list beside it."
  (if-let* ((entry (aob-session-ref s :asleep)))
      (let ((live (aob-acp-resume-entry entry)))
        (unless live
          (user-error "aob: %s would not come back" (aob-session-name s)))
        (aob-session-put s :asleep nil)
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
      (ignore-errors (aob-remove-session s))
      (when (fboundp 'aob-trace) (aob-trace live))
      live)))

(provide 'aob-transcript)
;;; aob-transcript.el ends here
