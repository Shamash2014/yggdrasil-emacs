;;; aob-transcript.el --- read a past conversation without reopening it -*- lexical-binding: t; -*-

;;; Commentary:
;; Resuming a session starts an agent: a process, a model connection, and
;; a bill.  Most of the time the question is only what was said, and that
;; is already on disk — the CLI writes every turn as it happens, under
;; the config home the session ran with, named by the same id aob keeps
;; in order to resume it.
;;
;; So this reads the file and shows it.  Nothing is spawned, nothing is
;; resumed, and the conversation is not altered by being looked at.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob-trace)

(declare-function ygg-agent--config-env "ygg-agent-conf" (preset cmd project &optional isolate))
(declare-function aob-acp-resume-entry "aob-acp" (e &optional pref))

(defgroup aob-transcript nil
  "Reading conversations that already happened."
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
  (replace-regexp-in-string "/" "-" (directory-file-name
                                     (expand-file-name dir))))

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
    (let ((parts (delq nil
                       (seq-map (lambda (part)
                                  (let ((type (alist-get 'type part)))
                                    (cond
                                     ((equal type "text") (alist-get 'text part))
                                     ((equal type "tool_use")
                                      (format "· %s" (or (alist-get 'name part) "tool")))
                                     (t nil))))
                                content))))
      (string-join (append parts nil) "\n")))
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
                                              :null-object nil
                                              :false-object nil))))
               (kind (and rec (alist-get 'type rec)))
               (msg (and rec (alist-get 'message rec))))
          (when (member kind '("user" "assistant"))
            (when-let* ((text (aob-transcript--text (alist-get 'content msg)))
                        ((not (string-empty-p (string-trim text)))))
              (push (cons kind (string-trim text)) out))))
        (forward-line 1)))
    (nreverse out)))

(defvar-local aob-transcript-entry nil
  "The conversation this buffer was read from.")

(defvar aob-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "R") #'aob-transcript-resume)
    (define-key map (kbd "RET") #'aob-transcript-resume)
    (define-key map (kbd "<return>") #'aob-transcript-resume)
    (define-key map "a" #'aob-transcript-resume)
    (define-key map "A" #'aob-transcript-resume)
    map))

;;;###autoload
(defun aob-transcript-resume ()
  "Start the conversation this buffer is showing up again.
Reading it cost nothing; this starts an agent, so it asks first."
  (interactive)
  (let ((e (or aob-transcript-entry
               (user-error "aob: this buffer is not a stored conversation"))))
    (when (y-or-n-p (format "Pick up %s again? "
                            (or (plist-get e :name) (plist-get e :agent)
                                "that conversation")))
      (let ((s (aob-acp-resume-entry e)))
        (when (and s (fboundp 'aob-trace)) (aob-trace s))
        s))))

(defvar ygg-aob--trace-modal)

(defun aob-transcript--plain-keys ()
  "Take the live trace's verbs off the emulation level.
They are put there so they outrank everything, which is right for a
running session and wrong here: the map is still this mode's parent, so
the verbs remain — they simply stop shadowing the two keys a recording
needs."
  (setq-local ygg-aob--trace-modal nil))

(add-hook 'aob-transcript-mode-hook #'aob-transcript--plain-keys)

(define-derived-mode aob-transcript-mode aob-trace-mode "aob-past"
  "A conversation that already happened, read from disk.
A session has one surface, whether it is running or not: this is the
trace, with its measure, its gutter and its keys, over words that were
written earlier.")

(defun aob-transcript--mark (who)
  "The gutter mark for WHO, the same one a live trace hangs there."
  (aob-trace--avatar (list :type (if (equal who "user") 'prompt 'message))))

;;;###autoload
(defun aob-transcript-view (entry)
  "Show ENTRY's conversation, without starting anything."
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
           (cdr (assoc (completing-read "Read: " (mapcar #'car rows) nil t) rows)))))
  (let ((file (aob-transcript-file entry)))
    (unless file
      (user-error "aob: no transcript on disk for %s"
                  (or (plist-get entry :name) "that session")))
    (let ((buf (get-buffer-create
                (format "*past:%s*" (or (plist-get entry :name) "session")))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (aob-transcript-mode)
          (setq aob-transcript-entry entry)
          (setq header-line-format
                (format " %s · %s · read from disk · R to pick up"
                        (or (plist-get entry :name) "session")
                        (abbreviate-file-name (or (plist-get entry :dir) ""))))
          (dolist (turn (aob-transcript-turns file))
            (insert (aob-trace--gutter
                     (aob-transcript--mark (car turn))
                     (aob-trace--prose (aob-trace--md (cdr turn))))
                    (aob-trace--sep)))
          (goto-char (point-min))))
      (pop-to-buffer buf))))

(provide 'aob-transcript)
;;; aob-transcript.el ends here
