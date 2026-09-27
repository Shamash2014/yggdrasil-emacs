;;; aob-btw.el --- a side question the conversation never hears -*- lexical-binding: t; -*-

;;; Commentary:
;; Ask a quick question with everything a session knows, without it
;; becoming part of that session.  The session is forked, the fork is
;; asked, its answer is shown in a popup, and the fork is thrown away:
;; no row, no trace, no entry to resume.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob)
(require 'aob-acp)

(defgroup aob-btw nil
  "Side questions asked of a fork of a session."
  :group 'aob :prefix "aob-btw-")

(defcustom aob-btw-display-action
  '((display-buffer-in-side-window) (side . bottom) (slot . 0)
    (window-height . 0.3))
  "How the answer to a side question is put on screen."
  :type 'sexp :group 'aob-btw)

(defcustom aob-btw-close-wait 5
  "Seconds a discarded fork's transcript waits for the adapter's close reply."
  :type 'number :group 'aob-btw)

(defconst aob-btw-buffer-name "*aob-btw*")

(declare-function aob-transcript-move "aob-transcript" (entry where))

(define-derived-mode aob-btw-mode special-mode "btw"
  "The answer to a side question.  It never entered the conversation."
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defun aob-btw--show (source question text &optional failed)
  "Show TEXT, the answer SOURCE's fork gave to QUESTION, in the popup.
FAILED says TEXT is why there is no answer."
  (let ((buf (get-buffer-create aob-btw-buffer-name)))
    (with-current-buffer buf
      (aob-btw-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "btw · %s%s\n" source (if failed " · failed" ""))
                            'face 'bold))
        (dolist (line (split-string (string-trim question) "\n"))
          (insert (propertize (concat "> " line) 'face 'shadow) "\n"))
        (insert "\n" (string-trim (or text "")) "\n")
        (goto-char (point-min))))
    (display-buffer buf aob-btw-display-action)
    buf))

(defun aob-btw--answer (fork)
  "The last thing FORK said in the turn it was asked in, or nil."
  (let ((since (or (plist-get (aob-session-ref fork :btw) :since) 0)))
    (when-let* ((ev (seq-find (lambda (e) (and (eq (plist-get e :type) 'message)
                                               (not (plist-get e :parent))
                                               (> (plist-get e :seq) since)))
                              (aob-session-events fork)))
                (text (string-trim (aob-event-text ev)))
                ((not (string-empty-p text))))
      text)))

(defun aob-btw--put-away (entry)
  "Move ENTRY's transcript into the folder no list reads; none there is fine."
  (when (fboundp 'aob-transcript-move)
    (ignore-errors (aob-transcript-move entry "discarded"))))

(defun aob-btw--discard (fork)
  "Kill FORK and forget it ever was, on screen and on disk.
Its transcript moves once the adapter has answered the close, having
flushed it, or after aob-btw-close-wait when no answer comes."
  (let* ((acp-id (aob-session-ref fork :acp-id))
         (entry (and acp-id (list :agent (aob-session-ref fork :agent)
                                  :acp-id acp-id
                                  :project (aob-session-project fork)
                                  :dir (aob-session-dir fork))))
         (proc (aob-session-conn fork))
         (closing (and acp-id proc (process-live-p proc)
                       (aob-session-get (aob-session-id fork))))
         (moved nil)
         (move (lambda (&rest _)
                 (unless moved
                   (setq moved t)
                   (when entry (aob-btw--put-away entry))))))
    (aob-session-put fork :on-close move)
    (when (aob-session-get (aob-session-id fork))
      (ignore-errors (aob--call fork :kill)))
    (when (aob-session-get (aob-session-id fork))
      (aob-remove-session fork))
    (when acp-id (aob-acp--forget acp-id))
    (if closing
        (run-at-time aob-btw-close-wait nil move)
      (funcall move))))

(defun aob-btw--finish (fork text &optional failed)
  "Hand TEXT, FORK's answer, on, then discard FORK.  FAILED marks an error.
An answer goes to the asker's callback when it gave one; an error, or an
answer nobody asked to take, is shown in the popup."
  (when-let* ((btw (aob-session-ref fork :btw)))
    (aob-session-put fork :btw nil)
    (unwind-protect
        (if-let* ((then (plist-get btw :then))
                  ((not failed)))
            (funcall then text)
          (aob-btw--show (plist-get btw :source)
                         (or (plist-get btw :shown) (plist-get btw :question))
                         text failed))
      (aob-btw--discard fork))))

(defun aob-btw--refuse (fork)
  "Say no to what FORK is waiting on; a side question changes nothing."
  (when (and (aob-session-ref fork :btw)
             (eq (aob-session-state fork) 'blocked))
    (unless (ignore-errors (aob-reject fork))
      (aob-btw--finish fork "it stopped to ask something a side question cannot answer" t))))

(defun aob-btw--on-state (s old new)
  (when-let* ((btw (aob-session-ref s :btw)))
    (pcase new
      ('working
       (unless (plist-get btw :since)
         (aob-session-put s :btw (plist-put btw :since aob--seq))))
      ('failed
       (aob-btw--finish s (or (aob-session-ref s :fail-reason) "the fork failed") t))
      ;; the decision is still being filed when the state flips; answer it after
      ('blocked (run-at-time 0 nil #'aob-btw--refuse s))
      ((and 'idle (guard (memq old '(working blocked)))
            (guard (not (aob-session-ref s :turn-error))))
       (let ((answer (aob-btw--answer s)))
         (aob-btw--finish s (or answer "(no answer)")
                          (and (null answer) (plist-get btw :then) t)))))))

(defun aob-btw--on-event (s ev)
  "A failed turn is idle before its error is told; finish on the telling."
  (when (and (eq (plist-get ev :type) 'error)
             (aob-session-ref s :btw)
             (aob-session-ref s :turn-error))
    (aob-btw--finish s (or (plist-get ev :title) "error") t)))

(add-hook 'aob-state-change-hook #'aob-btw--on-state)
(add-hook 'aob-event-functions #'aob-btw--on-event)

(defun aob-btw-ask (source question &optional then shown)
  "Ask QUESTION of a hidden fork of SOURCE and show the answer in a popup.
SOURCE hears nothing of it.  THEN, when given, is called with the answer
instead of the popup; an error is shown all the same.  SHOWN stands for
QUESTION in the popup.  Returns the fork, or nil when none opened."
  (let* ((aob-acp-session-refs
          (append (list :hidden t
                        :btw (list :source (aob-session-name source)
                                   :question question :then then
                                   :shown shown))
                  aob-acp-session-refs))
         (fork (condition-case err
                   (aob-acp-fork source)
                 (error (aob-btw--show (aob-session-name source)
                                       (or shown question)
                                       (error-message-string err) t)
                        nil))))
    (when (and fork (aob-session-ref fork :btw)
               (aob-session-get (aob-session-id fork)))
      (let ((aob-prompt-typed t))
        (aob-prompt fork question)))
    fork))

;;;###autoload
(defun aob-btw (source)
  "Write a side question for SOURCE; the answer never enters its conversation."
  (interactive (list (aob-target)))
  (aob-compose (lambda (text _atts) (aob-btw-ask source text))
               nil (format "btw:%s" (aob-session-name source))))

(provide 'aob-btw)
;;; aob-btw.el ends here
