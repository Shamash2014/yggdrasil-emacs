;;; aob-sleep.el --- put idle sessions to sleep -*- lexical-binding: t; -*-

;;; Commentary:
;; A sleeping session has no agent process but keeps its place, its trace
;; and its conversation; the next prompt to it, or `aob-transcript-wake',
;; brings the agent back.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob)
(require 'aob-acp)

(declare-function aob-trace--name "aob-trace" (s))

(defgroup aob-sleep nil "Putting idle agent sessions to sleep."
  :group 'aob)

(defcustom aob-idle-sleep-after nil
  "Seconds a session may sit idle before its agent is put to sleep, or nil."
  :type '(choice (const :tag "Never" nil) natnum) :group 'aob-sleep)

(defcustom aob-sleep-interval 60
  "Seconds between looks for idle sessions to put to sleep."
  :type 'natnum :group 'aob-sleep)

(defcustom aob-sleep-grace 300
  "Seconds after Emacs starts or resumes before the first look."
  :type 'natnum :group 'aob-sleep)

(defvar aob-sleep--timer nil)
(defvar aob-sleep--last-tick nil)
(defvar aob-sleep--expected nil
  "Seconds the armed timer was due to wait before its next tick.")
(defvar aob-sleep-mode)

(defun aob-sleep--stamp (s &rest _)
  "Say S changed state just now."
  (aob-session-put s :state-at (float-time)))

(defun aob-sleep--idle-secs (s)
  "Seconds since S last changed state, counted from now when unknown."
  (- (float-time)
     (or (aob-session-ref s :state-at)
         (progn (aob-sleep--stamp s) (aob-session-ref s :state-at)))))

(defun aob-sleep--visible-p (s)
  "Whether S\='s trace is in a window on any frame."
  (and (fboundp 'aob-trace--name)
       (get-buffer-window (aob-trace--name s) t)
       t))

(defun aob-sleep--draft-p (s)
  "Whether S has compose text nobody sent."
  (when-let* ((buf (get-buffer (format "compose:%s" (aob-session-name s)))))
    (with-current-buffer buf
      (string-match-p "[^[:space:]]" (buffer-string)))))

(defun aob-sleep--has-live-subagents-p (s)
  (seq-some (lambda (o)
              (and (equal (aob-session-ref o :parent-session) (aob-session-id s))
                   (memq (aob-session-state o) '(starting working blocked))))
            (aob-sessions)))

(defun aob-sleep--sleepable-p (s)
  "Whether S has an agent running that nothing needs."
  (and (eq (aob-session-backend s) 'acp)
       (eq (aob-session-state s) 'idle)
       (aob-session-ref s :acp-id)
       (not (aob-session-ref s :asleep))
       (not (aob-session-ref s :hidden))
       (not (aob-session-ref s :parent-session))
       (not (aob-session-ref s :pinned))
       (not (aob-session-queued-p s))
       (null (aob-session-decisions s))
       (process-live-p (aob-session-conn s))
       (not (aob-sleep--visible-p s))
       (not (aob-sleep--draft-p s))
       (not (aob-sleep--has-live-subagents-p s))))

(defun aob-sleep-session (s)
  "Stop S\='s agent and keep S as a conversation that wakes on its next prompt."
  (let ((entry (aob-acp--entry s)))
    (unless entry
      (user-error "aob: %s has no conversation to wake" (aob-session-name s)))
    (aob-session-put s :asleep entry)
    (aob-session-put s :restarting t)
    (unwind-protect (aob--call s :kill)
      (aob-session-put s :restarting nil))
    (aob-set-state s 'done)
    s))

;;;###autoload
(defun aob-sleep-now (s)
  "Put S to sleep now, whatever its idle time."
  (interactive (list (aob-target)))
  (unless (and (process-live-p (aob-session-conn s))
               (not (aob-session-ref s :asleep))
               (memq (aob-session-state s) '(idle done)))
    (user-error "aob: %s is not an idle running session" (aob-session-name s)))
  (aob-sleep-session s)
  (message "aob: %s asleep" (aob-session-name s)))

;;;###autoload
(defun aob-pin-session (s)
  "Toggle whether S is kept awake however long it sits idle."
  (interactive (list (aob-target)))
  (let ((pinned (not (aob-session-ref s :pinned))))
    (aob-session-put s :pinned pinned)
    (aob--dirty s)
    (message "aob: %s %s" (aob-session-name s)
             (if pinned "pinned awake" "may sleep"))))

(defun aob-sleep--sweep ()
  "Put to sleep each session idle longer than `aob-idle-sleep-after'."
  (when aob-idle-sleep-after
    (dolist (s (aob-sessions))
      (when (and (aob-sleep--sleepable-p s)
                 (>= (aob-sleep--idle-secs s) aob-idle-sleep-after))
        (condition-case nil (aob-sleep-session s) (error nil))))))

(defun aob-sleep--restamp ()
  (dolist (s (aob-sessions)) (aob-sleep--stamp s)))

(defun aob-sleep--arm (delay)
  "Start the repeating timer, first firing after DELAY seconds."
  (when aob-sleep--timer (cancel-timer aob-sleep--timer))
  (setq aob-sleep--last-tick (float-time)
        aob-sleep--expected delay)
  (setq aob-sleep--timer
        (run-with-timer delay aob-sleep-interval #'aob-sleep--tick)))

(defun aob-sleep--tick ()
  (let ((gap (- (float-time) aob-sleep--last-tick))
        (expected (or aob-sleep--expected aob-sleep-interval)))
    (setq aob-sleep--last-tick (float-time)
          aob-sleep--expected aob-sleep-interval)
    (if (> gap (+ expected aob-sleep-interval))
        (progn (aob-sleep--restamp) (aob-sleep--arm aob-sleep-grace))
      (aob-sleep--sweep))))

;;;###autoload
(define-minor-mode aob-sleep-mode
  "Put sessions idle past `aob-idle-sleep-after' to sleep."
  :global t
  :group 'aob-sleep
  (when aob-sleep--timer
    (cancel-timer aob-sleep--timer)
    (setq aob-sleep--timer nil))
  (remove-hook 'aob-state-change-hook #'aob-sleep--stamp)
  (when aob-sleep-mode
    (add-hook 'aob-state-change-hook #'aob-sleep--stamp)
    (aob-sleep--arm aob-sleep-grace)))

(provide 'aob-sleep)
;;; aob-sleep.el ends here
