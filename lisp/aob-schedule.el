;;; aob-schedule.el --- prompts sent to agents later, once or on a repeat -*- lexical-binding: t; -*-

;;; Commentary:
;; A schedule is a prompt, a target and a time.  The target is either a
;; conversation, held by its agent's own id so it outlives this Emacs, or
;; a new session of an agent in a project.  The time is a single moment or
;; a repeat: every 30m, daily 09:00, weekdays 09:00, every mon,thu 09:00.
;; One timer waits for whichever schedule is due first.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'aob)
(require 'aob-acp)

(declare-function org-read-date "org")

(defcustom aob-schedule-file (locate-user-emacs-file "var/aob-schedules.eld")
  "File the schedules are kept in between sessions of Emacs."
  :type 'file :group 'aob)

(defvar aob-schedule--list nil
  "Every schedule, each a plist of :id :prompt :target :when :next :paused :error.
:when is the repeat as typed, nil for a schedule that runs once; :next
is the float time it is due.")

(defvar aob-schedule--timer nil
  "The one timer, set for whichever schedule is due first.")

(defvar aob-schedule-changed-hook nil
  "Run after any schedule is made, changed, sent or dropped.")

(defconst aob-schedule--buffer "*aob-schedules*")

(defconst aob-schedule--weekdays
  '(("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3)
    ("thu" . 4) ("fri" . 5) ("sat" . 6)))

;;; Time

(defun aob-schedule--seconds (n unit)
  "N of UNIT (a word starting m, h or d) in seconds."
  (* n (pcase (downcase (substring unit 0 1))
         ("m" 60) ("h" 3600) ("d" 86400))))

(defun aob-schedule--parse-repeat (text)
  "TEXT as a repeat plist, or nil when it names a single moment.
Either (:every SECONDS) or (:days WEEKDAYS :hour H :minute M)."
  (let ((case-fold-search t))
    (cond
     ((string-match "\\`every +0*\\([1-9][0-9]*\\) *\\([mhd][a-z]*\\)\\'" text)
      (list :every (aob-schedule--seconds
                    (string-to-number (match-string 1 text)) (match-string 2 text))))
     ((string-match (concat "\\`\\(daily\\|weekdays\\|every +[a-z,]+\\) +"
                            "\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'")
                    text)
      (let* ((word (downcase (match-string 1 text)))
             (hour (string-to-number (match-string 2 text)))
             (minute (string-to-number (match-string 3 text)))
             (days (pcase word
                     ("daily" '(0 1 2 3 4 5 6))
                     ("weekdays" '(1 2 3 4 5))
                     (_ (mapcar (lambda (d)
                                  (cdr (assoc (substring d 0 (min 3 (length d)))
                                              aob-schedule--weekdays)))
                                (split-string (substring word 6) "," t " +"))))))
        (when (and days (not (memq nil days)))
          (list :days days :hour hour :minute minute)))))))

(defun aob-schedule--next (repeat now)
  "The first moment after NOW that REPEAT comes due, as a float time.
Days are stepped through the calendar, not by adding 86400, so a
morning schedule stays in the morning across a clock change."
  (if-let* ((secs (plist-get repeat :every)))
      (+ now secs)
    (let ((today (decode-time now)))
      (seq-some (lambda (ahead)
                  (let ((at (float-time
                             (encode-time
                              (list 0 (plist-get repeat :minute) (plist-get repeat :hour)
                                    (+ (decoded-time-day today) ahead)
                                    (decoded-time-month today)
                                    (decoded-time-year today) nil -1 nil)))))
                    (and (> at now)
                         (memq (decoded-time-weekday (decode-time at))
                               (plist-get repeat :days))
                         at)))
                (number-sequence 0 7)))))

(defun aob-schedule--moment (text now)
  "TEXT as a single moment after NOW: in 2h, tomorrow 9:00, an ISO time.
Anything but `in N' goes to `org-read-date', which reads tomorrow as
today, so it is spelt the way org knows it."
  (let* ((case-fold-search t)
         (at (if (string-match "\\`in +\\([0-9]+\\) *\\([mhd][a-z]*\\)\\'" text)
                 (+ now (aob-schedule--seconds (string-to-number (match-string 1 text))
                                               (match-string 2 text)))
               (require 'org)
               (float-time
                (org-read-date t t (replace-regexp-in-string
                                    "\\btomorrow\\b" "+1" text))))))
    (when (<= at now)
      (user-error "aob: %s has already passed" (format-time-string "%F %R" at)))
    at))

(defun aob-schedule--first-run (text now)
  "When TEXT is first due after NOW, as (NEXT . WHEN).
WHEN is TEXT itself for a repeat, nil for a single moment."
  (if-let* ((repeat (aob-schedule--parse-repeat text)))
      (cons (aob-schedule--next repeat now) text)
    (cons (aob-schedule--moment text now) nil)))

;;; State

(defun aob-schedule--load ()
  "Read the schedules from the file.
One that will not read is kept beside it as .bad, since the next save
would otherwise write an empty list over every schedule in it."
  (setq aob-schedule--list
        (when (file-readable-p aob-schedule-file)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents aob-schedule-file)
                (read (current-buffer)))
            (error (copy-file aob-schedule-file (concat aob-schedule-file ".bad") t)
                   nil)))))

(defun aob-schedule--save ()
  (make-directory (file-name-directory aob-schedule-file) t)
  (let (print-length print-level)
    (with-temp-file aob-schedule-file
      (prin1 aob-schedule--list (current-buffer)))))

(defun aob-schedule--arm ()
  "Set the one timer for whichever schedule is due first, or none."
  (when aob-schedule--timer (cancel-timer aob-schedule--timer))
  (setq aob-schedule--timer
        (when-let* ((times (delq nil (mapcar (lambda (s)
                                               (unless (plist-get s :paused)
                                                 (plist-get s :next)))
                                             aob-schedule--list))))
          (run-at-time (max 0 (- (apply #'min times) (float-time)))
                       nil #'aob-schedule--tick))))

(defun aob-schedule--changed ()
  "Keep the file, the timer and the list in step with the schedules."
  (aob-schedule--save)
  (aob-schedule--arm)
  (when-let* ((buf (get-buffer aob-schedule--buffer)))
    (with-current-buffer buf
      (aob-schedule--entries)
      (tabulated-list-print t)))
  (run-hooks 'aob-schedule-changed-hook))

;;;###autoload
(defun aob-schedule-start ()
  "Read the schedules back and wait for the first one due.
Whatever came due while Emacs was closed is due now, so it goes out
once, straight away, and a repeat then keeps its own cadence."
  (aob-schedule--load)
  (aob-schedule--arm)
  (run-hooks 'aob-schedule-changed-hook))

(defun aob-schedule-for (acp-id)
  "The schedules sent to the conversation ACP-ID, soonest first."
  (sort (seq-filter (lambda (s) (equal (plist-get (plist-get s :target) :acp-id) acp-id))
                    aob-schedule--list)
        (lambda (a b) (< (plist-get a :next) (plist-get b :next)))))

(defun aob-schedule-for-project (root)
  "The schedules whose conversation or new session works in ROOT or below it."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (seq-filter (lambda (s)
                  (when-let* ((dir (plist-get (plist-get s :target) :project)))
                    (string-prefix-p root (file-name-as-directory (expand-file-name dir)))))
                aob-schedule--list)))

;;; Sending

(defun aob-schedule--session (target)
  "The session TARGET names, woken or reopened if it has to be."
  (let ((id (plist-get target :acp-id)))
    (or (seq-find (lambda (s) (equal (aob-session-ref s :acp-id) id))
                  (aob-live-sessions))
        (when-let* ((entry (aob-acp-persisted-entry id)))
          (aob-acp-resume-entry entry))
        (user-error "aob: %s is gone" (plist-get target :name)))))

(defun aob-schedule--send (target prompt)
  (if (plist-get target :acp-id)
      (aob-prompt (aob-schedule--session target) prompt)
    (aob-acp-spawn-with (plist-get target :agent) (plist-get target :project)
                        nil prompt)))

(defun aob-schedule--fire (sched)
  "Send SCHED now; a repeat moves on from now, a single run is done with.
A single run that fails stays, paused, so the failure is still there to see."
  (let ((err (condition-case e
                 (progn (aob-schedule--send (plist-get sched :target)
                                            (plist-get sched :prompt))
                        nil)
               (error (error-message-string e)))))
    (plist-put sched :error err)
    (cond ((plist-get sched :when)
           (plist-put sched :next (aob-schedule--next
                                   (aob-schedule--parse-repeat (plist-get sched :when))
                                   (float-time))))
          (err (plist-put sched :paused t))
          (t (setq aob-schedule--list (delq sched aob-schedule--list))))))

(defun aob-schedule--tick ()
  "Send every schedule that has come due, then wait for the next."
  (unwind-protect
      (let ((now (float-time)))
        (dolist (sched (seq-filter (lambda (s)
                                     (and (not (plist-get s :paused))
                                          (<= (plist-get s :next) now)))
                                   aob-schedule--list))
          (aob-schedule--fire sched)))
    (aob-schedule--changed)))

;;; Making one

(defun aob-schedule--session-target (s)
  "S as a target: a session, or a persisted conversation's plist."
  (let ((target (if (aob-session-p s)
                    (list :acp-id (aob-session-ref s :acp-id)
                          :agent (aob-session-ref s :agent)
                          :name (aob-session-name s)
                          :project (aob-session-project s))
                  (list :acp-id (plist-get s :acp-id) :agent (plist-get s :agent)
                        :name (plist-get s :name) :project (plist-get s :project)))))
    (unless (plist-get target :acp-id)
      (user-error "aob: %s has no conversation to come back to yet"
                  (plist-get target :name)))
    target))

(defun aob-schedule--read-target ()
  "Ask for a session to schedule into, or an agent and project to start one in."
  (let* ((new "new session of an agent…")
         (live (seq-filter (lambda (s) (aob-session-ref s :acp-id))
                           (aob-live-sessions)))
         (pick (completing-read "Send to: "
                                (cons new (mapcar #'aob-session-name live)) nil t)))
    (if (equal pick new)
        (list :agent (completing-read "Agent: " (aob-acp-names) nil t nil nil
                                      aob-acp-default-agent)
              :project (expand-file-name (read-directory-name "Project: ")))
      (aob-schedule--session-target
       (seq-find (lambda (s) (equal (aob-session-name s) pick)) live)))))

(defun aob-schedule--read-when (&optional initial)
  (read-string "When (in 2h, tomorrow 9:00, every 30m, weekdays 09:00): "
               initial))

(defun aob-schedule-create (target prompt when)
  "Schedule PROMPT for TARGET at WHEN, a moment or a repeat; return the schedule."
  (let* ((run (aob-schedule--first-run when (float-time)))
         (sched (list :id (1+ (apply #'max 0 (mapcar (lambda (s) (plist-get s :id))
                                                      aob-schedule--list)))
                      :prompt prompt :target target :when (cdr run)
                      :next (car run) :paused nil :error nil)))
    (setq aob-schedule--list (append aob-schedule--list (list sched)))
    (aob-schedule--changed)
    sched))

(defun aob-schedule-read (s)
  "Ask what to send S later and when, as the arguments `aob-schedule' takes."
  (list s (read-string (format "%s later » " (plist-get (aob-schedule--session-target s)
                                                         :name)))
        (aob-schedule--read-when)))

;;;###autoload
(defun aob-schedule (s prompt when)
  "Send PROMPT to session S at WHEN, a moment or a repeat.
S may also be a persisted conversation's plist.  In 2h, tomorrow 9:00
or an ISO time; every 30m, daily 09:00, weekdays 09:00 or every
mon,thu 09:00."
  (interactive (aob-schedule-read (aob-target)))
  (aob-schedule-create (aob-schedule--session-target s) prompt when))

;;; The list

(defun aob-schedule--target-label (target)
  (if (plist-get target :acp-id)
      (plist-get target :name)
    (format "new %s in %s" (plist-get target :agent)
            (file-name-nondirectory (directory-file-name (plist-get target :project))))))

(defun aob-schedule--entries ()
  (setq tabulated-list-entries
        (mapcar (lambda (s)
                  (list (plist-get s :id)
                        (vector (format-time-string "%F %R" (plist-get s :next))
                                (or (plist-get s :when) "once")
                                (aob-schedule--target-label (plist-get s :target))
                                (cond ((plist-get s :paused) "paused")
                                      ((plist-get s :error)
                                       (propertize "failed" 'help-echo (plist-get s :error)))
                                      (t ""))
                                (replace-regexp-in-string "\n" " " (plist-get s :prompt)))))
                aob-schedule--list)))

(defun aob-schedule--at-point ()
  (or (seq-find (lambda (s) (equal (plist-get s :id) (tabulated-list-get-id)))
                aob-schedule--list)
      (user-error "aob: no schedule here")))

(defun aob-schedule-add ()
  "Schedule a prompt for a session, or for a new one of an agent."
  (interactive)
  (let ((target (aob-schedule--read-target)))
    (aob-schedule-create target (read-string "Prompt: ") (aob-schedule--read-when))))

(defun aob-schedule-edit (sched)
  "Change what SCHED says and when; its target stays."
  (interactive (list (aob-schedule--at-point)))
  (let* ((prompt (read-string "Prompt: " (plist-get sched :prompt)))
         (run (aob-schedule--first-run
               (aob-schedule--read-when
                (or (plist-get sched :when)
                    (format-time-string "%F %R" (plist-get sched :next))))
               (float-time))))
    (plist-put sched :prompt prompt)
    (plist-put sched :next (car run))
    (plist-put sched :when (cdr run))
    (plist-put sched :error nil)
    (plist-put sched :paused nil)
    (aob-schedule--changed)))

(defun aob-schedule-toggle-pause (sched)
  "Pause SCHED, or let it run again; one that came due meanwhile goes out once."
  (interactive (list (aob-schedule--at-point)))
  (plist-put sched :paused (not (plist-get sched :paused)))
  (aob-schedule--changed))

(defun aob-schedule-run-now (sched)
  "Send SCHED now, as if it had come due."
  (interactive (list (aob-schedule--at-point)))
  (aob-schedule--fire sched)
  (aob-schedule--changed))

(defun aob-schedule-delete (sched)
  "Drop SCHED for good."
  (interactive (list (aob-schedule--at-point)))
  (when (yes-or-no-p (format "Delete the schedule for %s? "
                             (aob-schedule--target-label (plist-get sched :target))))
    (setq aob-schedule--list (delq sched aob-schedule--list))
    (aob-schedule--changed)))

(defvar-keymap aob-schedule-list-mode-map
  :parent tabulated-list-mode-map
  "a" #'aob-schedule-add
  "e" #'aob-schedule-edit
  "p" #'aob-schedule-toggle-pause
  "r" #'aob-schedule-run-now
  "d" #'aob-schedule-delete)

(define-derived-mode aob-schedule-list-mode tabulated-list-mode "Schedules"
  "Every scheduled prompt, soonest first."
  (setq tabulated-list-format [("Next" 16 t) ("Repeat" 20 t) ("Target" 24 t)
                               ("State" 7 t) ("Prompt" 0 nil)])
  (setq tabulated-list-sort-key '("Next"))
  (add-hook 'tabulated-list-revert-hook #'aob-schedule--entries nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun aob-schedule-list ()
  "Show every scheduled prompt: a add, e edit, p pause, r run now, d delete."
  (interactive)
  (with-current-buffer (get-buffer-create aob-schedule--buffer)
    (aob-schedule-list-mode)
    (aob-schedule--entries)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(provide 'aob-schedule)
;;; aob-schedule.el ends here
