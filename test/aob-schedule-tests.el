;;; aob-schedule-tests.el --- prompts sent later, once or on a repeat -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'aob-schedule)

(defun aob-schedule-tests--at (y mo d h mi)
  (float-time (encode-time (list 0 mi h d mo y nil -1 nil))))

(defmacro aob-schedule-tests--with (&rest body)
  "Run BODY against a schedule file of its own, no timer left behind."
  (declare (indent 0))
  `(let ((aob-schedule-file (make-temp-file "aob-schedules-" nil ".eld"))
         (aob-schedule--list nil)
         (aob-schedule--timer nil)
         (aob-acp-persist-file nil))
     (unwind-protect (progn ,@body)
       (when aob-schedule--timer (cancel-timer aob-schedule--timer))
       (delete-file aob-schedule-file))))

(defmacro aob-schedule-tests--with-session (sent &rest body)
  "Run BODY with a session `sched-acp' whose prompts are pushed onto SENT."
  (declare (indent 1))
  `(let ((s (aob-create-session :id "acp:sched-test" :backend 'acp
                                :name "sched-test" :project "/tmp/"
                                :dir "/tmp/" :state 'idle)))
     (aob-session-put s :acp-id "sched-acp")
     (unwind-protect
         (cl-letf (((symbol-function 'aob-prompt)
                    (lambda (to text &rest _) (push (cons to text) ,sent))))
           ,@body)
       (aob-remove-session s))))

(ert-deftest aob-schedule-reads-a-repeat-apart-from-a-moment ()
  (should (equal (aob-schedule--parse-repeat "every 30m") '(:every 1800)))
  (should (equal (aob-schedule--parse-repeat "every 2 hours") '(:every 7200)))
  (should (equal (aob-schedule--parse-repeat "weekdays 09:00")
                 '(:days (1 2 3 4 5) :hour 9 :minute 0)))
  (should (equal (aob-schedule--parse-repeat "every mon,thu 18:30")
                 '(:days (1 4) :hour 18 :minute 30)))
  (should-not (aob-schedule--parse-repeat "every 0m"))
  (should-not (aob-schedule--parse-repeat "tomorrow 9:00"))
  (should-not (aob-schedule--parse-repeat "in 2h")))

(ert-deftest aob-schedule-an-interval-runs-from-now ()
  (should (= (aob-schedule--next '(:every 1800) 1000.0) 2800.0)))

(ert-deftest aob-schedule-daily-is-today-if-still-ahead-else-tomorrow ()
  (let ((now (aob-schedule-tests--at 2026 9 30 10 0))
        (daily (lambda (h) (aob-schedule--parse-repeat (format "daily %02d:00" h)))))
    (should (= (aob-schedule--next (funcall daily 11) now)
               (aob-schedule-tests--at 2026 9 30 11 0)))
    (should (= (aob-schedule--next (funcall daily 9) now)
               (aob-schedule-tests--at 2026 10 1 9 0)))
    (should (= (aob-schedule--next (funcall daily 10) now)
               (aob-schedule-tests--at 2026 10 1 10 0)))))

(ert-deftest aob-schedule-weekdays-skip-the-weekend ()
  (let ((friday (aob-schedule-tests--at 2026 10 2 10 0))
        (weekdays (aob-schedule--parse-repeat "weekdays 09:00")))
    (should (= (aob-schedule--next weekdays friday)
               (aob-schedule-tests--at 2026 10 5 9 0)))
    (should (= (aob-schedule--next weekdays (aob-schedule-tests--at 2026 9 30 8 0))
               (aob-schedule-tests--at 2026 9 30 9 0)))))

(ert-deftest aob-schedule-a-moment-in-the-past-is-refused ()
  (should (= (aob-schedule--moment "in 2h" 100.0) 7300.0))
  (should-error (aob-schedule--moment "2001-01-01 09:00" (float-time))
                :type 'user-error))

(ert-deftest aob-schedule-survives-a-save-and-load ()
  (aob-schedule-tests--with
    (aob-schedule-create '(:acp-id "a1" :agent "claude" :name "claude:1")
                         "check the build\nand report" "every 30m")
    (aob-schedule-create '(:agent "codex" :project "/tmp/proj/")
                         "morning review" "in 2h")
    (let ((before (copy-tree aob-schedule--list)))
      (setq aob-schedule--list nil)
      (aob-schedule-start)
      (should (equal aob-schedule--list before))
      (should (= (length aob-schedule--list) 2))
      (should (memq aob-schedule--timer timer-list)))))

(ert-deftest aob-schedule-a-file-that-will-not-read-is-kept ()
  (aob-schedule-tests--with
    (with-temp-file aob-schedule-file (insert "((:id 1 :prompt"))
    (aob-schedule-start)
    (should-not aob-schedule--list)
    (should (file-exists-p (concat aob-schedule-file ".bad")))
    (delete-file (concat aob-schedule-file ".bad"))))

(ert-deftest aob-schedule-catches-up-at-most-once ()
  (aob-schedule-tests--with
    (let ((sent nil)
          (target '(:acp-id "sched-acp" :agent "claude" :name "sched-test"))
          (long-ago (- (float-time) (* 3 86400))))
      (setq aob-schedule--list
            (list (list :id 1 :prompt "hourly" :target target :when "every 1h"
                        :next long-ago :paused nil :error nil)
                  (list :id 2 :prompt "once" :target target :when nil
                        :next long-ago :paused nil :error nil)))
      (aob-schedule--save)
      (setq aob-schedule--list nil)
      (aob-schedule-tests--with-session sent
        (aob-schedule-start)
        (aob-schedule--tick)
        (aob-schedule--tick)
        (should (equal (sort (mapcar #'cdr sent) #'string<) '("hourly" "once")))
        (should (= (length aob-schedule--list) 1))
        (should (> (plist-get (car aob-schedule--list) :next)
                   (+ (float-time) 3000)))))))

(ert-deftest aob-schedule-a-failed-run-stays-paused-with-its-reason ()
  (aob-schedule-tests--with
    (setq aob-schedule--list
          (list (list :id 1 :prompt "hi" :when nil :next 0.0 :paused nil :error nil
                      :target '(:acp-id "nowhere" :name "gone"))))
    (aob-schedule--tick)
    (let ((sched (car aob-schedule--list)))
      (should (plist-get sched :paused))
      (should (string-match-p "gone" (plist-get sched :error))))
    (should-not aob-schedule--timer)))

(ert-deftest aob-schedule-found-by-conversation-and-by-project ()
  "A conversation's schedules come soonest first; a project's include a new session's."
  (aob-schedule-tests--with
    (setq aob-schedule--list
          (list (list :id 1 :next 200.0 :target '(:acp-id "c" :project "/tmp/p/"))
                (list :id 2 :next 100.0 :target '(:acp-id "c" :project "/tmp/p/"))
                (list :id 3 :next 300.0 :target '(:agent "claude" :project "/tmp/p/sub"))
                (list :id 4 :next 300.0 :target '(:agent "claude" :project "/tmp/q/"))))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) (aob-schedule-for "c")) '(2 1)))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) (aob-schedule-for-project "/tmp/p"))
                   '(1 2 3)))))

(ert-deftest aob-schedule-takes-an-ended-conversation ()
  "A persisted conversation's plist is a target as a session is, its folder kept."
  (aob-schedule-tests--with
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _) (if (string-prefix-p "When" prompt) "in 2h" "hi"))))
      (apply #'aob-schedule
             (aob-schedule-read '(:acp-id "old" :agent "claude" :name "old" :project "/tmp/p/"))))
    (should (equal (plist-get (car aob-schedule--list) :target)
                   '(:acp-id "old" :agent "claude" :name "old" :project "/tmp/p/")))))

(ert-deftest aob-schedule-change-runs-the-hook ()
  (aob-schedule-tests--with
    (let* ((ran 0)
           (aob-schedule-changed-hook (list (lambda () (cl-incf ran)))))
      (aob-schedule-create '(:agent "claude" :project "/tmp/") "hi" "in 2h")
      (should (= ran 1)))))

(provide 'aob-schedule-tests)
;;; aob-schedule-tests.el ends here
