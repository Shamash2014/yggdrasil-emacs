;;; aob-unread-tests.el --- Tests for unread sessions and the attention cycle -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'grep)
(yggdrasil-global-mode 1)
(eval-and-compile
  (defvar ygg-space-state-functions nil)
  (defvar ygg-space-detail-functions nil)
  (defvar ygg-leader-open-map (make-sparse-keymap))
  (defvar aob-acp-persist-file)
  (setq aob-acp-persist-file (make-temp-file "aob-unread-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'layer-quickfix)
(require 'layer-aob)

(defvar aob-unread-tests--now 1000.0)

(defmacro aob-unread-tests--with-sessions (names &rest body)
  "Run BODY with an empty registry holding idle sessions called NAMES.
Each is bound to a variable of its name."
  (declare (indent 1))
  `(let ((aob--sessions (make-hash-table :test #'equal))
         (aob--order nil)
         (aob--unread-timer nil)
         (aob--unread-last nil)
         (aob--render-timer t)
         (aob-state-change-hook '(aob--unread-on-state))
         (aob-unread-change-hook nil))
     (let ,(mapcar (lambda (n)
                     `(,n (aob-session--create :id ,(symbol-name n) :name ,(symbol-name n)
                                               :state 'working)))
                   names)
       (dolist (s (list ,@names))
         (puthash (aob-session-id s) s aob--sessions)
         (push (aob-session-id s) aob--order))
       (unwind-protect (progn ,@body)
         (when aob--unread-timer (cancel-timer aob--unread-timer))))))

(defmacro aob-unread-tests--showing (session &rest body)
  "Run BODY with SESSION's conversation in the selected window."
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local aob-buffer-session-id (aob-session-id ,session))
     (save-window-excursion
       (set-window-buffer (selected-window) (current-buffer))
       ,@body)))

(defun aob-unread-tests--tick-at (time)
  (cl-letf (((symbol-function 'float-time) (lambda (&rest _) time)))
    (aob--unread-tick)))

(ert-deftest aob-unread/finishing-out-of-sight-marks-unread ()
  (aob-unread-tests--with-sessions (a)
    (aob-set-state a 'idle)
    (should (aob-session-unread-p a))
    (should aob--unread-timer)))

(ert-deftest aob-unread/blocking-out-of-sight-marks-unread ()
  (aob-unread-tests--with-sessions (a)
    (aob-set-state a 'blocked)
    (should (aob-session-unread-p a))))

(ert-deftest aob-unread/finishing-in-the-selected-window-does-not ()
  (aob-unread-tests--with-sessions (a)
    (aob-unread-tests--showing a
      (aob-set-state a 'idle)
      (should-not (aob-session-unread-p a))
      (should-not aob--unread-timer))))

(ert-deftest aob-unread/starting-work-does-not ()
  (aob-unread-tests--with-sessions (a)
    (setf (aob-session-state a) 'idle)
    (aob-set-state a 'working)
    (should-not (aob-session-unread-p a))))

(defun aob-unread-tests--ticks (from to)
  (let ((time from))
    (while (<= time to)
      (aob-unread-tests--tick-at time)
      (setq time (+ time 0.5)))))

(ert-deftest aob-unread/dwell-clears-after-the-delay ()
  (aob-unread-tests--with-sessions (a)
    (aob-set-state a 'idle)
    (aob-unread-tests--showing a
      (aob-unread-tests--ticks 1000.0 (+ 1000.0 aob-unread-dwell -0.5))
      (should (aob-session-unread-p a))
      (aob-unread-tests--tick-at (+ 1000.0 aob-unread-dwell))
      (should-not (aob-session-unread-p a))
      (should-not aob--unread-timer))))

(ert-deftest aob-unread/gap-over-a-second-restarts-the-dwell ()
  (aob-unread-tests--with-sessions (a)
    (aob-set-state a 'idle)
    (aob-unread-tests--showing a
      (aob-unread-tests--ticks 1000.0 1002.0)
      (aob-unread-tests--ticks 1010.0 (+ 1010.0 aob-unread-dwell -0.5))
      (should (aob-session-unread-p a))
      (aob-unread-tests--tick-at (+ 1010.0 aob-unread-dwell))
      (should-not (aob-session-unread-p a)))))

(ert-deftest aob-unread/leaving-the-window-restarts-the-dwell ()
  (aob-unread-tests--with-sessions (a)
    (aob-set-state a 'idle)
    (aob-unread-tests--showing a
      (aob-unread-tests--tick-at 1000.0)
      (aob-unread-tests--tick-at 1000.5))
    (aob-unread-tests--tick-at 1001.0)
    (aob-unread-tests--showing a
      (aob-unread-tests--tick-at 1001.5)
      (aob-unread-tests--tick-at (+ 1001.0 aob-unread-dwell))
      (should (aob-session-unread-p a)))))

(ert-deftest aob-unread/entry-round-trip-keeps-it ()
  (aob-unread-tests--with-sessions (a)
    (aob-session-put a :acp-id "x")
    (setf (aob-session-backend a) 'acp)
    (aob-set-state a 'idle)
    (should (plist-get (aob-acp--entry a) :unread))
    (aob-session-put a :unread nil)
    (should-not (plist-get (aob-acp--entry a) :unread))))

(defun aob-unread-tests--cycle (by)
  (let (went)
    (cl-letf (((symbol-function 'ygg-aob--goto) (lambda (s) (setq went s)))
              ((symbol-function 'ygg-aob--put-down-p) #'ignore))
      (ygg-aob--attention-step by))
    (aob-session-name went)))

(ert-deftest aob-unread/next-and-prev-put-blocked-before-unread-and-wrap ()
  (aob-unread-tests--with-sessions (a b c d)
    (setf (aob-session-state a) 'idle (aob-session-state d) 'idle)
    (aob-session-put d :unread t)
    (aob-session-put b :unread t)
    (setf (aob-session-state b) 'idle)
    (setf (aob-session-state c) 'blocked)
    (should (equal (mapcar #'aob-session-name (ygg-aob--attention-sessions))
                   '("c" "d" "b")))
    (with-temp-buffer
      (should (equal (aob-unread-tests--cycle 1) "c"))
      (setq-local aob-buffer-session-id "c")
      (should (equal (aob-unread-tests--cycle 1) "d"))
      (setq-local aob-buffer-session-id "b")
      (should (equal (aob-unread-tests--cycle 1) "c"))
      (setq-local aob-buffer-session-id "c")
      (should (equal (aob-unread-tests--cycle -1) "b")))))

(ert-deftest aob-unread/attention-cycle-skips-hidden-and-quiet-sessions ()
  (aob-unread-tests--with-sessions (a b)
    (setf (aob-session-state a) 'idle (aob-session-state b) 'idle)
    (aob-session-put b :unread t)
    (aob-session-put b :hidden t)
    (cl-letf (((symbol-function 'ygg-aob--put-down-p) #'ignore))
      (should-not (ygg-aob--attention-sessions)))))

(ert-deftest aob-unread/navigating-does-not-read ()
  (aob-unread-tests--with-sessions (a)
    (aob-set-state a 'idle)
    (aob-unread-tests--cycle 1)
    (should (aob-session-unread-p a))))

(provide 'aob-unread-tests)
;;; aob-unread-tests.el ends here
