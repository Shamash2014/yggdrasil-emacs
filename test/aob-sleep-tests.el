;;; aob-sleep-tests.el --- idle sessions sleep -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'aob-sleep)

(defvar aob-sleep-tests--killed nil)

(defmacro aob-sleep-tests--with (&rest body)
  (declare (indent 0))
  `(let ((aob--sessions (make-hash-table :test #'equal))
         (aob--order nil)
         (aob-state-change-hook nil)
         (aob-idle-sleep-after 600)
         (aob-sleep-tests--killed nil))
     (cl-letf (((symbol-function 'aob--dirty) #'ignore)
               ((symbol-function 'process-live-p) (lambda (p) (eq p 'fake)))
               ((symbol-function 'aob--call)
                (lambda (s key &rest _)
                  (when (eq key :kill)
                    (push (aob-session-id s) aob-sleep-tests--killed)))))
       ,@body)))

(defun aob-sleep-tests--session (id &rest refs)
  (let ((s (aob-create-session :id id :backend 'acp :name id :project "/tmp/"
                               :dir "/tmp/" :state 'idle)))
    (setf (aob-session-conn s) 'fake)
    (aob-session-put s :acp-id (concat "acp-" id))
    (aob-session-put s :agent "claude")
    (aob-session-put s :state-at (- (float-time) 1000))
    (while refs (aob-session-put s (pop refs) (pop refs)))
    s))

(ert-deftest aob-sleep-idle-session-sleeps ()
  (aob-sleep-tests--with
    (let ((s (aob-sleep-tests--session "a")))
      (aob-sleep--sweep)
      (should (equal aob-sleep-tests--killed '("a")))
      (should (aob-session-ref s :asleep))
      (should (equal (plist-get (aob-session-ref s :asleep) :acp-id) "acp-a"))
      (should (eq (aob-session-state s) 'done))
      (should-not (aob-session-ref s :restarting)))))

(ert-deftest aob-sleep-recently-idle-session-stays ()
  (aob-sleep-tests--with
    (aob-sleep-tests--session "a" :state-at (float-time))
    (aob-sleep--sweep)
    (should-not aob-sleep-tests--killed)))

(ert-deftest aob-sleep-off-does-nothing ()
  (aob-sleep-tests--with
    (let ((aob-idle-sleep-after nil))
      (aob-sleep-tests--session "a")
      (aob-sleep--sweep)
      (should-not aob-sleep-tests--killed))))

(ert-deftest aob-sleep-pinned-stays ()
  (aob-sleep-tests--with
    (aob-sleep-tests--session "a" :pinned t)
    (aob-sleep--sweep)
    (should-not aob-sleep-tests--killed)))

(ert-deftest aob-sleep-queued-stays ()
  (aob-sleep-tests--with
    (aob-sleep-tests--session "a" :queued (list (list :text "later")))
    (aob-sleep--sweep)
    (should-not aob-sleep-tests--killed)))

(ert-deftest aob-sleep-working-stays ()
  (aob-sleep-tests--with
    (dolist (state '(working blocked starting))
      (let ((s (aob-sleep-tests--session (symbol-name state))))
        (setf (aob-session-state s) state)))
    (aob-sleep--sweep)
    (should-not aob-sleep-tests--killed)))

(ert-deftest aob-sleep-visible-trace-stays ()
  (aob-sleep-tests--with
    (aob-sleep-tests--session "a")
    (cl-letf (((symbol-function 'aob-trace--name) (lambda (_) "trace:a"))
              ((symbol-function 'get-buffer-window) (lambda (&rest _) 'win)))
      (aob-sleep--sweep))
    (should-not aob-sleep-tests--killed)))

(ert-deftest aob-sleep-unsent-draft-stays ()
  (aob-sleep-tests--with
    (aob-sleep-tests--session "a")
    (with-current-buffer (get-buffer-create "compose:a")
      (unwind-protect
          (progn (insert "half a thought")
                 (aob-sleep--sweep)
                 (should-not aob-sleep-tests--killed))
        (kill-buffer)))))

(ert-deftest aob-sleep-hidden-subagent-stays ()
  (aob-sleep-tests--with
    (aob-sleep-tests--session "lead")
    (let ((kid (aob-sleep-tests--session "kid" :parent-session "lead" :hidden t)))
      (setf (aob-session-state kid) 'working))
    (aob-sleep--sweep)
    (should-not aob-sleep-tests--killed)))

(ert-deftest aob-pin-session-toggles ()
  (aob-sleep-tests--with
    (let ((s (aob-sleep-tests--session "a")))
      (aob-pin-session s)
      (should (aob-session-ref s :pinned))
      (should (plist-get (aob-acp--entry s) :pinned))
      (aob-pin-session s)
      (should-not (aob-session-ref s :pinned)))))

(provide 'aob-sleep-tests)
;;; aob-sleep-tests.el ends here
