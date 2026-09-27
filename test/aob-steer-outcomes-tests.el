;;; aob-steer-outcomes-tests.el --- every answer to a steer keeps the words -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-steer-outcomes-sessions-" nil ".eld"))

(defmacro aob-steer-tests--with-session (var &rest body)
  "Bind VAR to a steering-capable fake ACP session over a cat connection."
  (declare (indent 1))
  `(let* ((proc (make-process :name "aob-test-cat" :command '("cat")
                              :connection-type 'pipe :noquery t))
          (,var (aob-create-session :id "acp:test:1" :backend 'acp
                                    :name "test:1" :project "/tmp/proj/"
                                    :dir "/tmp/proj/" :state 'starting)))
     (unwind-protect
         (progn
           (process-put proc 'aob-sessions (make-hash-table :test #'equal))
           (process-put proc 'aob-next-id (list 0))
           (process-put proc 'aob-pending (make-hash-table :test #'eql))
           (process-put proc 'aob-json-buf (generate-new-buffer " *aob-test-json*"))
           (setf (aob-session-conn ,var) proc)
           (aob-acp--register proc "sess-test" ,var)
           (aob-session-put ,var :acp-id "sess-test")
           (aob-session-put ,var :agent-meta '(:steering (:supported t)))
           ,@body)
       (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
       (ignore-errors (delete-process proc))
       (when (aob-session-get (aob-session-id ,var))
         (aob-remove-session ,var)))))

(defmacro aob-steer-tests--capturing (var &rest body)
  "Run BODY with outgoing frames collected newest-first into VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'aob-acp--send-proc)
                (lambda (_proc msg) (push msg ,var))))
       ,@body)))

(defun aob-steer-tests--frame (frames method)
  (seq-find (lambda (f) (equal (plist-get f :method) method)) frames))

(defun aob-steer-tests--answer (s frame result &optional err)
  "Answer FRAME, a request S sent, with RESULT or ERR."
  (let* ((pending (process-get (aob-session-conn s) 'aob-pending))
         (id (plist-get frame :id))
         (cb (or (gethash id pending)
                 (error "Nothing awaits a reply to %S" id))))
    (remhash id pending)
    (funcall cb result err)))

(defun aob-steer-tests--steer (s frames result &optional err)
  (aob-steer-tests--answer
   s (aob-steer-tests--frame frames "_session/steering") result err))

(defun aob-steer-tests--prompts (s)
  (seq-filter (lambda (e) (eq (plist-get e :type) 'prompt)) (aob-session-events s)))

(ert-deftest aob-steer-asks-to-be-told-when-idle ()
  "Every steer opts into promptRequired, and the meta survives encoding."
  (aob-steer-tests--with-session s
    (aob-set-state s 'working)
    (aob-steer-tests--capturing frames
      (aob-acp--interject s "use the other endpoint")
      (let ((params (plist-get (aob-steer-tests--frame frames "_session/steering")
                               :params)))
        (should (equal (plist-get (plist-get (plist-get params :_meta) :steering)
                                  :idleBehavior)
                       "promptRequired"))
        (should (string-match-p
                 "\"_meta\":{\"steering\":{\"idleBehavior\":\"promptRequired\"}}"
                 (json-serialize params)))))))

(ert-deftest aob-steer-injected-is-steered ()
  (aob-steer-tests--with-session s
    (aob-set-state s 'working)
    (aob-steer-tests--capturing frames
      (aob-acp--interject s "use the other endpoint")
      (aob-steer-tests--steer s frames '(:outcome "injected"))
      (should (eq (aob-session-state s) 'working))
      (should (equal (plist-get (car (aob-session-events s)) :title) "steered"))
      (should-not (aob-steer-tests--frame frames "session/cancel"))
      (should-not (aob-session-ref s :queued)))))

(ert-deftest aob-steer-prompt-required-becomes-a-prompt ()
  (aob-steer-tests--with-session s
    (aob-set-state s 'working)
    (aob-steer-tests--capturing frames
      (aob-acp--interject s "and skip the cache")
      (aob-steer-tests--steer s frames '(:outcome "promptRequired" :reason "noRunningTurn"))
      (should (aob-steer-tests--frame frames "session/prompt"))
      (should (eq (aob-session-state s) 'working)))))

(ert-deftest aob-steer-new-turn-after-ours-ended-stays-idle ()
  "Our turn ends before the steer is answered: the words are a prompt, not a steer."
  (aob-steer-tests--with-session s
    (aob-set-state s 'idle)
    (aob-steer-tests--capturing frames
      (aob-acp--prompt-1 s "build it")
      (let ((ours (aob-steer-tests--frame frames "session/prompt")))
        (aob-acp--interject s "and test it")
        (aob-steer-tests--answer s ours '(:stopReason "end_turn"))
        (should (eq (aob-session-state s) 'idle))
        (aob-steer-tests--steer s frames '(:outcome "startedNewTurn"))
        (should (eq (aob-session-state s) 'idle))
        (let ((said (car (aob-session-events s))))
          (should (eq (plist-get said :type) 'prompt))
          (should (equal (aob-event-text said) "and test it"))
          (should-not (plist-get said :title)))
        (should-not (aob-session-ref s :queued))
        (should-not (aob-steer-tests--frame frames "session/cancel"))))))

(ert-deftest aob-steer-new-turn-while-ours-runs-leaves-ours-to-end ()
  "Answered before our prompt returns: that prompt still brings the session back."
  (aob-steer-tests--with-session s
    (aob-set-state s 'idle)
    (aob-steer-tests--capturing frames
      (aob-acp--prompt-1 s "build it")
      (let ((ours (aob-steer-tests--frame frames "session/prompt")))
        (aob-acp--interject s "and test it")
        (aob-steer-tests--steer s frames '(:outcome "startedNewTurn"))
        (should (eq (aob-session-state s) 'working))
        (should (equal (mapcar #'aob-event-text (aob-steer-tests--prompts s))
                       '("and test it" "build it")))
        (aob-steer-tests--answer s ours '(:stopReason "end_turn"))
        (should (eq (aob-session-state s) 'idle))))))

(ert-deftest aob-steer-failed-queues-and-cancels ()
  (dolist (result '((:outcome "failed") (:outcome "somethingNew") nil))
    (aob-steer-tests--with-session s
      (aob-set-state s 'working)
      (aob-steer-tests--capturing frames
        (aob-acp--interject s "stop that")
        (aob-steer-tests--steer s frames result)
        (should (equal (mapcar #'car (aob-session-ref s :queued)) '("stop that")))
        (should (aob-steer-tests--frame frames "session/cancel"))
        (should-not (equal (plist-get (car (aob-session-events s)) :title) "steered"))))))

(ert-deftest aob-steer-failed-while-idle-prompts-at-once ()
  "The turn ended before the failure came back: nothing would flush a queue."
  (dolist (err '(nil (:code -32603 :message "boom")))
    (aob-steer-tests--with-session s
      (aob-set-state s 'working)
      (aob-steer-tests--capturing frames
        (aob-acp--interject s "stop that")
        (aob-set-state s 'idle)
        (aob-steer-tests--steer s frames (unless err '(:outcome "failed")) err)
        (should-not (aob-session-ref s :queued))
        (should-not (aob-steer-tests--frame frames "session/cancel"))
        (should (equal (aob-event-text (car (aob-steer-tests--prompts s))) "stop that"))
        (should (aob-steer-tests--frame frames "session/prompt"))
        (should (eq (aob-session-state s) 'working))))))

(ert-deftest aob-steer-error-queues-and-cancels ()
  (aob-steer-tests--with-session s
    (aob-set-state s 'working)
    (aob-steer-tests--capturing frames
      (aob-acp--interject s "stop that")
      (aob-steer-tests--steer s frames nil '(:code -32601 :message "Method not found"))
      (should (equal (mapcar #'car (aob-session-ref s :queued)) '("stop that")))
      (should (aob-steer-tests--frame frames "session/cancel")))))

(provide 'aob-steer-outcomes-tests)
;;; aob-steer-outcomes-tests.el ends here
