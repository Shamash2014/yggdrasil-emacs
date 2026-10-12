;;; aob-pi-subagent-tests.el --- pi's subagent tool calls are subagents -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'aob)
(require 'aob-acp)

(setq aob-acp-persist-file (make-temp-file "aob-pi-subagent-sessions-" nil ".eld"))

(defmacro aob-pi-subagent-tests--with-session (var &rest body)
  (declare (indent 1))
  `(let* ((proc (make-process :name "aob-pi-test-cat" :command '("cat")
                              :connection-type 'pipe :noquery t))
          (,var (aob-create-session :id "acp:pi:1" :backend 'acp
                                    :name "pi:1" :project "/tmp/proj/"
                                    :dir "/tmp/proj/" :state 'starting)))
     (unwind-protect
         (progn
           (process-put proc 'aob-sessions (make-hash-table :test #'equal))
           (process-put proc 'aob-next-id (list 0))
           (process-put proc 'aob-pending (make-hash-table :test #'eql))
           (process-put proc 'aob-json-buf (generate-new-buffer " *aob-pi-test-json*"))
           (setf (aob-session-conn ,var) proc)
           (aob-acp--register proc "sess-test" ,var)
           ,@body)
       (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
       (ignore-errors (delete-process proc))
       (when (aob-session-get (aob-session-id ,var))
         (aob-remove-session ,var)))))

(defun aob-pi-subagent-tests--update (s update)
  (aob-acp--filter
   (aob-session-conn s)
   (concat (json-serialize `(:jsonrpc "2.0" :method "session/update"
                             :params (:sessionId "sess-test" :update ,update)))
           "\n")))

(defun aob-pi-subagent-tests--tool (s id)
  (seq-find (lambda (e) (and (eq (plist-get e :type) 'tool)
                             (equal (plist-get e :tool-id) id)))
            (aob-session-events s)))

(defun aob-pi-subagent-tests--call (s id name raw &optional status)
  (aob-pi-subagent-tests--update
   s `(:sessionUpdate "tool_call" :toolCallId ,id :title ,name :kind "other"
       :status ,(or status "pending") :locations [] :rawInput ,raw)))

(ert-deftest aob-pi-single-subagent-is-named-by-agent-and-task ()
  (aob-pi-subagent-tests--with-session s
    (aob-pi-subagent-tests--call s "c1" "subagent"
                                 '(:agent "scout" :task "Map the auth flow\nthen report"))
    (let ((ev (aob-pi-subagent-tests--tool s "c1")))
      (should (plist-get ev :subagent))
      (should (equal (plist-get ev :title) "scout: Map the auth flow")))))

(ert-deftest aob-pi-subagent-is-named-once-its-input-streams-in ()
  (aob-pi-subagent-tests--with-session s
    (aob-pi-subagent-tests--update
     s '(:sessionUpdate "tool_call" :toolCallId "c1" :title "subagent" :kind "other"
         :status "pending" :locations []))
    (should (plist-get (aob-pi-subagent-tests--tool s "c1") :subagent))
    (aob-pi-subagent-tests--update
     s '(:sessionUpdate "tool_call_update" :toolCallId "c1" :title "subagent"
         :status "in_progress" :locations []
         :rawInput (:agent "worker" :task "Fix the bug")))
    (should (equal (plist-get (aob-pi-subagent-tests--tool s "c1") :title)
                   "worker: Fix the bug"))))

(ert-deftest aob-pi-parallel-subagents-count-their-tasks ()
  (aob-pi-subagent-tests--with-session s
    (aob-pi-subagent-tests--call
     s "c1" "subagent"
     '(:tasks [(:agent "scout" :task "a") (:agent "reviewer" :task "b")]))
    (let ((ev (aob-pi-subagent-tests--tool s "c1")))
      (should (plist-get ev :subagent))
      (should (equal (plist-get ev :title) "2 tasks")))))

(ert-deftest aob-pi-chain-subagents-count-their-steps ()
  (aob-pi-subagent-tests--with-session s
    (aob-pi-subagent-tests--call
     s "c1" "subagent"
     '(:chain [(:agent "scout" :task "a") (:agent "planner") (:agent "worker")]))
    (let ((ev (aob-pi-subagent-tests--tool s "c1")))
      (should (plist-get ev :subagent))
      (should (equal (plist-get ev :title) "3-step chain")))))

(ert-deftest aob-pi-one-task-parallel-names-its-agent ()
  (aob-pi-subagent-tests--with-session s
    (aob-pi-subagent-tests--call
     s "c1" "subagent" '(:tasks [(:agent "scout" :task "look around")]))
    (should (equal (plist-get (aob-pi-subagent-tests--tool s "c1") :title)
                   "scout: look around"))))

(ert-deftest aob-pi-plain-tools-are-not-subagents ()
  (aob-pi-subagent-tests--with-session s
    (aob-pi-subagent-tests--call s "b1" "bash" '(:command "ls -la") "in_progress")
    (let ((ev (aob-pi-subagent-tests--tool s "b1")))
      (should-not (plist-get ev :subagent))
      (should (string-match-p "ls -la" (plist-get ev :title))))))

(ert-deftest aob-claude-task-is-unchanged ()
  (aob-pi-subagent-tests--with-session s
    (aob-pi-subagent-tests--update
     s '(:sessionUpdate "tool_call" :toolCallId "T1" :title "Task" :kind "think"
         :status "in_progress" :rawInput (:description "Count files")))
    (let ((ev (aob-pi-subagent-tests--tool s "T1")))
      (should (plist-get ev :subagent))
      (should (equal (plist-get ev :title) "Count files")))))

(provide 'aob-pi-subagent-tests)
;;; aob-pi-subagent-tests.el ends here
