;;; aob-subagent-state-tests.el --- a subagent call runs only while it can -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)

(defmacro aob-sub-state-tests--with (svar &rest body)
  (declare (indent 1))
  `(let* ((aob-trace-icons nil)
          (,svar (aob-create-session :id "acp:substate:1" :backend 'acp
                                     :name "substate" :project "/tmp/proj/"
                                     :dir "/tmp/proj/" :state 'working)))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (string-match-p "\\`\\(subs\\|trace\\):" (buffer-name b))
           (kill-buffer b)))
       (when (aob-session-get (aob-session-id ,svar))
         (aob-remove-session ,svar)))))

(defun aob-sub-state-tests--task (s &optional status)
  (aob-event s 'tool :tool-id "T1" :kind "think" :subagent t
             :title "Count files" :status (or status "in_progress")
             :ts (- (float-time) 100)))

(defun aob-sub-state-tests--header (s)
  (with-current-buffer (aob-subagents-buffer s)
    header-line-format))

(defun aob-sub-state-tests--subs-text (s)
  (with-current-buffer (aob-subagents-buffer s)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest aob-sub-state-live-turn-runs ()
  (aob-sub-state-tests--with s
    (let ((ev (aob-sub-state-tests--task s)))
      (should (equal (aob-subagents--status ev) "running"))
      (should (string-match-p "1 running" (aob-sub-state-tests--header s))))))

(ert-deftest aob-sub-state-completed-is-done ()
  (aob-sub-state-tests--with s
    (let ((ev (aob-sub-state-tests--task s "completed")))
      (aob-session-settle-subagents s)
      (should (equal (aob-subagents--status ev) "done")))))

(ert-deftest aob-sub-state-turn-stop-stops-and-freezes ()
  (aob-sub-state-tests--with s
    (let ((ev (aob-sub-state-tests--task s)))
      (aob-event s 'stop :reason "end_turn")
      (aob-session-settle-subagents s)
      (should (equal (aob-subagents--status ev) "stopped"))
      (should (equal (plist-get ev :status) "in_progress"))
      (let ((secs (aob-subagents--secs ev)))
        (sleep-for 0.05)
        (should (= secs (aob-subagents--secs ev))))
      (should (string-match-p "stopped" (aob-sub-state-tests--subs-text s)))
      (should-not (string-match-p "running" (aob-sub-state-tests--subs-text s)))
      (should-not (string-match-p "running" (aob-sub-state-tests--header s)))
      (should-not (eq (aob-trace--state ev) 'running)))))

(ert-deftest aob-sub-state-cancelled-turn-stops ()
  (aob-sub-state-tests--with s
    (let ((ev (aob-sub-state-tests--task s "pending")))
      (aob-event s 'stop :reason "cancelled")
      (aob-session-settle-subagents s)
      (should (equal (aob-subagents--status ev) "stopped")))))

(ert-deftest aob-sub-state-process-death-stops ()
  (aob-sub-state-tests--with s
    (let ((ev (aob-sub-state-tests--task s)))
      (plist-put ev :raw (list :run_in_background t))
      (aob-session-settle-subagents s t)
      (should (equal (aob-subagents--status ev) "stopped")))))

(ert-deftest aob-sub-state-background-outlives-turn ()
  (aob-sub-state-tests--with s
    (let ((ev (aob-sub-state-tests--task s)))
      (plist-put ev :raw (list :run_in_background t))
      (aob-session-settle-subagents s)
      (should (equal (aob-subagents--status ev) "running")))))

(ert-deftest aob-sub-state-late-completion-wins ()
  (aob-sub-state-tests--with s
    (let ((ev (aob-sub-state-tests--task s)))
      (aob-session-settle-subagents s)
      (plist-put ev :status "completed")
      (should (equal (aob-subagents--status ev) "done")))))

(defmacro aob-sub-state-tests--wired (svar &rest body)
  (declare (indent 1))
  `(let* ((aob-trace-icons nil)
          (proc (make-process :name "aob-test-cat" :command '("cat")
                              :connection-type 'pipe :noquery t))
          (,svar (aob-create-session :id "acp:substate:1" :backend 'acp
                                     :name "substate" :project "/tmp/proj/"
                                     :dir "/tmp/proj/" :state 'working)))
     (unwind-protect
         (progn
           (process-put proc 'aob-sessions (make-hash-table :test #'equal))
           (process-put proc 'aob-next-id (list 0))
           (process-put proc 'aob-pending (make-hash-table :test #'eql))
           (process-put proc 'aob-json-buf (generate-new-buffer " *aob-sub-json*"))
           (setf (aob-session-conn ,svar) proc)
           (aob-acp--register proc "sess-sub" ,svar)
           ,@body)
       (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
       (ignore-errors (delete-process proc))
       (dolist (b (buffer-list))
         (when (string-match-p "\\`\\(subs\\|trace\\):" (buffer-name b))
           (kill-buffer b)))
       (when (aob-session-get (aob-session-id ,svar))
         (aob-remove-session ,svar)))))

(defun aob-sub-state-tests--feed (s json)
  (aob-acp--filter (aob-session-conn s) (concat json "\n")))

(defun aob-sub-state-tests--update (s update)
  (aob-sub-state-tests--feed
   s (json-serialize
      (list :jsonrpc "2.0" :method "session/update"
            :params (list :sessionId "sess-sub" :update update)))))

(defun aob-sub-state-tests--call (s id status &optional parent)
  (aob-sub-state-tests--update
   s (append (list :sessionUpdate "tool_call" :toolCallId id :title "Task"
                   :kind "think" :status status
                   :rawInput (list :description "Count files"))
             (when parent (list :_meta (list :claudeCode (list :parentToolUseId parent)))))))

(defun aob-sub-state-tests--tool-update (s id status)
  (aob-sub-state-tests--update
   s (list :sessionUpdate "tool_call_update" :toolCallId id :status status)))

(defun aob-sub-state-tests--ev (s id)
  (seq-find (lambda (e) (equal (plist-get e :tool-id) id)) (aob-session-events s)))

(defun aob-sub-state-tests--prompt-reply (s result &optional error)
  (aob-acp--prompt-1 s "go")
  (aob-sub-state-tests--feed
   s (json-serialize
      (append (list :jsonrpc "2.0" :id (car (process-get (aob-session-conn s) 'aob-next-id)))
              (if error (list :error error) (list :result result))))))

(ert-deftest aob-sub-state-prompt-end-turn-settles ()
  (aob-sub-state-tests--wired s
    (aob-sub-state-tests--call s "T1" "in_progress")
    (aob-sub-state-tests--prompt-reply s (list :stopReason "end_turn"))
    (should (equal (aob-subagents--status (aob-sub-state-tests--ev s "T1")) "stopped"))))

(ert-deftest aob-sub-state-prompt-cancelled-settles ()
  (aob-sub-state-tests--wired s
    (aob-sub-state-tests--call s "T1" "pending")
    (aob-sub-state-tests--prompt-reply s (list :stopReason "cancelled"))
    (should (equal (aob-subagents--status (aob-sub-state-tests--ev s "T1")) "stopped"))))

(ert-deftest aob-sub-state-prompt-error-settles ()
  (aob-sub-state-tests--wired s
    (aob-sub-state-tests--call s "T1" "in_progress")
    (aob-sub-state-tests--prompt-reply s nil (list :code -32000 :message "overloaded"))
    (should (equal (aob-subagents--status (aob-sub-state-tests--ev s "T1")) "stopped"))))

(ert-deftest aob-sub-state-sentinel-settles ()
  (aob-sub-state-tests--wired s
    (aob-sub-state-tests--call s "T1" "in_progress")
    (process-put (aob-session-conn s) 'aob-test t)
    (let ((proc (aob-session-conn s)))
      (delete-process proc)
      (aob-acp--sentinel proc "killed\n"))
    (should (equal (aob-subagents--status (aob-sub-state-tests--ev s "T1")) "stopped"))))

(ert-deftest aob-sub-state-restore-open-settles-replayed-calls ()
  (aob-sub-state-tests--wired s
    (let (then)
      (cl-letf (((symbol-function 'aob-acp--open)
                 (lambda (_agent _name _project _dir _open cb &rest _)
                   (setq then cb)
                   s))
                ((symbol-function 'aob-acp--seed-history) #'ignore)
                ((symbol-function 'aob-acp--session-opened) #'ignore)
                ((symbol-function 'aob-transcript-file) (lambda (_e) nil)))
        (aob-acp--resume-entry (list :dir "/tmp/proj/" :acp-id "old" :name "substate"
                                     :agent "claude")
                               'load))
      (aob-sub-state-tests--call s "T1" "in_progress")
      (funcall then s nil)
      (should (equal (aob-subagents--status (aob-sub-state-tests--ev s "T1")) "stopped")))))

(ert-deftest aob-sub-state-late-update-runs-again ()
  (aob-sub-state-tests--wired s
    (aob-sub-state-tests--call s "T1" "in_progress")
    (aob-session-settle-subagents s)
    (let ((ev (aob-sub-state-tests--ev s "T1")))
      (aob-sub-state-tests--tool-update s "T1" "in_progress")
      (should (equal (aob-subagents--status ev) "running"))
      (should-not (plist-get ev :done-ts))
      (aob-session-settle-subagents s)
      (aob-sub-state-tests--tool-update s "T1" "completed")
      (should (equal (aob-subagents--status ev) "done")))))

(ert-deftest aob-sub-state-late-update-revives-native-kid ()
  (aob-sub-state-tests--wired s
    (aob-sub-state-tests--call s "T1" "in_progress")
    (let* ((ev (aob-sub-state-tests--ev s "T1"))
           (kid (aob-session-native-child s ev)))
      (should kid)
      (aob-session-settle-subagents s)
      (should (eq (aob-session-state kid) 'done))
      (aob-sub-state-tests--tool-update s "T1" "pending")
      (should (eq (aob-session-state kid) 'working)))))

(ert-deftest aob-sub-state-stopped-task-children-do-not-run ()
  (aob-sub-state-tests--wired s
    (aob-sub-state-tests--call s "T1" "in_progress")
    (aob-sub-state-tests--update
     s (list :sessionUpdate "tool_call" :toolCallId "C1" :title "Read" :kind "read"
             :status "in_progress"
             :_meta (list :claudeCode (list :parentToolUseId "T1"))))
    (let ((ev (aob-sub-state-tests--ev s "T1"))
          (child (aob-sub-state-tests--ev s "C1")))
      (should (= 1 (plist-get ev :child-live)))
      (aob-session-settle-subagents s)
      (should (= 0 (plist-get ev :child-live)))
      (should (aob-event-stopped-p child))
      (should-not (string-match-p "⟳" (aob-trace--rollup ev))))))

(ert-deftest aob-sub-state-gone-announced-kid-reads-cancelled ()
  (aob-sub-state-tests--wired s
    (process-put (aob-session-conn s) 'aob-subagents t)
    (aob-subagent-announced s "sub-1" "Count files" nil nil)
    (let ((kid (car (aob-subagent-children s))))
      (should (eq (aob-session-state kid) 'working))
      (aob-session-settle-subagents s t)
      (should (eq (aob-session-state kid) 'done))
      (should (equal (aob-session-ref kid :ended) "cancelled"))
      (let ((rows (seq-filter (lambda (e) (plist-get e :stand-in))
                              (aob-session-events s))))
        (should rows)
        (dolist (row rows)
          (should (equal (plist-get row :status) "cancelled"))
          (should (equal (aob-subagents--status row) "cancelled")))))))

(provide 'aob-subagent-state-tests)
