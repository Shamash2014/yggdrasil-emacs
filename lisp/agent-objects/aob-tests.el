;;; aob-tests.el --- ERT tests for aob -*- lexical-binding: t; -*-

;; Run: emacs -Q -batch -L lisp/agent-objects -l aob-tests -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'aob-workflow)
(require 'ygg-diagram nil t)
(require 'aob-transcript nil t)

(defmacro aob-tests--with-session (var &rest body)
  "Bind VAR to a wired-up fake ACP session over a `cat' connection."
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
           ,@body)
       (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
       (ignore-errors (delete-process proc))
       (when (aob-session-get (aob-session-id ,var))
         (aob-remove-session ,var)))))

(defun aob-tests--feed (s json)
  (aob-acp--filter (aob-session-conn s) (concat json "\n")))

(ert-deftest aob-framing-split-frames ()
  (aob-tests--with-session s
    (let* ((tc "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"sess-test\",\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"t1\",\"title\":\"Edit auth.ts\",\"kind\":\"edit\",\"status\":\"in_progress\",\"content\":[{\"type\":\"diff\",\"path\":\"a.ts\",\"oldText\":\"a\\nb\",\"newText\":\"a\\nb\\nc\\nd\"}]}}}")
           (mid (/ (length tc) 2)))
      (aob-acp--filter (aob-session-conn s) (substring tc 0 mid))
      (should-not (seq-find (lambda (e) (eq (plist-get e :type) 'tool))
                            (aob-session-events s)))
      (aob-acp--filter (aob-session-conn s) (concat (substring tc mid) "\n"))
      (let ((ev (seq-find (lambda (e) (eq (plist-get e :type) 'tool))
                          (aob-session-events s))))
        (should (equal (plist-get ev :title) "Edit auth.ts"))
        (should (equal (plist-get ev :stat) "+4 −2"))))))

(ert-deftest aob-tool-update-mutates-in-place ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"t1\",\"title\":\"x\",\"status\":\"in_progress\"}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"t1\",\"status\":\"completed\"}}}")
    (let ((tools (seq-filter (lambda (e) (eq (plist-get e :type) 'tool))
                             (aob-session-events s))))
      (should (= (length tools) 1))
      (should (equal (plist-get (car tools) :status) "completed")))))

(ert-deftest aob-chunks-coalesce ()
  (aob-tests--with-session s
    (dolist (txt '("hello " "world"))
      (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"%s\"}}}}" txt)))
    (let ((msgs (seq-filter (lambda (e) (eq (plist-get e :type) 'message))
                            (aob-session-events s))))
      (should (= (length msgs) 1))
      (should (equal (aob-event-text (car msgs)) "hello world")))))

(ert-deftest aob-permission-decision-roundtrip ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"session/request_permission\",\"params\":{\"toolCall\":{\"title\":\"run npm test\",\"rawInput\":{\"command\":\"npm test\"}},\"options\":[{\"optionId\":\"allow\",\"name\":\"Allow\",\"kind\":\"allow_once\"}]}}")
    (should (eq (aob-session-state s) 'blocked))
    (let ((d (car (aob-session-decisions s))))
      (should (equal (plist-get d :title) "run npm test"))
      (should (equal (plist-get d :detail) "npm test"))
      (aob-acp--resolve s d "allow"))
    (should (eq (aob-session-state s) 'working))
    (should (null (aob-session-decisions s)))))

(ert-deftest aob-request-hook-sees-the-decision-and-the-tool-call ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (let (seen)
      (let ((aob-acp-request-functions
             (list (lambda (sess d tc) (push (list sess d tc) seen)))))
        (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"session/request_permission\",\"params\":{\"toolCall\":{\"title\":\"Bash\",\"kind\":\"execute\",\"locations\":[{\"path\":\"lib/core.el\"}]},\"options\":[{\"optionId\":\"allow\",\"name\":\"Allow\",\"kind\":\"allow_once\"}]}}"))
      (should (= (length seen) 1))
      (let ((call (car seen)))
        (should (eq (nth 0 call) s))
        (should (equal (plist-get (nth 1 call) :reply-id) 11))
        (should (eq (nth 1 call) (car (aob-session-decisions s))))
        (should (equal (plist-get (nth 2 call) :title) "Bash"))
        (should (equal (plist-get (nth 2 call) :kind) "execute"))
        (should (equal (mapcar (lambda (l) (plist-get l :path))
                               (plist-get (nth 2 call) :locations))
                       '("lib/core.el")))))
    ;; the request goes on as it always did: blocked, with the event recorded
    (should (eq (aob-session-state s) 'blocked))
    (should (seq-find (lambda (e) (eq (plist-get e :type) 'permission))
                      (aob-session-events s)))
    (aob-acp--resolve s (car (aob-session-decisions s)) "allow")
    (should (eq (aob-session-state s) 'working))))

(ert-deftest aob-cancel-answers-pending-decisions ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"session/request_permission\",\"params\":{\"toolCall\":{\"title\":\"x\"},\"options\":[{\"optionId\":\"a\",\"name\":\"A\",\"kind\":\"allow_once\"}]}}")
    (should (eq (aob-session-state s) 'blocked))
    (aob-acp--cancel s)
    (should (null (aob-session-decisions s)))
    (should (eq (aob-session-state s) 'working))))

(ert-deftest aob-response-routing ()
  (aob-tests--with-session s
    (let (got)
      (aob-acp--request s "session/prompt" (list :x 1)
                        (lambda (res _e) (setq got res)))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"stopReason\":\"end_turn\"}}")
      (should (equal (plist-get got :stopReason) "end_turn")))))

(ert-deftest aob-live-usage-and-subagent-nesting ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"usage_update\",\"used\":31906,\"size\":1000000}}}")
    (should (equal (aob-session-ctx s) "31.9k/1.0M"))
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"toolu_1\",\"title\":\"Task\",\"kind\":\"think\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"Count files\"},\"_meta\":{\"claudeCode\":{\"toolName\":\"Agent\"}}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"toolu_2\",\"title\":\"find | wc -l\",\"kind\":\"execute\",\"status\":\"in_progress\",\"_meta\":{\"claudeCode\":{\"toolName\":\"Bash\",\"parentToolUseId\":\"toolu_1\"}}}}}")
    (let* ((tools (seq-filter (lambda (e) (eq (plist-get e :type) 'tool))
                              (aob-session-events s)))
           (child (car tools))
           (task (cadr tools)))
      (should (equal (plist-get task :title) "Count files"))
      (should-not (plist-get task :parent))
      (should (equal (plist-get child :parent) "toolu_1")))))

(ert-deftest aob-usage-tracked-on-stop ()
  (aob-tests--with-session s
    (aob-acp--prompt-1 s "hi")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"stopReason\":\"end_turn\",\"usage\":{\"inputTokens\":2,\"outputTokens\":22,\"totalTokens\":31604}}}")
    (should (= (plist-get (aob-session-ref s :usage) :totalTokens) 31604))
    (should (equal (aob-session-blurb s) "done (end_turn) · 31.6k ctx"))))

(ert-deftest aob-commands-capture ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"available_commands_update\",\"availableCommands\":[{\"name\":\"debug\",\"description\":\"d\"}]}}}")
    (should (equal (plist-get (car (aob-session-ref s :commands)) :name)
                   "debug"))))

(ert-deftest aob-event-cap-trims ()
  (aob-tests--with-session s
    (let ((aob-event-cap 20))
      (dotimes (i 40) (aob-event s 'state :title (format "e%d" i)))
      (should (<= (aob-session-nevents s) 20)))))

(ert-deftest aob-dead-sessions-filtered ()
  (aob-tests--with-session s
    (aob-set-state s 'dead)
    (should-not (memq s (aob-live-sessions)))
    (should (memq s (aob-sessions)))))

(ert-deftest aob-artifact-stage-transitions ()
  (aob-tests--with-session s
    (let ((aob-artifact-auto-open nil)
          (f (make-temp-file "aob-artifact" nil ".md" "content")))
      (unwind-protect
          (progn
            (aob-session-put s :artifact-file f)
            (aob-session-put s :artifact-stage 'draft)
            (aob-set-state s 'working)
            (aob-set-state s 'idle)
            (should (eq (aob-session-ref s :artifact-stage) 'review)))
        (delete-file f)))))

(ert-deftest aob-artifact-noticed-from-edit ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"t1\",\"title\":\"Write spec\",\"kind\":\"edit\",\"status\":\"completed\",\"locations\":[{\"path\":\"/tmp/proj/.aob/spec.md\"}]}}}")
    (should (equal (aob-session-ref s :artifact-file) "/tmp/proj/.aob/spec.md"))
    (should (eq (aob-session-ref s :artifact-stage) 'draft))
    ;; edits elsewhere never tag
    (aob-session-put s :artifact-file nil)
    (aob-session-put s :artifact-stage nil)
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"t2\",\"title\":\"Edit code\",\"kind\":\"edit\",\"status\":\"completed\",\"locations\":[{\"path\":\"/tmp/proj/src/x.ts\"}]}}}")
    (should-not (aob-session-ref s :artifact-file))))

(ert-deftest aob-prompt-queues-while-starting ()
  (aob-tests--with-session s
    (aob-set-state s 'starting)
    (aob-acp--prompt s "early bird")
    (should (equal (caar (aob-session-ref s :queued)) "early bird"))))

(ert-deftest aob-queue-merges-on-flush ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-acp--prompt s "first" nil)
    (aob-acp--prompt s "second" '("/tmp/a.png"))
    (should (= (length (aob-session-ref s :queued)) 2))
    ;; flush path: fake the stop by calling flush directly with idle state
    (aob-set-state s 'idle)
    (cl-letf* ((sent nil)
               ((symbol-function 'aob-acp--prompt-1)
                (lambda (_s text atts &optional _queued)
                  (setq sent (cons text atts)))))
      (aob-acp--flush-queue s)
      (should (equal (car sent) "first\n\nsecond"))
      (should (equal (cdr sent) '("/tmp/a.png"))))))

(ert-deftest aob-queued-prompts-visible ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-acp--prompt s "later")
    (let ((ev (car (aob-session-events s))))
      (should (eq (plist-get ev :type) 'prompt))
      (should (equal (plist-get ev :status) "queued"))
      (aob--modeline-refresh)
      (should (string-match-p "»1" aob-modeline-string))
      (with-current-buffer (aob-trace-buffer s)
        ;; words carry a speaker line rather than a glyph behind a clock —
        ;; under the plain style, where the name is the only thing saying
        ;; who spoke; delta draws a mark in the margin instead
        (cl-flet ((redraw ()
                    ;; a block is cached on its event: a style read at
                    ;; render time is only read again once that is gone
                    (dolist (e (aob-session-events s)) (plist-put e :line nil))
                    (aob-trace--render t)))
          (let ((aob-trace-style 'plain))
            (redraw)
            (should (string-match-p "you" (buffer-string))))
          (redraw))
        (should (string-match-p "later ⋯ queued" (buffer-string))))
      ;; flush promotes the same event in place — no duplicate, no marker
      (aob-set-state s 'idle)
      (cl-letf (((symbol-function 'aob-acp--request)
                 (lambda (&rest _) nil)))
        (aob-acp--flush-queue s))
      (should-not (plist-get ev :status))
      (should (= 1 (seq-count (lambda (e) (eq (plist-get e :type) 'prompt))
                              (aob-session-events s))))
      (with-current-buffer (aob-trace-buffer s)
        (aob-trace--render t)
        (should-not (string-match-p "queued" (buffer-string)))))))

(ert-deftest aob-workflow-record-and-read ()
  (let ((dir (make-temp-file "aob-wf" t)))
    (unwind-protect
        (aob-tests--with-session s
          (setf (aob-session-project s) dir)
          (aob-event s 'prompt :text "step one\nwith a second line")
          (aob-event s 'message :text "noise between turns")
          (aob-event s 'prompt :text "step two")
          (aob-workflow-record s "smoke")
          (should (equal (aob-workflow-names dir) '("smoke")))
          (let ((wf (aob-workflow-read dir "smoke")))
            (should (equal (plist-get wf :stages)
                           '(("step one\nwith a second line")
                             ("step two"))))))
      (delete-directory dir t))))

(ert-deftest aob-workflow-md-graph-parse ()
  (let ((wf (aob-workflow--parse
             "# port\nagent: claude\n\nSome prose describing intent.\n
## explore\n- audit A\n  second line\n- audit B\n\n## merge\n- combine results\n")))
    (should (equal (plist-get wf :agent) "claude"))
    (should (equal (plist-get wf :stages)
                   '(("audit A\nsecond line" "audit B")
                     ("combine results"))))))

(ert-deftest aob-workflow-replay-advances ()
  (aob-tests--with-session s
    (aob-session-put s :wf-name "smoke")
    (aob-session-put s :wf-stages '(("s2") ("s3")))
    (cl-letf* ((sent nil)
               ((symbol-function 'aob-prompt)
                (lambda (_s text) (push text sent))))
      (dotimes (_ 3)
        (aob-set-state s 'working)
        (aob-set-state s 'idle))
      (should (equal (nreverse sent) '("s2" "s3")))
      (should-not (aob-session-ref s :wf-name))
      (should (seq-find (lambda (ev)
                          (and (eq (plist-get ev :type) 'state)
                               (equal (plist-get ev :title)
                                      "workflow smoke done")))
                        (aob-session-events s))))))

(ert-deftest aob-workflow-graph-barrier ()
  (aob-tests--with-session boss
    (let ((spawned nil) (sent nil) (n 0))
      (cl-letf* (((symbol-function 'aob-acp-spawn)
                  (lambda (_agent step)
                    (let ((w (aob-create-session
                              :id (format "acp:wf-w%d" (cl-incf n))
                              :backend 'acp :name (format "wf-w%d" n)
                              :project "/tmp/proj/" :dir "/tmp/proj/"
                              :state 'working)))
                      (push (cons w step) spawned)
                      w)))
                 ((symbol-function 'aob-prompt)
                  (lambda (_s text) (push text sent))))
        (aob-session-put boss :wf-name "g")
        (aob-session-put boss :wf-agent "claude")
        (aob-session-put boss :wf-stages '(("wa" "wb") ("merge")))
        ;; the opening handshake settles → stage 1 fans out
        (aob-set-state boss 'idle)
        (should (= (length spawned) 2))
        (should (equal (mapcar #'cdr (reverse spawned)) '("wa" "wb")))
        ;; first worker settles — the barrier holds
        (aob-set-state (car (car spawned)) 'idle)
        (should-not sent)
        ;; last worker settles — merge releases on the coordinator
        (aob-set-state (car (cadr spawned)) 'idle)
        (should (equal sent '("merge")))
        (should-not (aob-session-ref boss :wf-waiting))
        ;; merge turn ends → workflow done
        (aob-set-state boss 'working)
        (aob-set-state boss 'idle)
        (should-not (aob-session-ref boss :wf-name))
        (dolist (w spawned) (aob-set-state (car w) 'dead))))))

(ert-deftest aob-workflow-halts-on-turn-error ()
  (aob-tests--with-session s
    (aob-session-put s :wf-name "smoke")
    (aob-session-put s :wf-stages '(("never sent")))
    (cl-letf* ((sent nil)
               ((symbol-function 'aob-prompt)
                (lambda (_s text) (push text sent))))
      (aob-set-state s 'working)
      (aob-session-put s :turn-error t)
      (aob-set-state s 'idle)
      (should-not sent)
      (should-not (aob-session-ref s :wf-name))
      (should (seq-find (lambda (ev)
                          (equal (plist-get ev :title) "workflow smoke halted"))
                        (aob-session-events s))))))

(ert-deftest aob-evict-pins-prompts ()
  (aob-tests--with-session s
    (let ((aob-event-cap 6))
      (aob-event s 'prompt :text "keep me")
      (dotimes (i 9)
        (aob-event s 'message :text (format "m%d" i)))
      (should (seq-find (lambda (ev) (eq (plist-get ev :type) 'prompt))
                        (aob-session-events s))))))

(ert-deftest aob-model-config-check-and-switch ()
  (aob-tests--with-session s
    (let ((aob-acp-persist-file nil))
      (aob-acp--session-opened
       s '(:sessionId "sess-test"
           :modes (:currentModeId "default"
                   :availableModes ((:id "default" :name "Default")))
           :configOptions
           ((:id "mode" :name "Mode" :currentValue "default"
             :options ((:value "default" :name "Default")))
            (:id "model" :name "Model" :currentValue "claude-sonnet-4-5"
             :options ((:value "claude-sonnet-4-5" :name "Sonnet 4.5")
                       (:value "claude-opus-4-8" :name "Opus 4.8"))))))
      (should (equal (aob-session-ref s :model-id) "claude-sonnet-4-5"))
      (should (equal (aob-session-ref s :model-name) "Sonnet 4.5"))
      (let (wire)
        (cl-letf (((symbol-function 'aob-acp--request)
                   (lambda (_s method params cb)
                     (setq wire (list method params))
                     (funcall cb '(:configOptions
                                   ((:id "model"
                                     :currentValue "claude-opus-4-8"
                                     :options ((:value "claude-sonnet-4-5"
                                                :name "Sonnet 4.5")
                                               (:value "claude-opus-4-8"
                                                :name "Opus 4.8")))))
                              nil))))
          (aob-acp--set-config s "model" "claude-opus-4-8"))
        (should (equal (car wire) "session/set_config_option"))
        (should (equal (plist-get (cadr wire) :configId) "model"))
        (should (equal (plist-get (cadr wire) :value) "claude-opus-4-8"))
        (should (equal (aob-session-ref s :model-id) "claude-opus-4-8"))
        (should (equal (aob-session-ref s :model-name) "Opus 4.8"))
        ;; the switch is ambient: a state event marks it in the trace
        (should (seq-find (lambda (ev)
                            (equal (plist-get ev :title) "model: Opus 4.8"))
                          (aob-session-events s)))))))

(ert-deftest aob-model-via-models-field ()
  ;; codex speaks the spec's models field + session/set_model, not
  ;; configOptions — the same M verb must serve both
  (aob-tests--with-session s
    (let ((aob-acp-persist-file nil))
      (aob-acp--session-opened
       s '(:sessionId "sess-test"
           :models (:currentModelId "gpt-5.2-codex"
                    :availableModels ((:modelId "gpt-5.2-codex" :name "GPT-5.2 Codex")
                                      (:modelId "gpt-5.2" :name "GPT-5.2")))))
      (should (equal (aob-session-ref s :model-name) "GPT-5.2 Codex"))
      (should (equal (car (aob-acp--model-info s)) "gpt-5.2-codex"))
      (let (wire)
        (cl-letf (((symbol-function 'aob-acp--request)
                   (lambda (_s method params cb)
                     (setq wire (list method params))
                     (funcall cb nil nil))))
          (aob-acp--set-model s "gpt-5.2"))
        (should (equal (car wire) "session/set_model"))
        (should (equal (plist-get (cadr wire) :modelId) "gpt-5.2"))
        (should (equal (aob-session-ref s :model-name) "GPT-5.2"))
        (should (seq-find (lambda (ev)
                            (equal (plist-get ev :title) "model: GPT-5.2"))
                          (aob-session-events s)))))))

(ert-deftest aob-model-self-heals-via-mode-noop ()
  ;; a session with no stored options (opened before ingestion) fetches
  ;; them through a no-op mode set, then opens the picker
  (aob-tests--with-session s
    (aob-set-state s 'idle)
    (aob-session-put s :mode-id "default")
    (let (wire picked)
      (cl-letf* (((symbol-function 'aob-acp--request)
                  (lambda (_s method params cb)
                    (push (list method params) wire)
                    (funcall cb '(:configOptions
                                  ((:id "model" :currentValue "opus"
                                    :options ((:value "opus" :name "Opus")
                                              (:value "haiku" :name "Haiku")))))
                             nil)))
                 ((symbol-function 'run-at-time)
                  (lambda (_ _ fn &rest args) (apply fn args)))
                 ((symbol-function 'completing-read)
                  (lambda (&rest _) (setq picked t) "Haiku")))
        (aob-acp-model s))
      (let ((calls (nreverse wire)))
        (should (equal (car (nth 0 calls)) "session/set_config_option"))
        (should (equal (plist-get (cadr (nth 0 calls)) :configId) "mode"))
        (should (equal (plist-get (cadr (nth 0 calls)) :value) "default"))
        (should picked)
        (should (equal (plist-get (cadr (nth 1 calls)) :configId) "model"))
        (should (equal (plist-get (cadr (nth 1 calls)) :value) "haiku"))))))

(ert-deftest aob-ask-user-question-roundtrip ()
  (aob-tests--with-session s
    (let (sent)
      (cl-letf (((symbol-function 'aob-acp--respond)
                 (lambda (_s id result &optional _err)
                   (setq sent (list id result)))))
        (aob-acp--on-request
         s 7 "elicitation/create"
         '(:mode "form" :sessionId "sess-test"
           :message "Which auth method?"
           :requestedSchema
           (:type "object"
            :properties
            (:question_0 (:type "string" :title "Auth"
                          :oneOf ((:const "OAuth" :title "OAuth")
                                  (:const "API key" :title "API key")))
             :question_0_custom (:type "string" :title "Other")))))
        (should (eq (aob-session-state s) 'blocked))
        (let ((d (car (aob-session-decisions s))))
          (should (eq (plist-get d :kind) 'elicitation))
          (should (equal (plist-get d :title) "Which auth method?"))
          (let ((q (car (plist-get d :questions))))
            (should (equal (plist-get q :options) '("OAuth" "API key")))
            (should-not (plist-get q :multi)))
          ;; picking an option answers the form field
          (aob-acp--resolve s d '(("question_0" . "OAuth")))
          (should (equal sent '(7 (:action "accept"
                                   :content (:question_0 "OAuth")))))
          (should (eq (aob-session-state s) 'working))
          (should-not (aob-session-decisions s)))
        ;; a typed answer rides the custom field instead
        (aob-acp--on-request
         s 8 "elicitation/create"
         '(:message "Pick" :requestedSchema
           (:type "object"
            :properties (:question_0 (:type "string"
                                      :oneOf ((:const "A")))))))
        (aob-acp--resolve s (car (aob-session-decisions s))
                          '(("question_0_custom" . "my own take")))
        (should (equal (cadr sent)
                       '(:action "accept"
                         :content (:question_0_custom "my own take"))))))))

(ert-deftest aob-model-waits-for-handshake ()
  ;; M pressed during the seconds-long resume handshake must defer and
  ;; then open the picker, never claim nothing is advertised
  (aob-tests--with-session s
    (should (eq (aob-session-state s) 'starting))
    (let (picked)
      (cl-letf* (((symbol-function 'run-at-time)
                  (lambda (_ _ fn &rest args) (apply fn args)))
                 ((symbol-function 'aob-acp--model-1)
                  (lambda (_s) (setq picked t))))
        (aob-acp-model s)
        (should-not picked)
        (aob-set-state s 'idle)
        (should picked)))))

(ert-deftest aob-model-failed-session-tells-truth ()
  ;; a failed resume ingests nothing — M must surface the failure, not
  ;; claim the agent advertises no models
  (aob-tests--with-session s
    (aob-acp--fail s '(:message "Resource not found: 5ecf182b"))
    (should (equal (aob-session-ref s :fail-reason)
                   "Resource not found: 5ecf182b"))
    (let ((err (should-error (aob-acp-model s) :type 'user-error)))
      (should (string-match-p "Resource not found" (cadr err)))
      (should-not (string-match-p "advertises no models" (cadr err))))
    ;; a died-underneath session (npx flake, adapter crash) is the same
    ;; story through the sentinel's state
    (aob-session-put s :fail-reason "npm ERR! network")
    (aob-set-state s 'dead)
    (let ((err (should-error (aob-acp-model s) :type 'user-error)))
      (should (string-match-p "npm ERR! network" (cadr err))))))

(ert-deftest aob-image-capability-gate ()
  ;; image blocks only go to agents that declared image support; others
  ;; get the paths inline so the reference is never silently dropped
  (aob-tests--with-session s
    (aob-set-state s 'idle)
    (let ((img (make-temp-file "aob-img" nil ".png" "PNG"))
          sent)
      (unwind-protect
          (cl-letf (((symbol-function 'aob-acp--request)
                     (lambda (_s _m params _cb)
                       (setq sent (plist-get params :prompt)))))
            (aob-session-put s :agent-caps '(:promptCapabilities (:image t)))
            (aob-acp--prompt-1 s "look" (list img))
            (should (= (length sent) 2))
            (should (equal (plist-get (aref sent 1) :type) "image"))
            (aob-session-put s :agent-caps nil)
            (aob-acp--prompt-1 s "look" (list img))
            (should (= (length sent) 1))
            (should (string-match-p (format "\\[image: %s\\]" (regexp-quote img))
                                    (plist-get (aref sent 0) :text))))
        (delete-file img)))))

(ert-deftest aob-config-picker-sets-non-model-option ()
  ;; deepen codex: reasoning_effort etc. are set through the generic
  ;; config path, model/mode excluded (they have M/m)
  (aob-tests--with-session s
    (aob-session-put s :config-options
                     '((:id "model" :name "Model" :currentValue "gpt-5.2")
                       (:id "reasoning_effort" :name "Reasoning" :currentValue "medium"
                        :options ((:value "low" :name "Low")
                                  (:value "high" :name "High")))))
    (let (wire)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt coll &rest _)
                   (cond ((string-prefix-p "Option:" prompt) "Reasoning")
                         (t "High"))))
                ((symbol-function 'aob-acp--set-config)
                 (lambda (_s id val) (setq wire (list id val)))))
        (aob-acp-config s)
        (should (equal wire '("reasoning_effort" "high")))))))

(ert-deftest aob-pi-hermes-registered ()
  (should (assoc "pi" aob-acp-agents))
  (should (equal (plist-get (cdr (assoc "pi" aob-acp-agents)) :command)
                 '("npx" "-y" "pi-acp")))
  (should (assoc "hermes" aob-acp-agents))
  (should (equal (plist-get (cdr (assoc "hermes" aob-acp-agents)) :command)
                 '("uvx" "--from" "hermes-agent[acp]" "hermes-acp"))))

(ert-deftest aob-autocompact-fires-only-when-settled-and-over ()
  (aob-tests--with-session s
    (let ((aob-acp-autocompact-ratio 0.85)
          fired)
      (cl-letf (((symbol-function 'aob-acp--prompt-1)
                 (lambda (_s text &rest _) (setq fired text))))
        (aob-session-put s :commands '((:name "compact")))
        (aob-session-put s :ctx-size 1000000)
        (aob-set-state s 'idle)
        ;; under the line → nothing
        (aob-session-put s :ctx-used 700000)
        (aob-acp--autocompact-check s)
        (should-not fired)
        ;; over the line but WORKING → must wait for settle
        (aob-session-put s :ctx-used 900000)
        (aob-set-state s 'working)
        (aob-acp--autocompact-check s)
        (should-not fired)
        ;; settled and over → fires /compact once
        (aob-set-state s 'idle)
        (aob-acp--autocompact-check s)
        (should (equal fired "/compact"))
        (should (aob-session-ref s :autocompact-fired))
        ;; still over, already fired → no re-fire (no loop even if usage
        ;; never drops)
        (setq fired nil)
        (aob-acp--autocompact-check s)
        (should-not fired)))))

(ert-deftest aob-autocompact-rearms-after-drop ()
  (aob-tests--with-session s
    (let ((aob-acp-autocompact-ratio 0.85))
      (aob-session-put s :ctx-size 1000000)
      (aob-session-put s :autocompact-fired t)
      ;; still high → stays fired
      (aob-session-put s :ctx-used 800000)
      (aob-acp--autocompact-check s)
      (should (aob-session-ref s :autocompact-fired))
      ;; dropped below trigger-hysteresis (0.75) → re-armed
      (aob-session-put s :ctx-used 700000)
      (aob-acp--autocompact-check s)
      (should-not (aob-session-ref s :autocompact-fired)))))

(ert-deftest aob-autocompact-guards ()
  (aob-tests--with-session s
    (let ((aob-acp-autocompact-ratio 0.85) fired)
      (cl-letf (((symbol-function 'aob-acp--prompt-1)
                 (lambda (&rest _) (setq fired t))))
        (aob-session-put s :ctx-size 1000000)
        (aob-session-put s :ctx-used 950000)
        (aob-set-state s 'idle)
        ;; no /compact advertised → never fires
        (aob-session-put s :commands nil)
        (aob-acp--autocompact-check s)
        (should-not fired)
        ;; advertised but a workflow worker → coordinator's call, skip
        (aob-session-put s :commands '((:name "compact")))
        (aob-session-put s :wf-boss "acp:boss")
        (aob-acp--autocompact-check s)
        (should-not fired)
        ;; advertised, standalone, queue non-empty → not settled
        (aob-session-put s :wf-boss nil)
        (aob-session-put s :queued '(("x" nil nil)))
        (aob-acp--autocompact-check s)
        (should-not fired)
        ;; disabled entirely
        (aob-session-put s :queued nil)
        (let ((aob-acp-autocompact-ratio nil))
          (aob-acp--autocompact-check s)
          (should-not fired))))))

(ert-deftest aob-clear-command-recognized ()
  (should (aob-acp--clear-p "/clear"))
  (should (aob-acp--clear-p "  /clear  "))
  (should (aob-acp--clear-p "/clear now"))
  (should-not (aob-acp--clear-p "/cleared"))
  (should-not (aob-acp--clear-p "/clearcache"))
  (should-not (aob-acp--clear-p "clear"))
  (should-not (aob-acp--clear-p "please /clear"))
  (should-not (aob-acp--clear-p "/compact")))

(ert-deftest aob-clear-wipes-trace-on-completion ()
  ;; /clear resets the agent's context — the trace history from before it
  ;; must collapse to a single marker; an ordinary prompt leaves it intact
  (aob-tests--with-session s
    (aob-set-state s 'idle)
    (aob-event s 'message :text "old history 1")
    (aob-event s 'message :text "old history 2")
    (cl-letf (((symbol-function 'aob-acp--request)
               (lambda (_s _m _params cb)
                 (funcall cb '(:stopReason "end_turn") nil))))
      ;; a normal prompt: history grows, nothing wiped
      (aob-acp--prompt-1 s "hello")
      (should (seq-find (lambda (e) (equal (aob-event-text e) "old history 1"))
                        (aob-session-events s)))
      ;; /clear: everything before collapses to the marker
      (aob-acp--prompt-1 s "/clear")
      (should-not (seq-find (lambda (e) (equal (aob-event-text e) "old history 1"))
                            (aob-session-events s)))
      (should (= (aob-session-nevents s) 1))
      (should (equal (plist-get (car (aob-session-events s)) :title)
                     "context cleared")))))

(ert-deftest aob-remove-gc-reaps-views ()
  ;; removal kills the dead object's views and sheds the struct's bulk
  (aob-tests--with-session s
    (aob-event s 'message :text "hello")
    (let ((buf (aob-trace-buffer s)))
      (should (buffer-live-p buf))
      (aob-remove-session s)
      (should-not (buffer-live-p buf))
      (should-not (aob-session-events s))
      (should-not (aob-session-ref s :queued)))))

(ert-deftest aob-kill-answers-held-decisions ()
  ;; killing a session on a shared connection must not leave the adapter
  ;; waiting on a held permission reply
  (aob-tests--with-session s
    (setf (aob-session-decisions s)
          (list '(:reply-id 7 :kind permission :title "write?")))
    (aob-set-state s 'blocked)
    (let (replied)
      (cl-letf (((symbol-function 'aob-acp--respond)
                 (lambda (_s id result &optional _err)
                   (setq replied (list id result)))))
        (aob-acp--kill s))
      (should (equal (car replied) 7))
      (should (equal (plist-get (plist-get (cadr replied) :outcome) :outcome)
                     "cancelled"))
      (should-not (aob-session-get "acp:test:1")))))

(ert-deftest aob-delete-session-forgets-it-and-closes-its-buffers ()
  (let ((aob-acp-persist-file (make-temp-file "aob-persist-" nil ".eld"))
        (aob-acp--opened-any t))
    (aob-tests--with-session s
      (aob-session-put s :acp-id "sess-test")
      (let ((trace (get-buffer-create " *aob-test-trace*")))
        (with-current-buffer trace
          (setq-local aob-buffer-session-id (aob-session-id s)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'aob-acp--respond) #'ignore))
          (aob-acp-delete-session s))
        (should-not (aob-session-get "acp:test:1"))
        (should-not (buffer-live-p trace))
        (should-not (seq-find (lambda (e) (equal (plist-get e :acp-id) "sess-test"))
                              (aob-acp--persisted-entries)))))
    (delete-file aob-acp-persist-file)))

(ert-deftest aob-want-model-takes-a-name-the-agent-offers ()
  (aob-tests--with-session s
    (let (set)
      (cl-letf (((symbol-function 'aob-acp--model-info)
                 (lambda (_s) (cons "claude-opus-5"
                                    (list (list :value "claude-opus-5" :name "Opus 5")
                                          (list :value "claude-haiku-4-5" :name "Haiku 4.5")))))
                ((symbol-function 'aob-acp--set-model)
                 (lambda (_s id) (setq set id))))
        (aob-acp--want-model s "haiku")
        (should (equal set "claude-haiku-4-5"))
        (setq set nil)
        (aob-acp--want-model s "opus")
        (should-not set)
        (aob-acp--want-model s "gemini")
        (should-not set)))))

(ert-deftest aob-worktree-async-roundtrip ()
  ;; spawn-side add and kill-side reap both run off the main thread;
  ;; a clean merged worktree disappears, and nothing here blocks
  (let ((aob-acp-worktree-root (make-temp-file "aob-wt-root" t))
        (project (make-temp-file "aob-proj" t)))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "-C" project "init" "-q")
          (call-process "git" nil nil nil "-C" project
                        "-c" "user.email=t@t" "-c" "user.name=t"
                        "commit" "--allow-empty" "-q" "-m" "x")
          (let ((dir (aob-acp--worktree-path project "tester"))
                done err)
            (aob-acp--worktree-make project dir
                                    (lambda (e) (setq done t err e)))
            (let ((deadline (+ (float-time) 15)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should done)
            (should-not err)
            (should (file-directory-p dir))
            (let ((s (aob-create-session :id "acp:wt-test" :backend 'acp
                                         :name "wt-test" :project project
                                         :dir dir :state 'idle)))
              (unwind-protect
                  (progn
                    (aob-acp--reap-worktree s)
                    (let ((deadline (+ (float-time) 15)))
                      (while (and (file-directory-p dir)
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.05)))
                    (should-not (file-directory-p dir)))
                (aob-remove-session s)))))
      (let ((deadline (+ (float-time) 5)))
        (while (and (< (float-time) deadline)
                    (seq-some (lambda (p) (string-prefix-p "git" (process-name p)))
                              (process-list)))
          (accept-process-output nil 0.05)))
      (ignore-errors (delete-directory aob-acp-worktree-root t))
      (ignore-errors (delete-directory project t)))))

(ert-deftest aob-compose-paste-attaches-clipboard-image ()
  (with-temp-buffer
    (aob-compose-mode)
    (let ((img (make-temp-file "aob-clip-test" nil ".png")))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'aob-compose--clipboard-image)
                       (lambda () img)))
              (aob-compose-paste))
            (should (equal aob-compose--attachments (list (cons 1 img))))
            (should (equal (buffer-string) "[[Image1]]"))
            (kill-new "plain text")
            (cl-letf (((symbol-function 'aob-compose--clipboard-image)
                       (lambda () nil)))
              (aob-compose-paste))
            (should (equal (buffer-string) "[[Image1]]plain text")))
        (delete-file img)))))

(ert-deftest aob-compose-tokens-editable-and-spawn-carries-atts ()
  ;; [[ImageN]] is the attachment: deleting the token drops the file,
  ;; and a not-yet-live target gets its images through the spawn path
  (with-temp-buffer
    (aob-compose-mode)
    (setq aob-compose--target '(new . "claude"))
    (aob-compose-attach "/tmp/a.png")
    (insert " compare with ")
    (aob-compose-attach "/tmp/b.png")
    (should (equal (buffer-string) "[[Image1]] compare with [[Image2]]"))
    (goto-char (point-min))
    (delete-region (point) (+ (point) (length "[[Image1]]")))
    (let (spawned)
      (cl-letf (((symbol-function 'quit-window) (lambda (&rest _)))
                ((symbol-value 'aob-compose-spawn-function)
                 (lambda (text &optional agent atts)
                   (setq spawned (list text agent atts)))))
        (aob-compose-send))
      (should (equal spawned
                     (list "compare with" "claude"
                           (list "/tmp/b.png")))))))

(ert-deftest aob-model-owns-the-M-verb ()
  ;; a later define-key once re-pointed M at the codex-only picker and
  ;; claude sessions "advertised no models" for a whole day — the verb
  ;; belongs to the both-wires command
  (should (eq (lookup-key aob-object-map "M") #'aob-acp-model)))

(ert-deftest aob-model-wait-unhooks-on-failure ()
  ;; M deferred through the handshake must not fire the picker — or
  ;; leak its hook — when the handshake dies instead of settling
  (aob-tests--with-session s
    (let (picked (depth (length aob-state-change-hook)))
      (cl-letf (((symbol-function 'aob-acp--model-1)
                 (lambda (_s) (setq picked t))))
        (aob-acp-model s)
        (aob-acp--fail s '(:message "boom"))
        (should-not picked)
        (should (= (length aob-state-change-hook) depth))))))

(ert-deftest aob-trace-target-from-anywhere ()
  ;; tail-follow parks point past all propertized text — verbs must
  ;; still resolve the trace's own session there
  (aob-tests--with-session s
    (aob-event s 'message :text "hello")
    (with-current-buffer (aob-trace-buffer s)
      (aob-trace--render t)
      (goto-char (point-max))
      (should (eq (aob-session-at-point) s)))))

(ert-deftest aob-content-blocks-shape ()
  (let ((f (make-temp-file "aob-img" nil ".png" "PNGDATA")))
    (unwind-protect
        (let ((blocks (aob-acp--content-blocks "hi" (list f))))
          (should (= (length blocks) 2))
          (should (equal (plist-get (aref blocks 0) :type) "text"))
          (should (equal (plist-get (aref blocks 1) :type) "image"))
          (should (equal (plist-get (aref blocks 1) :mimeType) "image/png"))
          (should (equal (base64-decode-string
                          (plist-get (aref blocks 1) :data))
                         "PNGDATA")))
      (delete-file f))))

(ert-deftest aob-content-blocks-file-mention ()
  "@path mentions of existing files ride along as resource_link blocks."
  (let* ((dir (make-temp-file "aob-ment" t))
         (rel "sub/note.txt")
         (abs (expand-file-name rel dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory abs) t)
          (write-region "hi" nil abs)
          (let ((blocks (aob-acp--content-blocks
                         (format "read @%s and @nope/missing.txt" rel) nil dir)))
            (should (= (length blocks) 2))
            (should (equal (plist-get (aref blocks 0) :type) "text"))
            (should (equal (plist-get (aref blocks 1) :type) "resource_link"))
            (should (equal (plist-get (aref blocks 1) :name) rel))
            (should (equal (plist-get (aref blocks 1) :uri) (concat "file://" abs))))
          (should (= 1 (length (aob-acp--content-blocks
                                "mail me at foo@bar.com" nil dir)))))
      (delete-directory dir t))))

(ert-deftest aob-embedded-context-capability-gate ()
  ;; the buffer you are looking at is the file the agent must read; an
  ;; agent that never declared embeddedContext still gets the path alone
  (aob-tests--with-session s
    (aob-set-state s 'idle)
    (let* ((dir (make-temp-file "aob-embed" t))
           (abs (expand-file-name "note.py" dir))
           buf sent)
      (unwind-protect
          (progn
            (write-region "on disk\n" nil abs)
            (setf (aob-session-dir s) dir)
            (setq buf (find-file-noselect abs))
            (with-current-buffer buf
              (erase-buffer)
              (insert "in the buffer\n")
              (narrow-to-region (point-min) 3))
            (cl-letf (((symbol-function 'aob-acp--request)
                       (lambda (_s _m params _cb)
                         (setq sent (plist-get params :prompt)))))
              (aob-session-put s :agent-caps
                               '(:promptCapabilities (:image t :embeddedContext t)))
              (aob-acp--prompt-1 s "read @note.py")
              (should (= (length sent) 2))
              (should (equal (plist-get (aref sent 1) :type) "resource"))
              (let ((res (plist-get (aref sent 1) :resource)))
                (should (equal (plist-get res :text) "in the buffer\n"))
                (should (equal (plist-get res :mimeType) "text/x-python"))
                (should (equal (plist-get res :uri) (concat "file://" abs))))
              (aob-session-put s :agent-caps nil)
              (aob-acp--prompt-1 s "read @note.py")
              (should (equal (plist-get (aref sent 1) :type) "resource_link"))
              (should (equal (plist-get (aref sent 1) :name) "note.py"))))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-directory dir t)))))

(ert-deftest aob-content-blocks-embed-limit ()
  "A mention too big, or not text at all, goes back to being a link."
  (let* ((dir (make-temp-file "aob-big" t))
         (abs (expand-file-name "big.txt" dir))
         (bin (expand-file-name "shot.png" dir)))
    (unwind-protect
        (progn
          (write-region (make-string 64 ?x) nil abs)
          (let ((coding-system-for-write 'binary))
            (write-region (unibyte-string #x89 ?P ?N ?G 0 0 #xFE #xFF) nil bin))
          (should (equal (plist-get (aref (aob-acp--content-blocks
                                           "see @shot.png" nil dir t) 1)
                                    :type)
                         "resource_link"))
          (let ((aob-acp-embed-limit 1000))
            (should (equal (plist-get (aref (aob-acp--content-blocks
                                             "see @big.txt" nil dir t) 1)
                                      :type)
                           "resource")))
          (let ((aob-acp-embed-limit 8))
            (should (equal (plist-get (aref (aob-acp--content-blocks
                                             "see @big.txt" nil dir t) 1)
                                      :type)
                           "resource_link"))))
      (delete-directory dir t))))

(ert-deftest aob-mode-update-stored ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"current_mode_update\",\"currentModeId\":\"plan\"}}}")
    (should (equal (aob-session-ref s :mode-id) "plan"))))

(ert-deftest aob-subagent-rollup-counts ()
  "Children roll up into their Task as counts, and as nothing else."
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"T1\",\"title\":\"Task\",\"kind\":\"think\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"Refactor auth\"}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"c1\",\"title\":\"Edit a.ts\",\"kind\":\"edit\",\"status\":\"in_progress\",\"content\":[{\"type\":\"diff\",\"path\":\"a.ts\",\"oldText\":\"x\",\"newText\":\"a\\nb\"}],\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"T1\"}}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"c2\",\"title\":\"npm test\",\"kind\":\"execute\",\"status\":\"in_progress\",\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"T1\"}}}}}")
    (let ((task (gethash "T1" (aob-acp--tools s))))
      (should (equal (plist-get task :children) 2))
      (should (equal (plist-get task :child-live) 2))
      ;; what a child is doing is never kept on the parent, and the
      ;; session's activity line stays the Task, not the child's step
      (should-not (plist-get task :child-last))
      (should (equal (aob-session-summary s) task))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"c1\",\"status\":\"completed\"}}}")
      (should (equal (plist-get task :child-live) 1))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"c2\",\"status\":\"failed\"}}}")
      (should (equal (plist-get task :child-live) 0))
      (should (equal (plist-get task :child-fail) 1))
      (should (equal (aob-session-ref s :turn-fails) 1))
      ;; finished Task: children leave the ring, rollup + diffstat stay
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"T1\",\"status\":\"completed\"}}}")
      (should-not (seq-find (lambda (e) (plist-get e :parent))
                            (aob-session-events s)))
      (should-not (gethash "c1" (aob-acp--tools s)))
      (should (equal (plist-get task :child-stat) "+2 −1"))
      (should (equal (plist-get task :children) 2))
      (should (memq task (aob-session-events s))))))

(ert-deftest aob-subagent-locations-survive-collapse ()
  "A finished Task keeps its children's file locations under its name."
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"T1\",\"title\":\"Task\",\"kind\":\"think\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"Refactor auth\"}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"c1\",\"title\":\"Edit a.ts\",\"kind\":\"edit\",\"status\":\"in_progress\",\"locations\":[{\"path\":\"a.ts\",\"line\":10}],\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"T1\"}}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"T1\",\"status\":\"completed\"}}}")
    (let ((task (gethash "T1" (aob-acp--tools s))))
      (should-not (seq-find (lambda (e) (plist-get e :parent))
                            (aob-session-events s)))
      (should (equal (plist-get (car (plist-get task :child-locs)) :path) "a.ts"))
      (should (equal (plist-get (car (plist-get task :child-locs)) :line) 10)))))

(ert-deftest aob-evict-children-before-narrative ()
  "Cap pressure drops finished children before top-level events."
  (aob-tests--with-session s
    (let ((aob-event-cap 6))
      (dotimes (i 3) (aob-event s 'message :title (format "m%d" i)))
      (dotimes (i 4)
        (aob-event s 'tool :parent "T" :status "completed"
                   :title (format "c%d" i)))
      (let ((evs (aob-session-events s)))
        (should (equal (aob-session-nevents s) 3))
        (should (seq-every-p (lambda (e) (eq (plist-get e :type) 'message))
                             evs))))))

(ert-deftest aob-trace-folds-children ()
  "Children hide behind their Task line until it is expanded."
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"T1\",\"title\":\"Task\",\"kind\":\"think\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"Count files\"}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"c1\",\"title\":\"find | wc -l\",\"kind\":\"execute\",\"status\":\"in_progress\",\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"T1\"}}}}}")
    (with-current-buffer (aob-trace-buffer s)
      (let ((inhibit-read-only t))
        (aob-trace--render t)
        ;; the Task says what it was sent to do and how many steps it
        ;; took — never which step, expanded or not.  `aob-subagents'
        ;; is where a subagent gets read in full.
        (should (string-match-p "Count files" (buffer-string)))
        (should (string-match-p " 1⟳1" (buffer-string)))
        (should-not (string-match-p "find | wc" (buffer-string)))
        (setq aob-trace--expanded
              (list (plist-get (gethash "T1" (aob-acp--tools s)) :seq)))
        (aob-trace--render t)
        (should-not (string-match-p "find | wc" (buffer-string)))))))

(defun aob-tests--read (s id title status)
  "Feed S a read of TITLE as tool ID in STATUS."
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"%s\",\"title\":\"%s\",\"kind\":\"read\",\"status\":\"%s\"}}}"
             id title status)))

(ert-deftest aob-trace-folds-a-run-of-looking-into-one-row ()
  "Four file names down the left margin say the agent read four files
and nothing about the turn they were for.  A run of finished reads is
one step of looking around, drawn as one row that names what it read,
and TAB opens it back into the reads it folded."
  (aob-tests--with-session s
    (dolist (file '("lib/a.dart" "lib/b.dart" "lib/c.dart"))
      (aob-tests--read s file file "completed"))
    (aob-tests--read s "live" "lib/d.dart" "in_progress")
    (with-current-buffer (aob-trace-buffer s)
      (let ((inhibit-read-only t)
            (aob-trace-icons nil))
        (aob-trace--render t)
        (should (string-match-p "Explored 3 a.dart, b.dart, c.dart"
                                (buffer-string)))
        ;; the read still running is the one thing worth watching
        (should (string-match-p "lib/d.dart" (buffer-string)))
        ;; and the folded ones are not repeated outside the row
        (should-not (string-match-p "lib/a.dart" (buffer-string)))
        (setq aob-trace--expanded
              (list (plist-get (gethash "lib/a.dart" (aob-acp--tools s)) :seq)))
        (aob-trace--render t)
        (should (string-match-p "    .*lib/a.dart" (buffer-string)))))))

(ert-deftest aob-trace-a-folded-run-is-the-same-block-twice ()
  "The incremental render skips a block only while it stays the same
object.  A group rebuilt from its parts on every tick would be a new
string each time, and every tick would redraw the whole trace below it."
  (aob-tests--with-session s
    (dolist (file '("lib/a.dart" "lib/b.dart" "lib/c.dart"))
      (aob-tests--read s file file "completed"))
    (with-current-buffer (aob-trace-buffer s)
      (let ((first (aob-trace--blocks-of s))
            (again (aob-trace--blocks-of s)))
        (should (= 1 (length first)))
        (should (eq (car first) (car again)))
        ;; a read that changes takes its group with it
        (aob-tests--feed
         s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"lib/a.dart\",\"status\":\"failed\"}}}")
        (should-not (eq (car first) (car (aob-trace--blocks-of s))))))))

(ert-deftest aob-trace-a-short-look-stays-its-own-row ()
  "Two reads are two reads: folding them costs a keystroke to read back
and saves nothing."
  (aob-tests--with-session s
    (dolist (file '("lib/a.dart" "lib/b.dart"))
      (aob-tests--read s file file "completed"))
    (with-current-buffer (aob-trace-buffer s)
      (let ((inhibit-read-only t)
            (aob-trace-icons nil))
        (aob-trace--render t)
        (should-not (string-match-p "Explored" (buffer-string)))
        (should (string-match-p "lib/a.dart" (buffer-string)))))))

(defmacro aob-tests--capturing (var &rest body)
  "Run BODY with outgoing frames collected newest-first into VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'aob-acp--send-proc)
                (lambda (_proc msg) (push msg ,var))))
       ,@body)))

(defun aob-tests--reply (s result)
  "Answer S's most recent request with RESULT, through the pending table."
  (let* ((pending (process-get (aob-session-conn s) 'aob-pending))
         (ids (or (hash-table-keys pending)
                  (error "aob-tests--reply: nothing is awaiting a reply")))
         (id (apply #'max ids))
         (cb (gethash id pending)))
    (remhash id pending)
    (funcall cb result nil)))

(ert-deftest aob-steering-keeps-the-turn ()
  "A correction goes into the running turn; an idle agent gets a prompt."
  (aob-tests--with-session s
    (aob-session-put s :acp-id "sess-test")
    (aob-session-put s :agent-meta '(:steering (:supported t)))
    (aob-set-state s 'working)
    (aob-tests--capturing frames
      (aob-acp--interject s "use the other endpoint")
      (should (equal (plist-get (car frames) :method) "_session/steering"))
      ;; injected: the turn carries on, nothing was cancelled
      (aob-tests--reply s '(:outcome "injected"))
      (should (eq (aob-session-state s) 'working))
      (should (equal (plist-get (car (aob-session-events s)) :title) "steered"))
      ;; nothing was running: the adapter says so and it becomes a prompt
      (aob-set-state s 'working)
      (setq frames nil)
      (aob-acp--interject s "and skip the cache")
      (aob-tests--reply s '(:outcome "promptRequired"))
      (should (equal (plist-get (car frames) :method) "session/prompt")))
    ;; an agent that never advertised steering keeps the old bargain
    (aob-session-put s :agent-meta nil)
    (aob-set-state s 'working)
    (aob-tests--capturing frames
      (aob-acp--interject s "stop that")
      (should (equal (mapcar #'car (aob-session-ref s :queued)) '("stop that")))
      (should (seq-find (lambda (f) (equal (plist-get f :method) "session/cancel"))
                        frames)))))

(ert-deftest aob-goal-precedes-the-first-prompt ()
  "The goal is set before the queue flushes, and the runtime's answer is kept."
  (aob-tests--with-session s
    (aob-session-put s :agent-meta '(:goal (:controlMethod "_session/goal")))
    (aob-session-put s :want-goal "Ship the fix. Done when: tests pass")
    (let ((flushed nil))
      (cl-letf (((symbol-function 'aob-acp--flush-queue) (lambda (_s) (setq flushed t))))
        (aob-tests--capturing frames
          (aob-acp--session-opened s '(:sessionId "sess-test"))
          (should (equal (plist-get (car frames) :method) "_session/goal"))
          (should (equal (plist-get (plist-get (car frames) :params) :action) "set"))
          (should-not flushed)
          (aob-tests--reply s '(:ok t))
          (should flushed))))
    (should-not (aob-session-ref s :want-goal))
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"session_info_update\",\"_meta\":{\"goal\":{\"objective\":\"Ship the fix\",\"status\":\"active\",\"iterations\":3}}}}}")
    (should (equal (plist-get (aob-session-ref s :goal) :iterations) 3))))

(ert-deftest aob-subagent-text-stays-under-its-task ()
  "Subagent words accumulate under their Task, never in the agent's own message."
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"T1\",\"title\":\"Task\",\"kind\":\"think\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"Audit the exports\"}}}}")
    (dolist (txt '("Looking at ExportController" " and the twig strings."))
      (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"%s\"},\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"T1\"}}}}}" txt)))
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"Main agent talking.\"}}}}")
    (let ((subs (seq-filter (lambda (e) (and (eq (plist-get e :type) 'message)
                                             (plist-get e :parent)))
                            (aob-session-events s)))
          (main (seq-find (lambda (e) (and (eq (plist-get e :type) 'message)
                                           (not (plist-get e :parent))))
                          (aob-session-events s))))
      (should (= (length subs) 1))
      (should (equal (aob-event-text (car subs))
                     "Looking at ExportController and the twig strings."))
      (should (equal (aob-event-text main) "Main agent talking."))
      ;; and the agent's own activity line is never a subagent's voice
      (should (equal (aob-session-blurb s) "Main agent talking.")))))

(defun aob-tests--codex-activity (s id kind name status)
  "Feed S the update codex builds for a subagent activity."
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"%s\",\"kind\":\"other\",\"title\":\"%s subagent %s\",\"status\":\"%s\",\"rawInput\":{\"agentThreadId\":\"thread-%s\",\"agentPath\":\"agents/%s\",\"activityKind\":\"%s\"},\"_meta\":{\"codex\":{\"subagent\":{\"threadId\":\"thread-%s\",\"path\":\"agents/%s\",\"activity\":\"%s\"}}}}}}"
             id (pcase kind ("started" "Start") (_ "Interact with")) name status
             name name kind name name kind)))

(ert-deftest aob-codex-subagents-nest-by-thread ()
  "Codex names a subagent by the thread its activities share; the first
becomes the row, the rest its steps — the shape claude reaches by parent."
  (aob-tests--with-session s
    (aob-tests--codex-activity s "a1" "started" "researcher" "completed")
    (aob-tests--codex-activity s "a2" "interacted" "researcher" "in_progress")
    (aob-tests--codex-activity s "a3" "interacted" "researcher" "completed")
    (aob-tests--codex-activity s "b1" "started" "reviewer" "completed")
    (let ((subs (aob-session-subagents s)))
      (should (equal (mapcar (lambda (e) (plist-get e :title)) subs)
                     '("researcher" "reviewer")))
      (should (equal (plist-get (car subs) :children) 2))
      (should (equal (plist-get (car subs) :child-live) 1)))
    ;; and the activities themselves stay out of the agent's own trace
    (unwind-protect
        (with-current-buffer (aob-trace-buffer s)
          (aob-trace--render t)
          (should-not (string-match-p "Interact with subagent" (buffer-string)))
          (should (string-match-p "researcher" (buffer-string))))
      (dolist (b (buffer-list))
        (when (string-prefix-p "trace:" (buffer-name b)) (kill-buffer b))))))

(ert-deftest aob-extensions-stay-off-without-them ()
  "An adapter that advertises no `_meta' — codex — loses no ground: a goal
it cannot hold never blocks the queue, and steering falls back to cancel."
  (aob-tests--with-session s
    (should-not (aob-acp--steers-p s))
    (should-not (aob-acp--goal-method s))
    (aob-session-put s :want-goal "Ship it")
    (let ((flushed nil))
      (cl-letf (((symbol-function 'aob-acp--flush-queue) (lambda (_s) (setq flushed t))))
        (aob-acp--session-opened s '(:sessionId "sess-test"))
        (should flushed)))
    (should-not (aob-session-ref s :want-goal))))

(ert-deftest aob-first-prompt-can-be-held-at-readiness ()
  "A hook run at readiness holds the opening turn, and the queue keeps it."
  (aob-tests--with-session s
    (aob-session-put s :want-goal "Ship it")
    (let* ((flushed nil)
           (seen nil)
           (aob-acp-before-first-prompt-functions
            (list (lambda (session) (setq seen session) t))))
      (cl-letf (((symbol-function 'aob-acp--flush-queue)
                 (lambda (_s) (setq flushed t))))
        (aob-acp--session-opened s '(:sessionId "sess-held")))
      (should (eq seen s))
      (should-not flushed)
      (should (equal (aob-session-ref s :want-goal) "Ship it")))))

(ert-deftest aob-subagents-panel-lists-delegations ()
  "Every delegation gets a row — finished ones too — and RET opens its steps."
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"T1\",\"title\":\"Task\",\"kind\":\"think\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"Count files\"}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"c1\",\"title\":\"find | wc -l\",\"kind\":\"execute\",\"status\":\"completed\",\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"T1\"}}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"c2\",\"title\":\"rg gift\",\"kind\":\"search\",\"status\":\"in_progress\",\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"T1\"}}}}}")
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"T2\",\"title\":\"Task\",\"kind\":\"think\",\"status\":\"completed\",\"rawInput\":{\"description\":\"Read the schema\"}}}}")
    (should (equal (mapcar (lambda (e) (plist-get e :title))
                           (aob-session-subagents s))
                   '("Count files" "Read the schema")))
    (unwind-protect
        (with-current-buffer (aob-subagents-buffer s)
          (should (string-match-p "1/2 *Count files" (buffer-string)))
          (should (string-match-p "Read the schema" (buffer-string)))
          ;; a child's own step belongs to the subagent's trace, not here
          (should-not (string-match-p "find | wc" (buffer-string)))
          (goto-char (point-min))
          (with-current-buffer (progn (aob-subagents-open) (current-buffer))
            (should (string-match-p "find | wc" (buffer-string)))))
      (dolist (b (buffer-list))
        (when (string-match-p "\\`\\(subs\\|subtrace\\|trace\\):" (buffer-name b))
          (kill-buffer b))))))

(ert-deftest aob-plan-view-and-todo-edits ()
  "Plan updates carry the current task; the view splits doing/queued/done."
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan\",\"entries\":[{\"content\":\"scaffold module\",\"status\":\"completed\",\"priority\":\"high\"},{\"content\":\"implement parser\",\"status\":\"in_progress\",\"priority\":\"high\"},{\"content\":\"write tests\",\"status\":\"pending\",\"priority\":\"medium\"}]}}}")
    (let ((ev (aob-session-ref s :plan-ev)))
      (should (equal (plist-get ev :title) "plan 1/3 · implement parser"))
      (should (equal (aob-session-ref s :plan-tick) 1)))
    (with-current-buffer (aob-plan-buffer s)
      (let ((str (buffer-string)))
        (should (string-match-p "◉ implement parser" str))
        (should (string-match-p "▫ write tests" str))
        (should (string-match-p "── done 1" str))
        (should (< (string-match "◉" str) (string-match "▫" str)))
        (should (< (string-match "▫" str) (string-match "── done" str))))
      ;; a todo edit replaces the entries and re-renders in place
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan\",\"entries\":[{\"content\":\"scaffold module\",\"status\":\"completed\",\"priority\":\"high\"},{\"content\":\"implement parser\",\"status\":\"completed\",\"priority\":\"high\"},{\"content\":\"write tests\",\"status\":\"in_progress\",\"priority\":\"medium\"}]}}}")
      (aob-plan--render)
      (should (string-match-p "◉ write tests" (buffer-string)))
      (should (string-match-p "── done 2" (buffer-string)))
      (should (equal (plist-get (aob-session-ref s :plan-ev) :title)
                     "plan 2/3 · write tests")))))

(defun aob-tests--plan-json (entries)
  (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan\",\"entries\":%s}}}"
          (json-encode entries)))

(defun aob-tests--plan-events (s)
  (seq-filter (lambda (e) (eq (plist-get e :type) 'plan)) (aob-session-events s)))

(ert-deftest aob-plan-lands-in-trace-where-it-changed ()
  "A re-sent plan is one trace event; a plan that changes after the agent
moved on lands again at the tail instead of rewriting a scrolled-past line."
  (aob-tests--with-session s
    (let ((one (aob-tests--plan-json '(((content . "a") (status . "in_progress"))
                                       ((content . "b") (status . "pending")))))
          (two (aob-tests--plan-json '(((content . "a") (status . "completed"))
                                       ((content . "b") (status . "in_progress"))))))
      (aob-tests--feed s one)
      (aob-tests--feed s one)
      (should (= 1 (length (aob-tests--plan-events s))))
      (should (equal 1 (aob-session-ref s :plan-tick)))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"t1\",\"title\":\"x\",\"status\":\"in_progress\"}}}")
      (aob-tests--feed s two)
      (should (= 2 (length (aob-tests--plan-events s))))
      (should (eq (aob-session-ref s :plan-ev) (car (aob-session-events s))))
      (should (equal (plist-get (aob-session-ref s :plan-ev) :title)
                     "plan 1/2 · b"))
      (with-current-buffer (aob-trace-buffer s)
        (let ((aob-trace-icons nil))
          (aob-trace--render t)
          (let ((str (buffer-string)))
            (should (string-match-p "plan 0/2 · a" str))
            (should (string-match-p "plan 1/2 · b" str))
            (should (< (string-match "plan 0/2" str) (string-match "plan 1/2" str))))
          ;; the todo items themselves are the expanded detail
          (setq aob-trace--expanded
                (list (plist-get (aob-session-ref s :plan-ev) :seq)))
          (aob-trace--render t)
          (should (string-match-p "✓ a" (buffer-string)))
          (should (string-match-p "◉ b" (buffer-string))))))))

(ert-deftest aob-plan-survives-eviction ()
  "An evicted plan event leaves no orphan pointer: the next update makes
a fresh event, so a long session keeps showing its plan."
  (aob-tests--with-session s
    (aob-tests--feed s (aob-tests--plan-json '(((content . "a") (status . "pending")))))
    (setf (aob-session-events s) nil (aob-session-nevents s) 0)
    (aob-tests--feed s (aob-tests--plan-json '(((content . "a") (status . "completed")))))
    (should (= 1 (length (aob-tests--plan-events s))))
    (should (eq (aob-session-ref s :plan-ev) (car (aob-session-events s))))))

(ert-deftest aob-plan-removed-clears-the-plan ()
  (aob-tests--with-session s
    (aob-tests--feed s (aob-tests--plan-json '(((content . "a") (status . "pending")))))
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan_removed\"}}}")
    (should-not (aob-session-ref s :plan-ev))
    (should (= 1 (length (aob-tests--plan-events s))))))

(ert-deftest aob-mode-trait-applied-on-open ()
  "A definition's :mode fires session/set_mode as the session opens;
an unadvertised id is refused without a request."
  (aob-tests--with-session s
    (let ((aob-acp-persist-file nil)
          (sent nil))
      (cl-letf (((symbol-function 'aob-acp--request)
                 (lambda (_s method params _cb)
                   (push (cons method params) sent))))
        (aob-session-put s :want-mode "bypassPermissions")
        (aob-acp--session-opened
         s '(:sessionId "sess-test"
             :modes (:currentModeId "default"
                     :availableModes [(:id "default")
                                      (:id "bypassPermissions")])))
        (should (equal (caar sent) "session/set_mode"))
        (should (equal (plist-get (cdar sent) :modeId) "bypassPermissions"))
        (should-not (aob-session-ref s :want-mode))
        (setq sent nil)
        (aob-session-put s :want-mode "no-such-mode")
        (aob-acp--session-opened
         s '(:sessionId "sess-test"
             :modes (:currentModeId "default"
                     :availableModes [(:id "default")])))
        (should-not sent)))))

(ert-deftest aob-capf-commands-and-files ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"available_commands_update\",\"availableCommands\":[{\"name\":\"debug\",\"description\":\"Debug it\"},{\"name\":\"deploy\",\"description\":\"Ship it\"}]}}}")
    (puthash "/tmp/proj/" '("src/auth.ts" "src/main.ts") aob--files-cache)
    (with-temp-buffer
      (aob-compose-mode)
      (setq aob-compose--target (aob-session-id s))
      ;; /command at input start
      (insert "/de")
      (pcase-let ((`(,beg ,end ,table . ,_) (aob-compose-capf)))
        (should (= beg 2))
        (should (= end 4))
        (should (member "debug" table))
        (should (member "deploy" table)))
      ;; /command not at start never completes
      (erase-buffer)
      (insert "run /de")
      (should-not (aob-compose-capf))
      ;; @file anywhere
      (erase-buffer)
      ;; a bare token answers off the tracked tree; once a `/' appears the
      ;; table switches to live path completion and the cache is not consulted
      (insert "look at @src")
      (pcase-let ((`(,beg ,_end ,table . ,_) (aob-compose-capf)))
        (should (= beg 10))
        (should (member "src/auth.ts" (all-completions "src" table)))))))

(ert-deftest aob-compose-per-target ()
  (aob-tests--with-session s
    (let ((b1 (progn (aob-compose s) (current-buffer)))
          (b2 (progn (aob-compose (cons 'new "codex")) (current-buffer))))
      (should-not (eq b1 b2))
      (should (equal (buffer-name b1) "compose:test:1"))
      (should (equal (buffer-name b2) "compose:new-codex"))
      (with-current-buffer b1
        (should (equal aob-compose--target "acp:test:1")))
      (with-current-buffer b2
        (should (equal aob-compose--target '(new . "codex"))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest aob-trace-renders ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"t1\",\"title\":\"Edit auth.ts\",\"kind\":\"edit\",\"status\":\"completed\"}}}")
    (with-temp-buffer
      (aob-trace-mode)
      (setq aob-trace--session-id (aob-session-id s))
      (let ((inhibit-read-only t)) (aob-trace--render))
      (should (string-match-p "Edit auth\\.ts" (buffer-string))))))

(ert-deftest aob-trace-G-lands-on-the-live-edge ()
  "The buffer ends in a newline, so `point-max' is an empty line no motion
puts you on.  `G' lands on the last line with something on it — and a
trace that only follows from `point-max' stops following exactly when you
asked to watch it."
  (with-temp-buffer
    (insert "first\nsecond\nthe live edge\n")
    ;; the edge is the last line with content, not the empty one after it
    (should (= (aob-trace--tail-start)
               (save-excursion (goto-char (point-max))
                               (forward-line -1)
                               (point))))
    ;; standing on that line counts as following
    (goto-char (aob-trace--tail-start))
    (should (>= (point) (aob-trace--tail-start)))
    ;; and the old test — point-max itself — still does
    (goto-char (point-max))
    (should (>= (point) (aob-trace--tail-start)))
    ;; anywhere above it does not
    (goto-char (point-min))
    (should-not (>= (point) (aob-trace--tail-start)))))

(ert-deftest aob-a-renamed-session-keeps-the-id-its-buffers-key-on ()
  "The name is what lists show; the id is what views and the persist file
hold on to.  A rename that moved the id would orphan the trace the
session is writing into, so it moves the name and nothing else."
  (let ((s (aob-create-session :id "acp:rename:1" :backend 'acp
                               :name "claude:7" :state 'idle)))
    (unwind-protect
        (progn
          (aob-rename-session s "  fix-reconnect-race  ")
          (should (equal (aob-session-name s) "fix-reconnect-race"))
          (should (equal (aob-session-id s) "acp:rename:1"))
          (should (eq (aob-session-get "acp:rename:1") s))
          (should-error (aob-rename-session s "   ")))
      (aob-remove-session s))))

(ert-deftest aob-a-session-can-be-numbered-off-something-other-than-its-agent ()
  "A session started for a task is named for the task, and a second one on
the same task is told apart the way two agents always were."
  (let ((s (aob-create-session :id "acp:fix-the-race:1" :backend 'acp
                               :name "fix-the-race:1" :state 'idle)))
    (unwind-protect
        (progn
          (should (equal (aob-acp--gen-name "fix-the-race") "fix-the-race:2"))
          (should (equal (aob-acp--gen-name "claude") "claude:1")))
      (aob-remove-session s))))

(ert-deftest aob-a-resumed-task-session-is-numbered-off-its-task ()
  "A resumed conversation keeps the name it had.  When that id is already
taken it is numbered again — off its task, because falling back to the
agent is how a session that said what it was on stops saying it."
  (let ((held (aob-create-session :id "acp:fix-the-race:1" :backend 'acp
                                  :name "fix-the-race:1" :state 'idle)))
    (unwind-protect
        (cl-letf (((symbol-function 'aob-acp--open)
                   (lambda (_agent name &rest _) name)))
          (should (equal (aob-acp-resume-entry
                          '(:agent "claude" :name "fix-the-race:1"
                            :dir "/tmp/" :acp-id "x"
                            :task "/w/guardio/.aob/tasks/fix-the-race"))
                         "fix-the-race:2"))
          ;; and one with no task behind it still falls back to its agent
          (should (equal (aob-acp-resume-entry
                          '(:agent "claude" :name "fix-the-race:1"
                            :dir "/tmp/" :acp-id "x"))
                         "claude:1")))
      (aob-remove-session held))))

(ert-deftest aob-event-hook-fires-with-the-stored-event ()
  (aob-tests--with-session s
    (let* ((calls nil)
           (aob-event-functions (list (lambda (sess ev) (push (cons sess ev) calls)))))
      (let ((ev (aob-event s 'state :title "one")))
        (should (= (length calls) 1))
        (should (eq (caar calls) s))
        (should (eq (cdar calls) ev))
        (should (eq (cdar calls) (car (aob-session-events s))))))))

(ert-deftest aob-acp-notification-hook-sees-dispatched-and-ignored ()
  (aob-tests--with-session s
    (let* ((calls nil)
           (aob-acp-notification-functions
            (list (lambda (sess method params) (push (list sess method params) calls)))))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"sess-test\",\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"t1\",\"title\":\"x\",\"status\":\"in_progress\"}}}")
      (should (= (length calls) 1))
      (let ((c (car calls)))
        (should (eq (nth 0 c) s))
        (should (equal (nth 1 c) "session/update"))
        (should (equal (plist-get (plist-get (nth 2 c) :update) :sessionUpdate)
                       "tool_call")))
      (let ((before (length (aob-session-events s))))
        (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/unknown_thing\",\"params\":{\"sessionId\":\"sess-test\",\"note\":\"n\"}}")
        (should (= (length calls) 2))
        (let ((c (car calls)))
          (should (equal (nth 1 c) "session/unknown_thing"))
          (should (equal (plist-get (nth 2 c) :note) "n")))
        (should (= (length (aob-session-events s)) before))))))

(ert-deftest aob-event-hook-survives-a-signalling-subscriber ()
  (aob-tests--with-session s
    (let* ((ran nil)
           (aob-event-functions
            (list (lambda (_s _ev) (error "subscriber blew up"))
                  (lambda (_s _ev) (setq ran t)))))
      (let ((ev (aob-event s 'state :title "two")))
        (should ran)
        (should (memq ev (aob-session-events s)))))))

(ert-deftest aob-capf-commands-take-what-the-hook-adds ()
  "A function on `aob-capf-command-functions' adds /commands for the dir."
  (aob-tests--with-session s
    (let ((aob-capf-command-functions
           (list (lambda (dir) (list (list :name "tidy" :description dir)))
                 (lambda (_dir) (error "a broken source adds nothing")))))
      (with-temp-buffer
        (aob-compose-mode)
        (setq aob-compose--target (aob-session-id s))
        (insert "/ti")
        (pcase-let ((`(,_beg ,_end ,table . ,_) (aob-compose-capf)))
          (should (member "tidy" table)))))))

(ert-deftest aob-resumed-session-preserves-project-and-dir ()
  "A resumed session receives its persisted project and dir intact.
The project and dir define the session's folder context for completion
and space assignment, so they must survive the resume handshake."
  (let ((held (aob-create-session :id "acp:work:1" :backend 'acp
                                  :name "work:1" :state 'idle)))
    (unwind-protect
        (let ((captured-args nil))
          (cl-letf (((symbol-function 'aob-acp--open)
                     (lambda (agent name project dir _open _then &optional _prepare)
                       (push (list agent name project dir) captured-args)
                       (aob-create-session :id (concat "acp:" name) :backend 'acp
                                           :name name :project project :dir dir
                                           :state 'idle))))
            (let* ((entry '(:agent "claude" :name "work:2"
                           :project "/Users/shamash/temp/test-work/"
                           :dir "/Users/shamash/temp/test-work/"
                           :acp-id "session-123"
                           :task "/Users/shamash/temp/test-work/.aob/tasks/my-task"))
                   (_s (aob-acp-resume-entry entry)))
              (should (= (length captured-args) 1))
              (should (equal (car captured-args)
                             '("claude" "work:2"
                               "/Users/shamash/temp/test-work/"
                               "/Users/shamash/temp/test-work/"))))))
      (aob-remove-session held))))


;;; Where a session and a draft stand

(ert-deftest aob-acp-project-takes-an-explicit-start-dir-as-given ()
  "A caller that names the folder is not second-guessed.
`aob-acp-start-dir' beats both the start-dir function and the buffer,
and is never climbed past to a `.git' above it — that is how a task in a
worktree ends up spawning in the main checkout."
  (let ((dir (file-name-as-directory (make-temp-file "aob-start" t))))
    (unwind-protect
        (let ((aob-acp-start-dir-function (lambda () "/tmp/"))
              (default-directory temporary-file-directory))
          (make-directory (expand-file-name ".git" dir) t)
          (make-directory (expand-file-name "sub" dir) t)
          (let ((aob-acp-start-dir (expand-file-name "sub" dir)))
            (should (equal (aob-acp--project)
                           (file-name-as-directory (expand-file-name "sub" dir)))))
          (let ((aob-acp-start-dir nil)
                (aob-acp-start-dir-function (lambda () (expand-file-name "sub" dir))))
            (should (equal (aob-acp--project) dir))))
      (delete-directory dir t))))

(ert-deftest aob-acp-open-normalizes-project-and-dir ()
  "One spelling of a folder for every entry point.
A session opened on an unslashed path still reports a directory, so the
buffers made off it stand somewhere `expand-file-name' agrees with."
  (cl-letf (((symbol-function 'aob-acp--connect) (lambda (&rest _) nil)))
    (let ((s (aob-acp--open "claude" "norm:1" "/tmp/proj" "/tmp/proj/wt"
                            #'ignore #'ignore)))
      (unwind-protect
          (progn (should (equal (aob-session-project s) "/tmp/proj/"))
                 (should (equal (aob-session-dir s) "/tmp/proj/wt/")))
        (aob-remove-session s)))))

(ert-deftest aob-acp-open-without-a-dir-falls-back-to-the-project ()
  "A session with no folder of its own stands in its project, not nowhere."
  (cl-letf (((symbol-function 'aob-acp--connect) (lambda (&rest _) nil)))
    (let ((s (aob-acp--open "claude" "norm:2" "/tmp/proj" nil #'ignore #'ignore)))
      (unwind-protect (should (equal (aob-session-dir s) "/tmp/proj/"))
        (aob-remove-session s)))))

(ert-deftest aob-compose-stands-in-the-session-folder ()
  "A draft to a session stands where that session works."
  (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
    (aob-tests--with-session s
      (let ((default-directory "/"))
        (aob-compose s))
      (unwind-protect
          (with-current-buffer "compose:test:1"
            (should (equal default-directory "/tmp/proj/")))
        (kill-buffer "compose:test:1")))))

(ert-deftest aob-compose-reused-draft-follows-the-caller-and-an-explicit-dir ()
  "The folder is set on every call, not only on creation.
`get-buffer-create' hands back yesterday's draft, and a draft that kept
yesterday's folder completes `@file' and `/skill' against the wrong tree."
  (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) buf)))
    (unwind-protect
        (progn
          (let ((default-directory "/tmp/")) (aob-compose '(new . "claude")))
          (with-current-buffer "compose:new-claude"
            (should (equal default-directory "/tmp/")))
          (let ((default-directory "/")) (aob-compose '(new . "claude")))
          (with-current-buffer "compose:new-claude"
            (should (equal default-directory "/")))
          ;; a picked folder wins over the buffer the picker was called from
          (let ((default-directory "/"))
            (aob-compose '(new . "claude") nil nil "/tmp"))
          (with-current-buffer "compose:new-claude"
            (should (equal default-directory "/tmp/"))))
      (when (get-buffer "compose:new-claude") (kill-buffer "compose:new-claude")))))

(ert-deftest aob-acp-spawn-opens-in-the-folder-it-was-started-in ()
  "A spawn belongs to the folder its caller named, project and cwd alike.
The draft, the task root and the space all reach `aob-acp--open' the
same way, so a freeform session keeps the folder it was started in."
  (let ((dir (file-name-as-directory (make-temp-file "aob-spawn" t)))
        captured)
    (unwind-protect
        (cl-letf (((symbol-function 'aob-acp--open)
                   (lambda (_agent _name project d _open _then &optional _prep)
                     (setq captured (list project d))
                     (aob-create-session :id "acp:spawn-test" :backend 'acp
                                         :name "spawn-test" :project project
                                         :dir d :state 'idle))))
          (let ((aob-acp-start-dir dir)
                (aob-acp-start-dir-function (lambda () "/tmp/"))
                (default-directory "/")
                (s nil))
            (unwind-protect
                (progn (setq s (aob-acp-spawn "claude"))
                       (should (equal captured (list dir dir))))
              (when (and s (aob-session-get (aob-session-id s)))
                (aob-remove-session s)))))
      (delete-directory dir t))))

(ert-deftest aob-a-draft-says-whether-its-folder-was-named ()
  "A caller that names a folder is an answer about where a spawn belongs;
the folder of whatever buffer opened the draft is not, and a host that
resolves one for itself has to be able to tell the two apart."
  (let ((named (make-temp-file "aob-composed" t))
        (elsewhere (make-temp-file "aob-elsewhere" t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq default-directory (file-name-as-directory elsewhere))
            (aob-compose '(new . "claude") nil "named" named))
          (with-current-buffer "compose:named"
            (should (equal aob-compose--dir (file-name-as-directory named)))
            (should (equal default-directory (file-name-as-directory named))))
          (with-temp-buffer
            (setq default-directory (file-name-as-directory elsewhere))
            (aob-compose '(new . "claude") nil "unnamed"))
          (with-current-buffer "compose:unnamed"
            (should-not aob-compose--dir)
            (should (equal default-directory
                           (file-name-as-directory elsewhere)))))
      (dolist (b '("compose:named" "compose:unnamed"))
        (when (get-buffer b) (kill-buffer b)))
      (delete-directory named t)
      (delete-directory elsewhere t))))

(ert-deftest aob-a-resumed-session-goes-back-to-the-folder-it-ran-in ()
  "The entry records where the conversation ran, so resuming it from a
buffer standing somewhere else reopens it there and not here."
  (let ((ran (make-temp-file "aob-ranin" t))
        (elsewhere (make-temp-file "aob-elsewhere" t))
        captured)
    (unwind-protect
        (cl-letf (((symbol-function 'aob-acp--connect)
                   (lambda (&rest _) nil)))
          (let ((default-directory (file-name-as-directory elsewhere))
                (aob-acp-start-dir-function (lambda () elsewhere))
                (s nil))
            (unwind-protect
                (progn
                  (setq s (aob-acp-resume-entry
                           (list :agent "claude" :name "resumed-here"
                                 :project ran :dir ran :acp-id "x")))
                  (setq captured (list (aob-session-project s)
                                       (aob-session-dir s)))
                  (should (equal captured
                                 (list (file-name-as-directory ran)
                                       (file-name-as-directory ran)))))
              (when (and s (aob-session-get (aob-session-id s)))
                (aob-remove-session s)))))
      (delete-directory ran t)
      (delete-directory elsewhere t))))

(defun aob-tests--restore (init &optional pref task)
  "Restore a persisted entry against INIT, without a connection.
Returns (METHOD PARAMS RESTORED-BY TASK)."
  (let ((dir (make-temp-file "aob-restore" t))
        (aob-acp-persist-file nil)
        opened s)
    (unwind-protect
        (cl-letf (((symbol-function 'aob-acp--connect)
                   (lambda (sess open then)
                     (setq opened (funcall open init))
                     (funcall then sess '(:sessionId "sid-1"))))
                  ((symbol-function 'aob-acp--session-opened)
                   (lambda (&rest _) nil)))
          (setq s (aob-acp-resume-entry
                   (append (list :agent "claude" :name "restore-me"
                                 :project dir :dir dir :acp-id "sid-1")
                           (when task (list :task task)))
                   pref))
          (list (car opened) (cadr opened)
                (aob-session-ref s :restored-by)
                (aob-session-ref s :task)))
      (when (and s (aob-session-get (aob-session-id s)))
        (aob-remove-session s))
      (delete-directory dir t))))

(ert-deftest aob-an-advertised-resume-is-preferred-to-a-replay ()
  "Resume reconnects without replay, which is the whole point of it:
a restart with dozens of workers must not pull every past update back."
  (let ((got (aob-tests--restore
              '(:agentCapabilities (:loadSession t
                                    :sessionCapabilities (:resume t))))))
    (should (equal (nth 0 got) "session/resume"))
    (should (equal (plist-get (nth 1 got) :sessionId) "sid-1"))
    (should (eq (nth 2 got) 'resume)))
  ;; the spec spells it at the top of the result; adapters here nest it
  (should (equal (car (aob-tests--restore
                       '(:loadSession nil :sessionCapabilities (:resume t)
                         :agentCapabilities (:loadSession t))))
                 "session/resume")))

(ert-deftest aob-an-agent-without-resume-still-replays ()
  "An adapter that only knows session/load gets session/load."
  (let ((got (aob-tests--restore '(:agentCapabilities (:loadSession t)))))
    (should (equal (nth 0 got) "session/load"))
    (should (eq (nth 2 got) 'load))))

(ert-deftest aob-asking-for-the-history-back-overrides-resume ()
  "The owner who wants the transcript in the buffer says so and gets it,
even from an agent that would have reconnected silently."
  (let ((got (aob-tests--restore
              '(:agentCapabilities (:loadSession t
                                    :sessionCapabilities (:resume t)))
              'load)))
    (should (equal (nth 0 got) "session/load"))
    (should (eq (nth 2 got) 'load))))

(ert-deftest aob-an-agent-that-restores-nothing-starts-fresh ()
  "Neither verb advertised is a new session, and it says which it was."
  (let ((got (aob-tests--restore '(:agentCapabilities (:promptCapabilities t)))))
    (should (equal (nth 0 got) "session/new"))
    (should-not (plist-get (nth 1 got) :sessionId))
    (should (eq (nth 2 got) 'fresh))))

(ert-deftest aob-a-resumed-worker-still-knows-its-task ()
  "The daemon finds its worker by the task tag; reconnecting cannot drop it."
  (let ((got (aob-tests--restore
              '(:agentCapabilities (:sessionCapabilities (:resume t)))
              nil "/w/guardio/.aob/tasks/fix-the-race")))
    (should (eq (nth 2 got) 'resume))
    (should (equal (nth 3 got) "/w/guardio/.aob/tasks/fix-the-race"))))

(defmacro aob-tests--with-trace (svar bvar &rest body)
  "Bind SVAR to a bare session and BVAR to a trace buffer rendering it."
  (declare (indent 2))
  `(let* ((aob-trace-icons nil)
          (,svar (aob-create-session :id "trace:bench" :backend 'acp
                                     :name "bench" :state 'idle))
          (,bvar (generate-new-buffer " *aob-trace-test*")))
     (unwind-protect
         (with-current-buffer ,bvar
           (aob-trace-mode)
           (setq aob-trace--session-id (aob-session-id ,svar))
           (let ((inhibit-read-only t)) ,@body))
       (kill-buffer ,bvar)
       (when (aob-session-get (aob-session-id ,svar))
         (aob-remove-session ,svar)))))

(ert-deftest aob-trace-keeps-an-unchanged-block ()
  "A line is built once per event and handed back until the event moves."
  (aob-tests--with-trace s buf
    (let ((built 0))
      (advice-add 'aob-trace--line :before (lambda (&rest _) (cl-incf built)))
      (unwind-protect
          (let ((ev (aob-event s 'message :text "hello")))
            (aob-trace--render-1 s)
            (aob-trace--render-1 s)
            (aob-trace--render-1 s)
            (should (= built 1))
            (should (eq (aob-trace--block s ev) (aob-trace--block s ev)))
            (aob-event-push-text ev " again")
            (aob-trace--render-1 s)
            (should (= built 2))
            (should (string-match-p "again" (aob-trace--block s ev))))
        (advice-mapc (lambda (f _p) (advice-remove 'aob-trace--line f))
                     'aob-trace--line)))))

(ert-deftest aob-trace-bounds-a-huge-message ()
  "A half-megabyte message draws a bounded block that says what was left."
  (aob-tests--with-trace s buf
    (let* ((aob-trace-block-max-chars 4000)
           (ev (aob-event s 'message :text (make-string 512000 ?x)))
           (block (aob-trace--block s ev)))
      (should (< (length block) 4200))
      (should (string-match-p "508000 more chars" block))
      (setq aob-trace--expanded (list (plist-get ev :seq)))
      (let ((open (aob-trace--block s ev)))
        (should (> (length open) (length block)))
        (should (< (length open)
                   (* 4 aob-trace-block-max-chars)))))))

(ert-deftest aob-trace-detail-keeps-its-line-budget ()
  "Bounding is per line: an expanded entry still shows the lines it promised."
  (aob-tests--with-trace s buf
    (let* ((aob-trace-block-max-chars 4000)
           (aob-trace-detail-lines 40)
           (ev (aob-event s 'message
                          :text (mapconcat (lambda (_) (make-string 200 ?a))
                                           (number-sequence 1 60) "\n"))))
      (should (= 40 (length (split-string (aob-trace--detail-block s ev) "\n"))))
      (let ((aob-trace-block-max-chars 40))
        (should (string-match-p "160 more chars"
                                (aob-trace--detail-block s ev)))))))

(ert-deftest aob-trace-appends-one-new-event ()
  "A new event edits the tail: earlier blocks are neither rebuilt nor rewritten."
  (aob-tests--with-trace s buf
    (dotimes (i 20) (aob-event s 'message :text (format "line %d" i)))
    (aob-trace--render-1 s)
    (let* ((built 0)
           ;; where the blocks end, which is before the line you type the
           ;; next prompt in: that line is rewritten by design
           (end (aob-trace--tail-end))
           (mark (copy-marker end))
           (blocks aob-trace--blocks))
      (advice-add 'aob-trace--line :before (lambda (&rest _) (cl-incf built)))
      (unwind-protect
          (progn
            (aob-event s 'message :text "the newest")
            (aob-trace--render-1 s)
            (should (= built 1))
            (should (= (marker-position mark) end))
            (should (> (aob-trace--tail-end) end))
            (should (equal blocks (butlast aob-trace--blocks)))
            (should (cl-every #'eq blocks (butlast aob-trace--blocks))))
        (advice-mapc (lambda (f _p) (advice-remove 'aob-trace--line f))
                     'aob-trace--line)))))

(ert-deftest aob-trace-buried-draws-nothing ()
  "No window shows it, so the render pass walks past it."
  (aob-tests--with-trace s buf
    (aob-register-view buf #'aob-trace--render)
    (unwind-protect
        (let ((drawn 0))
          (advice-add 'aob-trace--render-1 :before (lambda (&rest _) (cl-incf drawn)))
          (unwind-protect
              (progn
                (aob-event s 'message :text "unseen")
                (should-not (get-buffer-window buf t))
                (aob--render-all)
                (should (= drawn 0))
                (should (= (buffer-size) 0))
                (save-window-excursion
                  (set-window-buffer (selected-window) buf)
                  (aob-event s 'message :text "seen")
                  (aob--render-all)
                  (should (> drawn 0))
                  (should (> (buffer-size) 0))))
            (advice-mapc (lambda (f _p) (advice-remove 'aob-trace--render-1 f))
                         'aob-trace--render-1)))
      (setq aob--views (assq-delete-all buf aob--views)))))

(defmacro aob-tests--with-markdown (&rest body)
  "Run BODY with the markdown helper stubbed to face the whole text bold."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'ygg-ui-markdown)
              (lambda (text) (propertize (or text "") 'font-lock-face 'bold))))
     ,@body))

(defun aob-tests--md-face (s needle)
  "The face NEEDLE displays with where it sits in rendered string S."
  (let ((i (string-match (regexp-quote needle) s))
        (char-property-alias-alist '((face font-lock-face))))
    (should i)
    (get-char-property i 'face s)))

(defun aob-tests--md-session ()
  (aob-create-session :id "acp:md:1" :backend 'acp :name "md"
                      :project "/tmp/proj/" :dir "/tmp/proj/" :state 'starting))

(ert-deftest aob-trace-markdown-faces-prose-rows ()
  "A message row reads as markdown; a tool row stays as it is."
  (aob-tests--with-markdown
    (should (memq 'bold (ensure-list
                         (aob-tests--md-face
                          (aob-trace--line '(:type message :ts 0 :text "# head"))
                          "# head"))))
    (should-not (memq 'bold (ensure-list
                             (aob-tests--md-face
                              (aob-trace--line '(:type tool :ts 0 :kind "read"
                                                 :title "# head"))
                              "# head"))))))

(ert-deftest aob-trace-markdown-faces-prose-detail ()
  "Expanded message detail reads as markdown; a diff detail stays as it is."
  (let ((s (aob-tests--md-session)))
    (aob-tests--with-markdown
      (should (eq 'bold (aob-tests--md-face
                         (aob-trace--detail-block
                          s '(:type message :ts 0 :seq 1 :text "*soft*"))
                         "*soft*")))
      (should-not (eq 'bold (aob-tests--md-face
                            (aob-trace--detail-block
                             s '(:type tool :ts 0 :seq 2 :kind "edit"
                                 :content ((:type "diff" :path "a.el"
                                            :newText "*soft*"))))
                            "*soft*"))))))

(ert-deftest aob-trace-markdown-absent-helper-renders-plain ()
  "With no helper the row still carries the text, unfaced."
  (cl-letf (((symbol-function 'ygg-ui-markdown) nil))
    (should-not (fboundp 'ygg-ui-markdown))
    (let ((line (aob-trace--line '(:type message :ts 0 :text "# head"))))
      (should (string-match-p "# head" line))
      (should-not (eq 'bold (aob-tests--md-face line "# head"))))))

(ert-deftest aob-acp-a-death-is-blamed-on-the-line-that-says-why ()
  "An adapter that fails writes the reason and then keeps talking, so
the last line of its stderr is the least useful line in it."
  ;; the real shape: an error, then telemetry after it
  (should (equal "Error: connect ECONNREFUSED 127.0.0.1:4317"
                 (aob-acp--fail-reason
                  (concat "starting\n"
                          "Error: connect ECONNREFUSED 127.0.0.1:4317\n"
                          "[session/create] sessionId=f2e8 phase=register"
                          " durationMs=1 totalMs=684\n"))))
  ;; nothing that reads like blame: the newest line that is not noise
  (should (equal "closing down"
                 (aob-acp--fail-reason
                  (concat "closing down\n"
                          "[session/create] sessionId=f2e8 phase=register"
                          " durationMs=1 totalMs=684\n"
                          "{\"type\":\"system\",\"subtype\":\"dev_intent\"}\n"))))
  ;; nothing but noise: say the noise rather than nothing
  (should (equal "[session/create] sessionId=f2e8 durationMs=1"
                 (aob-acp--fail-reason
                  "[session/create] sessionId=f2e8 durationMs=1\n")))
  (should-not (aob-acp--fail-reason "   \n\n")))

(ert-deftest aob-acp-a-subagent-is-known-by-any-of-its-spellings ()
  "One fact, three spellings.  Reading only the one claude wrote first
leaves every subagent speaking as the agent that sent it, which is five
voices in a lead's transcript with no way to tell whose is whose."
  (should (equal "call_7"
                 (aob-acp--parent-of '(:_meta (:claudeCode (:parentToolUseId "call_7"))))))
  ;; the spec's own spelling, which codex and newer claude send
  (should (equal "call_8" (aob-acp--parent-of '(:_meta (:parentToolCallId "call_8")))))
  (should (equal "call_9" (aob-acp--parent-of '(:_meta (:parentToolUseId "call_9")))))
  (should-not (aob-acp--parent-of '(:_meta (:something "else"))))
  (should-not (aob-acp--parent-of '(:content (:type "text" :text "hi")))))

(ert-deftest aob-acp-servers-go-only-to-an-adapter-that-knows-mcp ()
  "A session/new an adapter rejects is a session that never opens, so
one that names MCP nowhere is sent what it was always sent."
  (let ((aob-acp-mcp-servers
         (list (list :name "ygg-threads" :command "node"
                     :args '("/tmp/s.mjs")
                     :env (list (list :name "YGG_LEAD_INBOX" :value "/tmp/i"))))))
    ;; claude and codex both advertise it
    (let ((sent (aob-acp--mcp-servers '(:agentCapabilities (:mcpCapabilities (:http t))))))
      (should (= 1 (length sent)))
      (should (equal "ygg-threads" (plist-get (aref sent 0) :name)))
      (should (vectorp (plist-get (aref sent 0) :args))))
    ;; an adapter that says nothing about MCP gets nothing
    (should (equal (vector)
                   (aob-acp--mcp-servers '(:agentCapabilities (:loadSession t)))))
    ;; and a caller that bound none sends none either way
    (let ((aob-acp-mcp-servers nil))
      (should (equal (vector)
                     (aob-acp--mcp-servers
                      '(:agentCapabilities (:mcpCapabilities (:http t)))))))))

(ert-deftest aob-acp-an-isolated-connection-is-nobody-elses ()
  "Sessions that share an adapter die together — three tasks ending on
the same stderr line is what that looks like.  A caller that binds the
isolation token gets a process keyed to it, and the env function is
told, so it can hand that process a config home of its own."
  (should (equal '("claude" "/r") (aob-acp--conn-key "claude" "/r")))
  (let ((aob-acp-isolate "decode-path"))
    (should (equal '("claude" "/r" "decode-path")
                   (aob-acp--conn-key "claude" "/r")))
    ;; two tasks in one tree are two keys
    (let ((first (aob-acp--conn-key "claude" "/r")))
      (let ((aob-acp-isolate "barcode"))
        (should-not (equal first (aob-acp--conn-key "claude" "/r")))))))

(ert-deftest aob-acp-a-checkout-keeps-the-servers-it-declares ()
  "Claude reads .mcp.json when it runs itself; over ACP nothing reads it
for us, so a session opened here would have the harness's servers and
not the ones the repository says its work needs."
  (let ((root (make-temp-file "mcp-" t))
        (aob-acp-mcp-servers
         (list (list :name "ygg-threads" :command "node" :args '("/x/s.mjs")
                     :env (list (list :name "YGG_LEAD_INBOX" :value "/x/i"))))))
    (unwind-protect
        (progn
          (write-region
           (concat "{\"mcpServers\":{"
                   "\"notion\":{\"type\":\"http\",\"url\":\"https://mcp.notion.com/mcp\"},"
                   "\"local\":{\"command\":\"uvx\",\"args\":[\"thing\"],"
                   "\"env\":{\"TOKEN\":\"t\"}}}}")
           nil (expand-file-name ".mcp.json" root) nil 'silent)
          (let ((declared (aob-acp-project-mcp-servers root)))
            (should (equal '("notion" "local")
                           (mapcar (lambda (e) (plist-get e :name)) declared)))
            (should (equal "http" (plist-get (car declared) :type)))
            (should (equal ["thing"] (plist-get (cadr declared) :args)))
            (should (equal [(:name "TOKEN" :value "t")]
                           (plist-get (cadr declared) :env))))
          ;; both kinds reach an adapter that takes both, ours last
          (let ((sent (aob-acp--mcp-servers
                       '(:agentCapabilities (:mcpCapabilities (:http t :sse t)))
                       root)))
            (should (equal '("notion" "local" "ygg-threads")
                           (mapcar (lambda (e) (plist-get e :name))
                                   (append sent nil)))))
          ;; and a kind the adapter never claimed is left out rather than
          ;; failing the session it would have ridden in
          (let ((sent (aob-acp--mcp-servers
                       '(:agentCapabilities (:mcpCapabilities (:http nil :sse nil)))
                       root)))
            (should (equal '("local" "ygg-threads")
                           (mapcar (lambda (e) (plist-get e :name))
                                   (append sent nil))))))
      (delete-directory root t))))

(provide 'aob-tests)
;;; aob-tests.el ends here

(ert-deftest aob-a-queued-prompt-is-visible-to-the-idle-hook ()
  "The turn ends idle first and flushes after, so whoever would send its
own turn on that idle can ask what is waiting and stand down."
  (aob-tests--with-session s
    (aob-session-put s :agent-caps '(:promptCapabilities (:image t)))
    (let (cb (seen 'unasked) (order nil))
      (cl-letf (((symbol-function 'aob-acp--request)
                 (lambda (_s _method _params callback) (setq cb callback))))
        (aob-acp--prompt-1 s "first")
        (should (eq (aob-session-state s) 'working))
        (aob-acp--prompt s "second")
        (should (aob-session-queued-p s))
        (let ((hook (lambda (session _old new)
                      (when (eq new 'idle)
                        (setq seen (aob-session-queued-p session))
                        (push 'idle order)))))
          (unwind-protect
              (progn
                (add-hook 'aob-state-change-hook hook)
                (cl-letf (((symbol-function 'aob-acp--flush-queue)
                           (lambda (_s) (push 'flush order))))
                  (funcall cb '(:stopReason "end_turn") nil)))
            (remove-hook 'aob-state-change-hook hook)))
        (should (eq seen t))
        (should (equal (nreverse order) '(idle flush)))))))

(ert-deftest aob-a-session-with-nothing-waiting-answers-no ()
  "The predicate is about pending work, not about having ever queued any."
  (aob-tests--with-session s
    (should-not (aob-session-queued-p s))
    (aob-acp--queue s "later" nil)
    (should (aob-session-queued-p s))
    (aob-acp--drop-queue s)
    (should-not (aob-session-queued-p s))))

(ert-deftest aob-trace-tab-draws-the-fence-it-is-on ()
  ;; TAB keeps its one meaning per place: a fence draws, anything else
  ;; expands the event, and the trace never has to be told which
  (let (drawn expanded)
    (cl-letf (((symbol-function 'ygg-diagram-fence-at-point)
               (lambda () (list "mermaid" "graph TD\n" 1 2)))
              ((symbol-function 'ygg-diagram-toggle-at-point)
               (lambda () (setq drawn t)))
              ((symbol-function 'aob-trace-toggle)
               (lambda () (setq expanded t))))
      (aob-trace-tab))
    (should drawn)
    (should-not expanded)))

(ert-deftest aob-trace-tab-draws-the-image-the-line-names ()
  (let (drawn expanded)
    (cl-letf (((symbol-function 'ygg-diagram-fence-at-point) (lambda () nil))
              ((symbol-function 'ygg-diagram-image-at-point)
               (lambda () "/tmp/shot.png"))
              ((symbol-function 'ygg-diagram-toggle-image-at-point)
               (lambda () (setq drawn t)))
              ((symbol-function 'aob-trace-toggle)
               (lambda () (setq expanded t))))
      (aob-trace-tab))
    (should drawn)
    (should-not expanded)))

(ert-deftest aob-trace-tab-expands-off-a-fence ()
  (let (drawn expanded)
    (cl-letf (((symbol-function 'ygg-diagram-fence-at-point) (lambda () nil))
              ((symbol-function 'ygg-diagram-image-at-point) (lambda () nil))
              ((symbol-function 'ygg-diagram-toggle-at-point)
               (lambda () (setq drawn t)))
              ((symbol-function 'aob-trace-toggle)
               (lambda () (setq expanded t))))
      (aob-trace-tab))
    (should expanded)
    (should-not drawn)))

(ert-deftest aob-trace-tab-owns-the-tab-key ()
  (should (eq (lookup-key aob-trace-mode-map (kbd "TAB")) #'aob-trace-tab)))

(ert-deftest aob-compose-is-a-text-buffer-and-one-seam ()
  "The compose buffer is the prompt and nothing above it: the section
that once drew rows over the text is gone, and the send is the only
place a host is called."
  (should-not (fboundp 'aob-compose-body-start))
  (should-not (fboundp 'aob-compose-refresh-section))
  (should-not (boundp 'aob-compose-section-function))
  (should-not (boundp 'aob-compose--body))
  (with-temp-buffer
    (aob-compose-mode)
    (setq aob-compose--target '(new . "claude"))
    (insert "the ask itself")
    (let (spawned)
      (cl-letf (((symbol-function 'quit-window) (lambda (&rest _)))
                ((symbol-value 'aob-compose-spawn-function)
                 (lambda (text &optional _agent _atts) (setq spawned text))))
        (aob-compose-send))
      (should (equal spawned "the ask itself")))))

(ert-deftest aob-compose-before-send-sees-the-words ()
  ;; where an @path becomes a file the turn carries, while it is still text
  (with-temp-buffer
    (aob-compose-mode)
    (setq aob-compose--target '(new . "claude"))
    (insert "look at @one.el")
    (let (seen)
      (cl-letf (((symbol-function 'quit-window) (lambda (&rest _)))
                ((symbol-value 'aob-compose-before-send-functions)
                 (list (lambda (text) (setq seen text))))
                ((symbol-value 'aob-compose-spawn-function)
                 (lambda (_text &optional _agent _atts) nil)))
        (aob-compose-send))
      (should (equal seen "look at @one.el")))))

(defun aob-tests--rendered (line)
  "LINE as it is drawn, without a display and without faces."
  (substring-no-properties line))

(ert-deftest aob-compose-title-carries-the-target ()
  "The title row says where the draft goes, and a host's tags stand beside it."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-compose s)
          (with-current-buffer "compose:test:1"
            (should (equal header-line-format '((:eval (aob-compose--header)))))
            (should (string-match-p
                     "→ test:1" (aob-tests--rendered (aob-compose--header))))
            (setq aob-compose--tags '("claude in proj" "tdd" "opus"))
            (let ((line (aob-tests--rendered (aob-compose--header))))
              (should (string-match-p "claude in proj · tdd · opus" line))))
          (aob-compose (cons 'new "codex"))
          (with-current-buffer "compose:new-codex"
            (should (string-match-p
                     "→ new codex"
                     (aob-tests--rendered (aob-compose--header))))))
      (dolist (b '("compose:test:1" "compose:new-codex"))
        (when (get-buffer b) (kill-buffer b))))))

(ert-deftest aob-compose-placeholder-shows-only-while-empty-and-is-not-sent ()
  "The empty box says what it wants as an overlay: it goes the moment
there are words, and it never rides along with them."
  (with-temp-buffer
    (aob-compose-mode)
    (setq aob-compose--target '(new . "claude"))
    (should (string-match-p "@ a file"
                            (overlay-get aob-compose--placeholder
                                         'before-string)))
    (insert "the ask itself")
    (should-not (overlay-get aob-compose--placeholder 'before-string))
    (should (equal (buffer-string) "the ask itself"))
    (let (spawned)
      (cl-letf (((symbol-function 'quit-window) (lambda (&rest _)))
                ((symbol-function 'kill-buffer) (lambda (&rest _) nil))
                ((symbol-value 'aob-compose-spawn-function)
                 (lambda (text &optional _agent _atts) (setq spawned text))))
        (aob-compose-send))
      (should (equal spawned "the ask itself")))
    (erase-buffer)
    (should (overlay-get aob-compose--placeholder 'before-string))))

(ert-deftest aob-compose-footer-names-the-counts-and-the-weight ()
  "The footer carries what the draft carries and on the right whatever
the host weighs it at, and no keys."
  (with-temp-buffer
    (aob-compose-mode)
    (setq aob-compose--target '(new . "claude"))
    (should (equal mode-line-format '((:eval (aob-compose--footer)))))
    (let ((aob-compose-status-function (lambda () "2 file(s), 40 tokens")))
      (should-not (string-match-p "send"
                                  (aob-tests--rendered (aob-compose--footer))))
      (should (string-match-p "2 file(s), 40 tokens"
                              (aob-tests--rendered (aob-compose--footer))))
      (should-not (string-match-p "attached\\|mention"
                                  (aob-tests--rendered (aob-compose--footer))))
      (insert "look at @one.el and @two.el ")
      (aob-compose-attach "/tmp/a.png")
      (let ((line (aob-tests--rendered (aob-compose--footer))))
        (should (string-match-p "1 attached" line))
        (should (string-match-p "2 mention(s)" line))))))

(ert-deftest aob-compose-attach-never-holds-one-file-twice ()
  "Attaching the same file again reuses its token."
  (with-temp-buffer
    (aob-compose-mode)
    (aob-compose-attach "/tmp/one.png")
    (aob-compose-attach "/tmp/one.png")
    (should (= 1 (length aob-compose--attachments)))
    (should (equal (buffer-string) "[[Image1]][[Image1]]"))))

(ert-deftest aob-compose-footer-counts-the-draft-tokens ()
  "The footer says what the words weigh, before whatever the host adds."
  (with-temp-buffer
    (aob-compose-mode)
    (insert (make-string 40 ?x))
    (let ((aob-compose-status-function nil))
      (should (string-match-p "10 tokens" (aob-tests--rendered (aob-compose--footer)))))
    (let ((aob-compose-status-function (lambda () "2 file(s), 40 tokens")))
      (should (string-match-p "10 tokens · 2 file(s), 40 tokens"
                              (aob-tests--rendered (aob-compose--footer)))))))

(ert-deftest aob-compose-box-wants-more-lines-as-the-draft-grows ()
  "A short draft keeps the base height and a long one grows to the cap."
  (with-temp-buffer
    (aob-compose-mode)
    (let ((aob-compose-float-height 12) (aob-compose-float-max-height 32))
      (should (= (aob-compose--wanted-height) 12))
      (dotimes (_ 20) (insert "line\n"))
      (should (= (aob-compose--wanted-height) 22))
      (dotimes (_ 40) (insert "line\n"))
      (should (= (aob-compose--wanted-height) 32)))))

(ert-deftest aob-compose-hide-keeps-the-draft ()
  "Hiding the box leaves the words for the next open."
  (with-temp-buffer
    (aob-compose-mode)
    (insert "kept words")
    (cl-letf (((symbol-function 'aob-compose--float-frame) (lambda (_b) nil))
              ((symbol-function 'quit-window) #'ignore))
      (aob-compose-hide))
    (should (equal (buffer-string) "kept words"))))

(ert-deftest aob-compose-send-closes-the-draft-it-was-sent-from ()
  "A spawn that leaves another buffer current still closes the draft."
  (with-temp-buffer
    (aob-compose-mode)
    (insert "words")
    (let ((draft (current-buffer)) (other (generate-new-buffer "other")))
      (setq-local aob-compose--target '(new . "claude"))
      (setq-local aob-compose-spawn-function
                  (lambda (&rest _) (set-buffer other) nil))
      (cl-letf (((symbol-function 'aob-compose--float-frame) (lambda (_b) nil))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
        (aob-compose-send))
      (should-not (buffer-live-p draft))
      (should (buffer-live-p other))
      (kill-buffer other))))

(ert-deftest aob-streaming-text-stays-bounded-until-asked ()
  "Chunks past the live prefix wait in :parts rather than re-joining."
  (let ((aob-event-live-prefix 100)
        (ev (list :type 'message :text "")))
    (dotimes (_ 40) (aob-event-push-text ev "0123456789"))
    (should (<= (length (aob-event-text-so-far ev)) (+ aob-event-live-prefix 10)))
    (should (plist-get ev :parts))
    (should (equal (aob-event-text ev) (mapconcat #'identity
                                                  (make-list 40 "0123456789") "")))))

(ert-deftest aob-transcript-reads-only-the-end ()
  "A long log is read from its tail, whole lines only."
  (skip-unless (fboundp 'aob-transcript--insert-tail))
  (let ((file (make-temp-file "aob-transcript" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file file
            (dotimes (i 5000) (insert (format "{\"n\":%d,\"pad\":\"%s\"}\n" i
                                              (make-string 200 ?x)))))
          (with-temp-buffer
            (aob-transcript--insert-tail file 600)
            (should (equal (count-lines (point-min) (point-max)) 600))
            (goto-char (point-min))
            (should (equal (alist-get 'n (json-parse-string
                                          (buffer-substring-no-properties
                                           (point) (line-end-position))
                                          :object-type 'alist))
                           4400))))
      (delete-file file))))
