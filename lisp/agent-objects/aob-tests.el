;;; aob-tests.el --- ERT tests for aob -*- lexical-binding: t; -*-

;; Run: emacs -Q -batch -L lisp/agent-objects -l aob-tests -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'aob-workflow)
(require 'aob-subagent)
(require 'ygg-todo)
(require 'ygg-diagram nil t)

;; -Q resolves to the real config: a suite run must never rewrite the owner's sessions
(setq aob-acp-persist-file (make-temp-file "aob-tests-sessions-" nil ".eld"))
(require 'aob-transcript nil t)

(defconst aob-tests--file (or load-file-name buffer-file-name))

(defun aob-tests--modal (form)
  "Run FORM in a fresh Emacs with the modal layer and layer-aob loaded.
A child process, so the glue's advice and keys never reach the other tests."
  (unless (and (locate-library "yggdrasil") (locate-library "layer-aob"))
    (ert-skip "no modal layer on the load path"))
  (let ((code (format "%S"
                      `(progn
                         (setq load-prefer-newer t)
                         (defvar ygg-space-state-functions nil)
                         (defvar ygg-space-detail-functions nil)
                         (require 'yggdrasil)
                         (require 'layer-aob)
                         (load ,aob-tests--file nil t)
                         ,form))))
    (with-temp-buffer
      (unless (eq 0 (call-process (expand-file-name invocation-name invocation-directory)
                                  nil t nil "-Q" "--batch"
                                  "-L" (file-name-directory (locate-library "layer-aob"))
                                  "-L" (file-name-directory (locate-library "aob"))
                                  "--eval" code))
        (ert-fail (buffer-string))))))

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
    (let ((aob-prompt-typed t))
      (aob-acp--prompt s "later"))
    (let ((ev (car (aob-session-events s))))
      (should (eq (plist-get ev :type) 'prompt))
      (should (equal (plist-get ev :status) "queued"))
      (aob--modeline-refresh)
      (should (string-match-p "»\\s-*1" aob-modeline-string))
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

(ert-deftest aob-model-owns-the-model-verb ()
  ;; a later define-key once re-pointed M at the codex-only picker and
  ;; claude sessions "advertised no models" for a whole day — the verb
  ;; belongs to the both-wires command, now on the localleader
  (should-not (lookup-key aob-object-map "M"))
  (aob-tests--modal
   '(should (eq (lookup-key (ygg-localleader--get-map 'aob-trace-mode) "l")
                #'aob-acp-model))))

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
          (should (string-match-p "running .* 2 Count files" (buffer-string)))
          (should (string-match-p "done .* 0 Read the schema" (buffer-string)))
          (should-not (string-match-p "find | wc" (buffer-string)))
          (goto-char (point-min))
          (with-current-buffer (progn (aob-subagents-open) (current-buffer))
            (should (string-match-p "find | wc" (buffer-string)))))
      (dolist (b (buffer-list))
        (when (string-match-p "\\`\\(subs\\|trace\\):" (buffer-name b))
          (kill-buffer b))))))

(defconst aob-tests--agent-call
  "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"toolu_A\",\"title\":\"Count files\",\"kind\":\"think\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"Count files\",\"prompt\":\"Count the files under src\",\"subagent_type\":\"general-purpose\"},\"_meta\":{\"claudeCode\":{\"toolName\":\"Agent\",\"subagent\":true}}}}}"
  "An Agent call as claude-agent-acp sends it: titled by its description.")

(defun aob-tests--agent-child (s id title status)
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"%s\",\"title\":\"%s\",\"kind\":\"execute\",\"status\":\"%s\",\"_meta\":{\"claudeCode\":{\"toolName\":\"Bash\",\"parentToolUseId\":\"toolu_A\"}}}}}"
             id title status)))

(defun aob-tests--agent-done (s status)
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"toolu_A\",\"status\":\"%s\",\"_meta\":{\"claudeCode\":{\"toolName\":\"Agent\"}}}}}"
             status)))

(defun aob-tests--kill-views ()
  (dolist (b (buffer-list))
    (when (string-match-p "\\`\\(subs\\|trace\\):" (buffer-name b))
      (kill-buffer b))))

(ert-deftest aob-native-subagent-is-a-session-with-its-own-trace ()
  "A claude Agent call becomes a read-only session under its sender; its
steps are its own trace, one row in the list, and folded in the parent."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-tests--feed s aob-tests--agent-call)
          (aob-tests--agent-child s "k1" "find src | wc -l" "completed")
          (aob-tests--agent-child s "k2" "rg TODO src" "in_progress")
          (let* ((call (car (aob-session-subagents s)))
                 (kid (aob-session-native-child s call)))
            (should (= 1 (length (aob-session-subagents s))))
            (should kid)
            (should (equal "Count files" (aob-session-name kid)))
            (should (equal (aob-session-id s) (aob-session-ref kid :parent-session)))
            (should (memq kid (aob-subagent-children s)))
            (should (eq 'working (aob-session-state kid)))
            (with-current-buffer (aob-subagents-buffer s)
              (should (string-match-p "^running .* 2 Count files" (buffer-string))))
            (aob-tests--agent-done s "completed")
            (should (eq 'done (aob-session-state kid)))
            (with-current-buffer (aob-subagents-buffer s)
              (aob-subagents--render t)
              (should (string-match-p "^done .* 2 Count files" (buffer-string))))
            (with-current-buffer (aob-trace-buffer kid)
              (aob-trace--render t)
              (should (string-match-p "Count the files under src" (buffer-string)))
              (should (string-match-p "find src | wc -l" (buffer-string)))
              (should (string-match-p "rg TODO src" (buffer-string)))
              (should-not (string-match-p "└" (buffer-string)))
              (should (string-match-p "read-only" (format "%s" header-line-format))))
            (with-current-buffer (aob-trace-buffer s)
              (aob-trace--render t)
              (should (string-match-p "Count files" (buffer-string)))
              (should-not (string-match-p "rg TODO src" (buffer-string))))
            (should-error (aob-prompt kid "hello" nil) :type 'user-error)))
      (aob-tests--kill-views))))

(ert-deftest aob-native-subagent-failed-call-fails-its-session ()
  "A failed Agent call leaves its session failed, not working forever."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-tests--feed s aob-tests--agent-call)
          (aob-tests--agent-done s "failed")
          (let ((kid (aob-session-native-child s (car (aob-session-subagents s)))))
            (should (eq 'failed (aob-session-state kid)))
            (with-current-buffer (aob-subagents-buffer s)
              (should (string-match-p "^failed .* 0 Count files" (buffer-string))))))
      (aob-tests--kill-views))))

(ert-deftest aob-native-codex-subagent-is-a-session-with-its-own-trace ()
  "A codex thread becomes a session named for the agent, holding its activities."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-tests--codex-activity s "a1" "started" "researcher" "completed")
          (aob-tests--codex-activity s "a2" "interacted" "researcher" "in_progress")
          (let* ((call (car (aob-session-subagents s)))
                 (kid (aob-session-native-child s call)))
            (should kid)
            (should (equal "researcher" (aob-session-name kid)))
            (should (eq 'working (aob-session-state kid)))
            (should (equal (aob-session-id s) (aob-session-ref kid :parent-session)))
            (with-current-buffer (aob-trace-buffer kid)
              (aob-trace--render t)
              (should (string-match-p "Interact with subagent researcher" (buffer-string))))
            (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call_update\",\"toolCallId\":\"a2\",\"status\":\"completed\"}}}")
            (should (eq 'done (aob-session-state kid)))))
      (aob-tests--kill-views))))

(ert-deftest aob-native-subagent-rows-open-the-call ()
  "RET on a row goes to the Agent call in the trace; o opens the subagent's
own trace; RET on the call in the trace opens it too."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-tests--feed s aob-tests--agent-call)
          (aob-tests--agent-child s "k1" "find src | wc -l" "completed")
          (let* ((call (car (aob-session-subagents s)))
                 (kid (aob-session-native-child s call)))
            (with-current-buffer (aob-subagents-buffer s)
              (goto-char (point-min))
              (let ((buf (aob-subagents-visit)))
                (should (eq buf (get-buffer (aob-trace--name s))))
                (with-current-buffer buf
                  (should (eql (plist-get call :seq)
                               (get-text-property (point) 'aob-event)))
                  (aob-trace-answer))
                (should (equal (aob-trace--name kid) (buffer-name (window-buffer))))))
            (with-current-buffer (aob-subagents-buffer s)
              (goto-char (point-min))
              (with-current-buffer (progn (aob-subagents-open) (current-buffer))
                (should (equal (aob-trace--name kid) (buffer-name)))))))
      (aob-tests--kill-views))))

(ert-deftest aob-native-subagent-start-splits-no-window ()
  "A subagent starting and working shows nothing new: no split, no panel."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) (aob-trace-buffer s))
          (let ((before (length (window-list))))
            (aob-tests--feed s aob-tests--agent-call)
            (aob-tests--agent-child s "k1" "find src | wc -l" "in_progress")
            (with-current-buffer (aob-trace-buffer s) (aob-trace--render t))
            (aob--render-all)
            (should (= before (length (window-list))))
            (should-not (get-buffer-window (aob-subagents--name s)))
            (aob-subagents s)
            (should (get-buffer-window (aob-subagents--name s)))
            (aob-subagents s)
            (should-not (get-buffer-window (aob-subagents--name s)))
            (should (= before (length (window-list))))))
      (aob-tests--kill-views))))

(defmacro aob-tests--with-list (var file &rest body)
  "Run BODY with VAR a fake session whose project is a fresh folder; FILE
is bound to a function returning the session's list file."
  (declare (indent 2))
  `(let ((dir (file-name-as-directory (make-temp-file "aob-todo-" t))))
     (advice-add 'aob-acp--plan :after #'ygg-todo-mirror-plan)
     (unwind-protect
         (aob-tests--with-session ,var
           (setf (aob-session-project ,var) dir
                 (aob-session-dir ,var) dir)
           (let ((,file (lambda () (ygg-todo-session-file ,var))))
             ,@body))
       (advice-remove 'aob-acp--plan #'ygg-todo-mirror-plan)
       (delete-directory dir t))))

(defun aob-tests--list-items (file)
  (mapcar (lambda (it) (cons (plist-get it :text) (and (plist-get it :done) t)))
          (plist-get (ygg-todo-read file) :items)))

(ert-deftest aob-todo-plan-snapshot-makes-and-binds-the-list ()
  "The first plan starts the session's tasks.md under .aob/tasks and fills it."
  (aob-tests--with-list s file
    (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "in_progress"))
                                               ((content . "Write the fix") (status . "pending")))))
    (should (funcall file))
    (should (string-prefix-p (expand-file-name ".aob/tasks/" (aob-session-project s))
                             (funcall file)))
    (should (equal '(("Read the code") ("Write the fix"))
                   (aob-tests--list-items (funcall file))))))

(ert-deftest aob-todo-plan-snapshot-updates-the-agents-items ()
  "A later snapshot ticks, unticks, rewords and drops the agent's own items."
  (aob-tests--with-list s file
    (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "in_progress"))
                                               ((content . "Write the fix") (status . "pending"))
                                               ((content . "Ship it") (status . "pending")))))
    (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "completed"))
                                               ((content . "Write the fix") (status . "in_progress"))
                                               ((content . "Ship it") (status . "pending")))))
    (should (equal '(("Read the code" . t) ("Write the fix") ("Ship it"))
                   (aob-tests--list-items (funcall file))))
    (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "pending"))
                                               ((content . "Write the fix and a test") (status . "completed")))))
    (should (equal '(("Read the code") ("Write the fix and a test" . t))
                   (aob-tests--list-items (funcall file))))
    (aob-tests--feed s (aob-tests--plan-json nil))
    (should (null (aob-tests--list-items (funcall file))))))

(ert-deftest aob-todo-user-items-survive-and-reach-the-agent ()
  "What the user adds stays through a snapshot and goes out as a note."
  (aob-tests--with-list s file
    (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "pending")))))
    (let ((ygg-todo-by 'user)) (ygg-todo-add (funcall file) "Ask about the API"))
    (with-temp-buffer
      (insert "- [ ] Check the logs\n")
      (append-to-file (point-min) (point-max) (funcall file)))
    (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "completed"))
                                               ((content . "Write the fix") (status . "pending")))))
    (let ((items (aob-tests--list-items (funcall file))))
      (should (assoc "Ask about the API" items))
      (should (assoc "Check the logs" items))
      (should (equal '("Read the code" . t) (assoc "Read the code" items)))
      (should (assoc "Write the fix" items)))
    (let ((note (ygg-todo-session-note s)))
      (should (string-match-p "added: Ask about the API" note))
      (should (string-match-p "added: Check the logs" note)))
    (should-not (ygg-todo-session-note s))))

(ert-deftest aob-todo-user-removed-item-stays-removed ()
  "An item the user took out is not put back by a plan that still names it."
  (aob-tests--with-list s file
    (let ((plan (aob-tests--plan-json '(((content . "Read the code") (status . "pending"))
                                        ((content . "Write the fix") (status . "pending"))))))
      (aob-tests--feed s plan)
      (let ((ygg-todo-by 'user))
        (ygg-todo-remove (funcall file) "Write the fix"))
      (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "completed"))
                                                 ((content . "Write the fix") (status . "pending")))))
      (should (equal '(("Read the code" . t)) (aob-tests--list-items (funcall file))))
      (should (string-match-p "removed: Write the fix" (ygg-todo-session-note s))))))

(declare-function aob-todo-buffer "aob-todo-view" (s))

(ert-deftest aob-todo-view-shows-the-step-in-hand-over-the-list ()
  "The list view draws tasks.md, with the plan's current step as one muted
line above it and no second copy of the plan."
  (require 'aob-todo-view)
  (aob-tests--with-list s file
    (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "completed"))
                                               ((content . "Write the fix") (status . "in_progress"))
                                               ((content . "Ship it") (status . "pending")))))
    (should (funcall file))
    (unwind-protect
        (with-current-buffer (aob-todo-buffer s)
          (goto-char (point-min))
          (should (looking-at "◐ now: Write the fix\n"))
          (should (eq 'shadow (get-text-property (point) 'font-lock-face)))
          (should (= 2 (how-many "Write the fix" (point-min) (point-max))))
          (should (= 1 (how-many "Ship it" (point-min) (point-max))))
          (should (string-match-p "\\[x\\] [0-9.]+ Read the code" (buffer-string)))
          (should (string-match-p "\\[ \\] [0-9.]+ Ship it" (buffer-string)))
          (should-not (string-match-p "agent plan" (buffer-string))))
      (kill-buffer (aob-todo-buffer s)))))

(ert-deftest aob-acp-repeated-tool-call-updates-the-call ()
  "A tool_call re-sent with an id already known is that call changing,
not a second one: the parent's child count stays true."
  (aob-tests--with-session s
    (aob-tests--feed s aob-tests--agent-call)
    (aob-tests--agent-child s "k1" "find src | wc -l" "in_progress")
    (aob-tests--agent-child s "k1" "find src | wc -l" "completed")
    (let ((call (car (aob-session-subagents s))))
      (should (= 1 (seq-count (lambda (e) (equal (plist-get e :tool-id) "k1"))
                              (aob-session-events s))))
      (should (equal 1 (plist-get call :children)))
      (should (equal 0 (plist-get call :child-live)))
      (should (equal "completed"
                     (plist-get (seq-find (lambda (e) (equal (plist-get e :tool-id) "k1"))
                                          (aob-session-events s))
                                :status))))))

(ert-deftest aob-acp-a-withdrawn-question-stops-waiting ()
  "Codex takes a question back with $/cancel_request when it resolves it on
its own: the decision closes, the trace says so, and a late answer is not sent."
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"elicitation/create\",\"params\":{\"sessionId\":\"sess-test\",\"mode\":\"form\",\"toolCallId\":\"item-1\",\"message\":\"Which branch?\",\"requestedSchema\":{\"type\":\"object\",\"properties\":{\"branch\":{\"type\":\"string\",\"title\":\"Which branch?\"}},\"required\":[\"branch\"]},\"_meta\":{\"codex\":{\"autoResolutionMs\":30000}}}}")
    (let ((d (car (aob-session-decisions s))))
      (should d)
      (should (eq 'blocked (aob-session-state s)))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"$/cancel_request\",\"params\":{\"requestId\":7}}")
      (should-not (aob-session-decisions s))
      (should (eq 'working (aob-session-state s)))
      (should (eq 'withdrawn (plist-get (aob-acp--decision-event s d) :answer)))
      (let ((sent nil))
        (cl-letf (((symbol-function 'aob-acp--respond) (lambda (&rest _) (setq sent t))))
          (should-error (aob-acp--resolve s d '(("branch" . "main"))) :type 'user-error))
        (should-not sent))
      (unwind-protect
          (with-current-buffer (aob-trace-buffer s)
            (aob-trace--render t)
            (should (string-match-p "withdrawn by the agent" (buffer-string))))
        (aob-tests--kill-views)))))

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
      ;; /command after a space completes too, where a skill is named
      ;; mid-sentence; a slash inside a path never does
      (erase-buffer)
      (insert "run /de")
      (pcase-let ((`(,beg ,_end ,table . ,_) (aob-compose-capf)))
        (should (= beg 6))
        (should (member "debug" table)))
      (erase-buffer)
      (insert "see src/ma")
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
                           (file-name-as-directory
                            (file-truename (expand-file-name "sub" dir))))))
          (let ((aob-acp-start-dir nil)
                (aob-acp-start-dir-function (lambda () (expand-file-name "sub" dir))))
            (should (equal (aob-acp--project)
                           (file-name-as-directory (file-truename dir))))))
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
                       (let ((real (file-name-as-directory (file-truename dir))))
                         (should (equal captured (list real real)))))
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

(defun aob-tests--aob-command-p (def)
  (and (symbolp def) (string-match-p "\\`\\(?:ygg-\\)?aob-" (symbol-name def))))

(ert-deftest aob-modal-trace-insert-state-types ()
  "In insert state no key of the trace's own reaches a printing key, DEL or RET."
  (aob-tests--modal
   '(progn
     (with-temp-buffer
       (aob-trace-mode)
       (ygg-insert-state)
       (dolist (c (number-sequence 32 126))
         (unless (eq c ?j)
           (should (eq (key-binding (string c)) #'self-insert-command))))
       (dolist (k (list (kbd "DEL") (kbd "RET")))
         (let ((def (key-binding k)))
           (should-not (aob-tests--aob-command-p def))
           (should-not (eq def #'scroll-down-command))))))))

(ert-deftest aob-modal-trace-normal-state-keys ()
  "Normal state: vim motions, the kept verbs, and nothing that shadows an operator."
  (aob-tests--modal
   '(progn
     (with-temp-buffer
       (aob-trace-mode)
       (ygg-normal-state)
       (should (eq (key-binding "j") #'ygg-j))
       (should (eq (key-binding "k") #'ygg-k))
       (should (eq (key-binding (kbd "RET")) #'aob-trace-answer))
       (should (eq (key-binding "a") #'aob-compose))
       (should (eq (key-binding "C") #'aob-trace-comment))
       (should (eq (key-binding "i") #'aob-compose))
       (should (eq (key-binding "c") #'aob-steer))
       (should (eq (key-binding "ZZ") #'aob-trace-send))
       (dolist (k '("K" "x" "t" "T" "M" "E" "X" "S" "p" "r" "R" "d" "y" "w" "b"
                    "e" "u" "v" "s" "n" "f" "h" "l" "0" "$" "G"))
         (should-not (aob-tests--aob-command-p (key-binding k))))))))

(ert-deftest aob-modal-agent-verbs-on-localleader ()
  (aob-tests--modal
   '(progn
     (let ((trace (ygg-localleader--get-map 'aob-trace-mode))
           (plan (ygg-localleader--get-map 'aob-plan-mode)))
       (pcase-dolist (`(,k . ,def) '(("C" . aob-cancel) ("k" . aob-kill-session)
                                      ("x" . aob-acp-command) ("d" . aob-todo)
                                      ("a" . ygg-aob-activity) ("M" . aob-acp-cycle-mode)
                                      ("e" . aob-trace-queue-edit) ("X" . aob-trace-queue-drop)
                                      ("s" . aob-trace-queue-steer)))
         (should (eq (lookup-key trace k) def)))
       (should (eq (lookup-key plan "C") #'aob-cancel))
       (should (eq (lookup-key plan "c") #'aob-acp-mcp))))))

(ert-deftest aob-modal-zz-sends-only-in-the-trace ()
  (aob-tests--modal
   '(progn
     (with-temp-buffer
       (aob-plan-mode)
       (ygg-normal-state)
       (should-not (eq (key-binding "ZZ") #'aob-trace-send))))))

(ert-deftest aob-modal-todo-keys-in-normal-state ()
  (aob-tests--modal
   '(progn
     (with-temp-buffer
       (aob-todo-mode)
       (should ygg--normal-p)
       (pcase-dolist (`(,k . ,def) '(("o" . aob-todo-add) ("a" . aob-todo-add)
                                      ("c" . aob-todo-edit) ("x" . aob-todo-toggle-done)
                                      ("dd" . aob-todo-remove-item) ("gr" . aob-todo-refresh)
                                      ("C" . aob-todo-comment) ("\r" . aob-todo-open-at-line)
                                      ("q" . quit-window) ("j" . ygg-j) ("k" . ygg-k)
                                      ("gg" . ygg-goto-first) ("G" . ygg-goto-last-line)))
         (should (eq (key-binding k) def)))
       (should-not (eq (key-binding " ") #'scroll-up-command))))))

(ert-deftest aob-modal-lists-refresh-on-g-r ()
  (aob-tests--modal
   '(progn
     (with-temp-buffer
       (aob-context-mode)
       (should (eq (key-binding "gr") #'aob-context-list))
       (should (eq (key-binding "gg") #'ygg-goto-first))
       (should (eq (key-binding "d") #'aob-context-drop)))
     (with-temp-buffer
       (aob-acp-mcp-mode)
       (should (eq (key-binding "gr") #'aob-acp-mcp-refresh))
       (should (eq (key-binding "j") #'ygg-j))))))

(ert-deftest aob-comment-leaves-a-plain-buffer-stateless ()
  "Holding a comment from a buffer without the modal layer gives it no state."
  (aob-tests--modal
   '(progn
     (aob-tests--with-session s
       (with-temp-buffer
         (aob-trace--add-comment s 1 "quoted" "a comment")
         (should-not ygg--state)
         (should-not ygg--normal-p))))))

(ert-deftest aob-context-drop-keeps-the-line ()
  (let ((aob-context--items (list (list :file "/c" :text "c")
                                  (list :file "/b" :text "b")
                                  (list :file "/a" :text "a"))))
    (with-temp-buffer
      (aob-context--render)
      (goto-char (point-min))
      (forward-line 3)
      (should (equal (plist-get (get-text-property (point) 'aob-context) :file) "/b"))
      (aob-context-drop)
      (should (= (line-number-at-pos) 4))
      (should (equal (plist-get (get-text-property (point) 'aob-context) :file) "/c")))))

(ert-deftest aob-plan-render-keeps-the-line ()
  (aob-tests--with-session s
    (aob-session-put s :plan-ev (list :entries (vector (list :content "one" :status "pending")
                                                       (list :content "two" :status "pending")
                                                       (list :content "three" :status "pending"))))
    (with-temp-buffer
      (aob-plan-mode)
      (setq aob-plan--session-id (aob-session-id s))
      (aob-plan--render t)
      (goto-char (point-min))
      (forward-line 1)
      (aob-plan--render t)
      (should (= (line-number-at-pos) 2)))))

(ert-deftest aob-queue-rewrite-refuses-a-sent-message ()
  (aob-tests--with-session s
    (let* ((ev (list :type 'user :seq 7 :text "old"))
           (entry (list "old" nil ev)))
      (aob-session-put s :queued (list entry))
      (aob-trace--queue-rewrite s entry "new")
      (should (equal (car entry) "new"))
      (should (equal (plist-get ev :text) "new"))
      (aob-session-put s :queued nil)
      (should-error (aob-trace--queue-rewrite s entry "newer") :type 'user-error))))

(defconst aob-tests--usage-update
  "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"sess-test\",\"update\":{\"sessionUpdate\":\"usage_update\",\"used\":434308,\"size\":1000000,\"cost\":{\"amount\":%s,\"currency\":\"USD\"}%s}}}"
  "A usage_update as claude-agent-acp sends it once a turn's result is in.")

(defconst aob-tests--prompt-response
  "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"stopReason\":\"end_turn\",\"usage\":{\"inputTokens\":%d,\"outputTokens\":%d,\"cachedReadTokens\":7348000,\"cachedWriteTokens\":22125,\"totalTokens\":%d},\"_meta\":{\"quota\":{\"token_count\":{\"totalTokens\":%d},\"model_usage\":[]}}}}"
  "A session/prompt response as claude-agent-acp answers a finished turn.")

(defun aob-tests--stop (s)
  (seq-find (lambda (e) (eq (plist-get e :type) 'stop)) (aob-session-events s)))

(ert-deftest aob-usage-captured-from-adapter-payloads ()
  (aob-tests--with-session s
    (aob-set-state s 'idle)
    (aob-acp--prompt-1 s "one")
    (aob-tests--feed s (format aob-tests--usage-update "1.25" ""))
    (should (= (aob-session-ref s :ctx-used) 434308))
    (aob-tests--feed s (format aob-tests--prompt-response 1 50 17464 7387639 7387639))
    (let ((stop (aob-tests--stop s)))
      (should (= (plist-get stop :cost) 1.25))
      (should (= (plist-get (plist-get stop :usage) :outputTokens) 17464))
      (should (numberp (plist-get stop :secs))))
    (should (equal (aob-session-spend s) "$1.25"))
    (aob-acp--prompt-1 s "two")
    (aob-tests--feed s (format aob-tests--usage-update "3.0" ""))
    (aob-tests--feed s (format aob-tests--prompt-response 2 72 27641 16811843 16811843))
    (should (= (plist-get (aob-tests--stop s) :cost) 1.75))
    (should (= (aob-session-cost s) 3.0))
    (should (= (aob-session-ref s :turns) 2))
    (let ((tk (aob-session-ref s :tokens)))
      (should (= (plist-get tk :outputTokens) (+ 17464 27641)))
      (should (= (plist-get tk :inputTokens) 122))
      (should (= (plist-get tk :totalTokens) (+ 7387639 16811843))))
    (should (string-match-p "2 turns" (aob-usage-describe s)))
    (should (string-match-p "45.1k out" (aob-usage-describe s)))))

(ert-deftest aob-usage-cost-resets-and-autonomous-spend ()
  (aob-tests--with-session s
    (aob-set-state s 'idle)
    (aob-acp--prompt-1 s "one")
    (aob-tests--feed s (format aob-tests--usage-update "2.0" ""))
    (aob-tests--feed s (format aob-tests--prompt-response 1 1 1 1 1))
    (aob-tests--feed s (format aob-tests--usage-update "2.5"
                               ",\"_meta\":{\"_claude/origin\":{\"kind\":\"task\"}}"))
    (should (= (aob-session-cost s) 2.5))
    (aob-acp--prompt-1 s "after a reset")
    (aob-tests--feed s (format aob-tests--usage-update "0.4" ""))
    (aob-tests--feed s (format aob-tests--prompt-response 2 1 1 1 1))
    (should (= (plist-get (aob-tests--stop s) :cost) 0.4))
    (should (< (abs (- (aob-session-cost s) 2.9)) 1e-9))))

(ert-deftest aob-usage-zero-reading-is-no-reset ()
  (aob-tests--with-session s
    (dolist (amount '(2.0 0 2.5))
      (aob-usage-note-cost s amount "USD"))
    (should (= (aob-session-cost s) 2.5))))

(ert-deftest aob-usage-first-reading-of-a-resumed-session-is-no-turns ()
  (aob-tests--with-session s
    (aob-session-put s :restored-by "load")
    (aob-set-state s 'idle)
    (aob-acp--prompt-1 s "again")
    (aob-tests--feed s (format aob-tests--usage-update "46.32" ""))
    (aob-tests--feed s (format aob-tests--prompt-response 1 1 1 1 1))
    (should-not (plist-get (aob-tests--stop s) :cost))
    (should (equal (aob-session-spend s) "$46.32"))))

(ert-deftest aob-duration-and-cost-format ()
  (should (equal (aob-duration-short 0) "0s"))
  (should (equal (aob-duration-short 42.7) "42s"))
  (should (equal (aob-duration-short 130) "2m"))
  (should (equal (aob-duration-short 130 t) "2m 10s"))
  (should (equal (aob-duration-short 120 t) "2m"))
  (should (equal (aob-duration-short 3900) "1h05m"))
  (should (equal (aob-cost-short 0.4237) "$0.43"))
  (should (equal (aob-cost-short 0.07) "$0.07"))
  (should (equal (aob-cost-short 0.000597) "$0.01"))
  (should (equal (aob-cost-short 1.5 "EUR") "EUR 1.50"))
  (should (equal (aob-turn-meter (list :secs 130 :cost 4.69 :usage '(:outputTokens 27641)))
                 "2m 10s · 27.6k out · $4.69")))

(ert-deftest aob-turn-clock-totals-and-supersession ()
  (aob-tests--with-session s
    (let ((now 100.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (let ((first (aob-turn-begin s)))
          (setq now 130.0)
          (should (equal (aob-session-clock s) "30s…"))
          (should (equal (aob-session-clock s t) "30s… / 30s"))
          (should (= (aob-turn-end s first) 30.0))
          (should-not (aob-session-ref s :turn-start))
          (should (equal (aob-session-clock s) "30s")))
        (setq now 200.0)
        (let ((old (aob-turn-begin s)))
          (setq now 210.0)
          (let ((new (aob-turn-begin s)))
            (setq now 400.0)
            (aob-turn-end s old)
            (should (eql (aob-session-ref s :turn-start) new))
            (should (= (aob-session-work-secs s) (+ 30 10 190)))
            (aob-turn-end s new)
            (should (= (aob-session-work-secs s) (+ 30 10 190)))))
        (aob-turn-begin s)
        (setq now 460.0)
        (aob-set-state s 'dead)
        (should-not (aob-session-ref s :turn-start))
        (should (equal (aob-session-clock s) "4m"))))
    (when (timerp aob--clock-timer)
      (cancel-timer aob--clock-timer)
      (setq aob--clock-timer nil))))

(ert-deftest aob-queue-reorder-footer-and-flush-order ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (dolist (text '("alpha" "beta" "gamma"))
      (aob-acp--prompt s text))
    (aob-event s 'message :text "still answering")
    (let ((q (aob-session-ref s :queued)))
      (aob-queue-move s (nth 2 q) -2)
      (should (equal (mapcar #'car (aob-session-ref s :queued)) '("gamma" "alpha" "beta")))
      (aob-queue-move s (nth 0 (aob-session-ref s :queued)) 5)
      (should (equal (mapcar #'car (aob-session-ref s :queued)) '("alpha" "beta" "gamma"))))
    (with-current-buffer (aob-trace-buffer s)
      (aob-trace--render t)
      (let ((text (buffer-string)))
        (should (< (string-match "still answering" text) (string-match "alpha" text)))
        (should (< (string-match "alpha" text) (string-match "beta" text)))
        (should (string-match-p "» 3 queued · sent as one" text)))
      (goto-char (point-min))
      (search-forward "alpha")
      (aob-trace-queue-later)
      (should (equal (mapcar #'car (aob-session-ref s :queued)) '("beta" "alpha" "gamma")))
      (should (aob-trace--queued-at-point))
      (should (equal (car (aob-trace--queued-at-point)) "alpha")))
    (aob-set-state s 'idle)
    (with-current-buffer (aob-trace-buffer s)
      (aob-trace--render t)
      (should (string-match-p "» 3 held · \\\\ s sends now" (buffer-string))))
    (let (sent)
      (cl-letf (((symbol-function 'aob-acp--request)
                 (lambda (_s _m params &rest _) (setq sent params))))
        (with-current-buffer (aob-trace-buffer s)
          (aob-trace-queue-steer)))
      (should (equal (plist-get (aref (plist-get sent :prompt) 0) :text)
                     "beta\n\nalpha\n\ngamma"))
      (should (equal (mapcar (lambda (e) (plist-get e :text))
                             (seq-filter (lambda (e) (eq (plist-get e :type) 'prompt))
                                         (reverse (aob-session-events s))))
                     '("beta" "alpha" "gamma"))))
    (with-current-buffer (aob-trace-buffer s)
      (aob-trace--render t)
      (should-not (string-match-p "queued\\|held" (buffer-string))))
    (aob-turn-end s)))

(ert-deftest aob-trace-shows-turn-meter-and-session-totals ()
  (aob-tests--with-session s
    (aob-set-state s 'idle)
    (aob-acp--prompt-1 s "one")
    (aob-tests--feed s (format aob-tests--usage-update "1.25" ""))
    (aob-tests--feed s (format aob-tests--prompt-response 1 50 17464 7387639 7387639))
    (with-current-buffer (aob-trace-buffer s)
      (aob-trace--render t)
      (should (string-match-p "done (end_turn) · 7.4M ctx · [0-9]+s · 17.5k out · \\$1.25"
                              (buffer-string)))
      (should (string-match-p "\\` test:1 · idle · [0-9]+s · \\$1.25 · 434k ctx\\'"
                              header-line-format))
      (should-not (string-match-p "out\\|/proj\\|%" header-line-format))
      (should-not (get-text-property 1 'face header-line-format))
      (should (eq (get-text-property (string-search "434k" header-line-format) 'face
                                     header-line-format)
                  'warning))
      (should (eq (get-text-property (1- (length header-line-format)) 'face header-line-format)
                  'shadow)))))

(defmacro aob-tests--with-trace-session (var &rest body)
  "Bind VAR to a wired session and run BODY in its rendered trace."
  (declare (indent 1))
  `(aob-tests--with-session ,var
     (let ((aob-trace-icons nil)
           (buf (aob-trace-buffer ,var)))
       (unwind-protect
           (with-current-buffer buf
             (aob-set-state ,var 'working)
             ,@body)
         (kill-buffer buf)))))

(defun aob-tests--request (s id method params)
  "Feed S a request frame, serialised and parsed as the wire does."
  (aob-tests--feed s (json-serialize (list :jsonrpc "2.0" :id id
                                           :method method :params params))))

(defun aob-tests--goto (text)
  "Redraw the trace and put point on the line that shows TEXT."
  (aob-trace--render t)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(defun aob-tests--replies (sent)
  "The replies in SENT, oldest first, as the JSON that went out."
  (mapcar #'json-serialize
          (reverse (seq-filter (lambda (m) (plist-member m :result)) sent))))

(defconst aob-tests--claude-plan-options
  [(:optionId "exit-plan-clear-accept-edits"
    :name "Yes, clear context (12% used) and auto-accept edits" :kind "allow_always")
   (:optionId "exit-plan-accept-edits" :name "Yes, auto-accept edits" :kind "allow_always")
   (:optionId "exit-plan-default" :name "Yes, manually approve edits" :kind "allow_once")
   (:optionId "reject" :name "No, keep planning" :kind "reject_once")]
  "The options claude-agent-acp offers on ExitPlanMode with a plan, in its order.")

(defun aob-tests--claude-plan (s id)
  "Feed S the permission request claude-agent-acp sends for ExitPlanMode."
  (let ((plan "# Plan\n\n1. Add the flag\n2. Test it"))
    (aob-tests--request
     s id "session/request_permission"
     (list :sessionId "sess-test"
           :toolCall (list :toolCallId "toolu_plan" :name "ExitPlanMode"
                           :status "pending" :rawInput (list :plan plan)
                           :title "Approve Plan" :kind "switch_mode"
                           :content (vector (list :type "content"
                                                  :content (list :type "text" :text plan)))
                           :locations [])
           :_meta (list :permission (list :version 1 :title "Ready to code?"))
           :options aob-tests--claude-plan-options))))

(ert-deftest aob-initialize-declares-form-elicitation-as-an-object ()
  (let ((json (json-serialize (aob-acp--client-capabilities))))
    (should (string-match-p "\"elicitation\":{\"form\":{}}" json))
    (should (equal json (concat "{\"fs\":{\"readTextFile\":false,\"writeTextFile\":false},"
                                "\"elicitation\":{\"form\":{}},"
                                "\"_meta\":{\"subagent-transcript\":true}}")))))

(ert-deftest aob-trace-answers-claude-ask-user-question ()
  (aob-tests--with-trace-session s
    (aob-tests--request
     s 21 "elicitation/create"
     '(:mode "form" :sessionId "sess-test" :toolCallId "toolu_q"
       :message "Which auth method?"
       :requestedSchema
       (:type "object"
        :properties
        (:question_0 (:type "string" :title "Auth"
                      :oneOf [(:const "OAuth" :title "OAuth" :description "Browser flow")
                              (:const "API key" :title "API key")])
         :question_0_custom (:type "string" :title "Other"
                             :description "Type your own answer, or add a note to the option you chose above (optional)."
                             :_meta (:_askUserQuestionCustomAnswer
                                     (:questionId "question_0" :isCustomAnswer t)))))))
    (let ((d (car (aob-session-decisions s))))
      (should (equal (mapcar (lambda (q) (plist-get q :custom)) (plist-get d :questions))
                     '("question_0_custom")))
      (aob-tests--goto "Auth · Which auth method?")
      (should (get-text-property (point) 'aob-question))
      (should-not (string-match-p "Other" (buffer-string)))
      (aob-tests--goto "◦ API key")
      (aob-trace-answer)
      (aob-tests--goto "◦ OAuth")
      (aob-trace-answer)
      (should (string-match-p "◦ OAuth  ✓" (buffer-string)))
      (should-not (string-match-p "◦ API key  ✓" (buffer-string)))
      (aob-trace--add-comment s (plist-get d :seq) "Which auth method?" "keep the refresh token")
      (aob-tests--capturing sent
        (aob-trace-send)
        (should (equal (aob-tests--replies sent)
                       (list (concat "{\"jsonrpc\":\"2.0\",\"id\":21,\"result\":{\"action\":\"accept\","
                                     "\"content\":{\"question_0\":\"OAuth\","
                                     "\"question_0_custom\":\"keep the refresh token\"}}}")))))
      (should-not (aob-session-decisions s))
      (should (eq (aob-session-state s) 'working))
      (should-not (aob-session-ref s :comments))
      (aob-trace--render t)
      (should (string-match-p "→ OAuth, keep the refresh token" (buffer-string))))))

(ert-deftest aob-trace-answers-claude-multi-select-questions ()
  (aob-tests--with-trace-session s
    (aob-tests--request
     s 22 "elicitation/create"
     '(:mode "form" :sessionId "sess-test"
       :message "Please answer the following questions."
       :requestedSchema
       (:type "object"
        :properties
        (:question_0 (:type "array" :title "Parts" :description "Which parts change?"
                      :items (:anyOf [(:const "api" :title "api")
                                      (:const "ui" :title "ui")]))
         :question_0_custom (:type "string" :title "Other"
                             :_meta (:_askUserQuestionCustomAnswer (:questionId "question_0")))
         :question_1 (:type "string" :title "When" :description "Ship when?"
                      :oneOf [(:const "now" :title "now") (:const "later" :title "later")])
         :question_1_custom (:type "string" :title "Other"
                             :_meta (:_askUserQuestionCustomAnswer (:questionId "question_1")))))))
    (aob-tests--goto "◦ api")
    (aob-trace-answer)
    (aob-tests--goto "◦ ui")
    (aob-trace-answer)
    (aob-tests--goto "◦ api")
    (aob-trace-answer)
    (aob-tests--goto "◦ later")
    (aob-trace-answer)
    (aob-tests--capturing sent
      (aob-trace-send)
      (should (equal (aob-tests--replies sent)
                     (list (concat "{\"jsonrpc\":\"2.0\",\"id\":22,\"result\":{\"action\":\"accept\","
                                   "\"content\":{\"question_0\":[\"ui\"],\"question_1\":\"later\"}}}")))))))

(ert-deftest aob-trace-answers-codex-user-input-and-declines ()
  (aob-tests--with-trace-session s
    (let ((params
           '(:sessionId "sess-test" :toolCallId "item_7" :mode "form"
             :message "Input requested"
             :requestedSchema
             (:type "object"
              :properties
              (:env (:title "Env" :description "Which environment?"
                     :_meta (:codex (:isOther t :isSecret :false))
                     :type "string"
                     :oneOf [(:const "prod" :title "prod" :description "live")
                             (:const "staging" :title "staging")])
               :env__other (:type "string" :title "Other"
                            :description "Type your own answer instead of choosing an option above."
                            :_meta (:codex (:questionId "env" :isOtherAnswer t :isSecret :false)))
               :name (:title "Name" :description "What should it be called?"
                      :_meta (:codex (:isOther :false :isSecret :false))
                      :type "string"))
              :required ["name"])
             :_meta (:codex (:autoResolutionMs :null)))))
      (aob-tests--request s 41 "elicitation/create" params)
      (let* ((d (car (aob-session-decisions s)))
             (qs (plist-get d :questions)))
        (should (equal (mapcar (lambda (q) (plist-get q :key)) qs) '("env" "name")))
        (should (equal (plist-get (car qs) :custom) "env__other"))
        (should-not (plist-get (cadr qs) :custom))
        (aob-trace--add-comment s (plist-get d :seq) "Which environment?" "qa")
        (aob-trace--add-comment s (plist-get d :seq) "What should it be called?" "widget"))
      (aob-tests--capturing sent
        (aob-trace-send)
        (should (equal (aob-tests--replies sent)
                       (list (concat "{\"jsonrpc\":\"2.0\",\"id\":41,\"result\":{\"action\":\"accept\","
                                     "\"content\":{\"env__other\":\"qa\",\"name\":\"widget\"}}}")))))
      (aob-tests--request s 42 "elicitation/create" params)
      (aob-tests--capturing sent
        (aob-trace-decline)
        (should (equal (aob-tests--replies sent)
                       '("{\"jsonrpc\":\"2.0\",\"id\":42,\"result\":{\"action\":\"decline\"}}"))))
      (should-not (aob-session-decisions s))
      (aob-trace--render t)
      (should (string-match-p "→ declined" (buffer-string))))))

(ert-deftest aob-trace-approves-claude-plan-the-least-lasting-way ()
  (aob-tests--with-trace-session s
    (aob-tests--claude-plan s 31)
    (let ((d (car (aob-session-decisions s))))
      (should (eq (plist-get d :kind) 'plan))
      (should (equal (plist-get d :plan) "# Plan\n\n1. Add the flag\n2. Test it")))
    (aob-tests--goto "Add the flag")
    (should (get-text-property (point) 'aob-event))
    (should (string-match-p "▸ Yes, auto-accept edits" (buffer-string)))
    (should (string-match-p "ZZ Yes, manually approve edits" (buffer-string)))
    (aob-tests--capturing sent
      (aob-trace-send)
      (should (equal (aob-tests--replies sent)
                     '("{\"jsonrpc\":\"2.0\",\"id\":31,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"exit-plan-default\"}}}"))))
    (should (eq (aob-session-state s) 'working))
    (aob-trace--render t)
    (should (string-match-p "→ Yes, manually approve edits" (buffer-string)))
    (aob-tests--claude-plan s 32)
    (aob-tests--goto "▸ Yes, auto-accept edits")
    (aob-tests--capturing sent
      (aob-trace-answer)
      (should (equal (aob-tests--replies sent)
                     '("{\"jsonrpc\":\"2.0\",\"id\":32,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"exit-plan-accept-edits\"}}}"))))))

(ert-deftest aob-trace-sends-plan-comments-back-as-a-revision ()
  (aob-tests--with-trace-session s
    (aob-tests--claude-plan s 33)
    (let ((seq (plist-get (car (aob-session-decisions s)) :seq)))
      (aob-trace--add-comment s seq "Test it" "cover the error path too"))
    (aob-tests--capturing sent
      (aob-trace-send)
      (should (equal (aob-tests--replies sent)
                     '("{\"jsonrpc\":\"2.0\",\"id\":33,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"reject\"}}}"))))
    (should-not (aob-session-ref s :comments))
    (should (string-match-p "cover the error path too" (car (car (aob-session-ref s :queued)))))
    (should (string-match-p "Test it" (car (car (aob-session-ref s :queued)))))
    (aob-tests--claude-plan s 34)
    (aob-tests--capturing sent
      (aob-trace-decline)
      (should (equal (aob-tests--replies sent)
                     '("{\"jsonrpc\":\"2.0\",\"id\":34,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"reject\"}}}"))))))

(ert-deftest aob-trace-approves-codex-plan ()
  (aob-tests--with-trace-session s
    (aob-tests--request
     s 51 "session/request_permission"
     '(:sessionId "sess-test"
       :toolCall (:toolCallId "plan-review:item_1" :title "Implement this plan?"
                  :kind "switch_mode" :status "pending"
                  :rawInput (:plan "- step one\n- step two"))
       :options [(:optionId "implement_plan" :name "Yes, implement this plan" :kind "allow_once")
                 (:optionId "revise_plan" :name "No, and tell Codex what to do differently"
                  :kind "reject_once")]
       :_meta (:codex (:kind "plan_review" :planItemId "item_1"))))
    (aob-tests--goto "step two")
    (aob-tests--capturing sent
      (aob-trace-send)
      (should (equal (aob-tests--replies sent)
                     '("{\"jsonrpc\":\"2.0\",\"id\":51,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"implement_plan\"}}}"))))))

(defun aob-tests--update (s u)
  "Feed S the session update U, serialised as the wire does."
  (aob-tests--feed s (json-serialize (list :jsonrpc "2.0" :method "session/update"
                                           :params (list :sessionId "sess-test" :update u)))))

(defun aob-tests--tool-seq (s id)
  "The seq of S's tool call ID."
  (plist-get (seq-find (lambda (e) (equal (plist-get e :tool-id) id))
                       (aob-session-events s))
             :seq))

(defun aob-tests--block-text (needle)
  "The text of the event drawn where NEEDLE is."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (let ((seq (get-text-property (point) 'aob-event)))
      (buffer-substring-no-properties
       (text-property-any (point-min) (point-max) 'aob-event seq)
       (text-property-not-all (point) (point-max) 'aob-event seq)))))

(defun aob-tests--top (win)
  "The text WIN starts with, and the text its cursor stands on."
  (with-current-buffer (window-buffer win)
    (list (buffer-substring-no-properties (window-start win) (+ (window-start win) 30))
          (buffer-substring-no-properties (window-point win) (+ (window-point win) 30)))))

(ert-deftest aob-trace-held-window-keeps-its-top ()
  "A window read higher up starts on the same text however the trace moves.
Blocks shed off the top, a task above whose rollup ticks with every
subagent step, output with a table and a picture landing above, and
every table drawn again for a new width: none of it moves the page."
  (aob-tests--with-trace-session s
    (let ((aob-trace-limit 55)
          (win (selected-window))
          (table (concat "| name | value | note |\n|---|---|---|\n"
                         "| alpha | 1 | a note long enough to be squeezed by a narrower window |\n"
                         "| beta | 2 | short |")))
      (delete-other-windows)
      (set-window-buffer win (current-buffer))
      (dotimes (i 14)
        (when (= i 4)
          (aob-tests--update s '(:sessionUpdate "tool_call" :toolCallId "build" :title "make all"
                                                :kind "execute" :status "in_progress")))
        (when (= i 6)
          (aob-tests--update s '(:sessionUpdate "tool_call" :toolCallId "task" :title "Task"
                                                :kind "think" :status "in_progress")))
        (aob-tests--update s (list :sessionUpdate "agent_message_chunk"
                                   :content (list :type "text"
                                                  :text (format "Step %d reads:\n\n%s\n" i table))))
        (aob-tests--update s (list :sessionUpdate "tool_call" :toolCallId (format "r%d" i)
                                   :title (format "read file%d.el" i) :kind "execute"
                                   :status "completed")))
      (aob-trace--render-1 s)
      (push (aob-tests--tool-seq s "build") aob-trace--expanded)
      (aob-trace--render-1 s)
      (goto-char (point-min))
      (search-forward "Step 9 reads")
      (beginning-of-line)
      (set-window-start win (point))
      (set-window-point win (point))
      (let ((top (aob-tests--top win))
            (first (get-text-property (point-min) 'aob-event))
            (drawn nil))
        (dotimes (i 24)
          (aob-tests--update s (list :sessionUpdate "agent_message_chunk"
                                     :content (list :type "text" :text (format "more %d " i))))
          (when (cl-oddp i)
            (aob-tests--update s (list :sessionUpdate "tool_call" :toolCallId (format "c%d" i)
                                       :title (format "child %d" i) :kind "read" :status "completed"
                                       :_meta '(:claudeCode (:parentToolUseId "task")))))
          (aob-tests--update s (list :sessionUpdate "usage_update" :used (* 1000 i) :size 200000))
          (when (= i 8)
            (aob-tests--update
             s (list :sessionUpdate "tool_call_update" :toolCallId "build" :status "completed"
                     :content (vector (list :type "content"
                                            :content (list :type "text"
                                                           :text (concat table "\n![shot](/tmp/shot.png)")))))))
          (when (zerop (% i 6))
            (aob-tests--update s (list :sessionUpdate "tool_call" :toolCallId (format "n%d" i)
                                       :title (format "grep %d" i) :kind "execute"
                                       :status "completed")))
          (when (= i 16)
            (setq drawn (aob-tests--block-text "Step 8 reads"))
            (split-window-right)
            (aob-trace--fit-margins))
          (aob-trace--render-1 s)
          (when (= i 16)
            (should-not (equal (aob-tests--block-text "Step 8 reads") drawn)))
          (should (equal (aob-tests--top win) top)))
        (should-not (eql (get-text-property (point-min) 'aob-event) first))
        (should (< (save-excursion (goto-char (point-min)) (search-forward "shot.png"))
                   (window-start win)))
        (should (< (text-property-any (point-min) (point-max) 'aob-event
                                      (aob-tests--tool-seq s "task"))
                   (window-start win)))))))

(defun aob-tests--tool (s id kind title status &rest more)
  "Feed S a tool call ID of KIND titled TITLE in STATUS, with MORE fields."
  (aob-tests--update s (append (list :sessionUpdate "tool_call" :toolCallId id
                                     :title title :kind kind :status status)
                               more)))

(defun aob-tests--say (s text)
  "Feed S an answer chunk of TEXT."
  (aob-tests--update s (list :sessionUpdate "agent_message_chunk"
                             :content (list :type "text" :text text))))

(defun aob-tests--think (s text)
  "Feed S a thought chunk of TEXT."
  (aob-tests--update s (list :sessionUpdate "agent_thought_chunk"
                             :content (list :type "text" :text text))))

(defun aob-tests--output (text)
  "TEXT as the content of a tool update."
  (vector (list :type "content" :content (list :type "text" :text text))))

(defun aob-tests--at (needle prop)
  "PROP where NEEDLE first appears in the buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (get-text-property (match-beginning 0) prop)))

(defun aob-tests--seven-tools (s)
  "Feed S seven finished tool calls in a row: three reads, two edits, two runs."
  (dolist (f '("a" "b" "c"))
    (aob-tests--tool s (concat "r" f) "read" (format "Read /tmp/proj/%s.el" f) "completed"))
  (aob-tests--tool s "e1" "edit" "Edit /tmp/proj/a.el" "completed")
  (aob-tests--tool s "e2" "edit" "Edit /tmp/proj/b.el" "completed")
  (aob-tests--tool s "x1" "execute" "`make`" "completed")
  (aob-tests--tool s "x2" "execute" "`make test`" "completed"))

(ert-deftest aob-trace-folds-a-run-of-tool-calls ()
  "Seven calls with no words between them are one step of work: one row
that counts them by kind, TAB opening it back into the calls."
  (aob-tests--with-trace-session s
    (aob-tests--say s "Looking.")
    (aob-tests--seven-tools s)
    (aob-trace--render t)
    (should (string-match-p "▸ 7 tools · 3 read, 2 edit, 2 shell" (buffer-string)))
    (should-not (string-match-p "make test" (buffer-string)))
    (aob-tests--goto "7 tools")
    (aob-trace-toggle)
    (should (string-match-p "▾ 7 tools" (buffer-string)))
    (should (string-match-p "\\$ make test" (buffer-string)))
    (should (eql (aob-tests--at "make test" 'aob-item) (aob-tests--tool-seq s "x2")))
    (aob-tests--goto "7 tools")
    (aob-trace-toggle)
    (should-not (string-match-p "make test" (buffer-string)))
    (let ((aob-trace-run-min 0))
      (aob-trace--render t)
      (should-not (string-match-p "7 tools" (buffer-string))))))

(ert-deftest aob-trace-a-run-shows-its-live-call-and-its-failures ()
  "Shut, a run still shows the call it is waiting on and every call that
failed, and its row says it failed."
  (aob-tests--with-trace-session s
    (aob-tests--seven-tools s)
    (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "x1" :status "failed"))
    (aob-tests--tool s "live" "execute" "`sleep 9`" "in_progress")
    (aob-trace--render t)
    (should (string-match-p "▸ 8 tools .* · 1 failed" (buffer-string)))
    (should (string-match-p "\\$ make failed" (buffer-string)))
    (should (string-match-p "\\$ sleep 9" (buffer-string)))
    (should-not (string-match-p "make test" (buffer-string)))
    (let ((mark (text-property-any (point-min) (point-max) 'aob-status 'failed)))
      (should (eql (get-text-property mark 'aob-run) (aob-tests--tool-seq s "ra"))))
    (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "live" :status "completed"))
    (aob-trace--render t)
    (should-not (string-match-p "sleep 9" (buffer-string)))))

(ert-deftest aob-trace-marks-each-block-in-the-fringe ()
  "The fringe says running, done, failed or waiting, and the character
carrying the mark changes with it, so a state change redraws the block."
  (aob-tests--with-trace-session s
    (aob-tests--tool s "t1" "read" "Read /tmp/proj/a.el" "in_progress")
    (aob-trace--render t)
    (let ((at (text-property-any (point-min) (point-max) 'aob-status 'running)))
      (should at)
      (should (equal (get-text-property at 'display)
                     '(left-fringe aob-trace-running aob-trace-status)))
      (should (eql (get-text-property at 'aob-event) (aob-tests--tool-seq s "t1"))))
    (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "t1" :status "failed"))
    (aob-trace--render t)
    (let ((at (text-property-any (point-min) (point-max) 'aob-status 'failed)))
      (should (equal (get-text-property at 'display)
                     '(left-fringe aob-trace-failed aob-trace-status-failed)))
      (should (equal (buffer-substring-no-properties at (1+ at)) "!")))
    (aob-tests--request s 60 "session/request_permission"
                        '(:sessionId "sess-test"
                          :toolCall (:toolCallId "t2" :title "rm -rf build" :kind "execute")
                          :options [(:optionId "ok" :name "Allow" :kind "allow_once")]))
    (aob-trace--render t)
    (should (text-property-any (point-min) (point-max) 'aob-status 'waiting))
    (aob-tests--claude-plan s 61)
    (aob-trace--render t)
    (let* ((seq (plist-get (car (aob-session-decisions s)) :seq))
           (at (text-property-any (point-min) (point-max) 'aob-event seq)))
      (should (eq (get-text-property at 'aob-status) 'waiting)))
    (let ((aob-trace-status-gutter nil))
      (dolist (ev (aob-session-events s)) (plist-put ev :line nil))
      (aob-trace--render t)
      (should-not (text-property-not-all (point-min) (point-max) 'aob-status nil)))))

(ert-deftest aob-trace-folds-thinking-to-one-line ()
  "A thought is one muted line saying how long it is, even while it is
being had; TAB opens it, and it stays open through every redraw."
  (aob-tests--with-trace-session s
    (aob-tests--think s "first I look\n\nthen I")
    (aob-tests--think s " decide\nand act")
    (aob-trace--render t)
    (should (string-match-p "thinking · 3 lines" (buffer-string)))
    (should-not (string-match-p "then I decide" (buffer-string)))
    (should (memq 'aob-trace-thinking
                  (ensure-list (aob-tests--at "thinking ·" 'font-lock-face))))
    (aob-tests--say s "Done.")
    (aob-tests--goto "thinking ·")
    (aob-trace-toggle)
    (should (string-match-p "then I decide" (buffer-string)))
    (aob-tests--say s " More.")
    (aob-trace--render t)
    (aob-trace--render t)
    (should (string-match-p "then I decide" (buffer-string)))
    (aob-tests--goto "thinking ·")
    (aob-trace-toggle)
    (should-not (string-match-p "then I decide" (buffer-string)))))

(ert-deftest aob-trace-draws-an-edit-as-a-diff ()
  "An edit is its file, what it added and removed, and the change with
the words that moved marked, cut short until TAB; RET on the file line
opens it where the change is."
  (let ((file (make-temp-file "aob-diff" nil ".el"
                              "(defun a ()\n  (one)\n  (new thing here))\n")))
    (unwind-protect
        (aob-tests--with-trace-session s
          (aob-tests--tool s "e1" "edit" (concat "Edit " file) "completed"
                           :content (vector (list :type "diff" :path file
                                                  :oldText "(defun a ()\n  (one)\n  (old thing))"
                                                  :newText "(defun a ()\n  (one)\n  (new thing here))")))
          (aob-trace--render t)
          (let ((name (file-name-nondirectory file)))
            (should (string-match-p (concat (regexp-quote name) "  \\+1 −1") (buffer-string))))
          (should (string-match-p "^− +(old thing))" (buffer-string)))
          (should (string-match-p "^\\+ +(new thing here))" (buffer-string)))
          (should (memq 'aob-trace-diff-refine-removed
                        (ensure-list (aob-tests--at "old" 'font-lock-face))))
          (should (memq 'aob-trace-diff-refine-added
                        (ensure-list (aob-tests--at "here" 'font-lock-face))))
          (should-not (memq 'aob-trace-diff-refine-added
                            (ensure-list (aob-tests--at "thing here" 'font-lock-face))))
          (aob-tests--goto "+1 −1")
          (save-window-excursion
            (aob-trace-answer)
            (should (equal (buffer-file-name) file))
            (should (= (line-number-at-pos) 3)))
          (let ((aob-trace-diff-lines 2))
            (dolist (ev (aob-session-events s)) (plist-put ev :line nil))
            (aob-trace--render t)
            (should (string-match-p "… 2 more lines" (buffer-string)))
            (aob-tests--goto "+1 −1")
            (aob-trace-toggle)
            (should-not (string-match-p "more lines" (buffer-string)))
            (should (string-match-p "new thing here" (buffer-string)))))
      (when-let* ((b (get-file-buffer file))) (kill-buffer b))
      (delete-file file))))

(ert-deftest aob-trace-draws-a-command-as-a-card ()
  "A command run is its command, its output cut to a few lines, how it
ended on the right, and a file:line in the output RET opens."
  (let ((file (make-temp-file "aob-card" nil ".el" "one\ntwo\nthree\n")))
    (unwind-protect
        (aob-tests--with-trace-session s
          (aob-tests--tool s "x1" "execute" "`make test`" "in_progress")
          (aob-trace--render t)
          (should (string-match-p "\\$ make test running" (buffer-string)))
          (aob-tests--update
           s (list :sessionUpdate "tool_call_update" :toolCallId "x1" :status "failed"
                   :rawOutput '(:exit_code 2)
                   :content (aob-tests--output
                             (concat "```console\n" file ":2: error: boom\n"
                                     (mapconcat (lambda (i) (format "line %d" i))
                                                (number-sequence 1 20) "\n")
                                     "\n```"))))
          (aob-trace--render t)
          (should (string-match-p "\\$ make test exit 2 · [0-9]+ms" (buffer-string)))
          (should-not (string-match-p "```" (buffer-string)))
          (should (string-match-p "line 11\n  … 9 more lines" (buffer-string)))
          (should-not (string-match-p "line 12" (buffer-string)))
          (should (equal (aob-tests--at (concat file ":2") 'aob-file) (list file 2 nil)))
          (goto-char (point-min))
          (search-forward (concat file ":2"))
          (goto-char (match-beginning 0))
          (save-window-excursion
            (aob-trace-answer)
            (should (equal (buffer-file-name) file))
            (should (= (line-number-at-pos) 2)))
          (with-current-buffer (aob-trace-buffer s)
            (aob-tests--goto "$ make test")
            (aob-trace-toggle)
            (should (string-match-p "line 20" (buffer-string)))
            (should-not (string-match-p "more lines" (buffer-string)))))
      (when-let* ((b (get-file-buffer file))) (kill-buffer b))
      (delete-file file))))

(ert-deftest aob-trace-shows-files-as-chips ()
  "A path a tool acted on is its name, and its line; the whole path,
relative to the project, is under the pointer, and RET opens it."
  (let* ((dir (file-name-as-directory (make-temp-file "aob-proj" t)))
         (file (expand-file-name "src/a.el" dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert "1\n2\n3\n4\n"))
    (unwind-protect
        (aob-tests--with-trace-session s
          (setf (aob-session-project s) dir)
          (aob-tests--tool s "r1" "read" (concat "Read File  " file) "completed"
                           :locations (vector (list :path file :line 3)))
          (aob-trace--render t)
          (should (string-match-p "Read a.el:3" (buffer-string)))
          (should-not (string-match-p (regexp-quote file) (buffer-string)))
          (should (equal (aob-tests--at "a.el:3" 'help-echo) "src/a.el"))
          (should (memq 'aob-trace-target (ensure-list (aob-tests--at "a.el:3" 'font-lock-face))))
          (aob-tests--goto "a.el:3")
          (save-window-excursion
            (aob-trace-answer)
            (should (equal (buffer-file-name) file))
            (should (= (line-number-at-pos) 3))))
      (when-let* ((b (get-file-buffer file))) (kill-buffer b))
      (delete-directory dir t))))

(defun aob-tests--breaks (beg end)
  "Where between BEG and END a space is shown as a line break."
  (let (out)
    (dotimes (i (- end beg))
      (when (equal (get-text-property (+ beg i) 'display) "\n")
        (push (+ beg i) out)))
    (nreverse out)))

(ert-deftest aob-trace-wraps-prose-at-its-measure ()
  "Prose breaks at the measure however wide the window, without a newline
in the text; a table keeps its width, and a window too narrow for the
measure wraps at its own edge."
  (aob-tests--with-trace-session s
    (let ((aob-trace-prose-width 30)
          (aob-trace-card-width 120)
          (words (mapconcat (lambda (i) (format "word%02d" i)) (number-sequence 1 30) " ")))
      (aob-tests--say s (concat words "\n\n| a | b |\n|---|---|\n| "
                                (make-string 50 ?x) " | y |"))
      (aob-trace--render t)
      (let* ((beg (save-excursion (goto-char (point-min)) (search-forward "word01")
                                  (match-beginning 0)))
             (end (save-excursion (goto-char beg) (line-end-position)))
             (breaks (aob-tests--breaks beg end)))
        (should (> (length breaks) 3))
        (should (= (count-lines beg end) 1))
        (let ((prev beg))
          (dolist (b (append breaks (list end)))
            (should (<= (- b prev) 31))
            (setq prev (1+ b)))))
      (should-not (aob-tests--breaks (save-excursion (goto-char (point-min))
                                                     (search-forward "xxxx") (point))
                                     (save-excursion (goto-char (point-min))
                                                     (search-forward "xxxx") (line-end-position))))
      (let ((aob-trace-card-width 32))
        (aob-trace--fit-margins)
        (aob-trace--render t)
        (should-not (aob-tests--breaks (point-min) (point-max)))))))

(ert-deftest aob-trace-spaces-words-apart-and-work-close ()
  "Before words a line of air, between calls a little, inside a card none,
in either style."
  (dolist (style '(delta log))
    (let ((aob-trace-style style))
      (aob-tests--with-trace-session s
        (aob-tests--say s "Plan.")
        (aob-tests--tool s "x1" "execute" "`make`" "completed"
                         :content (aob-tests--output "a\nb"))
        (aob-tests--tool s "r1" "read" "Read /tmp/proj/a.el" "completed")
        (aob-tests--say s "Now the words.")
        (aob-trace--render t)
        (let ((above-block (lambda (needle)
                             (let ((seq (aob-tests--at needle 'aob-event)))
                               (get-text-property
                                (1- (text-property-any (point-min) (point-max) 'aob-event seq))
                                'line-spacing))))
              (above-line (lambda (needle)
                            (save-excursion
                              (goto-char (point-min))
                              (search-forward needle)
                              (goto-char (match-beginning 0))
                              (forward-line 0)
                              (get-text-property (1- (point)) 'line-spacing)))))
          (should (equal (funcall above-block "Now the words") aob-trace-gap-words))
          (should (equal (funcall above-block "a.el") aob-trace-gap-work))
          (should (equal (funcall above-line "  a\n") 0))
          (should (equal (funcall above-line "  b") 0)))
        (should-not (string-match-p "\n\n" (buffer-substring-no-properties
                                             (point-min) (aob-trace--tail-end))))))))

(ert-deftest aob-trace-sets-its-type-scale ()
  "Prose a little larger, tool rows a little smaller, headings bold at
the size of the words around them; a table stays at the size it was laid
out in."
  (aob-tests--with-trace-session s
    (should (member '(:height 1.05) (alist-get 'aob-trace-prose face-remapping-alist)))
    (should (member '(:height 0.92) (alist-get 'aob-trace-small face-remapping-alist)))
    (aob-tests--say s "Words here.\n\n| a | b |\n|---|---|\n| 1 | 2 |")
    (aob-tests--tool s "r1" "read" "Read /tmp/proj/a.el" "completed")
    (aob-trace--render t)
    (should (memq 'aob-trace-prose (ensure-list (aob-tests--at "Words" 'font-lock-face))))
    (should (string-match-p "│ a " (buffer-string)))
    (should-not (memq 'aob-trace-prose (ensure-list (aob-tests--at "│ a " 'font-lock-face))))
    (should (memq 'aob-trace-small (ensure-list (aob-tests--at "Read" 'font-lock-face))))
    (should-not (memq 'aob-trace-prose (ensure-list (aob-tests--at "Read" 'font-lock-face))))))

(ert-deftest aob-trace-header-says-only-what-is-looked-at ()
  "Name, state, clock, cost, context and todo; a mode only when it is not
the one a session runs in anyway; no folder, model or token count out."
  (aob-tests--with-trace-session s
    (aob-session-put s :mode-id "default")
    (aob-session-put s :model-name "opus")
    (aob-session-put s :ctx-used 213400)
    (aob-trace--render t)
    (should (string-match-p "\\` test:1 · working · .*213k ctx\\'" header-line-format))
    (should-not (string-match-p "default\\|opus\\|/tmp/proj" header-line-format))
    (should (eq 'warning (get-text-property (string-search "213k" header-line-format)
                                            'face header-line-format)))
    (aob-session-put s :mode-id "plan")
    (aob-session-put s :ctx-used 1000)
    (aob-trace--render t)
    (should (string-match-p " · working · plan · " header-line-format))
    (should (eq 'shadow (get-text-property (string-search "1k" header-line-format)
                                           'face header-line-format)))))

(ert-deftest aob-trace-a-shut-run-shows-every-call-still-running ()
  "Parallel calls finish in any order: a run is running while any call
is, and each still running is shown under its row."
  (aob-tests--with-trace-session s
    (dotimes (i 6)
      (aob-tests--tool s (format "p%d" i) "execute" (format "`job %d`" i) "in_progress"))
    (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "p5" :status "completed"))
    (aob-trace--render t)
    (should (string-match-p "▸ 6 tools" (buffer-string)))
    (should (= 5 (how-many "\\$ job [0-4]" (point-min) (point-max))))
    (should-not (string-match-p "job 5" (buffer-string)))
    (should (eq 'running (get-text-property
                          (text-property-any (point-min) (point-max) 'aob-run
                                             (aob-tests--tool-seq s "p0"))
                          'aob-status)))))

(ert-deftest aob-trace-a-call-you-opened-stays-when-a-run-forms ()
  "A card opened by TAB and read in a window stays shown and stays put
when the calls around it fold into a run."
  (aob-tests--with-trace-session s
    (let ((win (selected-window)))
      (delete-other-windows)
      (set-window-buffer win (current-buffer))
      (dotimes (i 4)
        (aob-tests--tool s (format "q%d" i) "execute" (format "`step %d`" i) "completed"
                         :content (aob-tests--output
                                   (mapconcat (lambda (k) (format "out %d.%d" i k))
                                              (number-sequence 1 30) "\n"))))
      (push (aob-tests--tool-seq s "q2") aob-trace--expanded)
      (aob-trace--render-1 s)
      (goto-char (point-min))
      (search-forward "out 2.20")
      (beginning-of-line)
      (set-window-start win (point))
      (aob-tests--tool s "q4" "execute" "`step 4`" "completed")
      (aob-trace--render-1 s)
      (should (string-match-p "▸ 5 tools" (buffer-string)))
      (should (string-match-p "out 2.30" (buffer-string)))
      (should (string-match-p "\\`[ ]*out 2\\.20$"
                              (save-excursion
                                (goto-char (window-start win))
                                (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position))))))))

(ert-deftest aob-trace-a-streamed-answer-breaks-where-it-was-drawn ()
  "A chunk that pushes a line past the measure breaks it at a space drawn
by an earlier chunk, not only in the words that just arrived."
  (aob-tests--with-trace-session s
    (let ((aob-trace-prose-width 30)
          (aob-trace-card-width 120))
      (aob-tests--say s "aaaa bbbb cccc dddd eeee ff")
      (aob-trace--render t)
      (should-not (aob-tests--breaks (point-min) (point-max)))
      (aob-tests--say s "ffff gggg")
      (aob-trace--render t)
      (let ((breaks (aob-tests--breaks (point-min) (point-max))))
        (should breaks)
        (should (< (car breaks)
                   (save-excursion (goto-char (point-min)) (search-forward "ff") (point))))))))

(defconst aob-tests--limit
  "You've hit your org's monthly spend limit · run /usage-credits to raise it"
  "What a subagent that ran out of money reports.")

(defun aob-tests--reports (s ids)
  "S hears, from the editor, that each subagent in IDS finished out of money."
  (dolist (n ids)
    (aob-event s 'prompt :typed nil
               :text (format "Subagent claude:%d (acp:claude:%d) finished:\n\n%s"
                             n n aob-tests--limit))))

(ert-deftest aob-trace-folds-what-the-editor-said-for-you ()
  "What the editor sent for you is one quiet row each; notes that say the
same thing are one row counting them; TAB opens them; what you typed is
left as you wrote it."
  (aob-tests--with-trace-session s
    (aob-event s 'prompt :text "Fix the parser." :typed t)
    (aob-tests--reports s '(10 8 11))
    (aob-event s 'prompt :typed nil :text "Todo: 3 items pending.\n\nkeep going")
    (aob-trace--render t)
    (should (string-match-p "^Fix the parser\\.$" (buffer-string)))
    (should (string-match-p "▸ 3 subagents finished · You've hit your org's monthly spend limit.* ×3"
                            (buffer-string)))
    (should (= 1 (how-many "spend limit" (point-min) (point-max))))
    (should (string-match-p "▸ Todo: 3 items pending\\.$" (buffer-string)))
    (should (memq 'aob-trace-note (ensure-list (aob-tests--at "3 subagents" 'font-lock-face))))
    (aob-tests--goto "3 subagents")
    (aob-trace-toggle)
    (should (string-match-p "▾ 3 subagents" (buffer-string)))
    (should (= 4 (how-many "spend limit" (point-min) (point-max))))
    (aob-tests--goto "claude:8 (acp")
    (aob-trace-toggle)
    (should-not (string-match-p "claude:8 (acp" (buffer-string)))
    (aob-tests--goto "Todo: 3")
    (aob-trace-toggle)
    (should (string-match-p "keep going" (buffer-string)))))

(ert-deftest aob-trace-folds-queued-notes-and-keeps-them-movable ()
  "Queued notes that say the same thing fold into one row that still says
they wait; the queue under it counts each."
  (aob-tests--with-trace-session s
    (let ((aob-prompt-typed nil))
      (dolist (n '(5 7))
        (aob-acp--queue s (format "Subagent claude:%d (acp:claude:%d) finished:\n\n%s"
                                  n n aob-tests--limit)
                        nil)))
    (aob-trace--render t)
    (should (string-match-p "▸ 2 subagents finished · .* ×2 ⋯" (buffer-string)))
    (should (string-match-p "» 2 queued" (buffer-string)))
    (aob-tests--goto "2 subagents")
    (should (equal (car (aob-trace--queued-at-point))
                   (car (car (aob-session-ref s :queued)))))))

(defmacro aob-tests--held (s win &rest body)
  "With S's trace in WIN, read from the line holding Step 9, run BODY and
check after every render in it that WIN starts on the same text."
  (declare (indent 2))
  `(progn
     (delete-other-windows)
     (set-window-buffer ,win (current-buffer))
     (aob-trace--render-1 ,s)
     (goto-char (point-min))
     (search-forward "Step 9 reads")
     (beginning-of-line)
     (set-window-start ,win (point))
     (set-window-point ,win (point))
     (let ((top (aob-tests--top ,win)))
       (cl-flet ((again () (aob-trace--render-1 ,s)
                   (should (equal (aob-tests--top ,win) top))))
         ,@body))))

(defun aob-tests--steps (s from to)
  "Feed S answers FROM to TO, each a few words long."
  (dolist (i (number-sequence from to))
    (aob-tests--say s (format "Step %d reads a few words.\n" i))
    (aob-tests--tool s (format "k%d" i) "read" (format "Read /tmp/proj/k%d.el" i) "completed")))

(ert-deftest aob-trace-held-window-keeps-its-top-through-folds ()
  "Opening and shutting a run of calls and a thought above the page the
window is on leaves the page where it is."
  (aob-tests--with-trace-session s
    (let ((win (selected-window)))
      (aob-tests--think s "one\ntwo\nthree")
      (aob-tests--seven-tools s)
      (aob-tests--reports s '(1 2 3))
      (aob-tests--steps s 1 14)
      (aob-tests--held s win
        (let ((run (aob-tests--tool-seq s "ra"))
              (thought (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'thought))
                                            (aob-session-events s))
                                  :seq))
              (notes (plist-get (car (last (seq-filter #'aob-trace--editor-p
                                                       (aob-session-events s))))
                                :seq)))
          (should (< (text-property-any (point-min) (point-max) 'aob-run run)
                     (window-start win)))
          (dotimes (_ 2)
            (push run aob-trace--open-runs)
            (again)
            (should (string-match-p "▾ 7 tools" (buffer-string)))
            (setq aob-trace--open-runs (delq run aob-trace--open-runs))
            (again)
            (push thought aob-trace--expanded)
            (again)
            (should (string-match-p "three" (buffer-string)))
            (setq aob-trace--expanded (delq thought aob-trace--expanded))
            (again)
            (push notes aob-trace--expanded)
            (again)
            (should (string-match-p "▾ 3 subagents" (buffer-string)))
            (setq aob-trace--expanded (delq notes aob-trace--expanded))
            (again)))))))

(ert-deftest aob-trace-held-window-keeps-its-top-while-a-card-grows ()
  "A command above the page printing as it runs grows its card, and the
page stays where it is."
  (aob-tests--with-trace-session s
    (let ((win (selected-window)))
      (aob-tests--say s "Building.\n")
      (aob-tests--tool s "build" "execute" "`make all`" "in_progress")
      (aob-tests--steps s 1 14)
      (aob-tests--held s win
        (dotimes (i 20)
          (aob-tests--update
           s (list :sessionUpdate "tool_call_update" :toolCallId "build"
                   :status "in_progress"
                   :content (aob-tests--output
                             (mapconcat (lambda (k) (format "compiling unit %d" k))
                                        (number-sequence 0 i) "\n"))))
          (when (= i 10) (push (aob-tests--tool-seq s "build") aob-trace--expanded))
          (again))
        (should (string-match-p "compiling unit 19" (buffer-string)))))))

(ert-deftest aob-trace-held-window-keeps-its-top-while-prose-reflows ()
  "A window made narrower breaks the prose above it at other places, and
the page it shows starts on the same words."
  (aob-tests--with-trace-session s
    (let ((win (selected-window))
          (aob-trace-prose-width 24))
      (aob-tests--say s (concat (mapconcat (lambda (i) (format "longword%02d" i))
                                           (number-sequence 1 40) " ")
                                "\n"))
      (aob-tests--steps s 1 14)
      (aob-tests--held s win
        (let ((before (aob-tests--breaks (point-min) (window-start win))))
          (should before)
          (let ((aob-trace-prose-width 12))
            (dolist (ev (aob-session-events s)) (plist-put ev :line nil))
            (setq aob-trace--blocks nil)
            (again)
            (should-not (equal before (aob-tests--breaks (point-min) (window-start win)))))
          (split-window-right)
          (aob-trace--fit-margins)
          (again))))))

(ert-deftest aob-trace-held-cursor-stays-on-the-page ()
  "A card growing above the cursor pushes it below the page; the render
puts it on the page's last line, or redisplay would scroll the page after it."
  (aob-tests--with-trace-session s
    (let ((win (selected-window))
          (rows 6))
      (delete-other-windows)
      (set-window-buffer win (current-buffer))
      (aob-tests--say s "Building.\n")
      (aob-tests--tool s "build" "execute" "`make all`" "in_progress")
      (aob-tests--steps s 1 14)
      (aob-trace--render-1 s)
      (push (aob-tests--tool-seq s "build") aob-trace--expanded)
      (aob-trace--render-1 s)
      (aob-tests--goto "make all")
      (set-window-start win (line-beginning-position 0))
      (cl-letf (((symbol-function 'pos-visible-in-window-p)
                 (lambda (&optional pos w _partially)
                   (let ((start (window-start w)))
                     (and (>= pos start)
                          (< (count-lines start (save-excursion (goto-char pos) (line-beginning-position)))
                             rows)))))
                ((symbol-function 'move-to-window-line)
                 (lambda (_arg)
                   (goto-char (window-start))
                   (forward-line (1- rows)))))
        (set-window-point win (save-excursion (goto-char (window-start win))
                                              (forward-line (1- rows))
                                              (point)))
        (should (pos-visible-in-window-p (window-point win) win))
        (let ((top (aob-tests--top win)))
          (aob-tests--update
           s (list :sessionUpdate "tool_call_update" :toolCallId "build" :status "in_progress"
                   :content (aob-tests--output
                             (mapconcat (lambda (k) (format "compiling unit %d" k))
                                        (number-sequence 0 20) "\n"))))
          (aob-trace--render-1 s)
          (should (equal (car (aob-tests--top win)) (car top)))
          (should (pos-visible-in-window-p (window-point win) win)))))))

(ert-deftest aob-trace-nudge-up-from-the-tail-stops-following ()
  "A wheel step back from the live edge takes the cursor off it, so the
next chunk does not pull the page back down."
  (require 'mwheel)
  (aob-tests--with-trace-session s
    (let ((win (selected-window)))
      (delete-other-windows)
      (set-window-buffer win (current-buffer))
      (aob-tests--steps s 1 14)
      (aob-trace--render-1 s)
      (goto-char (point-max))
      (set-window-point win (point-max))
      (set-window-start win (save-excursion (forward-line -5) (point)))
      (should (>= (window-point win) (aob-trace--tail-start)))
      (with-selected-window win
        (funcall mwheel-scroll-down-function 1))
      (should (< (window-point win) (aob-trace--tail-start)))
      (let ((start (window-start win)))
        (aob-tests--steps s 15 18)
        (aob-trace--render-1 s)
        (should (< (window-point win) (aob-trace--tail-start)))
        (should (equal (window-start win) start))))))

(defmacro aob-tests--with-comment-trace (var s &rest body)
  "Bind VAR to a buffer shown in the selected window as S's trace."
  (declare (indent 2))
  `(let ((,var (generate-new-buffer "trace")))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer ,var
             (setq default-directory "/tmp/elsewhere/")
             (setq-local aob-trace--session-id (aob-session-id ,s)))
           (set-window-buffer (selected-window) ,var)
           ,@body)
       (kill-buffer ,var)
       (dolist (b (buffer-list))
         (when (string-prefix-p "compose:comment:" (buffer-name b))
           (kill-buffer b))))))

(ert-deftest aob-trace-comment-box-completes-for-its-session ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"available_commands_update\",\"availableCommands\":[{\"name\":\"debug\",\"description\":\"Debug it\"}]}}}")
    (puthash "/tmp/proj/" '("src/auth.ts" "src/main.ts") aob--files-cache)
    (aob-tests--with-comment-trace trace s
      (with-current-buffer (aob-trace--comment-box trace 3 "why?" 1)
        (should (derived-mode-p 'aob-compose-mode))
        (should aob-trace-comment-mode)
        (should aob-compose--anchor)
        (should (equal aob-compose--target "acp:test:1"))
        (should (equal default-directory "/tmp/proj/"))
        (should (equal (seq-take completion-at-point-functions 2)
                       '(aob-compose-capf aob-compose--dabbrev-capf)))
        (should (memq #'aob-compose--autogrow post-command-hook))
        (should (string-prefix-p "comment on: why?" aob-compose--label))
        (insert "/de")
        (pcase-let ((`(,_ ,_ ,table . ,_) (aob-compose-capf)))
          (should (member "debug" table)))
        (erase-buffer)
        (insert "see @src")
        (pcase-let ((`(,_ ,_ ,table . ,_) (aob-compose-capf)))
          (should (member "src/auth.ts" (all-completions "src" table)))))
      (with-current-buffer trace (setq-local aob-trace--session-id nil))
      (with-current-buffer (aob-trace--comment-box trace nil "q" 1 s)
        (should (equal aob-compose--target "acp:test:1"))
        (should (equal default-directory "/tmp/proj/"))))))

(ert-deftest aob-trace-comment-box-grows-within-bounds ()
  (aob-tests--with-session s
    (aob-tests--with-comment-trace trace s
      (with-current-buffer (aob-trace--comment-box trace 1 "q" 1)
        (set-window-buffer (selected-window) (current-buffer))
        (insert "one line")
        (should (= (aob-compose--wanted-height) 3))
        (insert "\n2\n3\n4\n5")
        (should (= (aob-compose--wanted-height) 7))
        (insert (make-string 40 ?\n))
        (should (= (aob-compose--wanted-height)
                   aob-compose-anchored-max-height))
        (erase-buffer)
        (should (= (aob-compose--wanted-height) 3))))))

(ert-deftest aob-trace-comment-box-holds-and-send-now-sends-all ()
  (aob-tests--with-session s
    (aob-tests--with-comment-trace trace s
      (let ((said nil)
            (aob-compose-before-send-functions
             (list (lambda (_) "rewritten as a turn"))))
        (cl-letf (((symbol-function 'aob-trace--say)
                   (lambda (_s text &optional _files) (push text said))))
          (let ((a (aob-trace--comment-box trace 3 "why?" 1)))
            (with-current-buffer a
              (insert "fix this")
              (aob-compose-send))
            (should-not (buffer-live-p a)))
          (should-not said)
          (should (equal (plist-get (car (aob-session-ref s :comments)) :text)
                         "fix this"))
          (with-current-buffer (aob-trace--comment-box trace 4 "and here" 1)
            (insert "and this")
            (aob-trace-comment-send-now))
          (should (= (length said) 1))
          (should (string-match-p "> why\\?\nfix this" (car said)))
          (should (string-match-p "> and here\nand this" (car said)))
          (should-not (aob-session-ref s :comments)))))))

(ert-deftest aob-trace-comment-box-keeps-a-draft-per-anchor ()
  (aob-tests--with-session s
    (aob-tests--with-comment-trace trace s
      (let ((a (aob-trace--comment-box trace 3 "why?" 1)))
        (with-current-buffer a (insert "half a thought"))
        (let ((b (aob-trace--comment-box trace 3 "other words" 1)))
          (should-not (eq a b))
          (should (equal (with-current-buffer b (buffer-string)) "")))
        (should (eq (aob-trace--comment-box trace 3 "why?" 1) a))
        (should (equal (with-current-buffer a (buffer-string))
                       "half a thought"))))))

(ert-deftest aob-trace-comment-box-leaves-the-tail-alone ()
  (aob-tests--with-session s
    (aob-tests--with-comment-trace trace s
      (let ((lifted 0)
            (aob-compose-float t))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'posframe-show)
                   (lambda (&rest _) (selected-frame)))
                  ((symbol-function 'select-frame-set-input-focus) #'ignore)
                  ((symbol-function 'aob-trace-uncover-all)
                   (lambda () (setq lifted (1+ lifted)))))
          (let ((box (aob-trace--comment-box trace 3 "why?" 1)))
            (should (= lifted 0))
            (should-not (aob-trace--lifting-box-p box)))
          (let ((draft (aob-compose s)))
            (should (= lifted 1))
            (should (aob-trace--lifting-box-p draft))
            (kill-buffer draft)))))))

(ert-deftest aob-trace-comment-box-send-now-answers-a-waiting-question ()
  (aob-tests--with-trace-session s
    (aob-tests--request
     s 21 "elicitation/create"
     '(:mode "form" :sessionId "sess-test" :toolCallId "toolu_q"
       :message "Which auth method?"
       :requestedSchema
       (:type "object"
        :properties
        (:question_0 (:type "string" :title "Auth"
                      :oneOf [(:const "OAuth" :title "OAuth")
                              (:const "API key" :title "API key")])))))
    (let ((said nil))
      (cl-letf (((symbol-function 'aob-trace--say)
                 (lambda (_s text &optional _files) (push text said))))
        (aob-tests--goto "Auth · Which auth method?")
        (let ((box (aob-trace-answer)))
          (with-current-buffer box
            (should aob-compose--anchor)
            (insert "neither, a token")
            (aob-tests--capturing sent
              (aob-trace-comment-send-now)
              (should (= (length (aob-tests--replies sent)) 1))
              (should (string-match-p "neither, a token"
                                      (car (aob-tests--replies sent)))))))
        (should-not said)
        (should-not (aob-session-decisions s))
        (should-not (aob-session-ref s :comments))))))

(ert-deftest aob-trace-comment-box-carries-images-to-send-now ()
  (aob-tests--with-session s
    (aob-tests--with-comment-trace trace s
      (let ((prompted nil))
        (cl-letf (((symbol-function 'aob-prompt)
                   (lambda (_s text files) (push (cons text files) prompted))))
          (with-current-buffer (aob-trace--comment-box trace 3 "why?" 1)
            (setq aob-compose--attachments '((1 . "/tmp/x.png")))
            (insert "look [[Image1]]")
            (aob-compose-send))
          (should (equal (plist-get (car (aob-session-ref s :comments)) :files)
                         '("/tmp/x.png")))
          (with-current-buffer (aob-trace--comment-box trace 4 "and" 1)
            (insert "more")
            (aob-trace-comment-send-now))
          (should (equal (cdar prompted) '("/tmp/x.png")))
          (should (string-match-p "> why\\?\nlook" (caar prompted))))))))

(defun aob-tests--host-defun (name)
  "Evaluate the host's own definition of NAME from layer-aob.el."
  (with-temp-buffer
    (insert-file-contents (locate-library "layer-aob.el"))
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (eq (car-safe form) 'defun) (eq (cadr form) name))
              (eval form t))))
      (end-of-file nil))))

(defmacro aob-tests--prompts (var &rest body)
  "Run BODY with every prompt collected newest-first into VAR as (TEXT . FILES)."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'aob-prompt)
                (lambda (_s text &optional files) (push (cons text files) ,var)))
               ((symbol-function 'aob-interject)
                (lambda (_s text) (push (list text) ,var))))
       ,@body)))

(ert-deftest aob-trace-send-carries-held-images ()
  (aob-tests--with-trace-session s
    (aob-trace--add-comment s 1 "why?" "see this" '("/tmp/held.png"))
    (aob-tests--prompts sent
      (aob-trace-send)
      (should (= (length sent) 1))
      (should (string-match-p "> why\\?\nsee this" (caar sent)))
      (should (equal (cdar sent) '("/tmp/held.png"))))
    (should-not (aob-session-ref s :comments))))

(ert-deftest aob-compose-send-carries-held-images-through-the-host-hook ()
  (aob-tests--host-defun 'ygg-aob--comments-compose)
  (aob-tests--with-session s
    (aob-trace--add-comment s 1 "why?" "see this" '("/tmp/held.png"))
    (let ((aob-compose-before-send-functions '(ygg-aob--comments-compose)))
      (aob-tests--prompts sent
        (with-current-buffer (aob-compose s)
          (aob-compose-attach "/tmp/own.png")
          (insert " go")
          (aob-compose-send))
        (should (= (length sent) 1))
        (should (string-match-p "\\`> why\\?\nsee this\n\n *go" (caar sent)))
        (should (equal (cdar sent) '("/tmp/own.png" "/tmp/held.png")))))
    (should-not (aob-session-ref s :comments))))

(ert-deftest aob-compose-reopened-draft-keeps-its-images ()
  (aob-tests--with-session s
    (let ((draft (aob-compose s)))
      (unwind-protect
          (progn
            (with-current-buffer draft
              (aob-compose-attach "/tmp/kept.png")
              (insert " look"))
            (should (eq (aob-compose s) draft))
            (with-current-buffer draft
              (should (equal aob-compose--attachments '((1 . "/tmp/kept.png")))))
            (let ((other (aob-compose (cons 'new "codex"))))
              (should-not (buffer-local-value 'aob-compose--attachments other))
              (kill-buffer other))
            (aob-tests--prompts sent
              (with-current-buffer draft (aob-compose-send))
              (should (equal (cdar sent) '("/tmp/kept.png")))
              (should (equal (caar sent) "look"))))
        (when (buffer-live-p draft) (kill-buffer draft))))))

(ert-deftest aob-trace-comment-box-reopened-keeps-its-images ()
  (aob-tests--with-session s
    (aob-tests--with-comment-trace trace s
      (with-current-buffer (aob-trace--comment-box trace 3 "why?" 1)
        (aob-compose-attach "/tmp/kept.png"))
      (with-current-buffer (aob-trace--comment-box trace 3 "why?" 1)
        (insert " this")
        (aob-compose-send))
      (should (equal (plist-get (car (aob-session-ref s :comments)) :files)
                     '("/tmp/kept.png"))))))

(ert-deftest aob-compose-deleted-image-token-drops-its-file ()
  (aob-tests--with-session s
    (aob-tests--prompts sent
      (with-current-buffer (aob-compose s)
        (aob-compose-attach "/tmp/gone.png")
        (erase-buffer)
        (insert "no picture after all")
        (aob-compose-send))
      (should (equal (car sent) '("no picture after all"))))))

(defvar aob-mcp--tools)
(defvar ygg-preset-config-directory)
(defvar ygg-preset-user-directory)
(defvar ygg-preset-read-old-homes)
(declare-function ygg-aob--preset-limits "layer-aob" (text))
(declare-function ygg-preset-list "ygg-preset" (&optional root))
(declare-function ygg-preset-get "ygg-preset" (name &optional root))
(declare-function ygg-preset-tools "ygg-preset" (d))
(declare-function ygg-preset-thinking "ygg-preset" (d))

(defmacro aob-tests--quietly (&rest body)
  "Run BODY, then stop the quiet timer it may have started."
  `(unwind-protect (progn ,@body)
     (when (timerp aob--quiet-timer)
       (cancel-timer aob--quiet-timer)
       (setq aob--quiet-timer nil))))

(ert-deftest aob-quiet-mark-follows-progress-not-chunks ()
  (aob-tests--with-session s
    (aob-tests--quietly
     (let ((aob-quiet-minutes 10)
           (stale (lambda () (aob-session-put s :progress-at (- (float-time) 720)))))
       (aob-set-state s 'working)
       (should-not (aob-session-quiet s))
       (funcall stale)
       (should (equal (aob-session-quiet s) "quiet 12m"))
       (aob-tests--say s "first words")
       (should-not (aob-session-quiet s))
       (funcall stale)
       (aob-tests--say s " more words")
       (aob-tests--think s "weighing it")
       (should (equal (aob-session-quiet s) "quiet 12m"))
       (aob-tests--tool s "t1" "read" "Read a.el" "in_progress")
       (should-not (aob-session-quiet s))
       (funcall stale)
       (aob-tests--update s (list :sessionUpdate "tool_call_update" :toolCallId "t1"
                                  :status "in_progress"
                                  :content (aob-tests--output "partial")))
       (should (aob-session-quiet s))
       (aob-tests--update s (list :sessionUpdate "tool_call_update" :toolCallId "t1"
                                  :status "completed"))
       (should-not (aob-session-quiet s))
       (funcall stale)
       (aob-set-state s 'idle)
       (should-not (aob-session-quiet s))
       (aob-set-state s 'working)
       (should-not (aob-session-quiet s))))))

(ert-deftest aob-quiet-tick-marks-header-and-modeline-and-progress-lifts-it ()
  (aob-tests--with-session s
    (aob-tests--quietly
     (let* ((aob-quiet-minutes 10)
            (seen nil)
            (aob-meter-change-hook
             (list (lambda (x) (when (eq x s) (push (aob-session-ref x :quiet-shown) seen))))))
       (aob-set-state s 'working)
       (should (timerp aob--quiet-timer))
       (aob-session-put s :progress-at (- (float-time) 720))
       (aob--quiet-tick)
       (should (equal (aob-session-ref s :quiet-shown) "quiet 12m"))
       (should (string-match-p "working · quiet 12m" (aob-trace--header s)))
       (aob--modeline-refresh)
       (let ((at (string-search "quiet 12m" aob-modeline-string)))
         (should at)
         (should (eq (get-text-property at 'face aob-modeline-string) 'shadow)))
       (aob-note-progress s)
       (should-not (aob-session-ref s :quiet-shown))
       (should (equal seen '(nil "quiet 12m")))
       (aob-set-state s 'idle)
       (dolist (x (aob-sessions)) (unless (eq x s) (aob-set-state x 'idle)))
       (aob--quiet-tick)
       (should-not aob--quiet-timer)))))

(defun aob-tests--big (label n)
  "N characters opening with LABEL-HEAD and closing with LABEL-TAIL."
  (let ((head (format "%s-HEAD" label)) (tail (format "%s-TAIL" label)))
    (concat head (make-string (- n (length head) (length tail)) ?x) tail)))

(ert-deftest aob-session-tail-trims-results-and-drops-thinking ()
  (aob-tests--with-session s
    (aob-tests--quietly
     (aob-set-state s 'working)
     (aob-event s 'prompt :text "find the leak")
     (aob-tests--think s "SECRET-THOUGHT")
     (aob-tests--tool s "t1" "execute" "Bash" "completed"
                      :content (aob-tests--output (aob-tests--big "OUT" 10000)))
     (aob-tests--say s "found it")
     (let ((tail (aob-session-tail s)))
       (should (string-prefix-p "test:1 (working)" tail))
       (should (string-match-p "user: find the leak" tail))
       (should (string-match-p "agent: found it\\'" tail))
       (should-not (string-match-p "SECRET-THOUGHT" tail))
       (should (string-match-p "OUT-HEAD" tail))
       (should (string-match-p "OUT-TAIL" tail))
       (should (string-match-p "\\[\\.\\.\\. 5904 characters cut \\.\\.\\.\\]" tail))
       (should (< (length tail) 5000))))))

(ert-deftest aob-session-tail-keeps-within-its-bound-newest-last ()
  (aob-tests--with-session s
    (dotimes (i 12)
      (aob-tests--tool s (format "t%d" i) "execute" (format "Bash %d" i) "completed"
                       :content (aob-tests--output (aob-tests--big (format "R%d" i) 3000))))
    (let ((tail (aob-session-tail s)))
      (should (<= (length tail) aob-session-tail-limit))
      (should (string-match-p "\\[earlier events not shown\\]" tail))
      (should (string-match-p "R11-TAIL\\'" tail))
      (should-not (string-match-p "R0-HEAD" tail)))
    (aob-tests--tool s "huge" "execute" "Bash huge" "completed"
                     :content (aob-tests--output (make-string 40000 ?y)))
    (should (<= (length (aob-session-tail s 1000)) 1000))))

(ert-deftest aob-mcp-session-read-resolves-id-or-unique-name ()
  (require 'aob-mcp-tools)
  (let ((handler (plist-get (gethash "session_read" aob-mcp--tools) :handler)))
    (cl-flet ((ask (who)
                (let (form)
                  (cl-letf (((symbol-function 'aob-mcp-relay)
                             (lambda (_conn _id f &rest _) (setq form f))))
                    (let ((direct (funcall handler (list :id who) nil 1)))
                      (if form (eval form t) direct))))))
      (aob-tests--with-session s
        (aob-event s 'prompt :text "hello there")
        (should (equal (car (ask "acp:test:1")) "test:1 (starting)"))
        (should (member "user: hello there" (ask "test:1")))
        (should (equal (ask "nobody") '("no session called nobody")))
        (should (equal (ask " ") "which session? pass id"))
        (let ((twin (aob-create-session :id "acp:test:twin" :backend 'acp
                                        :name "test:1" :state 'idle)))
          (unwind-protect
              (should (string-match-p "2 conversations are called test:1"
                                      (car (ask "test:1"))))
            (aob-remove-session twin)))))))

(ert-deftest aob-preset-tools-and-thinking-overlay ()
  (require 'ygg-preset)
  (let* ((base (make-temp-file "aob-presets-" t))
         (config (expand-file-name "config/" base))
         (user (expand-file-name "user/" base))
         (root (expand-file-name "repo/" base))
         (write (lambda (dir name text)
                  (make-directory dir t)
                  (with-temp-file (expand-file-name name dir) (insert text)))))
    (unwind-protect
        (let ((ygg-preset-config-directory config)
              (ygg-preset-user-directory user)
              (ygg-preset-read-old-homes nil))
          (funcall write config "look.md"
                   "---\nname: look\ntools: Read, Grep, Bash\nthinking: high\n---\nlook")
          (funcall write config "peek.md" "---\nname: peek\npreset: look\n---\npeek")
          (funcall write config "odd.md" "---\nname: odd\nthinking: extreme\n---\nodd")
          (funcall write user "look.md" "---\nthinking: Off\n---\nmore")
          (funcall write (expand-file-name ".aob/presets/" root) "look.md"
                   "---\ntools: [Read, Grep]\n---\nhere")
          (let ((look (ygg-preset-get "look" root))
                (plain (ygg-preset-get "look")))
            (should (equal (ygg-preset-tools look) '("Read" "Grep")))
            (should (equal (ygg-preset-thinking look) "off"))
            (should (equal (ygg-preset-tools plain) '("Read" "Grep" "Bash")))
            (should (equal (ygg-preset-thinking plain) "off")))
          (should (equal (ygg-preset-tools (ygg-preset-get "peek")) '("Read" "Grep" "Bash")))
          (should-not (ygg-preset-thinking (ygg-preset-get "odd")))
          (should-not (ygg-preset-tools (ygg-preset-get "odd"))))
      (delete-directory base t))))

(ert-deftest aob-inspect-preset-allows-no-editing-tools ()
  (require 'ygg-preset)
  (let ((tools (ygg-preset-tools (ygg-preset-get "inspect"))))
    (should (member "Read" tools))
    (should (member "Bash" tools))
    (should (member "Agent" tools))
    (dolist (edit '("Edit" "Write" "NotebookEdit"))
      (should-not (member edit tools)))))

(ert-deftest aob-compose-spawn-takes-limits-from-the-presets-it-carries ()
  (require 'ygg-preset)
  (aob-tests--host-defun 'ygg-aob--preset-limits)
  (cl-letf (((symbol-function 'ygg-aob--presets-of)
             (lambda (_dir) (cons nil (ygg-preset-list)))))
    (let ((refs (ygg-aob--preset-limits
                 "why does it hang\n\n<preset name=\"inspect\">\nbody\n</preset>")))
      (should (equal (plist-get refs :want-tools)
                     (ygg-preset-tools (ygg-preset-get "inspect"))))
      (should-not (plist-member refs :want-thinking)))
    (should-not (ygg-aob--preset-limits "no preset named here"))))

(defun aob-tests--opened-with (init refs &optional method)
  "What METHOD (session/new) under INIT with REFS sends, notes and leaves over."
  (let* ((aob-acp-system-append "told")
         (aob-acp-session-refs refs)
         spec)
    (cl-letf (((symbol-function 'aob-acp--connect)
               (lambda (_s open _then) (setq spec (funcall open init)))))
      (let ((s (aob-acp--open "claude" "limits:1" "/tmp/proj/" nil
                              (lambda (_init)
                                (list (or method "session/new")
                                      (list :cwd "/tmp/proj" :mcpServers [])))
                              #'ignore)))
        (unwind-protect
            (list (json-parse-string
                   (json-serialize (list :method (car spec) :params (cadr spec)))
                   :object-type 'plist :array-type 'list)
                  (mapcar (lambda (e) (plist-get e :title)) (aob-session-events s))
                  (aob-session-ref s :want-thinking))
          (aob-remove-session s))))))

(ert-deftest aob-session-new-carries-claude-tools-and-thinking ()
  (pcase-let* ((claude '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp")))
               (`(,wire ,_ ,left)
                (aob-tests--opened-with claude '(:want-tools ("Read" "Grep")
                                                 :want-thinking "high")))
               (meta (plist-get (plist-get wire :params) :_meta))
               (opts (plist-get (plist-get meta :claudeCode) :options)))
    (should (equal (plist-get (plist-get meta :systemPrompt) :append) "told"))
    (should (equal (plist-get opts :tools) '("Read" "Grep")))
    (should (equal (plist-get opts :effort) "high"))
    (should-not left))
  (pcase-let* ((claude '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp")))
               (`(,wire ,_ ,_) (aob-tests--opened-with claude '(:want-thinking "off")))
               (opts (plist-get (plist-get (plist-get (plist-get wire :params) :_meta)
                                           :claudeCode)
                                :options)))
    (should (equal (plist-get opts :thinking) '(:type "disabled")))
    (should-not (plist-member opts :tools)))
  (pcase-let ((`(,wire ,_ ,_) (aob-tests--opened-with
                               '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp"))
                               nil)))
    (should-not (plist-member (plist-get (plist-get wire :params) :_meta) :claudeCode))))

(ert-deftest aob-session-new-elsewhere-notes-an-unenforced-tools-limit ()
  (pcase-let ((`(,wire ,titles ,left)
               (aob-tests--opened-with '(:agentInfo (:name "codex-acp"))
                                       '(:want-tools ("Read") :want-thinking "low"))))
    (should-not (plist-member (plist-get (plist-get wire :params) :_meta) :claudeCode))
    (should (member "tools limit not enforced for claude" titles))
    (should (equal left "low"))))

(ert-deftest aob-session-resume-and-load-carry-claude-limits-and-fork-says-not ()
  (let ((claude '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp")))
        (refs '(:want-tools ("Read") :want-thinking "high")))
    (dolist (method '("session/resume" "session/load"))
      (pcase-let* ((`(,wire ,titles ,left) (aob-tests--opened-with claude refs method))
                   (opts (plist-get (plist-get (plist-get (plist-get wire :params) :_meta)
                                               :claudeCode)
                                    :options)))
        (should (equal (plist-get wire :method) method))
        (should (equal (plist-get opts :tools) '("Read")))
        (should (equal (plist-get opts :effort) "high"))
        (should-not (member "tools limit not enforced for claude" titles))
        (should-not left)))
    (pcase-let ((`(,wire ,titles ,left) (aob-tests--opened-with claude refs "session/fork")))
      (should-not (plist-member (plist-get (plist-get wire :params) :_meta) :claudeCode))
      (should (member "tools limit not enforced for claude" titles))
      (should (equal left "high")))
    (pcase-let ((`(,_ ,titles ,_) (aob-tests--opened-with '(:agentInfo (:name "codex-acp"))
                                                          refs "session/resume")))
      (should (member "tools limit not enforced for claude" titles)))))

(ert-deftest aob-limits-survive-into-a-restore-and-a-fork ()
  (let (seen)
    (aob-tests--with-session s
      (aob-session-put s :agent "claude")
      (aob-session-put s :acp-id "sid-1")
      (aob-session-put s :want-tools '("Read"))
      (aob-session-put s :want-thinking "low")
      (aob-acp--with-limits s '(:agentInfo (:name "claude-agent-acp")) "session/new" nil)
      (should (equal (plist-get (aob-acp--entry s) :limits)
                     '(:want-tools ("Read") :want-thinking "low")))
      (cl-letf (((symbol-function 'aob-acp--open)
                 (lambda (&rest _) (push aob-acp-session-refs seen) s))
                ((symbol-function 'aob-acp--seed-history) #'ignore))
        (aob-acp-fork s)
        (aob-acp-resume-entry (aob-acp--entry s)))
      (dolist (refs seen)
        (should (equal (plist-get refs :want-tools) '("Read")))
        (should (equal (plist-get refs :want-thinking) "low"))))
    (should (= (length seen) 2))))

(ert-deftest aob-trace-linkify-never-dials-a-remote-name ()
  (let* ((dialled 0)
         (dial (lambda (&rest _) (cl-incf dialled) (error "dialled out")))
         (file-name-handler-alist (cons (cons "\\`/ssh:" dial) file-name-handler-alist))
         (aob-trace--refs (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'tramp-file-name-handler) dial)
              ((symbol-function 'tramp-autoload-file-name-handler) dial))
      (dolist (line '("/ssh:evil.example:foo.c:12: error" "/sudo::/etc/passwd:3: x"))
        (dotimes (_ 2)
          (let ((out (aob-trace--linkify line "/tmp/")))
            (should (equal out line))
            (should-not (text-property-not-all 0 (length out) 'aob-file nil out))))))
    (should (= dialled 0))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) (error "broken"))))
      (should (equal (aob-trace--linkify "/tmp/a.c:1: x" "/tmp/") "/tmp/a.c:1: x")))
    (should (gethash (cons "/tmp/" "/tmp/a.c:1: x") aob-trace--refs))))

(ert-deftest aob-want-thinking-takes-the-thought-level-option ()
  (aob-tests--with-session s
    (aob-session-put s :agent "codex")
    (aob-session-put s :config-options
                     '((:id "model" :currentValue "gpt-5.2")
                       (:id "reasoning_effort" :category "thought_level" :currentValue "medium"
                        :options ((:value "minimal") (:value "low")
                                  (:value "medium") (:value "high")))))
    (let (wire)
      (cl-letf (((symbol-function 'aob-acp--set-config)
                 (lambda (_s id val) (push (list id val) wire))))
        (aob-acp--want-thinking s "off")
        (aob-acp--want-thinking s "high")
        (should (equal wire '(("reasoning_effort" "high") ("reasoning_effort" "minimal"))))
        (aob-session-put s :config-options nil)
        (aob-acp--want-thinking s "low")
        (should (= (length wire) 2))
        (should (equal (plist-get (car (aob-session-events s)) :title)
                       "thinking low not supported by codex"))))))
