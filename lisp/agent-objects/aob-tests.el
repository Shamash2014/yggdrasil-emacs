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
(require 'aob-shells nil t)

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

(defun aob-tests--answer (s msg result)
  "Feed S the response to MSG, a request it sent, with RESULT."
  (aob-tests--feed s (json-serialize (list :jsonrpc "2.0" :id (plist-get msg :id)
                                           :result result))))

(defconst aob-tests--codex-model-option
  '(:id "model" :name "Model" :category "model" :type "select"
    :currentValue "gpt-5.2-codex"
    :options [(:value "gpt-5.2-codex" :name "GPT-5.2 Codex")
              (:value "gpt-5.2" :name "GPT-5.2")])
  "The model option codex-acp 2.0.1 advertises among its configOptions.")

(ert-deftest aob-model-changes-only-through-config-options ()
  "Codex's model option is the picker, and nothing ever sends session/set_model:
not with a models field beside the option, not with one alone, not when
a current_model_update names another model."
  (aob-tests--with-session s
    (let ((aob-acp-persist-file nil))
      (aob-tests--capturing sent
        (aob-acp--session-opened
         s '(:sessionId "sess-test"
             :models (:currentModelId "gpt-5.2"
                      :availableModels ((:modelId "gpt-5.2" :name "GPT-5.2")))))
        (aob-tests--update s (list :sessionUpdate "config_option_update"
                                   :configOptions (vector aob-tests--codex-model-option)))
        (should (equal (aob-session-ref s :model-name) "GPT-5.2 Codex"))
        (aob-tests--update s '(:sessionUpdate "current_model_update"
                               :currentModelId "gpt-5.2"))
        (should (equal (aob-session-ref s :model-id) "gpt-5.2-codex"))
        (aob-acp--set-model s "gpt-5.2")
        (let ((req (car sent)))
          (should (equal (plist-get req :method) "session/set_config_option"))
          (should (equal (plist-get (plist-get req :params) :configId) "model"))
          (should (equal (plist-get (plist-get req :params) :value) "gpt-5.2"))
          (aob-tests--answer
           s req (list :configOptions
                       (vector (plist-put (copy-sequence aob-tests--codex-model-option)
                                          :currentValue "gpt-5.2")))))
        (should (equal (aob-session-ref s :model-name) "GPT-5.2"))
        (should (seq-find (lambda (ev) (equal (plist-get ev :title) "model: GPT-5.2"))
                          (aob-session-events s)))
        (aob-acp--config-apply s nil)
        (should-not (aob-acp--model-info s))
        (aob-acp--set-model s "gpt-5.2-codex")
        (should-not (seq-find (lambda (m) (equal (plist-get m :method) "session/set_model"))
                              sent))))))

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
                 (lambda (prompt _coll &rest _)
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
        (aob-session-put s :ctx-used 990000)
        (let ((aob-acp-autocompact-ratio nil)
              (aob-acp-autocompact-reserve nil))
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

(ert-deftest aob-model-waits-on-a-working-session-with-no-options ()
  ;; a spawn with an intent is working before its handshake: M then must
  ;; wait for the options, not report a session that advertises nothing
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (let (picked)
      (cl-letf* (((symbol-function 'run-at-time)
                  (lambda (_ _ fn &rest args) (apply fn args)))
                 ((symbol-function 'aob-acp--model-1)
                  (lambda (_s) (setq picked t))))
        (aob-acp-model s)
        (should-not picked)
        (aob-set-state s 'blocked)
        (should-not picked)
        (cl-letf (((symbol-function 'aob-acp--flush-queue) #'ignore))
          (aob-acp--session-opened
           s (list :sessionId "sess-test"
                   :configOptions (vector aob-tests--codex-model-option))))
        (should picked)))))

(ert-deftest aob-model-on-an-opened-session-with-no-options-does-not-wait ()
  ;; an agent that never advertises a model must be told so at once, not
  ;; waited on across every working and blocked turn
  (aob-tests--with-session s
    (aob-session-put s :opened t)
    (aob-set-state s 'working)
    (let ((hooks aob-state-change-hook))
      (should-error (aob-acp-model s) :type 'user-error)
      (should (equal aob-state-change-hook hooks)))))

(ert-deftest aob-model-on-an-asleep-session-keeps-it-for-the-wake ()
  ;; asleep there is no connection to ask: the choice goes where the
  ;; resume reads it, and nothing is sent
  (let ((s (aob-create-session :id "acp:test:asleep" :backend 'acp
                               :name "asleep" :project "/tmp/proj/"
                               :dir "/tmp/proj/" :state 'done))
        sent)
    (unwind-protect
        (progn
          (aob-session-put s :agent "claude")
          (aob-session-put s :mode-id "default")
          (aob-session-put s :asleep (list :acp-id "sess-asleep" :agent "claude"))
          (cl-letf (((symbol-function 'aob-acp--request)
                     (lambda (&rest args) (push args sent)))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) "haiku")))
            (aob-acp-model s))
          (should-not sent)
          (should (equal (aob-session-ref s :want-model) "haiku"))
          (should (equal (plist-get (aob-session-ref s :asleep) :model) "haiku")))
      (aob-remove-session s))))

(ert-deftest aob-model-switch-that-lands-elsewhere-is-traced ()
  ;; the reply carries the model the agent is on; one other than asked
  ;; for, or a refusal, is a line in the trace naming what it runs
  (aob-tests--with-session s
    (aob-acp--config-apply s (list aob-tests--codex-model-option))
    (let (sent)
      (cl-letf (((symbol-function 'aob-acp--send-proc)
                 (lambda (_proc msg) (push msg sent))))
        (aob-acp--set-model s "gpt-5.2")
        (aob-tests--answer s (car sent)
                           (list :configOptions (vector aob-tests--codex-model-option)))
        (let ((ev (seq-find (lambda (e) (plist-get e :warning)) (aob-session-events s))))
          (should ev)
          (should (string-match-p "gpt-5.2" (plist-get ev :title)))
          (should (string-match-p "GPT-5.2 Codex" (plist-get ev :title))))
        (setf (aob-session-events s) nil)
        (aob-acp--set-model s "gpt-5.2")
        (aob-tests--feed s (json-serialize
                            (list :jsonrpc "2.0" :id (plist-get (car sent) :id)
                                  :error (list :code -32602 :message "Invalid params"))))
        (let ((ev (seq-find (lambda (e) (plist-get e :warning)) (aob-session-events s))))
          (should ev)
          (should (string-match-p "Invalid params" (plist-get ev :title)))
          (should (string-match-p "GPT-5.2 Codex" (plist-get ev :title))))))))

(ert-deftest aob-want-model-ambiguous-names ()
  ;; a name several models carry is not the first of them: codex takes
  ;; exact ids only and is refused, claude resolves the name itself
  (aob-tests--with-session s
    (let (set info)
      (cl-letf (((symbol-function 'aob-acp--model-info) (lambda (_s) info))
                ((symbol-function 'aob-acp--set-model)
                 (lambda (_s id) (setq set id))))
        (setq info (cons "gpt-5.5"
                         (list (list :value "gpt-5.5" :name "GPT-5.5")
                               (list :value "gpt-5.6-sol" :name "GPT-5.6 Sol")
                               (list :value "gpt-5.6-terra" :name "GPT-5.6 Terra"))))
        (aob-session-put s :agent "codex")
        (aob-acp--want-model s "gpt-5.6")
        (should-not set)
        (let ((ev (seq-find (lambda (e) (plist-get e :warning)) (aob-session-events s))))
          (should ev)
          (should (string-match-p "gpt-5.6-sol" (plist-get ev :title)))
          (should (string-match-p "gpt-5.6-terra" (plist-get ev :title))))
        (setq info (cons "default"
                         (list (list :value "default" :name "Default")
                               (list :value "sonnet" :name "Sonnet")
                               (list :value "sonnet[1m]" :name "Sonnet (1M context)"))))
        (aob-acp--want-model s "sonn")
        (should (equal set "sonnet"))
        (setq set nil)
        (setq info (cons "default"
                         (list (list :value "default" :name "Default")
                               (list :value "sonnet" :name "Sonnet 4.6")
                               (list :value "opus" :name "Opus 4.6"))))
        (aob-session-put s :agent "claude")
        (aob-acp--want-model s "4.6")
        (should (equal set "4.6"))))))

(ert-deftest aob-usage-model-meta-is-the-live-model ()
  ;; the model that answered rides usage_update's _meta; the header names
  ;; it only when it is not the one picked
  (aob-tests--with-session s
    (aob-session-put s :model-id "sonnet")
    (aob-tests--update s (list :sessionUpdate "usage_update" :used 10 :size 1000
                               :_meta (list :_claude/model "claude-opus-4-7")))
    (should (equal (aob-session-ref s :model-live) "claude-opus-4-7"))
    (should (string-match-p "claude-opus-4-7" (aob-trace--header s)))
    (aob-session-put s :model-id "opus")
    (should-not (string-match-p "claude-opus-4-7" (aob-trace--header s)))
    (aob-tests--update s (list :sessionUpdate "notice" :severity "info"
                               :title "Model rerouted"
                               :description "Switched from gpt-5.6-sol to gpt-5.5 (high load)."))
    (should (equal (aob-session-ref s :model-live) "gpt-5.5"))
    (aob-set-state s 'idle)
    (cl-letf (((symbol-function 'aob-acp--request) #'ignore))
      (aob-acp--prompt-1 s "again"))
    (should-not (aob-session-ref s :model-live))))

(ert-deftest aob-model-alias-names-its-live-model-without-warning ()
  ;; default and opusplan pick no one model, so the one answering never
  ;; contradicts them; a family that is not the live one still does
  (aob-tests--with-session s
    (aob-session-put s :model-live "claude-opus-4-7")
    (let ((warns (lambda ()
                   (let ((h (aob-trace--model s)))
                     (text-property-any 0 (length h) 'face 'warning h)))))
      (aob-session-put s :model-id "default")
      (aob-session-put s :model-name "Default (recommended)")
      (should-not (funcall warns))
      (should (equal (substring-no-properties (aob-trace--model s))
                     "default (opus-4-7)"))
      (aob-session-put s :model-id "opusplan")
      (aob-session-put s :model-name "Opus Plan Mode")
      (should-not (funcall warns))
      (aob-session-put s :model-id "sonnet")
      (aob-session-put s :model-name "Sonnet")
      (should (funcall warns))
      (aob-session-put s :model-id "claude-sonnet-4-6")
      (aob-session-put s :model-name "Sonnet 4.6")
      (should (funcall warns)))))

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

(ert-deftest aob-steering-carries-images-and-reaches-a-held-turn ()
  "A correction with an image steers with the image block, a turn held on
a decision is steered rather than prompted, and a queued message with an
image steers from the queue."
  (let ((png (make-temp-file "aob-steer" nil ".png" "\x89PNG")))
    (unwind-protect
        (aob-tests--with-session s
          (aob-session-put s :acp-id "sess-test")
          (aob-session-put s :agent-meta '(:steering (:supported t)))
          (aob-session-put s :agent-caps '(:promptCapabilities (:image t)))
          (aob-set-state s 'blocked)
          (aob-tests--capturing frames
            (aob-acp--interject s "look at this" (list png))
            (let ((f (car frames)))
              (should (equal (plist-get f :method) "_session/steering"))
              (should (seq-find (lambda (b) (equal (plist-get b :type) "image"))
                                (plist-get (plist-get f :params) :prompt))))
            (aob-tests--reply s '(:outcome "injected"))
            (should (eq (aob-session-state s) 'blocked))
            (should (equal (plist-get (car (aob-session-events s)) :images) 1)))
          (aob-set-state s 'working)
          (aob-acp--queue s "fix" (list png))
          (with-temp-buffer
            (setq-local aob-trace--session-id (aob-session-id s))
            (aob-tests--capturing frames
              (aob-trace--queue-steer-1 s)
              (should (equal (plist-get (car frames) :method) "_session/steering"))
              (should (seq-find (lambda (b) (equal (plist-get b :type) "image"))
                                (plist-get (plist-get (car frames) :params) :prompt)))
              (should-not (aob-session-ref s :queued)))))
      (delete-file png))))

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

(ert-deftest aob-native-background-subagent-runs-until-its-sender-settles ()
  "A background Agent call completes as it launches; its subagent is still
working, and ends only when the turn that sent it does."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-set-state s 'working)
          (aob-tests--feed s (replace-regexp-in-string
                              "\"subagent_type\"" "\"run_in_background\":true,\"subagent_type\""
                              aob-tests--agent-call t t))
          (aob-tests--agent-done s "completed")
          (let ((kid (aob-session-native-child s (car (aob-session-subagents s)))))
            (should (eq 'working (aob-session-state kid)))
            (aob-tests--agent-child s "k1" "find src | wc -l" "in_progress")
            (aob-tests--agent-child s "k1" "find src | wc -l" "completed")
            (should (eq 'working (aob-session-state kid)))
            (aob-set-state s 'idle)
            (should (eq 'done (aob-session-state kid)))
            (aob-tests--agent-child s "k2" "rg TODO src" "in_progress")
            (should (eq 'working (aob-session-state kid)))
            (aob-tests--agent-child s "k2" "rg TODO src" "completed")
            (should (eq 'done (aob-session-state kid)))))
      (aob-tests--kill-views))))

(defun aob-tests--announce (s sid &optional on)
  "Feed S the subagent_spawned claude-agent-acp sends for SID, on session ON."
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"%s\",\"update\":{\"sessionUpdate\":\"subagent_spawned\",\"subagentSessionId\":\"%s\",\"name\":\"Count files\",\"task\":\"Count the files under src\",\"prompt\":\"Count the files under src\",\"capabilities\":{}}}}"
             (or on "sess-test") sid)))

(defun aob-tests--child-says (s sid text)
  "Feed S what the subagent SID says, stamped with the Agent call that
spawned it, as claude-agent-acp routes it."
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"%s\",\"update\":{\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"%s\"},\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"toolu_A\"}}}}}"
             sid text)))

(defun aob-tests--child-tool (s sid id title &optional stamp tool)
  "Feed S the tool call ID the subagent SID makes, stamped STAMP or toolu_A."
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"%s\",\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"%s\",\"title\":\"%s\",\"kind\":\"execute\",\"status\":\"in_progress\",\"rawInput\":{\"description\":\"%s\"},\"_meta\":{\"claudeCode\":{\"toolName\":\"%s\",\"parentToolUseId\":\"%s\"}}}}}"
             sid id title title (or tool "Bash") (or stamp "toolu_A"))))

(defun aob-tests--child-ends (s sid state)
  (aob-tests--feed
   s (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"sess-test\",\"update\":{\"sessionUpdate\":\"subagent_state_update\",\"subagentSessionId\":\"%s\",\"state\":\"%s\"}}}"
             sid state)))

(defun aob-tests--said (s)
  "Everything S's message events say, oldest first."
  (mapconcat #'aob-event-text
             (seq-filter (lambda (e) (eq (plist-get e :type) 'message))
                         (reverse (aob-session-events s)))
             ""))

(ert-deftest aob-initialize-offers-native-subagents-both-ways ()
  "The draft field and the AIR list go out; the reply that takes either
leaves the connection announcing, and one that takes neither does not."
  (aob-tests--with-session s
    (let ((proc (aob-session-conn s))
          (aob-acp-native-subagents t))
      (aob-tests--capturing sent
        (aob-acp--initialize proc)
        (let* ((caps (plist-get (plist-get (car sent) :params) :clientCapabilities))
               (json (json-serialize caps)))
          (should-not (plist-member caps :subagents))
          (should-not (string-match-p "\"subagents\"" json))
          (should (string-match-p (regexp-quote "\"subagent-transcript\":true,\"terminal_output_delta\":true,\"jetbrains\":{\"air\":{\"version\":1,\"capabilities\":[\"nativeSubagentSessions\"]}}")
                                  json))))
      (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":1,\"agentCapabilities\":{\"sessionCapabilities\":{\"subagents\":{}}},\"_meta\":{\"jetbrains\":{\"air\":{\"version\":1,\"capabilities\":[\"nativeSubagentSessions\"]}}}}}"
                                 (process-get proc 'aob-init-id)))
      (should (eq t (process-get proc 'aob-subagents)))
      (aob-tests--capturing _sent (aob-acp--initialize proc))
      (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":1,\"agentCapabilities\":{}}}"
                                 (process-get proc 'aob-init-id)))
      (should-not (process-get proc 'aob-subagents))
      (let ((aob-acp-native-subagents nil))
        (aob-tests--capturing _sent (aob-acp--initialize proc)))
      (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":{\"protocolVersion\":1,\"agentCapabilities\":{\"sessionCapabilities\":{\"subagents\":{}}}}}"
                                 (process-get proc 'aob-init-id)))
      (should-not (process-get proc 'aob-subagents)))))

(ert-deftest aob-announced-subagent-is-a-session-of-its-own ()
  "An announced subagent is one read-only session under its sender; what
it says under its own id is its own, and its end is the one announced."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-set-state s 'working)
          (aob-tests--announce s "child-1")
          (aob-tests--child-says s "child-1" "There are 12 files.")
          (let ((kids (aob-subagent-children s)))
            (should (= 1 (length kids)))
            (let ((kid (car kids)))
              (should (equal "Count files" (aob-session-name kid)))
              (should (equal (aob-session-id s) (aob-session-ref kid :parent-session)))
              (should (equal (aob-session-id s) (aob-session-ref kid :native-root)))
              (should (aob-subagent-native-p kid))
              (should (equal "There are 12 files." (aob-tests--said kid)))
              (should-not (string-match-p "12 files" (aob-tests--said s)))
              (should (eq 'working (aob-session-state kid)))
              (should-error (aob-prompt kid "hello" nil) :type 'user-error)
              (with-current-buffer (aob-trace-buffer kid)
                (aob-trace--render t)
                (should (string-match-p "Count the files under src" (buffer-string)))
                (should (string-match-p "read-only" (format "%s" header-line-format))))
              (aob-set-state s 'idle)
              (should (eq 'working (aob-session-state kid)))
              (aob-tests--child-ends s "child-1" "completed")
              (should (eq 'done (aob-session-state kid)))
              (aob-tests--announce s "child-1")
              (aob-tests--child-says s "child-1" " Again.")
              (aob-tests--child-ends s "child-1" "completed")
              (should (equal (list kid) (aob-subagent-children s)))
              (should (eq 'done (aob-session-state kid))))))
      (aob-tests--kill-views))))

(ert-deftest aob-loaded-conversation-brings-its-subagents-back ()
  "A load replays the announcements with the rest, before its own reply;
each subagent comes back once, with its words and its end."
  (aob-tests--with-session s
    (unwind-protect
        (let (loaded)
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-tests--capturing _sent
            (aob-acp--request s "session/load" (list :sessionId "sess-test")
                              (lambda (res _err) (setq loaded res))))
          (dotimes (_ 2)
            (aob-tests--announce s "child-1")
            (aob-tests--child-says s "child-1" "Done.")
            (aob-tests--child-ends s "child-1" "completed"))
          (aob-tests--reply s '(:sessionId "sess-test"))
          (should loaded)
          (let ((kids (aob-subagent-children s)))
            (should (= 1 (length kids)))
            (should (string-prefix-p "Done." (aob-tests--said (car kids))))
            (should (eq 'done (aob-session-state (car kids))))))
      (aob-tests--kill-views))))

(ert-deftest aob-announced-subagent-ends-as-it-was-stopped ()
  "Cancelled is done and says so; disconnected and failed fail, the one
that lost its agent saying that too."
  (pcase-dolist (`(,state ,ends ,row) '(("cancelled" done "cancelled")
                                         ("disconnected" failed "disconnected")
                                         ("failed" failed "failed")))
    (aob-tests--with-session s
      (unwind-protect
          (progn
            (process-put (aob-session-conn s) 'aob-subagents t)
            (aob-tests--announce s "child-1")
            (aob-tests--child-ends s "child-1" state)
            (let ((kid (car (aob-subagent-children s))))
              (should (eq ends (aob-session-state kid)))
              (unless (equal state "failed")
                (should (seq-find (lambda (e) (and (eq (plist-get e :type) 'state)
                                                   (string-match-p state (plist-get e :title))))
                                  (aob-session-events kid))))
              (with-current-buffer (aob-subagents-buffer s)
                (aob-subagents--render t)
                (should (string-match-p (concat "^" row " ") (buffer-string))))))
        (aob-tests--kill-views)))))

(ert-deftest aob-announced-subagent-trace-shows-its-own-stamped-steps ()
  "Every step claude routes to a subagent is stamped with the Agent call
that spawned it; that stamp is the subagent's own, so its trace shows
its words, steps and plan, and only a call it made itself nests."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-tests--announce s "child-1")
          (aob-tests--child-says s "child-1" "There are 12 files.")
          (aob-tests--child-tool s "child-1" "toolu_c1" "find src | wc -l")
          (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"child-1\",\"update\":{\"sessionUpdate\":\"plan\",\"entries\":[{\"content\":\"a\",\"status\":\"completed\",\"priority\":\"medium\"},{\"content\":\"b\",\"status\":\"pending\",\"priority\":\"medium\"}],\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"toolu_A\"}}}}}")
          (aob-tests--child-tool s "child-1" "toolu_N" "Look deeper" "toolu_A" "Agent")
          (aob-tests--child-tool s "child-1" "toolu_g" "rg deeper" "toolu_N")
          (let ((kid (car (aob-subagent-children s))))
            (should (equal '(1 . 2) (aob-session-ref kid :plan-progress)))
            (should (equal "toolu_N"
                           (plist-get (seq-find (lambda (e) (equal (plist-get e :tool-id) "toolu_g"))
                                                (aob-session-events kid))
                                      :parent)))
            (with-current-buffer (aob-trace-buffer kid)
              (aob-trace--render t)
              (let ((text (buffer-string)))
                (should (string-match-p "There are 12 files" text))
                (should (string-match-p "find src" text))
                (should (string-match-p "Look deeper" text))))))
      (aob-tests--kill-views))))

(ert-deftest aob-announced-subagent-stands-in-for-its-hidden-call ()
  "With native subagents the adapter sends no Agent call; the announcement
gives the sender a row of its own, listed while the subagent works and
holding its last words once it ends."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-set-state s 'working)
          (aob-tests--announce s "child-1")
          (aob-tests--child-says s "child-1" "There are 12 files.")
          (let* ((kid (car (aob-subagent-children s)))
                 (calls (aob-session-subagents s))
                 (call (car calls)))
            (should (= 1 (length calls)))
            (should (equal "Count files" (plist-get call :title)))
            (should (equal "in_progress" (plist-get call :status)))
            (should (eq kid (aob-session-native-child s call)))
            (with-current-buffer (aob-subagents-buffer s)
              (aob-subagents--render t)
              (should (string-match-p "^running .*Count files" (buffer-string))))
            (with-current-buffer (aob-trace-buffer s)
              (aob-trace--render t)
              (should (string-match-p "Count files" (buffer-string))))
            (aob-tests--child-ends s "child-1" "completed")
            (should (equal "completed" (plist-get call :status)))
            (should (plist-get call :done-ts))
            (should (string-match-p "There are 12 files" (aob-trace--content-text call)))
            (with-current-buffer (aob-subagents-buffer s)
              (aob-subagents--render t)
              (should (string-match-p "^done .*Count files" (buffer-string))))
            (aob-tests--announce s "child-1")
            (should (= 1 (length (aob-session-subagents s))))
            (should (equal "in_progress" (plist-get call :status)))
            (aob-tests--announce s "child-2" "child-1")
            (let ((grandkid (car (aob-subagent-children kid))))
              (should (equal (aob-session-id kid) (aob-session-ref grandkid :parent-session)))
              (should (= 1 (length (aob-session-subagents s))))
              (should (eq grandkid (aob-session-native-child
                                    kid (car (aob-session-subagents kid))))))
            (aob-set-state s 'dead)
            (should (equal "failed" (plist-get call :status)))))
      (aob-tests--kill-views))))

(ert-deftest aob-plan-cleared-context-settles-announced-subagents ()
  "Accepting a plan into a fresh context replaces the agent's runtime, and
the subagents it ran are never told to end: the answer ends them."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-set-state s 'working)
          (aob-tests--announce s "child-1")
          (let ((kid (car (aob-subagent-children s))))
            (aob-tests--claude-plan s 11)
            (should (eq 'working (aob-session-state kid)))
            (aob-tests--capturing sent
              (aob--call s :resolve (car (aob-session-decisions s)) "exit-plan-clear-accept-edits"))
            (should (eq 'done (aob-session-state kid)))
            (should (equal "cancelled" (plist-get (car (aob-session-subagents s)) :status)))))
      (aob-tests--kill-views))))

(ert-deftest aob-removed-subagent-frames-reach-no-one ()
  "Once an announced subagent is let go, what its agent still sends for it
lands nowhere: not in the sender, and its requests are cancelled."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-tests--announce s "child-1")
          (aob-remove-session (car (aob-subagent-children s)))
          (aob-tests--child-says s "child-1" "late words")
          (should-not (string-match-p "late words" (aob-tests--said s)))
          (aob-tests--capturing sent
            (aob-tests--request s 12 "session/request_permission"
                                (list :sessionId "child-1"
                                      :toolCall (list :toolCallId "toolu_c9" :title "rm -rf build")
                                      :options [(:optionId "allow" :name "Allow" :kind "allow_once")]))
            (aob-tests--request s 13 "elicitation/create"
                                (list :sessionId "child-1" :mode "form" :message "pick"
                                      :requestedSchema (list :type "object")))
            (should (equal (aob-tests--replies sent)
                           '("{\"jsonrpc\":\"2.0\",\"id\":12,\"result\":{\"outcome\":{\"outcome\":\"cancelled\"}}}"
                             "{\"jsonrpc\":\"2.0\",\"id\":13,\"result\":{\"action\":\"cancel\"}}"))))
          (should-not (aob-session-decisions s))
          (aob-tests--announce s "child-1")
          (aob-tests--child-says s "child-1" "back again")
          (should (string-match-p "back again" (aob-tests--said (car (aob-subagent-children s))))))
      (aob-tests--kill-views))))

(ert-deftest aob-announced-subagent-asks-its-own-permission ()
  "A permission a subagent asks lands as a Decision on it, whichever session
carries it, and its answer goes back over the connection with its id."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-tests--announce s "child-1")
          (let ((kid (car (aob-subagent-children s))))
            (aob-tests--request s 7 "session/request_permission"
                                (list :sessionId "child-1"
                                      :toolCall (list :toolCallId "toolu_c1" :title "rm -rf build"
                                                      :kind "execute"
                                                      :rawInput (list :command "rm -rf build"))
                                      :options [(:optionId "allow" :name "Allow" :kind "allow_once")]))
            (should-not (aob-session-decisions s))
            (let ((d (car (aob-session-decisions kid))))
              (should (equal "rm -rf build" (plist-get d :title)))
              (should (eq 'blocked (aob-session-state kid)))
              (aob-tests--capturing sent
                (aob--call kid :resolve d "allow")
                (should (equal (aob-tests--replies sent)
                               '("{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"allow\"}}}")))))
            (should (eq 'working (aob-session-state kid)))
            (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"child-1\",\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"toolu_c2\",\"title\":\"git push\",\"kind\":\"execute\",\"status\":\"pending\"}}}")
            (aob-tests--request s 8 "session/request_permission"
                                (list :sessionId "sess-test"
                                      :toolCall (list :toolCallId "toolu_c2" :title "git push")
                                      :options [(:optionId "allow" :name "Allow" :kind "allow_once")]))
            (should-not (aob-session-decisions s))
            (should (equal "git push" (plist-get (car (aob-session-decisions kid)) :title)))
            (aob-tests--capturing sent
              (aob-remove-session kid)
              (should (equal (aob-tests--replies sent)
                             '("{\"jsonrpc\":\"2.0\",\"id\":8,\"result\":{\"outcome\":{\"outcome\":\"cancelled\"}}}"))))
            (should-not (gethash "child-1" (aob-acp--proc-sessions (aob-session-conn s))))))
      (aob-tests--kill-views))))

(ert-deftest aob-announcing-agent-makes-no-subagent-of-its-calls ()
  "Where the agent announces its subagents, an Agent call makes none and
opens the announced one of its name, whichever came first; where it
does not, the call is still read as one."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-tests--feed s aob-tests--agent-call)
          (aob-tests--announce s "child-1")
          (should (= 1 (length (aob-subagent-children s))))
          (should (eq (car (aob-subagent-children s))
                      (aob-session-native-child s (car (aob-session-subagents s))))))
      (aob-tests--kill-views)))
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (process-put (aob-session-conn s) 'aob-subagents t)
          (aob-tests--announce s "child-1")
          (aob-tests--feed s aob-tests--agent-call)
          (let ((kid (car (aob-subagent-children s))))
            (should (= 1 (length (aob-subagent-children s))))
            (should (eq kid (aob-session-native-child s (car (aob-session-subagents s)))))
            (with-current-buffer (aob-subagents-buffer s)
              (aob-subagents--render t)
              (should (string-match-p "Count files" (buffer-string)))
              (goto-char (point-min))
              (with-current-buffer (progn (aob-subagents-open) (current-buffer))
                (should (equal (aob-trace--name kid) (buffer-name)))))))
      (aob-tests--kill-views)))
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-tests--feed s aob-tests--agent-call)
          (aob-tests--announce s "child-1")
          (should (= 1 (length (aob-subagent-children s))))
          (should (aob-session-native-child s (car (aob-session-subagents s)))))
      (aob-tests--kill-views))))

(ert-deftest aob-plan-permission-without-its-kind-is-still-a-plan ()
  "An AIR client is asked about ExitPlanMode with the call's id, title and
input only; the kind the call was announced with makes it a plan."
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"sessionId\":\"sess-test\",\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"toolu_plan\",\"name\":\"ExitPlanMode\",\"title\":\"Ready to code?\",\"kind\":\"switch_mode\",\"status\":\"pending\"}}}")
    (aob-tests--request s 9 "session/request_permission"
                        (list :sessionId "sess-test"
                              :toolCall (list :toolCallId "toolu_plan" :title "Ready to code?"
                                              :rawInput (list :plan "# Plan\n\n1. Ship"))
                              :options aob-tests--claude-plan-options))
    (let ((d (car (aob-session-decisions s))))
      (should (eq 'plan (plist-get d :kind)))
      (should (equal "# Plan\n\n1. Ship" (plist-get d :plan))))))

(ert-deftest aob-native-subagent-says-whose-it-is ()
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-tests--feed s aob-tests--agent-call)
          (should (equal "subagent of test:1"
                         (aob-subagent-of
                          (aob-session-native-child s (car (aob-session-subagents s))))))
          (should-not (aob-subagent-of s)))
      (aob-tests--kill-views))))

(ert-deftest aob-native-subagent-is-listed-only-while-it-runs ()
  "A subagent is a live session while it works; done, failed or killed it
leaves every list, and its sender's trace still holds the call."
  (dolist (end '(done failed killed))
    (aob-tests--with-session s
      (unwind-protect
          (progn
            (aob-tests--feed s aob-tests--agent-call)
            (let* ((call (car (aob-session-subagents s)))
                   (kid (aob-session-native-child s call)))
              (should (memq kid (aob-live-sessions)))
              (pcase end
                ('done (aob-tests--agent-done s "completed"))
                ('failed (aob-tests--agent-done s "failed"))
                ('killed (aob--call kid :kill)))
              (should-not (memq kid (aob-live-sessions)))
              (should (memq s (aob-live-sessions)))
              (should (eq call (car (aob-session-subagents s))))))
        (aob-tests--kill-views)))))

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

(defconst aob-tests--openspec-tasks
  "## 1. Parser\n\n- [x] 1.1 Read the format\n- [ ] 1.2 Parse numbered items\n\n## 2. Tests\n\n- [ ] 2.1 Cover the spec\n"
  "An OpenSpec change's tasks.md.")

(defun aob-tests--spec (dir)
  "Write an OpenSpec tasks.md under DIR and return its path."
  (let ((file (expand-file-name "openspec/changes/parse/tasks.md" dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert aob-tests--openspec-tasks))
    file))

(defun aob-tests--todo-write (s args)
  "Call the todo_write tool as S's agent would, with ARGS; return its answer."
  (require 'aob-mcp-tools)
  (let ((handler (plist-get (gethash "todo_write" aob-mcp--tools) :handler))
        form)
    (cl-letf (((symbol-function 'aob-mcp-host-session) (lambda (_) s))
              ((symbol-function 'aob-mcp-relay)
               (lambda (_conn _id f &rest _) (setq form f))))
      (let ((direct (funcall handler args nil 1)))
        (if form (eval form t) direct)))))

(ert-deftest aob-todo-openspec-tasks-parse ()
  "Numbered checkbox items under numbered headings read as items, and a
tick by id changes that one line and no other byte."
  (let* ((dir (make-temp-file "aob-spec-" t))
         (file (aob-tests--spec dir)))
    (unwind-protect
        (let ((items (plist-get (ygg-todo-read file) :items)))
          (should (equal '("1.1" "1.2" "2.1") (mapcar (lambda (it) (plist-get it :id)) items)))
          (should (equal '("1. Parser" "1. Parser" "2. Tests")
                         (mapcar (lambda (it) (plist-get it :section)) items)))
          (should (equal '(("1.1 Read the format" . t) ("1.2 Parse numbered items") ("2.1 Cover the spec"))
                         (aob-tests--list-items file)))
          (should (equal '(1 . 3) (ygg-todo-progress file)))
          (ygg-todo-set-done file "1.2" t "1.2 Parse numbered items")
          (should (equal (replace-regexp-in-string "- \\[ \\] 1.2" "- [x] 1.2"
                                                   aob-tests--openspec-tasks)
                         (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest aob-todo-write-binds-an-existing-file ()
  "todo_write with file binds that tasks.md instead of making a list; a
missing file or one outside the project is refused and binds nothing."
  (aob-tests--with-list s file
    (let* ((root (aob-session-project s))
           (spec (aob-tests--spec root))
           (outside (make-temp-file "aob-outside-" nil ".md" "- [ ] elsewhere\n")))
      (unwind-protect
          (progn
            (should (string-match-p "no such file"
                                    (car (aob-tests--todo-write
                                          s '(:file "openspec/changes/none/tasks.md")))))
            (should-not (funcall file))
            (should (string-match-p "outside the project"
                                    (car (aob-tests--todo-write s (list :file outside)))))
            (should-not (funcall file))
            (let ((answer (aob-tests--todo-write
                           s '(:file "openspec/changes/parse/tasks.md" :title "ignored"))))
              (should (string-match-p "1/3" (car answer)))
              (should (member "1.2 1.2 Parse numbered items" answer)))
            (should (equal (funcall file) spec))
            (should-not (file-exists-p (expand-file-name ".aob/tasks/" root)))
            (should (string-match-p "tasks.md 1/3"
                                    (car (aob-tests--todo-write s (list :file spec))))))
        (delete-file outside)))))

(ert-deftest aob-todo-plan-never-mirrors-into-a-bound-spec ()
  "Once a spec's tasks.md is the session's list, plans leave it alone:
same bytes, still bound, and no list of the agent's own is started."
  (aob-tests--with-list s file
    (let ((spec (aob-tests--spec (aob-session-project s))))
      (ygg-todo-session-adopt s spec)
      (aob-tests--feed s (aob-tests--plan-json '(((content . "Read the code") (status . "completed"))
                                                 ((content . "Write the fix") (status . "pending")))))
      (should (equal aob-tests--openspec-tasks
                     (with-temp-buffer (insert-file-contents spec) (buffer-string))))
      (should (equal (funcall file) spec))
      (should-not (file-exists-p (expand-file-name ".aob/tasks/" (aob-session-project s))))
      (let ((ygg-todo-by 'user)) (ygg-todo-set-done spec "2.1" t))
      (should (string-match-p "ticked: 2.1 Cover the spec" (ygg-todo-session-note s))))))

(ert-deftest aob-subagent-plan-counts-under-its-row ()
  "A subagent's TodoWrite reaches aob as a plan stamped with the Agent
call it runs under: the subagent keeps it as :plan-progress, and the
sender's plan and list never see it."
  (aob-tests--with-list s file
    (aob-tests--feed s aob-tests--agent-call)
    (let ((kid (aob-session-native-child s (car (aob-session-subagents s))))
          (plan (lambda (entries)
                  (format "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan\",\"entries\":%s,\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"toolu_A\"}}}}}"
                          (json-encode entries)))))
      (should kid)
      (aob-tests--feed s (funcall plan '(((content . "a") (status . "in_progress"))
                                         ((content . "b") (status . "pending"))
                                         ((content . "c") (status . "pending")))))
      (should (equal '(0 . 3) (aob-session-ref kid :plan-progress)))
      (aob-tests--feed s (funcall plan '(((content . "a") (status . "completed"))
                                         ((content . "b") (status . "in_progress"))
                                         ((content . "c") (status . "pending")))))
      (should (equal '(1 . 3) (aob-session-ref kid :plan-progress)))
      (should (= 1 (seq-count (lambda (e) (eq (plist-get e :type) 'plan))
                              (aob-session-events kid))))
      (should-not (aob-session-ref s :plan-ev))
      (should-not (funcall file))
      (should-not (aob-session-ref s :plan-progress))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"tool_call\",\"toolCallId\":\"toolu_T\",\"title\":\"Update TODOs: a, b, c\",\"kind\":\"think\",\"status\":\"pending\",\"rawInput\":{\"todos\":[{\"content\":\"a\",\"status\":\"completed\",\"activeForm\":\"A\"},{\"content\":\"b\",\"status\":\"completed\",\"activeForm\":\"B\"},{\"content\":\"c\",\"status\":\"in_progress\",\"activeForm\":\"C\"}]},\"_meta\":{\"claudeCode\":{\"toolName\":\"TodoWrite\",\"parentToolUseId\":\"toolu_A\"}}}}}")
      (should (equal '(2 . 3) (aob-session-ref kid :plan-progress)))
      (aob-tests--agent-child s "k9" "ls" "completed")
      (should (equal '(2 . 3) (aob-session-ref kid :plan-progress))))))

(ert-deftest aob-subagent-plan-removed-leaves-the-sender-alone ()
  "A subagent clearing its plan clears its own count, never the sender's plan or list."
  (aob-tests--with-list s file
    (aob-tests--feed s aob-tests--agent-call)
    (let ((kid (aob-session-native-child s (car (aob-session-subagents s))))
          (told 0))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan\",\"entries\":[{\"content\":\"root one\",\"status\":\"pending\"},{\"content\":\"root two\",\"status\":\"pending\"}]}}}")
      (should (= 2 (length (aob-tests--list-items (funcall file)))))
      (let ((aob-subagent-progress-functions (list (lambda (_) (setq told (1+ told))))))
        (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan\",\"entries\":[{\"content\":\"a\",\"status\":\"pending\"}],\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"toolu_A\"}}}}}")
        (should (equal '(0 . 1) (aob-session-ref kid :plan-progress)))
        (should (> told 0))
        (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan_removed\",\"_meta\":{\"claudeCode\":{\"parentToolUseId\":\"toolu_A\"}}}}}"))
      (should (aob-session-ref s :plan-ev))
      (should (= 2 (length (aob-tests--list-items (funcall file)))))
      (should-not (aob-session-ref kid :plan-progress)))))

(ert-deftest aob-todo-adopted-list-is-never-mirrored-even-under-aob-tasks ()
  "A list adopted from another session's .aob/tasks is not the adopter's to rewrite."
  (aob-tests--with-list s file
    (let ((other (ygg-todo-create (ygg-todo-session-dir s) "worker-x" "worker-x" nil)))
      (let ((ygg-todo-by 'agent)) (ygg-todo-add other "worker item"))
      (ygg-todo-session-adopt s other)
      (let ((before (with-temp-buffer (insert-file-contents other) (buffer-string))))
        (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"plan\",\"entries\":[{\"content\":\"lead item\",\"status\":\"pending\"}]}}}")
        (should (equal (funcall file) (expand-file-name other)))
        (should (equal before (with-temp-buffer (insert-file-contents other) (buffer-string))))))))

(ert-deftest aob-workflow-subagent-todowrite-counts ()
  "A workflow agent's TodoWrite step, read from its transcript, counts too."
  (let* ((lead (aob-create-session :id "wf-lead" :backend 'acp :name "lead" :state 'working))
         (kid (aob-create-session :id "wf-lead/a1" :backend 'workflow-subagent :name "a1"
                                  :state 'working :refs (list :parent-session "wf-lead"))))
    (unwind-protect
        (progn
          (aob-subagent--workflow-step
           kid (aob-subagent--workflow-parse
                "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"TodoWrite\",\"input\":{\"todos\":[{\"content\":\"x\",\"status\":\"completed\"},{\"content\":\"y\",\"status\":\"pending\"}]}}]}}"))
          (should (equal '(1 . 2) (aob-session-ref kid :plan-progress)))
          (should-not (aob-session-ref lead :plan-progress)))
      (mapc #'aob-remove-session (list kid lead)))))

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

(ert-deftest aob-a-session-is-told-its-worktree-and-branch-once-per-change ()
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "aob-place" t))))
         (main (file-name-as-directory (expand-file-name "main" root)))
         (tree (file-name-as-directory (expand-file-name "tree" root)))
         (git (lambda (dir &rest args)
                (let ((default-directory dir))
                  (should (eq 0 (apply #'call-process "git" nil nil nil args))))))
         (s (aob-create-session :id "acp:place:1" :backend 'acp :name "place"
                                :project main :dir tree :state 'idle)))
    (unwind-protect
        (progn
          (make-directory main)
          (funcall git main "init" "-q" "-b" "trunk")
          (funcall git main "-c" "user.email=t@t" "-c" "user.name=t"
                   "commit" "-q" "--allow-empty" "-m" "0")
          (aob-tests--link-worktree main tree "feat")
          (should (equal (aob-acp--place-note main)
                         (format "[workspace: %s · branch trunk · other worktrees: tree (feat)]"
                                 (directory-file-name main))))
          (should (equal (plist-get (aob-acp--place-block s) :text)
                         (format "[workspace: %s · branch feat · linked worktree of %s · other worktrees: main (trunk)]"
                                 (directory-file-name tree)
                                 (directory-file-name main))))
          (should-not (aob-acp--place-block s))
          (funcall git tree "checkout" "-q" "-b" "feat2")
          (should (string-match-p "branch feat2 " (plist-get (aob-acp--place-block s) :text)))
          (make-directory (expand-file-name "sub" tree))
          (should (string-match-p "branch feat2 " (aob-acp--place-note (expand-file-name "sub/" tree))))
          (should-not (aob-acp--place-note temporary-file-directory)))
      (aob-remove-session s)
      (delete-directory root t))))

(ert-deftest aob-a-session-is-told-of-the-other-worktrees-when-one-is-added ()
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "aob-place" t))))
         (main (file-name-as-directory (expand-file-name "main" root)))
         (tree (file-name-as-directory (expand-file-name "tree" root)))
         (other (file-name-as-directory (expand-file-name "other" root)))
         (git (lambda (dir &rest args)
                (let ((default-directory dir))
                  (should (eq 0 (apply #'call-process "git" nil nil nil args))))))
         (s (aob-create-session :id "acp:place:2" :backend 'acp :name "place"
                                :project main :dir tree :state 'idle)))
    (unwind-protect
        (progn
          (make-directory main)
          (funcall git main "init" "-q" "-b" "trunk")
          (funcall git main "-c" "user.email=t@t" "-c" "user.name=t"
                   "commit" "-q" "--allow-empty" "-m" "0")
          (aob-tests--link-worktree main tree "feat")
          (should (string-suffix-p "other worktrees: main (trunk)]"
                                   (plist-get (aob-acp--place-block s) :text)))
          (aob-tests--link-worktree main other "fix/y")
          (should (equal (plist-get (aob-acp--place-block s) :text)
                         (format "[workspace: %s · branch feat · linked worktree of %s · other worktrees: main (trunk), other (fix/y)]"
                                 (directory-file-name tree)
                                 (directory-file-name main))))
          (should-not (aob-acp--place-block s))
          (should (string-suffix-p "other worktrees: other (fix/y), tree (feat)]"
                                   (aob-acp--place-note main)))
          (delete-directory other t)
          (should (string-suffix-p "other worktrees: main (trunk)]"
                                   (plist-get (aob-acp--place-block s) :text))))
      (aob-remove-session s)
      (delete-directory root t))))

(ert-deftest aob-a-place-note-names-at-most-a-few-other-worktrees ()
  (let* ((wts (cons '("/r/main/" . "trunk")
                    (mapcar (lambda (n) (cons (format "/r/w%d/" n) (format "b%d" n)))
                            (number-sequence 1 10))))
         (tail (aob-acp--place-others wts (nth 1 wts))))
    (should (string-prefix-p " · other worktrees: main (trunk), w2 (b2), " tail))
    (should (string-suffix-p "w8 (b8), …" tail))
    (should-not (string-match-p "w9" tail))
    (should (equal (aob-acp--place-others (list (car wts) '("/r/x/")) (car wts))
                   " · other worktrees: x (detached)"))
    (should (equal (aob-acp--place-others (list (car wts)) (car wts)) ""))))

(ert-deftest aob-a-renamed-session-renames-the-trace-it-already-has ()
  (let* ((aob-acp-persist-file nil)
         (s (aob-create-session :id "acp:rename:2" :backend 'acp
                                :name "claude:5 · Sep 24" :state 'done))
         (buf (aob-trace-buffer s)))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "vbet auth fix")))
            (call-interactively #'aob-rename-session))
          (should (equal (buffer-name buf) "trace:vbet auth fix"))
          (should (eq (aob-trace-buffer s) buf))
          (should-not (get-buffer "trace:claude:5 · Sep 24")))
      (aob-remove-session s)
      (aob-tests--kill-views))))

(ert-deftest aob-a-renamed-conversation-keeps-its-name-asleep-awake-and-on-disk ()
  (let* ((aob-acp-persist-file (make-temp-file "aob-rename-" nil ".eld"))
         (aob-acp--opened-any nil)
         (stored (list :agent "claude" :name "claude:5" :dir "/tmp/"
                       :acp-id "rename-3"))
         (s (aob-create-session :id "acp:rename-3" :backend 'acp
                                :name "claude:5 · Sep 24" :state 'done
                                :refs (list :agent "claude" :acp-id "rename-3"
                                            :asleep stored)))
         woken)
    (unwind-protect
        (progn
          (with-temp-file aob-acp-persist-file (prin1 (list stored) (current-buffer)))
          (should (equal (plist-get (aob-acp--entry s) :name) "claude:5"))
          (aob-rename-session s "vbet auth fix")
          (let ((e (aob-acp-persisted-entry "rename-3")))
            (should (equal (plist-get e :name) "vbet auth fix"))
            (should (plist-get e :named-by-user)))
          (cl-letf (((symbol-function 'aob-acp-resume-entry)
                     (lambda (e &rest _) (setq woken e) nil)))
            (should-error (aob-transcript-wake s)))
          (should (equal (plist-get woken :name) "vbet auth fix"))
          (should (plist-get woken :named-by-user))
          (aob-remove-session s)
          (let* ((file (make-temp-file "aob-rename-" nil ".jsonl"))
                 (back (aob-transcript--session
                        (aob-acp-persisted-entry "rename-3") file)))
            (unwind-protect
                (progn
                  (should (equal (aob-session-name back) "vbet auth fix"))
                  (should (aob-session-ref back :named-by-user)))
              (aob-remove-session back)
              (delete-file file))))
      (when (aob-session-get "acp:rename-3") (aob-remove-session s))
      (aob-tests--kill-views)
      (delete-file aob-acp-persist-file))))

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

(ert-deftest aob-compose-from-a-trace-whose-folder-is-gone ()
  (let* ((live (make-temp-file "aob-live" t))
         (gone (expand-file-name "removed-worktree/" temporary-file-directory))
         (s (aob-create-session :id "acp:gone:1" :backend 'acp :name "gone"
                                :project gone :dir live))
         buf)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory gone)
          (setq buf (aob-compose s))
          (with-current-buffer buf
            (should (file-directory-p default-directory))
            (should (equal (file-name-as-directory live) default-directory))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (aob-remove-session s)
      (delete-directory live t))))

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

(ert-deftest aob-transcript-a-file-with-no-title-is-read-once ()
  "A conversation with no opening line to show is not queued again once
read: re-reading it every draw kept the reader and a full redraw running."
  (let ((file (make-temp-file "aob-notitle" nil ".jsonl"
                              "{\"type\":\"summary\"}\n"))
        (aob-transcript--titles (make-hash-table :test 'equal))
        (aob-transcript--queue nil)
        (aob-transcript--timer nil))
    (unwind-protect
        (progn
          (aob-transcript--want-title file)
          (aob-transcript--read-some)
          (should-not (aob-transcript--title-cached file))
          (should (aob-transcript--title-read-p file))
          (aob-transcript--entries "/p/" "claude" nil (list (list file "id-1" "/p/")))
          (should-not aob-transcript--queue)
          (should-not aob-transcript--timer))
      (when (timerp aob-transcript--timer) (cancel-timer aob-transcript--timer))
      (delete-file file))))

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

(ert-deftest aob-transcript-reads-a-codex-rollout ()
  "A Codex session is found by id under its day folder, and read without
the harness's own user turns."
  (skip-unless (fboundp 'aob-transcript--codex-file))
  (let* ((home (make-temp-file "aob-codex-home" t))
         (id "01a0ee9c-f365-7e22-90e3-ff308a4b9ec7")
         (dir (expand-file-name "sessions/2026/09/29" home))
         (file (expand-file-name (format "rollout-2026-09-29T22-20-58-%s.jsonl" id) dir))
         (aob-transcript--codex-files (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file file
            (insert "{\"type\":\"session_meta\",\"payload\":{\"id\":\"x\"}}\n"
                    "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"developer\",\"content\":[{\"type\":\"input_text\",\"text\":\"rules\"}]}}\n"
                    "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":\"<environment_context>x</environment_context>\"}]}}\n"
                    "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":\"[workspace: /r · branch m]\"},{\"type\":\"input_text\",\"text\":\"fix the dashboard\"}]}}\n"
                    "{\"type\":\"response_item\",\"payload\":{\"type\":\"reasoning\"}}\n"
                    "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"Fixed.\"}]}}\n"))
          (should (equal (aob-transcript--codex-file id (list home)) file))
          (should (equal (aob-transcript-turns file)
                         '(("user" . "fix the dashboard") ("assistant" . "Fixed."))))
          (should (equal (aob-transcript--title-1 file) "fix the dashboard")))
      (delete-directory home t))))

(defun aob-tests--transcript (&rest lines)
  "A transcript file holding LINES, one record each."
  (let ((file (make-temp-file "aob-transcript" nil ".jsonl")))
    (with-temp-file file
      (dolist (line lines) (insert line "\n")))
    file))

(ert-deftest aob-transcript-shows-a-claude-tool-call-as-a-tool-line ()
  "A tool_use reads as the line a live session draws for it, and its
tool_result adds no turn of its own."
  (skip-unless (fboundp 'aob-transcript--tools))
  (let ((file (aob-tests--transcript
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"is the tree clean?\"}}"
               "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"\"},{\"type\":\"text\",\"text\":\"Checking.\"},{\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"Bash\",\"input\":{\"command\":\"git status\",\"description\":\"Status\"}}]}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"toolu_1\",\"type\":\"tool_result\",\"content\":\"nothing to commit\",\"is_error\":false}]}}"
               "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Clean.\"}]}}")))
    (unwind-protect
        (progn
          (should (equal (aob-transcript-turns file t)
                         '(("user" . "is the tree clean?")
                           ("assistant" . "Checking.")
                           ("tool" . "Bash  git status")
                           ("assistant" . "Clean."))))
          (should-not (assoc "tool" (aob-transcript-turns file)))
          (let ((s (aob-transcript--session
                    (list :agent "claude" :acp-id "tooltest-0000" :name "t" :found t
                          :dir temporary-file-directory)
                    file)))
            (unwind-protect
                (let ((tool (seq-find (lambda (ev) (eq (plist-get ev :type) 'tool))
                                      (aob-session-events s))))
                  (should (equal (plist-get tool :title) "Bash  git status"))
                  (should (eq (aob-trace--state tool) 'done)))
              (aob-remove-session s))))
      (delete-file file))))

(ert-deftest aob-transcript-leaves-out-what-claude-wrote-to-itself ()
  "Meta notes, a compaction's summary, a background task's notice, a
command's output and an interrupt are not prompts; a slash command
reads as it was typed."
  (skip-unless (fboundp 'aob-transcript--command))
  (let ((file (aob-tests--transcript
               "{\"type\":\"user\",\"isMeta\":true,\"message\":{\"role\":\"user\",\"content\":\"Another Claude session sent a message\"}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<command-name>/model</command-name>\\n<command-message>model</command-message>\\n<command-args>opus</command-args>\"}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<local-command-stdout>Set model to opus</local-command-stdout>\"}}"
               "{\"type\":\"system\",\"subtype\":\"compact_boundary\"}"
               "{\"type\":\"user\",\"isCompactSummary\":true,\"message\":{\"role\":\"user\",\"content\":\"This session is being continued from a previous conversation\"}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<task-notification>\\n<task-id>a1</task-id></task-notification>\"}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"[Request interrupted by user]\"}]}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<bash-input>make</bash-input>\"}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<bash-stdout>ok</bash-stdout><bash-stderr></bash-stderr>\"}}"
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"go on\"}}")))
    (unwind-protect
        (should (equal (aob-transcript-turns file)
                       '(("user" . "/model opus") ("user" . "!make") ("user" . "go on"))))
      (delete-file file))))

(ert-deftest aob-transcript-shows-a-codex-turn-once ()
  "Codex writes a prompt and an answer both as an event and as a
response item; the conversation shows each once, and its tool calls as
tool lines."
  (skip-unless (fboundp 'aob-transcript--tools))
  (let ((file (aob-tests--transcript
               "{\"type\":\"session_meta\",\"payload\":{\"id\":\"x\",\"cwd\":\"/p\"}}"
               "{\"type\":\"turn_context\",\"payload\":{\"model\":\"gpt\"}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":\"run the tests\"}]}}"
               "{\"type\":\"event_msg\",\"payload\":{\"type\":\"user_message\",\"message\":\"run the tests\",\"images\":[]}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"reasoning\",\"summary\":[],\"encrypted_content\":\"x\"}}"
               "{\"type\":\"event_msg\",\"payload\":{\"type\":\"agent_reasoning\",\"text\":\"thinking\"}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"name\":\"exec_command\",\"arguments\":\"{\\\"cmd\\\":\\\"make test\\\"}\",\"call_id\":\"c1\"}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call_output\",\"call_id\":\"c1\",\"output\":\"ok\"}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"custom_tool_call\",\"name\":\"exec\",\"input\":\"const r = await tools.exec_command({\\\"cmd\\\":\\\"git diff\\\"});\",\"call_id\":\"c2\"}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"custom_tool_call_output\",\"call_id\":\"c2\",\"output\":[{\"type\":\"input_text\",\"text\":\"Script completed\"}]}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"custom_tool_call\",\"name\":\"apply_patch\",\"input\":\"*** Begin Patch\\n*** Update File: a.el\\n@@\\n*** End Patch\",\"call_id\":\"c3\"}}"
               "{\"type\":\"event_msg\",\"payload\":{\"type\":\"agent_message\",\"message\":\"All pass.\"}}"
               "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"All pass.\"}]}}"
               "{\"type\":\"compacted\",\"payload\":{\"message\":\"\"}}")))
    (unwind-protect
        (should (equal (aob-transcript-turns file t)
                       '(("user" . "run the tests")
                         ("tool" . "exec_command  make test")
                         ("tool" . "exec  git diff")
                         ("tool" . "apply_patch  a.el")
                         ("assistant" . "All pass."))))
      (delete-file file))))

(ert-deftest aob-transcript-titles-a-claude-conversation-by-its-name ()
  "The name you gave a conversation beats the one Claude made up, which
beats its opening line; an old file's summary counts as its name."
  (skip-unless (fboundp 'aob-transcript--named-in))
  (let ((prompt "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"hello there\"}}")
        files)
    (unwind-protect
        (progn
          (push (aob-tests--transcript prompt) files)
          (should (equal (aob-transcript--title-1 (car files)) "hello there"))
          (push (aob-tests--transcript
                 "{\"type\":\"summary\",\"summary\":\"Fix the sidebar\",\"leafUuid\":\"u1\"}"
                 prompt)
                files)
          (should (equal (aob-transcript--title-1 (car files)) "Fix the sidebar"))
          (push (aob-tests--transcript
                 prompt
                 "{\"type\":\"ai-title\",\"aiTitle\":\"Old guess\",\"sessionId\":\"s\"}"
                 "{\"type\":\"ai-title\",\"aiTitle\":\"Greeting\",\"sessionId\":\"s\"}")
                files)
          (should (equal (aob-transcript--title-1 (car files)) "Greeting"))
          (push (aob-tests--transcript
                 prompt
                 "{\"type\":\"custom-title\",\"customTitle\":\"Mine\",\"sessionId\":\"s\"}"
                 "{\"type\":\"ai-title\",\"aiTitle\":\"Greeting\",\"sessionId\":\"s\"}")
                files)
          (should (equal (aob-transcript--title-1 (car files)) "Mine")))
      (mapc #'delete-file files))))

(ert-deftest aob-transcript-names-a-codex-thread-by-its-thread-name ()
  "A Codex thread is listed under the name session_index.jsonl gives it."
  (skip-unless (fboundp 'aob-transcript--codex-names))
  (let* ((home (make-temp-file "aob-codex-home" t))
         (dir (expand-file-name "sessions/2026/09/29" home))
         (id "01a0ee9c-f365-7e22-90e3-ff308a4b9ec7")
         (aob-transcript--found (make-hash-table :test 'equal))
         (aob-transcript--codex-heads (make-hash-table :test 'equal))
         (aob-transcript--titles (make-hash-table :test 'equal))
         (aob-transcript--queue nil))
    (cl-letf (((symbol-function 'aob-transcript--homes) (lambda (&rest _) (list home)))
              ((symbol-function 'aob-transcript--want-title) #'ignore))
      (unwind-protect
          (progn
            (make-directory dir t)
            (with-temp-file (expand-file-name
                             (format "rollout-2026-09-29T22-20-58-%s.jsonl" id) dir)
              (insert (format "{\"type\":\"session_meta\",\"payload\":{\"id\":\"%s\",\"cwd\":\"/p\"}}\n" id)))
            (with-temp-file (expand-file-name "session_index.jsonl" home)
              (insert (format "{\"id\":\"%s\",\"thread_name\":\"First name\"}\n" id)
                      (format "{\"id\":\"%s\",\"thread_name\":\"Fix the dashboard\"}\n" id)))
            (should (equal (plist-get (car (aob-transcript-found "/p/" "codex")) :name)
                           "Fix the dashboard")))
        (delete-directory home t)))))

(ert-deftest aob-transcript-lists-codex-rollouts-by-cwd ()
  "Codex rollouts are listed for the project their cwd is in, worktrees
under it included, and not for any other."
  (skip-unless (fboundp 'aob-transcript--codex-found))
  (let* ((home (make-temp-file "aob-codex-home" t))
         (dir (expand-file-name "sessions/2026/09/29" home))
         (here "01a0ee9c-f365-7e22-90e3-ff308a4b9ec7")
         (there "01a0ee9d-0000-7e22-90e3-ff308a4b9ec7")
         (aob-transcript--found (make-hash-table :test 'equal))
         (aob-transcript--codex-heads (make-hash-table :test 'equal))
         (aob-transcript--titles (make-hash-table :test 'equal))
         (aob-transcript--queue nil))
    (cl-letf (((symbol-function 'aob-transcript--homes) (lambda (&rest _) (list home)))
              ((symbol-function 'aob-transcript--want-title) #'ignore))
      (unwind-protect
          (progn
            (make-directory dir t)
            (dolist (thread `((,here "/p/wt/a") (,there "/q")))
              (with-temp-file (expand-file-name
                               (format "rollout-2026-09-29T22-20-58-%s.jsonl" (car thread)) dir)
                (insert (format "{\"type\":\"session_meta\",\"payload\":{\"session_id\":\"s\",\"id\":\"%s\",\"cwd\":\"%s\",\"base_instructions\":{\"text\":\"%s\"}}}\n"
                                (car thread) (cadr thread) (make-string 20000 ?x)))))
            (let ((found (aob-transcript-found "/p/" "codex")))
              (should (equal (mapcar (lambda (e) (plist-get e :acp-id)) found) (list here)))
              (should (equal (plist-get (car found) :agent) "codex"))
              (should (equal (plist-get (car found) :project) "/p/"))
              (should (plist-get (car found) :found)))
            (should-not (aob-transcript-found "/p/" "codex" "archive")))
        (delete-directory home t)))))

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
       (pcase-dolist (`(,k . ,def) '(("S" . aob-cancel) ("k" . aob-kill-session)
                                      ("x" . aob-acp-command) ("d" . aob-todo)
                                      ("a" . ygg-aob-activity) ("M" . aob-acp-cycle-mode)
                                      ("e" . aob-trace-queue-edit) ("X" . aob-trace-queue-drop)
                                      ("s" . aob-trace-queue-steer)))
         (should (eq (lookup-key trace k) def)))
       (should-not (lookup-key trace "C"))
       (should (eq (lookup-key plan "S") #'aob-cancel))
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

(ert-deftest aob-clock-ticks-only-while-a-running-clock-is-seen ()
  "A clock no window shows moves nothing and keeps no timer; showing it winds it."
  (aob-tests--with-session s
    (let ((now 100.0) (heard 0) (seen nil))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (let ((aob-meter-change-hook (list (lambda (x) (when (eq x s) (cl-incf heard)))))
              (aob-clock-shown-functions (list (lambda (x) (and seen (eq x s))))))
          (unwind-protect
              (progn
                (aob-turn-begin s)
                (should (timerp aob--clock-timer))
                (setq now 101.0)
                (aob--clock-tick)
                (should (= heard 0))
                (should-not aob--clock-timer)
                (aob--clock-on-display)
                (should-not aob--clock-timer)
                (should (= heard 0))
                (setq seen t)
                (aob--clock-on-display)
                (should (timerp aob--clock-timer))
                (should (= heard 1))
                (aob--clock-tick)
                (should (= heard 1))
                (setq now 102.0)
                (aob--clock-tick)
                (should (= heard 2)))
            (aob-turn-end s)
            (when (timerp aob--clock-timer)
              (cancel-timer aob--clock-timer)
              (setq aob--clock-timer nil))))))))

(ert-deftest aob-trace-clock-is-seen-only-in-a-shown-trace ()
  (let* ((s (aob-create-session :id "trace:clock" :backend 'acp :name "clock" :state 'idle))
         (buf (aob-trace-buffer s)))
    (unwind-protect
        (progn
          (should (memq #'aob-trace--clock-shown-p aob-clock-shown-functions))
          (should-not (aob-trace--clock-shown-p s))
          (with-current-buffer buf (rename-buffer "*a trace by another name*"))
          (save-window-excursion
            (set-window-buffer (selected-window) buf)
            (should (aob-trace--clock-shown-p s))))
      (kill-buffer buf)
      (aob-remove-session s))))

(ert-deftest aob-trace-meter-change-redraws-the-header-not-the-trace ()
  "A spend that moves rewrites a shown header and leaves the session's tick
alone; a buried trace is only marked, and drawn whole when it is shown."
  (let* ((s (aob-create-session :id "trace:meter" :backend 'acp :name "meter" :state 'idle))
         (buf (aob-trace-buffer s))
         (header (lambda () (buffer-local-value 'header-line-format buf))))
    (unwind-protect
        (let ((tick (aob-session-ref s :tick)))
          (should (memq #'aob-trace--meter-changed aob-meter-change-hook))
          (should-not (memq #'aob--dirty aob-meter-change-hook))
          (should-not (string-match-p "\\$1\\.50" (funcall header)))
          (with-current-buffer buf (rename-buffer "*a trace by another name*"))
          (aob-session-put s :cost-reading 1.5)
          (aob-trace--meter-changed s)
          (should-not (string-match-p "\\$1\\.50" (funcall header)))
          (save-window-excursion
            (set-window-buffer (selected-window) buf)
            (aob--render-on-display nil)
            (should (string-match-p "\\$1\\.50" (funcall header)))
            (aob-session-put s :cost-reading 2.5)
            (aob-trace--meter-changed s)
            (should (string-match-p "\\$2\\.50" (funcall header))))
          (should (eql tick (aob-session-ref s :tick))))
      (kill-buffer buf)
      (aob-remove-session s))))

(ert-deftest aob-queue-send-now-stops-the-turn-with-this-one-first ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (dolist (text '("alpha" "beta"))
      (aob-acp--prompt s text))
    (let (called)
      (cl-letf (((symbol-function 'aob--call)
                 (lambda (_s op &rest args) (push (cons op args) called))))
        (with-current-buffer (aob-trace-buffer s)
          (aob-trace--render t)
          (goto-char (point-min))
          (search-forward "beta")
          (aob-trace-queue-send-now))
        (should (equal (mapcar #'car (aob-session-ref s :queued)) '("beta" "alpha")))
        (should (equal called '((:cancel))))
        (setq called nil)
        (aob-set-state s 'idle)
        (with-current-buffer (aob-trace-buffer s)
          (aob-trace-queue-send-now))
        (should (equal called '((:flush))))))))

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

(ert-deftest aob-trace-stands-in-the-sessions-own-worktree ()
  "A session working in a worktree of its project has its trace in that
worktree, so git and project commands from the trace reach the same tree
the agent edits."
  (let* ((project (file-name-as-directory (make-temp-file "aob-proj" t)))
         (tree (file-name-as-directory (make-temp-file "aob-tree" t))))
    (unwind-protect
        (aob-tests--with-session s
          (setf (aob-session-project s) project
                (aob-session-dir s) tree)
          (let ((buf (aob-trace-buffer s)))
            (unwind-protect
                (with-current-buffer buf
                  (should (equal default-directory tree))
                  (should (equal (aob-trace--root) tree)))
              (kill-buffer buf))))
      (delete-directory project t)
      (delete-directory tree t))))

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
  (let* ((aob-acp-native-subagents nil)
         (json (json-serialize (aob-acp--client-capabilities))))
    (should (string-match-p "\"elicitation\":{\"form\":{},\"url\":{}}" json))
    (should (equal json (concat "{\"fs\":{\"readTextFile\":false,\"writeTextFile\":false},"
                                "\"elicitation\":{\"form\":{},\"url\":{}},"
                                "\"auth\":{\"terminal\":true},"
                                "\"session\":{\"configOptions\":{\"boolean\":{}},"
                                "\"notices\":{},\"compaction\":{}},"
                                "\"_meta\":{\"subagent-transcript\":true,"
                                "\"terminal_output_delta\":true}}")))))

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
      (should-not (string-match-p "◇ Other" (buffer-string)))
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

(defconst aob-tests--ask-one
  '(:mode "form" :sessionId "sess-test" :toolCallId "toolu_o"
    :message "Which cache?"
    :requestedSchema
    (:type "object"
     :properties
     (:question_0 (:type "string" :title "Cache"
                   :oneOf [(:const "Redis" :title "Redis") (:const "None" :title "None")])
      :question_0_custom (:type "string" :title "Other"
                          :_meta (:_askUserQuestionCustomAnswer
                                  (:questionId "question_0" :isCustomAnswer t)))))))

(defmacro aob-tests--answering (choice typed &rest body)
  "Run BODY with every choice read answering CHOICE and every text read TYPED.
The tables offered are collected into `offered', each as its completions."
  (declare (indent 2))
  `(let ((offered nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_p table &rest _)
                  (push (all-completions "" table) offered) ,choice))
               ((symbol-function 'completing-read-multiple)
                (lambda (_p table &rest _)
                  (push (all-completions "" table) offered) ,choice))
               ((symbol-function 'read-string) (lambda (&rest _) ,typed)))
       ,@body)))

(ert-deftest aob-choice-table-ends-in-other-and-keeps-the-agents-order ()
  (let ((table (aob--choice-table '("b" "a"))))
    (should (equal (all-completions "" table) '("b" "a" "Other…")))
    (should (eq (cdr (assq 'display-sort-function
                           (cdr (funcall table "" nil 'metadata))))
                'identity))))

(ert-deftest aob-resolve-other-sends-claude-its-custom-answer ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 61 "elicitation/create" aob-tests--ask-one)
    (aob-tests--answering "Other…" "Memcached"
      (aob-tests--capturing sent
        (aob-resolve s)
        (should (equal (car offered) '("Redis" "None" "Other…")))
        (should (equal (aob-tests--replies sent)
                       (list (concat "{\"jsonrpc\":\"2.0\",\"id\":61,\"result\":{\"action\":\"accept\","
                                     "\"content\":{\"question_0_custom\":\"Memcached\"}}}"))))))
    (aob-tests--request s 62 "elicitation/create" aob-tests--ask-one)
    (aob-tests--answering "Redis" "unused"
      (aob-tests--capturing sent
        (aob-resolve s)
        (should (equal (aob-tests--replies sent)
                       (list (concat "{\"jsonrpc\":\"2.0\",\"id\":62,\"result\":{\"action\":\"accept\","
                                     "\"content\":{\"question_0\":\"Redis\"}}}"))))))))

(ert-deftest aob-resolve-other-joins-a-multi-select ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request
     s 63 "elicitation/create"
     '(:mode "form" :sessionId "sess-test" :message "Which parts?"
       :requestedSchema
       (:type "object"
        :properties
        (:question_0 (:type "array" :items (:anyOf [(:const "api" :title "api")
                                                    (:const "ui" :title "ui")]))
         :question_0_custom (:type "string" :title "Other"
                             :_meta (:_askUserQuestionCustomAnswer (:questionId "question_0")))))))
    (aob-tests--answering '("api" "Other…") "db"
      (aob-tests--capturing sent
        (aob-resolve s)
        (should (equal (car offered) '("api" "ui" "Other…")))
        (should (equal (aob-tests--replies sent)
                       (list (concat "{\"jsonrpc\":\"2.0\",\"id\":63,\"result\":{\"action\":\"accept\","
                                     "\"content\":{\"question_0\":[\"api\"],\"question_0_custom\":\"db\"}}}"))))))))

(ert-deftest aob-resolve-string-field-reads-plain-text ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request
     s 64 "elicitation/create"
     '(:mode "form" :sessionId "sess-test" :message "Name it"
       :requestedSchema (:type "object" :properties (:name (:type "string" :title "Name")))))
    (aob-tests--answering "never" "widget"
      (aob-tests--capturing sent
        (aob-resolve s)
        (should-not offered)
        (should (equal (aob-tests--replies sent)
                       (list (concat "{\"jsonrpc\":\"2.0\",\"id\":64,\"result\":{\"action\":\"accept\","
                                     "\"content\":{\"name\":\"widget\"}}}"))))))))

(ert-deftest aob-resolve-other-on-an-enum-only-field-declines-and-says-it ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request
     s 65 "elicitation/create"
     '(:mode "form" :sessionId "sess-test" :message "Pick a region"
       :requestedSchema
       (:type "object"
        :properties (:region (:type "string" :title "Region" :enum ["eu" "us"]))
        :required ["region"])))
    (aob-tests--answering "Other…" "ap-south, if it is cheaper"
      (aob-tests--capturing sent
        (aob-resolve s)
        (should (equal (car offered) '("eu" "us" "Other…")))
        (should (equal (aob-tests--replies sent)
                       '("{\"jsonrpc\":\"2.0\",\"id\":65,\"result\":{\"action\":\"decline\"}}")))))
    (should (equal (caar (aob-session-ref s :queued))
                   "Pick a region: ap-south, if it is cheaper"))))

(ert-deftest aob-resolve-permission-offers-no-other ()
  (aob-tests--with-session s
    (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"id\":66,\"method\":\"session/request_permission\",\"params\":{\"toolCall\":{\"title\":\"x\"},\"options\":[{\"optionId\":\"a\",\"name\":\"Allow\",\"kind\":\"allow_once\"},{\"optionId\":\"r\",\"name\":\"Reject\",\"kind\":\"reject_once\"}]}}")
    (aob-tests--answering "Allow" "unused"
      (aob-tests--capturing sent
        (aob-resolve s)
        (should (equal (car offered) '("Allow" "Reject")))
        (should (equal (aob-tests--replies sent)
                       '("{\"jsonrpc\":\"2.0\",\"id\":66,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"a\"}}}")))))))

(ert-deftest aob-trace-shows-other-last-and-reads-your-own-there ()
  (aob-tests--with-trace-session s
    (aob-tests--request s 67 "elicitation/create" aob-tests--ask-one)
    (aob-tests--goto "◦ None")
    (forward-line 1)
    (should (looking-at "    ◦ Other…$"))
    (should (equal (get-text-property (point) 'aob-question) "Which cache?"))
    (let (boxed)
      (cl-letf (((symbol-function 'aob-trace--comment-box)
                 (lambda (_buf _seq question &rest _) (setq boxed question))))
        (aob-trace-answer))
      (should (equal boxed "Which cache?")))
    (aob-trace--add-comment s (plist-get (car (aob-session-decisions s)) :seq)
                            "Which cache?" "Memcached")
    (aob-tests--capturing sent
      (aob-trace-send)
      (should (equal (aob-tests--replies sent)
                     (list (concat "{\"jsonrpc\":\"2.0\",\"id\":67,\"result\":{\"action\":\"accept\","
                                   "\"content\":{\"question_0_custom\":\"Memcached\"}}}")))))))

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
          (setf (aob-session-project s) dir
                (aob-session-dir s) dir)
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
  "Name, model, state, clock, cost, context and todo; a mode only when it
is not the one a session runs in anyway; no folder or token count out."
  (aob-tests--with-trace-session s
    (aob-session-put s :mode-id "default")
    (aob-session-put s :model-name "opus")
    (aob-session-put s :ctx-used 213400)
    (aob-trace--render t)
    (should (string-match-p "\\` test:1 · opus · working · .*213k ctx\\'" header-line-format))
    (should-not (string-match-p "default\\|/tmp/proj" header-line-format))
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
        (should (= (aob-compose--wanted-height) aob-compose-anchored-height))
        (insert "\n2\n3\n4\n5")
        (should (= (aob-compose--wanted-height) 7))
        (insert (make-string 40 ?\n))
        (should (= (aob-compose--wanted-height)
                   aob-compose-anchored-max-height))
        (erase-buffer)
        (should (= (aob-compose--wanted-height) aob-compose-anchored-height))))))

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

(defun aob-tests--commenting ()
  "The spans lit as being commented on in this buffer, as (START . END)."
  (mapcar (lambda (o) (cons (overlay-start o) (overlay-end o)))
          (seq-filter (lambda (o) (eq (overlay-get o 'face) 'aob-trace-commenting))
                      (overlays-in (point-min) (point-max)))))

(defmacro aob-tests--commenting-on (s beg end &rest body)
  "Say words in S's trace, with BEG and END bound around \"bravo\", and run BODY."
  (declare (indent 3))
  `(aob-tests--with-trace-session ,s
     (save-window-excursion
       (set-window-buffer (selected-window) (current-buffer))
       (aob-tests--say ,s "alpha bravo charlie")
       (aob-tests--goto "bravo")
       (search-forward "bravo")
       (let ((,beg (match-beginning 0))
             (,end (match-end 0))
             (aob-compose-float nil))
         (unwind-protect
             (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                       ((symbol-function 'posframe-show)
                        (lambda (&rest _) (selected-frame)))
                       ((symbol-function 'select-frame-set-input-focus) #'ignore))
               ,@body)
           (dolist (b (buffer-list))
             (when (string-prefix-p "compose:comment:" (buffer-name b))
               (kill-buffer b))))))))

(defun aob-tests--comment-draft ()
  "The comment draft open now."
  (seq-find (lambda (b) (string-prefix-p "compose:comment:" (buffer-name b)))
            (buffer-list)))

(ert-deftest aob-trace-comment-lights-what-it-is-on ()
  (aob-tests--commenting-on s beg end
    (aob-trace-comment beg end)
    (should (equal (aob-tests--commenting) (list (cons beg end))))
    (aob-trace-comment beg end)
    (should (equal (aob-tests--commenting) (list (cons beg end))))))

(ert-deftest aob-trace-comment-light-outlives-a-redraw ()
  (aob-tests--commenting-on s beg end
    (aob-trace-comment beg end)
    (setq aob-trace--blocks nil)
    (aob-trace--render t)
    (should (equal (aob-tests--commenting) (list (cons beg end))))))

(ert-deftest aob-trace-comment-light-goes-when-held-or-killed ()
  (aob-tests--commenting-on s beg end
    (aob-trace-comment beg end)
    (let ((draft (aob-tests--comment-draft)))
      (should (aob-tests--commenting))
      (with-current-buffer draft
        (funcall (nth 2 aob-compose--anchor) "fix this" nil))
      (should (buffer-live-p draft))
      (should-not (aob-tests--commenting))
      (kill-buffer draft))
    (aob-tests--goto "bravo")
    (search-forward "bravo")
    (aob-trace-comment (match-beginning 0) (match-end 0))
    (should (aob-tests--commenting))
    (kill-buffer (aob-tests--comment-draft))
    (should-not (aob-tests--commenting))))

(ert-deftest aob-trace-comment-lights-while-read-in-the-minibuffer ()
  (aob-tests--commenting-on s beg end
    (let (while-read)
      (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
                ((symbol-function 'read-string)
                 (lambda (&rest _)
                   (setq while-read (aob-tests--commenting))
                   "fix this")))
        (aob-trace-comment beg end))
      (should (equal while-read (list (cons beg end))))
      (should-not (aob-tests--commenting)))))

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
              (should (equal (aob-tests--replies sent)
                             '("{\"jsonrpc\":\"2.0\",\"id\":21,\"result\":{\"action\":\"decline\"}}"))))))
        (should (equal (caar (aob-session-ref s :queued))
                       "Which auth method?: neither, a token"))
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
                (lambda (_s text &rest _) (push (list text) ,var))))
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

(ert-deftest aob-sidebar-shows-only-the-clocks-it-draws ()
  "A visible sidebar keeps the clock ticking only for sessions it has a row for."
  (aob-tests--host-defun 'ygg-aob--sidebar-shows-clock-p)
  (defvar ygg-projects-buffer-name)
  (let ((ygg-projects-buffer-name " *aob-tests-sidebar*"))
    (aob-tests--with-session drawn
      (aob-tests--with-session hidden
        (with-current-buffer (get-buffer-create ygg-projects-buffer-name)
          (insert (propertize "row" 'ygg-entry drawn)))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
                (should (ygg-aob--sidebar-shows-clock-p drawn))
                (should-not (ygg-aob--sidebar-shows-clock-p hidden)))
              (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
                (should-not (ygg-aob--sidebar-shows-clock-p drawn))))
          (kill-buffer ygg-projects-buffer-name))))))

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

(ert-deftest aob-compose-diff-reads-the-target-sessions-own-worktree ()
  (dolist (name '(ygg-aob--repo ygg-aob--draft-target ygg-aob--diff-dir
                  ygg-aob--worktree-line ygg-aob--expand-diff))
    (aob-tests--host-defun name))
  (defvar ygg-aob-diff-max-chars)
  (let* ((ygg-aob-diff-max-chars 60000)
         (root (file-name-as-directory (file-truename (make-temp-file "aob-diff" t))))
         (main (file-name-as-directory (expand-file-name "main" root)))
         (tree (file-name-as-directory (expand-file-name "feat-x" root)))
         (git (lambda (dir &rest args)
                (let ((default-directory dir))
                  (should (eq 0 (apply #'call-process "git" nil nil nil args))))))
         (in-tree (aob-create-session :id "acp:diff:1" :backend 'acp :name "diff"
                                      :project main :dir tree :state 'idle))
         (on-main (aob-create-session :id "acp:diff:2" :backend 'acp :name "diff"
                                      :project main :state 'idle))
         (expand (lambda (target)
                   (with-temp-buffer
                     (setq default-directory main)
                     (setq-local aob-compose--target target)
                     (ygg-aob--expand-diff "look @diff")))))
    (unwind-protect
        (progn
          (make-directory main)
          (funcall git main "init" "-q" "-b" "trunk")
          (with-temp-file (expand-file-name "a.txt" main) (insert "one\n"))
          (funcall git main "add" "a.txt")
          (funcall git main "-c" "user.email=t@t" "-c" "user.name=t"
                   "commit" "-q" "-m" "0")
          (aob-tests--link-worktree main tree "feat/x")
          (with-temp-file (expand-file-name "a.txt" tree) (insert "two\n"))
          (let ((out (funcall expand "acp:diff:1")))
            (should (string-match-p "<diff>\nworktree feat-x · branch feat/x\n" out))
            (should (string-match-p "^\\+two$" out)))
          (should-not (funcall expand "acp:diff:2"))
          (should-not (funcall expand nil))
          (with-temp-file (expand-file-name "a.txt" main) (insert "three\n"))
          (let ((out (funcall expand "acp:diff:2")))
            (should (string-match-p "<diff>\nworktree main · branch trunk\n" out))
            (should (string-match-p "^\\+three$" out))
            (should-not (string-match-p "two" out))))
      (aob-remove-session in-tree)
      (aob-remove-session on-main)
      (delete-directory root t))))

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
(declare-function ygg-aob--space-of "layer-aob" (s))
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

(defmacro aob-tests--with-default-name (var &rest body)
  "VAR is a fake ACP session on claude, still called claude:9."
  (declare (indent 1))
  `(aob-tests--with-session ,var
     (setf (aob-session-name ,var) "claude:9")
     (aob-session-put ,var :agent "claude")
     (cl-letf (((symbol-function 'aob-acp--request) #'ignore))
       ,@body)))

(defun aob-tests--info-update (s update)
  (aob-tests--feed s (json-encode
                      `((jsonrpc . "2.0") (method . "session/update")
                        (params (sessionId . "sess-test")
                                (update . ,(cons '(sessionUpdate . "session_info_update")
                                                 update)))))))

(ert-deftest aob-auto-name-a-default-session-is-named-from-its-first-prompt ()
  (aob-tests--with-default-name s
    (aob-acp--prompt-1 s "Fix the JDT handler so it stops dropping diagnostics on save")
    (should (equal (aob-session-name s) "claude: Fix the JDT handler so it stops"))
    (should (equal (aob-session-id s) "acp:test:1"))
    (aob-set-state s 'idle)
    (aob-acp--prompt-1 s "and now something else")
    (should (equal (aob-session-name s) "claude: Fix the JDT handler so it stops"))))

(ert-deftest aob-auto-name-counts-a-dated-reading-name-as-default ()
  (aob-tests--with-default-name s
    (dolist (name '("claude:6 · Sep 23" "claude:6 · Sep 23 ac1e"))
      (setf (aob-session-name s) name)
      (should (aob-acp--default-name-p s)))
    (setf (aob-session-name s) "vbet · Sep 23")
    (should-not (aob-acp--default-name-p s))))

(ert-deftest aob-auto-name-leaves-a-name-the-user-gave-alone ()
  (aob-tests--with-default-name s
    (aob-rename-session s "reconnect race")
    (aob-acp--prompt-1 s "fix the jdt handler")
    (aob-tests--info-update s '((title . "Agent title")))
    (should (equal (aob-session-name s) "reconnect race")))
  (aob-tests--with-default-name s
    (aob-session-put s :named-by-user t)
    (aob-acp--prompt-1 s "fix the jdt handler")
    (should (equal (aob-session-name s) "claude:9"))))

(ert-deftest aob-auto-name-leaves-a-name-that-is-not-a-default-alone ()
  "A name from before the flag existed, or numbered off a task, says
something already."
  (aob-tests--with-default-name s
    (setf (aob-session-name s) "fix-the-race:2")
    (aob-acp--prompt-1 s "fix the jdt handler")
    (should (equal (aob-session-name s) "fix-the-race:2"))))

(ert-deftest aob-auto-name-an-agent-title-wins-over-the-prompt ()
  (aob-tests--with-default-name s
    (aob-acp--prompt-1 s "fix the jdt handler")
    (should (equal (aob-session-name s) "claude: fix the jdt handler"))
    (aob-tests--info-update s '((title . "JDT diagnostics on save")))
    (should (equal (aob-session-name s) "claude: JDT diagnostics on save"))
    (aob-tests--info-update s '((_meta (goal (objective . "ship the handler")))))
    (should (equal (aob-session-name s) "claude: JDT diagnostics on save"))))

(ert-deftest aob-auto-name-uses-the-goal-over-the-prompt ()
  (aob-tests--with-default-name s
    (aob-acp--prompt-1 s "fix the jdt handler")
    (aob-tests--info-update s '((_meta (goal (objective . "Ship the JDT handler")))))
    (should (equal (aob-session-name s) "claude: Ship the JDT handler"))
    (aob-set-state s 'idle)
    (aob-acp--prompt-1 s "something unrelated")
    (should (equal (aob-session-name s) "claude: Ship the JDT handler"))))

(ert-deftest aob-auto-name-is-unique-among-live-sessions ()
  (let ((twin (aob-create-session :id "acp:twin:1" :backend 'acp
                                  :name "claude: fix the jdt handler"
                                  :state 'idle)))
    (unwind-protect
        (aob-tests--with-default-name s
          (aob-acp--prompt-1 s "fix the jdt handler")
          (should (equal (aob-session-name s) "claude: fix the jdt handler 2"))
          (aob-tests--info-update s '((title . "fix the jdt handler")))
          (should (equal (aob-session-name s) "claude: fix the jdt handler 2")))
      (aob-remove-session twin))))

(ert-deftest aob-auto-name-strips-what-is-not-words ()
  (should (equal (aob-acp--name-from-text
                  "/review @lisp/aob.el [[Image1]] \"**fix** the \x60jdt\x60 handler\"")
                 "fix the jdt handler"))
  (should (equal (aob-acp--name-from-text
                  "<context region a.el>\n(defun x ())\n</context>\n\n/clear\n\n## Don't   drop [the](http://x) diagnostics!")
                 "Don't drop the diagnostics"))
  (should (equal (aob-acp--name-from-text "mail me at foo@bar.com please")
                 "mail me at foo@bar.com please"))
  (should (equal (aob-acp--name-from-text
                  "make the sidebar remember which projects were folded open")
                 "make the sidebar remember which"))
  (should (equal (aob-acp--name-from-text "/Users/x/foo.el crashes on load")
                 "Users/x/foo.el crashes on load"))
  (should (equal (aob-acp--name-from-text
                  (concat "<context a.el>\n" (make-string 200000 ?x)
                          "\n</context>\nfix it"))
                 "fix it"))
  (should-not (aob-acp--name-from-text
               "\n\n<preset name=\"review\">\nReview the diff.\n</preset>"))
  (should-not (aob-acp--name-from-text "/compact"))
  (should-not (aob-acp--name-from-text "[[Image2]] @a.png")))

(ert-deftest aob-auto-name-an-explicit-spawn-name-counts-as-given ()
  (let (s)
    (unwind-protect
        (cl-letf (((symbol-function 'aob-acp--project) (lambda () "/tmp/"))
                  ((symbol-function 'aob-acp--open)
                   (lambda (_agent name &rest _)
                     (setq s (aob-create-session :id (concat "acp:" name)
                                                 :backend 'acp :name name
                                                 :state 'starting))))
                  (aob-acp-show-trace nil))
          (aob-acp-spawn "claude" nil nil "reviewer")
          (should (aob-session-ref s :named-by-user)))
      (when s (aob-remove-session s)))))

(ert-deftest aob-auto-name-flags-survive-a-restore ()
  (let ((s (aob-create-session :id "acp:persist:1" :backend 'acp
                               :name "claude: fix the jdt handler" :state 'idle
                               :refs (list :agent "claude" :acp-id "x"
                                           :named-by-user t :auto-named 'prompt)))
        refs)
    (unwind-protect
        (let ((e (aob-acp--entry s)))
          (should (eq (plist-get e :named-by-user) t))
          (should (eq (plist-get e :auto-named) 'prompt))
          (setq e (plist-put e :dir "/tmp/"))
          (setq e (read (prin1-to-string e)))
          (aob-remove-session s)
          (cl-letf (((symbol-function 'aob-acp--open)
                     (lambda (&rest _) (setq refs aob-acp-session-refs) nil)))
            (aob-acp-resume-entry e))
          (should (eq (plist-get refs :named-by-user) t))
          (should (eq (plist-get refs :auto-named) 'prompt)))
      (when (aob-session-get "acp:persist:1") (aob-remove-session s)))))

(ert-deftest aob-worktree-porcelain-names-each-checked-out-tree ()
  (should (equal (aob-acp--parse-worktrees
                  (concat "worktree /r/main\nHEAD 1\nbranch refs/heads/main\n\n"
                          "worktree /r/feat\nHEAD 2\nbranch refs/heads/feat/x\n\n"
                          "worktree /r/loose\nHEAD 3\ndetached\n\n"
                          "worktree /r/bare\nbare\n\n"
                          "worktree /r/gone\nHEAD 4\nbranch refs/heads/g\nprunable gitdir file points to non-existent location\n"))
                 '(("/r/main/" . "main") ("/r/feat/" . "feat/x") ("/r/loose/")))))

(defun aob-tests--git (dir &rest args)
  (with-temp-buffer
    (unless (eq 0 (apply #'call-process "git" nil t nil "-C" dir
                         "-c" "user.email=t@t" "-c" "user.name=t" args))
      (error "git %S: %s" args (buffer-string)))))

(defun aob-tests--link-worktree (main tree branch)
  "Lay out TREE as a linked worktree of the repository MAIN on a new BRANCH,
the files git itself leaves on disk for one, without asking git to make it.
MAIN\='s index goes with it, so the tree starts out clean."
  (let* ((main (file-name-as-directory (expand-file-name main)))
         (tree (directory-file-name (expand-file-name tree)))
         (admin (expand-file-name (concat ".git/worktrees/" (file-name-nondirectory tree))
                                  main))
         (write (lambda (file text)
                  (make-directory (file-name-directory file) t)
                  (with-temp-file file (insert text)))))
    (funcall write (expand-file-name (concat ".git/refs/heads/" branch) main)
             (with-temp-buffer
               (call-process "git" nil t nil "-C" main "rev-parse" "HEAD")
               (buffer-string)))
    (funcall write (expand-file-name "gitdir" admin) (concat tree "/.git\n"))
    (funcall write (expand-file-name "commondir" admin) "../..\n")
    (funcall write (expand-file-name "HEAD" admin) (format "ref: refs/heads/%s\n" branch))
    (when (file-exists-p (expand-file-name ".git/index" main))
      (copy-file (expand-file-name ".git/index" main) (expand-file-name "index" admin)))
    (funcall write (expand-file-name ".git" tree) (format "gitdir: %s\n" admin))))

(defun aob-tests--linking-worktree-make (project dir done &optional branch)
  "`aob-acp--worktree-make\=', laying DIR out by hand rather than asking git."
  (aob-tests--link-worktree project dir
                            (or branch (concat "aob/" (file-name-nondirectory dir))))
  (funcall done nil))

(defmacro aob-tests--with-trees (repo second &rest body)
  "REPO is a scratch repository; SECOND its linked worktree, or nil for none."
  (declare (indent 2))
  `(let* ((,repo (file-name-as-directory (file-truename (make-temp-file "aob-trees" t))))
          (,second nil))
     (unwind-protect
         (progn
           (aob-tests--git ,repo "init" "-q")
           (aob-tests--git ,repo "commit" "--allow-empty" "-q" "-m" "x")
           (ignore ,second)
           ,@body)
       (dolist (w (aob-acp--worktrees ,repo))
         (unless (equal (file-truename (car w)) ,repo)
           (ignore-errors (delete-directory (car w) t))
           (let ((parent (file-name-directory (directory-file-name (car w)))))
             (when (string-prefix-p "aob-tree-home"
                                    (file-name-nondirectory (directory-file-name parent)))
               (ignore-errors (delete-directory parent t))))))
       (delete-directory ,repo t))))

(defun aob-tests--add-tree (repo name)
  (let ((dir (file-name-as-directory
              (file-truename (expand-file-name name (make-temp-file "aob-tree-home" t))))))
    (aob-tests--link-worktree repo dir name)
    dir))

(defun aob-tests--no-asking (&rest args)
  (error "asked: %S" (car args)))

(ert-deftest aob-worktree-choice-only-when-there-is-one-to-make ()
  (let ((plain (make-temp-file "aob-nogit" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read) #'aob-tests--no-asking)
                  ((symbol-function 'read-string) #'aob-tests--no-asking))
          (should-not (aob-acp-read-worktree plain))
          (aob-tests--with-trees repo second
            (should-not (aob-acp-read-worktree repo))
            (should (equal (mapcar #'car (aob-acp--worktrees repo)) (list repo)))
            (setq second (aob-tests--add-tree repo "second"))
            (should (= (length (aob-acp--worktree-choices repo)) 2))
            (should-error (aob-acp-read-worktree repo))))
      (delete-directory plain t))))

(defun aob-tests--spawn-cwd (start tree &optional agent)
  "Spawn AGENT, else claude, from START in TREE; answer (CWD DIR PROJECT)."
  (let ((aob-acp-start-dir start)
        (aob-acp-show-trace nil)
        spec s)
    (cl-letf (((symbol-function 'aob-acp--connect)
               (lambda (_s open _then) (setq spec (funcall open '(:agentInfo (:name "x")))))))
      (setq s (aob-acp-spawn (or agent "claude") nil nil nil tree))
      (let ((deadline (+ (float-time) 15)))
        (while (and (not spec) (< (float-time) deadline))
          (accept-process-output nil 0.05))))
    (unwind-protect
        (list (plist-get (cadr spec) :cwd) (aob-session-dir s) (aob-session-project s))
      (aob-remove-session s))))

(ert-deftest aob-a-picked-worktree-is-where-the-session-starts ()
  (aob-tests--with-trees repo second
    (setq second (aob-tests--add-tree repo "second"))
    (should (equal (aob-tests--spawn-cwd repo second)
                   (list (directory-file-name second) second repo)))
    (should (equal (aob-tests--spawn-cwd repo nil)
                   (list (directory-file-name repo) repo repo)))
    (let* ((aob-acp-worktree-root (file-truename (make-temp-file "aob-wt-root" t)))
           (fresh (file-name-as-directory (expand-file-name "made" aob-acp-worktree-root))))
      (unwind-protect
          (cl-letf (((symbol-function 'aob-acp--worktree-make)
                     #'aob-tests--linking-worktree-make))
            (should (equal (car (aob-tests--spawn-cwd repo (cons fresh "topic/new")))
                           (directory-file-name fresh)))
            (should (file-directory-p fresh))
            (should (equal (cdr (assoc fresh (aob-acp--worktrees repo))) "topic/new")))
        (delete-directory aob-acp-worktree-root t)))))

(ert-deftest aob-spawn-prompt-asks-no-worktree-in-a-single-tree-repo ()
  (aob-tests--with-trees repo second
    (let ((aob-acp-start-dir repo)
          (aob-acp-show-trace nil)
          spec s)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-prefix-p "ACP agent" prompt) "claude"
                     (error "asked: %s" prompt))))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'aob-acp--connect)
                 (lambda (_s open _then) (setq spec (funcall open nil)))))
        (setq s (call-interactively #'aob-acp-spawn)))
      (unwind-protect
          (should (equal (plist-get (cadr spec) :cwd) (directory-file-name repo)))
        (aob-remove-session s)))))

(defmacro aob-tests--add-folder (var init &rest body)
  "VAR is an idle claude session over INIT whose requests land in SENT."
  (declare (indent 2))
  `(let ((home (file-name-as-directory (file-truename (make-temp-file "aob-home" t))))
         (extra (file-name-as-directory (file-truename (make-temp-file "aob-extra" t))))
         (aob-acp-show-trace nil)
         sent)
     (aob-tests--with-session ,var
       (setf (aob-session-dir ,var) home (aob-session-project ,var) home)
       (aob-session-put ,var :agent "claude")
       (aob-session-put ,var :acp-id "sess-test")
       (aob-session-put ,var :want-tools '("Read"))
       (aob-acp--with-limits ,var ,init "session/new" nil)
       (aob-set-state ,var 'idle)
       (process-put (aob-session-conn ,var) 'aob-init (list 'done ,init))
       (let ((proc (aob-session-conn ,var)))
         (unwind-protect
             (cl-letf (((symbol-function 'aob-acp--request)
                        (lambda (_s method params &rest _) (push (cons method params) sent)))
                       ((symbol-function 'aob-acp--notify) #'ignore)
                       ((symbol-function 'aob-acp--live-conn) (lambda (&rest _) proc))
                       ((symbol-function 'aob-acp--conn-cleanup) #'ignore)
                       ((symbol-function 'aob-acp--seed-history) #'ignore))
               ,@body)
           (when-let* ((again (aob-session-get "acp:test:1")))
             (unless (eq again ,var) (aob-remove-session again)))
           (delete-directory home t)
           (delete-directory extra t))))))

(ert-deftest aob-add-folder-resumes-with-it-and-keeps-limits ()
  (let ((init '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp")
                :agentCapabilities (:loadSession t :sessionCapabilities
                                    (:additionalDirectories nil :resume nil :close nil)))))
    (aob-tests--add-folder s init
      (aob-acp-add-folder s extra)
      (let* ((resume (cdr (assoc "session/resume" sent)))
             (wire (json-parse-string (json-serialize resume)
                                      :object-type 'plist :array-type 'list)))
        (should (assoc "session/close" sent))
        (should (equal (plist-get wire :sessionId) "sess-test"))
        (should (equal (plist-get wire :cwd) (directory-file-name home)))
        (should (equal (plist-get wire :additionalDirectories)
                       (list (directory-file-name extra))))
        (should (equal (plist-get (plist-get (plist-get (plist-get wire :_meta) :claudeCode)
                                             :options)
                                  :tools)
                       '("Read")))
        (should (equal (aob-session-ref (aob-session-get "acp:test:1") :extra-dirs)
                       (list extra)))))))

(ert-deftest aob-add-folder-refuses-an-agent-without-additional-directories ()
  (let ((init '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp")
                :agentCapabilities (:loadSession t :sessionCapabilities (:resume nil)))))
    (aob-tests--add-folder s init
      (should-error (aob-acp-add-folder s extra) :type 'user-error)
      (should-not sent)
      (should-not (aob-session-ref s :extra-dirs))
      (should (eq (aob-session-get "acp:test:1") s)))))

(ert-deftest aob-spawn-with-asks-a-worktree-only-when-there-are-two ()
  (aob-tests--with-trees repo second
    (let ((aob-acp-start-dir repo) asked trees)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _) (push prompt asked) "claude"))
                ((symbol-function 'aob-compose)
                 (lambda (target &rest _) (funcall target "hi" nil) (current-buffer)))
                ((symbol-function 'aob-acp--spawn-with-1)
                 (lambda (&rest args) (push (nth 5 args) trees))))
        (call-interactively #'aob-acp-spawn-with)
        (should (equal asked '("Preset: ")))
        (should (equal trees '(nil)))
        (setq second (aob-tests--add-tree repo "second") asked nil)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt coll &rest _)
                     (push prompt asked)
                     (if (equal prompt "Worktree: ")
                         (seq-find (lambda (c) (string-search "second" c)) coll)
                       "claude"))))
          (call-interactively #'aob-acp-spawn-with)
          (should (equal (car trees) second))
          (should (equal (reverse asked) '("Preset: " "Worktree: "))))))))

(ert-deftest aob-compose-offers-a-worktree-only-when-there-are-two ()
  (aob-tests--modal
   '(aob-tests--with-trees repo second
      (let ((aob-acp-show-trace nil) asked)
        (with-temp-buffer
          (aob-compose-mode)
          (setq default-directory repo aob-compose--dir repo)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt coll &rest _)
                       (push (cons prompt (and (listp coll) coll)) asked)
                       "claude")))
            (ygg-compose-transient))
          (unless (and (= (length asked) 1)
                       (not (member "worktree" (cdar asked))))
            (error "single tree asked %S" asked))
          (setq second (aob-tests--add-tree repo "second") asked nil)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt coll &rest _)
                       (push prompt asked)
                       (if (equal prompt "Worktree: ")
                           (seq-find (lambda (c) (string-search "second" c)) coll)
                         "worktree"))))
            (ygg-compose-transient))
          (unless (equal ygg-aob--draft-tree second)
            (error "draft tree %S" ygg-aob--draft-tree))
          (unless (member "⌥ second" aob-compose--tags)
            (error "tags %S" aob-compose--tags))
          (let (spec s)
            (cl-letf (((symbol-function 'aob-acp--connect)
                       (lambda (_s open _then) (setq spec (funcall open nil)))))
              (setq s (funcall aob-compose-spawn-function "hi" "claude")))
            (unwind-protect
                (unless (and (equal (plist-get (cadr spec) :cwd) (directory-file-name second))
                             (equal (aob-session-dir s) second)
                             (equal (aob-session-project s) repo))
                  (error "spawned %S in %S" (cadr spec) (aob-session-dir s)))
              (aob-remove-session s))))))))

(ert-deftest aob-picking-the-own-tree-is-no-choice-at-all ()
  (aob-tests--with-trees repo second
    (setq second (aob-tests--add-tree repo "second"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt coll &rest _)
                 (seq-find (lambda (c) (string-search "master" c)) coll))))
      (should-not (aob-acp-read-worktree repo))
      (should (equal (aob-acp-read-worktree second) repo)))))

(ert-deftest aob-an-isolated-preset-keeps-its-own-worktree ()
  (aob-tests--with-trees repo second
    (setq second (aob-tests--add-tree repo "second"))
    (let ((aob-acp-worktree-root (file-truename (make-temp-file "aob-wt-root" t))))
      (unwind-protect
          (cl-letf (((symbol-function 'aob-acp--worktree-make)
                     #'aob-tests--linking-worktree-make))
            (pcase-let ((`(,_ ,dir ,_) (aob-tests--spawn-cwd repo second "claude-isolated")))
              (should (string-prefix-p aob-acp-worktree-root dir))))
        (delete-directory aob-acp-worktree-root t)))))

(ert-deftest aob-a-worktree-another-session-works-in-is-not-reaped ()
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "aob-wt-root" t))))
         (aob-acp-worktree-root root)
         (dir (file-name-as-directory (expand-file-name "shared" root)))
         (a (aob-create-session :id "acp:reap-a" :backend 'acp :name "reap-a"
                                :project "/tmp/" :dir dir :state 'idle))
         (b (aob-create-session :id "acp:reap-b" :backend 'acp :name "reap-b"
                                :project "/tmp/" :dir dir :state 'idle))
         asked)
    (make-directory dir t)
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-git-async) (lambda (&rest _) (setq asked t))))
          (aob-acp--reap-worktree a)
          (should-not asked)
          (aob-remove-session b)
          (aob-acp--reap-worktree a)
          (should asked))
      (aob-remove-session a)
      (when (aob-session-get "acp:reap-b") (aob-remove-session b))
      (delete-directory root t))))

(ert-deftest aob-add-folder-forgets-it-when-the-restart-fails ()
  (let ((init '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp")
                :agentCapabilities (:loadSession t :sessionCapabilities
                                    (:additionalDirectories nil :resume nil)))))
    (aob-tests--add-folder s init
      (cl-letf (((symbol-function 'aob-acp-restart)
                 (lambda (&rest _) (user-error "gone"))))
        (should-error (aob-acp-add-folder s extra) :type 'user-error))
      (should-not (aob-session-ref s :extra-dirs)))))

(ert-deftest aob-trace-names-its-task-once ()
  "The task is the header line's; the mode line calls the buffer a trace."
  (aob-tests--with-trace-session s
    (with-current-buffer (aob-trace-buffer s)
      (should (equal ygg-modeline-name "trace")))))

(ert-deftest aob-trace-marks-sit-level-with-their-text ()
  "A mark is drawn from the top of its row, padded to the middle of a
line of text, so the gap a row's newline carries stays below it."
  (should (equal (aob-trace--mark-bits [1 2] 8) [0 0 0 1 2]))
  (should (equal (aob-trace--mark-bits [1 2 3] 2) [1 2 3]))
  (let ((aob-trace--marks-line nil) (defined nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'frame-char-height) (lambda (&rest _) 17))
              ((symbol-function 'define-fringe-bitmap)
               (lambda (name bits _h _w align) (push (list name bits align) defined))))
      (aob-trace--define-marks)
      (should (= (length defined) (length aob-trace--marks)))
      (dolist (d defined)
        (should (eq (nth 2 d) 'top))
        (should (zerop (aref (nth 1 d) 0))))
      (setq defined nil)
      (aob-trace--define-marks)
      (should-not defined))))

(ert-deftest aob-trace-gutter-cells-are-one-width ()
  "Every mark in the gutter takes the same cell and keeps the same gap
before the text: fringe marks draw inside one eight-pixel width and keep
off its two columns nearest the text, row glyphs are no bigger than the
text, a narrow window keeps a margin for the speaker's mark, a thought
mid-turn carries no glyph of its own, nor does any other delta row or
the folded reads, which show no clock either, the agent's mark is no
wider than one cell, wrapped command output hangs under the command,
and copying a line leaves its mark behind."
  (dolist (m aob-trace--marks)
    (should (vectorp (nth 4 m)))
    (seq-doseq (row (nth 4 m))
      (should (< row 256))
      (should (zerop (logand row 3)))))
  (should (eq 'unspecified (face-attribute 'aob-trace-icon :height nil t)))
  (should (<= (car (aob-trace--agent-size 27 8)) 8))
  (should (<= (cdr (aob-trace--agent-size 27 8)) 27))
  (should (equal (aob-trace--agent-size 6 8) '(7 . 6)))
  (dolist (style '(delta log))
    (let ((aob-trace-style style) (aob-trace-icons nil))
      (aob-tests--with-trace-session s
        (dolist (ev (list (list :type 'error :text "boom" :seq 1)
                          (list :type 'stop :stopReason "end_turn" :seq 2)
                          (list :type 'permission :title "rm" :seq 3)
                          (list :type 'plan :seq 4)
                          (list :type 'error :text "boom" :seq 5 :turn-head t)))
          (let ((line (aob-trace--plain-line ev "00:00:00")))
            (should (eq (eq style 'log)
                        (string-prefix-p (concat (aob-trace--glyph ev) " ") line)))
            (should-not (string-search "■ rm" line)))))))
  (let ((aob-trace-style 'delta))
    (aob-tests--with-trace-session s
      (aob-tests--say s "Looking.")
      (aob-tests--think s "which file")
      (aob-tests--tool s "x1" "execute" "`pwd`" "completed"
                       :content (aob-tests--output "/tmp/proj"))
      (dolist (file '("lib/a.dart" "lib/b.dart" "lib/c.dart"))
        (aob-tests--read s file file "completed"))
      (let ((win (selected-window)))
        (delete-other-windows)
        (set-window-buffer win (current-buffer))
        (aob-trace--fit-margins)
        (should (> (or (car (window-margins win)) 0) 0))
        (select-window (split-window-right))
        (set-window-buffer (selected-window) (current-buffer))
        (aob-trace--fit-margins)
        (should (< (window-total-width) 60))
        (should (> (or (car (window-margins)) 0) 0))
        (delete-other-windows))
      (aob-trace--render t)
      (goto-char (point-min))
      (search-forward "thinking ·")
      (should (= (match-beginning 0) (line-beginning-position)))
      (goto-char (point-min))
      (search-forward aob-trace-explore-heading)
      (should (string-match-p (concat "\\`" aob-trace-explore-heading)
                              (aob-trace--unmarked
                               (buffer-substring (line-beginning-position) (line-end-position)))))
      (let ((wrap (aob-tests--at "$ pwd" 'wrap-prefix)))
        (should (= (string-width wrap) (string-width "$ ")))
        (should (memq 'aob-trace-small (ensure-list (get-text-property 0 'face wrap)))))
      (should (text-property-any (point-min) (point-max) 'aob-status 'done))
      (let ((copied (filter-buffer-substring (point-min) (point-max))))
        (should (string-search "$ pwd" copied))
        (should-not (text-property-any 0 (length copied) 'aob-status 'done copied))))))

(defconst aob-tests--full-brief
  (mapconcat #'identity
             '("GOAL" "Count the files under src." "SCOPE" "Writes nothing."
               "CONTEXT" "src/" "ACCEPTANCE" "A number." "VERIFY" "find src | wc -l"
               "REPORT" "The count and the command.")
             "\n"))

(defun aob-tests--send-agent (s id prompt)
  "Feed S a claude Agent call ID whose prompt is PROMPT."
  (aob-tests--feed
   s (json-encode
      `((jsonrpc . "2.0") (method . "session/update")
        (params . ((update . ((sessionUpdate . "tool_call") (toolCallId . ,id)
                              (title . ,id) (kind . "think") (status . "in_progress")
                              (rawInput . ((description . ,id) (prompt . ,prompt)
                                           (subagent_type . "general-purpose")))
                              (_meta . ((claudeCode . ((toolName . "Agent")
                                                       (subagent . t)))))))))))))

(defun aob-tests--notices (s)
  (delq nil (mapcar (lambda (e) (and (eq (plist-get e :type) 'state)
                                     (plist-get e :title)))
                    (aob-session-events s))))

(ert-deftest aob-orch-cap-stored-from-preset ()
  "The lead preset named in a draft gives its session a cap, briefs and workers."
  (require 'ygg-preset)
  (aob-tests--host-defun 'ygg-aob--preset-limits)
  (let* ((ygg-preset-config-directory
          (expand-file-name "../../presets/" (file-name-directory aob-tests--file)))
         (ygg-preset-user-directory (make-temp-name "/tmp/aob-tests-no-presets-")))
    (cl-letf (((symbol-function 'ygg-aob--presets-of)
               (lambda (_dir) (cons nil (ygg-preset-list)))))
      (let* ((refs (ygg-aob--preset-limits
                    "run it\n\n<preset name=\"lead\">\nbody\n</preset>"))
             (search (ygg-aob--preset-limits
                      "find it\n\n<preset name=\"search\">\nbody\n</preset>"))
             (s (aob-create-session :id "acp:orch:cap" :backend 'acp :name "orch"
                                    :project "/tmp/proj/" :dir "/tmp/proj/"
                                    :state 'idle :refs refs)))
        (unwind-protect
            (progn
              (should (eql 6 (aob-session-ref s :subagent-cap)))
              (should (aob-session-ref s :subagent-briefs))
              (should (equal "build" (plist-get (car (aob-session-ref s :workers)) :name)))
              (should (string-prefix-p "# Build" (aob-session-ref s :worker-prompt)))
              (should (eql 0 (plist-get search :subagent-cap)))
              (should-not (plist-get search :subagent-briefs))
              (should-not (plist-get search :workers)))
          (aob-remove-session s))))))

(ert-deftest aob-orch-live-count ()
  "Only the subagents still working count as live."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-tests--send-agent s "toolu_1" aob-tests--full-brief)
          (aob-tests--send-agent s "toolu_2" aob-tests--full-brief)
          (should (= 2 (aob-subagent-live-count s)))
          (aob-set-state (aob-session-get (format "%s/toolu_1" (aob-session-id s))) 'done)
          (should (= 1 (aob-subagent-live-count s))))
      (aob-tests--kill-views))))

(ert-deftest aob-orch-warns-over-cap ()
  "A subagent past the cap leaves a notice in the sender's trace."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-session-put s :subagent-cap 1)
          (aob-tests--send-agent s "toolu_1" aob-tests--full-brief)
          (should-not (seq-find (lambda (n) (string-match-p "cap passed" n))
                                (aob-tests--notices s)))
          (aob-tests--send-agent s "toolu_2" aob-tests--full-brief)
          (should (member "subagent cap passed: 2 working, cap 1"
                          (aob-tests--notices s)))
          (with-current-buffer (aob-trace-buffer s)
            (aob-trace--render t)
            (should (string-match-p "subagent cap passed" (buffer-string)))))
      (aob-tests--kill-views))))

(ert-deftest aob-orch-warns-missing-brief-headings ()
  "A brief without its headings is named in the sender's trace."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-session-put s :subagent-cap 6)
          (aob-session-put s :subagent-briefs t)
          (aob-tests--send-agent s "toolu_1" "GOAL\nDo it.\nscope\nall\nVERIFY\nmake")
          (should (member "brief for toolu_1 lacks SCOPE CONTEXT ACCEPTANCE REPORT"
                          (aob-tests--notices s))))
      (aob-tests--kill-views))))

(ert-deftest aob-orch-no-warning-for-complete-brief-or-uncapped ()
  "A full brief warns nothing, and a session with no cap is never checked."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-session-put s :subagent-cap 6)
          (aob-session-put s :subagent-briefs t)
          (aob-tests--send-agent s "toolu_1" aob-tests--full-brief)
          (should-not (aob-tests--notices s)))
      (aob-tests--kill-views)))
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (dotimes (i 3)
            (aob-tests--send-agent s (format "toolu_%d" i) "just count the files"))
          (should (= 3 (aob-subagent-live-count s)))
          (should-not (aob-tests--notices s)))
      (aob-tests--kill-views))))

(ert-deftest aob-orch-subagent-space-follows-lead ()
  "A subagent's buffers go to the space of the session at the head of its chain."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-session-put s :space 7)
          (aob-tests--send-agent s "toolu_1" aob-tests--full-brief)
          (let* ((kid (aob-session-get (format "%s/toolu_1" (aob-session-id s))))
                 (grand (aob-create-session :id "acp:orch:grand" :backend 'native-subagent
                                            :name "grand" :project "/tmp/proj/"
                                            :dir "/tmp/proj/" :state 'working
                                            :refs (list :parent-session (aob-session-id kid)
                                                        :space 99))))
            (unwind-protect
                (progn
                  (should (eq s (aob-subagent-lead grand)))
                  (should (eq s (aob-subagent-lead s)))
                  (should (eql 7 (aob-session-ref (aob-subagent-lead grand) :space)))
                  (aob-tests--host-defun 'ygg-aob--space-of)
                  (should (eql 7 (ygg-aob--space-of grand))))
              (aob-remove-session grand))))
      (aob-tests--kill-views))))

(defconst aob-tests--claude-init
  '(:agentInfo (:name "@agentclientprotocol/claude-agent-acp")))

(defun aob-tests--agents-sent (init refs)
  "The agents option a session/new under INIT with REFS sends, or :none."
  (pcase-let* ((`(,wire ,_ ,_) (aob-tests--opened-with init refs))
               (cc (plist-get (plist-get (plist-get wire :params) :_meta) :claudeCode)))
    (if (plist-member (plist-get cc :options) :agents)
        (plist-get (plist-get cc :options) :agents)
      :none)))

(ert-deftest aob-orch-effort-agents-map-per-level ()
  "Each worker level reaches Claude as an agent with its model, effort and prompt."
  (let ((agents (aob-tests--agents-sent
                 aob-tests--claude-init
                 '(:want-thinking "high" :worker-prompt "Build it."
                   :workers ((:name "build") (:name "quick" :model "sonnet" :effort "low")
                             (:name "deep" :model "opus" :effort "xhigh"))))))
    (should (equal "opus" (plist-get (plist-get agents :worker-build) :model)))
    (should (equal "medium" (plist-get (plist-get agents :worker-build) :effort)))
    (should (equal "sonnet" (plist-get (plist-get agents :worker-quick) :model)))
    (should (equal "low" (plist-get (plist-get agents :worker-quick) :effort)))
    (should (equal "xhigh" (plist-get (plist-get agents :worker-deep) :effort)))
    (should (equal "Build it." (plist-get (plist-get agents :worker-quick) :prompt)))
    (should (stringp (plist-get (plist-get agents :worker-deep) :description))))
  (let ((agents (aob-tests--agents-sent
                 aob-tests--claude-init
                 '(:workers ((:name "quick" :model "sonnet" :effort "low"))))))
    (should (equal "opus" (plist-get (plist-get agents :worker-build) :model)))
    (should (equal "medium" (plist-get (plist-get agents :worker-build) :effort)))))

(ert-deftest aob-orch-effort-override-wins ()
  "The owner's worker effort is every level's, the derived one included."
  (let ((agents (aob-tests--agents-sent
                 aob-tests--claude-init
                 '(:want-thinking "high" :worker-effort "max"
                   :workers ((:name "build") (:name "quick" :model "sonnet" :effort "low"))))))
    (should (equal "max" (plist-get (plist-get agents :worker-build) :effort)))
    (should (equal "max" (plist-get (plist-get agents :worker-quick) :effort))))
  (aob-tests--with-session s
    (aob-session-put s :limits '(:workers ((:name "build"))))
    (aob-acp-worker-effort s "xhigh")
    (should (equal "xhigh" (aob-session-ref s :worker-effort)))
    (should (equal "xhigh" (plist-get (aob-session-ref s :limits) :worker-effort)))
    (aob-acp-worker-effort s "none")
    (should-not (aob-session-ref s :worker-effort))))

(ert-deftest aob-orch-effort-none-without-workers-or-claude ()
  "A preset naming no workers sends no agents, and no other adapter gets any."
  (should (eq :none (aob-tests--agents-sent aob-tests--claude-init
                                            '(:want-thinking "high"))))
  (pcase-let ((`(,wire ,_ ,_) (aob-tests--opened-with
                               '(:agentInfo (:name "codex-acp"))
                               '(:workers ((:name "build"))))))
    (should-not (plist-member (plist-get (plist-get wire :params) :_meta) :claudeCode))))

(ert-deftest aob-orch-effort-below-lead-steps-down-one ()
  "The build worker runs one effort under its lead: xhigh high, high medium, low low."
  (should (equal "high" (aob-acp--effort-below "xhigh")))
  (should (equal "medium" (aob-acp--effort-below "high")))
  (should (equal "low" (aob-acp--effort-below "low")))
  (should (equal "xhigh" (aob-acp--effort-below "max")))
  (should (equal "medium" (aob-acp--effort-below nil)))
  (should (equal "low" (plist-get (plist-get (aob-tests--agents-sent
                                              aob-tests--claude-init
                                              '(:want-thinking "low"
                                                :workers ((:name "build"))))
                                             :worker-build)
                                  :effort)))
  (aob-tests--with-session s
    (aob-session-put s :config-options
                     '((:id "effort" :category "thought_level" :currentValue "xhigh")))
    (should (equal "high" (aob-acp--effort-below (aob-acp--lead-effort s nil))))))

(ert-deftest aob-orch-effort-xhigh-lead-sends-high-workers ()
  "A preset lead at xhigh asks Claude for xhigh and gives its opus workers high."
  (require 'ygg-preset)
  (should (member "xhigh" (symbol-value 'ygg-preset-thinking-levels)))
  (let ((sent (aob-tests--agents-sent aob-tests--claude-init
                                      '(:want-thinking "xhigh"
                                        :workers ((:name "build"))))))
    (should (equal "high" (plist-get (plist-get sent :worker-build) :effort)))
    (should (equal "opus" (plist-get (plist-get sent :worker-build) :model)))))

(defun aob-tests--conn-env (refs &optional spec)
  "The environment a connection for a session with REFS is started under.
SPEC is the agent the session runs, a fake Claude by default."
  (let* ((spec (or spec '("fake" :command ("cat"))))
         (aob-acp-agents (list spec))
         (aob-acp-command-function #'identity)
         (aob-acp-environment-function nil)
         (s (aob-create-session :id "acp:orch:env" :backend 'acp :name "env"
                                :project "/tmp/" :dir "/tmp/" :state 'starting
                                :refs (append (list :agent (car spec)) refs)))
        env)
    (unwind-protect
        (cl-letf* ((real (symbol-function 'make-process))
                   ((symbol-function 'make-process)
                    (lambda (&rest args)
                      (setq env process-environment)
                      (apply real args))))
          (aob-acp--connect s #'ignore #'ignore)
          env)
      (when-let* ((proc (aob-session-conn s)))
        (remhash (process-get proc 'aob-conn-key) aob-acp--conns)
        (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
        (ignore-errors (kill-buffer (process-get proc 'aob-stderr-buf)))
        (delete-process proc))
      (aob-remove-session s))))

(ert-deftest aob-orch-workflow-cap-reaches-the-process ()
  "A capped session's adapter starts with the workflow limit; an uncapped one without."
  (should (member "CLAUDE_CODE_WORKFLOW_MAX_CONCURRENT_AGENTS=6"
                  (aob-tests--conn-env '(:subagent-cap 6))))
  (should-not (seq-find (lambda (v) (string-prefix-p "CLAUDE_CODE_WORKFLOW_MAX_CONCURRENT_AGENTS=" v))
                        (aob-tests--conn-env nil)))
  (should-not (seq-find (lambda (v) (string-prefix-p "CLAUDE_CODE_WORKFLOW_MAX_CONCURRENT_AGENTS=" v))
                        (aob-tests--conn-env '(:subagent-cap 0)))))

(defconst aob-tests--codex-agent '("codex" :command ("sh" "-c" "cat" "codex-acp"))
  "A codex agent the connection code can tell apart, running cat.")

(defconst aob-tests--codex-refs
  '(:want-thinking "xhigh" :subagent-cap 6 :worker-prompt "Build it."
    :workers ((:name "build")
              (:name "quick" :model "sonnet" :effort "low" :read-only t :prompt "Find it.")
              (:name "deep" :model "opus" :effort "high"))))

(defmacro aob-tests--with-codex-roles (dir &rest body)
  "Run BODY with codex role files written under a fresh DIR, removed after."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "aob-codex-roles-" t)))
          (aob-acp-codex-roles-directory ,dir))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun aob-tests--codex-config (refs)
  "The CODEX_CONFIG a codex connection for REFS starts under, parsed, or nil."
  (when-let* ((entry (seq-find (lambda (v) (string-prefix-p "CODEX_CONFIG=" v))
                               (aob-tests--conn-env refs aob-tests--codex-agent))))
    (json-parse-string (substring entry (length "CODEX_CONFIG="))
                       :object-type 'plist :array-type 'list)))

(defun aob-tests--codex-role (config name)
  "The text of the role file CONFIG names for role NAME."
  (with-temp-buffer
    (insert-file-contents
     (plist-get (plist-get (plist-get config :agents) (intern (concat ":" name)))
                :config_file))
    (buffer-string)))

(ert-deftest aob-orch-codex-session-carries-roles-with-model-and-effort ()
  "Codex gets each worker level as a role on its connection, model and effort in the role file."
  (aob-tests--with-codex-roles dir
    (let* ((config (aob-tests--codex-config aob-tests--codex-refs))
           (build (aob-tests--codex-role config "worker-build"))
           (quick (aob-tests--codex-role config "worker-quick"))
           (deep (aob-tests--codex-role config "worker-deep")))
      (should (string-match-p "^name = \"worker-build\"$" build))
      (should (string-match-p "^model = \"gpt-6-astra\"$" build))
      (should (string-match-p "^model_reasoning_effort = \"high\"$" build))
      (should (string-match-p "^developer_instructions = \"Build it.\"$" build))
      (should (string-match-p "^model = \"gpt-6-sol\"$" quick))
      (should (string-match-p "^model_reasoning_effort = \"low\"$" quick))
      (should (string-match-p "^model = \"gpt-6-astra\"$" deep))
      (should (string-match-p "^model_reasoning_effort = \"high\"$" deep))
      (should (string-prefix-p dir (plist-get (plist-get (plist-get config :agents) :worker-deep)
                                              :config_file)))
      (should (stringp (plist-get (plist-get (plist-get config :agents) :worker-quick)
                                  :description)))
      (should (eq :false (plist-get (plist-get config :features) :multi_agent_v2)))))
  (pcase-let ((`(,wire ,_ ,_) (aob-tests--opened-with '(:agentInfo (:name "codex-acp"))
                                                      aob-tests--codex-refs)))
    (should-not (plist-member (plist-get (plist-get wire :params) :_meta) :claudeCode))))

(ert-deftest aob-orch-codex-effort-below-steps-down-codex-ladder ()
  "One step under the lead is taken on codex's ladder, which runs past Claude's to ultra."
  (let ((ladder aob-acp-codex-effort-ladder))
    (should (equal "max" (aob-acp--effort-below "ultra" ladder)))
    (should (equal "xhigh" (aob-acp--effort-below "max" ladder)))
    (should (equal "high" (aob-acp--effort-below "xhigh" ladder)))
    (should (equal "low" (aob-acp--effort-below "low" ladder)))
    (should (equal "medium" (aob-acp--effort-below nil ladder)))
    (should (equal "medium" (aob-acp--effort-below "ultra"))))
  (aob-tests--with-codex-roles dir
    (let ((config (aob-tests--codex-config '(:want-thinking "ultra" :workers ((:name "build"))))))
      (should (string-match-p "^model_reasoning_effort = \"max\"$"
                              (aob-tests--codex-role config "worker-build"))))))

(ert-deftest aob-orch-codex-override-wins ()
  "The owner's worker effort is every codex role's, and a pair two roles share names no role."
  (aob-tests--with-codex-roles dir
    (let ((config (aob-tests--codex-config
                   (append '(:worker-effort "max") aob-tests--codex-refs))))
      (dolist (name '("worker-build" "worker-quick" "worker-deep"))
        (should (string-match-p "^model_reasoning_effort = \"max\"$"
                                (aob-tests--codex-role config name))))))
  (aob-tests--with-session s
    (aob-session-put s :codex-roles '(("worker-build" "gpt-6-astra" "max")
                                      ("worker-quick" "gpt-6-sol" "max")
                                      ("worker-deep" "gpt-6-astra" "max")))
    (should (equal "worker-quick"
                   (aob-acp--codex-role s '(:model "gpt-6-sol" :reasoningEffort "max"))))
    (should (equal "gpt-6-astra · max"
                   (aob-acp--codex-role s '(:model "gpt-6-astra" :reasoningEffort "max"))))
    (should-not (aob-acp--codex-role s '(:model "" :reasoningEffort "medium")))))

(ert-deftest aob-orch-codex-cap-setting ()
  "A capped codex session caps codex's concurrent threads; an uncapped one sends nothing."
  (aob-tests--with-codex-roles dir
    (should (eql 6 (plist-get (plist-get (aob-tests--codex-config '(:subagent-cap 6)) :agents)
                              :max_concurrent_threads_per_session)))
    (should-not (aob-tests--codex-config '(:subagent-cap 0)))
    (should-not (aob-tests--codex-config nil))))

(ert-deftest aob-orch-codex-roles-go-to-codex-alone ()
  "Claude gets no codex config and codex no Claude agents or workflow limit."
  (aob-tests--with-codex-roles dir
    (let ((claude-env (aob-tests--conn-env aob-tests--codex-refs))
          (codex-env (aob-tests--conn-env aob-tests--codex-refs aob-tests--codex-agent)))
      (should-not (seq-find (lambda (v) (string-prefix-p "CODEX_CONFIG=" v)) claude-env))
      (should (member "CLAUDE_CODE_WORKFLOW_MAX_CONCURRENT_AGENTS=6" claude-env))
      (should (seq-find (lambda (v) (string-prefix-p "CODEX_CONFIG=" v)) codex-env))
      (should-not (seq-find (lambda (v) (string-prefix-p "CLAUDE_CODE_WORKFLOW" v)) codex-env)))
    (let ((agents (aob-tests--agents-sent aob-tests--claude-init aob-tests--codex-refs)))
      (should (equal "opus" (plist-get (plist-get agents :worker-build) :model)))
      (should-not (string-prefix-p "gpt-" (plist-get (plist-get agents :worker-quick) :model))))))

(defun aob-tests--codex-spawn (s id update prompt status &optional thread model effort state)
  "Feed S codex's spawn call ID as UPDATE, sent PROMPT, the call at STATUS.
Once the thread exists, THREAD ran on MODEL at EFFORT and is in STATE."
  (aob-tests--feed
   s (json-encode
      `((jsonrpc . "2.0") (method . "session/update")
        (params . ((update . ((sessionUpdate . ,update) (toolCallId . ,id)
                              (kind . "other") (title . "spawnAgent") (status . ,status)
                              (rawInput . ((prompt . ,prompt) (senderThreadId . "lead")
                                           (receiverThreadIds . ,(if thread (vector thread) []))
                                           (agentsStates . ,(if thread
                                                                `((,(intern thread) . ((status . ,state))))
                                                              (make-hash-table)))
                                           (model . ,(or model ""))
                                           (reasoningEffort . ,(or effort "medium"))
                                           (status . ,status)))
                              (_meta . ((codex . ((collaboration . ((tool . "spawnAgent")
                                                                    (senderThreadId . "lead")
                                                                    (receiverThreadIds . ,(if thread (vector thread) []))))))))))))))))

(defun aob-tests--codex-wait (s id thread state)
  "Feed S codex's finished wait call ID reporting THREAD in STATE."
  (aob-tests--feed
   s (json-encode
      `((jsonrpc . "2.0") (method . "session/update")
        (params . ((update . ((sessionUpdate . "tool_call") (toolCallId . ,id)
                              (kind . "other") (title . "wait") (status . "completed")
                              (rawInput . ((senderThreadId . "lead")
                                           (receiverThreadIds . ,(vector thread))
                                           (agentsStates . ((,(intern thread) . ((status . ,state)))))))
                              (_meta . ((codex . ((collaboration . ((tool . "wait")))))))))))))))

(ert-deftest aob-orch-codex-brief-warning-on-spawn ()
  "A codex spawn is a subagent: its brief is checked, its role named, its cap counted."
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-session-put s :subagent-cap 1)
          (aob-session-put s :subagent-briefs t)
          (aob-session-put s :codex-roles '(("worker-quick" "gpt-6-luna" "low")))
          (aob-tests--codex-spawn s "call_1" "tool_call" "GOAL\nList the files.\nVERIFY\nls" "in_progress")
          (aob-tests--codex-spawn s "call_1" "tool_call_update" "GOAL\nList the files.\nVERIFY\nls"
                                  "completed" "thread-1" "gpt-6-luna" "low" "running")
          (let ((kid (aob-session-get (format "%s/call_1" (aob-session-id s)))))
            (should kid)
            (should (equal "List the files." (aob-session-name kid)))
            (should (equal "worker-quick" (aob-session-ref kid :subagent-type)))
            (should (eq 'working (aob-session-state kid)))
            (should (member "brief for List the files. lacks SCOPE CONTEXT ACCEPTANCE REPORT"
                            (aob-tests--notices s)))
            (aob-tests--codex-spawn s "call_2" "tool_call" aob-tests--full-brief "in_progress")
            (should (member "subagent cap passed: 2 working, cap 1" (aob-tests--notices s)))
            (aob-tests--codex-wait s "call_3" "thread-1" "completed")
            (should (eq 'done (aob-session-state kid)))))
      (aob-tests--kill-views))))

;; a codex thread outlives the lead's turn: its own report settles it, not the lead going idle
(ert-deftest aob-orch-codex-worker-outlives-the-lead-turn ()
  (aob-tests--with-session s
    (unwind-protect
        (progn
          (aob-session-put s :codex-roles '(("worker-quick" "gpt-6-luna" "low")))
          (aob-tests--codex-spawn s "call_1" "tool_call_update" "GOAL\nx" "completed"
                                  "thread-1" "gpt-6-luna" "low" "running")
          (let ((kid (aob-session-get (format "%s/call_1" (aob-session-id s)))))
            (aob-set-state s 'working)
            (aob-set-state s 'idle)
            (should (eq 'working (aob-session-state kid)))
            (should (= 1 (aob-subagent-live-count s)))
            (aob-tests--codex-wait s "call_2" "thread-1" "errored")
            (should (eq 'failed (aob-session-state kid)))))
      (aob-tests--kill-views))))

(ert-deftest aob-orch-codex-role-file-escapes-del ()
  (should (equal (aob-acp--toml-string (string ?a #x7f ?b)) "\"a\\u007Fb\""))
  (should (equal (aob-acp--toml-string "q\"n\n") "\"q\\\"n\\n\"")))

(ert-deftest aob-orch-worker-effort-offers-the-agent-ladder ()
  (aob-tests--with-session s
    (aob-session-put s :agent "claude")
    (should-not (member "ultra" (aob-acp--worker-efforts s)))
    (aob-session-put s :agent "codex")
    (should (member "ultra" (aob-acp--worker-efforts s)))))

(ert-deftest aob-orch-lead-preset-deep-differs-from-build ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "../../presets/lead.md"
                                                        (file-name-directory aob-tests--file)))
                (buffer-string))))
    (should (string-match "^thinking: medium$" text))
    (should (string-match "build=opus/medium" text))
    (should (string-match "deep=opus/high" text))
    (should (string-match "review=opus/high" text))
    (should (string-match "ui=opus/medium" text))))

(ert-deftest aob-orch-effort-quick-is-read-only-explorer ()
  "The lead preset's quick level runs the search body with the reading tools only."
  (require 'ygg-preset)
  (aob-tests--host-defun 'ygg-aob--preset-limits)
  (let* ((ygg-preset-config-directory
          (expand-file-name "../../presets/" (file-name-directory aob-tests--file)))
         (ygg-preset-user-directory (make-temp-name "/tmp/aob-tests-no-presets-")))
    (cl-letf (((symbol-function 'ygg-aob--presets-of)
               (lambda (_dir) (cons nil (ygg-preset-list)))))
      (let* ((refs (ygg-aob--preset-limits
                    "run it\n\n<preset name=\"lead\">\nbody\n</preset>"))
             (agents (aob-tests--agents-sent aob-tests--claude-init refs)))
        (should (equal '("Read" "Grep" "Glob") (plist-get (plist-get agents :worker-quick) :tools)))
        (should (string-prefix-p "# Search" (plist-get (plist-get agents :worker-quick) :prompt)))
        (should (string-prefix-p "# Build" (plist-get (plist-get agents :worker-build) :prompt)))
        (should-not (plist-member (plist-get agents :worker-build) :tools))
        (should-not (plist-member (plist-get agents :worker-deep) :tools))
        (aob-tests--with-codex-roles dir
          (let* ((config (aob-tests--codex-config refs))
                 (quick (aob-tests--codex-role config "worker-quick"))
                 (build (aob-tests--codex-role config "worker-build")))
            (should (string-match-p "^sandbox_mode = \"read-only\"$" quick))
            (should (string-match-p "^developer_instructions = \"# Search" quick))
            (should-not (string-match-p "sandbox_mode" build))
            (should (string-match-p "^developer_instructions = \"# Build" build))))))))

(ert-deftest aob-orch-review-and-ui-levels-run-their-skills-read-only ()
  "The lead's review and ui levels run the ICE review skills with the reading tools only."
  (require 'ygg-preset)
  (aob-tests--host-defun 'ygg-aob--preset-limits)
  (let* ((repo (expand-file-name "../../" (file-name-directory aob-tests--file)))
         (ygg-preset-config-directory (expand-file-name "presets/" repo))
         (ygg-preset-own-skills-dir (expand-file-name "skills/" repo))
         (ygg-preset-user-directory (make-temp-name "/tmp/aob-tests-no-presets-"))
         (ygg-preset-home-dir (make-temp-name "/tmp/aob-tests-no-home-"))
         (default-directory (file-name-as-directory (make-temp-name "/tmp/aob-tests-no-root-"))))
    (cl-letf (((symbol-function 'ygg-aob--presets-of)
               (lambda (_dir) (cons nil (ygg-preset-list)))))
      (let* ((refs (ygg-aob--preset-limits
                    "run it\n\n<preset name=\"lead\">\nbody\n</preset>"))
             (agents (aob-tests--agents-sent aob-tests--claude-init refs)))
        (dolist (level '((:worker-review . "# ICE review loop")
                         (:worker-ui . "# ICE UI review")))
          (let ((agent (plist-get agents (car level))))
            (should (equal '("Read" "Grep" "Glob") (plist-get agent :tools)))
            (should (string-prefix-p (cdr level) (plist-get agent :prompt)))))
        (should (equal "opus" (plist-get (plist-get agents :worker-review) :model)))
        (should (equal "high" (plist-get (plist-get agents :worker-review) :effort)))
        (should (equal "medium" (plist-get (plist-get agents :worker-ui) :effort)))
        (aob-tests--with-codex-roles dir
          (let ((review (aob-tests--codex-role (aob-tests--codex-config refs) "worker-review")))
            (should (string-match-p "^sandbox_mode = \"read-only\"$" review))
            (should (string-match-p "^developer_instructions = \"# ICE review loop" review))))))))

(ert-deftest aob-orch-skill-level-without-its-skill-keeps-the-carried-prompt ()
  "A level whose skill nobody wrote gets no prompt of its own and stays read-only."
  (require 'ygg-preset)
  (let ((ygg-preset-worker-levels '(("review" :skill "no-such-skill" :read-only t)))
        (ygg-preset-own-skills-dir (make-temp-name "/tmp/aob-tests-no-skills-"))
        (ygg-preset-home-dir (make-temp-name "/tmp/aob-tests-no-home-"))
        (default-directory (file-name-as-directory (make-temp-name "/tmp/aob-tests-no-root-"))))
    (let ((w (ygg-preset--worker-level (list :name "review" :model "opus") nil)))
      (should-not (plist-member w :prompt))
      (should (plist-get w :read-only)))))

(defmacro aob-tests--with-workflow (s dir &rest body)
  "Bind S to a fake ACP session and DIR to a fresh run dir, then clear followers."
  (declare (indent 2))
  `(let ((,dir (file-name-as-directory (make-temp-file "aob-wf-" t)))
         (aob-subagent-workflow-settle-secs 0))
     (unwind-protect
         (aob-tests--with-session ,s ,@body)
       (maphash (lambda (_d wf) (aob-subagent--workflow-stop wf)) aob-subagent--workflows)
       (clrhash aob-subagent--workflows)
       (aob-tests--kill-views)
       (delete-directory ,dir t))))

(defun aob-tests--wf-append (file &rest objs)
  "Append each of OBJS to FILE as one JSON line."
  (let ((coding-system-for-write 'utf-8))
    (write-region (mapconcat (lambda (o) (concat (json-encode o) "\n")) objs "")
                  nil file t 'silent)))

(defun aob-tests--send-tool (s id title dir)
  "Feed S a finished tool call ID titled TITLE whose output names DIR."
  (aob-tests--feed
   s (json-encode
      `((jsonrpc . "2.0") (method . "session/update")
        (params . ((update . ((sessionUpdate . "tool_call") (toolCallId . ,id)
                              (title . ,title) (kind . "other") (status . "completed")
                              (rawInput . ((script . "export const meta = {}")))
                              (content . [((type . "content")
                                           (content . ((type . "text")
                                                       (text . ,(format "Workflow launched in background.\nTranscript dir: %s\nRun ID: wf_1" (directory-file-name dir))))))])))))))))

(defun aob-tests--wf-of (dir)
  (gethash dir aob-subagent--workflows))

(defun aob-tests--wf-kid (s id)
  (aob-session-get (format "%s/%s" (aob-session-id s) id)))

(ert-deftest aob-orch-workflow-journal-children-appear-on-started ()
  "Each started line makes one read-only child of the lead, working."
  (aob-tests--with-workflow s dir
    (aob-session-put s :agent "claude")
    (aob-tests--wf-append (expand-file-name "journal.jsonl" dir)
                          '((type . "launched"))
                          '((type . "started") (key . "k1") (agentId . "a1")
                            (label . "count:a.txt") (phase . "Count"))
                          '((type . "started") (key . "k2") (agentId . "a2") (phase . "Count")))
    (aob-tests--send-tool s "toolu_wf" "Workflow" dir)
    (should (equal "Workflow" (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'tool))
                                                   (aob-session-events s))
                                         :title)))
    (let ((wf (aob-tests--wf-of dir)))
      (should wf)
      (should (timerp (aob-subagent--wf-timer wf)))
      (should-not (aob-subagent-children s))
      (aob-subagent--workflow-poll wf)
      (let ((a1 (aob-tests--wf-kid s "a1"))
            (a2 (aob-tests--wf-kid s "a2")))
        (should (= 2 (length (aob-subagent-children s))))
        (should (eq 'workflow-subagent (aob-session-backend a1)))
        (should (equal "count:a.txt" (aob-session-name a1)))
        (should (equal "Count a2" (aob-session-name a2)))
        (should (eq 'working (aob-session-state a1)))
        (should (equal (aob-session-id s) (aob-session-ref a1 :parent-session)))
        (should (equal "claude" (aob-session-ref a1 :agent)))
        (should (equal "workflow · Count" (aob-session-ref a1 :subagent-type)))
        (should (equal dir (aob-session-ref a1 :workflow-dir)))
        (should (equal "a1" (aob-session-ref a1 :workflow-agent)))
        (should (= 2 (aob-subagent-live-count s)))
        (should-error (aob-prompt a1 "hi") :type 'user-error)
        (should-error (aob-interject a1 "hi") :type 'user-error)
        (aob-subagent--workflow-poll wf)
        (should (= 2 (length (aob-subagent-children s))))
        (should (timerp (aob-subagent--wf-timer wf)))))))

(ert-deftest aob-orch-workflow-journal-result-settles-child ()
  "A result line ends its child done with the answer in its trace; a failure fails it;
the follower stops once every started agent has settled."
  (aob-tests--with-workflow s dir
    (let ((journal (expand-file-name "journal.jsonl" dir)))
      (aob-tests--wf-append journal
                            '((type . "launched"))
                            '((type . "started") (agentId . "a1") (label . "one"))
                            '((type . "started") (agentId . "a2") (label . "two")))
      (aob-tests--send-tool s "toolu_wf" "Workflow" dir)
      (let ((wf (aob-tests--wf-of dir)))
        (aob-subagent--workflow-poll wf)
        (aob-tests--wf-append journal '((type . "result") (agentId . "a1") (result . "3 lines — counted")))
        (aob-subagent--workflow-poll wf)
        (let ((a1 (aob-tests--wf-kid s "a1")))
          (should (eq 'done (aob-session-state a1)))
          (should (seq-find (lambda (e) (and (eq (plist-get e :type) 'message)
                                             (equal "3 lines — counted" (plist-get e :text))))
                            (aob-session-events a1)))
          (with-current-buffer (aob-trace-buffer a1)
            (aob-trace--render t)
            (should (string-match-p "3 lines — counted" (buffer-string))))
          (should (= 1 (aob-subagent-live-count s)))
          (should (timerp (aob-subagent--wf-timer wf)))
          (aob-tests--wf-append journal '((type . "stopped") (agentId . "a2") (error . "boom")))
          (aob-subagent--workflow-poll wf)
          (should (eq 'failed (aob-session-state (aob-tests--wf-kid s "a2"))))
          (should (= 0 (aob-subagent-live-count s)))
          (should-not (aob-subagent--wf-timer wf)))))))

(ert-deftest aob-orch-workflow-journal-waits-out-a-gap-between-agents ()
  "With every agent answered the journal is still read for the settle time."
  (aob-tests--with-workflow s dir
    (let ((journal (expand-file-name "journal.jsonl" dir))
          (aob-subagent-workflow-settle-secs 60))
      (aob-tests--wf-append journal
                            '((type . "started") (agentId . "a1") (label . "one"))
                            '((type . "result") (agentId . "a1") (result . "ok")))
      (aob-tests--send-tool s "toolu_wf" "Workflow" dir)
      (let ((wf (aob-tests--wf-of dir)))
        (aob-subagent--workflow-poll wf)
        (should (timerp (aob-subagent--wf-timer wf)))
        (aob-tests--wf-append journal '((type . "started") (agentId . "a2") (label . "two")))
        (aob-subagent--workflow-poll wf)
        (should (eq 'working (aob-session-state (aob-tests--wf-kid s "a2"))))
        (aob-tests--wf-append journal '((type . "result") (agentId . "a2") (result . "ok")))
        (aob-subagent--workflow-poll wf)
        (setf (aob-subagent--wf-heard wf) (- (float-time) 61))
        (aob-subagent--workflow-poll wf)
        (should-not (aob-subagent--wf-timer wf))))))

(ert-deftest aob-orch-workflow-journal-reads-only-whole-new-lines ()
  "A half-written line waits for its end; each poll reads only what is new."
  (aob-tests--with-workflow s dir
    (let ((journal (expand-file-name "journal.jsonl" dir)))
      (aob-tests--send-tool s "toolu_wf" "Workflow" dir)
      (let ((wf (aob-tests--wf-of dir)))
        (aob-subagent--workflow-poll wf)
        (should (= 0 (aob-subagent--wf-offset wf)))
        (write-region "{\"type\":\"started\",\"agentId\":\"a1\",\"label\":\"one — é\"" nil journal nil 'silent)
        (aob-subagent--workflow-poll wf)
        (should-not (aob-subagent-children s))
        (write-region "}\n" nil journal t 'silent)
        (aob-subagent--workflow-poll wf)
        (should (equal "one — é" (aob-session-name (aob-tests--wf-kid s "a1"))))
        (should (= (file-attribute-size (file-attributes journal))
                   (aob-subagent--wf-offset wf)))
        (aob-subagent--workflow-poll wf)
        (should (= 1 (length (aob-subagent-children s))))))))

(ert-deftest aob-orch-workflow-journal-steps-from-agent-transcript ()
  "An agent's transcript gives its child the prompt and the tools it ran."
  (aob-tests--with-workflow s dir
    (let ((journal (expand-file-name "journal.jsonl" dir))
          (steps (expand-file-name "agent-a1.jsonl" dir)))
      (aob-tests--wf-append journal '((type . "started") (agentId . "a1") (label . "one")))
      (aob-tests--wf-append steps
                            '((type . "user") (message . ((role . "user") (content . "Count the lines — all of them"))))
                            '((type . "attachment") (attachment . ((type . "x")))))
      (aob-tests--send-tool s "toolu_wf" "Workflow" dir)
      (let ((wf (aob-tests--wf-of dir)))
        (aob-subagent--workflow-poll wf)
        (let ((a1 (aob-tests--wf-kid s "a1")))
          (should (equal "Count the lines — all of them"
                         (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'prompt))
                                              (aob-session-events a1))
                                    :text)))
          (aob-tests--wf-append steps
                                '((type . "assistant")
                                  (message . ((role . "assistant")
                                              (content . [((type . "tool_use") (id . "tu1") (name . "Bash")
                                                           (input . ((command . "wc -l a.txt"))))]))))
                                '((type . "user") (message . ((role . "user")
                                                              (content . [((type . "tool_result") (content . "3"))])))))
          (aob-tests--wf-append journal '((type . "result") (agentId . "a1") (result . "3")))
          (aob-subagent--workflow-poll wf)
          (should (= 1 (seq-count (lambda (e) (eq (plist-get e :type) 'prompt)) (aob-session-events a1))))
          (should (seq-find (lambda (e) (and (eq (plist-get e :type) 'tool)
                                             (equal "Bash wc -l a.txt" (plist-get e :title))))
                            (aob-session-events a1)))
          (should (eq 'message (plist-get (car (aob-session-events a1)) :type))))))))

(ert-deftest aob-orch-workflow-journal-non-workflow-tool-starts-nothing ()
  "Only a call titled Workflow is followed, whatever its output says."
  (aob-tests--with-workflow s dir
    (aob-tests--wf-append (expand-file-name "journal.jsonl" dir)
                          '((type . "started") (agentId . "a1") (label . "one")))
    (aob-tests--send-tool s "toolu_bash" "Bash" dir)
    (should-not (aob-tests--wf-of dir))
    (should (= 0 (hash-table-count aob-subagent--workflows)))))

(ert-deftest aob-orch-workflow-journal-lead-gone-stops-and-drops ()
  "Removing the lead lets its workflow go and takes the children with it."
  (aob-tests--with-workflow s dir
    (aob-tests--wf-append (expand-file-name "journal.jsonl" dir)
                          '((type . "started") (agentId . "a1") (label . "one")))
    (aob-tests--send-tool s "toolu_wf" "Workflow" dir)
    (let ((wf (aob-tests--wf-of dir)))
      (aob-subagent--workflow-poll wf)
      (let ((a1 (aob-tests--wf-kid s "a1")))
        (aob-remove-session s)
        (should-not (aob-session-get (aob-session-id a1)))
        (should-not (aob-subagent--wf-timer wf))
        (should-not (aob-tests--wf-of dir))))))

(ert-deftest aob-orch-workflow-journal-followed-when-output-comes-in-an-update ()
  "A Workflow call first seen with no output is followed once its update names the dir."
  (aob-tests--with-workflow s dir
    (aob-tests--feed
     s (json-encode
        `((jsonrpc . "2.0") (method . "session/update")
          (params . ((update . ((sessionUpdate . "tool_call") (toolCallId . "toolu_wf")
                                (title . "Workflow") (kind . "other") (status . "pending")
                                (rawInput . ((script . "export const meta = {}"))))))))))
    (should (= 0 (hash-table-count aob-subagent--workflows)))
    (aob-tests--feed
     s (json-encode
        `((jsonrpc . "2.0") (method . "session/update")
          (params . ((update . ((sessionUpdate . "tool_call_update") (toolCallId . "toolu_wf")
                                (title . "Workflow") (status . "completed")
                                (content . [((type . "content")
                                             (content . ((type . "text")
                                                         (text . ,(format "Transcript dir: %s" dir)))))]))))))))
    (should (equal "Workflow" (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'tool))
                                                   (aob-session-events s))
                                         :title)))
    (should (timerp (aob-subagent--wf-timer (aob-tests--wf-of dir))))))

(ert-deftest aob-orch-workflow-journal-idle-cap-counts-transcript-growth ()
  "A working agent whose transcript grows keeps its workflow followed; silence past
the cap fails it and lets the workflow go."
  (aob-tests--with-workflow s dir
    (let ((steps (expand-file-name "agent-a1.jsonl" dir))
          (aob-subagent-workflow-idle-secs 60))
      (aob-tests--wf-append (expand-file-name "journal.jsonl" dir)
                            '((type . "started") (agentId . "a1") (label . "one")))
      (aob-tests--send-tool s "toolu_wf" "Workflow" dir)
      (let ((wf (aob-tests--wf-of dir)))
        (aob-subagent--workflow-poll wf)
        (setf (aob-subagent--wf-heard wf) (- (float-time) 61))
        (aob-tests--wf-append steps '((type . "user") (message . ((role . "user") (content . "go")))))
        (aob-subagent--workflow-poll wf)
        (should (eq 'working (aob-session-state (aob-tests--wf-kid s "a1"))))
        (should (timerp (aob-subagent--wf-timer wf)))
        (setf (aob-subagent--wf-heard wf) (- (float-time) 61))
        (aob-subagent--workflow-poll wf)
        (should (eq 'failed (aob-session-state (aob-tests--wf-kid s "a1"))))
        (should-not (aob-subagent--wf-timer wf))))))

(defun aob-tests--init-reply (s json)
  "Answer S's pending initialize with the result JSON, as the wire does."
  (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"result\":%s}"
                             (process-get (aob-session-conn s) 'aob-init-id) json)))

(ert-deftest aob-initialize-offering-1-is-the-request-it-always-was ()
  (aob-tests--with-session s
    (let ((aob-acp-native-subagents nil)
          (aob-acp-offer-protocol-version 1))
      (aob-tests--capturing sent
        (aob-acp--initialize (aob-session-conn s))
        (should (equal (json-serialize (plist-get (car sent) :params))
                       (concat "{\"protocolVersion\":1,\"clientCapabilities\":"
                               "{\"fs\":{\"readTextFile\":false,\"writeTextFile\":false},"
                               "\"elicitation\":{\"form\":{},\"url\":{}},"
                               "\"auth\":{\"terminal\":true},"
                               "\"session\":{\"configOptions\":{\"boolean\":{}},"
                               "\"notices\":{},\"compaction\":{}},"
                               "\"_meta\":{\"subagent-transcript\":true,"
                               "\"terminal_output_delta\":true}},"
                               "\"clientInfo\":{\"name\":\"aob.el\",\"version\":\"0.1\"}}"))))
      (aob-tests--init-reply s "{\"protocolVersion\":2,\"info\":{\"name\":\"x\"}}")
      (should (eq 'failed (car (process-get (aob-session-conn s) 'aob-init)))))))

(ert-deftest aob-initialize-takes-the-version-the-agent-answers ()
  "Offering 2 carries both key sets; a v1 answer is spoken in v1, and a
v2 one is read into the shape every reader expects, list, resume and
close coming with the session capability itself."
  (aob-tests--with-session s
    (let ((proc (aob-session-conn s))
          (aob-acp-native-subagents nil)
          (aob-acp-offer-protocol-version 2))
      (aob-tests--capturing sent
        (aob-acp--initialize proc)
        (let ((params (plist-get (car sent) :params)))
          (should (eql 2 (plist-get params :protocolVersion)))
          (should (plist-get params :clientCapabilities))
          (should (equal (plist-get params :info) (plist-get params :clientInfo)))
          (should (string-match-p "\"capabilities\":{\"elicitation\":{\"form\":{},\"url\":{}}"
                                  (json-serialize params)))))
      (aob-tests--init-reply
       s "{\"protocolVersion\":1,\"agentCapabilities\":{\"loadSession\":true,\"sessionCapabilities\":{\"resume\":{}}}}")
      (should (eql 1 (aob-acp-protocol proc)))
      (should (aob-acp--session-cap (aob-acp--conn-init proc) :resume))
      (should-not (aob-acp--session-cap (aob-acp--conn-init proc) :close))
      (aob-tests--capturing _sent (aob-acp--initialize proc))
      (aob-tests--init-reply
       s "{\"protocolVersion\":2,\"info\":{\"name\":\"codex-acp\"},\"capabilities\":{\"session\":{\"delete\":{},\"mcp\":{\"http\":true}}}}")
      (let ((init (aob-acp--conn-init proc)))
        (should (eql 2 (aob-acp-protocol proc)))
        (dolist (cap '(:list :resume :close :delete))
          (should (aob-acp--session-cap init cap)))
        (should-not (aob-acp--session-cap init :fork))
        (should-not (plist-get (plist-get init :agentCapabilities) :loadSession))
        (should (plist-get (plist-get (plist-get init :agentCapabilities) :mcpCapabilities)
                           :http))
        (should (equal (plist-get (plist-get init :agentInfo) :name) "codex-acp"))))))

(ert-deftest aob-listed-sessions-merge-into-the-found-ones-by-id ()
  (let* ((found (list (list :agent "claude" :acp-id "a" :name "claude:1 · Sep 30")
                      (list :agent "claude" :acp-id "b" :name "kept")))
         (listed (list (list :sessionId "b" :title "Fix the race" :updatedAt "2026-09-30T08:00:00Z")
                       (list :sessionId "c" :title "From the terminal" :cwd "/w/p")))
         (merged (aob-acp-merge-listed found listed "claude" "/w/p/")))
    (should (equal (mapcar (lambda (e) (plist-get e :acp-id)) merged) '("a" "b" "c")))
    (should (equal (car merged) (car found)))
    (should (equal (plist-get (nth 1 merged) :name) "kept"))
    (should (equal (plist-get (nth 1 merged) :listed-title) "Fix the race"))
    (should (equal (plist-get (nth 1 merged) :updated-at) "2026-09-30T08:00:00Z"))
    (should (equal (plist-get (nth 2 merged) :name) "From the terminal"))
    (should (equal (plist-get (nth 2 merged) :dir) "/w/p"))
    (should (equal (plist-get (nth 2 merged) :agent) "claude"))))

(ert-deftest aob-listing-without-the-capability-answers-nil ()
  (let ((aob-acp--conns (make-hash-table :test #'equal))
        (got 'unset))
    (aob-acp-list-sessions "claude" "/nowhere/" (lambda (x) (setq got x)))
    (should-not got)))

(ert-deftest aob-v2-wake-resumes-and-replays-only-what-the-trace-lacks ()
  (let ((init (aob-acp--v1-init '(:protocolVersion 2 :capabilities (:session nil)))))
    (let ((got (aob-tests--restore init 'load)))
      (should (equal (nth 0 got) "session/resume"))
      (should (equal (plist-get (nth 1 got) :replayFrom) '(:type "start")))
      (should (eq (nth 2 got) 'load)))
    (let ((got (aob-tests--restore init 'resume)))
      (should (equal (nth 0 got) "session/resume"))
      (should-not (plist-member (nth 1 got) :replayFrom))
      (should (eq (nth 2 got) 'resume)))
    (let* ((verb (list nil))
           (form (cl-letf (((symbol-function 'aob-acp--mcp-servers) #'ignore))
                   (aob-acp--restore-open init "sid-1" "/tmp" "n" verb 'load t))))
      (should (equal (car form) "session/resume"))
      (should-not (plist-member (cadr form) :replayFrom))
      (should (eq (car verb) 'resume))))
  (should (equal (car (aob-tests--restore
                       (aob-acp--v1-init '(:protocolVersion 2 :capabilities (:prompt nil)))))
                 "session/new")))

(defmacro aob-tests--with-transcript-turns (turns &rest body)
  "Run BODY with every entry's transcript reading as TURNS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'aob-transcript-file) (lambda (_e) "/tmp/aob-fake.jsonl"))
             ((symbol-function 'aob-transcript-turns)
              (lambda (_file &optional tools)
                (if tools ,turns
                  (seq-remove (lambda (x) (equal (car x) "tool")) ,turns)))))
     ,@body))

(ert-deftest aob-seeded-history-shows-tool-lines ()
  (aob-tests--with-transcript-turns '(("user" . "read it") ("tool" . "Read a.el")
                                      ("assistant" . "done"))
    (let ((s (aob-create-session :id "acp:seed:1" :backend 'acp :name "seed"
                                 :project "/tmp/" :dir "/tmp/" :state 'starting)))
      (unwind-protect
          (progn
            (aob-acp--seed-history s '(:acp-id "x"))
            (let ((evs (reverse (aob-session-events s))))
              (should (equal (mapcar (lambda (e) (plist-get e :type)) evs)
                             '(prompt tool message)))
              (should (plist-get (nth 0 evs) :typed))
              (should (equal (plist-get (nth 1 evs) :title) "Read a.el"))
              (should (equal (plist-get (nth 1 evs) :status) "completed"))))
        (aob-remove-session s)))))

(ert-deftest aob-a-replaying-wake-does-not-show-the-history-twice ()
  "A load replays what the transcript already seeded: whether the agent
answers before the session exists or after, the trace holds it once."
  (aob-tests--with-transcript-turns '(("user" . "hi") ("assistant" . "ok"))
    (let ((init '(:agentCapabilities (:loadSession t)))
          (aob-acp-persist-file nil)
          (dir (make-temp-file "aob-replay" t))
          later)
      (unwind-protect
          (dolist (deferred '(nil t))
            (let ((s (cl-letf (((symbol-function 'aob-acp--connect)
                                (lambda (sess open then)
                                  (let ((go (lambda ()
                                              (funcall open init)
                                              (funcall then sess '(:sessionId "sid-1")))))
                                    (if deferred (setq later go) (funcall go)))))
                               ((symbol-function 'aob-acp--session-opened) #'ignore)
                               ((symbol-function 'aob-acp--mcp-servers) #'ignore))
                       (let ((s (aob-acp-resume-entry
                                 (list :agent "claude" :name "replay-me"
                                       :project dir :dir dir :acp-id "sid-1"))))
                         (when deferred
                           (aob-event s 'prompt :text "typed while waking" :typed t)
                           (funcall later))
                         s))))
              (unwind-protect
                  (progn
                    (should (eq (aob-session-ref s :restored-by) 'load))
                    (should (equal (mapcar (lambda (ev) (plist-get ev :text))
                                           (aob-session-events s))
                                   (and deferred '("typed while waking")))))
                (aob-remove-session s))))
        (delete-directory dir t)))))

(ert-deftest aob-a-wake-with-its-transcript-resumes-even-when-load-is-preferred ()
  "The file already gives the history, user lines and tools included;
an agent that can resume is not asked to replay it on top.  A reload
asked for by name still reloads."
  (let ((init '(:agentCapabilities (:loadSession t :sessionCapabilities (:resume nil))))
        (aob-acp-restore-preference 'load))
    (aob-tests--with-transcript-turns '(("user" . "hi") ("assistant" . "ok"))
      (let ((got (aob-tests--restore init)))
        (should (equal (nth 0 got) "session/resume"))
        (should (eq (nth 2 got) 'resume)))
      (should (equal (car (aob-tests--restore init 'load)) "session/load")))
    (should (equal (car (aob-tests--restore init)) "session/load"))))

(ert-deftest aob-kill-closes-only-an-agent-that-can-close ()
  (dolist (caps '((:close nil) (:resume nil)))
    (let ((aob-acp-persist-file nil))
      (aob-tests--with-session s
        (process-put (aob-session-conn s) 'aob-init
                     (list 'done (list :agentCapabilities (list :sessionCapabilities caps))))
        (aob-set-state s 'idle)
        (aob-tests--capturing sent
          (cl-letf (((symbol-function 'aob-acp--conn-cleanup) #'ignore))
            (aob-acp--kill s))
          (should (eq (and (seq-find (lambda (m) (equal (plist-get m :method) "session/close"))
                                     sent)
                           t)
                      (and (plist-member caps :close) t))))))))

(ert-deftest aob-discard-deletes-only-where-the-agent-can ()
  "A discard copies the file first and then asks the running agent to
delete, finishing the move when it answers; an agent that cannot delete
is not asked, and an archive never."
  (dolist (case '(((:delete nil) "discarded" t)
                  ((:list nil) "discarded" nil)
                  ((:delete nil) "archive" nil)))
    (let* ((home (make-temp-file "aob-discard" t))
           (file (expand-file-name "sid-9.jsonl" home))
           (aob-acp--conns (make-hash-table :test #'equal)))
      (aob-tests--with-session s
        (let ((proc (aob-session-conn s)))
          (process-put proc 'aob-init
                       (list 'done (list :agentCapabilities
                                         (list :sessionCapabilities (car case)))))
          (puthash (list "claude" home "iso") proc aob-acp--conns)
          (with-temp-file file (insert "{}\n"))
          (unwind-protect
              (cl-letf (((symbol-function 'aob-transcript-file) (lambda (_e) file)))
                (aob-tests--capturing sent
                  (let ((to (aob-transcript-move
                             (list :agent "claude" :project home :acp-id "sid-9")
                             (nth 1 case)))
                        (del (seq-find (lambda (m) (equal (plist-get m :method) "session/delete"))
                                       sent)))
                    (should (eq (and del t) (nth 2 case)))
                    (when del
                      (should (equal (plist-get del :params) '(:sessionId "sid-9")))
                      (should (file-exists-p file))
                      (aob-tests--reply s nil))
                    (should (file-exists-p to))
                    (should-not (file-exists-p file)))))
            (delete-directory home t)))))))

(ert-deftest aob-discard-ends-with-only-the-copy-whatever-the-agent-says ()
  "The file ends up in discarded/ alone: the agent deleting it, the agent
failing to, no agent to ask, and one still awake that is never asked."
  (dolist (case '(deleted failed none awake))
    (let* ((home (make-temp-file "aob-discard" t))
           (file (expand-file-name "sid-9.jsonl" home))
           (to (expand-file-name "discarded/sid-9.jsonl" home))
           (aob-acp--conns (make-hash-table :test #'equal)))
      (aob-tests--with-session s
        (let ((proc (aob-session-conn s)))
          (process-put proc 'aob-init
                       '(done (:agentCapabilities (:sessionCapabilities (:delete nil)))))
          (unless (eq case 'none)
            (puthash (list "claude" home "iso") proc aob-acp--conns))
          (aob-set-state s 'idle)
          (with-temp-file file (insert "{}\n"))
          (unwind-protect
              (cl-letf (((symbol-function 'aob-transcript-file)
                         (lambda (_e) (and (file-exists-p file) file))))
                (aob-tests--capturing sent
                  (aob-transcript-move
                   (list :agent "claude" :project home
                         :acp-id (if (eq case 'awake) "sess-test" "sid-9"))
                   "discarded")
                  (let ((del (seq-find (lambda (m)
                                         (equal (plist-get m :method) "session/delete"))
                                       sent)))
                    (should (eq (and del t) (and (memq case '(deleted failed)) t)))
                    (pcase case
                      ('deleted (delete-file file) (aob-tests--reply s nil))
                      ('failed
                       (let* ((pending (process-get proc 'aob-pending))
                              (id (apply #'max (hash-table-keys pending))))
                         (funcall (gethash id pending) nil
                                  '(:code -32603 :message "not found"))))))
                  (should (file-exists-p to))
                  (should-not (file-exists-p file))))
            (delete-directory home t)))))))

(ert-deftest aob-an-adapter-that-dies-fails-what-it-still-owed ()
  "A delete in flight when its adapter exits is answered once with an
error, so the discard still ends with only the copy; a turn the dead
session was running is not answered back to idle."
  (let* ((home (make-temp-file "aob-discard" t))
         (file (expand-file-name "sid-9.jsonl" home))
         (to (expand-file-name "discarded/sid-9.jsonl" home))
         (aob-acp--conns (make-hash-table :test #'equal))
         (answers nil))
    (aob-tests--with-session s
      (let ((proc (aob-session-conn s)))
        (process-put proc 'aob-init
                     '(done (:agentCapabilities (:sessionCapabilities (:delete nil)))))
        (puthash (list "claude" home "iso") proc aob-acp--conns)
        (aob-set-state s 'idle)
        (with-temp-file file (insert "{}\n"))
        (unwind-protect
            (cl-letf (((symbol-function 'aob-transcript-file)
                       (lambda (_e) (and (file-exists-p file) file))))
              (aob-tests--capturing sent
                (aob-acp--request s "session/prompt" '(:sessionId "sess-test")
                                  (lambda (_res _err) (aob-set-state s 'idle)))
                (aob-transcript-move (list :agent "claude" :project home :acp-id "sid-9")
                                     "discarded"
                                     (lambda () (push 'moved answers)))
                (aob-acp-delete-entry (list :agent "claude" :project home :acp-id "sid-8")
                                      (lambda (res err) (push (list res err) answers)))
                (should (= 2 (seq-count (lambda (m) (equal (plist-get m :method) "session/delete"))
                                        sent)))
                (should (file-exists-p file))
                (delete-process proc)
                (aob-acp--sentinel proc "killed\n")
                (should (eq (aob-session-state s) 'dead))
                (aob-acp--sentinel proc "killed\n")
                (should (= 2 (length answers)))
                (should (memq 'moved answers))
                (let ((reply (seq-find #'consp answers)))
                  (should-not (car reply))
                  (should (stringp (plist-get (cadr reply) :message))))
                (should (= 0 (hash-table-count (process-get proc 'aob-pending))))
                (should (file-exists-p to))
                (should-not (file-exists-p file))))
          (delete-directory home t))))))

(ert-deftest aob-an-adapter-that-dies-mid-handshake-leaves-its-session-dead ()
  "The initialize still pending when the adapter exits is answered with an
error, and the session waiting on it stays dead, said so once."
  (aob-tests--with-session s
    (let ((proc (aob-session-conn s)))
      (aob-session-put s :agent "claude")
      (cl-letf (((symbol-function 'aob-acp--live-conn) (lambda (&rest _) proc)))
        (aob-tests--capturing _sent
          (aob-acp--initialize proc)
          (aob-acp--connect s (lambda (_init) (error "Never opened")) #'ignore)
          (delete-process proc)
          (aob-acp--sentinel proc "killed\n")))
      (should (eq (aob-session-state s) 'dead))
      (should (= 1 (seq-count (lambda (e) (eq (plist-get e :type) 'error))
                              (aob-session-events s)))))))

(ert-deftest aob-discard-unlists-at-once-and-waits-for-the-agent-only-so-long ()
  "A discarded conversation leaves the listing as soon as its copy is made.
An agent that never answers the delete is not waited on past
`aob-transcript-delete-wait', and its answer when it does come is ignored."
  (let* ((home (make-temp-file "aob-discard" t))
         (project (file-name-as-directory (make-temp-file "aob-project" t)))
         (dir (expand-file-name (format "projects/%s" (aob-transcript--slug project)) home))
         (file (expand-file-name "sid-9.jsonl" dir))
         (aob-transcript-delete-wait 0.1)
         (reply nil)
         (moved 0)
         (listed (lambda ()
                   (mapcar (lambda (e) (plist-get e :acp-id))
                           (aob-transcript-found project "claude")))))
    (make-directory (expand-file-name "discarded" dir) t)
    (with-temp-file (expand-file-name "discarded/old.jsonl" dir) (insert "{}\n"))
    (with-temp-file file (insert "{}\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'aob-transcript--homes) (lambda (&rest _) (list home)))
                  ((symbol-function 'aob-transcript--want-title) #'ignore)
                  ((symbol-function 'aob-acp-delete-entry)
                   (lambda (_e &optional then) (setq reply then) t)))
          (aob-transcript-forget)
          (should (equal (funcall listed) '("sid-9")))
          (aob-transcript-move (list :agent "claude" :project project :acp-id "sid-9"
                                     :file file)
                               "discarded" (lambda () (cl-incf moved)))
          (should (file-exists-p file))
          (should-not (funcall listed))
          (let ((deadline (+ (float-time) 3)))
            (while (and (file-exists-p file) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should-not (file-exists-p file))
          (should (= moved 1))
          (funcall reply nil nil)
          (should (= moved 1)))
      (aob-transcript-forget)
      (delete-directory home t)
      (delete-directory project t))))

(ert-deftest aob-info-update-names-and-dates-a-session-not-named-by-you ()
  (aob-tests--with-default-name s
    (aob-tests--info-update s '((title . "Fix the reconnect race")
                                (updatedAt . "2026-09-30T08:00:00Z")))
    (should (equal (aob-session-name s) "claude: Fix the reconnect race"))
    (should (equal (aob-session-ref s :updated-at) "2026-09-30T08:00:00Z")))
  (aob-tests--with-default-name s
    (aob-rename-session s "mine")
    (aob-tests--info-update s '((title . "Agent title")))
    (should (equal (aob-session-name s) "mine"))))

(ert-deftest aob-v2-permission-is-the-same-decision-as-v1 ()
  (let ((options [(:optionId "allow" :name "Allow" :kind "allow_once")
                  (:optionId "reject" :name "Reject" :kind "reject_once")])
        (tc '(:toolCallId "t1" :title "Run make" :kind "execute"
              :rawInput (:command "make test"))))
    (cl-flet ((decision (params)
                (aob-tests--with-session s
                  (aob-tests--request s 5 "session/request_permission"
                                      (append (list :sessionId "sess-test") params
                                              (list :options options)))
                  (let ((d (copy-sequence (car (aob-session-decisions s)))))
                    (cl-remf d :seq)
                    d))))
      (let ((v1 (decision (list :toolCall tc))))
        (should (equal (plist-get v1 :title) "Run make"))
        (should (equal v1 (decision (list :title "Allow make?"
                                          :subject (list :type "tool_call" :toolCall tc)))))
        (let ((cmd (decision (list :title "Run make?"
                                   :subject (list :type "command" :command "make test"
                                                  :cwd "/w/p" :toolCallId "t1")))))
          (should (equal (plist-get cmd :title) "Run make?"))
          (should (equal (plist-get cmd :detail) "make test"))
          (should (equal (plist-get cmd :options) (plist-get v1 :options))))))))

(ert-deftest aob-subagent-update-spawns-and-ends-like-the-two-it-replaces ()
  (aob-tests--with-session s
    (process-put (aob-session-conn s) 'aob-subagents t)
    (aob-set-state s 'working)
    (aob-tests--update s '(:sessionUpdate "subagent_update" :subagentSessionId "k1"
                           :name "explore" :task "look" :state "running"))
    (let ((kid (car (aob-subagent-children s))))
      (should kid)
      (should-not (memq (aob-session-state kid) '(done failed)))
      (aob-tests--update s '(:sessionUpdate "subagent_update" :subagentSessionId "k1"
                             :state "completed"))
      (should (eq (aob-session-state kid) 'done))
      (should (= 1 (length (aob-subagent-children s)))))))

(ert-deftest aob-discard-leaves-a-conversation-that-is-still-awake ()
  (let ((aob-acp--conns (make-hash-table :test #'equal)))
    (aob-tests--with-session s
      (let ((proc (aob-session-conn s)))
        (process-put proc 'aob-init
                     (list 'done '(:agentCapabilities (:sessionCapabilities (:delete nil)))))
        (puthash (list "claude" "/tmp/proj/") proc aob-acp--conns)
        (aob-set-state s 'idle)
        (aob-tests--capturing sent
          (should-not (aob-acp-delete-entry
                       '(:agent "claude" :project "/tmp/proj/" :acp-id "sess-test")))
          (should-not sent)
          (should (aob-acp-delete-entry
                   '(:agent "claude" :project "/tmp/proj/" :acp-id "other")))
          (should (equal (plist-get (car sent) :method) "session/delete")))))))

;;; Auth and URL elicitation — the agent's own login, pages it asks to open

(defconst aob-tests--auth-init
  "{\"protocolVersion\":1,\"authMethods\":[{\"id\":\"sub-login\",\"name\":\"Subscription\",\"type\":\"terminal\",\"args\":[\"login\"],\"env\":{\"AOB_T\":\"yes\"}},{\"id\":\"api-key\",\"name\":\"API Key\",\"_meta\":{\"api-key\":{\"provider\":\"openai\"}}},{\"id\":\"old-login\",\"name\":\"Old Login\",\"_meta\":{\"terminal-auth\":{\"command\":\"true\",\"args\":[\"x\"],\"label\":\"L\"}}}]}"
  "An initialize result offering a terminal login, an agent login and an
older terminal-auth one, in the shapes claude-agent-acp and codex-acp send.")

(defun aob-tests--auth-conn (s)
  "Settle S's connection with `aob-tests--auth-init' as the wire does.
S's project becomes a real directory, where a login terminal can start."
  (aob-session-put s :agent "fake")
  (setf (aob-session-project s) (file-name-as-directory temporary-file-directory))
  (cl-letf (((symbol-function 'aob-acp--send-proc) #'ignore))
    (aob-acp--initialize (aob-session-conn s)))
  (aob-tests--init-reply s aob-tests--auth-init))

(defun aob-tests--auth-refuse (s id)
  "Refuse S's request ID for want of a login."
  (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"error\":{\"code\":-32000,\"message\":\"Authentication required\"}}" id)))

(defun aob-tests--sent-method (sent method)
  "The frames in SENT calling METHOD, newest first."
  (seq-filter (lambda (m) (equal (plist-get m :method) method)) sent))

(defun aob-tests--auth-open (s)
  "Open S over its settled connection and return the session/new id."
  (cl-letf (((symbol-function 'aob-acp--live-conn) (lambda (&rest _) (aob-session-conn s))))
    (aob-acp--connect s (lambda (_init) (list "session/new" (list :cwd "/tmp/proj")))
                      #'ignore)))

(ert-deftest aob-auth-methods-are-kept-per-connection ()
  (aob-tests--with-session s
    (aob-tests--auth-conn s)
    (let ((methods (aob-acp--auth-methods (aob-session-conn s)))
          (aob-acp-agents '(("fake" :command ("fake-acp")))))
      (should (equal (mapcar (lambda (m) (plist-get m :id)) methods)
                     '("sub-login" "api-key" "old-login")))
      (should (equal (aob-acp--auth-argv s (nth 0 methods)) '("fake-acp" "login")))
      (should (equal (aob-acp--auth-env (nth 0 methods)) '("AOB_T=yes")))
      (should (equal (aob-acp--auth-argv s (nth 2 methods)) '("true" "x")))
      (should (aob-acp--auth-terminal-p (nth 2 methods)))
      (should-not (aob-acp--auth-terminal-p (nth 1 methods))))))

(ert-deftest aob-auth-required-on-session-new-offers-the-agent-logins ()
  (aob-tests--with-session s
    (aob-tests--auth-conn s)
    (aob-tests--capturing sent
      (aob-tests--auth-open s)
      (aob-tests--auth-refuse s (plist-get (car (aob-tests--sent-method sent "session/new")) :id))
      (let ((d (car (aob-session-decisions s))))
        (should (eq (plist-get d :kind) 'auth))
        (should (equal (mapcar (lambda (o) (plist-get o :name)) (plist-get d :options))
                       '("Subscription" "API Key" "Old Login" "Not now")))
        (should (eq (aob-session-state s) 'blocked))
        (should (string-match-p "fake needs a login; log in with Subscription or API Key or Old Login"
                                (plist-get (car (last (aob-session-events s))) :title)))))))

(defun aob-tests--await (pred)
  "Pump process output until PRED holds, failing after five seconds."
  (with-timeout (5 (error "aob-tests--await: timed out"))
    (while (not (funcall pred))
      (accept-process-output nil 0.05))))

(ert-deftest aob-auth-terminal-login-then-one-retry-of-session-new ()
  "A terminal login runs the agent's program with the method's args and
env, is never passed to authenticate, and session/new goes out once more;
a second refusal fails the session, readably, instead of asking again."
  (aob-tests--with-session s
    (let ((aob-acp-command-function #'identity)
          (aob-acp-agents
           '(("fake" :command ("sh" "-c" "test \"$1\" = login && test \"$AOB_T\" = yes" "sh")))))
      (aob-tests--auth-conn s)
      (unwind-protect
          (aob-tests--capturing sent
            (aob-tests--auth-open s)
            (aob-tests--auth-refuse s (plist-get (car sent) :id))
            (aob-acp--resolve s (car (aob-session-decisions s)) "sub-login")
            (aob-tests--await (lambda () (cdr (aob-tests--sent-method sent "session/new"))))
            (should-not (aob-tests--sent-method sent "authenticate"))
            (should (= 2 (length (aob-tests--sent-method sent "session/new"))))
            (aob-tests--auth-refuse s (plist-get (car sent) :id))
            (should (= 2 (length (aob-tests--sent-method sent "session/new"))))
            (should-not (aob-session-decisions s))
            (should (eq (aob-session-state s) 'failed))
            (should (string-match-p "fake needs a login"
                                    (aob-session-ref s :fail-reason))))
        (ignore-errors (kill-buffer "*login fake*"))))))

(ert-deftest aob-auth-terminal-login-that-fails-sends-nothing ()
  (aob-tests--with-session s
    (let ((aob-acp-command-function #'identity)
          (aob-acp-agents '(("fake" :command ("false")))))
      (aob-tests--auth-conn s)
      (unwind-protect
          (aob-tests--capturing sent
            (aob-tests--auth-open s)
            (aob-tests--auth-refuse s (plist-get (car sent) :id))
            (aob-acp--resolve s (car (aob-session-decisions s)) "sub-login")
            (aob-tests--await (lambda () (eq (aob-session-state s) 'failed)))
            (should (= 1 (length (aob-tests--sent-method sent "session/new"))))
            (should (string-match-p "fake's login Subscription did not finish"
                                    (aob-session-ref s :fail-reason))))
        (ignore-errors (kill-buffer "*login fake*"))))))

(ert-deftest aob-auth-older-terminal-login-authenticates-then-retries ()
  (aob-tests--with-session s
    (let ((aob-acp-command-function #'identity))
      (aob-tests--auth-conn s)
      (unwind-protect
          (aob-tests--capturing sent
            (aob-tests--auth-open s)
            (aob-tests--auth-refuse s (plist-get (car sent) :id))
            (aob-acp--resolve s (car (aob-session-decisions s)) "old-login")
            (aob-tests--await (lambda () (aob-tests--sent-method sent "authenticate")))
            (should (equal (plist-get (car sent) :params) '(:methodId "old-login")))
            (should (= 1 (length (aob-tests--sent-method sent "session/new"))))
            (aob-tests--reply s nil)
            (should (= 2 (length (aob-tests--sent-method sent "session/new")))))
        (ignore-errors (kill-buffer "*login fake*"))))))

(ert-deftest aob-auth-agent-login-authenticates-and-says-how-it-went ()
  (aob-tests--with-session s
    (aob-tests--auth-conn s)
    (aob-tests--capturing sent
      (aob-tests--auth-open s)
      (aob-tests--auth-refuse s (plist-get (car sent) :id))
      (aob-acp--resolve s (car (aob-session-decisions s)) "api-key")
      (should (equal (plist-get (car sent) :method) "authenticate"))
      (should (equal (plist-get (car sent) :params) '(:methodId "api-key")))
      (aob-tests--feed s (format "{\"jsonrpc\":\"2.0\",\"id\":%d,\"error\":{\"code\":-32602,\"message\":\"Invalid params\"}}"
                                 (plist-get (car sent) :id)))
      (should (eq (aob-session-state s) 'failed))
      (should (equal (aob-session-ref s :fail-reason)
                     "fake could not log in with API Key (Invalid params); pick another login, or log in with fake's own CLI")))))

(ert-deftest aob-auth-status-update-shows-on-every-session-of-the-connection ()
  (aob-tests--with-session s
    (let ((s2 (aob-create-session :id "acp:test:2" :backend 'acp :name "test:2"
                                  :project "/tmp/proj/" :dir "/tmp/proj/" :state 'idle)))
      (unwind-protect
          (progn
            (setf (aob-session-conn s2) (aob-session-conn s))
            (aob-acp--register (aob-session-conn s) "sess-2" s2)
            (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"_auth/status_update\",\"params\":{\"authStatus\":{\"kind\":\"account\",\"label\":\"Claude Max\",\"account\":{\"plan\":\"max\",\"email\":\"a@b.c\"}}}}")
            (dolist (x (list s s2))
              (should (equal (aob-session-ref x :auth-label) "Claude Max (a@b.c)")))
            (should (string-match-p "Claude Max (a@b\\.c)" (aob-trace--header s)))
            (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"_auth/status_update\",\"params\":{\"authStatus\":{\"kind\":\"none\",\"label\":\"Not logged in\"}}}")
            (should (eq (get-text-property 0 'face (aob-session-ref s2 :auth-label)) 'warning)))
        (aob-remove-session s2)))))

(defconst aob-tests--url-ask
  '(:sessionId "sess-test" :mode "url" :elicitationId "e1"
    :url "https://example.test/device"
    :message "Sign in to ChatGPT and enter this code: ABCD-1234")
  "A url elicitation as codex-acp sends it for its device-code login.")

(ert-deftest aob-url-elicitation-accept-opens-the-page ()
  (aob-tests--with-session s
    (let (opened)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (push url opened))))
        (aob-tests--capturing sent
          (aob-tests--request s 71 "elicitation/create" aob-tests--url-ask)
          (let ((d (car (aob-session-decisions s))))
            (should (eq (plist-get d :kind) 'url))
            (should (equal (plist-get d :title) "Sign in to ChatGPT and enter this code: ABCD-1234"))
            (aob-acp--resolve s d "accept"))
          (should (equal opened '("https://example.test/device")))
          (should (equal (aob-tests--replies sent)
                         '("{\"jsonrpc\":\"2.0\",\"id\":71,\"result\":{\"action\":\"accept\"}}"))))))))

(ert-deftest aob-url-elicitation-decline-opens-nothing ()
  (aob-tests--with-session s
    (let (opened)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (push url opened))))
        (aob-tests--capturing sent
          (aob-tests--request s 72 "elicitation/create" aob-tests--url-ask)
          (aob-acp--resolve s (car (aob-session-decisions s)) 'decline)
          (should-not opened)
          (should (equal (aob-tests--replies sent)
                         '("{\"jsonrpc\":\"2.0\",\"id\":72,\"result\":{\"action\":\"decline\"}}"))))))))

(ert-deftest aob-elicitation-complete-closes-the-pending-page ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--capturing sent
      (aob-tests--request s 73 "elicitation/create" aob-tests--url-ask)
      (should (eq (aob-session-state s) 'blocked))
      (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"elicitation/complete\",\"params\":{\"elicitationId\":\"e1\"}}")
      (should-not (aob-session-decisions s))
      (should (eq (aob-session-state s) 'working))
      (should (equal (aob-tests--replies sent)
                     '("{\"jsonrpc\":\"2.0\",\"id\":73,\"result\":{\"action\":\"accept\"}}"))))))

(defmacro aob-tests--recording-states (var &rest body)
  "Run BODY with each state change of a session kept in VAR as (OLD NEW),
oldest first."
  (declare (indent 1))
  `(let ((,var nil)
         (aob-state-change-hook nil))
     (add-hook 'aob-state-change-hook
               (lambda (_s old new) (setq ,var (append ,var (list (list old new))))))
     ,@body))

(ert-deftest aob-url-elicitation-on-an-idle-session-leaves-it-idle ()
  "A page asked for outside a turn, as codex's device-code login does,
returns the session to idle when answered or completed, never to working."
  (cl-letf (((symbol-function 'browse-url) #'ignore))
    (dolist (finish (list (lambda (s) (aob-acp--resolve s (car (aob-session-decisions s)) "accept"))
                          (lambda (s) (aob-acp--resolve s (car (aob-session-decisions s)) 'decline))
                          (lambda (s) (aob-tests--feed s "{\"jsonrpc\":\"2.0\",\"method\":\"elicitation/complete\",\"params\":{\"elicitationId\":\"e1\"}}"))))
      (aob-tests--with-session s
        (aob-set-state s 'idle)
        (aob-tests--capturing _sent
          (aob-tests--recording-states states
            (aob-tests--request s 75 "elicitation/create" aob-tests--url-ask)
            (funcall finish s)
            (should (equal states '((idle blocked) (blocked idle))))))))))

(ert-deftest aob-auth-cancel-while-opening-fails-the-open ()
  "Cancelling the login offered while session/new waits fails the session
with why, and sends no session/cancel for a session the agent never opened."
  (aob-tests--with-session s
    (aob-tests--auth-conn s)
    (aob-tests--capturing sent
      (aob-tests--auth-open s)
      (aob-tests--auth-refuse s (plist-get (car (aob-tests--sent-method sent "session/new")) :id))
      (should (eq (aob-session-state s) 'blocked))
      (aob-tests--recording-states states
        (aob-acp--cancel s)
        (should (equal states '((blocked failed)))))
      (should-not (aob-session-decisions s))
      (should (equal (aob-session-ref s :fail-reason) "login cancelled"))
      (should-not (aob-tests--sent-method sent "session/cancel")))))

(ert-deftest aob-auth-cancel-after-a-refused-turn-goes-back-to-idle ()
  (aob-tests--with-session s
    (aob-tests--auth-conn s)
    (aob-set-state s 'idle)
    (aob-tests--capturing sent
      (aob-tests--recording-states states
        (aob-acp--auth-offer s '(:code -32000 :message "Authentication required") nil)
        (aob-acp--cancel s)
        (should (equal states '((idle blocked) (blocked idle)))))
      (should-not (aob-session-decisions s))
      (should-not (aob-tests--sent-method sent "session/cancel")))))

(ert-deftest aob-auth-login-after-a-refused-turn-never-passes-through-working ()
  "No turn runs while the agent logs in, so the session goes back to idle
once and stays there; a working → idle pair would end a turn twice."
  (aob-tests--with-session s
    (aob-tests--auth-conn s)
    (aob-set-state s 'idle)
    (aob-tests--capturing sent
      (aob-tests--recording-states states
        (aob-acp--auth-offer s '(:code -32000 :message "Authentication required") nil)
        (aob-acp--resolve s (car (aob-session-decisions s)) "api-key")
        (should (aob-tests--sent-method sent "authenticate"))
        (aob-tests--reply s nil)
        (should (equal states '((idle blocked) (blocked idle))))))))

(ert-deftest aob-request-scoped-elicitation-finds-a-session ()
  "A form scoped to a request no session sent, with two sessions on the
connection, is asked on one of them rather than refused as unknown."
  (aob-tests--with-session s
    (let ((s2 (aob-create-session :id "acp:test:2" :backend 'acp :name "test:2"
                                  :project "/tmp/proj/" :dir "/tmp/proj/" :state 'idle)))
      (unwind-protect
          (aob-tests--capturing sent
            (setf (aob-session-conn s2) (aob-session-conn s))
            (aob-acp--register (aob-session-conn s) "sess-2" s2)
            (aob-tests--request s 74 "elicitation/create"
                                '(:requestId 99 :mode "form" :message "Which org?"
                                  :requestedSchema (:type "object"
                                                    :properties (:org (:type "string")))))
            (should-not (seq-filter (lambda (m) (plist-member m :error)) sent))
            (should (seq-find (lambda (x)
                                (equal (plist-get (car (aob-session-decisions x)) :reply-id) 74))
                              (list s s2))))
        (aob-remove-session s2)))))

(defun aob-tests--sent-initialize (s)
  "The clientCapabilities S's connection declares in the initialize it sends."
  (aob-tests--capturing sent
    (aob-acp--initialize (aob-session-conn s))
    (plist-get (plist-get (seq-find (lambda (m) (equal (plist-get m :method) "initialize"))
                                    sent)
                          :params)
               :clientCapabilities)))

(defun aob-tests--events-of (s type)
  "S's events of TYPE, oldest first."
  (reverse (seq-filter (lambda (e) (eq (plist-get e :type) type))
                       (aob-session-events s))))

(ert-deftest aob-acp-terminal-output-streams-into-the-tool-call ()
  "Declared terminal_output_delta, a command's output is appended to its
tool call as it streams, and a final update that repeats it as content
is not shown twice."
  (aob-tests--with-session s
    (should (eq t (plist-get (plist-get (aob-tests--sent-initialize s) :_meta)
                             :terminal_output_delta)))
    (aob-tests--update s '(:sessionUpdate "tool_call" :toolCallId "t1" :kind "execute"
                           :title "ls" :status "in_progress" :rawInput (:command "ls")
                           :_meta (:terminal_info (:terminal_id "t1"))))
    (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "t1"
                           :_meta (:terminal_output_delta (:terminal_id "t1" :data "a.txt\n"))))
    (let ((ev (car (aob-tests--events-of s 'tool))))
      (should (equal (aob-trace--shell-output ev) '("a.txt")))
      (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "t1"
                             :_meta (:terminal_output_delta (:terminal_id "t1" :data "b.txt\n"))))
      (should (equal (aob-trace--shell-output ev) '("a.txt" "b.txt")))
      (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "t1" :status "completed"
                             :content [(:type "content"
                                        :content (:type "text"
                                                  :text "```console\na.txt\nb.txt\n```"))]
                             :_meta (:terminal_exit (:terminal_id "t1" :exit_code 2))))
      (should (equal (aob-trace--shell-output ev) '("a.txt" "b.txt")))
      (should (= (aob-trace--exit-code ev (aob-trace--shell-output ev)) 2))
      (should (= 1 (with-temp-buffer
                     (insert (aob-trace--tail-block ev))
                     (count-matches "a\\.txt" (point-min) (point-max)))))
      (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "t1"
                             :content [(:type "terminal" :terminalId "t1")]))
      (should (equal (aob-trace--shell-output ev) '("a.txt" "b.txt"))))))

(ert-deftest aob-acp-replayed-user-chunks-become-prompts ()
  "A load's user_message_chunk is your prompt again, a turn of its own;
the echo of a prompt sent live is not a second one."
  (aob-tests--with-session s
    (dolist (u '((:sessionUpdate "user_message_chunk" :content (:type "text" :text "hello "))
                 (:sessionUpdate "user_message_chunk" :content (:type "text" :text "there"))
                 (:sessionUpdate "agent_message_chunk" :content (:type "text" :text "hi"))
                 (:sessionUpdate "user_message_chunk" :content (:type "text" :text "again"))
                 (:sessionUpdate "agent_message_chunk" :content (:type "text" :text "ok"))))
      (aob-tests--update s u))
    (let ((prompts (aob-tests--events-of s 'prompt)))
      (should (equal (mapcar #'aob-event-text prompts) '("hello there" "again")))
      (should (seq-every-p (lambda (p) (plist-get p :typed)) prompts)))
    (should (equal (mapcar #'aob-event-text (aob-tests--events-of s 'message))
                   '("hi" "ok")))
    (aob-set-state s 'idle)
    (aob-tests--capturing _sent
      (aob-acp--prompt-1 s "live words"))
    (aob-tests--update s '(:sessionUpdate "user_message_chunk"
                           :content (:type "text" :text "live words")))
    (should (equal (mapcar #'aob-event-text (aob-tests--events-of s 'prompt))
                   '("hello there" "again" "live words")))))

(ert-deftest aob-acp-short-stops-warn ()
  "refusal, max_tokens and max_turn_requests end a turn as a warning in the
trace and the header; a turn that ends as usual clears it."
  (aob-tests--with-session s
    (dolist (reason '("refusal" "max_tokens" "max_turn_requests" "end_turn"))
      (aob-set-state s 'idle)
      (aob-tests--capturing sent
        (aob-acp--prompt-1 s "go")
        (aob-tests--answer s (car sent) (list :stopReason reason)))
      (let* ((stop (car (last (aob-tests--events-of s 'stop))))
             (summary (aob-event-summary stop))
             (header (aob-trace--header s))
             (warning (aob-session-ref s :stop-warning)))
        (if (equal reason "end_turn")
            (progn (should (equal summary "done (end_turn)"))
                   (should-not warning)
                   (should-not (text-property-any 0 (length header) 'face 'warning header)))
          (should (string-prefix-p "stopped: " summary))
          (should (equal (plist-get stop :warning) warning))
          (should (string-search warning header))
          (should (eq 'warning (get-text-property (string-search warning header)
                                                  'face header))))))
    (with-current-buffer (aob-trace-buffer s)
      (unwind-protect
          (progn (aob-trace--render t)
                 (goto-char (point-min))
                 (search-forward "stopped: the model refused")
                 (should (memq 'warning (ensure-list (get-text-property (match-beginning 0)
                                                                        'font-lock-face)))))
        (kill-buffer)))))

(ert-deftest aob-acp-content-blocks-render-or-say-so ()
  "Every kind of block an agent sends is shown or named: a picture is
drawn where it can be, a link follows, a resource shows its text."
  (aob-tests--with-session s
    (dolist (block '((:type "text" :text "see ")
                     (:type "image" :mimeType "image/png" :data "iVBORw0KGgo=")
                     (:type "resource_link" :uri "file:///tmp/proj/a.el" :name "a.el")
                     (:type "resource_link" :uri "https://example.com/x" :name "x")
                     (:type "resource" :resource (:uri "file:///tmp/proj/b.txt" :text "body"))
                     (:type "audio" :mimeType "audio/wav" :data "AAAA")
                     (:type "hologram")))
      (aob-tests--update s (list :sessionUpdate "agent_message_chunk" :content block)))
    (let* ((msg (car (aob-tests--events-of s 'message)))
           (text (aob-event-text msg)))
      (should (= 1 (length (aob-tests--events-of s 'message))))
      (dolist (shown '("see " "[[Image]]" "[[a.el]]" "[[x]]" "[[/tmp/proj/b.txt]]" "body"
                       "[[Audio]]" "[[hologram]]"))
        (should (string-search shown text)))
      (should (equal (get-text-property (string-search "[[a.el]]" text) 'aob-file text)
                     '("/tmp/proj/a.el" nil nil)))
      (should (get-text-property (string-search "[[x]]" text) 'button text))
      (should (= 1 (length (plist-get msg :image-data))))
      (should (string-empty-p (aob-trace--images-of msg)))
      (cl-letf (((symbol-function 'display-graphic-p) #'always)
                ((symbol-function 'create-image) (lambda (&rest _) '(image :type png))))
        (let ((aob-trace-icons t))
          (should (equal (get-text-property 1 'display (aob-trace--images-of msg))
                         '(image :type png))))))))

(ert-deftest aob-acp-notices-and-compaction-are-status ()
  "A notice and a compaction are status lines, never the agent's words, and
initialize declares both."
  (aob-tests--with-session s
    (let ((session (plist-get (aob-tests--sent-initialize s) :session)))
      (should (hash-table-p (plist-get session :notices)))
      (should (hash-table-p (plist-get session :compaction))))
    (aob-tests--update s '(:sessionUpdate "notice" :severity "warning"
                           :title "Model fallback" :description "Using sonnet"))
    (aob-tests--update s '(:sessionUpdate "compaction_update" :compactionId "c1"
                           :status "in_progress"))
    (aob-tests--update s '(:sessionUpdate "compaction_summary_chunk" :compactionId "c1"
                           :content (:type "text" :text "Summary here")))
    (aob-tests--update s '(:sessionUpdate "compaction_update" :compactionId "c1"
                           :status "completed"))
    (should-not (aob-tests--events-of s 'message))
    (let ((states (aob-tests--events-of s 'state)))
      (should (= 2 (length states)))
      (should (equal (plist-get (car states) :title) "Model fallback: Using sonnet"))
      (should (plist-get (car states) :warning))
      (should (equal (plist-get (cadr states) :title) "context compacted"))
      (should (equal (aob-event-text (cadr states)) "Summary here")))))

;;; Bringing a conversation back keeps the trace you were reading

(defmacro aob-tests--reviving (spec &rest body)
  "Run BODY with `aob-acp--connect' doing what SPEC says, held in a window.
SPEC is `quiet' (nothing answers), `fail' (the agent refuses) or
`signal' (the connection cannot be made).  BODY sees `listed', which
says whether the conversation was in the registry while it opened, and
`dir', a folder that exists."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "aob-revive" t)))
          (aob-acp-persist-file nil)
          (listed 'never-asked))
     (unwind-protect
         (cl-letf (((symbol-function 'aob-acp--connect)
                    (lambda (sess &rest _)
                      (setq listed (and (seq-find (lambda (x)
                                                    (equal (aob-session-ref x :acp-id)
                                                           (aob-session-ref sess :acp-id)))
                                                  (aob-sessions))
                                        t))
                      (pcase ,spec
                        ('fail (aob-acp--fail sess '(:message "would not open")))
                        ('signal (error "no such agent"))))))
           (save-window-excursion ,@body))
       (dolist (x (aob-sessions))
         (when (string-prefix-p "revive-" (or (aob-session-ref x :acp-id) ""))
           (aob-remove-session x)))
       (aob-tests--kill-views)
       (delete-directory dir t))))

(defun aob-tests--dead-session (dir acp-id)
  "A dead session on DIR that ran conversation ACP-ID, its trace on screen."
  (let ((s (aob-create-session :id (concat "acp:" acp-id) :backend 'acp
                               :name acp-id :project dir :dir dir :state 'dead
                               :refs (list :agent "claude" :acp-id acp-id))))
    (aob-event s 'message :text "what was said before")
    (set-window-buffer (selected-window) (aob-trace-buffer s))
    s))

(defun aob-tests--asleep-session (dir acp-id)
  "A conversation ACP-ID on DIR opened for reading, its trace on screen.
No transcript on disk: what it shows is only what it holds."
  (let ((s (aob-create-session
            :id (concat "acp:" acp-id) :backend 'acp :name "claude:9 · Sep 30"
            :project dir :dir dir :state 'done
            :refs (list :agent "claude" :acp-id acp-id
                        :asleep (list :agent "claude" :name "claude:9" :dir dir
                                      :project dir :acp-id acp-id)))))
    (aob-event s 'message :text "what was said before")
    (set-window-buffer (selected-window) (aob-trace-buffer s))
    s))

(defun aob-tests--holding (s buf)
  "Assert S is the one session for its conversation, BUF its trace on screen."
  (should (eq (aob-session-get (aob-session-id s)) s))
  (should (= 1 (seq-count (lambda (x) (equal (aob-session-ref x :acp-id)
                                             (aob-session-ref s :acp-id)))
                          (aob-sessions))))
  (should (buffer-live-p buf))
  (should (eq (aob-trace-buffer s) buf))
  (should (eq (window-buffer (selected-window)) buf))
  (should (string-match-p "what was said before"
                          (with-current-buffer buf (buffer-string)))))

(ert-deftest aob-a-restart-keeps-the-trace-and-the-window-it-was-in ()
  (aob-tests--reviving 'quiet
    (let* ((s (aob-tests--dead-session dir "revive-r1"))
           (buf (aob-trace-buffer s))
           (live (aob-acp-restart s)))
      (should (eq listed t))
      (should-not (eq live s))
      (should (eq (aob-session-state live) 'starting))
      (should (equal (aob-session-name live) "revive-r1"))
      (aob-tests--holding live buf))))

(ert-deftest aob-a-restart-the-agent-refuses-stays-listed-failed-in-its-trace ()
  (aob-tests--reviving 'fail
    (let* ((s (aob-tests--dead-session dir "revive-r2"))
           (buf (aob-trace-buffer s))
           (live (aob-acp-restart s)))
      (should (eq listed t))
      (should (eq (aob-session-state live) 'failed))
      (aob-tests--holding live buf)
      ;; and failed with its id, so it can be restarted again
      (should (equal (plist-get (aob-acp--entry live) :acp-id) "revive-r2")))))

(ert-deftest aob-a-restart-that-cannot-start-leaves-the-session-where-it-was ()
  (aob-tests--reviving 'signal
    (let* ((s (aob-tests--dead-session dir "revive-r3"))
           (buf (aob-trace-buffer s)))
      (should-error (aob-acp-restart s))
      (should (eq listed t))
      (should-not (aob-session-ref s :restarting))
      (aob-tests--holding s buf))))

(ert-deftest aob-resuming-a-session-failed-on-a-live-agent-lets-the-agent-go ()
  ;; a turn error fails the session but leaves its agent up: bringing it
  ;; back must answer what it held, stop it and drop its connection,
  ;; and still come back in the trace it failed in
  (aob-tests--reviving 'quiet
    (let* ((proc (make-process :name "aob-test-cat" :command '("cat")
                               :connection-type 'pipe :noquery t))
           (s (aob-tests--dead-session dir "revive-f1"))
           (buf (aob-trace-buffer s))
           (table (make-hash-table :test #'equal))
           answered told reaped)
      (unwind-protect
          (progn
            (process-put proc 'aob-sessions table)
            (setf (aob-session-conn s) proc)
            (aob-acp--register proc "revive-f1" s)
            (aob-set-state s 'failed)
            (setf (aob-session-decisions s)
                  (list (list :reply-id 7 :kind 'permission :title "Edit a.el")))
            (cl-letf (((symbol-function 'aob-acp--respond)
                       (lambda (_s id result &rest _) (push (cons id result) answered)))
                      ((symbol-function 'aob-acp--notify)
                       (lambda (_s method &rest _) (push method told)))
                      ((symbol-function 'aob-acp--kill-tree)
                       (lambda (p) (push p reaped))))
              (let ((live (aob-acp-resume-entry (aob-acp--entry s))))
                (should (eq listed t))
                (should-not (eq live s))
                (should (equal answered '((7 :outcome (:outcome "cancelled")))))
                (should (member "session/cancel" told))
                (should-not (gethash "revive-f1" table))
                (should (equal reaped (list proc)))
                (aob-tests--holding live buf))))
        (delete-process proc)))))

(ert-deftest aob-a-session-brought-back-keeps-its-row ()
  ;; a restart or a wake is the same conversation: its row stays put
  ;; rather than jumping to the top of every list
  (aob-tests--reviving 'quiet
    (let* ((top (aob-tests--dead-session dir "revive-o1"))
           (mid (aob-tests--dead-session dir "revive-o2"))
           (low (aob-tests--asleep-session dir "revive-o3"))
           (bottom (aob-tests--dead-session dir "revive-o4"))
           (rows (lambda ()
                   (seq-filter (lambda (id) (string-search "revive-o" id))
                               (mapcar (lambda (s) (aob-session-ref s :acp-id))
                                       (aob-sessions)))))
           (before (funcall rows)))
      (ignore top bottom)
      (should (equal before '("revive-o4" "revive-o3" "revive-o2" "revive-o1")))
      (aob-acp-restart mid)
      (should (eq listed t))
      (should (equal (funcall rows) before))
      (aob-transcript-wake low)
      (should (equal (funcall rows) before)))))

(ert-deftest aob-waking-a-conversation-keeps-the-trace-you-were-reading ()
  (aob-tests--reviving 'quiet
    (let* ((s (aob-tests--asleep-session dir "revive-w1"))
           (buf (aob-trace-buffer s))
           (live (aob-transcript-wake s)))
      (should (eq listed t))
      (should-not (aob-session-get (aob-session-id s)))
      (should-not (aob-session-ref live :asleep))
      (should (equal (buffer-name buf) (aob-trace--name live)))
      (aob-tests--holding live buf))))

(ert-deftest aob-writing-to-a-sleeping-conversation-wakes-it-in-its-own-trace ()
  (aob-tests--reviving 'quiet
    (let* ((s (aob-tests--asleep-session dir "revive-w2"))
           (buf (aob-trace-buffer s)))
      (aob-prompt s "and now?")
      (let ((live (aob-session-get (buffer-local-value 'aob-trace--session-id buf))))
        (should (eq listed t))
        (should-not (eq live s))
        (should (seq-find (lambda (ev) (equal (plist-get ev :text) "and now?"))
                          (aob-session-events live)))
        (aob-tests--holding live buf)))))

(ert-deftest aob-a-conversation-that-will-not-wake-stays-listed-failed ()
  (aob-tests--reviving 'fail
    (let* ((s (aob-tests--asleep-session dir "revive-w3"))
           (buf (aob-trace-buffer s))
           (live (aob-transcript-wake s)))
      (should (eq listed t))
      (should (eq (aob-session-state live) 'failed))
      (aob-tests--holding live buf))))

(ert-deftest aob-a-conversation-that-cannot-start-stays-asleep-in-its-trace ()
  (aob-tests--reviving 'signal
    (let* ((s (aob-tests--asleep-session dir "revive-w4"))
           (buf (aob-trace-buffer s)))
      (should-error (aob-transcript-wake s))
      (should (aob-session-ref s :asleep))
      (aob-tests--holding s buf))))

(ert-deftest aob-modal-waking-a-conversation-leaves-you-where-you-read-it ()
  "With the glue that puts traces on screen and makes spaces: the window
the trace was read in still shows it, and no space is landed in."
  (aob-tests--modal
   '(progn
      (require 'yggdrasil-spacetree nil t)
      (let ((noninteractive nil))
        (aob-tests--reviving 'quiet
          (let* ((s (aob-tests--asleep-session dir "revive-m1"))
                 (buf (aob-trace-buffer s))
                 (wins (get-buffer-window-list buf nil t))
                 (tabs (lambda () (list (tab-bar--current-tab-index)
                                        (length (funcall tab-bar-tabs-function)))))
                 (tab (funcall tabs))
                 (live (aob-transcript-wake s)))
            (should wins)
            (dolist (w wins)
              (should (window-live-p w))
              (should (eq (window-buffer w) buf)))
            (should (equal tab (funcall tabs)))
            (aob-tests--holding live buf)))))))

(ert-deftest aob-a-conversation-brought-back-is-written-down-once ()
  (dolist (how '(restart wake))
    (aob-tests--reviving 'quiet
      (let* ((aob-acp-persist-file (make-temp-file "aob-revive-" nil ".eld"))
             (aob-acp--opened-any t)
             (acp-id (format "revive-p-%s" how)))
        (unwind-protect
            (progn
              (if (eq how 'restart)
                  (aob-acp-restart (aob-tests--dead-session dir acp-id))
                (aob-transcript-wake (aob-tests--asleep-session dir acp-id)))
              (should (= 1 (seq-count (lambda (e) (equal (plist-get e :acp-id) acp-id))
                                      (aob-acp--persisted-entries)))))
          (delete-file aob-acp-persist-file))))))

(require 'ygg-agent-conf)

(defun aob-tests--with-config-homes (fn)
  "Call FN with a git project, under config homes of a scratch root.
FN gets the project.  The keychain answers as a home logged in, with no
other homes to share from, so nothing reaches the real one."
  (let* ((root (make-temp-file "aob-conf-homes" t))
         (home (expand-file-name "home/claude" root))
         (project (file-name-as-directory (expand-file-name "repo" root)))
         (ygg-agent-conf-root (expand-file-name "conf" root))
         (ygg-agent--config-homes
          `(("claude" :var "CLAUDE_CONFIG_DIR" :marker ".claude-config-dir"
             :home ,home :share ("skills"))))
         (ygg-agent--login-cache (make-hash-table :test #'equal))
         (ygg-agent--config-dirs (make-hash-table :test #'equal))
         (aob-acp-environment-function
          (lambda (agent project _dir &optional _isolate)
            (when-let* ((env (ygg-agent--known-config-env agent agent project)))
              (list env))))
         (aob-acp-prepare-function
          (lambda (agent project &optional _isolate)
            (ygg-agent--config-env agent agent project))))
    (make-directory (expand-file-name "skills" home) t)
    (make-directory project t)
    (call-process "git" nil nil nil "-C" project "init" "-q")
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-agent--keychain-read)
                   (lambda (_service) (let ((json (make-hash-table :test #'equal)))
                                        (puthash "claudeAiOauth" t json)
                                        json)))
                  ((symbol-function 'ygg-agent--credential-services) #'ignore)
                  ((symbol-function 'ygg-agent--keychain-account) #'ignore))
          (funcall fn project))
      (delete-directory root t))))

(defun aob-tests--conn-home (project)
  "The config home the connection key for claude on PROJECT was filed with."
  (car (funcall aob-acp-environment-function "claude" project project nil)))

(ert-deftest aob-acp-conn-key-runs-no-process ()
  "Once a connection has started, asking for its key spawns nothing and makes nothing."
  (aob-tests--with-config-homes
   (lambda (project)
     (funcall aob-acp-prepare-function "claude" project)
     (let ((key (aob-acp--conn-key "claude" project))
           (spawned 0) (made 0))
       (cl-letf (((symbol-function 'call-process)
                  (lambda (&rest _) (setq spawned (1+ spawned)) 0))
                 ((symbol-function 'ygg-agent--bootstrap-share)
                  (lambda (&rest _) (setq made (1+ made)))))
         (dotimes (_ 50)
           (should (equal key (aob-acp--conn-key "claude" project)))))
       (should (= 0 spawned))
       (should (= 0 made))
       (should (equal (ygg-agent--config-env "claude" "claude" project)
                      (aob-tests--conn-home project)))))))

(ert-deftest aob-acp-conn-key-follows-what-names-the-home ()
  "A marker file appearing, or a login check changing its answer, renames the home."
  (aob-tests--with-config-homes
   (lambda (project)
     (funcall aob-acp-prepare-function "claude" project)
     (should (string-match-p "/conf/repo/claude\\'" (aob-tests--conn-home project)))
     (let ((other (expand-file-name "elsewhere" project)))
       (with-temp-file (expand-file-name ".claude-config-dir" project)
         (insert other "\n"))
       (should (equal (concat "CLAUDE_CONFIG_DIR=" other)
                      (aob-tests--conn-home project)))
       (delete-file (expand-file-name ".claude-config-dir" project)))
     (should (string-match-p "/conf/repo/claude\\'" (aob-tests--conn-home project)))
     (cl-letf (((symbol-function 'ygg-agent--keychain-read) #'ignore))
       (ygg-agent--logged-in-p "claude" (ygg-agent--own-home "claude" project)))
     (should-not (aob-tests--conn-home project)))))

(ert-deftest aob-acp-start-conn-makes-the-home-ready ()
  "Starting a connection provisions its config home; looking one up does not."
  (aob-tests--with-config-homes
   (lambda (project)
     (let ((aob-acp-agents '(("claude" :command ("cat"))))
           (aob-acp-command-function #'identity)
           (aob-acp--session-env nil)
           (aob-acp-isolate nil)
           (made nil) proc)
       (cl-letf* ((real (symbol-function 'ygg-agent--bootstrap-share))
                  ((symbol-function 'ygg-agent--bootstrap-share)
                   (lambda (spec dir) (push dir made) (funcall real spec dir))))
         (aob-acp--conn-key "claude" project)
         (should-not made)
         (unwind-protect
             (progn
               (setq proc (aob-acp--start-conn "claude" project))
               (should (= 1 (length made)))
               (should (file-symlink-p (expand-file-name "skills" (car made))))
               (should (member (aob-tests--conn-home project)
                               (process-get proc 'aob-env)))
               (should (eq proc (aob-acp--live-conn "claude" project))))
           (when proc
             (remhash (process-get proc 'aob-conn-key) aob-acp--conns)
             (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
             (ignore-errors (kill-buffer (process-get proc 'aob-stderr-buf)))
             (delete-process proc))))))))

(defun aob-tests--shell-meta (ev)
  "The right-hand words of EV's command card."
  (with-temp-buffer
    (car (split-string (substring-no-properties (aob-trace--shell-card ev)) "\n"))))

(ert-deftest aob-shells-command-runs-on-after-its-turn ()
  "A command still running when its turn ends is a background command: its
row stays running with its time and keeps taking what it prints."
  (aob-tests--with-session s
    (aob-tests--update s '(:sessionUpdate "tool_call" :toolCallId "c1" :kind "execute"
                           :title "npm run dev" :status "in_progress"
                           :rawInput (:command "npm run dev")
                           :_meta (:terminal_info (:terminal_id "c1" :cwd "/tmp/proj/web"))))
    (aob-set-state s 'idle)
    (aob-tests--capturing sent
      (aob-acp--prompt-1 s "start the server")
      (aob-tests--answer s (car sent) '(:stopReason "end_turn")))
    (let ((ev (car (aob-tests--events-of s 'tool))))
      (should (plist-get ev :background))
      (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "c1"
                             :_meta (:terminal_output_delta (:terminal_id "c1"
                                                             :data "ready on :3000\n"))))
      (should (aob-trace-shell-live-p ev))
      (should (eq (aob-trace--state ev) 'running))
      (should (equal (aob-trace--shell-output ev) '("ready on :3000")))
      (should (equal (plist-get ev :cwd) "/tmp/proj/web"))
      (should (string-match-p "background · running · [0-9.]+m?s"
                              (aob-tests--shell-meta ev)))
      (aob-tests--update s '(:sessionUpdate "tool_call_update" :toolCallId "c1"
                             :status "completed"
                             :_meta (:terminal_exit (:terminal_id "c1" :exit_code 130))))
      (should-not (aob-trace-shell-live-p ev))
      (should (string-match-p "exit 130" (aob-tests--shell-meta ev))))))

(defun aob-tests--claude-handoff (s id)
  "Feed S claude's Bash call ID that hands sleep 300 to the background."
  (aob-tests--update s `(:sessionUpdate "tool_call" :toolCallId ,id :kind "execute"
                         :title "sleep 300" :status "pending" :rawInput (:command "sleep 300")))
  (aob-tests--update s `(:sessionUpdate "tool_call_update" :toolCallId ,id
                         :_meta (:terminal_output_delta
                                 (:terminal_id ,id
                                  :data "Command running in background with ID: bx7q2. Output is being written to: /tmp/claude/tasks/bx7q2.output. You will be notified when it completes."))))
  (aob-tests--update s `(:sessionUpdate "tool_call_update" :toolCallId ,id
                         :status "completed"
                         :_meta (:terminal_exit (:terminal_id ,id :exit_code 0))))
  (seq-find (lambda (e) (equal (plist-get e :tool-id) id)) (aob-session-events s)))

(ert-deftest aob-shells-claude-handoff-keeps-running ()
  "Claude completes a Bash call it handed to the background; the command
it names stays running, with the task and the file its output goes to.
The same call replayed by a load is history, not a command running."
  (aob-tests--with-session s
    (let ((replayed (aob-tests--claude-handoff s "toolu_0")))
      (should-not (plist-get replayed :background))
      (should-not (aob-trace-shell-live-p replayed)))
    (aob-set-state s 'working)
    (let ((ev (aob-tests--claude-handoff s "toolu_1")))
      (should (equal (plist-get ev :background) "bx7q2"))
      (should (equal (plist-get ev :output-file) "/tmp/claude/tasks/bx7q2.output"))
      (should (aob-trace-shell-live-p ev))
      (should (eq (aob-trace--state ev) 'running)))))

(ert-deftest aob-shells-lists-commands-of-every-session ()
  "The list holds each running command of every session, and nothing that ended."
  (skip-unless (featurep 'aob-shells))
  (aob-tests--with-session a
    (aob-tests--with-session b
      (aob-tests--update a '(:sessionUpdate "tool_call" :toolCallId "x1" :kind "execute"
                             :title "make watch" :status "in_progress"
                             :rawInput (:command "make watch")))
      (aob-tests--update b '(:sessionUpdate "tool_call" :toolCallId "y1" :kind "execute"
                             :title "cargo test" :status "in_progress"
                             :rawInput (:command "cargo test")))
      (aob-tests--update b '(:sessionUpdate "tool_call" :toolCallId "y2" :kind "execute"
                             :title "ls" :status "completed" :rawInput (:command "ls")))
      (let ((live (aob-shells--live)))
        (should (equal (sort (mapcar (lambda (p) (aob-trace--shell-command (cdr p))) live)
                             #'string<)
                       '("cargo test" "make watch")))
        (should (seq-set-equal-p (mapcar #'car live) (list a b) #'eq)))
      (with-temp-buffer
        (aob-shells-mode)
        (aob-shells--entries)
        (should (equal (sort (mapcar (lambda (e) (aref (cadr e) 4)) tabulated-list-entries)
                             #'string<)
                       '("cargo test" "make watch")))))))

(ert-deftest aob-shells-stop-asks-the-agent-to-stop-its-task ()
  "A command the agent reports as a task is stopped by the agent, with
_session/async_task/stop, and its row in the trace says stopped."
  (skip-unless (featurep 'aob-shells))
  (aob-tests--with-trace-session s
    (process-put (aob-session-conn s) 'aob-async-tasks-offered t)
    (aob-tests--update s '(:sessionUpdate "tool_call" :toolCallId "item_9" :kind "execute"
                           :title "tail -f log" :status "in_progress"
                           :rawInput (:command "tail -f log")))
    (aob-tests--update s '(:sessionUpdate "async_task_spawned" :asyncTaskId "sess-test:item_9"
                           :name "tail -f log" :taskType "shell" :canStop t
                           :toolCallId "item_9"))
    (let ((ev (car (aob-tests--events-of s 'tool)))
          (summary (aob-session-summary s)))
      (should (aob-acp-task-stoppable-p s ev))
      (aob-event s 'message :text "watching it")
      (setq summary (aob-session-summary s))
      (aob-trace--render-1 s)
      (should (string-match-p "\\$ tail -f log.*running" (buffer-string)))
      (aob-tests--capturing sent
        (cl-letf (((symbol-function 'aob-shells-kill)
                   (lambda (&rest _) (error "the process must not be signalled"))))
          (aob-shells-stop (cons s ev)))
        (should (equal (plist-get (car sent) :method) "_session/async_task/stop"))
        (should (equal (plist-get (car sent) :params)
                       '(:sessionId "sess-test" :asyncTaskId "sess-test:item_9")))
        (aob-tests--update s '(:sessionUpdate "async_task_state_update"
                               :asyncTaskId "sess-test:item_9" :state "stopped"))
        (aob-tests--reply s '(:stopped t)))
      (should (eq (plist-get ev :shell-end) 'stopped))
      (should-not (aob-trace-shell-live-p ev))
      (should (eq (aob-session-summary s) summary))
      (aob-trace--render-1 s)
      (should (string-match-p "\\$ tail -f log.*stopped" (buffer-string))))))

(ert-deftest aob-shells-kill-ends-only-the-commands-process ()
  "Without a task to stop, the process running the command is ended, and
neither the agent nor its other commands are touched."
  (skip-unless (featurep 'aob-shells))
  (aob-tests--with-session s
    (let* ((cat (aob-session-conn s))
           (ev (aob-event s 'tool :tool-id "k1" :kind "execute" :status "in_progress"
                          :title "sleep 301" :raw '(:command "sleep 301")))
           (agent (make-process :name "aob-test-agent" :noquery t
                                :command '("sh" "-c" "sleep 301 & sleep 302 & wait")))
           (other nil))
      (unwind-protect
          (progn
            (setf (aob-session-conn s) agent)
            (let ((deadline (+ (float-time) 5)))
              (while (and (< (length (aob-shells--below (process-id agent)
                                                         (aob-shells--process-table)))
                             2)
                          (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (let* ((below (aob-shells--below (process-id agent) (aob-shells--process-table)))
                   (target (caar (seq-filter (lambda (row) (equal (nth 2 row) "sleep 301")) below)))
                   (pids nil))
              (setq other (caar (seq-filter (lambda (row) (equal (nth 2 row) "sleep 302")) below)))
              (should (and target other))
              (setq pids (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
                           (aob-shells-kill s ev)))
              (should (equal pids (list target)))
              (let ((deadline (+ (float-time) 5)))
                (while (and (process-attributes (car pids))
                            (not (equal (alist-get 'state (process-attributes (car pids))) "Z"))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should (or (null (process-attributes (car pids)))
                          (equal (alist-get 'state (process-attributes (car pids))) "Z"))))
            (should (process-live-p agent))
            (should (process-attributes other))
            (should-not (equal (alist-get 'state (process-attributes other)) "Z"))
            (should (eq (plist-get ev :shell-end) 'stopped))
            (should (string-match-p "stopped" (aob-tests--shell-meta ev))))
        (when other (ignore-errors (signal-process other 'KILL)))
        (ignore-errors (delete-process agent))
        (setf (aob-session-conn s) cat)))))

(defmacro aob-tests--with-tree (s dir state &rest body)
  "Bind S to a session in STATE working in DIR, a fresh folder, for BODY."
  (declare (indent 3))
  `(let* ((,dir (file-name-as-directory (make-temp-file "aob-tree" t)))
          (,s (aob-create-session :id "acp:tree:1" :backend 'acp :name "tree"
                                  :project ,dir :dir ,dir :state ,state)))
     (unwind-protect
         (progn ,@body)
       (when-let* ((buf (get-buffer (aob-trace--name ,s)))) (kill-buffer buf))
       (aob-remove-session ,s)
       (delete-directory ,dir t))))

(defun aob-tests--write (dir name text)
  "Write TEXT to NAME under DIR, making its folder, and return the file."
  (let ((file (expand-file-name name dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))
    file))

(defun aob-tests--preview ()
  "The preview hung in this trace, or nil."
  (seq-find (lambda (o) (overlay-get o 'aob-preview))
            (overlays-in (point-min) (point-max))))

(defun aob-tests--goto-chip ()
  "Put point on the first file chip in the buffer."
  (goto-char (text-property-not-all (point-min) (point-max) 'aob-file nil)))

(ert-deftest aob-trace-tab-on-a-chip-opens-the-file-around-its-line ()
  "TAB on a chip hangs the file under its row, read against the session\='s
own folder, coloured by its mode, numbered, the named line lit; TAB again
folds it, and copying never takes it."
  (aob-tests--with-tree s dir 'idle
    (aob-tests--write dir "src/a.el"
                      (mapconcat (lambda (i) (format "(defun f%d () %d)\n" i i))
                                 (number-sequence 0 39) ""))
    (aob-event s 'tool :kind "read" :status "completed" :title "Read"
               :locations (list (list :path "src/a.el" :line 25)))
    (with-current-buffer (aob-trace-buffer s)
      (let ((inhibit-read-only t) (aob-trace-icons nil))
        (aob-trace--render t)
        (aob-tests--goto-chip)
        (should (equal (car (get-text-property (point) 'aob-file))
                       (expand-file-name "src/a.el" dir)))
        (aob-trace-tab)
        (let* ((shown (overlay-get (aob-tests--preview) 'after-string))
               (lit (string-search "(defun f24 () 24)" shown))
               (dim (string-search "(defun f23 () 23)" shown)))
          (should (string-search "src/a.el:25 · 40 lines" shown))
          (should (= 20 (cl-count ?\n (substring shown (string-search "40 lines" shown)))))
          (should (string-search "25 (defun f24" shown))
          (should (memq 'highlight (ensure-list (get-text-property lit 'face shown))))
          (should-not (memq 'highlight (ensure-list (get-text-property dim 'face shown))))
          (should (memq 'font-lock-keyword-face
                        (ensure-list (get-text-property (1+ dim) 'face shown)))))
        (should-not (string-search "(defun" (filter-buffer-substring (point-min) (point-max))))
        (aob-trace-tab)
        (should-not (aob-tests--preview))))))

(ert-deftest aob-trace-a-preview-of-a-file-not-there-says-so ()
  (aob-tests--with-tree s dir 'idle
    (aob-event s 'tool :kind "read" :status "completed" :title "Read"
               :locations (list (list :path "gone.el")))
    (with-current-buffer (aob-trace-buffer s)
      (let ((inhibit-read-only t) (aob-trace-icons nil))
        (aob-trace--render t)
        (aob-tests--goto-chip)
        (aob-trace-tab)
        (should (equal (string-trim (overlay-get (aob-tests--preview) 'after-string))
                       "gone.el · not on disk"))))))

(ert-deftest aob-trace-a-preview-outlives-the-next-event ()
  "A redraw for a new event puts the preview back under the same row."
  (aob-tests--with-tree s dir 'idle
    (aob-tests--write dir "a.txt" "one\ntwo\n")
    (aob-event s 'tool :kind "read" :status "completed" :title "Read"
               :locations (list (list :path "a.txt" :line 2)))
    (with-current-buffer (aob-trace-buffer s)
      (let ((inhibit-read-only t) (aob-trace-icons nil))
        (aob-trace--render t)
        (aob-tests--goto-chip)
        (aob-trace-tab)
        (let ((before aob-trace--blocks))
          (aob-event s 'message :text "and then some")
          (aob-trace--render)
          (should-not (equal before aob-trace--blocks)))
        (should (string-search "and then some" (buffer-string)))
        (let ((ov (aob-tests--preview)))
          (should ov)
          (should (= (overlay-start ov)
                     (save-excursion (aob-tests--goto-chip) (line-end-position))))
          (should (string-search "a.txt:2 · 2 lines" (overlay-get ov 'after-string))))))))

(ert-deftest aob-trace-a-preview-refuses-binary-and-caps-large-files ()
  (let ((bin (make-temp-file "aob-bin" nil ".dat" "head\0tail"))
        (big (make-temp-file "aob-big" nil ".txt"
                             (mapconcat (lambda (i) (format "row %04d\n" i))
                                        (number-sequence 1 1000) "")))
        (aob-trace-preview-bytes 2048))
    (unwind-protect
        (progn
          (should (string-search "binary, not shown" (aob-trace--preview bin nil)))
          (should-not (string-search "head" (aob-trace--preview bin nil)))
          (let ((shown (aob-trace--preview big 3)))
            (should (string-search ":3 · 228+ lines" shown))
            (should (string-search "row 0003" shown)))
          (should (string-search "past the first 2 KB" (aob-trace--preview big 900))))
      (delete-file bin)
      (delete-file big))))

(ert-deftest aob-trace-tab-on-an-edit-chip-opens-its-diff ()
  "An edit\='s chip opens the change it made, uncut, and no file text."
  (aob-tests--with-tree s dir 'idle
    (aob-event s 'tool :kind "edit" :status "completed" :title "Edit a.el"
               :content (list (list :type "diff" :path "a.el" :oldText "keep\n"
                                    :newText (mapconcat (lambda (i) (format "added %d" i))
                                                        (number-sequence 1 30) "\n"))))
    (with-current-buffer (aob-trace-buffer s)
      (let ((inhibit-read-only t) (aob-trace-icons nil))
        (aob-trace--render t)
        (should (string-search "more line" (buffer-string)))
        (should-not (string-search "added 30" (buffer-string)))
        (aob-tests--goto-chip)
        (aob-trace-tab)
        (should-not (string-search "more line" (buffer-string)))
        (should (string-search "added 30" (buffer-string)))
        (should-not (aob-tests--preview))))))

(defmacro aob-tests--drawing (made &rest body)
  "Run BODY as if on a display, each image made pushed onto MADE as its args."
  (declare (indent 1))
  `(let ((,made nil) (aob-trace-icons t))
     (cl-letf (((symbol-function 'display-graphic-p) #'always)
               ((symbol-function 'aob-trace--agent-icon) #'ignore)
               ((symbol-function 'create-image)
                (lambda (&rest args) (push args ,made) (list 'image :n (length ,made)))))
       ,@body)))

(defun aob-tests--thumbs ()
  "The pictures drawn in this buffer, as (POS KEY DISPLAY) in order."
  (let ((pos (point-min)) out)
    (while (setq pos (text-property-not-all pos (point-max) 'aob-thumb nil))
      (push (list pos (get-text-property pos 'aob-thumb) (get-text-property pos 'display)) out)
      (setq pos (next-single-property-change pos 'aob-thumb nil (point-max))))
    (nreverse out)))

(ert-deftest aob-trace-a-finished-tool-draws-its-pictures-inline ()
  "The picture a tool returned and the screenshot it names are drawn under
its row, small, once: a redraw reuses them, a name with no file draws
nothing, TAB opens one whole and back, and copying passes over them."
  (aob-tests--with-tree s dir 'idle
    (let ((shot (aob-tests--write dir "shots/home.png" "png")))
      (aob-tests--drawing made
        (let ((ev (aob-event s 'tool :kind "other" :status "completed" :title "Screenshot"
                             :content (list (list :type "content"
                                                  :content (list :type "text"
                                                                 :text "Saved shots/home.png, not shots/gone.png"))
                                            (list :type "content"
                                                  :content (list :type "image" :mimeType "image/png"
                                                                 :data "iVBORw0KGgo="))))))
          (with-current-buffer (aob-trace-buffer s)
            (let ((inhibit-read-only t))
              (aob-trace--render t)
              (let ((thumbs (aob-tests--thumbs)))
                (should (= 2 (length thumbs)))
                (should (equal (nth 1 (nth 1 thumbs)) shot))
                (should (equal (get-text-property (car (nth 1 thumbs)) 'aob-file)
                               (list shot nil nil)))
                (should (= 2 (length made)))
                (should (seq-every-p (lambda (args) (memq :max-height args)) made))
                (plist-put ev :line nil)
                (aob-trace--render t)
                (should (= 2 (length made)))
                (should-not (string-search "[[Image]]"
                                           (filter-buffer-substring (point-min) (point-max))))
                (goto-char (car (nth 1 thumbs)))
                (aob-trace-tab)
                (should (= 3 (length made)))
                (should-not (memq :max-height (car made)))
                (should (equal (get-text-property (point) 'display) (list 'image :n 3)))
                (aob-trace-tab)
                (should (= 3 (length made)))
                (should (equal (get-text-property (point) 'display)
                               (nth 2 (nth 1 thumbs))))))))))))

(ert-deftest aob-trace-a-picture-the-agent-is-still-sending-waits-to-settle ()
  "Nothing is decoded while its message streams; it is drawn once it settles."
  (aob-tests--with-tree s dir 'working
    (aob-tests--drawing made
      (aob-event s 'message :text "look [[Image]]"
                 :image-data (list (list :type "image" :mimeType "image/png"
                                         :data "iVBORw0KGgo=")))
      (with-current-buffer (aob-trace-buffer s)
        (let ((inhibit-read-only t))
          (aob-trace--render t)
          (should-not (aob-tests--thumbs))
          (should-not made)
          (aob-event s 'tool :kind "read" :status "in_progress" :title "Read")
          (aob-trace--render)
          (should (= 1 (length (aob-tests--thumbs))))
          (should (= 1 (length made))))))))
