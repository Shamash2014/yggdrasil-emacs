;;; aob-acp-conformance-tests.el --- the connection keeps to the ACP spec -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(defvar aob-acp-persist-file)
(setq aob-acp-persist-file (make-temp-file "aob-conformance-sessions-" nil ".eld"))
(require 'aob-acp)

(defmacro aob-conf--with-conn (proc sessions &rest body)
  "Bind PROC to a fake connection and SESSIONS to two sessions bound to it."
  (declare (indent 2))
  `(let* ((,proc (make-process :name "aob-conf-cat" :command '("cat")
                               :connection-type 'pipe :noquery t))
          (,sessions
           (list (aob-create-session :id "acp:conf:1" :backend 'acp :name "conf:1"
                                     :project "/tmp/proj/" :dir "/tmp/proj/"
                                     :state 'starting)
                 (aob-create-session :id "acp:conf:2" :backend 'acp :name "conf:2"
                                     :project "/tmp/proj/" :dir "/tmp/proj/"
                                     :state 'starting))))
     (unwind-protect
         (progn
           (process-put ,proc 'aob-sessions (make-hash-table :test #'equal))
           (process-put ,proc 'aob-next-id (list 0))
           (process-put ,proc 'aob-pending (make-hash-table :test #'eql))
           (process-put ,proc 'aob-json-buf (generate-new-buffer " *aob-conf-json*"))
           (process-put ,proc 'aob-init 'pending)
           (let ((n 0))
             (dolist (s ,sessions)
               (setf (aob-session-conn s) ,proc)
               (aob-acp--register ,proc (format "sess-%d" (cl-incf n)) s)))
           (ignore ,proc ,sessions)
           ,@body)
       (ignore-errors (kill-buffer (process-get ,proc 'aob-json-buf)))
       (ignore-errors (delete-process ,proc))
       (dolist (s ,sessions)
         (when (aob-session-get (aob-session-id s))
           (aob-remove-session s))))))

(defmacro aob-conf--capturing (var &rest body)
  "Run BODY with outgoing frames collected newest-first into VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'aob-acp--send-proc)
                (lambda (_proc msg) (push msg ,var))))
       ,@body)))

(defun aob-conf--feed (proc &rest frames)
  "Feed PROC's filter FRAMES, each a string or a plist, in one chunk."
  (aob-acp--filter proc (mapconcat (lambda (f)
                                     (concat (if (stringp f) f (json-serialize f)) "\n"))
                                   frames "")))

(defun aob-conf--errors (frames)
  "The error codes in FRAMES, oldest first."
  (mapcar (lambda (f) (plist-get (plist-get f :error) :code))
          (reverse (seq-filter (lambda (f) (plist-member f :error)) frames))))

(defun aob-conf--initialize (proc sessions result)
  "Start PROC's initialize with SESSIONS waiting on it, and answer RESULT."
  (aob-conf--capturing sent
    (cl-letf (((symbol-function 'aob-acp--live-conn) (lambda (&rest _) proc)))
      (aob-acp--initialize proc)
      (dolist (s sessions)
        (aob-acp--connect s (lambda (_init) (list "session/new" (list :cwd "/tmp/proj")))
                          #'ignore))
      (aob-conf--feed proc (list :jsonrpc "2.0"
                                 :id (process-get proc 'aob-init-id)
                                 :result result))))
  (process-get proc 'aob-init))

(ert-deftest aob-conf-initialize-declares-boolean-config-options ()
  (should (string-match-p "\"session\":{\"configOptions\":{\"boolean\":{}}}"
                          (json-serialize (aob-acp--client-capabilities)))))

(ert-deftest aob-conf-version-one-is-accepted ()
  (aob-conf--with-conn proc sessions
    (should (eq 'done (car (aob-conf--initialize proc sessions
                                                 '(:protocolVersion 1)))))
    (dolist (s sessions)
      (should-not (eq 'failed (aob-session-state s))))
    (should (eq (process-sentinel proc) #'internal-default-process-sentinel))))

(ert-deftest aob-conf-another-version-fails-every-waiting-session ()
  (aob-conf--with-conn proc sessions
    (with-temp-buffer
      (insert "untouched")
      (let ((other (current-buffer)))
        (should (eq 'failed (car (aob-conf--initialize proc sessions
                                                       '(:protocolVersion 2)))))
        (should (equal (with-current-buffer other (buffer-string)) "untouched"))))
    (dolist (s sessions)
      (should (eq 'failed (aob-session-state s)))
      (should (string-match-p "ACP v2" (aob-session-ref s :fail-reason))))
    (should (eq (process-sentinel proc) #'ignore))
    (let (late)
      (aob-acp--with-init proc (lambda (_res err) (setq late err)))
      (should (string-match-p "ACP v2" (plist-get late :message))))))

(ert-deftest aob-conf-missing-version-fails ()
  (aob-conf--with-conn proc sessions
    (should (eq 'failed (car (aob-conf--initialize proc sessions
                                                   '(:agentCapabilities (:loadSession t))))))
    (dolist (s sessions)
      (should (eq 'failed (aob-session-state s)))
      (should (string-match-p "no ACP version" (aob-session-ref s :fail-reason))))))

(ert-deftest aob-conf-null-capability-reads-as-unsupported ()
  (aob-conf--with-conn proc sessions
    (aob-conf--initialize
     proc sessions
     (list :protocolVersion 1
           :agentCapabilities
           (list :loadSession :null
                 :sessionCapabilities (list :fork :null :close (make-hash-table))
                 :mcpCapabilities (list :http :null :sse t))
           :authMethods (vector :null (list :id "a" :description :null) :null)))
    (let ((init (aob-acp--init-of (car sessions))))
      (should-not (aob-acp--session-cap init :fork))
      (should (aob-acp--session-cap init :close))
      (should-not (aob-acp--session-cap init :list))
      (should-not (plist-member (plist-get init :agentCapabilities) :loadSession))
      (should-not (plist-member (plist-get (plist-get init :agentCapabilities)
                                           :mcpCapabilities)
                                :http))
      (should (equal (plist-get init :authMethods) '((:id "a")))))))

(ert-deftest aob-conf-cancel-request-answers-once ()
  (aob-conf--with-conn proc sessions
    (let ((s (car sessions)))
      (aob-set-state s 'working)
      (aob-conf--capturing sent
        (aob-conf--feed proc (list :jsonrpc "2.0" :id 7 :method "elicitation/create"
                                   :params (list :sessionId "sess-1" :mode "form"
                                                 :message "Which branch?"
                                                 :requestedSchema
                                                 (list :type "object"
                                                       :properties (make-hash-table)))))
        (should (aob-session-decisions s))
        (aob-conf--feed proc "{\"jsonrpc\":\"2.0\",\"method\":\"$/cancel_request\",\"params\":{\"requestId\":7}}")
        (should-not (aob-session-decisions s))
        (should (equal (aob-conf--errors sent) '(-32800)))
        (should (equal (plist-get (car sent) :id) 7))
        (should (equal (plist-get (plist-get (car sent) :error) :message)
                       "Request cancelled"))
        (setq sent nil)
        (aob-conf--feed proc "{\"jsonrpc\":\"2.0\",\"method\":\"$/cancel_request\",\"params\":{\"requestId\":7}}")
        (aob-conf--feed proc "{\"jsonrpc\":\"2.0\",\"method\":\"$/cancel_request\",\"params\":{\"requestId\":99}}")
        (should-not sent)))))

(ert-deftest aob-conf-unknown-method-is-not-found-before-session-lookup ()
  (aob-conf--with-conn proc sessions
    (aob-conf--capturing sent
      (aob-conf--feed proc (list :jsonrpc "2.0" :id 5 :method "fs/read_text_file"
                                 :params (list :sessionId "nobody" :path "/x")))
      (should (equal (aob-conf--errors sent) '(-32601)))
      (should (equal (plist-get (plist-get (car sent) :error) :message)
                     "Method not found"))
      (setq sent nil)
      (aob-conf--feed proc (list :jsonrpc "2.0" :method "_vendor/ping"
                                 :params (list :sessionId "nobody")))
      (should-not sent))))

(ert-deftest aob-conf-request-scoped-elicitation-finds-its-session ()
  (aob-conf--with-conn proc sessions
    (pcase-let ((`(,s1 ,s2) sessions))
      (aob-conf--capturing sent
        (aob-acp--request s2 "session/prompt" (list :sessionId "sess-2") #'ignore)
        (let ((rid (plist-get (car sent) :id)))
          (aob-conf--feed proc (list :jsonrpc "2.0" :id 11 :method "elicitation/create"
                                     :params (list :requestId rid :mode "form"
                                                   :message "Token?"
                                                   :requestedSchema
                                                   (list :type "object"
                                                         :properties (make-hash-table)))))
          (should-not (aob-conf--errors sent))
          (should-not (aob-session-decisions s1))
          (should (equal (plist-get (car (aob-session-decisions s2)) :reply-id) 11))
          (aob-conf--feed proc (list :jsonrpc "2.0" :id rid :result (make-hash-table)))
          (should-not (gethash rid (aob-acp--request-owners proc))))))))

(ert-deftest aob-conf-elicitation-mode-other-than-form-is-invalid ()
  (aob-conf--with-conn proc sessions
    (aob-conf--capturing sent
      (aob-conf--feed proc (list :jsonrpc "2.0" :id 12 :method "elicitation/create"
                                 :params (list :sessionId "sess-1" :mode "url"
                                               :url "https://example.com"
                                               :elicitationId "e1"
                                               :message "Sign in")))
      (should (equal (aob-conf--errors sent) '(-32602)))
      (should-not (aob-session-decisions (car sessions))))))

(ert-deftest aob-conf-malformed-frame-is-a-parse-error-and-reading-goes-on ()
  (aob-conf--with-conn proc sessions
    (aob-conf--capturing sent
      (aob-conf--feed proc "{not json" "  [1, oops" "   "
                      (list :jsonrpc "2.0" :id 13 :method "session/request_permission"
                            :params (list :sessionId "sess-1"
                                          :toolCall (list :toolCallId "t1" :title "Run ls")
                                          :options [])))
      (should (equal (aob-conf--errors sent) '(-32700 -32700)))
      (should (string-match-p "\"id\":null" (json-serialize (car sent))))
      (should (equal (plist-get (car (aob-session-decisions (car sessions))) :reply-id)
                     13)))))

(ert-deftest aob-conf-stdout-noise-is-dropped-quietly ()
  (aob-conf--with-conn proc sessions
    (aob-conf--capturing sent
      (aob-conf--feed proc "Starting agent v1.2 ..." "info: ready"
                      (list :jsonrpc "2.0" :id 14 :method "session/request_permission"
                            :params (list :sessionId "sess-1"
                                          :toolCall (list :toolCallId "t2" :title "Run ls")
                                          :options [])))
      (should-not sent)
      (should (equal (plist-get (car (aob-session-decisions (car sessions))) :reply-id)
                     14)))))

(ert-deftest aob-conf-steering-and-goal-read-from-agent-capabilities ()
  (aob-conf--with-conn proc sessions
    (let ((s (car sessions)))
      (aob-session-put s :agent-caps
                       '(:_meta (:steering (:supported t)
                                 :goal (:controlMethod "_session/goal"))))
      (should (aob-acp--steers-p s))
      (should (equal (aob-acp--goal-method s) "_session/goal"))
      (aob-session-put s :agent-caps '(:_meta (:goal (:controlMethod "session/goal"))))
      (should-not (aob-acp--goal-method s)))))

(ert-deftest aob-conf-claude-top-level-steering-still-detected ()
  (aob-conf--with-conn proc sessions
    (let ((s (car sessions)))
      (aob-session-put s :agent-caps '(:loadSession t))
      (aob-session-put s :agent-meta '(:steering (:supported t)
                                       :goal (:controlMethod "_session/goal")))
      (should (aob-acp--steers-p s))
      (should (equal (aob-acp--goal-method s) "_session/goal")))))

;;; aob-acp-conformance-tests.el ends here
