;;; aob-reject-reason-tests.el --- A refusal that asks why -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-reject-reason-sessions-" nil ".eld"))
(require 'aob-trace)

(defmacro aob-reject-reason-tests--with-session (var &rest body)
  "Bind VAR to a wired-up fake ACP session over a cat connection."
  (declare (indent 1))
  `(let* ((proc (make-process :name "aob-test-cat" :command '("cat")
                              :connection-type 'pipe :noquery t))
          (,var (aob-create-session :id "acp:test:1" :backend 'acp
                                    :name "test:1" :project "/tmp/"
                                    :dir "/tmp/" :state 'starting)))
     (unwind-protect
         (progn
           (process-put proc 'aob-sessions (make-hash-table :test #'equal))
           (process-put proc 'aob-next-id (list 0))
           (process-put proc 'aob-pending (make-hash-table :test #'eql))
           (process-put proc 'aob-json-buf (generate-new-buffer " *aob-test-json*"))
           (setf (aob-session-conn ,var) proc)
           (aob-acp--register proc "sess-test" ,var)
           ,@body)
       (ignore-errors (kill-buffer "compose:test:1"))
       (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
       (ignore-errors (delete-process proc))
       (when (aob-session-get (aob-session-id ,var))
         (aob-remove-session ,var)))))

(defmacro aob-reject-reason-tests--capturing (var &rest body)
  "Run BODY with outgoing frames collected newest-first into VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'aob-acp--send-proc)
                (lambda (_proc msg) (push msg ,var))))
       ,@body)))

(defmacro aob-reject-reason-tests--picking (choice &rest body)
  "Run BODY with every completing-read answering CHOICE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ,choice)))
     ,@body))

(defmacro aob-reject-reason-tests--no-compose (&rest body)
  "Run BODY failing the test if it opens a compose box."
  `(cl-letf (((symbol-function 'aob-compose)
              (lambda (&rest _) (ert-fail "compose was opened"))))
     ,@body))

(defun aob-reject-reason-tests--ask (s)
  "Feed S a permission request offering allow and reject once."
  (aob-acp--filter
   (aob-session-conn s)
   (concat "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"session/request_permission\",\"params\":{\"sessionId\":\"sess-test\",\"toolCall\":{\"title\":\"rm -rf build\"},\"options\":[{\"optionId\":\"a\",\"name\":\"Allow\",\"kind\":\"allow_once\"},{\"optionId\":\"r\",\"name\":\"Reject\",\"kind\":\"reject_once\"}]}}"
           "\n")))

(defun aob-reject-reason-tests--replies (sent)
  "The replies in SENT, oldest first, as the JSON that went out."
  (mapcar #'json-serialize
          (reverse (seq-filter (lambda (m) (plist-member m :result)) sent))))

(defconst aob-reject-reason-tests--reject-reply
  "{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"r\"}}}")

(defun aob-reject-reason-tests--reject (s)
  "Refuse S's pending permission through aob-resolve; the compose box it opened."
  (aob-reject-reason-tests--ask s)
  (aob-reject-reason-tests--capturing sent
    (aob-reject-reason-tests--picking "Reject"
      (aob-resolve s))
    (should (equal (aob-reject-reason-tests--replies sent)
                   (list aob-reject-reason-tests--reject-reply))))
  (get-buffer "compose:test:1"))

(ert-deftest aob-reject-reason-opens-compose-for-the-reason ()
  (aob-reject-reason-tests--with-session s
    (aob-set-state s 'working)
    (let ((buf (aob-reject-reason-tests--reject s)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (derived-mode-p 'aob-compose-mode))
        (should (eq aob-compose--purpose 'reject-reason))
        (should (equal aob-compose--target (aob-session-id s)))
        (should (member "reason for the rejection" aob-compose--tags))
        (should (equal aob-compose-placeholder aob-reject-reason-placeholder))
        (should (equal (overlay-get aob-compose--placeholder 'before-string)
                       aob-reject-reason-placeholder))
        (should-not aob-compose--steer)))
    (should-not (aob-session-decisions s))))

(ert-deftest aob-reject-reason-sent-queues-when-the-agent-cannot-steer ()
  (aob-reject-reason-tests--with-session s
    (aob-set-state s 'working)
    (let ((buf (aob-reject-reason-tests--reject s)))
      (with-current-buffer buf
        (insert "the build folder is shared; clean only dist")
        (cl-letf (((symbol-function 'aob-acp--steers-p) (lambda (_) nil)))
          (aob-reject-reason-tests--capturing sent
            (aob-compose-send)
            (should-not (aob-reject-reason-tests--replies sent)))))
      (should-not (buffer-live-p buf)))
    (should (equal (car (car (aob-session-ref s :queued)))
                   "the build folder is shared; clean only dist"))))

(ert-deftest aob-reject-reason-sent-steers-a-turn-that-takes-it ()
  (aob-reject-reason-tests--with-session s
    (aob-set-state s 'working)
    (let ((buf (aob-reject-reason-tests--reject s))
          said)
      (with-current-buffer buf
        (insert "use a temp dir instead")
        (cl-letf (((symbol-function 'aob-acp--steers-p) (lambda (_) t))
                  ((symbol-function 'aob-interject)
                   (lambda (to text &rest _) (push (cons (aob-session-id to) text) said))))
          (aob-compose-send)))
      (should (equal said (list (cons (aob-session-id s) "use a temp dir instead")))))
    (should-not (aob-session-ref s :queued))))

(ert-deftest aob-reject-reason-closed-empty-says-nothing ()
  (aob-reject-reason-tests--with-session s
    (aob-set-state s 'working)
    (let ((buf (aob-reject-reason-tests--reject s)))
      (aob-reject-reason-tests--capturing sent
        (with-current-buffer buf (aob-compose-abort))
        (should-not sent)))
    (should-not (aob-session-ref s :queued))))

(ert-deftest aob-reject-reason-allow-opens-nothing ()
  (aob-reject-reason-tests--with-session s
    (aob-set-state s 'working)
    (aob-reject-reason-tests--ask s)
    (aob-reject-reason-tests--no-compose
      (aob-reject-reason-tests--capturing sent
        (aob-reject-reason-tests--picking "Allow"
          (aob-resolve s))
        (should (equal (aob-reject-reason-tests--replies sent)
                       '("{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"a\"}}}")))))))

(ert-deftest aob-reject-reason-off-when-the-option-is-nil ()
  (aob-reject-reason-tests--with-session s
    (aob-set-state s 'working)
    (aob-reject-reason-tests--ask s)
    (let ((aob-reject-asks-reason nil))
      (aob-reject-reason-tests--no-compose
        (aob-reject-reason-tests--capturing sent
          (aob-reject-reason-tests--picking "Reject"
            (aob-resolve s))
          (should (equal (aob-reject-reason-tests--replies sent)
                         (list aob-reject-reason-tests--reject-reply))))))))

(defun aob-reject-reason-tests--plan (s id)
  "Feed S a plan to approve, refusable once."
  (aob-acp--filter
   (aob-session-conn s)
   (concat
    (json-serialize
     (list :jsonrpc "2.0" :id id :method "session/request_permission"
           :params (list :sessionId "sess-test"
                         :toolCall (list :toolCallId "toolu_plan" :name "ExitPlanMode"
                                         :status "pending" :rawInput (list :plan "1. Add the flag")
                                         :title "Approve Plan" :kind "switch_mode")
                         :options [(:optionId "ok" :name "Yes" :kind "allow_once")
                                   (:optionId "no" :name "No, keep planning" :kind "reject_once")])))
    "\n")))

(ert-deftest aob-reject-reason-trace-plan-refusals-ask-why ()
  (aob-reject-reason-tests--with-session s
    (let ((aob-trace-icons nil)
          (trace (aob-trace-buffer s)))
      (unwind-protect
          (with-current-buffer trace
            (aob-set-state s 'working)
            (aob-reject-reason-tests--plan s 8)
            (aob-reject-reason-tests--capturing sent
              (aob-trace-decline)
              (should (equal (aob-reject-reason-tests--replies sent)
                             '("{\"jsonrpc\":\"2.0\",\"id\":8,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"no\"}}}"))))
            (should (eq (buffer-local-value 'aob-compose--purpose (get-buffer "compose:test:1"))
                        'reject-reason))
            (kill-buffer "compose:test:1")
            (set-buffer trace)
            (aob-reject-reason-tests--plan s 9)
            (aob-trace--render t)
            (goto-char (point-min))
            (search-forward "▸ No, keep planning")
            (beginning-of-line)
            (aob-reject-reason-tests--capturing sent
              (aob-trace-answer)
              (should (equal (aob-reject-reason-tests--replies sent)
                             '("{\"jsonrpc\":\"2.0\",\"id\":9,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"no\"}}}"))))
            (should (eq (buffer-local-value 'aob-compose--purpose (get-buffer "compose:test:1"))
                        'reject-reason))
            (kill-buffer "compose:test:1")
            (set-buffer trace)
            (aob-reject-reason-tests--plan s 10)
            (aob-trace--render t)
            (goto-char (point-min))
            (search-forward "▸ Yes")
            (beginning-of-line)
            (aob-reject-reason-tests--no-compose
              (aob-reject-reason-tests--capturing sent
                (aob-trace-answer)
                (should (equal (length (aob-reject-reason-tests--replies sent)) 1)))))
        (kill-buffer trace)))))

;;; aob-reject-reason-tests.el ends here
