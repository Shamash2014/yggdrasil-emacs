;;; aob-resolve-ret-tests.el --- RET on a permission option -*- lexical-binding: t; -*-

(require 'ert)
(require 'ert-x)
(require 'aob-tests)
(require 'vertico)
(require 'prescient nil t)
(require 'vertico-prescient nil t)
(require 'orderless nil t)
(require 'layer-completion)

(defmacro aob-resolve-ret--with-pickers (&rest body)
  `(let ((completion-styles '(orderless basic))
         (vertico-count 8))
     (vertico-mode 1)
     (when (fboundp 'vertico-prescient-mode)
       (setq vertico-prescient-enable-filtering nil)
       (vertico-prescient-mode 1))
     (unwind-protect (progn ,@body)
       (vertico-mode -1)
       (when (fboundp 'vertico-prescient-mode) (vertico-prescient-mode -1)))))

(ert-deftest aob-resolve-ret-on-an-option-answers-the-permission ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 71 "session/request_permission"
                        (list :toolCall (list :title "Bash")
                              :options [(:optionId "always" :name "Always Allow" :kind "allow_always")
                                        (:optionId "allow" :name "Allow" :kind "allow_once")
                                        (:optionId "reject" :name "Reject" :kind "reject_once")]))
    (aob-resolve-ret--with-pickers
     (aob-tests--capturing sent
       (ert-simulate-keys (kbd "RET") (aob-resolve s))
       (should (equal (aob-tests--replies sent)
                      (list (concat "{\"jsonrpc\":\"2.0\",\"id\":71,\"result\":{\"outcome\":"
                                    "{\"outcome\":\"selected\",\"optionId\":\"allow\"}}}"))))))))

(defconst aob-resolve-ret--permission
  (list :toolCall (list :title "Bash")
        :options [(:optionId "allow" :name "Allow" :kind "allow_once")
                  (:optionId "reject" :name "Reject" :kind "reject_once")]))

(defun aob-resolve-ret--dead-session-holding-a-permission (&optional before-death)
  (let* ((proc (make-process :name "aob-test-dead" :command '("cat")
                             :connection-type 'pipe :noquery t))
         (s (aob-create-session :id "acp:dead:1" :backend 'acp :name "dead:1"
                                :project "/tmp/old/" :dir "/tmp/old/" :state 'working)))
    (process-put proc 'aob-sessions (make-hash-table :test #'equal))
    (process-put proc 'aob-next-id (list 0))
    (process-put proc 'aob-pending (make-hash-table :test #'eql))
    (process-put proc 'aob-json-buf (generate-new-buffer " *aob-test-dead-json*"))
    (setf (aob-session-conn s) proc)
    (aob-acp--register proc "sess-dead" s)
    (aob-tests--request s 5 "session/request_permission" aob-resolve-ret--permission)
    (when before-death (funcall before-death s))
    (delete-process proc)
    (aob-acp--sentinel proc "finished\n")
    s))

(ert-deftest aob-resolve-ret-answers-the-live-permission-past-a-dead-session ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 71 "session/request_permission" aob-resolve-ret--permission)
    (let ((dead (aob-resolve-ret--dead-session-holding-a-permission)))
      (unwind-protect
          (aob-tests--capturing sent
            (aob-resolve-ret--with-pickers
             (let ((target (aob-session-awaiting-answer)))
               (should (eq target s))
               (ert-simulate-keys (kbd "RET") (aob-resolve target))))
            (should (equal (aob-tests--replies sent)
                           (list (concat "{\"jsonrpc\":\"2.0\",\"id\":71,\"result\":{\"outcome\":"
                                         "{\"outcome\":\"selected\",\"optionId\":\"allow\"}}}")))))
        (aob-remove-session dead)))))

(ert-deftest aob-resolve-ret-a-session-that-died-holds-no-decision ()
  (let ((dead (aob-resolve-ret--dead-session-holding-a-permission)))
    (unwind-protect
        (should-not (aob-session-decisions dead))
      (aob-remove-session dead))))

(ert-deftest aob-resolve-ret-on-a-bare-prompt-grants-once-never-sends-nothing ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 72 "session/request_permission" aob-resolve-ret--permission)
    (aob-tests--capturing sent
      (ert-simulate-keys (kbd "RET") (aob-resolve s))
      (should (equal (aob-tests--replies sent)
                     (list (concat "{\"jsonrpc\":\"2.0\",\"id\":72,\"result\":{\"outcome\":"
                                   "{\"outcome\":\"selected\",\"optionId\":\"allow\"}}}")))))))

(ert-deftest aob-resolve-ret-on-an-elicitation-option-sends-its-label ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 81 "elicitation/create" aob-tests--ask-one)
    (aob-resolve-ret--with-pickers
     (aob-tests--capturing sent
       (ert-simulate-keys (kbd "RET") (aob-resolve s))
       (should (equal (aob-tests--replies sent)
                      (list (concat "{\"jsonrpc\":\"2.0\",\"id\":81,\"result\":{\"action\":\"accept\","
                                    "\"content\":{\"question_0\":\"Redis\"}}}"))))))))

(ert-deftest aob-resolve-ret-dead-session-drops-the-decision-and-says-so ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 91 "session/request_permission" aob-resolve-ret--permission)
    (let ((d (car (aob-session-decisions s))))
      (should (aob-acp--decision-event s d))
      (delete-process (aob-session-conn s))
      (should-error (aob-acp--resolve s d "allow") :type 'user-error)
      (should-not (aob-session-decisions s))
      (should-not (plist-get (aob-acp--decision-event s d) :line)))))

(ert-deftest aob-resolve-ret-send-that-signals-drops-the-decision ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 92 "session/request_permission" aob-resolve-ret--permission)
    (let ((d (car (aob-session-decisions s))))
      (cl-letf (((symbol-function 'aob-acp--send-proc)
                 (lambda (&rest _) (error "Process not running"))))
        (should-error (aob-acp--resolve s d "allow") :type 'user-error))
      (should-not (aob-session-decisions s)))))

(ert-deftest aob-resolve-ret-sentinel-clears-the-awaiting-line-of-a-dead-trace ()
  (let* ((ev nil)
         (dead (aob-resolve-ret--dead-session-holding-a-permission
                (lambda (s)
                  (setq ev (aob-acp--decision-event s (car (aob-session-decisions s))))
                  (plist-put ev :line "awaiting")))))
    (unwind-protect
        (progn (should ev)
               (should-not (plist-get ev :line)))
      (aob-remove-session dead))))

(ert-deftest aob-resolve-ret-a-state-dead-session-is-not-awaiting-an-answer ()
  (aob-tests--with-session dead
    (aob-tests--request dead 93 "session/request_permission" aob-resolve-ret--permission)
    (setf (aob-session-state dead) 'dead)
    (should (aob-session-decisions dead))
    (should-not (aob-session-awaiting-answer))))

(ert-deftest aob-resolve-ret-a-failed-session-with-a-live-process-keeps-its-prompt ()
  (aob-tests--with-session s
    (aob-tests--request s 94 "session/request_permission" aob-resolve-ret--permission)
    (setf (aob-session-state s) 'failed)
    (should (process-live-p (aob-session-conn s)))
    (should (eq (aob-session-awaiting-answer) s))))

(defun aob-resolve-ret--point-to-prompt ()
  (interactive)
  (goto-char (point-min)))

(ert-deftest aob-resolve-ret-with-point-in-the-prompt-still-answers ()
  (aob-tests--with-session s
    (aob-set-state s 'working)
    (aob-tests--request s 73 "session/request_permission"
                        (list :toolCall (list :title (make-string 277 ?x))
                              :options [(:optionId "allow" :name "Allow" :kind "allow_once")
                                        (:optionId "reject" :name "Reject" :kind "reject_once")]))
    (aob-resolve-ret--with-pickers
     (aob-tests--capturing sent
       (let ((minibuffer-local-map (copy-keymap minibuffer-local-map)))
         (define-key vertico-map (kbd "C-a") #'aob-resolve-ret--point-to-prompt)
         (with-timeout (10 (ignore-errors (abort-minibuffers)))
           (ert-simulate-keys (vconcat (kbd "C-a") (kbd "RET")) (aob-resolve s))))
       (should (equal (aob-tests--replies sent)
                      (list (concat "{\"jsonrpc\":\"2.0\",\"id\":73,\"result\":{\"outcome\":"
                                    "{\"outcome\":\"selected\",\"optionId\":\"allow\"}}}"))))))))
