;;; aob-mcp-session-ask-tests.el --- session_ask returns answers; targets can be limited -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-mcp)
(require 'aob-mcp-tools)
(require 'aob-mcp-host)

(defvar aob-mcp-session-ask-tests--told nil
  "What was prompted to which session, newest first, as (NAME . TEXT).")

(defmacro aob-mcp-session-ask-tests--with (vars &rest body)
  "Bind (ASKER TARGET) in VARS to idle sessions around BODY, prompting recorded."
  (declare (indent 1))
  `(let ((aob--sessions (make-hash-table :test #'equal))
         (aob--order nil)
         (aob-mcp-host--asks nil)
         (aob-mcp-host-session-targets nil)
         (aob-mcp-session-ask-tests--told nil))
     (let ((,(nth 0 vars) (aob-create-session :id "acp:asker" :backend 'acp :name "asker"
                                              :project "/tmp/a/" :dir "/tmp/a/" :state 'idle))
           (,(nth 1 vars) (aob-create-session :id "acp:target" :backend 'acp :name "target"
                                              :project "/tmp/t/" :dir "/tmp/t/" :state 'idle)))
       (cl-letf (((symbol-function 'aob-prompt)
                  (lambda (s text &optional _atts)
                    (push (cons (aob-session-name s) text) aob-mcp-session-ask-tests--told)
                    (aob-event s 'prompt :text text)
                    (aob-set-state s 'working)))
                 ((symbol-function 'aob-acp--steers-p) (lambda (_s) nil))
                 ((symbol-function 'aob-mcp-host-session) (lambda (_) ,(nth 0 vars)))
                 ((symbol-function 'aob-mcp-relay)
                  (lambda (_conn _id form) (eval form t))))
         (ignore ,@vars)
         ,@body))))

(defun aob-mcp-session-ask-tests--call (tool args)
  (funcall (plist-get (gethash tool aob-mcp--tools) :handler) args nil 1))

(defun aob-mcp-session-ask-tests--finish-turn (s text)
  (aob-event s 'message :text text)
  (aob-set-state s 'idle))

(ert-deftest aob-mcp-session-ask-answer-returns-to-the-asker-on-turn-end ()
  (aob-mcp-session-ask-tests--with (asker target)
    (let ((ack (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "what is 2+2?"))))
      (should (string-match-p "arrive as a message" (car ack)))
      (should (string-match-p "what is 2\\+2\\?" (cdr (assoc "target" aob-mcp-session-ask-tests--told))))
      (should-not (assoc "asker" aob-mcp-session-ask-tests--told))
      (aob-mcp-session-ask-tests--finish-turn target "four")
      (aob-set-state asker 'idle)
      (should (string-match-p "\\`Reply from target:\n\nfour\\'"
                              (cdr (assoc "asker" aob-mcp-session-ask-tests--told))))
      (should-not aob-mcp-host--asks))))

(ert-deftest aob-mcp-session-ask-waits-for-the-turn-that-took-the-question ()
  (aob-mcp-session-ask-tests--with (asker target)
    (aob-set-state target 'working)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "q"))
    (let ((ev (car (aob-session-events target))))
      (plist-put ev :status "queued")
      (aob-event target 'message :text "answer to something else")
      (aob-set-state target 'idle)
      (should-not (assoc "asker" aob-mcp-session-ask-tests--told))
      (plist-put ev :status nil)
      (aob-set-state target 'working)
      (aob-mcp-session-ask-tests--finish-turn target "the real answer")
      (should (string-match-p "the real answer" (cdr (assoc "asker" aob-mcp-session-ask-tests--told)))))))

(ert-deftest aob-mcp-session-ask-says-so-when-the-target-ends-first ()
  (aob-mcp-session-ask-tests--with (asker target)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "q"))
    (aob-set-state target 'dead)
    (should (string-match-p "\\`No reply from target" (cdr (assoc "asker" aob-mcp-session-ask-tests--told))))
    (should-not aob-mcp-host--asks)))

(ert-deftest aob-mcp-session-ask-drops-quietly-when-the-asker-is-gone ()
  (aob-mcp-session-ask-tests--with (asker target)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "q"))
    (remhash (aob-session-id asker) aob--sessions)
    (aob-mcp-session-ask-tests--finish-turn target "late")
    (should-not (assoc "asker" aob-mcp-session-ask-tests--told))
    (should-not aob-mcp-host--asks)))

(ert-deftest aob-mcp-session-ask-keeps-one-question-per-pair ()
  (aob-mcp-session-ask-tests--with (asker target)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "one"))
    (let ((again (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "two"))))
      (should (string-match-p "has not answered" (car again)))
      (should (= (length aob-mcp-session-ask-tests--told) 1)))))

(ert-deftest aob-mcp-session-ask-rollback-after-heir-creation-keeps-the-ask-and-delivers-olds-answer ()
  "rollback after heir creation keeps the ask and delivers old's answer"
  (aob-mcp-session-ask-tests--with (asker target)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "q"))
    (ignore-errors
      (aob-succeed target
                   (lambda ()
                     (aob-create-session :id (aob-session-id target) :backend 'acp :name "target"
                                         :project "/tmp/t/" :dir "/tmp/t/" :state 'idle)
                     (error "wake failed"))))
    (should (eq (aob-session-get "acp:target") target))
    (should (= (length aob-mcp-host--asks) 1))
    (should-not (assoc "asker" aob-mcp-session-ask-tests--told))
    (aob-mcp-session-ask-tests--finish-turn target "four")
    (should (string-match-p "\\`Reply from target:\n\nfour\\'"
                            (cdr (assoc "asker" aob-mcp-session-ask-tests--told))))
    (should-not aob-mcp-host--asks)))

(ert-deftest aob-mcp-session-ask-survives-a-restart-of-the-target ()
  (aob-mcp-session-ask-tests--with (asker target)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "q"))
    (aob-session-put target :restarting t)
    (aob-set-state target 'dead)
    (should-not (assoc "asker" aob-mcp-session-ask-tests--told))
    (should (= (length aob-mcp-host--asks) 1))))

(ert-deftest aob-mcp-session-ask-ignores-the-answer-to-an-earlier-copy-of-the-question ()
  (aob-mcp-session-ask-tests--with (asker target)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "q"))
    (aob-mcp-session-ask-tests--finish-turn target "first answer")
    (setq aob-mcp-session-ask-tests--told nil)
    (aob-set-state target 'working)
    (aob-mcp-session-ask-tests--call "session_ask" '(:id "target" :text "q"))
    (plist-put (car (aob-session-events target)) :status "queued")
    (aob-mcp-session-ask-tests--finish-turn target "answer to something else")
    (should-not (assoc "asker" aob-mcp-session-ask-tests--told))
    (should (= (length aob-mcp-host--asks) 1))))

(ert-deftest aob-mcp-session-targets-block-a-name-no-pattern-matches ()
  (aob-mcp-session-ask-tests--with (asker target)
    (setq aob-mcp-host-session-targets '("\\`worker-"))
    (dolist (call '(("session_say" (:id "target" :text "hi"))
                    ("session_read" (:id "target"))
                    ("session_ask" (:id "target" :text "hi"))))
      (should (string-match-p "not a session you are allowed"
                              (car (aob-mcp-session-ask-tests--call (car call) (cadr call))))))
    (should-not aob-mcp-session-ask-tests--told)))

(ert-deftest aob-mcp-session-targets-allow-a-matching-name ()
  (aob-mcp-session-ask-tests--with (asker target)
    (setq aob-mcp-host-session-targets '("\\`worker-" "\\`tar"))
    (should (string-match-p "said to target"
                            (car (aob-mcp-session-ask-tests--call "session_say" '(:id "target" :text "hi")))))
    (should (assoc "target" aob-mcp-session-ask-tests--told))))

(ert-deftest aob-mcp-session-targets-nil-allows-every-session ()
  (aob-mcp-session-ask-tests--with (asker target)
    (should-not aob-mcp-host-session-targets)
    (should (string-match-p "said to target"
                            (car (aob-mcp-session-ask-tests--call "session_say" '(:id "target" :text "hi")))))))

(provide 'aob-mcp-session-ask-tests)

;;; aob-mcp-session-ask-tests.el ends here
