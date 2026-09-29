;;; aob-native-cancel-tests.el --- stopping a subagent stops its agent -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-subagent)

(ert-deftest aob-native-cancel-stops-the-agent-that-runs-it ()
  (let (stopped)
    (aob-register-backend 'native-cancel-test
                          (list :cancel (lambda (s &rest args) (push (cons (aob-session-id s) args) stopped))))
    (let* ((root (aob-create-session :id "native-cancel:root" :backend 'native-cancel-test
                                     :name "lead" :state 'working))
           (kid (aob-create-session :id "native-cancel:kid" :backend 'native-subagent
                                    :name "kid" :state 'working)))
      (unwind-protect
          (progn
            (aob-session-put kid :native-root (aob-session-id root))
            (aob--call kid :cancel t)
            (should (equal stopped '(("native-cancel:root" t)))))
        (aob-remove-session kid)
        (aob-remove-session root)))))

(ert-deftest aob-native-cancel-without-an-agent-says-so ()
  (let ((kid (aob-create-session :id "native-cancel:orphan" :backend 'native-subagent
                                 :name "orphan" :state 'working)))
    (unwind-protect
        (should-error (aob--call kid :cancel) :type 'user-error)
      (aob-remove-session kid))))

;;; aob-native-cancel-tests.el ends here
