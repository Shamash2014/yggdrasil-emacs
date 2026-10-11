;;; aob-awake-tests.el --- ERT tests for aob-awake -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-awake)

(defvar aob-awake-tests--calls nil)

(defmacro aob-awake-tests--with (block-form &rest body)
  (declare (indent 1))
  `(let ((aob--sessions (make-hash-table :test #'equal))
         (aob--order nil)
         (aob-state-change-hook nil)
         (aob-session-removed-hook nil)
         (aob-awake--token nil)
         (aob-awake--warned nil)
         (aob-keep-awake-mode nil)
         (aob-awake-tests--calls nil))
     (cl-letf (((symbol-function 'system-sleep-block-sleep)
                (lambda (&rest args)
                  (push (cons 'block args) aob-awake-tests--calls)
                  ,block-form))
               ((symbol-function 'system-sleep-unblock-sleep)
                (lambda (token) (push (list 'unblock token) aob-awake-tests--calls)))
               ((symbol-function 'aob--dirty) #'ignore))
       (provide 'system-sleep)
       (unwind-protect
           (progn (aob-keep-awake-mode 1) ,@body)
         (aob-keep-awake-mode -1)))))

(defun aob-awake-tests--session (id)
  (aob-create-session :id id :backend 'acp :name id :project "/tmp/"
                      :dir "/tmp/" :state 'idle))

(defun aob-awake-tests--count (kind)
  (seq-count (lambda (c) (eq (car c) kind)) aob-awake-tests--calls))

(ert-deftest aob-awake-first-working-blocks-once-without-display ()
  (aob-awake-tests--with 'tok
    (let ((a (aob-awake-tests--session "a")))
      (aob-set-state a 'working)
      (should (= 1 (aob-awake-tests--count 'block)))
      (should (equal '(block "aob agent working" t)
                     (car (last aob-awake-tests--calls)))))))

(ert-deftest aob-awake-second-working-does-not-block-again ()
  (aob-awake-tests--with 'tok
    (let ((a (aob-awake-tests--session "a"))
          (b (aob-awake-tests--session "b")))
      (aob-set-state a 'working)
      (aob-set-state b 'working)
      (should (= 1 (aob-awake-tests--count 'block)))
      (aob-set-state a 'done)
      (should (= 0 (aob-awake-tests--count 'unblock))))))

(ert-deftest aob-awake-last-leaving-working-releases ()
  (aob-awake-tests--with 'tok
    (let ((a (aob-awake-tests--session "a"))
          (b (aob-awake-tests--session "b")))
      (aob-set-state a 'working)
      (aob-set-state b 'working)
      (aob-set-state a 'done)
      (aob-set-state b 'done)
      (should (equal '(unblock tok) (car aob-awake-tests--calls)))
      (should (= 1 (aob-awake-tests--count 'unblock))))))

(ert-deftest aob-awake-blocked-releases ()
  (aob-awake-tests--with 'tok
    (let ((a (aob-awake-tests--session "a")))
      (aob-set-state a 'working)
      (aob-set-state a 'blocked)
      (should (= 1 (aob-awake-tests--count 'unblock)))
      (aob-set-state a 'working)
      (should (= 2 (aob-awake-tests--count 'block))))))

(ert-deftest aob-awake-removing-a-working-session-releases ()
  (aob-awake-tests--with 'tok
    (let ((a (aob-awake-tests--session "a")))
      (aob-set-state a 'working)
      (cl-letf (((symbol-function 'aob--retire)
                 (lambda (s &optional _)
                   (run-hook-with-args 'aob-session-removed-hook s))))
        (aob-remove-session a))
      (should (= 1 (aob-awake-tests--count 'unblock))))))

(ert-deftest aob-awake-mode-off-releases-and-stops-blocking ()
  (aob-awake-tests--with 'tok
    (let ((a (aob-awake-tests--session "a")))
      (aob-set-state a 'working)
      (aob-keep-awake-mode -1)
      (should (= 1 (aob-awake-tests--count 'unblock)))
      (aob-set-state a 'blocked)
      (aob-set-state a 'working)
      (should (= 1 (aob-awake-tests--count 'block))))))

(ert-deftest aob-awake-mode-on-while-working-takes-a-block ()
  (aob-awake-tests--with 'tok
    (let ((a (aob-awake-tests--session "a")))
      (aob-keep-awake-mode -1)
      (aob-set-state a 'working)
      (should (= 0 (aob-awake-tests--count 'block)))
      (aob-keep-awake-mode 1)
      (should (= 1 (aob-awake-tests--count 'block))))))

(ert-deftest aob-awake-failed-block-messages-at-most-once ()
  (dolist (form '(nil (error "nope")))
    (aob-awake-tests--with (eval form t)
      (let ((a (aob-awake-tests--session "a"))
            (msgs 0))
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest _) (cl-incf msgs))))
          (aob-set-state a 'working)
          (aob-set-state a 'blocked)
          (aob-set-state a 'working)
          (aob-set-state a 'done))
        (should (= 1 msgs))
        (should (= 0 (aob-awake-tests--count 'unblock)))))))

(provide 'aob-awake-tests)
;;; aob-awake-tests.el ends here
