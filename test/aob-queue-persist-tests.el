;;; aob-queue-persist-tests.el --- queued prompts in the persist file -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'aob-transcript)

(defmacro aob-queue-persist-tests--with (&rest body)
  (declare (indent 0))
  `(let ((aob--sessions (make-hash-table :test #'equal))
         (aob--order nil)
         (aob-state-change-hook nil)
         (aob-acp--opened-any t)
         (aob-acp--persisted-cache nil)
         (aob-acp--queued-writes nil)
         (aob-acp--queued-timer nil)
         (aob-acp-persist-file (make-temp-file "aob-queue-persist-" nil ".eld")))
     (unwind-protect
         (cl-letf (((symbol-function 'aob--dirty) #'ignore))
           ,@body)
       (when (timerp aob-acp--queued-timer)
         (cancel-timer aob-acp--queued-timer))
       (delete-file aob-acp-persist-file))))

(defun aob-queue-persist-tests--session (id)
  (let ((s (aob-create-session :id id :backend 'acp :name id
                               :project temporary-file-directory
                               :dir temporary-file-directory :state 'idle)))
    (aob-session-put s :acp-id (concat "acp-" id))
    (aob-session-put s :agent "claude")
    s))

(defun aob-queue-persist-tests--entry (id)
  (seq-find (lambda (e) (equal (plist-get e :acp-id) id))
            (with-temp-buffer
              (insert-file-contents aob-acp-persist-file)
              (read (current-buffer)))))

(defmacro aob-queue-persist-tests--counting (var &rest body)
  (declare (indent 1))
  `(cl-letf* ((orig (symbol-function 'aob-acp--rewrite))
              ((symbol-function 'aob-acp--rewrite)
               (lambda (&rest args) (cl-incf ,var) (apply orig args))))
     ,@body))

(ert-deftest aob-queue-persist-resume-shows-each-queued-prompt-once ()
  (aob-queue-persist-tests--with
    (let ((aob-acp-persist-file nil)
          (old (aob-queue-persist-tests--session "a"))
          s)
      (aob-acp--queue old "later" nil)
      (aob-session-put old :asleep (aob-acp--entry old))
      (cl-letf (((symbol-function 'aob-acp--connect) (lambda (&rest _) nil)))
        (setq s (aob-acp-resume-entry (aob-acp--entry old))))
      (let ((prompts (seq-filter (lambda (ev) (equal (plist-get ev :text) "later"))
                                 (aob-session-events s))))
        (should (= 1 (length prompts)))
        (should (equal "queued" (plist-get (car prompts) :status)))
        (should (eq (car prompts) (nth 2 (car (aob-session-ref s :queued)))))))))

(ert-deftest aob-queue-persist-writes-the-pause ()
  (aob-queue-persist-tests--with
    (let ((s (aob-queue-persist-tests--session "a")))
      (puthash "a" s aob--sessions)
      (push "a" aob--order)
      (aob-acp--queue s "later" nil)
      (aob-acp--flush-queued-writes)
      (aob-session-put s :queue-paused t)
      (aob-acp--persist-queued s)
      (aob-acp--flush-queued-writes)
      (should (plist-get (aob-queue-persist-tests--entry "acp-a") :queue-paused)))))

(ert-deftest aob-queue-persist-missing-entry-falls-back-to-a-full-write ()
  (aob-queue-persist-tests--with
    (let ((s (aob-queue-persist-tests--session "a")))
      (puthash "a" s aob--sessions)
      (push "a" aob--order)
      (write-region "nil" nil aob-acp-persist-file)
      (aob-acp--queue s "later" nil)
      (aob-acp--flush-queued-writes)
      (should (equal '(("later" nil))
                     (plist-get (aob-queue-persist-tests--entry "acp-a") :queued))))))

(ert-deftest aob-queue-persist-asleep-session-keeps-pin-queue-and-pause ()
  (aob-queue-persist-tests--with
    (cl-letf (((symbol-function 'aob-transcript-turns) (lambda (&rest _) nil)))
      (let* ((s (aob-transcript--session
                 (list :agent "claude" :acp-id "gone" :name "n"
                       :project temporary-file-directory
                       :dir temporary-file-directory
                       :pinned t :queue-paused t :queued '(("kept" nil)))
                 nil))
             (e (aob-acp--entry s)))
        (should (plist-get e :pinned))
        (should (plist-get e :queue-paused))
        (should (equal '(("kept" nil)) (plist-get e :queued)))))))

(ert-deftest aob-queue-persist-restore-writes-once ()
  (aob-queue-persist-tests--with
    (let ((s (aob-queue-persist-tests--session "a"))
          (writes 0))
      (puthash "a" s aob--sessions)
      (push "a" aob--order)
      (aob-acp--persist)
      (aob-queue-persist-tests--counting writes
        (aob-acp--restore-queued s '(("one" nil) ("two" nil) ("three" nil)))
        (aob-acp--flush-queued-writes))
      (should (= 1 writes))
      (should (= 3 (length (plist-get (aob-queue-persist-tests--entry "acp-a") :queued)))))))

(ert-deftest aob-queue-persist-changes-are-debounced-and-flushed-on-exit ()
  (aob-queue-persist-tests--with
    (let ((s (aob-queue-persist-tests--session "a"))
          (writes 0))
      (puthash "a" s aob--sessions)
      (push "a" aob--order)
      (aob-acp--persist)
      (aob-queue-persist-tests--counting writes
        (dotimes (_ 5) (aob-acp--queue s "x" nil))
        (should (= 0 writes))
        (should (memq 'aob-acp--flush-queued-writes kill-emacs-hook))
        (aob-acp--flush-queued-writes))
      (should (= 1 writes))
      (should (= 5 (length (plist-get (aob-queue-persist-tests--entry "acp-a") :queued)))))))

(provide 'aob-queue-persist-tests)

(defun aob-queue-persist-tests--register (s)
  (puthash (aob-session-id s) s aob--sessions)
  (push (aob-session-id s) aob--order))

(ert-deftest aob-queue-persist-removed-session-leaves-no-pending-write ()
  (aob-queue-persist-tests--with
    (let ((s (aob-queue-persist-tests--session "a")))
      (aob-queue-persist-tests--register s)
      (aob-acp--persist)
      (aob-acp--queue s "p1" nil)
      (aob-acp--queue s "p2" nil)
      (aob-remove-session s)
      (should-not (memq s aob-acp--queued-writes))
      (aob-acp--flush-queued-writes)
      (should (= 2 (length (plist-get (aob-queue-persist-tests--entry "acp-a")
                                      :queued)))))))

(ert-deftest aob-queue-persist-flush-skips-unregistered-session ()
  (aob-queue-persist-tests--with
    (let ((s (aob-queue-persist-tests--session "a")))
      (aob-queue-persist-tests--register s)
      (aob-acp--persist)
      (aob-acp--queue s "p1" nil)
      (aob-acp--flush-queued-writes)
      (aob-acp--queue s "p2" nil)
      (remhash (aob-session-id s) aob--sessions)
      (aob-acp--flush-queued-writes)
      (should (= 1 (length (plist-get (aob-queue-persist-tests--entry "acp-a")
                                      :queued)))))))

(ert-deftest aob-queue-persist-asleep-session-keeps-its-queue-on-direct-write ()
  (aob-queue-persist-tests--with
    (cl-letf (((symbol-function 'aob-transcript-turns) (lambda (&rest _) nil)))
      (let ((s (aob-transcript--session
                (list :agent "claude" :acp-id "gone" :name "n"
                      :project temporary-file-directory
                      :dir temporary-file-directory
                      :queue-paused t :queued '(("kept" nil)))
                nil)))
        (aob-queue-persist-tests--register s)
        (aob-acp--persist)
        (aob-acp--persist-queued s)
        (aob-acp--flush-queued-writes)
        (let ((e (aob-queue-persist-tests--entry "gone")))
          (should (equal '(("kept" nil)) (plist-get e :queued)))
          (should (plist-get e :queue-paused)))))))

(ert-deftest aob-queue-persist-pause-on-empty-queue-is-not-written ()
  (aob-queue-persist-tests--with
    (let ((s (aob-queue-persist-tests--session "a")))
      (aob-queue-persist-tests--register s)
      (aob-acp--persist)
      (aob-session-put s :queue-paused t)
      (run-hook-with-args 'aob-queue-change-hook s)
      (aob-acp--flush-queued-writes)
      (should-not (plist-get (aob-queue-persist-tests--entry "acp-a")
                             :queue-paused)))))
