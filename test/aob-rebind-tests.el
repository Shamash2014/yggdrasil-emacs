;;; aob-rebind-tests.el --- rebinding saved sessions whose folder is gone  -*- lexical-binding: t -*-

(require 'ert)
(require 'aob-acp)

(defmacro aob-rebind-tests--with-file (entries &rest body)
  (declare (indent 1))
  `(let ((aob-acp-persist-file (make-temp-file "aob-rebind-" nil ".eld"))
         (aob-acp--opened-any t)
         (aob-acp--persisted-cache nil))
     (unwind-protect
         (progn
           (with-temp-file aob-acp-persist-file (prin1 ,entries (current-buffer)))
           ,@body)
       (delete-file aob-acp-persist-file))))

(defun aob-rebind-tests--entry (dir)
  (list :agent "claude" :name "claude:1" :project dir :dir dir :acp-id "gone-1"))

(ert-deftest aob-rebind-persist-keeps-an-entry-with-a-missing-dir ()
  (aob-rebind-tests--with-file (list (aob-rebind-tests--entry "/no/such/folder/"))
    (aob-acp--persist)
    (should (equal (plist-get (aob-acp-persisted-entry "gone-1") :dir)
                   "/no/such/folder/"))))

(ert-deftest aob-rebind-rewrites-dir-and-keeps-the-conversation ()
  (let ((new (make-temp-file "aob-rebind-dir-" t)))
    (unwind-protect
        (aob-rebind-tests--with-file (list (aob-rebind-tests--entry "/no/such/folder/"))
          (aob-acp-rebind-persisted (aob-acp-persisted-entry "gone-1") new)
          (let ((e (aob-acp-persisted-entry "gone-1")))
            (should (equal (plist-get e :dir) (file-name-as-directory new)))
            (should (equal (plist-get e :project) (file-name-as-directory new)))
            (should (equal (plist-get e :name) "claude:1"))
            (should (member e (aob-acp-resumable-entries)))))
      (delete-directory new t))))

(ert-deftest aob-rebind-refuses-a-non-directory ()
  (let ((file (make-temp-file "aob-rebind-file-")))
    (unwind-protect
        (aob-rebind-tests--with-file (list (aob-rebind-tests--entry "/no/such/folder/"))
          (should-error (aob-acp-rebind-persisted
                         (aob-acp-persisted-entry "gone-1") file)
                        :type 'user-error)
          (should (equal (plist-get (aob-acp-persisted-entry "gone-1") :dir)
                         "/no/such/folder/")))
      (delete-file file))))

(ert-deftest aob-rebind-resume-of-a-missing-dir-names-the-rebind-command ()
  (aob-rebind-tests--with-file (list (aob-rebind-tests--entry "/no/such/folder/"))
    (should-not (aob-acp-resumable-entries))
    (let ((err (should-error (aob-acp-resume-entry (aob-acp-persisted-entry "gone-1"))
                             :type 'user-error)))
      (should (string-match-p "aob-acp-rebind-persisted" (cadr err))))))

(provide 'aob-rebind-tests)
