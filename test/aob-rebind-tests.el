;;; aob-rebind-tests.el --- rebinding saved sessions whose folder is gone  -*- lexical-binding: t -*-

(require 'ert)
(require 'aob-acp)
(require 'aob-transcript)
(require 'cl-lib)

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

(ert-deftest aob-rebind-moves-every-path-under-the-old-folder ()
  (let ((new (make-temp-file "aob-rebind-dir-" t)))
    (unwind-protect
        (aob-rebind-tests--with-file
            (list (list :agent "claude" :name "n" :acp-id "gone-1"
                        :project "/no/such/" :dir "/no/such/app/"
                        :task "/no/such/app/tasks/t1" :todo-file "/no/such/app/todo.md"
                        :extra-dirs '("/no/such/app/lib" "/elsewhere/lib")))
          (aob-acp-rebind-persisted (aob-acp-persisted-entry "gone-1") new)
          (let ((e (aob-acp-persisted-entry "gone-1"))
                (root (directory-file-name (expand-file-name new))))
            (should (equal (plist-get e :dir) (file-name-as-directory new)))
            (should (equal (plist-get e :project) "/no/such/"))
            (should (equal (plist-get e :task) (concat root "/tasks/t1")))
            (should (equal (plist-get e :todo-file) (concat root "/todo.md")))
            (should (equal (plist-get e :extra-dirs)
                           (list (concat root "/lib") "/elsewhere/lib")))))
      (delete-directory new t))))

(ert-deftest aob-rebind-with-no-matching-entry-says-so ()
  (let ((new (make-temp-file "aob-rebind-dir-" t)))
    (unwind-protect
        (aob-rebind-tests--with-file (list (aob-rebind-tests--entry "/no/such/folder/"))
          (let ((err (should-error
                      (aob-acp-rebind-persisted
                       (list :acp-id "other" :dir "/x/") new)
                      :type 'user-error)))
            (should (string-match-p "no saved sessions" (cadr err))))
          (should (equal (plist-get (aob-acp-persisted-entry "gone-1") :dir)
                         "/no/such/folder/")))
      (delete-directory new t))))

(ert-deftest aob-rebind-moves-the-held-session-so-a-persist-keeps-it ()
  (let ((new (make-temp-file "aob-rebind-dir-" t))
        (aob--sessions (make-hash-table :test #'equal))
        (aob--order nil))
    (unwind-protect
        (aob-rebind-tests--with-file (list (aob-rebind-tests--entry "/no/such/folder/"))
          (let ((s (aob-session--create :id "acp:gone-1" :backend 'acp :name "claude:1"
                                        :project "/no/such/folder/"
                                        :dir "/no/such/folder/" :state 'done)))
            (aob-session-put s :acp-id "gone-1")
            (aob-session-put s :asleep (aob-rebind-tests--entry "/no/such/folder/"))
            (puthash (aob-session-id s) s aob--sessions)
            (push (aob-session-id s) aob--order)
            (aob-acp-rebind-persisted (aob-acp-persisted-entry "gone-1") new)
            (should (equal (aob-session-dir s) (file-name-as-directory new)))
            (should (equal (plist-get (aob-session-ref s :asleep) :dir)
                           (file-name-as-directory new)))
            (aob-acp--persist)
            (should (equal (plist-get (aob-acp-persisted-entry "gone-1") :dir)
                           (file-name-as-directory new)))))
      (delete-directory new t))))

(ert-deftest aob-rebind-persist-trims-gone-folders-first ()
  (let ((here (make-temp-file "aob-rebind-here-" t))
        (aob-acp-history-limit 2))
    (unwind-protect
        (aob-rebind-tests--with-file
            (list (list :acp-id "gone-a" :dir "/no/such/a/")
                  (list :acp-id "live-b" :dir here)
                  (list :acp-id "gone-c" :dir "/no/such/c/")
                  (list :acp-id "live-d" :dir here))
          (aob-acp--persist)
          (should (equal (mapcar (lambda (e) (plist-get e :acp-id))
                                 (aob-acp--persisted-entries))
                         '("live-b" "live-d"))))
      (delete-directory here t))))

(ert-deftest aob-rebind-unread-flip-writes-only-that-entry ()
  (aob-rebind-tests--with-file
      (list (list :acp-id "a" :dir "/x/" :name "a")
            (list :acp-id "b" :dir "/y/" :name "b"))
    (let ((s (aob-session--create :id "acp:a" :backend 'acp :name "a" :state 'idle)))
      (aob-session-put s :acp-id "a")
      (aob-session-put s :unread t)
      (cl-letf (((symbol-function 'aob-acp--persist)
                 (lambda (&rest _) (error "full persist"))))
        (aob-acp--persist-unread s))
      (should (plist-get (aob-acp-persisted-entry "a") :unread))
      (should (equal (plist-get (aob-acp-persisted-entry "b") :name) "b")))))

(ert-deftest aob-rebind-asleep-session-keeps-its-unread-flag ()
  (let* ((file (make-temp-file "aob-asleep-" nil ".txt"))
         (aob--sessions (make-hash-table :test #'equal))
         (aob--order nil))
    (unwind-protect
        (cl-letf (((symbol-function 'aob-transcript-turns) (lambda (&rest _) nil)))
          (let ((s (aob-transcript--session
                    (list :acp-id "z" :agent "claude" :name "z" :dir "/x/" :unread t)
                    file)))
            (should (aob-session-ref s :unread))))
      (delete-file file))))

(provide 'aob-rebind-tests)
