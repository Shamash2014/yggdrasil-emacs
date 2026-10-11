;;; aob-trace-work-root-tests.el --- Work root of remote and local sessions -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-trace-work-root-sessions-" nil ".eld"))
(require 'aob-trace)

(defun aob-trace-work-root-tests--session (dir path)
  (aob-session--create
   :id "wr" :dir dir
   :events (list (list :type 'tool :locations (list (list :path path))))))

(ert-deftest aob-trace-work-root-local-walks-to-git ()
  (let* ((root (file-name-as-directory (make-temp-file "aob-wr-" t)))
         (file (expand-file-name "sub/a.el" root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (make-directory (file-name-directory file))
          (should (equal (aob-trace--work-root
                          (aob-trace-work-root-tests--session root file))
                         (file-name-as-directory (expand-file-name root)))))
      (delete-directory root t))))

(ert-deftest aob-trace-work-root-remote-never-walks ()
  (cl-letf (((symbol-function 'locate-dominating-file)
             (lambda (&rest _) (error "walked"))))
    (let ((s (aob-trace-work-root-tests--session
              "/ssh:host:/home/u/proj/" "/home/u/proj/sub/a.el")))
      (should (equal (aob-trace--work-root s) "/ssh:host:/home/u/proj/"))
      (setf (aob-session-events s)
            (list (list :type 'tool
                        :locations (list (list :path "/ssh:host:/home/u/proj/b.el")))))
      (should (equal (aob-trace--work-root s) "/ssh:host:/home/u/proj/")))))

(ert-deftest aob-trace-work-root-remote-outside-is-nil ()
  (cl-letf (((symbol-function 'locate-dominating-file)
             (lambda (&rest _) (error "walked"))))
    (should-not (aob-trace--work-root
                 (aob-trace-work-root-tests--session
                  "/ssh:host:/home/u/proj/" "/etc/hosts")))
    (should-not (aob-trace--work-root
                 (aob-trace-work-root-tests--session
                  "/ssh:host:/home/u/proj/" "/ssh:other:/home/u/proj/a.el")))))

(ert-deftest aob-trace-work-root-follow-root-remote-does-not-walk ()
  (cl-letf (((symbol-function 'locate-dominating-file)
             (lambda (&rest _) (error "walked"))))
    (with-temp-buffer
      (let ((default-directory "/tmp/")
            (aob-trace--root-seq 'none))
        (aob-trace--follow-root
         (aob-trace-work-root-tests--session
          "/ssh:host:/home/u/proj/" "/home/u/proj/a.el"))
        (should (equal default-directory "/tmp/"))))))

(provide 'aob-trace-work-root-tests)
