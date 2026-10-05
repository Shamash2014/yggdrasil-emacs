;;; ygg-session-startup-tests.el --- Startup loads a session -*- lexical-binding: t; -*-

(require 'ert)
(require 'layer-sessions)
(require 'easysession)
(require 'server)

(defmacro ygg-session-startup-tests--with (var &rest body)
  "Run BODY with a temp session dir, a git project at VAR, and nothing loaded yet."
  (declare (indent 1))
  `(let* ((easysession-directory (make-temp-file "ygg-es" t))
          (,var (file-name-as-directory (file-truename (make-temp-file "ygg-proj" t))))
          (default-directory ,var)
          (command-line-args '("emacs"))
          (ygg-session--started nil)
          (loaded nil)
          (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment)))
     (call-process "git" nil nil nil "init" "-q" "-b" "trunk")
     (setq easysession--current-session-name nil easysession--session-loaded nil)
     (cl-letf (((symbol-function 'easysession-switch-to)
                (lambda (name) (push name loaded) (ygg-session--adopt name))))
       (unwind-protect (progn ,@body)
         (setq easysession--current-session-name nil easysession--session-loaded nil)
         (delete-directory easysession-directory t)
         (delete-directory ,var t)))))

(defun ygg-session-startup-tests--saved (name &optional age)
  (let ((file (easysession-get-session-file-path name)))
    (write-region "" nil file)
    (set-file-times file (time-subtract nil (or age 0)))))

(ert-deftest ygg-session-start-loads-the-project-session-and-marks-it-loaded ()
  (ygg-session-startup-tests--with root
    (let ((name (ygg-session--project-name root)))
      (ygg-session-startup-tests--saved name 100)
      (ygg-session-startup-tests--saved "other" 0)
      (ygg-session-restore-on-start)
      (should (equal loaded (list name)))
      (should (equal (easysession-get-session-name) name))
      (should easysession--session-loaded))))

(ert-deftest ygg-session-start-without-a-project-session-takes-the-latest ()
  (ygg-session-startup-tests--with root
    (ygg-session-startup-tests--saved "old" 100)
    (ygg-session-startup-tests--saved "new" 10)
    (ygg-session-restore-on-start)
    (should (equal loaded '("new")))))

(ert-deftest ygg-session-start-project-only-loads-nothing-without-one ()
  (ygg-session-startup-tests--with root
    (let ((ygg-session-restore-on-start 'project))
      (ygg-session-startup-tests--saved "new")
      (ygg-session-restore-on-start)
      (should-not loaded))))

(ert-deftest ygg-session-start-nil-loads-nothing ()
  (ygg-session-startup-tests--with root
    (let ((ygg-session-restore-on-start nil))
      (ygg-session-startup-tests--saved (ygg-session--project-name root))
      (ygg-session-restore-on-start)
      (should-not loaded))))

(ert-deftest ygg-session-start-follows-the-first-file-argument ()
  (ygg-session-startup-tests--with root
    (let* ((other (file-name-as-directory (file-truename (make-temp-file "ygg-proj" t))))
           (file (expand-file-name "a.txt" other))
           (command-line-args (list "emacs" "-nw" file)))
      (unwind-protect
          (let ((name (let ((default-directory other))
                        (call-process "git" nil nil nil "init" "-q" "-b" "trunk")
                        (ygg-session--project-name other))))
            (write-region "" nil file)
            (ygg-session-startup-tests--saved name)
            (ygg-session-restore-on-start)
            (should (equal loaded (list name))))
        (delete-directory other t)))))

(ert-deftest ygg-session-start-runs-once-and-a-daemon-waits-for-a-frame ()
  (ygg-session-startup-tests--with root
    (ygg-session-startup-tests--saved "only")
    (let ((server-after-make-frame-hook (list #'ygg-session-restore-on-start)))
      (should-not loaded)
      (run-hooks 'server-after-make-frame-hook)
      (should (equal loaded '("only")))
      (should-not server-after-make-frame-hook)
      (ygg-session-restore-on-start)
      (should (equal loaded '("only"))))))

(provide 'ygg-session-startup-tests)
;;; ygg-session-startup-tests.el ends here
