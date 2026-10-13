;;; ygg-agent-profiles-tests.el --- agent profiles: another config home per session -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'ygg-agent-conf)
(require 'aob)
(require 'aob-acp)
(require 'layer-aob)

(defmacro ygg-agent-profiles-tests--with (&rest body)
  (declare (indent 0))
  `(let* ((top (file-truename (make-temp-file "ygg-profiles-" t)))
          (process-environment (cons (concat "HOME=" top) process-environment))
          (ygg-agent-conf-root (expand-file-name "conf" top))
          (project (expand-file-name "proj/" top)))
     (make-directory project t)
     (clrhash ygg-agent--config-dirs)
     (unwind-protect (progn ,@body)
       (delete-directory top t))))

(defun ygg-agent-profiles-tests--choose (name)
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) name)))
    (ygg-agent-read-profile "claude")))

(ert-deftest ygg-agent-profiles-default-keeps-the-project-home ()
  (ygg-agent-profiles-tests--with
    (let ((before (ygg-agent--config-env "claude" "claude" project)))
      (should-not (ygg-agent-profiles-tests--choose "default"))
      (should-not (ygg-agent-profile-env nil "claude"))
      (should (equal before (ygg-agent--config-env "claude" "claude" project)))
      (should-not (file-exists-p (expand-file-name "profiles" ygg-agent-conf-root))))))

(ert-deftest ygg-agent-profiles-point-every-kind-at-the-profile-home ()
  (ygg-agent-profiles-tests--with
    (dolist (kind '("claude" "codex" "pi"))
      (let ((var (plist-get (cdr (assoc kind ygg-agent--config-homes)) :var)))
        (should (equal (ygg-agent-profile-env "work" kind)
                       (list (format "%s=%s" var
                                     (expand-file-name (concat "profiles/work/" kind)
                                                       ygg-agent-conf-root)))))))))

(ert-deftest ygg-agent-profiles-a-new-name-is-made-and-seeded ()
  (ygg-agent-profiles-tests--with
    (let ((shared (expand-file-name ".claude/skills" top))
          (seeded nil))
      (make-directory shared t)
      (cl-letf (((symbol-function 'ygg-agent--bootstrap-share)
                 (lambda (spec dir &rest _)
                   (push (cons (plist-get spec :var) dir) seeded)
                   (make-directory dir t))))
        (should (equal (ygg-agent-profiles-tests--choose "work") "work"))
        (should (equal (ygg-agent-profiles) '("work")))
        (should (equal seeded
                       (list (cons "CLAUDE_CONFIG_DIR"
                                   (expand-file-name "profiles/work/claude"
                                                     ygg-agent-conf-root)))))
        (should (equal (ygg-agent-profiles-tests--choose "work") "work"))
        (should (= (length seeded) 1))))))

(ert-deftest ygg-agent-profiles-a-new-profile-shares-skills-like-a-project-home ()
  (ygg-agent-profiles-tests--with
    (make-directory (expand-file-name ".claude/skills" top) t)
    (ygg-agent-profiles-tests--choose "work")
    (should (file-symlink-p (expand-file-name "profiles/work/claude/skills"
                                              ygg-agent-conf-root)))))

(ert-deftest ygg-agent-profiles-reject-a-path-as-a-name ()
  (ygg-agent-profiles-tests--with
    (should-error (ygg-agent-profiles-tests--choose "../x") :type 'user-error)))

(ert-deftest ygg-agent-profiles-persist-and-resume-without-asking ()
  (ygg-agent-profiles-tests--with
    (let* ((s (aob-create-session :id "acp:profiled" :backend 'acp :name "profiled"
                                  :project project :dir project :state 'idle
                                  :refs (list :agent "codex" :acp-id "abc"
                                              :profile "work")))
           (entry (unwind-protect (aob-acp--entry s)
                    (aob-remove-session s)))
           refs asked)
      (should (equal (plist-get entry :profile) "work"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq asked t) "other"))
                ((symbol-function 'aob-acp--open)
                 (lambda (&rest _) (setq refs aob-acp-session-refs) nil)))
        (aob-acp--resume-entry entry nil))
      (should-not asked)
      (should (equal (plist-get refs :profile) "work")))))

(ert-deftest ygg-agent-profiles-asked-on-every-typed-spawn ()
  (ygg-agent-profiles-tests--with
    (let (refs)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "work"))
                ((symbol-function 'aob-acp-spawn)
                 (lambda (&rest _) (setq refs aob-acp-session-refs) nil)))
        (let ((noninteractive nil))
          (ygg-aob--with-profile #'aob-acp-spawn "claude" nil)
          (should-not (plist-get refs :profile))
          (let ((aob-prompt-typed t))
            (ygg-aob--with-profile #'aob-acp-spawn "claude" nil)
            (should (equal (plist-get refs :profile) "work"))
            (let ((aob-acp-session-refs '(:profile "kept")))
              (ygg-aob--with-profile #'aob-acp-spawn "claude" nil)
              (should (equal (plist-get refs :profile) "kept")))))))))

(provide 'ygg-agent-profiles-tests)
;;; ygg-agent-profiles-tests.el ends here
