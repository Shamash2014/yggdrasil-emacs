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

(defun ygg-agent-profiles-tests--into-global (dir global)
  "Entries under DIR that resolve into GLOBAL, as paths relative to DIR."
  (seq-filter (lambda (rel)
                (string-prefix-p global (file-truename (expand-file-name rel dir))))
              (mapcar (lambda (f) (file-relative-name f dir))
                      (directory-files-recursively dir "" t nil))))

(ert-deftest ygg-agent-profiles-a-profile-home-writes-nothing-global ()
  (ygg-agent-profiles-tests--with
    (let ((global (file-name-as-directory (expand-file-name ".claude" top))))
      (dolist (d '("skills/mine" "agents" "plugins/cache/x" "plugins/marketplaces/m"))
        (make-directory (expand-file-name d global) t))
      (with-temp-file (expand-file-name "skills/mine/SKILL.md" global) (insert "s"))
      (with-temp-file (expand-file-name "plugins/installed_plugins.json" global) (insert "{}"))
      (ygg-agent-profiles-tests--choose "work")
      (let ((home (expand-file-name "profiles/work/claude" ygg-agent-conf-root)))
        (should (file-exists-p (expand-file-name "skills/mine/SKILL.md" home)))
        (should (file-exists-p (expand-file-name "plugins/installed_plugins.json" home)))
        (should (equal (sort (ygg-agent-profiles-tests--into-global home global) #'string<)
                       '("plugins/cache" "plugins/marketplaces")))))))

(ert-deftest ygg-agent-profiles-an-old-linked-profile-is-made-private ()
  (ygg-agent-profiles-tests--with
    (let* ((global (expand-file-name ".claude" top))
           (home (expand-file-name "profiles/work/claude" ygg-agent-conf-root)))
      (make-directory (expand-file-name "skills/mine" global) t)
      (make-directory home t)
      (make-symbolic-link (expand-file-name "skills" global) (expand-file-name "skills" home))
      (ygg-agent--profile-home "work" "claude")
      (should-not (file-symlink-p (expand-file-name "skills" home)))
      (should (file-directory-p (expand-file-name "skills/mine" home)))
      (should (file-directory-p (expand-file-name "skills/mine" global)))
      (make-directory (expand-file-name "skills/later" global))
      (ygg-agent--profile-home "work" "claude")
      (should (file-directory-p (expand-file-name "skills/later" home))))))

(ert-deftest ygg-agent-profiles-keep-their-mcp-logins-to-themselves ()
  (ygg-agent-profiles-tests--with
    (let* ((home (ygg-agent--profile-home "work" "claude"))
           (profile (ygg-agent--credential-name home))
           (written nil))
      (cl-letf (((symbol-function 'ygg-agent--credential-services)
                 (lambda () (list profile)))
                ((symbol-function 'ygg-agent--keychain-read)
                 (lambda (service)
                   (let ((h (make-hash-table :test #'equal))
                         (mcp (make-hash-table :test #'equal)))
                     (when (equal service profile)
                       (puthash "secret-server" (make-hash-table :test #'equal) mcp))
                     (puthash "mcpOAuth" mcp h)
                     h)))
                ((symbol-function 'ygg-agent--keychain-account) (lambda (_) "me"))
                ((symbol-function 'call-process)
                 (lambda (&rest args) (push args written) 0)))
        (let ((system-type 'darwin))
          (ygg-agent--share-mcp-auth (expand-file-name "proj/claude" ygg-agent-conf-root))))
      (should-not written))))

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

(ert-deftest ygg-agent-profiles-a-new-preset-is-made-saved-and-profiled ()
  (ygg-agent-profiles-tests--with
    (let ((aob-acp-profiles '(("claude" :agent "claude")))
          (answers '("work" "claude"))
          saved)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop answers)))
                ((symbol-function 'customize-save-variable)
                 (lambda (var val) (set var val) (setq saved t))))
        (should (equal (aob-acp-read-profile "Preset: ") "work"))
        (should saved)
        (should (equal (aob-acp-profile "work") '(:agent "claude" :profile "work")))
        (setq saved nil answers '("work"))
        (should (equal (aob-acp-read-profile "Preset: ") "work"))
        (should-not saved)))))

(ert-deftest ygg-agent-profiles-a-preset-spawns-under-its-profile ()
  (ygg-agent-profiles-tests--with
    (let ((aob-acp-profiles '(("work" :agent "claude" :profile "work")
                             ("claude" :agent "claude")))
          refs)
      (cl-letf (((symbol-function 'aob-acp-spawn)
                 (lambda (&rest _) (setq refs aob-acp-session-refs) nil)))
        (ygg-aob--with-profile #'aob-acp-spawn "work" nil)
        (should (equal (plist-get refs :profile) "work"))
        (ygg-aob--with-profile #'aob-acp-spawn "claude" nil)
        (should-not (plist-get refs :profile))
        (let ((aob-acp-session-refs '(:profile "kept")))
          (ygg-aob--with-profile #'aob-acp-spawn "work" nil)
          (should (equal (plist-get refs :profile) "kept")))))))

(ert-deftest ygg-agent-profiles-login-runs-where-q-leaves ()
  (ygg-agent-profiles-tests--with
    (let (call)
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'ghostel-compile--start)
                 (lambda (&rest args)
                   (let ((process-environment
                          (append compilation-environment
                                  (list "CLAUDE_CONFIG_DIR=/elsewhere")
                                  process-environment)))
                     (setq call (cons (getenv "CLAUDE_CONFIG_DIR") args)))
                   (current-buffer))))
        (let ((compilation-environment nil))
          (ygg-agent-login-config-home project "work")))
      (should (string-suffix-p "/profiles/work/claude" (car call)))
      (should (string-suffix-p "auth login" (nth 1 call)))
      (should (string-match-p "\\`env CLAUDE_CONFIG_DIR\\\\?=[^ ]*/profiles/work/claude " (nth 1 call)))
      (should (eq (nth 5 call) t)))))

(ert-deftest ygg-agent-profiles-start-without-the-global-provider ()
  (ygg-agent-profiles-tests--with
    (let ((global (expand-file-name ".pi/agent" top)))
      (make-directory global t)
      (with-temp-file (expand-file-name "settings.json" global)
        (insert "{\"defaultProvider\":\"omlx\",\"defaultModel\":\"m\",\"packages\":[\"npm:x\"]}"))
      (with-temp-file (expand-file-name "models.json" global)
        (insert "{\"providers\":{\"omlx\":{}}}"))
      (let* ((home (ygg-agent--profile-home "work" "pi"))
             (settings (json-parse-string
                        (with-temp-buffer
                          (insert-file-contents (expand-file-name "settings.json" home))
                          (buffer-string)))))
        (should-not (gethash "defaultProvider" settings))
        (should-not (gethash "defaultModel" settings))
        (should (gethash "packages" settings))
        (should-not (file-exists-p (expand-file-name "models.json" home)))))))

(ert-deftest ygg-agent-profiles-pi-edit-sets-the-home-engine-and-model ()
  (ygg-agent-profiles-tests--with
    (let ((spec '(:agent "pi" :profile "work")))
      (cl-letf (((symbol-function 'ygg-pi--home-models) (lambda (_) '("anthropic/claude-x")))
                ((symbol-function 'completing-read) (lambda (&rest _) "anthropic/claude-x")))
        (should (equal (ygg-pi-edit-profile "work" spec) spec)))
      (let ((settings (json-parse-string
                       (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "profiles/work/pi/settings.json" ygg-agent-conf-root))
                         (buffer-string)))))
        (should (equal (gethash "defaultProvider" settings) "anthropic"))
        (should (equal (gethash "defaultModel" settings) "claude-x")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
        (ygg-pi-edit-profile "work" spec))
      (should-not (string-match-p "defaultProvider"
                                  (with-temp-buffer
                                    (insert-file-contents
                                     (expand-file-name "profiles/work/pi/settings.json"
                                                       ygg-agent-conf-root))
                                    (buffer-string)))))))

(ert-deftest ygg-agent-profiles-other-agents-edit-their-model-and-mode ()
  (let ((answers '("opus" "")))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop answers))))
      (should (equal (ygg-pi-edit-profile "c" '(:agent "claude" :mode "auto"))
                     '(:agent "claude" :model "opus"))))))

(ert-deftest ygg-agent-profiles-delete-offers-the-home-to-the-trash ()
  (ygg-agent-profiles-tests--with
    (let ((home (ygg-agent--profile-home "work" "claude"))
          trashed)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'move-file-to-trash) (lambda (f) (setq trashed f))))
        (let ((noninteractive nil))
          (ygg-aob--trash-profile-home "work" '(:agent "claude" :profile "work"))))
      (should (equal trashed (directory-file-name (file-name-directory (directory-file-name home))))))))

(ert-deftest ygg-agent-profiles-a-session-login-runs-in-ghostel ()
  (let (started outcome)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'ghostel-compile--start)
               (lambda (command name dir &rest rest)
                 (setq started (list command name dir (nth 1 rest)))
                 (get-buffer-create name))))
      (let ((buf (ygg-aob--auth-in-ghostel "*login pi*" '("env" "A=b" "pi-acp" "--terminal-login")
                                           "/tmp/" (lambda (ok) (setq outcome (list ok))))))
        (unwind-protect
            (progn
              (should (equal started (list "env A\\=b pi-acp --terminal-login" "*login pi*" "/tmp/" t)))
              (run-hook-with-args 'ghostel-compile-finish-functions (get-buffer-create "other") "finished\n")
              (should-not outcome)
              (run-hook-with-args 'ghostel-compile-finish-functions buf "finished\n")
              (should (equal outcome '(t)))
              (should-not ghostel-compile-finish-functions))
          (kill-buffer buf))))
    (setq outcome nil)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'ghostel-compile--start)
               (lambda (_command name &rest _) (get-buffer-create name))))
      (kill-buffer (ygg-aob--auth-in-ghostel "*login pi*" '("pi") "/tmp/"
                                             (lambda (ok) (setq outcome (list ok)))))
      (should (equal outcome '(nil)))
      (should-not ghostel-compile-finish-functions))
    (setq outcome nil)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'ghostel-compile--start) (lambda (&rest _) (error "No pty"))))
      (should-error (ygg-aob--auth-in-ghostel "*login pi*" '("pi") "/tmp/"
                                              (lambda (ok) (setq outcome (list ok)))))
      (should (equal outcome '(nil)))
      (should-not ghostel-compile-finish-functions))))

(ert-deftest ygg-agent-profiles-open-the-home-a-session-runs-on ()
  (let* ((proc (start-process "home-probe" nil "sleep" "5"))
         (s (aob-create-session :id "acp:home" :backend 'acp :name "home"
                                :project "/tmp/" :dir "/tmp/" :state 'idle
                                :refs (list :agent "claude")))
         opened)
    (unwind-protect
        (progn
          (setf (aob-session-conn s) proc)
          (process-put proc 'aob-env '("CLAUDE_CONFIG_DIR=/x/profiles/w/claude" "HOME=/h"))
          (cl-letf (((symbol-function 'dired) (lambda (d) (setq opened d))))
            (ygg-agent-open-config-home s))
          (should (equal opened "/x/profiles/w/claude")))
      (delete-process proc)
      (aob-remove-session s))))

(provide 'ygg-agent-profiles-tests)
;;; ygg-agent-profiles-tests.el ends here
