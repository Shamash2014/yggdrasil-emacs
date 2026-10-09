;;; ygg-forge-config-tests.el --- per-project gh and glab logins -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-forge-config)
(defvar ygg-project-dirs)
(require 'ygg-git-review-requests)

(defmacro ygg-forge-config-tests--with-project (logins &rest body)
  "Run BODY in a fake project whose LOGINS, a list of (KIND FILE), exist."
  (declare (indent 1))
  `(let* ((conf (file-name-as-directory (make-temp-file "ygg-forge-conf-" t)))
          (root (file-name-as-directory (make-temp-file "ygg-forge-proj-" t)))
          (ygg-agent-conf-root conf)
          (default-directory root)
          (ygg-forge-config--repos (make-hash-table :test #'equal))
          (name (file-name-nondirectory (directory-file-name root)))
          (process-environment
           (append (list "GH_CONFIG_DIR=/nonexistent-gh" "GLAB_CONFIG_DIR=/nonexistent-glab"
                         "GITLAB_HOST" "GL_HOST" "XDG_CONFIG_HOME=/nonexistent-xdg")
                   process-environment)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".git" root) t)
           (pcase-dolist (`(,kind ,file ,text) ,logins)
             (let ((dir (expand-file-name (concat name "/" kind) conf)))
               (make-directory dir t)
               (with-temp-file (expand-file-name file dir) (insert text))))
           ,@body)
       (delete-directory conf t)
       (delete-directory root t))))

(ert-deftest ygg-forge-config-env-needs-a-login-file ()
  (ygg-forge-config-tests--with-project '(("gh" "hosts.yml" "git.example:\n"))
    (let ((env (ygg-forge-config-env root)))
      (should (equal 1 (length env)))
      (should (string-prefix-p "GH_CONFIG_DIR=" (car env)))
      (should (string-suffix-p (concat name "/gh") (car env))))
    (should-not (ygg-forge-config-env root "glab")))
  (ygg-forge-config-tests--with-project nil
    (make-directory (expand-file-name (concat name "/gh") conf) t)
    (should-not (ygg-forge-config-env root))))

(ert-deftest ygg-forge-config-env-names-only-the-forge-with-a-login ()
  (ygg-forge-config-tests--with-project '(("glab" "config.yml" "hosts:\n"))
    (should (seq-find (lambda (e) (string-prefix-p "GLAB_CONFIG_DIR=" e))
                      (ygg-forge-config-env root)))
    (should-not (seq-find (lambda (e) (string-prefix-p "GH_CONFIG_DIR=" e))
                          (ygg-forge-config-env root)))))

(ert-deftest ygg-forge-config-compare-process-sees-project-vars ()
  (ygg-forge-config-tests--with-project '(("gh" "hosts.yml" "git.example:\n"))
    (let (seen)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (push (getenv "GH_CONFIG_DIR") seen) (signal 'file-error nil)))
                ((symbol-function 'make-pipe-process) (lambda (&rest _) nil)))
        (ygg-git-compare--forge-async "gh" '("pr" "list") #'ignore)
        (should (string-suffix-p (concat name "/gh") (car seen)))
        (ygg-git-compare--forge-async "glab" '("api" "x") #'ignore)
        (should (equal "/nonexistent-glab" (getenv "GLAB_CONFIG_DIR")))))))

(ert-deftest ygg-forge-config-without-login-leaves-global ()
  (ygg-forge-config-tests--with-project nil
    (let (seen)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (push (getenv "GH_CONFIG_DIR") seen) (signal 'file-error nil)))
                ((symbol-function 'make-pipe-process) (lambda (&rest _) nil)))
        (ygg-git-compare--forge-async "gh" '("pr" "list") #'ignore)
        (should (equal "/nonexistent-gh" (car seen)))))))

(ert-deftest ygg-forge-config-review-requests-spawn-sees-project-vars ()
  (ygg-forge-config-tests--with-project '(("glab" "config.yml" "hosts:\n"))
    (let (seen)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (push (getenv "GLAB_CONFIG_DIR") seen) (signal 'file-error nil))))
        (ygg-git-review-requests--spawn '("glab" "api" "user") #'ignore)
        (should (string-suffix-p (concat name "/glab") (car seen)))
        (setq seen nil)
        (ygg-git-review-requests--spawn '("git" "status") #'ignore)
        (should (equal "/nonexistent-glab" (car seen)))))))

(ert-deftest ygg-forge-config-host-readers-prefer-project-file ()
  (ygg-forge-config-tests--with-project
      '(("gh" "hosts.yml" "ghe.project.example:\n    user: x\n")
        ("glab" "config.yml" "hosts:\n    gl.project.example:\n        api_host: gl.project.example\n"))
    (should (equal '("ghe.project.example") (ygg-git-compare--gh-hosts)))
    (should (assoc "gl.project.example" (ygg-git-compare--glab-hosts)))))

(ert-deftest ygg-forge-config-login-builds-command-and-env ()
  (ygg-forge-config-tests--with-project nil
    (let (call)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'completing-read)
                 (let ((answers '("glab" "gl.example.test")))
                   (lambda (&rest _) (pop answers))))
                ((symbol-function 'ygg-forge-config--run-terminal)
                 (lambda (r argv env) (setq call (list r argv env)))))
        (ygg-project-forge-login)
        (should (equal (expand-file-name root) (nth 0 call)))
        (should (equal '("glab" "auth" "login" "--hostname" "gl.example.test") (nth 1 call)))
        (should (equal (list (concat "GLAB_CONFIG_DIR="
                                     (expand-file-name (concat name "/glab") conf)))
                       (nth 2 call)))
        (should (file-directory-p (expand-file-name (concat name "/glab") conf)))
        (should-not (directory-files (expand-file-name (concat name "/glab") conf) nil "\\`[^.]"))))))

(ert-deftest ygg-forge-config-login-gh-has-no-hostname ()
  (ygg-forge-config-tests--with-project nil
    (let (call)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'completing-read) (lambda (&rest _) "gh"))
                ((symbol-function 'ygg-forge-config--run-terminal)
                 (lambda (r argv env) (setq call (list r argv env)))))
        (ygg-project-forge-login)
        (should (equal '("gh" "auth" "login") (nth 1 call)))
        (should (string-prefix-p "GH_CONFIG_DIR=" (car (nth 2 call))))))))

(ert-deftest ygg-forge-config-acp-environment-carries-homes-and-logins ()
  (ygg-forge-config-tests--with-project
      '(("gh" "hosts.yml" "git.example:\n") ("glab" "config.yml" "hosts:\n"))
    (let ((env (ygg-agent-acp-environment "claude" root root nil)))
      (should (seq-find (lambda (e) (string-prefix-p "CLAUDE_CONFIG_DIR=" e)) env))
      (should (seq-find (lambda (e) (string-suffix-p (concat name "/gh") e)) env))
      (should (seq-find (lambda (e) (string-suffix-p (concat name "/glab") e)) env))
      (should (seq-find (lambda (e) (string-prefix-p "CODEX_HOME=" e))
                        (ygg-agent-acp-environment "codex" root root nil)))
      (should (seq-find (lambda (e) (string-prefix-p "PI_CODING_AGENT_DIR=" e))
                        (ygg-agent-acp-environment "pi" root root nil))))))

(ert-deftest ygg-forge-config-acp-environment-omits-absent-logins ()
  (ygg-forge-config-tests--with-project nil
    (let ((env (ygg-agent-acp-environment "claude" root root nil)))
      (should-not (seq-find (lambda (e) (string-match-p "\\`GL?A?B?_?CONFIG_DIR=\\|\\`GH_CONFIG_DIR=" e))
                            env)))))

(ert-deftest ygg-forge-config-run-terminal-missing-ghostel-is-user-error ()
  (cl-letf (((symbol-function 'require)
             (lambda (feature &rest _) (not (eq feature 'ghostel)))))
    (should-error (ygg-forge-config--run-terminal "/tmp/" '("gh") nil) :type 'user-error)))

(ert-deftest ygg-forge-config-run-terminal-failure-cleans-up ()
  (let (window-deleted buffer-seen)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'ygg--term-split-window) (lambda () 'fake-window))
              ((symbol-function 'window-live-p) (lambda (w) (eq w 'fake-window)))
              ((symbol-function 'frame-root-window-p) (lambda (_) nil))
              ((symbol-function 'delete-window) (lambda (w) (setq window-deleted w)))
              ((symbol-function 'set-window-buffer) #'ignore)
              ((symbol-function 'select-window) #'ignore)
              ((symbol-function 'ygg-call-with-buffer-env)
               (lambda (thunk &optional _env) (funcall thunk)))
              ((symbol-function 'ghostel-exec)
               (lambda (buffer &rest _) (setq buffer-seen buffer) (error "boom"))))
      (should-error (ygg-forge-config--run-terminal temporary-file-directory '("gh") nil)
                    :type 'user-error)
      (should (eq 'fake-window window-deleted))
      (should-not (buffer-live-p buffer-seen)))))

(ert-deftest ygg-forge-config-login-glab-hosts-read-in-project-root ()
  (ygg-forge-config-tests--with-project
      '(("glab" "config.yml" "hosts:\n    gl.project.example:\n        api_host: gl.project.example\n"))
    (let ((elsewhere (file-name-as-directory (make-temp-file "ygg-forge-else-" t)))
          (answers (list "glab" ""))
          offered)
      (unwind-protect
          (let ((default-directory elsewhere))
            (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'fake))
                      ((symbol-function 'project-root) (lambda (_) root))
                      ((symbol-function 'completing-read)
                       (lambda (prompt coll &rest _)
                         (when (string-prefix-p "GitLab" prompt) (setq offered coll))
                         (pop answers)))
                      ((symbol-function 'ygg-forge-config--run-terminal) #'ignore))
              (ygg-project-forge-login)
              (should (member "gl.project.example" offered))))
        (delete-directory elsewhere t)))))

(defun ygg-forge-config-tests--git (dir &rest args)
  (let ((default-directory dir))
    (apply #'call-process "git" nil nil nil args)))

(ert-deftest ygg-project-config-open-visits-the-main-project-folder ()
  (let* ((conf (file-name-as-directory (make-temp-file "ygg-cfg-conf-" t)))
         (base (file-name-as-directory (make-temp-file "ygg-cfg-base-" t)))
         (repo (file-name-as-directory (expand-file-name "myrepo" base)))
         (tree (file-name-as-directory (expand-file-name "mytree" base)))
         (ygg-agent-conf-root conf)
         (ygg-forge-config--repos (make-hash-table :test #'equal))
         (want (file-name-as-directory (expand-file-name "myrepo" conf)))
         seen)
    (unwind-protect
        (progn
          (make-directory repo t)
          (ygg-forge-config-tests--git repo "init" "-q")
          (ygg-forge-config-tests--git repo "-c" "user.name=t" "-c" "user.email=t@t"
                                       "commit" "-q" "--allow-empty" "-m" "x")
          (ygg-forge-config-tests--git repo "worktree" "add" "-q" "-b" "wt" tree)
          (cl-letf (((symbol-function 'dired) (lambda (d &rest _) (setq seen d))))
            (dolist (start (list repo tree))
              (setq seen nil)
              (let ((default-directory start))
                (ygg-project-config-open))
              (should (equal want (file-name-as-directory seen)))
              (should (file-directory-p want)))))
      (delete-directory conf t)
      (delete-directory base t))))

(ert-deftest ygg-project-config-open-needs-a-root-and-a-project ()
  (let ((base (file-name-as-directory (make-temp-file "ygg-cfg-base-" t)))
        (repo nil))
    (unwind-protect
        (cl-letf (((symbol-function 'dired) (lambda (&rest _) (error "no dired"))))
          (setq repo (file-name-as-directory (expand-file-name "r" base)))
          (make-directory repo t)
          (ygg-forge-config-tests--git repo "init" "-q")
          (let ((ygg-agent-conf-root nil) (default-directory repo))
            (should-error (ygg-project-config-open) :type 'user-error))
          (let ((ygg-agent-conf-root base) (default-directory base))
            (cl-letf (((symbol-function 'ygg-project-roots) (lambda (&rest _) nil)))
              (should-error (ygg-project-config-open) :type 'user-error))))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-root-reads-the-sidebar-line ()
  (with-temp-buffer
    (insert (propertize "row\n" 'ygg-project "/tmp/ygg-side/"))
    (goto-char (point-min))
    (let ((default-directory "/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) (error "no"))))
        (should (equal "/tmp/ygg-side/" (ygg-forge-config--project-root)))))))

(ert-deftest ygg-forge-config-root-falls-back-to-known-roots ()
  (let* ((base (file-name-as-directory (make-temp-file "ygg-cfg-roots-" t)))
         (outer (file-name-as-directory (expand-file-name "a" base)))
         (inner (file-name-as-directory (expand-file-name "a/b" base)))
         (extra (file-name-as-directory (expand-file-name "extra" base)))
         (other (file-name-as-directory (expand-file-name "other" base))))
    (unwind-protect
        (progn
          (dolist (d (list outer inner extra other)) (make-directory d t))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'ygg-project-roots) (lambda (&rest _) (list outer inner)))
                    ((symbol-function 'completing-read) (lambda (&rest _) (error "prompted"))))
            (let ((default-directory (expand-file-name "sub/" inner))
                  (ygg-project-dirs (list (cons other (list extra)))))
              (make-directory default-directory t)
              (should (equal inner (ygg-forge-config--project-root)))
              (let ((default-directory extra))
                (should (equal other (ygg-forge-config--project-root)))))))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-root-finds-a-worktree-git-file ()
  (let* ((base (file-name-as-directory (make-temp-file "ygg-cfg-wt-" t)))
         (tree (file-name-as-directory (expand-file-name "tree" base)))
         (deep (file-name-as-directory (expand-file-name "src" tree))))
    (unwind-protect
        (progn
          (make-directory deep t)
          (with-temp-file (expand-file-name ".git" tree) (insert "gitdir: /nowhere\n"))
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'ygg-project-roots) (lambda (&rest _) nil))
                    ((symbol-function 'completing-read) (lambda (&rest _) (error "prompted"))))
            (let ((default-directory deep))
              (should (equal tree (ygg-forge-config--project-root))))))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-root-prompts-only-as-a-last-resort ()
  (let ((base (file-name-as-directory (make-temp-file "ygg-cfg-none-" t)))
        asked)
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'ygg-project-roots) (lambda (&rest _) '("/tmp/ygg-pick/")))
                  ((symbol-function 'completing-read)
                   (lambda (_p coll _pred req &rest _)
                     (setq asked (list coll req))
                     "/tmp/ygg-pick/")))
          (let ((default-directory base) (ygg-project-dirs nil))
            (should (equal "/tmp/ygg-pick/" (ygg-forge-config--project-root)))
            (should (equal '(("/tmp/ygg-pick/") t) asked))))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-root-without-projects-is-a-user-error ()
  (let ((base (file-name-as-directory (make-temp-file "ygg-cfg-empty-" t))))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'ygg-project-roots) (lambda (&rest _) nil)))
          (let ((default-directory base) (ygg-project-dirs nil))
            (should-error (ygg-forge-config--project-root) :type 'user-error)))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-root-owning-project-beats-folder-repo ()
  (let* ((base (file-name-as-directory (make-temp-file "ygg-cfg-own-" t)))
         (owner (file-name-as-directory (expand-file-name "owner" base)))
         (folder (file-name-as-directory (expand-file-name "folder" base))))
    (unwind-protect
        (progn
          (dolist (d (list owner folder)) (make-directory d t))
          (ygg-forge-config-tests--git folder "init" "-q")
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) (cons 'transient folder)))
                    ((symbol-function 'project-root) (lambda (_) folder))
                    ((symbol-function 'ygg-project-roots) (lambda (&rest _) (list owner))))
            (let ((default-directory folder)
                  (ygg-project-dirs (list (cons owner (list folder)))))
              (should (equal owner (ygg-forge-config--project-root))))))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-root-resolves-symlinks-and-keeps-sibling-guard ()
  (let* ((base (file-name-as-directory (make-temp-file "ygg-cfg-link-" t)))
         (proj (file-name-as-directory (expand-file-name "proj" base)))
         (proj2 (file-name-as-directory (expand-file-name "proj2" base)))
         (link (expand-file-name "link" base)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" proj) t)
          (make-directory proj2 t)
          (make-symbolic-link (expand-file-name "sub" proj) link)
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'ygg-project-roots) (lambda (&rest _) (list proj)))
                    ((symbol-function 'completing-read) (lambda (&rest _) (error "prompted"))))
            (let ((ygg-project-dirs nil))
              (let ((default-directory (file-name-as-directory link)))
                (should (equal proj (ygg-forge-config--project-root))))
              (let ((default-directory proj2))
                (should-error (ygg-forge-config--project-root))))))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-containing-skips-remote-roots-without-tramp ()
  (let* ((base (file-name-as-directory (make-temp-file "ygg-cfg-rem-" t)))
         (local (file-name-as-directory (expand-file-name "proj" base)))
         (remote "/ssh:nobody@host.invalid:/srv/proj/"))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" local) t)
          (cl-letf (((symbol-function 'tramp-file-name-handler)
                     (lambda (&rest _) (error "tramp touched"))))
            (should (equal (cons 'l local)
                           (ygg-forge-config--containing
                            (expand-file-name "sub" local)
                            (list (cons 'r remote) (cons 'l local)))))
            (should-not (ygg-forge-config--containing
                         "/ssh:nobody@host.invalid:/srv/proj/x"
                         (list (cons 'l local))))))
      (delete-directory base t))))

(ert-deftest ygg-forge-config-containing-symlinked-root-keeps-sibling-guard ()
  (let* ((base (file-name-as-directory (make-temp-file "ygg-cfg-sl-" t)))
         (proj (expand-file-name "proj" base))
         (proj2 (expand-file-name "proj2" base))
         (link (expand-file-name "link" base)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" proj) t)
          (make-directory proj2 t)
          (make-symbolic-link proj link)
          (let ((cands (list (cons 'r link))))
            (should (ygg-forge-config--containing (expand-file-name "sub" proj) cands))
            (should-not (ygg-forge-config--containing proj2 cands))))
      (delete-directory base t))))

;;; config folders

(defmacro ygg-forge-config-tests--with-homes (&rest body)
  (declare (indent 0))
  `(ygg-forge-config-tests--with-project nil
     (let* ((real (file-name-as-directory (make-temp-file "ygg-forge-real-" t)))
            (ygg-agent--config-homes
             (mapcar (lambda (cell)
                       (let ((spec (copy-sequence (cdr cell))))
                         (plist-put spec :home (expand-file-name (car cell) real))
                         (cons (car cell) spec)))
                     ygg-agent--config-homes))
            (ygg-agent--config-dirs (make-hash-table :test #'equal)))
       (unwind-protect (progn ,@body)
         (delete-directory real t)))))

(ert-deftest ygg-forge-config-init-makes-every-kind ()
  (ygg-forge-config-tests--with-homes
    (let ((made (ygg-project-config-init root)))
      (should (= 5 (length made)))
      (dolist (kind '("claude" "codex" "pi" "gh" "glab"))
        (should (file-directory-p (expand-file-name (concat name "/" kind) conf)))))))

(ert-deftest ygg-forge-config-init-skips-a-kind-with-a-marker ()
  (ygg-forge-config-tests--with-homes
    (with-temp-file (expand-file-name ".codex-home" root) (insert "elsewhere\n"))
    (ygg-project-config-init root)
    (should-not (file-exists-p (expand-file-name (concat name "/codex") conf)))
    (should (file-directory-p (expand-file-name (concat name "/claude") conf)))))

(ert-deftest ygg-forge-config-init-leaves-remote-projects-alone ()
  (ygg-forge-config-tests--with-homes
    (should-not (ygg-project-config-init "/ssh:nobody@host.invalid:/srv/proj/"))
    (should-not (directory-files conf nil directory-files-no-dot-files-regexp))))

(ert-deftest ygg-forge-config-init-is-idempotent ()
  (ygg-forge-config-tests--with-homes
    (let ((first (ygg-project-config-init root)))
      (should (equal first (ygg-project-config-init root))))))

(ert-deftest ygg-forge-config-init-runs-no-keychain-process ()
  (ygg-forge-config-tests--with-homes
    (let (programs)
      (cl-letf (((symbol-function 'ygg-agent--repo-home) (lambda (project) project))
                ((symbol-function 'ygg-agent--keychain-read)
                 (lambda (&rest _) (push "keychain-read" programs) nil))
                ((symbol-function 'call-process)
                 (lambda (program &rest _) (push program programs) 0))
                ((symbol-function 'process-file)
                 (lambda (program &rest _) (push program programs) 0))
                ((symbol-function 'process-lines)
                 (lambda (program &rest _) (push program programs) nil))
                ((symbol-function 'make-process)
                 (lambda (&rest args) (push (plist-get args :command) programs) nil)))
        (ygg-project-config-init root))
      (should-not programs))))

(ert-deftest ygg-forge-config-init-keeps-home-selection-and-env ()
  (ygg-forge-config-tests--with-homes
    (let* ((spec (cdr (assoc "claude" ygg-agent--config-homes)))
           (own (ygg-agent--own-home "claude" root))
           (home (plist-get spec :home))
           (before (cl-letf (((symbol-function 'ygg-agent--logged-in-p)
                              (lambda (_k dir &optional _c) (equal dir home))))
                     (ygg-agent--authenticated-home "claude" spec (list own)))))
      (ygg-project-config-init root)
      (should (file-directory-p own))
      (should (equal before
                     (cl-letf (((symbol-function 'ygg-agent--logged-in-p)
                                (lambda (_k dir &optional _c) (equal dir home))))
                       (ygg-agent--authenticated-home "claude" spec (list own)))))
      (should (equal (expand-file-name home) before))
      (should-not (ygg-forge-config-env root)))))

(ert-deftest ygg-forge-config-import-has-a-config-folders-step ()
  (require 'ygg-project-scan)
  (let (steps)
    (cl-letf (((symbol-function 'ygg-project-import--run) (lambda (_root s _cb) (setq steps s))))
      (ygg-project-import "/tmp/cart/"))
    (should (assoc "config folders" steps))))

(defun ygg-forge-config-tests--import-calls (extras forge-repo)
  (let (steps calls)
    (cl-letf (((symbol-function 'ygg-project-import--run) (lambda (_r s _cb) (setq steps s)))
              ((symbol-function 'ygg-forge-config--run-terminal)
               (lambda (r argv env)
                 (push (list r argv (mapcar (lambda (e) (car (split-string e "="))) env)) calls)))
              ((symbol-function 'ygg-git-compare--forge-repo) (lambda (&rest _) forge-repo)))
      (ygg-project-import default-directory nil extras)
      (dolist (step steps) (funcall (cdr step))))
    (list (mapcar #'car steps) (nreverse calls))))

(ert-deftest ygg-forge-config-import-gh-runs-login-with-config-dir-name ()
  (require 'ygg-project-scan)
  (ygg-forge-config-tests--with-project nil
    (let ((calls (cadr (ygg-forge-config-tests--import-calls '("gh") nil))))
      (should (equal 1 (length calls)))
      (should (equal '("gh" "auth" "login") (nth 1 (car calls))))
      (should (equal '("GH_CONFIG_DIR") (nth 2 (car calls)))))))

(ert-deftest ygg-forge-config-import-glab-takes-host-from-gitlab-remote ()
  (require 'ygg-project-scan)
  (ygg-forge-config-tests--with-project nil
    (let ((calls (cadr (ygg-forge-config-tests--import-calls
                        '("glab") '(gitlab "gl.example.test" "a/b")))))
      (should (equal '("glab" "auth" "login" "--hostname" "gl.example.test")
                     (nth 1 (car calls))))
      (should (equal '("GLAB_CONFIG_DIR") (nth 2 (car calls)))))
    (let ((calls (cadr (ygg-forge-config-tests--import-calls
                        '("glab") '(github "github.com" "a/b")))))
      (should (equal '("glab" "auth" "login") (nth 1 (car calls)))))))

(ert-deftest ygg-forge-config-import-skips-existing-login-and-unpicked ()
  (require 'ygg-project-scan)
  (ygg-forge-config-tests--with-project '(("gh" "hosts.yml" "git.example:\n"))
    (should-not (cadr (ygg-forge-config-tests--import-calls '("gh") nil)))
    (should-not (cadr (ygg-forge-config-tests--import-calls nil nil)))
    (should-not (cadr (ygg-forge-config-tests--import-calls '("skills") nil)))))

(ert-deftest ygg-forge-config-import-keeps-ice-last ()
  (require 'ygg-project-scan)
  (ygg-forge-config-tests--with-project nil
    (let* ((res (ygg-forge-config-tests--import-calls '("ice" "glab" "gh") nil))
           (names (car res)))
      (should (equal "ice" (car (last names))))
      (should (equal 2 (length (cadr res))))
      (should (equal '("gh login" "glab login") (seq-intersection names '("gh login" "glab login")))))))

(provide 'ygg-forge-config-tests)
;;; ygg-forge-config-tests.el ends here
