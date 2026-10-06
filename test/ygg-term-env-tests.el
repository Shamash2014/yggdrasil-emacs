;;; ygg-term-env-tests.el --- terminals carry their project's agent homes -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'ygg-agent-conf)
(require 'ygg-pi)
(require 'ghostel)
(require 'layer-sessions)
(require 'layer-aob)
(require 'layer-terminal)

(defvar ygg-term-tests--root nil)
(defvar ygg-term-tests--spawned nil)

(defconst ygg-term-tests--homes '("CLAUDE_CONFIG_DIR" "CODEX_HOME" "PI_CODING_AGENT_DIR"))
(defconst ygg-term-tests--session-vars '("PI_ACP_PI_COMMAND" "AOB_PI_MCP_SERVERS" "AOB_PI_APPROVE"))

(defun ygg-term-tests--dir (name)
  (let ((dir (expand-file-name name ygg-term-tests--root)))
    (make-directory dir t)
    (file-name-as-directory dir)))

(defun ygg-term-tests--ghostel (&optional _arg)
  (let ((buf (generate-new-buffer " *ygg-term-test*")))
    (with-current-buffer buf
      (ghostel--spawn-pty "sh" nil nil (file-remote-p default-directory)))
    buf))

(defmacro ygg-term-tests--with (&rest body)
  (declare (indent 0))
  `(let* ((ygg-term-tests--root (file-truename (make-temp-file "ygg-term-" t)))
          (ygg-term-tests--spawned nil)
          (ygg-inject-mise nil)
          (ygg-agent-conf-root (expand-file-name "conf" ygg-term-tests--root))
          (process-environment (append (list (concat "HOME=" ygg-term-tests--root)
                                             "CLAUDE_CONFIG_DIR=/global/claude")
                                       process-environment)))
     (clrhash ygg-agent--config-dirs)
     (cl-letf (((symbol-function 'ygg-agent--logged-in-p) (lambda (&rest _) t))
               ((symbol-function 'ygg-agent--adopt-plugin-mcp) #'ignore)
               ((symbol-function 'ygg-agent--share-mcp-auth) #'ignore)
               ((symbol-function 'ghostel) #'ygg-term-tests--ghostel)
               ((symbol-function 'ghostel--terminal-env) (lambda () nil))
               ((symbol-function 'ghostel--spawn-process)
                (lambda (&rest _)
                  (push (copy-sequence process-environment) ygg-term-tests--spawned)
                  nil)))
       (unwind-protect (progn ,@body)
         (clrhash ygg-agent--config-dirs)
         (mapc #'kill-buffer (seq-filter (lambda (b) (string-prefix-p " *ygg-term-test" (buffer-name b)))
                                         (buffer-list)))
         (delete-directory ygg-term-tests--root t)))))

(defun ygg-term-tests--last ()
  (car ygg-term-tests--spawned))

(defun ygg-term-tests--value (var env)
  (let ((hit (seq-find (lambda (e) (string-prefix-p (concat var "=") e)) env)))
    (and hit (substring hit (1+ (length var))))))

(defun ygg-term-tests--expected (dir)
  (mapcar (lambda (e) (string-match "\\`\\([^=]+\\)=" e)
            (cons (match-string 1 e) e))
          (ygg-agent-terminal-env dir)))

(ert-deftest ygg-term-new-carries-the-project-homes ()
  (ygg-term-tests--with
    (let ((p (ygg-term-tests--dir "p")))
      (let ((default-directory p))
        (ygg--ghostel-shell "*ygg-term:t*"))
      (let ((env (ygg-term-tests--last)))
        (dolist (var ygg-term-tests--homes)
          (should (ygg-term-tests--value var env))
          (should (string-prefix-p ygg-agent-conf-root
                                   (ygg-term-tests--value var env))))
        (should (string-match-p "/p/" (ygg-term-tests--value "PI_CODING_AGENT_DIR" env)))))))

(ert-deftest ygg-term-p-then-q-leaves-nothing-of-p ()
  (ygg-term-tests--with
    (let ((p (ygg-term-tests--dir "p")) (q (ygg-term-tests--dir "q")))
      (let ((default-directory p)) (ygg--ghostel-shell "*ygg-term:p*"))
      (let ((default-directory q)) (ygg--ghostel-shell "*ygg-term:q*"))
      (let ((env-q (ygg-term-tests--last))
            (env-p (cadr ygg-term-tests--spawned)))
        (dolist (var ygg-term-tests--homes)
          (should (string-match-p "/q/" (ygg-term-tests--value var env-q)))
          (should (string-match-p "/p/" (ygg-term-tests--value var env-p)))
          (should-not (string-match-p "/p/" (ygg-term-tests--value var env-q))))))))

(ert-deftest ygg-term-leaves-the-global-environment-alone ()
  (ygg-term-tests--with
    (let ((before (copy-sequence process-environment))
          (before-default (copy-sequence (default-value 'process-environment))))
      (let ((default-directory (ygg-term-tests--dir "p")))
        (ygg--ghostel-shell "*ygg-term:g*"))
      (should (equal process-environment before))
      (should (equal (default-value 'process-environment) before-default))
      (should (equal (getenv "CLAUDE_CONFIG_DIR") "/global/claude"))
      (should-not (getenv "CODEX_HOME")))))

(ert-deftest ygg-term-space-dir-sets-the-project-the-shell-is-in ()
  (ygg-term-tests--with
    (let ((p (ygg-term-tests--dir "p")) (q (ygg-term-tests--dir "q")))
      (let ((default-directory q))
        (ygg--ghostel-shell "*ygg-term:sp*" p))
      (dolist (var ygg-term-tests--homes)
        (should (string-match-p "/p/" (ygg-term-tests--value var (ygg-term-tests--last))))))))

(defconst ygg-term-tests--all-vars
  (append ygg-term-tests--homes ygg-term-tests--session-vars))

(defun ygg-term-tests--dirty-env ()
  (append (mapcar (lambda (v) (concat v "=stale")) ygg-term-tests--all-vars)
          process-environment))

(defun ygg-term-tests--unset-p (var env)
  (let ((hit (seq-find (lambda (e) (or (equal e var) (string-prefix-p (concat var "=") e))) env)))
    (or (null hit) (equal hit var))))

(ert-deftest ygg-term-remote-terminals-get-no-local-homes ()
  (ygg-term-tests--with
    (let ((remote "/ssh:box:/srv/app/"))
      (ygg--ghostel-shell "*ygg-term:r*" remote)
      (dolist (var ygg-term-tests--homes)
        (should (ygg-term-tests--unset-p var (ygg-term-tests--last))))
      (dolist (var (append ygg-term-tests--homes ygg-term-tests--session-vars))
        (should (member var (ygg-agent-terminal-env remote))))
      (should-not (seq-some (lambda (e) (string-search "=" e))
                            (ygg-agent-terminal-env remote)))
      (let ((default-directory remote))
        (should (member "CODEX_HOME" (ygg-agent-terminal-env)))))))

(ert-deftest ygg-term-contaminated-local-terminal-drops-session-vars ()
  (ygg-term-tests--with
    (let ((process-environment (ygg-term-tests--dirty-env))
          (default-directory (ygg-term-tests--dir "p")))
      (ygg--ghostel-shell "*ygg-term:c*"))
    (let ((env (ygg-term-tests--last)))
      (dolist (var ygg-term-tests--session-vars)
        (should (ygg-term-tests--unset-p var env)))
      (dolist (var ygg-term-tests--homes)
        (should (string-prefix-p ygg-agent-conf-root (ygg-term-tests--value var env)))))))

(ert-deftest ygg-term-contaminated-remote-terminal-drops-everything ()
  (ygg-term-tests--with
    (let ((process-environment (ygg-term-tests--dirty-env)))
      (ygg--ghostel-shell "*ygg-term:cr*" "/ssh:box:/srv/"))
    (dolist (var ygg-term-tests--all-vars)
      (should (ygg-term-tests--unset-p var (ygg-term-tests--last))))))

(ert-deftest ygg-term-shared-home-project-keeps-what-an-agent-there-keeps ()
  (ygg-term-tests--with
    (cl-letf (((symbol-function 'ygg-agent--logged-in-p)
               (lambda (_kind dir &rest _) (not (string-prefix-p ygg-agent-conf-root dir)))))
      (let ((default-directory (ygg-term-tests--dir "p")))
        (ygg--ghostel-shell "*ygg-term:sh*"))
      (dolist (var ygg-term-tests--homes)
        (should (equal (ygg-term-tests--value var (ygg-term-tests--last))
                       (and (equal var "CLAUDE_CONFIG_DIR") "/global/claude"))))
      (should-not (ygg-agent--known-config-env "claude" "claude" (ygg-term-tests--dir "p")))
      (should-not (seq-some (lambda (e) (string-search "=" e))
                            (seq-remove (lambda (e) (member e ygg-agent-session-env-vars))
                                        (ygg-agent-terminal-env (ygg-term-tests--dir "p"))))))))

(ert-deftest ygg-term-local-after-remote-gets-homes-again ()
  (ygg-term-tests--with
    (ygg--ghostel-shell "*ygg-term:r*" "/ssh:box:/srv/")
    (let ((default-directory (ygg-term-tests--dir "p")))
      (ygg--ghostel-shell "*ygg-term:l*"))
    (should (ygg-term-tests--value "CODEX_HOME" (ygg-term-tests--last)))))

(ert-deftest ygg-term-never-carries-per-session-variables ()
  (ygg-term-tests--with
    (let ((default-directory (ygg-term-tests--dir "p")))
      (ygg--ghostel-shell "*ygg-term:s*"))
    (let ((env (ygg-term-tests--last)))
      (dolist (var ygg-term-tests--session-vars)
        (should-not (ygg-term-tests--value var env)))
      (should-not (seq-some (lambda (e) (string-match-p "TOKEN\\|SIDECAR" e))
                            (seq-difference env (default-value 'process-environment)))))))

(ert-deftest ygg-term-ghostel-exec-path-gets-the-homes-too ()
  (ygg-term-tests--with
    (let ((default-directory (ygg-term-tests--dir "p")))
      (with-temp-buffer
        (ghostel--spawn-pty "vd" nil nil nil)))
    (should (ygg-term-tests--value "PI_CODING_AGENT_DIR" (ygg-term-tests--last)))))

(ert-deftest ygg-task-exec-hands-compile-the-project-homes ()
  (require 'layer-tasks)
  (ygg-term-tests--with
    (let (seen)
      (cl-letf (((symbol-function 'compile)
                 (lambda (&rest _) (setq seen (copy-sequence (default-value 'process-environment))))))
        (ygg-task--exec "true" (ygg-term-tests--dir "p"))
        (dolist (var ygg-term-tests--homes)
          (should (ygg-term-tests--value var seen)))
        (setq seen nil)
        (ygg-task--exec "true" "/ssh:box:/srv/")
        (dolist (var ygg-term-tests--homes)
          (should (ygg-term-tests--unset-p var seen)))))))

(ert-deftest ygg-task-exec-contaminated-drops-session-vars-and-remote-homes ()
  (require 'layer-tasks)
  (ygg-term-tests--with
    (let (seen)
      (cl-letf (((symbol-function 'compile)
                 (lambda (&rest _) (setq seen (copy-sequence (default-value 'process-environment))))))
        (let ((process-environment (ygg-term-tests--dirty-env)))
          (ygg-task--exec "true" (ygg-term-tests--dir "p"))
          (dolist (var ygg-term-tests--session-vars)
            (should (ygg-term-tests--unset-p var seen)))
          (setq seen nil)
          (ygg-task--exec "true" "/ssh:box:/srv/")
          (dolist (var ygg-term-tests--all-vars)
            (should (ygg-term-tests--unset-p var seen))))))))

(ert-deftest ygg-term-unset-entry-reaches-the-child ()
  (let ((process-environment (append '("YGG_T_GONE") (list "YGG_T_GONE=1" "YGG_T_KEEP=2")))
        (out ""))
    (let ((p (make-process :name "ygg-t" :buffer nil :noquery t
                           :command '("sh" "-c" "echo [$YGG_T_GONE][$YGG_T_KEEP]")
                           :filter (lambda (_ s) (setq out (concat out s))))))
      (while (process-live-p p) (accept-process-output p 0.1))
      (accept-process-output p 0.1))
    (should (equal (string-trim out) "[][2]"))))

(provide 'ygg-term-env-tests)
;;; ygg-term-env-tests.el ends here
