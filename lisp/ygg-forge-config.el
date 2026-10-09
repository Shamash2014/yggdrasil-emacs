;;; ygg-forge-config.el --- per-project gh and glab logins -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)
(require 'ygg-agent-conf)

(declare-function project-current "project")
(declare-function project-root "project")
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ygg--term-split-window "layer-terminal")
(declare-function ygg-call-with-buffer-env "layer-terminal" (thunk &optional extra-env))
(declare-function ygg-git-compare--glab-hosts "ygg-git-compare")

(defconst ygg-forge-config-kinds
  '(("gh" :var "GH_CONFIG_DIR" :login "hosts.yml")
    ("glab" :var "GLAB_CONFIG_DIR" :login "config.yml"))
  "Per forge CLI: the variable naming its config dir, and the file that holds a login.")

(defvar ygg-forge-config--repos (make-hash-table :test #'equal))

(defun ygg-forge-config--repo (project)
  (let ((key (expand-file-name project)))
    (or (gethash key ygg-forge-config--repos)
        (puthash key (ygg-agent--repo-home key) ygg-forge-config--repos))))

(defun ygg-forge-config-dir (kind project)
  "The config dir of KIND (\"gh\" or \"glab\") that PROJECT keeps, made or not."
  (when (and ygg-agent-conf-root project (not (file-remote-p project)))
    (ygg-agent--own-home kind (ygg-forge-config--repo project))))

(defun ygg-forge-config-login-file (kind project)
  "PROJECT's own login file for KIND, only when it exists."
  (when-let* ((dir (ygg-forge-config-dir kind project))
              (file (expand-file-name (plist-get (cdr (assoc kind ygg-forge-config-kinds)) :login)
                                      dir))
              ((file-readable-p file)))
    file))

(defun ygg-forge-config-env (project &optional kind)
  "\"VAR=DIR\" entries for each forge CLI whose login PROJECT keeps.
KIND narrows it to one CLI.  A CLI with no login of its own is left out, so
it keeps reading the global one."
  (delq nil
        (mapcar (lambda (spec)
                  (when (and (or (null kind) (equal kind (car spec)))
                             (ygg-forge-config-login-file (car spec) project))
                    (format "%s=%s" (plist-get (cdr spec) :var)
                            (directory-file-name (ygg-forge-config-dir (car spec) project)))))
                ygg-forge-config-kinds)))

(defun ygg-forge-config-program-env (program project)
  "Entries to bind for running PROGRAM in PROJECT, nil unless it is gh or glab."
  (when (assoc program ygg-forge-config-kinds)
    (ygg-forge-config-env project program)))

(defun ygg-agent-acp-environment (agent project _dir &optional _isolate)
  "Env entries for an aob connection of AGENT in PROJECT.
An isolated connection shares the project's home: a home per worker is a
worker that has never authenticated."
  (append (when-let* ((env (ygg-agent--known-config-env agent agent project)))
            (list env))
          (ygg-forge-config-env project)))

(defun ygg-forge-config--run-terminal (root argv env)
  "Run ARGV in a terminal sitting in ROOT, with ENV in its environment."
  (unless (require 'ghostel nil t)
    (user-error "ghostel is not installed"))
  (let* ((default-directory root)
         (buffer (generate-new-buffer (format "*forge-login: %s*"
                                              (file-name-nondirectory
                                               (directory-file-name root)))))
         window)
    (condition-case err
        (progn
          (setq window (ygg--term-split-window))
          (set-window-buffer window buffer)
          (select-window window)
          (ygg-call-with-buffer-env
           (lambda () (ghostel-exec buffer (car argv) (cdr argv)))
           env))
      (error (when (and (window-live-p window) (not (frame-root-window-p window)))
               (delete-window window))
             (kill-buffer buffer)
             (user-error "Forge login terminal failed: %s" (error-message-string err))))))

;;;###autoload
(defun ygg-project-forge-login ()
  "Log gh or glab in for this project alone, typing the token in a terminal."
  (interactive)
  (let* ((root (expand-file-name
                (or (when-let* ((pr (project-current nil))) (project-root pr))
                    default-directory)))
         (kind (completing-read "Forge CLI: " (mapcar #'car ygg-forge-config-kinds) nil t))
         (host (when (equal kind "glab")
                 (completing-read "GitLab host: "
                                  (when (fboundp 'ygg-git-compare--glab-hosts)
                                    (let ((default-directory root))
                                      (delete-dups (mapcar #'car (ygg-git-compare--glab-hosts)))))
                                  nil nil)))
         (dir (or (ygg-forge-config-dir kind root)
                  (user-error "No agent config root, or a remote project")))
         (var (plist-get (cdr (assoc kind ygg-forge-config-kinds)) :var))
         (argv (append (list kind "auth" "login")
                       (when (and host (not (string-empty-p host)))
                         (list "--hostname" host)))))
    (make-directory dir t)
    (ygg-forge-config--run-terminal
     root argv (list (format "%s=%s" var (directory-file-name dir))))))

(provide 'ygg-forge-config)
;;; ygg-forge-config.el ends here
