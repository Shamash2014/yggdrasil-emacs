;;; ygg-forge-config.el --- per-project gh and glab logins -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)
(require 'ygg-agent-conf)

(declare-function project-current "project")
(declare-function project-root "project")
(declare-function ygg-project-roots "ygg-project-scan")
(defvar ygg-project-dirs)
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
         (buffer (generate-new-buffer (format "*forge-login: %s %s*" (car argv)
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

(defun ygg-forge-config--containing (dir candidates)
  "The longest of CANDIDATES, as (ROOT . PATH), that holds DIR."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        (best-len 0) best)
    (unless (file-remote-p dir)
      (setq dir (file-name-as-directory (file-truename dir)))
      (dolist (c candidates)
        (let ((path (file-name-as-directory (expand-file-name (cdr c)))))
          (unless (file-remote-p path)
            (let ((real (file-name-as-directory (file-truename path))))
              (when (and (string-prefix-p real dir) (> (length real) best-len))
                (setq best-len (length real)
                      best (cons (car c) path))))))))
    best))

(defun ygg-forge-config--project-root ()
  "The project root for the buffer at hand, never prompting when one can be found."
  (expand-file-name
   (or (get-text-property (line-beginning-position) 'ygg-project)
       (let ((roots (and (fboundp 'ygg-project-roots) (ygg-project-roots))))
         (car (ygg-forge-config--containing
               default-directory
               (append (mapcar (lambda (r) (cons r r)) roots)
                       (mapcan (lambda (cell)
                                 (mapcar (lambda (d) (cons (car cell) d)) (cdr cell)))
                               (bound-and-true-p ygg-project-dirs))))))
       (ignore-errors
         (when-let* ((pr (project-current nil))) (project-root pr)))
       (locate-dominating-file default-directory ".git")
       (let ((roots (and (fboundp 'ygg-project-roots) (ygg-project-roots))))
         (if roots
             (completing-read "Project: " roots nil t)
           (user-error "No projects known"))))))

(defun ygg-forge-config-login (root kind &optional host)
  "Open a terminal in ROOT that logs KIND in under ROOT's own config dir.
HOST, when non-empty, is passed as --hostname."
  (let* ((dir (or (ygg-forge-config-dir kind root)
                  (user-error "No agent config root, or a remote project")))
         (var (plist-get (cdr (assoc kind ygg-forge-config-kinds)) :var))
         (argv (append (list kind "auth" "login")
                       (when (and host (not (string-empty-p host)))
                         (list "--hostname" host)))))
    (make-directory dir t)
    (ygg-forge-config--run-terminal
     root argv (list (format "%s=%s" var (directory-file-name dir))))))

(defun ygg-forge-config-import-step (root kind)
  "Log KIND in for ROOT unless it already has a login; nil when it does not apply."
  (when (and ygg-agent-conf-root
             (not (file-remote-p root))
             (not (ygg-forge-config-login-file kind root)))
    (ygg-forge-config-login
     root kind
     (when (and (equal kind "glab") (fboundp 'ygg-git-compare--forge-repo))
       (let* ((default-directory root)
              (repo (ignore-errors (ygg-git-compare--forge-repo))))
         (and (eq (car-safe repo) 'gitlab) (cadr repo)))))))

;;;###autoload
(defun ygg-project-forge-login ()
  "Log gh or glab in for this project alone, typing the token in a terminal."
  (interactive)
  (let* ((root (ygg-forge-config--project-root))
         (kind (completing-read "Forge CLI: " (mapcar #'car ygg-forge-config-kinds) nil t))
         (host (when (equal kind "glab")
                 (completing-read "GitLab host: "
                                  (when (fboundp 'ygg-git-compare--glab-hosts)
                                    (let ((default-directory root))
                                      (delete-dups (mapcar #'car (ygg-git-compare--glab-hosts)))))
                                  nil nil))))
    (ygg-forge-config-login root kind host)))

(declare-function dired "dired")

;;;###autoload
(defun ygg-project-config-init (project)
  "Make PROJECT's own config folder for every agent kind and forge CLI.
A kind whose marker file names a home is left to that home.  Returns the
folders made or ensured; nothing here signals."
  (interactive (list (ygg-forge-config--project-root)))
  (when (and ygg-agent-conf-root project (not (file-remote-p project)))
    (let* ((repo (ignore-errors (ygg-forge-config--repo project)))
           (made nil))
      (when repo
        (pcase-dolist (`(,kind . ,spec) ygg-agent--config-homes)
          (let ((marker (plist-get spec :marker)))
            (unless (or (ignore-errors (ygg-agent--read-marker (expand-file-name marker project)))
                        (ignore-errors (ygg-agent--read-marker (expand-file-name marker repo))))
              (condition-case nil
                  (let ((dir (ygg-agent--own-home kind repo)))
                    (ygg-agent--bootstrap-share spec dir t)
                    (push dir made))
                (error nil)))))
        (pcase-dolist (`(,kind . ,_) ygg-forge-config-kinds)
          (condition-case nil
              (let ((dir (ygg-forge-config-dir kind project)))
                (make-directory dir t)
                (push dir made))
            (error nil))))
      (clrhash ygg-agent--config-dirs)
      (nreverse made))))

;;;###autoload
(defun ygg-project-config-open ()
  "Open this project's agent config folder in dired, making it if absent."
  (interactive)
  (let* ((root (ygg-forge-config--project-root))
         (own (or (ygg-forge-config-dir "gh" root)
                  (user-error "No agent config root, or a remote project")))
         (dir (file-name-directory own)))
    (make-directory dir t)
    (ygg-project-config-init root)
    (dired dir)))

(provide 'ygg-forge-config)
;;; ygg-forge-config.el ends here
