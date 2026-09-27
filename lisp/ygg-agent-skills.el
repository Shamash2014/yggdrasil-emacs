;;; ygg-agent-skills.el --- the skills the agents get, lifted out of layer-agent -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'ygg-agent-conf)

(declare-function aob-acp--project "aob-acp")
(defvar aob-acp-start-dir)

(defun ygg-agent--notify (msg &optional level)
  (if (fboundp 'ygg-notify) (ygg-notify msg level) (message "%s" msg)))

(defun ygg-agent--project (&optional dir)
  "The checkout an agent belongs to."
  (let ((aob-acp-start-dir (or dir (and (boundp 'aob-acp-start-dir) aob-acp-start-dir))))
    (if (fboundp 'aob-acp--project) (aob-acp--project) default-directory)))

(defcustom ygg-agent-skills-root
  (let ((emacs-skills (expand-file-name "skills" user-emacs-directory))
        (nvim-skills (expand-file-name "~/.config/nvim/skills")))
    (cond ((file-directory-p emacs-skills) emacs-skills)
          ((file-directory-p nvim-skills) nvim-skills)
          (t emacs-skills)))
  "Directory of agent skills `ygg-agent-skill-install' installs."
  :type 'directory :group 'yggdrasil)

(defun ygg-agent--ensure-config-dirs ()
  "Bootstrap each configured agent's per-project config home (nvim ensure_project_dirs)."
  (let ((project (ygg-agent--project)))
    (dolist (kind-spec ygg-agent--config-homes)
      (ignore-errors
        (ygg-agent--config-dir (car kind-spec) (cdr kind-spec) project)))))

(define-derived-mode ygg-agent-skills-error-mode special-mode "skills-error"
  "Read-only output of a failed skills run.")

(defvar ygg-modal-special-modes)
(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-agent-skills-error-mode))

(defun ygg-agent--skills-error-buffer (name code out)
  (let ((buf (get-buffer-create (format "*skills:error:%s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "skills: %s failed (exit %d)\n\n" name code) out))
      (goto-char (point-min))
      (ygg-agent-skills-error-mode))
    (display-buffer buf '((display-buffer-at-bottom) (window-height . 15)))))

(defun ygg-agent--skills-run (argv name)
  "Run `npx -y skills ARGV' async; notify on done, log buffer on failure."
  (ygg-agent--ensure-config-dirs)
  (make-process
   :name (format "ygg-skills:%s" name)
   :buffer (generate-new-buffer " *ygg-skills*")
   :command (list shell-file-name "-lc" (concat "npx -y skills " argv))
   :noquery t
   :sentinel
   (lambda (proc _event)
     (when (memq (process-status proc) '(exit signal))
       (let ((code (process-exit-status proc))
             (out (with-current-buffer (process-buffer proc) (buffer-string))))
         (if (= code 0)
             (ygg-agent--notify (format "skills: %s ✓" name))
           (ygg-agent--notify (format "skills: %s failed (exit %d)" name code) 'error)
           (ygg-agent--skills-error-buffer name code out))
         (kill-buffer (process-buffer proc)))))))

(defun ygg-agent-skill-install ()
  "Install every skill under `ygg-agent-skills-root' into the agent CLIs."
  (interactive)
  (unless (file-directory-p ygg-agent-skills-root)
    (user-error "skills: dir not found: %s" ygg-agent-skills-root))
  (ygg-agent--notify (format "skills: installing from %s" ygg-agent-skills-root))
  (ygg-agent--skills-run
   (format "add %s -g --all"
           (shell-quote-argument (expand-file-name ygg-agent-skills-root)))
   "install"))

(defun ygg-agent--skill-names ()
  (when (file-directory-p ygg-agent-skills-root)
    (seq-filter
     (lambda (n) (and (not (string-prefix-p "." n))
                      (file-directory-p (expand-file-name n ygg-agent-skills-root))))
     (directory-files ygg-agent-skills-root))))

(defun ygg-agent-skill-uninstall (name)
  "Uninstall skill NAME from the agent CLIs."
  (interactive (list (completing-read "Uninstall skill: " (ygg-agent--skill-names))))
  (when (string-empty-p name) (user-error "skills: name required"))
  (ygg-agent--skills-run
   (format "remove -g -s %s -a '*' -y" (shell-quote-argument name))
   (concat "remove-" name)))

(defconst ygg-agent--shared-skills "~/.agents/skills"
  "Where the installer keeps each skill once, for every agent that reads skills.")

(defun ygg-agent--skills-stale ()
  "The config's skills an agent would not see as written: never installed,
or edited since.  The installer copies, so a change here reaches no one
until it runs again."
  (seq-filter
   (lambda (name)
     (let* ((dir (expand-file-name name ygg-agent-skills-root))
            (src (expand-file-name "SKILL.md" dir))
            (dst (expand-file-name (concat name "/SKILL.md") ygg-agent--shared-skills)))
       (and (file-readable-p src)
            (or (not (file-exists-p dst))
                ;; a skill's side files are read on demand, so they go stale too
                (seq-some (lambda (f) (file-newer-than-file-p f dst))
                          (directory-files-recursively dir ""))))))
   (ygg-agent--skill-names)))

(defun ygg-agent-skills-ensure ()
  "Install the config's skills for every agent when any is missing or stale."
  (interactive)
  (if-let* ((stale (ygg-agent--skills-stale)))
      (progn (ygg-agent--notify (format "skills: %d to install (%s)" (length stale)
                                        (string-join (seq-take stale 3) ", ")))
             (ygg-agent-skill-install))
    (when (called-interactively-p 'any)
      (ygg-agent--notify "skills: every agent has them"))))

(unless noninteractive
  (run-with-idle-timer 30 nil #'ygg-agent-skills-ensure))

;;; A project's own skills.  The config home cannot carry them: its
;;; skills entry is the shared link to the user's, and the CLI reads a
;;; project's from .claude/skills under the directory the session starts
;;; in and nowhere else.

(defun ygg-agent--skills-dir-p (dir)
  "Whether DIR holds skills rather than merely being named for them."
  (and (file-directory-p dir)
       (file-expand-wildcards (expand-file-name "*/SKILL.md" dir))
       t))

(defun ygg-agent-project-skills (root)
  "The directory holding ROOT's own skills, or nil.
A worktree carries none of its own, so the checkout it was cut from
answers for it."
  (when root
    (let* ((dir (expand-file-name root))
           (repo (or (ignore-errors (ygg-agent--repo-home dir)) dir)))
      (seq-find #'ygg-agent--skills-dir-p
                (list (expand-file-name ".claude/skills" dir)
                      (expand-file-name "skills" dir)
                      (expand-file-name ".claude/skills" repo)
                      (expand-file-name "skills" repo))))))

(defun ygg-agent--exclude-skills-link (root)
  "Keep the link out of ROOT's git status.
A worktree whose status is not clean is never reaped."
  (when-let* ((line "/.claude/skills")
              (common (ignore-errors
                        (car (process-lines "git" "-C" root "rev-parse"
                                            "--path-format=absolute"
                                            "--git-common-dir"))))
              ((not (string-empty-p common)))
              (file (expand-file-name "info/exclude" common)))
    (ignore-errors
      (make-directory (file-name-directory file) t)
      (unless (and (file-readable-p file)
                   (with-temp-buffer
                     (insert-file-contents file)
                     (re-search-forward
                      (concat "^" (regexp-quote line) "$") nil t)))
        (write-region (concat line "\n") nil file 'append 'silent)))))

(defun ygg-agent-link-project-skills (root)
  "Make ROOT's own skills reach the agents spawned in ROOT.
Returns a cons of what was done and the path: ready when they already
resolve there, kept when something else holds the place, linked or
copied when this put them there; nil when the project has none."
  (when-let* ((src (ygg-agent-project-skills root))
              (link (expand-file-name ".claude/skills" (expand-file-name root))))
    (cond
     ((equal (file-truename link) (file-truename src)) (cons 'ready link))
     ((or (file-symlink-p link) (file-exists-p link)) (cons 'kept link))
     (t (make-directory (file-name-directory link) t)
        (ygg-agent--exclude-skills-link root)
        (condition-case nil
            (progn (make-symbolic-link src link) (cons 'linked link))
          (error (copy-directory src link t t t) (cons 'copied link)))))))


(provide 'ygg-agent-skills)
;;; ygg-agent-skills.el ends here
