;;; ygg-project-setup.el --- install the CLIs a project's own files declare -*- lexical-binding: t; -*-

(require 'compile)
(require 'project)
(require 'ygg-project-commands)
(require 'ygg-agent-skills)
(require 'seq)
(require 'subr-x)

(defcustom ygg-project-setup-tools 'ask
  "Whether a new project gets the toolchain its manifests declare.
`ask' offers once per project, `always' installs, `never' does nothing."
  :type '(choice (const ask) (const always) (const never))
  :group 'yggdrasil)

(defcustom ygg-project-setup-never-file (locate-user-emacs-file "var/project-setup-never.eld")
  "Where the projects that declined the toolchain are remembered."
  :type 'file :group 'yggdrasil)

(defvar ygg-project-setup--seen (make-hash-table :test #'equal))

(defun ygg-project-setup--name (root)
  (file-name-nondirectory (directory-file-name root)))

(defun ygg-project-setup--first (root names)
  (seq-find (lambda (n) (file-exists-p (expand-file-name n root))) names))

(defun ygg-project-setup--output (&rest argv)
  (with-temp-buffer
    (and (zerop (apply #'call-process (car argv) nil t nil (cdr argv)))
         (buffer-string))))

(defun ygg-project-setup--mise (root)
  (when (ygg-project-setup--first root '("mise.toml" ".mise.toml" ".tool-versions"))
    (list "mise install" '("mise" "install")
          (lambda ()
            (string-empty-p
             (string-trim (or (ygg-project-setup--output "mise" "ls" "--missing") "x")))))))

(defun ygg-project-setup--brew (root)
  (when (ygg-project-setup--first root '("Brewfile"))
    (list "brew bundle" '("brew" "bundle" "--file=Brewfile")
          (lambda ()
            (ygg-project-setup--output "brew" "bundle" "check" "--file=Brewfile")))))

(defun ygg-project-setup--manager (root)
  (when-let* ((json (ygg-project-commands--json (expand-file-name "package.json" root)))
              (spec (alist-get 'packageManager json))
              (name (car (split-string spec "@"))))
    (list "corepack enable" '("corepack" "enable")
          (lambda () (executable-find name)))))

(defun ygg-project-setup--versions (root)
  (when-let* (((not (ygg-project-setup--first root '("mise.toml" ".mise.toml" ".tool-versions"))))
              (file (ygg-project-setup--first
                     root '(".nvmrc" ".node-version" ".python-version" ".ruby-version"))))
    (list (format "%s: covered by mise if you add it" file) nil nil)))

(defvar ygg-project-setup-detectors
  '(ygg-project-setup--mise ygg-project-setup--brew
    ygg-project-setup--manager ygg-project-setup--versions)
  "Functions of a root giving (LABEL ARGV CHECK), or nil.
CHECK returns non-nil when ARGV has nothing left to do; no ARGV is a note.")

(defun ygg-project-setup--plan (root)
  "The steps ROOT still needs and the notes about it, as (STEPS . NOTES)."
  (let ((default-directory root) steps notes)
    (dolist (detect ygg-project-setup-detectors)
      (when-let* ((step (funcall detect root)))
        (cond ((null (cadr step)) (push (car step) notes))
              ((not (executable-find (car (cadr step))))
               (push (format "%s: %s not found, skipped" (car step) (car (cadr step))) notes))
              ((not (funcall (nth 2 step))) (push step steps)))))
    (cons (nreverse steps) (nreverse notes))))

(defun ygg-project-setup--run (root steps)
  "Run STEPS one after another in ROOT's setup buffer."
  (let* ((name (ygg-project-setup--name root))
         (buf (get-buffer-create (format "*setup: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (unless (derived-mode-p 'compilation-mode) (compilation-mode))
      (setq default-directory root))
    (ygg-project-setup--next name buf root steps)))

(defun ygg-project-setup--next (name buf root steps)
  (if (null steps)
      (ygg-agent--notify (format "setup: %s ✓" name))
    (let ((default-directory root))
      (make-process
       :name (format "ygg-setup:%s" name)
       :buffer buf
       :command (cadr (car steps))
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (if (zerop (process-exit-status proc))
               (ygg-project-setup--next name buf root (cdr steps))
             (ygg-agent--notify
              (format "setup: %s failed at %s" name (car (car steps))) 'error)
             (display-buffer buf))))))))

(defun ygg-project-setup--never (&optional add)
  "The projects that declined, with ADD among them when given."
  (let ((roots (ignore-errors
                 (with-temp-buffer
                   (insert-file-contents ygg-project-setup-never-file)
                   (read (current-buffer))))))
    (when (and add (not (member add roots)))
      (push add roots)
      (make-directory (file-name-directory ygg-project-setup-never-file) t)
      (with-temp-file ygg-project-setup-never-file (prin1 roots (current-buffer))))
    roots))

(defun ygg-project-setup--ask (root steps)
  (let ((answer (completing-read
                 (format "Install toolchain for %s: %s? (y/n/never for this project) "
                         (ygg-project-setup--name root)
                         (mapconcat #'car steps ", "))
                 '("y" "n" "never") nil t)))
    (pcase answer
      ("y" (ygg-project-setup--run root steps))
      ("never" (ygg-project-setup--never root)))))

(defun ygg-project-setup--consider (root)
  (let ((steps (car (ygg-project-setup--plan root))))
    (when steps
      (if (eq ygg-project-setup-tools 'always)
          (ygg-project-setup--run root steps)
        (ygg-project-setup--ask root steps)))))

;;;###autoload
(defun ygg-project-setup-offer (root)
  "Offer ROOT its toolchain once per session, as `ygg-project-setup-tools' says."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (unless (or (eq ygg-project-setup-tools 'never)
                (file-remote-p root)
                (gethash root ygg-project-setup--seen)
                (member root (ygg-project-setup--never)))
      (puthash root t ygg-project-setup--seen)
      (run-with-idle-timer 0.5 nil #'ygg-project-setup--consider root))))

;;;###autoload
(defun ygg-project-setup (&optional root)
  "Install the toolchain ROOT's files declare: the project at point, or this one."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name
                 (or root
                     (get-text-property (line-beginning-position) 'ygg-project)
                     (when-let* ((pr (project-current nil))) (project-root pr))
                     (user-error "setup: not in a project")))))
         (plan (ygg-project-setup--plan root)))
    (dolist (note (cdr plan)) (ygg-agent--notify (format "setup: %s" note)))
    (if (car plan)
        (ygg-project-setup--run root (car plan))
      (ygg-agent--notify
       (format "setup: %s has nothing to install" (ygg-project-setup--name root))))))

(provide 'ygg-project-setup)
