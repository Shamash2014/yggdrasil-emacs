;;; ygg-project-scan.el --- every repo on disk, not just the ones you opened -*- lexical-binding: t; -*-

;; `project-known-project-roots' remembers what you have visited, which is
;; the wrong list to pick from: the repo you have not opened yet is exactly
;; the one you are looking for.  A bounded find over the folders you keep
;; work in answers that, and stays fast by pruning at the repository it
;; finds rather than walking into it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'project)

(defgroup ygg-project nil
  "Finding repositories rather than remembering them."
  :group 'convenience)

(defcustom ygg-project-search-paths '("~/temp/" "~/src/" "~/projects/")
  "Folders scanned for repositories.  A missing one is skipped."
  :type '(repeat directory) :group 'ygg-project)

(defcustom ygg-project-search-depth 4
  "How deep below a search path a repository is still found.
Counts the `.git' itself, so 4 reaches a repo three folders down."
  :type 'natnum :group 'ygg-project)

(defcustom ygg-project-ignored nil
  "Roots never offered, however often the scan finds them again.
Forgetting a remembered project is enough to drop it; one the scan
turns up on its own would come straight back, so it is named here."
  :type '(repeat directory) :group 'yggdrasil)

(defcustom ygg-project-dirs nil
  "Extra folders belonging to a project, as (ROOT . DIRECTORIES).
A project is not always one checkout: an agent asked to work on one
usually needs the others in view.  Every folder listed for a root is
handed to that project's agents as an additional directory."
  :type '(alist :key-type directory :value-type (repeat directory))
  :group 'yggdrasil)

(defcustom ygg-project-scan-cache-file
  (locate-user-emacs-file "var/project-scan.eld")
  "Where the list of repositories found on disk is kept between runs."
  :type 'file :group 'yggdrasil)

(defcustom ygg-project-scan-ttl 86400
  "Seconds a saved scan is trusted before the tree is walked again."
  :type 'natnum :group 'yggdrasil)

(defvar ygg-project-scan--found nil
  "Cached scan, since the set of repositories on disk moves slowly.")

(declare-function project-known-project-roots "project" ())

(defun ygg-project-scan--walk-1 (dir)
  "Repositories under DIR, by their `.git', which may be a file or a folder."
  (with-temp-buffer
    ;; find exits non-zero on any unreadable folder; the hits it did print
    ;; are still good, so the status is deliberately ignored
    (call-process "find" nil t nil
                  dir "-maxdepth" (number-to-string ygg-project-search-depth)
                  "-name" ".git" "-prune")
    (goto-char (point-min))
    (let (roots)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string-empty-p line)
            (push (file-name-directory line) roots)))
        (forward-line 1))
      (nreverse roots))))

(defun ygg-project-scan--walk ()
  (seq-mapcat (lambda (path)
                (let ((dir (directory-file-name (expand-file-name path))))
                  (and (file-directory-p dir) (ygg-project-scan--walk-1 dir))))
              ygg-project-search-paths))

(defun ygg-project-scan--save (roots)
  "Keep ROOTS for the next run."
  (ignore-errors
    (make-directory (file-name-directory ygg-project-scan-cache-file) t)
    (with-temp-file ygg-project-scan-cache-file
      (prin1 (list :when (float-time) :paths ygg-project-search-paths
                   :depth ygg-project-search-depth :roots roots)
             (current-buffer))))
  roots)

(defun ygg-project-scan--load ()
  "The saved scan, when it is still worth believing."
  (ignore-errors
    (when (file-readable-p ygg-project-scan-cache-file)
      (let ((saved (with-temp-buffer
                     (insert-file-contents ygg-project-scan-cache-file)
                     (read (current-buffer)))))
        ;; a scan of other folders, or to another depth, answers a
        ;; different question
        (when (and (equal (plist-get saved :paths) ygg-project-search-paths)
                   (equal (plist-get saved :depth) ygg-project-search-depth)
                   (< (- (float-time) (or (plist-get saved :when) 0))
                      ygg-project-scan-ttl))
          (plist-get saved :roots))))))

(defun ygg-project-scan-async (&optional callback)
  "Walk the tree without holding Emacs up; CALLBACK gets the roots.
Half a second of `find' is not much until it is the half second before
the sidebar appears."
  (let ((left (length ygg-project-search-paths))
        (acc nil))
    (if (zerop left)
        (when callback (funcall callback nil))
      (dolist (path ygg-project-search-paths)
        (let* ((dir (directory-file-name (expand-file-name path)))
               (buf (generate-new-buffer " *ygg-scan*")))
          (if (not (file-directory-p dir))
              (progn (kill-buffer buf)
                     (when (zerop (cl-decf left))
                       (setq ygg-project-scan--found
                             (ygg-project-scan--save (nreverse acc)))
                       (when callback (funcall callback ygg-project-scan--found))))
            (make-process
             :name "ygg-scan" :buffer buf :noquery t
             :command (list "find" dir "-maxdepth"
                            (number-to-string ygg-project-search-depth)
                            "-name" ".git" "-prune")
             :sentinel
             (lambda (proc _e)
               (when (memq (process-status proc) '(exit signal))
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (goto-char (point-min))
                     (while (not (eobp))
                       (let ((line (buffer-substring-no-properties
                                    (line-beginning-position) (line-end-position))))
                         (unless (string-empty-p line)
                           (push (file-name-directory line) acc)))
                       (forward-line 1)))
                   (kill-buffer buf))
                 (when (zerop (cl-decf left))
                   (setq ygg-project-scan--found
                         (ygg-project-scan--save (nreverse acc)))
                   (when callback
                     (funcall callback ygg-project-scan--found))))))))))))

(defun ygg-project-roots (&optional refresh)
  "Every repository worth offering: the ones found, and the ones remembered.
Remembered roots come first — you opened them, so you meant them — and a
project outside every search path is still reachable through them."
  (when (or refresh (null ygg-project-scan--found))
    (setq ygg-project-scan--found
          (or (and (not refresh) (ygg-project-scan--load))
              (ygg-project-scan--save (ygg-project-scan--walk)))))
  (let ((known (and (fboundp 'project-known-project-roots)
                    (project-known-project-roots))))
    ;; the list file keeps `~/...' and find prints absolute paths, so the
    ;; two spellings of one project only collapse once both are expanded
    (let ((ignored (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
                           ygg-project-ignored)))
      (seq-remove
       (lambda (d) (member d ignored))
       (delete-dups
        (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
                (append known ygg-project-scan--found)))))))

;;; Taking a project in — skills, config, layout, commands

(defvar ygg-project-import-hook nil
  "Run with no arguments whenever an import moves, so a view can redraw.")

(defvar ygg-project-import--state (make-hash-table :test 'equal)
  "Root to the step its import is on, while one is running.")

(defconst ygg-project-import--frames ["◐" "◓" "◑" "◒"])
(defvar ygg-project-import--tick 0)
(defvar ygg-project-import--timer nil)

(defun ygg-project-importing (root)
  "What ROOT's import is doing now, or nil when nothing is."
  (gethash (ygg-project--key root) ygg-project-import--state))

(defun ygg-project-import-mark (root)
  "ROOT's spinner and step while it is being taken in, else nil."
  (when-let* ((step (ygg-project-importing root)))
    (format "%s %s"
            (aref ygg-project-import--frames
                  (mod ygg-project-import--tick
                       (length ygg-project-import--frames)))
            step)))

(defun ygg-project-import--turn ()
  (setq ygg-project-import--tick (1+ ygg-project-import--tick))
  (if (zerop (hash-table-count ygg-project-import--state))
      (when ygg-project-import--timer
        (cancel-timer ygg-project-import--timer)
        (setq ygg-project-import--timer nil))
    (run-hooks 'ygg-project-import-hook)))

(defun ygg-project-import--mark (root step)
  (puthash root step ygg-project-import--state)
  (unless ygg-project-import--timer
    (setq ygg-project-import--timer
          (run-at-time 0 0.15 #'ygg-project-import--turn)))
  (run-hooks 'ygg-project-import-hook))

(defun ygg-project-import--done (root callback)
  (remhash root ygg-project-import--state)
  (run-hooks 'ygg-project-import-hook)
  (message "ygg: %s is ready" (abbreviate-file-name root))
  (when callback (funcall callback root)))

(defun ygg-project-import--run (root steps callback)
  (if (null steps)
      ;; the command scan is asynchronous already, so it ends the import
      (progn
        (ygg-project-import--mark root "commands")
        (if (fboundp 'ygg-project-commands-refresh)
            (ygg-project-commands-refresh
             root (lambda (&rest _) (ygg-project-import--done root callback)))
          (ygg-project-import--done root callback)))
    (ygg-project-import--mark root (caar steps))
    ;; a step at a time off the timer: each one is short, and between
    ;; them the sidebar draws what is happening rather than freezing
    (run-at-time 0.05 nil
                 (lambda ()
                   (ignore-errors (funcall (cdar steps)))
                   (ygg-project-import--run root (cdr steps) callback)))))

;;;###autoload
(defun ygg-project-import (root &optional callback)
  "Take ROOT in: its skills, the config its agents answer under, what
it is laid out as and what it can run.  CALLBACK is called with ROOT
when the last of it settles.  Nothing here blocks."
  (interactive (list (completing-read "Import project: "
                                      (mapcar #'abbreviate-file-name
                                              (ygg-project-roots))
                                      nil t)))
  (let ((root (ygg-project--key root)))
    (ygg-project-import--run
     root
     (list (cons "skills"
                 (lambda ()
                   (when (fboundp 'ygg-agent-link-project-skills)
                     (ygg-agent-link-project-skills root))))
           (cons "config"
                 (lambda ()
                   (when (and (fboundp 'ygg-agent--config-env)
                              (boundp 'aob-acp-default-agent))
                     (ygg-agent--config-env aob-acp-default-agent
                                            aob-acp-default-agent root))))
           (cons "layout"
                 (lambda ()
                   (when (fboundp 'ygg-project-workspaces)
                     (ygg-project-workspaces root)))))
     callback)))

;;;###autoload
(defun ygg-project-add (dir)
  "Remember DIR as a project worth offering."
  (interactive "DProject: ")
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (pr (project-current nil dir)))
    (unless (file-directory-p dir)
      (user-error "ygg: %s is not a directory" (abbreviate-file-name dir)))
    (setq ygg-project-ignored
          (seq-remove (lambda (d)
                        (equal dir (file-name-as-directory (expand-file-name d))))
                      ygg-project-ignored))
    (unless pr
      (user-error "ygg: %s is not a repository" (abbreviate-file-name dir)))
    (project-remember-project pr)
    (customize-save-variable 'ygg-project-ignored ygg-project-ignored)
    ;; no walk: a remembered root is already one of the roots, and the
    ;; scan that finds the rest is not what you are waiting for
    (ygg-project-import dir)
    (message "ygg: remembered %s" (abbreviate-file-name dir))
    dir))

;;;###autoload
(defun ygg-project-remove (dir)
  "Stop offering DIR, whether it was remembered or found."
  (interactive
   (list (completing-read "Forget project: "
                          (mapcar #'abbreviate-file-name (ygg-project-roots))
                          nil t)))
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (when (fboundp 'project-forget-project)
      (ignore-errors (project-forget-project dir)))
    (add-to-list 'ygg-project-ignored dir)
    (customize-save-variable 'ygg-project-ignored ygg-project-ignored)
    (remhash dir ygg-project-import--state)
    (run-hooks 'ygg-project-import-hook)
    (message "ygg: forgot %s" (abbreviate-file-name dir))
    dir))

;;;###autoload
(defun ygg-project-rescan ()
  "Look for repositories again, after adding or moving one."
  (interactive)
  (ygg-project-scan-async
   (lambda (roots)
     (message "ygg: %d repositories" (length roots)))))

(defun ygg-project--key (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun ygg-project-folders (root)
  "The extra folders ROOT carries, those that still exist."
  (seq-filter #'file-directory-p
              (mapcar #'expand-file-name
                      (cdr (assoc (ygg-project--key root) ygg-project-dirs)))))

;;;###autoload
(defun ygg-project-add-folder (root dir)
  "Add DIR to ROOT's folders, so ROOT's agents can see it too."
  (interactive
   (let ((root (completing-read "Project: "
                                (mapcar #'abbreviate-file-name (ygg-project-roots))
                                nil t (abbreviate-file-name
                                       (or (ignore-errors
                                             (project-root (project-current nil)))
                                           default-directory)))))
     (list root (read-directory-name "Folder to add: "))))
  (let* ((key (ygg-project--key root))
         (dir (ygg-project--key dir))
         (cell (assoc key ygg-project-dirs)))
    (unless (file-directory-p dir)
      (user-error "ygg: %s is not a directory" (abbreviate-file-name dir)))
    (if cell
        (setcdr cell (delete-dups (append (cdr cell) (list dir))))
      (push (cons key (list dir)) ygg-project-dirs))
    (customize-save-variable 'ygg-project-dirs ygg-project-dirs)
    (message "ygg: %s also covers %s" (abbreviate-file-name key)
             (abbreviate-file-name dir))
    dir))

;;;###autoload
(defun ygg-project-remove-folder (root dir)
  "Take DIR back out of ROOT's folders."
  (interactive
   (let* ((root (completing-read "Project: "
                                 (mapcar #'abbreviate-file-name (ygg-project-roots))
                                 nil t (abbreviate-file-name
                                        (or (ignore-errors
                                              (project-root (project-current nil)))
                                            default-directory))))
          (dirs (ygg-project-folders root)))
     (unless dirs (user-error "ygg: that project has no extra folders"))
     (list root (completing-read "Folder to drop: "
                                 (mapcar #'abbreviate-file-name dirs) nil t))))
  (let* ((key (ygg-project--key root))
         (dir (ygg-project--key dir))
         (cell (assoc key ygg-project-dirs)))
    (when cell
      (setcdr cell (seq-remove (lambda (d) (equal dir (ygg-project--key d)))
                               (cdr cell)))
      (unless (cdr cell)
        (setq ygg-project-dirs (delq cell ygg-project-dirs)))
      (customize-save-variable 'ygg-project-dirs ygg-project-dirs))
    (message "ygg: %s no longer covers %s" (abbreviate-file-name key)
             (abbreviate-file-name dir))
    dir))

(provide 'ygg-project-scan)
;;; ygg-project-scan.el ends here
