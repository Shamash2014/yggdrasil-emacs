;;; ygg-project-scan.el --- every repo on disk, not just the ones you opened -*- lexical-binding: t; -*-

;; `project-known-project-roots' remembers what you have visited, which is
;; the wrong list to pick from: the repo you have not opened yet is exactly
;; the one you are looking for.  A bounded find over the folders you keep
;; work in answers that, and stays fast by pruning at the repository it
;; finds rather than walking into it.

;;; Code:

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

(defun ygg-project-roots (&optional refresh)
  "Every repository worth offering: the ones found, and the ones remembered.
Remembered roots come first — you opened them, so you meant them — and a
project outside every search path is still reachable through them."
  (when (or refresh (null ygg-project-scan--found))
    (setq ygg-project-scan--found (ygg-project-scan--walk)))
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
    (ygg-project-roots t)
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
    (ygg-project-roots t)
    (message "ygg: forgot %s" (abbreviate-file-name dir))
    dir))

;;;###autoload
(defun ygg-project-rescan ()
  "Look for repositories again, after adding or moving one."
  (interactive)
  (message "ygg: %d repositories" (length (ygg-project-roots t))))

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
