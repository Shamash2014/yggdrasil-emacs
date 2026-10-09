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
(require 'ygg-project-setup)

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
(declare-function ygg-projects--scan-worktrees "ygg-projects" ())
(declare-function ygg-projects--scan-docker "ygg-projects" ())
(declare-function ygg-projects-forget-root "ygg-projects" (root))
(declare-function aob-transcript-forget "aob-transcript" ())
(declare-function aob-transcript-found "aob-transcript" (project &optional agent))
(declare-function ygg-ice-import-step "ygg-ice" (root))

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

(defun ygg-project-roots (&optional _refresh)
  "The projects you took in, newest way round the way `project' keeps them.
What a walk of the disk turned up is not a project until you import
it: a list nobody chose is a list nobody can keep, and removing a row
from it only lasts until the next scan finds the folder again."
  (delete-dups
   (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
           (and (fboundp 'project-known-project-roots)
                (project-known-project-roots)))))

(defvar project--list)
(declare-function project--write-project-list "project" ())

(defun ygg-project--forget (dir)
  "Take DIR out of `project\='s own list, however that list spells it.
It stores what each caller handed it — one abbreviates the home
folder, another expands it — and forgets by string equality, so a
project remembered as ~/x is still there after forgetting /home/me/x."
  (dolist (spelling (delete-dups
                     (list dir (directory-file-name dir)
                           (abbreviate-file-name dir)
                           (abbreviate-file-name (directory-file-name dir)))))
    (when (fboundp 'project-forget-project)
      (ignore-errors (project-forget-project spelling))))
  ;; and whatever spelling neither of those was
  (when (and (boundp 'project--list) (listp project--list))
    (let ((rest (seq-remove (lambda (entry)
                              (equal dir (ygg-project--key (car entry))))
                            project--list)))
      (unless (equal rest project--list)
        (setq project--list rest)
        (when (fboundp 'project--write-project-list)
          (ignore-errors (project--write-project-list)))))))

(defvar ygg-project--children (make-hash-table :test 'equal)
  "Umbrella root to the repositories found under it.")

(defvar ygg-project--importing nil
  "Non-nil while an import is putting a project on the list.")

(defun ygg-project--only-on-import (fn &rest args)
  "Let only an import add to the project list.
Visiting a file, switching to a folder or restoring a session all ask
`project\=' about where they are, and it writes down every answer — so
the list fills itself with everything you have ever opened, and a row
you removed comes back the next time anything looks at that folder."
  (when ygg-project--importing (apply fn args)))

(with-eval-after-load 'project
  (advice-add 'project-remember-project :around #'ygg-project--only-on-import))

(defun ygg-project-candidates (&optional refresh)
  "Repositories found on disk that have not been imported.
The walk is a source of suggestions for the import, and nothing else."
  (when (or refresh (null ygg-project-scan--found))
    (setq ygg-project-scan--found
          (or (and (not refresh) (ygg-project-scan--load))
              (ygg-project-scan--save (ygg-project-scan--walk)))))
  (let ((taken (ygg-project-roots)))
    (seq-remove
     (lambda (d) (member d taken))
     (delete-dups
      (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
              ygg-project-scan--found)))))

;;; Taking a project in — skills, config, layout, commands

(defvar ygg-project-import-hook nil
  "Run with no arguments whenever an import moves, so a view can redraw.")

(defvar ygg-project-import--state (make-hash-table :test 'equal)
  "Root to the step its import is on, while one is running.")

(defconst ygg-project-import--frames ["▘" "▝" "▗" "▖"]
  "A spinner the mono font draws itself: a fallback glyph is a\ndifferent face at a different width, in a panel made of columns.")
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
(defcustom ygg-project-import-extras nil
  "Extras an import runs without being picked: skills installs and
refreshes the config's skills for every agent, gh and glab open a login
terminal for the project unless it has one, ice wires the project for
ICE or refreshes its wiring."
  :type '(set (const "skills") (const "gh") (const "glab") (const "ice")) :group 'ygg)

(defconst ygg-project-import--extra-names '("skills" "gh" "glab" "ice"))

(defun ygg-project-import--read-extras ()
  (let ((picked (completing-read-multiple
                 "Extras (skills, gh, glab, ice; RET for none): "
                 ygg-project-import--extra-names nil t
                 (and ygg-project-import-extras
                      (string-join ygg-project-import-extras ",")))))
    (seq-intersection picked ygg-project-import--extra-names)))

;;;###autoload
(defun ygg-project-import (root &optional callback extras)
  "Take ROOT in: its project skills, the config its agents answer under,
what it is laid out as and what it can run.  EXTRAS, picked when called
interactively, add the config's skills for every agent (skills) and ICE
wiring (ice), a gh or glab login terminal (gh, glab).  CALLBACK is
called with ROOT when the last of it settles.
Nothing here blocks."
  (interactive (list (completing-read "Import project: "
                                      (mapcar #'abbreviate-file-name
                                              (ygg-project-roots))
                                      nil t)
                     nil
                     (ygg-project-import--read-extras)))
  (let ((root (ygg-project--key root)))
    ;; an import is also a re-import: whatever was cached about this
    ;; project is what the import is being run to replace
    (when (fboundp 'aob-transcript-forget) (aob-transcript-forget))
    (when (fboundp 'ygg-projects-forget-root) (ygg-projects-forget-root root))
    (ygg-project-import--run
     root
     (append
      (list (cons "skills"
                  (lambda ()
                    ;; the project's own skills, where its sessions start
                    (when (fboundp 'ygg-agent-link-project-skills)
                      (ygg-agent-link-project-skills root))))
           (cons "config"
                 (lambda ()
                   (when (and (fboundp 'ygg-agent--config-env)
                              (boundp 'aob-acp-default-agent))
                     (ygg-agent--config-env aob-acp-default-agent
                                            aob-acp-default-agent root))))
           (cons "config folders"
                 (lambda ()
                   (when (fboundp 'ygg-project-config-init)
                     (ygg-project-config-init root))))
           (cons "layout"
                 (lambda ()
                   (when (fboundp 'ygg-project-workspaces)
                     (ygg-project-workspaces root))))
           (cons "worktrees"
                 (lambda ()
                   (when (fboundp 'ygg-projects--scan-worktrees)
                     (ygg-projects--scan-worktrees))))
           (cons "containers"
                 (lambda ()
                   (when (fboundp 'ygg-projects--scan-docker)
                     (ygg-projects--scan-docker))))
           (cons "sessions"
                 (lambda ()
                   (when (fboundp 'aob-transcript-found)
                     (aob-transcript-found root))))
           )
      (and (member "skills" extras)
           (list (cons "agent skills"
                       (lambda ()
                         (when (fboundp 'ygg-agent-skills-ensure)
                           (ygg-agent-skills-ensure))))))
      (mapcan (lambda (kind)
                (when (member kind extras)
                  (list (cons (concat kind " login")
                              (lambda ()
                                (when (fboundp 'ygg-forge-config-import-step)
                                  (ygg-forge-config-import-step root kind)))))))
              '("gh" "glab"))
      (and (member "ice" extras)
           (list (cons "ice"
                       (lambda ()
                         (when (fboundp 'ygg-ice-import-step)
                           (ygg-ice-import-step root)))))))
     callback)))

;;;###autoload
(defun ygg-project-add (dir)
  "Remember DIR as a project worth offering."
  (interactive "DProject: ")
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (pr (project-current nil dir)))
    (unless (file-directory-p dir)
      (user-error "ygg: %s is not a directory" (abbreviate-file-name dir)))
    (if (and pr (not (eq (car-safe pr) 'ygg-umbrella)))
        (progn
          (let ((ygg-project--importing t)) (project-remember-project pr))
          ;; no walk: an imported root is already one of the roots, and the
          ;; scan that suggests the others is not what you are waiting for
          (ygg-project-import dir))
      (let ((children (ygg-project--repos-under dir)))
        (unless children
          (user-error "ygg: %s is not a repository" (abbreviate-file-name dir)))
        (remhash dir ygg-project--children)
        (let ((ygg-project--importing t))
          (project-remember-project (cons 'ygg-umbrella dir))
          (dolist (child children)
            (project-remember-project (project-current nil child))))
        (ygg-project-import dir (ygg-project--import-each children))))
    (message "ygg: remembered %s" (abbreviate-file-name dir))
    (ygg-project-setup-offer dir)
    dir))

(defun ygg-project--import-each (roots)
  "A callback importing ROOTS one after another, so an umbrella of
twenty repos does not start twenty imports at once."
  (lambda (&rest _)
    (when roots
      (ygg-project-import (car roots)
                          (ygg-project--import-each (cdr roots))))))

;;;###autoload
(defun ygg-project-remove (dir)
  "Drop DIR from the projects you took in.
Nothing is remembered about it: the folder is still on disk and the
scan will offer it again next time you import one."
  (interactive
   (list (completing-read "Remove project: "
                          (mapcar #'abbreviate-file-name (ygg-project-roots))
                          nil t)))
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (ygg-project--forget dir)
    (remhash dir ygg-project--children)
    (remhash dir ygg-project-import--state)
    (run-hooks 'ygg-project-import-hook)
    (message "ygg: removed %s" (abbreviate-file-name dir))
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

;;; An umbrella — a plain folder holding repositories

(defun ygg-project--worktree-main (root)
  "The main checkout ROOT is a linked worktree of, as a true name, else nil."
  (let ((git (expand-file-name ".git" root)))
    (when (file-regular-p git)
      (with-temp-buffer
        (insert-file-contents git)
        (when (re-search-forward "^gitdir: \\(.*\\)/\\.git/worktrees/[^/\n]+/?$" nil t)
          (ygg-project--key (file-truename (expand-file-name (match-string 1) root))))))))

(defun ygg-project--repos-under (dir)
  "The repositories under DIR, stopping at the outermost of each nest,
so a submodule stays part of the repository that carries it.  A linked
worktree of one of them is that one's, not a repository of its own."
  (let* ((roots (mapcar #'ygg-project--key
                        (ygg-project-scan--walk-1 (directory-file-name dir))))
         (true (mapcar #'file-truename roots)))
    (seq-remove (lambda (r)
                  (or (seq-some (lambda (o) (and (not (equal o r))
                                                 (string-prefix-p o r)))
                                roots)
                      (member (ygg-project--worktree-main r) true)))
                roots)))

(defun ygg-project-umbrella-p (root)
  "Non-nil when ROOT is a project on the list with no `.git' of its own.
Such a folder only got on the list as an umbrella, so being on it and
not being a repository is what registers one; removing the project
unregisters it."
  (let ((key (ygg-project--key root)))
    (and (not (file-exists-p (expand-file-name ".git" key)))
         (member key (ygg-project-roots)))))

(defcustom ygg-project-order nil
  "The order an umbrella's repositories are listed in, as (ROOT . CHILDREN).
A repository not named here yet goes after the ones that are."
  :type '(alist :key-type directory :value-type (repeat directory))
  :group 'yggdrasil)

(defun ygg-project-children (root)
  "The repositories directly under the umbrella ROOT, nil for any other
project, in the order `ygg-project-order' gives them.  Found once per
session and kept, since a walk is a `find'."
  (let ((key (ygg-project--key root)))
    ;; asked on every redraw: never of a remote root, and never of a
    ;; folder that is gone, where an empty walk is not remembered
    (when (and (not (file-remote-p key)) (file-directory-p key)
               (ygg-project-umbrella-p key))
      (let ((found (with-memoization (gethash key ygg-project--children)
                     (ygg-project--repos-under key)))
            (order (mapcar #'ygg-project--key
                           (cdr (assoc key ygg-project-order)))))
        (append (seq-filter (lambda (c) (member c found)) order)
                (seq-remove (lambda (c) (member c order)) found))))))

(defun ygg-project-umbrellas ()
  "The umbrellas on the project list; a remote root is never asked."
  (seq-filter (lambda (r) (and (not (file-remote-p r))
                               (not (file-exists-p (expand-file-name ".git" r)))))
              (ygg-project-roots)))

(defun ygg-project-umbrella-of (root)
  "The umbrella on the project list ROOT is a repository of, else nil."
  (let ((key (ygg-project--key root)))
    (seq-find (lambda (u) (member key (ygg-project-children u)))
              (ygg-project-umbrellas))))

(defun ygg-project-top-roots ()
  "The projects on the list, less the repositories of an umbrella on it."
  (let ((nested (seq-mapcat #'ygg-project-children (ygg-project-umbrellas))))
    (seq-remove (lambda (r) (member r nested)) (ygg-project-roots))))

(defun ygg-project--moved (items item n)
  "ITEMS with ITEM moved N places later, earlier when N is negative."
  (let* ((rest (remove item items))
         (at (max 0 (min (length rest) (+ n (seq-position items item))))))
    (append (seq-take rest at) (list item) (seq-drop rest at))))

(defun ygg-project-move-child (child n)
  "Move CHILD N places down among its umbrella's repositories, and keep it."
  (let* ((child (ygg-project--key child))
         (umbrella (or (ygg-project-umbrella-of child)
                       (user-error "ygg: %s is in no umbrella"
                                   (abbreviate-file-name child)))))
    (setf (alist-get umbrella ygg-project-order nil nil #'equal)
          (ygg-project--moved (ygg-project-children umbrella) child n))
    (customize-save-variable 'ygg-project-order ygg-project-order)))

(defun ygg-project-move (root n)
  "Move ROOT N places down the project list, and keep it.
The list is `project\='s own, so its file holds the order; an umbrella's
repositories are not counted, being listed under it."
  (let* ((root (ygg-project--key root))
         (top (ygg-project-top-roots)))
    (unless (member root top)
      (user-error "ygg: %s is not on the list" (abbreviate-file-name root)))
    (let ((order (ygg-project--moved top root n)))
      (setq project--list
            (append (seq-mapcat (lambda (k)
                                  (seq-filter (lambda (e) (equal k (ygg-project--key (car e))))
                                              project--list))
                                order)
                    (seq-remove (lambda (e) (member (ygg-project--key (car e)) order))
                                project--list)))
      (when (fboundp 'project--write-project-list)
        (project--write-project-list)))))

(defun ygg-project-try-umbrella (dir)
  "The umbrella project DIR is in, for `project-find-functions'.
It sits after `project-try-vc', so it answers only where no repository
does: inside a child, the child is the project.  The project is
\(ygg-umbrella . ROOT); `project-root' is ROOT and `project-files' is
the umbrella's loose files plus each child's own list, so finding a
file reaches every child and still honours its ignores."
  (let ((dir (ygg-project--key dir)))
    (when-let* ((root (seq-find (lambda (r) (and (string-prefix-p r dir)
                                                 (ygg-project-umbrella-p r)))
                                (sort (ygg-project-roots)
                                      (lambda (a b) (> (length a) (length b)))))))
      (cons 'ygg-umbrella root))))

(cl-defmethod project-root ((project (head ygg-umbrella)))
  (cdr project))

(cl-defmethod project-ignores ((project (head ygg-umbrella)) dir)
  (append (cl-call-next-method)
          (and (equal (ygg-project--key dir) (cdr project))
               (mapcar (lambda (child)
                         (concat "./" (file-relative-name child (cdr project))))
                       (ygg-project-children (cdr project))))))

(cl-defmethod project-files ((project (head ygg-umbrella)) &optional dirs)
  (let ((project-files-relative-names nil))
    (append (cl-call-next-method)
            (and (null dirs)
                 (mapcan (lambda (child)
                           (when-let* ((pr (project-try-vc child)))
                             (project-files pr)))
                         (ygg-project-children (cdr project)))))))

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'ygg-project-try-umbrella 90))

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
