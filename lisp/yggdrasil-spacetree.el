;;; yggdrasil-spacetree.el --- nestable tab-bar workspaces -*- lexical-binding: t; -*-

;; A from-scratch port of the Neovim space-tree: nestable, unscoped
;; workspaces where a space IS a native tab-bar tab (window configuration).
;; The tree is layered over the flat tab list by tagging each tab's alist
;; with `ygg-id'/`ygg-parent' — the tabs themselves are the source of truth,
;; so there is no parallel state to desync and no leaf-address resolution.
;; tab-bar-select-tab natively saves/restores each space's window-state.

;;; Code:

(require 'tab-bar)
(require 'subr-x)
(require 'cl-lib)
(require 'ygg-git)
(require 'ygg-ui)

(defconst ygg-space--root-id 0
  "Phantom root: parent of every top-level space, so siblings always share
a parent even at the top level.  No real tab ever carries this id (ids
start at 1), so it exists only as a parent tag, never as a switchable tab.")

(defvar ygg-space--id 0 "Monotonic space-id counter for the session.")
(defvar ygg-space--spawning nil "Non-nil while we create a tab, to mute adopt.")
(defvar ygg-space--closing nil "Non-nil while we close a subtree, to mute reconcile.")

(defun ygg-space--new-id ()
  (setq ygg-space--id (1+ ygg-space--id)))

(defvar ygg-space--format-tick 0
  "Counter bumped whenever anything the tab bar is drawn from changes.")

(defvar ygg-space--format-cache nil
  "Alist of frame -> (KEY . ITEMS): the bar each frame was last given.")

(defun ygg-space--format-invalidate (&rest _)
  "Say that the tab bar is out of date, so the next redisplay rebuilds it."
  (setq ygg-space--format-tick (1+ ygg-space--format-tick)))

(defun ygg-space--tabs ()
  (funcall tab-bar-tabs-function))

(defun ygg-space--current ()
  "The live alist of the current tab (its car is `current-tab')."
  (assq 'current-tab (ygg-space--tabs)))

(defun ygg-space--set (tab key val)
  "Mutate TAB's alist in place: KEY -> VAL (TAB is a live tabs-list cons)."
  (let ((cell (assq key tab)))
    (if cell (setcdr cell val)
      (setcdr tab (cons (cons key val) (cdr tab)))))
  (ygg-space--format-invalidate))

(defun ygg-space--id-of (tab) (alist-get 'ygg-id tab))
(defun ygg-space--parent-of (tab) (alist-get 'ygg-parent tab))

;;; The folder a space is for
;;
;; It lives on the tab.  Buffers come and go, the one you are looking at
;; may belong to another repo, the window showing it may close, and a
;; restored session brings tabs back with their parameters — so the tab
;; is the only place a space's folder cannot be lost from.

(defun ygg-space--dir-of (tab) (alist-get 'ygg-dir tab))

(defun ygg-space--pin (tab dir)
  "Pin TAB to DIR, and return it, when DIR is a directory that exists."
  (when-let* ((dir (and dir (file-name-as-directory (expand-file-name dir))))
              ((file-directory-p dir)))
    (ygg-space--set tab 'ygg-dir dir)
    dir))

(defun ygg-space--tab-buffers (tab)
  (if (eq tab (ygg-space--current))
      (mapcar #'window-buffer (window-list))
    (alist-get 'wc-bl tab)))

(defun ygg-space-root (dir)
  "DIR's repository root, or DIR itself when it is not inside one.
A space is for a tree, not for the folder you happened to name: pin it a
level down and its agents, its terminal and its name all speak for a
subdirectory instead of the project."
  (when dir
    (let ((dir (file-name-as-directory (expand-file-name dir))))
      (or (locate-dominating-file dir ".git") dir))))

(defun ygg-space-task-p (tab)
  "Non-nil when TAB was opened for a task rather than named for a tree.
Two spaces can stand on one tree; the one that is simply the tree is the
one that tree's work belongs to, and this is what tells them apart."
  (and (alist-get 'ygg-task tab) t))

(defun ygg-space--infer-dir (tab)
  "Where TAB is evidently working, for a space that was never pinned.
Its task's repository first — a task space is for that tree whatever is
on screen — then the repository of the first file it actually holds."
  (or (when-let* ((key (alist-get 'ygg-task tab))
                  ((file-directory-p key)))
        (ygg-space-root key))
      (when-let* ((file (seq-some (lambda (b)
                                    (and (buffer-live-p b)
                                         (buffer-file-name b)))
                                  (ygg-space--tab-buffers tab))))
        (ygg-space-root (file-name-directory file)))))

(defun ygg-space-dir (&optional tab)
  "The folder TAB — the current space by default — is for.
A space made before it was pinned, or restored pointing at a tree that
has since moved, is healed from what it holds and pinned again on the
spot, so this answers for every space that holds anything at all."
  (when-let* ((tab (or tab (ygg-space--current))))
    (let ((pinned (ygg-space--dir-of tab)))
      (or (and pinned (file-directory-p pinned) pinned)
          (ygg-space--pin tab (ygg-space--infer-dir tab))))))

;;;###autoload
(defun ygg-space-cd (dir &optional stay)
  "Move this space to DIR: the folder its new work starts in.
The space goes there rather than merely remembering it — the folder
opens, and a space still carrying a generated name takes the folder's,
because a space named for one tree and working in another is the thing
this is meant to stop.  With a prefix argument, STAY where you are and
only re-pin.

Nothing already open is disturbed: buffers keep their own directories,
and only what you start next follows the space."
  (interactive
   (list (read-directory-name "Space folder: " (ygg-space-dir) nil t)
         current-prefix-arg))
  (ygg-space--ensure-root)
  (let* ((tab (ygg-space--current))
         (old (ygg-space--dir-of tab))
         (base (lambda (d) (and d (file-name-nondirectory
                                   (directory-file-name d))))))
    (unless (ygg-space--pin tab dir)
      (user-error "space: %s is not a directory" dir))
    (let ((new (ygg-space-dir tab)))
      ;; a name you chose is yours; a generated one, or one that was just
      ;; the last folder's, follows the space to where it now works
      (when (or (not (alist-get 'explicit-name tab))
                (equal (alist-get 'name tab) (funcall base old)))
        (ygg-space--set tab 'name (funcall base new))
        (ygg-space--set tab 'explicit-name t))
      ;; a task space answers for its task's tree; re-pointing it elsewhere
      ;; would leave the tag lying about where the work is
      (when-let* ((key (alist-get 'ygg-task tab))
                  ((not (string-prefix-p (expand-file-name new)
                                         (expand-file-name key)))))
        (ygg-space--set tab 'ygg-task nil)
        (ygg-space--echo "untagged: this space no longer sits over its task"))
      (unless (or stay (file-remote-p new))
        (dired new))
      (ygg-space--echo "⌂ %s → %s" (ygg-space--name tab)
                       (abbreviate-file-name new)))))

(defun ygg-space--name (tab)
  (or (and (alist-get 'explicit-name tab) (alist-get 'name tab))
      (format "#%s" (ygg-space--id-of tab))))

(defun ygg-space--tab-by-id (id)
  (seq-find (lambda (tb) (eql id (ygg-space--id-of tb))) (ygg-space--tabs)))

(defun ygg-space--index-of-id (id)
  (seq-position (ygg-space--tabs) id
                (lambda (tb x) (eql x (ygg-space--id-of tb)))))

(defun ygg-space--children-of (id)
  "Tabs whose parent is ID, in flat tab-list order.
A tab with no id parents nothing: an unadopted tab has neither id nor
parent, and would otherwise come back as its own child."
  (when id
    (seq-filter (lambda (tb) (eql id (ygg-space--parent-of tb))) (ygg-space--tabs))))

(defun ygg-space--current-id ()
  (ygg-space--id-of (ygg-space--current)))

(defun ygg-space--goto-id (id)
  (when-let* ((idx (ygg-space--index-of-id id)))
    (tab-bar-select-tab (1+ idx))
    t))

(defvar ygg-space--switching nil
  "Non-nil only while a space switch restores a tab's window-state.")

(defun ygg-space--allow-side-split (orig window &rest args)
  "While switching, split an existing side window the way
`window--make-side-window' does, so a saved layout that carries one
rebuilds instead of erroring; every other split is left untouched."
  (if (and ygg-space--switching window (window-parameter window 'window-side))
      (let ((window-combination-resize 'side)) (apply orig window args))
    (apply orig window args)))

(defun ygg-space--safe-select (orig &rest args)
  "Switch without aborting on a side-window split error.
Session save bakes open side windows (sidebar, quickfix, agent traces)
into a tab's window-state; restoring one makes `split-window' refuse the
side window.  `ygg-space--allow-side-split' permits exactly those splits
for the length of the switch."
  (let ((ygg-space--switching t))
    (with-demoted-errors "ygg-space switch: %S"
      (apply orig args))))

(defun ygg-space--echo (fmt &rest args)
  (message "space: %s" (apply #'format fmt args)))

;;; Adoption + creation

(defun ygg-space--adopt-current (parent)
  "Tag the current tab as a space with PARENT (an id or nil), fresh id.
A tab adopted rather than made — the one Emacs starts in — gets its
folder now, so no space exists without one."
  (let ((tab (ygg-space--current)))
    (ygg-space--set tab 'ygg-id (ygg-space--new-id))
    (ygg-space--set tab 'ygg-parent parent)
    (unless (ygg-space--dir-of tab)
      (ygg-space--pin tab default-directory))
    tab))

(defun ygg-space--ensure-root ()
  "Make sure the current tab is a tagged space; adopt as a top-level one."
  (unless (ygg-space--id-of (ygg-space--current))
    (ygg-space--adopt-current ygg-space--root-id)))

(defun ygg-space--spawn (parent)
  "Create a new tab as a child of PARENT (id) and tag it; land on it.
The folder goes on the tab and nowhere else: a new space lands on the
buffer you were already in, and writing a directory into that buffer
would move somebody else's file out from under them."
  (let* ((ygg-space--spawning t)
         (cwd default-directory)
         (tab (progn (tab-bar-new-tab)
                     (ygg-space--adopt-current parent))))
    (ygg-space--pin tab cwd)
    tab))

(defun ygg-space-new-tab-buffer ()
  "What a new space opens on: its folder, listed.
A space that opens on whatever you were last reading starts its work in
that buffer's directory — which is how a space ends up living somewhere
it was never meant to."
  ;; never over a remote path: `file-directory-p' alone can sit on a dead
  ;; connection for the whole timeout, and this runs inside tab creation
  (or (and (not (file-remote-p default-directory))
           (file-directory-p default-directory)
           (ignore-errors (dired-noselect default-directory)))
      (get-buffer-create "*scratch*")))

(setq tab-bar-new-tab-choice #'ygg-space-new-tab-buffer)

(defmacro ygg-space--in-dir (&rest body)
  "Run BODY with the current space's folder as `default-directory'.
A space made from a space inherits where that one works, not where the
buffer under point happens to live."
  (declare (indent 0))
  `(let ((default-directory (or (ygg-space-dir) default-directory)))
     ,@body))

;;;###autoload
(defun ygg-space-child ()
  "Create a child space of the current one and switch to it."
  (interactive)
  (ygg-space--ensure-root)
  (let ((tab (ygg-space--in-dir (ygg-space--spawn (ygg-space--current-id)))))
    (ygg-space--echo "＋ child %s" (ygg-space--name tab))))

;;;###autoload
(defun ygg-space-sibling ()
  "Create a sibling of the current space (a child of its parent) and switch."
  (interactive)
  (ygg-space--ensure-root)
  (let* ((cur (ygg-space--current))
         (parent (or (ygg-space--parent-of cur) ygg-space--root-id))
         (tab (ygg-space--in-dir (ygg-space--spawn parent))))
    (ygg-space--echo "＋ sibling %s" (ygg-space--name tab))))

;;;###autoload
(defun ygg-space-clone ()
  "Duplicate the current space (layout + buffers) into a new sibling."
  (interactive)
  (ygg-space--ensure-root)
  (let* ((state (window-state-get (frame-root-window) t))
         (cur (ygg-space--current))
         (parent (or (ygg-space--parent-of cur) ygg-space--root-id))
         (from (ygg-space--name cur))
         (tab (ygg-space--in-dir (ygg-space--spawn parent))))
    (window-state-put state (frame-root-window))
    (ygg-space--echo "⧉ cloned %s → %s" from (ygg-space--name tab))))

(declare-function ygg-project-roots "ygg-project-scan" (&optional refresh))

(defun ygg-space--for-dir (dir)
  "The space already pinned to DIR, or nil.
Compared through `file-truename' so a symlinked checkout and the path
you picked it by are one folder, not two spaces for the same work.  A
space pinned on a host is passed over rather than resolved, since
resolving it would sit on that connection to answer about a folder
here."
  (let ((want (file-truename (directory-file-name (expand-file-name dir)))))
    (seq-find (lambda (tab)
                (when-let* ((d (ygg-space--dir-of tab))
                            ((not (file-remote-p d))))
                  (equal want (file-truename (directory-file-name
                                              (expand-file-name d))))))
              (ygg-space--tabs))))

;;; The place a space is opened on
;;
;; One verb takes every way of writing a place: a folder here, a folder
;; over there.  Where it lands is decided before anything is touched, so
;; a remote place costs no connection until you have actually gone to it.

(defconst ygg-space--tramp-name-re
  "\\`/\\([a-zA-Z][a-zA-Z0-9-]*\\):\\(?:\\([^/:@|]+\\)@\\)?\\([^/:@|]*\\):\\(.*\\)\\'"
  "A TRAMP name written out: /METHOD:[USER@]HOST:PATH.")

(defconst ygg-space--uri-re
  "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://\\(.*\\)\\'"
  "A place with a scheme and an authority: SCHEME://[USERINFO@]HOST/PATH.")

(defconst ygg-space--scp-re
  "\\`\\(?:\\([^/:@]+\\)@\\)?\\([^/:@]+\\):\\(/.*\\)\\'"
  "The scp short form, which means ssh: [USER@]HOST:/PATH.")

(defvar tramp-methods)

(defun ygg-space--uri-path (path)
  "PATH with its trailing slashes settled to one, and never empty."
  (let ((path (string-trim-right (or path "") "/+")))
    (if (string-empty-p path) "/" (concat path "/"))))

(defun ygg-space--method-known-p (method)
  "Non-nil when TRAMP has METHOD, or when TRAMP is not loaded to say."
  (if (and (boundp 'tramp-methods) tramp-methods)
      (and (assoc method tramp-methods) t)
    t))

(defun ygg-space--local-place (path)
  (let ((dir (file-name-as-directory
              (expand-file-name (ygg-space--uri-path path)))))
    (list :dir dir :method nil :user nil :host nil :path dir)))

(defun ygg-space--remote-place (method user host path)
  (unless (ygg-space--method-known-p method)
    (user-error "space: no TRAMP method named %s" method))
  (let ((path (ygg-space--uri-path path)))
    (list :dir (concat "/" method ":" (if user (concat user "@") "") host
                       ":" path)
          :method method :user user :host host :path path)))

(defun ygg-space-uri-parse (uri)
  "Read URI as the place a space can stand on.
Takes a local path, a place written SCHEME://[USER@]HOST/PATH, the scp
short form HOST:/PATH, and a TRAMP name as it is.  Returns a plist of
:dir, the directory to stand on, and the :method, :user, :host and
:path it was read from; a local place has no method, user or host, and
no scheme means local.  A scheme TRAMP has no method for is refused."
  (let ((uri (string-trim (or uri ""))))
    (cond
     ((string-empty-p uri) (user-error "space: no place given"))
     ((string-match ygg-space--tramp-name-re uri)
      (ygg-space--remote-place (match-string 1 uri) (match-string 2 uri)
                               (match-string 3 uri) (match-string 4 uri)))
     ((string-match ygg-space--uri-re uri)
      (let* ((scheme (match-string 1 uri))
             (rest (match-string 2 uri))
             (slash (string-match-p "/" rest))
             (authority (if slash (substring rest 0 slash) rest))
             (path (if slash (substring rest slash) ""))
             (at (string-match-p "@" authority))
             (user (and at (substring authority 0 at)))
             (host (if at (substring authority (1+ at)) authority)))
        (if (equal scheme "file")
            (ygg-space--local-place path)
          (ygg-space--remote-place scheme user host path))))
     ((string-match ygg-space--scp-re uri)
      (ygg-space--remote-place "ssh" (match-string 1 uri)
                               (match-string 2 uri) (match-string 3 uri)))
     (t (ygg-space--local-place uri)))))

(defvar ygg--space-buffers)

(defun ygg-space--tab-for-dir (dir)
  "The space already standing on DIR, or nil.
A remote DIR is matched by its name alone, since resolving it would
open the very connection this is asked ahead of."
  (if (file-remote-p dir)
      (let ((want (directory-file-name dir)))
        (seq-find (lambda (tab)
                    (when-let* ((d (ygg-space--dir-of tab)))
                      (equal want (directory-file-name d))))
                  (ygg-space--tabs)))
    (ygg-space--for-dir dir)))

(defun ygg-space--empty-p (tab)
  "Non-nil when TAB stands on nothing and holds nothing of its own.
A scratch buffer is what an untouched space holds, so it does not count
as work that would be pushed aside."
  (and (null (ygg-space--dir-of tab))
       (not (seq-find (lambda (buf)
                        (and (buffer-live-p buf)
                             (not (equal (buffer-name buf) "*scratch*"))))
                      (and (boundp 'ygg--space-buffers)
                           (gethash (ygg-space--id-of tab)
                                    ygg--space-buffers))))))

(defun ygg-space--landing (dir)
  "Where opening DIR lands: (goto . TAB), (here . TAB) or (new).
The space already on DIR wins, because you meant the work you left;
then this space when it stands on nothing, because an empty space is
where you already are; otherwise a space is made."
  (let ((tab (ygg-space--tab-for-dir dir))
        (cur (ygg-space--current)))
    (cond (tab (cons 'goto tab))
          ((and cur (ygg-space--empty-p cur)) (cons 'here cur))
          (t (list 'new)))))

(defun ygg-space--name-for-host (dir host)
  "Name this space for HOST and the last part of DIR, for a remote place.
A remote space named for its folder alone would read as the local one."
  (when host
    (let* ((tab (ygg-space--current))
           (base (file-name-nondirectory (directory-file-name dir))))
      (ygg-space--set tab 'name
                      (format "%s:%s" host
                              (if (string-empty-p base) "/" base)))
      (ygg-space--set tab 'explicit-name t))))

(defvar ygg-space-uri-history nil
  "Places opened as spaces, most recently opened first.")

(defvar savehist-additional-variables)

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'ygg-space-uri-history))

;;;###autoload
(defun ygg-space-open (uri)
  "Go to the space standing on URI, opening one for it when none is.
URI is a folder here, or a place over there written
SCHEME://[USER@]HOST/PATH, HOST:/PATH, or as a TRAMP name; no scheme
means a local folder.  Naming the same place twice returns you to the
work you left rather than starting a second space over it.  Candidates
are the projects you have opened and the places you have opened before,
and anything you type is taken as written."
  (interactive
   (list (let ((roots (mapcar #'abbreviate-file-name
                              (and (fboundp 'ygg-project-roots)
                                   (ygg-project-roots)))))
           (completing-read "Space for place: "
                            (append roots ygg-space-uri-history) nil nil))))
  (ygg-space--ensure-root)
  (let* ((place (ygg-space-uri-parse uri))
         (host (plist-get place :host))
         (remote (plist-get place :method))
         (dir (if remote (plist-get place :dir)
                (ygg-space-root (plist-get place :dir)))))
    (setq ygg-space-uri-history (cons uri (delete uri ygg-space-uri-history)))
    (pcase (ygg-space--landing dir)
      (`(goto . ,tab)
       (ygg-space--goto-id (ygg-space--id-of tab))
       (ygg-space--echo "⌂ %s" (ygg-space--name tab)))
      (`(here . ,_)
       (ygg-space-cd dir)
       (ygg-space--name-for-host dir host))
      (_
       (if (not remote)
           (ygg-space-new-on dir)
         ;; a remote place is spawned on as it was written: the repo-root
         ;; walk would open the connection and climb it directory by directory
         (let ((default-directory dir))
           (ygg-space--spawn ygg-space--root-id))
         (ygg-space-cd dir)
         (ygg-space--name-for-host dir host))))
    (when remote (dired dir))
    dir))

(define-obsolete-function-alias 'ygg-space-open-folder #'ygg-space-open "2026-09")

;;;###autoload
(defun ygg-space-new-on (dir)
  "Open a new top-level space on DIR and land in it.
Top level, not a child of wherever you were: a separate folder is
separate work."
  (interactive (list (read-directory-name "New space in: " nil nil t)))
  (ygg-space--ensure-root)
  (let ((dir (ygg-space-root dir)))
    (let ((default-directory dir))
      (ygg-space--spawn ygg-space--root-id))
    (ygg-space-cd dir)))

;;; A worktree is a space

(defcustom ygg-space-worktree-directory "~/.aob/worktrees"
  "Where the worktrees made for spaces live, keyed by repository."
  :type 'directory :group 'yggdrasil)

(defvar ygg-ex--commands)

(defun ygg-space--repo (&optional dir)
  "The repository this space works in, or nil when there is none."
  (let* ((dir (or dir (ygg-space-dir) default-directory))
         (root (and dir (locate-dominating-file
                         (file-name-as-directory (expand-file-name dir))
                         ".git"))))
    (and root (directory-file-name (expand-file-name root)))))

(defun ygg-space--worktree-key (root)
  "The twelve characters that stand for ROOT among worktree folders."
  (substring (sha1 (directory-file-name (expand-file-name root))) 0 12))

(defun ygg-space--worktree-path (root name)
  "Where the worktree called NAME for the repository ROOT belongs."
  (expand-file-name
   name (expand-file-name (ygg-space--worktree-key root)
                          (expand-file-name ygg-space-worktree-directory))))

(defun ygg-space--worktrees (root)
  "ROOT's worktrees as a list of (PATH . BRANCH), the checkout first.
Read from the porcelain listing, whose first entry is the repository's
own checkout however the verb was reached."
  (let ((found nil))
    (dolist (block (split-string
                    (ygg-git root "worktree" "list" "--porcelain")
                    "\n\n" t))
      (let ((path nil) (branch nil))
        (dolist (line (split-string block "\n" t))
          (cond ((string-prefix-p "worktree " line)
                 (setq path (substring line 9)))
                ((string-prefix-p "branch " line)
                 (setq branch (replace-regexp-in-string
                               "\\`refs/heads/" "" (substring line 7))))))
        (when path (push (cons path branch) found))))
    (nreverse found)))

(defun ygg-space--worktree-space (path root)
  "Open PATH as a space named for it and ROOT, and pinned to it.
The branch alone would read as the repository it is a branch of, so the
repository's name follows it."
  (ygg-space-open path)
  (when-let* ((tab (ygg-space--current)))
    (ygg-space--pin tab path)
    (ygg-space--set tab 'name
                    (format "%s:%s"
                            (file-name-nondirectory
                             (directory-file-name path))
                            (file-name-nondirectory
                             (directory-file-name root))))
    (ygg-space--set tab 'explicit-name t))
  path)

(defun ygg-space--worktree-new (root name base)
  "Make ROOT's worktree for the branch NAME from BASE and open it.
A branch that does not exist yet is started at BASE, which defaults to
the branch the repository is on; one that exists is checked out as it
stands."
  (when (or (null name) (string-empty-p name))
    (user-error "worktree new: name a branch"))
  (let ((path (ygg-space--worktree-path root name))
        (known (not (string-empty-p
                     (string-trim (ygg-git root "branch"
                                                  "--list" name))))))
    (make-directory (file-name-directory path) t)
    (if known
        (ygg-git root "worktree" "add" path name)
      (let ((base (or base (string-trim (ygg-git root "branch"
                                                        "--show-current")))))
        (ygg-git root "worktree" "add" "-b" name path base)))
    (ygg-space--worktree-space path root)))

(defun ygg-space--worktree-remove (root)
  "Remove one of ROOT's worktrees, after asking, and close its space.
The repository's own checkout is not a worktree you may take away."
  (let* ((all (ygg-space--worktrees root))
         (main (caar all))
         (path (let ((paths (mapcar #'car all)))
                 (completing-read "Remove worktree: " paths nil t))))
    (when (equal (directory-file-name (expand-file-name path))
                 (directory-file-name (expand-file-name main)))
      (user-error "worktree remove: %s is the checkout itself" path))
    (when (y-or-n-p (format "Remove worktree %s? "
                            (abbreviate-file-name path)))
      (when-let* ((tab (ygg-space--for-dir path)))
        (ygg-space--goto-id (ygg-space--id-of tab))
        (ygg-space-close))
      (ygg-git root "worktree" "remove" path)
      (ygg-space--echo "✕ worktree %s" (abbreviate-file-name path))
      path)))

;;;###autoload
(defun ygg-space-worktree (line)
  "Work the worktrees of this space's repository, one verb for all three.
LINE begins with the subcommand: new NAME [BASE] makes the worktree for
the branch NAME and opens it as its own space, open picks one that
exists and goes to it, remove picks one and takes it away with its
space.  No key: this is the colon line and M-x."
  (interactive
   (list (completing-read "worktree: " '("new" "open" "remove") nil nil)))
  (let* ((parts (split-string (or line "") nil t))
         (root (ygg-space--repo)))
    (unless root (user-error "worktree: no repository here"))
    (pcase (car parts)
      ("new" (ygg-space--worktree-new root (nth 1 parts) (nth 2 parts)))
      ("open" (let ((paths (mapcar #'car (ygg-space--worktrees root))))
                (unless paths (user-error "worktree: none listed"))
                (ygg-space--worktree-space
                 (completing-read "Worktree: " paths nil t) root)))
      ("remove" (ygg-space--worktree-remove root))
      (_ (user-error "usage: worktree new NAME [BASE] | open | remove")))))

(defun ygg-ex--cmd-worktree (_range _bang args)
  "Work a worktree: ARGS names the subcommand and what it needs."
  (ygg-space-worktree args))

(with-eval-after-load 'yggdrasil-ex
  (setf (alist-get "worktree" ygg-ex--commands nil nil #'equal)
        'ygg-ex--cmd-worktree))

;;; Navigation

;;;###autoload
(defun ygg-space-up ()
  "Switch to the parent space itself."
  (interactive)
  (let ((parent (ygg-space--parent-of (ygg-space--current))))
    (if (and parent (ygg-space--goto-id parent))
        (ygg-space--echo "▲ %s" (ygg-space--name (ygg-space--tab-by-id parent)))
      (ygg-space--echo "at root"))))

;;;###autoload
(defun ygg-space-down ()
  "Switch to the first child of the current space."
  (interactive)
  (let ((first (car (ygg-space--children-of (ygg-space--current-id)))))
    (if (and first (ygg-space--goto-id (ygg-space--id-of first)))
        (ygg-space--echo "▼ %s" (ygg-space--name first))
      (ygg-space--echo "no children"))))

(defun ygg-space--sibling-step (delta)
  (let* ((cur (ygg-space--current))
         (parent (ygg-space--parent-of cur)))
    (if (not parent)
        (ygg-space--echo "root has no siblings")
      (let* ((sibs (ygg-space--children-of parent))
             (pos (seq-position sibs (ygg-space--id-of cur)
                                (lambda (tb x) (eql x (ygg-space--id-of tb)))))
             (target (and pos (nth (+ pos delta) sibs))))
        (when (and target (ygg-space--goto-id (ygg-space--id-of target)))
          (ygg-space--echo "%s %s" (if (> delta 0) "▶" "◀")
                           (ygg-space--name target)))))))

;;;###autoload
(defun ygg-space-next-sibling ()
  "Switch to the next sibling space."
  (interactive) (ygg-space--sibling-step 1))

;;;###autoload
(defun ygg-space-prev-sibling ()
  "Switch to the previous sibling space."
  (interactive) (ygg-space--sibling-step -1))

;;;###autoload
(defun ygg-space-toggle ()
  "Ping-pong to the most recently used space."
  (interactive)
  (if (> (length (ygg-space--tabs)) 1)
      (tab-bar-switch-to-recent-tab)
    (ygg-space--echo "no previous space")))

;;;###autoload
(defun ygg-space-goto (&optional n)
  "Switch to the Nth sibling at the current level (1-based); else next."
  (interactive "P")
  (if n
      (let ((tab (nth (1- (prefix-numeric-value n))
                      (ygg-space--children-of
                       (or (ygg-space--parent-of (ygg-space--current))
                           ygg-space--root-id)))))
        (when tab (ygg-space--goto-id (ygg-space--id-of tab))))
    (ygg-space-next-sibling)))

;;; Rename / close

;;;###autoload
(defun ygg-space-rename (name)
  "Rename the current space to NAME (empty clears back to #id)."
  (interactive (list (read-string "Space name: "
                                   (let ((c (ygg-space--current)))
                                     (and (alist-get 'explicit-name c)
                                          (alist-get 'name c))))))
  (if (string-empty-p name)
      (progn (ygg-space--set (ygg-space--current) 'explicit-name nil)
             (tab-bar-rename-tab (ygg-space--name (ygg-space--current))))
    (tab-bar-rename-tab name))
  (force-mode-line-update t))

(defun ygg-space--subtree-ids (id)
  "IDs of ID and all its descendants (depth-first)."
  (cons id (mapcan (lambda (tb) (ygg-space--subtree-ids (ygg-space--id-of tb)))
                   (ygg-space--children-of id))))

;;;###autoload
(defun ygg-space-close ()
  "Close the current space and its whole subtree; land on the parent."
  (interactive)
  (let* ((cur (ygg-space--current))
         (parent (ygg-space--parent-of cur))
         (ids (ygg-space--subtree-ids (ygg-space--id-of cur)))
         ;; land on the real parent, or — at the top level — a sibling space
         ;; outside the closing subtree; nil means this is the last space.
         (land (if (eql parent ygg-space--root-id)
                   (ygg-space--id-of
                    (car (seq-remove
                          (lambda (tb) (memql (ygg-space--id-of tb) ids))
                          (ygg-space--children-of ygg-space--root-id))))
                 parent)))
    (cond
     ((null parent) (ygg-space--echo "won't close untagged space"))
     ((null land) (ygg-space--echo "won't close the last space"))
     (t (let ((ygg-space--closing t))
          (ygg-space--goto-id land)
          (dolist (id ids)
            (when-let* ((idx (ygg-space--index-of-id id)))
              (tab-bar-close-tab (1+ idx))))
          (ygg-space--echo "✕ closed, now %s"
                           (ygg-space--name (ygg-space--current))))))))

;;; Breadcrumb / picker

(defun ygg-space--breadcrumb (&optional tab)
  (let ((parts nil) (tb (or tab (ygg-space--current))))
    (while tb
      (push (ygg-space--name tb) parts)
      (setq tb (when-let* ((p (ygg-space--parent-of tb))) (ygg-space--tab-by-id p))))
    (string-join parts " ❯ ")))

;;;###autoload
(defun ygg-space-where ()
  "Echo the breadcrumb path of the current space, and the folder it is for."
  (interactive)
  (ygg-space--echo "%s%s" (ygg-space--breadcrumb)
                   (if-let* ((dir (ygg-space-dir)))
                       (format "   ⌂ %s" (abbreviate-file-name dir))
                     "")))

(defun ygg-space--roots ()
  "Tabs with no live parent — the forest roots, in tab-list order."
  (seq-filter (lambda (tb)
                (let ((p (ygg-space--parent-of tb)))
                  (or (null p) (null (ygg-space--tab-by-id p)))))
              (ygg-space--tabs)))

(defun ygg-space--tree-lines ()
  "Alist of (LABEL . ID) for every space, indented depth-first."
  (let ((cur (ygg-space--current-id)) (acc nil))
    (cl-labels
        ((walk (tb depth)
           (let* ((id (ygg-space--id-of tb))
                  (mark (if (eql id cur) "● " "○ "))
                  (label (concat (make-string (* 2 depth) ?\s)
                                 mark (ygg-space--name tb))))
             (push (cons label id) acc)
             (dolist (child (ygg-space--children-of id))
               (walk child (1+ depth))))))
      (dolist (root (ygg-space--roots)) (walk root 0)))
    (nreverse acc)))

(defvar ygg-space-pick-rows-functions nil
  "Abnormal hook: each is called with a space id, or nil for no space,
and returns rows (LABEL . ACTION) the picker lists under that space.
Picking a row switches to its space, then calls ACTION with no arguments.")

(defun ygg-space--pick-rows (id)
  "The rows every function on ygg-space-pick-rows-functions gives for ID."
  (let ((rows nil))
    (run-hook-wrapped 'ygg-space-pick-rows-functions
                      (lambda (fn)
                        (setq rows (append rows (funcall fn id)))
                        nil))
    rows))

(defun ygg-space--pick-lines ()
  "The picker's (LABEL . PICK) lines: the tree, each space's hook rows one
level under it, then the rows for no space under a heading of their own.
PICK is a space id, (ID . ACTION) for a hook row, or nil for the heading."
  (let ((acc nil) (seen (make-hash-table :test #'equal)))
    (cl-flet ((add (label pick)
                (let ((unique label) (n 1))
                  (while (gethash unique seen)
                    (setq unique (format "%s %d" label (cl-incf n))))
                  (puthash unique t seen)
                  (push (cons unique pick) acc))))
      (dolist (line (ygg-space--tree-lines))
        (puthash (car line) t seen)
        (push line acc)
        (let ((indent (make-string (+ 2 (string-match-p "[^ ]" (car line))) ?\s)))
          (dolist (row (ygg-space--pick-rows (cdr line)))
            (add (concat indent (car row)) (cons (cdr line) (cdr row))))))
      (when-let* ((rows (ygg-space--pick-rows nil)))
        (add (propertize "no zone" 'face 'shadow) nil)
        (dolist (row rows)
          (add (concat "  " (car row)) (cons nil (cdr row))))))
    (nreverse acc)))

;;;###autoload
(defun ygg-space-pick ()
  "Pick any space from an indented tree and switch to it.
Rows other layers hang under a space go there after the switch."
  (interactive)
  (let* ((lines (ygg-space--pick-lines))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity))
                    (complete-with-action action (mapcar #'car lines) str pred))))
         (choice (completing-read "Space: " table nil t))
         (pick (cdr (assoc choice lines))))
    (if (consp pick)
        (progn (when (car pick) (ygg-space--goto-id (car pick)))
               (funcall (cdr pick)))
      (when pick (ygg-space--goto-id pick)))))

;;; Reconciliation for tabs created/closed outside these commands

(defun ygg-space--on-open (&optional _tab)
  (unless ygg-space--spawning
    (unless (ygg-space--id-of (ygg-space--current))
      ;; a tab born outside our commands becomes a fresh top-level space
      (ygg-space--adopt-current ygg-space--root-id))))

(defun ygg-space--on-close (tab _only)
  "Promote the closed TAB's orphaned children to its parent."
  (unless ygg-space--closing
    (let ((gone (ygg-space--id-of tab))
          (grand (ygg-space--parent-of tab)))
      (when gone
        (dolist (child (ygg-space--children-of gone))
          (ygg-space--set child 'ygg-parent grand))))))

;;; Space-aware tab-bar: only same-level siblings + dimmed ancestor crumbs

(defun ygg-space--format-key ()
  "All the bar can differ by: the tick, where we stand, how many stand,
and the room and the font its tabs are stretched to."
  (let ((tabs (ygg-space--tabs)))
    (list ygg-space--format-tick
          (ygg-space--id-of (assq 'current-tab tabs))
          (seq-position tabs 'current-tab (lambda (tb x) (eq (car tb) x)))
          (length tabs)
          (frame-inner-width)
          (frame-char-width))))

(defun ygg-space--format ()
  "`tab-bar-format' entry: parent breadcrumb then current-level spaces.
Emacs rebuilds the tab-bar keymap on every single redisplay, so the
items are kept per frame and handed back as they are until something
the bar is drawn from has moved.  They are kept already stretched to
the frame's width, which Emacs would otherwise measure again each time."
  (let* ((frame (selected-frame))
         (key (ygg-space--format-key))
         (hit (assq frame ygg-space--format-cache)))
    (if (and hit (equal (cadr hit) key))
        (cddr hit)
      (let ((items (tab-bar-auto-width (ygg-space--format-1))))
        (setq ygg-space--format-cache
              (cons (cons frame (cons key items))
                    (seq-filter (lambda (entry)
                                  (and (not (eq (car entry) frame))
                                       (frame-live-p (car entry))))
                                ygg-space--format-cache)))
        items))))

(defun ygg-space--format-1 ()
  "Build the bar: parent breadcrumb then current-level spaces."
  (let* ((cur (ygg-space--current))
         (parent (ygg-space--parent-of cur))
         (crumbs nil) (items nil)
         (tb (and parent (ygg-space--tab-by-id parent))))
    (while tb
      (push (ygg-space--name tb) crumbs)
      (setq tb (when-let* ((p (ygg-space--parent-of tb))) (ygg-space--tab-by-id p))))
    (dolist (c crumbs)
      (push `(,(intern (format "ygg-crumb-%s" c)) menu-item
              ,(propertize (format " %s ›" c) 'face 'shadow) ignore)
            items))
    (let ((i 0))
      (dolist (sib (if parent (ygg-space--children-of parent) (ygg-space--roots)))
        (let* ((id (ygg-space--id-of sib))
               (name (ygg-space--name sib))
               (this (eql id (ygg-space--id-of cur)))
               (label (concat " " (propertize (number-to-string (setq i (1+ i)))
                                              'face 'shadow)
                              " " name
                              (and (ygg-space--children-of id) " ▸") " ")))
          (add-face-text-property
           0 (length label) (if this 'tab-bar-tab 'tab-bar-tab-inactive) t label)
          (push `(,(intern (format "ygg-space-%s" id)) menu-item ,label
                  ,(let ((tid id)) (lambda () (interactive) (ygg-space--goto-id tid)))
                  :help ,(format "switch to space %s" name))
                items))))
    (nreverse items)))

;;; Vertical sidebar — a herdr-style minimal space strip on the left

(defconst ygg-space-tree--buffer "*spaces*")

(defvar ygg-space-tree-width 18
  "Column width of the space-tree sidebar.")

(defvar ygg-space-tree--on nil
  "Non-nil when the sidebar is toggled on.")

(defvar ygg-space-tree--pending nil
  "Non-nil while a sidebar refresh is already queued.")

(defun ygg-space-tree--id-at-point ()
  (get-text-property (line-beginning-position) 'ygg-id))

(defun ygg-space-tree--goto-line-of (id)
  "Move point to the sidebar line showing space ID."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (eql (get-text-property (point) 'ygg-id) id)))
    (forward-line 1)))

(defun ygg-space-tree--select-at-point ()
  "Switch to the space at point, landing in the content window (vim-style)."
  (interactive)
  (when-let* ((id (ygg-space-tree--id-at-point)))
    (select-window (ygg-ui-main-window))
    (ygg-space--goto-id id)))

(defun ygg-space-tree--mouse-select (event)
  "Switch to the space clicked in the sidebar."
  (interactive "e")
  (mouse-set-point event)
  (ygg-space-tree--select-at-point))

(defun ygg-space-tree-parent ()
  "Move point to the parent of the space at point."
  (interactive)
  (when-let* ((id (ygg-space-tree--id-at-point))
              (tab (ygg-space--tab-by-id id))
              (parent (ygg-space--parent-of tab))
              ((ygg-space--tab-by-id parent)))
    (ygg-space-tree--goto-line-of parent)))

(defun ygg-space-tree-child ()
  "Move point to the first child of the space at point."
  (interactive)
  (when-let* ((id (ygg-space-tree--id-at-point))
              (child (car (ygg-space--children-of id))))
    (ygg-space-tree--goto-line-of (ygg-space--id-of child))))

(defun ygg-space-tree-add-child ()
  "Create a child of the space at point and switch to it."
  (interactive)
  (when-let* ((id (ygg-space-tree--id-at-point)))
    (select-window (ygg-ui-main-window))
    (ygg-space--spawn id)))

(defun ygg-space-tree-rename ()
  "Rename the space at point without switching to it (empty clears to #id)."
  (interactive)
  (when-let* ((id (ygg-space-tree--id-at-point))
              (tab (ygg-space--tab-by-id id))
              (idx (ygg-space--index-of-id id)))
    (let ((name (read-string "Space name: "
                             (and (alist-get 'explicit-name tab)
                                  (alist-get 'name tab)))))
      (if (string-empty-p name)
          (progn (ygg-space--set tab 'explicit-name nil)
                 (tab-bar-rename-tab (ygg-space--name tab) (1+ idx)))
        (tab-bar-rename-tab name (1+ idx))))))

(defun ygg-space-tree-jump ()
  "Switch to the space on the sidebar row numbered by the digit pressed."
  (interactive)
  (when-let* ((line (nth (- (event-basic-type last-command-event) ?1)
                         (ygg-space--tree-lines))))
    (select-window (ygg-ui-main-window))
    (ygg-space--goto-id (cdr line))))

(defun ygg-space-tree--forward-space (n)
  "Move N rows, stopping only on a space's row."
  (let ((moved 0))
    (while (and (zerop moved)
                (zerop (forward-line n))
                (not (eobp)))
      (when (ygg-space-tree--id-at-point)
        (setq moved 1)))
    (beginning-of-line)))

(defun ygg-space-tree-next ()
  "Move to the next space row."
  (interactive)
  (ygg-space-tree--forward-space 1))

(defun ygg-space-tree-prev ()
  "Move to the previous space row."
  (interactive)
  (ygg-space-tree--forward-space -1))

(defun ygg-space-tree-forward-rows (n)
  "Move N space rows, the way a half page moves a list."
  (dotimes (_ (abs n))
    (ygg-space-tree--forward-space (if (< n 0) -1 1))))

(defun ygg-space-tree-down-half ()
  "Move down several space rows."
  (interactive)
  (ygg-space-tree-forward-rows 5))

(defun ygg-space-tree-up-half ()
  "Move up several space rows."
  (interactive)
  (ygg-space-tree-forward-rows -5))

(defun ygg-space-tree-first ()
  "Go to the first space row."
  (interactive)
  (goto-char (point-min))
  (unless (ygg-space-tree--id-at-point)
    (ygg-space-tree--forward-space 1)))

(defun ygg-space-tree-last ()
  "Go to the last space row."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  (unless (ygg-space-tree--id-at-point)
    (ygg-space-tree--forward-space -1)))

(defun ygg-space-tree-help ()
  "Echo the sidebar keys."
  (interactive)
  (message "RET/click switch · j k h l move · 1-9 jump · a add · r rename · d close · q hide"))

(defun ygg-space-tree-close ()
  "Close the space at point with its subtree; ask first."
  (interactive)
  (when-let* ((id (ygg-space-tree--id-at-point))
              (tab (ygg-space--tab-by-id id))
              (name (ygg-space--name tab)))
    (when (y-or-n-p (format "Close space %s? " name))
      (let ((ids (ygg-space--subtree-ids id)))
        (if (memql (ygg-space--current-id) ids)
            (progn (ygg-space--goto-id id) (ygg-space-close))
          (let ((ygg-space--closing t))
            (dolist (i ids)
              (when-let* ((idx (ygg-space--index-of-id i)))
                (tab-bar-close-tab (1+ idx))))))))))

;;;###autoload
(defun ygg-space-tree ()
  "Toggle the minimal vertical space-tree sidebar."
  (interactive)
  (setq ygg-space-tree--on (not ygg-space-tree--on))
  (ygg-space-tree--sync))

;;;###autoload
(defun ygg-space-tree-adopt ()
  "Keep the sidebar iff the restored frameset brought its window along."
  (setq ygg-space-tree--on (and (get-buffer-window ygg-space-tree--buffer) t))
  (ygg-space-tree--sync))

;; special-mode buffers stay outside yggdrasil modal, so vim keys live here
(defvar-keymap ygg-space-tree-mode-map
  "RET" #'ygg-space-tree--select-at-point
  "<mouse-1>" #'ygg-space-tree--mouse-select
  "j" #'ygg-space-tree-next
  "k" #'ygg-space-tree-prev
  "h" #'ygg-space-tree-parent
  "l" #'ygg-space-tree-child
  "g g" #'ygg-space-tree-first
  "G" #'ygg-space-tree-last
  "C-d" #'ygg-space-tree-down-half
  "C-u" #'ygg-space-tree-up-half
  "a" #'ygg-space-tree-add-child
  "r" #'ygg-space-tree-rename
  "d" #'ygg-space-tree-close
  "?" #'ygg-space-tree-help
  "q" #'ygg-space-tree)

(dotimes (i 9)
  (keymap-set ygg-space-tree-mode-map (format "%d" (1+ i)) #'ygg-space-tree-jump))

(define-derived-mode ygg-space-tree-mode special-mode "spaces"
  "Read-only vertical listing of the space tree."
  (ygg-ui-plain-layout)
  (setq-local mode-line-format nil
              cursor-in-non-selected-windows nil)
  (hl-line-mode 1))

(defface ygg-space-tree-current
  '((t :inherit hl-line :extend t))
  "Row of the active space in the sidebar.")

(defvar ygg-space-tree-state-function nil
  "Optional (SPACE-ID) -> face for a sidebar row's state dot, or nil.")

(defvar ygg-space-tree-detail-function nil
  "Optional (SPACE-ID) -> propertized lines rendered under that space's row.
Detail lines carry no ygg-id property, so space navigation skips them.")

(defun ygg-space-tree--render ()
  "(Re)draw the sidebar buffer; return it."
  (let ((buf (get-buffer-create ygg-space-tree--buffer))
        (cur (ygg-space--current-id))
        (cur-pos 1)
        (n 0))
    (with-current-buffer buf
      (unless (derived-mode-p 'ygg-space-tree-mode)
        (ygg-space-tree-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (line (ygg-space--tree-lines))
          (let* ((this (eql (cdr line) cur))
                 (label (propertize (car line) 'face (if this 'default 'shadow)))
                 (row (progn
                        (when-let* ((fn ygg-space-tree-state-function)
                                    (face (funcall fn (cdr line)))
                                    (i (string-match "[●○]" label)))
                          (put-text-property i (1+ i) 'face face label))
                        (concat
                         (propertize (format "%d " (setq n (1+ n))) 'face 'shadow)
                         label "\n"))))
            (when this
              (setq cur-pos (point))
              (add-face-text-property 0 (length row) 'ygg-space-tree-current t row))
            (insert (propertize row
                                'ygg-id (cdr line)
                                'mouse-face 'highlight))
            (when-let* ((fn ygg-space-tree-detail-function)
                        (details (funcall fn (cdr line))))
              (dolist (d details) (insert d "\n"))))))
      (goto-char cur-pos)
      (dolist (win (get-buffer-window-list buf nil t))
        (set-window-point win cur-pos)))
    buf))

(defun ygg-space-tree--show ()
  (with-demoted-errors "ygg-space-tree: %S"
    (display-buffer
     (ygg-space-tree--render)
     `(display-buffer-in-side-window
       (side . left) (slot . 0)
       (dedicated . t)
       (window-width . ,ygg-space-tree-width)
       (window-parameters . ((no-other-window . t)
                             (no-delete-other-windows . t)))))))

(defun ygg-space-tree--sync ()
  "Make reality match the toggle: window present and fresh, or absent."
  (let ((win (get-buffer-window ygg-space-tree--buffer)))
    (cond ((not ygg-space-tree--on) (when win (delete-window win)))
          (win (ygg-space-tree--render))
          (t (ygg-space-tree--show)))))

(defun ygg-space-tree--queue (&rest _)
  "Refresh the sidebar once the current tab operation settles."
  (unless ygg-space-tree--pending
    (setq ygg-space-tree--pending t)
    (run-at-time 0 nil (lambda ()
                         (setq ygg-space-tree--pending nil)
                         (ygg-space-tree--sync)))))

;;;###autoload
(defun ygg-space-reseed-ids ()
  "Reset the id counter above every id present, after a session restore.
Also heals legacy spaces saved with a nil parent (pre-phantom-root data)
by re-parenting them to the phantom root, so they group as siblings."
  (setq ygg-space--id
        (apply #'max 0 (delq nil (mapcar #'ygg-space--id-of (ygg-space--tabs)))))
  (dolist (tb (ygg-space--tabs))
    (when (and (ygg-space--id-of tb) (null (ygg-space--parent-of tb)))
      (ygg-space--set tb 'ygg-parent ygg-space--root-id)))
  ;; a restore writes the tabs behind every hook, and can bring back the
  ;; same count under the same ids the bar was last drawn for
  (ygg-space--format-invalidate))

(defun ygg-space-modeline ()
  "Mode-line segment: the current space's name, dimmed."
  (when-let* ((name (and tab-bar-mode (ygg-space--name (ygg-space--current)))))
    (propertize (concat "  " name) 'face 'shadow)))

;;;###autoload
(defun ygg-spacetree-setup ()
  "Enable the space-tree tab-bar: crumbs + numbered same-level siblings."
  (setq tab-bar-show t
        tab-bar-format '(ygg-space--format)
        tab-bar-auto-width nil)
  (tab-bar-mode 1)
  (ygg-space--ensure-root)
  (add-hook 'tab-bar-tab-post-open-functions #'ygg-space--on-open)
  (add-hook 'tab-bar-tab-pre-close-functions #'ygg-space--on-close)
  ;; every way a tab, its name or the faces it is drawn in can change
  (add-hook 'tab-bar-tab-post-open-functions #'ygg-space--format-invalidate)
  (add-hook 'tab-bar-tab-pre-close-functions #'ygg-space--format-invalidate)
  (add-hook 'tab-bar-tab-post-select-functions #'ygg-space--format-invalidate)
  (add-hook 'tab-bar-tab-post-change-group-functions
            #'ygg-space--format-invalidate)
  (add-hook 'enable-theme-functions #'ygg-space--format-invalidate)
  (add-hook 'disable-theme-functions #'ygg-space--format-invalidate)
  (advice-add 'tab-bar-rename-tab :after #'ygg-space--format-invalidate)
  (advice-add 'tab-bar-move-tab-to :after #'ygg-space--format-invalidate)
  (advice-add 'tab-bar-move-tab-to-frame :after #'ygg-space--format-invalidate)
  ;; keep the sidebar in step with every way a tab can change
  (add-hook 'tab-bar-tab-post-select-functions #'ygg-space-tree--queue)
  (add-hook 'tab-bar-tab-post-open-functions #'ygg-space-tree--queue)
  (add-hook 'tab-bar-tab-pre-close-functions #'ygg-space-tree--queue)
  (advice-add 'tab-bar-rename-tab :after #'ygg-space-tree--queue)
  ;; a side window baked into a tab's saved layout makes the restore split
  ;; around it; the guard rebuilds it under `window-combination-resize'
  (advice-add 'split-window :around #'ygg-space--allow-side-split)
  (advice-add 'tab-bar-select-tab :around #'ygg-space--safe-select))

(provide 'yggdrasil-spacetree)
;;; yggdrasil-spacetree.el ends here
