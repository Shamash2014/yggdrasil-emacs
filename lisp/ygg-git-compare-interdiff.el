;;; ygg-git-compare-interdiff.el --- what B changed since it was last seen -*- lexical-binding: t; -*-

;;; Commentary:
;; Every compare remembers the commit side B stands at, under B's name:
;; its branch, pull request or worktree.  When B is force-pushed or
;; rebased, the reviewer sees how its commits changed since a version
;; seen before, as git range-diff shows it, or as a plain diff of the two
;; versions over the files B changes.  A worktree is remembered by its
;; HEAD, so what it has not committed is left out.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'ansi-color)

(defvar ygg-git-compare--root)
(defvar ygg-git-compare--a)
(defvar ygg-git-compare--b)
(defvar ygg-git-compare--b-spec)
(defvar ygg-git-compare--file-window)
(defvar ygg-git-compare--list-buffer)
(defvar magit-display-buffer-function)
(declare-function ygg-git-compare--git "ygg-git-compare" (&rest args))
(declare-function ygg-git-compare--list "ygg-git-compare" ())
(declare-function ygg-git-compare-mode "ygg-git-compare" (&optional arg))
(declare-function ygg-git-compare-table "ygg-git-compare" (cands category))
(declare-function ygg-git-compare--group "ygg-git-compare" (label group &optional note))
(declare-function magit-gitdir "magit-git" (&optional directory common))
(declare-function magit-rev-format "magit-git" (format &optional rev args))
(declare-function magit-diff-setup-buffer "magit-diff"
                  (range typearg args files &optional type locked))

(defcustom ygg-git-compare-interdiff-keep 20
  "How many versions of a side B are remembered."
  :type 'natnum
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-interdiff-keep-sides 200
  "How many sides B are remembered; the longest unchanged go first."
  :type 'natnum
  :group 'ygg-git-compare)

(defun ygg-git-compare-interdiff--file ()
  "The file this repository's seen versions are kept in, shared by its worktrees."
  (expand-file-name "ygg-review-seen.eld" (magit-gitdir nil t)))

(defun ygg-git-compare-interdiff--load (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (ignore-errors (read (current-buffer))))))

(defun ygg-git-compare-interdiff-key (spec)
  "The name side SPEC is remembered under: its checkout, pull request or rev."
  (pcase spec
    (`(worktree . ,dir) (directory-file-name dir))
    (`(pr . ,pr) (format "#%s" (plist-get pr :number)))
    (`(rev . ,rev) rev)))

(defun ygg-git-compare-interdiff-seen (key)
  "The versions of KEY seen in this repository, as (SHA . TIME), newest first."
  (alist-get key (ygg-git-compare-interdiff--load (ygg-git-compare-interdiff--file))
             nil nil #'equal))

(defun ygg-git-compare-interdiff-remember (key sha)
  "Remember SHA as the newest version of KEY seen."
  (let* ((file (ygg-git-compare-interdiff--file))
         (all (ygg-git-compare-interdiff--load file))
         (seen (alist-get key all nil nil #'equal)))
    (unless (equal (caar seen) sha)
      (let ((versions (seq-take (cons (cons sha (truncate (float-time)))
                                      (seq-remove (lambda (e) (equal (car e) sha)) seen))
                                ygg-git-compare-interdiff-keep)))
        (with-temp-file file
          (let ((print-length nil) (print-level nil))
            (prin1 (seq-take (cons (cons key versions) (assoc-delete-all key all #'equal))
                             ygg-git-compare-interdiff-keep-sides)
                   (current-buffer))))))))

(defun ygg-git-compare-interdiff-record ()
  "Remember the commit side B of the compare here stands at."
  (when-let* ((spec (bound-and-true-p ygg-git-compare--b-spec))
              (sha (plist-get ygg-git-compare--b :log)))
    (with-demoted-errors "B's version not remembered: %S"
      (let ((default-directory ygg-git-compare--root))
        (ygg-git-compare-interdiff-remember (ygg-git-compare-interdiff-key spec) sha)))))

(add-hook 'magit-refresh-buffer-hook #'ygg-git-compare-interdiff-record)

(defun ygg-git-compare-interdiff--read-old (key new)
  "Read a version of KEY seen before other than NEW, the newest by default."
  (let* ((now (float-time))
         (cands (mapcar (pcase-lambda (`(,sha . ,time))
                          (cons (ygg-git-compare--group
                                 (substring sha 0 (min 12 (length sha))) "Seen"
                                 (format "%s ago  %s" (seconds-to-string (- now time) t)
                                         (or (magit-rev-format "%s" sha) "(gone)")))
                                sha))
                        (seq-remove (lambda (e) (equal (car e) new))
                                    (ygg-git-compare-interdiff-seen key)))))
    (unless cands
      (user-error "No earlier version of %s seen" key))
    (cdr (assoc (completing-read (format-prompt "%s changed since" (caar cands) key)
                                 (ygg-git-compare-table cands 'ygg-review-sha)
                                 nil t nil nil (caar cands))
                cands))))

(defun ygg-git-compare-interdiff--display (list)
  "A function showing a buffer in LIST's right pane, else as `display-buffer' does."
  (let ((window (buffer-local-value 'ygg-git-compare--file-window list)))
    (lambda (buffer)
      (if (window-live-p window)
          (progn (set-window-buffer window buffer) window)
        (display-buffer buffer)))))

(defun ygg-git-compare-interdiff--adopt (buffer list)
  "Make BUFFER a pane of the compare LIST."
  (with-current-buffer buffer
    (setq ygg-git-compare--list-buffer list)
    (ygg-git-compare-mode 1)
    buffer))

(defun ygg-git-compare-interdiff--range-diff (a old new list)
  "Show git range-diff of A..OLD against A..NEW, a pane of LIST."
  (let ((text (ygg-git-compare--git "range-diff" "--color=always"
                                    (concat a ".." old) (concat a ".." new)))
        (dir default-directory)
        (buffer (get-buffer-create (format "*range-diff: %s*" (buffer-name list)))))
    (with-current-buffer buffer
      (special-mode)
      (setq default-directory dir
            header-line-format (format "B was %s, now %s, against A %s"
                                       (substring old 0 7) (substring new 0 7)
                                       (substring a 0 7)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (string-empty-p text) "No commits on either side." text))
        (ansi-color-apply-on-region (point-min) (point-max))
        (goto-char (point-min))))
    (ygg-git-compare-interdiff--adopt buffer list)
    (select-window (funcall (ygg-git-compare-interdiff--display list) buffer))
    buffer))

(defun ygg-git-compare-interdiff--plain (a old new list)
  "Show git diff OLD..NEW over the files A...NEW touches, a pane of LIST."
  (let ((files (split-string (ygg-git-compare--git "diff" "--name-only" "-z"
                                                   (concat a "..." new))
                             "\0" t)))
    (unless files
      (user-error "B changes no file against A"))
    (ygg-git-compare-interdiff--adopt
     (let ((magit-display-buffer-function (ygg-git-compare-interdiff--display list)))
       (magit-diff-setup-buffer (concat old ".." new) nil nil files 'committed t))
     list)))

;;;###autoload
(defun ygg-git-compare-interdiff (&optional plain)
  "Show how side B's commits changed since a version of B seen before.
Each compare remembers the commit B stands at; the one read here
defaults to the newest other than B now.  The change is shown as git
range-diff over A; with PLAIN, as a diff of the two versions over the
files B changes against A.  A worktree's uncommitted changes are left out."
  (interactive "P")
  (with-current-buffer (ygg-git-compare--list)
    (let* ((default-directory ygg-git-compare--root)
           (a (plist-get ygg-git-compare--a :diff))
           (new (plist-get ygg-git-compare--b :log))
           (old (ygg-git-compare-interdiff--read-old
                 (ygg-git-compare-interdiff-key ygg-git-compare--b-spec) new))
           (list (current-buffer)))
      (ygg-git-compare--git "cat-file" "-e" (concat old "^{commit}"))
      (prog1 (if plain
                 (ygg-git-compare-interdiff--plain a old new list)
               (ygg-git-compare-interdiff--range-diff a old new list))
        (when (plist-get ygg-git-compare--b :uncommitted)
          (message "Commits only: B's uncommitted changes are left out"))))))

(provide 'ygg-git-compare-interdiff)
;;; ygg-git-compare-interdiff.el ends here
