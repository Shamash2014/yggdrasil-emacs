;;; ygg-git-compare-marks.el --- check hunks of a compare off as reviewed -*- lexical-binding: t; -*-

;;; Commentary:
;; A hunk marked reviewed is folded and ticked in every compare of the
;; repository.  A mark is the hunk's file and its added and removed lines,
;; so it holds when lines above shift and lapses when the hunk itself
;; changes.  Marks are kept in the repository's git directory, never in a
;; worktree.  Unreviewed-only hides what is done, and marking there moves
;; on to what is left.

;;; Code:

(require 'subr-x)
(require 'magit)
(require 'ygg-git-compare)

(defconst ygg-git-compare-marks-file "ygg-review-marks.eld"
  "The file in the repository's common git directory that keeps its marks.")

(defcustom ygg-git-compare-marks-keep 5000
  "How many marks a repository keeps; the oldest go first."
  :type 'natnum
  :group 'ygg-git-compare)

(defface ygg-git-compare-unreviewed '((t :inherit bold))
  "The changed lines of a hunk not yet reviewed."
  :group 'ygg-git-compare)

(defvar-local ygg-git-compare-marks--unreviewed-only nil
  "Whether this compare hides the hunks and files already reviewed.")

;;; Marks on disk

(defun ygg-git-compare-marks--path ()
  (expand-file-name ygg-git-compare-marks-file
                    (or (magit-gitdir nil t) (user-error "Not in a git repository"))))

(defun ygg-git-compare-marks--read ()
  "The repository's marks as key to time marked, empty when the file is missing
or unreadable.  A key kept without a time is the oldest."
  (let ((marks (make-hash-table :test #'equal))
        (entries (ignore-errors
                   (with-temp-buffer
                     (insert-file-contents (ygg-git-compare-marks--path))
                     (read (current-buffer))))))
    (dolist (entry (and (proper-list-p entries) entries) marks)
      (pcase entry
        ((pred stringp) (puthash entry 0 marks))
        (`(,(and key (pred stringp)) . ,(and time (pred numberp)))
         (puthash key time marks))))))

(defun ygg-git-compare-marks--write (marks)
  "Keep the newest `ygg-git-compare-marks-keep' of MARKS."
  (let ((print-length nil) (print-level nil)
        (entries nil))
    (maphash (lambda (key time) (push (cons key time) entries)) marks)
    (with-temp-file (ygg-git-compare-marks--path)
      (prin1 (seq-take (seq-sort-by #'cdr #'> entries) ygg-git-compare-marks-keep)
             (current-buffer)))))

;;; Hunks

(defvar ygg-git-compare-marks--bases (make-hash-table :test #'eq :weakness 'key)
  "Hunk section to its changed-lines hash.")

(defun ygg-git-compare-marks--base (hunk)
  "HUNK's file and its added and removed lines, hashed."
  (with-memoization (gethash hunk ygg-git-compare-marks--bases)
    (save-excursion
      (goto-char (oref hunk content))
      (let (changed)
        (while (< (point) (oref hunk end))
          (when (memq (char-after) '(?+ ?-))
            (push (buffer-substring-no-properties (point) (line-end-position)) changed))
          (forward-line))
        (secure-hash 'sha1 (string-join (cons (oref (oref hunk parent) value)
                                              (nreverse changed))
                                        "\n"))))))

(defvar ygg-git-compare-marks--keys (make-hash-table :test #'eq :weakness 'key)
  "Hunk section to its mark key.")

(defun ygg-git-compare-marks--key (hunk)
  "HUNK's hash, numbered among the hunks of its file with the same changed lines."
  (with-memoization (gethash hunk ygg-git-compare-marks--keys)
    (let* ((base (ygg-git-compare-marks--base hunk))
           (twins (seq-count (lambda (h) (equal (ygg-git-compare-marks--base h) base))
                             (seq-take-while (lambda (h) (not (eq h hunk)))
                                             (ygg-git-compare-marks--hunks (oref hunk parent))))))
      (if (zerop twins) base (format "%s#%d" base twins)))))

(defun ygg-git-compare-marks--hunks (&optional file)
  "Every hunk section of FILE's section, or of the buffer, in order."
  (let (hunks)
    (magit-map-sections (lambda (s) (when (magit-section-match 'hunk s) (push s hunks)))
                        (or file magit-root-section))
    (nreverse hunks)))

(defun ygg-git-compare-marks--file-at-point ()
  (when-let* ((section (magit-current-section)))
    (if (magit-section-match 'hunk section) (oref section parent)
      (and (magit-section-match 'file section)
           (ygg-git-compare-marks--hunks section)
           section))))

;;; Display

(defun ygg-git-compare-marks--span (beg end &rest props)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'ygg-git-compare-marks t)
    (overlay-put o 'evaporate t)
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun ygg-git-compare-marks--overlay (section &rest props)
  (apply #'ygg-git-compare-marks--span (oref section start) (oref section end) props))

(defun ygg-git-compare-marks--embolden (hunk)
  "Draw the added and removed lines of HUNK in `ygg-git-compare-unreviewed'."
  (save-excursion
    (goto-char (oref hunk content))
    (while (< (point) (oref hunk end))
      (if (not (memq (char-after) '(?+ ?-)))
          (forward-line)
        (let ((beg (point)))
          (while (and (< (point) (oref hunk end)) (memq (char-after) '(?+ ?-)))
            (forward-line))
          (ygg-git-compare-marks--span beg (point) 'face 'ygg-git-compare-unreviewed))))))

(defun ygg-git-compare-marks--tick (section)
  (let ((o (make-overlay (oref section start) (or (oref section content) (oref section end)))))
    (overlay-put o 'ygg-git-compare-marks t)
    (overlay-put o 'evaporate t)
    (overlay-put o 'face 'magit-dimmed)
    (overlay-put o 'before-string (propertize "✓ " 'face 'success))))

(defun ygg-git-compare-marks--apply (&optional marks)
  "Tick and fold the reviewed hunks here, and files with nothing left."
  (when magit-root-section
    (let ((marks (or marks (ygg-git-compare-marks--read)))
          (magit-section-cache-visibility nil)
          files)
      (ygg-git-compare-marks--import marks)
      (remove-overlays (point-min) (point-max) 'ygg-git-compare-marks t)
      (add-to-invisibility-spec 'ygg-git-compare-marks)
      (dolist (hunk (ygg-git-compare-marks--hunks))
        (let ((file (oref hunk parent)))
          (unless (assq file files) (push (list file t) files))
          (if (not (gethash (ygg-git-compare-marks--key hunk) marks))
              (progn (setcar (cdr (assq file files)) nil)
                     (ygg-git-compare-marks--embolden hunk))
            (ygg-git-compare-marks--tick hunk)
            (ygg-git-compare-marks--overlay hunk 'invisible 'ygg-git-compare-marks)
            (magit-section-hide hunk))))
      (pcase-dolist (`(,file ,done) files)
        (when done
          (ygg-git-compare-marks--tick file)
          (ygg-git-compare-marks--overlay file 'invisible 'ygg-git-compare-marks)))
      (unless (buffer-local-value 'ygg-git-compare-marks--unreviewed-only
                                  (ygg-git-compare-marks--home))
        (remove-from-invisibility-spec 'ygg-git-compare-marks)))))

(defun ygg-git-compare-marks--home ()
  "The compare's diff of every file, or this buffer when it has none."
  (or (ignore-error user-error (ygg-git-compare--list)) (current-buffer)))

(defun ygg-git-compare-marks--buffers ()
  (let ((list (ygg-git-compare-marks--home)))
    (seq-filter #'buffer-live-p
                (delete-dups
                 (list (current-buffer) list
                       (when-let* ((window (buffer-local-value
                                            'ygg-git-compare--file-window list))
                                   ((window-live-p window)))
                         (window-buffer window)))))))

(defun ygg-git-compare-marks--apply-all (&optional marks)
  (let ((marks (or marks (ygg-git-compare-marks--read))))
    (dolist (buffer (ygg-git-compare-marks--buffers))
      (with-current-buffer buffer
        (when ygg-git-compare-mode (ygg-git-compare-marks--apply marks))))))

(defun ygg-git-compare-marks--setup ()
  (when ygg-git-compare-mode
    (add-hook 'magit-refresh-buffer-hook #'ygg-git-compare-marks--apply nil t)
    (ygg-git-compare-marks--apply)
    (ygg-git-compare-marks--sync)))

(add-hook 'ygg-git-compare-mode-hook #'ygg-git-compare-marks--setup)
(add-hook 'ygg-git-compare-redraw-hook #'ygg-git-compare-marks--sync)

;;; GitHub

(defvar ygg-git-compare-marks--ids (make-hash-table :test #'equal)
  "Pull request (HOST PATH NUMBER) to its GraphQL node id.")

(defvar ygg-git-compare-marks--viewed (make-hash-table :test #'equal)
  "Pull request (HOST PATH NUMBER) to the files GitHub shows viewed that
are not yet marked here.")

(defvar-local ygg-git-compare-marks--synced nil
  "The pull request whose viewed files this compare has asked GitHub for.")

(defconst ygg-git-compare-marks--query
  "query($owner:String!,$name:String!,$number:Int!,$after:String){repository(owner:$owner,name:$name){pullRequest(number:$number){id files(first:100,after:$after){nodes{path viewerViewedState} pageInfo{hasNextPage endCursor}}}}}")

(defun ygg-git-compare-marks--github-pr ()
  "This compare's pull request as (HOST PATH NUMBER) when it is GitHub's."
  (pcase ygg-git-compare--b-spec
    (`(pr . ,pr)
     (let ((repo (if (plist-get pr :host)
                     (list (plist-get pr :forge) (plist-get pr :host) (plist-get pr :path))
                   (ignore-errors (ygg-git-compare--forge-repo (plist-get pr :remote))))))
       (when (and (eq (car repo) 'github) (plist-get pr :number))
         (list (nth 1 repo) (nth 2 repo) (plist-get pr :number)))))))

(defun ygg-git-compare-marks--graphql (pr query vars done)
  "Run QUERY with VARS, flags for gh, against PR's host, then call DONE as
`--answer' does with the parsed reply."
  (ygg-git-compare--forge-async
   "gh" (append (list "api" "--hostname" (car pr) "graphql" "-f" (concat "query=" query)) vars)
   (lambda (status text err)
     (ygg-git-compare--answer done "gh" status text err #'ygg-git-compare--json))))

(defun ygg-git-compare-marks--page (pr after done)
  "Call DONE with PR's node id and one page of its files, or nil and why."
  (pcase-let ((`(,_ ,path ,number) pr))
    (pcase-let ((`(,owner ,name) (split-string path "/")))
      (ygg-git-compare-marks--graphql
       pr ygg-git-compare-marks--query
       (append (list "-f" (concat "owner=" owner) "-f" (concat "name=" name)
                     "-F" (format "number=%s" number))
               (and after (list "-f" (concat "after=" after))))
       (lambda (reply &optional failure)
         (if-let* ((data (plist-get (plist-get (plist-get reply :data) :repository)
                                    :pullRequest)))
             (progn (puthash pr (plist-get data :id) ygg-git-compare-marks--ids)
                    (funcall done data))
           (funcall done nil (or failure "no such pull request"))))))))

(defun ygg-git-compare-marks--say (what failure)
  (message "GitHub: could not %s: %s" what (car (split-string failure "\n"))))

(defun ygg-git-compare-marks--send (path viewed)
  "Tell GitHub PATH is viewed, or no longer, in this compare's pull request."
  (when-let* ((pr (with-current-buffer (ygg-git-compare-marks--home)
                    (ygg-git-compare-marks--github-pr))))
    (let* ((what (format "%s %s" (if viewed "mark viewed" "unmark viewed") path))
           (mutation (format "mutation($id:ID!,$path:String!){%s(input:{pullRequestId:$id,path:$path}){clientMutationId}}"
                             (if viewed "markFileAsViewed" "unmarkFileAsViewed")))
           (send (lambda (id)
                   (ygg-git-compare-marks--graphql
                    pr mutation (list "-f" (concat "id=" id) "-f" (concat "path=" path))
                    (lambda (_reply &optional failure)
                      (when failure (ygg-git-compare-marks--say what failure)))))))
      (if-let* ((id (gethash pr ygg-git-compare-marks--ids)))
          (funcall send id)
        (ygg-git-compare-marks--page
         pr nil (lambda (data &optional failure)
                  (if data (funcall send (plist-get data :id))
                    (ygg-git-compare-marks--say what failure))))))))

(defun ygg-git-compare-marks--import (marks)
  "Mark in MARKS the hunks here of files GitHub shows viewed."
  (when-let* ((pr (with-current-buffer (ygg-git-compare-marks--home)
                    (ygg-git-compare-marks--github-pr)))
              (pending (gethash pr ygg-git-compare-marks--viewed)))
    (let ((seen nil) (changed nil))
      (dolist (hunk (ygg-git-compare-marks--hunks))
        (let ((file (oref (oref hunk parent) value)))
          (when (member file pending)
            (push file seen)
            (let ((key (ygg-git-compare-marks--key hunk)))
              (unless (gethash key marks)
                (puthash key (truncate (float-time)) marks)
                (setq changed t))))))
      (puthash pr (seq-difference pending seen) ygg-git-compare-marks--viewed)
      (when changed (ygg-git-compare-marks--write marks)))))

(defun ygg-git-compare-marks--sync ()
  "Once per pull request, take the files GitHub shows viewed as reviewed."
  (when ygg-git-compare-mode
    (let ((buffer (ygg-git-compare-marks--home)))
      (with-current-buffer buffer
        (when-let* ((pr (ygg-git-compare-marks--github-pr))
                    ((not (equal pr ygg-git-compare-marks--synced))))
          (setq ygg-git-compare-marks--synced pr)
          (let (paths)
            (named-let page ((after nil))
              (ygg-git-compare-marks--page
               pr after
               (lambda (data &optional failure)
                 (if (null data)
                     (progn (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (setq ygg-git-compare-marks--synced nil)))
                            (ygg-git-compare-marks--say "read viewed files" failure))
                   (let ((files (plist-get data :files)))
                     (dolist (node (plist-get files :nodes))
                       (when (equal (plist-get node :viewerViewedState) "VIEWED")
                         (push (plist-get node :path) paths)))
                     (if (plist-get (plist-get files :pageInfo) :hasNextPage)
                         (page (plist-get (plist-get files :pageInfo) :endCursor))
                       (puthash pr paths ygg-git-compare-marks--viewed)
                       (when (and paths (buffer-live-p buffer))
                         (with-current-buffer buffer
                           (ygg-git-compare-marks--apply-all)))))))))))))))

;;; Commands

(defun ygg-git-compare-marks--count (marks)
  "The hunks here as (REVIEWED . ALL), or nil when there are none."
  (when-let* ((hunks (ygg-git-compare-marks--hunks)))
    (cons (seq-count (lambda (h) (gethash (ygg-git-compare-marks--key h) marks)) hunks)
          (length hunks))))

(defun ygg-git-compare-marks--progress (marks)
  "Say how many hunks are reviewed: of the compare, or of this file when the
compare is too large to hold its hunks."
  (pcase-let ((`(,done . ,all)
               (or (with-current-buffer (ygg-git-compare-marks--home)
                     (ygg-git-compare-marks--count marks))
                   (ygg-git-compare-marks--count marks)
                   '(0 . 0))))
    (message "Reviewed %d/%d hunks" done all)))

(defun ygg-git-compare-marks--toggle (hunks &optional file)
  "Mark HUNKS reviewed, or unmark them all when every one already is.  When
they are all of FILE, GitHub is told it is viewed, or no longer."
  (let* ((marks (ygg-git-compare-marks--read))
         (keys (mapcar #'ygg-git-compare-marks--key hunks))
         (done (seq-every-p (lambda (k) (gethash k marks)) keys)))
    (dolist (key keys)
      (if done (remhash key marks) (puthash key (truncate (float-time)) marks)))
    (ygg-git-compare-marks--write marks)
    (ygg-git-compare-marks--apply-all marks)
    (when file (ygg-git-compare-marks--send (oref file value) (not done)))
    (when done
      (let ((magit-section-cache-visibility nil))
        (mapc #'magit-section-show hunks)))
    (when (and (not done) (buffer-local-value 'ygg-git-compare-marks--unreviewed-only
                                              (ygg-git-compare-marks--home)))
      (ygg-git-compare-marks--next marks))
    (ygg-git-compare-marks--progress marks)))

;;;###autoload
(defun ygg-git-compare-mark-hunk-reviewed ()
  "Mark the hunk at point reviewed, or not reviewed when it already is."
  (interactive)
  (let ((hunk (magit-current-section)))
    (unless (and hunk (magit-section-match 'hunk hunk)) (user-error "No hunk at point"))
    (ygg-git-compare-marks--toggle (list hunk))))

;;;###autoload
(defun ygg-git-compare-mark-file-reviewed ()
  "Mark every hunk of the file at point reviewed, or none when all already are."
  (interactive)
  (let ((file (or (ygg-git-compare-marks--file-at-point) (user-error "No diffed file at point"))))
    (ygg-git-compare-marks--toggle (ygg-git-compare-marks--hunks file) file)))

(defun ygg-git-compare-marks--next-here (marks &optional back)
  (let* ((here (line-beginning-position))
         (unreviewed (seq-filter
                      (lambda (h) (and (if back (< (oref h start) here) (> (oref h start) (point)))
                                       (not (gethash (ygg-git-compare-marks--key h) marks))))
                      (ygg-git-compare-marks--hunks))))
    (when-let* ((hunk (if back (car (last unreviewed)) (car unreviewed))))
      (magit-section-show (oref hunk parent))
      (goto-char (oref hunk start)))))

(defun ygg-git-compare-marks--next (marks &optional back)
  "Go to the first unreviewed hunk after point, or the last before it with
BACK, its file opened; nil when none.  Past the right pane's end, the list
moves on and the pane follows."
  (or (ygg-git-compare-marks--next-here marks back)
      (let ((list (ygg-git-compare-marks--home)))
        (unless (eq list (current-buffer))
          (with-current-buffer list
            (let ((window (get-buffer-window list)))
              (when window (goto-char (window-point window)))
              (when-let* ((at (ygg-git-compare-marks--next-here marks back)))
                (when window (set-window-point window at))
                (ygg-git-compare--follow list)
                at)))))))

;;;###autoload
(defun ygg-git-compare-next-unreviewed ()
  "Go to the next hunk not yet reviewed."
  (interactive)
  (unless (ygg-git-compare-marks--next (ygg-git-compare-marks--read))
    (user-error "No unreviewed hunk after point")))

;;;###autoload
(defun ygg-git-compare-previous-unreviewed ()
  "Go to the hunk before point not yet reviewed."
  (interactive)
  (unless (ygg-git-compare-marks--next (ygg-git-compare-marks--read) t)
    (user-error "No unreviewed hunk before point")))

;;;###autoload
(defun ygg-git-compare-toggle-unreviewed ()
  "Show only the hunks and files not yet reviewed, or everything again."
  (interactive)
  (let ((only (with-current-buffer (ygg-git-compare-marks--home)
                (setq ygg-git-compare-marks--unreviewed-only
                      (not ygg-git-compare-marks--unreviewed-only)))))
    (ygg-git-compare-marks--apply-all)
    (message (if only "Showing unreviewed hunks only" "Showing every hunk"))))

(provide 'ygg-git-compare-marks)
;;; ygg-git-compare-marks.el ends here
