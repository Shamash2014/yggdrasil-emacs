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

(defvar-local ygg-git-compare-marks--unreviewed-only nil
  "Whether this compare hides the hunks and files already reviewed.")

;;; Marks on disk

(defun ygg-git-compare-marks--path ()
  (expand-file-name ygg-git-compare-marks-file
                    (or (magit-gitdir nil t) (user-error "Not in a git repository"))))

(defun ygg-git-compare-marks--read ()
  "The repository's marks as a set, empty when the file is missing or unreadable."
  (let ((marks (make-hash-table :test #'equal))
        (keys (ignore-errors
                (with-temp-buffer
                  (insert-file-contents (ygg-git-compare-marks--path))
                  (read (current-buffer))))))
    (dolist (key (and (proper-list-p keys) keys) marks)
      (when (stringp key) (puthash key t marks)))))

(defun ygg-git-compare-marks--write (marks)
  (let ((print-length nil) (print-level nil))
    (with-temp-file (ygg-git-compare-marks--path)
      (prin1 (hash-table-keys marks) (current-buffer)))))

;;; Hunks

(defun ygg-git-compare-marks--key (hunk)
  "HUNK's file and its added and removed lines, hashed."
  (save-excursion
    (goto-char (oref hunk content))
    (let (changed)
      (while (< (point) (oref hunk end))
        (when (memq (char-after) '(?+ ?-))
          (push (buffer-substring-no-properties (point) (line-end-position)) changed))
        (forward-line))
      (secure-hash 'sha1 (string-join (cons (oref (oref hunk parent) value)
                                            (nreverse changed))
                                      "\n")))))

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

(defun ygg-git-compare-marks--overlay (section &rest props)
  (let ((o (make-overlay (oref section start) (oref section end))))
    (overlay-put o 'ygg-git-compare-marks t)
    (overlay-put o 'evaporate t)
    (while props (overlay-put o (pop props) (pop props)))
    o))

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
      (remove-overlays (point-min) (point-max) 'ygg-git-compare-marks t)
      (add-to-invisibility-spec 'ygg-git-compare-marks)
      (dolist (hunk (ygg-git-compare-marks--hunks))
        (let ((file (oref hunk parent)))
          (unless (assq file files) (push (list file t) files))
          (if (not (gethash (ygg-git-compare-marks--key hunk) marks))
              (setcar (cdr (assq file files)) nil)
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
    (ygg-git-compare-marks--apply)))

(add-hook 'ygg-git-compare-mode-hook #'ygg-git-compare-marks--setup)

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

(defun ygg-git-compare-marks--toggle (hunks)
  "Mark HUNKS reviewed, or unmark them all when every one already is."
  (let* ((marks (ygg-git-compare-marks--read))
         (keys (mapcar #'ygg-git-compare-marks--key hunks))
         (done (seq-every-p (lambda (k) (gethash k marks)) keys)))
    (dolist (key keys)
      (if done (remhash key marks) (puthash key t marks)))
    (ygg-git-compare-marks--write marks)
    (ygg-git-compare-marks--apply-all marks)
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
  (ygg-git-compare-marks--toggle
   (ygg-git-compare-marks--hunks
    (or (ygg-git-compare-marks--file-at-point) (user-error "No diffed file at point")))))

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
