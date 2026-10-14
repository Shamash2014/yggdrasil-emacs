;;; ygg-git-compare-context.el --- more lines around a compare's hunks -*- lexical-binding: t; -*-

;;; Commentary:
;; Raising the diff's context draws lines outside the changes, and they
;; take comments like any other line of a hunk.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-compare-comments)

(defcustom ygg-git-compare-context-step 10
  "Lines of context that one press of more or less context adds or removes."
  :type 'natnum)

(defconst ygg-git-compare-context--whole 1000000
  "Lines of context that draw a whole file.")

(defun ygg-git-compare-context--default ()
  (or (ygg-git-compare--default-context) 3))

(defun ygg-git-compare-context--current ()
  (with-current-buffer (ygg-git-compare--list)
    (or ygg-git-compare--context (ygg-git-compare-context--default))))

(defun ygg-git-compare-context--hunk-at-point ()
  (let ((section (magit-current-section)))
    (while (and section (not (magit-section-match 'hunk section)))
      (setq section (oref section parent)))
    (and section (oref section from-range) (oref section to-range) section)))

(defun ygg-git-compare-context--spot ()
  "Point as (PATH SIDE LINE) on a diff line, (PATH) on a file, else nil."
  (let ((hunk (ygg-git-compare-context--hunk-at-point)))
    (if hunk
        (let ((entry (seq-find (lambda (e) (= (car e) (line-beginning-position)))
                               (ygg-git-compare--hunk-lines hunk))))
          (if entry
              (list (oref (oref hunk parent) value) (nth 1 entry) (nth 2 entry))
            (list (oref (oref hunk parent) value))))
      (when-let* ((file (magit-file-at-point)))
        (list file)))))

(defun ygg-git-compare-context--file-section (path)
  (seq-find (lambda (s) (and (eq (oref s type) 'file)
                             (or (equal (oref s value) path)
                                 (and (slot-exists-p s 'source) (equal (oref s source) path)))))
            (oref magit-root-section children)))

(defun ygg-git-compare-context--marker (path side line)
  "A marker on the diff line of PATH's SIDE and LINE drawn in this buffer."
  (when-let* ((file (ygg-git-compare-context--file-section path)))
    (catch 'found
      (dolist (hunk (oref file children))
        (when (and (magit-section-match 'hunk hunk) (oref hunk from-range) (oref hunk to-range))
          (dolist (entry (ygg-git-compare--hunk-lines hunk))
            (when (if (eq side 'old)
                      (or (and (eq (nth 1 entry) 'old) (eql (nth 2 entry) line))
                          (eql (nth 3 entry) line))
                    (and (eq (nth 1 entry) 'new) (eql (nth 2 entry) line)))
              (throw 'found (copy-marker (car entry))))))))))

(defun ygg-git-compare-context--drawn-in (path)
  "The buffer that draws PATH's hunks: this compare, or its file pane when
the compare is too large to wash."
  (let ((file (ygg-git-compare-context--file-section path)))
    (if (and file (not (seq-some (lambda (s) (magit-section-match 'hunk s))
                                 (oref file children))))
        (progn (ygg-git-compare--show-file path)
               (get-buffer (ygg-git-compare--file-buffer-name)))
      (current-buffer))))

(defun ygg-git-compare-context--distance (path side line)
  "How many lines PATH's SIDE LINE lies from the nearest change, nil when the
file has none."
  (let* ((plan ygg-git-compare--plan)
         (default-directory (plist-get plan :dir))
         (magit-git-global-arguments (ygg-git-compare--global-args))
         (nearest nil))
    (dolist (text (apply #'magit-git-lines "diff" "--no-color" "--no-ext-diff" "-U0"
                         (append (ygg-git-compare--diff-args plan) (list "--" path))))
      (when (string-match "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@" text)
        (let* ((old (eq side 'old))
               (first (string-to-number (match-string (if old 1 3) text)))
               (count-text (match-string (if old 2 4) text))
               (count (if count-text (string-to-number count-text) 1))
               (distance (cond ((zerop count) (if (<= line first) (1+ (- first line)) (- line first)))
                               ((< line first) (- first line))
                               ((> line (+ first count -1)) (- line first count -1))
                               (t 0))))
          (setq nearest (if nearest (min nearest distance) distance)))))
    nearest))

(defun ygg-git-compare-context--restore (spot)
  (when-let* ((pos (pcase spot
                     (`(,path ,side ,line) (ygg-git-compare-context--marker path side line))
                     (`(,path) (when-let* ((file (ygg-git-compare-context--file-section path)))
                                 (oref file start))))))
    (goto-char pos)
    (ygg-git-compare--reveal)
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window (point)))))

(defun ygg-git-compare-context--set (n)
  "Draw the compare with N lines of context, point kept on its file line."
  (let ((origin (current-buffer))
        (list (ygg-git-compare--list))
        (spot (ygg-git-compare-context--spot)))
    (with-current-buffer list
      (setq ygg-git-compare--context (and (/= n (ygg-git-compare-context--default)) n))
      (ygg-git-compare--redraw)
      (ygg-git-compare-context--restore spot)
      (ygg-git-compare--follow list))
    (when (and (not (eq origin list)) (buffer-live-p origin))
      (with-current-buffer origin (ygg-git-compare-context--restore spot)))))

;;;###autoload
(defun ygg-git-compare-expand-context ()
  "Draw more lines around each hunk, to comment on lines outside the changes."
  (interactive)
  (ygg-git-compare-context--set
   (+ (ygg-git-compare-context--current) ygg-git-compare-context-step)))

;;;###autoload
(defun ygg-git-compare-shrink-context ()
  "Draw fewer lines around each hunk, down to magit's own."
  (interactive)
  (ygg-git-compare-context--set
   (max (ygg-git-compare-context--default)
        (- (ygg-git-compare-context--current) ygg-git-compare-context-step))))

;;;###autoload
(defun ygg-git-compare-show-line (path side line)
  "A marker on PATH's SIDE (`old' or `new') LINE in the compare's diff, with
just enough context drawn for it; nil when the file has no such line."
  (with-current-buffer (ygg-git-compare--list)
    (cl-flet ((find ()
                (with-current-buffer (ygg-git-compare-context--drawn-in path)
                  (ygg-git-compare-context--marker path side line))))
      (or (find)
          (when-let* ((distance (ygg-git-compare-context--distance path side line)))
            (ygg-git-compare-context--set
             (max distance (ygg-git-compare-context--current)))
            (or (find)
                (progn (ygg-git-compare-context--set ygg-git-compare-context--whole)
                       (find))))))))

(provide 'ygg-git-compare-context)
;;; ygg-git-compare-context.el ends here
