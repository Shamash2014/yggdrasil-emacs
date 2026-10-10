;;; aob-edits-qf.el --- the files an agent session edited, in the quickfix -*- lexical-binding: t; -*-

;;; Commentary:
;; Rows come only from what the agent told us in tool calls of kind edit:
;; their diff content and locations.  Reverting undoes the agent's
;; replacements newest first, each only where its new text occurs exactly once.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'aob)

(declare-function ygg-qf-define-kind "layer-quickfix")
(declare-function ygg-qf-show-kind "layer-quickfix")
(declare-function ygg-qf-kind-refresh "layer-quickfix")
(declare-function ygg-qf-kind-target-id "layer-quickfix")
(defvar aob--views)

(defun aob-edits--root (s)
  (file-name-as-directory
   (expand-file-name (or (aob-session-dir s) (aob-session-project s) default-directory))))

(defun aob-edits--abs (s path)
  (expand-file-name path (aob-edits--root s)))

(defun aob-edits--rel (s file)
  (file-relative-name file (aob-edits--root s)))

(defun aob-edits--collect-files (s)
  "The files S edited as plists in first-touched order.
Each holds :file, :edits (diffs oldest first), :line, :status and :calls."
  (let (order (table (make-hash-table :test #'equal)))
    (dolist (ev (reverse (aob-session-events s)))
      (when (and (eq (plist-get ev :type) 'tool)
                 (equal (plist-get ev :kind) "edit"))
        (let ((status (plist-get ev :status))
              (diffs (seq-filter (lambda (c) (equal (plist-get c :type) "diff"))
                                 (plist-get ev :content)))
              (locs (plist-get ev :locations)))
          (dolist (path (delete-dups
                         (delq nil (append (mapcar (lambda (c) (plist-get c :path)) diffs)
                                           (mapcar (lambda (l) (plist-get l :path))
                                                   (append locs nil))))))
            (let* ((file (aob-edits--abs s path))
                   (cell (or (gethash file table)
                             (let ((new (list :file file :calls 0)))
                               (push file order)
                               (puthash file new table)
                               new))))
              (plist-put cell :calls (1+ (plist-get cell :calls)))
              (plist-put cell :status status)
              (unless (plist-get cell :line)
                (when-let* ((loc (seq-find (lambda (l) (equal (plist-get l :path) path))
                                           (append locs nil)))
                            (line (plist-get loc :line)))
                  (plist-put cell :line line)))
              (unless (equal status "failed")
                (dolist (d diffs)
                  (when (equal (plist-get d :path) path)
                    (plist-put cell :edits
                               (append (plist-get cell :edits) (list d)))))))))))
    (mapcar (lambda (f) (gethash f table)) (nreverse order))))

(defun aob-edits--count (text)
  (if (or (null text) (string-empty-p text))
      0
    (+ (cl-count ?\n text) (if (string-suffix-p "\n" text) 0 1))))

(defun aob-edits--stat (cell)
  (when-let* ((edits (plist-get cell :edits)))
    (format "+%d −%d"
            (cl-loop for d in edits sum (aob-edits--count (plist-get d :newText)))
            (cl-loop for d in edits sum (aob-edits--count (plist-get d :oldText))))))

(defun aob-edits--reviewed-p (s cell)
  (let ((seen (assoc (plist-get cell :file) (aob-session-ref s :edits-reviewed))))
    (and seen (equal (cdr seen) (aob-edits--signature cell)))))

(defun aob-edits--signature (cell)
  (sxhash-equal (plist-get (car (last (plist-get cell :edits))) :newText)))

(defun aob-edits--rows (s)
  (let ((root (file-name-as-directory (file-truename (aob-edits--root s)))))
    (mapcar
   (lambda (cell)
     (let ((file (plist-get cell :file)))
       (list (cons (aob-session-id s) file)
             (format "%s  %s%s" (aob-edits--rel s file)
                     (or (aob-edits--stat cell) "—")
                     (if (> (plist-get cell :calls) 1)
                         (format "  ×%d" (plist-get cell :calls)) ""))
             (string-join
              (delq nil (list (aob-session-name s)
                              (or (plist-get cell :status) "?")
                              (when (aob-edits--reviewed-p s cell) "accepted")
                              (unless (aob-edits--inside-p s file root) "outside project")))
              " · "))))
   (aob-edits--collect-files s))))

(defun aob-edits--cell (id)
  "The (SESSION . CELL) the row ID stands for."
  (let* ((s (or (and id (aob-session-get (car id))) (user-error "aob: that session is gone")))
         (cell (seq-find (lambda (c) (equal (plist-get c :file) (cdr id)))
                         (aob-edits--collect-files s))))
    (cons s (or cell (user-error "aob: no edit of that file is left")))))

(defun aob-edits-visit (id)
  "Open the file of row ID at its first changed place."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let* ((`(,_ . ,cell) (aob-edits--cell id))
               (file (plist-get cell :file)))
    (unless (file-exists-p file) (user-error "aob: %s is gone" (abbreviate-file-name file)))
    (find-file-other-window file)
    (goto-char (point-min))
    (let ((line (plist-get cell :line))
          (probe (when-let* ((new (plist-get (car (plist-get cell :edits)) :newText)))
                   (car (split-string new "\n")))))
      (cond (line (forward-line (1- line)))
            ((and probe (not (string-empty-p probe))
                  (search-forward probe nil t))
             (beginning-of-line))))))

(defun aob-edits--diff-text (rel old new)
  (let ((a (make-temp-file "aob-edits-a"))
        (b (make-temp-file "aob-edits-b")))
    (unwind-protect
        (progn
          (write-region (or old "") nil a nil 'silent)
          (write-region (or new "") nil b nil 'silent)
          (with-output-to-string
            (call-process "diff" nil standard-output nil "-u"
                          "--label" (concat "a/" rel) "--label" (concat "b/" rel) a b)))
      (delete-file a)
      (delete-file b))))

(defun aob-edits-compare (id)
  "Show what the agent changed in the file of row ID, edit by edit, as a diff."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let* ((`(,s . ,cell) (aob-edits--cell id))
               (rel (aob-edits--rel s (plist-get cell :file))))
    (unless (plist-get cell :edits) (user-error "aob: the agent sent no diff for %s" rel))
    (let ((buf (get-buffer-create (format "*aob-edits: %s*" rel))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (d (plist-get cell :edits))
            (insert (aob-edits--diff-text rel (plist-get d :oldText) (plist-get d :newText))))
          (diff-mode)
          (goto-char (point-min))))
      (pop-to-buffer buf))))

(defun aob-edits-accept (id)
  "Mark the edits of the file of row ID as reviewed."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let ((`(,s . ,cell) (aob-edits--cell id)))
    (aob-session-put s :edits-reviewed
                     (cons (cons (plist-get cell :file) (aob-edits--signature cell))
                           (assoc-delete-all (plist-get cell :file)
                                             (aob-session-ref s :edits-reviewed))))
    (aob--dirty)
    (message "aob: accepted %s" (aob-edits--rel s (plist-get cell :file)))))

(defun aob-edits--file-text (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun aob-edits--inside-p (s file &optional root)
  (string-prefix-p (or root (file-name-as-directory (file-truename (aob-edits--root s))))
                   (file-truename file)))

(defun aob-edits--busy-p (s)
  (memq (aob-session-state s) '(working blocked starting)))

(defun aob-edits--empty-p (text)
  (or (null text) (string-empty-p text)))

(defun aob-edits--occurrences (needle text)
  (let ((n 0) (from 0))
    (while-let ((at (string-search needle text from)))
      (cl-incf n)
      (setq from (1+ at)))
    n))

(defun aob-edits--undo-all (edits text)
  "(REASON . TEXT): TEXT with EDITS undone newest first, REASON naming the edit that cannot be."
  (let ((i (length edits)) (bad nil))
    (while (and edits (not bad))
      (let* ((e (car (last edits)))
             (new (plist-get e :newText))
             (n (if (aob-edits--empty-p new) 0 (aob-edits--occurrences new text))))
        (setq edits (butlast edits))
        (cond ((aob-edits--empty-p new)
               (setq bad (format "edit %d added no text to find" i)))
              ((= n 0)
               (setq bad (format "edit %d: can't find the agent's text — the file changed since" i)))
              ((> n 1)
               (setq bad (format "edit %d: ambiguous — the agent's text appears %d times" i n)))
              (t (let ((at (string-search new text)))
                   (setq text (concat (substring text 0 at)
                                      (or (plist-get e :oldText) "")
                                      (substring text (+ at (length new))))))))
        (cl-decf i)))
    (cons bad text)))

(defun aob-edits--current-text (file)
  (if-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
    (aob-edits--file-text file)))

(defun aob-edits--revert-plan (s cell)
  "(restore FILE TEXT) or (delete FILE) for CELL, or a string saying why not."
  (let* ((file (plist-get cell :file))
         (edits (plist-get cell :edits))
         (buf (find-buffer-visiting file)))
    (cond
     ((aob-edits--busy-p s) "the session is still running a turn")
     ((null edits) "the agent sent no diff for it")
     ((not (file-exists-p file)) "the file is gone")
     ((not (aob-edits--inside-p s file)) "it is outside the session's project")
     ((and buf (buffer-modified-p buf)) "its buffer has unsaved changes")
     ((aob-edits--empty-p (plist-get (car edits) :oldText))
      (if (equal (aob-edits--current-text file) (plist-get (car (last edits)) :newText))
          (list 'delete file)
        "the agent created it and it has changed since"))
     (t (pcase-let ((`(,bad . ,text) (aob-edits--undo-all edits (aob-edits--current-text file))))
          (or bad (list 'restore file text)))))))

(defvar apheleia-mode)

(defun aob-edits--apply-restore (file text)
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (with-undo-amalgamate
        (replace-region-contents (point-min) (point-max) (lambda () text))))
    (let ((apheleia-mode nil))
      (save-buffer))))

(defun aob-edits--revert-cell (s cell)
  (let* ((file (plist-get cell :file))
         (rel (aob-edits--rel s file))
         (plan (aob-edits--revert-plan s cell))
         (delete (and (listp plan) (eq (car plan) 'delete))))
    (when (stringp plan) (user-error "aob: not reverting %s: %s" rel plan))
    (unless (y-or-n-p (if delete
                          (format "Delete file %s the agent created? " rel)
                        (format "Undo the agent's edits to %s? " rel)))
      (user-error "aob: kept %s: you declined" rel))
    (unless (equal plan (aob-edits--revert-plan s cell))
      (user-error "aob: not reverting %s: it changed while you were asked" rel))
    (if delete
        (progn (when-let* ((buf (find-buffer-visiting file))) (kill-buffer buf))
               (delete-file file))
      (aob-edits--apply-restore file (nth 2 plan)))
    (aob--dirty)
    (message "aob: reverted %s" rel)
    t))

(defun aob-edits-revert (id)
  "Undo the agent's edits to the file of row ID, after asking."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let ((`(,s . ,cell) (aob-edits--cell id)))
    (aob-edits--revert-cell s cell)))

(defun aob-edits--report (done kept failed)
  (let* ((lines (append (reverse kept) (reverse failed)))
         (head (format "aob: reverted %d, kept %d, failed %d"
                       done (length kept) (length failed))))
    (if (<= (length lines) 3)
        (message "%s" (string-join (cons head lines) "; "))
      (with-current-buffer (get-buffer-create "*aob-edits: kept*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert head "\n\n" (string-join lines "\n") "\n")
          (special-mode)
          (goto-char (point-min)))
        (display-buffer (current-buffer)))
      (message "%s" head))))

(defun aob-edits-revert-all (id)
  "Undo the agent's edits in every file of row ID's session, asking for each."
  (interactive (list (ygg-qf-kind-target-id)))
  (let ((s (car (aob-edits--cell id)))
        (done 0) kept failed)
    (unwind-protect
        (dolist (cell (aob-edits--collect-files s))
          (condition-case err
              (when (aob-edits--revert-cell s cell) (cl-incf done))
            (user-error (push (substring (cadr err) (length "aob: ")) kept))
            (error (push (cadr err) failed))))
      (aob-edits--report done kept failed))))

(defvar aob-edits-map
  (let ((m (make-sparse-keymap)))
    (define-key m "v" #'aob-edits-visit)
    (define-key m "=" #'aob-edits-compare)
    (define-key m "a" #'aob-edits-accept)
    (define-key m "x" #'aob-edits-revert)
    (define-key m "X" #'aob-edits-revert-all)
    m)
  "What embark offers on an edited-file row of the quickfix.")

(defun aob-edits--live-p (s)
  (eq s (aob-session-get (aob-session-id s))))

(defun aob-edits--arm (buf token _s)
  (letrec ((render (lambda ()
                     (ignore-errors
                       (unless (ygg-qf-kind-refresh buf token)
                         (funcall disarm)))))
           (disarm (lambda ()
                     (when (eq (alist-get buf aob--views nil nil #'eq) render)
                       (setq aob--views (assq-delete-all buf aob--views))))))
    (aob-register-view buf render)
    disarm))

(with-eval-after-load 'layer-quickfix
  (ygg-qf-define-kind 'edits
                      :collect #'aob-edits--rows
                      :action #'aob-edits-visit
                      :map 'aob-edits-map
                      :arm #'aob-edits--arm
                      :live-p #'aob-edits--live-p
                      :glyph "±"))

;;;###autoload
(defun aob-edits (s)
  "Collect the files session S edited into the quickfix.
RET and v open a file, = shows the diff, a accepts, x undoes the
agent's edits and X does so for every file."
  (interactive (list (aob-target)))
  (unless (aob-edits--collect-files s)
    (user-error "aob: %s has edited nothing" (aob-session-name s)))
  (require 'layer-quickfix)
  (let ((default-directory (aob-edits--root s)))
    (ygg-qf-show-kind 'edits s)))

(provide 'aob-edits-qf)
;;; aob-edits-qf.el ends here
