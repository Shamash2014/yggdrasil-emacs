;;; ygg-git-compare-tour.el --- walk a compare in an agent's order -*- lexical-binding: t; -*-

;;; Commentary:
;; An agent orders the compared change into a short tour of steps, each
;; naming the hunks it covers and what to check there.  The compare shows
;; one step at a time, the rest folded; leaving a step marks its hunks
;; reviewed, and the hunks no step covers come last as one more step.  The
;; tour is kept beside the branch's review, `reviews/BRANCH.tour.json' in
;; the repository's common git directory, and follows the branch when its
;; head moves: a hunk is found again by its mark key, and a step that lost
;; one is stale and gives its hunks to the last step.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-compare-marks)
(require 'ygg-git-compare-comments)
(require 'ygg-git-compare-explain)

(defcustom ygg-git-compare-tour-instructions
  "Order this change into a tour a reviewer walks one step at a time.
Read the diff and the code at B yourself, then group the hunks by behaviour, \
not by file, and put the steps in the order that makes the change make \
sense: the core first, what depends on it after, tests last.  Every hunk \
belongs to one step.  Keep it to a handful of steps.
Hand the tour back by calling the MCP tool review_tour with dir %S and branch \
%S.  Each step has a title, a check (what the reviewer must confirm there), \
a risk (low, medium or high) and its hunks, each as file, start and end \
(the hunk's lines on that side) and side (new, or old for a removed hunk).
Steps carry notes only: do not propose comments here, and change no file, \
commit, branch or ref."
  "What a session ordering a compare into a tour is asked for, after the range.
Its two directives are the repository and the branch review_tour is called with."
  :type 'string
  :group 'ygg-git-compare)

(defvar-local ygg-git-compare-tour--steps nil
  "The tour's steps as resolved against this compare's diff.")

(defvar-local ygg-git-compare-tour--loaded nil
  "The base..head the steps were resolved for, or nil before the tour is read.")

(defvar-local ygg-git-compare-tour--index nil
  "The step shown, counted from 1, or nil off the tour.")

(defvar-local ygg-git-compare-tour--folds nil
  "The folds the compare had on entering the tour, as (KIND KEY . HIDDEN) entries.")

(defvar-local ygg-git-compare-tour--cache nil
  "The root section and the hunks by mark key built for it.")

;;; Where a tour is kept

(defun ygg-git-compare-tour--path (branch &optional dir)
  "The file BRANCH's tour is kept in, in DIR's repository."
  (let* ((gitdir (or (magit-gitdir dir t) (user-error "Not in a git repository")))
         (reviews (file-name-as-directory (expand-file-name "reviews" gitdir)))
         (file (expand-file-name
                (format "%s.tour.json"
                        (string-remove-prefix "branch " (ygg-git-compare--branch-review-key branch)))
                reviews)))
    (unless (string-prefix-p reviews file)
      (user-error "Branch name %S would keep its tour outside the repository's reviews" branch))
    file))

(defun ygg-git-compare-tour--key ()
  "The range this compare shows, as base..head."
  (format "%s..%s"
          (or (plist-get ygg-git-compare--a :diff) (plist-get ygg-git-compare--a :label))
          (or (plist-get ygg-git-compare--b :diff) (plist-get ygg-git-compare--b :label))))

(defun ygg-git-compare-tour--read (file)
  "The tour FILE keeps as a plist of :base, :head and :steps, or nil."
  (when (file-readable-p file)
    (ignore-errors
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (json-parse-buffer :object-type 'plist :array-type 'list
                           :null-object nil :false-object nil)))))

(defun ygg-git-compare-tour--plain (step)
  "STEP as it is kept: its hunks as vectors, nothing derived."
  (list :title (plist-get step :title)
        :check (plist-get step :check)
        :risk (plist-get step :risk)
        :hunks (vconcat
                (mapcar (lambda (h)
                          (apply #'append
                                 (mapcar (lambda (k)
                                           (when-let* ((v (plist-get h k))) (list k v)))
                                         '(:file :start :end :side :key))))
                        (plist-get step :hunks)))))

(defun ygg-git-compare-tour--write (file base head steps)
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert (decode-coding-string
               (json-serialize
                (append (and base (list :base base))
                        (and head (list :head head))
                        (list :steps (vconcat (mapcar #'ygg-git-compare-tour--plain steps))))
                :null-object nil :false-object nil)
               'utf-8)
              "\n"))))

;;; Hunks of the diff

(defun ygg-git-compare-tour--keyed ()
  "This diff's hunks by mark key, built once per drawn diff."
  (unless (eq (car ygg-git-compare-tour--cache) magit-root-section)
    (let ((keyed (make-hash-table :test #'equal)))
      (dolist (hunk (ygg-git-compare-marks--hunks))
        (puthash (ygg-git-compare-marks--key hunk) hunk keyed))
      (setq ygg-git-compare-tour--cache (cons magit-root-section keyed))))
  (cdr ygg-git-compare-tour--cache))

(defun ygg-git-compare-tour--span (hunk side)
  "HUNK's first and last line on SIDE."
  (pcase (if (equal side "old") (oref hunk from-range) (oref hunk to-range))
    (`(,start ,len . ,_) (cons start (+ start (max 0 (1- len)))))))

(defun ygg-git-compare-tour--entry (hunk side key)
  (let ((span (ygg-git-compare-tour--span hunk side)))
    (list :file (oref (oref hunk parent) value) :start (car span) :end (cdr span)
          :side side :key key)))

(defun ygg-git-compare-tour--file-p (hunk file)
  (let ((section (oref hunk parent)))
    (or (equal (oref section value) file)
        (equal (ignore-errors (oref section source)) file))))

(defun ygg-git-compare-tour--sorted (entries)
  "ENTRIES by file, then first line."
  (seq-sort-by (lambda (e) (cons (plist-get e :file) (plist-get e :start)))
               (lambda (a b) (or (string< (car a) (car b))
                                 (and (equal (car a) (car b)) (< (cdr a) (cdr b)))))
               entries))

(defun ygg-git-compare-tour--found (entry keyed)
  "The entries ENTRY stands for in this diff: itself again by its key, else the
hunks its file and lines overlap.  Nil when the diff has none."
  (let ((side (or (plist-get entry :side) "new")))
    (if-let* ((key (plist-get entry :key)))
        (when-let* ((hunk (gethash key keyed)))
          (list (ygg-git-compare-tour--entry hunk side key)))
      (let (found)
        (maphash
         (lambda (key hunk)
           (when-let* (((ygg-git-compare-tour--file-p hunk (plist-get entry :file)))
                       (span (ygg-git-compare-tour--span hunk side))
                       ((<= (car span) (plist-get entry :end)))
                       ((>= (cdr span) (plist-get entry :start))))
             (push (ygg-git-compare-tour--entry hunk side key) found)))
         keyed)
        (ygg-git-compare-tour--sorted found)))))

(defun ygg-git-compare-tour--resolve (steps)
  "STEPS against this diff, each hunk found again; a step with one gone is :stale."
  (let ((keyed (ygg-git-compare-tour--keyed)))
    (mapcar (lambda (step)
              (let (stale hunks)
                (dolist (entry (plist-get step :hunks))
                  (if-let* ((found (ygg-git-compare-tour--found entry keyed)))
                      (setq hunks (append hunks found))
                    (setq stale t hunks (append hunks (list entry)))))
                (list :title (plist-get step :title) :check (plist-get step :check)
                      :risk (plist-get step :risk) :hunks hunks :stale stale)))
            steps)))

(defun ygg-git-compare-tour--leftover ()
  "The step of hunks no live step covers, or nil when every hunk is in one."
  (let ((covered (make-hash-table :test #'equal))
        left)
    (dolist (step ygg-git-compare-tour--steps)
      (unless (plist-get step :stale)
        (dolist (entry (plist-get step :hunks))
          (puthash (plist-get entry :key) t covered))))
    (maphash (lambda (key hunk)
               (unless (gethash key covered)
                 (push (ygg-git-compare-tour--entry hunk "new" key) left)))
             (ygg-git-compare-tour--keyed))
    (when left
      (list :title (format "not in any step (%d hunk%s)" (length left)
                           (if (= (length left) 1) "" "s"))
            :hunks (ygg-git-compare-tour--sorted left)
            :leftover t))))

(defun ygg-git-compare-tour-steps ()
  "The tour's steps and, last, the leftover step."
  (append ygg-git-compare-tour--steps
          (when ygg-git-compare-tour--steps
            (when-let* ((left (ygg-git-compare-tour--leftover)))
              (list left)))))

(defun ygg-git-compare-tour--sections (step)
  "The hunk sections STEP covers here."
  (let ((keyed (ygg-git-compare-tour--keyed)))
    (delq nil (mapcar (lambda (e) (gethash (plist-get e :key) keyed))
                      (unless (plist-get step :stale) (plist-get step :hunks))))))

;;; Loading and keeping

(defun ygg-git-compare-tour--branch ()
  (or (ygg-git-compare--b-branch) (user-error "A tour needs side B to be a branch")))

(defun ygg-git-compare-tour--base-p (base)
  "Whether a tour kept for BASE, or for no base yet, belongs to this compare."
  (or (null base) (equal base (plist-get ygg-git-compare--a :diff))))

(defvar ygg-git-compare-tour--pending (make-hash-table :test #'equal)
  "The base each tour file was last asked for, by file.")

(defun ygg-git-compare-tour--install (steps &optional replace)
  "Make STEPS this compare's tour, found again in its diff, and keep them.
The file is written only when there are steps and it is this compare's to
write: its base is this compare's, or none yet, or REPLACE says the tour is new."
  (setq ygg-git-compare-tour--steps (ygg-git-compare-tour--resolve steps)
        ygg-git-compare-tour--loaded (ygg-git-compare-tour--key))
  (when-let* ((steps ygg-git-compare-tour--steps)
              (branch (ygg-git-compare--b-branch))
              (file (ygg-git-compare-tour--path branch default-directory))
              ((or replace
                   (ygg-git-compare-tour--base-p
                    (plist-get (ygg-git-compare-tour--read file) :base)))))
    (ygg-git-compare-tour--write
     file (plist-get ygg-git-compare--a :diff) (plist-get ygg-git-compare--b :diff) steps)))

(defun ygg-git-compare-tour--ensure ()
  "Read this branch's tour for this range once."
  (unless (equal ygg-git-compare-tour--loaded (ygg-git-compare-tour--key))
    (let* ((branch (ygg-git-compare--b-branch))
           (kept (and branch (ygg-git-compare-tour--read
                              (ygg-git-compare-tour--path branch default-directory)))))
      (if (and (plist-get kept :steps)
               (ygg-git-compare-tour--base-p (plist-get kept :base)))
          (ygg-git-compare-tour--install (plist-get kept :steps))
        (setq ygg-git-compare-tour--steps nil
              ygg-git-compare-tour--loaded (ygg-git-compare-tour--key))))))

(defun ygg-git-compare-tour--reanchor ()
  "After a redraw, find the tour's hunks again; a step that lost one is stale."
  (when ygg-git-compare-tour--loaded
    (let ((index ygg-git-compare-tour--index))
      (setq ygg-git-compare-tour--cache nil
            ygg-git-compare-tour--index nil
            ygg-git-compare--tour-status nil)
      (ygg-git-compare-tour--install ygg-git-compare-tour--steps)
      (when index
        (ygg-git-compare-tour-goto (min index (length (ygg-git-compare-tour-steps))))))))

(add-hook 'ygg-git-compare-redraw-hook #'ygg-git-compare-tour--reanchor)

;;;###autoload
(defun ygg-git-compare-tour-receive (dir branch steps author)
  "Keep STEPS, AUTHOR's tour of BRANCH in DIR's repository, shown in the compare
of BRANCH.  Answer (COUNT . STALE), the steps and those naming hunks the diff
lacks."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (key (ygg-git-compare--branch-review-key branch))
         (file (ygg-git-compare--store-file))
         (path (ygg-git-compare-tour--path branch))
         (base (gethash path ygg-git-compare-tour--pending))
         (steps (mapcar (lambda (s)
                          (list :title (plist-get s :title) :check (plist-get s :check)
                                :risk (plist-get s :risk) :hunks (plist-get s :hunks)))
                        steps))
         installed stale)
    (remhash path ygg-git-compare-tour--pending)
    (dolist (buffer (buffer-list))
      (when (equal (buffer-local-value 'ygg-git-compare--store buffer) (cons file key))
        (with-current-buffer buffer
          (ygg-git-compare-tour-leave)
          (if (or (null base) (equal base (plist-get ygg-git-compare--a :diff)))
              (progn (ygg-git-compare-tour--install steps t)
                     (setq base (plist-get ygg-git-compare--a :diff)
                           stale (seq-count (lambda (s) (plist-get s :stale))
                                            ygg-git-compare-tour--steps)
                           installed t))
            (setq ygg-git-compare-tour--steps nil
                  ygg-git-compare-tour--loaded nil)))))
    (unless installed
      (ygg-git-compare-tour--write path base nil steps))
    (message "%d tour step%s from %s on %s — t to walk them"
             (length steps) (if (= (length steps) 1) "" "s") author branch)
    (cons (length steps) (or stale 0))))

;;; Walking

(defun ygg-git-compare-tour--status (n steps)
  (let ((step (nth (1- n) steps)))
    (concat (format "step %d/%d — %s" n (length steps) (plist-get step :title))
            (when-let* ((risk (plist-get step :risk))) (format "  risk: %s" risk))
            (when-let* ((check (plist-get step :check))) (format "  check: %s" check))
            (when (plist-get step :stale) "  stale: t asks for a new tour"))))

(defun ygg-git-compare-tour--files ()
  (seq-filter (lambda (s) (eq (oref s type) 'file)) (oref magit-root-section children)))

(defun ygg-git-compare-tour--narrow (hunks)
  "Open HUNKS and fold every other hunk and file."
  (let ((magit-section-cache-visibility nil))
    (dolist (file (ygg-git-compare-tour--files))
      (if (seq-some (lambda (h) (memq h hunks)) (oref file children))
          (progn (magit-section-show file)
                 (dolist (h (oref file children))
                   (if (memq h hunks) (magit-section-show h) (magit-section-hide h))))
        (magit-section-hide file)))))

(defun ygg-git-compare-tour--reveal (hunks)
  "Show HUNKS even where the unreviewed-only filter hides them."
  (dolist (hunk hunks)
    (dolist (o (overlays-at (oref hunk start)))
      (when (overlay-get o 'ygg-git-compare-marks)
        (overlay-put o 'invisible nil)))))

(defun ygg-git-compare-tour--save-folds ()
  "Remember which files and hunks are folded, once per walk."
  (unless ygg-git-compare-tour--folds
    (let (folds)
      (dolist (file (ygg-git-compare-tour--files))
        (push (cons (cons 'file (oref file value)) (and (oref file hidden) t)) folds))
      (maphash (lambda (key hunk)
                 (push (cons (cons 'hunk key) (and (oref hunk hidden) t)) folds))
               (ygg-git-compare-tour--keyed))
      (setq ygg-git-compare-tour--folds (or folds t)))))

(defun ygg-git-compare-tour--restore-folds ()
  "Fold files and hunks as they were before the tour, the default for new ones."
  (let ((magit-section-cache-visibility nil)
        (folds (and (consp ygg-git-compare-tour--folds) ygg-git-compare-tour--folds)))
    (setq ygg-git-compare-tour--folds nil)
    (dolist (file (ygg-git-compare-tour--files))
      (when-let* ((fold (assoc (cons 'file (oref file value)) folds)))
        (if (cdr fold) (magit-section-hide file) (magit-section-show file))))
    (maphash (lambda (key hunk)
               (when-let* ((fold (assoc (cons 'hunk key) folds)))
                 (if (cdr fold) (magit-section-hide hunk) (magit-section-show hunk))))
             (ygg-git-compare-tour--keyed))))

(defun ygg-git-compare-tour-goto (n)
  "Show step N of the tour: its hunks open, the rest folded."
  (let* ((steps (ygg-git-compare-tour-steps))
         (step (or (nth (1- n) steps) (user-error "No step %d" n)))
         (hunks (ygg-git-compare-tour--sections step)))
    (ygg-git-compare-tour--save-folds)
    (setq ygg-git-compare-tour--index n
          ygg-git-compare--tour-status (ygg-git-compare-tour--status n steps))
    (ygg-git-compare--header)
    (ygg-git-compare-marks--apply)
    (ygg-git-compare-tour--reveal hunks)
    (ygg-git-compare-tour--narrow hunks)
    (when hunks (goto-char (oref (car hunks) start)))
    n))

(defun ygg-git-compare-tour--reviewed (step)
  "Mark the hunks of STEP reviewed, none already marked off again."
  (let ((hunks (ygg-git-compare-tour--sections step))
        (marks (ygg-git-compare-marks--read)))
    (when (and hunks (seq-some (lambda (h)
                                 (not (gethash (ygg-git-compare-marks--key h) marks)))
                               hunks))
      (ygg-git-compare-marks--toggle hunks))))

;;;###autoload
(defun ygg-git-compare-tour-leave ()
  "Leave the tour: the folds the compare had before it come back."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (when ygg-git-compare-tour--index
      (setq ygg-git-compare-tour--index nil
            ygg-git-compare--tour-status nil)
      (ygg-git-compare--header)
      (ygg-git-compare--fold)
      (ygg-git-compare-tour--restore-folds)
      (ygg-git-compare-marks--apply))))

(defun ygg-git-compare-tour--move (delta)
  (with-current-buffer (ygg-git-compare--list)
    (ygg-git-compare-tour--branch)
    (ygg-git-compare-tour--ensure)
    (let* ((steps (or (ygg-git-compare-tour-steps)
                      (user-error "No tour for this range; t asks an agent for one")))
           (here ygg-git-compare-tour--index)
           (to (cond (here (+ here delta))
                     ((> delta 0) 1)
                     (t (length steps)))))
      (cond ((< to 1) (user-error "First step"))
            ((> to (length steps))
             (ygg-git-compare-tour--reviewed (nth (1- here) steps))
             (ygg-git-compare-tour-leave)
             (message "End of the tour"))
            (t (when here (ygg-git-compare-tour--reviewed (nth (1- here) steps)))
               (ygg-git-compare-tour-goto to))))))

;;;###autoload
(defun ygg-git-compare-tour-next ()
  "Go to the next step of the tour, marking the step left reviewed."
  (interactive)
  (ygg-git-compare-tour--move 1))

;;;###autoload
(defun ygg-git-compare-tour-previous ()
  "Go back a step of the tour, marking the step left reviewed."
  (interactive)
  (ygg-git-compare-tour--move -1))

;;;###autoload
(defun ygg-git-compare-tour ()
  "Walk the branch's tour from its first step, or ask an agent to order one.
Asks when there is no tour, a step went stale or the tour is already
being walked."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (let ((branch (ygg-git-compare-tour--branch)))
      (ygg-git-compare-tour--ensure)
      (if (and ygg-git-compare-tour--steps
               (not ygg-git-compare-tour--index)
               (not (seq-some (lambda (s) (plist-get s :stale)) ygg-git-compare-tour--steps)))
          (ygg-git-compare-tour-goto 1)
        (puthash (ygg-git-compare-tour--path branch default-directory)
                 (plist-get ygg-git-compare--a :diff) ygg-git-compare-tour--pending)
        (ygg-git-compare-explain--ask
         "tour" (format ygg-git-compare-tour-instructions
                        (ygg-git-compare-agent-path ygg-git-compare--root) branch))))))

(provide 'ygg-git-compare-tour)
;;; ygg-git-compare-tour.el ends here
