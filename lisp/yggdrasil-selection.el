;;; yggdrasil-selection.el --- Yggdrasil selection model -*- lexical-binding: t; -*-

;; Built-ins wrapped: region (point/mark), transient-mark-mode, overlays,
;; markers, change groups (prepare/activate/undo-amalgamate/accept),
;; with-undo-amalgamate, while-no-input.
;; Custom: primary-region + secondary-overlay model, multi-selection set
;; operations, post-command normalization, insert-echo mirroring.

;;; Code:

(require 'yggdrasil-core)
(require 'cl-lib)
(require 'seq)

(declare-function ygg-percent "yggdrasil-motions" (&optional _n))

(defvar-local ygg--echo-spans nil
  "((START-MARKER . END-MARKER) ...) for secondary cursors during insert.")
(defvar-local ygg--echo-primary nil
  "Start marker of the primary span during insert.")
(defvar-local ygg--echo-last nil)
(defvar-local ygg--echo-cursor-ovs nil)

(defvar-local ygg--insert-group nil
  "Change group making the insert session in progress one undo step.")
(defvar-local ygg--insert-anchor nil
  "Position the recorded edits of the insert session are relative to.
It moves with the text like a marker that stays before an insertion.")
(defvar-local ygg--insert-changes nil
  "Edits of the insert session so far, newest first, as
\(OFFSET DELETED TEXT): at anchor plus OFFSET, DELETED chars were
replaced by TEXT.")
(defvar-local ygg--insert-count nil
  "(COUNT . OPEN-LINE) for a counted insert session, or nil.
OPEN-LINE, when non-nil, opens the line each extra copy goes on.")
(defvar ygg--insert-unrecorded nil
  "Non-nil while edits are made that the insert session must not record.")
(defvar ygg--repeat-count nil
  "Count a dot replay hands to the insert session it re-enters.")

(defvar ygg--replaying nil
  "Bound to t around a `.' or @-macro `execute-kbd-macro' call, here and in
yggdrasil-verbs.el, so replayed commands don't re-journal or re-record.")

(defvar-local ygg--repeat-vector nil
  "What dot replays: a verb's key vector, or an insert session as
\(insert KEYS COUNT CHANGES EXIT-OFFSET).")
(defvar-local ygg--repeat-tick nil
  "buffer-chars-modified-tick as of the end of the previous command.")
(defvar-local ygg--repeat-verb-pending nil
  "(KEYS) set by `ygg-with-verb' when it changes the buffer; promoted to
`ygg--repeat-vector' in `ygg--post-command' unless insert state follows,
in which case the insert-exit path journals the whole session.")
(defvar-local ygg--repeat-insert-entry nil
  "(KEYS . ENTRY-CHANGED-P) for the insert session in progress, or nil.")

(defcustom ygg-max-selections 2000
  "Hard cap on live selections; operations refuse to exceed it."
  :type 'natnum :group 'yggdrasil)

(defface ygg-secondary-selection '((t :inherit region))
  "Secondary (non-primary) selections.")
(defface ygg-secondary-cursor '((t :inherit isearch))
  "Cursor cell of secondary selections.")

(defvar-local ygg--secondaries nil
  "Secondary selection overlays, permanently sorted by start.")
(defvar-local ygg--fake-cursor-ovs nil
  "Caret-cell overlays marking where each secondary selection's cursor sits.")
(defvar-local ygg--inhibit-normalize nil)
(defvar-local ygg--last-visual nil
  "(BEG END DIR) of the last visual selection, for gv.")

;;; Primary selection primitives

(defun ygg-selection-bounds ()
  "Primary selection as (BEG END DIR) in raw point/mark positions.
Point rests ON the cursor cell (Helix), so for verb bounds use
`ygg-selection-effective-bounds' which includes that cell."
  (let ((m (or (mark t) (point))))
    (if (<= m (point))
        (list m (point) 1)
      (list (point) m -1))))

(defun ygg-selection-effective-bounds ()
  "Primary selection as (BEG END DIR), always covering the cursor cell.
One formula both directions: [min(point,mark), max(point+1, mark))."
  (let* ((m (or (mark t) (point)))
         (beg (min (point) m))
         (end (min (max (1+ (point)) m) (point-max))))
    (list beg (max end beg) (if (< (point) m) -1 1))))

(defun ygg-set-selection (anchor cursor)
  "Select the text between gap positions ANCHOR and CURSOR.
Helix cursor semantics: for a forward selection point lands ON the
last character; for a backward one point is the first character and
the mark holds the gap after the anchor cell."
  (set-mark anchor)
  (goto-char (if (> cursor anchor) (1- cursor) cursor))
  (setq mark-active t))

(defun ygg--dir (ov) (or (overlay-get ov 'ygg-dir) 1))

(defun ygg--ov-anchor (ov)
  (if (> (ygg--dir ov) 0) (overlay-start ov) (overlay-end ov)))
(defun ygg--ov-cursor (ov)
  (if (> (ygg--dir ov) 0) (overlay-end ov) (overlay-start ov)))

;;; Secondary selection primitives

(defun ygg--make-secondary (beg end &optional dir)
  (let ((ov (make-overlay beg end nil nil t)))
    (overlay-put ov 'ygg-sel t)
    (overlay-put ov 'ygg-dir (or dir 1))
    (overlay-put ov 'face 'ygg-secondary-selection)
    (overlay-put ov 'priority 99)
    ov))

(defun ygg--insert-secondary-sorted (ov)
  "Insert OV into `ygg--secondaries' keeping the list sorted by start."
  (let ((start (overlay-start ov)))
    (if (or (null ygg--secondaries)
            (< start (overlay-start (car ygg--secondaries))))
        (push ov ygg--secondaries)
      (let ((tail ygg--secondaries))
        (while (and (cdr tail)
                    (<= (overlay-start (cadr tail)) start))
          (setq tail (cdr tail)))
        (setcdr tail (cons ov (cdr tail)))))))

(defun ygg-add-selection (beg end &optional dir)
  "Add a secondary selection over BEG..END."
  (when (>= (ygg-selections-count) ygg-max-selections)
    (user-error "Selection cap reached (%d)" ygg-max-selections))
  (ygg--insert-secondary-sorted (ygg--make-secondary beg end dir)))

(defun ygg--delete-secondary (ov)
  (setq ygg--secondaries (delq ov ygg--secondaries))
  (delete-overlay ov))

(defun ygg--clear-fake-cursors ()
  (mapc #'delete-overlay ygg--fake-cursor-ovs)
  (setq ygg--fake-cursor-ovs nil))

(defun ygg--render-fake-cursors ()
  "Draw a visible caret on the cursor cell of every secondary selection."
  (ygg--clear-fake-cursors)
  (setq ygg--fake-cursor-ovs
        (mapcar
         (lambda (ov)
           (let* ((s (overlay-start ov))
                  (e (overlay-end ov))
                  (fwd (> (ygg--dir ov) 0))
                  (cbeg (if fwd (max s (1- e)) s))
                  (cend (if fwd e (min e (1+ s))))
                  (co (make-overlay cbeg cend nil nil nil)))
             (overlay-put co 'ygg-fake-cursor t)
             (overlay-put co 'priority 101)
             ;; caret on a newline / eob has no cell to paint — echo one
             (if (or (>= cbeg (point-max)) (eq (char-after cbeg) ?\n))
                 (overlay-put co 'after-string
                              (propertize " " 'face 'ygg-secondary-cursor))
               (overlay-put co 'face 'ygg-secondary-cursor))
             co))
         ygg--secondaries)))

(defun ygg-clear-secondaries ()
  (mapc #'delete-overlay ygg--secondaries)
  (setq ygg--secondaries nil)
  (ygg--clear-fake-cursors))

(defun ygg-selections-count ()
  (1+ (length ygg--secondaries)))

;;; Flash feedback (evil-goggles-ish, on built-in pulse.el)

(defcustom ygg-flash t
  "If non-nil, pulse the buffer region a verb or yank just touched."
  :type 'boolean :group 'yggdrasil)

(defface ygg-flash-face '((t :inherit pulse-highlight-start-face))
  "Face used to flash the region touched by a verb or yank.")

(declare-function pulse-momentary-highlight-region "pulse")

(defun ygg-flash-region (beg end)
  "Pulse BEG..END per `ygg-flash'; a no-op outside interactive use."
  (when (and ygg-flash (not noninteractive))
    (require 'pulse)
    (pulse-momentary-highlight-region beg end 'ygg-flash-face)))

;;; Iteration — the shared engine for motions and verbs

(defun ygg-do-selections (fn)
  "Call FN with (BEG END DIR) for every selection.
Secondaries are visited bottom-up (descending), primary last, so edits
never invalidate pending bounds. Point/mark restored per secondary."
  (let ((ygg--inhibit-normalize t))
    (dolist (ov (reverse ygg--secondaries))
      (when (overlay-buffer ov)
        (save-excursion
          (funcall fn (overlay-start ov) (overlay-end ov) (ygg--dir ov)))))
    (pcase-let ((`(,beg ,end ,dir) (ygg-selection-effective-bounds)))
      (funcall fn beg end dir))))

(defun ygg-each-selection-update (fn)
  "Move every selection with FN.
FN is called with point at the selection's cursor CELL and
\(ANCHOR CURSOR DIR) as arguments; ANCHOR is a gap position, CURSOR the
cell under the cursor.  It must return (NEW-ANCHOR . NEW-CURSOR) as an
exclusive region in `ygg-set-selection' terms."
  (let ((ygg--inhibit-normalize t))
    (dolist (ov ygg--secondaries)
      (when (overlay-buffer ov)
        (let* ((dir (ygg--dir ov))
               (anchor (ygg--ov-anchor ov))
               (cursor (if (> dir 0)
                           (max (overlay-start ov) (1- (overlay-end ov)))
                         (overlay-start ov))))
          (save-excursion
            (goto-char cursor)
            (pcase-let ((`(,na . ,nc) (funcall fn anchor cursor dir)))
              (move-overlay ov (min na nc) (max na nc))
              (overlay-put ov 'ygg-dir (if (<= na nc) 1 -1)))))))
    (pcase-let* ((`(,_ ,_ ,dir) (ygg-selection-bounds))
                 (anchor (or (mark t) (point)))
                 (`(,na . ,nc) (funcall fn anchor (point) dir)))
      (ygg-set-selection na nc))
    (ygg--merge-secondaries)))

(defmacro ygg-with-verb (&rest body)
  "Run BODY as one editing verb: single undo step, normalization off.
Stashes this command's key vector for `.' when BODY changes the buffer
outside a `.'/macro replay; `ygg--post-command' folds it into the journal
unless insert state follows, in which case insert-exit does instead."
  (declare (indent 0))
  `(progn
     (let ((ygg--inhibit-normalize t))
       (with-undo-amalgamate ,@body))
     (when (and (not ygg--replaying)
                ygg--repeat-tick
                (/= ygg--repeat-tick (buffer-chars-modified-tick)))
       (setq ygg--repeat-verb-pending (this-command-keys-vector)))
     (when (and (not noninteractive) ygg-flash
                ygg--repeat-tick
                (/= ygg--repeat-tick (buffer-chars-modified-tick)))
       (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
         (ygg-flash-region beg end)))))

;;; Normalization (the invariant keeper)

(defun ygg--prune-dead-secondaries ()
  (setq ygg--secondaries
        (cl-delete-if (lambda (ov)
                        (or (null (overlay-buffer ov))
                            (and (= (overlay-start ov) (overlay-end ov))
                                 (progn (delete-overlay ov) t))))
                      ygg--secondaries)))

(defun ygg--merge-secondaries ()
  "Merge overlapping secondaries and absorb any overlapping the primary."
  (when ygg--secondaries
    (ygg--prune-dead-secondaries)
    (setq ygg--secondaries (sort ygg--secondaries
                                 (lambda (a b) (< (overlay-start a) (overlay-start b)))))
    (let ((merged nil))
      (dolist (ov ygg--secondaries)
        (let ((prev (car merged)))
          (if (and prev (<= (overlay-start ov) (overlay-end prev)))
              (progn (move-overlay prev (overlay-start prev)
                                   (max (overlay-end prev) (overlay-end ov)))
                     (delete-overlay ov))
            (push ov merged))))
      (setq ygg--secondaries (nreverse merged)))
    (pcase-let ((`(,pbeg ,pend ,_) (ygg-selection-effective-bounds)))
      (setq ygg--secondaries
            (cl-delete-if (lambda (ov)
                            (when (and (< (overlay-start ov) pend)
                                       (> (overlay-end ov) pbeg))
                              (delete-overlay ov) t))
                          ygg--secondaries)))))

(defun ygg--post-command ()
  (cond
   ((and ygg--insert-p ygg--echo-spans) (ygg--echo-sync))
   ((and (or ygg--normal-p ygg--visual-p) (not ygg--inhibit-normalize))
    (unless (mark t) (push-mark (point) t nil))
    (setq mark-active t deactivate-mark nil)
    (when ygg--secondaries (ygg--prune-dead-secondaries))
    (if ygg--secondaries (ygg--render-fake-cursors) (ygg--clear-fake-cursors)))
   (ygg--fake-cursor-ovs (ygg--clear-fake-cursors)))
  (when ygg--repeat-verb-pending
    (unless ygg--insert-p (setq ygg--repeat-vector ygg--repeat-verb-pending))
    (setq ygg--repeat-verb-pending nil))
  (setq ygg--repeat-tick (buffer-chars-modified-tick)))

(add-hook 'yggdrasil-local-mode-hook
          (lambda ()
            (if yggdrasil-local-mode
                (progn (add-hook 'post-command-hook #'ygg--post-command nil t)
                       (unless (mark t) (push-mark (point) t nil))
                       (setq mark-active t
                             ygg--repeat-tick (buffer-chars-modified-tick)))
              (remove-hook 'post-command-hook #'ygg--post-command t)
              (ygg-clear-secondaries))))

;;; Collapse / flip / rotate / keep

(defun ygg-collapse-selections ()
  "Collapse every selection to its cursor cell."
  (interactive)
  (set-mark (point))
  (dolist (ov ygg--secondaries)
    (if (> (ygg--dir ov) 0)
        (move-overlay ov (max (1- (overlay-end ov)) (point-min)) (overlay-end ov))
      (move-overlay ov (overlay-start ov)
                    (min (1+ (overlay-start ov)) (point-max))))
    (overlay-put ov 'ygg-dir 1))
  (when ygg--visual-p (ygg-normal-state)))

(defun ygg-flip-selections ()
  "Exchange anchor and cursor of every selection, keeping coverage."
  (interactive)
  (let ((m (mark t)))
    (when m
      (cond ((> (point) m) (set-mark (1+ (point))) (goto-char m))
            ((< (point) m) (set-mark (point)) (goto-char (1- m))))
      (setq mark-active t)))
  (dolist (ov ygg--secondaries)
    (overlay-put ov 'ygg-dir (- (ygg--dir ov)))))

(defun ygg-shrink-to-line-bounds ()
  "Shrink multi-line selections to line bounds; single-line unchanged."
  (interactive)
  (let ((m (mark t)))
    (when m
      (let* ((p (point))
             (forward-p (> p m))
             (beg (min p m))
             (end (max p m))
             (start-line (line-number-at-pos beg))
             (end-line (line-number-at-pos end)))
        (unless (eq start-line end-line)
          (save-excursion
            (goto-char beg)
            (let ((new-beg (line-beginning-position)))
              (goto-char end)
              (let ((new-end (if (bolp) (point) (line-beginning-position 2))))
                (if forward-p
                    (ygg-set-selection new-beg new-end)
                  (ygg-set-selection new-end new-beg)))))))))
  (dolist (ov ygg--secondaries)
    (when (overlay-buffer ov)
      (let* ((start (overlay-start ov))
             (end (overlay-end ov))
             (start-line (line-number-at-pos start))
             (end-line (line-number-at-pos end)))
        (unless (eq start-line end-line)
          (save-excursion
            (goto-char start)
            (let ((new-start (line-beginning-position)))
              (goto-char end)
              (let ((new-end (if (bolp) (point) (line-beginning-position 2))))
                (move-overlay ov new-start new-end)))))))))

(defun ygg-ensure-selections-forward ()
  "Set every selection to forward direction."
  (interactive)
  (let ((m (mark t)))
    (when m
      (when (<= (point) m)
        (set-mark (point))
        (goto-char (1+ m)))
      (setq mark-active t)))
  (dolist (ov ygg--secondaries)
    (when (< (ygg--dir ov) 0)
      (overlay-put ov 'ygg-dir 1))))

(defun ygg-keep-primary ()
  "Drop all secondary selections."
  (interactive)
  (ygg-clear-secondaries))

(defun ygg-remove-primary ()
  "Drop the primary; the nearest secondary becomes primary."
  (interactive)
  (if (null ygg--secondaries)
      (user-error "Only one selection")
    (let* ((pos (point))
           (ov (car (sort (copy-sequence ygg--secondaries)
                          (lambda (a b)
                            (< (abs (- (overlay-start a) pos))
                               (abs (- (overlay-start b) pos))))))))
      (ygg-set-selection (ygg--ov-anchor ov) (ygg--ov-cursor ov))
      (ygg--delete-secondary ov))))

(defun ygg--rotate (backward)
  (if (null ygg--secondaries)
      (user-error "Only one selection")
    (pcase-let ((`(,pbeg ,pend ,pdir) (ygg-selection-effective-bounds)))
      (let* ((ovs ygg--secondaries)
             (next (if backward
                       (or (cl-find-if (lambda (o) (< (overlay-start o) pbeg))
                                       (reverse ovs))
                           (car (last ovs)))
                     (or (cl-find-if (lambda (o) (> (overlay-start o) pbeg)) ovs)
                         (car ovs)))))
        (let ((na (ygg--ov-anchor next)) (nc (ygg--ov-cursor next)))
          (ygg--delete-secondary next)
          (ygg-add-selection pbeg pend pdir)
          (ygg-set-selection na nc))))))

(defun ygg-rotate-forward () (interactive) (ygg--rotate nil))
(defun ygg-rotate-backward () (interactive) (ygg--rotate t))

;;; Line/buffer selections

(defun ygg--line-selected-p (beg end)
  (and (/= beg end)
       (= beg (save-excursion (goto-char beg) (line-beginning-position)))
       (= end (save-excursion (goto-char end)
                              (if (bolp) (point) (line-beginning-position 2))))))

(defun ygg-select-line (&optional n)
  "Select the current line; repeats or a count extend N lines down."
  (interactive "p")
  (dotimes (_ (max 1 (or n 1)))
    (ygg--select-line-1)))

(defun ygg--select-line-1 ()
  (ygg-each-selection-update
   (lambda (anchor cursor dir)
     (let ((beg (min anchor cursor))
           (end (max anchor (if (> dir 0) (min (1+ cursor) (point-max)) cursor))))
       (if (ygg--line-selected-p beg end)
           (cons beg (save-excursion (goto-char end) (line-beginning-position 2)))
         (cons (save-excursion (goto-char beg) (line-beginning-position))
               (save-excursion (goto-char end) (line-beginning-position 2))))))))

(defun ygg-extend-to-line-bounds ()
  "Extend every selection to whole-line bounds."
  (interactive)
  (ygg-each-selection-update
   (lambda (anchor cursor dir)
     (let ((end (max anchor (if (> dir 0) (min (1+ cursor) (point-max)) cursor))))
       (cons (save-excursion (goto-char (min anchor cursor)) (line-beginning-position))
             (save-excursion (goto-char end)
                             (if (bolp) (point) (line-beginning-position 2))))))))

(defun ygg-extend-to-line-start ()
  "Extend every selection to the beginning of its line."
  (interactive)
  (ygg-each-selection-update
   (lambda (anchor cursor _dir)
     (let ((line-start (save-excursion (goto-char cursor) (line-beginning-position))))
       (cons anchor line-start)))))

(defun ygg-extend-to-line-end ()
  "Extend every selection to the end of its line (cursor on last char)."
  (interactive)
  (ygg-each-selection-update
   (lambda (anchor cursor _dir)
     (let ((line-end (save-excursion (goto-char cursor) (end-of-line)
                                      (unless (bolp) (backward-char 1))
                                      (point))))
       (cons anchor line-end)))))

(defun ygg-select-buffer ()
  "Select the whole buffer as a single selection."
  (interactive)
  (ygg-clear-secondaries)
  (ygg-set-selection (point-min) (point-max)))

;;; Multi-selection creators

(defun ygg--selection-regions ()
  "All selections as ((BEG END DIR) ...) in buffer order, primary included."
  (let ((regions (mapcar (lambda (ov)
                           (list (overlay-start ov) (overlay-end ov) (ygg--dir ov)))
                         ygg--secondaries)))
    (sort (cons (ygg-selection-effective-bounds) regions)
          (lambda (a b) (< (car a) (car b))))))

(defun ygg--install-regions (regions &optional primary-last)
  "Replace the selection set with REGIONS ((BEG . END)...); non-empty."
  (ygg-clear-secondaries)
  (when (> (length regions) ygg-max-selections)
    (setq regions (seq-take regions ygg-max-selections))
    (message "yggdrasil: capped at %d selections" ygg-max-selections))
  (let* ((primary (if primary-last (car (last regions)) (car regions)))
         (rest (delq primary (copy-sequence regions))))
    (dolist (r rest) (ygg-add-selection (car r) (cdr r)))
    (ygg-set-selection (car primary) (cdr primary))))

(defun ygg--region-pairs (regions)
  "Map ((BEG END DIR)...) from `ygg--selection-regions' to ((BEG . END)...)."
  (mapcar (lambda (r) (cons (car r) (cadr r))) regions))

(defun ygg--select-regex-scan (regexp scope)
  "Matches ((BEG . END)...) of REGEXP inside SCOPE ((BEG END DIR)...)."
  (let ((matches nil))
    (dolist (r scope)
      (save-excursion
        (goto-char (car r))
        (while (re-search-forward regexp (cadr r) t)
          (when (= (match-beginning 0) (match-end 0)) (forward-char))
          (unless (= (match-beginning 0) (match-end 0))
            (push (cons (match-beginning 0) (match-end 0)) matches)))))
    (nreverse matches)))

(defvar ygg--select-regex-buffer nil "Target buffer for live `s' preview.")
(defvar ygg--select-regex-scope nil "Original regions the live `s' searches within.")

(defun ygg--select-regex-preview ()
  "Minibuffer `post-command-hook': live-apply the typed pattern to the buffer.
Interruptible per frame (`while-no-input') so fast typing stays smooth; an
empty pattern or one with no match falls back to the original selection."
  (when (buffer-live-p ygg--select-regex-buffer)
    (let ((input (minibuffer-contents-no-properties)))
      (with-current-buffer ygg--select-regex-buffer
        (if (string-empty-p input)
            (ygg--install-regions (ygg--region-pairs ygg--select-regex-scope))
          (when-let* ((re (ignore-errors (ygg-regexp input))))
            (let ((res (while-no-input (ygg--select-regex-scan re ygg--select-regex-scope))))
              (cond
               ((memq res '(t throw-on-input)))   ; interrupted: keep last frame
               (res (ygg--install-regions res))
               (t (ygg--install-regions          ; completed, no match: show source
                   (ygg--region-pairs ygg--select-regex-scope)))))))))))

(defun ygg-select-regex (&optional regexp)
  "Select every match of a regexp inside the current selection.
With only the degenerate cursor (no real selection), searches the whole
buffer so a bare `s' in normal mode is useful.  Interactively the
selection updates live as you type (Helix `s'): RET commits, ESC
restores.  With REGEXP non-nil, apply it once."
  (interactive)
  (let* ((regions (ygg--selection-regions))
         (scope (if (and (null ygg--secondaries)
                         (<= (- (nth 1 (car regions)) (nth 0 (car regions))) 1))
                    (list (list (point-min) (point-max) 1))
                  regions))
         (commit (lambda (input)
                   (let ((matches (and input (not (string-empty-p input))
                                       (ignore-errors
                                         (ygg--select-regex-scan
                                          (ygg-regexp input) scope)))))
                     (cond
                      (matches (ygg--install-regions matches)
                               (unless ygg--visual-p (ygg-visual-state)))
                      (t (ygg--install-regions (ygg--region-pairs regions))
                         (when input (message "no matches"))))))))
    (if regexp
        (let ((matches (ygg--select-regex-scan regexp scope)))
          (if (null matches) (message "no matches")
            (ygg--install-regions matches)
            (unless ygg--visual-p (ygg-visual-state))))
      ;; live preview while typing, then a definitive final scan with the
      ;; committed input — never trust the last preview frame
      (let ((ygg--select-regex-buffer (current-buffer))
            (ygg--select-regex-scope scope))
        (funcall commit
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (add-hook 'post-command-hook
                                 #'ygg--select-regex-preview nil t))
                   (condition-case nil (read-regexp "select: ") (quit nil))))))))

(defun ygg-select-all-word ()
  "Put a cursor on every occurrence of the word (or selection) under point.
Helix `*'-into-select / VS Code select-all: with a real selection, matches
its exact text; otherwise the symbol under point, whole-word."
  (interactive)
  (let* ((bounds (ygg-selection-effective-bounds))
         (sel (and bounds (> (- (cadr bounds) (car bounds)) 1)
                   (buffer-substring-no-properties (car bounds) (cadr bounds))))
         (word (or sel (thing-at-point 'symbol t)))
         (regexp (cond ((null word) (user-error "No word under cursor"))
                       (sel (regexp-quote sel))
                       (t (concat "\\_<" (regexp-quote word) "\\_>"))))
         (matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (match-beginning 0) (match-end 0)) matches)))
    (if (null matches)
        (message "no matches")
      (ygg--install-regions (nreverse matches))
      (unless ygg--visual-p (ygg-visual-state))
      (message "%d cursors on %S" (length matches) word))))

(defun ygg-split-regex (regexp)
  "Split every selection on matches of REGEXP."
  (interactive (list (ygg-regexp (read-regexp "split on: "))))
  (let ((pieces nil))
    (dolist (r (ygg--selection-regions))
      (save-excursion
        (goto-char (car r))
        (let ((prev (car r)))
          (while (re-search-forward regexp (cadr r) t)
            (when (= (match-beginning 0) (match-end 0)) (forward-char))
            (when (> (match-beginning 0) prev)
              (push (cons prev (match-beginning 0)) pieces))
            (setq prev (match-end 0)))
          (when (< prev (cadr r))
            (push (cons prev (cadr r)) pieces)))))
    (if (null pieces)
        (message "nothing to split")
      (ygg--install-regions (nreverse pieces))
      (unless ygg--visual-p (ygg-visual-state)))))

(defun ygg-split-lines ()
  "Split every selection into per-line selections."
  (interactive)
  (ygg-split-regex "\n"))

(defun ygg--filter-selections (regexp keep)
  (let ((regions (cl-remove-if-not
                  (lambda (r)
                    (eq keep
                        (not (null (string-match-p
                                    regexp (buffer-substring-no-properties
                                            (car r) (cadr r)))))))
                  (ygg--selection-regions))))
    (if (null regions)
        (user-error "No selections would remain")
      (ygg--install-regions
       (mapcar (lambda (r) (cons (car r) (cadr r))) regions))
      (unless ygg--visual-p (ygg-visual-state)))))

(defun ygg-keep-matching (regexp)
  "Keep only selections containing a match of REGEXP."
  (interactive (list (ygg-regexp (read-regexp "keep matching: "))))
  (ygg--filter-selections regexp t))

(defun ygg-remove-matching (regexp)
  "Remove selections containing a match of REGEXP."
  (interactive (list (ygg-regexp (read-regexp "remove matching: "))))
  (ygg--filter-selections regexp nil))

(defun ygg--copy-selection (up)
  (pcase-let* ((`(,beg ,end ,dir) (ygg-selection-effective-bounds))
               (cb (save-excursion (goto-char beg) (current-column)))
               (ce (save-excursion (goto-char end) (current-column)))
               (lines (count-lines beg end))
               (step (max 1 (if (and (> lines 1)
                                     (save-excursion (goto-char end) (bolp)))
                                (1- lines) lines))))
    (let ((found (save-excursion
                   (goto-char (if up beg end))
                   (let (hit)
                     (while (and (not hit)
                                 (zerop (forward-line (if up (- step) step)))
                                 (or up (not (eobp))))
                       (let* ((nb (progn (move-to-column cb)
                                         (and (= (current-column) cb) (point))))
                              (ne (progn (move-to-column ce)
                                         (and (= (current-column) ce) (point)))))
                         (when (and nb ne (or (/= nb ne) (= beg end)))
                           (setq hit (cons nb ne)))))
                     hit))))
      (if (not found)
          (message "no room to copy selection")
        (ygg-add-selection beg end dir)
        (ygg-set-selection (if (> dir 0) (car found) (cdr found))
                           (if (> dir 0) (cdr found) (car found)))
        (unless ygg--visual-p (ygg-visual-state))))))

(defun ygg-copy-selection-down (&optional n)
  (interactive "p")
  (dotimes (_ (max 1 (or n 1))) (ygg--copy-selection nil)))
(defun ygg-copy-selection-up (&optional n)
  (interactive "p")
  (dotimes (_ (max 1 (or n 1))) (ygg--copy-selection t)))

(defun ygg-rotate-contents (&optional backward)
  "Rotate the text contents between selections."
  (interactive)
  (let* ((regions (ygg--selection-regions))
         (texts (mapcar (lambda (r)
                          (buffer-substring-no-properties (car r) (cadr r)))
                        regions)))
    (when (> (length regions) 1)
      (let ((rotated (if backward
                         (append (cdr texts) (list (car texts)))
                       (cons (car (last texts)) (butlast texts))))
            (new-regions nil))
        (ygg-with-verb
          (cl-loop for r in (reverse regions)
                   for text in (reverse rotated)
                   do (save-excursion
                        (goto-char (car r))
                        (delete-region (car r) (cadr r))
                        (insert text)
                        (push (cons (car r) (+ (car r) (length text)))
                              new-regions))))
        (ygg--install-regions new-regions)
        (unless ygg--visual-p (ygg-visual-state))))))

(defun ygg-rotate-contents-backward ()
  (interactive)
  (ygg-rotate-contents t))

(defun ygg-align-selections ()
  "Align all selections into the same column by padding with spaces."
  (interactive)
  (let* ((regions (ygg--selection-regions))
         (target (apply #'max (mapcar (lambda (r)
                                        (save-excursion (goto-char (car r))
                                                        (current-column)))
                                      regions))))
    (ygg-with-verb
      (dolist (r (reverse regions))
        (save-excursion
          (goto-char (car r))
          (let ((col (current-column)))
            (when (< col target)
              (insert (make-string (- target col) ?\s)))))))))

(defun ygg-trim-selections ()
  "Shrink every selection to exclude surrounding whitespace."
  (interactive)
  (let ((trimmed
         (delq nil
               (mapcar (lambda (r)
                         (save-excursion
                           (goto-char (car r))
                           (skip-chars-forward " \t\n" (cadr r))
                           (let ((b (point)))
                             (goto-char (cadr r))
                             (skip-chars-backward " \t\n" b)
                             (when (< b (point)) (cons b (point))))))
                       (ygg--selection-regions)))))
    (if (null trimmed)
        (message "selections are all whitespace")
      (ygg--install-regions trimmed)
      (unless ygg--visual-p (ygg-visual-state)))))

;;; Merge selections (Helix M-- / M-_)

(defun ygg-merge-selections ()
  "Merge every selection into one spanning the combined bounds of all,
becoming the (forward) primary; all secondaries are dropped."
  (interactive)
  (let* ((regions (ygg--selection-regions))
         (beg (apply #'min (mapcar #'car regions)))
         (end (apply #'max (mapcar #'cadr regions))))
    (ygg-clear-secondaries)
    (ygg-set-selection beg end)))

(defun ygg--selections-consecutive-p (a b)
  "Non-nil if selection A (BEG END DIR) touches, overlaps, or is only
whitespace apart from selection B."
  (or (>= (cadr a) (car b))
      (string-blank-p (buffer-substring-no-properties (cadr a) (car b)))))

(defun ygg--merge-region (group)
  "GROUP is a run of consecutive (BEG END DIR) selections.
A single member passes through unchanged; several merge forward across
their combined span."
  (if (cdr group)
      (list (apply #'min (mapcar #'car group))
            (apply #'max (mapcar #'cadr group))
            1)
    (car group)))

(defun ygg--install-region (r)
  "Make region R = (BEG END DIR) the primary selection."
  (pcase-let ((`(,beg ,end ,dir) r))
    (if (> dir 0) (ygg-set-selection beg end) (ygg-set-selection end beg))))

(defun ygg-merge-consecutive-selections ()
  "Merge each run of touching or whitespace-separated selections into one.
A selection with nothing consecutive to merge into is left untouched;
the group holding the primary stays primary."
  (interactive)
  (pcase-let ((`(,pbeg ,pend ,_) (ygg-selection-effective-bounds)))
    (let* ((regions (ygg--selection-regions))
           (groups (list (list (car regions)))))
      (dolist (r (cdr regions))
        (if (ygg--selections-consecutive-p (caar groups) r)
            (push r (car groups))
          (push (list r) groups)))
      (setq groups (mapcar #'nreverse (nreverse groups)))
      (let* ((merged (mapcar #'ygg--merge-region groups))
             (primary-idx (cl-position-if
                           (lambda (g)
                             (cl-find-if (lambda (r) (and (= (car r) pbeg)
                                                          (= (cadr r) pend)))
                                         g))
                           groups))
             (primary-region (nth primary-idx merged)))
        (ygg-clear-secondaries)
        (dolist (r merged)
          (unless (eq r primary-region)
            (apply #'ygg-add-selection r)))
        (ygg--install-region primary-region)))))

;;; Visual state bookkeeping (gv)

(add-hook 'ygg-visual-exit-hook
          (lambda ()
            (pcase-let ((`(,beg ,end ,dir) (ygg-selection-effective-bounds)))
              (when (/= beg end)
                (setq ygg--last-visual (list beg end dir))))))

(defun ygg-reselect-last ()
  "Reselect the last visual selection (vim gv)."
  (interactive)
  (if (null ygg--last-visual)
      (user-error "No previous visual selection")
    (pcase-let ((`(,beg ,end ,dir) ygg--last-visual))
      (ygg-visual-state)
      (if (> dir 0) (ygg-set-selection beg end)
        (ygg-set-selection end beg)))))

;;; Insert entry with N cursors + echo engine

(defun ygg-enter-insert-at (positions &optional count open-line)
  "Enter insert state with a cursor at each of POSITIONS (primary first).
With one position this is a plain insert entry; with several, typed text
is mirrored at every cursor until insert state exits.  A COUNT above one
repeats the session's edits on exit, each extra copy on a line opened by
OPEN-LINE (called with point twice) when that is given (vim 3o)."
  (let ((count (or ygg--repeat-count count)))
    (setq ygg--insert-count (and count (> count 1) (cons count open-line))))
  (setq ygg--repeat-insert-entry
        (unless ygg--replaying
          (cons (this-command-keys-vector)
                (and ygg--repeat-tick
                     (/= ygg--repeat-tick (buffer-chars-modified-tick))))))
  (goto-char (car positions))
  (setq ygg--echo-primary (copy-marker (point)))
  (when (cdr positions)
    (setq ygg--echo-last ""
          ygg--echo-spans
          (mapcar (lambda (pos)
                    (cons (copy-marker pos) (copy-marker pos t)))
                  (sort (cdr positions) #'>)))
    (setq ygg--echo-cursor-ovs
          (mapcar (lambda (span)
                    (let ((ov (make-overlay (car span)
                                            (min (1+ (car span)) (point-max)))))
                      (overlay-put ov 'face 'ygg-secondary-cursor)
                      (overlay-put ov 'priority 100)
                      ov))
                  ygg--echo-spans)))
  (ygg-clear-secondaries)
  (set-mark (point))
  (ygg-insert-state))

(defun ygg--echo-teardown ()
  (dolist (span ygg--echo-spans)
    (set-marker (car span) nil)
    (set-marker (cdr span) nil))
  (mapc #'delete-overlay ygg--echo-cursor-ovs)
  (when ygg--echo-primary (set-marker ygg--echo-primary nil))
  (setq ygg--echo-spans nil ygg--echo-primary nil
        ygg--echo-last nil ygg--echo-cursor-ovs nil))

(defun ygg--echo-abort (&optional msg)
  (ygg--echo-teardown)
  (when msg (message "yggdrasil: %s" msg)))

(defun ygg--echo-sync ()
  (cond
   ((< (point) ygg--echo-primary)
    (ygg--echo-abort "cursor left the insert span — mirroring stopped"))
   (t
    (let ((new (buffer-substring-no-properties ygg--echo-primary (point))))
      (unless (string= new ygg--echo-last)
        (let ((ygg--inhibit-normalize t)
              (ygg--insert-unrecorded t))
          (dolist (span ygg--echo-spans)
            (when (marker-buffer (car span))
              (save-excursion
                (delete-region (car span) (cdr span))
                (goto-char (car span))
                (insert new)))))
        (setq ygg--echo-last new))))))

(defun ygg--echo-finish ()
  (when ygg--echo-spans
    (ygg--echo-sync)
    (let ((regions (mapcar (lambda (span)
                             (cons (marker-position (car span))
                                   (marker-position (cdr span))))
                           ygg--echo-spans))
          (pbeg (marker-position ygg--echo-primary))
          (pend (point)))
      (ygg--echo-teardown)
      (dolist (r regions)
        (ygg-add-selection (car r) (max (cdr r) (car r))))
      (ygg-set-selection pbeg (max pend pbeg)))))

;;; Insert sessions: one undo step, recorded edits for dot and counts

(defun ygg--shift-anchor (anchor beg end deleted)
  "Where ANCHOR lands after DELETED chars at BEG became the text BEG..END."
  (cond ((<= anchor beg) anchor)
        ((>= anchor (+ beg deleted)) (+ anchor (- end beg deleted)))
        (t beg)))

(defun ygg--insert-record-change (beg end deleted)
  (unless ygg--insert-unrecorded
    (let ((offset (- beg ygg--insert-anchor))
          (text (buffer-substring-no-properties beg end))
          (last (car ygg--insert-changes)))
      (if (and last (zerop deleted) (zerop (nth 1 last))
               (= offset (+ (nth 0 last) (length (nth 2 last)))))
          (setcar ygg--insert-changes
                  (list (nth 0 last) 0 (concat (nth 2 last) text)))
        (push (list offset deleted text) ygg--insert-changes))))
  (setq ygg--insert-anchor
        (ygg--shift-anchor ygg--insert-anchor beg end deleted)))

(defun ygg--insert-apply-changes (changes anchor)
  "Make CHANGES, oldest first, relative to ANCHOR; return the moved anchor."
  (pcase-dolist (`(,offset ,deleted ,text) changes)
    (let* ((beg (max (point-min) (min (+ anchor offset) (point-max))))
           (end (min (+ beg deleted) (point-max))))
      (delete-region beg end)
      (goto-char beg)
      (insert text)
      (setq anchor (ygg--shift-anchor anchor beg (point) (- end beg)))))
  anchor)

(defun ygg--insert-prepare-group ()
  "A change group that also takes in edits this command made before insert.
Those are the c deletion or the o newline, so one undo reverts them too."
  (let ((undo buffer-undo-list))
    (when (and ygg--repeat-tick
               (/= ygg--repeat-tick (buffer-chars-modified-tick)))
      (while (and (consp undo) (car undo))
        (setq undo (cdr undo))))
    (list (cons (current-buffer) undo))))

(defun ygg--insert-begin ()
  (setq ygg--insert-anchor (point)
        ygg--insert-changes nil
        ygg--insert-group (ygg--insert-prepare-group))
  (activate-change-group ygg--insert-group)
  (add-hook 'after-change-functions #'ygg--insert-record-change nil t))

(add-hook 'ygg-insert-entry-hook #'ygg--insert-begin)

(defun ygg--insert-close-group ()
  (when ygg--insert-group
    (undo-amalgamate-change-group ygg--insert-group)
    (accept-change-group ygg--insert-group)
    (setq ygg--insert-group nil)))

(defun ygg--insert-repeat-count (count open-line changes exit-offset)
  "Make CHANGES COUNT - 1 more times from point, as vim 3i and 3o do."
  (let ((ygg--inhibit-normalize t))
    (dotimes (_ (1- count))
      (when open-line (goto-char (funcall open-line (point) (point))))
      (goto-char (+ (ygg--insert-apply-changes changes (point)) exit-offset)))))

(defun ygg--repeat-finish-insert (count changes exit-offset)
  "Journal this insert session for dot when it changed the buffer."
  (when ygg--repeat-insert-entry
    (pcase-let ((`(,keys . ,entry-changed) ygg--repeat-insert-entry))
      (when (or entry-changed changes)
        (setq ygg--repeat-vector
              (list 'insert keys count changes exit-offset))))
    (setq ygg--repeat-insert-entry nil)))

(defun ygg--insert-exit ()
  "Leave insert: finish a multi-cursor session, or vim-collapse the cursor.
Vim/Helix step back onto the last typed character and drop any span the
insert left behind; without this, the next verb acts on the whole typed
text plus one char."
  (remove-hook 'after-change-functions #'ygg--insert-record-change t)
  (let ((changes (reverse ygg--insert-changes))
        (exit-offset (if ygg--insert-anchor (- (point) ygg--insert-anchor) 0)))
    (pcase-let ((`(,count . ,open-line) ygg--insert-count))
      (setq ygg--insert-count nil
            ygg--insert-changes nil
            ygg--insert-anchor nil)
      (when (and count (or changes open-line))
        (ygg--insert-repeat-count count open-line changes exit-offset))
      (ygg--repeat-finish-insert count changes exit-offset)))
  (if ygg--echo-spans
      (ygg--echo-finish)
    (when (> (point) (line-beginning-position))
      (backward-char 1))
    (set-mark (point))
    (when ygg--echo-primary
      (set-marker ygg--echo-primary nil)
      (setq ygg--echo-primary nil)))
  (ygg--insert-close-group))

(add-hook 'ygg-insert-exit-hook #'ygg--insert-exit)

(defun ygg-insert-undo ()
  "Undo during insert: abort any echo session first, then undo."
  (interactive)
  (when ygg--echo-spans
    (ygg--echo-abort "multi-cursor session aborted"))
  (undo))

;;; Dot-repeat (vim `.')

(defun ygg-repeat (&optional count)
  "Vim `.': replay the last buffer-changing verb COUNT times."
  (interactive "p")
  (unless ygg--repeat-vector (user-error "Nothing to repeat"))
  (let ((ygg--replaying t)
        (record ygg--repeat-vector))
    (if (vectorp record)
        (execute-kbd-macro record count)
      (dotimes (_ (max 1 (or count 1)))
        (apply #'ygg--repeat-insert (cdr record))))))

(defun ygg--repeat-insert (keys count changes exit-offset)
  "Re-enter insert with KEYS and COUNT, remake CHANGES, then leave insert."
  (let ((ygg--repeat-count count))
    (execute-kbd-macro keys))
  (when ygg--insert-p
    (unwind-protect
        (goto-char (+ (ygg--insert-apply-changes changes ygg--insert-anchor)
                      exit-offset))
      (ygg-normal-state))))

;;; Default bindings owned by this module

(defun ygg-escape ()
  "Normal-state escape: drop secondaries, collapse selection."
  (interactive)
  (ygg-keep-primary)
  (set-mark (point)))

(yggdrasil-define-keys 'normal
  "." #'ygg-repeat :label "repeat"
  "(" #'ygg-rotate-backward
  ")" #'ygg-rotate-forward
  "x" #'ygg-select-line
  "X" #'ygg-extend-to-line-bounds
  "%" #'ygg-percent
  "s" #'ygg-select-regex
  "S" #'ygg-split-regex
  "K" #'ygg-keep-matching
  "C" #'ygg-copy-selection-down
  "&" #'ygg-align-selections
  "_" #'ygg-trim-selections
  "<escape>" #'ygg-escape)

;; the V family keeps each key it had under Meta, so the finger moves
;; house without relearning the letter
;; ; and , belong to f/t here, as they do in vim; the shifted key is the
;; variant of the plain one, the way < is the other side of ,
(yggdrasil-define-keys 'ygg-selections-map
  ";" #'ygg-collapse-selections :label "collapse"
  ":" #'ygg-flip-selections :label "flip ends"
  "," #'ygg-keep-primary :label "keep primary"
  "<" #'ygg-remove-primary :label "remove primary"
  "(" #'ygg-rotate-contents-backward :label "rotate contents back"
  ")" #'ygg-rotate-contents :label "rotate contents"
  "s" #'ygg-split-lines :label "split by line"
  "K" #'ygg-remove-matching :label "remove matching"
  "C" #'ygg-copy-selection-up :label "copy up"
  "-" #'ygg-merge-selections :label "merge selections"
  "_" #'ygg-merge-consecutive-selections :label "merge consecutive"
  "x" #'ygg-shrink-to-line-bounds :label "shrink to line"
  ">" #'ygg-ensure-selections-forward :label "forward")

(yggdrasil-define-keys 'insert
  "C-/" #'ygg-insert-undo
  "C-<delete>" #'kill-word)

(yggdrasil-define-keys 'ygg-goto-map
  "v" #'ygg-reselect-last :label "last visual")

(yggdrasil-define-keys 'visual
  "n" #'ygg-select-next-match :label "add next match"
  "N" #'ygg-select-prev-match :label "add prev match"
  "<home>" #'ygg-extend-to-line-start :label "extend to line start"
  "<end>" #'ygg-extend-to-line-end :label "extend to line end")

(yggdrasil-define-keys 'ygg-selections-map
  "n" #'ygg-skip-to-next-match :label "skip to next match")

(defun ygg--match-word-regexp ()
  "Regexp and text of the word or selection under the primary cursor.
Returns (REGEXP . TEXT) by the rule `ygg-select-all-word' uses: a real
selection matches its exact text, a bare cursor the symbol under it,
whole-word."
  (let* ((bounds (ygg-selection-effective-bounds))
         (sel (and bounds (> (- (cadr bounds) (car bounds)) 1)
                   (buffer-substring-no-properties (car bounds) (cadr bounds))))
         (word (or sel (thing-at-point 'symbol t))))
    (cond ((null word) (user-error "No word under cursor"))
          (sel (cons (regexp-quote sel) sel))
          (t (cons (concat "\\_<" (regexp-quote word) "\\_>") word)))))

(defun ygg--word-match-list (regexp)
  "Every match of REGEXP in the buffer as ((BEG . END)...) in buffer order."
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (match-beginning 0) (match-end 0)) matches)))
    (nreverse matches)))

(defun ygg--select-match-step (backward keep)
  "Make the next unselected occurrence of the primary's word the primary.
BACKWARD searches towards `point-min' first; the scan wraps around the
buffer either way.  With KEEP the outgoing primary stays as a secondary,
without it the primary is dropped."
  (pcase-let* ((`(,regexp . ,word) (ygg--match-word-regexp))
               (`(,pbeg ,pend ,_) (ygg-selection-effective-bounds))
               (matches (ygg--word-match-list regexp))
               (origin (or (seq-find (lambda (m)
                                       (and (<= (car m) pbeg) (< pbeg (cdr m))))
                                     matches)
                           (cons pbeg pend)))
               (taken (cons origin
                            (ygg--region-pairs (ygg--selection-regions))))
               (ordered (if backward (reverse matches) matches))
               (ahead (if backward
                          (lambda (m) (< (car m) (car origin)))
                        (lambda (m) (> (car m) (car origin)))))
               (queue (append (seq-filter ahead ordered)
                              (seq-remove ahead ordered)))
               (found (seq-find (lambda (m) (not (member m taken))) queue)))
    (unless found (user-error "No further occurrence of %S" word))
    (when keep (ygg-add-selection (car origin) (cdr origin)))
    (ygg-set-selection (car found) (cdr found))
    (unless ygg--visual-p (ygg-visual-state))))

(defun ygg-select-next-match ()
  "Add the next occurrence of the word under the cursor as the primary.
The outgoing primary stays behind as a secondary, so pressing this
repeatedly grows the cursor set one match at a time, wrapping around the
buffer and stepping over occurrences already selected."
  (interactive)
  (ygg--select-match-step nil t))

(defun ygg-select-prev-match ()
  "Like `ygg-select-next-match' but towards the start of the buffer."
  (interactive)
  (ygg--select-match-step t t))

(defun ygg-skip-to-next-match ()
  "Move the primary to the next unselected occurrence, leaving none behind.
The occurrence the cursor stood on is dropped rather than kept."
  (interactive)
  (ygg--select-match-step nil nil))

(provide 'yggdrasil-selection)
;;; yggdrasil-selection.el ends here
