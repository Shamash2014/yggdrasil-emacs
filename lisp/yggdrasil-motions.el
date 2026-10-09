;;; yggdrasil-motions.el --- Yggdrasil motions -*- lexical-binding: t; -*-

;; Built-ins wrapped: forward-char, line-move, char-syntax scanning,
;; search-forward/backward, isearch-forward-regexp, re-search-forward/backward,
;; recenter, scroll-up-line/scroll-down-line, move-to-window-line,
;; xref-find-definitions, comment-or-uncomment-region, next-buffer,
;; previous-buffer, mode-line-other-buffer, digit-argument, buffer-undo-list,
;; after-change-functions, repeat-mode.
;; Custom: Helix normal/visual replace-vs-extend motion wrapper, word/WORD
;; span scanning, find/till char search with repeat, jumplist, search state,
;; vim marks, change list, sticky view via repeat-mode.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-selection)
(require 'seq)
(require 'repeat)
(require 'ring)

(declare-function ygg-downcase "yggdrasil-verbs")
(declare-function ygg-upcase "yggdrasil-verbs")
(declare-function ygg-number-increment "yggdrasil-verbs")
(declare-function ygg-number-decrement "yggdrasil-verbs")
(declare-function ygg-number-increment-sequential "yggdrasil-verbs")
(declare-function ygg-number-decrement-sequential "yggdrasil-verbs")
(declare-function avy-jump "avy")
(declare-function avy-goto-line-below "avy")
(declare-function avy-goto-line-above "avy")
(declare-function ffap-guesser "ffap")
(declare-function ffap-file-at-point "ffap")
(declare-function ffap "ffap")
(declare-function yggdrasil-leader-def "yggdrasil-leader")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-beg "flymake")

;;; The one wrapper every motion is built on

(defun ygg--motion (move-fn &optional span)
  "Wrap MOVE-FN for `ygg-each-selection-update'.
MOVE-FN moves point (called with point already at the selection's cursor,
no args). In normal state: SPAN non-nil anchors the new selection at the
old cursor (a traversed span); nil collapses anchor to the new position.
Visual state always extends: anchor is kept, only the cursor moves."
  (lambda (anchor cursor dir)
    (ignore-errors (funcall move-fn))
    (let ((new (point)))
      (cond ((ygg-visual-p)
             ;; span motions land on gaps, plain moves on cells — normalize;
             ;; when the head crosses the anchor, the anchor cell stays covered
             (let ((nc (if (or span (< new anchor))
                           new
                         (min (1+ new) (point-max))))
                   (na (cond ((and (< new anchor) (> dir 0))
                              (min (1+ anchor) (point-max)))
                             ((and (>= new anchor) (< dir 0))
                              (max (1- anchor) (point-min)))
                             (t anchor))))
               (cons na nc)))
            (span (cons cursor new))
            (t (cons new new))))))

;;; h l j k

(defun ygg-h (&optional n)
  (interactive "p")
  (ygg-each-selection-update
   (ygg--motion (lambda ()
                  (goto-char (max (- (point) n) (line-beginning-position)))))))

(defun ygg-l (&optional n)
  (interactive "p")
  (ygg-each-selection-update
   (ygg--motion (lambda ()
                  (goto-char (min (+ (point) n) (line-end-position)))))))

(defun ygg--vmove (n)
  "Move N lines via `line-move'; mimics next-line/previous-line so
`temporary-goal-column' survives across repeated presses."
  (setq this-command (if (>= n 0) 'next-line 'previous-line))
  (line-move n))

(defun ygg-j (&optional n)
  (interactive "p")
  (ygg-each-selection-update (ygg--motion (lambda () (ygg--vmove n)))))

(defun ygg-k (&optional n)
  (interactive "p")
  (ygg-each-selection-update (ygg--motion (lambda () (ygg--vmove (- n))))))

;;; % — jump to the matching item (vim matchit): brackets AND keyword
;;; pairs (do/end, def/end, if/endif, tags…). Reuses evil-matchit's
;;; evil-free native jump; our selection engine handles normal/visual.

(declare-function evilmi-jump-items-native "evil-matchit")

(defun ygg-percent (&optional _n)
  "Jump to the matching item (vim %); extends the selection in visual."
  (interactive "p")
  (unless (or (featurep 'evil-matchit) (require 'evil-matchit nil t))
    (user-error "evil-matchit is still installing — retry in a moment"))
  (ygg-each-selection-update (ygg--motion #'evilmi-jump-items-native)))

;;; Word / WORD motions

(defun ygg--char-class (pos words)
  ;; Helix's fixed classes (alnum + _), not the mode's syntax table
  (let ((c (char-after pos)))
    (cond ((or (null c) (memq c '(?\s ?\t ?\n ?\r ?\f))) 'blank)
          (words 'word)
          ((or (eq c ?_)
               (memq (get-char-code-property c 'general-category)
                     '(Ll Lu Lt Lm Lo Mn Mc Nd Nl)))
           'word)
          (t 'punct))))

(defun ygg--skip-fwd (class words)
  (while (eq (ygg--char-class (point) words) class) (forward-char 1)))

(defun ygg--skip-back-at (class words)
  (while (and (not (bobp)) (eq (ygg--char-class (point) words) class))
    (backward-char 1)))

(defun ygg--skip-back (class words)
  (while (and (not (bobp)) (eq (ygg--char-class (1- (point)) words) class))
    (backward-char 1)))

(defun ygg--word-fwd (words)
  "Advance one Helix word span; return the span's start position.
At a class boundary the scan starts from the NEXT char, or repeated
w sticks forever on the last char of the previous span."
  (when (and (not (eobp))
             (not (eq (ygg--char-class (point) words)
                      (ygg--char-class (1+ (point)) words))))
    (forward-char 1))
  (let ((begin (point))
        (cls (ygg--char-class (point) words)))
    (ygg--skip-fwd cls words)
    (unless (eq cls 'blank) (ygg--skip-fwd 'blank words))
    begin))

(defun ygg--word-end-fwd (words)
  (forward-char 1)
  (ygg--skip-fwd 'blank words)
  (ygg--skip-fwd (ygg--char-class (point) words) words))

(defun ygg--word-back (words)
  "Mirrors `ygg--word-fwd' but with the run-check on the trailing char,
since point sits before the char under a backward scan, not after it."
  (backward-char 1)
  (ygg--skip-back-at 'blank words)
  (unless (eq (ygg--char-class (point) words) 'blank)
    (ygg--skip-back (ygg--char-class (point) words) words)))

(defun ygg--n-times (n fn)
  (dotimes (_ (max 1 (or n 1))) (funcall fn)))

(defmacro ygg--defword (name step words)
  `(defun ,name (&optional n)
     (interactive "p")
     (ygg-each-selection-update
      (lambda (anchor cursor _dir)
        (let (begin)
          (ignore-errors
            (ygg--n-times n (lambda ()
                              (let ((b (funcall #',step ,words)))
                                (unless begin (setq begin b))))))
          (let ((new (point)))
            (cond ((ygg-visual-p) (cons anchor new))
                  ((< new cursor) (cons cursor new))
                  (t (cons (or begin cursor) new)))))))))

(ygg--defword ygg-w ygg--word-fwd nil)
(ygg--defword ygg-e ygg--word-end-fwd nil)
(ygg--defword ygg-b ygg--word-back nil)
(ygg--defword ygg-W ygg--word-fwd t)
(ygg--defword ygg-E ygg--word-end-fwd t)
(ygg--defword ygg-B ygg--word-back t)

;;; f t F T + M-. repeat

(defvar-local ygg--last-find nil "(CHAR FORWARD TILL) of the last f/t/F/T.")
(defvar-local ygg--mark-last-jump-pos nil
  "Position before the last jump, for '' and backtick marks.")
(defvar-local ygg--change-list nil
  "Change positions, most recent first, capped at `ygg--change-list-max'.")

(defun ygg--do-find (ch forward till n)
  (let ((s (string ch)) (n (max 1 (or n 1))))
    (if forward
        (progn (forward-char 1) (search-forward s nil nil n)
               (when till (backward-char 1)))
      (progn (search-backward s nil nil n)
             (when till (forward-char 1))))))

(defun ygg--find-updater (ch forward till n)
  "Backward finds keep the origin cell selected (Helix put_cursor)."
  (lambda (anchor cursor _dir)
    (ignore-errors (ygg--do-find ch forward till n))
    (let ((new (point)))
      (cond ((ygg-visual-p) (cons anchor new))
            (forward (cons cursor new))
            (t (cons (min (1+ cursor) (point-max)) new))))))

(defun ygg--find-command (forward till)
  (let ((ch (read-char)) (n (prefix-numeric-value current-prefix-arg)))
    (setq ygg--last-find (list ch forward till))
    (ygg-each-selection-update (ygg--find-updater ch forward till n))))

(defun ygg-find-forward () (interactive) (ygg--find-command t nil))
(defun ygg-till-forward () (interactive) (ygg--find-command t t))
(defun ygg-find-backward () (interactive) (ygg--find-command nil nil))
(defun ygg-till-backward () (interactive) (ygg--find-command nil t))

(defun ygg--repeat-find (reverse)
  (unless ygg--last-find (user-error "No previous find"))
  (pcase-let ((`(,ch ,forward ,till) ygg--last-find)
              (n (prefix-numeric-value current-prefix-arg)))
    (ygg-each-selection-update
     (ygg--find-updater ch (if reverse (not forward) forward) till n))))

(defun ygg-repeat-find ()
  "Repeat the last f/t the way it went (vim `;\')."
  (interactive)
  (ygg--repeat-find nil))

(defun ygg-repeat-find-reverse ()
  "Repeat the last f/t the other way (vim `,\')."
  (interactive)
  (ygg--repeat-find t))

;;; Line motions: 0 $ gh gl gs

(defun ygg-goto-line-start (&optional n)
  "Line start, or column N (1-indexed) when count given (Helix g| goto_column)."
  (interactive "p")
  (if (and n (> n 1))
      (ygg-each-selection-update
       (ygg--motion
        (lambda ()
          (beginning-of-line)
          (goto-char (min (+ (line-beginning-position) (1- n)) (line-end-position))))))
    (ygg-each-selection-update (ygg--motion #'beginning-of-line))))

(defun ygg-goto-line-end ()
  "Line end, cursor ON the last character (Helix gl / vim $)."
  (interactive)
  (ygg-each-selection-update
   (ygg--motion (lambda ()
                  (end-of-line)
                  (unless (bolp) (backward-char 1))))))

(defun ygg-goto-first-non-blank ()
  (interactive)
  (ygg-each-selection-update (ygg--motion #'back-to-indentation)))

(defun ygg-0 (n)
  "Line start, or digit-argument continuation when a count is pending."
  (interactive "P")
  (if current-prefix-arg (digit-argument n) (ygg-goto-line-start)))

;;; H M L

(defun ygg-H ()
  (interactive)
  (ygg-each-selection-update (ygg--motion (lambda () (move-to-window-line 0)))))

(defun ygg-M ()
  (interactive)
  (ygg-each-selection-update (ygg--motion (lambda () (move-to-window-line nil)))))

(defun ygg-L ()
  (interactive)
  (ygg-each-selection-update (ygg--motion (lambda () (move-to-window-line -1)))))

;;; Jumplist (C-o / C-i) — backed by better-jumper (the evil-free vim
;;; jumplist): per-window, spanning files.  Our motions call ygg--jump-push
;;; before a jump; C-o/C-i and the picker delegate to better-jumper's list.

(declare-function better-jumper-set-jump "better-jumper" (&optional pos))
(declare-function better-jumper-jump-backward "better-jumper" (&optional count))
(declare-function better-jumper-jump-forward "better-jumper" (&optional count))
(declare-function better-jumper-get-jumps "better-jumper" (&optional context))
(declare-function better-jumper-jump-list-struct-ring "better-jumper" (s))

(defun ygg--jump-push ()
  "Record point as a jump origin (called before a jump motion)."
  (setq ygg--mark-last-jump-pos (point-marker))
  (when (fboundp 'better-jumper-set-jump) (better-jumper-set-jump)))

(defun ygg--jump-to (m &optional linewise)
  "Jump to marker M, switching to its buffer first (used by vim marks).
With LINEWISE, land on the target line's first non-blank (vim `')."
  (let ((buf (marker-buffer m)) (pos (marker-position m)))
    (when (and buf (buffer-live-p buf) pos)
      (unless (eq buf (current-buffer)) (switch-to-buffer buf))
      (ygg-each-selection-update
       (ygg--motion (lambda ()
                      (goto-char pos)
                      (when linewise (back-to-indentation))))))))

(defun ygg-jump-back (&optional count)
  "Go to the previous position in the jump list (vim C-o)."
  (interactive "p")
  (if (fboundp 'better-jumper-jump-backward)
      (better-jumper-jump-backward count)
    (user-error "Jump list not ready (better-jumper still installing)")))

(defun ygg-jump-forward (&optional count)
  "Go to the next position in the jump list (vim C-i)."
  (interactive "p")
  (if (fboundp 'better-jumper-jump-forward)
      (better-jumper-jump-forward count)
    (user-error "Jump list not ready (better-jumper still installing)")))

(defun ygg-jumplist-pick ()
  "Pick from the jump list (vim :jumps); candidates show file and position."
  (interactive)
  (unless (fboundp 'better-jumper-get-jumps)
    (user-error "Jump list not ready"))
  (require 'ring)
  (let* ((entries (ring-elements
                   (better-jumper-jump-list-struct-ring (better-jumper-get-jumps))))
         (_ (unless entries (user-error "Jump list empty")))
         (cands (mapcar
                 (lambda (e)
                   (cons (format "%-44s %d"
                                 (abbreviate-file-name (or (car e) "?")) (cadr e))
                         e))
                 entries))
         (choice (completing-read "Jump: " (mapcar #'car cands) nil t))
         (e (cdr (assoc choice cands)))
         (target (car e)) (pos (cadr e)))
    (ygg--jump-push)
    (cond ((and target (file-exists-p target)) (find-file target))
          ((and target (get-buffer target)) (switch-to-buffer target)))
    (goto-char (min (or pos (point)) (point-max)))))

;;; Vim marks: g m {char} sets, ' {char} jumps

(defvar-local ygg--marks-local nil
  "Hash table CHAR -> marker, for a-z buffer-local marks.")
(defvar ygg--marks-global nil
  "A-Z global marks: alist CHAR -> (FILE . POS) for file buffers, or
CHAR -> marker for non-file buffers (traces, scratch, …).")
(defvar-local ygg--mark-last-yank-beg nil
  "Start of the last yank/change region, for '[ mark.")
(defvar-local ygg--mark-last-yank-end nil
  "End of the last yank/change region, for '] mark.")

(defun ygg--record-yank-boundaries (regions)
  "Record the boundaries of a yank/change operation from REGIONS."
  (when regions
    (let ((beg (apply #'min (mapcar #'car regions)))
          (end (apply #'max (mapcar #'cadr regions))))
      (setq ygg--mark-last-yank-beg (copy-marker beg)
            ygg--mark-last-yank-end (copy-marker end)))))

(defun ygg-mark-set-char (ch)
  "Set mark CH at point."
  (cond
   ((and (>= ch ?a) (<= ch ?z))
    (unless ygg--marks-local (setq ygg--marks-local (make-hash-table :test 'eql)))
    (puthash ch (point-marker) ygg--marks-local))
   ((and (>= ch ?A) (<= ch ?Z))
    ;; file buffers store (FILE . POS) so the mark survives kill/reopen;
    ;; non-file buffers (traces, scratch) store a live marker instead
    (let ((val (if buffer-file-name (cons buffer-file-name (point)) (point-marker)))
          (cell (assq ch ygg--marks-global)))
      (if cell (setcdr cell val)
        (push (cons ch val) ygg--marks-global))))
   (t (user-error "Invalid mark: %c" ch))))

(defun ygg-mark-set ()
  (interactive)
  (ygg-mark-set-char (read-char)))

(defvar ygg--marks-global-saved nil
  "The file-backed global marks, as savehist stores them.")

(defun ygg--marks-global-stash ()
  (setq ygg--marks-global-saved
        (seq-filter (lambda (cell) (and (consp (cdr cell)) (stringp (cadr cell))))
                    ygg--marks-global)))

(defun ygg--marks-global-restore ()
  (dolist (cell ygg--marks-global-saved)
    (unless (assq (car cell) ygg--marks-global)
      (push cell ygg--marks-global))))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'ygg--marks-global-saved)
  (add-hook 'savehist-save-hook #'ygg--marks-global-stash)
  (add-hook 'savehist-mode-hook #'ygg--marks-global-restore))

(defun ygg--mark-file-target (file)
  "Open buffer visiting FILE, else FILE; never contacts a remote host."
  (or (if (find-file-name-handler file 'file-exists-p)
          (seq-find (lambda (b) (equal (buffer-local-value 'buffer-file-name b) file))
                    (buffer-list))
        (find-buffer-visiting file))
      file))

(defun ygg-mark-position (ch)
  "Where mark CH points, as (BUFFER-OR-FILE . POS); `user-error' when unset.
Pure: no jump, window or selection work.  BUFFER-OR-FILE is a live buffer, or
a file name for a global mark whose file is not open."
  (cl-flet ((marker-pos (m)
              (if (and (markerp m) (marker-buffer m))
                  (cons (marker-buffer m) (marker-position m))
                (user-error "Mark %c not set" ch))))
    (cond
     ((memq ch '(?' ?`))
      (marker-pos ygg--mark-last-jump-pos))
     ((eq ch ?.)
      (unless ygg--change-list (user-error "Change list empty"))
      (cons (current-buffer) (car ygg--change-list)))
     ((eq ch ?^) (marker-pos ygg--last-insert))
     ((eq ch ?\[) (marker-pos ygg--mark-last-yank-beg))
     ((eq ch ?\]) (marker-pos ygg--mark-last-yank-end))
     ((and (>= ch ?a) (<= ch ?z))
      (marker-pos (and ygg--marks-local (gethash ch ygg--marks-local))))
     ((and (>= ch ?A) (<= ch ?Z))
      (let ((entry (cdr (assq ch ygg--marks-global))))
        (cond
         ((null entry) (user-error "Mark %c not set" ch))
         ((markerp entry)
          (if (marker-buffer entry) (marker-pos entry)
            (user-error "Mark %c: buffer is gone" ch)))
         (t (cons (ygg--mark-file-target (car entry)) (cdr entry))))))
     (t (user-error "Invalid mark: %c" ch)))))

(defun ygg-mark-goto (ch &optional linewise)
  "Jump to mark CH, recording the jump; LINEWISE lands on the first non-blank."
  (when (and linewise (eq ch ?')) (user-error "'' is already linewise"))
  (pcase-let ((`(,target . ,pos) (ygg-mark-position ch)))
    (ygg--jump-push)
    (if (stringp target) (find-file target)
      (unless (eq target (current-buffer)) (switch-to-buffer target)))
    (ygg-each-selection-update
     (ygg--motion (lambda ()
                    (goto-char (min (max pos (point-min)) (point-max)))
                    (when (or linewise (eq ch ?')) (back-to-indentation)))))))

(defun ygg-mark-jump (&optional linewise)
  "Jump to the mark named by the next key.
LINEWISE (vim `') lands on the line's first non-blank; without it (vim
backtick) the exact stored position. Special marks: '' (last jump),
'. (last change), '^ (last insert exit), '[ (yank start), '] (yank end)."
  (interactive)
  (ygg-mark-goto (read-char) linewise))

(defun ygg-mark-jump-line ()
  "Jump to a mark's line, first non-blank (vim `')."
  (interactive)
  (ygg-mark-jump t))

;;; goto-map: gg ge gd gn gp ga gc g.

(defun ygg-goto-first (&optional n)
  (interactive "P")
  (ygg--jump-push)
  (ygg-each-selection-update
   (ygg--motion (lambda ()
                  (goto-char (point-min))
                  (when n (forward-line (1- (prefix-numeric-value n))))))))

(defun ygg-goto-last ()
  "Start of the last line (Helix ge)."
  (interactive)
  (ygg--jump-push)
  (ygg-each-selection-update
   (ygg--motion (lambda ()
                  (goto-char (point-max))
                  (when (and (bolp) (not (bobp))) (forward-line -1))))))

(defun ygg-goto-definition ()
  (interactive)
  (ygg--jump-push)
  (call-interactively #'xref-find-definitions))

(declare-function ffap-file-exists-string "ffap" (file))

(defun ygg--refuse-remote (name)
  (when (and name (file-remote-p name) (not (file-remote-p default-directory)))
    (user-error "Remote path refused: %s" name)))

(defun ygg--project-base ()
  (when-let* ((pr (project-current nil)))
    (project-root pr)))

(defun ygg--existing-file (name)
  (ygg--refuse-remote name)
  (or (let ((f (expand-file-name name)))
        (and (file-exists-p f) f))
      (when-let* ((root (ygg--project-base)))
        (let ((f (expand-file-name name root)))
          (and (file-exists-p f) f)))
      (ffap-file-exists-string name)))

(defun ygg-goto-file ()
  "Open the file path in the selection, or under point (vim/helix gf)."
  (interactive)
  (require 'ffap)
  (let* ((sel (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
                (when (> (- end beg) 1)
                  (string-trim (buffer-substring-no-properties beg end)))))
         (raw (or (and sel (not (string-empty-p sel)) sel)
                  (thing-at-point 'filename t))))
    (ygg--refuse-remote raw)
    (let* ((name (or (and sel (not (string-empty-p sel)) sel)
                     (ffap-guesser)
                     raw))
           (_ (ygg--refuse-remote name))
           (target (and name (ffap-file-at-point))))
      (cond
       ((and target (file-exists-p target)) (ygg--jump-push) (find-file target))
       ((and name (ygg--existing-file name))
        (ygg--jump-push) (find-file (ygg--existing-file name)))
       (name (ygg--jump-push) (ffap name))
       (t (user-error "No file path at point"))))))

(defconst ygg--file-line-re
  (rx bos (group (+? nonl))
      (or (seq ":" (group (+ digit)) (? ":" (group (+ digit))))
          (seq "(" (group (+ digit)) (? "," (group (+ digit))) ")"))
      (* (any ",:.;")) eos))

(defun ygg--file-line-parse (text)
  "Return (FILE LINE COL) from TEXT like foo.el:12:3, foo.el(12), or foo.el."
  (if (string-match ygg--file-line-re text)
      (list (match-string 1 text)
            (string-to-number (or (match-string 2 text) (match-string 4 text)))
            (let ((c (or (match-string 3 text) (match-string 5 text))))
              (and c (string-to-number c))))
    (list (string-trim-right text "[,:.;]+") nil nil)))

(defun ygg--file-line-text ()
  (or (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
        (when (> (- end beg) 1)
          (string-trim (buffer-substring-no-properties beg end))))
      (save-excursion
        (let* ((chars "^ \t\n\"'<>")
               (beg (progn (skip-chars-backward chars) (point)))
               (end (progn (skip-chars-forward chars) (point)))
               (text (buffer-substring-no-properties beg end)))
          (if (and (string-match-p "\\`[^:(]+\\'" text)
                   (looking-at "[\"']?,?[ \t]+\\(?:line[ \t]+\\)?\\([0-9]+\\)"))
              (concat text ":" (match-string 1))
            text)))))

(defun ygg-goto-file-line ()
  "Open the file under point at the line after it (vim gF)."
  (interactive)
  (require 'ffap)
  (pcase-let* ((`(,name ,line ,col) (ygg--file-line-parse (ygg--file-line-text)))
               (file (and (not (string-empty-p name)) (ygg--existing-file name))))
    (unless file (user-error "No file path at point"))
    (ygg--jump-push)
    (find-file file)
    (when line
      (goto-char (point-min))
      (forward-line (1- line))
      (when col (move-to-column (max 0 (1- col)))))))

(defun ygg-goto-next-buffer () (interactive) (next-buffer))
(defun ygg-goto-prev-buffer () (interactive) (previous-buffer))
(defun ygg-goto-other-buffer () (interactive) (mode-line-other-buffer))

(defun ygg-goto-comment ()
  (interactive)
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg end _dir)
       (comment-or-uncomment-region
        (save-excursion (goto-char beg) (line-beginning-position))
        (save-excursion (goto-char end) (line-end-position)))))))

(defun ygg--undo-entry-pos (entry)
  (cond ((integerp entry) entry)
        ((not (consp entry)) nil)
        ((integerp (car entry)) (car entry))
        ((markerp (car entry)) (marker-position (car entry)))
        ((integerp (cdr entry)) (abs (cdr entry)))))

(defun ygg-goto-last-change ()
  (interactive)
  (ygg--jump-push)
  (let ((pos (seq-some #'ygg--undo-entry-pos buffer-undo-list)))
    (unless pos (user-error "No changes recorded"))
    (ygg-each-selection-update
     (ygg--motion (lambda () (goto-char (min (max pos (point-min)) (point-max))))))))

;;; Change list: g ; (older) / g , (newer)

(defvar-local ygg--change-list-idx nil
  "Walk index into `ygg--change-list'; reset by any buffer modification.")
(defconst ygg--change-list-max 100)

(defun ygg--change-list-record (beg _end _len)
  "Push BEG, or fold into the head entry when it shares its line (typing).
Same-line check is a bol/eol probe around the head position, not
`line-number-at-pos' — that counts from point-min and would make every
keystroke O(buffer size)."
  (if (and ygg--change-list
           (save-excursion
             ;; head is a raw position; deletions can shrink the buffer past it
             (goto-char (min (car ygg--change-list) (point-max)))
             (<= (line-beginning-position) beg (line-end-position))))
      (setcar ygg--change-list beg)
    (push beg ygg--change-list)
    (when (> (length ygg--change-list) ygg--change-list-max)
      (setcdr (nthcdr (1- ygg--change-list-max) ygg--change-list) nil)))
  (setq ygg--change-list-idx nil))

(defun ygg--record-change-bounds (beg end _len)
  "Record change boundaries for '[ and '] marks."
  (unless (markerp ygg--mark-last-yank-beg) (setq ygg--mark-last-yank-beg (make-marker)))
  (unless (markerp ygg--mark-last-yank-end) (setq ygg--mark-last-yank-end (make-marker)))
  (set-marker ygg--mark-last-yank-beg beg)
  (set-marker ygg--mark-last-yank-end end))

(add-hook 'yggdrasil-local-mode-hook
          (lambda ()
            (if yggdrasil-local-mode
                (progn (add-hook 'after-change-functions #'ygg--change-list-record nil t)
                       (add-hook 'after-change-functions #'ygg--record-change-bounds nil t))
              (progn (remove-hook 'after-change-functions #'ygg--change-list-record t)
                     (remove-hook 'after-change-functions #'ygg--record-change-bounds t)))))

(defun ygg--change-list-goto (idx)
  (setq ygg--change-list-idx idx)
  (let ((pos (nth idx ygg--change-list)))
    (ygg-each-selection-update
     (ygg--motion (lambda () (goto-char (min (max pos (point-min)) (point-max))))))))

(defun ygg-change-list-older ()
  (interactive)
  (unless ygg--change-list (user-error "Change list empty"))
  (let ((idx (1+ (or ygg--change-list-idx -1))))
    (if (>= idx (length ygg--change-list))
        (user-error "No older changes")
      (ygg--change-list-goto idx))))

(defun ygg-change-list-newer ()
  (interactive)
  (unless (and ygg--change-list-idx (> ygg--change-list-idx 0))
    (user-error "No newer changes"))
  (ygg--change-list-goto (1- ygg--change-list-idx)))

;;; G

(defun ygg-goto-last-line (&optional n)
  (interactive "P")
  (ygg--jump-push)
  (ygg-each-selection-update
   (ygg--motion (lambda ()
                  (if n
                      (progn (goto-char (point-min)) (forward-line (1- (prefix-numeric-value n))))
                    (goto-char (point-max))
                    (when (and (bolp) (not (bobp))) (forward-line -1))
                    (beginning-of-line))))))

;;; view-map: zz zt zb zj zk

(defun ygg-view-center () (interactive) (recenter))
(defun ygg-view-top () (interactive) (recenter 0))
(defun ygg-view-bottom () (interactive) (recenter -1))
(defun ygg-view-down () (interactive) (scroll-up-line))
(defun ygg-view-up () (interactive) (scroll-down-line))

(defun ygg-narrow-indirect ()
  "Edit the selection in isolation via an indirect, narrowed clone (z n)."
  (interactive)
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (deactivate-mark)
    (let ((buf (clone-indirect-buffer
                (generate-new-buffer-name (format "%s<narrow>" (buffer-name)))
                nil)))
      (with-current-buffer buf
        (narrow-to-region beg end)
        (goto-char (point-min)))
      (switch-to-buffer buf)
      (ygg-normal-state))))

(defun ygg-widen-indirect ()
  "Kill an indirect narrowing clone and return to its base buffer (z w);
in a normal buffer just widen."
  (interactive)
  (let ((base (buffer-base-buffer)))
    (if base
        (let ((clone (current-buffer)))
          (switch-to-buffer base)
          (kill-buffer clone))
      (widen))))

(defvar ygg-view-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "z" #'ygg-view-center)
    (define-key map "t" #'ygg-view-top)
    (define-key map "b" #'ygg-view-bottom)
    (define-key map "j" #'ygg-view-down)
    (define-key map "k" #'ygg-view-up)
    map)
  "Sticky Helix Z view: repeat.el keymap entered after any zz/zt/zb/zj/zk.")

(dolist (cmd '(ygg-view-center ygg-view-top ygg-view-bottom ygg-view-down ygg-view-up))
  (put cmd 'repeat-map 'ygg-view-repeat-map))

(unless (or noninteractive repeat-mode) (repeat-mode 1))

(defvar ygg-change-list-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";" #'ygg-change-list-older)
    (define-key map "," #'ygg-change-list-newer)
    map)
  "Sticky change list: ; older, , newer after g ; or g ,.")

(defvar ygg-number-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") #'ygg-number-increment)
    (define-key map (kbd "C-x") #'ygg-number-decrement)
    map)
  "Sticky C-a / C-x.")

(defvar ygg-number-sequential-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") #'ygg-number-increment-sequential)
    (define-key map (kbd "C-x") #'ygg-number-decrement-sequential)
    map)
  "Sticky g C-a / g C-x.")

(dolist (spec '((ygg-change-list-older . ygg-change-list-repeat-map)
                (ygg-change-list-newer . ygg-change-list-repeat-map)
                (ygg-number-increment . ygg-number-repeat-map)
                (ygg-number-decrement . ygg-number-repeat-map)
                (ygg-number-increment-sequential . ygg-number-sequential-repeat-map)
                (ygg-number-decrement-sequential . ygg-number-sequential-repeat-map)))
  (put (car spec) 'repeat-map (cdr spec)))

;;; Folds beyond hideshow/outline: recursion, reveal, manual zf folds

(declare-function hs-hide-block "hideshow")
(declare-function hs-find-block-beginning "hideshow")
(declare-function outline-hide-subtree "outline")
(declare-function outline-show-subtree "outline")
(declare-function outline-show-entry "outline")
(declare-function outline-show-children "outline")
(declare-function outline-back-to-heading "outline")
(declare-function outline-up-heading "outline")
(declare-function outline-next-heading "outline")
(declare-function outline-previous-heading "outline")
(declare-function ygg-fold-toggle "yggdrasil-verbs")
(declare-function ygg-fold-close "yggdrasil-verbs")
(declare-function ygg-fold-open "yggdrasil-verbs")
(declare-function ygg-fold-close-all "yggdrasil-verbs")
(declare-function ygg-fold-open-all "yggdrasil-verbs")
(defvar hs-minor-mode)
(defvar hs-allow-nesting)
(defvar hs-find-block-beginning-function)
(declare-function hs-get-first-block-on-line "hideshow")
(declare-function hs-block-positions "hideshow")
(declare-function hs-hide-block-at-point "hideshow")
(declare-function hs-hideable-block-p "hideshow")
(declare-function treesit-hs-find-next-block "treesit")
(declare-function treesit-navigate-thing "treesit")
(declare-function treesit-search-forward "treesit")
(declare-function treesit-node-at "treesit")
(declare-function treesit-thing-at "treesit")
(defvar hs-find-next-block-function)
(defvar hs-treesit-things)

(defun ygg-fold--outline-p ()
  (bound-and-true-p outline-minor-mode))

(defun ygg-fold--hs-on ()
  (require 'hideshow)
  (unless hs-minor-mode (hs-minor-mode 1)))

(defun ygg-fold--hs-block-start-p ()
  "Non-nil, leaving point on the line's first block opener, when one starts here."
  (let ((pos (save-excursion (beginning-of-line) (hs-get-first-block-on-line))))
    (when pos (goto-char pos) t)))

(defun ygg-fold--manual-p (ov)
  (overlay-get ov 'ygg-fold))

(defun ygg-fold--manual-in (beg end)
  (seq-filter #'ygg-fold--manual-p (overlays-in beg end)))

(defun ygg-fold--manual-here ()
  "Innermost manual fold on the current line or covering point."
  (let ((cands (ygg-fold--manual-in (line-beginning-position)
                                    (min (1+ (line-end-position)) (point-max)))))
    (car (sort cands (lambda (a b)
                       (< (- (overlay-end a) (overlay-start a))
                          (- (overlay-end b) (overlay-start b))))))))

(defun ygg-fold--manual-set (ov closed)
  (overlay-put ov 'invisible (and closed 'ygg-fold)))

(defun ygg-fold--manual-closed-p (ov)
  (overlay-get ov 'invisible))

(defun ygg-fold--manual-tree (ov)
  (cons ov (seq-filter (lambda (o) (and (not (eq o ov))
                                        (>= (overlay-start o) (overlay-start ov))
                                        (<= (overlay-end o) (overlay-end ov))))
                       (ygg-fold--manual-in (overlay-start ov) (overlay-end ov)))))

(defun ygg-fold-create ()
  "Fold the lines of the selection (z f): manual, shown as an ellipsis."
  (interactive)
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (let ((from (save-excursion (goto-char beg) (line-end-position)))
          (to (save-excursion (goto-char (if (> end beg) (1- end) end))
                              (line-end-position))))
      (when (<= to from) (user-error "A fold needs at least two lines"))
      (deactivate-mark)
      (add-to-invisibility-spec '(ygg-fold . t))
      (let ((ov (make-overlay from to nil t nil)))
        (overlay-put ov 'ygg-fold t)
        (overlay-put ov 'evaporate t)
        (ygg-fold--manual-set ov t)
        (goto-char (save-excursion (goto-char from) (line-beginning-position)))))))

(defun ygg-fold-delete ()
  "Delete the manual fold at point (z d)."
  (interactive)
  (let ((ov (ygg-fold--manual-here)))
    (unless ov (user-error "No manual fold here"))
    (delete-overlay ov)))

(defun ygg-fold-delete-all ()
  "Delete every manual fold in the buffer (z E)."
  (interactive)
  (mapc #'delete-overlay (ygg-fold--manual-in (point-min) (point-max))))

(defun ygg-fold--hs-bounds ()
  (save-excursion
    (end-of-line)
    (let ((hidden (seq-find (lambda (o) (overlay-get o 'hs))
                            (overlays-at (point)))))
      (if hidden
          (cons (line-beginning-position) (overlay-end hidden))
        (beginning-of-line)
        (when (or (ygg-fold--hs-block-start-p)
                  (funcall hs-find-block-beginning-function))
          (when-let* ((end (cadr (hs-block-positions))))
            (cons (point) end)))))))

(defun ygg-fold--hs-valid-start-p (pos)
  (save-excursion
    (goto-char pos)
    (and (not (nth 8 (syntax-ppss))) (hs-hideable-block-p))))

(defun ygg-fold--hs-treesit-p ()
  (eq hs-find-next-block-function #'treesit-hs-find-next-block))

(defun ygg-fold--ts-candidate-p (node)
  (save-excursion
    (goto-char (treesit-node-start node))
    (and (< (pos-eol) (treesit-node-end node))
         (treesit-node-match-p node hs-treesit-things))))

(defun ygg-fold--ts-scan-back (from test)
  (let ((node (treesit-node-at (max (point-min) (1- from))))
        (last from))
    (lambda ()
      (let (found)
        (while (and (not found) node)
          (let ((n node))
            (setq node (treesit-search-forward node #'ygg-fold--ts-candidate-p t))
            (when (ygg-fold--ts-candidate-p n)
              (let ((p (treesit-node-start n)))
                (when (and (< p last) (funcall test p) (ygg-fold--hs-valid-start-p p))
                  (setq found p last p))))))
        found))))

(defun ygg-fold--ts-scan (from backward test)
  "Closure yielding successive valid block openers from FROM, forward or BACKWARD."
  (if backward
      (ygg-fold--ts-scan-back from test)
    (ygg-fold--ts-scan-fwd from test)))

(defun ygg-fold--ts-scan-fwd (from test)
  (let ((pos (max (point-min) (1- from)))
        (head (and (<= from (point-min))
                   (when-let* ((th (treesit-thing-at from #'ygg-fold--ts-candidate-p)))
                     (and (= (treesit-node-start th) from) from)))))
    (lambda ()
      (let (found next)
        (while (and (not found)
                    (setq next (or (prog1 head (setq head nil))
                                   (treesit-navigate-thing pos 1 'beg #'ygg-fold--ts-candidate-p)))
                    (funcall test next))
          (setq pos (1+ next))
          (when (ygg-fold--hs-valid-start-p next) (setq found next)))
        found))))

(defun ygg-fold--hs-starts (beg end &optional first)
  "Hideable block openers between BEG and END in order; just one when FIRST."
  (save-excursion
    (if (ygg-fold--hs-treesit-p)
        (let ((next (ygg-fold--ts-scan beg nil (lambda (s) (and (>= s beg) (<= s end)))))
              acc s)
          (while (and (not (and first acc)) (setq s (funcall next))) (push s acc))
          (nreverse acc))
      (goto-char beg)
      (let (acc)
        (while (and (not (and first acc))
                    (funcall hs-find-next-block-function hs-block-start-regexp end nil))
          (let ((s (match-beginning 0)))
            (when (ygg-fold--hs-valid-start-p s) (push s acc))))
        (nreverse acc)))))

(defun ygg-fold--hs-closed-at-p (pos)
  (save-excursion
    (goto-char pos)
    (seq-some (lambda (o) (and (overlay-get o 'hs) (>= (overlay-start o) pos)))
              (overlays-in pos (1+ (line-end-position))))))

(defun ygg-fold--hs-each-block (beg end fn)
  (save-excursion
    (dolist (s (nreverse (ygg-fold--hs-starts beg end)))
      (goto-char s)
      (funcall fn))))

(defun ygg-fold--lisp-openers (from to first)
  "Parens opened and still open at the end of their line between FROM and TO.
One incremental parse, not a syntax check per candidate; FIRST stops at one."
  (save-excursion
    (let ((st (syntax-ppss from)) (pos from) acc)
      (while (and (< pos to) (not (and first acc)))
        (let ((start pos)
              (next (min to (save-excursion (goto-char pos) (line-beginning-position 2)))))
          (setq st (parse-partial-sexp start next nil nil st) pos next)
          (dolist (p (nth 9 st))
            (when (and (>= p start)
                       (save-excursion (goto-char p) (looking-at hs-block-start-regexp)))
              (push p acc)))
          (when (and (nth 3 st) (>= (nth 8 st) start)
                     (save-excursion (goto-char (nth 8 st)) (looking-at hs-block-start-regexp)))
            (push (nth 8 st) acc))))
      (nreverse acc))))

(defun ygg-fold--lisp-next-start (dir limit)
  (if (> dir 0)
      (let ((from limit) s)
        (while (and (not s) (< from (point-max)))
          (let ((cands (ygg-fold--lisp-openers from (point-max) t)))
            (if (null cands)
                (setq from (point-max))
              (if (ygg-fold--hs-valid-start-p (car cands))
                  (setq s (car cands))
                (setq from (save-excursion (goto-char (car cands)) (line-beginning-position 2)))))))
        s)
    (let* ((open (car (last (nth 9 (syntax-ppss limit)))))
           (cands (nreverse (ygg-fold--lisp-openers (or open (point-min)) limit nil))))
      (seq-find #'ygg-fold--hs-valid-start-p cands))))

(defun ygg-fold--hs-next-start (dir)
  "Line start of the nearest hideable block opener in direction DIR."
  (save-excursion
    (let ((limit (if (> dir 0) (line-beginning-position 2) (line-beginning-position)))
          s)
      (cond
       ((ygg-fold--hs-treesit-p)
        (setq s (funcall (ygg-fold--ts-scan limit (< dir 0)
                                            (if (> dir 0)
                                                (lambda (p) (>= p limit))
                                              (lambda (p) (< p limit)))))))
       ((derived-mode-p 'lisp-data-mode) (setq s (ygg-fold--lisp-next-start dir limit)))
       ((> dir 0) (setq s (car (ygg-fold--hs-starts limit (point-max) t))))
       (t (goto-char limit)
          (while (and (not s) (re-search-backward hs-block-start-regexp nil t))
            (when (ygg-fold--hs-valid-start-p (match-beginning 0))
              (setq s (match-beginning 0))))))
      (when s (goto-char s) (line-beginning-position)))))

(defun ygg-fold--hidden-p ()
  (let ((ov (ygg-fold--manual-here)))
    (if ov (ygg-fold--manual-closed-p ov) (invisible-p (line-end-position)))))

(defun ygg-fold-close-recursive ()
  "Close the fold at point and every fold nested in it (z C)."
  (interactive)
  (let ((ov (ygg-fold--manual-here)))
    (cond
     (ov (dolist (o (ygg-fold--manual-tree ov)) (ygg-fold--manual-set o t)))
     ((ygg-fold--outline-p) (outline-hide-subtree))
     (t (ygg-fold--hs-on)
        (when-let* ((b (ygg-fold--hs-bounds)))
          (let ((hs-allow-nesting t))
            (ygg-fold--hs-each-block (car b) (cdr b)
                                     (lambda ()
                                       (unless (ygg-fold--hs-closed-at-p (point))
                                         (save-excursion (hs-hide-block-at-point)))))))))))

(defun ygg-fold-open-recursive ()
  "Open the fold at point and every fold nested in it (z O)."
  (interactive)
  (let ((ov (ygg-fold--manual-here)))
    (cond
     (ov (dolist (o (ygg-fold--manual-tree ov)) (ygg-fold--manual-set o nil)))
     ((ygg-fold--outline-p) (outline-show-subtree))
     (t (ygg-fold--hs-on)
        (when-let* ((b (ygg-fold--hs-bounds)))
          (dolist (o (overlays-in (car b) (cdr b)))
            (when (overlay-get o 'hs) (delete-overlay o))))))))

(defun ygg-fold-toggle-recursive ()
  "Toggle the fold at point and everything nested in it (z A)."
  (interactive)
  (if (ygg-fold--hidden-p)
      (ygg-fold-open-recursive)
    (ygg-fold-close-recursive)))

(defun ygg-fold-reveal ()
  "Open every fold hiding point (z v)."
  (interactive)
  (let ((pos (point)))
    (dolist (o (overlays-at pos))
      (when (and (> pos (overlay-start o)) (overlay-get o 'invisible))
        (cond ((ygg-fold--manual-p o) (ygg-fold--manual-set o nil))
              ((overlay-get o 'hs) (delete-overlay o)))))
    (when (and (ygg-fold--outline-p) (invisible-p pos))
      (outline-back-to-heading t)
      (outline-show-entry)
      (condition-case nil
          (while t (outline-up-heading 1 t) (outline-show-children))
        (error nil)))))

(defun ygg-fold-reset ()
  "Close every fold, then open the ones hiding point (z x)."
  (interactive)
  (ygg-fold-close-all)
  (ygg-fold-reveal))

(defun ygg-fold--next-start (dir)
  "Start position of the nearest fold in direction DIR (1 or -1), or nil."
  (let* ((manual (mapcar (lambda (o) (save-excursion (goto-char (overlay-start o))
                                                     (line-beginning-position)))
                         (ygg-fold--manual-in (point-min) (point-max))))
         (backend (save-excursion
                    (if (ygg-fold--outline-p)
                        (and (if (> dir 0) (outline-next-heading) (outline-previous-heading))
                             (point))
                      (ygg-fold--hs-on)
                      (ygg-fold--hs-next-start dir))))
         (all (seq-filter (lambda (p) (if (> dir 0)
                                          (> p (line-end-position))
                                        (< p (line-beginning-position))))
                          (delq nil (cons backend manual)))))
    (and all (if (> dir 0) (apply #'min all) (apply #'max all)))))

(defun ygg-fold-next ()
  "Go to the start of the next fold (] z)."
  (interactive)
  (let ((pos (or (ygg-fold--next-start 1) (user-error "No next fold"))))
    (ygg--jump-push)
    (ygg-each-selection-update (ygg--motion (lambda () (goto-char pos))))))

(defun ygg-fold-prev ()
  "Go to the start of the previous fold ([ z)."
  (interactive)
  (let ((pos (or (ygg-fold--next-start -1) (user-error "No previous fold"))))
    (ygg--jump-push)
    (ygg-each-selection-update (ygg--motion (lambda () (goto-char pos))))))

(yggdrasil-define-keys 'normal
  "z A" #'ygg-fold-toggle-recursive :label "fold toggle (recursive)"
  "z C" #'ygg-fold-close-recursive :label "fold close (recursive)"
  "z O" #'ygg-fold-open-recursive :label "fold open (recursive)"
  "z v" #'ygg-fold-reveal :label "reveal cursor"
  "z x" #'ygg-fold-reset :label "reset folds"
  "z f" #'ygg-fold-create :label "create fold"
  "z d" #'ygg-fold-delete :label "delete fold"
  "z E" #'ygg-fold-delete-all :label "delete all folds"
  "] z" #'ygg-fold-next :label "next fold"
  "[ z" #'ygg-fold-prev :label "prev fold")

;;; Search: / ? n N * # — n follows the search direction, N opposes it (nvim)

(defvar-local ygg--last-search nil)
(defvar-local ygg--search-dir 1)

(defun ygg--isearch-pcre (string &optional _lax)
  "`isearch-regexp-function' for `/'/`?': live PCRE-to-elisp per keystroke."
  (ygg-regexp string))

(defun ygg-search-forward ()
  (interactive)
  (ygg--jump-push)
  (isearch-mode t t nil t #'ygg--isearch-pcre))

(defun ygg-search-backward ()
  (interactive)
  (ygg--jump-push)
  (isearch-mode nil t nil t #'ygg--isearch-pcre))

(defun ygg--isearch-finish ()
  (when (and (not isearch-mode-end-hook-quit)
             (or (ygg-normal-p) (ygg-visual-p))
             isearch-other-end)
    ;; isearch-string is the raw typed pattern; convert once here so
    ;; n/N never re-run it through pcre2el (double conversion breaks).
    (setq ygg--last-search (ygg-regexp isearch-string)
          ygg--search-dir (if isearch-forward 1 -1))
    (ygg-set-selection isearch-other-end (point))))

(add-hook 'isearch-mode-end-hook #'ygg--isearch-finish)

(defun ygg--research (forward)
  (unless ygg--last-search (user-error "No previous search"))
  (ygg--jump-push)
  (unless (if forward (re-search-forward ygg--last-search nil t)
            (re-search-backward ygg--last-search nil t))
    (goto-char (if forward (point-min) (point-max)))
    (unless (if forward (re-search-forward ygg--last-search nil t)
              (re-search-backward ygg--last-search nil t))
      (user-error "search failed: %s" ygg--last-search)))
  (if forward
      (ygg-set-selection (match-beginning 0) (match-end 0))
    (ygg-set-selection (match-end 0) (match-beginning 0)))
  (recenter))

(defun ygg-search-next ()
  "Repeat the last search in its own direction."
  (interactive)
  (ygg--research (> ygg--search-dir 0))
  (ygg--hlsearch ygg--last-search))

(defun ygg-search-prev ()
  "Repeat the last search against its direction."
  (interactive)
  (ygg--research (< ygg--search-dir 0))
  (ygg--hlsearch ygg--last-search))

(defun ygg-search-word-forward ()
  (interactive)
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (let ((sel (and (> (- end beg) 1)
                    (buffer-substring-no-properties beg end)))
          (word (or (and (> (- end beg) 1)
                         (buffer-substring-no-properties beg end))
                    (thing-at-point 'symbol t))))
      (if word
          (setq ygg--last-search
                (if sel (regexp-quote sel)
                  (concat "\\_<" (regexp-quote word) "\\_>"))
                ygg--search-dir 1)
        (user-error "No word under cursor"))))
  (ygg--research t)
  (ygg--hlsearch ygg--last-search))

(defun ygg-search-word-backward ()
  (interactive)
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (let ((sel (and (> (- end beg) 1)
                    (buffer-substring-no-properties beg end)))
          (word (or (and (> (- end beg) 1)
                         (buffer-substring-no-properties beg end))
                    (thing-at-point 'symbol t))))
      (if word
          (setq ygg--last-search
                (if sel (regexp-quote sel)
                  (concat "\\_<" (regexp-quote word) "\\_>"))
                ygg--search-dir -1)
        (user-error "No word under cursor"))))
  (ygg--research nil)
  (ygg--hlsearch ygg--last-search))

;;; hlsearch: persistent buffer-wide match highlight (vim `hlsearch').
;; Reuses isearch's lazy-highlight so it lights up everywhere isearch runs —
;; normal buffers, dired, magit — from / ? C-s alike; :noh clears it.

;; window-scoped (not whole-buffer): a broad pattern in a huge or growing
;; buffer — a log, a terminal, *Messages* — must not explode into tens of
;; thousands of persistent overlays or rescan on every append. cleanup nil
;; keeps the VISIBLE matches lit until :noh (the vim hlsearch feel).
(setq lazy-highlight-cleanup nil)

(defvar ygg-hlsearch t
  "When non-nil, keep the last search pattern highlighted (vim hlsearch).")

(defun ygg--hlsearch (regexp)
  "Light up matches of REGEXP in the visible window via lazy-highlight."
  (when (and ygg-hlsearch regexp isearch-lazy-highlight)
    (setq isearch-string regexp
          isearch-regexp t
          isearch-forward t
          isearch-regexp-function nil
          isearch-case-fold-search case-fold-search
          isearch-lax-whitespace nil
          isearch-regexp-lax-whitespace nil)
    (isearch-lazy-highlight-new-loop (window-start) (window-end))
    (add-hook 'window-scroll-functions #'ygg--hlsearch-on-scroll nil t)))

(defun ygg--hlsearch-on-scroll (_win _pos)
  "Refresh highlights on window scroll when hlsearch is active."
  (when (and isearch-lazy-highlight-last-string
             (not (equal isearch-lazy-highlight-last-string "")))
    (isearch-lazy-highlight-new-loop (window-start) (window-end))))

(defun ygg-hlsearch-clear ()
  "Clear search highlighting (vim :nohlsearch)."
  (interactive)
  (lazy-highlight-cleanup t)
  (setq isearch-lazy-highlight-last-string nil)
  (remove-hook 'window-scroll-functions #'ygg--hlsearch-on-scroll t))

;;; Scrolling (vim keys): C-d C-u half page, C-f C-b page, C-e C-y line

(defun ygg-scroll-half-down ()
  (interactive)
  (ygg-each-selection-update
   (ygg--motion (lambda () (forward-line (/ (window-height) 2)))))
  (recenter))

(defun ygg-scroll-half-up ()
  (interactive)
  (ygg-each-selection-update
   (ygg--motion (lambda () (forward-line (- (/ (window-height) 2))))))
  (recenter))

(defun ygg-scroll-page-down ()
  (interactive)
  (ygg-each-selection-update
   (ygg--motion (lambda () (scroll-up-command))))
  (recenter))

(defun ygg-scroll-page-up ()
  (interactive)
  (ygg-each-selection-update
   (ygg--motion (lambda () (scroll-down-command))))
  (recenter))

;;; Easymotion (hel-style, on avy): gw gb gW gB gj gk

(defun ygg--avy-ready-p ()
  (or (featurep 'avy) (require 'avy nil t)))

(defun ygg--avy-mark-word (forward words)
  (unless (ygg--avy-ready-p) (user-error "avy is still installing — retry in a moment"))
  (let ((beg (if forward (point) (window-start)))
        (end (if forward (window-end) (point))))
    (avy-jump (if words "[^ \r\n\t]+" "\\b\\sw") :beg beg :end end)
    (if words
        (let ((b (progn (skip-chars-backward "^ \r\n\t") (point)))
              (e (progn (skip-chars-forward "^ \r\n\t") (point))))
          (ygg-set-selection b e))
      (pcase-let ((`(,b . ,e) (or (bounds-of-thing-at-point 'word)
                                  (cons (point) (point)))))
        (ygg-set-selection b e)))))

(defun ygg-easymotion-word-forward () (interactive) (ygg--avy-mark-word t nil))
(defun ygg-easymotion-word-backward () (interactive) (ygg--avy-mark-word nil nil))
(defun ygg-easymotion-WORD-forward () (interactive) (ygg--avy-mark-word t t))
(defun ygg-easymotion-WORD-backward () (interactive) (ygg--avy-mark-word nil t))

(defun ygg-easymotion-line-down ()
  (interactive)
  (unless (ygg--avy-ready-p) (user-error "avy is still installing — retry in a moment"))
  (ygg--jump-push)
  (avy-goto-line-below))

(defun ygg-easymotion-line-up ()
  (interactive)
  (unless (ygg--avy-ready-p) (user-error "avy is still installing — retry in a moment"))
  (ygg--jump-push)
  (avy-goto-line-above))

;;; Bracketed motions (mini.bracketed): b c d e f i q w x

(defun ygg-next-error ()
  (interactive)
  (ygg--record-bracket-motion 1 "d")
  (when (fboundp 'flymake-goto-next-error) (call-interactively #'flymake-goto-next-error)))

(defun ygg-prev-error ()
  (interactive)
  (ygg--record-bracket-motion -1 "d")
  (when (fboundp 'flymake-goto-prev-error) (call-interactively #'flymake-goto-prev-error)))

(defun ygg--bracketed-goto (finder)
  "Jump to FINDER's position (or message), collapsing per motion rules."
  (let ((hit (funcall finder)))
    (if (not hit)
        (message "no target")
      (ygg--jump-push)
      (ygg-each-selection-update (ygg--motion (lambda () (goto-char hit)))))))

(defun ygg--find-comment-line (dir)
  (when comment-start
    (save-excursion
      (let ((re (concat "^[ \t]*" (regexp-quote (string-trim comment-start))))
            hit)
        (while (and (not hit) (zerop (forward-line dir))
                    (not (and (< dir 0) (bobp) (setq dir 0))))
          (when (looking-at re) (setq hit (point))))
        (when (and (not hit) (bobp) (looking-at re)) (setq hit (point)))
        hit))))

(defun ygg-next-comment ()
  (interactive)
  (ygg--record-bracket-motion 1 "c")
  (ygg--bracketed-goto (lambda () (ygg--find-comment-line 1))))

(defun ygg-prev-comment ()
  (interactive)
  (ygg--record-bracket-motion -1 "c")
  (ygg--bracketed-goto (lambda () (ygg--find-comment-line -1))))

(defun ygg--find-indent-line (dir)
  (let ((cur (current-indentation)))
    (when (> cur 0)
      (save-excursion
        (let (hit)
          (while (and (not hit) (zerop (forward-line dir)))
            (when (and (not (looking-at "[ \t]*$"))
                       (< (current-indentation) cur))
              (setq hit (progn (back-to-indentation) (point)))))
          hit)))))

(defun ygg-next-indent ()
  (interactive)
  (ygg--record-bracket-motion 1 "i")
  (ygg--bracketed-goto (lambda () (ygg--find-indent-line 1))))

(defun ygg-prev-indent ()
  (interactive)
  (ygg--record-bracket-motion -1 "i")
  (ygg--bracketed-goto (lambda () (ygg--find-indent-line -1))))

(defun ygg--sibling-file (dir)
  (let ((cur (buffer-file-name)))
    (unless cur (user-error "Buffer visits no file"))
    (let* ((files (seq-filter #'file-regular-p
                              (directory-files (file-name-directory cur)
                                               t "\\`[^.]")))
           (idx (cl-position cur files :test #'string=)))
      (when (and idx (> (length files) 1))
        (nth (mod (+ idx dir) (length files)) files)))))

(defun ygg-next-file ()
  (interactive)
  (ygg--record-bracket-motion 1 "f")
  (let ((f (ygg--sibling-file 1))) (when f (find-file f))))
(defun ygg-prev-file ()
  (interactive)
  (ygg--record-bracket-motion -1 "f")
  (let ((f (ygg--sibling-file -1))) (when f (find-file f))))

(defun ygg-next-window ()
  (interactive)
  (ygg--record-bracket-motion 1 "w")
  (other-window 1))
(defun ygg-prev-window ()
  (interactive)
  (ygg--record-bracket-motion -1 "w")
  (other-window -1))

(defun ygg-next-error-any ()
  "next-error when an error buffer exists, else flymake."
  (interactive)
  (if (ignore-errors (next-error-find-buffer)) (next-error) (ygg-next-error)))
(defun ygg-prev-error-any ()
  (interactive)
  (if (ignore-errors (next-error-find-buffer)) (previous-error) (ygg-prev-error)))

;;; g M — last modified file

(defvar-local ygg--modified-file-ring nil
  "Ring of recently modified file buffers, oldest first.")

(defun ygg--record-buffer-modified ()
  "Record current buffer in the modified file ring (file buffers only)."
  (when (and (buffer-file-name) (not (buffer-modified-p)))
    (unless ygg--modified-file-ring
      (setq ygg--modified-file-ring (make-ring 50)))
    (ring-insert ygg--modified-file-ring (current-buffer))))

(add-hook 'after-save-hook #'ygg--record-buffer-modified)

(defun ygg-goto-last-modified-file ()
  "Go to the most recently modified file buffer."
  (interactive)
  (unless ygg--modified-file-ring (user-error "No modified files in ring"))
  (let* ((current (current-buffer))
         (buffers (ring-elements ygg--modified-file-ring))
         (target (car (delq current buffers))))
    (unless target (user-error "No other modified files"))
    (ygg--jump-push)
    (switch-to-buffer target)))

;;; g J / g K — move by textual line ignoring visual wrap

(defun ygg-goto-textual-line-down (&optional n)
  "Move down N textual lines, ignoring visual wrap."
  (interactive "p")
  (let ((line-move-visual nil))
    (ygg-each-selection-update
     (ygg--motion (lambda () (ygg--vmove n))))))

(defun ygg-goto-textual-line-up (&optional n)
  "Move up N textual lines, ignoring visual wrap."
  (interactive "p")
  (let ((line-move-visual nil))
    (ygg-each-selection-update
     (ygg--motion (lambda () (ygg--vmove (- n)))))))

;;; [ D / ] D — first / last diagnostic in buffer

(defun ygg--find-flymake-diagnostics ()
  "Return sorted list of all flymake diagnostics in current buffer."
  (sort (flymake-diagnostics (point-min) (point-max))
        (lambda (a b) (< (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))))

(defun ygg-goto-first-diagnostic ()
  "Go to first diagnostic in buffer (flymake)."
  (interactive)
  (ygg--record-bracket-motion -1 "D")
  (let ((diags (ygg--find-flymake-diagnostics)))
    (if diags
        (ygg--bracketed-goto (lambda () (flymake-diagnostic-beg (car diags))))
      (message "no diagnostics"))))

(defun ygg-goto-last-diagnostic ()
  "Go to last diagnostic in buffer (flymake)."
  (interactive)
  (ygg--record-bracket-motion 1 "D")
  (let ((diags (ygg--find-flymake-diagnostics)))
    (if diags
        (ygg--bracketed-goto (lambda () (flymake-diagnostic-beg (car (last diags)))))
      (message "no diagnostics"))))

;;; [ p / ] p — previous / next paragraph

(defun ygg-goto-next-paragraph (&optional n)
  "Move to next paragraph N times."
  (interactive "p")
  (ygg--record-bracket-motion 1 "p")
  (ygg--bracketed-goto (lambda () (forward-paragraph n) (point))))

(defun ygg-goto-prev-paragraph (&optional n)
  "Move to previous paragraph N times."
  (interactive "p")
  (ygg--record-bracket-motion -1 "p")
  (ygg--bracketed-goto (lambda () (forward-paragraph (- n)) (point))))

;;; z m — recenter view middle

(defun ygg-view-middle ()
  "Recenter view to middle (same as ygg-view-center, provided for completeness)."
  (interactive)
  (recenter))

;;; z SPC / z DEL — page down/up

(defun ygg-view-page-down ()
  "Page down (half-page scroll)."
  (interactive)
  (ygg-scroll-half-down))

(defun ygg-view-page-up ()
  "Page up (half-page scroll)."
  (interactive)
  (ygg-scroll-half-up))

;;; Bracket repeat via repeat-mode

(defvar ygg--last-bracket-motion nil
  "Stores (direction . letter) of last bracket motion, e.g. (1 . \"d\").")

(defun ygg--record-bracket-motion (dir letter)
  "Record DIR (1 or -1) and LETTER for bracket motion repeats."
  (setq ygg--last-bracket-motion (cons dir letter))
  (when (and dir letter)
    (setq repeat-mode t)))

;;; Bindings

(yggdrasil-define-keys 'normal
  "h" #'ygg-h
  "l" #'ygg-l
  "j" #'ygg-j
  "k" #'ygg-k
  "w" #'ygg-w
  "e" #'ygg-e
  "b" #'ygg-b
  "W" #'ygg-W
  "E" #'ygg-E
  "B" #'ygg-B
  "{" #'ygg-goto-prev-paragraph
  "}" #'ygg-goto-next-paragraph
  "f" #'ygg-find-forward
  "t" #'ygg-till-forward
  "F" #'ygg-find-backward
  "T" #'ygg-till-backward
  ";" #'ygg-repeat-find :label "repeat find"
  "," #'ygg-repeat-find-reverse :label "repeat find back"
  "0" #'ygg-0
  "1" #'digit-argument
  "2" #'digit-argument
  "3" #'digit-argument
  "4" #'digit-argument
  "5" #'digit-argument
  "6" #'digit-argument
  "7" #'digit-argument
  "8" #'digit-argument
  "9" #'digit-argument
  "<home>" #'ygg-goto-line-start
  "<end>" #'ygg-goto-line-end
  "$" #'ygg-goto-line-end
  "^" #'ygg-goto-first-non-blank
  "H" #'ygg-H
  "M" #'ygg-M
  "L" #'ygg-L
  "G" #'ygg-goto-last-line
  "C-d" #'ygg-scroll-half-down
  "C-u" #'ygg-scroll-half-up
  "C-f" #'ygg-scroll-page-down
  "C-b" #'ygg-scroll-page-up
  "C-e" #'scroll-up-line
  "C-y" #'scroll-down-line
  "C-o" #'ygg-jump-back
  "C-i" #'ygg-jump-forward
  "'" #'ygg-mark-jump-line :label "jump to mark (line)"
  "`" #'ygg-mark-jump :label "jump to mark (exact)"
  "/" #'ygg-search-forward :label "search"
  "?" #'ygg-search-backward :label "search back"
  "n" #'ygg-search-next
  "N" #'ygg-search-prev
  "*" #'ygg-search-word-forward
  "#" #'ygg-search-word-backward
  "] b" #'ygg-goto-next-buffer
  "[ b" #'ygg-goto-prev-buffer
  "] d" #'ygg-next-error
  "[ d" #'ygg-prev-error
  "] D" #'ygg-goto-last-diagnostic
  "[ D" #'ygg-goto-first-diagnostic
  "] c" #'ygg-next-comment
  "[ c" #'ygg-prev-comment
  "] i" #'ygg-next-indent
  "[ i" #'ygg-prev-indent
  "] p" #'ygg-goto-next-paragraph
  "[ p" #'ygg-goto-prev-paragraph
  "] q" #'ygg-next-error-any
  "[ q" #'ygg-prev-error-any
  "] f" #'ygg-next-file
  "[ f" #'ygg-prev-file
  "] w" #'ygg-next-window
  "[ w" #'ygg-prev-window)

(yggdrasil-define-keys 'ygg-goto-map
  "g" #'ygg-goto-first :label "buffer start"
  "e" #'ygg-goto-last :label "buffer end"
  "h" #'ygg-goto-line-start :label "line start / column N"
  "l" #'ygg-goto-line-end :label "line end"
  "s" #'ygg-goto-first-non-blank :label "first non-blank"
  "d" #'ygg-goto-definition :label "definition"
  "f" #'ygg-goto-file :label "file under cursor"
  "n" #'ygg-goto-next-buffer :label "next buffer"
  "p" #'ygg-goto-prev-buffer :label "prev buffer"
  "a" #'ygg-goto-other-buffer :label "other buffer"
  "c" #'ygg-goto-comment :label "comment"
  "." #'ygg-goto-last-change :label "last change"
  "m" #'ygg-mark-set :label "set mark"
  "M" #'ygg-goto-last-modified-file :label "last modified file"
  "J" #'ygg-goto-textual-line-down :label "textual line down"
  "K" #'ygg-goto-textual-line-up :label "textual line up"
  "u" #'ygg-downcase :label "downcase"
  "U" #'ygg-upcase :label "upcase"
  ";" #'ygg-change-list-older :label "older change"
  "," #'ygg-change-list-newer :label "newer change"
  "w" #'ygg-easymotion-word-forward :label "hint word →"
  "o" #'ygg-easymotion-word-backward :label "hint word ←"
  "W" #'ygg-easymotion-WORD-forward :label "hint WORD →"
  "O" #'ygg-easymotion-WORD-backward :label "hint WORD ←"
  "H" #'ygg-H :label "window top"
  "b" #'ygg-L :label "window bottom"
  "F" #'ygg-goto-file-line :label "file:line under cursor"
  "j" #'ygg-easymotion-line-down :label "hint line ↓"
  "k" #'ygg-easymotion-line-up :label "hint line ↑")

(yggdrasil-define-keys 'ygg-view-map
  "z" #'ygg-view-center
  "c" #'ygg-view-center :label "center"
  "t" #'ygg-view-top
  "b" #'ygg-view-bottom
  "m" #'ygg-view-middle :label "middle"
  "j" #'ygg-view-down
  "k" #'ygg-view-up
  "SPC" #'ygg-view-page-down :label "page down"
  "DEL" #'ygg-view-page-up :label "page up"
  "n" #'ygg-narrow-indirect :label "narrow (indirect)"
  "w" #'ygg-widen-indirect :label "widen")

(with-eval-after-load 'yggdrasil-leader
  (yggdrasil-leader-def "b j" #'ygg-jumplist-pick "jumplist"))

(provide 'yggdrasil-motions)
;;; yggdrasil-motions.el ends here
