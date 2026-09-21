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

(declare-function ygg-downcase "yggdrasil-verbs")
(declare-function ygg-upcase "yggdrasil-verbs")
(declare-function avy-jump "avy")
(declare-function avy-goto-line-below "avy")
(declare-function avy-goto-line-above "avy")
(declare-function ffap-guesser "ffap")
(declare-function ffap-file-at-point "ffap")
(declare-function ffap "ffap")
(declare-function yggdrasil-leader-def "yggdrasil-leader")

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

(defun ygg-goto-line-start ()
  (interactive)
  (ygg-each-selection-update (ygg--motion #'beginning-of-line)))

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

(defun ygg-mark-set ()
  (interactive)
  (let ((ch (read-char)))
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
     (t (user-error "Invalid mark: %c" ch)))))

(defun ygg-mark-jump (&optional linewise)
  "Jump to the mark named by the next key.
LINEWISE (vim `') lands on the line's first non-blank; without it (vim
backtick) the exact stored position."
  (interactive)
  (let ((ch (read-char)))
    (cond
     ((and (>= ch ?a) (<= ch ?z))
      (let ((m (and ygg--marks-local (gethash ch ygg--marks-local))))
        (unless (and m (marker-buffer m)) (user-error "Mark %c not set" ch))
        (ygg--jump-push)
        (ygg--jump-to m linewise)))
     ((and (>= ch ?A) (<= ch ?Z))
      (let ((entry (cdr (assq ch ygg--marks-global))))
        (unless entry (user-error "Mark %c not set" ch))
        (ygg--jump-push)
        (if (markerp entry)
            (if (marker-buffer entry) (ygg--jump-to entry linewise)
              (user-error "Mark %c: buffer is gone" ch))
          (find-file (car entry))
          (ygg-each-selection-update
           (ygg--motion (lambda ()
                          (goto-char (min (max (cdr entry) (point-min)) (point-max)))
                          (when linewise (back-to-indentation))))))))
     (t (user-error "Invalid mark: %c" ch)))))

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

(defun ygg-goto-file ()
  "Open the file path in the selection, or under point (vim/helix gf)."
  (interactive)
  (require 'ffap)
  (let* ((sel (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
                (when (> (- end beg) 1)
                  (string-trim (buffer-substring-no-properties beg end)))))
         (name (or (and sel (not (string-empty-p sel)) sel)
                   (ffap-guesser)
                   (thing-at-point 'filename t)))
         (target (and name (ffap-file-at-point))))
    (cond
     ((and target (file-exists-p target)) (ygg--jump-push) (find-file target))
     ((and name (file-exists-p (expand-file-name name)))
      (ygg--jump-push) (find-file (expand-file-name name)))
     (name (ygg--jump-push) (ffap name))
     (t (user-error "No file path at point")))))

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

(defvar-local ygg--change-list nil
  "Change positions, most recent first, capped at `ygg--change-list-max'.")
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

(add-hook 'yggdrasil-local-mode-hook
          (lambda ()
            (if yggdrasil-local-mode
                (add-hook 'after-change-functions #'ygg--change-list-record nil t)
              (remove-hook 'after-change-functions #'ygg--change-list-record t))))

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
  (ygg--research (> ygg--search-dir 0)))

(defun ygg-search-prev ()
  "Repeat the last search against its direction."
  (interactive)
  (ygg--research (< ygg--search-dir 0)))

(defun ygg-search-word-forward ()
  (interactive)
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (setq ygg--last-search (regexp-quote (buffer-substring-no-properties beg end))
          ygg--search-dir 1))
  (ygg--research t)
  (ygg--hlsearch ygg--last-search))

(defun ygg-search-word-backward ()
  (interactive)
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (setq ygg--last-search (regexp-quote (buffer-substring-no-properties beg end))
          ygg--search-dir -1))
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
    (isearch-lazy-highlight-new-loop (window-start) (window-end))))

(defun ygg-hlsearch-clear ()
  "Clear search highlighting (vim :nohlsearch)."
  (interactive)
  (lazy-highlight-cleanup t)
  (setq isearch-lazy-highlight-last-string nil))

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
  (when (fboundp 'flymake-goto-next-error) (call-interactively #'flymake-goto-next-error)))

(defun ygg-prev-error ()
  (interactive)
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

(defun ygg-next-comment () (interactive) (ygg--bracketed-goto (lambda () (ygg--find-comment-line 1))))
(defun ygg-prev-comment () (interactive) (ygg--bracketed-goto (lambda () (ygg--find-comment-line -1))))

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

(defun ygg-next-indent () (interactive) (ygg--bracketed-goto (lambda () (ygg--find-indent-line 1))))
(defun ygg-prev-indent () (interactive) (ygg--bracketed-goto (lambda () (ygg--find-indent-line -1))))

(defun ygg--find-conflict (dir)
  (save-excursion
    (forward-line dir)
    (when (if (> dir 0)
              (re-search-forward "^<\\{7\\}" nil t)
            (re-search-backward "^<\\{7\\}" nil t))
      (line-beginning-position))))

(defun ygg-next-conflict () (interactive) (ygg--bracketed-goto (lambda () (ygg--find-conflict 1))))
(defun ygg-prev-conflict () (interactive) (ygg--bracketed-goto (lambda () (ygg--find-conflict -1))))

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
  (let ((f (ygg--sibling-file 1))) (when f (find-file f))))
(defun ygg-prev-file ()
  (interactive)
  (let ((f (ygg--sibling-file -1))) (when f (find-file f))))

(defun ygg-next-window () (interactive) (other-window 1))
(defun ygg-prev-window () (interactive) (other-window -1))

(defun ygg-next-error-any ()
  "next-error when an error buffer exists, else flymake."
  (interactive)
  (if (ignore-errors (next-error-find-buffer)) (next-error) (ygg-next-error)))
(defun ygg-prev-error-any ()
  (interactive)
  (if (ignore-errors (next-error-find-buffer)) (previous-error) (ygg-prev-error)))

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
  "] e" #'ygg-next-error
  "[ e" #'ygg-prev-error
  "] d" #'ygg-next-error
  "[ d" #'ygg-prev-error
  "] c" #'ygg-next-comment
  "[ c" #'ygg-prev-comment
  "] i" #'ygg-next-indent
  "[ i" #'ygg-prev-indent
  "] q" #'ygg-next-error-any
  "[ q" #'ygg-prev-error-any
  "] f" #'ygg-next-file
  "[ f" #'ygg-prev-file
  "] w" #'ygg-next-window
  "[ w" #'ygg-prev-window
  "] x" #'ygg-next-conflict
  "[ x" #'ygg-prev-conflict)

(yggdrasil-define-keys 'ygg-goto-map
  "g" #'ygg-goto-first :label "buffer start"
  "e" #'ygg-goto-last :label "buffer end"
  "h" #'ygg-goto-line-start :label "line start"
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
  "u" #'ygg-downcase :label "downcase"
  "U" #'ygg-upcase :label "upcase"
  ";" #'ygg-change-list-older :label "older change"
  "," #'ygg-change-list-newer :label "newer change"
  "w" #'ygg-easymotion-word-forward :label "hint word →"
  "b" #'ygg-easymotion-word-backward :label "hint word ←"
  "W" #'ygg-easymotion-WORD-forward :label "hint WORD →"
  "B" #'ygg-easymotion-WORD-backward :label "hint WORD ←"
  "j" #'ygg-easymotion-line-down :label "hint line ↓"
  "k" #'ygg-easymotion-line-up :label "hint line ↑")

(yggdrasil-define-keys 'ygg-view-map
  "z" #'ygg-view-center
  "t" #'ygg-view-top
  "b" #'ygg-view-bottom
  "j" #'ygg-view-down
  "k" #'ygg-view-up
  "n" #'ygg-narrow-indirect :label "narrow (indirect)"
  "w" #'ygg-widen-indirect :label "widen")

(with-eval-after-load 'yggdrasil-leader
  (yggdrasil-leader-def "b j" #'ygg-jumplist-pick "jumplist"))

(provide 'yggdrasil-motions)
;;; yggdrasil-motions.el ends here
