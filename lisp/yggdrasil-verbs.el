;;; yggdrasil-verbs.el --- Yggdrasil editing verbs -*- lexical-binding: t; -*-

;; Built-ins wrapped: kill-ring (kill-new/current-kill), delete-indentation,
;; indent-rigidly, upcase/downcase-region, newline-and-indent, open-line,
;; indent-according-to-mode, undo/undo-redo, window/buffer commands.
;; Custom: per-selection dispatch (d/c/y/p/R/r/J/>/</~/o/i/I/A) atop
;; ygg-do-selections and ygg-enter-insert-at.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-selection)
(require 'yggdrasil-ex)
(require 'cl-lib)

(defvar-local ygg--kill-list nil
  "Per-selection texts from the last yank/delete, in buffer order.")

(defvar ygg--kill-linewise nil
  "Non-nil when the last yank/delete spanned whole lines (vim/helix linewise).
Then `p'/`P' open a fresh line instead of splicing the text inline.")

(defvar ygg--kill-joined nil
  "The joined text `ygg--yank-selections' last pushed onto the kill ring.
A differing ring head means the kill came from outside — another program's
clipboard, or a plain Emacs kill — and then neither `ygg--kill-list' nor
`ygg--kill-linewise' describes what `p' is about to paste.")

(defvar-local ygg--last-paste-markers nil
  "Markers (BEG END PRIMARY) of the text the last paste inserted.")

(defvar ygg--last-paste-linewise nil
  "Linewise flag of the paste that `ygg-paste-pop' is cycling.")

;;; Registers (vim: named a-z, append A-Z, yank "0, delete ring "1-"9)
;;; Macro registers (q/@ below) share this same table and key space: a
;;; macro is just keys in a register, vim-style; see `ygg--register-read'
;;; and `ygg--macro-executable' for how each direction degrades the other.

(defvar ygg--registers (make-hash-table :test 'eql)
  "Register char to a list of per-selection texts (yank), or to raw
`last-kbd-macro' data — a string or vector, never a list (macro).")
(defvar ygg--pending-register nil)

(defun ygg--register-preview-line (val)
  "First line of VAL (a yank-list, or raw macro string/vector), truncated
to about 50 chars."
  (let* ((text (cond ((listp val) (or (car val) ""))
                      ((stringp val) val)
                      (t (key-description val))))
         (line (car (split-string text "\n"))))
    (if (> (length line) 50) (concat (substring line 0 50) "...") line)))

(defun ygg--register-listing ()
  "One line per non-empty register, \"c  preview\", sorted by char."
  (let (entries)
    (maphash (lambda (reg val) (when val (push (cons reg val) entries)))
             ygg--registers)
    (mapcar (lambda (e) (format "%c  %s" (car e) (ygg--register-preview-line (cdr e))))
            (sort entries (lambda (a b) (< (car a) (car b)))))))

(declare-function posframe-show "posframe")
(declare-function posframe-delete "posframe")

(defun ygg-use-register ()
  "Read a register for the next yank/delete/change/paste.
Previews the non-empty registers first: a posframe under a GUI with
`posframe' available, else the listing prepended to the prompt."
  (interactive)
  (let* ((lines (ygg--register-listing))
         (listing (mapconcat #'identity lines "\n")))
    (setq ygg--pending-register
          (cond
           ((null lines) (read-char "register: "))
           ((and (display-graphic-p) (require 'posframe nil t))
            (let ((buf " *ygg-registers*"))
              (unwind-protect
                  (progn
                    (posframe-show buf
                                    :string listing
                                    :internal-border-width 0
                                    :background-color
                                    (if (facep 'ygg-float)
                                        (face-attribute 'ygg-float :background nil t)
                                      (face-attribute 'default :background nil t)))
                    (read-char "register: "))
                (posframe-delete buf))))
           (t (read-char (concat listing "\nregister: ")))))
    (message "register: %c" ygg--pending-register)))

(defun ygg--register-consume ()
  (prog1 ygg--pending-register (setq ygg--pending-register nil)))

(defun ygg--register-store (reg texts)
  (cond
   ((eq reg ?_))                                   ; "_ black hole: discard
   ((memq reg '(?+ ?*))                            ; "+ / "* system clipboard
    (let ((s (mapconcat #'identity texts "\n")))
      (ignore-errors
        (gui-set-selection (if (eq reg ?*) 'PRIMARY 'CLIPBOARD) s))
      (kill-new s)))
   ((memq reg '(?. ?# ?%))
    (user-error "Register %c is read-only" reg))
   (t
    (let* ((append-p (<= ?A reg ?Z))
           (key (if append-p (downcase reg) reg))
           (value (if append-p
                      (append (gethash key ygg--registers) texts)
                    texts)))
      (puthash key value ygg--registers)
      (set-register key (mapconcat #'identity value "\n"))))))

(defun ygg--register-filename ()
  "Vim %% register: the current file name, abbreviated, or the buffer name."
  (if buffer-file-name (abbreviate-file-name buffer-file-name) (buffer-name)))

(defun ygg--register-read (reg)
  "Yank/paste read of REG: yank registers are a list of texts already;
a macro register (raw `last-kbd-macro' data, never a list) degrades to
its literal chars when playable as text, else a human-readable listing.
Specials: %% file name, \"+/\"* system clipboard, \". selection
contents, \"# selection indices, \"_ black hole (empty) — all vim/helix."
  (pcase reg
    (?% (list (ygg--register-filename)))
    ((or ?+ ?*)
     (list (or (ignore-errors
                 (let ((s (gui-get-selection
                           (if (eq reg ?*) 'PRIMARY 'CLIPBOARD))))
                   (and s (substring-no-properties s))))
               (current-kill 0) "")))
    (?. (or (ygg--collect-selection-texts) (user-error "No selection")))
    (?# (cl-loop for i from 1 to (max 1 (ygg-selections-count))
                 collect (number-to-string i)))
    (?_ (list ""))
    (_ (let ((key (if (<= ?A reg ?Z) (downcase reg) reg)))
         (or (let ((val (gethash key ygg--registers)))
               (cond ((null val) nil)
                     ((listp val) val)
                     ((stringp val) (list val))
                     (t (list (key-description val)))))
             (let ((v (get-register key)))
               (and (stringp v) (list v)))
             (user-error "Register %c is empty" reg))))))

(defun ygg--register-shift-ring (texts)
  (cl-loop for i from ?9 downto ?2
           do (puthash i (gethash (1- i) ygg--registers) ygg--registers))
  (puthash ?1 texts ygg--registers))

;;; Shared helpers

(defun ygg--verb-exit ()
  "Return to normal state if a verb ran from visual state."
  (when (ygg-visual-p) (ygg-normal-state)))

(defun ygg--verb-regions ()
  "All selections as (BEG END PRIMARY), in buffer order."
  (let* ((secs (mapcar (lambda (ov) (list (overlay-start ov) (overlay-end ov) nil))
                       ygg--secondaries))
         (prim (pcase-let ((`(,b ,e ,_) (ygg-selection-effective-bounds)))
                 (list b e t))))
    (sort (cons prim secs) (lambda (a b) (< (car a) (car b))))))

(defun ygg--collect-selection-texts ()
  (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cadr r)))
          (ygg--verb-regions)))

(defun ygg--regions-linewise-p (regions)
  "Non-nil when every region in REGIONS spans whole lines (BOL to BOL/eob)."
  (and regions
       (seq-every-p
        (lambda (r)
          (pcase-let ((`(,b ,e ,_) r))
            (and (> e b)
                 (save-excursion (goto-char b) (bolp))
                 (save-excursion (goto-char e) (or (bolp) (eobp))))))
        regions)))

(defun ygg--yank-selections (&optional kind text-fn)
  "Store selection texts in `ygg--kill-list' and push them as one kill.
KIND is `yank' or `delete'; it feeds the vim register conventions
\(\"0 for yanks, the \"1-\"9 ring for deletes, named on demand).
TEXT-FN maps (BEG END) to the text kept for a selection."
  (let* ((regions (ygg--verb-regions))
         (texts (mapcar (lambda (r)
                          (substring-no-properties
                           (funcall (or text-fn #'filter-buffer-substring)
                                    (car r) (cadr r))))
                        regions))
         (reg (ygg--register-consume)))
    ;; "_ black hole: leave the kill ring and every register untouched
    (unless (eq reg ?_)
      (setq ygg--kill-list texts
            ygg--kill-linewise (ygg--regions-linewise-p regions)
            ygg--kill-joined (mapconcat #'identity texts "\n"))
      (kill-new ygg--kill-joined)
      (when reg (ygg--register-store reg texts))
      (pcase kind
        ('yank (ygg--register-store ?0 texts))
        ('delete (ygg--register-shift-ring texts))))))

(defvar ygg--paste-chunks nil)
(defvar ygg--paste-fallback nil)
(defvar ygg--paste-linewise nil)

(defun ygg--paste-setup ()
  "Resolve pending register into the chunk list/fallback for this paste.
`current-kill' first: it pulls a copy made outside Emacs onto the ring,
and that kill then owns the paste over any older yank's chunks."
  (let ((reg (ygg--register-consume)))
    (if reg
        (let ((texts (ygg--register-read reg)))
          (setq ygg--paste-chunks texts
                ygg--paste-fallback (mapconcat #'identity texts "\n")
                ygg--paste-linewise nil))
      (let* ((kill (current-kill 0))
             (ours (equal kill ygg--kill-joined)))
        (setq ygg--paste-chunks
              (and ours
                   (equal kill (mapconcat #'identity ygg--kill-list "\n"))
                   ygg--kill-list)
              ygg--paste-fallback kill
              ygg--paste-linewise (and ours ygg--kill-linewise))))))

(defun ygg--kill-text-for (idx count)
  "Pairwise paste rule: chunk IDX when COUNT matches, else the fallback."
  (if (= (length ygg--paste-chunks) count)
      (nth idx ygg--paste-chunks)
    ygg--paste-fallback))

(defun ygg--install-selection-set (regions)
  "REGIONS is ((BEG END PRIMARY) ...); install as the new selection set."
  (ygg-clear-secondaries)
  (let (primary-region)
    (dolist (r regions)
      (pcase-let ((`(,b ,e ,primary) r))
        (if primary (setq primary-region (cons b e)) (ygg-add-selection b e))))
    (when primary-region
      (ygg-set-selection (car primary-region) (cdr primary-region)))))

(defun ygg--collect-insert-points (fn)
  "Run FN (BEG END DIR) -> POS per selection via `ygg-do-selections'.
Returns the resulting positions, primary first. Each position is captured
as a marker the instant FN returns, so an edit made by a selection
processed later (e.g. a primary positioned before a secondary) can never
invalidate a position already collected."
  (let (markers)
    (ygg-do-selections
     (lambda (beg end dir) (push (copy-marker (funcall fn beg end dir)) markers)))
    (mapcar (lambda (m) (prog1 (marker-position m) (set-marker m nil)))
            markers)))

(defun ygg--markers-to-regions (markers)
  "MARKERS is ((BEG-MARKER END-MARKER PRIMARY) ...); collapse to positions."
  (mapcar (lambda (m)
            (prog1 (list (marker-position (car m)) (marker-position (cadr m)) (caddr m))
              (set-marker (car m) nil)
              (set-marker (cadr m) nil)))
          markers))

(defun ygg--copy-markers (markers)
  (mapcar (lambda (m) (list (copy-marker (car m)) (copy-marker (cadr m)) (caddr m)))
          markers))

;;; d / M-d — delete

(defun ygg--delete-selections ()
  "Delete every selection, leaving a cursor at each site (Helix).
Cursors that would land past the last character clamp onto it, so
adjacent end-of-buffer cursors can merge — Emacs overlays cannot be
zero-width, unlike Helix selections."
  (let* ((points (ygg--collect-insert-points
                  (lambda (beg end _dir) (delete-region beg end) beg)))
         (primary (min (car points) (point-max))))
    (ygg-clear-secondaries)
    (dolist (p (cdr points))
      (let ((q (min p (max (point-min) (1- (point-max))))))
        (unless (= q primary)
          (ygg-add-selection q (min (1+ q) (point-max))))))
    (ygg-set-selection primary primary)))

(defun ygg-delete ()
  "Delete every selection after yanking their text."
  (interactive)
  (ygg-with-verb
    (ygg--yank-selections 'delete)
    (ygg--delete-selections))
  (ygg--verb-exit))

(defun ygg-delete-no-yank ()
  "Delete every selection without yanking."
  (interactive)
  (ygg-with-verb
    (ygg--delete-selections))
  (ygg--verb-exit))

(defun ygg-delete-via-blackhole ()
  "Delete every selection via the black-hole register (Helix A-d)."
  (interactive)
  (setq ygg--pending-register ?_)
  (ygg-delete))

(declare-function ygg-select-line "yggdrasil-selection")

(defun ygg-delete-dwim (&optional n)
  "Delete the selection; on a bare cursor a quick second d takes the
whole line (vim dd, count deletes N lines)."
  (interactive "p")
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (if (or ygg--secondaries (> (- end beg) 1))
        (ygg-delete)
      (let* ((macro-left (and executing-kbd-macro
                              (< executing-kbd-macro-index
                                 (length executing-kbd-macro))))
             (ev (cond (macro-left (read-event))
                       (executing-kbd-macro nil)
                       (t (read-event nil nil 0.4)))))
        (cond ((eq ev ?d)
               (dotimes (_ (max 1 (or n 1))) (ygg-select-line))
               (ygg-delete))
              (t (ygg-delete)
                 (when ev (push ev unread-command-events))))))))

;;; c / M-c — change

(defun ygg--change-collapse ()
  "Delete every selection, returning collapse points (primary first)."
  (ygg--collect-insert-points
   (lambda (beg end _dir) (delete-region beg end) beg)))

(defun ygg-change ()
  "Delete every selection after yanking, then insert at the collapse points."
  (interactive)
  (let (points)
    (ygg-with-verb
      (ygg--yank-selections 'delete)
      (setq points (ygg--change-collapse)))
    (ygg-enter-insert-at points)))

(defun ygg-change-no-yank ()
  "Delete every selection, then insert at the collapse points."
  (interactive)
  (let (points)
    (ygg-with-verb (setq points (ygg--change-collapse)))
    (ygg-enter-insert-at points)))

(defun ygg-change-via-blackhole ()
  "Change every selection via the black-hole register (Helix A-c)."
  (interactive)
  (setq ygg--pending-register ?_)
  (ygg-change))

;;; y — yank

(defun ygg-yank ()
  "Copy every selection into `ygg--kill-list' and the kill ring."
  (interactive)
  (ygg--yank-selections 'yank)
  (pcase-dolist (`(,beg ,end ,_) (ygg--verb-regions))
    (ygg-flash-region beg end))
  (ygg--verb-exit))

(defun ygg--unindented-text (beg end)
  (let* ((start (save-excursion
                  (goto-char beg)
                  (if (looking-back "^[ \t]*" (line-beginning-position))
                      (line-beginning-position)
                    beg)))
         (lines (split-string (buffer-substring-no-properties start end) "\n"))
         (indents (mapcar (lambda (l) (and (string-match "\\`[ \t]*[^ \t]" l)
                                           (1- (match-end 0))))
                          lines))
         (cut (apply #'min (or (delq nil indents) '(0)))))
    (mapconcat (lambda (l) (replace-regexp-in-string
                            (format "\\`[ \t]\\{0,%d\\}" cut) "" l))
               lines "\n")))

(defun ygg-yank-unindented ()
  "Copy every selection with its common indentation removed."
  (interactive)
  (ygg--yank-selections 'yank #'ygg--unindented-text)
  (pcase-dolist (`(,beg ,end ,_) (ygg--verb-regions))
    (ygg-flash-region beg end))
  (ygg--verb-exit))

;;; p / P / visual-p — paste

(defun ygg--paste-insert (before)
  (ygg--paste-setup)
  (let* ((regions (ygg--verb-regions))
         (count (length regions))
         (line ygg--paste-linewise)
         (idx (1- count))
         (markers nil))
    (dolist (r (reverse regions))
      (pcase-let ((`(,beg ,end ,primary) r))
        (let ((text (ygg--kill-text-for idx count)))
          (if line
              ;; linewise: open a fresh line below (p) or above (P)
              (let ((text (if (string-suffix-p "\n" text) text (concat text "\n"))))
                (if before
                    (progn (goto-char beg) (forward-line 0))
                  (goto-char end) (unless (bolp) (forward-line 1)))
                (when (and (not before) (eobp) (not (bolp))) (insert "\n"))
                (let ((s (point)))
                  (insert text)
                  (push (list (copy-marker s) (copy-marker (max s (1- (point)))) primary)
                        markers)))
            (let ((pos (if before beg end)))
              (goto-char pos)
              (insert text)
              (push (list (copy-marker pos) (copy-marker (point)) primary) markers)))))
      (setq idx (1- idx)))
    (setq ygg--last-paste-linewise line
          ygg--last-paste-markers (ygg--copy-markers markers))
    (ygg--install-selection-set (ygg--markers-to-regions markers))))

(defvar-local ygg-paste-function nil
  "When set, `p'/`P' call this instead of inserting into the buffer.
Read-only modal buffers that paste elsewhere (e.g. embr, into the focused
web-page field) set it so vim paste still works where `insert' would signal
`buffer-read-only'.")

(defun ygg-paste-after (&optional n)
  "Paste after each selection (pairwise); select the pasted text."
  (interactive "p")
  (if ygg-paste-function
      (funcall ygg-paste-function)
    (ygg-with-verb (dotimes (_ (max 1 (or n 1))) (ygg--paste-insert nil)))
    (ygg--verb-exit)))

(defun ygg-paste-before (&optional n)
  "Paste before each selection (pairwise); select the pasted text."
  (interactive "p")
  (if ygg-paste-function
      (funcall ygg-paste-function)
    (ygg-with-verb (dotimes (_ (max 1 (or n 1))) (ygg--paste-insert t)))
    (ygg--verb-exit)))

(defun ygg-clipboard-paste ()
  "Paste the system clipboard anywhere (bound to Cmd/`s-v').
Routes through `ygg-paste-function' when set so a read-only page buffer
\(embr) still receives it instead of signalling `buffer-read-only'."
  (interactive)
  (if ygg-paste-function (funcall ygg-paste-function) (clipboard-yank)))

(global-set-key (kbd "s-v") #'ygg-clipboard-paste)

;;; C-p / C-n — paste-pop: cycle the kill ring over the just-pasted text

(defun ygg--paste-pop (n)
  "Replace the just-pasted selections with the kill N steps along the ring."
  (unless (memq last-command
                '(ygg-paste-after ygg-paste-before
                  ygg-paste-pop ygg-paste-undo-pop))
    (user-error "Previous command was not a paste"))
  (let ((text (current-kill n))
        (linewise ygg--last-paste-linewise))
    (ygg-with-verb
      (let ((markers nil))
        (dolist (r (reverse (ygg--verb-regions)))
          (pcase-let ((`(,beg ,end ,primary) r))
            (delete-region beg end)
            (goto-char beg)
            ;; linewise selections exclude their trailing newline; drop the
            ;; kill's own so a linewise cycle keeps one newline, not two
            (insert (if (and linewise (string-suffix-p "\n" text))
                        (substring text 0 -1)
                      text))
            (push (list (copy-marker beg) (copy-marker (point)) primary) markers)))
        (setq ygg--last-paste-markers (ygg--copy-markers markers))
        (ygg--install-selection-set (ygg--markers-to-regions markers))))))

(defun ygg-reselect-paste ()
  "Select the text the last paste inserted."
  (interactive)
  (unless ygg--last-paste-markers (user-error "Nothing pasted"))
  (ygg--install-selection-set
   (ygg--markers-to-regions (ygg--copy-markers ygg--last-paste-markers))))

(defun ygg-paste-pop (&optional n)
  "After a paste, replace it with the next-older kill (like `yank-pop')."
  (interactive "p")
  (ygg--paste-pop (or n 1)))

(defun ygg-paste-undo-pop (&optional n)
  "After a paste, cycle back toward the newest kill."
  (interactive "p")
  (ygg--paste-pop (- (or n 1))))

;;; R / visual p — replace with pairwise kill

(defun ygg--replace-pairwise ()
  (ygg--paste-setup)
  (let* ((regions (ygg--verb-regions))
         (count (length regions))
         (idx (1- count))
         (markers nil))
    (dolist (r (reverse regions))
      (pcase-let ((`(,beg ,end ,primary) r))
        (let ((text (ygg--kill-text-for idx count)))
          (delete-region beg end)
          (goto-char beg)
          (insert text)
          (push (list (copy-marker beg) (copy-marker (point)) primary) markers)))
      (setq idx (1- idx)))
    (ygg--install-selection-set (ygg--markers-to-regions markers))))

(defun ygg-replace-with-kill ()
  "Replace every selection with its pairwise kill, without entering insert."
  (interactive)
  (ygg-with-verb (ygg--replace-pairwise))
  (ygg--verb-exit))

(defun ygg-visual-paste ()
  "Visual-state p: replace the selection with the paste (paste-over)."
  (interactive)
  (ygg-with-verb (ygg--replace-pairwise))
  (ygg--verb-exit))

;;; r — replace char

(defun ygg--toggle-char (c) (if (eq c (upcase c)) (downcase c) (upcase c)))

(defun ygg--count-widen (n)
  "Vim count-on-a-bare-cursor: widen the selection to N chars, eol-clamped."
  (when (and n (> n 1) (null ygg--secondaries))
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (when (<= (- end beg) 1)
        (ygg-set-selection beg (min (+ beg n) (1+ (line-end-position))))))))

(defun ygg-replace-char (ch &optional n)
  "Replace every char in every selection with CH; count widens (vim 3rx)."
  (interactive (list (read-char "replace with: ")
                     (prefix-numeric-value current-prefix-arg)))
  (ygg--count-widen n)
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg end _dir)
       (let* ((s (buffer-substring-no-properties beg end))
              (new (replace-regexp-in-string "[^\n]" (lambda (_) (string ch)) s)))
         (delete-region beg end)
         (goto-char beg)
         (insert new)))))
  (ygg--verb-exit))

;;; J — join

(defun ygg--join-lines (beg end)
  (let* ((lbeg (line-number-at-pos beg))
         (lend (line-number-at-pos
                (if (and (> end beg)
                         (save-excursion (goto-char end) (bolp)))
                    (1- end)
                  end))))
    (goto-char beg)
    (if (> lend lbeg)
        (progn
          (forward-line (- lend lbeg))
          (dotimes (_ (- lend lbeg)) (delete-indentation)))
      (delete-indentation t))))

(defun ygg-join-lines (&optional n)
  "Join the lines touched by each selection; a count joins N lines (vim 3J)."
  (interactive "p")
  (ygg-with-verb
    (if (and n (> n 1))
        (dotimes (_ (1- n))
          (ygg-do-selections (lambda (beg end _dir) (ygg--join-lines beg end))))
      (ygg-do-selections (lambda (beg end _dir) (ygg--join-lines beg end)))))
  (ygg--verb-exit))

(defun ygg--join-lines-space (beg end)
  "Join from BEG to END with next line, selecting inserted space (Helix A-J).
Returns position of the first inserted space, or nil if no join occurred."
  (let ((lbeg (line-number-at-pos beg))
        (lend (line-number-at-pos
               (if (and (> end beg)
                        (save-excursion (goto-char end) (bolp)))
                   (1- end)
                 end)))
        (space-positions nil))
    (goto-char beg)
    (when (> lend lbeg)
      (dotimes (_ (- lend lbeg))
        (end-of-line)
        (let ((join-pos (point)))
          (forward-line 1)
          (skip-chars-forward " \t")
          (delete-region join-pos (point))
          (goto-char join-pos)
          (unless (eolp)
            (insert " ")
            (push join-pos space-positions)))))
    (if space-positions
        (car (reverse space-positions))
      nil)))

(defun ygg-join-lines-space (&optional _n)
  "Join lines, selecting inserted spaces (Helix A-J)."
  (interactive "p")
  (ygg-with-verb
    (let ((markers
           (cl-loop for r in (ygg--verb-regions)
                    collect (pcase-let ((`(,beg ,end ,_) r))
                              (ygg--join-lines-space beg end)))))
      (ygg--install-selection-set
       (delq nil
             (mapcar (lambda (m) (and m (list m (min (1+ m) (point-max)) nil)))
                     markers)))))
  (ygg--verb-exit))

;;; [ SPC / ] SPC — add newlines above/below

(defun ygg-add-newline-above (&optional count)
  "Insert empty lines above each selection (Helix [[ and add_newline_above)."
  (interactive "p")
  (let ((count (max 1 (or count 1))))
    (ygg-with-verb
      (let* ((regions (ygg--verb-regions))
             (newline-text (make-string count ?\n)))
        (dolist (r (reverse regions))
          (pcase-let ((`(,beg ,_ ,_) r))
            (save-excursion
              (goto-char beg)
              (beginning-of-line)
              (insert newline-text))))
        (ygg--install-selection-set
         (mapcar (lambda (r)
                   (pcase-let ((`(,b ,e ,p) r))
                     (list (+ b (* count 1)) (+ e (* count 1)) p)))
                 regions))))
    (ygg--verb-exit)))

(defun ygg-add-newline-below (&optional count)
  "Insert empty lines below each selection (Helix ] and add_newline_below)."
  (interactive "p")
  (let ((count (max 1 (or count 1))))
    (ygg-with-verb
      (let* ((regions (ygg--verb-regions))
             (newline-text (make-string count ?\n)))
        (dolist (r (reverse regions))
          (pcase-let ((`(,_ ,end ,_) r))
            (save-excursion
              (goto-char end)
              (end-of-line)
              (forward-line 1)
              (beginning-of-line)
              (insert newline-text))))
        (ygg--install-selection-set regions)))
    (ygg--verb-exit)))

;;; > / < — indent, keeping the selection active

(defun ygg--indent-lines (beg end amount)
  (let ((lbeg (save-excursion (goto-char beg) (line-beginning-position)))
        (lend (save-excursion (goto-char end)
                              (if (bolp) (point) (line-beginning-position 2)))))
    (indent-rigidly lbeg lend amount)))

(defconst ygg--shift-width-vars
  '((lisp-data-mode . lisp-body-indent)
    (python-base-mode . python-indent-offset)
    (js-base-mode . js-indent-level)
    (c-ts-mode . c-ts-mode-indent-offset)
    (c++-ts-mode . c-ts-mode-indent-offset)
    (java-ts-mode . java-ts-mode-indent-offset)
    (go-ts-mode . go-ts-mode-indent-offset)
    (json-ts-mode . json-ts-mode-indent-offset)
    (swift-mode . swift-mode:basic-offset)
    (dart-mode . c-basic-offset)
    (c-mode . c-basic-offset)
    (c++-mode . c-basic-offset)
    (java-mode . c-basic-offset))
  "Mode to offset variable for modes `editorconfig-indentation-alist' lacks.")

(defun ygg-shift-width ()
  "Return the indent step of the current buffer, like vim's `shiftwidth'."
  (require 'editorconfig nil t)
  (let* ((from-alist
          (and (boundp 'editorconfig-indentation-alist)
               (cl-loop for (mode . v) in editorconfig-indentation-alist
                        when (derived-mode-p mode)
                        return (if (consp v) (car v) v))))
         (from-table
          (cl-loop for (mode . v) in ygg--shift-width-vars
                   when (derived-mode-p mode) return v))
         (val (cl-loop for var in (list from-alist from-table)
                       for x = (and (symbolp var) (boundp var) (symbol-value var))
                       when (and (integerp x) (> x 0)) return x)))
    (or val
        (and (integerp standard-indent) (> standard-indent 0) standard-indent)
        tab-width)))

(defun ygg-indent-right (&optional n)
  "Indent the full lines touched by each selection by count * `ygg-shift-width'."
  (interactive "p")
  (let ((amount (* (max 1 (or n 1)) (ygg-shift-width))))
    (ygg-with-verb
      (ygg-do-selections (lambda (beg end _dir) (ygg--indent-lines beg end amount)))))
  (ygg--verb-exit))

(defun ygg-indent-left (&optional n)
  "Dedent the full lines touched by each selection by count * `ygg-shift-width'."
  (interactive "p")
  (let ((amount (- (* (max 1 (or n 1)) (ygg-shift-width)))))
    (ygg-with-verb
      (ygg-do-selections (lambda (beg end _dir) (ygg--indent-lines beg end amount)))))
  (ygg--verb-exit))

;;; insert C-t / C-d — shift the current line

(defun ygg--insert-shift-line (dir)
  (let* ((sw (ygg-shift-width))
         (cur (current-indentation))
         (text-pos (save-excursion (back-to-indentation) (point)))
         (offset (- (point) text-pos))
         (target (if (> dir 0)
                     (* sw (1+ (/ cur sw)))
                   (max 0 (* sw (1- (/ (+ cur sw -1) sw)))))))
    (indent-line-to target)
    (back-to-indentation)
    (goto-char (max (line-beginning-position)
                    (min (line-end-position) (+ (point) offset))))))

(defun ygg-insert-indent ()
  "Indent the current line to the next `ygg-shift-width' multiple."
  (interactive)
  (ygg--insert-shift-line 1))

(defun ygg-insert-dedent ()
  "Dedent the current line to the previous `ygg-shift-width' multiple."
  (interactive)
  (ygg--insert-shift-line -1))

(yggdrasil-define-keys 'insert
  "C-t" #'ygg-insert-indent :label "indent line"
  "C-d" #'ygg-insert-dedent :label "dedent line")

;;; ~ / ` / M-` — case

(defun ygg-toggle-case (&optional n)
  "Toggle the case of every char in every selection; count widens (vim 3~)."
  (interactive "p")
  (ygg--count-widen n)
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg end _dir)
       (let* ((s (buffer-substring-no-properties beg end))
              (new (apply #'string (mapcar #'ygg--toggle-char (append s nil)))))
         (delete-region beg end)
         (goto-char beg)
         (insert new)))))
  (ygg--verb-exit))

(defun ygg-downcase (&optional n)
  "Downcase every selection; count widens on a bare cursor."
  (interactive "p")
  (ygg--count-widen n)
  (ygg-with-verb
    (ygg-do-selections (lambda (beg end _dir) (downcase-region beg end))))
  (ygg--verb-exit))

(defun ygg-upcase (&optional n)
  "Upcase every selection; count widens on a bare cursor."
  (interactive "p")
  (ygg--count-widen n)
  (ygg-with-verb
    (ygg-do-selections (lambda (beg end _dir) (upcase-region beg end))))
  (ygg--verb-exit))

;;; o / O — open line, then insert

(defcustom ygg-open-continue-comments t
  "Non-nil makes o and O on a comment-only line open a commented line."
  :type 'boolean
  :group 'yggdrasil)

(defun ygg--comment-only-line-p ()
  (and ygg-open-continue-comments comment-start
       (save-excursion
         (let ((bol (line-beginning-position))
               (indent (progn (back-to-indentation) (point))))
           (end-of-line)
           (let* ((ppss (syntax-ppss))
                  (start (nth 8 ppss)))
             (and (nth 4 ppss) (not (nth 3 ppss)) start
                  (or (< start bol) (= start indent))))))))

(defun ygg--open-below (_beg end)
  (goto-char end)
  (end-of-line)
  (if (ygg--comment-only-line-p)
      (comment-indent-new-line)
    (newline-and-indent))
  (point))

(defun ygg--comment-prefix-below ()
  (end-of-line)
  (comment-indent-new-line)
  (prog1 (buffer-substring-no-properties (line-beginning-position) (point))
    (delete-region (1- (line-beginning-position)) (point))))

(defun ygg--open-above (beg _end)
  (goto-char beg)
  (let ((indent (save-excursion (back-to-indentation) (current-column)))
        (prefix (and (ygg--comment-only-line-p)
                     (save-excursion (ygg--comment-prefix-below)))))
    (beginning-of-line)
    (open-line 1)
    (if prefix
        (insert prefix)
      (indent-to indent))
    (point)))

(defvar-local ygg-open-line-redirect-function nil
  "Function run before o and O to move point off read-only text.
Buffers that keep typing at a prompt (a REPL's output above it) set it.")

(defun ygg--open-line-redirect ()
  (when ygg-open-line-redirect-function
    (funcall ygg-open-line-redirect-function)))

(defun ygg-open-below (&optional count)
  "Open a new line below each selection's end line, then insert there.
COUNT opens that many lines, each holding the inserted text (vim 3o)."
  (interactive "p")
  (ygg--open-line-redirect)
  (let (points)
    (ygg-with-verb
      (setq points (ygg--collect-insert-points
                    (lambda (beg end _dir) (ygg--open-below beg end)))))
    (ygg-enter-insert-at points count #'ygg--open-below)))

(defun ygg-open-above (&optional count)
  "Open a new line above each selection's start line, then insert there.
COUNT opens that many lines, each holding the inserted text (vim 3O)."
  (interactive "p")
  (ygg--open-line-redirect)
  (let (points)
    (ygg-with-verb
      (setq points (ygg--collect-insert-points
                    (lambda (beg end _dir) (ygg--open-above beg end)))))
    (ygg-enter-insert-at points count #'ygg--open-below)))

;;; i / a — insert at selection edges

(defun ygg-insert-before (&optional count)
  "Enter insert at the start of every selection.
COUNT inserts the typed text that many times (vim 3i)."
  (interactive "p")
  (ygg-enter-insert-at
   (ygg--collect-insert-points (lambda (beg _end _dir) beg))
   count))

(defun ygg-insert-after (&optional count)
  "Enter insert after the end of every selection.
Vim semantics: never hop over a newline — `a' on an empty line (or a
selection ending in one) appends on that line, not the next.  COUNT
inserts the typed text that many times."
  (interactive "p")
  (ygg-enter-insert-at
   (ygg--collect-insert-points
    (lambda (beg end _dir)
      (if (eq (char-before end) ?\n) (max beg (1- end)) end)))
   count))

;;; I / A — insert at first-non-blank / end of each selection's line

(defun ygg--line-first-non-blank (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (point)))

(defun ygg--line-end (pos)
  (save-excursion (goto-char pos) (end-of-line) (point)))

(defun ygg-insert-bol (&optional count)
  "Enter insert at the first non-blank of each selection's line.
COUNT inserts the typed text that many times."
  (interactive "p")
  (ygg-enter-insert-at
   (ygg--collect-insert-points (lambda (beg _end _dir) (ygg--line-first-non-blank beg)))
   count))

(defun ygg-insert-column-0 (&optional count)
  "Enter insert at column 0 of each selection's line (vim gI).
COUNT inserts the typed text that many times."
  (interactive "p")
  (ygg-enter-insert-at
   (ygg--collect-insert-points (lambda (beg _end _dir) (save-excursion (goto-char beg) (line-beginning-position))))
   count))

(defun ygg-insert-eol (&optional count)
  "Enter insert at the end of each selection's line.
COUNT inserts the typed text that many times."
  (interactive "p")
  (ygg-enter-insert-at
   (ygg--collect-insert-points (lambda (_beg end _dir) (ygg--line-end end)))
   count))

;;; R — vim overwrite (Replace) mode: type over existing text

(defun ygg-overwrite ()
  "vim R: enter insert overwriting existing chars (Emacs `overwrite-mode').
Helix replace-with-kill lives on visual `p' (select, then paste over)."
  (interactive)
  (ygg-enter-insert-at
   (ygg--collect-insert-points (lambda (beg _end _dir) beg)))
  (overwrite-mode 1))

(defun ygg--overwrite-off ()
  (when (bound-and-true-p overwrite-mode) (overwrite-mode -1)))
(add-hook 'ygg-insert-exit-hook #'ygg--overwrite-off)

;;; C-s save-selection — push point onto the jumplist (helix)

(declare-function better-jumper-set-jump "better-jumper")

(defun ygg-save-selection ()
  "Push point onto the jumplist (helix save-selection) so `C-o' returns here."
  (interactive)
  (when (fboundp 'better-jumper-set-jump) (better-jumper-set-jump))
  (message "yggdrasil: saved to jumplist"))

;;; g $ — keep pipe

(defun ygg-keep-pipe (command)
  "Keep only selections for which COMMAND exits 0 (Helix keep_pipe / g $)."
  (interactive (list (ygg--shell-line "keep pipe: ")))
  (let* ((regions (ygg--verb-regions))
         (survivors (cl-loop for r in regions
                             collect (pcase-let ((`(,beg ,end ,_) r))
                                       (when (zerop (call-process-region beg end shell-file-name
                                                                         nil nil nil
                                                                         shell-command-switch command))
                                         r)))))
    (setq survivors (delq nil survivors))
    (unless survivors
      (user-error "No selections remaining"))
    (ygg--install-selection-set survivors)
    (pcase-let ((`(,b ,e ,_) (car (last survivors))))
      (ygg-set-selection b e))))

;;; | / M-| / ! / M-! — shell pipe verbs

(defun ygg--pipe-replace (beg end command)
  "Pipe BEG..END through COMMAND, replacing it with the output.
A single trailing newline in the output is trimmed; empty output trims
nothing, so a pre-existing newline right before BEG survives."
  (goto-char beg)
  (let ((start (point)))
    (call-process-region beg end shell-file-name t t nil
                          shell-command-switch command)
    (when (and (> (point) start) (eq (char-before) ?\n)) (delete-char -1))))

(defun ygg--shell-line (prompt)
  "Read a shell line under PROMPT, expanded the way the ex line is."
  (ygg-ex--expand (read-shell-command prompt)))

(defun ygg-pipe-replace (command)
  "Pipe every selection's text through COMMAND, replacing it with the output."
  (interactive (list (ygg--shell-line "pipe: ")))
  (ygg-with-verb
    (ygg-do-selections (lambda (beg end _dir) (ygg--pipe-replace beg end command))))
  (ygg--verb-exit))

(defun ygg-pipe-discard (command)
  "Run COMMAND with each selection as stdin, discarding the output."
  (interactive (list (ygg--shell-line "pipe (discard): ")))
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg end _dir)
       (call-process-region beg end shell-file-name nil nil nil
                             shell-command-switch command))))
  (ygg--verb-exit))

(defun ygg-insert-command-before (command)
  "Insert COMMAND's output (no stdin) before each selection."
  (interactive (list (ygg--shell-line "insert before: ")))
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg _end _dir)
       (let ((output (shell-command-to-string command)))
         (goto-char beg)
         (insert output)))))
  (ygg--verb-exit))

(defun ygg-insert-command-after (command)
  "Insert COMMAND's output (no stdin) after each selection."
  (interactive (list (ygg--shell-line "insert after: ")))
  (ygg-with-verb
    (ygg-do-selections
     (lambda (_beg end _dir)
       (let ((output (shell-command-to-string command)))
         (goto-char end)
         (insert output)))))
  (ygg--verb-exit))

;;; g q — reflow

(defun ygg--reflow-selection (beg end)
  (if (= (1+ beg) end)
      (progn (goto-char beg) (fill-paragraph))
    (fill-region beg end)))

(defun ygg-reflow ()
  "Fill/reflow each selection; a degenerate (1-char) selection reflows
the paragraph at point instead (vim gq)."
  (interactive)
  (ygg-with-verb
    (ygg-do-selections (lambda (beg end _dir) (ygg--reflow-selection beg end))))
  (ygg--verb-exit))

;;; g X — exchange (evil-exchange)

(defface ygg-exchange-highlight '((t :inherit region :underline t))
  "Region marked by a pending `g X' exchange."
  :group 'yggdrasil)

(defvar-local ygg--exchange-pending nil
  "(BEG-MARKER END-MARKER OVERLAY) for a first `g X' mark, or nil.")

(defun ygg--exchange-cancel ()
  (when ygg--exchange-pending
    (pcase-let ((`(,m-beg ,m-end ,ov) ygg--exchange-pending))
      (set-marker m-beg nil)
      (set-marker m-end nil)
      (delete-overlay ov))
    (setq ygg--exchange-pending nil)))

(defun ygg--exchange-swap (beg1 end1 text1 beg2 end2 text2)
  "Replace BEG1..END1 with TEXT2 and BEG2..END2 with TEXT1.
Edits the higher-positioned region first so the lower one's bounds hold."
  (if (<= beg1 beg2)
      (progn
        (goto-char beg2) (delete-region beg2 end2) (insert text1)
        (goto-char beg1) (delete-region beg1 end1) (insert text2))
    (progn
      (goto-char beg1) (delete-region beg1 end1) (insert text2)
      (goto-char beg2) (delete-region beg2 end2) (insert text1))))

(defun ygg-exchange ()
  "Evil-exchange g X: the first call marks the primary selection; a
second call swaps its text with the new primary selection (one undo
step). A second call on the SAME region cancels the pending exchange."
  (interactive)
  (pcase-let ((`(,beg ,end ,_dir) (ygg-selection-effective-bounds)))
    (if (null ygg--exchange-pending)
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'face 'ygg-exchange-highlight)
          (setq ygg--exchange-pending
                (list (copy-marker beg) (copy-marker end t) ov)))
      (pcase-let ((`(,m-beg ,m-end ,_ov) ygg--exchange-pending))
        (if (and (= beg (marker-position m-beg)) (= end (marker-position m-end)))
            (ygg--exchange-cancel)
          (let ((first-beg (marker-position m-beg))
                (first-end (marker-position m-end))
                (first-text (buffer-substring-no-properties m-beg m-end))
                (second-text (buffer-substring-no-properties beg end)))
            (ygg-with-verb
              (ygg--exchange-swap first-beg first-end first-text beg end second-text))
            (ygg--exchange-cancel))))))
  (ygg--verb-exit))

;;; Window commands (C-w prefix)

(defun ygg-window-split-below ()
  "Split the window below and focus the new one."
  (interactive)
  (select-window (split-window-below)))

(defun ygg-window-split-right ()
  "Split the window to the right and focus the new one."
  (interactive)
  (select-window (split-window-right)))

(defun ygg-window-swap-next ()
  "Swap the selected window's state with the next window's."
  (interactive)
  (window-swap-states (selected-window) (next-window)))

(defun ygg-window-zoom-toggle ()
  "Maximize the current window fullscreen; call again to restore the split.
Works from a side window too (sidebar, quickfix, agent trace): its buffer
fills the frame and every panel is hidden, then the toggle restores the
exact layout.  Per-frame; the saved config's presence is the toggle state."
  (interactive)
  (let ((saved (frame-parameter nil 'ygg-zoom-wconf)))
    (if saved
        (progn (set-frame-parameter nil 'ygg-zoom-wconf nil)
               (set-window-configuration saved))
      (if (and (one-window-p)
               (not (window-parameter (selected-window) 'window-side)))
          (message "Only one window")
        (let ((buf (current-buffer))
              (ignore-window-parameters t))
          (set-frame-parameter nil 'ygg-zoom-wconf (current-window-configuration))
          ;; a side window can never be the sole window; move its buffer into
          ;; a normal one first, then drop every panel along with the rest
          (when (window-parameter (selected-window) 'window-side)
            (select-window
             (or (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                           (window-list nil 'no-minibuf))
                 (split-window (frame-root-window) nil 'below)))
            (switch-to-buffer buf))
          (delete-other-windows))))))

(defun ygg-window-minimize ()
  "Collapse the focused window to a sliver; `SPC w +' restores its share.
Shrinks width for a left/right panel, height otherwise; works on side
windows (sidebar, quickfix, agent trace) too."
  (interactive)
  (if (one-window-p)
      (message "Only one window")
    (let* ((win (selected-window))
           (side (window-parameter win 'window-side))
           (horiz (cond ((memq side '(left right)) t)
                        ((memq side '(top bottom)) nil)
                        (t (not (window-combined-p win)))))
           (ignore-window-parameters t))
      (with-current-buffer (window-buffer win)
        (let* ((window-size-fixed nil)
               (cur (window-size win horiz))
               (floor (window-min-size win horiz t)))
          (when (> cur floor)
            (window-resize win (- floor cur) horiz t)))))))

(defun ygg-window-open-file-right ()
  "Open file at point in a split to the right (Helix C-w f)."
  (interactive)
  (let ((filename (word-at-point t)))
    (unless filename (user-error "No file at point"))
    (select-window (split-window-right))
    (find-file filename)))

(defun ygg-window-open-file-below ()
  "Open file at point in a split below (Helix C-w F, brief reversed)."
  (interactive)
  (let ((filename (word-at-point t)))
    (unless filename (user-error "No file at point"))
    (select-window (split-window-below))
    (find-file filename)))

(defun ygg-window-new-scratch ()
  "Create a new scratch buffer in a split to the right (C-w n)."
  (interactive)
  (select-window (split-window-right))
  (switch-to-buffer (generate-new-buffer "*scratch*")))

;;; Z-prefix buffer commands

(defun ygg-save-and-kill-buffer ()
  "Save the buffer, then kill it."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(defun ygg-kill-buffer-no-save ()
  "Kill the buffer without saving."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-current-buffer))

;;; Bindings

(yggdrasil-define-keys 'normal
  "d" #'ygg-delete-dwim :label "delete (dd line)"
  "c" #'ygg-change :label "change"
  "y" #'ygg-yank :label "yank"
  "p" #'ygg-paste-after :label "paste after"
  "P" #'ygg-paste-before :label "paste before"
  "C-p" #'ygg-paste-pop :label "paste-pop"
  "C-n" #'ygg-paste-undo-pop :label "paste-pop back"
  "R" #'ygg-overwrite :label "overwrite (vim R)"
  "r" #'ygg-replace-char :label "replace char"
  "J" #'ygg-join-lines :label "join"
  ">" #'ygg-indent-right :label "indent"
  "<" #'ygg-indent-left :label "dedent"
  "~" #'ygg-toggle-case :label "toggle case"
  "o" #'ygg-open-below :label "open below"
  "O" #'ygg-open-above :label "open above"
  "i" #'ygg-insert-before :label "insert"
  "a" #'ygg-insert-after :label "append"
  "I" #'ygg-insert-bol :label "insert bol"
  "A" #'ygg-insert-eol :label "append eol"
  "\"" #'ygg-use-register :label "register"
  "|" #'ygg-pipe-replace :label "pipe"
  "!" #'ygg-insert-command-before :label "insert command"
  "u" #'ygg-undo :label "undo"
  "U" #'ygg-redo :label "redo"
  "C-r" #'ygg-redo :label "redo"
  "C-s" #'save-buffer :label "save"
  "C-h" #'ygg-window-left
  "C-j" #'ygg-window-down
  "C-k" #'ygg-window-up
  "C-l" #'ygg-window-right)

;; side windows + minibuffer are reachable, edges wrap — every window
;; is one hjkl away, tmux-style
(defvar windmove-allow-all-windows)
(defvar windmove-wrap-around)
(setq windmove-allow-all-windows t
      windmove-wrap-around t)

;; global, not state-bound: window nav must work from the minibuffer,
;; terminals, and special buffers too (help lives on SPC h / F1)
(global-set-key (kbd "C-h") #'ygg-window-left)
(global-set-key (kbd "C-j") #'ygg-window-down)
(global-set-key (kbd "C-k") #'ygg-window-up)
(global-set-key (kbd "C-l") #'ygg-window-right)
(global-set-key (kbd "C-w") ygg-window-map)
(define-key minibuffer-local-map (kbd "C-w") #'backward-kill-word)

;;; Vim navigation for list buffers that keep their own verbs

(declare-function ygg-scroll-half-down "yggdrasil-motions")
(declare-function ygg-scroll-half-up "yggdrasil-motions")
(declare-function ygg-goto-first "yggdrasil-motions")
(declare-function ygg-goto-last-line "yggdrasil-motions")
(defvar tabulated-list-mode-map)
(defvar Buffer-menu-mode-map)

(defvar ygg-list-goto-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" (cons "first line" #'ygg-goto-first))
    (define-key map "r" (cons "refresh" #'revert-buffer))
    (define-key map "?" (cons "this mode's keys" #'describe-mode))
    map)
  "The g prefix in list buffers; g r takes over g's revert.")

(defun ygg-list-vim-keys (map)
  "Bind j k, C-d C-u, g g and G into list MAP."
  (define-key map "j" #'next-line)
  (define-key map "k" #'previous-line)
  (define-key map (kbd "C-d") #'ygg-scroll-half-down)
  (define-key map (kbd "C-u") #'ygg-scroll-half-up)
  (define-key map "g" ygg-list-goto-map)
  (define-key map "G" #'ygg-goto-last-line))

(with-eval-after-load 'tabulated-list
  (ygg-list-vim-keys tabulated-list-mode-map))

;; preloaded with no feature; its own k and C-d shadow the parent (d deletes)
(ygg-list-vim-keys Buffer-menu-mode-map)

(defun ygg--float-frame ()
  "A child frame floating over this frame that takes focus, or nil."
  (seq-find (lambda (frame)
              (and (eq (frame-parent frame) (selected-frame))
                   (frame-visible-p frame)
                   (fboundp 'aob-compose-frame-buffer)
                   (when-let* ((buffer (aob-compose-frame-buffer frame)))
                     (and (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (and (derived-mode-p 'aob-compose-mode)
                                 (not (bound-and-true-p aob-compose--anchor))))))))
            (frame-list)))

(defun ygg--windmove (dir)
  "Move DIR between windows, a float counting as one more of them.
From a float any direction lands back in the frame under it; from that
frame up with nothing above steps into the float."
  (cond
   ((frame-parent (selected-frame))
    (let ((parent (frame-parent (selected-frame))))
      (select-frame-set-input-focus parent)
      (select-window (frame-selected-window parent))))
   (t
    (condition-case nil
        (funcall (intern (format "windmove-%s" dir)))
      (error
       (if-let* ((float (and (eq dir 'up) (ygg--float-frame))))
           (progn (select-frame-set-input-focus float)
                  (select-window (frame-root-window float)))
         (message "no window %s" dir)))))))

(defun ygg-window-left () (interactive) (ygg--windmove 'left))
(defun ygg-window-down () (interactive) (ygg--windmove 'down))
(defun ygg-window-up () (interactive) (ygg--windmove 'up))
(defun ygg-window-right () (interactive) (ygg--windmove 'right))

(defun ygg--number-bounds ()
  "Bounds of hex/binary/octal/decimal number at point, or the next on this line."
  (save-excursion
    (let ((start-pos (point)))
      ;; Try to match at current position or after backing up slightly
      (let ((found nil))
        ;; First, try matching at point
        (when (looking-at "-?\\(?:0[xX][0-9a-fA-F]+\\|0[bB][01]+\\|0[oO][0-7]+\\|[0-9]+\\)")
          (setq found (cons (point) (match-end 0))))
        ;; If that didn't work, try backing up to find a number we're inside
        (unless found
          (skip-chars-backward "-0-9a-fA-FxXbBoO")
          (when (and (> (point) (point-min)) (not (memq (char-before) '(?\s ?\t ?\n ?\r))))
            (backward-char))
          (when (looking-at "-?\\(?:0[xX][0-9a-fA-F]+\\|0[bB][01]+\\|0[oO][0-7]+\\|[0-9]+\\)")
            (setq found (cons (point) (match-end 0)))))
        ;; If still not found, search forward
        (unless found
          (goto-char start-pos)
          (when (re-search-forward "-?\\(?:0[xX][0-9a-fA-F]+\\|0[bB][01]+\\|0[oO][0-7]+\\|[0-9]+\\)" (line-end-position) t)
            (setq found (cons (match-beginning 0) (match-end 0)))))
        found))))

(defun ygg--number-increment-at (pos count)
  "Add COUNT to number at POS; return end pos or nil.
Preserves hex/binary/octal base, case, and zero padding width."
  (goto-char pos)
  (let ((bounds (ygg--number-bounds)))
    (when bounds
      (let* ((text (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (negative (string-prefix-p "-" text))
             (stripped (if negative (substring text 1) text))
             (radix 10) (case-sensitive t) (padding 0) (value 0))
        (cond
         ((string-match "\\`0[xX]\\([0-9a-fA-F]+\\)" stripped)
          (let ((hex-digits (match-string 1 stripped)))
            (setq radix 16
                  case-sensitive (not (null (string-match "[A-F]" hex-digits)))
                  padding (length hex-digits)
                  value (string-to-number hex-digits 16))))
         ((string-match "\\`0[bB]\\([01]+\\)" stripped)
          (setq radix 2 padding (length (match-string 1 stripped))
                value (string-to-number (match-string 1 stripped) 2)))
         ((string-match "\\`0[oO]\\([0-7]+\\)" stripped)
          (setq radix 8 padding (length (match-string 1 stripped))
                value (string-to-number (match-string 1 stripped) 8)))
         (t
          (setq radix 10 padding (length stripped) value (string-to-number stripped))))
        (let* ((unsigned-val (if negative (- value) value))
               (new-value (+ unsigned-val count)))
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert (ygg--format-number new-value radix case-sensitive padding))
          (point))))))

(defun ygg--format-number (value radix case-sensitive padding)
  "Format VALUE in RADIX with PADDING width and optional CASE-SENSITIVE hex."
  (let* ((is-negative (< value 0))
         (abs-value (abs value))
         (formatted (cond
                      ((= radix 16)
                       (let ((hex (format (if case-sensitive "%X" "%x") abs-value)))
                         (concat "0x" (make-string (max 0 (- padding (length hex))) ?0) hex)))
                      ((= radix 2)
                       (let ((bin (format "%b" abs-value)))
                         (concat "0b" (make-string (max 0 (- padding (length bin))) ?0) bin)))
                      ((= radix 8)
                       (let ((oct (format "%o" abs-value)))
                         (concat "0o" (make-string (max 0 (- padding (length oct))) ?0) oct)))
                      (t
                       (let ((dec (format "%d" abs-value)))
                         (concat (make-string (max 0 (- padding (length dec))) ?0) dec))))))
    (concat (when is-negative "-") formatted)))

(defun ygg-number-increment (&optional count)
  "Add COUNT (default 1) to numbers at all selections."
  (interactive "p")
  (ygg-with-verb
    (let ((any-found nil))
      (ygg-do-selections
       (lambda (beg end _dir)
         (ignore end)
         (when (ygg--number-increment-at beg (or count 1)) (setq any-found t))))
      (unless any-found (user-error "No number on this line"))))
  (ygg--verb-exit))

(defun ygg-number-decrement (&optional count)
  "Subtract COUNT (default 1) from the number at or after point."
  (interactive "p")
  (ygg-number-increment (- (or count 1))))

(yggdrasil-define-keys 'normal
  "C-a" #'ygg-number-increment :label "increment number"
  "C-x" #'ygg-number-decrement :label "decrement number")

;;; g C-a / g C-x — sequential increment (evil-numbers g C-a)

(defun ygg--number-increment-sequential (count)
  "Add COUNT × (i+1) to the number at the i-th selection (buffer order),
skipping selections with no number."
  (let* ((regions (ygg--verb-regions))
         (i (1- (length regions))))
    (dolist (r (reverse regions))
      (ygg--number-increment-at (car r) (* count (1+ i)))
      (setq i (1- i)))))

(defun ygg-number-increment-sequential (&optional count)
  "Vim/evil-numbers g C-a: the i-th selection (buffer order) gets
COUNT × (i+1) added, so parallel cursors count up together."
  (interactive "p")
  (ygg-with-verb
    (ygg--number-increment-sequential (or count 1)))
  (ygg--verb-exit))

(defun ygg-number-decrement-sequential (&optional count)
  "Vim/evil-numbers g C-x: sequential decrement, see
`ygg-number-increment-sequential'."
  (interactive "p")
  (ygg-number-increment-sequential (- (or count 1))))

;;; g r / g R — rotate the symbol at point through a group (Doom rotate-text)

(defcustom ygg-rotate-groups
  '(("true" "false") ("yes" "no") ("on" "off") ("enable" "disable")
    ("enabled" "disabled") ("let" "const") ("get" "set") ("show" "hide")
    ("min" "max") ("width" "height") ("left" "right") ("up" "down")
    ("top" "bottom") ("first" "last") ("start" "end") ("and" "or")
    ("public" "private" "protected") ("&&" "||") ("==" "!=") ("++" "--"))
  "Groups of tokens `ygg-rotate-text' cycles between (case-insensitive)."
  :type '(repeat (repeat string)) :group 'yggdrasil)

(defconst ygg-rotate--op-chars "!&|=<>+*/~^%-"
  "Punctuation that forms an operator token when point is not on a symbol.")

(defun ygg-rotate--bounds (pos)
  "Bounds ((BEG . END)) of the symbol or operator run at POS, or nil."
  (save-excursion
    (goto-char pos)
    (or (bounds-of-thing-at-point 'symbol)
        (and (char-after) (memq (char-after) (string-to-list ygg-rotate--op-chars))
             (progn (skip-chars-backward ygg-rotate--op-chars)
                    (let ((b (point)))
                      (skip-chars-forward ygg-rotate--op-chars)
                      (cons b (point))))))))

(defun ygg-rotate--recase (template new)
  "Cast NEW into TEMPLATE's case (all-upper / capitalized / as-is)."
  (cond ((string= template (upcase template)) (upcase new))
        ((and (string= template (capitalize template))
              (not (string= template (downcase template))))
         (capitalize new))
        (t new)))

(defun ygg-rotate--next (token count)
  "TOKEN rotated COUNT steps within its `ygg-rotate-groups' group, or nil."
  (cl-loop for group in ygg-rotate-groups
           for i = (cl-position (downcase token) group
                                :test #'string= :key #'downcase)
           when i return (ygg-rotate--recase
                          token (nth (mod (+ i count) (length group)) group))))

(defun ygg-rotate--at (pos count)
  "Rotate the token at POS by COUNT in place; return non-nil on a change."
  (when-let* ((b (ygg-rotate--bounds pos))
              (new (ygg-rotate--next
                    (buffer-substring-no-properties (car b) (cdr b)) count)))
    (save-excursion (goto-char (car b))
                    (delete-region (car b) (cdr b))
                    (insert new))
    t))

(defun ygg-rotate-text (&optional count)
  "Rotate the symbol/operator at each cursor to the next member of its
`ygg-rotate-groups' group; negative COUNT rotates backward.  Token case
is preserved."
  (interactive "p")
  (let ((count (or count 1)) (any nil))
    (ygg-with-verb
      (dolist (r (reverse (ygg--verb-regions)))
        (when (ygg-rotate--at (car r) count) (setq any t))))
    (ygg--verb-exit)
    (unless any (message "nothing to rotate here"))))

(defun ygg-rotate-text-backward (&optional count)
  "Rotate the token at each cursor to the previous group member."
  (interactive "p")
  (ygg-rotate-text (- (or count 1))))

(yggdrasil-define-keys 'ygg-goto-map
  "I" #'ygg-insert-column-0 :label "insert at column 0"
  "C-a" #'ygg-number-increment-sequential :label "sequential increment"
  "C-x" #'ygg-number-decrement-sequential :label "sequential decrement"
  "P" #'ygg-reselect-paste :label "select last paste"
  "y" #'ygg-yank-unindented :label "yank unindented"
  "q" #'ygg-reflow :label "reflow"
  "X" #'ygg-exchange :label "exchange"
  "!" #'ygg-rotate-text :label "rotate token"
  "(" #'ygg-rotate-text-backward :label "rotate token back"
  "|" #'ygg-pipe-discard :label "pipe (discard)"
  "$" #'ygg-keep-pipe :label "keep pipe"
  "A" #'ygg-insert-command-after :label "append command")

;;; Macros, vim keys: q records into a register / stops, @ plays, @@ replays

(defvar ygg--macro-recording-register nil
  "Register char currently recording into (vim q), or nil.")
(defvar ygg--macro-recording-keys nil
  "Keys collected so far this recording, newest first.")
(defvar ygg--macro-last-register nil
  "Register char last played with @, for @@.")

(defun ygg--macro-tag-update ()
  (setq ygg--macro-tag
        (if ygg--macro-recording-register
            (propertize (format " recording @%c " ygg--macro-recording-register)
                        'face 'ygg-state-visual)
          ""))
  (force-mode-line-update))

(defun ygg--macro-record-collect ()
  "Append this command's keys to an in-progress recording.
Excludes the `q' that started or will stop it; a macro is just keys in
a register, recorded ourselves since kmacro can't see replayed keys
\(`execute-kbd-macro'-driven, as `.' and `@' both are\)."
  (when (and ygg--macro-recording-register
             (not ygg--replaying)
             (not (eq this-command 'ygg-macro-record)))
    (push (this-command-keys-vector) ygg--macro-recording-keys)))

(add-hook 'yggdrasil-local-mode-hook
          (lambda ()
            (if yggdrasil-local-mode
                (add-hook 'post-command-hook #'ygg--macro-record-collect nil t)
              (remove-hook 'post-command-hook #'ygg--macro-record-collect t))))

(defun ygg-macro-record ()
  "Vim q: start recording into a register, or stop if already recording."
  (interactive)
  (if ygg--macro-recording-register
      (let ((reg ygg--macro-recording-register))
        (puthash reg (apply #'vconcat (nreverse ygg--macro-recording-keys))
                 ygg--registers)
        (setq ygg--macro-recording-register nil ygg--macro-recording-keys nil)
        (ygg--macro-tag-update)
        (message "yggdrasil: recorded @%c" reg))
    (let ((reg (read-char "record macro into register: ")))
      (setq ygg--macro-recording-register reg ygg--macro-recording-keys nil)
      (ygg--macro-tag-update))))

(defun ygg--macro-executable (reg)
  "Raw kbd-macro data for REG, ready for `execute-kbd-macro'.
A yank register (list of texts) plays as vim does: its text as keys."
  (let ((val (gethash reg ygg--registers)))
    (cond
     ((null val) (user-error "Register %c is empty" reg))
     ((listp val) (mapconcat #'identity val "\n"))
     (t val))))

(defun ygg-macro-play (&optional count)
  "Vim @: play a register's macro COUNT times; @@ replays the last one."
  (interactive "p")
  (let ((reg (read-char "play macro register: ")))
    (when (eq reg ?@)
      (unless ygg--macro-last-register (user-error "No previously played macro"))
      (setq reg ygg--macro-last-register))
    (setq ygg--macro-last-register reg)
    (if (eq reg ?:)
        (if (fboundp 'ygg-ex-repeat-last)
            (ygg-ex-repeat-last (or count 1))
          (user-error "yggdrasil: ex command history not available"))
      (let ((ygg--replaying t))
        (execute-kbd-macro (ygg--macro-executable reg) count)))))

(yggdrasil-define-keys 'normal
  "q" #'ygg-macro-record :label "record macro"
  "@" #'ygg-macro-play :label "play macro")

;;; Folds, vim keys on built-in hideshow

(declare-function hs-toggle-hiding "hideshow")
(declare-function hs-hide-block "hideshow")
(declare-function hs-show-block "hideshow")
(declare-function hs-hide-all "hideshow")
(declare-function hs-show-all "hideshow")

(declare-function outline-cycle "outline")
(declare-function outline-hide-subtree "outline")
(declare-function outline-show-subtree "outline")
(declare-function outline-hide-body "outline")
(declare-function outline-show-all "outline")

(defvar hs-allow-nesting)

(defun ygg-fold--call (block section)
  "Fold with SECTION where the buffer is headings, with BLOCK where it is code.
Hideshow reads syntax to find a block, and a document made of headings
has none to read: on a Work card the whole `z' family turns a minor mode
on and then does nothing with it."
  (if (bound-and-true-p outline-minor-mode)
      (funcall section)
    (unless (bound-and-true-p hs-minor-mode) (hs-minor-mode 1))
    (let ((hs-allow-nesting t))
      (funcall block))))

(declare-function ygg-fold--manual-here "yggdrasil-motions")
(declare-function ygg-fold--manual-in "yggdrasil-motions")
(declare-function ygg-fold--manual-set "yggdrasil-motions")
(declare-function ygg-fold--manual-closed-p "yggdrasil-motions")

(defun ygg-fold--manual-all (closed)
  (dolist (o (ygg-fold--manual-in (point-min) (point-max)))
    (ygg-fold--manual-set o closed)))

(defun ygg-fold-toggle ()
  (interactive)
  (if-let* ((ov (ygg-fold--manual-here)))
      (ygg-fold--manual-set ov (not (ygg-fold--manual-closed-p ov)))
    (ygg-fold--call #'hs-toggle-hiding #'outline-cycle)))
(defun ygg-fold-close ()
  (interactive)
  (if-let* ((ov (ygg-fold--manual-here)))
      (ygg-fold--manual-set ov t)
    (ygg-fold--call #'hs-hide-block #'outline-hide-subtree)))
(defun ygg-fold-open ()
  (interactive)
  (if-let* ((ov (ygg-fold--manual-here)))
      (ygg-fold--manual-set ov nil)
    (ygg-fold--call #'hs-show-block #'outline-show-subtree)))
(defun ygg-fold-close-all ()
  (interactive)
  (ygg-fold--manual-all t)
  (ygg-fold--call #'hs-hide-all #'outline-hide-body))
(defun ygg-fold-open-all ()
  (interactive)
  (ygg-fold--manual-all nil)
  (ygg-fold--call #'hs-show-all #'outline-show-all))

(yggdrasil-define-keys 'normal
  "z a" #'ygg-fold-toggle :label "fold toggle"
  "z c" #'ygg-fold-close :label "fold close"
  "z o" #'ygg-fold-open :label "fold open"
  "z M" #'ygg-fold-close-all :label "fold all"
  "z R" #'ygg-fold-open-all :label "unfold all")

(defun ygg-undo (&optional n)
  "Whole-buffer undo; never undo-in-region (the region is always
active in normal state, which would silently scope undo to it)."
  (interactive "p")
  (let ((mark-active nil))
    (undo n))
  (set-mark (point)))

(defun ygg-redo (&optional n)
  "Whole-buffer redo, immune to the always-active region."
  (interactive "p")
  (let ((mark-active nil))
    (undo-redo n))
  (set-mark (point)))

(yggdrasil-define-keys 'visual
  "p" #'ygg-visual-paste :label "paste over")

(yggdrasil-define-keys 'ygg-window-map
  "s" #'ygg-window-split-below :label "split below"
  "v" #'ygg-window-split-right :label "split right"
  "q" #'delete-window :label "close"
  "c" #'delete-window :label "close"
  "o" #'delete-other-windows :label "only"
  "w" #'other-window :label "other"
  "z" #'ygg-window-zoom-toggle :label "zoom (maximize)"
  "x" #'ygg-window-swap-next :label "swap"
  "r" #'ygg-window-swap-next :label "rotate"
  "=" #'balance-windows :label "balance"
  "-" #'ygg-window-minimize :label "minimize (sliver)"
  "+" #'balance-windows :label "restore share"
  "h" #'ygg-window-left :label "left"
  "j" #'ygg-window-down :label "down"
  "k" #'ygg-window-up :label "up"
  "l" #'ygg-window-right :label "right"
  "H" #'windmove-swap-states-left :label "swap left"
  "J" #'windmove-swap-states-down :label "swap down"
  "K" #'windmove-swap-states-up :label "swap up"
  "L" #'windmove-swap-states-right :label "swap right"
  "C-w" #'other-window :label "other"
  "C-s" #'ygg-window-split-below :label "split below"
  "C-v" #'ygg-window-split-right :label "split right"
  "C-q" #'delete-window :label "close"
  "C-o" #'delete-other-windows :label "only"
  "C-h" #'ygg-window-left :label "left"
  "C-j" #'ygg-window-down :label "down"
  "C-k" #'ygg-window-up :label "up"
  "C-l" #'ygg-window-right :label "right"
  "t" #'ygg-window-swap-next :label "swap"
  "f" #'ygg-window-open-file-right :label "open file right"
  "F" #'ygg-window-open-file-below :label "open file below"
  "n" #'ygg-window-new-scratch :label "new scratch")

(yggdrasil-define-keys 'ygg-z-cap-map
  "Z" #'ygg-save-and-kill-buffer :label "save & quit"
  "Q" #'ygg-kill-buffer-no-save :label "quit!")

(yggdrasil-define-keys 'normal
  "[ SPC" #'ygg-add-newline-above :label "add line above"
  "] SPC" #'ygg-add-newline-below :label "add line below")

(yggdrasil-define-keys 'ygg-selections-map
  "S" #'ygg-save-selection :label "save selection to jumplist"
  "J" #'ygg-join-lines-space :label "join with space selection"
  "d" #'ygg-delete-via-blackhole :label "delete (no yank)"
  "c" #'ygg-change-via-blackhole :label "change (no yank)")

(provide 'yggdrasil-verbs)
;;; yggdrasil-verbs.el ends here
