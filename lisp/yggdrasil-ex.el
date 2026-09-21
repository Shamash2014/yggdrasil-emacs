;;; yggdrasil-ex.el --- Yggdrasil ex command line -*- lexical-binding: t; -*-

;; Built-ins wrapped: minibuffer read (completing-read), re-search-forward
;; + replace-match, query-replace-regexp, async-shell-command,
;; shell-command-to-string, save-buffer/write-file/find-file, windows.
;; Custom: range tokenizer, command-name resolver, substitute/global engines.

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'posframe nil t)
(require 'yggdrasil-core)
(require 'yggdrasil-selection)

;;; Range parsing

(defun ygg-ex--last-line ()
  "Last real line: a trailing newline's phantom empty line doesn't count."
  (line-number-at-pos
   (if (and (> (point-max) 1) (eq (char-before (point-max)) ?\n))
       (1- (point-max))
     (point-max))))

(defun ygg-ex--skip-ws (string pos)
  (let ((len (length string)))
    (while (and (< pos len) (memq (aref string pos) '(?\s ?\t)))
      (setq pos (1+ pos)))
    pos))

(defun ygg-ex--parse-address (string pos-cell)
  "Parse one ex address at (car POS-CELL) in STRING, advancing it.
Return a line number, or nil when no address starts there."
  (let* ((len (length string))
         (pos (car pos-cell))
         (base nil))
    (when (< pos len)
      (let ((c (aref string pos)))
        (cond
         ((eq c ?.)
          (setq base (line-number-at-pos (point)) pos (1+ pos)))
         ((eq c ?$)
          (setq base (ygg-ex--last-line) pos (1+ pos)))
         ((eq c ?')
          (let ((mc (and (< (1+ pos) len) (aref string (1+ pos)))))
            (unless ygg--last-visual
              (user-error "yggdrasil: no previous visual selection"))
            (pcase-let ((`(,vb ,ve ,_) ygg--last-visual))
              (setq base (line-number-at-pos (if (eq mc ?<) vb ve))))
            (setq pos (+ pos 2))))
         ((<= ?0 c ?9)
          (let ((start pos))
            (while (and (< pos len) (<= ?0 (aref string pos) ?9))
              (setq pos (1+ pos)))
            (setq base (string-to-number (substring string start pos)))))
         ((memq c '(?+ ?-))
          (setq base (line-number-at-pos (point))))))
      (setcar pos-cell pos))
    (when base
      (let ((cur base) (p (car pos-cell)))
        (while (and (< p len) (memq (aref string p) '(?+ ?-)))
          (let* ((sign (if (eq (aref string p) ?+) 1 -1))
                 (start (1+ p)))
            (setq p start)
            (while (and (< p len) (<= ?0 (aref string p) ?9))
              (setq p (1+ p)))
            (let ((numstr (substring string start p)))
              (setq cur (+ cur (* sign (if (string-empty-p numstr) 1
                                          (string-to-number numstr))))))))
        (setcar pos-cell p)
        (setq base cur)))
    base))

(defun ygg-ex--parse-range (string)
  "Parse a leading ex address range off STRING.
Return (RANGE . REST): RANGE is (BEG-LINE . END-LINE), or nil when
STRING carries no range; REST is the unconsumed tail of STRING."
  (let* ((len (length string))
         (pos (ygg-ex--skip-ws string 0)))
    (if (and (< pos len) (eq (aref string pos) ?%))
        (cons (cons 1 (ygg-ex--last-line))
              (substring string (1+ pos)))
      (let* ((pos-cell (list pos))
             (beg (ygg-ex--parse-address string pos-cell)))
        (if (null beg)
            (cons nil string)
          (let ((p (ygg-ex--skip-ws string (car pos-cell))))
            (if (and (< p len) (eq (aref string p) ?,))
                (progn
                  (setcar pos-cell (ygg-ex--skip-ws string (1+ p)))
                  (let ((end (ygg-ex--parse-address string pos-cell)))
                    (unless end (user-error "yggdrasil: malformed range"))
                    (cons (cons beg end) (substring string (car pos-cell)))))
              (cons (cons beg beg) (substring string (car pos-cell))))))))))

(defun ygg-ex--line-range-bounds (range)
  "Char (BEG END) spanning whole lines (car RANGE)..(cdr RANGE)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (car range)))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (cdr range))
      (list beg (point)))))

(defun ygg-ex--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;;; Substitute

(defun ygg-ex--unescape-delim (s delim)
  (replace-regexp-in-string (regexp-quote (concat "\\" (string delim)))
                             (string delim) s t t))

(defun ygg-ex--split-delimited (string)
  "Split STRING (starting with a delimiter char) into (RE REP FLAGS)."
  (let* ((delim (aref string 0))
         (len (length string))
         (positions nil)
         (i 1))
    (while (and (< i len) (< (length positions) 2))
      (cond
       ((and (eq (aref string i) ?\\) (< (1+ i) len)
             (eq (aref string (1+ i)) delim))
        (setq i (+ i 2)))
       ((eq (aref string i) delim)
        (push i positions)
        (setq i (1+ i)))
       (t (setq i (1+ i)))))
    (setq positions (nreverse positions))
    (unless positions
      (user-error "yggdrasil: malformed substitute"))
    (let* ((p1 (nth 0 positions))
           (p2 (nth 1 positions))
           (re (ygg-ex--unescape-delim (substring string 1 p1) delim))
           (rep (ygg-ex--unescape-delim (substring string (1+ p1) (or p2 len)) delim))
           (flags (if p2 (substring string (1+ p2)) "")))
      (list re rep flags))))

(defun ygg-ex--substitute-region (beg end re rep global-p)
  "Replace RE with REP between BEG and END.
When GLOBAL-P, every match; otherwise only the first match per line."
  (let ((end-marker (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (goto-char beg)
          (let ((last-line nil))
            ;; zero-width matches can step point past the bound, and can
            ;; land exactly AT the exclusive bound — both must stop the loop
            (while (and (< (point) end-marker)
                        (re-search-forward re end-marker t)
                        (< (match-beginning 0) end-marker))
              (let* ((mbeg (match-beginning 0))
                     (mend (match-end 0))
                     (line (line-number-at-pos mbeg))
                     (empty (= mbeg mend)))
                (if (or global-p (not (equal line last-line)))
                    (progn (replace-match rep nil nil) (setq last-line line))
                  (goto-char mend))
                (when (and empty (< (point) (point-max)))
                  (forward-char 1))))))
      (set-marker end-marker nil))))

(defun ygg-ex--substitute-query (range re rep)
  (if range
      (pcase-let ((`(,beg ,end) (ygg-ex--line-range-bounds range)))
        (query-replace-regexp re rep nil beg end))
    (query-replace-regexp re rep)))

(defun ygg-ex--cmd-substitute (range _bang args)
  (when (string-empty-p args)
    (user-error "yggdrasil: substitute needs a pattern"))
  (pcase-let ((`(,re ,rep ,flags) (ygg-ex--split-delimited args)))
    (setq re (ygg-regexp re))
    (let ((global-p (and (cl-find ?g flags) t))
          (case-fold-search (and (cl-find ?i flags) t)))
      (if (cl-find ?c flags)
          (ygg-ex--substitute-query range re rep)
        (ygg-with-verb
          (cond
           (range
            (pcase-let ((`(,beg ,end) (ygg-ex--line-range-bounds range)))
              (ygg-ex--substitute-region beg end re rep global-p)))
           ((> (ygg-selections-count) 1)
            (ygg-do-selections
             (lambda (b e _dir) (ygg-ex--substitute-region b e re rep global-p))))
           (t
            (pcase-let ((`(,sb ,se ,_) (ygg-selection-bounds)))
              (if (/= sb se)
                  (ygg-ex--substitute-region sb se re rep global-p)
                (ygg-ex--substitute-region (line-beginning-position)
                                           (line-end-position) re rep global-p))))))))))

;;; Global

(defun ygg-ex--parse-global (args)
  (let* ((delim (aref args 0))
         (len (length args))
         (i 1))
    (while (and (< i len) (not (eq (aref args i) delim)))
      (setq i (1+ i)))
    (unless (< i len) (user-error "yggdrasil: malformed global"))
    (list (substring args 1 i) (string-trim (substring args (1+ i))))))

(defun ygg-ex--delete-line (ln)
  "Delete line LN and exactly one adjoining newline."
  (ygg-ex--goto-line ln)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (if (< end (point-max))
        (delete-region beg (1+ end))
      (delete-region (max (point-min) (1- beg)) end))))

(defun ygg-ex--global-lines (r re invert)
  "Markers at bol of lines in range R matching RE (or not, when INVERT).
Markers, not line numbers, so per-line edits never invalidate the list."
  (let (markers)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- (car r)))
      (cl-loop repeat (1+ (- (cdr r) (car r)))
               while (not (eobp))
               do (let ((m (string-match-p re (buffer-substring-no-properties
                                               (line-beginning-position)
                                               (line-end-position)))))
                    (when (if invert (not m) m)
                      (push (copy-marker (line-beginning-position)) markers)))
                  (forward-line 1)))
    (nreverse markers)))

(defun ygg-ex--cmd-global (range bang args)
  "vim :g[!]/re/cmd — run CMD on lines matching RE (or not matching, with !).
CMD defaults to reporting the count; d takes a no-yank fast path; anything
else (s///, normal, move/copy, join, > <) routes through the ex dispatcher
with the matched line as its implicit range."
  (when (string-empty-p args)
    (user-error "yggdrasil: global needs a pattern"))
  (pcase-let ((`(,re ,action) (ygg-ex--parse-global args)))
    (setq re (ygg-regexp re))
    (let* ((r (or range (cons 1 (ygg-ex--last-line))))
           (markers (ygg-ex--global-lines r re bang)))
      (cond
       ((string-empty-p action)
        (when markers (goto-char (car markers)))
        (message "yggdrasil: %d matching line%s" (length markers)
                 (if (= (length markers) 1) "" "s")))
       ((string= action "d")
        (ygg-with-verb
          (dolist (m (nreverse markers))       ; bottom-up: line numbers stay put
            (when (marker-position m)
              (goto-char m) (ygg-ex--delete-line (line-number-at-pos)))
            (set-marker m nil))))
       (t
        (ygg-with-verb
          (dolist (m markers)
            (when (marker-position m)
              (save-excursion
                (goto-char m)
                (pcase-let ((`(,arange . ,arest) (ygg-ex--parse-range action)))
                  (ygg-ex--run (or arange (cons (line-number-at-pos)
                                                (line-number-at-pos)))
                               arest))))
            (set-marker m nil))))))))

(defun ygg-ex--cmd-vglobal (range _bang args)
  "vim :v/re/cmd — :global acting on the non-matching lines."
  (ygg-ex--cmd-global range t args))

;;; Range defaults shared by :normal and :sort

(defun ygg-ex--selection-line-span ()
  "The active selection's line span, or nil when nothing is selected."
  (pcase-let ((`(,sb ,se ,_) (ygg-selection-bounds)))
    (when (/= sb se)
      (pcase-let ((`(,eb ,ee ,_) (ygg-selection-effective-bounds)))
        (cons (line-number-at-pos eb) (line-number-at-pos (1- ee)))))))

;;; Normal

(defvar ygg-ex--normal-depth 0
  "Recursion guard: :normal invoking :normal errors past this depth.")

(defun ygg-ex--normal-range (range)
  (or range (ygg-ex--selection-line-span)
      (cons (line-number-at-pos (point)) (line-number-at-pos (point)))))

(defun ygg-ex--cmd-normal (range _bang args)
  (when (string-empty-p args)
    (user-error "yggdrasil: normal needs keys"))
  (when (> ygg-ex--normal-depth 10)
    (user-error "yggdrasil: :normal nested too deep"))
  (let ((keys (kbd args))
        (r (ygg-ex--normal-range range))
        (ygg-ex--normal-depth (1+ ygg-ex--normal-depth)))
    (ygg-with-verb
      (cl-loop for ln from (car r) to (cdr r)
               do (ygg-ex--goto-line ln)
                  (execute-kbd-macro keys)
                  (ygg-normal-state)))))

;;; Sort

(defun ygg-ex--selection-regions ()
  "All active selections as (BEG END PRIMARY), ascending buffer order.
`ygg-do-selections' visits secondaries then the primary last."
  (let* ((n (ygg-selections-count)) (i 0) regions)
    (ygg-do-selections
     (lambda (beg end _dir)
       (setq i (1+ i))
       (push (list beg end (= i n)) regions)))
    (sort regions (lambda (a b) (< (car a) (car b))))))

(defun ygg-ex--sort-selection-texts (descending)
  "Reorder every selection's text among the others (Helix behavior):
the Nth selection in buffer order gets the Nth sorted text."
  (let* ((regions (ygg-ex--selection-regions))
         (sorted (sort (mapcar (lambda (r) (buffer-substring-no-properties (nth 0 r) (nth 1 r)))
                                regions)
                       (if descending #'string> #'string<)))
         (pairs (cl-mapcar #'cons regions sorted))
         installed)
    (dolist (pair (reverse pairs))
      (pcase-let ((`(,beg ,end ,primary) (car pair)) (text (cdr pair)))
        (delete-region beg end)
        (goto-char beg)
        (insert text)
        (push (list (copy-marker beg) (copy-marker (point)) primary) installed)))
    (ygg-clear-secondaries)
    (let (primary-region)
      (dolist (r installed)
        (pcase-let ((`(,bm ,em ,primary) r))
          (if primary
              (setq primary-region (cons (marker-position bm) (marker-position em)))
            (ygg-add-selection (marker-position bm) (marker-position em)))
          (set-marker bm nil)
          (set-marker em nil)))
      (when primary-region
        (ygg-set-selection (car primary-region) (cdr primary-region))))))

(defun ygg-ex--sort-range (range)
  (or range (ygg-ex--selection-line-span) (cons 1 (ygg-ex--last-line))))

(defun ygg-ex--do-sort (range descending args)
  (ygg-with-verb
    (if (> (ygg-selections-count) 1)
        (ygg-ex--sort-selection-texts descending)
      (pcase-let ((`(,beg ,end) (ygg-ex--line-range-bounds (ygg-ex--sort-range range))))
        (sort-lines descending beg end)
        (when (cl-find ?u args) (delete-duplicate-lines beg end))))))

(defun ygg-ex--cmd-sort (range bang args)
  (ygg-ex--do-sort range (and bang t) args))

(defun ygg-ex--cmd-rsort (range _bang args)
  (ygg-ex--do-sort range t args))

;;; File / buffer / window commands

(defun ygg-ex--save-buffer (force)
  "Save the current buffer; FORCE writes past a read-only buffer/file (vim :w!)."
  (when force
    (setq buffer-read-only nil)
    (when (and buffer-file-name (file-exists-p buffer-file-name)
               (not (file-writable-p buffer-file-name)))
      (ignore-errors
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o200)))))
  (save-buffer))

(declare-function wdired-finish-edit "wdired")
(declare-function wdired-abort-changes "wdired")

(defun ygg-ex--cmd-write (_range bang args)
  (cond
   ;; wdired: :w commits the edited names to the filesystem (oil's :w)
   ((derived-mode-p 'wdired-mode) (wdired-finish-edit))
   ((string-empty-p args) (ygg-ex--save-buffer bang))
   (t (write-file (expand-file-name args)))))

(declare-function with-editor-finish "with-editor")
(declare-function with-editor-cancel "with-editor")
(defvar with-editor-mode)

(defun ygg-ex--cmd-quit (_range bang _args)
  (cond
   ;; wdired: :q drops the pending name edits and returns to dired
   ((derived-mode-p 'wdired-mode) (wdired-abort-changes))
   ;; in a commit/rebase/tag buffer, :q aborts the with-editor session;
   ;; :q! forces it (skips the "discard message?" confirmation), like vim
   ((bound-and-true-p with-editor-mode) (with-editor-cancel bang))
   (t (when bang (set-buffer-modified-p nil))
      (if (> (count-windows) 1)
          (delete-window)
        (kill-buffer)))))

(defun ygg-ex--cmd-wq (range bang args)
  ;; :wq / :x follow ZZ's per-buffer remap — a compose buffer sends, an
  ;; artifact approves, a commit/rebase/wgrep buffer finishes — so the
  ;; two ways to "write & close" never diverge
  (if-let* ((zz (command-remapping 'ygg-save-and-kill-buffer)))
      (call-interactively zz)
    (ygg-ex--save-buffer bang)
    (ygg-ex--cmd-quit range bang args)))

(defun ygg-ex--cmd-edit (_range bang args)
  (cond
   ((not (string-empty-p args)) (find-file (expand-file-name args)))
   (buffer-file-name
    (cond (bang (revert-buffer t t))
          ((buffer-modified-p)
           (user-error "yggdrasil: no write since last change (add ! to override)"))
          (t (revert-buffer t t))))
   ;; dired / magit / other refreshable buffers: :e re-reads (oil-style)
   ((or (derived-mode-p 'dired-mode) revert-buffer-function)
    (revert-buffer t t))
   (t (user-error "yggdrasil: buffer has no file"))))

(defun ygg-ex--cmd-quitall (_range bang _args)
  (if bang (kill-emacs) (save-buffers-kill-terminal)))

(defun ygg-ex--cmd-wall (_range _bang _args)
  (save-some-buffers t))

(defun ygg-ex--cmd-wqall (_range bang _args)
  (save-some-buffers t)
  (if bang (kill-emacs) (save-buffers-kill-terminal)))

(defun ygg-ex--cmd-recenter (_range _bang _args)
  (recenter))

(declare-function ygg-hlsearch-clear "yggdrasil-motions")
(defun ygg-ex--cmd-nohlsearch (_range _bang _args)
  (ygg-hlsearch-clear))

(defun ygg-ex--cmd-bdelete (_range bang args)
  (let ((buf (if (string-empty-p args) (current-buffer) (get-buffer args))))
    (unless buf (user-error "No such buffer: %s" args))
    (when bang (with-current-buffer buf (set-buffer-modified-p nil)))
    (kill-buffer buf)))

(defun ygg-ex--cmd-bnext (_range _bang _args) (next-buffer))
(defun ygg-ex--cmd-bprevious (_range _bang _args) (previous-buffer))

(declare-function tab-new "tab-bar")
(declare-function tab-close "tab-bar")
(declare-function tab-close-other "tab-bar")
(declare-function tab-next "tab-bar")
(declare-function tab-previous "tab-bar")
(declare-function ygg-space-sibling "yggdrasil-spacetree")
(declare-function ygg-space-close "yggdrasil-spacetree")
(declare-function ygg-space-next-sibling "yggdrasil-spacetree")
(declare-function ygg-space-prev-sibling "yggdrasil-spacetree")

;; tabs are the spacetree now, so :tab* drives spaces when it is loaded
(defun ygg-ex--cmd-tabnew (_range _bang args)
  (if (fboundp 'ygg-space-sibling) (ygg-space-sibling) (tab-new))
  (unless (string-empty-p args) (find-file (expand-file-name args))))
(defun ygg-ex--cmd-tabclose (_range _bang _args)
  (if (fboundp 'ygg-space-close) (ygg-space-close) (tab-close)))
(defun ygg-ex--cmd-tabonly (_range _bang _args) (ignore-errors (tab-close-other)))
(defun ygg-ex--cmd-tabnext (_range _bang _args)
  (if (fboundp 'ygg-space-next-sibling) (ygg-space-next-sibling) (tab-next)))
(defun ygg-ex--cmd-tabprev (_range _bang _args)
  (if (fboundp 'ygg-space-prev-sibling) (ygg-space-prev-sibling) (tab-previous)))

(defun ygg-ex--cmd-buffer (_range _bang args)
  (when (string-empty-p args) (user-error "yggdrasil: buffer needs a name"))
  (switch-to-buffer args))

(defun ygg-ex--cmd-split (_range _bang _args)
  (select-window (split-window-below)))

(defun ygg-ex--cmd-vsplit (_range _bang _args)
  (select-window (split-window-right)))

(defun ygg-ex--cmd-new (_range _bang _args)
  (select-window (split-window-below))
  (switch-to-buffer (generate-new-buffer "*new*")))

(defun ygg-ex--read-shell (command)
  "Stream COMMAND's stdout in below the current line asynchronously."
  (let ((marker (copy-marker (save-excursion (end-of-line)
                                             (unless (eobp) (forward-char 1))
                                             (point))
                             t)))
    (make-process
     :name "ygg-ex-read" :buffer nil :noquery t
     :command (list shell-file-name shell-command-switch command)
     :filter (lambda (_proc chunk)
               (when (marker-buffer marker)
                 (with-current-buffer (marker-buffer marker)
                   (save-excursion (goto-char marker) (insert chunk)))))
     :sentinel (lambda (_proc _event) (set-marker marker nil)))))

(defun ygg-ex--cmd-read (_range _bang args)
  (when (string-empty-p args)
    (user-error "yggdrasil: read needs a file or !cmd"))
  (if (string-prefix-p "!" args)
      (ygg-ex--read-shell (string-trim-left (substring args 1)))
    (ygg-with-verb
      (save-excursion
        (end-of-line)
        (unless (eobp) (forward-char 1))
        (insert-file-contents (expand-file-name args))))))

;;; Line-range commands (evil/vim: :move :copy :delete :yank :put :join :>/:<)

(defun ygg-ex--range-lines (range)
  "RANGE as (BEG . END) lines, defaulting to the current line."
  (or range (let ((l (line-number-at-pos))) (cons l l))))

(defun ygg-ex--parse-dest (args)
  "Parse a destination address off ARGS (0 means above the first line)."
  (let ((s (string-trim args)))
    (when (string-empty-p s)
      (user-error "yggdrasil: destination line required"))
    (let* ((pos-cell (list 0))
           (line (ygg-ex--parse-address s pos-cell)))
      (unless line (user-error "yggdrasil: bad destination: %s" s))
      line)))

(defun ygg-ex--insert-lines-after (dest text)
  "Insert whole-line TEXT after line DEST (0 = buffer top).
Return (BEG . END) of the inserted block."
  (goto-char (point-min))
  (forward-line dest)
  (when (and (eobp) (not (bolp))) (insert "\n"))
  (let ((text (if (string-suffix-p "\n" text) text (concat text "\n")))
        (beg (point)))
    (insert text)
    (cons beg (point))))

(defun ygg-ex--select-block (beg end)
  "Put the primary selection over the line block BEG..END (END exclusive)."
  (ygg-clear-secondaries)
  (goto-char beg)
  (ygg-set-selection beg (max beg (1- end))))

(defun ygg-ex--cmd-move (range _bang args)
  (pcase-let* ((`(,beg . ,end) (ygg-ex--range-lines range))
               (dest (ygg-ex--parse-dest args)))
    (when (and (>= dest beg) (<= dest end))
      (user-error "yggdrasil: cannot move lines into themselves"))
    (ygg-with-verb
      (pcase-let ((`(,cbeg ,cend) (ygg-ex--line-range-bounds (cons beg end))))
        (let ((text (buffer-substring cbeg cend))
              (nlines (1+ (- end beg))))
          (delete-region cbeg cend)
          (let ((ins (ygg-ex--insert-lines-after
                      (if (> dest end) (- dest nlines) dest) text)))
            (ygg-ex--select-block (car ins) (cdr ins))))))))

(defun ygg-ex--cmd-copy (range _bang args)
  (pcase-let* ((`(,beg . ,end) (ygg-ex--range-lines range))
               (dest (ygg-ex--parse-dest args)))
    (ygg-with-verb
      (pcase-let ((`(,cbeg ,cend) (ygg-ex--line-range-bounds (cons beg end))))
        (let* ((text (buffer-substring cbeg cend))
               (ins (ygg-ex--insert-lines-after dest text)))
          (ygg-ex--select-block (car ins) (cdr ins)))))))

(defun ygg-ex--cmd-delete (range _bang _args)
  (pcase-let ((`(,beg . ,end) (ygg-ex--range-lines range)))
    (ygg-with-verb
      (pcase-let ((`(,cbeg ,cend) (ygg-ex--line-range-bounds (cons beg end))))
        (kill-new (buffer-substring cbeg cend))
        (delete-region cbeg cend)
        (ygg-clear-secondaries)
        (goto-char (min cbeg (point-max)))
        (ygg-set-selection (point) (point))))))

(defun ygg-ex--cmd-yank (range _bang _args)
  (pcase-let ((`(,beg . ,end) (ygg-ex--range-lines range)))
    (pcase-let ((`(,cbeg ,cend) (ygg-ex--line-range-bounds (cons beg end))))
      (kill-new (buffer-substring cbeg cend))
      (message "yggdrasil: yanked %d line(s)" (1+ (- end beg))))))

(defun ygg-ex--cmd-put (range _bang _args)
  (let ((dest (cdr (ygg-ex--range-lines range)))
        (text (current-kill 0)))
    (ygg-with-verb
      (let ((ins (ygg-ex--insert-lines-after dest text)))
        (goto-char (car ins))
        (ygg-set-selection (car ins) (car ins))))))

(defun ygg-ex--cmd-join (range bang _args)
  (pcase-let ((`(,beg . ,end) (ygg-ex--range-lines range)))
    (when (= beg end) (setq end (1+ end)))
    (ygg-with-verb
      (ygg-ex--goto-line beg)
      (dotimes (_ (- end beg))
        (end-of-line)
        (unless (eobp)
          (if bang (delete-char 1) (delete-indentation t))))
      (ygg-set-selection (point) (point)))))

(defun ygg-ex--cmd-shift (range chars _args)
  "Indent (>) or dedent (<) the RANGE by one shiftwidth per char in CHARS."
  (pcase-let ((`(,beg . ,end) (ygg-ex--range-lines range)))
    (ygg-with-verb
      (pcase-let ((`(,cbeg ,cend) (ygg-ex--line-range-bounds (cons beg end))))
        (indent-rigidly cbeg cend
                        (* (length chars)
                           (if (eq (aref chars 0) ?<) (- tab-width) tab-width))))
      (ygg-ex--goto-line end)
      (ygg-set-selection (point) (point)))))

;;; Command table + resolution

(defvar ygg-ex--commands
  '(("write"      . ygg-ex--cmd-write)
    ("quit"       . ygg-ex--cmd-quit)
    ("quitall"    . ygg-ex--cmd-quitall)
    ("wall"       . ygg-ex--cmd-wall)
    ("wqall"      . ygg-ex--cmd-wqall)
    ("xall"       . ygg-ex--cmd-wqall)
    ("zz"         . ygg-ex--cmd-recenter)
    ("nohlsearch" . ygg-ex--cmd-nohlsearch)
    ("wq"         . ygg-ex--cmd-wq)
    ("x"          . ygg-ex--cmd-wq)
    ("edit"       . ygg-ex--cmd-edit)
    ("bdelete"    . ygg-ex--cmd-bdelete)
    ("bnext"      . ygg-ex--cmd-bnext)
    ("bprevious"  . ygg-ex--cmd-bprevious)
    ("buffer"     . ygg-ex--cmd-buffer)
    ("split"      . ygg-ex--cmd-split)
    ("vsplit"     . ygg-ex--cmd-vsplit)
    ("new"        . ygg-ex--cmd-new)
    ("substitute" . ygg-ex--cmd-substitute)
    ("global"     . ygg-ex--cmd-global)
    ("read"       . ygg-ex--cmd-read)
    ("normal"     . ygg-ex--cmd-normal)
    ("move"       . ygg-ex--cmd-move)
    ("copy"       . ygg-ex--cmd-copy)
    ("vglobal"    . ygg-ex--cmd-vglobal)
    ("delete"     . ygg-ex--cmd-delete)
    ("yank"       . ygg-ex--cmd-yank)
    ("put"        . ygg-ex--cmd-put)
    ("join"       . ygg-ex--cmd-join)
    ("sort"       . ygg-ex--cmd-sort)
    ("rsort"      . ygg-ex--cmd-rsort)
    ("tabnew"     . ygg-ex--cmd-tabnew)
    ("tabedit"    . ygg-ex--cmd-tabnew)
    ("tabclose"   . ygg-ex--cmd-tabclose)
    ("tabonly"    . ygg-ex--cmd-tabonly)
    ("tabnext"    . ygg-ex--cmd-tabnext)
    ("tabprevious" . ygg-ex--cmd-tabprev))
  "Alist of full ex command name to handler symbol.
Handlers take (RANGE BANG ARGS).")

(defvar ygg-ex--abbrevs
  '(("w"  . "write")
    ("q"  . "quit")
    ("qa" . "quitall")
    ("wa" . "wall")
    ("wqa" . "wqall")
    ("xa" . "wqall")
    ("e"  . "edit")
    ("bd" . "bdelete")
    ("bn" . "bnext")
    ("bp" . "bprevious")
    ("b"  . "buffer")
    ("sp" . "split")
    ("vs" . "vsplit")
    ("s"  . "substitute")
    ("g"  . "global")
    ("v"  . "vglobal")
    ("r"  . "read")
    ("norm" . "normal")
    ("m"  . "move")
    ("mo" . "move")
    ("co" . "copy")
    ("t"  . "copy")
    ("d"  . "delete")
    ("del" . "delete")
    ("y"  . "yank")
    ("ya" . "yank")
    ("pu" . "put")
    ("j"  . "join")
    ("noh" . "nohlsearch")
    ("nohl" . "nohlsearch")
    ("tabe" . "tabedit")
    ("tabc" . "tabclose")
    ("tabo" . "tabonly")
    ("tabn" . "tabnext")
    ("tabp" . "tabprevious"))
  "Explicit abbreviations that resolve before unique-prefix matching.")

(defun ygg-ex--resolve (word)
  "Resolve WORD to a full command name, or nil if ambiguous/unknown."
  (cond
   ((assoc word ygg-ex--commands) word)
   ((cdr (assoc word ygg-ex--abbrevs)))
   (t (let ((matches (cl-remove-if-not
                       (lambda (c) (string-prefix-p word (car c)))
                       ygg-ex--commands)))
        (and (= (length matches) 1) (caar matches))))))

(defun ygg-ex--fallback (word)
  (let ((sym (intern-soft word)))
    (if (and sym (commandp sym))
        (command-execute sym)
      (user-error "yggdrasil: unknown command: %s" word))))

(defun ygg-ex--dispatch (range command-string)
  (if (string-match "\\`\\([a-zA-Z][a-zA-Z0-9-]*\\)\\(!?\\)\\([^z-a]*\\)\\'" command-string)
      (let* ((word (match-string 1 command-string))
             (bang (not (string-empty-p (match-string 2 command-string))))
             (args (string-trim-left (match-string 3 command-string)))
             (full (ygg-ex--resolve word)))
        (cond
         (full (funcall (cdr (assoc full ygg-ex--commands)) range bang args))
         ((ygg-ex--alias-steps word) (ygg-ex--run-alias word args))
         (t (ygg-ex--fallback word))))
    (ygg-ex--fallback command-string)))

;;; Command-line expansions — Helix's %{...} grammar

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function vc-root-dir "vc-hooks" ())

(defconst ygg-ex--expand-delimiters
  '((?\{ . ?\}) (?\[ . ?\]) (?\( . ?\)) (?< . ?>))
  "Open/close pairs an expansion's contents may be wrapped in.")

(defconst ygg-ex--expand-max-depth 8
  "How far a nested expansion may recurse before it is left as written.")

(defvar ygg-ex--expand-complained nil
  "Set once per `ygg-ex--expand' call after an unknown variable is named.")

(defun ygg-ex--workspace-directory ()
  "The project root this buffer sits in, or where it sits."
  (or (when-let* ((p (and (fboundp 'project-current) (project-current nil))))
        (expand-file-name (project-root p)))
      (and (fboundp 'vc-root-dir) (vc-root-dir))
      default-directory))

(defun ygg-ex--expand-variable (name)
  "The editor value NAME stands for, or nil when nothing does."
  (pcase name
    ("selection" (let ((b (ygg-selection-effective-bounds)))
                   (buffer-substring-no-properties (nth 0 b) (nth 1 b))))
    ("buffer_name" (if (buffer-file-name)
                       (file-relative-name (buffer-file-name)
                                           (ygg-ex--workspace-directory))
                     (buffer-name)))
    ("language" (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode)))
    ("cursor_line" (number-to-string (line-number-at-pos)))
    ("cursor_column" (number-to-string (1+ (current-column))))
    ("line_ending" "\n")
    ("workspace_directory" (ygg-ex--workspace-directory))
    (_ nil)))

(defun ygg-ex--expand-close (string open close from)
  "Position of the CLOSE matching the OPEN just before FROM in STRING.
Nil when the pair never closes."
  (let ((len (length string)) (level 1) (i from) (found nil))
    (while (and (null found) (< i len))
      (let ((c (aref string i)))
        (cond
         ((and (eq c ?\\) (< (1+ i) len)) (setq i (1+ i)))
         ((eq c open) (setq level (1+ level)))
         ((eq c close) (setq level (1- level))
          (when (zerop level) (setq found i)))))
      (setq i (1+ i)))
    found))

(defun ygg-ex--expand-one (kind body raw depth)
  "The text expansion KIND over BODY stands for, or RAW when nothing does.
DEPTH is the nesting already spent."
  (let ((inner (ygg-ex--expand-1 body nil depth)))
    (pcase kind
      ("" (or (ygg-ex--expand-variable inner)
              (progn (unless ygg-ex--expand-complained
                       (setq ygg-ex--expand-complained t)
                       (message "yggdrasil: no such variable: %s" inner))
                     raw)))
      ("sh" (string-trim-right (shell-command-to-string inner) "\n"))
      ("reg" (if (zerop (length inner))
                 raw
               (let ((v (get-register (aref inner 0))))
                 (if (stringp v) v ""))))
      (_ raw))))

(defun ygg-ex--expand-1 (string quoted depth)
  "Expand STRING once.  QUOTED honours single quotes; DEPTH caps recursion."
  (if (>= depth ygg-ex--expand-max-depth)
      string
    (let ((len (length string)) (i 0) (out nil))
      (while (< i len)
        (let ((c (aref string i)))
          (cond
           ((and (eq c ?\\) (< (1+ i) len) (eq (aref string (1+ i)) ?%))
            (push "%" out) (setq i (+ i 2)))
           ((and quoted (eq c ?'))
            (let ((end (or (cl-position ?' string :start (1+ i)) (1- len))))
              (push (substring string i (1+ end)) out)
              (setq i (1+ end))))
           ((eq c ?%)
            (let* ((k i)
                   (kind-end (progn (setq k (1+ i))
                                    (while (and (< k len)
                                                (let ((ch (aref string k)))
                                                  (or (and (>= ch ?a) (<= ch ?z))
                                                      (and (>= ch ?A) (<= ch ?Z))
                                                      (eq ch ?_))))
                                      (setq k (1+ k)))
                                    k))
                   (pair (and (< kind-end len)
                              (assq (aref string kind-end)
                                    ygg-ex--expand-delimiters)))
                   (close (and pair (ygg-ex--expand-close
                                     string (car pair) (cdr pair) (1+ kind-end)))))
              (if (null close)
                  (progn (push "%" out) (setq i (1+ i)))
                (push (ygg-ex--expand-one
                       (substring string (1+ i) kind-end)
                       (substring string (1+ kind-end) close)
                       (substring string i (1+ close))
                       (1+ depth))
                      out)
                (setq i (1+ close)))))
           (t (push (string c) out) (setq i (1+ i))))))
      (apply #'concat (nreverse out)))))

(defun ygg-ex--expand (string)
  "STRING with Helix's command-line expansions replaced by their values.
`%{name}' is an editor variable, `%sh{cmd}' the output of a shell
command, `%reg{c}' a register; `[]', `()' and `<>' delimit as `{}'
does, `\\%' is a literal percent, and single quotes hold their
contents as written."
  (if (not (string-match-p "%" string))
      string
    (let ((ygg-ex--expand-complained nil))
      (ygg-ex--expand-1 string t 0))))

;;; Shell escapes — ! streams into a float, !! runs headless

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function ygg-space-dir "yggdrasil-spacetree" (&optional tab))

(defvar ygg-ex--float-buffer nil
  "The buffer the command float stands on now, or nil when none does.")

(defvar ygg-ex--float-exit nil
  "What ends the float's transient q binding, once one is in place.")

(defun ygg-ex--float-p ()
  "Whether a command float is a child frame rather than a side window."
  (and (display-graphic-p) (fboundp 'posframe-show)))

(defun ygg-ex--face-background (face)
  "The background color of FACE as a string, or nil when it has none."
  (and (facep face)
       (let ((bg (face-attribute face :background nil t)))
         (and (stringp bg) bg))))

(defun ygg-ex-float-close ()
  "Take the command float down; its buffer stays for M-x switch."
  (interactive)
  (remove-hook 'pre-command-hook #'ygg-ex-float-close)
  (when ygg-ex--float-exit
    (funcall ygg-ex--float-exit)
    (setq ygg-ex--float-exit nil))
  (when-let* ((buf ygg-ex--float-buffer))
    (setq ygg-ex--float-buffer nil)
    (when (buffer-live-p buf)
      (if (ygg-ex--float-p)
          (ignore-errors (posframe-hide buf))
        (when-let* ((win (get-buffer-window buf)))
          (ignore-errors (delete-window win)))))))

(defun ygg-ex--float-geometry (window)
  "Where and how large a float over WINDOW's lower half stands."
  (pcase-let ((`(,left ,top ,_right ,bottom) (window-pixel-edges window)))
    (list (cons left (+ top (/ (- bottom top) 2)))
          (window-body-width window)
          (max 1 (/ (window-body-height window) 2)))))

(defun ygg-ex--float-open (buf)
  "Float BUF over the selected window's lower half and bind q to close it.
A graphic frame that can load posframe gets a child frame; a terminal
gets a bottom side window instead.  Point stays where it was, so the
output arrives beside the work rather than in place of it."
  (unless (eq buf ygg-ex--float-buffer)
    (ygg-ex-float-close))
  (setq ygg-ex--float-buffer buf)
  (if (ygg-ex--float-p)
      (pcase-let ((`(,position ,width ,height)
                   (ygg-ex--float-geometry (selected-window))))
        (posframe-show buf
                       :position position
                       :width width
                       :height height
                       :background-color (or (ygg-ex--face-background 'ygg-float)
                                             (ygg-ex--face-background 'default))
                       :border-width 1
                       :border-color (ygg-ex--face-background 'child-frame-border)
                       :respect-mode-line t
                       :accept-focus nil))
    (display-buffer buf '((display-buffer-at-bottom)
                          (window-height . 0.4)
                          (window-parameters . ((no-other-window . t))))))
  (setq ygg-ex--float-exit
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (define-key map "q" #'ygg-ex-float-close)
           map)
         (lambda () (buffer-live-p ygg-ex--float-buffer)))))

(defun ygg-ex--shell-buffer (command)
  "A cleared buffer named for COMMAND, sitting in this directory."
  (let ((buf (get-buffer-create (format "*ex: %s*" command)))
        (dir default-directory))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq default-directory dir))
    buf))

(defun ygg-ex--shell-float (command)
  "Run COMMAND through the shell here, streaming it into a float.
The float goes down on q, or on the first key after COMMAND ends."
  (let ((buf (ygg-ex--shell-buffer command)))
    (ygg-ex--float-open buf)
    (make-process
     :name "ygg-ex-shell" :buffer buf :noquery t
     :command (list shell-file-name shell-command-switch command)
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (add-hook 'pre-command-hook #'ygg-ex-float-close))))))

(defun ygg-ex--last-lines (buffer count)
  "The last COUNT lines of BUFFER's text."
  (with-current-buffer buffer
    (save-excursion
      (let ((end (point-max)))
        (goto-char end)
        (forward-line (- count))
        (buffer-substring-no-properties (point) end)))))

(defconst ygg-ex--shell-failure-lines 20
  "How many of a headless command's last stderr lines a failure shows.")

(defun ygg-ex--shell-failure (command errors)
  "Float COMMAND's last stderr lines, held in the ERRORS buffer."
  (let ((text (ygg-ex--last-lines errors ygg-ex--shell-failure-lines))
        (buf (ygg-ex--shell-buffer command)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (insert text)))
    (ygg-ex--float-open buf)
    (add-hook 'pre-command-hook #'ygg-ex-float-close)
    buf))

(defun ygg-ex--shell-quiet (command)
  "Run COMMAND through the shell here with nothing on screen while it runs.
A clean exit says so in the echo area; a failure floats the tail of
what COMMAND wrote to stderr."
  (let* ((errors (generate-new-buffer " *ex stderr*"))
         (stderr (make-pipe-process :name "ygg-ex-stderr" :buffer errors
                                    :noquery t :sentinel #'ignore)))
    (make-process
     :name "ygg-ex-quiet" :buffer nil :noquery t
     :command (list shell-file-name shell-command-switch command)
     :stderr stderr
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (while (accept-process-output stderr 0.05))
         (if (zerop (process-exit-status proc))
             (message "yggdrasil: %s done" command)
           (ygg-ex--shell-failure command errors))
         (delete-process stderr)
         (when (buffer-live-p errors) (kill-buffer errors)))))))

;;; Aliases — a name for a list of ex lines, taking $1 to $9

(defcustom ygg-ex-aliases nil
  "Alist of alias name to the list of ex lines it stands for.
Each step is an ex line, run in order, stopping at the first error; a
step may be a shell escape.  $1 to $9 in a step come from the words
after the alias name, ${1:-text} fills a missing one in, and the
editor vocabulary expands in every step."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'yggdrasil)

(defconst ygg-ex--alias-variables
  '("FILE" "FILE_DIR" "FILE_NAME" "FILE_STEM" "FILE_EXT" "LINE" "COLUMN"
    "WORD" "SELECTION" "WORKSPACE" "WORKSPACE_HASH")
  "The variable vocabulary a step may name with a dollar sign.")

(defun ygg-ex--alias-workspace ()
  "The space's directory, or the project root the buffer sits in."
  (or (and (fboundp 'ygg-space-dir) (ygg-space-dir))
      (ygg-ex--workspace-directory)))

(defun ygg-ex--alias-variable (name)
  "The text NAME stands for, or nil when the vocabulary has no NAME."
  (let ((file (or (buffer-file-name) "")))
    (pcase name
      ("FILE" file)
      ("FILE_DIR" (if (string-empty-p file) "" (file-name-directory file)))
      ("FILE_NAME" (file-name-nondirectory file))
      ("FILE_STEM" (file-name-base file))
      ("FILE_EXT" (or (file-name-extension file) ""))
      ("LINE" (number-to-string (line-number-at-pos)))
      ("COLUMN" (number-to-string (1+ (current-column))))
      ("WORD" (or (thing-at-point 'symbol t) ""))
      ("SELECTION" (ygg-ex--expand-variable "selection"))
      ("WORKSPACE" (ygg-ex--alias-workspace))
      ("WORKSPACE_HASH" (substring (sha1 (ygg-ex--alias-workspace)) 0 12))
      (_ nil))))

(defun ygg-ex--alias-positional (alias words body)
  "What the positional reference BODY stands for under ALIAS.
BODY is a digit, optionally followed by a POSIX operator; a reference
with nothing to stand for and no default is an error naming ALIAS."
  (let* ((index (string-to-number (substring body 0 1)))
         (operator (substring body 1))
         (value (nth (1- index) words))
         (given (and value (not (string-empty-p value)))))
    (cond
     (given value)
     ((string-prefix-p ":-" operator) (substring operator 2))
     (t (user-error "yggdrasil: alias %s wants $%d" alias index)))))

(defun ygg-ex--alias-braced (alias words body)
  "What ${BODY} stands for in ALIAS's step, given its WORDS."
  (if (string-match-p "\\`[1-9]" body)
      (ygg-ex--alias-positional alias words body)
    (or (ygg-ex--alias-variable body)
        (user-error "yggdrasil: alias %s names no such variable: %s"
                    alias body))))

(defun ygg-ex--alias-step (alias step words)
  "STEP with its dollar references filled in from WORDS, under ALIAS."
  (let ((len (length step)) (i 0) (out nil))
    (while (< i len)
      (let ((c (aref step i)))
        (cond
         ((and (eq c ?$) (< (1+ i) len) (eq (aref step (1+ i)) ?\{))
          (let ((close (ygg-ex--expand-close step ?\{ ?\} (+ i 2))))
            (if (null close)
                (progn (push "$" out) (setq i (1+ i)))
              (push (ygg-ex--alias-braced alias words
                                          (substring step (+ i 2) close))
                    out)
              (setq i (1+ close)))))
         ((and (eq c ?$) (< (1+ i) len) (<= ?1 (aref step (1+ i)) ?9))
          (push (ygg-ex--alias-positional
                 alias words (substring step (1+ i) (+ i 2)))
                out)
          (setq i (+ i 2)))
         ((eq c ?$)
          (let ((k (1+ i)))
            (while (and (< k len)
                        (let ((ch (aref step k)))
                          (or (<= ?A ch ?Z) (eq ch ?_))))
              (setq k (1+ k)))
            (let ((value (and (> k (1+ i))
                              (ygg-ex--alias-variable (substring step (1+ i) k)))))
              (if value
                  (progn (push value out) (setq i k))
                (push "$" out) (setq i (1+ i))))))
         (t (push (string c) out) (setq i (1+ i))))))
    (apply #'concat (nreverse out))))

(defun ygg-ex--aliases ()
  "The alias table, checked as it is read.
An alias whose name an ex command already answers to is a dispatch
error: the built-in wins, so the alias could never be reached."
  (dolist (alias ygg-ex-aliases)
    (when (or (assoc (car alias) ygg-ex--commands)
              (assoc (car alias) ygg-ex--abbrevs))
      (user-error "yggdrasil: alias %s is already an ex command" (car alias))))
  ygg-ex-aliases)

(defun ygg-ex--alias-steps (name)
  "The steps alias NAME stands for, or nil when no alias goes by NAME."
  (cdr (assoc name (ygg-ex--aliases))))

(defun ygg-ex--alias-names ()
  "The names the alias table offers the prompt, or none when it is bad."
  (condition-case nil (mapcar #'car (ygg-ex--aliases)) (error nil)))

(defun ygg-ex--run-alias (name args)
  "Run the steps of alias NAME with ARGS split into its positionals."
  (let ((words (split-string args " " t))
        (steps (ygg-ex--alias-steps name)))
    (dolist (step steps)
      (ygg-ex--execute (ygg-ex--alias-step name step words)))
    t))

(defun ygg-ex--run (range rest)
  "Run REST (a range-stripped ex command) against RANGE."
  (let ((rest (ygg-ex--expand (string-trim-left rest))))
    (cond
     ((string-prefix-p "!!" rest)
      (ygg-ex--shell-quiet (string-trim (substring rest 2))))
     ((string-prefix-p "!" rest)
      (ygg-ex--shell-float (string-trim (substring rest 1))))
     ((string-empty-p rest) (when range (ygg-ex--goto-line (cdr range))))
     ((string-match "\\`\\([<>]+\\)[ \t]*\\([^z-a]*\\)\\'" rest)
      (ygg-ex--cmd-shift range (match-string 1 rest) (match-string 2 rest)))
     (t (ygg-ex--dispatch range rest)))))

(defun ygg-ex--execute (input)
  "Parse and run the ex command line INPUT."
  (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range input)))
    (ygg-ex--run range rest)))

;;; Leader commands — `:' is the other way into everything the leader can
;;; reach, since a command you can only get to by chord is one you cannot
;;; search for

(defvar ygg-leader-map)
(declare-function ygg-localleader--resolve "yggdrasil-localleader" (&optional _))

(defun ygg-ex--walk-map (map keys found)
  "Collect (NAME KEY LABEL) for every named command in MAP into FOUND's car.
An anonymous command is skipped: with no name there is nothing to type."
  (map-keymap
   (lambda (event def)
     (let* ((key (vconcat keys (vector event)))
            (label (and (consp def) (stringp (car def)) (car def)))
            (def (if label (cdr def) def)))
       (cond
        ((keymapp def) (ygg-ex--walk-map def key found))
        ((and (symbolp def) (commandp def))
         (push (list (symbol-name def) (key-description key) label) (car found))))))
   map)
  found)

(defun ygg-ex--leader-commands ()
  "Named commands under the leader and this buffer's localleader."
  (let ((found (list nil)))
    (when (boundp 'ygg-leader-map)
      (ygg-ex--walk-map ygg-leader-map (kbd "SPC") found))
    (when-let* (((fboundp 'ygg-localleader--resolve))
                (map (ygg-localleader--resolve)))
      (ygg-ex--walk-map map (kbd "\\") found))
    (nreverse (car found))))

;;; Completion + entry point

(defun ygg-ex--split-prefix (string)
  (if (string-match "\\`[0-9%.$'<>,+-]*" string)
      (cons (match-string 0 string) (substring string (match-end 0)))
    (cons "" string)))

(defvar ygg-ex--names
  (append (mapcar #'car ygg-ex--commands) (mapcar #'car ygg-ex--abbrevs))
  "Names the `:' prompt offers; `ygg-ex' widens it to the leader's for each read.")

(defvar ygg-ex--annotations nil
  "Name to (KEY LABEL), for what the `:' prompt is showing beside a leader command.")

(defun ygg-ex--every-command ()
  "The name of every interactive command, as the palette offers them."
  (let (names)
    (mapatoms (lambda (symbol)
                (when (and (commandp symbol) (not (get symbol 'byte-obsolete-info)))
                  (push (symbol-name symbol) names))))
    names))

(defun ygg-ex--candidates ()
  "The prompt's names and their annotations.
Ex commands first, the leader's commands after them with their keys,
then every other interactive command: the one line is the palette too,
so what M-x would run, this runs."
  (let* ((base (append (mapcar #'car ygg-ex--commands) (mapcar #'car ygg-ex--abbrevs)))
         (seen (make-hash-table :test #'equal))
         (extra nil) (annotations nil))
    (dolist (name base) (puthash name t seen))
    (dolist (name (ygg-ex--alias-names))
      (unless (gethash name seen)
        (puthash name t seen)
        (push name extra)
        (push (list name "alias") annotations)))
    (dolist (entry (ygg-ex--leader-commands))
      (unless (gethash (car entry) seen)
        (puthash (car entry) t seen)
        (push (car entry) extra)
        (push (cons (car entry) (cdr entry)) annotations)))
    (dolist (name (ygg-ex--every-command))
      (unless (gethash name seen)
        (puthash name t seen)
        (push name extra)))
    (list (append base (nreverse extra)) (nreverse annotations))))

(defun ygg-ex--annotate (name)
  (when-let* ((info (cdr (assoc name ygg-ex--annotations))))
    (propertize (concat "  " (car info)
                        (and (cadr info) (concat "  " (cadr info))))
                'face 'shadow)))

(defun ygg-ex--completion-table (string pred action)
  "Completion table that only completes the command-name word.
Any leading range prefix and any text after the first space pass through
untouched."
  (pcase-let* ((`(,prefix . ,tail) (ygg-ex--split-prefix string))
               (space (string-match "[ \t]" tail))
               (word (if space (substring tail 0 space) tail)))
    (pcase action
      ('nil (if space string
              (let ((m (try-completion word ygg-ex--names pred)))
                (cond ((eq m t) string)
                      ((null m) nil)
                      (t (concat prefix m))))))
      ('t (if space nil (all-completions word ygg-ex--names pred)))
      ('lambda (if space t (test-completion word ygg-ex--names pred)))
      (`(boundaries . ,suffix)
       `(boundaries ,(length prefix)
                    . ,(or (string-match "[ \t]" suffix) (length suffix))))
      ('metadata '(metadata (annotation-function . ygg-ex--annotate)))
      (_ nil))))

(defvar ygg-ex--minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map " " #'self-insert-command)
    (define-key map "?" #'self-insert-command)
    map)
  "Completion map where SPC/? type literally (\":w file.txt\" stays \"w\").")

;;;###autoload
(defun ygg-ex ()
  "Read an ex command line from the minibuffer and execute it."
  (interactive)
  (let* ((candidates (ygg-ex--candidates))
         (ygg-ex--names (car candidates))
         (ygg-ex--annotations (cadr candidates))
         (minibuffer-local-completion-map ygg-ex--minibuffer-map)
         (input (completing-read ":" #'ygg-ex--completion-table nil nil)))
    (ygg-ex--execute input)))

(provide 'yggdrasil-ex)
;;; yggdrasil-ex.el ends here
