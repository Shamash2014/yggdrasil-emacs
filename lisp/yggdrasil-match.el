;;; yggdrasil-match.el --- Helix match mode: pairs, textobjects, surround -*- lexical-binding: t; -*-

;; Built-ins wrapped: forward-sexp/backward-sexp, syntax-ppss, scan-lists,
;; thingatpt, treesit.
;; Custom: match prefix self-promotion, surround insert/delete/replace.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-selection)
(require 'cl-lib)
(require 'thingatpt)
(require 'treesit nil t)

(defvar ygg-match-map (make-sparse-keymap) "The m (match) prefix keymap.")

(defconst ygg-match--bracket-pairs
  `((?\( . (?\( . ?\))) (?\) . (?\( . ?\))) (?b . (?\( . ?\)))
    (?\{ . (?\{ . ?\})) (?\} . (?\{ . ?\})) (?B . (?\{ . ?\}))
    (?\[ . (?\[ . ?\])) (?\] . (?\[ . ?\]))
    (?< . (?< . ?>)) (?> . (?< . ?>))))

(defconst ygg-match--quote-chars '(?\" ?\' ?\`))

;;; mm — jump to matching bracket

(defun ygg-match--scan-line-for-bracket ()
  (let* ((lb (line-beginning-position)) (le (line-end-position))
         (fwd (save-excursion
                (while (and (< (point) le)
                            (not (memq (char-syntax (char-after)) '(?\( ?\)))))
                  (forward-char 1))
                (and (< (point) le) (point))))
         (bwd (save-excursion
                (while (and (> (point) lb)
                            (not (memq (char-syntax (char-before)) '(?\( ?\)))))
                  (backward-char 1))
                (and (> (point) lb) (1- (point))))))
    (cond ((and fwd bwd) (if (<= (- fwd (point)) (- (point) bwd)) fwd bwd))
          (fwd fwd)
          (bwd bwd))))

(defun ygg-match--bracket-bounds ()
  "Return (OPEN CLOSE FORWARD) around point, CLOSE one past the delimiter."
  (condition-case nil
      (let ((after (char-after)) (before (char-before)))
        (cond
         ((and after (eq (char-syntax after) ?\())
          (let ((beg (point))) (forward-sexp 1) (list beg (point) t)))
         ((and after (eq (char-syntax after) ?\)))
          (forward-char 1)
          (let ((end (point))) (backward-sexp 1) (list (point) end nil)))
         ((and before (eq (char-syntax before) ?\)))
          (let ((end (point))) (backward-sexp 1) (list (point) end nil)))
         (t (let ((pos (ygg-match--scan-line-for-bracket)))
              (when pos (goto-char pos) (ygg-match--bracket-bounds))))))
    (scan-error (message "yggdrasil: unbalanced brackets") nil)))

(defun ygg-match-jump ()
  "Select from the bracket at/near point to its match, inclusive."
  (interactive)
  (ygg-each-selection-update
   (lambda (anchor cursor _dir)
     (pcase (ygg-match--bracket-bounds)
       (`(,beg ,end ,fwd) (if fwd (cons beg end) (cons end beg)))
       (_ (cons anchor cursor))))))

;;; Enclosing-pair finders shared by textobjects and surround ops

(defun ygg-match--find-enclosing-syntax (open)
  ;; syntax-ppss hasn't "entered" a pair whose opener is AT point — check it
  (let ((found (if (eq (char-after) open)
                   (point)
                 (cl-find-if (lambda (pos) (eq (char-after pos) open))
                             (reverse (nth 9 (syntax-ppss)))))))
    (when found
      (condition-case nil
          (save-excursion (goto-char found) (forward-sexp 1) (cons found (point)))
        (scan-error nil)))))

(defun ygg-match--find-enclosing-charscan (open close)
  (save-excursion
    (let ((start (point)) (depth 0) beg)
      (when (eq (char-after) open)
        (setq beg (point)))
      (while (and (not beg) (> (point) (point-min)))
        (backward-char 1)
        (cond ((eq (char-after) close) (setq depth (1+ depth)))
              ((eq (char-after) open)
               (if (zerop depth) (setq beg (point)) (setq depth (1- depth))))))
      (when beg
        (goto-char start)
        (let ((depth2 0) end)
          (while (and (not end) (< (point) (point-max)))
            (cond ((eq (char-after) open) (setq depth2 (1+ depth2)))
                  ((eq (char-after) close)
                   (if (zerop depth2) (setq end (1+ (point))) (setq depth2 (1- depth2)))))
            (unless end (forward-char 1)))
          (when end (cons beg end)))))))

(defun ygg-match--find-enclosing-quote (qc)
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (let ((beg (nth 8 ppss)))
        (condition-case nil
            (save-excursion (goto-char beg) (forward-sexp 1) (cons beg (point)))
          (scan-error nil))))
     ((eq (char-after) qc)
      (let ((fwd (save-excursion
                   (forward-char 1)
                   (and (search-forward (char-to-string qc)
                                        (line-end-position) t)
                        (point)))))
        (when fwd (cons (point) fwd))))
     (t
      (let* ((lb (line-beginning-position)) (le (line-end-position))
             (bwd (save-excursion (and (search-backward (char-to-string qc) lb t) (point))))
             (fwd (save-excursion (and (search-forward (char-to-string qc) le t) (1- (point))))))
        (when (and bwd fwd (< bwd fwd)) (cons bwd (1+ fwd))))))))

;;; Textobjects (mi / ma)

(defun ygg-match--bounds-of-WORD ()
  (let (beg end)
    (save-excursion (skip-chars-forward "^ \t\n") (setq end (point)))
    (save-excursion (skip-chars-backward "^ \t\n") (setq beg (point)))
    (cons beg end)))

(defun ygg-match--around-pad (bounds)
  (pcase-let ((`(,beg . ,end) bounds))
    (save-excursion
      (goto-char end)
      (let ((e2 (progn (skip-chars-forward " \t") (point))))
        (if (> e2 end)
            (cons beg e2)
          (goto-char beg)
          (cons (progn (skip-chars-backward " \t") (point)) end))))))

(defun ygg-match--bracket-textobj-bounds (c which)
  (pcase-let ((`(,open . ,close) (cdr (assq c ygg-match--bracket-pairs))))
    (let ((bounds (if (memq c '(?< ?>))
                       (ygg-match--find-enclosing-charscan open close)
                     (ygg-match--find-enclosing-syntax open))))
      (when bounds
        (if (eq which 'around) bounds (cons (1+ (car bounds)) (1- (cdr bounds))))))))

(defun ygg-match--quote-textobj-bounds (qc which)
  (let ((bounds (ygg-match--find-enclosing-quote qc)))
    (when bounds
      (if (eq which 'around) bounds (cons (1+ (car bounds)) (1- (cdr bounds)))))))

(defun ygg-match--thing-bounds (thing which)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds (if (eq which 'around) (ygg-match--around-pad bounds) bounds))))

(defun ygg-match--paragraph-bounds (which)
  "Vim ip/ap: ip is the contiguous non-blank block around point (or the
blank-line run, if point sits in one); ap adds the block's trailing
blank lines, or its leading ones when there is no trailing run."
  (let (beg end)
    (save-excursion
      (goto-char (line-beginning-position))
      (if (looking-at-p "[ \t]*$")
          (progn
            (while (and (not (bobp))
                        (save-excursion (forward-line -1) (looking-at-p "[ \t]*$")))
              (forward-line -1))
            (setq beg (point))
            (while (and (not (eobp)) (looking-at-p "[ \t]*$")) (forward-line 1))
            (setq end (point)))
        (while (and (not (bobp))
                    (save-excursion (forward-line -1) (not (looking-at-p "[ \t]*$"))))
          (forward-line -1))
        (setq beg (point))
        (while (and (not (eobp)) (not (looking-at-p "[ \t]*$"))) (forward-line 1))
        (setq end (point))))
    (if (eq which 'inside)
        (cons beg end)
      (let ((e2 (save-excursion
                  (goto-char end)
                  (while (and (not (eobp)) (looking-at-p "[ \t]*$")) (forward-line 1))
                  (point))))
        (if (> e2 end)
            (cons beg e2)
          (cons (save-excursion
                  (goto-char beg)
                  (while (and (not (bobp))
                              (save-excursion (forward-line -1) (looking-at-p "[ \t]*$")))
                    (forward-line -1))
                  (point))
                end))))))

(defun ygg-match--enclosing-any-paren ()
  "Innermost enclosing (), [], or {} pair around point; a raw character
scan, so it works regardless of the buffer's syntax table."
  (let (best)
    (dolist (pair '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))
      (let ((b (ygg-match--find-enclosing-charscan (car pair) (cdr pair))))
        (when (and b (or (null best) (> (car b) (car best)))) (setq best b))))
    best))

(defun ygg-match--comma-segments (beg end)
  "Top-level comma-split segments of BEG..END as ((SEG-BEG . SEG-END)...);
a segment's raw END lands on its trailing comma, except the last."
  (save-excursion
    (goto-char beg)
    (let ((segs nil) (seg-start beg) (depth 0))
      (while (< (point) end)
        (cond
         ((memq (char-after) '(?\( ?\[ ?\{)) (setq depth (1+ depth)) (forward-char 1))
         ((memq (char-after) '(?\) ?\] ?\})) (setq depth (1- depth)) (forward-char 1))
         ((memq (char-after) '(?\" ?\'))
          (condition-case nil (forward-sexp 1) (scan-error (forward-char 1))))
         ((and (zerop depth) (eq (char-after) ?,))
          (push (cons seg-start (point)) segs)
          (forward-char 1)
          (setq seg-start (point)))
         (t (forward-char 1))))
      (push (cons seg-start end) segs)
      (nreverse segs))))

(defun ygg-match--argument-fallback-bounds (which)
  "Comma-split argument object: nearest enclosing paren, split at
top-level commas; around includes one adjacent comma (trailing
preferred, else leading)."
  (let ((enc (ygg-match--enclosing-any-paren)))
    (when enc
      (let ((ibeg (1+ (car enc))) (iend (1- (cdr enc))))
        (when (<= ibeg iend)
          (let* ((segs (ygg-match--comma-segments ibeg iend))
                 (pt (point))
                 (idx (or (cl-position-if
                           (lambda (s) (and (>= pt (car s)) (<= pt (cdr s))))
                           segs)
                          (1- (length segs))))
                 (seg (nth idx segs))
                 (tb (save-excursion (goto-char (car seg)) (skip-chars-forward " \t\n") (point)))
                 (te (max tb (save-excursion
                               (goto-char (cdr seg)) (skip-chars-backward " \t\n") (point)))))
            (if (eq which 'inside)
                (cons tb te)
              (cond
               ((< idx (1- (length segs))) (cons tb (1+ (cdr seg))))
               ((> idx 0) (cons (cdr (nth (1- idx) segs)) te))
               (t (cons tb te))))))))))

(defun ygg-match--argument-treesit-bounds (which)
  "inside = the argument/parameter node; around adds one adjacent separator
comma (trailing preferred, else leading). Matches only per-argument wrapper
nodes (TS `required_parameter', Rust `parameter' &c.), NOT the plural
container (`arguments'/`parameters') — bare call args have no wrapper node,
so this returns nil and the comma-split fallback takes over."
  (when (and (fboundp 'treesit-node-at) (treesit-parser-list))
    (let ((node (treesit-node-at (point)))
          (re "\\(?:\\`\\|_\\)\\(?:argument\\|parameter\\)\\'"))
      (while (and node (not (string-match-p re (treesit-node-type node))))
        (setq node (treesit-node-parent node)))
      (when node
        (let ((ibeg (treesit-node-start node))
              (iend (treesit-node-end node)))
          (if (eq which 'inside)
              (cons ibeg iend)
            (or (save-excursion            ; trailing ", " up to the next arg
                  (goto-char iend)
                  (skip-chars-forward " \t\n")
                  (when (eq (char-after) ?,)
                    (forward-char 1) (skip-chars-forward " \t\n")
                    (cons ibeg (point))))
                (save-excursion            ; else a leading " ,"
                  (goto-char ibeg)
                  (skip-chars-backward " \t\n")
                  (when (eq (char-before) ?,)
                    (backward-char 1) (skip-chars-backward " \t\n")
                    (cons (point) iend)))
                (cons ibeg iend))))))))

(defun ygg-match--comment-treesit-bounds (which)
  (when (and (fboundp 'treesit-node-at) (treesit-parser-list))
    (let ((node (treesit-node-at (point))))
      (while (and node (not (string-match-p "comment" (treesit-node-type node))))
        (setq node (treesit-node-parent node)))
      (when node
        (let ((b (cons (treesit-node-start node) (treesit-node-end node))))
          (if (eq which 'around) (ygg-match--around-pad b) b))))))

(defun ygg-match--comment-bounds (which)
  "Treesit comment node when parsed, else `bounds-of-thing-at-point'
on the comment thing if this Emacs defines it; nil otherwise (never errors)."
  (or (ygg-match--comment-treesit-bounds which)
      (ignore-errors (ygg-match--thing-bounds 'comment which))))

(defun ygg-match--indent-run (cur-indent forward)
  "Position at the far edge of the same-or-deeper-indent run containing
point, scanning FORWARD or backward (evil-indent-plus semantics)."
  (save-excursion
    (while (and (if forward (not (eobp)) (not (bobp)))
                (save-excursion
                  (forward-line (if forward 1 -1))
                  (or (looking-at-p "[ \t]*$") (>= (current-indentation) cur-indent))))
      (forward-line (if forward 1 -1)))
    (point)))

(defun ygg-match--indent-bounds (which)
  "evil-indent-plus: `ii' is the run of lines at or deeper than point's
indentation (blank lines pass through); `ai' adds the header line above."
  (save-excursion
    (back-to-indentation)
    (let* ((cur-indent (current-indentation))
           (beg (progn (goto-char (ygg-match--indent-run cur-indent nil)) (line-beginning-position)))
           (end (progn (goto-char (ygg-match--indent-run cur-indent t)) (line-end-position)))
           (end1 (min (point-max) (1+ end))))
      (if (eq which 'inside)
          (cons beg end1)
        (cons (save-excursion
                (goto-char beg)
                (if (bobp) beg (progn (forward-line -1) (line-beginning-position))))
              end1)))))

;;; Tag textobject (vim it / at) — HTML/XML/JSX element pairs

(declare-function sgml-get-context "sgml-mode")
(declare-function sgml-skip-tag-forward "sgml-mode")
(declare-function sgml-tag-start "sgml-mode")
(declare-function sgml-tag-end "sgml-mode")

(defconst ygg-match--tag-node-types
  '("element" "jsx_element" "jsx_self_closing_element")
  "Treesit node types that count as a tag element (HTML, JSX/TSX).")

(defun ygg-match--tag-treesit-bounds (which)
  "Tag bounds from the enclosing treesit element, or nil when unparsed."
  (when (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (let ((node (treesit-node-at (point))))
      (while (and node (not (member (treesit-node-type node)
                                    ygg-match--tag-node-types)))
        (setq node (treesit-node-parent node)))
      (when node
        (if (eq which 'around)
            (cons (treesit-node-start node) (treesit-node-end node))
          (let* ((kids (treesit-node-children node))
                 (open (seq-find (lambda (n) (member (treesit-node-type n)
                                                     '("start_tag" "jsx_opening_element")))
                                 kids))
                 (close (seq-find (lambda (n) (member (treesit-node-type n)
                                                      '("end_tag" "jsx_closing_element")))
                                  kids)))
            ;; self-closing / malformed: inside collapses to the element span
            (if (and open close)
                (cons (treesit-node-end open) (treesit-node-start close))
              (cons (treesit-node-start node) (treesit-node-end node)))))))))

(defun ygg-match--tag-sgml-bounds (which)
  "Tag bounds via sgml-mode's tag stack, for non-treesit HTML/XML buffers."
  (when (fboundp 'sgml-get-context)
    (ignore-errors
      (require 'sgml-mode)
      (save-excursion
        (when-let* ((tag (car (last (sgml-get-context)))))
          (let ((astart (sgml-tag-start tag))
                (iopen (sgml-tag-end tag)))
            (goto-char astart)
            (sgml-skip-tag-forward 1)
            (let ((aend (point)))
              (if (eq which 'around)
                  (cons astart aend)
                (cons iopen (or (and (search-backward "</" iopen t) (point))
                                aend))))))))))

(defun ygg-match--tag-bounds (which)
  "Bounds of the enclosing HTML/XML/JSX tag pair; WHICH is `inside'/`around'."
  (or (ygg-match--tag-treesit-bounds which)
      (ygg-match--tag-sgml-bounds which)))

(defun ygg-match--textobject-bounds (c which)
  (cond
   ((memq c ygg-match--quote-chars) (ygg-match--quote-textobj-bounds c which))
   ((assq c ygg-match--bracket-pairs) (ygg-match--bracket-textobj-bounds c which))
   ((eq c ?w) (ygg-match--thing-bounds 'word which))
   ((eq c ?p) (ygg-match--paragraph-bounds which))
   ((eq c ?W) (let ((b (ygg-match--bounds-of-WORD)))
                (if (eq which 'around) (ygg-match--around-pad b) b)))
   ((eq c ?s) (ygg-match--thing-bounds 'sentence which))
   ((eq c ?e) (cons (point-min) (point-max)))
   ((eq c ?a) (or (ygg-match--argument-treesit-bounds which)
                  (ygg-match--argument-fallback-bounds which)))
   ((eq c ?c) (ygg-match--comment-bounds which))
   ((eq c ?i) (ygg-match--indent-bounds which))
   ((eq c ?t) (ygg-match--tag-bounds which))))

(defun ygg-match--apply-textobject (c which)
  (ygg-each-selection-update
   (lambda (anchor cursor _dir)
     (let ((bounds (ygg-match--textobject-bounds c which)))
       (if bounds (cons (car bounds) (cdr bounds)) (cons anchor cursor))))))

(defun ygg-match-inside (c)
  "Select inside the pair/thing for C."
  (interactive (list (read-char)))
  (ygg-match--apply-textobject c 'inside))

(defun ygg-match-around (c)
  "Select around the pair/thing for C, delimiters included."
  (interactive (list (read-char)))
  (ygg-match--apply-textobject c 'around))

;;; Surround (ms / md / mr)

(defun ygg-match--surround-pair (c)
  (cond
   ((assq c ygg-match--bracket-pairs)
    (let ((p (cdr (assq c ygg-match--bracket-pairs))))
      (cons (char-to-string (car p)) (char-to-string (cdr p)))))
   ((memq c ygg-match--quote-chars) (cons (char-to-string c) (char-to-string c)))))

(defun ygg-match-surround (c)
  "Wrap every selection with the pair for C."
  (interactive (list (read-char)))
  (let ((pair (ygg-match--surround-pair c)))
    (when pair
      (ygg-with-verb
        (ygg-do-selections
         (lambda (beg end _dir)
           (save-excursion
             (goto-char end) (insert (cdr pair))
             (goto-char beg) (insert (car pair)))))))))

(defun ygg-match-delete-surround (c)
  "Delete the enclosing pair for C around every selection."
  (interactive (list (read-char)))
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg _end _dir)
       (save-excursion
         (goto-char beg)
         (let ((bounds (ygg-match--textobject-bounds c 'around)))
           (when bounds
             (goto-char (cdr bounds)) (delete-char -1)
             (goto-char (car bounds)) (delete-char 1))))))))

(defun ygg-match-replace-surround (c1 c2)
  "Replace the enclosing pair for C1 with the pair for C2."
  (interactive (list (read-char) (read-char)))
  (let ((pair (ygg-match--surround-pair c2)))
    (when pair
      (ygg-with-verb
        (ygg-do-selections
         (lambda (beg _end _dir)
           (save-excursion
             (goto-char beg)
             (let ((bounds (ygg-match--textobject-bounds c1 'around)))
               (when bounds
                 (goto-char (1- (cdr bounds))) (delete-char 1) (insert (cdr pair))
                 (goto-char (car bounds)) (delete-char 1) (insert (car pair)))))))))))

;;; Prefix self-promotion

;;;###autoload
(defun ygg-match-prefix ()
  "First-use shim: promote \"m\" to `ygg-match-map', then dispatch this key."
  (interactive)
  (yggdrasil-define-keys 'normal "m" ygg-match-map :label "match")
  (let* ((key (read-char))
         (cmd (lookup-key ygg-match-map (vector key))))
    (if (commandp cmd)
        ;; the shortcut command reads last-command-event for its char;
        ;; this manual dispatch bypasses the key sequence that would set it
        (let ((last-command-event key)) (call-interactively cmd))
      (message "yggdrasil: no match binding for %c" key))))

(yggdrasil-define-keys 'ygg-match-map
  "m" #'ygg-match-jump :label "jump"
  "i" #'ygg-match-inside :label "inside"
  "a" #'ygg-match-around :label "around"
  "s" #'ygg-match-surround :label "surround"
  "d" #'ygg-match-delete-surround :label "delete surround"
  "r" #'ygg-match-replace-surround :label "replace surround")

;;; m-prefix shortcuts (hel-style): `m C' = `m i C' for every object char
;;; not already claimed by a match-map binding above.

(defconst ygg-match--dispatch-chars
  (append ygg-match--quote-chars
          (mapcar #'car ygg-match--bracket-pairs)
          '(?w ?p ?W ?s ?e ?a ?c ?i ?t))
  "Object chars `ygg-match--textobject-bounds' recognizes for mi/ma.
f stays absent: layer-lsp owns it via :before-until advice for the
function object. t hosts the vim/evil tag object here, but layer-lsp's
type object (same advice) takes precedence when point sits inside a
class/struct/impl node — so t means type in code, tag in markup.")

(defun ygg-match--shortcut-command ()
  "Hel-style shortcut: `m C' selects inner object for C, as `m i C' would."
  (interactive)
  (ygg-match--apply-textobject last-command-event 'inside))

(dolist (c ygg-match--dispatch-chars)
  (unless (lookup-key ygg-match-map (vector c))
    (define-key ygg-match-map (vector c) #'ygg-match--shortcut-command)))

;;; Tree-sitter expand/shrink

(defvar-local ygg-match--treesit-stack nil)

(defun ygg-match--treesit-ready-p ()
  (and (fboundp 'treesit-available-p) (treesit-available-p) (treesit-parser-list)))

(defun ygg-match--sexp-expand ()
  ;; select OUTSIDE any save-excursion — it restores point and would
  ;; silently discard the new selection
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (let* ((sym (save-excursion (goto-char beg)
                                (bounds-of-thing-at-point 'symbol)))
           (target
            (if (and sym (or (< (car sym) beg) (> (cdr sym) end)))
                sym
              (condition-case nil
                  (save-excursion
                    (goto-char beg)
                    (backward-up-list 1 t t)
                    (let ((s (point)))
                      (forward-sexp 1)
                      (cons s (point))))
                (error nil)))))
      (if (and target (or (< (car target) beg) (> (cdr target) end)))
          (progn (push (cons beg end) ygg-match--treesit-stack)
                 (ygg-set-selection (car target) (cdr target)))
        (message "yggdrasil: nothing to expand")))))

;;;###autoload
(defun ygg-treesit-expand ()
  "Expand the primary selection to the next enclosing syntax node."
  (interactive)
  (if (ygg-match--treesit-ready-p)
      (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
        (let ((node (treesit-node-on beg end)))
          (while (and node (>= (treesit-node-start node) beg) (<= (treesit-node-end node) end))
            (setq node (treesit-node-parent node)))
          (if (null node)
              (message "yggdrasil: no larger node to expand to")
            (push (cons beg end) ygg-match--treesit-stack)
            (ygg-set-selection (treesit-node-start node) (treesit-node-end node)))))
    (ygg-match--sexp-expand)))

;;;###autoload
(defun ygg-treesit-shrink ()
  "Pop the selection saved by the last `ygg-treesit-expand'."
  (interactive)
  (if (ygg-match--treesit-ready-p)
      (if ygg-match--treesit-stack
          (pcase-let ((`(,beg . ,end) (pop ygg-match--treesit-stack)))
            (ygg-set-selection beg end))
        (message "yggdrasil: no selection to shrink to"))
    (message "yggdrasil: tree-sitter unavailable")))

(defun ygg-match--treesit-select-sibling (next)
  (unless (ygg-match--treesit-ready-p)
    (user-error "tree-sitter unavailable"))
  (pcase-let* ((`(,beg ,end ,_) (ygg-selection-effective-bounds))
               (node (treesit-node-on beg end))
               (sib (and node (if next (treesit-node-next-sibling node t)
                                 (treesit-node-prev-sibling node t)))))
    (unless sib (user-error (if next "No next sibling" "No previous sibling")))
    (setq ygg-match--treesit-stack nil)
    (ygg-set-selection (treesit-node-start sib) (treesit-node-end sib))))

;;;###autoload
(defun ygg-treesit-next-sibling ()
  "Select the next named sibling of the node covering the selection."
  (interactive)
  (ygg-match--treesit-select-sibling t))

;;;###autoload
(defun ygg-treesit-prev-sibling ()
  "Select the previous named sibling of the node covering the selection."
  (interactive)
  (ygg-match--treesit-select-sibling nil))

(yggdrasil-define-keys 'ygg-match-map
  "+" #'ygg-treesit-expand :label "expand"
  "-" #'ygg-treesit-shrink :label "shrink"
  "n" #'ygg-treesit-next-sibling :label "next sibling"
  "N" #'ygg-treesit-prev-sibling :label "prev sibling")

(provide 'yggdrasil-match)
;;; yggdrasil-match.el ends here
