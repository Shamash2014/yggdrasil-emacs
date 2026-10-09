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

(defvar ygg-match--textobject-count 1
  "Enclosing level a bracket textobject selects; 1 is the innermost.")

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

(defun ygg-match--unmoved (anchor cursor)
  "The `ygg-each-selection-update' result that leaves ANCHOR/CURSOR as is."
  (cons anchor (if (>= cursor anchor) (1+ cursor) cursor)))

(defun ygg-match-jump ()
  "Move to the match of the bracket at/near point; in visual state,
select from that bracket to its match, inclusive."
  (interactive)
  (ygg-each-selection-update
   (lambda (anchor cursor _dir)
     (pcase (ygg-match--bracket-bounds)
       (`(,beg ,end ,fwd)
        (cond ((ygg-visual-p) (if fwd (cons beg end) (cons end beg)))
              (fwd (cons (1- end) end))
              (t (cons beg (1+ beg)))))
       (_ (ygg-match--unmoved anchor cursor))))))

;;; Enclosing-pair finders shared by textobjects and surround ops

(defun ygg-match--enclosing-syntax-levels (open)
  "Pairs opened by OPEN around point as ((BEG . END)...), innermost first."
  (let ((openers (cl-remove-if-not (lambda (pos) (eq (char-after pos) open))
                                   (reverse (nth 9 (syntax-ppss))))))
    ;; syntax-ppss hasn't "entered" a pair whose opener is AT point — check it
    (when (eq (char-after) open) (push (point) openers))
    (delq nil (mapcar (lambda (beg)
                        (condition-case nil
                            (save-excursion (goto-char beg) (forward-sexp 1) (cons beg (point)))
                          (scan-error nil)))
                      openers))))

(defun ygg-match--find-enclosing-charscan (open close &optional exclude-opener-at-point)
  (save-excursion
    (let ((start (point)) (depth 0) beg)
      (when (and (not exclude-opener-at-point) (eq (char-after) open))
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

(defun ygg-match--enclosing-charscan-levels (open close)
  (let (levels bounds)
    (save-excursion
      (while (setq bounds (ygg-match--find-enclosing-charscan open close (and levels t)))
        (push bounds levels)
        (goto-char (car bounds))))
    (nreverse levels)))

(defun ygg-match--enclosing-levels (open close)
  (if (eq open ?<)
      (ygg-match--enclosing-charscan-levels open close)
    (ygg-match--enclosing-syntax-levels open)))

(defun ygg-match--bracket-levels (open close)
  "Levels of OPEN/CLOSE pairs around point, innermost first; outside any,
those of the next pair on the line, else of the previous one (targets.el)."
  (or (ygg-match--enclosing-levels open close)
      (save-excursion
        (let ((start (point)))
          (or (and (search-forward (char-to-string open) (line-end-position) t)
                   (progn (backward-char 1) (ygg-match--enclosing-levels open close)))
              (progn (goto-char start)
                     (and (search-backward (char-to-string close) (line-beginning-position) t)
                          (ygg-match--enclosing-levels open close))))))))

(defun ygg-match--find-enclosing-char (c)
  "Nearest C on each side of point within the line as (BEG . END), END
past the closing C; a C at point opens the pair."
  (let* ((s (char-to-string c))
         (lb (line-beginning-position)) (le (line-end-position))
         (beg (if (eq (char-after) c)
                  (point)
                (save-excursion (and (search-backward s lb t) (point)))))
         (end (and beg (save-excursion
                         (goto-char (1+ beg))
                         (and (search-forward s le t) (point))))))
    (when end (cons beg end))))

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
    (let ((bounds (nth (1- ygg-match--textobject-count)
                       (ygg-match--bracket-levels open close))))
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
  "Innermost enclosing (), [], or {} pair around point; the parser state
answers it, a raw character scan covers odd syntax tables."
  (or (when-let* ((open (nth 1 (syntax-ppss)))
                  ((memq (char-after open) '(?\( ?\[ ?\{))))
        (condition-case nil
            (save-excursion (goto-char open) (forward-sexp 1) (cons open (point)))
          (scan-error nil)))
      (let (best)
        (dolist (pair '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))
          (let ((b (ygg-match--find-enclosing-charscan (car pair) (cdr pair))))
            (when (and b (or (null best) (> (car b) (car best)))) (setq best b))))
        best)))

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
  (let ((re "\\(?:\\`\\|_\\)\\(?:argument\\|parameter\\)\\'"))
    (let ((node (ygg-match--ts-ancestor
                 (lambda (n) (or (ygg-match--ts-thing-p n 'argument)
                                 (string-match-p re (treesit-node-type n)))))))
      (when node
        (ygg-match--argument-span (treesit-node-start node) (treesit-node-end node) which)))))

(defun ygg-match--argument-span (ibeg iend which)
  (if (eq which 'inside)
      (cons ibeg iend)
    (or (save-excursion
          (goto-char iend)
          (skip-chars-forward " \t\n")
          (when (eq (char-after) ?,)
            (forward-char 1) (skip-chars-forward " \t\n")
            (cons ibeg (point))))
        (save-excursion
          (goto-char ibeg)
          (skip-chars-backward " \t\n")
          (when (eq (char-before) ?,)
            (backward-char 1) (skip-chars-backward " \t\n")
            (cons (point) iend)))
        (cons ibeg iend))))

(defconst ygg-match--argument-list-regexp
  "\\`\\(?:[a-z_]*_\\)?\\(?:arguments\\|parameters\\|argument_list\\|parameter_list\\)\\'")

(defun ygg-match--argument-children-bounds (which)
  "The argument-list child around point, cheaper than a character scan."
  (when-let* ((list (ygg-match--ts-ancestor
                     (lambda (n) (string-match-p ygg-match--argument-list-regexp
                                                 (treesit-node-type n)))))
              (kids (seq-remove (lambda (n) (string-match-p "comment" (treesit-node-type n)))
                                (treesit-node-children list t))))
    (let* ((pt (point))
           (kid (or (seq-find (lambda (n) (and (>= pt (treesit-node-start n))
                                               (<= pt (treesit-node-end n))))
                              kids)
                    (car (last (seq-filter (lambda (n) (< (treesit-node-end n) pt)) kids)))
                    (car kids))))
      (ygg-match--argument-span (treesit-node-start kid) (treesit-node-end kid) which))))

(defun ygg-match--argument-bounds (which)
  (or (ygg-match--argument-children-bounds which)
      (ygg-match--argument-fallback-bounds which)))

(defun ygg-match--comment-treesit-bounds (which)
  (when-let* ((node (ygg-match--ts-ancestor
                     (lambda (n) (or (ygg-match--ts-thing-p n 'comment)
                                     (string-match-p "comment" (treesit-node-type n)))))))
    (let ((b (cons (treesit-node-start node) (treesit-node-end node))))
      (if (eq which 'around) (ygg-match--around-pad b) b))))

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

;;; Syntax-node textobjects (f t T): builtin treesit things, then combobulate,
;;; then node-type regexps, then non-treesit fallbacks

(declare-function combobulate-node-at-point "combobulate-navigation")
(declare-function combobulate-procedure-collect-activation-nodes "combobulate-procedure")
(declare-function combobulate-get "combobulate-setup")
(declare-function python-nav-end-of-statement "python")

(defconst ygg-match--type-regexp
  "\\`\\(?:[a-z_]*_\\)?\\(?:\\(?:class\\|struct\\|impl\\|interface\\|enum\\|trait\\|module\\|protocol\\|object\\|record\\)\\(?:_\\(?:definition\\|declaration\\|item\\|specifier\\)\\)?\\|type_\\(?:alias_\\)?\\(?:declaration\\|definition\\|item\\|statement\\)\\)\\'"
  "Node types counted as a type/class definition; annotations never are.")

(defconst ygg-match--function-regexp
  "\\`\\(?:[a-z_]*_\\)?\\(?:function\\|method\\|func\\|lambda\\)\\(?:_\\(?:definition\\|declaration\\|item\\|expression\\|literal\\|signature\\|signature_item\\)\\)?\\'"
  "Node types counted as a function definition.")

(defconst ygg-match--test-name-regexp
  "\\`\\(?:[Tt]est\\|it_\\)\\|_test\\'\\|Test\\'"
  "Function names that mark a test.")

(defconst ygg-match--test-call-regexp
  "\\`\\(?:it\\|test\\|describe\\|specify\\)\\(?:\\.[a-z]+\\)?\\'"
  "Callee names of JS-style test blocks.")

(defun ygg-match--ts-ancestor (pred &optional skip)
  "Innermost node at point, or ancestor of it, satisfying PRED, after
passing over SKIP matches; a decorator wrapper counts with its definition."
  (when (and (fboundp 'treesit-node-at) (treesit-parser-list))
    (let ((node (treesit-node-at (point))) (left (or skip 0)) last hit)
      (while (and node (not hit))
        (when (and (funcall pred node)
                   (not (and last (treesit-node-eq (ygg-match--ts-unwrap node) last))))
          (if (zerop left) (setq hit node) (setq left (1- left) last node)))
        (unless hit (setq node (treesit-node-parent node))))
      hit)))

(defun ygg-match--ts-thing-p (node thing)
  "Non-nil when NODE is THING per the mode's `treesit-thing-settings'."
  (and (fboundp 'treesit-node-match-p)
       (ignore-errors (treesit-node-match-p node thing t))))

(defun ygg-match--ts-unwrap (node)
  "The definition inside a decorator-style wrapper NODE, else NODE."
  (or (treesit-node-child-by-field-name node "definition") node))

(defun ygg-match--ts-type-p (node)
  (let ((type (treesit-node-type (ygg-match--ts-unwrap node))))
    (and (treesit-node-parent node)
         (not (string-suffix-p "_body" type))
         (string-match-p ygg-match--type-regexp type))))

(defun ygg-match--ts-function-p (node)
  (string-match-p ygg-match--function-regexp (treesit-node-type (ygg-match--ts-unwrap node))))

(defun ygg-match--builtin-pred (kind)
  (pcase kind
    ('function (lambda (n) (and (or (ygg-match--ts-thing-p n 'defun)
                                    (ygg-match--ts-thing-p n 'function))
                                (not (ygg-match--ts-type-p n)))))
    ('type (lambda (n) (and (or (ygg-match--ts-thing-p n 'defun)
                                (ygg-match--ts-thing-p n 'type))
                            (ygg-match--ts-type-p n))))))

(defconst ygg-match--elixir-heads
  '((function "def" "defp" "defmacro" "defmacrop")
    (type "defmodule" "defprotocol" "defimpl")
    (test "test" "describe")
    (conditional "if" "unless" "case" "cond" "with")
    (loop "for"))
  "Call targets that make an Elixir `call' node each kind of object.")

(defun ygg-match--elixir-p ()
  (and (fboundp 'treesit-parser-list) (treesit-parser-list nil 'elixir) t))

(defun ygg-match--elixir-pred (kind)
  (if (eq kind 'block)
      (lambda (n) (equal (treesit-node-type n) "do_block"))
    (let ((heads (cdr (assq kind ygg-match--elixir-heads))))
      (lambda (n)
        (and (equal (treesit-node-type n) "call")
             (when-let* ((target (treesit-node-child-by-field-name n "target")))
               (member (treesit-node-text target t) heads)))))))

(defun ygg-match--elixir-call-p (node)
  (and (ygg-match--elixir-p) (equal (treesit-node-type node) "call")))

(defun ygg-match--regexp-pred (kind)
  (pcase kind
    ('function #'ygg-match--ts-function-p)
    ('type #'ygg-match--ts-type-p)))

(defun ygg-match--combobulate-node (kind)
  "Defun-class node at point from combobulate's per-language defun procedures."
  (when (and (fboundp 'combobulate-node-at-point)
             (fboundp 'combobulate-procedure-collect-activation-nodes)
             (fboundp 'combobulate-get))
    (let ((var (ignore-errors (combobulate-get 'procedures-defun))))
      (when (and var (boundp var))
        (let ((types (seq-filter
                      (lambda (type)
                        (let ((typep (string-match-p ygg-match--type-regexp type)))
                          (if (eq kind 'type) typep (not typep))))
                      (combobulate-procedure-collect-activation-nodes (symbol-value var)))))
          (when types
            (let ((node (ignore-errors (combobulate-node-at-point types t))))
              (and node (not (and (eq kind 'function) (ygg-match--ts-type-p node)))
                   node))))))))

(defun ygg-match--ts-object-node (kind)
  (when (treesit-parser-list)
    (let ((skip (1- ygg-match--textobject-count)))
      (if (ygg-match--elixir-p)
          (ygg-match--ts-ancestor (ygg-match--elixir-pred kind) skip)
        (or (ygg-match--ts-ancestor (ygg-match--builtin-pred kind) skip)
            (and (zerop skip) (ygg-match--combobulate-node kind))
            (ygg-match--ts-ancestor (ygg-match--regexp-pred kind) skip))))))

(defun ygg-match--node-span (node)
  (cons (treesit-node-start node) (treesit-node-end node)))

(defun ygg-match--ts-brace-inner (body)
  "BODY without its braces or do/end, else the whole node."
  (let* ((kids (treesit-node-children body))
         (open (seq-find (lambda (n) (member (treesit-node-type n) '("{" "do"))) kids))
         (close (seq-find (lambda (n) (member (treesit-node-type n) '("}" "end")))
                          (reverse kids))))
    (if (and open close (< (treesit-node-start open) (treesit-node-start close)))
        (cons (treesit-node-end open) (treesit-node-start close))
      (ygg-match--node-span body))))

(defun ygg-match--ts-go-type-body (def)
  "The field list or interface inside a Go `type_declaration'."
  (when-let* (((equal (treesit-node-type def) "type_declaration"))
              (spec (treesit-node-child def 0 t))
              (ty (treesit-node-child-by-field-name spec "type")))
    (or (seq-find (lambda (n) (equal (treesit-node-type n) "field_declaration_list"))
                  (treesit-node-children ty t))
        ty)))

(defun ygg-match--ts-body (def)
  "The body of DEF: body field, R rhs function body, or if consequence."
  (or (treesit-node-child-by-field-name def "body")
      (ygg-match--ts-go-type-body def)
      (seq-find (lambda (n) (equal (treesit-node-type n) "do_block"))
                (and (ygg-match--elixir-call-p def) (treesit-node-children def t)))
      (when-let* ((rhs (treesit-node-child-by-field-name def "rhs")))
        (treesit-node-child-by-field-name rhs "body"))
      (treesit-node-child-by-field-name def "consequence")))

(defun ygg-match--ts-inner (node)
  "Body of NODE without its braces, else the whole node."
  (let* ((def (ygg-match--ts-unwrap node))
         (body (ygg-match--ts-body def)))
    (ygg-match--ts-brace-inner (or body def))))

(defun ygg-match--lisp-form-bounds ()
  "The defun around point as (BEG . END) without its trailing blank space."
  (when-let* ((b (bounds-of-thing-at-point 'defun)))
    (cons (car b) (save-excursion (goto-char (cdr b))
                                  (skip-chars-backward " \t\n" (car b))
                                  (point)))))

(defun ygg-match--lisp-form-head (beg)
  (save-excursion
    (goto-char beg)
    (and (eq (char-after) ?\()
         (progn (forward-char 1) (thing-at-point 'symbol t)))))

(defun ygg-match--lisp-form-inner (bounds skip-arglist)
  "Body of the definition form BOUNDS: past head, name and arglist."
  (save-excursion
    (goto-char (1+ (car bounds)))
    (ignore-errors
      (forward-sexp 2)
      (skip-chars-forward " \t\n")
      (when (and skip-arglist (eq (char-after) ?\())
        (forward-sexp 1)
        (skip-chars-forward " \t\n")))
    (cons (point) (max (point) (1- (cdr bounds))))))

(defconst ygg-match--lisp-type-heads '("cl-defstruct" "defstruct" "defclass" "define-error"))

(defun ygg-match--lisp-bounds (which heads-pred)
  "Fallback f/t/T for buffers without a parser: the top-level form at point."
  (when-let* ((b (ygg-match--lisp-form-bounds))
              (head (ygg-match--lisp-form-head (car b)))
              ((funcall heads-pred head)))
    (if (eq which 'around)
        b
      (ygg-match--lisp-form-inner b (not (member head ygg-match--lisp-type-heads))))))

(defun ygg-match--defun-body-bounds (bounds)
  "BOUNDS of a non-treesit defun without its header: python's lines after
the colon, else what sits between the first brace and the last."
  (save-excursion
    (goto-char (car bounds))
    (or (when (derived-mode-p 'python-mode)
          (ignore-errors
            (python-nav-end-of-statement)
            (when (< (1+ (point)) (cdr bounds))
              (cons (min (1+ (point)) (cdr bounds)) (cdr bounds)))))
        (when-let* (((search-forward "{" (cdr bounds) t))
                    (open (point))
                    (close (save-excursion (goto-char (cdr bounds))
                                           (and (search-backward "}" open t) (point)))))
          (cons open close))
        bounds)))

(defun ygg-match--function-bounds (which)
  (if-let* ((node (ygg-match--ts-object-node 'function)))
      (if (eq which 'around) (ygg-match--node-span node) (ygg-match--ts-inner node))
    (or (ygg-match--lisp-bounds which (lambda (head) (not (member head ygg-match--lisp-type-heads))))
        (when-let* (((not (treesit-parser-list)))
                    (b (ygg-match--lisp-form-bounds)))
          (if (or (eq which 'around) (derived-mode-p 'lisp-data-mode))
              b
            (ygg-match--defun-body-bounds b))))))

(defun ygg-match--type-bounds (which)
  (if-let* ((node (ygg-match--ts-object-node 'type)))
      (if (eq which 'around) (ygg-match--node-span node) (ygg-match--ts-inner node))
    (ygg-match--lisp-bounds which (lambda (head) (member head ygg-match--lisp-type-heads)))))

(defun ygg-match--node-name (node)
  (when-let* ((name (treesit-node-child-by-field-name (ygg-match--ts-unwrap node) "name")))
    (treesit-node-text name t)))

(defun ygg-match--test-function-p (node)
  (let ((name (and (ygg-match--ts-function-p node) (ygg-match--node-name node))))
    (or (and name (string-match-p ygg-match--test-name-regexp name))
        (and name (when-let* ((prev (treesit-node-prev-sibling node t)))
                    (and (string-match-p "attribute\\|decorator" (treesit-node-type prev))
                         (string-match-p "test" (treesit-node-text prev t))))))))

(defun ygg-match--test-call-p (node)
  (and (string-match-p "\\`call" (treesit-node-type node))
       (when-let* ((callee (treesit-node-child-by-field-name node "function")))
         (string-match-p ygg-match--test-call-regexp (treesit-node-text callee t)))))

(defun ygg-match--test-call-inner (node)
  (let* ((args (treesit-node-child-by-field-name node "arguments"))
         (fn (and args (seq-find #'ygg-match--ts-function-p
                                 (reverse (treesit-node-children args t))))))
    (if fn (ygg-match--ts-inner fn) (ygg-match--node-span node))))

(defun ygg-match--test-bounds (which)
  (if-let* ((node (ygg-match--ts-ancestor
                   (if (ygg-match--elixir-p)
                       (ygg-match--elixir-pred 'test)
                     (lambda (n) (or (ygg-match--test-function-p n) (ygg-match--test-call-p n))))
                   (1- ygg-match--textobject-count))))
      (cond ((eq which 'around) (ygg-match--node-span node))
            ((ygg-match--test-call-p node) (ygg-match--test-call-inner node))
            (t (ygg-match--ts-inner node)))
    (ygg-match--lisp-bounds which (lambda (head) (string-match-p "deftest\\'" head)))))

;;; Loop, conditional, parameter, call, string, comment, block (l C P k S M B)

(defconst ygg-match--simple-objects
  `((?l (loop) "\\`\\(?:enhanced_\\)?\\(?:for\\|foreach\\|while\\|do\\|repeat\\|repeat_while\\|loop\\)\\(?:_in\\)?\\(?:_statement\\|_expression\\|_loop\\)?\\'" body)
    (?C (conditional) "\\`\\(?:[a-z_]*_\\)?\\(?:if\\|switch\\|match\\|when\\|case\\)_\\(?:statement\\|expression\\)\\'" body)
    (?P (parameter) "\\(?:\\`\\|_\\)\\(?:parameter\\|argument\\)\\'" span)
    (?k (call) "call\\(?:_expression\\)?\\'\\|invocation" call)
    (?S (string) "\\`\\(?:[a-z_]*_\\)?string\\(?:_literal\\)?\\'" string)
    (?M (comment) "comment" span)
    (?B (block) "block\\|function_body" block))
  "Object char, treesit things, node-type regexp and inner kind per object.")

(defun ygg-match--delimited-inner (node)
  "NODE without its first and last child when those are bare delimiters."
  (let ((first (treesit-node-child node 0))
        (last (treesit-node-child node -1)))
    (if (and first last (not (eq first last))
             (string-match-p "\\`[a-zA-Z]*[\"'`(\\[{]+\\'" (treesit-node-text first t))
             (string-match-p "\\`[]\"'`)}]+\\'" (treesit-node-text last t)))
        (cons (treesit-node-end first) (treesit-node-start last))
      (ygg-match--node-span node))))

(defun ygg-match--call-suffix-arguments (node)
  (when-let* ((suffix (seq-find (lambda (n) (equal (treesit-node-type n) "call_suffix"))
                                (treesit-node-children node t))))
    (seq-find (lambda (n) (equal (treesit-node-type n) "value_arguments"))
              (treesit-node-children suffix t))))

(defun ygg-match--simple-inner (node kind)
  (pcase kind
    ('body (ygg-match--ts-inner node))
    ('block (ygg-match--ts-brace-inner node))
    ('string (if (> (treesit-node-child-count node) 1)
                 (ygg-match--delimited-inner node)
               (let ((b (ygg-match--node-span node)))
                 (if (> (- (cdr b) (car b)) 1) (cons (1+ (car b)) (1- (cdr b))) b))))
    ('call (if-let* ((args (or (treesit-node-child-by-field-name node "arguments")
                              (ygg-match--call-suffix-arguments node)
                              (seq-find (lambda (n) (equal (treesit-node-type n) "token_tree"))
                                        (treesit-node-children node t)))))
               (ygg-match--delimited-inner args)
             (ygg-match--node-span node)))
    (_ (ygg-match--node-span node))))

(defun ygg-match--simple-bounds (c which)
  (pcase-let ((`(,things ,regexp ,kind) (cdr (assq c ygg-match--simple-objects))))
    (when (treesit-parser-list)
      (when-let* ((skip (1- ygg-match--textobject-count))
                  (node (if (and (ygg-match--elixir-p) (memq c '(?l ?C ?B)))
                            (ygg-match--ts-ancestor
                             (ygg-match--elixir-pred (if (eq c ?B) 'block (car things)))
                             skip)
                          (or (ygg-match--ts-ancestor
                               (lambda (n) (seq-some (lambda (th) (ygg-match--ts-thing-p n th)) things))
                               skip)
                              (ygg-match--ts-ancestor
                               (lambda (n) (string-match-p regexp (treesit-node-type n)))
                               skip)))))
        (if (eq which 'around)
            (ygg-match--node-span node)
          (ygg-match--simple-inner node kind))))))

;;; Syntax-table string and comment fallback (S M) for buffers without a parser

(defun ygg-match--syntax-bounds (c which)
  (let* ((ppss (syntax-ppss))
         (start (and (if (eq c ?S) (nth 3 ppss) (nth 4 ppss)) (nth 8 ppss)))
         (end (and start
                   (save-excursion
                     (goto-char start)
                     (and (if (eq c ?S)
                              (ignore-errors (forward-sexp 1) t)
                            (forward-comment 1))
                          (point))))))
    (when end
      (if (eq c ?S)
          (if (eq which 'around) (cons start end) (cons (1+ start) (1- end)))
        (let ((b (cons start (save-excursion (goto-char end)
                                             (skip-chars-backward "\n" start)
                                             (point)))))
          (if (eq which 'around) (ygg-match--around-pad b) b))))))

;;; Closest surround pair (Helix m)

(defun ygg-match--closest-pair-candidates ()
  "Pairs enclosing point as (BEG . END), innermost first."
  (let (cands (ppss (syntax-ppss)))
    (dolist (open '(?\( ?\[ ?\{))
      (setq cands (append (ygg-match--enclosing-syntax-levels open) cands)))
    (when (nth 3 ppss)
      (when-let* ((s (condition-case nil
                         (save-excursion (goto-char (nth 8 ppss)) (forward-sexp 1)
                                         (cons (nth 8 ppss) (point)))
                       (scan-error nil))))
        (push s cands)))
    (sort cands (lambda (a b) (> (car a) (car b))))))

(defun ygg-match--closest-pair-bounds (which)
  (when-let* ((b (nth (1- ygg-match--textobject-count)
                      (ygg-match--closest-pair-candidates))))
    (if (eq which 'around) b (cons (1+ (car b)) (1- (cdr b))))))

;;; Tag textobject (x) — HTML/XML/JSX element pairs

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
   ((and (eq c ?B) (ygg-match--simple-bounds c which)))
   ((assq c ygg-match--bracket-pairs) (ygg-match--bracket-textobj-bounds c which))
   ((eq c ?w) (ygg-match--thing-bounds 'word which))
   ((eq c ?p) (ygg-match--paragraph-bounds which))
   ((eq c ?W) (let ((b (ygg-match--bounds-of-WORD)))
                (if (eq which 'around) (ygg-match--around-pad b) b)))
   ((eq c ?s) (ygg-match--thing-bounds 'sentence which))
   ((eq c ?e) (cons (point-min) (point-max)))
   ((eq c ?a) (or (ygg-match--argument-treesit-bounds which)
                  (ygg-match--argument-bounds which)))
   ((eq c ?c) (ygg-match--comment-bounds which))
   ((eq c ?i) (ygg-match--indent-bounds which))
   ((eq c ?x) (ygg-match--tag-bounds which))
   ((eq c ?f) (ygg-match--function-bounds which))
   ((eq c ?t) (ygg-match--type-bounds which))
   ((eq c ?T) (ygg-match--test-bounds which))
   ((eq c ?m) (ygg-match--closest-pair-bounds which))
   ((eq c ?P) (or (ygg-match--simple-bounds c which)
                  (ygg-match--argument-bounds which)))
   ((assq c ygg-match--simple-objects)
    (or (ygg-match--simple-bounds c which)
        (and (memq c '(?S ?M)) (ygg-match--syntax-bounds c which))))))

(defun ygg-match--textobject-level-bounds (c which level)
  (let ((ygg-match--textobject-count level))
    (ygg-match--textobject-bounds c which)))

(defun ygg-match--apply-textobject (c which)
  "Select object C per WHICH; a count picks the Nth enclosing bracket
level, and a selection already equal to it grows one level out."
  (let ((level (max 1 (prefix-numeric-value current-prefix-arg))))
    (ygg-each-selection-update
     (lambda (anchor cursor _dir)
       (let ((bounds (ygg-match--textobject-level-bounds c which level)))
         (when (and bounds (or (memq c '(?m ?f ?t ?T ?l ?C ?k)) (assq c ygg-match--bracket-pairs))
                    (equal bounds (cons (min anchor cursor) (max anchor (1+ cursor)))))
           (setq bounds (or (ygg-match--textobject-level-bounds c which (1+ level))
                            bounds)))
         (if bounds (cons (car bounds) (cdr bounds)) (ygg-match--unmoved anchor cursor)))))))

(defun ygg-match-inside (c)
  "Select inside the pair/thing for C."
  (interactive (list (read-char)))
  (ygg-match--apply-textobject c 'inside))

(defun ygg-match-around (c)
  "Select around the pair/thing for C, delimiters included."
  (interactive (list (read-char)))
  (ygg-match--apply-textobject c 'around))

;;; Surround (ms / md / mr)

(defun ygg-match--read-tag ()
  (let* ((input (read-string "<" "" nil))
         (name (and (string-match "^\\([^ \t/>]+\\)" input)
                    (match-string 1 input))))
    (when (and name (not (string-empty-p name)))
      (cons (format "<%s>" input)
            (format "</%s>" name)))))

(defun ygg-match--read-function ()
  (let ((name (read-string "function: " "" nil)))
    (when (and name (not (string-empty-p name)))
      (cons (format "%s(" name) ")"))))

(defun ygg-match--surround-pair (c &optional tight)
  "Opening and closing strings for surrounding with C; TIGHT drops the
space an opening bracket pads with."
  (cond
   ((eq c 27) nil)
   ((memq c '(?t ?<)) (ygg-match--read-tag))
   ((eq c ?f) (ygg-match--read-function))
   ((assq c ygg-match--bracket-pairs)
    (pcase-let ((`(,open . ,close) (cdr (assq c ygg-match--bracket-pairs)))
                (pad (if (and (not tight) (memq c '(?\( ?\[ ?\{))) " " "")))
      (cons (concat (string open) pad) (concat pad (string close)))))
   ((and (characterp c) (>= c 32) (/= c 127)) (cons (string c) (string c)))))

(defun ygg-match--surround-bounds (c)
  "Delimiters of the C pair around point as
\(OUTER-BEG INNER-BEG INNER-END OUTER-END), or nil."
  (pcase c
    (?t (let ((outer (ygg-match--tag-bounds 'around))
              (inner (ygg-match--tag-bounds 'inside)))
          (when (and outer inner)
            (list (car outer) (car inner) (cdr inner) (cdr outer)))))
    (?f (pcase (car (ygg-match--enclosing-levels ?\( ?\)))
          (`(,open . ,end)
           (list (save-excursion (goto-char open) (skip-syntax-backward "w_") (point))
                 (1+ open) (1- end) end))))
    (_ (pcase (cond
               ((memq c ygg-match--quote-chars) (ygg-match--find-enclosing-quote c))
               ((assq c ygg-match--bracket-pairs)
                (pcase-let ((`(,open . ,close) (cdr (assq c ygg-match--bracket-pairs))))
                  (car (ygg-match--enclosing-levels open close))))
               ((characterp c) (ygg-match--find-enclosing-char c)))
         (`(,beg . ,end) (list beg (1+ beg) (1- end) end))))))

(defun ygg-match--surround-targets (c)
  "Distinct delimiters of the C pair around each selection, as markers."
  (let (targets)
    (ygg-do-selections
     (lambda (beg _end _dir)
       (let ((bounds (save-excursion (goto-char beg) (ygg-match--surround-bounds c))))
         (when bounds (cl-pushnew bounds targets :test #'equal)))))
    ;; insertion types keep a nested or adjacent pair's delimiters out of this one's
    (mapcar (lambda (bounds) (cl-mapcar #'copy-marker bounds '(t nil t nil)))
            targets)))

(defun ygg-match--rewrite-surround (c pair)
  (save-excursion
    (dolist (target (ygg-match--surround-targets c))
      (pcase-let ((`(,obeg ,ibeg ,iend ,oend) target))
        (delete-region iend oend)
        (goto-char iend) (insert (cdr pair))
        (delete-region obeg ibeg)
        (goto-char obeg) (insert (car pair))))))

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
    (ygg-match--rewrite-surround c '("" . ""))))

(defun ygg-match-replace-surround (c1 c2)
  "Replace the enclosing pair for C1 with the pair for C2."
  (interactive (list (read-char) (read-char)))
  (let ((pair (ygg-match--surround-pair c2 t)))
    (when pair
      (ygg-with-verb
        (ygg-match--rewrite-surround c1 pair)))))

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
          '(?w ?p ?W ?s ?e ?a ?c ?i ?f ?t ?T ?m ?x))
  "Object chars `ygg-match--textobject-bounds' recognizes for mi/ma.
w W p s e a c i f t T m x plus every quote and bracket char; `m m' stays jump.")

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

(defun ygg-match--select-treesit-nodes (nodes-of)
  "Replace each selection with one per node NODES-OF returns for the
named node covering it; a selection it returns nothing for stays."
  (unless (ygg-match--treesit-ready-p)
    (user-error "tree-sitter unavailable"))
  (let (regions)
    (dolist (region (ygg--selection-regions))
      (pcase-let ((`(,beg ,end ,_) region))
        (let ((nodes (funcall nodes-of (treesit-node-on beg end nil t))))
          (if nodes
              (dolist (node nodes)
                (push (cons (treesit-node-start node) (treesit-node-end node)) regions))
            (push (cons beg end) regions)))))
    (setq ygg-match--treesit-stack nil)
    (ygg--install-regions (sort (delete-dups regions) (lambda (a b) (< (car a) (car b)))))))

;;;###autoload
(defun ygg-treesit-select-children ()
  "Select every named child of the node covering each selection."
  (interactive)
  (ygg-match--select-treesit-nodes
   (lambda (node) (and node (treesit-node-children node t)))))

;;;###autoload
(defun ygg-treesit-select-siblings ()
  "Select every named sibling of the node covering each selection."
  (interactive)
  (ygg-match--select-treesit-nodes
   (lambda (node)
     (let ((parent (and node (treesit-parent-until
                              node (lambda (p) (> (treesit-node-child-count p) 1))))))
       (and parent (treesit-node-children parent t))))))

(defun ygg-match--parent-node-target (beg end cursor forward)
  "Cell the parent-node motion lands on from the selection BEG..END:
FORWARD, the one just past the covering node; else its first cell, or
the enclosing node's when CURSOR is already there (Helix)."
  (let ((node (treesit-node-on beg end nil t)))
    (when node
      (let ((start (treesit-node-start node)))
        (cond
         (forward (treesit-node-end node))
         ((/= start cursor) start)
         (t (let ((parent (treesit-parent-until
                           node (lambda (p) (and (treesit-node-check p 'named)
                                                 (< (treesit-node-start p) start))))))
              (if parent (treesit-node-start parent) start))))))))

(defun ygg-match--move-parent-node (forward)
  (unless (ygg-match--treesit-ready-p)
    (user-error "tree-sitter unavailable"))
  (ygg-each-selection-update
   (lambda (anchor cursor _dir)
     (let ((target (ygg-match--parent-node-target
                    (min anchor cursor) (max anchor (1+ cursor)) cursor forward)))
       (cond
        ((or (null target) (>= target (point-max))) (ygg-match--unmoved anchor cursor))
        ((not (ygg-visual-p)) (cons target (1+ target)))
        ((>= target anchor) (cons anchor (1+ target)))
        (t (cons anchor target)))))))

;;;###autoload
(defun ygg-treesit-parent-node-end ()
  "Move past the end of the node covering each selection; visual extends."
  (interactive)
  (ygg-match--move-parent-node t))

;;;###autoload
(defun ygg-treesit-parent-node-start ()
  "Move to the start of the node covering each selection, or of its
parent when already there; visual extends."
  (interactive)
  (ygg-match--move-parent-node nil))

(yggdrasil-define-keys 'ygg-match-map
  "+" #'ygg-treesit-expand :label "expand"
  "-" #'ygg-treesit-shrink :label "shrink"
  "n" #'ygg-treesit-next-sibling :label "next sibling"
  "N" #'ygg-treesit-prev-sibling :label "prev sibling")

;; Helix's Alt tree keys, minus A-n: V n is already skip-to-next-match
(yggdrasil-define-keys 'ygg-selections-map
  "o" #'ygg-treesit-expand :label "expand"
  "i" #'ygg-treesit-shrink :label "shrink"
  "p" #'ygg-treesit-prev-sibling :label "prev sibling"
  "I" #'ygg-treesit-select-children :label "select children"
  "a" #'ygg-treesit-select-siblings :label "select siblings"
  "e" #'ygg-treesit-parent-node-end :label "parent node end"
  "b" #'ygg-treesit-parent-node-start :label "parent node start")

(provide 'yggdrasil-match)
;;; yggdrasil-match.el ends here
