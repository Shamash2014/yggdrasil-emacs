;;; aob-trace.el --- zoom 1: the operation trace -*- lexical-binding: t; -*-

;;; Commentary:
;; One collapsed line per event — kind glyph, title, status, diffstat —
;; TAB expands an entry to its detail (message text, diff patch, raw
;; output).  Renders the last `aob-trace-limit' events on the registry
;; timer; token streams update the summary line, never the buffer
;; per-chunk.  Tails like a log when point is at the end.

;;; Code:

(require 'aob)
(require 'ygg-ui)
(require 'aob-context nil t)
(require 'ygg-diagram nil t)

(declare-function ygg-ui-markdown "ygg-ui" (text))
(declare-function ygg-diagram-fence-at-point "ygg-diagram" ())
(declare-function ygg-diagram-toggle-at-point "ygg-diagram" ())
(declare-function ygg-diagram-replace "ygg-diagram" ())
(declare-function ygg-diagram-toggle-any-at-point "ygg-diagram" ())
(defvar ygg-diagram-image-root)
(defvar ygg-diagram--shown)
(declare-function ygg-diagram-image-at-point "ygg-diagram" ())
(declare-function ygg-diagram-md-fence-at-point "ygg-diagram" ())
(declare-function ygg-normal-state "yggdrasil-core" ())

(defcustom aob-trace-limit 300
  "Events rendered in a trace buffer."
  :type 'natnum :group 'aob)

(defcustom aob-trace-detail-lines 40
  "Lines of detail shown for an expanded entry."
  :type 'natnum :group 'aob)

(defcustom aob-trace-block-max-chars 4000
  "Characters of text one entry draws before the rest is left off.
A block that runs past this ends in a line counting what was cut.  A
coalesced message can run to hundreds of kilobytes, and a buffer holding
three hundred of them whole costs more to lay out than to build."
  :type 'natnum :group 'aob)

(defvar-local aob-trace--session-id nil)

(defvar-local aob-trace--own-parent nil
  "The subagent call whose steps this trace shows as its own, or nil.")

(defun aob-trace--sub-p (ev)
  "Whether EV is a subagent's step rather than this trace's own."
  (when-let* ((parent (plist-get ev :parent)))
    (not (equal parent aob-trace--own-parent))))
(defvar-local aob-trace--expanded nil)
(defvar-local aob-trace--tick -1)
(defvar-local aob-trace--dir nil)
(defvar-local aob-trace--blocks nil)

(defvar aob-buffer-name-function nil
  "Called with KIND, SESSION and SUFFIX to name a session buffer.
Nil keeps aob's own `kind:session\' names, so the package stands alone
and a config that arranges sessions can address them its own way.")

(defun aob--buffer-name (kind s &optional suffix)
  (or (and aob-buffer-name-function
           (funcall aob-buffer-name-function kind s suffix))
      (if suffix
          (format "%s:%s/%s" kind (aob-session-name s) suffix)
        (format "%s:%s" kind (aob-session-name s)))))

(defun aob-trace--name (s) (aob--buffer-name "trace" s))

(defvar aob-trace-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap aob-object-map special-mode-map))
    (define-key map (kbd "TAB") #'aob-trace-tab)
    ;; Delta sends with Command-Enter; Control-Enter for keyboards without it
    (define-key map (kbd "<s-return>") #'aob-trace-send)
    (define-key map (kbd "<C-return>") #'aob-trace-send)
    ;; REPL feel: RET composes to this agent (queues mid-turn, never
    ;; cancels); i steers (cancel + compose)
    (define-key map (kbd "RET") #'aob-trace-answer)
    ;; view/select/copy are yggdrasil's own: v visual state, y the yank
    ;; operator (yy, yw, visual-y) — no special-cased copy keys here.
    ;; interacting is the compose buffer, never an in-trace input line:
    ;; every vim way into insert opens it (i keeps its steer meaning —
    ;; cancel first — a/A/o talk without cancelling, like RET)
    (define-key map "a" #'aob-compose)
    (define-key map "i" #'aob-compose)
    ;; vim c changes what is there: cancel the running turn, then say it anew
    (define-key map "c" #'aob-steer)
    ;; vim: A appends at the end — here that is the inline input
    (define-key map "A" #'aob-trace-input)
    (define-key map "C" #'aob-trace-comment)
    (define-key map (kbd "] p") #'aob-trace-queued-next)
    (define-key map (kbd "[ p") #'aob-trace-queued-prev)
    (define-key map "o" #'aob-compose)
    map))

(define-derived-mode aob-trace-mode special-mode "aob-trace"
  "Operation trace of one agent session."
  (ygg-ui-plain-layout)
  (setq truncate-lines nil)
  (setq-local char-property-alias-alist '((face font-lock-face)))
  (visual-line-mode 1)
  (add-to-invisibility-spec 'markdown-markup)
  (require 'markdown-mode nil t)
  ;; whatever a face inherits from, it is drawn in this family: one
  ;; proportional heading is one line that does not line up
  (let ((mono (face-attribute 'default :family nil t)))
    (dolist (f '(variable-pitch markdown-inline-code-face markdown-pre-face
                 markdown-code-face markdown-language-keyword-face
                 markdown-header-face markdown-header-face-1
                 markdown-header-face-2 markdown-header-face-3))
      (when (facep f)
        (face-remap-add-relative f :family mono))))
  (when (aob-trace--delta-p)
    (setq-local line-spacing 0.3)
    (setq-local left-margin-width 4)
    (setq-local fill-column aob-trace-measure)
    (setq-local word-wrap t)
    (add-hook 'window-configuration-change-hook #'aob-trace--fit-margins nil t)
    (add-hook 'window-buffer-change-functions
              (lambda (_frame) (aob-trace--fit-margins)) nil t)
    (aob-trace--fit-margins)))

(defvar-local aob-trace--fit-width nil
  "The width the blocks in this buffer were drawn for.")

(defun aob-trace--fit-margins ()
  "Give the trace the window, keeping the gutter the mark hangs in.
`aob-trace-measure\=' caps the line only when it is set: a measure is
worth having in a window wide enough to need one, and a window nobody
widened is not that window."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (let* ((total (window-total-width win))
           ;; the gutter costs four columns, which a narrow window does
           ;; not have to spare: the mark goes inline there instead
           (gutter (if (>= total 60) 4 0))
           (slack (if (> aob-trace-measure 0)
                      (max 0 (- total aob-trace-measure gutter))
                    0)))
      (set-window-margins win gutter slack)
      (set-window-fringes win 0 0)
      (with-current-buffer (window-buffer win)
        (setq-local fill-column (max 20 (- total gutter slack)))
        ;; a word broken in half is a window that stopped wrapping on
        ;; words; nothing here wants character wrapping
        (setq-local word-wrap t)
        (setq-local truncate-lines nil)
        ;; a card was clipped to the width it was drawn at; a window that
        ;; changed width is a window whose cards are the wrong length
        (let ((now (window-body-width win)))
          (unless (equal now aob-trace--fit-width)
            (setq aob-trace--fit-width now)
            (when-let* ((s (and aob-trace--session-id
                                (aob-session-get aob-trace--session-id))))
              (dolist (ev (aob-session-events s)) (plist-put ev :line nil))
              (setq aob-trace--blocks nil))))))))

(defcustom aob-trace-icons t
  "Draw event kinds as nerd-font glyphs (needs a Nerd Font) instead of ASCII."
  :type 'boolean :group 'aob)

(defcustom aob-trace-style 'delta
  "How the trace reads: `delta' is prose with a text measure and no
clock; `log' is the timestamped row-per-event shape."
  :type '(choice (const delta) (const log)) :group 'aob)

(defcustom aob-trace-word-space 1.0
  "How wide a space between words of prose runs, in space widths."
  :type 'number :group 'aob)

(defcustom aob-trace-paragraph-space 0.9
  "Extra height under a paragraph break, in units of the line height."
  :type 'number :group 'aob)

(defcustom aob-trace-measure 0
  "Columns of prose before the right margin takes over, under `delta'.
Zero gives the trace the whole window, less the gutter its marks hang
in: a paragraph reads better at seventy-eight columns, and a shell
command, a diff and a path do not."
  :type 'natnum :group 'aob)

(defface aob-trace-prose
  '((t :inherit default))
  "Face for message and prompt bodies under the delta style.
One family for the whole interface: a proportional face reads better
in a paragraph and worse in everything a paragraph here is made of —
paths, diffs, names, and the grid the rest of the frame stands on."
  :group 'aob)

(defface aob-trace-icon '((t :height 1.6))
  "Face sizing the gutter and row glyphs under the delta style."
  :group 'aob)

(defface aob-trace-done '((t :inherit shadow :height 0.8))
  "Face for the line that closes a turn.
The end of a turn is a footnote to it, not a heading."
  :group 'aob)

(defface aob-trace-queued '((t :inherit shadow :slant italic))
  "Face for a prompt written but not yet sent."
  :group 'aob)

(defface aob-trace-aside '((t :inherit shadow))
  "Face for thinking lines and other collapsed asides.
Dim and nothing else: there is no size axis here, and a span that is
dim and smaller is two ways of saying one thing."
  :group 'aob)

(defun aob-trace--delta-p () (eq aob-trace-style 'delta))

(defun aob-trace--stamp (time)
  "TIME as a dim prefix, or nothing at all under the delta style."
  (if (aob-trace--delta-p) "" (propertize time 'face 'shadow)))

(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-faicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")

(defun aob-trace--nf (fn name &optional face)
  "Nerd-icon NAME from family FN in FACE, or nil when icons are off/absent.
A bad glyph name degrades to nil so the caller falls back to ASCII."
  (and aob-trace-icons
       (or (featurep 'nerd-icons) (require 'nerd-icons nil t))
       (ignore-errors
         (funcall fn name :face (if (eq aob-trace-style 'delta)
                                    (list (or face 'default) 'aob-trace-icon)
                                  (or face 'default))))))

(defun aob-trace--glyph (ev)
  "One propertized glyph naming EV's kind: a nerd-icon, else an ASCII fallback.
Computed once and cached into the event's `:line', so the render-diff's
`eq' skip and the Mono font's single-cell advance both stay intact."
  (pcase (plist-get ev :type)
    ('tool (if (plist-get ev :subagent)
               (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-account_arrow_right_outline" 'shadow) "└")
             (pcase (plist-get ev :kind)
               ("read"    (or (aob-trace--nf #'nerd-icons-faicon "nf-fa-file_o" 'shadow) "→"))
               ("edit"    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-pencil" 'warning) "±"))
               ("delete"  (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-trash_can_outline" 'error) "−"))
               ("move"    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-file_move_outline" 'shadow) "↷"))
               ("search"  (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-magnify" 'shadow) "?"))
               ("execute" (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-console" 'success) "$"))
               ("think"   (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-lightbulb_outline" 'shadow) "…"))
               ("fetch"   (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-web" 'shadow) "↓"))
               (_         (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-tools" 'shadow) "•")))))
    ('message    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-message_text_outline" 'shadow) "┃"))
    ('thought    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-thought_bubble_outline" 'shadow) "∴"))
    ('prompt     (or (aob-trace--nf #'nerd-icons-octicon "nf-oct-chevron_right" 'ygg-state-insert) "❯"))
    ('permission (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-shield_key_outline" 'warning) "■"))
    ('plan       (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-format_list_checks" 'shadow) "▤"))
    ('stop       (or (and aob-trace-icons
                          (or (featurep 'nerd-icons) (require 'nerd-icons nil t))
                          (ignore-errors
                            (nerd-icons-mdicon "nf-md-stop_circle_outline"
                                               :face 'aob-trace-done)))
                     "■"))
    ('error      (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-alert_circle_outline" 'error) "✗"))
    (_           "·")))

(defun aob-trace--bound (text)
  "TEXT cut to `aob-trace-block-max-chars', ending in what was left off."
  (if (<= (length text) aob-trace-block-max-chars)
      (string-trim-right text)
    (concat (substring text 0 aob-trace-block-max-chars)
            (propertize (format "\n… %d more chars"
                                (- (length text) aob-trace-block-max-chars))
                        'face 'shadow))))

(defun aob-trace--body (ev)
  "EV's words as a block: bounded, and joined only once they have settled.
While the answer is still arriving it is drawn from the prefix
`aob-event-push-text' keeps, since joining every chunk at redraw speed
is quadratic in the length of the turn."
  (if (not (aob-trace--live-p ev))
      (aob-trace--bound (aob-event-text ev))
    (let ((head (aob-event-text-so-far ev)))
      (if (< (length head) aob-event-live-prefix)
          (string-trim-right head)
        (concat (aob-trace--bound head) (propertize "\n…" 'face 'shadow))))))

(defcustom aob-trace-live-markdown-max 1500
  "Longest answer decorated while it is still arriving, in characters.
A streaming message is re-fontified on every redraw, so its cost is
paid ten times a second; past this it waits and is decorated once, when
the turn settles."
  :type 'natnum :group 'aob)

(defun aob-trace--md (text &optional live)
  "TEXT carrying markdown faces, or TEXT as it is when that is too dear.
LIVE says the text is still arriving and will be redrawn again shortly."
  (if (and (fboundp 'ygg-ui-markdown)
           (not (and live (> (length text) aob-trace-live-markdown-max))))
      (aob-trace--diff-blocks (aob-trace--tables (ygg-ui-markdown text)))
    text))

(defun aob-trace--diff-blocks (text)
  "TEXT with the lines of each diff fence in it coloured as a diff is.
markdown-mode shades a fence as code and nothing more, and a change shown
as a diff is read by its plus and minus lines."
  (if (not (and (stringp text) (string-match-p "^[ \t]*```[ \t]*\\(?:diff\\|patch\\)" text)))
      text
    (require 'diff-mode)
    (let ((out (copy-sequence text)) (pos 0) (in nil))
      (dolist (line (split-string out "\n"))
        (let ((end (+ pos (length line))))
          (cond
           ((string-match-p "^[ \t]*```[ \t]*\\(?:diff\\|patch\\)" line) (setq in t))
           ((and in (string-match-p "^[ \t]*```" line)) (setq in nil))
           (in
            (when-let* ((face (cond ((string-prefix-p "@@" line) 'diff-hunk-header)
                                    ((string-prefix-p "+" line) 'diff-added)
                                    ((string-prefix-p "-" line) 'diff-removed))))
              (let ((i pos))
                (while (< i end)
                  (let* ((next (next-single-property-change i 'font-lock-face out end))
                         (old (get-text-property i 'font-lock-face out)))
                    (put-text-property i next 'font-lock-face
                                       (cons face (cond ((null old) nil)
                                                        ((listp old) old)
                                                        (t (list old))))
                                       out)
                    (setq i next)))))))
          (setq pos (1+ end))))
      out)))

(defconst aob-trace--table-row-re "^[ \t]*|.*|[ \t]*$"
  "A markdown table row: a line between pipes.")

(defconst aob-trace--table-rule-re "\\`[ \t]*:?-+:?[ \t]*\\'"
  "A cell of the line under a table's header: dashes, colons for alignment.")

(defun aob-trace--visible-width (str)
  "Columns STR takes on screen, what markdown hid not counted."
  (let ((w 0) (i 0) (n (length str)))
    (while (< i n)
      (unless (get-text-property i 'invisible str)
        (setq w (+ w (char-width (aref str i)))))
      (setq i (1+ i)))
    w))

(defun aob-trace--visible-cut (str width)
  "STR cut to WIDTH visible columns, an ellipsis standing for the rest."
  (if (<= (aob-trace--visible-width str) width)
      str
    (let ((w 0) (i 0) (n (length str)))
      (while (and (< i n)
                  (or (get-text-property i 'invisible str)
                      (<= (+ w (char-width (aref str i))) (1- width))))
        (unless (get-text-property i 'invisible str)
          (setq w (+ w (char-width (aref str i)))))
        (setq i (1+ i)))
      (concat (substring str 0 i) "…"))))

(defun aob-trace--table-cells (line)
  "LINE's cells, trimmed, their text properties kept."
  (let* ((body (string-trim line))
         (body (substring body (if (string-prefix-p "|" body) 1 0)
                          (if (string-suffix-p "|" body) -1 nil)))
         (cells nil) (start 0) (i 0) (n (length body)))
    (while (< i n)
      (cond ((and (eq (aref body i) ?\\) (< (1+ i) n)) (setq i (1+ i)))
            ((eq (aref body i) ?|)
             (push (substring body start i) cells)
             (setq start (1+ i))))
      (setq i (1+ i)))
    (push (substring body start) cells)
    (mapcar #'string-trim (nreverse cells))))

(defun aob-trace--table-render (rows width)
  "ROWS, a header and its cells, as aligned lines no wider than WIDTH, or nil.
The second row is the rule under the header and says each column's
alignment.  Columns are trimmed widest first when the table does not
fit; one that would not fit at ten columns each is left as written."
  (let* ((head (car rows))
         (align (mapcar (lambda (c)
                          (let ((c (substring-no-properties c)))
                            (cond ((and (string-prefix-p ":" c) (string-suffix-p ":" c)) 'center)
                                  ((string-suffix-p ":" c) 'right)
                                  (t 'left))))
                        (cadr rows)))
         (body (cddr rows))
         (ncol (apply #'max (mapcar #'length (cons head body))))
         (all (mapcar (lambda (r) (append r (make-list (- ncol (length r)) "")))
                      (cons head body)))
         (widths (mapcar (lambda (i)
                           (apply #'max 1 (mapcar (lambda (r) (aob-trace--visible-width (nth i r)))
                                                  all)))
                         (number-sequence 0 (1- ncol))))
         (frame (+ 1 (* 3 ncol))))
    (while (and (> (+ frame (apply #'+ widths)) width)
                (> (apply #'max widths) 10))
      (let ((i (seq-position widths (apply #'max widths))))
        (setf (nth i widths) (1- (nth i widths)))))
    (when (<= (+ frame (apply #'+ widths)) width)
      ;; and wider when it can: a table spans the page it is on, its spare
      ;; columns shared out by how much each column already holds
      (let* ((room (- width 1 frame))
             (sum (apply #'+ widths))
             (extra (- room sum)))
        (when (> extra 0)
          (let ((given 0))
            (setq widths (mapcar (lambda (w)
                                   (let ((add (floor (* extra w) sum)))
                                     (setq given (+ given add))
                                     (+ w add)))
                                 widths))
            (setf (car (last widths)) (+ (car (last widths)) (- extra given))))))
      (let* ((bar (propertize "│" 'font-lock-face 'shadow))
             (line (lambda (row bold)
                     (concat
                      bar
                      (mapconcat
                       (lambda (i)
                         (let* ((w (nth i widths))
                                (cell (aob-trace--visible-cut (nth i row) w))
                                (pad (- w (aob-trace--visible-width cell)))
                                (how (or (nth i align) 'left))
                                (left (pcase how ('right pad) ('center (/ pad 2)) (_ 0)))
                                (cell (if bold
                                          (let ((c (copy-sequence cell)))
                                            (add-face-text-property 0 (length c) 'bold t c)
                                            c)
                                        cell)))
                           (concat " " (make-string left ?\s) cell
                                   (make-string (- pad left) ?\s) " " bar)))
                       (number-sequence 0 (1- ncol)) ""))))
             (rule (propertize
                    (concat "├"
                            (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths "┼")
                            "┤")
                    'font-lock-face 'shadow)))
        (append (list (funcall line (car all) t) rule)
                (mapcar (lambda (r) (funcall line r nil)) (cdr all)))))))

(defun aob-trace--tables (text)
  "TEXT with each markdown table in it drawn with its columns lined up.
markdown-mode colours a table and leaves it as typed, and its hidden
markup, the stars around a bold word, pulls every cell after it out of
line."
  (if (not (and (stringp text) (string-match-p "^[ \t]*|" text)))
      text
    (let ((lines (split-string text "\n"))
          (width (aob-trace--text-width))
          out)
      (while lines
        (if (and (cdr lines)
                 (string-match-p aob-trace--table-row-re (car lines))
                 (string-match-p aob-trace--table-row-re (cadr lines))
                 (seq-every-p (lambda (c) (string-match-p aob-trace--table-rule-re
                                                          (substring-no-properties c)))
                              (aob-trace--table-cells (cadr lines))))
            (let ((block (list (pop lines) (pop lines))))
              (while (and lines (string-match-p aob-trace--table-row-re (car lines)))
                (setq block (append block (list (pop lines)))))
              (dolist (l (or (aob-trace--table-render
                              (mapcar #'aob-trace--table-cells block) width)
                             block))
                (push l out)))
          (push (pop lines) out)))
      (mapconcat #'identity (nreverse out) "\n"))))

(defun aob-trace--status (ev)
  (pcase (plist-get ev :status)
    ("queued" (propertize "⋯ queued" 'face 'shadow))
    ("pending" (propertize "⋯" 'face 'shadow))
    ("in_progress" (propertize "⟳" 'face 'warning))
    ("completed" (propertize "✓" 'face 'success))
    ("failed" (propertize "✗" 'face 'error))
    ("cancelled" (propertize "⊘ cancelled" 'face 'shadow))
    (_ "")))

(defun aob-trace--rollup (ev)
  "A Task's subagent digest: how many steps, how many running, how many
failed, what they changed.  Never what they are doing — a trace that
echoed each subagent's current step would be five agents talking over
the one you asked.  `aob-subagents' keeps them all in one place under the trace."
  (if-let* ((n (plist-get ev :children)))
      (concat
       (propertize (format " %d" n) 'face 'shadow)
       (let ((live (or (plist-get ev :child-live) 0)))
         (if (> live 0) (propertize (format "⟳%d" live) 'face 'warning) ""))
       (let ((f (or (plist-get ev :child-fail) 0)))
         (if (> f 0) (propertize (format "✗%d" f) 'face 'error) ""))
       (if-let* ((cs (plist-get ev :child-stat))) (concat " " cs) ""))
    ""))

(defcustom aob-trace-speakers t
  "Whether words get a speaker line above them rather than a timestamp.

A transcript where the agent\='s paragraphs, your own prompts and the
tool rows all begin the same way is a wall: everything is at the left
margin behind a clock, and finding where a turn starts means reading
until the voice changes.  With this on, prose carries a dim line
naming who is speaking and when, and the words start on their own
line.  Tool rows are unchanged — they are a log and read as one."
  :type 'boolean :group 'aob)

(defface aob-trace-speaker
  '((t :inherit font-lock-function-name-face :weight bold))
  "The name above a turn's words."
  :group 'aob)

(defun aob-trace--speaker-line (ev time name)
  "The line naming who EV is from, at TIME, or nil when it gets none.
NAME is the agent this session runs; the owner\='s own turns say you."
  (when aob-trace-speakers
    (let ((who (pcase (plist-get ev :type)
                 ('prompt "you")
                 ('message (or name "agent"))
                 (_ nil))))
      (when who
        (if (and (aob-trace--delta-p) (not (aob-trace--sub-p ev)))
            ""
          (concat "\n"
                  (propertize who 'font-lock-face 'aob-trace-speaker)
                  (if (aob-trace--sub-p ev)
                      (propertize " └ subagent" 'font-lock-face 'shadow)
                    "")
                  (if (aob-trace--delta-p)
                      ""
                    (propertize (concat "  " time) 'font-lock-face 'shadow))
                  "\n"))))))

(defface aob-trace-target
  '((t :inherit aob-trace-prose :underline t))
  "Face for what a tool acted on: the path, symbol or command."
  :group 'aob)

(defconst aob-trace--verbs
  '(("read" . "Read") ("edit" . "Edit") ("delete" . "Delete")
    ("move" . "Move") ("search" . "Search") ("execute" . "Run")
    ("think" . "Thinking") ("fetch" . "Fetch"))
  "Kind of tool call → the word Delta puts in front of its target.")

(defun aob-trace--mcp (ev)
  "(SERVER . TOOL) when EV called a tool an MCP server offers, else nil.
The adapter names such a call mcp__SERVER__TOOL and files it as other,
which reads as a call nobody could name."
  (when-let* ((title (plist-get ev :title))
              ((string-match "\\`mcp__\\([^_]+\\(?:_[^_]+\\)*?\\)__\\([^ ]+\\)" title)))
    (cons (match-string 1 title)
          (replace-regexp-in-string "_" " " (match-string 2 title)))))

(defun aob-trace--verb (ev)
  "The word naming what EV did: the server, for a tool an MCP server offers."
  (or (car (aob-trace--mcp ev))
      (cdr (assoc (plist-get ev :kind) aob-trace--verbs))
      (capitalize (or (plist-get ev :kind) "Call"))))

(defun aob-trace--target (ev)
  "What EV did it to: the tool's own name, for one an MCP server offers."
  (or (cdr (aob-trace--mcp ev))
      (plist-get ev :title) (plist-get ev :kind) ""))

(defface aob-trace-card
  '((((background dark)) :background "#1c1c1c" :extend t)
    (t :background "#f4f4f4" :extend t))
  "Face behind a command card."
  :group 'aob)

(defface aob-trace-tool
  '((((background dark)) :foreground "#8EA4C9")
    (t :foreground "#4A5B76"))
  "Face for what the agent ran, told apart from what it said.
A trace is two kinds of line — words, and machine work — and the eye
should not have to read one to find out which it is."
  :group 'aob)

(defface aob-trace-card-meta '((t :inherit shadow))
  "Face for the chevron and elapsed time on a command card."
  :group 'aob)

(defun aob-trace--elapsed (ev)
  "How long EV took, as Delta prints it, or nil."
  (when-let* ((done (plist-get ev :done-ts))
              (start (plist-get ev :ts))
              (ms (round (* 1000 (- done start)))))
    (if (< ms 1000) (format "%dms" ms) (format "%.1fs" (/ ms 1000.0)))))

(defcustom aob-trace-card-width 0
  "Columns a command card shows before it is clipped.
Zero follows `aob-trace-measure\='.  A card is a row in a log: what ran
and how long it took.  The command itself is under TAB, whole."
  :type 'natnum :group 'aob)

(defun aob-trace--text-width ()
  "Columns the trace has for a line of text, as it stands now."
  (let ((win (get-buffer-window (current-buffer) t)))
    (cond ((> aob-trace-card-width 0) aob-trace-card-width)
          (win (max 20 (window-body-width win)))
          ((> aob-trace-measure 0) aob-trace-measure)
          (t 78))))

(defun aob-trace--one-line (text)
  "TEXT as the single line a card has room for.
Against the window, not against the measure: the measure is off by
default, and a card measured against zero is a card twelve columns
wide."
  (let ((line (car (split-string (or text "") "\n" t)))
        (room (max 24 (- (aob-trace--text-width) 14))))
    (truncate-string-to-width (string-trim (or line "")) room)))

(defface aob-trace-tool-run
  '((((background dark)) :foreground "#B5A27E") (t :foreground "#6F6246"))
  "Face for a command the agent ran in a shell: the one tool told apart.
Muted, since a trace is read for its words and colour pulls the eye." :group 'aob)

(defface aob-trace-tool-edit
  '((t :inherit aob-trace-tool))
  "Face for a tool that changed a file: an edit, a move, a delete." :group 'aob)

(defface aob-trace-tool-search
  '((t :inherit aob-trace-tool))
  "Face for a search across the tree." :group 'aob)

(defface aob-trace-tool-fetch
  '((t :inherit aob-trace-tool))
  "Face for a fetch from the network." :group 'aob)

(defface aob-trace-tool-mcp
  '((t :inherit aob-trace-tool))
  "Face for a tool an MCP server offers." :group 'aob)

(defun aob-trace--tool-face (ev)
  "The face EV's kind of work is shown in: each kind its own colour, so a
command run reads apart from a file read at a glance."
  (cond ((aob-trace--mcp ev) 'aob-trace-tool-mcp)
        (t (pcase (plist-get ev :kind)
             ("execute" 'aob-trace-tool-run)
             ((or "edit" "delete" "move") 'aob-trace-tool-edit)
             ("search" 'aob-trace-tool-search)
             ("fetch" 'aob-trace-tool-fetch)
             (_ 'aob-trace-tool)))))

(defun aob-trace--card (ev body)
  "BODY as a command card: tinted, monospaced, its timing on the right."
  (let* ((meta (concat "› " (or (aob-trace--elapsed ev) "")))
         (head (concat
                body
                (propertize " " 'display
                            ;; a value in a table column sits one cell off
                            ;; the edge; two is the menu-shortcut rule
                            `(space :align-to (- right ,(1+ (string-width meta)))))
                (propertize meta 'font-lock-face 'aob-trace-card-meta))))
    (add-face-text-property 0 (length head) (aob-trace--tool-face ev) t head)
    (add-face-text-property 0 (length head) 'aob-trace-card t head)
    head))

(defconst aob-trace--agent-art
  "<rect x=\"76\" y=\"0\" width=\"1\" height=\"1\"/><rect x=\"75\" y=\"1\" width=\"2\" height=\"1\"/><rect x=\"65\" y=\"2\" width=\"2\" height=\"1\"/><rect x=\"76\" y=\"2\" width=\"1\" height=\"1\"/><rect x=\"68\" y=\"3\" width=\"2\" height=\"1\"/><rect x=\"78\" y=\"3\" width=\"1\" height=\"1\"/><rect x=\"69\" y=\"4\" width=\"3\" height=\"1\"/><rect x=\"78\" y=\"4\" width=\"1\" height=\"1\"/><rect x=\"70\" y=\"5\" width=\"3\" height=\"1\"/><rect x=\"78\" y=\"5\" width=\"2\" height=\"1\"/><rect x=\"58\" y=\"6\" width=\"7\" height=\"1\"/><rect x=\"71\" y=\"6\" width=\"3\" height=\"1\"/><rect x=\"78\" y=\"6\" width=\"2\" height=\"1\"/><rect x=\"54\" y=\"7\" width=\"15\" height=\"1\"/><rect x=\"71\" y=\"7\" width=\"4\" height=\"1\"/><rect x=\"79\" y=\"7\" width=\"2\" height=\"1\"/><rect x=\"10\" y=\"8\" width=\"3\" height=\"1\"/><rect x=\"35\" y=\"8\" width=\"2\" height=\"1\"/><rect x=\"53\" y=\"8\" width=\"1\" height=\"1\"/><rect x=\"63\" y=\"8\" width=\"13\" height=\"1\"/><rect x=\"79\" y=\"8\" width=\"3\" height=\"1\"/><rect x=\"9\" y=\"9\" width=\"7\" height=\"1\"/><rect x=\"20\" y=\"9\" width=\"5\" height=\"1\"/><rect x=\"34\" y=\"9\" width=\"2\" height=\"1\"/><rect x=\"62\" y=\"9\" width=\"15\" height=\"1\"/><rect x=\"79\" y=\"9\" width=\"3\" height=\"1\"/><rect x=\"9\" y=\"10\" width=\"19\" height=\"1\"/><rect x=\"31\" y=\"10\" width=\"5\" height=\"1\"/><rect x=\"58\" y=\"10\" width=\"21\" height=\"1\"/><rect x=\"80\" y=\"10\" width=\"3\" height=\"1\"/><rect x=\"8\" y=\"11\" width=\"27\" height=\"1\"/><rect x=\"56\" y=\"11\" width=\"28\" height=\"1\"/><rect x=\"8\" y=\"12\" width=\"12\" height=\"1\"/><rect x=\"24\" y=\"12\" width=\"10\" height=\"1\"/><rect x=\"54\" y=\"12\" width=\"31\" height=\"1\"/><rect x=\"8\" y=\"13\" width=\"25\" height=\"1\"/><rect x=\"53\" y=\"13\" width=\"34\" height=\"1\"/><rect x=\"13\" y=\"14\" width=\"23\" height=\"1\"/><rect x=\"52\" y=\"14\" width=\"36\" height=\"1\"/><rect x=\"15\" y=\"15\" width=\"24\" height=\"1\"/><rect x=\"51\" y=\"15\" width=\"15\" height=\"1\"/><rect x=\"73\" y=\"15\" width=\"16\" height=\"1\"/><rect x=\"16\" y=\"16\" width=\"26\" height=\"1\"/><rect x=\"50\" y=\"16\" width=\"14\" height=\"1\"/><rect x=\"75\" y=\"16\" width=\"15\" height=\"1\"/><rect x=\"17\" y=\"17\" width=\"27\" height=\"1\"/><rect x=\"49\" y=\"17\" width=\"15\" height=\"1\"/><rect x=\"77\" y=\"17\" width=\"7\" height=\"1\"/><rect x=\"87\" y=\"17\" width=\"3\" height=\"1\"/><rect x=\"18\" y=\"18\" width=\"22\" height=\"1\"/><rect x=\"49\" y=\"18\" width=\"2\" height=\"1\"/><rect x=\"53\" y=\"18\" width=\"10\" height=\"1\"/><rect x=\"77\" y=\"18\" width=\"8\" height=\"1\"/><rect x=\"87\" y=\"18\" width=\"3\" height=\"1\"/><rect x=\"19\" y=\"19\" width=\"5\" height=\"1\"/><rect x=\"30\" y=\"19\" width=\"12\" height=\"1\"/><rect x=\"48\" y=\"19\" width=\"1\" height=\"1\"/><rect x=\"52\" y=\"19\" width=\"11\" height=\"1\"/><rect x=\"78\" y=\"19\" width=\"8\" height=\"1\"/><rect x=\"88\" y=\"19\" width=\"3\" height=\"1\"/><rect x=\"20\" y=\"20\" width=\"3\" height=\"1\"/><rect x=\"32\" y=\"20\" width=\"11\" height=\"1\"/><rect x=\"52\" y=\"20\" width=\"11\" height=\"1\"/><rect x=\"78\" y=\"20\" width=\"9\" height=\"1\"/><rect x=\"88\" y=\"20\" width=\"3\" height=\"1\"/><rect x=\"33\" y=\"21\" width=\"11\" height=\"1\"/><rect x=\"52\" y=\"21\" width=\"11\" height=\"1\"/><rect x=\"78\" y=\"21\" width=\"14\" height=\"1\"/><rect x=\"34\" y=\"22\" width=\"11\" height=\"1\"/><rect x=\"52\" y=\"22\" width=\"11\" height=\"1\"/><rect x=\"78\" y=\"22\" width=\"15\" height=\"1\"/><rect x=\"34\" y=\"23\" width=\"12\" height=\"1\"/><rect x=\"52\" y=\"23\" width=\"12\" height=\"1\"/><rect x=\"78\" y=\"23\" width=\"16\" height=\"1\"/><rect x=\"35\" y=\"24\" width=\"8\" height=\"1\"/><rect x=\"45\" y=\"24\" width=\"2\" height=\"1\"/><rect x=\"52\" y=\"24\" width=\"12\" height=\"1\"/><rect x=\"79\" y=\"24\" width=\"17\" height=\"1\"/><rect x=\"35\" y=\"25\" width=\"9\" height=\"1\"/><rect x=\"46\" y=\"25\" width=\"2\" height=\"1\"/><rect x=\"52\" y=\"25\" width=\"13\" height=\"1\"/><rect x=\"81\" y=\"25\" width=\"16\" height=\"1\"/><rect x=\"36\" y=\"26\" width=\"9\" height=\"1\"/><rect x=\"47\" y=\"26\" width=\"1\" height=\"1\"/><rect x=\"52\" y=\"26\" width=\"13\" height=\"1\"/><rect x=\"86\" y=\"26\" width=\"12\" height=\"1\"/><rect x=\"36\" y=\"27\" width=\"10\" height=\"1\"/><rect x=\"52\" y=\"27\" width=\"14\" height=\"1\"/><rect x=\"89\" y=\"27\" width=\"9\" height=\"1\"/><rect x=\"36\" y=\"28\" width=\"10\" height=\"1\"/><rect x=\"52\" y=\"28\" width=\"15\" height=\"1\"/><rect x=\"77\" y=\"28\" width=\"1\" height=\"1\"/><rect x=\"90\" y=\"28\" width=\"7\" height=\"1\"/><rect x=\"36\" y=\"29\" width=\"11\" height=\"1\"/><rect x=\"52\" y=\"29\" width=\"16\" height=\"1\"/><rect x=\"77\" y=\"29\" width=\"2\" height=\"1\"/><rect x=\"92\" y=\"29\" width=\"5\" height=\"1\"/><rect x=\"36\" y=\"30\" width=\"11\" height=\"1\"/><rect x=\"52\" y=\"30\" width=\"16\" height=\"1\"/><rect x=\"78\" y=\"30\" width=\"2\" height=\"1\"/><rect x=\"93\" y=\"30\" width=\"4\" height=\"1\"/><rect x=\"36\" y=\"31\" width=\"11\" height=\"1\"/><rect x=\"52\" y=\"31\" width=\"3\" height=\"1\"/><rect x=\"56\" y=\"31\" width=\"14\" height=\"1\"/><rect x=\"79\" y=\"31\" width=\"2\" height=\"1\"/><rect x=\"93\" y=\"31\" width=\"3\" height=\"1\"/><rect x=\"35\" y=\"32\" width=\"9\" height=\"1\"/><rect x=\"45\" y=\"32\" width=\"2\" height=\"1\"/><rect x=\"53\" y=\"32\" width=\"2\" height=\"1\"/><rect x=\"57\" y=\"32\" width=\"16\" height=\"1\"/><rect x=\"80\" y=\"32\" width=\"4\" height=\"1\"/><rect x=\"94\" y=\"32\" width=\"1\" height=\"1\"/><rect x=\"35\" y=\"33\" width=\"10\" height=\"1\"/><rect x=\"46\" y=\"33\" width=\"1\" height=\"1\"/><rect x=\"53\" y=\"33\" width=\"2\" height=\"1\"/><rect x=\"58\" y=\"33\" width=\"17\" height=\"1\"/><rect x=\"81\" y=\"33\" width=\"8\" height=\"1\"/><rect x=\"35\" y=\"34\" width=\"10\" height=\"1\"/><rect x=\"46\" y=\"34\" width=\"1\" height=\"1\"/><rect x=\"54\" y=\"34\" width=\"1\" height=\"1\"/><rect x=\"59\" y=\"34\" width=\"33\" height=\"1\"/><rect x=\"35\" y=\"35\" width=\"10\" height=\"1\"/><rect x=\"46\" y=\"35\" width=\"1\" height=\"1\"/><rect x=\"54\" y=\"35\" width=\"2\" height=\"1\"/><rect x=\"60\" y=\"35\" width=\"33\" height=\"1\"/><rect x=\"34\" y=\"36\" width=\"11\" height=\"1\"/><rect x=\"55\" y=\"36\" width=\"1\" height=\"1\"/><rect x=\"60\" y=\"36\" width=\"34\" height=\"1\"/><rect x=\"34\" y=\"37\" width=\"11\" height=\"1\"/><rect x=\"57\" y=\"37\" width=\"31\" height=\"1\"/><rect x=\"91\" y=\"37\" width=\"4\" height=\"1\"/><rect x=\"34\" y=\"38\" width=\"11\" height=\"1\"/><rect x=\"56\" y=\"38\" width=\"2\" height=\"1\"/><rect x=\"63\" y=\"38\" width=\"26\" height=\"1\"/><rect x=\"92\" y=\"38\" width=\"5\" height=\"1\"/><rect x=\"9\" y=\"39\" width=\"1\" height=\"1\"/><rect x=\"33\" y=\"39\" width=\"12\" height=\"1\"/><rect x=\"55\" y=\"39\" width=\"1\" height=\"1\"/><rect x=\"63\" y=\"39\" width=\"40\" height=\"1\"/><rect x=\"10\" y=\"40\" width=\"2\" height=\"1\"/><rect x=\"21\" y=\"40\" width=\"2\" height=\"1\"/><rect x=\"33\" y=\"40\" width=\"9\" height=\"1\"/><rect x=\"43\" y=\"40\" width=\"2\" height=\"1\"/><rect x=\"60\" y=\"40\" width=\"43\" height=\"1\"/><rect x=\"11\" y=\"41\" width=\"2\" height=\"1\"/><rect x=\"19\" y=\"41\" width=\"2\" height=\"1\"/><rect x=\"33\" y=\"41\" width=\"9\" height=\"1\"/><rect x=\"43\" y=\"41\" width=\"2\" height=\"1\"/><rect x=\"58\" y=\"41\" width=\"21\" height=\"1\"/><rect x=\"81\" y=\"41\" width=\"23\" height=\"1\"/><rect x=\"11\" y=\"42\" width=\"2\" height=\"1\"/><rect x=\"17\" y=\"42\" width=\"2\" height=\"1\"/><rect x=\"32\" y=\"42\" width=\"9\" height=\"1\"/><rect x=\"43\" y=\"42\" width=\"1\" height=\"1\"/><rect x=\"57\" y=\"42\" width=\"22\" height=\"1\"/><rect x=\"83\" y=\"42\" width=\"21\" height=\"1\"/><rect x=\"11\" y=\"43\" width=\"2\" height=\"1\"/><rect x=\"15\" y=\"43\" width=\"3\" height=\"1\"/><rect x=\"32\" y=\"43\" width=\"9\" height=\"1\"/><rect x=\"42\" y=\"43\" width=\"2\" height=\"1\"/><rect x=\"56\" y=\"43\" width=\"24\" height=\"1\"/><rect x=\"84\" y=\"43\" width=\"20\" height=\"1\"/><rect x=\"10\" y=\"44\" width=\"3\" height=\"1\"/><rect x=\"14\" y=\"44\" width=\"10\" height=\"1\"/><rect x=\"32\" y=\"44\" width=\"9\" height=\"1\"/><rect x=\"42\" y=\"44\" width=\"1\" height=\"1\"/><rect x=\"55\" y=\"44\" width=\"25\" height=\"1\"/><rect x=\"84\" y=\"44\" width=\"20\" height=\"1\"/><rect x=\"10\" y=\"45\" width=\"16\" height=\"1\"/><rect x=\"32\" y=\"45\" width=\"9\" height=\"1\"/><rect x=\"54\" y=\"45\" width=\"13\" height=\"1\"/><rect x=\"70\" y=\"45\" width=\"11\" height=\"1\"/><rect x=\"85\" y=\"45\" width=\"7\" height=\"1\"/><rect x=\"100\" y=\"45\" width=\"4\" height=\"1\"/><rect x=\"10\" y=\"46\" width=\"18\" height=\"1\"/><rect x=\"32\" y=\"46\" width=\"8\" height=\"1\"/><rect x=\"53\" y=\"46\" width=\"13\" height=\"1\"/><rect x=\"71\" y=\"46\" width=\"10\" height=\"1\"/><rect x=\"86\" y=\"46\" width=\"3\" height=\"1\"/><rect x=\"102\" y=\"46\" width=\"1\" height=\"1\"/><rect x=\"9\" y=\"47\" width=\"16\" height=\"1\"/><rect x=\"27\" y=\"47\" width=\"2\" height=\"1\"/><rect x=\"32\" y=\"47\" width=\"8\" height=\"1\"/><rect x=\"53\" y=\"47\" width=\"12\" height=\"1\"/><rect x=\"71\" y=\"47\" width=\"10\" height=\"1\"/><rect x=\"9\" y=\"48\" width=\"18\" height=\"1\"/><rect x=\"32\" y=\"48\" width=\"8\" height=\"1\"/><rect x=\"52\" y=\"48\" width=\"12\" height=\"1\"/><rect x=\"72\" y=\"48\" width=\"9\" height=\"1\"/><rect x=\"99\" y=\"48\" width=\"1\" height=\"1\"/><rect x=\"8\" y=\"49\" width=\"20\" height=\"1\"/><rect x=\"32\" y=\"49\" width=\"8\" height=\"1\"/><rect x=\"51\" y=\"49\" width=\"2\" height=\"1\"/><rect x=\"55\" y=\"49\" width=\"9\" height=\"1\"/><rect x=\"72\" y=\"49\" width=\"10\" height=\"1\"/><rect x=\"98\" y=\"49\" width=\"2\" height=\"1\"/><rect x=\"7\" y=\"50\" width=\"22\" height=\"1\"/><rect x=\"32\" y=\"50\" width=\"8\" height=\"1\"/><rect x=\"51\" y=\"50\" width=\"1\" height=\"1\"/><rect x=\"54\" y=\"50\" width=\"9\" height=\"1\"/><rect x=\"72\" y=\"50\" width=\"10\" height=\"1\"/><rect x=\"89\" y=\"50\" width=\"2\" height=\"1\"/><rect x=\"98\" y=\"50\" width=\"2\" height=\"1\"/><rect x=\"6\" y=\"51\" width=\"10\" height=\"1\"/><rect x=\"19\" y=\"51\" width=\"11\" height=\"1\"/><rect x=\"32\" y=\"51\" width=\"9\" height=\"1\"/><rect x=\"54\" y=\"51\" width=\"9\" height=\"1\"/><rect x=\"73\" y=\"51\" width=\"9\" height=\"1\"/><rect x=\"90\" y=\"51\" width=\"4\" height=\"1\"/><rect x=\"97\" y=\"51\" width=\"3\" height=\"1\"/><rect x=\"6\" y=\"52\" width=\"2\" height=\"1\"/><rect x=\"9\" y=\"52\" width=\"6\" height=\"1\"/><rect x=\"20\" y=\"52\" width=\"11\" height=\"1\"/><rect x=\"32\" y=\"52\" width=\"9\" height=\"1\"/><rect x=\"53\" y=\"52\" width=\"10\" height=\"1\"/><rect x=\"73\" y=\"52\" width=\"9\" height=\"1\"/><rect x=\"93\" y=\"52\" width=\"3\" height=\"1\"/><rect x=\"98\" y=\"52\" width=\"2\" height=\"1\"/><rect x=\"5\" y=\"53\" width=\"3\" height=\"1\"/><rect x=\"9\" y=\"53\" width=\"5\" height=\"1\"/><rect x=\"21\" y=\"53\" width=\"8\" height=\"1\"/><rect x=\"30\" y=\"53\" width=\"1\" height=\"1\"/><rect x=\"33\" y=\"53\" width=\"8\" height=\"1\"/><rect x=\"53\" y=\"53\" width=\"10\" height=\"1\"/><rect x=\"73\" y=\"53\" width=\"9\" height=\"1\"/><rect x=\"90\" y=\"53\" width=\"11\" height=\"1\"/><rect x=\"5\" y=\"54\" width=\"2\" height=\"1\"/><rect x=\"9\" y=\"54\" width=\"6\" height=\"1\"/><rect x=\"21\" y=\"54\" width=\"8\" height=\"1\"/><rect x=\"33\" y=\"54\" width=\"8\" height=\"1\"/><rect x=\"53\" y=\"54\" width=\"10\" height=\"1\"/><rect x=\"73\" y=\"54\" width=\"9\" height=\"1\"/><rect x=\"88\" y=\"54\" width=\"13\" height=\"1\"/><rect x=\"6\" y=\"55\" width=\"1\" height=\"1\"/><rect x=\"8\" y=\"55\" width=\"7\" height=\"1\"/><rect x=\"21\" y=\"55\" width=\"9\" height=\"1\"/><rect x=\"33\" y=\"55\" width=\"9\" height=\"1\"/><rect x=\"52\" y=\"55\" width=\"12\" height=\"1\"/><rect x=\"73\" y=\"55\" width=\"9\" height=\"1\"/><rect x=\"86\" y=\"55\" width=\"16\" height=\"1\"/><rect x=\"5\" y=\"56\" width=\"10\" height=\"1\"/><rect x=\"21\" y=\"56\" width=\"9\" height=\"1\"/><rect x=\"33\" y=\"56\" width=\"9\" height=\"1\"/><rect x=\"52\" y=\"56\" width=\"12\" height=\"1\"/><rect x=\"72\" y=\"56\" width=\"10\" height=\"1\"/><rect x=\"85\" y=\"56\" width=\"18\" height=\"1\"/><rect x=\"5\" y=\"57\" width=\"9\" height=\"1\"/><rect x=\"21\" y=\"57\" width=\"10\" height=\"1\"/><rect x=\"34\" y=\"57\" width=\"9\" height=\"1\"/><rect x=\"52\" y=\"57\" width=\"13\" height=\"1\"/><rect x=\"72\" y=\"57\" width=\"10\" height=\"1\"/><rect x=\"84\" y=\"57\" width=\"2\" height=\"1\"/><rect x=\"87\" y=\"57\" width=\"17\" height=\"1\"/><rect x=\"4\" y=\"58\" width=\"8\" height=\"1\"/><rect x=\"21\" y=\"58\" width=\"10\" height=\"1\"/><rect x=\"34\" y=\"58\" width=\"10\" height=\"1\"/><rect x=\"52\" y=\"58\" width=\"14\" height=\"1\"/><rect x=\"72\" y=\"58\" width=\"10\" height=\"1\"/><rect x=\"86\" y=\"58\" width=\"19\" height=\"1\"/><rect x=\"4\" y=\"59\" width=\"6\" height=\"1\"/><rect x=\"21\" y=\"59\" width=\"10\" height=\"1\"/><rect x=\"35\" y=\"59\" width=\"9\" height=\"1\"/><rect x=\"53\" y=\"59\" width=\"14\" height=\"1\"/><rect x=\"72\" y=\"59\" width=\"10\" height=\"1\"/><rect x=\"85\" y=\"59\" width=\"9\" height=\"1\"/><rect x=\"96\" y=\"59\" width=\"6\" height=\"1\"/><rect x=\"103\" y=\"59\" width=\"2\" height=\"1\"/><rect x=\"3\" y=\"60\" width=\"6\" height=\"1\"/><rect x=\"21\" y=\"60\" width=\"10\" height=\"1\"/><rect x=\"35\" y=\"60\" width=\"10\" height=\"1\"/><rect x=\"53\" y=\"60\" width=\"14\" height=\"1\"/><rect x=\"71\" y=\"60\" width=\"10\" height=\"1\"/><rect x=\"85\" y=\"60\" width=\"8\" height=\"1\"/><rect x=\"98\" y=\"60\" width=\"4\" height=\"1\"/><rect x=\"104\" y=\"60\" width=\"2\" height=\"1\"/><rect x=\"2\" y=\"61\" width=\"6\" height=\"1\"/><rect x=\"20\" y=\"61\" width=\"11\" height=\"1\"/><rect x=\"36\" y=\"61\" width=\"10\" height=\"1\"/><rect x=\"53\" y=\"61\" width=\"2\" height=\"1\"/><rect x=\"56\" y=\"61\" width=\"12\" height=\"1\"/><rect x=\"71\" y=\"61\" width=\"10\" height=\"1\"/><rect x=\"84\" y=\"61\" width=\"8\" height=\"1\"/><rect x=\"98\" y=\"61\" width=\"5\" height=\"1\"/><rect x=\"104\" y=\"61\" width=\"2\" height=\"1\"/><rect x=\"2\" y=\"62\" width=\"5\" height=\"1\"/><rect x=\"20\" y=\"62\" width=\"8\" height=\"1\"/><rect x=\"29\" y=\"62\" width=\"2\" height=\"1\"/><rect x=\"36\" y=\"62\" width=\"11\" height=\"1\"/><rect x=\"53\" y=\"62\" width=\"2\" height=\"1\"/><rect x=\"57\" y=\"62\" width=\"12\" height=\"1\"/><rect x=\"70\" y=\"62\" width=\"11\" height=\"1\"/><rect x=\"84\" y=\"62\" width=\"8\" height=\"1\"/><rect x=\"98\" y=\"62\" width=\"8\" height=\"1\"/><rect x=\"3\" y=\"63\" width=\"4\" height=\"1\"/><rect x=\"19\" y=\"63\" width=\"9\" height=\"1\"/><rect x=\"29\" y=\"63\" width=\"1\" height=\"1\"/><rect x=\"37\" y=\"63\" width=\"12\" height=\"1\"/><rect x=\"54\" y=\"63\" width=\"1\" height=\"1\"/><rect x=\"58\" y=\"63\" width=\"23\" height=\"1\"/><rect x=\"83\" y=\"63\" width=\"9\" height=\"1\"/><rect x=\"97\" y=\"63\" width=\"10\" height=\"1\"/><rect x=\"4\" y=\"64\" width=\"2\" height=\"1\"/><rect x=\"19\" y=\"64\" width=\"8\" height=\"1\"/><rect x=\"29\" y=\"64\" width=\"1\" height=\"1\"/><rect x=\"38\" y=\"64\" width=\"12\" height=\"1\"/><rect x=\"54\" y=\"64\" width=\"2\" height=\"1\"/><rect x=\"58\" y=\"64\" width=\"23\" height=\"1\"/><rect x=\"84\" y=\"64\" width=\"8\" height=\"1\"/><rect x=\"98\" y=\"64\" width=\"10\" height=\"1\"/><rect x=\"19\" y=\"65\" width=\"8\" height=\"1\"/><rect x=\"39\" y=\"65\" width=\"13\" height=\"1\"/><rect x=\"55\" y=\"65\" width=\"1\" height=\"1\"/><rect x=\"59\" y=\"65\" width=\"22\" height=\"1\"/><rect x=\"84\" y=\"65\" width=\"8\" height=\"1\"/><rect x=\"101\" y=\"65\" width=\"8\" height=\"1\"/><rect x=\"19\" y=\"66\" width=\"8\" height=\"1\"/><rect x=\"39\" y=\"66\" width=\"14\" height=\"1\"/><rect x=\"60\" y=\"66\" width=\"21\" height=\"1\"/><rect x=\"84\" y=\"66\" width=\"8\" height=\"1\"/><rect x=\"103\" y=\"66\" width=\"6\" height=\"1\"/><rect x=\"19\" y=\"67\" width=\"8\" height=\"1\"/><rect x=\"40\" y=\"67\" width=\"15\" height=\"1\"/><rect x=\"60\" y=\"67\" width=\"21\" height=\"1\"/><rect x=\"84\" y=\"67\" width=\"9\" height=\"1\"/><rect x=\"105\" y=\"67\" width=\"4\" height=\"1\"/><rect x=\"19\" y=\"68\" width=\"8\" height=\"1\"/><rect x=\"42\" y=\"68\" width=\"16\" height=\"1\"/><rect x=\"60\" y=\"68\" width=\"21\" height=\"1\"/><rect x=\"84\" y=\"68\" width=\"9\" height=\"1\"/><rect x=\"106\" y=\"68\" width=\"3\" height=\"1\"/><rect x=\"19\" y=\"69\" width=\"8\" height=\"1\"/><rect x=\"43\" y=\"69\" width=\"37\" height=\"1\"/><rect x=\"84\" y=\"69\" width=\"10\" height=\"1\"/><rect x=\"106\" y=\"69\" width=\"3\" height=\"1\"/><rect x=\"19\" y=\"70\" width=\"9\" height=\"1\"/><rect x=\"44\" y=\"70\" width=\"36\" height=\"1\"/><rect x=\"84\" y=\"70\" width=\"2\" height=\"1\"/><rect x=\"87\" y=\"70\" width=\"8\" height=\"1\"/><rect x=\"19\" y=\"71\" width=\"9\" height=\"1\"/><rect x=\"45\" y=\"71\" width=\"35\" height=\"1\"/><rect x=\"85\" y=\"71\" width=\"1\" height=\"1\"/><rect x=\"87\" y=\"71\" width=\"8\" height=\"1\"/><rect x=\"19\" y=\"72\" width=\"10\" height=\"1\"/><rect x=\"46\" y=\"72\" width=\"33\" height=\"1\"/><rect x=\"85\" y=\"72\" width=\"1\" height=\"1\"/><rect x=\"88\" y=\"72\" width=\"7\" height=\"1\"/><rect x=\"19\" y=\"73\" width=\"11\" height=\"1\"/><rect x=\"47\" y=\"73\" width=\"32\" height=\"1\"/><rect x=\"86\" y=\"73\" width=\"1\" height=\"1\"/><rect x=\"88\" y=\"73\" width=\"7\" height=\"1\"/><rect x=\"20\" y=\"74\" width=\"12\" height=\"1\"/><rect x=\"47\" y=\"74\" width=\"32\" height=\"1\"/><rect x=\"88\" y=\"74\" width=\"7\" height=\"1\"/><rect x=\"20\" y=\"75\" width=\"14\" height=\"1\"/><rect x=\"47\" y=\"75\" width=\"31\" height=\"1\"/><rect x=\"88\" y=\"75\" width=\"7\" height=\"1\"/><rect x=\"21\" y=\"76\" width=\"17\" height=\"1\"/><rect x=\"47\" y=\"76\" width=\"31\" height=\"1\"/><rect x=\"88\" y=\"76\" width=\"7\" height=\"1\"/><rect x=\"22\" y=\"77\" width=\"23\" height=\"1\"/><rect x=\"46\" y=\"77\" width=\"31\" height=\"1\"/><rect x=\"88\" y=\"77\" width=\"7\" height=\"1\"/><rect x=\"22\" y=\"78\" width=\"55\" height=\"1\"/><rect x=\"87\" y=\"78\" width=\"8\" height=\"1\"/><rect x=\"23\" y=\"79\" width=\"53\" height=\"1\"/><rect x=\"85\" y=\"79\" width=\"9\" height=\"1\"/><rect x=\"24\" y=\"80\" width=\"52\" height=\"1\"/><rect x=\"83\" y=\"80\" width=\"11\" height=\"1\"/><rect x=\"25\" y=\"81\" width=\"50\" height=\"1\"/><rect x=\"78\" y=\"81\" width=\"15\" height=\"1\"/><rect x=\"27\" y=\"82\" width=\"65\" height=\"1\"/><rect x=\"29\" y=\"83\" width=\"62\" height=\"1\"/><rect x=\"30\" y=\"84\" width=\"60\" height=\"1\"/><rect x=\"26\" y=\"85\" width=\"63\" height=\"1\"/><rect x=\"16\" y=\"86\" width=\"5\" height=\"1\"/><rect x=\"27\" y=\"86\" width=\"61\" height=\"1\"/><rect x=\"90\" y=\"86\" width=\"1\" height=\"1\"/><rect x=\"15\" y=\"87\" width=\"7\" height=\"1\"/><rect x=\"28\" y=\"87\" width=\"62\" height=\"1\"/><rect x=\"14\" y=\"88\" width=\"10\" height=\"1\"/><rect x=\"28\" y=\"88\" width=\"62\" height=\"1\"/><rect x=\"92\" y=\"88\" width=\"3\" height=\"1\"/><rect x=\"104\" y=\"88\" width=\"6\" height=\"1\"/><rect x=\"13\" y=\"89\" width=\"77\" height=\"1\"/><rect x=\"91\" y=\"89\" width=\"6\" height=\"1\"/><rect x=\"104\" y=\"89\" width=\"2\" height=\"1\"/><rect x=\"109\" y=\"89\" width=\"2\" height=\"1\"/><rect x=\"16\" y=\"90\" width=\"83\" height=\"1\"/><rect x=\"104\" y=\"90\" width=\"2\" height=\"1\"/><rect x=\"107\" y=\"90\" width=\"1\" height=\"1\"/><rect x=\"110\" y=\"90\" width=\"2\" height=\"1\"/><rect x=\"1\" y=\"91\" width=\"2\" height=\"1\"/><rect x=\"7\" y=\"91\" width=\"3\" height=\"1\"/><rect x=\"16\" y=\"91\" width=\"86\" height=\"1\"/><rect x=\"105\" y=\"91\" width=\"2\" height=\"1\"/><rect x=\"109\" y=\"91\" width=\"3\" height=\"1\"/><rect x=\"0\" y=\"92\" width=\"2\" height=\"1\"/><rect x=\"7\" y=\"92\" width=\"99\" height=\"1\"/><rect x=\"107\" y=\"92\" width=\"5\" height=\"1\"/><rect x=\"0\" y=\"93\" width=\"112\" height=\"1\"/><rect x=\"0\" y=\"94\" width=\"111\" height=\"1\"/>"
  "The agent's mark as SVG shapes, on a 112x95 grid.")

(defconst aob-trace--agent-art-size '(112 . 95)
  "The grid `aob-trace--agent-art' is drawn on.")

(defcustom aob-trace-agent-mark-scale 0.62
  "How much of a line's height the agent's mark takes."
  :type 'number :group 'aob)

(defvar aob-trace--agent-image nil
  "Cached mark, as (HEIGHT COLOUR . IMAGE): both change under a theme.")

(defun aob-trace--agent-icon ()
  "The agent's mark, drawn to fit one line, or nil without image support."
  (when (and aob-trace-icons (display-graphic-p)
             (image-type-available-p 'svg))
    (let* ((lh (max 6 (round (* aob-trace-agent-mark-scale
                                (default-line-height)))))
           (colour (or (face-attribute 'aob-trace-speaker :foreground nil t)
                       "white"))
           (key (cons lh colour)))
      (unless (equal (car-safe aob-trace--agent-image) key)
        (let* ((gw (car aob-trace--agent-art-size))
               (gh (cdr aob-trace--agent-art-size))
               ;; the whole animal, inside the line it stands beside
               (h lh)
               (w (max 1 (round (* gw (/ (float h) gh))))))
          (setq aob-trace--agent-image
                (cons key
                      (ignore-errors
                        (create-image
                         (format (concat "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                                         "width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\" "
                                         "fill=\"%s\" shape-rendering=\"crispEdges\">%s</svg>")
                                 w h gw gh colour aob-trace--agent-art)
                         'svg t :ascent 'center))))))
      (cdr aob-trace--agent-image))))

(defun aob-trace--agent-mark ()
  "What stands for the agent in the gutter: its own mark, else a glyph.
An image is returned as itself rather than wrapped in a string: the
margin takes a display spec, and a `display' property nested inside a
margin string is never looked at."
  (or (aob-trace--agent-icon)
      (aob-trace--nf #'nerd-icons-mdicon "nf-md-triangle_outline"
                     'aob-trace-speaker)
      ""))

(defun aob-trace--avatar-glyph ()
  "The agent's gutter mark."
  (aob-trace--agent-mark))

(defun aob-trace--avatar (ev)
  "A glyph standing in for whoever EV is from, or an empty string.
One mark per turn, beside the row that opens it: an agent that answers
in eight parts is one agent speaking once, and eight marks down the
gutter say eight."
  (cond ((not (aob-trace--delta-p)) "")
        ;; a prompt a tool or a workflow put there is nobody standing at
        ;; the keyboard, and a face beside it says someone was
        ;; a prompt sent from here says whether it was typed; one read
        ;; back from a written conversation says nothing, and is yours
        ((and (eq (plist-get ev :type) 'prompt)
              (or (plist-get ev :typed) (not (plist-member ev :typed))))
         (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-account_circle"
                            'aob-trace-speaker)
             ""))
        ((plist-get ev :turn-head) (aob-trace--agent-mark))
        (t "")))

(defun aob-trace--line (ev &optional name)
  (let ((time (format-time-string "%H:%M:%S" (plist-get ev :ts))))
    (if-let* ((head (aob-trace--speaker-line ev time name)))
        (concat head
                (aob-trace--gutter
                 (aob-trace--avatar ev)
                 ;; a prompt still waiting to go out is a draft, not
                 ;; something that was said: it reads as one until it is
                 (if (equal (plist-get ev :status) "queued")
                     (propertize (aob-trace--bound (aob-event-text ev))
                                 'font-lock-face 'aob-trace-queued)
                   (aob-trace--prose
                    (aob-trace--md (aob-trace--body ev)
                                   (aob-trace--live-p ev)))))
                (aob-trace--images-of ev)
                (let ((st (aob-trace--status ev)))
                  (if (string-empty-p st) "" (concat " " st))))
      (aob-trace--plain-line ev time))))

(defun aob-trace--gutter (glyph str)
  "STR with GLYPH hung in the left margin beside its first line.
Delta keeps every line of a block on one left edge and puts the speaker
mark outside it; a margin display does the same without indenting text."
  (if (or (not (aob-trace--delta-p))
          (null glyph)
          (and (stringp glyph) (string-empty-p glyph))
          (string-empty-p str))
      str
    (if (aob-trace--gutter-p)
        (concat (propertize " " 'display `((margin left-margin) ,glyph))
                str)
      ;; no margin to hang it in — a narrow window has none — so it
      ;; goes on the line rather than nowhere
      (concat (if (stringp glyph) glyph (propertize " " 'display glyph))
              " " str))))

(defun aob-trace--gutter-p ()
  "Whether a window showing this buffer has a margin to hang a mark in."
  (let ((win (get-buffer-window (current-buffer) t)))
    (if win (> (or (car (window-margins win)) 0) 0) t)))

(defun aob-trace--prose (str)
  "STR in the prose face, applied as a property so a window-level
remap such as `ygg-focus-dim' cannot outrank it."
  (if (not (aob-trace--delta-p))
      str
    (let ((copy (copy-sequence str)))
      ;; markdown arrives as `font-lock-face'; putting prose in `face'
      ;; does not merge with it, it hides it — the alias is a fallback
      ;; for when `face' is absent.  Prose goes under the same property,
      ;; last, so bold and code keep what they set and inherit the rest.
      (let ((i 0) (len (length copy)))
        (while (< i len)
          (let* ((next (next-single-property-change i 'font-lock-face copy len))
                 (cur (get-text-property i 'font-lock-face copy)))
            (put-text-property i next 'font-lock-face
                               (append (cond ((null cur) nil)
                                             ((listp cur) cur)
                                             (t (list cur)))
                                       (list 'aob-trace-prose))
                               copy)
            (setq i next))))
      ;; a paragraph break is a blank line, so the extra height goes on
      ;; the newline that opens one.  On every newline it is not
      ;; paragraph air at all — it is a third again the height of every
      ;; wrapped line in the answer
      (let ((i 0) (len (length copy)))
        (while (< i len)
          (when (and (eq (aref copy i) ?\n)
                     (< (1+ i) len)
                     (eq (aref copy (1+ i)) ?\n))
            (put-text-property i (1+ i) 'line-spacing
                               aob-trace-paragraph-space copy))
          (setq i (1+ i))))
      (when (> aob-trace-word-space 1)
        (let ((i 0) (len (length copy)))
          (while (< i len)
            (when (eq (aref copy i) ?\s)
              (put-text-property i (1+ i) 'display
                                 `(space :relative-width ,aob-trace-word-space)
                                 copy))
            (setq i (1+ i)))))
      copy)))

(defcustom aob-trace-image-height 160
  "How tall a picture sent to an agent is drawn in the trace, in pixels."
  :type 'natnum :group 'aob)

(defvar aob-trace--images (make-hash-table :test 'equal)
  "(FILE HEIGHT . MTIME) to the image drawn for it.")

(defun aob-trace--thumb (file)
  "FILE as a picture, or its name when there is nothing to draw it with."
  (or (when (and aob-trace-icons (display-graphic-p)
                 (stringp file) (file-readable-p file))
        (let* ((mtime (file-attribute-modification-time (file-attributes file)))
               (key (list file aob-trace-image-height mtime))
               (img (gethash key aob-trace--images 'miss)))
          (when (eq img 'miss)
            (setq img (ignore-errors
                        (create-image file nil nil
                                      :max-height aob-trace-image-height
                                      :max-width 600
                                      :ascent 'center)))
            (puthash key img aob-trace--images))
          (when img (propertize "[[Image]]" 'display img))))
      (and (stringp file) (format "[[%s]]" (file-name-nondirectory file)))))

(defun aob-trace--images-of (ev)
  "The pictures EV carries, shown where they can be and named where not."
  (when-let* ((n (plist-get ev :images)) ((> n 0)))
    (let ((files (plist-get ev :image-files)))
      (concat " " (string-join
                   (if files
                       (delq nil (mapcar #'aob-trace--thumb files))
                     (make-list n "[[Image]]"))
                   " ")))))

(defun aob-trace--hang (ev str)
  "STR with the agent\='s mark beside its first line when EV opens a turn.
A turn that starts with a command run is a turn nobody appeared to
take, so the mark goes where the turn does and not only where it
speaks."
  (if (not (and (aob-trace--delta-p) (plist-get ev :turn-head)
                (stringp str) (not (string-empty-p str))))
      str
    (let ((i 0))
      (while (and (< i (length str)) (eq (aref str i) ?\n))
        (setq i (1+ i)))
      (concat (substring str 0 i)
              (aob-trace--gutter (aob-trace--avatar-glyph) (substring str i))))))

(defun aob-trace--plain-line (ev time)
  "EV as one row behind its clock: the shape a log reads in."
  (progn
    (pcase (plist-get ev :type)
      ('tool
       (aob-trace--hang
        ev
        (if (aob-trace--delta-p)
           (if (equal (plist-get ev :kind) "execute")
               (aob-trace--card
                ev (aob-trace--one-line
                    (or (plist-get ev :title) (plist-get ev :kind) "")))
           (concat
            (propertize (concat (if (aob-trace--sub-p ev) "└ " "")
                                (aob-trace--verb ev) " ")
                        'font-lock-face (aob-trace--tool-face ev))
            (propertize (aob-trace--target ev)
                        'font-lock-face 'aob-trace-target)
            (let ((st (plist-get ev :status)))
              (pcase st
                ((or "completed" "success" 'nil) "")
                (_ (let ((mark (aob-trace--status ev)))
                     (if (string-empty-p mark) "" (concat " " mark))))))
            (aob-trace--rollup ev)))
         (concat
              (aob-trace--glyph ev) " "
              (concat
              ;; the glyph already says read/edit/run: naming the kind
              ;; again costs a column and reads like a log, not a buffer
              (string-join
               (delq nil (list (let ((st (aob-trace--stamp time)))
                                 (unless (string-empty-p st) st))
                               ;; subagent work nests under its Task
                               (and (aob-trace--sub-p ev) " └")
                               (or (plist-get ev :title) (plist-get ev :kind))
                               (let ((st (aob-trace--status ev)))
                                 (unless (string-empty-p st) st))
                               (plist-get ev :stat)))
               " ")
              (aob-trace--rollup ev))))))
      (_ (let ((st (aob-trace--status ev)))
           (funcall
            (cond
             ;; a thought is the agent speaking: Delta hangs its mark in
             ;; the gutter and leaves the line itself clean
             ((and (aob-trace--delta-p) (eq (plist-get ev :type) 'thought)
                   (plist-get ev :turn-head))
              (lambda (str) (aob-trace--gutter (aob-trace--avatar-glyph) str)))
             ;; and the row that opens a turn carries the mark too, kind
             ;; glyph and all: a turn that starts with a command run is a
             ;; turn nobody appeared to take
             ((and (aob-trace--delta-p) (plist-get ev :turn-head))
              (lambda (str) (aob-trace--gutter (aob-trace--avatar-glyph)
                                               (concat (aob-trace--glyph ev) " " str))))
             (t (lambda (str) (concat (aob-trace--glyph ev) " " str))))
            (format "%s%s%s%s"
                   (let ((stamp (aob-trace--stamp time)))
                     (if (string-empty-p stamp) "" (concat stamp " ")))
                   ;; a subagent's own words, in its own trace
                   (if (aob-trace--sub-p ev) "└ " "")
                   ;; narrative is the point of the trace: messages and
                   ;; prompts show whole (visual-line wraps them); thoughts
                   ;; stay a head, TAB expands.  The glyph column already
                   ;; names the type — no »/… prefix doubling
                   (pcase (plist-get ev :type)
                     ((or 'message 'prompt)
                      (concat
                       ;; a prompt still waiting to go is not something
                       ;; that was said: it reads as a draft until it is
                       (if (equal (plist-get ev :status) "queued")
                           (propertize (aob-trace--bound (aob-event-text ev))
                                       'font-lock-face 'aob-trace-queued)
                         (aob-trace--prose
                          (aob-trace--md (aob-trace--body ev)
                                         (aob-trace--live-p ev))))
                       (aob-trace--images-of ev)))
                     ('thought
                      (if (aob-trace--delta-p)
                          (propertize (concat "Thinking: " (aob-event-head ev) " ›")
                                      'font-lock-face 'aob-trace-aside)
                        (aob-event-head ev)))
                     ('stop (propertize
                             (let ((meter (aob-turn-meter
                                           ev (when-let* ((s (aob-session-get aob-trace--session-id)))
                                                (aob-session-ref s :cost-currency)))))
                               (if (string-empty-p meter)
                                   (aob-event-summary ev)
                                 (concat (aob-event-summary ev) " · " meter)))
                             'font-lock-face 'aob-trace-done))
                     (_ (aob-event-summary ev)))
                   (if (string-empty-p st) "" (concat " " st)))))))))

(defun aob-trace--detail (ev)
  (or (pcase (plist-get ev :type)
        ('tool
         (or (mapconcat
              (lambda (c)
                (pcase (plist-get c :type)
                  ("diff" (format "--- %s\n%s"
                                  (plist-get c :path)
                                  (or (plist-get c :newText) "")))
                  ("content" (or (plist-get (plist-get c :content) :text) ""))
                  (_ nil)))
              (plist-get ev :content) "\n")
             (when-let* ((raw (plist-get ev :rawOutput)))
               (format "%S" raw))))
        ((or 'message 'thought 'prompt 'error)
         (aob-trace--md (aob-event-text ev)))
        ('plan (mapconcat
                (lambda (e) (format "%s %s"
                                    (aob-plan--glyph (plist-get e :status))
                                    (plist-get e :content)))
                (plist-get ev :entries) "\n")))
      ""))

(defun aob-trace--line-cached (s ev)
  "EV's fully rendered, propertized line — recomputed only after the
event mutated (chunk pushes, tool updates clear the cache), or after
the width it was clipped against changed, which is the same thing to a
row that no longer fits."
  (let ((width (aob-trace--text-width)))
    (unless (equal (plist-get ev :line-width) width)
      (plist-put ev :line-width width)
      (plist-put ev :line nil)))
  (or (plist-get ev :line)
      (let ((l (propertize (aob-trace--line
                            ev (and (fboundp 'aob-session-ref)
                                    (aob-session-ref s :agent)))
                           'aob-session (aob-session-id s)
                           'aob-event (plist-get ev :seq))))
        (plist-put ev :line l)
        l)))

(defcustom aob-trace-thinking-autohide t
  "Show a thought whole while the agent is still thinking.
As soon as anything newer arrives it folds back to its one-line head, so
the trace carries the thinking that is happening rather than every
thought the turn went through."
  :type 'boolean :group 'aob)

(defvar-local aob-trace--live-seq nil
  "Seq of the event this buffer is watching arrive, or nil.")

(defun aob-trace--live-p (ev)
  "Whether EV is the one still being written."
  (and aob-trace--live-seq (eql aob-trace--live-seq (plist-get ev :seq))))

(defun aob-trace--live-thought-p (s ev)
  "Non-nil when EV is the thought S is having right now.
S's events are newest-first.  A subagent's step is not S moving on from
its own thought, so the comparison skips children, as the trace does."
  (and aob-trace-thinking-autohide
       (eq (plist-get ev :type) 'thought)
       (eq ev (seq-find (lambda (e) (not (aob-trace--sub-p e)))
                        (aob-session-events s)))))

(defun aob-trace--block (s ev)
  "EV's rendered block: its line, plus children and detail when expanded.
A collapsed block IS the cached line string, so an unchanged event stays
`eq' across renders and the incremental pass skips it."
  (if (not (or (memq (plist-get ev :seq) aob-trace--expanded)
               (aob-trace--live-thought-p s ev)))
      (if (plist-get ev :decision-kind)
          (aob-trace--decision s ev (aob-trace--line-cached s ev))
        (aob-trace--annotate s ev (aob-trace--questions s ev (aob-trace--line-cached s ev))))
    ;; subagent steps are NOT inlined here — they have their own trace
    ;; (the row under the trace, `aob-subagents'); an expanded Task shows detail
    (concat (aob-trace--line-cached s ev) "\n"
            (aob-trace--detail-block s ev))))

(defcustom aob-trace-explore-kinds '("read" "search")
  "Tool kinds that look at the tree without changing it.
A run of them is one step of looking around, and a trace that gives each
its own row buries the turn that follows under file names."
  :type '(repeat string) :group 'aob)

(defcustom aob-trace-explore-min 3
  "How long a run of looking has to be before it folds into one row."
  :type 'natnum :group 'aob)

(defcustom aob-trace-explore-width 64
  "How much of what a folded run looked at its row is allowed to carry."
  :type 'natnum :group 'aob)

(defconst aob-trace-explore-heading "Explored"
  "What a folded run of looking calls itself.")

(defun aob-trace--explores-p (ev)
  "Non-nil when EV is a finished look at the tree and nothing more.
Only finished: a read still running is the one thing on the screen worth
watching, and folding it away hides the trace exactly when it is live."
  (and (eq (plist-get ev :type) 'tool)
       (not (plist-get ev :subagent))
       (member (plist-get ev :kind) aob-trace-explore-kinds)
       (equal (plist-get ev :status) "completed")))

(defun aob-trace--explore-name (ev)
  "What EV looked at, short enough to sit beside its neighbours.
A path is its last segment — the folder is the same for all of them and
saying it once per row is how the line stops fitting.  Anything with a
space in it is a phrase, not a path, and is left alone."
  (let ((title (string-trim (or (plist-get ev :title) (plist-get ev :kind) ""))))
    (if (string-match-p "[ \t]" title)
        title
      (file-name-nondirectory (directory-file-name title)))))

(defun aob-trace--explore-line (evs)
  "The one row EVS fold into: when it happened, and what it read."
  (let* ((names (seq-uniq (mapcar #'aob-trace--explore-name evs)))
         (text (string-join names ", "))
         (text (if (<= (length text) aob-trace-explore-width)
                   text
                 (concat (substring text 0 (1- aob-trace-explore-width)) "⋯"))))
    (string-join
     (list (propertize (format-time-string "%H:%M:%S" (plist-get (car evs) :ts))
                       'face 'shadow)
           (aob-trace--glyph (car evs))
           aob-trace-explore-heading
           (propertize (format "%d" (length evs)) 'face 'shadow)
           (propertize text 'face 'shadow))
     " ")))

(defun aob-trace--stamp-eq (a b)
  "Non-nil when stamps A and B are made of the same objects.
The parts of a stamp are the cached lines themselves, which are replaced
rather than edited when an event changes, so comparing them as objects
says whether anything moved without reading a character of them."
  (and (eq (car a) (car b))
       (let ((x (cdr a)) (y (cdr b)))
         (while (and x y (eq (car x) (car y))) (pop x) (pop y))
         (and (null x) (null y)))))

(defun aob-trace--explore-block (s evs)
  "EVS as one row, plus what it folded when that row is expanded.
Kept on the first event so an unchanged run stays the same string across
renders and the incremental pass skips it, as a single event does."
  (let* ((head (car evs))
         (lines (mapcar (lambda (ev) (aob-trace--line-cached s ev)) evs))
         (open (and (memq (plist-get head :seq) aob-trace--expanded) t))
         (stamp (cons open lines))
         (cached (plist-get head :group)))
    (if (and cached (aob-trace--stamp-eq (car cached) stamp))
        (cdr cached)
      (let ((block (propertize
                    (if open
                        (concat (aob-trace--explore-line evs) "\n"
                                (mapconcat (lambda (l) (concat "    " l))
                                           lines "\n"))
                      (aob-trace--explore-line evs))
                    'aob-session (aob-session-id s)
                    'aob-event (plist-get head :seq))))
        (plist-put head :group (cons stamp block))
        block))))

(defun aob-trace--blocks-of (s)
  "S's own events as rendered blocks, a run of looking folded into one.
Children never come up here: they render under the Task that spawned
them, grouped and in order, or in their own trace."
  (let* ((queued (delq nil (mapcar (lambda (e) (nth 2 e)) (aob-session-ref s :queued))))
         (evs (seq-remove (lambda (ev) (or (aob-trace--sub-p ev) (memq ev queued)))
                          (reverse (seq-take (aob-session-events s)
                                             aob-trace-limit))))
         (acc nil)
        ;; the first thing the agent does after you speak — and the first
        ;; thing in the trace — is where its mark belongs
        (opening t))
    (while evs
      (let* ((run (seq-take-while #'aob-trace--explores-p evs))
             (folded (>= (length run) aob-trace-explore-min))
             (group (if folded run (list (car evs))))
             (first t))
        ;; every event in the group is told whether it opens a turn, not
        ;; just the one that gets drawn: a flag left over from a render
        ;; where it was the head is a mark beside every row it touches
        (dolist (ev group)
          (let ((head (and first opening
                           (memq (plist-get ev :type) '(tool message thought))
                           t)))
            (unless (eq (and (plist-get ev :turn-head) t) head)
              (plist-put ev :turn-head head)
              (plist-put ev :line nil))
            (setq first nil)
            (setq opening (or (eq (plist-get ev :type) 'prompt)
                              (and opening
                                   (not (memq (plist-get ev :type)
                                              '(tool message thought))))))))
        (if folded
            (push (aob-trace--explore-block s run) acc)
          (push (aob-trace--block s (car evs)) acc))
        (setq evs (nthcdr (length group) evs))))
    (dolist (ev queued)
      (push (aob-trace--block s ev) acc))
    (when queued
      (push (aob-trace--queue-footer s (length queued)) acc))
    (let ((blocks (nreverse acc)))
      (when (and blocks (aob-trace--delta-p)
                 (string-prefix-p "\n" (car blocks)))
        (setcar blocks (substring (car blocks) 1)))
      blocks)))

(defvar-local aob-trace--footer nil
  "The queue's footer line as last drawn, kept so an unchanged one stays eq.")

(defun aob-trace--queue-footer (s n)
  "The muted line under N messages queued for S: when they go, how to move them."
  (let* ((held (not (memq (aob-session-state s) '(working blocked starting))))
         (key (list n held)))
    (if (equal (car aob-trace--footer) key)
        (cdr aob-trace--footer)
      (let ((line (propertize
                   (format "» %d %s · %s · \\ J/K reorder · \\ X drop"
                           n (if held "held" "queued")
                           (cond (held "\\ s sends now")
                                 ((> n 1) "sent as one when this turn ends")
                                 (t "sent when this turn ends")))
                   'font-lock-face 'aob-trace-done)))
        (setq aob-trace--footer (cons key line))
        line))))

(defun aob-trace--detail-block (s ev)
  (let* ((prose (memq (plist-get ev :type) '(message thought prompt error)))
         (props (append (list 'aob-session (aob-session-id s)
                              'aob-event (plist-get ev :seq))
                        (unless prose '(face shadow)))))
    (mapconcat (lambda (l)
                 (apply #'propertize (concat "    " (aob-trace--bound l)) props))
               (seq-take (split-string (aob-trace--detail ev) "\n")
                         aob-trace-detail-lines)
               "\n")))

;;; Subagents — every Agent or Task call the agent made, one row each, in
;;; a panel along the bottom.  RET goes to the call in the trace; TAB or
;;; o opens the subagent's own trace.

(defvar-local aob-subagents--session-id nil)
(defvar-local aob-subagents--tick -1)

(defvar aob-subagents-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap aob-object-map special-mode-map))
    (define-key map (kbd "RET") #'aob-subagents-visit)
    (define-key map (kbd "<tab>") #'aob-subagents-open)
    (define-key map "o" #'aob-subagents-open)
    map))

(define-derived-mode aob-subagents-mode special-mode "aob-subs"
  "The subagents one session delegated to."
  (ygg-ui-plain-layout)
  (setq truncate-lines t)
  (setq-local cursor-in-non-selected-windows nil)
  (hl-line-mode 1))

(defun aob-subagents--name (s) (aob--buffer-name "subs" s))

(defun aob-subagents--status (ev)
  "Running, done or failed, as the subagent call EV last reported."
  (pcase (plist-get ev :status)
    ("failed" "failed")
    ((or "completed" "success") "done")
    (_ "running")))

(defun aob-subagents--secs (ev)
  "Seconds the subagent call EV has run, or nil when it cannot be told."
  (let ((start (plist-get ev :ts))
        (end (or (plist-get ev :done-ts)
                 (and (equal (aob-subagents--status ev) "running") (float-time)))))
    (and start end (max 0 (- end start)))))

(defun aob-subagents--row (s ev)
  "The row for S's subagent call EV: status, time, steps, what it was sent to do."
  (let* ((status (aob-subagents--status ev))
         (secs (aob-subagents--secs ev))
         (n (or (plist-get ev :children) 0)))
    (propertize
     (concat (propertize (format "%-8s" status)
                         'face (pcase status
                                 ("failed" 'error)
                                 ("running" 'warning)
                                 (_ 'success)))
             (propertize (format "%7s" (if secs (aob-duration-short secs) "·"))
                         'face 'shadow)
             (propertize (format "%5d " n) 'face 'shadow)
             (if (aob-trace--sub-p ev) "└ " "")
             (aob--first-line (or (plist-get ev :title) "subagent") 72)
             (if-let* ((cs (plist-get ev :child-stat))) (concat "  " cs) ""))
     'aob-session (aob-session-id s)
     'aob-event (plist-get ev :seq))))

(defun aob-subagents--restore (line)
  "Put every window back on LINE: erasing collapses each window's point."
  (goto-char (point-min))
  (forward-line (1- line))
  (dolist (w (get-buffer-window-list (current-buffer) nil t))
    (when (window-live-p w)
      (set-window-point w (point)))))

(defun aob-subagents--render (&optional force)
  (when-let* ((s (aob-session-get aob-subagents--session-id)))
    (let ((tick (or (aob-session-ref s :tick) 0)))
      (unless (and (not force) (eql aob-subagents--tick tick))
        (setq aob-subagents--tick tick)
        (let* ((subs (aob-session-subagents s))
               (live (seq-count (lambda (e)
                                  (equal (aob-subagents--status e) "running"))
                                subs))
               (line (line-number-at-pos))
               (inhibit-read-only t))
          (setq header-line-format
                (format " %s · %d subagent%s%s · RET the call · o its trace"
                        (aob-session-name s) (length subs)
                        (if (= (length subs) 1) "" "s")
                        (if (> live 0) (format " · %d running" live) "")))
          (erase-buffer)
          (if subs
              (dolist (ev subs) (insert (aob-subagents--row s ev) "\n"))
            (insert (propertize " working alone\n" 'face 'shadow)))
          (aob-subagents--restore line))))))

(defun aob-subagents--at-point ()
  "The session and the subagent call seq on this line, or a user error."
  (let ((seq (get-text-property (line-beginning-position) 'aob-event))
        (s (aob-session-get aob-subagents--session-id)))
    (unless (and s seq) (user-error "aob: no subagent on this line"))
    (cons s seq)))

(defun aob-subagents-visit ()
  "Go to the subagent call on this line in its agent's trace."
  (interactive)
  (pcase-let ((`(,s . ,seq) (aob-subagents--at-point)))
    (aob-subagents--goto s seq)))

(defun aob-subagents--goto (s seq)
  "Show S's trace in a window other than this one, at the event SEQ."
  (let* ((buf (aob-trace-buffer s))
         (win (or (get-buffer-window buf)
                  (display-buffer buf '((display-buffer-reuse-window
                                         display-buffer-use-some-window)
                                        (inhibit-same-window . t))))))
    (with-current-buffer buf
      (when-let* ((at (aob-trace--event-bounds seq)))
        (goto-char (car at))
        (when (window-live-p win) (set-window-point win (car at)))))
    (when (window-live-p win) (select-window win))
    buf))

(defun aob-subagents-open ()
  "Open the trace of the subagent on this line."
  (interactive)
  (pcase-let ((`(,s . ,seq) (aob-subagents--at-point)))
    (aob-trace (or (aob-trace--child-at s seq)
                   (user-error "aob: that subagent kept no trace")))))

(defun aob-subagents-buffer (s)
  "Return S's subagents buffer, creating and registering it if needed."
  (let ((buf (get-buffer-create (aob-subagents--name s))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aob-subagents-mode) (aob-subagents-mode))
      (setq aob-subagents--session-id (aob-session-id s)
            aob-buffer-session-id (aob-session-id s))
      (aob-register-view buf #'aob-subagents--render)
      (let ((inhibit-read-only t)) (aob-subagents--render t)))
    buf))

(defcustom aob-subagents-display-action
  '((display-buffer-reuse-window display-buffer-at-bottom)
    (window-height . 0.3)
    (preserve-size . (nil . t))
    (window-parameters . ((no-delete-other-windows . t))))
  "Where the subagents list opens: a panel along the bottom, as the quickfix does."
  :type 'sexp :group 'aob)

;;;###autoload
(defun aob-subagents (s)
  "Show the list of subagents S ran, or put it away when it is showing."
  (interactive (list (aob-target)))
  (if-let* ((win (get-buffer-window (aob-subagents--name s))))
      (quit-window nil win)
    (let ((win (display-buffer (aob-subagents-buffer s) aob-subagents-display-action)))
      (when (window-live-p win) (select-window win))
      win)))

(defun aob-trace--render (&optional force)
  ;; another agent's chatter must not redraw this trace: skip unless
  ;; *this* session ticked since the last render
  (when-let* ((s (aob-session-get aob-trace--session-id)))
    (let ((tick (or (aob-session-ref s :tick) 0)))
      (unless (and (not force) (eql aob-trace--tick tick))
        (setq aob-trace--tick tick)
        (aob-trace--render-1 s)))))

(defun aob-trace--dir-line (s)
  "S's folder for the header, or nothing when it has none yet.
Recomputed on every render rather than kept from when the buffer was
made: a session can learn its project later, and a trace that stopped
naming a folder is a trace you have to guess the tree of."
  (if-let* ((dir (or (aob-session-dir s) (aob-session-project s))))
      (propertize (format " · %s" (abbreviate-file-name dir)) 'face 'shadow)
    ""))

(defun aob-trace--queued-at-point (&optional pos)
  "The queue entry the line at POS stands for, or nil.
An entry is (TEXT ATTACHMENTS EVENT), as the session keeps it."
  (when-let* ((s (aob-session-get aob-trace--session-id))
              (seq (get-text-property (line-beginning-position) 'aob-event))
              (q (aob-session-ref s :queued)))
    (ignore pos)
    (seq-find (lambda (e) (eql seq (plist-get (nth 2 e) :seq))) q)))

(defun aob-trace--queue-rewrite (s entry text)
  "Make ENTRY, queued for S, say TEXT instead."
  (unless (memq entry (aob-session-ref s :queued))
    (user-error "aob: that message has already gone"))
  (let ((ev (nth 2 entry)))
    (setcar entry text)
    (plist-put ev :text text)
    (plist-put ev :line nil)
    (plist-put ev :head nil)
    (run-hook-with-args 'aob-queue-change-hook s)
    (aob--dirty s)))

(defun aob-trace-queue-edit ()
  "Rewrite the queued prompt on this line in a draft; sending it replaces it."
  (interactive)
  (let* ((s (or (aob-session-get aob-trace--session-id) (user-error "aob: no session here")))
         (entry (or (aob-trace--queued-at-point)
                    (user-error "aob: no queued message on this line")))
         (name (format "queued:%s:%s" (aob-session-name s)
                       (plist-get (nth 2 entry) :seq))))
    (if-let* ((held (get-buffer (concat "compose:" name))))
        (aob-compose-show held)
      (aob-compose (lambda (text _atts) (aob-trace--queue-rewrite s entry text))
                   (car entry) name))))

(defun aob-trace--queued-starts ()
  "Where each message queued for this session begins in the trace, in order."
  (when-let* ((s (aob-session-get aob-trace--session-id)))
    (sort (delq nil (mapcar (lambda (e)
                              (car (aob-trace--event-bounds (plist-get (nth 2 e) :seq))))
                            (aob-session-ref s :queued)))
          #'<)))

(defun aob-trace--queued-go (dir)
  "Go to the queued message DIR of point: 1 the next, -1 the one before."
  (let* ((starts (aob-trace--queued-starts))
         (here (line-beginning-position))
         (to (if (> dir 0)
                 (seq-find (lambda (p) (> p here)) starts)
               (seq-find (lambda (p) (< p here)) (reverse starts)))))
    (cond ((null starts) (user-error "aob: nothing queued"))
          ((null to) (user-error "aob: no queued message %s" (if (> dir 0) "after this" "before this")))
          (t (goto-char to)
             (message "%d queued · \\ e rewrite · \\ J/K reorder · \\ X drop · \\ s say it now" (length starts))))))

(defun aob-trace-queued-next ()
  "Go to the next message waiting to be sent."
  (interactive)
  (aob-trace--queued-go 1))

(defun aob-trace-queued-prev ()
  "Go to the message before this one waiting to be sent."
  (interactive)
  (aob-trace--queued-go -1))

(defun aob-trace-queue-steer ()
  "Say the queued prompt on this line now, into the turn that is running.
The first one queued when point is on none of them.  Between turns, when
a failed turn has left the queue held, the whole queue goes out now."
  (interactive)
  (let ((s (or (aob-session-get aob-trace--session-id) (user-error "aob: no session here"))))
    (if (and (aob-session-ref s :queued)
             (eq (aob-session-state s) 'idle))
        (aob--call s :flush)
      (aob-trace--queue-steer-1 s))))

(defun aob-trace--queue-steer-1 (s)
  (let* ((entry (or (aob-trace--queued-at-point)
                    (car (aob-session-ref s :queued))
                    (user-error "aob: nothing is queued")))
         (ev (nth 2 entry)))
    (when (nth 1 entry)
      (user-error "aob: a message with images waits for the turn to end"))
    (unless (and (fboundp 'aob-acp--steers-p) (ignore-errors (aob-acp--steers-p s)))
      (user-error "aob: %s takes nothing mid-turn" (aob-session-name s)))
    (aob-session-put s :queued (delq entry (aob-session-ref s :queued)))
    (setf (aob-session-events s) (delq ev (aob-session-events s)))
    (run-hook-with-args 'aob-queue-change-hook s)
    (let ((aob-prompt-typed (plist-get ev :typed)))
      (aob-interject s (car entry)))))

(defun aob-trace--queue-move (by)
  (let* ((s (or (aob-session-get aob-trace--session-id) (user-error "aob: no session here")))
         (entry (or (aob-trace--queued-at-point)
                    (user-error "aob: no queued message on this line")))
         (seq (plist-get (nth 2 entry) :seq)))
    (aob-queue-move s entry by)
    (aob-trace--render t)
    (when-let* ((at (aob-trace--event-bounds seq)))
      (goto-char (car at)))))

(defun aob-trace-queue-earlier ()
  "Move the queued prompt on this line one place earlier in the queue."
  (interactive)
  (aob-trace--queue-move -1))

(defun aob-trace-queue-later ()
  "Move the queued prompt on this line one place later in the queue."
  (interactive)
  (aob-trace--queue-move 1))

(defun aob-trace-usage ()
  "Say what this session has spent: time, every token count, and cost."
  (interactive)
  (message "%s" (aob-usage-describe
                 (or (aob-session-get aob-trace--session-id)
                     (user-error "aob: no session here")))))

(defun aob-trace-queue-drop ()
  "Take the queued prompt on this line back out of the queue."
  (interactive)
  (let* ((s (or (aob-session-get aob-trace--session-id) (user-error "aob: no session here")))
         (entry (or (aob-trace--queued-at-point)
                    (user-error "aob: no queued message on this line")))
         (ev (nth 2 entry)))
    (aob-session-put s :queued (delq entry (aob-session-ref s :queued)))
    (setf (aob-session-events s) (delq ev (aob-session-events s)))
    (run-hook-with-args 'aob-queue-change-hook s)
    (aob--dirty s)
    (message "aob: dropped a queued message")))

(defun aob-trace--tail-start ()
  "Where the live edge of this trace begins.
The buffer ends in a newline, so `point-max' sits on an empty line that
no motion puts you on: `G' lands on the last line with something on it,
one short of the end, and a trace that only follows from `point-max'
quietly stops following exactly when you asked to watch it."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (line-beginning-position)))

(defun aob-trace--sep ()
  "The newline that separates one block from the next, with Delta's air."
  (if (aob-trace--delta-p)
      (propertize "\n" 'line-spacing aob-trace-paragraph-space)
    "\n"))

(defcustom aob-trace-inline-input t
  "Compose at the end of the trace itself, the way Delta does, rather
than only in the compose buffer."
  :type 'boolean :group 'aob)

(defvar-local aob-trace--input nil
  "Marker where the rendered trace ends and what you are typing begins.")

(defun aob-trace--inline-p ()
  (and (aob-trace--delta-p) aob-trace-inline-input))

(defun aob-trace--tail-end ()
  "Where the rendered blocks end: before the input line, when there is one.
The input marker stands after the glyph that line carries, so writing
up to it rewrites that glyph on every append — and takes every marker
standing at the end of the trace with it."
  (if (and aob-trace--input (marker-position aob-trace--input))
      (save-excursion
        (goto-char (marker-position aob-trace--input))
        (line-beginning-position))
    (point-max)))

(defun aob-trace--content-end ()
  "Where the rendered trace ends: the input marker, else the buffer end."
  (if (and aob-trace--input (marker-position aob-trace--input))
      (marker-position aob-trace--input)
    (point-max)))

(defun aob-trace--user-glyph ()
  (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-account_circle" 'aob-trace-speaker) ""))

(defun aob-trace--ensure-input ()
  "Make sure the buffer ends in a writable line to type the next prompt in."
  (when (and (aob-trace--inline-p) (not aob-trace--own-parent))
    (let ((inhibit-read-only t))
      (unless (and aob-trace--input (marker-position aob-trace--input))
        (save-excursion
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          ;; no mark beside it: the line you type in is not something
          ;; said, and a face in the gutter says somebody spoke
          (insert " ")
          (setq aob-trace--input (point-marker))
          (set-marker-insertion-type aob-trace--input nil)))
      (setq buffer-read-only nil)
      ;; the separator before the input has nothing after it to be
      ;; separated from, and a cursor standing on a line stretched to
      ;; nearly twice its height reads as a block, not a caret
      (when (and aob-trace--input (marker-position aob-trace--input)
                 (> (marker-position aob-trace--input) (point-min)))
        (remove-text-properties (1- (marker-position aob-trace--input))
                                (marker-position aob-trace--input)
                                '(line-spacing nil)))
      (let ((end (aob-trace--content-end)))
        (add-text-properties (point-min) end
                             '(read-only t front-sticky (read-only)))
        (when (> end (point-min))
          (put-text-property (1- end) end 'rear-nonsticky '(read-only)))))))

(defun aob-trace-input ()
  "Put point where the next prompt is typed, in insert state."
  (interactive)
  (aob-trace--ensure-input)
  (goto-char (point-max))
  (when (fboundp 'ygg-insert-state) (ygg-insert-state)))

(declare-function aob-acp--steers-p "aob-acp" (s))
(declare-function aob-interject "aob" (s text))

(defun aob-trace--say (s text)
  "Say TEXT to S: into the turn it is running, where it takes that.
A turn already running is not a reason to wait — an agent whose
subagents are working is an agent you can still talk to, and where
the adapter takes steering the words go in without costing it the
work in flight."
  (let ((aob-prompt-typed t))
    (if (and (eq (aob-session-state s) 'working)
             (fboundp 'aob-acp--steers-p)
             (ignore-errors (aob-acp--steers-p s)))
        (aob-interject s text)
      (aob-prompt s text nil))))

(defun aob-trace-send ()
  "Send what is typed at the end of the trace to this session.
While the agent waits on a question or a plan, this answers it instead."
  (interactive)
  (let* ((start (aob-trace--content-end))
         (text (string-trim (buffer-substring-no-properties start (point-max))))
         (s (aob-session-get aob-trace--session-id))
         (waiting (aob-trace-waiting-decision s)))
    (unless s (user-error "aob: this trace has no session"))
    (if waiting
        (progn
          (when (eq (plist-get waiting :kind) 'plan)
            (let ((inhibit-read-only t))
              (delete-region start (point-max))))
          (when (fboundp 'ygg-normal-state) (ygg-normal-state))
          (aob-trace--answer-decision s waiting text)
          (aob-trace--render t))
      (let ((comments (aob-trace--comments-message s)))
        (when (and (string-empty-p text) (not comments))
          (user-error "aob: nothing to send"))
        (let ((inhibit-read-only t))
          (delete-region start (point-max)))
        (when (fboundp 'ygg-normal-state) (ygg-normal-state))
        (aob-session-put s :comments nil)
        (aob-trace--say s (string-trim
                           (string-join
                            (delq nil (list (and (fboundp 'aob-context-text)
                                                 (aob-context-text))
                                            comments
                                            (unless (string-empty-p text) text)))
                            "\n\n")))))))

(defface aob-trace-anchor
  '((((background dark)) :background "#3a3222" :underline "#8a7a4a")
    (t :background "#fdf3d0" :underline "#b08a3a"))
  "Face marking text a comment is attached to."
  :group 'aob)

(defface aob-trace-comment
  '((((background dark)) :background "#1c1c1c" :extend t)
    (t :background "#eeeeee" :extend t))
  "Face behind a comment card."
  :group 'aob)

(defun aob-trace--comments (s)
  "Comments held against S, oldest first."
  (reverse (aob-session-ref s :comments)))

(defun aob-trace--comments-for (s seq)
  "Comments attached to the event numbered SEQ."
  (seq-filter (lambda (c) (eql (plist-get c :seq) seq))
              (aob-trace--comments s)))

(defconst aob-trace--question-re "\\?[*_`)\"” \t]*\\'"
  "A line that asks: its last word before any closing markup is a question mark.")

(defconst aob-trace--option-re
  "\\`[ \t]*\\(?:[-*+]\\|[0-9A-Za-z][.)]\\|\\[[ xX]\\]\\)[ \t]+"
  "A line that offers an answer: a list item under a question.")

(defun aob-trace--visible (str)
  "STR without what markdown hid in it, and trimmed."
  (let ((out nil) (i 0) (n (length str)))
    (while (< i n)
      (unless (get-text-property i 'invisible str) (push (aref str i) out))
      (setq i (1+ i)))
    (string-trim (concat (nreverse out)))))

(defun aob-trace--questions (s ev str)
  "STR, an answer of S, with its questions and their options made answerable.
A question line is marked open or answered; an option under it carries
what choosing it says.  Answers are held like comments and go with the
next message, so a questionnaire is answered in the trace itself."
  (if (not (and (eq (plist-get ev :type) 'message)
                (not (aob-trace--live-p ev))
                (string-search "?" str)))
      str
    (let* ((seq (plist-get ev :seq))
           (held (aob-trace--comments-for s seq))
           (lines (split-string str "\n"))
           (question nil)
           out)
      (dolist (line lines)
        (let ((seen (aob-trace--visible line)))
          (cond
           ((and (not (string-empty-p seen))
                 (string-match-p aob-trace--question-re seen))
            (setq question (replace-regexp-in-string
                            "\\`[ \t]*\\(?:[0-9]+[.)]\\|[-*+]\\)[ \t]*" "" seen))
            (let* ((done (seq-find (lambda (c) (equal (plist-get c :quote) question)) held))
                   (mark (propertize (if done "◆ " "◇ ") 'font-lock-face 'shadow))
                   (l (concat mark line)))
              (put-text-property 0 (length l) 'aob-question question l)
              (push l out)))
           ((and question (string-match-p aob-trace--option-re
                                           (substring-no-properties line)))
            (let* ((choice (replace-regexp-in-string aob-trace--option-re "" seen))
                   (picked (seq-find (lambda (c) (and (equal (plist-get c :quote) question)
                                                      (equal (plist-get c :text) choice)))
                                     held))
                   (l (concat line (if picked (propertize "  ✓" 'font-lock-face 'shadow) ""))))
              (put-text-property 0 (length l) 'aob-option (list question choice) l)
              (push l out)))
           (t
            (unless (string-empty-p seen) (setq question nil))
            (push line out)))))
      (let ((joined (mapconcat #'identity (nreverse out) "\n")))
        ;; the whole block is the event's, its newlines too, or everything
        ;; that finds a message by its text stops at its first line
        (dolist (prop '(aob-event aob-session))
          (when-let* ((v (get-text-property 0 prop str)))
            (put-text-property 0 (length joined) prop v joined)))
        joined))))

(defun aob-trace--answered-count (s)
  "How many questions S's held answers speak to."
  (length (delete-dups (mapcar (lambda (c) (plist-get c :quote)) (aob-trace--comments s)))))

(defun aob-trace-answer ()
  "Answer what point is on: pick the option, or write to the question.
Anywhere else, compose as before.  Answers are held and sent together
with the next message, ZZ at the end of the trace."
  (interactive)
  (let ((option (get-text-property (line-beginning-position) 'aob-option))
        (question (get-text-property (line-beginning-position) 'aob-question))
        (plan-option (get-text-property (line-beginning-position) 'aob-plan-option))
        (seq (get-text-property (point) 'aob-event))
        (s (aob-session-get aob-trace--session-id)))
    (cond
     ((and plan-option s)
      (let ((d (aob-trace--pending s seq)))
        (unless d (user-error "aob: this plan is no longer waiting"))
        (aob--call s :resolve d plan-option)
        (aob-trace--render t)))
     ((and option s)
      (let* ((multi (get-text-property (line-beginning-position) 'aob-multi))
             (choices (get-text-property (line-beginning-position) 'aob-choices))
             (picked (seq-find (lambda (c) (and (eql (plist-get c :seq) seq)
                                                (equal (plist-get c :quote) (car option))
                                                (equal (plist-get c :text) (cadr option))))
                               (aob-session-ref s :comments))))
        (aob-session-put s :comments
                         (seq-remove (lambda (c)
                                       (and (eql (plist-get c :seq) seq)
                                            (equal (plist-get c :quote) (car option))
                                            (cond (multi (eq c picked))
                                                  (choices (member (plist-get c :text) choices))
                                                  (t t))))
                                     (aob-session-ref s :comments)))
        (if (and multi picked)
            (aob-trace--render t)
          (aob-trace--add-comment s seq (car option) (cadr option))))
      (message "%d answered · ZZ sends them" (aob-trace--answered-count s)))
     ((and question s)
      (aob-trace--comment-box (current-buffer) seq question (point)))
     ((when-let* ((kid (and s seq (aob-trace--child-at s seq))))
        (aob-trace kid)))
     (aob-trace--own-parent (aob-trace-toggle))
     (t (call-interactively #'aob-compose)))))

(defun aob-trace--child-at (s seq)
  "The subagent session the call SEQ of S is traced in, or nil."
  (when-let* ((ev (seq-find (lambda (e) (eql (plist-get e :seq) seq))
                            (aob-session-events s))))
    (aob-session-native-child s ev)))

(defun aob-trace--pending (s seq)
  "The decision S waits on that the trace drew as event SEQ, or nil."
  (and seq (seq-find (lambda (d) (eql (plist-get d :seq) seq))
                     (aob-session-decisions s))))

(defun aob-trace-waiting-decision (s)
  "The question or plan S's agent is waiting on the trace to answer, or nil."
  (and s (seq-find (lambda (d) (and (plist-get d :seq)
                                    (memq (plist-get d :kind) '(elicitation plan))))
                   (aob-session-decisions s))))

(defvar-local aob-trace--decision-blocks nil
  "Seq to (KEY . BLOCK): a decision's drawn block and what it was drawn from.")

(defun aob-trace--decision (s ev str)
  "STR, the row of a question or plan S's agent asked, with its body under it.
The body is redrawn only when what it shows has moved, so an unchanged
block stays the same string and the incremental pass skips it."
  (let* ((seq (plist-get ev :seq))
         (pending (and (aob-trace--pending s seq) t))
         (key (list str pending (plist-get ev :answer)
                    (aob-trace--comments-for s seq)
                    (and (eq (plist-get ev :decision-kind) 'plan)
                         (length (aob-trace--comments s)))))
         (hit (cdr (assq seq aob-trace--decision-blocks))))
    (if (and hit (equal (car hit) key))
        (cdr hit)
      (let ((block (concat str "\n"
                           (if (eq (plist-get ev :decision-kind) 'plan)
                               (aob-trace--plan-review s ev pending)
                             (aob-trace--asked s ev pending)))))
        (dolist (prop '(aob-event aob-session))
          (when-let* ((v (get-text-property 0 prop str)))
            (put-text-property 0 (length block) prop v block)))
        (when (eq (plist-get ev :decision-kind) 'plan)
          (setq block (aob-trace--annotate s ev block)))
        (setf (alist-get seq aob-trace--decision-blocks) (cons key block))
        block))))

(defun aob-trace--hint (text)
  "TEXT as a quiet line under a block."
  (propertize (concat "  " text) 'font-lock-face 'shadow))

(defun aob-trace--asked (s ev pending)
  "The questions EV holds, answerable in place while PENDING."
  (let ((held (aob-trace--comments-for s (plist-get ev :seq)))
        (answer (plist-get ev :answer))
        lines)
    (dolist (q (plist-get ev :questions))
      (let* ((text (plist-get q :text))
             (options (plist-get q :options))
             (mine (seq-filter (lambda (c) (equal (plist-get c :quote) text)) held))
             (header (plist-get q :header))
             (head (concat (if (and header (not (equal header text)))
                               (concat header " · ")
                             "")
                           text)))
        (if (not pending)
            (let* ((given (and (consp answer)
                               (append (ensure-list (cdr (assoc (plist-get q :key) answer)))
                                       (ensure-list (cdr (assoc (plist-get q :custom) answer))))))
                   (said (cond ((eq answer 'decline) "declined")
                               ((eq answer 'withdrawn) "withdrawn by the agent")
                               (given (mapconcat (lambda (v) (format "%s" v)) given ", "))
                               (t "not answered"))))
              (push (concat "  " head) lines)
              (push (aob-trace--hint (concat "→ " said)) lines))
          (let ((l (concat "  " (propertize (if mine "◆ " "◇ ") 'font-lock-face 'shadow)
                           head)))
            (put-text-property 0 (length l) 'aob-question text l)
            (push l lines))
          (dolist (o options)
            (let ((l (concat "    ◦ " o
                             (if (seq-find (lambda (c) (equal (plist-get c :text) o)) mine)
                                 (propertize "  ✓" 'font-lock-face 'shadow)
                               ""))))
              (add-text-properties 0 (length l)
                                   (list 'aob-option (list text o)
                                         'aob-choices options
                                         'aob-multi (plist-get q :multi))
                                   l)
              (push l lines)))
          (dolist (c mine)
            (unless (member (plist-get c :text) options)
              (push (concat "    ✎ " (plist-get c :text)) lines))))))
    (when pending
      (push (aob-trace--hint
             "RET picks an option · RET on a question types your own · ZZ sends · ZQ declines")
            lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun aob-trace--plan-review (s ev pending)
  "The plan EV holds, with what leaving planning can mean while PENDING."
  (let* ((plan (aob-trace--prose (aob-trace--md (plist-get ev :plan))))
         (options (plist-get ev :options))
         (d (and pending (aob-trace--pending s (plist-get ev :seq))))
         (least (and d (aob-trace--least-allow d)))
         (noted (aob-trace--comments s)))
    (concat
     plan "\n"
     (if (not pending)
         (aob-trace--hint (format "→ %s" (pcase (plist-get ev :answer)
                                            ('nil "not answered")
                                            ('withdrawn "withdrawn by the agent")
                                            (a a))))
       (concat
        (mapconcat (lambda (o)
                     (let ((l (concat "    ▸ " (plist-get o :name))))
                       (put-text-property 0 (length l) 'aob-plan-option (plist-get o :optionId) l)
                       l))
                   options "\n")
        "\n"
        (aob-trace--hint
         (concat (if noted
                     (format "ZZ sends %d comment%s back to revise"
                             (length noted) (if (cdr noted) "s" ""))
                   (format "ZZ %s"
                           (or (plist-get (seq-find (lambda (o) (equal (plist-get o :optionId) least))
                                                    options)
                                          :name)
                               "approves")))
                 " · RET takes the option under point · C comments · ZQ keeps planning")))))))

(defun aob-trace--least-allow (d)
  "The option of plan decision D that approves it for the least time."
  (seq-some (lambda (kind)
              (plist-get (seq-find (lambda (o) (equal (plist-get o :kind) kind))
                                   (plist-get d :options))
                         :optionId))
            '("allow_once" "allow_always")))

(defun aob-trace--elicit-content (s d)
  "The answers held on D's questions, as ((FIELD . VALUE)...).
A typed answer goes to the field the agent keeps for one, or to the
question itself when it has none and nothing was picked."
  (let ((held (aob-trace--comments-for s (plist-get d :seq)))
        content)
    (dolist (q (plist-get d :questions) (nreverse content))
      (let* ((options (plist-get q :options))
             (multi (plist-get q :multi))
             (texts (mapcar (lambda (c) (plist-get c :text))
                            (seq-filter (lambda (c) (equal (plist-get c :quote) (plist-get q :text)))
                                        held)))
             (picks (seq-filter (lambda (x) (member x options)) texts))
             (typed (string-join (seq-remove (lambda (x) (member x options)) texts) "\n")))
        (when picks
          (push (cons (plist-get q :key) (if multi picks (car (last picks)))) content))
        (unless (string-empty-p typed)
          (cond ((plist-get q :custom) (push (cons (plist-get q :custom) typed) content))
                ((not picks) (push (cons (plist-get q :key) (if multi (list typed) typed))
                                   content))))))))

(defun aob-trace--answer-decision (s d text)
  "Answer D, the question or plan S's agent waits on, from what is held.
A question takes the answers held on it.  A plan is approved the least
lasting way unless comments are held or TEXT was typed; then it is
refused, and they go back as the next message, since a refusal carries
no words of its own."
  (pcase (plist-get d :kind)
    ('elicitation
     (let ((content (aob-trace--elicit-content s d))
           (seq (plist-get d :seq)))
       (aob-session-put s :comments
                        (seq-remove (lambda (c) (eql (plist-get c :seq) seq))
                                    (aob-session-ref s :comments)))
       (aob--call s :resolve d content)))
    ('plan
     (let ((feedback (string-join
                      (delq nil (list (aob-trace--comments-message s)
                                      (and text (not (string-empty-p text)) text)))
                      "\n\n")))
       (if (string-empty-p feedback)
           (aob--call s :resolve d (or (aob-trace--least-allow d)
                                       (user-error "aob: this plan offers no approval")))
         (unless (aob-reject s d)
           (user-error "aob: this plan offers no way to refuse it"))
         (aob-session-put s :comments nil)
         (let ((aob-prompt-typed t))
           (aob-prompt s feedback)))))))

(defun aob-trace-decline ()
  "Turn down what the agent waits on here: a question goes unanswered, a
plan stays a plan.  With nothing waiting, ZQ does what it does elsewhere."
  (interactive)
  (let* ((s (aob-session-get aob-trace--session-id))
         (d (aob-trace-waiting-decision s)))
    (cond ((null d)
           (if (fboundp 'ygg-kill-buffer-no-save)
               (call-interactively 'ygg-kill-buffer-no-save)
             (quit-window)))
          ((eq (plist-get d :kind) 'plan)
           (aob-reject s d)
           (aob-trace--render t))
          (t
           (let ((seq (plist-get d :seq)))
             (aob-session-put s :comments
                              (seq-remove (lambda (c) (eql (plist-get c :seq) seq))
                                          (aob-session-ref s :comments))))
           (aob--call s :resolve d 'decline)
           (aob-trace--render t)))))

(defun aob-trace--annotate (s ev str)
  "STR with its comments marked: the quoted text lit, each comment under it."
  (let ((cs (aob-trace--comments-for s (plist-get ev :seq))))
    (if (null cs)
        str
      (let ((copy (copy-sequence str)))
        (dolist (c cs)
          (when-let* ((quoted (plist-get c :quote))
                      ((not (string-empty-p quoted)))
                      (at (string-search quoted copy)))
            (add-face-text-property at (+ at (length quoted))
                                    'aob-trace-anchor t copy)))
        (concat copy "\n"
                (mapconcat
                 (lambda (c)
                   (let ((line (concat "  " (aob-trace--user-glyph) "  "
                                       (plist-get c :text) "\n")))
                     (add-face-text-property 0 (length line)
                                             'aob-trace-comment t line)
                     line))
                 cs ""))))))

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")

(defun aob-trace--add-comment (s seq quoted text)
  "Hold TEXT as a comment on QUOTED in event SEQ of S, and redraw."
  (when (string-empty-p (string-trim text)) (user-error "aob: empty comment"))
  (aob-session-put s :comments
                   (cons (list :seq seq :quote quoted :text text
                               :ts (float-time))
                         (aob-session-ref s :comments)))
  (when (bound-and-true-p yggdrasil-local-mode) (ygg-normal-state))
  (if (and (derived-mode-p 'aob-trace-mode)
           (equal aob-trace--session-id (aob-session-id s)))
      (aob-trace--render t)
    (when-let* ((trace (get-buffer (aob-trace--name s))))
      (with-current-buffer trace (aob-trace--render t)))))

(defun aob-trace-comment-on (s quote)
  "Comment on QUOTE for session S, in a box under the line point is on."
  (unless s (user-error "aob: no session to comment to"))
  (if (and (display-graphic-p) (fboundp 'posframe-show)
           (get-buffer-window (current-buffer)))
      (aob-trace--comment-box (current-buffer) nil quote (point) s)
    (aob-trace--add-comment
     s nil quote
     (read-string (format "Comment on %s: "
                          (truncate-string-to-width quote 40 nil nil t))))))

(defun aob-trace-comment (start end)
  "Comment on the selected text, or on the block point is in.
The box opens under what it is about, in the trace itself; the comment
is held until the next message is sent, the way Delta submits a thread
of comments together."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (let* ((s (aob-session-get aob-trace--session-id))
         (seq (get-text-property start 'aob-event))
         (quoted (string-trim (buffer-substring-no-properties start end))))
    (unless s (user-error "aob: this trace has no session"))
    (unless seq (user-error "aob: nothing to comment on here"))
    (deactivate-mark)
    (if (and (display-graphic-p) (fboundp 'posframe-show)
             (get-buffer-window (current-buffer)))
        (aob-trace--comment-box (current-buffer) seq quoted end)
      (aob-trace--add-comment
       s seq quoted
       (read-string (format "Comment on %s: "
                            (truncate-string-to-width quoted 40 nil nil t)))))))

(defvar-local aob-trace--comment-target nil
  "(FROM SEQ QUOTE S): the buffer it opened in, event, words and session.
S is nil when FROM is a trace, whose own session takes the comment.")

(defvar aob-trace-comment-box-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") #'aob-trace-comment-box-cancel)
    (define-key map [remap ygg-save-and-kill-buffer] #'aob-trace-comment-box-send-now)
    (define-key map [remap ygg-kill-buffer-no-save] #'aob-trace-comment-box-cancel)
    (define-key map (kbd "<C-return>") #'aob-trace-comment-box-send-now)
    (define-key map (kbd "<s-return>") #'aob-trace-comment-box-send-now)
    map)
  "Keys of the box a trace comment is written in.")

(define-derived-mode aob-trace-comment-box-mode text-mode "comment"
  "Where a comment on the trace is written, under what it is about.")

(defun aob-trace--comment-box (trace seq quoted pos &optional s)
  "Open the comment box in TRACE under the line POS is on, for QUOTED in SEQ.
S is the session the comment goes to when TRACE is not its trace."
  (let* ((win (get-buffer-window trace))
         (buf (get-buffer-create " *aob-comment*"))
         (anchor (with-current-buffer trace
                   (save-excursion (goto-char pos) (line-beginning-position)))))
    (with-current-buffer buf
      (aob-trace-comment-box-mode)
      (erase-buffer)
      ;; a name led by a space is a buffer Emacs keeps no undo for
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (setq aob-trace--comment-target (list trace seq quoted s))
      (setq header-line-format
            (propertize (concat " comment on: "
                                (truncate-string-to-width
                                 (replace-regexp-in-string "\n" " " quoted)
                                 60 nil nil "…"))
                        'face 'shadow))
      (setq mode-line-format
            (propertize " ZZ or :w hold it for the next message · ZQ or :q cancel" 'face 'shadow)))
    (let ((frame (with-selected-window win
                   (posframe-show buf
                                  :position anchor
                                  :parent-window win
                                  :width (max 30 (- (window-body-width win) 4))
                                  :height 3 :min-height 3
                                  :border-width 1
                                  :border-color (face-attribute 'vertical-border
                                                                :foreground nil t)
                                  :respect-header-line t
                                  :respect-mode-line t
                                  :accept-focus t))))
      (select-frame-set-input-focus frame)
      (select-window (frame-root-window frame))
      (when (fboundp 'ygg-insert-state) (ygg-insert-state)))))

(defun aob-trace--covered-lines (win)
  "How many of WIN's bottom lines a floating box stands over, or 0.
The compose box floats at the foot of the frame, over whatever window
is there, and the end of a conversation is exactly what it covers."
  (let* ((frame (window-frame win))
         (edges (window-pixel-edges win))
         (bottom (nth 3 edges))
         (lh (with-selected-window win (default-line-height)))
         (covered 0))
    (dolist (float (frame-list))
      (when (and (frame-live-p float)
                 (eq (frame-parent float) frame)
                 (frame-visible-p float)
                 ;; only the compose box: a hint, a popup or a picker comes
                 ;; and goes with a key, and lifting the trace for it jumps
                 (fboundp 'aob-compose-frame-buffer)
                 (when-let* ((b (aob-compose-frame-buffer float)))
                   (with-current-buffer b (derived-mode-p 'aob-compose-mode))))
        (let* ((pos (frame-position float))
               (top (cdr pos))
               (left (car pos))
               (right (+ left (frame-pixel-width float))))
          (when (and (< top bottom)
                     (< left (nth 2 edges))
                     (> right (nth 0 edges)))
            (setq covered (max covered (- bottom top)))))))
    (if (> covered 0) (ceiling covered (float lh)) 0)))

(defun aob-trace--uncover (win)
  "Put the end of WIN's trace above the compose box, when it is under it.
Only when it is: a view that already shows the end clear of the box is
left where it is."
  (let ((n (aob-trace--covered-lines win)))
    (when (> n 0)
      (with-selected-window win
        (let* ((pt (window-point win))
               (y (cdr (posn-x-y (posn-at-point pt win))))
               (lh (default-line-height))
               (clear (- (window-body-height win t) (* n lh))))
          (when (or (null y) (> (+ y lh) clear))
            (goto-char pt)
            (ignore-errors (recenter (- (1+ n))))))))))

(defun aob-trace-uncover-all ()
  "Lift the end of every trace that follows its conversation above a float.
Called when a box opens over the frame."
  (dolist (win (window-list nil 'no-minibuf))
    (with-current-buffer (window-buffer win)
      (when (and (derived-mode-p 'aob-trace-mode)
                 (>= (window-point win) (aob-trace--tail-start)))
        (aob-trace--uncover win)))))

(defun aob-trace--comment-box-close ()
  "Take the box away and give the trace its cursor back."
  (let ((trace (car aob-trace--comment-target))
        (buf (current-buffer)))
    (posframe-hide buf)
    (when-let* ((win (and (buffer-live-p trace) (get-buffer-window trace t))))
      (select-frame-set-input-focus (window-frame win))
      (select-window win))))

(defun aob-trace-comment-box-send ()
  "Hold what the box says as a comment on what it was opened under."
  (interactive)
  (pcase-let ((`(,trace ,seq ,quoted ,s) aob-trace--comment-target)
              (text (string-trim (buffer-string))))
    (when (string-empty-p text) (user-error "aob: empty comment"))
    (aob-trace--comment-box-close)
    (cond (s (if (buffer-live-p trace)
                 (with-current-buffer trace
                   (aob-trace--add-comment s seq quoted text))
               (aob-trace--add-comment s seq quoted text)))
          ((buffer-live-p trace)
           (with-current-buffer trace
             (when-let* ((s (aob-session-get aob-trace--session-id)))
               (aob-trace--add-comment s seq quoted text)))))))

(defun aob-trace-comment-box-send-now ()
  "Hold this comment, then send every comment held for its session."
  (interactive)
  (pcase-let ((`(,trace ,_ ,_ ,s) aob-trace--comment-target))
    (let ((s (or s (and (buffer-live-p trace)
                        (aob-session-get
                         (buffer-local-value 'aob-trace--session-id trace))))))
      (unless s (user-error "aob: no session to send to"))
      (aob-trace-comment-box-send)
      (if-let* ((waiting (aob-trace-waiting-decision s)))
          (progn (aob-trace--answer-decision s waiting nil)
                 (when-let* ((trace (get-buffer (aob-trace--name s))))
                   (with-current-buffer trace (aob-trace--render t))))
        (when-let* ((comments (aob-trace--comments-message s)))
          (aob-session-put s :comments nil)
          (aob-trace--say s comments))))))

(defun aob-trace-comment-box-cancel ()
  "Close the box without commenting."
  (interactive)
  (aob-trace--comment-box-close))

(defun aob-trace--comments-message (s)
  "The held comments as one message, or nil when there are none."
  (when-let* ((cs (aob-trace--comments s)))
    (mapconcat (lambda (c)
                 (format "> %s\n%s" (plist-get c :quote) (plist-get c :text)))
               cs "\n\n")))

(defcustom aob-trace-rot-window 200000
  "Tokens past which answers get worse for the size of the context.
A model that takes a million does not answer at nine hundred thousand
the way it answers at fifty.  This is the window worth staying inside,
and what the header counts against — not the window the agent claims."
  :type 'natnum :group 'aob)

(defun aob-trace--meter (s)
  "S's clock, the tokens it has written and what it has cost, for the header."
  (let ((parts (delq nil
                     (list (aob-session-clock s t)
                           (when-let* ((out (plist-get (aob-session-ref s :tokens) :outputTokens))
                                       ((> out 0)))
                             (concat (aob-tokens-short out) " out"))
                           (when-let* ((cost (aob-session-cost s)) ((> cost 0)))
                             (aob-cost-short cost (aob-session-ref s :cost-currency)))))))
    (if parts
        (propertize (concat " · " (mapconcat #'identity parts " · ")) 'face 'shadow)
      "")))

(add-hook 'aob-meter-change-hook #'aob--dirty)

(defun aob-trace--rot (s)
  "How much of the sharp window S has spent, as a badge."
  (when-let* (((> aob-trace-rot-window 0))
              (used (or (aob-session-ref s :ctx-used)
                        (plist-get (aob-session-ref s :usage) :totalTokens))))
    (let ((pct (round (* 100.0 (/ (float used) aob-trace-rot-window)))))
      ;; the header is a mode-line format string: a lone per-cent is a
      ;; construct there, and the one the reader wants is two
      (propertize (format " %d%%%% of %s" pct (aob-tokens-short aob-trace-rot-window))
                  'face (cond ((>= pct 100) 'error)
                              ((>= pct 75) 'warning)
                              (t 'shadow))))))

(defun aob-trace--tool-paths (ev)
  "The absolute paths tool EV names: its locations, its file, a cd it ran."
  (let ((raw (plist-get ev :raw)))
    (append
     (mapcar (lambda (l) (plist-get l :path)) (plist-get ev :locations))
     (and (consp raw) (keywordp (car raw))
          (list (plist-get raw :file_path) (plist-get raw :path)
                (plist-get raw :notebook_path)))
     (when-let* (((consp raw))
                 ((keywordp (car raw)))
                 (cmd (plist-get raw :command))
                 ((stringp cmd)))
       (let ((start 0) out)
         (while (string-match "\\bcd +\\([~/][^ ;&|)]*\\)" cmd start)
           (push (match-string 1 cmd) out)
           (setq start (match-end 0)))
         out)))))

(defun aob-trace--work-root (s)
  "The repository S's agent was last seen working in, or nil."
  (seq-some
   (lambda (ev)
     (and (eq (plist-get ev :type) 'tool)
          (seq-some (lambda (path)
                      (and (stringp path) (file-name-absolute-p path)
                           (when-let* ((root (locate-dominating-file
                                              (expand-file-name path) ".git")))
                             (file-name-as-directory (expand-file-name root)))))
                    (aob-trace--tool-paths ev))))
   (seq-take (aob-session-events s) 40)))

(defvar-local aob-trace--root-seq nil
  "The newest tool event the trace's folder was last worked out from.")

(defun aob-trace--follow-root (s)
  "Stand the trace in the repository S works in, when its own folder is none.
An agent started in the home folder goes and finds the checkout it is
asked about; magit and a terminal started from its trace belong there,
not in a folder no command of it runs in."
  (let ((newest (plist-get (seq-find (lambda (e) (eq (plist-get e :type) 'tool))
                                     (aob-session-events s))
                           :seq)))
    (unless (equal newest aob-trace--root-seq)
      (setq aob-trace--root-seq newest)
      (let ((home (or (aob-session-project s) (aob-session-dir s))))
        (unless (and home (locate-dominating-file home ".git"))
          (when-let* ((root (aob-trace--work-root s)))
            (setq default-directory root)))))))

(declare-function ygg-todo-session-file "ygg-todo" (s))
(declare-function ygg-todo-progress "ygg-todo" (file))

(defcustom aob-trace-draw-diagrams t
  "Whether a Mermaid diagram in a settled answer is drawn without asking.
Each is drawn once; TAB takes one away and it stays away."
  :type 'boolean :group 'aob)

(defvar-local aob-trace--diagrams-seen nil
  "Diagram sources already drawn once here, so one taken away stays away.")

(defvar ygg-diagram--shown)
(defvar ygg-diagram--open-re)

(defun aob-trace--mark-diagrams (s)
  "Mark the Mermaid fences of S's settled answers to be drawn, once each."
  (dolist (ev (aob-session-events s))
    (when (and (eq (plist-get ev :type) 'message)
               (not (aob-trace--live-p ev))
               (string-search "```mermaid" (or (plist-get ev :text) "")))
      (when-let* ((bounds (aob-trace--event-bounds (plist-get ev :seq))))
        (save-excursion
          (goto-char (car bounds))
          (while (re-search-forward ygg-diagram--open-re (cdr bounds) t)
            (beginning-of-line)
            (pcase (ygg-diagram-fence-at-point)
              (`(,lang ,src ,_beg ,end)
               (let ((key (cons lang src)))
                 (when (and (equal lang "mermaid")
                            (not (member key aob-trace--diagrams-seen)))
                   (push key aob-trace--diagrams-seen)
                   (unless (member key ygg-diagram--shown)
                     (push key ygg-diagram--shown))))
               (goto-char end))
              (_ (end-of-line)))))))))

(defun aob-trace--place (pos)
  "POS as the event it stands in, the line of that event and its column.
Also a marker, for a spot no event owns.  A number goes stale the moment
anything above it is shed or written again; the event it names does not."
  (cons (when-let* ((seq (get-text-property pos 'aob-event))
                    (beg (text-property-any (point-min) (point-max) 'aob-event seq)))
          (save-excursion
            (goto-char pos)
            (let ((bol (line-beginning-position)))
              (list seq
                    (count-lines (save-excursion (goto-char beg) (line-beginning-position)) bol)
                    (- pos bol)))))
        (copy-marker pos)))

(defun aob-trace--place-pos (place)
  "Where PLACE, as aob-trace--place took it, stands now; its marker is let go."
  (pcase-let ((`((,seq ,line ,col) . ,marker) place))
    (prog1 (or (when-let* ((beg (and seq (text-property-any (point-min) (point-max)
                                                             'aob-event seq))))
                 (save-excursion
                   (goto-char beg)
                   (forward-line line)
                   (min (+ (point) col) (line-end-position))))
               (marker-position marker))
      (set-marker marker nil))))

(defun aob-trace--render-1 (s)
  (aob-trace--follow-root s)
  ;; whichever event is still being written: decorating it is work that
  ;; will be thrown away by the next chunk
  (setq aob-trace--live-seq
        (and (eq (aob-session-state s) 'working)
             (plist-get (seq-find (lambda (e) (not (aob-trace--sub-p e)))
                                  (aob-session-events s))
                        :seq)))
  (setq header-line-format
        (format " %s · %s%s%s%s%s%s%s%s%s%s"
                (aob-session-name s)
                (aob-session-state s)
                (if aob-trace--own-parent
                    (propertize " · subagent, read-only: talk to the agent that sent it"
                                'face 'shadow)
                  "")
                (aob-trace--dir-line s)
                (if-let* (((fboundp 'ygg-todo-session-file))
                          (file (ygg-todo-session-file s))
                          (progress (ygg-todo-progress file)))
                    (format " · todo %d/%d" (car progress) (cdr progress))
                  "")
                (if-let* ((m (aob-session-ref s :mode-id)))
                    (format " · %s" m)
                  "")
                (if-let* ((m (aob-session-ref s :model-name)))
                    (format " · %s" m)
                  "")
                (if-let* ((ctx (aob-session-ctx s)))
                    (concat (format " · %s ctx" ctx) (or (aob-trace--rot s) ""))
                  "")
                (aob-trace--meter s)
                (if-let* ((goal (aob-session-ref s :goal)))
                    (propertize
                     (format " · goal%s"
                             (if-let* ((n (plist-get goal :iterations)))
                                 (format " %d×" n) ""))
                     'face 'warning)
                  "")
                (if-let* ((wf (aob-session-ref s :wf-name)))
                    (propertize (format " · wf:%s +%d" wf
                                        (length (aob-session-ref s :wf-stages)))
                                'face 'warning)
                  "")))
  (let* ((blocks (aob-trace--blocks-of s))
         (new blocks)
         (old aob-trace--blocks)
         (pos 1)
         ;; standing on a queued prompt is reading, not watching: a
         ;; draft sits at the live edge, and following would take the
         ;; cursor off the line being worked on at the next chunk
         (at-end (and (>= (point) (aob-trace--tail-start))
                      (not (aob-trace--queued-at-point))))
         (point-before (aob-trace--place (point)))
         ;; each window follows by its own cursor: the buffer's is at the
         ;; end while a window scrolled up to read has its own higher up,
         ;; and pinning that window to the bottom takes the page from you
         (views (mapcar (lambda (w)
                          (let ((pt (window-point w))
                                (start (window-start w)))
                            (list w (aob-trace--place start) (aob-trace--place pt)
                                  (or (and (>= pt (aob-trace--tail-start))
                                           (not (save-excursion
                                                  (goto-char pt)
                                                  (aob-trace--queued-at-point))))
                                      ;; held still for a subagent, and not
                                      ;; moved since: still watching the end
                                      (eql start (window-parameter w 'aob-trace-held))))))
                        (get-buffer-window-list (current-buffer) nil t)))
         ;; a subagent step that changed nothing drawn is not the thread moving on
         (child-tick (and (aob-trace--sub-p (car (aob-session-events s)))
                          (equal blocks old)))
         (inhibit-read-only t)
         (inhibit-modification-hooks t))
    ;; past `aob-trace-limit' the window sheds its oldest block each tick;
    ;; cutting them off the TOP (marker-tracked delete above window-start)
    ;; realigns `old' so the prefix skip holds — no full rebuild, no jump
    (when (and new old)
      (let* ((first-seq (get-text-property 0 'aob-event (car new)))
             (cut (and first-seq
                       (text-property-any (point-min) (point-max)
                                          'aob-event first-seq))))
        (when (and cut (> cut (point-min)))
          (while (and old
                      (let ((os (get-text-property 0 'aob-event (car old))))
                        (and os (< os first-seq))))
            (pop old))
          (delete-region (point-min) cut))))
    ;; `equal', not `eq': a block rebuilt to the same text is the same
    ;; text, and rewriting it churns the tail and moves every marker
    ;; standing at the end of it
    (while (and new old (equal (car new) (car old)))
      (cl-incf pos (1+ (length (car new))))
      (pop new) (pop old))
    ;; every write below lands at an offset counted off the blocks, so it
    ;; holds only while the buffer still reads as they say.  A write at a
    ;; drifted offset lands inside a word or over the row above it, and
    ;; nothing later repairs what it wrote — so a buffer whose length no
    ;; longer answers to the blocks, or whose first changed block is not
    ;; where it is said to be, is drawn again from the top
    (when (and old
               (or (> (+ pos (length (car old))) (aob-trace--tail-end))
                   (/= (aob-trace--tail-end)
                       (+ pos
                          (apply #'+ (mapcar (lambda (b) (1+ (length b))) old))))
                   (let ((end (min (point-max) (+ pos (length (car old))))))
                     (not (equal (buffer-substring-no-properties pos end)
                                 (car old))))))
      (setq new blocks old nil pos 1))
    (when (and (null old) (< pos (aob-trace--tail-end)))
      (delete-region pos (aob-trace--tail-end)))
    (when (or new old)
      (save-excursion
        (while (or new old)
          (let* ((n (car new))
                 (o (car old))
                 (ns (and n (get-text-property 0 'aob-event n)))
                 (os (and o (get-text-property 0 'aob-event o))))
            (cond
             ((and n o (equal n o))
              (cl-incf pos (1+ (length o)))
              (pop new) (pop old))
             ((and n o (eql ns os))
              (if (and (> (length n) (length o))
                       (eq t (compare-strings o nil nil n nil (length o))))
                  (progn (goto-char (+ pos (length o)))
                         (insert (substring n (length o))))
                (delete-region pos (+ pos (length o)))
                (goto-char pos)
                (insert n))
              (cl-incf pos (1+ (length n)))
              (pop new) (pop old))
             ((and o (or (null n) (and ns os (< os ns))))
              (delete-region pos (+ pos (length o) 1))
              (pop old))
             (t
              ;; the separator above may be the one the input line flattened
              (when (and (> pos (point-min)) (aob-trace--delta-p))
                (put-text-property (1- pos) pos 'line-spacing aob-trace-paragraph-space))
              (goto-char pos)
              (insert n (aob-trace--sep))
              (cl-incf pos (1+ (length n)))
              (pop new)))))
        (when (and aob-trace--input (marker-position aob-trace--input))
          (set-marker aob-trace--input pos))))
    (setq aob-trace--blocks blocks)
    (aob-trace--ensure-input)
    (let ((before (aob-trace--place-pos point-before)))
      (goto-char (if at-end (point-max) before)))
    (pcase-dolist (`(,win ,start ,pt ,follow) views)
      (let ((start (aob-trace--place-pos start))
            (pt (aob-trace--place-pos pt)))
        (when (window-live-p win)
          (cond
           ((and follow child-tick)
            (unless (eql start (window-start win))
              (set-window-start win start))
            (set-window-parameter win 'aob-trace-held start))
           (follow
            (set-window-parameter win 'aob-trace-held nil)
            (set-window-point win (point-max))
            (aob-trace--uncover win))
           (t
            (set-window-parameter win 'aob-trace-held nil)
            (unless (eql start (window-start win))
              (let ((vscroll (window-vscroll win t)))
                (set-window-start win start)
                (set-window-vscroll win vscroll t)))
            (set-window-point win pt))))))
    (when (and aob-trace-draw-diagrams (fboundp 'ygg-diagram-fence-at-point))
      (aob-trace--mark-diagrams s))
    (when (fboundp 'ygg-diagram-replace) (ygg-diagram-replace))))

(defun aob-trace--event-bounds (seq)
  "Where the text event SEQ was drawn as begins and ends, or nil."
  (when-let* ((beg (text-property-any (point-min) (point-max) 'aob-event seq)))
    (cons beg (or (text-property-not-all beg (point-max) 'aob-event seq)
                  (point-max)))))

(defun aob-trace--event-images (seq)
  "The image files the lines of event SEQ name, in the order they appear."
  (when-let* ((bounds (aob-trace--event-bounds seq)))
    (save-excursion
      (goto-char (car bounds))
      (let (out)
        (while (< (point) (cdr bounds))
          (when-let* ((path (ygg-diagram-image-at-point)))
            (unless (member path out) (push path out)))
          (forward-line 1))
        (nreverse out)))))

(defun aob-trace-tab ()
  "Draw the fence at point, or open the event point is on with its pictures.
An event naming images opens and draws every one of them, not only the
one its folded line has room for; TAB again folds it and takes them away.
Anything else expands or collapses as before."
  (interactive)
  (let ((seq (get-text-property (point) 'aob-event)))
    (cond
     ((and (fboundp 'ygg-diagram-fence-at-point)
           (or (ygg-diagram-fence-at-point) (ygg-diagram-md-fence-at-point)))
      (ygg-diagram-toggle-any-at-point))
     ((and seq (fboundp 'ygg-diagram-image-at-point) (ygg-diagram-image-at-point))
      (if (memq seq aob-trace--expanded)
          (let ((drawn (aob-trace--event-images seq)))
            (setq ygg-diagram--shown
                  (seq-remove (lambda (c) (and (eq (car c) 'image) (member (cdr c) drawn)))
                              ygg-diagram--shown))
            (aob-trace-toggle))
        (aob-trace-toggle)
        (dolist (path (aob-trace--event-images seq))
          (unless (member (cons 'image path) ygg-diagram--shown)
            (push (cons 'image path) ygg-diagram--shown)))
        (ygg-diagram-replace)))
     ((and (fboundp 'ygg-diagram-toggle-any-at-point)
           (ygg-diagram-toggle-any-at-point)))
     (t (aob-trace-toggle)))))

(defun aob-trace-toggle ()
  "Expand or collapse the event at point."
  (interactive)
  (when-let* ((seq (get-text-property (point) 'aob-event)))
    (setq aob-trace--expanded
          (if (memq seq aob-trace--expanded)
              (delq seq aob-trace--expanded)
            (cons seq aob-trace--expanded)))
    (aob-trace--render t)))

(defun aob-trace-buffer (s)
  "Return S's trace buffer, creating and registering it if needed."
  (let ((buf (get-buffer-create (aob-trace--name s))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aob-trace-mode)
        (aob-trace-mode))
      (setq aob-trace--session-id (aob-session-id s)
            aob-buffer-session-id (aob-session-id s)
            aob-trace--own-parent (aob-session-ref s :native-tool-id))
      (setq aob-trace--dir
            (when-let* ((dir (or (aob-session-dir s) (aob-session-project s))))
              (propertize (format "  %s" (abbreviate-file-name dir))
                          'face 'shadow)))
      (when-let* ((dir (or (aob-session-project s) (aob-session-dir s)))
                  ((file-directory-p dir)))
        ;; a trace stands where its agent does: magit, a terminal, a
        ;; find-file started from here open on the agent\='s project and
        ;; not on whatever folder the buffer happened to be made in
        (setq default-directory (file-name-as-directory (expand-file-name dir))
              aob-trace--root-seq nil)
        (setq-local ygg-diagram-image-root default-directory))
      (aob-register-view buf #'aob-trace--render)
      (let ((inhibit-read-only t)) (aob-trace--render t))
      (goto-char (point-max)))
    buf))

;;;###autoload
(defun aob-trace (s)
  "Open the operation trace for session S."
  (interactive (list (aob-target)))
  (ygg-ui-show (aob-trace-buffer s)))

;;; Plan — the agent's live todo list (TodoWrite streams in as ACP plan
;;; updates) as its own view: doing first, queued next, done below

(defvar-local aob-plan--session-id nil)
(defvar-local aob-plan--tick -1)

(defvar aob-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap aob-object-map special-mode-map))
    (define-key map (kbd "RET") #'aob-compose)
    map))

(define-derived-mode aob-plan-mode special-mode "aob-plan"
  "Live task list of one agent session."
  (ygg-ui-plain-layout)
  (setq truncate-lines nil)
  (visual-line-mode 1))

(defun aob-plan--glyph (status)
  (pcase status
    ("in_progress" (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-progress_clock" 'warning)
                       (propertize "◉" 'face 'warning)))
    ("completed" (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-checkbox_marked_outline" 'success)
                     (propertize "✓" 'face 'success)))
    (_ (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-checkbox_blank_outline" 'shadow) "▫"))))

(defun aob-plan--render (&optional force)
  (when-let* ((s (aob-session-get aob-plan--session-id)))
    (let ((tick (or (aob-session-ref s :plan-tick) 0)))
      (unless (and (not force) (eql aob-plan--tick tick))
        (setq aob-plan--tick tick)
        (let* ((ev (aob-session-ref s :plan-ev))
               (entries (append (plist-get ev :entries) nil))
               (donep (lambda (e) (equal (plist-get e :status) "completed")))
               (done (seq-filter donep entries))
               (open (seq-remove donep entries))
               (inhibit-read-only t))
          (setq header-line-format
                (format " %s · %s" (aob-session-name s)
                        (or (plist-get ev :title) "no plan yet")))
          (aob--redraw-keeping-lines
           (lambda ()
             (erase-buffer)
             (dolist (e (seq-sort-by
                         (lambda (e)
                           (if (equal (plist-get e :status) "in_progress") 0 1))
                         #'< open))
               (insert (format "%s %s\n"
                               (aob-plan--glyph (plist-get e :status))
                               (plist-get e :content))))
             (when done
               (insert (propertize (format "── done %d\n" (length done))
                                   'face 'shadow))
               (dolist (e done)
                 (insert (propertize (format "✓ %s\n" (plist-get e :content))
                                     'face 'shadow)))))))))))

(defun aob-plan-buffer (s)
  "Return S's plan buffer, creating and registering it if needed."
  (let ((buf (get-buffer-create (aob--buffer-name "plan" s))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aob-plan-mode)
        (aob-plan-mode))
      (setq aob-plan--session-id (aob-session-id s)
            aob-buffer-session-id (aob-session-id s))
      (aob-register-view buf #'aob-plan--render)
      (aob-plan--render t))
    buf))

;;;###autoload
(defun aob-plan (s)
  "Open the live task view (doing / queued / done) for session S."
  (interactive (list (aob-target)))
  (ygg-ui-show (aob-plan-buffer s)))

(autoload 'aob-todo "aob-todo-view" nil t)

;; T (agent activity → quickfix) is defined in layer-aob.el: it bridges
;; the trace's tool `:locations' to the quickfix machinery, and lives in
;; the glue layer so aob-trace.el keeps no dependency on layer-quickfix.

(provide 'aob-trace)
;;; aob-trace.el ends here
