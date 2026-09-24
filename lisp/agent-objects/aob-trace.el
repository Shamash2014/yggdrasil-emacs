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

(defcustom aob-trace-prose-height 1.05
  "Height of the agent\='s and your own words, relative to the default face."
  :type 'number :group 'aob)

(defcustom aob-trace-tool-height 0.92
  "Height of tool rows, cards and run summaries, relative to the default face."
  :type 'number :group 'aob)

(defcustom aob-trace-status-gutter t
  "Mark each block in the left fringe: running, done, failed or waiting on you."
  :type 'boolean :group 'aob)

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

(defvar mwheel-scroll-down-function)

(defun aob-trace--wheel-back (&optional lines)
  "Scroll back LINES as the wheel does, taking a cursor on the live edge along.
Left there while the end is still on screen, the next chunk counts the
window as following and pulls the page back down."
  (let ((scroll-preserve-screen-position
         (if (>= (point) (aob-trace--tail-start)) 'always scroll-preserve-screen-position)))
    (scroll-down lines)))

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
  (face-remap-add-relative 'aob-trace-prose :height aob-trace-prose-height)
  (face-remap-add-relative 'aob-trace-small :height aob-trace-tool-height)
  (dolist (f '(markdown-header-face markdown-header-face-1 markdown-header-face-2
               markdown-header-face-3 markdown-header-face-4
               markdown-header-face-5 markdown-header-face-6))
    (when (facep f)
      (face-remap-set-base f :weight 'bold)))
  (when (aob-trace--delta-p)
    (setq-local line-spacing 0.3)
    (setq-local left-margin-width 4)
    (setq-local fill-column aob-trace-measure))
  (setq-local word-wrap t)
  (setq-local mwheel-scroll-down-function #'aob-trace--wheel-back)
  (add-hook 'window-configuration-change-hook #'aob-trace--fit-margins nil t)
  (add-hook 'window-buffer-change-functions
            (lambda (_frame) (aob-trace--fit-margins)) nil t)
  (aob-trace--fit-margins))

(defvar-local aob-trace--fit-width nil
  "The width the blocks in this buffer were drawn for.")

(defun aob-trace--fit-margins ()
  "Give the trace the window, keeping the gutter the mark hangs in.
`aob-trace-measure\=' caps the line only when it is set: a measure is
worth having in a window wide enough to need one, and a window nobody
widened is not that window."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (let* ((total (window-total-width win))
           (delta (aob-trace--delta-p))
           ;; the gutter costs four columns, which a narrow window does
           ;; not have to spare: the mark goes inline there instead
           (gutter (if (and delta (>= total 60)) 4 0))
           (slack (if (and delta (> aob-trace-measure 0))
                      (max 0 (- total aob-trace-measure gutter))
                    0)))
      (set-window-margins win gutter slack)
      (set-window-fringes win (if aob-trace-status-gutter 8 0) 0)
      (with-current-buffer (window-buffer win)
        (setq-local fill-column (max 20 (- total gutter slack)))
        ;; a word broken in half is a window that stopped wrapping on
        ;; words; nothing here wants character wrapping
        (setq-local word-wrap t)
        (setq-local truncate-lines nil)
        ;; a card was clipped to the width it was drawn at, and prose was
        ;; broken for it; a window that changed width has both wrong
        (let ((now (aob-trace--text-width)))
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

(defface aob-trace-queued '((t :inherit default :slant italic))
  "Face for a prompt written but not yet sent.
Set apart by its slant, not by a lighter ink."
  :group 'aob)

(defface aob-trace-aside '((t :inherit shadow))
  "Face for thinking lines and other collapsed asides.
Dim and nothing else: there is no size axis here, and a span that is
dim and smaller is two ways of saying one thing."
  :group 'aob)

(defcustom aob-trace-prose-width 100
  "Columns the agent\='s and your own words wrap at, however wide the window.
Tables, code, diffs and cards keep the whole width.  Zero wraps prose at
the window edge like everything else."
  :type 'natnum :group 'aob)

(defcustom aob-trace-gap-words 1.0
  "Space above a message, a prompt or the first block of a turn, in lines."
  :type 'number :group 'aob)

(defcustom aob-trace-gap-work 0.35
  "Space between tool rows and cards, in lines."
  :type 'number :group 'aob)

(defcustom aob-trace-run-min 5
  "Consecutive tool calls that fold into one summary row.  Zero never folds."
  :type 'natnum :group 'aob)

(defcustom aob-trace-shell-lines 12
  "Lines of a command\='s output its card shows before TAB opens the rest."
  :type 'natnum :group 'aob)

(defcustom aob-trace-diff-lines 12
  "Lines of an edit\='s diff its card shows before TAB opens the rest."
  :type 'natnum :group 'aob)

(defcustom aob-trace-quiet-modes '("default" "auto" "build" "agent")
  "Agent modes the header leaves unsaid, being what a session runs in anyway."
  :type '(repeat string) :group 'aob)

(defface aob-trace-small '((t))
  "Face sizing machine lines: tool rows, cards, run summaries.
Its height comes from `aob-trace-tool-height\='."
  :group 'aob)

(defface aob-trace-thinking '((t :inherit aob-trace-aside :slant italic))
  "Face for the folded line that stands for a thought."
  :group 'aob)

(defface aob-trace-status '((t :inherit shadow))
  "Face for the fringe mark of a block that is running, done or waiting."
  :group 'aob)

(defface aob-trace-status-failed
  '((((background dark)) :foreground "#D4484B")
    (t :inherit error))
  "Face for the fringe mark of a block that failed."
  :group 'aob)

(defface aob-trace-run '((t :inherit shadow))
  "Face for the summary row a run of tool calls folds into."
  :group 'aob)

(defface aob-trace-output '((t :inherit shadow))
  "Face for a command\='s output under its card."
  :group 'aob)

(defface aob-trace-diff-added '((t :inherit default))
  "Face for a line an edit added."
  :group 'aob)

(defface aob-trace-diff-removed '((t :inherit shadow))
  "Face for a line an edit removed."
  :group 'aob)

(defface aob-trace-diff-context '((t :inherit shadow))
  "Face for a line an edit kept, shown around what changed."
  :group 'aob)

(defface aob-trace-diff-refine-added '((t :weight bold))
  "Face for the words inside an added line that are new."
  :group 'aob)

(defface aob-trace-diff-refine-removed '((t :strike-through t))
  "Face for the words inside a removed line that are gone."
  :group 'aob)

(defun aob-trace--delta-p () (eq aob-trace-style 'delta))

(defun aob-trace--stamp (time)
  "TIME as a dim prefix, or nothing at all under the delta style."
  (if (aob-trace--delta-p) "" (propertize time 'face 'shadow)))

(when (fboundp 'define-fringe-bitmap)
  (ignore-errors
    (define-fringe-bitmap 'aob-trace-running [#x18 #x18] nil nil 'center)
    (define-fringe-bitmap 'aob-trace-done [#x01 #x03 #x06 #x8c #xd8 #x70 #x20] nil nil 'center)
    (define-fringe-bitmap 'aob-trace-failed [#x18 #x18 #x18 #x18 #x18 #x00 #x18] nil nil 'center)
    (define-fringe-bitmap 'aob-trace-waiting [#x3c #x66 #x06 #x0c #x18 #x00 #x18] nil nil 'center)))

(defconst aob-trace--marks
  '((running "·" aob-trace-running aob-trace-status)
    (done "✓" aob-trace-done aob-trace-status)
    (failed "!" aob-trace-failed aob-trace-status-failed)
    (waiting "?" aob-trace-waiting aob-trace-status))
  "State to (CHAR BITMAP FACE): what stands for it in the fringe.
The character differs per state so a block whose state moved is a block
whose text moved, which is all the incremental render compares.")

(defun aob-trace--mark (state str)
  "STR with STATE marked in the fringe beside its first line."
  (if-let* (((and aob-trace-status-gutter (stringp str) (not (string-empty-p str))))
            (spec (cdr (assq state aob-trace--marks))))
      (let ((i 0))
        (while (and (< i (length str)) (eq (aref str i) ?\n))
          (setq i (1+ i)))
        (let ((carrier (propertize (car spec)
                                   'display `(left-fringe ,(nth 1 spec) ,(nth 2 spec))
                                   'aob-status state)))
          (when (< i (length str))
            (dolist (prop '(aob-event aob-session aob-gap aob-run aob-item))
              (when-let* ((v (get-text-property i prop str)))
                (put-text-property 0 1 prop v carrier))))
          (concat (substring str 0 i) carrier (substring str i))))
    str))

(defun aob-trace--state (ev)
  "Running, done or failed, for an event that has such a thing, else nil."
  (pcase (plist-get ev :type)
    ('tool (pcase (plist-get ev :status)
             ((or "pending" "in_progress") 'running)
             ((or "completed" "success") 'done)
             ("failed" 'failed)))
    ('error 'failed)))

(defun aob-trace--faces-of (v)
  "V, a face property value, as a list of faces."
  (cond ((null v) nil)
        ((and (consp v) (keywordp (car v))) (list v))
        ((listp v) v)
        (t (list v))))

(defun aob-trace--add-face (str face &optional beg end)
  "STR with FACE appended between BEG and END, under whichever face property
each stretch already uses, since one set hides the other."
  (let ((i (or beg 0)) (end (or end (length str))))
    (while (< i end)
      (let* ((next (min (next-single-property-change i 'face str end)
                        (next-single-property-change i 'font-lock-face str end)))
             (props (text-properties-at i str))
             (prop (if (plist-get props 'face) 'face 'font-lock-face))
             (cur (aob-trace--faces-of (plist-get props prop))))
        (unless (or (plist-get props 'display)
                    (seq-find (lambda (f) (and (consp f) (plist-get f :family))) cur))
          (put-text-property i next prop (append cur (list face)) str))
        (setq i next)))
    str))

(defun aob-trace--small (str)
  "A copy of STR at the size tool rows are drawn at."
  (aob-trace--add-face (copy-sequence str) 'aob-trace-small))

(defun aob-trace--tight (str)
  "STR with no air under its own lines: a card is one object, not a list."
  (let ((i 0))
    (while (setq i (string-search "\n" str i))
      (put-text-property i (1+ i) 'line-spacing 0 str)
      (setq i (1+ i)))
    str))

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
               ("edit"    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-pencil" 'shadow) "±"))
               ("delete"  (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-trash_can_outline" 'shadow) "−"))
               ("move"    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-file_move_outline" 'shadow) "↷"))
               ("search"  (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-magnify" 'shadow) "?"))
               ("execute" (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-console" 'shadow) "$"))
               ("think"   (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-lightbulb_outline" 'shadow) "…"))
               ("fetch"   (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-web" 'shadow) "↓"))
               (_         (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-tools" 'shadow) "•")))))
    ('message    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-message_text_outline" 'shadow) "┃"))
    ('thought    (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-thought_bubble_outline" 'shadow) "∴"))
    ('prompt     (or (aob-trace--nf #'nerd-icons-octicon "nf-oct-chevron_right" 'aob-trace-speaker) "❯"))
    ('permission (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-shield_key_outline" 'shadow) "■"))
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
    (let ((out (copy-sequence text)) (pos 0) (in nil))
      (dolist (line (split-string out "\n"))
        (let ((end (+ pos (length line))))
          (cond
           ((string-match-p "^[ \t]*```[ \t]*\\(?:diff\\|patch\\)" line) (setq in t))
           ((and in (string-match-p "^[ \t]*```" line)) (setq in nil))
           (in
            (when-let* ((face (cond ((string-prefix-p "@@" line) 'aob-trace-diff-context)
                                    ((string-prefix-p "+" line) 'aob-trace-diff-added)
                                    ((string-prefix-p "-" line) 'aob-trace-diff-removed))))
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
      (let* ((room (- (min width (or (aob-trace--measure-width) width)) 1 frame))
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
  "EV's status as words on its row; the fringe carries the rest when it can."
  (pcase (plist-get ev :status)
    ("queued" (propertize "⋯ queued" 'face 'shadow))
    ("cancelled" (propertize "⊘ cancelled" 'face 'shadow))
    ((guard (and aob-trace-status-gutter (display-graphic-p))) "")
    ("pending" (propertize "⋯" 'face 'shadow))
    ("in_progress" (propertize "⟳" 'face 'shadow))
    ("completed" (propertize "✓" 'face 'shadow))
    ("failed" (propertize "✗" 'face 'error))
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
         (if (> live 0) (propertize (format "⟳%d" live) 'face 'shadow) ""))
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
  '((t :inherit default :weight bold))
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
          (concat (propertize who 'font-lock-face 'aob-trace-speaker)
                  (if (aob-trace--sub-p ev)
                      (propertize " └ subagent" 'font-lock-face 'shadow)
                    "")
                  (if (aob-trace--delta-p)
                      ""
                    (propertize (concat "  " time) 'font-lock-face 'shadow))
                  "\n"))))))

(defface aob-trace-target
  '((t :underline t))
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

(defun aob-trace--short-target (ev)
  "EV's target without the verb its row already starts with."
  (let ((target (aob-trace--target ev))
        (verb (aob-trace--verb ev))
        (case-fold-search t))
    (if (and (not (aob-trace--mcp ev))
             (string-match (concat "\\`" (regexp-quote verb) "\\(?: file\\)?[ \t]+") target)
             (< (match-end 0) (length target)))
        (substring target (match-end 0))
      target)))

(defface aob-trace-tool
  '((t :inherit shadow))
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

(defvar aob-trace--width nil
  "The text width one pass over the blocks measured, while it runs.")

(defun aob-trace--text-width ()
  "Columns the trace has for a line of text, as it stands now."
  (or aob-trace--width
      (let ((win (get-buffer-window (current-buffer) t)))
        (cond ((> aob-trace-card-width 0) aob-trace-card-width)
              (win (max 20 (window-body-width win)))
              ((> aob-trace-measure 0) aob-trace-measure)
              (t 78)))))

(defface aob-trace-tool-run
  '((t :inherit aob-trace-tool))
  "Face for a command the agent ran in a shell.
Grey like every tool, since a trace is read for its words and colour
pulls the eye." :group 'aob)

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

(defvar aob-trace--opening nil
  "Non-nil while a card is drawn whole rather than cut to its first lines.")

(defun aob-trace--right (body meta)
  "BODY with META set against the right edge, one cell in."
  (if (string-empty-p meta)
      body
    (concat body
            (propertize " " 'display
                        `(space :align-to (- right ,(1+ (string-width meta)))))
            (propertize meta 'font-lock-face 'aob-trace-card-meta))))

(defun aob-trace--root ()
  "The folder this trace's session works in, or nil."
  (when-let* ((s (and aob-trace--session-id (aob-session-get aob-trace--session-id)))
              (dir (or (aob-session-project s) (aob-session-dir s))))
    (file-name-as-directory (expand-file-name dir))))

(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function nerd-icons-icon-for-dir "nerd-icons")

(defun aob-trace--chip (path &optional line)
  "PATH as a chip: its icon and name, with LINE; the whole path is its help."
  (let* ((root (aob-trace--root))
         (abs (expand-file-name path root))
         (name (file-name-nondirectory (directory-file-name path)))
         (dir (string-suffix-p "/" path))
         (icon (and aob-trace-icons
                    (or (featurep 'nerd-icons) (require 'nerd-icons nil t))
                    (ignore-errors
                      (if dir
                          (nerd-icons-icon-for-dir name :face 'shadow)
                        (nerd-icons-icon-for-file name :face 'shadow)))))
         (where (if (and root (string-prefix-p root abs))
                    (file-relative-name abs root)
                  (abbreviate-file-name abs))))
    (propertize (concat (if icon (concat icon " ") "")
                        (propertize (concat name (if (and line (> line 0)) (format ":%d" line) ""))
                                    'font-lock-face 'aob-trace-target))
                'aob-file (list abs line nil)
                'help-echo where)))

(defun aob-trace--chip-paths (ev)
  "(PATH . LINE) for each file EV names, in the order it names them."
  (let ((raw (plist-get ev :raw)) out)
    (seq-doseq (loc (plist-get ev :locations))
      (when-let* ((p (plist-get loc :path)) ((stringp p)))
        (push (cons p (plist-get loc :line)) out)))
    (when (and (consp raw) (keywordp (car raw)))
      (dolist (k '(:file_path :notebook_path :path))
        (when-let* ((p (plist-get raw k)) ((stringp p)) ((not (string-empty-p p))))
          (push (cons p nil) out))))
    (seq-uniq (nreverse out) (lambda (a b) (equal (car a) (car b))))))

(defun aob-trace--chipped (ev text)
  "TEXT with each file EV acted on shown as a chip; one it names only in
its locations goes after it."
  (let ((out text) (left nil))
    (pcase-dolist (`(,path . ,line) (aob-trace--chip-paths ev))
      (if (string-match (concat "`?" (regexp-quote path) "`?") out)
          (setq out (concat (substring out 0 (match-beginning 0))
                            (aob-trace--chip path line)
                            (substring out (match-end 0))))
        (unless (string-match-p (regexp-quote (file-name-nondirectory path)) out)
          (push (aob-trace--chip path line) left))))
    (if left
        (concat out "  " (mapconcat #'identity (nreverse left) " "))
      out)))

(defun aob-trace--content-text (ev)
  "The words EV's content carries, joined."
  (let ((parts nil))
    (seq-doseq (c (plist-get ev :content))
      (when (equal (plist-get c :type) "content")
        (let ((inner (plist-get c :content)))
          (when (and (equal (plist-get inner :type) "text")
                     (stringp (plist-get inner :text)))
            (push (plist-get inner :text) parts)))))
    (string-join (nreverse parts) "\n")))

(defun aob-trace--shell-command (ev)
  "The command EV ran."
  (let ((raw (plist-get ev :raw))
        (title (or (plist-get ev :title) "")))
    (or (and (consp raw) (keywordp (car raw))
             (let ((c (plist-get raw :command))) (and (stringp c) c)))
        (if (string-match "\\``\\(.*\\)`\\'" title)
            (string-replace "\\`" "`" (match-string 1 title))
          title))))

(defun aob-trace--shell-output (ev)
  "What EV's command printed, without the fence an adapter wraps it in."
  (let* ((text (aob-trace--content-text ev))
         (raw (plist-get ev :rawOutput))
         (text (if (string-empty-p (string-trim text))
                   (cond ((stringp raw) raw)
                         ((and (consp raw) (keywordp (car raw)))
                          (let ((o (or (plist-get raw :output) (plist-get raw :stdout))))
                            (if (stringp o) o "")))
                         (t ""))
                 text))
         (lines (split-string (string-trim-right text) "\n")))
    (when (and lines (string-match-p "\\`[ \t]*```" (car lines)))
      (setq lines (cdr lines)))
    (when (and lines (string-match-p "\\`[ \t]*```[ \t]*\\'" (car (last lines))))
      (setq lines (butlast lines)))
    (seq-drop-while #'string-empty-p lines)))

(defconst aob-session-tail-limit 16384
  "Characters the tail of a session is read out in, at most.")

(defconst aob-session-tail-result-end 2048
  "Characters a tool result keeps from each of its two ends.")

(defun aob-trace--clip-middle (text n)
  "TEXT whole, or its first and last N characters with the cut said between."
  (if (<= (length text) (* 2 n))
      text
    (format "%s\n[... %d characters cut ...]\n%s"
            (substring text 0 n) (- (length text) (* 2 n))
            (substring text (- n)))))

(defun aob-trace--tail-block (ev)
  "EV as plain text for another agent to read, or nil for reasoning."
  (let ((text (concat (or (plist-get ev :text) "")
                      (apply #'concat (reverse (plist-get ev :parts))))))
    (pcase (plist-get ev :type)
      ('thought nil)
      ('message (concat "agent: " text))
      ('prompt (concat "user: " text))
      ('tool
       (let ((out (string-join (aob-trace--shell-output ev) "\n")))
         (concat "tool: " (or (plist-get ev :title) "?")
                 (when-let* ((st (plist-get ev :status))) (format " [%s]" st))
                 (when-let* ((stat (plist-get ev :stat))) (concat " " stat))
                 (unless (string-empty-p out)
                   (concat "\n" (aob-trace--clip-middle
                                 out aob-session-tail-result-end))))))
      ('plan (mapconcat (lambda (e) (format "- [%s] %s" (plist-get e :status)
                                            (plist-get e :content)))
                        (plist-get ev :entries) "\n"))
      (type (format "%s: %s" type (or (plist-get ev :title) (plist-get ev :reason)
                                      text))))))

(defun aob-session-tail (s &optional limit)
  "S's latest events as plain text, at most LIMIT characters.
Reasoning is left out and each tool result keeps only its two ends, so
whoever reads why S is stuck reads what it did and said."
  (let* ((limit (or limit aob-session-tail-limit))
         (head (format "%s (%s)" (aob-session-name s) (aob-session-state s)))
         (marker "[earlier events not shown]")
         (room (- limit (length head) (length marker) 4))
         (blocks nil)
         (cut nil))
    (catch 'full
      (dolist (ev (aob-session-events s))
        (when-let* ((block (aob-trace--tail-block ev)))
          (let ((block (substring-no-properties block)))
            (when (> (+ (length block) 2) room)
              (setq cut t)
              (when (and (null blocks) (> room 2))
                (push (substring block (- (length block) (- room 2))) blocks))
              (throw 'full nil))
            (push block blocks)
            (setq room (- room (length block) 2))))))
    (string-join (append (list head) (and cut (list marker)) blocks) "\n\n")))

(defun aob-trace--exit-code (ev lines)
  "The exit status EV's command reported, or nil when it said none."
  (let* ((raw (plist-get ev :rawOutput))
         (plist (and (consp raw) (keywordp (car raw)) raw)))
    (or (seq-some (lambda (p)
                    (and p (seq-some (lambda (k) (let ((v (plist-get p k))) (and (integerp v) v)))
                                     '(:exit_code :exitCode :exit :returnCode))))
                  (list plist (plist-get plist :metadata)))
        (and lines (string-match "\\`Exit code \\([0-9]+\\)" (car lines))
             (string-to-number (match-string 1 (car lines)))))))

(defconst aob-trace--ref-hint "[:(][0-9]+\\|line [0-9]+"
  "A line that might name a place in a file; only these are matched in full.")

(defvar aob-trace--refs (make-hash-table :test 'equal)
  "Output line to its drawn form, so a card redrawn as it streams matches once.")

(defvar compilation-error-regexp-alist)
(defvar compilation-error-regexp-alist-alist)

(defun aob-trace--linkify (line root)
  "LINE with a file:line in it made a button RET opens, as compilation does."
  (if (not (string-match-p aob-trace--ref-hint line))
      line
    (require 'compile)
    (let ((key (cons root line)))
      (or (gethash key aob-trace--refs)
          (let ((out (or (ignore-errors
                          (catch 'found
                           (dolist (entry (cons 'gnu (remq 'gnu compilation-error-regexp-alist)))
                             (let* ((spec (if (symbolp entry)
                                              (cdr (assq entry compilation-error-regexp-alist-alist))
                                            entry))
                                    (re (car-safe spec))
                                    (fi (nth 1 spec))
                                    (li (nth 2 spec))
                                    (fi (if (consp fi) (car fi) fi))
                                    (li (if (consp li) (car li) li)))
                               (when (and (stringp re) (integerp fi) (string-match re line)
                                          (match-beginning fi))
                                 (let* ((file (let ((file-name-handler-alist nil))
                                                (expand-file-name (match-string fi line) root)))
                                        (n (and (integerp li) (match-beginning li)
                                                (string-to-number (match-string li line))))
                                        (beg (match-beginning fi))
                                        (end (max (match-end fi)
                                                  (or (and (integerp li) (match-end li)) 0))))
                                   ;; a handled name (TRAMP above all) would dial out mid-draw
                                   (when (and (not (find-file-name-handler file 'file-exists-p))
                                              (file-exists-p file))
                                     (let ((copy (copy-sequence line)))
                                       (add-text-properties
                                        beg end (list 'aob-file (list file n nil)
                                                      'help-echo (abbreviate-file-name file))
                                        copy)
                                       (aob-trace--add-face copy 'aob-trace-target beg end)
                                       (throw 'found copy)))))))))
                         line)))
            (when (> (hash-table-count aob-trace--refs) 4000)
              (clrhash aob-trace--refs))
            (puthash key out aob-trace--refs))))))

(defun aob-trace--more (n)
  "The line under a card saying N lines are held back."
  (propertize (format "  … %d more line%s" n (if (= n 1) "" "s"))
              'font-lock-face 'shadow))

(defun aob-trace--shell-card (ev)
  "EV, a command the agent ran, as a card: the command, its output cut to
`aob-trace-shell-lines\=' unless opened, and how it ended on the right."
  (let* ((cmd (split-string (aob-trace--shell-command ev) "\n"))
         (out (aob-trace--shell-output ev))
         (code (aob-trace--exit-code ev out))
         (status (plist-get ev :status))
         (meta (string-join
                (delq nil (list (cond ((member status '("pending" "in_progress")) "running")
                                      ((and code (/= code 0)) (format "exit %d" code))
                                      ((equal status "failed") "failed"))
                                (aob-trace--elapsed ev)))
                " · "))
         (room (max 20 (- (aob-trace--text-width) (string-width meta) 6)))
         (first (if (or aob-trace--opening (null (cdr cmd)))
                    (car cmd)
                  (concat (car cmd) " …")))
         (head (aob-trace--right
                (concat (propertize "$ " 'font-lock-face 'shadow)
                        (if aob-trace--opening first
                          (truncate-string-to-width first room nil nil "…")))
                meta))
         (cap (if aob-trace--opening 2000 aob-trace-shell-lines))
         (root (or (aob-trace--root) default-directory))
         (shown (seq-take out cap))
         (linked 0)
         (lines (append
                 (list head)
                 (and aob-trace--opening
                      (mapcar (lambda (l) (concat "  " l)) (cdr cmd)))
                 (mapcar (lambda (l)
                           (concat "  " (aob-trace--add-face
                                         (copy-sequence
                                          (let ((l (if (> (length l) 400)
                                                      (truncate-string-to-width l 400 nil nil "…")
                                                    l)))
                                           (if (or (> (setq linked (1+ linked)) 300)
                                                   (file-remote-p root))
                                               l
                                             (aob-trace--linkify l root))))
                                         'aob-trace-output)))
                         shown)
                 (and (> (length out) cap)
                      (list (aob-trace--more (- (length out) cap)))))))
    (aob-trace--tight (aob-trace--small (string-join lines "\n")))))

(defun aob-trace--diff-items (ev)
  "The file changes EV carries, each a plist with a path and old and new text."
  (and (eq (plist-get ev :type) 'tool)
       (seq-filter (lambda (c) (equal (plist-get c :type) "diff"))
                   (plist-get ev :content))))

(defun aob-trace--lcs (a b)
  "Edit script turning vector A into vector B: a list of (OP . ITEM),
OP one of same, del, add.  Common ends are peeled first, and a middle
too large to compare is taken as all gone and all new."
  (let* ((n (length a)) (m (length b)) (pre 0) (post 0))
    (while (and (< pre n) (< pre m) (equal (aref a pre) (aref b pre)))
      (setq pre (1+ pre)))
    (while (and (< post (- n pre)) (< post (- m pre))
                (equal (aref a (- n post 1)) (aref b (- m post 1))))
      (setq post (1+ post)))
    (let* ((an (- n pre post)) (bm (- m pre post))
           (mid
            (if (> (* an bm) 90000)
                (append (mapcar (lambda (i) (cons 'del (aref a (+ pre i)))) (number-sequence 0 (1- an)))
                        (mapcar (lambda (j) (cons 'add (aref b (+ pre j)))) (number-sequence 0 (1- bm))))
              (let ((dp (make-vector (* (1+ an) (1+ bm)) 0))
                    (w (1+ bm)) (out nil) (i 0) (j 0))
                (dotimes (ii an)
                  (let ((i (- an ii 1)))
                    (dotimes (jj bm)
                      (let ((j (- bm jj 1)))
                        (aset dp (+ (* i w) j)
                              (if (equal (aref a (+ pre i)) (aref b (+ pre j)))
                                  (1+ (aref dp (+ (* (1+ i) w) (1+ j))))
                                (max (aref dp (+ (* (1+ i) w) j))
                                     (aref dp (+ (* i w) (1+ j))))))))))
                (while (or (< i an) (< j bm))
                  (cond ((and (< i an) (< j bm)
                              (equal (aref a (+ pre i)) (aref b (+ pre j))))
                         (push (cons 'same (aref a (+ pre i))) out)
                         (setq i (1+ i) j (1+ j)))
                        ((and (< i an)
                              (or (>= j bm)
                                  (>= (aref dp (+ (* (1+ i) w) j))
                                      (aref dp (+ (* i w) (1+ j))))))
                         (push (cons 'del (aref a (+ pre i))) out)
                         (setq i (1+ i)))
                        (t (push (cons 'add (aref b (+ pre j))) out)
                           (setq j (1+ j)))))
                (nreverse out)))))
      (append (mapcar (lambda (i) (cons 'same (aref a i))) (number-sequence 0 (1- pre)))
              mid
              (mapcar (lambda (i) (cons 'same (aref a i))) (number-sequence (- n post) (1- n)))))))

(defun aob-trace--words (line)
  "LINE as a vector of words, runs of space and single other characters."
  (let ((i 0) (out nil))
    (while (string-match "\\w+\\|\\s-+\\|." line i)
      (push (match-string 0 line) out)
      (setq i (match-end 0)))
    (vconcat (nreverse out))))

(defun aob-trace--refine (old new)
  "OLD and NEW, a removed line and the line that replaced it, with the
words that differ marked: struck through in one, bold in the other."
  (if (or (> (length old) 300) (> (length new) 300))
      (cons old new)
    (let ((o "") (n ""))
      (pcase-dolist (`(,op . ,w) (aob-trace--lcs (aob-trace--words old) (aob-trace--words new)))
        (pcase op
          ('same (setq o (concat o w) n (concat n w)))
          ('del (setq o (concat o (if (string-blank-p w) w
                                     (propertize w 'font-lock-face 'aob-trace-diff-refine-removed)))))
          ('add (setq n (concat n (if (string-blank-p w) w
                                     (propertize w 'font-lock-face 'aob-trace-diff-refine-added)))))))
      (cons o n))))

(defvar aob-trace--diff-cache (make-hash-table :test 'eq :weakness 'key)
  "Diff item to its edit script: an edit is compared once, not per redraw.")

(defun aob-trace--diff-ops (item)
  "ITEM's change as ((OP . LINE)...), its replaced lines refined word by word.
A run of more than forty replaced lines is shown whole, unrefined."
  (or (gethash item aob-trace--diff-cache)
      (puthash item (aob-trace--diff-ops-1 item) aob-trace--diff-cache)))

(defun aob-trace--diff-ops-1 (item)
  (let* ((old (plist-get item :oldText))
         (new (or (plist-get item :newText) ""))
         (ops (aob-trace--lcs (vconcat (if (stringp old) (split-string old "\n") nil))
                              (vconcat (split-string new "\n"))))
         (out nil))
    (while ops
      (if (eq (caar ops) 'same)
          (push (pop ops) out)
        (let ((dels nil) (adds nil))
          (while (and ops (eq (caar ops) 'del)) (push (cdr (pop ops)) dels))
          (while (and ops (eq (caar ops) 'add)) (push (cdr (pop ops)) adds))
          (setq dels (nreverse dels) adds (nreverse adds))
          (let ((pairs (if (> (max (length dels) (length adds)) 40)
                           0
                         (min (length dels) (length adds))))
                (rd nil) (ra nil))
            (dotimes (k (max (length dels) (length adds)))
              (let ((d (nth k dels)) (a (nth k adds)))
                (if (< k pairs)
                    (let ((r (aob-trace--refine d a)))
                      (push (car r) rd) (push (cdr r) ra))
                  (when d (push d rd))
                  (when a (push a ra)))))
            (dolist (d (nreverse rd)) (push (cons 'del d) out))
            (dolist (a (nreverse ra)) (push (cons 'add a) out))))))
    (nreverse out)))

(defun aob-trace--diff-lines (ops)
  "OPS drawn as lines: two lines of what was kept around each change."
  (let* ((v (vconcat ops)) (n (length v)) (keep (make-bool-vector n nil)) (out nil) (gap nil))
    (dotimes (i n)
      (unless (eq (car (aref v i)) 'same)
        (dotimes (k 5)
          (let ((j (+ i (- k 2))))
            (when (and (>= j 0) (< j n)) (aset keep j t))))))
    (dotimes (i n)
      (if (not (aref keep i))
          (setq gap t)
        (when (and gap out) (push (propertize "  ⋯" 'font-lock-face 'shadow) out))
        (setq gap nil)
        (pcase-let ((`(,op . ,line) (aref v i)))
          (push (pcase op
                  ('same (concat (propertize "  " 'font-lock-face 'shadow)
                                 (aob-trace--add-face (copy-sequence line) 'aob-trace-diff-context)))
                  ('del (concat (propertize "− " 'font-lock-face 'shadow)
                                (aob-trace--add-face (copy-sequence line) 'aob-trace-diff-removed)))
                  ('add (concat (propertize "+ " 'font-lock-face 'shadow)
                                (aob-trace--add-face (copy-sequence line) 'aob-trace-diff-added))))
                out))))
    (nreverse out)))

(defun aob-trace--diff-card (ev)
  "EV's edits as a card per file: the file, what it added and removed, and
the change itself, cut to `aob-trace-diff-lines\=' unless opened."
  (let ((budget (if aob-trace--opening most-positive-fixnum aob-trace-diff-lines))
        (held 0) (lines nil))
    (seq-doseq (item (aob-trace--diff-items ev))
      (let* ((ops (aob-trace--diff-ops item))
             (plus (seq-count (lambda (o) (eq (car o) 'add)) ops))
             (minus (seq-count (lambda (o) (eq (car o) 'del)) ops))
             (path (or (plist-get item :path) "?"))
             (find (cdr (or (seq-find (lambda (o) (and (eq (car o) 'add)
                                                       (not (string-blank-p (cdr o)))))
                                      ops)
                            (seq-find (lambda (o) (eq (car o) 'same)) ops))))
             (chip (aob-trace--chip path))
             (head (concat chip (propertize (format "  +%d −%d" plus minus)
                                            'font-lock-face 'shadow)))
             (body (aob-trace--diff-lines ops)))
        (setq head (propertize head 'aob-file
                               (list (car (get-text-property 0 'aob-file chip)) nil
                                     (and find (string-trim (substring-no-properties find))))))
        (push head lines)
        (dolist (l body)
          (if (> budget 0)
              (progn (push l lines) (setq budget (1- budget)))
            (setq held (1+ held))))))
    (when (> held 0) (push (aob-trace--more held) lines))
    (aob-trace--tight (aob-trace--small (string-join (nreverse lines) "\n")))))

(defun aob-trace--card-p (ev)
  "Non-nil when EV draws as a card: a command run, or an edit with its diff."
  (and (eq (plist-get ev :type) 'tool)
       (or (equal (plist-get ev :kind) "execute")
           (aob-trace--diff-items ev))))

(defun aob-trace--card (ev)
  "EV as its card."
  (if (aob-trace--diff-items ev)
      (aob-trace--diff-card ev)
    (aob-trace--shell-card ev)))

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

(defconst aob-trace--literal-re "\\`[ \t]*[|│├]"
  "A line drawn to a grid of its own: a table row keeps its width.")

(defun aob-trace--prose-lines (str fn)
  "Call FN with the bounds of each line of STR that is prose.
A fenced block and a table are not: they keep the whole width and the
default size, since their columns line up only at that size."
  (let ((i 0) (n (length str)) (fence nil))
    (while (<= i n)
      (let* ((eol (or (string-search "\n" str i) n))
             (fenced (and (< i eol)
                          (string-prefix-p "```" (string-trim-left
                                                  (substring-no-properties
                                                   str i (min eol (+ i 40))))))))
        (cond (fenced (setq fence (not fence)))
              ((or fence (= i eol)
                   (string-match-p aob-trace--literal-re
                                   (substring-no-properties str i (min eol (+ i 8))))))
              (t (funcall fn i eol)))
        (setq i (1+ eol))))))

(defun aob-trace--measure-width ()
  "Columns prose wraps at here, or nil when the window is narrower than that."
  (and (> aob-trace-prose-width 0)
       (> (aob-trace--text-width)
          (+ 2 (ceiling (* aob-trace-prose-width (max 1 aob-trace-prose-height)))))
       aob-trace-prose-width))

(defun aob-trace--fill-line (str beg end width)
  "Break STR's line between BEG and END at WIDTH visible columns.
The break is a space shown as a newline: the text stays one line, so
copying it, searching it and every place kept in it stay as they were."
  (let* ((lead (progn (string-match "[ \t]*\\(?:\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\)?" str beg)
                      (min 8 (- (match-end 0) beg))))
         (hang (if (> lead 0) (concat "\n" (make-string lead ?\s)) "\n"))
         (col 0) (space nil) (space-col 0) (i beg))
    (while (< i end)
      (let ((c (aref str i)))
        (unless (get-text-property i 'invisible str)
          (when (and (eq c ?\s) (>= i (+ beg lead))
                     (not (get-text-property i 'display str)))
            (setq space i space-col col))
          (setq col (+ col (char-width c)))
          (when (and (> col width) space)
            (put-text-property space (1+ space) 'display hang str)
            (setq col (+ lead (- col space-col 1)) space nil))))
      (setq i (1+ i)))))

(defun aob-trace--prose (str)
  "STR in the prose face, applied as a property so a window-level
remap such as `ygg-focus-dim' cannot outrank it."
  (let ((copy (copy-sequence str))
        (width (aob-trace--measure-width)))
    ;; markdown arrives as `font-lock-face'; putting prose in `face'
    ;; does not merge with it, it hides it — the alias is a fallback
    ;; for when `face' is absent.  Prose goes under the same property,
    ;; last, so bold and code keep what they set and inherit the rest.
    (aob-trace--prose-lines
     copy
     (lambda (beg end)
       (let ((i beg))
         (while (< i end)
           (let* ((next (next-single-property-change i 'font-lock-face copy end))
                  (cur (get-text-property i 'font-lock-face copy)))
             (put-text-property i next 'font-lock-face
                                (append (aob-trace--faces-of cur)
                                        (list 'aob-trace-prose))
                                copy)
             (setq i next))))
       (when width (aob-trace--fill-line copy beg end width))))
    (if (not (aob-trace--delta-p))
        copy
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
            (when (and (eq (aref copy i) ?\s)
                       (not (get-text-property i 'display copy)))
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

(defun aob-trace--thought-lines (ev)
  "How many lines with something on them EV's thought runs to.
Counted over the pieces as they arrived, so the count is the same
whether or not they have been joined."
  (let* ((text (plist-get ev :text))
         (parts (plist-get ev :parts))
         (hit (plist-get ev :thought-count))
         (seen (and hit (eq (nth 0 hit) text) (nth 1 hit)))
         (fresh-parts nil)
         (tail parts))
    (while (and tail (not (eq tail seen)))
      (push (car tail) fresh-parts)
      (setq tail (cdr tail)))
    (let* ((resume (and hit (eq (nth 0 hit) text) (eq tail seen)))
           (n (if resume (nth 2 hit) 0))
           (fresh (if resume (nth 3 hit) t)))
      (dolist (part (if resume fresh-parts (cons text (reverse parts))))
        (when (stringp part)
          (let ((i 0) (len (length part)))
            (while (< i len)
              (if fresh
                  (let ((c (string-match "[^ \t\n]\\|\n" part i)))
                    (cond ((null c) (setq i len))
                          ((eq (aref part c) ?\n) (setq i (1+ c)))
                          (t (setq n (1+ n) fresh nil i (1+ c)))))
                (let ((nl (string-search "\n" part i)))
                  (if nl (setq fresh t i (1+ nl)) (setq i len))))))))
      (plist-put ev :thought-count (list text parts n fresh))
      (max 1 n))))

(defun aob-trace--plain-line (ev time)
  "EV as one row behind its clock: the shape a log reads in."
  (progn
    (pcase (plist-get ev :type)
      ('tool
       (aob-trace--hang
        ev
        (cond
         ((aob-trace--card-p ev)
          (let ((stamp (aob-trace--stamp time)))
            (if (string-empty-p stamp)
                (aob-trace--card ev)
              (concat (aob-trace--small (concat stamp " ")) (aob-trace--card ev)))))
         ((aob-trace--delta-p)
          (aob-trace--small
           (concat
            (propertize (concat (if (aob-trace--sub-p ev) "└ " "")
                                (aob-trace--verb ev) " ")
                        'font-lock-face (aob-trace--tool-face ev))
            (aob-trace--chipped ev (aob-trace--short-target ev))
            (let ((st (plist-get ev :status)))
              (pcase st
                ((or "completed" "success" 'nil) "")
                (_ (let ((mark (aob-trace--status ev)))
                     (if (string-empty-p mark) "" (concat " " mark))))))
            (aob-trace--rollup ev))))
         (t
          (aob-trace--small
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
                               (aob-trace--chipped
                                ev (or (plist-get ev :title) (plist-get ev :kind) ""))
                               (let ((st (aob-trace--status ev)))
                                 (unless (string-empty-p st) st))
                               (plist-get ev :stat)))
               " ")
              (aob-trace--rollup ev))))))))
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
                      (aob-trace--small
                       (propertize (let ((n (aob-trace--thought-lines ev)))
                                     (format "thinking · %d line%s" n (if (= n 1) "" "s")))
                                   'font-lock-face 'aob-trace-thinking)))
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
      (let ((l (aob-trace--build-line s ev nil)))
        (plist-put ev :line l)
        l)))

(defun aob-trace--open-line (s ev)
  "EV's card drawn whole, kept while the collapsed line it was drawn with is."
  (let ((line (aob-trace--line-cached s ev))
        (hit (plist-get ev :line-open)))
    (if (and hit (eq (car hit) line))
        (cdr hit)
      (let ((l (aob-trace--build-line s ev t)))
        (plist-put ev :line-open (cons line l))
        l))))

(defun aob-trace--gap-class (ev)
  "Words when EV opens a turn or is someone speaking, else work."
  (if (or (memq (plist-get ev :type) '(message prompt permission error))
          (plist-get ev :turn-head))
      'words
    'work))

(defun aob-trace--build-line (s ev open)
  "EV's line with its fringe mark, owned by EV; its card whole when OPEN."
  (let ((aob-trace--opening open))
    (propertize (aob-trace--mark
                 (aob-trace--state ev)
                 (aob-trace--line ev (and (fboundp 'aob-session-ref)
                                          (aob-session-ref s :agent))))
                'aob-session (aob-session-id s)
                'aob-event (plist-get ev :seq)
                'aob-gap (aob-trace--gap-class ev))))

(defcustom aob-trace-thinking-autohide nil
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
  (let ((open (or (memq (plist-get ev :seq) aob-trace--expanded)
                  (aob-trace--live-thought-p s ev))))
    (cond
     ((and open (aob-trace--card-p ev))
      (aob-trace--open-line s ev))
     ((not open)
      (cond ((plist-get ev :decision-kind)
             (aob-trace--decision s ev (aob-trace--line-cached s ev)))
            ((eq (plist-get ev :type) 'permission)
             (aob-trace--mark (and (aob-trace--pending s (plist-get ev :seq)) 'waiting)
                              (aob-trace--annotate s ev (aob-trace--line-cached s ev))))
            (t (aob-trace--annotate
                s ev (aob-trace--questions s ev (aob-trace--line-cached s ev))))))
     ;; subagent steps are NOT inlined here — they have their own trace
     ;; (the row under the trace, `aob-subagents'); an expanded Task shows detail
     (t (concat (aob-trace--line-cached s ev) "\n"
                (aob-trace--detail-block s ev))))))

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
                    (aob-trace--mark
                     'done
                     (aob-trace--tight
                      (if open
                          (concat (aob-trace--small (aob-trace--explore-line evs)) "\n"
                                  (mapconcat (lambda (l) (concat "    " l))
                                             lines "\n"))
                        (aob-trace--small (aob-trace--explore-line evs)))))
                    'aob-session (aob-session-id s)
                    'aob-event (plist-get head :seq)
                    'aob-gap (aob-trace--gap-class head))))
        (plist-put head :group (cons stamp block))
        block))))

(defface aob-trace-note '((t :inherit default))
  "Face for a message the editor sent on your behalf.
It reads like prose; only a note still queued leans."
  :group 'aob)

(defun aob-trace--editor-p (ev)
  "Non-nil when EV is a prompt the editor sent for you rather than one you typed.
A prompt read back from a written conversation says nothing either way,
and is yours."
  (and (eq (plist-get ev :type) 'prompt)
       (plist-member ev :typed)
       (not (plist-get ev :typed))))

(defun aob-trace--note-parts (ev)
  "EV's words as (HEAD . BODY): its first line with anything on it, and the rest."
  (let ((text (aob-event-text ev))
        (hit (plist-get ev :note-parts)))
    (if (and hit (eq (car hit) text))
        (cdr hit)
      (let* ((lines (seq-remove (lambda (l) (string-match-p "\\`[ \t]*\\(?:-\\{3,\\}\\)?[ \t]*\\'" l))
                                (split-string text "\n")))
             (parts (cons (string-trim (or (car lines) ""))
                          (string-trim (string-join (cdr lines) "\n")))))
        (plist-put ev :note-parts (cons text parts))
        parts))))

(defun aob-trace--note-key (ev)
  "What EV says, for telling one note from the next: its body, else its head."
  (let ((parts (aob-trace--note-parts ev)))
    (if (string-empty-p (cdr parts)) (car parts) (cdr parts))))

(defun aob-trace--notes-ahead (evs)
  "How many notes from the editor EVS starts with that say the same thing."
  (if (not (aob-trace--editor-p (car evs)))
      0
    (let ((key (aob-trace--note-key (car evs))) (n 0))
      (while (and evs (aob-trace--editor-p (car evs))
                  (equal (aob-trace--note-key (car evs)) key))
        (setq n (1+ n) evs (cdr evs)))
      n)))

(defun aob-trace--note-line (evs open)
  "The one row EVS, notes that say the same thing, fold into."
  (let* ((parts (mapcar #'aob-trace--note-parts evs))
         (n (length evs))
         (head (car (car parts)))
         (said (car (split-string (cdr (car parts)) "\n" t)))
         (text (cond ((and (> n 1)
                           (seq-every-p (lambda (p) (string-match-p "\\`Subagent .* finished:?\\'" (car p)))
                                        parts))
                      (concat (format "%d subagents finished" n) (if said (concat " · " said) "")))
                     ((and said (string-suffix-p ":" head)) (concat head " " said))
                     (t head)))
         (room (max 20 (- (or (aob-trace--measure-width) (aob-trace--text-width)) 10)))
         (queued (seq-find (lambda (e) (equal (plist-get e :status) "queued")) evs)))
    (concat (propertize (concat (if open "▾ " "▸ ")
                                (truncate-string-to-width text room nil nil "…")
                                (if (> n 1) (format " ×%d" n) ""))
                        'font-lock-face 'aob-trace-note)
            (if queued (propertize " ⋯" 'font-lock-face 'shadow) ""))))

(defun aob-trace--note-block (s evs)
  "EVS, notes from the editor that say the same thing, as one folded row.
Open, each note is under it whole, owned by its own event so a queued one
can still be rewritten, moved or dropped."
  (let* ((head (car evs))
         (seq (plist-get head :seq))
         (open (and (memq seq aob-trace--expanded) t))
         (key (list open (aob-trace--text-width) (aob-trace--measure-width)
                    (mapcar (lambda (e) (list (plist-get e :seq) (plist-get e :status)
                                              (aob-event-text e)))
                            evs)))
         (hit (plist-get head :note)))
    (if (and hit (equal (car hit) key))
        (cdr hit)
      (let ((block (aob-trace--tight
                    (concat
                     (propertize (aob-trace--small (aob-trace--note-line evs open))
                                 'aob-event seq)
                     (if open
                         (mapconcat
                          (lambda (e)
                            (propertize
                             (concat "\n  " (replace-regexp-in-string
                                             "\n" "\n  " (aob-trace--bound (aob-event-text e)) t t))
                             'aob-event (plist-get e :seq)
                             'font-lock-face (if (equal (plist-get e :status) "queued")
                                                 'aob-trace-queued
                                               'aob-trace-note)))
                          evs "")
                       "")))))
        (add-text-properties 0 (length block)
                             (list 'aob-session (aob-session-id s) 'aob-fold seq 'aob-gap 'work)
                             block)
        (plist-put head :note (cons key block))
        block))))

(defun aob-trace--tools-ahead (evs)
  "How many tool calls EVS starts with."
  (let ((n 0))
    (while (and evs (eq (plist-get (car evs) :type) 'tool))
      (setq n (1+ n) evs (cdr evs)))
    n))

(defun aob-trace--run-length (pred evs)
  "How many of EVS, from the first, satisfy PRED."
  (let ((n 0))
    (while (and evs (funcall pred (car evs)))
      (setq n (1+ n) evs (cdr evs)))
    n))

(defvar-local aob-trace--open-runs nil
  "Seqs of the tool runs TAB has opened, each the seq of the run's first call.")

(defun aob-trace--run-label (ev)
  "The word a run summary counts EV under."
  (cond ((aob-trace--mcp ev) "mcp")
        ((plist-get ev :subagent) "task")
        (t (pcase (plist-get ev :kind)
             ("execute" "shell")
             ((and k (pred stringp)) k)
             (_ "other")))))

(defun aob-trace--run-line (evs open)
  "The summary row EVS fold into: how many, of which kinds, how long."
  (let ((counts nil) (fails 0) (start nil) (end nil))
    (dolist (ev evs)
      (let* ((label (aob-trace--run-label ev))
             (cell (assoc label counts)))
        (if cell (setcdr cell (1+ (cdr cell))) (push (cons label 1) counts)))
      (when (equal (plist-get ev :status) "failed") (setq fails (1+ fails)))
      (when-let* ((ts (plist-get ev :ts))) (setq start (if start (min start ts) ts)))
      (when-let* ((done (plist-get ev :done-ts))) (setq end (if end (max end done) done))))
    (setq counts (sort (nreverse counts) (lambda (a b) (> (cdr a) (cdr b)))))
    (propertize
     (string-join
      (delq nil
            (list (format "%s %d tools" (if open "▾" "▸") (length evs))
                  (mapconcat (lambda (c) (format "%d %s" (cdr c) (car c))) counts ", ")
                  (and start end (>= (- end start) 1)
                       (aob-duration-short (- end start) t))
                  (and (> fails 0) (format "%d failed" fails))))
      " · ")
     'font-lock-face 'aob-trace-run)))

(defun aob-trace--run-block (s evs head)
  "EVS, a run of tool calls, as one summary row; HEAD when it opens a turn.
Open, every call is under it.  Shut, a call that failed, runs or was
opened by TAB still is.  Kept on the first call so an unchanged
run stays the same string and the incremental pass skips it."
  (let* ((first (car evs))
         (seq (plist-get first :seq))
         (open (and (memq seq aob-trace--open-runs) t))
         (items (mapcar (lambda (ev) (aob-trace--block s ev)) evs))
         (stamp (cons (+ (if open 1 0) (if head 2 0)) items))
         (cached (plist-get first :run)))
    (if (and cached (aob-trace--stamp-eq (car cached) stamp))
        (cdr cached)
      (let* ((state (cond ((seq-find (lambda (e) (equal (plist-get e :status) "failed")) evs)
                           'failed)
                          ((seq-find (lambda (e) (eq (aob-trace--state e) 'running)) evs)
                           'running)
                          (t 'done)))
             (summary (aob-trace--small (aob-trace--run-line evs open)))
             (summary (if (and head (aob-trace--delta-p))
                          (aob-trace--gutter (aob-trace--avatar-glyph) summary)
                        summary))
             (shown (seq-filter
                     (lambda (pair)
                       (let ((ev (car pair)))
                         (or open
                             (memq (aob-trace--state ev) '(failed running))
                             (memq (plist-get ev :seq) aob-trace--expanded))))
                     (cl-mapcar #'cons evs items)))
             (block (propertize
                     (aob-trace--tight
                      (mapconcat #'identity
                                 (cons (propertize summary 'aob-run seq)
                                       (mapcar (lambda (pair)
                                                 (propertize
                                                  (concat "  " (replace-regexp-in-string
                                                                "\n" "\n  " (cdr pair) t t))
                                                  'aob-item (plist-get (car pair) :seq)))
                                               shown))
                                 "\n"))
                     'aob-session (aob-session-id s)
                     'aob-event seq
                     'aob-gap (if head 'words 'work)))
             (block (aob-trace--mark state block)))
        (plist-put first :run (cons stamp block))
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
         (aob-trace--width (aob-trace--text-width))
        ;; the first thing the agent does after you speak — and the first
        ;; thing in the trace — is where its mark belongs
        (opening t))
    (while evs
      (let* ((tool (eq (plist-get (car evs) :type) 'tool))
             (tools (if (and tool (> aob-trace-run-min 0)) (aob-trace--tools-ahead evs) 0))
             (as-run (>= tools (max 1 aob-trace-run-min)))
             (looks (if (and tool (not as-run))
                        (aob-trace--run-length #'aob-trace--explores-p evs)
                      0))
             (run (cond (as-run (take tools evs))
                        ((>= looks aob-trace-explore-min) (take looks evs))))
             (folded (and run t))
             (notes (if tool 0 (aob-trace--notes-ahead evs)))
             (group (cond (folded run)
                          ((> notes 0) (take notes evs))
                          (t (list (car evs)))))
             (run-head nil)
             (first t))
        ;; every event in the group is told whether it opens a turn, not
        ;; just the one that gets drawn: a flag left over from a render
        ;; where it was the head is a mark beside every row it touches
        (dolist (ev group)
          (let ((head (and first opening
                           (memq (plist-get ev :type) '(tool message thought))
                           t)))
            (when as-run
              (setq run-head (or run-head head) head nil))
            (unless (eq (and (plist-get ev :turn-head) t) head)
              (plist-put ev :turn-head head)
              (plist-put ev :line nil))
            (setq first nil)
            (setq opening (or (eq (plist-get ev :type) 'prompt)
                              (and opening
                                   (not (memq (plist-get ev :type)
                                              '(tool message thought))))))))
        (push (cond (as-run (aob-trace--run-block s run run-head))
                    (folded (aob-trace--explore-block s run))
                    ((> notes 0) (aob-trace--note-block s group))
                    (t (aob-trace--block s (car evs))))
              acc)
        (setq evs (nthcdr (length group) evs))))
    (let ((q queued))
      (while q
        (let ((n (aob-trace--notes-ahead q)))
          (if (> n 0)
              (progn (push (aob-trace--note-block s (take n q)) acc)
                     (setq q (nthcdr n q)))
            (push (aob-trace--block s (car q)) acc)
            (setq q (cdr q))))))
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
  (let* ((prose (memq (plist-get ev :type) '(message prompt error)))
         (props (append (list 'aob-session (aob-session-id s)
                              'aob-event (plist-get ev :seq))
                        (cond ((eq (plist-get ev :type) 'thought) '(face aob-trace-thinking))
                              ((not prose) '(face shadow))))))
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
  "The newline that separates one block from the next.
It is spaced for work until the block after it says otherwise."
  (propertize "\n" 'line-spacing aob-trace-gap-work))

(defun aob-trace--same-breaks-p (old new)
  "Non-nil when NEW, which starts with OLD's text, shows that text as OLD does.
A line break measured afresh can land on a space already drawn, and
writing only what was added would leave that line unbroken."
  (let ((i 0) (len (length old)) (same t))
    (while (and same (< i len))
      (let ((next (min (next-single-property-change i 'display old len)
                       (next-single-property-change i 'display new len))))
        (unless (equal (get-text-property i 'display old)
                       (get-text-property i 'display new))
          (setq same nil))
        (setq i next)))
    same))

(defun aob-trace--gap-before (pos block)
  "Space the newline before POS for BLOCK, which starts there."
  (when (> pos (point-min))
    (let ((want (if (eq (get-text-property 0 'aob-gap block) 'words)
                    aob-trace-gap-words
                  aob-trace-gap-work)))
      (unless (equal (get-text-property (1- pos) 'line-spacing) want)
        (put-text-property (1- pos) pos 'line-spacing want)))))

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

(defun aob-trace--say (s text &optional files)
  "Say TEXT to S: into the turn it is running, where it takes that.
A turn already running is not a reason to wait — an agent whose
subagents are working is an agent you can still talk to, and where
the adapter takes steering the words go in without costing it the
work in flight.  FILES, images, ride a prompt: steering carries words."
  (let ((aob-prompt-typed t))
    (if (and (null files)
             (eq (aob-session-state s) 'working)
             (fboundp 'aob-acp--steers-p)
             (ignore-errors (aob-acp--steers-p s)))
        (aob-interject s text)
      (aob-prompt s text files))))

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
      (let ((held (aob-trace--held s)))
        (when (and (string-empty-p text) (not held))
          (user-error "aob: nothing to send"))
        (let ((inhibit-read-only t))
          (delete-region start (point-max)))
        (when (fboundp 'ygg-normal-state) (ygg-normal-state))
        (aob-session-put s :comments nil)
        (aob-trace--say s (string-trim
                           (string-join
                            (delq nil (list (and (fboundp 'aob-context-text)
                                                 (aob-context-text))
                                            (car held)
                                            (unless (string-empty-p text) text)))
                            "\n\n"))
                          (cdr held))))))

(defface aob-trace-anchor
  '((((background dark)) :background "#1c1c1c" :underline "#707070")
    (t :background "#e4dfd3" :underline "#5c5a55"))
  "Face marking text a comment is attached to."
  :group 'aob)

(defface aob-trace-comment
  '((((background dark)) :background "#1c1c1c" :extend t)
    (t :background "#ebe7dd" :extend t))
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
        (file (or (get-text-property (point) 'aob-file)
                  (when-let* ((at (text-property-not-all (line-beginning-position)
                                                         (line-end-position)
                                                         'aob-file nil)))
                    (get-text-property at 'aob-file))))
        (seq (or (get-text-property (point) 'aob-item)
                 (get-text-property (point) 'aob-event)))
        (s (aob-session-get aob-trace--session-id)))
    (cond
     (file (aob-trace--visit file))
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
         (key (list str pending aob-trace-status-gutter (plist-get ev :answer)
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
        (setq block (aob-trace--mark (and pending 'waiting) block))
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
  (let ((cs (and (aob-session-ref s :comments)
                 (aob-trace--comments-for s (plist-get ev :seq)))))
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

(defun aob-trace--add-comment (s seq quoted text &optional files)
  "Hold TEXT as a comment on QUOTED in event SEQ of S, and redraw.
FILES are images that ride with it when the held comments are sent."
  (when (string-empty-p (string-trim text)) (user-error "aob: empty comment"))
  (aob-session-put s :comments
                   (cons (append (list :seq seq :quote quoted :text text
                                       :ts (float-time))
                                 (and files (list :files files)))
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
         (seq (or (get-text-property start 'aob-item)
                  (get-text-property start 'aob-event)))
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

(defvar aob-trace-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") #'aob-trace-comment-send-now)
    (define-key map (kbd "<s-return>") #'aob-trace-comment-send-now)
    map)
  "Keys a draft anchored to a trace line adds to compose's own.")

(define-minor-mode aob-trace-comment-mode
  "A compose draft that is a comment on a trace line.
Its send holds the comment for the next message; C-return holds it and
sends every comment held."
  :lighter nil)

(defun aob-trace--comment-box (trace seq quoted pos &optional s)
  "Open a draft under the line POS is on in TRACE, on QUOTED in SEQ.
S is the session the comment goes to when TRACE is not its trace.  One
draft per session, event and words: a comment left unsent is still
there when the same line is commented on again."
  (let ((s (or s (aob-session-get
                  (buffer-local-value 'aob-trace--session-id trace)))))
    (unless s (user-error "aob: no session to comment to"))
    (let* ((line (with-current-buffer trace
                   (save-excursion (goto-char pos) (line-beginning-position))))
           (hold (lambda (text files)
                   (aob-trace--hold-comment trace s seq quoted text files)))
           (buf (aob-compose s nil
                             (format "comment:%s:%s:%s" (aob-session-name s)
                                     (or seq "-") (substring (md5 quoted) 0 6))
                             nil (list (get-buffer-window trace) line hold))))
      (with-current-buffer buf
        (aob-trace-comment-mode 1)
        (setq aob-compose--label
              (concat "comment on: "
                      (truncate-string-to-width
                       (replace-regexp-in-string "\n" " " quoted)
                       60 nil nil "…")))
        (setq aob-compose--tags '("ZZ holds" "C-RET sends all"))
        (goto-char (point-max))
        (when-let* ((win (get-buffer-window buf t)))
          (set-window-point win (point-max)))
        (force-mode-line-update))
      buf)))

(defun aob-trace--hold-comment (from s seq quoted text files)
  "Hold TEXT and FILES on QUOTED in SEQ of S, from the buffer FROM."
  (if (buffer-live-p from)
      (with-current-buffer from (aob-trace--add-comment s seq quoted text files))
    (aob-trace--add-comment s seq quoted text files)))

(defun aob-trace-comment-send-now ()
  "Hold this comment, then send every comment held for its session.
While its agent waits on a question or a plan, the held answers go to
that instead."
  (interactive)
  (let ((s (and (stringp aob-compose--target)
                (aob-session-get aob-compose--target))))
    (unless s (user-error "aob: no session to send to"))
    (aob-compose-send)
    (if-let* ((waiting (aob-trace-waiting-decision s)))
        (progn (aob-trace--answer-decision s waiting nil)
               (when-let* ((trace (get-buffer (aob-trace--name s))))
                 (with-current-buffer trace (aob-trace--render t))))
      (when-let* ((held (aob-trace--held s)))
        (aob-session-put s :comments nil)
        (aob-trace--say s (car held) (cdr held))))))

(defun aob-trace--lifting-box-p (buffer)
  "Whether BUFFER floating over a trace should lift the trace's end.
Only the compose box at the foot does; a draft under a line stands
where it was asked for."
  (with-current-buffer buffer
    (and (derived-mode-p 'aob-compose-mode) (not aob-compose--anchor))))

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
                   (aob-trace--lifting-box-p b)))
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

(defun aob-trace--held (s)
  "The comments held for S as one message and its images, (TEXT . FILES).
Nil when none are held; every path that sends them sends both."
  (when-let* ((text (aob-trace--comments-message s)))
    (cons text (seq-mapcat (lambda (c) (plist-get c :files))
                           (aob-trace--comments s)))))

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

(add-hook 'aob-meter-change-hook #'aob--dirty)

(defun aob-trace--tokens-round (n)
  "N tokens as the header says them: 213k, 1.2M."
  (cond ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
        ((>= n 1000) (format "%dk" (round n 1000)))
        (t (number-to-string n))))

(defun aob-trace--header (s)
  "S's header: name, state, clock, cost, context and todo, grey but the name.
A subagent's names the agent it works for instead of cost and context.
The full account is \\ u; the header carries only what is looked at."
  (let* ((grey (lambda (str) (propertize (string-replace "%" "%%" str) 'face 'shadow)))
         (mode (aob-session-ref s :mode-id))
         (parent (and aob-trace--own-parent
                      (aob-session-get (aob-session-ref s :native-root))))
         (clock (aob-session-clock s))
         (cost (let ((c (aob-session-cost s)))
                 (and c (> c 0) (aob-cost-short c (aob-session-ref s :cost-currency)))))
         (used (or (aob-session-ref s :ctx-used)
                   (plist-get (aob-session-ref s :usage) :totalTokens)))
         (ctx (and (numberp used) (> used 0)
                   (concat (propertize (aob-trace--tokens-round used)
                                       'face (if (and (> aob-trace-rot-window 0)
                                                      (> used aob-trace-rot-window))
                                                 'warning 'shadow))
                           (funcall grey " ctx"))))
         (todo (when-let* (((fboundp 'ygg-todo-session-file))
                           (file (ygg-todo-session-file s))
                           (progress (ygg-todo-progress file))
                           ((> (cdr progress) 0)))
                 (format "%d/%d" (car progress) (cdr progress))))
         (goal (when-let* ((g (aob-session-ref s :goal)))
                 (if-let* ((n (plist-get g :iterations))) (format "goal %d×" n) "goal")))
         (wf (when-let* ((w (aob-session-ref s :wf-name)))
               (format "wf:%s +%d" w (length (aob-session-ref s :wf-stages)))))
         (parts (if aob-trace--own-parent
                    (list (and parent (concat "subagent of " (aob-session-name parent)))
                          (unless parent "subagent")
                          "read-only"
                          (format "%s" (aob-session-state s))
                          clock)
                  (list (format "%s" (aob-session-state s))
                        (aob-session-quiet s)
                        (and mode (not (member mode aob-trace-quiet-modes)) mode)
                        clock cost ctx todo goal wf))))
    (concat " " (string-replace "%" "%%" (aob-session-name s))
            (mapconcat (lambda (p) (concat (funcall grey " · ")
                                           (if (text-property-any 0 (length p) 'face 'warning p)
                                               p
                                             (funcall grey p))))
                       (delq nil parts) ""))))

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
  (cons (when-let* ((prop (if (get-text-property pos 'aob-item) 'aob-item 'aob-event))
                    (seq (get-text-property pos prop))
                    (beg (text-property-any (point-min) (point-max) prop seq)))
          (save-excursion
            (goto-char pos)
            (let ((bol (line-beginning-position)))
              (list (cons prop seq)
                    (count-lines (save-excursion (goto-char beg) (line-beginning-position)) bol)
                    (- pos bol)))))
        (copy-marker pos)))

(defun aob-trace--place-pos (place)
  "Where PLACE, as aob-trace--place took it, stands now; its marker is let go.
A call folded into a run since is found by the item it became."
  (pcase-let ((`((,key ,line ,col) . ,marker) place))
    (prog1 (or (when-let* ((beg (and key
                                     (or (text-property-any (point-min) (point-max)
                                                            (car key) (cdr key))
                                         (text-property-any (point-min) (point-max)
                                                            (if (eq (car key) 'aob-item)
                                                                'aob-event 'aob-item)
                                                            (cdr key))))))
                 (save-excursion
                   (goto-char beg)
                   (forward-line line)
                   (min (+ (point) col) (line-end-position))))
               (marker-position marker))
      (set-marker marker nil))))

(defun aob-trace--on-page (win pos)
  "POS, or the start of WIN's last whole line when POS is below the page."
  (if (or (< pos (window-start win))
          ;; no layout to ask (batch, the initial frame): every answer is nil
          (not (pos-visible-in-window-p (window-start win) win t))
          (pos-visible-in-window-p pos win))
      pos
    (with-selected-window win
      (save-excursion
        (move-to-window-line -1)
        (unless (pos-visible-in-window-p (point) win)
          (vertical-motion -1))
        (point)))))

(defun aob-trace--render-1 (s)
  (aob-trace--follow-root s)
  ;; whichever event is still being written: decorating it is work that
  ;; will be thrown away by the next chunk
  (setq aob-trace--live-seq
        (and (eq (aob-session-state s) 'working)
             (plist-get (seq-find (lambda (e) (not (aob-trace--sub-p e)))
                                  (aob-session-events s))
                        :seq)))
  (setq header-line-format (aob-trace--header s))
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
            (when n (aob-trace--gap-before pos n))
            (cond
             ((and n o (equal n o))
              (cl-incf pos (1+ (length o)))
              (pop new) (pop old))
             ((and n o (eql ns os))
              (if (and (> (length n) (length o))
                       (eq t (compare-strings o nil nil n nil (length o)))
                       (aob-trace--same-breaks-p o n))
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
            ;; a cursor a growing block pushed off the page drags the page after it
            (set-window-point win (aob-trace--on-page win pt)))))))
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
  (let ((seq (or (get-text-property (point) 'aob-item)
                 (get-text-property (point) 'aob-event))))
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
  "Expand or collapse the event at point: a call inside a run, the run
itself on its summary row, else the block point is in."
  (interactive)
  (let ((item (get-text-property (point) 'aob-item))
        (run (get-text-property (point) 'aob-run))
        (seq (or (get-text-property (point) 'aob-fold)
                 (get-text-property (point) 'aob-event))))
    (cond (item (setq aob-trace--expanded
                      (if (memq item aob-trace--expanded)
                          (delq item aob-trace--expanded)
                        (cons item aob-trace--expanded))))
          (run (setq aob-trace--open-runs
                     (if (memq run aob-trace--open-runs)
                         (delq run aob-trace--open-runs)
                       (cons run aob-trace--open-runs))))
          (seq (setq aob-trace--expanded
                     (if (memq seq aob-trace--expanded)
                         (delq seq aob-trace--expanded)
                       (cons seq aob-trace--expanded)))))
    (when (or item run seq)
      (aob-trace--render t))))

(defun aob-trace--visit (spec)
  "Open SPEC, a (FILE LINE SEARCH) a chip or a card names, beside the trace.
LINE is where to land; without one, SEARCH is text to land on."
  (pcase-let ((`(,file ,line ,search) spec))
    (unless (file-exists-p file)
      (user-error "aob: %s is not there" (abbreviate-file-name file)))
    (pop-to-buffer (find-file-noselect file)
                   '((display-buffer-reuse-window display-buffer-use-some-window)
                     (inhibit-same-window . t)))
    (widen)
    (goto-char (point-min))
    (cond ((and line (> line 0)) (forward-line (1- line)))
          ((and search (not (string-empty-p search)) (search-forward search nil t))
           (goto-char (match-beginning 0))))))

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
