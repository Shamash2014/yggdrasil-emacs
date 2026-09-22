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
    (define-key map (kbd "RET") #'aob-compose)
    ;; view/select/copy are yggdrasil's own: v visual state, y the yank
    ;; operator (yy, yw, visual-y) — no special-cased copy keys here.
    ;; interacting is the compose buffer, never an in-trace input line:
    ;; every vim way into insert opens it (i keeps its steer meaning —
    ;; cancel first — a/A/o talk without cancelling, like RET)
    (define-key map "a" #'aob-compose)
    ;; vim: A appends at the end — here that is the inline input
    (define-key map "A" #'aob-trace-input)
    (define-key map "C" #'aob-trace-comment)
    (define-key map "M" #'aob-acp-cycle-mode)
    ;; a queued message is still yours until it goes: change it or take
    ;; it back, from the line it is drawn on
    (define-key map "E" #'aob-trace-queue-edit)
    (define-key map "X" #'aob-trace-queue-drop)
    (define-key map "o" #'aob-compose)
    map))

(define-derived-mode aob-trace-mode special-mode "aob-trace"
  "Operation trace of one agent session."
  (ygg-ui-plain-layout)
  (setq truncate-lines nil)
  (setq-local char-property-alias-alist '((face font-lock-face)))
  (visual-line-mode 1)
  (add-to-invisibility-spec 'markdown-markup)
  (when (aob-trace--delta-p)
    (setq-local line-spacing 0.3)
    (setq-local left-margin-width 4)
    (setq-local fill-column aob-trace-measure)
    (setq-local word-wrap t)
    (add-hook 'window-configuration-change-hook #'aob-trace--fit-margins nil t)
    (add-hook 'window-buffer-change-functions
              (lambda (_frame) (aob-trace--fit-margins)) nil t)
    (aob-trace--fit-margins)))

(defun aob-trace--fit-margins ()
  "Hold the prose to `aob-trace-measure' by padding the right margin."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (let* ((total (window-total-width win))
           (slack (max 0 (- total aob-trace-measure 4))))
      (set-window-margins win 4 slack)
      (set-window-fringes win 0 0))))

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

(defcustom aob-trace-measure 78
  "Columns of prose before the right margin takes over, under `delta'."
  :type 'natnum :group 'aob)

(defface aob-trace-prose
  '((t :inherit variable-pitch :family "SF Pro Text" :height 1.15))
  "Face for message and prompt bodies under the delta style."
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
               (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-account_arrow_right_outline" 'shadow) "↳")
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
    ('permission (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-shield_key_outline" 'warning) "✋"))
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
      (ygg-ui-markdown text)
    text))

(defun aob-trace--status (ev)
  (pcase (plist-get ev :status)
    ("queued" (propertize "⋯ queued" 'face 'shadow))
    ("pending" (propertize "…" 'face 'shadow))
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
        (if (and (aob-trace--delta-p) (not (plist-get ev :parent)))
            ""
          (concat "\n"
                  (propertize who 'font-lock-face 'aob-trace-speaker)
                  (if (plist-get ev :parent)
                      (propertize " ↳ subagent" 'font-lock-face 'shadow)
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

(defun aob-trace--verb (ev)
  "The word naming what EV did."
  (or (cdr (assoc (plist-get ev :kind) aob-trace--verbs))
      (capitalize (or (plist-get ev :kind) "Call"))))

(defface aob-trace-card
  '((((background dark)) :background "#1a1a1a" :extend t)
    (t :background "#f4f4f4" :extend t))
  "Face behind a command card."
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
  "A glyph standing in for whoever EV is from, or an empty string."
  (if (not (aob-trace--delta-p))
      ""
    (if (eq (plist-get ev :type) 'prompt)
        (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-account_circle"
                           'aob-trace-speaker)
            "")
      (aob-trace--agent-mark))))

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
                    (aob-trace--md (aob-trace--bound (aob-event-text ev))
                                   (aob-trace--live-p ev)))))
                (when-let* ((n (plist-get ev :images)) ((> n 0)))
                  (concat " " (mapconcat #'identity
                                         (make-list n "[[Image]]") " ")))
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
    (concat (propertize " " 'display `((margin left-margin) ,glyph))
            str)))

(defun aob-trace--prose (str)
  "STR in the prose face, applied as a property so a window-level
remap such as `ygg-focus-dim' cannot outrank it."
  (if (not (aob-trace--delta-p))
      str
    (let ((copy (copy-sequence str)))
      (add-face-text-property 0 (length copy) 'aob-trace-prose t copy)
      (let ((i 0) (len (length copy)))
        (while (< i len)
          (when (eq (aref copy i) ?\n)
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

(defun aob-trace--plain-line (ev time)
  "EV as one row behind its clock: the shape a log reads in."
  (progn
    (pcase (plist-get ev :type)
      ('tool
       (if (aob-trace--delta-p)
           (if (equal (plist-get ev :kind) "execute")
               (aob-trace--card
                ev (or (plist-get ev :title) (plist-get ev :kind) ""))
           (concat
            (aob-trace--prose
             (concat (if (plist-get ev :parent) "↳ " "")
                     (aob-trace--verb ev) " "))
            (propertize (or (plist-get ev :title) (plist-get ev :kind) "")
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
                               (and (plist-get ev :parent) " ↳")
                               (or (plist-get ev :title) (plist-get ev :kind))
                               (let ((st (aob-trace--status ev)))
                                 (unless (string-empty-p st) st))
                               (plist-get ev :stat)))
               " ")
              (aob-trace--rollup ev)))))
      (_ (let ((st (aob-trace--status ev)))
           (funcall
            (if (and (aob-trace--delta-p) (eq (plist-get ev :type) 'thought))
                ;; a thought is the agent speaking: Delta hangs its mark in
                ;; the gutter and leaves the line itself clean
                (lambda (str) (aob-trace--gutter (aob-trace--avatar-glyph) str))
              (lambda (str) (concat (aob-trace--glyph ev) " " str)))
            (format "%s%s%s%s"
                   (let ((stamp (aob-trace--stamp time)))
                     (if (string-empty-p stamp) "" (concat stamp " ")))
                   ;; a subagent's own words, in its own trace
                   (if (plist-get ev :parent) "↳ " "")
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
                          (aob-trace--md
                           (aob-trace--bound (aob-event-text ev)))))
                       (when-let* ((n (plist-get ev :images))
                                   ((> n 0)))
                         (concat " " (mapconcat #'identity
                                                (make-list n "[[Image]]")
                                                " ")))))
                     ('thought
                      (if (aob-trace--delta-p)
                          (propertize (concat "Thinking: " (aob-event-head ev) " ›")
                                      'font-lock-face 'aob-trace-aside)
                        (aob-event-head ev)))
                     ('stop (propertize (aob-event-summary ev)
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
event mutated (chunk pushes, tool updates clear the cache)."
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
       (eq ev (seq-find (lambda (e) (not (plist-get e :parent)))
                        (aob-session-events s)))))

(defun aob-trace--block (s ev)
  "EV's rendered block: its line, plus children and detail when expanded.
A collapsed block IS the cached line string, so an unchanged event stays
`eq' across renders and the incremental pass skips it."
  (if (not (or (memq (plist-get ev :seq) aob-trace--expanded)
               (aob-trace--live-thought-p s ev)))
      (aob-trace--annotate s ev (aob-trace--line-cached s ev))
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
                 (concat (substring text 0 (1- aob-trace-explore-width)) "…"))))
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
  (let ((evs (seq-remove (lambda (ev) (plist-get ev :parent))
                         (reverse (seq-take (aob-session-events s)
                                            aob-trace-limit))))
        (acc nil))
    (while evs
      (let* ((run (seq-take-while #'aob-trace--explores-p evs))
             (n (length run)))
        (if (>= n aob-trace-explore-min)
            (progn (push (aob-trace--explore-block s run) acc)
                   (setq evs (nthcdr n evs)))
          (push (aob-trace--block s (car evs)) acc)
          (setq evs (cdr evs)))))
    (let ((blocks (nreverse acc)))
      (when (and blocks (aob-trace--delta-p)
                 (string-prefix-p "\n" (car blocks)))
        (setcar blocks (substring (car blocks) 1)))
      blocks)))

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

;;; Subagents — the fleet under the agent, in one place under its trace.
;;; The trace is the work you asked for; a delegation is one line there
;;; and one row here, and RET here opens that subagent's own steps.

(defvar-local aob-subagents--session-id nil)
(defvar-local aob-subagents--tick -1)

(defvar aob-subagents-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap aob-object-map special-mode-map))
    (define-key map (kbd "RET") #'aob-subagents-open)
    (define-key map (kbd "<tab>") #'aob-subagents-open)
    map))

(define-derived-mode aob-subagents-mode special-mode "aob-subs"
  "The subagents one session delegated to."
  (ygg-ui-plain-layout)
  (setq truncate-lines nil)
  (setq-local cursor-in-non-selected-windows nil)
  (hl-line-mode 1))

(defun aob-subagents--name (s) (aob--buffer-name "subs" s))

(defun aob-subagents--row (s ev)
  ;; status and progress in a fixed gutter, so the eye runs down one
  ;; column instead of hunting the end of ragged titles
  (let* ((n (or (plist-get ev :children) 0))
         (live (or (plist-get ev :child-live) 0))
         (fail (or (plist-get ev :child-fail) 0)))
    (propertize
     (concat (format "%-2s" (aob-trace--status ev))
             (propertize (format "%-7s" (if (> n 0) (format "%d/%d" (- n live) n) "·"))
                         'face 'shadow)
             (aob--first-line (or (plist-get ev :title) "subagent") 64)
             (if (> fail 0) (propertize (format "  ✗%d" fail) 'face 'error) "")
             (if-let* ((cs (plist-get ev :child-stat))) (concat "  " cs) ""))
     'aob-session (aob-session-id s)
     'aob-event (plist-get ev :seq))))

(defcustom aob-subagents-height 20
  "Tallest the subagents panel grows to, in lines.
It fits its rows and stops here; below `aob-subagents-min-height' it
stops shrinking, so a delegation that has just started still has room to
show the next few under it."
  :type 'natnum :group 'aob)

(defcustom aob-subagents-min-height 8
  "Shortest the subagents panel is drawn, in lines."
  :type 'natnum :group 'aob)

(defvar-local aob-subagents--shown 0)

(defun aob-subagents--restore (line rows)
  "Put every window back on LINE, and refit only when ROWS changed.
`erase-buffer' collapses window-point in each window, not just the one
you are in — and a height refitted on every tick would take and give
back lines of the trace above as titles wrap."
  (goto-char (point-min))
  (forward-line (1- line))
  (let ((refit (/= rows aob-subagents--shown)))
    (setq aob-subagents--shown rows)
    (dolist (w (get-buffer-window-list (current-buffer) nil t))
      (when (window-live-p w)
        (set-window-point w (point))
        (when refit (ignore-errors (fit-window-to-buffer w aob-subagents-height aob-subagents-min-height)))))))

(defun aob-subagents--render (&optional force)
  (when-let* ((s (aob-session-get aob-subagents--session-id)))
    (let ((tick (or (aob-session-ref s :tick) 0)))
      (unless (and (not force) (eql aob-subagents--tick tick))
        (setq aob-subagents--tick tick)
        (let* ((subs (aob-session-subagents s))
               (live (seq-count (lambda (e)
                                  (member (plist-get e :status)
                                          '("pending" "in_progress")))
                                subs))
               ;; rows only ever append, so the line number IS the row:
               ;; point stays on the subagent you were reading
               (line (line-number-at-pos))
               (inhibit-read-only t))
          (setq header-line-format
                (format " %s · %d subagent%s%s"
                        (aob-session-name s) (length subs)
                        (if (= (length subs) 1) "" "s")
                        (if (> live 0) (format " · %d running" live) "")))
          (erase-buffer)
          (if subs
              (dolist (ev subs) (insert (aob-subagents--row s ev) "\n"))
            (insert (propertize " working alone\n" 'face 'shadow)))
          (aob-subagents--restore line (length subs)))))))

(defun aob-subtrace-buffer (s task)
  "A buffer of TASK's own steps: live from the ring, else the
`:child-events' kept for it when it finished and its children left."
  (let* ((tid (plist-get task :tool-id))
         (kids (or (reverse (seq-filter (lambda (e) (equal (plist-get e :parent) tid))
                                        (aob-session-events s)))
                   (plist-get task :child-events)))
         (buf (get-buffer-create
               (aob--buffer-name
                "subtrace" s
                (aob--first-line (or (plist-get task :title) "sub") 30)))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aob-trace-mode) (aob-trace-mode))
      (setq aob-buffer-session-id (aob-session-id s))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (aob-trace--block s task) "\n")
        (if kids
            (dolist (ev kids) (insert (aob-trace--block s ev) "\n"))
          (insert (propertize "  · steps collapsed — none retained\n" 'face 'shadow))))
      (goto-char (point-min)))
    buf))

(defcustom aob-subtrace-display-action
  '((display-buffer-in-side-window)
    (side . right)
    (slot . 1)
    (window-width . 0.4)
    (window-parameters . ((no-delete-other-windows . t))))
  "Where a subagent's own trace opens: a side window beside the work."
  :type 'sexp :group 'aob)

(defun aob-subagents-open ()
  "Open the subagent on this line in its own trace, in the side window."
  (interactive)
  (let* ((seq (get-text-property (line-beginning-position) 'aob-event))
         (s (aob-session-get aob-subagents--session-id))
         (task (and s seq (seq-find (lambda (e) (eql (plist-get e :seq) seq))
                                    (aob-session-events s)))))
    (unless task (user-error "aob: no subagent on this line"))
    (aob-subtrace-show (aob-subtrace-buffer s task))))

(defun aob-subtrace-show (buffer)
  "Show BUFFER in the subagent side window and select it."
  (let ((win (display-buffer buffer aob-subtrace-display-action)))
    (when (window-live-p win) (select-window win))
    win))

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

(defun aob-subagents--below-trace (buffer alist)
  "Split the trace window itself for BUFFER, never taking over a neighbour."
  (when-let* ((trace (cdr (assq 'window alist)))
              ((window-live-p trace))
              (height (cdr (assq 'window-height alist)))
              (win (ignore-errors (split-window trace (- height) 'below))))
    (window--display-buffer buffer win 'window alist)))

(defun aob-subagents--show (s)
  "Put S's subagents under its trace, or below the selected window."
  (let* ((buf (aob-subagents-buffer s))
         (trace (get-buffer-window (aob-trace--name s) t))
         (side (and trace (window-parameter trace 'window-side)))
         (height (max aob-subagents-min-height
                      (min aob-subagents-height
                           (1+ (length (aob-session-subagents s)))))))
    (display-buffer
     buf
     (if side
         ;; a side window cannot be split, so asking for the one below it
         ;; hands over one of the frame's real splits — take a slot instead
         `(display-buffer-in-side-window
           (side . ,side)
           (slot . ,(1+ (or (window-parameter trace 'window-slot) 0)))
           (dedicated . t)
           (window-height . ,height))
       `((display-buffer-reuse-window aob-subagents--below-trace
                                      display-buffer-below-selected)
         ,@(and trace (list (cons 'window trace)))
         (dedicated . t)
         (window-height . ,height))))
    buf))

;;;###autoload
(defun aob-subagents (s)
  "Show the subagents S delegated to, in their place under its trace."
  (interactive (list (aob-target)))
  (aob-subagents--show s))

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

(defun aob-trace-queue-edit ()
  "Rewrite the queued prompt on this line before it goes out."
  (interactive)
  (let* ((s (or (aob-session-get aob-trace--session-id) (user-error "aob: no session here")))
         (entry (or (aob-trace--queued-at-point)
                    (user-error "aob: no queued message on this line")))
         (ev (nth 2 entry))
         (text (read-string "queued » " (car entry))))
    (if (string-empty-p (string-trim text))
        (aob-trace-queue-drop)
      (setcar entry text)
      (plist-put ev :text text)
      (plist-put ev :line nil)
      (plist-put ev :head nil)
      (run-hook-with-args 'aob-queue-change-hook s)
      (aob--dirty s))))

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

(defun aob-trace--content-end ()
  "Where the rendered trace ends: the input marker, else the buffer end."
  (if (and aob-trace--input (marker-position aob-trace--input))
      (marker-position aob-trace--input)
    (point-max)))

(defun aob-trace--user-glyph ()
  (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-account_circle" 'aob-trace-speaker) ""))

(defun aob-trace--ensure-input ()
  "Make sure the buffer ends in a writable line to type the next prompt in."
  (when (aob-trace--inline-p)
    (let ((inhibit-read-only t))
      (unless (and aob-trace--input (marker-position aob-trace--input))
        (save-excursion
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (propertize " " 'display
                              `((margin left-margin) ,(aob-trace--user-glyph))))
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

(defun aob-trace-send ()
  "Send what is typed at the end of the trace to this session."
  (interactive)
  (let* ((start (aob-trace--content-end))
         (text (string-trim (buffer-substring-no-properties start (point-max))))
         (s (aob-session-get aob-trace--session-id)))
    (unless s (user-error "aob: this trace has no session"))
    (let ((comments (aob-trace--comments-message s)))
      (when (and (string-empty-p text) (not comments))
        (user-error "aob: nothing to send"))
      (let ((inhibit-read-only t))
        (delete-region start (point-max)))
      (when (fboundp 'ygg-normal-state) (ygg-normal-state))
      (aob-session-put s :comments nil)
      (aob-prompt s (string-trim
                     (string-join
                      (delq nil (list (and (fboundp 'aob-context-text)
                                           (aob-context-text))
                                      comments
                                      (unless (string-empty-p text) text)))
                      "\n\n"))
                  nil))))

(defface aob-trace-anchor
  '((((background dark)) :background "#3a3222" :underline "#8a7a4a")
    (t :background "#fdf3d0" :underline "#b08a3a"))
  "Face marking text a comment is attached to."
  :group 'aob)

(defface aob-trace-comment
  '((((background dark)) :background "#1d2027" :extend t)
    (t :background "#eef1f6" :extend t))
  "Face behind a comment card."
  :group 'aob)

(defun aob-trace--comments (s)
  "Comments held against S, oldest first."
  (reverse (aob-session-ref s :comments)))

(defun aob-trace--comments-for (s seq)
  "Comments attached to the event numbered SEQ."
  (seq-filter (lambda (c) (eql (plist-get c :seq) seq))
              (aob-trace--comments s)))

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

(defun aob-trace-comment (start end)
  "Comment on the selected text, or on the block point is in.
The comment is held until the next message is sent, the way Delta
submits a thread of comments together."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (let* ((s (aob-session-get aob-trace--session-id))
         (seq (get-text-property start 'aob-event))
         (quoted (string-trim (buffer-substring-no-properties start end)))
         (text (read-string (format "Comment on %s: "
                                    (truncate-string-to-width quoted 40 nil nil t)))))
    (unless s (user-error "aob: this trace has no session"))
    (unless seq (user-error "aob: nothing to comment on here"))
    (when (string-empty-p (string-trim text)) (user-error "aob: empty comment"))
    (aob-session-put s :comments
                     (cons (list :seq seq :quote quoted :text text
                                 :ts (float-time))
                           (aob-session-ref s :comments)))
    (when (fboundp 'ygg-normal-state) (ygg-normal-state))
    (aob-trace--render t)))

(defun aob-trace--comments-message (s)
  "The held comments as one message, or nil when there are none."
  (when-let* ((cs (aob-trace--comments s)))
    (mapconcat (lambda (c)
                 (format "> %s\n%s" (plist-get c :quote) (plist-get c :text)))
               cs "\n\n")))

(defun aob-trace--render-1 (s)
  ;; whichever event is still being written: decorating it is work that
  ;; will be thrown away by the next chunk
  (setq aob-trace--live-seq
        (and (eq (aob-session-state s) 'working)
             (plist-get (seq-find (lambda (e) (not (plist-get e :parent)))
                                  (aob-session-events s))
                        :seq)))
  (setq header-line-format
        (format " %s · %s%s%s%s%s%s%s"
                (aob-session-name s)
                (aob-session-state s)
                (aob-trace--dir-line s)
                (if-let* ((m (aob-session-ref s :mode-id)))
                    (format " · %s" m)
                  "")
                (if-let* ((m (aob-session-ref s :model-name)))
                    (format " · %s" m)
                  "")
                (if-let* ((ctx (aob-session-ctx s)))
                    (format " · %s ctx" ctx)
                  "")
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
         (point-before (point))
         (starts (mapcar (lambda (w) (cons w (window-start w)))
                         (get-buffer-window-list (current-buffer) nil t)))
         (rebuilt (and (null old) (> (buffer-size) 0)))
         (inhibit-read-only t))
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
    ;; incremental: the buffer is edited only from the first changed
    ;; block on, so redisplay re-wraps the tail, never all visible lines
    (while (and new old (eq (car new) (car old)))
      (cl-incf pos (1+ (length (car new))))
      (pop new) (pop old))
    (when (or new old)
      ;; a streaming block only grows: when the old text is a strict
      ;; prefix of the new, keep it in place and insert just the tail —
      ;; redisplay then lays out the appended words, not the whole
      ;; message again at every tick
      (let ((suffix nil))
        (when (and new old
                   (>= (length (car new)) (length (car old)))
                   (eq t (compare-strings (car old) nil nil
                                          (car new) nil (length (car old)))))
          (setq suffix (length (car old)))
          (cl-incf pos suffix))
        (delete-region (min pos (aob-trace--content-end))
                       (aob-trace--content-end))
        (save-excursion
          (goto-char (aob-trace--content-end))
          (when suffix
            (insert (substring (car new) suffix) (aob-trace--sep))
            (pop new) (pop old))
          (dolist (b new) (insert b (aob-trace--sep)))
          (when (and aob-trace--input (marker-position aob-trace--input))
            (set-marker aob-trace--input (point))))))
    (setq aob-trace--blocks blocks)
    (aob-trace--ensure-input)
    (unless at-end
      (pcase-dolist (`(,win . ,start) starts)
        (when (and (window-live-p win) (<= start (point-max)))
          (set-window-start win start t))))
    (when (and rebuilt (not at-end))
      (goto-char (min point-before (point-max))))
    (when at-end
      (goto-char (point-max))
      (dolist (w (get-buffer-window-list (current-buffer) nil t))
        (set-window-point w (point-max))))
    ;; the fleet takes its place under the trace the first time this
    ;; session delegates — and only that once: the buffer outlives being
    ;; closed, so putting it away is a decision that stays made
    (when (fboundp 'ygg-diagram-replace) (ygg-diagram-replace))
    (when (and (not (get-buffer (aob-subagents--name s)))
               (get-buffer-window (current-buffer) t)
               (aob-session-subagents s))
      (aob-subagents--show s))))

(defun aob-trace-tab ()
  "Draw the fence or image at point, or expand the event point is on."
  (interactive)
  (unless (and (fboundp 'ygg-diagram-toggle-any-at-point)
               (ygg-diagram-toggle-any-at-point))
    (aob-trace-toggle)))

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
            aob-buffer-session-id (aob-session-id s))
      (setq aob-trace--dir
            (when-let* ((dir (or (aob-session-dir s) (aob-session-project s))))
              (propertize (format "  %s" (abbreviate-file-name dir))
                          'face 'shadow)))
      (when-let* ((dir (or (aob-session-dir s) (aob-session-project s)))
                  ((file-directory-p dir)))
        (setq-local ygg-diagram-image-root
                    (file-name-as-directory (expand-file-name dir))))
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
                       (propertize "◐" 'face 'warning)))
    ("completed" (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-checkbox_marked_outline" 'success)
                     (propertize "☑" 'face 'success)))
    (_ (or (aob-trace--nf #'nerd-icons-mdicon "nf-md-checkbox_blank_outline" 'shadow) "☐"))))

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
              (insert (propertize (format "☑ %s\n" (plist-get e :content))
                                  'face 'shadow)))))))))

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

(define-key aob-object-map "t" (cons "plan/todos" #'aob-plan))

;; T (agent activity → quickfix) is defined in layer-aob.el: it bridges
;; the trace's tool `:locations' to the quickfix machinery, and lives in
;; the glue layer so aob-trace.el keeps no dependency on layer-quickfix.

(provide 'aob-trace)
;;; aob-trace.el ends here
