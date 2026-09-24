;;; ygg-ui.el --- the primitives every ygg view shares -*- lexical-binding: t; -*-

;;; Commentary:

;; The window a view opens into, the face it dims with, the width it
;; cuts to and the tokens it counts: four answers every panel, tree and
;; map had its own copy of, and one place to change them.

;;; Code:

(require 'subr-x)
(require 'seq)

(defgroup ygg-ui nil
  "What the views of the harness have in common."
  :group 'convenience)

(defcustom ygg-ui-chars-per-token 4
  "Characters a token is counted as, standing in for a tokenizer."
  :type 'natnum :group 'ygg-ui)

(defun ygg-ui-main-frame (&optional frame)
  "The top-level frame FRAME stands on, itself when it is one.
A float is a child frame over the real one, and a sidebar or a panel
asked for from inside a float belongs to the frame under it."
  (let ((frame (or frame (selected-frame))))
    (while (frame-parent frame) (setq frame (frame-parent frame)))
    frame))

(defmacro ygg-ui-on-main-frame (&rest body)
  "Run BODY on the top-level frame, selecting and focusing it first when
the call stood in a float; the window BODY selects stays selected."
  (declare (indent 0))
  `(let ((main (ygg-ui-main-frame)))
     (unless (eq main (selected-frame))
       (select-frame-set-input-focus main))
     ,@body))

(defun ygg-ui-main-window (&optional frame)
  "The first live window of FRAME's main area, or nil when there is none.
window-main-window can be an internal window, which is nothing a
buffer can be shown in, so its children are walked until one is live."
  (let ((window (window-main-window (or frame (selected-frame)))))
    (while (and window (not (window-live-p window)))
      (setq window (window-child window)))
    (and (window-live-p window)
         (not (window-parameter window 'window-side))
         window)))

(defcustom ygg-ui-prefer-buffers t
  "Whether a thing the owner reads opens in the window they stand in.
More buffers, fewer windows: a trace, a report, a panel or a map takes
over the main window and is left with the buffer keys, the way magit
opens its buffers.  With this off every reader falls back to the action
it used before, through the same door."
  :type 'boolean :group 'ygg-ui)

(defconst ygg-ui-own-buffer-regexp
  (concat "\\`\\*\\(?:ygg-\\)?"
          (regexp-opt '("trace" "subtrace" "trace event" "context" "actions"
                        "usage" "project" "doctor" "qa" "c4" "map" "review"
                        "queue" "proposal" "evidence" "check" "brief"
                        "transcript" "tools" "watch"))
          "\\(?:[:*]\\| \\)")
  "Buffer names of the config's own readers, routed to the main window.
A call the audit missed still lands where the owner stands.")

(defun ygg-ui--main-window-p (window)
  "Whether WINDOW is a window of the main area a reader may take over.
A side window, a float's window, the minibuffer and a window dedicated
to its buffer are all somebody else's, and none of them is one."
  (and (window-live-p window)
       (null (frame-parent (window-frame window)))
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (window-dedicated-p window))))

(defun ygg-ui-display-in-main (buffer alist)
  "Show BUFFER in the main area and return its window, or nil.
A display-buffer action function: the selected window when the call
stands in one of the main area, else the first live main window of the
top frame, and never a side window or a split of its own.  Nil when
ygg-ui-prefer-buffers is off, so display-buffer goes on to the next
action and the site keeps what it did before."
  (when ygg-ui-prefer-buffers
    (let* ((frame (ygg-ui-main-frame))
           (here (selected-window)))
      (or (and (eq (window-frame here) frame)
               (ygg-ui--main-window-p here)
               (window--display-buffer buffer here 'reuse alist))
          (when-let* ((window (ygg-ui-main-window frame))
                      ((ygg-ui--main-window-p window)))
            (window--display-buffer buffer window 'reuse alist))
          (when-let* ((window (seq-find #'ygg-ui--main-window-p
                                        (window-list frame 'no-minibuf))))
            (window--display-buffer buffer window 'reuse alist))
          (display-buffer-use-some-window buffer alist)))))

(defvar better-jumper--buffer-targets)

(defconst ygg-ui-jump-target-regexp
  (concat "\\`\\*#"                      ; the cockpit's own, *#1:1 acp*
          "\\|" ygg-ui-own-buffer-regexp    ; every reader this config opens
          "\\|\\`\\*\\(trace\\|transcript\\|handoff\\|report\\)")
  "Buffers the jumplist may land on, beyond the files it always could.
A jump names a file, and a reader has none: without this the jumplist
cannot hold the place you were reading, so C-o out of a trace has
nowhere to go and C-i never comes back to it.")

(with-eval-after-load 'better-jumper
  (setq better-jumper--buffer-targets
        (concat better-jumper--buffer-targets "\\|" ygg-ui-jump-target-regexp)))

(declare-function better-jumper-set-jump "better-jumper" (&optional pos))
(declare-function better-jumper-get-jumps "better-jumper" (&optional context))
(declare-function better-jumper-set-jumps "better-jumper" (jumps &optional context))

(defun ygg-ui--carry-jumps (from)
  "Give the window now selected the jumplist of FROM.
The list is kept per window, so a reader that opens in a window of its
own starts with none and C-o out of it has nowhere to go: the way back
to the file has to travel with the owner."
  (when (and (fboundp 'better-jumper-set-jumps)
             (window-live-p from)
             (not (eq from (selected-window))))
    (ignore-errors
      (better-jumper-set-jumps (better-jumper-get-jumps from)))))

(defun ygg-ui-show (buffer &optional keep-focus fallback)
  "Show BUFFER in the main area and select it; the one door for a reader.
With KEEP-FOCUS the buffer is shown and the point stays where it was.
A call made from inside a float or a side window shows BUFFER in the
main window of the top frame.  FALLBACK is the display action the site
used before, taken only when ygg-ui-prefer-buffers is off.
Where this takes the owner somewhere else, where they stood goes on the
jumplist and the list travels with them, so C-o leads back out of the
reader and C-i returns to it."
  (let* ((buffer (if (bufferp buffer) buffer (get-buffer-create buffer)))
         (origin (selected-window))
         (leaving (and (not keep-focus) (not (eq buffer (current-buffer)))))
         (window (progn
                   (when (and leaving (fboundp 'better-jumper-set-jump))
                     (ignore-errors (better-jumper-set-jump)))
                   (if ygg-ui-prefer-buffers
                       (ygg-ui-on-main-frame (ygg-ui-display-in-main buffer nil))
                     (display-buffer buffer fallback)))))
    (when (and window (not keep-focus))
      (select-window window)
      (when leaving (ygg-ui--carry-jumps origin)))
    window))

(defun ygg-ui-plain-layout ()
  "Lay this buffer out left to right, skipping the bidi pass entirely.
No reader of the harness carries right-to-left text, and reordering is
where redisplay spends most of its time on a long streamed line: a
resolved bracket pair and a level per character, every pass, over every
line on screen.  Turning reordering off is the one that skips the
iterator; the direction and the bracket rule are set with it so nothing
turns it back on."
  (setq-local bidi-display-reordering nil)
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local bidi-inhibit-bpa t))

(defun ygg-ui-dimmed ()
  "The face a rank, a count and a spent row are dimmed with."
  (if (facep 'magit-dimmed) 'magit-dimmed 'shadow))

(defun ygg-ui-cut (string width)
  "STRING cut to WIDTH columns, an ellipsis standing for what did not fit."
  (if (<= (string-width string) width)
      string
    (truncate-string-to-width string (max 0 width) nil nil "…")))

(defun ygg-ui-pad-right (string width)
  "STRING padded on the right to WIDTH columns, never cut."
  (concat string (make-string (max 0 (- width (string-width string))) 32)))

(defvar markdown-hide-markup)
(defvar markdown-fontify-code-blocks-natively)
(declare-function markdown-mode "markdown-mode" ())

(defcustom ygg-ui-markdown-hide-markup t
  "Render markdown formatted: the asterisks and backticks go invisible."
  :type 'boolean :group 'ygg-ui)

(defconst ygg-ui-markdown--props '(face invisible display)
  "What is carried over from the fontified copy onto the answer.")

(defcustom ygg-ui-markdown-max 4000
  "Longest text markdown-mode is asked to fontify, in characters.
Its font-lock is quadratic in the size of the buffer — measured on
Emacs 31: 3ms at 1k, 10ms at 2k, 49ms at 4k, 161ms at 8k, 1.3s at 24k —
so a long answer is left as plain text rather than stopping the editor
to decorate it."
  :type 'natnum :group 'yggdrasil)

(defvar ygg-ui--markdown-buffer nil
  "One buffer kept in markdown-mode for rendering snippets.")

(defun ygg-ui--markdown-buffer ()
  "A buffer already in markdown-mode, ready to be filled.
Standing the mode up costs more than fontifying the text does, and a
streaming trace asks for this ten times a second."
  (if (buffer-live-p ygg-ui--markdown-buffer)
      ygg-ui--markdown-buffer
    (setq ygg-ui--markdown-buffer
          (with-current-buffer (get-buffer-create " *ygg-markdown*")
            (delay-mode-hooks (markdown-mode))
            (setq-local inhibit-modification-hooks t)
            (buffer-disable-undo)
            (current-buffer)))))

(defun ygg-ui-markdown (text)
  "TEXT carrying markdown-mode's rendering as text properties.
Faces come over as `font-lock-face', and with
`ygg-ui-markdown-hide-markup' the markup characters come over
invisible, so bold reads bold rather than showing its asterisks.  TEXT
is answered as it stands when it is not a string, when it is empty,
when markdown-mode is missing, and whenever the fontification itself
goes wrong."
  (if (or (not (stringp text)) (string-empty-p text)
          (> (length text) ygg-ui-markdown-max)
          (not (or (fboundp 'markdown-mode)
                   (require 'markdown-mode nil t))))
      text
    (or (ignore-errors
          (let ((out (copy-sequence text)))
            (with-current-buffer (ygg-ui--markdown-buffer)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert text))
              ;; hooks are off here: nothing else says the old text is gone
              (syntax-ppss-flush-cache (point-min))
              (let ((markdown-hide-markup ygg-ui-markdown-hide-markup)
                    (markdown-fontify-code-blocks-natively t))
                (syntax-propertize (point-max))
                (font-lock-ensure))
              (dolist (prop ygg-ui-markdown--props)
                (let ((pos (point-min)))
                  (while (< pos (point-max))
                    (let ((next (next-single-property-change
                                 pos prop nil (point-max)))
                          (val (get-text-property pos prop)))
                      (when val
                        (if (eq prop 'face)
                            (let ((had (get-text-property (1- pos) 'font-lock-face
                                                          out)))
                              (put-text-property
                               (1- pos) (1- next) 'font-lock-face
                               (if had (append (ensure-list val)
                                               (ensure-list had))
                                 val)
                               out))
                          (put-text-property (1- pos) (1- next) prop val out)))
                      (setq pos next))))))
            out))
        text)))

(defun ygg-ui-tokens (text &optional chars-per-token)
  "Tokens TEXT is counted as: its characters over CHARS-PER-TOKEN.
CHARS-PER-TOKEN defaults to ygg-ui-chars-per-token."
  (/ (length text) (max 1 (or chars-per-token ygg-ui-chars-per-token))))

(add-to-list 'display-buffer-alist
             (list ygg-ui-own-buffer-regexp '(ygg-ui-display-in-main)))

(provide 'ygg-ui)
;;; ygg-ui.el ends here
