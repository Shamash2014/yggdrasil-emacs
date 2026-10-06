;;; layer-ui.el --- snacks.nvim-flavored UI layer -*- lexical-binding: t; -*-

;; Built-ins wrapped: posframe child frames (via the posframe package),
;; window margins, mode-line-format, display-line-numbers-mode, faces.
;; Custom: stacked toast notifier + history log, zen-mode window centering,
;; vertico picker floating via vertico-posframe.

;;; Code:

(require 'seq)
(require 'yggdrasil-leader)
(require 'ygg-focus)
(require 'ygg-tile)
(require 'ygg-ui)

(declare-function posframe-show "posframe")
(declare-function posframe-delete "posframe")
(declare-function posframe-poshandler-frame-top-right-corner "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function vertico-posframe-mode "vertico-posframe")
(declare-function ygg-space-toggle-tab-bar "yggdrasil-spacetree")

(defvar vertico-posframe-poshandler)
(defvar vertico-posframe-border-width)
(defvar vertico-posframe-width)
(defvar vertico-posframe-size-function)
(defvar vertico-posframe-parameters)
(defvar vertico-posframe-border)

(defgroup ygg-ui nil
  "snacks.nvim-inspired UI layer: toasts, zen mode, floating picker."
  :group 'yggdrasil
  :prefix "ygg-")

;;; Notifier (snacks.notifier feel): stacked toast posframes + history log

(defcustom ygg-notify-duration 4
  "Seconds before a toast notification auto-dismisses."
  :type 'number :group 'ygg-ui)

(defcustom ygg-notify-width 42
  "Width in characters of a toast notification."
  :type 'integer :group 'ygg-ui)

(defconst ygg-notify--max-stack 3
  "Maximum number of toasts shown at once.")

(defvar ygg-notify--slots (make-vector ygg-notify--max-stack nil)
  "Per-slot (BUFFER-NAME . TIMER), or nil when the slot is free.")

(defvar ygg-notify-history nil
  "List of (TIME LEVEL MSG), most recent first.")

(defun ygg-notify--face (level)
  "Map notification LEVEL to a theme face, inherited rather than hardcoded."
  (pcase level
    ('warn 'warning)
    ('error 'error)
    (_ 'success)))

(defun ygg-notify--free-slot ()
  "Return an open toast slot, evicting the oldest toast if all are busy."
  (or (seq-position ygg-notify--slots nil)
      (progn (ygg-notify--dismiss 0) 0)))

(defun ygg-notify--dismiss (slot)
  "Delete the posframe occupying SLOT and mark it free."
  (let ((entry (aref ygg-notify--slots slot)))
    (when entry
      (when (timerp (cdr entry)) (cancel-timer (cdr entry)))
      (ignore-errors (posframe-delete (car entry)))
      (aset ygg-notify--slots slot nil))))

(defun ygg-notify (msg &optional level)
  "Show MSG as an auto-dismissing toast. LEVEL is `info', `warn', or `error'."
  (interactive "sNotify: ")
  (setq level (or level 'info))
  (push (list (current-time) level msg) ygg-notify-history)
  (when (nthcdr 200 ygg-notify-history)
    (setcdr (nthcdr 199 ygg-notify-history) nil))
  (if (not (and (display-graphic-p)
                (or (featurep 'posframe) (locate-library "posframe"))))
      (message "%s" msg)
    (require 'posframe)
    (let* ((slot (ygg-notify--free-slot))
           (buf (format " *ygg-notify-%d*" slot))
           (face (ygg-notify--face level))
           (fg (face-attribute face :foreground nil t))
           (bg (if (facep 'ygg-float)
                   (face-attribute 'ygg-float :background nil t)
                 (face-attribute 'default :background nil t)))
           (toast-height (+ (* 3 (frame-char-height)) 10)))
      (ignore fg)
      (posframe-show buf
                      :string (propertize
                               (truncate-string-to-width msg (- ygg-notify-width 4))
                               'face `(:inherit ,face))
                      :poshandler #'posframe-poshandler-frame-top-right-corner
                      :internal-border-width 0
                      :border-width 1
                      :border-color (face-background 'child-frame-border nil t)
                      :background-color bg
                      :min-width ygg-notify-width
                      :x-pixel-offset -12
                      :y-pixel-offset (+ 10 (* slot toast-height)))
      (aset ygg-notify--slots slot
            (cons buf (run-at-time ygg-notify-duration nil #'ygg-notify--dismiss slot))))))

(define-derived-mode ygg-notify-history-mode special-mode "Notify-History"
  "Major mode listing past `ygg-notify' calls."
  (ygg-ui-plain-layout))

(defun ygg-notify-history ()
  "Show a buffer listing past `ygg-notify' calls, most recent first."
  (interactive)
  (let ((buf (get-buffer-create "*ygg-notify-history*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if ygg-notify-history
            (dolist (entry ygg-notify-history)
              (pcase-let ((`(,time ,level ,msg) entry))
                (insert (format-time-string "[%H:%M:%S] " time)
                        (propertize (format "%-5s " (upcase (symbol-name level)))
                                    'face (ygg-notify--face level))
                        msg "\n")))
          (insert "(no notifications yet)\n")))
      (ygg-notify-history-mode))
    (ygg-ui-show buf)))

;;; Indent guides (indent-bars: fast, tree-sitter-aware, minimal config)

(when (fboundp 'elpaca)
  (elpaca indent-bars
    ;; loading costs ~0.8s — defer off the startup path
    (run-with-idle-timer
     1 nil
     (lambda ()
       (require 'indent-bars)
       (setq indent-bars-color '(highlight :face-bg t :blend 0.15))
       (add-hook 'prog-mode-hook #'indent-bars-mode)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (derived-mode-p 'prog-mode) (indent-bars-mode 1))))))))

;;; Zen mode (snacks.zen feel): center the selected window, hide chrome

(defcustom ygg-zen-width 100
  "Target content width in columns for `ygg-zen-toggle'."
  :type 'integer :group 'ygg-ui)

(defvar-local ygg-zen--active nil)
(defvar-local ygg-zen--had-line-numbers nil)

(defun ygg--zen-enable ()
  (let ((margin (max 0 (/ (- (frame-width) ygg-zen-width) 2))))
    (setq ygg-zen--had-line-numbers (bound-and-true-p display-line-numbers-mode))
    (set-window-margins (selected-window) margin margin)
    (when ygg-zen--had-line-numbers (display-line-numbers-mode -1))
    (setq-local mode-line-format nil)
    (setq ygg-zen--active t)))

(defun ygg--zen-disable ()
  (set-window-margins (selected-window) nil)
  (when ygg-zen--had-line-numbers (display-line-numbers-mode 1))
  (kill-local-variable 'mode-line-format) ; falls back to the shared modeline, never clobbers it
  (setq ygg-zen--active nil)
  (force-mode-line-update))

(defun ygg-zen-toggle ()
  "Toggle a distraction-free centered view of the selected window."
  (interactive)
  (if ygg-zen--active (ygg--zen-disable) (ygg--zen-enable)))

;;; Floating picker (snacks.picker feel): vertico's minibuffer as a centered posframe

(defun ygg--posframe-top-center (info)
  "snacks vscode-preset: centered horizontally, one line height from top."
  (cons (max 0 (/ (- (plist-get info :parent-frame-width)
                     (plist-get info :posframe-width))
                  2))
        (frame-char-height)))

(defun ygg--vertico-posframe-prompt-lines (buffer)
  "Lines the minibuffer prompt of BUFFER takes beyond its first."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (seq-count (lambda (c) (eq c ?\n))
                   (buffer-substring-no-properties (point-min) (minibuffer-prompt-end))))
    0))

(defun ygg--vertico-posframe-size (buffer)
  "Compute vertico-posframe dimensions as ratios of the parent frame.
vscode preset: width 0.4 (min 80), height 0.4 (min 8), grown by the
extra lines of a multi-line prompt up to 0.8 of the frame."
  (let* ((frame-cols (frame-width))
         (frame-lines (frame-height))
         (width (max 80 (round (* 0.4 frame-cols))))
         (base (max 8 (round (* 0.4 frame-lines))))
         (height (max base (min (round (* 0.8 frame-lines))
                                (+ base (ygg--vertico-posframe-prompt-lines buffer))))))
    (list :height height
          :width width
          :min-height height
          :min-width width)))

(defconst ygg--vertico-posframe-min-candidate 40
  "Columns of a row the candidate keeps before its annotation may take the rest.")

(defun ygg--vertico-posframe-fit-row (args)
  "Filter ARGS of `vertico--format-candidate' so its row fits the floating picker.
An over-long suffix is cut first, never below what the candidate needs; an
over-long candidate is then cut with an ellipsis, keeping prefix and suffix."
  (if (not (bound-and-true-p vertico-posframe-mode))
      args
    (pcase-let* ((`(,cand ,prefix ,suffix . ,rest) args)
                 (room (- (plist-get (ygg--vertico-posframe-size (current-buffer)) :width)
                          1 (string-width prefix)))
                 (suffix-room (max 0 (- room (min (string-width cand)
                                                  ygg--vertico-posframe-min-candidate))))
                 (suffix (if (<= (string-width suffix) suffix-room)
                             suffix
                           (truncate-string-to-width suffix suffix-room nil nil "…")))
                 (cand-room (- room (string-width suffix))))
      (cons (if (<= (string-width cand) cand-room)
                cand
              (truncate-string-to-width cand (max 8 cand-room) nil nil "…"))
            (cons prefix (cons suffix rest))))))

(defun ygg--vertico-posframe-enable (&optional frame)
  "Float the picker, once there is a FRAME that can show one."
  (with-selected-frame (or (and (frame-live-p frame) frame) (selected-frame))
    (when (and (display-graphic-p)
               (require 'vertico-posframe nil t)
               (not (bound-and-true-p vertico-posframe-mode)))
      (vertico-posframe-mode 1))))

;; registered before the package is anywhere near loaded: a daemon makes
;; its first frame whenever the first client connects, which is as often
;; before the deferred setup below as after it
(add-hook 'server-after-make-frame-hook #'ygg--vertico-posframe-enable)
(add-hook 'after-make-frame-functions #'ygg--vertico-posframe-enable)

(when (fboundp 'elpaca)
  (elpaca vertico-posframe
    (run-with-idle-timer
     1 nil
     (lambda ()
       (require 'vertico-posframe)
       (with-eval-after-load 'vertico
         (setq vertico-posframe-poshandler #'ygg--posframe-top-center
               vertico-posframe-border-width 1
               vertico-posframe-size-function #'ygg--vertico-posframe-size
               vertico-posframe-parameters '((left-fringe . 0) (right-fringe . 0)))
         (advice-add 'vertico--format-candidate :filter-args #'ygg--vertico-posframe-fit-row)
         (ygg--vertico-posframe-enable))))))

(defun ygg-vertico-posframe-toggle ()
  "Toggle floating (posframe) rendering of the vertico minibuffer."
  (interactive)
  (if (fboundp 'vertico-posframe-mode)
      (vertico-posframe-mode 'toggle)
    (message "vertico-posframe not loaded yet")))

;;; Cursorword (mini.cursorword feel): underline other occurrences on idle

(defface ygg-cursorword '((t :underline t))
  "Face for other occurrences of the symbol at point."
  :group 'ygg-ui)

(defvar ygg-cursorword-delay 0.3)
(defvar ygg-cursorword--timer nil)
(defvar-local ygg-cursorword--overlays nil)

(defun ygg-cursorword--clear ()
  (mapc #'delete-overlay ygg-cursorword--overlays)
  (setq ygg-cursorword--overlays nil))

(defun ygg-cursorword--paint ()
  (when (and ygg-cursorword-mode
             (derived-mode-p 'prog-mode 'text-mode 'conf-mode)
             (not (minibufferp)))
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                (sym (buffer-substring-no-properties (car bounds) (cdr bounds)))
                (re (concat "\\_<" (regexp-quote sym) "\\_>")))
      (save-excursion
        (goto-char (max (point-min) (window-start)))
        (let ((limit (min (point-max) (window-end nil t))))
          (while (re-search-forward re limit t)
            (unless (eq (match-beginning 0) (car bounds))
              (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'face 'ygg-cursorword)
                (push ov ygg-cursorword--overlays)))))))))

(defun ygg-cursorword--post-command ()
  ;; only churn the debounce timer where --paint can actually run — skips the
  ;; minibuffer, terminals, dired, magit, help &c. (the sole per-keystroke allocator)
  (when (and (not (minibufferp))
             (derived-mode-p 'prog-mode 'text-mode 'conf-mode))
    (ygg-cursorword--clear)
    (when ygg-cursorword--timer (cancel-timer ygg-cursorword--timer))
    (setq ygg-cursorword--timer
          (run-with-idle-timer ygg-cursorword-delay nil #'ygg-cursorword--paint))))

(define-minor-mode ygg-cursorword-mode
  "Underline other visible occurrences of the symbol at point."
  :init-value nil :global t
  (if ygg-cursorword-mode
      (add-hook 'post-command-hook #'ygg-cursorword--post-command)
    (remove-hook 'post-command-hook #'ygg-cursorword--post-command)
    (ygg-cursorword--clear)))

(ygg-cursorword-mode 1)

;;; Notifications picker (SPC s n): pick an entry, copy it to the kill ring

(defun ygg-notify-pick ()
  "Pick a past notification; the selection is copied to the kill ring."
  (interactive)
  (unless ygg-notify-history (user-error "No notifications yet"))
  (let* ((cands (mapcar (lambda (entry)
                          (pcase-let ((`(,time ,level ,msg) entry))
                            (format "%s %-5s %s"
                                    (format-time-string "%H:%M:%S" time)
                                    (upcase (symbol-name level)) msg)))
                        ygg-notify-history))
         (choice (completing-read "Notification: " cands nil t)))
    (kill-new choice)
    (message "copied")))

(with-eval-after-load 'layer-completion
  (yggdrasil-define-keys 'ygg-leader-search-map
    "n" #'ygg-notify-pick :label "notifications"))

(yggdrasil-define-keys 'normal
  "g x" #'browse-url-at-point :label "open url")

;;; vundo — visual undo-tree browser (nvim undotree feel); h/l walk the
;;; timeline, j/k switch branches, RET jumps, q quits.

(defvar vundo-mode-map)
(defvar vundo-glyph-alist)
(defvar vundo-unicode-symbols)
(declare-function vundo "vundo")
(declare-function vundo-forward "vundo")
(declare-function vundo-backward "vundo")
(declare-function vundo-next "vundo")
(declare-function vundo-previous "vundo")
(declare-function vundo-quit "vundo")
(declare-function vundo-confirm "vundo")

(when (fboundp 'elpaca)
  (elpaca vundo
    (with-eval-after-load 'vundo
      (setq vundo-glyph-alist vundo-unicode-symbols)
      (define-key vundo-mode-map (kbd "h") #'vundo-backward)
      (define-key vundo-mode-map (kbd "l") #'vundo-forward)
      (define-key vundo-mode-map (kbd "j") #'vundo-next)
      (define-key vundo-mode-map (kbd "k") #'vundo-previous)
      (define-key vundo-mode-map (kbd "q") #'vundo-quit)
      (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))))

;;; ace-window — jump to any window by letter (SPC w w); the one window
;;; op directional C-w h/j/k/l is weak at once 3+ windows are open

(defvar aw-keys)
(defvar aw-scope)
(defvar aw-background)
(defvar ygg-leader-window-map)

(when (fboundp 'elpaca)
  (elpaca ace-window
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
          aw-scope 'frame
          aw-background nil)
    (define-key ygg-leader-window-map (kbd "w") #'ace-window)))

;;; winpulse — briefly flash the focused window's background on focus
;;; change, a visual cue for the active window across many splits/side windows

(declare-function winpulse-mode "winpulse")

(when (fboundp 'elpaca)
  (elpaca (winpulse :host github :repo "xenodium/winpulse")
    (winpulse-mode 1)))

;;; Leader menu

(defvar ygg-leader-ui-map (make-sparse-keymap) "The u prefix: ui.")

(yggdrasil-define-keys 'ygg-leader-ui-map
  "z" #'ygg-zen-toggle :label "zen"
  "l" #'display-line-numbers-mode :label "line numbers"
  "w" #'ygg-cursorword-mode :label "cursorword"
  "p" #'ygg-vertico-posframe-toggle :label "picker float"
  "T" #'ygg-space-toggle-tab-bar :label "space bar")

(yggdrasil-leader-def "u" ygg-leader-ui-map "ui")

;;; Sticky scroll (Zed): pin enclosing tree-sitter scope headers in the
;;; header line as their opening lines scroll off the top. On-demand —
;;; the header line only appears when there is context to pin.

(declare-function treesit-parser-list "treesit")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-type "treesit")

(defcustom ygg-sticky-types
  "\\(function\\|method\\|class\\|struct\\|impl\\|interface\\|module\\|trait\\|enum\\|namespace\\)"
  "Regexp of treesit node types whose opening line pins as sticky context."
  :type 'string :group 'ygg-ui)

(defun ygg-sticky--line-at (node)
  (save-excursion
    (goto-char (treesit-node-start node))
    (string-trim-right
     (buffer-substring (line-beginning-position) (line-end-position)))))

(defun ygg-sticky--compute (start)
  (when (treesit-parser-list)
    (let ((node (treesit-node-at start)) lines last)
      (while node
        (when (and (treesit-node-parent node) ; skip the file-root node
                   (< (treesit-node-start node) start)
                   (string-match-p ygg-sticky-types (treesit-node-type node))
                   (not (eql (treesit-node-start node) last)))
          (push (ygg-sticky--line-at node) lines)
          (setq last (treesit-node-start node)))
        (setq node (treesit-node-parent node)))
      (when lines
        (list (mapconcat #'identity lines
                         (propertize "  ›  " 'face 'shadow)))))))

(defun ygg-sticky--update (_win start)
  (let ((new (ygg-sticky--compute start)))
    (unless (equal new header-line-format)
      (setq header-line-format new))))

(define-minor-mode ygg-sticky-mode
  "Zed-style sticky scroll via the header line."
  :init-value nil
  (if ygg-sticky-mode
      (progn
        (add-hook 'window-scroll-functions #'ygg-sticky--update nil t)
        (when-let* ((w (get-buffer-window)))
          (ygg-sticky--update w (window-start w))))
    (remove-hook 'window-scroll-functions #'ygg-sticky--update t)
    (setq header-line-format nil)))

(defun ygg-sticky--maybe ()
  (when (and (not noninteractive) (treesit-parser-list))
    (ygg-sticky-mode 1)))

(add-hook 'prog-mode-hook #'ygg-sticky--maybe)

;;; Golden-ratio window auto-resize

(defvar ygg-golden-ratio-value 1.618
  "Divisor for the focused window's target width/height.")

(defun ygg-golden--grow (win target horizontal)
  (let ((delta (window-resizable
                win (- target (if horizontal (window-total-width win)
                                (window-total-height win)))
                horizontal)))
    (unless (zerop delta) (window-resize win delta horizontal))))

(defun ygg-golden--resize (&optional frame)
  (let* ((frame (or frame (selected-frame)))
         (wins (window-list frame 'nomini))
         (main (window-main-window frame))
         (win (frame-selected-window frame)))
    (when (and (window-live-p win)
               (not (window-minibuffer-p win))
               ;; real side-windows resize via a different mechanism
               (not (window-parameter win 'window-side))
               (> (length wins) 1))
      ;; balance/size within the main tree only — balancing the frame root
      ;; also equalizes side windows, ballooning the spacetree sidebar
      (with-demoted-errors "golden-ratio: %S"
        (balance-windows main)
        (ygg-golden--grow win (floor (/ (window-total-height main)
                                        ygg-golden-ratio-value))
                          nil)
        (ygg-golden--grow win (floor (/ (window-total-width main)
                                        ygg-golden-ratio-value))
                          t)))))

(define-minor-mode ygg-golden-ratio-mode
  "Auto-resize the focused window toward the golden ratio on focus change."
  :global t
  :init-value nil
  (if ygg-golden-ratio-mode
      (add-hook 'window-selection-change-functions #'ygg-golden--resize)
    (remove-hook 'window-selection-change-functions #'ygg-golden--resize)
    (balance-windows)))

;; opt-in (SPC u g): a focus-driven resizer must not auto-run until the
;; multi-terminal CPU report is understood — a feature can't pin the CPU by default

(yggdrasil-define-keys 'ygg-leader-ui-map
  "s" #'ygg-sticky-mode :label "sticky scroll"
  "g" #'ygg-golden-ratio-mode :label "golden ratio")

;;; Font zoom under the g prefix — frame-wide (default face), unlike text-scale
(defvar ygg-goto-map)

(defvar ygg-font--base nil
  "Default face height before the first zoom, restored by `ygg-font-reset'.")

(defun ygg-font-adjust (delta)
  "Step the `default' face height frame-wide by DELTA (1/10 pt units)."
  (unless ygg-font--base (setq ygg-font--base (face-attribute 'default :height)))
  (set-face-attribute 'default nil :height
                      (max 60 (+ (face-attribute 'default :height) delta))))

(defun ygg-font-increase () "Enlarge the frame font." (interactive) (ygg-font-adjust 10))
(defun ygg-font-decrease () "Shrink the frame font." (interactive) (ygg-font-adjust -10))
(defun ygg-font-reset ()
  "Restore the frame font to its base size."
  (interactive)
  (when ygg-font--base (set-face-attribute 'default nil :height ygg-font--base)))

(yggdrasil-define-keys 'ygg-goto-map
  "=" #'ygg-font-increase :label "font bigger"
  "+" #'ygg-font-increase :label "font bigger"
  "-" #'ygg-font-decrease :label "font smaller"
  "0" #'ygg-font-reset :label "font reset")

;;; Doom modeline as the frame, Yggdrasil's segments inside it

(declare-function doom-modeline-mode "doom-modeline")
(declare-function doom-modeline-def-modeline "doom-modeline")
(declare-function doom-modeline-set-modeline "doom-modeline")
(defvar doom-modeline-icon)
(defvar doom-modeline-buffer-file-name-style)
(defvar doom-modeline-buffer-encoding)
(defvar doom-modeline-modal)
(defvar doom-modeline-bar-width)
(defvar doom-modeline-height)
(defvar doom-modeline-check-simple-format)
(defvar ygg--modeline-tag)
(defvar ygg--macro-tag)

(when (fboundp 'elpaca)
  (elpaca doom-modeline
    (setq doom-modeline-icon nil
          doom-modeline-buffer-file-name-style 'relative-to-project
          doom-modeline-buffer-encoding nil
          doom-modeline-bar-width 1
          doom-modeline-height 1
          doom-modeline-check-simple-format t
          doom-modeline-modal nil)
    (doom-modeline-mode 1)
    (eval '(progn
             (doom-modeline-def-segment ygg-state
               (concat " " ygg--modeline-tag))
             (doom-modeline-def-segment ygg-macro
               (or ygg--macro-tag "")))
          t)
    (doom-modeline-def-modeline 'main
      '(bar ygg-state ygg-macro matches buffer-info remote-host
            buffer-position selection-info)
      '(misc-info check lsp vcs major-mode))
    (doom-modeline-set-modeline 'main 'default)))

(defvar ygg-modeline-file-state-interval 30
  "Seconds between rechecks of a file changed behind the back of Emacs.")

(declare-function doom-mdeline-refresh-buffer-file-state "doom-modeline-segments")

(defun ygg-modeline-slow-file-state-refresh ()
  "Recheck the file state rarely rather than every two seconds.
Doom modeline arms a two second repeat that stats the visited file and
redraws the mode line, for a check that already runs after every
command; an idle session wakes up for it and learns nothing.  Without
icons the state it finds is never drawn, so it is not rechecked at all."
  (when (fboundp 'doom-mdeline-refresh-buffer-file-state)
    (cancel-function-timers #'doom-mdeline-refresh-buffer-file-state)
    (when doom-modeline-icon
      (run-with-timer ygg-modeline-file-state-interval
                      ygg-modeline-file-state-interval
                      #'doom-mdeline-refresh-buffer-file-state))))

(with-eval-after-load 'doom-modeline-segments
  (ygg-modeline-slow-file-state-refresh))

(ygg-focus-mode 1)
(ygg-tile-mode 1)

(provide 'layer-ui)
;;; layer-ui.el ends here

(defvar-local ygg-ui-line-scroll nil
  "Whether wheel events scroll this buffer by lines, not by pixels.
Pixel scrolling lays out every wrapped line it passes, and a reader
that streams thousands of long word-wrapped lines, a trace or the
task tree, spent the whole CPU on one trackpad flick.")

(defun ygg-ui-line-scroll-here ()
  "Scroll this buffer by lines under the wheel."
  (setq ygg-ui-line-scroll t))

(defun ygg-ui--pixel-scroll-or-lines (orig event &rest args)
  "Scroll EVENT's window by lines when its buffer asks, else through ORIG."
  (let ((window (and (consp event) (mwheel-event-window event))))
    (if (and (windowp window)
             (buffer-local-value 'ygg-ui-line-scroll (window-buffer window)))
        (mwheel-scroll event)
      (apply orig event args))))

(with-eval-after-load 'pixel-scroll
  (advice-add 'pixel-scroll-precision :around #'ygg-ui--pixel-scroll-or-lines))
(with-eval-after-load 'ultra-scroll
  (advice-add 'ultra-scroll :around #'ygg-ui--pixel-scroll-or-lines))

(dolist (hook '(ygg-trace-mode-hook ygg-task-tree-mode-hook aob-trace-mode-hook
                aob-subagents-mode-hook ygg-pending-mode-hook))
  (add-hook hook #'ygg-ui-line-scroll-here))

