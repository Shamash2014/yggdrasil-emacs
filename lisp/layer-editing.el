;;; layer-editing.el --- Polished editing display layer -*- lexical-binding: t; -*-

;; Built-ins wrapped: show-paren (matched-bracket highlight, styled by
;; inheritance so it tracks the theme), whitespace (lean visualization of
;; the whitespace that actually matters).
;; Custom: rainbow-delimiters + colorful-mode wiring (inline hex-color
;; preview), and SPC u toggles for all four.

;;; Code:

(require 'yggdrasil-leader)

(declare-function rainbow-delimiters-mode "rainbow-delimiters")
(declare-function colorful-mode "colorful-mode")
(declare-function whitespace-mode "whitespace")

(defvar colorful-use-prefix)
(defvar colorful-only-strings)
(defvar whitespace-style)
(defvar whitespace-display-mappings)
(defvar ygg-leader-ui-map)

;;; 1. show-paren — elegant matched-bracket highlight (built-in, no package)

;; paren.el is preloaded, so these vars are already bound.
(setq show-paren-delay 0.04                     ; snappy without per-keystroke jank
      show-paren-style 'parenthesis
      show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen 'overlay) ; peek the offscreen opener inline

(defun ygg-editing--style-paren (&rest _)
  "Style `show-paren-match' by inheriting the theme's highlight face.
Re-run on theme change so a later theme never clobbers the bold."
  (set-face-attribute 'show-paren-match nil :inherit 'highlight :weight 'bold))

(ygg-editing--style-paren)
(add-hook 'enable-theme-functions #'ygg-editing--style-paren)
(show-paren-mode 1)

;;; 2. rainbow-delimiters — off by default, a toggle only

(when (fboundp 'elpaca)
  (elpaca rainbow-delimiters))

;;; 3. colorful-mode — inline swatch on hex / named color literals

(when (fboundp 'elpaca)
  (elpaca colorful-mode
    (with-eval-after-load 'colorful-mode
      (setq colorful-use-prefix nil       ; tint the literal itself, not a leading glyph
            colorful-only-strings nil))
    (dolist (hook '(prog-mode-hook conf-mode-hook css-mode-hook html-mode-hook
                                   text-mode-hook help-mode-hook))
      (add-hook hook #'colorful-mode))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'prog-mode 'conf-mode 'text-mode) (colorful-mode 1))))))

;;; 4. whitespace — lean visualization (only what ws-butler would trim / mixed indent)

(defun ygg-editing--whitespace-setup ()
  "Enable a lean `whitespace-mode' in code buffers."
  (unless (or (minibufferp) (derived-mode-p 'special-mode))
    (setq-local whitespace-style
                '(face trailing tabs tab-mark space-before-tab space-after-tab))
    (whitespace-mode 1)))

(with-eval-after-load 'whitespace
  ;; a thin tab guide beats the default heavy »; keep the trailing marker default
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?│ ?\t] [?\\ ?\t]))))

(add-hook 'prog-mode-hook #'ygg-editing--whitespace-setup)

;;; 5. line length — 140 columns, and what runs past it wraps

(defcustom ygg-editing-line-length 140
  "Columns a line may use before it is worth breaking.
What `gq' fills to, and where the rule sits when the indicator is on."
  :type 'integer :group 'yggdrasil)

(setq-default fill-column ygg-editing-line-length
              ;; a line that runs off the right edge is a line you scroll to
              ;; read; `global-so-long-mode' still truncates the minified
              ;; files this default was once protecting
              truncate-lines nil
              word-wrap t)

;; two modes turn truncation back on themselves, so the default never
;; reaches them: `tabulated-list-mode' in its own body (and with it the
;; package list, the process list and everything built on them), and org
;; through `org-startup-truncated'.  Nothing here truncates.
(add-hook 'tabulated-list-mode-hook
          (lambda () (setq-local truncate-lines nil)))
(setq org-startup-truncated nil)

;;; Leader toggles (SPC u)

(with-eval-after-load 'layer-ui
  (yggdrasil-define-keys 'ygg-leader-ui-map
    "r" #'rainbow-delimiters-mode :label "rainbow delimiters"
    "c" #'colorful-mode :label "color preview"
    "W" #'whitespace-mode :label "whitespace"
    "t" #'toggle-truncate-lines :label "wrap/truncate long lines"
    "|" #'display-fill-column-indicator-mode :label "rule at column 140"))

(provide 'layer-editing)
;;; layer-editing.el ends here
