;;; init.el --- Yggdrasil bootstrap -*- lexical-binding: t; -*-

;;; Elpaca installer (canonical snippet, v0.12)
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca avy)
(elpaca evil-matchit)
(elpaca better-jumper
  (require 'better-jumper)
  ;; per-window list that spans buffers = vim's cross-file jumplist
  (setq better-jumper-context 'window
        better-jumper-use-evil-jump-advice nil)
  (better-jumper-mode 1))
(elpaca markdown-mode
  ;; GitHub-flavored for .md: task lists, ~~strike~~, tables
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  ;; native code highlighting is jit-lazy (on-screen only); so-long guards huge files
  (setq markdown-fontify-code-blocks-natively t
        markdown-fontify-whole-heading-line t
        markdown-asymmetric-header t
        markdown-gfm-uppercase-checkbox t
        markdown-hide-urls nil
        markdown-enable-math t
        markdown-max-image-size '(800 . nil))
  (when (fboundp 'global-so-long-mode) (global-so-long-mode 1)))
(elpaca (vui :host github :repo "d12frosted/vui.el")
  (require 'vui))
(elpaca bufferfile
  ;; vc t → renames go through git mv; eglot gets willRenameFiles
  (setq bufferfile-use-vc t)
  (yggdrasil-leader-def "f R" #'bufferfile-rename "rename file")
  (yggdrasil-leader-def "f D" #'bufferfile-delete "delete file")
  (yggdrasil-leader-def "f C" #'bufferfile-copy "copy file"))
(elpaca undo-fu-session
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode 1))

;;; lisp/ — compile the engine only; layers stay source (they carry elpaca
;;; macro calls that poison the .elc when compiled outside a session)
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path dir)
  (add-to-list 'load-path (expand-file-name "agent-objects" dir))
  (dolist (stale (directory-files dir t "\\`layer-.*\\.elc\\'"))
    (delete-file stale))
  (dolist (f (directory-files dir t "\\`yggdrasil.*\\.el\\'"))
    (let ((elc (concat f "c")))
      (when (file-newer-than-file-p f elc)
        (byte-compile-file f)))))

;;; Yggdrasil — modal core (match/ex/rect autoload lazily)
(require 'yggdrasil)
(yggdrasil-global-mode 1)
(require 'layer-completion)
(require 'layer-dired)
(require 'layer-terminal)
(require 'layer-browser)
(require 'layer-ui)
(require 'layer-editing)
(require 'layer-lsp)
;; multi-LSP via rass: harper grammar alongside code servers — comment to disable
(require 'layer-rass)
(require 'layer-http)
(require 'layer-sessions)
(require 'layer-astgrep)
(require 'layer-git)
(require 'layer-tasks)
(require 'layer-format)
(require 'layer-markdown)
(require 'layer-aob)
(require 'layer-quickfix)
(require 'layer-pcre)
(require 'layer-dap)
(require 'layer-tramp)

;;; which-key, styled like Helix's hint panel
(setq which-key-idle-delay 0.05
      which-key-idle-secondary-delay 0.05
      which-key-sort-order 'which-key-key-order-alpha
      which-key-max-description-length 28
      which-key-max-display-columns 1
      which-key-separator "  "
      which-key-prefix-prefix "+"
      which-key-add-column-padding 1)
(which-key-mode 1)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; after load-theme, or the theme clobbers these
            (set-face-attribute 'which-key-key-face nil
                                :inherit 'default :weight 'bold)
            (set-face-attribute 'which-key-command-description-face nil
                                :inherit 'font-lock-comment-face)
            (set-face-attribute 'which-key-group-description-face nil
                                :inherit 'warning :weight 'normal))
          95)
(defun ygg--posframe-size (info)
  "Measure the posframe buffer's own content size in pixels.
The :posframe-width/-height in INFO can be stale (previous show) and
exclude fringes/border, so a resized panel clips off-screen; measuring
the text is the only stable source."
  (let* ((cw (or (plist-get info :font-width) (frame-char-width)))
         (ch (or (plist-get info :font-height) (frame-char-height)))
         ;; global line-spacing pads every rendered line in the child frame too
         (ls (let ((v (default-value 'line-spacing)))
               (cond ((floatp v) (round (* ch v)))
                     ((integerp v) v)
                     (t 0))))
         (cols 0)
         (rows 0))
    (with-current-buffer (plist-get info :posframe-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (end-of-line)
          (setq cols (max cols (current-column))
                rows (1+ rows))
          (forward-line 1))))
    (cons (+ (* (1+ cols) cw) 16 4)
          (+ (* rows (+ ch ls)) 4))))

(defun ygg--posframe-bottom-right (info)
  "Explicit bottom-right coords; the stock handler's negative values
render above the top edge on macOS child frames."
  (let ((size (ygg--posframe-size info)))
    (cons (max 0 (- (plist-get info :parent-frame-width) (car size) 16))
          (max 0 (- (plist-get info :parent-frame-height) (cdr size) 16)))))

(defun ygg--posframe-bottom-bar (info)
  "Helix's hint panel: flush to the bottom edge, starting at the left."
  (let ((size (ygg--posframe-size info)))
    (cons 0 (max 0 (- (plist-get info :parent-frame-height) (cdr size))))))

(elpaca which-key-posframe
  ;; Helix shows its hints as a bar across the bottom, key then label,
  ;; one per line — not a floating box in the corner
  (setq which-key-posframe-poshandler #'ygg--posframe-bottom-bar
        which-key-posframe-border-width 0
        which-key-posframe-min-width 0
        which-key-posframe-parameters
        '((left-fringe . 0) (right-fringe . 0)
          (internal-border-width . 8)))
  (setq which-key-max-display-columns nil
        which-key-separator "  "
        which-key-prefix-prefix ""
        which-key-show-prefix 'bottom
        which-key-add-column-padding 2)
  (if (display-graphic-p)
      (which-key-posframe-mode 1)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (when (and (display-graphic-p)
                           (not (bound-and-true-p which-key-posframe-mode)))
                  (which-key-posframe-mode 1))))))

;;; Visuals: the nvim theme.lua palette (near-monochrome + muted accents)
;;; ported onto built-in modus. Dark = dark_palette(), light = light_palette().
(setq modus-vivendi-palette-overrides
      '((bg-main "#080808")
        (fg-main "#bcbcbc")
        (cursor "#e00000")
        (comment "#c4142e")
        (docstring "#c4142e")
        (docmarkup "#c4142e")
        (builtin fg-main)
        (constant fg-main)
        (fnname "#e0e0e0")
        (keyword "#e0a85c")
        (preprocessor fg-main)
        (string "#969696")
        (fg-alt "#969696")
        (fg-prose-code "#969696")
        (fg-prose-verbatim "#969696")
        (fg-prose-macro "#969696")
        (fg-prose-block "#969696")
        (fg-prose-table fg-main)
        (fg-prose-tag "#969696")
        (bg-prose-code unspecified)
        (bg-prose-verbatim unspecified)
        (bg-prose-macro unspecified)
        (bg-prose-block unspecified)
        (type fg-main)
        (variable fg-main)
        (rx-construct "#969696")
        (rx-backslash "#5f5f5f")
        (bg-region "#202020")
        (fg-region unspecified)
        (bg-hl-line "#171717")
        (fg-line-number-active "#e0e0e0")
        (fg-line-number-inactive "#707070")
        (bg-line-number-active "#202020")
        (bg-line-number-inactive unspecified)
        (fringe unspecified)
        (border "#282828")
        (bg-paren-match "#202020")
        (fg-mode-line-active "#d0d0d0")
        (bg-mode-line-active "#171717")
        (fg-mode-line-inactive "#5f5f5f")
        (bg-mode-line-inactive "#0f0f0f")
        (fg-link "#969696")
        (bg-link unspecified)
        (fg-prompt "#e0e0e0")
        (bg-prompt unspecified)
        (err "#C34043")
        (warning "#FFA066")
        (info "#7E9CD8")
        (success "#98BB6C")
        (bg-added "#1A2618")
        (fg-added "#98BB6C")
        (bg-added-refine "#26381f")
        (bg-changed "#2E2A1E")
        (fg-changed "#E8B468")
        (bg-changed-refine "#403a28")
        (bg-removed "#2B1418")
        (fg-removed "#D4484B")
        (bg-removed-refine "#3d1c20")
        (fg-heading-1 "#e0a85c")
        (bg-heading-1 unspecified)
        (fg-heading-2 "#d9a25a")
        (bg-heading-2 unspecified)
        (fg-heading-3 "#b08750")
        (bg-heading-3 unspecified)
        (bg-tab-bar "#0f0f0f")
        (bg-tab-current "#080808")
        (bg-tab-other "#171717")))
(setq modus-operandi-palette-overrides
      '((bg-main "#ffffff")
        (fg-main "#1f1f1f")
        (cursor "#c00000")
        (comment "#9a1a2a")
        (docstring "#9a1a2a")
        (docmarkup "#9a1a2a")
        (builtin fg-main)
        (constant fg-main)
        (fnname "#000000")
        (keyword "#8a5a10")
        (preprocessor fg-main)
        (string "#4f4f4f")
        (fg-alt "#4f4f4f")
        (fg-prose-code "#4f4f4f")
        (fg-prose-verbatim "#4f4f4f")
        (fg-prose-macro "#4f4f4f")
        (fg-prose-block "#4f4f4f")
        (fg-prose-table fg-main)
        (fg-prose-tag "#4f4f4f")
        (bg-prose-code unspecified)
        (bg-prose-verbatim unspecified)
        (bg-prose-macro unspecified)
        (bg-prose-block unspecified)
        (type fg-main)
        (variable fg-main)
        (bg-region "#e6e6e6")
        (fg-region unspecified)
        (bg-hl-line "#f0f0f0")
        (fg-line-number-active "#1f1f1f")
        (fg-line-number-inactive "#6f6f6f")
        (bg-line-number-active "#e6e6e6")
        (bg-line-number-inactive unspecified)
        (fringe unspecified)
        (border "#d8d8d8")
        (bg-paren-match "#e6e6e6")
        (fg-mode-line-active "#1f1f1f")
        (bg-mode-line-active "#f0f0f0")
        (fg-mode-line-inactive "#747474")
        (bg-mode-line-inactive "#fafafa")
        (fg-link "#4f4f4f")
        (bg-link unspecified)
        (fg-prompt "#0f0f0f")
        (bg-prompt unspecified)
        (err "#a5222f")
        (warning "#8a4a00")
        (info "#2d5f8a")
        (success "#3f6f2a")
        (bg-added "#e7f2e4")
        (fg-added "#3f6f2a")
        (bg-changed "#f3eadb")
        (fg-changed "#805000")
        (bg-removed "#f5e2e2")
        (fg-removed "#9a2020")
        (fg-heading-1 "#8a5a10")
        (bg-heading-1 unspecified)
        (fg-heading-2 "#7a5a2a")
        (bg-heading-2 unspecified)
        (fg-heading-3 "#6b5030")
        (bg-heading-3 unspecified)
        (bg-tab-bar "#fafafa")
        (bg-tab-current "#ffffff")
        (bg-tab-other "#f0f0f0")))
(defface ygg-float '((t))
  "Elevated float background (nvim NormalFloat); posframes/popups read it.")

(defun ygg--face (face &rest attrs)
  "Apply ATTRS to FACE when it exists."
  (when (facep face) (apply #'set-face-attribute face nil attrs)))

(defun ygg--magit-theme-tweaks (dark)
  "Mute magit + diff-hl onto the nvim theme.lua palette (neogit groups)."
  (when (facep 'magit-section-heading)
    (let* ((strong  (if dark "#e0e0e0" "#000000"))
           (mid     (if dark "#969696" "#4f4f4f"))
           (muted   (if dark "#5f5f5f" "#747474"))
           (text    (if dark "#bcbcbc" "#1f1f1f"))
           (salient (if dark "#7E9CD8" "#2d5f8a"))
           (bg1     (if dark "#0f0f0f" "#fafafa"))
           (bg2     (if dark "#171717" "#f0f0f0"))
           (bg3     (if dark "#202020" "#e6e6e6"))
           (add-fg  (if dark "#98BB6C" "#3f6f2a")) (add-bg  (if dark "#1A2618" "#e7f2e4"))
           (add-bg2 (if dark "#26381f" "#d6ead0"))
           (chg-fg  (if dark "#E8B468" "#805000"))
           (del-fg  (if dark "#D4484B" "#9a2020")) (del-bg  (if dark "#2B1418" "#f5e2e2"))
           (del-bg2 (if dark "#3d1c20" "#f0d0d0")))
      (ygg--face 'magit-section-heading :foreground strong :weight 'bold :background 'unspecified)
      (ygg--face 'magit-section-secondary-heading :foreground mid :weight 'normal)
      (ygg--face 'magit-section-heading-selection :foreground chg-fg)
      (ygg--face 'magit-section-highlight :background bg1)
      (ygg--face 'magit-diff-context :foreground text :background 'unspecified)
      (ygg--face 'magit-diff-context-highlight :foreground text :background bg1)
      (ygg--face 'magit-diff-added :foreground add-fg :background add-bg)
      (ygg--face 'magit-diff-added-highlight :foreground add-fg :background add-bg2)
      (ygg--face 'magit-diff-removed :foreground del-fg :background del-bg)
      (ygg--face 'magit-diff-removed-highlight :foreground del-fg :background del-bg2)
      (ygg--face 'magit-diff-hunk-heading :foreground mid :background bg1 :weight 'normal)
      (ygg--face 'magit-diff-hunk-heading-highlight :foreground strong :background bg2 :weight 'bold)
      (ygg--face 'magit-diff-hunk-heading-selection :foreground strong :background bg3)
      (ygg--face 'magit-diff-file-heading :foreground strong :weight 'bold :background 'unspecified)
      (ygg--face 'magit-diff-file-heading-highlight :foreground strong :background bg1 :weight 'bold)
      (ygg--face 'magit-diff-lines-heading :foreground strong :background bg2)
      (ygg--face 'magit-diffstat-added :foreground add-fg)
      (ygg--face 'magit-diffstat-removed :foreground del-fg)
      (ygg--face 'magit-branch-local :foreground salient :weight 'bold)
      (ygg--face 'magit-branch-current :foreground salient :weight 'bold :box nil)
      (ygg--face 'magit-branch-remote :foreground mid :weight 'bold)
      (ygg--face 'magit-head :foreground salient)
      (ygg--face 'magit-tag :foreground chg-fg)
      (ygg--face 'magit-refname :foreground muted)
      (ygg--face 'magit-hash :foreground muted)
      (ygg--face 'magit-log-author :foreground mid)
      (ygg--face 'magit-log-date :foreground muted)
      (ygg--face 'magit-log-graph :foreground muted)
      (ygg--face 'magit-dimmed :foreground muted)
      (ygg--face 'magit-filename :foreground text)
      (ygg--face 'magit-header-line :foreground strong :weight 'bold :background 'unspecified)
      (ygg--face 'diff-hl-insert :foreground add-fg :background 'unspecified)
      (ygg--face 'diff-hl-change :foreground chg-fg :background 'unspecified)
      (ygg--face 'diff-hl-delete :foreground del-fg :background 'unspecified))))

(defun ygg--theme-tweaks (&rest _)
  "Face details the modus palette can't express (matches nvim theme.lua)."
  (let* ((dark (memq 'modus-vivendi custom-enabled-themes))
         (float-bg (if dark "#121212" "#f5f5f5")))
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (when (facep 'font-lock-operator-face)
      (set-face-attribute 'font-lock-operator-face nil
                          :foreground (if dark "#5f5f5f" "#747474")))
    (when (facep 'font-lock-punctuation-face)
      (set-face-attribute 'font-lock-punctuation-face nil
                          :foreground (if dark "#5f5f5f" "#747474")))
    (set-face-attribute 'isearch nil
                        :background (if dark "#d0d0d0" "#4f4f4f")
                        :foreground (if dark "#080808" "#ffffff"))
    (set-face-attribute 'lazy-highlight nil
                        :background (if dark "#969696" "#d8d8d8")
                        :foreground (if dark "#080808" "#1f1f1f"))
    (set-face-attribute 'ygg-float nil :background float-bg)
    (ygg--face 'ygg-task-tree--row-highlight
               :inherit 'unspecified :extend t
               :background (if dark "#1f1f1f" "#ececec"))
    (when (facep 'ygg-task-tree--dim)
      (ygg--face 'ygg-task-tree--dim :foreground (if dark "#626262" "#a3a3a3"))
      (ygg--face 'ygg-task-tree--heading
                 :foreground (if dark "#a3a3a3" "#6f6f6f") :weight 'normal)
      (ygg--face 'ygg-task-tree--primary
                 :foreground (if dark "#d9d9d9" "#1f1f1f") :weight 'bold)
      (ygg--face 'ygg-task-tree--accent :foreground "#0091FF"))
    (when (facep 'ygg-focus-dim)
      (set-face-attribute 'ygg-focus-dim nil
                          :foreground (if dark "#7a7a7a" "#8a8a8a"))
      (set-face-attribute 'ygg-focus-border nil :background float-bg))
    (let ((divider (if dark "#080808" "#ffffff")))
      (set-face-attribute 'window-divider nil :foreground divider)
      (dolist (face '(window-divider-first-pixel window-divider-last-pixel))
        (when (facep face)
          (set-face-attribute face nil :foreground divider)))
      (set-face-attribute 'internal-border nil :background divider)
      (set-face-attribute 'child-frame-border nil :background float-bg))
    (setq window-divider-default-places t
          window-divider-default-bottom-width 1
          window-divider-default-right-width 1)
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'internal-border-width 4))
    (setf (alist-get 'internal-border-width default-frame-alist) 4)
    (fringe-mode '(4 . 4))
    (when (fboundp 'window-divider-mode)
      (window-divider-mode 1))
    (dolist (face '(vertico-posframe which-key-posframe))
      (when (facep face)
        (set-face-attribute face nil :background float-bg)))
    (when (facep 'vertico-posframe-border)
      (set-face-attribute 'vertico-posframe-border nil :background float-bg))
    (when (facep 'which-key-posframe-border)
      (set-face-attribute 'which-key-posframe-border nil :background float-bg))
    (when (facep 'corfu-default)
      (set-face-attribute 'corfu-default nil :background float-bg))
    (when (facep 'corfu-current)
      (set-face-attribute 'corfu-current nil
                          :background (if dark "#171717" "#e6e6e6")
                          :weight 'bold))
    (when (facep 'corfu-border)
      (set-face-attribute 'corfu-border nil :background float-bg))
    (when (facep 'doom-modeline-bar)
      (set-face-attribute 'doom-modeline-bar nil
                          :background (if dark "#333333" "#d8d8d8")))
    (set-face-attribute 'mode-line nil
                        :background (if dark "#121212" "#e8e8e8")
                        :foreground (if dark "#bcbcbc" "#1f1f1f")
                        :box nil :overline nil :underline nil)
    (when (facep 'mode-line-active)
      (set-face-attribute 'mode-line-active nil :inherit 'mode-line
                          :background 'unspecified :foreground 'unspecified
                          :box nil :overline nil :underline nil))
    (set-face-attribute 'mode-line-inactive nil
                        :background (if dark "#080808" "#f4f4f4")
                        :foreground (if dark "#5f5f5f" "#9c9c9c")
                        :box nil :overline nil :underline nil)
    (let ((block-bg "#a80000")
          (block-fg "#ffffff")
          (pill-bg (if dark "#202020" "#e6e6e6"))
          (fade (if dark "#202020" "#e6e6e6"))
          (chg-fg (if dark "#E8B468" "#805000"))
          (add-fg (if dark "#98BB6C" "#3f6f2a"))
          (del-fg (if dark "#D4484B" "#9a2020")))
      (ygg--face 'ygg-modeline-pill
                 :background pill-bg :foreground del-fg :weight 'bold)
      (ygg--face 'ygg-modeline-lang
                 :background block-bg :foreground block-fg :weight 'bold)
      (ygg--face 'ygg-modeline-pill-fade :background fade :foreground fade)
      (ygg--face 'ygg-modeline-added :foreground add-fg)
      (ygg--face 'ygg-modeline-removed :foreground del-fg)
      (ygg--face 'ygg-modeline-path :foreground (if dark "#969696" "#4f4f4f"))
      (when (facep 'ygg-state-normal)
        (set-face-attribute 'ygg-state-normal nil :inherit 'ygg-modeline-pill
                            :background pill-bg :foreground del-fg
                            :weight 'bold)
        (set-face-attribute 'ygg-state-visual nil :inherit 'ygg-modeline-pill
                            :background pill-bg :foreground chg-fg
                            :weight 'bold)
        (set-face-attribute 'ygg-state-insert nil :inherit 'ygg-modeline-pill
                            :background pill-bg :foreground add-fg
                            :weight 'bold)))
    (when (facep 'doom-modeline-bar-inactive)
      (set-face-attribute 'doom-modeline-bar-inactive nil
                          :background (if dark "#080808" "#f4f4f4")))
    (when (facep 'doom-modeline-buffer-modified)
      (set-face-attribute 'doom-modeline-buffer-modified nil
                          :foreground (if dark "#FFA066" "#b35a00")))
    (when (facep 'doom-modeline-buffer-file)
      (set-face-attribute 'doom-modeline-buffer-file nil
                          :foreground (if dark "#e0e0e0" "#101010") :weight 'bold))
    (when (facep 'doom-modeline-minor-mode)
      (set-face-attribute 'doom-modeline-minor-mode nil
                          :foreground (if dark "#707070" "#9c9c9c")))
    (ygg--magit-theme-tweaks dark)))

(add-hook 'enable-theme-functions #'ygg--theme-tweaks)
;; packages that define faces after the theme loads need a re-run
(dolist (pkg '(corfu doom-modeline vertico-posframe which-key-posframe magit diff-hl))
  (with-eval-after-load pkg (ygg--theme-tweaks)))
(load-theme 'modus-vivendi t)
(setq display-line-numbers-type 'relative)
(defvar ygg-line-numbers t
  "Set to nil in early custom code to disable line numbers.")
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda () (when ygg-line-numbers (display-line-numbers-mode 1)))))

;;; Sane defaults
;; exit never stalls on children: no process prompt, hard-kill leftovers
;; last (depth 90, after layer-agent's graceful shutdown + session save);
;; on a hard crash the closing ptys SIGHUP terminal children anyway
(setq confirm-kill-processes nil)
(defun ygg--kill-all-jobs ()
  ;; what Emacs holds is a launch shell; the adapters, servers and CLIs it
  ;; started sit in its process group, which `delete-process' never reaches
  (dolist (proc (process-list))
    (let ((pid (and (process-live-p proc) (process-id proc))))
      (set-process-query-on-exit-flag proc nil)
      (when (and pid (/= pid (emacs-pid)))
        (ignore-errors (signal-process (- pid) 'KILL)))
      (ignore-errors (delete-process proc)))))
(add-hook 'kill-emacs-hook #'ygg--kill-all-jobs 90)

(blink-cursor-mode -1)
(setq auto-revert-use-notify t
      auto-revert-avoid-polling t)

;; A day-long Emacs keeps what it no longer needs: a package index nobody
;; is browsing, event logs of servers that answered hours ago, image data
;; for frames long gone.  None of it is a leak — it is all reachable — so
;; only saying to drop it gets it back.
(defvar ygg-memory-trim-interval (* 30 60))

(defun ygg-memory-trim (&optional say)
  "Drop what a long session accumulates and can rebuild.
Reports what it dropped, not megabytes: `memory-limit' answers for
address space, which is not what a freed cache gives back."
  (interactive (list t))
  (let ((killed 0) (trimmed 0))
    ;; the MELPA index is ~10 MiB of cache, read back from disk on demand
    (when (and (boundp 'elpaca-menu-melpa--index-cache)
               (symbol-value 'elpaca-menu-melpa--index-cache))
      (set 'elpaca-menu-melpa--index-cache nil)
      (setq trimmed (1+ trimmed)))
    (dolist (b (buffer-list))
      (let ((name (buffer-name b)))
        (cond
         ;; a language server's transcript, kept for a bug you are not having
         ((string-match-p "\\`\\*EGLOT .*\\(events\\|stderr\\)\\*\\'" name)
          (kill-buffer b)
          (setq killed (1+ killed)))
         ;; an agent's stderr: keep the tail, which is all a crash needs
         ((string-prefix-p " *aob-stderr:" name)
          (with-current-buffer b
            (when (> (buffer-size) 65536)
              (let ((inhibit-read-only t))
                (delete-region (point-min) (- (point-max) 65536))
                (setq trimmed (1+ trimmed)))))))))
    (clear-image-cache t)
    (garbage-collect)
    (when say
      (message "memory: %d buffer(s) dropped, %d cache(s) cleared" killed trimmed))
    (cons killed trimmed)))

(run-with-idle-timer ygg-memory-trim-interval t #'ygg-memory-trim)

(setq use-short-answers t
      select-enable-clipboard t
      scroll-conservatively 101
      scroll-margin 2
      make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      require-final-newline t
      kill-do-not-save-duplicates t
      read-process-output-max (* 1024 1024)
      sentence-end-double-space nil)
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq project-mode-line t)
(add-to-list 'mode-line-misc-info '(project-mode-line project-mode-line-format))
(global-so-long-mode 1)
(electric-pair-mode 1)
(editorconfig-mode 1)
(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(setq-default line-spacing 5)
(setq echo-keystrokes 0.15)
;; Zed-style save-as-you-go; apheleia formats on the resulting save
(setq auto-save-visited-interval 8
      auto-save-visited-predicate
      (lambda () (and buffer-file-name (not (bound-and-true-p ygg--replaying)))))
(auto-save-visited-mode 1)
(pixel-scroll-precision-mode 1)
(set-face-attribute 'default nil :family "JetBrains Mono" :height 130 :weight 'regular)
(when (boundp 'ns-use-thin-smoothing) (setq ns-use-thin-smoothing t))
(set-fontset-font t '(#xe000 . #xf8ff) "JetBrainsMono Nerd Font Mono")
(set-fontset-font t '(#xf0000 . #xfffff) "JetBrainsMono Nerd Font Mono")
;; set before elpaca loads nerd-icons (after-init) so its glyphs come from the
;; installed JBMono patch, not the default "Symbols Nerd Font Mono" we don't ship
(defvar nerd-icons-font-family)
(setq nerd-icons-font-family "JetBrainsMono Nerd Font Mono")

;;; Idle warmup: preload lazy modules + recentf so first use is instant
(run-with-idle-timer
 2 nil
 (lambda ()
   (require 'yggdrasil-match nil t)
   (require 'yggdrasil-ex nil t)
   (require 'yggdrasil-rect nil t)
   (require 'yggdrasil-quickscope nil t)
   (require 'avy nil t)
   (require 'dired nil t)
   (recentf-mode 1)
   (savehist-mode 1)
   (setq enable-recursive-minibuffers t)
   (minibuffer-depth-indicate-mode 1)
   (save-place-mode 1)))

;;; emacsclient reaches this session (agent tooling relies on it)
(require 'server)
(unless (server-running-p) (server-start))

;;; User key overrides — applied last, wins unconditionally.
;; (yggdrasil-key 'normal "x" #'my-command)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error :no-message)
