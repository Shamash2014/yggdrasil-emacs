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
  (autoload 'bufferfile-copy "bufferfile" nil t)
  (yggdrasil-leader-def "f C" #'bufferfile-copy "copy file"))
(elpaca undo-fu-session
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode 1))
(elpaca ultra-scroll
  (ultra-scroll-mode 1))

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
(require 'ygg-device)
(require 'layer-swift)
(require 'ygg-eglot-x)
(require 'ygg-r-mode)
;; multi-LSP via rass: harper grammar alongside code servers — comment to disable
(require 'layer-rass)
(require 'layer-spell)
(require 'layer-http)
(require 'layer-sessions)
(require 'layer-astgrep)
(require 'layer-git)
(require 'layer-tasks)
(require 'layer-format)
(require 'layer-markdown)
(require 'layer-notebook)
(require 'ygg-kernel-picker)
(require 'layer-aob)
(require 'ygg-ice)
(require 'layer-quickfix)
(require 'layer-pcre)
(require 'layer-dap)
(require 'ygg-dap-ios)
(require 'layer-tramp)
(require 'ygg-ark)
(require 'ygg-kernel-vars)
(require 'ygg-visidata)
(require 'ygg-compose-preview)
(require 'ygg-db)
(require 'ygg-code-verbs)
(require 'ygg-json-lsp)
(require 'layer-react-native)

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
(defun ygg--which-key-faces (&rest _)
  "Helix's hint colours, read from the theme that is on now."
  ;; after load-theme, or the theme clobbers these
  ;; Helix: the key is the thing you are looking for, so it
  ;; carries the colour; what it does is plain text, not a
  ;; comment; a submenu is marked by being another colour again
  ;; the colour has to be set, not inherited: a theme that
  ;; already gave the face a foreground keeps it, and the
  ;; inherit is never consulted
  (when (facep 'which-key-key-face)
    (let ((key (face-attribute 'warning :foreground nil t))
          (group (face-attribute 'success :foreground nil t))
          (text (face-attribute 'default :foreground nil t)))
      (set-face-attribute 'which-key-key-face nil
                          :foreground key :weight 'bold)
      (set-face-attribute 'which-key-command-description-face nil
                          :foreground text :weight 'normal)
      (set-face-attribute 'which-key-group-description-face nil
                          :foreground group :underline nil
                          :weight 'normal))
    (set-face-attribute 'which-key-separator-face nil
                        :inherit 'shadow))
  (when (facep 'which-key-posframe-border)
    (set-face-attribute 'which-key-posframe-border nil
                        :background (face-attribute 'vertical-border
                                                    :foreground nil t))))

(add-hook 'emacs-startup-hook #'ygg--which-key-faces 95)
(add-hook 'enable-theme-functions #'ygg--which-key-faces 95)
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

(defun ygg--posframe-bottom-right (info)
  "Helix\='s keymap box: bottom right, a cell clear of the edges."
  (let* ((size (ygg--posframe-size info))
         (pad (or (frame-char-width) 8)))
    (cons (max 0 (- (plist-get info :parent-frame-width) (car size) pad))
          (max 0 (- (plist-get info :parent-frame-height) (cdr size)
                    (* 2 (or (frame-char-height) 16)))))))

(defun ygg--which-key-enable (&optional frame)
  "Float the keymap box, once there is a FRAME that can show one."
  (with-selected-frame (or (and (frame-live-p frame) frame) (selected-frame))
    (when (and (display-graphic-p)
               (require 'which-key-posframe nil t)
               (not (bound-and-true-p which-key-posframe-mode)))
      (which-key-posframe-mode 1))))

;; registered before the package is near loaded: a daemon makes its first
;; frame when the first client connects, which is as often before the
;; deferred setup below as after it
(add-hook 'server-after-make-frame-hook #'ygg--which-key-enable)
(add-hook 'after-make-frame-functions #'ygg--which-key-enable)

(elpaca which-key-posframe
  ;; Helix's keymap box: bottom right, bordered, hugging its contents,
  ;; key then label — not a bar across the whole frame
  (setq which-key-posframe-poshandler #'ygg--posframe-bottom-right
        which-key-posframe-border-width 1
        which-key-posframe-min-width 0
        which-key-posframe-parameters
        '((left-fringe . 0) (right-fringe . 0)
          (internal-border-width . 10)))
  (setq which-key-max-display-columns 1
        which-key-separator "  "
        which-key-prefix-prefix ""
        ;; the pending keys belong in the status line, which is where
        ;; Helix puts them; the box is only what can follow them
        which-key-show-prefix nil
        which-key-add-column-padding 2
        which-key-max-description-length 32)
  (ygg--which-key-enable))

;;; Visuals: the nvim theme.lua palette (near-monochrome + muted accents)
;;; ported onto built-in modus. Dark = dark_palette(), light = light_palette().
(setq modus-vivendi-palette-overrides
      '((bg-main "#080808")
        (fg-main "#bcbcbc")
        (cursor "#e00000")
        (comment "#c4142e")
        (docstring fg-dim)
        (docmarkup fg-dim)
        (builtin fg-main)
        (constant fg-main)
        (fnname "#e0e0e0")
        (fnname-call fg-main)
        (keyword fg-main)
        (property fg-main)
        (variable-use fg-main)
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
        (bg-completion bg-region)
        (fg-completion-match-0 fg-main)
        (fg-completion-match-1 fg-main)
        (fg-completion-match-2 fg-main)
        (fg-completion-match-3 fg-main)
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
        (info fg-main)
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
        (fg-heading-0 fg-main)
        (fg-heading-1 fg-main)
        (bg-heading-1 unspecified)
        (fg-heading-2 fg-main)
        (bg-heading-2 unspecified)
        (fg-heading-3 fg-main)
        (bg-heading-3 unspecified)
        (fg-heading-4 fg-main)
        (fg-heading-5 fg-main)
        (fg-heading-6 fg-main)
        (fg-heading-7 fg-main)
        (fg-heading-8 fg-main)
        (bg-tab-bar "#0f0f0f")
        (bg-tab-current "#080808")
        (bg-tab-other "#171717")))
(setq modus-operandi-palette-overrides
      '((bg-main "#f3f0e8")
        (fg-main "#141414")
        (cursor "#c00000")
        (comment "#525252")
        (docstring fg-dim)
        (docmarkup fg-dim)
        (builtin fg-main)
        (constant fg-main)
        (fnname "#000000")
        (fnname-call fg-main)
        (keyword fg-main)
        (property fg-main)
        (variable-use fg-main)
        (preprocessor fg-main)
        (string "#3f3f3f")
        (fg-alt "#3f3f3f")
        (fg-dim "#474747")
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
        (bg-region "#dcd7cb")
        (fg-region unspecified)
        (bg-hl-line "#ebe7dd")
        (bg-completion bg-region)
        (fg-completion-match-0 fg-main)
        (fg-completion-match-1 fg-main)
        (fg-completion-match-2 fg-main)
        (fg-completion-match-3 fg-main)
        (fg-line-number-active "#1f1f1f")
        (fg-line-number-inactive "#5c5a55")
        (bg-line-number-active "#e4dfd3")
        (bg-line-number-inactive unspecified)
        (fringe unspecified)
        (border "#c9c3b6")
        (bg-paren-match "#dcd7cb")
        (fg-mode-line-active "#0f0f0f")
        (bg-mode-line-active "#e2ddd1")
        (fg-mode-line-inactive "#3f3f3f")
        (bg-mode-line-inactive "#ebe7dd")
        (fg-link "#4f4f4f")
        (bg-link unspecified)
        (fg-prompt "#0f0f0f")
        (bg-prompt unspecified)
        (err "#a5222f")
        (warning "#8a4a00")
        (info fg-main)
        (success "#3f6f2a")
        (bg-added "#e7f2e4")
        (fg-added "#3f6f2a")
        (bg-changed "#f3eadb")
        (fg-changed "#805000")
        (bg-removed "#f5e2e2")
        (fg-removed "#9a2020")
        (fg-heading-0 fg-main)
        (fg-heading-1 fg-main)
        (bg-heading-1 unspecified)
        (fg-heading-2 fg-main)
        (bg-heading-2 unspecified)
        (fg-heading-3 fg-main)
        (bg-heading-3 unspecified)
        (fg-heading-4 fg-main)
        (fg-heading-5 fg-main)
        (fg-heading-6 fg-main)
        (fg-heading-7 fg-main)
        (fg-heading-8 fg-main)
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
           (muted   (if dark "#5f5f5f" "#5c5a55"))
           (text    (if dark "#bcbcbc" "#1f1f1f"))
           (salient (face-foreground 'success nil t))
           (bg1     (if dark "#0f0f0f" "#ebe7dd"))
           (bg2     (if dark "#171717" "#e4dfd3"))
           (bg3     (if dark "#202020" "#dcd7cb"))
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
         (float-bg (if dark "#121212" "#ebe7dd"))
         (float-edge (if dark "#262626" "#c9c3b6")))
    ;; paper reads thin; its greys live in the palette, out of new dark frames
    (set-face-attribute 'default nil :weight (if dark 'regular 'medium))
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
    (when (facep 'font-lock-operator-face)
      (set-face-attribute 'font-lock-operator-face nil
                          :foreground (if dark "#5f5f5f" "#5a5a5a")))
    (when (facep 'font-lock-punctuation-face)
      (set-face-attribute 'font-lock-punctuation-face nil
                          :foreground (if dark "#5f5f5f" "#5a5a5a")))
    (set-face-attribute 'isearch nil
                        :background (if dark "#d0d0d0" "#4f4f4f")
                        :foreground (if dark "#080808" "#ffffff"))
    (set-face-attribute 'lazy-highlight nil
                        :background (if dark "#969696" "#dcd7cb")
                        :foreground (if dark "#080808" "#141414"))
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
                          :foreground (if dark "#7a7a7a" "#595959"))
      ;; margins take the default background, so on dark a fringe band floats mid-gutter
      (set-face-attribute 'ygg-focus-border nil
                          :background (if dark (face-background 'default nil t) float-bg)))
    (let ((divider (if dark "#080808" "#c9c3b6")))
      (set-face-attribute 'window-divider nil :foreground divider)
      (dolist (face '(window-divider-first-pixel window-divider-last-pixel))
        (when (facep face)
          (set-face-attribute face nil :foreground divider)))
      (set-face-attribute 'internal-border nil
                          :background (face-attribute 'default :background nil t))
      (set-face-attribute 'child-frame-border nil :background float-edge))
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
      (set-face-attribute 'vertico-posframe-border nil :background float-edge))
    (when (facep 'which-key-posframe-border)
      (set-face-attribute 'which-key-posframe-border nil :background float-edge))
    (when (facep 'corfu-default)
      (set-face-attribute 'corfu-default nil :background float-bg))
    (when (facep 'corfu-current)
      (set-face-attribute 'corfu-current nil
                          :background (if dark "#171717" "#dcd7cb")
                          :weight 'bold))
    (when (facep 'corfu-border)
      (set-face-attribute 'corfu-border nil :background float-edge))
    (when (facep 'doom-modeline-bar)
      (set-face-attribute 'doom-modeline-bar nil
                          :background (if dark "#333333" "#c9c3b6")))
    (set-face-attribute 'mode-line nil
                        :background (if dark "#080808" "#f3f0e8")
                        :foreground (if dark "#bcbcbc" "#0f0f0f")
                        :box nil :overline (if dark "#282828" "#c9c3b6")
                        :underline nil)
    (when (facep 'mode-line-active)
      (set-face-attribute 'mode-line-active nil :inherit 'mode-line
                          :background 'unspecified :foreground 'unspecified
                          :box nil :overline 'unspecified :underline nil))
    (set-face-attribute 'mode-line-inactive nil
                        :background (if dark "#080808" "#f3f0e8")
                        :foreground (if dark "#5f5f5f" "#3f3f3f")
                        :box nil :overline (if dark "#282828" "#c9c3b6")
                        :underline nil)
    (when (facep 'doom-modeline-bar-inactive)
      (set-face-attribute 'doom-modeline-bar-inactive nil
                          :background (if dark "#080808" "#f3f0e8")))
    (when (facep 'doom-modeline-buffer-modified)
      (set-face-attribute 'doom-modeline-buffer-modified nil
                          :foreground (if dark "#e0e0e0" "#101010")
                          :weight 'bold :slant 'italic))
    (when (facep 'doom-modeline-buffer-file)
      (set-face-attribute 'doom-modeline-buffer-file nil
                          :foreground (if dark "#e0e0e0" "#101010") :weight 'bold))
    (when (facep 'doom-modeline-minor-mode)
      (set-face-attribute 'doom-modeline-minor-mode nil
                          :foreground (if dark "#707070" "#3f3f3f")))
    (dolist (face '(doom-modeline-emphasis doom-modeline-vcs-default doom-modeline-info))
      (ygg--face face :foreground 'unspecified :inherit 'unspecified
                 :slant 'normal :weight 'bold))
    (ygg--magit-theme-tweaks dark)))

(add-hook 'enable-theme-functions #'ygg--theme-tweaks)
;; packages that define faces after the theme loads need a re-run
(dolist (pkg '(corfu doom-modeline vertico-posframe which-key-posframe magit diff-hl))
  (with-eval-after-load pkg (ygg--theme-tweaks)))
(defvar ygg-theme-file (locate-user-emacs-file "var/theme")
  "Where the chosen theme is kept; read before custom.el loads.")

(defun ygg-theme-saved ()
  "The theme chosen last time, light when none was."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents ygg-theme-file)
          (car (memq (intern (string-trim (buffer-string)))
                     '(modus-operandi modus-vivendi)))))
      'modus-operandi))

(defun ygg-theme-set (theme)
  "Switch to THEME alone and remember it for the next start."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (make-directory (file-name-directory ygg-theme-file) t)
  (write-region (symbol-name theme) nil ygg-theme-file nil 'silent))

(defun ygg-theme-toggle ()
  "Flip between the light and the dark theme."
  (interactive)
  (ygg-theme-set (if (memq 'modus-vivendi custom-enabled-themes)
                     'modus-operandi
                   'modus-vivendi)))

(load-theme (ygg-theme-saved) t)
(with-eval-after-load 'layer-terminal
  (yggdrasil-define-keys 'ygg-leader-open-map
    "T" #'ygg-theme-toggle :label "theme light ⇄ dark"))
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
      scroll-margin 0
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
(set-face-attribute 'default nil :family "JetBrains Mono" :height 130 :weight 'regular)
(when (boundp 'ns-use-thin-smoothing) (setq ns-use-thin-smoothing t))
(set-fontset-font t '(#xe000 . #xf8ff) "JetBrainsMono Nerd Font Mono")
(set-fontset-font t '(#xf0000 . #xfffff) "JetBrainsMono Nerd Font Mono")
;; set before elpaca loads nerd-icons (after-init) so its glyphs come from the
;; installed JBMono patch, not the default "Symbols Nerd Font Mono" we don't ship
(defvar nerd-icons-font-family)
(defvar nerd-icons-color-icons)
(setq nerd-icons-font-family "JetBrainsMono Nerd Font Mono"
      nerd-icons-color-icons nil)

(defun ygg-font-unify (&rest _)
  "Draw the whole interface in the one family.
A proportional face in a frame built on a grid is the one line that
does not line up, and the generic Monospace is not this monospace:
both are pointed at whatever `default' is wearing.  Re-run when a
theme loads, since a theme is free to put them back."
  (let ((mono (face-attribute 'default :family nil t)))
    (dolist (face '(variable-pitch variable-pitch-text
                    fixed-pitch fixed-pitch-serif
                    custom-button custom-button-mouse custom-button-pressed
                    icon-button modus-themes-button))
      (when (facep face)
        (set-face-attribute face nil :family mono)))))

(ygg-font-unify)
(add-hook 'enable-theme-functions #'ygg-font-unify)

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

;;; Compiled where the macros live: a package's macro — vui's components,
;;; elpaca's own — only expands correctly in a session that has loaded it,
;;; so the config compiles itself once everything is up rather than from a
;;; bare batch Emacs.  Layers are left as source: their elpaca calls do not
;;; survive compilation.
(defun ygg--elc-stale-p (file)
  "Whether FILE\='s .elc is missing, or not strictly newer than the source.
Equal timestamps count as stale: a file written and compiled inside the
same second is what `load\=' prefers ever after, and it is the one case
where a wrong .elc can never be replaced by the source beside it."
  (let* ((elc (concat file "c"))
         (theirs (and (file-exists-p elc)
                      (file-attribute-modification-time (file-attributes elc)))))
    (or (null theirs)
        (not (time-less-p (file-attribute-modification-time (file-attributes file))
                          theirs)))))

(defun ygg--readable-elisp-p (file)
  "Whether FILE\='s parens close.
A file being written by an agent while this runs is caught here: a .elc
compiled from half a definition is loaded in preference to the source
that will be correct a second later."
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (condition-case nil (progn (check-parens) t) (error nil))))

(defun ygg-recompile-lisp ()
  "Byte-compile what is stale under lisp/, then native-compile the lot."
  (interactive)
  (let* ((dir (locate-user-emacs-file "lisp"))
         (dirs (list dir (expand-file-name "agent-objects" dir)))
         (done 0))
    (dolist (d dirs)
      (dolist (f (directory-files d t "\\`[^.].*\\.el\\'"))
        (unless (or (string-prefix-p "layer-" (file-name-nondirectory f))
                    ;; tests are loaded from source by the batch runs, never by this Emacs
                    (string-suffix-p "-tests.el" f))
          (when (and (ygg--elc-stale-p f) (ygg--readable-elisp-p f))
            (when (ignore-errors (byte-compile-file f)) (setq done (1+ done)))))))
    ;; native compilation is left to the jit: it works from the .elc,
    ;; where the macros are already expanded, while an async compile of
    ;; the source would re-expand them in a subprocess that has loaded
    ;; none of the packages they come from
    (when (called-interactively-p 'interactive)
      (message "recompiled %d file%s" done (if (= done 1) "" "s")))
    done))

(add-hook 'elpaca-after-init-hook
          ;; not a bare idle timer: a daemon is idle long before elpaca has
          ;; finished, and a file compiled before the package whose macros
          ;; it uses is loaded compiles those macros as function calls
          (lambda () (run-with-idle-timer 5 nil #'ygg-recompile-lisp)))

;;; emacsclient reaches this session (agent tooling relies on it)
(require 'server)
(unless (server-running-p) (server-start))

;;; User key overrides — applied last, wins unconditionally.
;; (yggdrasil-key 'normal "x" #'my-command)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error :no-message)
