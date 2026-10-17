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
(with-eval-after-load 'elpaca
  (add-to-list 'elpaca-ignored-dependencies 'modus-themes))
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
        markdown-max-image-size '(800 . nil)))
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
  (ultra-scroll-mode 1)
  ;; the mode keeps the GC step it saw at load and restores it after every scroll
  (add-hook 'emacs-startup-hook
            (lambda () (setq ultra-scroll--gc-percentage-orig gc-cons-percentage))
            90))

;;; lisp/ — compile the engine only; layers stay source (they carry elpaca
;;; macro calls that poison the .elc when compiled outside a session)
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path dir)
  (add-to-list 'load-path (expand-file-name "agent-objects" dir))
  (add-to-list 'load-path (expand-file-name "vendor/agent-shell" dir))
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

(require 'layer-theme)
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
(defcustom ygg-so-long-line-threshold 10000
  "Longest line, in characters, past which a buffer gets so-long treatment."
  :type 'integer
  :group 'convenience)
(defvar-local ygg--so-long-fns nil)
(put 'ygg--so-long-fns 'permanent-local t)
(defun ygg-so-long-p ()
  (unless (string-prefix-p " " (buffer-name))
    (let ((longest (cadr (buffer-line-statistics))))
      (when (> longest ygg-so-long-line-threshold)
        (setq ygg--so-long-fns
              (if (> longest (* 5 ygg-so-long-line-threshold))
                  '(so-long-mode . so-long-mode-revert)
                '(so-long-minor-mode . turn-off-so-long-minor-mode)))
        t))))
(with-eval-after-load 'so-long
  (add-to-list 'so-long-action-alist
               (list 'ygg-so-long "Enable so-long-mode or so-long-minor-mode"
                     (lambda () (funcall (or (car ygg--so-long-fns) #'ignore)))
                     (lambda () (funcall (or (cdr ygg--so-long-fns) #'ignore)))))
  (setq so-long-action 'ygg-so-long
        so-long-predicate #'ygg-so-long-p)
  (setq so-long-minor-modes (delq 'font-lock-mode so-long-minor-modes))
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1)))
(setq large-file-warning-threshold (* 50 1024 1024))
(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t
        ns-pop-up-frames nil
        locate-command "mdfind"))
(global-so-long-mode 1)
(electric-pair-mode 1)
(editorconfig-mode 1)
(setq editorconfig-trim-whitespaces-mode #'ws-butler-mode)
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
