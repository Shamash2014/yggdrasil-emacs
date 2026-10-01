;;; early-init.el --- Yggdrasil early init -*- lexical-binding: t; -*-

;; Elpaca manages packages; package.el must stay off.
(setq package-enable-at-startup nil)

;; Defer GC and expensive file-name handlers until startup finishes.
(defvar ygg--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; a pause marks the live heap whatever the step: fewer GCs, fixed headroom
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.05
                  file-name-handler-alist ygg--file-name-handler-alist)
            (message "Yggdrasil up in %s (%d GCs)" (emacs-init-time) gcs-done)))

;; a stale .elc beats its source by default, which is how a file compiled
;; against a package that was not loaded yet can keep breaking every boot
;; after the source is fixed
(setq load-prefer-newer t)

(setq native-comp-async-report-warnings-errors nil
      native-comp-jit-compilation t)

;; Strip UI before the first frame exists.
(setq default-frame-alist '((menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)
                            (horizontal-scroll-bars)
                            (fullscreen . maximized)
                            (left-fringe . 8)
                            (right-fringe . 8)
                            ;; macOS: title bar blends into the frame
                            (ns-transparent-titlebar . t)
                            (ns-appearance . dark))
      ;; minimal window title: just the buffer name, no " — GNU Emacs"
      frame-title-format '("%b")
      icon-title-format '("%b")
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      ring-bell-function #'ignore
      use-dialog-box nil)

;; Editing-latency tuning (carried over from the previous config).
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      inhibit-compacting-font-caches t
      idle-update-delay 1.0
      bidi-inhibit-bpa t)
;; truncate-lines is deliberately not set here: `layer-editing' wraps instead,
;; and `global-so-long-mode' is what guards the long-line files
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(setq server-name "e31")
