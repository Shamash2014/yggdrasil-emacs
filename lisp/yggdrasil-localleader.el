;;; yggdrasil-localleader.el --- Per-major-mode localleader on `\' -*- lexical-binding: t; -*-

;; Built-ins wrapped: menu-item :filter dispatch (dynamic prefix binding),
;; derived-mode-p parent-chain walk, project.el, compile.
;; Custom: per-mode sparse keymap registry, def API mirroring the leader.

;;; Code:

(require 'yggdrasil-core)
(require 'project)
(require 'compile)

(defvar ygg-localleader--maps (make-hash-table :test #'eq)
  "Major-mode symbol -> its localleader sparse keymap.")

(defun ygg-localleader--get-map (mode)
  "Return MODE's localleader keymap, creating it on first use."
  (or (gethash mode ygg-localleader--maps)
      (puthash mode (make-sparse-keymap) ygg-localleader--maps)))

(defun yggdrasil-localleader-def (mode key def &optional label)
  "Bind KEY (kbd string) in MODE's localleader map; LABEL feeds which-key."
  (define-key (ygg-localleader--get-map mode) (kbd key) (if label (cons label def) def)))

(defun ygg-localleader--resolve (&optional _real-binding)
  "Menu-item filter: nearest registered ancestor map of `major-mode', or nil."
  (let ((mode major-mode))
    (while (and mode (not (gethash mode ygg-localleader--maps)))
      (setq mode (get mode 'derived-mode-parent)))
    (and mode (gethash mode ygg-localleader--maps))))

;; Also in visual, because a selection is an argument: what it covers is
;; what the mode's verb acts on, and a prefix that only exists in normal
;; makes you drop the selection to reach it.
(dolist (state '(normal visual))
  (yggdrasil-define-keys state
    "\\" '(menu-item "localleader" nil :filter ygg-localleader--resolve)))

(defun ygg-localleader--root ()
  "Project root, falling back to `default-directory'."
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun ygg-localleader--compile (command)
  (let ((default-directory (ygg-localleader--root)))
    (compilation-start command)))

(defun ygg-localleader--require-file ()
  (or buffer-file-name (user-error "Buffer visits no file")))

;;; emacs-lisp-mode

(defun ygg-localleader-elisp-run-tests ()
  "Eval the buffer, then run all loaded ERT tests interactively."
  (interactive)
  (eval-buffer)
  (ert t))

(yggdrasil-localleader-def 'emacs-lisp-mode "e" #'eval-last-sexp "eval last sexp")
(yggdrasil-localleader-def 'emacs-lisp-mode "E" #'eval-buffer "eval buffer")
(yggdrasil-localleader-def 'emacs-lisp-mode "d" #'eval-defun "eval defun")
(yggdrasil-localleader-def 'emacs-lisp-mode "m" #'pp-macroexpand-last-sexp "macroexpand")

;;; elixir-ts-mode / elixir-mode

(defun ygg-localleader-elixir-test ()
  "Run `mix test' on the current file under MIX_ENV=test."
  (interactive)
  (ygg-localleader--compile
   (format "MIX_ENV=test mix test %s" (shell-quote-argument (ygg-localleader--require-file)))))

(defun ygg-localleader-elixir-test-line ()
  "Run `mix test' at point (file:line) under MIX_ENV=test."
  (interactive)
  (ygg-localleader--compile
   (format "MIX_ENV=test mix test %s:%d"
           (shell-quote-argument (ygg-localleader--require-file))
           (line-number-at-pos))))

(declare-function eat "eat" (&optional program arg))
(declare-function eat-semi-char-mode "eat")

(declare-function ygg--term-split-window "layer-terminal" ())

(defun ygg-localleader-elixir-iex ()
  "Open `iex -S mix' in an eat terminal split at the project root."
  (interactive)
  (unless (require 'eat nil t) (user-error "eat is not installed"))
  (let* ((default-directory (ygg-localleader--root))
         (buf (save-window-excursion (eat "iex -S mix" t)))
         (win (ygg--term-split-window)))
    (set-window-buffer win buf)
    (select-window win)
    (eat-semi-char-mode)))

(dolist (mode '(elixir-mode elixir-ts-mode))
  (yggdrasil-localleader-def mode "i" #'ygg-localleader-elixir-iex "iex -S mix"))

;;; python-ts-mode / python-mode

(defun ygg-localleader-python-run ()
  "Run the current file with `python' via `compile'."
  (interactive)
  (ygg-localleader--compile
   (format "python %s" (shell-quote-argument (ygg-localleader--require-file)))))

(defun ygg-localleader-python-test ()
  "Run `pytest' on the current file."
  (interactive)
  (ygg-localleader--compile
   (format "pytest %s" (shell-quote-argument (ygg-localleader--require-file)))))

;;; rust-ts-mode

(defun ygg-localleader-rust-build ()
  "Run cargo build from the project root."
  (interactive)
  (ygg-localleader--compile "cargo build"))

(defun ygg-localleader-rust-run ()
  "Run `cargo run' from the project root."
  (interactive)
  (ygg-localleader--compile "cargo run"))

(defun ygg-localleader-rust-test ()
  "Run `cargo test' from the project root."
  (interactive)
  (ygg-localleader--compile "cargo test"))

(defun ygg-localleader-rust-check ()
  "Run `cargo check' from the project root."
  (interactive)
  (ygg-localleader--compile "cargo check"))

(yggdrasil-localleader-def 'rust-ts-mode "c" #'ygg-localleader-rust-check "cargo check")

;;; dart-mode / dart-ts-mode (Flutter)

(declare-function ygg-device-flutter-id "ygg-device")
(declare-function ygg-device-android-serial "ygg-device")

(defun ygg-localleader--flutter-run-command ()
  "flutter run on the selected device, asking for one when there is none."
  (if (fboundp 'ygg-device-flutter-id)
      (concat "flutter run -d " (shell-quote-argument (ygg-device-flutter-id)))
    "flutter run"))

(defun ygg-localleader-flutter-run ()
  "Run the Flutter app (flutter run) from the project root on the selected device."
  (interactive)
  (let ((default-directory (ygg-localleader--root)))
    (compile (ygg-localleader--flutter-run-command) t)))

(defun ygg-localleader-flutter-test ()
  "Run `flutter test' on the current file."
  (interactive)
  (ygg-localleader--compile
   (format "flutter test %s" (shell-quote-argument (ygg-localleader--require-file)))))

(defun ygg-localleader-flutter-widget-preview ()
  "Start the Flutter widget previewer for @Preview-annotated widgets.
See https://docs.flutter.dev/tools/widget-previewer — runs the
long-lived `flutter widget-preview start' server in a comint buffer."
  (interactive)
  (unless (executable-find "flutter")
    (user-error "flutter is not on PATH"))
  (let ((default-directory (ygg-localleader--root)))
    (compile "flutter widget-preview start" t)))

(defun ygg-localleader-flutter-widget-preview-clean ()
  "Clear the Flutter widget previewer's cached environment."
  (interactive)
  (let ((default-directory (ygg-localleader--root)))
    (compile "flutter widget-preview clean")))

(dolist (mode '(dart-mode dart-ts-mode))
  (yggdrasil-localleader-def mode "P" #'ygg-localleader-flutter-widget-preview-clean "widget preview clean"))

;;; kotlin-mode / kotlin-ts-mode (Gradle / Android)

(defun ygg-localleader--gradle (task)
  "Run gradle TASK from the project root, preferring the ./gradlew wrapper.
Install, run and connected tasks go to the selected Android device."
  (let* ((root (ygg-localleader--root))
         (default-directory root)
         (gradle (if (file-exists-p (expand-file-name "gradlew" root)) "./gradlew" "gradle"))
         (serial (and (string-match-p "\\`\\(install\\|run\\|connected\\)" task)
                      (fboundp 'ygg-device-android-serial)
                      (ygg-device-android-serial)))
         (process-environment (if serial
                                  (cons (concat "ANDROID_SERIAL=" serial) process-environment)
                                process-environment)))
    (ygg-localleader--compile (format "%s %s" gradle task))))

(defun ygg-localleader-kotlin-build () (interactive) (ygg-localleader--gradle "build"))
(defun ygg-localleader-kotlin-run () (interactive) (ygg-localleader--gradle "run"))
(defun ygg-localleader-kotlin-test () (interactive) (ygg-localleader--gradle "test"))
(defun ygg-localleader-kotlin-clean () (interactive) (ygg-localleader--gradle "clean"))
(defun ygg-localleader-kotlin-assemble () (interactive) (ygg-localleader--gradle "assembleDebug"))
(defun ygg-localleader-kotlin-install () (interactive) (ygg-localleader--gradle "installDebug"))

(dolist (mode '(kotlin-mode kotlin-ts-mode))
  (yggdrasil-localleader-def mode "a" #'ygg-localleader-kotlin-assemble "assemble debug")
  (yggdrasil-localleader-def mode "i" #'ygg-localleader-kotlin-install "install debug"))

;;; markdown-mode / gfm-mode

(defun ygg-localleader-markdown-preview ()
  "Preview the buffer with `markdown-preview', if installed."
  (interactive)
  (if (fboundp 'markdown-preview)
      (call-interactively #'markdown-preview)
    (user-error "markdown-preview: markdown-mode is not installed")))

(provide 'yggdrasil-localleader)
;;; yggdrasil-localleader.el ends here
