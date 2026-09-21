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
(yggdrasil-localleader-def 'emacs-lisp-mode "t" #'ygg-localleader-elisp-run-tests "ert run tests")

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

(defun ygg-localleader-elixir-format ()
  "Run `mix format' on the current file."
  (interactive)
  (ygg-localleader--compile
   (format "mix format %s" (shell-quote-argument (ygg-localleader--require-file)))))

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
  (yggdrasil-localleader-def mode "t" #'ygg-localleader-elixir-test "mix test file")
  (yggdrasil-localleader-def mode "T" #'ygg-localleader-elixir-test-line "mix test line")
  (yggdrasil-localleader-def mode "f" #'ygg-localleader-elixir-format "mix format")
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

(dolist (mode '(python-mode python-ts-mode))
  (yggdrasil-localleader-def mode "r" #'ygg-localleader-python-run "run file")
  (yggdrasil-localleader-def mode "t" #'ygg-localleader-python-test "pytest file"))

;;; rust-ts-mode

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

(yggdrasil-localleader-def 'rust-ts-mode "r" #'ygg-localleader-rust-run "cargo run")
(yggdrasil-localleader-def 'rust-ts-mode "t" #'ygg-localleader-rust-test "cargo test")
(yggdrasil-localleader-def 'rust-ts-mode "c" #'ygg-localleader-rust-check "cargo check")

;;; dart-mode / dart-ts-mode (Flutter)

(defun ygg-localleader-flutter-run ()
  "Run the Flutter app (flutter run) from the project root."
  (interactive)
  (let ((default-directory (ygg-localleader--root)))
    (compile "flutter run" t)))

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
  (yggdrasil-localleader-def mode "r" #'ygg-localleader-flutter-run "flutter run")
  (yggdrasil-localleader-def mode "t" #'ygg-localleader-flutter-test "flutter test file")
  (yggdrasil-localleader-def mode "p" #'ygg-localleader-flutter-widget-preview "widget previewer")
  (yggdrasil-localleader-def mode "P" #'ygg-localleader-flutter-widget-preview-clean "widget preview clean"))

;;; swift-mode / swift-ts-mode (konrad1977/swift-development)

(declare-function swift-development-compile-and-run "swift-development")
(declare-function swift-development-compile-app "swift-development")
(declare-function swift-development-run "swift-development")
(declare-function swift-development-reset "swift-development")
(declare-function swift-development-test-at-point "swift-development")
(declare-function swift-development-test-transient "swift-development")
(declare-function swift-development-transient "swift-development")
(declare-function swift-development-settings-transient "swift-development")
(declare-function swiftui-preview-generate "swiftui-preview")
(declare-function swiftui-preview-generate-with-hot-reload "swiftui-preview")

(dolist (mode '(swift-mode swift-ts-mode))
  (yggdrasil-localleader-def mode "r" #'swift-development-compile-and-run "build & run")
  (yggdrasil-localleader-def mode "b" #'swift-development-compile-app "build")
  (yggdrasil-localleader-def mode "R" #'swift-development-run "run (no build)")
  (yggdrasil-localleader-def mode "t" #'swift-development-test-at-point "test at point")
  (yggdrasil-localleader-def mode "T" #'swift-development-test-transient "tests…")
  (yggdrasil-localleader-def mode "x" #'swift-development-reset "reset build")
  (yggdrasil-localleader-def mode "p" #'swiftui-preview-generate "SwiftUI preview")
  (yggdrasil-localleader-def mode "P" #'swiftui-preview-generate-with-hot-reload "preview hot-reload")
  (yggdrasil-localleader-def mode "s" #'swift-development-transient "swift menu")
  (yggdrasil-localleader-def mode "S" #'swift-development-settings-transient "settings"))

;;; kotlin-mode / kotlin-ts-mode (Gradle / Android)

(defun ygg-localleader--gradle (task)
  "Run gradle TASK from the project root, preferring the ./gradlew wrapper."
  (let* ((root (ygg-localleader--root))
         (default-directory root)
         (gradle (if (file-exists-p (expand-file-name "gradlew" root)) "./gradlew" "gradle")))
    (ygg-localleader--compile (format "%s %s" gradle task))))

(defun ygg-localleader-kotlin-build () (interactive) (ygg-localleader--gradle "build"))
(defun ygg-localleader-kotlin-run () (interactive) (ygg-localleader--gradle "run"))
(defun ygg-localleader-kotlin-test () (interactive) (ygg-localleader--gradle "test"))
(defun ygg-localleader-kotlin-clean () (interactive) (ygg-localleader--gradle "clean"))
(defun ygg-localleader-kotlin-assemble () (interactive) (ygg-localleader--gradle "assembleDebug"))
(defun ygg-localleader-kotlin-install () (interactive) (ygg-localleader--gradle "installDebug"))

(dolist (mode '(kotlin-mode kotlin-ts-mode))
  (yggdrasil-localleader-def mode "b" #'ygg-localleader-kotlin-build "gradle build")
  (yggdrasil-localleader-def mode "r" #'ygg-localleader-kotlin-run "gradle run")
  (yggdrasil-localleader-def mode "t" #'ygg-localleader-kotlin-test "gradle test")
  (yggdrasil-localleader-def mode "c" #'ygg-localleader-kotlin-clean "gradle clean")
  (yggdrasil-localleader-def mode "a" #'ygg-localleader-kotlin-assemble "assemble debug")
  (yggdrasil-localleader-def mode "i" #'ygg-localleader-kotlin-install "install debug"))

;;; markdown-mode / gfm-mode

(defun ygg-localleader-markdown-preview ()
  "Preview the buffer with `markdown-preview', if installed."
  (interactive)
  (if (fboundp 'markdown-preview)
      (call-interactively #'markdown-preview)
    (user-error "markdown-preview: markdown-mode is not installed")))

(dolist (mode '(markdown-mode gfm-mode))
  (yggdrasil-localleader-def mode "p" #'ygg-localleader-markdown-preview "preview"))

(provide 'yggdrasil-localleader)
;;; yggdrasil-localleader.el ends here
