;;; layer-lsp.el --- LSP + tree-sitter layer -*- lexical-binding: t; -*-

;; Built-ins wrapped: treesit, eglot, flymake, eldoc, xref, indent-region.
;; Custom: treesit textobjects for match-mode (f/t), ] f [ f function
;; motions, the = format verb, and the LSP leader/code bindings.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-selection)
(require 'yggdrasil-motions)
(require 'yggdrasil-leader)
(require 'treesit nil t)
(require 'ygg-ast)
(require 'ygg-jdk)

(declare-function ygg-qf-buffer-create "layer-quickfix" (&optional list))
(declare-function ygg-qf-reset "layer-quickfix" ())

(declare-function eglot-managed-p "eglot")
(declare-function eglot-format "eglot")
(declare-function eglot-rename "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-reconnect "eglot")
(declare-function eglot-inlay-hints-mode "eglot")
(defvar eglot-autoshutdown)
(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)
(declare-function jsonrpc--process "jsonrpc" (conn))
(declare-function eglot--project "eglot")
(defvar ygg-lsp--jdtls-project-homes)

(defvar ygg-lsp-unwatched-servers '("harper-ls")
  "Servers whose file-watch requests are declined.
A watch is one kernel descriptor per directory on macOS, and a prose
checker asking for a whole repository costs thousands of them for a
dictionary it could reread on demand.")

(defun ygg-lsp--server-binary (server)
  "Name of the binary SERVER runs, without its directory or an env wrapper.
Through rass it is the language server rass leads with, not rass."
  (when-let* ((proc (ignore-errors (jsonrpc--process server)))
              (command (ygg-lsp--primary-command (process-command proc)))
              (cmd (seq-find (lambda (arg)
                               (not (or (equal (file-name-nondirectory arg) "env")
                                        (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" arg))))
                             command)))
    (file-name-nondirectory cmd)))

(defun ygg-lsp--primary-command (command)
  "COMMAND, or the first server's command when COMMAND runs rass."
  (if (equal (file-name-nondirectory (car command)) "rass")
      (let ((after (cdr (member "--" command))))
        (seq-take after (or (seq-position after "--") (length after))))
    command))

(defun ygg-lsp--unwatched-server-p (server)
  "Whether SERVER is one whose file watches are declined."
  (member (ygg-lsp--server-binary server) ygg-lsp-unwatched-servers))

(defun ygg-lsp-workspace-configuration (server)
  "Settings answered to SERVER, an empty object for harper-ls.
Harper aborts while logging that a null settings value is not an
object, so it is handed an object with nothing in it.  jdtls gets its
Java settings again, since an empty answer resets it to its defaults.
A TypeScript server gets `ygg-lsp-typescript-configuration'.  Every
other server keeps the nil it was already sent."
  (pcase (ygg-lsp--server-binary server)
    ("harper-ls" (list :harper-ls (make-hash-table :test 'equal)))
    ("jdtls" (ygg-lsp--jdtls-settings
              (gethash (expand-file-name (project-root (eglot--project server)))
                       ygg-lsp--jdtls-project-homes (ygg-jdk-home))))
    (_ (ygg-lsp-typescript-configuration server))))

(defconst ygg-lsp--agent-dir-re "/\\.aob/"
  "Directory the harness works in, at any depth, holding no editing session.")

(defconst ygg-lsp--harness-prose-re "/\\(?:\\.aob\\|openspec\\)/"
  "Directories whose prose is written by the harness, not read by a person.")

(defvar ygg-markdown-large-size)

(defun ygg-lsp-may-join-server-p (&rest _)
  "Whether this buffer may join a language server at all.
A terminal frame is the agent workflow, where a server answers nobody
and costs a process per buffer.  A buffer with no file, and one whose
unmodified text belongs to a path that was moved away, has nothing a
server can open.  The harness scratch tree joins nothing, and prose the
harness wrote gets no grammar checker, nor does large markdown, while
other markdown the owner opens by hand in a window keeps the one it had."
  (let ((file (and buffer-file-name (expand-file-name buffer-file-name))))
    (and (display-graphic-p)
         file
         (or (file-exists-p file) (buffer-modified-p) (zerop (buffer-size)))
         (not (string-match-p ygg-lsp--agent-dir-re file))
         (not (and (memq major-mode '(markdown-mode gfm-mode))
                   (or (string-prefix-p "*" (buffer-name))
                       (and (boundp 'ygg-markdown-large-size)
                            (> (buffer-size) ygg-markdown-large-size))
                       (string-match-p ygg-lsp--harness-prose-re file)))))))

(advice-add 'eglot-ensure :before-while #'ygg-lsp-may-join-server-p)
(advice-add 'eglot--maybe-activate-editing-mode
            :before-while #'ygg-lsp-may-join-server-p)

(defvar ygg-session--restoring)
(defvar ygg-session-buffer-restored-hook)
(defvar-local ygg-lsp--ensure-deferred nil)

(defun ygg-lsp--defer-ensure-while-restoring (&rest _)
  "Hold back `eglot-ensure' for a buffer restored with version control off,
so the server roots at the project it will have once that is back."
  (if (bound-and-true-p ygg-session--restoring)
      (progn (setq ygg-lsp--ensure-deferred t) nil)
    t))

(defun ygg-lsp--ensure-after-restore ()
  (when ygg-lsp--ensure-deferred
    (setq ygg-lsp--ensure-deferred nil)
    (vc-file-setprop default-directory 'project-vc nil)
    (eglot-ensure)))

(advice-add 'eglot-ensure :before-while #'ygg-lsp--defer-ensure-while-restoring
            '((depth . -100)))
(add-hook 'ygg-session-buffer-restored-hook #'ygg-lsp--ensure-after-restore)
(defvar eglot-report-progress)
(defvar eglot-ignored-server-capabilities)

(autoload 'flymake-show-buffer-diagnostics "flymake" nil t)
(declare-function consult-flymake "consult-flymake")
(defvar ygg-qf--header)  ; defined in layer-quickfix; used by the *quickfix* dumps

;; bun global bins (vtsls etc.) land in ~/.bun/bin, which mise leaves off PATH
(let ((bun-bin (expand-file-name "~/.bun/bin")))
  (when (file-directory-p bun-bin)
    (add-to-list 'exec-path bun-bin)
    (unless (member bun-bin (split-string (or (getenv "PATH") "") path-separator))
      (setenv "PATH" (concat bun-bin path-separator (getenv "PATH"))))))

;; sourcekit-lsp lives in the Xcode/Swift toolchain and isn't always on PATH
;; (swiftly/toolchain installs); resolve via xcrun so the eglot swift
;; registration below sees it.  No-op when it's already found (e.g. /usr/bin).
(unless (executable-find "sourcekit-lsp")
  (let ((p (ignore-errors
             (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp 2>/dev/null")))))
    (when (and p (not (string-empty-p p)) (file-executable-p p))
      (add-to-list 'exec-path (file-name-directory p)))))

;;; 1. Tree-sitter modes & grammars

(defun ygg-lsp--remode-fundamentals ()
  "Re-detect modes for file buffers stuck in fundamental-mode.
Session restore can run before deferred mode registration — elpaca-provided
modes like gfm-mode or kotlin-ts-mode — leaving e.g. .ts or .md buffers
modeless.  Runs both after session load and after elpaca finishes, so
whichever completes last re-modes the stragglers."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and buffer-file-name (eq major-mode 'fundamental-mode))
        (ignore-errors (normal-mode))))))

(add-hook 'easysession-after-load-hook #'ygg-lsp--remode-fundamentals)
(add-hook 'elpaca-after-init-hook #'ygg-lsp--remode-fundamentals)

;;; Enable all built-in tree-sitter modes; never prompt per-buffer for grammars
(when (fboundp 'treesit-available-p)
  (when (treesit-available-p)
    ;; Enable all available ts-modes: no per-buffer grammar probing like treesit-auto
    (setopt treesit-enabled-modes t)
    ;; Never auto-install grammars: they're already prebuilt for common languages,
    ;; and 'ask would prompt once per buffer during session restore
    (setopt treesit-auto-install-grammar 'never)
    (setopt treesit-font-lock-level 4)
    ;; anything restored before elpaca finishes sits modeless — heal it now
    (ygg-lsp--remode-fundamentals)))

;; Emacs 30 ships elixir-ts-mode built in; only fetch it where absent.
(unless (fboundp 'elixir-ts-mode)
  (when (fboundp 'elpaca)
    (elpaca elixir-ts-mode)))

;; Emacs ships no kotlin ts-mode; without a major mode, .kt/.kts open in
;; fundamental-mode and the eglot kotlin-lsp registration below never fires.
;; The package supplies the mode + grammar source; build the grammar on
;; first contact so the first .kt open doesn't prompt.
(when (fboundp 'elpaca)
  (elpaca (kotlin-ts-mode :host gitlab :repo "bricka/emacs-kotlin-ts-mode")
    (add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-ts-mode))
    (with-eval-after-load 'kotlin-ts-mode
      (add-to-list 'treesit-language-source-alist
                   '(kotlin "https://github.com/fwcd/tree-sitter-kotlin"))
      (unless (treesit-ready-p 'kotlin t)
        (ignore-errors (treesit-install-language-grammar 'kotlin))))))

;; Emacs ships no dart mode; .dart must use the non-ts dart-mode so the dart
;; eglot hook fires and the dape debugger can find the language.
(when (fboundp 'elpaca)
  (elpaca dart-mode
    (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))))

;; Emacs ships no swift mode; without it .swift opens in fundamental-mode and
;; the sourcekit-lsp registration + hook below never fire.
(when (fboundp 'elpaca)
  (elpaca swift-mode
    (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))))

;; dockerfile-ts-mode ships in Emacs 31, but no default filename pattern exists
;; (Dockerfile has no extension), and the grammar is unbuilt — wire both.
(add-to-list 'auto-mode-alist
             '("\\(?:Dockerfile\\|Containerfile\\)\\(?:\\.[^/]*\\)?\\'"
               . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-ts-mode))
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

;; compose files are yaml and nothing else: docker-compose-mode was
;; archived in 2024 and its whole offer — completion of compose keys —
;; is what a yaml language server does from the published schema, which
;; is also the one that knows this year's spec.
(run-with-idle-timer
 2 nil (lambda ()
         (require 'treesit)
         (unless (treesit-ready-p 'dockerfile t)
           (ignore-errors (treesit-install-language-grammar 'dockerfile)))))

;;; combobulate — structural editing over tree-sitter.  Navigation
;;; (siblings/expand) already lives on M-n/M-p/M-o/M-i, so we expose its
;;; *edits*: M-j/M-k drag a node, SPC c s the rest (mark/clone/kill/splice).

(declare-function combobulate-mode "combobulate")
(declare-function combobulate-drag-up "combobulate")
(declare-function combobulate-drag-down "combobulate")
(declare-function combobulate-mark-node-dwim "combobulate")
(declare-function combobulate-clone-node-dwim "combobulate")
(declare-function combobulate-kill-node-dwim "combobulate")
(declare-function combobulate-splice-up "combobulate")
(declare-function combobulate-cursor-edit-node-type-dwim "combobulate")
(declare-function combobulate-transpose-sexps "combobulate")

(defun ygg--maybe-combobulate ()
  "Enable combobulate only where a tree-sitter parser is live."
  (when (treesit-parser-list) (combobulate-mode 1)))

(when (fboundp 'elpaca)
  (elpaca (combobulate :host github :repo "mickeynp/combobulate")
    (add-hook 'prog-mode-hook #'ygg--maybe-combobulate)))

;;; dumb-jump — xref definitions everywhere eglot isn't

(declare-function dumb-jump-xref-activate "dumb-jump")
(defvar dumb-jump-force-searcher)
(defvar dumb-jump-prefer-searcher)

(when (fboundp 'elpaca)
  (elpaca dumb-jump
    (setq dumb-jump-prefer-searcher 'rg)
    ;; append: eglot's backend must stay first in managed buffers
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90)))

(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; hel-style: when the winning backend finds nothing, retry the query
;; with dumb-jump instead of giving up
(define-advice xref--create-fetcher (:around (orig input kind arg) ygg-fallback)
  (let ((fetcher (funcall orig input kind arg)))
    (lambda ()
      (condition-case err
          (funcall fetcher)
        (user-error
         (if (and (fboundp 'dumb-jump-xref-activate)
                  (not (equal xref-backend-functions '(dumb-jump-xref-activate))))
             (let ((xref-backend-functions '(dumb-jump-xref-activate)))
               (funcall (funcall orig input kind arg)))
           (signal (car err) (cdr err))))))))

;;; Server binary probe — warn at load, never fail

;; `executable-find' only proves a file is sitting on PATH.  A pip or brew
;; console script outlives the interpreter its shebang names: a Homebrew
;; python bump orphaned basedpyright-langserver on a python3.13 that no
;; longer exists, so the roster happily registered it, armed `eglot-ensure',
;; and every Python buffer then lost its server to ENOENT — xref fell back
;; to etags and go-to-definition went dead.  Reading the shebang catches that
;; whole class before a hook is armed.  Unreadable or exotic launchers are
;; trusted as-is: a probe that guesses wrong must never disarm a live server.
(defun ygg-lsp--executable (name)
  "Path to NAME on PATH, or nil when it is a script whose interpreter is gone."
  (when-let* ((path (executable-find name)))
    (let ((interp (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents-literally (file-truename path) nil 0 128)
                      (when (looking-at "#![ \t]*\\([^ \t\n]+\\)[ \t]*\\([^ \t\n]*\\)")
                        (let ((cmd (match-string 1)) (arg (match-string 2)))
                          (if (string-match-p "/env\\'" cmd) arg cmd)))))))
      (if (and interp
               (not (string-empty-p interp))
               (not (string-prefix-p "-" interp))
               (not (if (string-search "/" interp)
                        (file-executable-p interp)
                      (executable-find interp))))
          nil
        path))))

;;; Server roster (ported from nvim core/lsp.lua): register always, hook
;;; when the buffer opens and the binary exists, so no buffer ever waits on a
;;; doomed connect.  PATH is not final at load: mise and envrc extend it per buffer.

(defconst ygg-lsp--server-binaries
  '(("expert" . "Elixir (expert)")
    ("lua-language-server" . "Lua")
    ("rust-analyzer" . "Rust")
    ("gopls" . "Go")
    ("dart" . "Dart")
    ("sourcekit-lsp" . "Swift")
    ("emmet-language-server" . "Emmet")
    ("ngserver" . "Angular"))
  "Server binaries probed with `ygg-lsp--executable'; missing ones only warn.")

(defvar ygg-lsp--warned nil
  "Labels already reported as not runnable this session.")

(defun ygg-lsp--warn-once (label)
  (unless (member label ygg-lsp--warned)
    (push label ygg-lsp--warned)
    (message "yggdrasil-lsp: %s not runnable" label)))

(defun ygg-lsp--binary-label (bin)
  (when-let* ((name (cdr (assoc bin ygg-lsp--server-binaries))))
    (format "%s server (%s)" name bin)))

;; uvx last, the way the TypeScript chain below keeps bunx last: it needs no
;; global install, so it leaves nothing on PATH to rot the next time the system
;; python moves out from under a launcher script.  Python and TypeScript stay
;; out of the roster above: with a fallback chain the warning belongs to the
;; chain, not to any one binary in it.
(defun ygg-lsp--python-server ()
  "Command eglot runs for Python, or nil when no server can be reached."
  (cond ((ygg-lsp--executable "basedpyright-langserver")
         '("basedpyright-langserver" "--stdio"))
        ((ygg-lsp--executable "pyright-langserver")
         '("pyright-langserver" "--stdio"))
        ((ygg-lsp--executable "uvx")
         '("uvx" "--from" "basedpyright" "basedpyright-langserver" "--stdio"))))

(defun ygg-lsp-python-contact (&rest _)
  (or (ygg-lsp--python-server) (user-error "No Python language server runnable")))

(defvar ygg-lsp--dart-which-cache (make-hash-table :test 'equal)
  "Real `dart' per (pin-root . pin-file mtimes), from `mise which dart'.")

(defconst ygg-lsp--mise-pin-files '(".tool-versions" "mise.toml" ".mise.toml"))

(defun ygg-lsp--mise-pin-root (dir)
  (or (locate-dominating-file
       dir (lambda (d) (seq-some (lambda (f) (file-exists-p (expand-file-name f d)))
                                 ygg-lsp--mise-pin-files)))
      dir))

(defun ygg-lsp--mise-shim-p (path)
  (or (string-match-p "/mise/shims/" path)
      (equal (file-name-nondirectory (file-truename path)) "mise")))

(defun ygg-lsp--mise-which-dart (dir)
  "Output of `mise which dart' run in DIR, or nil on any failure."
  (when-let* ((mise (or (executable-find "mise")
                        (let ((f (expand-file-name "~/.local/bin/mise")))
                          (and (file-executable-p f) f)))))
    (let* ((default-directory dir)
           (out (generate-new-buffer " *mise-which*"))
           (err (generate-new-buffer " *mise-which-err*"))
           (proc (ignore-errors
                   (make-process :name "mise-which" :buffer out :noquery t
                                 :connection-type 'pipe :stderr err
                                 :sentinel #'ignore
                                 :command (list mise "which" "dart"))))
           (deadline (+ (float-time) 5)))
      (unwind-protect
          (when proc
            (while (and (process-live-p proc) (< (float-time) deadline))
              (accept-process-output proc 0.1))
            (if (process-live-p proc)
                (delete-process proc)
              (accept-process-output proc 0)
              (when (zerop (process-exit-status proc))
                (let ((path (string-trim (with-current-buffer out (buffer-string)))))
                  (and (file-executable-p path) path)))))
        (kill-buffer out)
        (when-let* ((p (get-buffer-process err))) (delete-process p))
        (kill-buffer err)))))

(defun ygg-lsp--mise-real-dart (dir)
  (let* ((root (ygg-lsp--mise-pin-root dir))
         (key (cons root
                    (mapcar (lambda (f)
                              (file-attribute-modification-time
                               (file-attributes (expand-file-name f root))))
                            ygg-lsp--mise-pin-files))))
    (or (gethash key ygg-lsp--dart-which-cache)
        (when-let* ((real (ygg-lsp--mise-which-dart dir)))
          (puthash key real ygg-lsp--dart-which-cache)))))

(defun ygg-lsp--fvm-sdk (dir)
  (when-let* ((root (locate-dominating-file dir ".fvm/flutter_sdk")))
    (expand-file-name ".fvm/flutter_sdk" root)))

(defun ygg-lsp--flutter-sdk-dart (sdk)
  "Dart inside the Flutter SDK at SDK, preferring its cached engine copy."
  (let ((cached (expand-file-name "bin/cache/dart-sdk/bin/dart" sdk))
        (wrapper (expand-file-name "bin/dart" sdk)))
    (cond ((file-executable-p cached) cached)
          ((file-executable-p wrapper) wrapper))))

(defun ygg-lsp--dart-sdk-dart (dir)
  "Absolute `dart' for DIR: FVM sdk, else mise's pin, on the Flutter sdk's Dart."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (or (when-let* ((sdk (ygg-lsp--fvm-sdk dir)))
          (ygg-lsp--flutter-sdk-dart sdk))
        (when-let* ((dart (ygg-lsp--executable "dart")))
          (let ((real (or (and (ygg-lsp--mise-shim-p dart)
                               (ygg-lsp--mise-real-dart dir))
                          dart)))
            (let ((sdk (file-name-directory
                        (directory-file-name (file-name-directory real)))))
              (or (and (equal (file-name-nondirectory real) "dart")
                       (or (file-executable-p
                            (expand-file-name "bin/cache/dart-sdk/bin/dart" sdk))
                           (file-exists-p (expand-file-name "bin/flutter" sdk)))
                       (ygg-lsp--flutter-sdk-dart sdk))
                  real))))
        "dart")))

(defun ygg-lsp-dart-contact (&rest _)
  "Dart's server, on the Flutter-pinned sdk's Dart when the project has one."
  (list (ygg-lsp--dart-sdk-dart (if-let* ((p (project-current))) (project-root p) default-directory))
        "language-server" "--protocol=lsp"))

(defun ygg-lsp--angular-root ()
  (locate-dominating-file default-directory "angular.json"))

(defun ygg-lsp-html-contact (&rest _)
  "Eglot contact for HTML: ngserver in an Angular project, else a full
HTML server, else Emmet."
  (let ((angular (and (ygg-lsp--executable "ngserver") (ygg-lsp--angular-root))))
    (cond (angular
           (let ((modules (expand-file-name "node_modules" angular)))
             (list "ngserver" "--stdio"
                   "--tsProbeLocations" modules "--ngProbeLocations" modules)))
          ((ygg-lsp--executable "vscode-html-language-server")
           '("vscode-html-language-server" "--stdio"))
          ((ygg-lsp--executable "emmet-language-server")
           '("emmet-language-server" "--stdio"))
          (t (user-error "No HTML language server runnable")))))

;; Emmet takes CSS only where no full CSS server would be displaced
(defun ygg-lsp--emmet-css-p ()
  (and (ygg-lsp--executable "emmet-language-server")
       (not (ygg-lsp--executable "vscode-css-language-server"))))

(defun ygg-lsp-css-contact (&rest _)
  (if (ygg-lsp--emmet-css-p)
      '("emmet-language-server" "--stdio")
    '("vscode-css-language-server" "--stdio")))

(defcustom ygg-lsp-ts-server 'tsls
  "The TypeScript/JavaScript language server.
tsls is typescript-language-server and vtsls the server of VS Code's
TypeScript extension; each falls back to the other when its binary is
missing, and bunx runs vtsls when neither is installed.  tsgo is the
native TypeScript 7 server, `ygg-lsp-tsgo-program' run as tsc --lsp
--stdio, fast but young; it is never paired with ESLint through rass.
Install it under a prefix of its own, so it never shadows a project's
tsc:

  npm install --prefix ~/.local/share/tsgo typescript@7

A new value applies to the next server started."
  :type '(choice (const :tag "typescript-language-server" tsls)
                 (const :tag "vtsls" vtsls)
                 (const :tag "TypeScript 7 native (tsc --lsp)" tsgo))
  :group 'eglot)

(defcustom ygg-lsp-tsgo-program
  (expand-file-name "~/.local/share/tsgo/node_modules/.bin/tsc")
  "The TypeScript 7 tsc that `ygg-lsp-ts-server' tsgo runs."
  :type 'file :group 'eglot)

(defconst ygg-lsp--tsls-initialization-options
  '(:disableAutomaticTypingAcquisition t
    :preferences (:includeInlayParameterNameHints "all"
                  :includeInlayVariableTypeHints t
                  :includeInlayFunctionLikeReturnTypeHints t
                  :includeInlayPropertyDeclarationTypeHints t
                  :providePrefixAndSuffixTextForRename :json-false))
  "What typescript-language-server is started with.")

(defun ygg-lsp-ts-command ()
  "The command for `ygg-lsp-ts-server', a fallback, or nil when none runs."
  (let ((tsls (when (ygg-lsp--executable "typescript-language-server")
                `("typescript-language-server" "--stdio"
                  :initializationOptions ,ygg-lsp--tsls-initialization-options)))
        (vtsls (when (ygg-lsp--executable "vtsls") '("vtsls" "--stdio")))
        (bunx (when (ygg-lsp--executable "bunx")
                '("bunx" "@vtsls/language-server" "--stdio")))
        (tsgo (when (file-executable-p ygg-lsp-tsgo-program)
                (list ygg-lsp-tsgo-program "--lsp" "--stdio"))))
    (pcase ygg-lsp-ts-server
      ('tsgo (or tsgo tsls vtsls bunx))
      ('vtsls (or vtsls tsls bunx))
      (_ (or tsls vtsls bunx)))))

(defun ygg-lsp-ts-contact (&rest _)
  "Eglot contact for TypeScript and JavaScript buffers."
  (ygg-lsp-ts-command))

(defun ygg-lsp-ts-server-kind (server)
  "Which TypeScript server SERVER runs, tsls, vtsls or tsgo, through rass or not."
  (when-let* ((process (ignore-errors (jsonrpc--process server))))
    (seq-some (lambda (arg)
                (if (equal arg "@vtsls/language-server") 'vtsls
                  (pcase (file-name-nondirectory arg)
                    ("typescript-language-server" 'tsls)
                    ("vtsls" 'vtsls)
                    ("tsc" 'tsgo))))
              (process-command process))))

(defun ygg-lsp-typescript-configuration (server)
  "Workspace settings for SERVER when it is a TypeScript server.
Completing a function call brings its argument placeholders along, for
eglot-tempel to step through.  vtsls also runs the workspace TypeScript."
  (pcase (ygg-lsp-ts-server-kind server)
    ('tsls '(:completions (:completeFunctionCalls t)))
    ('vtsls '(:vtsls (:autoUseWorkspaceTsdk t)
              :typescript (:suggest (:completeFunctionCalls t))
              :javascript (:suggest (:completeFunctionCalls t))))))

;;; Java: jdtls on JDK 21, one workspace per project, jdt:// sources via the server

(defconst ygg-lsp--mise-installs (expand-file-name "~/.local/share/mise/installs/")
  "Where mise keeps each tool's versions.")

(defun ygg-lsp--newest-version-dir (glob)
  "Newest version directory matching GLOB, skipping mise's alias links."
  (car (sort (seq-filter (lambda (d)
                           (and (not (file-symlink-p d))
                                (string-match-p "\\`[0-9][0-9.]*\\'"
                                                (file-name-nondirectory d))))
                         (file-expand-wildcards glob))
             (lambda (a b) (version< (file-name-nondirectory b)
                                     (file-name-nondirectory a))))))

(defun ygg-lsp--jdtls ()
  "The jdtls launcher, or nil.
The daemon's PATH is frozen into the app bundle at build time, so a
jdtls mise installed later is looked up in mise's install tree."
  (or (ygg-lsp--executable "jdtls")
      (when-let* ((dir (ygg-lsp--newest-version-dir
                        (expand-file-name "http-jdtls/*" ygg-lsp--mise-installs))))
        (ygg-lsp--executable (expand-file-name "bin/jdtls" dir)))))

(defun ygg-lsp--kotlin-lsp ()
  "JetBrains kotlin-lsp, looked up in mise's install tree like jdtls."
  (or (ygg-lsp--executable "kotlin-lsp")
      (when-let* ((dir (ygg-lsp--newest-version-dir
                        (expand-file-name "http-kotlin-lsp/*" ygg-lsp--mise-installs))))
        (ygg-lsp--executable (expand-file-name "bin/kotlin-lsp" dir)))))

(defun ygg-lsp--kotlin-lsp-contact (_interactive _project)
  "kotlin-lsp run on its bundled runtime, importing on this buffer's JDK.
Its Gradle import otherwise takes the bundled runtime, or any JDK it finds."
  (if-let* ((home (ygg-jdk-settled-home)))
      (list "env"
            (concat "JAVA_HOME=" home)
            (concat "IJ_JAVA_OPTIONS="
                    (string-join
                     (delq nil (list (getenv "IJ_JAVA_OPTIONS")
                                     (concat "-Dcom.jetbrains.ls.imports.gradle.java.home=" home)
                                     (concat "-DJB_MAVEN_JAVA_HOME=" home)))
                     " "))
            (ygg-lsp--kotlin-lsp) "--stdio")
    (list (ygg-lsp--kotlin-lsp) "--stdio")))

(defconst ygg-lsp--jdtls-dir
  (file-name-as-directory (file-truename (locate-user-emacs-file "var/jdtls/")))
  "Holds each project's jdtls workspace and the class sources it served.")

(defun ygg-lsp--java-runtimes (project-home)
  "Alist of major version to JDK home, one per major, from mise's installs.
The pin, then PROJECT-HOME, take the place of any other JDK of their major."
  (let ((root (expand-file-name "java" ygg-lsp--mise-installs))
        found)
    (when (file-directory-p root)
      (dolist (home (directory-files root t "\\`[^.]"))
        (when-let* (((not (file-symlink-p home)))
                    ((file-executable-p (expand-file-name "bin/javac" home)))
                    (major (ygg-jdk-major home)))
          (unless (assq major found) (push (cons major home) found)))))
    (dolist (home (list ygg-jdk-default-home project-home))
      (when-let* ((home) (major (ygg-jdk-major home)))
        (setf (alist-get major found) home)))
    (sort found (lambda (a b) (< (car a) (car b))))))

(defun ygg-lsp--jdtls-java-home (project-home)
  "JDK jdtls itself runs on: PROJECT-HOME if it is 21 or newer, else the pin.
jdtls needs 21 to run; the project still builds against PROJECT-HOME."
  (or (seq-find (lambda (home) (and home (>= (or (ygg-jdk-major home) 0) 21)))
                (list project-home ygg-jdk-default-home))
      (cdr (car (last (seq-filter (lambda (rt) (>= (car rt) 21))
                                  (ygg-lsp--java-runtimes nil)))))))

(defvar ygg-lsp--jdtls-project-homes (make-hash-table :test 'equal)
  "Project root to the JDK its jdtls was started for.
Settings are asked for again outside the project's buffers, where its
mise or direnv environment is not in effect.")

(defun ygg-lsp--jdtls-settings (home)
  "The java settings jdtls starts with for a project on the JDK at HOME."
  `(:java
    (:configuration
     (:updateBuildConfiguration "automatic"
      :runtimes
      ,(vconcat
        (mapcar (pcase-lambda (`(,major . ,runtime))
                  `(:name ,(format (if (< major 9) "JavaSE-1.%d" "JavaSE-%d") major)
                    :path ,runtime
                    ,@(when (equal runtime home) '(:default t))))
                (ygg-lsp--java-runtimes home))))
     :import (:maven (:enabled t)
              :gradle (:enabled t ,@(when home `(:java (:home ,home)))))
     :maven (:downloadSources t)
     :eclipse (:downloadSources t)
     :autobuild (:enabled t)
     :format (:enabled t)
     :completion
     (:favoriteStaticMembers
      ["org.junit.jupiter.api.Assertions.*"
       "org.junit.jupiter.api.Assumptions.*"
       "org.junit.jupiter.api.DynamicTest.*"
       "org.assertj.core.api.Assertions.*"
       "org.mockito.Mockito.*"
       "org.mockito.ArgumentMatchers.*"
       "org.mockito.BDDMockito.*"])
     :referencesCodeLens (:enabled :json-false)
     :implementationCodeLens "none"
     :signatureHelp (:enabled t))))

(defun ygg-lsp--lombok-agent (root)
  "Newest lombok jar in the local Maven repository when ROOT's pom uses it."
  (let ((pom (expand-file-name "pom.xml" root)))
    (when (and (file-readable-p pom)
               (with-temp-buffer
                 (insert-file-contents pom)
                 (re-search-forward
                  "<groupId>[ \t\n]*org\\.projectlombok[ \t\n]*</groupId>" nil t)))
      (when-let* ((dir (ygg-lsp--newest-version-dir
                        (expand-file-name "~/.m2/repository/org/projectlombok/lombok/*")))
                  (jar (expand-file-name
                        (format "lombok-%s.jar" (file-name-nondirectory dir)) dir)))
        (and (file-readable-p jar) jar)))))

(defun ygg-lsp--jdtls-bundles ()
  "The java-debug and java-test plugin jars mise installed, for jdtls to load."
  (append
   (when-let* ((dir (ygg-lsp--newest-version-dir
                     (expand-file-name "http-java-debug/*" ygg-lsp--mise-installs))))
     (file-expand-wildcards
      (expand-file-name "extension/server/com.microsoft.java.debug.plugin-*.jar" dir)))
   (when-let* ((version (ygg-lsp--newest-version-dir
                         (expand-file-name "http-java-test/*" ygg-lsp--mise-installs)))
               (dir (expand-file-name "extension" version))
               (manifest (expand-file-name "package.json" dir))
               ((file-readable-p manifest)))
     (with-temp-buffer
       (insert-file-contents manifest)
       (mapcar (lambda (jar) (expand-file-name jar dir))
               (gethash "javaExtensions"
                        (gethash "contributes" (json-parse-buffer))))))))

(defun ygg-lsp--jdtls-contact (_interactive project)
  "The jdtls command for PROJECT, with a workspace keyed by its root."
  (let* ((root (expand-file-name (project-root project)))
         (home (ygg-jdk-settled-home))
         (java (ygg-lsp--jdtls-java-home home))
         (lombok (ygg-lsp--lombok-agent root))
         (bundles (ygg-lsp--jdtls-bundles)))
    (puthash root home ygg-lsp--jdtls-project-homes)
    `(,(ygg-lsp--jdtls)
      ,@(when java (list "--java-executable" (expand-file-name "bin/java" java)))
      "--jvm-arg=-Djava.import.generatesMetadataFilesAtProjectRoot=false"
      ,@(when lombok (list (concat "--jvm-arg=-javaagent:" lombok)))
      "-data" ,(expand-file-name
                (format "workspaces/%s-%s"
                        (file-name-nondirectory (directory-file-name root))
                        (substring (md5 root) 0 10))
                ygg-lsp--jdtls-dir)
      :initializationOptions
      (:settings ,(ygg-lsp--jdtls-settings home)
       ,@(when bundles (list :bundles (vconcat bundles)))
       :extendedClientCapabilities (:classFileContentsSupport t)))))

(defvar eglot--servers-by-project)
(defvar eglot-extend-to-xref)
(declare-function eglot-current-server "eglot")
(declare-function eglot-path-to-uri "eglot")
(declare-function jsonrpc-request "jsonrpc")
(declare-function jsonrpc-notify "jsonrpc")
(declare-function yggdrasil-localleader-def "yggdrasil-localleader")

(defun ygg-lsp--jdtls-p (server)
  (equal (ygg-lsp--server-binary server) "jdtls"))

(defun ygg-lsp--jdtls-server (&optional anywhere)
  "This buffer's jdtls server, or with ANYWHERE any live one."
  (or (when-let* ((s (and (fboundp 'eglot-current-server) (eglot-current-server))))
        (and (ygg-lsp--jdtls-p s) s))
      (and anywhere (boundp 'eglot--servers-by-project)
           (catch 'found
             (maphash (lambda (_ servers)
                        (when-let* ((s (seq-find #'ygg-lsp--jdtls-p servers)))
                          (throw 'found s)))
                      eglot--servers-by-project)))))

(defvar ygg-lsp--jdt-uris (make-hash-table :test 'equal)
  "Cached class source file to the jdt URI it was read from.")

(defconst ygg-lsp--jdt-class-uri-re
  "\\`jdt://contents/.*?/\\([^/?]+\\)\\.\\(?:class\\|java\\)\\?"
  "A whole jdt class URI, its class name in group 1.")

(defconst ygg-lsp--jdt-name-operations
  '(file-name-nondirectory file-remote-p file-name-case-insensitive-p)
  "Operations that yield no path, so need no source; the rest hand back the cached file.")

(defun ygg-lsp--jdt-source-file (uri)
  (string-match ygg-lsp--jdt-class-uri-re uri)
  (expand-file-name
   (format "sources/%s/%s.java" (substring (md5 uri) 0 12) (match-string 1 uri))
   ygg-lsp--jdtls-dir))

(defun ygg-lsp--jdt-file-handler (operation &rest args)
  "Run OPERATION on the source jdtls serves for the jdt URI in ARGS.
The source is cached under var/, so a visit lands on a real java file
that eglot then speaks about under its jdt URI.  A partial URI, such as
the common prefix of several results, is left to the default handlers."
  (if (not (and (stringp (car args))
                (string-match-p ygg-lsp--jdt-class-uri-re (car args))))
      (let ((inhibit-file-name-handlers
             (cons #'ygg-lsp--jdt-file-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args))
    (let* ((uri (car args))
           (file (ygg-lsp--jdt-source-file uri)))
      (puthash file uri ygg-lsp--jdt-uris)
      (unless (or (memq operation ygg-lsp--jdt-name-operations) (file-exists-p file))
        (let* ((server (or (ygg-lsp--jdtls-server t)
                           (error "No jdtls server to read %s" uri)))
               (text (jsonrpc-request server :java/classFileContents
                                      (list :uri uri))))
          (unless (stringp text) (error "jdtls has no source for %s" uri))
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert text))))
      (apply operation file (cdr args)))))

(defun ygg-lsp--jdt-source-uri (orig path &rest args)
  "The jdt URI a cached class source PATH came from, else ORIG's answer."
  (or (gethash path ygg-lsp--jdt-uris) (apply orig path args)))

(defun ygg-lsp--jdt-source-p ()
  (and buffer-file-name
       (string-prefix-p (expand-file-name "sources/" ygg-lsp--jdtls-dir)
                        (expand-file-name buffer-file-name))))

(defun ygg-lsp--jdt-source-setup ()
  "Make a cached class source read-only, joining the server that served it."
  (when (ygg-lsp--jdt-source-p)
    (setq-local eglot-extend-to-xref t)
    (setq buffer-read-only t)))

(defun ygg-lsp--not-jdt-source-p (&rest _)
  "Nil in a cached class source: its project is this config's repository."
  (not (ygg-lsp--jdt-source-p)))

(defun ygg-lsp-java-update-project ()
  "Have jdtls re-import this project from its build file."
  (interactive)
  (let* ((server (or (ygg-lsp--jdtls-server) (user-error "No jdtls server here")))
         (root (project-root (project-current t)))
         (build (seq-find #'file-exists-p
                          (mapcar (lambda (f) (expand-file-name f root))
                                  '("pom.xml" "build.gradle" "build.gradle.kts")))))
    (jsonrpc-notify server :java/projectConfigurationUpdate
                    (list :uri (eglot-path-to-uri (or build buffer-file-name))))))

(defun ygg-lsp--add-jdt-handler ()
  (add-to-list 'file-name-handler-alist '("\\`jdt://" . ygg-lsp--jdt-file-handler)))

(ygg-lsp--add-jdt-handler)
;; early-init empties the handler list until startup, then restores its own copy
(add-hook 'emacs-startup-hook #'ygg-lsp--add-jdt-handler 90)
(advice-add 'eglot-ensure :before-while #'ygg-lsp--not-jdt-source-p)
(with-eval-after-load 'yggdrasil-localleader
  (dolist (mode '(java-mode java-ts-mode))
    (yggdrasil-localleader-def mode "m u" #'ygg-lsp-java-update-project
                               "re-import build file")))

;;; 4. Eglot — fully async posture: never block on connect, no event log,
;;; batch didChange on idle, and no server chatter the mode line redraws for

(defcustom ygg-lsp-shutdown-delay 3
  "Seconds to wait after a server's last buffer closes before shutting it down.
Zero or nil shuts down immediately, as eglot does."
  :type '(choice (const :tag "Immediately" nil) number)
  :group 'eglot)

(defvar ygg-lsp--in-managed-mode nil)
(defvar ygg-lsp--shutdown-timers (make-hash-table :test #'eq)
  "Server to its pending deferred-shutdown timer.")

(defvar eglot--managed-mode)
(declare-function eglot--managed-buffers "eglot")
(declare-function eglot-current-server "eglot")
(declare-function jsonrpc-running-p "jsonrpc")

(defun ygg-lsp--cancel-shutdown (server)
  (when-let* ((timer (gethash server ygg-lsp--shutdown-timers)))
    (cancel-timer timer)
    (remhash server ygg-lsp--shutdown-timers)))

(defun ygg-lsp--shutdown-if-idle (fn server args)
  (remhash server ygg-lsp--shutdown-timers)
  (when (and (null (eglot--managed-buffers server))
             (jsonrpc-running-p server))
    (apply fn server args)))

(defun ygg-lsp--managed-mode-a (fn &rest args)
  "Mark that `eglot-shutdown' is the autoshutdown; cancel a pending one on re-join."
  (prog1 (let ((ygg-lsp--in-managed-mode t))
           (apply fn args))
    (when-let* ((server (and (bound-and-true-p eglot--managed-mode)
                             (eglot-current-server))))
      (ygg-lsp--cancel-shutdown server))))

(defun ygg-lsp--defer-shutdown-a (fn server &rest args)
  (if (and ygg-lsp--in-managed-mode
           (numberp ygg-lsp-shutdown-delay)
           (> ygg-lsp-shutdown-delay 0))
      (progn
        (ygg-lsp--cancel-shutdown server)
        (puthash server
                 (run-at-time ygg-lsp-shutdown-delay nil
                              #'ygg-lsp--shutdown-if-idle fn server args)
                 ygg-lsp--shutdown-timers))
    (ygg-lsp--cancel-shutdown server)
    (apply fn server args)))

(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-events-buffer-config '(:size 0)
        eglot-send-changes-idle-time 0.3
        eglot-report-progress nil
        ;; these two only: inlay hints and highlights are on by choice here,
        ;; and formatting happens on save rather than mid-keystroke
        eglot-ignored-server-capabilities
        '(:semanticTokensProvider :documentOnTypeFormattingProvider))
  (setq-default eglot-workspace-configuration #'ygg-lsp-workspace-configuration)
  (advice-add 'eglot--managed-mode :around #'ygg-lsp--managed-mode-a)
  (advice-add 'eglot-shutdown :around #'ygg-lsp--defer-shutdown-a)
  (dolist (entry
           '(((elixir-ts-mode elixir-mode heex-ts-mode) . ("expert" "--stdio"))
             ((lua-mode lua-ts-mode) . ("lua-language-server"))
             ((dart-mode dart-ts-mode) . ygg-lsp-dart-contact)
             ((swift-mode swift-ts-mode) . ("sourcekit-lsp"))
             ((html-mode html-ts-mode) . ygg-lsp-html-contact)
             ((css-mode css-ts-mode) . ygg-lsp-css-contact)
             ((kotlin-mode kotlin-ts-mode) . ygg-lsp--kotlin-lsp-contact)
             ((java-mode java-ts-mode) . ygg-lsp--jdtls-contact)
             ((python-mode python-ts-mode) . ygg-lsp-python-contact)
             ;; yaml, schema and all: SchemaStore's compose entry matches both the
             ;; docker-compose*.y{a,}ml and the compose*.y{a,}ml names
             ((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio"))
             ;; harper: grammar/spell checker for prose (no other LSP owns these modes)
             ((markdown-mode gfm-mode) . ("harper-ls" "--stdio"))))
    (add-to-list 'eglot-server-programs entry))
  (advice-add 'eglot-path-to-uri :around #'ygg-lsp--jdt-source-uri)
  (cl-defmethod eglot-register-capability :around
    (server (_method (eql workspace/didChangeWatchedFiles)) _id &rest _)
    (unless (ygg-lsp--unwatched-server-p server)
      (cl-call-next-method)))
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (js-jsx-mode :language-id "javascriptreact")
                  (jsx-ts-mode :language-id "javascriptreact")
                  (typescript-mode :language-id "typescript")
                  (typescript-ts-mode :language-id "typescript")
                  (tsx-ts-mode :language-id "typescriptreact"))
                 . ygg-lsp-ts-contact)))

(defun ygg-lsp--hook-when (available hooks &optional label)
  "Have HOOKS start eglot once AVAILABLE, a binary name or predicate, holds in the buffer."
  (let ((fn (intern (format "ygg-lsp--ensure-when-%s" available)))
        (label (or label (and (stringp available) (ygg-lsp--binary-label available)))))
    (defalias fn
      (lambda ()
        (cond ((if (stringp available) (ygg-lsp--executable available) (funcall available))
               (eglot-ensure))
              (label (ygg-lsp--warn-once label)))))
    (dolist (h hooks) (add-hook h fn))))

(ygg-lsp--hook-when "expert" '(elixir-ts-mode-hook elixir-mode-hook heex-ts-mode-hook))
(ygg-lsp--hook-when #'ygg-lsp--python-server '(python-mode-hook python-ts-mode-hook)
                    "Python (basedpyright/pyright/uvx) server")
(ygg-lsp--hook-when "yaml-language-server" '(yaml-mode-hook yaml-ts-mode-hook))
(ygg-lsp--hook-when "harper-ls" '(markdown-mode-hook gfm-mode-hook))
(ygg-lsp--hook-when "rust-analyzer" '(rust-mode-hook rust-ts-mode-hook))
(ygg-lsp--hook-when "gopls" '(go-mode-hook go-ts-mode-hook))
(ygg-lsp--hook-when "lua-language-server" '(lua-mode-hook lua-ts-mode-hook))
(ygg-lsp--hook-when "dart" '(dart-mode-hook dart-ts-mode-hook))
(ygg-lsp--hook-when "sourcekit-lsp" '(swift-mode-hook swift-ts-mode-hook))
(ygg-lsp--hook-when "clangd" '(c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook))

(defun ygg-lsp--html-available-p ()
  "Whether HTML has a server beyond eglot's default, and one for this buffer."
  (and (or (ygg-lsp--executable "ngserver") (ygg-lsp--executable "emmet-language-server"))
       (or (ygg-lsp--executable "vscode-html-language-server")
           (ygg-lsp--executable "emmet-language-server")
           (ygg-lsp--angular-root))))

(ygg-lsp--hook-when #'ygg-lsp--html-available-p '(html-mode-hook html-ts-mode-hook)
                    "Emmet server (emmet-language-server)")
(ygg-lsp--hook-when #'ygg-lsp--emmet-css-p '(css-mode-hook css-ts-mode-hook))
(ygg-lsp--hook-when #'ygg-lsp--kotlin-lsp '(kotlin-mode-hook kotlin-ts-mode-hook)
                    "Kotlin (kotlin-lsp) server")
(ygg-lsp--hook-when #'ygg-lsp--jdtls '(java-mode-hook java-ts-mode-hook)
                    "Java (jdtls) server")
(dolist (h '(java-mode-hook java-ts-mode-hook))
  (add-hook h #'ygg-lsp--jdt-source-setup -50))
(ygg-lsp--hook-when #'ygg-lsp-ts-command
                    '(typescript-ts-mode-hook tsx-ts-mode-hook
                      js-mode-hook js-ts-mode-hook js-jsx-mode-hook jsx-ts-mode-hook)
                    "TypeScript/JavaScript (typescript-language-server/vtsls/bunx) server")

(defun ygg-lsp--ts-source-locations (server)
  "SERVER's source definitions of the symbol at point, as LSP locations."
  (let* ((params (eglot--TextDocumentPositionParams))
         (arguments (vector (plist-get (plist-get params :textDocument) :uri)
                            (plist-get params :position)))
         (command (lambda (name)
                    (jsonrpc-request server :workspace/executeCommand
                                     (list :command name :arguments arguments)
                                     :timeout 60))))
    (pcase (ygg-lsp-ts-server-kind server)
      ('tsls (funcall command "_typescript.goToSourceDefinition"))
      ('vtsls (funcall command "typescript.goToSourceDefinition"))
      ('tsgo (jsonrpc-request server :custom/textDocument/sourceDefinition params
                              :timeout 60))
      (_ (user-error "No TypeScript server here")))))

(defvar eglot--temp-location-buffers)
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot--xref-make-match "eglot" (name uri range))
(declare-function xref-push-marker-stack "xref" (&optional m))
(declare-function xref-show-definitions-completing-read "xref" (fetcher alist))

(defun ygg-lsp--xref-matches (name locations)
  "Xrefs named NAME for LSP LOCATIONS, Location and LocationLink alike."
  (unwind-protect
      (delq nil
            (mapcar (lambda (location)
                      (let ((uri (or (plist-get location :targetUri)
                                     (plist-get location :uri)))
                            (range (or (plist-get location :targetSelectionRange)
                                       (plist-get location :range))))
                        (and uri range (eglot--xref-make-match name uri range))))
                    (if (vectorp locations) locations
                      (and locations (list locations)))))
    (maphash (lambda (_uri buffer) (kill-buffer buffer)) eglot--temp-location-buffers)
    (clrhash eglot--temp-location-buffers)))

(defun ygg-lsp-ts-source-definition ()
  "Go to the JavaScript that implements the symbol at point, past its types."
  (interactive)
  (let* ((locations (ygg-lsp--ts-source-locations (eglot--current-server-or-lose)))
         (name (or (thing-at-point 'symbol t) ""))
         (xrefs (ygg-lsp--xref-matches name locations)))
    (unless xrefs (user-error "No source definition for %s" name))
    (xref-push-marker-stack)
    (xref-show-definitions-completing-read (lambda () xrefs) nil)))

(with-eval-after-load 'yggdrasil-localleader
  (dolist (mode '(js-mode js-ts-mode typescript-ts-mode tsx-ts-mode))
    (yggdrasil-localleader-def mode "m s" #'ygg-lsp-ts-source-definition
                               "source definition")))

;; inlay hints on by default under a managing server (Zed shows them always)
(add-hook 'eglot-managed-mode-hook
          (lambda () (when (and (fboundp 'eglot-inlay-hints-mode) (eglot-managed-p))
                       (eglot-inlay-hints-mode 1))))

;; buffers that miss the mode-hook moment — easysession restores at
;; boot (no server running yet to adopt into), consult previews kept on
;; selection (mode hooks were delayed) — get their LSP when displayed:
;; adopt into a live server outright, else arm `eglot-ensure'
(declare-function eglot-current-server "eglot")
(declare-function eglot--maybe-activate-editing-mode "eglot")
(defvar eglot--managed-mode)

(defun ygg-lsp--wants-lsp-p ()
  (let ((hv (intern-soft (concat (symbol-name major-mode) "-hook"))))
    (and hv (boundp hv) (memq 'eglot-ensure (symbol-value hv)))))

(defun ygg-lsp--ensure-displayed (_frame)
  (dolist (win (window-list nil 'no-minibuf))
    (with-current-buffer (window-buffer win)
      (when (and buffer-file-name
                 (not (bound-and-true-p eglot--managed-mode))
                 (ygg-lsp--wants-lsp-p))
        (if (and (fboundp 'eglot-current-server) (eglot-current-server))
            (eglot--maybe-activate-editing-mode)
          (eglot-ensure))))))

(add-hook 'window-buffer-change-functions #'ygg-lsp--ensure-displayed)

;;; 3. Function motions — ] f / [ f, with ] F / [ F taking over file-siblings

(defun ygg-lsp--defun-edge (n)
  "Position of the start of the Nth next (N>0) or previous (N<0) defun."
  (save-excursion
    (let ((start (point)))
      (if (treesit-parser-list)
          (treesit-beginning-of-defun (- n))
        (beginning-of-defun (- n)))
      (and (/= (point) start) (point)))))

(defun ygg-next-defun ()
  (interactive)
  (ygg--record-bracket-motion 1 "f")
  (ygg--bracketed-goto (lambda () (ygg-lsp--defun-edge 1))))

(defun ygg-prev-defun ()
  (interactive)
  (ygg--record-bracket-motion -1 "f")
  (ygg--bracketed-goto (lambda () (ygg-lsp--defun-edge -1))))

(declare-function treesit-search-forward "treesit")
(declare-function treesit-node-text "treesit")

(defconst ygg-lsp--class-re "class\\|struct\\|impl\\|interface\\|trait\\|enum\\|module\\|protocol")
(defconst ygg-lsp--arg-re "\\`\\(parameter\\|argument\\)")
(defconst ygg-lsp--entry-re
  "\\`\\(field_declaration\\|enum_variant\\|pair\\|element\\|entry\\)")
(defconst ygg-lsp--test-re
  "\\_<\\(test\\|it\\|describe\\|deftest\\)\\_>\\|#\\[test\\]\\|@\\(pytest\\|test\\)")

(defun ygg-lsp--ts-edge (n type-re &optional text-re)
  "Start of the Nth next (N>0) / previous (N<0) node whose type matches
TYPE-RE (and whose text matches TEXT-RE when given)."
  (when (treesit-parser-list)
    (save-excursion
      (let* ((start (point))
             (backward (< n 0))
             (node (treesit-node-at (point)))
             (pred (lambda (nd)
                     (and (string-match-p type-re (treesit-node-type nd))
                          (or (null text-re)
                              (string-match-p text-re (treesit-node-text nd t))))))
             (found (and node (treesit-search-forward node pred backward))))
        (when (and found (/= (treesit-node-start found) start))
          (treesit-node-start found))))))

(defmacro ygg-lsp--def-ts-motion (name n re &optional text-re)
  `(defun ,name ()
     (interactive)
     (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge ,n ,re ,text-re)))))

(defun ygg-next-class ()
  (interactive)
  (ygg--record-bracket-motion 1 "C")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge 1 ygg-lsp--class-re))))
(defun ygg-prev-class ()
  (interactive)
  (ygg--record-bracket-motion -1 "C")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge -1 ygg-lsp--class-re))))
(defun ygg-next-arg ()
  (interactive)
  (ygg--record-bracket-motion 1 "a")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge 1 ygg-lsp--arg-re))))
(defun ygg-prev-arg ()
  (interactive)
  (ygg--record-bracket-motion -1 "a")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge -1 ygg-lsp--arg-re))))
(defun ygg-next-test ()
  (interactive)
  (ygg--record-bracket-motion 1 "T")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge 1 "function\\|call\\|declaration\\|definition" ygg-lsp--test-re))))
(defun ygg-prev-test ()
  (interactive)
  (ygg--record-bracket-motion -1 "T")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge -1 "function\\|call\\|declaration\\|definition" ygg-lsp--test-re))))
(defun ygg-next-loop ()
  (interactive)
  (ygg--record-bracket-motion 1 "l")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge 1 "for\\|while\\|do\\|repeat"))))
(defun ygg-prev-loop ()
  (interactive)
  (ygg--record-bracket-motion -1 "l")
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge -1 "for\\|while\\|do\\|repeat"))))

(defun ygg-next-entry ()
  "Move to the next entry (field, element, variant)."
  (interactive)
  (ygg--record-bracket-motion 1 "e")
  (unless (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (user-error "No entry textobject for this mode"))
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge 1 ygg-lsp--entry-re))))

(defun ygg-prev-entry ()
  "Move to the previous entry (field, element, variant)."
  (interactive)
  (ygg--record-bracket-motion -1 "e")
  (unless (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (user-error "No entry textobject for this mode"))
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge -1 ygg-lsp--entry-re))))

(defconst ygg-lsp--element-re "element\\|tag\\|component")

(defun ygg-next-xml-element ()
  "Move to the next XML/JSX element."
  (interactive)
  (ygg--record-bracket-motion 1 "X")
  (unless (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (user-error "No element textobject for this mode"))
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge 1 ygg-lsp--element-re))))

(defun ygg-prev-xml-element ()
  "Move to the previous XML/JSX element."
  (interactive)
  (ygg--record-bracket-motion -1 "X")
  (unless (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (user-error "No element textobject for this mode"))
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge -1 ygg-lsp--element-re))))

(yggdrasil-define-keys 'normal
  "] F" #'ygg-next-file :label "next file"
  "[ F" #'ygg-prev-file :label "prev file"
  "] f" #'ygg-next-defun :label "next function"
  "[ f" #'ygg-prev-defun :label "prev function"
  "] C" #'ygg-next-class :label "next class"
  "[ C" #'ygg-prev-class :label "prev class"
  "] a" #'ygg-next-arg :label "next argument"
  "[ a" #'ygg-prev-arg :label "prev argument"
  "] T" #'ygg-next-test :label "next test"
  "[ T" #'ygg-prev-test :label "prev test"
  "] e" #'ygg-next-entry :label "next entry"
  "[ e" #'ygg-prev-entry :label "prev entry"
  "] L" #'ygg-next-loop :label "next loop"
  "[ L" #'ygg-prev-loop :label "prev loop"
  "] X" #'ygg-next-xml-element :label "next element"
  "[ X" #'ygg-prev-xml-element :label "prev element")

;;; 6. Formatting — the = verb

(declare-function ygg-format-region "layer-format")
(defvar apheleia-mode)

(defun ygg-format--multiline-p (beg end)
  (and (< beg end)
       (save-excursion
         (goto-char beg)
         (search-forward "\n" (1- end) t))))

(defun ygg-format--whole-lines (beg end)
  (save-excursion
    (goto-char beg)
    (let ((from (line-beginning-position)))
      (goto-char end)
      (cons from (if (bolp) end (min (point-max) (line-beginning-position 2)))))))

(defun ygg-format ()
  "Format each selection: the save formatter on a multi-line one, else eglot, else `indent-region'.
A multi-line selection grows to whole lines.  A degenerate (zero-width)
selection formats the whole buffer instead, with the save formatter only
when it already runs on save here."
  (interactive)
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg end _dir)
       (let* ((whole (= beg end))
              (beg (if whole (point-min) beg))
              (end (if whole (point-max) end))
              (managed (and (fboundp 'eglot-managed-p) (eglot-managed-p)))
              (fallback (if managed "eglot" "indent-region"))
              (lines (cond (whole (and (bound-and-true-p apheleia-mode) (cons beg end)))
                           ((ygg-format--multiline-p beg end) (ygg-format--whole-lines beg end)))))
         (unless (and lines (fboundp 'ygg-format-region)
                      (ygg-format-region (car lines) (cdr lines) fallback))
           (if managed
               (if whole (eglot-format) (eglot-format beg end))
             (indent-region beg end))))))))

(yggdrasil-define-keys 'normal
  "=" #'ygg-format :label "format")

(defun ygg-toggle-inlay-hints ()
  "Toggle `eglot-inlay-hints-mode' in the current buffer."
  (interactive)
  (if (fboundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode 'toggle)
    (message "yggdrasil-lsp: eglot not loaded in this buffer")))

;;; 7. Diagnostics polish

;;; Leader: SPC c code submap

(defvar ygg-leader-code-map (make-sparse-keymap) "The c prefix: code actions.")

(autoload 'eglot-find-declaration "eglot" nil t)
(autoload 'eglot-find-implementation "eglot" nil t)
(autoload 'eglot-find-typeDefinition "eglot" nil t)
(declare-function eglot-format-buffer "eglot")
(declare-function apheleia--get-formatters "apheleia")
(declare-function apheleia-format-buffer "apheleia")
(declare-function flymake-diagnostics "flymake")

(declare-function xref-find-backend "xref")
(declare-function xref-backend-identifier-at-point "xref")
(declare-function xref-backend-references "xref")
(declare-function xref-item-location "xref")
(declare-function xref-item-summary "xref")
(declare-function xref-location-group "xref")
(declare-function xref-location-line "xref")

(defun ygg-references-qf ()
  "Show references in the *quickfix* multibuffer instead of an xref buffer."
  (interactive)
  (require 'xref)
  (let* ((backend (xref-find-backend))
         (id (or (xref-backend-identifier-at-point backend)
                 (user-error "No identifier at point")))
         (xrefs (or (xref-backend-references backend id)
                    (user-error "No references for %s" id)))
         (dir (or (when-let* ((p (project-current))) (project-root p))
                  default-directory))
         (lines (mapcar
                 (lambda (x)
                   (let ((loc (xref-item-location x)))
                     (format "%s:%d: %s"
                             (file-relative-name (xref-location-group loc) dir)
                             (or (xref-location-line loc) 1)
                             (string-trim (substring-no-properties
                                           (xref-item-summary x))))))
                 xrefs))
         (buf (ygg-qf-buffer-create)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (ygg-qf-reset)
        (setq default-directory dir)
        (insert ygg-qf--header)
        (insert (mapconcat #'identity lines "\n") "\n"))
      (grep-mode))
    (setq next-error-last-buffer buf)
    (select-window (display-buffer buf '((display-buffer-at-bottom))))))

(yggdrasil-define-keys 'normal
  "g D" #'eglot-find-declaration :label "declaration"
  "g i" #'eglot-find-implementation :label "implementation"
  "g y" #'eglot-find-typeDefinition :label "type definition"
  "g r" #'ygg-references-qf :label "references → quickfix")

(defun ygg-diagnostic-line ()
  "Show the full text of this line's diagnostics (nvim SPC e float)."
  (interactive)
  (let ((diags (and (bound-and-true-p flymake-mode)
                    (flymake-diagnostics (line-beginning-position)
                                         (line-end-position)))))
    (if (null diags)
        (message "no diagnostics on this line")
      (message "%s" (mapconcat #'flymake-diagnostic-text diags "\n")))))

(defun ygg-format-buffer ()
  "Format the buffer asynchronously: apheleia, else eglot, else indent."
  (interactive)
  (cond
   ((and (require 'apheleia nil t) (apheleia--get-formatters))
    (apheleia-format-buffer (apheleia--get-formatters)))
   ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format-buffer))
   (t (indent-region (point-min) (point-max)))))


(declare-function ygg-ast-grep-rewrite "layer-astgrep")

(defun ygg-lsp--ensure ()
  ;; eglot-* are not autoloaded — touch them only once eglot has loaded,
  ;; else a call in a non-managed buffer errors with a void-function
  (unless (and (featurep 'eglot) (eglot-managed-p))
    (user-error "No LSP server for this buffer")))

(defun ygg-lsp-code-actions ()
  "Code actions at point or over the region."
  (interactive)
  (ygg-lsp--ensure)
  (call-interactively #'eglot-code-actions))

(defun ygg-lsp-rename ()
  "Rename the symbol at point via LSP."
  (interactive)
  (ygg-lsp--ensure)
  (call-interactively #'eglot-rename))

(defun ygg-lsp-reconnect ()
  "Reconnect this buffer's LSP server."
  (interactive)
  (ygg-lsp--ensure)
  (call-interactively #'eglot-reconnect))

(defun ygg-lsp-type-definition ()
  "Jump to the type definition of the symbol at point."
  (interactive)
  (ygg-lsp--ensure)
  (call-interactively #'eglot-find-typeDefinition))

(defun ygg-lsp-organize-imports ()
  "Apply the LSP `source.organizeImports' code action to the buffer."
  (interactive)
  (ygg-lsp--ensure)
  (eglot-code-actions (point-min) (point-max) "source.organizeImports" t))

(defun ygg-lsp-fix-all ()
  "Apply the LSP `source.fixAll' code action (auto-fixable diagnostics)."
  (interactive)
  (ygg-lsp--ensure)
  (eglot-code-actions (point-min) (point-max) "source.fixAll" t))

(defun ygg-toggle-line-comment ()
  "Toggle line comment on each selection."
  (interactive)
  (ygg-do-selections
   (lambda (beg end _dir)
     (if (fboundp 'comment-region)
         (comment-region beg end nil)
       (user-error "comment-region not available")))))

(defun ygg-toggle-block-comment ()
  "Toggle block comment on each selection."
  (interactive)
  (ygg-do-selections
   (lambda (beg end _dir)
     (let ((comment-style (if (eq comment-style 'multi-line) 'indent 'multi-line)))
       (if (fboundp 'comment-region)
           (comment-region beg end nil)
         (user-error "comment-region not available"))))))

(defun ygg-multi-cursor-references ()
  "Create one cursor per reference of the symbol at point."
  (interactive)
  (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
      (when-let* ((syms (eglot-findHierarchy-prepared)))
        (let ((refs (save-excursion
                      (xref-find-references (car (car syms))))))
          (dolist (ref (xref-alist-to-xref-item-list refs))
            (ygg-add-cursor
             (xref-item-location ref)))))
    (user-error "Symbol references require LSP")))

;; SPC c = coder: LSP actions/refactors as flat sub-suffixes, with the
;; combobulate structural edits nested under SPC c e
(defvar ygg-leader-code-structural-map (make-sparse-keymap)
  "The c e prefix: combobulate structural edits.")

(yggdrasil-define-keys 'ygg-leader-code-structural-map
  "m" #'combobulate-mark-node-dwim :label "mark node"
  "c" #'combobulate-clone-node-dwim :label "clone node"
  "k" #'combobulate-kill-node-dwim :label "kill node"
  "p" #'combobulate-splice-up :label "splice up"
  "t" #'combobulate-transpose-sexps :label "transpose"
  "e" #'combobulate-cursor-edit-node-type-dwim :label "edit same-type")

(yggdrasil-define-keys 'ygg-leader-code-map
  "a" #'ygg-lsp-code-actions :label "code actions"
  "c" #'ygg-toggle-line-comment :label "toggle line comment"
  "C" #'ygg-toggle-block-comment :label "toggle block comment"
  "H" #'ygg-multi-cursor-references :label "cursors on references"
  "r" #'ygg-lsp-rename :label "rename"
  "o" #'ygg-lsp-organize-imports :label "organize imports"
  "x" #'ygg-lsp-fix-all :label "fix all"
  "f" #'ygg-format-buffer :label "format buffer"
  "F" #'ygg-format :label "format selection"
  "R" #'ygg-ast-grep-rewrite :label "structural replace"
  "i" #'ygg-toggle-inlay-hints :label "inlay hints"
  "n" #'ygg-lsp-reconnect :label "reconnect"
  "S" #'consult-eglot-symbols :label "workspace symbols"
  "t" #'ygg-lsp-type-definition :label "type definition"
  "E" #'ygg-diagnostic-line :label "line diagnostics"
  "D" #'flymake-show-buffer-diagnostics :label "diagnostics list"
  "h" #'ygg-code-hydra/body :label "actions + structural (hydra)"
  "e" ygg-leader-code-structural-map :label "structural edit")

(declare-function ygg-code-hydra/body "layer-lsp")

(yggdrasil-leader-def "c" ygg-leader-code-map "coder")

;;; SPC c h — the same actions + chainable combobulate structural edits in
;;; one hydra (structural heads stay open to chain; the rest exit).  Heads
;;; call the ygg-lsp-* wrappers so an unmanaged buffer errors cleanly rather
;;; than hitting a not-yet-autoloaded eglot symbol.
(when (fboundp 'elpaca)
  (elpaca hydra
    (require 'hydra)
    ;; defhydra is a macro; eval at run time so it isn't baked as a bare call
    ;; when this uncompiled layer loads (elpaca has hydra ready in this body)
    (eval
     '(defhydra ygg-code-hydra (:hint nil :color red)
        "
 ^Actions / refactor^              ^Structural edit (repeat)^
 _a_ code actions   _o_ organize    _v_ mark      _c_ clone
 _r_ rename         _x_ fix all     _k_ kill      _p_ splice
 _S_ struct replace _f_ format      _t_ transpose _e_ edit same-type
 ^ ^                ^ ^             _J_ drag down  _K_ drag up   _q_ quit
"
        ("a" ygg-lsp-code-actions :exit t)
        ("r" ygg-lsp-rename :exit t)
        ("o" ygg-lsp-organize-imports :exit t)
        ("x" ygg-lsp-fix-all :exit t)
        ("S" ygg-ast-grep-rewrite :exit t)
        ("f" ygg-format-buffer :exit t)
        ("v" combobulate-mark-node-dwim)
        ("c" combobulate-clone-node-dwim)
        ("k" combobulate-kill-node-dwim)
        ("p" combobulate-splice-up)
        ("t" combobulate-transpose-sexps)
        ("e" combobulate-cursor-edit-node-type-dwim :exit t)
        ("J" combobulate-drag-down)
        ("K" combobulate-drag-up)
        ("q" nil))
     t)))

;; combobulate: drag a node like vim moves a line (rest under SPC c e)
(yggdrasil-define-keys 'normal
  "] n" #'combobulate-drag-down :label "drag node down"
  "[ n" #'combobulate-drag-up :label "drag node up")

;;; Cape in eglot buffers — eglot clobbers the buffer-local capfs

(declare-function cape-capf-super "cape")
(declare-function jsonrpc-running-p "jsonrpc")

(defvar-local ygg-lsp--capfs-before-eglot nil)

(defun ygg-lsp--eglot-capf-while-live ()
  (let ((server (eglot-current-server)))
    (when (and server (jsonrpc-running-p server))
      (eglot-completion-at-point))))

(defun ygg-lsp--wire-cape ()
  ;; cape-capf-super MERGES its members, so dabbrev (a cross-buffer scan) would
  ;; run on every corfu-auto keystroke; keep it an ordered FALLBACK instead —
  ;; it fires only when eglot+file return nothing.
  (cond
   ((not (fboundp 'cape-capf-super)))
   ((bound-and-true-p eglot--managed-mode)
    (unless ygg-lsp--capfs-before-eglot
      (setq ygg-lsp--capfs-before-eglot
            (remq #'eglot-completion-at-point completion-at-point-functions)))
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'ygg-lsp--eglot-capf-while-live #'cape-file)
                      #'cape-dabbrev)))
   (ygg-lsp--capfs-before-eglot
    (setq-local completion-at-point-functions ygg-lsp--capfs-before-eglot)
    (setq ygg-lsp--capfs-before-eglot nil))))

(add-hook 'eglot-managed-mode-hook #'ygg-lsp--wire-cape)

(with-eval-after-load 'eglot
  ;; buster stays: LSP isIncomplete lists must be re-queried. noninterruptible
  ;; removed — a new keystroke should abort an in-flight completion, not block on it.
  (when (fboundp 'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

;;; Hover diagnostics — one line, top-right, on idle

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake-diagnostics "flymake")

(defvar ygg-diag-hover--timer nil)
(defvar ygg-diag-hover--visible nil)

(declare-function ygg--posframe-size "init")

;; reported width can be stale and excludes fringes/border — measure
;; the buffer content (init.el helper) instead of trusting the plist
(defun ygg-diag-hover--posframe-pos (info)
  (let ((w (if (fboundp 'ygg--posframe-size)
               (car (ygg--posframe-size info))
             (+ (plist-get info :posframe-width) 32))))
    (cons (max 0 (- (plist-get info :parent-frame-width) w 16))
          8)))

(defun ygg-diag-hover--hide ()
  (when ygg-diag-hover--visible
    (setq ygg-diag-hover--visible nil)
    (when (and (fboundp 'posframe-hide) (display-graphic-p))
      (posframe-hide " *ygg-diag-hover*"))))

(defun ygg-diag-hover--render (diag)
  (let* ((text (flymake-diagnostic-text diag))
         (line (truncate-string-to-width
                (car (split-string text "\n")) 70 nil nil "…"))
         (face (pcase (flymake-diagnostic-type diag)
                 ((or :error 'eglot-error) 'error)
                 ((or :warning 'eglot-warning) 'warning)
                 (_ 'success))))
    (if (and (display-graphic-p) (require 'posframe nil t))
        (progn
          (posframe-show " *ygg-diag-hover*"
                         :string (propertize line 'face face)
                         :poshandler #'ygg-diag-hover--posframe-pos
                         :border-width 0
                         :left-fringe 6 :right-fringe 6)
          (setq ygg-diag-hover--visible t))
      (message "%s" line))))

(defun ygg-diag-hover--check ()
  (let ((diags (and (bound-and-true-p flymake-mode)
                    (flymake-diagnostics (point)))))
    (if diags
        (ygg-diag-hover--render (car diags))
      (ygg-diag-hover--hide))))

(defun ygg-diag-hover--post-command ()
  (when ygg-diag-hover--timer (cancel-timer ygg-diag-hover--timer))
  (setq ygg-diag-hover--timer
        (run-with-idle-timer 0.3 nil #'ygg-diag-hover--check)))

(define-minor-mode ygg-diag-hover-mode
  "Show the diagnostic at point as one line in the frame's top-right."
  :init-value nil
  (if ygg-diag-hover-mode
      (add-hook 'post-command-hook #'ygg-diag-hover--post-command nil t)
    (remove-hook 'post-command-hook #'ygg-diag-hover--post-command t)
    (ygg-diag-hover--hide)))

(add-hook 'flymake-mode-hook (lambda () (ygg-diag-hover-mode 1)))

(yggdrasil-define-keys 'ygg-leader-code-map
  "d" #'ygg-diag-hover-mode :label "hover diagnostics")

(autoload 'ygg-lsp-call-hierarchy "ygg-lsp-calls" nil t)
(autoload 'ygg-lsp-type-hierarchy "ygg-lsp-calls" nil t)
(autoload 'ygg-lsp-calls-qf "ygg-lsp-calls" nil t)
(autoload 'ygg-call-graph "ygg-call-graph" nil t)

(yggdrasil-define-keys 'ygg-leader-code-map
  "I" #'ygg-lsp-call-hierarchy :label "call hierarchy"
  "Y" #'ygg-lsp-type-hierarchy :label "type hierarchy"
  "G" #'ygg-call-graph :label "call graph")

;;; Symbols: snacks-style pickers + multibuffer dump

(declare-function consult-eglot-symbols "consult-eglot")

(when (fboundp 'elpaca)
  (elpaca consult-eglot))


(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")
(defvar imenu-auto-rescan)

(defun ygg--imenu-flat ()
  "Flatten the buffer's imenu (eglot-fed under LSP) to (POS . NAME)."
  (require 'imenu)
  (let ((imenu-auto-rescan t)
        lines)
    (letrec ((walk (lambda (items prefix)
                     (dolist (it items)
                       (cond
                        ((imenu--subalist-p it)
                         (funcall walk (cdr it) (concat prefix (car it) "/")))
                        ((consp it)
                         (let ((pos (cdr it)))
                           (when (consp pos) (setq pos (car pos)))
                           (when (markerp pos) (setq pos (marker-position pos)))
                           (when (and (numberp pos) (> pos 0))
                             (push (cons pos (concat prefix (car it))) lines)))))))))
      (funcall walk (imenu--make-index-alist t) ""))
    (sort lines #'car-less-than-car)))

(defun ygg-symbols-qf ()
  "Dump the buffer's symbol outline into the *quickfix* multibuffer."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Buffer visits no file")))
         (syms (or (ygg--imenu-flat) (user-error "No symbols here")))
         (dir default-directory)
         (buf (ygg-qf-buffer-create))
         (lines (mapcar
                 (lambda (s)
                   (save-excursion
                     (goto-char (car s))
                     (format "%s:%d:%d: %s"
                             (file-relative-name file dir)
                             (line-number-at-pos)
                             (1+ (current-column))
                             (cdr s))))
                 syms)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (ygg-qf-reset)
        (setq default-directory dir)
        (insert ygg-qf--header)
        (insert (mapconcat #'identity lines "\n") "\n"))
      (grep-mode))
    (setq next-error-last-buffer buf)
    (select-window (display-buffer buf '((display-buffer-at-bottom))))))

(yggdrasil-define-keys 'ygg-leader-quit-map
  "y" #'ygg-symbols-qf :label "symbols → quickfix"
  "h" #'ygg-lsp-calls-qf :label "calls → quickfix")

(provide 'layer-lsp)
;;; layer-lsp.el ends here
