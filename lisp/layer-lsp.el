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
(autoload 'ygg-cm-show "ygg-context-manager" nil t)
(autoload 'ygg-cm-show-buffer "ygg-context-manager" nil t)
(autoload 'ygg-cm-text "ygg-context-manager")
(autoload 'ygg-cm-files-for "ygg-context-manager")
(autoload 'ygg-cm-rank-async "ygg-context-manager")
(autoload 'ygg-ex--cmd-context "ygg-context-manager")
(autoload 'ygg-ex--cmd-repomap "ygg-context-manager")
(with-eval-after-load 'yggdrasil-ex
  (defvar ygg-ex--commands)
  (unless (assoc "context" ygg-ex--commands)
    (push (cons "context" #'ygg-ex--cmd-context) ygg-ex--commands))
  (unless (assoc "repomap" ygg-ex--commands)
    (push (cons "repomap" #'ygg-ex--cmd-repomap) ygg-ex--commands)))

(declare-function ygg-qf-buffer-create "layer-quickfix" (&optional list))
(declare-function treesit-auto-add-to-auto-mode-alist "treesit-auto")
(declare-function global-treesit-auto-mode "treesit-auto")
(defvar treesit-auto-install)

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

(defvar ygg-lsp-unwatched-servers '("harper-ls")
  "Servers whose file-watch requests are declined.
A watch is one kernel descriptor per directory on macOS, and a prose
checker asking for a whole repository costs thousands of them for a
dictionary it could reread on demand.")

(defun ygg-lsp--server-binary (server)
  "Name of the binary SERVER runs, without its directory."
  (when-let* ((proc (ignore-errors (jsonrpc--process server)))
              (cmd (car-safe (process-command proc))))
    (file-name-nondirectory cmd)))

(defun ygg-lsp--unwatched-server-p (server)
  "Whether SERVER is one whose file watches are declined."
  (member (ygg-lsp--server-binary server) ygg-lsp-unwatched-servers))

(defun ygg-lsp-workspace-configuration (server)
  "Settings answered to SERVER, an empty object for harper-ls.
Harper aborts while logging that a null settings value is not an
object, so it is handed an object with nothing in it; every other
server keeps the nil it was already sent."
  (when (equal (ygg-lsp--server-binary server) "harper-ls")
    (list :harper-ls (make-hash-table :test 'equal))))

(defconst ygg-lsp--agent-dir-re "/\\.aob/"
  "Directory the harness works in, at any depth, holding no editing session.")

(defconst ygg-lsp--harness-prose-re "/\\(?:\\.aob\\|openspec\\)/"
  "Directories whose prose is written by the harness, not read by a person.")

(defun ygg-lsp-may-join-server-p (&rest _)
  "Whether this buffer may join a language server at all.
A terminal frame is the agent workflow, where a server answers nobody
and costs a process per buffer.  A buffer with no file, and one whose
unmodified text belongs to a path that was moved away, has nothing a
server can open.  The harness scratch tree joins nothing, and prose the
harness wrote gets no grammar checker, while markdown the owner opens
by hand in a window keeps the one it had."
  (let ((file (and buffer-file-name (expand-file-name buffer-file-name))))
    (and (display-graphic-p)
         file
         (or (file-exists-p file) (buffer-modified-p) (zerop (buffer-size)))
         (not (string-match-p ygg-lsp--agent-dir-re file))
         (not (and (memq major-mode '(markdown-mode gfm-mode))
                   (or (string-prefix-p "*" (buffer-name))
                       (string-match-p ygg-lsp--harness-prose-re file)))))))

(advice-add 'eglot-ensure :before-while #'ygg-lsp-may-join-server-p)
(advice-add 'eglot--maybe-activate-editing-mode
            :before-while #'ygg-lsp-may-join-server-p)
(defvar eglot-report-progress)
(defvar eglot-ignored-server-capabilities)

(declare-function flymake-show-buffer-diagnostics "flymake")
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
  "Re-detect modes for file buffers stuck in `fundamental-mode'.
Session restore can run before deferred mode registration — treesit-auto
below, or elpaca-provided modes like gfm-mode — leaving e.g. .ts or .md
buffers modeless.  Runs both after session load and after elpaca finishes,
so whichever completes last re-modes the stragglers."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and buffer-file-name (eq major-mode 'fundamental-mode))
        (ignore-errors (normal-mode))))))

(add-hook 'easysession-after-load-hook #'ygg-lsp--remode-fundamentals)
(add-hook 'elpaca-after-init-hook #'ygg-lsp--remode-fundamentals)

(when (fboundp 'elpaca)
  (elpaca treesit-auto
    ;; register synchronously: an idle-timer deferral here loses the
    ;; race against session restore and strands every restored ts-mode
    ;; buffer (any language) in fundamental-mode
    (require 'treesit-auto)
    ;; a missing grammar leaves the ts-mode body erroring before its
    ;; hooks run (half-dead buffer) — just build it on first contact
    (setq treesit-auto-install t)
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)
    ;; anything restored before this point sat modeless — heal it now
    (ygg-lsp--remode-fundamentals)))

;; treesit-auto rebuilds major-mode-remap-alist on every set-auto-mode-0,
;; re-probing ~60 grammars (dlopen) each time — 13s to restore a 244-file
;; session.  Availability is fixed per session; memoize, clear on real install.
(defvar ygg-lsp--treesit-ready-cache (make-hash-table :test 'eq))

(defun ygg-lsp--treesit-ready-cached (orig lang &rest args)
  (let ((hit (gethash lang ygg-lsp--treesit-ready-cache 'miss)))
    (if (eq hit 'miss)
        (puthash lang (apply orig lang args) ygg-lsp--treesit-ready-cache)
      hit)))

(defun ygg-lsp--treesit-ready-cache-clear (&rest _)
  (clrhash ygg-lsp--treesit-ready-cache))

(when (fboundp 'treesit-ready-p)
  (advice-add 'treesit-ready-p :around #'ygg-lsp--treesit-ready-cached)
  (advice-add 'treesit-install-language-grammar :after
              #'ygg-lsp--treesit-ready-cache-clear))

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

;; Emacs ships no dart mode and treesit-auto has no dart-ts-mode to add, so
;; .dart would open in fundamental-mode and the dart eglot hook never fires.
(when (fboundp 'elpaca)
  (elpaca dart-mode
    (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))))

;; Emacs ships no swift mode; without it .swift opens in fundamental-mode and
;; the sourcekit-lsp registration + hook below never fire.
(when (fboundp 'elpaca)
  (elpaca swift-mode
    (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))))

;; konrad1977/swift-development: build/run/test/debug iOS & macOS apps on the
;; simulator or a device, SwiftUI hot-reload previews, a test explorer.  Its
;; many (require ... nil t) are sibling files in the same repo (elpaca clones
;; them all); spinner is the optional progress-bar dep.  The package self-hooks
;; swift-development-mode-enable onto swift-mode-hook, so C-c s (its transient)
;; is live in swift buffers with no extra wiring.
(when (fboundp 'elpaca)
  (elpaca spinner)
  (elpaca (swift-development :host github :repo "konrad1977/swift-development")))

;; dockerfile-ts-mode ships in Emacs 30, but treesit-auto maps no filename to
;; it (Dockerfile has no extension) and the grammar is unbuilt — wire both.
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

;;; Server roster (ported from nvim core/lsp.lua): register + hook only
;;; when the binary exists, so no buffer ever waits on a doomed connect

(defconst ygg-lsp--server-binaries
  '(("expert" . "Elixir (expert)")
    ("lua-language-server" . "Lua")
    ("vtsls" . "TypeScript/JavaScript (vtsls)")
    ("rust-analyzer" . "Rust")
    ("gopls" . "Go")
    ("dart" . "Dart")
    ("sourcekit-lsp" . "Swift")
    ("kotlin-lsp" . "Kotlin")
    ("jdtls" . "Java (jdtls)")
    ("emmet-language-server" . "Emmet")
    ("astro-ls" . "Astro")
    ("ngserver" . "Angular"))
  "Server binaries probed with `ygg-lsp--executable'; missing ones only warn.")

(dolist (spec ygg-lsp--server-binaries)
  (unless (ygg-lsp--executable (car spec))
    (message "yggdrasil-lsp: %s server (%s) not runnable" (cdr spec) (car spec))))

;; uvx last, the way the TypeScript entry below keeps bunx last: it needs no
;; global install, so it leaves nothing on PATH to rot the next time the system
;; python moves out from under a launcher script.  Python stays out of the
;; roster above for the same reason only vtsls is listed there — with a fallback
;; chain the warning belongs to the chain, not to any one binary in it.
(defconst ygg-lsp--python-server
  (cond ((ygg-lsp--executable "basedpyright-langserver")
         '("basedpyright-langserver" "--stdio"))
        ((ygg-lsp--executable "pyright-langserver")
         '("pyright-langserver" "--stdio"))
        ((ygg-lsp--executable "uvx")
         '("uvx" "--from" "basedpyright" "basedpyright-langserver" "--stdio")))
  "Command eglot runs for Python, or nil when no server can be reached.")

(unless ygg-lsp--python-server
  (message "yggdrasil-lsp: Python (basedpyright/pyright/uvx) server not runnable"))

;;; 4. Eglot — fully async posture: never block on connect, no event log,
;;; batch didChange on idle, and no server chatter the mode line redraws for

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
  (when (ygg-lsp--executable "expert")
    (add-to-list 'eglot-server-programs
                 '((elixir-ts-mode elixir-mode heex-ts-mode) . ("expert" "--stdio"))))
  (when (ygg-lsp--executable "lua-language-server")
    (add-to-list 'eglot-server-programs
                 '((lua-mode lua-ts-mode) . ("lua-language-server"))))
  (when-let* ((dart (ygg-lsp--executable "dart")))
    ;; flutter installs pin their own dart sdk next to the binary
    (let ((flutter-dart (expand-file-name "cache/dart-sdk/bin/dart"
                                          (file-name-directory dart))))
      (add-to-list 'eglot-server-programs
                   `((dart-mode dart-ts-mode)
                     . (,(if (file-exists-p flutter-dart) flutter-dart dart)
                        "language-server" "--protocol=lsp")))))
  (when (ygg-lsp--executable "sourcekit-lsp")
    (add-to-list 'eglot-server-programs
                 '((swift-mode swift-ts-mode) . ("sourcekit-lsp"))))
  (when (ygg-lsp--executable "kotlin-lsp")
    (add-to-list 'eglot-server-programs
                 '((kotlin-mode kotlin-ts-mode) . ("kotlin-lsp" "--stdio"))))
  ;; jdtls launcher on PATH wins over eglot's built-in eclipse.jdt.ls contact
  (when (ygg-lsp--executable "jdtls")
    (add-to-list 'eglot-server-programs
                 '((java-mode java-ts-mode) . ("jdtls"))))
  (when ygg-lsp--python-server
    (add-to-list 'eglot-server-programs
                 (cons '(python-mode python-ts-mode) ygg-lsp--python-server)))
  ;; yaml, schema and all: SchemaStore's compose entry matches both the
  ;; docker-compose*.y{a,}ml and the compose*.y{a,}ml names
  (when (ygg-lsp--executable "yaml-language-server")
    (add-to-list 'eglot-server-programs
                 '((yaml-mode yaml-ts-mode)
                   . ("yaml-language-server" "--stdio"))))
  ;; harper: grammar/spell checker for prose (no other LSP owns these modes)
  (when (ygg-lsp--executable "harper-ls")
    (add-to-list 'eglot-server-programs
                 '((markdown-mode gfm-mode) . ("harper-ls" "--stdio"))))
  (cl-defmethod eglot-register-capability :around
    (server (_method (eql workspace/didChangeWatchedFiles)) _id &rest _)
    (unless (ygg-lsp--unwatched-server-p server)
      (cl-call-next-method)))
  (let* ((modes '((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (js-jsx-mode :language-id "javascriptreact")
                  (jsx-ts-mode :language-id "javascriptreact")
                  (typescript-mode :language-id "typescript")
                  (typescript-ts-mode :language-id "typescript")
                  (tsx-ts-mode :language-id "typescriptreact")))
         (cmd (cond ((ygg-lsp--executable "vtsls") '("vtsls" "--stdio"))
                    ((ygg-lsp--executable "typescript-language-server")
                     '("typescript-language-server" "--stdio"))
                    ((ygg-lsp--executable "bunx")
                     '("bunx" "@vtsls/language-server" "--stdio")))))
    (when cmd
      (add-to-list 'eglot-server-programs (cons modes cmd)))))

(defun ygg-lsp--hook-when (bin hooks)
  (when (ygg-lsp--executable bin)
    (dolist (h hooks) (add-hook h #'eglot-ensure))))

(ygg-lsp--hook-when "expert" '(elixir-ts-mode-hook elixir-mode-hook heex-ts-mode-hook))
(when ygg-lsp--python-server
  (dolist (h '(python-mode-hook python-ts-mode-hook)) (add-hook h #'eglot-ensure)))
(ygg-lsp--hook-when "yaml-language-server" '(yaml-mode-hook yaml-ts-mode-hook))
(ygg-lsp--hook-when "harper-ls" '(markdown-mode-hook gfm-mode-hook))
(ygg-lsp--hook-when "rust-analyzer" '(rust-mode-hook rust-ts-mode-hook))
(ygg-lsp--hook-when "gopls" '(go-mode-hook go-ts-mode-hook))
(ygg-lsp--hook-when "lua-language-server" '(lua-mode-hook lua-ts-mode-hook))
(ygg-lsp--hook-when "dart" '(dart-mode-hook dart-ts-mode-hook))
(ygg-lsp--hook-when "sourcekit-lsp" '(swift-mode-hook swift-ts-mode-hook))
(ygg-lsp--hook-when "kotlin-lsp" '(kotlin-mode-hook kotlin-ts-mode-hook))
(ygg-lsp--hook-when "jdtls" '(java-mode-hook java-ts-mode-hook))
(when (or (ygg-lsp--executable "vtsls")
          (ygg-lsp--executable "typescript-language-server")
          (ygg-lsp--executable "bunx"))
  (dolist (h '(typescript-ts-mode-hook tsx-ts-mode-hook
               js-mode-hook js-ts-mode-hook js-jsx-mode-hook jsx-ts-mode-hook))
    (add-hook h #'eglot-ensure)))

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

;;; 2. Treesit textobjects (m i f / m a f, m i t / m a t)

(defun ygg-lsp--defun-bounds ()
  "Bounds of the defun at point: treesit node when parsed, else thingatpt."
  (or (and (treesit-parser-list)
           (fboundp 'treesit-defun-at-point)
           (when-let* ((node (treesit-defun-at-point)))
             (cons (treesit-node-start node) (treesit-node-end node))))
      (bounds-of-thing-at-point 'defun)))

(defun ygg-lsp--type-bounds ()
  "Bounds of the nearest enclosing class/struct/impl/interface/module node."
  (when (treesit-parser-list)
    (let ((node (treesit-node-at (point))))
      (while (and node
                  (not (string-match-p "class\\|struct\\|impl\\|interface\\|module"
                                       (treesit-node-type node))))
        (setq node (treesit-node-parent node)))
      (and node (cons (treesit-node-start node) (treesit-node-end node))))))

(defun ygg-lsp--textobject-bounds (c which)
  "Resolve the `f' (function) and `t' (type) treesit textobjects.
Installed as :before-until advice on `ygg-match--textobject-bounds';
returning nil for any other char falls through to the original dispatcher.
v1: `mi' and `ma' return identical bounds for f/t (no whitespace trim)."
  (ignore which)
  (pcase c
    (?f (ygg-lsp--defun-bounds))
    (?t (ygg-lsp--type-bounds))))

(with-eval-after-load 'yggdrasil-match
  (advice-add 'ygg-match--textobject-bounds :before-until #'ygg-lsp--textobject-bounds))

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
  (ygg--bracketed-goto (lambda () (ygg-lsp--defun-edge 1))))

(defun ygg-prev-defun ()
  (interactive)
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

(ygg-lsp--def-ts-motion ygg-next-class 1 ygg-lsp--class-re)
(ygg-lsp--def-ts-motion ygg-prev-class -1 ygg-lsp--class-re)
(ygg-lsp--def-ts-motion ygg-next-arg 1 ygg-lsp--arg-re)
(ygg-lsp--def-ts-motion ygg-prev-arg -1 ygg-lsp--arg-re)
(ygg-lsp--def-ts-motion ygg-next-test 1 "function\\|call\\|declaration\\|definition" ygg-lsp--test-re)
(ygg-lsp--def-ts-motion ygg-prev-test -1 "function\\|call\\|declaration\\|definition" ygg-lsp--test-re)

(defun ygg-next-entry ()
  "Move to the next entry (field, element, variant)."
  (interactive)
  (unless (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (user-error "No entry textobject for this mode"))
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge 1 ygg-lsp--entry-re))))

(defun ygg-prev-entry ()
  "Move to the previous entry (field, element, variant)."
  (interactive)
  (unless (and (fboundp 'treesit-parser-list) (treesit-parser-list))
    (user-error "No entry textobject for this mode"))
  (ygg--bracketed-goto (lambda () (ygg-lsp--ts-edge -1 ygg-lsp--entry-re))))

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
  "[ e" #'ygg-prev-entry :label "prev entry")

;;; 6. Formatting — the = verb

(defun ygg-format ()
  "Format each selection: eglot when the buffer is managed, else `indent-region'.
A degenerate (zero-width) selection formats the whole buffer instead."
  (interactive)
  (ygg-with-verb
    (ygg-do-selections
     (lambda (beg end _dir)
       (let ((whole (= beg end))
             (managed (and (fboundp 'eglot-managed-p) (eglot-managed-p))))
         (if managed
             (if whole (eglot-format) (eglot-format beg end))
           (if whole (indent-region (point-min) (point-max)) (indent-region beg end))))))))

(yggdrasil-define-keys 'normal
  "=" #'ygg-format :label "format")

(defun ygg-toggle-inlay-hints ()
  "Toggle `eglot-inlay-hints-mode' in the current buffer."
  (interactive)
  (if (fboundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode 'toggle)
    (message "yggdrasil-lsp: eglot not loaded in this buffer")))

;;; 5. Leader — replace the LSP placeholder stubs, same keys

;; "s" is layer-completion.el's search submap (already carries imenu at "s i");
;; leave it alone rather than race its load order for the top-level key.
(yggdrasil-leader-def "d" #'consult-flymake "diagnostics")

;;; 7. Diagnostics polish

;;; Leader: SPC c code submap

(defvar ygg-leader-code-map (make-sparse-keymap) "The c prefix: code actions.")

(declare-function eglot-find-declaration "eglot")
(declare-function eglot-find-implementation "eglot")
(declare-function eglot-find-typeDefinition "eglot")
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
        (erase-buffer)
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

(defun ygg-lsp--wire-cape ()
  ;; cape-capf-super MERGES its members, so dabbrev (a cross-buffer scan) would
  ;; run on every corfu-auto keystroke; keep it an ordered FALLBACK instead —
  ;; it fires only when eglot+file return nothing.
  (when (fboundp 'cape-capf-super)
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'eglot-completion-at-point #'cape-file)
                      #'cape-dabbrev))))

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
        (erase-buffer)
        (setq default-directory dir)
        (insert ygg-qf--header)
        (insert (mapconcat #'identity lines "\n") "\n"))
      (grep-mode))
    (setq next-error-last-buffer buf)
    (select-window (display-buffer buf '((display-buffer-at-bottom))))))

(yggdrasil-define-keys 'ygg-leader-quit-map
  "y" #'ygg-symbols-qf :label "symbols → quickfix")

(provide 'layer-lsp)
;;; layer-lsp.el ends here
