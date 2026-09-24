;;; layer-dap.el --- Debug Adapter Protocol layer (dape) -*- lexical-binding: t; -*-

;; Built-in wrapped: none — dape is the eglot-ecosystem DAP client (not
;; dap-mode, which drags in lsp-mode). Fully deferred: dape.el itself
;; only loads on the first bound command below (autoload/require), same
;; posture as eglot in layer-lsp. Drawer UI ported from nvim-dap-view:
;; one shared bottom window that swaps buffers instead of the classic
;; dap-ui side-window sprawl.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-leader)
(require 'seq)
(require 'cl-lib)

(declare-function ygg-qf-buffer-create "layer-quickfix" (&optional list))
(declare-function ygg-selection-effective-bounds "yggdrasil-selection")
(declare-function consult-flymake "consult-flymake")

(declare-function dape "dape")
(declare-function dape-continue "dape")
(declare-function dape-next "dape")
(declare-function dape-step-in "dape")
(declare-function dape-step-out "dape")
(declare-function dape-restart "dape")
(declare-function dape-quit "dape")
(declare-function dape-breakpoint-toggle "dape")
(declare-function dape-breakpoint-expression "dape")
(declare-function dape-repl "dape")
(declare-function dape-evaluate-expression "dape")
(declare-function dape-breakpoint-save "dape")
(declare-function dape-breakpoint-load "dape")
(declare-function dape-breakpoint-global-mode "dape")
(declare-function dape-ensure-command "dape")
(declare-function dape--live-connection "dape")
(declare-function dape--live-connections "dape")
(declare-function dape--display-buffer "dape")
(declare-function dape--info-get-buffer-create "dape")
(declare-function dape--breakpoint-file-name "dape")
(declare-function dape--breakpoint-line "dape")
(declare-function dape--source-breakpoint-p "dape")
(declare-function ygg-list-vim-keys "yggdrasil-verbs")
(defvar dape-info-parent-mode-map)

(defvar dape-configs)
(defvar dape-buffer-window-arrangement)
(defvar dape-info-buffer-window-groups)
(defvar dape-inlay-hints)
(defvar dape--breakpoints)

(when (fboundp 'elpaca)
  (elpaca dape))

;; dape.el ships autoload cookies for only 2 symbols; every other command
;; bound directly below needs its own so the first keypress loads dape.el.
(autoload 'dape "dape" nil t)
(autoload 'dape-breakpoint-toggle "dape" nil t)
(autoload 'dape-breakpoint-expression "dape" nil t)
(autoload 'dape-step-in "dape" nil t)
(autoload 'dape-next "dape" nil t)
(autoload 'dape-step-out "dape" nil t)
(autoload 'dape-restart "dape" nil t)
(autoload 'dape-quit "dape" nil t)
(autoload 'dape-repl "dape" nil t)

;;; Adapter roster — warn, never fail

(defconst ygg-dape-adapters
  '((:name "node" :serves "Node (js-debug adapter via mise)")
    (:name "debug_adapter.sh" :serves "Elixir (ElixirLS debugger)")
    (:name "codelldb" :serves "Rust (codelldb)")
    (:name "dlv" :serves "Go (delve, ships with dape)")
    (:name "flutter" :serves "Flutter (ships with dape)")
    (:name "dart" :serves "Dart (dart debug_adapter)"))
  "Debug adapters this layer probes for on PATH.")

(defun ygg-dape--probe ()
  "Say which adapters are not on PATH."
  (dolist (tool ygg-dape-adapters)
    (unless (executable-find (plist-get tool :name))
      (message "yggdrasil-dap: %s adapter (%s) not found on PATH"
               (plist-get tool :serves) (plist-get tool :name)))))

(add-hook 'elpaca-after-init-hook #'ygg-dape--probe)

(with-eval-after-load 'dape
  ;; dlv/debugpy/js-debug ship in `dape-configs' (ygg-debugpy extends debugpy); only the two
  ;; adapters dape doesn't bundle need registering, and only when present.
  (when (executable-find "debug_adapter.sh")
    (add-to-list 'dape-configs
                 '(elixir-ls
                   modes (elixir-mode elixir-ts-mode)
                   command "debug_adapter.sh"
                   command-cwd dape-command-cwd
                   :type "mix_task"
                   :request "launch"
                   :task "test"
                   :taskArgs ["--trace"]
                   :projectDir dape-cwd
                   :requireFiles ["test/**/test_helper.exs" "test/**/*_test.exs"])))
  (when (executable-find "codelldb")
    (add-to-list 'dape-configs
                 '(codelldb
                   modes (rust-mode rust-ts-mode rustic-mode)
                   ensure dape-ensure-command
                   command "codelldb"
                   command-args ("--port" :autoport)
                   port :autoport
                   command-cwd dape-command-cwd
                   :type "lldb"
                   :request "launch"
                   :cwd "."
                   :program "target/debug/PROGRAM"))))

;;; Flutter/Dart + web tuning. Mutate the built-in entries' `modes' in place
;;; (not the whole literal) so a dape upgrade keeps its command-args/ensure.
(with-eval-after-load 'dape
  ;; Dart uses dart-mode (non-ts); flutter configuration supports both
  ;; dart-mode and dart-ts-mode for extensibility
  (when-let* ((cfg (assq 'flutter dape-configs)))
    (setf (plist-get (cdr cfg) 'modes) '(dart-mode dart-ts-mode))
    (when (fboundp 'ygg-device-dape-flutter) (ygg-device-dape-flutter cfg)))
  ;; React .tsx is `tsx-ts-mode', missing from the chrome config's modes
  (when-let* ((cfg (assq 'js-debug-chrome dape-configs))
              (m (plist-get (cdr cfg) 'modes)))
    (unless (memq 'tsx-ts-mode m)
      (setf (plist-get (cdr cfg) 'modes) (append m '(tsx-ts-mode)))))
  ;; pure Dart (CLI/tests): `dart debug_adapter' (args verified); flutter apps
  ;; keep their own config above
  (unless (assq 'dart dape-configs)
    (add-to-list 'dape-configs
                 '(dart
                   modes (dart-mode dart-ts-mode)
                   ensure dape-ensure-command
                   command "dart"
                   command-args ("debug_adapter")
                   command-cwd dape-command-cwd
                   :type "dart"
                   :cwd "."
                   :program "bin/main.dart"))))

;;; Drawer — nvim-dap-view ported to dape: one shared bottom window.
;; arrangement nil is dape's own documented escape hatch to defer fully
;; to `display-buffer-alist' — no built-in arrangement collapses to a
;; single window, so only that funneling is hand-rolled below. Collapsing
;; `dape-info-buffer-window-groups' to one group also makes dape's own
;; `dape-info-buffer-tab' (TAB / <backtab>) cycle every section in place.

(defun ygg-dape--drawer-buffer-p (buffer-name _action)
  "Match dape info/REPL/console buffers for the shared bottom drawer."
  (when-let* ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (derived-mode-p 'dape-info-parent-mode 'dape-repl-mode 'dape-shell-mode))))

(with-eval-after-load 'dape
  (setq dape-buffer-window-arrangement nil
        dape-inlay-hints t
        dape-info-buffer-window-groups
        '((dape-info-scope-mode dape-info-watch-mode dape-info-stack-mode
           dape-info-modules-mode dape-info-sources-mode
           dape-info-breakpoints-mode dape-info-threads-mode)))
  (add-to-list 'display-buffer-alist
               '(ygg-dape--drawer-buffer-p
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom) (slot . 0) (window-height . 0.3)
                 (dedicated . weakly)
                 (window-parameters . ((no-delete-other-windows . t)))))
  (ygg-list-vim-keys dape-info-parent-mode-map)
  ;; margin/fringe breakpoint glyphs, auto-chosen by `dape' per frame capability
  (dape-breakpoint-global-mode 1)
  (ignore-errors (dape-breakpoint-load)))

(add-hook 'kill-emacs-hook
          (lambda () (when (fboundp 'dape-breakpoint-save) (ignore-errors (dape-breakpoint-save)))))

(defun ygg-dape--drawer-window ()
  "Return the live window currently showing a drawer buffer, if any."
  (seq-find (lambda (w) (ygg-dape--drawer-buffer-p (buffer-name (window-buffer w)) nil))
            (window-list)))

(defun ygg-dape--show-info (mode)
  (require 'dape)
  (select-window (dape--display-buffer (dape--info-get-buffer-create mode))))

(defun ygg-dape-view-scope ()
  "Show the dape Scope section in the drawer."
  (interactive)
  (ygg-dape--show-info 'dape-info-scope-mode))

(defun ygg-dape-view-watch ()
  "Show the dape Watch section in the drawer."
  (interactive)
  (ygg-dape--show-info 'dape-info-watch-mode))

(defun ygg-dape-view-stack ()
  "Show the dape Stack section in the drawer."
  (interactive)
  (ygg-dape--show-info 'dape-info-stack-mode))

(defun ygg-dape-view-breakpoints ()
  "Show the dape Breakpoints section (includes exceptions) in the drawer."
  (interactive)
  (ygg-dape--show-info 'dape-info-breakpoints-mode))

(defun ygg-dape-view-threads ()
  "Show the dape Threads section in the drawer."
  (interactive)
  (ygg-dape--show-info 'dape-info-threads-mode))

(defun ygg-dape-view-console ()
  "Show the adapter's console/shell buffer in the drawer, if one exists."
  (interactive)
  (require 'dape)
  (if-let* ((buf (get-buffer "*dape-shell*")))
      (select-window (dape--display-buffer buf))
    (message "yggdrasil-dap: no console buffer yet (adapter hasn't opened a terminal)")))

(defun ygg-dape-drawer-toggle ()
  "Open or close the shared dape drawer window (nvim-dap-view open/close)."
  (interactive)
  (require 'dape)
  (if-let* ((win (ygg-dape--drawer-window)))
      (delete-window win)
    (ygg-dape-view-watch)))

;;; Commands needing more than a direct bind

(defun ygg-dape-continue ()
  "Continue the stopped session, or start one via `dape' if none is running."
  (interactive)
  (require 'dape)
  (if (dape--live-connections)
      (call-interactively #'dape-continue)
    (call-interactively #'dape)))

(defun ygg-dape-eval ()
  "Evaluate the active selection with dape; prompt when there is none."
  (interactive)
  (require 'dape)
  (pcase-let* ((`(,beg ,end ,_dir) (ygg-selection-effective-bounds))
               (expr (if (= beg end)
                         (read-string "Eval: ")
                       (buffer-substring-no-properties beg end))))
    (dape-evaluate-expression (or (dape--live-connection 'stopped t)
                                  (dape--live-connection 'last))
                              expr "repl")))

(defun ygg-dape--breakpoint-qf-line (bp)
  (when (dape--source-breakpoint-p bp)
    (when-let* ((file (dape--breakpoint-file-name bp))
                (line (dape--breakpoint-line bp)))
      (format "%s:%d: breakpoint" (file-relative-name file) line))))

(defun ygg-dape-breakpoints-qf ()
  "Dump source breakpoints into the *quickfix* multibuffer."
  (interactive)
  (require 'dape)
  (let ((lines (delq nil (mapcar #'ygg-dape--breakpoint-qf-line dape--breakpoints))))
    (unless lines (user-error "No breakpoints"))
    (let ((dir default-directory)
          (buf (ygg-qf-buffer-create)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq default-directory dir)
          (insert (mapconcat #'identity lines "\n") "\n"))
        (grep-mode))
      (setq next-error-last-buffer buf)
      (select-window (display-buffer buf '((display-buffer-at-bottom)))))))

;;; VS Code launch.json → dape.  dape has no native launch.json support,
;;; but DAP launch/attach request args ARE the launch.json config body, so
;;; parsing with :object-type 'plist yields a plist that merges straight
;;; onto a dape base adapter.  We map `type' → adapter, resolve ${...}
;;; vars, and bridge preLaunchTask → dape's `compile' key.

(declare-function project-current "project")
(declare-function project-root "project")

(defvar ygg-dape-vscode-type-adapters
  '(("node" js-debug-node) ("pwa-node" js-debug-node) ("node-terminal" js-debug-node)
    ("chrome" js-debug-chrome) ("pwa-chrome" js-debug-chrome)
    ("python" debugpy) ("debugpy" debugpy)
    ("go" dlv) ("delve" dlv)
    ("lldb" codelldb lldb-dap) ("codelldb" codelldb lldb-dap) ("lldb-dap" lldb-dap codelldb)
    ("cppdbg" cpptools) ("cppvsdbg" cpptools) ("gdb" gdb)
    ("php" xdebug) ("coreclr" netcoredbg) ("clr" netcoredbg)
    ("rdbg" rdbg) ("ruby" rdbg) ("java" jdtls)
    ("dart" flutter dart) ("flutter" flutter dart))
  "Map a VS Code launch.json `type' to candidate `dape-configs' keys.
The first candidate present in `dape-configs' wins.")

(defun ygg-dape--project-root ()
  (or (when-let* ((p (project-current))) (expand-file-name (project-root p)))
      default-directory))

(defun ygg-dape--strip-jsonc (s)
  "Strip // and /* */ comments and trailing commas from JSONC string S.
String-aware: comment/comma syntax inside JSON string literals is left alone."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (let ((in-str nil))
      (while (not (eobp))
        (let ((c (char-after)))
          (cond
           (in-str (cond ((eq c ?\\) (forward-char 2))
                         ((eq c ?\") (setq in-str nil) (forward-char 1))
                         (t (forward-char 1))))
           ((eq c ?\") (setq in-str t) (forward-char 1))
           ((and (eq c ?/) (eq (char-after (1+ (point))) ?/))
            (delete-region (point) (line-end-position)))
           ((and (eq c ?/) (eq (char-after (1+ (point))) ?*))
            (let ((start (point)))
              (if (search-forward "*/" nil t) (delete-region start (point))
                (delete-region start (point-max)))))
           (t (forward-char 1))))))
    (goto-char (point-min))
    (while (re-search-forward ",\\([[:space:]\n]*[]}]\\)" nil t)
      (replace-match "\\1"))
    (buffer-string)))

(defun ygg-dape--read-jsonc (file)
  "Parse JSONC FILE into a plist (keyword keys, vector arrays)."
  (with-temp-buffer
    (insert (ygg-dape--strip-jsonc
             (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'plist :array-type 'array
                       :null-object nil :false-object :json-false)))

(defun ygg-dape--subst-string (s root file)
  "Expand VS Code ${...} variables in string S against ROOT and FILE."
  (let ((base (file-name-nondirectory (directory-file-name root))))
    (replace-regexp-in-string
     "\\${\\([^}]+\\)}"
     (lambda (m)
       (let ((var (match-string 1 m)))
         (cond
          ((string= var "workspaceFolder") (directory-file-name root))
          ((string= var "workspaceFolderBasename") base)
          ((string= var "file") (or file ""))
          ((string= var "fileWorkspaceFolder") (directory-file-name root))
          ((string= var "fileDirname")
           (if file (directory-file-name (file-name-directory file)) ""))
          ((string= var "fileBasename") (if file (file-name-nondirectory file) ""))
          ((string= var "fileBasenameNoExtension") (if file (file-name-base file) ""))
          ((string= var "relativeFile") (if file (file-relative-name file root) ""))
          ((string= var "cwd") (directory-file-name default-directory))
          ((string= var "pathSeparator") "/")
          ((string-prefix-p "env:" var) (or (getenv (substring var 4)) ""))
          ((string-prefix-p "input:" var) (read-string (format "%s: " (substring var 6))))
          (t m))))
     s t t)))

(defun ygg-dape--subst (v root file)
  "Recursively expand ${...} vars in V (string/array/nested object)."
  (cond
   ((stringp v) (ygg-dape--subst-string v root file))
   ((vectorp v) (apply #'vector (mapcar (lambda (x) (ygg-dape--subst x root file)) v)))
   ((and (consp v) (keywordp (car v)))
    (cl-loop for (k val) on v by #'cddr append (list k (ygg-dape--subst val root file))))
   (t v)))

(defun ygg-dape--adapter-for (type)
  "First `dape-configs' key mapped from VS Code TYPE that actually exists."
  (seq-find (lambda (k) (assq k dape-configs))
            (cdr (assoc type ygg-dape-vscode-type-adapters))))

(defun ygg-dape--task-command (label root)
  "Resolve preLaunchTask LABEL from .vscode/tasks.json to a shell command."
  (let ((f (expand-file-name ".vscode/tasks.json" root)))
    (when (file-readable-p f)
      (when-let* ((tasks (plist-get (ygg-dape--read-jsonc f) :tasks)))
        (seq-some
         (lambda (tk)
           (when (equal (plist-get tk :label) label)
             (string-trim
              (concat (ygg-dape--subst-string (or (plist-get tk :command) "") root nil)
                      (let ((args (plist-get tk :args)))
                        (when (vectorp args)
                          (mapconcat (lambda (a)
                                       (concat " " (ygg-dape--subst-string
                                                    (format "%s" a) root nil)))
                                     args "")))))))
         (append tasks nil))))))

(defun ygg-dape--vscode->dape (vsconf root file)
  "Merge a VS Code launch config plist VSCONF onto its dape base adapter."
  (let* ((type (plist-get vsconf :type))
         (akey (ygg-dape--adapter-for type))
         (base (and akey (copy-tree (cdr (assq akey dape-configs)))))
         (skip '(:type :name :request :presentation :preLaunchTask :postDebugTask
                 :internalConsoleOptions :__configurationTarget :serverReadyAction)))
    (unless base
      (user-error "No dape adapter for VS Code type %S (add one to dape-configs)" type))
    (setq base (plist-put base :request (or (plist-get vsconf :request) "launch")))
    (cl-loop for (k v) on vsconf by #'cddr
             unless (memq k skip)
             do (setq base (plist-put base k (ygg-dape--subst v root file))))
    (when-let* ((label (plist-get vsconf :preLaunchTask))
                (cmd (ygg-dape--task-command label root)))
      (setq base (plist-put base 'compile cmd)))
    base))

(defun ygg-dape-vscode ()
  "Pick and launch a debug session from .vscode/launch.json."
  (interactive)
  (require 'dape)
  (let* ((root (ygg-dape--project-root))
         (f (expand-file-name ".vscode/launch.json" root)))
    (unless (file-readable-p f)
      (user-error "No .vscode/launch.json under %s" root))
    (let* ((configs (plist-get (ygg-dape--read-jsonc f) :configurations)))
      (unless (and (vectorp configs) (> (length configs) 0))
        (user-error "launch.json has no configurations"))
      (let* ((alist (mapcar (lambda (c) (cons (or (plist-get c :name) "(unnamed)") c))
                            (append configs nil)))
             (name (completing-read "Debug (launch.json): " alist nil t))
             (vsconf (cdr (assoc name alist))))
        (dape (ygg-dape--vscode->dape vsconf root (buffer-file-name)))))))

;;; The debug verb on the colon line

(defvar ygg-ex--commands)

(declare-function dape--config-eval "dape")
(declare-function dape--config-from-string "dape")

(defun ygg-dape--ex-comment-or-blank-p ()
  "Whether the current line carries nothing but whitespace or a comment."
  (save-excursion
    (back-to-indentation)
    (or (eolp)
        (let* ((start (point))
               (state (syntax-ppss (line-end-position))))
          (and (nth 4 state) (<= (nth 8 state) start))))))

(defun ygg-dape--ex-break (_args)
  "Toggle the breakpoint on the nearest line that can carry one.
Point moves forward off a blank or comment-only line first, so the
breakpoint lands where the adapter can stop."
  (while (and (not (eobp)) (ygg-dape--ex-comment-or-blank-p))
    (forward-line 1))
  (back-to-indentation)
  (dape-breakpoint-toggle))

(defun ygg-dape--ex-step (command)
  "Run COMMAND on the connection that is stopped right now."
  (require 'dape nil t)
  (funcall command (dape--live-connection 'stopped)))

(defun ygg-dape--ex-launch (args)
  "Start a session from the config ARGS names, or from dape's own prompt.
ARGS takes the whole config line dape reads, overrides included."
  (require 'dape)
  (if (string-empty-p args)
      (call-interactively #'dape)
    (pcase-let ((`(,key ,config) (dape--config-from-string args)))
      (dape (dape--config-eval key config)))))

(defconst ygg-dape--ex-local-attach
  '((js-debug-node-attach host port)
    (debugpy-attach host port)
    (lldb-dap host port)
    (kotlin-attach nil :port)
    (jdtls-attach :hostName :port))
  "Per language, the attach entry that goes where it is told, and its keys.
The typed host and port fill those keys: host and port name the adapter
itself, the others the program's debug port behind an adapter the entry
starts.  A nil host key means loopback only.")

(defun ygg-dape--ex-bound-p (cell)
  "Whether dape config CELL names a mode this buffer's derives from."
  (when-let* ((modes (plist-get (cdr cell) 'modes)))
    (apply #'provided-mode-derived-p major-mode (append modes nil))))

(defun ygg-dape--ex-language-config ()
  "The dape config key bound to this buffer's language, a local attach first.
Configs that name no mode at all are not this buffer's and are skipped."
  (or (seq-find (lambda (key) (ygg-dape--ex-bound-p (assq key dape-configs)))
                (mapcar #'car ygg-dape--ex-local-attach))
      (let ((bound (seq-filter #'ygg-dape--ex-bound-p dape-configs)))
        (car (or (seq-find (lambda (c)
                             (equal (plist-get (cdr c) :request) "attach"))
                           bound)
                 (car bound))))))

(defconst ygg-dape--ex-spawn-keys
  '(command command-args command-cwd command-env command-insert-stderr ensure)
  "Keys that make dape start an adapter itself instead of connecting to one.")

(defun ygg-dape--ex-connect-only (config)
  "CONFIG with whatever would spawn a local adapter taken out of it."
  (cl-loop for (key value) on config by #'cddr
           unless (memq key ygg-dape--ex-spawn-keys)
           append (list key value)))

(defun ygg-dape--ex-attach (args)
  "Attach to the adapter listening at the HOST:PORT that ARGS spells."
  (require 'dape)
  (unless (string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" args)
    (user-error "usage: :debug attach HOST:PORT"))
  (let ((host (match-string 1 args))
        (port (string-to-number (match-string 2 args)))
        (key (ygg-dape--ex-language-config)))
    (unless key
      (user-error "no dape config for %s; dape has: %s" major-mode
                  (mapconcat (lambda (c) (symbol-name (car c)))
                             dape-configs ", ")))
    (pcase-let ((`(,host-key ,port-key) (alist-get key ygg-dape--ex-local-attach '(host port))))
      (unless (or host-key (member host '("localhost" "127.0.0.1" "::1")))
        (user-error "%s attaches on loopback only; forward %s:%d to a local port" key host port))
      (let ((config (dape--config-eval key `(,@(and host-key (list host-key host))
                                              ,port-key ,port :request "attach"))))
        (dape (if (eq port-key 'port) (ygg-dape--ex-connect-only config) config))))))

(defconst ygg-dape--ex-subcommands
  (list (cons "launch" #'ygg-dape--ex-launch)
        (cons "attach" #'ygg-dape--ex-attach)
        (cons "break" #'ygg-dape--ex-break)
        (cons "continue" (lambda (_args) (ygg-dape--ex-step #'dape-continue)))
        (cons "next" (lambda (_args) (ygg-dape--ex-step #'dape-next)))
        (cons "in" (lambda (_args) (ygg-dape--ex-step #'dape-step-in)))
        (cons "out" (lambda (_args) (ygg-dape--ex-step #'dape-step-out)))
        (cons "quit" (lambda (_args) (dape-quit)))
        (cons "stack" (lambda (_args) (ygg-dape-view-stack)))
        (cons "scope" (lambda (_args) (ygg-dape-view-scope)))
        (cons "watch" (lambda (_args) (ygg-dape-view-watch)))
        (cons "breaks" (lambda (_args) (ygg-dape-view-breakpoints))))
  "What the debug verb answers to, each taking the rest of the line.")

(defun ygg-ex--cmd-debug (_range _bang args)
  "Drive the debug session: ARGS names a subcommand and what it needs."
  (let* ((parts (split-string args nil t))
         (cell (and (car parts) (assoc (car parts) ygg-dape--ex-subcommands))))
    (unless cell
      (user-error "usage: :debug %s"
                  (mapconcat #'car ygg-dape--ex-subcommands "|")))
    (funcall (cdr cell) (string-join (cdr parts) " "))))

(with-eval-after-load 'yggdrasil-ex
  (setf (alist-get "debug" ygg-ex--commands nil nil #'equal)
        'ygg-ex--cmd-debug))

;;; Keys — launch.json when present, else the nvim-dap fallback convention

(defvar ygg-leader-dape-map (make-sparse-keymap) "The d prefix: debug (dape).")
(defvar ygg-leader-dape-view-map (make-sparse-keymap) "The d v prefix: drawer sections.")

(yggdrasil-define-keys 'ygg-leader-dape-view-map
  "s" #'ygg-dape-view-scope :label "scope"
  "w" #'ygg-dape-view-watch :label "watch"
  "k" #'ygg-dape-view-stack :label "stack"
  "b" #'ygg-dape-view-breakpoints :label "breakpoints"
  "t" #'ygg-dape-view-threads :label "threads"
  "c" #'ygg-dape-view-console :label "console")

(yggdrasil-define-keys 'ygg-leader-dape-map
  "b" #'dape-breakpoint-toggle :label "toggle breakpoint"
  "B" #'dape-breakpoint-expression :label "conditional breakpoint"
  "c" #'ygg-dape-continue :label "continue/launch"
  "j" #'ygg-dape-vscode :label "launch.json (vscode)"
  "i" #'dape-step-in :label "step in"
  "o" #'dape-next :label "step over"
  "O" #'dape-step-out :label "step out"
  "R" #'dape-restart :label "restart"
  "t" #'dape-quit :label "terminate"
  "e" #'ygg-dape-eval :label "eval"
  "r" #'dape-repl :label "repl"
  "l" #'ygg-dape-breakpoints-qf :label "breakpoints → quickfix"
  "d" #'ygg-dape-drawer-toggle :label "toggle drawer"
  "v" ygg-leader-dape-view-map :label "view"
  "x" #'consult-flymake :label "diagnostics"
  "X" #'ygg-diagnostics-project :label "diagnostics (project)")

(defun ygg-diagnostics-project ()
  "Project-wide diagnostics across every flymake-checked buffer (Zed panel)."
  (interactive)
  (consult-flymake t))

;; layer-lsp bound "d" to consult-flymake; this must load after layer-lsp
;; so the debug map wins, with diagnostics demoted to "SPC d x".
(yggdrasil-leader-def "d" ygg-leader-dape-map "debug")

(require 'ygg-debugpy)
(require 'ygg-dap-kotlin)
(require 'ygg-dap-js)
(require 'ygg-dap-java)
(require 'ygg-dap-lldb)

(provide 'layer-dap)
;;; layer-dap.el ends here
