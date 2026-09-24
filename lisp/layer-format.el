;;; layer-format.el --- Format-on-save + external linters layer -*- lexical-binding: t; -*-

;; Built-ins wrapped: flymake (backend registration only; flymake.el itself
;; ships with Emacs).
;; Custom: apheleia format-on-save wiring (ports conform.nvim's
;; formatters_by_ft), flymake-collection external-linter wiring (ports
;; nvim-lint's linters_by_ft), and the format-on-save toggle.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst ygg-format-tools
  '((:name "biome" :serves "JS/TS/JSON/CSS formatter (biome.json)" :formatter biome
           :prefer-when ("biome.json" "biome.jsonc")
           :modes (js-mode js-ts-mode js-jsx-mode jsx-ts-mode typescript-mode
                   typescript-ts-mode tsx-ts-mode json-mode json-ts-mode js-json-mode
                   css-mode css-ts-mode))
    (:name "prettier" :serves "JSON formatter (project prettier)" :formatter prettier-json
           :prefer-when (".prettierrc" ".prettierrc.json" ".prettierrc.json5" ".prettierrc.yaml"
                         ".prettierrc.yml" ".prettierrc.toml" ".prettierrc.js" ".prettierrc.cjs"
                         ".prettierrc.mjs" ".prettierrc.ts" "prettier.config.js"
                         "prettier.config.cjs" "prettier.config.mjs" "prettier.config.ts"
                         "node_modules/.bin/prettier" ("package.json" . "\"prettier\"[ \t]*:"))
           :modes (json-mode json-ts-mode js-json-mode))
    (:name "jq" :serves "JSON formatter" :formatter jq :modes (json-mode json-ts-mode js-json-mode))
    (:name "prettier" :serves "TypeScript formatter" :formatter prettier-typescript
           :modes (typescript-mode typescript-ts-mode tsx-ts-mode))
    (:name "prettier" :serves "JavaScript formatter" :formatter prettier-javascript
           :modes (js-mode js-ts-mode js-jsx-mode jsx-ts-mode))
    (:name "prettier" :serves "CSS formatter" :formatter prettier-css :modes (css-mode css-ts-mode))
    (:name "prettier" :serves "HTML formatter" :formatter prettier-html
           :modes (html-mode html-ts-mode mhtml-mode))
    (:name "prettier" :serves "YAML formatter" :formatter prettier-yaml :modes (yaml-mode yaml-ts-mode))
    (:name "prettier" :serves "Markdown formatter" :formatter prettier-markdown
           :modes (markdown-mode gfm-mode))
    (:name "black" :serves "Python formatter ([tool.black])" :formatter black :project-bin t
           :prefer-when (("pyproject.toml" . "^\\[tool\\.black\\]"))
           :unless (("pyproject.toml" . "^\\[tool\\.ruff") "ruff.toml" ".ruff.toml")
           :modes (python-mode python-ts-mode))
    (:name "ruff" :serves "Python formatter and linter" :formatter ruff :project-bin t
           :linter flymake-collection-ruff :modes (python-mode python-ts-mode))
    (:name "rustfmt" :serves "Rust formatter" :formatter rustfmt :modes (rust-mode rust-ts-mode))
    (:name "gofmt" :serves "Go formatter" :formatter gofmt :modes (go-mode go-ts-mode))
    (:name "swiftformat" :serves "Swift formatter (.swiftformat)" :formatter swiftformat
           :prefer-when (".swiftformat") :modes (swift-mode swift-ts-mode))
    (:name "xcrun" :serves "Swift formatter (xcrun swift-format)" :formatter swift-format
           :save-when (".swift-format") :modes (swift-mode swift-ts-mode))
    (:name "swiftlint" :serves "Swift linter" :linter ygg-format-swiftlint
           :modes (swift-mode swift-ts-mode))
    (:name "ktlint" :serves "Kotlin formatter" :formatter ktlint
           :save-when ((".editorconfig" . "ktlint") ("build.gradle.kts" . "ktlint")
                       ("build.gradle" . "ktlint"))
           :modes (kotlin-mode kotlin-ts-mode))
    (:name "google-java-format" :serves "Java formatter" :formatter google-java-format
           :save-when (("build.gradle" . "google-java-format\\|googleJavaFormat")
                       ("build.gradle.kts" . "google-java-format\\|googleJavaFormat")
                       ("pom.xml" . "google-java-format\\|googleJavaFormat"))
           :modes (java-mode java-ts-mode))
    (:name "mix" :serves "Elixir formatter" :formatter mix-format
           :modes (elixir-mode elixir-ts-mode heex-ts-mode))
    (:name "dart" :serves "Dart formatter" :formatter dart-format :modes (dart-mode dart-ts-mode))
    (:name "clang-format" :serves "C/C++ formatter" :formatter clang-format
           :save-when (".clang-format" "_clang-format")
           :modes (c-mode c-ts-mode c++-mode c++-ts-mode objc-mode))
    (:name "standardrb" :serves "Ruby formatter (standard)" :formatter ruby-standard
           :project-bin t :gem "standard"
           :prefer-when (("Gemfile.lock" . "^    standard ("))
           :modes (ruby-mode ruby-ts-mode))
    (:name "rubocop" :serves "Ruby formatter" :formatter rubocop :project-bin t
           :save-when (".rubocop.yml" ("Gemfile.lock" . "^    rubocop ("))
           :modes (ruby-mode ruby-ts-mode))
    (:name "stylua" :serves "Lua formatter" :formatter stylua :modes (lua-mode lua-ts-mode))
    (:name "shfmt" :serves "Shell formatter" :formatter shfmt :modes (sh-mode bash-ts-mode))
    (:name "air" :serves "R formatter" :formatter air :modes (r-ts-mode))
    (:name "taplo" :serves "TOML formatter" :formatter taplo :modes (conf-toml-mode toml-ts-mode))
    (:name "pg_format" :serves "SQL formatter" :formatter pgformatter
           :save-when (".pg_format") :modes (sql-mode))
    (:name "terraform" :serves "Terraform formatter" :formatter terraform :modes (terraform-mode))
    (:serves "Dockerfile: no formatter" :formatter none :modes (dockerfile-mode dockerfile-ts-mode))
    (:name "shellcheck" :serves "Shell linter" :modes (sh-mode bash-ts-mode))
    (:name "yamllint" :serves "YAML linter" :modes (yaml-mode yaml-ts-mode))
    (:name "markdownlint" :serves "Markdown linter" :linter ygg-format-markdownlint
           :modes (markdown-mode gfm-mode)))
  "Formatters and linters this layer wires up and probes for on PATH.
Per buffer, the first row for the mode whose :prefer-when files exist
upward (and whose :unless files do not) chooses the formatter, else the
first row without :prefer-when does.  A row with :save-when formats on
save only in projects holding one of those files.  A file entry is a
name or (NAME . REGEXP) that NAME's contents must match.  :project-bin
runs the project's own binary (bundle exec, node_modules/.bin, .venv)
before PATH.  :linter rows are re-added after eglot takes a buffer; rows
without one are linted by flymake-collection's defaults, only where no
server runs.")

(declare-function apheleia-global-mode "apheleia")
(declare-function apheleia-mode "apheleia")
(defvar apheleia-mode-alist)
(defvar apheleia-formatters)
(defvar apheleia-formatter)
(defvar apheleia-formatters-respect-indent-level)
(defvar apheleia-inhibit-functions)

(declare-function flymake-collection-hook-setup "flymake-collection-hook")
(declare-function flymake-start "flymake")
(declare-function eglot-managed-p "eglot")
(defvar flymake-collection-hook-config)

(declare-function yggdrasil-define-keys "yggdrasil-core")
(defvar ygg-leader-code-map)

(defun ygg-format--rows (key &optional mode)
  "Rows of the tools table carrying KEY, only those serving MODE if given."
  (seq-filter (lambda (tool)
                (and (plist-get tool key)
                     (or (null mode) (memq mode (plist-get tool :modes)))))
              ygg-format-tools))

(defun ygg-format--mode-alist ()
  "What each mode is formatted with when its project names nothing."
  (let ((cells nil))
    (dolist (tool (ygg-format--rows :formatter))
      (unless (plist-get tool :prefer-when)
        (let ((formatter (plist-get tool :formatter)))
          (dolist (mode (plist-get tool :modes))
            (unless (assq mode cells)
              (push (cons mode (unless (eq formatter 'none) formatter)) cells))))))
    (nreverse cells)))

;;; 1. Project resolution — which tool, which binary

(defun ygg-format--locate (name dir)
  "Path of NAME at or above DIR, never above DIR's git root (DIR alone
outside a repository), or nil."
  (let* ((top (or (locate-dominating-file dir ".git") dir))
         (found (locate-dominating-file dir name)))
    (when (and found (file-in-directory-p found top))
      (expand-file-name name found))))

(defun ygg-format--file-p (spec dir)
  "Whether file SPEC, a name or (NAME . REGEXP), is found by the locator from DIR."
  (when-let* ((path (ygg-format--locate (if (consp spec) (car spec) spec) dir)))
    (or (atom spec)
        (with-temp-buffer
          (insert-file-contents path)
          (re-search-forward (cdr spec) nil t)))))

(defun ygg-format--any-file-p (specs dir)
  (and dir (seq-some (lambda (spec) (ygg-format--file-p spec dir)) specs)))

(defun ygg-format--project-bin (name &optional gem)
  "The project's own NAME: bundle exec when Gemfile.lock lists GEM (else
NAME), else node_modules/.bin or .venv/bin within the project."
  (let ((dir default-directory))
    (cond
     ((ygg-format--file-p (cons "Gemfile.lock" (format "^    %s (" (regexp-quote (or gem name))))
                          dir)
      (list "bundle" "exec" name))
     ((ygg-format--locate (concat "node_modules/.bin/" name) dir))
     ((ygg-format--locate (concat ".venv/bin/" name) dir)))))

(defun ygg-format--bin (name &optional gem)
  "Command head for NAME: the project's own binary, else NAME on PATH."
  (or (ygg-format--project-bin name gem) name))

(defvar ygg-format--decisions (make-hash-table :test #'equal)
  "Per directory and mode: (FORMATTER . FORMAT-ON-SAVE).")

(defun ygg-format--decide-uncached (rows dir)
  (let* ((preferred (seq-find (lambda (tool)
                                (and (plist-get tool :prefer-when)
                                     (ygg-format--any-file-p (plist-get tool :prefer-when) dir)
                                     (not (ygg-format--any-file-p (plist-get tool :unless) dir))))
                              rows))
         (row (or preferred
                  (seq-find (lambda (tool) (not (plist-get tool :prefer-when))) rows))))
    (when row
      (cons (plist-get row :formatter)
            (and (not (eq (plist-get row :formatter) 'none))
                 (or (null (plist-get row :save-when))
                     (ygg-format--any-file-p (plist-get row :save-when) dir)))))))

(defun ygg-format--decide ()
  "(FORMATTER . FORMAT-ON-SAVE) for this buffer, or nil when no row serves it."
  (when-let* ((rows (ygg-format--rows :formatter major-mode)))
    ;; nested projects may each name their own tool, so a whole repo can't share one answer
    (with-memoization (gethash (cons default-directory major-mode) ygg-format--decisions)
      (ygg-format--decide-uncached
       rows (unless (file-remote-p default-directory) default-directory)))))

(defun ygg-format-forget-projects ()
  "Re-read project formatter config files in buffers opened from now on."
  (interactive)
  (clrhash ygg-format--decisions))

(defun ygg-format--choose ()
  "Point apheleia at the formatter this buffer's project asks for."
  (when-let* ((decision (ygg-format--decide)))
    (unless (local-variable-p 'apheleia-formatter)
      (setq-local apheleia-formatter
                  (unless (eq (car decision) 'none) (car decision))))))

(defun ygg-format--inhibit-p ()
  "Whether this buffer must not format on save."
  (when-let* ((decision (ygg-format--decide)))
    (not (cdr decision))))

(add-hook 'after-change-major-mode-hook #'ygg-format--choose)
(add-hook 'apheleia-inhibit-functions #'ygg-format--inhibit-p)

;; Emacs launched outside a mise-activated shell never sees the global tools.
(let ((shims (expand-file-name "~/.local/share/mise/shims")))
  (when (and (file-directory-p shims) (not (member shims exec-path)))
    (setq exec-path (append exec-path (list shims)))
    (setenv "PATH" (concat (getenv "PATH") path-separator shims))))

;;; 2. Apheleia — format on save

(defun ygg-format--use-project-bins ()
  "Run each :project-bin row's formatter through the project binary resolver."
  (dolist (tool (ygg-format--rows :project-bin))
    (let* ((formatter (plist-get tool :formatter))
           (command (alist-get formatter apheleia-formatters)))
      (when (and (consp command) (equal (car command) (plist-get tool :name)))
        (setf (alist-get formatter apheleia-formatters)
              (cons (list 'ygg-format--bin (plist-get tool :name) (plist-get tool :gem))
                    (cdr command)))))))

(defun ygg-format-setup-apheleia ()
  "Wire the tools table into apheleia and turn format on save on."
  (require 'apheleia)
  (setq apheleia-formatters-respect-indent-level nil)
  (setf (alist-get 'swift-format apheleia-formatters)
        '("xcrun" "swift-format" "format" "--assume-filename" filepath "-")
        (alist-get 'swiftformat apheleia-formatters)
        '("swiftformat" "--quiet" "--stdinpath" filepath))
  (ygg-format--use-project-bins)
  (dolist (cell (reverse (ygg-format--mode-alist)))
    (setq apheleia-mode-alist
          (cons cell (seq-remove (lambda (old) (eq (car-safe old) (car cell)))
                                 apheleia-mode-alist))))
  (apheleia-global-mode 1))

(when (fboundp 'elpaca)
  (elpaca apheleia
    ;; formatter dispatch table + npx/project lookups cost real time — defer
    (run-with-idle-timer 1 nil #'ygg-format-setup-apheleia)))

;;; 3. Flymake-collection — external linters, beside the server or alone

(defun ygg-format--with-project-path (backend &rest args)
  "Run linter BACKEND with the project's node_modules/.bin and .venv/bin first."
  (let ((exec-path
         (append (delq nil (mapcar (lambda (dir) (ygg-format--locate dir default-directory))
                                   '("node_modules/.bin" ".venv/bin")))
                 exec-path)))
    (apply backend args)))

(defun ygg-format--rejoin-linters ()
  "Put this mode's linters back beside eglot, which replaced them."
  (when-let* (((eglot-managed-p))
              (rows (ygg-format--rows :linter major-mode)))
    (dolist (tool rows)
      (add-hook 'flymake-diagnostic-functions (plist-get tool :linter) nil t))
    (flymake-start)))

(defun ygg-format-setup-linters ()
  "Wire flymake-collection linters, with and without a language server."
  (require 'flymake-collection-hook)
  (require 'ygg-format-linters)
  ;; defaults already cover lua-mode/sh+bash/markdown/yaml; only add what's missing
  (push '(lua-ts-mode . (flymake-collection-luacheck)) flymake-collection-hook-config)
  (push '((json-mode json-ts-mode)
          . (flymake-collection-jsonlint (flymake-collection-jq :disabled t)))
        flymake-collection-hook-config)
  (dolist (tool (ygg-format--rows :linter))
    (advice-add (plist-get tool :linter) :around #'ygg-format--with-project-path)
    (push (cons (plist-get tool :modes) (list (plist-get tool :linter)))
          flymake-collection-hook-config))
  (flymake-collection-hook-setup)
  (add-hook 'eglot-managed-mode-hook #'ygg-format--rejoin-linters)
  (dolist (mode (delete-dups
                 (append '(sh-mode bash-ts-mode lua-mode lua-ts-mode json-mode json-ts-mode
                           markdown-mode gfm-mode yaml-mode yaml-ts-mode)
                         (mapcan (lambda (tool) (copy-sequence (plist-get tool :modes)))
                                 (ygg-format--rows :linter)))))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'flymake-mode)))

(when (fboundp 'elpaca)
  (elpaca flymake-collection
    (run-with-idle-timer 1 nil #'ygg-format-setup-linters)))

;;; 4. Binary probe — warn, never fail

(defun ygg-format--probe ()
  "Say which formatters are not on PATH."
  (let ((seen nil))
    (dolist (tool ygg-format-tools)
      (let ((name (plist-get tool :name)))
        (unless (or (null name) (member name seen) (plist-get tool :prefer-when)
                    (executable-find name))
          (push name seen)
          (message "yggdrasil-format: %s (%s) not found on PATH"
                   (plist-get tool :serves) name))))))

(add-hook 'elpaca-after-init-hook #'ygg-format--probe)

;;; 5. Toggle command — leader `c s`

(defun ygg-format-on-save-toggle ()
  "Toggle `apheleia-mode' (format on save) in the current buffer."
  (interactive)
  (if (fboundp 'apheleia-mode)
      (apheleia-mode 'toggle)
    (message "yggdrasil-format: apheleia not loaded yet")))

(yggdrasil-define-keys 'ygg-leader-code-map
  "s" #'ygg-format-on-save-toggle :label "format on save")

;;; 6. ws-butler — trim trailing whitespace only on touched lines (no
;;; whole-file whitespace diffs) — and dtrt-indent — adopt each file's
;;; existing indentation so edits match the file, not our defaults

(declare-function ws-butler-global-mode "ws-butler")
(declare-function dtrt-indent-global-mode "dtrt-indent")

(when (fboundp 'elpaca)
  (elpaca ws-butler
    (run-with-idle-timer
     1 nil (lambda () (require 'ws-butler) (ws-butler-global-mode 1))))
  (elpaca dtrt-indent
    (run-with-idle-timer
     1 nil (lambda () (require 'dtrt-indent) (dtrt-indent-global-mode 1)))))

(provide 'layer-format)
;;; layer-format.el ends here
