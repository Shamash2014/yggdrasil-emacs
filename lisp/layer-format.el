;;; layer-format.el --- Format-on-save + external linters layer -*- lexical-binding: t; -*-

;; Built-ins wrapped: flymake (backend registration only; flymake.el itself
;; ships with Emacs).
;; Custom: apheleia format-on-save wiring (ports conform.nvim's
;; formatters_by_ft), flymake-collection external-linter wiring (ports
;; nvim-lint's linters_by_ft), and the format-on-save toggle.

;;; Code:

(defconst ygg-format-tools
  '((:name "stylua" :serves "Lua formatter" :modes (lua-mode lua-ts-mode))
    (:name "shfmt" :serves "Shell formatter" :formatter shfmt :modes (sh-mode))
    (:name "jq" :serves "JSON formatter" :formatter jq :modes (json-mode json-ts-mode))
    (:name "prettier" :serves "Markdown formatter" :formatter prettier-markdown
           :modes (markdown-mode gfm-mode))
    (:name "shellcheck" :serves "Shell linter" :modes (sh-mode bash-ts-mode))
    (:name "markdownlint" :serves "Markdown linter" :modes (markdown-mode gfm-mode)))
  "Formatters and linters this layer wires up and probes for on PATH.")

(declare-function apheleia-global-mode "apheleia")
(declare-function apheleia-mode "apheleia")
(defvar apheleia-mode-alist)

(declare-function flymake-collection-hook-setup "flymake-collection-hook")
(defvar flymake-collection-hook-config)

(declare-function yggdrasil-define-keys "yggdrasil-core")
(defvar ygg-leader-code-map)

(defun ygg-format--mode-alist ()
  "What each mode is formatted with."
  (let ((cells nil))
    (dolist (tool ygg-format-tools)
      (when-let* ((formatter (plist-get tool :formatter)))
        (dolist (mode (plist-get tool :modes))
          (push (cons mode formatter) cells))))
    (nreverse cells)))

;;; 1. Apheleia — format on save

(when (fboundp 'elpaca)
  (elpaca apheleia
    ;; formatter dispatch table + npx/project lookups cost real time — defer
    (run-with-idle-timer
     1 nil
     (lambda ()
       (require 'apheleia)
       ;; bash-ts-mode/lua(-ts)-mode/elixir(-ts)-mode already correct by default
       (dolist (cell (ygg-format--mode-alist))
         (add-to-list 'apheleia-mode-alist cell))
       (apheleia-global-mode 1)))))

;;; 2. Flymake-collection — external (non-LSP) linters

(when (fboundp 'elpaca)
  (elpaca flymake-collection
    (run-with-idle-timer
     1 nil
     (lambda ()
       (require 'flymake-collection-hook)
       ;; defaults already cover lua-mode/sh+bash/markdown; only add what's missing
       (push '(lua-ts-mode . (flymake-collection-luacheck)) flymake-collection-hook-config)
       (push '((json-mode json-ts-mode)
               . (flymake-collection-jsonlint (flymake-collection-jq :disabled t)))
             flymake-collection-hook-config)
       (flymake-collection-hook-setup)
       (dolist (mode '(sh-mode bash-ts-mode lua-mode lua-ts-mode
                       json-mode json-ts-mode markdown-mode gfm-mode))
         (when (fboundp mode)
           (add-hook (intern (concat (symbol-name mode) "-hook")) #'flymake-mode)))))))

;;; 3. Binary probe — warn, never fail

(defun ygg-format--probe ()
  "Say which formatters are not on PATH."
  (dolist (tool ygg-format-tools)
    (unless (executable-find (plist-get tool :name))
      (message "yggdrasil-format: %s (%s) not found on PATH"
               (plist-get tool :serves) (plist-get tool :name)))))

(add-hook 'elpaca-after-init-hook #'ygg-format--probe)

;;; 4. Toggle command — leader `c s`

(defun ygg-format-on-save-toggle ()
  "Toggle `apheleia-mode' (format on save) in the current buffer."
  (interactive)
  (if (fboundp 'apheleia-mode)
      (apheleia-mode 'toggle)
    (message "yggdrasil-format: apheleia not loaded yet")))

(yggdrasil-define-keys 'ygg-leader-code-map
  "s" #'ygg-format-on-save-toggle :label "format on save")

;;; 5. ws-butler — trim trailing whitespace only on touched lines (no
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
