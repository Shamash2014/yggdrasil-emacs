;;; layer-completion.el --- Completion layer -*- lexical-binding: t; -*-

;;; Code:

(require 'yggdrasil-leader)

(declare-function ygg-hlsearch-clear "yggdrasil-motions")

(declare-function vertico-mode "vertico")
(declare-function vertico-next "vertico")
(declare-function vertico-previous "vertico")
(declare-function marginalia-mode "marginalia")
(declare-function global-corfu-mode "corfu")
(declare-function corfu-popupinfo-mode "corfu-popupinfo")
(declare-function corfu-next "corfu")
(declare-function corfu-previous "corfu")
(declare-function corfu-insert "corfu")
(declare-function corfu-complete "corfu")
(declare-function cape-dabbrev "cape")
(declare-function cape-file "cape")
(declare-function cape-keyword "cape")
(declare-function consult-buffer "consult")
(declare-function consult-line "consult")
(declare-function consult-ripgrep "consult")
(declare-function ygg-cm-show "ygg-context-manager" (&optional budget ask))
(declare-function ygg-cm-build "ygg-context-manager"
                  (&optional root callback forget))
(autoload 'ygg-qf-from-selection "layer-quickfix" nil t)
(declare-function consult-recent-file "consult")
(declare-function consult-imenu "consult")
(declare-function consult-preview-at-point-mode "consult")
(declare-function embark-act "embark")
(declare-function embark-export "embark")
(declare-function prescient-persist-mode "prescient")
(declare-function vertico-prescient-mode "vertico-prescient")
(declare-function corfu-prescient-mode "corfu-prescient")
(declare-function nerd-icons-completion-mode "nerd-icons-completion")
(declare-function nerd-icons-completion-marginalia-setup "nerd-icons-completion")
(declare-function nerd-icons-corfu-formatter "nerd-icons-corfu")
(defvar corfu-margin-formatters)

(defvar vertico-prescient-enable-filtering)
(defvar corfu-prescient-enable-filtering)
(defvar vertico-map)
(defvar corfu-map)
(defvar corfu-auto)
(defvar corfu-auto-delay)
(defvar corfu-auto-prefix)
(defvar corfu-cycle)
(defvar corfu-popupinfo-delay)

(when (fboundp 'elpaca)
  (elpaca vertico
    ;; snacks-picker feel: fixed-height panel, no jumping as matches shrink
    (setq vertico-count (max 8 (round (* 0.4 (frame-height))))
          vertico-resize nil
          vertico-cycle t)
    (vertico-mode 1)
    (define-key vertico-map (kbd "C-j") #'vertico-next)
    (define-key vertico-map (kbd "C-k") #'vertico-previous)
    ;; C-u prefix-arg is intentionally shadowed here — vim paging wins
    (define-key vertico-map (kbd "C-d") #'vertico-scroll-up)
    (define-key vertico-map (kbd "C-u") #'vertico-scroll-down)
    ;; ESC cancels the picker (helix/evil), not just C-g
    (define-key vertico-map [escape] #'abort-minibuffers)))

(when (fboundp 'elpaca)
  (elpaca orderless
    (setq completion-styles '(orderless basic)
          completion-category-overrides '((file (styles partial-completion basic))))))

(when (fboundp 'elpaca)
  (elpaca marginalia
    (marginalia-mode 1)))

;; snacks.picker feel: a file/buffer/command icon before each candidate
;; (nerd-icons-font-family is pinned early in init.el, before this loads)
(when (fboundp 'elpaca)
  (elpaca nerd-icons)
  (elpaca nerd-icons-completion
    (nerd-icons-completion-mode 1)
    (when (featurep 'marginalia) (nerd-icons-completion-marginalia-setup))
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(when (fboundp 'elpaca)
  (elpaca consult
    (setq consult-narrow-key "<")
    (with-eval-after-load 'consult
      (setq consult-fd-args
            (append consult-fd-args '("--hidden" "--exclude" ".git")))
      (consult-customize
       consult-ripgrep
       consult-line
       consult-recent-file
       consult-buffer
       :preview-key '(:debounce 0.2 any)))))

(when (fboundp 'elpaca)
  (elpaca corfu
    (setq corfu-auto t
          corfu-auto-delay 0.1
          corfu-auto-prefix 2
          corfu-cycle t)
    (global-corfu-mode 1)
    (require 'corfu-popupinfo)
    (setq corfu-popupinfo-delay '(0.5 . 0.2))
    (corfu-popupinfo-mode 1)
    (define-key corfu-map (kbd "C-j") #'corfu-next)
    (define-key corfu-map (kbd "C-k") #'corfu-previous)
    (define-key corfu-map (kbd "C-d") #'corfu-scroll-up)
    (define-key corfu-map (kbd "C-u") #'corfu-scroll-down)
    (define-key corfu-map (kbd "RET") #'corfu-insert)
    (define-key corfu-map (kbd "TAB") #'corfu-complete)
    (define-key corfu-map [tab] #'corfu-complete)))

;; kind icon in the corfu popup (snacks completion feel)
(when (fboundp 'elpaca)
  (elpaca nerd-icons-corfu
    (with-eval-after-load 'corfu
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))))

(when (fboundp 'elpaca)
  (elpaca cape
    ;; append: dabbrev is a buffer scan — let real capfs win first, it fires
    ;; only as a last resort rather than scanning on every corfu-auto keystroke
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
    (setq text-mode-ispell-word-completion nil)
    (add-to-list 'completion-at-point-functions #'cape-file t)
    (add-to-list 'completion-at-point-functions #'cape-keyword t)))

(declare-function tempel-complete "tempel")
(declare-function tempel-next "tempel")
(declare-function tempel-previous "tempel")
(declare-function tempel-done "tempel")
(defvar tempel-path)
(defvar tempel-map)

;; snippet expansion via tempel, surfaced through corfu as a capf
(when (fboundp 'elpaca)
  (elpaca tempel
    (setq tempel-path (expand-file-name "templates" user-emacs-directory))
    (add-to-list 'completion-at-point-functions #'tempel-complete)
    (with-eval-after-load 'tempel
      ;; field navigation while a template is being filled
      (define-key tempel-map (kbd "TAB") #'tempel-next)
      (define-key tempel-map (kbd "<backtab>") #'tempel-previous)
      (define-key tempel-map (kbd "C-j") #'tempel-next)
      (define-key tempel-map (kbd "C-k") #'tempel-previous)
      (define-key tempel-map (kbd "<escape>") #'tempel-done))))

(declare-function embark-collect "embark")
(declare-function embark-become "embark")

(when (fboundp 'elpaca)
  (elpaca embark
    (global-set-key (kbd "C-.") #'embark-act)
    (global-set-key (kbd "C-;") #'embark-act)
    (define-key minibuffer-local-map (kbd "C-c C-e") #'embark-export)
    ;; any picker -> one editable results buffer (Zed multibuffer feel)
    (define-key minibuffer-local-map (kbd "C-e") #'embark-export)
    (define-key minibuffer-local-map (kbd "C-c C-v") #'embark-collect)
    (define-key minibuffer-local-map (kbd "C-c C-b") #'embark-become)
    (require 'ygg-embark)
    (yggdrasil-leader-def "." #'embark-act "context menu")))

(when (fboundp 'elpaca)
  (elpaca embark-consult
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))

(when (fboundp 'elpaca)
  (elpaca prescient
    (require 'prescient)
    (prescient-persist-mode 1)))

(when (fboundp 'elpaca)
  (elpaca vertico-prescient
    (setq vertico-prescient-enable-filtering nil)
    (vertico-prescient-mode 1)))

(when (fboundp 'elpaca)
  (elpaca corfu-prescient
    (setq corfu-prescient-enable-filtering nil)
    (corfu-prescient-mode 1)))

(declare-function consult-fd "consult")
(declare-function project-root "project")
(declare-function project-current "project")

(defun ygg-files-picker ()
  "Snacks files picker: fuzzy recursive from the project root, else cwd.
Falls through to plain `find-file' (path completion, new files) when fd
is missing; all of it renders in the vertico posframe."
  (interactive)
  (let ((root (or (when-let* ((p (project-current))) (project-root p))
                  default-directory)))
    (cond
     ((and (executable-find "fd") (fboundp 'consult-fd)) (consult-fd root))
     ((project-current) (project-find-file))
     (t (call-interactively #'find-file)))))

(defun ygg-find-file-here ()
  "Plain path-completion find-file (create files, arbitrary paths)."
  (interactive)
  (call-interactively #'find-file))

(yggdrasil-leader-def "f f" #'ygg-files-picker "files (fuzzy)")
(yggdrasil-leader-def "f e" #'ygg-find-file-here "edit path")
(yggdrasil-leader-def "f r" #'consult-recent-file "recent file")
(yggdrasil-leader-def "b b" #'consult-buffer "switch buffer")
(yggdrasil-leader-def "f /" #'consult-line "search buffer")
(yggdrasil-leader-def "f i" #'consult-imenu "imenu")
(when (executable-find "fd")
  (yggdrasil-leader-def "f F" #'consult-fd "fd fuzzy (fff-style)"))

(defun ygg-search-word-at-point ()
  "Project grep seeded with the symbol at point (nvim SPC s w)."
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (unless sym (user-error "No symbol at point"))
    (if (executable-find "rg")
        (consult-ripgrep nil sym)
      (project-find-regexp (regexp-quote sym)))))

(defvar ygg-leader-search-map (make-sparse-keymap))
(yggdrasil-define-keys 'ygg-leader-search-map
  "s" #'consult-line :label "search buffer"
  "p" #'ygg-cm-show :label "repo map"
  "P" #'ygg-cm-build :label "build the map"
  "g" (if (executable-find "rg") #'consult-ripgrep #'project-find-regexp)
  :label "grep project (ripgrep)"
  "w" #'ygg-search-word-at-point :label "grep word"
  "i" #'consult-imenu :label "imenu"
  "x" #'ygg-qf-from-selection :label "selection to quickfix"
  "m" #'consult-mark :label "marks"
  "o" #'consult-outline :label "outline"
  "c" #'ygg-hlsearch-clear :label "clear highlight")
(yggdrasil-leader-def "s" ygg-leader-search-map "search")

;;; Resume the last picker, under the search prefix the finders share

(declare-function vertico-repeat "vertico-repeat")
(declare-function vertico-repeat-save "vertico-repeat")
(declare-function vertico-repeat-select "vertico-repeat")

(with-eval-after-load 'vertico
  (when (require 'vertico-repeat nil t)
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (yggdrasil-define-keys 'ygg-leader-search-map
      "r" #'vertico-repeat :label "resume picker"
      "R" #'vertico-repeat-select :label "select from history")))

;;; Buffer picker: terminals and agent buffers open in a split, never in place

(defvar consult--buffer-display)
(defvar ygg-term-display-action)

(defun ygg--job-buffer-p (buf)
  (or (string-match-p "\\`\\*\\(ygg-term\\|agent:\\|task:\\)" (buffer-name buf))
      (provided-mode-derived-p (buffer-local-value 'major-mode buf) 'ghostel-mode)))

(defun ygg--buffer-display-split (buffer &optional norecord)
  (let ((buf (get-buffer buffer)))
    (if (and buf (ygg--job-buffer-p buf))
        (select-window (display-buffer buf ygg-term-display-action))
      (switch-to-buffer buffer norecord))))

(with-eval-after-load 'consult
  (setq consult--buffer-display #'ygg--buffer-display-split))

(provide 'layer-completion)
;;; layer-completion.el ends here
