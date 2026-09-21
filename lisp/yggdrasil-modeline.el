;;; yggdrasil-modeline.el --- Helix-flavoured mode line -*- lexical-binding: t; -*-

;; Built-ins wrapped: mode-line-format, mode-line-format-right-align,
;; vc-mode, flymake diagnostics, isearch lazy count.
;; Custom: one function per segment, none of them touching a process.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'warnings)
(require 'project)

(declare-function eglot-managed-p "eglot")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flymake--severity "flymake")
(declare-function ygg-space-modeline "yggdrasil-spacetree")
(declare-function diff-hl-changes "diff-hl")
(declare-function diff-hl-changes-from-buffer "diff-hl")

(defvar ygg--modeline-tag)
(defvar ygg--macro-tag)
(defvar diff-hl-mode)

(defface ygg-modeline-pill
  '((((background dark)) :background "#e0e0e0" :foreground "#0f0f0f" :weight bold)
    (t :background "#1f1f1f" :foreground "#ffffff" :weight bold))
  "Inverted block carrying the state word at the head of the line.")

(defface ygg-modeline-pill-fade
  '((((background dark)) :background "#202020" :foreground "#202020")
    (t :background "#e6e6e6" :foreground "#e6e6e6"))
  "One step of grey between the state block and the bar.")

(defface ygg-modeline-lang
  '((((background dark)) :background "#e0e0e0" :foreground "#0f0f0f" :weight bold)
    (t :background "#1f1f1f" :foreground "#ffffff" :weight bold))
  "Inverted block carrying the language name at the tail of the line.")

(defface ygg-modeline-added
  '((((background dark)) :foreground "#98BB6C")
    (t :foreground "#3f6f2a"))
  "Count of lines this buffer adds over the committed file.")

(defface ygg-modeline-removed
  '((((background dark)) :foreground "#D4484B")
    (t :foreground "#9a2020"))
  "Count of lines this buffer removes from the committed file.")

(defface ygg-modeline-path
  '((((background dark)) :foreground "#969696")
    (t :foreground "#4f4f4f"))
  "The file's name under its project root.")

(defcustom ygg-modeline-branch-glyph "⎇"
  "Glyph drawn before the branch name, in a font that has it."
  :type 'string :group 'yggdrasil)

(defun ygg-modeline--dim (string)
  "STRING as it reads in a window that is not the selected one."
  (if (or (not (fboundp 'mode-line-window-selected-p))
          (mode-line-window-selected-p))
      string
    (propertize (substring-no-properties string) 'face 'mode-line-inactive)))

(defun ygg-modeline--marks ()
  "The modified mark or read-only glyph this buffer has earned."
  (cond (buffer-read-only (propertize " ⊘" 'face 'shadow))
        ((buffer-modified-p) (propertize " +" 'face 'mode-line-emphasis))
        (t "")))

(defun ygg-modeline-buffer ()
  "The buffer's name, with a modified mark or a read-only glyph."
  (concat (propertize (buffer-name) 'face 'mode-line-buffer-id)
          (ygg-modeline--marks)))

(defun ygg-modeline-state ()
  "The state block, dimmed where the window is not the selected one."
  (ygg-modeline--dim (or (bound-and-true-p ygg--modeline-tag) "")))

(defun ygg-modeline-fade ()
  "The short fade that carries the state block into the bar."
  (ygg-modeline--dim (propertize "  " 'face 'ygg-modeline-pill-fade)))

(defvar-local ygg-modeline--path nil
  "The file's name under its project root, cached for redisplay.")

(defun ygg-modeline-refresh-path (&rest _)
  "Re-read this buffer's file name against its project into the cache."
  (setq ygg-modeline--path
        (when-let* ((file (buffer-file-name)))
          (let ((root (when-let* ((project (project-current nil)))
                        (expand-file-name (project-root project)))))
            (if (and root (string-prefix-p root (expand-file-name file)))
                (file-relative-name file root)
              (file-name-nondirectory file))))))

(defun ygg-modeline-path ()
  "The cached project-relative file name, or the buffer's own name."
  (if ygg-modeline--path
      (concat "  " (propertize ygg-modeline--path 'face 'ygg-modeline-path)
              (ygg-modeline--marks))
    (concat "  " (ygg-modeline-buffer))))

(add-hook 'find-file-hook #'ygg-modeline-refresh-path t)
(add-hook 'after-save-hook #'ygg-modeline-refresh-path)
(add-hook 'after-revert-hook #'ygg-modeline-refresh-path)

(defun ygg-modeline-mode ()
  "The major mode's language name, on the block at the line's tail."
  (let ((name (string-remove-suffix "-mode" (symbol-name major-mode))))
    (ygg-modeline--dim
     (propertize (concat " " name " ") 'face 'ygg-modeline-lang))))

(defun ygg-modeline-search ()
  "Where the running search stands, when it counts its matches."
  (when (and (bound-and-true-p isearch-mode)
             (bound-and-true-p isearch-lazy-count-total))
    (propertize (format "  %s/%s"
                        (or (bound-and-true-p isearch-lazy-count-current) 0)
                        isearch-lazy-count-total)
                'face 'mode-line-emphasis)))

(defun ygg-modeline-flymake-counts ()
  "Flymake's error and warning counts for this buffer, as a cons."
  (when (and (bound-and-true-p flymake-mode) (fboundp 'flymake--severity))
    (let ((errors 0) (warnings 0)
          (error-level (warning-numeric-level :error))
          (warning-level (warning-numeric-level :warning)))
      (dolist (diag (flymake-diagnostics))
        (let ((severity (flymake--severity (flymake-diagnostic-type diag))))
          (cond ((>= severity error-level) (setq errors (1+ errors)))
                ((>= severity warning-level) (setq warnings (1+ warnings))))))
      (cons errors warnings))))

(defun ygg-modeline-flymake ()
  "Flymake's counts, each shown only when it is above zero."
  (when-let* ((counts (ygg-modeline-flymake-counts)))
    (concat (unless (zerop (car counts))
              (propertize (format "  E%d" (car counts)) 'face 'error))
            (unless (zerop (cdr counts))
              (propertize (format "  W%d" (cdr counts)) 'face 'warning)))))

(defun ygg-modeline-lsp ()
  "A dim tag while a language server is managing this buffer."
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (propertize "  lsp" 'face 'shadow)))

(defvar-local ygg-modeline--branch nil
  "The branch `vc-mode' last named, cached so redisplay runs no process.")

(defun ygg-modeline-refresh-branch (&rest _)
  "Re-read the branch out of `vc-mode' into the cache."
  (setq ygg-modeline--branch
        (when-let* ((raw (and (stringp vc-mode)
                              (string-trim (substring-no-properties vc-mode))))
                    (branch (replace-regexp-in-string
                             "\\`[A-Za-z]+[-:@!?]" "" raw)))
          (unless (string-empty-p branch) branch))))

(defun ygg-modeline-branch ()
  "The cached branch behind its glyph, dimmed."
  (when ygg-modeline--branch
    (propertize (concat "  " ygg-modeline-branch-glyph " " ygg-modeline--branch)
                'face 'shadow)))

(add-hook 'find-file-hook #'ygg-modeline-refresh-branch t)
(add-hook 'after-revert-hook #'ygg-modeline-refresh-branch)
(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'ygg-modeline-refresh-branch))

(defun ygg-modeline--diff-hunks (raw)
  "The working-tree hunks in RAW, whichever shape diff-hl handed back."
  (let ((hunks (if (and (consp raw) (consp (car raw)) (keywordp (caar raw)))
                   (cdr (assq :working raw))
                 raw)))
    (cond ((bufferp hunks)
           (when (and (buffer-live-p hunks)
                      (fboundp 'diff-hl-changes-from-buffer))
             (diff-hl-changes-from-buffer hunks)))
          ((listp hunks) hunks))))

(defun ygg-modeline--diff-tally (hunks)
  "Added and removed line counts over HUNKS, as a cons."
  (let ((added 0) (removed 0))
    (dolist (hunk hunks)
      (pcase hunk
        (`(,_ ,inserts ,deletes ,type)
         (pcase type
           ('insert (cl-incf added inserts))
           ('delete (cl-incf removed deletes))
           (_ (cl-incf added inserts) (cl-incf removed deletes))))
        (`(,_ ,length ,type)
         (pcase type
           ('insert (cl-incf added length))
           ('delete (cl-incf removed length))
           (_ (cl-incf added length) (cl-incf removed length))))))
    (cons added removed)))

(defun ygg-modeline-diff-counts ()
  "This buffer's added and removed line counts, as a cons, from diff-hl."
  (when (and (bound-and-true-p diff-hl-mode) (buffer-file-name)
             (fboundp 'diff-hl-changes))
    (ygg-modeline--diff-tally (ygg-modeline--diff-hunks (diff-hl-changes)))))

(defvar-local ygg-modeline--diff-counts nil
  "The counts diff-hl last gave, cached so redisplay runs no process.")

(defun ygg-modeline-refresh-diff-counts (&rest _)
  "Re-tally this buffer's hunks into the cache."
  (setq ygg-modeline--diff-counts (ignore-errors (ygg-modeline-diff-counts))))

(defun ygg-modeline-diff ()
  "Both counters of uncommitted lines, zeros included, as Rune shows them."
  (when ygg-modeline--diff-counts
    (concat (propertize (format "  ⊕ %d" (car ygg-modeline--diff-counts))
                        'face 'ygg-modeline-added)
            (propertize (format " ⊖ %d" (cdr ygg-modeline--diff-counts))
                        'face 'ygg-modeline-removed))))

(add-hook 'after-save-hook #'ygg-modeline-refresh-diff-counts)
(with-eval-after-load 'diff-hl
  (advice-add 'diff-hl-update :after #'ygg-modeline-refresh-diff-counts))

(defvar-local ygg-modeline--lines nil
  "The line count last shown, with the modification tick it was true at.")

(defun ygg-modeline-lines ()
  "How many lines the buffer holds, counted once per change."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (and ygg-modeline--lines (eq (car ygg-modeline--lines) tick))
      (setq ygg-modeline--lines
            (cons tick (count-lines (point-min) (point-max)))))
    (propertize (format "  %d lines" (cdr ygg-modeline--lines))
                'face 'shadow)))

(defun ygg--setup-modeline ()
  "Install the Helix-flavoured line as the default `mode-line-format'."
  (setq-default
   mode-line-format
   `("%e"
     (:eval (ygg-modeline-state))
     ygg--macro-tag
     (:eval (ygg-modeline-fade))
     (:eval (ygg-modeline-path))
     (:eval (ygg-modeline-branch))
     (:eval (ygg-modeline-diff))
     (:eval (ygg-modeline-search))
     (:eval (ygg-modeline-flymake))
     (:eval (ygg-modeline-lsp))
     mode-line-misc-info
     (:eval (when (fboundp 'ygg-space-modeline) (ygg-space-modeline)))
     ,@(and (boundp 'mode-line-right-align-edge) '(mode-line-format-right-align))
     "  %l:%c"
     (:eval (ygg-modeline-lines))
     "  "
     (:eval (ygg-modeline-mode)))))

(ygg--setup-modeline)

(provide 'yggdrasil-modeline)
;;; yggdrasil-modeline.el ends here
