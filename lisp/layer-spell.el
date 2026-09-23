;;; layer-spell.el --- Spell checking with jinx -*- lexical-binding: t; -*-

;;; Code:


(declare-function jinx-correct "jinx")
(declare-function jinx-next "jinx")
(declare-function jinx-previous "jinx")
(declare-function jinx-mode "jinx")
(declare-function jinx--word-valid-p "jinx")

(defvar ygg-view-map)

(defface ygg-spell-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "#888888"))
    (t :underline t :foreground "#888888"))
  "Muted underline for spelling errors."
  :group 'yggdrasil)

(defun ygg-spell--enable-jinx ()
  "Enable jinx in text-mode derived buffers, excluding Harper-handled modes."
  (unless (or (derived-mode-p 'prog-mode)
              (derived-mode-p 'markdown-mode)
              (derived-mode-p 'gfm-mode)
              (derived-mode-p 'git-commit-mode))
    (jinx-mode 1)))

(when (fboundp 'elpaca)
  (elpaca (jinx :repo "minad/jinx")
    (setq jinx-delay 0.5)
    (setq jinx-face 'ygg-spell-error)

    (add-hook 'text-mode-hook #'ygg-spell--enable-jinx)
    (define-key ygg-view-map "=" #'jinx-correct)
    (yggdrasil-define-keys 'normal
      "] s" #'jinx-next
      "[ s" #'jinx-previous)))

(provide 'layer-spell)
;;; layer-spell.el ends here
