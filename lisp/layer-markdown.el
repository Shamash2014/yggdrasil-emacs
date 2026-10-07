;;; layer-markdown.el --- inline diagrams, math and pasted images in markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; The modal layer's way into the diagram renderer and the image paster:
;; markdown and gfm buffers get the whole-buffer toggle, the raw-fences
;; toggle and the paste on their localleader, org buffers the paste.
;; Plans in .aob/plans open in ygg-plan-mode, loaded with the first one.
;; ygg-diagram and ygg-md-image know nothing of the layer.

;;; Code:

(require 'yggdrasil-localleader)
(require 'ygg-diagram)
(require 'ygg-markdown-fences)

(dolist (mode '(markdown-mode gfm-mode))
  (yggdrasil-localleader-def mode "m" #'ygg-diagram-toggle "diagrams & math"))

(dolist (mode '(markdown-mode gfm-mode))
  (yggdrasil-localleader-def mode "f" #'ygg-markdown-fences-toggle "raw code fences"))

(autoload 'ygg-md-paste-image "ygg-md-image" nil t)
(dolist (mode '(markdown-mode gfm-mode org-mode))
  (yggdrasil-localleader-def mode "p" #'ygg-md-paste-image "paste clipboard image"))

(autoload 'ygg-plan-mode "ygg-plan" nil t)

(defconst ygg-markdown--plan-entry '("/\\.aob/plans/[^/]+\\.md\\'" . ygg-plan-mode))

(defun ygg-markdown--plans-first ()
  "Put the plan pattern ahead of markdown's own `.md' entry."
  (setq auto-mode-alist (cons ygg-markdown--plan-entry
                              (delete ygg-markdown--plan-entry auto-mode-alist))))

(ygg-markdown--plans-first)
(with-eval-after-load 'markdown-mode (ygg-markdown--plans-first))

(provide 'layer-markdown)
;;; layer-markdown.el ends here
