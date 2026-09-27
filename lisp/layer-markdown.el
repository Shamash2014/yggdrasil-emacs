;;; layer-markdown.el --- inline diagrams, math and pasted images in markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; The modal layer's way into the diagram renderer and the image paster:
;; markdown and gfm buffers get the whole-buffer toggle and the paste on
;; their localleader, org buffers the paste.  ygg-diagram and ygg-md-image
;; know nothing of the layer.

;;; Code:

(require 'yggdrasil-localleader)
(require 'ygg-diagram)

(dolist (mode '(markdown-mode gfm-mode))
  (yggdrasil-localleader-def mode "m" #'ygg-diagram-toggle "diagrams & math"))

(autoload 'ygg-md-paste-image "ygg-md-image" nil t)
(dolist (mode '(markdown-mode gfm-mode org-mode))
  (yggdrasil-localleader-def mode "p" #'ygg-md-paste-image "paste clipboard image"))

(provide 'layer-markdown)
;;; layer-markdown.el ends here
