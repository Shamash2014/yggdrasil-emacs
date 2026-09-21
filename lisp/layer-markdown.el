;;; layer-markdown.el --- inline diagrams & math in markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; The modal layer's way into the diagram renderer: markdown and gfm buffers
;; get the whole-buffer toggle on their localleader.  The renderer itself is
;; ygg-diagram, which knows nothing of the layer.

;;; Code:

(require 'yggdrasil-localleader)
(require 'ygg-diagram)

(dolist (mode '(markdown-mode gfm-mode))
  (yggdrasil-localleader-def mode "m" #'ygg-diagram-toggle "diagrams & math"))

(provide 'layer-markdown)
;;; layer-markdown.el ends here
