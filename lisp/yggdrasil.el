;;; yggdrasil.el --- Helix-first modal editing, vim/neovim blend -*- lexical-binding: t; -*-

;; Startup loads only the keystroke path; match/ex/rect autoload on first use.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-selection)
(require 'yggdrasil-motions)
(require 'yggdrasil-verbs)
(require 'yggdrasil-actions)
(require 'yggdrasil-leader)
(require 'yggdrasil-localleader)

(autoload 'ygg-match-prefix "yggdrasil-match"
  "Dispatch the m match prefix." t)
(autoload 'ygg-ex "yggdrasil-ex"
  "Read and execute an ex command." t)
(autoload 'ygg-ex-repeat-substitute-all "yggdrasil-ex"
  "Repeat the last :s with the same flags on every line." t)
(autoload 'ygg-rect-enter "yggdrasil-rect"
  "Enter visual-block (rectangle) editing." t)

(yggdrasil-define-keys 'normal
  "m" #'ygg-match-prefix :label "match"
  ":" #'ygg-ex :label "ex"
  "C-v" #'ygg-rect-enter :label "visual block")

(yggdrasil-define-keys 'ygg-goto-map
  "&" #'ygg-ex-repeat-substitute-all :label "repeat last :s everywhere")

(dolist (cmd '(ygg-treesit-expand ygg-treesit-shrink ygg-treesit-prev-sibling
               ygg-treesit-next-sibling ygg-treesit-select-children
               ygg-treesit-select-siblings ygg-treesit-parent-node-end
               ygg-treesit-parent-node-start))
  (autoload cmd "yggdrasil-match" nil t))

(yggdrasil-define-keys 'ygg-selections-map
  "o" #'ygg-treesit-expand :label "expand"
  "i" #'ygg-treesit-shrink :label "shrink"
  "p" #'ygg-treesit-prev-sibling :label "prev sibling"
  "[" #'ygg-treesit-prev-sibling :label "prev sibling"
  "]" #'ygg-treesit-next-sibling :label "next sibling"
  "I" #'ygg-treesit-select-children :label "select children"
  "a" #'ygg-treesit-select-siblings :label "select siblings"
  "e" #'ygg-treesit-parent-node-end :label "parent node end"
  "b" #'ygg-treesit-parent-node-start :label "parent node start")

(provide 'yggdrasil)
;;; yggdrasil.el ends here
