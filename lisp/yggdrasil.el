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
(autoload 'ygg-rect-enter "yggdrasil-rect"
  "Enter visual-block (rectangle) editing." t)

(yggdrasil-define-keys 'normal
  "m" #'ygg-match-prefix :label "match"
  ":" #'ygg-ex :label "ex"
  "C-v" #'ygg-rect-enter :label "visual block")

(provide 'yggdrasil)
;;; yggdrasil.el ends here
