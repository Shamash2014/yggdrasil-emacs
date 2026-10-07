;;; ygg-comment.el --- the look every comment shares -*- lexical-binding: t; -*-

;;; Commentary:
;; One face for the words of a comment, whether it is held on a trace,
;; written on a compare or noted on a plan.  Each surface's own faces
;; inherit it and add only what is theirs.

;;; Code:

(defface ygg-comment
  '((((background dark)) :background "#1c1c1c" :extend t)
    (t :background "#ebe7dd" :extend t))
  "What a comment's words look like, in a trace, a compare and a plan."
  :group 'faces)

(provide 'ygg-comment)
;;; ygg-comment.el ends here
