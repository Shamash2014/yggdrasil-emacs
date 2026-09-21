;;; yggdrasil-rect.el --- Visual block on built-in rectangles -*- lexical-binding: t; -*-

;; Built-ins wrapped: rectangle-mark-mode, delete/copy/yank/string-rectangle,
;; string-insert-rectangle, emulation-mode-map-alists (via ygg--emulation-alist).

;;; Code:

(require 'yggdrasil-core)
(require 'rect)

(defvar-local ygg--rect-p nil)

(defvar ygg-rect-map (make-sparse-keymap))

;; emulation-mode-map-alists holds the SYMBOL ygg--emulation-alist, so mutating
;; its value here is picked up automatically; pushing to the front wins ties.
(push (cons 'ygg--rect-p ygg-rect-map) ygg--emulation-alist)

(defun ygg--rect-exit ()
  (rectangle-mark-mode -1)
  (deactivate-mark)
  (setq ygg--rect-p nil)
  (ygg-normal-state))

;;;###autoload
(defun ygg-rect-enter ()
  "Enter visual-block (rectangle) editing on built-in rectangles."
  (interactive)
  (push-mark (point) t nil)
  (rectangle-mark-mode 1)
  (setq ygg--rect-p t))

(defun ygg--rect-include-cursor-column ()
  "Widen the rectangle's right edge one column, matching the engine's
inclusive-cursor selection model (Emacs rectangles exclude it)."
  (let ((pcol (current-column))
        (mcol (save-excursion (goto-char (mark)) (current-column))))
    (if (>= pcol mcol)
        (move-to-column (1+ pcol))
      (save-excursion
        (goto-char (mark))
        (move-to-column (1+ mcol))
        (set-mark (point))))))

(defun ygg-rect-delete ()
  (interactive)
  (ygg--rect-include-cursor-column)
  (call-interactively #'delete-rectangle)
  (ygg--rect-exit))

(defun ygg-rect-copy ()
  (interactive)
  (ygg--rect-include-cursor-column)
  (call-interactively #'copy-rectangle-as-kill)
  (ygg--rect-exit))

(defun ygg-rect-paste ()
  (interactive)
  (call-interactively #'yank-rectangle)
  (ygg--rect-exit))

(defun ygg-rect-string ()
  (interactive)
  (ygg--rect-include-cursor-column)
  (call-interactively #'string-rectangle)
  (ygg--rect-exit))

(defun ygg-rect-append ()
  "Insert after the rectangle's right edge; built-ins have no append primitive."
  (interactive)
  (let* ((mcol (save-excursion (goto-char (mark)) (current-column)))
         (col (max mcol (current-column)))
         (pt (progn (move-to-column col t) (point))))
    (save-excursion (goto-char (mark)) (move-to-column col t) (set-mark (point)))
    (goto-char pt))
  (call-interactively #'string-insert-rectangle)
  (ygg--rect-exit))

(defun ygg-rect-quit ()
  (interactive)
  (ygg--rect-exit))

(define-key ygg-rect-map "h" #'backward-char)
(define-key ygg-rect-map "j" #'next-line)
(define-key ygg-rect-map "k" #'previous-line)
(define-key ygg-rect-map "l" #'forward-char)
(define-key ygg-rect-map "d" #'ygg-rect-delete)
(define-key ygg-rect-map "y" #'ygg-rect-copy)
(define-key ygg-rect-map "p" #'ygg-rect-paste)
(define-key ygg-rect-map "c" #'ygg-rect-string)
(define-key ygg-rect-map "I" #'ygg-rect-string)
(define-key ygg-rect-map "A" #'ygg-rect-append)
(define-key ygg-rect-map (kbd "<escape>") #'ygg-rect-quit)
(define-key ygg-rect-map (kbd "C-g") #'ygg-rect-quit)
(define-key ygg-rect-map "v" #'ygg-rect-quit)

(provide 'yggdrasil-rect)
;;; yggdrasil-rect.el ends here
