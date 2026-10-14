;;; ygg-git-compare-space.el --- a compare belongs to its space -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ygg-git-compare)

(defvar ygg--space-buffers)
(defvar ygg-space-exclusive-functions)
(defvar ygg-space-cycle-skip-functions)
(declare-function ygg-space--current-id "yggdrasil-spacetree")
(declare-function ygg--space-buffer-owner "layer-sessions")

(defun ygg-git-compare-space--compare-p (buffer)
  (and (buffer-live-p buffer)
       (or (buffer-local-value 'ygg-git-compare--a-spec buffer)
           (buffer-local-value 'ygg-git-compare--list-buffer buffer))
       t))

(defun ygg-git-compare-space--home (buffer)
  (when (and (boundp 'ygg--space-buffers) (fboundp 'ygg--space-buffer-owner))
    (let ((list (or (ygg-git-compare--owner buffer) buffer)))
      (ygg--space-buffer-owner list))))

(defun ygg-git-compare-space--here-p (buffer)
  "Whether BUFFER's compare may show in the selected space: its own, or unclaimed."
  (let ((home (ygg-git-compare-space--home buffer))
        (here (and (fboundp 'ygg-space--current-id) (ygg-space--current-id))))
    (or (null home) (null here) (eql home here))))

(defvar ygg-git-compare-space--opening nil
  "Non-nil while the user opens a compare, which then moves to the selected space.")

(defun ygg-git-compare-space--children (list)
  (seq-filter (lambda (b) (eq (buffer-local-value 'ygg-git-compare--list-buffer b) list))
              (buffer-list)))

(defun ygg-git-compare-space--claim (&optional move)
  "Hold the current buffer in the compare's space, the selected one when MOVE
or the compare has none; MOVE takes the whole compare there."
  (when-let* (((boundp 'ygg--space-buffers))
              (list (or (ygg-git-compare--owner (current-buffer)) (current-buffer)))
              (id (or (and move (fboundp 'ygg-space--current-id) (ygg-space--current-id))
                      (ygg-git-compare-space--home (current-buffer))
                      (and (fboundp 'ygg-space--current-id) (ygg-space--current-id)))))
    (let ((buffers (delete-dups (append (list (current-buffer))
                                        (and move (cons list (ygg-git-compare-space--children list)))))))
      (maphash (lambda (sid held)
                 (unless (eql sid id)
                   (puthash sid (seq-difference held buffers #'eq) ygg--space-buffers)))
               ygg--space-buffers)
      (dolist (buffer buffers)
        (cl-pushnew buffer (gethash id ygg--space-buffers))))))

(defun ygg-git-compare-space--open (orig &rest args)
  (let ((ygg-git-compare-space--opening t))
    (apply orig args)))

(defun ygg-git-compare-space--claim-opening ()
  (when ygg-git-compare-space--opening
    (ygg-git-compare-space--claim t)))

(defun ygg-git-compare-space--only-here (orig &rest args)
  (ygg-git-compare-space--claim-opening)
  (when (ygg-git-compare-space--here-p (current-buffer))
    (apply orig args)))

(defun ygg-git-compare-space--follow-here (orig buffer &rest args)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (ygg-git-compare-space--claim-opening)))
  (if (and (buffer-live-p buffer) (not (ygg-git-compare-space--here-p buffer)))
      (with-current-buffer buffer
        (when ygg-git-compare--timer (cancel-timer ygg-git-compare--timer))
        (setq ygg-git-compare--timer nil))
    (apply orig buffer args)))

(defun ygg-git-compare-space--skip-p (buffer)
  "Whether BUFFER is a compare out of its own space."
  (and (ygg-git-compare-space--compare-p buffer)
       (not (ygg-git-compare-space--here-p buffer))))

(defun ygg-git-compare-space--jump-back-here (orig &rest args)
  (let ((previous (car (window-prev-buffers))))
    (when (or (not (ygg-git-compare-space--compare-p (car previous)))
              (ygg-git-compare-space--here-p (car previous)))
      (apply orig args))))

(add-hook 'ygg-git-compare-mode-hook #'ygg-git-compare-space--claim)
(add-hook 'ygg-space-exclusive-functions #'ygg-git-compare-space--compare-p)
(add-hook 'ygg-space-cycle-skip-functions #'ygg-git-compare-space--skip-p)
(advice-add 'ygg-git-compare-open :around #'ygg-git-compare-space--open)
(advice-add 'ygg-git-compare--follow :around #'ygg-git-compare-space--follow-here)
(advice-add 'ygg-git-compare--show-file :around #'ygg-git-compare-space--only-here)
(advice-add 'ygg-git-compare--jump-back :around #'ygg-git-compare-space--jump-back-here)

(provide 'ygg-git-compare-space)
;;; ygg-git-compare-space.el ends here
