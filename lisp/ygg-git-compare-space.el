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

(defun ygg-git-compare-space--claim ()
  (when-let* (((boundp 'ygg--space-buffers))
              (id (or (ygg-git-compare-space--home (current-buffer))
                      (and (fboundp 'ygg-space--current-id) (ygg-space--current-id)))))
    (let ((buffer (current-buffer)))
      (maphash (lambda (sid buffers)
                 (unless (eql sid id)
                   (puthash sid (delq buffer buffers) ygg--space-buffers)))
               ygg--space-buffers)
      (cl-pushnew buffer (gethash id ygg--space-buffers)))))

(defun ygg-git-compare-space--only-here (orig &rest args)
  (when (ygg-git-compare-space--here-p (current-buffer))
    (apply orig args)))

(defun ygg-git-compare-space--follow-here (orig buffer &rest args)
  (if (and (buffer-live-p buffer) (not (ygg-git-compare-space--here-p buffer)))
      (with-current-buffer buffer
        (when ygg-git-compare--timer (cancel-timer ygg-git-compare--timer))
        (setq ygg-git-compare--timer nil))
    (apply orig buffer args)))

(defun ygg-git-compare-space--jump-back-here (orig &rest args)
  (let ((previous (car (window-prev-buffers))))
    (when (or (not (ygg-git-compare-space--compare-p (car previous)))
              (ygg-git-compare-space--here-p (car previous)))
      (apply orig args))))

(add-hook 'ygg-git-compare-mode-hook #'ygg-git-compare-space--claim)
(add-hook 'ygg-space-exclusive-functions #'ygg-git-compare-space--compare-p)
(add-hook 'ygg-space-cycle-skip-functions #'ygg-git-compare-space--compare-p)
(advice-add 'ygg-git-compare--follow :around #'ygg-git-compare-space--follow-here)
(advice-add 'ygg-git-compare--show-file :around #'ygg-git-compare-space--only-here)
(advice-add 'ygg-git-compare--jump-back :around #'ygg-git-compare-space--jump-back-here)

(provide 'ygg-git-compare-space)
;;; ygg-git-compare-space.el ends here
