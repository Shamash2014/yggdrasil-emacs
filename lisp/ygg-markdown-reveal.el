;;; ygg-markdown-reveal.el --- raw markup of the element at point while inserting -*- lexical-binding: t; -*-

;;; Commentary:
;; With `markdown-hide-markup' on, the markup of the emphasis, code, link or
;; heading under point shows while the buffer is in insert state and hides
;; again once point leaves the element or insert state ends.  Only text
;; properties change, never the text.

;;; Code:

(require 'jit-lock)

(defvar markdown-hide-markup)
(defvar markdown-regex-bold)
(defvar markdown-regex-italic)
(defvar markdown-regex-gfm-italic)
(defvar markdown-regex-strike-through)
(defvar markdown-regex-code)
(defvar markdown-regex-link-inline)
(defvar markdown-regex-wiki-link)
(defvar markdown-regex-header-atx)
(declare-function markdown-code-block-at-point-p "markdown-mode")
(defvar ygg-insert-entry-hook)
(defvar ygg-insert-exit-hook)

(defvar-local ygg-markdown-reveal--region nil)
(defvar-local ygg-markdown-reveal--last-point nil)
(defvar-local ygg-markdown-reveal--last-tick nil)

(defun ygg-markdown-reveal--candidates ()
  `((,markdown-regex-bold . 2)
    (,markdown-regex-italic . 1)
    (,markdown-regex-gfm-italic . 1)
    (,markdown-regex-strike-through . 2)
    (,markdown-regex-code . 1)
    (,markdown-regex-link-inline . 0)
    (,markdown-regex-wiki-link . 1)))

(defun ygg-markdown-reveal--bounds ()
  (unless (markdown-code-block-at-point-p)
    (save-excursion
      (let ((pos (point))
            (limit (line-end-position))
            best)
        (dolist (candidate (ygg-markdown-reveal--candidates))
          (goto-char (line-beginning-position))
          (while (re-search-forward (car candidate) limit t)
            (let ((beg (match-beginning (cdr candidate)))
                  (end (match-end (cdr candidate))))
              (when (and (<= beg pos end)
                         (or (null best) (> (- end beg) (- (cdr best) (car best)))))
                (setq best (cons beg end)))
              (goto-char (max (1+ beg) (match-beginning 0))))))
        (or best
            (progn (goto-char (line-beginning-position))
                   (when (looking-at markdown-regex-header-atx)
                     (cons (match-beginning 0) (match-end 0)))))))))

(defun ygg-markdown-reveal--strip (beg end)
  (with-silent-modifications
    (let ((pos beg))
      (while (< pos end)
        (let ((next (or (next-property-change pos nil end) end)))
          (when (eq (get-text-property pos 'invisible) 'markdown-markup)
            (remove-text-properties pos next '(invisible nil)))
          (when (equal (get-text-property pos 'display) "")
            (remove-text-properties pos next '(display nil)))
          (setq pos next))))))

(defun ygg-markdown-reveal--hide ()
  (when ygg-markdown-reveal--region
    (let ((beg (car ygg-markdown-reveal--region))
          (end (cdr ygg-markdown-reveal--region)))
      (setq ygg-markdown-reveal--region nil)
      (when (< beg end)
        (font-lock-flush beg end)
        (font-lock-ensure beg end))
      (set-marker beg nil)
      (set-marker end nil))))

(defun ygg-markdown-reveal--update ()
  (let ((point (point))
        (tick (buffer-chars-modified-tick)))
    (unless (and (eql point ygg-markdown-reveal--last-point)
                 (eql tick ygg-markdown-reveal--last-tick))
      (setq ygg-markdown-reveal--last-point point
            ygg-markdown-reveal--last-tick tick)
      (let ((bounds (and markdown-hide-markup (ygg-markdown-reveal--bounds))))
        (unless (and bounds ygg-markdown-reveal--region
                     (= (car bounds) (car ygg-markdown-reveal--region))
                     (= (cdr bounds) (cdr ygg-markdown-reveal--region)))
          (ygg-markdown-reveal--hide)
          (when bounds
            (setq ygg-markdown-reveal--region
                  (cons (copy-marker (car bounds)) (copy-marker (cdr bounds) t)))))
        (when ygg-markdown-reveal--region
          (ygg-markdown-reveal--strip (car ygg-markdown-reveal--region)
                                      (cdr ygg-markdown-reveal--region)))))))

(defun ygg-markdown-reveal--forget-last ()
  (setq ygg-markdown-reveal--last-point nil
        ygg-markdown-reveal--last-tick nil))

(defun ygg-markdown-reveal--after-fontify (beg end)
  (when (and ygg-markdown-reveal--region
             (< beg (cdr ygg-markdown-reveal--region))
             (> end (car ygg-markdown-reveal--region)))
    (ygg-markdown-reveal--strip (car ygg-markdown-reveal--region)
                                (cdr ygg-markdown-reveal--region))))

(defun ygg-markdown-reveal--enter ()
  (add-hook 'post-command-hook #'ygg-markdown-reveal--update nil t)
  (add-hook 'jit-lock-functions #'ygg-markdown-reveal--after-fontify 90 t)
  (ygg-markdown-reveal--forget-last)
  (ygg-markdown-reveal--update))

(defun ygg-markdown-reveal--leave ()
  (remove-hook 'post-command-hook #'ygg-markdown-reveal--update t)
  (remove-hook 'jit-lock-functions #'ygg-markdown-reveal--after-fontify t)
  (ygg-markdown-reveal--hide)
  (ygg-markdown-reveal--forget-last))

(defun ygg-markdown-reveal-setup ()
  "Reveal markup at point in this buffer while it is in insert state."
  (add-hook 'ygg-insert-entry-hook #'ygg-markdown-reveal--enter nil t)
  (add-hook 'ygg-insert-exit-hook #'ygg-markdown-reveal--leave nil t))

(provide 'ygg-markdown-reveal)
;;; ygg-markdown-reveal.el ends here
