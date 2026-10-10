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

(dolist (mode '(markdown-mode gfm-mode))
  (yggdrasil-define-mode-keys mode 'normal "<tab>" #'ygg-diagram-markdown-tab))

(autoload 'ygg-md-paste-image "ygg-md-image" nil t)
(dolist (mode '(markdown-mode gfm-mode org-mode))
  (yggdrasil-localleader-def mode "p" #'ygg-md-paste-image "paste clipboard image"))

(defcustom ygg-markdown-large-size 256000
  "Characters past which a markdown buffer is treated as large."
  :type 'integer
  :group 'markdown)

(defvar ygg-markdown--entering-mode nil)

(defvar-local ygg-markdown--propertize-timer nil)

(defun ygg-markdown--propertize-chunk (buffer)
  "Propertize the next chunk of BUFFER and return non-nil once it is all done."
  (with-current-buffer buffer
    (let* ((start syntax-propertize--done)
           (interrupted (while-no-input
                          (syntax-propertize (min (point-max) (+ start 65536))))))
      (if interrupted
          (progn (setq syntax-propertize--done start) nil)
        (>= syntax-propertize--done (point-max))))))

(defun ygg-markdown--cancel-propertize-timer ()
  (when (timerp ygg-markdown--propertize-timer)
    (cancel-timer ygg-markdown--propertize-timer))
  (setq ygg-markdown--propertize-timer nil))

(defun ygg-markdown--propertize-later (buffer)
  (setq ygg-markdown--propertize-timer
        (run-with-idle-timer
         0.2 nil
         (lambda ()
           (when (and (buffer-live-p buffer)
                      (with-current-buffer buffer (derived-mode-p 'markdown-mode))
                      (not (ygg-markdown--propertize-chunk buffer)))
             (with-current-buffer buffer (ygg-markdown--propertize-later buffer)))))))

(defun ygg-markdown--enter-lazily (mode &rest args)
  (let ((ygg-markdown--entering-mode (current-buffer)))
    (ygg-markdown--cancel-propertize-timer)
    (prog1 (unwind-protect
               (progn
                 (advice-add 'syntax-propertize :around #'ygg-markdown--skip-eager-propertize)
                 (apply mode args))
             (advice-remove 'syntax-propertize #'ygg-markdown--skip-eager-propertize))
      (when (and (> (buffer-size) ygg-markdown-large-size)
                 (< syntax-propertize--done (point-max)))
        (ygg-markdown--propertize-later (current-buffer))
        (add-hook 'kill-buffer-hook #'ygg-markdown--cancel-propertize-timer nil t)))))

(defun ygg-markdown--skip-eager-propertize (propertize pos)
  (if (and ygg-markdown--entering-mode
           (eq ygg-markdown--entering-mode (current-buffer))
           (eq pos (point-max)))
      (progn
        (setq ygg-markdown--entering-mode nil)
        (unless (> (buffer-size) ygg-markdown-large-size)
          (funcall propertize pos)))
    (funcall propertize pos)))

(defun ygg-markdown--propertize-all (&rest _)
  (syntax-propertize (point-max)))

;; markdown-mode propertizes the whole buffer before its hooks run, which costs seconds on big files.
(advice-add 'markdown-mode :around #'ygg-markdown--enter-lazily)
(advice-add 'markdown-imenu-create-nested-index :before #'ygg-markdown--propertize-all)
(advice-add 'markdown-imenu-create-flat-index :before #'ygg-markdown--propertize-all)

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
