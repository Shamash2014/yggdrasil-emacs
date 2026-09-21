;;; ygg-focus.el --- the selected window framed, the others dimmed -*- lexical-binding: t; -*-

;;; Commentary:
;; One window has your attention.  Its fringes take a grey band on both
;; sides, and every other window's text drops a step towards the ground,
;; so the eye lands where the cursor is without a search.  Faces are
;; remapped per buffer, which is per window whenever a buffer is shown
;; once, the common case.

;;; Code:

(require 'face-remap)
(require 'seq)

(defface ygg-focus-dim '((t :inherit default))
  "Text in a window that is not selected."
  :group 'yggdrasil)

(defface ygg-focus-border '((t :inherit fringe))
  "The fringes of the selected window."
  :group 'yggdrasil)

(defvar-local ygg-focus--dim-cookie nil
  "The remap that dims this buffer, while it is dimmed.")

(defvar-local ygg-focus--border-cookie nil
  "The remap that frames this buffer, while it is framed.")

(defun ygg-focus--dim (buffer)
  "Dim BUFFER and take its frame away; non-nil when either moved."
  (with-current-buffer buffer
    (let ((moved nil))
      (when ygg-focus--border-cookie
        (face-remap-remove-relative ygg-focus--border-cookie)
        (setq ygg-focus--border-cookie nil moved t))
      (unless ygg-focus--dim-cookie
        (setq ygg-focus--dim-cookie
              (face-remap-add-relative 'default 'ygg-focus-dim)
              moved t))
      moved)))

(defun ygg-focus--frame (buffer)
  "Frame BUFFER and take its dimming away; non-nil when either moved."
  (with-current-buffer buffer
    (let ((moved nil))
      (when ygg-focus--dim-cookie
        (face-remap-remove-relative ygg-focus--dim-cookie)
        (setq ygg-focus--dim-cookie nil moved t))
      (unless ygg-focus--border-cookie
        (setq ygg-focus--border-cookie
              (face-remap-add-relative 'fringe 'ygg-focus-border)
              moved t))
      moved)))

(defun ygg-focus--clear (buffer)
  "Take both remaps away from BUFFER; non-nil when either was there."
  (with-current-buffer buffer
    (let ((moved nil))
      (when ygg-focus--dim-cookie
        (face-remap-remove-relative ygg-focus--dim-cookie)
        (setq ygg-focus--dim-cookie nil moved t))
      (when ygg-focus--border-cookie
        (face-remap-remove-relative ygg-focus--border-cookie)
        (setq ygg-focus--border-cookie nil moved t))
      moved)))

(defun ygg-focus--window ()
  "The window the owner is in: the selected one, or behind a picker the
window the picker was opened from.  Nil inside a child frame, where
nothing is to be framed or dimmed."
  (let ((window (selected-window)))
    (cond
     ((frame-parent (window-frame window)) nil)
     ((minibufferp (window-buffer window))
      (let ((from (minibuffer-selected-window)))
        (and (window-live-p from) (not (frame-parent (window-frame from)))
             from)))
     (t window))))

(defun ygg-focus-refresh (&rest _)
  "Frame the selected window's buffer and dim every other shown buffer.
A buffer shown in several windows follows the selected one when it is
among them.  A lone window is framed and nothing is dimmed.  While a
picker holds the minibuffer the window it was opened from stays the
framed one, and inside a child frame nothing moves."
  (when-let* ((ygg-focus-mode)
              ((not (active-minibuffer-window)))
              (window (ygg-focus--window)))
    (let* ((selected (window-buffer window))
           (windows (window-list (window-frame window) 'no-minibuf))
           (others (seq-remove (lambda (b) (eq b selected))
                               (delete-dups (mapcar #'window-buffer windows)))))
      (let ((moved (ygg-focus--frame selected))
            (fn (if (cdr windows) #'ygg-focus--dim #'ygg-focus--clear)))
        (dolist (buffer others)
          (when (funcall fn buffer) (setq moved t)))
        (dolist (buffer (buffer-list))
          (when (and (not (eq buffer selected))
                     (not (memq buffer others))
                     (or (buffer-local-value 'ygg-focus--dim-cookie buffer)
                         (buffer-local-value 'ygg-focus--border-cookie buffer)))
            (when (ygg-focus--clear buffer) (setq moved t))))
        (when moved (ygg-focus--redraw))))))

(defun ygg-focus--redraw ()
  "Ask for one more redisplay, since the hooks run inside the current one.
A remap set during redisplay shows only at the next, and with the cursor
not blinking there is no next until a key is pressed.  Only a refresh
that moved a remap comes here: laying the whole frame out again for a
refresh that changed nothing is work with nothing to show."
  (force-window-update)
  (run-at-time 0 nil #'redisplay))

(defun ygg-focus--lift ()
  "Take every dim away while a picker holds the minibuffer.
The frame on the window the picker came from stays, so the eye still
knows where it will land; the dimming comes back on exit."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'ygg-focus--dim-cookie buffer)
      (with-current-buffer buffer
        (face-remap-remove-relative ygg-focus--dim-cookie)
        (setq ygg-focus--dim-cookie nil)))))

(defun ygg-focus--restore ()
  "Put the dimming back once the picker has closed."
  (run-at-time 0 nil #'ygg-focus-refresh))

(define-minor-mode ygg-focus-mode
  "Frame the selected window and dim the rest."
  :global t :group 'yggdrasil
  (if ygg-focus-mode
      (progn
        (add-hook 'window-selection-change-functions #'ygg-focus-refresh)
        (add-hook 'window-buffer-change-functions #'ygg-focus-refresh)
        (add-hook 'window-configuration-change-hook #'ygg-focus-refresh)
        (add-hook 'minibuffer-setup-hook #'ygg-focus--lift)
        (add-hook 'minibuffer-exit-hook #'ygg-focus--restore)
        (ygg-focus-refresh))
    (remove-hook 'window-selection-change-functions #'ygg-focus-refresh)
    (remove-hook 'window-buffer-change-functions #'ygg-focus-refresh)
    (remove-hook 'window-configuration-change-hook #'ygg-focus-refresh)
    (remove-hook 'minibuffer-setup-hook #'ygg-focus--lift)
    (remove-hook 'minibuffer-exit-hook #'ygg-focus--restore)
    (mapc #'ygg-focus--clear (buffer-list))))

(provide 'ygg-focus)
;;; ygg-focus.el ends here
