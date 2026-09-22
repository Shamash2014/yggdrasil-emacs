;;; aob-trace-vui.el --- vui.el-backed trace renderer (prototype) -*- lexical-binding: t; -*-

;;; Commentary:
;; A prototype trace renderer built on vui.el, selectable per session via
;; `aob-trace-vui'.  It reuses `aob-trace--block' verbatim: only the draw
;; MECHANISM changes — vui reconciliation instead of the hand-rolled
;; incremental diff.  The built-in renderer stays the default until this
;; proves two things live: the yggdrasil modal layer survives on a
;; `vui-mode' buffer, and streaming keeps the view stable (no jump-to-top).

;;; Code:

(require 'aob)
(require 'aob-trace)
(require 'ygg-ui)
(require 'vui nil t)

(declare-function vui-defcomponent "vui")
(declare-function vui-component "vui")
(declare-function vui-mount "vui")
(declare-function vui-update-props "vui")
(declare-function vui-fragment "vui")
(declare-function vui-text "vui")
(declare-function vui-mode "vui")
(defvar aob-trace--session-id)

(defcustom aob-trace-renderer 'builtin
  "Engine that draws agent traces: the hand-rolled `builtin' or `vui'.
The vui path is a prototype; flip to `vui' only to A/B it."
  :type '(choice (const builtin) (const vui)) :group 'aob)

(defvar-local aob-trace-vui--instance nil
  "The mounted vui root instance backing this trace buffer.")

(when (featurep 'vui)
  (vui-defcomponent aob-trace-view (session)
    "Render SESSION's trace: one keyed text block per top-level event.
The block strings come straight from `aob-trace--block', so glyphs,
faces and the `aob-event' navigation properties are unchanged — vui only
owns layout and diffing."
    :render
    (let ((s (aob-session-get session)))
      (apply
       #'vui-fragment
       (and s (delq nil
                    (mapcar
                     (lambda (ev)
                       (unless (plist-get ev :parent)
                         (vui-text (concat (aob-trace--block s ev) "\n")
                           :key (plist-get ev :seq))))
                     (reverse (seq-take (aob-session-events s)
                                        aob-trace-limit)))))))))

(define-derived-mode aob-trace-vui-mode vui-mode "aob-trace"
  "Operation trace of one agent session, drawn by vui.el."
  (ygg-ui-plain-layout)
  (setq truncate-lines nil)
  (visual-line-mode 1))

(defun aob-trace-vui--render (&optional _force)
  "Push the session's latest events into the mounted view.
Registered per buffer, so the coalescing tick loop calls it with this
trace buffer current."
  (when-let* ((s (aob-session-get aob-trace--session-id))
              (inst aob-trace-vui--instance))
    (vui-update-props inst (list :session (aob-session-id s)))))

(defun aob-trace-vui-buffer (s)
  "Return S's vui-rendered trace buffer, mounting the view once."
  (let ((buf (get-buffer-create (format "trace:%s" (aob-session-name s)))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aob-trace-vui-mode) (aob-trace-vui-mode))
      (setq aob-trace--session-id (aob-session-id s)
            aob-buffer-session-id (aob-session-id s))
      (setq aob-trace-vui--instance
            (vui-mount (vui-component 'aob-trace-view :session (aob-session-id s))
                       (buffer-name buf)))
      (aob-register-view buf #'aob-trace-vui--render)
      (goto-char (point-max)))
    buf))

;;;###autoload
(defun aob-trace-vui (s)
  "Open S's trace drawn by vui.el, to A/B against the built-in `aob-trace'."
  (interactive (list (aob-target)))
  (unless (featurep 'vui) (user-error "aob: vui.el is not installed"))
  (ygg-ui-show (aob-trace-vui-buffer s)))

(provide 'aob-trace-vui)
;;; aob-trace-vui.el ends here
