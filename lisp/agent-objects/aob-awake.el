;;; aob-awake.el --- keep the machine awake while an agent works -*- lexical-binding: t; -*-

;;; Commentary:
;; One system sleep block while any session works; waiting on you is not working.

;;; Code:

(require 'seq)
(require 'aob)

(require 'system-sleep nil t)

(declare-function system-sleep-block-sleep "system-sleep")
(declare-function system-sleep-unblock-sleep "system-sleep")

(defvar aob-keep-awake-mode)
(defvar aob-awake--token nil)
(defvar aob-awake--warned nil)

(defun aob-awake--warn (why)
  (unless aob-awake--warned
    (setq aob-awake--warned t)
    (message "aob: cannot keep the machine awake (%s)" why)))

(defun aob-awake--working-p ()
  (seq-some (lambda (s) (eq (aob-session-state s) 'working)) (aob-sessions)))

(defun aob-awake--take ()
  (if (not (fboundp 'system-sleep-block-sleep))
      (aob-awake--warn "system-sleep not loaded")
    (condition-case err
        (if-let* ((token (system-sleep-block-sleep "aob agent working" t)))
            (setq aob-awake--token token)
          (aob-awake--warn "no block granted"))
      (error (aob-awake--warn (error-message-string err))))))

(defun aob-awake--release ()
  (when-let* ((token aob-awake--token))
    (setq aob-awake--token nil)
    (condition-case nil
        (system-sleep-unblock-sleep token)
      (error nil))))

(defun aob-awake--sync (&rest _)
  (let ((want (and aob-keep-awake-mode (aob-awake--working-p))))
    (cond ((and want (not aob-awake--token)) (aob-awake--take))
          ((and (not want) aob-awake--token) (aob-awake--release)))))

;;;###autoload
(define-minor-mode aob-keep-awake-mode
  "Keep the machine from idle-sleeping while any agent is working."
  :global t
  :group 'aob
  (if aob-keep-awake-mode
      (progn
        (add-hook 'aob-state-change-hook #'aob-awake--sync)
        (add-hook 'aob-session-removed-hook #'aob-awake--sync)
        (aob-awake--sync))
    (remove-hook 'aob-state-change-hook #'aob-awake--sync)
    (remove-hook 'aob-session-removed-hook #'aob-awake--sync)
    (aob-awake--sync)))

(provide 'aob-awake)
;;; aob-awake.el ends here
