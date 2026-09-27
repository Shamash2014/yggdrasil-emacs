;;; aob-diag-push.el --- Tell the agent what its edits broke -*- lexical-binding: t; -*-

;;; Code:

(require 'aob)
(require 'aob-acp)

(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))
(declare-function flymake--severity "flymake" (type))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(defvar flycheck-current-errors)

(defcustom aob-diag-push t
  "Non-nil to send the errors in the files an agent edited with the next
message from compose."
  :type 'boolean :group 'aob)

(defcustom aob-diag-push-max-lines 20
  "At most this many errors ride one message."
  :type 'natnum :group 'aob)

(defun aob-diag-push--paths (s u)
  "The absolute paths the tool call update U of session S edits."
  (let ((dir (or (aob-session-dir s) default-directory))
        paths)
    (dolist (loc (plist-get u :locations))
      (when-let* ((p (plist-get loc :path))) (push p paths)))
    (dolist (c (plist-get u :content))
      (when-let* ((p (and (equal (plist-get c :type) "diff") (plist-get c :path))))
        (push p paths)))
    (mapcar (lambda (p) (expand-file-name p dir)) paths)))

(defun aob-diag-push--note (s method params)
  "Remember the files an edit tool call in S touches."
  (when-let* (((equal method "session/update"))
              (u (plist-get params :update))
              ((member (plist-get u :sessionUpdate) '("tool_call" "tool_call_update")))
              ((equal (or (plist-get u :kind)
                          (plist-get (gethash (plist-get u :toolCallId)
                                              (aob-acp--tools s))
                                     :kind))
                      "edit")))
    (aob-diag-push--add s (aob-diag-push--paths s u))))

(defun aob-diag-push--add (s paths)
  (dolist (p paths)
    (unless (member p (aob-session-ref s :diag-touched))
      (aob-session-put s :diag-touched
                       (cons p (aob-session-ref s :diag-touched))))))

(defun aob-diag-push--errors ()
  "The current buffer's error-severity diagnostics as (LINE . MESSAGE)."
  (cond
   ((bound-and-true-p flymake-mode)
    (let ((floor (flymake--severity :error)))
      (delq nil
            (mapcar (lambda (d)
                      (when (>= (flymake--severity (flymake-diagnostic-type d)) floor)
                        (cons (line-number-at-pos (flymake-diagnostic-beg d) t)
                              (substring-no-properties
                               (flymake-diagnostic-text d)))))
                    (flymake-diagnostics)))))
   ((bound-and-true-p flycheck-mode)
    (delq nil
          (mapcar (lambda (e)
                    (when (eq (flycheck-error-level e) 'error)
                      (cons (or (flycheck-error-line e) 1)
                            (substring-no-properties
                             (or (flycheck-error-message e) "")))))
                  flycheck-current-errors)))))

(defun aob-diag-push--fresh-buffer (path)
  "The buffer visiting PATH when it holds what is on disk, else nil."
  (when-let* ((buf (find-buffer-visiting path))
              ((buffer-live-p buf))
              ((not (buffer-modified-p buf)))
              ((verify-visited-file-modtime buf)))
    buf))

(defun aob-diag-push--lines (s paths)
  "The first error lines for PATHS, each named relative to S's folder."
  (let ((dir (aob-session-dir s))
        lines)
    (dolist (path (sort (copy-sequence paths) #'string<))
      (when-let* ((buf (aob-diag-push--fresh-buffer path)))
        (let ((name (if (and dir (file-in-directory-p path dir))
                        (file-relative-name path dir)
                      (abbreviate-file-name path))))
          (dolist (e (sort (with-current-buffer buf (aob-diag-push--errors))
                           (lambda (a b) (< (car a) (car b)))))
            (push (format "%s:%d: %s" name (car e)
                          (car (split-string (cdr e) "\n")))
                  lines)))))
    (take aob-diag-push-max-lines (nreverse lines))))

(defun aob-diag-push-compose (text)
  "TEXT followed by the errors now in the files the target session edited."
  (when-let* ((id (and (stringp aob-compose--target) aob-compose--target))
              (s (aob-session-get id))
              (paths (aob-session-ref s :diag-touched)))
    (aob-session-put s :diag-touched nil)
    (when-let* (aob-diag-push
                (lines (aob-diag-push--lines s paths)))
      (concat text "\n\n<diagnostics>\nErrors the checker reports in files you edited:\n"
              (mapconcat #'identity lines "\n")
              "\n</diagnostics>"))))

(add-hook 'aob-acp-notification-functions #'aob-diag-push--note)
(add-hook 'aob-compose-before-send-functions #'aob-diag-push-compose t)

(provide 'aob-diag-push)
;;; aob-diag-push.el ends here
