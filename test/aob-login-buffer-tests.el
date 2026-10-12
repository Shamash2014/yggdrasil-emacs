;;; aob-login-buffer-tests.el --- The terminal login's buffer after it exits -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(require 'ert)
(require 'yggdrasil nil t)
(require 'aob)
(require 'aob-acp)
(require 'term)

(defun aob-login-buffer-tests--run (script)
  (let (result buf)
    (cl-letf (((symbol-function 'aob-acp--proc-of) #'ignore)
              ((symbol-function 'process-get) #'ignore)
              ((symbol-function 'aob-session-ref) (lambda (&rest _) "claude"))
              ((symbol-function 'aob-acp--auth-argv) (lambda (&rest _) (list "sh" "-c" script)))
              (aob-acp-command-function #'identity))
      (setq buf (aob-acp--auth-terminal (aob-session--create :project default-directory) nil (lambda (ok) (setq result (list ok)))))
      (with-timeout (5 nil)
        (while (not result) (accept-process-output nil 0.05))))
    (cons result buf)))

(ert-deftest aob-login-buffer-failed-login-leaves-a-live-line-mode-buffer ()
  (let* ((r (aob-login-buffer-tests--run "echo nope; exit 1"))
         (buf (cdr r)))
    (unwind-protect
        (progn
          (should (equal (car r) '(nil)))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (term-in-line-mode))
            (should-not (get-buffer-process buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest aob-login-buffer-finished-login-closes-its-buffer ()
  (let* ((r (aob-login-buffer-tests--run "exit 0")))
    (should (equal (car r) '(t)))
    (should-not (buffer-live-p (cdr r)))))
