;;; aob-compose-send-tests.el --- Sending from a floating box -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'aob)

;; a floating box's window is dedicated to the draft, so a send that shows
;; a buffer must not run there, or the box can never be closed
(ert-deftest aob-compose-send-from-a-float-runs-in-the-window-under-it ()
  (let* ((draft (generate-new-buffer "compose:test"))
         (shown (generate-new-buffer "trace:test"))
         (s (aob-session--create :id "aob-send-float" :name "t" :state 'idle))
         (box (selected-window))
         (home (split-window box))
         seen)
    (unwind-protect
        (progn
          (puthash (aob-session-id s) s aob--sessions)
          (set-window-buffer box draft)
          (set-window-dedicated-p box t)
          (with-current-buffer draft
            (aob-compose-mode)
            (setq aob-compose--target (aob-session-id s))
            (insert "hello"))
          (cl-letf (((symbol-function 'aob-compose--home-window)
                     (lambda (_draft) home))
                    ((symbol-function 'aob-prompt)
                     (lambda (&rest _)
                       (setq seen aob-compose--target)
                       (switch-to-buffer shown)))
                    ((symbol-function 'aob-compose-close) #'ignore))
            (with-current-buffer draft (aob-compose-send)))
          (should (eq (window-buffer home) shown))
          (should (eq (window-buffer box) draft))
          (should (equal seen (aob-session-id s))))
      (remhash (aob-session-id s) aob--sessions)
      (set-window-dedicated-p box nil)
      (when (window-live-p home) (delete-window home))
      (kill-buffer draft)
      (kill-buffer shown))))
