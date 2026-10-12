;;; aob-new-space-trace-tests.el --- a new agent's trace replaces its space's landing listing -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'yggdrasil)
(eval-and-compile
  (defvar ygg-space-state-functions nil)
  (defvar ygg-space-detail-functions nil)
  (defvar ygg-leader-open-map (make-sparse-keymap))
  (defvar aob-acp-persist-file)
  (setq aob-acp-persist-file (make-temp-file "aob-new-space-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'layer-aob)

(defmacro aob-new-space-tests--with (dir var &rest body)
  (declare (indent 2))
  `(let* ((,dir (file-name-as-directory (make-temp-file "aob-new-space-" t)))
          (,var (aob-create-session :id "acp:new-space" :backend 'acp :name "new"
                                    :project ,dir :dir ,dir :state 'idle))
          (listing (dired-noselect ,dir)))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (set-window-buffer (selected-window) listing)
           (cl-letf (((symbol-function 'aob-trace-buffer)
                      (lambda (_) (get-buffer-create " *aob-new-space-trace*"))))
             ,@body))
       (kill-buffer listing)
       (when (get-buffer " *aob-new-space-trace*") (kill-buffer " *aob-new-space-trace*"))
       (aob-remove-session ,var)
       (delete-directory ,dir t))))

(ert-deftest aob-new-space-trace-takes-the-landing-listing ()
  (aob-new-space-tests--with dir s
    (let ((noninteractive nil)
          (ygg-aob--compose-sending t))
      (ygg-aob--show-trace s))
    (should (= 1 (length (window-list nil 'no-minibuf))))
    (should (equal " *aob-new-space-trace*" (buffer-name (window-buffer (selected-window)))))))

(ert-deftest aob-new-space-trace-leaves-another-folders-listing ()
  (aob-new-space-tests--with dir s
    (let ((other (dired-noselect temporary-file-directory)))
      (set-window-buffer (selected-window) other)
      (let ((noninteractive nil))
        (ygg-aob--show-trace s))
      (should (get-buffer-window other)))))

(provide 'aob-new-space-trace-tests)
