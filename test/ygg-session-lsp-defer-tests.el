;;; ygg-session-lsp-defer-tests.el --- eglot waits for version control on restored buffers -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'yggdrasil)
(require 'layer-sessions)
(require 'layer-lsp)

(defvar ygg-session-lsp-defer-tests--seen nil)

(defmacro ygg-session-lsp-defer-tests--in-repo (root-var file-var &rest body)
  "Run BODY with ROOT-VAR a fresh git repository and FILE-VAR a file in its lib/."
  (declare (indent 2))
  `(let* ((,root-var (file-name-as-directory (file-truename (make-temp-file "ygg-lspd" t))))
          (,file-var (expand-file-name "lib/main.dart" ,root-var))
          (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment))
          (ygg-session--deferred-vc nil)
          (ygg-session-lsp-defer-tests--seen nil))
     (unwind-protect
         (progn
           (let ((default-directory ,root-var))
             (call-process "git" nil nil nil "init" "-q"))
           (make-directory (file-name-directory ,file-var))
           (write-region "void main() {}\n" nil ,file-var)
           (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
             (advice-add 'eglot-ensure :override
                         (lambda (&rest _) (push (project-current) ygg-session-lsp-defer-tests--seen))
                         '((name . ygg-session-lsp-defer-tests-recorder) (depth . 100)))
             (unwind-protect (progn ,@body)
               (advice-remove 'eglot-ensure 'ygg-session-lsp-defer-tests-recorder))))
       (delete-directory ,root-var t))))

(defun ygg-session-lsp-defer-tests--ensure (file restoring)
  "Visit FILE and call `eglot-ensure' as its mode hook would, restoring or not."
  (with-current-buffer (find-file-noselect file)
    (let ((vc-handled-backends (if restoring nil vc-handled-backends))
          (ygg-session--restoring restoring))
      (project--clear-cache)
      (eglot-ensure))
    (current-buffer)))

(ert-deftest ygg-session-lsp-defer-tests-held-while-restoring ()
  (ygg-session-lsp-defer-tests--in-repo _root file
    (let ((buffer (ygg-session-lsp-defer-tests--ensure file t)))
      (unwind-protect
          (progn
            (should-not ygg-session-lsp-defer-tests--seen)
            (should (buffer-local-value 'ygg-lsp--ensure-deferred buffer)))
        (kill-buffer buffer)))))

(ert-deftest ygg-session-lsp-defer-tests-runs-at-first-display-on-the-vc-project ()
  (ygg-session-lsp-defer-tests--in-repo root file
    (let ((buffer (ygg-session-lsp-defer-tests--ensure file t)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (let ((vc-handled-backends nil)) (project-current)))
            (push buffer ygg-session--deferred-vc)
            (save-window-excursion
              (switch-to-buffer buffer)
              (ygg-session--vc-on-display (selected-frame)))
            (should (= 1 (length ygg-session-lsp-defer-tests--seen)))
            (should (eq 'vc (car (car ygg-session-lsp-defer-tests--seen))))
            (should (equal root (file-name-as-directory
                                 (project-root (car ygg-session-lsp-defer-tests--seen)))))
            (should-not (buffer-local-value 'ygg-lsp--ensure-deferred buffer)))
        (kill-buffer buffer)))))

(ert-deftest ygg-session-lsp-defer-tests-immediate-outside-restore ()
  (ygg-session-lsp-defer-tests--in-repo root file
    (let ((buffer (ygg-session-lsp-defer-tests--ensure file nil)))
      (unwind-protect
          (progn
            (should (= 1 (length ygg-session-lsp-defer-tests--seen)))
            (should (equal root (file-name-as-directory
                                 (project-root (car ygg-session-lsp-defer-tests--seen)))))
            (should-not (buffer-local-value 'ygg-lsp--ensure-deferred buffer)))
        (kill-buffer buffer)))))

(provide 'ygg-session-lsp-defer-tests)
