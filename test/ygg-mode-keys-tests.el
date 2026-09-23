;;; ygg-mode-keys-tests.el --- State-scoped mode keymaps -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'yggdrasil)
(require 'grep)
(yggdrasil-global-mode 1)

(unless (fboundp 'magit-blame-mode)
  (define-minor-mode magit-blame-mode "Stand-in for magit's blame mode."))
(require 'layer-quickfix)
(require 'layer-git)

(defun ygg-mode-keys-tests--probe () (interactive))
(defun ygg-mode-keys-tests--major-cmd () (interactive))
(defun ygg-mode-keys-tests--minor-cmd () (interactive))

(define-derived-mode ygg-mode-keys-tests-parent-mode special-mode "P")
(define-derived-mode ygg-mode-keys-tests-child-mode
  ygg-mode-keys-tests-parent-mode "C")

(yggdrasil-define-mode-keys 'ygg-mode-keys-tests-minor-mode 'normal
  "j" #'ygg-mode-keys-tests--minor-cmd)
(define-minor-mode ygg-mode-keys-tests-minor-mode "Registered before it existed.")

(defmacro ygg-mode-keys-tests--in (mode &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (,mode)
     (unless yggdrasil-local-mode (yggdrasil-local-mode 1))
     ,@body))

(ert-deftest ygg-mode-keys-hook-lift-survives-modalize ()
  (let* ((probe (make-sparse-keymap))
         (lift (lambda () (yggdrasil-define-local-keys 'normal probe))))
    (define-key probe "j" #'ygg-mode-keys-tests--probe)
    (add-hook 'grep-mode-hook lift)
    (unwind-protect
        (with-temp-buffer
          (grep-mode)
          (should yggdrasil-local-mode)
          (should (eq ygg--state 'normal))
          (should (eq (key-binding "j") #'ygg-mode-keys-tests--probe))
          (should (eq (key-binding "k") #'ygg-qf-prev))
          (should (eq (key-binding (kbd "<backtab>"))
                      (lookup-key grep-mode-map (kbd "<backtab>")))))
      (remove-hook 'grep-mode-hook lift))))

(ert-deftest ygg-mode-keys-normal-only-absent-in-insert ()
  (yggdrasil-define-mode-keys 'ygg-mode-keys-tests-parent-mode 'normal
    "x" #'ygg-mode-keys-tests--major-cmd)
  (ygg-mode-keys-tests--in ygg-mode-keys-tests-child-mode
    (should (eq (key-binding "x") #'ygg-mode-keys-tests--major-cmd))
    (ygg-insert-state)
    (should-not (eq (key-binding "x") #'ygg-mode-keys-tests--major-cmd))
    (ygg-normal-state)
    (ygg-toggle-visual)
    (should-not (eq (key-binding "x") #'ygg-mode-keys-tests--major-cmd))))

(ert-deftest ygg-mode-keys-child-beats-parent ()
  (yggdrasil-define-mode-keys 'ygg-mode-keys-tests-parent-mode 'normal
    "J" #'ygg-mode-keys-tests--major-cmd)
  (ygg-mode-keys-tests--in ygg-mode-keys-tests-child-mode
    (yggdrasil-define-mode-keys 'ygg-mode-keys-tests-child-mode 'normal
      "J" #'ygg-mode-keys-tests--probe)
    (should (eq (key-binding "J") #'ygg-mode-keys-tests--probe))))

(ert-deftest ygg-mode-keys-minor-beats-major-and-leaves-when-off ()
  (yggdrasil-define-mode-keys 'ygg-mode-keys-tests-parent-mode 'normal
    "j" #'ygg-mode-keys-tests--major-cmd)
  (ygg-mode-keys-tests--in ygg-mode-keys-tests-child-mode
    (should (eq (key-binding "j") #'ygg-mode-keys-tests--major-cmd))
    (ygg-mode-keys-tests-minor-mode 1)
    (should (eq (key-binding "j") #'ygg-mode-keys-tests--minor-cmd))
    (ygg-mode-keys-tests-minor-mode -1)
    (should (eq (key-binding "j") #'ygg-mode-keys-tests--major-cmd))))

(ert-deftest ygg-mode-keys-magit-blame-on-and-off ()
  (with-temp-buffer
    (insert "text\n")
    (yggdrasil-local-mode 1)
    (should (eq (key-binding "j") #'ygg-j))
    (magit-blame-mode 1)
    (should (eq (key-binding "j") #'magit-blame-next-chunk))
    (should (eq (key-binding "q") #'magit-blame-quit))
    (ygg-insert-state)
    (should-not (eq (key-binding "q") #'magit-blame-quit))
    (ygg-normal-state)
    (magit-blame-mode -1)
    (should (eq (key-binding "j") #'ygg-j))
    (should-not (eq (key-binding "q") #'magit-blame-quit))))

(ert-deftest ygg-mode-keys-quickfix-normal-and-insert ()
  (with-temp-buffer
    (grep-mode)
    (should (eq ygg--state 'normal))
    (should (eq (key-binding "j") #'ygg-qf-next))
    (should (eq (key-binding "k") #'ygg-qf-prev))
    (should (eq (key-binding (kbd "RET")) #'ygg-qf-open))
    (ygg-insert-state)
    (should (eq (key-binding "j") #'ygg--jk-escape))
    (should-not (eq (key-binding "k") #'ygg-qf-prev))
    (should-not (eq (key-binding (kbd "RET")) #'ygg-qf-open))))

(ert-deftest ygg-mode-keys-finished-compile-gets-panel ()
  (with-temp-buffer
    (compilation-mode)
    (should (eq (key-binding "j") #'ygg-j))
    (ygg-qf--style-on-finish-keys (current-buffer) "finished\n")
    (should (eq (key-binding "j") #'ygg-qf-next))
    (ygg-insert-state)
    (should-not (eq (key-binding "k") #'ygg-qf-prev))))

(ert-deftest ygg-mode-keys-local-lift-is-idempotent ()
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'ygg-mode-keys-tests--probe)
    (with-temp-buffer
      (yggdrasil-local-mode 1)
      (yggdrasil-define-local-keys 'normal map)
      (yggdrasil-define-local-keys 'normal map)
      (should (equal (alist-get 'normal ygg--local-keys)
                     (list (car (alist-get 'normal ygg--local-keys)) map)))
      (should (eq (key-binding "q") #'ygg-mode-keys-tests--probe)))))

(ert-deftest ygg-mode-keys-special-keep-stays-below-mode-keys ()
  (with-temp-buffer
    (help-mode)
    (should (eq ygg--state 'normal))
    (should (eq (key-binding "q") (lookup-key help-mode-map "q")))
    (should (eq (key-binding (kbd "] h")) (lookup-key help-mode-map "r")))
    (ygg-insert-state)
    (should-not (eq (key-binding (kbd "] h")) (lookup-key help-mode-map "r")))))

;;; ygg-mode-keys-tests.el ends here
