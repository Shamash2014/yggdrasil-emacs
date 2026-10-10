;;; ygg-steal-windows-tests.el --- winner keys and popup rules -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-ui)
(require 'layer-quickfix)
(require 'layer-terminal)

(defun ygg-steal-windows-tests--key (map key)
  (lookup-key (symbol-value map) (kbd key)))

(ert-deftest ygg-steal-windows-winner-keys ()
  (should (eq (ygg-steal-windows-tests--key 'ygg-leader-window-map "u") #'winner-undo))
  (should (eq (ygg-steal-windows-tests--key 'ygg-leader-window-map "U") #'winner-redo))
  (should (eq (ygg-steal-windows-tests--key 'ygg-leader-window-map "p") #'ygg-popup-restore))
  (should (eq (ygg-steal-windows-tests--key 'ygg-leader-window-map "P") #'ygg-popup-toggle))
  (should winner-mode)
  (should (eq (get 'winner-undo 'repeat-map) 'ygg-winner-repeat-map))
  (should (eq (get 'winner-redo 'repeat-map) 'ygg-winner-repeat-map))
  (should (eq (lookup-key ygg-winner-repeat-map "u") #'winner-undo))
  (should (eq (lookup-key ygg-winner-repeat-map "U") #'winner-redo)))

(defun ygg-steal-windows-tests--entry-for (name)
  (cl-find-if (lambda (e) (and (stringp (car e)) (assq 'ygg-popup (cddr e))
                               (string-match-p (car e) name)))
              display-buffer-alist))

(defconst ygg-steal-windows-tests--targets
  '("*Help*" "*Messages*" "*Warnings*" "*Backtrace*" "*Man ls*" "*info*"
    "*Apropos*" "*Calculator*" "*Calc*"))

(defconst ygg-steal-windows-tests--others
  '("*quickfix*" "*quickfix:2*" "*aob-btw*" "*aob-bang*" "*task:build*"
    "*dape-repl*" "*http-response*" "*Compile-Log*" "*scratch*" "foo.el"
    "*Help* <2>" "*Messages*x" "*browser*"))

(ert-deftest ygg-steal-windows-rules-match-targets-only ()
  (dolist (name ygg-steal-windows-tests--targets)
    (should (ygg-steal-windows-tests--entry-for name)))
  (dolist (name ygg-steal-windows-tests--others)
    (should-not (ygg-steal-windows-tests--entry-for name))))

(ert-deftest ygg-steal-windows-rules-shape ()
  (let ((help (cdr (ygg-steal-windows-tests--entry-for "*Help*")))
        (man (cdr (ygg-steal-windows-tests--entry-for "*Man ls*"))))
    (should (memq #'display-buffer-in-side-window (car help)))
    (should (eq (alist-get 'side (cdr help)) 'bottom))
    (should (equal (alist-get 'window-height (cdr help)) 0.35))
    (should (eq (alist-get 'side (cdr man)) 'right))
    (should (equal (alist-get 'window-width (cdr man)) 0.45))))

(ert-deftest ygg-steal-windows-rules-install-idempotent ()
  (let ((before (length display-buffer-alist)))
    (ygg-popup-install)
    (ygg-popup-install)
    (should (= before (length display-buffer-alist)))))

(ert-deftest ygg-steal-windows-existing-entries-win ()
  (let ((display-buffer-alist (append (list (list "\\`\\*Help\\*\\'" '(display-buffer-pop-up-window)))
                                      display-buffer-alist)))
    (should (equal (cadr (assoc "\\`\\*Help\\*\\'" display-buffer-alist))
                   '(display-buffer-pop-up-window)))
    (should (eq (car display-buffer-alist) (assoc "\\`\\*Help\\*\\'" display-buffer-alist)))
    (let ((qf (cl-position-if (lambda (e) (eq (car e) #'ygg-qf--quickfix-buffer-p))
                              display-buffer-alist))
          (pop (cl-position-if (lambda (e) (assq 'ygg-popup (cddr e))) display-buffer-alist)))
      (should (< qf pop)))))

(ert-deftest ygg-steal-windows-display-chooses-side-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buf (get-buffer-create "*Apropos*")))
      (unwind-protect
          (let ((win (display-buffer buf)))
            (should (window-live-p win))
            (should (eq (window-parameter win 'window-side) 'bottom))
            (should (memq buf ygg-popup--shown)))
        (kill-buffer buf)))))

(ert-deftest ygg-steal-windows-quit-deletes-side-window ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((buf (get-buffer-create "*Apropos*"))
           (win (display-buffer buf)))
      (unwind-protect
          (progn (quit-window nil win)
                 (should-not (window-live-p win))
                 (should (= 1 (length (window-list)))))
        (kill-buffer buf)))))

(ert-deftest ygg-steal-windows-restore-last ()
  (save-window-excursion
    (delete-other-windows)
    (let* ((a (get-buffer-create "*Apropos*"))
           (b (get-buffer-create "*info*")))
      (unwind-protect
          (progn
            (setq ygg-popup--shown nil)
            (display-buffer a)
            (quit-window nil (get-buffer-window a))
            (should-not (get-buffer-window a))
            (ygg-popup-restore)
            (should (get-buffer-window a))
            (ygg-popup-toggle)
            (should-not (get-buffer-window a))
            (ygg-popup-toggle)
            (should (get-buffer-window a))
            (ignore b))
        (kill-buffer a)
        (kill-buffer b)))))

(provide 'ygg-steal-windows-tests)
;;; ygg-steal-windows-tests.el ends here
