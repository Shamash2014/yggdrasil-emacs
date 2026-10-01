;;; idle-cost-tests.el --- What an idle Emacs keeps doing, kept to one of each -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'layer-git)
(require 'yggdrasil-spacetree)

(defun idle-cost-tests--blame-timers ()
  (seq-count (lambda (timer) (eq (timer--function timer) #'ygg-blame--show))
             timer-idle-list))

(ert-deftest idle-cost-blame-keeps-one-timer-however-often-it-is-enabled ()
  (unwind-protect
      (progn
        (ygg-inline-blame-mode 1)
        (ygg-inline-blame-mode 1)
        (should (= (idle-cost-tests--blame-timers) 1))
        (ygg-inline-blame-mode -1)
        (should (= (idle-cost-tests--blame-timers) 0)))
    (ygg-inline-blame-mode -1)))

(ert-deftest idle-cost-tab-bar-is-stretched-once-until-it-changes ()
  "The bar comes back stretched from the cache until a tab or the width moves."
  (let ((ygg-space--format-cache nil)
        (stretched 0)
        (width 800))
    (cl-letf (((symbol-function 'frame-inner-width) (lambda (&rest _) width)))
      (let ((count (lambda (items) (cl-incf stretched) items)))
        (advice-add 'tab-bar-auto-width :filter-return count)
        (unwind-protect
            (let ((first (ygg-space--format)))
              (should (eq first (ygg-space--format)))
              (should (= stretched 1))
              (ygg-space--format-invalidate)
              (should-not (eq first (ygg-space--format)))
              (should (= stretched 2))
              (setq width 1200)
              (ygg-space--format)
              (should (= stretched 3))
              (ygg-space--format)
              (should (= stretched 3)))
          (advice-remove 'tab-bar-auto-width count))))))

(ert-deftest idle-cost-tab-bar-leaves-the-stretching-to-the-cache ()
  (let ((tab-bar-auto-width t)
        (tab-bar-format nil))
    (cl-letf (((symbol-function 'tab-bar-mode) #'ignore)
              ((symbol-function 'ygg-space--ensure-root) #'ignore)
              ((symbol-function 'advice-add) #'ignore)
              ((symbol-function 'add-hook) #'ignore))
      (ygg-spacetree-setup))
    (should-not tab-bar-auto-width)
    (should (equal tab-bar-format '(ygg-space--format)))))

(provide 'idle-cost-tests)
;;; idle-cost-tests.el ends here
