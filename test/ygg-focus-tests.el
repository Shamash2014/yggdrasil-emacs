;;; ygg-focus-tests.el --- Tests for dimming the unselected windows -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-focus)

(ert-deftest ygg-focus-dims-a-window-but-not-a-side-window ()
  "An unselected window is dimmed; a side window beside it is not."
  (let ((main (get-buffer-create " *focus-main*"))
        (other (get-buffer-create " *focus-other*"))
        (side (get-buffer-create " *focus-side*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer main)
          (set-window-buffer (split-window-right) other)
          (display-buffer-in-side-window side '((side . left)))
          (select-window (get-buffer-window main))
          (ygg-focus-mode 1)
          (ygg-focus-refresh)
          (should (buffer-local-value 'ygg-focus--dim-cookie other))
          (should-not (buffer-local-value 'ygg-focus--dim-cookie side))
          (should-not (buffer-local-value 'ygg-focus--dim-cookie main)))
      (ygg-focus-mode -1)
      (mapc #'kill-buffer (list main other side)))))

(provide 'ygg-focus-tests)
;;; ygg-focus-tests.el ends here
