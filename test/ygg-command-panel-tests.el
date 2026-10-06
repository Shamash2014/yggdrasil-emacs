;;; ygg-command-panel-tests.el --- The command panel lists tasks and commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar ygg-leader-open-map (make-sparse-keymap))
(require 'layer-tasks)

(defun ygg-command-panel-tests--candidates ()
  (let (shown)
    (cl-letf (((symbol-function 'ygg-task--collect)
               (lambda (&rest _) (list (list :label "just: build" :command "just build" :directory "/"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _) (setq shown (all-completions "" table)) "")))
      (ignore-errors (ygg-command-panel)))
    shown))

(ert-deftest ygg-command-panel/lists-tasks-first-and-every-command ()
  (let ((shown (ygg-command-panel-tests--candidates)))
    (should (equal (car shown) "▶ run  just: build"))
    (should (member "find-file" shown))))

(provide 'ygg-command-panel-tests)
;;; ygg-command-panel-tests.el ends here
