;;; ygg-vertico-posframe-fit-tests.el --- Rows fit the floating picker -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'layer-ui)

(defvar vertico-posframe-mode)

(defun ygg-vertico-posframe-fit-tests--fit (cand prefix suffix)
  (with-temp-buffer
    (let ((vertico-posframe-mode t))
      (cl-letf (((symbol-function 'frame-height) (lambda (&rest _) 50))
                ((symbol-function 'frame-width) (lambda (&rest _) 200))
                ((symbol-function 'minibuffer-prompt-end) (lambda () 1)))
        (car (ygg--vertico-posframe-fit-row (list cand prefix suffix 0 0)))))))

(ert-deftest ygg-vertico-posframe-fit/over-long-row-is-cut-with-an-ellipsis ()
  (let ((fit (ygg-vertico-posframe-fit-tests--fit (make-string 300 ?x) "→ " "")))
    (should (= (string-width fit) 77))
    (should (string-suffix-p "…" fit))))

(ert-deftest ygg-vertico-posframe-fit/suffix-keeps-its-room ()
  (should (= (string-width (ygg-vertico-posframe-fit-tests--fit
                            (make-string 300 ?x) "→ " (make-string 20 ?a)))
             57)))

(ert-deftest ygg-vertico-posframe-fit/command-name-outlives-its-annotation ()
  (let ((fit (ygg-vertico-posframe-fit-tests--fit
              "find-file" "" (concat "  " (make-string 200 ?d)))))
    (should (equal fit "find-file"))))

(ert-deftest ygg-vertico-posframe-fit/short-row-is-untouched ()
  (should (equal (ygg-vertico-posframe-fit-tests--fit "short" "→ " "") "short")))

(provide 'ygg-vertico-posframe-fit-tests)
;;; ygg-vertico-posframe-fit-tests.el ends here
