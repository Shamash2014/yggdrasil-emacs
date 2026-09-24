;;; ygg-projects-tests.el --- Tests for the projects sidebar rows -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ygg-projects)

(defvar ygg-projects-tests--state 'working
  "The state the stand-in session reports.")

(defmacro ygg-projects-tests--with-session (clock spend progress &rest body)
  "Run BODY with the symbol session standing for an agent session.
It reports CLOCK, SPEND and todo PROGRESS as a (DONE . TOTAL) cons, and
ygg-projects-tests--state as its state."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'aob-session-p) (lambda (s) (eq s 'session)))
             ((symbol-function 'aob-session-state) (lambda (_) ygg-projects-tests--state))
             ((symbol-function 'aob-session-clock) (lambda (_) ,clock))
             ((symbol-function 'aob-session-spend) (lambda (_) ,spend))
             ((symbol-function 'ygg-todo-session-file) (lambda (_) "todo"))
             ((symbol-function 'ygg-todo-progress) (lambda (_) ,progress)))
     ,@body))

(defun ygg-projects-tests--row (label)
  "LABEL's row for the session, as plain text with its alignment spaces gone."
  (let ((row (ygg-projects--entry-text label "/tmp/p/" 'agents 'session)))
    (should-not (string-search "\n" row))
    (let ((i 0) (out ""))
      (while (< i (length row))
        (let ((next (next-single-property-change i 'display row (length row))))
          (unless (get-text-property i 'display row)
            (setq out (concat out (substring-no-properties row i next))))
          (setq i next)))
      out)))

(defun ygg-projects-tests--fits (text)
  "Non-nil when TEXT leaves at least a column for the gap the badge sits after."
  (< (string-width text) (ygg-projects--width)))

(ert-deftest ygg-projects-row-keeps-a-short-name-and-the-whole-meter ()
  "A short name and a full meter share one row at the stock width."
  (let ((ygg-projects-width 40))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "claude:9" row))
        (should (string-search "3h15m… $160.85 18/20" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-sheds-spend-before-cutting-a-name-short ()
  "A long name costs the meter its spend, then is cut, never wrapped."
  (let ((ygg-projects-width 40))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "a-conversation-with-a-very-long-name")))
        (should-not (string-search "$160.85" row))
        (should (string-search "3h15m… 18/20" row))
        (should (string-search "…" (car (split-string row "3h15m"))))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-keeps-progress-last-when-narrow ()
  "In a narrow sidebar the progress is the part of the meter that stays."
  (let ((ygg-projects-width 30))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "claude:9" row))
        (should (string-search "18/20" row))
        (should-not (string-search "3h15m" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-keeps-a-waiting-state-last ()
  "A session waiting on you keeps saying so when the meter has to shrink."
  (let ((ygg-projects-width 34)
        (ygg-projects-tests--state 'blocked))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "blocked" row))
        (should-not (string-search "$160.85" row))
        (should-not (string-search "3h15m" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-keeps-the-end-of-a-long-path ()
  "A folder too long for its row loses its head and keeps its own name."
  (let* ((ygg-projects-width 40)
         (label "~/work/clients/some-client/repositories/the-folder-name")
         (row (ygg-projects--entry-text label "/tmp/p/" 'folders "/tmp/x/")))
    (should-not (string-search "\n" row))
    (should (string-search "…" row))
    (should (string-search "the-folder-name" row))
    (should (< (string-width (substring-no-properties row)) (+ 2 (ygg-projects--width))))))

(provide 'ygg-projects-tests)
;;; ygg-projects-tests.el ends here
