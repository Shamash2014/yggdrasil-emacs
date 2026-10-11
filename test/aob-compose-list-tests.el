;;; aob-compose-list-tests.el --- list continuation in compose  -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'aob)

(defun aob-compose-list-tests--type (text key)
  (with-temp-buffer
    (aob-compose-list-mode)
    (insert text)
    (call-interactively (key-binding (kbd key)))
    (buffer-string)))

(ert-deftest aob-compose-list-bullet-continues ()
  (should (equal (aob-compose-list-tests--type "  - one" "RET") "  - one\n  - ")))

(ert-deftest aob-compose-list-number-increments ()
  (should (equal (aob-compose-list-tests--type "1. one" "RET") "1. one\n2. "))
  (should (equal (aob-compose-list-tests--type "9) one" "RET") "9) one\n10) ")))

(ert-deftest aob-compose-list-empty-item-ends-list ()
  (should (equal (aob-compose-list-tests--type "- one\n- " "RET") "- one\n")))

(ert-deftest aob-compose-list-checkbox-continues-unchecked ()
  (should (equal (aob-compose-list-tests--type "- [x] done" "RET")
                 "- [x] done\n- [ ] ")))

(ert-deftest aob-compose-list-tab-indents-and-backtab-outdents ()
  (should (equal (aob-compose-list-tests--type "- one" "TAB") "  - one"))
  (should (equal (aob-compose-list-tests--type "  - one" "<backtab>") "- one")))

(ert-deftest aob-compose-list-plain-line-unchanged ()
  (should (equal (aob-compose-list-tests--type "hello" "RET") "hello\n")))

(ert-deftest aob-compose-list-tab-off-list-keeps-indent-command ()
  (let (called)
    (cl-letf (((symbol-function 'indent-for-tab-command)
               (lambda (&rest _) (setq called t))))
      (aob-compose-list-tests--type "hello" "TAB"))
    (should called)))
