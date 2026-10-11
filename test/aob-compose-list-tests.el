;;; aob-compose-list-tests.el --- list continuation in compose  -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'aob)
(require 'layer-aob)

(defun aob-compose-list-tests--type (text key &optional state)
  (with-temp-buffer
    (aob-compose-mode)
    (insert text)
    (ygg--switch-state (or state 'insert))
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

(ert-deftest aob-compose-list-normal-state-has-no-list-keys ()
  (with-temp-buffer
    (aob-compose-mode)
    (ygg--switch-state 'normal)
    (dolist (key '("RET" "TAB" "<backtab>"))
      (should-not (memq (key-binding (kbd key))
                        '(aob-compose-list-newline aob-compose-list-indent
                          aob-compose-list-outdent)))))
  (should-not (equal (aob-compose-list-tests--type "- one" "RET" 'normal)
                     "- one\n- ")))

(ert-deftest aob-compose-list-insert-state-reaches-list-commands ()
  (with-temp-buffer
    (aob-compose-mode)
    (ygg--switch-state 'insert)
    (should (eq (key-binding (kbd "RET")) 'aob-compose-list-newline))
    (should (eq (key-binding (kbd "TAB")) 'aob-compose-list-indent))
    (should (eq (key-binding (kbd "<backtab>")) 'aob-compose-list-outdent))))
