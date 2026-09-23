;;; ygg-increment-tests.el --- Tests for number increment/decrement -*- lexical-binding: t; -*-

(require 'ert)
(require 'yggdrasil-verbs)

(defun ygg-inc-test-at (text)
  "Test incrementing a number in TEXT, return the result."
  (let ((buf (generate-new-buffer "*test-inc*")))
    (unwind-protect
        (with-current-buffer buf
          (insert text)
          (goto-char (point-min))
          (condition-case err
              (progn
                (ygg--number-increment-at (point) 1)
                (buffer-string))
            (error (message "Error: %s" err)
                   (buffer-string))))
      (kill-buffer buf))))

;;; Decimal numbers

(ert-deftest ygg-inc-decimal ()
  "Increment 9 to 10."
  (should (string= (ygg-inc-test-at "9") "10")))

(ert-deftest ygg-inc-zero-padded ()
  "Increment 007 to 008 with zero padding preserved."
  (should (string= (ygg-inc-test-at "007") "008")))

;;; Hexadecimal numbers

(ert-deftest ygg-inc-hex-lowercase ()
  "Increment 0x0f to 0x10 preserving lowercase."
  (should (string= (ygg-inc-test-at "0x0f") "0x10")))

(ert-deftest ygg-inc-hex-uppercase ()
  "Increment 0xFF to 0x100 preserving uppercase."
  (should (string= (ygg-inc-test-at "0xFF") "0x100")))

(ert-deftest ygg-inc-hex-mixed ()
  "Increment 0x09 to 0x0a preserving lowercase."
  (should (string= (ygg-inc-test-at "0x09") "0x0a")))

;;; Binary numbers

(ert-deftest ygg-inc-binary ()
  "Increment 0b0111 to 0b1000."
  (should (string= (ygg-inc-test-at "0b0111") "0b1000")))

;;; Octal numbers

(ert-deftest ygg-inc-octal ()
  "Increment 0o07 to 0o10."
  (should (string= (ygg-inc-test-at "0o07") "0o10")))

;;; Negative numbers

(ert-deftest ygg-inc-negative ()
  "Increment -1 to 0."
  (should (string= (ygg-inc-test-at "-1") "0")))

;;; Two-cursor increment

(ert-deftest ygg-inc-two-cursors ()
  "Two cursors both increment: 1 -> 2 and 5 -> 6."
  (let ((buf (generate-new-buffer "*test-multi*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "1 and 5")
          ;; Increment at position 1 (first "1")
          (ygg--number-increment-at 1 1)
          ;; Increment at position 7 (the "5")
          (ygg--number-increment-at 7 1)
          (should (string= (buffer-string) "2 and 6")))
      (kill-buffer buf))))

(provide 'ygg-increment-tests)
