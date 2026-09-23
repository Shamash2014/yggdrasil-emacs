;;; ygg-insert-repeat-tests.el --- Insert sessions: dot-repeat, undo, counts -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'yggdrasil)

(defmacro ygg-insert-test-buffer (content &rest body)
  "Run BODY in a modal text buffer holding CONTENT, point at its start.
The buffer is shown in the selected window so keyboard macros drive it."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer "ygg-insert-test")))
     (unwind-protect
         (progn
           (switch-to-buffer buf)
           (text-mode)
           (buffer-enable-undo)
           (yggdrasil-local-mode 1)
           (insert ,content)
           (goto-char (point-min))
           (undo-boundary)
           ,@body)
       (kill-buffer buf))))

(defun ygg-insert-test-keys (keys)
  (execute-kbd-macro (kbd keys)))

(defun ygg-insert-test-goto-line (n)
  (goto-char (point-min))
  (forward-line (1- n))
  (set-mark (point)))

(ert-deftest ygg-insert-repeat-newline ()
  (ygg-insert-test-buffer "one\ntwo\n"
    (ygg-insert-test-keys "A x RET y <escape>")
    (should (equal (buffer-string) "onex\ny\ntwo\n"))
    (ygg-insert-test-goto-line 3)
    (ygg-insert-test-keys ".")
    (should (equal (buffer-string) "onex\ny\ntwox\ny\n"))
    (should (eq (ygg-state) 'normal))))

(ert-deftest ygg-insert-repeat-backspace-past-entry ()
  (ygg-insert-test-buffer "one two\none two\n"
    (goto-char 5)
    (set-mark (point))
    (ygg-insert-test-keys "i DEL DEL X <escape>")
    (should (equal (buffer-string) "onXtwo\none two\n"))
    (goto-char (+ (line-beginning-position 2) 4))
    (set-mark (point))
    (ygg-insert-test-keys ".")
    (should (equal (buffer-string) "onXtwo\nonXtwo\n"))))

(ert-deftest ygg-insert-single-undo-step ()
  (ygg-insert-test-buffer "start\n"
    (ygg-insert-test-keys
     (concat "A " (mapconcat #'string "abcdefghiklmnopqrstuvwxyzabcdefghikl" " ")
             " <escape>"))
    (should (equal (buffer-string) "startabcdefghiklmnopqrstuvwxyzabcdefghikl\n"))
    (ygg-insert-test-keys "u")
    (should (equal (buffer-string) "start\n"))))

(ert-deftest ygg-insert-change-is-one-undo-step ()
  (ygg-insert-test-buffer "abc def\n"
    (ygg-insert-test-keys "w c X Y <escape>")
    (let ((changed (buffer-string)))
      (should-not (equal changed "abc def\n"))
      (ygg-insert-test-keys "u")
      (should (equal (buffer-string) "abc def\n")))))

(ert-deftest ygg-insert-count-insert ()
  (ygg-insert-test-buffer "\n"
    (ygg-insert-test-keys "3 i h e l l o <escape>")
    (should (equal (buffer-string) "hellohellohello\n"))
    (ygg-insert-test-keys "u")
    (should (equal (buffer-string) "\n"))))

(ert-deftest ygg-insert-count-append-eol ()
  (ygg-insert-test-buffer "ab\n"
    (ygg-insert-test-keys "2 A x y <escape>")
    (should (equal (buffer-string) "abxyxy\n"))))

(ert-deftest ygg-insert-count-open-below ()
  (ygg-insert-test-buffer "a\nz\n"
    (ygg-insert-test-keys "2 o f o o <escape>")
    (should (equal (buffer-string) "a\nfoo\nfoo\nz\n"))
    (ygg-insert-test-keys "u")
    (should (equal (buffer-string) "a\nz\n"))))

(ert-deftest ygg-insert-count-open-above ()
  (ygg-insert-test-buffer "a\nz\n"
    (ygg-insert-test-goto-line 2)
    (ygg-insert-test-keys "3 O b <escape>")
    (should (equal (buffer-string) "a\nb\nb\nb\nz\n"))))

(ert-deftest ygg-insert-repeat-counted ()
  (ygg-insert-test-buffer "\n"
    (ygg-insert-test-keys "3 i a b <escape>")
    (should (equal (buffer-string) "ababab\n"))
    (ygg-insert-test-keys ".")
    (should (equal (buffer-string) "ababaabababb\n"))))

(ert-deftest ygg-insert-repeat-open-below ()
  (ygg-insert-test-buffer "a\nz\n"
    (ygg-insert-test-keys "o f o o <escape>")
    (ygg-insert-test-goto-line 3)
    (ygg-insert-test-keys ".")
    (should (equal (buffer-string) "a\nfoo\nz\nfoo\n"))))

(ert-deftest ygg-insert-multi-cursor-then-repeat ()
  (ygg-insert-test-buffer "aa\nbb\ncc\n"
    (ygg-insert-test-goto-line 2)
    (ygg-insert-test-keys "V C i X Y <escape>")
    (should (equal (buffer-string) "XYaa\nXYbb\ncc\n"))
    (ygg-insert-test-keys "<escape>")
    (ygg-insert-test-goto-line 3)
    (ygg-insert-test-keys ".")
    (should (equal (buffer-string) "XYaa\nXYbb\nXYcc\n"))
    (ygg-insert-test-keys "u u")
    (should (equal (buffer-string) "aa\nbb\ncc\n"))))

(ert-deftest ygg-insert-repeat-with-secondaries ()
  (ygg-insert-test-buffer "aa\nbb\ncc\ndd\n"
    (ygg-insert-test-goto-line 2)
    (ygg-insert-test-keys "V C i X RET <escape>")
    (should (equal (buffer-string) "X\naa\nX\nbb\ncc\ndd\n"))
    (ygg-insert-test-keys "<escape>")
    (ygg-insert-test-goto-line 5)
    (ygg-insert-test-keys "C .")
    (should (equal (buffer-string) "X\naa\nX\nbb\nX\ncc\nX\ndd\n"))))

(ert-deftest ygg-insert-one-command-keeps-working ()
  (ygg-insert-test-buffer "x\n"
    (ygg-insert-test-keys "i f o o C-o 0 b a r <escape>")
    (should (equal (buffer-string) "barfoox\n"))
    (should (eq (ygg-state) 'normal))
    (ygg-insert-test-keys "u u")
    (should (equal (buffer-string) "x\n"))
    (ygg-insert-test-keys ".")
    (should (equal (buffer-string) "foox\n"))))

(provide 'ygg-insert-repeat-tests)
;;; ygg-insert-repeat-tests.el ends here
