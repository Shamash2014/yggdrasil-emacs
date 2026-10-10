;;; ygg-steal-modal-tests.el --- Comment-continuing o/O, url object, mode surrounds, g P / g y -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'yggdrasil)

(defmacro ygg-steal-tests--in (mode content &rest body)
  (declare (indent 2))
  `(let ((buf (generate-new-buffer "ygg-steal")))
     (unwind-protect
         (progn
           (switch-to-buffer buf)
           (funcall ,mode)
           (buffer-enable-undo)
           (yggdrasil-local-mode 1)
           (insert ,content)
           (goto-char (point-min))
           ,@body)
       (kill-buffer buf))))

(defun ygg-steal-tests--keys (keys)
  (execute-kbd-macro (kbd keys)))

(defun ygg-steal-tests--selected ()
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (buffer-substring-no-properties beg end)))

(ert-deftest ygg-steal-o-continues-comment ()
  (ygg-steal-tests--in #'emacs-lisp-mode ";; foo\n(bar)\n"
    (ygg-steal-tests--keys "o")
    (should (equal (buffer-substring (point-min) (point)) ";; foo\n;; "))))

(ert-deftest ygg-steal-O-continues-comment ()
  (ygg-steal-tests--in #'emacs-lisp-mode "(bar)\n;; foo\n"
    (ygg-steal-tests--keys "j O")
    (should (equal (buffer-substring (point-min) (point)) "(bar)\n;; "))
    (should (equal (buffer-string) "(bar)\n;; \n;; foo\n"))))

(ert-deftest ygg-steal-o-code-line-no-prefix ()
  (ygg-steal-tests--in #'emacs-lisp-mode "(bar)\n"
    (ygg-steal-tests--keys "o")
    (should (equal (buffer-substring (point-min) (point)) "(bar)\n"))))

(ert-deftest ygg-steal-o-string-with-semicolon-no-prefix ()
  (ygg-steal-tests--in #'emacs-lisp-mode "(setq x \"a ; b\n; c\")\n"
    (ygg-steal-tests--keys "j o")
    (should (equal (buffer-substring (point-min) (point)) "(setq x \"a ; b\n; c\")\n"))))

(ert-deftest ygg-steal-o-string-line-no-prefix ()
  (ygg-steal-tests--in #'emacs-lisp-mode "(setq x \"a ; b\")\n"
    (ygg-steal-tests--keys "o")
    (should (equal (buffer-substring (point-min) (point)) "(setq x \"a ; b\")\n"))))

(ert-deftest ygg-steal-o-text-mode-no-prefix ()
  (ygg-steal-tests--in #'text-mode "; foo\n"
    (ygg-steal-tests--keys "o")
    (should (equal (buffer-substring (point-min) (point)) "; foo\n"))))

(ert-deftest ygg-steal-o-option-nil-old-behaviour ()
  (ygg-steal-tests--in #'emacs-lisp-mode ";; foo\n"
    (let ((ygg-open-continue-comments nil))
      (ygg-steal-tests--keys "o"))
    (should (equal (buffer-substring (point-min) (point)) ";; foo\n"))))

(ert-deftest ygg-steal-url-inner-and-around ()
  (ygg-steal-tests--in #'text-mode "see <https://x.y/z>."
    (goto-char 12)
    (ygg-steal-tests--keys "m i u")
    (should (equal (ygg-steal-tests--selected) "https://x.y/z"))
    (ygg-steal-tests--keys "<escape>")
    (goto-char 12)
    (ygg-steal-tests--keys "m a u")
    (should (equal (ygg-steal-tests--selected) "<https://x.y/z>"))))

(ert-deftest ygg-steal-elisp-backquote-surround ()
  (ygg-steal-tests--in #'emacs-lisp-mode "foo-bar"
    (ygg-set-selection 1 8)
    (ygg-steal-tests--keys "m s `")
    (should (equal (buffer-string) "`foo-bar'"))))

(ert-deftest ygg-steal-backquote-surround-other-mode ()
  (ygg-steal-tests--in #'text-mode "foo"
    (ygg-set-selection 1 4)
    (ygg-steal-tests--keys "m s `")
    (should (equal (buffer-string) "`foo`"))))

(ert-deftest ygg-steal-g-P-selects-paste ()
  (ygg-steal-tests--in #'text-mode "abc def"
    (kill-new "XYZ")
    (goto-char (point-max))
    (ygg-set-selection 7 8)
    (ygg-steal-tests--keys "p")
    (ygg-steal-tests--keys "<escape>")
    (goto-char (point-min))
    (ygg-steal-tests--keys "g P")
    (should (equal (ygg-steal-tests--selected) "XYZ"))))

(ert-deftest ygg-steal-g-y-yanks-unindented ()
  (ygg-steal-tests--in #'text-mode "    one\n      two\n"
    (ygg-set-selection 5 18)
    (ygg-steal-tests--keys "g y")
    (should (equal (car kill-ring) "one\n  two"))))

(provide 'ygg-steal-modal-tests)
