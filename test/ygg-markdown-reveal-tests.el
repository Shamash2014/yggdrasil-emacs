;;; ygg-markdown-reveal-tests.el --- raw markup shown at point while inserting -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(load (expand-file-name "test-elpaca" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'markdown-mode)
(require 'yggdrasil-core)
(require 'ygg-markdown-reveal)

(defmacro ygg-markdown-reveal-tests--with (text &rest body)
  `(with-temp-buffer
     (insert ,text)
     (gfm-mode)
     (setq-local markdown-hide-markup t)
     (ygg-markdown-reveal-setup)
     (font-lock-ensure)
     (set-buffer-modified-p nil)
     ,@body))

(defun ygg-markdown-reveal-tests--hidden-p (pos)
  (or (eq (get-text-property pos 'invisible) 'markdown-markup)
      (equal (get-text-property pos 'display) "")))

(defun ygg-markdown-reveal-tests--goto (pos)
  (goto-char pos)
  (run-hooks 'post-command-hook))

(ert-deftest ygg-markdown-reveal-bold-shown-in-insert ()
  (ygg-markdown-reveal-tests--with "a **bold** b\n"
    (should (ygg-markdown-reveal-tests--hidden-p 3))
    (ygg--switch-state 'insert)
    (ygg-markdown-reveal-tests--goto 6)
    (should-not (ygg-markdown-reveal-tests--hidden-p 3))
    (should-not (ygg-markdown-reveal-tests--hidden-p 4))
    (should-not (ygg-markdown-reveal-tests--hidden-p 9))))

(ert-deftest ygg-markdown-reveal-bold-hidden-in-normal ()
  (ygg-markdown-reveal-tests--with "a **bold** b\n"
    (ygg--switch-state 'normal)
    (ygg-markdown-reveal-tests--goto 6)
    (should (ygg-markdown-reveal-tests--hidden-p 3))
    (should (ygg-markdown-reveal-tests--hidden-p 4))))

(ert-deftest ygg-markdown-reveal-hides-when-point-leaves ()
  (ygg-markdown-reveal-tests--with "a **bold** b\n"
    (ygg--switch-state 'insert)
    (ygg-markdown-reveal-tests--goto 6)
    (ygg-markdown-reveal-tests--goto 12)
    (should (ygg-markdown-reveal-tests--hidden-p 3))
    (should (ygg-markdown-reveal-tests--hidden-p 4))))

(ert-deftest ygg-markdown-reveal-hides-on-leaving-insert ()
  (ygg-markdown-reveal-tests--with "a **bold** b\n"
    (ygg--switch-state 'insert)
    (ygg-markdown-reveal-tests--goto 6)
    (ygg--switch-state 'normal)
    (should (ygg-markdown-reveal-tests--hidden-p 3))
    (should (ygg-markdown-reveal-tests--hidden-p 9))))

(ert-deftest ygg-markdown-reveal-link-shown ()
  (ygg-markdown-reveal-tests--with "see [text](http://x.y) now\n"
    (should (ygg-markdown-reveal-tests--hidden-p 5))
    (ygg--switch-state 'insert)
    (ygg-markdown-reveal-tests--goto 7)
    (dolist (pos '(5 9 10 15))
      (should-not (ygg-markdown-reveal-tests--hidden-p pos)))))

(ert-deftest ygg-markdown-reveal-heading-shown ()
  (ygg-markdown-reveal-tests--with "## Title\n\ntext\n"
    (should (ygg-markdown-reveal-tests--hidden-p 1))
    (ygg--switch-state 'insert)
    (ygg-markdown-reveal-tests--goto 5)
    (should-not (ygg-markdown-reveal-tests--hidden-p 1))))

(ert-deftest ygg-markdown-reveal-survives-refontify ()
  (ygg-markdown-reveal-tests--with "a `code` b\n"
    (ygg--switch-state 'insert)
    (ygg-markdown-reveal-tests--goto 5)
    (should-not (ygg-markdown-reveal-tests--hidden-p 3))
    (font-lock-flush)
    (font-lock-ensure)
    (run-hook-with-args 'jit-lock-functions (point-min) (point-max))
    (should-not (ygg-markdown-reveal-tests--hidden-p 3))))

(ert-deftest ygg-markdown-reveal-leaves-text-alone ()
  (ygg-markdown-reveal-tests--with "a **bold** and [t](u)\n"
    (let ((text (buffer-string)))
      (ygg--switch-state 'insert)
      (ygg-markdown-reveal-tests--goto 6)
      (ygg-markdown-reveal-tests--goto 16)
      (ygg--switch-state 'normal)
      (should-not (buffer-modified-p))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (substring-no-properties text))))))

(ert-deftest ygg-markdown-reveal-skips-work-when-nothing-changed ()
  (ygg-markdown-reveal-tests--with "a **bold** b\n"
    (ygg--switch-state 'insert)
    (let ((calls 0)
          (finder (symbol-function 'ygg-markdown-reveal--bounds)))
      (cl-letf (((symbol-function 'ygg-markdown-reveal--bounds)
                 (lambda () (cl-incf calls) (funcall finder))))
        (ygg-markdown-reveal-tests--goto 6)
        (should (= calls 1))
        (ygg-markdown-reveal-tests--goto 6)
        (should (= calls 1))
        (ygg-markdown-reveal-tests--goto 7)
        (should (= calls 2))
        (insert "x")
        (run-hooks 'post-command-hook)
        (should (= calls 3))))))

(ert-deftest ygg-markdown-reveal-ignores-code-block-lines ()
  (ygg-markdown-reveal-tests--with "```\n# not heading\n```\n"
    (ygg--switch-state 'insert)
    (ygg-markdown-reveal-tests--goto 8)
    (should-not ygg-markdown-reveal--region)))

(ert-deftest ygg-markdown-reveal-strip-keeps-foreign-display ()
  (ygg-markdown-reveal-tests--with "a **bold** b\n"
    (let ((inhibit-read-only t))
      (with-silent-modifications
        (put-text-property 3 5 'invisible 'markdown-markup)
        (put-text-property 3 5 'display "X")))
    (ygg-markdown-reveal--strip 1 10)
    (should-not (get-text-property 3 'invisible))
    (should (equal (get-text-property 3 'display) "X"))))

(provide 'ygg-markdown-reveal-tests)
;;; ygg-markdown-reveal-tests.el ends here
