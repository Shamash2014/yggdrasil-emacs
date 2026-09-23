;;; ygg-helix-selection-tests.el --- Tests for Helix selection commands -*- lexical-binding: t; -*-

(require 'ert)
(require 'yggdrasil-motions)
(require 'yggdrasil-selection)

(defmacro ygg-with-temp-buffer (content &rest body)
  "Create a temp buffer with CONTENT, execute BODY, cleanup."
  (declare (indent 1))
  `(with-temp-buffer
     (yggdrasil-local-mode 1)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; V x - shrink to line bounds

(ert-deftest ygg-helix-shrink-single-line-noop ()
  "V x on single line should be no-op."
  (ygg-with-temp-buffer "foo bar baz"
    (ygg-normal-state)
    (ygg-set-selection 1 6)
    (call-interactively #'ygg-shrink-to-line-bounds)
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (should (equal (buffer-substring-no-properties beg end) "foo b")))))

(ert-deftest ygg-helix-shrink-multi-line ()
  "V x should shrink multi-line selection."
  (ygg-with-temp-buffer "line1\nline2\nline3"
    (ygg-normal-state)
    (ygg-set-selection 2 13)
    (call-interactively #'ygg-shrink-to-line-bounds)
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (should (equal beg 1))
      (should (equal end 13)))))

(ert-deftest ygg-helix-shrink-backward-multi-line ()
  "V x should work with backward selections."
  (ygg-with-temp-buffer "line1\nline2\nline3"
    (ygg-normal-state)
    (ygg-set-selection 13 2)
    (call-interactively #'ygg-shrink-to-line-bounds)
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (should (< (point) (mark t))))))

;;; V > - ensure selections forward

(ert-deftest ygg-helix-ensure-forward-already-forward ()
  "V > on forward selection should be no-op."
  (ygg-with-temp-buffer "foo bar baz"
    (ygg-normal-state)
    (ygg-set-selection 1 6)
    (call-interactively #'ygg-ensure-selections-forward)
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (should (equal (buffer-substring-no-properties beg end) "foo b")))))

(ert-deftest ygg-helix-ensure-forward-backward ()
  "V > should flip backward selections to forward."
  (ygg-with-temp-buffer "foo bar baz"
    (ygg-normal-state)
    (ygg-set-selection 6 1)
    (should (< (point) (mark t)))
    (call-interactively #'ygg-ensure-selections-forward)
    (should (> (point) (mark t)))))

;;; <home> in normal mode

(ert-deftest ygg-helix-home-normal ()
  "<home> in normal mode should go to line start."
  (ygg-with-temp-buffer "  indented"
    (ygg-normal-state)
    (goto-char 5)
    (call-interactively #'ygg-goto-line-start)
    (should (equal (point) 1))))

(ert-deftest ygg-helix-home-multiline ()
  "<home> should respect line boundaries."
  (ygg-with-temp-buffer "  indented\n  next"
    (ygg-normal-state)
    (goto-char 15)
    (call-interactively #'ygg-goto-line-start)
    (should (equal (point) 12))))

;;; <end> in normal mode

(ert-deftest ygg-helix-end-normal ()
  "<end> in normal mode should go to line end (cursor on last char)."
  (ygg-with-temp-buffer "hello world"
    (ygg-normal-state)
    (goto-char 1)
    (call-interactively #'ygg-goto-line-end)
    (should (equal (point) 11))))

(ert-deftest ygg-helix-end-multiline ()
  "<end> should respect line boundaries."
  (ygg-with-temp-buffer "hello\nworld"
    (ygg-normal-state)
    (goto-char 1)
    (call-interactively #'ygg-goto-line-end)
    (should (equal (point) 5))))

;;; <home> in insert mode

(ert-deftest ygg-helix-home-insert ()
  "<home> in insert mode should go to line start."
  (ygg-with-temp-buffer "  indented text"
    (ygg-insert-state)
    (goto-char 8)
    (call-interactively #'beginning-of-line)
    (should (equal (point) 1))))

;;; <end> in insert mode

(ert-deftest ygg-helix-end-insert ()
  "<end> in insert mode should go to line end."
  (ygg-with-temp-buffer "hello world"
    (ygg-insert-state)
    (goto-char 1)
    (call-interactively #'end-of-line)
    (should (equal (point) 12))))

;;; C-<delete> in insert mode

(ert-deftest ygg-helix-insert-delete-word ()
  "C-<delete> in insert should delete word forward."
  (ygg-with-temp-buffer "hello world test"
    (ygg-insert-state)
    (goto-char 1)
    (call-interactively #'kill-word)
    (should (equal (buffer-string) " world test"))))

(ert-deftest ygg-helix-insert-delete-mid-word ()
  "C-<delete> mid-word should delete to word end."
  (ygg-with-temp-buffer "hello world"
    (ygg-insert-state)
    (goto-char 2)
    (call-interactively #'kill-word)
    (should (equal (buffer-string) "h world"))))

;;; Provide the test file

(provide 'ygg-helix-selection-tests)
;;; ygg-helix-selection-tests.el ends here
