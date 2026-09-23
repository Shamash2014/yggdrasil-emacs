;;; ygg-ex-tests.el --- Tests for yggdrasil-ex.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'yggdrasil)
(require 'yggdrasil-selection)
(require 'yggdrasil-motions)
(require 'yggdrasil-verbs)
(require 'yggdrasil-ex)

;;; Helper functions

(defun ygg-ex-test-setup-buffer (text)
  "Create a test buffer with TEXT as contents."
  (let ((buf (generate-new-buffer "*ygg-ex-test*")))
    (with-current-buffer buf
      (insert text)
      (yggdrasil-local-mode 1)
      (goto-char (point-min)))
    buf))

(defun ygg-ex-test-set-mark (buf char line)
  "Set mark CHAR at line LINE in BUF."
  (with-current-buffer buf
    (unless ygg--marks-local
      (setq ygg--marks-local (make-hash-table :test 'eql)))
    (ygg-ex--goto-line line)
    (puthash char (point-marker) ygg--marks-local)))

;;; Tests for range parsing

(ert-deftest ygg-ex-test-parse-range-line-numbers ()
  "Range parsing with line numbers."
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (yggdrasil-local-mode 1)
    (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range "1,3s/a/b/")))
      (should (equal range '(1 . 3)))
      (should (equal rest "s/a/b/")))))

(ert-deftest ygg-ex-test-parse-range-current-and-offset ()
  "Range parsing with current line and offset."
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (yggdrasil-local-mode 1)
    (goto-line 2)
    (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range ".,+3s/a/b/")))
      (should (equal range '(2 . 5)))
      (should (equal rest "s/a/b/")))))

(ert-deftest ygg-ex-test-parse-range-marks ()
  "Range parsing with mark addresses."
  (let ((buf (ygg-ex-test-setup-buffer "a\nb\nc\nd\ne\n")))
    (unwind-protect
        (ygg-ex-test-set-mark buf ?a 1)
        (ygg-ex-test-set-mark buf ?b 3)
        (with-current-buffer buf
          (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range "'a,'bs/x/y/")))
            (should (equal range '(1 . 3)))
            (should (equal rest "s/x/y/"))))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-parse-range-pattern-forward ()
  "Range parsing with forward pattern address."
  (let ((buf (ygg-ex-test-setup-buffer "foo\nbar\nfoo\nbaz\n")))
    (unwind-protect
        (with-current-buffer buf
          (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range "/foo/s/a/b/")))
            (should (equal range '(3 . 3)))
            (should (equal rest "s/a/b/"))))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-parse-range-pattern-backward ()
  "Range parsing with backward pattern address."
  (let ((buf (ygg-ex-test-setup-buffer "foo\nbar\nfoo\nbaz\n")))
    (unwind-protect
        (with-current-buffer buf
          (goto-line 4)
          (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range "?foo?s/a/b/")))
            (should (equal range '(3 . 3)))
            (should (equal rest "s/a/b/"))))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-parse-range-semicolon-separator ()
  "Range parsing with semicolon separator (relative address)."
  (let ((buf (ygg-ex-test-setup-buffer "a\ny\nx\ny\n")))
    (unwind-protect
        (with-current-buffer buf
          (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range "/x/;/y/s/a/b/")))
            (should (equal range '(3 . 4)))
            (should (equal rest "s/a/b/"))))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-parse-range-percent ()
  "Range parsing with % (entire buffer)."
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (yggdrasil-local-mode 1)
    (pcase-let ((`(,range . ,rest) (ygg-ex--parse-range "%s/a/b/")))
      (should (equal range '(1 . 5)))
      (should (equal rest "s/a/b/")))))

;;; Tests for :s preview

(ert-deftest ygg-ex-test-preview-update-creates-overlays ()
  "Preview update creates overlays for matches."
  (let ((buf (ygg-ex-test-setup-buffer "aaa\nbbb\naaa\n")))
    (unwind-protect
        (with-current-buffer buf
          (ygg-ex--preview-clear)
          (should (null ygg-ex--preview-overlays))
          (ygg-ex--preview-update buf "%s/a/b/")
          (should (> (length ygg-ex--preview-overlays) 0))
          (let ((count 0))
            (dolist (ov ygg-ex--preview-overlays)
              (when (overlay-get ov 'ygg-ex-preview) (setq count (1+ count))))
            (should (> count 0))))
      (ygg-ex--preview-clear)
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-preview-clear-removes-overlays ()
  "Preview clear removes all overlays."
  (let ((buf (ygg-ex-test-setup-buffer "aaa\nbbb\n")))
    (unwind-protect
        (with-current-buffer buf
          (ygg-ex--preview-update buf "%s/a/b/")
          (should (> (length ygg-ex--preview-overlays) 0))
          (ygg-ex--preview-clear)
          (should (null ygg-ex--preview-overlays)))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-preview-handles-empty-pattern ()
  "Preview handles empty or invalid patterns gracefully."
  (let ((buf (ygg-ex-test-setup-buffer "aaa\nbbb\n")))
    (unwind-protect
        (with-current-buffer buf
          (ygg-ex--preview-clear)
          (ygg-ex--preview-update buf "%s//b/")
          (ygg-ex--preview-update buf "%s/(")
          (should (null ygg-ex--preview-overlays)))
      (kill-buffer buf))))

;;; Tests for history

(ert-deftest ygg-ex-test-history-records-commands ()
  "History records executed ex commands."
  (let ((buf (ygg-ex-test-setup-buffer "test\n")))
    (unwind-protect
        (with-current-buffer buf
          (let ((ygg-ex-history nil))
            (setq ygg-ex-history nil)
            (add-to-history 'ygg-ex-history "w")
            (add-to-history 'ygg-ex-history "q")
            (should (equal (car ygg-ex-history) "q"))
            (should (equal (cadr ygg-ex-history) "w"))))
      (kill-buffer buf))))

;;; Tests for @: repeat

(ert-deftest ygg-ex-test-macro-play-colon ()
  "@: plays the last ex command."
  (let ((buf (ygg-ex-test-setup-buffer "aaa\nbbb\nccc\n")))
    (unwind-protect
        (with-current-buffer buf
          (let ((ygg-ex-history nil))
            (setq ygg-ex-history (list "%s/a/b/"))
            (ygg-ex-repeat-last 1)
            (should (string-match "bbb" (buffer-substring-no-properties (point-min) (point-max))))))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-macro-play-colon-multiple ()
  "@: plays the last ex command multiple times."
  (let ((buf (ygg-ex-test-setup-buffer "aaa\n")))
    (unwind-protect
        (with-current-buffer buf
          (let ((ygg-ex-history nil))
            (setq ygg-ex-history (list "s/a/b/"))
            (ygg-ex-repeat-last 3)
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (or (string-match "bbb" text)
                          (string-match "bba" text))))))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-macro-play-colon-error-on-empty ()
  "@: errors when history is empty."
  (let ((buf (ygg-ex-test-setup-buffer "test\n")))
    (unwind-protect
        (with-current-buffer buf
          (let ((ygg-ex-history nil))
            (setq ygg-ex-history nil)
            (should-error (ygg-ex-repeat-last 1))))
      (kill-buffer buf))))

;;; Tests for PCRE support

(ert-deftest ygg-ex-test-preview-pcre-dialect ()
  "Preview respects PCRE dialect setting."
  (let ((buf (ygg-ex-test-setup-buffer "aaa1 aaa2 aaa3\n")))
    (unwind-protect
        (with-current-buffer buf
          (when (require 'pcre2el nil t)
            (let ((ygg-pcre-regexps t))
              (ygg-ex--preview-clear)
              (ygg-ex--preview-update buf "%s/\\d/X/g")
              (should (> (length ygg-ex--preview-overlays) 0)))))
      (ygg-ex--preview-clear)
      (kill-buffer buf))))

(provide 'ygg-ex-tests)
;;; ygg-ex-tests.el ends here
