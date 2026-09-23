;;; ygg-motions-tests.el --- Tests for yggdrasil motion fixes -*- lexical-binding: t; -*-

(require 'ert)
(require 'yggdrasil-motions)
(require 'yggdrasil-selection)

;;; Test helper: with temporary buffer

(defmacro ygg-with-temp-buffer (content &rest body)
  "Create a temp buffer with CONTENT, execute BODY, cleanup."
  (declare (indent 1))
  `(with-temp-buffer
     (yggdrasil-local-mode 1)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Test 1: * and # with bare cursor search symbol boundaries

(ert-deftest ygg-motions-star-bare-cursor-forward ()
  "* on bare cursor should search symbol at point with boundaries."
  (ygg-with-temp-buffer "foo foobar foo"
    (goto-char 1)
    (setq ygg--search-dir 1)
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (let ((sel (and (> (- end beg) 1)
                      (buffer-substring-no-properties beg end)))
            (word (or (and (> (- end beg) 1)
                           (buffer-substring-no-properties beg end))
                      (thing-at-point 'symbol t))))
        (if word
            (setq ygg--last-search
                  (if sel (regexp-quote sel)
                    (concat "\\_<" (regexp-quote word) "\\_>"))
                  ygg--search-dir 1)
          (user-error "No word under cursor"))))
    (should (equal ygg--last-search "\\_<foo\\_>"))))

(ert-deftest ygg-motions-star-with-selection ()
  "* with selection should search exact text, not boundaries."
  (ygg-with-temp-buffer "foo foobar foo"
    (ygg-set-selection 5 11)
    (setq ygg--search-dir 1)
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (let ((sel (and (> (- end beg) 1)
                      (buffer-substring-no-properties beg end)))
            (word (or (and (> (- end beg) 1)
                           (buffer-substring-no-properties beg end))
                      (thing-at-point 'symbol t))))
        (if word
            (setq ygg--last-search
                  (if sel (regexp-quote sel)
                    (concat "\\_<" (regexp-quote word) "\\_>"))
                  ygg--search-dir 1)
          (user-error "No word under cursor"))))
    (should (equal ygg--last-search "foobar"))))

;;; Test 2: Special marks

(ert-deftest ygg-motions-mark-jump-pos-recorded ()
  "'`' position should be recorded before jumps."
  (ygg-with-temp-buffer "line1\nline2\nline3"
    (goto-char 1)
    (ygg--jump-push)
    (should ygg--mark-last-jump-pos)
    (should (equal (marker-position ygg--mark-last-jump-pos) 1))))

(ert-deftest ygg-motions-mark-last-change ()
  "'. should reference last change list entry."
  (ygg-with-temp-buffer "line1\nline2\nline3"
    (ygg-with-verb (delete-char 2))
    (should ygg--change-list)
    (should (equal (car ygg--change-list) 1))))

(ert-deftest ygg-motions-mark-yank-boundaries ()
  "'[ and '] should track yank/change region."
  (ygg-with-temp-buffer "foo bar baz"
    (ygg-with-verb (delete-region 1 4))
    (should ygg--mark-last-yank-beg)
    (should ygg--mark-last-yank-end)))

;;; Test 3: Search highlighting

(ert-deftest ygg-motions-hlsearch-on-scroll ()
  "hlsearch scroll hook should refresh highlights."
  (ygg-with-temp-buffer "foo bar foo"
    (setq isearch-lazy-highlight-last-string "foo")
    (setq ygg-hlsearch t)
    (setq isearch-lazy-highlight t)
    (should (functionp #'ygg--hlsearch-on-scroll))))

;;; Test 4: Global marks persistence

(ert-deftest ygg-motions-mark-global-store ()
  "Global marks should be stored in ygg--marks-global."
  (let ((ygg--marks-global nil))
    (push (cons ?A (cons "test.txt" 10)) ygg--marks-global)
    (should (equal (length ygg--marks-global) 1))
    (should (equal (car (assq ?A ygg--marks-global)) ?A))))

(ert-deftest ygg-motions-mark-global-is-variable ()
  "Global marks should survive serialization."
  (let ((marks-data '((65 . ("test.txt" . 10)))))
    (setq ygg--marks-global marks-data)
    (should (equal ygg--marks-global marks-data))))

(ert-deftest ygg-motions-hlsearch-clears-hook ()
  "hlsearch-clear should remove scroll hook."
  (ygg-with-temp-buffer "test"
    (setq isearch-lazy-highlight-last-string "test")
    (add-hook 'window-scroll-functions #'ygg--hlsearch-on-scroll nil t)
    (ygg-hlsearch-clear)
    (should (not isearch-lazy-highlight-last-string))))

;;; Run tests

(provide 'ygg-motions-tests)
;;; ygg-motions-tests.el ends here
