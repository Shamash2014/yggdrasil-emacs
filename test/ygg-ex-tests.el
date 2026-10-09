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

;;; Repeat substitute, mark addresses, :k, :marks, :reg, dot-repeat

(defmacro ygg-ex-test-with (text &rest body)
  (declare (indent 1))
  `(let ((buf (ygg-ex-test-setup-buffer ,text)) (ygg-ex--last-substitute nil))
     (unwind-protect (with-current-buffer buf ,@body)
       (kill-buffer buf))))

(ert-deftest ygg-ex-test-substitute-remembers-and-amp-repeats ()
  (ygg-ex-test-with "aa\naa\naa\n"
    (ygg-ex--execute "1s/a/b/g")
    (should (equal ygg-ex--last-substitute '("a" "b" "g")))
    (ygg-ex--execute "2&&")
    (should (equal (buffer-string) "bb\nbb\naa\n"))
    (ygg-ex--execute "3&")
    (should (equal (buffer-string) "bb\nbb\nba\n"))
    (ygg-ex--execute "3&g")
    (should (equal (buffer-string) "bb\nbb\nbb\n"))))

(ert-deftest ygg-ex-test-substitute-empty-pattern-reuses-last ()
  (ygg-ex-test-with "ab\nab\n"
    (ygg-ex--execute "1s/a/x/")
    (ygg-ex--execute "2s//y/")
    (should (equal (buffer-string) "xb\nyb\n"))))

(ert-deftest ygg-ex-test-repeat-substitute-without-history-errors ()
  (ygg-ex-test-with "a\n"
    (should-error (ygg-ex--execute "&&") :type 'user-error)))

(ert-deftest ygg-ex-test-g-amp-repeats-on-all-lines ()
  (ygg-ex-test-with "aa\naa\naa\n"
    (ygg-ex--execute "1s/a/b/g")
    (ygg-ex-repeat-substitute-all)
    (should (equal (buffer-string) "bb\nbb\nbb\n"))))

(ert-deftest ygg-ex-test-g-amp-bound-in-goto-map ()
  (should (eq (lookup-key ygg-goto-map "&") #'ygg-ex-repeat-substitute-all)))

(ert-deftest ygg-ex-test-mark-address-uppercase-and-specials ()
  (ygg-ex-test-with "1\n2\n3\n4\n"
    (let ((ygg--marks-global nil))
      (ygg-ex--goto-line 3)
      (ygg-ex--execute "kA")
      (ygg-ex--goto-line 1)
      (should (equal (car (ygg-ex--parse-range "'A,$")) '(3 . 4)))
      (setq ygg--mark-last-yank-beg (copy-marker (save-excursion (ygg-ex--goto-line 2) (point))))
      (should (equal (car (ygg-ex--parse-range "'[")) '(2 . 2)))
      (should (= (line-number-at-pos) 1)))))

(ert-deftest ygg-ex-test-mark-address-unset-errors ()
  (ygg-ex-test-with "1\n2\n"
    (let ((ygg--marks-global nil))
      (should-error (ygg-ex--parse-range "'Z") :type 'user-error))))

(ert-deftest ygg-ex-test-k-sets-lowercase-mark-at-range-line ()
  (ygg-ex-test-with "1\n2\n3\n"
    (ygg-ex--execute "2ka")
    (should (= (ygg-ex--mark-line ?a) 2))
    (ygg-ex--execute "k b")
    (should (= (ygg-ex--mark-line ?b) 1))))

(ert-deftest ygg-ex-test-marks-lists-in-quickfix-and-jumps ()
  (require 'layer-quickfix)
  (ygg-ex-test-with "one\ntwo\nthree\n"
    (ygg-ex--execute "3ka")
    (let ((rows (ygg-ex--marks-collect buf)))
      (should (= (length rows) 1))
      (should (string-match-p "three" (nth 1 (car rows))))
      (ygg-ex--goto-line 1)
      (save-window-excursion
        (switch-to-buffer buf)
        (ygg-ex--marks-open (car (car rows)))
        (should (= (line-number-at-pos) 3))))))

(ert-deftest ygg-ex-test-marks-collect-includes-remote-mark ()
  (require 'tramp)
  (ygg-ex-test-with "x\n"
    (let ((ygg--marks-global (list (cons ?R (cons "/ssh:nonexistent.invalid:/x" 3))))
          (tramp-connection-timeout 2))
      (cl-letf (((symbol-function 'tramp-file-name-handler)
                 (lambda (&rest _) (error "tramp handler called"))))
        (let ((row (seq-find (lambda (r) (eq (cdr (car r)) ?R))
                             (ygg-ex--marks-collect buf))))
          (should row)
          (should (equal (nth 2 row) "x")))))))

(ert-deftest ygg-ex-test-marks-command-shows-kind ()
  (require 'layer-quickfix)
  (ygg-ex-test-with "x\n"
    (ygg-ex--execute "ka")
    (let (shown)
      (cl-letf (((symbol-function 'ygg-qf-show-kind)
                 (lambda (kind &rest args) (setq shown (cons kind args)))))
        (ygg-ex--execute "marks"))
      (should (equal shown (list 'marks buf))))))

(ert-deftest ygg-ex-test-registers-list-and-paste ()
  (require 'layer-quickfix)
  (ygg-ex-test-with "x\n"
    (let ((ygg--registers (make-hash-table :test 'eql)) (kill-ring nil))
      (puthash ?q (list "hello") ygg--registers)
      (let ((rows (ygg-ex--registers-collect buf)))
        (should (equal (cdar (car rows)) ?q))
        (should (string-match-p "hello" (nth 1 (car rows))))
        (save-window-excursion
          (switch-to-buffer buf)
          (ygg-ex--registers-open (car (car rows)))
          (should (string-match-p "hello" (buffer-string))))))))

(ert-deftest ygg-ex-test-dot-repeats-whole-ex-line ()
  (ygg-ex-test-with "a1\na2\na3\n"
    (ygg-normal-state)
    (ygg-ex--execute-journaled "2s/a/b/")
    (should (equal ygg--repeat-verb-pending (vconcat ":2s/a/b/" [return])))))

(ert-deftest ygg-ex-test-dot-after-ex-substitute-replays-ex ()
  (let ((buf (ygg-ex-test-setup-buffer "a1\na2\na3\n")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buf)
          (ygg-normal-state)
          (ygg-set-selection 1 1)
          (execute-kbd-macro (vconcat ":s/a/b/" [return]))
          (should (equal (buffer-string) "b1\na2\na3\n"))
          (goto-char (point-min))
          (forward-line 1)
          (ygg-set-selection (point) (point))
          (execute-kbd-macro (kbd "."))
          (should (equal (buffer-string) "b1\nb2\na3\n")))
      (kill-buffer buf))))

(ert-deftest ygg-ex-test-marks-global-in-unvisited-file-opens-no-buffer ()
  (require 'layer-quickfix)
  (let ((file (make-temp-file "ygg-mark" nil ".txt" "one\ntwo\nthree\n")))
    (unwind-protect
        (ygg-ex-test-with "x\n"
          (let ((ygg--marks-global (list (cons ?A (cons file 6))))
                (before (buffer-list)))
            (let ((rows (ygg-ex--marks-collect buf)))
              (should (cl-find-if (lambda (r) (and (eq (cdar r) ?A)
                                                   (string-match-p "2  two" (nth 1 r))))
                                  rows)))
            (should (equal (buffer-list) before))
            (should-not (find-buffer-visiting file))))
      (delete-file file))))

(ert-deftest ygg-ex-test-marks-row-truncates-long-lines ()
  (ygg-ex-test-with (concat (make-string 200 ?z) "\n")
    (ygg-ex--execute "ka")
    (should (<= (length (nth 1 (car (ygg-ex--marks-collect buf)))) 100))))

(ert-deftest ygg-ex-test-marks-from-quickfix-uses-origin-buffer ()
  (require 'layer-quickfix)
  (ygg-ex-test-with "x\n"
    (let (shown)
      (cl-letf (((symbol-function 'ygg-qf-show-kind)
                 (lambda (_kind b) (setq shown b))))
        (let ((qf (generate-new-buffer "*qf*")))
          (unwind-protect
              (save-window-excursion
                (switch-to-buffer buf)
                (switch-to-buffer qf)
                (with-current-buffer qf (grep-mode) (ygg-ex--cmd-marks nil nil ""))
                (should (eq shown buf)))
            (kill-buffer qf)))))))

(ert-deftest ygg-ex-test-substitute-empty-pattern-uses-last-search ()
  (ygg-ex-test-with "foo\nfoo\n"
    (setq ygg--last-search "foo")
    (ygg-ex--execute "1s//x/")
    (should (equal (buffer-string) "x\nfoo\n"))
    (setq ygg--last-search "oo")
    (ygg-ex-repeat-substitute-all)
    (should (equal (buffer-string) "x\nfx\n"))
    (should (equal ygg--last-search "oo"))))

(ert-deftest ygg-ex-test-substitute-sets-last-search ()
  (ygg-ex-test-with "ab\n"
    (ygg-ex--execute "s/a/x/")
    (should (equal ygg--last-search "a"))))

(ert-deftest ygg-ex-test-journal-clears-pending-when-command-errors ()
  (ygg-ex-test-with "a\n"
    (cl-letf (((symbol-function 'ygg-ex--execute)
               (lambda (_)
                 (setq ygg--repeat-verb-pending [?:])
                 (user-error "boom"))))
      (should-error (ygg-ex--execute-journaled "bad") :type 'user-error))
    (should-not ygg--repeat-verb-pending)))

(ert-deftest ygg-ex-test-journal-leaves-unset-pending-alone ()
  (ygg-ex-test-with "a\n"
    (cl-letf (((symbol-function 'ygg-ex--execute) #'ignore))
      (ygg-ex--execute-journaled "noop"))
    (should-not ygg--repeat-verb-pending)))

(ert-deftest ygg-ex-test-journal-follows-origin-buffer-across-switch ()
  (ygg-ex-test-with "a\n"
    (let ((other (generate-new-buffer "*other*")) (origin buf))
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'ygg-ex--execute)
                       (lambda (_)
                         (setq ygg--repeat-verb-pending [?:])
                         (switch-to-buffer other))))
              (ygg-ex--execute-journaled "b other"))
            (should (equal (buffer-local-value 'ygg--repeat-verb-pending origin)
                           (vconcat ":b other" [return]))))
        (kill-buffer other)))))

(ert-deftest ygg-ex-test-registers-paste-journals-as-normal-paste ()
  (require 'layer-quickfix)
  (ygg-ex-test-with "x\n"
    (let ((ygg--registers (make-hash-table :test 'eql)) (kill-ring (list "kk")))
      (puthash ?q (list "hello") ygg--registers)
      (save-window-excursion
        (switch-to-buffer buf)
        (ygg-normal-state)
        (setq ygg--repeat-tick (buffer-chars-modified-tick))
        (ygg-ex--registers-open (cons buf ?q))
        (should (equal ygg--repeat-verb-pending [?\" ?q ?p]))
        (setq ygg--repeat-verb-pending nil)
        (ygg-ex--registers-open (cons buf ?\"))
        (should (string-match-p "kk" (buffer-string)))
        (should (equal ygg--repeat-verb-pending [?p]))))))

(ert-deftest ygg-ex-test-replayed-ex-line-stays-out-of-history ()
  (let ((ygg-ex-history '("keep")) (ygg--replaying t))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _t _pred _r _i hist)
                 (should (eq hist 'ygg-ex--replay-history))
                 "")))
      (ygg-ex))
    (should (equal ygg-ex-history '("keep")))))

(provide 'ygg-ex-tests)
;;; ygg-ex-tests.el ends here

(ert-deftest ygg-ex-test-marks-remote-global-mark-never-connects ()
  (require 'layer-quickfix)
  (ygg-ex-test-with "x\n"
    (let ((ygg--marks-global (list (cons ?A (cons "/ssh:nohost:/x" 3))))
          (reads 0))
      (cl-letf (((symbol-function 'insert-file-contents)
                 (lambda (f &rest _) (when (file-remote-p f) (cl-incf reads))))
                ((symbol-function 'file-readable-p)
                 (lambda (f) (if (file-remote-p f) (error "remote touched") t))))
        (let ((row (cl-find ?A (ygg-ex--marks-collect buf) :key #'cdar)))
          (should row)
          (should (= reads 0)))))))

(ert-deftest ygg-ex-test-marks-unopened-file-reads-bounded-slice ()
  (require 'layer-quickfix)
  (let ((file (make-temp-file "ygg-mark" nil ".txt" (concat (make-string 5000 ?a) "\nhit\n" (make-string 200000 ?b))))
        ends)
    (unwind-protect
        (ygg-ex-test-with "x\n"
          (let ((ygg--marks-global (list (cons ?A (cons file 5002)))))
            (cl-letf* ((orig (symbol-function 'insert-file-contents))
                       ((symbol-function 'insert-file-contents)
                        (lambda (f v b e &rest r)
                          (push e ends) (apply orig f v b e r))))
              (let ((row (cl-find ?A (ygg-ex--marks-collect buf) :key #'cdar)))
                (should (string-match-p "2  hit" (nth 1 row)))))
            (should (and ends (car ends) (<= (car ends) 30000)))
            (cl-letf (((symbol-value 'ygg-ex--mark-file-slice-cap) 100))
              (should (string-match-p "0  $" (nth 1 (cl-find ?A (ygg-ex--marks-collect buf)
                                                             :key #'cdar)))))))
      (delete-file file))))

(ert-deftest ygg-ex-test-repeat-substitute-after-empty-pattern ()
  (ygg-ex-test-with "aa\naa\n"
    (ygg-ex--execute "s/a/b/")
    (ygg-ex--execute "s//c/")
    (should (equal ygg-ex--last-substitute '("a" "c" "")))
    (ygg-ex--execute "&")
    (should (equal (buffer-string) "bc\nbc\n"))))

(ert-deftest ygg-ex-test-global-empty-pattern-uses-last-search ()
  (ygg-ex-test-with "ax\nb\nax\n"
    (setq ygg--last-search (ygg-regexp "ax"))
    (ygg-ex--execute "g//d")
    (should (equal (buffer-string) "b\n"))))

(ert-deftest ygg-ex-test-bare-quote-address-is-user-error ()
  (ygg-ex-test-with "1\n2\n"
    (should-error (ygg-ex--parse-range "1,'") :type 'user-error)))
