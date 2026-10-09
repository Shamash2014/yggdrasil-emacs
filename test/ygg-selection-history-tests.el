;;; ygg-selection-history-tests.el --- Tests for M-u / M-U selection history -*- lexical-binding: t; -*-

(require 'ert)
(require 'yggdrasil)
(require 'yggdrasil-selection)

(defmacro ygg-sh--with (content &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (yggdrasil-local-mode 1)
     (insert ,content)
     (goto-char (point-min))
     (ygg-normal-state)
     ,@body))

(defun ygg-sh--select (a b)
  (ygg-set-selection a b)
  (ygg--post-command))

(defun ygg-sh--text ()
  (pcase-let ((`(,b ,e ,_) (ygg-selection-effective-bounds)))
    (buffer-substring-no-properties b e)))

(ert-deftest ygg-sh-undo-redo ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg-sh--select 1 4)
    (ygg-sh--select 5 8)
    (ygg-sh--select 9 12)
    (ygg-selection-undo)
    (should (equal (ygg-sh--text) "bbb"))
    (ygg-selection-undo)
    (should (equal (ygg-sh--text) "aaa"))
    (ygg-selection-redo)
    (should (equal (ygg-sh--text) "bbb"))
    (ygg--post-command)
    (should (equal (ygg-sh--text) "bbb"))))

(ert-deftest ygg-sh-new-change-truncates-forward ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg-sh--select 1 4)
    (ygg-sh--select 5 8)
    (ygg-selection-undo)
    (ygg-sh--select 9 12)
    (should-error (ygg-selection-redo) :type 'user-error)
    (ygg-selection-undo)
    (should (equal (ygg-sh--text) "aaa"))))

(ert-deftest ygg-sh-edit-restores-recorded-positions ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg-sh--select 5 8)
    (ygg-sh--select 9 12)
    (save-excursion (goto-char 1) (insert "XX"))
    (ygg--post-command)
    (ygg-selection-undo)
    (should (equal (cons (point) (mark t)) '(11 . 9)))))

(ert-deftest ygg-sh-multi-selection-primary ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg-sh--select 1 4)
    (ygg-add-selection 5 8)
    (ygg-sh--select 9 12)
    (ygg-clear-secondaries)
    (ygg--post-command)
    (should (= (ygg-selections-count) 1))
    (ygg-selection-undo)
    (should (= (ygg-selections-count) 2))
    (should (equal (ygg-sh--text) "ccc"))
    (should (equal (buffer-substring-no-properties
                    (overlay-start (car ygg--secondaries)) (overlay-end (car ygg--secondaries)))
                   "bbb"))))

(ert-deftest ygg-sh-unchanged-no-allocation ()
  (ygg-sh--with (make-string 400 ?a)
    (ygg-sh--select 1 4)
    (dotimes (i 50) (ygg-add-selection (+ 10 (* i 6)) (+ 13 (* i 6))))
    (ygg--post-command)
    (let ((cur ygg--sel-cur) (back ygg--sel-back)
          (run (progn (byte-compile 'ygg--sel-snapshot-current-p)
                      (byte-compile 'ygg--sel-record)
                      (byte-compile (lambda () (dotimes (_ 200) (ygg--sel-record)))))))
      (garbage-collect)
      (let ((before (car (memory-use-counts))))
        (funcall run)
        (should (< (- (car (memory-use-counts)) before) 20)))
      (should (eq cur ygg--sel-cur))
      (should (eq back ygg--sel-back)))))

(ert-deftest ygg-sh-initial-selection-restorable ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg--post-command)
    (let ((init (cons (point) (mark t))))
      (ygg-sh--select 5 8)
      (call-interactively #'ygg-selection-undo)
      (ygg--post-command)
      (should (equal (cons (point) (mark t)) init))
      (should (equal (length ygg--sel-back) 0))
      (should (= (length ygg--sel-fwd) 1)))))

(ert-deftest ygg-sh-commands-do-not-push ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg--post-command)
    (ygg-sh--select 5 8)
    (ygg-sh--select 9 12)
    (let ((n (length ygg--sel-back)))
      (call-interactively #'ygg-selection-undo)
      (ygg--post-command)
      (should (= (length ygg--sel-back) (1- n)))
      (call-interactively #'ygg-selection-redo)
      (ygg--post-command)
      (should (= (length ygg--sel-back) n))
      (should (null ygg--sel-fwd))
      (should (equal (ygg-sh--text) "ccc")))))

(ert-deftest ygg-sh-stale-positions-clamped ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg-sh--select 9 12)
    (ygg-sh--select 1 4)
    (delete-region 5 (point-max))
    (ygg--post-command)
    (ygg-selection-undo)
    (should (<= (point) (point-max)))
    (should (<= (mark t) (point-max)))))

(ert-deftest ygg-sh-macro-records-per-changed-command ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg--post-command)
    (dolist (r '((1 . 4) (5 . 8) (9 . 12))) (ygg-sh--select (car r) (cdr r)))
    (should (= (length ygg--sel-back) 3))))

(ert-deftest ygg-sh-rings-buffer-local ()
  (ygg-sh--with "aaa bbb ccc"
    (ygg-sh--select 5 8)
    (ygg-sh--select 9 12)
    (let ((n (length ygg--sel-back)))
      (ygg-sh--with "xxx yyy"
        (should (null ygg--sel-back)))
      (should (= (length ygg--sel-back) n)))))

(ert-deftest ygg-sh-ring-bounded ()
  (ygg-sh--with (make-string 300 ?a)
    (dotimes (i 150) (ygg-sh--select (1+ i) (+ i 2)))
    (should (<= (length ygg--sel-back) ygg--sel-history-limit))))

(ert-deftest ygg-sh-keys ()
  (should (eq (lookup-key ygg-normal-map (kbd "M-u")) #'ygg-selection-undo))
  (should (eq (lookup-key ygg-normal-map (kbd "M-U")) #'ygg-selection-redo)))

(provide 'ygg-selection-history-tests)
