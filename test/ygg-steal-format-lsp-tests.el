;;; ygg-steal-format-lsp-tests.el --- editorconfig vs dtrt, deferred shutdown, region format -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-lsp)
(require 'layer-format)
(require 'apheleia)
(require 'dtrt-indent)
(require 'editorconfig)
(require 'eglot)

(defconst ygg-steal--py-body
  (mapconcat #'identity
             (cl-loop repeat 12 append '("def f():" "    x = 1" "    if x:" "        return x"))
             "\n"))

(defun ygg-steal--visit-python (editorconfig)
  (let* ((dir (file-name-as-directory (file-truename (make-temp-file "ygg-ec" t))))
         (file (expand-file-name "a.py" dir))
         (editorconfig-mode nil))
    (when editorconfig
      (with-temp-file (expand-file-name ".editorconfig" dir)
        (insert "root = true\n[*]\nindent_style = space\nindent_size = 2\n")))
    (with-temp-file file (insert ygg-steal--py-body "\n"))
    (let ((dtrt-indent-verbosity 0))
      (editorconfig-mode 1)
      (dtrt-indent-global-mode 1)
      (ygg-format-install-dtrt-guard)
      (unwind-protect
          (let ((buf (find-file-noselect file)))
            (prog1 (with-current-buffer buf python-indent-offset)
              (kill-buffer buf)))
        (dtrt-indent-global-mode -1)
        (editorconfig-mode -1)
        (delete-directory dir t)))))

(ert-deftest ygg-steal-editorconfig-beats-dtrt ()
  (should (= 2 (ygg-steal--visit-python t))))

(ert-deftest ygg-steal-dtrt-runs-without-editorconfig ()
  (should (= 4 (ygg-steal--visit-python nil))))

(defconst ygg-steal--js-body
  (mapconcat #'identity
             (cl-loop repeat 12 append '("function f() {" "    var x = 1;" "    if (x) {" "        return x;" "    }" "}"))
             "\n"))

(defun ygg-steal--visit-js (editorconfig)
  (let* ((dir (file-name-as-directory (file-truename (make-temp-file "ygg-ec" t))))
         (file (expand-file-name "a.js" dir))
         (editorconfig-mode nil))
    (when editorconfig
      (with-temp-file (expand-file-name ".editorconfig" dir)
        (insert "root = true\n[*]\n" editorconfig "\n")))
    (with-temp-file file (insert ygg-steal--js-body "\n"))
    (let ((dtrt-indent-verbosity 0))
      (editorconfig-mode 1)
      (dtrt-indent-global-mode 1)
      (ygg-format-install-dtrt-guard)
      (unwind-protect
          (let ((buf (find-file-noselect file)))
            (prog1 (with-current-buffer buf (cons indent-tabs-mode js-indent-level))
              (kill-buffer buf)))
        (dtrt-indent-global-mode -1)
        (editorconfig-mode -1)
        (delete-directory dir t)))))

(ert-deftest ygg-steal-editorconfig-tab-style-beats-dtrt-space-guess ()
  (should (eq t (car (ygg-steal--visit-js "indent_style = tab")))))

(ert-deftest ygg-steal-editorconfig-indent-size-beats-dtrt ()
  (should (= 2 (cdr (ygg-steal--visit-js "indent_size = 2")))))

(ert-deftest ygg-steal-dtrt-guesses-js-without-editorconfig ()
  (should (= 4 (cdr (ygg-steal--visit-js nil)))))

(defvar ygg-steal--live nil)
(defvar ygg-steal--shutdowns nil)

(defun ygg-steal--fake-shutdown (server &rest _)
  (push server ygg-steal--shutdowns))

(defun ygg-steal--fake-managed-mode (&rest _)
  (ygg-steal--fake-shutdown 'srv))

(defmacro ygg-steal--with-fake-eglot (&rest body)
  `(let ((ygg-steal--shutdowns nil)
         (ygg-lsp-shutdown-delay 0.15)
         (ygg-lsp--shutdown-timers (make-hash-table :test #'eq)))
     (cl-letf (((symbol-function 'eglot--managed-buffers)
                (lambda (_s) ygg-steal--live))
               ((symbol-function 'jsonrpc-running-p) (lambda (_s) t)))
       (advice-add 'ygg-steal--fake-managed-mode :around #'ygg-lsp--managed-mode-a)
       (advice-add 'ygg-steal--fake-shutdown :around #'ygg-lsp--defer-shutdown-a)
       (unwind-protect (progn ,@body)
         (advice-remove 'ygg-steal--fake-managed-mode #'ygg-lsp--managed-mode-a)
         (advice-remove 'ygg-steal--fake-shutdown #'ygg-lsp--defer-shutdown-a)))))

(ert-deftest ygg-steal-shutdown-deferred-then-fires ()
  (ygg-steal--with-fake-eglot
   (setq ygg-steal--live nil)
   (ygg-steal--fake-managed-mode)
   (should-not ygg-steal--shutdowns)
   (sleep-for 0.3)
   (should (equal ygg-steal--shutdowns '(srv)))
   (should (zerop (hash-table-count ygg-lsp--shutdown-timers)))))

(ert-deftest ygg-steal-shutdown-skipped-when-buffer-rejoins ()
  (ygg-steal--with-fake-eglot
   (setq ygg-steal--live nil)
   (ygg-steal--fake-managed-mode)
   (setq ygg-steal--live '(buf))
   (sleep-for 0.3)
   (should-not ygg-steal--shutdowns)
   (should (zerop (hash-table-count ygg-lsp--shutdown-timers)))))

(ert-deftest ygg-steal-explicit-shutdown-is-immediate ()
  (ygg-steal--with-fake-eglot
   (ygg-steal--fake-shutdown 'srv)
   (should (equal ygg-steal--shutdowns '(srv)))))

(ert-deftest ygg-steal-shutdown-zero-delay-is-immediate ()
  (ygg-steal--with-fake-eglot
   (let ((ygg-lsp-shutdown-delay 0))
     (setq ygg-steal--live nil)
     (ygg-steal--fake-managed-mode)
     (should (equal ygg-steal--shutdowns '(srv))))))

(defmacro ygg-steal--with-upcase-formatter (&rest body)
  `(let ((apheleia-formatters (cons '(ygg-up . ("tr" "a-z" "A-Z")) apheleia-formatters)))
     (with-temp-buffer
       (setq-local apheleia-formatter 'ygg-up)
       ,@body)))

(ert-deftest ygg-steal-region-format-uses-apheleia-formatter ()
  (ygg-steal--with-upcase-formatter
   (insert "keep one\n    make loud\nkeep two\n")
   (let ((beg (+ (point-min) (length "keep one\n")))
         (end (+ (point-min) (length "keep one\n    make loud\n"))))
     (should (ygg-format-region beg end))
     (should (equal (buffer-string) "keep one\n    MAKE LOUD\nkeep two\n")))))

(ert-deftest ygg-steal-region-format-falls-back-without-formatter ()
  (with-temp-buffer
    (let ((apheleia-mode-alist nil))
      (insert "abc\n")
      (should-not (ygg-format-region (point-min) (point-max)))
      (should (equal (buffer-string) "abc\n")))))

(ert-deftest ygg-steal-region-format-failure-leaves-text ()
  (let ((apheleia-formatters (cons '(ygg-bad . ("false")) apheleia-formatters)))
    (with-temp-buffer
      (setq-local apheleia-formatter 'ygg-bad)
      (insert "abc\n")
      (should-not (ygg-format-region (point-min) (point-max)))
      (should (equal (buffer-string) "abc\n")))))

(defmacro ygg-steal--with-semi-formatter (&rest body)
  `(let ((apheleia-formatters (cons '(ygg-semi . ("sed" "s/$/;/")) apheleia-formatters)))
     (with-temp-buffer
       (setq-local apheleia-formatter 'ygg-semi)
       ,@body)))

(defun ygg-steal--select (beg end)
  (set-mark beg)
  (goto-char end))

(ert-deftest ygg-steal-bare-format-skips-external-formatter ()
  (ygg-steal--with-semi-formatter
   (insert "x = foo + 1\n")
   (let ((called nil))
     (ygg-steal--select 5 8)
     (cl-letf (((symbol-function 'indent-region) (lambda (&rest _) (setq called t))))
       (ygg-format))
     (should called)
     (should (equal (buffer-string) "x = foo + 1\n")))))

(ert-deftest ygg-steal-multiline-format-expands-to-whole-lines ()
  (ygg-steal--with-upcase-formatter
   (insert "a\nb c\nd e\nf\n")
   (ygg-steal--select 4 8)
   (ygg-format)
   (should (equal (buffer-string) "a\nB C\nD E\nf\n"))))

(ert-deftest ygg-steal-region-format-respects-save-when-off ()
  (ygg-steal--with-upcase-formatter
   (insert "abc\ndef\n")
   (cl-letf (((symbol-function 'ygg-format--decide) (lambda () '(prettier-javascript))))
     (should-not (ygg-format-region (point-min) (point-max))))
   (should (equal (buffer-string) "abc\ndef\n"))))

(ert-deftest ygg-steal-region-format-keeps-tabs ()
  (ygg-steal--with-upcase-formatter
   (setq-local indent-tabs-mode t tab-width 8)
   (insert "\t\tfoo\n\t\t\tbar\n")
   (should (ygg-format-region (point-min) (point-max)))
   (should (equal (buffer-string) "\t\tFOO\n\t\t\tBAR\n"))))

(ert-deftest ygg-steal-region-format-timeout-kills-formatter ()
  (let ((apheleia-formatters (cons '(ygg-slow . ("sleep" "30")) apheleia-formatters))
        (ygg-format-region-timeout 0.3)
        (messages nil))
    (with-temp-buffer
      (setq-local apheleia-formatter 'ygg-slow)
      (insert "abc\n")
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages) nil)))
        (should-not (ygg-format-region (point-min) (point-max) "eglot")))
      (should (member "format: ygg-slow timed out, used eglot" messages))
      (should (equal (buffer-string) "abc\n"))
      (should-not (seq-some (lambda (p) (string-prefix-p "apheleia-sleep" (process-name p)))
                            (process-list))))))

(ert-deftest ygg-steal-ygg-format-falls-back-to-indent ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a\nb)")
    (cl-letf (((symbol-function 'ygg-format-region) (lambda (&rest _) nil)))
      (set-mark (point-min))
      (goto-char (point-max))
      (let ((called nil))
        (cl-letf (((symbol-function 'indent-region) (lambda (&rest _) (setq called t))))
          (ygg-format))
        (should called)))))

(ert-deftest ygg-steal-save-without-format-skips-apheleia ()
  (let* ((file (make-temp-file "ygg-save" nil ".txt"))
         (ran nil))
    (unwind-protect
        (cl-letf* ((apheleia-formatters (cons '(ygg-up . ("cat")) apheleia-formatters))
                   ((symbol-function 'apheleia-format-buffer)
                   (lambda (&rest _) (setq ran t))))
          (with-current-buffer (find-file-noselect file)
            (setq-local apheleia-formatter 'ygg-up)
            (apheleia-mode 1)
            (insert "hi")
            (ygg-format-save-without-format)
            (should-not ran)
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "hi\n"))
            (should apheleia-mode)
            (insert "!")
            (save-buffer)
            (should ran)
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (delete-file file))))

(ert-deftest ygg-steal-treesit-font-lock-level ()
  (should (= treesit-font-lock-level 4)))

(provide 'ygg-steal-format-lsp-tests)

(defmacro ygg-steal--with-file-source (formatter &rest body)
  (declare (indent 1))
  `(let* ((apheleia-formatters (cons ',formatter apheleia-formatters))
          (dir (file-name-as-directory (make-temp-file "ygg-fmt" t)))
          (real (expand-file-name "real.ts" dir))
          (kills nil)
          (hook (lambda ()
                  (when (string-match-p "\\` \\*ygg-format\\*" (buffer-name))
                    (push (list (buffer-modified-p) buffer-file-name) kills)))))
     (write-region "" nil real nil 0)
     (let ((src (find-file-noselect real)))
       (unwind-protect
           (with-current-buffer src
             (setq-local apheleia-formatter ',(car formatter))
             (insert "abc\ndef\n")
             (add-hook 'kill-buffer-hook hook)
             (unwind-protect (progn ,@body)
               (remove-hook 'kill-buffer-hook hook))
             (should (equal kills '((nil nil))))
             (should (eq (find-buffer-visiting real) src)))
         (set-buffer-modified-p nil)
         (kill-buffer src)
         (delete-directory dir t)))))

(ert-deftest ygg-steal-fragment-tmp-buffer-dies-clean-and-unvisiting ()
  (ygg-steal--with-file-source (ygg-up . ("tr" "a-z" "A-Z"))
    (should (ygg-format-region (point-min) (point-max)))
    (should (equal (buffer-string) "ABC\nDEF\n"))))

(ert-deftest ygg-steal-fragment-file-arg-formatter-fails-fast ()
  (ygg-steal--with-file-source (ygg-cat . ("cat" file))
    (let ((start (float-time)))
      (should-not (ygg-format-region (point-min) (point-max)))
      (should (< (- (float-time) start) 2)))
    (should (equal (buffer-string) "abc\ndef\n"))))

(ert-deftest ygg-steal-fragment-filepath-keeps-source-extension ()
  (ygg-steal--with-file-source (ygg-path . ("sh" "-c" "cat >/dev/null; echo $0" filepath))
    (should (ygg-format-region (point-min) (point-max)))
    (should (string-match-p "/ygg-format\\.ts\n\\'" (buffer-string)))
    (should-not (string-match-p "real\\.ts" (buffer-string)))))
