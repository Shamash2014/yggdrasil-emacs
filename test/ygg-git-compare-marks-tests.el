;;; ygg-git-compare-marks-tests.el --- hunks of a compare checked off as reviewed -*- lexical-binding: t; -*-

;;; Code:

(let ((builds (expand-file-name "../elpaca/builds/"
                                (file-name-directory
                                 (or load-file-name buffer-file-name)))))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds))))

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-compare-marks)

(defun ygg-git-compare-marks-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-compare-marks-tests--lines (&rest changes)
  "Twenty numbered lines, each (N . TEXT) of CHANGES putting TEXT at line N."
  (mapconcat (lambda (n) (concat (or (alist-get n changes) (format "line %d" n)) "\n"))
             (number-sequence 1 20) ""))

(defmacro ygg-git-compare-marks-tests--with-compare (root &rest body)
  "BODY in a compare of HEAD against ROOT's worktree, where a.txt changes line 15."
  (declare (indent 1))
  `(let* ((process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (,root (file-name-as-directory
                  (file-truename (make-temp-file "ygg-git-compare-marks-" t))))
          (default-directory ,root)
          (magit-refresh-verbose nil)
          (inhibit-message t))
     (unwind-protect
         (cl-flet ((git (&rest args) (apply #'ygg-git-compare-marks-tests--git ,root args))
                   (write (text) (with-temp-file (expand-file-name "a.txt" ,root)
                                   (insert text))))
           (git "init" "-q" "-b" "main")
           (git "config" "user.name" "Marks Test")
           (git "config" "user.email" "marks@example.invalid")
           (git "config" "commit.gpgsign" "false")
           (write (ygg-git-compare-marks-tests--lines))
           (git "add" ".")
           (git "commit" "-q" "-m" "base")
           (write (ygg-git-compare-marks-tests--lines '(15 . "fifteen")))
           (with-current-buffer (ygg-git-compare-buffer ,root '(rev . "HEAD")
                                                        (cons 'worktree ,root))
             ,@body))
       (dolist (b (buffer-list))
         (when (and (with-current-buffer b (derived-mode-p 'magit-mode))
                    (file-in-directory-p (buffer-local-value 'default-directory b) ,root))
           (kill-buffer b)))
       (delete-directory ,root t))))

(defun ygg-git-compare-marks-tests--hunk (text)
  "The hunk whose changed lines include TEXT."
  (seq-find (lambda (h) (string-match-p (regexp-quote text)
                                        (buffer-substring (oref h content) (oref h end))))
            (ygg-git-compare-marks--hunks)))

(defun ygg-git-compare-marks-tests--marked-p (hunk)
  (seq-some (lambda (o) (overlay-get o 'before-string))
            (overlays-at (oref hunk start))))

(defun ygg-git-compare-marks-tests--mark (text)
  (goto-char (oref (ygg-git-compare-marks-tests--hunk text) start))
  (ygg-git-compare-mark-hunk-reviewed))

(ert-deftest ygg-git-compare-marks-hold-when-lines-above-shift ()
  (ygg-git-compare-marks-tests--with-compare root
    (ygg-git-compare-marks-tests--mark "fifteen")
    (should (ygg-git-compare-marks-tests--marked-p
             (ygg-git-compare-marks-tests--hunk "fifteen")))
    (with-temp-file (expand-file-name "a.txt" root)
      (insert "new top\n" (ygg-git-compare-marks-tests--lines '(15 . "fifteen"))))
    (ygg-git-compare-refresh)
    (let ((top (ygg-git-compare-marks-tests--hunk "new top"))
          (fifteen (ygg-git-compare-marks-tests--hunk "fifteen")))
      (should-not (eq top fifteen))
      (should (ygg-git-compare-marks-tests--marked-p fifteen))
      (should (oref fifteen hidden))
      (should-not (ygg-git-compare-marks-tests--marked-p top)))))

(ert-deftest ygg-git-compare-marks-lapse-when-the-hunk-changes ()
  (ygg-git-compare-marks-tests--with-compare root
    (ygg-git-compare-marks-tests--mark "fifteen")
    (with-temp-file (expand-file-name "a.txt" root)
      (insert (ygg-git-compare-marks-tests--lines '(15 . "fifteen, again"))))
    (ygg-git-compare-refresh)
    (let ((hunk (ygg-git-compare-marks-tests--hunk "fifteen")))
      (should-not (ygg-git-compare-marks-tests--marked-p hunk))
      (should-not (oref hunk hidden)))))

(ert-deftest ygg-git-compare-marks-round-trip-through-the-git-dir ()
  (ygg-git-compare-marks-tests--with-compare root
    (let ((path (ygg-git-compare-marks--path)))
      (should (file-in-directory-p path (expand-file-name ".git" root)))
      (should (= 0 (hash-table-count (ygg-git-compare-marks--read))))
      (ygg-git-compare-marks-tests--mark "fifteen")
      (let ((key (ygg-git-compare-marks--key (ygg-git-compare-marks-tests--hunk "fifteen"))))
        (should (equal (mapcar #'car (with-temp-buffer (insert-file-contents path)
                                                       (read (current-buffer))))
                       (list key)))
        (should (gethash key (ygg-git-compare-marks--read)))
        (ygg-git-compare-marks-tests--mark "fifteen")
        (should-not (gethash key (ygg-git-compare-marks--read))))
      (with-temp-file path (insert "(unclosed \"junk"))
      (should (= 0 (hash-table-count (ygg-git-compare-marks--read)))))))

(ert-deftest ygg-git-compare-marks-unreviewed-only-hides-what-is-done ()
  (ygg-git-compare-marks-tests--with-compare root
    (with-temp-file (expand-file-name "a.txt" root)
      (insert (ygg-git-compare-marks-tests--lines '(2 . "two") '(15 . "fifteen"))))
    (ygg-git-compare-refresh)
    (ygg-git-compare-toggle-unreviewed)
    (let ((two (ygg-git-compare-marks-tests--hunk "two"))
          (fifteen (ygg-git-compare-marks-tests--hunk "fifteen")))
      (ygg-git-compare-marks-tests--mark "two")
      (should (= (point) (oref fifteen start)))
      (magit-section-show (oref two parent))
      (should (invisible-p (oref two start)))
      (should-not (invisible-p (oref fifteen start)))
      (ygg-git-compare-mark-file-reviewed)
      (should (invisible-p (oref (oref two parent) start)))
      (ygg-git-compare-toggle-unreviewed)
      (should-not (invisible-p (oref (oref two parent) start)))
      (should (ygg-git-compare-marks-tests--marked-p two)))))

(ert-deftest ygg-git-compare-marks-next-goes-past-reviewed-hunks ()
  (ygg-git-compare-marks-tests--with-compare root
    (with-temp-file (expand-file-name "a.txt" root)
      (insert (ygg-git-compare-marks-tests--lines '(2 . "two") '(15 . "fifteen"))))
    (ygg-git-compare-refresh)
    (ygg-git-compare-marks-tests--mark "two")
    (goto-char (point-min))
    (ygg-git-compare-next-unreviewed)
    (should (= (point) (oref (ygg-git-compare-marks-tests--hunk "fifteen") start)))
    (should-error (ygg-git-compare-next-unreviewed) :type 'user-error)))

(ert-deftest ygg-git-compare-marks-previous-goes-back-past-reviewed-hunks ()
  (ygg-git-compare-marks-tests--with-compare root
    (with-temp-file (expand-file-name "a.txt" root)
      (insert (ygg-git-compare-marks-tests--lines '(2 . "two") '(10 . "ten") '(18 . "eighteen"))))
    (ygg-git-compare-refresh)
    (ygg-git-compare-marks-tests--mark "ten")
    (goto-char (point-max))
    (ygg-git-compare-previous-unreviewed)
    (should (= (point) (oref (ygg-git-compare-marks-tests--hunk "eighteen") start)))
    (ygg-git-compare-previous-unreviewed)
    (should (= (point) (oref (ygg-git-compare-marks-tests--hunk "two") start)))
    (should-error (ygg-git-compare-previous-unreviewed) :type 'user-error)
    (should (eq (keymap-lookup ygg-git-compare-mode-map "[ u")
                #'ygg-git-compare-previous-unreviewed))))

(ert-deftest ygg-git-compare-marks-from-the-right-pane-reach-the-list ()
  (ygg-git-compare-marks-tests--with-compare root
    (let* ((list (current-buffer))
           (ygg-git-compare--file-window (selected-window))
           (pane (ygg-git-compare--show-file "a.txt")))
      (with-current-buffer pane
        (ygg-git-compare-marks-tests--mark "fifteen")
        (should (ygg-git-compare-marks-tests--marked-p
                 (ygg-git-compare-marks-tests--hunk "fifteen"))))
      (should (ygg-git-compare-marks-tests--marked-p
               (oref (ygg-git-compare-marks-tests--hunk "fifteen") parent)))
      (with-current-buffer (ygg-git-compare--show-file "a.txt")
        (should (ygg-git-compare-marks-tests--marked-p
                 (ygg-git-compare-marks-tests--hunk "fifteen"))))
      (should (eq list (current-buffer))))))

(ert-deftest ygg-git-compare-marks-identical-hunks-are-marked-apart ()
  (ygg-git-compare-marks-tests--with-compare root
    (let ((file (expand-file-name "a.txt" root)))
      (with-temp-file file
        (insert (ygg-git-compare-marks-tests--lines '(3 . "same") '(18 . "same"))))
      (ygg-git-compare-marks-tests--git root "commit" "-q" "-am" "twins")
      (with-temp-file file
        (insert (ygg-git-compare-marks-tests--lines '(3 . "other") '(18 . "other")))))
    (ygg-git-compare-refresh)
    (let ((first (nth 0 (ygg-git-compare-marks--hunks)))
          (second (nth 1 (ygg-git-compare-marks--hunks))))
      (should-not (equal (ygg-git-compare-marks--key first)
                         (ygg-git-compare-marks--key second)))
      (goto-char (oref second start))
      (ygg-git-compare-mark-hunk-reviewed)
      (let ((marks (ygg-git-compare-marks--read)))
        (should-not (gethash (ygg-git-compare-marks--key first) marks))
        (should (gethash (ygg-git-compare-marks--key second) marks)))
      (should-not (ygg-git-compare-marks-tests--marked-p (nth 0 (ygg-git-compare-marks--hunks))))
      (should (ygg-git-compare-marks-tests--marked-p (nth 1 (ygg-git-compare-marks--hunks)))))))

(ert-deftest ygg-git-compare-marks-keep-the-newest ()
  (ygg-git-compare-marks-tests--with-compare root
    (let ((marks (make-hash-table :test #'equal))
          (ygg-git-compare-marks-keep 3))
      (dotimes (n 5) (puthash (format "k%d" n) (* 10 n) marks))
      (ygg-git-compare-marks--write marks)
      (should (equal (sort (hash-table-keys (ygg-git-compare-marks--read)) #'string<)
                     '("k2" "k3" "k4"))))))

(ert-deftest ygg-git-compare-marks-read-keys-kept-without-times ()
  (ygg-git-compare-marks-tests--with-compare root
    (with-temp-file (ygg-git-compare-marks--path) (insert "(\"old\" (\"new\" . 5))"))
    (let ((marks (ygg-git-compare-marks--read)))
      (should (gethash "old" marks))
      (should (= (gethash "new" marks) 5)))
    (let ((ygg-git-compare-marks-keep 1))
      (ygg-git-compare-marks--write (ygg-git-compare-marks--read)))
    (should (equal (hash-table-keys (ygg-git-compare-marks--read)) '("new")))))

(provide 'ygg-git-compare-marks-tests)
;;; ygg-git-compare-marks-tests.el ends here
