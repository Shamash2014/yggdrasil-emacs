;;; ygg-markdown-large-open-tests.el --- Big markdown files open without a full propertize -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'markdown-mode)
(require 'layer-markdown)

(defun ygg-markdown-large-open-tests--file (lines &optional paragraphs)
  (let ((file (make-temp-file "ygg-md-large" nil ".md")))
    (with-temp-file file
      (dotimes (i lines)
        (insert (format "- item %d with `code` and *emphasis*\n%s" i
                      (if (and paragraphs (zerop (% i 10))) "\n" "")))))
    file))

(defmacro ygg-markdown-large-open-tests--open (var lines paragraphs &rest body)
  "Open a generated file of LINES in `gfm-mode' as VAR and run BODY."
  (declare (indent 3))
  `(let ((file (ygg-markdown-large-open-tests--file ,lines ,paragraphs)))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (unwind-protect
               (let ((,var (current-buffer)))
                 ,@body)
             (kill-buffer)))
       (delete-file file))))

(defun ygg-markdown-large-open-tests--middle-bounds ()
  "List-item bounds and next-item distance at the middle line, as redisplay would see them."
  (goto-char (/ (point-max) 2))
  (let ((line (line-beginning-position)))
    (syntax-propertize (line-end-position 3))
    (let ((bounds (markdown-cur-list-item-bounds)))
      (list (- (nth 0 bounds) line)
            (- (nth 1 bounds) line)
            (nth 2 bounds)
            (nth 3 bounds)
            (progn (markdown-next-list-item 1) (- (point) line))))))

(defun ygg-markdown-large-open-tests--drain ()
  (let ((n 0))
    (while (and (< n 1000)
                (not (ygg-markdown--propertize-chunk (current-buffer))))
      (cl-incf n))
    n))

(ert-deftest ygg-markdown-large-open-skips-full-propertize ()
  (let ((auto-mode-alist '(("\\.md\\'" . gfm-mode))))
    (ygg-markdown-large-open-tests--open buf 30000 t
      (should (> (buffer-size) ygg-markdown-large-size))
      (should (eq major-mode 'gfm-mode))
      (should (< syntax-propertize--done (point-max)))
      (should (timerp ygg-markdown--propertize-timer))
      (should (> (ygg-markdown-large-open-tests--drain) 0))
      (should (>= syntax-propertize--done (point-max))))))

(ert-deftest ygg-markdown-large-open-imenu-skips-fenced-headings ()
  (let ((auto-mode-alist '(("\\.md\\'" . gfm-mode))))
    (ygg-markdown-large-open-tests--open buf 30000 t
      (goto-char (point-max))
      (insert "# real\n\n```\n# fake\n```\n")
      (should (< syntax-propertize--done (point-max)))
      (let ((index (format "%S" (funcall imenu-create-index-function))))
        (should (string-match-p "real" index))
        (should-not (string-match-p "fake" index))))))

(defvar ygg-markdown-large-open-tests--seen nil)

(defun ygg-markdown-large-open-tests--hook ()
  (syntax-propertize (point-max))
  (setq ygg-markdown-large-open-tests--seen syntax-propertize--done))

(ert-deftest ygg-markdown-large-open-mode-hook-propertize-is-honoured ()
  (let ((auto-mode-alist '(("\\.md\\'" . markdown-mode)))
        (ygg-markdown-large-open-tests--seen nil))
    (add-hook 'markdown-mode-hook #'ygg-markdown-large-open-tests--hook)
    (unwind-protect
        (ygg-markdown-large-open-tests--open buf 30000 t
          (should (>= ygg-markdown-large-open-tests--seen (point-max))))
      (remove-hook 'markdown-mode-hook #'ygg-markdown-large-open-tests--hook))))

(ert-deftest ygg-markdown-small-open-propertizes-everything ()
  (let ((auto-mode-alist '(("\\.md\\'" . gfm-mode))))
    (ygg-markdown-large-open-tests--open buf 200 t
      (should (<= (buffer-size) ygg-markdown-large-size))
      (should (>= syntax-propertize--done (point-max)))
      (should-not ygg-markdown--propertize-timer))))

(ert-deftest ygg-markdown-large-open-list-bounds-match-eager ()
  (let ((auto-mode-alist '(("\\.md\\'" . gfm-mode))))
    (ygg-markdown-large-open-tests--open buf 30000 nil
      (let ((lazy (ygg-markdown-large-open-tests--middle-bounds))
            (eager (let ((ygg-markdown-large-size most-positive-fixnum))
                     (gfm-mode)
                     (ygg-markdown-large-open-tests--middle-bounds))))
        (should (equal lazy eager))
        (should (= (nth 0 lazy) 0))
        (should (> (nth 1 lazy) 0))))))

(defun ygg-markdown-large-open-tests--count (prop)
  (let ((count 0) (pos (point-min)))
    (while (setq pos (text-property-not-all pos (point-max) prop nil))
      (cl-incf count)
      (setq pos (or (next-single-property-change pos prop) (point-max))))
    count))

(defun ygg-markdown-large-open-tests--text (n)
  (with-temp-buffer
    (dotimes (i n) (insert (format "## head %d\n\n- item %d `c` *e*\n\n" i i)))
    (buffer-string)))

(ert-deftest ygg-markdown-large-open-interrupted-chunk-is-redone ()
  (let* ((text (ygg-markdown-large-open-tests--text 14000))
         (eager (with-temp-buffer
                  (insert text)
                  (gfm-mode)
                  (syntax-propertize (point-max))
                  (ygg-markdown-large-open-tests--count 'markdown-heading)))
         (calls 0)
         (interrupt (lambda (fn &rest args)
                      (if (= (cl-incf calls) 2)
                          (throw throw-on-input t)
                        (apply fn args)))))
    (with-temp-buffer
      (insert text)
      (gfm-mode)
      (should (< syntax-propertize--done (point-max)))
      (advice-add 'markdown-syntax-propertize-headings :around interrupt)
      (unwind-protect
          (should (> (ygg-markdown-large-open-tests--drain) 1))
        (advice-remove 'markdown-syntax-propertize-headings interrupt))
      (should (>= calls 2))
      (should (= eager (ygg-markdown-large-open-tests--count 'markdown-heading))))))

(ert-deftest ygg-markdown-large-open-reentering-mode-keeps-one-timer ()
  (let ((before (length timer-idle-list)))
    (with-temp-buffer
      (insert (ygg-markdown-large-open-tests--text 14000))
      (gfm-mode)
      (gfm-mode)
      (markdown-view-mode)
      (should (= 1 (- (length timer-idle-list) before)))
      (should (memq ygg-markdown--propertize-timer timer-idle-list))
      (ygg-markdown--cancel-propertize-timer))))

;;; ygg-markdown-large-open-tests.el ends here

(ert-deftest ygg-markdown-large-open-propertize-advice-only-during-entry ()
  (should-not (advice-member-p #'ygg-markdown--skip-eager-propertize 'syntax-propertize))
  (let ((auto-mode-alist '(("\\.md\\'" . gfm-mode))))
    (ygg-markdown-large-open-tests--open buf 30000 t
      (should-not (advice-member-p #'ygg-markdown--skip-eager-propertize
                                   'syntax-propertize)))))
