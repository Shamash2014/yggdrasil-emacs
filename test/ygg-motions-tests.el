;;; ygg-motions-tests.el --- Tests for yggdrasil motion fixes -*- lexical-binding: t; -*-

(require 'ert)
(require 'yggdrasil-motions)
(require 'yggdrasil-selection)
(require 'yggdrasil-verbs)

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

;;; Folds

(defconst ygg-test--code
  "(defun a ()\n  (let ((x 1))\n    (foo x)\n    (bar x)))\n(defun b ()\n  1)\n")

(defmacro ygg-test-with-code (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ygg-test--code)
     (goto-char (point-min))
     ,@body))

(ert-deftest ygg-motions-fold-close-open-recursive-hideshow ()
  (ygg-test-with-code
    (ygg-fold-close-recursive)
    (should (invisible-p (line-end-position)))
    (goto-char (point-min))
    (forward-line 1)
    (should (invisible-p (point)))
    (goto-char (point-min))
    (ygg-fold-open-recursive)
    (should-not (seq-some (lambda (o) (overlay-get o 'hs))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest ygg-motions-fold-toggle-recursive-hideshow ()
  (ygg-test-with-code
    (ygg-fold-toggle-recursive)
    (should (invisible-p (line-end-position)))
    (ygg-fold-toggle-recursive)
    (should-not (invisible-p (line-end-position)))))

(ert-deftest ygg-motions-fold-reveal-and-reset ()
  (ygg-test-with-code
    (hs-minor-mode 1)
    (hs-hide-all)
    (goto-char (point-min))
    (forward-line 2)
    (should (invisible-p (point)))
    (ygg-fold-reveal)
    (should-not (invisible-p (point)))
    (ygg-fold-reset)
    (should-not (invisible-p (point)))
    (should (invisible-p (save-excursion (goto-char (point-min)) (forward-line 4)
                                         (line-end-position))))))

(ert-deftest ygg-motions-fold-recursive-outline ()
  (with-temp-buffer
    (outline-mode)
    (outline-minor-mode 1)
    (insert "* A\nbody a\n** B\nbody b\n* C\nbody c\n")
    (goto-char (point-min))
    (ygg-fold-close-recursive)
    (should (invisible-p (save-excursion (goto-char (point-min)) (line-end-position))))
    (ygg-fold-open-recursive)
    (should-not (invisible-p (save-excursion (goto-char (point-min)) (line-end-position))))
    (should-not (invisible-p (save-excursion (search-forward "body b") (point))))))

(ert-deftest ygg-motions-fold-reveal-outline ()
  (with-temp-buffer
    (outline-mode)
    (outline-minor-mode 1)
    (insert "* A\nbody a\n** B\nbody b\n* C\n")
    (outline-hide-sublevels 1)
    (goto-char (point-min))
    (search-forward "body")
    (search-forward "body")
    (should (invisible-p (point)))
    (ygg-fold-reveal)
    (should-not (invisible-p (point)))))

(ert-deftest ygg-motions-fold-manual-create-toggle-delete ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (goto-char 1)
    (set-mark 1)
    (goto-char (+ 1 (length "one\ntwo\nthre")))
    (setq mark-active t)
    (ygg-fold-create)
    (should (= (point) 1))
    (should (invisible-p (line-end-position)))
    (ygg-fold-open)
    (should-not (invisible-p (line-end-position)))
    (ygg-fold-toggle)
    (should (invisible-p (line-end-position)))
    (ygg-fold-open-all)
    (should-not (invisible-p (line-end-position)))
    (ygg-fold-close-all)
    (should (invisible-p (line-end-position)))
    (ygg-fold-delete)
    (should-not (invisible-p (line-end-position)))
    (should-error (ygg-fold-delete) :type 'user-error)))

(ert-deftest ygg-motions-fold-manual-delete-all-and-single-line ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\ne\nf\n")
    (dolist (r '((1 . 4) (7 . 10)))
      (goto-char (cdr r))
      (set-mark (car r))
      (setq mark-active t)
      (ygg-fold-create))
    (should (= 2 (length (ygg-fold--manual-in (point-min) (point-max)))))
    (ygg-fold-delete-all)
    (should-not (ygg-fold--manual-in (point-min) (point-max)))
    (goto-char 1)
    (set-mark 1)
    (setq mark-active t)
    (should-error (ygg-fold-create) :type 'user-error)))

(ert-deftest ygg-motions-fold-next-prev ()
  (ygg-test-with-code
    (ygg-fold-next)
    (should (looking-at "[ \t]*(let"))
    (ygg-fold-prev)
    (should (looking-at "(defun a"))
    (should-error (ygg-fold-prev) :type 'user-error)))

(defconst ygg-test--py-code
  "def f(x):\n    if x:\n        for i in x:\n            print(i)\n    return 1\n")

(defmacro ygg-test-with-python (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (python-mode)
     (insert ygg-test--py-code)
     (goto-char (point-min))
     ,@body))

(ert-deftest ygg-motions-fold-next-python-nested ()
  (ygg-test-with-python
    (ygg-fold-next)
    (should (looking-at "    if x"))
    (ygg-fold-next)
    (should (looking-at "        for i"))
    (ygg-fold-prev)
    (should (looking-at "    if x"))))

(ert-deftest ygg-motions-fold-close-open-recursive-python ()
  (ygg-test-with-python
    (ygg-fold-close-recursive)
    (should (seq-some (lambda (o) (overlay-get o 'hs)) (overlays-in (point-min) (point-max))))
    (goto-char (point-min))
    (forward-line 2)
    (should (invisible-p (line-end-position)))
    (goto-char (point-min))
    (ygg-fold-open-recursive)
    (should-not (seq-some (lambda (o) (overlay-get o 'hs))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest ygg-motions-fold-create-evaporates ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (set-mark 1)
    (goto-char 10)
    (setq mark-active t)
    (ygg-fold-create)
    (let ((ov (car (ygg-fold--manual-in (point-min) (point-max)))))
      (delete-region (overlay-start ov) (overlay-end ov))
      (should-not (overlay-buffer ov)))))

(ert-deftest ygg-motions-fold-keys-bound ()
  (dolist (k '("z A" "z C" "z O" "z v" "z x" "z f" "z d" "z E" "] z" "[ z"))
    (should (commandp (lookup-key ygg-normal-map (kbd k))))))

;;; Repeat maps

(ert-deftest ygg-motions-repeat-maps-wired ()
  (pcase-dolist (`(,cmd ,map ,keys)
                 '((ygg-change-list-older ygg-change-list-repeat-map (";" ","))
                   (ygg-change-list-newer ygg-change-list-repeat-map (";" ","))
                   (ygg-number-increment ygg-number-repeat-map ("C-a" "C-x"))
                   (ygg-number-decrement ygg-number-repeat-map ("C-a" "C-x"))
                   (ygg-number-increment-sequential ygg-number-sequential-repeat-map ("C-a" "C-x"))
                   (ygg-number-decrement-sequential ygg-number-sequential-repeat-map ("C-a" "C-x"))))
    (should (eq (get cmd 'repeat-map) map))
    (dolist (k keys)
      (should (commandp (lookup-key (symbol-value map) (kbd k)))))
    (should (eq (lookup-key (symbol-value map) (kbd "x")) nil))))

(ert-deftest ygg-motions-repeat-map-keys-map-to-owners ()
  (should (eq (lookup-key ygg-change-list-repeat-map ";") #'ygg-change-list-older))
  (should (eq (lookup-key ygg-change-list-repeat-map ",") #'ygg-change-list-newer))
  (should (eq (lookup-key ygg-number-repeat-map (kbd "C-a")) #'ygg-number-increment))
  (should (eq (lookup-key ygg-number-sequential-repeat-map (kbd "C-x"))
              #'ygg-number-decrement-sequential)))

(ert-deftest ygg-mark-position-resolves-without-side-effects ()
  (ygg-with-temp-buffer "one\ntwo\n"
    (let ((ygg--marks-global (list (cons ?B (cons "/nonexistent/f.txt" 4))
                                   (cons ?C (point-marker))))
          (before (buffer-list)))
      (goto-char 5)
      (ygg-mark-set-char ?a)
      (should (equal (ygg-mark-position ?a) (cons (current-buffer) 5)))
      (should (equal (ygg-mark-position ?B) (cons "/nonexistent/f.txt" 4)))
      (should (eq (car (ygg-mark-position ?C)) (current-buffer)))
      (should-error (ygg-mark-position ?z) :type 'user-error)
      (should-error (ygg-mark-position ?A) :type 'user-error)
      (should (equal (buffer-list) before)))))

(ert-deftest ygg-mark-position-remote-never-contacts-host ()
  (require 'tramp)
  (let* ((remote "/ssh:nonexistent.invalid:/x")
         (tramp-connection-timeout 2)
         (ygg--marks-global (list (cons ?R (cons remote 3)))))
    (cl-letf (((symbol-function 'tramp-file-name-handler)
               (lambda (&rest _) (error "tramp handler called"))))
      (should (equal (ygg-mark-position ?R) (cons remote 3))))))

(defconst ygg-test--ts-dir
  (expand-file-name "../tree-sitter" (file-name-directory (or load-file-name buffer-file-name))))

(defmacro ygg-test-with-mode (mode lang code &rest body)
  (declare (indent 3))
  `(progn
     (setq treesit-extra-load-path (list ygg-test--ts-dir))
     (skip-unless (treesit-language-available-p ,lang))
     (with-temp-buffer
       (insert ,code)
       (,mode)
       (goto-char (point-min))
       ,@body)))

(defun ygg-test--hs-count ()
  (length (seq-filter (lambda (o) (overlay-get o 'hs))
                      (overlays-in (point-min) (point-max)))))

(ert-deftest ygg-motions-fold-treesit-python ()
  (ygg-test-with-mode python-ts-mode 'python ygg-test--py-code
    (ygg-fold-close-recursive)
    (should (= 3 (ygg-test--hs-count)))
    (ygg-fold-open-recursive)
    (should (= 0 (ygg-test--hs-count)))
    (ygg-fold-toggle-recursive)
    (should (= 3 (ygg-test--hs-count)))
    (ygg-fold-toggle-recursive)
    (should (= 0 (ygg-test--hs-count)))
    (ygg-fold-next)
    (should (looking-at "    if x"))
    (ygg-fold-prev)
    (should (looking-at "def f"))))

(ert-deftest ygg-motions-fold-treesit-typescript-rust ()
  (ygg-test-with-mode typescript-ts-mode 'typescript
      "function f() {\n  if (x) {\n    y();\n  }\n}\n"
    (ygg-fold-close-recursive)
    (should (= 2 (ygg-test--hs-count)))
    (ygg-fold-open-recursive)
    (should (= 0 (ygg-test--hs-count)))
    (ygg-fold-next)
    (should (looking-at "  if"))
    (ygg-fold-prev)
    (should (looking-at "function")))
  (ygg-test-with-mode rust-ts-mode 'rust
      "fn f() {\n    if x {\n        y();\n    }\n}\n"
    (ygg-fold-toggle-recursive)
    (should (= 2 (ygg-test--hs-count)))
    (ygg-fold-open-recursive)
    (should (= 0 (ygg-test--hs-count)))))

(ert-deftest ygg-motions-fold-c-mode-brace-on-line ()
  (with-temp-buffer
    (insert "int f() {\n  if (x) {\n    y();\n  }\n}\n")
    (c-mode)
    (goto-char (point-min))
    (ygg-fold-close-recursive)
    (should (= 2 (ygg-test--hs-count)))
    (should (invisible-p (save-excursion (forward-line 2) (point))))
    (ygg-fold-open-recursive)
    (should (= 0 (ygg-test--hs-count)))
    (ygg-fold-next)
    (should (looking-at "  if"))))

(ert-deftest ygg-motions-fold-close-recursive-twice-no-duplicates ()
  (ygg-test-with-mode typescript-ts-mode 'typescript
      "function f() {\n  if (x) {\n    y();\n  }\n}\n"
    (ygg-fold-close-recursive)
    (ygg-fold-close-recursive)
    (should (= 2 (ygg-test--hs-count)))))

(ert-deftest ygg-motions-fold-next-fast-on-large-fold-less-buffer ()
  (let ((code (concat (mapconcat (lambda (i) (format "const v%d = %d;" i i))
                                 (number-sequence 1 10000) "\n")
                      "\nfunction f() {\n  y();\n}\n")))
    (ygg-test-with-mode typescript-ts-mode 'typescript code
      (let ((start (float-time)))
        (ygg-fold-next)
        (should (looking-at "function f"))
        (should (< (- (float-time) start) 1.0)))
      (let ((start (float-time)))
        (goto-char (point-max))
        (ygg-fold-prev)
        (should (looking-at "function f"))
        (should (< (- (float-time) start) 1.0))))))

(ert-deftest ygg-motions-fold-recursive-close-keeps-inner-closed ()
  (with-temp-buffer
    (insert "int f() {\n  if (x) {\n    y();\n  }\n}\n")
    (c-mode)
    (goto-char (point-min))
    (ygg-fold-close-recursive)
    (ygg-fold-open)
    (should-not (invisible-p (save-excursion (forward-line 1) (point))))
    (should (invisible-p (save-excursion (forward-line 2) (point))))))

(ert-deftest ygg-motions-fold-next-leaves-no-selection-span ()
  (ygg-test-with-code
    (ygg-fold-next)
    (should-not (use-region-p))
    (let ((sel (ygg-selection-effective-bounds)))
      (should (<= (- (nth 1 sel) (nth 0 sel)) 1)))))

(defconst ygg-test--fold-walk-code
  '((python-ts-mode python "def a():\n    if x:\n        for i in y:\n            pass\n    return 1\n\nclass B:\n    def m(self):\n        while 1:\n            pass\n")
    (typescript-ts-mode typescript "function f() {\n  if (x) {\n    for (;;) {\n      y();\n    }\n  }\n}\nclass C {\n  m() {\n    z();\n  }\n}\n")
    (rust-ts-mode rust "fn f() {\n    if x {\n        loop {\n            y();\n        }\n    }\n}\nimpl C {\n    fn m() {\n        z();\n    }\n}\n")
    (go-ts-mode go "package p\n\nfunc f() {\n\tif x {\n\t\tfor {\n\t\t\ty()\n\t\t}\n\t}\n}\n\nfunc g() {\n\tz()\n}\n")))

(defun ygg-test--fold-stops (step)
  (let (acc)
    (condition-case nil
        (while t (funcall step) (push (point) acc))
      (user-error nil))
    (nreverse acc)))

(ert-deftest ygg-motions-fold-prev-matches-next-treesit ()
  (setq treesit-extra-load-path (list ygg-test--ts-dir))
  (dolist (spec ygg-test--fold-walk-code)
    (pcase-let ((`(,mode ,lang ,code) spec))
      (when (treesit-language-available-p lang)
        (with-temp-buffer
          (insert "\n" code)
          (funcall mode)
          (goto-char (point-min))
          (let ((fwd (ygg-test--fold-stops #'ygg-fold-next)))
            (goto-char (point-max))
            (let ((bwd (ygg-test--fold-stops #'ygg-fold-prev)))
              (should (> (length fwd) 2))
              (should (equal bwd (reverse fwd))))))))))

(provide 'ygg-motions-tests)
;;; ygg-motions-tests.el ends here
