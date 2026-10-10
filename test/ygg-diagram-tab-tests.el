;;; ygg-diagram-tab-tests.el --- TAB on diagram fences and tables in markdown -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'markdown-mode)
(require 'ygg-diagram)

(defconst ygg-diagram-tab-tests--table
  (concat "| Name | Qty | Note |\n"
          "|:--|:-:|--:|\n"
          "| **apple** | 3 | a\\|b |\n"
          "| 日本語 | 10 | `x` |\n"))

(defconst ygg-diagram-tab-tests--box
  (concat "┌────────┬─────┬──────┐\n"
          "│ Name   │ Qty │ Note │\n"
          "├────────┼─────┼──────┤\n"
          "│ apple  │  3  │  a|b │\n"
          "│ 日本語 │ 10  │    x │\n"
          "└────────┴─────┴──────┘"))

(defmacro ygg-diagram-tab-tests--in (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defun ygg-diagram-tab-tests--fence (text)
  (ygg-diagram-tab-tests--in text
    (forward-line 1)
    (ygg-diagram-fence-at-point)))

(ert-deftest ygg-diagram-tab-fence-plain ()
  (should (equal (ygg-diagram-tab-tests--fence "```mermaid\ngraph TD\n```\n")
                 '("mermaid" "graph TD\n" 1 24))))

(ert-deftest ygg-diagram-tab-fence-variants ()
  (dolist (case '(("```mermaid title=\"x\"\ngraph TD\n```\n" "mermaid")
                  ("```{mermaid}\ngraph TD\n```\n" "mermaid")
                  ("~~~mermaid\ngraph TD\n~~~\n" "mermaid")
                  ("```mermaid\r\ngraph TD\r\n```\r\n" "mermaid")
                  ("```Mermaid\ngraph TD\n```\n" "mermaid")
                  ("```DOT\ndigraph {}\n```\n" "dot")))
    (let ((hit (ygg-diagram-tab-tests--fence (car case))))
      (should hit)
      (should (equal (car hit) (cadr case)))
      (should-not (string-match-p "\r" (cadr hit))))))

(ert-deftest ygg-diagram-tab-fence-tilde-needs-tilde-close ()
  (should-not (ygg-diagram-tab-tests--fence "~~~mermaid\ngraph TD\n```\n")))

(ert-deftest ygg-diagram-tab-fence-rejects-other-langs ()
  (should-not (ygg-diagram-tab-tests--fence "```mermaidx\na\n```\n"))
  (should-not (ygg-diagram-tab-tests--fence "```python\na\n```\n")))

(ert-deftest ygg-diagram-tab-show-regexp-variants ()
  (dolist (text '("```mermaid title=\"x\"\na\n```\n" "```{mermaid}\na\n```\n"
                  "~~~mermaid\na\n~~~\n" "```MERMAID\r\na\r\n```\r\n"))
    (let ((case-fold-search t))
      (should (string-match ygg-diagram--fence-re text))
      (should (equal (downcase (match-string 1 text)) "mermaid")))))

(ert-deftest ygg-diagram-tab-table-bounds ()
  (let ((text (concat "intro\n\n" ygg-diagram-tab-tests--table "\nafter | not a table\n")))
    (ygg-diagram-tab-tests--in text
      (let ((beg (+ (point-min) 7))
            (end (+ (point-min) 7 (1- (length ygg-diagram-tab-tests--table)))))
        (dolist (line '(3 4 5 6))
          (goto-char (point-min))
          (forward-line (1- line))
          (should (equal (seq-take (ygg-diagram-table-at-point) 2) (list beg end))))
        (dolist (line '(1 2 7 8))
          (goto-char (point-min))
          (forward-line (1- line))
          (should-not (ygg-diagram-table-at-point)))))))

(ert-deftest ygg-diagram-tab-table-needs-delimiter-row ()
  (ygg-diagram-tab-tests--in "| a | b |\n| c | d |\n"
    (should-not (ygg-diagram-table-at-point))))

(ert-deftest ygg-diagram-tab-table-render-exact ()
  (ygg-diagram-tab-tests--in ygg-diagram-tab-tests--table
    (pcase-let ((`(,_ ,_ ,rows) (ygg-diagram-table-at-point)))
      (let ((box (ygg-diagram--table-render rows)))
        (should (equal (substring-no-properties box) ygg-diagram-tab-tests--box))
        (should (eq (get-text-property (1+ (string-search "Name" box)) 'face box) 'bold))))))

(ert-deftest ygg-diagram-tab-table-toggle-twice ()
  (ygg-diagram-tab-tests--in ygg-diagram-tab-tests--table
    (forward-line 2)
    (ygg-diagram-toggle-table-at-point)
    (should (= (length (ygg-diagram--overlays)) 1))
    (should (equal (overlay-get (car (ygg-diagram--overlays)) 'display)
                   ygg-diagram-tab-tests--box))
    (should (equal (buffer-string) ygg-diagram-tab-tests--table))
    (ygg-diagram-toggle-table-at-point)
    (should-not (ygg-diagram--overlays))))

(ert-deftest ygg-diagram-tab-table-toggle-leaves-fence-overlays-on-edit ()
  (ygg-diagram-tab-tests--in (concat "```mermaid\ngraph TD\n```\n\n" ygg-diagram-tab-tests--table)
    (let ((fence (make-overlay 1 1)))
      (overlay-put fence 'ygg-diagram t)
      (goto-char (point-max))
      (forward-line -2)
      (ygg-diagram-toggle-table-at-point)
      (goto-char 1)
      (insert "x")
      (should (overlay-buffer fence))
      (should (= (length (ygg-diagram--overlays)) 2)))))

(ert-deftest ygg-diagram-tab-toggle-clear-hides-table ()
  (ygg-diagram-tab-tests--in ygg-diagram-tab-tests--table
    (ygg-diagram-toggle-table-at-point)
    (ygg-diagram--clear)
    (should-not (ygg-diagram--overlays))))

(ert-deftest ygg-diagram-tab-any-leaves-table-to-the-fold ()
  (ygg-diagram-tab-tests--in ygg-diagram-tab-tests--table
    (should-not (ygg-diagram-toggle-any-at-point))
    (should-not (ygg-diagram--overlays))))

(ert-deftest ygg-diagram-tab-table-survives-redraw ()
  (ygg-diagram-tab-tests--in ygg-diagram-tab-tests--table
    (ygg-diagram-toggle-table-at-point)
    (should (= (length (ygg-diagram--overlays)) 1))
    (ygg-diagram-replace)
    (let ((ovs (ygg-diagram--overlays)))
      (should (= (length ovs) 1))
      (should (overlay-get (car ovs) 'ygg-diagram-table))
      (should (equal (overlay-get (car ovs) 'display)
                     (ygg-diagram--table-render (nth 2 (ygg-diagram-table-at-point))
                                                (ygg-diagram--table-window-width)))))
    (goto-char (point-min))
    (ygg-diagram-toggle-table-at-point)
    (ygg-diagram-replace)
    (should-not (ygg-diagram--overlays))))

(ert-deftest ygg-diagram-tab-table-survives-redraw-in-trace-text ()
  (ygg-diagram-tab-tests--in (concat "agent reply\n" ygg-diagram-tab-tests--table "tail\n")
    (forward-line 2)
    (ygg-diagram-toggle-table-at-point)
    (ygg-diagram-replace)
    (ygg-diagram-replace)
    (should (= (length (ygg-diagram--overlays)) 1))))

(ert-deftest ygg-diagram-tab-fence-scan-is-bounded ()
  (let ((text (concat "```mermaid\ngraph TD\n"
                      (mapconcat #'identity (make-list 500 "node") "\n")
                      "\n```\n")))
    (ygg-diagram-tab-tests--in text
      (goto-char (point-max))
      (forward-line -3)
      (let ((ygg-diagram-scan-limit 100))
        (should-not (ygg-diagram-fence-at-point)))
      (let ((ygg-diagram-scan-limit 1000))
        (should (ygg-diagram-fence-at-point))))))

(ert-deftest ygg-diagram-tab-fence-scan-stops-after-both-closers ()
  (ygg-diagram-tab-tests--in "```mermaid\na\n```\n~~~\ntext\n~~~\nplain\n"
    (goto-char (point-max))
    (forward-line -1)
    (let ((ygg-diagram-scan-limit 3))
      (should-not (ygg-diagram-fence-at-point)))))

(ert-deftest ygg-diagram-tab-fence-scan-large-buffer-is-fast ()
  (ygg-diagram-tab-tests--in (mapconcat #'identity (make-list 20000 "plain line") "\n")
    (goto-char (point-max))
    (let ((start (float-time)))
      (dotimes (_ 200) (ygg-diagram-fence-at-point))
      (should (< (- (float-time) start) 2.0)))))

(ert-deftest ygg-diagram-tab-table-from-mid-header-line ()
  (ygg-diagram-tab-tests--in ygg-diagram-tab-tests--table
    (goto-char 3)
    (should (ygg-diagram-table-at-point))))

(ert-deftest ygg-diagram-tab-fence-other-marker-inside-is-body ()
  (let ((text "~~~mermaid\ngraph\n```\nB\n~~~\nafter\n"))
    (dolist (line '(2 3 4 5))
      (ygg-diagram-tab-tests--in text
        (forward-line (1- line))
        (should (ygg-diagram-fence-at-point))))
    (ygg-diagram-tab-tests--in text
      (forward-line 5)
      (should-not (ygg-diagram-fence-at-point))))
  (ygg-diagram-tab-tests--in "```mermaid\ngraph\n~~~\nB\n```\n"
    (forward-line 3)
    (should (ygg-diagram-fence-at-point))))

(ert-deftest ygg-diagram-tab-fence-closer-line-counts ()
  (ygg-diagram-tab-tests--in "```mermaid\ngraph\n```\n"
    (forward-line 2)
    (should (ygg-diagram-fence-at-point))))

(ert-deftest ygg-diagram-tab-table-ignores-code-blocks ()
  (dolist (text '("```python\n| h | i |\n|---|---|\n| 1 | 2 |\n```\n"
                  "```\n| h | i |\n|---|---|\n| 1 | 2 |\n```\n"
                  "~~~\n| h | i |\n|---|---|\n| 1 | 2 |\n~~~\n"
                  "    | h | i |\n    |---|---|\n    | 1 | 2 |\n"
                  "\t| h | i |\n\t|---|---|\n\t| 1 | 2 |\n"))
    (dolist (mode '(fundamental-mode gfm-mode))
      (with-temp-buffer
        (funcall mode)
        (insert text)
        (goto-char (point-min))
        (forward-line (if (string-match-p "\\`[`~]" text) 1 0))
        (should-not (ygg-diagram-table-at-point))))))

(ert-deftest ygg-diagram-tab-table-after-code-block-still-found ()
  (dolist (mode '(fundamental-mode gfm-mode))
    (with-temp-buffer
      (funcall mode)
      (insert "```\ncode\n```\n\n" ygg-diagram-tab-tests--table)
      (goto-char (point-max))
      (forward-line -2)
      (should (ygg-diagram-table-at-point)))))

(ert-deftest ygg-diagram-tab-wide-table-fits-window ()
  (let* ((rows (list '("Name" "Note") '("---" "---")
                     (list "a" (make-string 80 ?x))))
         (box (ygg-diagram--table-render rows 40)))
    (should (seq-every-p (lambda (l) (<= (string-width l) 40)) (split-string box "\n")))
    (should (string-match-p "…" box))
    (should-not (string-match-p "…" (ygg-diagram--table-render rows)))))

(ert-deftest ygg-diagram-tab-wide-table-keeps-three-chars-per-column ()
  (let* ((rows (list (make-list 6 "wide header") (make-list 6 "---")))
         (box (ygg-diagram--table-render rows 10)))
    (should (string-match-p "│ wi… │ wi… │" box))))

(ert-deftest ygg-diagram-tab-revert-clears-overlays ()
  (let ((file (make-temp-file "ygg-tab" nil ".md" ygg-diagram-tab-tests--table)))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (progn
                (ygg-diagram-toggle-table-at-point)
                (should (ygg-diagram--overlays))
                (revert-buffer t t)
                (should-not (ygg-diagram--overlays)))
            (kill-buffer)))
      (delete-file file))))

(ert-deftest ygg-diagram-tab-setext-heading-is-not-a-table ()
  (ygg-diagram-tab-tests--in "x |\n---\n"
    (should-not (ygg-diagram-table-at-point))))

(ert-deftest ygg-diagram-tab-falls-back-on-plain-text ()
  (let (called)
    (cl-letf (((symbol-function 'ygg-diagram-tab-tests--fallback)
               (lambda () (interactive) (setq called t))))
      (let ((ygg-diagram-markdown-tab-fallback 'ygg-diagram-tab-tests--fallback))
        (ygg-diagram-tab-tests--in "just words\n"
          (ygg-diagram-markdown-tab))))
    (should called)))

(ert-deftest ygg-diagram-tab-default-fallback-is-jump-forward ()
  (should (eq ygg-diagram-markdown-tab-fallback 'ygg-jump-forward)))

(ert-deftest ygg-diagram-tab-does-not-fall-back-on-table-or-fence ()
  (let (called)
    (cl-letf (((symbol-function 'ygg-diagram-tab-tests--fallback)
               (lambda () (interactive) (setq called t)))
              ((symbol-function 'ygg-diagram--render) #'ignore))
      (let ((ygg-diagram-markdown-tab-fallback 'ygg-diagram-tab-tests--fallback))
        (ygg-diagram-tab-tests--in ygg-diagram-tab-tests--table
          (ygg-diagram-markdown-tab)
          (should (ygg-diagram--overlays)))
        (ygg-diagram-tab-tests--in "```mermaid\ngraph TD\n```\n"
          (ygg-diagram-markdown-tab)
          (should (ygg-diagram--overlays)))))
    (should-not called)))

(ert-deftest ygg-diagram-tab-gfm-mermaid-places-overlay ()
  (let (asked)
    (cl-letf (((symbol-function 'ygg-diagram--render)
               (lambda (lang src _done) (setq asked (cons lang src)))))
      (with-temp-buffer
        (gfm-mode)
        (insert "# t\n\n```mermaid\ngraph TD\n  A-->B\n```\n")
        (goto-char (point-min))
        (forward-line 4)
        (ygg-diagram-markdown-tab)
        (should (= (length (ygg-diagram--overlays)) 1))
        (should (equal asked '("mermaid" . "graph TD\n  A-->B\n")))
        (ygg-diagram-markdown-tab)
        (should-not (ygg-diagram--overlays))))))

(ert-deftest ygg-diagram-tab-gfm-table-toggles ()
  (with-temp-buffer
    (gfm-mode)
    (insert ygg-diagram-tab-tests--table)
    (goto-char (point-min))
    (ygg-diagram-markdown-tab)
    (should (= (length (ygg-diagram--overlays)) 1))
    (ygg-diagram-markdown-tab)
    (should-not (ygg-diagram--overlays))))

(ert-deftest ygg-diagram-tab-plan-mode-keeps-its-own-tab ()
  (require 'yggdrasil)
  (require 'ygg-plan)
  (yggdrasil-define-mode-keys 'markdown-mode 'normal "<tab>" #'ygg-diagram-markdown-tab)
  (yggdrasil-define-mode-keys 'gfm-mode 'normal "<tab>" #'ygg-diagram-markdown-tab)
  (unwind-protect
      (progn
        (with-temp-buffer
          (gfm-mode) (yggdrasil-local-mode 1) (ygg-normal-state)
          (should (eq (key-binding (kbd "<tab>")) #'ygg-diagram-markdown-tab))
          (should (eq (key-binding (kbd "C-i")) #'ygg-jump-forward)))
        (with-temp-buffer
          (cl-letf (((symbol-function 'ygg-plan--load) #'ignore)
                    ((symbol-function 'ygg-plan-fold) #'ignore)
                    ((symbol-function 'ygg-plan--refresh) #'ignore))
            (ygg-plan-mode))
          (yggdrasil-local-mode 1) (ygg-normal-state)
          (should (eq (key-binding (kbd "<tab>")) #'ygg-plan-tab))))
    (remhash '(markdown-mode . normal) ygg--mode-keys)
    (remhash '(gfm-mode . normal) ygg--mode-keys)))

(defconst ygg-diagram-tab-tests--front-matter
  "```mermaid\n---\ntitle: \"Context\"\n---\ngraph TB\n  A -. \"x\" .-> B\n```\n")

(ert-deftest ygg-diagram-tab-fence-keeps-front-matter-in-markdown ()
  (with-temp-buffer
    (gfm-mode)
    (insert ygg-diagram-tab-tests--front-matter)
    (goto-char (point-min))
    (forward-line 4)
    (should (string-prefix-p "---\ntitle:" (nth 1 (ygg-diagram-fence-at-point))))
    (should (string-match-p "^---$" (nth 1 (ygg-diagram-fence-at-point))))))

(ert-deftest ygg-diagram-tab-fence-keeps-dash-lines-in-markdown ()
  (with-temp-buffer
    (gfm-mode)
    (insert "```mermaid\ngraph TD\n-- x --> y\n```\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (nth 1 (ygg-diagram-fence-at-point)) "graph TD\n-- x --> y\n"))))

(ert-deftest ygg-diagram-tab-fence-drops-removed-lines-in-diff-modes ()
  (dolist (mode '(diff-mode magit-diff-mode))
    (when (or (eq mode 'diff-mode) (require 'magit-diff nil t))
      (with-temp-buffer
        (insert " ```mermaid\n graph TD\n-  A-->OLD\n+  A-->NEW\n ```\n")
        (funcall mode)
        (goto-char (point-min))
        (forward-line 1)
        (should (equal (nth 1 (ygg-diagram-fence-at-point)) "graph TD\n  A-->NEW\n"))))))

(ert-deftest ygg-diagram-tab-fence-drops-removed-lines-under-plus-opener ()
  (with-temp-buffer
    (insert "+```mermaid\n+graph TD\n-  A-->OLD\n+  A-->NEW\n+```\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (nth 1 (ygg-diagram-fence-at-point)) "graph TD\n  A-->NEW\n"))))

(ert-deftest ygg-diagram-tab-error-summary-skips-stack-frames ()
  (let ((log (concat "Error: Parse error on line 2:\n"
                     "...title: \"Context\"\n"
                     "Expecting 'SPACE', got 'NEWLINE'\n"
                     "    at Parser.parseError (file:///x/parser.js:1:2)\n"
                     "    at async renderMermaid (file:///x/index.js:480:22)\n")))
    (should (equal (ygg-diagram--log-summary log) "Expecting 'SPACE', got 'NEWLINE'"))
    (should (equal (ygg-diagram--log-summary "boom\nat x (y)\n") "boom"))
    (should (equal (ygg-diagram--log-summary "one\ntwo\n") "two"))
    (should-not (ygg-diagram--log-summary ""))))

(ert-deftest ygg-diagram-tab-error-overlay-shows-the-summary ()
  (cl-letf (((symbol-function 'ygg-diagram--render)
             (lambda (_l _s done) (funcall done nil (ygg-diagram--log-summary
                                                      "Error: bad\n    at async renderMermaid (f:1:2)\n")))))
    (ygg-diagram-tab-tests--in "```mermaid\ngraph TD\n```\n"
      (forward-line 1)
      (ygg-diagram-toggle-at-point)
      (let ((text (overlay-get (car (ygg-diagram--overlays)) 'after-string)))
        (should (string-match-p "⚠ Error: bad" text))
        (should-not (string-match-p "renderMermaid" text))))))

(ert-deftest ygg-diagram-tab-table-edit-drops-its-overlay-and-key ()
  (ygg-diagram-tab-tests--in (concat "```mermaid\ngraph TD\n```\n\n" ygg-diagram-tab-tests--table)
    (let ((fence (make-overlay 1 1)))
      (overlay-put fence 'ygg-diagram t)
      (setq ygg-diagram--shown (list '("mermaid" . "graph TD\n")))
      (goto-char (point-max))
      (forward-line -2)
      (ygg-diagram-toggle-table-at-point)
      (should (seq-find (lambda (k) (eq (car k) 'table)) ygg-diagram--shown))
      (goto-char (point-max))
      (forward-line -2)
      (forward-char 3)
      (insert "z")
      (should-not (seq-find (lambda (o) (overlay-get o 'ygg-diagram-table))
                            (ygg-diagram--overlays)))
      (should-not (seq-find (lambda (k) (eq (car k) 'table)) ygg-diagram--shown))
      (should (overlay-buffer fence))
      (should (= (length ygg-diagram--shown) 1)))))

(ert-deftest ygg-diagram-tab-markdown-tab-draws-images-and-md-fences-via-toggle-any ()
  (let (calls)
    (cl-letf (((symbol-function 'ygg-diagram-md-fence-at-point) (lambda () t))
              ((symbol-function 'ygg-diagram-toggle-md-at-point)
               (lambda () (push 'md calls))))
      (ygg-diagram-tab-tests--in "```md\n# hi\n```\n"
        (ygg-diagram-markdown-tab)))
    (cl-letf (((symbol-function 'ygg-diagram-image-at-point) (lambda () "/x.png"))
              ((symbol-function 'ygg-diagram-toggle-image-at-point)
               (lambda () (push 'image calls))))
      (ygg-diagram-tab-tests--in "![a](x.png)\n"
        (ygg-diagram-markdown-tab)))
    (should (equal (nreverse calls) '(md image)))))

;;; ygg-diagram-tab-tests.el ends here
