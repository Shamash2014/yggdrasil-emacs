;;; ygg-diagram-tab-tests.el --- TAB on diagram fences and tables in markdown -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(load (expand-file-name "test-elpaca" (file-name-directory (or load-file-name buffer-file-name))) nil t)
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

(defun ygg-diagram-tab-tests--svg ()
  (let ((f (make-temp-file "ygg-scale" nil ".svg")))
    (with-temp-file f
      (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"/>"))
    f))

(defmacro ygg-diagram-tab-tests--with-svg (var &rest body)
  (declare (indent 1))
  `(let ((,var (ygg-diagram-tab-tests--svg)))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(defun ygg-diagram-tab-tests--plist (ov)
  (cdr (get-text-property 1 'display (overlay-get ov 'after-string))))

(ert-deftest ygg-diagram-scale-enlarge-shrink-reset ()
  (ygg-diagram-tab-tests--with-svg f
    (with-temp-buffer
      (insert "x\n")
      (let ((ygg-diagram-scale 1.0) (ygg-diagram--scale nil) (inhibit-message t))
        (ygg-diagram--place-image 2 f 840)
        (let ((ov (car (ygg-diagram--overlays))))
          (should (= 1.0 (plist-get (ygg-diagram-tab-tests--plist ov) :scale)))
          (ygg-diagram-enlarge)
          (ygg-diagram-enlarge)
          (let ((plist (ygg-diagram-tab-tests--plist ov)))
            (should (= 1.5625 (plist-get plist :scale)))
            (should (= (round (* 800 1.5625)) (plist-get plist :max-width))))
          (ygg-diagram-shrink)
          (ygg-diagram-shrink)
          (should (= 1.0 (plist-get (ygg-diagram-tab-tests--plist ov) :scale)))
          (ygg-diagram-enlarge)
          (ygg-diagram-scale-reset)
          (should (= 1.0 (plist-get (ygg-diagram-tab-tests--plist ov) :scale)))
          (dotimes (_ 20) (ygg-diagram-shrink))
          (should (= 0.25 (plist-get (ygg-diagram-tab-tests--plist ov) :scale))))))))

(ert-deftest ygg-diagram-scale-applies-to-new-images ()
  (ygg-diagram-tab-tests--with-svg f
    (with-temp-buffer
      (insert "x\n")
      (let ((ygg-diagram-scale 1.0) (ygg-diagram--scale nil) (inhibit-message t))
        (ygg-diagram-enlarge)
        (ygg-diagram--place-image 2 f 840)
        (should (= 1.25 (plist-get (ygg-diagram-tab-tests--plist
                                    (car (ygg-diagram--overlays)))
                                   :scale)))))))

(ert-deftest ygg-diagram-scale-repeat-map-set ()
  (dolist (cmd '(ygg-diagram-enlarge ygg-diagram-shrink ygg-diagram-scale-reset))
    (should (eq 'ygg-diagram-scale-repeat-map (get cmd 'repeat-map))))
  (should (eq #'ygg-diagram-enlarge (lookup-key ygg-diagram-scale-repeat-map "+"))))

(ert-deftest ygg-diagram-scale-localleader-keys ()
  (require 'layer-markdown)
  (dolist (mode '(markdown-mode gfm-mode))
    (let ((map (ygg-localleader--get-map mode)))
      (dolist (kv '(("+" . ygg-diagram-enlarge) ("-" . ygg-diagram-shrink)
                    ("0" . ygg-diagram-scale-reset)))
        (let ((b (lookup-key map (kbd (car kv)))))
          (should (eq (cdr kv) (if (consp b) (cdr b) b))))))))

(ert-deftest ygg-diagram-scale-capped-at-max ()
  (ygg-diagram-tab-tests--with-svg f
    (with-temp-buffer
      (insert "x\n")
      (let ((ygg-diagram-scale 1.0) (ygg-diagram-scale-max 4.0)
            (ygg-diagram--scale nil) (inhibit-message t))
        (ygg-diagram--place-image 2 f 840)
        (dotimes (_ 20) (ygg-diagram-enlarge))
        (let ((plist (ygg-diagram-tab-tests--plist (car (ygg-diagram--overlays)))))
          (should (= 4.0 (plist-get plist :scale)))
          (should (= (round (* 800 4.0)) (plist-get plist :max-width))))))))

(ert-deftest ygg-diagram-scale-defcustom-below-floor-clamped-everywhere ()
  (ygg-diagram-tab-tests--with-svg f
    (with-temp-buffer
      (insert "x\n")
      (let ((ygg-diagram-scale 0.2) (ygg-diagram--scale nil) (inhibit-message t))
        (ygg-diagram--place-image 2 f 840)
        (should (= 0.25 (plist-get (ygg-diagram-tab-tests--plist
                                    (car (ygg-diagram--overlays)))
                                   :scale)))
        (ygg-diagram-enlarge)
        (ygg-diagram-scale-reset)
        (should (= 0.25 (plist-get (ygg-diagram-tab-tests--plist
                                    (car (ygg-diagram--overlays)))
                                   :scale)))))))

;;; ygg-diagram-tab-tests.el ends here

(defun ygg-diagram-tab-tests--center-spec (ov)
  (let ((str (overlay-get ov 'after-string)) found)
    (dotimes (i (length str))
      (let ((disp (get-text-property i 'display str)))
        (when (ygg-diagram--center-p disp) (setq found disp))))
    found))

(ert-deftest ygg-diagram-math-display-block-is-centered ()
  (ygg-diagram-tab-tests--with-svg f
    (cl-letf (((symbol-function 'ygg-diagram--render)
               (lambda (_l _s done) (funcall done f nil))))
      (ygg-diagram-tab-tests--in "$$x^2$$\n\ntext $$y$$ more\n"
        (let ((inhibit-message t)) (ygg-diagram--show))
        (let* ((ovs (sort (ygg-diagram--overlays)
                          (lambda (a b) (< (overlay-start a) (overlay-start b)))))
               (spec (ygg-diagram-tab-tests--center-spec (car ovs))))
          (should (= 2 (length ovs)))
          (should (equal (butlast (plist-get (cdr spec) :align-to) 1)
                         '(- center)))
          (should (eq (car (cdr (nth 2 (plist-get (cdr spec) :align-to)))) 'image))
          (should-not (ygg-diagram-tab-tests--center-spec (cadr ovs))))))))

(ert-deftest ygg-diagram-math-centering-follows-rescale ()
  (ygg-diagram-tab-tests--with-svg f
    (cl-letf (((symbol-function 'ygg-diagram--render)
               (lambda (_l _s done) (funcall done f nil))))
      (ygg-diagram-tab-tests--in "$$x^2$$\n"
        (let ((ygg-diagram--scale nil) (inhibit-message t))
          (ygg-diagram--show)
          (ygg-diagram-enlarge)
          (let* ((spec (ygg-diagram-tab-tests--center-spec (car (ygg-diagram--overlays))))
                 (image (cdr (nth 2 (plist-get (cdr spec) :align-to)))))
            (should (= 1.25 (plist-get (cdr image) :scale)))))))))

(ert-deftest ygg-diagram-error-overlay-help-echo-carries-the-full-log ()
  (let ((log "Error: bad\n    at async renderMermaid (f:1:2)\n"))
    (cl-letf (((symbol-function 'ygg-diagram--render)
               (lambda (_l _s done)
                 (funcall done nil (ygg-diagram--log-summary log) log))))
      (ygg-diagram-tab-tests--in "```mermaid\ngraph TD\n```\n"
        (forward-line 1)
        (ygg-diagram-toggle-at-point)
        (let* ((text (overlay-get (car (ygg-diagram--overlays)) 'after-string))
               (pos (string-search "⚠" text)))
          (should (equal (get-text-property pos 'help-echo text) (string-trim log))))))))

(defun ygg-diagram-tab-tests--fake-renderer (script)
  `(arg stdout "sh" "-c" ,script "x"))

(defun ygg-diagram-tab-tests--live-renderers ()
  (seq-count (lambda (p) (and (process-live-p p)
                              (string-prefix-p "ygg-diagram" (process-name p))))
             (process-list)))

(defun ygg-diagram-tab-tests--render-all (srcs)
  "Render SRCS through a fake renderer; return (PEAK-LIVE . RESULTS) by source."
  (let* ((ygg-diagram--cache-dir (file-name-as-directory
                                  (make-temp-file "ygg-q" t)))
         (ygg-diagram--rendering (make-hash-table :test #'equal))
         (ygg-diagram--queue nil)
         (ygg-diagram--busy nil)
         (script "sleep 0.15; case \"$1\" in fail*) echo boom >&2; exit 3;; esac; echo '<svg/>'")
         (results nil) (peak 0) (deadline (+ (float-time) 15)))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-diagram--spec)
                   (lambda (_l) (ygg-diagram-tab-tests--fake-renderer script)))
                  ((symbol-function 'ygg-diagram--login-argv) #'identity))
          (dolist (src srcs)
            (let ((src src))
              (ygg-diagram--render "mermaid" src
                                   (lambda (svg err &optional log)
                                     (push (list src svg err log) results)))))
          (setq peak (max peak (ygg-diagram-tab-tests--live-renderers)))
          (while (and (< (length results) (length srcs)) (< (float-time) deadline))
            (accept-process-output nil 0.02)
            (setq peak (max peak (ygg-diagram-tab-tests--live-renderers))))
          (cons peak results))
      (delete-directory ygg-diagram--cache-dir t))))

(ert-deftest ygg-diagram-queue-runs-one-renderer-at-a-time ()
  (let ((out (ygg-diagram-tab-tests--render-all '("a" "b" "c"))))
    (should (= 1 (car out)))
    (should (= 3 (length (cdr out))))
    (should (seq-every-p #'cadr (cdr out)))))

(ert-deftest ygg-diagram-queue-failure-starts-the-next-job ()
  (let* ((out (ygg-diagram-tab-tests--render-all '("fail" "b" "c")))
         (failed (assoc "fail" (cdr out))))
    (should (= 1 (car out)))
    (should (= 3 (length (cdr out))))
    (should-not (cadr failed))
    (should (equal (nth 2 failed) "boom"))
    (should (string-match-p "boom" (nth 3 failed)))
    (should (cadr (assoc "b" (cdr out))))
    (should (cadr (assoc "c" (cdr out))))))

(ert-deftest ygg-diagram-queue-coalesces-identical-renders ()
  (let ((out (ygg-diagram-tab-tests--render-all '("a" "a" "b"))))
    (should (= 1 (car out)))
    (should (= 3 (length (cdr out))))))

(ert-deftest ygg-diagram-queue-survives-cleared-overlays ()
  (let ((ygg-diagram--cache-dir (file-name-as-directory (make-temp-file "ygg-q" t)))
        (ygg-diagram--rendering (make-hash-table :test #'equal))
        (ygg-diagram--queue nil)
        (ygg-diagram--busy nil)
        (script "sleep 0.1; echo '<svg/>'"))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-diagram--spec)
                   (lambda (_l) (ygg-diagram-tab-tests--fake-renderer script)))
                  ((symbol-function 'ygg-diagram--login-argv) #'identity))
          (ygg-diagram-tab-tests--in "```mermaid\na\n```\n\n```mermaid\nb\n```\n"
            (let ((inhibit-message t)) (ygg-diagram--show))
            (ygg-diagram--clear)
            (let ((deadline (+ (float-time) 15)))
              (while (and (or ygg-diagram--busy ygg-diagram--queue) (< (float-time) deadline))
                (accept-process-output nil 0.02)))
            (should-not ygg-diagram--busy)
            (should-not ygg-diagram--queue)
            (should-not (ygg-diagram--overlays))))
      (delete-directory ygg-diagram--cache-dir t))))

(defmacro ygg-diagram-tab-tests--with-fake-renderer (script &rest body)
  (declare (indent 1))
  `(let ((ygg-diagram--cache-dir (file-name-as-directory (make-temp-file "ygg-q" t)))
         (ygg-diagram--rendering (make-hash-table :test #'equal))
         (ygg-diagram--queue nil)
         (ygg-diagram--busy nil))
     (unwind-protect
         (cl-letf (((symbol-function 'ygg-diagram--spec)
                    (lambda (_l) (ygg-diagram-tab-tests--fake-renderer ,script)))
                   ((symbol-function 'ygg-diagram--login-argv) #'identity))
           ,@body)
       (delete-directory ygg-diagram--cache-dir t))))

(defun ygg-diagram-tab-tests--wait (pred)
  (let ((deadline (+ (float-time) 15)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02))))

(ert-deftest ygg-diagram-queue-timeout-kills-the-renderer-and-starts-the-next ()
  (ygg-diagram-tab-tests--with-fake-renderer
      "case \"$1\" in hang*) sleep 30;; esac; echo '<svg/>'"
    (let ((ygg-diagram-render-timeout 0.3) hung next)
      (ygg-diagram--render "mermaid" "hang" (lambda (&rest a) (setq hung a)))
      (ygg-diagram--render "mermaid" "ok" (lambda (&rest a) (setq next a)))
      (ygg-diagram-tab-tests--wait (lambda () (and hung next)))
      (should-not (car hung))
      (should (string-match-p "timed out" (cadr hung)))
      (should (car next))
      (ygg-diagram-tab-tests--wait (lambda () (not ygg-diagram--busy)))
      (should-not ygg-diagram--busy))))

(ert-deftest ygg-diagram-queue-settle-error-still-frees-the-renderer ()
  (ygg-diagram-tab-tests--with-fake-renderer "echo '<svg/>'"
    (let* ((svg (expand-file-name "stuck.svg" ygg-diagram--cache-dir))
           (proc (make-process :name "ygg-diagram-test" :noquery t
                               :buffer (generate-new-buffer " *ygg-diagram*")
                               :command '("sh" "-c" "echo boom >&2; exit 3")))
           next)
      (ygg-diagram-tab-tests--wait (lambda () (not (process-live-p proc))))
      (puthash svg (list #'ignore) ygg-diagram--rendering)
      (setq ygg-diagram--busy t)
      (setq ygg-diagram--queue (list (list (expand-file-name "next.svg" ygg-diagram--cache-dir)
                                           "mermaid" "n" (ygg-diagram--spec "mermaid"))))
      (puthash (expand-file-name "next.svg" ygg-diagram--cache-dir)
               (list (lambda (&rest a) (setq next a))) ygg-diagram--rendering)
      (cl-letf (((symbol-function 'ygg-diagram--log-summary)
                 (lambda (_log) (error "summary broke"))))
        (should-error (ygg-diagram--finish proc svg 'stdout nil nil)))
      (ygg-diagram-tab-tests--wait (lambda () next))
      (should (car next))
      (should-not (gethash svg ygg-diagram--rendering))
      (ygg-diagram-tab-tests--wait (lambda () (not ygg-diagram--busy)))
      (should-not ygg-diagram--busy))))

(ert-deftest ygg-diagram-error-help-echo-is-plain-and-capped ()
  (let ((log (concat "\e[31m" (make-string 1000000 ?x) "\e[0m\nreal \e[1merror\e[0m")))
    (cl-letf (((symbol-function 'ygg-diagram--render)
               (lambda (_l _s done) (funcall done nil "\e[31mbad\e[0m" log))))
      (ygg-diagram-tab-tests--in "```mermaid\ngraph TD\n```\n"
        (forward-line 1)
        (ygg-diagram-toggle-at-point)
        (let* ((text (overlay-get (car (ygg-diagram--overlays)) 'after-string))
               (echo (get-text-property (string-search "⚠" text) 'help-echo text)))
          (should (<= (length echo) (1+ ygg-diagram--log-cap)))
          (should (string-suffix-p "real error" echo))
          (should-not (string-search "\e" echo))
          (should-not (string-search "\e" text)))))))

(ert-deftest ygg-diagram-make-process-failure-leaks-no-buffer-or-input-file ()
  (let ((tmp-before (directory-files temporary-file-directory nil "\\`ygg-dia"))
        done)
    (ygg-diagram-tab-tests--with-fake-renderer "true"
      (cl-letf (((symbol-function 'ygg-diagram--spec)
                 (lambda (_l) '(file file "sh" "-c" "true" "x" in out)))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (error "no process"))))
        (ygg-diagram--render "mermaid" "a" (lambda (&rest a) (setq done a)))
        (should done)
        (should-not (car done))
        (should-not ygg-diagram--busy)))
    (should-not (seq-some (lambda (b) (string-prefix-p " *ygg-diagram*" (buffer-name b)))
                          (buffer-list)))
    (should (equal tmp-before (directory-files temporary-file-directory nil "\\`ygg-dia")))))
