;;; aob-trace-markdown-tests.el --- Agent markdown drawn in the trace -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-trace-markdown-sessions-" nil ".eld"))
(require 'aob-trace)

(defun aob-trace-markdown-tests--faces (out needle)
  (let ((i (string-search needle out)))
    (should i)
    (aob-trace--faces-of (get-text-property i 'font-lock-face out))))

(defun aob-trace-markdown-tests--mixed (size)
  (let ((unit (concat "# Heading\n\nSome **bold** text with `inline code` and a [link](https://x.org) "
                      "plus _italic_ words to fill the paragraph out a little.\n\n- one\n- two\n\n"
                      "| a | b | c |\n|---|---|---|\n| 1 | two | `three` |\n| 4 | five | six |\n\n"
                      "```elisp\n(defun f (x)\n  (let ((y 1)) (+ x y)))\n```\n\n"
                      "```diff\n@@ -1,2 +1,2 @@\n-old\n+new\n```\n\n"))
        (text ""))
    (while (< (length text) size) (setq text (concat text unit)))
    (substring text 0 size)))

(ert-deftest aob-trace-markdown-inline-markup-gone-faces-present ()
  (let ((out (aob-trace--md "# Title\n\nsome **bold** and `code` here\n")))
    (should-not (string-match-p "[#*`]" out))
    (should (memq 'agent-shell-markdown-header-1 (aob-trace-markdown-tests--faces out "Title")))
    (should (memq 'agent-shell-markdown-bold (aob-trace-markdown-tests--faces out "bold")))
    (should (memq 'agent-shell-markdown-inline-code (aob-trace-markdown-tests--faces out "code")))
    (should-not (text-property-not-all 0 (length out) 'face nil out))))

(ert-deftest aob-trace-markdown-table-aligned-and-borderless-of-markup ()
  (let* ((out (aob-trace--md "| a | b |\n|---|---|\n| 1 | **two** |\n"))
         (rows (seq-filter (lambda (l) (string-match-p "│" l)) (split-string out "\n"))))
    (should-not (string-search "**" out))
    (should (string-search "├" out))
    (should (= 1 (length (delete-dups (mapcar #'string-width rows)))))
    (should (memq 'agent-shell-markdown-table-header (aob-trace-markdown-tests--faces out "a")))))

(ert-deftest aob-trace-markdown-table-fits-the-trace-width ()
  (let* ((aob-trace--width 40)
         (out (aob-trace--md (concat "| name | note |\n|---|---|\n| x | "
                                     (make-string 120 ?w) " |\n"))))
    (dolist (l (split-string out "\n"))
      (should (<= (string-width l) 40)))))

(ert-deftest aob-trace-markdown-fenced-elisp-framed-and-fontified ()
  (let ((out (aob-trace--md "before\n\n```elisp\n(defun f () 1)\n```\n\nafter\n")))
    (should-not (string-search "```" out))
    (should (string-search "elisp" out))
    (should (text-property-any 0 (length out) 'agent-shell-markdown-source-block-rendered t out))
    (should (seq-some (lambda (f) (memq f '(font-lock-keyword-face font-lock-function-name-face)))
                      (aob-trace-markdown-tests--faces out "defun")))
    (should (memq 'agent-shell-markdown-source-block
                  (aob-trace-markdown-tests--faces out "(defun")))
    (should-not (memq 'agent-shell-markdown-source-block
                      (aob-trace-markdown-tests--faces out "before")))))

(ert-deftest aob-trace-markdown-diff-fence-lines-coloured ()
  (let ((out (aob-trace--md "```diff\n@@ -1 +1 @@\n-gone\n+came\n kept\n```\n")))
    (should-not (string-search "```" out))
    (should (memq 'aob-trace-diff-context (aob-trace-markdown-tests--faces out "@@")))
    (should (memq 'aob-trace-diff-removed (aob-trace-markdown-tests--faces out "-gone")))
    (should (memq 'aob-trace-diff-added (aob-trace-markdown-tests--faces out "+came")))
    (should-not (memq 'aob-trace-diff-added (aob-trace-markdown-tests--faces out " kept")))
    (should (memq 'agent-shell-markdown-source-block
                  (aob-trace-markdown-tests--faces out "+came")))))

(ert-deftest aob-trace-markdown-prose-face-reaches-prose-not-fences ()
  (let ((out (aob-trace--prose (aob-trace--md "**bold** word\n\n```elisp\n(a)\n```\n"))))
    (should (memq 'aob-trace-prose (aob-trace-markdown-tests--faces out "bold")))
    (should (memq 'agent-shell-markdown-bold (aob-trace-markdown-tests--faces out "bold")))
    (should-not (memq 'aob-trace-prose (aob-trace-markdown-tests--faces out "(a)")))))

(ert-deftest aob-trace-markdown-live-cap-leaves-long-text-alone ()
  (let ((aob-trace-live-markdown-max 10))
    (should (equal "**long text here**" (aob-trace--md "**long text here**" t)))
    (should (equal "long text here" (aob-trace--md "**long text here**")))))

(ert-deftest aob-trace-markdown-25k-chars-render-fast ()
  (let* ((text (aob-trace-markdown-tests--mixed 25000))
         (start (float-time))
         (out (aob-trace--md text))
         (took (- (float-time) start)))
    (should (> (length out) 10000))
    (should (< took 2.0))))

(ert-deftest aob-trace-markdown-copy-yields-the-markdown ()
  (with-temp-buffer
    (aob-trace-mode)
    (let ((inhibit-read-only t)
          (md "# Title\n\nsome **bold** and `code`\n\n| a | b |\n|---|---|\n| 1 | 2 |\n\n```elisp\n(a)\n```\n"))
      (insert (aob-trace--md md))
      (let ((copied (filter-buffer-substring (point-min) (point-max))))
        (should (string-search "# Title" copied))
        (should (string-search "**bold**" copied))
        (should (string-search "| a | b |" copied))
        (should (string-search "```elisp" copied))
        (should-not (string-search "⧉" copied))
        (should-not (text-properties-at 0 copied)))
      (goto-char (point-min))
      (search-forward "some bo")
      (should (equal "some bo" (filter-buffer-substring (match-beginning 0) (match-end 0)))))))

(ert-deftest aob-trace-markdown-copy-span-cut-only-by-a-mark-stays-markdown ()
  (with-temp-buffer
    (aob-trace-mode)
    (let ((inhibit-read-only t))
      (insert (aob-trace--md "x **bold word** y\n"))
      (goto-char (point-min))
      (search-forward "bold")
      (insert (propertize "@" 'aob-status 'done))
      (should (equal "x **bold word** y\n"
                     (filter-buffer-substring (point-min) (point-max)))))))

(provide 'aob-trace-markdown-tests)
;;; aob-trace-markdown-tests.el ends here
