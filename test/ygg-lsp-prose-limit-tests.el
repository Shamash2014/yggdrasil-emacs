;;; ygg-lsp-prose-limit-tests.el --- markdown size gate for language servers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'markdown-mode)
(require 'layer-lsp)
(require 'layer-markdown)

(defun ygg-lsp-prose-limit-test--joins-p (file-name mode size)
  "Whether a MODE buffer visiting FILE-NAME of SIZE characters may join."
  (let* ((dir (make-temp-file "ygg-prose-" t))
         (file (expand-file-name file-name dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert (make-string size ?a)))
          (with-current-buffer (find-file-noselect file t)
            (unwind-protect
                (progn
                  (funcall mode)
                  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
                    (ygg-lsp-may-join-server-p)))
              (kill-buffer))))
      (delete-directory dir t))))

(ert-deftest ygg-lsp-prose-limit-over-markdown-joins-nothing ()
  (should-not (ygg-lsp-prose-limit-test--joins-p
               "big.md" 'gfm-mode (1+ ygg-markdown-large-size))))

(ert-deftest ygg-lsp-prose-limit-under-markdown-joins ()
  (should (ygg-lsp-prose-limit-test--joins-p "small.md" 'gfm-mode 70000)))

(ert-deftest ygg-lsp-prose-limit-over-non-markdown-unaffected ()
  (should (ygg-lsp-prose-limit-test--joins-p
           "big.txt" 'text-mode (1+ ygg-markdown-large-size))))

(provide 'ygg-lsp-prose-limit-tests)
