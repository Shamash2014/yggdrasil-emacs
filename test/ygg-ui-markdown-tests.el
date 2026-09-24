;;; ygg-ui-markdown-tests.el --- Tests for the markdown renderer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-ui)

(defconst ygg-ui-markdown-tests--builds
  (expand-file-name "../elpaca/builds/markdown-mode"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun ygg-ui-markdown-tests--load ()
  "Non-nil once markdown-mode is loaded, from the config's own build."
  (or (featurep 'markdown-mode)
      (let ((load-path (cons ygg-ui-markdown-tests--builds load-path)))
        (require 'markdown-mode nil t))))

(defun ygg-ui-markdown-tests--heading-p (text)
  "Non-nil when TEXT rendered has its hashes hidden and a heading face after them."
  (let* ((out (ygg-ui-markdown text))
         (word (string-search "Heading" out))
         (face (get-text-property word 'font-lock-face out)))
    (and (equal (get-text-property 0 'display out) "")
         (memq 'markdown-header-face-2 (ensure-list face)))))

(ert-deftest ygg-ui-markdown-heading-renders-every-time ()
  "A heading renders on every call, not only while the scratch buffer is fresh."
  (skip-unless (ygg-ui-markdown-tests--load))
  (let ((ygg-ui-markdown-hide-markup t))
    (should (ygg-ui-markdown-tests--heading-p "## Heading\n\nbody\n"))
    (should (ygg-ui-markdown-tests--heading-p "## Heading\n\nbody\n"))
    (should (ygg-ui-markdown-tests--heading-p "## Heading\n"))))

(provide 'ygg-ui-markdown-tests)
;;; ygg-ui-markdown-tests.el ends here
