;;; lead-lazy-tests.el --- the lead preset keeps its rules and leaves the how-to to a skill -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ygg-preset)

(defconst lead-lazy-tests--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defun lead-lazy-tests--file (path)
  (expand-file-name path lead-lazy-tests--root))

(defun lead-lazy-tests--text (path)
  (with-temp-buffer
    (insert-file-contents (lead-lazy-tests--file path))
    (buffer-string)))

(ert-deftest lead-lazy-preset-is-short ()
  (should (< (length (lead-lazy-tests--text "presets/lead.md")) 3500)))

(ert-deftest lead-lazy-preset-names-the-howto ()
  (should (string-match-p "skill lead-howto" (lead-lazy-tests--text "presets/lead.md"))))

(ert-deftest lead-lazy-preset-keeps-the-no-paste-rule ()
  (should (string-match-p "do not paste one" (lead-lazy-tests--text "presets/lead.md"))))

(ert-deftest lead-lazy-every-howto-file-the-preset-names-exists ()
  (let ((text (lead-lazy-tests--text "presets/lead.md"))
        (case-fold-search nil)
        (start 0)
        named)
    (while (string-match "lead-howto/\\([a-z-]+\\.md\\)" text start)
      (push (match-string 1 text) named)
      (setq start (match-end 0)))
    (should (>= (length named) 5))
    (dolist (file named)
      (should (file-exists-p (lead-lazy-tests--file (concat "skills/lead-howto/" file)))))))

(ert-deftest lead-lazy-howto-index-is-short-and-links-only-real-files ()
  (let ((text (lead-lazy-tests--text "skills/lead-howto/SKILL.md"))
        (start 0))
    (should (< (length text) 1200))
    (while (string-match "(\\([a-z-]+\\.md\\))" text start)
      (should (file-exists-p (lead-lazy-tests--file
                              (concat "skills/lead-howto/" (match-string 1 text)))))
      (setq start (match-end 0)))))

(ert-deftest lead-lazy-howto-is-not-inlined-by-the-preset ()
  (let ((fields (car (ygg-preset--parse (lead-lazy-tests--file "presets/lead.md")))))
    (should-not (member "lead-howto" (ygg-preset-names (plist-get fields :skills))))))

(ert-deftest lead-lazy-howto-parses-with-a-short-description ()
  (let* ((parsed (ygg-preset--parse (lead-lazy-tests--file "skills/lead-howto/SKILL.md")))
         (description (plist-get (car parsed) :description)))
    (should (equal "lead-howto" (plist-get (car parsed) :name)))
    (should (stringp description))
    (should (<= (length description) 150))
    (should-not (plist-get (car parsed) :disable-model-invocation))
    (should (string-match-p "(briefs\\.md)" (cdr parsed)))))

(provide 'lead-lazy-tests)
;;; lead-lazy-tests.el ends here
