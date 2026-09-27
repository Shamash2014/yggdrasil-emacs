;;; skills-description-tests.el --- Every shipped SKILL.md lists cheaply -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ygg-preset)

(defconst skills-description-tests--root
  (expand-file-name "../skills" (file-name-directory (or load-file-name buffer-file-name))))

(defconst skills-description-tests--limit 150)

(defun skills-description-tests--files ()
  (directory-files-recursively skills-description-tests--root "\\`SKILL\\.md\\'"))

(defun skills-description-tests--closed-p (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (and (looking-at "---[ \t]*\n")
         (progn (forward-line) (re-search-forward "^---[ \t]*$" nil t))
         t)))

(defun skills-description-tests--unquote (raw)
  "RAW as the text a YAML reader would hand the model."
  (cond ((string-match "\\`'\\(.*\\)'\\'" raw)
         (string-replace "''" "'" (match-string 1 raw)))
        ((string-match-p "\\`\".*\"\\'" raw) (read raw))
        (t raw)))

(defun skills-description-tests--fields (file)
  (car (ygg-preset--parse file)))

(ert-deftest skills-description-every-skill-has-frontmatter-name-and-description ()
  (let ((files (skills-description-tests--files)))
    (should (> (length files) 40))
    (dolist (file files)
      (let ((fields (skills-description-tests--fields file)))
        (should (equal (list file t) (list file (skills-description-tests--closed-p file))))
        (should (equal (list file t)
                       (list file (and (stringp (plist-get fields :name))
                                       (not (string-empty-p (plist-get fields :name)))))))
        (should (equal (list file t)
                       (list file (and (stringp (plist-get fields :description))
                                       (not (string-empty-p (plist-get fields :description)))))))))))

(ert-deftest skills-description-model-invocable-descriptions-fit-the-listing ()
  (let (over)
    (dolist (file (skills-description-tests--files))
      (let ((fields (skills-description-tests--fields file)))
        (unless (eq (plist-get fields :disable-model-invocation) t)
          (let ((text (skills-description-tests--unquote (plist-get fields :description))))
            (when (> (length text) skills-description-tests--limit)
              (push (cons (plist-get fields :name) (length text)) over))))))
    (should (equal over nil))))

(ert-deftest skills-description-unquote-reads-yaml-scalars ()
  (should (equal (skills-description-tests--unquote "'it''s: here'") "it's: here"))
  (should (equal (skills-description-tests--unquote "\"say \\\"hi\\\"\"") "say \"hi\""))
  (should (equal (skills-description-tests--unquote "plain text") "plain text")))

(provide 'skills-description-tests)
;;; skills-description-tests.el ends here
