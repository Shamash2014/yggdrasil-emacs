;;; aob-trim-tests.el --- the text every agent is sent stays short and says the same -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob-mcp)
(require 'aob-mcp-tools)
(require 'ygg-agent-conf)

(defconst aob-trim-tests--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defconst aob-trim-tests--schemas
  '(("diagnostics" ("file" "checker") ("file"))
    ("imenu_symbols" ("file") ("file"))
    ("session_list" () ())
    ("session_read" ("id") ("id"))
    ("session_say" ("id" "text") ("id" "text"))
    ("todo_add" ("text" "section" "file") ("text"))
    ("todo_list" ("file" "all") ())
    ("todo_remove" ("id" "expect" "file") ("id"))
    ("todo_update" ("id" "done" "text" "expect" "file") ("id"))
    ("todo_write" ("file" "title" "slug" "sections") ())
    ("tool_names" ("prefix") ())
    ("treesit_info" ("file" "line" "column") ("file"))
    ("xref_apropos" ("file" "pattern") ("file" "pattern"))
    ("xref_references" ("file" "symbol") ("file" "symbol"))))

(ert-deftest aob-trim-tools-list-is-short-and-keeps-every-argument ()
  (let ((listing (aob-mcp--listing)))
    (should (< (length (json-serialize `(:tools ,listing))) 5500))
    (should (equal (mapcar (lambda (tool) (plist-get tool :name)) listing)
                   (mapcar #'car aob-trim-tests--schemas)))
    (seq-doseq (tool listing)
      (let* ((schema (plist-get tool :inputSchema))
             (props (plist-get schema :properties))
             (want (cdr (assoc (plist-get tool :name) aob-trim-tests--schemas))))
        (should (equal (cl-loop for (k _) on props by #'cddr
                                collect (substring (symbol-name k) 1))
                       (car want)))
        (should (equal (append (plist-get schema :required) nil) (cadr want)))
        (should (> (length (plist-get tool :description)) 0))))))

(ert-deftest aob-trim-instructions-point-at-the-aob-server ()
  (should (string-match-p "aob MCP server" ygg-agent-instructions)))

(ert-deftest aob-trim-lead-does-not-paste-the-build-preset ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "presets/lead.md" aob-trim-tests--root))
                (buffer-string))))
    (should-not (string-match-p "build preset you[ \n]+were handed" text))
    (should (string-match-p "do not paste one" text))))

(provide 'aob-trim-tests)
;;; aob-trim-tests.el ends here
