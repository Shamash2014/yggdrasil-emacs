;;; ygg-agent-project-tool-mcp-tests.el --- repowise MCP for projects that index with it -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob-acp)
(require 'ygg-agent-conf)

(defmacro ygg-ptm-tests--with (var &rest body)
  (declare (indent 1))
  `(let* ((,var (file-truename (make-temp-file "ygg-ptm-" t)))
          (bin (expand-file-name "bin" ,var))
          (exec-path (list bin))
          (ygg-agent-project-tool-mcp '(repowise)))
     (make-directory bin t)
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun ygg-ptm-tests--fake (root name)
  (let ((f (expand-file-name name (expand-file-name "bin" root))))
    (with-temp-file f (insert "#!/bin/sh\n"))
    (set-file-modes f #o755)))

(defun ygg-ptm-tests--index (root)
  (make-directory (expand-file-name ".repowise" root))
  (with-temp-file (expand-file-name ".repowise/wiki.db" root) (insert "")))

(ert-deftest ygg-ptm-repowise-with-index-and-binary ()
  (ygg-ptm-tests--with root
    (ygg-ptm-tests--fake root "repowise")
    (ygg-ptm-tests--index root)
    (should (equal (ygg-agent-project-tool-mcp-servers root)
                   (list (list :name "repowise"
                               :command (expand-file-name "bin/repowise" root)
                               :args (vector "mcp" root "--transport" "stdio")
                               :env (list (list :name "DO_NOT_TRACK" :value "1")
                                          (list :name "REPOWISE_TELEMETRY_DISABLED"
                                                :value "1"))))))))

(ert-deftest ygg-ptm-nothing-without-index ()
  (ygg-ptm-tests--with root
    (ygg-ptm-tests--fake root "repowise")
    (should-not (ygg-agent-project-tool-mcp-servers root))))

(ert-deftest ygg-ptm-nothing-without-binary ()
  (ygg-ptm-tests--with root
    (ygg-ptm-tests--index root)
    (should-not (ygg-agent-project-tool-mcp-servers root))))

(ert-deftest ygg-ptm-nothing-when-turned-off-or-no-project ()
  (ygg-ptm-tests--with root
    (ygg-ptm-tests--fake root "repowise")
    (ygg-ptm-tests--index root)
    (let ((ygg-agent-project-tool-mcp nil))
      (should-not (ygg-agent-project-tool-mcp-servers root)))
    (should-not (ygg-agent-project-tool-mcp-servers nil))))

(ert-deftest ygg-ptm-not-duplicated-when-declared ()
  (ygg-ptm-tests--with root
    (ygg-ptm-tests--fake root "repowise")
    (ygg-ptm-tests--index root)
    (should-not (ygg-agent-project-tool-mcp-servers
                 root (list (list :name "repowise" :command "x"))))
    (should-not (ygg-agent-project-tool-mcp-servers
                 root (list (list :name "other" :command "/usr/bin/repowise"))))
    (with-temp-file (expand-file-name ".mcp.json" root)
      (insert "{\"mcpServers\":{\"repowise\":{\"command\":\"repowise\",\"args\":[\"mcp\"]}}}"))
    (should-not (ygg-agent-project-tool-mcp-servers root))))

(provide 'ygg-agent-project-tool-mcp-tests)
;;; ygg-agent-project-tool-mcp-tests.el ends here
