;;; plugin-mcp-adopt-tests.el --- which plugin MCP servers a config home adopts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ygg-agent-conf)

(defvar plugin-mcp-adopt-tests--root nil)

(defun plugin-mcp-adopt-tests--file (rel content)
  (let ((path (expand-file-name rel plugin-mcp-adopt-tests--root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defun plugin-mcp-adopt-tests--plugin (market plugin version servers)
  "Cache PLUGIN of MARKET at VERSION with SERVERS, a list of (NAME . URL)."
  (plugin-mcp-adopt-tests--file
   (format "home/plugins/cache/%s/%s/%s/.mcp.json" market plugin version)
   (json-serialize
    (list :mcpServers
          (let ((table (make-hash-table :test #'equal)))
            (pcase-dolist (`(,name . ,url) servers)
              (puthash name (list :type "http" :url url) table))
            table)))))

(defun plugin-mcp-adopt-tests--settings (json)
  (plugin-mcp-adopt-tests--file "conf/settings.json" json))

(defun plugin-mcp-adopt-tests--config (json)
  (plugin-mcp-adopt-tests--file "conf/.claude.json" json))

(defun plugin-mcp-adopt-tests--servers ()
  "The conf home\\='s mcpServers as an alist of (NAME . URL-OR-COMMAND), sorted."
  (let ((conf (ygg-agent--read-json
               (expand-file-name "conf/.claude.json" plugin-mcp-adopt-tests--root)))
        out)
    (when-let* ((mcp (and conf (gethash "mcpServers" conf))))
      (maphash (lambda (name entry)
                 (push (cons name (or (gethash "url" entry) (gethash "command" entry))) out))
               mcp))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun plugin-mcp-adopt-tests--adopt ()
  (ygg-agent--adopt-plugin-mcp
   (list :home (expand-file-name "home" plugin-mcp-adopt-tests--root)
         :settings "settings.json" :adopt-plugin-mcp t)
   (expand-file-name "conf" plugin-mcp-adopt-tests--root)))

(defmacro plugin-mcp-adopt-tests--with (&rest body)
  (declare (indent 0))
  `(let* ((plugin-mcp-adopt-tests--root (make-temp-file "plugin-mcp-adopt-" t))
          (process-environment
           (cons (concat "HOME=" plugin-mcp-adopt-tests--root)
                 (seq-remove (lambda (var) (string-prefix-p "CLAUDE_CODE_PLUGIN_CACHE_DIR=" var))
                             process-environment)))
          (ygg-agent-conf-root nil))
     (make-directory (expand-file-name "conf" plugin-mcp-adopt-tests--root) t)
     (unwind-protect (progn ,@body)
       (delete-directory plugin-mcp-adopt-tests--root t))))

(ert-deftest plugin-mcp-adopt-enabled-plugin-adopted ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (should (equal (plugin-mcp-adopt-tests--adopt) '("linear-mcp")))
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("linear-mcp" . "https://linear.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-disabled-plugin-not-adopted ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--plugin "mkt" "notion" "1.0.0" '(("notion" . "https://notion.example/mcp")))
    (plugin-mcp-adopt-tests--settings
     "{\"enabledPlugins\":{\"linear@mkt\":false,\"notion@mkt\":true}}")
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("notion-mcp" . "https://notion.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-one-server-from-current-version ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://old.example/mcp")))
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.2.0" '(("linear" . "https://new.example/mcp")))
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("linear-mcp" . "https://new.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-install-record-wins-over-newest ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://old.example/mcp")))
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.2.0" '(("linear" . "https://new.example/mcp")))
    (plugin-mcp-adopt-tests--file
     "home/plugins/installed_plugins.json"
     "{\"version\":2,\"plugins\":{\"linear@mkt\":[{\"scope\":\"user\",\"installPath\":\"/elsewhere/cache/mkt/linear/1.0.0\",\"version\":\"1.0.0\"}]}}")
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("linear-mcp" . "https://old.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-orphaned-version-passed-over ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://kept.example/mcp")))
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "2.0.0" '(("linear" . "https://gone.example/mcp")))
    (plugin-mcp-adopt-tests--file "home/plugins/cache/mkt/linear/2.0.0/.orphaned_at" "1")
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("linear-mcp" . "https://kept.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-unreadable-settings-adopt-nothing ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (should-not (plugin-mcp-adopt-tests--adopt))
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\": {")
    (should-not (plugin-mcp-adopt-tests--adopt))
    (should-not (plugin-mcp-adopt-tests--servers))))

(ert-deftest plugin-mcp-adopt-unreadable-settings-remove-nothing ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--config
     "{\"mcpServers\":{\"linear-mcp\":{\"type\":\"http\",\"url\":\"https://linear.example/mcp\"}}}")
    (plugin-mcp-adopt-tests--settings "not json")
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("linear-mcp" . "https://linear.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-duplicate-url-skipped ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (plugin-mcp-adopt-tests--config
     "{\"mcpServers\":{\"my-linear\":{\"type\":\"http\",\"url\":\"https://linear.example/mcp\"}}}")
    (should-not (plugin-mcp-adopt-tests--adopt))
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("my-linear" . "https://linear.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-stale-adopted-entry-removed ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--plugin "mkt" "notion" "1.0.0" '(("notion" . "https://notion.example/mcp")))
    (plugin-mcp-adopt-tests--settings
     "{\"enabledPlugins\":{\"linear@mkt\":true,\"notion@mkt\":true}}")
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (mapcar #'car (plugin-mcp-adopt-tests--servers)) '("linear-mcp" "notion-mcp")))
    (plugin-mcp-adopt-tests--settings
     "{\"enabledPlugins\":{\"linear@mkt\":true,\"notion@mkt\":false}}")
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("linear-mcp" . "https://linear.example/mcp"))))))

(ert-deftest plugin-mcp-adopt-legacy-unmarked-entry-removed ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--config
     "{\"mcpServers\":{\"linear-mcp\":{\"type\":\"http\",\"url\":\"https://linear.example/mcp\"}}}")
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{}}")
    (plugin-mcp-adopt-tests--adopt)
    (should-not (plugin-mcp-adopt-tests--servers))))

(ert-deftest plugin-mcp-adopt-ledger-entry-removed-after-cache-gone ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (plugin-mcp-adopt-tests--adopt)
    (delete-directory (expand-file-name "home/plugins/cache/mkt/linear" plugin-mcp-adopt-tests--root) t)
    (plugin-mcp-adopt-tests--adopt)
    (should-not (plugin-mcp-adopt-tests--servers))))

(ert-deftest plugin-mcp-adopt-user-servers-never-touched ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--plugin "mkt" "notion" "1.0.0" '(("notion" . "https://notion.example/mcp")))
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (plugin-mcp-adopt-tests--config
     (concat "{\"mcpServers\":{"
             "\"local\":{\"command\":\"node\",\"args\":[\"srv.js\"]},"
             "\"mine\":{\"type\":\"http\",\"url\":\"https://mine.example/mcp\"},"
             "\"notion-mcp\":{\"type\":\"http\",\"url\":\"https://my-notion.example/mcp\"},"
             "\"linear-mcp\":{\"type\":\"http\",\"url\":\"https://my-linear.example/mcp\"}},"
             "\"theme\":\"dark\"}"))
    (plugin-mcp-adopt-tests--adopt)
    (should (equal (plugin-mcp-adopt-tests--servers)
                   '(("linear-mcp" . "https://my-linear.example/mcp")
                     ("local" . "node")
                     ("mine" . "https://mine.example/mcp")
                     ("notion-mcp" . "https://my-notion.example/mcp"))))
    (should (equal "dark" (gethash "theme" (ygg-agent--read-json
                                            (expand-file-name "conf/.claude.json"
                                                              plugin-mcp-adopt-tests--root)))))))

(ert-deftest plugin-mcp-adopt-corrupt-config-left-alone ()
  (plugin-mcp-adopt-tests--with
    (plugin-mcp-adopt-tests--plugin "mkt" "linear" "1.0.0" '(("linear" . "https://linear.example/mcp")))
    (plugin-mcp-adopt-tests--settings "{\"enabledPlugins\":{\"linear@mkt\":true}}")
    (let ((path (plugin-mcp-adopt-tests--config "{\"mcpServers\":")))
      (should-not (plugin-mcp-adopt-tests--adopt))
      (should (equal "{\"mcpServers\":"
                     (with-temp-buffer (insert-file-contents path) (buffer-string)))))))

(provide 'plugin-mcp-adopt-tests)
;;; plugin-mcp-adopt-tests.el ends here
