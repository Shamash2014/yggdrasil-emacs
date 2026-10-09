;;; aob-lat-mcp-tests.el --- the lat MCP server every ACP session gets -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)

(defconst aob-lat-tests--caps '(:agentCapabilities (:mcpCapabilities (:http t :sse t))))

(defmacro aob-lat-tests--with (var &rest body)
  (declare (indent 1))
  `(let* ((,var (file-truename (make-temp-file "aob-lat-" t)))
          (bin (expand-file-name "bin" ,var))
          (exec-path (cons bin exec-path))
          (process-environment (cons (concat "PATH=" bin path-separator (getenv "PATH"))
                                     process-environment))
          (aob-acp-lat-mcp t)
          (aob-acp-mcp-servers nil))
     (make-directory bin t)
     (dolist (name '("lat" "node"))
       (let ((f (expand-file-name name bin)))
         (with-temp-file f (insert "#!/bin/sh\n"))
         (set-file-modes f #o755)))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun aob-lat-tests--lats (root &optional init)
  (seq-filter (lambda (e) (equal (plist-get e :name) "lat"))
              (append (aob-acp--mcp-servers (or init aob-lat-tests--caps) root) nil)))

(ert-deftest aob-lat-default-attaches-nothing ()
  (should-not (default-value 'aob-acp-lat-mcp))
  (aob-lat-tests--with root
    (make-directory (expand-file-name "lat.md" root))
    (should (aob-acp-lat-entry root))
    (let ((aob-acp-lat-mcp nil))
      (should-not (aob-lat-tests--lats root)))))

(ert-deftest aob-lat-every-agent-gets-one-absolute-server-with-lat-md ()
  (aob-lat-tests--with root
    (make-directory (expand-file-name "lat.md" root))
    (dolist (agent '("claude" "codex" "pi"))
      (let ((lats (aob-lat-tests--lats
                   root (list :agentInfo (list :name agent)
                              :agentCapabilities '(:mcpCapabilities (:http t))))))
        (should (= 1 (length lats)))
        (should (equal (plist-get (car lats) :command) "/bin/sh"))
        (should (equal (plist-get (car lats) :args)
                       (vector "-c" aob-acp--lat-launcher "sh" root
                               (directory-file-name (expand-file-name "bin" root))
                               (expand-file-name "bin/lat" root))))))))

(ert-deftest aob-lat-launcher-takes-every-path-positionally ()
  (aob-lat-tests--with root
    (let ((odd (expand-file-name "a b'\"$x" root)))
      (make-directory (expand-file-name "lat.md" odd) t)
      (let ((args (plist-get (aob-acp-lat-entry odd) :args)))
        (should (equal (aref args 3) (directory-file-name odd)))
        (should-not (string-match-p "bin\\|a b" (aref args 1)))))))

(ert-deftest aob-lat-none-when-node-is-not-installed ()
  (aob-lat-tests--with root
    (make-directory (expand-file-name "lat.md" root))
    (delete-file (expand-file-name "bin/node" root))
    (let ((exec-path (list (expand-file-name "bin" root))))
      (should-not (aob-lat-tests--lats root)))))

(ert-deftest aob-lat-remote-is-checked-before-anything-else ()
  (should-not (aob-acp-lat-entry "/ssh:host:/proj")))

(ert-deftest aob-lat-none-without-lat-md ()
  (aob-lat-tests--with root
    (should-not (aob-lat-tests--lats root))))

(ert-deftest aob-lat-none-when-lat-is-not-installed ()
  (aob-lat-tests--with root
    (make-directory (expand-file-name "lat.md" root))
    (let ((exec-path nil)
          (process-environment (cons "PATH=/nonexistent" process-environment)))
      (should-not (aob-lat-tests--lats root)))))

(ert-deftest aob-lat-none-when-adapter-names-no-mcp ()
  (aob-lat-tests--with root
    (make-directory (expand-file-name "lat.md" root))
    (should (equal [] (aob-acp--mcp-servers '(:agentCapabilities (:loadSession t)) root)))))

(ert-deftest aob-lat-no-duplicate-when-project-declares-lat ()
  (aob-lat-tests--with root
    (make-directory (expand-file-name "lat.md" root))
    (with-temp-file (expand-file-name ".mcp.json" root)
      (insert "{\"mcpServers\":{\"lat\":{\"command\":\"/bin/echo\",\"args\":[\"mine\"]}}}"))
    (let ((lats (aob-lat-tests--lats root)))
      (should (= 1 (length lats)))
      (should (equal (plist-get (car lats) :command) "/bin/echo")))))

(ert-deftest aob-lat-no-duplicate-when-user-config-declares-lat ()
  (aob-lat-tests--with root
    (make-directory (expand-file-name "lat.md" root))
    (let ((aob-acp-mcp-servers (list (list :name "lat" :command "/bin/echo" :args '("u")))))
      (let ((lats (aob-lat-tests--lats root)))
        (should (= 1 (length lats)))
        (should (equal (plist-get (car lats) :command) "/bin/echo"))))))

(ert-deftest aob-lat-payload-without-lat-md-is-what-it-was ()
  (aob-lat-tests--with root
    (with-temp-file (expand-file-name ".mcp.json" root)
      (insert "{\"mcpServers\":{\"p\":{\"command\":\"/bin/echo\"}}}"))
    (let ((aob-acp-mcp-servers (list (list :name "u" :url "http://h/mcp"))))
      (should (equal (mapcar (lambda (e) (plist-get e :name))
                             (append (aob-acp--mcp-servers aob-lat-tests--caps root) nil))
                     '("p" "u"))))))

(defun aob-lat-tests--await (proc pred)
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (float-time) deadline) (not (funcall pred)))
      (accept-process-output proc 0.1))
    (funcall pred)))

(ert-deftest aob-lat-real-server-starts-from-a-foreign-cwd-and-minimal-path ()
  (skip-unless (and (executable-find "lat") (executable-find "node")))
  (let* ((proj (file-truename (make-temp-file "aob-lat-proj-" t)))
         (cwd (file-truename (make-temp-file "aob-lat-cwd-" t)))
         (entry (progn
                  (make-directory (expand-file-name "lat.md" proj))
                  (with-temp-file (expand-file-name "lat.md/lat.md" proj)
                    (insert "A minimal graph.\n"))
                  (aob-acp-lat-entry proj)))
         (out "")
         proc)
    (unwind-protect
        (let ((default-directory cwd)
              (process-environment (cons "PATH=/usr/bin:/bin" process-environment))
              (exec-path '("/usr/bin" "/bin")))
          (setq proc (make-process
                      :name "aob-lat-real" :connection-type 'pipe :noquery t
                      :command (cons (plist-get entry :command)
                                     (append (plist-get entry :args) nil))
                      :filter (lambda (_p s) (setq out (concat out s)))
                      :stderr (generate-new-buffer " *aob-lat-err*")))
          (process-send-string
           proc
           (concat "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-03-26\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}}\n"
                   "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                   "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}\n"))
          (should (aob-lat-tests--await
                   proc (lambda () (string-match-p "\"id\":2[^\n]*\"tools\"\\|\"tools\"[^\n]*\"id\":2" out)))))
      (when (process-live-p proc) (kill-process proc))
      (delete-directory proj t)
      (delete-directory cwd t))))

(provide 'aob-lat-mcp-tests)
;;; aob-lat-mcp-tests.el ends here
