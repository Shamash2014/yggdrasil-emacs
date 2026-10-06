;;; ygg-pi-harness-tests.el --- pi behind aob: config home, MCP environment, launch variables -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ygg-agent-conf)
(require 'ygg-pi)
(require 'aob)
(require 'aob-acp)

(declare-function ygg-agent-terminal-env "layer-aob")

(defvar ygg-pi-tests--root nil)

(defun ygg-pi-tests--file (rel content)
  (let ((path (expand-file-name rel ygg-pi-tests--root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))
    path))

(defmacro ygg-pi-tests--with (&rest body)
  (declare (indent 0))
  `(let* ((ygg-pi-tests--root (file-truename (make-temp-file "ygg-pi-" t)))
          (process-environment (cons (concat "HOME=" ygg-pi-tests--root)
                                     process-environment))
          (ygg-agent-conf-root (expand-file-name "conf" ygg-pi-tests--root)))
     (clrhash ygg-agent--config-dirs)
     (make-directory (expand-file-name "proj" ygg-pi-tests--root) t)
     (unwind-protect (progn ,@body)
       (clrhash ygg-agent--config-dirs)
       (delete-directory ygg-pi-tests--root t))))

(defun ygg-pi-tests--proj ()
  (file-name-as-directory (expand-file-name "proj" ygg-pi-tests--root)))

(defun ygg-pi-tests--json (path)
  (ygg-agent--read-json path))

(ert-deftest ygg-pi-home-falls-back-to-the-conf-root ()
  (ygg-pi-tests--with
    (should (equal (ygg-agent--config-env "pi" "npx -y pi-acp" (ygg-pi-tests--proj))
                   (format "PI_CODING_AGENT_DIR=%s/conf/proj/pi" ygg-pi-tests--root)))
    (should (file-directory-p (expand-file-name "conf/proj/pi" ygg-pi-tests--root)))))

(ert-deftest ygg-pi-home-marker-wins ()
  (ygg-pi-tests--with
    (ygg-pi-tests--file "proj/.pi-agent-dir" "shared-pi\n")
    (should (equal (ygg-agent--config-env "pi" "pi" (ygg-pi-tests--proj))
                   (format "PI_CODING_AGENT_DIR=%s/proj/shared-pi" ygg-pi-tests--root)))))

(ert-deftest ygg-pi-known-config-env-emits-the-variable ()
  (ygg-pi-tests--with
    (ygg-agent--config-env "pi" "pi" (ygg-pi-tests--proj))
    (should (string-prefix-p "PI_CODING_AGENT_DIR="
                             (ygg-agent--known-config-env "pi" "pi" (ygg-pi-tests--proj))))))

(ert-deftest ygg-pi-kind-matches-the-name-only ()
  (should (equal (ygg-agent--kind "pi" "x") "pi"))
  (should (equal (ygg-agent--kind "pi-foo" "x") "pi"))
  (dolist (cmd '("npx -y pi-acp" "pipx run thing" "/Users/pi/bin/hermes-acp"
                 "/opt/pi/bin/thing" "/Users/shamash/pi-tools/x"
                 "/bin/zsh -l -c npx\\ -y\\ pi-acp"))
    (should-not (ygg-agent--kind "x" cmd)))
  (should-not (ygg-agent--kind "pipe" "x")))

(ert-deftest ygg-pi-kind-keeps-claude-and-codex-as-before ()
  (dolist (case '(("x" "npx -y @agentclientprotocol/claude-agent-acp" "claude")
                  ("x" "npx -y @zed-industries/codex-acp" "codex")
                  ("x" "claudecode" "claude")
                  ("x" "my-claude" "claude")
                  ("x" "claude_agent_acp" "claude")
                  ("claude-isolated" "x" "claude")
                  ("codex-2" "x" "codex")
                  ("x" "/usr/local/bin/claude" "claude")
                  ("x" "pipx run claude-agent-acp" "claude")
                  ("x" "gemini --experimental-acp" nil)))
    (should (equal (ygg-agent--kind (nth 0 case) (nth 1 case)) (nth 2 case)))))

(ert-deftest ygg-pi-home-is-seeded-from-the-global-home-without-overwriting ()
  (ygg-pi-tests--with
    (ygg-pi-tests--file ".pi/agent/settings.json"
                        "{\"packages\":[\"npm:p\"],\"defaultProvider\":\"provA\",\"defaultModel\":\"mA\",\"theme\":\"dark\"}")
    (ygg-pi-tests--file ".pi/agent/models.json"
                        "{\"providers\":{\"provA\":{\"baseUrl\":\"http://a\"},\"provB\":{\"baseUrl\":\"http://b\"}}}")
    (ygg-pi-tests--file "conf/proj/pi/settings.json" "{\"defaultProvider\":\"provB\"}")
    (ygg-pi-tests--file "conf/proj/pi/models.json"
                        "{\"providers\":{\"provB\":{\"baseUrl\":\"http://mine\"}}}")
    (ygg-agent--config-env "pi" "pi" (ygg-pi-tests--proj))
    (let* ((home (expand-file-name "conf/proj/pi" ygg-pi-tests--root))
           (settings (ygg-pi-tests--json (expand-file-name "settings.json" home)))
           (providers (gethash "providers" (ygg-pi-tests--json (expand-file-name "models.json" home)))))
      (should (equal (gethash "defaultProvider" settings) "provB"))
      (should (equal (gethash "defaultModel" settings) "mA"))
      (should (equal (gethash "packages" settings) ["npm:p"]))
      (should-not (gethash "theme" settings))
      (should (equal (gethash "baseUrl" (gethash "provA" providers)) "http://a"))
      (should (equal (gethash "baseUrl" (gethash "provB" providers)) "http://mine")))))

(ert-deftest ygg-pi-fresh-home-takes-the-global-config ()
  (ygg-pi-tests--with
    (ygg-pi-tests--file ".pi/agent/settings.json" "{\"defaultProvider\":\"provA\",\"packages\":[\"npm:p\"]}")
    (ygg-pi-tests--file ".pi/agent/models.json" "{\"providers\":{\"provA\":{}}}")
    (ygg-agent--config-env "pi" "pi" (ygg-pi-tests--proj))
    (let ((home (expand-file-name "conf/proj/pi" ygg-pi-tests--root)))
      (should (equal (gethash "defaultProvider"
                              (ygg-pi-tests--json (expand-file-name "settings.json" home)))
                     "provA"))
      (should (gethash "provA" (gethash "providers"
                                        (ygg-pi-tests--json (expand-file-name "models.json" home))))))))

(ert-deftest ygg-pi-home-without-a-global-config-stays-empty ()
  (ygg-pi-tests--with
    (ygg-agent--config-env "pi" "pi" (ygg-pi-tests--proj))
    (let ((home (expand-file-name "conf/proj/pi" ygg-pi-tests--root)))
      (should-not (file-exists-p (expand-file-name "settings.json" home)))
      (should-not (file-exists-p (expand-file-name "models.json" home))))))

(ert-deftest ygg-pi-session-env-failure-warns-and-continues ()
  (let (warned
        (aob-acp-session-env-function (lambda (_s) (error "boom"))))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warned msg)))
              ((symbol-function 'aob-session-ref) (lambda (&rest _) "pi"))
              ((symbol-function 'aob-acp--codex-agent-p) #'ignore)
              ((symbol-function 'aob-acp--cap-env) (lambda (_s) '("A=1"))))
      (should (equal (aob-acp--agent-env 'sess) '("A=1")))
      (should (string-match-p "pi.*boom" warned)))))

(ert-deftest ygg-pi-home-shares-resources-not-credentials ()
  (ygg-pi-tests--with
    (dolist (dir '("skills" "prompts" "npm" "extensions" "themes" "sessions"))
      (make-directory (expand-file-name (concat ".pi/agent/" dir) ygg-pi-tests--root) t))
    (dolist (file '("auth.json" "settings.json" "models.json"))
      (ygg-pi-tests--file (concat ".pi/agent/" file) "{}"))
    (ygg-agent--config-env "pi" "pi" (ygg-pi-tests--proj))
    (let ((home (expand-file-name "conf/proj/pi" ygg-pi-tests--root)))
      (dolist (shared '("skills" "prompts" "npm" "extensions" "themes"))
        (should (file-symlink-p (expand-file-name shared home))))
      (dolist (own '("auth.json" "settings.json" "models.json" "sessions"))
        (should-not (file-exists-p (expand-file-name own home)))))))

(ert-deftest ygg-pi-user-mcp-reads-the-project-homes-mcp-json ()
  (ygg-pi-tests--with
    (ygg-pi-tests--file
     "conf/proj/pi/mcp.json"
     "{\"mcpServers\":{\"docs\":{\"url\":\"https://x.example/mcp\"},\"fs\":{\"command\":\"npx\",\"args\":[\"-y\",\"srv\"]}}}")
    (ygg-pi-tests--file "proj/.pi-agent-dir.unused" "")
    (let ((servers (ygg-agent-user-mcp-servers "pi" (ygg-pi-tests--proj))))
      (should (equal (sort (mapcar (lambda (s) (plist-get s :name)) servers) #'string<)
                     '("docs" "fs")))
      (should (equal (plist-get (seq-find (lambda (s) (equal (plist-get s :name) "docs"))
                                          servers)
                                :url)
                     "https://x.example/mcp")))))

(defun ygg-pi-tests--session (id agent token &optional extra)
  (let ((s (aob-create-session :id (concat "acp:" id) :backend 'acp :name id
                               :project (ygg-pi-tests--proj) :dir (ygg-pi-tests--proj)
                               :state 'starting :refs (list :agent agent))))
    (aob-session-put s :mcp-declared
                     (append (list (list :name "aob" :type "http"
                                         :url (format "http://127.0.0.1:1/mcp?session=%s" token)))
                             extra))
    s))

(defun ygg-pi-tests--env-json (env)
  (let ((entry (seq-find (lambda (v) (string-prefix-p "AOB_PI_MCP_SERVERS=" v)) env)))
    (json-parse-string (substring entry (length "AOB_PI_MCP_SERVERS="))
                       :object-type 'plist :array-type 'list)))

(ert-deftest ygg-pi-session-env-sets-the-adapter-and-the-servers-for-pi-only ()
  (ygg-pi-tests--with
    (let ((pi (ygg-pi-tests--session "p1" "pi" "tok1"))
          (claude (ygg-pi-tests--session "c1" "claude" "tok1")))
      (let ((env (ygg-pi-session-env pi)))
        (should (member (concat "PI_ACP_PI_COMMAND=" ygg-pi--wrapper) env))
        (should (file-executable-p ygg-pi--wrapper))
        (let ((servers (ygg-pi-tests--env-json env)))
          (should (equal (mapcar (lambda (s) (plist-get s :name)) servers) '("aob")))
          (should (equal (plist-get (car servers) :url)
                         "http://127.0.0.1:1/mcp?session=tok1"))
          (should (equal (plist-get (car servers) :type) "http"))))
      (should-not (ygg-pi-session-env claude)))))

(ert-deftest ygg-pi-session-env-matches-the-wire-entries ()
  (ygg-pi-tests--with
    (let* ((extra (list (list :name "tools" :command "/bin/echo" :args '("a" "b")
                              :env '(:K "v"))))
           (s (ygg-pi-tests--session "p1" "pi" "tok1" extra))
           (servers (ygg-pi-tests--env-json (ygg-pi-session-env s)))
           (tools (seq-find (lambda (e) (equal (plist-get e :name) "tools")) servers)))
      (should (equal (plist-get tools :command) "/bin/echo"))
      (should (equal (plist-get tools :args) '("a" "b")))
      (should (equal (plist-get tools :env) '((:name "K" :value "v")))))))

(ert-deftest ygg-pi-session-env-adds-lat-only-where-lat-md-exists ()
  (ygg-pi-tests--with
    (let ((s (ygg-pi-tests--session "p1" "pi" "tok1")))
      (should-not (seq-find (lambda (e) (equal (plist-get e :name) "lat"))
                            (ygg-pi-tests--env-json (ygg-pi-session-env s))))
      (make-directory (expand-file-name "lat.md" (ygg-pi-tests--proj)) t)
      (let ((lat (seq-find (lambda (e) (equal (plist-get e :name) "lat"))
                           (ygg-pi-tests--env-json (ygg-pi-session-env s)))))
        (should lat)
        (should (equal (plist-get lat :args) '("mcp")))
        (should (string-suffix-p "lat" (plist-get lat :command)))))))

(ert-deftest ygg-pi-session-env-leaves-out-what-pi-loads-itself ()
  (ygg-pi-tests--with
    (ygg-pi-tests--file "conf/proj/pi/mcp.json"
                        "{\"mcpServers\":{\"tools\":{\"url\":\"https://x.example/mcp\"}}}")
    (let* ((extra (list (list :name "tools" :url "https://x.example/mcp" :type "http")))
           (s (ygg-pi-tests--session "p1" "pi" "tok1" extra)))
      (should (equal (mapcar (lambda (e) (plist-get e :name))
                             (ygg-pi-tests--env-json (ygg-pi-session-env s)))
                     '("aob"))))))

(ert-deftest ygg-pi-sessions-get-their-own-connection ()
  (ygg-pi-tests--with
    (let* ((aob-acp-session-env-function #'ygg-pi-session-env)
           (aob-acp-environment-function nil)
           (a (ygg-pi-tests--session "p1" "pi" "tok1"))
           (b (ygg-pi-tests--session "p2" "pi" "tok2"))
           (key (lambda (s)
                  (let ((aob-acp--session-env (aob-acp--agent-env s)))
                    (aob-acp--conn-key "pi" (ygg-pi-tests--proj))))))
      (should (equal (funcall key a) (funcall key a)))
      (should-not (equal (funcall key a) (funcall key b))))))

(ert-deftest ygg-pi-open-keeps-the-servers-it-was-handed-for-the-env ()
  (ygg-pi-tests--with
    (let ((aob-acp-mcp-servers (list (list :name "aob" :type "http" :url "http://h/mcp?session=t9")))
          s)
      (cl-letf (((symbol-function 'aob-acp--connect) #'ignore))
        (setq s (aob-acp--open "pi" "open-t9" (ygg-pi-tests--proj) (ygg-pi-tests--proj)
                               (lambda (_init) nil) #'ignore)))
      (should (equal (aob-session-ref s :mcp-declared) aob-acp-mcp-servers))
      (should (equal (plist-get (car (ygg-pi-tests--env-json (ygg-pi-session-env s))) :url)
                     "http://h/mcp?session=t9")))))

(ert-deftest ygg-pi-extension-registers-mapped-servers ()
  (skip-unless (executable-find "node"))
  (let* ((ext (expand-file-name "../etc/pi/aob-pi-mcp.js"
                                (file-name-directory (locate-library "ygg-pi"))))
         (servers (json-serialize
                   (vector
                    (list :name "aob side.car" :type "http" :url "http://h/mcp?s=1"
                          :headers (vector (list :name "X-K" :value "v")))
                    (list :name "tools" :command "/bin/echo" :args (vector "a")
                          :env (vector (list :name "K" :value "v")))
                    (list :name "bare" :type "http" :url "http://h/b" :headers (vector))
                    (list :name "lat" :command "lat" :args (vector "mcp") :env (vector)
                          :cwd "/p"))))
         (script (format "const m = await import(%s); const calls = []; m.default({ registerMcpServer: (n, c) => calls.push([n, c]), on() {} }); console.log(JSON.stringify(calls));"
                         (json-serialize (concat "file://" ext))))
         (out (with-temp-buffer
                (let ((default-directory temporary-file-directory)
                      (process-environment (cons (concat "AOB_PI_MCP_SERVERS=" servers)
                                                 process-environment)))
                  (call-process "node" nil (list t nil) nil "--input-type=module" "-e" script))
                (json-parse-string (buffer-string) :object-type 'plist :array-type 'list))))
    (should (equal (mapcar #'car out) '("aob_side_car" "tools" "bare" "lat")))
    (should (equal (nth 1 (car out))
                   '(:exposure "direct" :url "http://h/mcp?s=1" :headers (:X-K "v"))))
    (should (equal (nth 1 (nth 1 out))
                   '(:exposure "direct" :command "/bin/echo" :args ("a") :env (:K "v"))))
    (should (equal (nth 1 (nth 2 out)) '(:exposure "direct" :url "http://h/b")))
    (should (equal (plist-get (nth 1 (nth 3 out)) :cwd) "/p"))))

(ert-deftest ygg-pi-extension-registers-nothing-without-servers ()
  (skip-unless (executable-find "node"))
  (let* ((ext (expand-file-name "../etc/pi/aob-pi-mcp.js"
                                (file-name-directory (locate-library "ygg-pi"))))
         (script (format "const m = await import(%s); const calls = []; m.default({ registerMcpServer: (n, c) => calls.push(n), on() {} }); console.log(JSON.stringify(calls));"
                         (json-serialize (concat "file://" ext))))
         (out (with-temp-buffer
                (let ((default-directory temporary-file-directory)
                      (process-environment (cons "AOB_PI_MCP_SERVERS=not json"
                                                 process-environment)))
                  (call-process "node" nil (list t nil) nil "--input-type=module" "-e" script))
                (string-trim (buffer-string)))))
    (should (equal out "[]"))))

(defun ygg-pi-tests--node-plan (servers)
  (let* ((ext (expand-file-name "../etc/pi/aob-pi-mcp.js"
                                (file-name-directory (locate-library "ygg-pi"))))
         (script (format "const m = await import(%s); const calls = []; const notes = []; let h; m.default({ registerMcpServer: (n, c) => calls.push(n), on: (e, f) => { h = f; } }); if (h) h({}, { ui: { notify: (t) => notes.push(t) } }); console.log(JSON.stringify([calls, notes]));"
                         (json-serialize (concat "file://" ext)))))
    (with-temp-buffer
      (let ((default-directory temporary-file-directory)
            (process-environment (cons (concat "AOB_PI_MCP_SERVERS=" servers)
                                       process-environment)))
        (call-process "node" nil (list t nil) nil "--input-type=module" "-e" script))
      (json-parse-string (buffer-string) :object-type 'plist :array-type 'list))))

(ert-deftest ygg-pi-extension-skips-malformed-entries-and-says-so ()
  (skip-unless (executable-find "node"))
  (let ((out (ygg-pi-tests--node-plan
              "[{\"name\":\"a\",\"command\":\"x\",\"args\":\"notarray\"},{\"name\":\"b\",\"command\":\"x\",\"args\":[1]},{\"name\":\"c\",\"url\":5},{\"name\":\"d\",\"command\":\"x\",\"env\":\"s\"},{\"name\":\"e\",\"command\":\"x\",\"args\":[\"ok\"]},null,{\"name\":\"f\",\"type\":\"sse\",\"url\":\"http://h\"}]")))
    (should (equal (car out) '("e")))
    (should (= (length (cadr out)) 6))
    (should (string-match-p "sse" (car (last (cadr out)))))))

(ert-deftest ygg-pi-extension-suffixes-names-that-collapse ()
  (skip-unless (executable-find "node"))
  (let ((out (ygg-pi-tests--node-plan
              "[{\"name\":\"a b\",\"url\":\"http://1\"},{\"name\":\"a.b\",\"url\":\"http://2\"},{\"name\":\"a_b\",\"url\":\"http://3\"}]")))
    (should (equal (car out) '("a_b" "a_b_2" "a_b_3")))
    (should-not (cadr out))))

(defmacro ygg-pi-tests--collect-warnings (var &rest body)
  (declare (indent 1))
  `(let (,var)
     (cl-letf (((symbol-function 'display-warning)
                (lambda (_type msg &rest _) (push msg ,var))))
       ,@body)
     (setq ,var (nreverse ,var))))

(defmacro ygg-pi-tests--without-keychain (&rest body)
  (declare (indent 0))
  `(cl-letf (((symbol-function 'ygg-agent--logged-in-p) (lambda (&rest _) t))
             ((symbol-function 'ygg-agent--adopt-plugin-mcp) #'ignore)
             ((symbol-function 'ygg-agent--share-mcp-auth) #'ignore))
     ,@body))

(defconst ygg-pi-tests--bad-json '("{not json" "[1,2]" "42" "\"s\""))

(defun ygg-pi-tests--seed-case (kind where bad)
  (let* ((file (if (equal kind "pi") ".pi/agent/settings.json" ".claude/settings.json"))
         (home (format "conf/proj/%s" kind))
         (project-file (format "%s/settings.json" home))
         (var (if (equal kind "pi") "PI_CODING_AGENT_DIR" "CLAUDE_CONFIG_DIR"))
         (expected (format "%s=%s/%s" var ygg-pi-tests--root home))
         (good "{\"defaultProvider\":\"provA\",\"enabledPlugins\":{\"p\":true}}"))
    (ygg-pi-tests--file file (if (eq where 'global) bad good))
    (ygg-pi-tests--file project-file (if (eq where 'project) bad "{\"theme\":\"mine\"}"))
    (when (equal kind "pi")
      (ygg-pi-tests--file ".pi/agent/models.json" "{\"providers\":{\"provA\":{}}}"))
    (let* (env
           (warnings (ygg-pi-tests--collect-warnings w
                       (ygg-pi-tests--without-keychain
                         (setq env (ygg-agent--config-env kind kind (ygg-pi-tests--proj))))))
           (culprit (expand-file-name (if (eq where 'global) file project-file)
                                      ygg-pi-tests--root)))
      (should (equal env expected))
      (should (seq-some (lambda (w) (string-search culprit w)) warnings))
      (should (equal (with-temp-buffer
                       (insert-file-contents (expand-file-name project-file ygg-pi-tests--root))
                       (buffer-string))
                     (if (eq where 'project) bad "{\"theme\":\"mine\"}")))
      (when (and (equal kind "pi") (eq where 'global))
        (should (gethash "provA" (gethash "providers"
                                          (ygg-pi-tests--json
                                           (expand-file-name "models.json"
                                                             (expand-file-name home ygg-pi-tests--root))))))))))

(ert-deftest ygg-pi-malformed-config-warns-seeds-the-rest-and-keeps-the-project-home ()
  (dolist (kind '("pi" "claude"))
    (dolist (where '(global project))
      (dolist (bad ygg-pi-tests--bad-json)
        (ygg-pi-tests--with
          (ygg-pi-tests--seed-case kind where bad))))))

(ert-deftest ygg-pi-home-env-warns-instead-of-failing-silently ()
  (let ((warnings (ygg-pi-tests--collect-warnings w
                    (should-not (ygg-agent--home-env "pi" "pi" (lambda (_k _s) (error "boom")))))))
    (should (seq-some (lambda (w) (string-match-p "boom" w)) warnings))))

(ert-deftest ygg-pi-config-homes-answer-for-every-kind-the-terminal-env-folds ()
  (ygg-pi-tests--with
    (ygg-pi-tests--without-keychain
      (let ((env (delq nil (mapcar (lambda (kind)
                                     (ygg-agent--config-env kind kind (ygg-pi-tests--proj)))
                                   (mapcar #'car ygg-agent--config-homes)))))
        (dolist (var '("CLAUDE_CONFIG_DIR" "CODEX_HOME" "PI_CODING_AGENT_DIR"))
          (should (seq-some (lambda (e) (string-prefix-p (concat var "=") e)) env)))))))

(ert-deftest ygg-pi-terminal-env-exports-the-pi-home-with-the-others ()
  (skip-unless (require 'layer-aob nil t))
  (skip-unless (fboundp 'ygg-agent-terminal-env))
  (ygg-pi-tests--with
    (ygg-pi-tests--without-keychain
      (let ((env (ygg-agent-terminal-env (ygg-pi-tests--proj))))
        (dolist (var '("CLAUDE_CONFIG_DIR" "CODEX_HOME" "PI_CODING_AGENT_DIR"))
          (should (seq-some (lambda (e) (string-prefix-p (concat var "=") e)) env)))))))

(ert-deftest ygg-pi-approve-env-only-when-the-option-is-t ()
  (ygg-pi-tests--with
    (let ((s (ygg-pi-tests--session "p1" "pi" "tok1")))
      (let ((ygg-pi-approve-project t))
        (should (member "AOB_PI_APPROVE=1" (ygg-pi-session-env s))))
      (let ((ygg-pi-approve-project nil))
        (should-not (member "AOB_PI_APPROVE=1" (ygg-pi-session-env s)))))))

(ert-deftest ygg-pi-agent-p-follows-the-home-rule ()
  (let ((aob-acp-agents '(("pi" :command ("npx" "-y" "pi-acp"))
                          ("pi-work" :command ("npx" "-y" "pi-acp"))
                          ("raw" :command ("/opt/bin/pi" "--mode" "rpc"))
                          ("x" :command ("npx" "-y" "pi-acp"))
                          ("claude" :command ("npx" "claude-agent-acp"))
                          ("pipe" :command ("pipe")))))
    (dolist (agent '("pi" "pi-work" "raw"))
      (should (ygg-pi-agent-p agent)))
    (dolist (agent '("x" "claude" "pipe" "missing" nil))
      (should-not (ygg-pi-agent-p agent)))))

(ert-deftest ygg-pi-own-server-names-read-the-sessions-pi-home ()
  (ygg-pi-tests--with
    (ygg-pi-tests--file ".pi-agent-dir.unused" "")
    (ygg-pi-tests--file "proj/.pi-agent-dir" "shared-pi\n")
    (ygg-pi-tests--file "proj/shared-pi/mcp.json"
                        "{\"mcpServers\":{\"mine\":{\"url\":\"https://x.example/mcp\"}}}")
    (let ((aob-acp-agents '(("pi-work" :command ("npx" "-y" "pi-acp")))))
      (should (equal (ygg-pi--own-server-names "pi-work" (ygg-pi-tests--proj))
                     '("mine"))))))

(defun ygg-pi-tests--skipped-names (warnings)
  (let ((msg (car warnings)))
    (and msg (string-match "skipped: \\(.*\\)\\'" msg) (match-string 1 msg))))

(ert-deftest ygg-pi-session-env-warns-once-and-drops-entries-pi-cannot-load ()
  (ygg-pi-tests--with
    (let* ((extra (list (list :name "badurl" :url 5)
                        (list :name "badargs" :command "x" :args '("a" 1))
                        (list :name "badheaders" :type "http" :url "http://h" :headers '(("K" . "v")))
                        (list :name "empty")
                        (list :name "legacy" :type "sse" :url "http://h/sse")
                        (list :name "good" :command "x" :args '("ok"))))
           (s (ygg-pi-tests--session "p1" "pi" "tok1" extra))
           servers)
      (let ((warnings (ygg-pi-tests--collect-warnings w
                        (setq servers (ygg-pi-tests--env-json (ygg-pi-session-env s)))
                        (ygg-pi-session-env s))))
        (should (equal (mapcar (lambda (e) (plist-get e :name)) servers) '("aob" "good")))
        (should (= (length warnings) 1))
        (dolist (name '("badurl" "badargs" "badheaders" "empty" "legacy"))
          (should (string-match-p (concat name " (") (car warnings))))
        (should (string-match-p "url is not a string" (car warnings)))
        (should (string-match-p "args are not a list of strings" (car warnings)))
        (should (string-match-p "headers are not an alist or plist" (car warnings)))
        (should (string-match-p "neither url nor command" (car warnings)))
        (should (string-match-p "not sse" (car warnings)))))))

(ert-deftest ygg-pi-session-env-stays-quiet-when-every-entry-loads ()
  (ygg-pi-tests--with
    (let ((s (ygg-pi-tests--session "p1" "pi" "tok1")))
      (should-not (ygg-pi-tests--collect-warnings w (ygg-pi-session-env s))))))

(ert-deftest ygg-pi-entry-problem-rejects-a-nameless-entry ()
  (should (equal (ygg-pi--entry-problem (list :command "x")) "no name"))
  (should (equal (ygg-pi--entry-problem (list :name "" :command "x")) "no name"))
  (should (equal (ygg-pi--entry-problem (list :name 3 :command "x")) "no name"))
  (should-not (ygg-pi--entry-problem (list :name "a" :command "x" :args [] :env []))))

(ert-deftest ygg-pi-entry-problem-names-the-reason ()
  (dolist (case '(((:name "a" :command "x" :env (("K" . "v"))) "env is not an alist or plist")
                  ((:name "a" :command "x" :cwd 7) "cwd is not a string")
                  ((:name "a" :command 5) "command is not a string")
                  ((:name "a" :command "x" :args "s") "args are not a list of strings")
                  ((:name "a" :url "http://h" :headers "s") "headers are not an alist or plist")
                  ((:name "a" :type "sse" :url "http://h") "pi supports stdio and streamable HTTP, not sse")))
    (should (equal (ygg-pi--entry-problem (car case)) (cadr case)))))

(provide 'ygg-pi-harness-tests)
;;; ygg-pi-harness-tests.el ends here
