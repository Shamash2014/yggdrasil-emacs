;;; ygg-agent-conf.el --- per-project agent config homes, lifted out of layer-agent -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'subr-x)

(defun ygg-agent--plugin-mcp-servers ()
  "Remote MCP servers the installed plugins define, as (NAME . URL).
Read from the plugin caches rather than the CLI: these are the servers a
plugin brings, which is not the same set the CLI will hand to a session."
  (let (out)
    (dolist (file (file-expand-wildcards
                   (expand-file-name "~/.claude/plugins/cache/*/*/*/.mcp.json")))
      (when-let* ((json (ygg-agent--read-json file))
                  (servers (gethash "mcpServers" json)))
        (maphash (lambda (name spec)
                   (when-let* (((hash-table-p spec))
                               (url (gethash "url" spec)))
                     (push (cons name url) out)))
                 servers)))
    (seq-uniq (nreverse out))))

;;;###autoload

(defun ygg-agent--mcp-spec (name spec)
  "One server SPEC from a CLI config, in the shape a session is handed."
  (when (hash-table-p spec)
    (let ((url (gethash "url" spec))
          (type (or (gethash "type" spec) "stdio"))
          (command (gethash "command" spec)))
      (cond
       (url (list :name name :type (if (equal type "sse") "sse" "http") :url url))
       (command
        (list :name name :command command
              :args (vconcat (gethash "args" spec))
              :env (let (env)
                     (when-let* ((table (gethash "env" spec)))
                       (maphash (lambda (key value)
                                  (push (list :name key :value (format "%s" value))
                                        env))
                                table))
                     ;; a list of (:name :value), which is the shape the
                     ;; wire builder recognises; a vector is not
                     (nreverse env))))))))

(defun ygg-agent--json-mcp (file &optional project)
  "The servers FILE declares, and the ones it declares for PROJECT."
  (when-let* (((file-readable-p (expand-file-name file)))
              (json (ygg-agent--read-json (expand-file-name file))))
    (let (out)
      (dolist (table (delq nil
                           (list (gethash "mcpServers" json)
                                 (when-let* ((project)
                                             (projects (gethash "projects" json))
                                             (entry (or (gethash (directory-file-name
                                                                  (expand-file-name project))
                                                                 projects)
                                                        (gethash (file-name-as-directory
                                                                  (expand-file-name project))
                                                                 projects))))
                                   (gethash "mcpServers" entry)))))
        (when (hash-table-p table)
          (maphash (lambda (name spec)
                     (push (ygg-agent--mcp-spec name spec) out))
                   table)))
      (delq nil (nreverse out)))))

(defun ygg-agent--toml-strings (value)
  "The quoted strings in VALUE, a TOML scalar or inline array."
  (let ((out nil) (start 0))
    (while (string-match "\"\\([^\"]*\\)\"" value start)
      (push (match-string 1 value) out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun ygg-agent--toml-mcp (file)
  "The servers FILE declares, read as much TOML as this needs.
Only the `mcp_servers\=' tables: a section head, its scalars and its
env table.  Anything else in the file is somebody else\='s business."
  (when (file-readable-p (expand-file-name file))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file))
      (goto-char (point-min))
      (let ((servers (make-hash-table :test 'equal))
            (order nil)
            name mode)
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
            (cond
             ;; a section under mcp_servers names a server, and what
             ;; follows the name says which part of it: nothing is the
             ;; server, env is its environment, anything else — argent
             ;; keeps a table per tool down there — is not ours
             ((string-match "\\`\\[mcp_servers\\.\\([^]]+\\)\\]\\'" line)
              (let* ((parts (split-string (match-string 1 line) "\\." t))
                     (server (car parts))
                     (rest (cdr parts)))
                (setq name server
                      mode (cond ((null rest) 'server)
                                 ((equal rest '("env")) 'env)))
                (when (and (eq mode 'server) (not (gethash server servers)))
                  (puthash server (list :name server) servers)
                  (push server order))))
             ((string-prefix-p "[" line) (setq name nil mode nil))
             ((and name mode
                   (string-match "\\`\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*=[ \t]*\\(.*\\)\\'" line))
              (let* ((key (match-string 1 line))
                     (values (ygg-agent--toml-strings (match-string 2 line)))
                     (spec (gethash name servers)))
                (when spec
                  (puthash name
                           (pcase (cons mode key)
                             (`(env . ,_) (plist-put spec :env
                                                     (append (plist-get spec :env)
                                                             (list (list :name key
                                                                         :value (or (car values) ""))))))
                             ('(server . "command") (plist-put spec :command (car values)))
                             ('(server . "url") (plist-put spec :url (car values)))
                             ('(server . "args") (plist-put spec :args (vconcat values)))
                             (_ spec))
                           servers)))))
            (forward-line 1)))
        (delq nil
              (mapcar (lambda (n)
                        (let ((spec (gethash n servers)))
                          (cond ((plist-get spec :url) (plist-put spec :type "http"))
                                ((plist-get spec :command) spec))))
                      (nreverse order)))))))

;;;###autoload
(defun ygg-agent-user-mcp-servers (agent &optional project)
  "The MCP servers AGENT\='s own configuration declares, in wire shape.
A session gets what session/new carries and nothing else, so the
servers the CLI would have loaded from its own config have to be
handed over too — for whichever CLI is behind this session, since an
agent started from Emacs should reach what that agent reaches when it
is started by hand."
  (pcase agent
    ("claude" (ygg-agent--json-mcp "~/.claude.json" project))
    ("gemini" (ygg-agent--json-mcp "~/.gemini/settings.json" project))
    ("codex" (ygg-agent--toml-mcp "~/.codex/config.toml"))
    ((pred stringp)
     ;; anything else: the two places the others keep it
     (or (ygg-agent--json-mcp (format "~/.%s/settings.json" agent) project)
         (ygg-agent--json-mcp (format "~/.%s.json" agent) project)))))

(defcustom ygg-agent-instructions
  "## This editor (aob)

You are running inside Emacs. Plan and delegate with your own built-in
tools: your todo or plan tool, and your subagent tool (Task or Agent).
The editor mirrors both. Your plan becomes this session's tasks.md, and
each subagent gets a trace of its own. When the user changes that list,
the changes reach you as a note with the next message.

When you ask the user a question with choices (AskUserQuestion or a
form), keep the set open: the user can always answer in their own words,
so never phrase the options as exhaustive, and take a typed answer as
the answer.

Use the aob MCP server only for what your own tools cannot do:

- Code questions: xref_references and xref_apropos for who calls what,
  imenu_symbols for a file's shape, treesit_info for the parse,
  diagnostics for what a checker says about an open file.
- Other conversations: session_list gives every conversation open here
  with its id; session_say puts words into one; session_read shows the
  tail of one when you need to diagnose it. Address it by its id.
- tool_names lists everything the server offers."
  "What every session started from here is told about this editor.
It goes out in the request that opens the session, appended to the
agent's own system prompt, so a session hears it whichever config home
it runs under."
  :type 'string :group 'yggdrasil)

(defconst ygg-agent--instructions-open "<!-- aob: managed, edited by Emacs -->")
(defconst ygg-agent--instructions-close "<!-- /aob -->")

(defun ygg-agent-remove-instructions (home)
  "Take the fenced block an older setup wrote out of HOME\='s memory file.
The system prompt carries it now, and a second copy in the memory file
is the same words read twice in every turn."
  (let ((file (and home (expand-file-name "CLAUDE.md" home))))
    (when (and file (file-readable-p file))
      (let* ((old (with-temp-buffer (insert-file-contents file) (buffer-string)))
             (new (and (string-match (concat "\n*" (regexp-quote ygg-agent--instructions-open)
                                             "\\(?:.\\|\n\\)*?"
                                             (regexp-quote ygg-agent--instructions-close)
                                             "\n?")
                                     old)
                       (replace-match "\n" t t old))))
        (when (and new (not (equal old new)))
          (with-temp-file file (insert (string-trim-left new)))
          file)))))

(defun ygg-agent-write-instructions (home)
  "Put `ygg-agent-instructions\=' in HOME\='s memory file, and only that.
The block is fenced, so what you wrote around it stays yours and a
second run replaces ours rather than adding another."
  (when (and home (file-directory-p home))
    (let* ((file (expand-file-name "CLAUDE.md" home))
           (block (concat ygg-agent--instructions-open "\n"
                          ygg-agent-instructions "\n"
                          ygg-agent--instructions-close "\n"))
           (old (when (file-readable-p file)
                  (with-temp-buffer (insert-file-contents file) (buffer-string))))
           (new (cond
                 ((null old) block)
                 ((string-match (concat (regexp-quote ygg-agent--instructions-open)
                                        "\\(?:.\\|\n\\)*?"
                                        (regexp-quote ygg-agent--instructions-close)
                                        "\n?")
                                old)
                  (replace-match block t t old))
                 (t (concat old (if (string-suffix-p "\n" old) "" "\n") "\n" block)))))
      (unless (equal old new)
        (make-directory (file-name-directory file) t)
        (with-temp-file file (insert new)))
      file)))

(defconst ygg-agent--config-homes
  '(("claude" :var "CLAUDE_CONFIG_DIR" :marker ".claude-config-dir" :home "~/.claude"
     :share ("skills" "agents" "commands" "plugins" "hooks"
             "output-styles" "statusline-cache")
     ;; the plugins directory holds what is installed; which of them are on
     ;; is a settings key, so sharing the directory alone starts an agent
     ;; with every plugin off.  settings.json cannot be shared the way a
     ;; directory is — the CLI rewrites it by rename, which would leave a
     ;; copy where the symlink was — so these keys are seeded into whatever
     ;; the config home already has, and only when it lacks them.
     :settings "settings.json"
     :seed ("enabledPlugins" "extraKnownMarketplaces")
     :adopt-plugin-mcp t)
    ("codex" :var "CODEX_HOME" :marker ".codex-home" :home "~/.codex"
     :share ("agents" "prompts")))
  "Per-kind config-home spec: env var, marker file, real home, shared subdirs.")

(defcustom ygg-agent-conf-root "~/.agents-conf"
  "Root for fallback per-project agent config homes; nil disables the fallback."
  :type '(choice directory (const nil)) :group 'yggdrasil)

(defun ygg-agent--kind (preset cmd)
  "The config-home kind PRESET or CMD belongs to, or nil.
An ACP agent named after its kind with a suffix, claude-isolated, or a
command naming the kind's adapter, still reads that kind's home."
  (let ((exe (file-name-nondirectory (car (split-string cmd)))))
    (cond
     ((assoc preset ygg-agent--config-homes) preset)
     ((assoc exe ygg-agent--config-homes) exe)
     ((seq-find (lambda (kind)
                  (or (string-prefix-p (concat kind "-") preset)
                      (string-match-p (regexp-quote kind) cmd)))
                (mapcar #'car ygg-agent--config-homes))))))

(defun ygg-agent--repo-home (project)
  "Main repo root of PROJECT (worktrees resolve to the primary checkout)."
  (or (when-let* ((common (ignore-errors
                            (car (process-lines "git" "-C" project "rev-parse"
                                                "--path-format=absolute"
                                                "--git-common-dir")))))
        (and (not (string-empty-p common))
             (file-name-directory (directory-file-name common))))
      project))

(defun ygg-agent--read-marker (path)
  (when (file-regular-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (let ((line (string-trim (buffer-substring (point-min) (line-end-position)))))
        (unless (string-empty-p line)
          (expand-file-name line (file-name-directory path)))))))

(defun ygg-agent--read-json (path)
  "PATH as a hash table, or nil when it is missing or not JSON."
  (when (file-readable-p path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents path)
        (json-parse-buffer :object-type 'hash-table
                           :null-object nil :false-object :false)))))

(defun ygg-agent--seed-settings (spec dir)
  "Give DIR's settings file what SPEC's real home has and it lacks.
Entry by entry, not key by key: a config home that once turned one
plugin on has said something about that plugin and nothing about the
others, so seeding the whole object only where it is absent leaves every
home the CLI ever wrote to stuck with whatever it decided that day.  An
explicit answer here always wins — this only fills silence."
  (when-let* ((name (plist-get spec :settings))
              (keys (plist-get spec :seed))
              (src (expand-file-name name (expand-file-name (plist-get spec :home))))
              (global (ygg-agent--read-json src)))
    (let* ((dest (expand-file-name name dir))
           (local (or (ygg-agent--read-json dest) (make-hash-table :test #'equal)))
           (added nil))
      (dolist (key keys)
        (let ((have (gethash key local 'missing))
              (want (gethash key global 'missing)))
          (cond
           ((eq want 'missing))
           ((eq have 'missing) (puthash key want local) (push key added))
           ((and (hash-table-p want) (hash-table-p have))
            (let ((n 0))
              (maphash (lambda (id value)
                         (when (eq 'missing (gethash id have 'missing))
                           (puthash id value have)
                           (setq n (1+ n))))
                       want)
              (when (> n 0) (push (format "%s+%d" key n) added)))))))
      (when added
        (ignore-errors
          (with-temp-file dest
            (insert (json-serialize local :null-object nil :false-object :false)
                    "\n")))
        added))))

(defun ygg-agent--write-json (path table)
  "Write TABLE to PATH as JSON, utf-8, no questions.
The coding system is pinned because these files carry whatever the CLI
put in them — a prompt here would hang a launch behind a question nobody
is looking at."
  (ignore-errors
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file path
        (insert (json-serialize table :null-object nil :false-object :false) "\n")))
    t))

(defun ygg-agent--adopt-plugin-mcp (spec dir)
  "Copy the plugin-defined MCP servers into DIR as user-scoped ones.
A plugin-scoped server is invisible to any agent started through the
SDK — every ACP session — while the same URL configured in the home is
not.  Written straight into the home's config: `claude mcp add' does
exactly this and costs a process per launch."
  (when (plist-get spec :adopt-plugin-mcp)
    (when-let* ((servers (ygg-agent--plugin-mcp-servers))
                (path (expand-file-name ".claude.json" dir)))
      (let* ((conf (or (ygg-agent--read-json path) (make-hash-table :test #'equal)))
             (mcp (or (gethash "mcpServers" conf) (make-hash-table :test #'equal)))
             added)
        (pcase-dolist (`(,name . ,url) servers)
          (let ((key (format "%s-mcp" name)))
            (when (eq 'missing (gethash key mcp 'missing))
              (let ((entry (make-hash-table :test #'equal)))
                (puthash "type" "http" entry)
                (puthash "url" url entry)
                (puthash key entry mcp))
              (push key added))))
        (when added
          (puthash "mcpServers" mcp conf)
          (and (ygg-agent--write-json path conf) (nreverse added)))))))

(defconst ygg-agent--credential-service "Claude Code-credentials"
  "Keychain service Claude Code keeps a config home's tokens under.
The default home uses the bare name; every other is suffixed with the
first eight hex of the sha256 of its path.")

(defun ygg-agent--credential-name (dir)
  "Keychain service name holding DIR's credentials.
The home the CLI uses when nothing names one keeps the bare service."
  (let ((path (directory-file-name (expand-file-name dir))))
    (if (equal path (directory-file-name (expand-file-name "~/.claude")))
        ygg-agent--credential-service
      (format "%s-%s" ygg-agent--credential-service
              (substring (secure-hash 'sha256 path) 0 8)))))

(defun ygg-agent--keychain-read (service)
  "SERVICE's secret parsed as JSON, or nil."
  (let ((out (with-output-to-string
               (with-current-buffer standard-output
                 (ignore-errors
                   (call-process "security" nil t nil
                                 "find-generic-password" "-s" service "-w"))))))
    (ignore-errors
      (json-parse-string (string-trim out) :object-type 'hash-table
                         :null-object nil :false-object :false))))

(defun ygg-agent--keychain-account (service)
  "The account attribute SERVICE's item was stored under."
  (let ((out (with-output-to-string
               (with-current-buffer standard-output
                 (ignore-errors
                   (call-process "security" nil t nil
                                 "find-generic-password" "-s" service))))))
    (when (string-match "\"acct\"<blob>=\"\\([^\"]*\\)\"" out)
      (match-string 1 out))))

(defun ygg-agent--share-mcp-auth (dir)
  "Give DIR's keychain item the MCP logins other config homes already hold.
Claude Code keys its credential store on the config home's path, so an
OAuth login done in one repository is anonymous in the next — the same
account, a different keyring entry, and a server that answers with no
tools.  Only `mcpOAuth' travels: the account token stays each home's own."
  (when (eq system-type 'darwin)
    (let* ((service (ygg-agent--credential-name dir))
           (target (ygg-agent--keychain-read service)))
      ;; no item yet means the home has never authenticated anything; writing
      ;; one would invent an account token, so leave it to the CLI
      (when target
        (let ((have (or (gethash "mcpOAuth" target) (make-hash-table :test #'equal)))
              (pool (make-hash-table :test #'equal))
              added)
          (dolist (other (cons ygg-agent--credential-service
                               (ygg-agent--credential-services)))
            (unless (equal other service)
              (when-let* ((json (ygg-agent--keychain-read other))
                          (mcp (gethash "mcpOAuth" json)))
                (maphash (lambda (k v)
                           (let ((seen (gethash k pool)))
                             (when (or (null seen)
                                       (> (or (gethash "expiresAt" v) 0)
                                          (or (gethash "expiresAt" seen) 0)))
                               (puthash k v pool))))
                         mcp))))
          (maphash (lambda (k v)
                     (when (eq 'missing (gethash k have 'missing))
                       (puthash k v have)
                       (push k added)))
                   pool)
          (when added
            (puthash "mcpOAuth" have target)
            (when-let* ((acct (ygg-agent--keychain-account service)))
              (ignore-errors
                (call-process "security" nil nil nil
                              "add-generic-password" "-U" "-a" acct
                              "-s" service "-w"
                              (json-serialize target :null-object nil
                                              :false-object :false)))
              (nreverse added))))))))

(defun ygg-agent--credential-services ()
  "Every Claude Code credential item this login keychain holds."
  (let ((out (with-output-to-string
               (with-current-buffer standard-output
                 (ignore-errors
                   (call-process "security" nil t nil "dump-keychain")))))
        names)
    (let ((start 0))
      (while (string-match "\"svce\"<blob>=\"\\(Claude Code-credentials[^\"]*\\)\""
                           out start)
        (push (match-string 1 out) names)
        (setq start (match-end 0))))
    (seq-uniq names)))

(defvar ygg-agent--login-cache (make-hash-table :test #'equal)
  "Config home to whether its keychain item still holds an account token.")

(defun ygg-agent--logged-in-p (kind dir &optional cached)
  "Non-nil when a KIND agent started in DIR can reach an account.
Claude Code keys its credentials on the config home\='s path, so a home
that has never been logged in — or whose refresh has since been revoked
— is left holding MCP logins and no account token, and every prompt
sent there comes back as \"Authentication required\" before a line is
read.  CACHED answers from what is already known and never goes near
the keychain, for a picker that has not been answered yet."
  (cond
   ((not (and (eq system-type 'darwin) (equal kind "claude"))) t)
   (cached (let ((known (gethash dir ygg-agent--login-cache 'unknown)))
             (if (eq known 'unknown) t known)))
   (t (puthash dir
               (and (when-let* ((json (ygg-agent--keychain-read
                                       (ygg-agent--credential-name dir))))
                      (gethash "claudeAiOauth" json))
                    t)
               ygg-agent--login-cache))))

(defun ygg-agent--authenticated-home (kind spec homes &optional cached)
  "The first of HOMES with a login left, else the home the CLI itself uses.
A private home is worth having only once someone has logged into it;
until then the shared home is the one place a login is certain to be,
and a session that runs is worth more than a home nothing writes to."
  (or (seq-find (lambda (dir) (ygg-agent--logged-in-p kind dir cached)) homes)
      (let ((home (expand-file-name (plist-get spec :home))))
        (if (ygg-agent--logged-in-p kind home cached) home (car homes)))))

(defun ygg-agent--bootstrap-share (spec dir)
  (make-directory dir t)
  (let ((home (expand-file-name (plist-get spec :home))))
    (dolist (sub (plist-get spec :share))
      (let ((link (expand-file-name sub dir))
            (src (expand-file-name sub home)))
        (when (and (not (file-attributes link)) (file-exists-p src))
          (ignore-errors (make-symbolic-link src link))))))
  (ygg-agent--seed-settings spec dir)
  (ygg-agent--adopt-plugin-mcp spec dir)
  (ygg-agent--share-mcp-auth dir))

(defun ygg-agent--dangling-shares (spec)
  "Entries in SPEC's real shared directories that point nowhere.
Every config home links to these, so one dead entry is dead in every
agent: a skill pinned to a plugin version that has since updated leaves
its name in the listing with nothing behind it, and the agent reaches
for a neighbour instead."
  (let ((home (expand-file-name (plist-get spec :home)))
        dead)
    (dolist (sub (plist-get spec :share))
      (let ((dir (expand-file-name sub home)))
        (when (file-directory-p dir)
          (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
            (when (and (file-symlink-p entry) (not (file-exists-p entry)))
              (push (format "%s/%s" sub (file-name-nondirectory entry)) dead))))))
    (nreverse dead)))

(defun ygg-agent--reclaim-share (spec dir)
  "Turn DIR's private copies of SPEC's shared subdirs back into links.
A home made before a share existed has a real directory where the link
belongs, and the CLI fills it with a second, private set of installs —
so the plugins you enabled globally are not merely off there, they are
absent.  The old directory is kept beside the link, never deleted."
  (let ((home (expand-file-name (plist-get spec :home)))
        (stamp (format-time-string "%Y%m%d%H%M%S"))
        moved)
    (dolist (sub (plist-get spec :share))
      (let ((link (expand-file-name sub dir))
            (src (expand-file-name sub home)))
        (when (and (file-directory-p src)
                   (file-directory-p link)
                   (not (file-symlink-p link)))
          (let ((aside (format "%s.private-%s" link stamp)))
            (rename-file link aside)
            (make-symbolic-link src link)
            (push sub moved)))))
    (nreverse moved)))

;;;###autoload
(defun ygg-agent-refresh-config-homes ()
  "Re-run the share bootstrap over every per-project agent config home.
Homes made before a share or a seed was added never get it otherwise —
they are only built once, when the project first launches an agent."
  (interactive)
  (clrhash ygg-agent--login-cache)
  (let ((root (and ygg-agent-conf-root (expand-file-name ygg-agent-conf-root)))
        (touched 0) (seeded nil) (reclaimed nil) (anonymous nil))
    (unless (and root (file-directory-p root))
      (user-error "No agent config root to refresh"))
    ;; no dot rule here: a home is named after its repository, and .emacs.d
    ;; is a repository like any other
    (dolist (project (directory-files root t directory-files-no-dot-files-regexp))
      (when (file-directory-p project)
        (pcase-dolist (`(,kind . ,spec) ygg-agent--config-homes)
          (let ((home (expand-file-name kind project)))
            (when (file-directory-p home)
              (setq touched (1+ touched))
              (let ((label (format "%s/%s" (file-name-nondirectory project) kind)))
                (when-let* ((back (ygg-agent--reclaim-share spec home)))
                  (push (format "%s [%s]" label (string-join back " ")) reclaimed))
                (when (ygg-agent--seed-settings spec home)
                  (push label seeded))
                (unless (ygg-agent--logged-in-p kind home)
                  (push label anonymous)))
              (ygg-agent--bootstrap-share spec home))))))
    (let ((dead (seq-mapcat (lambda (cell) (ygg-agent--dangling-shares (cdr cell)))
                            ygg-agent--config-homes)))
      (message "agent config homes: %d checked, %d seeded%s, %d reclaimed%s%s%s"
               touched (length seeded)
               (if seeded (format " (%s)" (string-join (nreverse seeded) ", ")) "")
               (length reclaimed)
               (if reclaimed
                   (format " (%s)" (string-join (nreverse reclaimed) ", ")) "")
               (if anonymous
                   (format "  ·  %d never logged in, on the shared home: %s"
                           (length anonymous)
                           (string-join (nreverse anonymous) " "))
                 "")
               (if dead
                   (format "  ·  %d dead shared link(s): %s"
                           (length dead) (string-join dead " "))
                 "")))))

(defun ygg-agent--own-home (kind repo &optional isolate)
  "The home under `ygg-agent-conf-root\=' REPO would keep for KIND.
ISOLATE names a worker whose home sits beside the repository\='s own."
  (expand-file-name
   (format "%s%s/%s"
           (file-name-nondirectory (directory-file-name repo))
           (if isolate
               (concat "@" (replace-regexp-in-string
                            "[^A-Za-z0-9_-]" "-" (format "%s" isolate)))
             "")
           kind)
   ygg-agent-conf-root))

(defun ygg-agent--config-dir (kind spec project &optional peek isolate)
  "Config home for KIND in PROJECT, made ready unless PEEK.
Naming a path is not asking for one: the task panels show the config
home of whatever repository the cursor is on, and provisioning one per
row scrolled past would fill the conf root with repositories you only
looked at.

ISOLATE names a worker that is to have a home nothing else writes to.
It lands beside the project\='s own, under the same root and with the
kind at the leaf, so what seeds a shared home seeds this one.  A marker
file is passed over for it: a marker names the home the repository
chose to share.

Nothing passes it today.  A home per worker is a worker that has never
logged in — the share seeds plugins and prompts, not credentials — so
workers take the project\='s home, the same one SPC a s and SPC a a give
their agents, and isolation stops at the process.  This stays for the
day a login can be seeded too."
  (let ((marker (plist-get spec :marker))
        (repo (ygg-agent--repo-home project)))
    (or (and (not isolate)
             (or (ygg-agent--read-marker (expand-file-name marker project))
                 (ygg-agent--read-marker (expand-file-name marker repo))))
        (when ygg-agent-conf-root
          (let* ((own (ygg-agent--own-home kind repo isolate))
                 (shared (and isolate (ygg-agent--own-home kind repo)))
                 (dir (ygg-agent--authenticated-home
                       kind spec (delq nil (list own shared)) peek)))
            (unless (or peek
                        (equal (directory-file-name dir)
                               (directory-file-name
                                (expand-file-name (plist-get spec :home)))))
              (ygg-agent--bootstrap-share spec dir))
            dir)))))

(defun ygg-agent--config-label (preset cmd project)
  "Abbreviated fallback config home for PRESET/CMD in PROJECT, or nil.
A home a marker file names is the project's own and needs no saying; a
home under `ygg-agent-conf-root' is one this config chose, and the owner
launching an agent is owed which one it is.  Only the name is read: the
spawn provisions it, and a picker that has not been answered yet must
not go near the keychain."
  (condition-case nil
      (when-let* ((root (and ygg-agent-conf-root
                             (file-name-as-directory
                              (expand-file-name ygg-agent-conf-root))))
                  (kind (ygg-agent--kind preset cmd))
                  (spec (cdr (assoc kind ygg-agent--config-homes)))
                  (dir (ygg-agent--config-dir kind spec project 'peek))
                  ((or (string-prefix-p root (expand-file-name dir))
                       (equal (directory-file-name (expand-file-name dir))
                              (directory-file-name
                               (expand-file-name (plist-get spec :home)))))))
        (abbreviate-file-name (directory-file-name dir)))
    (error nil)))

(defun ygg-agent--config-env (preset cmd project &optional isolate)
  "Return \"VAR=DIR\" for PRESET/CMD in PROJECT, or nil; never signals.
ISOLATE, when given, names a worker whose home is its own.

The shared home answers nil, and not its own path: the CLI keys its
credentials on the variable, so naming the default home is not the same
as leaving the variable alone — it is a home of that name nobody has
ever logged into."
  (condition-case nil
      (when-let* ((kind (ygg-agent--kind preset cmd))
                  (spec (cdr (assoc kind ygg-agent--config-homes)))
                  (dir (ygg-agent--config-dir kind spec project nil isolate))
                  ((not (equal (directory-file-name dir)
                               (directory-file-name
                                (expand-file-name (plist-get spec :home)))))))
        (format "%s=%s" (plist-get spec :var) (directory-file-name dir)))
    (error nil)))

(declare-function project-root "project" (project))
(declare-function make-term "term" (name program &optional startfile &rest switches))
(declare-function term-mode "term" ())
(declare-function term-char-mode "term" ())

;;;###autoload
(defun ygg-agent-login-config-home (project)
  "Log PROJECT\='s own config home in, instead of borrowing the shared one.
The CLI keys its credentials on the home it is pointed at, so a home no
one has logged into answers every prompt with \"Authentication
required\".  Such a home is passed over when a session starts — this is
where it earns its place back."
  (interactive
   (list (read-directory-name
          "Log in the config home of: "
          (or (when (fboundp 'project-current)
                (when-let* ((pr (project-current nil)))
                  (project-root pr)))
              default-directory))))
  (unless ygg-agent-conf-root (user-error "No agent config root"))
  (require 'term)
  (let* ((kind "claude")
         (spec (cdr (assoc kind ygg-agent--config-homes)))
         (home (ygg-agent--own-home kind (ygg-agent--repo-home
                                          (expand-file-name project))))
         (exe (or (executable-find "claude") "claude")))
    (ygg-agent--bootstrap-share spec home)
    (remhash home ygg-agent--login-cache)
    (let ((process-environment
           (cons (format "%s=%s" (plist-get spec :var) (directory-file-name home))
                 process-environment))
          (default-directory (file-name-as-directory (expand-file-name project))))
      (pop-to-buffer
       (save-window-excursion
         (with-current-buffer
             (make-term (format "login %s" (abbreviate-file-name home))
                        exe nil "auth" "login")
           (term-mode)
           (term-char-mode)
           (current-buffer)))))))

(provide 'ygg-agent-conf)
