;;; ygg-agent-conf.el --- per-project agent config homes, lifted out of layer-agent -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'subr-x)

(defun ygg-agent--plugins-root (spec)
  "The directory SPEC's CLI keeps its plugin cache and install record in."
  (let ((env (getenv "CLAUDE_CODE_PLUGIN_CACHE_DIR")))
    (if (and env (not (string-empty-p env)))
        (expand-file-name env)
      (expand-file-name "plugins" (expand-file-name (plist-get spec :home))))))

(defun ygg-agent--mcp-file-urls (file)
  "Remote servers the plugin server file FILE declares, as (NAME . URL).
The file may wrap its servers in mcpServers or list them bare."
  (when-let* ((json (ygg-agent--read-json file)))
    (let ((inner (gethash "mcpServers" json))
          out)
      (maphash (lambda (name spec)
                 (when-let* (((hash-table-p spec))
                             (url (gethash "url" spec))
                             ((stringp url)))
                   (push (cons name url) out)))
               (if (hash-table-p inner) inner json))
      (nreverse out))))

(defun ygg-agent--plugin-caches (root)
  "Every plugin cached under ROOT, as (ID . VERSION-DIRS).
ID is plugin@marketplace, the form enabledPlugins keys take."
  (let ((cache (expand-file-name "cache" root))
        out)
    (dolist (market (and (file-directory-p cache)
                         (directory-files cache t directory-files-no-dot-files-regexp)))
      (when (file-directory-p market)
        (dolist (plugin (directory-files market t directory-files-no-dot-files-regexp))
          (when (file-directory-p plugin)
            (push (cons (format "%s@%s" (file-name-nondirectory plugin)
                                (file-name-nondirectory market))
                        (seq-filter #'file-directory-p
                                    (directory-files plugin t
                                                     directory-files-no-dot-files-regexp)))
                  out)))))
    (nreverse out)))

(defun ygg-agent--plugin-installs (root)
  "Version directory names ROOT's install record gives each plugin id.
A record is taken alone or from a list, the user-scoped one first; its
installPath names the directory, its version is the fallback."
  (let ((table (make-hash-table :test #'equal)))
    (when-let* ((json (ygg-agent--read-json (expand-file-name "installed_plugins.json" root)))
                (plugins (let ((inner (gethash "plugins" json)))
                           (if (hash-table-p inner) inner json))))
      (maphash
       (lambda (id value)
         (let* ((records (seq-filter #'hash-table-p
                                     (if (hash-table-p value) (list value) (append value nil))))
                (record (or (seq-find (lambda (r) (equal (gethash "scope" r) "user")) records)
                            (car records))))
           (when record
             (puthash id
                      (delq nil
                            (list (when-let* ((path (gethash "installPath" record))
                                              ((stringp path)))
                                    (file-name-nondirectory (directory-file-name path)))
                                  (let ((version (gethash "version" record)))
                                    (and (stringp version) version))))
                      table))))
       plugins))
    table))

(defun ygg-agent--plugin-current-dir (dirs recorded)
  "The one of DIRS a plugin is on: the name RECORDED says, else the newest.
A directory the CLI marked orphaned is an old version waiting to go."
  (let ((live (seq-remove (lambda (dir) (file-exists-p (expand-file-name ".orphaned_at" dir)))
                          dirs)))
    (or (seq-find (lambda (dir) (member (file-name-nondirectory dir) recorded)) live)
        (car (if (seq-every-p (lambda (dir)
                                (ignore-errors (version-to-list (file-name-nondirectory dir))))
                              live)
                 (sort live (lambda (a b)
                              (version< (file-name-nondirectory b)
                                        (file-name-nondirectory a))))
               (sort live (lambda (a b)
                            (time-less-p (file-attribute-modification-time (file-attributes b))
                                         (file-attribute-modification-time (file-attributes a))))))))))

(defun ygg-agent--enabled-plugins (spec dir)
  "DIR\\='s enabledPlugins table, empty when unset.
Nil when the settings file cannot be read."
  (when-let* ((name (plist-get spec :settings))
              (json (ygg-agent--read-json (expand-file-name name dir))))
    (let ((table (gethash "enabledPlugins" json)))
      (if (hash-table-p table) table (make-hash-table :test #'equal)))))

(defun ygg-agent--plugin-mcp-servers (root enabled)
  "Remote MCP servers of the plugins ENABLED turns on, as (NAME . URL).
Read from the plugin cache under ROOT, at each plugin\\='s current version
only: these are the servers a plugin brings, which is not the same set
the CLI will hand to a session."
  (let ((installs (ygg-agent--plugin-installs root))
        out)
    (pcase-dolist (`(,id . ,dirs) (ygg-agent--plugin-caches root))
      (when-let* (((eq t (gethash id enabled)))
                  (current (ygg-agent--plugin-current-dir dirs (gethash id installs))))
        (setq out (append out (ygg-agent--mcp-file-urls
                               (expand-file-name ".mcp.json" current))))))
    out))

;;;###autoload

(defun ygg-agent-mcp-value (value)
  "VALUE as a header or env string, or nil when it says nothing."
  (cond ((memq value '(nil :null :false :json-false)) nil)
        ((eq value t) "true")
        ((stringp value) value)
        ((or (hash-table-p value) (vectorp value) (consp value))
         (condition-case nil
             (json-serialize value :null-object nil :false-object :false)
           (error (format "%s" value))))
        (t (format "%s" value))))

(defun ygg-agent--mcp-pairs (table)
  (let (out)
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (when-let* ((text (ygg-agent-mcp-value value)))
                   (push (list :name key :value text) out)))
               table))
    (nreverse out)))

(defun ygg-agent--mcp-spec (name spec)
  "One server SPEC from a CLI config, in the shape a session is handed."
  (when (hash-table-p spec)
    (let ((url (gethash "url" spec))
          (type (or (gethash "type" spec) "stdio"))
          (command (gethash "command" spec)))
      (cond
       (url (list :name name :type (if (equal type "sse") "sse" "http") :url url
                  :headers (ygg-agent--mcp-pairs (gethash "headers" spec))))
       (command
        (list :name name :command command
              :args (vconcat (gethash "args" spec))
              :env (ygg-agent--mcp-pairs (gethash "env" spec))))))))

(defun ygg-agent--json-mcp (file &optional project jsonc)
  "The servers FILE declares, and the ones it declares for PROJECT.
JSONC lets FILE carry comments."
  (when-let* ((json (ygg-agent--read-json-or-warn
                     (expand-file-name file) 'user-mcp jsonc)))
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

(defvar ygg-agent--config-homes)

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
    ("gemini" (ygg-agent--json-mcp "~/.gemini/settings.json" project t))
    ("codex" (ygg-agent--toml-mcp "~/.codex/config.toml"))
    ("pi" (ygg-agent--json-mcp
           (expand-file-name "mcp.json"
                             (or (ignore-errors
                                   (ygg-agent--known-config-dir
                                    "pi" (cdr (assoc "pi" ygg-agent--config-homes))
                                    (or project default-directory)))
                                 "~/.pi/agent"))
           project))
    ((pred stringp)
     ;; anything else: the two places the others keep it
     (or (ygg-agent--json-mcp (format "~/.%s/settings.json" agent) project)
         (ygg-agent--json-mcp (format "~/.%s.json" agent) project)))))

(defcustom ygg-agent-instructions
  "## This editor (aob)

You run inside Emacs. Plan and delegate with your own todo or plan tool
and subagent tool (Task or Agent). The editor mirrors both: your plan
becomes this session's tasks.md, each subagent gets its own trace, and
the user's edits to the list reach you as a note with the next message.

When you ask the user a question with choices (AskUserQuestion or a
form), keep the set open: they can always answer in their own words, so
never phrase the options as exhaustive, and take a typed answer as the
answer.

Read big files (over ~400 lines) in line ranges: find the spot with grep -n
first. Don't re-read a file you already read unless it changed.

Use the aob MCP server only for what your own tools cannot do: code
references, symbol outlines, tree-sitter parses, diagnostics, and other
conversations open in this editor."
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
     ;; the config home already has, entry by entry, only where it lacks them.
     :settings "settings.json"
     :seed ("enabledPlugins" "extraKnownMarketplaces" "skillOverrides")
     :adopt-plugin-mcp t)
    ("codex" :var "CODEX_HOME" :marker ".codex-home" :home "~/.codex"
     :share ("agents" "prompts"))
    ("pi" :var "PI_CODING_AGENT_DIR" :marker ".pi-agent-dir" :home "~/.pi/agent"
     :share ("skills" "prompts" "npm" "extensions" "themes")
     :settings "settings.json"
     :seed ("packages" "defaultProvider" "defaultModel")
     :seed-files (("models.json" "providers") ("mcp.json" "mcpServers"))))
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
                      (and (not (equal kind "pi"))
                           (string-match-p (regexp-quote kind) cmd))))
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

(defun ygg-agent--strip-jsonc ()
  "Delete the // and /* */ comments from the buffer, leaving strings alone."
  (goto-char (point-min))
  (while (re-search-forward "\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\|//.*\\|/\\*\\(?:.\\|\n\\)*?\\*/" nil t)
    (unless (eq (char-after (match-beginning 0)) ?\")
      (replace-match ""))))

(defun ygg-agent--read-json (path &optional jsonc)
  "PATH as a hash table, or nil when it is missing, not JSON or not an object.
JSONC lets the file carry comments."
  (when (file-readable-p path)
    (let ((json (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents path)
                    (when jsonc (ygg-agent--strip-jsonc) (goto-char (point-min)))
                    (json-parse-buffer :object-type 'hash-table
                                       :null-object nil :false-object :false)))))
      (and (hash-table-p json) json))))

(defvar ygg-agent--warned-json (make-hash-table :test #'equal))

(defun ygg-agent--json-problem (path)
  (cond ((file-directory-p path) "a directory")
        ((not (file-readable-p path)) "unreadable")
        ((string-blank-p (or (ignore-errors
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (buffer-string)))
                             "x"))
         "empty")
        (t "not a JSON object")))

(defun ygg-agent--read-json-or-warn (path &optional context jsonc)
  "PATH as a hash table, or nil; never signals.
Warns once per PATH and modification time when it exists but cannot be
used.  CONTEXT is `user-mcp' for a file read only for its MCP servers.
JSONC lets the file carry comments."
  (when (file-exists-p path)
    (or (ygg-agent--read-json path jsonc)
        (let ((mtime (file-attribute-modification-time (file-attributes path))))
          (unless (equal (gethash path ygg-agent--warned-json) mtime)
            (puthash path mtime ygg-agent--warned-json)
            (display-warning
             'ygg-agent
             (format "%s is %s; %s" path (ygg-agent--json-problem path)
                     (if (eq context 'user-mcp)
                         "its MCP servers are skipped"
                       "skipped; the project home is still used"))
             :warning))
          nil))))

(defun ygg-agent--seed-file (spec dir name keys)
  "Give DIR's file NAME what SPEC's real home has to say under KEYS.
Entry by entry, not key by key: a config home that once turned one
plugin on has said something about that plugin and nothing about the
others, so seeding the whole object only where it is absent leaves every
home the CLI ever wrote to stuck with whatever it decided that day.  An
explicit answer here always wins — a project keeps the plugins it turned
on and the skills it switched off — and this only fills silence, so a
later change to an entry the real home already gave does not reach a
seeded copy.
Returns what changed, nil when nothing did and nothing was written."
  (when-let* ((src (expand-file-name name (expand-file-name (plist-get spec :home))))
              (global (ygg-agent--read-json-or-warn src)))
    (let* ((dest (expand-file-name name dir))
           (local (if (file-exists-p dest)
                      (ygg-agent--read-json-or-warn dest)
                    (make-hash-table :test #'equal)))
           (changed nil))
      (when local
        (dolist (key keys)
          (let ((have (gethash key local 'missing))
                (want (gethash key global 'missing)))
            (cond
             ((eq want 'missing))
             ((eq have 'missing) (puthash key want local) (push key changed))
             ((and (hash-table-p want) (hash-table-p have))
              (let ((n 0))
                (maphash (lambda (id value)
                           (when (eq 'missing (gethash id have 'missing))
                             (puthash id value have)
                             (setq n (1+ n))))
                         want)
                (when (> n 0)
                  (push (format "%s+%d" key n) changed)))))))
        (when (and changed (ygg-agent--replace-json dest local))
          (nreverse changed))))))

(defun ygg-agent--seed-settings (spec dir)
  "Seed DIR from SPEC's real home: its settings keys and its extra files.
Returns what changed, nil when nothing did."
  (let (changed)
    (when-let* ((name (plist-get spec :settings)))
      (setq changed (ygg-agent--seed-file spec dir name (plist-get spec :seed))))
    (pcase-dolist (`(,name . ,keys) (plist-get spec :seed-files))
      (setq changed (append changed (ygg-agent--seed-file spec dir name keys))))
    changed))

(defun ygg-agent--replace-json (path table)
  "Swap TABLE in for PATH, pretty-printed, by rename.
The CLI reads and rewrites this file on its own; a rename leaves it
either the old file or the new one, never half of each."
  (when-let* ((tmp (ignore-errors
                     (make-temp-file (expand-file-name
                                      (concat "." (file-name-nondirectory path) ".ygg-")
                                      (file-name-directory path))))))
    (condition-case nil
        (let ((coding-system-for-write 'utf-8-unix)
              (modes (file-modes path)))
          (with-temp-file tmp
            ;; json-serialize hands back UTF-8 bytes, which the pretty printer escapes
            (json-insert table :null-object nil :false-object :false)
            (json-pretty-print-buffer)
            (goto-char (point-max))
            (insert "\n"))
          (when modes (set-file-modes tmp modes))
          (rename-file tmp path t)
          t)
      (error (ignore-errors (delete-file tmp)) nil))))

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

(defconst ygg-agent--adopted-mcp-file ".ygg-adopted-mcp.json"
  "Ledger in a config home of the MCP entries adoption wrote, name to URL.")

(defun ygg-agent--adopt-plugin-mcp (spec dir)
  "Copy the MCP servers of DIR\\='s enabled plugins into DIR as user-scoped ones.
A plugin-scoped server is invisible to any agent started through the
SDK — every ACP session — while the same URL configured in the home is
not.  Written straight into the home\\='s config: claude mcp add does
exactly this and costs a process per launch.

Every server costs every session its tool list, so only a plugin DIR
turns on counts, at its current version, and never a URL the home
already has.  An entry adopted before that no longer qualifies goes;
adopted means named in the ledger beside the config, or, for one
written before the ledger, named and pointed exactly as a cached plugin
server would have been.  Settings or a config that cannot be read
change nothing."
  (when-let* (((plist-get spec :adopt-plugin-mcp))
              (enabled (ygg-agent--enabled-plugins spec dir))
              (path (expand-file-name ".claude.json" dir))
              (conf (if (file-exists-p path)
                        (ygg-agent--read-json path)
                      (make-hash-table :test #'equal))))
    (let* ((root (ygg-agent--plugins-root spec))
           (ledger-path (expand-file-name ygg-agent--adopted-mcp-file dir))
           (ledger (or (ygg-agent--read-json ledger-path) (make-hash-table :test #'equal)))
           (mcp (let ((table (gethash "mcpServers" conf)))
                  (if (hash-table-p table) table (make-hash-table :test #'equal))))
           (cached (seq-mapcat
                    (lambda (cell)
                      (seq-mapcat (lambda (version)
                                    (ygg-agent--mcp-file-urls
                                     (expand-file-name ".mcp.json" version)))
                                  (cdr cell)))
                    (ygg-agent--plugin-caches root)))
           (fresh (make-hash-table :test #'equal))
           ours own-urls want added removed)
      (maphash (lambda (key entry)
                 (let ((url (and (hash-table-p entry) (gethash "url" entry))))
                   (cond
                    ((and (stringp url)
                          (or (equal url (gethash key ledger))
                              (and (string-suffix-p "-mcp" key)
                                   (member (cons (string-remove-suffix "-mcp" key) url)
                                           cached))))
                     (push (cons key url) ours))
                    ((stringp url) (push url own-urls)))))
               mcp)
      (pcase-dolist (`(,name . ,url) (ygg-agent--plugin-mcp-servers root enabled))
        (unless (or (member url own-urls) (rassoc url want))
          (push (cons (format "%s-mcp" name) url) want)))
      (setq want (nreverse want))
      (pcase-dolist (`(,key . ,url) ours)
        (if (member (cons key url) want)
            (puthash key url fresh)
          (remhash key mcp)
          (push key removed)))
      (pcase-dolist (`(,key . ,url) want)
        (when (eq 'missing (gethash key mcp 'missing))
          (let ((entry (make-hash-table :test #'equal)))
            (puthash "type" "http" entry)
            (puthash "url" url entry)
            (puthash key entry mcp))
          (puthash key url fresh)
          (push key added)))
      (when (or added removed)
        (puthash "mcpServers" mcp conf)
        (ygg-agent--write-json path conf))
      (unless (and (= (hash-table-count fresh) (hash-table-count ledger))
                   (seq-every-p (lambda (key) (equal (gethash key fresh)
                                                     (gethash key ledger)))
                                (hash-table-keys fresh)))
        (ygg-agent--write-json ledger-path fresh))
      (nreverse added))))

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

(defvar ygg-agent--config-dirs (make-hash-table :test #'equal)
  "Kind, project, isolate and conf root to the config home last named for them.
Each value is (DIR REPO TIMES): the home, the repository it was read
from, and the modification times of the marker files it was read under.")

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
   (t (let ((known (gethash dir ygg-agent--login-cache t))
              (now (and (when-let* ((json (ygg-agent--keychain-read
                                           (ygg-agent--credential-name dir))))
                          (gethash "claudeAiOauth" json))
                        t)))
          (unless (eq known now) (clrhash ygg-agent--config-dirs))
          (puthash dir now ygg-agent--login-cache)))))

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
  (clrhash ygg-agent--config-dirs)
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
  (let* ((marker (plist-get spec :marker))
         (repo (ygg-agent--repo-home project))
         (times (ygg-agent--marker-times marker project repo))
         (dir (or (and (not isolate)
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
    (puthash (list kind project isolate ygg-agent-conf-root)
             (list dir repo times) ygg-agent--config-dirs)
    dir))

(defun ygg-agent--marker-times (marker project repo)
  "Modification times of MARKER in PROJECT and in REPO, nil where absent."
  (mapcar (lambda (dir)
            (file-attribute-modification-time
             (file-attributes (expand-file-name marker dir))))
          (list project repo)))

(defun ygg-agent--known-config-dir (kind spec project &optional isolate)
  "Config home for KIND in PROJECT, as last named, provisioning nothing.
Asked once per agent per row on every sidebar refresh, so it runs no
process and reads no keychain: a home is worked out afresh only when a
marker file has come, gone or changed, or a login check changed its
answer, and then the way a peek would."
  (let ((hit (gethash (list kind project isolate ygg-agent-conf-root)
                      ygg-agent--config-dirs)))
    (if (and hit (equal (nth 2 hit)
                        (ygg-agent--marker-times (plist-get spec :marker)
                                                 project (nth 1 hit))))
        (car hit)
      (ygg-agent--config-dir kind spec project 'peek isolate))))

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
  (ygg-agent--home-env preset cmd
                       (lambda (kind spec)
                         (ygg-agent--config-dir kind spec project nil isolate))))

(defun ygg-agent--known-config-env (preset cmd project &optional isolate)
  "Return the \"VAR=DIR\" `ygg-agent--config-env' would, making nothing ready.
For whoever only needs to know which home: no process runs, so it is
cheap to ask on every refresh, and once a home has been made ready it
names the one that was."
  (ygg-agent--home-env preset cmd
                       (lambda (kind spec)
                         (ygg-agent--known-config-dir kind spec project isolate))))

(defun ygg-agent--home-env (preset cmd home-of)
  "\"VAR=DIR\" for PRESET/CMD, DIR what HOME-OF gives its kind and spec."
  (condition-case err
      (when-let* ((kind (ygg-agent--kind preset cmd))
                  (spec (cdr (assoc kind ygg-agent--config-homes)))
                  (dir (funcall home-of kind spec))
                  ((not (equal (directory-file-name dir)
                               (directory-file-name
                                (expand-file-name (plist-get spec :home)))))))
        (format "%s=%s" (plist-get spec :var) (directory-file-name dir)))
    (error (display-warning
            'ygg-agent
            (format "config home for %s/%s not set up: %s" preset cmd
                    (error-message-string err))
            :warning)
           nil)))

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
    (clrhash ygg-agent--config-dirs)
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

;;; Skill listing

(declare-function ygg-skill-index-skills "ygg-skill-index" (&optional project all))
(declare-function ygg-skill-index-read "ygg-skill-index" (file dir prefix))
(defvar ygg-skill-index-agents-dir)

(defcustom ygg-agent-skill-core
  '("code-review" "claiming-a-device" "100czk-pr-format" "cog2" "cog3"
    "debug-mantra" "show-me" "figma-build-design" "create-pr" "grill-me"
    "unslop" "explain-architecture" "scrutinize" "differential-review"
    "agent-browser" "daemon")
  "Skills whose descriptions every agent is always shown.
Every other skill is listed by name alone, and found through skill_search."
  :type '(repeat string) :group 'yggdrasil)

(defun ygg-agent--skill-core-p (name)
  (or (member name ygg-agent-skill-core)
      (member (car (last (split-string name ":"))) ygg-agent-skill-core)))

(defun ygg-agent-skill-overrides (current skills)
  "CURRENT skillOverrides, with SKILLS outside the core listed by name.
A skill already off or hidden from the model stays so, one explicitly on
stays on, and a core skill named only is given its description back.
Plugin skills get no entry: the CLI ignores overrides for them.  Neither
do skills the model may not invoke, which are never listed."
  (let ((out (if (hash-table-p current) (copy-hash-table current)
               (make-hash-table :test #'equal))))
    (dolist (skill skills)
      (let* ((name (plist-get skill :name))
             (have (gethash name out)))
        (unless (or (equal (plist-get skill :kind) "plugin")
                    (eq t (plist-get skill :dmi))
                    (member have '("off" "user-invocable-only" "on")))
          (if (ygg-agent--skill-core-p name)
              (when (equal have "name-only") (remhash name out))
            (puthash name "name-only" out)))))
    out))

(defun ygg-agent--table-changes (old new)
  "Keys whose value differs between OLD and NEW, as (KEY OLD-VALUE NEW-VALUE)."
  (let (keys)
    (dolist (table (list old new))
      (when (hash-table-p table)
        (maphash (lambda (k _) (cl-pushnew k keys :test #'equal)) table)))
    (seq-keep (lambda (k)
                (let ((a (and (hash-table-p old) (gethash k old)))
                      (b (gethash k new)))
                  (unless (equal a b) (list k a b))))
              (sort keys #'string<))))

;;;###autoload
(defun ygg-agent-write-skill-overrides (file &optional project)
  "Write the name-only skill listing into the settings FILE.
PROJECT's own skills count too.  Returns what changed, as (NAME OLD NEW)."
  (interactive
   (list (read-file-name "Settings file: " "~/.claude/" nil t "settings.json")
         (and current-prefix-arg (read-directory-name "Project whose skills count too: "))))
  (require 'ygg-skill-index)
  (let* ((json (or (ygg-agent--read-json file) (user-error "Not readable as JSON: %s" file)))
         (current (gethash "skillOverrides" json))
         (new (ygg-agent-skill-overrides current (ygg-skill-index-skills project t)))
         (changes (ygg-agent--table-changes current new)))
    (when changes
      (puthash "skillOverrides" new json)
      (unless (ygg-agent--replace-json file json)
        (error "Could not write %s" file)))
    (when (called-interactively-p 'interactive)
      (message "%d skill override(s) changed in %s" (length changes) file))
    changes))

(defun ygg-agent--implicit-off (yaml)
  "YAML, an agents/openai.yaml, saying the skill is not offered unasked."
  (let ((block "policy:\n  allow_implicit_invocation: false"))
    (cond ((null yaml) (concat block "\n"))
          ((string-match "^\\([ \t]+allow_implicit_invocation:\\).*$" yaml)
           (replace-match "\\1 false" t nil yaml))
          ((string-match "^policy:[ \t]*\n\\([ \t]+\\)" yaml)
           (replace-match "policy:\n\\1allow_implicit_invocation: false\n\\1" t nil yaml))
          ((string-match "^policy:.*$" yaml)
           (replace-match block t t yaml))
          (t (concat yaml (if (string-suffix-p "\n" yaml) "" "\n") block "\n")))))

(defun ygg-agent-codex-skill-policies (&optional dir)
  "Edits to agents/openai.yaml keeping DIR\='s non-core skills from codex.
Codex has no name-only listing; a skill it is not offered is still one
the user can name.  A skill DIR only links to lives elsewhere, and is
left alone.  Each edit is (FILE OLD NEW), OLD nil for a new file."
  (require 'ygg-skill-index)
  (let* ((dir (expand-file-name (or dir ygg-skill-index-agents-dir)))
         (true (file-name-as-directory (file-truename dir)))
         out)
    (dolist (skill-dir (and (file-directory-p dir)
                            (directory-files dir t directory-files-no-dot-files-regexp)))
      (let ((md (expand-file-name "SKILL.md" skill-dir)))
        (when (and (file-regular-p md)
                   (string-prefix-p true (file-truename skill-dir))
                   (not (ygg-agent--skill-core-p
                         (plist-get (ygg-skill-index-read md skill-dir nil) :name))))
          (let* ((file (expand-file-name "agents/openai.yaml" skill-dir))
                 (old (when (file-readable-p file)
                        (with-temp-buffer
                          (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
                          (buffer-string))))
                 (new (ygg-agent--implicit-off old)))
            (unless (equal old new) (push (list file old new) out))))))
    (nreverse out)))

;;;###autoload
(defun ygg-agent-write-codex-skill-policies (&optional dir)
  "Keep DIR's non-core skills out of codex's model-visible list.
Returns the files written."
  (interactive)
  (let ((edits (ygg-agent-codex-skill-policies dir))
        (coding-system-for-write 'utf-8-unix))
    (pcase-dolist (`(,file ,_old ,new) edits)
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert new)))
    (when (called-interactively-p 'interactive)
      (message "%d codex skill polic(ies) written" (length edits)))
    (mapcar #'car edits)))

(provide 'ygg-agent-conf)
