;;; aob-acp.el --- ACP backend for aob -*- lexical-binding: t; -*-

;;; Commentary:
;; Speaks Agent Client Protocol v1 (newline-delimited JSON-RPC over
;; stdio) to agents like claude-agent-acp.  Hand-rolled instead of
;; jsonrpc.el because permission requests must be answered *later*,
;; after a human resolves the Decision — jsonrpc.el dispatchers can
;; only reply synchronously.
;;
;; One connection per (agent, project), many sessions multiplexed over
;; it by sessionId — ACP's own model.  The first spawn pays the adapter
;; boot; every further agent in the project is a session/new away.
;; The process filter only buffers bytes and parses complete frames;
;; rendering is the registry's job.

;;; Code:

(require 'aob)
(require 'aob-subagent)
(require 'json)
(require 'ygg-agent-conf)
(require 'ygg-git)
(require 'ygg-ui)

(defcustom aob-acp-agents
  ;; deliberately npx-pinned, not `executable-find': a stale adapter on
  ;; PATH silently lacks methods like session/load — pin your own
  ;; install here if npx startup bothers you
  ;; isolated agents run unattended in a throwaway worktree — that is
  ;; what makes their full-permission :mode safe (and dontAsk is NOT
  ;; that mode: it denies whatever isn't pre-approved)
  '(("claude" :command ("npx" "-y" "@agentclientprotocol/claude-agent-acp"))
    ("codex" :command ("npx" "-y" "@agentclientprotocol/codex-acp"))
    ("gemini" :command ("gemini" "--experimental-acp"))
    ;; pi-acp spawns `pi --mode rpc'; first use needs `pi --terminal-login'.
    ;; No :worktree/:mode until a live session/new shows availableModes —
    ;; never invent a mode id (per adapter-gotchas doctrine)
    ("pi" :command ("npx" "-y" "pi-acp"))
    ;; hermes-agent[acp] via uvx; logs to stderr, ACP JSON-RPC on stdout
    ("hermes" :command ("uvx" "--from" "hermes-agent[acp]" "hermes-acp")))
  "What starts an adapter: NAME → plist with :command argv.
Only the process lives here.  How a session is to behave — worktree,
permission mode, model, effort — is a preset, because the same binary
answers to several of them; see `aob-acp-presets'."
  :type '(alist :key-type string :value-type plist)
  :group 'aob)

(defcustom aob-acp-command-function #'aob-acp-login-shell-command
  "Function turning an agent's argv into the final launch command.
The default wraps it in a login shell so the agent binary resolves off
the profile PATH (a GUI Emacs misses it)."
  :type 'function :group 'aob)

(defvar aob-acp-show-trace t
  "Whether a spawn shows the raw ACP trace in a side window.
The daemon binds it nil: its own trace is the view, the raw one is a key.")

(defcustom aob-acp-environment-function nil
  "Function (AGENT PROJECT DIR ISOLATE) returning extra \"VAR=VAL\" strings
prepended to the agent\='s environment, or nil.
ISOLATE is what this connection belongs to when it is one of its own,
so the caller can give it a config home nothing else writes to."
  :type '(choice function (const nil)) :group 'aob)

(defcustom aob-acp-prepare-function nil
  "Function (AGENT PROJECT ISOLATE) run as a connection for them starts, or nil.
Where whatever the connection's environment names is made ready: the
environment function is also asked for every lookup of a running
connection, so it must only name things, never make them."
  :type '(choice function (const nil)) :group 'aob)

(defun aob-acp-login-shell-command (argv)
  ;; -l only, never -i: interactive shell startup can print to stdout
  ;; and stdout is the ndjson wire
  (list (or (getenv "SHELL") "/bin/zsh") "-l" "-c"
        (mapconcat #'shell-quote-argument argv " ")))

(defcustom aob-acp-worktree-root
  (expand-file-name "aob-worktrees"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory isolated agent worktrees are created under."
  :type 'directory :group 'aob)

(defcustom aob-acp-persist-file (locate-user-emacs-file "var/aob-sessions.eld")
  "File persisting live ACP sessions for resuming."
  :type '(choice file (const nil)) :group 'aob)

(defcustom aob-acp-default-agent "claude"
  "Agent definition commands default to."
  :type 'string :group 'aob)

;;; Wire — proc level

(defun aob-acp--send-proc (proc msg)
  (process-send-string proc (concat (json-serialize msg) "\n")))

(defun aob-acp--request-owners (proc)
  "PROC's table of our outstanding request ids to the session that sent each."
  (or (process-get proc 'aob-request-owners)
      (let ((h (make-hash-table :test #'equal)))
        (process-put proc 'aob-request-owners h)
        h)))

(defun aob-acp--request-proc (proc method params cb &optional owner)
  "Send METHOD with PARAMS on PROC, CB taking the reply; return the id.
OWNER is the session the request speaks for, so an agent request scoped
to this one can find its way back."
  (let ((id (cl-incf (car (process-get proc 'aob-next-id)))))
    (puthash id cb (process-get proc 'aob-pending))
    (when owner (puthash id owner (aob-acp--request-owners proc)))
    (aob-acp--send-proc proc (list :jsonrpc "2.0" :id id
                                   :method method :params params))
    id))

(defun aob-acp--respond-proc (proc id result &optional error)
  (aob-acp--send-proc proc (if error
                               (list :jsonrpc "2.0" :id id :error error)
                             (list :jsonrpc "2.0" :id id :result result))))

;;; Wire — session level

(defun aob-acp--request (s method params cb)
  (aob-acp--request-proc (aob-session-conn s) method params cb s))

(defun aob-acp--notify (s method params)
  (aob-acp--send-proc (aob-session-conn s)
                      (list :jsonrpc "2.0" :method method :params params)))

(defun aob-acp--proc-of (s)
  "The connection S speaks over; a subagent the agent announced has none
of its own and answers over the one that announced it."
  (or (aob-session-conn s) (aob-session-ref s :acp-conn)))

(defun aob-acp--respond (s id result &optional error)
  (aob-acp--respond-proc (aob-acp--proc-of s) id result error))

;;; Connections — one per (agent . project), sessions multiplexed

(defvar aob-acp--conns (make-hash-table :test #'equal))

(defun aob-acp--proc-sessions (proc)
  "PROC's sid→session table; conns from before a hot-load lack it, so
rebuild from the registry rather than crash the process filter."
  (or (process-get proc 'aob-sessions)
      (let ((h (make-hash-table :test #'equal)))
        (dolist (s (aob-acp--conn-sessions proc))
          (when-let* ((sid (aob-session-ref s :acp-id)))
            (puthash sid s h)))
        (process-put proc 'aob-sessions h)
        h)))

(defun aob-acp--register (proc sid s)
  (puthash sid s (aob-acp--proc-sessions proc))
  (aob-session-put s :acp-id sid))

(defun aob-acp--deregister (proc s)
  (let ((tbl (process-get proc 'aob-sessions)))
    (when tbl
      (maphash (lambda (k v) (when (eq v s) (remhash k tbl))) tbl))))

(defun aob-acp--conn-sessions (proc)
  "Registry sessions still bound to PROC."
  (seq-filter (lambda (s) (eq (aob-session-conn s) proc)) (aob-sessions)))

(defvar aob-acp-isolate nil
  "What the connection about to be made belongs to, or nil for the tree.

An adapter process is shared by every session that keys to the same
thing, and one that exits takes all of them with it — a record of three
tasks dying on the same stderr line is what this is for.  Bound around a
spawn, the caller gets a process of its own, with its own agent config
home, its own authentication cache and its own MCP registry: nothing
another session does can reach it.

Nil keys the connection the way it always was, by agent and tree.")

(defvar aob-acp--session-env nil
  "Entries the session opening now adds to its connection's environment.
Bound around a connect; the key carries them, so only sessions asking
for the same entries share a process.")

(defun aob-acp--cap-env (s)
  "S's subagent cap as the environment Claude's workflows read their limit from."
  (when-let* ((cap (aob-session-ref s :subagent-cap))
              ((>= cap 1)))
    (list (format "CLAUDE_CODE_WORKFLOW_MAX_CONCURRENT_AGENTS=%d" (min cap 256)))))

(defun aob-acp--codex-agent-p (agent)
  "Whether AGENT runs codex's adapter."
  (and (seq-some (lambda (arg) (string-match-p "codex-acp" arg))
                 (plist-get (cdr (assoc agent aob-acp-agents)) :command))
       t))

(defcustom aob-acp-session-env-function nil
  "Function (SESSION) returning extra \"VAR=VAL\" strings for SESSION's connection.
For an adapter that reads from its environment what session/new would
have told it.  The strings join the connection's key, so sessions that
differ in them never share a process."
  :type '(choice (const nil) function) :group 'aob)

(defun aob-acp--agent-env (s)
  "The environment S's agent reads its subagent limits and roles from.
Codex's adapter merges the JSON in CODEX_CONFIG into every thread it
starts or resumes, and reads nothing of the kind from session/new."
  (append
   (if (aob-acp--codex-agent-p (aob-session-ref s :agent))
       (when-let* ((config (aob-acp--codex-config s)))
         (list (concat "CODEX_CONFIG=" (json-serialize config))))
     (aob-acp--cap-env s))
   (and aob-acp-session-env-function
        (condition-case err
            (funcall aob-acp-session-env-function s)
          (error
           (display-warning
            'aob (format "%s: extra session environment failed, continuing without it: %s"
                         (aob-session-ref s :agent) (error-message-string err))
            :warning)
           nil)))))

(defun aob-acp--conn-env (agent project)
  "The environment a connection for AGENT on PROJECT would be started with."
  (append aob-acp--session-env
          (and aob-acp-environment-function
               (ignore-errors
                 (funcall aob-acp-environment-function
                          agent project project aob-acp-isolate)))))

(defun aob-acp--conn-key (agent project)
  "What the connection for AGENT on PROJECT is filed under.
The environment is part of it.  A process is started with the
environment of whoever opened it and keeps it for life, so a session
asking for a different one — another config home, another set of
credentials — must not be handed a connection that already has
somebody else\='s.  Without this the second caller silently runs as the
first."
  (let ((env (aob-acp--conn-env agent project)))
    (append (list agent project)
            (and aob-acp-isolate (list aob-acp-isolate))
            (and env (list (secure-hash 'sha1 (format "%S" env)))))))

(defun aob-acp--live-conn (agent project)
  (let ((proc (gethash (aob-acp--conn-key agent project) aob-acp--conns)))
    (and proc (process-live-p proc) proc)))

(defun aob-acp--start-conn (agent project)
  (let* ((spec (or (cdr (assoc agent aob-acp-agents))
                   (user-error "aob: unknown agent %s" agent)))
         (argv (plist-get spec :command))
         (default-directory project)
         (_ready (and aob-acp-prepare-function
                      (funcall aob-acp-prepare-function
                               agent project aob-acp-isolate)))
         ;; a claude spawned with CLAUDECODE set refuses to start (nested
         ;; guard); the append keeps envrc/mise buffer-local env visible
         (process-environment
          (append aob-acp--session-env
                  (and aob-acp-environment-function
                       (funcall aob-acp-environment-function
                                agent project project aob-acp-isolate))
                  (seq-remove (lambda (v) (string-prefix-p "CLAUDECODE=" v))
                              process-environment)))
         (base (format "%s:%s" agent
                       (or aob-acp-isolate
                           (file-name-nondirectory
                            (directory-file-name project)))))
         (stderr (generate-new-buffer (format " *aob-stderr:%s*" base)))
         (proc (make-process
                :name (concat "aob-" base)
                :command (funcall aob-acp-command-function argv)
                :connection-type 'pipe
                :coding 'utf-8-unix
                :noquery t
                :filter #'aob-acp--filter
                :sentinel #'aob-acp--sentinel
                ;; consulted for `default-directory': a remote one puts the
                ;; agent on that host.  stderr stays a stream of its own
                ;; there too, which the protocol on stdout depends on
                :file-handler t
                :stderr stderr)))
    (process-put proc 'aob-conn-key (aob-acp--conn-key agent project))
    (process-put proc 'aob-remote (file-remote-p default-directory))
    (process-put proc 'aob-env process-environment)
    (process-put proc 'aob-sessions (make-hash-table :test #'equal))
    (process-put proc 'aob-next-id (list 0))
    (process-put proc 'aob-pending (make-hash-table :test #'eql))
    (process-put proc 'aob-json-buf (generate-new-buffer (format " *aob-json:%s*" base)))
    (process-put proc 'aob-stderr-buf stderr)
    (process-put proc 'aob-init 'pending)
    (puthash (aob-acp--conn-key agent project) proc aob-acp--conns)
    (aob-acp--initialize proc)
    proc))

(defcustom aob-acp-native-subagents nil
  "Non-nil asks an agent to announce its subagents as sessions of their own.
The agent that agrees sends each subagent's words and steps under the
subagent's own session id, and says when it ends; one that does not is
read as before, each Agent call or codex thread becoming a subagent.
Read when a connection starts, so a change reaches new connections only.

Off, because the only switch the adapters take today is JetBrains AIR's
`_meta' list, and saying it puts claude-agent-acp into AIR-client mode
for the whole connection, not just for its subagents."
  :type 'boolean :group 'aob)

(defconst aob-acp--air-subagents "nativeSubagentSessions"
  "The name JetBrains AIR's capability list gives native subagent sessions.")

(defcustom aob-acp-async-tasks nil
  "Non-nil asks an agent to report each background command as a task it can stop.
The agent that agrees says when a command it left running ends, and
stops one on request; without it a background command is seen only as
the call that started it, and stopping one means ending its process.
Read when a connection starts.

Off for the reason `aob-acp-native-subagents' is: the switch is the
same AIR list, and it puts the adapter into AIR-client mode."
  :type 'boolean :group 'aob)

(defconst aob-acp--air-async-tasks "asyncTasks"
  "The name JetBrains AIR's capability list gives stoppable background tasks.")

(defconst aob-acp-protocol-version 1
  "The ACP version every agent is spoken to in when nothing newer is agreed.")

(defcustom aob-acp-offer-protocol-version 1
  "The highest ACP version an initialize offers; the agent answers which it takes.
2 is the draft.  No released adapter takes it yet: claude-agent-acp and
codex-acp both answer 1 whatever is offered, and codex-acp's unreleased
main picks v2 from the offer.  Under v2 aob reads the init result, the
session lifetime (resume with replayFrom, list, close, delete) and the
subject-generic permission request; the rest of the v2 update vocabulary
\(agent_message, state_update, tool_call_update as an upsert) is not
read, so offering 2 is an experiment.  Read when a
connection starts."
  :type '(choice (const 1) (const 2)) :group 'aob)

(defun aob-acp--initialize-params (offer)
  "The initialize params offering ACP version OFFER.
Version 1 is the request as it always was.  A v2 offer carries the v2
keys beside the v1 ones: an agent without a version router reads the
request as v1 whatever it says, and both schemas drop keys they do not
know, so each side finds its own."
  (let ((caps (aob-acp--client-capabilities))
        (info (list :name "aob.el" :version "0.1")))
    (append (list :protocolVersion offer
                  :clientCapabilities caps
                  :clientInfo info)
            (when (>= offer 2)
              (list :info info
                    :capabilities
                    (list :elicitation (plist-get caps :elicitation)
                          :_meta (plist-get caps :_meta)))))))

(defun aob-acp--initialize (proc)
  "Send PROC the initialize request and settle its init state on the reply."
  (process-put proc 'aob-subagents-offered aob-acp-native-subagents)
  (process-put proc 'aob-async-tasks-offered aob-acp-async-tasks)
  (process-put proc 'aob-protocol-offered aob-acp-offer-protocol-version)
  (process-put
   proc 'aob-init-id
   (aob-acp--request-proc
    proc "initialize"
    (aob-acp--initialize-params aob-acp-offer-protocol-version)
    (lambda (res err) (aob-acp--initialized proc res err)))))

(defun aob-acp--version-error (res &optional offered)
  "Why the initialize result RES cannot be spoken with, or nil.
Any version from 1 up to OFFERED is spoken; OFFERED defaults to
`aob-acp-protocol-version'."
  (let ((version (plist-get res :protocolVersion))
        (offered (or offered aob-acp-protocol-version)))
    (cond
     ((null version)
      (list :message (format "agent names no ACP version; aob speaks %d"
                             offered)))
     ((not (and (integerp version) (<= 1 version offered)))
      (list :message (format "agent speaks ACP v%s; aob speaks %d"
                             version offered))))))

(defun aob-acp--v1-init (res)
  "RES, a v2 initialize result, in the v1 shape every reader here expects.
The v2 capabilities sit under `capabilities', the session ones under its
`session', and a present `session' means list, resume and close; there
is no `loadSession', since resume replays.  A v1 result is RES itself."
  (if (not (eql (plist-get res :protocolVersion) 2))
      res
    (let* ((caps (plist-get res :capabilities))
           (session (plist-get caps :session))
           (base (and (plist-member caps :session)
                      (list :list nil :resume nil :close nil))))
      (append (list :protocolVersion 2
                    :agentInfo (plist-get res :info)
                    :agentCapabilities
                    (append (list :sessionCapabilities (append base session)
                                  :promptCapabilities (plist-get session :prompt)
                                  :mcpCapabilities (plist-get session :mcp))
                            caps))
              res))))

(defun aob-acp-protocol (proc)
  "The ACP version PROC's agent agreed to, or nil before it answered."
  (plist-get (aob-acp--conn-init proc) :protocolVersion))

(defun aob-acp--initialized (proc res err)
  "Settle PROC's init state with RES or ERR and wake whoever waits on it.
An agent on another protocol version cannot be spoken with, so its
connection is closed and every waiting session fails with the reason."
  (let ((mismatch (and (not err)
                       (aob-acp--version-error
                        res (process-get proc 'aob-protocol-offered)))))
    (setq err (or err mismatch))
    (unless err (setq res (aob-acp--v1-init res)))
    (process-put proc 'aob-init (if err (list 'failed err) (list 'done res)))
    (process-put proc 'aob-subagents
                 (and (not err) (aob-acp--native-subagents-p proc res)))
    (when mismatch
      (when-let* ((key (process-get proc 'aob-conn-key)))
        (when (eq (gethash key aob-acp--conns) proc)
          (remhash key aob-acp--conns))))
    (dolist (w (process-get proc 'aob-init-waiters))
      (funcall w (and (not err) res) err))
    (process-put proc 'aob-init-waiters nil)
    (when mismatch
      ;; the sentinel would repaint the failure as a plain exit; the filter
      ;; that brought this reply is still reading the frame buffer
      (set-process-sentinel proc #'ignore)
      (run-at-time 0 nil #'aob-acp--conn-cleanup proc))))

(defun aob-acp--client-capabilities ()
  "What this client can do, as the initialize request declares it.
Form elicitation is what turns on an agent's own questions: claude
disallows AskUserQuestion and codex answers request_user_input empty
without it.  The protocol reads the declaration as an object, and a bare
true fails that schema and is dropped as though never sent.  URL
elicitation is declared the same way; it is how an agent hands over a
page to sign in on, and codex offers its device-code login only with it.
Terminal auth, a plain boolean in the schema, is what makes claude list
its own logins at all.  Boolean config options are opt-in: an agent
withholds its toggles until the client says it can show them.  Native
subagents are asked for through the AIR list alone: the released schema
has no field for them, and the adapters strip one sent anyway.  Notices,
compaction and command output are declared so each arrives as an update
of its own, where an agent that is not told sends them as prose."
  (append
   (list :fs (list :readTextFile :false :writeTextFile :false)
         :elicitation (list :form (make-hash-table) :url (make-hash-table))
         :auth (list :terminal t)
         :session (list :configOptions (list :boolean (make-hash-table))
                        :notices (make-hash-table)
                        :compaction (make-hash-table)))
   ;; without subagent-transcript the adapter treats us as a client that
   ;; cannot nest, and strips every subagent's words before sending
   (list :_meta (append
                 (list :subagent-transcript t :terminal_output_delta t)
                 (when-let* ((air (delq nil (list (and aob-acp-native-subagents
                                                       aob-acp--air-subagents)
                                                  (and aob-acp-async-tasks
                                                       aob-acp--air-async-tasks)))))
                   (list :jetbrains
                         (list :air (list :version 1
                                          :capabilities (vconcat air)))))))))

(defun aob-acp--native-subagents-p (proc init)
  "Whether PROC offered native subagents and its INIT result took them.
Both adapters list subagents among their session capabilities whoever
asks, so that alone says they can; asking is what makes it so."
  (and (process-get proc 'aob-subagents-offered)
       (or (aob-acp--session-cap init :subagents)
           (member aob-acp--air-subagents
                   (plist-get (plist-get (plist-get (plist-get init :_meta) :jetbrains)
                                         :air)
                              :capabilities)))
       t))

(defun aob-acp--with-init (proc cb)
  "Run CB with (INIT-RESULT ERR) once PROC's initialize settles."
  (pcase (process-get proc 'aob-init)
    (`(done ,res) (funcall cb res nil))
    (`(failed ,err) (funcall cb nil err))
    (_ (process-put proc 'aob-init-waiters
                    (cons cb (process-get proc 'aob-init-waiters))))))

(defun aob-acp--kill-tree (proc)
  "End PROC and every process it spawned.
Emacs signals only its own child, the launch shell, while the adapter
and each CLI it runs sit below it in that child's process group — so a
plain `delete-process' leaves them orphaned and still talking to a
model.  Emacs gives every child a group of its own, which is why the
group can be signalled without touching Emacs or anything else."
  (when (process-live-p proc)
    (when-let* ((pid (process-id proc)))
      (ignore-errors
        ;; a bare pid is signalled here, whoever it belongs to: over TRAMP
        ;; that number means a process on the other host, and the signal
        ;; only follows it there when the remote place is named too
        (if-let* ((remote (process-get proc 'aob-remote)))
            (signal-process (- pid) 'TERM remote)
          (signal-process (- pid) 'TERM))))
    (set-process-query-on-exit-flag proc nil)
    (delete-process proc)))

(defun aob-acp--conn-cleanup (proc)
  (when-let* ((key (process-get proc 'aob-conn-key)))
    (when (eq (gethash key aob-acp--conns) proc)
      (remhash key aob-acp--conns)))
  (aob-acp--kill-tree proc)
  (dolist (key '(aob-json-buf aob-stderr-buf))
    (when-let* ((buf (process-get proc key)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; Frame pump

(defun aob-acp--filter (proc chunk)
  (let ((buf (process-get proc 'aob-json-buf)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert chunk)
        (goto-char (point-min))
        ;; zero-copy framing: parse the value straight out of the buffer
        ;; instead of extracting each line as a string first
        (while (and (buffer-live-p buf)
                    (progn (goto-char (point-min))
                           (search-forward "\n" nil t)))
          (let* ((end (point))
                 (msg (progn (goto-char (point-min))
                             (aob-acp--read-frame proc end))))
            (delete-region (point-min) end)
            (pcase msg
              (:noise nil)
              (:malformed
               (aob-acp--respond-proc proc :null nil
                                      (list :code -32700 :message "Parse error")))
              (_ (aob-acp--dispatch proc msg)))))))))

(defun aob-acp--read-frame (proc end)
  "The frame at point, ending by END, as PROC's dispatch takes it.
:noise for a blank line or stdout chatter such as a banner, and
:malformed for an attempted frame that is not JSON.  The initialize
reply is read a second time with null kept apart from the empty object,
then every null key dropped, so a capability sent as null
reads as one never sent while one sent as {} is still present."
  (if (progn (skip-chars-forward " \t\r\n" end) (= (point) end))
      :noise
    (let ((start (point)))
      (condition-case nil
          (let ((msg (json-parse-buffer :object-type 'plist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)))
            (if (and (not (plist-get msg :method))
                     (plist-get msg :id)
                     (eql (plist-get msg :id) (process-get proc 'aob-init-id)))
                (progn
                  (goto-char start)
                  (aob-acp--drop-nulls
                   (json-parse-buffer :object-type 'plist
                                      :array-type 'array
                                      :null-object :null
                                      :false-object nil)))
              msg))
        (json-error (if (memq (char-after start) '(?{ ?\[)) :malformed :noise))))))

(defun aob-acp--drop-nulls (value)
  "VALUE, parsed with arrays as vectors and null as :null, with every
null key and array element removed and arrays turned back into the
lists the rest of the wire reads."
  (cond
   ((vectorp value)
    (mapcar #'aob-acp--drop-nulls
            (seq-remove (lambda (v) (eq v :null)) value)))
   ((consp value)
    (let (out)
      (while value
        (let ((key (pop value))
              (v (pop value)))
          (unless (eq v :null)
            (push key out)
            (push (aob-acp--drop-nulls v) out))))
      (nreverse out)))
   (t value)))

(defun aob-acp--route (proc sid)
  "The session a frame belongs to: by SID, else the sole bound session.
A subagent let go of belongs to none."
  (or (and sid (gethash sid (aob-acp--proc-sessions proc)))
      (unless (and sid (gethash sid (aob-acp--gone proc)))
        (let ((bound (aob-acp--conn-sessions proc)))
          (and bound (null (cdr bound)) (car bound))))))

(defconst aob-acp--request-methods
  '("session/request_permission" "elicitation/create")
  "The agent requests this client answers; any other is Method not found.")

(defun aob-acp--request-owner (proc params)
  "The session an agent request with PARAMS on PROC belongs to, or nil.
A request scoped to one of ours names that request instead of a
session, and goes to the session that sent it.  One scoped to a request
no session sent, such as the initialize, belongs to the connection, so
it goes to a session on it rather than unanswered."
  (let ((sid (plist-get params :sessionId))
        (rid (plist-get params :requestId)))
    (aob-acp--tool-maker
     proc
     (or (and (not sid) rid
              (gethash rid (aob-acp--request-owners proc)))
         (aob-acp--route proc sid)
         (and (not sid) rid
              (car (aob-acp--conn-sessions proc))))
     (plist-get (aob-acp--permission-tool-call params) :toolCallId))))

(defun aob-acp--tool-maker (proc s tid)
  "The session on PROC that made tool call TID, S when it did or none did.
Codex may ask a subagent's permission on the session that sent it."
  (or (and s tid
           (not (gethash tid (aob-acp--tools s)))
           (seq-find (lambda (other)
                       (when-let* ((tools (aob-session-ref other :tools)))
                         (gethash tid tools)))
                     (hash-table-values (aob-acp--proc-sessions proc))))
      s))

(defun aob-acp--dispatch (proc msg)
  (let* ((method (plist-get msg :method))
         (id (plist-get msg :id))
         (params (plist-get msg :params)))
    (cond
     ((equal method "$/cancel_request")
      (aob-acp--request-withdrawn proc (plist-get params :requestId)))
     ((and method id)
      (let ((s nil))
        (cond
         ((not (member method aob-acp--request-methods))
          (aob-acp--respond-proc proc id nil
                                 (list :code -32601 :message "Method not found")))
         ((and (equal method "elicitation/create")
               (plist-member params :mode)
               (not (member (plist-get params :mode) '("form" "url"))))
          (aob-acp--respond-proc proc id nil
                                 (list :code -32602 :message "Invalid params")))
         ((gethash (plist-get params :sessionId) (aob-acp--gone proc))
          (aob-acp--respond-proc proc id
                                 (if (equal method "elicitation/create")
                                     (list :action "cancel")
                                   (list :outcome (list :outcome "cancelled")))))
         ((setq s (aob-acp--request-owner proc params))
          (aob-acp--on-request s id method params))
         (t (aob-acp--respond-proc proc id nil
                                   (list :code -32603
                                         :message "aob: unknown session"))))))
     ((equal method "_auth/status_update")
      (aob-acp--auth-status proc (plist-get params :authStatus)))
     ((equal method "elicitation/complete")
      (aob-acp--elicitation-complete proc (plist-get params :elicitationId)))
     (method (when-let* ((s (aob-acp--route proc (plist-get params :sessionId))))
               (aob-acp--on-notification s method params)))
     (id (remhash id (aob-acp--request-owners proc))
         (when-let* ((cb (gethash id (process-get proc 'aob-pending))))
           (remhash id (process-get proc 'aob-pending))
           (funcall cb (plist-get msg :result) (plist-get msg :error)))))))

(defconst aob-acp--noise-re
  "\\`\\[[a-z]+/[a-z]+\\][^\n]*\\(durationMs\\|totalMs\\)=\\|\\`{\"type\":"
  "Lines an adapter prints after the thing that killed it.
Telemetry and stray protocol chatter: an adapter that fails writes the
reason and then keeps talking, so the last line of its stderr is the
least useful line in it.")

(defconst aob-acp--blame-re
  "\\(?:error\\|fatal\\|panic\\|exception\\|traceback\\|refused\\|denied\\|\
not found\\|cannot\\|could not\\|unauthor\\|ENOENT\\|EACCES\\|EADDR\\)"
  "What a line that says why a process died tends to carry.")

(defun aob-acp--fail-reason (tail)
  "The line of TAIL that says why the process went, or nil.
The last line is what a naive read takes and is usually telemetry
printed after the failure; the blame is the newest line that reads like
one, and failing that the newest line that is not noise."
  (when-let* ((lines (seq-remove #'string-empty-p
                                 (mapcar #'string-trim
                                         (split-string (or tail "") "\n")))))
    (let ((newest-first (reverse lines))
          (case-fold-search t))
      (or (seq-find (lambda (line) (string-match-p aob-acp--blame-re line))
                    newest-first)
          (seq-find (lambda (line)
                      (not (string-match-p aob-acp--noise-re line)))
                    newest-first)
          (car newest-first)))))

(defun aob-acp--sentinel (proc _event)
  (unless (process-live-p proc)
    ;; the actual failure lives on stderr — surface its tail everywhere
    (let ((tail (when-let* ((buf (process-get proc 'aob-stderr-buf)))
                  (and (buffer-live-p buf)
                       (with-current-buffer buf
                         (string-trim
                          (buffer-substring-no-properties
                           (max (point-min) (- (point-max) 2000))
                           (point-max)))))))
          (died nil))
      (dolist (s (aob-acp--conn-sessions proc))
        (when (aob-session-get (aob-session-id s))
          (aob-session-put s :fail-reason
                           (or (aob-acp--fail-reason tail) "process exited"))
          (dolist (d (aob-session-decisions s))
            (when-let* ((ev (aob-acp--decision-event s d)))
              (plist-put ev :line nil)))
          (setf (aob-session-decisions s) nil)
          (aob-session-settle-subagents s t)
          (aob-set-state s 'dead)
          (push s died)
          (aob-event s 'error :title "process exited" :text tail)
          (message "aob: %s died: %s" (aob-session-name s)
                   (aob-session-ref s :fail-reason))))
      (aob-acp--fail-pending proc died
                             (or (aob-acp--fail-reason tail) "process exited")))
    (aob-acp--conn-cleanup proc)))

(defun aob-acp--fail-pending (proc died why)
  "Answer each request still awaiting PROC, once, with an error saying WHY.
One sent for a session in DIED gets nothing: that session was just told
it is dead, and a reply would carry it back to idle."
  (let ((pending (process-get proc 'aob-pending))
        (owners (aob-acp--request-owners proc))
        (owed nil))
    (when pending
      (maphash (lambda (id cb)
                 (unless (memq (gethash id owners) died)
                   (push cb owed)))
               pending)
      (clrhash pending))
    (clrhash owners)
    (dolist (cb (nreverse owed))
      (ignore-errors (funcall cb nil (list :code -32603 :message why))))))

;;; Incoming requests — permission becomes a Decision object; the JSON-RPC
;;; reply is held until a human resolves it

(defvar aob-acp-request-functions nil
  "Abnormal hook run with (SESSION DECISION TOOLCALL) for every permission
request, after its decision is pushed and its event recorded.")

(defun aob-acp--on-request (s id method params)
  (pcase method
    ("session/request_permission"
     (let* ((tc (aob-acp--known-tool-call s (aob-acp--permission-tool-call params)))
            (raw (plist-get tc :rawInput))
            (plan (aob-acp--plan-text tc))
            (d (list :reply-id id
                     :title (or (plist-get tc :title) (plist-get params :title)
                                "permission")
                     :detail (when-let* ((str (cond (plan nil)
                                                    ((and (listp raw)
                                                          (plist-get raw :command)))
                                                    (raw (format "%S" raw)))))
                               (truncate-string-to-width (format "%s" str) 72))
                     :options (append (plist-get params :options) nil))))
       (when plan
         (setq d (append d (list :kind 'plan :plan plan))))
       (push d (aob-session-decisions s))
       (aob-set-state s 'blocked)
       (let ((ev (if plan
                     (aob-event s 'permission :title (plist-get d :title)
                                :decision-kind 'plan :plan plan
                                :options (plist-get d :options))
                   (aob-event s 'permission :title (plist-get d :title)))))
         (nconc d (list :seq (plist-get ev :seq))))
       (dolist (fn aob-acp-request-functions)
         (condition-case err (funcall fn s d tc)
           (error (message "aob-acp-request-functions: %S" err))))))
    ((and "elicitation/create" (guard (equal (plist-get params :mode) "url")))
     (aob-acp--url-elicitation s id params))
    ;; AskUserQuestion (and MCP elicitations) arrive as a form request;
    ;; it becomes a Decision like permissions do — ■, then r answers
    ("elicitation/create"
     (let ((d (list :reply-id id
                    :kind 'elicitation
                    :title (aob--first-line (or (plist-get params :message)
                                                "question")
                                            64)
                    :questions (aob-acp--elicit-questions params))))
       (push d (aob-session-decisions s))
       (aob-set-state s 'blocked)
       (let ((ev (aob-event s 'permission :title (plist-get d :title)
                            :decision-kind 'elicitation
                            :questions (plist-get d :questions))))
         (nconc d (list :seq (plist-get ev :seq))))
       ;; the same subscribers a permission wakes: a question nobody is
       ;; told about is a session that stops and does not say why
       (dolist (fn aob-acp-request-functions)
         (condition-case err (funcall fn s d nil)
           (error (message "aob-acp-request-functions: %S" err))))))
    (_ (aob-acp--respond s id nil
                         (list :code -32601
                               :message (format "aob: %s not supported" method))))))

(defun aob-acp--permission-tool-call (params)
  "The tool call a permission request PARAMS asks about.
v1 names it outright; the v2 draft names a subject instead, a tool call
or a command, and a command is read as the call that would run it."
  (let ((subject (plist-get params :subject)))
    (or (plist-get params :toolCall)
        (pcase (plist-get subject :type)
          ("tool_call" (plist-get subject :toolCall))
          ("command" (list :toolCallId (plist-get subject :toolCallId)
                           :kind "execute"
                           :rawInput (list :command (plist-get subject :command)
                                           :cwd (plist-get subject :cwd))))))))

(defun aob-acp--known-tool-call (s tc)
  "TC with the kind and content S already heard for it filled in where
TC leaves them out.  An adapter speaking to an AIR client sends a
permission only what it adds to the call, and a plan is told by its kind."
  (if-let* ((ev (gethash (plist-get tc :toolCallId) (aob-acp--tools s))))
      (append tc (list :kind (plist-get ev :kind) :content (plist-get ev :content)))
    tc))

(defun aob-acp--plan-text (toolcall)
  "The plan TOOLCALL asks to leave planning with, or nil when it asks
something else.  Claude carries it as the call's text content, codex
only in its raw input."
  (when (equal (plist-get toolcall :kind) "switch_mode")
    (let ((raw (plist-get toolcall :rawInput)))
      (or (seq-some (lambda (c)
                      (let ((text (plist-get (plist-get c :content) :text)))
                        (and (equal (plist-get c :type) "content")
                             (stringp text) (not (string-empty-p text))
                             text)))
                    (plist-get toolcall :content))
          (and (listp raw) (stringp (plist-get raw :plan))
               (plist-get raw :plan))))))

(defun aob-acp--elicit-field (key field text)
  "One question from schema FIELD called KEY, asking TEXT.
Options are read as oneOf or anyOf constants, and as a plain enum:
claude writes the first, and a tool codex is carrying writes whichever
its own schema used.  A constant's title and description are kept in
:notes as (CONST :title TITLE :description DESCRIPTION)."
  (let* ((multi (equal (plist-get field :type) "array"))
         (spec (if multi (or (plist-get field :items) field) field))
         (constants (or (plist-get spec :oneOf) (plist-get spec :anyOf)))
         (enum (plist-get spec :enum)))
    (list :key key
          :text text
          :header (plist-get field :title)
          :multi multi
          :options (cond (constants (delq nil (mapcar (lambda (o)
                                                        (plist-get o :const))
                                                      constants)))
                         (enum (append enum nil)))
          :notes (delq nil (mapcar (lambda (o)
                                     (when (and (plist-get o :const)
                                                (or (plist-get o :title)
                                                    (plist-get o :description)))
                                       (list (plist-get o :const)
                                             :title (plist-get o :title)
                                             :description (plist-get o :description))))
                                   constants)))))

(defun aob-acp--elicit-companion (key field keys)
  "The question KEY answers in its own words, or nil when FIELD is a question.
Claude marks the free-text field beside each question in _meta, codex
marks its own other field there too; a field named after a question with
_custom on the end is read the same way.  KEYS are every field's name."
  (let* ((meta (plist-get field :_meta))
         (claude (plist-get meta :_askUserQuestionCustomAnswer))
         (codex (plist-get meta :codex)))
    (cond ((plist-get claude :questionId))
          ((eq (plist-get codex :isOtherAnswer) t) (plist-get codex :questionId))
          ((and (string-suffix-p "_custom" key)
                (member (string-remove-suffix "_custom" key) keys))
           (string-remove-suffix "_custom" key)))))

(defun aob-acp--elicit-questions (params)
  "PARAMS' form schema as a list of question plists, in schema order.
Each is (:key FIELD :text QUESTION :header TITLE :multi BOOL
:options LABELS :custom FIELD), :custom naming the field a typed answer
goes to when it is not the question's own.  A lone question's words are
the request's message, since claude leaves them out of the field."
  (let* ((props (plist-get (plist-get params :requestedSchema) :properties))
         (message (plist-get params :message))
         (fields (let ((rest props) acc)
                   (while rest
                     (push (cons (substring (symbol-name (pop rest)) 1) (pop rest))
                           acc))
                   (nreverse acc)))
         (keys (mapcar #'car fields))
         (companions (delq nil (mapcar (lambda (f)
                                         (when-let* ((q (aob-acp--elicit-companion
                                                         (car f) (cdr f) keys)))
                                           (cons q (car f))))
                                       fields)))
         (asked (seq-remove (lambda (f) (rassoc (car f) companions)) fields)))
    (mapcar (lambda (f)
              (append
               (aob-acp--elicit-field
                (car f) (cdr f)
                (or (plist-get (cdr f) :description)
                    (and (null (cdr asked)) message)
                    (plist-get (cdr f) :title)
                    message))
               (list :custom (cdr (assoc (car f) companions)))))
            asked)))

;;; Session updates → registry events.  Chunks coalesce into one mutable
;;; event; tool_call_update mutates the original tool event in place.

(defvar aob-acp-notification-functions nil
  "Abnormal hook run with (SESSION METHOD PARAMS) for every notification
before it is dispatched.")

(defun aob-acp--on-notification (s method params)
  (dolist (fn aob-acp-notification-functions)
    (condition-case err (funcall fn s method params)
      (error (message "aob-acp-notification-functions: %S" err))))
  (pcase method
    ("session/update"
     (let ((u (aob-acp--own-step s (plist-get params :update))))
       (pcase (plist-get u :sessionUpdate)
         ("tool_call" (aob-note-progress s)
          (if (gethash (plist-get u :toolCallId) (aob-acp--tools s))
              (aob-acp--tool-update s u)
            (aob-acp--tool-call s u)))
         ("tool_call_update"
          (when (member (plist-get u :status) '("completed" "failed"))
            (aob-note-progress s))
          (aob-acp--tool-update s u))
         ("agent_message_chunk"
          (if-let* ((id (aob-session-ref s :local-compaction)))
              (aob-acp--compaction
               s (list :sessionUpdate "compaction_summary_chunk"
                       :compactionId id :content (plist-get u :content)))
            (aob-acp--chunk s :msg-ev 'message u)))
         ("agent_thought_chunk" (aob-acp--chunk s :thought-ev 'thought u))
         ;; only a load or a replay speaks for you: a live turn's prompt is
         ;; already in the trace, from when it was sent
         ("user_message_chunk"
          (when (eq (aob-session-state s) 'starting)
            (aob-acp--chunk s :user-ev 'prompt u)))
         ("notice" (aob-acp--notice s u))
         ((or "compaction_update" "compaction_summary_chunk")
          (aob-acp--compaction s u))
         ("available_commands_update"
          (aob-session-put s :commands (aob-acp--with-local-commands
                                        s (plist-get u :availableCommands))))
         ("current_mode_update"
          (aob-session-put s :mode-id (plist-get u :currentModeId))
          (aob-event s 'state
                     :title (format "mode: %s" (plist-get u :currentModeId))))
         ("usage_update"
          (aob-session-put s :ctx-used (plist-get u :used))
          (aob-session-put s :ctx-size (plist-get u :size))
          (aob-session-put s :usage-latest u)
          (when-let* ((live (plist-get (plist-get u :_meta) :_claude/model)))
            (aob-session-put s :model-live live))
          (when-let* ((cost (plist-get u :cost)))
            (aob-usage-note-cost s (plist-get cost :amount) (plist-get cost :currency)
                                 (plist-get (plist-get u :_meta) :_claude/origin)))
          (when (fboundp 'ygg-usage-note) (ygg-usage-note s u))
          (unless (aob-subagent-native-p s)
            (aob-acp--autocompact-check s))
          (aob-session-kid-changed s)
          (aob--dirty s))
         ("subagent_spawned" (aob-acp--subagent-spawned s u))
         ("subagent_state_update" (aob-acp--subagent-ended s u))
         ("subagent_update" (aob-acp--subagent-update s u))
         ((or "async_task_spawned" "async_task_progress" "async_task_state_update")
          (aob-acp--async-task s u))
         ("session_info_update"
          ;; the goal rides this update with no title of its own — writing
          ;; the absent title through would erase the session's
          (when-let* ((title (plist-get u :title)))
            (aob-session-put s :info-title title)
            (aob-acp--auto-name s 'title title))
          (when-let* ((at (plist-get u :updatedAt)))
            (aob-session-put s :updated-at at))
          (when-let* ((meta (plist-get u :_meta)))
            (aob-session-put s :goal (plist-get meta :goal))
            (aob-acp--auto-name s 'goal (aob-acp--goal-text (plist-get meta :goal)))
            (aob--dirty s)))
         ("config_option_update"
          (aob-acp--config-apply s (plist-get u :configOptions)))
         ;; claude sends "plan"; codex/hermes stream "plan_update" (and
         ;; "plan_removed" to clear); entries sit flat or under plan
         ((or "plan" "plan_update")
          (when-let* ((plan (aob-acp--plan-of u)))
            (if-let* ((pid (aob-acp--parent-of u)))
                (aob-acp--sub-plan s plan pid)
              (aob-acp--plan s plan))))
         ("plan_removed"
          (if-let* ((pid (aob-acp--parent-of u)))
              (aob-acp--sub-plan s '(:entries nil) pid)
            (aob-acp--plan s '(:entries nil)))))))))

(defun aob-acp--subagent-spawned (s u)
  "Open the subagent U announces as a session S sent, and route its id there.
Only a connection that agreed to announce subagents is heard.  Hearing
one again, as a load replays it, finds its session already open."
  (when-let* ((proc (aob-acp--proc-of s))
              ((process-get proc 'aob-subagents))
              (sid (plist-get u :subagentSessionId)))
    (remhash sid (aob-acp--gone proc))
    (let ((kid (aob-subagent-announced s sid (plist-get u :name)
                                       (plist-get u :task) (plist-get u :prompt))))
      (aob-session-put kid :acp-conn proc)
      (aob-acp--register proc sid kid))))

(defun aob-acp--subagent-ended (s u)
  "End the subagent U names: done when it completed or was cancelled, and
failed however else it stopped; a cancel or a lost agent says so."
  (when-let* ((proc (aob-acp--proc-of s))
              ((process-get proc 'aob-subagents))
              (kid (gethash (plist-get u :subagentSessionId)
                            (aob-acp--proc-sessions proc))))
    (pcase (plist-get u :state)
      ("completed" (aob-subagent-announced-end kid 'done))
      ("cancelled" (aob-subagent-announced-end kid 'done "cancelled"))
      ("disconnected" (aob-subagent-announced-end kid 'failed "disconnected"))
      (_ (aob-subagent-announced-end kid 'failed)))))

(defun aob-acp--subagent-update (s u)
  "Read the draft's single subagent update U as the two it replaces.
An id not yet heard is a spawn; a state other than running ends it."
  (when-let* ((proc (aob-acp--proc-of s))
              (sid (plist-get u :subagentSessionId)))
    (unless (gethash sid (aob-acp--proc-sessions proc))
      (aob-acp--subagent-spawned s u))
    (when-let* ((state (plist-get u :state))
                ((not (equal state "running"))))
      (aob-acp--subagent-ended s u))))

(defun aob-acp--subagent-removed (s)
  "Answer the requests the announced subagent S still holds, and stop
routing its id: its agent is owed a reply either way."
  (when-let* (((eq (aob-session-backend s) 'native-subagent))
              (proc (aob-session-ref s :acp-conn)))
    (dolist (d (aob-session-decisions s))
      (ignore-errors
        (aob-acp--cancel-held s d)))
    (when-let* ((sid (aob-session-ref s :acp-id)))
      (puthash sid t (aob-acp--gone proc)))
    (aob-acp--deregister proc s)))

(defun aob-acp--gone (proc)
  "PROC's table of the subagent ids it routed and has let go of.
Their agent may still speak for them; nobody here is left to listen."
  (or (process-get proc 'aob-gone)
      (let ((h (make-hash-table :test #'equal)))
        (process-put proc 'aob-gone h)
        h)))

(add-hook 'aob-session-removed-hook #'aob-acp--subagent-removed)

(defvar aob-acp--command-map nil)

(defun aob-acp--annotate-command (cand)
  (when-let* ((c (cdr (assoc cand aob-acp--command-map))))
    (concat (propertize " " 'display '(space :align-to 24))
            (propertize (or (plist-get c :description) "")
                        'face 'completions-annotations))))

(defun aob-acp-command (s)
  "Run one of S's advertised slash commands (skills surface here too)."
  (interactive (list (aob-target)))
  (let ((cmds (aob-session-ref s :commands)))
    (unless cmds (user-error "aob: %s advertises no commands" (aob-session-name s)))
    (let* ((aob-acp--command-map
            (mapcar (lambda (c) (cons (plist-get c :name) c)) cmds))
           (table (lambda (str pred action)
                    (if (eq action 'metadata)
                        '(metadata (category . aob-command)
                                   (annotation-function . aob-acp--annotate-command))
                      (complete-with-action action (mapcar #'car aob-acp--command-map)
                                            str pred))))
           (name (completing-read "Command: " table nil t))
           (args (read-string (format "/%s " name))))
      (aob-prompt s (string-trim (format "/%s %s" name args))))))

;;; Config options — the agent's advertised pickers (model, effort,
;;; fast mode); `session/set_config_option' switches, updates stream
;;; back as config_option_update

(defun aob-acp--config-values (option)
  "OPTION's selectable values, flattened through option groups."
  (apply #'append
         (mapcar (lambda (v)
                   (if (plist-get v :options) (plist-get v :options) (list v)))
                 (plist-get option :options))))

(defun aob-acp--model-info (s)
  "S's model picker as (CURRENT-ID . VALUES), from its model config option.
Both adapters advertise one; the spec's models field and its
session/set_model never stabilized, and no longer reach here.  VALUES
are plists with :value/:name/:description."
  (when-let* ((opt (aob-acp--config-option s "model")))
    (cons (plist-get opt :currentValue) (aob-acp--config-values opt))))

(defun aob-acp--models-refresh (s)
  "Recompute the ambient model reading; a change becomes a state event."
  (when-let* ((info (aob-acp--model-info s)))
    (let* ((old (aob-session-ref s :model-id))
           (cur (car info))
           (hit (seq-find (lambda (v) (equal (plist-get v :value) cur))
                          (cdr info)))
           (name (or (plist-get hit :name) cur)))
      (aob-session-put s :model-id cur)
      (aob-session-put s :model-name name)
      (unless (equal old cur) (aob-session-put s :model-live nil))
      (aob-session-kid-changed s)
      (when (and old cur (not (equal old cur)))
        (aob-event s 'state :title (format "model: %s" name)
                   :via (aob-session-ref s :model-via))
        (aob-session-put s :model-via nil))
      (aob--dirty s))))

(defun aob-acp--config-apply (s opts)
  (aob-session-put s :config-options opts)
  (aob-acp--models-refresh s))

(defun aob-acp--config-option (s category)
  "S's advertised config option of CATEGORY, or the one whose id is CATEGORY."
  (seq-find (lambda (o) (or (equal (plist-get o :category) category)
                            (equal (plist-get o :id) category)))
            (aob-session-ref s :config-options)))

(defun aob-acp--declares-boolean-options-p ()
  "Whether the initialize request declares boolean config options."
  (let ((opts (plist-get (plist-get (aob-acp--client-capabilities) :session)
                         :configOptions)))
    (and (listp opts)
         (plist-member opts :boolean)
         (not (eq (plist-get opts :boolean) :null)))))

(defun aob-acp--config-params (s config-id value)
  "The set_config_option params that give S's CONFIG-ID VALUE, or nil.
A boolean goes typed and as a JSON boolean, and only when the client
declared it; nil is never sent, since it would go out as an object."
  (let* ((opt (seq-find (lambda (o) (equal (plist-get o :id) config-id))
                        (aob-session-ref s :config-options)))
         (type (plist-get opt :type))
         (boolean (if (stringp type) (equal type "boolean")
                    (memq value '(t :false))))
         (base (list :sessionId (aob-acp--acp-id s) :configId config-id)))
    (cond
     ((and boolean (not (aob-acp--declares-boolean-options-p)))
      (message "aob: %s option %s is boolean, which aob does not declare"
               (aob-session-name s) config-id)
      nil)
     (boolean
      (append base (list :type "boolean"
                         :value (if (memq value '(nil :false)) :false t))))
     ((null value)
      (message "aob: no value for %s option %s" (aob-session-name s) config-id)
      nil)
     (t (append base (list :value value))))))

(defun aob-acp--set-config (s config-id value &optional then)
  "Set S's CONFIG-ID to VALUE; THEN, if given, gets the (RES ERR) reply.
Non-nil when the request went out."
  (when-let* ((params (aob-acp--config-params s config-id value)))
    (aob-acp--request
     s "session/set_config_option" params
     (lambda (res err)
       (if err
           (message "aob: %s" (plist-get err :message))
         (aob-acp--config-apply s (plist-get res :configOptions))
         (message "aob: %s → %s" config-id
                  (or (plist-get (seq-find (lambda (o) (equal (plist-get o :id) config-id))
                                           (plist-get res :configOptions))
                                 :currentValue)
                      value)))
       (when then (funcall then res err))))
    t))

(defun aob-acp--model-landed (s model-id res err)
  "Trace where S's switch to MODEL-ID landed, when not where it was sent.
RES and ERR are the set_config_option reply.  A refusal, or a reply on
another model than an offered id asked for, is a warning naming the
model S is actually on; a name the agent resolved itself is no miss."
  (let* ((info (aob-acp--model-info s))
         (now (or (aob-session-ref s :model-name) (car info) "?")))
    (cond
     (err
      (aob-session-put s :model-via nil)
      (aob-event s 'state :warning t
                 :title (format "model: %s refused (%s) — still %s"
                                model-id (plist-get err :message) now)))
     ((and (plist-get res :configOptions)
           (not (equal (car info) model-id))
           (seq-find (lambda (v) (equal (plist-get v :value) model-id)) (cdr info)))
      (aob-session-put s :model-via nil)
      (aob-event s 'state :warning t
                 :title (format "model: asked for %s, on %s" model-id now))))))

(defun aob-acp--set-model (s model-id)
  "Switch S to MODEL-ID through its model config option."
  (cond
   ((aob-acp--config-option s "model")
    (aob-session-put s :model-via "session/set_config_option")
    (let (sent)
      (unwind-protect
          (setq sent (aob-acp--set-config
                      s (plist-get (aob-acp--config-option s "model") :id) model-id
                      (lambda (res err) (aob-acp--model-landed s model-id res err))))
        (unless sent (aob-session-put s :model-via nil)))))
   (t (message "aob: %s advertises no models" (aob-session-name s)))))

(defun aob-acp--model-pick (s info)
  (let* ((cur (car info))
         (cands (mapcar (lambda (v)
                          (cons (concat (or (plist-get v :name)
                                            (plist-get v :value))
                                        (when (equal (plist-get v :value) cur)
                                          "  ·current"))
                                (plist-get v :value)))
                        (cdr info)))
         (choice (completing-read
                  (format "Model (now %s): "
                          (or (aob-session-ref s :model-name) "?"))
                  (mapcar #'car cands) nil t)))
    (aob-acp--set-model s (cdr (assoc choice cands)))))

(defun aob-acp-model (s)
  "Check and switch S's model — the picker names the current one.
A session without a stored list heals itself first: ACP has no options
getter, but a no-op mode set makes the adapter return the full
configOptions in its response."
  (interactive (list (aob-target)))
  (let ((state (aob-session-state s)))
    (cond
     ((memq state '(failed dead))
      (user-error "aob: %s %s (%s) — SPC a c R respawns it"
                  (aob-session-name s) state
                  (or (aob-session-ref s :fail-reason) "no reason recorded")))
     ((and (aob-session-ref s :asleep) (not (aob-session-conn s)))
      (aob-acp--model-asleep s))
     ;; mid-handshake nothing is ingested yet, and a spawn with a first
     ;; turn is already working then — wait for the open, by what arrived
     ;; rather than by the state's name
     ((and (memq state '(starting working blocked))
           (not (and (aob-session-conn s) (aob-session-ref s :opened))))
      (message "aob: %s is still opening — the model picker will follow"
               (aob-session-name s))
      (letrec ((fn (lambda (s2 _old new)
                     (when (eq s2 s)
                       (cond
                        ((memq new '(failed dead))
                         (remove-hook 'aob-state-change-hook fn))
                        ((or (eq new 'idle) (aob-session-ref s :opened))
                         (remove-hook 'aob-state-change-hook fn)
                         (run-at-time 0 nil #'aob-acp-model s)))))))
        (add-hook 'aob-state-change-hook fn)))
     ((not (aob-session-conn s))
      (user-error "aob: %s is not running" (aob-session-name s)))
     (t (aob-acp--model-1 s)))))

(defun aob-acp--model-1 (s)
  (let ((info (aob-acp--model-info s)))
    (if (and info (cdr info))
        (aob-acp--model-pick s info)
      (let ((mode (aob-session-ref s :mode-id)))
        (unless mode
          (user-error "aob: %s advertises no models" (aob-session-name s)))
        (message "aob: fetching %s's model list…" (aob-session-name s))
        (aob-acp--request
         s "session/set_config_option"
         (list :sessionId (aob-acp--acp-id s) :configId "mode" :value mode)
         (lambda (res err)
           (if err
               (message "aob: %s advertises no models (%s)"
                        (aob-session-name s) (plist-get err :message))
             (aob-acp--config-apply s (plist-get res :configOptions))
             (let ((info (aob-acp--model-info s)))
               (if (and info (cdr info))
                   ;; a picker must not open from filter context
                   (run-at-time 0 nil #'aob-acp--model-pick s info)
                 (message "aob: %s advertises no models"
                          (aob-session-name s)))))))))))

(defun aob-acp--break-accum (s)
  (aob-session-put s :msg-ev nil)
  (aob-session-put s :thought-ev nil)
  (aob-session-put s :user-ev nil)
  (dolist (slot (aob-session-ref s :sub-accums))
    (aob-session-put s slot nil))
  (aob-session-put s :sub-accums nil))

(defun aob-acp--parent-of (u)
  "The tool call U belongs under, or nil when U is the agent\='s own words.

Three spellings for one fact.  The spec puts parentToolCallId at the
top of _meta; claude wrote parentToolUseId under a claudeCode key
before that existed and still does in places; and a tool call carries
its own parentToolUseId beside it.  Reading only one of them leaves
every subagent speaking as the agent that sent it, which is five
voices in the lead\='s transcript and no way to tell whose is whose."
  (let ((meta (plist-get u :_meta)))
    (or (plist-get (plist-get meta :claudeCode) :parentToolUseId)
        (plist-get meta :parentToolCallId)
        (plist-get meta :parentToolUseId))))

(defun aob-acp--own-step (s u)
  "U as the subagent S, when its agent announced it, takes it: as its own.
Claude stamps every step it routes to a subagent with the Agent call
that spawned it, a call S's sender never hears of.  Only a call S made
itself is one its steps nest under; any other stamp is S's own."
  (let ((parent (aob-acp--parent-of u)))
    (if (or (null parent)
            (not (aob-session-ref s :announced))
            (gethash parent (aob-acp--tools s)))
        u
      (cl-flet ((without (plist keys)
                  (cl-loop for (k v) on plist by #'cddr
                           unless (memq k keys) nconc (list k v))))
        (let ((meta (without (plist-get u :_meta) '(:parentToolCallId :parentToolUseId)))
              (claude (plist-get (plist-get u :_meta) :claudeCode)))
          (when claude
            (setq meta (plist-put meta :claudeCode (without claude '(:parentToolUseId)))))
          (plist-put (copy-sequence u) :_meta meta))))))

(defun aob-acp--link (uri name)
  "URI as a link named NAME, or by itself: RET visits a file, browses the rest."
  (let* ((path (cond ((not (stringp uri)) nil)
                     ((string-prefix-p "file://" uri)
                      (url-unhex-string (substring uri (length "file://"))))
                     ((file-name-absolute-p uri) uri)))
         (label (format "[[%s]]" (or name (and path (abbreviate-file-name path))
                                     uri "resource"))))
    (cond (path (propertize label 'aob-file (list path nil nil)
                            'font-lock-face 'link 'help-echo path))
          ((stringp uri) (buttonize label #'browse-url uri uri))
          (t label))))

(defun aob-acp--block-text (block)
  "BLOCK, a content block the agent sent, as the words a trace shows.
Only text is words; a trace that dropped the rest would read as a gap.
A picture or a sound is named where it stood, a link is one RET
follows, an embedded resource shows its own text under its name, and a
kind not known is named rather than lost."
  (let ((type (plist-get block :type)))
    (pcase type
      ("text" (plist-get block :text))
      ("image" "[[Image]]")
      ("audio" "[[Audio]]")
      ("resource_link"
       (aob-acp--link (plist-get block :uri)
                      (or (plist-get block :title) (plist-get block :name))))
      ("resource"
       (let* ((resource (plist-get block :resource))
              (text (plist-get resource :text)))
         (concat (aob-acp--link (plist-get resource :uri) nil)
                 (when (stringp text)
                   (concat "\n```\n" (string-trim-right text) "\n```\n")))))
      (_ (format "[[%s]]" (or type "?"))))))

(defun aob-acp--chunk (s slot type u)
  "Fold U's content into S's running message, thought or replayed prompt.
A subagent speaks on the same stream as the agent that sent it, so each
one accumulates under its own Task — otherwise five voices would land
in one paragraph, and the trace would show them as the main agent's.
Your words and the agent's never share an event: a replay that goes
from one to the other with no tool call between is still two turns.
A picture keeps its data on the event, for a trace that can draw it."
  (let* ((content (plist-get u :content))
         (parent (aob-acp--parent-of u))
         (slot (if parent (intern (format "%s@%s" slot parent)) slot))
         (ev (aob-session-ref s slot)))
    (if (eq type 'prompt)
        (unless ev (aob-acp--break-accum s))
      (aob-session-put s :user-ev nil))
    (unless ev
      (setq ev (apply #'aob-event s type :parent parent
                      (and (eq type 'prompt) (list :typed t))))
      (when (eq type 'message) (aob-note-progress s))
      (aob-session-put s slot ev)
      (when parent
        (aob-session-put s :sub-accums
                         (cons slot (aob-session-ref s :sub-accums)))))
    (aob-event-push-text ev (aob-acp--block-text content))
    (when (equal (plist-get content :type) "image")
      (plist-put ev :image-data (append (plist-get ev :image-data) (list content))))
    (aob-refresh-summary s ev)))

(defun aob-acp--notice (s u)
  "Put the notice U in S's trace as a status line, marked when it warns."
  (let ((title (plist-get u :title))
        (description (plist-get u :description)))
    (when (and (equal title "Model rerouted") (stringp description)
               (string-match " to \\([^ ]+\\) (" description))
      (aob-session-put s :model-live (match-string 1 description))
      (aob-session-kid-changed s))
    (aob-event s 'state
               :title (if (and (stringp description) (not (string-empty-p description)))
                          (format "%s: %s" title
                                  (replace-regexp-in-string "[ \t]*\n[ \t\n]*" " "
                                                            description))
                        title)
               :warning (and (member (plist-get u :severity) '("warning" "error")) t))))

(defun aob-acp--compaction (s u)
  "Show the compaction U speaks of as one status line in S's trace.
Each compaction is a line of its own, rewritten as it moves on; the
summary it keeps streams into the line's text, for TAB to open, and is
never read as the agent's words."
  (let* ((id (plist-get u :compactionId))
         (ev (aob-session-ref s :compaction-ev)))
    (unless (and ev (equal (plist-get ev :compaction-id) id))
      (setq ev (aob-event s 'state :title "compacting context" :compaction-id id))
      (aob-session-put s :compaction-ev ev))
    (if (equal (plist-get u :sessionUpdate) "compaction_summary_chunk")
        (aob-event-push-text ev (aob-acp--block-text (plist-get u :content)))
      (seq-doseq (block (plist-get u :summary))
        (aob-event-push-text ev (aob-acp--block-text block)))
      (pcase (plist-get u :status)
        ("completed" (plist-put ev :title "context compacted"))
        ("cancelled" (plist-put ev :title "compaction cancelled"))
        ("failed"
         (let ((err (plist-get u :error)))
           (plist-put ev :title (if (stringp err)
                                    (format "compaction failed: %s" err)
                                  "compaction failed"))
           (plist-put ev :warning t)))))
    (plist-put ev :line nil)
    (aob-refresh-summary s ev)))

(defun aob-acp--diff-stat (content)
  (let ((plus 0) (minus 0) (any nil))
    (dolist (c content)
      (when (equal (plist-get c :type) "diff")
        (setq any t)
        (cl-incf plus (1+ (cl-count ?\n (or (plist-get c :newText) ""))))
        (cl-incf minus (if (plist-get c :oldText)
                           (1+ (cl-count ?\n (plist-get c :oldText)))
                         0))))
    (when any (format "+%d −%d" plus minus))))

(defun aob-acp--tools (s)
  (or (aob-session-ref s :tools)
      (let ((h (make-hash-table :test #'equal)))
        (aob-session-put s :tools h)
        h)))

(defun aob-acp--threads (s)
  (or (aob-session-ref s :threads)
      (let ((h (make-hash-table :test #'equal)))
        (aob-session-put s :threads h)
        h)))

(defun aob-acp--codex-subagent (s u id)
  "How codex names a subagent in U, as (PARENT . NAME), or nil for none.
Two adapters, two dialects: claude stamps each child with the tool call
that spawned it, while codex gives every subagent activity an id of its
own and a thread they share.  Taking a thread's first activity as the
row the rest nest under turns the second dialect into the first, so
everything above this — rollups, the panel, the subagent's own trace —
never learns there was more than one."
  (when-let* ((sub (plist-get (plist-get (plist-get u :_meta) :codex) :subagent))
              (thread (plist-get sub :threadId)))
    (let ((head (gethash thread (aob-acp--threads s))))
      (if (and head (not (equal head id)))
          (cons head nil)
        (puthash thread id (aob-acp--threads s))
        (cons nil (file-name-nondirectory
                   (directory-file-name (or (plist-get sub :path) "subagent"))))))))

(defun aob-acp--codex-spawn-p (u)
  "Whether U is codex's spawn call, whose raw input holds the prompt it sent."
  (equal (plist-get (plist-get (plist-get (plist-get u :_meta) :codex) :collaboration) :tool)
         "spawnAgent"))

(defun aob-acp--brief-title (text)
  "The first line of TEXT that is more than a heading, or nil."
  (when (stringp text)
    (let ((case-fold-search nil))
      (seq-find (lambda (l) (not (or (string-empty-p l)
                                     (string-match-p "\\`#*[ \t]*[A-Z]+:?\\'" l))))
                (mapcar #'string-trim (split-string text "\n"))))))

(defun aob-acp--tool-name (u)
  "The tool U calls: the spec's name field, else claude's older _meta key."
  (let ((name (plist-get u :name)))
    (if (stringp name) name
      (plist-get (plist-get (plist-get u :_meta) :claudeCode) :toolName))))

(defun aob-acp--tool-call (s u)
  (aob-acp--break-accum s)
  (when (equal (plist-get u :kind) "edit")
    (dolist (loc (plist-get u :locations))
      (when-let* ((path (plist-get loc :path)))
        (aob-artifact-notice s path)))
    (dolist (c (plist-get u :content))
      (when-let* ((path (and (equal (plist-get c :type) "diff")
                             (plist-get c :path))))
        (aob-artifact-notice s path))))
  (let* ((meta (plist-get (plist-get u :_meta) :claudeCode))
         (raw (plist-get u :rawInput))
         (id (plist-get u :toolCallId))
         (codex (aob-acp--codex-subagent s u id))
         (spawn (aob-acp--codex-spawn-p u))
         ;; an Agent call is titled by its description once its input streams in
         (task (or (equal (plist-get u :title) "Task")
                   (eq (plist-get meta :subagent) t)
                   (member (aob-acp--tool-name u) '("Agent" "Task"))))
         (ev (aob-event s 'tool
                        :tool-id id
                        :kind (plist-get u :kind)
                        ;; a subagent deserves to be named by what it was
                        ;; sent to do (claude) or by which agent it is
                        ;; (codex) — never by the bare tool
                        :title (or (and task (or (plist-get raw :description)
                                                 (plist-get raw :prompt)))
                                   (and spawn (or (aob-acp--brief-title (plist-get raw :prompt))
                                                  "subagent"))
                                   (cdr codex)
                                   (aob-acp--tool-title u raw))
                        :raw raw
                        :subagent (and (or task spawn (cdr codex)) t)
                        :parent-model (and (or task spawn (cdr codex))
                                           (aob-session-model-now s))
                        :codex-spawn spawn
                        :subagent-type (and spawn (aob-acp--codex-role s raw))
                        :parent (or (aob-acp--parent-of u)
                                    (plist-get meta :parentToolUseId)
                                    (car codex))
                        :status (plist-get u :status)
                        :locations (plist-get u :locations)
                        :content (plist-get u :content)
                        :stat (aob-acp--diff-stat (plist-get u :content)))))
    (puthash (plist-get u :toolCallId) ev (aob-acp--tools s))
    (aob-acp--terminal-note ev u)
    (aob-acp--background-note s ev u)
    (aob-acp--child-note s ev nil)))

(defconst aob-acp--terminal-output-max 65536
  "Characters of what one command prints that its tool call keeps.")

(defun aob-acp--terminal-note (ev u)
  "Fold the command output U carries in its `_meta' into tool call EV.
Declared terminal_output_delta, both adapters send what a command
prints as chunks to append, and its exit status in terminal_exit, in
place of the text block that would otherwise carry the output."
  (let ((meta (plist-get u :_meta)))
    (when-let* ((chunk (plist-get (or (plist-get meta :terminal_output_delta)
                                      (plist-get meta :terminal_output))
                                  :data))
                ((stringp chunk))
                (kept (or (plist-get ev :terminal-output) ""))
                (room (- aob-acp--terminal-output-max (length kept)))
                ((> room 0)))
      (plist-put ev :terminal-output
                 (if (<= (length chunk) room)
                     (concat kept chunk)
                   (concat kept (substring chunk 0 room)
                           "\n[… output past this was not kept]"))))
    (when-let* ((code (plist-get (plist-get meta :terminal_exit) :exit_code))
                ((integerp code)))
      (plist-put ev :terminal-exit code))
    (when-let* ((cwd (plist-get (plist-get meta :terminal_info) :cwd))
                ((stringp cwd)))
      (plist-put ev :cwd cwd))))

(defun aob-acp--background-note (s ev u)
  "Mark EV as a command left running when U says its agent handed it off.
Claude's Bash returns at once for a command run in the background, the
call completed, and says only in its text which task holds the command
and the file its output goes to.  A load replaying S's history says it
too, of commands long gone, so nothing is marked while S is starting."
  (when (and (equal (plist-get ev :kind) "execute")
             (not (eq (aob-session-state s) 'starting))
             (plist-get (plist-get (plist-get (plist-get (plist-get u :_meta) :jetbrains)
                                              :air)
                                   :asyncTasks)
                        :backgrounded))
    (plist-put ev :background (or (plist-get ev :background) t)))
  (when-let* (((equal (plist-get ev :kind) "execute"))
              ((not (eq (aob-session-state s) 'starting)))
              (text (concat (plist-get (plist-get (plist-get u :_meta) :terminal_output_delta)
                                       :data)
                            (aob-acp--content-text (plist-get u :content))))
              ((string-match "Command running in background with ID: \\([^ .\n]+\\)\\." text)))
    (plist-put ev :background (match-string 1 text))
    (when (string-match "Output is being written to: \\(.+?\\)\\. You will be notified" text)
      (plist-put ev :output-file (match-string 1 text)))))

(defun aob-acp--content-text (content)
  "The text blocks of tool call CONTENT, joined."
  (mapconcat (lambda (c)
               (let ((inner (plist-get c :content)))
                 (if (and (equal (plist-get c :type) "content")
                          (stringp (plist-get inner :text)))
                     (plist-get inner :text)
                   "")))
             content ""))

(defun aob-acp--shell-changed (s ev)
  "Redraw tool call EV of S, leaving what S is doing now as it was."
  (plist-put ev :line nil)
  (run-hook-with-args 'aob-event-change-functions s ev)
  (aob--dirty s))

(defun aob-acp-shell-end (s ev state)
  "Say the command of tool call EV in S ended in STATE, and redraw it.
STATE is stopped, completed, failed or gone; the first word of an end
is the one kept."
  (unless (plist-get ev :shell-end)
    (plist-put ev :shell-end state)
    (plist-put ev :shell-end-ts (float-time))
    (aob-acp--shell-changed s ev)))

(defun aob-acp--async-task (s u)
  "Fold async task update U into the command it runs in S.
An agent told it may report background work names each task's tool call
when it starts; a task without one is not guessed at."
  (let* ((tools (aob-acp--tools s))
         (id (plist-get u :asyncTaskId))
         (ev (or (gethash (plist-get u :toolCallId) tools)
                 (seq-find (lambda (e) (equal (plist-get e :task-id) id))
                           (hash-table-values tools)))))
    (when ev
      (plist-put ev :task-id id)
      (plist-put ev :background (or (plist-get ev :background) t))
      (when-let* ((file (plist-get u :outputFilePath)))
        (plist-put ev :output-file file))
      (when-let* ((state (plist-get u :state))
                  ((member state '("completed" "failed" "stopped"))))
        (aob-acp-shell-end s ev (intern state)))
      (aob-acp--shell-changed s ev))))

(defun aob-acp-task-stoppable-p (s ev)
  "Non-nil when S's agent itself can stop the background task of EV."
  (and (plist-get ev :task-id)
       (not (plist-get ev :shell-end))
       (when-let* ((proc (aob-acp--proc-of s)))
         (process-get proc 'aob-async-tasks-offered))))

(defun aob-acp-stop-task (s ev cb)
  "Ask S's agent to stop the background task of EV; CB takes (STOPPED ERR).
The task's own end usually arrives as an update before this answer does."
  (aob-acp--request-proc
   (aob-acp--proc-of s) "_session/async_task/stop"
   (list :sessionId (aob-acp--acp-id s) :asyncTaskId (plist-get ev :task-id))
   (lambda (res err)
     (let ((stopped (and (not err) (eq t (plist-get res :stopped)))))
       (when stopped (aob-acp-shell-end s ev 'stopped))
       (funcall cb stopped err)))
   s))

(defun aob-acp--background-running (s)
  "Mark each command of S still running at the end of its turn as background."
  (maphash (lambda (_ ev)
             (when (and (equal (plist-get ev :kind) "execute")
                        (aob-acp--child-live-p (plist-get ev :status)))
               (plist-put ev :background (or (plist-get ev :background) t))
               (plist-put ev :line nil)))
           (aob-acp--tools s)))

(defconst aob-acp-tool-detail-keys
  '(:command :pattern :query :url :file_path :path :filePath :description)
  "Raw-input keys worth reading, in the order a tool line prefers them.")

(defun aob-acp--tool-title (u raw)
  "What a tool line says: the tool, and the one thing it is doing.
An adapter sends the tool\='s name and leaves the command, the path or
the pattern in its raw input, so a trace of a working agent reads as a
column of the word bash."
  (let* ((title (plist-get u :title))
         (detail (and (listp raw)
                      (seq-some (lambda (k)
                                  (let ((v (plist-get raw k)))
                                    (and (stringp v)
                                         (not (string-empty-p (string-trim v)))
                                         v)))
                                aob-acp-tool-detail-keys)))
         (one (and detail (aob--first-line detail 110))))
    (cond ((null one) title)
          ((null title) one)
          ;; the adapter that already names what it is doing is left alone
          ((string-search one title) title)
          (t (concat title "  " one)))))

(defun aob-acp--tool-update (s u)
  (if-let* ((ev (gethash (plist-get u :toolCallId) (aob-acp--tools s))))
      (let ((old (plist-get ev :status)))
        (when-let* ((raw (plist-get u :rawInput))) (plist-put ev :raw raw))
        (dolist (key '(:kind :title :status :locations :content :rawOutput))
          (when-let* ((val (plist-get u key))
                      ;; updates re-send the bare tool name; a subagent
                      ;; already traded it for what it was sent to do
                      ((not (and (eq key :title) (plist-get ev :subagent)
                                 (member val '("Task" "spawnAgent"))))))
            (plist-put ev key
                       (if (eq key :title)
                           (aob-acp--tool-title u (plist-get ev :raw))
                         val))))
        (when-let* (((plist-get ev :subagent))
                    ((member (plist-get ev :title) '(nil "Task")))
                    (raw (plist-get ev :raw))
                    ((listp raw))
                    (named (or (plist-get raw :description) (plist-get raw :prompt))))
          (plist-put ev :title named))
        (when-let* (((plist-get ev :codex-spawn))
                    (role (aob-acp--codex-role s (plist-get ev :raw))))
          (plist-put ev :subagent-type role))
        (aob-acp--terminal-note ev u)
        (aob-acp--background-note s ev u)
        (when (and (plist-get ev :ended) (aob-acp--child-live-p (plist-get u :status)))
          (aob-event-revive s ev))
        (when-let* ((st (aob-acp--diff-stat (plist-get ev :content))))
          (plist-put ev :stat st))
        (when (and (member (plist-get ev :status) '("completed" "failed"))
                   (not (plist-get ev :done-ts)))
          (plist-put ev :done-ts (float-time)))
        (plist-put ev :line nil)
        (aob-acp--child-note s ev old)
        (when (and (plist-get ev :children)
                   (member (plist-get ev :status) '("completed" "failed"))
                   (not (member old '("completed" "failed"))))
          (aob-acp--collapse-task s ev))
        (aob-refresh-summary s ev))
    (aob-acp--tool-call s u)))

(defun aob-acp--child-live-p (status)
  (member status '("pending" "in_progress")))

(defun aob-acp--child-note (s ev old)
  "Roll EV's change up into its parent Task, O(1) per child event.
Counts only: what a subagent is reading this second is its own business,
and keeping none of it is also what stops every child event from
redrawing the parent's line."
  (when-let* ((pid (plist-get ev :parent))
              (parent (gethash pid (aob-acp--tools s))))
    (let ((new (plist-get ev :status))
          (moved nil))
      (unless old
        (plist-put parent :children (1+ (or (plist-get parent :children) 0)))
        (setq moved t))
      (when (and (aob-acp--child-live-p new)
                 (not (and old (aob-acp--child-live-p old))))
        (plist-put parent :child-live
                   (1+ (or (plist-get parent :child-live) 0)))
        (setq moved t))
      (when (and old (aob-acp--child-live-p old)
                 (not (aob-acp--child-live-p new)))
        (plist-put parent :child-live
                   (max 0 (1- (or (plist-get parent :child-live) 0))))
        (setq moved t))
      (when (and (equal new "failed") (not (equal old "failed")))
        (plist-put parent :child-fail
                   (1+ (or (plist-get parent :child-fail) 0)))
        (aob-session-put s :turn-fails
                         (1+ (or (aob-session-ref s :turn-fails) 0)))
        (setq moved t))
      (when moved
        (plist-put parent :line nil)
        (aob-refresh-summary s parent)))))

(defun aob-acp--collapse-task (s ev)
  "A finished Task keeps its rollup and report; its children leave the ring."
  (let* ((tid (plist-get ev :tool-id))
         (plus 0) (minus 0) (n 0) locs evs)
    (setf (aob-session-events s)
          (seq-remove
           (lambda (e)
             (when (equal (plist-get e :parent) tid)
               (cl-incf n)
               (push e evs)
               (setq locs (append (plist-get e :locations) locs))
               (when-let* ((st (plist-get e :stat))
                           ((string-match "\\+\\([0-9]+\\) −\\([0-9]+\\)" st)))
                 (cl-incf plus (string-to-number (match-string 1 st)))
                 (cl-incf minus (string-to-number (match-string 2 st))))
               (remhash (plist-get e :tool-id) (aob-acp--tools s))
               t))
           (aob-session-events s)))
    (cl-decf (aob-session-nevents s) n)
    (when locs (plist-put ev :child-locs locs))
    (when evs (plist-put ev :child-events (nreverse evs)))
    (when (> (+ plus minus) 0)
      (plist-put ev :child-stat (format "+%d −%d" plus minus)))
    (plist-put ev :child-live 0)
    (plist-put ev :line nil)))

(defun aob-acp--plan-of (u)
  "U's plan as (:entries ENTRIES), or nil when its shape is not one known.
The unstable plan_update nests the entries under plan; the stable plan
update and older adapters carry them flat."
  (let ((plan (plist-get u :plan)))
    (cond ((and (consp plan) (plist-member plan :entries))
           (list :entries (plist-get plan :entries)))
          ((plist-member u :entries)
           (list :entries (plist-get u :entries))))))

(defun aob-acp--plan (s u)
  (let ((entries (plist-get u :entries))
        (ev (aob-session-ref s :plan-ev)))
    ;; the whole list rides every todo write — an adapter that re-sends it
    ;; unchanged (claude sends each parsed item twice) must not tick a view
    (unless (equal entries (and ev (plist-get ev :entries)))
      (aob-session-put s :plan-tick
                       (1+ (or (aob-session-ref s :plan-tick) 0)))
      (if (null entries)
          (aob-session-put s :plan-ev nil)
        (let* ((done (seq-count (lambda (e) (equal (plist-get e :status) "completed"))
                                entries))
               (cur (seq-find (lambda (e) (equal (plist-get e :status) "in_progress"))
                              entries))
               (title (format "plan %d/%d%s" done (length entries)
                              (if cur (concat " · " (plist-get cur :content)) ""))))
          ;; only the newest event can be rewritten in place: once work
          ;; happened after it — or eviction took it off the ring — the
          ;; changed plan is news and lands where it changed
          (if (and ev (eq ev (car (aob-session-events s))))
              (progn (plist-put ev :title title)
                     (plist-put ev :entries entries)
                     (plist-put ev :line nil)
                     (aob-refresh-summary s ev))
            (aob-session-put s :plan-ev
                             (aob-event s 'plan :title title :entries entries))))))))

(defun aob-acp--sub-plan (s u pid)
  "A subagent's plan U, under the call PID that sent it.
It is the subagent's own, so it neither replaces S's plan nor reaches
S's todo list; its event rides with the subagent's other steps."
  (let* ((slot (intern (format ":plan-ev@%s" pid)))
         (ev (aob-session-ref s slot))
         (entries (plist-get u :entries)))
    (unless (if ev (equal entries (plist-get ev :entries)) (null entries))
      (let ((title (format "plan %d/%d"
                           (seq-count (lambda (e) (equal (plist-get e :status) "completed"))
                                      entries)
                           (length entries))))
        (if (and ev (eq ev (car (aob-session-events s))))
            (progn (plist-put ev :title title)
                   (plist-put ev :entries entries)
                   (plist-put ev :line nil)
                   (aob-refresh-summary s ev))
          (aob-session-put s slot
                           (aob-event s 'plan :parent pid :title title :entries entries)))))))

;;; Verbs (backend side)

(defun aob-acp--acp-id (s)
  (or (aob-session-ref s :acp-id)
      (user-error "aob: %s has no ACP session yet" (aob-session-name s))))

(defun aob-acp--queue (s text atts)
  ;; the queued prompt is an event from the start — visible dimmed in the
  ;; trace, and as blurb/»N ambiently — then promoted in place on flush
  (aob-session-put s :queued
                   (append (aob-session-ref s :queued)
                           (list (list text atts
                                       (aob-event s 'prompt :text text
                                                  :title (when atts
                                                           (format "+%d image(s)"
                                                                   (length atts)))
                                                  :typed aob-prompt-typed
                                                  :told aob-told-pending
                                                  :status "queued")))))
  (run-hook-with-args 'aob-queue-change-hook s))

(defun aob-acp--flush-queue (s)
  (when-let* ((q (aob-session-ref s :queued)))
    (aob-session-put s :queued nil)
    ;; a queued prompt is written before the session is open; a restored
    ;; one then replays its history on top of it, so the words you just
    ;; typed would sit above a conversation from hours earlier.  They go
    ;; out now, so they are the newest thing said.
    (let ((events (aob-session-events s))
          (moved nil))
      (dolist (e q)
        (when-let* ((ev (nth 2 e)))
          (plist-put ev :status nil)
          (plist-put ev :line nil)
          (when (memq ev events)
            (setq events (delq ev events))
            (push ev moved))))
      (setf (aob-session-events s) (append moved events)))
    (run-hook-with-args 'aob-queue-change-hook s)
    (let ((aob-told-pending (mapcan (lambda (e) (copy-sequence (plist-get (nth 2 e) :told)))
                                    q)))
      (aob-acp--prompt-1 s (mapconcat #'car q "\n\n")
                         (apply #'append (mapcar #'cadr q))
                         'queued))))

(defun aob-acp--mime (file)
  (pcase (downcase (or (file-name-extension file) ""))
    ("png" "image/png")
    ((or "jpg" "jpeg") "image/jpeg")
    ("gif" "image/gif")
    ("webp" "image/webp")
    (_ "application/octet-stream")))

(defun aob-acp--file-mentions (text dir)
  "Alist of (TOKEN . ABS) for @path mentions in TEXT that exist under DIR.
The existence check is what keeps a bare foo@bar.com from becoming a link."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "@\\([^@[:space:]]+\\)" nil t)
        (let* ((tok (match-string 1))
               (abs (expand-file-name tok dir)))
          (unless (file-exists-p abs)
            (let ((trimmed (replace-regexp-in-string "[[:punct:]]+\\'" "" tok)))
              (when (and (not (equal trimmed tok)) (> (length trimmed) 0)
                         (file-exists-p (expand-file-name trimmed dir)))
                (setq tok trimmed abs (expand-file-name trimmed dir)))))
          (when (and (file-exists-p abs) (not (gethash abs seen)))
            (puthash abs t seen)
            (push (cons tok abs) out)))))
    (nreverse out)))

(defcustom aob-acp-embed-limit 60000
  "Largest mention carried as its own text rather than as a link.
Past this the file goes as a resource_link, and the agent reads what
it needs of it: an ordinary source file fits, and a mention must not
spend the turn on one file."
  :type 'integer :group 'aob)

(defun aob-acp--text-mime (file)
  (pcase (downcase (or (file-name-extension file) ""))
    ("py" "text/x-python")
    ("el" "text/x-emacs-lisp")
    ((or "js" "mjs" "cjs" "jsx") "text/javascript")
    ((or "ts" "tsx") "text/typescript")
    ((or "yml" "yaml") "text/yaml")
    ("json" "application/json")
    ("md" "text/markdown")
    ("html" "text/html")
    ("css" "text/css")
    ("sh" "text/x-shellscript")
    (_ "text/plain")))

(defun aob-acp--embeddable-text (abs)
  "ABS as text the wire can carry, or nil when it has to stay a link.
A live buffer beats the file on disk — unsaved edits are the very thing
the mention meant.  Undecodable bytes are refused outright: one of them
signals out of `json-serialize', and losing the prompt is worse than
losing the freshness."
  (when-let* ((text (if-let* ((buf (find-buffer-visiting abs)))
                        (with-current-buffer buf
                          (when (<= (buffer-size) aob-acp-embed-limit)
                            (save-restriction
                              (widen)
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
                      (when (and (file-regular-p abs)
                                 (<= (file-attribute-size (file-attributes abs))
                                     aob-acp-embed-limit))
                        (with-temp-buffer
                          (insert-file-contents abs)
                          (buffer-string))))))
    (unless (string-match-p "[\x3FFF80-\x3FFFFF]" text) text)))

(defun aob-acp-embeds-p (s)
  "Non-nil when this agent takes content inline and not only paths to it.
Public because what a caller puts in a prompt depends on it: a file
named to an agent that embeds is handed over as a resource of its own,
and spelled out as text to one that does not."
  (plist-get (plist-get (aob-session-ref s :agent-caps) :promptCapabilities)
             :embeddedContext))

(defun aob-acp--mention-block (mention embed &optional s)
  "MENTION as a content block: its own text when EMBED, else a link to it.
Given S, a file S was already handed unchanged goes as a link too."
  (let ((abs (cdr mention)))
    (or (and embed
             (when-let* ((text (aob-acp--embeddable-text abs))
                         ((not (and s aob-dedupe-context
                                    (aob-told-p s :embeds-told abs
                                                (secure-hash 'sha1 text))))))
               (list :type "resource"
                     :resource (list :uri (aob-acp--file-uri abs)
                                     :mimeType (aob-acp--text-mime abs)
                                     :text text))))
        (list :type "resource_link"
              :uri (aob-acp--file-uri abs)
              :name (car mention)))))

(defun aob-acp--file-uri (abs)
  "ABS as a file URI the agent resolves: its own path, percent-encoded.
The Emacs spelling rides along as a property, so what was sent can be
matched back to the file here."
  (propertize (concat "file://"
                      (mapconcat #'url-hexify-string
                                 (split-string (aob-acp--wire-dir abs) "/")
                                 "/"))
              'aob-file abs))

(defun aob-acp--uri-file (s uri)
  "The Emacs name of the file URI names, as S reaches it."
  (or (get-text-property 0 'aob-file uri)
      (concat (or (file-remote-p (aob-session-dir s)) "")
              (decode-coding-string
               (url-unhex-string (string-remove-prefix "file://" uri))
               'utf-8))))

(defun aob-acp--tell-embeds (s blocks)
  "Note on S each file BLOCKS handed over whole, once the prompt landed."
  (seq-doseq (b blocks)
    (let* ((res (plist-get b :resource))
           (uri (plist-get res :uri)))
      (when (and (equal (plist-get b :type) "resource") (stringp uri)
                 (string-prefix-p "file://" uri) (plist-get res :text))
        (aob-tell s :embeds-told (aob-acp--uri-file s uri)
                  (secure-hash 'sha1 (plist-get res :text)))))))

(defun aob-acp--content-blocks (text atts &optional dir embed s)
  (apply #'vector
         (append
          (list (list :type "text" :text text))
          (mapcar (lambda (f)
                    (list :type "image"
                          :mimeType (aob-acp--mime f)
                          :data (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally f)
                                  (base64-encode-string (buffer-string) t))))
                  atts)
          (mapcar (lambda (m) (aob-acp--mention-block m embed s))
                  (and dir (aob-acp--file-mentions text dir))))))

(defun aob-acp--prompt (s text &optional atts)
  ;; prompting is never destructive: mid-turn (or mid-handshake, e.g. a
  ;; just-resumed session) it queues; interject is the explicit steer
  (cond
   ((aob-acp--local-command s text)
    (funcall (aob-acp--local-command s text) s))
   ((memq (aob-session-state s) '(working starting))
    (aob-acp--queue s text atts)
    (message "aob: queued for %s" (aob-session-name s)))
   (t (aob-acp--prompt-1 s text atts))))

(defun aob-acp--clear-p (text)
  "Non-nil when TEXT is the /clear slash command (context reset)."
  (and text (string-match-p "\\`/clear\\(?:[ \t].*\\)?\\'" (string-trim text))))

(defun aob-acp--span-id ()
  "A fresh 16-hex span id: one turn's own leg of a trace."
  (format "%08x%08x" (random (ash 1 32)) (random (ash 1 32))))

(defun aob-acp--prompt-meta (s)
  "The `_meta' a prompt to S carries, or nil when it carries none.
A session given a `:trace-id' sends every turn under one W3C
`traceparent' with a span of its own, and the span is kept on the session
so what went out can be recorded beside it.  `:prompt-meta' holds the
caller's own keys, which outlive the turn.  Minted here rather than by
the caller, so a prompt that waited in the queue still gets the span of
the turn it actually opens."
  (let ((trace (aob-session-ref s :trace-id)))
    (append
     (when trace
       (let ((span (aob-acp--span-id)))
         (aob-session-put s :span span)
         (list :traceparent (format "00-%s-%s-01" trace span))))
     (aob-session-ref s :prompt-meta))))

(defconst aob-acp--stop-warnings
  '(("refusal" . "the model refused")
    ("max_tokens" . "hit the token limit")
    ("max_turn_requests" . "hit the turn request limit"))
  "Stop reasons that cut a turn short, with what the trace says of each.")

(defvar aob-acp--overflow-retry nil
  "Non-nil while resending a prompt after an overflow compact.")

(defun aob-acp--image-paths (s text atts)
  "TEXT and ATTS as S's agent takes them, as (TEXT . ATTS).
An agent that never declared image support gets paths, not blocks it
can't parse: the reference survives, and the demotion is said."
  (if (and atts
           (not (plist-get (plist-get (aob-session-ref s :agent-caps)
                                      :promptCapabilities)
                           :image)))
      (progn
        (message "aob: %s takes no images — attached as paths"
                 (aob-session-name s))
        (cons (concat text "\n"
                      (mapconcat (lambda (f) (format "[image: %s]" f))
                                 atts "\n"))
              nil))
    (cons text atts)))

(defun aob-acp--prompt-1 (s text &optional atts queued)
  (aob-acp--auto-name s 'prompt text)
  (aob-acp--break-accum s)
  (pcase-let ((`(,said . ,imgs) (aob-acp--image-paths s text atts)))
    (setq text said atts imgs))
  (aob-session-put s :turn-fails nil)
  (aob-session-put s :turn-error nil)
  (aob-session-put s :stop-warning nil)
  ;; codex says nothing when a turn runs on the picked model again
  (aob-session-put s :model-live nil)
  (aob-session-kid-changed s)
  (unless queued
    ;; the tokens you wrote come back as tokens: what you sent is what the
    ;; trace shows, counted in the same [[Image]] the compose buffer used
    (aob-event s 'prompt :text text :images (length atts) :image-files atts
               :typed aob-prompt-typed))
  (aob-set-state s 'working)
  (aob-acp--local-compaction-begin s text)
  (let* ((stamp (aob-turn-begin s))
         (retry aob-acp--overflow-retry)
         (told aob-told-pending)
         (blocks (aob-acp--content-blocks
                  text atts (or (aob-session-dir s) (aob-session-project s))
                  (aob-acp-embeds-p s) s)))
    (aob-acp--request
     s "session/prompt"
     (append
      (list :sessionId (aob-acp--acp-id s)
            :prompt (if-let* ((place (aob-acp--place-block s)))
                        (vconcat (list place) blocks)
                      blocks))
      (when-let* ((meta (aob-acp--prompt-meta s))) (list :_meta meta)))
     (lambda (res err)
       (aob-acp--break-accum s)
       (aob-acp--local-compaction-end s res err)
       (unless err
         (aob-tell-all s told)
         (aob-acp--tell-embeds s blocks))
       (let* ((cost (and (eql stamp (aob-session-ref s :turn-start))
                         (aob-session-ref s :turn-cost)))
              (secs (aob-turn-end s stamp)))
         (if err
             (unless (aob-acp--overflow-handle s text atts err retry told)
               ;; the flag first: the idle transition runs hooks (workflow
               ;; advance) that must see this turn failed
               (aob-session-put s :turn-error t)
               (aob-session-settle-subagents s)
               (aob-set-state s 'idle)
               (if (aob-acp--auth-error-p err)
                   (aob-acp--auth-offer s err nil)
                 (aob-event s 'error :title (plist-get err :message))))
           ;; a /compact turn reports totalTokens 0 — never clobber the last
           ;; real reading with it
           (let ((usage (plist-get res :usage))
                 (warning (cdr (assoc (plist-get res :stopReason)
                                      aob-acp--stop-warnings))))
             (when (and usage (> (or (plist-get usage :totalTokens) 0) 0))
               (aob-session-put s :usage usage)
               (aob-session-put s :usage-latest usage)
               (aob-usage-note-turn s usage)
               (when (fboundp 'ygg-usage-note) (ygg-usage-note s usage)))
             (aob-session-put s :stop-warning warning)
             (aob-session-settle-subagents s)
             (unless (equal (plist-get res :stopReason) "cancelled")
               (aob-acp--background-running s))
             (aob-event s 'stop :reason (plist-get res :stopReason)
                        :warning warning
                        :tokens (let ((tk (plist-get usage :totalTokens)))
                                  (and tk (> tk 0) tk))
                        :usage usage :secs secs
                        :cost cost))
           ;; /clear wiped the agent's context — the trace history before it
           ;; now belongs to a conversation that no longer exists, so drop it
           ;; to a single marker (before the queue flushes new work in)
           (when (aob-acp--clear-p text)
             (setf (aob-session-events s) nil
                   (aob-session-nevents s) 0)
             (aob-session-put s :place-told nil)
             (aob-event s 'state :title "context cleared")
             (run-hook-with-args 'aob-acp-context-cleared-functions s))
           (aob-acp--compact-done s text res)
           (unless (aob-acp--overflow-resume s text res)
             (aob-set-state s 'idle)
             (aob-acp--flush-queue s))))))))

;;; Autosummarize — a big session sends itself /compact once its context
;;; window fills, so it never wedges at the limit (and a compacted session
;;; replays cheaper on resume).  Fires only between turns, once per fill.

(defcustom aob-acp-autocompact-reserve 16384
  "Auto-send /compact once fewer than this many tokens of a session's
context window are left (used > size - reserve).  Fires only when the
session is settled (idle, empty queue) and re-arms after usage falls well
below the trigger, so a session compacts at most once per fill.  With
aob-acp-autocompact-ratio also nil, autosummarize is off."
  :type '(choice (const :tag "off" nil) natnum) :group 'aob)

(defcustom aob-acp-autocompact-ratio nil
  "Optional extra ceiling: also auto-compact once context passes this
fraction of the window (used/size).  Whichever of this and
aob-acp-autocompact-reserve comes first fires."
  :type '(choice (const :tag "off" nil) number) :group 'aob)

(defcustom aob-acp-autocompact-overrides nil
  "Per-model autosummarize limits: an alist of (REGEXP . PLIST).
The first REGEXP matching the session's current model id wins; its
PLIST holds :reserve and/or :ratio, each replacing the global setting
it names (an explicit nil turns that limit off)."
  :type '(alist :key-type regexp :value-type plist) :group 'aob)

(defcustom aob-acp-overflow-regexp
  (concat "prompt \\(?:is \\)?too long\\|request_too_large\\|context[_ ]length"
          "\\|context window\\|context size\\|maximum context\\|input is too long")
  "An error from a prompt matching this (case-insensitively) means the
context is full: the session compacts and resends the prompt once.
Errors that also look like rate limits never count."
  :type 'regexp :group 'aob)

(defconst aob-acp--rate-limit-regexp
  "rate[_ ]limit\\|\\b429\\b\\|overloaded\\|too many requests"
  "Error text that marks a throttle, never a full context.")

(defcustom aob-acp-compact-instructions
  "Summarize under these headings: Goal; Constraints and Preferences; \
Progress (Done, In Progress, Blocked); Key Decisions; Next Steps; \
Critical Context; Relevant Files.  Keep exact file paths, identifiers, \
commands and error text verbatim.  Carry forward only requests that are \
still open.  Record the git state: branch, what is committed, what is \
pushed, what is not."
  "What autosummarize asks the summary to hold, sent after /compact.
Only agents in aob-acp-compact-instruction-agents get it; nil sends a
bare /compact to every agent."
  :type '(choice (const :tag "none" nil) string) :group 'aob)

(defcustom aob-acp-compact-instruction-agents '("claude")
  "Agents whose /compact takes instructions after the command.
Claude Code documents /compact with instructions; others are unconfirmed."
  :type '(repeat string) :group 'aob)

(defconst aob-acp--autocompact-rearm 0.10
  "Re-arm autosummarize once usage falls this fraction of the window
below the trigger.")

(defconst aob-acp--compact-files-max 40
  "At most this many paths in each file list /compact is told about.")

(defun aob-acp--ctx-ratio (s)
  "S's context fill as a fraction (used/size), or nil when unknown."
  (let ((used (aob-session-ref s :ctx-used))
        (size (aob-session-ref s :ctx-size)))
    (and (numberp used) (numberp size) (> size 0) (/ (float used) size))))

(defun aob-acp--offers-compact-p (s)
  (seq-find (lambda (c) (equal (plist-get c :name) "compact"))
            (aob-session-ref s :commands)))

(defconst aob-acp--compact-files-kept 200
  "At most this many paths a session remembers per list between compacts.")

(defun aob-acp--compact-note (s method params)
  "Remember the files a tool call in S reads or modifies, newest first."
  (when-let* (((equal method "session/update"))
              (u (plist-get params :update))
              ((member (plist-get u :sessionUpdate) '("tool_call" "tool_call_update")))
              (kind (or (plist-get u :kind)
                        (plist-get (gethash (plist-get u :toolCallId) (aob-acp--tools s))
                                   :kind)))
              (key (cond ((equal kind "read") :compact-read)
                         ((member kind '("edit" "delete" "move")) :compact-modified))))
    (let ((dir (file-name-as-directory
                (expand-file-name (or (aob-session-dir s) default-directory))))
          (paths (aob-session-ref s key)))
      (seq-doseq (p (vconcat (seq-map (lambda (l) (plist-get l :path))
                                      (plist-get u :locations))
                             (seq-map (lambda (c) (and (equal (plist-get c :type) "diff")
                                                       (plist-get c :path)))
                                      (plist-get u :content))))
        (when (stringp p)
          (let* ((abs (expand-file-name p dir))
                 (rel (if (string-prefix-p dir abs) (file-relative-name abs dir) abs)))
            (setq paths (cons rel (delete rel paths))))))
      (aob-session-put s key (seq-take paths aob-acp--compact-files-kept)))))

(add-hook 'aob-acp-notification-functions #'aob-acp--compact-note)

(defun aob-acp--touched-files (s)
  "The files S read and modified since its last compact, as (READ .
MODIFIED), newest first, each list capped; a file both read and
modified counts as modified."
  (let ((modified (aob-session-ref s :compact-modified)))
    (cons (seq-take (seq-remove (lambda (p) (member p modified))
                                (aob-session-ref s :compact-read))
                    aob-acp--compact-files-max)
          (seq-take modified aob-acp--compact-files-max))))

(defun aob-acp--compact-prompt (s)
  "The /compact S is sent, with instructions when its agent takes them."
  (let ((agent (aob-session-ref s :agent)))
    (if (and aob-acp-compact-instructions (stringp agent)
             (member (aob-acp-preset-agent agent) aob-acp-compact-instruction-agents))
        (let ((files (aob-acp--touched-files s)))
          (concat "/compact " aob-acp-compact-instructions
                  (when (car files)
                    (concat "\nFiles read: " (string-join (car files) ", ")))
                  (when (cdr files)
                    (concat "\nFiles modified: " (string-join (cdr files) ", ")))))
      "/compact")))

(defun aob-acp--compact-text-p (text)
  (and (stringp text) (string-match-p "\\`[ \t\n]*/compact\\b" text)))

(defun aob-acp--autocompact-limits (s)
  "S's (RESERVE . RATIO): the first override matching its model, else
the global settings."
  (let* ((model (or (aob-session-ref s :model-id) (car (aob-acp--model-info s))))
         (hit (and (stringp model)
                   (seq-find (lambda (o) (string-match-p (car o) model))
                             aob-acp-autocompact-overrides)))
         (o (cdr hit)))
    (cons (if (plist-member o :reserve) (plist-get o :reserve) aob-acp-autocompact-reserve)
          (if (plist-member o :ratio) (plist-get o :ratio) aob-acp-autocompact-ratio))))

(defun aob-acp--autocompact-trigger (s)
  "The token count at which S autocompacts, or nil when off or unknown."
  (let ((size (aob-session-ref s :ctx-size))
        (limits (aob-acp--autocompact-limits s)))
    (when (and (numberp size) (> size 0))
      (when-let* ((lines (delq nil (list (and (car limits) (- size (car limits)))
                                         (and (cdr limits) (* (cdr limits) size))))))
        (apply #'min lines)))))

(defun aob-acp--autocompact-check (s)
  "From S's context reading and settle state, arm or fire autosummarize."
  (when-let* ((trigger (aob-acp--autocompact-trigger s))
              (used (aob-session-ref s :ctx-used))
              ((numberp used))
              (r (aob-acp--ctx-ratio s)))
    (cond
     ;; recovered well below the line → ready to fire again next fill
     ((< used (- trigger (* aob-acp--autocompact-rearm (aob-session-ref s :ctx-size))))
      (aob-session-put s :autocompact-fired nil))
     ;; over the line, not yet fired this fill, and safe to fire now
     ((and (>= used trigger)
           (not (aob-session-ref s :autocompact-fired))
           (eq (aob-session-state s) 'idle)
           (null (aob-session-ref s :queued))
           ;; workflow workers compact at their coordinator's discretion
           (not (aob-session-ref s :wf-boss))
           (not (aob-session-ref s :hidden))
           (aob-acp--offers-compact-p s))
      (aob-session-put s :autocompact-fired t)
      (aob-event s 'state
                 :title (format "auto-compacting at %d%% context" (round (* 100 r))))
      (message "aob: %s auto-compacting (%d%% of context window)"
               (aob-session-name s) (round (* 100 r)))
      (aob-acp--prompt-1 s (aob-acp--compact-prompt s))))))

(defun aob-acp--overflow-p (err)
  "Whether the prompt error ERR says the context window is full."
  (let ((case-fold-search t)
        (said (format "%s %s" (or (plist-get err :message) "")
                      (or (plist-get err :data) ""))))
    (and (string-match-p aob-acp-overflow-regexp said)
         (not (string-match-p aob-acp--rate-limit-regexp said)))))

(defun aob-acp--overflow-handle (s text atts err retry &optional told)
  "When ERR is a full context on S's prompt TEXT, compact and hold TEXT
and ATTS for one resend; non-nil when handled.  RETRY marks a resend,
which never compacts again.  TOLD, what TEXT tells, is held with it."
  (cond
   ((aob-acp--compact-text-p text)
    (aob-session-put s :overflow-resend nil)
    nil)
   ((and (not retry) (aob-acp--overflow-p err) (aob-acp--offers-compact-p s))
    (aob-event s 'error :title (plist-get err :message))
    (aob-event s 'state :title "context full: compacting and retrying")
    (aob-session-put s :overflow-resend (list text atts told))
    (aob-session-put s :autocompact-fired t)
    (let ((aob-told-pending nil))
      (aob-acp--prompt-1 s (aob-acp--compact-prompt s)))
    t)))

(defun aob-acp--compact-done (s text res)
  "After S's compact or clear turn TEXT ends with RES, forget the files
its summary already names, or the context that no longer exists, and
the context entries and files it was told, so they are told again."
  (when (and (or (aob-acp--compact-text-p text) (aob-acp--clear-p text))
             (not (equal (plist-get res :stopReason) "cancelled")))
    (aob-session-put s :compact-read nil)
    (aob-session-put s :compact-modified nil)
    (aob-session-put s :context-told nil)
    (aob-session-put s :embeds-told nil)))

(defun aob-acp--overflow-resume (s text res)
  "After S's compact turn TEXT ends with RES, resend the prompt an overflow
held back; non-nil when it did."
  (when-let* (((aob-acp--compact-text-p text))
              (held (aob-session-ref s :overflow-resend)))
    (aob-session-put s :overflow-resend nil)
    (unless (equal (plist-get res :stopReason) "cancelled")
      (let ((aob-acp--overflow-retry t)
            (aob-told-pending (nth 2 held)))
        (aob-acp--prompt-1 s (car held) (cadr held) t))
      t)))

(defun aob-acp--autocompact-on-idle (s _old new)
  ;; usage that crosses the line mid-turn can't fire until the turn ends
  (when (eq new 'idle) (aob-acp--autocompact-check s)))

(add-hook 'aob-state-change-hook #'aob-acp--autocompact-on-idle)

;;; Compact, clear, new — one verb each, whatever the agent

(defvar aob-acp-start-dir)
(defvar aob-acp-mcp-servers)

(defvar aob-acp-context-cleared-functions nil
  "Abnormal hook run with a session whose agent context was just wiped.")

(defcustom aob-acp-plain-clear-limit 5
  "Idle conversations a plain adapter's connection may hold before aob says so.
Each /clear leaves the old conversation's process running, and only
`aob-acp-restart' frees them."
  :type 'natnum :group 'aob)

(defcustom aob-acp-plain-adapter-agents '("pi")
  "Agents whose adapter has no /clear and streams no compaction events.
Their /clear starts a fresh conversation on the same connection, a typed
/new opens another session beside this one, and a /compact turn is drawn
as a compaction line with the context reading dropped until the next."
  :type '(repeat string) :group 'aob)

(defun aob-acp--plain-adapter-p (s)
  (let ((agent (aob-session-ref s :agent)))
    (and (stringp agent)
         (member (aob-acp-preset-agent agent) aob-acp-plain-adapter-agents))))

(defun aob-acp--with-local-commands (s commands)
  "COMMANDS S's agent advertises, plus the /clear and /new aob answers itself."
  (let ((cmds (append commands nil)))
    (if (aob-acp--plain-adapter-p s)
        (append cmds
                (delq nil
                      (mapcar (lambda (c)
                                (unless (seq-find (lambda (x) (equal (plist-get x :name)
                                                                     (car c)))
                                                  cmds)
                                  (list :name (car c) :description (cdr c))))
                              '(("clear" . "Start a fresh conversation in this session")
                                ("new" . "Open a new session beside this one")))))
      cmds)))

(defun aob-acp--local-command (s text)
  "The verb aob answers TEXT with in place of sending it to S, or nil."
  (when (and (stringp text) (aob-acp--plain-adapter-p s))
    (cond ((aob-acp--clear-p text) #'aob-acp-clear)
          ((string-match-p "\\`/new\\(?:[ \t].*\\)?\\'" (string-trim text))
           #'aob-acp-new))))

(defun aob-acp--local-compaction-begin (s text)
  (when (and (aob-acp--plain-adapter-p s) (aob-acp--compact-text-p text))
    (let ((id (aob-acp--span-id)))
      (aob-session-put s :local-compaction id)
      (aob-acp--compaction s (list :sessionUpdate "compaction_update"
                                   :compactionId id :status "in_progress")))))

(defun aob-acp--local-compaction-end (s res err)
  "Close the compaction line a plain adapter's /compact turn opened.
The adapter reports no usage afterwards, so the old reading goes."
  (when-let* ((id (aob-session-ref s :local-compaction)))
    (aob-session-put s :local-compaction nil)
    (let ((stop (plist-get res :stopReason)))
      (aob-acp--compaction
       s (list :sessionUpdate "compaction_update" :compactionId id
               :status (cond (err "failed")
                             ((equal stop "cancelled") "cancelled")
                             (t "completed"))
               :error (and err (plist-get err :message))))
      (unless (or err (equal stop "cancelled"))
        (aob-acp--forget-context-reading s)))))

(defun aob-acp--forget-context-reading (s)
  (dolist (k '(:ctx-used :ctx-size :usage :usage-latest :autocompact-fired))
    (aob-session-put s k nil))
  (aob--dirty s))

;;;###autoload
(defun aob-acp-compact (s &optional instructions)
  "Compact S's context; INSTRUCTIONS, when given, say what the summary keeps."
  (interactive (list (aob-target)
                     (read-string "Compact, keeping (empty for plain): ")))
  (unless (aob-acp--offers-compact-p s)
    (user-error "aob: %s offers no /compact" (aob-session-name s)))
  (aob-prompt s (string-trim (concat "/compact " (or instructions "")))))

;;;###autoload
(defun aob-acp-clear (s)
  "Wipe S's context: /clear where the agent has one, else a fresh conversation."
  (interactive (list (aob-target)))
  (if (aob-acp--plain-adapter-p s)
      (aob-acp--fresh-conversation s)
    (aob-prompt s "/clear")))

;;;###autoload
(defun aob-acp-new (s)
  "Open a new session of S's agent in S's place, leaving S as it is."
  (interactive (list (aob-target)))
  (let ((aob-acp-start-dir (aob-session-project s))
        (aob-acp-mcp-servers (aob-session-ref s :mcp-declared)))
    (aob-acp-spawn (or (aob-session-ref s :preset) (aob-session-ref s :agent))
                   nil nil nil (aob-session-dir s))))

(defun aob-acp--fresh-conversation (s)
  "Start a new ACP session for S on its connection and move S onto it.
The conversation it leaves stays with the adapter, where its session
file keeps it resumable."
  (unless (eq (aob-session-state s) 'idle)
    (user-error "aob: %s is busy; stop the turn first" (aob-session-name s)))
  (let ((proc (aob-session-conn s)))
    (aob-set-state s 'starting)
    (aob-acp--with-init
     proc
     (lambda (init err)
       (if err
           (progn (aob-set-state s 'idle)
                  (aob-event s 'error :title (plist-get err :message)))
         (let* ((aob-acp-mcp-servers (aob-session-ref s :mcp-declared))
                (params (aob-acp--with-limits
                         s init "session/new"
                         (list :cwd (aob-acp--wire-dir
                                     (directory-file-name
                                      (expand-file-name (aob-session-dir s))))
                               :mcpServers (aob-acp--mcp-servers
                                            init (aob-session-project s))))))
           (when-let* ((dirs (aob-acp--extra-dirs s init)))
             (setq params (plist-put (copy-sequence params)
                                     :additionalDirectories dirs)))
           (aob-acp--request
            s "session/new" params
            (lambda (res err)
              (if err
                  (progn (aob-set-state s 'idle)
                         (aob-event s 'error :title (plist-get err :message)))
                (aob-acp--deregister proc s)
                (setf (aob-session-events s) nil
                      (aob-session-nevents s) 0)
                (dolist (k '(:compact-read :compact-modified :context-told
                             :embeds-told :compaction-ev :local-compaction
                             :tools :threads :place-told))
                  (aob-session-put s k nil))
                (aob-session-put s :want-model (aob-session-ref s :model-id))
                (aob-session-put s :want-mode (aob-session-ref s :mode-id))
                (aob-acp--forget-context-reading s)
                (run-hook-with-args 'aob-acp-context-cleared-functions s)
                (aob-acp--session-opened s res nil "context cleared")
                (aob-acp--note-abandoned proc s))))))))))

(defun aob-acp--note-abandoned (proc s)
  "Count the conversation S left running on PROC, and say when it adds up."
  (let ((n (1+ (or (process-get proc 'aob-abandoned) 0))))
    (process-put proc 'aob-abandoned n)
    (when (>= n aob-acp-plain-clear-limit)
      (aob-event s 'state :warning t
                 :title (format "%s holds %d idle %s processes; restart it to free them"
                                (aob-session-name s) n
                                (aob-session-ref s :agent))))))

(defun aob-acp--drop-queue (s)
  "Drop S's pending prompts so a cancel leaves nothing to flush back in."
  (when-let* ((q (aob-session-ref s :queued)))
    (aob-session-put s :queued nil)
    (dolist (e q)
      (when-let* ((ev (nth 2 e)))
        (plist-put ev :status "cancelled")
        (plist-put ev :line nil)))
    (run-hook-with-args 'aob-queue-change-hook s)))

(defun aob-acp--cancel (s &optional drop-queue)
  ;; spec: pending permission requests must be answered `cancelled'
  ;; alongside session/cancel, or the agent waits forever
  (let ((off-turn (and (eq (aob-session-state s) 'blocked)
                         (seq-find (lambda (d) (memq (plist-get d :was) '(idle starting)))
                                   (aob-session-decisions s)))))
    (dolist (d (aob-session-decisions s))
      (aob-acp--cancel-held s d)
      (when-let* ((ev (aob-acp--decision-event s d)))
        (plist-put ev :line nil)))
    (setf (aob-session-decisions s) nil)
    ;; a plain cancel stops just the turn; the full cancel (cc) also drops the
    ;; queue so nothing flushes back when the cancelled turn settles
    (when drop-queue (aob-acp--drop-queue s))
    (cond
     ((plist-get off-turn :then) (aob-acp--fail s '(:message "login cancelled")))
     (off-turn (aob-set-state s (plist-get off-turn :was)))
     ((memq (aob-session-state s) '(working blocked))
      (aob-set-state s 'working)
      (aob-acp--notify s "session/cancel" (list :sessionId (aob-acp--acp-id s)))))))

(defun aob-acp--extension (s key)
  "What S's agent advertises for extension KEY, or nil.
The spec's place is agentCapabilities._meta; claude puts it in the
initialize result's own _meta, so that is read too."
  (or (plist-get (plist-get (aob-session-ref s :agent-caps) :_meta) key)
      (plist-get (aob-session-ref s :agent-meta) key)))

(defun aob-acp--steers-p (s)
  "Non-nil when this agent takes a word into the turn it is running."
  (eq t (plist-get (aob-acp--extension s :steering) :supported)))

(defun aob-acp--interject (s text &optional atts)
  "Say TEXT, with the image files ATTS, to S now.
The _session/steering request injects it into the running turn at the top
of the agent's queue, so a correction costs the work in flight nothing.
A turn held on a decision is still running, and is steered the same way.
The request asks for promptRequired when there was no turn to steer,
which is the adapter telling us to say it the ordinary way.  An adapter
that starts a turn of its own instead answers startedNewTurn: the words are
said, but no prompt of ours owns that turn, so the state is left as it
is.  Any other answer, or an error, queues the text and cancels, as does
an agent that never advertised steering; a session already idle by then
is prompted with it at once."
  (cond
   ((not (memq (aob-session-state s) '(working blocked)))
    (aob-acp--prompt-1 s text atts))
   ((not (aob-acp--steers-p s))
    (aob-acp--queue s text atts)
    (aob-acp--cancel s))
   (t
    ;; the reply comes after the send has returned, and whether you typed
    ;; this is known only while it is being sent
    (let* ((typed aob-prompt-typed)
           (told aob-told-pending)
           (sent (aob-acp--image-paths s text atts))
           (blocks (aob-acp--content-blocks
                    (car sent) (cdr sent)
                    (or (aob-session-dir s) (aob-session-project s))
                    (aob-acp-embeds-p s) s)))
      (aob-acp--request
       s "_session/steering"
       (list :sessionId (aob-acp--acp-id s)
             :prompt blocks
             :_meta '(:steering (:idleBehavior "promptRequired")))
       (lambda (res err)
         (let ((aob-prompt-typed typed)
               (aob-told-pending told))
           (pcase (and (not err) (plist-get res :outcome))
             ("injected"
              (aob-tell-all s told)
              (aob-acp--tell-embeds s blocks)
              (aob-event s 'prompt :text (car sent) :title "steered"
                         :images (length (cdr sent)) :image-files (cdr sent)
                         :typed aob-prompt-typed)
              ;; a turn held on a decision stays held until it is answered
              (unless (eq (aob-session-state s) 'blocked)
                (aob-set-state s 'working)))
             ("promptRequired" (aob-acp--prompt-1 s text atts))
             ;; nothing tells us when a turn we never prompted ends
             ("startedNewTurn"
              (aob-tell-all s told)
              (aob-acp--tell-embeds s blocks)
              (aob-event s 'prompt :text (car sent)
                         :images (length (cdr sent)) :image-files (cdr sent)
                         :typed aob-prompt-typed))
             ;; a steer that never landed must not swallow what you wrote
             (_ (if (eq (aob-session-state s) 'idle)
                    (aob-acp--prompt-1 s text atts)
                  (aob-acp--queue s text atts)
                  (aob-acp--cancel s)))))))))))

;;; Goal — an objective the agent holds across turns and keeps working
;;; toward, reporting back how many rounds it has taken and why it last
;;; carried on.  Our own gate still decides what a task may dispatch;
;;; this is the same statement, said to the agent in terms it tracks.

(defun aob-acp--goal-method (s)
  "The extension method S's agent holds a goal through, or nil.
A name outside the underscore namespace would shadow a protocol method."
  (let ((method (plist-get (aob-acp--extension s :goal) :controlMethod)))
    (and (stringp method) (string-prefix-p "_" method) method)))

(defun aob-acp--goal (s objective &optional then)
  "Give S an OBJECTIVE to hold, or clear it when OBJECTIVE is nil; call THEN.
Setting a goal is itself a turn — the adapter steers `/goal' into the
running one, or prompts when there is none — so the reply lands only
once that turn is done.  That is why the queue waits on THEN instead of
racing the agent with the first real prompt."
  (let ((method (aob-acp--goal-method s)))
    (if (not method)
        (when then (funcall then))
      ;; a goal set on an agent already working is steered into that turn:
      ;; the reply is the goal command's, not the turn's, so the state it
      ;; came from is the state it goes back to
      (let ((was (aob-session-state s)))
        (unless (eq was 'working) (aob-set-state s 'working))
        (aob-acp--request
         s method (append (list :sessionId (aob-acp--acp-id s))
                          (if objective
                              (list :action "set" :objective objective)
                            (list :action "clear")))
         (lambda (_res err)
           (when err
             (aob-event s 'error :title (format "goal: %s" (plist-get err :message))))
           (unless (eq was 'working) (aob-set-state s was))
           (when then (funcall then))))))))

;;;###autoload
(defun aob-acp-goal (s objective)
  "Set the objective S works toward across turns; an empty answer clears it."
  (interactive
   (let ((s (aob-target)))
     (list s (read-string (format "%s goal (empty clears): " (aob-session-name s))
                          (plist-get (aob-session-ref s :goal) :objective)))))
  (unless (aob-acp--goal-method s)
    (user-error "aob: %s holds no goal" (aob-session-name s)))
  (setq objective (string-trim objective))
  (aob-acp--goal s (and (not (string-empty-p objective)) objective)))

(defun aob-acp--decision-event (s decision)
  "The trace event S drew DECISION as, or nil."
  (when-let* ((seq (plist-get decision :seq)))
    (seq-find (lambda (e) (eql (plist-get e :seq) seq))
              (aob-session-events s))))

(defun aob-acp--request-withdrawn (proc request-id)
  "Close the decision the agent on PROC asked with REQUEST-ID and took back.
Codex does this when a question times out on its own.  A request still
unanswered is owed one response, so it gets the Request cancelled error;
one that matches no open decision was answered already and gets nothing."
  (let ((open nil))
    (dolist (s (delete-dups (append (aob-acp--conn-sessions proc)
                                    (hash-table-values (aob-acp--proc-sessions proc)))))
      (when-let* ((d (seq-find (lambda (d) (equal (plist-get d :reply-id) request-id))
                               (aob-session-decisions s))))
        (setq open t)
        (when-let* ((ev (aob-acp--decision-event s d)))
          (plist-put ev :answer 'withdrawn)
          (plist-put ev :line nil))
        (setf (aob-session-decisions s) (delq d (aob-session-decisions s)))
        (unless (aob-session-decisions s)
          (when (eq (aob-session-state s) 'blocked)
            (aob-set-state s (or (plist-get d :was) 'working))))
        (aob--dirty s)))
    (when (and open request-id)
      (aob-acp--respond-proc proc request-id nil
                             (list :code -32800 :message "Request cancelled")))))

(defun aob-acp--cancel-held (s d)
  "Answer the agent's request behind S's decision D as cancelled.
A question or a page to open is cancelled as an elicitation, anything
else as a permission; a login offer answers no request and owes nothing."
  (when-let* ((id (plist-get d :reply-id)))
    (aob-acp--respond s id
                      (if (memq (plist-get d :kind) '(elicitation url))
                          (list :action "cancel")
                        (list :outcome (list :outcome "cancelled"))))))

(defun aob-acp--url-elicitation (s id params)
  "Hold the agent's request ID to open a page, PARAMS, as a Decision on S.
The message is kept whole: codex puts the device code to type there.
Accepting opens the page in a browser; what it asks is done there, and
the agent says when it is through with `elicitation/complete'."
  (let* ((url (plist-get params :url))
         (text (or (plist-get params :message) "open a page"))
         (d (list :reply-id id
                  :kind 'url
                  :was (if (eq (aob-session-state s) 'blocked)
                           (or (seq-some (lambda (held) (plist-get held :was))
                                         (aob-session-decisions s))
                               'working)
                         (aob-session-state s))
                  :elicitation-id (plist-get params :elicitationId)
                  :url url
                  :title (car (split-string text "\n"))
                  :detail url
                  :options (list (list :optionId "accept" :name "Open in browser"
                                       :kind "allow_once")
                                 (list :optionId "decline" :name "Decline"
                                       :kind "decline")))))
    (push d (aob-session-decisions s))
    (aob-set-state s 'blocked)
    (let ((ev (aob-event s 'permission :title (plist-get d :title)
                         :text (concat text "\n" url))))
      (nconc d (list :seq (plist-get ev :seq))))
    (dolist (fn aob-acp-request-functions)
      (condition-case err (funcall fn s d nil)
        (error (message "aob-acp-request-functions: %S" err))))))

(defun aob-acp--url-answer (decision answer)
  "The reply to the page DECISION offered, ANSWER being accept or not.
Accepting opens it; the accept carries no content, since what the page
asks is answered on the page."
  (if (equal answer "accept")
      (progn (browse-url (plist-get decision :url))
             (list :action "accept"))
    (list :action "decline")))

(defun aob-acp--elicitation-complete (proc eid)
  "Close the page the agent on PROC asked to open as EID, now it is done.
The agent races the page against its own flow, so the request may still
be unanswered; codex never withdraws it, so it gets its accept here."
  (dolist (s (delete-dups (append (aob-acp--conn-sessions proc)
                                  (hash-table-values (aob-acp--proc-sessions proc)))))
    (when-let* ((d (and eid
                        (seq-find (lambda (d) (equal (plist-get d :elicitation-id) eid))
                                  (aob-session-decisions s)))))
      (aob-acp--respond-proc proc (plist-get d :reply-id) (list :action "accept"))
      (when-let* ((ev (aob-acp--decision-event s d)))
        (plist-put ev :answer 'completed)
        (plist-put ev :line nil))
      (setf (aob-session-decisions s) (delq d (aob-session-decisions s)))
      (unless (aob-session-decisions s)
        (when (eq (aob-session-state s) 'blocked)
          (aob-set-state s (or (plist-get d :was) 'working))))
      (aob--dirty s))))

(defun aob-acp--clears-context-p (answer)
  "Whether ANSWER takes claude's plan into a fresh context.
The agent swaps its runtime for a new one, and the subagents the old
one ran end without a word."
  (and (stringp answer) (string-prefix-p "exit-plan-clear-" answer)))

(defun aob-acp--close-decision (s decision)
  "Take DECISION off S, and let S go back to what it was doing if it was the last."
  (setf (aob-session-decisions s) (delq decision (aob-session-decisions s)))
  (unless (aob-session-decisions s)
    (when (eq (aob-session-state s) 'blocked)
      (aob-set-state s (or (plist-get decision :was) 'working))))
  (aob--dirty s))

(defun aob-acp--respond-or-drop (s decision result)
  "Send RESULT as the reply to DECISION; when the connection cannot carry it,
drop DECISION, as it can never be answered, and say so."
  (condition-case nil
      (progn
        (unless (process-live-p (aob-acp--proc-of s)) (error "closed"))
        (aob-acp--respond s (plist-get decision :reply-id) result))
    (error
     (when-let* ((ev (aob-acp--decision-event s decision)))
       (plist-put ev :line nil))
     (aob-acp--close-decision s decision)
     (user-error "aob: %s is no longer running; its question is dropped"
                 (aob-session-name s)))))

(defun aob-acp--resolve (s decision answer)
  "Reply to DECISION with ANSWER: a permission's option id, or an
elicitation's ((FIELD . VALUE)...) alist, or decline to leave it unanswered.
A login offer is answered with the method to run, which replies to nothing."
  (unless (memq decision (aob-session-decisions s))
    (user-error "aob: %s no longer waits on that answer" (aob-session-name s)))
  (unless (eq (plist-get decision :kind) 'auth)
    (aob-acp--respond-or-drop
     s decision
     (cond
      ((eq (plist-get decision :kind) 'url) (aob-acp--url-answer decision answer))
      ((not (eq (plist-get decision :kind) 'elicitation))
       (list :outcome (list :outcome "selected" :optionId answer)))
      ((eq answer 'decline) (list :action "decline"))
      (t (list :action "accept"
               :content (let (pl)
                          (dolist (kv answer pl)
                            (setq pl (plist-put
                                      pl (intern (concat ":" (car kv)))
                                      (if (listp (cdr kv))
                                          (vconcat (cdr kv))
                                        (cdr kv)))))))))))
  (when-let* ((ev (aob-acp--decision-event s decision)))
    (plist-put ev :answer (if (eq (plist-get decision :kind) 'elicitation)
                              answer
                            (or (plist-get (seq-find (lambda (o)
                                                       (equal (plist-get o :optionId) answer))
                                                     (plist-get decision :options))
                                           :name)
                                answer)))
    (plist-put ev :line nil))
  (aob-acp--close-decision s decision)
  (when (aob-acp--clears-context-p answer)
    (aob-subagent-announced-settle s))
  (when (eq (plist-get decision :kind) 'auth)
    (aob-acp--auth-chosen s decision answer)))

(defun aob-acp--reap-worktree (s)
  "Remove S's worktree and branch when they hold nothing the project lacks:
a clean tree whose branch is merged into (or still at) the project HEAD.
Runs entirely in the background — the kill that triggers it never waits."
  (let* ((dir (aob-session-dir s))
         (project (aob-session-project s))
         (branch (and dir (concat "aob/" (file-name-nondirectory
                                          (directory-file-name dir))))))
    (when (and dir project
               (not (aob-session-ref s :restarting))
               (not (seq-some (lambda (o) (and (not (eq o s))
                                               (equal (aob-session-dir o) dir)))
                              (aob-sessions)))
               (not (equal (file-truename dir) (file-truename project)))
               (string-prefix-p (file-truename aob-acp-worktree-root)
                                (file-truename dir))
               (file-directory-p dir))
      (ygg-git-async
       dir (list "status" "--porcelain")
       (lambda (status _c1)
         (ygg-git-async
          project (list "branch" "--merged" "HEAD" "--list" branch)
          (lambda (merged _c2)
            (if (or (> (length status) 0) (zerop (length merged)))
                (message "aob: kept %s — it has work the project lacks" dir)
              (ygg-git-async
               project (list "worktree" "remove" "--force" dir)
               (lambda (_o _c3)
                 (ygg-git-async project (list "branch" "-D" branch)
                                #'ignore)))))))))))

(defun aob-acp--kill (s)
  (let* ((proc (aob-session-conn s))
         (told nil)
         (on-close (let ((fn (or (aob-session-ref s :on-close) #'ignore)))
                     (lambda (&rest args)
                       (unless told
                         (setq told t)
                         (apply fn args)))))
         ;; cleared only once a close is on the wire to answer it
         (unclosed on-close))
    ;; a shared connection must not be left awaiting replies from a corpse:
    ;; answer held decisions, stop the turn, then let go
    (dolist (d (aob-session-decisions s))
      (ignore-errors
        (aob-acp--cancel-held s d)))
    (setf (aob-session-decisions s) nil)
    (when (and proc (process-live-p proc) (aob-session-ref s :acp-id))
      (when (memq (aob-session-state s) '(working blocked failed))
        (ignore-errors
          (aob-acp--notify s "session/cancel"
                           (list :sessionId (aob-acp--acp-id s)))))
      ;; cancelling stops the turn; only closing tears the CLI subprocess down,
      ;; and until the last session goes the connection keeps every one alive
      (when (aob-acp--session-cap (aob-acp--init-of s) :close)
        (ignore-errors
          (aob-acp--request s "session/close"
                            (list :sessionId (aob-acp--acp-id s))
                            on-close)
          (setq unclosed nil))))
    ;; deferred one-shots watching this session unhook on the transition
    (aob-set-state s 'dead)
    (aob-acp--reap-worktree s)
    ;; a restart's successor takes its place and its trace from the registry
    (if (aob-session-ref s :restarting)
        (setf (aob-session-conn s) nil)
      (aob-remove-session s))
    (when proc
      (aob-acp--deregister proc s)
      ;; the connection outlives any one session; reap it with the last
      (when (null (aob-acp--conn-sessions proc))
        (aob-acp--conn-cleanup proc)))
    (when unclosed
      (ignore-errors (funcall unclosed nil nil)))))

(declare-function aob-trace "aob-trace" (s))
(declare-function aob-session-model-now "aob" (s))
(declare-function aob-session-kid-changed "aob" (kid))
(declare-function aob-trace-buffer "aob-trace" (s))

(defun aob-acp--focus (s)
  (aob-trace s))

(aob-register-backend
 'acp (list :prompt #'aob-acp--prompt
            :cancel #'aob-acp--cancel
            :interject #'aob-acp--interject
            :flush #'aob-acp--flush-queue
            :resolve #'aob-acp--resolve
            :kill #'aob-acp--kill
            :focus #'aob-acp--focus
            :rename #'aob-acp--renamed))

;;; Opening sessions — shared-connection core all entry points use

(defcustom aob-acp-start-dir-function nil
  "Function of no arguments returning where a new session should start,
or nil to start where the buffer is.  A host that pins a directory to
something larger than a buffer — a workspace, a tab — sets this so a
session opened from a scratch buffer still belongs to that place."
  :type '(choice function (const nil)) :group 'aob)

(defvar aob-acp-system-append nil
  "What every session is told on top of its agent's own system prompt.
A string, a function of the session returning one, or nil.  It rides in
the request that opens the session, so it reaches a session whatever
config home it runs under; an adapter that does not read
_meta.systemPrompt never sees it.")

(defvar aob-acp-start-dir nil
  "Where a spawn belongs, said outright by a caller that already knows.
A task's root is not a guess to be improved on, so this beats both
`aob-acp-start-dir-function' and the buffer, and is taken as given —
climbing to a `.git' above it would undo the worktree it names.")

(defvar aob-acp-start-worktree nil
  "The worktree a spawn works in, as aob-acp-read-worktree answers, or nil.")

(defun aob-acp--real-dir (dir)
  "DIR where it actually is, so a folder reached through a link is one folder.
The agent files its conversations, its config home and its project row
under the path it is given; the same tree under two names is two of each."
  (file-name-as-directory
   (if (file-remote-p dir) (expand-file-name dir) (file-truename dir))))

(defun aob-acp--project ()
  (if aob-acp-start-dir
      (aob-acp--real-dir aob-acp-start-dir)
    (let ((default-directory (or (and aob-acp-start-dir-function
                                      (ignore-errors
                                        (funcall aob-acp-start-dir-function)))
                                 default-directory)))
      (aob-acp--real-dir
       (or (locate-dominating-file default-directory ".git")
           default-directory)))))

(defun aob-acp--wire-dir (dir)
  "DIR as the agent itself will read it.
An agent reached over TRAMP runs on the far side of the handle, where
the method and host that name it here name nothing.  What travels in
the JSON has to be the path that agent's own filesystem answers to,
while the spelling Emacs keeps stays whole so a file read on this side
still finds the tree."
  (directory-file-name (file-local-name (expand-file-name dir))))

(defun aob-acp--worktree-path (project name)
  ;; the dir's basename becomes the branch name — a component starting
  ;; with "." is an invalid git ref (bites dot-dir projects like .emacs.d)
  (let ((base (string-remove-prefix
               "." (file-name-nondirectory (directory-file-name project)))))
    (expand-file-name (format "%s-%s-%s" base name
                              (format-time-string "%m%d%H%M%S"))
                      aob-acp-worktree-root)))

(defun aob-acp--worktree-make (project dir done &optional branch)
  "Create worktree DIR off PROJECT in the background; DONE gets nil or an
error plist.  BRANCH names the new branch, else it is aob/ and DIR's name."
  (make-directory aob-acp-worktree-root t)
  (ygg-git-async
   project
   (list "worktree" "add" "-b"
         (or branch (concat "aob/" (file-name-nondirectory dir))) dir)
   (lambda (out code)
     (funcall done (unless (zerop code)
                     (list :message (format "worktree add failed: %s"
                                            (string-trim out))))))))

(defun aob-acp--parse-worktrees (text)
  "The worktrees git worktree list --porcelain TEXT names, as (DIR . BRANCH).
Bare and prunable entries are left out; a detached one has no BRANCH."
  (let (found)
    (dolist (block (split-string text "\n\n" t))
      (let (dir branch skip)
        (dolist (line (split-string block "\n" t))
          (cond ((string-prefix-p "worktree " line)
                 (setq dir (file-name-as-directory (substring line 9))))
                ((string-prefix-p "branch " line)
                 (setq branch (string-remove-prefix "refs/heads/" (substring line 7))))
                ((or (equal line "bare") (string-prefix-p "prunable" line))
                 (setq skip t))))
        (when (and dir (not skip))
          (push (cons dir branch) found))))
    (nreverse found)))

(defun aob-acp--worktrees (dir)
  "The worktrees of the repository DIR is in, or nil outside one."
  (unless (or (null dir) (file-remote-p dir) (not (file-directory-p dir)))
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory dir)))
        (when (eq 0 (ignore-errors
                      (call-process "git" nil t nil "worktree" "list" "--porcelain")))
          (seq-filter (lambda (w) (file-directory-p (car w)))
                      (aob-acp--parse-worktrees (buffer-string))))))))

(defconst aob-acp-place-others-max 8
  "How many of the repository's other worktrees a place note names.")

(defun aob-acp--place-others (wts here)
  "The tail of a place note at HERE naming the other worktrees in WTS."
  (let* ((others (remove here wts))
         (named (mapcar (lambda (w)
                          (format "%s (%s)"
                                  (file-name-nondirectory (directory-file-name (car w)))
                                  (or (cdr w) "detached")))
                        (take aob-acp-place-others-max others))))
    (if (null named) ""
      (format " · other worktrees: %s%s"
              (string-join named ", ")
              (if (nthcdr aob-acp-place-others-max others) ", …" "")))))

(defun aob-acp--place-note (dir)
  "One line naming DIR's worktree and branch, and the repository's others.
Nil outside a repository."
  (when-let* ((wts (aob-acp--worktrees dir))
              (here (car (sort (seq-filter (lambda (w) (file-in-directory-p dir (car w))) wts)
                               :key (lambda (w) (- (length (car w))))))))
    (format "[workspace: %s · branch %s%s%s]"
            (directory-file-name (car here))
            (or (cdr here) "detached HEAD")
            (if (eq here (car wts)) ""
              (format " · linked worktree of %s" (directory-file-name (caar wts))))
            (aob-acp--place-others wts here))))

(defun aob-acp--place-block (s)
  "The place note S has not yet been told, as a prompt block, else nil.
Sent in the prompt because only some adapters read _meta.systemPrompt,
and again after a branch switch or a worktree added or removed."
  (let ((note (aob-acp--place-note (or (aob-session-dir s) (aob-session-project s)))))
    (unless (or (null note) (equal note (aob-session-ref s :place-told)))
      (aob-session-put s :place-told note)
      (list :type "text" :text note))))

(defun aob-acp--worktree-choices (dir)
  "DIR's worktrees when there is a choice between them, else nil."
  (let ((wts (aob-acp--worktrees dir)))
    (and (cdr wts) wts)))

(defconst aob-acp--new-worktree "new worktree…")

(defun aob-acp--worktree-short-name (folder main)
  "FOLDER named for a picker: inside MAIN relative to it, else to MAIN's parent."
  (let ((main (directory-file-name main))
        (folder (directory-file-name folder)))
    (if (and (file-in-directory-p folder main) (not (equal folder main)))
        (file-relative-name folder main)
      (file-relative-name folder (file-name-directory main)))))

(defun aob-acp--worktree-head (folder branch)
  "BRANCH, or detached and FOLDER's short commit when there is none."
  (or branch
      (let ((default-directory folder))
        (format "detached %s"
                (string-trim
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (ignore-errors
                       (call-process "git" nil t nil "rev-parse" "--short" "HEAD")))))))))

(defun aob-acp--worktree-rows (wts)
  "WTS as (LABEL . FOLDER), each LABEL unique, leading with branch and name."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar (lambda (w)
              (let* ((label (format "%s  %s"
                                    (aob-acp--worktree-head (car w) (cdr w))
                                    (aob-acp--worktree-short-name (car w) (caar wts))))
                     (n (puthash label (1+ (gethash label seen 0)) seen)))
                (cons (if (> n 1) (format "%s #%d" label n) label) (car w))))
            wts)))

(defun aob-acp--worktree-table (rows)
  "A completion table over ROWS' labels, in order, annotated with each folder."
  (let ((notes (mapcar (lambda (r)
                         (cons (car r)
                               (propertize (format "  %s" (abbreviate-file-name (cdr r)))
                                           'face 'shadow)))
                       (seq-filter #'cdr rows))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . aob-worktree)
                     (display-sort-function . identity)
                     (cycle-sort-function . identity)
                     (annotation-function . ,(lambda (c) (cdr (assoc c notes)))))
        (complete-with-action action (mapcar #'car rows) string pred)))))

(defun aob-acp-read-worktree (dir)
  "Ask which worktree of DIR's repository a session works in.
Nil without asking when the repository has one worktree or DIR is in
none, and nil for DIR's own.  Answers a folder, or (FOLDER . BRANCH)
for one still to be made.  Rows lead with the branch and a short name,
the folder is the annotation."
  (when-let* ((wts (aob-acp--worktree-choices dir)))
    (let* ((here (file-truename (file-name-as-directory
                                 (or (locate-dominating-file dir ".git") dir))))
           (mine (seq-find (lambda (w) (equal (file-truename (car w)) here)) wts))
           (rows (aob-acp--worktree-rows
                  (if mine (cons mine (remq mine wts)) wts)))
           (rows (append rows (list (cons aob-acp--new-worktree nil))))
           (pick (completing-read "Worktree: " (aob-acp--worktree-table rows)
                                  nil t nil nil (caar rows))))
      (if (equal pick aob-acp--new-worktree)
          (let ((branch (string-trim (read-string "Branch for the new worktree: "))))
            (when (string-empty-p branch)
              (user-error "aob: a new worktree needs a branch"))
            (cons (aob-acp--worktree-path
                   (caar wts) (replace-regexp-in-string "[^[:alnum:]._-]+" "-" branch))
                  branch))
        (let ((picked (cdr (assoc pick rows))))
          (unless (equal (file-truename picked) here) picked))))))

(defun aob-acp--gen-name (base)
  "BASE numbered past whatever is already registered under it."
  (let ((n 1))
    (while (aob-session-get (format "acp:%s:%d" base n))
      (setq n (1+ n)))
    (format "%s:%d" base n)))

(defcustom aob-auto-name t
  "Name a session for what it is about while it still has its default name.
The agent's own title wins, then its goal, then the first prompt.  A
name you gave it, by renaming or at spawn, is never touched."
  :type 'boolean
  :group 'aob)

(defconst aob-acp--auto-name-ranks '((prompt . 1) (goal . 2) (title . 3)))

(defun aob-acp--default-name-p (s)
  "Non-nil when S is still called what it was numbered at birth."
  (let ((name (aob-session-name s)))
    ;; a conversation opened for reading is labelled with its day, which
    ;; nobody chose, and the label outlives the reading into the file
    (and (string-match "\\`\\(.+?\\):[0-9]+\\(?: · [[:alpha:]]+ [0-9]+\\(?: [[:alnum:]]\\{4\\}\\)?\\)?\\'" name)
         (member (match-string 1 name)
                 (append (list (aob-session-ref s :agent)
                               (aob-session-ref s :preset))
                         (aob-acp-names))))))

(defun aob-acp--goal-text (goal)
  (cond ((stringp goal) goal)
        ((and (consp goal) (keywordp (car goal)))
         (let ((objective (plist-get goal :objective)))
           (and (stringp objective) objective)))))

(defun aob-acp--name-line (line)
  "LINE reduced to the words a name can carry, or nil when none are left."
  (let ((case-fold-search nil))
    (dolist (rule '(("\\`[[:space:]]*\\(?:#+\\|>+\\|[-*+]\\|[0-9]+[.)]\\)[[:space:]]+" . "")
                    ("\\`[[:space:]]*\\(?:/[[:alnum:]:_-]+\\(?:[[:space:]]+\\|\\'\\)\\)+" . "")
                    ("\\(\\`\\|[[:space:]]\\)@[^@[:space:]]+" . "\\1")
                    ("\\[\\[Image[0-9]*\\]\\]" . " ")
                    ("\\[\\([^]]*\\)\\]([^)]*)" . "\\1")
                    ("[*~\x60\"“”«»]+\\|__+" . "")
                    ("\\(\\`\\|[[:space:]]\\)['‘’]+" . "\\1")
                    ("['‘’]+\\([[:space:]]\\|\\'\\)" . "\\1")
                    ("[[:space:]]+" . " ")))
      (setq line (replace-regexp-in-string (car rule) (cdr rule) line t)))
    (setq line (string-trim line "[[:space:][:punct:]]+" "[[:space:][:punct:]]+"))
    (and (string-match-p "[[:alnum:]]" line) line)))

(defun aob-acp--name-from-text (text &optional max)
  "A short name for what TEXT asks, at most MAX (default 32) characters."
  (let* ((max (or max 32))
         (text (replace-regexp-in-string
                "<\\(context\\|preset\\)[ >]\\(?:.\\|\n\\)*?</\\1>" "" text t))
         (text (replace-regexp-in-string
                "^\x60\x60\x60\\(?:.\\|\n\\)*?^\x60\x60\x60.*$" "" text t t))
         (words (seq-some #'aob-acp--name-line (split-string text "\n"))))
    (when words
      (if (<= (length words) max)
          words
        (let* ((head (substring words 0 (1+ max)))
               (cut (string-match-p " [^ ]*\\'" head)))
          (string-trim-right (substring words 0 (if (and cut (> cut 0)) cut max))
                             "[[:space:][:punct:]]+"))))))

(defun aob-acp--unique-name (s name)
  "NAME, or NAME numbered past what another live session is already called."
  (let ((taken (mapcar #'aob-session-name (remq s (aob-live-sessions))))
        (try name)
        (n 1))
    (while (member try taken)
      (setq n (1+ n) try (format "%s %d" name n)))
    try))

(defun aob-acp--auto-name (s source text)
  "Name S from TEXT, which came from SOURCE: prompt, goal or title."
  (let ((rank (alist-get source aob-acp--auto-name-ranks))
        (had (alist-get (aob-session-ref s :auto-named) aob-acp--auto-name-ranks)))
    (when-let* ((aob-auto-name)
                ((not (aob-session-ref s :named-by-user)))
                ((if had
                     (or (> rank had) (and (= rank had) (not (eq source 'prompt))))
                   (aob-acp--default-name-p s)))
                ((stringp text))
                (words (aob-acp--name-from-text text))
                (name (aob-acp--unique-name
                       s (format "%s: %s" (or (aob-session-ref s :agent) "agent")
                                 words))))
      (aob-session-put s :auto-named source)
      (unless (equal name (aob-session-name s))
        (aob--set-name s name)))))

(defun aob-acp--want-mode (s want)
  "Switch S to the mode id WANT its definition pinned, if advertised.
A mode config option, when there is one, is the wire the spec prefers
over session/set_mode."
  (let ((modes (plist-get (aob-session-ref s :modes) :availableModes))
        (opt (aob-acp--config-option s "mode")))
    (cond
     (opt
      (cond
       ((not (seq-find (lambda (v) (equal (plist-get v :value) want))
                       (aob-acp--config-values opt)))
        (message "aob: %s has no mode %s" (aob-session-name s) want))
       ((equal want (plist-get opt :currentValue))
        (aob-session-put s :mode-id want))
       (t (aob-acp--set-config s (plist-get opt :id) want
                               (lambda (_res err)
                                 (unless err
                                   (aob-session-put s :mode-id want)
                                   (aob--dirty s)))))))
     ((not (seq-find (lambda (m) (equal (plist-get m :id) want)) modes))
      (message "aob: %s has no mode %s" (aob-session-name s) want))
     ((equal want (aob-session-ref s :mode-id)) nil)
     (t (aob-acp--request s "session/set_mode"
                          (list :sessionId (aob-acp--acp-id s) :modeId want)
                          (lambda (_res err)
                            (if err
                                (message "aob: set_mode failed: %s"
                                         (plist-get err :message))
                              (aob-session-put s :mode-id want)
                              (aob--dirty s))))))))

(defun aob-acp--model-hits (info want)
  "The ids in INFO that WANT may name, as a list.
An id or a name WANT is exactly is the one hit; else every model whose
id or name holds WANT, unless one of those is held by all the others —
sonnet beside sonnet[1m] — which is then the one."
  (let* ((values (cdr info))
         (needle (downcase want))
         (fields (lambda (v) (seq-filter #'stringp (list (plist-get v :value)
                                                         (plist-get v :name)))))
         (exact (seq-find (lambda (v)
                            (seq-some (lambda (f) (equal (downcase f) needle))
                                      (funcall fields v)))
                          values)))
    (if exact
        (list (plist-get exact :value))
      (let* ((hits (seq-filter (lambda (v)
                                 (seq-some (lambda (f) (string-search needle (downcase f)))
                                           (funcall fields v)))
                               values))
             (ids (mapcar (lambda (v) (plist-get v :value)) hits))
             (shortest (car (sort (copy-sequence ids)
                                  (lambda (a b) (< (length a) (length b)))))))
        (if (and shortest
                 (seq-every-p (lambda (id) (string-search shortest id)) ids))
            (list shortest)
          ids)))))

(defun aob-acp--model-offered (info want)
  "The one id in INFO that WANT names, or nil when none or several do."
  (let ((hits (aob-acp--model-hits info want)))
    (and (null (cdr hits)) (car hits))))

(defun aob-acp--resolves-model-names-p (s)
  "Whether S's agent turns a model name into the model itself.
Claude's adapter resolves aliases such as opus; codex takes exact ids."
  (and (string-match-p "claude" (or (aob-session-ref s :agent) "")) t))

(defun aob-acp--want-model (s want)
  "Switch S to the model WANT names, if the agent offers it.
WANT is an id, or a name such as \"haiku\" that an offered model carries.
A name several models carry goes as it is to an agent that resolves
names, and is otherwise refused with the models it could mean."
  (let* ((info (aob-acp--model-info s))
         (hits (aob-acp--model-hits info want))
         (now (or (aob-session-ref s :model-name) (car info) "?")))
    (cond
     ((null hits)
      (aob-event s 'state :warning t
                 :title (format "model: %s is not offered — still %s" want now)))
     ((cdr hits)
      (if (aob-acp--resolves-model-names-p s)
          (aob-acp--set-model s want)
        (aob-event s 'state :warning t
                   :title (format "model: %s could be %s — still %s"
                                  want (string-join hits ", ") now))))
     ((equal (car hits) (car info)) nil)
     (t (aob-acp--set-model s (car hits))))))

(defun aob-acp--fail (s err)
  "Mark S failed with ERR\='s message, unless its process already died.
A dead session was told why when it died; failing it again would bury
that reason under whatever its unanswered requests say.  A refusal for
want of a login says which agent and how to log it in."
  (unless (eq (aob-session-state s) 'dead)
    (let ((why (if (aob-acp--auth-error-p err)
                   (aob-acp--auth-message s err)
                 (or (and err (plist-get err :message)) "error"))))
      (aob-session-put s :fail-reason why)
      (aob-set-state s 'failed)
      (aob-event s 'error :title why)
      ;; a quiet death reads as a live-but-broken session — the human
      ;; pokes verbs at a corpse
      (message "aob: %s failed: %s" (aob-session-name s) why))))

(defvar aob-acp-before-first-prompt-functions nil
  "Abnormal hook run with a ready session before its queue flushes.
A function returning non-nil holds the opening turn: what is queued stays
queued, and whoever held it is who sends it afterwards.  Readiness is the
first place the session's model can be read, so this is where a caller
that cares which model answers gets to stop the turn.")

(defun aob-acp--want-config (s config)
  "Set the config options CONFIG names on S, those it advertises.
CONFIG is a plist of id to value.  An option the agent never offered is
said once and skipped: a preset written for one agent should not fail a
session on another."
  (let ((offered (mapcar (lambda (o) (plist-get o :id))
                         (aob-session-ref s :config-options))))
    (dolist (pair (seq-partition config 2))
      ;; written as a plist, so the ids arrive as keywords; the wire
      ;; wants the bare name the agent advertised
      (let* ((key (car pair))
             (id (if (keywordp key) (substring (symbol-name key) 1)
                   (format "%s" key)))
             (value (cadr pair)))
        (if (and offered (not (member id offered)))
            (message "aob: %s does not offer %s" (aob-session-name s) id)
          (aob-acp--set-config s id value))))))

(defun aob-acp--claude-p (init)
  "Whether INIT came from Claude's adapter, which reads _meta.claudeCode.options."
  (and (string-match-p "claude-\\(?:agent\\|code\\)-acp"
                       (or (plist-get (plist-get init :agentInfo) :name) ""))
       t))

(defun aob-acp--claude-options (tools thinking)
  "The options Claude's adapter takes for a TOOLS allowlist and THINKING level."
  (append (when tools (list :tools (vconcat tools)))
          (pcase thinking
            ("off" (list :thinking (list :type "disabled")))
            ((or "low" "medium" "high" "xhigh" "max") (list :effort thinking)))))

(defconst aob-acp-effort-ladder '("low" "medium" "high" "xhigh" "max")
  "Claude's effort levels, least first.")

(defconst aob-acp-codex-effort-ladder '("low" "medium" "high" "xhigh" "max" "ultra")
  "Codex's reasoning efforts its current models offer, least first.
Its enum also has none and minimal, which none of those models accept.")

(defcustom aob-acp-codex-worker-models
  '(("opus" . "gpt-6-astra") ("sonnet" . "gpt-6-sol") ("haiku" . "gpt-6-luna"))
  "The codex model each worker model a preset names stands for.
A name not here goes to codex as it is written."
  :type '(alist :key-type string :value-type string) :group 'aob)

(defcustom aob-acp-codex-roles-directory (locate-user-emacs-file "var/aob-codex-roles/")
  "Where the codex role files a session's worker levels point at are written.
Codex reads a role's model, effort and instructions only from a file;
the session names these files in its own config, never in a codex home."
  :type 'directory :group 'aob)

(defun aob-acp--effort-below (level &optional ladder)
  "The effort one step under LEVEL on LADDER, Claude's by default.
The lowest stays where it is; medium when LEVEL is unknown."
  (let ((ladder (or ladder aob-acp-effort-ladder)))
    (if-let* ((tail (member level ladder)))
        (nth (max 0 (- (length ladder) (length tail) 1)) ladder)
      "medium")))

(defun aob-acp--lead-effort (s thinking &optional ladder)
  "The effort S opens at: THINKING when on LADDER, else its effort option."
  (let ((ladder (or ladder aob-acp-effort-ladder)))
    (or (car (member thinking ladder))
        (when-let* ((opt (seq-find (lambda (o) (equal (plist-get o :category) "thought_level"))
                                   (aob-session-ref s :config-options))))
          (car (member (plist-get opt :currentValue) ladder))))))

(defun aob-acp--worker-levels (s thinking ladder model-of)
  "S's :workers as the levels an agent is handed, or nil for none.
Each is a plist of :level, :name worker-LEVEL, :model, :effort, :prompt
and :read-only.  The build level is always there: opus, one effort on
LADDER under the lead's, which is THINKING or S's own effort option.  A
level naming no model or effort of its own takes build's, one without a
prompt of its own takes S's :worker-prompt, and S's :worker-effort,
when set, is every level's.  MODEL-OF turns a preset's model name into
the agent's."
  (when-let* ((workers (aob-session-ref s :workers)))
    (let ((below (aob-acp--effort-below (aob-acp--lead-effort s thinking ladder) ladder))
          (override (car (member (aob-session-ref s :worker-effort) ladder)))
          (prompt (or (aob-session-ref s :worker-prompt)
                      "Carry out the brief you are given and report as it asks.")))
      (mapcar (lambda (w)
                (list :level (plist-get w :name)
                      :name (concat "worker-" (plist-get w :name))
                      :model (funcall model-of (or (plist-get w :model) "opus"))
                      :effort (or override (car (member (plist-get w :effort) ladder)) below)
                      :prompt (or (plist-get w :prompt) prompt)
                      :read-only (plist-get w :read-only)))
              (if (seq-find (lambda (w) (equal (plist-get w :name) "build")) workers)
                  workers
                (cons (list :name "build") workers))))))

(defun aob-acp--level-description (level)
  "What the agent is told of LEVEL when it picks a worker."
  (format "The %s worker: %s at %s effort%s." (plist-get level :level)
          (plist-get level :model) (plist-get level :effort)
          (if (plist-get level :read-only) ", read-only: finds and checks, changes nothing" "")))

(defun aob-acp--claude-agents (s thinking)
  "The agents option Claude's adapter takes for S's :workers, or nil for none.
A read-only level gets only the tools that read."
  (when-let* ((levels (aob-acp--worker-levels s thinking aob-acp-effort-ladder #'identity)))
    (let (map)
      (dolist (l levels)
        (setq map (plist-put map (intern (concat ":" (plist-get l :name)))
                             (append (list :description (aob-acp--level-description l)
                                           :prompt (plist-get l :prompt)
                                           :model (plist-get l :model)
                                           :effort (plist-get l :effort))
                                     (and (plist-get l :read-only)
                                          (list :tools (vector "Read" "Grep" "Glob")))))))
      (list :agents map))))

(defun aob-acp--codex-model (name)
  (or (cdr (assoc name aob-acp-codex-worker-models)) name))

(defun aob-acp--toml-string (text)
  "TEXT as a TOML basic string; JSON escapes all TOML needs but DEL."
  (replace-regexp-in-string "\x7f" "\\u007F" (json-encode-string text) t t))

(defun aob-acp--codex-role-file (level)
  "LEVEL as a codex role file under aob-acp-codex-roles-directory, its path.
The name carries a hash of the text, so a file once written never changes."
  (let* ((text (concat "name = " (aob-acp--toml-string (plist-get level :name))
                       "\ndescription = " (aob-acp--toml-string (aob-acp--level-description level))
                       "\nmodel = " (aob-acp--toml-string (plist-get level :model))
                       "\nmodel_reasoning_effort = " (aob-acp--toml-string (plist-get level :effort))
                       (if (plist-get level :read-only) "\nsandbox_mode = \"read-only\"" "")
                       "\ndeveloper_instructions = " (aob-acp--toml-string (plist-get level :prompt))
                       "\n"))
         (dir (file-name-as-directory (expand-file-name aob-acp-codex-roles-directory)))
         (file (format "%s%s-%s.toml" dir (plist-get level :name)
                       (substring (secure-hash 'sha1 text) 0 12))))
    (unless (file-exists-p file)
      (make-directory dir t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file nil 'silent)))
    file))

(defun aob-acp--codex-config (s)
  "The codex config S's subagent cap and worker levels ask for, or nil.
Each level is a role the spawn tool offers as its agent_type.  The
multi-agent tools stay the first version, whose spawn event carries the
prompt the brief check reads and the model and effort the role ran at.
A remote session gets its cap and no roles: their files are local."
  (let* ((cap (aob-session-ref s :subagent-cap))
         (levels (and (not (file-remote-p (or (aob-session-project s) "")))
                      (aob-acp--worker-levels s (aob-session-ref s :want-thinking)
                                              aob-acp-codex-effort-ladder
                                              #'aob-acp--codex-model)))
         (agents (and cap (>= cap 1)
                      (list :max_concurrent_threads_per_session (min cap 256)))))
    (dolist (l levels)
      (setq agents (plist-put agents (intern (concat ":" (plist-get l :name)))
                              (list :description (aob-acp--level-description l)
                                    :config_file (aob-acp--codex-role-file l)))))
    (aob-session-put s :codex-roles
                     (mapcar (lambda (l) (list (plist-get l :name) (plist-get l :model)
                                               (plist-get l :effort)))
                             levels))
    (when agents
      (append (list :agents agents)
              (and levels (list :features (list :multi_agent t :multi_agent_v2 :false)))))))

(defun aob-acp--codex-role (s raw)
  "The role S's codex spawn ran as, from the model and effort in its RAW input.
Codex tells neither the role nor the call's agent_type, so the role is
the one level of S's that runs at that pair; two such levels, or none,
and the pair itself is the answer."
  (when-let* ((model (plist-get raw :model))
              ((not (string-empty-p model)))
              (effort (plist-get raw :reasoningEffort)))
    (let ((hits (seq-filter (lambda (r) (equal (cdr r) (list model effort)))
                            (aob-session-ref s :codex-roles))))
      (if (= (length hits) 1)
          (caar hits)
        (format "%s · %s" model effort)))))

(defun aob-acp--with-limits (s init method params)
  "PARAMS for the opening METHOD carrying S's :want-tools and :want-thinking.
Claude's adapter takes both in _meta on session/new, resume and load, and
S keeps them as :limits so a restore or fork asks for them again.  A fork,
or any other agent, gets its thinking after it opens, and the trace says a
tools limit went unenforced.  S's :workers go to Claude here, as agents;
codex takes its roles from the connection, aob-acp--agent-env."
  (let ((tools (aob-session-ref s :want-tools))
        (thinking (aob-session-ref s :want-thinking))
        (workers (aob-session-ref s :workers)))
    (when-let* ((limits (append (and tools (list :want-tools tools))
                                (and thinking (list :want-thinking thinking))
                                (aob-acp--orch-refs s))))
      (aob-session-put s :limits limits))
    (cond
     ((not (or tools thinking workers)) params)
     ((and (aob-acp--claude-p init)
           (member method '("session/new" "session/resume" "session/load")))
      (aob-session-put s :want-thinking nil)
      (let* ((meta (copy-sequence (plist-get params :_meta)))
             (cc (copy-sequence (plist-get meta :claudeCode)))
             (opts (append (plist-get cc :options)
                           (aob-acp--claude-options tools thinking)
                           (aob-acp--claude-agents s thinking))))
        (plist-put (copy-sequence params) :_meta
                   (plist-put meta :claudeCode (plist-put cc :options opts)))))
     (t
      (when tools
        (aob-event s 'state :title (format "tools limit not enforced for %s"
                                           (aob-session-ref s :agent))))
      params))))

(defun aob-acp--orch-refs (s)
  "S's subagent cap and worker refs, as a restore or fork hands them on."
  (cl-loop for key in '(:subagent-cap :subagent-briefs :workers :worker-prompt :worker-effort)
           for value = (aob-session-ref s key)
           when value append (list key value)))

(defun aob-acp--worker-efforts (&optional s)
  "The efforts S's agent takes for its workers; both ladders when S is unknown."
  (pcase (and s (aob-session-ref s :agent))
    ("codex" aob-acp-codex-effort-ladder)
    ((pred stringp) aob-acp-effort-ladder)
    (_ (seq-uniq (append aob-acp-effort-ladder aob-acp-codex-effort-ladder)))))

(defun aob-acp-worker-effort (s level)
  "Run every worker S sends at LEVEL; none puts each back on its own level.
Claude and codex read their workers only when a session opens, so this
holds from S's next open, a wake or a restore, and not in the turn
already running."
  (interactive
   (let ((s (aob-target)))
     (list s (completing-read (format "%s workers at: " (aob-session-name s))
                              (cons "none" (aob-acp--worker-efforts s)) nil t nil nil
                              (or (aob-session-ref s :worker-effort) "none")))))
  (let ((effort (car (member level (aob-acp--worker-efforts s)))))
    (aob-session-put s :worker-effort effort)
    (when-let* ((limits (aob-session-ref s :limits)))
      (aob-session-put s :limits (plist-put (copy-sequence limits) :worker-effort effort)))
    (message "aob: %s workers %s from its next open" (aob-session-name s)
             (if effort (concat "at " effort) "on their own levels"))))

(defun aob-acp--want-thinking (s level)
  "Set S's thought_level option to LEVEL, or say in the trace it has none.
Off is the lowest the agent offers by the name none or minimal."
  (let* ((opt (seq-find (lambda (o) (equal (plist-get o :category) "thought_level"))
                        (aob-session-ref s :config-options)))
         (offered (mapcar (lambda (v) (plist-get v :value)) (plist-get opt :options)))
         (value (seq-find (lambda (v) (member v offered))
                          (if (equal level "off") '("none" "minimal") (list level)))))
    (if value
        (aob-acp--set-config s (plist-get opt :id) value)
      (aob-event s 'state :title (format "thinking %s not supported by %s"
                                         level (aob-session-ref s :agent))))))

(defun aob-acp--session-opened (s res &optional fallback-id title)
  "Ingest the result of any session-opening method (new/load/fork).
Every path stores modes/models identically — a resumed or forked
session must not be poorer than a fresh one."
  (aob-acp--register (aob-session-conn s)
                     (or (plist-get res :sessionId) fallback-id) s)
  (aob-session-put s :opened t)
  (when-let* ((modes (plist-get res :modes)))
    (aob-session-put s :modes modes)
    (aob-session-put s :mode-id (plist-get modes :currentModeId)))
  ;; model, effort and fast mode are configOptions; the models field is
  ;; the unstable one they replaced
  (when-let* ((opts (plist-get res :configOptions)))
    (aob-acp--config-apply s opts))
  ;; before the queue flushes below — the wire is ordered, so even the
  ;; first turn already runs under the definition's mode
  ;; before the flush: the wire is ordered, so the first turn already runs
  ;; on the model the task was assigned
  (when-let* ((want (aob-session-ref s :want-model)))
    (aob-session-put s :want-model nil)
    (aob-acp--want-model s want))
  ;; after the model: pi's setModel resets the thinking level the mode sets
  (when-let* ((want (aob-session-ref s :want-mode)))
    (aob-session-put s :want-mode nil)
    (aob-acp--want-mode s want))
  ;; and the rest of what the preset asked for — reasoning effort and
  ;; the like — in the same ordered window, so the first turn runs under
  ;; all of it and not only the parts that had a path of their own
  (when-let* ((want (aob-session-ref s :want-config)))
    (aob-session-put s :want-config nil)
    (aob-acp--want-config s want))
  (when-let* ((want (aob-session-ref s :want-thinking)))
    (aob-session-put s :want-thinking nil)
    (aob-acp--want-thinking s want))
  (aob-set-state s 'idle)
  (aob-event s 'state :title (or title "session ready"))
  (aob-acp--persist)
  ;; the goal goes in before the first prompt and the flush waits for it,
  ;; so the work starts with the agent already holding what it is for
  (unless (run-hook-with-args-until-success
           'aob-acp-before-first-prompt-functions s)
    (if-let* ((want (aob-session-ref s :want-goal)))
        (progn (aob-session-put s :want-goal nil)
               (aob-acp--goal s want (lambda () (aob-acp--flush-queue s))))
      (aob-acp--flush-queue s))))

(defvar aob-acp--opened-any nil
  "Non-nil once this Emacs has opened an ACP session.")

(defvar aob-acp-mcp-servers nil
  "MCP servers to hand the session being created, as a list of plists.
Bound around a spawn, like `aob-acp-session-refs\=': the servers a
session gets are decided by whoever opens it and are part of the
session/new call, so there is no later moment to add one.  Each is
(:name :command :args :env), env a list of (:name :value).")

(defun aob-acp--mcp-p (init)
  "Whether the adapter INIT came from says anything about MCP at all.
The spec has every agent take a stdio server, and claude and codex
both advertise mcpCapabilities.  An adapter that names it nowhere is
one this does not know, and a session/new it rejects is a session that
never opens — so it is sent the empty vector it was sent before."
  (or (null init)
      (and (plist-member (plist-get init :agentCapabilities) :mcpCapabilities)
           t)))

(defcustom aob-acp-presets
  '(("claude" :agent "claude" :mode "auto")
    ("claude-isolated" :agent "claude" :worktree t :mode "bypassPermissions")
    ("codex" :agent "codex")
    ("codex-isolated" :agent "codex" :worktree t :mode "agent-full-access")
    ("gemini" :agent "gemini")
    ("pi" :agent "pi")
    ("hermes" :agent "hermes"))
  "How a session is started: NAME → plist over one agent.
:agent names the adapter in `aob-acp-agents'; the rest is what this way
of working wants from it — :worktree for a checkout of its own, :mode a
permission mode applied as it opens, :model, and :config, a plist of
config options set once the session is up.

An isolated worker is not another agent, it is the same one asked for
different things, which is why it is a preset and not a second entry in
the agent table."
  :type '(alist :key-type string :value-type plist)
  :group 'aob)

(defun aob-acp-preset (name)
  "NAME as a preset plist.
A name that is only an agent is a preset of itself, so a caller may
pass either and old configuration keeps working."
  (or (cdr (assoc name aob-acp-presets))
      (and (assoc name aob-acp-agents) (list :agent name))))

(defun aob-acp-preset-agent (name)
  "The adapter NAME runs on."
  (or (plist-get (aob-acp-preset name) :agent) name))

(defun aob-acp-names ()
  "Everything that can be spawned: presets, and agents without one."
  (let ((presets (mapcar #'car aob-acp-presets)))
    (append presets
            (seq-remove (lambda (a) (member a presets))
                        (mapcar #'car aob-acp-agents)))))

(defcustom aob-acp-project-mcp-file ".mcp.json"
  "Where a checkout declares the MCP servers its work needs.
The file claude reads when it runs itself.  Over ACP nothing reads it
for us, so a session opened here would have the harness\='s own servers
and not the ones the repository says its work needs — notion, a
database, whatever it declared — and the difference would look like the
agent forgetting how to use them."
  :type 'string :group 'aob)

(defun aob-acp--mcp-kind (spec)
  "Which transport SPEC names: stdio, http or sse."
  (let ((said (plist-get spec :type)))
    (cond ((member said '("http" "sse")) (intern said))
          ((plist-get spec :url) 'http)
          (t 'stdio))))

(defun aob-acp--mcp-takes-p (kind init)
  "Whether the adapter INIT came from takes a server of KIND.
Stdio is the baseline every agent takes; http and sse are advertised,
and one sent to an adapter that never claimed it fails the session it
was sent with."
  (let ((caps (plist-get (plist-get init :agentCapabilities) :mcpCapabilities)))
    (pcase kind
      ('stdio t)
      ('http (and (plist-get caps :http) t))
      ('sse (and (plist-get caps :sse) t)))))

(defun aob-acp--mcp-key (key)
  (if (keywordp key) (substring (symbol-name key) 1) (format "%s" key)))

(defun aob-acp--mcp-pairs (value)
  "VALUE, headers or env in any of their shapes, as ACP name/value entries."
  (let ((items (cond ((hash-table-p value)
                      (let (out)
                        (maphash (lambda (k v) (push (cons k v) out)) value)
                        (nreverse out)))
                     ((vectorp value) (append value nil))
                     (t value))))
    (cl-flet ((entry (key val)
             (when-let* ((text (ygg-agent-mcp-value val)))
               (list :name (aob-acp--mcp-key key) :value text))))
      (cond
       ((not (consp items)) nil)
       ((consp (car items))
        (delq nil (mapcar (lambda (item)
                            (if (and (proper-list-p item) (plist-member item :name))
                                (entry (plist-get item :name) (plist-get item :value))
                              (entry (car item) (cdr item))))
                          items)))
       (t
        (let (out)
          (while items
            (push (entry (pop items) (pop items)) out))
          (nreverse (delq nil out))))))))

(defun aob-acp--mcp-entry (name spec)
  "SPEC under NAME as the entry a session/new carries, or nil."
  (pcase (aob-acp--mcp-kind spec)
    ('stdio
     (when-let* ((command (plist-get spec :command)))
       (list :name name :command command
             :args (vconcat (plist-get spec :args))
             :env (vconcat (aob-acp--mcp-pairs (plist-get spec :env))))))
    (kind
     (when-let* ((url (plist-get spec :url)))
       (list :name name :type (symbol-name kind) :url url
             :headers (vconcat (aob-acp--mcp-pairs (plist-get spec :headers))))))))

(defun aob-acp--mcp-probe (url)
  "Ask URL for its tools, and say how that went."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json") ("Accept" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-serialize (list :jsonrpc "2.0" :id 1 :method "tools/list"
                                 :params (make-hash-table)))
           'utf-8))
         (buf (ignore-errors (url-retrieve-synchronously url t t 3))))
    (if (not buf)
        "no answer"
      (with-current-buffer buf
        (goto-char (point-min))
        (if (not (re-search-forward "\r?\n\r?\n" nil t))
            "no body"
          (let* ((json (ignore-errors (json-parse-buffer :object-type 'plist
                                                         :array-type 'list
                                                         :false-object nil
                                                         :null-object nil)))
                 (tools (plist-get (plist-get json :result) :tools)))
            (cond ((null json) "not JSON")
                  (tools (format "%d tools: %s%s" (length tools)
                                 (string-join
                                  (seq-take (mapcar (lambda (tool)
                                                      (plist-get tool :name))
                                                    tools)
                                            4)
                                  " ")
                                 (if (> (length tools) 4) " …" "")))
                  (t "answered, no tools"))))))))

;;;###autoload
(defvar-local aob-acp-mcp--session nil
  "The session the listing in this buffer is about.")

(defvar aob-acp-mcp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g r") #'aob-acp-mcp-refresh)
    (define-key map "R" #'aob-acp-mcp-restart)
    map)
  "Keys of the server listing.")

(define-derived-mode aob-acp-mcp-mode special-mode "aob-mcp"
  "What a session can reach, and what it was given.")

(defun aob-acp-mcp-refresh ()
  "Ask every server again."
  (interactive)
  (when aob-acp-mcp--session (aob-acp-mcp aob-acp-mcp--session)))

(defun aob-acp-mcp-restart ()
  "Reload this conversation into a process that gets every server.
The servers a session has are the ones it was opened with; there is no
adding one to a session already running.  Its ACP id outlives the
process, so this is the same conversation, with them."
  (interactive)
  (let ((s aob-acp-mcp--session))
    (unless s (user-error "aob: no session here"))
    (when (y-or-n-p (format "Reload %s into a process with every server? "
                            (aob-session-name s)))
      (aob-acp-restart s))))

(defun aob-acp--mcp-target (entry)
  "What ENTRY points at, without the secret that gets you in.
A listing is the thing people screenshot."
  (or (when-let* ((url (plist-get entry :url)))
        (car (split-string url "?")))
      (string-join (cons (or (plist-get entry :command) "")
                         (append (plist-get entry :args) nil))
                   " ")))

(defun aob-acp--mcp-status (entry)
  "Whether ENTRY can be reached from here, as far as here can tell."
  (cond ((plist-get entry :url) (aob-acp--mcp-probe (plist-get entry :url)))
        ((plist-get entry :command)
         (if (executable-find (plist-get entry :command))
             "ready" "not on PATH"))
        (t "")))

(defun aob-acp--mcp-known (s)
  "Every server this session could have, by name, as (ENTRY . WHERE)."
  (let* ((project (aob-session-project s))
         (out nil))
    (dolist (entry (aob-session-ref s :mcp-sent))
      (push (cons entry "session") out))
    (dolist (entry (and (fboundp 'ygg-agent-user-mcp-servers)
                        (ignore-errors
                          (ygg-agent-user-mcp-servers
                           (aob-session-ref s :agent) project))))
      (unless (seq-find (lambda (cell) (equal (plist-get (car cell) :name)
                                              (plist-get entry :name)))
                        out)
        (push (cons entry "config") out)))
    (dolist (entry (ignore-errors (aob-acp-project-mcp-servers project)))
      (unless (seq-find (lambda (cell) (equal (plist-get (car cell) :name)
                                              (plist-get entry :name)))
                        out)
        (push (cons entry "project") out)))
    (when (and (fboundp 'aob-mcp-host-live-p) (aob-mcp-host-live-p)
               (not (seq-find (lambda (cell)
                                (equal (plist-get (car cell) :name)
                                       (bound-and-true-p aob-mcp-host-name)))
                              out)))
      ;; with the key, or the probe asks the way a stranger would and is
      ;; told 401 by the thing it is trying to describe
      (push (cons (if (fboundp 'aob-mcp-host-spec)
                      (aob-mcp-host-spec "listing")
                    (list :name (or (bound-and-true-p aob-mcp-host-name) "aob")
                          :type "http" :url (aob-mcp-url)))
                  "emacs")
            out))
    (nreverse out)))

;;;###autoload
(defun aob-acp-mcp (s)
  "Every MCP server S can reach, and whether S was given it.
The servers a session has are the ones it was opened with, so a
session older than a server is a session without it — which this says
rather than leaving you to wonder, and R puts right."
  (interactive (list (aob-target)))
  (let* ((caps (plist-get (aob-session-ref s :agent-caps) :mcpCapabilities))
         (sent (mapcar (lambda (e) (plist-get e :name))
                       (aob-session-ref s :mcp-sent)))
         (rows (aob-acp--mcp-known s))
         (buf (get-buffer-create (format "*aob-mcp: %s*" (aob-session-name s)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (fresh (not (derived-mode-p 'aob-acp-mcp-mode))))
        (when fresh (aob-acp-mcp-mode))
        (setq aob-acp-mcp--session s)
        (aob--redraw-keeping-lines
         (lambda ()
           (erase-buffer)
           (insert (propertize (format "%s" (aob-session-name s)) 'face 'bold)
                   (format "  ·  takes %s\n\n"
                           (if caps
                               (string-join
                                (delq nil (list (and (plist-get caps :http) "http")
                                                (and (plist-get caps :sse) "sse")
                                                "stdio"))
                                " · ")
                             "no kind it ever named")))
           (if (null rows)
               (insert "no servers anywhere: none configured, none declared, none served\n")
             (pcase-dolist (`(,entry . ,where) rows)
               (let* ((name (plist-get entry :name))
                      (has (member name sent)))
                 (insert (format "%s %-14s %-6s %s\n"
                                 (propertize (if has "✓" "·")
                                             'face (if has 'success 'shadow))
                                 name
                                 (or (plist-get entry :type) "stdio")
                                 (propertize (aob-acp--mcp-target entry) 'face 'shadow)))
                 (insert (format "  %-14s %s\n" ""
                                 (propertize (format "%s · from %s%s"
                                                     (aob-acp--mcp-status entry)
                                                     where
                                                     (if has "" " · not in this session"))
                                             'face (if has 'shadow 'warning)))))))
           (unless (and sent (= (length sent) (length rows)))
             (insert (propertize
                      "\nR reloads this conversation into a process that gets them all\n"
                      'face 'warning)))))
        (when fresh (goto-char (point-min)))))
    (display-buffer buf)))

(defun aob-acp-project-mcp-servers (project)
  "The servers PROJECT declares in its own MCP file, as entries.
Read as written: a checkout that says its work needs a server is a
checkout whose agents get it, whoever opened them."
  (when-let* ((project)
              (file (expand-file-name aob-acp-project-mcp-file project))
              ((file-readable-p file)))
    (ignore-errors
      (let* ((json (with-temp-buffer
                     (insert-file-contents file)
                     (json-parse-buffer :object-type 'plist
                                        :null-object nil
                                        :false-object nil)))
             (declared (plist-get json :mcpServers))
             out)
        (while declared
          (let* ((key (pop declared))
                 (spec (pop declared))
                 (name (if (keywordp key)
                           (substring (symbol-name key) 1)
                         (format "%s" key))))
            (when-let* ((entry (aob-acp--mcp-entry name spec)))
              (push entry out))))
        (nreverse out)))))

(defconst aob-acp--lat-launcher "cd \"$1\" && PATH=\"$2:$PATH\" exec \"$3\" mcp"
  "Shell script run as sh -c with root, node directory and lat as $1 to $3.")

(defun aob-acp-lat-entry (dir &rest taken)
  "The lat MCP server for DIR as an entry, or nil.
Nil when DIR is remote or has no lat.md, lat or node is not installed, or
one of the entry lists TAKEN already has a server named lat.  lat finds
lat.md from its working directory and ACP stdio servers have none, so sh
enters the root first; node's directory is put on PATH because lat's
shebang needs it.  Without node lat cannot run, so there is no entry."
  (when-let* ((dir)
              ((not (file-remote-p dir)))
              ((file-directory-p (expand-file-name "lat.md" dir)))
              ((not (seq-find (lambda (entries)
                                (seq-find (lambda (e) (equal (plist-get e :name) "lat"))
                                          entries))
                              taken)))
              (lat (executable-find "lat"))
              (node (executable-find "node")))
    (list :name "lat" :command "/bin/sh"
          :args (vector "-c" aob-acp--lat-launcher "sh"
                        (directory-file-name (expand-file-name dir))
                        (directory-file-name (file-name-directory (expand-file-name node)))
                        (expand-file-name lat))
          :env (vector))))

(defun aob-acp--mcp-servers (&optional init project)
  "What to send as mcpServers: what PROJECT declares and what the caller bound.
INIT is the adapter\='s initialize result, when the caller has it; a
server of a kind it never advertised is left out rather than failing
the session it would have ridden in.  The caller\='s own come last, so a
repository that declares a name of its own keeps it."
  (if (not (aob-acp--mcp-p init))
      (vector)
    (let* ((mine (delq nil
                       (mapcar (lambda (server)
                                 (aob-acp--mcp-entry (plist-get server :name)
                                                     server))
                               aob-acp-mcp-servers)))
           (theirs (seq-remove
                    (lambda (entry)
                      (seq-find (lambda (one)
                                  (equal (plist-get one :name)
                                         (plist-get entry :name)))
                                mine))
                    (aob-acp-project-mcp-servers project)))
           (lat (aob-acp-lat-entry project mine
                                   (aob-acp-project-mcp-servers project)))
           (all (seq-filter
                 (lambda (entry)
                   (aob-acp--mcp-takes-p
                    (if (plist-get entry :type)
                        (intern (plist-get entry :type))
                      'stdio)
                    init))
                 (append theirs mine (and lat (list lat))))))
      (vconcat (if (file-remote-p (or project default-directory))
                   all
                 (delq nil (mapcar #'aob-acp--mcp-absolute all)))))))

(defvar aob-acp--mcp-dropped nil
  "Stdio servers left out of the mcpServers now being built, as (NAME . COMMAND).")

(defun aob-acp--mcp-absolute (entry)
  "ENTRY with its stdio command as an absolute path, or nil when none is found.
The schema takes only an absolute path, and an adapter that spawns the
bare name resolves it against its own PATH, not the one Emacs has."
  (let ((command (plist-get entry :command)))
    (cond ((or (member (plist-get entry :type) '("http" "sse"))
               (not (stringp command))
               (file-name-absolute-p command))
           entry)
          ((when-let* ((found (executable-find command)))
             (plist-put (copy-sequence entry) :command found)))
          (t (push (cons (plist-get entry :name) command) aob-acp--mcp-dropped)
             nil))))

(defun aob-acp--warn-mcp-dropped (s dropped)
  "Say once in S's trace each server in DROPPED that went without a command."
  (dolist (cell (reverse dropped))
    (unless (member cell (aob-session-ref s :mcp-warned))
      (aob-session-put s :mcp-warned (cons cell (aob-session-ref s :mcp-warned)))
      (aob-event s 'state :title (format "mcp server %s left out: %s not found"
                                         (car cell) (cdr cell))))))

(defvar aob-acp-session-refs nil
  "Plist put on a session the moment it is created, before it connects.
Bind this around a spawn to tag one: `aob-acp--with-init' fires inline on
an already-live connection, so an `aob-session-put' after the spawn
returns would lose the race against the request that carries the tag.")

(defun aob-acp--session-cap (init key)
  "Whether INIT declares session capability KEY.
A capability map answers with an object describing the capability, and
one with nothing to describe is the empty object — which parses to nil
here, exactly like a key that was never there.  Presence is the signal,
so presence is what is asked.  An adapter that spells a capability out
as false is read as offering it; the spec omits what it does not have,
and no adapter seen here writes false."
  (let ((caps (or (plist-get (plist-get init :agentCapabilities)
                             :sessionCapabilities)
                  (plist-get init :sessionCapabilities))))
    (and (plist-member caps key) t)))

(defun aob-acp--extra-dirs (s init)
  "S's `:extra-dirs' as an additionalDirectories vector, or nil.
Gated on the capability — an adapter that never advertised it rejects the
parameter outright rather than ignoring it."
  (when (aob-acp--session-cap init :additionalDirectories)
    (when-let* ((dirs (aob-session-ref s :extra-dirs)))
      (vconcat (mapcar #'aob-acp--wire-dir dirs)))))

;;; Auth — the agent's own login, run as the agent advertised it

(defun aob-acp--auth-error-p (err)
  "Whether ERR is the agent saying it needs a login first."
  (eql (plist-get err :code) -32000))

(defun aob-acp--auth-methods (proc)
  "The logins PROC's agent advertised when it started, each as it sent it."
  (plist-get (aob-acp--conn-init proc) :authMethods))

(defun aob-acp--auth-message (s err)
  "ERR, S's agent refusing for want of a login, as what to do next."
  (let ((agent (or (aob-session-ref s :agent) "the agent"))
        (why (plist-get err :message))
        (names (mapcar (lambda (m) (plist-get m :name))
                       (aob-acp--auth-methods (aob-acp--proc-of s)))))
    (format "%s needs a login%s; %s"
            agent
            (if (and why (not (equal why "Authentication required")))
                (format " (%s)" why)
              "")
            (if names
                (format "log in with %s" (string-join names " or "))
              (format "log in with %s's own CLI, then start the session again"
                      agent)))))

(defun aob-acp--auth-terminal-p (method)
  "Whether METHOD is a login the client runs in a terminal."
  (or (equal (plist-get method :type) "terminal")
      (and (plist-get (plist-get method :_meta) :terminal-auth) t)))

(defun aob-acp--auth-argv (s method)
  "The command running S's agent's terminal login METHOD.
A typed method is the agent's own program with its args on the end; the
older `terminal-auth' extension names the whole command itself."
  (let ((legacy (and (not (equal (plist-get method :type) "terminal"))
                     (plist-get (plist-get method :_meta) :terminal-auth))))
    (if legacy
        (cons (plist-get legacy :command) (plist-get legacy :args))
      (append (plist-get (cdr (assoc (aob-session-ref s :agent) aob-acp-agents))
                         :command)
              (plist-get method :args)))))

(defun aob-acp--auth-env (method)
  "METHOD's environment as \"VAR=VAL\" strings."
  (let ((env (or (plist-get method :env)
                 (plist-get (plist-get (plist-get method :_meta) :terminal-auth)
                            :env)))
        out)
    (while env
      (push (format "%s=%s" (substring (symbol-name (pop env)) 1) (pop env)) out))
    (nreverse out)))

(declare-function term-char-mode "term" ())

(defun aob-acp--auth-terminal (s method done)
  "Run S's agent's terminal login METHOD in a terminal buffer.
DONE is called once with whether it exited 0.  It runs where and with
the environment the connection was started with, METHOD's own on top:
another environment is another config home, and a login there is one
this connection never reads."
  (require 'term)
  (let* ((proc (aob-acp--proc-of s))
         (argv (funcall aob-acp-command-function (aob-acp--auth-argv s method)))
         (process-environment (append (aob-acp--auth-env method)
                                      (or (process-get proc 'aob-env)
                                          process-environment)))
         (default-directory (aob-session-project s))
         (buf (apply #'make-term (format "login %s" (aob-session-ref s :agent))
                     (car argv) nil (cdr argv)))
         (told nil))
    (add-function :after (process-sentinel (get-buffer-process buf))
                  (lambda (p _event)
                    (unless (or told (process-live-p p))
                      (setq told t)
                      (funcall done (eql (process-exit-status p) 0)))))
    (with-current-buffer buf (term-char-mode))
    (unless noninteractive (pop-to-buffer buf))
    buf))

(defun aob-acp--auth-offer (s err then)
  "Offer the logins S's agent advertised as a Decision, ERR having refused.
THEN runs once a login went through; nil means S is past opening, and
refused a turn.  An agent that advertised none leaves nothing to offer,
and S is told how to log in instead."
  (let ((methods (aob-acp--auth-methods (aob-acp--proc-of s)))
        (why (aob-acp--auth-message s err)))
    (if (null methods)
        (aob-acp--auth-failed s then err)
      (let ((d (list :kind 'auth :then then :error err
                     :was (aob-session-state s)
                     :title why
                     :options (append
                               (mapcar (lambda (m)
                                         (list :optionId (plist-get m :id)
                                               :name (plist-get m :name)
                                               :kind "allow_once"
                                               :method m))
                                       methods)
                               (list (list :optionId "decline" :name "Not now"
                                           :kind "decline"))))))
        (push d (aob-session-decisions s))
        (aob-set-state s 'blocked)
        (let ((ev (aob-event s 'permission :title why)))
          (nconc d (list :seq (plist-get ev :seq))))
        (message "aob: %s" why)
        (dolist (fn aob-acp-request-functions)
          (condition-case e (funcall fn s d nil)
            (error (message "aob-acp-request-functions: %S" e))))))))

(defun aob-acp--auth-chosen (s decision answer)
  "Log S's agent in with the method ANSWER picks from DECISION.
A terminal method is the agent's own login run in a terminal, and the
schema forbids passing it to `authenticate' (claude-agent-acp answers
Method not implemented); any other the agent carries out itself.  No
method, a decline, fails S with what to do instead."
  (let* ((method (plist-get (seq-find (lambda (o) (equal (plist-get o :optionId) answer))
                                      (plist-get decision :options))
                            :method))
         (then (plist-get decision :then))
         (agent (or (aob-session-ref s :agent) "the agent"))
         (name (plist-get method :name)))
    (cond
     ((null method)
      (aob-acp--auth-failed s then (plist-get decision :error)))
     ((aob-acp--auth-terminal-p method)
      (aob-event s 'state :title (format "logging in to %s: %s" agent name))
      (aob-acp--auth-terminal
       s method
       (lambda (ok)
         (cond ((not (aob-session-get (aob-session-id s))))
               ((not ok)
                (aob-acp--auth-failed
                 s then (list :message
                         (format "%s's login %s did not finish; run it again, or log in with %s's own CLI"
                                 agent name agent))))
               ((equal (plist-get method :type) "terminal")
                (aob-acp--auth-done s name then))
               (t (aob-acp--authenticate s method then))))))
     (t (aob-acp--authenticate s method then)))))

(defun aob-acp--authenticate (s method then)
  "Ask S's agent to log in with METHOD itself, then run THEN.
Sent on S's behalf, so a page the agent asks to open for it, scoped to
this request rather than a session, finds its way back to S."
  (let ((agent (or (aob-session-ref s :agent) "the agent"))
        (name (plist-get method :name)))
    (aob-acp--request
     s "authenticate" (list :methodId (plist-get method :id))
     (lambda (_res err)
       (when (aob-session-get (aob-session-id s))
         (if err
             (aob-acp--auth-failed
              s then (list :message
                      (format "%s could not log in with %s (%s); pick another login, or log in with %s's own CLI"
                              agent name (or (plist-get err :message) "error") agent)))
           (aob-acp--auth-done s name then)))))))

(defun aob-acp--auth-done (s name then)
  "Note S's agent logged in with NAME, then run THEN."
  (aob-event s 'state :title (format "logged in to %s: %s"
                                    (or (aob-session-ref s :agent) "the agent") name))
  (when then (funcall then)))

(defun aob-acp--auth-failed (s opening err)
  "Say ERR on S.  A session still OPENING fails with it; one already open
keeps going, since only the turn was refused."
  (if opening
      (aob-acp--fail s err)
    (aob-event s 'error :title (if (aob-acp--auth-error-p err)
                                   (aob-acp--auth-message s err)
                                 (plist-get err :message)))))

(defun aob-acp--auth-label (status)
  "STATUS, who an agent says it is logged in as, in a header's few words."
  (when-let* ((label (plist-get status :label)))
    (let ((who (or (plist-get (plist-get status :account) :email)
                   (plist-get status :detail))))
      (propertize (if who (format "%s (%s)" label who) label)
                  'face (if (equal (plist-get status :kind) "none") 'warning 'shadow)))))

(defun aob-acp--show-auth (s status)
  "Show STATUS, the login of S's connection, on S."
  (aob-session-put s :auth-label (aob-acp--auth-label status))
  (aob--dirty s))

(defun aob-acp--auth-status (proc status)
  "Keep STATUS, who PROC's agent says it is logged in as, and show it.
The update names no session: a login belongs to the connection, so each
session on it shows it, and one attaching later reads it from PROC."
  (process-put proc 'aob-auth-status status)
  (dolist (s (aob-acp--conn-sessions proc))
    (aob-acp--show-auth s status)))

(defun aob-acp--connect (s open then)
  "Attach S to its agent's shared connection and open its ACP session."
  (let* ((agent (aob-session-ref s :agent))
         (project (aob-session-project s))
         (aob-acp--session-env (aob-acp--agent-env s))
         (proc (or (aob-acp--live-conn agent project)
                   (aob-acp--start-conn agent project))))
    (setf (aob-session-conn s) proc)
    (aob-acp--show-auth s (process-get proc 'aob-auth-status))
    (aob-acp--with-init
     proc
     (lambda (init err)
       (when (aob-session-get (aob-session-id s)) ; not killed meanwhile
         (if err
             (aob-acp--fail s err)
           (aob-session-put s :agent-caps (plist-get init :agentCapabilities))
           ;; extensions live in the top-level `_meta', not the capabilities:
           ;; steering and the goal control method are both advertised there
           (aob-session-put s :agent-meta (plist-get init :_meta))
           (pcase-let ((`(,method ,params ,pre-sid) (funcall open init)))
             ;; every open method — new, load, resume, fork — funnels through
             ;; here, and none of them restores a scope implicitly
             (when-let* ((dirs (aob-acp--extra-dirs s init)))
               (setq params (plist-put (copy-sequence params)
                                       :additionalDirectories dirs)))
             (when pre-sid (aob-acp--register proc pre-sid s))
             (aob-acp--send-open s method params then t))))))))

(defun aob-acp--send-open (s method params then retry)
  "Send S's open METHOD with PARAMS, THEN taking S and the result.
A refusal for want of a login offers the agent's own logins; RETRY
says the open may go out once more after one succeeds, and a second
refusal fails S rather than asking again."
  (aob-acp--request
   s method params
   (lambda (res err)
     (cond
      ((not err) (funcall then s res))
      ((and retry (aob-acp--auth-error-p err))
       (aob-acp--auth-offer
        s err (lambda ()
                (when (aob-session-get (aob-session-id s))
                  (aob-acp--send-open s method params then nil)))))
      (t (aob-acp--fail s err))))))

(defun aob-acp--open (agent name project dir open then &optional prepare)
  "Create a session for AGENT on the shared (AGENT . PROJECT) connection.
OPEN is called with the initialize result and returns (METHOD PARAMS
&optional PRE-SID); THEN receives the session and the open result.
PREPARE, when given, runs as (PREPARE S DONE) before the connection is
touched — DONE takes nil or an error plist; the session shows starting
the whole way through, so nothing here ever blocks.

Every entry point — new, load, fork, resume, list — funnels through
here, so PROJECT and DIR are normalized here and nowhere else: one
spelling of a folder, or the same tree keys two connections and every
buffer opened off the session stands somewhere slightly different.  The
JSON-RPC `:cwd' keeps its `directory-file-name' spelling, which is what
the adapters store their sessions under."
  (let* ((project (file-name-as-directory (expand-file-name project)))
         (dir (file-name-as-directory (expand-file-name (or dir project))))
         (s (aob-create-session :id (concat "acp:" name) :backend 'acp
                                :name name :project project :dir dir
                                :state 'starting
                                :refs (append (list :agent agent)
                                              aob-acp-session-refs))))
    (setq aob-acp--opened-any t)
    ;; the servers bound by whoever opened this session are what it was
    ;; meant to be handed.  The handshake is asynchronous, so by the time
    ;; the adapter asks what to open with, a caller's `let' is long
    ;; unwound and the session would go out with none of them
    (aob-session-put s :mcp-declared aob-acp-mcp-servers)
    (let ((servers aob-acp-mcp-servers)
          (told (if (functionp aob-acp-system-append)
                    (funcall aob-acp-system-append s)
                  aob-acp-system-append))
          (fn open))
      (setq open (lambda (init)
                   (let* ((aob-acp-mcp-servers servers)
                          (aob-acp--mcp-dropped nil)
                          (spec (funcall fn init)))
                     (aob-acp--warn-mcp-dropped s aob-acp--mcp-dropped)
                     ;; what went out, kept where it can be read back: an
                     ;; agent that cannot reach a server is a question
                     ;; about what it was handed
                     (when-let* ((params (cadr spec)))
                       (aob-session-put s :mcp-sent
                                        (append (plist-get params :mcpServers) nil))
                       (when (and (stringp told) (not (string-empty-p told)))
                         (setcar (cdr spec)
                                 (plist-put params :_meta
                                            (plist-put (copy-sequence (plist-get params :_meta))
                                                       :systemPrompt (list :append told)))))
                       (setcar (cdr spec)
                               (aob-acp--with-limits s init (car spec) (cadr spec))))
                     spec))))
    (if prepare
        (funcall prepare s
                 (lambda (err)
                   (when (aob-session-get (aob-session-id s))
                     (if err (aob-acp--fail s err)
                       (aob-acp--connect s open then)))))
      (aob-acp--connect s open then))
    s))

;;;###autoload
(defun aob-acp-run (intent)
  "The fast path: spawn the default agent straight into INTENT."
  (interactive (list (read-string (format "%s » " aob-acp-default-agent))))
  (aob-acp-spawn aob-acp-default-agent intent))

;;;###autoload
(defun aob-acp-spawn (agent &optional intent atts name tree)
  "Spawn ACP AGENT and send INTENT as its first prompt turn.
Whether the session gets an isolated worktree comes from the agent's
definition in `aob-acp-agents'.  INTENT (with image ATTS) is queued
through the handshake and fires the moment the session is ready.
NAME stands in for the agent when the session is numbered, so a session
started for something already named says so in every list it shows up in.
TREE, else aob-acp-start-worktree, is a worktree picked to work in, as
aob-acp-read-worktree answers; an isolated preset keeps its own instead."
  (interactive
   (let* ((agent (completing-read "ACP agent: " (aob-acp-names)
                                  nil t nil nil aob-acp-default-agent))
          (tree (unless (plist-get (aob-acp-preset agent) :worktree)
                  (aob-acp-read-worktree (aob-acp--project)))))
     (list agent (read-string (format "%s » " agent)) nil nil tree)))
  (let* ((spec (aob-acp-preset agent))
         (base (or (plist-get spec :agent) agent))
         (project (aob-acp--project))
         (worktree (plist-get spec :worktree))
         (tree (and (not worktree) (or tree aob-acp-start-worktree)))
         (dir (cond ((consp tree) (car tree))
                    (tree (aob-acp--real-dir tree))
                    (worktree (aob-acp--worktree-path project agent))
                    (t project)))
         (cwd (directory-file-name (expand-file-name dir)))
         (s (aob-acp--open base (aob-acp--gen-name (or name agent)) project dir
                           (lambda (init)
                             (list "session/new"
                                   (list :cwd (aob-acp--wire-dir cwd)
                                         :mcpServers
                                         (aob-acp--mcp-servers init project))))
                           (lambda (s res) (aob-acp--session-opened s res))
                           (cond ((consp tree)
                                  (lambda (_s done)
                                    (aob-acp--worktree-make project dir done (cdr tree))))
                                 (worktree
                                  (lambda (_s done)
                                    (aob-acp--worktree-make project dir done)))))))
    (aob-session-put s :preset agent)
    (when name (aob-session-put s :named-by-user t))
    (when-let* ((want (plist-get spec :mode)))
      (aob-session-put s :want-mode want))
    (when-let* ((want (plist-get spec :model)))
      (aob-session-put s :want-model want))
    (when-let* ((want (plist-get spec :config)))
      (aob-session-put s :want-config want))
    (when (or atts (and intent (not (string-empty-p intent))))
      (aob-acp--queue s intent atts)
      (aob-set-state s 'working))
    (when (and aob-acp-show-trace (not noninteractive)
               (fboundp 'aob-trace-buffer))
      (ygg-ui-show (aob-trace-buffer s) t))
    (message "aob: %s started in %s" (aob-session-name s)
             (abbreviate-file-name dir))
    s))

;;; Modes, models, fork — the adapter's session extensions

(defvar aob-acp--pick-map nil)

(defun aob-acp--annotate-pick (cand)
  (when-let* ((x (cdr (assoc cand aob-acp--pick-map)))
              (desc (cdr x)))
    (concat (propertize " " 'display '(space :align-to 22))
            (propertize desc 'face 'completions-annotations))))

(defun aob-acp--pick-plist (prompt items name-key id-key current)
  ;; descriptions matter here: dontAsk DENIES, bypassPermissions allows —
  ;; a name-only picker invites exactly the wrong choice
  (let* ((aob-acp--pick-map
          (mapcar (lambda (m) (cons (or (plist-get m name-key)
                                        (plist-get m id-key))
                                    (cons (plist-get m id-key)
                                          (plist-get m :description))))
                  items))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (annotation-function . aob-acp--annotate-pick)
                                 (display-sort-function . identity))
                    (complete-with-action action (mapcar #'car aob-acp--pick-map)
                                          str pred))))
         (pick (completing-read (format "%s (now %s): " prompt (or current "?"))
                                table nil t)))
    (cadr (assoc pick aob-acp--pick-map))))

(defcustom aob-acp-modes
  '(("claude"
     (:id "default" :name "default" :description "asks before edits and commands")
     (:id "acceptEdits" :name "accept edits" :description "edits without asking")
     (:id "plan" :name "plan" :description "reads and plans, changes nothing")
     (:id "auto" :name "auto" :description "a classifier decides what to ask")
     (:id "dontAsk" :name "don't ask" :description "DENIES whatever it would ask")
     (:id "bypassPermissions" :name "bypass" :description "asks about nothing")))
  "Modes to offer per agent before a session is up to advertise its own.
A session asleep, or not yet through its handshake, has said nothing
about what it takes; the choice is kept and put to it when it wakes."
  :type '(alist :key-type string :value-type (repeat plist))
  :group 'aob)

(defun aob-acp--mode-choices (s)
  "The modes S can be put in: what it advertised, else what its agent takes."
  (or (plist-get (aob-session-ref s :modes) :availableModes)
      (let ((agent (or (aob-session-ref s :agent) "")))
        (cdr (seq-find (lambda (cell) (string-match-p (regexp-quote (car cell)) agent))
                       aob-acp-modes)))))

(defun aob-acp--awake-p (s)
  "Whether S has a connection that can be asked anything."
  (and (aob-session-conn s) (aob-session-ref s :modes) t))

(defun aob-acp--put-mode (s id)
  "Put S in mode ID now, or when it wakes if it cannot be asked yet."
  (if (aob-acp--awake-p s)
      (aob-acp--want-mode s id)
    ;; an asleep session is woken from its entry, not from itself: the
    ;; mode has to be in what the resume reads
    (when-let* ((entry (aob-session-ref s :asleep)))
      (aob-session-put s :asleep (plist-put entry :mode id)))
    (aob-session-put s :want-mode id)
    (aob-session-put s :mode-id id)
    (aob--dirty s)
    (message "aob: %s wakes in %s" (aob-session-name s) id)))

(defun aob-acp-set-mode (s)
  "Switch S's session mode (plan / auto / accept-edits…)."
  (interactive (list (aob-target)))
  (let ((modes (aob-acp--mode-choices s)))
    (unless modes (user-error "aob: %s advertises no modes" (aob-session-name s)))
    (aob-acp--put-mode s (aob-acp--pick-plist "Mode" modes :name :id
                                              (aob-session-ref s :mode-id)))))

(defun aob-acp-cycle-mode (s)
  "Step S to the next mode the agent advertises."
  (interactive (list (aob-target)))
  (let* ((ids (mapcar (lambda (m) (plist-get m :id)) (aob-acp--mode-choices s)))
         (now (aob-session-ref s :mode-id))
         (next (or (cadr (member now ids)) (car ids))))
    (unless ids (user-error "aob: %s advertises no modes" (aob-session-name s)))
    (aob-acp--put-mode s next)
    (message "aob: mode %s" next)))


(defcustom aob-acp-models
  '(("claude" "opus" "sonnet" "haiku")
    ("codex" "gpt-6-astra" "gpt-6-sol" "gpt-6-luna" "gpt-5.6-sol" "gpt-5.6-terra" "gpt-5.6-luna" "gpt-5.5"))
  "Models to offer per agent before a session is up to ask.
Keyed by the adapter, not by the preset: which models exist is a fact
about the agent, and every preset over it inherits them.
The live list arrives with the handshake; this only seeds the picker, and
a name that is not offered is reported rather than forced."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'aob)

(defun aob-acp--model-choices (s)
  "The models S can be put on: what it advertised, else what its agent takes."
  (or (mapcar (lambda (v) (plist-get v :value)) (cdr (aob-acp--model-info s)))
      (let ((agent (or (aob-session-ref s :agent) "")))
        (cdr (seq-find (lambda (cell) (string-match-p (regexp-quote (car cell)) agent))
                       aob-acp-models)))))

(defun aob-acp--model-asleep (s)
  "Choose the model asleep S wakes on; nothing is asked of it until then.
The resume reads the asleep entry, so that is where the choice goes."
  (let ((want (completing-read
               (format "Model on waking (now %s): "
                       (or (aob-session-ref s :model-id) "?"))
               (aob-acp--model-choices s))))
    (when (string-empty-p want)
      (user-error "aob: no model named"))
    (when-let* ((entry (aob-session-ref s :asleep)))
      (aob-session-put s :asleep (plist-put entry :model want)))
    (aob-session-put s :want-model want)
    (aob-session-put s :model-id want)
    (aob-session-kid-changed s)
    (aob--dirty s)
    (message "aob: %s wakes on %s" (aob-session-name s) want)))

(defun aob-acp-spawn-with (agent project model &optional intent tree)
  "Spawn AGENT on PROJECT with MODEL, sending INTENT as its first turn.
AGENT is a preset name: what it runs on, how it may act and what it
answers with come from `aob-acp-presets', so the only thing still asked
is where.  MODEL overrides the preset\='s own, for a one-off.

With no INTENT the first turn is written in a compose buffer rather than
the minibuffer, and the session is spawned when that is sent: a first
prompt is the longest one there is, and it can carry attachments.
TREE is the worktree of PROJECT it works in, as aob-acp-read-worktree answers."
  (interactive
   (let* ((preset (completing-read "Preset: " (aob-acp-names)
                                   nil t nil nil aob-acp-default-agent))
          ;; the project you are in is the answer nearly every time; a
          ;; prefix argument is for the times it is not
          (project (if current-prefix-arg
                       (completing-read
                        "Project: "
                        (and (fboundp 'ygg-project-roots)
                             (mapcar #'abbreviate-file-name (ygg-project-roots)))
                        nil nil
                        (abbreviate-file-name (or (aob-acp--project)
                                                  default-directory)))
                     (or (aob-acp--project) default-directory))))
     (list preset (expand-file-name project) nil nil
           (unless (plist-get (aob-acp-preset preset) :worktree)
             (aob-acp-read-worktree (expand-file-name project))))))
  (let ((dir (file-name-as-directory project)))
    (if intent
        (aob-acp--spawn-with-1 agent dir model intent nil tree)
      (let ((buf (aob-compose (lambda (text atts)
                                (aob-acp--spawn-with-1 agent dir model text atts tree))
                              nil (format "new %s" agent) dir)))
        (when tree
          (with-current-buffer buf
            (push (concat "⌥ " (file-name-nondirectory
                                (directory-file-name (if (consp tree) (car tree) tree))))
                  aob-compose--tags)))
        buf))))

(defun aob-acp--spawn-with-1 (agent dir model intent &optional atts tree)
  "Spawn AGENT in DIR on MODEL with INTENT and ATTS, in worktree TREE if given."
  (let* ((default-directory dir)
         (aob-acp-start-dir dir)
         (aob-acp-start-worktree tree)
         (s (aob-acp-spawn agent intent atts)))
    (when (and s model) (aob-session-put s :want-model model))
    s))

(defun aob-acp-config (s)
  "Set one of S's advertised config options beyond model/mode.
Surfaces the depth codex exposes — reasoning_effort, collaboration_mode,
fast-mode — and claude's effort/agent/fast, which m/M don't reach."
  (interactive (list (aob-target)))
  (let ((opts (seq-remove
               (lambda (o) (member (plist-get o :id) '("model" "mode")))
               (aob-session-ref s :config-options))))
    (unless opts
      (user-error "aob: %s advertises no extra config" (aob-session-name s)))
    (let* ((by-name (mapcar (lambda (o)
                              (cons (or (plist-get o :name) (plist-get o :id)) o))
                            opts))
           (opt (cdr (assoc (completing-read "Option: " (mapcar #'car by-name)
                                             nil t)
                            by-name)))
           (vals (aob-acp--config-values opt))
           (vby (mapcar (lambda (v)
                          (cons (or (plist-get v :name) (plist-get v :value)) v))
                        vals))
           (val (cdr (assoc (completing-read
                             (format "%s (now %s): " (plist-get opt :name)
                                     (or (plist-get opt :currentValue) "?"))
                             (mapcar #'car vby) nil t)
                            vby))))
      (aob-acp--set-config s (plist-get opt :id) (plist-get val :value)))))

(defun aob-acp-fork (s)
  "Fork S: same conversation so far, independent from here — steer an
alternative without losing the original, compare with range-diff later."
  (interactive (list (aob-target)))
  (let* ((src-id (aob-acp--acp-id s))
         (agent (aob-session-ref s :agent))
         (known (aob-acp--init-of s))
         (refusal (format "aob: %s does not offer session/fork" agent))
         (_ (when (and known (not (aob-acp--session-cap known :fork)))
              (user-error "%s" refusal)))
         (cwd (directory-file-name (expand-file-name (aob-session-dir s))))
         ;; a fork inherits the conversation, not the scope — carry the task
         ;; tag and its extra roots across or the copy loses its other repos
         (aob-acp-session-refs
          (append (when-let* ((task (aob-session-ref s :task))) (list :task task))
                  (when-let* ((dirs (aob-session-ref s :extra-dirs)))
                    (list :extra-dirs dirs))
                  (when-let* ((step (aob-session-ref s :task-step)))
                    (list :task-step step))
                  (aob-session-ref s :limits)
                  aob-acp-session-refs))
         (new (aob-acp--open
               agent (aob-acp--gen-name agent)
               (aob-session-project s) (aob-session-dir s)
               (lambda (init)
                 (list "session/fork"
                       (list :sessionId src-id :cwd (aob-acp--wire-dir cwd)
                             :mcpServers (aob-acp--mcp-servers init cwd))))
               (lambda (new res)
                 (aob-session-put new :cost-inherited t)
                 (aob-acp--session-opened
                  new res src-id
                  (format "forked from %s" (aob-session-name s)))
                 ;; fork results may omit modes — inherit the source's
                 (unless (aob-session-ref new :modes)
                   (aob-session-put new :modes (aob-session-ref s :modes))
                   (aob-session-put new :mode-id (aob-session-ref s :mode-id))))
               (unless known
                 (lambda (new done)
                   (aob-acp--await-cap new :fork refusal done))))))
    (message "aob: forking %s → %s" (aob-session-name s) (aob-session-name new))
    new))

(defun aob-acp--await-cap (s key refusal done)
  "Call DONE once S's connection has said whether it offers session KEY.
DONE gets nil when it does, and REFUSAL as the error when it does not."
  (let* ((agent (aob-session-ref s :agent))
         (project (aob-session-project s))
         (aob-acp--session-env (aob-acp--agent-env s))
         (proc (or (aob-acp--live-conn agent project)
                   (aob-acp--start-conn agent project))))
    (aob-acp--with-init
     proc
     (lambda (init err)
       (funcall done (cond (err err)
                           ((not (aob-acp--session-cap init key))
                            (list :message refusal))))))))

;; mode/model live under the localleader (layer-aob) so m / M stay vim
;; set-mark / middle-of-screen in agent buffers
;; no object-map key: f stays the find-char motion under the modal
;; layer — fork lives on the leader acp prefix instead

;;; Persistence — live sessions survive Emacs restarts via session/load

(defcustom aob-acp-history-limit 200
  "How many conversations the persist file remembers, newest first.
Sessions that left the registry are kept behind the live ones so a
killed agent stays resumable; this is where that history stops growing."
  :type 'integer :group 'aob)

(defcustom aob-acp-restore-preference 'resume
  "Which verb brings a stored conversation back.
`resume' reconnects without replay: the agent keeps its context and
sends nothing, which is what a restart with dozens of daemon workers
needs.  `load' asks for the whole history back as `session/update'
notifications, so the transcript is in the buffer again.  Either falls
back to the other when the adapter advertises only one.  A conversation
whose transcript is on disk is resumed either way where it can be, unless
a command names the verb: its history is read from the file, and a
replay would show it twice."
  :type '(choice (const resume) (const load)) :group 'aob)

(defun aob-acp--resumes-p (init)
  "Non-nil when INIT's initialize result advertises `session/resume'.
The spec puts `sessionCapabilities' at the top of the result; adapters
here nest it under `agentCapabilities', the way additionalDirectories is
already read."
  (aob-acp--session-cap init :resume))

(defun aob-acp--restore-open (init acp-id cwd name verb &optional pref seeded)
  "The open form that brings ACP-ID back, honouring PREF.
PREF defaults to `aob-acp-restore-preference'.  VERB is a cons cell
whose car is set to the verb that restored it, so the session can say
afterwards what it was given.  SEEDED says the trace already holds the
history, from the transcript on disk: a replay would only show it twice,
so an agent that can resume is resumed whatever PREF asks.  Under v2
there is no load; resume asks for the history with replayFrom instead."
  (let ((params (list :sessionId acp-id :cwd (aob-acp--wire-dir cwd)
                      :mcpServers (aob-acp--mcp-servers init cwd)))
        (pref (or pref aob-acp-restore-preference))
        (loads (plist-get (plist-get init :agentCapabilities) :loadSession))
        (resumes (aob-acp--resumes-p init)))
    (cond
     ((and resumes (eql (plist-get init :protocolVersion) 2))
      (if (and (eq pref 'load) (not seeded))
          (progn (setcar verb 'load)
                 (list "session/resume"
                       (append params (list :replayFrom (list :type "start")))
                       acp-id))
        (setcar verb 'resume)
        (list "session/resume" params acp-id)))
     ((and resumes (or (eq pref 'resume) (not loads) seeded))
      (setcar verb 'resume)
      (list "session/resume" params acp-id))
     (loads
      (setcar verb 'load)
      (list "session/load" params acp-id))
     (t
      (setcar verb 'fresh)
      (message "aob: %s can't restore history — fresh session" name)
      (list "session/new"
            (list :cwd (aob-acp--wire-dir cwd)
                  :mcpServers (aob-acp--mcp-servers init cwd)))))))

(defun aob-acp--entry (s)
  "S as the persist file records it, or nil for a session it cannot resume."
  (when (and (eq (aob-session-backend s) 'acp)
             (aob-session-ref s :acp-id)
             (not (aob-session-ref s :hidden)))
    (list :agent (aob-session-ref s :agent)
          :name (or (plist-get (aob-session-ref s :asleep) :name)
                    (aob-session-name s))
          :project (aob-session-project s)
          :dir (aob-session-dir s)
          :acp-id (aob-session-ref s :acp-id)
          :task (aob-session-ref s :task)
          :task-step (aob-session-ref s :task-step)
          :model (aob-session-ref s :model-id)
          :mode (aob-session-ref s :mode-id)
          :extra-dirs (aob-session-ref s :extra-dirs)
          :todo-file (aob-session-ref s :todo-file)
          :limits (aob-session-ref s :limits)
          :named-by-user (aob-session-ref s :named-by-user)
          :auto-named (aob-session-ref s :auto-named))))

(defun aob-acp--persist (&optional removed &rest _)
  "Write every conversation this Emacs can still resume, REMOVED included.
`aob-session-removed-hook' runs after the session has left the registry
and hands it here, which is the only moment its entry can still be
written — an agent you killed is a conversation that happened, and
killing it must not be what makes it unreachable."
  ;; An Emacs that never opened a session has nothing to say about which
  ;; sessions exist, and saying it anyway writes nil over a good file — one
  ;; failed init then costs every resumable session.  Killing your own
  ;; sessions still empties it, because that Emacs did open them.
  (when (and aob-acp-persist-file aob-acp--opened-any)
    (ignore-errors
      ;; dead/failed sessions keep their entry — a failed resume must stay
      ;; resumable, not clobber the file
      (let* ((live (delq nil (mapcar #'aob-acp--entry (aob-sessions))))
             (going (and (aob-session-p removed) (aob-acp--entry removed)))
             (fresh (append live (when (and going
                                            (not (seq-find
                                                  (lambda (e)
                                                    (equal (plist-get e :acp-id)
                                                           (plist-get going :acp-id)))
                                                  live)))
                                   (list going))))
             (ids (mapcar (lambda (e) (plist-get e :acp-id)) fresh))
             (past (seq-filter
                    (lambda (e)
                      (and (not (member (plist-get e :acp-id) ids))
                           (file-directory-p (or (plist-get e :dir) ""))))
                    (aob-acp--persisted-entries))))
        (make-directory (file-name-directory aob-acp-persist-file) t)
        (with-temp-file aob-acp-persist-file
          (prin1 (seq-take (append fresh past) aob-acp-history-limit)
                 (current-buffer)))))))

(add-hook 'kill-emacs-hook #'aob-acp--persist)
(add-hook 'aob-session-removed-hook #'aob-acp--persist)

(defun aob-acp--shutdown-all ()
  "Kill every agent this Emacs opened, whole process tree and all.
Nothing outlives the Emacs that started it: an adapter left running has
no client, keeps its CLIs and its model connection open, and cannot be
found again except through `ps'.  The conversations are not lost — the
persist hook above has already run, so every one of them comes back
through `aob-acp-resume-persisted' or `ygg-task-resume'.
Each group is asked to stop before it is made to, so an agent
mid-write still flushes its transcript."
  (ignore-errors
    (let (groups procs)
      (maphash (lambda (_key proc)
                 (push proc procs)
                 (when-let* (((process-live-p proc)) (pid (process-id proc)))
                   (push (- pid) groups)))
               aob-acp--conns)
      (dolist (g groups) (ignore-errors (signal-process g 'TERM)))
      (when groups
        (sleep-for 0.2)
        (dolist (g groups) (ignore-errors (signal-process g 'KILL))))
      (dolist (proc procs) (ignore-errors (aob-acp--conn-cleanup proc))))))

;; ahead of any last-resort sweep: an agent asked to stop flushes its
;; transcript, one killed outright does not
(add-hook 'kill-emacs-hook #'aob-acp--shutdown-all 80)

(defvar aob-acp--persisted-cache nil
  "(KEY . ENTRIES) as last read, KEY the file, its mtime and size.
A header line asks for these on every redisplay.")

(defun aob-acp--persisted-entries ()
  "The persisted conversations, each a fresh copy callers may change."
  (when-let* ((file aob-acp-persist-file)
              (attrs (file-attributes file))
              (key (list file (file-attribute-modification-time attrs)
                         (file-attribute-size attrs))))
    (unless (equal (car aob-acp--persisted-cache) key)
      (setq aob-acp--persisted-cache
            (cons key (ignore-errors
                        (with-temp-buffer
                          (insert-file-contents file)
                          (read (current-buffer)))))))
    (copy-tree (cdr aob-acp--persisted-cache))))

(defun aob-acp-persisted-entry (acp-id)
  "The persisted conversation ACP-ID names, or nil."
  (seq-find (lambda (e) (equal (plist-get e :acp-id) acp-id))
            (aob-acp--persisted-entries)))

(defun aob-acp-resumable-entries ()
  "Persisted sessions that are not live and whose directory still exists."
  (let ((live-ids (delq nil (mapcar (lambda (s) (aob-session-ref s :acp-id))
                                    (aob-sessions)))))
    (seq-filter (lambda (e)
                  (and (file-directory-p (plist-get e :dir))
                       (not (plist-get e :archived))
                       (not (member (plist-get e :acp-id) live-ids))))
                (aob-acp--persisted-entries))))

(defun aob-acp-archived-entries ()
  "Conversations put away: still resumable, no longer offered."
  (seq-filter (lambda (e) (plist-get e :archived))
              (aob-acp--persisted-entries)))

(defun aob-acp--rewrite (acp-id fn)
  "Replace the entry for ACP-ID with the result of FN on it.
FN answering nil drops it.  The file is the record of what can be
resumed, so it is rewritten whole rather than appended to."
  (when (and aob-acp-persist-file (file-readable-p aob-acp-persist-file))
    (let ((kept (delq nil
                      (mapcar (lambda (e)
                                (if (equal (plist-get e :acp-id) acp-id)
                                    (funcall fn e)
                                  e))
                              (aob-acp--persisted-entries)))))
      (with-temp-file aob-acp-persist-file
        (prin1 kept (current-buffer)))
      kept)))

(defun aob-acp--renamed (s)
  "Hold S to the name it was just given, asleep, awake and on disk."
  (let ((named (lambda (e)
                 (plist-put (plist-put (copy-sequence e) :name (aob-session-name s))
                            :named-by-user t)))
        (acp-id (aob-session-ref s :acp-id)))
    (when-let* ((asleep (aob-session-ref s :asleep)))
      (aob-session-put s :asleep (funcall named asleep)))
    (cond
     ((not (and acp-id aob-acp-persist-file)))
     ((aob-acp-persisted-entry acp-id) (aob-acp--rewrite acp-id named))
     ((when-let* ((entry (aob-acp--entry s)))
        (let ((entries (cons entry (aob-acp--persisted-entries))))
          (make-directory (file-name-directory aob-acp-persist-file) t)
          (with-temp-file aob-acp-persist-file
            (prin1 (seq-take entries aob-acp-history-limit) (current-buffer)))))))))

(defun aob-acp--pick-entry (prompt entries)
  (let* ((rows (mapcar (lambda (e)
                         (cons (format "%s · %s"
                                       (or (plist-get e :name)
                                           (plist-get e :agent) "session")
                                       (abbreviate-file-name
                                        (or (plist-get e :dir) "")))
                               e))
                       entries)))
    (unless rows (user-error "aob: nothing to choose from"))
    (cdr (assoc (completing-read prompt (mapcar #'car rows) nil t) rows))))

;;;###autoload
(defun aob-acp-archive-entry (e)
  "Put conversation E away: kept, and no longer offered to resume."
  (interactive (list (aob-acp--pick-entry "Archive: " (aob-acp-resumable-entries))))
  (aob-acp--rewrite (plist-get e :acp-id)
                    (lambda (entry) (plist-put (copy-sequence entry) :archived t)))
  (message "aob: archived %s" (or (plist-get e :name) (plist-get e :agent))))

;;;###autoload
(defun aob-acp-unarchive-entry (e)
  "Offer conversation E again."
  (interactive (list (aob-acp--pick-entry "Bring back: " (aob-acp-archived-entries))))
  (aob-acp--rewrite (plist-get e :acp-id)
                    (lambda (entry)
                      (let ((copy (copy-sequence entry)))
                        (plist-put copy :archived nil))))
  (message "aob: brought back %s" (or (plist-get e :name) (plist-get e :agent))))

;;;###autoload
(defun aob-acp-forget-entry (e)
  "Drop conversation E for good; it cannot be resumed afterwards."
  (interactive (list (aob-acp--pick-entry
                      "Forget: " (append (aob-acp-resumable-entries)
                                         (aob-acp-archived-entries)))))
  (when (y-or-n-p (format "Forget %s for good? "
                          (or (plist-get e :name) (plist-get e :agent))))
    (aob-acp--forget (plist-get e :acp-id))
    (message "aob: forgot %s" (or (plist-get e :name) (plist-get e :agent)))))

(defun aob-acp--forget (acp-id)
  "Drop the persisted entry for ACP-ID, so nothing offers to resume it."
  (when (and aob-acp-persist-file (file-readable-p aob-acp-persist-file))
    (let ((kept (seq-remove (lambda (e) (equal (plist-get e :acp-id) acp-id))
                            (aob-acp--persisted-entries))))
      (with-temp-file aob-acp-persist-file
        (prin1 kept (current-buffer))))))

(defun aob-acp-delete-session (s)
  "Kill S, forget it, and close its buffers: it will not be offered again.
`aob-kill-session' ends the process and keeps the conversation resumable;
this is the other verb, for a session that is over for good."
  (interactive (list (aob-target)))
  (when (y-or-n-p (format "Delete %s for good? " (aob-session-name s)))
    (let ((id (aob-session-id s))
          (acp-id (aob-session-ref s :acp-id))
          (name (aob-session-name s)))
      (unless (memq (aob-session-state s) '(dead failed))
        (aob--call s :kill))
      (when (aob-session-get id) (aob-remove-session s))
      (when acp-id (aob-acp--forget acp-id))
      (dolist (buf (buffer-list))
        (when (equal (buffer-local-value 'aob-buffer-session-id buf) id)
          (kill-buffer buf)))
      (when-let* ((buf (aob-session-buffer s)) ((buffer-live-p buf)))
        (kill-buffer buf))
      (message "%s deleted" name))))

(declare-function aob-transcript-file "aob-transcript" (entry))
(declare-function aob-transcript-restore "aob-transcript" (entry))
(declare-function aob-transcript-turns "aob-transcript" (file &optional tools))

(defun aob-acp--seed-history (s entry)
  "Put ENTRY\='s past turns in S, so a resumed conversation opens on itself.
The adapter reloads the conversation on its own side and says nothing
about what was in it, so a resumed session\='s trace starts blank.  Only
where S has no events of its own: an adapter that does replay must not
be doubled."
  (when (and (fboundp 'aob-transcript-file)
             (fboundp 'aob-transcript-turns)
             (null (aob-session-events s)))
    (when-let* ((file (ignore-errors (aob-transcript-file entry))))
      (dolist (turn (ignore-errors (aob-transcript-turns file t)))
        (if (equal (car turn) "tool")
            (aob-event s 'tool :title (cdr turn) :status "completed" :seeded t)
          (aob-event s (if (equal (car turn) "user") 'prompt 'message)
                     :text (cdr turn)
                     :typed (equal (car turn) "user")
                     :seeded t))))))

(defun aob-acp--holder (acp-id)
  "The session standing for conversation ACP-ID with no agent in it.
Asleep, dead or failed: the one a resume of ACP-ID brings back to life."
  (and acp-id
       (seq-find (lambda (s)
                   (and (equal (aob-session-ref s :acp-id) acp-id)
                        (not (aob-session-ref s :hidden))
                        (or (aob-session-ref s :asleep)
                            (memq (aob-session-state s) '(dead failed)))))
                 (aob-sessions))))

(defun aob-acp-resume-entry (e &optional pref)
  "Respawn persisted entry E, restoring its conversation; return the session.
PREF overrides `aob-acp-restore-preference' for this one entry.
Prompts sent while it opens queue and fire on readiness.  A session
already standing for E, asleep, dead or failed, is succeeded rather
than doubled: the one that comes back takes its trace, in the windows
it was in, and its row in every list.  One that failed on an agent still
running is let go of first, as a restart lets go of it, so the agent
holds no turn, question or connection for a session no one reads."
  (when (fboundp 'aob-transcript-restore)
    (condition-case err
        (aob-transcript-restore e)
      (error (user-error "Cannot restore %s: %s"
                         (or (ignore-errors (aob-transcript-file e)) (plist-get e :acp-id))
                         (error-message-string err)))))
  (if-let* ((old (aob-acp--holder (plist-get e :acp-id))))
      (progn
        (when (process-live-p (aob-session-conn old))
          (aob-session-put old :restarting t)
          (unwind-protect (aob--call old :kill)
            (aob-session-put old :restarting nil)))
        (aob-succeed old (lambda () (aob-acp--resume-entry e pref))))
    (aob-acp--resume-entry e pref)))

(defun aob-acp--resume-entry (e pref)
  "Respawn E under PREF, as `aob-acp-resume-entry' does, in a new session."
  (let* ((dir (plist-get e :dir))
         (acp-id (plist-get e :acp-id))
         (cwd (directory-file-name (expand-file-name dir)))
         ;; the name it had, unless that id is taken — and then numbered
         ;; off its task, because a session resumed onto `claude:4' has
         ;; lost the one thing its name was telling every list
         (name (if (aob-session-get (concat "acp:" (plist-get e :name)))
                   (aob-acp--gen-name
                    (if-let* ((task (plist-get e :task)))
                        (file-name-nondirectory task)
                      (plist-get e :agent)))
                 (plist-get e :name)))
         (aob-acp-session-refs
          (append (list :acp-id acp-id)
                  (when-let* ((task (plist-get e :task))) (list :task task))
                  (when-let* ((dirs (plist-get e :extra-dirs)))
                    (list :extra-dirs (seq-filter #'file-directory-p dirs)))
                  (when-let* ((step (plist-get e :task-step)))
                    (list :task-step step))
                  (when-let* ((todo (plist-get e :todo-file)))
                    (list :todo-file todo))
                  (when (plist-get e :named-by-user)
                    (list :named-by-user t))
                  (when-let* ((auto (plist-get e :auto-named)))
                    (list :auto-named auto))
                  (plist-get e :limits)
                  ;; a conversation reloaded onto another model is a
                  ;; different conversation from the second turn on
                  (when-let* ((model (plist-get e :model)))
                    (list :want-model model))
                  ;; the mode it was last on, else the one its agent is
                  ;; pinned to: a reload that lands on the adapter's idea
                  ;; of a default asks about edits the session stopped
                  ;; asking about an hour ago
                  (when-let* ((mode (or (plist-get e :mode)
                                        (plist-get (cdr (assoc (plist-get e :agent)
                                                               aob-acp-agents))
                                                   :mode))))
                    (list :want-mode mode))
                  ;; only a restart carries this: a space id from a previous
                  ;; Emacs means nothing, and is never persisted
                  (when-let* ((space (plist-get e :space)))
                    (list :space space))))
         (verb (list nil))
         ;; a verb asked for by name is the verb sent
         (seeded (and (not pref)
                      (fboundp 'aob-transcript-file)
                      (ignore-errors (aob-transcript-file e))
                      t))
         (s nil))
    ;; the conversation goes in before the adapter answers: opening a
    ;; session takes seconds, and a trace that is empty for those seconds
    ;; is a conversation that looks lost
    (setq s (aob-acp--open
             (plist-get e :agent) name (plist-get e :project) dir
             (lambda (init)
               (prog1 (aob-acp--restore-open init acp-id cwd name verb pref seeded)
                 ;; the replay brings back what was seeded; shown once
                 (when (and (aob-session-p s) (eq (car verb) 'load))
                   (let ((kept (seq-remove (lambda (ev) (plist-get ev :seeded))
                                           (aob-session-events s))))
                     (setf (aob-session-events s) kept
                           (aob-session-nevents s) (length kept))))))
             (lambda (s res)
               (aob-session-put s :restored-by (car verb))
               (aob-session-settle-subagents s t)
               ;; a second time only where the first found nothing
               (unless (eq (car verb) 'load)
                 (aob-acp--seed-history s e))
               (aob-acp--session-opened s res acp-id "session resumed"))))
    ;; `aob-acp--open' is what makes the session; a caller that stands
    ;; in for it hands back whatever it likes
    (when (and (aob-session-p s) (not (eq (car verb) 'load)))
      (aob-acp--seed-history s e))
    s))

(defun aob-acp--restore-target ()
  "The stored conversation to bring back: the one at point, else one picked."
  (or (when-let* ((s (aob-session-at-point))) (aob-acp--entry s))
      (let ((map (mapcar (lambda (e) (cons (plist-get e :name) e))
                         (aob-acp-resumable-entries))))
        (unless map (user-error "aob: nothing to bring back"))
        (cdr (assoc (completing-read "Session: " map nil t) map)))))

;;;###autoload
(defun aob-acp-resume-session (e)
  "Bring conversation E back without replaying it.
The agent restores its own context and sends nothing, so the buffer
holds only what it held before or the transcript on disk has — this is
the verb for reconnecting, and the one every restart takes by default."
  (interactive (list (aob-acp--restore-target)))
  (aob-acp-resume-entry e 'resume))

;;;###autoload
(defun aob-acp-reload-session (e)
  "Bring conversation E back and replay its whole history into the buffer.
The verb for reading a transcript again; reconnecting costs less."
  (interactive (list (aob-acp--restore-target)))
  (aob-acp-resume-entry e 'load))

;;;###autoload
(defun aob-acp-restart (s &optional pref)
  "Restart the CLI behind S, reloading the same conversation into it.
The process is what breaks — the session is not.  Its ACP id, the task
it is on, the directories it may see, the model and mode it was on and
the space it belongs to all outlive the CLI, so this is that conversation
loaded into a fresh process under the same name, not a new session.

Dead or wedged either way: the old process is killed first, and the
session that comes back takes the old one's place, its name and its
trace."
  (interactive (list (aob-target)))
  (let ((entry (append (aob-acp--entry s)
                       ;; only a restart carries the space: it is this
                       ;; Emacs's own numbering, and never persisted
                       (list :space (aob-session-ref s :space)))))
    (unless (plist-get entry :acp-id)
      (user-error "aob: %s never got a session to reload" (aob-session-name s)))
    (unless (file-directory-p (or (plist-get entry :dir) ""))
      (user-error "aob: %s ran in %s, which is gone" (aob-session-name s)
                  (plist-get entry :dir)))
    ;; the tree it ran in is where it comes back; reaping it here pulls that out from under it
    (aob-session-put s :restarting t)
    (unwind-protect
        (progn
          (aob--call s :kill)
          (message "aob: restarting %s" (plist-get entry :name))
          (aob-acp-resume-entry entry pref))
      (aob-session-put s :restarting nil))))

(defun aob-acp--init-of (s)
  "The initialize result of S's connection, or nil before it has one."
  (when-let* ((proc (aob-session-conn s)))
    (pcase (process-get proc 'aob-init)
      (`(done ,res) res))))

(defconst aob-acp--other-folder "other folder…")

(defun aob-acp--read-folder (s)
  "Ask for a folder S should see: its repository's other worktrees, else any."
  (let* ((seen (mapcar (lambda (d) (file-truename (file-name-as-directory d)))
                       (cons (aob-session-dir s) (aob-session-ref s :extra-dirs))))
         (others (seq-remove (lambda (w) (member (file-truename (car w)) seen))
                             (aob-acp--worktrees (or (aob-session-dir s)
                                                     (aob-session-project s)))))
         (rows (mapcar (lambda (w)
                         (cons (format "%s  %s" (abbreviate-file-name (car w))
                                       (or (cdr w) "detached"))
                               (car w)))
                       others))
         (pick (and rows (completing-read
                          "Add folder: "
                          (append (mapcar #'car rows) (list aob-acp--other-folder))
                          nil t nil nil (caar rows)))))
    (or (cdr (assoc pick rows))
        (read-directory-name "Add folder: " nil nil t))))

(defun aob-acp--can-add-folder (s)
  "Refuse, changing nothing, unless S can be reopened with more folders."
  (let ((init (aob-acp--init-of s))
        (name (aob-session-name s)))
    (cond ((not init)
           (user-error "aob: %s is not connected; wake it first" name))
          ((not (aob-acp--session-cap init :additionalDirectories))
           (user-error "aob: %s's agent takes no additional directories; nothing changed"
                       name))
          ((not (or (aob-acp--resumes-p init)
                    (plist-get (plist-get init :agentCapabilities) :loadSession)))
           (user-error "aob: %s's agent cannot reopen a session; nothing changed" name))
          ((memq (aob-session-state s) '(working blocked))
           (user-error "aob: %s is mid-turn; add the folder once it is idle" name)))))

;;;###autoload
(defun aob-acp-add-folder (s dir)
  "Let S see DIR too, resuming its conversation with DIR among its folders.
ACP takes additional directories only when a session opens, so S is
resumed with them; an agent that does not take them is left as it was."
  (interactive (let ((s (aob-target)))
                 (aob-acp--can-add-folder s)
                 (list s (aob-acp--read-folder s))))
  (aob-acp--can-add-folder s)
  (let ((name (aob-session-name s))
        (dir (file-name-as-directory (expand-file-name dir)))
        (dirs (aob-session-ref s :extra-dirs)))
    (cond ((not (file-directory-p dir))
           (user-error "aob: %s is not a folder" dir))
          ((member (file-truename dir)
                   (mapcar (lambda (d) (file-truename (file-name-as-directory d)))
                           (cons (aob-session-dir s) dirs)))
           (user-error "aob: %s already sees %s" name (abbreviate-file-name dir))))
    (aob-session-put s :extra-dirs (append dirs (list dir)))
    (condition-case err
        (aob-acp-restart s 'resume)
      (error (aob-session-put s :extra-dirs dirs)
             (signal (car err) (cdr err))))
    (message "aob: %s now sees %s" name (abbreviate-file-name dir))))

;;;###autoload
(defun aob-acp-resume-persisted ()
  "Respawn every persisted ACP session, reloading each conversation."
  (interactive)
  (let ((entries (aob-acp-resumable-entries)))
    (if (null entries)
        (message "aob: nothing to resume")
      (dolist (e entries) (aob-acp-resume-entry e))
      (message "aob: resuming %d session(s)" (length entries)))))

(defvar aob-acp--list-map nil)

(defun aob-acp--list-label (x)
  (format "%s  %s · %s"
          (let ((ts (or (plist-get x :updatedAt) "")))
            (if (>= (length ts) 16)
                (concat (substring ts 0 10) " " (substring ts 11 16))
              ts))
          (truncate-string-to-width
           (or (plist-get x :title) "(untitled)") 64)
          (substring (plist-get x :sessionId) 0 8)))

(defconst aob-acp--list-page-cap 20
  "Most session/list pages read in one go; an agent that pages forever stops here.")

(defun aob-acp--list-sessions (proc init agent cwd then)
  "Call THEN with (RES ERR), RES holding every session AGENT lists under CWD.
Pages are followed until nextCursor is gone or the page cap is reached.
An agent INIT does not show listing sessions is never asked."
  (if (not (aob-acp--session-cap init :list))
      (message "aob: %s keeps no session list" agent)
    (let ((acc nil) (pages 0) page)
      (setq page
            (lambda (cursor)
              (aob-acp--request-proc
               proc "session/list"
               (append (list :cwd cwd) (when cursor (list :cursor cursor)))
               (lambda (res err)
                 (if (and err (null acc))
                     (funcall then nil err)
                   (setq acc (append acc (and (not err) (plist-get res :sessions))))
                   (setq pages (1+ pages))
                   (let ((next (and (not err) (plist-get res :nextCursor))))
                     (if (and (stringp next) (< pages aob-acp--list-page-cap))
                         (funcall page next)
                       (funcall then (list :sessions acc) nil))))))))
      (funcall page nil))))

(defun aob-acp--conn-for (agent project)
  "A running connection for AGENT on PROJECT, none being started to ask.
The one keyed the default way comes first; failing it, any connection
on that tree, whose config home may keep another store, where a delete
finds nothing and a list shows that home's conversations."
  (let ((dir (file-name-as-directory (expand-file-name project)))
        (found (ignore-errors
                 (let ((aob-acp--session-env nil) (aob-acp-isolate nil))
                   (aob-acp--live-conn agent project)))))
    (maphash (lambda (key proc)
               (when (and (not found) (process-live-p proc)
                          (equal (car key) agent)
                          (stringp (cadr key))
                          (equal (file-name-as-directory (expand-file-name (cadr key)))
                                 dir))
                 (setq found proc)))
             aob-acp--conns)
    found))

(defun aob-acp--conn-init (proc)
  "PROC's initialize result, or nil before it settled."
  (pcase (process-get proc 'aob-init)
    (`(done ,res) res)))

(defun aob-acp-list-sessions (agent project then)
  "Call THEN with the sessions AGENT itself lists under PROJECT, or nil.
Only an agent already running on PROJECT and advertising session/list
is asked; THEN is called with nil otherwise, so a caller never waits on
an agent that will not answer."
  (let* ((proc (aob-acp--conn-for agent project))
         (init (and proc (aob-acp--conn-init proc))))
    (if (not (aob-acp--session-cap init :list))
        (funcall then nil)
      (aob-acp--list-sessions
       proc init agent (aob-acp--wire-dir project)
       (lambda (res err) (funcall then (and (not err) (plist-get res :sessions))))))))

(defun aob-acp-merge-listed (entries listed agent project)
  "ENTRIES, the conversations found on disk, with LISTED merged in by id.
LISTED is what AGENT answered session/list with under PROJECT.  An entry
it names gains the agent's title and last activity; one it names that
the disk did not show is added after them.  The disk stays the record:
nothing found there is dropped for being missing from the list."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (x listed)
      (puthash (plist-get x :sessionId) x by-id))
    (append
     (mapcar (lambda (e)
               (if-let* ((x (gethash (plist-get e :acp-id) by-id)))
                   (progn (remhash (plist-get e :acp-id) by-id)
                          (append e (list :listed t
                                          :listed-title (plist-get x :title)
                                          :updated-at (plist-get x :updatedAt))))
                 e))
             entries)
     (delq nil
           (mapcar (lambda (x)
                     (when (gethash (plist-get x :sessionId) by-id)
                       (list :agent agent :acp-id (plist-get x :sessionId)
                             :project project
                             :dir (or (plist-get x :cwd) project)
                             :name (or (plist-get x :title) (plist-get x :sessionId))
                             :listed t
                             :listed-title (plist-get x :title)
                             :updated-at (plist-get x :updatedAt))))
                   listed)))))

(defun aob-acp-delete-entry (entry &optional then)
  "Ask ENTRY\='s agent to delete its conversation; non-nil when asked.
Only an agent already running on ENTRY\='s tree and advertising
session/delete is asked; THEN, when given, is called with the answer\='s
result and error once it comes.  Nothing is called when nothing was
asked.  A conversation still awake here is left alone; deleting it
would pull it out from under its session."
  (when-let* ((id (plist-get entry :acp-id))
              ((not (seq-find (lambda (s)
                                (and (equal (aob-session-ref s :acp-id) id)
                                     (aob-session-conn s)
                                     (not (memq (aob-session-state s) '(dead failed)))))
                              (aob-sessions))))
              (proc (aob-acp--conn-for (plist-get entry :agent)
                                       (or (plist-get entry :project)
                                           (plist-get entry :dir))))
              ((aob-acp--session-cap (aob-acp--conn-init proc) :delete)))
    (aob-acp--request-proc proc "session/delete" (list :sessionId id)
                           (or then #'ignore))
    t))

;;;###autoload
(defun aob-acp-resume-from-list (agent)
  "Resume one of AGENT's own stored sessions for this project.
Reaches the agent's full history via session/list — sessions started
from the terminal included, not just ones this client persisted."
  (interactive (list (completing-read "Agent: " (aob-acp-names)
                                      nil t nil nil aob-acp-default-agent)))
  (let* ((project (aob-acp--project))
         (cwd (directory-file-name (expand-file-name project)))
         (proc (or (aob-acp--live-conn agent project)
                   (aob-acp--start-conn agent project))))
    (aob-acp--with-init
     proc
     (lambda (init err)
       (if err
           (message "aob: %s failed to start: %s" agent (plist-get err :message))
         (aob-acp--list-sessions
          proc init agent (aob-acp--wire-dir cwd)
          (lambda (res err2)
            (let ((sessions (and (not err2) (plist-get res :sessions))))
              (if (null sessions)
                  (message "aob: %s has no stored sessions in %s"
                           agent (abbreviate-file-name project))
                (let* ((aob-acp--list-map
                        (mapcar (lambda (x) (cons (aob-acp--list-label x) x))
                                sessions))
                       (table (lambda (str pred action)
                                (if (eq action 'metadata)
                                    '(metadata (display-sort-function . identity))
                                  (complete-with-action
                                   action (mapcar #'car aob-acp--list-map)
                                   str pred))))
                       (pick (completing-read "Resume session: " table nil t))
                       (sid (plist-get (cdr (assoc pick aob-acp--list-map))
                                       :sessionId))
                       (s (aob-acp-resume-entry
                           (or (when-let* ((held (aob-acp--holder sid)))
                                 (aob-acp--entry held))
                               (list :agent agent :name (aob-acp--gen-name agent)
                                     :project project :dir project
                                     :acp-id sid)))))
                  (when (and aob-acp-show-trace (not noninteractive)
                             (fboundp 'aob-trace-buffer))
                    (ygg-ui-show (aob-trace-buffer s) t))))))))))))

(provide 'aob-acp)
;;; aob-acp.el ends here
