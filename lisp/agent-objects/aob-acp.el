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
(require 'json)
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

(defun aob-acp--request-proc (proc method params cb)
  (let ((id (cl-incf (car (process-get proc 'aob-next-id)))))
    (puthash id cb (process-get proc 'aob-pending))
    (aob-acp--send-proc proc (list :jsonrpc "2.0" :id id
                                   :method method :params params))))

(defun aob-acp--respond-proc (proc id result &optional error)
  (aob-acp--send-proc proc (if error
                               (list :jsonrpc "2.0" :id id :error error)
                             (list :jsonrpc "2.0" :id id :result result))))

;;; Wire — session level

(defun aob-acp--request (s method params cb)
  (aob-acp--request-proc (aob-session-conn s) method params cb))

(defun aob-acp--notify (s method params)
  (aob-acp--send-proc (aob-session-conn s)
                      (list :jsonrpc "2.0" :method method :params params)))

(defun aob-acp--respond (s id result &optional error)
  (aob-acp--respond-proc (aob-session-conn s) id result error))

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

(defun aob-acp--conn-env (agent project)
  "The environment a connection for AGENT on PROJECT would be started with."
  (and aob-acp-environment-function
       (ignore-errors
         (funcall aob-acp-environment-function
                  agent project project aob-acp-isolate))))

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
         ;; a claude spawned with CLAUDECODE set refuses to start (nested
         ;; guard); the append keeps envrc/mise buffer-local env visible
         (process-environment
          (append (and aob-acp-environment-function
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
    (process-put proc 'aob-sessions (make-hash-table :test #'equal))
    (process-put proc 'aob-next-id (list 0))
    (process-put proc 'aob-pending (make-hash-table :test #'eql))
    (process-put proc 'aob-json-buf (generate-new-buffer (format " *aob-json:%s*" base)))
    (process-put proc 'aob-stderr-buf stderr)
    (process-put proc 'aob-init 'pending)
    (puthash (aob-acp--conn-key agent project) proc aob-acp--conns)
    (aob-acp--request-proc
     proc "initialize"
     (list :protocolVersion 1
           ;; declaring form elicitation re-enables claude's
           ;; AskUserQuestion tool (the adapter disallows it otherwise)
           :clientCapabilities (list :fs (list :readTextFile :false
                                               :writeTextFile :false)
                                     :elicitation (list :form t)
                                     ;; without this the adapter treats us as
                                     ;; a client that cannot nest, and strips
                                     ;; every subagent's words before sending
                                     :_meta (list :subagent-transcript t))
           :clientInfo (list :name "aob.el" :version "0.1"))
     (lambda (res err)
       (process-put proc 'aob-init (if err (list 'failed err) (list 'done res)))
       (dolist (w (process-get proc 'aob-init-waiters))
         (funcall w res err))
       (process-put proc 'aob-init-waiters nil)))
    proc))

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
        (while (progn (goto-char (point-min))
                      (search-forward "\n" nil t))
          (let ((end (point))
                (msg (progn
                       (goto-char (point-min))
                       (ignore-errors
                         (json-parse-buffer :object-type 'plist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil)))))
            (delete-region (point-min) end)
            (when msg (aob-acp--dispatch proc msg))))))))

(defun aob-acp--route (proc sid)
  "The session a frame belongs to: by SID, else the sole bound session."
  (or (and sid (gethash sid (aob-acp--proc-sessions proc)))
      (let ((bound (aob-acp--conn-sessions proc)))
        (and bound (null (cdr bound)) (car bound)))))

(defun aob-acp--dispatch (proc msg)
  (let* ((method (plist-get msg :method))
         (id (plist-get msg :id))
         (params (plist-get msg :params))
         (s (and method (aob-acp--route proc (plist-get params :sessionId)))))
    (cond
     ((and method id)
      (if s
          (aob-acp--on-request s id method params)
        (aob-acp--respond-proc proc id nil
                               (list :code -32603
                                     :message "aob: unknown session"))))
     (method (when s (aob-acp--on-notification s method params)))
     (id (when-let* ((cb (gethash id (process-get proc 'aob-pending))))
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
                           (point-max))))))))
      (dolist (s (aob-acp--conn-sessions proc))
        (when (aob-session-get (aob-session-id s))
          (aob-session-put s :fail-reason
                           (or (aob-acp--fail-reason tail) "process exited"))
          (aob-set-state s 'dead)
          (aob-event s 'error :title "process exited" :text tail)
          (message "aob: %s died: %s" (aob-session-name s)
                   (aob-session-ref s :fail-reason)))))
    (aob-acp--conn-cleanup proc)))

;;; Incoming requests — permission becomes a Decision object; the JSON-RPC
;;; reply is held until a human resolves it

(defvar aob-acp-request-functions nil
  "Abnormal hook run with (SESSION DECISION TOOLCALL) for every permission
request, after its decision is pushed and its event recorded.")

(defun aob-acp--on-request (s id method params)
  (pcase method
    ("session/request_permission"
     (let* ((tc (plist-get params :toolCall))
            (raw (plist-get tc :rawInput))
            (d (list :reply-id id
                     :title (or (plist-get tc :title) "permission")
                     :detail (when-let* ((str (cond ((and (listp raw)
                                                          (plist-get raw :command)))
                                                    (raw (format "%S" raw)))))
                               (truncate-string-to-width (format "%s" str) 72))
                     :options (plist-get params :options))))
       (push d (aob-session-decisions s))
       (aob-set-state s 'blocked)
       (aob-event s 'permission :title (plist-get d :title))
       (dolist (fn aob-acp-request-functions)
         (condition-case err (funcall fn s d tc)
           (error (message "aob-acp-request-functions: %S" err))))))
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
       (aob-event s 'permission :title (plist-get d :title))
       ;; the same subscribers a permission wakes: a question nobody is
       ;; told about is a session that stops and does not say why
       (dolist (fn aob-acp-request-functions)
         (condition-case err (funcall fn s d nil)
           (error (message "aob-acp-request-functions: %S" err))))))
    (_ (aob-acp--respond s id nil
                         (list :code -32601
                               :message (format "aob: %s not supported" method))))))

(defun aob-acp--elicit-field (key field message)
  "One question from schema FIELD called KEY, falling back to MESSAGE.
Options are read as oneOf or anyOf constants, and as a plain enum:
claude writes the first, and a tool codex is carrying writes whichever
its own schema used."
  (let* ((multi (equal (plist-get field :type) "array"))
         (spec (if multi (or (plist-get field :items) field) field))
         (constants (or (plist-get spec :oneOf) (plist-get spec :anyOf)))
         (enum (plist-get spec :enum)))
    (list :key key
          :text (or (plist-get field :description)
                    (plist-get field :title)
                    message)
          :multi multi
          :options (cond (constants (delq nil (mapcar (lambda (o)
                                                        (plist-get o :const))
                                                      constants)))
                         (enum (append enum nil))))))

(defun aob-acp--elicit-questions (params)
  "PARAMS' form schema as a list of question plists.
Each is (:key FIELD :text QUESTION :multi BOOL :options LABELS).

Two shapes, because two agents ask.  Claude's AskUserQuestion numbers
its fields question_0 upward and pairs each with a question_<n>_custom
for a typed answer that is not one of the options, so a numbered schema
is read that way and the custom fields are left out of the asking.
Codex hands the tool's own schema through untouched, so anything else
is read field by field, in the order the schema wrote them."
  (let* ((props (plist-get (plist-get params :requestedSchema) :properties))
         (message (plist-get params :message))
         (numbered
          (let ((i 0) acc)
            (catch 'done
              (while t
                (let ((f (plist-get props (intern (format ":question_%d" i)))))
                  (unless f (throw 'done nil))
                  (push (aob-acp--elicit-field (format "question_%d" i) f message)
                        acc)
                  (cl-incf i))))
            (nreverse acc))))
    (or numbered
        (let ((rest props) acc)
          (while rest
            (let* ((key (substring (symbol-name (pop rest)) 1))
                   (field (pop rest)))
              (unless (string-suffix-p "_custom" key)
                (push (aob-acp--elicit-field key field message) acc))))
          (nreverse acc)))))

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
     (let ((u (plist-get params :update)))
       (pcase (plist-get u :sessionUpdate)
         ("tool_call" (aob-acp--tool-call s u))
         ("tool_call_update" (aob-acp--tool-update s u))
         ("agent_message_chunk" (aob-acp--chunk s :msg-ev 'message u))
         ("agent_thought_chunk" (aob-acp--chunk s :thought-ev 'thought u))
         ("available_commands_update"
          (aob-session-put s :commands (plist-get u :availableCommands)))
         ("current_mode_update"
          (aob-session-put s :mode-id (plist-get u :currentModeId))
          (aob-event s 'state
                     :title (format "mode: %s" (plist-get u :currentModeId))))
         ("usage_update"
          (aob-session-put s :ctx-used (plist-get u :used))
          (aob-session-put s :ctx-size (plist-get u :size))
          (aob-session-put s :usage-latest u)
          (when (fboundp 'ygg-usage-note) (ygg-usage-note s u))
          (aob-acp--autocompact-check s)
          (aob--dirty s))
         ("session_info_update"
          ;; the goal rides this update with no title of its own — writing
          ;; the absent title through would erase the session's
          (when-let* ((title (plist-get u :title)))
            (aob-session-put s :info-title title))
          (when-let* ((meta (plist-get u :_meta)))
            (aob-session-put s :goal (plist-get meta :goal))
            (aob--dirty s)))
         ("config_option_update"
          (aob-acp--config-apply s (plist-get u :configOptions)))
         ("current_model_update"
          (aob-session-put s :models
                           (plist-put (aob-session-ref s :models)
                                      :currentModelId
                                      (plist-get u :currentModelId)))
          (aob-acp--models-refresh s))
         ;; claude sends "plan"; codex/hermes stream "plan_update" (and
         ;; "plan_removed" to clear) — all carry the same :entries shape
         ((or "plan" "plan_update") (aob-acp--plan s u))
         ("plan_removed" (aob-acp--plan s '(:entries nil))))))))

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

(define-key aob-object-map "x" #'aob-acp-command)
;; M reaches the both-wires picker wherever a session is rendered; pointing it
;; at a per-backend one once left claude sessions "advertising no models"
(define-key aob-object-map "M" #'aob-acp-model)

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
  "S's model picker as (CURRENT-ID . VALUES) from whichever source the
agent speaks: claude's configOptions, or the spec's models field
\(codex).  VALUES are plists with :value/:name/:description."
  (if-let* ((opt (seq-find (lambda (o) (equal (plist-get o :id) "model"))
                           (aob-session-ref s :config-options))))
      (cons (plist-get opt :currentValue) (aob-acp--config-values opt))
    (when-let* ((models (aob-session-ref s :models)))
      (cons (plist-get models :currentModelId)
            (mapcar (lambda (m) (list :value (plist-get m :modelId)
                                      :name (plist-get m :name)
                                      :description (plist-get m :description)))
                    (plist-get models :availableModels))))))

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
      (when (and old cur (not (equal old cur)))
        (aob-event s 'state :title (format "model: %s" name)))
      (aob--dirty s))))

(defun aob-acp--config-apply (s opts)
  (aob-session-put s :config-options opts)
  (aob-acp--models-refresh s))

(defun aob-acp--set-config (s config-id value)
  (aob-acp--request
   s "session/set_config_option"
   (list :sessionId (aob-acp--acp-id s) :configId config-id :value value)
   (lambda (res err)
     (if err
         (message "aob: %s" (plist-get err :message))
       (aob-acp--config-apply s (plist-get res :configOptions))
       (message "aob: %s → %s" config-id value)))))

(defun aob-acp--set-model (s model-id)
  "Switch S to MODEL-ID over whichever wire the agent speaks."
  (if (seq-find (lambda (o) (equal (plist-get o :id) "model"))
                (aob-session-ref s :config-options))
      (aob-acp--set-config s "model" model-id)
    (aob-acp--request
     s "session/set_model"
     (list :sessionId (aob-acp--acp-id s) :modelId model-id)
     (lambda (_res err)
       (if err
           (message "aob: %s" (plist-get err :message))
         (aob-session-put s :models
                          (plist-put (aob-session-ref s :models)
                                     :currentModelId model-id))
         (aob-acp--models-refresh s)
         (message "aob: model → %s" model-id))))))

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
  ;; mid-handshake (resume takes seconds) nothing is ingested yet — the
  ;; instinctive immediate M must wait for readiness, not lie about
  ;; advertisement
  (pcase (aob-session-state s)
    ((or 'failed 'dead)
     (user-error "aob: %s %s (%s) — SPC a c R respawns it"
                 (aob-session-name s) (aob-session-state s)
                 (or (aob-session-ref s :fail-reason) "no reason recorded")))
    ('starting
     (message "aob: %s is still opening — the model picker will follow"
              (aob-session-name s))
     (letrec ((fn (lambda (s2 _old new)
                    (when (eq s2 s)
                      (cond
                       ((memq new '(idle working))
                        (remove-hook 'aob-state-change-hook fn)
                        (run-at-time 0 nil #'aob-acp-model s))
                       ((memq new '(failed dead))
                        (remove-hook 'aob-state-change-hook fn)))))))
       (add-hook 'aob-state-change-hook fn)))
    (_ (aob-acp--model-1 s))))

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

(defun aob-acp--chunk (s slot type u)
  "Fold U's text into S's running message or thought.
A subagent speaks on the same stream as the agent that sent it, so each
one accumulates under its own Task — otherwise five voices would land
in one paragraph, and the trace would show them as the main agent's."
  (let* ((content (plist-get u :content))
         ;; an image block carries no text; it is still something the agent
         ;; said, and a trace that dropped it would read as a gap
         (text (or (plist-get content :text)
                   (and (equal (plist-get content :type) "image") "[[Image]]")))
         (parent (aob-acp--parent-of u))
         (slot (if parent (intern (format "%s@%s" slot parent)) slot))
         (ev (aob-session-ref s slot)))
    (if ev
        (progn (aob-event-push-text ev text)
               (aob-refresh-summary s ev))
      (let ((ev (aob-event s type :parent parent)))
        (aob-event-push-text ev text)
        (aob-refresh-summary s ev)
        (aob-session-put s slot ev)
        (when parent
          (aob-session-put s :sub-accums
                           (cons slot (aob-session-ref s :sub-accums))))))))

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
         (task (equal (plist-get u :title) "Task"))
         (ev (aob-event s 'tool
                        :tool-id id
                        :kind (plist-get u :kind)
                        ;; a subagent deserves to be named by what it was
                        ;; sent to do (claude) or by which agent it is
                        ;; (codex) — never by the bare tool
                        :title (or (and task (or (plist-get raw :description)
                                                 (plist-get raw :prompt)))
                                   (cdr codex)
                                   (aob-acp--tool-title u raw))
                        :raw raw
                        :subagent (or task (and (cdr codex) t))
                        :parent (or (aob-acp--parent-of u)
                                    (plist-get meta :parentToolUseId)
                                    (car codex))
                        :status (plist-get u :status)
                        :locations (plist-get u :locations)
                        :content (plist-get u :content)
                        :stat (aob-acp--diff-stat (plist-get u :content)))))
    (puthash (plist-get u :toolCallId) ev (aob-acp--tools s))
    (aob-acp--child-note s ev nil)))

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
                                 (equal val "Task")))))
            (plist-put ev key
                       (if (eq key :title)
                           (aob-acp--tool-title u (plist-get ev :raw))
                         val))))
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
      (setf (aob-session-events s) (append (nreverse moved) events)))
    (run-hook-with-args 'aob-queue-change-hook s)
    (aob-acp--prompt-1 s (mapconcat #'car q "\n\n")
                       (apply #'append (mapcar #'cadr q))
                       'queued)))

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

(defcustom aob-acp-embed-limit 400000
  "Largest mention carried as its own text rather than as a link.
Past this the file goes as a `resource_link' again.  Sized for a
million-token window: a whole source file fits, and a mention still
must not spend the turn on one file."
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

(defun aob-acp--mention-block (mention embed)
  "MENTION as a content block: its own text when EMBED, else a link to it."
  (let ((abs (cdr mention)))
    (or (and embed
             (when-let* ((text (aob-acp--embeddable-text abs)))
               (list :type "resource"
                     :resource (list :uri (concat "file://" abs)
                                     :mimeType (aob-acp--text-mime abs)
                                     :text text))))
        (list :type "resource_link"
              :uri (concat "file://" abs)
              :name (car mention)))))

(defun aob-acp--content-blocks (text atts &optional dir embed)
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
          (mapcar (lambda (m) (aob-acp--mention-block m embed))
                  (and dir (aob-acp--file-mentions text dir))))))

(defun aob-acp--prompt (s text &optional atts)
  ;; prompting is never destructive: mid-turn (or mid-handshake, e.g. a
  ;; just-resumed session) it queues; interject is the explicit steer
  (if (memq (aob-session-state s) '(working starting))
      (progn
        (aob-acp--queue s text atts)
        (message "aob: queued for %s" (aob-session-name s)))
    (aob-acp--prompt-1 s text atts)))

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

(defun aob-acp--prompt-1 (s text &optional atts queued)
  (aob-acp--break-accum s)
  ;; an agent that never declared image support gets paths, not blocks
  ;; it can't parse — the reference survives, and the demotion is said
  (when (and atts
             (not (plist-get (plist-get (aob-session-ref s :agent-caps)
                                        :promptCapabilities)
                             :image)))
    (setq text (concat text "\n"
                       (mapconcat (lambda (f) (format "[image: %s]" f))
                                  atts "\n"))
          atts nil)
    (message "aob: %s takes no images — attached as paths"
             (aob-session-name s)))
  (aob-session-put s :turn-fails nil)
  (aob-session-put s :turn-error nil)
  (unless queued
    ;; the tokens you wrote come back as tokens: what you sent is what the
    ;; trace shows, counted in the same [[Image]] the compose buffer used
    (aob-event s 'prompt :text text :images (length atts) :image-files atts
               :typed aob-prompt-typed))
  (aob-set-state s 'working)
  (aob-acp--request
   s "session/prompt"
   (append
    (list :sessionId (aob-acp--acp-id s)
          :prompt (aob-acp--content-blocks
                   text atts (or (aob-session-dir s) (aob-session-project s))
                   (aob-acp-embeds-p s)))
    (when-let* ((meta (aob-acp--prompt-meta s))) (list :_meta meta)))
   (lambda (res err)
     (aob-acp--break-accum s)
     (if err
         ;; the flag first: the idle transition runs hooks (workflow
         ;; advance) that must see this turn failed
         (progn (aob-session-put s :turn-error t)
                (aob-set-state s 'idle)
                (aob-event s 'error :title (plist-get err :message)))
       ;; a /compact turn reports totalTokens 0 — never clobber the last
       ;; real reading with it
       (let ((usage (plist-get res :usage)))
         (when (and usage (> (or (plist-get usage :totalTokens) 0) 0))
           (aob-session-put s :usage usage)
           (aob-session-put s :usage-latest usage)
           (when (fboundp 'ygg-usage-note) (ygg-usage-note s usage)))
         (aob-event s 'stop :reason (plist-get res :stopReason)
                    :tokens (let ((tk (plist-get usage :totalTokens)))
                              (and tk (> tk 0) tk))))
       ;; /clear wiped the agent's context — the trace history before it
       ;; now belongs to a conversation that no longer exists, so drop it
       ;; to a single marker (before the queue flushes new work in)
       (when (aob-acp--clear-p text)
         (setf (aob-session-events s) nil
               (aob-session-nevents s) 0)
         (aob-event s 'state :title "context cleared"))
       (aob-set-state s 'idle)
       (aob-acp--flush-queue s)))))

;;; Autosummarize — a big session sends itself /compact once its context
;;; window fills, so it never wedges at the limit (and a compacted session
;;; replays cheaper on resume).  Fires only between turns, once per fill.

(defcustom aob-acp-autocompact-ratio 0.85
  "Auto-send /compact once a session's context passes this fraction of its
window (used/size).  nil disables autosummarize.  Fires only when the
session is settled (idle, empty queue) and re-arms after usage falls well
below the trigger, so a session compacts at most once per fill."
  :type '(choice (const :tag "disabled" nil) number) :group 'aob)

(defconst aob-acp--autocompact-rearm 0.10
  "Re-arm autosummarize once usage falls this far below the trigger ratio.")

(defun aob-acp--ctx-ratio (s)
  "S's context fill as a fraction (used/size), or nil when unknown."
  (let ((used (aob-session-ref s :ctx-used))
        (size (aob-session-ref s :ctx-size)))
    (and (numberp used) (numberp size) (> size 0) (/ (float used) size))))

(defun aob-acp--offers-compact-p (s)
  (seq-find (lambda (c) (equal (plist-get c :name) "compact"))
            (aob-session-ref s :commands)))

(defun aob-acp--autocompact-check (s)
  "From S's context reading and settle state, arm or fire autosummarize."
  (when-let* ((ratio aob-acp-autocompact-ratio)
              (r (aob-acp--ctx-ratio s)))
    (cond
     ;; recovered well below the line → ready to fire again next fill
     ((< r (- ratio aob-acp--autocompact-rearm))
      (aob-session-put s :autocompact-fired nil))
     ;; over the line, not yet fired this fill, and safe to fire now
     ((and (>= r ratio)
           (not (aob-session-ref s :autocompact-fired))
           (eq (aob-session-state s) 'idle)
           (null (aob-session-ref s :queued))
           ;; workflow workers compact at their coordinator's discretion
           (not (aob-session-ref s :wf-boss))
           (aob-acp--offers-compact-p s))
      (aob-session-put s :autocompact-fired t)
      (aob-event s 'state
                 :title (format "auto-compacting at %d%% context" (round (* 100 r))))
      (message "aob: %s auto-compacting (%d%% of context window)"
               (aob-session-name s) (round (* 100 r)))
      (aob-acp--prompt-1 s "/compact")))))

(defun aob-acp--autocompact-on-idle (s _old new)
  ;; usage that crosses the line mid-turn can't fire until the turn ends
  (when (eq new 'idle) (aob-acp--autocompact-check s)))

(add-hook 'aob-state-change-hook #'aob-acp--autocompact-on-idle)

;; no dedicated key/verb for manual compaction — the `x' command picker
;; already offers /compact; autosummarize handles it hands-off otherwise

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
  (dolist (d (aob-session-decisions s))
    (aob-acp--respond s (plist-get d :reply-id)
                      (if (eq (plist-get d :kind) 'elicitation)
                          (list :action "cancel")
                        (list :outcome (list :outcome "cancelled")))))
  (setf (aob-session-decisions s) nil)
  ;; a plain cancel stops just the turn; the full cancel (cc) also drops the
  ;; queue so nothing flushes back when the cancelled turn settles
  (when drop-queue (aob-acp--drop-queue s))
  (when (memq (aob-session-state s) '(working blocked))
    (aob-set-state s 'working)
    (aob-acp--notify s "session/cancel" (list :sessionId (aob-acp--acp-id s)))))

(defun aob-acp--steers-p (s)
  "Non-nil when this agent takes a word into the turn it is running."
  (eq t (plist-get (plist-get (aob-session-ref s :agent-meta) :steering)
                   :supported)))

(defun aob-acp--interject (s text)
  "Say TEXT to S now.
`_session/steering' injects it into the running turn at the top of the
agent's queue, so a correction costs the work in flight nothing.  The
agent answers `promptRequired' when there was no turn to steer, which is
the adapter telling us to say it the ordinary way.  An agent that never
advertised steering keeps the old bargain: queue the text and cancel."
  (cond
   ((not (eq (aob-session-state s) 'working)) (aob-acp--prompt-1 s text))
   ((not (aob-acp--steers-p s))
    (aob-acp--queue s text nil)
    (aob-acp--cancel s))
   (t
    (aob-acp--request
     s "_session/steering"
     (list :sessionId (aob-acp--acp-id s)
           :prompt (aob-acp--content-blocks
                    text nil (or (aob-session-dir s) (aob-session-project s))
                    (aob-acp-embeds-p s)))
     (lambda (res err)
       (cond
        ;; a steer that never landed must not swallow what you wrote
        (err (aob-acp--queue s text nil)
             (aob-acp--cancel s))
        ((equal (plist-get res :outcome) "promptRequired")
         (aob-acp--prompt-1 s text))
        (t (aob-event s 'prompt :text text :title "steered"
                      :typed aob-prompt-typed)
           (aob-set-state s 'working))))))))

;;; Goal — an objective the agent holds across turns and keeps working
;;; toward, reporting back how many rounds it has taken and why it last
;;; carried on.  Our own gate still decides what a task may dispatch;
;;; this is the same statement, said to the agent in terms it tracks.

(defun aob-acp--goal-method (s)
  (plist-get (plist-get (aob-session-ref s :agent-meta) :goal) :controlMethod))

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

(defun aob-acp--resolve (s decision answer)
  "Reply to DECISION with ANSWER: a permission's option id, or an
elicitation's ((FIELD . VALUE)...) alist."
  (aob-acp--respond
   s (plist-get decision :reply-id)
   (if (eq (plist-get decision :kind) 'elicitation)
       (list :action "accept"
             :content (let (pl)
                        (dolist (kv answer pl)
                          (setq pl (plist-put
                                    pl (intern (concat ":" (car kv)))
                                    (if (listp (cdr kv))
                                        (vconcat (cdr kv))
                                      (cdr kv)))))))
     (list :outcome (list :outcome "selected" :optionId answer))))
  (setf (aob-session-decisions s)
        (delq decision (aob-session-decisions s)))
  (unless (aob-session-decisions s)
    (aob-set-state s 'working)))

(defun aob-acp--reap-worktree (s)
  "Remove S's worktree and branch when they hold nothing the project lacks:
a clean tree whose branch is merged into (or still at) the project HEAD.
Runs entirely in the background — the kill that triggers it never waits."
  (let* ((dir (aob-session-dir s))
         (project (aob-session-project s))
         (branch (and dir (concat "aob/" (file-name-nondirectory
                                          (directory-file-name dir))))))
    (when (and dir project
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
  (let ((proc (aob-session-conn s)))
    ;; a shared connection must not be left awaiting replies from a corpse:
    ;; answer held decisions, stop the turn, then let go
    (dolist (d (aob-session-decisions s))
      (ignore-errors
        (aob-acp--respond s (plist-get d :reply-id)
                          (if (eq (plist-get d :kind) 'elicitation)
                              (list :action "cancel")
                            (list :outcome (list :outcome "cancelled"))))))
    (setf (aob-session-decisions s) nil)
    (when (and proc (process-live-p proc) (aob-session-ref s :acp-id))
      (when (memq (aob-session-state s) '(working blocked))
        (ignore-errors
          (aob-acp--notify s "session/cancel"
                           (list :sessionId (aob-acp--acp-id s)))))
      ;; cancelling stops the turn; only closing tears the CLI subprocess down,
      ;; and until the last session goes the connection keeps every one alive
      (ignore-errors
        (aob-acp--request s "session/close"
                          (list :sessionId (aob-acp--acp-id s))
                          #'ignore)))
    ;; deferred one-shots watching this session unhook on the transition
    (aob-set-state s 'dead)
    (aob-acp--reap-worktree s)
    (aob-remove-session s)
    (when proc
      (aob-acp--deregister proc s)
      ;; the connection outlives any one session; reap it with the last
      (when (null (aob-acp--conn-sessions proc))
        (aob-acp--conn-cleanup proc)))))

(declare-function aob-trace "aob-trace" (s))
(declare-function aob-trace-buffer "aob-trace" (s))

(defun aob-acp--focus (s)
  (aob-trace s))

(aob-register-backend
 'acp (list :prompt #'aob-acp--prompt
            :cancel #'aob-acp--cancel
            :interject #'aob-acp--interject
            :resolve #'aob-acp--resolve
            :kill #'aob-acp--kill
            :focus #'aob-acp--focus))

;;; Opening sessions — shared-connection core all entry points use

(defcustom aob-acp-start-dir-function nil
  "Function of no arguments returning where a new session should start,
or nil to start where the buffer is.  A host that pins a directory to
something larger than a buffer — a workspace, a tab — sets this so a
session opened from a scratch buffer still belongs to that place."
  :type '(choice function (const nil)) :group 'aob)

(defvar aob-acp-start-dir nil
  "Where a spawn belongs, said outright by a caller that already knows.
A task's root is not a guess to be improved on, so this beats both
`aob-acp-start-dir-function' and the buffer, and is taken as given —
climbing to a `.git' above it would undo the worktree it names.")

(defun aob-acp--project ()
  (if aob-acp-start-dir
      (file-name-as-directory (expand-file-name aob-acp-start-dir))
    (let ((default-directory (or (and aob-acp-start-dir-function
                                      (ignore-errors
                                        (funcall aob-acp-start-dir-function)))
                                 default-directory)))
      (file-name-as-directory
       (expand-file-name
        (or (locate-dominating-file default-directory ".git")
            default-directory))))))

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

(defun aob-acp--worktree-make (project dir done)
  "Create worktree DIR off PROJECT in the background; DONE gets nil or an
error plist."
  (make-directory aob-acp-worktree-root t)
  (ygg-git-async
   project
   (list "worktree" "add" "-b"
         (concat "aob/" (file-name-nondirectory dir)) dir)
   (lambda (out code)
     (funcall done (unless (zerop code)
                     (list :message (format "worktree add failed: %s"
                                            (string-trim out))))))))

(defun aob-acp--gen-name (base)
  "BASE numbered past whatever is already registered under it."
  (let ((n 1))
    (while (aob-session-get (format "acp:%s:%d" base n))
      (setq n (1+ n)))
    (format "%s:%d" base n)))

(defun aob-acp--want-mode (s want)
  "Switch S to the mode id WANT its definition pinned, if advertised."
  (let ((modes (plist-get (aob-session-ref s :modes) :availableModes)))
    (cond
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

(defun aob-acp--model-offered (info want)
  "The id in INFO that WANT names: its id exactly, else a name it is part of."
  (let ((values (cdr info)))
    (or (and (seq-find (lambda (v) (equal (plist-get v :value) want)) values)
             want)
        (let ((needle (downcase want)))
          (plist-get
           (seq-find (lambda (v)
                       (seq-some (lambda (field)
                                   (and (stringp field)
                                        (string-match-p (regexp-quote needle)
                                                        (downcase field))))
                                 (list (plist-get v :name) (plist-get v :value))))
                     values)
           :value)))))

(defun aob-acp--want-model (s want)
  "Switch S to the model WANT names, if the agent offers it.
WANT is an id, or a name such as \"haiku\" that an offered model carries."
  (let* ((info (aob-acp--model-info s))
         (id (aob-acp--model-offered info want)))
    (cond
     ((null id)
      (message "aob: %s has no model %s" (aob-session-name s) want))
     ((equal id (car info)) nil)
     (t (aob-acp--set-model s id)))))

(defun aob-acp--fail (s err)
  (let ((why (or (and err (plist-get err :message)) "error")))
    (aob-session-put s :fail-reason why)
    (aob-set-state s 'failed)
    (aob-event s 'error :title why)
    ;; a quiet death reads as a live-but-broken session — the human
    ;; pokes verbs at a corpse
    (message "aob: %s failed: %s" (aob-session-name s) why)))

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

(defun aob-acp--session-opened (s res &optional fallback-id title)
  "Ingest the result of any session-opening method (new/load/fork).
Every path stores modes/models identically — a resumed or forked
session must not be poorer than a fresh one."
  (aob-acp--register (aob-session-conn s)
                     (or (plist-get res :sessionId) fallback-id) s)
  (when-let* ((modes (plist-get res :modes)))
    (aob-session-put s :modes modes)
    (aob-session-put s :mode-id (plist-get modes :currentModeId)))
  (when-let* ((models (plist-get res :models)))
    (aob-session-put s :models models)
    (aob-acp--models-refresh s))
  ;; claude advertises model/effort/fast through configOptions, not a
  ;; top-level models field
  (when-let* ((opts (plist-get res :configOptions)))
    (aob-acp--config-apply s opts))
  ;; before the queue flushes below — the wire is ordered, so even the
  ;; first turn already runs under the definition's mode
  (when-let* ((want (aob-session-ref s :want-mode)))
    (aob-session-put s :want-mode nil)
    (aob-acp--want-mode s want))
  ;; likewise before the flush: the wire is ordered, so the first turn
  ;; already runs on the model the task was assigned
  (when-let* ((want (aob-session-ref s :want-model)))
    (aob-session-put s :want-model nil)
    (aob-acp--want-model s want))
  ;; and the rest of what the preset asked for — reasoning effort and
  ;; the like — in the same ordered window, so the first turn runs under
  ;; all of it and not only the parts that had a path of their own
  (when-let* ((want (aob-session-ref s :want-config)))
    (aob-session-put s :want-config nil)
    (aob-acp--want-config s want))
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
      ('http (or (null init) (and (plist-get caps :http) t)))
      ('sse (or (null init) (and (plist-get caps :sse) t))))))

(defun aob-acp--mcp-entry (name spec)
  "SPEC under NAME as the entry a session/new carries, or nil."
  (pcase (aob-acp--mcp-kind spec)
    ('stdio
     (when-let* ((command (plist-get spec :command)))
       (list :name name :command command
             :args (vconcat (plist-get spec :args))
             ;; already in the shape a session takes, or a json object
             :env (vconcat
                   (if (and (plist-get spec :env)
                            (plist-get (car (append (plist-get spec :env) nil))
                                       :name))
                       (plist-get spec :env)
                     (let ((env (plist-get spec :env)) out)
                       (while env
                         (let ((key (pop env)) (value (pop env)))
                           (push (list :name (if (keywordp key)
                                                 (substring (symbol-name key) 1)
                                               (format "%s" key))
                                       :value (format "%s" value))
                                 out)))
                       (nreverse out)))))))
    (kind
     (when-let* ((url (plist-get spec :url)))
       ;; headers is not optional in the wire schema: an adapter that
       ;; validates the entry drops a server that leaves it out
       (list :name name :type (symbol-name kind) :url url
             :headers (vconcat (plist-get spec :headers)))))))

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
    (define-key map "g" #'aob-acp-mcp-refresh)
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
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'aob-acp-mcp-mode) (aob-acp-mcp-mode))
        (setq aob-acp-mcp--session s)
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
                   'face 'warning)))
        (goto-char (point-min))))
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
           (all (seq-filter
                 (lambda (entry)
                   (aob-acp--mcp-takes-p
                    (if (plist-get entry :type)
                        (intern (plist-get entry :type))
                      'stdio)
                    init))
                 (append theirs mine))))
      (vconcat all))))

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

(defun aob-acp--connect (s open then)
  "Attach S to its agent's shared connection and open its ACP session."
  (let* ((agent (aob-session-ref s :agent))
         (project (aob-session-project s))
         (proc (or (aob-acp--live-conn agent project)
                   (aob-acp--start-conn agent project))))
    (setf (aob-session-conn s) proc)
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
             (aob-acp--request
              s method params
              (lambda (res err2)
                (if err2 (aob-acp--fail s err2)
                  (funcall then s res)))))))))))

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
                                :state 'starting)))
    (aob-session-put s :agent agent)
    (setq aob-acp--opened-any t)
    (dolist (pair (seq-partition aob-acp-session-refs 2))
      (aob-session-put s (car pair) (cadr pair)))
    ;; the servers bound by whoever opened this session are what it was
    ;; meant to be handed.  The handshake is asynchronous, so by the time
    ;; the adapter asks what to open with, a caller's `let' is long
    ;; unwound and the session would go out with none of them
    (let ((servers aob-acp-mcp-servers)
          (fn open))
      (setq open (lambda (init)
                   (let* ((aob-acp-mcp-servers servers)
                          (spec (funcall fn init)))
                     ;; what went out, kept where it can be read back: an
                     ;; agent that cannot reach a server is a question
                     ;; about what it was handed
                     (when-let* ((params (cadr spec)))
                       (aob-session-put s :mcp-sent
                                        (append (plist-get params :mcpServers) nil)))
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
(defun aob-acp-spawn (agent &optional intent atts name)
  "Spawn ACP AGENT and send INTENT as its first prompt turn.
Whether the session gets an isolated worktree comes from the agent's
definition in `aob-acp-agents'.  INTENT (with image ATTS) is queued
through the handshake and fires the moment the session is ready.
NAME stands in for the agent when the session is numbered, so a session
started for something already named says so in every list it shows up in."
  (interactive
   (let ((agent (completing-read "ACP agent: " (aob-acp-names)
                                 nil t nil nil aob-acp-default-agent)))
     (list agent (read-string (format "%s » " agent)))))
  (let* ((spec (aob-acp-preset agent))
         (base (or (plist-get spec :agent) agent))
         (project (aob-acp--project))
         (worktree (plist-get spec :worktree))
         (dir (if worktree (aob-acp--worktree-path project agent) project))
         (cwd (directory-file-name (expand-file-name dir)))
         (s (aob-acp--open base (aob-acp--gen-name (or name agent)) project dir
                           (lambda (init)
                             (list "session/new"
                                   (list :cwd (aob-acp--wire-dir cwd)
                                         :mcpServers
                                         (aob-acp--mcp-servers init project))))
                           (lambda (s res) (aob-acp--session-opened s res))
                           (and worktree
                                (lambda (_s done)
                                  (aob-acp--worktree-make project dir done))))))
    (aob-session-put s :preset agent)
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
    ("codex" "gpt-5-codex" "gpt-5"))
  "Models to offer per agent before a session is up to ask.
Keyed by the adapter, not by the preset: which models exist is a fact
about the agent, and every preset over it inherits them.
The live list arrives with the handshake; this only seeds the picker, and
a name that is not offered is reported rather than forced."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'aob)

(defun aob-acp-spawn-with (agent project model &optional intent)
  "Spawn AGENT on PROJECT with MODEL, sending INTENT as its first turn.
AGENT is a preset name: what it runs on, how it may act and what it
answers with come from `aob-acp-presets', so the only thing still asked
is where.  MODEL overrides the preset\='s own, for a one-off.

With no INTENT the first turn is written in a compose buffer rather than
the minibuffer, and the session is spawned when that is sent: a first
prompt is the longest one there is, and it can carry attachments."
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
     (list preset (expand-file-name project) nil nil)))
  (let ((dir (file-name-as-directory project)))
    (if intent
        (aob-acp--spawn-with-1 agent dir model intent nil)
      (aob-compose (lambda (text atts)
                     (aob-acp--spawn-with-1 agent dir model text atts))
                   nil (format "new %s" agent) dir))))

(defun aob-acp--spawn-with-1 (agent dir model intent &optional atts)
  "Spawn AGENT in DIR on MODEL with INTENT and ATTS."
  (let* ((default-directory dir)
         (aob-acp-start-dir dir)
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
         (cwd (directory-file-name (expand-file-name (aob-session-dir s))))
         ;; a fork inherits the conversation, not the scope — carry the task
         ;; tag and its extra roots across or the copy loses its other repos
         (aob-acp-session-refs
          (append (when-let* ((task (aob-session-ref s :task))) (list :task task))
                  (when-let* ((dirs (aob-session-ref s :extra-dirs)))
                    (list :extra-dirs dirs))
                  (when-let* ((step (aob-session-ref s :task-step)))
                    (list :task-step step))))
         (new (aob-acp--open
               agent (aob-acp--gen-name agent)
               (aob-session-project s) (aob-session-dir s)
               (lambda (_init)
                 (list "session/fork"
                       (list :sessionId src-id :cwd (aob-acp--wire-dir cwd)
                             :mcpServers (aob-acp--mcp-servers nil cwd))))
               (lambda (new res)
                 (aob-acp--session-opened
                  new res src-id
                  (format "forked from %s" (aob-session-name s)))
                 ;; fork results may omit modes — inherit the source's
                 (unless (aob-session-ref new :modes)
                   (aob-session-put new :modes (aob-session-ref s :modes))
                   (aob-session-put new :mode-id (aob-session-ref s :mode-id))
                   (aob-session-put new :models (aob-session-ref s :models)))))))
    (message "aob: forking %s → %s" (aob-session-name s) (aob-session-name new))
    new))

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
back to the other when the adapter advertises only one."
  :type '(choice (const resume) (const load)) :group 'aob)

(defun aob-acp--resumes-p (init)
  "Non-nil when INIT's initialize result advertises `session/resume'.
The spec puts `sessionCapabilities' at the top of the result; adapters
here nest it under `agentCapabilities', the way additionalDirectories is
already read."
  (aob-acp--session-cap init :resume))

(defun aob-acp--restore-open (init acp-id cwd name verb &optional pref)
  "The open form that brings ACP-ID back, honouring PREF.
PREF defaults to `aob-acp-restore-preference'.  VERB is a cons cell
whose car is set to the verb that restored it, so the session can say
afterwards what it was given."
  (let ((params (list :sessionId acp-id :cwd (aob-acp--wire-dir cwd)
                      :mcpServers (aob-acp--mcp-servers init cwd)))
        (pref (or pref aob-acp-restore-preference))
        (loads (plist-get (plist-get init :agentCapabilities) :loadSession))
        (resumes (aob-acp--resumes-p init)))
    (cond
     ((and resumes (or (eq pref 'resume) (not loads)))
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
             (aob-session-ref s :acp-id))
    (list :agent (aob-session-ref s :agent)
          :name (aob-session-name s)
          :project (aob-session-project s)
          :dir (aob-session-dir s)
          :acp-id (aob-session-ref s :acp-id)
          :task (aob-session-ref s :task)
          :task-step (aob-session-ref s :task-step)
          :model (aob-session-ref s :model-id)
          :mode (aob-session-ref s :mode-id)
          :extra-dirs (aob-session-ref s :extra-dirs))))

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
             (fresh (append live (when going (list going))))
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

(defun aob-acp--persisted-entries ()
  (when (and aob-acp-persist-file (file-readable-p aob-acp-persist-file))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents aob-acp-persist-file)
        (read (current-buffer))))))

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
(declare-function aob-transcript-turns "aob-transcript" (file))

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
      (dolist (turn (ignore-errors (aob-transcript-turns file)))
        (aob-event s (if (equal (car turn) "user") 'prompt 'message)
                   :text (cdr turn))))))

(defun aob-acp-resume-entry (e &optional pref)
  "Respawn persisted entry E, restoring its conversation; return the session.
PREF overrides `aob-acp-restore-preference' for this one entry.
Prompts sent while it opens queue and fire on readiness."
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
          (append (when-let* ((task (plist-get e :task))) (list :task task))
                  (when-let* ((dirs (plist-get e :extra-dirs)))
                    (list :extra-dirs (seq-filter #'file-directory-p dirs)))
                  (when-let* ((step (plist-get e :task-step)))
                    (list :task-step step))
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
         (verb (list nil)))
    ;; the conversation goes in before the adapter answers: opening a
    ;; session takes seconds, and a trace that is empty for those seconds
    ;; is a conversation that looks lost
    (let ((s (aob-acp--open
              (plist-get e :agent) name (plist-get e :project) dir
              (lambda (init) (aob-acp--restore-open init acp-id cwd name verb pref))
              (lambda (s res)
                (aob-session-put s :restored-by (car verb))
                ;; a second time only where the first found nothing
                (aob-acp--seed-history s e)
                (aob-acp--session-opened s res acp-id "session resumed")))))
      ;; `aob-acp--open' is what makes the session; a caller that stands
      ;; in for it hands back whatever it likes
      (when (aob-session-p s) (aob-acp--seed-history s e))
      s)))

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
starts empty — this is the verb for reconnecting, and the one every
restart takes by default."
  (interactive (list (aob-acp--restore-target)))
  (aob-acp-resume-entry e 'resume))

;;;###autoload
(defun aob-acp-reload-session (e)
  "Bring conversation E back and replay its whole history into the buffer.
The verb for reading a transcript again; reconnecting costs less."
  (interactive (list (aob-acp--restore-target)))
  (aob-acp-resume-entry e 'load))

;;;###autoload
(defun aob-acp-restart (s)
  "Restart the CLI behind S, reloading the same conversation into it.
The process is what breaks — the session is not.  Its ACP id, the task
it is on, the directories it may see, the model and mode it was on and
the space it belongs to all outlive the CLI, so this is that conversation
loaded into a fresh process under the same name, not a new session.

Dead or wedged either way: the old process is killed first, which is
what frees the name for the new one to take."
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
    (aob--call s :kill)
    (message "aob: restarting %s" (plist-get entry :name))
    (aob-acp-resume-entry entry)))

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
     (lambda (_init err)
       (if err
           (message "aob: %s failed to start: %s" agent (plist-get err :message))
         (aob-acp--request-proc
          proc "session/list" (list :cwd (aob-acp--wire-dir cwd))
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
                       (name (aob-acp--gen-name agent))
                       (verb (list nil))
                       (s (aob-acp--open
                           agent name project project
                           (lambda (init)
                             (aob-acp--restore-open init sid cwd name verb))
                           (lambda (s res2)
                             (aob-session-put s :restored-by (car verb))
                             (aob-acp--session-opened s res2 sid
                                                      "session resumed")))))
                  (when (and aob-acp-show-trace (not noninteractive)
                             (fboundp 'aob-trace-buffer))
                    (ygg-ui-show (aob-trace-buffer s) t))))))))))))

(provide 'aob-acp)
;;; aob-acp.el ends here
