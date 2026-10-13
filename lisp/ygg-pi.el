;;; ygg-pi.el --- the pi coding agent behind aob: MCP and launch environment -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ygg-agent-conf)

(declare-function aob-acp--edit-profile-plist "aob-acp" (name spec))

(declare-function aob-session-ref "aob")
(declare-function aob-session-put "aob")
(declare-function aob-session-project "aob")
(declare-function aob-session-dir "aob")
(declare-function aob-acp--mcp-entry "aob-acp")
(declare-function aob-acp--mcp-absolute "aob-acp")
(declare-function aob-acp-lat-entry "aob-acp")
(declare-function aob-acp-project-mcp-servers "aob-acp")
(defvar aob-acp-agents)
(defvar aob-acp-lat-mcp)
(defvar aob-acp-project-mcp-file)
(defvar aob-acp--mcp-dropped)

(defgroup ygg-pi nil "The pi coding agent under aob." :group 'yggdrasil)

(defcustom ygg-pi-approve-project nil
  "Whether sessions run pi with --approve, loading a project's own .pi files.
Pi in rpc mode cannot ask, so without this it skips the project's .pi
settings, extensions, skills, prompts and mcp.json instead of hanging."
  :type 'boolean :group 'ygg-pi)

(defconst ygg-pi--wrapper
  (expand-file-name "../etc/pi/aob-pi"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "The pi launcher that loads aob's MCP extension.")

(defun ygg-pi--command (agent)
  "AGENT's launch command as one string, its name when it has none."
  (let ((command (plist-get (cdr (assoc agent aob-acp-agents)) :command)))
    (cond ((consp command) (string-join command " "))
          ((and (stringp command) (not (string-empty-p command))) command)
          (t agent))))

(defun ygg-pi-agent-p (agent)
  "Whether AGENT runs pi: named pi or pi-*, or launched through a pi binary."
  (and (stringp agent)
       (equal (ygg-agent--kind agent (ygg-pi--command agent)) "pi")))

(defun ygg-pi--own-server-names (agent project)
  "Names pi loads itself from the mcp.json of AGENT's home in PROJECT."
  (mapcar (lambda (entry) (plist-get entry :name))
          (ignore-errors
            (ygg-agent-user-mcp-servers
             (or (ygg-agent--kind agent (ygg-pi--command agent)) "pi") project))))

(defun ygg-pi--pairs-problem (pairs)
  (unless (null pairs)
    (unless (and (or (vectorp pairs) (proper-list-p pairs))
                 (seq-every-p (lambda (pair)
                                (and (proper-list-p pair)
                                     (stringp (plist-get pair :name))
                                     (let ((value (plist-get pair :value)))
                                       (or (null value) (stringp value) (numberp value)))))
                              pairs))
      t)))

(defun ygg-pi--entry-problem (entry)
  "Why pi cannot load ENTRY, or nil; the rules of toPiConfig in aob-pi-mcp.js."
  (let ((name (plist-get entry :name))
        (url (plist-get entry :url))
        (command (plist-get entry :command))
        (args (plist-get entry :args))
        (cwd (plist-get entry :cwd)))
    (cond
     ((not (and (stringp name) (not (string-empty-p name)))) "no name")
     ((equal (plist-get entry :type) "sse")
      "pi supports stdio and streamable HTTP, not sse")
     ((and url (not (equal url "")))
      (cond ((not (stringp url)) "url is not a string")
            ((ygg-pi--pairs-problem (plist-get entry :headers))
             "headers are not an alist or plist")))
     ((and command (not (equal command "")))
      (cond ((not (stringp command)) "command is not a string")
            ((not (or (null args)
                      (and (or (vectorp args) (proper-list-p args))
                           (seq-every-p #'stringp args))))
             "args are not a list of strings")
            ((ygg-pi--pairs-problem (plist-get entry :env))
             "env is not an alist or plist")
            ((and cwd (not (stringp cwd))) "cwd is not a string")))
     (t "neither url nor command"))))

(defun ygg-pi--stringify-pairs (pairs)
  (vconcat (mapcar (lambda (pair)
                     (let ((value (plist-get pair :value)))
                       (if (numberp value)
                           (list :name (plist-get pair :name) :value (format "%s" value))
                         pair)))
                   pairs)))

(defun ygg-pi--stringify-entry (entry)
  (let ((entry (copy-sequence entry)))
    (dolist (key '(:headers :env))
      (when (plist-get entry key)
        (setq entry (plist-put entry key (ygg-pi--stringify-pairs (plist-get entry key))))))
    entry))

(defun ygg-pi--warn-skipped (s skipped)
  "Warn once per distinct SKIPPED list for session S."
  (when (and skipped (not (equal skipped (aob-session-ref s :pi-mcp-skipped))))
    (aob-session-put s :pi-mcp-skipped skipped)
    (display-warning
     'ygg-pi
     (format "pi MCP servers skipped: %s"
             (mapconcat (lambda (cell) (format "%s (%s)" (car cell) (cdr cell)))
                        skipped "; "))
     :warning)))

(defun ygg-pi-session-servers (s)
  "The servers session S would be handed in session/new, as wire entries.
Pi's adapter drops mcpServers, so these travel in the environment.  What
pi's own mcp.json names it loads itself, so those are left out; entries
pi cannot load are warned about once and left out.  Names that collapse
to the same pi server name are suffixed by the extension, not here."
  (let* ((project (aob-session-project s))
         (dir (or (aob-session-dir s) project))
         (agent (aob-session-ref s :agent))
         (own (ygg-pi--own-server-names agent project))
         (skipped nil)
         (mine (seq-remove
                (lambda (e) (member (plist-get e :name) own))
                (delq nil (mapcar (lambda (server)
                                    (let ((name (plist-get server :name)))
                                      (condition-case err
                                          (or (aob-acp--mcp-entry name server)
                                              (progn (push (cons name "neither url nor command, from user config or sidecar")
                                                           skipped)
                                                     nil))
                                        (error (push (cons name (format "%s, from user config or sidecar"
                                                                      (error-message-string err)))
                                                     skipped)
                                               nil))))
                                  (aob-session-ref s :mcp-declared)))))
         (names (append own (mapcar (lambda (e) (plist-get e :name)) mine)))
         (theirs (seq-remove (lambda (e) (member (plist-get e :name) names))
                             (aob-acp-project-mcp-servers project)))
         (lat (and aob-acp-lat-mcp
                   (not (member "lat" own))
                   (aob-acp-lat-entry dir mine theirs)))
         (aob-acp--mcp-dropped nil)
         valid)
    (dolist (tagged (append (mapcar (lambda (e) (cons e aob-acp-project-mcp-file)) theirs)
                            (mapcar (lambda (e) (cons e "user config or sidecar")) mine)
                            (and lat (list (cons lat "lat.md")))))
      (let ((entry (car tagged)))
        (if-let* ((why (ygg-pi--entry-problem entry)))
            (push (cons (or (plist-get entry :name) "?")
                        (format "%s, from %s" why (cdr tagged)))
                  skipped)
          (push (ygg-pi--stringify-entry entry) valid))))
    (ygg-pi--warn-skipped s (nreverse skipped))
    (mapcar (lambda (entry) (or (aob-acp--mcp-absolute entry) entry))
            (nreverse valid))))

(defun ygg-pi-session-env (s)
  "What S's pi connection needs in its environment, or nil for another agent."
  (when (ygg-pi-agent-p (aob-session-ref s :agent))
    (append
     (list (concat "PI_ACP_PI_COMMAND=" ygg-pi--wrapper)
           (concat "AOB_PI_MCP_SERVERS="
                   (json-serialize (vconcat (ygg-pi-session-servers s)))))
     (and ygg-pi-approve-project (list "AOB_PI_APPROVE=1"))
     (list (concat "AOB_PI_SKILL_CORE=" (string-join ygg-agent-skill-core " "))))))

;;; A pi profile's engine and model live in its own home's settings

(defun ygg-pi--home-models (home)
  "The provider/model ids pi can reach from HOME, by its own --list-models."
  (when-let* ((pi (executable-find "pi")))
    (let ((process-environment (cons (concat "PI_CODING_AGENT_DIR=" home)
                                     process-environment)))
      (with-temp-buffer
        (when (eql 0 (ignore-errors (call-process pi nil t nil "--list-models")))
          (let (ids)
            (dolist (line (cdr (split-string (buffer-string) "\n" t)) (nreverse ids))
              (pcase (split-string line)
                (`(,provider ,model . ,_) (push (concat provider "/" model) ids))))))))))

(defun ygg-pi-edit-profile (name spec)
  "Set profile NAME's provider and model in its pi home, SPEC unchanged.
A profile without a home of its own has nowhere but SPEC to keep them."
  (if-let* (((ygg-pi-agent-p (plist-get spec :agent)))
            (profile (plist-get spec :profile))
            (home (ygg-agent--profile-home profile "pi")))
      (let* ((file (expand-file-name "settings.json" home))
             (settings (or (ignore-errors
                             (json-parse-string
                              (with-temp-buffer (insert-file-contents file) (buffer-string))))
                           (make-hash-table :test #'equal)))
             (now (when-let* ((p (gethash "defaultProvider" settings))
                              (m (gethash "defaultModel" settings)))
                    (concat p "/" m)))
             (pick (string-trim
                    (completing-read (format "Profile %s model (provider/model, empty = pi default): " name)
                                     (ygg-pi--home-models home) nil nil now))))
        (if (string-empty-p pick)
            (progn (remhash "defaultProvider" settings) (remhash "defaultModel" settings))
          (unless (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" pick)
            (user-error "pi: give the model as provider/model"))
          (puthash "defaultProvider" (match-string 1 pick) settings)
          (puthash "defaultModel" (match-string 2 pick) settings))
        (unless (file-exists-p file) (with-temp-file file (insert "{}")))
        (ygg-agent--replace-json file settings)
        spec)
    (aob-acp--edit-profile-plist name spec)))

;;;###autoload
(defun ygg-pi-set-api-key (profile provider key)
  "Keep KEY as PROVIDER's API key in pi PROFILE's home, as pi's /login does."
  (interactive
   (list (completing-read "pi profile: "
                          (delq nil (mapcar (lambda (p)
                                              (and (ygg-pi-agent-p (plist-get (cdr p) :agent))
                                                   (plist-get (cdr p) :profile)))
                                            (bound-and-true-p aob-acp-profiles)))
                          nil t)
         (string-trim (completing-read "Provider: " '("anthropic" "openai" "google" "openrouter"
                                                      "groq" "xai" "mistral" "deepseek")))
         (read-passwd "API key: ")))
  (when (or (string-empty-p provider) (string-empty-p key))
    (user-error "pi: a provider and a key, both"))
  (let* ((file (expand-file-name "auth.json" (ygg-agent--profile-home profile "pi")))
         (auth (if (file-exists-p file)
                   (condition-case nil
                       (json-parse-string
                        (with-temp-buffer (insert-file-contents file) (buffer-string)))
                     (error (user-error "pi: %s does not parse; fix it before adding a key"
                                        (abbreviate-file-name file))))
                 (make-hash-table :test #'equal)))
         (entry (make-hash-table :test #'equal)))
    (puthash "type" "api_key" entry)
    (puthash "key" key entry)
    (puthash provider entry auth)
    (unless (file-exists-p file)
      (with-temp-file file (insert "{}"))
      (set-file-modes file #o600))
    (ygg-agent--replace-json file auth)
    (message "pi: %s key kept for profile %s" provider profile)))

(provide 'ygg-pi)
;;; ygg-pi.el ends here
