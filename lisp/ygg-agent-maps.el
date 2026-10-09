;;; ygg-agent-maps.el --- repo map and feature summary for a fresh agent chat -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(declare-function aob-session-ref "aob")
(declare-function aob-session-put "aob")
(declare-function aob-session-project "aob")
(declare-function aob-session-dir "aob")
(declare-function aob-session-backend "aob")
(declare-function project-root "project")
(declare-function aob-live-sessions "aob")
(declare-function aob-acp-spawn "aob-acp")
(declare-function aob-acp-preset "aob-acp")
(defvar aob-acp-default-agent)
(defvar aob-acp-start-dir)
(defvar aob-acp-start-worktree)
(defvar aob-acp-presets)
(defvar aob-acp-before-first-prompt-functions)
(defvar aob-acp-context-cleared-functions)
(defvar aob-state-change-hook)
(defvar aob-session-created-hook)

(defgroup ygg-agent-maps nil
  "Project maps handed to a new agent session with its first prompt."
  :group 'yggdrasil)

(defcustom ygg-agent-maps-enabled t
  "Whether a new agent session is handed its project's maps."
  :type 'boolean)

(defcustom ygg-agent-maps-map-budget 2000
  "Tokens the condensed repo map may take."
  :type 'natnum)

(defcustom ygg-agent-maps-summary-budget 1200
  "Tokens the condensed feature summary may take."
  :type 'natnum)

(defcustom ygg-agent-maps-project-budgets nil
  "Budgets per project root, as (ROOT :map TOKENS :summary TOKENS)."
  :type '(alist :key-type directory :value-type plist))

(defcustom ygg-agent-maps-wait 0.3
  "Seconds the first prompt waits for maps still being generated."
  :type 'number)

(defcustom ygg-agent-maps-subagents 'pointer
  "What a subagent session is handed.
`pointer' is the lat line only, `full' the whole block, nil nothing."
  :type '(choice (const :tag "Lat pointer only" pointer)
                 (const :tag "Full maps" full)
                 (const :tag "Nothing" nil)))

(defcustom ygg-agent-maps-interpreter "node"
  "Program the ICE tools need on PATH, or nil when they need none."
  :type '(choice (const nil) string))

(defcustom ygg-agent-maps-repowise-update t
  "Whether generating maps first refreshes an existing repowise index."
  :type 'boolean)

(defcustom ygg-agent-maps-tools-dir
  (expand-file-name
   "etc/ice/"
   (file-name-directory
    (directory-file-name
     (file-name-directory (or load-file-name buffer-file-name default-directory)))))
  "Folder holding ice-repo-map and ice-feature-summary."
  :type 'directory)

(defvar ygg-agent-maps--cache (make-hash-table :test #'equal))

(defun ygg-agent-maps--root (dir)
  (file-name-as-directory (expand-file-name dir)))

(defun ygg-agent-maps--budget (root kind)
  (or (plist-get (cdr (assoc root ygg-agent-maps-project-budgets
                             (lambda (a b)
                               (equal (ygg-agent-maps--root a)
                                      (ygg-agent-maps--root b)))))
                 kind)
      (if (eq kind :map) ygg-agent-maps-map-budget ygg-agent-maps-summary-budget)))

(defun ygg-agent-maps--tool (name)
  (let ((file (expand-file-name name ygg-agent-maps-tools-dir)))
    (and (file-executable-p file)
         (or (null ygg-agent-maps-interpreter)
             (executable-find ygg-agent-maps-interpreter))
         file)))

(defun ygg-agent-maps--run (dir argv done &optional merge-err)
  "Run ARGV in DIR; DONE gets the exit status and stdout when it ends.
MERGE-ERR puts stderr into that text.  Returns the process, or nil when
none started."
  (let* ((default-directory dir)
         (out (generate-new-buffer " *ygg-agent-maps*"))
         (err (generate-new-buffer " *ygg-agent-maps-err*")))
    (condition-case nil
        (make-process
         :name "ygg-agent-maps" :command argv :buffer out
         :stderr (if merge-err out err)
         :noquery t :connection-type 'pipe
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (let ((text (with-current-buffer out (buffer-string))))
               (kill-buffer out)
               (when (buffer-live-p err) (kill-buffer err))
               (funcall done (process-exit-status proc) text)))))
      (error (kill-buffer out)
             (kill-buffer err)
             (funcall done -1 "")
             nil))))

(defun ygg-agent-maps--entry (root)
  (or (gethash root ygg-agent-maps--cache)
      (puthash root (list :pending 0) ygg-agent-maps--cache)))

(defun ygg-agent-maps--bump (entry n)
  (plist-put entry :pending (+ n (plist-get entry :pending))))

(defun ygg-agent-maps--spawn (root argv done &optional merge-err)
  (let ((entry (ygg-agent-maps--entry root)))
    (ygg-agent-maps--bump entry 1)
    (let ((proc (apply #'ygg-agent-maps--run
                 root argv
                 (lambda (status out)
                   (unwind-protect (funcall done status out)
                     (ygg-agent-maps--bump entry -1)
                     (when-let* ((_ (zerop (plist-get entry :pending)))
                                 (idle (plist-get entry :on-idle)))
                       (plist-put entry :on-idle nil)
                       (funcall idle))))
                 (and merge-err '(t)))))
      (when (processp proc)
        (plist-put entry :procs (cons proc (plist-get entry :procs))))
      proc)))

(defconst ygg-agent-maps--stat-cap 200
  "Most dirty paths whose modification time is read per refresh.")

(defconst ygg-agent-maps--path-skip '(("1" . 8) ("2" . 9) ("u" . 10) ("?" . 1))
  "Space-separated fields before the path in a porcelain v2 record, by type.")

(defun ygg-agent-maps--record-path (record)
  (when-let* ((skip (cdr (assoc (substring record 0 1) ygg-agent-maps--path-skip)))
              (_ (string-match (format "\\`\\(?:[^ ]+ \\)\\{%d\\}\\(\\(?:.\\|\n\\)*\\)" skip)
                               record)))
    (match-string 1 record)))

(defun ygg-agent-maps--state (root out)
  "(OID DIRTY-FINGERPRINT) read from `git status --porcelain=v2 --branch -z' OUT.
The fingerprint is nil for a clean tree, else the hash of every dirty record
and the newest modification time among the first `ygg-agent-maps--stat-cap'
paths they list."
  (let ((records (split-string out "\0" t))
        (stat-left ygg-agent-maps--stat-cap)
        (newest 0.0)
        oid dirty)
    (while records
      (let ((record (pop records)))
        (cond ((string-match "\\`# branch\\.oid \\(.+\\)" record)
               (setq oid (match-string 1 record)))
              ((string-prefix-p "#" record))
              (t
               (push record dirty)
               (when (string-prefix-p "2 " record)
                 (push (pop records) dirty))
               (when-let* ((_ (> stat-left 0))
                           (path (ygg-agent-maps--record-path record)))
                 (cl-decf stat-left)
                 (setq newest (max newest
                                   (or (ygg-agent-maps--mtime
                                        (expand-file-name path root))
                                       0.0))))))))
    (list oid (and dirty
                   (cons (secure-hash 'md5 (mapconcat #'identity (nreverse dirty) "\0"))
                         newest)))))

(defun ygg-agent-maps--mtime (file)
  (when-let* ((attrs (file-attributes file)))
    (float-time (file-attribute-modification-time attrs))))

(defun ygg-agent-maps--generate (root entry kind tool key)
  (when (and tool (not (equal key (plist-get entry (if (eq kind :map) :map-key :sum-key)))))
    (ygg-agent-maps--spawn
     root
     (list tool "--root" (directory-file-name root)
           "--budget" (number-to-string (ygg-agent-maps--budget root kind)))
     (lambda (status out)
       (when (and (eql status 0) (not (string-blank-p out)))
         (plist-put entry kind (string-trim out))
         (plist-put entry (if (eq kind :map) :map-key :sum-key) key))))))

(defun ygg-agent-maps--marker ()
  "The marker ice-latgen-filter strips, read from the script, or nil."
  (let ((file (expand-file-name "ice-latgen-filter" ygg-agent-maps-tools-dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (when (re-search-forward "^marker='\\(.+\\)'$" nil t)
          (match-string 1))))))

(defun ygg-agent-maps--filter-ok (root done)
  "Call DONE with non-nil when ROOT's latgen filter is installed and applied."
  (ygg-agent-maps--spawn
   root (list "git" "-C" (directory-file-name root) "config" "filter.latgen.clean")
   (lambda (status out)
     (if-let* ((marker (ygg-agent-maps--marker))
               (_ (and (eql status 0) (string-search marker out))))
         (ygg-agent-maps--spawn
          root (list "git" "-C" (directory-file-name root)
                     "check-attr" "filter" "--" "lat.md/lat.md")
          (lambda (status out)
            (funcall done (and (eql status 0)
                               (equal (string-trim out) "lat.md/lat.md: filter: latgen")))))
       (funcall done nil)))))

(defun ygg-agent-maps--write (root entry tool key)
  (when (and tool
             (file-directory-p (expand-file-name "lat.md" root))
             (not (equal key (plist-get entry :write-key))))
    (ygg-agent-maps--filter-ok
     root
     (lambda (ok)
       (when ok
         (plist-put entry :write-key key)
         (ygg-agent-maps--spawn
          root (list tool "--root" (directory-file-name root) "--write")
          (lambda (status _out)
            (plist-put entry :written (eql status 0)))))))))

(defun ygg-agent-maps-refresh (root &optional force)
  "Regenerate what is stale for ROOT in the background; at most one run at a time.
FORCE starts the run even while another call's own processes are pending."
  (unless (file-remote-p root)
    (let* ((root (ygg-agent-maps--root root))
           (entry (ygg-agent-maps--entry root)))
      (when (or force (zerop (plist-get entry :pending)))
        (ygg-agent-maps--spawn
         root (list "git" "-C" (directory-file-name root)
                    "status" "--porcelain=v2" "--branch" "-uno" "-z")
         (lambda (status out)
           (let* ((state (and (eql status 0) (ygg-agent-maps--state root out)))
                  (head (list (or (car state) (ygg-agent-maps--mtime root))
                              (and (cadr state) t)))
                  (map-tool (ygg-agent-maps--tool "ice-repo-map"))
                  (features (expand-file-name "lat.md/features.md" root))
                  (sum-tool (and (file-readable-p features)
                                 (ygg-agent-maps--tool "ice-feature-summary"))))
             (ygg-agent-maps--generate
              root entry :map map-tool
              (list (car head) (cadr state) (ygg-agent-maps--budget root :map)))
             (ygg-agent-maps--generate
              root entry :summary sum-tool
              (list (ygg-agent-maps--mtime features)
                    (ygg-agent-maps--budget root :summary)))
             (ygg-agent-maps--write root entry map-tool head))))))))

(defvar ygg-agent-maps--deadline nil
  "The time the outermost wait gives up, shared by waits nested inside it.")

(defun ygg-agent-maps--wait (entry)
  (let ((ygg-agent-maps--deadline
         (or ygg-agent-maps--deadline (+ (float-time) ygg-agent-maps-wait))))
    (while (and (> (plist-get entry :pending) 0)
                (< (float-time) ygg-agent-maps--deadline))
      (let ((procs (cl-remove-if-not #'process-live-p (plist-get entry :procs))))
        (plist-put entry :procs procs)
        (if procs
            (dolist (proc procs) (accept-process-output proc 0.02 nil t))
          (accept-process-output nil 0.01))))))

(defun ygg-agent-maps--drop-details (text)
  (string-join (seq-remove (lambda (line) (string-prefix-p "Details:" line))
                           (split-string text "\n"))
               "\n"))

(defun ygg-agent-maps--names (root)
  (append
   (and (file-readable-p (expand-file-name "lat.md/features.md" root))
        '("[[features]]"))
   (and (file-readable-p (expand-file-name "lat.md/repo-map.md" root))
        '("[[repo-map]]"))))

(defun ygg-agent-maps--pointer (names &optional root)
  (let ((lat (and names
                  (format "Details: `lat section|locate|refs <id>` on %s; `lat search \"<q>\"` needs LAT_LLM_KEY.\n"
                          (string-join names " and "))))
        (repowise (and root
                       (file-readable-p (expand-file-name ".repowise/wiki.db" root))
                       "Code signals, add `--format json`: `repowise context <file> --include health --include callers`, `risk <rev>`, `dead-code`, `why \"<q>\" --target <file>`, `symbol <file>::<Name>`, `impacted-tests <rev>`. Only these: other repowise commands write files or call an LLM.\n")))
    (and (or lat repowise) (concat lat repowise))))

(defun ygg-agent-maps--expected (root)
  (append (and (ygg-agent-maps--tool "ice-repo-map") '(map))
          (and (file-readable-p (expand-file-name "lat.md/features.md" root))
               (ygg-agent-maps--tool "ice-feature-summary")
               '(summary))))

(defun ygg-agent-maps--block (root &optional pointer-only sent)
  "(TEXT PARTS MORE) for ROOT, or nil; waits briefly on maps in flight.
PARTS are the maps TEXT carries, never those in SENT; MORE says some are
still to come. POINTER-ONLY keeps just the lat pointer line."
  (let* ((root (ygg-agent-maps--root root))
         (entry (ygg-agent-maps--entry root))
         (names (ygg-agent-maps--names root)))
    (if pointer-only
        (when-let* ((line (ygg-agent-maps--pointer names root)))
          (list (concat "<project-maps>\n" line "</project-maps>") nil nil))
      (ygg-agent-maps--wait entry)
      (let* ((map (and (not (memq 'map sent)) (plist-get entry :map)))
             (summary (and (not (memq 'summary sent))
                           (plist-get entry :summary)
                           (string-trim (ygg-agent-maps--drop-details
                                         (plist-get entry :summary)))))
             (pointer (ygg-agent-maps--pointer names root))
             (parts (append (and map '(map)) (and summary '(summary))))
             (more (and (> (plist-get entry :pending) 0)
                        (cl-set-difference (ygg-agent-maps--expected root)
                                           (append sent parts)))))
        (when (or parts (and pointer (not more)))
          (list (concat
                 "<project-maps>\n"
                 (and map (format "<repo-map>\n%s\n</repo-map>\n" map))
                 (and summary (format "<features>\n%s\n</features>\n" summary))
                 (and more "Maps are still generating for this project.\n")
                 pointer
                 "</project-maps>")
                parts
                (and more t)))))))

(defun ygg-agent-maps--project (s)
  (or (aob-session-dir s) (aob-session-project s)))

(defun ygg-agent-maps--mode (s)
  (if (aob-session-ref s :parent-session) ygg-agent-maps-subagents 'full))

(defun ygg-agent-maps-warm (s)
  "Start generating S's project maps ahead of its first prompt."
  (condition-case nil
      (when-let* ((_ (and ygg-agent-maps-enabled
                          (eq (aob-session-backend s) 'acp)
                          (not (aob-session-ref s :restored-by))
                          (not (aob-session-ref s :cost-inherited))
                          (eq (ygg-agent-maps--mode s) 'full)))
                  (dir (ygg-agent-maps--project s))
                  (_ (not (file-remote-p dir))))
        (ygg-agent-maps-refresh dir))
    (error nil)))

(defun ygg-agent-maps-on-ready (s)
  "Mark new session S for the maps and refresh them; never holds the turn."
  (condition-case nil
      (when (and ygg-agent-maps-enabled
                 (ygg-agent-maps--mode s)
                 (not (aob-session-ref s :restored-by))
                 (not (aob-session-ref s :cost-inherited)))
        (when-let* ((dir (ygg-agent-maps--project s))
                    (_ (not (file-remote-p dir)))
                    (_ (file-directory-p dir)))
          (aob-session-put s :maps-pending t)
          (when (eq (ygg-agent-maps--mode s) 'full)
            (ygg-agent-maps-refresh dir))))
    (error nil))
  nil)

(defun ygg-agent-maps--place (orig s)
  (let ((place (funcall orig s)))
    (if-let* ((_ (and ygg-agent-maps-enabled (aob-session-ref s :maps-pending)))
              (dir (ygg-agent-maps--project s))
              (_ (not (file-remote-p dir)))
              (mode (ygg-agent-maps--mode s)))
        (let* ((sent (aob-session-ref s :maps-sent))
               (result (condition-case nil
                           (ygg-agent-maps--block dir (eq mode 'pointer) sent)
                         (error nil)))
               (block (car result)))
          (cond (block
                 (aob-session-put s :maps-flight (cadr result))
                 (aob-session-put s :maps-more (nth 2 result))
                 (aob-session-put s :maps-pending 'sent))
                ((zerop (plist-get (ygg-agent-maps--entry (ygg-agent-maps--root dir))
                                   :pending))
                 (aob-session-put s :maps-pending nil)))
          (if block
              (list :type "text"
                    :text (if place (concat (plist-get place :text) "\n\n" block) block))
            place))
      place)))

(defun ygg-agent-maps--settle (s _old new)
  "Once S's maps prompt ends, count its parts as sent, or keep them if it failed."
  (when (and (eq new 'idle) (eq (aob-session-ref s :maps-pending) 'sent))
    (if (aob-session-ref s :turn-error)
        (aob-session-put s :maps-pending t)
      (aob-session-put s :maps-sent (append (aob-session-ref s :maps-flight)
                                            (aob-session-ref s :maps-sent)))
      (aob-session-put s :maps-pending (aob-session-ref s :maps-more)))))

(defun ygg-agent-maps--cleared (s)
  (dolist (k '(:maps-sent :maps-flight :maps-more))
    (aob-session-put s k nil))
  (aob-session-put s :maps-pending t))

(defun ygg-agent-maps--default-root ()
  (or (and (fboundp 'project-current)
           (when-let* ((project (project-current)))
             (project-root project)))
      (locate-dominating-file default-directory ".git")
      (user-error "No project here")))

(defun ygg-agent-maps--tokens (text)
  (ceiling (length text) 2.5))

(defun ygg-agent-maps--live-sessions-in (root)
  (let ((root (file-truename (file-name-as-directory (expand-file-name root)))))
    (and (fboundp 'aob-live-sessions)
         (seq-filter
          (lambda (s)
            (when-let* ((dir (ygg-agent-maps--project s)))
              (string-prefix-p
               root (file-truename (file-name-as-directory (expand-file-name dir))))))
          (aob-live-sessions)))))

(defun ygg-agent-maps--report (root entry filter-ok why)
  (let* ((map (plist-get entry :map))
         (summary (plist-get entry :summary))
         (name (abbreviate-file-name (directory-file-name root)))
         (parts
          (delq nil
                (list (if map
                          (format "repo map ~%d tokens" (ygg-agent-maps--tokens map))
                        "repo map not generated")
                      (and summary
                           (format "feature summary ~%d tokens"
                                   (ygg-agent-maps--tokens summary)))
                      (cond ((plist-get entry :written)
                             "full map written to lat.md/repo-map.md")
                            ((not (file-directory-p (expand-file-name "lat.md" root)))
                             "full map not written: no lat.md folder")
                            (why (format "full map not written: %s" why))
                            ((not filter-ok)
                             "full map not written: git filter latgen not installed")
                            (t "full map not written"))))))
    (message "maps %s: %s" name (string-join parts "; "))))

(defun ygg-agent-maps--regenerate (root filter-ok &optional why)
  (let ((entry (ygg-agent-maps--entry root)))
    (dolist (key '(:map-key :sum-key :write-key :written)) (plist-put entry key nil))
    (plist-put entry :on-idle (lambda () (ygg-agent-maps--report root entry filter-ok why)))
    (ygg-agent-maps-refresh root t)))

(defun ygg-agent-maps--install (root script proceed)
  (ygg-agent-maps--spawn
   root (list script (directory-file-name root))
   (lambda (status out)
     (let ((line (car (split-string (string-trim out) "\n"))))
       (message "latgen filter %s"
                (cond ((eql status 0) (string-trim out))
                      ((and line (not (string-empty-p line)))
                       (format "install failed: %s" line))
                      (t "install failed")))
       (funcall proceed (eql status 0))))
   t))

(defun ygg-agent-maps--offer-install (root script proceed)
  "Offer the latgen filter for ROOT when it is the git toplevel, else say why not.
PROCEED gets whether the filter is in place and, if not, why."
  (ygg-agent-maps--spawn
   root (list "git" "-C" (directory-file-name root) "rev-parse" "--show-toplevel")
   (lambda (status out)
     (let ((top (and (eql status 0) (string-trim out))))
       (cond
        ((not top) (funcall proceed nil "not a git repo"))
        ((not (equal (file-name-as-directory (file-truename top))
                     (file-name-as-directory (file-truename root))))
         (funcall proceed nil "lat.md is not at the git toplevel"))
        ((y-or-n-p
          (format "Install the latgen git filter in %s so the full map can be written? It sets filter.latgen.clean and .smudge in .git/config and appends `lat.md/lat.md filter=latgen' to .gitattributes. "
                  (abbreviate-file-name (directory-file-name root))))
         (ygg-agent-maps--install root script proceed))
        (t (funcall proceed nil)))))))

(defconst ygg-agent-maps--repowise-unset-env
  '("ANTHROPIC_API_KEY" "OPENAI_API_KEY" "OPENROUTER_API_KEY" "GEMINI_API_KEY"
    "GOOGLE_API_KEY" "DEEPSEEK_API_KEY" "KIMI_API_KEY" "EDENAI_API_KEY"
    "LITELLM_API_KEY" "REPOWISE_API_KEY"
    "ANTHROPIC_BASE_URL" "OPENAI_BASE_URL" "GEMINI_BASE_URL" "DEEPSEEK_BASE_URL"
    "KIMI_BASE_URL" "EDENAI_BASE_URL" "OLLAMA_BASE_URL" "LITELLM_BASE_URL"
    "LITELLM_API_BASE"
    "REPOWISE_PROVIDER" "REPOWISE_MODEL" "REPOWISE_DOC_MODEL" "REPOWISE_EMBEDDER"
    "REPOWISE_EMBEDDING_MODEL" "REPOWISE_EMBEDDING_DIMS"
    "REPOWISE_EMBEDDING_DECLARED_DIMS" "OLLAMA_EMBEDDING_MODEL"
    "OLLAMA_EMBEDDING_DIMS" "REPOWISE_DB_URL" "REPOWISE_DATABASE_URL"
    "REPOWISE_REASONING"))

(defcustom ygg-agent-maps-repowise-update-timeout 120
  "Seconds the repowise index refresh may run before it is killed."
  :type 'natnum)

(defun ygg-agent-maps--repowise-claude-md-off-p (root)
  "Non-nil when ROOT's .repowise/config.yaml sets editor_files.claude_md to false."
  (let ((file (expand-file-name ".repowise/config.yaml" root))
        (case-fold-search nil))
    (and (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (and (re-search-forward "^editor_files:[ \t]*" nil t)
                (if (looking-at "{")
                    (re-search-forward "[{,][ \t]*claude_md:[ \t]*[Ff]alse[ \t]*[,}]"
                                       (line-end-position) t)
                  (let ((end (save-excursion
                               (forward-line)
                               (if (re-search-forward "^[^ \t\n#]" nil t)
                                   (match-beginning 0)
                                 (point-max)))))
                    (forward-line)
                    (re-search-forward "^[ \t]+claude_md:[ \t]*[Ff]alse[ \t]*\\(?:#.*\\)?$"
                                       end t))))))))

(defun ygg-agent-maps--repowise-update (root next)
  "Refresh ROOT's repowise index without a model, then call NEXT either way.
Runs only when repowise is configured not to rewrite CLAUDE.md."
  (if-let* ((_ ygg-agent-maps-repowise-update)
            (_ (file-directory-p (expand-file-name ".repowise" root)))
            (bin (executable-find "repowise")))
      (if (not (ygg-agent-maps--repowise-claude-md-off-p root))
          (progn
            (message "aob: repowise update skipped — set editor_files.claude_md: false in .repowise/config.yaml (it would rewrite CLAUDE.md)")
            (funcall next))
        (let ((process-environment
               (append '("DO_NOT_TRACK=1" "REPOWISE_TELEMETRY_DISABLED=1")
                       (cl-remove-if
                        (lambda (e)
                          (member (car (split-string e "=")) ygg-agent-maps--repowise-unset-env))
                        process-environment))))
          (let (timer timed-out finished proc)
            (setq proc
             (ygg-agent-maps--spawn
             root (list bin "update" "--index-only" "--no-agents")
             (lambda (status _out)
               (setq finished t)
               (when timer (cancel-timer timer))
               (cond (timed-out
                      (message "aob: repowise update timed out after %ds"
                               ygg-agent-maps-repowise-update-timeout))
                     ((not (eql status 0))
                      (message "aob: repowise update failed (exit %s)" status)))
               (funcall next))))
            (when (and (processp proc) (not finished) (process-live-p proc))
              (setq timer (run-at-time
                           ygg-agent-maps-repowise-update-timeout nil
                           (lambda ()
                             (when (process-live-p proc)
                               (setq timed-out t)
                               (kill-process proc)))))))))
    (funcall next)))

(defun ygg-agent-maps--start-feature-agent (root)
  (if-let* ((live (ygg-agent-maps--live-sessions-in root)))
      (message "Feature map session skipped: %d live agent session%s in %s"
               (length live) (if (cdr live) "s" "")
               (abbreviate-file-name (directory-file-name root)))
    (ygg-agent-maps-features root)))

;;;###autoload
(defun ygg-agent-maps-generate (&optional root)
  "Regenerate ROOT's repo map now and start the feature map agent session.
The repo map is rebuilt in the background even when not stale; without the
latgen git filter the full map is not written, and the filter is offered
first.  Only after that is settled does the feature agent start.  Refused
while an agent session is live in ROOT.  The feature summary follows
features.md the next time the maps refresh."
  (interactive)
  (let ((root (ygg-agent-maps--root (or root (ygg-agent-maps--default-root)))))
    (when (file-remote-p root) (user-error "Maps are not generated over TRAMP"))
    (when-let* ((live (ygg-agent-maps--live-sessions-in root)))
      (user-error "Agent %s is running in %s; maps not updated"
                  (mapconcat (lambda (s) (format "%s" (aob-session-name s))) live ", ")
                  (abbreviate-file-name (directory-file-name root))))
    (unless (zerop (plist-get (ygg-agent-maps--entry root) :pending))
      (user-error "Maps are already generating for %s" root))
    (ygg-agent-maps--repowise-update
     root
     (lambda ()
       (let ((script (expand-file-name "ice-latgen-filter" ygg-agent-maps-tools-dir))
             (lat (file-directory-p (expand-file-name "lat.md" root))))
         (ygg-agent-maps--filter-ok
          root
          (lambda (ok)
            (let ((proceed (lambda (ok &optional why)
                             (ygg-agent-maps--regenerate root ok why)
                             (ygg-agent-maps--start-feature-agent root))))
              (if (or ok (not lat) (not (file-executable-p script)))
                  (funcall proceed ok)
                (ygg-agent-maps--offer-install root script proceed))))))))))

(defun ygg-agent-maps--in-root-preset (agent)
  (let ((spec (copy-sequence (aob-acp-preset agent))))
    (cons agent (cl-loop for (k v) on spec by #'cddr
                         unless (eq k :worktree) append (list k v)))))

(defun ygg-agent-maps-features (&optional root)
  "Start an agent session that writes or refreshes ROOT's lat.md/features.md.
Uses create-verification-skill when the file is missing, else
maintain-verification-skill.  Works in ROOT itself, never a new worktree.
Refused while an agent session is live in ROOT."
  (interactive)
  (let ((root (ygg-agent-maps--root (or root (ygg-agent-maps--default-root)))))
    (when (file-remote-p root) (user-error "Maps are not generated over TRAMP"))
    (when-let* ((live (ygg-agent-maps--live-sessions-in root)))
      (user-error "Feature map skipped in %s: %d live agent session%s there"
                  (abbreviate-file-name (directory-file-name root))
                  (length live) (if (cdr live) "s" "")))
    (require 'aob-acp)
    (let* ((exists (file-exists-p (expand-file-name "lat.md/features.md" root)))
           (skill (if exists "maintain-verification-skill" "create-verification-skill"))
           (verb (if exists "refresh" "write"))
           (aob-acp-start-dir root)
           (aob-acp-start-worktree nil)
           (aob-acp-presets (cons (ygg-agent-maps--in-root-preset aob-acp-default-agent)
                                  aob-acp-presets))
           (prompt (format "Use the %s skill to %s lat.md/features.md from source. If that skill is not available, %s lat.md/features.md from source yourself, following the format of %s. Then verify each feature's keys and commands exist in the source, and correct any that do not."
                           skill verb verb
                           (if exists "the existing file" "the other notes in lat.md and lat.md/lat.md"))))
      (or (aob-acp-spawn aob-acp-default-agent prompt nil "feature map")
          (user-error "Could not start %s" aob-acp-default-agent)))))

(with-eval-after-load 'aob-acp
  (add-hook 'aob-acp-before-first-prompt-functions #'ygg-agent-maps-on-ready)
  (add-hook 'aob-session-created-hook #'ygg-agent-maps-warm)
  (add-hook 'aob-state-change-hook #'ygg-agent-maps--settle)
  (add-hook 'aob-acp-context-cleared-functions #'ygg-agent-maps--cleared)
  (advice-add 'aob-acp--place-block :around #'ygg-agent-maps--place))

(provide 'ygg-agent-maps)
;;; ygg-agent-maps.el ends here
