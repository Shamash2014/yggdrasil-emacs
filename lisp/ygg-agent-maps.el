;;; ygg-agent-maps.el --- repo map and feature summary for a fresh agent chat -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(declare-function aob-session-ref "aob")
(declare-function aob-session-put "aob")
(declare-function aob-session-project "aob")
(declare-function aob-session-dir "aob")
(declare-function aob-session-backend "aob")
(defvar aob-acp-before-first-prompt-functions)
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

(defun ygg-agent-maps--run (dir argv done)
  "Run ARGV in DIR; DONE gets the exit status and stdout when it ends.
Returns the process, or nil when none started."
  (let* ((default-directory dir)
         (out (generate-new-buffer " *ygg-agent-maps*"))
         (err (generate-new-buffer " *ygg-agent-maps-err*")))
    (condition-case nil
        (make-process
         :name "ygg-agent-maps" :command argv :buffer out :stderr err
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

(defun ygg-agent-maps--spawn (root argv done)
  (let ((entry (ygg-agent-maps--entry root)))
    (ygg-agent-maps--bump entry 1)
    (let ((proc (ygg-agent-maps--run
                 root argv
                 (lambda (status out)
                   (unwind-protect (funcall done status out)
                     (ygg-agent-maps--bump entry -1))))))
      (when (processp proc)
        (plist-put entry :procs (cons proc (plist-get entry :procs)))))))

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

(defun ygg-agent-maps--write (root entry tool key)
  (when (and tool
             (file-directory-p (expand-file-name "lat.md" root))
             (not (equal key (plist-get entry :write-key))))
    (ygg-agent-maps--spawn
     root (list "git" "-C" (directory-file-name root) "config" "filter.latgen.clean")
     (lambda (status out)
       (when-let* ((marker (ygg-agent-maps--marker))
                   (_ (and (eql status 0) (string-search marker out))))
         (ygg-agent-maps--spawn
          root (list "git" "-C" (directory-file-name root)
                     "check-attr" "filter" "--" "lat.md/lat.md")
          (lambda (status out)
            (when (and (eql status 0)
                       (equal (string-trim out) "lat.md/lat.md: filter: latgen"))
              (plist-put entry :write-key key)
              (ygg-agent-maps--spawn
               root (list tool "--root" (directory-file-name root) "--write")
               #'ignore)))))))))

(defun ygg-agent-maps-refresh (root)
  "Regenerate what is stale for ROOT in the background; at most one run at a time."
  (unless (file-remote-p root)
    (let* ((root (ygg-agent-maps--root root))
           (entry (ygg-agent-maps--entry root)))
      (when (zerop (plist-get entry :pending))
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

(defun ygg-agent-maps--pointer (names)
  (and names
       (format "Query details through the lat MCP: lat_search / lat_section on %s.\n"
               (string-join names " and "))))

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
        (when-let* ((line (ygg-agent-maps--pointer names)))
          (list (concat "<project-maps>\n" line "</project-maps>") nil nil))
      (ygg-agent-maps--wait entry)
      (let* ((map (and (not (memq 'map sent)) (plist-get entry :map)))
             (summary (and (not (memq 'summary sent))
                           (plist-get entry :summary)
                           (string-trim (ygg-agent-maps--drop-details
                                         (plist-get entry :summary)))))
             (parts (append (and map '(map)) (and summary '(summary))))
             (more (and (> (plist-get entry :pending) 0)
                        (cl-set-difference (ygg-agent-maps--expected root)
                                           (append sent parts)))))
        (when parts
          (list (concat
                 "<project-maps>\n"
                 (and map (format "<repo-map>\n%s\n</repo-map>\n" map))
                 (and summary (format "<features>\n%s\n</features>\n" summary))
                 (and more "Maps are still generating for this project.\n")
                 (ygg-agent-maps--pointer names)
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

(with-eval-after-load 'aob-acp
  (add-hook 'aob-acp-before-first-prompt-functions #'ygg-agent-maps-on-ready)
  (add-hook 'aob-session-created-hook #'ygg-agent-maps-warm)
  (add-hook 'aob-state-change-hook #'ygg-agent-maps--settle)
  (advice-add 'aob-acp--place-block :around #'ygg-agent-maps--place))

(provide 'ygg-agent-maps)
;;; ygg-agent-maps.el ends here
