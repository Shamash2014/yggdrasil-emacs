;;; ygg-db.el --- Databases through usql: connections, a drawer, query buffers -*- lexical-binding: t; -*-

;; Third-party: usql (mise aqua:xo/usql), the one adapter for every driver.
;; After vim-dadbod, vim-dadbod-ui and vim-dadbod-completion.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sql)
(require 'auth-source)
(require 'url-parse)
(require 'url-util)
(require 'json)
(require 'project)

(declare-function ygg-visidata-open-file "ygg-visidata" (file))
(declare-function ygg-visidata-remove-with-buffer "ygg-visidata" (buffer directory))
(declare-function yggdrasil-localleader-def "yggdrasil-localleader" (mode key def &optional label))
(declare-function yggdrasil-define-mode-keys "yggdrasil-core" (mode states &rest bindings))
(declare-function yggdrasil-define-keys "yggdrasil-core" (state &rest bindings))
(defvar ygg-modal-special-modes)
(defvar ygg-leader-open-map)
(defvar ygg--visual-p)
(defvar ygg--state)
(defvar vertico-group-format)
(defvar ygg-db-connection)

(defgroup ygg-db nil
  "Databases through usql."
  :group 'tools)

(defun ygg-db--connection-alist-p (value)
  (and (listp value)
       (cl-every (lambda (entry) (and (consp entry) (stringp (car entry)) (stringp (cdr entry))))
                 value)))

(defcustom ygg-db-connections nil
  "Connections as (NAME . URL), URL in usql form, such as pg://user@host/db.
Leave passwords out: they are read from auth-source when a query runs."
  :type '(alist :key-type string :value-type string)
  :safe #'ygg-db--connection-alist-p)

(defcustom ygg-db-usql-program "usql"
  "The usql executable."
  :type 'string)

(defcustom ygg-db-directory (locate-user-emacs-file "var/ygg-db/")
  "Where added connections and saved queries live."
  :type 'directory)

(defcustom ygg-db-result-max-rows 500
  "Rows drawn in the result window; the rest wait for VisiData."
  :type 'natnum)

(defcustom ygg-db-cell-max-width 48
  "Widest a cell is drawn before it is cut."
  :type 'natnum)

(defcustom ygg-db-table-limit 100
  "Rows the drawer's table listing asks for."
  :type 'natnum)

(defcustom ygg-db-drawer-width 36
  "Columns of the drawer's side window."
  :type 'natnum)

(defcustom ygg-db-result-height 0.35
  "Height of the result side window, a fraction of the frame."
  :type 'number)

(defcustom ygg-db-detect-depth 2
  "Directory levels under a project root searched for database files."
  :type 'natnum)

(defface ygg-db-rule '((t :inherit shadow))
  "Table rules and column separators.")

(defface ygg-db-null '((t :inherit shadow :slant italic))
  "NULL cells.")

(defface ygg-db-meta '((t :inherit shadow))
  "Row counts, timings, notes and drawer annotations.")

(defface ygg-db-header '((t :weight bold))
  "Column names.")

(defface ygg-db-failure '((t :inherit error))
  "The glyph before an error.")

(defconst ygg-db--null (string #x2400)
  "What usql prints for NULL, so it differs from an empty string.")

(defconst ygg-db--config-name "ygg_db"
  "Name of the connection in the run's own usql config.")

;;; Drivers

(defconst ygg-db--families
  '(("postgres" "postgres" "pg" "pgsql" "postgresql" "pgx" "px" "cockroachdb" "cr" "cdb"
     "crdb" "cockroach" "redshift" "rs")
    ("mysql" "mysql" "my" "maria" "aurora" "mariadb" "percona" "memsql" "me" "tidb" "ti"
     "vitess" "vt" "mymysql" "zm" "mymy")
    ("sqlite" "sqlite3" "sqlite" "sq" "file" "moderncsqlite" "mq" "modernsqlite")
    ("duckdb" "duckdb" "dk" "ddb" "duck")
    ("sqlserver" "sqlserver" "ms" "mssql" "azuresql")
    ("oracle" "oracle" "or" "ora" "oci" "oci8" "odpi" "odpi-c" "godror" "gr")
    ("clickhouse" "clickhouse" "ch")
    ("snowflake" "snowflake" "sf")
    ("trino" "trino" "tr" "trs" "trinos" "presto" "pr" "prs" "prestos" "prestodb" "prestodbs")
    ("redis" "redis" "rediss"))
  "Driver family, then the usql schemes that belong to it.")

(defconst ygg-db--default-ports
  '(("postgres" . "5432") ("mysql" . "3306") ("sqlserver" . "1433")
    ("oracle" . "1521") ("clickhouse" . "9000") ("redis" . "6379"))
  "Port a family listens on when its URL names none.")

(defconst ygg-db--default-schemas
  '(("postgres" . "public") ("sqlite" . "main") ("duckdb" . "main") ("sqlserver" . "dbo"))
  "Schema whose tables need no qualifier.")

(defun ygg-db--scheme (url)
  "The usql scheme URL starts with, lower case, without a transport."
  (when (string-match "\\`\\([A-Za-z][A-Za-z0-9.-]*\\)\\(?:\\+[^:]*\\)?:" url)
    (downcase (match-string 1 url))))

(defun ygg-db--family (url)
  "The driver family of URL, else its scheme."
  (let ((scheme (ygg-db--scheme url)))
    (or (car (cl-find-if (lambda (family) (member scheme (cdr family))) ygg-db--families))
        scheme)))

(defun ygg-db--file-based-p (url)
  (member (ygg-db--family url) '("sqlite" "duckdb")))

(defun ygg-db--redis-p (conn)
  (equal (ygg-db--family (plist-get conn :url)) "redis"))

(defun ygg-db--sql-product (family)
  (pcase family
    ("postgres" 'postgres) ("mysql" 'mysql) ("sqlite" 'sqlite)
    ("oracle" 'oracle) ("sqlserver" 'ms) (_ 'ansi)))

;;; Connections

(defun ygg-db--make-connection (name url source &optional root)
  (list :name name :url url :source source :root root))

(defun ygg-db--project-root (&optional directory)
  (when-let* ((project (project-current nil (or directory default-directory))))
    (expand-file-name (project-root project))))

(defun ygg-db--read-alist (file)
  "The connection alist FILE holds, read as data, never evaluated."
  (when (file-readable-p file)
    (let ((value (with-temp-buffer
                   (insert-file-contents file)
                   (ignore-errors (read (current-buffer))))))
      (and (ygg-db--connection-alist-p value) value))))

(defun ygg-db--file-url (scheme path)
  "The usql URL of the database file at PATH under SCHEME.
Nil when usql cannot open that path."
  (let* ((allowed (url--allowed-chars (cons ?/ url-unreserved-chars)))
         (escaped (url-hexify-string path allowed)))
    (cond ((equal escaped path) (concat scheme ":" path))
          ((equal (ygg-db--family (concat scheme ":")) "duckdb")
           ;; usql and go-duckdb each unescape once, then cut at ? and choke on a bare %.
           (unless (string-match-p "[?%]" path)
             (concat scheme ":" (url-hexify-string escaped allowed))))
          ((not (equal scheme "file")) (concat scheme ":file:" escaped)))))

(defun ygg-db--expand-file-url (url root)
  "URL with a relative sqlite or duckdb path made absolute against ROOT."
  (if (and root (ygg-db--file-based-p url)
           (string-match "\\`\\([^:]+\\):\\(?://\\)?\\([^/].*\\)\\'" url)
           (not (string-prefix-p "file:" (match-string 2 url))))
      (let ((scheme (match-string 1 url))
            (path (expand-file-name (match-string 2 url) root)))
        (or (ygg-db--file-url scheme path) url))
    url))

(defun ygg-db--project-dir (root)
  "ROOT's .ygg-db directory, when it is one."
  (let ((dir (expand-file-name ".ygg-db/" root)))
    (and (file-directory-p dir) dir)))

(defun ygg-db--project-connections (root)
  (let* ((dir (ygg-db--project-dir root))
         (file (if dir (expand-file-name "connections.eld" dir) (expand-file-name ".ygg-db" root))))
    (mapcar (lambda (entry)
              (ygg-db--make-connection (car entry) (ygg-db--expand-file-url (cdr entry) root)
                                       'project root))
            (ygg-db--read-alist file))))

(defun ygg-db--saved-file ()
  (expand-file-name "connections.eld" ygg-db-directory))

(defun ygg-db--saved-connections ()
  (mapcar (lambda (entry) (ygg-db--make-connection (car entry) (cdr entry) 'saved))
          (ygg-db--read-alist (ygg-db--saved-file))))

(defun ygg-db--file-kind (file)
  "sqlite or duckdb when FILE starts with that engine's magic, else nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (ignore-errors (insert-file-contents-literally file nil 0 16))
    (cond ((string-prefix-p "SQLite format 3" (buffer-string)) "sqlite")
          ((and (>= (buffer-size) 12) (equal (buffer-substring 9 13) "DUCK")) "duckdb"))))

(defun ygg-db--database-files (root depth)
  (let (found)
    (dolist (file (ignore-errors (directory-files root t directory-files-no-dot-files-regexp t)))
      (let ((base (file-name-nondirectory file)))
        (cond ((string-prefix-p "." base))
              ((file-directory-p file)
               (when (and (> depth 0) (not (member base '("node_modules" "target" "build" "dist"))))
                 (setq found (nconc found (ygg-db--database-files file (1- depth))))))
              ((string-match-p "\\.\\(?:sqlite3?\\|db\\|duckdb\\|ddb\\)\\'" base)
               (when-let* ((kind (ygg-db--file-kind file)))
                 (setq found (nconc found (list (cons kind file)))))))))
    found))

(defun ygg-db--detected-connections (root)
  (delq nil
        (mapcar (lambda (hit)
                  (when-let* ((url (ygg-db--file-url (car hit) (cdr hit))))
                    (ygg-db--make-connection (file-relative-name (cdr hit) root) url 'detected root)))
                (ygg-db--database-files root ygg-db-detect-depth))))

(defun ygg-db-connections-here (&optional directory)
  "Every connection reachable from DIRECTORY, first name wins.
Project .ygg-db, then added connections, then ygg-db-connections (which
dir-locals may set), then database files found in the project."
  (let* ((root (ygg-db--project-root directory))
         (all (append (and root (ygg-db--project-connections root))
                      (ygg-db--saved-connections)
                      (mapcar (lambda (entry)
                                (ygg-db--make-connection
                                 (car entry) (ygg-db--expand-file-url (cdr entry) root) 'config root))
                              ygg-db-connections)
                      (and root (ygg-db--detected-connections root))))
         (seen (make-hash-table :test #'equal)))
    (cl-remove-if (lambda (conn) (prog1 (gethash (plist-get conn :name) seen)
                                   (puthash (plist-get conn :name) t seen)))
                  all)))

(defun ygg-db--find-connection (name &optional directory)
  (cl-find name (ygg-db-connections-here directory)
           :key (lambda (conn) (plist-get conn :name)) :test #'equal))

(defun ygg-db--redact (url)
  "URL without its password."
  (let ((parsed (url-generic-parse-url url)))
    (if (url-password parsed)
        (progn (setf (url-password parsed) nil) (url-recreate-url parsed))
      url)))

;;; Credentials

(defun ygg-db--auth-secret (parsed family)
  (let* ((port (or (url-portspec parsed) (cdr (assoc family ygg-db--default-ports))))
         (user (url-user parsed))
         (found (car (apply #'auth-source-search
                            :host (url-host parsed)
                            :port (delq nil (list (and port (format "%s" port))
                                                  (url-type parsed) family))
                            :max 1
                            (and user (not (string-empty-p user))
                                 (list :user (url-unhex-string user)))))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret) (funcall secret) secret)))))

(defun ygg-db--dsn (url)
  "URL with the password it needs, from itself or auth-source."
  (if (ygg-db--file-based-p url)
      url
    (let* ((parsed (url-generic-parse-url url))
           (secret (cond ((url-password parsed) (url-unhex-string (url-password parsed)))
                         ((and (url-user parsed) (url-host parsed))
                          (ygg-db--auth-secret parsed (ygg-db--family url))))))
      (if (not secret)
          url
        (setf (url-password parsed) (url-hexify-string secret))
        (url-recreate-url parsed)))))

(defun ygg-db--program ()
  (or (executable-find ygg-db-usql-program)
      (let ((shim (expand-file-name "~/.local/share/mise/shims/usql")))
        (and (file-executable-p shim) shim))
      (user-error "usql not found; mise use -g aqua:xo/usql")))

(defun ygg-db--private-directory ()
  (with-file-modes #o700 (make-temp-file "ygg-db-" t)))

(defun ygg-db--write-private (file text)
  (with-file-modes #o600
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil file nil 'silent))))

(defun ygg-db--command (url directory script)
  "The usql command for SCRIPT against URL, its files written into DIRECTORY.
Returns (ARGV . ENVIRONMENT).  The DSN, password and all, goes into a
0600 config file named by USQL_CONFIG; argv carries only its name."
  (let ((config (expand-file-name "config.yaml" directory))
        (file (expand-file-name "query.sql" directory)))
    (ygg-db--write-private config (format "connections:\n  %s: %s\n" ygg-db--config-name
                                          (json-encode-string (ygg-db--dsn url))))
    (ygg-db--write-private file script)
    (cons (list (ygg-db--program) "-X" "-w" "-C" "-P" (concat "null=" ygg-db--null)
                "-f" file ygg-db--config-name)
          (list (concat "USQL_CONFIG=" config) "NO_COLOR=1" "TERM=dumb"
                "USQL_SHOW_HOST_INFORMATION=false"))))

;;; Statements

(defun ygg-db--blank-sql-p (text)
  (with-temp-buffer
    (set-syntax-table sql-mode-syntax-table)
    (insert text)
    (goto-char (point-min))
    (forward-comment (buffer-size))
    (skip-chars-forward " \t\n;")
    (eobp)))

(defun ygg-db--statement-bounds (start end)
  "Bounds (BEG . END) of the statements between START and END.
A statement ends at a semicolon or a blank line outside strings and
comments; a line starting with a backslash is a usql command of its own.
On a Redis connection every line that is not blank or a # comment is one."
  (if (and ygg-db-connection (ygg-db--redis-p ygg-db-connection))
      (ygg-db--line-bounds start end)
    (ygg-db--sql-bounds start end)))

(defun ygg-db--line-bounds (start end)
  (save-excursion
    (goto-char start)
    (let (bounds)
      (while (< (point) end)
        (let ((beg (max start (line-beginning-position)))
              (stop (min end (line-end-position))))
          (unless (string-match-p "\\`[ \t]*\\(?:#.*\\)?\\'" (buffer-substring-no-properties beg stop))
            (push (cons beg stop) bounds)))
        (forward-line 1))
      (nreverse bounds))))

(defun ygg-db--sql-bounds (start end)
  (save-excursion
    (with-syntax-table (if (derived-mode-p 'sql-mode) (syntax-table) sql-mode-syntax-table)
      (let (bounds)
        (goto-char start)
        (while (progn (skip-chars-forward " \t\n" end) (< (point) end))
          (let ((beg (point)))
            (if (eq (char-after) ?\\)
                (goto-char (min end (line-end-position)))
              (let (done)
                (while (and (not done) (re-search-forward ";\\|\n[ \t]*\\(?:\n\\|\\\\\\)" end 'move))
                  (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
                    (setq done t)
                    (unless (eq (char-before) ?\;)
                      (goto-char (match-beginning 0)))))))
            (let ((text (buffer-substring-no-properties beg (point))))
              (unless (ygg-db--blank-sql-p text)
                (push (cons beg (point)) bounds)))
            (when (= (point) beg) (forward-char 1))))
        (nreverse bounds)))))

(defun ygg-db--statements (start end)
  (mapcar (lambda (bound) (string-trim (buffer-substring-no-properties (car bound) (cdr bound))))
          (ygg-db--statement-bounds start end)))

(defun ygg-db--statement-at (position)
  "Bounds of the statement at POSITION, else the one before it."
  (let ((bounds (ygg-db--statement-bounds (point-min) (point-max))))
    (or (cl-find-if (lambda (bound) (<= (car bound) position (cdr bound))) bounds)
        (car (last (cl-remove-if (lambda (bound) (> (car bound) position)) bounds)))
        (car bounds))))

(defun ygg-db--terminate (statement)
  (if (or (string-prefix-p "\\" statement) (string-suffix-p ";" statement))
      statement
    (concat statement ";")))

(defun ygg-db--script (statements nonce)
  (mapconcat (lambda (statement)
               (format "%s\n\\echo %s\n\\warn %s\n" (ygg-db--terminate statement) nonce nonce))
             statements ""))

;;; CSV

(defun ygg-db--csv-field ()
  "Read the field at point, leaving point after it."
  (if (eq (char-after) ?\")
      (let ((parts nil) (done nil))
        (forward-char 1)
        (while (not done)
          (let ((start (point)))
            (if (not (search-forward "\"" nil t))
                (progn (push (buffer-substring-no-properties start (point-max)) parts)
                       (goto-char (point-max))
                       (setq done t))
              (push (buffer-substring-no-properties start (1- (point))) parts)
              (if (eq (char-after) ?\")
                  (progn (push "\"" parts) (forward-char 1))
                (setq done t)))))
        (apply #'concat (nreverse parts)))
    (let ((start (point)))
      (skip-chars-forward "^,\n")
      (let ((text (buffer-substring-no-properties start (point))))
        (unless (equal text ygg-db--null) text)))))

(defun ygg-db--csv-record ()
  (let (fields (more t))
    (while more
      (push (ygg-db--csv-field) fields)
      (if (eq (char-after) ?,)
          (forward-char 1)
        (setq more nil)
        (when (eq (char-after) ?\n) (forward-char 1))))
    (nreverse fields)))

(defun ygg-db--csv-skip-record ()
  (let ((more t))
    (while more
      (skip-chars-forward "^,\n\"")
      (pcase (char-after)
        (?\" (forward-char 1)
             (while (and (search-forward "\"" nil 'move) (eq (char-after) ?\"))
               (forward-char 1)))
        (?, (forward-char 1))
        (_ (setq more nil) (unless (eobp) (forward-char 1)))))))

(defun ygg-db-parse-csv (text &optional limit)
  "Records of the CSV TEXT as (RECORDS . TOTAL), at most LIMIT of them read.
A field usql printed as NULL becomes nil."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((records nil) (total 0))
      (while (not (eobp))
        (if (or (null limit) (< total limit))
            (push (ygg-db--csv-record) records)
          (ygg-db--csv-skip-record))
        (cl-incf total))
      (cons (nreverse records) total))))

;;; Results

(defconst ygg-db--query-words
  '("select" "with" "values" "show" "describe" "desc" "pragma" "explain" "table"
    "from" "call" "exec" "summarize" "pivot" "unpivot" "list")
  "First words of statements that return rows.")

(defun ygg-db--returns-rows-p (statement)
  (let ((case-fold-search t))
    (and (string-match "\\`[ \t\n(]*\\([A-Za-z]+\\)" statement)
         (member (downcase (match-string 1 statement)) ygg-db--query-words))))

(defun ygg-db--chunks (text nonce count)
  "TEXT cut at the NONCE lines into COUNT chunks, missing ones nil."
  (let ((parts (split-string text (concat (regexp-quote nonce) "\n"))))
    (cl-loop for i below count collect (nth i parts))))

(defun ygg-db--set (statement chunk failure)
  "What STATEMENT gave: its CHUNK of stdout and FAILURE text of stderr."
  (let ((failure (and failure (not (string-blank-p failure)) (string-trim failure)))
        (chunk (or chunk "")))
    (cond
     ((string-prefix-p "\\" statement)
      (list :statement statement :text (string-trim-right chunk) :error failure))
     ((string-empty-p chunk)
      (list :statement statement :error failure :status (unless failure "done")))
     ((and (not (string-search "\n" (string-trim-right chunk)))
           (not (ygg-db--returns-rows-p statement)))
      (list :statement statement :status (string-trim chunk) :error failure))
     (t
      (pcase-let ((`(,records . ,total) (ygg-db-parse-csv chunk (1+ ygg-db-result-max-rows))))
        (list :statement statement :columns (car records) :rows (cdr records)
              :total (max 0 (1- total)) :csv chunk :error failure))))))

(defun ygg-db--result (statements stdout stderr nonce exit)
  (let* ((count (length statements))
         (outs (ygg-db--chunks stdout nonce count))
         (errs (if (string-search nonce stderr) (ygg-db--chunks stderr nonce count)
                 (make-list count nil)))
         (general (and (not (string-search nonce stderr)) (not (string-blank-p stderr))
                       (string-trim stderr))))
    (list :sets (cl-loop for statement in statements
                         for out in outs
                         for err in errs
                         for i from 0
                         when (or out err (zerop i))
                         collect (ygg-db--set statement out err))
          :error general :exit exit)))

;;; Runs

(cl-defstruct (ygg-db-run (:constructor ygg-db--run-create))
  connection statements callback owner process started nonce directory)

(defvar ygg-db--running (make-hash-table :test #'equal)
  "Connection URL to its running ygg-db-run.")

(defvar ygg-db--queued (make-hash-table :test #'equal)
  "Connection URL to the runs waiting for it, oldest first.
One at a time per database: DuckDB locks its file against a second process.")

(defun ygg-db--key (conn)
  (plist-get conn :url))

(defun ygg-db-run (conn statements callback &optional owner)
  "Run STATEMENTS on CONN in one usql process; CALLBACK gets the result plist.
OWNER is the buffer that asked, so it can interrupt.  Runs on one
connection wait for each other."
  (let ((run (ygg-db--run-create :connection conn :statements statements
                                 :callback callback :owner owner))
        (key (ygg-db--key conn)))
    (if (or (gethash key ygg-db--running) (gethash key ygg-db--queued))
        (puthash key (append (gethash key ygg-db--queued) (list run)) ygg-db--queued)
      (ygg-db--start run))
    run))

(defun ygg-db--local-directory (conn)
  (let ((root (plist-get conn :root)))
    (if (and root (not (file-remote-p root)) (file-directory-p root))
        root
      (expand-file-name "~/"))))

(defun ygg-db--run-command (conn directory statements nonce)
  "(ARGV ENVIRONMENT INPUT) that run STATEMENTS on CONN, files kept in DIRECTORY."
  (if (ygg-db--redis-p conn)
      (pcase-let ((`(,argv . ,preamble) (ygg-db--redis-command (plist-get conn :url))))
        (list argv nil (concat preamble (ygg-db--redis-script statements nonce))))
    (pcase-let ((`(,argv . ,environment)
                 (ygg-db--command (plist-get conn :url) directory (ygg-db--script statements nonce))))
      (list argv environment nil))))

(defun ygg-db--start (run)
  (let* ((conn (ygg-db-run-connection run))
         (nonce (format "ygg-db-%s" (md5 (format "%s%s" (random) (float-time)))))
         (directory (ygg-db--private-directory))
         (started nil))
    (unwind-protect
        (pcase-let* ((`(,argv ,environment ,input)
                      (ygg-db--run-command conn directory (ygg-db-run-statements run) nonce))
                     (default-directory (ygg-db--local-directory conn))
                     (process-environment (append environment process-environment))
                     (errors (generate-new-buffer " *ygg-db stderr*" t))
                     (process (make-process
                               :name "ygg-db" :command argv :noquery t :connection-type 'pipe
                               :coding 'utf-8-unix
                               :buffer (generate-new-buffer " *ygg-db stdout*" t)
                               :stderr errors :sentinel #'ygg-db--sentinel)))
          (setf (ygg-db-run-process run) process
                (ygg-db-run-started run) (float-time)
                (ygg-db-run-nonce run) nonce
                (ygg-db-run-directory run) directory)
          (process-put process 'ygg-db-run run)
          (process-put process 'ygg-db-stderr errors)
          (when-let* ((pipe (get-buffer-process errors))) (set-process-sentinel pipe #'ignore))
          (puthash (ygg-db--key conn) run ygg-db--running)
          (when input (process-send-string process input))
          (process-send-eof process)
          (setq started t))
      (unless started (delete-directory directory t)))))

(defun ygg-db--drain (buffer)
  (if (not (buffer-live-p buffer))
      ""
    (let ((pipe (get-buffer-process buffer)))
      (when pipe
        (while (accept-process-output pipe 0.02 nil t))
        (delete-process pipe))
      (prog1 (with-current-buffer buffer (buffer-string))
        (kill-buffer buffer)))))

(defun ygg-db--sentinel (process _event)
  (when (memq (process-status process) '(exit signal))
    (let* ((run (process-get process 'ygg-db-run))
           (conn (ygg-db-run-connection run))
           (key (ygg-db--key conn))
           (buffer (process-buffer process))
           (stdout "")
           (stderr ""))
      (unwind-protect
          (progn
            (unwind-protect
                (setq stdout (if (buffer-live-p buffer)
                                 (with-current-buffer buffer (buffer-string))
                               "")
                      stderr (ygg-db--drain (process-get process 'ygg-db-stderr)))
              (when (buffer-live-p buffer) (kill-buffer buffer))
              (ignore-errors (delete-directory (ygg-db-run-directory run) t))
              (remhash key ygg-db--running))
            (let ((result (funcall (if (ygg-db--redis-p conn) #'ygg-db--redis-result #'ygg-db--result)
                                   (ygg-db-run-statements run) stdout stderr
                                   (ygg-db-run-nonce run) (process-exit-status process))))
              (funcall (ygg-db-run-callback run)
                       (append (list :connection conn
                                     :elapsed (- (float-time) (ygg-db-run-started run))
                                     :owner (ygg-db-run-owner run)
                                     :statements (ygg-db-run-statements run)
                                     :interrupted (eq (process-status process) 'signal))
                               result))))
        (ygg-db--next key)))))

(defun ygg-db--fail (run failure)
  "Tell RUN's callback it could not start, FAILURE saying why."
  (ignore-errors
    (funcall (ygg-db-run-callback run)
             (list :connection (ygg-db-run-connection run) :elapsed 0
                   :owner (ygg-db-run-owner run) :statements (ygg-db-run-statements run)
                   :sets nil :error failure))))

(defun ygg-db--next (key)
  "Start the oldest run waiting on KEY.
One that cannot start gets an error result, and the next is tried."
  (let (started)
    (while-let (((not started))
                (queue (gethash key ygg-db--queued)))
      (if (cdr queue) (puthash key (cdr queue) ygg-db--queued) (remhash key ygg-db--queued))
      (condition-case failure
          (progn (ygg-db--start (car queue)) (setq started t))
        ((error quit)
         (ygg-db--fail (car queue) (if (eq (car failure) 'quit) "quit"
                                     (error-message-string failure))))))))

(defun ygg-db--owner-runs (owner)
  (let (runs)
    (maphash (lambda (_ run) (when (eq (ygg-db-run-owner run) owner) (push run runs)))
             ygg-db--running)
    runs))

(defun ygg-db--drop-queued (owner)
  (maphash (lambda (key queue)
             (puthash key (cl-remove owner queue :key #'ygg-db-run-owner) ygg-db--queued))
           ygg-db--queued))

(defun ygg-db--stop (owner)
  "Kill OWNER's running queries and drop its waiting ones; non-nil if any ran."
  (ygg-db--drop-queued owner)
  (let ((runs (ygg-db--owner-runs owner)))
    (dolist (run runs)
      (when (process-live-p (ygg-db-run-process run))
        (kill-process (ygg-db-run-process run))))
    runs))

;;; Rendering

(defun ygg-db--cell-text (value)
  (if (null value)
      (propertize "NULL" 'face 'ygg-db-null)
    (let ((flat (replace-regexp-in-string "\t" " " (replace-regexp-in-string "\r?\n" "↵" value))))
      (truncate-string-to-width flat ygg-db-cell-max-width nil nil "…"))))

(defun ygg-db--numeric-column-p (rows index)
  (let ((values (delq nil (mapcar (lambda (row) (nth index row)) rows))))
    (and values
         (cl-every (lambda (v) (string-match-p "\\`[-+]?[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\'" v))
                   values))))

(defun ygg-db--pad (text width right)
  (let ((gap (make-string (max 0 (- width (string-width text))) ?\s)))
    (if right (concat gap text) (concat text gap))))

(defun ygg-db-render-table (columns rows)
  "COLUMNS over ROWS as aligned text with thin rules."
  (let* ((cells (mapcar (lambda (row) (mapcar #'ygg-db--cell-text row)) rows))
         (heads (mapcar (lambda (c) (ygg-db--cell-text (or c ""))) columns))
         (count (length columns))
         (widths (cl-loop for i below count
                          collect (apply #'max (string-width (nth i heads))
                                         (mapcar (lambda (row) (string-width (or (nth i row) "")))
                                                 cells))))
         (right (cl-loop for i below count collect (ygg-db--numeric-column-p rows i)))
         (bar (propertize " │ " 'face 'ygg-db-rule))
         (line (lambda (texts)
                 (concat " " (string-join (cl-loop for text in texts
                                                   for width in widths
                                                   for r in right
                                                   collect (ygg-db--pad (or text "") width r))
                                          bar)
                         "\n"))))
    (concat
     (funcall line (mapcar (lambda (h) (propertize h 'face 'ygg-db-header)) heads))
     (propertize (concat "─" (mapconcat (lambda (w) (make-string w ?─)) widths "─┼─") "─\n")
                 'face 'ygg-db-rule)
     (mapconcat line cells ""))))

(defun ygg-db--duration (seconds)
  (if (< seconds 1) (format "%d ms" (round (* 1000 seconds))) (format "%.2f s" seconds)))

(defun ygg-db--failure-text (text)
  (concat (propertize "✗ " 'face 'ygg-db-failure) text "\n"))

(defun ygg-db--first-line (statement)
  (let ((line (car (split-string statement "\n"))))
    (truncate-string-to-width line 72 nil nil "…")))

(defun ygg-db--render-set (set many)
  (concat
   (when many (propertize (concat "› " (ygg-db--first-line (plist-get set :statement)) "\n")
                          'face 'ygg-db-meta))
   (when-let* ((failure (plist-get set :error))) (ygg-db--failure-text failure))
   (cond
    ((plist-get set :columns)
     (let* ((total (plist-get set :total))
            (rows (plist-get set :rows))
            (shown (seq-take rows ygg-db-result-max-rows)))
       (concat (ygg-db-render-table (plist-get set :columns) shown)
               (propertize (concat (format " %d row%s" total (if (= total 1) "" "s"))
                                   (when (< (length shown) total)
                                     (format " · showing %d · \\d opens all in VisiData"
                                             (length shown))))
                           'face 'ygg-db-meta)
               "\n")))
    ((plist-get set :text) (concat (plist-get set :text) "\n"))
    ((plist-get set :status) (propertize (concat " " (plist-get set :status) "\n") 'face 'ygg-db-meta)))))

(defun ygg-db-render-result (result)
  "RESULT as the text of the result window."
  (let* ((sets (plist-get result :sets))
         (many (cdr sets)))
    (concat
     (when-let* ((failure (plist-get result :error))) (ygg-db--failure-text failure))
     (when (plist-get result :interrupted) (ygg-db--failure-text "interrupted"))
     (mapconcat (lambda (set) (ygg-db--render-set set many)) sets "\n")
     (propertize (format " %s%s\n" (ygg-db--duration (plist-get result :elapsed))
                         (let ((exit (plist-get result :exit)))
                           (if (and exit (/= exit 0) (not (plist-get result :interrupted)))
                               (format " · exit %d" exit) "")))
                 'face 'ygg-db-meta))))

;;; Result window

(defconst ygg-db-result-buffer "*db result*")

(defvar-local ygg-db--shown-result nil
  "The result plist this buffer shows or last received.")

(defvar-keymap ygg-db-result-mode-map
  :doc "Keys of the result window."
  "q" #'quit-window)

(define-derived-mode ygg-db-result-mode special-mode "DB result"
  "Rows a query returned."
  (setq-local truncate-lines t)
  (setq-local header-line-format '(:eval (ygg-db--result-header))))

(defun ygg-db--result-header ()
  (when-let* ((conn (plist-get ygg-db--shown-result :connection)))
    (propertize (format " %s  %s" (plist-get conn :name) (ygg-db--family (plist-get conn :url)))
                'face 'ygg-db-meta)))

(defun ygg-db--result-window (buffer)
  (display-buffer buffer
                  `((display-buffer-reuse-window display-buffer-in-side-window)
                    (side . bottom) (slot . 0) (window-height . ,ygg-db-result-height)
                    (window-parameters . ((no-delete-other-windows . t))))))

(defun ygg-db--show (conn text result)
  (let ((buffer (get-buffer-create ygg-db-result-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ygg-db-result-mode) (ygg-db-result-mode))
      (setq ygg-db--shown-result (or result (list :connection conn)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))))
    (let ((window (ygg-db--result-window buffer)))
      (when (window-live-p window) (set-window-point window 1)))
    buffer))

(defun ygg-db--deliver (result)
  (let ((owner (plist-get result :owner)))
    (when (buffer-live-p owner)
      (with-current-buffer owner
        (setq ygg-db--shown-result result)
        (force-mode-line-update)))
    (ygg-db--show (plist-get result :connection) (ygg-db-render-result result) result)))

(defun ygg-db-query-connection (conn statements &optional owner)
  "Run STATEMENTS on CONN and show what they return in the result window."
  (when (ygg-db--redis-p conn) (ygg-db--redis-confirm statements))
  (ygg-db--show conn (propertize (format " running on %s · \\i interrupts\n" (plist-get conn :name))
                                 'face 'ygg-db-meta)
                nil)
  (prog1 (ygg-db-run conn statements #'ygg-db--deliver owner)
    (unless (ygg-db--schema conn) (ygg-db-load-schema conn))
    (when (buffer-live-p owner)
      (with-current-buffer owner (force-mode-line-update)))))

;;; Schema

(defvar ygg-db--schemas (make-hash-table :test #'equal)
  "Connection URL to (:state loading|ready|failed :tables TABLES :error TEXT).
TABLES are (SCHEMA TABLE COLUMNS).")

(defun ygg-db--catalog-query (family)
  (pcase family
    ("sqlite" "select 'main' as table_schema, m.name as table_name, p.name as column_name from sqlite_master m join pragma_table_info(m.name) p where m.type in ('table', 'view') and m.name not like 'sqlite_%' order by m.name, p.cid")
    ("duckdb" "select table_schema, table_name, column_name from information_schema.columns where table_catalog = current_database() order by table_schema, table_name, ordinal_position")
    ("postgres" "select table_schema, table_name, column_name from information_schema.columns where table_schema not in ('pg_catalog', 'information_schema') and table_schema not like 'pg_toast%' order by table_schema, table_name, ordinal_position")
    ("mysql" "select table_schema, table_name, column_name from information_schema.columns where table_schema = database() order by table_schema, table_name, ordinal_position")
    ("clickhouse" "select database, table, name from system.columns where database = currentDatabase() order by database, table, position")
    ("oracle" "select owner, table_name, column_name from all_tab_columns where owner = user order by owner, table_name, column_id")
    ((or "snowflake" "trino") "select table_schema, table_name, column_name from information_schema.columns where lower(table_schema) <> 'information_schema' order by table_schema, table_name, ordinal_position")
    (_ "select table_schema, table_name, column_name from information_schema.columns order by table_schema, table_name, ordinal_position")))

(defun ygg-db--tables-from-rows (rows)
  "ROWS of (SCHEMA TABLE COLUMN) folded into (SCHEMA TABLE COLUMNS)."
  (let (tables)
    (dolist (row rows)
      (pcase-let ((`(,schema ,table ,column) row))
        (if (and tables (equal (car (car tables)) schema) (equal (cadr (car tables)) table))
            (push column (nth 2 (car tables)))
          (push (list schema table (list column)) tables))))
    (nreverse (mapcar (lambda (entry) (list (car entry) (cadr entry) (nreverse (nth 2 entry))))
                      tables))))

(defun ygg-db--catalog-entry (result)
  "The schema entry the catalog query's RESULT describes."
  (let* ((set (car (plist-get result :sets)))
         (failure (or (plist-get result :error) (plist-get set :error))))
    (condition-case parse-failure
        (if (or failure (not (plist-get set :columns)))
            (list :state 'failed :error (or failure "no catalog rows"))
          (list :state 'ready
                :tables (ygg-db--tables-from-rows
                         (cdr (car (ygg-db-parse-csv (plist-get set :csv) most-positive-fixnum))))))
      (error (list :state 'failed :error (error-message-string parse-failure))))))

(defun ygg-db--schema (conn)
  (gethash (ygg-db--key conn) ygg-db--schemas))

(defun ygg-db-load-schema (conn &optional force callback)
  "Read CONN's tables and columns in the background, once unless FORCE.
CALLBACK runs with the schema entry when it is in."
  (let ((key (ygg-db--key conn))
        (entry (ygg-db--schema conn)))
    (if (and entry (not force))
        (when callback (funcall callback entry))
      (puthash key (list :state 'loading) ygg-db--schemas)
      (ygg-db--drawer-redraw)
      (let ((finish (lambda (entry)
                      (puthash key entry ygg-db--schemas)
                      (ygg-db--drawer-redraw)
                      (when callback (funcall callback entry))))
            (queued nil))
        (unwind-protect
            (progn
              (if (ygg-db--redis-p conn)
                  (ygg-db--redis-sample conn finish)
                (ygg-db-run conn (list (ygg-db--catalog-query (ygg-db--family (plist-get conn :url))))
                            (lambda (result) (funcall finish (ygg-db--catalog-entry result)))))
              (setq queued t))
          (unless queued
            (puthash key (list :state 'failed :error "could not start the catalog query")
                     ygg-db--schemas)
            (ygg-db--drawer-redraw)))))))

;;; Table helpers

(defun ygg-db--identifier (name family)
  (let ((case-fold-search nil))
    (if (string-match-p (pcase family
                          ("postgres" "\\`[a-z_][a-z0-9_]*\\'")
                          ("oracle" "\\`[A-Z][A-Z0-9_]*\\'")
                          (_ "\\`[A-Za-z_][A-Za-z0-9_]*\\'"))
                        name)
        name
      (pcase family
        ((or "mysql" "clickhouse") (concat "`" (string-replace "`" "``" name) "`"))
        ("sqlserver" (concat "[" (string-replace "]" "]]" name) "]"))
        (_ (concat "\"" (string-replace "\"" "\"\"" name) "\""))))))

(defun ygg-db--literal (text)
  (concat "'" (string-replace "'" "''" (or text "")) "'"))

(defun ygg-db--table-ref (family schema table)
  (let ((default (cdr (assoc family ygg-db--default-schemas))))
    (if (and schema (member family '("postgres" "duckdb" "sqlserver" "snowflake" "trino"))
             (not (equal schema default)))
        (concat (ygg-db--identifier schema family) "." (ygg-db--identifier table family))
      (ygg-db--identifier table family))))

(defun ygg-db-helper-query (helper family schema table)
  "The SQL of HELPER (list, count, describe or indexes) on SCHEMA.TABLE, or nil."
  (let ((ref (ygg-db--table-ref family schema table))
        (s (ygg-db--literal schema))
        (tl (ygg-db--literal table)))
    (pcase helper
      ('list (pcase family
               ("sqlserver" (format "select top %d * from %s" ygg-db-table-limit ref))
               ("oracle" (format "select * from %s fetch first %d rows only" ref ygg-db-table-limit))
               (_ (format "select * from %s limit %d" ref ygg-db-table-limit))))
      ('count (format "select count(*) from %s" ref))
      ('describe
       (pcase family
         ("sqlite" (format "select name, type, \"notnull\", dflt_value, pk from pragma_table_info(%s)" tl))
         ("duckdb" (format "describe %s" ref))
         ("clickhouse" (format "describe table %s" ref))
         ("mysql" (format "show columns from %s" ref))
         ("oracle" (format "select column_name, data_type, nullable, data_default from all_tab_columns where owner = user and table_name = %s order by column_id" tl))
         (_ (format "select column_name, data_type, is_nullable, column_default from information_schema.columns where table_schema = %s and table_name = %s order by ordinal_position" s tl))))
      ('indexes
       (pcase family
         ("sqlite" (format "select name, sql from sqlite_master where type = 'index' and tbl_name = %s" tl))
         ("duckdb" (format "select index_name, is_unique, sql from duckdb_indexes() where schema_name = %s and table_name = %s" s tl))
         ("postgres" (format "select indexname, indexdef from pg_indexes where schemaname = %s and tablename = %s" s tl))
         ("mysql" (format "show index from %s" ref))
         ("sqlserver" (format "select name, type_desc, is_unique from sys.indexes where object_id = object_id(%s)" (ygg-db--literal (concat schema "." table))))
         ("oracle" (format "select index_name, uniqueness from all_indexes where table_owner = user and table_name = %s" tl)))))))

;;; Query buffers

(defvar-local ygg-db-connection nil
  "The connection plist this buffer's queries run on.")

(defvar-local ygg-db-connection-name nil
  "Name of the connection a saved query file binds to, as a file-local.")
(put 'ygg-db-connection-name 'safe-local-variable #'stringp)

(defun ygg-db--running-here-p ()
  (ygg-db--owner-runs (current-buffer)))

(defun ygg-db--query-header ()
  (when ygg-db-connection
    (propertize (format " %s  %s%s" (plist-get ygg-db-connection :name)
                        (ygg-db--family (plist-get ygg-db-connection :url))
                        (if (ygg-db--running-here-p) "  running" ""))
                'face 'ygg-db-meta)))

(define-minor-mode ygg-db-query-mode
  "Run this sql buffer's statements on a database connection."
  :lighter nil
  (if ygg-db-query-mode
      (progn
        (setq-local header-line-format '(:eval (ygg-db--query-header)))
        (add-hook 'completion-at-point-functions #'ygg-db-completion-at-point nil t)
        (add-hook 'kill-buffer-hook #'ygg-db--forget-buffer nil t))
    (kill-local-variable 'header-line-format)
    (remove-hook 'completion-at-point-functions #'ygg-db-completion-at-point t)))

(defun ygg-db--forget-buffer ()
  (ygg-db--stop (current-buffer)))

(defun ygg-db--attach (conn)
  "Make this buffer's queries run on CONN without reaching the database."
  (setq ygg-db-connection conn)
  (let ((product (ygg-db--sql-product (ygg-db--family (plist-get conn :url)))))
    (unless (or (not (derived-mode-p 'sql-mode)) (ygg-db--redis-p conn)
                (eq product (bound-and-true-p sql-product)))
      (ignore-errors (sql-set-product product))))
  (ygg-db-query-mode 1))

(defun ygg-db-bind (conn)
  "Make this buffer's queries run on CONN, and read its schema."
  (ygg-db--attach conn)
  (ygg-db-load-schema conn))

(defun ygg-db--query-buffer-p ()
  (derived-mode-p 'sql-mode 'ygg-db-redis-mode))

(defun ygg-db--scratch-buffer (conn)
  (let ((buffer (get-buffer-create (format "*db: %s*" (plist-get conn :name))))
        (mode (if (ygg-db--redis-p conn) 'ygg-db-redis-mode 'sql-mode)))
    (with-current-buffer buffer
      (unless (derived-mode-p mode)
        (setq default-directory (ygg-db--local-directory conn))
        (funcall mode))
      (unless (equal (plist-get ygg-db-connection :url) (plist-get conn :url))
        (ygg-db-bind conn)))
    buffer))

(defun ygg-db--main-window ()
  (or (cl-find-if-not (lambda (window) (window-parameter window 'window-side))
                      (delq nil (cons (get-mru-window nil nil t) (window-list nil 'never))))
      (selected-window)))

(defun ygg-db--visit (buffer)
  (select-window (ygg-db--main-window))
  (switch-to-buffer buffer))

(defun ygg-db--queries-directory (conn)
  (let ((project-dir (and (eq (plist-get conn :source) 'project)
                          (ygg-db--project-dir (plist-get conn :root))))
        (name (replace-regexp-in-string "[/\\:*?\"<>|]" "_" (plist-get conn :name))))
    (file-name-as-directory
     (expand-file-name name (expand-file-name "queries" (or project-dir ygg-db-directory))))))

(defun ygg-db--saved-queries (conn)
  (let ((dir (ygg-db--queries-directory conn)))
    (and (file-directory-p dir) (directory-files dir t "\\.\\(?:sql\\|redis\\)\\'"))))

(defun ygg-db--bind-visited-file ()
  (when (and (ygg-db--query-buffer-p) (not ygg-db-query-mode) buffer-file-name)
    (let* ((dir (file-name-directory buffer-file-name))
           (name (or ygg-db-connection-name
                     (and (string-match-p "/queries/[^/]+/\\'" dir)
                          (file-name-nondirectory (directory-file-name dir)))))
           (conn (and name (ygg-db--find-connection name))))
      (when conn (ygg-db--attach conn)))))

(add-hook 'hack-local-variables-hook #'ygg-db--bind-visited-file)

(defun ygg-db--connection-here ()
  (or ygg-db-connection
      (plist-get ygg-db--shown-result :connection)
      (let ((conn (ygg-db--read-connection)))
        (if (ygg-db--query-buffer-p)
            (ygg-db-bind conn)
          (ygg-db--visit (ygg-db--scratch-buffer conn)))
        conn)))

(defun ygg-db--selection-p ()
  (and (use-region-p)
       (or (bound-and-true-p ygg--visual-p) (not (bound-and-true-p ygg--state)))))

;;;###autoload
(defun ygg-db-execute ()
  "Run the selection, else the statement at point, on this buffer's connection."
  (interactive)
  (let* ((conn (ygg-db--connection-here))
         (statements (if (ygg-db--selection-p)
                         (ygg-db--statements (region-beginning) (region-end))
                       (let ((bound (ygg-db--statement-at (point))))
                         (and bound (ygg-db--statements (car bound) (cdr bound)))))))
    (unless statements (user-error "No statement here"))
    (ygg-db-query-connection conn statements (current-buffer))))

;;;###autoload
(defun ygg-db-execute-buffer ()
  "Run every statement in the buffer on its connection."
  (interactive)
  (let ((conn (ygg-db--connection-here))
        (statements (ygg-db--statements (point-min) (point-max))))
    (unless statements (user-error "No statements in the buffer"))
    (ygg-db-query-connection conn statements (current-buffer))))

(defun ygg-db-interrupt ()
  "Kill the query this buffer is running, and drop the ones it queued."
  (interactive)
  (let ((owner (if (derived-mode-p 'ygg-db-result-mode)
                   (plist-get ygg-db--shown-result :owner)
                 (current-buffer))))
    (unless (ygg-db--stop owner) (message "No query running here"))))

(defun ygg-db-refresh-schema ()
  "Read this buffer's connection's tables and columns again."
  (interactive)
  (let ((conn (ygg-db--connection-here)))
    (ygg-db-load-schema conn t
                        (lambda (entry)
                          (message "%s: %s" (plist-get conn :name)
                                   (if (eq (plist-get entry :state) 'ready)
                                       (format "%d tables" (length (plist-get entry :tables)))
                                     (plist-get entry :error)))))))

(defun ygg-db-save-query (name)
  "Save this buffer as the query NAME under its connection."
  (interactive
   (list (unless buffer-file-name
           (read-string "Save query as: "))))
  (let ((conn (ygg-db--connection-here)))
    (if buffer-file-name
        (save-buffer)
      (let* ((dir (ygg-db--queries-directory conn))
             (file (expand-file-name (concat (file-name-sans-extension name)
                                             (if (ygg-db--redis-p conn) ".redis" ".sql"))
                                     dir)))
        (make-directory dir t)
        (when (and (file-exists-p file) (not (y-or-n-p (format "%s exists; replace? " file))))
          (user-error "Not saved"))
        (write-region (point-min) (point-max) file)
        (find-file file)
        (unless ygg-db-connection (ygg-db-bind conn))))
    (ygg-db--drawer-redraw)))

;;; VisiData

(defun ygg-db-visidata ()
  "Send the last result, every row of it, to VisiData."
  (interactive)
  (unless (fboundp 'ygg-visidata-open-file)
    (user-error "VisiData is not loaded (ygg-visidata)"))
  (let* ((result (or ygg-db--shown-result
                     (buffer-local-value 'ygg-db--shown-result
                                         (or (get-buffer ygg-db-result-buffer)
                                             (user-error "No result yet")))))
         (set (or (car (last (cl-remove-if-not (lambda (s) (plist-get s :csv))
                                               (plist-get result :sets))))
                  (user-error "The last result has no rows")))
         (directory (ygg-db--private-directory))
         (file (expand-file-name
                (concat (replace-regexp-in-string
                         "[^A-Za-z0-9_.-]" "_"
                         (plist-get (plist-get result :connection) :name))
                        ".csv")
                directory))
         (opened nil))
    (unwind-protect
        (progn
          (ygg-db--write-private
           file (replace-regexp-in-string (regexp-quote ygg-db--null) "" (plist-get set :csv) t t))
          (let ((buffer (ygg-visidata-open-file file)))
            (setq opened t)
            (if (and (bufferp buffer) (fboundp 'ygg-visidata-remove-with-buffer))
                (ygg-visidata-remove-with-buffer buffer directory)
              buffer)))
      (unless opened (delete-directory directory t)))))

;;; Picking a connection

(defun ygg-db--source-label (source)
  (pcase source ('project "project") ('saved "added") ('config "config") ('detected "found")))

(defun ygg-db--read-connection (&optional prompt)
  "Ask for a connection with PROMPT; return it."
  (let* ((conns (or (ygg-db-connections-here)
                    (user-error "No connections; add one with a in the drawer")))
         (names (mapcar (lambda (c) (plist-get c :name)) conns))
         (column (+ 4 (apply #'max (mapcar #'string-width names))))
         (lookup (lambda (name) (cl-find name conns :key (lambda (c) (plist-get c :name)) :test #'equal)))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (category . ygg-db-connection)
                        (display-sort-function . identity)
                        (group-function
                         . ,(lambda (name transform)
                              (if transform name
                                (propertize (ygg-db--source-label
                                             (plist-get (funcall lookup name) :source))
                                            'face 'ygg-db-meta))))
                        (affixation-function
                         . ,(lambda (strings)
                              (mapcar (lambda (name)
                                        (let ((conn (funcall lookup name)))
                                          (list name ""
                                                (concat (propertize " " 'display `(space :align-to ,column))
                                                        (propertize (format "%s  %s"
                                                                            (ygg-db--family (plist-get conn :url))
                                                                            (ygg-db--redact (plist-get conn :url)))
                                                                    'face 'ygg-db-meta)))))
                                      strings))))
                    (complete-with-action action names string pred)))))
    (minibuffer-with-setup-hook
        (lambda () (setq-local vertico-group-format "%s"))
      (funcall lookup (completing-read (or prompt "Connection: ") table nil t nil nil
                                       (plist-get ygg-db-connection :name))))))

;;;###autoload
(defun ygg-db-pick-connection ()
  "Choose the connection this sql buffer runs on; elsewhere open its query buffer."
  (interactive)
  (let ((conn (ygg-db--read-connection)))
    (if (ygg-db--query-buffer-p)
        (ygg-db-bind conn)
      (ygg-db--visit (ygg-db--scratch-buffer conn)))))

;;;###autoload
(defun ygg-db-query (conn)
  "Open the query buffer of CONN."
  (interactive (list (ygg-db--read-connection)))
  (ygg-db--visit (ygg-db--scratch-buffer conn)))

;;;###autoload
(defun ygg-db-add-connection (name url)
  "Add a connection NAME at URL, kept in ygg-db-directory.
Passwords belong in auth-source, so a URL carrying one is refused."
  (interactive (list (read-string "Connection name: ")
                     (read-string "URL (no password): ")))
  (when (or (string-blank-p name) (not (ygg-db--scheme url)))
    (user-error "A connection needs a name and a URL such as pg://user@host/db"))
  (unless (ygg-db--file-based-p url)
    (when (url-password (url-generic-parse-url url))
      (user-error "Leave the password out; add it to auth-source for %s"
                  (url-host (url-generic-parse-url url)))))
  (let* ((file (ygg-db--saved-file))
         (entries (cl-remove name (ygg-db--read-alist file) :key #'car :test #'equal)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (let ((print-length nil) (print-level nil))
        (pp (append entries (list (cons name url))) (current-buffer))))
    (ygg-db--drawer-redraw)
    name))

;;; Completion

(defconst ygg-db--not-aliases
  '("where" "on" "join" "left" "right" "inner" "outer" "full" "cross" "natural" "group"
    "order" "limit" "using" "set" "values" "select" "union" "having" "window" "as" "and" "or")
  "Words after a table name that are not its alias.")

(defun ygg-db--unquote (identifier)
  (replace-regexp-in-string "[\"`]\\|\\[\\|\\]" "" identifier))

(defun ygg-db--referenced-tables (text)
  "The (TABLE . ALIAS) pairs a statement's FROM, JOIN, UPDATE and INTO name."
  (let ((case-fold-search t) (start 0) found)
    (while (string-match
            "\\_<\\(?:from\\|join\\|update\\|into\\)[ \t\n]+\\([][A-Za-z0-9_.\"`]+\\)\\(?:[ \t\n]+\\(?:as[ \t\n]+\\)?\\([A-Za-z_][A-Za-z0-9_]*\\)\\)?"
            text start)
      (let* ((table (ygg-db--unquote (match-string 1 text)))
             (alias (match-string 2 text)))
        (push (cons (car (last (split-string table "\\.")))
                    (and alias (not (member (downcase alias) ygg-db--not-aliases)) alias))
              found))
      (setq start (match-end 1)))
    (nreverse found)))

(defun ygg-db--tables-named (tables name)
  (cl-remove-if-not (lambda (entry) (string-equal-ignore-case (nth 1 entry) name)) tables))

(defun ygg-db-completion-candidates (tables statement qualifier)
  "Completions from TABLES for STATEMENT, after QUALIFIER and a dot if given."
  (let* ((refs (ygg-db--referenced-tables statement))
         (table-of (lambda (name)
                     (or (ygg-db--tables-named tables name)
                         (when-let* ((ref (cl-find name refs :key #'cdr
                                                   :test (lambda (a b) (and b (string-equal-ignore-case a b))))))
                           (ygg-db--tables-named tables (car ref))))))
         (seen (make-hash-table :test #'equal))
         (out nil)
         (add (lambda (name kind table)
                (unless (gethash name seen)
                  (puthash name t seen)
                  (push (propertize name 'ygg-db-kind kind 'ygg-db-table table) out)))))
    (if qualifier
        (let ((in-schema (cl-remove-if-not (lambda (e) (string-equal-ignore-case (or (car e) "") qualifier))
                                           tables)))
          (dolist (entry (funcall table-of qualifier))
            (dolist (column (nth 2 entry)) (funcall add column 'column (nth 1 entry))))
          (dolist (entry in-schema) (funcall add (nth 1 entry) 'table nil)))
      (let ((scope (or (cl-mapcan (lambda (ref) (copy-sequence (ygg-db--tables-named tables (car ref)))) refs)
                       tables)))
        (dolist (entry scope)
          (dolist (column (nth 2 entry)) (funcall add column 'column (nth 1 entry))))
        (dolist (entry tables) (funcall add (nth 1 entry) 'table nil))))
    (nreverse out)))

(defun ygg-db--annotate (candidate)
  (let ((table (get-text-property 0 'ygg-db-table candidate)))
    (if (eq (get-text-property 0 'ygg-db-kind candidate) 'table)
        " table"
      (concat " " table))))

(defun ygg-db-completion-at-point ()
  "Tables and columns of this buffer's connection, from its cached schema.
On Redis, command names and keys."
  (if (and ygg-db-connection (ygg-db--redis-p ygg-db-connection))
      (ygg-db--redis-completion-at-point)
    (ygg-db--sql-completion-at-point)))

(defun ygg-db--sql-completion-at-point ()
  (when-let* ((conn ygg-db-connection)
              ((not (nth 8 (syntax-ppss)))))
    (let ((entry (ygg-db--schema conn)))
      (if (not (eq (plist-get entry :state) 'ready))
          (progn (unless entry (ygg-db-load-schema conn)) nil)
        (let* ((end (point))
               (start (save-excursion (skip-chars-backward "A-Za-z0-9_$") (point)))
               (qualifier (save-excursion
                            (goto-char start)
                            (when (eq (char-before) ?.)
                              (let ((dot (1- (point))))
                                (skip-chars-backward "A-Za-z0-9_$\"`")
                                (ygg-db--unquote (buffer-substring-no-properties (point) dot))))))
               (bound (ygg-db--statement-at start))
               (statement (if bound
                              (buffer-substring-no-properties (car bound) (max (cdr bound) end))
                            ""))
               (candidates (ygg-db-completion-candidates (plist-get entry :tables) statement
                                                         (and (not (string-empty-p (or qualifier "")))
                                                              qualifier))))
          (list start end candidates
                :exclusive 'no
                :annotation-function #'ygg-db--annotate
                :company-kind (lambda (c) (if (eq (get-text-property 0 'ygg-db-kind c) 'table)
                                              'module 'field))))))))

;;; Redis

(defcustom ygg-db-redis-program "redis-cli"
  "The redis-cli executable; valkey-cli is tried when it is missing."
  :type 'string)

(defcustom ygg-db-redis-sample-count 1000
  "Keys one SCAN is asked for when the drawer samples a Redis database."
  :type 'natnum)

(defcustom ygg-db-redis-scan-count 1000
  "COUNT each SCAN of key completion asks for."
  :type 'natnum)

(defcustom ygg-db-redis-completion-rounds 10
  "Most SCAN calls one key completion makes."
  :type 'natnum)

(defcustom ygg-db-redis-completion-limit 500
  "Most keys one completion collects."
  :type 'natnum)

(defcustom ygg-db-redis-timeout 5
  "Seconds key completion waits for Redis before giving up."
  :type 'number)

(defcustom ygg-db-redis-dangerous-commands
  '(("FLUSHALL") ("FLUSHDB") ("KEYS") ("DEBUG") ("SHUTDOWN") ("CONFIG" "SET"))
  "Leading words of Redis commands that ask before they run."
  :type '(repeat (repeat string)))

(defconst ygg-db--redis-commands
  '("APPEND" "BITCOUNT" "BLPOP" "BRPOP" "CLIENT" "CONFIG" "COPY" "DBSIZE" "DECR" "DECRBY" "DEL"
    "DUMP" "ECHO" "EVAL" "EVALSHA" "EXISTS" "EXPIRE" "EXPIREAT" "EXPIRETIME" "FLUSHALL" "FLUSHDB"
    "GET" "GETDEL" "GETEX" "GETRANGE" "GETSET" "HDEL" "HEXISTS" "HGET" "HGETALL" "HINCRBY" "HKEYS"
    "HLEN" "HMGET" "HSCAN" "HSET" "HSETNX" "HSTRLEN" "HVALS" "INCR" "INCRBY" "INCRBYFLOAT" "INFO"
    "LINDEX" "LINSERT" "LLEN" "LMOVE" "LPOP" "LPOS" "LPUSH" "LRANGE" "LREM" "LSET" "LTRIM" "MEMORY"
    "MGET" "MSET" "OBJECT" "PERSIST" "PEXPIRE" "PFADD" "PFCOUNT" "PING" "PTTL" "PUBLISH" "RANDOMKEY"
    "RENAME" "RENAMENX" "RPOP" "RPUSH" "SADD" "SCAN" "SCARD" "SDIFF" "SET" "SETEX" "SETNX" "SINTER"
    "SISMEMBER" "SMEMBERS" "SMOVE" "SPOP" "SRANDMEMBER" "SREM" "SSCAN" "STRLEN" "SUNION" "TIME"
    "TOUCH" "TTL" "TYPE" "UNLINK" "XADD" "XDEL" "XINFO" "XLEN" "XRANGE" "XREVRANGE" "XTRIM" "ZADD"
    "ZCARD" "ZCOUNT" "ZINCRBY" "ZRANGE" "ZRANGEBYSCORE" "ZRANK" "ZREM" "ZREVRANGE" "ZSCAN" "ZSCORE")
  "Command names completed at the start of a line.")

(defvar ygg-db-redis-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    table))

(define-derived-mode ygg-db-redis-mode prog-mode "Redis"
  "Redis commands, one a line, run through ygg-db."
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "^[ \t]*#+[ \t]*")
  (setq-local font-lock-defaults
              '((("^[ \t]*#.*$" . font-lock-comment-face)
                 ("^[ \t]*\\([A-Za-z]+\\)" 1 font-lock-keyword-face))
                t t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.redis\\'" . ygg-db-redis-mode))

(defun ygg-db-redis-split (line)
  "The words redis-cli reads from LINE, or nil when a quote does not close.
Double quotes take backslash escapes and \\xHH; single quotes take only \\'."
  (let ((i 0) (n (length line)) (words nil) (ok t))
    (while (and ok (progn (while (and (< i n) (memq (aref line i) '(?\s ?\t ?\n ?\r))) (cl-incf i))
                          (< i n)))
      (let ((chars nil) (done nil))
        (while (and ok (not done))
          (let ((c (and (< i n) (aref line i))))
            (cond
             ((or (null c) (memq c '(?\s ?\t ?\n ?\r))) (setq done t))
             ((memq c '(?\" ?'))
              (cl-incf i)
              (let ((closed nil))
                (while (and (not closed) (< i n))
                  (let ((d (aref line i)))
                    (cond
                     ((eq d c) (setq closed t) (cl-incf i))
                     ((and (eq c ?\") (eq d ?\\) (< (1+ i) n)
                           (eq (aref line (1+ i)) ?x) (< (+ i 3) n)
                           (string-match-p "\\`[0-9a-fA-F]\\{2\\}\\'" (substring line (+ i 2) (+ i 4))))
                      (let ((byte (string-to-number (substring line (+ i 2) (+ i 4)) 16)))
                        (push (if (< byte 128) byte (unibyte-char-to-multibyte byte)) chars))
                      (cl-incf i 4))
                     ((and (eq c ?\") (eq d ?\\) (< (1+ i) n))
                      (push (pcase (aref line (1+ i))
                              (?n ?\n) (?r ?\r) (?t ?\t) (?b ?\b) (?a ?\a) (e e))
                            chars)
                      (cl-incf i 2))
                     ((and (eq c ?') (eq d ?\\) (< (1+ i) n) (eq (aref line (1+ i)) ?'))
                      (push ?' chars)
                      (cl-incf i 2))
                     (t (push d chars) (cl-incf i)))))
                (unless (and closed (or (>= i n) (memq (aref line i) '(?\s ?\t ?\n ?\r))))
                  (setq ok nil))))
             (t (push c chars) (cl-incf i)))))
        (push (apply #'string (nreverse chars)) words)))
    (and ok (nreverse words))))

(defun ygg-db--redis-quote (text)
  "TEXT as one double-quoted redis-cli word.
Any byte that is not printable ASCII is escaped."
  (concat "\""
          (mapconcat (lambda (byte)
                       (cond ((memq byte '(?\\ ?\")) (string ?\\ byte))
                             ((<= 32 byte 126) (string byte))
                             (t (format "\\x%02x" byte))))
                     (encode-coding-string text 'utf-8) "")
          "\""))

(defun ygg-db--redis-glob-quote (text)
  "TEXT with the glob characters of a MATCH pattern escaped."
  (replace-regexp-in-string "[][*?\\]" "\\\\\\&" text))

(defun ygg-db--redis-dangerous (statement)
  "The dangerous command STATEMENT starts with, else nil."
  (let ((words (mapcar #'upcase (or (ygg-db-redis-split statement) (split-string statement)))))
    (when (and words (string-match-p "\\`[0-9]+\\'" (car words)))
      (pop words))
    (cl-loop for danger in ygg-db-redis-dangerous-commands
             when (equal (seq-take words (length danger)) danger)
             return (string-join danger " "))))

(defun ygg-db--redis-confirm (statements)
  "Ask before STATEMENTS run anything in ygg-db-redis-dangerous-commands."
  (when-let* ((dangers (delete-dups (delq nil (mapcar #'ygg-db--redis-dangerous statements)))))
    (unless (yes-or-no-p (format "Run %s? " (string-join dangers ", ")))
      (user-error "Not run"))))

(defun ygg-db--redis-program ()
  (or (executable-find ygg-db-redis-program)
      (executable-find "valkey-cli")
      (user-error "redis-cli not found; brew install redis")))

(defun ygg-db--redis-command (url)
  "(ARGV . PREAMBLE) that reach URL.
The password goes only into PREAMBLE, the first lines sent on stdin."
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (user (let ((user (url-user parsed)))
                 (and user (not (string-empty-p user)) (url-unhex-string user))))
         (secret (if (url-password parsed)
                     (url-unhex-string (url-password parsed))
                   (and host (not (string-empty-p host)) (ygg-db--auth-secret parsed "redis"))))
         (db (string-trim (or (car (url-path-and-query parsed)) "") "/+" "/+")))
    (cons (append (list (ygg-db--redis-program)
                        "-h" (if (or (null host) (string-empty-p host)) "127.0.0.1" host)
                        "-p" (format "%s" (or (url-portspec parsed) 6379))
                        "-2" "--json")
                  (and (equal (ygg-db--scheme url) "rediss") (list "--tls")))
          (concat (when (and secret (not (string-empty-p secret)))
                    (concat "AUTH " (and user (concat (ygg-db--redis-quote user) " "))
                            (ygg-db--redis-quote secret) "\n"))
                  (when (string-match-p "\\`[0-9]+\\'" db) (concat "SELECT " db "\n"))
                  "HELLO 3\n"))))

(defun ygg-db--redis-script (statements nonce)
  (concat (mapconcat (lambda (statement)
                       (concat "ECHO " nonce "\n" (string-replace "\n" " " statement) "\n"))
                     statements "")
          "ECHO " nonce "\n"))

(defun ygg-db--redis-parse (line)
  "(ok . VALUE), (error . TEXT) or (raw . LINE) from LINE.
LINE is one reply redis-cli --json printed."
  (let ((json (lambda (text) (json-parse-string text :object-type 'alist :array-type 'array
                                               :null-object nil :false-object :false))))
    (condition-case nil
        (let ((text (apply #'string (mapcar (lambda (char) (if (>= char #x3fff80) #xfffd char)) line))))
          (if (string-prefix-p "error:" text)
              (cons 'error (funcall json (substring text 6)))
            (cons 'ok (funcall json text))))
      (error (cons 'raw line)))))

(defun ygg-db--redis-map-p (value)
  (and (consp value) (consp (car value)) (symbolp (caar value))))

(defun ygg-db--redis-scalar (value)
  (pcase value
    ('nil "nil") ('t "true") (:false "false")
    ((pred stringp) value)
    ((pred numberp) (number-to-string value))
    ((pred vectorp) (if (zerop (length value)) "(empty)"
                      (concat "[" (mapconcat #'ygg-db--redis-scalar value ", ") "]")))
    ((pred ygg-db--redis-map-p)
     (concat "{" (mapconcat (lambda (pair) (format "%s: %s" (car pair) (ygg-db--redis-scalar (cdr pair))))
                            value ", ")
             "}"))
    (_ (format "%S" value))))

(defun ygg-db--redis-hang (label lines)
  (let ((pad (make-string (string-width label) ?\s)))
    (cons (concat label (car lines)) (mapcar (lambda (line) (concat pad line)) (cdr lines)))))

(defun ygg-db--redis-tree (value)
  "Lines drawing VALUE as a numbered, indented tree."
  (cond
   ((and (vectorp value) (> (length value) 0))
    (cl-loop for item across value for i from 1
             nconc (ygg-db--redis-hang (format "%d) " i) (ygg-db--redis-tree item))))
   ((ygg-db--redis-map-p value)
    (cl-loop for (key . item) in value
             nconc (ygg-db--redis-hang (format "%s: " key) (ygg-db--redis-tree item))))
   (t (list (ygg-db--redis-scalar value)))))

(defun ygg-db--redis-value-set (statement value)
  (cond
   ((null value) (list :statement statement :status "nil" :value value))
   ((ygg-db--redis-map-p value)
    (list :statement statement :columns '("field" "value") :value value :total (length value)
          :rows (mapcar (lambda (pair) (list (symbol-name (car pair)) (ygg-db--redis-scalar (cdr pair))))
                        value)))
   ((and (vectorp value) (zerop (length value)))
    (list :statement statement :status "(empty)" :value value))
   ((and (vectorp value) (cl-notany (lambda (item) (or (vectorp item) (consp item))) value))
    (list :statement statement :columns '("#" "value") :value value :total (length value)
          :rows (cl-loop for item across value for i from 1
                         collect (list (number-to-string i) (and item (ygg-db--redis-scalar item))))))
   ((or (vectorp value) (consp value))
    (list :statement statement :text (string-join (ygg-db--redis-tree value) "\n") :value value))
   (t (list :statement statement :text (ygg-db--redis-scalar value) :value value))))

(defun ygg-db--redis-set (statement chunk)
  "What STATEMENT gave: its CHUNK of redis-cli --json output."
  (let ((replies (mapcar #'ygg-db--redis-parse (split-string (or chunk "") "\n" t))))
    (if (cdr replies)
        (list :statement statement
              :text (mapconcat (lambda (reply)
                                 (if (eq (car reply) 'ok)
                                     (string-join (ygg-db--redis-tree (cdr reply)) "\n")
                                   (format "%s" (cdr reply))))
                               replies "\n"))
      (pcase (car replies)
        ('nil (list :statement statement :status "no reply"))
        (`(error . ,text) (list :statement statement :error (format "%s" text)))
        (`(raw . ,line) (list :statement statement :text line))
        (`(ok . ,value) (ygg-db--redis-value-set statement value))))))

(defun ygg-db--redis-result (statements stdout stderr nonce exit)
  "The result of redis-cli's STDOUT and STDERR for STATEMENTS.
NONCE lines separate the replies; EXIT is the exit status."
  (let* ((parts (split-string stdout (concat "^" (regexp-quote (concat "\"" nonce "\"")) "\n")))
         (count (length statements))
         (complete (>= (length parts) (+ count 2)))
         (general (unless complete
                    (string-trim
                     (string-join
                      (delq nil (list (let ((failures (cl-remove-if-not
                                                       (lambda (line) (string-prefix-p "error:" line))
                                                       (split-string (car parts) "\n" t))))
                                        (and failures (format "%s" (cdr (ygg-db--redis-parse
                                                                         (car failures))))))
                                      (and (not (string-blank-p stderr))
                                           (string-join (delete-dups (split-string stderr "\n" t)) "\n"))
                                      (and (not (cdr parts)) (not (string-blank-p stdout))
                                           (not (string-search "error:" stdout)) stdout)))
                      "\n")))))
    (list :sets (and complete
                     (cl-loop for statement in statements
                              for chunk in (cdr parts)
                              collect (ygg-db--redis-set statement chunk)))
          :error (if (and general (string-empty-p general)) "no reply from redis-cli" general)
          :exit exit)))

(defun ygg-db--redis-pattern (key)
  "KEY with its id-like segments turned into *."
  (mapconcat (lambda (part)
               (if (string-match-p "\\`\\(?:[0-9]+\\|[0-9a-fA-F-]\\{8,\\}\\)\\'" part)
                   "*"
                 (ygg-db--redis-glob-quote part)))
             (split-string key ":") ":"))

(defun ygg-db--redis-entries (keys replies)
  "Schema tables of KEYS from REPLIES to TYPE and PTTL of each.
Each is (TYPE PATTERN (SAMPLE) NOTE)."
  (let ((groups nil))
    (cl-loop for key in keys
             for (type ttl) on replies by #'cddr
             do (let* ((id (cons type (ygg-db--redis-pattern key)))
                       (group (or (assoc id groups) (car (push (list id key 0 nil) groups)))))
                  (cl-incf (nth 2 group))
                  (when (and (numberp ttl) (> ttl 0))
                    (setf (nth 3 group) (min ttl (or (nth 3 group) ttl))))))
    (sort (mapcar (lambda (group)
                    (pcase-let ((`((,type . ,pattern) ,sample ,n ,ttl) group))
                      (list type pattern (list sample)
                            (concat (format "%d" n)
                                    (and ttl (format " · ttl %ds" (ceiling ttl 1000)))))))
                  groups)
          (lambda (a b) (if (equal (car a) (car b)) (string< (nth 1 a) (nth 1 b))
                          (string< (car a) (car b)))))))

(defun ygg-db--redis-sample (conn finish)
  "Sample CONN's keys with one SCAN, then TYPE and PTTL each.
FINISH gets the schema entry."
  (let ((fail (lambda (text) (funcall finish (list :state 'failed :error text)))))
    (ygg-db-run
     conn (list (format "SCAN 0 COUNT %d" ygg-db-redis-sample-count))
     (lambda (result)
       (condition-case failure
           (let* ((set (car (plist-get result :sets)))
                  (problem (or (plist-get result :error) (plist-get set :error)))
                  (keys (and (not problem)
                             (seq-take (append (and (vectorp (plist-get set :value))
                                                    (aref (plist-get set :value) 1))
                                               nil)
                                       ygg-db-redis-sample-count))))
             (cond
              (problem (funcall fail problem))
              ((null keys) (funcall finish (list :state 'ready :tables nil)))
              (t (ygg-db-run
                  conn (cl-mapcan (lambda (key) (list (concat "TYPE " (ygg-db--redis-quote key))
                                                      (concat "PTTL " (ygg-db--redis-quote key))))
                                  keys)
                  (lambda (typed)
                    (condition-case failure
                        (if (plist-get typed :error)
                            (funcall fail (plist-get typed :error))
                          (funcall finish
                                   (list :state 'ready
                                         :tables (ygg-db--redis-entries
                                                  keys (mapcar (lambda (set) (plist-get set :value))
                                                               (plist-get typed :sets))))))
                      (error (funcall fail (error-message-string failure)))))))))
         (error (funcall fail (error-message-string failure))))))))

(defun ygg-db--redis-helper (helper type _pattern sample)
  "Commands of HELPER for keys of TYPE, SAMPLE one of them."
  (let ((key (and sample (ygg-db--redis-quote sample))))
    (pcase helper
      ('describe
       (and key
            (delq nil (list (concat "TYPE " key) (concat "TTL " key)
                            (pcase type
                              ("string" (concat "GET " key))
                              ("hash" (concat "HGETALL " key))
                              ("list" (concat "LRANGE " key " 0 99"))
                              ("set" (concat "SSCAN " key " 0 COUNT 100"))
                              ("zset" (concat "ZRANGE " key " 0 99 WITHSCORES"))
                              ("stream" (concat "XRANGE " key " - + COUNT 100"))))))))))

(defun ygg-db--redis-await (process buffer errors lines deadline)
  "The LINES-th line PROCESS wrote to BUFFER, waiting until DEADLINE.
Anything on the ERRORS buffer ends the wait at once."
  (let ((line nil))
    (while (and (not line) (< (float-time) deadline))
      (when (> (buffer-size errors) 0)
        (error "%s" (string-trim (with-current-buffer errors (buffer-string)))))
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (and (zerop (forward-line lines)) (bolp))
          (forward-line -1)
          (setq line (buffer-substring-no-properties (point) (line-end-position)))))
      (unless (or line (accept-process-output process 0.05))
        (unless (process-live-p process) (setq deadline 0))))
    (or line (error "No reply from Redis"))))

(defun ygg-db--redis-scan (conn pattern limit)
  "(KEYS . COMPLETE) of CONN matching the glob PATTERN, by SCAN.
SCAN uses MATCH and COUNT, stops after LIMIT keys or
ygg-db-redis-completion-rounds calls, and signals on failure."
  (let ((output (generate-new-buffer " *ygg-db redis keys*" t))
        (errors (generate-new-buffer " *ygg-db redis keys stderr*" t))
        (process nil))
    (unwind-protect
        (pcase-let* ((`(,argv . ,preamble) (ygg-db--redis-command (plist-get conn :url)))
                     (match (ygg-db--redis-quote pattern))
                     (deadline (+ (float-time) ygg-db-redis-timeout))
                     (seen (cl-count ?\n preamble))
                     (cursor "0") (keys nil) (rounds 0))
          (setq process (make-process :name "ygg-db-keys" :command argv :buffer output
                                      :stderr errors :noquery t :connection-type 'pipe
                                      :coding 'utf-8-unix))
          (when-let* ((pipe (get-buffer-process errors))) (set-process-sentinel pipe #'ignore))
          (process-send-string process preamble)
          (while (progn
                   (process-send-string
                    process (format "SCAN %s MATCH %s COUNT %d\n" cursor match ygg-db-redis-scan-count))
                   (let ((reply (ygg-db--redis-parse
                                 (ygg-db--redis-await process output errors (cl-incf seen) deadline))))
                     (unless (and (eq (car reply) 'ok) (vectorp (cdr reply)))
                       (error "SCAN failed: %s" (cdr reply)))
                     (setq cursor (aref (cdr reply) 0)
                           keys (nconc keys (append (aref (cdr reply) 1) nil))))
                   (and (not (equal cursor "0"))
                        (< (cl-incf rounds) ygg-db-redis-completion-rounds)
                        (< (length keys) limit))))
          (cons (delete-dups keys) (equal cursor "0")))
      (when (process-live-p process) (delete-process process))
      (kill-buffer output)
      (kill-buffer errors))))

(defun ygg-db--redis-keys (conn prefix)
  "Keys of CONN that start with PREFIX; nil on any failure."
  (ignore-errors
    (car (ygg-db--redis-scan conn (concat (ygg-db--redis-glob-quote prefix) "*")
                             ygg-db-redis-completion-limit))))

(defun ygg-db--redis-list (conn pattern)
  "Show the keys of CONN matching PATTERN in the result window."
  (let* ((started (float-time))
         (statement (format "SCAN MATCH %s" pattern))
         (scan (condition-case failure
                   (ygg-db--redis-scan conn pattern ygg-db-redis-completion-limit)
                 (error (user-error "%s" (error-message-string failure)))))
         (result (list :connection conn
                       :sets (delq nil (list (ygg-db--redis-value-set statement (vconcat (car scan)))
                                             (unless (cdr scan)
                                               (list :statement statement
                                                     :status "stopped early; more keys may match"))))
                       :elapsed (- (float-time) started))))
    (ygg-db--show conn (ygg-db-render-result result) result)))

(defun ygg-db--redis-completion-at-point ()
  (let* ((conn ygg-db-connection)
         (end (point))
         (start (save-excursion (skip-chars-backward "^ \t\n\"'") (point)))
         (first (save-excursion (goto-char start) (skip-chars-backward " \t") (bolp))))
    (list start end
          (if first
              ygg-db--redis-commands
            (let ((prefix (buffer-substring-no-properties start end))
                  (keys 'unread))
              (lambda (string predicate action)
                (unless (eq action 'metadata)
                  (when (eq keys 'unread) (setq keys (ygg-db--redis-keys conn prefix)))
                  (complete-with-action action keys string predicate)))))
          :exclusive 'no
          :annotation-function (lambda (_) (if first " command" " key")))))

;;; Drawer

(defconst ygg-db-drawer-buffer "*db drawer*")

(defvar ygg-db--expanded (make-hash-table :test #'equal)
  "Drawer node id to t when open, or closed for a node open by default.")

(defvar-local ygg-db--drawer-directory nil
  "The directory whose connections the drawer lists.")

(defvar-keymap ygg-db-drawer-mode-map
  :doc "Keys of the database drawer."
  "j" #'ygg-db-drawer-next
  "k" #'ygg-db-drawer-previous
  "TAB" #'ygg-db-drawer-toggle-node
  "<tab>" #'ygg-db-drawer-toggle-node
  "RET" #'ygg-db-drawer-act
  "c" #'ygg-db-drawer-count
  "d" #'ygg-db-drawer-describe
  "i" #'ygg-db-drawer-indexes
  "a" #'ygg-db-add-connection
  "R" #'ygg-db-drawer-refresh
  "q" #'ygg-db-drawer-quit)

(define-derived-mode ygg-db-drawer-mode special-mode "DB"
  "Connections, their tables and saved queries."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  (setq-local header-line-format
              '(:eval (propertize (concat " databases  "
                                          (abbreviate-file-name (or ygg-db--drawer-directory "")))
                                  'face 'ygg-db-meta))))

(defun ygg-db--open-p (id &optional default)
  (let ((state (gethash id ygg-db--expanded (if default t nil))))
    (eq state t)))

(defun ygg-db--node-line (depth glyph label annotation node)
  (propertize (concat (make-string (* 2 depth) ?\s)
                      (propertize glyph 'face 'ygg-db-meta) " "
                      label
                      (if annotation (propertize (concat "  " annotation) 'face 'ygg-db-meta) "")
                      "\n")
              'ygg-db-node node))

(defun ygg-db--fold-glyph (open) (if open "▾" "▸"))

(defun ygg-db--table-lines (conn tables depth id-prefix)
  (mapconcat (lambda (entry)
               (ygg-db--node-line depth "·" (nth 1 entry) (nth 3 entry)
                                  (list :kind 'table :conn conn :schema (car entry) :table (nth 1 entry)
                                        :sample (car (nth 2 entry))
                                        :id (concat id-prefix "/" (nth 1 entry)))))
             tables ""))

(defun ygg-db--schema-lines (conn id)
  (let* ((entry (ygg-db--schema conn))
         (tables (plist-get entry :tables))
         (schemas (delete-dups (mapcar #'car tables))))
    (pcase (plist-get entry :state)
      ('loading (ygg-db--node-line 2 " " (propertize "loading…" 'face 'ygg-db-meta) nil
                                   (list :kind 'message :conn conn :id (concat id "/loading"))))
      ('failed (ygg-db--node-line 2 (propertize "✗" 'face 'ygg-db-failure)
                                  (ygg-db--first-line (or (plist-get entry :error) "failed")) nil
                                  (list :kind 'message :conn conn :id (concat id "/failed"))))
      ('ready
       (if (cdr schemas)
           (mapconcat
            (lambda (schema)
              (let ((sid (concat id "/" schema))
                    (in (cl-remove-if-not (lambda (e) (equal (car e) schema)) tables)))
                (concat (ygg-db--node-line 2 (ygg-db--fold-glyph (ygg-db--open-p sid)) schema
                                           (number-to-string (length in))
                                           (list :kind 'schema :conn conn :schema schema :id sid))
                        (when (ygg-db--open-p sid) (ygg-db--table-lines conn in 3 sid)))))
            schemas "")
         (ygg-db--table-lines conn tables 2 id))))))

(defun ygg-db--connection-lines (conn)
  (let* ((cid (concat "conn/" (plist-get conn :url)))
         (open (ygg-db--open-p cid))
         (tid (concat cid "/tables"))
         (qid (concat cid "/saved"))
         (saved (and open (ygg-db--saved-queries conn)))
         (schema (ygg-db--schema conn)))
    (concat
     (ygg-db--node-line 0 (ygg-db--fold-glyph open) (propertize (plist-get conn :name) 'face 'ygg-db-header)
                        (concat (ygg-db--family (plist-get conn :url))
                                (if (eq (plist-get conn :source) 'detected) "  found" ""))
                        (list :kind 'connection :conn conn :id cid))
     (when open
       (concat
        (ygg-db--node-line 1 "+" "new query" nil (list :kind 'new :conn conn :id (concat cid "/new")))
        (when saved
          (concat (ygg-db--node-line 1 (ygg-db--fold-glyph (ygg-db--open-p qid)) "saved queries"
                                     (number-to-string (length saved))
                                     (list :kind 'saved-group :conn conn :id qid))
                  (when (ygg-db--open-p qid)
                    (mapconcat (lambda (file)
                                 (ygg-db--node-line 2 "·" (file-name-base file) nil
                                                    (list :kind 'saved :conn conn :file file
                                                          :id (concat qid "/" file))))
                               saved ""))))
        (ygg-db--node-line 1 (ygg-db--fold-glyph (ygg-db--open-p tid t))
                           (if (ygg-db--redis-p conn) "keys" "tables")
                           (when (eq (plist-get schema :state) 'ready)
                             (number-to-string (length (plist-get schema :tables))))
                           (list :kind 'tables :conn conn :id tid :default t))
        (when (ygg-db--open-p tid t) (ygg-db--schema-lines conn tid)))))))

(defun ygg-db--node () (get-text-property (point) 'ygg-db-node))

(defun ygg-db--goto-node (id)
  (let ((match (text-property-search-forward 'ygg-db-node id
                                             (lambda (id node) (equal id (plist-get node :id))))))
    (when match (goto-char (prop-match-beginning match)) t)))

(defun ygg-db--drawer-render ()
  (let* ((id (plist-get (ygg-db--node) :id))
         (line (line-number-at-pos))
         (inhibit-read-only t)
         (conns (ygg-db-connections-here ygg-db--drawer-directory)))
    (erase-buffer)
    (if conns
        (mapc (lambda (conn) (insert (ygg-db--connection-lines conn))) conns)
      (insert (propertize " no connections · a adds one\n" 'face 'ygg-db-meta)))
    (goto-char (point-min))
    (unless (and id (ygg-db--goto-node id))
      (forward-line (1- line)))
    (ygg-db--to-label)))

(defun ygg-db--to-label ()
  (beginning-of-line)
  (skip-chars-forward " ")
  (unless (eolp) (forward-char 2)))

(defun ygg-db--drawer-redraw ()
  (when-let* ((buffer (get-buffer ygg-db-drawer-buffer)))
    (with-current-buffer buffer
      (let ((window (get-buffer-window buffer t)))
        (if window
            (with-selected-window window (ygg-db--drawer-render))
          (ygg-db--drawer-render))))))

(defun ygg-db--drawer-buffer (directory)
  (let ((buffer (get-buffer-create ygg-db-drawer-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ygg-db-drawer-mode) (ygg-db-drawer-mode))
      (setq ygg-db--drawer-directory directory
            default-directory directory)
      (ygg-db--drawer-render))
    buffer))

;;;###autoload
(defun ygg-db-drawer ()
  "Show the database drawer and select it."
  (interactive)
  (let* ((directory (if (derived-mode-p 'ygg-db-drawer-mode) ygg-db--drawer-directory
                      (or (ygg-db--project-root) default-directory)))
         (window (display-buffer-in-side-window
                  (ygg-db--drawer-buffer directory)
                  `((side . left) (slot . 0) (window-width . ,ygg-db-drawer-width)
                    (preserve-size . (t . nil))
                    (window-parameters . ((no-delete-other-windows . t)))))))
    (select-window window)))

;;;###autoload
(defun ygg-db-drawer-toggle ()
  "Show the database drawer, or hide it when it shows."
  (interactive)
  (if-let* ((buffer (get-buffer ygg-db-drawer-buffer))
            (window (get-buffer-window buffer)))
      (delete-window window)
    (ygg-db-drawer)))

(defun ygg-db-drawer-quit ()
  "Hide the drawer."
  (interactive)
  (quit-window))

(defun ygg-db-drawer-next (&optional n)
  "Move to the next N-th node."
  (interactive "p")
  (forward-line (or n 1))
  (when (eobp) (forward-line -1))
  (ygg-db--to-label))

(defun ygg-db-drawer-previous (&optional n)
  "Move to the previous N-th node."
  (interactive "p")
  (ygg-db-drawer-next (- (or n 1))))

(defun ygg-db-drawer-toggle-node ()
  "Open or close the node at point."
  (interactive)
  (let* ((node (or (ygg-db--node) (user-error "Nothing here")))
         (id (plist-get node :id)))
    (unless (memq (plist-get node :kind) '(connection saved-group tables schema))
      (user-error "Nothing to open here"))
    (let ((open (ygg-db--open-p id (plist-get node :default))))
      (puthash id (if open 'closed t) ygg-db--expanded)
      (when (and (not open) (eq (plist-get node :kind) 'connection))
        (ygg-db-load-schema (plist-get node :conn))))
    (ygg-db--drawer-render)))

(defun ygg-db--drawer-helper (helper)
  (let* ((node (ygg-db--node))
         (conn (plist-get node :conn)))
    (unless (eq (plist-get node :kind) 'table) (user-error "Not on a table"))
    (if (and (ygg-db--redis-p conn) (eq helper 'list))
        (ygg-db--redis-list conn (plist-get node :table))
      (ygg-db--drawer-run helper node conn))))

(defun ygg-db--drawer-run (helper node conn)
  (let* ((family (ygg-db--family (plist-get conn :url)))
         (statements (or (if (ygg-db--redis-p conn)
                             (ygg-db--redis-helper helper (plist-get node :schema)
                                                   (plist-get node :table) (plist-get node :sample))
                           (ygg-db--ensure-list
                            (ygg-db-helper-query helper family (plist-get node :schema)
                                                 (plist-get node :table))))
                         (user-error "No %s query for %s" helper family))))
    (ygg-db-query-connection conn statements (current-buffer))))

(defun ygg-db--ensure-list (value)
  (and value (list value)))

(defun ygg-db-drawer-act ()
  "Act on the node at point: open it, run it, or visit it."
  (interactive)
  (let ((node (or (ygg-db--node) (user-error "Nothing here"))))
    (pcase (plist-get node :kind)
      ('table (ygg-db--drawer-helper 'list))
      ('new (ygg-db--visit (ygg-db--scratch-buffer (plist-get node :conn))))
      ('saved (let ((conn (plist-get node :conn)))
                (ygg-db--visit (find-file-noselect (plist-get node :file)))
                (unless ygg-db-connection (ygg-db-bind conn))))
      ('message nil)
      (_ (ygg-db-drawer-toggle-node)))))

(defun ygg-db-drawer-count ()
  "Count the rows of the table at point."
  (interactive)
  (ygg-db--drawer-helper 'count))

(defun ygg-db-drawer-describe ()
  "Show the columns of the table at point."
  (interactive)
  (ygg-db--drawer-helper 'describe))

(defun ygg-db-drawer-indexes ()
  "Show the indexes of the table at point."
  (interactive)
  (ygg-db--drawer-helper 'indexes))

(defun ygg-db-drawer-refresh ()
  "Read connections again, and the schema of the connection at point."
  (interactive)
  (when-let* ((conn (plist-get (ygg-db--node) :conn)))
    (ygg-db-load-schema conn t))
  (ygg-db--drawer-render))

;;; Keys

(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-db-drawer-mode)
  (add-to-list 'ygg-modal-special-modes 'ygg-db-result-mode)
  (yggdrasil-define-mode-keys 'ygg-db-drawer-mode 'normal ygg-db-drawer-mode-map)
  (yggdrasil-define-mode-keys 'ygg-db-result-mode 'normal ygg-db-result-mode-map))

(with-eval-after-load 'yggdrasil-localleader
  (dolist (mode '(sql-mode ygg-db-redis-mode))
    (yggdrasil-localleader-def mode "e" #'ygg-db-execute "run statement / selection")
    (yggdrasil-localleader-def mode "b" #'ygg-db-execute-buffer "run buffer")
    (yggdrasil-localleader-def mode "k" #'ygg-db-pick-connection "pick connection")
    (yggdrasil-localleader-def mode "K" #'ygg-db-refresh-schema "refresh schema")
    (yggdrasil-localleader-def mode "i" #'ygg-db-interrupt "interrupt query")
    (yggdrasil-localleader-def mode "d" #'ygg-db-visidata "result in VisiData")
    (yggdrasil-localleader-def mode "u" #'ygg-db-drawer-toggle "database drawer")
    (yggdrasil-localleader-def mode "s" #'ygg-db-save-query "save query"))
  (yggdrasil-localleader-def 'ygg-db-result-mode "d" #'ygg-db-visidata "result in VisiData")
  (yggdrasil-localleader-def 'ygg-db-result-mode "i" #'ygg-db-interrupt "interrupt query")
  (yggdrasil-localleader-def 'ygg-db-result-mode "u" #'ygg-db-drawer-toggle "database drawer"))

(with-eval-after-load 'layer-terminal
  (yggdrasil-define-keys 'ygg-leader-open-map
    "s" #'ygg-db-drawer-toggle :label "databases (sql)"))

(provide 'ygg-db)
;;; ygg-db.el ends here
