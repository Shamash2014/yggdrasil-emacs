;;; db-tests.el --- Tests for the usql database layer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-db)

(defmacro db-test-with-dir (var &rest body)
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "db-test-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defmacro db-test-isolated (&rest body)
  "BODY with no user connections, a scratch ygg-db-directory, and no auth sources."
  (declare (indent 0))
  `(db-test-with-dir db-test-home
     (let ((ygg-db-connections nil)
           (ygg-db-directory db-test-home)
           (auth-sources nil))
       (auth-source-forget-all-cached)
       ,@body)))

(defun db-test-usql ()
  (or (ignore-errors (ygg-db--program)) (ert-skip "no usql")))

(defun db-test-wait (predicate &optional seconds)
  (let ((deadline (+ (float-time) (or seconds 30))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun db-test-run-sync (conn statements)
  (let (result)
    (ygg-db-run conn statements (lambda (r) (setq result r)))
    (should (db-test-wait (lambda () result)))
    result))

;;; Drivers and connections

(ert-deftest ygg-db-schemes-map-to-families ()
  (should (equal (ygg-db--family "pg://u@h/db") "postgres"))
  (should (equal (ygg-db--family "postgres+unix://u@/db") "postgres"))
  (should (equal (ygg-db--family "sq:/tmp/x.db") "sqlite"))
  (should (equal (ygg-db--family "duckdb:/tmp/x.duckdb") "duckdb"))
  (should (equal (ygg-db--family "mssql://u@h/db") "sqlserver"))
  (should (equal (ygg-db--family "ch://h") "clickhouse"))
  (should (equal (ygg-db--family "bigquery://p") "bigquery"))
  (should (ygg-db--file-based-p "sqlite3:/x"))
  (should-not (ygg-db--file-based-p "mysql://u@h/db")))

(ert-deftest ygg-db-redact-drops-only-the-password ()
  (should (equal (ygg-db--redact "pg://ann:s3cret@db.example:5432/app")
                 "pg://ann@db.example:5432/app"))
  (should (equal (ygg-db--redact "sqlite:/tmp/a.db") "sqlite:/tmp/a.db")))

(ert-deftest ygg-db-project-connections-read-as-data-with-paths-made-absolute ()
  (db-test-isolated
    (db-test-with-dir root
      (make-directory (expand-file-name ".git" root))
      (with-temp-file (expand-file-name ".ygg-db" root)
        (insert "((\"local\" . \"sqlite:data/app.db\") (\"warehouse\" . \"pg://ann@wh/app\"))"))
      (let ((conns (ygg-db-connections-here root)))
        (should (equal (plist-get (car conns) :url)
                       (concat "sqlite:" (expand-file-name "data/app.db" root))))
        (should (eq (plist-get (car conns) :source) 'project))
        (should (equal (plist-get (cadr conns) :url) "pg://ann@wh/app"))))))

(ert-deftest ygg-db-project-directory-holds-connections-and-queries ()
  (db-test-isolated
    (db-test-with-dir root
      (make-directory (expand-file-name ".git" root))
      (make-directory (expand-file-name ".ygg-db" root))
      (with-temp-file (expand-file-name ".ygg-db/connections.eld" root)
        (insert "((\"app\" . \"pg://ann@h/app\"))"))
      (let ((conn (car (ygg-db-connections-here root))))
        (should (equal (plist-get conn :name) "app"))
        (should (equal (ygg-db--queries-directory conn)
                       (expand-file-name ".ygg-db/queries/app/" root)))))))

(ert-deftest ygg-db-a-connection-file-that-is-code-is-not-evaluated ()
  (db-test-isolated
    (db-test-with-dir root
      (make-directory (expand-file-name ".git" root))
      (with-temp-file (expand-file-name ".ygg-db" root)
        (insert "(progn (setq db-test-evaluated t))"))
      (defvar db-test-evaluated nil)
      (should-not (ygg-db--project-connections root))
      (should-not db-test-evaluated))))

(ert-deftest ygg-db-finds-sqlite-and-duckdb-files-by-their-magic ()
  (db-test-isolated
    (db-test-with-dir root
      (make-directory (expand-file-name ".git" root))
      (make-directory (expand-file-name "data" root))
      (with-temp-file (expand-file-name "data/app.sqlite" root)
        (set-buffer-multibyte nil)
        (insert "SQLite format 3\0" (make-string 84 0)))
      (with-temp-file (expand-file-name "notes.db" root)
        (insert "not a database"))
      (with-temp-file (expand-file-name "w.duckdb" root)
        (set-buffer-multibyte nil)
        (insert (make-string 8 0) "DUCK" (make-string 20 0)))
      (let ((found (mapcar (lambda (c) (cons (plist-get c :name) (plist-get c :url)))
                           (ygg-db-connections-here root))))
        (should (equal (cdr (assoc "data/app.sqlite" found))
                       (concat "sqlite:" (expand-file-name "data/app.sqlite" root))))
        (should (equal (cdr (assoc "w.duckdb" found))
                       (concat "duckdb:" (expand-file-name "w.duckdb" root))))
        (should-not (assoc "notes.db" found))))))

(ert-deftest ygg-db-added-connections-refuse-passwords-and-persist ()
  (db-test-isolated
    (should-error (ygg-db-add-connection "x" "pg://ann:pw@h/db") :type 'user-error)
    (ygg-db-add-connection "wh" "pg://ann@h/db")
    (should (equal (ygg-db--read-alist (ygg-db--saved-file)) '(("wh" . "pg://ann@h/db"))))
    (should (eq (plist-get (ygg-db--find-connection "wh" db-test-home) :source) 'saved))))

;;; Credentials

(defun db-test-authinfo (dir line)
  (let ((file (expand-file-name "authinfo" dir)))
    (with-temp-file file (insert line "\n"))
    file))

(ert-deftest ygg-db-password-from-auth-source-stays-out-of-argv ()
  (db-test-isolated
    (db-test-with-dir dir
      (let* ((secret "p@ss:w/rd%#x")
             (auth-sources (list (db-test-authinfo
                                  dir (format "machine db.example login ann port 5432 password %s"
                                              (prin1-to-string secret)))))
             (ygg-db-usql-program "sh"))
        (auth-source-forget-all-cached)
        (pcase-let* ((`(,argv . ,env) (ygg-db--command "pg://ann@db.example/app" dir "select 1;\n"))
                     (config (expand-file-name "config.yaml" dir)))
          (should-not (cl-some (lambda (arg) (string-search "p@ss" arg)) argv))
          (should-not (cl-some (lambda (arg) (string-search "p%40ss" arg)) argv))
          (should (equal (car (last argv)) "ygg_db"))
          (should-not (cl-some (lambda (e) (string-search "p@ss" e)) env))
          (should (member (concat "USQL_CONFIG=" config) env))
          (should (= (file-modes config) #o600))
          (with-temp-buffer
            (insert-file-contents config)
            (should (string-search (url-hexify-string secret) (buffer-string)))
            (should (string-search "pg://ann:" (buffer-string)))))))))

(ert-deftest ygg-db-inline-password-moves-into-the-config ()
  (db-test-isolated
    (db-test-with-dir dir
      (let ((ygg-db-usql-program "sh"))
        (pcase-let ((`(,argv . ,_) (ygg-db--command "pg://ann:inline@h/app" dir "select 1;")))
          (should-not (cl-some (lambda (arg) (string-search "inline" arg)) argv)))))))

(ert-deftest ygg-db-no-secret-means-the-url-as-given ()
  (db-test-isolated
    (should (equal (ygg-db--dsn "pg://ann@nowhere/app") "pg://ann@nowhere/app"))
    (should (equal (ygg-db--dsn "sqlite:/tmp/a.db") "sqlite:/tmp/a.db"))))

(ert-deftest ygg-db-private-directory-is-owner-only ()
  (let ((dir (ygg-db--private-directory)))
    (unwind-protect (should (= (file-modes dir) #o700))
      (delete-directory dir t))))

;;; Statements

(defun db-test-statements (text)
  (with-temp-buffer
    (sql-mode)
    (insert text)
    (ygg-db--statements (point-min) (point-max))))

(ert-deftest ygg-db-statements-split-on-semicolons-outside-strings-and-comments ()
  (should (equal (db-test-statements
                  "select 'a;b' as x;\n-- a; comment\nselect 2\n  from t;\n")
                 '("select 'a;b' as x;" "-- a; comment\nselect 2\n  from t;"))))

(ert-deftest ygg-db-statements-split-on-blank-lines-and-meta-lines ()
  (should (equal (db-test-statements "select 1\n\nselect 2\n\\dt\nselect 3;")
                 '("select 1" "select 2" "\\dt" "select 3;"))))

(ert-deftest ygg-db-comment-only-statements-are-dropped ()
  (should (equal (db-test-statements "-- nothing\n;\nselect 1;") '("select 1;"))))

(ert-deftest ygg-db-statement-at-point-picks-the-one-around-point ()
  (with-temp-buffer
    (sql-mode)
    (insert "select 1;\nselect *\n  from people\n where age > 3;\nselect 3;")
    (goto-char (point-min))
    (search-forward "people")
    (let ((bound (ygg-db--statement-at (point))))
      (should (equal (buffer-substring (car bound) (cdr bound))
                     "select *\n  from people\n where age > 3;")))))

(ert-deftest ygg-db-script-terminates-and-marks-each-statement ()
  (should (equal (ygg-db--script '("select 1" "\\dt") "N")
                 "select 1;\n\\echo N\n\\warn N\n\\dt\n\\echo N\n\\warn N\n")))

;;; CSV and results

(ert-deftest ygg-db-csv-handles-quotes-newlines-and-null ()
  (let ((parsed (ygg-db-parse-csv
                 (concat "id,name,note\n1,\"Grace, H\",\"say \"\"hi\"\"\"\n2,"
                         ygg-db--null ",\"two\nlines\"\n3,,x\n"))))
    (should (= (cdr parsed) 4))
    (should (equal (car parsed)
                   '(("id" "name" "note") ("1" "Grace, H" "say \"hi\"")
                     ("2" nil "two\nlines") ("3" "" "x"))))))

(ert-deftest ygg-db-csv-limit-reads-few-but-counts-all ()
  (let ((parsed (ygg-db-parse-csv "a\n1\n\"2\n2\"\n3\n4\n" 2)))
    (should (equal (car parsed) '(("a") ("1"))))
    (should (= (cdr parsed) 5))))

(ert-deftest ygg-db-output-splits-into-sets-per-statement ()
  (let* ((result (ygg-db--result '("select 1 as a" "insert into t values (1)" "select * from nope")
                                 "a\n1\nN\nINSERT 1\nN\nN\n"
                                 "N\nN\nerror: no such table: nope\nN\n" "N" 1))
         (sets (plist-get result :sets)))
    (should (equal (plist-get (nth 0 sets) :columns) '("a")))
    (should (equal (plist-get (nth 0 sets) :rows) '(("1"))))
    (should (equal (plist-get (nth 1 sets) :status) "INSERT 1"))
    (should (equal (plist-get (nth 2 sets) :error) "error: no such table: nope"))
    (should-not (plist-get result :error))))

(ert-deftest ygg-db-a-connection-failure-is-a-general-error ()
  (let ((result (ygg-db--result '("select 1") "" "error: could not connect\n" "N" 1)))
    (should (equal (plist-get result :error) "error: could not connect"))))

(ert-deftest ygg-db-empty-select-is-a-table-with-no-rows ()
  (let ((set (ygg-db--set "select * from t where false" "id,name\n" nil)))
    (should (equal (plist-get set :columns) '("id" "name")))
    (should (= (plist-get set :total) 0))))

(ert-deftest ygg-db-render-aligns-columns-with-thin-rules ()
  (let* ((text (ygg-db-render-table '("id" "name") '(("1" "Ada") ("12" nil))))
         (lines (split-string text "\n" t)))
    (should (equal (mapcar #'substring-no-properties lines)
                   '(" id │ name" "────┼──────" "  1 │ Ada " " 12 │ NULL")))
    (should (eq (get-text-property (string-search "NULL" text) 'face text) 'ygg-db-null))
    (should (eq (get-text-property (string-search "│" text) 'face text) 'ygg-db-rule))
    (should (eq (get-text-property 1 'face text) 'ygg-db-header))))

(ert-deftest ygg-db-render-cuts-long-cells-and-flattens-newlines ()
  (let* ((ygg-db-cell-max-width 6)
         (text (substring-no-properties (ygg-db-render-table '("x") '(("abcdefghij") ("a\nb"))))))
    (should (string-search "abcde…" text))
    (should (string-search "a↵b" text))))

(ert-deftest ygg-db-render-notes-truncation-count-and-time ()
  (let* ((ygg-db-result-max-rows 2)
         (set (ygg-db--set "select * from t" "a\n1\n2\n3\n4\n" nil))
         (text (substring-no-properties
                (ygg-db-render-result (list :sets (list set) :elapsed 0.0123 :exit 0)))))
    (should (string-search " 4 rows · showing 2 · \\d opens all in VisiData" text))
    (should (string-search "12 ms" text))
    (should-not (string-search "│ 3" text))))

;;; Helpers

(ert-deftest ygg-db-identifiers-quote-only-when-needed ()
  (should (equal (ygg-db--identifier "people" "postgres") "people"))
  (should (equal (ygg-db--identifier "People" "postgres") "\"People\""))
  (should (equal (ygg-db--identifier "odd name" "mysql") "`odd name`"))
  (should (equal (ygg-db--identifier "x]y" "sqlserver") "[x]]y]"))
  (should (equal (ygg-db--identifier "PEOPLE" "oracle") "PEOPLE")))

(ert-deftest ygg-db-helper-queries-follow-the-driver ()
  (let ((ygg-db-table-limit 100))
    (should (equal (ygg-db-helper-query 'list "sqlite" "main" "people")
                   "select * from people limit 100"))
    (should (equal (ygg-db-helper-query 'list "postgres" "sales" "orders")
                   "select * from sales.orders limit 100"))
    (should (equal (ygg-db-helper-query 'list "postgres" "public" "orders")
                   "select * from orders limit 100"))
    (should (equal (ygg-db-helper-query 'list "sqlserver" "dbo" "t") "select top 100 * from t"))
    (should (equal (ygg-db-helper-query 'count "duckdb" "main" "sales") "select count(*) from sales"))
    (should (string-search "pragma_table_info('it''s')" (ygg-db-helper-query 'describe "sqlite" "main" "it's")))
    (should (string-search "pg_indexes" (ygg-db-helper-query 'indexes "postgres" "public" "t")))
    (should-not (ygg-db-helper-query 'indexes "bigquery" "d" "t"))))

(ert-deftest ygg-db-catalog-rows-fold-into-tables ()
  (should (equal (ygg-db--tables-from-rows '(("main" "a" "x") ("main" "a" "y") ("main" "b" "z")))
                 '(("main" "a" ("x" "y")) ("main" "b" ("z"))))))

;;; Completion

(defconst db-test-tables
  '(("main" "people" ("id" "name" "age")) ("main" "sales" ("id" "region" "amount"))))

(ert-deftest ygg-db-completion-after-select-offers-every-column ()
  (let ((names (ygg-db-completion-candidates db-test-tables "select " nil)))
    (should (member "name" names))
    (should (member "region" names))
    (should (member "people" names))))

(ert-deftest ygg-db-completion-scopes-columns-to-the-tables-named ()
  (let ((names (ygg-db-completion-candidates db-test-tables "select  from people" nil)))
    (should (member "age" names))
    (should-not (member "region" names))))

(ert-deftest ygg-db-completion-resolves-an-alias ()
  (should (equal (ygg-db-completion-candidates db-test-tables "select s. from sales as s join people p on" "s")
                 '("id" "region" "amount")))
  (should (equal (ygg-db-completion-candidates db-test-tables "select p. from sales s join people p" "p")
                 '("id" "name" "age"))))

(ert-deftest ygg-db-capf-returns-bounds-at-an-empty-prefix-and-is-not-exclusive ()
  (let ((conn (list :name "t" :url "sqlite:/nonexistent/t.db")))
    (puthash (ygg-db--key conn) (list :state 'ready :tables db-test-tables) ygg-db--schemas)
    (unwind-protect
        (with-temp-buffer
          (sql-mode)
          (setq ygg-db-connection conn)
          (insert "select ")
          (let ((capf (ygg-db-completion-at-point)))
            (should (= (nth 0 capf) (point)))
            (should (= (nth 1 capf) (point)))
            (should (member "age" (all-completions "" (nth 2 capf))))
            (should (eq (plist-get (nthcdr 3 capf) :exclusive) 'no)))
          (insert "a")
          (should (equal (all-completions "a" (nth 2 (ygg-db-completion-at-point))) '("age" "amount"))))
      (remhash (ygg-db--key conn) ygg-db--schemas))))

(ert-deftest ygg-db-capf-without-a-schema-returns-nothing-and-never-blocks ()
  (let ((conn (list :name "t" :url "sqlite:/nonexistent/t.db")))
    (puthash (ygg-db--key conn) (list :state 'loading) ygg-db--schemas)
    (unwind-protect
        (with-temp-buffer
          (sql-mode)
          (setq ygg-db-connection conn)
          (insert "select ")
          (should-not (ygg-db-completion-at-point)))
      (remhash (ygg-db--key conn) ygg-db--schemas))))

;;; Against real databases

(defun db-test-sqlite (dir)
  (let ((conn (ygg-db--make-connection "s" (concat "sqlite:" (expand-file-name "t.sqlite" dir)) 'config)))
    (db-test-run-sync conn '("create table people(id integer primary key, name text, age integer)"
                             "insert into people values (1, 'Ada', 36), (2, 'Linus', null)"
                             "create index people_name on people(name)"))
    conn))

(ert-deftest ygg-db-runs-on-sqlite-and-reads-its-schema ()
  (db-test-usql)
  (db-test-isolated
    (db-test-with-dir dir
      (let* ((conn (db-test-sqlite dir))
             (result (db-test-run-sync conn '("select id, name,\n  age from people order by id")))
             (set (car (plist-get result :sets)))
             (schema nil))
        (should (equal (plist-get set :columns) '("id" "name" "age")))
        (should (equal (plist-get set :rows) '(("1" "Ada" "36") ("2" "Linus" nil))))
        (ygg-db-load-schema conn t (lambda (entry) (setq schema entry)))
        (should (db-test-wait (lambda () schema)))
        (should (equal (plist-get schema :tables) '(("main" "people" ("id" "name" "age")))))
        (let ((indexes (car (plist-get (db-test-run-sync
                                        conn (list (ygg-db-helper-query 'indexes "sqlite" "main" "people")))
                                       :sets))))
          (should (equal (car (car (plist-get indexes :rows))) "people_name")))
        (remhash (ygg-db--key conn) ygg-db--schemas)))))

(ert-deftest ygg-db-runs-on-duckdb-one-process-at-a-time ()
  (db-test-usql)
  (db-test-isolated
    (db-test-with-dir dir
      (let* ((conn (ygg-db--make-connection "d" (concat "duckdb:" (expand-file-name "t.duckdb" dir)) 'config))
             (first nil) (second nil))
        (db-test-run-sync conn '("create table sales(id integer, region varchar)"
                                 "insert into sales values (1, 'north'), (2, null)"))
        (ygg-db-run conn '("select count(*) as n from range(20000000)") (lambda (r) (setq first r)))
        (ygg-db-run conn '("select * from sales order by id") (lambda (r) (setq second r)))
        (should (db-test-wait (lambda () (and first second)) 60))
        (should-not (plist-get second :error))
        (should (equal (plist-get (car (plist-get second :sets)) :rows) '(("1" "north") ("2" nil))))))))

(ert-deftest ygg-db-a-failing-statement-reports-its-error ()
  (db-test-usql)
  (db-test-isolated
    (db-test-with-dir dir
      (let* ((conn (db-test-sqlite dir))
             (result (db-test-run-sync conn '("select 1 as a" "select * from nope" "select 2 as b")))
             (sets (plist-get result :sets)))
        (should (equal (plist-get (nth 0 sets) :rows) '(("1"))))
        (should (string-search "no such table" (plist-get (nth 1 sets) :error)))
        (should (equal (plist-get (nth 2 sets) :rows) '(("2"))))
        (should (= (plist-get result :exit) 1))))))

(ert-deftest ygg-db-interrupt-kills-the-owner-query-and-cleans-up ()
  (db-test-usql)
  (db-test-isolated
    (db-test-with-dir dir
      (let* ((conn (ygg-db--make-connection "d" (concat "duckdb:" (expand-file-name "i.duckdb" dir)) 'config))
             (result nil)
             (run nil))
        (with-temp-buffer
          (setq run (ygg-db-run conn '("select count(*) from range(100000000000)")
                                (lambda (r) (setq result r)) (current-buffer)))
          (should (db-test-wait (lambda () (ygg-db-run-process run)) 5))
          (should (ygg-db--stop (current-buffer)))
          (should (db-test-wait (lambda () result) 10))
          (should (plist-get result :interrupted))
          (should-not (file-exists-p (ygg-db-run-directory run)))
          (should-not (gethash (ygg-db--key conn) ygg-db--running)))))))

;;; Review fixes

(defun db-test-sleeper (dir)
  "A stand-in usql in DIR that only sleeps, so a run stays running."
  (let ((file (expand-file-name "sleeper" dir)))
    (with-temp-file file (insert "#!/bin/sh\nexec sleep 30\n"))
    (set-file-modes file #o700)
    file))

(defun db-test-settle (key)
  "Kill whatever still runs on KEY and wait until nothing does."
  (remhash key ygg-db--queued)
  (when-let* ((run (gethash key ygg-db--running)))
    (when (process-live-p (ygg-db-run-process run))
      (kill-process (ygg-db-run-process run))))
  (should (db-test-wait (lambda () (not (gethash key ygg-db--running))) 10)))

(ert-deftest ygg-db-a-killed-process-buffer-still-cleans-up-and-advances ()
  (db-test-isolated
    (let* ((ygg-db-usql-program (db-test-sleeper db-test-home))
           (conn (ygg-db--make-connection "p" "pg://ann:hunter2@h.invalid/app" 'config))
           (key (ygg-db--key conn))
           (first-result nil)
           (first (ygg-db-run conn '("select 1") (lambda (r) (setq first-result r))))
           (second (ygg-db-run conn '("select 2") #'ignore))
           (config (expand-file-name "config.yaml" (ygg-db-run-directory first))))
      (unwind-protect
          (progn
            (should (file-exists-p config))
            (should (eq (gethash key ygg-db--running) first))
            (kill-buffer (process-buffer (ygg-db-run-process first)))
            (should (db-test-wait (lambda () (eq (gethash key ygg-db--running) second)) 10))
            (should-not (file-exists-p config))
            (should-not (file-exists-p (ygg-db-run-directory first)))
            (should first-result)
            (should-not (gethash key ygg-db--queued)))
        (db-test-settle key)))))

(ert-deftest ygg-db-a-queued-run-that-cannot-start-fails-and-the-queue-moves-on ()
  (db-test-isolated
    (let* ((sleeper (db-test-sleeper db-test-home))
           (ygg-db-usql-program sleeper)
           (conn (ygg-db--make-connection "p" "pg://ann:pw@h.invalid/app" 'config))
           (key (ygg-db--key conn))
           (calls 0)
           (failed nil))
      (cl-letf* ((real (symbol-function 'ygg-db--program))
                 ((symbol-function 'ygg-db--program)
                  (lambda ()
                    (if (= (cl-incf calls) 2) (user-error "usql went away") (funcall real)))))
        (let* ((first (ygg-db-run conn '("select 1") #'ignore))
               (_second (ygg-db-run conn '("select 2") (lambda (r) (setq failed r))))
               (third (ygg-db-run conn '("select 3") #'ignore)))
          (unwind-protect
              (progn
                (kill-process (ygg-db-run-process first))
                (should (db-test-wait (lambda () (eq (gethash key ygg-db--running) third)) 10))
                (should (equal (plist-get failed :error) "usql went away"))
                (should (string-search "usql went away"
                                       (substring-no-properties (ygg-db-render-result failed))))
                (should-not (gethash key ygg-db--queued)))
            (db-test-settle key)))))))

(ert-deftest ygg-db-a-file-local-connection-name-binds-without-connecting ()
  (db-test-isolated
    (let ((ygg-db-connections '(("prod" . "pg://ann@prod.invalid/app")))
          (enable-local-variables t)
          (reached nil))
      (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) (push 'auth reached) nil))
                ((symbol-function 'read-passwd) (lambda (&rest _) (push 'prompt reached) ""))
                ((symbol-function 'ygg-db-run) (lambda (&rest _) (push 'run reached) nil)))
        (let ((file (expand-file-name "evil.sql" db-test-home)))
          (with-temp-file file
            (insert "-- -*- mode: sql; ygg-db-connection-name: \"prod\" -*-\nselect 1;\n"))
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (should (equal (plist-get ygg-db-connection :name) "prod"))
                  (should-not reached)
                  (should-not (ygg-db--schema ygg-db-connection)))
              (kill-buffer buffer))))))))

(ert-deftest ygg-db-a-schema-load-that-cannot-start-is-failed-not-loading ()
  (let ((conn (ygg-db--make-connection "x" "pg://ann@nowhere.invalid/app" 'config)))
    (unwind-protect
        (dolist (failure '((user-error "no usql") (quit)))
          (remhash (ygg-db--key conn) ygg-db--schemas)
          (cl-letf (((symbol-function 'ygg-db--program)
                     (lambda () (signal (car failure) (cdr failure)))))
            (should (condition-case nil (progn (ygg-db-load-schema conn) nil) ((error quit) t))))
          (should (eq (plist-get (ygg-db--schema conn) :state) 'failed)))
      (remhash (ygg-db--key conn) ygg-db--schemas))))

(ert-deftest ygg-db-file-paths-are-escaped-for-usql ()
  (should (equal (ygg-db--file-url "sqlite" "/tmp/a.db") "sqlite:/tmp/a.db"))
  (should (equal (ygg-db--file-url "sqlite" "/tmp/p#q?r é/a.db")
                 "sqlite:file:/tmp/p%23q%3Fr%20%C3%A9/a.db"))
  (should (equal (ygg-db--file-url "duckdb" "/tmp/p#q/a.duckdb") "duckdb:/tmp/p%2523q/a.duckdb"))
  (should-not (ygg-db--file-url "duckdb" "/tmp/p?q/a.duckdb"))
  (should-not (ygg-db--file-url "duckdb" "/tmp/p%q/a.duckdb"))
  (should (equal (ygg-db--expand-file-url "sq:data/a.db" "/tmp/r#1/")
                 "sq:file:/tmp/r%231/data/a.db"))
  (should (equal (ygg-db--expand-file-url "sqlite:file:/tmp/a%23b.db" "/tmp/r/")
                 "sqlite:file:/tmp/a%23b.db")))

(defun db-test-seed (dsn statements)
  "Run STATEMENTS through usql itself at the already-escaped DSN."
  (should (zerop (call-process (ygg-db--program) nil nil nil "-X" "-w" "-c"
                               (mapconcat (lambda (s) (concat s ";")) statements " ") dsn))))

(ert-deftest ygg-db-found-files-under-hash-and-question-mark-paths-open-the-right-file ()
  (db-test-usql)
  (db-test-isolated
    (db-test-with-dir dir
      (let* ((allowed (url--allowed-chars (cons ?/ url-unreserved-chars)))
             (sqlite-root (file-name-as-directory (expand-file-name "p#r?j" dir)))
             (duck-root (file-name-as-directory (expand-file-name "d#x" dir))))
        (dolist (root (list sqlite-root duck-root))
          (make-directory (expand-file-name ".git" root) t))
        (db-test-seed (concat "sqlite:file:" (url-hexify-string (expand-file-name "a.db" sqlite-root) allowed))
                      '("create table t(x integer)" "insert into t values (7)"))
        (db-test-seed (concat "duckdb:" (url-hexify-string
                                         (url-hexify-string (expand-file-name "a.duckdb" duck-root) allowed)
                                         allowed))
                      '("create table t(x integer)" "insert into t values (8)"))
        (pcase-dolist (`(,root ,name ,row) (list (list sqlite-root "a.db" "7")
                                                 (list duck-root "a.duckdb" "8")))
          (let* ((found (cl-find name (ygg-db-connections-here root)
                                 :key (lambda (c) (plist-get c :name)) :test #'equal))
                 (result (db-test-run-sync found '("select x from t"))))
            (should (equal (plist-get (car (plist-get result :sets)) :rows) (list (list row))))))
        (should (file-exists-p (expand-file-name "a.db" sqlite-root)))
        (should-not (file-exists-p (expand-file-name "p" dir)))
        (should-not (file-exists-p (expand-file-name "d" dir)))))))

(ert-deftest ygg-db-visidata-file-is-private-and-removed-when-opening-fails ()
  (let ((seen nil))
    (cl-letf (((symbol-function 'ygg-visidata-open-file)
               (lambda (file)
                 (setq seen (list file (file-modes file) (file-modes (file-name-directory file))))
                 (error "vd broke"))))
      (with-temp-buffer
        (setq ygg-db--shown-result
              (list :connection (list :name "s") :sets (list (list :csv "a\n1\n"))))
        (should-error (ygg-db-visidata))))
    (should (equal (cdr seen) (list #o600 #o700)))
    (should-not (file-exists-p (file-name-directory (car seen))))))

;;; Redis

(defmacro db-test-with-redis-program (&rest body)
  (declare (indent 0))
  `(cl-letf (((symbol-function 'ygg-db--redis-program) (lambda () "redis-cli")))
     ,@body))

(ert-deftest ygg-db-redis-lines-split-like-redis-cli ()
  (should (equal (ygg-db-redis-split "SET \"a b\" 'c\\'d' \"x\\x41\\n\"")
                 '("SET" "a b" "c'd" "xA\n")))
  (should (equal (ygg-db-redis-split "  GET   k  ") '("GET" "k")))
  (should-not (ygg-db-redis-split "GET \"open"))
  (should-not (ygg-db-redis-split "GET \"a\"b"))
  (let ((secret "s3c\"r\\et #é"))
    (should (equal (decode-coding-string
                    (string-to-unibyte (car (ygg-db-redis-split (ygg-db--redis-quote secret))))
                    'utf-8)
                   secret))))

(ert-deftest ygg-db-redis-dangerous-commands-are-caught-however-written ()
  (should (equal (ygg-db--redis-dangerous "flushall") "FLUSHALL"))
  (should (equal (ygg-db--redis-dangerous "\"FLUSHDB\" async") "FLUSHDB"))
  (should (equal (ygg-db--redis-dangerous "'keys' *") "KEYS"))
  (should (equal (ygg-db--redis-dangerous "config set maxmemory 1") "CONFIG SET"))
  (should-not (ygg-db--redis-dangerous "CONFIG GET maxmemory"))
  (should-not (ygg-db--redis-dangerous "GET keys")))

(ert-deftest ygg-db-redis-dangerous-commands-ask-and-a-no-runs-nothing ()
  (let ((conn (ygg-db--make-connection "r" "redis://127.0.0.1:1" 'config))
        (asked nil) (ran nil) (answer nil))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt asked) answer))
              ((symbol-function 'ygg-db-run) (lambda (&rest _) (push t ran) nil))
              ((symbol-function 'ygg-db-load-schema) #'ignore)
              ((symbol-function 'ygg-db--show) #'ignore))
      (should-error (ygg-db-query-connection conn '("GET a" "flushall")) :type 'user-error)
      (should (string-search "FLUSHALL" (car asked)))
      (should-not ran)
      (setq answer t)
      (ygg-db-query-connection conn '("FLUSHDB"))
      (should ran)
      (setq asked nil ran nil)
      (ygg-db-query-connection conn '("GET a"))
      (should-not asked)
      (should ran))))

(ert-deftest ygg-db-redis-password-goes-to-stdin-never-argv ()
  (db-test-isolated
    (db-test-with-redis-program
      (pcase-let ((`(,argv . ,preamble) (ygg-db--redis-command "redis://ann:p%40ss%22w@h.invalid:6380/2")))
        (should (equal argv '("redis-cli" "-h" "h.invalid" "-p" "6380" "-2" "--json")))
        (should (equal preamble "AUTH \"ann\" \"p@ss\\\"w\"\nSELECT 2\nHELLO 3\n")))
      (should (member "--tls" (car (ygg-db--redis-command "rediss://h.invalid"))))
      (should (equal (cdr (ygg-db--redis-command "redis://h.invalid")) "HELLO 3\n"))
      (pcase-let ((`(,argv ,env ,input) (ygg-db--run-command
                                         (ygg-db--make-connection "r" "redis://:hunter2@h.invalid" 'config)
                                         db-test-home '("GET a") "N")))
        (should-not (cl-some (lambda (arg) (string-search "hunter2" arg)) argv))
        (should-not env)
        (should (string-prefix-p "AUTH \"hunter2\"\n" input))
        (should (string-suffix-p "ECHO N\nGET a\nECHO N\n" input))))))

(ert-deftest ygg-db-redis-buffers-split-statements-by-line ()
  (with-temp-buffer
    (ygg-db-redis-mode)
    (setq ygg-db-connection (ygg-db--make-connection "r" "redis://h.invalid" 'config))
    (insert "# seed\nSET a \"x; y\"\n\n  HGETALL user:1\n")
    (should (equal (ygg-db--statements (point-min) (point-max)) '("SET a \"x; y\"" "HGETALL user:1")))
    (goto-char (point-min))
    (search-forward "HGET")
    (let ((bound (ygg-db--statement-at (point))))
      (should (equal (buffer-substring (car bound) (cdr bound)) "  HGETALL user:1")))))

(ert-deftest ygg-db-redis-output-becomes-tables-trees-and-errors ()
  (let* ((stdout (concat "\"OK\"\n{\"server\":\"redis\"}\n\"N\"\n"
                         "{\"name\":\"ada\",\"age\":\"36\"}\n\"N\"\n"
                         "[\"x\",\"y\"]\n\"N\"\n"
                         "[\"0\",[\"a\",\"b\"]]\n\"N\"\n"
                         "error:\"ERR unknown command\"\n\"N\"\n"
                         "null\n\"N\"\n"))
         (result (ygg-db--redis-result '("HGETALL u" "LRANGE l 0 -1" "SCAN 0" "BAD" "GET nope")
                                       stdout "" "N" 0))
         (sets (plist-get result :sets)))
    (should-not (plist-get result :error))
    (should (equal (plist-get (nth 0 sets) :rows) '(("name" "ada") ("age" "36"))))
    (should (equal (plist-get (nth 1 sets) :rows) '(("1" "x") ("2" "y"))))
    (should (equal (plist-get (nth 2 sets) :text) "1) 0\n2) 1) a\n   2) b"))
    (should (equal (plist-get (nth 3 sets) :error) "ERR unknown command"))
    (should (equal (plist-get (nth 4 sets) :status) "nil"))
    (let ((failed (ygg-db--redis-result '("GET a" "GET b")
                                        "error:\"WRONGPASS invalid\"\nerror:\"NOAUTH x\"\nerror:\"NOAUTH y\"\n"
                                        "" "N" 0)))
      (should (equal (plist-get failed :error) "WRONGPASS invalid"))
      (should-not (plist-get failed :sets)))))

(ert-deftest ygg-db-redis-key-patterns-group-ids-and-show-ttl ()
  (should (equal (ygg-db--redis-pattern "user:42:session:9f86d081884c") "user:*:session:*"))
  (should (equal (ygg-db--redis-entries '("user:1" "user:2" "q") '("hash" -1 "hash" 5000 "list" -1))
                 '(("hash" "user:*" ("user:1") "2 · ttl 5s") ("list" "q" ("q") "1")))))

;;; Redis, live

(defconst db-test-redis-password "s3c\"r\\et #é")

(defun db-test-redis-ready-p (port)
  (with-temp-buffer
    (call-process "redis-cli" nil t nil "-p" (number-to-string port) "PING")
    (string-match-p "PONG\\|NOAUTH" (buffer-string))))

(defmacro db-test-with-redis (url-var &rest body)
  "BODY with URL-VAR a redis:// URL of a throwaway password-protected server."
  (declare (indent 1))
  `(progn
     (unless (and (executable-find "redis-server") (executable-find "redis-cli"))
       (ert-skip "no redis-server"))
     (db-test-with-dir redis-dir
       (let* ((port (+ 20000 (random 30000)))
              (server (make-process :name "db-test-redis" :noquery t :buffer nil
                                    :command (list "redis-server" "--port" (number-to-string port)
                                                   "--bind" "127.0.0.1" "--requirepass" db-test-redis-password
                                                   "--save" "" "--appendonly" "no" "--dir" redis-dir)))
              (,url-var (format "redis://:%s@127.0.0.1:%d"
                                (url-hexify-string db-test-redis-password) port)))
         (unwind-protect
             (progn
               (should (db-test-wait (lambda () (db-test-redis-ready-p port)) 10))
               ,@body)
           (delete-process server))))))

(ert-deftest ygg-db-redis-runs-commands-live-and-a-wrong-password-is-one-error ()
  (db-test-isolated
    (db-test-with-redis url
      (let* ((conn (ygg-db--make-connection "r" url 'config))
             (result (db-test-run-sync conn '("HSET user:1 name \"Ada L\" age 36" "RPUSH q x y"
                                              "HGETALL user:1" "LRANGE q 0 -1" "GET missing" "NOPE")))
             (sets (plist-get result :sets)))
        (should-not (plist-get result :error))
        (should (equal (plist-get (nth 2 sets) :rows) '(("name" "Ada L") ("age" "36"))))
        (should (equal (plist-get (nth 3 sets) :rows) '(("1" "x") ("2" "y"))))
        (should (equal (plist-get (nth 4 sets) :status) "nil"))
        (should (string-search "unknown command" (plist-get (nth 5 sets) :error)))
        (let ((wrong (db-test-run-sync
                      (ygg-db--make-connection "w" (replace-regexp-in-string ":s3c[^@]*@" ":nope@" url) 'config)
                      '("GET a" "GET b"))))
          (should (string-search "WRONGPASS" (plist-get wrong :error)))
          (should-not (plist-get wrong :sets)))))))

(ert-deftest ygg-db-redis-password-is-not-in-ps-args-or-environment ()
  (db-test-isolated
    (db-test-with-redis url
      (let* ((conn (ygg-db--make-connection "r" url 'config))
             (done nil)
             (run (ygg-db-run conn '("BLPOP nothing-here 3") (lambda (r) (setq done r)))))
        (should (db-test-wait (lambda () (process-id (ygg-db-run-process run))) 5))
        (let ((pid (number-to-string (process-id (ygg-db-run-process run)))))
          (dolist (flags '("-ww" "-wwE"))
            (let ((shown (with-temp-buffer
                           (call-process "ps" nil t nil flags "-o" "command=" "-p" pid)
                           (buffer-string))))
              (should (string-search "redis-cli" shown))
              (should-not (string-search "s3c" shown)))))
        (should (db-test-wait (lambda () done) 10))
        (should-not (plist-get done :error))))))

(ert-deftest ygg-db-redis-completes-keys-with-scan-and-escapes-globs ()
  (db-test-isolated
    (db-test-with-redis url
      (let ((conn (ygg-db--make-connection "r" url 'config))
            (ygg-db-redis-scan-count 10)
            (ygg-db-redis-completion-rounds 1000))
        (db-test-run-sync conn (append (cl-loop for i below 2000 collect (format "SET other:%d x" i))
                                       '("SET pick:1 a" "SET pick:2 b" "SET \"pick:*star\" c")))
        (should (equal (sort (ygg-db--redis-keys conn "pick:") #'string<)
                       '("pick:*star" "pick:1" "pick:2")))
        (should (equal (ygg-db--redis-keys conn "pick:*") '("pick:*star")))
        (with-temp-buffer
          (ygg-db-redis-mode)
          (setq ygg-db-connection conn)
          (insert "GET pick:")
          (let ((capf (ygg-db-completion-at-point)))
            (should (equal (sort (all-completions "pick:" (nth 2 capf)) #'string<)
                           '("pick:*star" "pick:1" "pick:2"))))
          (erase-buffer)
          (insert "HGETA")
          (should (equal (all-completions "HGETA" (nth 2 (ygg-db-completion-at-point))) '("HGETALL"))))))))

(ert-deftest ygg-db-redis-schema-samples-types-and-ttls ()
  (db-test-isolated
    (db-test-with-redis url
      (let ((conn (ygg-db--make-connection "r" url 'config))
            (schema nil))
        (db-test-run-sync conn '("HSET user:1 n a" "HSET user:2 n b" "SET token:9f86d081884c x EX 300"))
        (unwind-protect
            (progn
              (ygg-db-load-schema conn t (lambda (entry) (setq schema entry)))
              (should (db-test-wait (lambda () schema) 10))
              (should (eq (plist-get schema :state) 'ready))
              (let ((users (cl-find "user:*" (plist-get schema :tables) :key #'cadr :test #'equal)))
                (should (equal (car users) "hash"))
                (should (member (car (nth 2 users)) '("user:1" "user:2")))
                (should (equal (nth 3 users) "2")))
              (should (string-match-p "\\`1 · ttl [0-9]+s\\'"
                                      (nth 3 (assoc "string" (plist-get schema :tables))))))
          (remhash (ygg-db--key conn) ygg-db--schemas))))))

(provide 'db-tests)
;;; db-tests.el ends here
