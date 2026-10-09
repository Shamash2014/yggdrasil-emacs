;;; ygg-projects-context-tests.el --- Tests for the Context row's groups and its Repowise group -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'yggdrasil-leader)
(require 'ygg-projects)
(require 'ygg-ice)
(require 'ygg-repowise)
(require 'aob-context)

(defmacro ygg-projects-context-tests--with-root (var &rest body)
  "Run BODY with VAR a temp root holding files a.el and b.el, every cache empty."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "ctx-root" t))))
          (ygg-ice--context-cache (make-hash-table :test #'equal))
          (ygg-ice--context-pending (make-hash-table :test #'equal))
          (ygg-ice-context-changed-functions nil)
          (ygg-repowise--cache (make-hash-table :test #'equal))
          (ygg-repowise--pending (make-hash-table :test #'equal))
          (ygg-repowise--commits (make-hash-table :test #'equal))
          (ygg-repowise--commits-pending (make-hash-table :test #'equal))
          (ygg-repowise-changed-functions nil)
          (ygg-projects--context-flipped nil)
          (aob-context--items nil))
     (unwind-protect
         (progn
           (dolist (f '("a.el" "b.el" "c.el"))
             (with-temp-file (expand-file-name f ,var) (insert "x\n")))
           ,@body)
       (delete-directory ,var t))))

(defun ygg-projects-context-tests--fake-docs (root &rest keys)
  "Fill ROOT's ICE cache with one item for each of KEYS."
  (let ((file (lambda (f) (expand-file-name f root))))
    (puthash root
             (list :present t
                   :changes (and (memq :changes keys)
                                 (list (list :kind 'change :label "add-login"
                                             :file (funcall file "a.el")
                                             :change '(:tasks t :done 1 :total 2))))
                   :top (and (memq :top keys)
                             (list (list :kind 'section :label "Auth" :section t
                                         :file (funcall file "b.el") :line 1)
                                   (list :kind 'section :label "Arch" :section t
                                         :file (funcall file "c.el") :line 1)))
                   :adrs (and (memq :adrs keys)
                              (list (list :kind 'adr :label "0001 pg" :file (funcall file "a.el"))))
                   :glossary (and (memq :glossary keys)
                                  (list :kind 'glossary :label "CONTEXT.md" :file (funcall file "b.el")))
                   :views (and (memq :views keys)
                               (list (list :kind 'c4 :label "Landscape" :file (funcall file "c.el")))))
             ygg-ice--context-cache)))

(defun ygg-projects-context-tests--plain (nodes)
  "NODES as their text with the alignment padding squeezed out."
  (mapcar (lambda (n)
            (string-trim (replace-regexp-in-string
                          "\\`[ \t]*│ · " "" (replace-regexp-in-string
                                                "[ \t]+" " " (substring-no-properties n)))))
          nodes))

(defun ygg-projects-context-tests--make-db (root &rest sqls)
  (let ((db (sqlite-open (expand-file-name ".repowise/wiki.db"
                                           (progn (make-directory (expand-file-name ".repowise" root) t)
                                                  root)))))
    (dolist (s sqls) (sqlite-execute db s))
    (sqlite-close db)))

(defconst ygg-projects-context-tests--full-db
  '("CREATE TABLE git_metadata (file_path TEXT, commit_count_90d INTEGER)"
    "INSERT INTO git_metadata VALUES ('a.el', 5), ('b.el', 9), ('c.el', 0)"
    "CREATE TABLE health_file_metrics (file_path TEXT, score REAL)"
    "INSERT INTO health_file_metrics VALUES ('a.el', 80.5), ('b.el', 12.0), ('c.el', NULL)"
    "CREATE TABLE dead_code_findings (file_path TEXT, kind TEXT, symbol_name TEXT, start_line INTEGER, status TEXT)"
    "INSERT INTO dead_code_findings VALUES ('c.el', 'unused_function', 'foo', 7, 'open'), ('a.el', 'unreachable', 'bar', NULL, 'resolved')"))

(defmacro ygg-projects-context-tests--needs-sqlite ()
  `(skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))))

(ert-deftest ygg-projects-context-groups-in-order-and-empty-ones-left-out ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :changes :top :adrs :views)
    (let ((groups (ygg-projects--context-groups root)))
      (should (equal (mapcar #'car groups) '(openspec lat adrs c4)))
      (should (equal (mapcar (lambda (g) (length (nth 2 g))) groups) '(1 2 1 1))))
    (should-not (ygg-projects--context-groups (file-name-as-directory (make-temp-name "/nope"))))))

(ert-deftest ygg-projects-context-openspec-opens-and-the-rest-start-folded ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :changes :top :adrs)
    (let ((lines (ygg-projects-context-tests--plain (ygg-projects--context-nodes root))))
      (should (= (length lines) 4))
      (should (string-match-p "\\`▾ OpenSpec" (nth 0 lines)))
      (should (string-match-p "add-login.*1/2" (nth 1 lines)))
      (should (string-match-p "\\`▸ lat.*2\\'" (nth 2 lines)))
      (should (string-match-p "\\`▸ ADRs.*1\\'" (nth 3 lines))))))

(ert-deftest ygg-projects-context-toggle-is-remembered-like-folders ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :changes :top)
    (let ((lat (list :ice t :group 'lat)) (spec (list :ice t :group 'openspec)))
      (should (ygg-projects--toggle-context-group lat root))
      (should (ygg-projects--toggle-context-group spec root))
      (let ((lines (ygg-projects-context-tests--plain (ygg-projects--context-nodes root))))
        (should (equal (length lines) 4))
        (should (string-match-p "\\`▸ OpenSpec" (nth 0 lines)))
        (should (string-match-p "\\`▾ lat" (nth 1 lines)))
        (should (string-match-p "Auth" (nth 2 lines))))
      (should (ygg-projects--toggle-context-group lat root))
      (should-not (ygg-projects--context-group-open-p root 'lat))
      (should-not (ygg-projects--toggle-context-group "folder" root)))))

(ert-deftest ygg-projects-context-nothing-shows-a-none-line ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root)
    (should (equal (ygg-projects-context-tests--plain (ygg-projects--context-nodes root))
                   '("— none —")))))

(defun ygg-projects-context-tests--buffer (root nodes)
  "Insert NODES, the Context row's lines, under a Context row in a buffer."
  (insert (propertize "Context\n" 'ygg-project root 'ygg-row 'context))
  (dolist (n nodes) (insert n "\n"))
  (goto-char (point-min)))

(ert-deftest ygg-projects-context-header-targets-all-its-entries-and-an-entry-one ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :changes :top :adrs)
    (setq ygg-projects--context-flipped (list (cons root 'lat)))
    (with-temp-buffer
      (ygg-projects-context-tests--buffer root (ygg-projects--context-nodes root))
      (cl-letf (((symbol-function 'ygg-projects--selecting-p) #'ignore))
        (forward-line 3)
        (should (equal (mapcar (lambda (e) (plist-get e :label)) (ygg-projects--context-targets))
                       '("Auth" "Arch")))
        (forward-line 1)
        (should (equal (mapcar (lambda (e) (plist-get e :label)) (ygg-projects--context-targets))
                       '("Auth")))
        (goto-char (point-min))
        (should-not (ygg-projects--context-targets))))))

(ert-deftest ygg-projects-context-visual-selection-keeps-working-over-headers ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :changes :top :adrs)
    (setq ygg-projects--context-flipped (list (cons root 'lat)))
    (with-temp-buffer
      (ygg-projects-context-tests--buffer root (ygg-projects--context-nodes root))
      (forward-line 1)
      (set-mark (point))
      (forward-line 2)
      (let ((ygg--visual-p t))
        (should (equal (mapcar (lambda (e) (plist-get e :label)) (ygg-projects--context-targets))
                       '("add-login" "Auth" "Arch")))))))

(ert-deftest ygg-projects-context-header-quickfix-and-agent-take-every-entry ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :top)
    (let ((sent nil))
      (with-temp-buffer
        (ygg-projects-context-tests--buffer root (ygg-projects--context-nodes root))
        (forward-line 1)
        (cl-letf (((symbol-function 'ygg-projects--selecting-p) #'ignore)
                  ((symbol-function 'ygg-projects--in-sidebar-p) #'ignore)
                  ((symbol-function 'ygg-qf-from-text)
                   (lambda (text &rest _) (push text sent) 2)))
          (ygg-projects-context-quickfix)
          (should (equal (car sent)
                         (format "%sb.el:1: section Auth\n%sc.el:1: section Arch" root root)))
          (ygg-projects-context-to-agent)
          (should (= (length aob-context--items) 2)))))))

(ert-deftest ygg-projects-context-repowise-reads-the-db-into-entries ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (apply #'ygg-projects-context-tests--make-db root ygg-projects-context-tests--full-db)
    (should (ygg-repowise-present-p root))
    (should (ygg-repowise-update root))
    (let ((entries (ygg-repowise-entries root)))
      (should (equal (mapcar #'car entries) '("hotspots" "low health" "dead code")))
      (should (equal (plist-get (cdr (nth 0 entries)) :rows)
                     (list (list (concat root "b.el") 1 "9 commits/90d")
                           (list (concat root "a.el") 1 "5 commits/90d"))))
      (should (equal (plist-get (cdr (nth 1 entries)) :rows)
                     (list (list (concat root "b.el") 1 "score 12.0")
                           (list (concat root "a.el") 1 "score 80.5"))))
      (should (equal (plist-get (cdr (nth 2 entries)) :rows)
                     (list (list (concat root "c.el") 7 "unused_function foo"))))
      (should (equal (mapcar (lambda (e) (ygg-projects--entry-badge (cdr e))) entries)
                     '("2" "2" "1"))))))

(ert-deftest ygg-projects-context-repowise-caps-each-entry-at-the-limit ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (apply #'ygg-projects-context-tests--make-db root ygg-projects-context-tests--full-db)
    (let ((ygg-repowise-limit 1))
      (ygg-repowise-update root)
      (should (equal (mapcar (lambda (e) (length (plist-get (cdr e) :rows)))
                             (ygg-repowise-entries root))
                     '(1 1 1))))))

(ert-deftest ygg-projects-context-repowise-tolerates-missing-tables-and-columns ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--make-db
     root
     "CREATE TABLE git_metadata (file_path TEXT, commit_count_90d INTEGER)"
     "INSERT INTO git_metadata VALUES ('a.el', 3)"
     "CREATE TABLE dead_code_findings (file_path TEXT, kind TEXT)"
     "INSERT INTO dead_code_findings VALUES ('b.el', 'unused_import')")
    (ygg-repowise-update root)
    (let ((entries (ygg-repowise-entries root)))
      (should (equal (mapcar #'car entries) '("hotspots" "dead code")))
      (should (equal (plist-get (cdr (nth 1 entries)) :rows)
                     (list (list (concat root "b.el") 1 "unused_import")))))))

(ert-deftest ygg-projects-context-repowise-group-needs-a-read-index-and-keeps-an-empty-one ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :changes)
    (ygg-projects-context-tests--make-db root "CREATE TABLE unrelated (x INTEGER)")
    (should-not (assq 'repowise (ygg-projects--context-groups root)))
    (ygg-repowise-update root)
    (let ((group (assq 'repowise (ygg-projects--context-groups root))))
      (should group)
      (should (string-match-p "\\`Repowise now\\'" (nth 1 group)))
      (should-not (nth 2 group)))))

(ert-deftest ygg-projects-context-repowise-label-shows-age-and-stale ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (apply #'ygg-projects-context-tests--make-db root ygg-projects-context-tests--full-db)
    (let ((db (expand-file-name ".repowise/wiki.db" root)))
      (set-file-times db (time-subtract nil 7200)))
    (ygg-repowise-update root)
    (should (equal (ygg-projects--repowise-label root) "Repowise 2h"))
    (puthash root (cons (float-time) (- (float-time) 10000)) ygg-repowise--commits)
    (should-not (ygg-repowise-stale-p root))
    (puthash root (cons (float-time) (float-time)) ygg-repowise--commits)
    (should (equal (ygg-projects--repowise-label root) "Repowise 2h stale"))))

(ert-deftest ygg-projects-context-repowise-reads-only-when-the-mtime-moves-never-on-draw ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (apply #'ygg-projects-context-tests--make-db root ygg-projects-context-tests--full-db)
    (ygg-projects-context-tests--fake-docs root :changes)
    (let ((reads 0)
          (read (symbol-function 'ygg-repowise--read))
          (db (expand-file-name ".repowise/wiki.db" root)))
      (cl-letf (((symbol-function 'ygg-repowise--read)
                 (lambda (r) (cl-incf reads) (funcall read r))))
        (ygg-projects--context-nodes root)
        (should (= reads 0))
        (should (ygg-repowise-update root))
        (should (= reads 1))
        (dotimes (_ 3) (ygg-projects--context-nodes root) (ygg-projects--context-spec root))
        (should-not (ygg-repowise-update root))
        (should (= reads 1))
        (set-file-times db (time-add nil 100))
        (should (ygg-repowise-update root))
        (should (= reads 2))))))

(ert-deftest ygg-projects-context-repowise-scan-queues-a-read-off-the-draw-path ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (apply #'ygg-projects-context-tests--make-db root ygg-projects-context-tests--full-db)
    (let ((timers nil) (asked nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_ _ fn) (push fn timers)))
                ((symbol-function 'ygg-git-async)
                 (lambda (r args _cb) (push (cons r args) asked) t)))
        (ygg-repowise-scan root)
        (ygg-repowise-scan root)
        (should (= (length timers) 1))
        (should-not (ygg-repowise-cached-p root))
        (should (equal asked (list (cons root '("log" "-1" "--format=%ct")))))
        (funcall (car timers))
        (should (ygg-repowise-cached-p root))))))

(ert-deftest ygg-projects-context-repowise-row-appears-in-a-project-with-only-an-index ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (apply #'ygg-projects-context-tests--make-db root ygg-projects-context-tests--full-db)
    (cl-letf (((symbol-function 'ygg-projects--icon) (lambda (_ fallback) fallback)))
      (should (equal (ygg-projects--context-spec root) '(context "C" "Context" "0"))))))

(ert-deftest ygg-projects-context-repowise-entry-goes-to-quickfix-and-agent ()
  (ygg-projects-context-tests--needs-sqlite)
  (ygg-projects-context-tests--with-root root
    (apply #'ygg-projects-context-tests--make-db root ygg-projects-context-tests--full-db)
    (ygg-repowise-update root)
    (setq ygg-projects--context-flipped (list (cons root 'repowise)))
    (let ((sent nil))
      (with-temp-buffer
        (ygg-projects-context-tests--buffer root (ygg-projects--context-nodes root))
        (forward-line 1)
        (cl-letf (((symbol-function 'ygg-projects--selecting-p) #'ignore)
                  ((symbol-function 'ygg-projects--in-sidebar-p) #'ignore)
                  ((symbol-function 'ygg-qf-from-text)
                   (lambda (text &rest _) (push text sent) 1)))
          (forward-line 1)
          (ygg-projects-context-quickfix)
          (should (equal (car sent)
                         (format "%sb.el:1: hotspots 9 commits/90d\n%sa.el:1: hotspots 5 commits/90d"
                                 root root)))
          (ygg-projects-context-to-agent)
          (let ((item (car aob-context--items)))
            (should (equal (plist-get item :file) "repowise:hotspots"))
            (should (equal (plist-get item :text)
                           "b.el:1: 9 commits/90d\na.el:1: 5 commits/90d")))
          (setq sent nil aob-context--items nil)
          (goto-char (point-min))
          (forward-line 1)
          (ygg-projects-context-quickfix)
          (ygg-projects-context-to-agent)
          (should (= (length (split-string (car sent) "\n")) 5))
          (should (= (length aob-context--items) 3)))))))

(ert-deftest ygg-projects-context-visit-on-a-header-lists-its-entries-in-the-quickfix ()
  (ygg-projects-context-tests--with-root root
    (ygg-projects-context-tests--fake-docs root :top)
    (let ((sent nil) (visited nil))
      (cl-letf (((symbol-function 'ygg-qf-from-text)
                 (lambda (text &rest _) (push text sent) 2))
                ((symbol-function 'ygg-ice-visit-item) (lambda (it) (push it visited))))
        (let* ((group (assq 'lat (progn (setq ygg-projects--context-flipped nil)
                                        (ygg-projects--context-groups root))))
               (header (list :ice t :group 'lat :items (mapcar #'cdr (nth 2 group)))))
          (ygg-projects--context-visit header)
          (should (= (length (split-string (car sent) "\n")) 2))
          (should-not visited)
          (ygg-projects--context-visit (cdr (car (nth 2 group))))
          (should (= (length visited) 1)))))))

(ert-deftest ygg-projects-context-w-runs-the-repowise-update-on-its-group-only ()
  (ygg-projects-context-tests--with-root root
    (let ((ran nil))
      (cl-letf (((symbol-function 'ygg-agent-maps--repowise-update)
                 (lambda (r next) (push r ran) (should (functionp next)))))
        (with-temp-buffer
          (insert (propertize "Repowise\n" 'ygg-project root 'ygg-row 'context
                              'ygg-entry (list :ice t :group 'repowise :items nil)))
          (insert (propertize "lat\n" 'ygg-project root 'ygg-row 'context
                              'ygg-entry (list :ice t :group 'lat :items nil)))
          (goto-char (point-min))
          (ygg-projects-repowise-update)
          (should (equal ran (list root)))
          (forward-line 1)
          (should-error (ygg-projects-repowise-update) :type 'user-error)
          (should (eq (lookup-key ygg-projects-map "W") #'ygg-projects-repowise-update)))))))

(provide 'ygg-projects-context-tests)
;;; ygg-projects-context-tests.el ends here
