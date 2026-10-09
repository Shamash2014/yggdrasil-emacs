;;; ygg-repowise.el --- What a project's Repowise index says, for the sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; Hotspots, low health and dead code out of <root>/.repowise/wiki.db.
;; The index is read when its mtime moves, on a timer and never while
;; drawing; the draw looks at the cache alone.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'ygg-git nil t)

(defgroup ygg-repowise nil
  "Repowise index signals in the projects sidebar."
  :group 'tools)

(defcustom ygg-repowise-limit 20
  "How many files each Repowise entry lists."
  :type 'natnum :group 'ygg-repowise)

(defconst ygg-repowise--commit-ttl 120
  "Seconds the last commit's time is trusted before git is asked again.")

(defvar ygg-repowise-changed-functions nil
  "Abnormal hook run with ROOT when its cached Repowise signals changed.")

(defvar ygg-repowise--cache (make-hash-table :test #'equal)
  "Root to (:mtime T :entries CELLS) as the index was last read.")

(defvar ygg-repowise--pending (make-hash-table :test #'equal)
  "Roots with a read already queued.")

(defvar ygg-repowise--commits (make-hash-table :test #'equal)
  "Root to (ASKED . TIME), TIME the last commit's, as git last said.")

(defvar ygg-repowise--commits-pending (make-hash-table :test #'equal)
  "Roots with a git question already out.")

(defun ygg-repowise--root (root)
  (file-name-as-directory (expand-file-name root)))

(defun ygg-repowise--db (root)
  (expand-file-name ".repowise/wiki.db" root))

(defun ygg-repowise--mtime (root)
  "The index's mtime as a float, nil when ROOT is remote or has none."
  (unless (file-remote-p root)
    (when-let* ((attrs (file-attributes (ygg-repowise--db root))))
      (float-time (file-attribute-modification-time attrs)))))

(defun ygg-repowise-present-p (root)
  "Non-nil when ROOT has a local index; a stat, nothing read."
  (and (ygg-repowise--mtime root) t))

(defun ygg-repowise--columns (db table)
  (mapcar (lambda (row) (nth 1 row))
          (ignore-errors (sqlite-select db (format "PRAGMA table_info(%s)" table)))))

(defun ygg-repowise--select (db table needs sql)
  "SQL's rows when TABLE has every column NEEDS names, else nil."
  (let ((have (ygg-repowise--columns db table)))
    (when (seq-every-p (lambda (c) (member c have)) needs)
      (ignore-errors (sqlite-select db sql)))))

(defun ygg-repowise--entry (root name rows)
  "NAME's Repowise entry out of ROWS, each (FILE LINE TEXT), as (LABEL . ITEM)."
  (when rows
    (cons name
          (list :ice t :kind 'repowise :label name :metric name
                :badge (number-to-string (length rows))
                :file (concat "repowise:" (string-replace " " "-" name))
                :text (mapconcat (lambda (r)
                                   (format "%s:%d: %s" (car r) (nth 1 r) (nth 2 r)))
                                 rows "\n")
                :rows (mapcar (lambda (r)
                                (list (expand-file-name (car r) root) (nth 1 r) (nth 2 r)))
                              rows)))))

(defun ygg-repowise--read (root)
  "ROOT's Repowise entries as (LABEL . ITEM), read from its index."
  (when (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (when-let* ((db (ignore-errors (sqlite-open (ygg-repowise--db root) t))))
      (unwind-protect
          (let ((n ygg-repowise-limit)
                (dead (ygg-repowise--columns db "dead_code_findings")))
            (delq nil
                  (list
                   (ygg-repowise--entry
                    root "hotspots"
                    (mapcar (lambda (r) (list (nth 0 r) 1 (format "%d commits/90d" (nth 1 r))))
                            (ygg-repowise--select
                             db "git_metadata" '("file_path" "commit_count_90d")
                             (format "SELECT file_path, commit_count_90d FROM git_metadata WHERE commit_count_90d > 0 ORDER BY commit_count_90d DESC LIMIT %d" n))))
                   (ygg-repowise--entry
                    root "low health"
                    (mapcar (lambda (r) (list (nth 0 r) 1 (format "score %s" (nth 1 r))))
                            (ygg-repowise--select
                             db "health_file_metrics" '("file_path" "score")
                             (format "SELECT file_path, score FROM health_file_metrics WHERE score IS NOT NULL ORDER BY score ASC LIMIT %d" n))))
                   (ygg-repowise--entry
                    root "dead code"
                    (mapcar (lambda (r)
                              (list (nth 0 r)
                                    (if (integerp (nth 3 r)) (max 1 (nth 3 r)) 1)
                                    (string-join (delq nil (list (nth 1 r) (nth 2 r))) " ")))
                            (ygg-repowise--select
                             db "dead_code_findings" '("file_path" "kind")
                             (format "SELECT file_path, kind, %s, %s FROM dead_code_findings%s LIMIT %d"
                                     (if (member "symbol_name" dead) "symbol_name" "NULL")
                                     (if (member "start_line" dead) "start_line" "NULL")
                                     (if (member "status" dead) " WHERE status = 'open'" "")
                                     n)))))))
        (sqlite-close db)))))

(defun ygg-repowise-update (root)
  "Read ROOT's index again when its mtime moved; t when the cache changed."
  (let* ((root (ygg-repowise--root root))
         (mtime (ygg-repowise--mtime root))
         (was (gethash root ygg-repowise--cache)))
    (cond ((null mtime)
           (when was
             (remhash root ygg-repowise--cache)
             (run-hook-with-args 'ygg-repowise-changed-functions root)
             t))
          ((equal mtime (plist-get was :mtime)) nil)
          (t (puthash root (list :mtime mtime :entries (ygg-repowise--read root))
                      ygg-repowise--cache)
             (run-hook-with-args 'ygg-repowise-changed-functions root)
             t))))

(defun ygg-repowise--ask-commit (root)
  "Ask git, without waiting, when ROOT's last commit was made."
  (let ((was (gethash root ygg-repowise--commits)))
    (when (and (fboundp 'ygg-git-async)
               (not (gethash root ygg-repowise--commits-pending))
               (or (null was) (> (- (float-time) (car was)) ygg-repowise--commit-ttl)))
      (when (ignore-errors
              (ygg-git-async
               root '("log" "-1" "--format=%ct")
               (lambda (out exit)
                 (remhash root ygg-repowise--commits-pending)
                 (let* ((n (and (zerop exit) (string-to-number out)))
                        (time (and n (> n 0) n)))
                   (puthash root (cons (float-time) time) ygg-repowise--commits)
                   (unless (equal time (cdr was))
                     (run-hook-with-args 'ygg-repowise-changed-functions root))))))
        (puthash root t ygg-repowise--commits-pending)))))

(defun ygg-repowise-scan (root)
  "Queue a read of ROOT's index when its mtime moved, and ask for its last commit."
  (let ((root (ygg-repowise--root root)))
    (when (or (ygg-repowise--mtime root) (gethash root ygg-repowise--cache))
      (unless (gethash root ygg-repowise--pending)
        (puthash root t ygg-repowise--pending)
        (run-at-time 0 nil (lambda ()
                             (unwind-protect (ignore-errors (ygg-repowise-update root))
                               (remhash root ygg-repowise--pending)))))
      (ygg-repowise--ask-commit root))))

(defun ygg-repowise-cached-p (root)
  "Non-nil once ROOT's index has been read."
  (and (gethash (ygg-repowise--root root) ygg-repowise--cache) t))

(defun ygg-repowise-entries (root)
  "ROOT's Repowise entries as (LABEL . ITEM), from the cache only."
  (plist-get (gethash (ygg-repowise--root root) ygg-repowise--cache) :entries))

(defun ygg-repowise-indexed-at (root)
  "When ROOT's index was last written, as the cache has it."
  (plist-get (gethash (ygg-repowise--root root) ygg-repowise--cache) :mtime))

(defun ygg-repowise-stale-p (root)
  "Non-nil when ROOT has a commit newer than its index, as last asked."
  (let ((indexed (ygg-repowise-indexed-at root))
        (commit (cdr (gethash (ygg-repowise--root root) ygg-repowise--commits))))
    (and indexed commit (> commit indexed))))

(defun ygg-repowise-rows (items)
  "ITEMS with each Repowise entry turned into one item per file it lists."
  (mapcan (lambda (it)
            (if (eq (plist-get it :kind) 'repowise)
                (mapcar (lambda (r)
                          (list :kind (plist-get it :metric) :label (nth 2 r)
                                :file (nth 0 r) :line (nth 1 r)))
                        (plist-get it :rows))
              (list it)))
          items))

(provide 'ygg-repowise)
;;; ygg-repowise.el ends here
