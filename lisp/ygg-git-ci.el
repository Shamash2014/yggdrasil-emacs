;;; ygg-git-ci.el --- CI runs of the current branch in magit status -*- lexical-binding: t; -*-

;;; Commentary:
;; A magit status section showing the latest pipeline or workflow runs of
;; the checked-out branch, with keys to open, rerun, cancel, read the log
;; of, and trigger them.  Answers come from the forge in the background;
;; the section draws from the last one and never waits for the next.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'ansi-color)
(require 'url-util)
(require 'magit)
(require 'ygg-git-review-requests)
(require 'ygg-git-compare-pr-info)

(defgroup ygg-git-ci nil
  "CI runs in magit status."
  :group 'magit)

(defcustom ygg-git-ci-status t
  "Whether magit status shows the CI runs of the current branch."
  :type 'boolean)

(defcustom ygg-git-ci-ttl 120
  "Seconds before a branch's runs are asked for again."
  :type 'integer)

(defcustom ygg-git-ci-timeout 30
  "Seconds before a forge that has not answered is given up on."
  :type 'integer)

(defcustom ygg-git-ci-poll-interval 30
  "Seconds between asks while a run is in progress and its status is visible."
  :type 'integer)

(defvar ygg-git-ci--cache (make-hash-table :test 'equal))
(defvar ygg-git-ci--running (make-hash-table :test 'equal))
(defvar ygg-git-ci--watchers (make-hash-table :test 'equal))
(defvar ygg-git-ci--buffers (make-hash-table :test 'equal))
(defvar ygg-git-ci--timers (make-hash-table :test 'equal))
(defvar ygg-git-ci--waiters (make-hash-table :test 'equal))

(defconst ygg-git-ci--github-fields
  "databaseId,workflowName,status,conclusion,createdAt,startedAt,updatedAt,headSha,event,url")

(defun ygg-git-ci--program (repo)
  (if (eq (car repo) 'github) "gh" "glab"))

(defun ygg-git-ci--slug (repo)
  (concat (nth 1 repo) "/" (nth 2 repo)))

(defun ygg-git-ci--glab-repo (repo)
  (format "https://%s/%s" (nth 1 repo) (nth 2 repo)))

(defconst ygg-git-ci--page-size 100)
(defconst ygg-git-ci--max-pages 5)

(defun ygg-git-ci--project (repo)
  (concat "projects/" (url-hexify-string (nth 2 repo))))

(defun ygg-git-ci--github-state (status conclusion)
  (pcase status
    ("completed"
     (pcase conclusion
       ("success" 'passed)
       ((or "failure" "timed_out" "startup_failure" "action_required") 'failed)
       ("cancelled" 'cancelled)
       (_ 'skipped)))
    ("in_progress" 'running)
    (_ 'pending)))

(defun ygg-git-ci--gitlab-state (status allow-failure)
  (pcase status
    ("success" 'passed)
    ("failed" (if allow-failure 'warning 'failed))
    ("running" 'running)
    ((or "canceled" "canceling") 'cancelled)
    ((or "skipped" "manual" "scheduled") 'skipped)
    (_ 'pending)))

(defun ygg-git-ci--json (text)
  (json-parse-string text :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun ygg-git-ci--parse-github (text)
  "The latest run of each workflow in TEXT, gh's run list, newest first."
  (let (latest)
    (dolist (item (ygg-git-ci--json text))
      (let* ((name (or (plist-get item :workflowName) "workflow"))
             (created (ygg-git-review-requests--time (plist-get item :createdAt)))
             (row (list :id (plist-get item :databaseId)
                        :name name
                        :state (ygg-git-ci--github-state (plist-get item :status)
                                                         (plist-get item :conclusion))
                        :created created
                        :started (ygg-git-review-requests--time (plist-get item :startedAt))
                        :finished (ygg-git-review-requests--time (plist-get item :updatedAt))
                        :sha (plist-get item :headSha)
                        :event (plist-get item :event)
                        :url (plist-get item :url)))
             (seen (assoc name latest)))
        (cond ((null seen) (push (cons name row) latest))
              ((> (or created 0) (or (plist-get (cdr seen) :created) 0))
               (setcdr seen row)))))
    (sort (mapcar #'cdr latest)
          (lambda (a b) (> (or (plist-get a :created) 0) (or (plist-get b :created) 0))))))

(defun ygg-git-ci--parse-gitlab-pipeline (text)
  "The newest pipeline in TEXT, the API's pipeline list, as (:id :sha :url :state)."
  (when-let* ((pipeline (car (ygg-git-ci--json text))))
    (list :id (plist-get pipeline :id)
          :sha (plist-get pipeline :sha)
          :url (plist-get pipeline :web_url)
          :state (ygg-git-ci--gitlab-state (plist-get pipeline :status) nil)
          :created (ygg-git-review-requests--time (plist-get pipeline :created_at)))))

(defun ygg-git-ci--parse-gitlab-jobs (text pipeline)
  "The jobs in TEXT, the API's job list of PIPELINE, as rows."
  (ygg-git-ci--gitlab-rows (ygg-git-ci--json text) pipeline))

(defun ygg-git-ci--gitlab-rows (jobs pipeline)
  "The rows of JOBS, parsed job plists of PIPELINE."
  (if (null jobs)
      (list (list :id (plist-get pipeline :id) :kind 'pipeline
                  :name (format "pipeline #%s" (plist-get pipeline :id))
                  :state (plist-get pipeline :state)
                  :created (plist-get pipeline :created)
                  :sha (plist-get pipeline :sha) :url (plist-get pipeline :url)
                  :pipeline (plist-get pipeline :id)))
    (mapcar (lambda (job)
              (list :id (plist-get job :id) :kind 'job
                    :name (or (plist-get job :name) "job")
                    :stage (plist-get job :stage)
                    :state (ygg-git-ci--gitlab-state (plist-get job :status)
                                                     (plist-get job :allow_failure))
                    :created (ygg-git-review-requests--time (plist-get job :created_at))
                    :started (ygg-git-review-requests--time (plist-get job :started_at))
                    :finished (ygg-git-review-requests--time (plist-get job :finished_at))
                    :playable (and (member (plist-get job :status) '("manual" "scheduled")) t)
                    :sha (plist-get pipeline :sha)
                    :url (plist-get job :web_url)
                    :pipeline (plist-get pipeline :id)))
            jobs)))

(defun ygg-git-ci--active-p (answer)
  (seq-some (lambda (row) (memq (plist-get row :state) '(running pending)))
            (plist-get answer :rows)))

(defun ygg-git-ci--visible-buffer (key)
  (seq-find (lambda (buffer)
              (and (buffer-live-p buffer)
                   (get-buffer-window buffer t)))
            (gethash key ygg-git-ci--buffers)))

(defun ygg-git-ci--poll (key)
  (remhash key ygg-git-ci--timers)
  (when-let* ((buffer (ygg-git-ci--visible-buffer key))
              ((not (gethash key ygg-git-ci--running)))
              ((ygg-git-ci--active-p (cdr (gethash key ygg-git-ci--cache)))))
    (with-current-buffer buffer
      (ygg-git-ci--ensure key t))))

(defun ygg-git-ci--arm (key)
  (when (and (not (gethash key ygg-git-ci--timers))
             (ygg-git-ci--active-p (cdr (gethash key ygg-git-ci--cache)))
             (ygg-git-ci--visible-buffer key))
    (puthash key (run-at-time ygg-git-ci-poll-interval nil #'ygg-git-ci--poll key)
             ygg-git-ci--timers)))

(defun ygg-git-ci--prune (key)
  (puthash key (seq-filter #'buffer-live-p (gethash key ygg-git-ci--buffers))
           ygg-git-ci--buffers))

(defvar ygg-git-ci--rendering nil)

(defun ygg-git-ci--refresh (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (with-demoted-errors "ci: %S" (magit-refresh-buffer)))))

(defun ygg-git-ci--store (key result &optional cell)
  (let* ((cell (or cell (gethash key ygg-git-ci--running)))
         (previous (cdr (gethash key ygg-git-ci--cache)))
         (buffers (gethash key ygg-git-ci--watchers)))
    (when (and (plist-get result :error) (not (plist-member result :rows))
               (plist-member previous :rows))
      (setq result (list :rows (plist-get previous :rows)
                         :error (plist-get result :error))))
    (when (timerp (nth 2 cell))
      (cancel-timer (nth 2 cell)))
    (puthash key (cons (float-time) result) ygg-git-ci--cache)
    (remhash key ygg-git-ci--running)
    (remhash key ygg-git-ci--watchers)
    (ygg-git-ci--prune key)
    (dolist (buffer buffers)
      (if ygg-git-ci--rendering
          (run-at-time 0 nil #'ygg-git-ci--refresh buffer)
        (ygg-git-ci--refresh buffer)))
    (ygg-git-ci--arm key)
    (let ((waiting (nreverse (gethash key ygg-git-ci--waiters))))
      (remhash key ygg-git-ci--waiters)
      (dolist (continuation waiting)
        (with-demoted-errors "ci: %S" (funcall continuation))))))

(defun ygg-git-ci--land (key program status text stderr parser &optional partial)
  "Store what PARSER makes of TEXT; a failure keeps the PARTIAL rows."
  (let ((result (cond ((null status) (list :error (format "%s not found" program)))
                      ((/= status 0)
                       (list :error (ygg-git-review-requests--failure program status stderr)))
                      (t (condition-case nil
                             (funcall parser text)
                           (error (list :error (format "%s answered nothing readable"
                                                       program))))))))
    (when (and partial (plist-get result :error))
      (setq result (list :rows partial :error (plist-get result :error))))
    (ygg-git-ci--store key result)))

(defun ygg-git-ci--step (key cell command callback)
  (let ((process (ygg-git-review-requests--spawn
                  command
                  (lambda (status text &optional stderr)
                    (when (eq cell (gethash key ygg-git-ci--running))
                      (funcall callback status text stderr))))))
    (when (and process (eq cell (gethash key ygg-git-ci--running)))
      (setf (nth 1 cell) process))))

(defun ygg-git-ci--abandon (key)
  (when-let* ((cell (gethash key ygg-git-ci--running)))
    (remhash key ygg-git-ci--running)
    (when (timerp (nth 2 cell))
      (cancel-timer (nth 2 cell)))
    (when (processp (nth 1 cell))
      (ygg-git-review-requests--kill (nth 1 cell)))))

(defun ygg-git-ci--expire (key cell)
  (when (eq cell (gethash key ygg-git-ci--running))
    (ygg-git-ci--abandon key)
    (ygg-git-ci--store
     key (list :error (format "%s timed out" (ygg-git-ci--program (car key)))) cell)))

(defun ygg-git-ci--list (key cell)
  (pcase-let* ((`(,repo ,branch) key)
               (program (ygg-git-ci--program repo)))
    (pcase (car repo)
      ('github
       (ygg-git-ci--step
        key cell
        (list "gh" "run" "list" "--repo" (ygg-git-ci--slug repo) "--branch" branch
              "--limit" "100" "--json" ygg-git-ci--github-fields)
        (lambda (status text stderr)
          (ygg-git-ci--land key program status text stderr
                            (lambda (text) (list :rows (ygg-git-ci--parse-github text)))))))
      ('gitlab
       (ygg-git-ci--step
        key cell
        (list "glab" "api" "--hostname" (nth 1 repo)
              (format "%s/pipelines?ref=%s&per_page=1"
                      (ygg-git-ci--project repo) (url-hexify-string branch)))
        (lambda (status text stderr)
          (let ((pipeline (and (eql status 0)
                               (ignore-errors (ygg-git-ci--parse-gitlab-pipeline text)))))
            (if (not pipeline)
                (ygg-git-ci--land key program status text stderr
                                  (lambda (text)
                                    (ygg-git-ci--parse-gitlab-pipeline text)
                                    (list :rows nil)))
              (ygg-git-ci--jobs-page key cell pipeline 1 nil)))))))))

(defun ygg-git-ci--jobs-page (key cell pipeline page earlier)
  "Ask for PAGE of PIPELINE's jobs, after EARLIER ones; on to the next while full."
  (let* ((repo (car key))
         (program (ygg-git-ci--program repo)))
    (ygg-git-ci--step
     key cell
     (list "glab" "api" "--hostname" (nth 1 repo)
           (format "%s/pipelines/%s/jobs?per_page=%d&page=%d"
                   (ygg-git-ci--project repo) (plist-get pipeline :id)
                   ygg-git-ci--page-size page))
     (lambda (status text stderr)
       (let ((jobs (and (eql status 0) (ignore-errors (ygg-git-ci--json text)))))
         (if (and (length= jobs ygg-git-ci--page-size) (< page ygg-git-ci--max-pages))
             (ygg-git-ci--jobs-page key cell pipeline (1+ page) (append earlier jobs))
           (ygg-git-ci--land
            key program status text stderr
            (lambda (text)
              (list :rows (ygg-git-ci--gitlab-rows
                           (append earlier (ygg-git-ci--json text)) pipeline)))
            (and earlier (ygg-git-ci--gitlab-rows earlier pipeline)))))))))

(defun ygg-git-ci--start (key)
  (let ((cell (list (float-time) nil nil))
        (started nil))
    (puthash key cell ygg-git-ci--running)
    (unwind-protect
        (progn
          (setf (nth 2 cell)
                (run-at-time ygg-git-ci-timeout nil #'ygg-git-ci--expire key cell))
          (ygg-git-ci--list key cell)
          (setq started t))
      (unless started
        (when (eq cell (gethash key ygg-git-ci--running))
          (ygg-git-ci--abandon key))))))

(defun ygg-git-ci--ensure (key &optional force)
  "The cached answer for KEY, asking the forge when it is stale or FORCE
and no question is already out; the cache stays until the new answer lands."
  (let ((entry (gethash key ygg-git-ci--cache)))
    (when (derived-mode-p 'magit-mode)
      (cl-pushnew (current-buffer) (gethash key ygg-git-ci--watchers))
      (cl-pushnew (current-buffer) (gethash key ygg-git-ci--buffers)))
    (ygg-git-ci--prune key)
    (when force
      (ygg-git-ci--abandon key))
    (when (and (not (gethash key ygg-git-ci--running))
               (or force (null entry)
                   (> (- (float-time) (car entry)) ygg-git-ci-ttl)))
      (ygg-git-ci--start key))
    (cdr (gethash key ygg-git-ci--cache))))

(defun ygg-git-ci--await (key continuation)
  "Call CONTINUATION once KEY has a fresh answer: now, or when the fetch lands."
  (let ((entry (gethash key ygg-git-ci--cache)))
    (if (and entry (<= (- (float-time) (car entry)) ygg-git-ci-ttl))
        (funcall continuation)
      (push continuation (gethash key ygg-git-ci--waiters))
      (condition-case err
          (ygg-git-ci--ensure key)
        (error
         (setf (gethash key ygg-git-ci--waiters)
               (delq continuation (gethash key ygg-git-ci--waiters)))
         (signal (car err) (cdr err)))))))

(defun ygg-git-ci--context ()
  "What the section is about: (KEY) for a branch to ask about, else a note."
  (let ((repo (ygg-git-review-requests--repo))
        (branch (magit-get-current-branch)))
    (cond ((null repo) nil)
          ((null branch) (list :note "detached HEAD"))
          ((null (magit-get-upstream-branch branch)) (list :note "no upstream"))
          (t (list :key (list repo branch))))))

(defun ygg-git-ci--key ()
  (let ((context (ygg-git-ci--context)))
    (cond ((plist-get context :key))
          ((plist-get context :note) (user-error "ci: %s" (plist-get context :note)))
          (t (user-error "ci: not a forge remote")))))

(defun ygg-git-ci--row-line (row head)
  (pcase-let* ((state (plist-get row :state))
               (`(,mark ,face) (if (plist-get row :playable)
                                   '("▶" link)
                                 (cdr (assq state ygg-git-compare--check-marks))))
               (stale (and head (plist-get row :sha) (not (equal head (plist-get row :sha)))))
               (age (ygg-git-review-requests--age (plist-get row :created)))
               (details (delq nil (list (ygg-git-compare--check-duration row)
                                        (and (not (string-empty-p age)) age)
                                        (plist-get row :event)))))
    (concat (propertize mark 'font-lock-face face) " "
            (propertize (plist-get row :name)
                        'font-lock-face (if stale 'magit-dimmed 'default))
            "  "
            (propertize (string-join details "  ") 'font-lock-face 'magit-dimmed)
            (if stale (propertize "  stale" 'font-lock-face 'warning) ""))))

(defun ygg-git-ci--insert-row (row head)
  (magit-insert-section (ci-run row)
    (magit-insert-heading
      "  " (ygg-git-review-requests--fit (ygg-git-ci--row-line row head)))))

(defun ygg-git-ci--keys (repo)
  (if (eq (car repo) 'github)
      "r rerun  x cancel  l log  t run"
    "r retry  x cancel  p play  R/X pipeline  l log  t start"))

(defun ygg-git-ci--heading (branch &optional tail hint)
  (magit-insert-heading
    (concat "CI  " (propertize branch 'font-lock-face 'magit-branch-local)
            (if tail (concat "  " tail) "")
            (if hint (propertize (concat "  " hint) 'font-lock-face 'magit-dimmed) ""))))

;;;###autoload
(defun ygg-git-ci-insert-section ()
  "Insert the CI runs of the current branch, from the last answer."
  (when ygg-git-ci-status
    (condition-case err
        (let ((context (ygg-git-ci--context)))
          (cond
           ((plist-get context :note)
            (magit-insert-section (ci)
              (magit-insert-heading "CI")
              (ygg-git-review-requests--note (format "ci: %s" (plist-get context :note)))))
           ((plist-get context :key)
            (let* ((key (plist-get context :key))
                   (branch (cadr key))
                   (answer (let ((ygg-git-ci--rendering t))
                             (ygg-git-ci--ensure key)))
                   (rows (plist-get answer :rows)))
              (ygg-git-ci--arm key)
              (magit-insert-section (ci)
                (cond
                 ((null answer)
                  (ygg-git-ci--heading branch "(fetching…)")
                  (insert ?\n))
                 ((and (plist-get answer :error) (not (plist-member answer :rows)))
                  (ygg-git-ci--heading branch)
                  (ygg-git-review-requests--note
                   (format "ci: %s" (plist-get answer :error))))
                 (t
                  (ygg-git-ci--heading
                   branch (if rows
                              (ygg-git-compare--checks-summary rows)
                            (propertize "no runs" 'font-lock-face 'magit-dimmed))
                   (ygg-git-ci--keys (car key)))
                  (let ((head (ignore-errors (magit-rev-parse "HEAD"))))
                    (dolist (row rows)
                      (ygg-git-ci--insert-row row head)))
                  (if (plist-get answer :error)
                      (ygg-git-review-requests--note
                       (format "ci: %s (showing last runs)" (plist-get answer :error)))
                    (insert ?\n))))))))
          )
      (error
       (ygg-git-review-requests--note (format "ci: %s" (error-message-string err)))))))

(defun ygg-git-ci--at-point ()
  (or (magit-section-value-if 'ci-run)
      (user-error "No CI run at point")))

(defun ygg-git-ci--command (action repo row &optional extra)
  "The command doing ACTION to ROW on REPO's forge."
  (let ((id (plist-get row :id)))
    (if (eq (car repo) 'github)
        (pcase action
          ('rerun (list "gh" "run" "rerun" (format "%s" id) "--repo" (ygg-git-ci--slug repo)))
          ('rerun-failed (list "gh" "run" "rerun" (format "%s" id) "--failed"
                               "--repo" (ygg-git-ci--slug repo)))
          ('cancel (list "gh" "run" "cancel" (format "%s" id) "--repo" (ygg-git-ci--slug repo)))
          ('log (list "gh" "run" "view" (format "%s" id)
                      (if (memq (plist-get row :state) '(failed cancelled)) "--log-failed" "--log")
                      "--repo" (ygg-git-ci--slug repo))))
      (let ((project (ygg-git-ci--project repo))
            (api (list "glab" "api" "--hostname" (nth 1 repo))))
        (pcase action
          ('rerun (append api (list "--method" "POST"
                                    (if (eq (plist-get row :kind) 'pipeline)
                                        (format "%s/pipelines/%s/retry" project id)
                                      (format "%s/jobs/%s/retry" project id)))))
          ('cancel (append api (list "--method" "POST"
                                     (if (eq (plist-get row :kind) 'pipeline)
                                         (format "%s/pipelines/%s/cancel" project id)
                                       (format "%s/jobs/%s/cancel" project id)))))
          ('rerun-pipeline (append api (list "--method" "POST"
                                             (format "%s/pipelines/%s/retry" project
                                                     (plist-get row :pipeline)))))
          ('cancel-pipeline (append api (list "--method" "POST"
                                              (format "%s/pipelines/%s/cancel" project
                                                      (plist-get row :pipeline)))))
          ('play (append api (list "--method" "POST" (format "%s/jobs/%s/play" project id))
                         (and extra
                              (list "--header" "Content-Type: application/json"
                                    "--input" (let ((coding-system-for-write 'utf-8))
                                                (make-temp-file "ygg-git-ci-body-" nil ".json"
                                                                extra))))))
          ('log (if (eq (plist-get row :kind) 'pipeline)
                    (user-error "No job log for a pipeline row")
                  (append api (list (format "%s/jobs/%s/trace" project id))))))))))

(defun ygg-git-ci--do (key label command &optional prompt)
  "Run COMMAND after PROMPT is agreed to, say how LABEL went, then refetch."
  (let* ((input (cadr (member "--input" command)))
         (buffer (current-buffer))
         (default-directory (or (magit-toplevel) default-directory))
         (running nil)
         (timer nil)
         (cleanup (lambda () (when input (ignore-errors (delete-file input))))))
    (unwind-protect
        (when (or (null prompt) (y-or-n-p prompt))
          (message "ci: %s…" label)
          (setq running t)
          (let* ((finished nil)
                 (process
                  (ygg-git-review-requests--spawn
                   command
                   (lambda (status _text &optional stderr)
                     (setq finished t)
                     (when (timerp timer) (cancel-timer timer))
                     (funcall cleanup)
                     (message "%s" (if (eql status 0)
                                       (format "ci: %s done" label)
                                     (format "ci: %s failed: %s" label
                                             (ygg-git-review-requests--failure
                                              (car command) status stderr))))
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (ygg-git-ci--ensure key t)
                         (when (derived-mode-p 'magit-mode)
                           (magit-refresh-buffer))))))))
            (unless finished
              (setq timer
                    (run-at-time ygg-git-ci-timeout nil
                                 (lambda ()
                                   (when process (ygg-git-review-requests--kill process))
                                   (funcall cleanup)
                                   (message "ci: %s timed out" label)))))))
      (unless running (funcall cleanup)))))

(defun ygg-git-ci-browse ()
  "Open the run at point in the browser."
  (interactive)
  (browse-url (plist-get (ygg-git-ci--at-point) :url)))

(defcustom ygg-git-ci-log-buffers 5
  "How many CI log buffers are kept; older ones not on screen are killed."
  :type 'natnum :group 'ygg-git-ci)

(defvar-local ygg-git-ci--log-p nil)

(defun ygg-git-ci--gc-logs ()
  "Kill the CI log buffers past `ygg-git-ci-log-buffers', least recently shown first."
  (let ((logs (sort (seq-filter (lambda (b) (buffer-local-value 'ygg-git-ci--log-p b))
                                (buffer-list))
                    (lambda (a b)
                      (time-less-p (or (buffer-local-value 'buffer-display-time b) 0)
                                   (or (buffer-local-value 'buffer-display-time a) 0))))))
    (dolist (buffer (nthcdr ygg-git-ci-log-buffers logs))
      (unless (get-buffer-window buffer t)
        (kill-buffer buffer)))))

(defun ygg-git-ci-log ()
  "Show the log of the run at point."
  (interactive)
  (let* ((key (ygg-git-ci--key))
         (row (ygg-git-ci--at-point))
         (name (plist-get row :name))
         (default-directory (or (magit-toplevel) default-directory)))
    (message "ci: reading the log of %s…" name)
    (ygg-git-review-requests--spawn
     (ygg-git-ci--command 'log (car key) row)
     (lambda (status text &optional stderr)
       (if (not (eql status 0))
           (message "ci: log failed: %s"
                    (ygg-git-review-requests--failure
                     (ygg-git-ci--program (car key)) status stderr))
         (with-current-buffer (get-buffer-create (format "*ci log: %s*" name))
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert text)
             (ansi-color-apply-on-region (point-min) (point-max)))
           (special-mode)
           (setq ygg-git-ci--log-p t)
           (goto-char (point-min))
           (pop-to-buffer (current-buffer))
           (ygg-git-ci--gc-logs)))))))

(defun ygg-git-ci-rerun ()
  "Rerun the run at point; a failed GitHub run may rerun only its failed jobs."
  (interactive)
  (let* ((key (ygg-git-ci--key))
         (row (ygg-git-ci--at-point))
         (name (plist-get row :name)))
    (if (and (eq (car (car key)) 'github)
             (memq (plist-get row :state) '(failed cancelled))
             (y-or-n-p (format "Rerun only the failed jobs of %s? " name)))
        (ygg-git-ci--do key (format "rerun failed jobs of %s" name)
                        (ygg-git-ci--command 'rerun-failed (car key) row))
      (ygg-git-ci--do key (format "rerun %s" name)
                      (ygg-git-ci--command 'rerun (car key) row)
                      (format "Rerun %s? " name)))))

(defun ygg-git-ci-cancel ()
  "Cancel the run at point."
  (interactive)
  (let* ((key (ygg-git-ci--key))
         (row (ygg-git-ci--at-point))
         (name (plist-get row :name)))
    (unless (memq (plist-get row :state) '(running pending))
      (user-error "%s is not running" name))
    (ygg-git-ci--do key (format "cancel %s" name)
                    (ygg-git-ci--command 'cancel (car key) row)
                    (format "Cancel %s? " name))))

(defun ygg-git-ci-rerun-pipeline ()
  "Retry the whole GitLab pipeline of the job at point."
  (interactive)
  (let ((key (ygg-git-ci--key)))
    (unless (eq (car (car key)) 'gitlab)
      (user-error "Only GitLab has pipelines of jobs"))
    (ygg-git-ci--do key "retry the pipeline"
                    (ygg-git-ci--command 'rerun-pipeline (car key) (ygg-git-ci--at-point))
                    "Retry the whole pipeline? ")))

(defun ygg-git-ci-cancel-pipeline ()
  "Cancel the whole GitLab pipeline of the job at point."
  (interactive)
  (let ((key (ygg-git-ci--key)))
    (unless (eq (car (car key)) 'gitlab)
      (user-error "Only GitLab has pipelines of jobs"))
    (ygg-git-ci--do key "cancel the pipeline"
                    (ygg-git-ci--command 'cancel-pipeline (car key) (ygg-git-ci--at-point))
                    "Cancel the whole pipeline? ")))

(defun ygg-git-ci--local-dispatch (directory)
  "The workflows under DIRECTORY that can be started by hand, as (NAME . FILE)."
  (let (found)
    (dolist (file (directory-files (expand-file-name ".github/workflows" directory)
                                   t "\\.ya?ml\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^[^#\n]*\\bworkflow_dispatch\\b" nil t)
          (goto-char (point-min))
          (push (cons (if (re-search-forward "^name:[ \t]*\\(.+?\\)[ \t]*$" nil t)
                          (string-trim (match-string 1) "[\"']" "[\"']")
                        (concat ".github/workflows/" (file-name-nondirectory file)))
                      file)
                found))))
    found))

(defun ygg-git-ci--dispatchable (text directory)
  "The active workflows of TEXT, gh's workflow list, that DIRECTORY lets run by
hand, each with its :file."
  (let ((local (ignore-errors (ygg-git-ci--local-dispatch directory))))
    (seq-keep (lambda (workflow)
                (when-let* (((equal (plist-get workflow :state) "active"))
                            (hit (assoc (plist-get workflow :name) local)))
                  (append workflow (list :file (cdr hit)))))
              (ygg-git-ci--json text))))

(defun ygg-git-ci--unquote (text)
  (string-trim (string-trim text) "[\"']" "[\"']"))

(defun ygg-git-ci--inputs (file)
  "FILE's dispatch inputs: plists of name, default, options, description."
  (let* ((lines (with-temp-buffer
                  (insert-file-contents file)
                  (seq-keep (lambda (line)
                              (unless (string-match-p "\\`[ \t]*\\(?:#.*\\)?\\'" line)
                                (cons (- (length line) (length (string-trim-left line)))
                                      (string-trim line))))
                            (split-string (buffer-string) "\n"))))
         (nested (lambda (lines prefix)
                   (when-let* ((tail (seq-drop-while
                                      (lambda (l) (not (string-prefix-p prefix (cdr l))))
                                      lines)))
                     (let ((indent (car (car tail))))
                       (seq-take-while (lambda (l) (> (car l) indent)) (cdr tail))))))
         (inputs (funcall nested (funcall nested lines "workflow_dispatch:") "inputs:"))
         groups)
    (when inputs
      (dolist (line inputs)
        (if (= (car line) (car (car inputs)))
            (push (list line) groups)
          (push line (car groups))))
      (mapcar
       (lambda (group)
         (let* ((group (reverse group))
                (attributes (cdr group))
                (field (lambda (key)
                         (when-let* ((hit (seq-find (lambda (l)
                                                      (string-prefix-p (concat key ":") (cdr l)))
                                                    attributes)))
                           (ygg-git-ci--unquote
                            (substring (cdr hit) (1+ (length key))))))))
           (list :name (string-remove-suffix ":" (cdr (car group)))
                 :default (funcall field "default")
                 :description (funcall field "description")
                 :options
                 (let ((inline (funcall field "options")))
                   (if (and inline (string-prefix-p "[" inline))
                       (mapcar #'ygg-git-ci--unquote
                               (split-string (string-trim inline "\\[" "\\]") "," t))
                     (mapcar (lambda (l) (ygg-git-ci--unquote (substring (cdr l) 2)))
                             (seq-filter (lambda (l) (string-prefix-p "- " (cdr l)))
                                         (funcall nested attributes "options:"))))))))
       (nreverse groups)))))

(defun ygg-git-ci--read-inputs (file)
  "Ask for each input of FILE; the \"key=value\" strings of those given a value."
  (delq nil
        (mapcar
         (lambda (input)
           (let* ((name (plist-get input :name))
                  (prompt (format "%s%s: " name
                                  (if-let* ((d (plist-get input :description)))
                                      (format " (%s)" d)
                                    "")))
                  (value (if (plist-get input :options)
                             (completing-read prompt (plist-get input :options) nil t nil nil
                                              (plist-get input :default))
                           (read-string prompt (plist-get input :default)))))
             (and (not (string-empty-p value)) (concat name "=" value))))
         (ygg-git-ci--inputs file))))

(defun ygg-git-ci--read-variables ()
  "KEY:VALUE strings asked for until an empty answer."
  (let (variables entry)
    (while (not (string-empty-p
                 (setq entry (read-string "Variable KEY:VALUE (empty to go on): "))))
      (push entry variables))
    (nreverse variables)))

(defun ygg-git-ci--variable-pair (variable)
  (let ((at (string-search ":" variable)))
    (unless (and at (> at 0))
      (user-error "Variable %S is not KEY:VALUE" variable))
    (list :key (substring variable 0 at) :value (substring variable (1+ at)))))

(defun ygg-git-ci--variables-json (variables)
  "The play request body for the KEY:VALUE strings VARIABLES."
  (json-serialize
   (list :job_variables_attributes
         (vconcat (mapcar #'ygg-git-ci--variable-pair variables)))))

(defun ygg-git-ci--startables (key rows workflows)
  "What can be started on KEY's branch as (LABEL . SPEC): manual jobs among ROWS,
the dispatchable WORKFLOWS, and on GitLab a new pipeline."
  (let* ((entries
          (append
           (mapcar (lambda (row)
                     (list (format "job: %s" (plist-get row :name))
                           (format "%s#%s" (if (plist-get row :stage)
                                               (format "stage %s, " (plist-get row :stage))
                                             "")
                                   (plist-get row :id))
                           (list :kind 'job :name (plist-get row :name) :row row)))
                   (seq-filter (lambda (row) (plist-get row :playable)) rows))
           (mapcar (lambda (workflow)
                     (list (format "workflow: %s" (plist-get workflow :name))
                           (format "#%s" (plist-get workflow :id))
                           (list :kind 'workflow :name (plist-get workflow :name)
                                 :workflow workflow)))
                   workflows)
           (when (eq (car (car key)) 'gitlab)
             (list (list (format "new pipeline: %s" (cadr key)) nil
                         (list :kind 'pipeline :name (cadr key))))))))
    (mapcar (lambda (entry)
              (cons (if (and (cadr entry)
                             (> (seq-count (lambda (other) (equal (car other) (car entry)))
                                           entries)
                                1))
                        (format "%s (%s)" (car entry) (cadr entry))
                      (car entry))
                    (nth 2 entry)))
            entries)))

(defun ygg-git-ci--start-spec (key spec)
  (let ((repo (car key)) (branch (cadr key)))
    (pcase (plist-get spec :kind)
      ('job
       (let* ((row (plist-get spec :row))
              (variables (ygg-git-ci--read-variables)))
         (ygg-git-ci--do key (format "play %s" (plist-get row :name))
                         (ygg-git-ci--command 'play repo row
                                              (and variables (ygg-git-ci--variables-json variables)))
                         (format "Play %s? " (plist-get row :name)))))
      ('pipeline
       (let ((variables (ygg-git-ci--read-variables)))
         (mapc #'ygg-git-ci--variable-pair variables)
         (ygg-git-ci--do key (format "run a pipeline on %s" branch)
                         (append (list "glab" "ci" "run" "-R" (ygg-git-ci--glab-repo repo)
                                       "--branch" branch)
                                 (mapcan (lambda (v) (list "--variables" v)) variables))
                         (format "Run a pipeline on %s? " branch))))
      ('workflow
       (let* ((workflow (plist-get spec :workflow))
              (inputs (ygg-git-ci--read-inputs (plist-get workflow :file))))
         (ygg-git-ci--do key (format "run %s" (plist-get workflow :name))
                         (append (list "gh" "workflow" "run" (format "%s" (plist-get workflow :id))
                                       "--repo" (ygg-git-ci--slug repo) "--ref" branch)
                                 (mapcan (lambda (i) (list "-f" i)) inputs))
                         (format "Run %s on %s? " (plist-get workflow :name) branch)))))))

(defun ygg-git-ci-startable (root callback)
  "Call CALLBACK once with what can be started on the current branch of ROOT.
That is plists of :kind (job, workflow or pipeline), :name and :label, to give
`ygg-git-ci-start'; a second argument says why the forge could not be asked.
GitLab's manual jobs are those of the latest pipeline, fetched when the cache
is missing or stale."
  (let* ((default-directory root)
         (key (condition-case err
                  (ygg-git-ci--key)
                (error (funcall callback nil (error-message-string err)) nil)))
         (finish (lambda (workflows)
                   (apply callback
                          (condition-case err
                              (let* ((answer (cdr (gethash key ygg-git-ci--cache)))
                                     (rows (plist-get answer :rows)))
                                (when (and (eq (car (car key)) 'gitlab)
                                           (plist-get answer :error)
                                           (not (plist-member answer :rows)))
                                  (error "%s" (plist-get answer :error)))
                                (list (mapcar (lambda (choice)
                                                (append (list :label (car choice)) (cdr choice)))
                                              (ygg-git-ci--startables
                                               key rows (funcall workflows)))))
                            (error (list nil (error-message-string err))))))))
    (when key
      (if (eq (car (car key)) 'gitlab)
          (condition-case err
              (ygg-git-ci--await key (lambda () (funcall finish #'ignore)))
            (error (funcall callback nil (error-message-string err))))
        (let ((directory (or (magit-toplevel) default-directory)))
          (message "ci: listing workflows…")
          (ygg-git-review-requests--spawn
           (list "gh" "workflow" "list" "--repo" (ygg-git-ci--slug (car key))
                 "--limit" "200" "--json" "name,id,state")
           (lambda (status text &optional stderr)
             (if (eql status 0)
                 (funcall finish (lambda () (ygg-git-ci--dispatchable text directory)))
               (funcall callback nil (ygg-git-review-requests--failure "gh" status stderr))))))))))

(defun ygg-git-ci-start (root item)
  "Confirm and start ITEM, from `ygg-git-ci-startable', on ROOT's current branch."
  (let ((default-directory root))
    (let ((key (ygg-git-ci--key)))
      (with-current-buffer (or (and (not (derived-mode-p 'magit-mode))
                                    (magit-get-mode-buffer 'magit-status-mode))
                               (current-buffer))
        (ygg-git-ci--start-spec key item)))))

(defun ygg-git-ci--choose (root key items err)
  (cond
   (err (message "ci: %s" err))
   ((null items) (message "ci: nothing here can be started by hand"))
   (t (let ((choice (completing-read (format "Start on %s: " (cadr key))
                                     (mapcar (lambda (item) (plist-get item :label)) items)
                                     nil t)))
        (ygg-git-ci-start root (seq-find (lambda (item) (equal (plist-get item :label) choice))
                                         items))))))

(defun ygg-git-ci-trigger ()
  "Start something on the current branch: a manual job, a workflow, a pipeline.
On a playable job it plays that job."
  (interactive)
  (let ((key (ygg-git-ci--key))
        (row (magit-section-value-if 'ci-run))
        (root (or (magit-toplevel) default-directory))
        (buffer (current-buffer)))
    (if (plist-get row :playable)
        (ygg-git-ci--start-spec key (list :kind 'job :row row))
      (ygg-git-ci-startable
       root
       (lambda (items &optional err)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (ygg-git-ci--choose root key items err))))))))

(defun ygg-git-ci-play ()
  "Play the manual job at point."
  (interactive)
  (let ((row (ygg-git-ci--at-point)))
    (unless (plist-get row :playable)
      (user-error "%s cannot be played" (plist-get row :name)))
    (ygg-git-ci--start-spec (ygg-git-ci--key) (list :kind 'job :row row))))

(defun ygg-git-ci-refetch ()
  "Ask the forge for the runs now."
  (interactive)
  (ygg-git-ci--ensure (ygg-git-ci--key) t)
  (magit-refresh-buffer))

(defvar-keymap magit-ci-section-map
  "g" #'ygg-git-ci-refetch
  "t" #'ygg-git-ci-trigger)

(defvar-keymap magit-ci-run-section-map
  :parent magit-ci-section-map
  "RET" #'ygg-git-ci-browse
  "o" #'ygg-git-ci-browse
  "l" #'ygg-git-ci-log
  "p" #'ygg-git-ci-play
  "r" #'ygg-git-ci-rerun
  "x" #'ygg-git-ci-cancel
  "R" #'ygg-git-ci-rerun-pipeline
  "X" #'ygg-git-ci-cancel-pipeline)

(provide 'ygg-git-ci)
;;; ygg-git-ci.el ends here
