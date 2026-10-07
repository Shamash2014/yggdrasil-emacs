;;; ygg-git-compare-pr-info-tests.el --- conversation and checks in a compare -*- lexical-binding: t; -*-

;;; Code:

(let ((builds (expand-file-name "../elpaca/builds/"
                                (file-name-directory
                                 (or load-file-name buffer-file-name)))))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds))))

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-compare-comments)
(require 'ygg-git-compare-threads)
(require 'ygg-git-compare-pr-info)

(defvar ygg-git-compare-pr-info-tests--browsed nil)

(defconst ygg-git-compare-pr-info-tests--gh
  "#!/bin/sh
echo \"gh $*\" >> \"$FAKE_DIR/log\"
reply() { if [ -f \"$FAKE_DIR/$1.err\" ]; then cat \"$FAKE_DIR/$1.err\" >&2; exit 1; fi
  if [ -f \"$FAKE_DIR/$1.json\" ]; then cat \"$FAKE_DIR/$1.json\"; else echo '[]'; fi; }
case \"$*\" in
  *statusCheckRollup*) reply checks ;;
  *graphql*) reply graphql ;;
  *pulls/*/comments*) reply inline ;;
  *pulls/*/reviews*) reply reviews ;;
  *issues/*/comments*) reply notes ;;
  *) echo '[]' ;;
esac
")

(defconst ygg-git-compare-pr-info-tests--glab
  "#!/bin/sh
echo \"glab $*\" >> \"$FAKE_DIR/log\"
reply() { if [ -f \"$FAKE_DIR/$1.err\" ]; then cat \"$FAKE_DIR/$1.err\" >&2; exit 1; fi
  if [ -f \"$FAKE_DIR/$1.json\" ]; then cat \"$FAKE_DIR/$1.json\"; else echo '[]'; fi; }
case \"$*\" in
  *jobs*) reply jobs ;;
  *discussions*) reply discussions ;;
  *merge_requests/12) reply mr ;;
  *) echo '[]' ;;
esac
")

(defvar ygg-git-compare-pr-info-tests--fake nil)

(defun ygg-git-compare-pr-info-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-compare-pr-info-tests--put (name text)
  (with-temp-file (expand-file-name name ygg-git-compare-pr-info-tests--fake)
    (insert text)))

(defun ygg-git-compare-pr-info-tests--json (&rest values)
  (json-serialize (vconcat values)))

(defun ygg-git-compare-pr-info-tests--log ()
  (let ((file (expand-file-name "log" ygg-git-compare-pr-info-tests--fake)))
    (when (file-exists-p file)
      (with-temp-buffer (insert-file-contents file) (split-string (buffer-string) "\n" t)))))

(defun ygg-git-compare-pr-info-tests--calls (needle)
  (length (seq-filter (lambda (line) (string-search needle line))
                      (ygg-git-compare-pr-info-tests--log))))

(defmacro ygg-git-compare-pr-info-tests--with-repo (head &rest body)
  "BODY in a repo on main whose feature branch is HEAD, with fake gh and glab
first on PATH answering from the files ygg-git-compare-pr-info-tests--put wrote."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory
                 (file-truename (make-temp-file "ygg-git-compare-pr-info-" t))))
          (fake (file-name-as-directory (expand-file-name "fake" root)))
          (work (file-name-as-directory (expand-file-name "work" root)))
          (ygg-git-compare-pr-info-tests--fake fake)
          (process-environment
           (append (list "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                         (concat "PATH=" (directory-file-name fake) ":" (getenv "PATH"))
                         (concat "FAKE_DIR=" fake))
                   process-environment))
          (exec-path (cons (directory-file-name fake) exec-path))
          (default-directory work)
          (magit-refresh-verbose nil)
          (ygg-git-compare--caches (make-hash-table :test #'equal))
          (ygg-git-compare--inflight (make-hash-table :test #'equal))
          (ygg-git-compare--forge-repos (make-hash-table :test #'equal))
          (ygg-git-compare-threads-width 80)
          (kill-ring nil)
          (ygg-git-compare-pr-info-tests--browsed nil)
          (,head nil))
     (unwind-protect
         (progn
           (make-directory fake t)
           (make-directory work t)
           (ygg-git-compare-pr-info-tests--put "gh" ygg-git-compare-pr-info-tests--gh)
           (ygg-git-compare-pr-info-tests--put "glab" ygg-git-compare-pr-info-tests--glab)
           (set-file-modes (expand-file-name "gh" fake) #o755)
           (set-file-modes (expand-file-name "glab" fake) #o755)
           (cl-flet ((git (&rest args) (apply #'ygg-git-compare-pr-info-tests--git work args))
                     (write (file text) (with-temp-file (expand-file-name file work)
                                          (insert text))))
             (git "init" "-q" "-b" "main")
             (git "config" "user.name" "Info Test")
             (git "config" "user.email" "info@example.invalid")
             (git "config" "commit.gpgsign" "false")
             (write "a.txt" "1\n2\n3\n4\n5\n")
             (git "add" ".")
             (git "commit" "-q" "-m" "base")
             (git "checkout" "-q" "-b" "feature")
             (write "a.txt" "1\ntwo\nthree\n4\n5\n")
             (git "add" ".")
             (git "commit" "-q" "-m" "feature")
             (setq ,head (git "rev-parse" "HEAD"))
             (git "checkout" "-q" "main"))
           (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq ygg-git-compare-pr-info-tests--browsed url))))
             ,@body))
       (dolist (b (buffer-list))
         (when (and (with-current-buffer b (derived-mode-p 'magit-mode))
                    (file-in-directory-p (buffer-local-value 'default-directory b) root))
           (kill-buffer b)))
       (delete-directory root t))))

(defun ygg-git-compare-pr-info-tests--spec (forge head &optional sha)
  (cons 'pr (list :number 12 :sha (or sha head) :head "feature" :base "main"
                  :remote "origin" :forge forge
                  :host (if (eq forge 'gitlab) "gitlab.com" "github.com") :path "o/r")))

(defun ygg-git-compare-pr-info-tests--open (forge head &optional sha show)
  (let ((buffer (ygg-git-compare-buffer default-directory '(rev . "main")
                                        (ygg-git-compare-pr-info-tests--spec forge head sha))))
    (when show (set-window-buffer (selected-window) buffer))
    buffer))

(defun ygg-git-compare-pr-info-tests--settle (buffer &optional done)
  "Let the fake forge answer BUFFER's requests and the sections follow."
  (let ((deadline (+ (float-time) 15)))
    (while (and (< (float-time) deadline)
                (or (> (hash-table-count ygg-git-compare--inflight) 0)
                    (and done (not (with-current-buffer buffer (funcall done))))))
      (accept-process-output nil 0.05))
    (dotimes (_ 8) (accept-process-output nil 0.03))))

(defun ygg-git-compare-pr-info-tests--text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ygg-git-compare-pr-info-tests--header ()
  (substring-no-properties (or header-line-format "")))

(defun ygg-git-compare-pr-info-tests--section (type)
  (seq-find (lambda (s) (eq (oref s type) type)) (oref magit-root-section children)))

(defun ygg-git-compare-pr-info-tests--time (ago)
  (format-time-string "%FT%TZ" (- (float-time) ago) t))

(defun ygg-git-compare-pr-info-tests--rollup (&optional running)
  (ygg-git-compare-pr-info-tests--json
   (list :__typename "CheckRun" :name "build" :status "COMPLETED" :conclusion "SUCCESS"
         :startedAt "2024-01-01T10:00:00Z" :completedAt "2024-01-01T10:02:14Z"
         :detailsUrl "https://ci.example/build" :workflowName "CI")
   (list :__typename "CheckRun" :name "lint" :status "COMPLETED" :conclusion "SUCCESS"
         :startedAt "2024-01-01T10:00:00Z" :completedAt "2024-01-01T10:00:09Z"
         :detailsUrl "https://ci.example/lint" :workflowName "CI")
   (list :__typename "CheckRun" :name "e2e" :status "COMPLETED" :conclusion "FAILURE"
         :startedAt "2024-01-01T10:00:00Z" :completedAt "2024-01-01T11:05:00Z"
         :detailsUrl "https://ci.example/e2e" :workflowName "CI")
   (list :__typename "CheckRun" :name "deploy-preview"
         :status (if running "IN_PROGRESS" "COMPLETED")
         :conclusion (if running "" "SUCCESS")
         :startedAt (ygg-git-compare-pr-info-tests--time 125)
         :completedAt (if running "0001-01-01T00:00:00Z"
                        (ygg-git-compare-pr-info-tests--time 5))
         :detailsUrl "https://ci.example/deploy" :workflowName "CD")
   (list :__typename "CheckRun" :name "docs" :status (if running "QUEUED" "COMPLETED")
         :conclusion (if running "" "SKIPPED")
         :startedAt "0001-01-01T00:00:00Z" :completedAt "0001-01-01T00:00:00Z"
         :detailsUrl "https://ci.example/docs" :workflowName "CD")
   (list :__typename "CheckRun" :name "nightly" :status "COMPLETED" :conclusion "CANCELLED"
         :startedAt "2024-01-01T10:00:00Z" :completedAt "2024-01-01T10:00:30Z"
         :detailsUrl "https://ci.example/nightly" :workflowName "CD")
   (list :__typename "StatusContext" :context "ci/legacy" :state "SUCCESS"
         :startedAt "2024-01-01T10:00:00Z" :targetUrl "https://ci.example/legacy")
   (list :__typename "StatusContext" :context "ci/slow" :state (if running "PENDING" "SUCCESS")
         :startedAt "2024-01-01T10:00:00Z" :targetUrl "https://ci.example/slow")))

(defun ygg-git-compare-pr-info-tests--goto-row (name)
  (goto-char (point-min))
  (search-forward (format " %s " name))
  (beginning-of-line))

;;; Reading

(ert-deftest ygg-git-compare-pr-info-github-states-are-classified ()
  (let* ((value (ygg-git-compare--checks-parse-github
                 (concat "{\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup t) "}")))
         (states (mapcar (lambda (c) (cons (plist-get c :name) (plist-get c :state)))
                         (plist-get value :checks))))
    (should (equal states '(("build" . passed) ("lint" . passed) ("e2e" . failed)
                            ("deploy-preview" . running) ("docs" . pending)
                            ("nightly" . cancelled) ("ci/legacy" . passed)
                            ("ci/slow" . pending))))
    (should (null (plist-get (nth 3 (plist-get value :checks)) :finished)))
    (should (null (plist-get (nth 4 (plist-get value :checks)) :started)))
    (should (equal (plist-get (nth 6 (plist-get value :checks)) :url)
                   "https://ci.example/legacy"))))

(ert-deftest ygg-git-compare-pr-info-github-skipped-and-neutral ()
  (let ((checks (plist-get
                 (ygg-git-compare--checks-parse-github
                  (concat
                   "{\"statusCheckRollup\":"
                   (ygg-git-compare-pr-info-tests--json
                    (list :__typename "CheckRun" :name "a" :status "COMPLETED"
                          :conclusion "SKIPPED")
                    (list :__typename "CheckRun" :name "b" :status "COMPLETED"
                          :conclusion "NEUTRAL")
                    (list :__typename "CheckRun" :name "c" :status "COMPLETED"
                          :conclusion "TIMED_OUT"))
                   "}"))
                 :checks)))
    (should (equal (mapcar (lambda (c) (plist-get c :state)) checks)
                   '(skipped skipped failed)))))

(ert-deftest ygg-git-compare-pr-info-gitlab-jobs-are-classified ()
  (let ((checks (plist-get
                 (ygg-git-compare--checks-parse-gitlab-jobs
                  (concat
                   (ygg-git-compare-pr-info-tests--json
                    (list :name "unit" :status "success" :stage "test"
                          :started_at "2024-01-01T10:00:00Z"
                          :finished_at "2024-01-01T10:01:00Z" :web_url "https://gl/j/1")
                    (list :name "lint" :status "failed" :stage "test"
                          :web_url "https://gl/j/2"))
                   (ygg-git-compare-pr-info-tests--json
                    (list :name "ship" :status "manual" :stage "deploy")
                    (list :name "wait" :status "created" :stage "deploy")
                    (list :name "go" :status "running" :stage "deploy")
                    (list :name "stop" :status "canceled" :stage "deploy")))
                  (list :id 99 :web_url "https://gl/p/99"))
                 :checks)))
    (should (equal (mapcar (lambda (c) (plist-get c :state)) checks)
                   '(passed failed skipped pending running cancelled)))
    (should (equal (plist-get (car checks) :where) "test"))))

;;; Header and sections, GitHub

(ert-deftest ygg-git-compare-pr-info-github-checks-section-and-badge ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json"
                                        (concat "{\"statusCheckRollup\":"
                                                (ygg-git-compare-pr-info-tests--rollup t) "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((text (ygg-git-compare-pr-info-tests--text)))
          (should (string-search "Checks (8)  ✓ 3 · ✗ 2 · ● 3" text))
          (should (string-search "CI ✓ 3 · ✗ 2 · ● 3"
                                 (ygg-git-compare-pr-info-tests--header)))
          (should (string-search "2m 14s" text))
          (should (string-search "1h 05m" text))
          (should (string-search "running" text))
          (should (< (string-search " e2e " text) (string-search " nightly " text)))
          (should (< (string-search " nightly " text) (string-search " deploy-preview " text)))
          (should (< (string-search " deploy-preview " text) (string-search " build " text)))
          (should (< (string-search " docs " text) (string-search " build " text)))
          (should (< (string-search "Checks (8)" text) (string-search "a.txt" text))))
        (should-not (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)
                          hidden))))))

(ert-deftest ygg-git-compare-pr-info-all-passed-folds-the-checks ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json"
                                        (concat "{\"statusCheckRollup\":"
                                                (ygg-git-compare-pr-info-tests--rollup)
                                                "}"))
    (ygg-git-compare-pr-info-tests--put
     "checks.json"
     (concat "{\"statusCheckRollup\":"
             (ygg-git-compare-pr-info-tests--json
              (list :__typename "CheckRun" :name "build" :status "COMPLETED"
                    :conclusion "SUCCESS" :detailsUrl "https://ci.example/build"))
             "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks) hidden))
        (should (string-search "CI ✓ 1" (ygg-git-compare-pr-info-tests--header)))))))

(ert-deftest ygg-git-compare-pr-info-no-checks-draws-no-section ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should-not (string-search "Checks" (ygg-git-compare-pr-info-tests--text)))
        (should-not (string-search "CI" (ygg-git-compare-pr-info-tests--header)))))))

(ert-deftest ygg-git-compare-pr-info-check-keys-open-and-copy-its-url ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json"
                                        (concat "{\"statusCheckRollup\":"
                                                (ygg-git-compare-pr-info-tests--rollup t) "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (ygg-git-compare-pr-info-tests--goto-row "e2e")
        (call-interactively (key-binding (kbd "RET")))
        (should (equal ygg-git-compare-pr-info-tests--browsed "https://ci.example/e2e"))
        (setq ygg-git-compare-pr-info-tests--browsed nil)
        (call-interactively (key-binding (kbd "o")))
        (should (equal ygg-git-compare-pr-info-tests--browsed "https://ci.example/e2e"))
        (call-interactively (key-binding (kbd "y")))
        (should (equal (car kill-ring) "https://ci.example/e2e"))
        (goto-char (point-min))
        (search-forward "a.txt")
        (should-not (eq (key-binding (kbd "y")) #'ygg-git-compare-check-copy))
        (should-not (eq (key-binding (kbd "o")) #'ygg-git-compare-check-open))))))

(ert-deftest ygg-git-compare-pr-info-refresh-refetches-the-checks ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (should (= (ygg-git-compare-pr-info-tests--calls "statusCheckRollup") 1))
      (ygg-git-compare-pr-info-tests--put
       "checks.json"
       (concat "{\"statusCheckRollup\":"
               (ygg-git-compare-pr-info-tests--json
                (list :__typename "CheckRun" :name "late" :status "COMPLETED"
                      :conclusion "FAILURE" :detailsUrl "https://ci.example/late"))
               "}"))
      (with-current-buffer buffer (ygg-git-compare-refresh))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (string-search " late " (ygg-git-compare-pr-info-tests--text))))
      (should (= (ygg-git-compare-pr-info-tests--calls "statusCheckRollup") 2))
      (with-current-buffer buffer
        (should (string-search "CI ✗ 1" (ygg-git-compare-pr-info-tests--header)))))))

;;; Cache

(ert-deftest ygg-git-compare-pr-info-checks-are-kept-per-head-sha ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (ygg-git-compare-pr-info-tests--settle (ygg-git-compare-pr-info-tests--open 'github head))
    (should (= (ygg-git-compare-pr-info-tests--calls "statusCheckRollup") 1))
    (ygg-git-compare-pr-info-tests--settle (ygg-git-compare-pr-info-tests--open 'github head))
    (should (= (ygg-git-compare-pr-info-tests--calls "statusCheckRollup") 1))
    (ygg-git-compare-pr-info-tests--settle
     (ygg-git-compare-pr-info-tests--open
      'github head (ygg-git-compare-pr-info-tests--git work "rev-parse" "main")))
    (should (= (ygg-git-compare-pr-info-tests--calls "statusCheckRollup") 2))
    (let ((keys nil))
      (maphash (lambda (key _) (when (eq (car-safe key) 'checks) (push (nth 5 key) keys)))
               (ygg-git-compare--cache-table (ygg-git-compare--gitdir)))
      (should (equal (sort keys #'string<) (sort (list head (ygg-git-compare-pr-info-tests--git work "rev-parse" "main"))
                                         #'string<))))))

(ert-deftest ygg-git-compare-pr-info-settled-checks-are-not-asked-again-before-the-ttl ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.json"
     (concat "{\"statusCheckRollup\":"
             (ygg-git-compare-pr-info-tests--json
              (list :__typename "CheckRun" :name "build" :status "COMPLETED"
                    :conclusion "SUCCESS"))
             "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((key (ygg-git-compare--checks-key (ygg-git-compare--remote-pr))))
          (ygg-git-compare--checks-start)
          (should (zerop (hash-table-count ygg-git-compare--inflight)))
          (puthash key (list :value (ygg-git-compare--cache-value key)
                             :time (- (float-time) 100))
                   (ygg-git-compare--cache-table (ygg-git-compare--gitdir)))
          (ygg-git-compare--checks-start)
          (should (zerop (hash-table-count ygg-git-compare--inflight)))
          (puthash key (list :value (ygg-git-compare--cache-value key)
                             :time (- (float-time) 1000))
                   (ygg-git-compare--cache-table (ygg-git-compare--gitdir)))
          (ygg-git-compare--checks-start)
          (should (= (hash-table-count ygg-git-compare--inflight) 1))
          (ygg-git-compare-pr-info-tests--settle buffer))))))

;;; Polling

(ert-deftest ygg-git-compare-pr-info-polls-while-visible-and-running ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (let ((ygg-git-compare-checks-running-ttl 3600))
      (ygg-git-compare-pr-info-tests--put "checks.json"
                                          (concat "{\"statusCheckRollup\":"
                                                  (ygg-git-compare-pr-info-tests--rollup t) "}"))
      (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head nil t))
            (window (selected-window))
            (before (window-buffer (selected-window))))
        (unwind-protect
            (progn
              (ygg-git-compare-pr-info-tests--settle buffer)
              (with-current-buffer buffer
                (should (timerp ygg-git-compare--checks-timer)))
              (ygg-git-compare-pr-info-tests--put
               "checks.json"
               (concat "{\"statusCheckRollup\":"
                       (ygg-git-compare-pr-info-tests--rollup) "}"))
              (ygg-git-compare--checks-poll buffer)
              (ygg-git-compare-pr-info-tests--settle
               buffer (lambda () (not (string-search "●" (ygg-git-compare-pr-info-tests--header)))))
              (should (= (ygg-git-compare-pr-info-tests--calls "statusCheckRollup") 2))
              (with-current-buffer buffer
                (should-not ygg-git-compare--checks-timer)
                (should-not (string-search "●" (ygg-git-compare-pr-info-tests--header)))
                (should (string-search "CI ✓ 5 · ✗ 2"
                                       (ygg-git-compare-pr-info-tests--header)))))
          (set-window-buffer window before))))))

(ert-deftest ygg-git-compare-pr-info-does-not-poll-while-hidden ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json"
                                        (concat "{\"statusCheckRollup\":"
                                                (ygg-git-compare-pr-info-tests--rollup t) "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head))
          (window (selected-window))
          (before (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (ygg-git-compare-pr-info-tests--settle buffer)
            (with-current-buffer buffer
              (should-not ygg-git-compare--checks-timer))
            (ygg-git-compare--checks-poll buffer)
            (ygg-git-compare-pr-info-tests--settle buffer)
            (should (= (ygg-git-compare-pr-info-tests--calls "statusCheckRollup") 1))
            (set-window-buffer window buffer)
            (with-current-buffer buffer (ygg-git-compare--checks-resume))
            (with-current-buffer buffer
              (should (timerp ygg-git-compare--checks-timer))
              (ygg-git-compare--checks-stop)
              (should-not ygg-git-compare--checks-timer))
            (set-window-buffer window before)
            (with-current-buffer buffer (ygg-git-compare--checks-arm buffer))
            (with-current-buffer buffer
              (should-not ygg-git-compare--checks-timer)))
        (set-window-buffer window before)))))

;;; Errors

(ert-deftest ygg-git-compare-pr-info-gh-failure-is-an-error-row ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.err" "gh: To use GitHub CLI, run: gh auth login\n")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (string-search "checks: gh: To use GitHub CLI"
                               (ygg-git-compare-pr-info-tests--text)))
        (should (string-search "CI ?" (ygg-git-compare-pr-info-tests--header)))
        (should (string-search "a.txt" (ygg-git-compare-pr-info-tests--text)))))))

(ert-deftest ygg-git-compare-pr-info-unreachable-forge-is-an-error-row ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.err" "Post \"https://api.github.com/graphql\": dial tcp: i/o timeout\n")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (string-search "checks: Post" (ygg-git-compare-pr-info-tests--text)))))))

(ert-deftest ygg-git-compare-pr-info-missing-program-is-an-error-row ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (delete-file (expand-file-name "gh" fake))
    (let ((real-git (executable-find "git")))
      (ygg-git-compare-pr-info-tests--put "git" (format "#!/bin/sh\nexec %s \"$@\"\n" real-git))
      (set-file-modes (expand-file-name "git" fake) #o755))
    (let* ((exec-path (list (directory-file-name fake)))
           (process-environment (cons (concat "PATH=" (directory-file-name fake))
                                      process-environment)))
      (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
        (ygg-git-compare-pr-info-tests--settle buffer)
        (with-current-buffer buffer
          (should (string-search "checks: gh not found" (ygg-git-compare-pr-info-tests--text)))
          (should (string-search "a.txt" (ygg-git-compare-pr-info-tests--text))))))))

(ert-deftest ygg-git-compare-pr-info-garbled-answer-is-an-error-row ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "not json at all")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (string-search "checks: gh answered nothing readable"
                               (ygg-git-compare-pr-info-tests--text)))))))

;;; GitLab

(ert-deftest ygg-git-compare-pr-info-gitlab-pipeline-jobs ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "mr.json" (json-serialize (list :iid 12 :head_pipeline
                                     (list :id 99 :status "failed"
                                           :web_url "https://gitlab.com/o/r/-/pipelines/99"))))
    (ygg-git-compare-pr-info-tests--put
     "jobs.json"
     (ygg-git-compare-pr-info-tests--json
      (list :name "unit" :status "success" :stage "test"
            :started_at "2024-01-01T10:00:00Z" :finished_at "2024-01-01T10:01:30Z"
            :web_url "https://gitlab.com/o/r/-/jobs/1")
      (list :name "integration" :status "failed" :stage "test"
            :started_at "2024-01-01T10:00:00Z" :finished_at "2024-01-01T10:10:00Z"
            :web_url "https://gitlab.com/o/r/-/jobs/2")
      (list :name "deploy" :status "pending" :stage "deploy"
            :web_url "https://gitlab.com/o/r/-/jobs/3")))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'gitlab head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((text (ygg-git-compare-pr-info-tests--text)))
          (should (string-search "Checks (3)  ✓ 1 · ✗ 1 · ● 1" text))
          (should (< (string-search " integration " text) (string-search " unit " text)))
          (should (string-search "1m 30s" text))
          (should (string-search "10m 00s" text)))
        (should (string-search "CI ✓ 1 · ✗ 1 · ● 1" (ygg-git-compare-pr-info-tests--header)))
        (ygg-git-compare-pr-info-tests--goto-row "integration")
        (call-interactively (key-binding (kbd "RET")))
        (should (equal ygg-git-compare-pr-info-tests--browsed "https://gitlab.com/o/r/-/jobs/2")))
      (should (= (ygg-git-compare-pr-info-tests--calls "pipelines/99/jobs") 1))
      (should (= (length (seq-filter (lambda (line) (string-suffix-p "merge_requests/12" line))
                                     (ygg-git-compare-pr-info-tests--log)))
                 1)))))

(ert-deftest ygg-git-compare-pr-info-gitlab-without-a-pipeline-shows-nothing ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "mr.json" "{\"iid\":12,\"head_pipeline\":null}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'gitlab head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should-not (string-search "Checks" (ygg-git-compare-pr-info-tests--text)))
        (should (= (ygg-git-compare-pr-info-tests--calls "jobs") 0))))))

(ert-deftest ygg-git-compare-pr-info-gitlab-failure-is-an-error-row ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "mr.err" "glab: 401 Unauthorized\n")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'gitlab head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (string-search "checks: glab: 401 Unauthorized"
                               (ygg-git-compare-pr-info-tests--text)))))))

;;; Conversation

(defun ygg-git-compare-pr-info-tests--seed-conversation (&optional notes)
  (let ((stamp "2020-01-01T00:00:00Z"))
    (ygg-git-compare-pr-info-tests--put
     "inline.json"
     (ygg-git-compare-pr-info-tests--json
      (list :id 101 :body "inline on two" :path "a.txt" :line 2 :side "RIGHT"
            :user (list :login "alice-inline") :created_at stamp :html_url "https://gh/c/101"
            :diff_hunk "@@ -1,3 +1,3 @@")))
    (ygg-git-compare-pr-info-tests--put
     "reviews.json"
     (ygg-git-compare-pr-info-tests--json
      (list :id 7 :state "APPROVED" :body "ship it" :user (list :login "hank")
            :submitted_at stamp)
      (list :id 8 :state "CHANGES_REQUESTED" :body "fix the name" :user (list :login "ivy")
            :submitted_at "2020-01-02T00:00:00Z")))
    (ygg-git-compare-pr-info-tests--put
     "notes.json"
     (apply #'ygg-git-compare-pr-info-tests--json
            (cl-loop for n from 1 to (or notes 1)
                     collect (list :id (+ 300 n) :body (format "conversation note %d" n)
                                   :user (list :login "frank")
                                   :html_url (format "https://gh/n/%d" n)
                                   :created_at (format "2020-02-%02dT00:00:00Z" n)))))
    (ygg-git-compare-pr-info-tests--put
     "graphql.json"
     (json-serialize
      (list :data (list :repository
                        (list :pullRequest
                              (list :reviewThreads (list :nodes [])))))))))

(ert-deftest ygg-git-compare-pr-info-conversation-lists-comments-and-reviews-not-inline-threads ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation)
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (magit-section-show-level-4-all)
        (let* ((section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation))
               (text (buffer-substring-no-properties (oref section start) (oref section end)))
               (inline (mapconcat (lambda (ov) (concat (overlay-get ov 'before-string)
                                                       (overlay-get ov 'after-string)))
                                  (seq-filter (lambda (ov) (overlay-get ov 'ygg-git-compare-comments))
                                              (overlays-in (point-min) (point-max)))
                                  "\n")))
          (should (string-search "Conversation (3)" text))
          (should (string-search "conversation note 1" text))
          (should (string-search "approved" text))
          (should (string-search "ship it" text))
          (should (string-search "changes requested" text))
          (should (string-search "fix the name" text))
          (should-not (string-search "inline on two" text))
          (should (string-search "inline on two" inline))
          (should-not (string-search "conversation note" inline))
          (should-not (string-search "ship it" inline))
          (should (< (oref section start)
                     (oref (seq-find (lambda (s) (eq (oref s type) 'file))
                                     (oref magit-root-section children))
                           start))))))))

(ert-deftest ygg-git-compare-pr-info-conversation-comes-before-checks-before-changes ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation)
    (ygg-git-compare-pr-info-tests--put
     "checks.json"
     (concat "{\"statusCheckRollup\":"
             (ygg-git-compare-pr-info-tests--rollup t) "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (and (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)
                              (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation))))
      (with-current-buffer buffer
        (let ((text (ygg-git-compare-pr-info-tests--text)))
          (should (< (string-search "Conversation (" text) (string-search "Checks (" text)))
          (should (< (string-search "Checks (" text) (string-search "a.txt" text))))))))

(ert-deftest ygg-git-compare-pr-info-long-conversation-starts-folded ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation 6)
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (let ((section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
          (should (string-search "Conversation (8)"
                                 (ygg-git-compare-pr-info-tests--text)))
          (should (oref section hidden))
          (magit-section-show section)
          (should-not (oref section hidden)))))))

(ert-deftest ygg-git-compare-pr-info-short-conversation-starts-open ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation 2)
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (should-not (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)
                          hidden))))))

(ert-deftest ygg-git-compare-pr-info-conversation-keeps-its-comment-keys ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation)
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "conversation note 1")
        (should (eq (key-binding (kbd "y")) #'ygg-git-compare-threads-copy))
        (call-interactively (key-binding (kbd "y")))
        (should (equal (car kill-ring) "conversation note 1"))
        (call-interactively (key-binding (kbd "o")))
        (should (equal ygg-git-compare-pr-info-tests--browsed "https://gh/n/1"))))))

(ert-deftest ygg-git-compare-pr-info-folding-a-conversation-thread-redraws-the-section ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation)
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "conversation note 1")
        (call-interactively (key-binding (kbd "TAB")))
        (should-not (string-search "conversation note 1" (ygg-git-compare-pr-info-tests--text)))
        (should (string-search "frank" (ygg-git-compare-pr-info-tests--text)))))))

;;; Drawing in place

(defvar ygg-git-compare-pr-info-tests--diffs 0)

(defun ygg-git-compare-pr-info-tests--count-diffs (&rest _)
  (cl-incf ygg-git-compare-pr-info-tests--diffs))

(defmacro ygg-git-compare-pr-info-tests--counting (&rest body)
  (declare (indent 0))
  `(progn
     (setq ygg-git-compare-pr-info-tests--diffs 0)
     (advice-add 'magit-refresh-buffer :before #'ygg-git-compare-pr-info-tests--count-diffs)
     (advice-add 'magit-diff-wash-diffs :before #'ygg-git-compare-pr-info-tests--count-diffs)
     (unwind-protect (progn ,@body)
       (advice-remove 'magit-refresh-buffer #'ygg-git-compare-pr-info-tests--count-diffs)
       (advice-remove 'magit-diff-wash-diffs #'ygg-git-compare-pr-info-tests--count-diffs))))

(defun ygg-git-compare-pr-info-tests--line-at-point ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(ert-deftest ygg-git-compare-pr-info-drawing-sections-in-place-never-re-diffs ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (let ((ygg-git-compare-checks-running-ttl 3600)
          (buffer (ygg-git-compare-pr-info-tests--open 'github head nil t))
          (window (selected-window))
          (before (window-buffer (selected-window))))
      (unwind-protect
          (progn
            (ygg-git-compare-pr-info-tests--put
             "checks.json" (concat "{\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup t) "}"))
            (ygg-git-compare-pr-info-tests--settle buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "a.txt")
              (let ((line (ygg-git-compare-pr-info-tests--line-at-point))
                    (diffs 0))
                (ygg-git-compare-pr-info-tests--counting
                  (ygg-git-compare-pr-info-tests--seed-conversation 7)
                  (ygg-git-compare--pr-info-refetch)
                  (ygg-git-compare-pr-info-tests--settle
                   buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
                  (should (string-search "Conversation (9)" (ygg-git-compare-pr-info-tests--text)))
                  (goto-char (point-min))
                  (search-forward "a.txt")
                  (should (equal (ygg-git-compare-pr-info-tests--line-at-point) line))
                  (goto-char (point-min))
                  (search-forward "conversation note 1")
                  (call-interactively (key-binding (kbd "TAB")))
                  (should-not (string-search "conversation note 1" (ygg-git-compare-pr-info-tests--text)))
                  (setq ygg-git-compare--remote-wrapped-width 0)
                  (ygg-git-compare--remote-rewrap buffer)
                  (should (string-search "frank" (ygg-git-compare-pr-info-tests--text)))
                  (ygg-git-compare-pr-info-tests--put
                   "checks.json" (concat "{\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup) "}"))
                  (ygg-git-compare--checks-poll buffer)
                  (ygg-git-compare-pr-info-tests--settle
                   buffer (lambda () (not (string-search "●" (ygg-git-compare-pr-info-tests--header)))))
                  (should (string-search "CI ✓ 5 · ✗ 2" (ygg-git-compare-pr-info-tests--header)))
                  (should (string-search "Checks (8)  ✓ 5 · ✗ 2" (ygg-git-compare-pr-info-tests--text)))
                  (setq diffs ygg-git-compare-pr-info-tests--diffs))
                (should (zerop diffs))
                (goto-char (point-min))
                (search-forward "a.txt")
                (should (equal (ygg-git-compare-pr-info-tests--line-at-point) line))
                (ygg-git-compare-pr-info-tests--counting
                  (ygg-git-compare-refresh)
                  (ygg-git-compare-pr-info-tests--settle buffer)
                  (should (> ygg-git-compare-pr-info-tests--diffs 0))
                  (let ((after-g ygg-git-compare-pr-info-tests--diffs))
                    (ygg-git-compare-pr-info-tests--settle buffer)
                    (should (= ygg-git-compare-pr-info-tests--diffs after-g)))))))
        (set-window-buffer window before)))))

(ert-deftest ygg-git-compare-pr-info-redraw-keeps-the-section-tree-whole ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation 2)
    (ygg-git-compare-pr-info-tests--put
     "checks.json" (concat "{\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup t) "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)))
      (with-current-buffer buffer
        (let ((types (lambda () (mapcar (lambda (s) (oref s type)) (oref magit-root-section children)))))
          (should (equal (seq-take (funcall types) 3)
                         '(ygg-git-compare-conversation ygg-git-compare-checks diffstat)))
          (magit-section-hide (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks))
          (ygg-git-compare--pr-info-refresh buffer :conversation :checks)
          (should (equal (seq-take (funcall types) 3)
                         '(ygg-git-compare-conversation ygg-git-compare-checks diffstat)))
          (should (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks) hidden))
          (magit-section-show (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks))
          (goto-char (point-min))
          (search-forward " e2e ")
          (should (eq (oref (magit-current-section) type) 'ygg-git-compare-check))
          (dolist (s (oref magit-root-section children))
            (should (eq (marker-buffer (oref s start)) buffer))
            (should (<= (oref s start) (oref s end)))))))))

(ert-deftest ygg-git-compare-pr-info-running-elapsed-grows-on-a-poll-without-a-diff ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.json" (concat "{\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup t) "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (string-match-p " 2m 0[5-9]s" (ygg-git-compare-pr-info-tests--text)))
        (ygg-git-compare-pr-info-tests--counting
          (let ((real (symbol-function 'float-time)))
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&optional time) (+ (funcall real time) (if time 0 600)))))
              (ygg-git-compare--checks-landed buffer)))
          (should (zerop ygg-git-compare-pr-info-tests--diffs)))
        (should (string-match-p "12m 0[5-9]s" (ygg-git-compare-pr-info-tests--text)))))))

(defun ygg-git-compare-pr-info-tests--checks-json (&optional running)
  (concat "{\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup running) "}"))

(defun ygg-git-compare-pr-info-tests--redraw (buffer done)
  (with-current-buffer buffer (ygg-git-compare--pr-info-refetch))
  (ygg-git-compare-pr-info-tests--settle buffer done))

(defun ygg-git-compare-pr-info-tests--on-p (type)
  (let ((section (ygg-git-compare-pr-info-tests--section type)))
    (and section (= (point) (oref section start))
         (eq (oref (magit-current-section) type) type))))

(ert-deftest ygg-git-compare-pr-info-point-on-the-heading-after-a-redrawn-section-stays ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json t))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)))
      (with-current-buffer buffer
        (goto-char (oref (ygg-git-compare-pr-info-tests--section 'diffstat) start))
        (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json))
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (not (string-search "●" (ygg-git-compare-pr-info-tests--header)))))
        (should (ygg-git-compare-pr-info-tests--on-p 'diffstat))
        (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (not (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks))))
        (should (ygg-git-compare-pr-info-tests--on-p 'diffstat))
        (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json))
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)))
        (should (ygg-git-compare-pr-info-tests--on-p 'diffstat))))))

(ert-deftest ygg-git-compare-pr-info-point-on-checks-stays-when-the-conversation-appears-and-grows ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)))
      (with-current-buffer buffer
        (goto-char (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks) start))
        (ygg-git-compare-pr-info-tests--seed-conversation 1)
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
        (should (ygg-git-compare-pr-info-tests--on-p 'ygg-git-compare-checks))
        (ygg-git-compare-pr-info-tests--seed-conversation 3)
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (string-search "Conversation (5)" (ygg-git-compare-pr-info-tests--text))))
        (should (ygg-git-compare-pr-info-tests--on-p 'ygg-git-compare-checks))))))

(ert-deftest ygg-git-compare-pr-info-point-in-a-redrawn-section-keeps-its-row ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation 3)
    (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json t))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (and (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)
                              (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation))))
      (with-current-buffer buffer
        (magit-section-show-level-4-all)
        (ygg-git-compare-pr-info-tests--goto-row "e2e")
        (forward-char 4)
        (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json))
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (not (string-search "●" (ygg-git-compare-pr-info-tests--header)))))
        (should (string-search " e2e " (ygg-git-compare-pr-info-tests--line-at-point)))
        (should (eq (oref (magit-current-section) type) 'ygg-git-compare-check))
        (should (= (current-column) 4))
        (goto-char (point-min))
        (search-forward "conversation note 2")
        (let ((id (get-text-property (point) 'ygg-git-compare-conversation-id)))
          (should id)
          (ygg-git-compare-pr-info-tests--seed-conversation 4)
          (ygg-git-compare-pr-info-tests--redraw
           buffer (lambda () (string-search "Conversation (6)" (ygg-git-compare-pr-info-tests--text))))
          (should (equal (get-text-property (point) 'ygg-git-compare-conversation-id) id)))))))

(ert-deftest ygg-git-compare-pr-info-a-failed-draw-leaves-the-old-section-whole ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json t))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)))
      (with-current-buffer buffer
        (ygg-git-compare-pr-info-tests--goto-row "e2e")
        (let ((text (ygg-git-compare-pr-info-tests--text))
              (point (point))
              (section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks))
              (hidden (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks) hidden)))
          (should-error (ygg-git-compare--pr-info-replace
                         'ygg-git-compare-checks
                         (lambda () (insert "partial\n") (error "boom"))))
          (should (equal (ygg-git-compare-pr-info-tests--text) text))
          (should (= (point) point))
          (should (eq (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks) section))
          (should (eq (oref section hidden) hidden))
          (should (string-prefix-p "Checks (8)"
                                   (buffer-substring-no-properties
                                    (oref section start) (oref section end))))
          (should (eq (oref (magit-current-section) type) 'ygg-git-compare-check))
          (should (string-search "CI ✓" (ygg-git-compare-pr-info-tests--header))))))))

(ert-deftest ygg-git-compare-pr-info-unplaced-review-drafts-sit-above-the-diff-not-the-conversation ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation 1)
    (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json t))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (and (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)
                              (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation))))
      (with-current-buffer buffer
        (setq ygg-git-compare--comments
              (list (list :id "d1" :level 'review :text "overall thoughts")))
        (ygg-git-compare--draw-comments)
        (let ((ov (seq-find (lambda (ov) (member "d1" (overlay-get ov 'ygg-git-compare-comments)))
                            (overlays-in (point-min) (point-max)))))
          (should ov)
          (should (= (overlay-start ov)
                     (oref (ygg-git-compare-pr-info-tests--section 'diffstat) start)))
          (should (>= (overlay-start ov)
                      (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks) end))))))))

;;; Durations, warnings, backing off

(ert-deftest ygg-git-compare-pr-info-durations-are-only-the-known-ones ()
  (let ((now (float-time)))
    (should-not (ygg-git-compare--check-duration (list :state 'passed :started (- now 90))))
    (should-not (ygg-git-compare--check-duration (list :state 'pending :started (- now 90))))
    (should (equal (ygg-git-compare--check-duration
                    (list :state 'passed :started (- now 200) :finished (- now 80)))
                   "2m 00s"))
    (should (equal (ygg-git-compare--check-duration (list :state 'running :started (- now 31)))
                   "31s"))
    (let ((checks (plist-get (ygg-git-compare--checks-parse-github
                              (concat "{\"statusCheckRollup\":"
                                      (ygg-git-compare-pr-info-tests--json
                                       (list :__typename "StatusContext" :context "ci/x"
                                             :state "SUCCESS"
                                             :startedAt "2024-01-01T10:00:00Z"))
                                      "}"))
                             :checks)))
      (should-not (ygg-git-compare--check-duration (car checks))))))

(ert-deftest ygg-git-compare-pr-info-completed-without-a-conclusion-is-not-pending ()
  (let ((checks (plist-get (ygg-git-compare--checks-parse-github
                            (concat "{\"statusCheckRollup\":"
                                    (ygg-git-compare-pr-info-tests--json
                                     (list :__typename "CheckRun" :name "a" :status "COMPLETED"
                                           :conclusion ""))
                                    "}"))
                           :checks)))
    (should (eq (plist-get (car checks) :state) 'skipped))
    (should-not (ygg-git-compare--checks-active-p (list :checks checks)))))

(ert-deftest ygg-git-compare-pr-info-allowed-failure-is-a-warning-not-a-failure ()
  (let ((checks (mapcar #'ygg-git-compare--check-gitlab
                        (list (list :name "a" :status "failed" :allow_failure t)
                              (list :name "b" :status "failed" :allow_failure nil)
                              (list :name "c" :status "manual" :allow_failure t)
                              (list :name "d" :status "success")))))
    (should (equal (mapcar (lambda (c) (plist-get c :state)) checks)
                   '(warning failed skipped passed)))
    (should (equal (ygg-git-compare--checks-counts checks) '(1 1 0 1)))
    (should (equal (substring-no-properties (ygg-git-compare--checks-summary checks))
                   "✓ 1 · ✗ 1 · ! 1"))
    (should-not (ygg-git-compare--checks-active-p (list :checks checks)))))

(ert-deftest ygg-git-compare-pr-info-gives-up-polling-after-repeated-errors ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (let ((ygg-git-compare-checks-running-ttl 3600))
      (ygg-git-compare-pr-info-tests--put
       "checks.json" (concat "{\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup t) "}"))
      (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head nil t))
            (window (selected-window))
            (before (window-buffer (selected-window))))
        (unwind-protect
            (progn
              (ygg-git-compare-pr-info-tests--settle buffer)
              (ygg-git-compare-pr-info-tests--put "checks.err" "i/o timeout\n")
              (dotimes (_ 3)
                (with-current-buffer buffer
                  (should (timerp ygg-git-compare--checks-timer)))
                (ygg-git-compare--checks-poll buffer)
                (ygg-git-compare-pr-info-tests--settle buffer))
              (with-current-buffer buffer
                (should-not ygg-git-compare--checks-timer)
                (should (= ygg-git-compare--checks-errors 3))
                (delete-file (expand-file-name "checks.err" ygg-git-compare-pr-info-tests--fake))
                (ygg-git-compare--pr-info-refetch)
                (should (= ygg-git-compare--checks-errors 0))
                (ygg-git-compare-pr-info-tests--settle buffer)))
          (set-window-buffer window before))))))

(ert-deftest ygg-git-compare-pr-info-conversation-shows-the-comments-notice ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (ygg-git-compare-pr-info-tests--put "graphql.err" "gh: bad credentials\n")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (let* ((section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation))
               (text (buffer-substring-no-properties (oref section start) (oref section end))))
          (should (string-search "forge comments:" text)))))))

;;; Not a pull request

(ert-deftest ygg-git-compare-pr-info-plain-compares-have-no-sections ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ignore head)
    (let ((buffer (ygg-git-compare-buffer default-directory '(rev . "main") '(rev . "feature"))))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((text (ygg-git-compare-pr-info-tests--text)))
          (should-not (string-search "Conversation" text))
          (should-not (string-search "Checks" text))
          (should (string-search "a.txt" text)))
        (should-not (string-search "CI" (ygg-git-compare-pr-info-tests--header)))
        (should-not ygg-git-compare--checks-timer)
        (ygg-git-compare-refresh))
      (should-not (ygg-git-compare-pr-info-tests--log)))))

(provide 'ygg-git-compare-pr-info-tests)
;;; ygg-git-compare-pr-info-tests.el ends here
