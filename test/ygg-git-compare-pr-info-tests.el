;;; ygg-git-compare-pr-info-tests.el --- conversation and checks in a compare -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
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
(require 'ygg-git-compare-images)

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

(defun ygg-git-compare-pr-info-tests--github-status (state draft review)
  (ygg-git-compare--checks-parse-github
   (format "{\"statusCheckRollup\":[],\"state\":\"%s\",\"isDraft\":%s,\"reviewDecision\":\"%s\"}"
           state (if draft "true" "false") review)))

(ert-deftest ygg-git-compare-pr-info-github-status-is-parsed ()
  (dolist (case '(("OPEN" nil "" open nil)
                  ("OPEN" t "" draft nil)
                  ("MERGED" nil "" merged nil)
                  ("CLOSED" nil "" closed nil)
                  ("OPEN" nil "APPROVED" open approved)
                  ("OPEN" nil "CHANGES_REQUESTED" open changes-requested)
                  ("OPEN" nil "REVIEW_REQUIRED" open review-required)))
    (should (equal (plist-get (ygg-git-compare-pr-info-tests--github-status
                               (nth 0 case) (nth 1 case) (nth 2 case))
                              :status)
                   (list :state (nth 3 case) :review (nth 4 case))))))

(ert-deftest ygg-git-compare-pr-info-github-without-a-state-has-no-status ()
  (should-not (plist-get (ygg-git-compare--checks-parse-github "{\"statusCheckRollup\":[]}")
                         :status)))

(ert-deftest ygg-git-compare-pr-info-gitlab-state-is-mapped ()
  (dolist (case '(("opened" nil open) ("opened" t draft) ("merged" nil merged)
                  ("closed" nil closed) ("locked" nil closed)))
    (should (equal (ygg-git-compare--pr-status-gitlab
                    (list :state (nth 0 case) :draft (nth 1 case)))
                   (list :state (nth 2 case)))))
  (should (eq (plist-get (ygg-git-compare--pr-status-gitlab
                          (list :state "opened" :work_in_progress t))
                         :state)
              'draft))
  (should-not (ygg-git-compare--pr-status-gitlab (list :state "weird"))))

(ert-deftest ygg-git-compare-pr-info-header-shows-the-github-status-before-ci ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.json"
     (concat "{\"state\":\"OPEN\",\"isDraft\":false,\"reviewDecision\":\"APPROVED\","
             "\"statusCheckRollup\":" (ygg-git-compare-pr-info-tests--rollup t) "}"))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((header (ygg-git-compare-pr-info-tests--header)))
          (should (string-search "PR #12 open approved" header))
          (should (< (string-search "PR #12" header) (string-search "CI " header))))))))

(ert-deftest ygg-git-compare-pr-info-header-shows-a-merged-pull-request ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.json" "{\"state\":\"MERGED\",\"isDraft\":false,\"reviewDecision\":\"APPROVED\",\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((header (ygg-git-compare-pr-info-tests--header)))
          (should (string-search "PR #12 merged" header))
          (should-not (string-search "approved" header))
          (should (eq (get-text-property 0 'face (ygg-git-compare--pr-badge)) 'success)))))))

(ert-deftest ygg-git-compare-pr-info-header-shows-the-gitlab-status ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "mr.json" "{\"iid\":12,\"state\":\"opened\",\"draft\":true,\"head_pipeline\":null}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'gitlab head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (string-search "PR #12 draft" (ygg-git-compare-pr-info-tests--header)))))))

(ert-deftest ygg-git-compare-pr-info-header-has-no-status-when-b-is-not-a-pull-request ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ignore head)
    (let ((buffer (ygg-git-compare-buffer default-directory '(rev . "main") '(rev . "feature"))))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should-not (string-search "PR #" (ygg-git-compare-pr-info-tests--header)))
        (should-not (ygg-git-compare--pr-badge))))))

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

(ert-deftest ygg-git-compare-pr-info-no-checks-still-draws-the-section ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((text (ygg-git-compare-pr-info-tests--text)))
          (should (string-search "Checks\n  no checks" text))
          (should (< (string-search "Checks" text) (string-search "a.txt" text))))
        (should-not (string-search "CI" (ygg-git-compare-pr-info-tests--header)))))))

(ert-deftest ygg-git-compare-pr-info-checks-being-fetched-draw-the-section ()
  (with-temp-buffer
    (ygg-git-compare--insert-checks '(:fetching t))
    (should (string-search "Checks\n  fetching…" (buffer-string)))))

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
     "mr.json" (json-serialize (list :iid 12 :state "merged" :head_pipeline
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
        (should (string-search "PR #12 merged  CI ✓ 1 · ✗ 1 · ● 1" (ygg-git-compare-pr-info-tests--header)))
        (ygg-git-compare-pr-info-tests--goto-row "integration")
        (call-interactively (key-binding (kbd "o")))
        (should (equal ygg-git-compare-pr-info-tests--browsed "https://gitlab.com/o/r/-/jobs/2")))
      (should (= (ygg-git-compare-pr-info-tests--calls "pipelines/99/jobs") 1))
      (should (= (length (seq-filter (lambda (line) (string-suffix-p "merge_requests/12" line))
                                     (ygg-git-compare-pr-info-tests--log)))
                 1)))))

(ert-deftest ygg-git-compare-pr-info-gitlab-without-a-pipeline-says-no-checks ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "mr.json" "{\"iid\":12,\"head_pipeline\":null}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'gitlab head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (should (string-search "Checks\n  no checks" (ygg-git-compare-pr-info-tests--text)))
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

(ert-deftest ygg-git-compare-pr-info-conversation-comment-takes-a-reply ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation)
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (magit-section-show-level-4-all)
        (goto-char (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation) start))
        (search-forward "conversation note 1")
        (should (eq (key-binding "r") #'ygg-git-compare-threads-reply))
        (should (string-search "conversation note 1"
                               (plist-get (ygg-git-compare--remote-at-point t) :text)))
        (let (composed)
          (cl-letf (((symbol-function 'ygg-git-compare--compose)
                     (lambda (comment) (setq composed comment))))
            (call-interactively (key-binding "r")))
          (should (eq (plist-get composed :level) 'review)))))))

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
          (should (equal (seq-take (funcall types) 4)
                         '(ygg-git-compare-description ygg-git-compare-conversation
                           ygg-git-compare-checks ygg-git-compare-changes)))
          (magit-section-hide (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks))
          (ygg-git-compare--pr-info-refresh buffer :conversation :checks)
          (should (equal (seq-take (funcall types) 4)
                         '(ygg-git-compare-description ygg-git-compare-conversation
                           ygg-git-compare-checks ygg-git-compare-changes)))
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
        (goto-char (oref (ygg-git-compare-pr-info-tests--section 'file) start))
        (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json))
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (not (string-search "●" (ygg-git-compare-pr-info-tests--header)))))
        (should (ygg-git-compare-pr-info-tests--on-p 'file))
        (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (not (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks))))
        (should (ygg-git-compare-pr-info-tests--on-p 'file))
        (ygg-git-compare-pr-info-tests--put "checks.json" (ygg-git-compare-pr-info-tests--checks-json))
        (ygg-git-compare-pr-info-tests--redraw
         buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-checks)))
        (should (ygg-git-compare-pr-info-tests--on-p 'file))))))

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
                     (oref (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-changes) start)))
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

(ert-deftest ygg-git-compare-pr-info-github-description-leads-the-sections ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation)
    (ygg-git-compare-pr-info-tests--put
     "checks.json"
     (json-serialize (list :statusCheckRollup [] :title "gh title" :body "does the thing"
                           :author (list :login "alice") :createdAt "2020-01-01T00:00:00Z"
                           :headRefName "feature" :baseRefName "main"
                           :url "https://github.com/o/r/pull/12")))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (string-search "gh title" (ygg-git-compare-pr-info-tests--text))))
      (with-current-buffer buffer
        (let* ((text (ygg-git-compare-pr-info-tests--text))
               (section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-description))
               (own (buffer-substring-no-properties (oref section start) (oref section end))))
          (should (string-search "#12 gh title" own))
          (should (string-search "alice" own))
          (should (string-search "feature → main" own))
          (should (string-search "does the thing" own))
          (should-not (oref section hidden))
          (should (< (string-search "#12 gh title" text)
                     (string-search "Conversation (" text)))
          (should (< (string-search "Conversation (" text) (string-search "Checks" text)))
          (should (< (string-search "Checks" text) (string-search "a.txt" text))))))))

(ert-deftest ygg-git-compare-pr-info-gitlab-description-shows-the-merge-request ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "mr.json" (json-serialize (list :iid 12 :state "opened" :head_pipeline nil
                                     :title "mr title" :description "merges the thing"
                                     :author (list :username "bob") :created_at "2020-01-01T00:00:00Z"
                                     :source_branch "feature" :target_branch "main")))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'gitlab head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (string-search "mr title" (ygg-git-compare-pr-info-tests--text))))
      (with-current-buffer buffer
        (let* ((text (ygg-git-compare-pr-info-tests--text))
               (section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-description))
               (own (buffer-substring-no-properties (oref section start) (oref section end))))
          (should (string-search "!12 mr title" own))
          (should (string-search "bob" own))
          (should (string-search "feature → main" own))
          (should (string-search "merges the thing" own))
          (should (< (string-search "!12 mr title" text) (string-search "Checks" text)))
          (should (< (string-search "Checks" text) (string-search "a.txt" text))))))))

(ert-deftest ygg-git-compare-pr-info-empty-description-says-so ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.json" (json-serialize (list :statusCheckRollup [] :title "gh title" :body "  \n")))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (string-search "gh title" (ygg-git-compare-pr-info-tests--text))))
      (with-current-buffer buffer
        (let ((section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-description)))
          (should (string-search "(no description)"
                                 (buffer-substring-no-properties (oref section start)
                                                                 (oref section end)))))))))

(ert-deftest ygg-git-compare-pr-info-description-renders-a-template ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.json"
     (json-serialize
      (list :statusCheckRollup [] :title "gh title"
            :body (concat "<!-- Thanks\r\nfor the PR -->\r\n## Summary\r\n"
                          "- [x] done\r\n- [ ] later\r\nSee [docs](https://x) and *this*.\r\n"
                          "```elisp\r\n(message \"hi\")\r\n```\r\n"))))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (string-search "gh title" (ygg-git-compare-pr-info-tests--text))))
      (with-current-buffer buffer
        (let* ((section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-description))
               (own (buffer-substring-no-properties (oref section start) (oref section end))))
          (should-not (string-search "<!--" own))
          (should-not (string-search "](" own))
          (should-not (string-search "- [x]" own))
          (should-not (string-search "\r" own))
          (should (string-search "☑ done" own))
          (should (string-search "☐ later" own))
          (should (string-search "See docs and this." own))
          (should (string-search "Summary" own))
          (should (string-search "(message \"hi\")" own)))))))

(ert-deftest ygg-git-compare-pr-info-conversation-separates-top-level-comments ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-conversation 2)
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-conversation)))
      (with-current-buffer buffer
        (magit-section-show-level-4-all)
        (let* ((spans (ygg-git-compare--conversation-spans))
               (gap (buffer-substring-no-properties (nth 2 (car spans)) (nth 1 (cadr spans)))))
          (should (> (length spans) 1))
          (should (equal gap "\n")))))))

(defun ygg-git-compare-pr-info-tests--description-text (buffer)
  (with-current-buffer buffer
    (let ((section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-description)))
      (buffer-substring-no-properties (oref section start) (oref section end)))))

(ert-deftest ygg-git-compare-pr-info-cache-without-about-is-not-an-empty-description ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((key (ygg-git-compare--checks-key (ygg-git-compare--remote-pr))))
          (puthash key (list :value (list :checks nil) :time (float-time))
                   (ygg-git-compare--cache-table (ygg-git-compare--gitdir)))
          (should-not (plist-get (ygg-git-compare--description-view) :loaded))
          (ygg-git-compare--pr-info-refresh buffer :description)
          (should-not (string-search "(no description)"
                                     (ygg-git-compare-pr-info-tests--description-text buffer))))))))

(ert-deftest ygg-git-compare-pr-info-failed-fetch-shows-in-the-description ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.err" "gh: To use GitHub CLI, run: gh auth login\n")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (let ((own (ygg-git-compare-pr-info-tests--description-text buffer)))
        (should (string-search "description: gh: To use GitHub CLI" own))
        (should-not (string-search "not fetched" own))
        (should-not (string-search "showing the last fetch" own))))))

(ert-deftest ygg-git-compare-pr-info-failed-refetch-keeps-the-last-description ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put
     "checks.json" (json-serialize (list :statusCheckRollup [] :title "gh title" :body "does the thing")))
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (string-search "does the thing" (ygg-git-compare-pr-info-tests--text))))
      (with-current-buffer buffer
        (let* ((key (ygg-git-compare--checks-key (ygg-git-compare--remote-pr)))
               (entry (ygg-git-compare--cache-entry key)))
          (puthash key (plist-put (copy-sequence entry) :error "boom")
                   (ygg-git-compare--cache-table (ygg-git-compare--gitdir)))
          (ygg-git-compare--pr-info-refresh buffer :description)))
      (let ((own (ygg-git-compare-pr-info-tests--description-text buffer)))
        (should (string-search "does the thing" own))
        (should (string-search "description: boom (showing the last fetch)" own))))))

(ert-deftest ygg-git-compare-pr-info-malformed-about-keeps-the-checks ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--put "checks.json" "{\"statusCheckRollup\":[]}")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle buffer)
      (with-current-buffer buffer
        (let ((key (ygg-git-compare--checks-key (ygg-git-compare--remote-pr)))
              (check (list :name "build" :state 'passed)))
          (puthash key (list :value (list :checks (list check) :about '(:title 5))
                             :time (float-time))
                   (ygg-git-compare--cache-table (ygg-git-compare--gitdir)))
          (let ((value (ygg-git-compare--checks-value key)))
            (should (equal (plist-get value :checks) (list check)))
            (should-not (plist-get value :about))))))))

(ert-deftest ygg-git-compare-pr-info-answer-for-a-repinned-head-still-draws ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (let ((pending nil)
          (answer (list :checks nil :about (list :title "Repinned" :body "lands late"))))
      (cl-letf (((symbol-function 'ygg-git-compare--checks-fetch)
                 (lambda (_pr done) (setq pending (append pending (list done))))))
        (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
          (with-current-buffer buffer
            (plist-put (cdr ygg-git-compare--b-spec) :sha "0000000000000000000000000000000000000001")
            (ygg-git-compare--checks-start))
          (should (= (length pending) 2))
          (funcall (car pending) answer)
          (dotimes (_ 8) (accept-process-output nil 0.03))
          (funcall (cadr pending) answer)
          (ygg-git-compare-pr-info-tests--settle buffer)
          (should (string-search "lands late"
                                 (ygg-git-compare-pr-info-tests--description-text buffer))))))))

(defconst ygg-git-compare-pr-info-tests--png
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")

(defconst ygg-git-compare-pr-info-tests--gif
  "R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7")

(defun ygg-git-compare-pr-info-tests--picture (name data)
  (let ((file (expand-file-name name ygg-git-compare-image-directory))
        (coding-system-for-write 'no-conversion))
    (make-directory ygg-git-compare-image-directory t)
    (write-region (base64-decode-string data) nil file nil 'silent)
    file))

(defmacro ygg-git-compare-pr-info-tests--with-pictures (&rest body)
  (declare (indent 0))
  `(let ((ygg-git-compare-image-directory
          (file-name-as-directory (expand-file-name "pictures" root)))
         (ygg-git-compare--images (make-hash-table :test #'equal))
         (ygg-git-compare--image-waiting (make-hash-table :test #'equal))
         (ygg-git-compare--image-objects (make-hash-table :test #'equal))
         (ygg-git-compare--image-queue nil)
         (ygg-git-compare--image-active 0)
         (ygg-git-compare-image-external t))
     (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
       ,@body)))

(defun ygg-git-compare-pr-info-tests--seed-body (body)
  (ygg-git-compare-pr-info-tests--put
   "checks.json"
   (json-serialize (list :statusCheckRollup [] :title "gh title" :body body
                         :author (list :login "alice") :createdAt "2020-01-01T00:00:00Z"
                         :headRefName "feature" :baseRefName "main"
                         :url "https://github.com/o/r/pull/12"))))

(defun ygg-git-compare-pr-info-tests--displays (buffer)
  "The `display' values on the description section of BUFFER, in order."
  (with-current-buffer buffer
    (let* ((section (ygg-git-compare-pr-info-tests--section 'ygg-git-compare-description))
           (pos (oref section start))
           out)
      (while (< pos (oref section end))
        (when-let* ((display (get-text-property pos 'display)))
          (unless (eq display (car out)) (push display out)))
        (setq pos (next-single-property-change pos 'display nil (oref section end))))
      (nreverse out))))

(ert-deftest ygg-git-compare-pr-info-image-parts-read-markdown-and-html ()
  (let ((parts (ygg-git-compare--image-parts
                (concat "intro ![shot](https://x/a.png) after\r\n"
                        "<a href=\"https://l\"><img width=\"300\" alt=\"wide\" src=\"https://x/b.gif?a=1&amp;b=2\"></a>\n"
                        "[![badge](https://x/c.svg)](https://link)\n"
                        "<!-- ![hidden](https://x/h.png) -->\n"
                        "```\n![code](https://x/code.png)\n```\n"))))
    (should (equal (seq-filter #'consp parts)
                   '((:image "https://x/a.png" :alt "shot" :width nil)
                     (:image "https://x/b.gif?a=1&b=2" :alt "wide" :width 300)
                     (:image "https://x/c.svg" :alt "badge" :width nil))))
    (should (equal (car parts) "intro"))
    (should (string-search "![code](https://x/code.png)" (car (last parts))))
    (should-not (string-search "hidden" (format "%S" parts)))))

(ert-deftest ygg-git-compare-pr-info-image-sources-and-commands ()
  (let ((github '(:forge github :host "github.com" :path "o/r" :number 1))
        (gitlab '(:forge gitlab :host "git.example.com" :path "g/p" :number 1))
        (url "https://github.com/user-attachments/assets/1234"))
    (should (equal (ygg-git-compare--image-source "/uploads/abc/shot.png" gitlab)
                   '(remote . "https://git.example.com/g/p/uploads/abc/shot.png")))
    (should (equal (ygg-git-compare--image-source "//x.io/a.png" github)
                   '(remote . "https://x.io/a.png")))
    (should (equal (ygg-git-compare--image-token-command url github)
                   '("gh" "auth" "token" "--hostname" "github.com")))
    (should (equal (ygg-git-compare--image-token-command
                    "https://git.example.com/g/p/uploads/abc/shot.png" gitlab)
                   '("glab" "config" "get" "token" "--host" "git.example.com")))
    (should-not (ygg-git-compare--image-token-command "https://evil.example/a.png" github))
    (should (equal (car (last (ygg-git-compare--image-signed-command url github))) "repos/o/r/pulls/1"))
    (should-not (ygg-git-compare--image-signed-command url gitlab))
    (should (equal (ygg-git-compare--image-signed-url
                    "<p><img src=\"https://private-user-images.githubusercontent.com/1/2-1234.png?jwt=a&amp;b=c\"></p>"
                    url)
                   "https://private-user-images.githubusercontent.com/1/2-1234.png?jwt=a&b=c"))
    (let ((command (ygg-git-compare--image-curl-command url "/tmp/part" t)))
      (should (equal (cl-subseq command (- (length command) 4))
                     (list "--output" "/tmp/part" "--url" url)))
      (should (member "--config" command)))))

(ert-deftest ygg-git-compare-pr-info-description-draws-fetched-pictures ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((png (ygg-git-compare-pr-info-tests--picture "a.png" ygg-git-compare-pr-info-tests--png))
             (gif (ygg-git-compare-pr-info-tests--picture "b.gif" ygg-git-compare-pr-info-tests--gif))
             (ygg-git-compare-image-fetcher
              (lambda (url _pr done)
                (let ((file (if (string-suffix-p "a.png" url) png gif)))
                  (funcall done (ygg-git-compare--image-found file))))))
        (ygg-git-compare-pr-info-tests--seed-body
         "does the thing\n\n![still](https://x.example/a.png)\n<img src=\"https://x.example/b.gif\" alt=\"moving\" width=\"40\">\n\nthe end")
        (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
          (ygg-git-compare-pr-info-tests--settle
           buffer (lambda () (string-search "the end" (ygg-git-compare-pr-info-tests--text))))
          (let ((displays (ygg-git-compare-pr-info-tests--displays buffer))
                (own (ygg-git-compare-pr-info-tests--description-text buffer)))
            (should (= (length displays) 2))
            (should (eq (car (car displays)) 'image))
            (should (eq (plist-get (cdr (nth 0 displays)) :type) 'png))
            (should (eq (plist-get (cdr (nth 1 displays)) :type) 'gif))
            (should (<= (plist-get (cdr (nth 1 displays)) :max-width) 40))
            (should (string-search "still" own))
            (should (< (string-search "does the thing" own) (string-search "still" own)))
            (should (< (string-search "moving" own) (string-search "the end" own)))))))))

(ert-deftest ygg-git-compare-pr-info-description-remote-picture-lands-without-moving-point ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((waiting nil)
             (ygg-git-compare-image-fetcher
              (lambda (url _pr done) (push (cons url done) waiting))))
        (ygg-git-compare-pr-info-tests--seed-body
         "does the thing\n\n![remote shot](https://github.com/user-attachments/assets/9)\n\nthe end")
        (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
          (ygg-git-compare-pr-info-tests--settle
           buffer (lambda () (string-search "the end" (ygg-git-compare-pr-info-tests--text))))
          (should (equal (mapcar #'car waiting) '("https://github.com/user-attachments/assets/9")))
          (should (equal (ygg-git-compare-pr-info-tests--displays buffer) '("remote shot…")))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "the end")
            (let ((point (point))
                  (text (ygg-git-compare-pr-info-tests--text)))
              (funcall (cdr (car waiting))
                       (list :file (ygg-git-compare-pr-info-tests--picture
                                    "9" ygg-git-compare-pr-info-tests--png)
                             :type 'png))
              (should (= (point) point))
              (should (equal (ygg-git-compare-pr-info-tests--text) text))
              (should (eq (car (car (ygg-git-compare-pr-info-tests--displays buffer))) 'image)))))))))

(ert-deftest ygg-git-compare-pr-info-description-failed-picture-keeps-its-alt-text ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let ((ygg-git-compare-image-fetcher (lambda (_url _pr done) (funcall done nil))))
        (ygg-git-compare-pr-info-tests--seed-body "![remote shot](https://x.example/a.png)")
        (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
          (ygg-git-compare-pr-info-tests--settle
           buffer (lambda () (string-search "remote shot" (ygg-git-compare-pr-info-tests--text))))
          (should (equal (ygg-git-compare-pr-info-tests--displays buffer)
                         '("remote shot (image unavailable)"))))))))

(ert-deftest ygg-git-compare-pr-info-description-without-pictures-is-plain-text ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (ygg-git-compare-pr-info-tests--seed-body "See [docs](https://x) and *this*.\n\n- [x] done")
      (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
        (ygg-git-compare-pr-info-tests--settle
         buffer (lambda () (string-search "See docs" (ygg-git-compare-pr-info-tests--text))))
        (should-not (ygg-git-compare-pr-info-tests--displays buffer))
        (should (string-search "  See docs and this.\n\n  ☑ done"
                               (ygg-git-compare-pr-info-tests--description-text buffer)))))))

(ert-deftest ygg-git-compare-pr-info-description-in-a-text-terminal-shows-alt-text ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--seed-body "![shot](https://x.example/a.png)")
    (let ((buffer (ygg-git-compare-pr-info-tests--open 'github head)))
      (ygg-git-compare-pr-info-tests--settle
       buffer (lambda () (string-search "shot" (ygg-git-compare-pr-info-tests--text))))
      (should-not (ygg-git-compare-pr-info-tests--displays buffer))
      (should (string-search "  shot\n" (ygg-git-compare-pr-info-tests--description-text buffer))))))

(provide 'ygg-git-compare-pr-info-tests)
;;; ygg-git-compare-pr-info-tests.el ends here

(ert-deftest ygg-git-compare-pr-info-image-empty-body-fails-and-frees-the-slot ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let ((ygg-git-compare-image-fetcher
             (lambda (url _pr done) (ygg-git-compare--image-curl url nil done)))
            (source "https://x.example/empty.png"))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (let ((sentinel (plist-get args :sentinel))
                           (process (start-process "empty" nil "true")))
                       (set-process-sentinel process nil)
                       (while (process-live-p process) (accept-process-output process 0.05))
                       (funcall sentinel process "finished\n")
                       process))))
          (with-temp-buffer
            (ygg-git-compare--image-state source nil)
            (should (plist-get (gethash source ygg-git-compare--images) :failed))
            (should (= ygg-git-compare--image-active 0))))))))

(ert-deftest ygg-git-compare-pr-info-image-signed-fallback-is-cached-by-source ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((source "https://github.com/user-attachments/assets/1234")
             (pr '(:forge github :host "github.com" :path "o/r" :number 1))
             (signed "https://private-user-images.githubusercontent.com/1/2-1234.png?jwt=a")
             (captured nil))
        (cl-letf (((symbol-function 'ygg-git-compare--image-signed-command)
                   (lambda (&rest _) '("gh")))
                  ((symbol-function 'ygg-git-compare--forge-async)
                   (lambda (_program _args callback)
                     (funcall callback 0 (format "<img src=\"%s\">" signed) "")))
                  ((symbol-function 'ygg-git-compare--image-curl)
                   (lambda (url _config done &optional key)
                     (push (cons url key) captured)
                     (funcall done nil))))
          (ygg-git-compare--image-fetch-signed source pr #'ignore)
          (should (equal captured (list (cons signed source)))))
        (ygg-git-compare-pr-info-tests--picture
         (md5 source) ygg-git-compare-pr-info-tests--png)
        (with-temp-buffer
          (should (eq (plist-get (ygg-git-compare--image-state source pr) :type) 'png)))))))

(ert-deftest ygg-git-compare-pr-info-image-local-sources-are-never-read ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((png (ygg-git-compare-pr-info-tests--picture "a.png" ygg-git-compare-pr-info-tests--png))
             (read nil)
             (pr '(:forge github :host "github.com" :path "o/r" :number 1)))
        (cl-letf (((symbol-function 'ygg-git-compare--image-found)
                   (lambda (file) (push file read) nil)))
          (dolist (url (list (concat "file://" png) (concat "file:" png) "~/a.png"))
            (should-not (ygg-git-compare--image-source url pr))
            (should (equal (substring-no-properties
                            (get-text-property
                             2 'display
                             (ygg-git-compare--image-line (list :image url :alt "pic" :width nil) pr)))
                           "pic (image unavailable)")))
          (should (equal (ygg-git-compare--image-source png pr)
                         (cons 'remote (concat "https://github.com" png))))
          (should-not read))))))

(ert-deftest ygg-git-compare-pr-info-image-third-party-host-needs-opt-in ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((fetched nil)
             (pr '(:forge github :host "github.com" :path "o/r" :number 1))
             (ygg-git-compare-image-fetcher (lambda (url _pr _done) (push url fetched))))
        (dolist (url '("https://tracker.example.com/p.png" "http://github.com/a.png"))
          (let ((ygg-git-compare-image-external nil))
            (with-temp-buffer
              (should (string-match-p
                       "\\`pic (image from [^ ]+ — not loaded)\\'"
                       (substring-no-properties
                        (get-text-property
                         2 'display
                         (ygg-git-compare--image-line (list :image url :alt "pic" :width nil) pr))))))))
        (should-not fetched)
        (with-temp-buffer
          (let ((ygg-git-compare-image-external nil))
            (ygg-git-compare--image-line
             '(:image "https://private-user-images.githubusercontent.com/1/a.png" :alt "a" :width nil) pr))
          (let ((ygg-git-compare-image-external t))
            (ygg-git-compare--image-line
             '(:image "https://tracker.example.com/p.png" :alt "b" :width nil) pr)
            (ygg-git-compare--image-line
             '(:image "http://tracker.example.com/q.png" :alt "c" :width nil) pr)))
        (should (equal (sort fetched #'string<)
                       '("https://private-user-images.githubusercontent.com/1/a.png"
                         "https://tracker.example.com/p.png")))
        (should (string-match-p "tracker.example.com"
                                (let ((ygg-git-compare-image-external nil))
                                  (get-text-property
                                   2 'display
                                   (ygg-git-compare--image-line
                                    '(:image "https://tracker.example.com/z.png" :alt "z" :width nil)
                                    pr)))))))))

(ert-deftest ygg-git-compare-pr-info-image-parts-leave-code-as-text ()
  (let ((parts (ygg-git-compare--image-parts
                (concat "use `![a](https://x/a.png)` here\n"
                        "    ![b](https://x/b.png)\n"
                        "\t![c](https://x/c.png)\n"
                        "real ![d](https://x/d.png) and `code`\n"))))
    (should (equal (seq-filter #'consp parts)
                   '((:image "https://x/d.png" :alt "d" :width nil))))
    (let ((text (string-join (seq-filter #'stringp parts) "\n")))
      (should (string-search "use `![a](https://x/a.png)` here" text))
      (should (string-search "    ![b](https://x/b.png)" text))
      (should (string-search "\t![c](https://x/c.png)" text))
      (should (string-search "and `code`" text)))))

(ert-deftest ygg-git-compare-pr-info-image-resize-keeps-one-object-per-file ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((file (ygg-git-compare-pr-info-tests--picture "a.png" ygg-git-compare-pr-info-tests--png))
             (state (list :file file :type 'png))
             (columns 800))
        (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) columns)))
          (let ((first (ygg-git-compare--image-object state nil)))
            (should (eq first (ygg-git-compare--image-object state nil)))
            (dolist (width '(700 600 500))
              (setq columns width)
              (ygg-git-compare--image-object state nil))
            (should (= (hash-table-count ygg-git-compare--image-objects) 1))
            (should-not (eq first (ygg-git-compare--image-object state nil)))))))))

(ert-deftest ygg-git-compare-pr-info-image-userinfo-urls-are-never-fetched-or-given-a-token ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((fetched nil)
             (pr '(:forge github :host "github.com" :path "o/r" :number 1))
             (ygg-git-compare-image-fetcher (lambda (url _pr _done) (push url fetched))))
        (dolist (url '("https://github.com:pw@evil.com/a.png"
                       "https://user@github.com/a.png"
                       "//github.com:pw@evil.com/a.png"))
          (should-not (ygg-git-compare--image-source url pr))
          (should-not (ygg-git-compare--image-token-command url pr))
          (should-not (ygg-git-compare--image-allowed-p url pr))
          (dolist (external '(t nil))
            (let ((ygg-git-compare-image-external external))
              (with-temp-buffer
                (should (equal (substring-no-properties
                                (get-text-property
                                 2 'display
                                 (ygg-git-compare--image-line (list :image url :alt "pic" :width nil) pr)))
                               "pic (image unavailable)"))))))
        (should-not fetched)))))

(ert-deftest ygg-git-compare-pr-info-image-token-host-follows-the-host-parser ()
  (let ((pr '(:forge github :host "github.com" :path "o/r" :number 1)))
    (should (ygg-git-compare--image-token-command "https://github.com:443/a.png" pr))
    (should-not (ygg-git-compare--image-token-command "https://github.com.evil.com/a.png" pr))
    (should-not (ygg-git-compare--image-token-command "https://github.com\\@evil.com/a.png" pr))
    (should-not (ygg-git-compare--image-token-command "http://github.com/a.png" pr))))

(ert-deftest ygg-git-compare-pr-info-image-signed-url-ignores-foreign-hosts ()
  (let* ((url "https://github.com/user-attachments/assets/1234")
         (good "https://private-user-images.githubusercontent.com/1/2-1234.png?jwt=a&b=c")
         (html (concat "<a href=\"https://evil.example/2-1234.png\">x</a>"
                       "<img src=\"https://evil.example/x.githubusercontent.com/2-1234.png\">"
                       "<img src=\"http://private-user-images.githubusercontent.com/1/2-1234.png\">"
                       "<img src=\"https://private-user-images.githubusercontent.com/1/9-9999.png\">"
                       "<img src=\"" (replace-regexp-in-string "&" "&amp;" good) "\">")))
    (should (equal (ygg-git-compare--image-signed-url html url) good))
    (should-not (ygg-git-compare--image-signed-url
                 "<a href=\"https://evil.example/2-1234.png\">x</a>" url))))

(ert-deftest ygg-git-compare-pr-info-image-signed-fetch-skips-untrusted-address ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let ((source "https://github.com/user-attachments/assets/1234")
            (pr '(:forge github :host "github.com" :path "o/r" :number 1))
            (curled nil)
            (result 'unset))
        (cl-letf (((symbol-function 'ygg-git-compare--image-signed-command)
                   (lambda (&rest _) '("gh")))
                  ((symbol-function 'ygg-git-compare--forge-async)
                   (lambda (_program _args callback)
                     (funcall callback 0 "<a href=\"https://evil.example/2-1234.png\">x</a>" "")))
                  ((symbol-function 'ygg-git-compare--image-curl)
                   (lambda (url &rest _) (push url curled))))
          (ygg-git-compare--image-fetch-signed source pr (lambda (found) (setq result found)))
          (should-not curled)
          (should-not result))))))

(ert-deftest ygg-git-compare-pr-info-image-curl-ignores-curlrc-and-globs ()
  (let ((command (ygg-git-compare--image-curl-command "https://x/a[1].png" "/tmp/part" nil)))
    (should (equal (cl-subseq command 0 3) '("curl" "-q" "--globoff")))))

(ert-deftest ygg-git-compare-pr-info-image-animation-limit-is-seconds ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((file (ygg-git-compare-pr-info-tests--picture "b.gif" ygg-git-compare-pr-info-tests--gif))
             (state (list :file file :type 'gif))
             (calls nil))
        (cl-letf (((symbol-function 'image-multi-frame-p) (lambda (&rest _) t))
                  ((symbol-function 'image-animate)
                   (lambda (image &optional index limit) (push (list index limit) calls))))
          (let ((ygg-git-compare-gif-seconds 7))
            (ygg-git-compare--image-object state nil))
          (should (equal calls '((nil 7))))
          (clrhash ygg-git-compare--image-objects)
          (setq calls nil)
          (let ((ygg-git-compare-gif-seconds 0))
            (ygg-git-compare--image-object state nil))
          (should-not calls)
          (should (eq (indirect-variable 'ygg-git-compare-gif-loops)
                      'ygg-git-compare-gif-seconds)))))))

(ert-deftest ygg-git-compare-pr-info-image-curl-stops-at-the-byte-limit ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((url "https://x.example/big.png")
             (part (ygg-git-compare--image-part-file (ygg-git-compare--image-cache-file url)))
             (ygg-git-compare-image-bytes 10)
             (check nil)
             (results nil)
             (real-make-process (symbol-function 'make-process))
             (process nil))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _)
                     (setq process (funcall real-make-process :name "big" :buffer nil :noquery t
                                            :command '("sleep" "30")))))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function) (setq check function) (timer-create))))
          (unwind-protect
              (progn
                (ygg-git-compare--image-curl url nil (lambda (found) (push (list found) results)))
                (should check)
                (should (process-live-p process))
                (funcall check)
                (should (process-live-p process))
                (let ((coding-system-for-write 'no-conversion))
                  (write-region (make-string 11 ?x) nil part nil 'silent))
                (funcall check)
                (should-not (process-live-p process))
                (should (equal results '((nil))))
                (should-not (file-exists-p part)))
            (when (process-live-p process) (delete-process process))))))))

(ert-deftest ygg-git-compare-pr-info-image-directory-is-private ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (should (ygg-git-compare--image-prepare-directory))
      (should (= (logand (file-modes ygg-git-compare-image-directory) #o777) #o700)))))

(ert-deftest ygg-git-compare-pr-info-image-foreign-directory-is-refused ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (make-directory ygg-git-compare-image-directory t)
      (let ((source "https://x.example/a.png")
            (started nil))
        (cl-letf (((symbol-function 'user-uid) (lambda () (1+ (file-attribute-user-id
                                                              (file-attributes root 'integer)))))
                  ((symbol-function 'make-process) (lambda (&rest _) (setq started t) nil))
                  ((symbol-function 'ygg-git-compare--image-source) (lambda (url _pr) (cons 'remote url))))
          (should-not (ygg-git-compare--image-prepare-directory))
          (let ((ygg-git-compare-image-fetcher
                 (lambda (url _pr done) (ygg-git-compare--image-curl url nil done))))
            (with-temp-buffer
              (ygg-git-compare--image-state source nil)
              (should (plist-get (gethash source ygg-git-compare--images) :failed))
              (should-not started)
              (should (equal (substring-no-properties
                              (get-text-property
                               2 'display
                               (ygg-git-compare--image-line (list :image source :alt "pic" :width nil) nil)))
                             "pic (image unavailable)")))))))))

(ert-deftest ygg-git-compare-pr-info-image-curl-send-failure-ends-the-job ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let ((results nil)
            (real-make-process (symbol-function 'make-process))
            (process nil))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _)
                     (setq process (funcall real-make-process :name "pipe" :buffer nil :noquery t
                                            :command '("sleep" "30")))))
                  ((symbol-function 'process-send-string)
                   (lambda (&rest _) (signal 'file-error '("Broken pipe")))))
          (unwind-protect
              (progn
                (ygg-git-compare--image-curl "https://x.example/a.png" "header = \"x\"\n"
                                             (lambda (found) (push (list found) results)))
                (should (equal results '((nil))))
                (should-not (process-live-p process)))
            (when (process-live-p process) (delete-process process))))))))

(ert-deftest ygg-git-compare-pr-info-image-attributes-are-anchored-quoted-and-decoded ()
  (let ((parts (seq-filter
                #'consp
                (ygg-git-compare--image-parts
                 (concat "<img data-src=\"https://bad/x.png\" src=\"https://ok/x.png\" data-alt=\"no\" alt=\"yes\">\n"
                         "<img alt=\"a > b\" src=\"https://ok/q.png\">\n"
                         "<img src='https://ok/r.png?a=1&amp;b=2' alt=\"&quot;hi&quot; &lt;b&gt; &#39;x&#39; &amp;amp;\">\n"
                         "<img data-src=\"https://bad/only.png\">\n")))))
    (should (equal (mapcar (lambda (item) (plist-get item :image)) parts)
                   '("https://ok/x.png" "https://ok/q.png" "https://ok/r.png?a=1&b=2")))
    (should (equal (mapcar (lambda (item) (plist-get item :alt)) parts)
                   '("yes" "a > b" "\"hi\" <b> 'x' &amp;")))))

(ert-deftest ygg-git-compare-pr-info-image-host-refuses-ports-but-443 ()
  (let ((pr '(:forge github :host "github.com" :path "o/r" :number 1))
        (ygg-git-compare-image-external t))
    (should-not (ygg-git-compare--image-host "https://github.com:8443/x.png"))
    (should-not (ygg-git-compare--image-token-command "https://github.com:8443/x.png" pr))
    (should-not (ygg-git-compare--image-allowed-p "https://github.com:8443/x.png" pr))
    (should (equal (ygg-git-compare--image-host "https://github.com:443/x.png")
                   (ygg-git-compare--image-host "https://github.com/x.png")))
    (should (ygg-git-compare--image-token-command "https://github.com:443/x.png" pr))
    (should (ygg-git-compare--image-allowed-p "https://github.com:443" pr))))

(ert-deftest ygg-git-compare-pr-info-image-pump-survives-a-failing-paint ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((ygg-git-compare-image-jobs 1)
             (started nil)
             (dones nil)
             (ygg-git-compare-image-fetcher
              (lambda (url _pr done) (push url started) (push done dones))))
        (cl-letf (((symbol-function 'ygg-git-compare--image-paint)
                   (lambda (&rest _) (signal 'error '("paint")))))
          (with-temp-buffer
            (ygg-git-compare--image-state "https://x.example/1.png" nil)
            (ygg-git-compare--image-state "https://x.example/2.png" nil)
            (should (equal started '("https://x.example/1.png")))
            (funcall (car dones) nil)
            (should (equal started '("https://x.example/2.png" "https://x.example/1.png")))
            (should (= ygg-git-compare--image-active 1))))))))

(ert-deftest ygg-git-compare-pr-info-image-directory-is-created-private ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let ((modes nil))
        (cl-letf (((symbol-function 'set-file-modes) (lambda (&rest _) nil))
                  ((symbol-function 'make-directory)
                   (let ((real (symbol-function 'make-directory)))
                     (lambda (dir &optional parents)
                       (setq modes (default-file-modes))
                       (funcall real dir parents)))))
          (ygg-git-compare--image-prepare-directory)
          (should (= modes #o700))
          (should (= (logand (file-modes ygg-git-compare-image-directory) #o777) #o700)))))))

(ert-deftest ygg-git-compare-pr-info-image-attributes-text-in-values-cannot-spoof ()
  (let ((parts (seq-filter
                #'consp
                (ygg-git-compare--image-parts
                 (concat "<img alt=\"a src=https://github.com/evil\" src=\"https://github.com/real\">\n"
                         "<img src=https://x/a.png?a=1&#38;b=2&#x2F;c alt='&#65;&#x42;&#0;'>\n")))))
    (should (equal (mapcar (lambda (item) (plist-get item :image)) parts)
                   '("https://github.com/real" "https://x/a.png?a=1&b=2/c")))
    (should (equal (plist-get (nth 0 parts) :alt) "a src=https://github.com/evil"))
    (should (equal (plist-get (nth 1 parts) :alt) "AB&#0;"))))

(ert-deftest ygg-git-compare-pr-info-image-pathological-line-parses-fast ()
  (let* ((line (apply #'concat (make-list 8000 "<img '")))
         (start (float-time))
         (parts (ygg-git-compare--image-parts (concat line "\n" (make-string 3000 ?x)))))
    (should (> (length line) 40000))
    (should (< (- (float-time) start) 0.2))
    (should-not (seq-some #'consp parts))
    (let ((start (float-time)))
      (ygg-git-compare--image-parts (apply #'concat (make-list 700 "<img '")))
      (ygg-git-compare--image-parts (apply #'concat (make-list 700 "![a](")))
      (should (< (- (float-time) start) 0.2)))))

(ert-deftest ygg-git-compare-pr-info-image-state-does-not-read-a-refused-directory ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (make-directory ygg-git-compare-image-directory t)
      (let ((source "https://x.example/a.png")
            (read nil)
            (fetched nil)
            (ygg-git-compare-image-fetcher (lambda (&rest _) (setq fetched t))))
        (ygg-git-compare-pr-info-tests--picture (md5 source) ygg-git-compare-pr-info-tests--png)
        (cl-letf (((symbol-function 'user-uid)
                   (lambda () (1+ (file-attribute-user-id (file-attributes root 'integer)))))
                  ((symbol-function 'ygg-git-compare--image-found)
                   (lambda (file) (push file read) nil)))
          (with-temp-buffer
            (should (plist-get (ygg-git-compare--image-state source nil) :failed))
            (should-not read)
            (should-not fetched)))))))

(ert-deftest ygg-git-compare-pr-info-image-max-limits-images-per-description ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((ygg-git-compare-image-max 2)
             (fetched nil)
             (ygg-git-compare-image-fetcher (lambda (url _pr _done) (push url fetched)))
             (parts (seq-filter #'consp
                                (ygg-git-compare--image-parts
                                 "![a](https://x/1.png) ![b](https://x/2.png)\n![c](https://x/3.png)")))
             (lines nil))
        (with-temp-buffer
          (dolist (item parts)
            (push (substring-no-properties
                   (get-text-property 2 'display (ygg-git-compare--image-line item nil)))
                  lines)))
        (should (equal (sort fetched #'string<) '("https://x/1.png" "https://x/2.png")))
        (should (equal (car lines) "c (image not loaded: too many)"))))))

(ert-deftest ygg-git-compare-pr-info-image-cache-is-pruned-oldest-first-once ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((ygg-git-compare--image-pruned nil)
             (ygg-git-compare-image-cache-bytes 100)
             (old (ygg-git-compare-pr-info-tests--picture "old" ygg-git-compare-pr-info-tests--png))
             (mid (ygg-git-compare-pr-info-tests--picture "mid" ygg-git-compare-pr-info-tests--png))
             (new (ygg-git-compare-pr-info-tests--picture "new" ygg-git-compare-pr-info-tests--png))
             (real-make-process (symbol-function 'make-process)))
        (set-file-times old '(1000 0))
        (set-file-times mid '(2000 0))
        (set-file-times new '(3000 0))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (let ((process (funcall real-make-process :name "p" :buffer nil :noquery t
                                             :command '("true")
                                             :sentinel (plist-get args :sentinel))))
                       (while (process-live-p process) (accept-process-output process 0.05))
                       process))))
          (ygg-git-compare--image-curl "https://x.example/a.png" nil #'ignore)
          (should ygg-git-compare--image-pruned)
          (should-not (file-exists-p old))
          (should-not (file-exists-p mid))
          (should (file-exists-p new))
          (let ((again (ygg-git-compare-pr-info-tests--picture "again" ygg-git-compare-pr-info-tests--png)))
            (set-file-times again '(10 0))
            (ygg-git-compare--image-curl "https://x.example/b.png" nil #'ignore)
            (should (file-exists-p again))))))))

(ert-deftest ygg-git-compare-pr-info-image-found-accepts-only-raster-formats ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (make-directory ygg-git-compare-image-directory t)
      (let ((coding-system-for-write 'no-conversion))
        (dolist (case '(("s.svg" . "<?xml version=\"1.0\"?><svg xmlns=\"http://www.w3.org/2000/svg\"></svg>")
                        ("x.xpm" . "/* XPM */\nstatic char *a[] = {\"1 1 1 1\", \"a c #000\", \"a\"};")
                        ("b.xbm" . "#define a_width 1\n#define a_height 1\nstatic char a_bits[] = {0x00};")
                        ("p.pbm" . "P1\n1 1\n0\n")))
          (let ((file (expand-file-name (car case) ygg-git-compare-image-directory)))
            (write-region (cdr case) nil file nil 'silent)
            (should-not (ygg-git-compare--image-found file)))))
      (should (eq (plist-get (ygg-git-compare--image-found
                              (ygg-git-compare-pr-info-tests--picture
                               "a.png" ygg-git-compare-pr-info-tests--png))
                             :type)
                  'png)))))

(ert-deftest ygg-git-compare-pr-info-image-curl-removes-a-non-image-and-names-parts-by-pid ()
  (ygg-git-compare-pr-info-tests--with-repo head
    (ygg-git-compare-pr-info-tests--with-pictures
      (let* ((url "https://x.example/not-an-image")
             (cache (ygg-git-compare--image-cache-file url))
             (results nil)
             (output nil)
             (real-make-process (symbol-function 'make-process)))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (let* ((command (plist-get args :command))
                            (part (nth (1+ (cl-position "--output" command :test #'equal)) command))
                            (sentinel (plist-get args :sentinel))
                            (process (funcall real-make-process :name "t" :buffer nil
                                              :noquery t :command '("true"))))
                       (setq output part)
                       (set-process-sentinel process nil)
                       (let ((coding-system-for-write 'no-conversion))
                         (write-region "just text" nil part nil 'silent))
                       (while (process-live-p process) (accept-process-output process 0.05))
                       (funcall sentinel process "finished\n")
                       process))))
          (ygg-git-compare--image-curl url nil (lambda (found) (push (list found) results)))
          (should (equal results '((nil))))
          (should-not (file-exists-p cache))
          (should (string-search (format ".%d.part" (emacs-pid)) output))
          (should (string-prefix-p cache output)))))))
