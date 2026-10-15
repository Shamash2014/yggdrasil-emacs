;;; ygg-git-ci-tests.el --- CI runs in magit status -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(load (expand-file-name "test-elpaca" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-ci)

(defvar ygg-git-ci-tests--repo '(github "github.com" "o/r"))
(defvar ygg-git-ci-tests--branch "feat")
(defvar ygg-git-ci-tests--upstream "origin/feat")
(defvar ygg-git-ci-tests--visible t)
(defvar ygg-git-ci-tests--row nil)
(defvar ygg-git-ci-tests--dir nil)
(defvar ygg-git-ci-tests--answers nil)

(defconst ygg-git-ci-tests--github
  (concat
   "[{\"databaseId\":3,\"workflowName\":\"Build\",\"status\":\"completed\",\"conclusion\":\"failure\",\"createdAt\":\"2026-10-05T10:05:00Z\",\"startedAt\":\"2026-10-05T10:05:00Z\",\"updatedAt\":\"2026-10-05T10:07:00Z\",\"headSha\":\"aaa\",\"event\":\"push\",\"url\":\"https://github.com/o/r/actions/runs/3\"},"
   "{\"databaseId\":2,\"workflowName\":\"Build\",\"status\":\"completed\",\"conclusion\":\"success\",\"createdAt\":\"2026-10-05T09:00:00Z\",\"startedAt\":\"2026-10-05T09:00:00Z\",\"updatedAt\":\"2026-10-05T09:01:00Z\",\"headSha\":\"old\",\"event\":\"push\",\"url\":\"https://github.com/o/r/actions/runs/2\"},"
   "{\"databaseId\":5,\"workflowName\":\"Lint\",\"status\":\"in_progress\",\"conclusion\":\"\",\"createdAt\":\"2026-10-05T10:04:00Z\",\"startedAt\":\"2026-10-05T10:04:00Z\",\"updatedAt\":\"2026-10-05T10:04:30Z\",\"headSha\":\"old\",\"event\":\"push\",\"url\":\"https://github.com/o/r/actions/runs/5\"},"
   "{\"databaseId\":6,\"workflowName\":\"Docs\",\"status\":\"completed\",\"conclusion\":\"success\",\"createdAt\":\"2026-10-05T10:03:00Z\",\"startedAt\":\"2026-10-05T10:03:00Z\",\"updatedAt\":\"2026-10-05T10:03:20Z\",\"headSha\":\"aaa\",\"event\":\"workflow_dispatch\",\"url\":\"https://github.com/o/r/actions/runs/6\"}]"))

(defconst ygg-git-ci-tests--pipelines
  "[{\"id\":900,\"sha\":\"aaa\",\"status\":\"running\",\"web_url\":\"https://gl.example/p/900\",\"created_at\":\"2026-10-05T10:00:00.000Z\"}]")

(defconst ygg-git-ci-tests--jobs
  (concat
   "[{\"id\":11,\"name\":\"test\",\"status\":\"success\",\"allow_failure\":false,\"created_at\":\"2026-10-05T10:00:00.000Z\",\"started_at\":\"2026-10-05T10:00:10.000Z\",\"finished_at\":\"2026-10-05T10:01:10.000Z\",\"web_url\":\"https://gl.example/j/11\"},"
   "{\"id\":12,\"name\":\"build\",\"status\":\"running\",\"allow_failure\":false,\"created_at\":\"2026-10-05T10:00:00.000Z\",\"started_at\":\"2026-10-05T10:01:10.000Z\",\"finished_at\":null,\"web_url\":\"https://gl.example/j/12\"},"
   "{\"id\":13,\"name\":\"deploy\",\"status\":\"manual\",\"allow_failure\":true,\"created_at\":\"2026-10-05T10:00:00.000Z\",\"started_at\":null,\"finished_at\":null,\"web_url\":\"https://gl.example/j/13\"}]"))

(defmacro ygg-git-ci-tests--with (&rest body)
  "Run BODY with empty caches and a stubbed forge; spawn records its calls as
(COMMAND . CALLBACK) in `spawned', run-at-time its timers in `timers'."
  (declare (indent 0))
  `(let ((ygg-git-ci--cache (make-hash-table :test 'equal))
         (ygg-git-ci--running (make-hash-table :test 'equal))
         (ygg-git-ci--watchers (make-hash-table :test 'equal))
         (ygg-git-ci--buffers (make-hash-table :test 'equal))
         (ygg-git-ci--timers (make-hash-table :test 'equal))
         (ygg-git-ci-status t)
         (ygg-git-ci-tests--repo '(github "github.com" "o/r"))
         (ygg-git-ci-tests--branch "feat")
         (ygg-git-ci-tests--upstream "origin/feat")
         (ygg-git-ci-tests--visible t)
         (ygg-git-ci-tests--row nil)
         (ygg-git-ci-tests--answers nil)
         (ygg-git-ci-tests--dir (make-temp-file "ygg-ci" t))
         (spawned nil)
         (timers nil)
         (messages nil)
         (refreshed 0))
     (unwind-protect
         (cl-letf (((symbol-function 'ygg-git-review-requests--repo)
                    (lambda () ygg-git-ci-tests--repo))
                   ((symbol-function 'magit-get-current-branch)
                    (lambda () ygg-git-ci-tests--branch))
                   ((symbol-function 'magit-get-upstream-branch)
                    (lambda (&rest _) ygg-git-ci-tests--upstream))
                   ((symbol-function 'magit-rev-parse) (lambda (&rest _) "aaa"))
                   ((symbol-function 'magit-toplevel)
                    (lambda (&rest _) ygg-git-ci-tests--dir))
                   ((symbol-function 'ygg-git-review-requests--spawn)
                    (lambda (command callback) (push (cons command callback) spawned) nil))
                   ((symbol-function 'get-buffer-window)
                    (lambda (&rest _) ygg-git-ci-tests--visible))
                   ((symbol-function 'run-at-time)
                    (lambda (secs _repeat fn &rest args)
                      (let ((timer (list secs fn args)))
                        (push timer timers)
                        timer)))
                   ((symbol-function 'magit-section-value-if)
                    (lambda (&rest _) ygg-git-ci-tests--row))
                   ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                   ((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                   ((symbol-function 'magit-refresh-buffer)
                    (lambda () (cl-incf refreshed) (ygg-git-ci-tests--render))))
           ,@body)
       (delete-directory ygg-git-ci-tests--dir t))))

(define-derived-mode ygg-git-ci-tests-mode magit-mode "ci-test")

(defun ygg-git-ci-tests--render ()
  (ygg-git-ci-tests-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (status)
      (ygg-git-ci-insert-section))))

(defun ygg-git-ci-tests--key ()
  (list ygg-git-ci-tests--repo ygg-git-ci-tests--branch))

(defun ygg-git-ci-tests--seed (rows)
  (puthash (ygg-git-ci-tests--key) (cons (float-time) (list :rows rows)) ygg-git-ci--cache))

(defun ygg-git-ci-tests--body (command)
  (with-temp-buffer
    (insert-file-contents (cadr (member "--input" command)))
    (buffer-string)))

(defun ygg-git-ci-tests--run (state &rest more)
  (append more (list :id 7 :name "Build" :state state :sha "aaa"
                     :created (- (float-time) 600)
                     :url "https://example.invalid/run/7")))

(defun ygg-git-ci-tests--workflows (dir)
  (let ((workflows (expand-file-name ".github/workflows" dir)))
    (make-directory workflows t)
    (with-temp-file (expand-file-name "deploy.yml" workflows)
      (insert "name: Deploy\non:\n  workflow_dispatch:\n    inputs:\n      env:\n"
              "        description: Target\n        default: stage\n        options:\n"
              "          - stage\n          - prod\n      tag:\n        description: Tag\n"
              "jobs:\n  x:\n    runs-on: ubuntu-latest\n"))
    (with-temp-file (expand-file-name "build.yml" workflows)
      (insert "name: Build\non:\n  push:\n    branches: [main]\n"))
    (with-temp-file (expand-file-name "bare.yaml" workflows)
      (insert "on: [workflow_dispatch]\n"))
    (with-temp-file (expand-file-name "off.yml" workflows)
      (insert "name: Off\non:\n  workflow_dispatch:\n"))))

(ert-deftest ygg-git-ci-parses-github-latest-run-per-workflow ()
  (let ((rows (ygg-git-ci--parse-github ygg-git-ci-tests--github)))
    (should (equal (mapcar (lambda (r) (plist-get r :name)) rows) '("Build" "Lint" "Docs")))
    (should (equal (mapcar (lambda (r) (plist-get r :state)) rows) '(failed running passed)))
    (should (= 3 (plist-get (car rows) :id)))
    (should (equal (plist-get (car rows) :url) "https://github.com/o/r/actions/runs/3"))
    (should (= 120 (- (plist-get (car rows) :finished) (plist-get (car rows) :started))))))

(ert-deftest ygg-git-ci-parses-gitlab-jobs-with-playable ()
  (let* ((pipeline (ygg-git-ci--parse-gitlab-pipeline ygg-git-ci-tests--pipelines))
         (rows (ygg-git-ci--parse-gitlab-jobs ygg-git-ci-tests--jobs pipeline)))
    (should (= 900 (plist-get pipeline :id)))
    (should (equal (mapcar (lambda (r) (plist-get r :state)) rows) '(passed running skipped)))
    (should (equal (mapcar (lambda (r) (plist-get r :playable)) rows) '(nil nil t)))
    (should (equal (plist-get (nth 1 rows) :url) "https://gl.example/j/12"))
    (should (= 900 (plist-get (car rows) :pipeline)))))

(ert-deftest ygg-git-ci-gitlab-pipeline-without-jobs-is-one-row ()
  (let* ((pipeline (ygg-git-ci--parse-gitlab-pipeline ygg-git-ci-tests--pipelines))
         (rows (ygg-git-ci--parse-gitlab-jobs "[]" pipeline)))
    (should (equal (mapcar (lambda (r) (plist-get r :kind)) rows) '(pipeline)))))

(ert-deftest ygg-git-ci-heading-summarises-and-marks-stale ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--seed (ygg-git-ci--parse-github ygg-git-ci-tests--github))
      (puthash (ygg-git-ci-tests--key) (cons (float-time) (list :rows (ygg-git-ci--parse-github ygg-git-ci-tests--github)))
               ygg-git-ci--cache)
      (ygg-git-ci-tests--render)
      (let ((text (buffer-string)))
        (should (string-match-p "^CI  feat  ✓ 1 · ✗ 1 · ● 1" text))
        (should (string-match-p "^  ✗ Build" text))
        (should (string-match-p "^  ✓ Docs" text))
        (should (string-match-p "^  ● Lint.*stale" text))
        (should-not (string-match-p "Build.*stale" text))))))

(ert-deftest ygg-git-ci-first-render-fetches-once-and-says-so ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (ygg-git-ci-tests--render)
      (should (string-match-p "CI  feat  (fetching…)" (buffer-string)))
      (should (= 1 (length spawned)))
      (let ((command (car (car spawned))))
        (should (equal (seq-take command 4) '("gh" "run" "list" "--repo")))
        (should (equal (nth 4 command) "github.com/o/r"))
        (should (equal (cadr (member "--branch" command)) "feat"))
        (should (string-match-p "databaseId,workflowName,status,conclusion,createdAt"
                                (cadr (member "--json" command))))))))

(ert-deftest ygg-git-ci-landing-refreshes-watchers-even-when-hidden ()
  (ygg-git-ci-tests--with
    (let ((hidden (generate-new-buffer " ci-hidden")))
      (unwind-protect
          (progn
            (with-current-buffer hidden (ygg-git-ci-tests--render))
            (setq ygg-git-ci-tests--visible nil)
            (funcall (cdr (car spawned)) 0 ygg-git-ci-tests--github nil)
            (should (= 1 refreshed))
            (with-current-buffer hidden
              (should (string-match-p "✗ 1" (buffer-string)))))
        (kill-buffer hidden)))))

(ert-deftest ygg-git-ci-github-error-shows-note-and-keeps-last-rows ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (funcall (cdr (car spawned)) 1 "" "gh auth login required\n")
      (should (string-match-p "ci: gh not authenticated" (buffer-string)))
      (ygg-git-ci-tests--seed (list (ygg-git-ci-tests--run 'passed)))
      (ygg-git-ci--store (ygg-git-ci-tests--key) (list :error "gh timed out"))
      (ygg-git-ci-tests--render)
      (should (string-match-p "✓ Build" (buffer-string)))
      (should (string-match-p "showing last runs" (buffer-string))))))

(ert-deftest ygg-git-ci-gitlab-fetches-pipeline-then-jobs ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (let ((first (car spawned)))
        (should (equal (car first)
                       '("glab" "api" "--hostname" "gl.example"
                         "projects/g%2Fr/pipelines?ref=feat&per_page=1")))
        (funcall (cdr first) 0 ygg-git-ci-tests--pipelines nil))
      (should (= 2 (length spawned)))
      (let ((second (car spawned)))
        (should (equal (car second)
                       '("glab" "api" "--hostname" "gl.example"
                         "projects/g%2Fr/pipelines/900/jobs?per_page=100&page=1")))
        (funcall (cdr second) 0 ygg-git-ci-tests--jobs nil))
      (let ((text (buffer-string)))
        (should (string-match-p "^CI  feat  ✓ 1 · ● 1" text))
        (should (string-match-p "^  ▶ deploy" text))
        (should (string-match-p "^  ✓ test" text))))))

(ert-deftest ygg-git-ci-no-forge-detached-and-no-upstream-do-not-error ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (setq ygg-git-ci-tests--repo nil)
      (ygg-git-ci-tests--render)
      (should (equal "" (buffer-string)))
      (setq ygg-git-ci-tests--repo '(github "github.com" "o/r")
            ygg-git-ci-tests--branch nil)
      (ygg-git-ci-tests--render)
      (should (string-match-p "ci: detached HEAD" (buffer-string)))
      (setq ygg-git-ci-tests--branch "feat" ygg-git-ci-tests--upstream nil)
      (ygg-git-ci-tests--render)
      (should (string-match-p "ci: no upstream" (buffer-string)))
      (should-not spawned)
      (should-error (ygg-git-ci-refetch) :type 'user-error))))

(ert-deftest ygg-git-ci-off-switch-inserts-nothing ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (let ((ygg-git-ci-status nil))
        (ygg-git-ci-tests--render))
      (should (equal "" (buffer-string)))
      (should-not spawned))))

(ert-deftest ygg-git-ci-builds-github-commands ()
  (let ((repo '(github "github.com" "o/r"))
        (row (ygg-git-ci-tests--run 'failed)))
    (should (equal (ygg-git-ci--command 'rerun repo row)
                   '("gh" "run" "rerun" "7" "--repo" "github.com/o/r")))
    (should (equal (ygg-git-ci--command 'rerun-failed repo row)
                   '("gh" "run" "rerun" "7" "--failed" "--repo" "github.com/o/r")))
    (should (equal (ygg-git-ci--command 'cancel repo row)
                   '("gh" "run" "cancel" "7" "--repo" "github.com/o/r")))
    (should (equal (ygg-git-ci--command 'log repo row)
                   '("gh" "run" "view" "7" "--log-failed" "--repo" "github.com/o/r")))
    (should (equal (ygg-git-ci--command 'log repo (ygg-git-ci-tests--run 'passed))
                   '("gh" "run" "view" "7" "--log" "--repo" "github.com/o/r")))))

(ert-deftest ygg-git-ci-builds-gitlab-commands ()
  (let ((repo '(gitlab "gl.example" "g/r"))
        (job (ygg-git-ci-tests--run 'failed :kind 'job :pipeline 900)))
    (should (equal (ygg-git-ci--command 'rerun repo job)
                   '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                     "projects/g%2Fr/jobs/7/retry")))
    (should (equal (ygg-git-ci--command 'cancel repo job)
                   '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                     "projects/g%2Fr/jobs/7/cancel")))
    (should (equal (ygg-git-ci--command 'rerun-pipeline repo job)
                   '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                     "projects/g%2Fr/pipelines/900/retry")))
    (should (equal (ygg-git-ci--command 'cancel-pipeline repo job)
                   '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                     "projects/g%2Fr/pipelines/900/cancel")))
    (should (equal (ygg-git-ci--command 'log repo job)
                   '("glab" "api" "--hostname" "gl.example" "projects/g%2Fr/jobs/7/trace")))
    (should (equal (ygg-git-ci--command 'rerun repo (ygg-git-ci-tests--run 'failed :kind 'pipeline))
                   '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                     "projects/g%2Fr/pipelines/7/retry")))))

(ert-deftest ygg-git-ci-rerun-of-a-failed-run-offers-failed-jobs-and-refetches ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (setq spawned nil ygg-git-ci-tests--row (ygg-git-ci-tests--run 'failed))
      (ygg-git-ci-rerun)
      (should (equal (car (car spawned))
                     '("gh" "run" "rerun" "7" "--failed" "--repo" "github.com/o/r")))
      (let ((callback (cdr (pop spawned))))
        (setq ygg-git-ci--running (make-hash-table :test 'equal))
        (funcall callback 0 "" nil))
      (should (member "ci: rerun failed jobs of Build done" messages))
      (should (equal (seq-take (car (car spawned)) 3) '("gh" "run" "list"))))))

(ert-deftest ygg-git-ci-rerun-declined-runs-nothing ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'passed))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (ygg-git-ci-rerun))
      (should-not spawned))))

(ert-deftest ygg-git-ci-rerun-of-a-passed-run-reruns-all ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'passed))
      (ygg-git-ci-rerun)
      (should (equal (car (car spawned))
                     '("gh" "run" "rerun" "7" "--repo" "github.com/o/r"))))))

(ert-deftest ygg-git-ci-cancel-needs-a-running-run-and-refetches ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (setq spawned nil ygg-git-ci-tests--row (ygg-git-ci-tests--run 'passed))
      (should-error (ygg-git-ci-cancel) :type 'user-error)
      (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'running))
      (ygg-git-ci-cancel)
      (should (equal (car (car spawned))
                     '("gh" "run" "cancel" "7" "--repo" "github.com/o/r")))
      (setq ygg-git-ci--running (make-hash-table :test 'equal))
      (funcall (cdr (pop spawned)) 1 "" "boom\n")
      (should (string-match-p "ci: cancel Build failed" (car (last messages 3))))
      (should (equal (seq-take (car (car spawned)) 3) '("gh" "run" "list"))))))

(ert-deftest ygg-git-ci-gitlab-retries-job-and-whole-pipeline ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'failed :kind 'job :pipeline 900))
      (ygg-git-ci-rerun)
      (should (equal (car (pop spawned))
                     '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                       "projects/g%2Fr/jobs/7/retry")))
      (ygg-git-ci-rerun-pipeline)
      (should (equal (car (pop spawned))
                     '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                       "projects/g%2Fr/pipelines/900/retry")))
      (ygg-git-ci-cancel-pipeline)
      (should (equal (car (pop spawned))
                     '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                       "projects/g%2Fr/pipelines/900/cancel"))))))

(ert-deftest ygg-git-ci-log-opens-a-buffer ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'failed))
      (ygg-git-ci-log)
      (should (equal (car (car spawned))
                     '("gh" "run" "view" "7" "--log-failed" "--repo" "github.com/o/r")))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
        (funcall (cdr (car spawned)) 0 "step output\n" nil))
      (unwind-protect
          (with-current-buffer "*ci log: Build*"
            (should (string-match-p "step output" (buffer-string))))
        (kill-buffer "*ci log: Build*")))))

(ert-deftest ygg-git-ci-browse-opens-the-run-url ()
  (ygg-git-ci-tests--with
    (let (opened)
      (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'passed))
      (cl-letf (((symbol-function 'browse-url) (lambda (url) (setq opened url))))
        (ygg-git-ci-browse))
      (should (equal opened "https://example.invalid/run/7")))))

(ert-deftest ygg-git-ci-trigger-lists-only-dispatchable-workflows ()
  (ygg-git-ci-tests--with
    (ygg-git-ci-tests--workflows ygg-git-ci-tests--dir)
    (let ((listing "[{\"id\":1,\"name\":\"Deploy\",\"state\":\"active\"},{\"id\":2,\"name\":\"Build\",\"state\":\"active\"},{\"id\":3,\"name\":\".github/workflows/bare.yaml\",\"state\":\"active\"},{\"id\":4,\"name\":\"Off\",\"state\":\"disabled_manually\"},{\"id\":5,\"name\":\"Gone\",\"state\":\"active\"}]"))
      (should (equal (mapcar (lambda (w) (plist-get w :name))
                             (ygg-git-ci--dispatchable listing ygg-git-ci-tests--dir))
                     '("Deploy" ".github/workflows/bare.yaml")))
      (with-temp-buffer
        (let (offered)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_p collection &rest _) (unless offered (setq offered collection)) (car collection)))
                    ((symbol-function 'read-string) (lambda (&rest _) "")))
            (ygg-git-ci-trigger)
            (should (equal (car (car spawned))
                           '("gh" "workflow" "list" "--repo" "github.com/o/r"
                             "--limit" "200" "--json" "name,id,state")))
            (funcall (cdr (pop spawned)) 0 listing nil))
          (should (equal offered '("workflow: Deploy" "workflow: .github/workflows/bare.yaml"))))))))

(ert-deftest ygg-git-ci-reads-workflow-dispatch-inputs ()
  (ygg-git-ci-tests--with
    (ygg-git-ci-tests--workflows ygg-git-ci-tests--dir)
    (let ((inputs (ygg-git-ci--inputs
                   (expand-file-name ".github/workflows/deploy.yml" ygg-git-ci-tests--dir))))
      (should (equal (mapcar (lambda (i) (plist-get i :name)) inputs) '("env" "tag")))
      (should (equal (plist-get (car inputs) :default) "stage"))
      (should (equal (plist-get (car inputs) :options) '("stage" "prod")))
      (should (equal (plist-get (cadr inputs) :description) "Tag"))
      (should-not (plist-get (cadr inputs) :options)))
    (should-not (ygg-git-ci--inputs
                 (expand-file-name ".github/workflows/bare.yaml" ygg-git-ci-tests--dir)))))

(ert-deftest ygg-git-ci-trigger-github-prompts-inputs-and-passes-them ()
  (ygg-git-ci-tests--with
    (ygg-git-ci-tests--workflows ygg-git-ci-tests--dir)
    (with-temp-buffer
      (ygg-git-ci-trigger)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (if (string-match-p "\\`env" prompt) "prod" (car collection))))
                ((symbol-function 'read-string) (lambda (_p &optional init &rest _) (or init ""))))
        (funcall (cdr (pop spawned)) 0
                 "[{\"id\":1,\"name\":\"Deploy\",\"state\":\"active\"}]" nil))
      (should (equal (car (car spawned))
                     '("gh" "workflow" "run" "1" "--repo" "github.com/o/r"
                       "--ref" "feat" "-f" "env=prod"))))))

(ert-deftest ygg-git-ci-startables-label-every-kind ()
  (let* ((rows (list (ygg-git-ci-tests--run 'passed)
                     (ygg-git-ci-tests--run 'skipped :name "deploy" :playable t)))
         (workflows (list (list :name "Deploy" :id 1 :file "/x/deploy.yml"))))
    (should (equal (mapcar #'car (ygg-git-ci--startables
                                  (list '(gitlab "gl.example" "g/r") "feat") rows nil))
                   '("job: deploy" "new pipeline: feat")))
    (should (equal (mapcar #'car (ygg-git-ci--startables
                                  (list '(github "github.com" "o/r") "feat") rows workflows))
                   '("job: deploy" "workflow: Deploy")))))

(ert-deftest ygg-git-ci-trigger-gitlab-offers-jobs-and-new-pipeline ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (ygg-git-ci-tests--seed (list (ygg-git-ci-tests--run 'skipped :name "deploy" :playable t :kind 'job)))
      (let (offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p collection &rest _) (setq offered collection) "new pipeline: feat"))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) (or (pop ygg-git-ci-tests--answers) ""))))
          (setq ygg-git-ci-tests--answers '("A:1" "B:2"))
          (ygg-git-ci-trigger))
        (should (equal offered '("job: deploy" "new pipeline: feat")))
        (should (equal (car (car spawned))
                       '("glab" "ci" "run" "-R" "https://gl.example/g/r" "--branch" "feat"
                         "--variables" "A:1" "--variables" "B:2")))))))

(ert-deftest ygg-git-ci-trigger-on-a-playable-row-plays-that-job ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (setq ygg-git-ci-tests--row
            (ygg-git-ci-tests--run 'skipped :name "deploy" :playable t :kind 'job))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (or (pop ygg-git-ci-tests--answers) ""))))
        (setq ygg-git-ci-tests--answers '("ENV:prod"))
        (ygg-git-ci-play))
      (let ((command (car (car spawned))))
        (should (equal (seq-take command 6)
                       '("glab" "api" "--hostname" "gl.example" "--method" "POST")))
        (should (equal (nth 6 command) "projects/g%2Fr/jobs/7/play"))
        (should (equal (seq-subseq command 7 9) '("--header" "Content-Type: application/json")))
        (should (equal (ygg-git-ci-tests--body command)
                       "{\"job_variables_attributes\":[{\"key\":\"ENV\",\"value\":\"prod\"}]}")))
      (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'passed))
      (should-error (ygg-git-ci-play) :type 'user-error))))

(ert-deftest ygg-git-ci-play-without-variables-sends-none ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (setq ygg-git-ci-tests--row
            (ygg-git-ci-tests--run 'skipped :playable t :kind 'job))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (ygg-git-ci-play))
      (should (equal (car (car spawned))
                     '("glab" "api" "--hostname" "gl.example" "--method" "POST"
                       "projects/g%2Fr/jobs/7/play"))))))

(ert-deftest ygg-git-ci-polls-while-running-and-visible ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (funcall (cdr (pop spawned)) 0 ygg-git-ci-tests--github nil)
      (let ((poll (seq-find (lambda (timer) (eq (nth 1 timer) 'ygg-git-ci--poll)) timers)))
        (should poll)
        (should (= 30 (car poll)))
        (should (= 1 (hash-table-count ygg-git-ci--timers)))
        (apply #'ygg-git-ci--poll (nth 2 poll))
        (should (= 1 (length spawned)))
        (should (equal (seq-take (car (car spawned)) 3) '("gh" "run" "list")))))))

(ert-deftest ygg-git-ci-polling-stops-when-done-or-hidden ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (funcall (cdr (pop spawned))
               0 (replace-regexp-in-string "in_progress" "completed" ygg-git-ci-tests--github) nil)
      (should-not (seq-find (lambda (timer) (eq (nth 1 timer) 'ygg-git-ci--poll)) timers)))
    (with-temp-buffer
      (setq timers nil)
      (clrhash ygg-git-ci--cache)
      (clrhash ygg-git-ci--running)
      (clrhash ygg-git-ci--timers)
      (ygg-git-ci-tests--render)
      (setq ygg-git-ci-tests--visible nil)
      (funcall (cdr (pop spawned)) 0 ygg-git-ci-tests--github nil)
      (should-not (seq-find (lambda (timer) (eq (nth 1 timer) 'ygg-git-ci--poll)) timers))
      (setq ygg-git-ci-tests--visible t)
      (ygg-git-ci--arm (ygg-git-ci-tests--key))
      (setq ygg-git-ci-tests--visible nil spawned nil)
      (ygg-git-ci--poll (ygg-git-ci-tests--key))
      (should-not spawned)
      (should (= 0 (hash-table-count ygg-git-ci--timers))))))

(ert-deftest ygg-git-ci-log-of-a-pipeline-row-is-refused ()
  (let ((repo '(gitlab "gl.example" "g/r")))
    (should-error (ygg-git-ci--command 'log repo (ygg-git-ci-tests--run 'running :kind 'pipeline))
                  :type 'user-error)
    (should (ygg-git-ci--command 'log repo (ygg-git-ci-tests--run 'running :kind 'job)))))

(ert-deftest ygg-git-ci-pipeline-run-names-the-confirmed-project ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (ygg-git-ci--start-spec (ygg-git-ci-tests--key) (list :kind 'pipeline)))
      (should (equal (car (car spawned))
                     '("glab" "ci" "run" "-R" "https://gl.example/g/r" "--branch" "feat"))))))

(ert-deftest ygg-git-ci-malformed-variables-are-refused ()
  (dolist (bad '("NOCOLON" ":value" ""))
    (should-error (ygg-git-ci--variables-json (list "A:1" bad)) :type 'user-error))
  (should (equal (ygg-git-ci--variables-json '("A:b:c" "E:"))
                 "{\"job_variables_attributes\":[{\"key\":\"A\",\"value\":\"b:c\"},{\"key\":\"E\",\"value\":\"\"}]}")))

(ert-deftest ygg-git-ci-play-body-keeps-placeholder-text-and-is-cleaned-up ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (setq ygg-git-ci-tests--row
            (ygg-git-ci-tests--run 'skipped :playable t :kind 'job))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (or (pop ygg-git-ci-tests--answers) ""))))
        (setq ygg-git-ci-tests--answers '("REF::branch :id"))
        (ygg-git-ci-play))
      (let* ((command (car (car spawned)))
             (file (cadr (member "--input" command))))
        (should-not (member "-F" command))
        (should (string-match-p (regexp-quote "\"value\":\":branch :id\"")
                                (ygg-git-ci-tests--body command)))
        (funcall (cdr (car spawned)) 0 "" nil)
        (should-not (file-exists-p file))))))

(ert-deftest ygg-git-ci-declined-play-removes-its-body ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (let* ((command (ygg-git-ci--command 'play ygg-git-ci-tests--repo
                                         (ygg-git-ci-tests--run 'skipped) "{}"))
           (file (cadr (member "--input" command))))
      (should (file-exists-p file))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (ygg-git-ci--do (ygg-git-ci-tests--key) "play" command "Play? "))
      (should-not (file-exists-p file))
      (should-not spawned))))

(ert-deftest ygg-git-ci-forced-refetch-replaces-an-inflight-poll ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (let ((poll (pop spawned)))
        (ygg-git-ci--ensure (ygg-git-ci-tests--key) t)
        (should (= 1 (length spawned)))
        (funcall (cdr poll) 0 "[]" nil)
        (should-not (plist-get (cdr (gethash (ygg-git-ci-tests--key) ygg-git-ci--cache)) :rows))
        (should (gethash (ygg-git-ci-tests--key) ygg-git-ci--running))
        (funcall (cdr (car spawned)) 0 ygg-git-ci-tests--github nil)
        (should (= 3 (length (plist-get (cdr (gethash (ygg-git-ci-tests--key) ygg-git-ci--cache))
                                        :rows))))
        (should-not (gethash (ygg-git-ci-tests--key) ygg-git-ci--running))))))

(ert-deftest ygg-git-ci-mutation-during-a-poll-refetches ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (let ((poll (pop spawned)))
        (setq ygg-git-ci-tests--row (ygg-git-ci-tests--run 'passed))
        (ygg-git-ci-rerun)
        (funcall (cdr (pop spawned)) 0 "" nil)
        (should (equal (seq-take (car (car spawned)) 3) '("gh" "run" "list")))
        (funcall (cdr poll) 0 "[]" nil)
        (funcall (cdr (car spawned)) 0 ygg-git-ci-tests--github nil)
        (should (= 3 (length (plist-get (cdr (gethash (ygg-git-ci-tests--key) ygg-git-ci--cache))
                                        :rows))))))))

(ert-deftest ygg-git-ci-a-poll-does-not-interrupt-a-question-in-flight ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (funcall (cdr (pop spawned)) 0 ygg-git-ci-tests--github nil)
      (ygg-git-ci--ensure (ygg-git-ci-tests--key) t)
      (setq spawned nil)
      (ygg-git-ci--poll (ygg-git-ci-tests--key))
      (should-not spawned))))

(ert-deftest ygg-git-ci-lists-more-runs-and-workflows ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (ygg-git-ci-tests--render)
      (should (equal (cadr (member "--limit" (car (car spawned)))) "100")))
    (with-temp-buffer
      (setq spawned nil)
      (ygg-git-ci-startable ygg-git-ci-tests--dir #'ignore)
      (should (equal (cadr (member "--limit" (car (car spawned)))) "200")))))

(ert-deftest ygg-git-ci-gitlab-jobs-follow-pages-up-to-a-cap ()
  (let ((page (concat "["
                      (mapconcat (lambda (i) (format "{\"id\":%d,\"name\":\"j%d\",\"status\":\"success\"}" i i))
                                 (number-sequence 1 100) ",")
                      "]")))
    (ygg-git-ci-tests--with
      (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
      (with-temp-buffer
        (ygg-git-ci-tests--render)
        (funcall (cdr (pop spawned)) 0 ygg-git-ci-tests--pipelines nil)
        (dotimes (n 4)
          (should (string-suffix-p (format "per_page=100&page=%d" (1+ n))
                                   (car (last (car (car spawned))))))
          (funcall (cdr (pop spawned)) 0 page nil))
        (should (string-suffix-p "per_page=100&page=5" (car (last (car (car spawned))))))
        (funcall (cdr (pop spawned)) 0 page nil)
        (should-not spawned)
        (should (= 500 (length (plist-get (cdr (gethash (ygg-git-ci-tests--key) ygg-git-ci--cache))
                                          :rows))))))
    (ygg-git-ci-tests--with
      (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
      (with-temp-buffer
        (ygg-git-ci-tests--render)
        (funcall (cdr (pop spawned)) 0 ygg-git-ci-tests--pipelines nil)
        (funcall (cdr (pop spawned)) 0 page nil)
        (funcall (cdr (pop spawned)) 0 ygg-git-ci-tests--jobs nil)
        (should-not spawned)
        (should (= 103 (length (plist-get (cdr (gethash (ygg-git-ci-tests--key) ygg-git-ci--cache))
                                          :rows))))))))

(ert-deftest ygg-git-ci-dead-buffers-are-pruned ()
  (ygg-git-ci-tests--with
    (let ((dead (generate-new-buffer " ci-dead"))
          (key (ygg-git-ci-tests--key)))
      (puthash key (list dead) ygg-git-ci--buffers)
      (kill-buffer dead)
      (with-temp-buffer
        (ygg-git-ci-tests-mode)
        (ygg-git-ci--ensure key)
        (should (equal (gethash key ygg-git-ci--buffers) (list (current-buffer)))))
      (let ((dead (generate-new-buffer " ci-dead")))
        (puthash key (list dead) ygg-git-ci--buffers)
        (kill-buffer dead)
        (ygg-git-ci--store key (list :rows nil))
        (should-not (gethash key ygg-git-ci--buffers))))))

(ert-deftest ygg-git-ci-enterprise-hosts-qualify-the-repo ()
  (let ((repo '(github "ghe.corp" "o/r")))
    (should (equal (ygg-git-ci--command 'rerun repo (ygg-git-ci-tests--run 'passed))
                   '("gh" "run" "rerun" "7" "--repo" "ghe.corp/o/r")))
    (ygg-git-ci-tests--with
      (setq ygg-git-ci-tests--repo repo)
      (with-temp-buffer
        (ygg-git-ci-tests--render)
        (should (equal (nth 4 (car (car spawned))) "ghe.corp/o/r"))))))

(ert-deftest ygg-git-ci-gitlab-job-without-a-name-is-called-job ()
  (let ((rows (ygg-git-ci--parse-gitlab-jobs
               "[{\"id\":1,\"name\":null,\"status\":\"success\"}]" '(:id 9))))
    (should (equal (plist-get (car rows) :name) "job"))))

(ert-deftest ygg-git-ci-startable-lists-what-t-offers ()
  (ygg-git-ci-tests--with
    (ygg-git-ci-tests--workflows ygg-git-ci-tests--dir)
    (ygg-git-ci-tests--seed (list (ygg-git-ci-tests--run 'skipped :name "deploy" :playable t :kind 'job)))
    (with-temp-buffer
      (let (items)
        (ygg-git-ci-startable ygg-git-ci-tests--dir (lambda (found &rest _) (setq items found)))
        (funcall (cdr (pop spawned)) 0
                 "[{\"id\":1,\"name\":\"Deploy\",\"state\":\"active\"}]" nil)
        (should (equal (mapcar (lambda (i) (list (plist-get i :kind) (plist-get i :name)
                                                 (plist-get i :label)))
                               items)
                       '((job "deploy" "job: deploy") (workflow "Deploy" "workflow: Deploy"))))))
    (with-temp-buffer
      (let (items err)
        (ygg-git-ci-startable ygg-git-ci-tests--dir (lambda (found &optional why)
                                                      (setq items found err why)))
        (funcall (cdr (pop spawned)) 1 "" "gh auth login required\n")
        (should-not items)
        (should (stringp err))))
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (ygg-git-ci-tests--seed (list (ygg-git-ci-tests--run 'skipped :name "deploy" :playable t :kind 'job)))
    (let (items)
      (ygg-git-ci-startable ygg-git-ci-tests--dir (lambda (found &rest _) (setq items found)))
      (should (equal (mapcar (lambda (i) (plist-get i :kind)) items) '(job pipeline)))
      (should (equal (plist-get (cadr items) :name) "feat")))))

(ert-deftest ygg-git-ci-start-runs-an-item-as-t-does ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (or (pop ygg-git-ci-tests--answers) ""))))
        (setq ygg-git-ci-tests--answers '("A:1"))
        (ygg-git-ci-start ygg-git-ci-tests--dir
                          (list :kind 'pipeline :name "feat" :label "new pipeline: feat")))
      (should (equal (car (pop spawned))
                     '("glab" "ci" "run" "-R" "https://gl.example/g/r" "--branch" "feat"
                       "--variables" "A:1")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'read-string) (lambda (&rest _) "")))
        (ygg-git-ci-start ygg-git-ci-tests--dir
                          (list :kind 'pipeline :name "feat" :label "new pipeline: feat")))
      (should-not spawned))))

(ert-deftest ygg-git-ci-startable-answers-once-when-the-key-is-refused ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo nil)
    (let (calls)
      (ygg-git-ci-startable ygg-git-ci-tests--dir (lambda (&rest args) (push args calls)))
      (should (= 1 (length calls)))
      (should-not (car (car calls)))
      (should (stringp (cadr (car calls))))
      (should-not spawned)
      (should-not (member "ci: listing workflows…" messages)))))

(ert-deftest ygg-git-ci-startable-answers-once-when-the-workflow-list-is-unreadable ()
  (ygg-git-ci-tests--with
    (let (calls)
      (ygg-git-ci-startable ygg-git-ci-tests--dir (lambda (&rest args) (push args calls)))
      (funcall (cdr (pop spawned)) 0 "not json" nil)
      (should (= 1 (length calls)))
      (should-not (car (car calls)))
      (should (stringp (cadr (car calls)))))))

(ert-deftest ygg-git-ci-mutation-timeout-kills-the-process-and-removes-its-body ()
  (ygg-git-ci-tests--with
    (let* ((command (ygg-git-ci--command 'play '(gitlab "gl.example" "g/r")
                                         (ygg-git-ci-tests--run 'skipped) "{}"))
           (file (cadr (member "--input" command)))
           killed)
      (cl-letf (((symbol-function 'ygg-git-review-requests--spawn)
                 (lambda (_command _callback) 'fake-process))
                ((symbol-function 'ygg-git-review-requests--kill)
                 (lambda (process) (push process killed))))
        (ygg-git-ci--do (ygg-git-ci-tests--key) "play" command "Play? ")
        (should (file-exists-p file))
        (let ((timer (car timers)))
          (should (= ygg-git-ci-timeout (car timer)))
          (apply (cadr timer) (nth 2 timer))))
      (should (equal killed '(fake-process)))
      (should-not (file-exists-p file)))))

(ert-deftest ygg-git-ci-quitting-at-the-confirmation-removes-the-body ()
  (ygg-git-ci-tests--with
    (let* ((command (ygg-git-ci--command 'play '(gitlab "gl.example" "g/r")
                                         (ygg-git-ci-tests--run 'skipped) "{}"))
           (file (cadr (member "--input" command))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (signal 'quit nil))))
        (should (eq 'quit (condition-case err
                              (ygg-git-ci--do (ygg-git-ci-tests--key) "play" command "Play? ")
                            (quit (car err))))))
      (should-not (file-exists-p file))
      (should-not spawned))))

(ert-deftest ygg-git-ci-pipeline-variables-are-validated-before-confirming ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (let (asked)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (or (pop ygg-git-ci-tests--answers) "")))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) (setq asked t))))
        (dolist (bad '("NOCOLON" ":value"))
          (setq ygg-git-ci-tests--answers (list bad))
          (should-error (ygg-git-ci--start-spec (ygg-git-ci-tests--key) (list :kind 'pipeline))
                        :type 'user-error)))
      (should-not asked)
      (should-not spawned))))

(ert-deftest ygg-git-ci-gitlab-later-page-failure-keeps-earlier-rows ()
  (let ((page (concat "["
                      (mapconcat (lambda (i) (format "{\"id\":%d,\"name\":\"j%d\",\"status\":\"success\"}" i i))
                                 (number-sequence 1 100) ",")
                      "]")))
    (dolist (failure '((1 "" "boom\n") (0 "not json" nil)))
      (ygg-git-ci-tests--with
        (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
        (with-temp-buffer
          (ygg-git-ci-tests--render)
          (funcall (cdr (pop spawned)) 0 ygg-git-ci-tests--pipelines nil)
          (funcall (cdr (pop spawned)) 0 page nil)
          (apply (cdr (pop spawned)) failure)
          (let ((answer (cdr (gethash (ygg-git-ci-tests--key) ygg-git-ci--cache))))
            (should (= 100 (length (plist-get answer :rows))))
            (should (stringp (plist-get answer :error)))))))))

(ert-deftest ygg-git-ci-synchronous-failure-while-rendering-does-not-refresh-reentrantly ()
  (ygg-git-ci-tests--with
    (with-temp-buffer
      (cl-letf (((symbol-function 'ygg-git-review-requests--spawn)
                 (lambda (_command callback) (funcall callback nil "" nil) nil)))
        (ygg-git-ci-tests--render))
      (should (= 0 refreshed))
      (should (cl-find #'ygg-git-ci--refresh timers :key #'cadr))
      (apply #'ygg-git-ci--refresh (nth 2 (cl-find #'ygg-git-ci--refresh timers :key #'cadr)))
      (should (= 1 refreshed))
      (should (string-match-p "gh not found" (buffer-string))))))

(ert-deftest ygg-git-ci-startables-with-the-same-name-get-distinct-labels-and-play-the-chosen ()
  (ygg-git-ci-tests--with
    (setq ygg-git-ci-tests--repo '(gitlab "gl.example" "g/r"))
    (ygg-git-ci-tests--seed
     (list (ygg-git-ci-tests--run 'skipped :name "deploy" :playable t :kind 'job :id 123 :stage "prod")
           (ygg-git-ci-tests--run 'skipped :name "deploy" :playable t :kind 'job :id 124 :stage "stage")))
    (with-temp-buffer
      (ygg-git-ci-tests-mode)
      (let (items offered)
        (ygg-git-ci-startable ygg-git-ci-tests--dir (lambda (found &rest _) (setq items found)))
        (should (equal (mapcar (lambda (i) (plist-get i :label)) items)
                       '("job: deploy (stage prod, #123)" "job: deploy (stage stage, #124)"
                         "new pipeline: feat")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p collection &rest _) (setq offered collection) (cadr collection)))
                  ((symbol-function 'read-string) (lambda (&rest _) "")))
          (ygg-git-ci--choose ygg-git-ci-tests--dir (ygg-git-ci-tests--key) items nil))
        (should (member "projects/g%2Fr/jobs/124/play" (car (car spawned))))))))

(ert-deftest ygg-git-ci-only-magit-buffers-are-watchers ()
  (ygg-git-ci-tests--with
    (let ((key (ygg-git-ci-tests--key)))
      (with-temp-buffer
        (ygg-git-ci--ensure key)
        (should-not (gethash key ygg-git-ci--watchers))
        (should-not (gethash key ygg-git-ci--buffers)))
      (clrhash ygg-git-ci--running)
      (with-temp-buffer
        (ygg-git-ci-tests-mode)
        (ygg-git-ci--ensure key)
        (should (equal (gethash key ygg-git-ci--watchers) (list (current-buffer))))
        (should (equal (gethash key ygg-git-ci--buffers) (list (current-buffer))))))))

(ert-deftest ygg-git-ci-old-log-buffers-are-killed-past-the-cap ()
  (let ((ygg-git-ci-log-buffers 2) buffers)
    (unwind-protect
        (progn
          (dotimes (i 4)
            (let ((b (generate-new-buffer (format "*ci log: t%d*" i))))
              (with-current-buffer b
                (setq ygg-git-ci--log-p t
                      buffer-display-time (time-add (current-time) i)))
              (push b buffers)))
          (set-window-buffer (selected-window) (car (last buffers)))
          (ygg-git-ci--gc-logs)
          (should (equal (mapcar #'buffer-live-p (reverse buffers)) '(t nil t t))))
      (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b))) buffers))))

(provide 'ygg-git-ci-tests)
;;; ygg-git-ci-tests.el ends here
