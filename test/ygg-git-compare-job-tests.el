;;; ygg-git-compare-job-tests.el --- a CI job in Emacs -*- lexical-binding: t; -*-

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
(require 'ygg-git-compare-threads)
(require 'ygg-git-compare-pr-info)
(require 'ygg-git-compare-job)

(defconst ygg-git-compare-job-tests--gh-pr
  '(:forge github :host "github.com" :path "o/r" :number 7))

(defconst ygg-git-compare-job-tests--gl-pr
  '(:forge gitlab :host "gitlab.com" :path "grp/proj" :number 12))

(defconst ygg-git-compare-job-tests--gh-url
  "https://github.com/o/r/actions/runs/900/job/4242")

(defconst ygg-git-compare-job-tests--gl-url "https://gitlab.com/grp/proj/-/jobs/55")

(defconst ygg-git-compare-job-tests--gh-job
  (concat
   "{\"id\":4242,\"name\":\"build\",\"status\":\"completed\",\"conclusion\":\"failure\","
   "\"workflow_name\":\"CI\",\"started_at\":\"2026-01-02T03:04:05Z\","
   "\"completed_at\":\"2026-01-02T03:06:35Z\",\"html_url\":\"" ygg-git-compare-job-tests--gh-url "\","
   "\"steps\":[{\"name\":\"Set up job\",\"status\":\"completed\",\"conclusion\":\"success\","
   "\"number\":1,\"started_at\":\"2026-01-02T03:04:05Z\",\"completed_at\":\"2026-01-02T03:04:07Z\"},"
   "{\"name\":\"Run make test\",\"status\":\"completed\",\"conclusion\":\"failure\","
   "\"number\":2,\"started_at\":\"2026-01-02T03:04:07Z\",\"completed_at\":\"2026-01-02T03:06:07Z\"},"
   "{\"name\":\"Post job\",\"status\":\"completed\",\"conclusion\":\"skipped\",\"number\":3}]}"))

(defconst ygg-git-compare-job-tests--gh-log
  (concat
   "2026-01-02T03:04:05.1234567Z ##[group]Set up job\n"
   "2026-01-02T03:04:05.2000000Z Current runner version\n"
   "2026-01-02T03:04:05.3000000Z ##[endgroup]\n"
   "2026-01-02T03:04:07.0000000Z ##[group]Run make test\n"
   "2026-01-02T03:04:07.1000000Z \e[32mok\e[0m compiling\n"
   "2026-01-02T03:04:08.0000000Z ##[endgroup]\n"
   "2026-01-02T03:06:06.0000000Z \e[31mtest failed\e[0m\n"
   "2026-01-02T03:06:07.0000000Z ##[error]Process completed with exit code 2.\n"
   "2026-01-02T03:06:07.5000000Z FAILED tests/b\n"))

(defconst ygg-git-compare-job-tests--gl-job
  (concat "{\"id\":55,\"name\":\"unit\",\"stage\":\"test\",\"status\":\"failed\","
          "\"failure_reason\":\"script_failure\",\"started_at\":\"2026-01-02T03:04:05Z\","
          "\"finished_at\":\"2026-01-02T03:05:05Z\",\"web_url\":\"" ygg-git-compare-job-tests--gl-url "\"}"))

(defconst ygg-git-compare-job-tests--gl-trace
  (concat "section_start:1700000000:prepare_script[collapsed=true]\r\e[0K\e[0KPreparing\r\n"
          "section_end:1700000001:prepare_script\r\e[0K\n"
          "section_start:1700000002:step_script\r\e[0K\e[32;1m$ make test\e[0;m\r\n"
          "progress 10%\rprogress 100%\n"
          "\e[31;1mERROR: Job failed: exit code 1\e[0;m\r\n"
          "section_end:1700000003:step_script\r\e[0K\n"))

(defvar ygg-git-compare-job-tests--calls nil)

(defmacro ygg-git-compare-job-tests--with-forge (answers &rest body)
  "Run BODY with the forge helper answering from ANSWERS, an alist of
(SUFFIX-REGEXP . TEXT), and the calls recorded; no process starts."
  (declare (indent 1))
  `(let ((ygg-git-compare-job-tests--calls nil)
         (kill-ring nil))
     (cl-letf (((symbol-function 'display-buffer) #'ignore)
               ((symbol-function 'ygg-git-compare--forge-async)
                (lambda (program args callback &optional timeout)
                  (push (list program args timeout) ygg-git-compare-job-tests--calls)
                  (let ((hit (seq-find (lambda (a) (string-match-p (car a) (car (last args))))
                                       ,answers)))
                    (funcall callback (if hit 0 1) (or (cdr hit) "") (if hit "" "boom"))))))
       ,@body)))

(defun ygg-git-compare-job-tests--text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ygg-git-compare-job-tests--open (pr url name)
  (ygg-git-compare-job-show pr (list :name name :state 'failed :url url :where "CI")))

(defun ygg-git-compare-job-tests--gh-answers ()
  `(("/logs\\'" . ,ygg-git-compare-job-tests--gh-log)
    ("/jobs/4242\\'" . ,ygg-git-compare-job-tests--gh-job)))

(defun ygg-git-compare-job-tests--gl-answers ()
  `(("/trace\\'" . ,ygg-git-compare-job-tests--gl-trace)
    ("/jobs/55\\'" . ,ygg-git-compare-job-tests--gl-job)))

(ert-deftest ygg-git-compare-job-id-reads-both-forges ()
  (should (equal (ygg-git-compare-job-id ygg-git-compare-job-tests--gh-pr
                                         ygg-git-compare-job-tests--gh-url)
                 "4242"))
  (should (equal (ygg-git-compare-job-id ygg-git-compare-job-tests--gh-pr
                                         (concat ygg-git-compare-job-tests--gh-url "?pr=7"))
                 "4242"))
  (should-not (ygg-git-compare-job-id ygg-git-compare-job-tests--gh-pr "https://ci.example/e2e"))
  (should-not (ygg-git-compare-job-id ygg-git-compare-job-tests--gh-pr nil))
  (should (equal (ygg-git-compare-job-id ygg-git-compare-job-tests--gl-pr
                                         ygg-git-compare-job-tests--gl-url)
                 "55"))
  (should-not (ygg-git-compare-job-id ygg-git-compare-job-tests--gl-pr
                                      "https://gitlab.com/grp/proj/-/pipelines/9")))

(ert-deftest ygg-git-compare-job-github-shows-header-steps-and-log ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
      (unwind-protect
          (with-current-buffer buffer
            (let ((text (ygg-git-compare-job-tests--text)))
              (should (string-match-p "^build$" text))
              (should (string-match-p "Workflow +CI" text))
              (should (string-match-p "Status +✗ failed" text))
              (should (string-match-p "Conclusion +failure" text))
              (should (string-match-p "Started +2026-" text))
              (should (string-match-p "Duration +2m 30s" text))
              (should (string-match-p (regexp-quote ygg-git-compare-job-tests--gh-url) text))
              (should (string-match-p "✓  1 Set up job  passed  2s" text))
              (should (string-match-p "✗  2 Run make test  failed  2m 00s" text))
              (should (string-match-p "–  3 Post job  skipped" text))
              (should (string-match-p "^Log$" text))
              (should-not (string-match-p "##\\[group\\]\\|##\\[endgroup\\]\\|2026-01-02T03:04:05\\.1" text))
              (should (string-match-p "^##\\[error\\]Process completed" text))))
        (kill-buffer buffer)))
    (should (member '("gh" ("api" "--hostname" "github.com" "repos/o/r/actions/jobs/4242") nil)
                    ygg-git-compare-job-tests--calls))
    (should (seq-find (lambda (c) (equal (cadr c) '("api" "--hostname" "github.com"
                                                    "repos/o/r/actions/jobs/4242/logs")))
                      ygg-git-compare-job-tests--calls))))

(ert-deftest ygg-git-compare-job-ansi-colors-are-drawn-not-printed ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
      (unwind-protect
          (with-current-buffer buffer
            (should-not (string-match-p "\e\\|\\[32m" (ygg-git-compare-job-tests--text)))
            (goto-char (point-min))
            (search-forward "test failed")
            (should (get-text-property (match-beginning 0) 'face)))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-failed-job-opens-at-its-first-error ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
      (unwind-protect
          (with-current-buffer buffer
            (should (looking-at "##\\[error\\]Process completed")))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-error-keys-walk-error-lines ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (call-interactively (key-binding (kbd "] e")))
            (should (looking-at "##\\[error\\]"))
            (call-interactively (key-binding (kbd "] e")))
            (should (looking-at "FAILED tests/b"))
            (should-error (call-interactively (key-binding (kbd "] e"))) :type 'user-error)
            (call-interactively (key-binding (kbd "[ e")))
            (should (looking-at "##\\[error\\]"))
            (should-error (call-interactively (key-binding (kbd "[ e"))) :type 'user-error))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-ret-on-a-step-jumps-to-its-group ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "Run make test")
            (call-interactively (key-binding (kbd "RET")))
            (should (looking-at "Run make test$"))
            (should (> (point) ygg-git-compare-job--log-start))
            (goto-char (point-min))
            (search-forward "Post job")
            (should-error (call-interactively (key-binding (kbd "RET"))) :type 'user-error))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-timestamps-toggle ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
      (unwind-protect
          (with-current-buffer buffer
            (should-not (string-match-p "T03:04:05" (ygg-git-compare-job-tests--text)))
            (call-interactively (key-binding (kbd "t")))
            (should (string-match-p "2026-01-02T03:04:05.2000000Z Current runner"
                                    (ygg-git-compare-job-tests--text)))
            (call-interactively (key-binding (kbd "t")))
            (should-not (string-match-p "T03:04:05" (ygg-git-compare-job-tests--text))))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-gitlab-drops-section-markers ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gl-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gl-pr ygg-git-compare-job-tests--gl-url "unit")))
      (unwind-protect
          (with-current-buffer buffer
            (let ((text (ygg-git-compare-job-tests--text)))
              (should (string-match-p "Stage +test" text))
              (should (string-match-p "Conclusion +script_failure" text))
              (should-not (string-match-p "section_\\(start\\|end\\)" text))
              (should-not (string-match-p "\e\\|\r" text))
              (should (string-match-p "^Preparing$" text))
              (should (string-match-p "^\\$ make test$" text))
              (should (string-match-p "^progress 100%$" text))
              (should-not (string-match-p "progress 10%" text))
              (should (looking-at "ERROR: Job failed"))))
        (kill-buffer buffer)))
    (should (seq-find (lambda (c) (equal (cadr c) '("api" "--hostname" "gitlab.com"
                                                    "projects/grp%2Fproj/jobs/55/trace")))
                      ygg-git-compare-job-tests--calls))
    (should (seq-find (lambda (c) (equal (cadr c) '("api" "--hostname" "gitlab.com"
                                                    "projects/grp%2Fproj/jobs/55")))
                      ygg-git-compare-job-tests--calls))))

(ert-deftest ygg-git-compare-job-huge-logs-are-cut-from-the-front ()
  (let ((log (concat (mapconcat (lambda (i) (format "line %05d" i)) (number-sequence 1 2000) "\n")
                     "\n##[error]last\n"))
        (ygg-git-compare-job-log-max 500))
    (ygg-git-compare-job-tests--with-forge `(("/logs\\'" . ,log)
                                             ("/jobs/4242\\'" . ,ygg-git-compare-job-tests--gh-job))
      (let ((buffer (ygg-git-compare-job-tests--open
                     ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
        (unwind-protect
            (with-current-buffer buffer
              (let ((text (ygg-git-compare-job-tests--text)))
                (should (string-match-p "log cut: showing the last [0-9]+ of [0-9]+ characters" text))
                (should-not (string-match-p "line 00001" text))
                (should (string-match-p "line 02000" text))
                (should (< (length text) 1500))))
          (kill-buffer buffer))))
    (should (equal (ygg-git-compare-job--cap "abc\ndef\n" 100) '("abc\ndef\n" . 0)))
    (should (equal (ygg-git-compare-job--cap "abc\ndef\nghi\n" 6) '("ghi\n" . 8)))))

(ert-deftest ygg-git-compare-job-refresh-fetches-again-and-takes-the-new-state ()
  (let ((job ygg-git-compare-job-tests--gh-job))
    (ygg-git-compare-job-tests--with-forge `(("/logs\\'" . "first\n")
                                             ("/jobs/4242\\'" . ,job))
      (let ((buffer (ygg-git-compare-job-tests--open
                     ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p "^first$" (ygg-git-compare-job-tests--text)))
              (should (= (length ygg-git-compare-job-tests--calls) 2))
              (cl-letf (((symbol-function 'ygg-git-compare--forge-async)
                         (lambda (_program args callback &optional _timeout)
                           (funcall callback 0
                                    (if (string-suffix-p "/logs" (car (last args)))
                                        "second\n"
                                      (string-replace "\"failure\"" "\"success\"" job))
                                    ""))))
                (call-interactively (key-binding (kbd "g"))))
              (let ((text (ygg-git-compare-job-tests--text)))
                (should (string-match-p "^second$" text))
                (should-not (string-match-p "^first$" text))
                (should (string-match-p "Status +✓ passed" text))))
          (kill-buffer buffer))))))

(ert-deftest ygg-git-compare-job-forge-failure-is-shown-and-keeps-the-header ()
  (ygg-git-compare-job-tests--with-forge nil
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
      (unwind-protect
          (with-current-buffer buffer
            (let ((text (ygg-git-compare-job-tests--text)))
              (should (string-match-p "^build$" text))
              (should (string-match-p "job: boom" text))
              (should (string-match-p "log: boom" text))))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-running-job-polls-only-while-visible ()
  (let ((running (string-replace "\"completed\"" "\"in_progress\""
                                 ygg-git-compare-job-tests--gh-job)))
    (ygg-git-compare-job-tests--with-forge `(("/logs\\'" . "x\n") ("/jobs/4242\\'" . ,running))
      (let ((buffer (ygg-git-compare-job-tests--open
                     ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
        (unwind-protect
            (with-current-buffer buffer
              (should-not ygg-git-compare-job--timer)
              (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
                (ygg-git-compare-job--arm buffer)
                (should (timerp ygg-git-compare-job--timer))
                (ygg-git-compare-job--stop)
                (should-not ygg-git-compare-job--timer)))
          (kill-buffer buffer))))))

(defun ygg-git-compare-job-tests--timers ()
  (seq-count (lambda (timer) (eq (timer--function timer) #'ygg-git-compare-job--tick))
             timer-list))

(ert-deftest ygg-git-compare-job-showing-again-rearms-one-timer-while-running ()
  (let ((running (string-replace "\"completed\"" "\"in_progress\""
                                 ygg-git-compare-job-tests--gh-job))
        (visible nil)
        base)
    (ygg-git-compare-job-tests--with-forge `(("/logs\\'" . "x\n") ("/jobs/4242\\'" . ,running))
      (let ((buffer (ygg-git-compare-job-tests--open
                     ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build")))
        (unwind-protect
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) visible)))
                (ygg-git-compare-job--stop)
                (setq base (ygg-git-compare-job-tests--timers))
                (ygg-git-compare-job--shown)
                (should-not ygg-git-compare-job--timer)
                (setq visible t)
                (dotimes (_ 3) (ygg-git-compare-job--shown))
                (should (timerp ygg-git-compare-job--timer))
                (should (= (1+ base) (ygg-git-compare-job-tests--timers)))
                (setq visible nil)
                (cancel-timer ygg-git-compare-job--timer)
                (ygg-git-compare-job--tick buffer)
                (should-not ygg-git-compare-job--timer)
                (setq visible t)
                (dotimes (_ 3) (ygg-git-compare-job--shown))
                (should (= (1+ base) (ygg-git-compare-job-tests--timers)))
                (setq ygg-git-compare-job--details
                      (ygg-git-compare-job--parse ygg-git-compare-job-tests--gh-pr
                                                  ygg-git-compare-job-tests--gh-job))
                (ygg-git-compare-job--stop)
                (ygg-git-compare-job--shown)
                (should-not ygg-git-compare-job--timer)
                (should (= base (ygg-git-compare-job-tests--timers)))))
          (kill-buffer buffer))))))

(ert-deftest ygg-git-compare-job-id-wants-an-actions-job-url-on-the-pr-host ()
  (should-not (ygg-git-compare-job-id ygg-git-compare-job-tests--gh-pr
                                      "https://jenkins.example/job/77/"))
  (should-not (ygg-git-compare-job-id ygg-git-compare-job-tests--gh-pr
                                      "https://other.example/o/r/actions/runs/9/job/4"))
  (should (equal (ygg-git-compare-job-id ygg-git-compare-job-tests--gh-pr
                                         "https://github.com/o/r/actions/runs/9/job/4#step:2")
                 "4")))

(ert-deftest ygg-git-compare-job-placement-waits-for-details-in-either-order ()
  (dolist (order '((details log) (log details)))
    (let ((buffer (generate-new-buffer " *job-order*")))
      (unwind-protect
          (with-current-buffer buffer
            (ygg-git-compare-job-mode)
            (setq ygg-git-compare-job--pr ygg-git-compare-job-tests--gh-pr
                  ygg-git-compare-job--id "4242"
                  ygg-git-compare-job--check (list :name "build" :state 'running)
                  ygg-git-compare-job--strip t
                  ygg-git-compare-job--generation 1)
            (dolist (kind order)
              (ygg-git-compare-job--landed
               buffer 1 kind
               (if (eq kind 'log)
                   (list :raw ygg-git-compare-job-tests--gh-log :dropped 0)
                 (ygg-git-compare-job--parse ygg-git-compare-job-tests--gh-pr
                                             ygg-git-compare-job-tests--gh-job))
               nil))
            (should (looking-at "##\\[error\\]Process completed")))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-github-collapses-carriage-return-progress ()
  (let ((text (ygg-git-compare-job--log-text "2026-01-02T03:04:05Z fetch 10%\rfetch 100%\n" 'github t)))
    (should (equal (string-trim text) "fetch 100%"))))

(ert-deftest ygg-git-compare-job-q-kills-the-buffer ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build"))
          quit-kill)
      (with-current-buffer buffer
        (cl-letf (((symbol-function 'quit-window) (lambda (&optional kill &rest _) (setq quit-kill kill))))
          (call-interactively (key-binding (kbd "q")))))
      (should (eq quit-kill t))
      (kill-buffer buffer))))

(ert-deftest ygg-git-compare-job-unfinished-github-log-is-not-ready-not-an-error ()
  (let ((running (string-replace "\"completed\"" "\"in_progress\""
                                 ygg-git-compare-job-tests--gh-job)))
    (ygg-git-compare-job-tests--with-forge `(("/jobs/4242\\'" . ,running))
      (let ((buffer (ygg-git-compare-job-show
                     ygg-git-compare-job-tests--gh-pr
                     (list :name "build" :state 'running :url ygg-git-compare-job-tests--gh-url))))
        (unwind-protect
            (with-current-buffer buffer
              (should (string-match-p "log: not ready yet" (ygg-git-compare-job-tests--text)))
              (should (= ygg-git-compare-job--errors 0)))
          (kill-buffer buffer))))))

(ert-deftest ygg-git-compare-job-keys-work-in-normal-state ()
  (require 'yggdrasil)
  (let ((buffer (generate-new-buffer " *job-keys*")))
    (unwind-protect
        (with-current-buffer buffer
          (ygg-git-compare-job-mode)
          (let ((inhibit-read-only t)) (insert "a\nb\nc\n"))
          (goto-char (point-min))
          (yggdrasil-local-mode 1)
          (ygg-normal-state)
          (dolist (binding '(("g" . ygg-git-compare-job-refresh) ("o" . ygg-git-compare-job-browse)
                             ("y" . ygg-git-compare-job-copy)
                             ("t" . ygg-git-compare-job-toggle-timestamps)
                             ("] e" . ygg-git-compare-job-next-error)
                             ("[ e" . ygg-git-compare-job-previous-error)
                             ("RET" . ygg-git-compare-job-visit)
                             ("q" . ygg-git-compare-job-quit)))
            (should (eq (key-binding (kbd (car binding))) (cdr binding))))
          (should-not (eq (key-binding (kbd "j")) 'self-insert-command))
          (should (key-binding (kbd "j"))))
      (kill-buffer buffer))))

(ert-deftest ygg-git-compare-job-keymap ()
  (with-temp-buffer
    (ygg-git-compare-job-mode)
    (dolist (binding '(("g" . ygg-git-compare-job-refresh) ("o" . ygg-git-compare-job-browse)
                       ("y" . ygg-git-compare-job-copy) ("q" . ygg-git-compare-job-quit)
                       ("] e" . ygg-git-compare-job-next-error)
                       ("[ e" . ygg-git-compare-job-previous-error)
                       ("RET" . ygg-git-compare-job-visit)
                       ("t" . ygg-git-compare-job-toggle-timestamps)))
      (should (eq (key-binding (kbd (car binding))) (cdr binding))))))

(ert-deftest ygg-git-compare-job-o-browses-and-y-copies-the-job-url ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let ((buffer (ygg-git-compare-job-tests--open
                   ygg-git-compare-job-tests--gh-pr ygg-git-compare-job-tests--gh-url "build"))
          browsed)
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
              (call-interactively (key-binding (kbd "o"))))
            (should (equal browsed ygg-git-compare-job-tests--gh-url))
            (call-interactively (key-binding (kbd "y")))
            (should (equal (car kill-ring) ygg-git-compare-job-tests--gh-url)))
        (kill-buffer buffer)))))

(ert-deftest ygg-git-compare-job-check-ret-opens-a-job-and-o-the-browser ()
  (ygg-git-compare-job-tests--with-forge (ygg-git-compare-job-tests--gh-answers)
    (let (browsed shown)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url)))
                ((symbol-function 'ygg-git-compare--remote-pr)
                 (lambda () ygg-git-compare-job-tests--gh-pr))
                ((symbol-function 'ygg-git-compare--check-at-point)
                 (lambda () (list :name "build" :state 'failed
                                  :url ygg-git-compare-job-tests--gh-url))))
        (setq shown (ygg-git-compare-check-open))
        (unwind-protect
            (progn (should (bufferp shown))
                   (should-not browsed)
                   (ygg-git-compare-check-browse)
                   (should (equal browsed ygg-git-compare-job-tests--gh-url)))
          (when (bufferp shown) (kill-buffer shown))))
      (setq browsed nil)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url)))
                ((symbol-function 'ygg-git-compare--remote-pr)
                 (lambda () ygg-git-compare-job-tests--gh-pr))
                ((symbol-function 'ygg-git-compare--check-at-point)
                 (lambda () (list :name "e2e" :state 'passed :url "https://ci.example/e2e"))))
        (ygg-git-compare-check-open)
        (should (equal browsed "https://ci.example/e2e"))))))

(provide 'ygg-git-compare-job-tests)
;;; ygg-git-compare-job-tests.el ends here
