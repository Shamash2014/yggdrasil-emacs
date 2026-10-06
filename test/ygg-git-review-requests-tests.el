;;; ygg-git-review-requests-tests.el --- review requests in magit status -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-review-requests)

(defvar ygg-git-review-requests-tests--fake-dir nil)

(defmacro ygg-git-review-requests-tests--with-fake-forge (&rest body)
  (declare (indent 0))
  `(let* ((ygg-git-review-requests-tests--fake-dir (make-temp-file "ygg-rr-fake" t))
          (exec-path (cons ygg-git-review-requests-tests--fake-dir exec-path))
          (process-environment
           (cons (concat "PATH=" ygg-git-review-requests-tests--fake-dir ":" (getenv "PATH"))
                 process-environment)))
     (unwind-protect
         (progn
           (dolist (name '("gh" "glab"))
             (let ((script (expand-file-name name ygg-git-review-requests-tests--fake-dir)))
               (with-temp-file script (insert "#!/bin/sh\necho fake >&2\nexit 99\n"))
               (set-file-modes script #o755)
               (unless (equal (executable-find name) script)
                 (error "fake %s is not first on exec-path" name))))
           ,@body)
       (delete-directory ygg-git-review-requests-tests--fake-dir t))))

(defmacro ygg-git-review-requests-tests--deftest (name &rest body)
  (declare (indent 1))
  `(ert-deftest ,name ()
     (ygg-git-review-requests-tests--with-fake-forge ,@body)))

(defconst ygg-git-review-requests-tests--github
  "[{\"number\":12,\"title\":\"Add thing\",\"author\":{\"login\":\"ann\"},\"headRefName\":\"feat\",\"baseRefName\":\"main\",\"url\":\"https://github.com/o/r/pull/12\",\"isDraft\":true,\"updatedAt\":\"2026-10-05T10:00:00Z\"}]")

(defconst ygg-git-review-requests-tests--gitlab
  "[{\"iid\":7,\"title\":\"Fix it\",\"author\":{\"username\":\"bob\"},\"source_branch\":\"fix\",\"target_branch\":\"dev\",\"web_url\":\"https://gl.example/o/r/-/merge_requests/7\",\"draft\":false,\"updated_at\":\"2026-10-05T10:00:00.123Z\"}]")

(defvar ygg-git-review-requests-tests--repo '(github "github.com" "o/r"))

(defmacro ygg-git-review-requests-tests--with (&rest body)
  "Run BODY with empty caches, a stubbed forge and spawn recording its calls
as (COMMAND . CALLBACK) in `spawned'."
  (declare (indent 0))
  `(let ((ygg-git-review-requests--cache (make-hash-table :test 'equal))
         (ygg-git-review-requests--running (make-hash-table :test 'equal))
         (ygg-git-review-requests--watchers (make-hash-table :test 'equal))
         (ygg-git-review-requests--users (make-hash-table :test 'equal))
         (ygg-git-review-requests--lookups (make-hash-table :test 'equal))
         (ygg-git-review-requests--repos (make-hash-table :test 'equal))
         (ygg-git-review-requests-file nil)
         (ygg-git-review-requests t)
         (spawned nil)
         (refreshed 0))
     (cl-letf (((symbol-function 'ygg-git-compare--remote) (lambda () "origin"))
               ((symbol-function 'magit-get) (lambda (&rest _) "url"))
               ((symbol-function 'magit-toplevel) (lambda (&rest _) "/top/"))
               ((symbol-function 'ygg-git-compare--forge-repo)
                (lambda (&optional _) ygg-git-review-requests-tests--repo))
               ((symbol-function 'ygg-git-review-requests--spawn)
                (lambda (command callback) (push (cons command callback) spawned) nil))
               ((symbol-function 'get-buffer-window) (lambda (&rest _) t))
               ((symbol-function 'magit-refresh-buffer)
                (lambda () (cl-incf refreshed)
                  (ygg-git-review-requests-tests--render))))
       ,@body)))

(defun ygg-git-review-requests-tests--render ()
  (magit-section-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (status)
      (ygg-git-review-requests-insert-section))))

(defun ygg-git-review-requests-tests--seed (rows)
  (puthash ygg-git-review-requests-tests--repo
           (cons (float-time) (list :rows rows))
           ygg-git-review-requests--cache))

(defun ygg-git-review-requests-tests--row ()
  (list :number 12 :title "Add thing" :author "ann" :head "feat" :base "main"
        :url "https://example.invalid/pull/12" :draft nil
        :updated (- (float-time) 7200)))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-fresh-cache-starts-nothing
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--seed (list (ygg-git-review-requests-tests--row)))
      (ygg-git-review-requests-tests--render)
      (should-not spawned)
      (should (string-match-p "Review requests (1)" (buffer-string)))
      (should (string-match-p "#12  Add thing  ann  feat→main  2h" (buffer-string))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-stale-cache-fetches-once
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (puthash ygg-git-review-requests-tests--repo
               (cons (- (float-time) 1000) (list :rows nil))
               ygg-git-review-requests--cache)
      (ygg-git-review-requests-tests--render)
      (ygg-git-review-requests-tests--render)
      (should (= 1 (length spawned)))
      (should (string-match-p "Review requests (0)" (buffer-string))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-first-fetch-says-fetching
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (should (string-match-p "Review requests (fetching…)" (buffer-string)))
      (should (= 1 (length spawned)))
      (should (member "--search" (car (car spawned)))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-completion-refreshes-once-without-loop
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (funcall (cdr (car spawned)) 0 ygg-git-review-requests-tests--github)
      (should (= 1 refreshed))
      (should (= 1 (length spawned)))
      (should-not (gethash ygg-git-review-requests-tests--repo
                           ygg-git-review-requests--running))
      (should (string-match-p "Review requests (1)" (buffer-string)))
      (should (string-match-p "#12  Add thing  ann  feat→main" (buffer-string))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-parses-github
  (let ((row (car (ygg-git-review-requests--parse
                   'github ygg-git-review-requests-tests--github))))
    (should (equal (seq-take row 14)
                   '(:number 12 :title "Add thing" :author "ann" :head "feat"
                     :base "main" :url "https://github.com/o/r/pull/12" :draft t)))
    (should (numberp (plist-get row :updated)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-parses-gitlab
  (let ((row (car (ygg-git-review-requests--parse
                   'gitlab ygg-git-review-requests-tests--gitlab))))
    (should (equal (seq-take row 10) '(:number 7 :title "Fix it" :author "bob" :head "fix" :base "dev")))
    (should (equal (plist-get row :url) "https://gl.example/o/r/-/merge_requests/7"))
    (should (numberp (plist-get row :updated)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-gitlab-asks-for-user-then-list
  (let ((ygg-git-review-requests-tests--repo '(gitlab "gl.example" "o/r")))
    (ygg-git-review-requests-tests--with
      (with-temp-buffer
        (ygg-git-review-requests-tests--render)
        (should (equal (car (car spawned)) '("glab" "api" "--hostname" "gl.example" "user")))
        (funcall (cdr (car spawned)) 0 "{\"username\":\"me\"}")
        (let ((list-command (car (car spawned))))
          (should (equal (car (last list-command))
                         "projects/o%2Fr/merge_requests?state=opened&per_page=100&reviewer_username=me")))
        (funcall (cdr (car spawned)) 0 ygg-git-review-requests-tests--gitlab)
        (should (string-match-p "Review requests (1)" (buffer-string)))
        (should (string-match-p "#7  Fix it  bob  fix→dev" (buffer-string)))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-failure-lines
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (funcall (cdr (car spawned)) 1 "" "To get started, run: gh auth login")
      (should (string-match-p "reviews: gh not authenticated" (buffer-string))))
    (clrhash ygg-git-review-requests--cache)
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (funcall (cdr (car spawned)) nil "")
      (should (string-match-p "reviews: gh not found" (buffer-string))))
    (clrhash ygg-git-review-requests--cache)
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (funcall (cdr (car spawned)) 0 "not json")
      (should (string-match-p "reviews: gh answered nothing readable" (buffer-string))))
    (clrhash ygg-git-review-requests--repos)
    (cl-letf (((symbol-function 'ygg-git-compare--forge-repo)
               (lambda (&optional _) (user-error "x is neither GitHub nor GitLab"))))
      (with-temp-buffer
        (setq spawned nil)
        (ygg-git-review-requests-tests--render)
        (should-not spawned)
        (should (string-match-p "reviews: not a forge remote" (buffer-string)))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-spawn-reports-a-missing-program
  (let (got)
    (ygg-git-review-requests--spawn '("ygg-no-such-program-xyz") (lambda (s &rest _) (setq got (list s))))
    (should (equal got '(nil)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-spawn-runs-in-the-background
  (let (got)
    (ygg-git-review-requests--spawn '("printf" "hi") (lambda (s text &rest _) (setq got (list s text))))
    (should-not got)
    (with-timeout (5 (ert-fail "no answer"))
      (while (not got) (accept-process-output nil 0.05)))
    (should (equal got '(0 "hi")))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-off-inserts-nothing
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (let ((ygg-git-review-requests nil))
        (ygg-git-review-requests-tests--render))
      (should (string-empty-p (buffer-string)))
      (should-not spawned))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-refetch-ignores-the-ttl
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--seed (list (ygg-git-review-requests-tests--row)))
      (ygg-git-review-requests-refetch)
      (should (= 1 (length spawned))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-args-ask-for-a-hundred
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (should (equal (car (member "--limit" (car (car spawned)))) "--limit"))
      (should (equal (cadr (member "--limit" (car (car spawned)))) "100"))))
  (let ((ygg-git-review-requests-tests--repo '(gitlab "gl.example" "o/r")))
    (ygg-git-review-requests-tests--with
      (puthash "gl.example" "me" ygg-git-review-requests--users)
      (with-temp-buffer
        (ygg-git-review-requests-tests--render)
        (should (string-match-p "&per_page=100&" (car (last (car (car spawned))))))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-null-branch-renders-a-question-mark
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--seed
       (list (plist-put (ygg-git-review-requests-tests--row) :head nil)))
      (ygg-git-review-requests-tests--render)
      (should (string-match-p "?→main" (buffer-string)))
      (should-not (string-match-p "nil" (buffer-string))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-gitlab-work-in-progress-is-draft
  (let ((row (car (ygg-git-review-requests--parse
                   'gitlab "[{\"iid\":1,\"work_in_progress\":true,\"draft\":false}]"))))
    (should (plist-get row :draft))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-refetch-shows-fetching-at-once
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--seed (list (ygg-git-review-requests-tests--row)))
      (ygg-git-review-requests-refetch)
      (should (string-match-p "Review requests (fetching…)" (buffer-string))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-expiry-kills-and-reports
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (let ((process (start-process "ygg-sleeper" nil "sleep" "5"))
            killed)
        (unwind-protect
            (cl-letf (((symbol-function 'ygg-git-review-requests--kill)
                       (lambda (p) (setq killed p))))
              (ygg-git-review-requests-tests--render)
              (let ((cell (gethash ygg-git-review-requests-tests--repo
                                   ygg-git-review-requests--running)))
                (setf (nth 1 cell) process)
                (ygg-git-review-requests--expire ygg-git-review-requests-tests--repo cell)
                (should (eq killed process))
                (should-not (gethash ygg-git-review-requests-tests--repo
                                     ygg-git-review-requests--running))
                (should (string-match-p "reviews: gh timed out" (buffer-string)))))
          (delete-process process))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-timer-expires-a-hung-fetch
  (let ((ygg-git-review-requests-timeout 0.2)
        process)
    (ygg-git-review-requests-tests--with
      (cl-letf (((symbol-function 'ygg-git-review-requests--spawn)
                 (lambda (&rest _)
                   (setq process (start-process "ygg-sleeper" nil "sleep" "5")))))
        (with-temp-buffer
          (ygg-git-review-requests-tests--render)
          (with-timeout (3 (ert-fail "no timeout"))
            (while (gethash ygg-git-review-requests-tests--repo
                            ygg-git-review-requests--running)
              (accept-process-output nil 0.05)))
          (should-not (process-live-p process))
          (should (equal (cdr (gethash ygg-git-review-requests-tests--repo
                                       ygg-git-review-requests--cache))
                         '(:error "gh timed out"))))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-signal-after-puthash-clears-running
  (ygg-git-review-requests-tests--with
    (cl-letf (((symbol-function 'ygg-git-review-requests--spawn)
               (lambda (&rest _) (error "boom"))))
      (should-error (ygg-git-review-requests--ensure ygg-git-review-requests-tests--repo))
      (should-not (gethash ygg-git-review-requests-tests--repo
                           ygg-git-review-requests--running)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-refetch-replaces-a-stale-fetch
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (let ((stale (list (- (float-time) 100) nil nil))
            (process (start-process "ygg-sleeper" nil "sleep" "5"))
            killed)
        (setf (nth 1 stale) process)
        (puthash ygg-git-review-requests-tests--repo stale
                 ygg-git-review-requests--running)
        (unwind-protect
            (cl-letf (((symbol-function 'ygg-git-review-requests--kill)
                       (lambda (p) (setq killed p))))
              (ygg-git-review-requests-refetch)
              (should (eq killed process))
              (should (= 1 (length spawned))))
          (delete-process process))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-refetch-keeps-a-fresh-fetch
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (ygg-git-review-requests-refetch)
      (should (= 1 (length spawned))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-render-runs-no-forge-process
  (let* ((root (make-temp-file "ygg-rr" t))
         (repo (expand-file-name "repo/" root))
         (conf (expand-file-name "conf/" root))
         (process-environment
          (append (list (concat "GH_CONFIG_DIR=" conf)
                        (concat "XDG_CONFIG_HOME=" conf)
                        (concat "GLAB_CONFIG_DIR=" conf)
                        "LAB_HOST=")
                  process-environment))
         (native-comp-enable-subr-trampolines nil)
         (real-process-file (symbol-function 'process-file))
         (real-call-process (symbol-function 'call-process))
         (ygg-git-review-requests--cache (make-hash-table :test 'equal))
         (ygg-git-review-requests--running (make-hash-table :test 'equal))
         (ygg-git-review-requests--watchers (make-hash-table :test 'equal))
         (ygg-git-review-requests--repos (make-hash-table :test 'equal))
         (ygg-git-review-requests t)
         spawned)
    (unwind-protect
        (progn
          (make-directory repo)
          (make-directory conf)
          (let ((default-directory repo))
            (call-process "git" nil nil nil "init" "-q")
            (call-process "git" nil nil nil "remote" "add" "origin" "git@github.com:o/r.git"))
          (let ((default-directory repo))
            (cl-letf (((symbol-function 'make-process)
                       (lambda (&rest _) (ert-fail "make-process")))
                      ((symbol-function 'accept-process-output)
                       (lambda (&rest _) (ert-fail "accept-process-output")))
                      ((symbol-function 'call-process)
                       (lambda (program &rest args)
                         (if (equal (file-name-nondirectory program) "git")
                             (apply real-call-process program args)
                           (error "process %s" program))))
                      ((symbol-function 'process-file)
                       (lambda (program &rest args)
                         (if (equal (file-name-nondirectory program) "git")
                             (apply real-process-file program args)
                           (error "process %s" program))))
                      ((symbol-function 'ygg-git-review-requests--spawn)
                       (lambda (command _) (push command spawned) nil)))
              (with-temp-buffer
                (ygg-git-review-requests-tests--render)
                (should (string-match-p "Review requests (fetching…)" (buffer-string)))
                (should (equal (nth 4 (car spawned)) "github.com/o/r"))))))
      (delete-directory root t))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-forge-repo-is-memoized-per-url
  (ygg-git-review-requests-tests--with
    (let ((calls 0) (url "a"))
      (cl-letf (((symbol-function 'ygg-git-compare--forge-repo)
                 (lambda (&optional _) (cl-incf calls) ygg-git-review-requests-tests--repo))
                ((symbol-function 'magit-get) (lambda (&rest _) url)))
        (ygg-git-review-requests--repo)
        (ygg-git-review-requests--repo)
        (should (= 1 calls))
        (setq url "b")
        (ygg-git-review-requests--repo)
        (should (= 2 calls))))))

(defun ygg-git-review-requests-tests--disk (stamp)
  (let ((file (make-temp-file "ygg-rr" nil ".eld")))
    (with-temp-file file
      (prin1 (list (cons ygg-git-review-requests-tests--repo
                         (cons stamp (list :rows (list (ygg-git-review-requests-tests--row))))))
             (current-buffer)))
    file))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-disk-cache-shows-first-with-no-process
  (ygg-git-review-requests-tests--with
    (let ((ygg-git-review-requests-file
           (ygg-git-review-requests-tests--disk (float-time)))
          (ygg-git-review-requests--loaded nil))
      (unwind-protect
          (with-temp-buffer
            (ygg-git-review-requests-tests--render)
            (should-not spawned)
            (should (string-match-p "Review requests (1)" (buffer-string)))
            (should (string-match-p "#12  Add thing" (buffer-string))))
        (delete-file ygg-git-review-requests-file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-stale-disk-entry-refetches-once
  (ygg-git-review-requests-tests--with
    (let ((ygg-git-review-requests-file
           (ygg-git-review-requests-tests--disk (- (float-time) 1000)))
          (ygg-git-review-requests--loaded nil))
      (unwind-protect
          (with-temp-buffer
            (ygg-git-review-requests-tests--render)
            (ygg-git-review-requests-tests--render)
            (should (= 1 (length spawned)))
            (should (string-match-p "Review requests (1)" (buffer-string)))
            (funcall (cdr (car spawned)) 0 "[]")
            (should (string-match-p "Review requests (0)" (buffer-string)))
            (should (= 1 (length spawned)))
            (let ((saved (with-temp-buffer
                           (insert-file-contents ygg-git-review-requests-file)
                           (read (current-buffer)))))
              (should (equal (plist-get (cddr (car saved)) :rows) nil))
              (should (equal (car (car saved)) ygg-git-review-requests-tests--repo))))
        (delete-file ygg-git-review-requests-file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-keys-act-on-the-row
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--seed (list (ygg-git-review-requests-tests--row)))
      (ygg-git-review-requests-tests--render)
      (goto-char (point-min))
      (search-forward "#12")
      (should (eq (lookup-key magit-review-request-section-map (kbd "RET"))
                  #'ygg-git-review-requests-open))
      (let (target browsed)
        (cl-letf (((symbol-function 'ygg-git-compare-review-branch)
                   (lambda (&optional t0) (setq target t0)))
                  ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
          (ygg-git-review-requests-open)
          (should (equal target '(pr :number 12 :head "feat")))
          (ygg-git-review-requests-browse)
          (should (equal browsed "https://example.invalid/pull/12"))
          (ygg-git-review-requests-copy-url)
          (should (equal (car kill-ring) "https://example.invalid/pull/12")))))))

(defun ygg-git-review-requests-tests--expire-timers ()
  (cl-count-if (lambda (timer) (eq (timer--function timer) 'ygg-git-review-requests--expire))
               timer-list))

(defun ygg-git-review-requests-tests--ours ()
  (cl-remove-if-not (lambda (p) (string-prefix-p "ygg-review-requests" (process-name p)))
                    (process-list)))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-real-spawn-returns-the-process
  (let ((process (ygg-git-review-requests--spawn '("sleep" "5") #'ignore)))
    (unwind-protect
        (progn (should (processp process))
               (should (process-live-p process)))
      (ygg-git-review-requests--kill process))
    (should-not (process-live-p process))
    (should-not (ygg-git-review-requests-tests--ours))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-real-timeout-kills-the-process
  (let* ((script (expand-file-name "gh" ygg-git-review-requests-tests--fake-dir))
         (real (symbol-function 'ygg-git-review-requests--spawn))
         (ygg-git-review-requests-timeout 1)
         process)
    (with-temp-file script (insert "#!/bin/sh\nexec sleep 30\n"))
    (should (equal (executable-find "gh") script))
    (ygg-git-review-requests-tests--with
      (cl-letf (((symbol-function 'ygg-git-review-requests--spawn)
                 (lambda (&rest args) (setq process (apply real args)))))
        (with-temp-buffer
          (ygg-git-review-requests-tests--render)
          (should (process-live-p process))
          (with-timeout (5 (ert-fail "no timeout"))
            (while (gethash ygg-git-review-requests-tests--repo
                            ygg-git-review-requests--running)
              (accept-process-output nil 0.05)))
          (should-not (process-live-p process))
          (should-not (memq process (process-list)))
          (should-not (ygg-git-review-requests-tests--ours))
          (should (string-match-p "reviews: gh timed out" (buffer-string))))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-replaced-fetch-late-answer-is-ignored
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--render)
      (let ((old (cdr (car spawned)))
            (cell (gethash ygg-git-review-requests-tests--repo
                           ygg-git-review-requests--running)))
        (setf (car cell) (- (float-time) 1000))
        (ygg-git-review-requests-refetch)
        (let ((fresh (gethash ygg-git-review-requests-tests--repo
                              ygg-git-review-requests--running)))
          (should (= 2 (length spawned)))
          (should-not (eq fresh cell))
          (funcall old 0 ygg-git-review-requests-tests--github)
          (should-not (gethash ygg-git-review-requests-tests--repo
                               ygg-git-review-requests--cache))
          (should (eq fresh (gethash ygg-git-review-requests-tests--repo
                                     ygg-git-review-requests--running)))
          (funcall (cdr (car spawned)) 0 "[]")
          (should (equal (cdr (gethash ygg-git-review-requests-tests--repo
                                       ygg-git-review-requests--cache))
                         '(:rows nil))))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-completion-cancels-the-timer
  (ygg-git-review-requests-tests--with
    (let ((before (ygg-git-review-requests-tests--expire-timers)))
      (with-temp-buffer
        (ygg-git-review-requests-tests--render)
        (should (= (1+ before) (ygg-git-review-requests-tests--expire-timers)))
        (funcall (cdr (car spawned)) 0 "[]")
        (should (= before (ygg-git-review-requests-tests--expire-timers)))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-malformed-disk-cache-is-dropped
  (dolist (text '("foo" "5" "\"abc\"" "((key . 5))" "((key \"x\" :rows nil))"
                  "((key 1.0 . 3))" "((key 1.0 :rows 7))" "(" "(5)"
                  "(((github \"h\" \"p\") 1.0 :rows (5)))"))
    (ygg-git-review-requests-tests--with
      (let ((file (make-temp-file "ygg-rr" nil ".eld"))
            (ygg-git-review-requests--loaded nil))
        (unwind-protect
            (let ((ygg-git-review-requests-file file))
              (with-temp-file file (insert text))
              (with-temp-buffer
                (ygg-git-review-requests-tests--render)
                (should (string-match-p "Review requests (fetching…)" (buffer-string)))
                (should (= 1 (length spawned)))))
          (delete-file file))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-good-entries-survive-bad-neighbours
  (ygg-git-review-requests-tests--with
    (let ((file (ygg-git-review-requests-tests--disk (float-time)))
          (ygg-git-review-requests--loaded nil))
      (unwind-protect
          (let ((ygg-git-review-requests-file file))
            (with-temp-file file
              (prin1 (list 'foo
                           (cons ygg-git-review-requests-tests--repo
                                 (cons (float-time) (list :rows nil))))
                     (current-buffer)))
            (with-temp-buffer
              (ygg-git-review-requests-tests--render)
              (should-not spawned)
              (should (string-match-p "Review requests (0)" (buffer-string)))))
        (delete-file file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-save-merges-prunes-and-caps
  (ygg-git-review-requests-tests--with
    (let ((file (make-temp-file "ygg-rr" nil ".eld"))
          (now (float-time)))
      (unwind-protect
          (let ((ygg-git-review-requests-file file))
            (with-temp-file file
              (prin1 (append
                      (list (cons '(github "h" "other") (cons now (list :rows nil)))
                            (cons '(github "h" "old") (cons (- now (* 31 86400)) (list :rows nil)))
                            (cons ygg-git-review-requests-tests--repo
                                  (cons (- now 50) (list :rows nil))))
                      (cl-loop for i below 250
                               collect (cons (list 'github "h" (format "bulk%d" i))
                                             (cons (- now 100 i) (list :rows nil)))))
                     (current-buffer)))
            (puthash ygg-git-review-requests-tests--repo
                     (cons now (list :rows (list (ygg-git-review-requests-tests--row))))
                     ygg-git-review-requests--cache)
            (ygg-git-review-requests--save)
            (let ((saved (with-temp-buffer
                           (insert-file-contents file)
                           (read (current-buffer)))))
              (should (= 200 (length saved)))
              (should (assoc '(github "h" "other") saved))
              (should-not (assoc '(github "h" "old") saved))
              (should (plist-get (cddr (assoc ygg-git-review-requests-tests--repo saved)) :rows)))
            (should (equal (directory-files (file-name-directory file) nil
                                            (concat "\\`" (regexp-quote (file-name-nondirectory file))
                                                    ".+"))
                           nil)))
        (delete-file file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-failed-fetch-keeps-the-last-list
  (ygg-git-review-requests-tests--with
    (let ((file (ygg-git-review-requests-tests--disk (- (float-time) 10)))
          (ygg-git-review-requests--loaded nil))
      (unwind-protect
          (let ((ygg-git-review-requests-file file))
            (with-temp-buffer
              (ygg-git-review-requests-refetch)
              (funcall (cdr (car spawned)) 1 "" "HTTP 401")
              (should (string-match-p "Review requests (1)" (buffer-string)))
              (should (string-match-p "#12  Add thing" (buffer-string)))
              (should (string-match-p "reviews: gh not authenticated (showing last list)"
                                      (buffer-string)))
              (should (string-match-p ":rows ((:number 12"
                                      (with-temp-buffer (insert-file-contents file) (buffer-string))))
              (should-not (string-match-p ":error"
                                          (with-temp-buffer (insert-file-contents file) (buffer-string))))))
        (delete-file file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-nil-forge-repo-expires-after-the-ttl
  (ygg-git-review-requests-tests--with
    (let ((calls 0) (ygg-git-review-requests-ttl 300))
      (cl-letf (((symbol-function 'ygg-git-compare--forge-repo)
                 (lambda (&optional _) (cl-incf calls) nil)))
        (ygg-git-review-requests--repo)
        (ygg-git-review-requests--repo)
        (should (= 1 calls))
        (let ((hit (gethash "/top/" ygg-git-review-requests--repos)))
          (setf (nth 1 hit) (- (float-time) 301)))
        (ygg-git-review-requests--repo)
        (should (= 2 calls))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-refetch-names-a-non-forge-remote
  (ygg-git-review-requests-tests--with
    (cl-letf (((symbol-function 'ygg-git-compare--forge-repo) (lambda (&optional _) nil))
              ((symbol-function 'magit-get) (lambda (&rest _) "git@example.org:a/b.git")))
      (with-temp-buffer
        (should (equal (cadr (should-error (ygg-git-review-requests-refetch) :type 'user-error))
                       "Not a forge remote: origin (git@example.org:a/b.git)"))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-bad-row-values-are-dropped-on-read
  (dolist (bad '((:number "7") (:number nil) (:title 5) (:author 5) (:head 5) (:base 5)
                 (:updated "x") (:draft "yes") (:url 5)))
    (ygg-git-review-requests-tests--with
      (let ((file (make-temp-file "ygg-rr" nil ".eld"))
            (ygg-git-review-requests--loaded nil))
        (unwind-protect
            (let ((ygg-git-review-requests-file file)
                  (row (apply #'plist-put (ygg-git-review-requests-tests--row) bad)))
              (with-temp-file file
                (prin1 (list (cons ygg-git-review-requests-tests--repo
                                   (cons (float-time) (list :rows (list row)))))
                       (current-buffer)))
              (with-temp-buffer
                (ygg-git-review-requests-tests--render)
                (should (string-match-p "Review requests (fetching…)" (buffer-string)))
                (should (= 1 (length spawned)))))
          (delete-file file))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-bad-row-is-dropped-good-row-kept
  (ygg-git-review-requests-tests--with
    (let ((file (make-temp-file "ygg-rr" nil ".eld"))
          (ygg-git-review-requests--loaded nil))
      (unwind-protect
          (let ((ygg-git-review-requests-file file))
            (with-temp-file file
              (prin1 (list (cons ygg-git-review-requests-tests--repo
                                 (cons (float-time)
                                       (list :rows (list (plist-put (ygg-git-review-requests-tests--row)
                                                                    :updated "x")
                                                         (ygg-git-review-requests-tests--row))))))
                     (current-buffer)))
            (with-temp-buffer
              (ygg-git-review-requests-tests--render)
              (should-not spawned)
              (should (string-match-p "Review requests (1)" (buffer-string)))))
        (delete-file file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-odd-values-in-memory-do-not-break-render
  (dolist (bad '((:updated "x") (:updated (1)) (:title nil) (:title 5) (:author 5)
                 (:head 5) (:number "7") (:draft "yes")))
    (ygg-git-review-requests-tests--with
      (with-temp-buffer
        (ygg-git-review-requests-tests--seed
         (list (apply #'plist-put (ygg-git-review-requests-tests--row) bad)))
        (ygg-git-review-requests-tests--render)
        (should (string-match-p "Review requests (1)" (buffer-string)))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-render-error-becomes-one-dim-line
  (ygg-git-review-requests-tests--with
    (with-temp-buffer
      (ygg-git-review-requests-tests--seed (list (ygg-git-review-requests-tests--row)))
      (cl-letf (((symbol-function 'ygg-git-review-requests--row)
                 (lambda (_) (error "kaput"))))
        (ygg-git-review-requests-tests--render))
      (should (string-match-p "reviews: kaput" (buffer-string))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'ygg-git-review-requests--repo)
                 (lambda () (signal 'wrong-type-argument '(x)))))
        (ygg-git-review-requests-tests--render))
      (should (string-match-p "reviews: Wrong type argument" (buffer-string))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-failure-shows-first-stderr-line
  (let ((script (expand-file-name "gh" ygg-git-review-requests-tests--fake-dir))
        (real (symbol-function 'ygg-git-review-requests--spawn)))
    (with-temp-file script (insert "#!/bin/sh\necho boom >&2\necho second >&2\nexit 1\n"))
    (should (equal (executable-find "gh") script))
    (ygg-git-review-requests-tests--with
      (cl-letf (((symbol-function 'ygg-git-review-requests--spawn)
                 (lambda (&rest args) (apply real args))))
        (with-temp-buffer
          (ygg-git-review-requests-tests--render)
          (with-timeout (5 (ert-fail "no answer"))
            (while (gethash ygg-git-review-requests-tests--repo
                            ygg-git-review-requests--running)
              (accept-process-output nil 0.05)))
          (should (string-match-p "reviews: gh: boom$" (buffer-string)))
          (should-not (string-match-p "second\\|not authenticated" (buffer-string))))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-failure-text-is-trimmed-and-mapped
  (should (equal (ygg-git-review-requests--failure "gh" 1 (make-string 200 ?x))
                 (concat "gh: " (make-string 80 ?x))))
  (dolist (text '("run gh auth login" "HTTP 401" "you are not logged in"))
    (should (equal (ygg-git-review-requests--failure "gh" 1 text) "gh not authenticated")))
  (should (equal (ygg-git-review-requests--failure "gh" 3 "") "gh failed (exit 3)")))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-two-repos-on-one-host-ask-for-the-user-once
  (ygg-git-review-requests-tests--with
    (let ((a '(gitlab "gl.example" "o/a"))
          (b '(gitlab "gl.example" "o/b")))
      (with-temp-buffer
        (ygg-git-review-requests--ensure a)
        (ygg-git-review-requests--ensure b)
        (should (= 1 (length spawned)))
        (should (equal (car (car spawned)) '("glab" "api" "--hostname" "gl.example" "user")))
        (funcall (cdr (car spawned)) 0 "{\"username\":\"me\"}")
        (should (= 3 (length spawned)))
        (should (= 1 (cl-count '("glab" "api" "--hostname" "gl.example" "user")
                               spawned :key #'car :test #'equal)))
        (should (equal (sort (mapcar (lambda (s) (car (last (car s)))) (butlast spawned))
                             #'string<)
                       '("projects/o%2Fa/merge_requests?state=opened&per_page=100&reviewer_username=me"
                         "projects/o%2Fb/merge_requests?state=opened&per_page=100&reviewer_username=me")))
        (should-not (gethash "gl.example" ygg-git-review-requests--lookups))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-failed-user-lookup-fails-every-waiter
  (ygg-git-review-requests-tests--with
    (let ((a '(gitlab "gl.example" "o/a"))
          (b '(gitlab "gl.example" "o/b")))
      (with-temp-buffer
        (ygg-git-review-requests--ensure a)
        (ygg-git-review-requests--ensure b)
        (funcall (cdr (car spawned)) 1 "" "boom")
        (should (equal (cdr (gethash a ygg-git-review-requests--cache)) '(:error "glab: boom")))
        (should (equal (cdr (gethash b ygg-git-review-requests--cache)) '(:error "glab: boom")))))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-hung-user-lookup-times-out-and-frees-the-host
  (let ((ygg-git-review-requests-timeout 0.2))
    (ygg-git-review-requests-tests--with
      (let ((a '(gitlab "gl.example" "o/a")))
        (with-temp-buffer
          (ygg-git-review-requests--ensure a)
          (with-timeout (3 (ert-fail "no timeout"))
            (while (gethash a ygg-git-review-requests--running)
              (accept-process-output nil 0.05)))
          (should-not (gethash "gl.example" ygg-git-review-requests--lookups))
          (should (cdr (gethash a ygg-git-review-requests--cache))))))))

(defun ygg-git-review-requests-tests--saved-repos (file)
  (mapcar #'car (with-temp-buffer
                  (insert-file-contents file)
                  (read (current-buffer)))))

(defun ygg-git-review-requests-tests--cached (name)
  (puthash (list 'github "h" name) (cons (float-time) (list :rows nil))
           ygg-git-review-requests--cache))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-save-skips-when-the-lock-stays-taken
  (ygg-git-review-requests-tests--with
    (let* ((file (make-temp-file "ygg-rr" nil ".eld"))
           (ygg-git-review-requests-file file))
      (unwind-protect
          (progn
            (with-temp-file file (insert "nil"))
            (make-directory (concat file ".lock"))
            (ygg-git-review-requests-tests--cached "x")
            (ygg-git-review-requests--save)
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "nil"))
            (should (file-directory-p (concat file ".lock"))))
        (delete-directory (concat file ".lock") t)
        (delete-file file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-save-removes-a-stale-lock-and-releases-its-own
  (ygg-git-review-requests-tests--with
    (let* ((file (make-temp-file "ygg-rr" nil ".eld"))
           (lock (concat file ".lock"))
           (ygg-git-review-requests-file file))
      (unwind-protect
          (progn
            (make-directory lock)
            (set-file-times lock (time-subtract nil 60))
            (ygg-git-review-requests-tests--cached "x")
            (ygg-git-review-requests--save)
            (should (equal (ygg-git-review-requests-tests--saved-repos file) '((github "h" "x"))))
            (should-not (file-exists-p lock)))
        (ignore-errors (delete-directory lock))
        (delete-file file)))))

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-save-releases-the-lock-on-error
  (ygg-git-review-requests-tests--with
    (let* ((file (make-temp-file "ygg-rr" nil ".eld"))
           (ygg-git-review-requests-file file))
      (unwind-protect
          (progn
            (ygg-git-review-requests-tests--cached "x")
            (cl-letf (((symbol-function 'rename-file) (lambda (&rest _) (error "no"))))
              (ygg-git-review-requests--save))
            (should-not (file-exists-p (concat file ".lock"))))
        (delete-file file)))))

(defconst ygg-git-review-requests-tests--saver
  "(progn
  (provide 'magit)
  (load (nth 0 command-line-args-left) nil t)
  (let* ((file (nth 1 command-line-args-left))
         (tag (nth 2 command-line-args-left))
         (start (string-to-number (nth 3 command-line-args-left)))
         (ygg-git-review-requests-file file))
    (while (< (float-time) start))
    (dotimes (i 5)
      (puthash (list 'github \"h\" (format \"%s%d\" tag i))
               (cons (float-time) (list :rows nil))
               ygg-git-review-requests--cache)
      (ygg-git-review-requests--save))
    (setq command-line-args-left nil)))")

(ygg-git-review-requests-tests--deftest ygg-git-review-requests-two-processes-saving-lose-nothing
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (library (locate-library "ygg-git-review-requests.el"))
         (dir (make-temp-file "ygg-rr-race" t)))
    (should (equal (executable-find "gh") (expand-file-name "gh" ygg-git-review-requests-tests--fake-dir)))
    (unwind-protect
        (dotimes (run 10)
          (let* ((file (expand-file-name (format "r%d.eld" run) dir))
                 (start (+ (float-time) 0.6))
                 (processes
                  (mapcar (lambda (tag)
                            (start-process
                             (concat "ygg-race-" tag) nil emacs "-Q" "--batch"
                             "--eval" ygg-git-review-requests-tests--saver
                             library file tag (format "%f" start)))
                          '("a" "b"))))
            (with-timeout (15 (ert-fail "saver hung"))
              (while (seq-some #'process-live-p processes)
                (accept-process-output nil 0.05)))
            (should (equal (sort (mapcar #'caddr (ygg-git-review-requests-tests--saved-repos file))
                                 #'string<)
                           (sort (cl-loop for tag in '("a" "b")
                                          append (cl-loop for i below 5
                                                          collect (format "%s%d" tag i)))
                                 #'string<)))))
      (delete-directory dir t))))

(provide 'ygg-git-review-requests-tests)
;;; ygg-git-review-requests-tests.el ends here
