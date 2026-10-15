;;; ygg-git-compare-submit-tests.el --- review comments sent to a forge or an agent, or exported -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(let ((builds (expand-file-name "../elpaca/builds/"
                                (file-name-directory
                                 (or load-file-name buffer-file-name)))))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds)))
  (add-to-list 'load-path (expand-file-name "../../lisp/agent-objects" builds)))

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare-submit)
(require 'aob)
(require 'aob-acp)

(setq aob-acp-persist-file (make-temp-file "ygg-git-compare-submit-sessions-" nil ".eld"))
(setq aob-acp-show-trace nil)

(defvar ygg-git-compare-submit-tests--comments nil)
(defvar ygg-git-compare-submit-tests--dropped nil)
(defvar ygg-git-compare-submit-tests--calls nil "Each forge call, newest first, as (PROGRAM ARGS BODY).")
(defvar ygg-git-compare-submit-tests--messages nil)
(defvar ygg-git-compare-submit-tests--marked nil "Each mark of sent comments, newest first, as (IDS STORE).")
(defvar ygg-git-compare-submit-tests--prompts nil)
(defvar ygg-git-compare-submit-tests--remote nil
  "The comments the forge holds, or (:unread . WHY) when they cannot be read.")

(defconst ygg-git-compare-submit-tests--github-pr
  '(:forge github :host "github.com" :path "o/r" :number 12 :head "h" :base "b"
    :start "s" :url "https://github.com/o/r/pull/12"))

(defconst ygg-git-compare-submit-tests--gitlab-pr
  '(:forge gitlab :host "gitlab.com" :path "grp/proj" :number 7 :head "h" :base "b"
    :start "s" :url "https://gitlab.com/grp/proj/-/merge_requests/7"))

(defun ygg-git-compare-submit-tests--c (id &rest props)
  (append props (list :id id :range "R" :text id)))

(defconst ygg-git-compare-submit-tests--mixed
  (list (ygg-git-compare-submit-tests--c "summary" :level 'review :type 'issue :text "Looks off")
        (ygg-git-compare-submit-tests--c "line" :level 'line :file "a.txt" :old-path "a.txt"
                                         :new-path "a.txt" :side 'new :line 2 :type 'nit)
        (ygg-git-compare-submit-tests--c "range" :level 'range :file "a.txt" :old-path "a.txt"
                                         :new-path "a.txt" :side 'new :line 18 :start-side 'new
                                         :start-line 12)
        (ygg-git-compare-submit-tests--c "whole" :level 'file :file "b.txt" :old-path "b.txt"
                                         :new-path "b.txt")
        (ygg-git-compare-submit-tests--c "proposed" :level 'line :file "a.txt" :new-path "a.txt"
                                         :side 'new :line 3 :author "codex" :status 'pending)))

(defmacro ygg-git-compare-submit-tests--with (comments pr answer &rest body)
  "BODY in a compare holding COMMENTS for PR, gh and glab never run: each
forge call is recorded and ANSWER, a function of (PROGRAM ARGS BODY),
says what it prints.  The confirm answers `ygg-git-compare-submit-tests--yes'."
  (declare (indent 3))
  `(let ((ygg-git-compare-submit-tests--comments (copy-tree ,comments))
         (ygg-git-compare-submit--memo nil)
         (ygg-git-compare-submit-tests--dropped nil)
         (ygg-git-compare-submit-tests--marked nil)
         (ygg-git-compare-submit-tests--calls nil)
         (ygg-git-compare-submit-tests--messages nil)
         (ygg-git-compare-submit-tests--prompts nil)
         (ygg-git-compare-submit-tests--remote nil)
         (call-process-before (symbol-function 'call-process))
         (make-process-before (symbol-function 'make-process)))
     (cl-letf (((symbol-function 'call-process)
                (lambda (program &rest args)
                  (when (member program '("gh" "glab"))
                    (error "%s reached the network" program))
                  (apply call-process-before program args)))
               ((symbol-function 'make-process)
                (lambda (&rest args)
                  (when (member (car (plist-get args :command)) '("gh" "glab"))
                    (error "%s reached the network" (car (plist-get args :command))))
                  (apply make-process-before args)))
               ((symbol-function 'ygg-git-compare-comments-list)
                (lambda (&optional include-pending)
                  (seq-filter (lambda (c) (or include-pending
                                              (not (eq (plist-get c :status) 'pending))))
                              ygg-git-compare-submit-tests--comments)))
               ((symbol-function 'ygg-git-compare-comments-drop)
                (lambda (ids)
                  (setq ygg-git-compare-submit-tests--dropped
                        (append ygg-git-compare-submit-tests--dropped ids))
                  (setq ygg-git-compare-submit-tests--comments
                        (seq-remove (lambda (c) (member (plist-get c :id) ids))
                                    ygg-git-compare-submit-tests--comments))))
               ((symbol-function 'ygg-git-compare-comments-mark-sent)
                (lambda (ids &optional store)
                  (push (list ids store) ygg-git-compare-submit-tests--marked)
                  (setq ygg-git-compare-submit-tests--comments
                        (mapcar (lambda (c)
                                  (if (member (plist-get c :id) ids)
                                      (plist-put (copy-sequence c) :sent t)
                                    c))
                                ygg-git-compare-submit-tests--comments))))
               ((symbol-function 'ygg-git-compare--remote-fresh)
                (lambda (_list _pr then)
                  (let ((remote ygg-git-compare-submit-tests--remote))
                    (if (eq (car-safe remote) :unread)
                        (funcall then nil (cdr remote))
                      (funcall then remote nil)))))
               ((symbol-function 'ygg-git-compare--list) #'current-buffer)
               ((symbol-function 'ygg-git-compare--range-label) (lambda () "R"))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (push prompt ygg-git-compare-submit-tests--prompts)
                  (bound-and-true-p ygg-git-compare-submit-tests--yes)))
               ((symbol-function 'message)
                (lambda (format &rest args)
                  (when format
                    (push (apply #'format-message format args)
                          ygg-git-compare-submit-tests--messages))))
               ((symbol-function 'ygg-git-compare--forge-async)
                (lambda (program args callback &optional _timeout)
                  (let* ((input (cadr (member "--input" args)))
                         (body (and input (json-parse-string
                                           (with-temp-buffer
                                             (insert-file-contents input)
                                             (buffer-string))
                                           :object-type 'plist :array-type 'list))))
                    (push (list program args body) ygg-git-compare-submit-tests--calls)
                    (apply callback
                           (condition-case failure
                               (list 0 (funcall ,answer program args body) "")
                             (error (list 1 "" (error-message-string failure))))))))
               ((symbol-function 'ygg-git-compare--this-pr)
                (lambda (&optional continue)
                  (if continue (funcall continue ,pr) ,pr))))
       (with-temp-buffer ,@body))))

(defvar ygg-git-compare-submit-tests--yes t)

(defun ygg-git-compare-submit-tests--ids ()
  (mapcar (lambda (c) (plist-get c :id)) ygg-git-compare-submit-tests--comments))

(defun ygg-git-compare-submit-tests--posts ()
  "The forge calls that changed something, oldest first."
  (reverse (seq-filter (lambda (c) (or (member "--input" (nth 1 c))
                                       (member "--method" (nth 1 c))
                                       (member "graphql" (nth 1 c))))
                       ygg-git-compare-submit-tests--calls)))

(defun ygg-git-compare-submit-tests--endpoint (call)
  (seq-find (lambda (a) (or (string-prefix-p "projects/" a) (string-prefix-p "repos/" a)
                            (equal a "graphql")))
            (nth 1 call)))

;;; Markdown

(ert-deftest ygg-git-compare-submit-markdown-every-level ()
  (should (equal
           (ygg-git-compare-markdown
            (list (ygg-git-compare-submit-tests--c "l" :level 'line :file "src/a.rs" :side 'new
                                                   :line 42 :text "Magic number")
                  (ygg-git-compare-submit-tests--c "r" :level 'range :file "src/a.rs" :side 'new
                                                   :line 55 :start-side 'new :start-line 50
                                                   :type 'issue :text "Refactor\nthis block")
                  (ygg-git-compare-submit-tests--c "f" :level 'file :file "src/a.rs"
                                                   :text "Add tests" :author "codex")
                  (ygg-git-compare-submit-tests--c "o" :level 'line :file "src/a.rs" :side 'old
                                                   :line 7 :text "Gone")
                  (ygg-git-compare-submit-tests--c "s" :level 'review :type 'question
                                                   :text "Why now?")
                  (ygg-git-compare-submit-tests--c "p" :level 'line :file "src/a.rs" :side 'new
                                                   :line 1 :status 'pending :text "unchecked")))
           (concat "I reviewed your code and have the following comments. Please address them.\n\n"
                   "Reviewing R\n\n"
                   "1. **[QUESTION]** `Review comment` - Why now?\n"
                   "2. `src/a.rs` - Add tests — by codex\n"
                   "3. `src/a.rs:~7` - Gone\n"
                   "4. `src/a.rs:42` - Magic number\n"
                   "5. **[ISSUE]** `src/a.rs:50-55` - Refactor\n"
                   "   this block\n"))))

(ert-deftest ygg-git-compare-submit-markdown-quotes-when-asked ()
  (let ((c (list (ygg-git-compare-submit-tests--c "l" :level 'line :file "a" :side 'old :line 3
                                                  :old-path "a" :start-line nil :start-side nil
                                                  :quote " l2\n-l3" :range nil :text "x")))
        (ygg-git-compare-export-intro ""))
    (should (equal (ygg-git-compare-markdown c) "1. `a:~3` - x\n"))
    (let ((ygg-git-compare-export-quote t))
      (should (equal (ygg-git-compare-markdown c)
                     "1. `a:~3` - x\n   ```diff\n    l2\n   -l3\n   ```\n")))))

(ert-deftest ygg-git-compare-submit-markdown-findings-and-verdict ()
  (let ((ygg-git-compare-export-intro ""))
    (should (equal
             (ygg-git-compare-markdown
              (list (ygg-git-compare-submit-tests--c "v" :level 'review :range nil :text ""
                                                     :correctness "patch is incorrect"
                                                     :confidence 0.8 :author "codex")
                    (ygg-git-compare-submit-tests--c "f" :level 'line :range nil :file "a"
                                                     :side 'new :line 4 :priority 1
                                                     :title "Off by one" :text "Loop ends early"
                                                     :author "codex")))
             (concat "Verdict: patch is incorrect (confidence 0.8)\n\n"
                     "1. **[P1] Off by one** `a:4` - Loop ends early — by codex\n")))))

(ert-deftest ygg-git-compare-submit-max-priority-keeps-urgent-and-own ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "p0" :priority 0 :author "codex")
            (ygg-git-compare-submit-tests--c "p2" :priority 2 :author "codex")
            (ygg-git-compare-submit-tests--c "agent-unranked" :author "codex")
            (ygg-git-compare-submit-tests--c "mine")
            (ygg-git-compare-submit-tests--c "pending" :priority 0 :status 'pending))
      nil #'ignore
    (should (equal (mapcar (lambda (c) (plist-get c :id)) (ygg-git-compare-submit--select nil 1))
                   '("p0" "mine")))
    (should (equal (length (ygg-git-compare-submit--select nil)) 4))
    (should-not (ygg-git-compare-submit--select 'agent))))

(ert-deftest ygg-git-compare-submit-markdown-for-range-reads-the-drafts-file ()
  (let ((gitdir (make-temp-file "ygg-submit-gitdir-" t))
        (ygg-git-compare-export-intro ""))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "ygg-review-comments.eld" gitdir)
            (prin1 `(("k1" ,(ygg-git-compare-submit-tests--c "x" :level 'file :file "a" :range nil))
                     ("k2" ,(ygg-git-compare-submit-tests--c "y" :level 'file :file "b" :range nil)))
                   (current-buffer)))
          (cl-letf (((symbol-function 'magit-gitdir) (lambda (&rest _) gitdir)))
            (should (equal (ygg-git-compare-markdown-for-range gitdir "k2") "1. `b` - y\n"))
            (should (equal (ygg-git-compare-markdown-for-range gitdir "none") ""))))
      (delete-directory gitdir t))))

(ert-deftest ygg-git-compare-submit-export-copies-what-the-filter-keeps ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed nil #'ignore
    (let ((kill-ring nil)
          (interprogram-cut-function nil))
      (cl-letf (((symbol-function 'ygg-git-compare-submit--args) (lambda () '(nil))))
        (ygg-git-compare-export-markdown))
      (should (string-search "`a.txt:12-18`" (car kill-ring)))
      (should-not (string-search "proposed" (car kill-ring)))
      (should (equal (car ygg-git-compare-submit-tests--messages)
                     "4 review comments copied as markdown"))
      (should-not ygg-git-compare-submit-tests--calls))))

;;; GitHub

(ert-deftest ygg-git-compare-submit-github-comment-one-review ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (should (string-search "1 pending agent comment not included"
                           (car ygg-git-compare-submit-tests--prompts)))
    (should (equal (ygg-git-compare-submit-tests--ids) '("proposed")))
    (should (string-search "pull/12" (car ygg-git-compare-submit-tests--messages)))
    (let ((posts (ygg-git-compare-submit-tests--posts)))
      (should (= (length posts) 1))
      (pcase-let ((`(,program ,args ,body) (car posts)))
        (should (equal program "gh"))
        (should (member "repos/o/r/pulls/12/reviews" args))
        (should (equal (plist-get body :event) "COMMENT"))
        (should (equal (plist-get body :commit_id) "h"))
        (should (equal (plist-get body :body) "**issue:** Looks off\n\n`b.txt` - whole"))
        (should (equal (plist-get body :comments)
                       '((:path "a.txt" :line 2 :side "RIGHT" :body "**nit:** line")
                         (:path "a.txt" :line 18 :side "RIGHT" :body "range"
                          :start_line 12 :start_side "RIGHT"))))))))

(defconst ygg-git-compare-submit-tests--hunk-comment
  (ygg-git-compare-submit-tests--c "hunk" :level 'range :file "a.txt" :old-path "a.txt"
                                   :new-path "a.txt" :start-side 'old :start-line 11
                                   :side 'new :line 14)
  "A comment on the whole hunk of the diff below, from its removed line to its last added.")

(ert-deftest ygg-git-compare-submit-github-hunk-comment-spans-both-sides ()
  (ygg-git-compare-submit-tests--with (list ygg-git-compare-submit-tests--hunk-comment)
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts))) :comments)
                   '((:path "a.txt" :line 14 :side "RIGHT" :body "hunk"
                      :start_line 11 :start_side "LEFT"))))))

(ert-deftest ygg-git-compare-submit-gitlab-hunk-comment-has-a-line-range ()
  (cl-letf (((symbol-function 'magit-git-lines)
             (lambda (&rest _) ygg-git-compare-submit-tests--diff)))
    (let ((position (ygg-git-compare-submit--gitlab-position
                     ygg-git-compare-submit-tests--hunk-comment
                     ygg-git-compare-submit-tests--gitlab-pr))
          (hash (sha1 "a.txt")))
      (should (equal (plist-get position :line_range)
                     `(:start (:line_code ,(concat hash "_11_0") :type "old")
                       :end (:line_code ,(concat hash "_0_14") :type "new")))))))

(ert-deftest ygg-git-compare-submit-github-events ()
  (dolist (case '((draft nil) (approve "APPROVE") (request-changes "REQUEST_CHANGES")))
    (ygg-git-compare-submit-tests--with
        (list (ygg-git-compare-submit-tests--c "l" :level 'line :file "a" :new-path "a"
                                               :side 'old :line 3))
        ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
      (ygg-git-compare-submit-forge (car case))
      (let ((body (nth 2 (car (ygg-git-compare-submit-tests--posts)))))
        (if (cadr case)
            (should (equal (plist-get body :event) (cadr case)))
          (should-not (plist-member body :event)))
        (should (eq (not (plist-member body :body)) (not (eq (car case) 'request-changes))))
        (should-not (plist-member (car (plist-get body :comments)) :start_line))
        (should (equal (plist-get (car (plist-get body :comments)) :side) "LEFT")))))
  (ygg-git-compare-submit-tests--with nil ygg-git-compare-submit-tests--github-pr
      (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'approve)
    (should (equal (nth 2 (car (ygg-git-compare-submit-tests--posts)))
                   '(:commit_id "h" :event "APPROVE" :comments nil)))
    (should-error (ygg-git-compare-submit-forge 'comment) :type 'user-error)))

(ert-deftest ygg-git-compare-submit-github-failure-keeps-every-comment ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--github-pr
      (lambda (&rest _) (user-error "gh api: one pending review per pull request"))
    (ygg-git-compare-submit-forge 'draft)
    (should (= (length ygg-git-compare-submit-tests--comments) 5))
    (should-not ygg-git-compare-submit-tests--dropped)
    (should (string-search "Posted 0 of 4; 4 kept: gh api: one pending review per pull request"
                           (car ygg-git-compare-submit-tests--messages)))))

(defun ygg-git-compare-submit-tests--author (author &optional me)
  "An answer for a pull request by AUTHOR on a forge that knows you as ME."
  (lambda (_program args _body)
    (cond ((member "repos/o/r/pulls/12" args)
           (format "{\"user\":{\"login\":\"%s\"}}" author))
          ((member "user" args) (format "{\"login\":\"%s\"}" (or me "me")))
          (t "{}"))))

(ert-deftest ygg-git-compare-submit-own-pr-only-comments ()
  (let ((comments (list (ygg-git-compare-submit-tests--c "l" :level 'review))))
    (dolist (event '(approve request-changes))
      (ygg-git-compare-submit-tests--with comments ygg-git-compare-submit-tests--github-pr
          (ygg-git-compare-submit-tests--author "me")
        (ygg-git-compare-submit-forge event)
        (should (string-search "refuses" (car ygg-git-compare-submit-tests--prompts)))
        (should (string-search "Comment with 1 comment" (car ygg-git-compare-submit-tests--prompts)))
        (should-not (string-search "Approve" (car ygg-git-compare-submit-tests--prompts)))
        (should (equal (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts))) :event)
                       "COMMENT"))))))

(ert-deftest ygg-git-compare-submit-own-pr-without-comments-says-why ()
  (ygg-git-compare-submit-tests--with nil ygg-git-compare-submit-tests--github-pr
      (ygg-git-compare-submit-tests--author "me")
    (let ((failure (should-error (ygg-git-compare-submit-forge 'approve) :type 'user-error)))
      (should (string-search "your own pull request" (cadr failure))))
    (should-not (ygg-git-compare-submit-tests--posts))))

(ert-deftest ygg-git-compare-submit-other-pr-still-approves ()
  (ygg-git-compare-submit-tests--with nil ygg-git-compare-submit-tests--github-pr
      (ygg-git-compare-submit-tests--author "someone")
    (ygg-git-compare-submit-forge 'approve)
    (should (string-prefix-p "Approve with 0 comments" (car ygg-git-compare-submit-tests--prompts)))
    (should (equal (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts))) :event)
                   "APPROVE"))))

(ert-deftest ygg-git-compare-submit-github-refusal-carries-githubs-text ()
  (should (equal (ygg-git-compare-submit--github-refusal
                  "{\"message\":\"Unprocessable Entity\",\"errors\":[\"Can not approve your own pull request\"],\"status\":\"422\"}"
                  "gh: Unprocessable Entity (HTTP 422)")
                 "gh: 422 Can not approve your own pull request"))
  (should (equal (ygg-git-compare-submit--github-refusal
                  "{\"message\":\"Not Found\"}" "gh: Not Found (HTTP 404)")
                 "gh: 404 Not Found"))
  (should-not (ygg-git-compare-submit--github-refusal "" "gh: boom")))

(ert-deftest ygg-git-compare-submit-failed-approval-says-so ()
  (ygg-git-compare-submit-tests--with nil ygg-git-compare-submit-tests--github-pr
      (lambda (_program args _body)
        (if (member "--input" args)
            (error "gh: 422 Can not approve your own pull request")
          "{}"))
    (ygg-git-compare-submit-forge 'approve)
    (should (string-search "Approve failed on github PR #12: " (car ygg-git-compare-submit-tests--messages)))
    (should-not (string-search "Posted 0" (car ygg-git-compare-submit-tests--messages)))))

(ert-deftest ygg-git-compare-submit-confirm-no-sends-nothing ()
  (let ((ygg-git-compare-submit-tests--yes nil))
    (dolist (pr (list ygg-git-compare-submit-tests--github-pr
                      ygg-git-compare-submit-tests--gitlab-pr))
      (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed pr
          (lambda (&rest _) "{}")
        (dolist (event '(comment approve request-changes draft))
          (should-error (ygg-git-compare-submit-forge event) :type 'user-error))
        (should-not (ygg-git-compare-submit-tests--posts))
        (should-not ygg-git-compare-submit-tests--dropped)))))

(defconst ygg-git-compare-submit-tests--old-sha (make-string 40 ?a))

(defun ygg-git-compare-submit-tests--moved (id &rest props)
  "A comment made when the pull request ended at the old sha."
  (apply #'ygg-git-compare-submit-tests--c id
         :range (concat "A: " (make-string 40 ?b) " ↔ B: #7 x  " (make-string 40 ?b)
                        "..." ygg-git-compare-submit-tests--old-sha)
         props))

(defconst ygg-git-compare-submit-tests--moved-line
  (ygg-git-compare-submit-tests--moved "line" :level 'line :file "a.txt" :old-path "a.txt"
                                       :new-path "a.txt" :side 'new :line 2))

(ert-deftest ygg-git-compare-submit-sends-a-comment-made-on-another-range ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--moved "old" :level 'review))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (should (equal (ygg-git-compare-submit--written-head
                    (car ygg-git-compare-submit-tests--comments))
                   ygg-git-compare-submit-tests--old-sha))
    (ygg-git-compare-submit-forge 'comment)
    (should-not ygg-git-compare-submit-tests--comments)
    (should (equal (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts))) :body)
                   "(written on aaaaaaa) old"))))

(ert-deftest ygg-git-compare-submit-same-head-under-another-label-is-current ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      (append (list :head ygg-git-compare-submit-tests--old-sha)
              ygg-git-compare-submit-tests--github-pr)
      (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (let ((posts (ygg-git-compare-submit-tests--posts)))
      (should (= (length posts) 1))
      (should (equal (plist-get (nth 2 (car posts)) :comments)
                     '((:path "a.txt" :line 2 :side "RIGHT" :body "line")))))))

(ert-deftest ygg-git-compare-submit-github-moved-comment-lands-on-its-own-commit ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line
            (ygg-git-compare-submit-tests--c "now" :level 'line :file "a.txt"
                                             :new-path "a.txt" :side 'new :line 5)
            (ygg-git-compare-submit-tests--moved "span" :level 'range :file "a.txt"
                                                 :new-path "a.txt" :side 'new :line 9
                                                 :start-side 'old :start-line 7))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (should-not ygg-git-compare-submit-tests--comments)
    (should (equal ygg-git-compare-submit-tests--dropped '("line" "now" "span")))
    (let ((posts (ygg-git-compare-submit-tests--posts))
          (sha ygg-git-compare-submit-tests--old-sha))
      (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint posts)
                     '("repos/o/r/pulls/12/reviews" "repos/o/r/pulls/12/comments"
                       "repos/o/r/pulls/12/comments")))
      (should (equal (plist-get (nth 2 (nth 0 posts)) :comments)
                     '((:path "a.txt" :line 5 :side "RIGHT" :body "now"))))
      (should (equal (plist-get (nth 2 (nth 0 posts)) :commit_id) "h"))
      (should (equal (nth 2 (nth 1 posts))
                     `(:path "a.txt" :line 2 :side "RIGHT" :body "line" :commit_id ,sha)))
      (should (equal (nth 2 (nth 2 posts))
                     `(:path "a.txt" :line 9 :side "RIGHT" :body "span" :start_line 7
                       :start_side "LEFT" :commit_id ,sha))))))

(ert-deftest ygg-git-compare-submit-github-draft-says-where-moved-comments-were-written ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line
            (ygg-git-compare-submit-tests--c "now" :level 'line :file "a.txt"
                                             :new-path "a.txt" :side 'new :line 5))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'draft)
    (let ((posts (ygg-git-compare-submit-tests--posts)))
      (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint posts)
                     '("repos/o/r/pulls/12/reviews")))
      (should-not (plist-member (nth 2 (car posts)) :event))
      (should (equal (plist-get (nth 2 (car posts)) :commit_id) "h"))
      (should (equal (plist-get (nth 2 (car posts)) :body)
                     "(written on aaaaaaa) `a.txt:2` line"))
      (should (equal (plist-get (nth 2 (car posts)) :comments)
                     '((:path "a.txt" :line 5 :side "RIGHT" :body "now")))))))

(ert-deftest ygg-git-compare-submit-moved-note-names-lines-and-side ()
  (let ((sha ygg-git-compare-submit-tests--old-sha))
    (should (equal (ygg-git-compare-submit--moved-note
                    (ygg-git-compare-submit-tests--c "r" :level 'range :file "a.txt" :side 'new
                                                     :line 18 :start-line 12)
                    sha)
                   "(written on aaaaaaa) `a.txt:12–18` r"))
    (should (equal (ygg-git-compare-submit--moved-note
                    (ygg-git-compare-submit-tests--c "o" :level 'line :file "a.txt" :side 'old
                                                     :line 3)
                    sha)
                   "(written on aaaaaaa) `a.txt:3` (removed line) o"))))

(ert-deftest ygg-git-compare-submit-github-moved-comment-refused-becomes-a-note ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      ygg-git-compare-submit-tests--github-pr
      (lambda (_program args _body)
        (if (member "repos/o/r/pulls/12/comments" args) (user-error "gh: 422 no") "{}"))
    (ygg-git-compare-submit-forge 'comment)
    (should-not ygg-git-compare-submit-tests--comments)
    (let ((posts (ygg-git-compare-submit-tests--posts)))
      (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint posts)
                     '("repos/o/r/pulls/12/comments" "repos/o/r/issues/12/comments")))
      (should (equal (nth 2 (nth 1 posts))
                     '(:body "(written on aaaaaaa) `a.txt:2` line"))))))

(ert-deftest ygg-git-compare-submit-github-moved-comment-refused-twice-keeps-it ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      ygg-git-compare-submit-tests--github-pr
      (lambda (_program args _body)
        (if (seq-find (lambda (a) (string-match-p "/\\(pulls\\|issues\\)/12/comments" a)) args)
            (user-error "gh: 422 no")
          "{}"))
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (ygg-git-compare-submit-tests--ids) '("line")))))

(ert-deftest ygg-git-compare-submit-github-review-counts-only-what-it-carries ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line
            (ygg-git-compare-submit-tests--c "now" :level 'line :file "a.txt"
                                             :new-path "a.txt" :side 'new :line 5 :text "")
            (ygg-git-compare-submit-tests--moved "span" :level 'line :file "a.txt"
                                                 :new-path "a.txt" :side 'new :line 9))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts))) :body)
                   "1 comment"))))

(ert-deftest ygg-git-compare-submit-github-skips-an-empty-review ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint
                           (ygg-git-compare-submit-tests--posts))
                   '("repos/o/r/pulls/12/comments")))
    (should-not ygg-git-compare-submit-tests--comments)))

(ert-deftest ygg-git-compare-submit-github-approval-is-posted-with-nothing-else ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'approve)
    (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint
                           (ygg-git-compare-submit-tests--posts))
                   '("repos/o/r/pulls/12/reviews" "repos/o/r/pulls/12/comments")))))

(defun ygg-git-compare-submit-tests--versions (&rest heads)
  "An answer to glab that lists a diff version for each of HEADS, newest first."
  (lambda (_program args _body)
    (if (seq-find (lambda (a) (string-search "/versions" a)) args)
        (json-serialize
         (vconcat (mapcar (lambda (head)
                            `(:head_commit_sha ,head :base_commit_sha ,(concat "base-" head)
                              :start_commit_sha ,(concat "start-" head)))
                          heads)))
      "{\"id\":1}")))

(ert-deftest ygg-git-compare-submit-gitlab-moved-comment-uses-its-version ()
  (let ((sha ygg-git-compare-submit-tests--old-sha))
    (ygg-git-compare-submit-tests--with
        (list ygg-git-compare-submit-tests--moved-line)
        ygg-git-compare-submit-tests--gitlab-pr
        (ygg-git-compare-submit-tests--versions "h" sha)
      (ygg-git-compare-submit-forge 'comment)
      (should-not ygg-git-compare-submit-tests--comments)
      (should (= (length (seq-filter (lambda (c) (string-search "/versions" (format "%S" c)))
                                     ygg-git-compare-submit-tests--calls))
                 1))
      (let ((post (nth 2 (car (ygg-git-compare-submit-tests--posts)))))
        (should (equal (plist-get post :body) "line"))
        (should (equal (plist-get (plist-get post :position) :base_sha) (concat "base-" sha)))
        (should (equal (plist-get (plist-get post :position) :start_sha) (concat "start-" sha)))
        (should (equal (plist-get (plist-get post :position) :head_sha) sha))))))

(ert-deftest ygg-git-compare-submit-gitlab-moved-comment-without-version-goes-in-the-summary ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line
            (ygg-git-compare-submit-tests--c "sum" :level 'review))
      ygg-git-compare-submit-tests--gitlab-pr
      (ygg-git-compare-submit-tests--versions "h")
    (ygg-git-compare-submit-forge 'comment)
    (should-not ygg-git-compare-submit-tests--comments)
    (let ((posts (ygg-git-compare-submit-tests--posts)))
      (should (= (length posts) 1))
      (should (equal (plist-get (nth 2 (car posts)) :body)
                     "(written on aaaaaaa) `a.txt:2` line\n\nsum"))
      (should-not (plist-member (nth 2 (car posts)) :position)))))

(ert-deftest ygg-git-compare-submit-gitlab-failed-versions-fall-back-to-the-summary ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      ygg-git-compare-submit-tests--gitlab-pr
      (lambda (_program args _body)
        (if (seq-find (lambda (a) (string-search "/versions" a)) args)
            (user-error "glab: 500")
          "{\"id\":1}"))
    (ygg-git-compare-submit-forge 'comment)
    (should-not ygg-git-compare-submit-tests--comments)
    (let ((posts (ygg-git-compare-submit-tests--posts)))
      (should (equal (plist-get (nth 2 (car posts)) :body)
                     "(written on aaaaaaa) `a.txt:2` line"))
      (should-not (plist-member (nth 2 (car posts)) :position)))))

(ert-deftest ygg-git-compare-submit-gitlab-summary-failure-keeps-text-moved-comments ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      ygg-git-compare-submit-tests--gitlab-pr
      (lambda (_program args _body)
        (if (seq-find (lambda (a) (string-search "/versions" a)) args)
            "[]"
          (user-error "glab: 400")))
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (ygg-git-compare-submit-tests--ids) '("line")))))

(ert-deftest ygg-git-compare-submit-gitlab-moved-comment-refused-becomes-a-note ()
  (let ((sha ygg-git-compare-submit-tests--old-sha))
    (ygg-git-compare-submit-tests--with
        (list ygg-git-compare-submit-tests--moved-line)
        ygg-git-compare-submit-tests--gitlab-pr
        (lambda (program args body)
          (if (plist-get body :position)
              (user-error "glab: 400 line_code")
            (funcall (ygg-git-compare-submit-tests--versions "h" sha) program args body)))
      (ygg-git-compare-submit-forge 'comment)
      (should-not ygg-git-compare-submit-tests--comments)
      (let ((posts (ygg-git-compare-submit-tests--posts)))
        (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint posts)
                       '("projects/grp%2Fproj/merge_requests/7/discussions"
                         "projects/grp%2Fproj/merge_requests/7/notes")))
        (should (equal (nth 2 (nth 1 posts))
                       '(:body "(written on aaaaaaa) `a.txt:2` line")))))))

(ert-deftest ygg-git-compare-submit-gitlab-reads-versions-only-for-moved-comments ()
  (ygg-git-compare-submit-tests--with
      (list (nth 1 ygg-git-compare-submit-tests--mixed))
      ygg-git-compare-submit-tests--gitlab-pr (ygg-git-compare-submit-tests--lab)
    (ygg-git-compare-submit-forge 'comment)
    (should (= (length ygg-git-compare-submit-tests--calls) 1))))

(ert-deftest ygg-git-compare-submit-moved-file-comment-says-where-in-the-summary ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--moved "whole" :level 'file :file "b.txt"
                                                 :old-path "b.txt" :new-path "b.txt")
            (ygg-git-compare-submit-tests--c "now" :level 'review))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts))) :body)
                   "(written on aaaaaaa) `b.txt` - whole\n\nnow"))))

(ert-deftest ygg-git-compare-submit-refuses-a-comment-made-on-a-worktree ()
  (dolist (event '(comment approve request-changes draft))
    (ygg-git-compare-submit-tests--with
        (list (ygg-git-compare-submit-tests--c "wt" :level 'review :range "A: /tmp/wt"))
        ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
      (should (equal (cadr (should-error (ygg-git-compare-submit-forge event) :type 'user-error))
                     "A comment was made on A: /tmp/wt; drop it or send it to an agent"))
      (should-not ygg-git-compare-submit-tests--prompts)
      (should-not ygg-git-compare-submit-tests--calls))))

(ert-deftest ygg-git-compare-submit-older-fetch-still-counts-its-comments-as-moved ()
  (ygg-git-compare-submit-tests--with
      (list ygg-git-compare-submit-tests--moved-line)
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (cl-letf (((symbol-function 'ygg-git-compare--range-label)
               (lambda () (plist-get (car ygg-git-compare-submit-tests--comments) :range))))
      (ygg-git-compare-submit-forge 'comment))
    (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint
                           (ygg-git-compare-submit-tests--posts))
                   '("repos/o/r/pulls/12/comments")))))

(ert-deftest ygg-git-compare-submit-never-sends-a-pending-comment ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (cl-letf (((symbol-function 'ygg-git-compare-comments-list)
               (lambda (&rest _) ygg-git-compare-submit-tests--comments)))
      (ygg-git-compare-submit-forge 'comment)
      (should (= (length (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts)))
                                    :comments))
                 2))
      (should-not (member "proposed" ygg-git-compare-submit-tests--dropped)))))

(ert-deftest ygg-git-compare-submit-github-draft-is-one-pending-review ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "a" :level 'line :file "a.txt" :new-path "a.txt"
                                             :side 'new :line 2 :text "why two")
            (ygg-git-compare-submit-tests--c "o" :level 'line :file "old.txt" :old-path "old.txt"
                                             :new-path "new.txt" :side 'old :line 3
                                             :text "keep l3 é → ü"))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'draft)
    (should-not ygg-git-compare-submit-tests--comments)
    (pcase-let ((`(,program ,args ,body) (car (ygg-git-compare-submit-tests--posts))))
      (should (equal program "gh"))
      (should (equal (cadr (member "--method" args)) "POST"))
      (should (equal body '(:commit_id "h"
                            :comments ((:path "a.txt" :line 2 :side "RIGHT" :body "why two")
                                       (:path "new.txt" :line 3 :side "LEFT"
                                        :body "keep l3 é → ü"))))))))

;;; GitLab

(ert-deftest ygg-git-compare-submit-gitlab-line-positions-by-side ()
  (let ((pr ygg-git-compare-submit-tests--gitlab-pr)
        (refs '(:position_type "text" :base_sha "b" :start_sha "s" :head_sha "h")))
    (should (equal (ygg-git-compare-submit--gitlab-position
                    (ygg-git-compare-submit-tests--c "c" :level 'line :old-path "old.txt"
                                                     :new-path "new.txt" :side 'new :line 2
                                                     :old-line 2)
                    pr)
                   (append refs '(:old_path "old.txt" :new_path "new.txt"
                                  :new_line 2 :old_line 2))))
    (should (equal (ygg-git-compare-submit--gitlab-position
                    (ygg-git-compare-submit-tests--c "o" :level 'line :old-path "old.txt"
                                                     :new-path "new.txt" :side 'old :line 3)
                    pr)
                   (append refs '(:old_path "old.txt" :new_path "new.txt" :old_line 3))))))


(defun ygg-git-compare-submit-tests--lab (&optional refuse)
  "Answer glab, failing every call REFUSE, a function of (ARGS BODY), says to."
  (lambda (_program args body)
    (if (and refuse (funcall refuse args body))
        (user-error "glab api: 400 position is invalid")
      "{\"id\":1}")))

(defvar ygg-git-compare-submit-tests--reviewer-state nil)

(defvar ygg-git-compare-submit-tests--honours nil)

(defun ygg-git-compare-submit-tests--lab-reviewer (_program args body)
  "Answer glab as user 5, a reviewer in the state held in
`ygg-git-compare-submit-tests--reviewer-state' or none when nil; a note of
\"/request_changes\" moves it to requested_changes when it is honoured."
  (let ((path (car (last args))))
      (when (and ygg-git-compare-submit-tests--honours ygg-git-compare-submit-tests--reviewer-state
                 (equal (plist-get body :body) "/request_changes"))
        (setq ygg-git-compare-submit-tests--reviewer-state "requested_changes"))
      (cond ((equal path "user") "{\"id\":5}")
            ((string-suffix-p "/reviewers" path)
             (if ygg-git-compare-submit-tests--reviewer-state
                 (format "[{\"user\":{\"id\":9},\"state\":\"unreviewed\"},{\"user\":{\"id\":5},\"state\":\"%s\"}]"
                         ygg-git-compare-submit-tests--reviewer-state)
               "[{\"user\":{\"id\":9},\"state\":\"unreviewed\"}]"))
            (t "{\"id\":1}"))))

(ert-deftest ygg-git-compare-submit-gitlab-comment-and-approve ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--gitlab-pr (ygg-git-compare-submit-tests--lab)
    (ygg-git-compare-submit-forge 'approve)
    (should (equal (ygg-git-compare-submit-tests--ids) '("proposed")))
    (let* ((posts (ygg-git-compare-submit-tests--posts))
           (mr "projects/grp%2Fproj/merge_requests/7/")
           (hash (sha1 "a.txt")))
      (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint posts)
                     (list (concat mr "notes") (concat mr "discussions")
                           (concat mr "discussions") (concat mr "approve"))))
      (should (equal (nth 2 (nth 0 posts)) '(:body "**issue:** Looks off\n\n`b.txt` - whole")))
      (should (equal (nth 2 (nth 1 posts))
                     '(:body "**nit:** line"
                       :position (:position_type "text" :base_sha "b" :start_sha "s"
                                  :head_sha "h" :old_path "a.txt" :new_path "a.txt"
                                  :new_line 2))))
      (should (equal (plist-get (plist-get (nth 2 (nth 2 posts)) :position) :line_range)
                     `(:start (:line_code ,(concat hash "_0_12") :type "new")
                       :end (:line_code ,(concat hash "_0_18") :type "new"))))
      (should (member "POST" (nth 1 (nth 3 posts)))))))

(ert-deftest ygg-git-compare-submit-gitlab-range-context-codes ()
  (let ((c (ygg-git-compare-submit-tests--c "r" :level 'range :new-path "a" :old-path "a"
                                            :side 'new :line 9 :old-line 8
                                            :start-side 'old :start-line 5)))
    (should (equal (plist-get (ygg-git-compare-submit--gitlab-position
                               c ygg-git-compare-submit-tests--gitlab-pr)
                              :line_range)
                   `(:start (:line_code ,(concat (sha1 "a") "_5_0") :type "old")
                     :end (:line_code ,(concat (sha1 "a") "_8_9")))))
    (should (equal (plist-get (plist-get (ygg-git-compare-submit--gitlab-position
                                          (append '(:start-side new :start-old-line 3) c)
                                          ygg-git-compare-submit-tests--gitlab-pr)
                                         :line_range)
                              :start)
                   `(:line_code ,(concat (sha1 "a") "_3_5"))))))

(ert-deftest ygg-git-compare-submit-gitlab-range-falls-back-to-its-last-line ()
  (ygg-git-compare-submit-tests--with
      (list (nth 2 ygg-git-compare-submit-tests--mixed))
      ygg-git-compare-submit-tests--gitlab-pr
      (ygg-git-compare-submit-tests--lab
       (lambda (_args body) (plist-get (plist-get body :position) :line_range)))
    (ygg-git-compare-submit-forge 'draft)
    (should-not ygg-git-compare-submit-tests--comments)
    (let ((retry (nth 2 (car (last (ygg-git-compare-submit-tests--posts))))))
      (should (equal (plist-get retry :note) "L12–18: range"))
      (should-not (plist-member (plist-get retry :position) :line_range))
      (should (string-suffix-p "/draft_notes"
                               (ygg-git-compare-submit-tests--endpoint
                                (car ygg-git-compare-submit-tests--calls)))))))

(ert-deftest ygg-git-compare-submit-gitlab-keeps-what-failed-and-does-not-approve ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--gitlab-pr
      (ygg-git-compare-submit-tests--lab (lambda (_args body)
                                           (equal (plist-get body :body) "**nit:** line")))
    (ygg-git-compare-submit-forge 'approve)
    (should (equal (ygg-git-compare-submit-tests--ids) '("line" "proposed")))
    (should-not (seq-find (lambda (c) (string-suffix-p "/approve"
                                                       (ygg-git-compare-submit-tests--endpoint c)))
                          ygg-git-compare-submit-tests--calls))
    (should (string-search "1 kept" (car ygg-git-compare-submit-tests--messages)))))

(defun ygg-git-compare-submit-tests--request-changes (state honours)
  (setq ygg-git-compare-submit-tests--reviewer-state state
        ygg-git-compare-submit-tests--honours honours)
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "s" :level 'review))
      ygg-git-compare-submit-tests--gitlab-pr
      #'ygg-git-compare-submit-tests--lab-reviewer
    (ygg-git-compare-submit-forge 'request-changes)
    (list (reverse ygg-git-compare-submit-tests--calls)
          ygg-git-compare-submit-tests--dropped
          (car ygg-git-compare-submit-tests--messages))))

(defun ygg-git-compare-submit-tests--quick-action-notes (calls)
  (seq-filter (lambda (c) (equal (plist-get (nth 2 c) :body) "/request_changes")) calls))

(ert-deftest ygg-git-compare-submit-gitlab-request-changes-posts-the-quick-action ()
  (pcase-let ((`(,calls ,dropped ,message)
               (ygg-git-compare-submit-tests--request-changes "unreviewed" t)))
    (let ((notes (ygg-git-compare-submit-tests--quick-action-notes calls)))
      (should (= (length notes) 1))
      (should (string-suffix-p "/merge_requests/7/notes"
                               (ygg-git-compare-submit-tests--endpoint (car notes)))))
    (should-not (seq-find (lambda (c) (string-search "bulk_publish"
                                                     (or (ygg-git-compare-submit-tests--endpoint c) "")))
                          calls))
    (should (equal dropped '("s")))
    (should (string-search "requested changes" message))))

(ert-deftest ygg-git-compare-submit-gitlab-request-changes-names-a-missing-reviewer ()
  (pcase-let ((`(,calls ,dropped ,message) (ygg-git-compare-submit-tests--request-changes nil t)))
    (should-not (ygg-git-compare-submit-tests--quick-action-notes calls))
    (should (equal dropped '("s")))
    (should (string-search "add yourself as a reviewer" message))))

(ert-deftest ygg-git-compare-submit-gitlab-request-changes-notices-an-ignored-state ()
  (pcase-let ((`(,_ ,_ ,message) (ygg-git-compare-submit-tests--request-changes "unreviewed" nil)))
    (should (string-search "did not record the request for changes" message))))

(ert-deftest ygg-git-compare-submit-gitlab-keeps-only-what-failed-on-a-non-user-error ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--gitlab-pr
      (lambda (_program _args body)
        (if (equal (plist-get body :body) "**nit:** line")
            (signal 'file-missing '("Opening input file" "No such file" "/tmp/gone"))
          "{\"id\":1}"))
    (ygg-git-compare-submit-forge 'approve)
    (should (equal (ygg-git-compare-submit-tests--ids) '("line" "proposed")))
    (should (equal (sort (copy-sequence ygg-git-compare-submit-tests--dropped) #'string<)
                   '("range" "summary" "whole")))
    (should (string-search "No such file" (car ygg-git-compare-submit-tests--messages)))))

(defconst ygg-git-compare-submit-tests--diff
  '("diff --git a/a.txt b/a.txt" "index 1..2 100644" "--- a/a.txt" "+++ b/a.txt"
    "@@ -10,4 +12,5 @@ fn" " keep" "-gone" "+new one" "+new two" " tail" " last")
  "A diff in which new lines 12 and 15 and 16 are context for old 10, 12 and 13.")

(ert-deftest ygg-git-compare-submit-context-old-line-reads-the-hunk ()
  (let ((diff ygg-git-compare-submit-tests--diff))
    (should (eql (ygg-git-compare-submit--context-old-line diff 12) 10))
    (should (eql (ygg-git-compare-submit--context-old-line diff 15) 12))
    (should (eql (ygg-git-compare-submit--context-old-line diff 16) 13))
    (should-not (ygg-git-compare-submit--context-old-line diff 13))
    (should-not (ygg-git-compare-submit--context-old-line diff 40))))

(ert-deftest ygg-git-compare-submit-gitlab-position-numbers-a-context-line-on-both-sides ()
  (cl-letf ((ygg-git-compare-submit--memo nil)
            ((symbol-function 'magit-git-lines)
             (lambda (&rest _) ygg-git-compare-submit-tests--diff)))
    (let* ((agent (ygg-git-compare-submit-tests--c "a" :level 'range :new-path "a.txt"
                                                   :old-path "a.txt" :side 'new :line 15
                                                   :start-side 'new :start-line 12))
           (position (ygg-git-compare-submit--gitlab-position
                      agent ygg-git-compare-submit-tests--gitlab-pr))
           (hash (sha1 "a.txt")))
      (should (equal (plist-get position :old_line) 12))
      (should (equal (plist-get position :new_line) 15))
      (should (equal (plist-get position :line_range)
                     `(:start (:line_code ,(concat hash "_10_12"))
                       :end (:line_code ,(concat hash "_12_15"))))))
    (let ((added (ygg-git-compare-submit--gitlab-position
                  (ygg-git-compare-submit-tests--c "n" :level 'line :new-path "a.txt"
                                                   :side 'new :line 13)
                  ygg-git-compare-submit-tests--gitlab-pr)))
      (should-not (plist-member added :old_line)))))

;;; Agent

(ert-deftest ygg-git-compare-submit-agent-sends-markdown-and-drops-agent-comments ()
  (let ((comments (list (ygg-git-compare-submit-tests--c "for pr")
                        (ygg-git-compare-submit-tests--c "for agent" :level 'file
                                                         :file "a.txt" :type 'todo)
                        (ygg-git-compare-submit-tests--c "unchecked" :level 'file
                                                         :file "a.txt" :type 'todo :status 'pending)))
        sent shown)
    (ygg-git-compare-submit-tests--with comments nil #'ignore
      (cl-letf (((symbol-function 'ygg-git-compare--reviewers) (lambda (&rest _) '(("live" . session))))
                ((symbol-function 'y-or-n-p) #'ignore)
                ((symbol-function 'completing-read) (lambda (&rest _) "live"))
                ((symbol-function 'ygg-git-compare-compare-block) (lambda () "<compare/>"))
                ((symbol-function 'aob-prompt) (lambda (s text &rest _) (push (cons s text) sent)))
                ((symbol-function 'aob-trace) (lambda (&rest _) (error "main-area trace")))
                ((symbol-function 'ygg-git-compare-show-trace) (lambda (s) (push s shown))))
        (ygg-git-compare-submit-agent))
      (should (equal shown '(session)))
      (should (eq (caar sent) 'session))
      (should (string-search "1. **[TODO]** `a.txt` - for agent" (cdar sent)))
      (should-not (string-search "unchecked" (cdar sent)))
      (should-not (string-search "for pr" (cdar sent)))
      (should (equal ygg-git-compare-submit-tests--dropped '("for agent")))
      (should-not ygg-git-compare-submit-tests--calls))))

(ert-deftest ygg-git-compare-submit-agent-refuses-when-nothing-qualifies ()
  (let ((comments (list (ygg-git-compare-submit-tests--c "for pr")
                        (ygg-git-compare-submit-tests--c "unchecked" :level 'file
                                                         :file "a.txt" :type 'todo :status 'pending)))
        sent)
    (ygg-git-compare-submit-tests--with comments nil #'ignore
      (cl-letf (((symbol-function 'ygg-git-compare--reviewers) (lambda (&rest _) '(("live" . session))))
                ((symbol-function 'y-or-n-p) #'ignore)
                ((symbol-function 'completing-read) (lambda (&rest _) "live"))
                ((symbol-function 'ygg-git-compare-compare-block) (lambda () "<compare/>"))
                ((symbol-function 'aob-prompt) (lambda (&rest args) (push args sent)))
                ((symbol-function 'ygg-git-compare-show-trace) #'ignore))
        (should (string-search "1 goes to the PR; press D on a comment to make it a todo or fix"
                               (cadr (should-error (ygg-git-compare-submit-agent)
                                                   :type 'user-error))))
        (should-not sent)
        (should-not ygg-git-compare-submit-tests--dropped)))))

(ert-deftest ygg-git-compare-submit-select-picks-by-type ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "todo" :type 'todo)
            (ygg-git-compare-submit-tests--c "fix" :type 'fix)
            (ygg-git-compare-submit-tests--c "nit" :type 'nit)
            (ygg-git-compare-submit-tests--c "plain")
            (ygg-git-compare-submit-tests--c "old" :to 'agent :type 'nit))
      nil #'ignore
    (let ((ids (lambda (to) (mapcar (lambda (c) (plist-get c :id))
                                    (ygg-git-compare-submit--select to)))))
      (should (equal (funcall ids 'forge) '("nit" "plain" "old")))
      (should (equal (funcall ids 'agent) '("todo" "fix")))
      (should (equal (length (funcall ids nil)) 5)))))

(defconst ygg-git-compare-submit-tests--typed
  (list (ygg-git-compare-submit-tests--c "todo" :level 'file :file "a.txt" :type 'todo)
        (ygg-git-compare-submit-tests--c "nit" :level 'file :file "b.txt" :type 'nit)))

(ert-deftest ygg-git-compare-submit-forge-leaves-agent-comments-held ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--typed
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'comment)
    (should (equal ygg-git-compare-submit-tests--dropped '("nit")))
    (should (equal (ygg-git-compare-submit-tests--ids) '("todo")))
    (should-not (string-search "a.txt" (plist-get (nth 2 (car (ygg-git-compare-submit-tests--posts))) :body)))))

(ert-deftest ygg-git-compare-submit-agent-leaves-forge-comments-held ()
  (let (sent)
    (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--typed nil #'ignore
      (cl-letf (((symbol-function 'ygg-git-compare--reviewers) (lambda (&rest _) '(("live" . session))))
                ((symbol-function 'y-or-n-p) #'ignore)
                ((symbol-function 'completing-read) (lambda (&rest _) "live"))
                ((symbol-function 'ygg-git-compare-compare-block) (lambda () "<compare/>"))
                ((symbol-function 'aob-prompt) (lambda (_s text &rest _) (push text sent)))
                ((symbol-function 'ygg-git-compare-show-trace) #'ignore))
        (ygg-git-compare-submit-agent))
      (should-not (string-search "b.txt" (car sent)))
      (should (equal ygg-git-compare-submit-tests--dropped '("todo")))
      (should (equal (ygg-git-compare-submit-tests--ids) '("nit"))))))

(ert-deftest ygg-git-compare-submit-export-takes-every-checked-comment ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--typed nil #'ignore
    (pcase-let ((`(,n . ,text) (ygg-git-compare-submit--export-text nil)))
      (should (= n 2))
      (should (string-search "a.txt" text))
      (should (string-search "b.txt" text)))
    (should (equal (ygg-git-compare-submit-tests--ids) '("todo" "nit")))))

(ert-deftest ygg-git-compare-submit-gitlab-reports-each-comment-then-the-approval ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--gitlab-pr (lambda (&rest _) "{\"id\":1}")
    (ygg-git-compare-submit-forge 'approve)
    (should (equal (reverse ygg-git-compare-submit-tests--messages)
                   (list "Posting 1/4…" "Posting 3/4…" "Posting 4/4…" "Approving…"
                         (format "Posted 4 comments to gitlab MR !7 and approved it (%s)"
                                 (plist-get ygg-git-compare-submit-tests--gitlab-pr :url)))))))

(ert-deftest ygg-git-compare-submit-gitlab-names-the-first-failure-and-keeps-the-rest ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--gitlab-pr
      (lambda (_program _args body)
        (if (equal (plist-get body :body) "**nit:** line")
            (user-error "400 position is invalid")
          "{\"id\":1}"))
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (car ygg-git-compare-submit-tests--messages)
                   "Posted 3 of 4; 1 kept: 400 position is invalid"))
    (should (equal (ygg-git-compare-submit-tests--ids) '("line" "proposed")))))

(ert-deftest ygg-git-compare-submit-refuses-a-second-review-while-one-is-in-flight ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--typed
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (let (pending)
      (cl-letf (((symbol-function 'ygg-git-compare--forge-async)
                 (lambda (_program _args callback &optional _timeout)
                   (push callback pending))))
        (ygg-git-compare-submit-forge 'comment)
        (should (= (length pending) 1))
        (should (string-search "already being posted"
                               (cadr (should-error (ygg-git-compare-submit-forge 'comment)
                                                   :type 'user-error))))
        (should (= (length ygg-git-compare-submit-tests--prompts) 1))
        (funcall (pop pending) 0 "{}" "")
        (should (equal ygg-git-compare-submit-tests--dropped '("nit")))
        (should (string-prefix-p "Posted 1 comment to"
                                 (car ygg-git-compare-submit-tests--messages)))
        (should (string-search "No comments for the pull request"
                               (cadr (should-error (ygg-git-compare-submit-forge 'comment)
                                                   :type 'user-error))))))))

(ert-deftest ygg-git-compare-submit-says-what-was-also-decided ()
  (dolist (case '((approve . " and approved it") (request-changes . " and requested changes")))
    (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
        ygg-git-compare-submit-tests--gitlab-pr
        #'ygg-git-compare-submit-tests--lab-reviewer
      (setq ygg-git-compare-submit-tests--reviewer-state "unreviewed"
            ygg-git-compare-submit-tests--honours t)
      (ygg-git-compare-submit-forge (car case))
      (should (equal (car ygg-git-compare-submit-tests--messages)
                     (format "Posted 4 comments to gitlab MR !7%s (%s)" (cdr case)
                             (plist-get ygg-git-compare-submit-tests--gitlab-pr :url)))))))


(ert-deftest ygg-git-compare-submit-forge-refusal-counts-agent-comments ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "todo" :level 'file :file "a.txt" :type 'todo))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (should (string-search "1 goes to the agent; give it a type"
                           (cadr (should-error (ygg-git-compare-submit-forge 'comment)
                                               :type 'user-error))))))

(ert-deftest ygg-git-compare-submit-forge-refusal-pluralises ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "a" :level 'file :file "a.txt" :type 'todo)
            (ygg-git-compare-submit-tests--c "b" :level 'file :file "a.txt" :type 'fix))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (should (string-search "2 go to the agent; give one a type"
                           (cadr (should-error (ygg-git-compare-submit-forge 'comment)
                                               :type 'user-error))))))

(ert-deftest ygg-git-compare-submit-agent-refusal-pluralises ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "a") (ygg-git-compare-submit-tests--c "b"))
      nil #'ignore
    (cl-letf (((symbol-function 'ygg-git-compare--reviewers) (lambda (&rest _) '(("live" . session)))))
      (should (string-search "2 go to the PR; press D on a comment to make it a todo or fix"
                             (cadr (should-error (ygg-git-compare-submit-agent)
                                                 :type 'user-error)))))))

(defun ygg-git-compare-submit-tests--agent-send ()
  (let (sent)
    (cl-letf (((symbol-function 'ygg-git-compare--reviewers) (lambda (&rest _) '(("live" . session))))
              ((symbol-function 'completing-read) (lambda (&rest _) "live"))
              ((symbol-function 'ygg-git-compare-compare-block) (lambda () "<compare/>"))
              ((symbol-function 'aob-prompt) (lambda (s text &rest _) (push (cons s text) sent)))
              ((symbol-function 'ygg-git-compare-show-trace) #'ignore))
      (ygg-git-compare-submit-agent))
    sent))

(defmacro ygg-git-compare-submit-tests--accepting (&rest body)
  `(cl-letf (((symbol-function 'ygg-git-compare--accept-comment)
              (lambda (c)
                (setq ygg-git-compare-submit-tests--comments
                      (mapcar (lambda (x) (if (equal (plist-get x :id) (plist-get c :id))
                                              (plist-put (copy-sequence x) :status nil)
                                            x))
                              ygg-git-compare-submit-tests--comments)))))
     ,@body))

(ert-deftest ygg-git-compare-submit-agent-asks-to-accept-pending-and-sends-them ()
  (let ((comments (list (ygg-git-compare-submit-tests--c "proposed" :level 'file :file "a.txt"
                                                         :type 'todo :status 'pending
                                                         :author "claude: geo")
                        (ygg-git-compare-submit-tests--c "proposed too" :level 'file :file "a.txt"
                                                         :type 'fix :status 'pending
                                                         :author "claude: geo")))
        asked)
    (ygg-git-compare-submit-tests--with comments nil #'ignore
      (ygg-git-compare-submit-tests--accepting
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) t)))
         (let ((sent (ygg-git-compare-submit-tests--agent-send)))
           (should (equal asked '("Accept and send 2 pending todo/fix comments from claude: geo? ")))
           (should (string-search "proposed too" (cdar sent)))
           (should (equal ygg-git-compare-submit-tests--dropped '("proposed" "proposed too")))))))))

(ert-deftest ygg-git-compare-submit-agent-declined-pending-send-nothing ()
  (let ((comments (list (ygg-git-compare-submit-tests--c "proposed" :level 'file :file "a.txt"
                                                         :type 'todo :status 'pending))))
    (ygg-git-compare-submit-tests--with comments nil #'ignore
      (cl-letf (((symbol-function 'y-or-n-p) #'ignore)
                ((symbol-function 'ygg-git-compare--reviewers) (lambda (&rest _) '(("live" . session)))))
        (should-error (ygg-git-compare-submit-agent) :type 'user-error)
        (should-not ygg-git-compare-submit-tests--dropped)))))

(ert-deftest ygg-git-compare-submit-agent-sends-whole-comments-and-keeps-the-rest ()
  (let ((comments (list (ygg-git-compare-submit-tests--c (make-string 300 ?a) :level 'file
                                                         :file "a.txt" :type 'todo)
                        (ygg-git-compare-submit-tests--c (make-string 300 ?b) :level 'file
                                                         :file "b.txt" :type 'todo)
                        (ygg-git-compare-submit-tests--c (make-string 300 ?c) :level 'file
                                                         :file "c.txt" :type 'todo)))
        (ygg-git-compare-review-comments-max-chars 800)
        (ygg-git-compare-export-intro ""))
    (ygg-git-compare-submit-tests--with comments nil #'ignore
      (let ((sent (ygg-git-compare-submit-tests--agent-send)))
        (should (string-search (make-string 300 ?a) (cdar sent)))
        (should (string-search (make-string 300 ?b) (cdar sent)))
        (should-not (string-search (make-string 300 ?c) (cdar sent)))
        (should (equal ygg-git-compare-submit-tests--dropped
                       (mapcar (lambda (c) (plist-get c :id)) (seq-take comments 2))))
        (should (seq-some (lambda (m) (string-search "sent 2 of 3; 1 kept for the next send" m))
                          ygg-git-compare-submit-tests--messages))))))

(ert-deftest ygg-git-compare-submit-agent-refusal-with-no-comments-is-bare ()
  (ygg-git-compare-submit-tests--with nil nil #'ignore
    (cl-letf (((symbol-function 'ygg-git-compare--reviewers) (lambda (&rest _) '(("live" . session)))))
      (should (equal "No comments for the agent"
                     (cadr (should-error (ygg-git-compare-submit-agent)
                                         :type 'user-error)))))))

(ert-deftest ygg-git-compare-submit-approve-without-comments-posts-an-approval ()
  (ygg-git-compare-submit-tests--with nil ygg-git-compare-submit-tests--gitlab-pr
      (lambda (&rest _) "{}")
    (ygg-git-compare-submit-forge 'approve)
    (should (equal (mapcar #'ygg-git-compare-submit-tests--endpoint
                           (ygg-git-compare-submit-tests--posts))
                   '("projects/grp%2Fproj/merge_requests/7/approve")))))

(defconst ygg-git-compare-submit-tests--inside-and-outside
  (list (ygg-git-compare-submit-tests--c "near" :level 'line :file "a.txt" :old-path "a.txt"
                                         :new-path "a.txt" :side 'new :line 2)
        (ygg-git-compare-submit-tests--c "far" :level 'line :file "a.txt" :old-path "a.txt"
                                         :new-path "a.txt" :side 'new :line 40)))

(defmacro ygg-git-compare-submit-tests--with-forge-hunks (hunks &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'ygg-git-compare-submit--forge-hunks)
              (lambda (&rest _) ,hunks)))
     ,@body))

(ert-deftest ygg-git-compare-submit-github-line-outside-the-hunks-goes-in-the-summary ()
  (ygg-git-compare-submit-tests--with-forge-hunks '((1 7 1 7))
    (ygg-git-compare-submit-tests--with
        ygg-git-compare-submit-tests--inside-and-outside
        ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
      (ygg-git-compare-submit-forge 'comment)
      (should-not ygg-git-compare-submit-tests--comments)
      (let ((posts (ygg-git-compare-submit-tests--posts)))
        (should (= (length posts) 1))
        (let ((review (nth 2 (car posts))))
          (should (equal (plist-get review :body) "`a.txt:40` far"))
          (should (equal (mapcar (lambda (c) (plist-get c :line)) (plist-get review :comments))
                         '(2))))))))

(ert-deftest ygg-git-compare-submit-gitlab-line-outside-the-hunks-goes-in-the-summary ()
  (ygg-git-compare-submit-tests--with-forge-hunks '((1 7 1 7))
    (ygg-git-compare-submit-tests--with
        ygg-git-compare-submit-tests--inside-and-outside
        ygg-git-compare-submit-tests--gitlab-pr (ygg-git-compare-submit-tests--lab)
      (ygg-git-compare-submit-forge 'comment)
      (should-not ygg-git-compare-submit-tests--comments)
      (let ((posts (ygg-git-compare-submit-tests--posts)))
        (should (= (length posts) 2))
        (should (equal (mapcar (lambda (p) (plist-get (nth 2 p) :body)) posts)
                       '("`a.txt:40` far" "near")))
        (should-not (plist-member (nth 2 (car posts)) :position))
        (should (plist-get (nth 2 (cadr posts)) :position))))))

(ert-deftest ygg-git-compare-submit-line-inside-the-hunks-or-unknown-stays-inline ()
  (let ((near (car ygg-git-compare-submit-tests--inside-and-outside))
        (far (cadr ygg-git-compare-submit-tests--inside-and-outside)))
    (ygg-git-compare-submit-tests--with-forge-hunks '((1 7 1 7))
      (should-not (ygg-git-compare-submit--outside-p near ygg-git-compare-submit-tests--github-pr))
      (should (ygg-git-compare-submit--outside-p far ygg-git-compare-submit-tests--github-pr)))
    (ygg-git-compare-submit-tests--with-forge-hunks 'unknown
      (should-not (ygg-git-compare-submit--outside-p far ygg-git-compare-submit-tests--github-pr)))))

;;; ygg-git-compare-submit-tests.el ends here

(ert-deftest ygg-git-compare-submit-asks-git-once-for-a-pair-and-path ()
  (let ((asked 0)
        (ygg-git-compare-submit--memo nil))
    (cl-letf (((symbol-function 'magit-git-lines)
               (lambda (&rest _) (cl-incf asked) ygg-git-compare-submit-tests--diff))
              ((symbol-function 'magit-toplevel) (lambda (&rest _) "/r/")))
      (ygg-git-compare-submit--moved-heads
       (list (ygg-git-compare-submit-tests--c "a" :level 'line :new-path "a.txt"
                                              :old-path "a.txt" :side 'new :line 12)
             (ygg-git-compare-submit-tests--c "b" :level 'line :new-path "a.txt"
                                              :old-path "a.txt" :side 'new :line 15))
       ygg-git-compare-submit-tests--gitlab-pr)
      (should (= asked 1))
      (ygg-git-compare-submit--gitlab-position
       (ygg-git-compare-submit-tests--c "a" :level 'line :new-path "a.txt"
                                        :old-path "a.txt" :side 'new :line 15)
       ygg-git-compare-submit-tests--gitlab-pr)
      (should (= asked 1))
      (ygg-git-compare-submit--moved-heads nil ygg-git-compare-submit-tests--gitlab-pr)
      (ygg-git-compare-submit--forge-hunks ygg-git-compare-submit-tests--gitlab-pr "a.txt" "a.txt")
      (should (= asked 2)))))

(defvar ygg-git-compare-submit-tests--refetched nil)

(defmacro ygg-git-compare-submit-tests--conversation (spec answer &rest body)
  (declare (indent 2))
  `(let ((ygg-git-compare--b-spec ,spec)
         (ygg-git-compare-submit-tests--refetched nil))
     (ygg-git-compare-submit-tests--with nil nil ,answer
       (setq ygg-git-compare--b-spec ,spec)
       (cl-letf (((symbol-function 'ygg-git-compare--pr-info-refetch)
                  (lambda () (setq ygg-git-compare-submit-tests--refetched t))))
         (unwind-protect (progn ,@body)
           (when (get-buffer "compose:conversation:o/r#12") (kill-buffer "compose:conversation:o/r#12"))
           (when (get-buffer "compose:conversation:grp/proj#7") (kill-buffer "compose:conversation:grp/proj#7")))))))

(defun ygg-git-compare-submit-tests--send (name text)
  (with-current-buffer name
    (erase-buffer)
    (insert text)
    (aob-compose-send)))

(ert-deftest ygg-git-compare-comment-conversation-github-posts-an-issue-comment ()
  (ygg-git-compare-submit-tests--conversation
      '(pr :forge github :host "github.com" :path "o/r" :number 12) (lambda (&rest _) "{}")
    (ygg-git-compare-comment-conversation)
    (ygg-git-compare-submit-tests--send "compose:conversation:o/r#12" "hello there")
    (let ((call (car ygg-git-compare-submit-tests--calls)))
      (should (equal (car call) "gh"))
      (should (equal (ygg-git-compare-submit-tests--endpoint call) "repos/o/r/issues/12/comments"))
      (should (equal (plist-get (nth 2 call) :body) "hello there")))
    (should ygg-git-compare-submit-tests--refetched)
    (should (equal (car ygg-git-compare-submit-tests--messages) "Commented on #12"))))

(ert-deftest ygg-git-compare-comment-conversation-gitlab-posts-a-note ()
  (ygg-git-compare-submit-tests--conversation
      '(pr :forge gitlab :host "gitlab.com" :path "grp/proj" :number 7) (lambda (&rest _) "{}")
    (ygg-git-compare-comment-conversation)
    (ygg-git-compare-submit-tests--send "compose:conversation:grp/proj#7" "hello there")
    (let ((call (car ygg-git-compare-submit-tests--calls)))
      (should (equal (car call) "glab"))
      (should (equal (ygg-git-compare-submit-tests--endpoint call)
                     "projects/grp%2Fproj/merge_requests/7/notes"))
      (should (equal (plist-get (nth 2 call) :body) "hello there")))
    (should ygg-git-compare-submit-tests--refetched)))

(ert-deftest ygg-git-compare-comment-conversation-failure-keeps-the-draft ()
  (ygg-git-compare-submit-tests--conversation
      '(pr :forge github :host "github.com" :path "o/r" :number 12)
      (lambda (&rest _) (error "boom"))
    (ygg-git-compare-comment-conversation)
    (ygg-git-compare-submit-tests--send "compose:conversation:o/r#12" "keep me")
    (sleep-for 0.1)
    (should-not ygg-git-compare-submit-tests--refetched)
    (should (string-search "boom" (car ygg-git-compare-submit-tests--messages)))
    (should (equal (with-current-buffer "compose:conversation:o/r#12" (string-trim (buffer-string)))
                   "keep me"))))

(ert-deftest ygg-git-compare-comment-conversation-needs-a-pull-request ()
  (ygg-git-compare-submit-tests--conversation '(rev :diff "x") (lambda (&rest _) "{}")
    (should-error (ygg-git-compare-comment-conversation) :type 'user-error)
    (should-not ygg-git-compare-submit-tests--calls)))

;;; Never twice

(ert-deftest ygg-git-compare-submit-gitlab-marks-each-comment-sent-as-it-lands ()
  (let (during)
    (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
        ygg-git-compare-submit-tests--gitlab-pr
        (lambda (_program _args body)
          (when (equal (plist-get body :body) "**nit:** line")
            (setq during (mapcar (lambda (c) (plist-get c :id))
                                 (ygg-git-compare-submit--select 'forge))))
          "{\"id\":1}")
      (ygg-git-compare-submit-forge 'comment)
      (should (member "summary" ygg-git-compare-submit-tests--dropped))
      (should-not (member "summary" during))
      (should (member "line" during)))))

(ert-deftest ygg-git-compare-submit-marks-sent-in-the-store-when-the-buffer-is-gone ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--github-pr
      (lambda (&rest _) (kill-buffer (current-buffer)) "{}")
    (setq ygg-git-compare--store '("file" . "key"))
    (ygg-git-compare-submit-forge 'comment)
    (let ((mark (car ygg-git-compare-submit-tests--marked)))
      (should (equal (car mark) '("summary" "line" "range" "whole")))
      (should (equal (cadr mark) '("file" . "key"))))))

(ert-deftest ygg-git-compare-submit-a-second-send-posts-nothing-already-sent ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (cl-letf (((symbol-function 'ygg-git-compare-comments-drop) #'ignore))
      (ygg-git-compare-submit-forge 'comment)
      (should (= (length (ygg-git-compare-submit-tests--posts)) 1))
      (should-error (ygg-git-compare-submit-forge 'comment) :type 'user-error)
      (should (= (length (ygg-git-compare-submit-tests--posts)) 1))
      (should-not (ygg-git-compare-submit--select 'forge)))))

(ert-deftest ygg-git-compare-submit-a-failed-send-keeps-the-draft-and-a-resend-posts-it ()
  (let ((fail t))
    (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--mixed
        ygg-git-compare-submit-tests--github-pr
        (lambda (&rest _) (if fail (user-error "gh api: 502") "{}"))
      (ygg-git-compare-submit-forge 'comment)
      (should-not ygg-git-compare-submit-tests--marked)
      (should (= (length (ygg-git-compare-submit--select 'forge)) 4))
      (setq fail nil)
      (ygg-git-compare-submit-forge 'comment)
      (should (equal (car (car ygg-git-compare-submit-tests--marked))
                     '("summary" "line" "range" "whole")))
      (should (= (length (ygg-git-compare-submit-tests--posts)) 2)))))

;;; Already on the forge

(defun ygg-git-compare-submit-tests--remote-line (id text path line)
  (list :remote t :id id :level 'line :text text :new-path path :line line))

(defconst ygg-git-compare-submit-tests--old-drafts
  (list (ygg-git-compare-submit-tests--c "old-line" :level 'line :file "a.txt"
                                         :new-path "a.txt" :side 'new :line 2
                                         :text "  Rename   this\nvariable ")
        (ygg-git-compare-submit-tests--c "old-summary" :level 'review :text "Looks off overall")
        (ygg-git-compare-submit-tests--c "fresh" :level 'line :file "a.txt"
                                         :new-path "a.txt" :side 'new :line 9
                                         :text "Never sent before")))

(defun ygg-git-compare-submit-tests--forge-has (forge)
  (setq ygg-git-compare-submit-tests--remote
        (list (ygg-git-compare-submit-tests--remote-line
              "r1" "**nit:** Rename this variable please" "a.txt" 2)
              (if (eq forge 'github)
                  (list :remote t :id "r2" :level 'review
                        :text "Review: Looks off overall, see inline")
                (list :remote t :id "r2" :level 'review :text "Looks off overall")))))

(ert-deftest ygg-git-compare-submit-github-skips-and-marks-drafts-the-forge-has ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--old-drafts
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-tests--forge-has 'github)
    (ygg-git-compare-submit-forge 'comment)
    (should (equal (car (last ygg-git-compare-submit-tests--marked))
                   '(("old-line" "old-summary") nil)))
    (should (string-search "1 comment on" (car ygg-git-compare-submit-tests--prompts)))
    (should (string-search "2 already on the forge — skipped"
                           (car ygg-git-compare-submit-tests--prompts)))
    (let ((posted (format "%S" (nth 2 (car (ygg-git-compare-submit-tests--posts))))))
      (should (string-search "Never sent before" posted))
      (should-not (string-search "Rename" posted))
      (should-not (string-search "Looks off" posted)))
    (should (string-search "2 already on the forge — skipped"
                           (car ygg-git-compare-submit-tests--messages)))))

(ert-deftest ygg-git-compare-submit-gitlab-skips-drafts-the-forge-has ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--old-drafts
      ygg-git-compare-submit-tests--gitlab-pr (lambda (&rest _) "{\"id\":1}")
    (ygg-git-compare-submit-tests--forge-has 'gitlab)
    (ygg-git-compare-submit-forge 'comment)
    (let ((bodies (mapcar (lambda (call) (plist-get (nth 2 call) :body))
                          (ygg-git-compare-submit-tests--posts))))
      (should (seq-some (lambda (b) (and b (string-search "Never sent before" b))) bodies))
      (should-not (seq-some (lambda (b) (and b (or (string-search "Rename" b)
                                                   (string-search "Looks off" b))))
                            bodies)))
    (should (string-search "2 already on the forge — skipped"
                           (car ygg-git-compare-submit-tests--messages)))))

(ert-deftest ygg-git-compare-submit-sends-nothing-when-the-forge-has-it-all ()
  (ygg-git-compare-submit-tests--with (seq-take ygg-git-compare-submit-tests--old-drafts 2)
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-tests--forge-has 'github)
    (ygg-git-compare-submit-forge 'comment)
    (should-not ygg-git-compare-submit-tests--calls)
    (should-not ygg-git-compare-submit-tests--prompts)
    (should-not ygg-git-compare-submit--busy)
    (should-not (ygg-git-compare-submit--select 'forge))
    (should (string-search "already on" (car ygg-git-compare-submit-tests--messages)))
    (should (string-search "nothing sent" (car ygg-git-compare-submit-tests--messages)))))

(ert-deftest ygg-git-compare-submit-review-draft-needs-a-review-level-remote ()
  (ygg-git-compare-submit-tests--with
      (list (ygg-git-compare-submit-tests--c "summary" :level 'review :text "Looks off"))
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (setq ygg-git-compare-submit-tests--remote
          (list (ygg-git-compare-submit-tests--remote-line "r1" "Looks off" "a.txt" 2)))
    (ygg-git-compare-submit-forge 'comment)
    (should (= (length (ygg-git-compare-submit-tests--posts)) 1))))

(ert-deftest ygg-git-compare-submit-unreadable-forge-asks-and-defaults-to-not-sending ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--old-drafts
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (setq ygg-git-compare-submit-tests--remote '(:unread . "gh: 502"))
    (let ((ygg-git-compare-submit-tests--yes nil))
      (should-error (ygg-git-compare-submit-forge 'comment) :type 'user-error))
    (should (equal (car ygg-git-compare-submit-tests--prompts)
                   "Couldn't read the forge's comments; send anyway? (y or n) "))
    (should (= (length ygg-git-compare-submit-tests--prompts) 1))
    (should-not ygg-git-compare-submit-tests--calls)
    (should-not ygg-git-compare-submit-tests--marked)
    (should-not ygg-git-compare-submit--busy)))

(ert-deftest ygg-git-compare-submit-unreadable-forge-sends-all-when-told-to ()
  (ygg-git-compare-submit-tests--with ygg-git-compare-submit-tests--old-drafts
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (setq ygg-git-compare-submit-tests--remote '(:unread . "gh: 502"))
    (ygg-git-compare-submit-forge 'comment)
    (should (string-search "3 comments on" (car ygg-git-compare-submit-tests--prompts)))
    (should (= (length (ygg-git-compare-submit-tests--posts)) 1))))

(ert-deftest ygg-git-compare-submit-an-approval-still-goes-when-the-forge-has-every-comment ()
  (ygg-git-compare-submit-tests--with (seq-take ygg-git-compare-submit-tests--old-drafts 2)
      ygg-git-compare-submit-tests--github-pr (lambda (&rest _) "{}")
    (ygg-git-compare-submit-tests--forge-has 'github)
    (ygg-git-compare-submit-forge 'approve)
    (should (string-prefix-p "Approve with 0 comments"
                             (car ygg-git-compare-submit-tests--prompts)))
    (should (= (length (ygg-git-compare-submit-tests--posts)) 1))))
