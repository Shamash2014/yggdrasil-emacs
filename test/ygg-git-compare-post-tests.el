;;; ygg-git-compare-post-tests.el --- review comments sent to a pull request or an agent -*- lexical-binding: t; -*-

;;; Code:

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
(require 'ygg-git-compare)
(require 'ygg-git-compare-submit)
(require 'aob)
(require 'aob-acp)

(setq aob-acp-persist-file (make-temp-file "ygg-git-compare-post-sessions-" nil ".eld"))
(setq aob-acp-show-trace nil)

(defvar ygg-lab-host)

(defun ygg-git-compare-post-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-compare-post-tests--write (dir file text)
  (with-temp-file (expand-file-name file dir) (insert text)))

(defmacro ygg-git-compare-post-tests--with-repo (vars url &rest body)
  "A repo whose remote origin is URL, never fetched.  VARS binds (ROOT
BASE HEAD): main holds a.txt and old.txt; feature changes a line of
a.txt and renames old.txt to new.txt with one line changed; main moves
on after."
  (declare (indent 2))
  (pcase-let ((`(,root ,base ,head) vars))
    `(let* ((process-environment
             (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1" "LAB_HOST")
                     process-environment))
            (,root (file-name-as-directory
                    (file-truename (make-temp-file "ygg-git-compare-post-" t))))
            (default-directory ,root)
            (magit-refresh-verbose nil)
            ,base ,head)
       (unwind-protect
           (cl-flet ((git (&rest args) (apply #'ygg-git-compare-post-tests--git ,root args)))
             (git "init" "-q" "-b" "main")
             (git "config" "user.name" "Post Test")
             (git "config" "user.email" "post@example.invalid")
             (git "config" "commit.gpgsign" "false")
             (git "remote" "add" "origin" ,url)
             (ygg-git-compare-post-tests--write ,root "a.txt" "1\n2\n3\n")
             (ygg-git-compare-post-tests--write ,root "old.txt" "l1\nl2\nl3\nl4\nl5\nl6\n")
             (git "add" ".")
             (git "commit" "-q" "-m" "base")
             (setq ,base (git "rev-parse" "HEAD"))
             (git "checkout" "-q" "-b" "feature")
             (ygg-git-compare-post-tests--write ,root "a.txt" "1\ntwo\n3\n")
             (git "mv" "old.txt" "new.txt")
             (ygg-git-compare-post-tests--write ,root "new.txt" "l1\nl2\nL3\nl4\nl5\nl6\n")
             (git "commit" "-q" "-am" "feature")
             (setq ,head (git "rev-parse" "HEAD"))
             (git "checkout" "-q" "main")
             (ygg-git-compare-post-tests--write ,root "a.txt" "1\n2\n3\n4\n")
             (git "commit" "-q" "-am" "main moves")
             ,@body)
         (dolist (b (buffer-list))
           (when (and (with-current-buffer b (derived-mode-p 'magit-mode))
                      (file-in-directory-p (buffer-local-value 'default-directory b) ,root))
             (kill-buffer b)))
         (delete-directory ,root t)))))

(defvar ygg-git-compare-post-tests--calls nil
  "Each forge call made, newest first, as (PROGRAM ARGS BODY).")

(defvar ygg-git-compare-post-tests--messages nil
  "Each message shown, newest first.")

(defmacro ygg-git-compare-post-tests--offline (answer &rest body)
  "BODY with gh and glab never run: each forge call is recorded and ANSWER,
a function of (PROGRAM ARGS BODY), says what it prints."
  (declare (indent 1))
  `(let ((ygg-git-compare-post-tests--calls nil)
         (ygg-git-compare-post-tests--messages nil)
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
               ((symbol-function 'ygg-git-compare--gh) #'ignore)
               ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
               ((symbol-function 'message)
                (lambda (format &rest args)
                  (when format
                    (push (apply #'format-message format args)
                          ygg-git-compare-post-tests--messages))))
               ((symbol-function 'ygg-git-compare--forge-run)
                (lambda (program &rest args)
                  (let* ((input (cadr (member "--input" args)))
                         (body (and input (json-parse-string
                                           (with-temp-buffer
                                             (insert-file-contents input)
                                             (buffer-string))
                                           :object-type 'plist :array-type 'list))))
                    (push (list program args body) ygg-git-compare-post-tests--calls)
                    (funcall ,answer program args body)))))
       ,@body)))

(defun ygg-git-compare-post-tests--goto (text)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(defun ygg-git-compare-post-tests--comment (text at)
  (ygg-git-compare-post-tests--goto at)
  (with-current-buffer (ygg-git-compare-comment)
    (insert text)
    (ygg-git-compare-draft-save)))

(defun ygg-git-compare-post-tests--toggle (text)
  "Send the comment TEXT to the other target, from where it is shown."
  (let ((comment (seq-find (lambda (c) (equal (plist-get c :text) text))
                           (ygg-git-compare-comments-list))))
    (ygg-git-compare--goto-comment (plist-get comment :id))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p table &rest _)
                 (seq-find (lambda (label) (string-search text label))
                           (all-completions "" table)))))
      (ygg-git-compare-comment-toggle-destination))))

(defmacro ygg-git-compare-post-tests--with-compare (a b &rest body)
  (declare (indent 2))
  `(with-current-buffer (ygg-git-compare-buffer default-directory ,a ,b)
     (magit-section-show-level-4-all)
     ,@body))

(defun ygg-git-compare-post-tests--gh-pr (base head)
  (lambda (_program args _body)
    (if (equal (car args) "pr")
        (json-serialize (list :number 12 :state "OPEN" :baseRefOid base
                              :headRefOid head :baseRefName "main"
                              :url "https://github.com/o/r/pull/12"))
      "{}")))

(defun ygg-git-compare-post-tests--pr-spec (head)
  (cons 'pr (list :number 12 :sha head :head "feature" :base "main" :remote "origin")))

(ert-deftest ygg-git-compare-submit-refuses-what-is-not-the-pr-range ()
  (ygg-git-compare-post-tests--with-repo (_root base head) "git@github.com:o/r.git"
    (ygg-git-compare-post-tests--offline (ygg-git-compare-post-tests--gh-pr base head)
      (ygg-git-compare-post-tests--with-compare
          '(rev . "main") (ygg-git-compare-post-tests--pr-spec head)
        (ygg-git-compare-toggle-dots)
        (ygg-git-compare-post-tests--comment "why two" "+two")
        (should (string-search "A..B" (cadr (should-error (ygg-git-compare-submit-forge 'draft)
                                                          :type 'user-error))))
        (ygg-git-compare-toggle-dots)
        (should (string-search "made on" (cadr (should-error (ygg-git-compare-submit-forge 'draft)
                                                             :type 'user-error))))
        (ygg-git-compare-comments-drop
         (mapcar (lambda (c) (plist-get c :id)) (ygg-git-compare-comments-list)))
        (ygg-git-compare-post-tests--comment "fresh" "+two")
        (ygg-git-compare-submit-forge 'draft))
      (should (= (length (seq-filter (lambda (c) (member "--input" (nth 1 c)))
                                     ygg-git-compare-post-tests--calls))
                 1)))
    (ygg-git-compare-post-tests--offline (ygg-git-compare-post-tests--gh-pr base base)
      (ygg-git-compare-post-tests--with-compare
          (cons 'rev base) (ygg-git-compare-post-tests--pr-spec head)
        (ygg-git-compare-post-tests--comment "stale head" "+two")
        (should (string-search "head" (cadr (should-error (ygg-git-compare-submit-forge 'draft)
                                                          :type 'user-error)))))
      (should-not (seq-find (lambda (c) (member "--input" (nth 1 c)))
                            ygg-git-compare-post-tests--calls)))))

(defun ygg-git-compare-post-tests--lab-mr (base start head fail-on)
  (lambda (_program args body)
    (let ((path (seq-find (lambda (a) (string-prefix-p "projects/" a)) args)))
      (cond ((string-search "source_branch=feature" path) "[{\"iid\":7}]")
            ((string-suffix-p "/draft_notes" path)
             (if (equal (plist-get body :note) fail-on)
                 (user-error "glab api: 400 position is invalid")
               "{\"id\":1}"))
            ((string-suffix-p "/merge_requests/7" path)
             (json-serialize
              (list :iid 7 :target_branch "main"
                    :web_url "https://code.example.com/grp/sub/proj/-/merge_requests/7"
                    :diff_refs (list :base_sha base :start_sha start :head_sha head))))
            (t (error "Unexpected glab call %S" args))))))

(ert-deftest ygg-git-compare-submit-gitlab-draft-notes-from-diff-lines ()
  (ygg-git-compare-post-tests--with-repo (root base head)
      "git@code.example.com:grp/sub/proj.git"
    (let ((ygg-lab-host "https://code.example.com")
          (start (ygg-git-compare-post-tests--git root "rev-parse" "main")))
      (ygg-git-compare-post-tests--offline
          (ygg-git-compare-post-tests--lab-mr base start head nil)
        (ygg-git-compare-post-tests--with-compare (cons 'rev base) '(rev . "feature")
          (ygg-git-compare-post-tests--comment "added" "+two")
          (ygg-git-compare-post-tests--comment "removed" "-l3")
          (ygg-git-compare-post-tests--comment "context" " l2")
          (should-not (seq-filter #'ygg-git-compare--forge-p ygg-git-compare--comments))
          (dolist (text '("added" "removed" "context"))
            (ygg-git-compare-post-tests--toggle text))
          (ygg-git-compare-submit-forge 'draft)
          (should-not ygg-git-compare--comments))
        (let ((notes (mapcar (lambda (c) (nth 2 c))
                             (reverse (seq-filter (lambda (c) (nth 2 c))
                                                  ygg-git-compare-post-tests--calls))))
              (refs (list :position_type "text" :base_sha base :start_sha start
                          :head_sha head)))
          (should (member "--hostname" (nth 1 (car ygg-git-compare-post-tests--calls))))
          (should (member "projects/grp%2Fsub%2Fproj/merge_requests/7/draft_notes"
                          (nth 1 (car ygg-git-compare-post-tests--calls))))
          (should (equal notes
                         (list (list :note "added"
                                     :position (append refs '(:old_path "a.txt" :new_path "a.txt"
                                                              :new_line 2)))
                               (list :note "removed"
                                     :position (append refs '(:old_path "old.txt" :new_path "new.txt"
                                                              :old_line 3)))
                               (list :note "context"
                                     :position (append refs '(:old_path "old.txt" :new_path "new.txt"
                                                              :new_line 2 :old_line 2)))))))))))

(ert-deftest ygg-git-compare-review-sends-and-drops-only-agent-comments ()
  (ygg-git-compare-post-tests--with-repo (_root base head) "git@github.com:o/r.git"
    (let ((aob--sessions (make-hash-table :test #'equal))
          (aob--order nil)
          (aob-acp-agents '(("claude" :command ("x"))))
          sent)
      (ygg-git-compare-post-tests--offline (ygg-git-compare-post-tests--gh-pr base head)
        (ygg-git-compare-post-tests--with-compare
            (cons 'rev base) (ygg-git-compare-post-tests--pr-spec head)
          (ygg-git-compare-post-tests--comment "for the pr" "+two")
          (ygg-git-compare-post-tests--comment "for the agent" "-l3")
          (ygg-git-compare-post-tests--toggle "for the agent")
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "new: claude"))
                    ((symbol-function 'aob-acp-spawn)
                     (lambda (&rest _)
                       (aob-create-session :id "fresh" :backend 'acp :name "fresh"
                                           :project default-directory :state 'starting)))
                    ((symbol-function 'aob-prompt)
                     (lambda (_s text &rest _) (push text sent)))
                    ((symbol-function 'aob-trace) #'ignore))
            (ygg-git-compare-review)
            (should (equal (mapcar (lambda (c) (plist-get c :text)) ygg-git-compare--comments)
                           '("for the pr")))
            (ygg-git-compare-review)
            (should (equal (mapcar (lambda (c) (plist-get c :text)) ygg-git-compare--comments)
                           '("for the pr")))))
        (pcase-let ((`(,second ,first) sent))
          (should (string-search "for the agent" first))
          (should-not (string-search "for the pr" first))
          (should-not (string-search "<review-comments>" second))
          (should (string-search "+two" second)))))))

(ert-deftest ygg-git-compare-review-branch-opens-its-pull-request ()
  (ygg-git-compare-post-tests--with-repo (root base head) "git@github.com:o/r.git"
    (ygg-git-compare-post-tests--git root "checkout" "-q" "feature")
    (let (opened)
      (cl-letf (((symbol-function 'ygg-git-compare-open)
                 (lambda (a b) (push (list a b) opened))))
        (ygg-git-compare-post-tests--offline (ygg-git-compare-post-tests--gh-pr base head)
          (with-temp-buffer
            (ygg-git-compare-review-branch))
          (pcase-let ((`(,a ,b) (car opened)))
            (should (equal a (cons 'rev base)))
            (should (equal (plist-get (cdr b) :sha) head))
            (with-current-buffer (ygg-git-compare-buffer root a b)
              (should (equal ygg-git-compare--dots "..."))
              (should (equal (plist-get (ygg-git-compare--this-pr) :number) 12)))))
        (ygg-git-compare-post-tests--offline
            (lambda (_program _args _body)
              (user-error "gh pr: no pull requests found for branch \"feature\""))
          (with-temp-buffer
            (ygg-git-compare-review-branch))
          (should (equal (car opened) '((rev . "main") (rev . "feature"))))
          (should (string-search "No open pull request" (car ygg-git-compare-post-tests--messages))))
        (ygg-git-compare-post-tests--offline
            (lambda (_program _args _body) (user-error "gh pr: HTTP 401 bad credentials"))
          (with-temp-buffer
            (ygg-git-compare-review-branch))
          (should (string-search "bad credentials"
                                 (car ygg-git-compare-post-tests--messages))))))))

(ert-deftest ygg-git-compare-review-branch-opens-a-picked-pull-request ()
  (ygg-git-compare-post-tests--with-repo (_root base head) "git@github.com:o/r.git"
    (let (opened)
      (cl-letf (((symbol-function 'ygg-git-compare-open)
                 (lambda (a b) (push (list a b) opened))))
        (ygg-git-compare-post-tests--offline (ygg-git-compare-post-tests--gh-pr base head)
          (with-temp-buffer
            (ygg-git-compare-review-branch
             (cons 'pr (list :number 12 :head "someone:fork-branch"))))
          (should (seq-find (lambda (call) (member "12" (nth 1 call)))
                            ygg-git-compare-post-tests--calls))
          (pcase-let ((`(,a ,b) (car opened)))
            (should (equal a (cons 'rev base)))
            (should (equal (plist-get (cdr b) :number) 12))
            (should (equal (plist-get (cdr b) :sha) head))))))))

(ert-deftest ygg-git-compare-review-targets-list-gitlab-merge-requests ()
  (ygg-git-compare-post-tests--with-repo (_root _base _head) "git@gitlab.example.com:g/r.git"
    (ygg-git-compare-post-tests--offline
        (lambda (program args _body)
          (should (equal program "glab"))
          (should (string-search "merge_requests?state=opened" (car (last args))))
          (json-serialize (vector (list :iid 7 :title "Fix it" :source_branch "feature"
                                        :target_branch "main"))))
      (cl-letf (((symbol-function 'ygg-git-compare--gh-pulls)
                 (lambda () (error "gh asked on a GitLab remote"))))
        (let* ((targets (ygg-git-compare--review-targets))
               (mr (seq-find (lambda (c) (string-prefix-p "!7 " (car c))) targets)))
          (should mr)
          (should (equal (cdr mr) (cons 'pr (list :number 7 :head "feature" :base "main"))))
          (should (assoc "feature" targets))
          (should-not (seq-find (lambda (c) (eq (cadr c) 'worktree)) targets)))))))

(ert-deftest ygg-git-compare-review-branch-refuses-an-empty-pick ()
  (ygg-git-compare-post-tests--with-repo (_root _base _head) "git@github.com:o/r.git"
    (cl-letf (((symbol-function 'ygg-git-compare--review-targets) #'ignore)
              ((symbol-function 'ygg-git-compare--read)
               (lambda (&rest _) (cons 'rev ""))))
      (should (equal (cadr (should-error (call-interactively #'ygg-git-compare-review-branch)
                                         :type 'user-error))
                     "No branch here")))))

(ert-deftest ygg-git-compare-forge-repo-ignores-the-port-in-a-url ()
  (dolist (url '("ssh://git@gitlab.example.com:2222/g/p.git"
                 "https://gitlab.example.com:8443/g/p.git"
                 "git@gitlab.example.com:g/p.git"))
    (ygg-git-compare-post-tests--with-repo (_root _base _head) url
      (should (equal (ygg-git-compare--forge-repo "origin")
                     '(gitlab "gitlab.example.com" "g/p"))))))

(ert-deftest ygg-git-compare-forge-run-names-a-program-that-will-not-start ()
  (let* ((temporary-file-directory (file-name-as-directory (make-temp-file "forge-run-" t)))
         (program (expand-file-name "ygg-no-such-program" temporary-file-directory)))
    (unwind-protect
        (progn
          (should (string-search program
                                 (cadr (should-error (ygg-git-compare--forge-run program "pr")
                                                     :type 'user-error))))
          (should-not (directory-files temporary-file-directory nil "forge")))
      (delete-directory temporary-file-directory t))))

;;; ygg-git-compare-post-tests.el ends here
