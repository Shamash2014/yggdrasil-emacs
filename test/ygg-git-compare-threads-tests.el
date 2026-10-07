;;; ygg-git-compare-threads-tests.el --- the forge's comments in a compare -*- lexical-binding: t; -*-

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
(require 'ygg-git-compare-submit)
(require 'ygg-git-compare-threads)

(defvar ygg-git-compare-threads-tests--spawned nil)
(defvar ygg-git-compare-threads-tests--real nil)
(defvar browsed nil)

(defun ygg-git-compare-threads-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defmacro ygg-git-compare-threads-tests--with-repo (root head &rest body)
  "BODY in a repo bound to ROOT: feature, at HEAD, rewrites lines 2 and 3 of
a.txt and adds b.txt; main stays checked out.  The forge is never reached."
  (declare (indent 2))
  `(let* ((process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (,root (file-name-as-directory
                  (file-truename (make-temp-file "ygg-git-compare-threads-" t))))
          (default-directory ,root)
          (magit-refresh-verbose nil)
          (ygg-git-compare-threads-tests--spawned nil)
          (ygg-git-compare-threads-tests--real nil)
          (ygg-git-compare--caches (make-hash-table :test #'equal))
          (ygg-git-compare--inflight (make-hash-table :test #'equal))
          (ygg-git-compare--forge-repos (make-hash-table :test #'equal))
          (ygg-git-compare-threads-width 80)
          (kill-ring nil)
          (async (symbol-function 'ygg-git-compare--forge-async))
          ,head)
     (unwind-protect
         (cl-flet ((git (&rest args) (apply #'ygg-git-compare-threads-tests--git ,root args))
                   (write (file text) (with-temp-file (expand-file-name file ,root)
                                        (insert text))))
           (git "init" "-q" "-b" "main")
           (git "config" "user.name" "Threads Test")
           (git "config" "user.email" "threads@example.invalid")
           (git "config" "commit.gpgsign" "false")
           (write "a.txt" "1\n2\n3\n4\n5\n")
           (git "add" ".")
           (git "commit" "-q" "-m" "base")
           (git "checkout" "-q" "-b" "feature")
           (write "a.txt" "1\ntwo\nthree\n4\n5\n")
           (write "b.txt" "b1\nb2\n")
           (git "add" ".")
           (git "commit" "-q" "-m" "feature")
           (setq ,head (git "rev-parse" "HEAD"))
           (git "checkout" "-q" "main")
           (cl-letf (((symbol-function 'ygg-git-compare--forge-async)
                      (lambda (program args callback &optional timeout)
                        (cond (ygg-git-compare-threads-tests--real
                               (funcall async program args callback timeout))
                              ((member "statusCheckRollup" args)
                               (funcall callback 0 "{\"statusCheckRollup\":[]}" ""))
                              ((and (equal program "glab")
                                    (string-match-p "merge_requests/[0-9]+\\'" (car (last args))))
                               (funcall callback 0 "{}" ""))
                              (t (push (list program args callback timeout)
                                       ygg-git-compare-threads-tests--spawned)))))
                     ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
             ,@body))
       (dolist (b (buffer-list))
         (when (or (string-prefix-p "compose:review:" (buffer-name b))
                   (and (with-current-buffer b (derived-mode-p 'magit-mode))
                        (file-in-directory-p (buffer-local-value 'default-directory b)
                                             ,root)))
           (kill-buffer b)))
       (delete-directory ,root t))))

(defun ygg-git-compare-threads-tests--pr (forge head)
  (cons 'pr (list :number 12 :sha head :head "feature" :base "main" :remote "origin"
                  :forge forge :host (if (eq forge 'gitlab) "gitlab.com" "github.com")
                  :path "o/r")))

(defmacro ygg-git-compare-threads-tests--with-compare (head forge &rest body)
  (declare (indent 2))
  `(with-current-buffer (ygg-git-compare-buffer
                         default-directory '(rev . "main")
                         (ygg-git-compare-threads-tests--pr ,forge ,head))
     (magit-section-show-level-4-all)
     ,@body))

(defun ygg-git-compare-threads-tests--json (&rest values)
  (json-serialize (vconcat values)))

(defconst ygg-git-compare-threads-tests--stamp "2020-01-01T00:00:00Z")

(defun ygg-git-compare-threads-tests--graphql (&rest nodes)
  (json-serialize
   (list :data (list :repository
                     (list :pullRequest
                           (list :reviewThreads
                                 (list :nodes (vconcat nodes))))))))

(defun ygg-git-compare-threads-tests--github (args)
  "What gh prints for ARGS: one thread of two on the new line 2, a comment on the
removed line 3, an outdated one, a file comment, an approval and a note."
  (let ((stamp ygg-git-compare-threads-tests--stamp)
        (path (car (last args))))
    (cond
     ((member "graphql" args)
      (ygg-git-compare-threads-tests--graphql
       (list :isResolved t :isOutdated :false
             :comments (list :nodes (vector (list :databaseId 101) (list :databaseId 102))))
       (list :isResolved :false :isOutdated t
             :comments (list :nodes (vector (list :databaseId 104))))
       (list :isResolved :false :isOutdated :false
             :comments (list :nodes (vector (list :databaseId 103) (list :databaseId 105))))))
     ((string-search "pulls/12/comments" path)
      (ygg-git-compare-threads-tests--json
       (list :id 101 :path "a.txt" :line 2 :original_line 2 :side "RIGHT" :body "why two?"
             :user (list :login "alice") :created_at stamp :html_url "https://x/c101")
       (list :id 102 :in_reply_to_id 101 :path "a.txt" :line 2 :side "RIGHT" :body "to be clear"
             :user (list :login "bob") :created_at "2020-01-02T00:00:00Z" :html_url "https://x/c102")
       (list :id 103 :path "a.txt" :line 3 :side "LEFT" :body "old three" :start_line 2
             :user (list :login "carol") :created_at stamp :html_url "https://x/c103")
       (list :id 104 :path "a.txt" :original_line 9 :side "RIGHT" :body "gone code"
             :user (list :login "dave") :created_at stamp :html_url "https://x/c104")
       (list :id 105 :path "b.txt" :subject_type "file" :body "whole file"
             :user (list :login "erin") :created_at stamp :html_url "https://x/c105")))
     ((string-search "pulls/12/reviews" path)
      (ygg-git-compare-threads-tests--json
       (list :id 7 :state "APPROVED" :body "ship it" :user (list :login "frank")
             :submitted_at "2020-01-02T00:00:00Z" :html_url "https://x/r7")
       (list :id 8 :state "COMMENTED" :body "" :user (list :login "gina")
             :submitted_at stamp)))
     ((string-search "issues/12/comments" path)
      (ygg-git-compare-threads-tests--json
       (list :id 900 :body "conversation note" :user (list :login "hank")
             :created_at "2020-01-03T00:00:00Z" :html_url "https://x/n900"))))))

(defun ygg-git-compare-threads-tests--gitlab (_args)
  (json-serialize
   (vector
    (list :id "d1" :notes
          (vector (list :id 1 :body "line note" :system :false :resolvable t :resolved t
                        :author (list :username "ivy") :created_at ygg-git-compare-threads-tests--stamp
                        :position (list :new_path "a.txt" :old_path "a.txt" :new_line 2
                                        :head_sha "oldhead"))
                  (list :id 2 :body "reply" :system :false :resolvable t :resolved t
                        :author (list :username "jo") :created_at "2020-01-02T00:00:00Z"
                        :position (list :new_path "a.txt" :old_path "a.txt" :new_line 2))))
    (list :id "d2" :notes
          (vector (list :id 3 :body "added 1 commit" :system t
                        :author (list :username "ivy") :created_at ygg-git-compare-threads-tests--stamp)))
    (list :id "d3" :notes
          (vector (list :id 4 :body "general" :system :false :resolvable :false
                        :author (list :username "kim") :created_at ygg-git-compare-threads-tests--stamp)))
    (list :id "d4" :notes
          (vector (list :id 5 :body "removed line" :system :false :resolvable t :resolved :false
                        :author (list :username "lou") :created_at ygg-git-compare-threads-tests--stamp
                        :position (list :new_path "a.txt" :old_path "a.txt" :old_line 3)))))))

(defun ygg-git-compare-threads-tests--answer (answer)
  "Finish every spawned request with what ANSWER prints for its arguments: a
string, or (STATUS . WHAT-IT-SAID)."
  (let ((spawned (reverse ygg-git-compare-threads-tests--spawned)))
    (setq ygg-git-compare-threads-tests--spawned nil)
    (pcase-dolist (`(,_program ,args ,callback ,_timeout) spawned)
      (let ((out (funcall answer args)))
        (if (stringp out)
            (funcall callback 0 out "")
          (funcall callback (car out) "" (cdr out)))))))

(defun ygg-git-compare-threads-tests--spec (forge)
  (list :forge forge :host (if (eq forge 'gitlab) "gitlab.com" "github.com")
        :path "o/r" :number 12))

(defun ygg-git-compare-threads-tests--results (forge answer)
  (let ((pr (ygg-git-compare-threads-tests--spec forge)))
    (mapcar (lambda (request)
              (let ((out (funcall answer (cddr request))))
                (cons (car request)
                      (if (stringp out) (list 0 out "") (list (car out) "" (cdr out))))))
            (ygg-git-compare--remote-requests pr))))

(defun ygg-git-compare-threads-tests--value (forge answer &optional old)
  (ygg-git-compare--remote-parse (ygg-git-compare-threads-tests--spec forge)
                                 (ygg-git-compare-threads-tests--results forge answer)
                                 old))

(defun ygg-git-compare-threads-tests--parsed (forge answer)
  (plist-get (ygg-git-compare-threads-tests--value forge answer) :comments))

(defun ygg-git-compare-threads-tests--find (comments id)
  (seq-find (lambda (c) (equal (plist-get c :id) id)) comments))

(defun ygg-git-compare-threads-tests--key (&optional forge)
  (ygg-git-compare--remote-key (ygg-git-compare-threads-tests--spec (or forge 'github))))

(defun ygg-git-compare-threads-tests--table ()
  (ygg-git-compare--cache-table (ygg-git-compare--gitdir)))

(defun ygg-git-compare-threads-tests--keep (forge value &optional age)
  (puthash (ygg-git-compare-threads-tests--key forge)
           (list :value value :time (- (float-time) (or age 0)))
           (ygg-git-compare-threads-tests--table)))

(defun ygg-git-compare-threads-tests--seed (_head forge answer &optional age)
  "Keep the forge's comments for the compare as ANSWER prints them."
  (ygg-git-compare-threads-tests--keep
   forge (ygg-git-compare-threads-tests--value forge answer) age))

(defun ygg-git-compare-threads-tests--age (seconds)
  (let* ((table (ygg-git-compare-threads-tests--table))
         (key (ygg-git-compare-threads-tests--key))
         (entry (gethash key table)))
    (puthash key (plist-put (copy-sequence entry) :time (- (float-time) seconds)) table)))

(defun ygg-git-compare-threads-tests--shown ()
  (mapconcat (lambda (ov) (concat (overlay-get ov 'before-string)
                                  (overlay-get ov 'after-string)))
             (seq-filter (lambda (ov) (overlay-get ov 'ygg-git-compare-comments))
                         (overlays-in (point-min) (point-max)))
             "\n"))

(defun ygg-git-compare-threads-tests--conversation ()
  (if-let* ((section (ygg-git-compare--conversation-section)))
      (buffer-substring-no-properties (oref section start) (oref section end))
    ""))

(defun ygg-git-compare-threads-tests--line-above (text)
  "The diff line the overlay showing TEXT sits under."
  (let ((ov (seq-find (lambda (ov) (string-search text (or (overlay-get ov 'after-string) "")))
                      (overlays-in (point-min) (point-max)))))
    (save-excursion (goto-char (overlay-end ov))
                    (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun ygg-git-compare-threads-tests--notices ()
  (concat (ygg-git-compare-threads-tests--conversation) (ygg-git-compare-threads-tests--top)))

(defun ygg-git-compare-threads-tests--top ()
  (mapconcat (lambda (ov) (or (overlay-get ov 'before-string) ""))
             (overlays-in (point-min) (1+ (point-min))) ""))

(defun ygg-git-compare-threads-tests--goto-plain ()
  (goto-char (point-min))
  (re-search-forward "^ 4$")
  (beginning-of-line))

(defun ygg-git-compare-threads-tests--goto (text)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(defun ygg-git-compare-threads-tests--faces (string pos)
  (let (faces)
    (dolist (face (ensure-list (get-text-property pos 'face string)))
      (setq faces (append faces (ensure-list face))))
    faces))

(ert-deftest ygg-git-compare-threads-github-parses-into-the-model ()
  (let* ((comments (ygg-git-compare-threads-tests--parsed
                    'github #'ygg-git-compare-threads-tests--github))
         (root (ygg-git-compare-threads-tests--find comments "remote:gh:101"))
         (reply (ygg-git-compare-threads-tests--find comments "remote:gh:102"))
         (left (ygg-git-compare-threads-tests--find comments "remote:gh:103"))
         (outdated (ygg-git-compare-threads-tests--find comments "remote:gh:104"))
         (file (ygg-git-compare-threads-tests--find comments "remote:gh:105"))
         (review (ygg-git-compare-threads-tests--find comments "remote:gh-review:7"))
         (note (ygg-git-compare-threads-tests--find comments "remote:gh-note:900")))
    (should (equal (list (plist-get root :author) (plist-get root :text) (plist-get root :side)
                         (plist-get root :line) (plist-get root :level) (plist-get root :resolved))
                   '("alice" "why two?" new 2 line t)))
    (should (= (plist-get reply :depth) 1))
    (should (equal (plist-get reply :thread) 101))
    (should (plist-get reply :resolved))
    (should (equal (list (plist-get left :side) (plist-get left :line)
                         (plist-get left :start-line) (plist-get left :level))
                   '(old 3 2 range)))
    (should (equal (list (plist-get outdated :level) (plist-get outdated :line)
                         (plist-get outdated :orig-line) (plist-get outdated :outdated))
                   '(file nil 9 t)))
    (should (equal (list (plist-get file :level) (plist-get file :outdated)) '(file nil)))
    (should (equal (list (plist-get review :level) (plist-get review :state)) '(review "APPROVED")))
    (should (equal (plist-get note :text) "conversation note"))
    (should-not (ygg-git-compare-threads-tests--find comments "remote:gh-review:8"))))

(ert-deftest ygg-git-compare-threads-gitlab-parses-discussions ()
  (let* ((comments (ygg-git-compare-threads-tests--parsed
                    'gitlab #'ygg-git-compare-threads-tests--gitlab))
         (line (ygg-git-compare-threads-tests--find comments "remote:gl:1")))
    (should (equal (mapcar (lambda (c) (plist-get c :text)) comments)
                   '("line note" "reply" "general" "removed line")))
    (should (equal (list (plist-get line :level) (plist-get line :side) (plist-get line :line)
                         (plist-get line :resolved))
                   '(line new 2 t)))
    (should (= (plist-get (ygg-git-compare-threads-tests--find comments "remote:gl:2") :depth) 1))
    (should (eq (plist-get (ygg-git-compare-threads-tests--find comments "remote:gl:4") :level)
                'review))
    (let ((removed (ygg-git-compare-threads-tests--find comments "remote:gl:5")))
      (should (equal (list (plist-get removed :side) (plist-get removed :line)
                           (plist-get removed :resolved))
                     '(old 3 nil))))
    (should (string-search "/o/r/-/merge_requests/12#note_1" (plist-get line :url)))))

(ert-deftest ygg-git-compare-threads-inline-comment-renders-under-its-line ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (let ((shown (ygg-git-compare-threads-tests--shown)))
        (should (equal (ygg-git-compare-threads-tests--line-above "alice") "+two"))
        (should (equal (ygg-git-compare-threads-tests--line-above "carol") "-3"))
        (should (string-search "alice" shown))
        (should (string-search "y ago" shown))
        (should (string-search "1 reply" shown))
        (should (string-search "resolved" shown))
        (should (string-search "approved" (ygg-git-compare-threads-tests--conversation)))
        (should (string-search "conversation note" (ygg-git-compare-threads-tests--conversation)))
        (should-not (string-search "conversation note" shown))
        (should-not ygg-git-compare-threads-tests--spawned)))))

(ert-deftest ygg-git-compare-threads-outdated-and-file-comments-list-under-their-file ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (should (string-search "a.txt" (ygg-git-compare-threads-tests--line-above "dave")))
      (should (string-search "outdated · was L9" (ygg-git-compare-threads-tests--shown)))
      (should (string-search "b.txt" (ygg-git-compare-threads-tests--line-above "erin"))))))

(ert-deftest ygg-git-compare-threads-stay-out-of-the-local-comments ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--goto "+two")
      (with-current-buffer (ygg-git-compare-comment)
        (insert "mine")
        (aob-compose-send))
      (should (string-search "draft" (ygg-git-compare-threads-tests--shown)))
      (should (equal (mapcar (lambda (c) (plist-get c :text)) (ygg-git-compare-comments-list t))
                     '("mine")))
      (should (equal (mapcar (lambda (c) (plist-get c :text)) (ygg-git-compare-submit--select))
                     '("mine")))
      (ygg-git-compare-export-markdown)
      (should (string-search "mine" (car kill-ring)))
      (should-not (string-search "why two?" (car kill-ring)))
      (should-not (string-search "alice" (car kill-ring))))))

(ert-deftest ygg-git-compare-threads-fetch-is-kept-and-redraws-once ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (should (= (length ygg-git-compare-threads-tests--spawned) 4))
      (ygg-git-compare-refresh)
      (ygg-git-compare--draw-comments)
      (should (= (length ygg-git-compare-threads-tests--spawned) 4))
      (let ((redraws 0)
            (redraw (symbol-function 'ygg-git-compare--redraw-comments)))
        (cl-letf (((symbol-function 'ygg-git-compare--redraw-comments)
                   (lambda (list) (cl-incf redraws) (funcall redraw list))))
          (ygg-git-compare-threads-tests--answer #'ygg-git-compare-threads-tests--github))
        (should (= redraws 1)))
      (should-not ygg-git-compare-threads-tests--spawned)
      (should (string-search "alice" (ygg-git-compare-threads-tests--shown)))
      (ygg-git-compare--draw-comments)
      (should-not ygg-git-compare-threads-tests--spawned)
      (ygg-git-compare-threads-tests--age 500)
      (ygg-git-compare--draw-comments)
      (ygg-git-compare--draw-comments)
      (should (= (length ygg-git-compare-threads-tests--spawned) 4))
      (should (string-search "alice" (ygg-git-compare-threads-tests--shown))))))

(ert-deftest ygg-git-compare-threads-fresh-cache-starts-no-process ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
      (setq ygg-git-compare-threads-tests--spawned nil)
      (ygg-git-compare--draw-comments)
      (should-not ygg-git-compare-threads-tests--spawned))))

(ert-deftest ygg-git-compare-threads-failure-is-one-dim-line ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--answer (lambda (_) '(1 . "HTTP 404: Not Found\nmore")))
      (let ((top (ygg-git-compare-threads-tests--notices)))
        (should (string-search "forge comments: HTTP 404: Not Found" top))
        (should-not (string-search "more" top))))))

(ert-deftest ygg-git-compare-threads-first-render-says-it-is-fetching ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (should (string-search "(fetching comments…)" (ygg-git-compare-threads-tests--top)))
      (ygg-git-compare-threads-tests--answer #'ygg-git-compare-threads-tests--github)
      (should-not (string-search "fetching comments" (ygg-git-compare-threads-tests--top))))))

(ert-deftest ygg-git-compare-threads-branch-compare-fetches-nothing ()
  (ygg-git-compare-threads-tests--with-repo root head
    (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "feature"))
      (magit-section-show-level-4-all)
      (ygg-git-compare--draw-comments)
      (should-not ygg-git-compare-threads-tests--spawned)
      (should (zerop (hash-table-count ygg-git-compare--inflight))))))

(ert-deftest ygg-git-compare-threads-spec-without-a-repo-fetches-nothing ()
  (ygg-git-compare-threads-tests--with-repo root head
    (with-current-buffer (ygg-git-compare-buffer
                          root '(rev . "main")
                          (cons 'pr (list :number 12 :sha head :head "feature" :remote "origin")))
      (magit-section-show-level-4-all)
      (ygg-git-compare--draw-comments)
      (should-not ygg-git-compare-threads-tests--spawned))))

;;; Forge resolution

(defmacro ygg-git-compare-threads-tests--with-glab-config (config &rest body)
  (declare (indent 1))
  `(let* ((config-dir (file-name-as-directory (make-temp-file "ygg-threads-glab-" t)))
          (process-environment
           (append (list (concat "GLAB_CONFIG_DIR=" config-dir) "GITLAB_HOST" "GL_HOST" "LAB_HOST")
                   process-environment)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "config.yml" config-dir) (insert ,config))
           (cl-letf (((symbol-function 'ygg-git-compare--ssh-hostname) #'ignore))
             ,@body))
       (delete-directory config-dir t))))

(ert-deftest ygg-git-compare-threads-self-hosted-gitlab-is-read-from-glab-config ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-glab-config "hosts:\n    code.corp.example:\n"
      (ygg-git-compare-threads-tests--git root "remote" "add" "origin"
                                          "git@code.corp.example:g/p.git")
      (let* ((specs (ygg-git-compare--review-specs
                     (list :number 12 :head head :start head :base-ref "main")
                     "feature" "origin"))
             (spec (cdr (cdr specs))))
        (should (equal (seq-mapn #'plist-get (list spec spec spec) '(:forge :host :path))
                       '(gitlab "code.corp.example" "g/p")))
        (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") (cdr specs))
          (magit-section-show-level-4-all)
          (let ((request (car (last ygg-git-compare-threads-tests--spawned))))
            (should (equal (car request) "glab"))
            (should (member "code.corp.example" (cadr request)))
            (should (seq-some (lambda (a) (string-search "g%2Fp/merge_requests/12/discussions" a))
                              (cadr request)))))))))

(ert-deftest ygg-git-compare-threads-merge-request-and-pull-request-candidates-carry-the-repo ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-glab-config "hosts:\n    code.corp.example:\n"
      (ygg-git-compare-threads-tests--git root "remote" "add" "origin"
                                          "git@code.corp.example:g/p.git")
      (cl-letf (((symbol-function 'ygg-git-compare--merge-requests)
                 (lambda (_repo) (list (list :number 3 :title "t" :headRefName "h"
                                             :baseRefName "main")))))
        (let ((spec (cdr (seq-find (lambda (c) (eq (car-safe (cdr c)) 'pr))
                                   (ygg-git-compare--review-targets)))))
          (should (equal (list (plist-get (cdr spec) :forge) (plist-get (cdr spec) :host)
                               (plist-get (cdr spec) :path))
                         '(gitlab "code.corp.example" "g/p"))))))
    (ygg-git-compare-threads-tests--git root "remote" "set-url" "origin" "git@github.com:o/r.git")
    (clrhash ygg-git-compare--forge-repos)
    (cl-letf (((symbol-function 'ygg-git-compare--pulls)
               (lambda (_repo) (list (list :number 5 :title "p" :headRefName "h"
                                           :baseRefName "main")))))
      (let ((spec (cdr (seq-find (lambda (c) (eq (car-safe (cdr c)) 'pr))
                                 (ygg-git-compare-candidates t)))))
        (should (equal (list (plist-get (cdr spec) :forge) (plist-get (cdr spec) :host)
                             (plist-get (cdr spec) :path))
                       '(github "github.com" "o/r")))))))

(ert-deftest ygg-git-compare-threads-url-rewrites-are-honoured ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-glab-config "hosts:\n    code.corp.example:\n"
      (ygg-git-compare-threads-tests--git root "config" "url.git@code.corp.example:.insteadOf" "cc:")
      (ygg-git-compare-threads-tests--git root "remote" "add" "origin" "cc:g/p.git")
      (should (equal (ygg-git-compare--forge-repo "origin")
                     '(gitlab "code.corp.example" "g/p"))))))

(ert-deftest ygg-git-compare-threads-forge-url-rewritten-to-a-local-mirror-is-still-the-forge ()
  (ygg-git-compare-threads-tests--with-repo root head
    (let ((mirror (file-name-as-directory (make-temp-file "ygg-threads-mirror-" t))))
      (unwind-protect
          (progn
            (ygg-git-compare-threads-tests--git
             root "config" (format "url.%s.insteadOf" mirror) "git@github.com:")
            (ygg-git-compare-threads-tests--git root "remote" "add" "origin" "git@github.com:o/r.git")
            (should (equal (ygg-git-compare--forge-repo "origin") '(github "github.com" "o/r"))))
        (delete-directory mirror t)))))

;;; Keys

(ert-deftest ygg-git-compare-threads-keys-fall-through-matrix ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (let ((keys `(("r" ,#'ygg-git-compare-threads-reply ,#'ygg-git-compare-mark-file-reviewed)
                    ("y" ,#'ygg-git-compare-threads-copy ,#'ygg-git-compare-export-markdown)
                    ("o" ,#'ygg-git-compare-threads-open nil)
                    ("TAB" ,#'ygg-git-compare-threads-toggle-fold nil)
                    ("<tab>" ,#'ygg-git-compare-threads-toggle-fold nil))))
        (dolist (spot '(bol mid eol))
          (dolist (case '(("+two" t) (nil nil)))
            (if (car case)
                (ygg-git-compare-threads-tests--goto (car case))
              (ygg-git-compare-threads-tests--goto-plain))
            (pcase spot
              ('mid (forward-char 1))
              ('eol (end-of-line)))
            (pcase-dolist (`(,key ,remote ,plain) keys)
              (let ((bound (key-binding (kbd key))))
                (if (cadr case)
                    (should (eq bound remote))
                  (should-not (eq bound remote))
                  (when plain (should (eq bound plain))))))))
        (should (eq (key-binding (kbd "H")) #'ygg-git-compare-threads-toggle-resolved))
        (ygg-git-compare-threads-tests--goto-plain)
        (should (eq (key-binding (kbd "TAB")) #'magit-section-toggle))))))

;;; Keeping

(defun ygg-git-compare-threads-tests--stub (n &optional age)
  (list :comments (list (list :remote t :id (format "remote:gh:%d" n) :thread n :level 'file
                              :author "a" :text "t" :created (- (float-time) (or age 0))
                              :file "a.txt" :new-path "a.txt"))))

(defun ygg-git-compare-threads-tests--saved ()
  (ygg-git-compare--cache-rows (ygg-git-compare--gitdir)))

(ert-deftest ygg-git-compare-threads-disk-keeps-at-most-fifty-pull-requests ()
  (ygg-git-compare-threads-tests--with-repo root head
    (let ((table (ygg-git-compare-threads-tests--table))
          (now (float-time)))
      (dotimes (i 70)
        (puthash (list 'threads 'github "github.com" "o/r" i)
                 (list :value (ygg-git-compare-threads-tests--stub i) :time (- now (* 10 (- 70 i))))
                 table))
      (puthash '(pulls (github "github.com" "o/r")) (list :value (list (list :number 1)) :time now) table)
      (ygg-git-compare--cache-save (ygg-git-compare--gitdir) table)
      (let* ((rows (ygg-git-compare-threads-tests--saved))
             (kept (seq-filter (lambda (r) (eq (car-safe (car r)) 'threads)) rows)))
        (should (= (length kept) 50))
        (should (equal (sort (mapcar (lambda (r) (nth 4 (car r))) kept) #'<)
                       (number-sequence 20 69)))
        (should (seq-find (lambda (r) (eq (car-safe (car r)) 'pulls)) rows))))))

(ert-deftest ygg-git-compare-threads-disk-drops-the-old-and-writes-atomically ()
  (ygg-git-compare-threads-tests--with-repo root head
    (let ((table (ygg-git-compare-threads-tests--table))
          (now (float-time)))
      (puthash (list 'threads 'github "github.com" "o/r" 1)
               (list :value (ygg-git-compare-threads-tests--stub 1) :time (- now (* 15 86400)))
               table)
      (puthash (list 'threads 'github "github.com" "o/r" 2)
               (list :value (ygg-git-compare-threads-tests--stub 2) :time (- now 60))
               table)
      (ygg-git-compare--cache-save (ygg-git-compare--gitdir) table)
      (should (equal (mapcar (lambda (r) (nth 4 (car r))) (ygg-git-compare-threads-tests--saved))
                     '(2)))
      (should-not (directory-files (ygg-git-compare--gitdir) nil "\\.lock\\'")))))

(ert-deftest ygg-git-compare-threads-wrong-shapes-on-disk-never-break-a-redraw ()
  (dolist (content '("(((" "42" "\"text\"" "((threads . 1))" "(5 6 7)"
                     "(((threads github \"github.com\" \"o/r\" 12) :value 5 :time 9999999999))"
                     "(((threads github \"github.com\" \"o/r\" 12) :value (:comments 5) :time 1))"
                     "(((threads github \"github.com\" \"o/r\" 12) :value (:comments ((:id 5))) :time 1))"
                     "(((threads github \"github.com\" \"o/r\" 12) :value (:comments ((:id \"remote:x\" :level line :text 5))) :time 1))"))
    (ygg-git-compare-threads-tests--with-repo root head
      (with-temp-file (ygg-git-compare--cache-file (ygg-git-compare--gitdir)) (insert content))
      (ygg-git-compare-threads-tests--with-compare head 'github
        (ygg-git-compare--draw-comments)
        (ygg-git-compare--draw-comments)
        (should-not (string-search "alice" (ygg-git-compare-threads-tests--shown)))))))

(ert-deftest ygg-git-compare-threads-a-comment-that-cannot-be-drawn-is-one-line ()
  (let ((out (ygg-git-compare--remote-block
              (list :remote t :id "remote:x" :author "a" :text "t" :depth "deep" :level 'line))))
    (should (string-search "could not be shown" out))))

(ert-deftest ygg-git-compare-threads-landing-writes-into-the-waiters-git-dir ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (let ((elsewhere (file-name-as-directory (make-temp-file "ygg-threads-else-" t))))
        (unwind-protect
            (with-temp-buffer
              (setq default-directory elsewhere)
              (ygg-git-compare-threads-tests--answer #'ygg-git-compare-threads-tests--github))
          (delete-directory elsewhere t))
        (should (file-exists-p (ygg-git-compare--cache-file (ygg-git-compare--gitdir))))
        (should (ygg-git-compare--remote-value (ygg-git-compare-threads-tests--key)))))))

;;; Resolved state

(ert-deftest ygg-git-compare-threads-graphql-failure-keeps-the-resolved-state-held ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--answer #'ygg-git-compare-threads-tests--github)
      (ygg-git-compare-threads-tests--age 500)
      (ygg-git-compare--draw-comments)
      (ygg-git-compare-threads-tests--answer
       (lambda (args) (if (member "graphql" args) '(1 . "graphql down")
                        (ygg-git-compare-threads-tests--github args))))
      (let* ((value (ygg-git-compare--remote-value (ygg-git-compare-threads-tests--key)))
             (comments (plist-get value :comments)))
        (should (equal (plist-get value :states-error) "graphql down"))
        (should (eq (plist-get (ygg-git-compare-threads-tests--find comments "remote:gh:101")
                               :resolved)
                    t))
        (should (eq (plist-get (ygg-git-compare-threads-tests--find comments "remote:gh:104")
                               :outdated)
                    t))
        (should-not (string-search "· state unknown" (ygg-git-compare-threads-tests--shown)))))))

(ert-deftest ygg-git-compare-threads-unknown-resolved-state-is-its-own-state ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--answer
       (lambda (args) (if (member "graphql" args) '(1 . "graphql down")
                        (ygg-git-compare-threads-tests--github args))))
      (let ((shown (ygg-git-compare-threads-tests--shown)))
        (should (string-search "state unknown" shown))
        (should-not (string-search "· resolved" shown))
        (should (string-search "▾ alice" shown))
        (should (memq 'warning (ygg-git-compare-threads-tests--faces
                                shown (+ 2 (string-search "· state unknown" shown))))))
      (should (string-search "resolved state unknown: graphql down"
                             (ygg-git-compare-threads-tests--notices)))
      (ygg-git-compare-threads-toggle-resolved)
      (should (string-search "alice" (ygg-git-compare-threads-tests--shown))))))

(ert-deftest ygg-git-compare-threads-graphql-errors-document-counts-as-failure ()
  (let* ((value (ygg-git-compare-threads-tests--value
                 'github (lambda (args)
                           (if (member "graphql" args)
                               (json-serialize (list :errors (vector (list :message "bad"))))
                             (ygg-git-compare-threads-tests--github args))))))
    (should (plist-get value :states-error))
    (should (eq (plist-get (ygg-git-compare-threads-tests--find
                            (plist-get value :comments) "remote:gh:101")
                           :resolved)
                'unknown))))

(ert-deftest ygg-git-compare-threads-graphql-is-paginated ()
  (let ((request (seq-find (lambda (r) (eq (car r) 'threads))
                           (ygg-git-compare--remote-requests
                            (ygg-git-compare-threads-tests--spec 'github)))))
    (should (member "--paginate" request))
    (should (seq-some (lambda (a) (string-search "$endCursor" a)) (cdr request)))
    (should (seq-some (lambda (a) (string-search "hasNextPage" a)) (cdr request))))
  (let* ((value (ygg-git-compare-threads-tests--value
                 'github
                 (lambda (args)
                   (if (member "graphql" args)
                       (concat
                        (ygg-git-compare-threads-tests--graphql
                         (list :isResolved t :isOutdated :false
                               :comments (list :nodes (vector (list :databaseId 101)))))
                        (ygg-git-compare-threads-tests--graphql
                         (list :isResolved :false :isOutdated t
                               :comments (list :nodes (vector (list :databaseId 104))))))
                     (ygg-git-compare-threads-tests--github args)))))
         (comments (plist-get value :comments)))
    (should-not (plist-get value :states-error))
    (should (eq (plist-get (ygg-git-compare-threads-tests--find comments "remote:gh:101") :resolved) t))
    (should (eq (plist-get (ygg-git-compare-threads-tests--find comments "remote:gh:104") :outdated) t))))

(ert-deftest ygg-git-compare-threads-gitlab-note-on-another-head-is-outdated ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'gitlab #'ygg-git-compare-threads-tests--gitlab)
    (ygg-git-compare-threads-tests--with-compare head 'gitlab
      (ygg-git-compare--draw-comments)
      (let ((note (ygg-git-compare-threads-tests--find
                   (ygg-git-compare--remote-shown (current-buffer)) "remote:gl:1")))
        (should (plist-get note :outdated))
        (should (equal (list (plist-get note :level) (plist-get note :orig-line)) '(file 2))))
      (should (string-search "a.txt" (ygg-git-compare-threads-tests--line-above "ivy")))
      (should-not (equal (ygg-git-compare-threads-tests--line-above "ivy") "+two")))))

;;; Timeouts

(ert-deftest ygg-git-compare-threads-timeout-lands-an-error-and-frees-the-lookup ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (should (= (hash-table-count ygg-git-compare--inflight) 1))
      (let ((spawned (reverse ygg-git-compare-threads-tests--spawned)))
        (setq ygg-git-compare-threads-tests--spawned nil)
        (dolist (request spawned) (funcall (nth 2 request) 'timeout "" "")))
      (should (zerop (hash-table-count ygg-git-compare--inflight)))
      (should-not ygg-git-compare--remote-waiting)
      (should (string-search "forge comments: gh timed out" (ygg-git-compare-threads-tests--notices)))
      (ygg-git-compare--draw-comments)
      (should-not ygg-git-compare-threads-tests--spawned)
      (let ((ygg-git-compare-forge-retry -1))
        (ygg-git-compare--draw-comments))
      (should (= (length ygg-git-compare-threads-tests--spawned) 4)))))

(ert-deftest ygg-git-compare-threads-a-hung-forge-is-given-up-on ()
  (ygg-git-compare-threads-tests--with-repo root head
    (let ((ygg-git-compare-threads-tests--real t)
          (ygg-git-compare-forge-timeout 0.3))
      (cl-letf (((symbol-function 'ygg-git-compare--remote-requests)
                 (lambda (_pr) (list (list 'inline "sleep" "5")))))
        (ygg-git-compare-threads-tests--with-compare head 'github
          (let ((deadline (+ (float-time) 4)))
            (while (and (> (hash-table-count ygg-git-compare--inflight) 0)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (zerop (hash-table-count ygg-git-compare--inflight)))
          (should (string-search "timed out" (ygg-git-compare-threads-tests--notices))))))))

;;; Rendering

(defun ygg-git-compare-threads-tests--block (&rest props)
  (ygg-git-compare--remote-block
   (append props (list :remote t :author "alice" :created (- (float-time) 7200)
                       :thread 1 :level 'line))))

(defun ygg-git-compare-threads-tests--lines (width &rest props)
  (let ((ygg-git-compare-threads-width width))
    (split-string (apply #'ygg-git-compare-threads-tests--block props) "\n")))

(defun ygg-git-compare-threads-tests--fits (lines width)
  (seq-every-p (lambda (line) (<= (string-width line) width)) lines))

(ert-deftest ygg-git-compare-threads-header-shows-author-age-replies-and-state ()
  (let ((open (ygg-git-compare-threads-tests--block :text "body" :replies 2))
        (done (ygg-git-compare-threads-tests--block :text "body" :replies 1 :resolved t :folded t)))
    (should (string-match-p "▾ alice · 2h ago · 2 replies · open\n.*body" open))
    (should (string-match-p "▸ alice · 2h ago · 1 reply · resolved\\'" done))))

(ert-deftest ygg-git-compare-threads-reply-headers-do-not-repeat-the-state ()
  (let ((reply (ygg-git-compare-threads-tests--block :text "body" :depth 1 :resolved t)))
    (should (string-match-p "alice · 2h ago\n" reply))
    (should-not (string-search "resolved" reply))
    (should-not (string-search "open" reply))))

(ert-deftest ygg-git-compare-threads-replies-are-indented-under-the-thread ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--goto "+two")
      (call-interactively (key-binding (kbd "TAB")))
      (let ((shown (ygg-git-compare-threads-tests--shown)))
        (should (string-search "\n  ▎ bob · " shown))
        (should (string-search "\n  ▎ to be clear" shown))
        (should-not (string-search "bob · 1" shown))))))

(ert-deftest ygg-git-compare-threads-wraps-by-display-width-at-60-and-120 ()
  (dolist (width '(60 120))
    (let* ((text (concat (string-join (make-list 30 "日本語のコメント") " ") " and ascii words "
                         (string-join (make-list 30 "plain") " ")))
           (lines (ygg-git-compare-threads-tests--lines width :text text)))
      (should (> (length lines) 2))
      (should (ygg-git-compare-threads-tests--fits lines width)))))

(ert-deftest ygg-git-compare-threads-nested-lists-indent-and-hang-at-60-and-120 ()
  (dolist (width '(60 120))
    (let* ((item (string-join (make-list 3 "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi omicron pi rho sigma tau") " "))
           (lines (ygg-git-compare-threads-tests--lines
                   width :text (format "- top %s\n  - nested %s\n    - third %s\n1. first %s"
                                       item item item item))))
      (should (ygg-git-compare-threads-tests--fits lines width))
      (should (seq-find (lambda (l) (string-prefix-p "▎ - top" l)) lines))
      (should (seq-find (lambda (l) (string-prefix-p "▎   - nested" l)) lines))
      (should (seq-find (lambda (l) (string-prefix-p "▎     - third" l)) lines))
      (should (seq-find (lambda (l) (string-prefix-p "▎ 1. first" l)) lines))
      (let ((hung (seq-filter (lambda (l) (string-prefix-p "▎     " l)) lines)))
        (should (seq-find (lambda (l) (string-match-p "\\`▎ \\{7\\}[a-z]" l)) hung)))
      (should (seq-find (lambda (l) (string-match-p "\\`▎ \\{5\\}[a-z]" l)) lines))
      (should (seq-find (lambda (l) (string-match-p "\\`▎ \\{3\\}[a-z]" l)) lines)))))

(ert-deftest ygg-git-compare-threads-quotes-keep-their-mark-on-wrapped-lines ()
  (dolist (width '(60 120))
    (let* ((lines (ygg-git-compare-threads-tests--lines
                   width :text (concat "> " (string-join (make-list 25 "quoted words") " ")
                                       "\n>> " (string-join (make-list 25 "deeper") " ")
                                       "\nafter")))
           (body (cdr lines)))
      (should (ygg-git-compare-threads-tests--fits lines width))
      (should (> (length body) 3))
      (should (seq-every-p (lambda (l) (or (string-prefix-p "▎ > " l) (equal l "▎ after")))
                           body))
      (should (seq-find (lambda (l) (string-prefix-p "▎ > > " l)) body)))))

(ert-deftest ygg-git-compare-threads-fenced-code-is-labelled-and-highlighted ()
  (dolist (fence '("```" "~~~"))
    (dolist (width '(60 120))
      (let* ((lines (ygg-git-compare-threads-tests--lines
                     width :text (format "before\n%selisp\n(defun f (x) \"s\")\n%s\nafter" fence fence)))
             (label (seq-position lines "▎ elisp"
                                  (lambda (l s) (string-match-p (concat s "\\'") l))))
             (code (seq-find (lambda (l) (string-search "(defun f" l)) lines)))
        (should label)
        (should code)
        (should (= (seq-position lines code) (1+ label)))
        (should (memq 'font-lock-keyword-face
                      (ygg-git-compare-threads-tests--faces code (string-search "defun" code))))
        (should (equal (car (last lines)) "▎ after"))))))

(ert-deftest ygg-git-compare-threads-fence-closes-only-on-its-own-kind ()
  (let ((lines (ygg-git-compare-threads-tests--lines
                80 :text "~~~\n```\nstill code\n```\n~~~\nprose")))
    (should (seq-find (lambda (l) (string-search "still code" l)) lines))
    (should (seq-find (lambda (l) (string-search "```" l)) lines))
    (should (equal (car (last lines)) "▎ prose"))))

(ert-deftest ygg-git-compare-threads-inline-code-split-by-a-wrap-keeps-its-face ()
  (dolist (width '(60 120))
    (let* ((text (concat (string-join (make-list (if (= width 60) 8 17) "filler") " ")
                         " `alpha beta gamma delta` tail"))
           (lines (ygg-git-compare-threads-tests--lines width :text text))
           (joined (string-join (cdr lines) "\n")))
      (should-not (string-search "`" joined))
      (dolist (word '("alpha" "beta" "gamma" "delta"))
        (should (memq 'ygg-git-compare-remote-code
                      (ygg-git-compare-threads-tests--faces
                       joined (string-search word joined)))))
      (should-not (memq 'ygg-git-compare-remote-code
                        (ygg-git-compare-threads-tests--faces joined (string-search "tail" joined)))))))

(ert-deftest ygg-git-compare-threads-suggestion-old-lines-skip-removed-hunk-lines ()
  (let* ((hunk "@@ -1,4 +1,4 @@\n 1\n+two\n-old three\n+three")
         (lines (ygg-git-compare-threads-tests--lines
                 80 :text "```suggestion\nTWO\nTHREE\n```" :line 3 :start-line 2
                 :diff-hunk hunk))
         (text (string-join lines "\n")))
    (should (string-search "- two" text))
    (should (string-search "- three" text))
    (should-not (string-search "old three" text))
    (should (string-search "+ TWO" text))
    (should (string-search "+ THREE" text))
    (should (string-search "suggestion" text))))

(ert-deftest ygg-git-compare-threads-resolved-start-folded-and-tab-toggles ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (let ((shown (ygg-git-compare-threads-tests--shown)))
        (should (string-search "▸ alice" shown))
        (should-not (string-search "to be clear" shown))
        (should (string-search "▾ carol" shown))
        (should (string-search "old three" shown)))
      (ygg-git-compare-threads-tests--goto "+two")
      (should (eq (key-binding (kbd "TAB")) #'ygg-git-compare-threads-toggle-fold))
      (call-interactively (key-binding (kbd "TAB")))
      (let ((shown (ygg-git-compare-threads-tests--shown)))
        (should (string-search "▾ alice" shown))
        (should (string-search "to be clear" shown)))
      (ygg-git-compare-threads-tests--goto "+two")
      (call-interactively (key-binding (kbd "TAB")))
      (should (string-search "▸ alice" (ygg-git-compare-threads-tests--shown)))
      (should-not (string-search "to be clear" (ygg-git-compare-threads-tests--shown))))))

(ert-deftest ygg-git-compare-threads-resolved-threads-hide-and-show-with-H ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (should (string-search "alice" (ygg-git-compare-threads-tests--shown)))
      (call-interactively (key-binding (kbd "H")))
      (let ((shown (ygg-git-compare-threads-tests--shown)))
        (should-not (string-search "alice" shown))
        (should (string-search "carol" shown)))
      (call-interactively (key-binding (kbd "H")))
      (should (string-search "alice" (ygg-git-compare-threads-tests--shown))))))

(ert-deftest ygg-git-compare-threads-conversation-section-is-chronological ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (let ((text (ygg-git-compare-threads-tests--conversation)))
        (should (string-search "Conversation (2)" text))
        (should (< (string-search "Conversation (2)" text) (string-search "frank" text)))
        (should (< (string-search "frank" text) (string-search "hank" text)))
        (should (string-search "approved" text))
        (should (string-search "ship it" text))
        (should-not (string-search "frank" (ygg-git-compare-threads-tests--top)))))))

(ert-deftest ygg-git-compare-threads-draft-follows-the-thread-it-replies-to ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--goto "+two")
      (with-current-buffer (call-interactively (key-binding "r"))
        (insert "my reply")
        (aob-compose-send))
      (let ((shown (ygg-git-compare-threads-tests--shown)))
        (should (< (string-search "alice" shown) (string-search "my reply" shown)))
        (should (string-search "draft" shown))
        (should (equal (ygg-git-compare-threads-tests--faces shown (string-search "draft" shown))
                       '(warning italic)))))))

(ert-deftest ygg-git-compare-threads-navigation-and-actions ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--goto "+two")
      (cl-letf (((symbol-function 'completing-read) (lambda (_p rows &rest _) (caar rows))))
        (call-interactively (key-binding "o"))
        (should (equal browsed "https://x/c101"))
        (call-interactively (key-binding "y"))
        (should (equal (car kill-ring) "why two?"))
        (with-current-buffer (call-interactively (key-binding "r"))
          (should (equal (list (plist-get ygg-git-compare--draft :side)
                               (plist-get ygg-git-compare--draft :line))
                         '(new 2))))))))

;;; Resize

(ert-deftest ygg-git-compare-threads-rewrap-follows-the-window-width ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--keep
     'github (list :comments (list (list :remote t :id "remote:gh:1" :thread 1 :level 'line
                                         :author "alice" :created (float-time) :side 'new :line 2
                                         :file "a.txt" :new-path "a.txt"
                                         :text (string-join (make-list 40 "word") " ")))))
    (ygg-git-compare-threads-tests--with-compare head 'github
      (let ((ygg-git-compare-threads-width 60))
        (ygg-git-compare--draw-comments))
      (should (memq 'ygg-git-compare--remote-resized window-size-change-functions))
      (let ((narrow (length (split-string (ygg-git-compare-threads-tests--shown) "\n"))))
        (setq ygg-git-compare-threads-width 120)
        (ygg-git-compare--remote-resized)
        (let ((first ygg-git-compare--remote-rewrap-timer))
          (should (timerp first))
          (ygg-git-compare--remote-resized)
          (should-not (memq first timer-idle-list))
          (should (eq (timer--function ygg-git-compare--remote-rewrap-timer)
                      #'ygg-git-compare--remote-rewrap)))
        (apply (timer--function ygg-git-compare--remote-rewrap-timer)
               (timer--args ygg-git-compare--remote-rewrap-timer))
        (cancel-function-timers #'ygg-git-compare--remote-rewrap)
        (should (< (length (split-string (ygg-git-compare-threads-tests--shown) "\n")) narrow))))))

;;; imenu

(ert-deftest ygg-git-compare-threads-imenu-shows-author-and-first-words ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--seed head 'github #'ygg-git-compare-threads-tests--github)
    (ygg-git-compare-threads-tests--with-compare head 'github
      (let* ((labels nil))
        (cl-labels ((walk (items)
                      (dolist (item items)
                        (push (car item) labels)
                        (when (listp (cdr item)) (walk (cdr item))))))
          (walk (ygg-git-compare--imenu-index)))
        (should (member "▎ alice: why two?" labels))
        (should (member "▎ carol: old three" labels))
        (should-not (seq-find (lambda (l) (string-search "remote:" l)) labels))))))

(ert-deftest ygg-git-compare-threads-imenu-label-has-no-markdown ()
  (dolist (case '(("```elisp\n(foo)\n```\nreal words" . "bob: (foo) real words")
                  ("## Heading **bold** and `code`" . "bob: Heading bold and code")
                  ("  *a*\n\n  _b_   snake_case" . "bob: a b snake_case")))
    (should (equal (ygg-git-compare--remote-label (list :author "bob" :text (car case)))
                   (cdr case)))))

(ert-deftest ygg-git-compare-threads-a-comment-past-the-first-hundred-of-its-thread-is-unknown ()
  (let* ((value (ygg-git-compare-threads-tests--value
                 'github
                 (lambda (args)
                   (if (member "graphql" args)
                       (ygg-git-compare-threads-tests--graphql
                        (list :isResolved t :isOutdated :false
                              :comments (list :nodes (vector (list :databaseId 101)))))
                     (ygg-git-compare-threads-tests--github args)))))
         (comments (plist-get value :comments)))
    (should-not (plist-get value :states-error))
    (should (eq (plist-get (ygg-git-compare-threads-tests--find comments "remote:gh:101") :resolved) t))
    (should (eq (plist-get (ygg-git-compare-threads-tests--find comments "remote:gh:104") :resolved)
                'unknown))))

;;; Guards

(ert-deftest ygg-git-compare-threads-render-runs-no-process ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--answer #'ygg-git-compare-threads-tests--github)
      (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (error "call-process")))
                ((symbol-function 'process-file) (lambda (&rest _) (error "process-file")))
                ((symbol-function 'start-process) (lambda (&rest _) (error "start-process")))
                ((symbol-function 'make-process) (lambda (&rest _) (error "make-process")))
                ((symbol-function 'accept-process-output) (lambda (&rest _) (error "accept")))
                ((symbol-function 'ygg-git-compare--forge-repo) (lambda (&rest _) (error "forge-repo"))))
        (ygg-git-compare--draw-comments)
        (should (string-search "alice" (ygg-git-compare-threads-tests--shown)))
        (ygg-git-compare-threads-tests--age 500)
        (setq ygg-git-compare-threads-tests--spawned nil)
        (ygg-git-compare--draw-comments)
        (should (string-search "alice" (ygg-git-compare-threads-tests--shown)))
        (should (= (length ygg-git-compare-threads-tests--spawned) 4))))))

(ert-deftest ygg-git-compare-threads-many-comments-render-fast ()
  (ygg-git-compare-threads-tests--with-repo root head
    (ygg-git-compare-threads-tests--with-compare head 'github
      (ygg-git-compare-threads-tests--keep
       'github
       (list :comments
             (cl-loop for i below 200
                      collect (list :remote t :id (format "remote:gh:%d" i) :thread i
                                    :author "alice" :created (- (float-time) i)
                                    :text "one `two` three four five six seven eight nine ten\n\n- item\n```\ncode\n```"
                                    :file "a.txt" :new-path "a.txt" :level 'line
                                    :side 'new :line (+ 2 (% i 2))))))
      (let ((elapsed (car (benchmark-run 1 (ygg-git-compare--draw-comments)))))
        (should (< elapsed 2))
        (should (string-search "alice" (ygg-git-compare-threads-tests--shown)))))))

(provide 'ygg-git-compare-threads-tests)
;;; ygg-git-compare-threads-tests.el ends here
