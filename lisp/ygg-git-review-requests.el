;;; ygg-git-review-requests.el --- pull and merge requests awaiting your review -*- lexical-binding: t; -*-

;;; Commentary:
;; A magit status section listing the open pull and merge requests that
;; ask for your review.  The forge is asked in the background; the
;; section draws from the last answer and never waits for the next.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'iso8601)
(require 'magit)

(declare-function ygg-git-compare--remote "ygg-git-compare" ())
(declare-function ygg-git-compare--forge-repo "ygg-git-compare" (&optional remote))
(declare-function ygg-git-compare-review-branch "ygg-git-compare" (&optional target))

(defgroup ygg-git-review-requests nil
  "Review requests in magit status."
  :group 'magit)

(defcustom ygg-git-review-requests t
  "Whether magit status lists the requests waiting for your review."
  :type 'boolean)

(defcustom ygg-git-review-requests-ttl 300
  "Seconds before a repository's review requests are asked for again."
  :type 'integer)

(defcustom ygg-git-review-requests-timeout 30
  "Seconds before a forge that has not answered is given up on."
  :type 'integer)

(defcustom ygg-git-review-requests-file
  (locate-user-emacs-file "var/ygg-review-requests.eld")
  "File keeping the last answers between sessions; nil keeps none."
  :type '(choice file (const nil)))

(defvar ygg-git-review-requests--loaded nil)
(defvar ygg-git-review-requests--cache (make-hash-table :test 'equal))
(defvar ygg-git-review-requests--running (make-hash-table :test 'equal))
(defvar ygg-git-review-requests--watchers (make-hash-table :test 'equal))
(defvar ygg-git-review-requests--users (make-hash-table :test 'equal))
(defvar ygg-git-review-requests--lookups (make-hash-table :test 'equal))
(defvar ygg-git-review-requests--repos (make-hash-table :test 'equal))

(defun ygg-git-review-requests--spawn (command callback)
  "Run COMMAND without waiting, then call CALLBACK with its exit status,
standard output and standard error; a status of nil means it did not start."
  (let* ((default-directory (if (file-remote-p default-directory)
                                temporary-file-directory
                              default-directory))
         (out (generate-new-buffer " *ygg-review-requests*"))
         (err (generate-new-buffer " *ygg-review-requests-err*")))
    (condition-case nil
        (let ((process
               (make-process
                :name "ygg-review-requests" :buffer out :stderr err :command command
                :noquery t :connection-type 'pipe
                :sentinel (lambda (process _event)
                           (unless (process-live-p process)
                             (let ((status (process-exit-status process))
                                   (text (with-current-buffer out (buffer-string)))
                                   (errtext (and (buffer-live-p err)
                                                 (with-current-buffer err (buffer-string)))))
                               (kill-buffer out)
                               (when (buffer-live-p err)
                                 (kill-buffer err))
                               (funcall callback status text errtext)))))))
          (process-put process 'err err)
          process)
      (file-error
       (kill-buffer out)
       (kill-buffer err)
       (funcall callback nil "")
       nil))))

(defun ygg-git-review-requests--kill (process)
  (set-process-sentinel process #'ignore)
  (let ((err (process-get process 'err)))
    (when-let* ((pipe (and (buffer-live-p err) (get-buffer-process err))))
      (delete-process pipe))
    (delete-process process)
    (dolist (buffer (list (process-buffer process) err))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ygg-git-review-requests--time (stamp)
  (when stamp
    (ignore-errors (float-time (encode-time (iso8601-parse stamp))))))

(defun ygg-git-review-requests--parse (forge text)
  "The requests in TEXT, a forge's JSON, as plists."
  (let ((items (json-parse-string text :object-type 'plist :array-type 'list
                                  :null-object nil :false-object nil)))
    (mapcar (lambda (item)
              (if (eq forge 'github)
                  (list :number (plist-get item :number)
                        :title (plist-get item :title)
                        :author (plist-get (plist-get item :author) :login)
                        :head (plist-get item :headRefName)
                        :base (plist-get item :baseRefName)
                        :url (plist-get item :url)
                        :draft (plist-get item :isDraft)
                        :updated (ygg-git-review-requests--time (plist-get item :updatedAt)))
                (list :number (plist-get item :iid)
                      :title (plist-get item :title)
                      :author (plist-get (plist-get item :author) :username)
                      :head (plist-get item :source_branch)
                      :base (plist-get item :target_branch)
                      :url (plist-get item :web_url)
                      :draft (or (plist-get item :draft)
                                 (plist-get item :work_in_progress))
                      :updated (ygg-git-review-requests--time (plist-get item :updated_at)))))
            items)))

(defconst ygg-git-review-requests--max-age (* 30 86400))
(defconst ygg-git-review-requests--max-entries 200)

(defun ygg-git-review-requests--row-valid (row)
  (and (proper-list-p row)
       (cl-evenp (length row))
       (integerp (plist-get row :number))
       (cl-every (lambda (key)
                   (let ((value (plist-get row key)))
                     (or (null value) (stringp value))))
                 '(:title :author :head :base :url))
       (let ((updated (plist-get row :updated)))
         (or (null updated) (numberp updated)))
       (memq (plist-get row :draft) '(nil t))))

(defun ygg-git-review-requests--valid (entry)
  (and (consp entry)
       (proper-list-p (car entry))
       (= (length (car entry)) 3)
       (numberp (cadr entry))
       (proper-list-p (cddr entry))
       (plist-member (cddr entry) :rows)
       (proper-list-p (plist-get (cddr entry) :rows))
       (> (cadr entry) (- (float-time) ygg-git-review-requests--max-age))))

(defun ygg-git-review-requests--clean (entry)
  (when (ygg-git-review-requests--valid entry)
    (let* ((rows (plist-get (cddr entry) :rows))
           (good (seq-filter #'ygg-git-review-requests--row-valid rows)))
      (when (or good (null rows))
        (cons (car entry) (cons (cadr entry) (list :rows good)))))))

(defun ygg-git-review-requests--read ()
  (when (and ygg-git-review-requests-file
             (file-readable-p ygg-git-review-requests-file))
    (let ((data (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents ygg-git-review-requests-file)
                    (read (current-buffer))))))
      (and (proper-list-p data)
           (seq-keep #'ygg-git-review-requests--clean data)))))

(defun ygg-git-review-requests--load ()
  (unless ygg-git-review-requests--loaded
    (setq ygg-git-review-requests--loaded t)
    (dolist (entry (ygg-git-review-requests--read))
      (unless (gethash (car entry) ygg-git-review-requests--cache)
        (puthash (car entry) (cdr entry) ygg-git-review-requests--cache)))))

(defun ygg-git-review-requests--locked (file body)
  "Call BODY holding FILE's lock; skip it if the lock stays taken for a second."
  (let ((lock (concat file ".lock"))
        (deadline (+ (float-time) 1))
        held)
    (make-directory (file-name-directory file) t)
    (while (not (or held (> (float-time) deadline)))
      (condition-case nil
          (progn (make-directory lock) (setq held t))
        (file-already-exists
         (let ((attributes (file-attributes lock)))
           (when (and attributes
                      (> (- (float-time) (float-time (file-attribute-modification-time attributes)))
                         10))
             (ignore-errors (delete-directory lock))))
         (sleep-for 0.02))))
    (when held
      (unwind-protect (funcall body)
        (ignore-errors (delete-directory lock))))))

(defun ygg-git-review-requests--save ()
  (when ygg-git-review-requests-file
    (ignore-errors
      (ygg-git-review-requests--locked
       ygg-git-review-requests-file
       (lambda ()
         (let ((merged (make-hash-table :test 'equal))
               (file ygg-git-review-requests-file)
               entries print-length print-level
               (coding-system-for-write 'utf-8))
           (dolist (entry (ygg-git-review-requests--read))
             (puthash (car entry) (cdr entry) merged))
           (maphash (lambda (repo entry)
                      (when (and (plist-member (cdr entry) :rows)
                                 (not (plist-get (cdr entry) :error))
                                 (> (car entry) (car (gethash repo merged '(0)))))
                        (puthash repo entry merged)))
                    ygg-git-review-requests--cache)
           (maphash (lambda (repo entry) (push (cons repo entry) entries)) merged)
           (setq entries (seq-take (sort entries (lambda (a b) (> (cadr a) (cadr b))))
                                   ygg-git-review-requests--max-entries))
           (let ((temp (make-temp-file file)))
             (with-temp-file temp
               (prin1 entries (current-buffer)))
             (rename-file temp file t))))))))

(defun ygg-git-review-requests--store (repo buffers result &optional cell)
  (let* ((cell (or cell (gethash repo ygg-git-review-requests--running)))
         (previous (or (cdr (gethash repo ygg-git-review-requests--cache))
                       (nth 3 cell))))
    (when (and (plist-get result :error) (plist-member previous :rows))
      (setq result (list :rows (plist-get previous :rows)
                         :error (plist-get result :error))))
    (when (timerp (nth 2 cell))
      (cancel-timer (nth 2 cell)))
    (puthash repo (cons (float-time) result) ygg-git-review-requests--cache)
    (ygg-git-review-requests--save)
    (remhash repo ygg-git-review-requests--running)
    (dolist (buffer buffers)
      (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
        (with-current-buffer buffer
          (magit-refresh-buffer))))))

(defun ygg-git-review-requests--failure (program status stderr)
  (let ((line (or (car (split-string (or stderr "") "\n" t "[ \t\r]+")) "")))
    (cond ((string-match-p "auth login\\|401\\|not logged" (or stderr ""))
           (format "%s not authenticated" program))
          ((string-empty-p line) (format "%s failed (exit %s)" program status))
          (t (format "%s: %s" program (truncate-string-to-width line 80))))))

(defun ygg-git-review-requests--finish (repo program status text &optional stderr)
  (let ((buffers (gethash repo ygg-git-review-requests--watchers)))
    (remhash repo ygg-git-review-requests--watchers)
    (ygg-git-review-requests--store
     repo buffers
     (cond ((null status) (list :error (format "%s not found" program)))
           ((/= status 0)
            (list :error (ygg-git-review-requests--failure program status stderr)))
           (t (condition-case nil
                  (list :rows (ygg-git-review-requests--parse (car repo) text))
                (error (list :error (format "%s answered nothing readable" program)))))))))

(defun ygg-git-review-requests--list (repo user)
  (pcase-let ((`(,forge ,host ,path) repo))
    (pcase forge
      ('github
       (ygg-git-review-requests--run
        repo
        (list "gh" "pr" "list" "--repo" (concat host "/" path)
              "--search" "review-requested:@me" "--state" "open" "--limit" "100" "--json"
              "number,title,author,headRefName,baseRefName,url,isDraft,updatedAt")
        (lambda (status text &optional stderr)
          (ygg-git-review-requests--finish repo "gh" status text stderr))))
      ('gitlab
       (ygg-git-review-requests--run
        repo
        (list "glab" "api" "--hostname" host
              (format "projects/%s/merge_requests?state=opened&per_page=100&reviewer_username=%s"
                      (url-hexify-string path) (url-hexify-string user)))
        (lambda (status text &optional stderr)
          (ygg-git-review-requests--finish repo "glab" status text stderr)))))))

(defun ygg-git-review-requests--run (repo command callback)
  (let* ((cell (gethash repo ygg-git-review-requests--running))
         (process (ygg-git-review-requests--spawn
                   command
                   (lambda (status text &optional stderr)
                     (when (eq cell (gethash repo ygg-git-review-requests--running))
                       (funcall callback status text stderr))))))
    (when (and cell (eq cell (gethash repo ygg-git-review-requests--running)))
      (setf (nth 1 cell) process))))

(defun ygg-git-review-requests--abandon (repo)
  (when-let* ((cell (gethash repo ygg-git-review-requests--running)))
    (remhash repo ygg-git-review-requests--running)
    (when (timerp (nth 2 cell))
      (cancel-timer (nth 2 cell)))
    (when (processp (nth 1 cell))
      (ygg-git-review-requests--kill (nth 1 cell)))))

(defun ygg-git-review-requests--expire (repo cell)
  (when (eq cell (gethash repo ygg-git-review-requests--running))
    (ygg-git-review-requests--abandon repo)
    (let ((buffers (gethash repo ygg-git-review-requests--watchers)))
      (remhash repo ygg-git-review-requests--watchers)
      (ygg-git-review-requests--store
       repo buffers
       (list :error (format "%s timed out" (if (eq (car repo) 'github) "gh" "glab")))
       cell))))

(defun ygg-git-review-requests--lookup-expire (host entry)
  (when (eq entry (gethash host ygg-git-review-requests--lookups))
    (remhash host ygg-git-review-requests--lookups)
    (when (processp (nth 0 entry))
      (ygg-git-review-requests--kill (nth 0 entry)))
    (dolist (waiter (reverse (nth 2 entry)))
      (funcall waiter nil 1 "timed out"))))

(defun ygg-git-review-requests--lookup-user (host waiter)
  "Call WAITER with the user name, exit status and standard error once HOST
has been asked who you are; concurrent askers share one question."
  (if-let* ((entry (gethash host ygg-git-review-requests--lookups)))
      (push waiter (nth 2 entry))
    (let ((entry (list nil nil (list waiter)))
          (started nil))
      (puthash host entry ygg-git-review-requests--lookups)
      (unwind-protect
          (progn
            (setf (nth 0 entry)
                  (ygg-git-review-requests--spawn
                   (list "glab" "api" "--hostname" host "user")
                   (lambda (status text &optional stderr)
                     (when (eq entry (gethash host ygg-git-review-requests--lookups))
                       (remhash host ygg-git-review-requests--lookups)
                       (when (timerp (nth 1 entry))
                         (cancel-timer (nth 1 entry)))
                       (let ((name (and (eql status 0)
                                        (ignore-errors
                                          (plist-get (json-parse-string text :object-type 'plist
                                                                        :null-object nil)
                                                     :username)))))
                         (when name
                           (puthash host name ygg-git-review-requests--users))
                         (dolist (waiter (reverse (nth 2 entry)))
                           (funcall waiter name status stderr)))))))
            (when (eq entry (gethash host ygg-git-review-requests--lookups))
              (setf (nth 1 entry)
                    (run-at-time ygg-git-review-requests-timeout nil
                                 #'ygg-git-review-requests--lookup-expire host entry)))
            (setq started t))
        (unless started
          (when (eq entry (gethash host ygg-git-review-requests--lookups))
            (remhash host ygg-git-review-requests--lookups)))))))

(defun ygg-git-review-requests--start (repo &optional previous)
  (let ((cell (list (float-time) nil nil previous))
        (started nil))
    (puthash repo cell ygg-git-review-requests--running)
    (unwind-protect
        (progn
          (setf (nth 2 cell)
                (run-at-time ygg-git-review-requests-timeout nil
                             #'ygg-git-review-requests--expire repo cell))
          (let* ((host (nth 1 repo))
                 (user (gethash host ygg-git-review-requests--users)))
            (if (or (eq (car repo) 'github) user)
                (ygg-git-review-requests--list repo user)
              (ygg-git-review-requests--lookup-user
               host
               (lambda (name status stderr)
                 (when (eq cell (gethash repo ygg-git-review-requests--running))
                   (if name
                       (ygg-git-review-requests--list repo name)
                     (ygg-git-review-requests--finish
                      repo "glab" (if (eql status 0) 1 status) "" stderr)))))))
          (setq started t))
      (unless started
        (when (eq cell (gethash repo ygg-git-review-requests--running))
          (ygg-git-review-requests--abandon repo))))))

(defun ygg-git-review-requests--ensure (repo &optional force)
  "The cached answer for REPO, asking the forge when it is stale or FORCE
and no question is already out."
  (ygg-git-review-requests--load)
  (let ((cell (gethash repo ygg-git-review-requests--running))
        (previous (cdr (gethash repo ygg-git-review-requests--cache))))
    (when force
      (setq previous (or previous (nth 3 cell)))
      (remhash repo ygg-git-review-requests--cache)
      (when (and cell (> (- (float-time) (car cell)) ygg-git-review-requests-timeout))
        (ygg-git-review-requests--abandon repo)))
    (let ((entry (gethash repo ygg-git-review-requests--cache)))
      (cl-pushnew (current-buffer) (gethash repo ygg-git-review-requests--watchers))
      (when (and (not (gethash repo ygg-git-review-requests--running))
                 (or force (null entry)
                     (> (- (float-time) (car entry)) ygg-git-review-requests-ttl)))
        (ygg-git-review-requests--start repo previous))
      (cdr entry))))

(defun ygg-git-review-requests--repo ()
  "The forge repository of this one's remote, from git config alone."
  (require 'ygg-git-compare)
  (when-let* ((remote (ygg-git-compare--remote))
              (url (magit-get "remote" remote "url")))
    (let* ((top (magit-toplevel))
           (hit (gethash top ygg-git-review-requests--repos)))
      (unless (and (equal (car hit) url)
                   (or (nth 2 hit)
                       (< (- (float-time) (nth 1 hit)) ygg-git-review-requests-ttl)))
        (setq hit (puthash top
                           (list url (float-time)
                                 (condition-case nil
                                     (ygg-git-compare--forge-repo remote)
                                   (user-error nil)))
                           ygg-git-review-requests--repos)))
      (nth 2 hit))))

(defun ygg-git-review-requests--age (stamp)
  (if (not (numberp stamp))
      ""
    (let ((seconds (max 0 (- (float-time) stamp))))
      (cond ((< seconds 3600) (format "%dm" (/ seconds 60)))
            ((< seconds 86400) (format "%dh" (/ seconds 3600)))
            (t (format "%dd" (/ seconds 86400)))))))

(defun ygg-git-review-requests--note (text)
  (insert (propertize text 'font-lock-face 'magit-dimmed) "\n\n"))

(defun ygg-git-review-requests--row (pr)
  (let ((face (and (plist-get pr :draft) 'magit-dimmed)))
    (magit-insert-section (review-request pr)
      (magit-insert-heading
        (propertize
         (format "#%s  %s  %s  %s→%s  %s"
                 (plist-get pr :number) (plist-get pr :title)
                 (or (plist-get pr :author) "")
                 (or (plist-get pr :head) "?") (or (plist-get pr :base) "?")
                 (ygg-git-review-requests--age (plist-get pr :updated)))
         'font-lock-face (or face 'default))))))

;;;###autoload
(defun ygg-git-review-requests-insert-section ()
  "Insert the requests waiting for your review, from the last answer."
  (when ygg-git-review-requests
    (magit-insert-section (review-requests)
      (condition-case err
          (let* ((repo (ygg-git-review-requests--repo))
                 (answer (and repo (ygg-git-review-requests--ensure repo))))
            (cond ((null repo)
                   (magit-insert-heading "Review requests")
                   (ygg-git-review-requests--note "reviews: not a forge remote"))
                  ((null answer)
                   (magit-insert-heading "Review requests (fetching…)")
                   (insert ?\n))
                  ((and (plist-get answer :error) (not (plist-member answer :rows)))
                   (magit-insert-heading "Review requests")
                   (ygg-git-review-requests--note
                    (concat "reviews: " (plist-get answer :error))))
                  (t
                   (let ((rows (plist-get answer :rows)))
                     (magit-insert-heading (format "Review requests (%d)" (length rows)))
                     (mapc #'ygg-git-review-requests--row rows)
                     (if (plist-get answer :error)
                         (ygg-git-review-requests--note
                          (format "reviews: %s (showing last list)" (plist-get answer :error)))
                       (insert ?\n))))))
        (error
         (ygg-git-review-requests--note
          (format "reviews: %s" (error-message-string err))))))))

(defun ygg-git-review-requests--at-point ()
  (or (magit-section-value-if 'review-request)
      (user-error "No review request at point")))

(defun ygg-git-review-requests-open ()
  "Open the compare review of the request at point."
  (interactive)
  (let ((pr (ygg-git-review-requests--at-point)))
    (require 'ygg-git-compare)
    (ygg-git-compare-review-branch
     (cons 'pr (list :number (plist-get pr :number) :head (plist-get pr :head))))))

(defun ygg-git-review-requests-browse ()
  "Open the request at point in the browser."
  (interactive)
  (browse-url (plist-get (ygg-git-review-requests--at-point) :url)))

(defun ygg-git-review-requests-copy-url ()
  "Copy the URL of the request at point."
  (interactive)
  (let ((url (plist-get (ygg-git-review-requests--at-point) :url)))
    (kill-new url)
    (message "%s" url)))

(defun ygg-git-review-requests-refetch ()
  "Ask the forge for the review requests now, whatever the cache holds."
  (interactive)
  (ygg-git-review-requests--ensure
   (or (ygg-git-review-requests--repo)
       (let ((remote (ygg-git-compare--remote)))
         (if remote
             (user-error "Not a forge remote: %s (%s)" remote
                         (magit-get "remote" remote "url"))
           (user-error "No remote"))))
   t)
  (magit-refresh-buffer))

(defvar-keymap magit-review-requests-section-map
  "r" #'ygg-git-review-requests-refetch)

(defvar-keymap magit-review-request-section-map
  "RET" #'ygg-git-review-requests-open
  "o" #'ygg-git-review-requests-browse
  "y" #'ygg-git-review-requests-copy-url
  "r" #'ygg-git-review-requests-refetch)

(provide 'ygg-git-review-requests)
;;; ygg-git-review-requests.el ends here
