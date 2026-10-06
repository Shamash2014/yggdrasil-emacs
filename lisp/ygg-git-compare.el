;;; ygg-git-compare.el --- compare any two sides of a repository -*- lexical-binding: t; -*-

;;; Commentary:
;; Two sides, each a worktree (with what it has not committed), a branch,
;; a commit or a pull request, compared in the whole frame: magit's own
;; diff of the two on the left, folded to its files, and the diff of the
;; file at point on the right.  No checkout, branch or stash entry is made
;; to get there.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'url-util)

(defvar ygg-magit-diff-line-limit)
(defvar ygg-lab-host)
(defvar aob-acp-agents)
(defvar aob-acp-start-dir)
(defvar aob-prompt-typed)
(defvar aob-compose--dir)
(defvar aob-compose-spawn-function)
(defvar ygg-aob--draft-tree)
(declare-function aob-live-sessions "aob")
(declare-function aob-session-name "aob")
(declare-function aob-session-state "aob")
(declare-function aob-session-cwd "aob")
(declare-function aob-prompt "aob")
(declare-function aob-trace "aob-trace")
(declare-function aob-acp-spawn "aob-acp")
(declare-function ygg-aob--expand-presets "layer-aob" (text))

(defgroup ygg-git-compare nil
  "Compare two sides of a repository."
  :group 'magit-extensions :prefix "ygg-git-compare-")

(defcustom ygg-git-compare-list-width 0.38
  "The share of the frame's width the diff of every file takes."
  :type 'number)

(defcustom ygg-git-compare-follow-delay 0.15
  "Idle seconds after a move before the right pane follows the file at point."
  :type 'number)

(defcustom ygg-git-compare-forge-timeout 30
  "Seconds before a forge lookup that has not answered is given up on."
  :type 'number)

(defcustom ygg-git-compare-fetch-timeout 300
  "Seconds before a git fetch that has not finished is given up on."
  :type 'number)

(defcustom ygg-git-compare-forge-retry 20
  "Seconds before a lookup that failed is tried again."
  :type 'number)

(defcustom ygg-git-compare-pr-ttl 60
  "Seconds before a pull request's state and fetched head are asked for again."
  :type 'number)

(defcustom ygg-git-compare-list-ttl 120
  "Seconds before the open pull or merge requests are asked for again."
  :type 'number)

(defcustom ygg-git-compare-untracked-max-size 1000000
  "Bytes past which an untracked file is shown by its diffstat alone and
left out of a review."
  :type 'natnum)

(defvar-local ygg-git-compare--root nil "The checkout the compare was opened from.")
(defvar-local ygg-git-compare--plan nil
  "How the sides are diffed, as `ygg-git-compare--planned' answers.")
(defvar-local ygg-git-compare--a-spec nil "Side A as picked: (KIND . WHAT).")
(defvar-local ygg-git-compare--b-spec nil "Side B as picked: (KIND . WHAT).")
(defvar-local ygg-git-compare--a nil "Side A as resolved, a plist.")
(defvar-local ygg-git-compare--b nil "Side B as resolved, a plist.")
(defvar-local ygg-git-compare--dots "..."
  "\"...\" diffs from where the sides parted, \"..\" diffs them directly.")
(defvar-local ygg-git-compare--window-config nil)
(defvar-local ygg-git-compare--file-window nil)
(defvar-local ygg-git-compare--shown nil "(RANGE . FILE) the right pane shows.")
(defvar-local ygg-git-compare--timer nil)
(defvar-local ygg-git-compare--comments nil
  "This compare's review comments as kept, newest first.")
(defvar-local ygg-git-compare--list-buffer nil
  "In the right pane, the diff of every file it belongs to.")
(defvar-local ygg-git-compare--note nil
  "What the header says is still under way, or nil.")
(defvar-local ygg-git-compare--review-token nil
  "Identifies the review request the compare was last opened for; answers to
an earlier one are dropped.")
(defvar vertico--input)
(defvar magit--refresh-cache)
(declare-function vertico--exhibit "vertico")

(defmacro ygg-git-compare--with-git-cache (&rest body)
  "Run BODY with magit remembering where the repository and its git directory
are, so the many lookups of one call chain ask git once."
  (declare (indent 0))
  `(let ((magit--refresh-cache (or magit--refresh-cache (list (cons 0 0)))))
     ,@body))

;;; Git

(defun ygg-git-compare--git (&rest args)
  "What git ARGS prints, trimmed; a user error naming git's complaint on failure."
  (with-temp-buffer
    (let* ((err (make-temp-file "ygg-git-compare-"))
           (status (apply #'process-file "git" nil (list t err) nil args)))
      (unwind-protect
          (unless (eql status 0)
            (user-error "git %s: %s" (car args)
                        (string-trim (with-temp-buffer
                                       (insert-file-contents err)
                                       (buffer-string)))))
        (delete-file err)))
    (string-trim (buffer-string))))

(defun ygg-git-compare--commit (rev)
  (ygg-git-compare--git "rev-parse" "--verify" "--quiet" (concat rev "^{commit}")))

(defun ygg-git-compare--remote ()
  (or (magit-primary-remote) (car (magit-list-remotes))))

(defun ygg-git-compare--default-branch ()
  "The branch the repository's work merges into, local when there is one."
  (or (magit-main-branch)
      (when-let* ((remote (ygg-git-compare--remote)))
        (magit-git-string "symbolic-ref" "--short"
                          (format "refs/remotes/%s/HEAD" remote)))
      (magit-get-current-branch)
      "HEAD"))

;;; Forges, asked in the background

(defvar ygg-git-compare--ssh-commands (make-hash-table :test #'equal)
  "Each local repository's core.sshCommand, or `none', by git directory.")

(defun ygg-git-compare--configured-ssh-command ()
  (when-let* (((not (file-remote-p default-directory)))
              (gitdir (magit-gitdir)))
    (let ((known (gethash gitdir ygg-git-compare--ssh-commands 'unread)))
      (when (eq known 'unread)
        (setq known (or (let ((value (magit-get "core.sshCommand")))
                          (and (not (string-empty-p (or value ""))) value))
                        'none))
        (puthash gitdir known ygg-git-compare--ssh-commands))
      (and (stringp known) known))))

(defun ygg-git-compare--ssh-environment ()
  "The variable that keeps git's ssh from asking, and leaves the user's own
choice of ssh in place: GIT_SSH_COMMAND, else core.sshCommand, else GIT_SSH."
  (let ((env (getenv "GIT_SSH_COMMAND"))
        (git-ssh (getenv "GIT_SSH")))
    (if-let* ((command (if (and env (not (string-empty-p env)))
                           env
                         (ygg-git-compare--configured-ssh-command))))
        (list (concat "GIT_SSH_COMMAND=" command " -o BatchMode=yes"))
      (unless (or (and git-ssh (not (string-empty-p git-ssh)))
                  (file-remote-p default-directory))
        (list "GIT_SSH_COMMAND=ssh -o BatchMode=yes")))))

(defun ygg-git-compare--forge-async (program args callback &optional timeout)
  "Run PROGRAM with ARGS on this machine without waiting, then call CALLBACK
with (STATUS TEXT ERR): its exit code, nil when it did not start, or
`timeout' when it was killed after TIMEOUT seconds; what it printed; what
it said.  Nothing it runs may ask a question."
  (let* ((git (equal program "git"))
         (default-directory (if (and (file-remote-p default-directory) (not git))
                                temporary-file-directory
                              default-directory))
         (process-environment
          (append (list "GIT_TERMINAL_PROMPT=0" "GH_PROMPT_DISABLED=1")
                  (and git (ygg-git-compare--ssh-environment))
                  process-environment))
         (out (generate-new-buffer " *ygg-git-compare-forge*"))
         (err (generate-new-buffer " *ygg-git-compare-forge-err*"))
         (open 2)
         (status nil)
         (timer nil)
         (main nil)
         (errproc nil)
         (finish (lambda (status)
                   (when timer (cancel-timer timer))
                   (let ((text (with-current-buffer out (buffer-string)))
                         (said (with-current-buffer err (string-trim (buffer-string)))))
                     (kill-buffer out)
                     (kill-buffer err)
                     (funcall callback status text said))))
         (join (lambda () (when (zerop (cl-decf open)) (funcall finish status)))))
    (condition-case nil
        (progn
          (setq errproc (make-pipe-process
                         :name "ygg-git-compare-forge-err" :buffer err :noquery t
                         :sentinel (lambda (process _event)
                                     (unless (process-live-p process) (funcall join))))
                main (make-process
                      :name "ygg-git-compare-forge" :buffer out :stderr errproc
                      :noquery t :connection-type 'pipe :coding 'utf-8
                      :file-handler git
                      :command (cons program args)
                      :sentinel (lambda (process _event)
                                  (unless (process-live-p process)
                                    (setq status (process-exit-status process))
                                    (funcall join))))
                timer (run-at-time (or timeout ygg-git-compare-forge-timeout) nil
                                   (lambda ()
                                     (when (process-live-p main)
                                       (set-process-sentinel main #'ignore)
                                       (set-process-sentinel errproc #'ignore)
                                       (delete-process main)
                                       (delete-process errproc)
                                       (funcall finish 'timeout))))))
      (file-error
       (when errproc
         (set-process-sentinel errproc #'ignore)
         (delete-process errproc))
       (funcall finish nil)))))

(defun ygg-git-compare--answer (done program status text err parse)
  "Call DONE with PARSE of TEXT, or with nil and what went wrong when PROGRAM,
which ended with STATUS and said ERR, did not answer."
  (cond ((null status) (funcall done nil (format "%s not found" program)))
        ((eq status 'timeout) (funcall done nil (format "%s timed out" program)))
        ((/= status 0) (funcall done nil (if (string-empty-p err)
                                             (format "%s failed" program)
                                           err)))
        (t (let ((value (condition-case nil (list (funcall parse text)) (error nil))))
             (if value
                 (funcall done (car value))
               (funcall done nil (format "%s answered nothing readable" program)))))))

(defvar ygg-git-compare--fetching nil)
(defvar ygg-git-compare--fetch-queue nil)

(defun ygg-git-compare--serially (job)
  "Call JOB with the function that says it is done; jobs run one at a time."
  (if ygg-git-compare--fetching
      (setq ygg-git-compare--fetch-queue (append ygg-git-compare--fetch-queue (list job)))
    (setq ygg-git-compare--fetching t)
    (funcall job (lambda ()
                   (setq ygg-git-compare--fetching nil)
                   (when-let* ((next (pop ygg-git-compare--fetch-queue)))
                     (ygg-git-compare--serially next))))))

(defun ygg-git-compare--fetch-ref (remote refspec done)
  "Fetch REFSPEC from REMOTE into FETCH_HEAD, then call DONE with the commit
there, or with nil and what went wrong.  Fetches share FETCH_HEAD, so they
run one at a time."
  (let ((dir default-directory))
    (ygg-git-compare--serially
     (lambda (release)
       (let ((default-directory dir))
         (ygg-git-compare--forge-async
          "git" (list "fetch" "--quiet" "--no-tags" remote refspec)
          (lambda (status text err)
            (let ((default-directory dir))
              (unwind-protect
                  (ygg-git-compare--answer
                   done "git" status text err
                   (lambda (_text) (ygg-git-compare--commit "FETCH_HEAD")))
                (funcall release))))
          ygg-git-compare-fetch-timeout))))))

(defvar ygg-git-compare--caches (make-hash-table :test #'equal)
  "Each git directory's answers, by key, as (:value V :time T) or, after a
failure, also :failed and :error.")

(defvar ygg-git-compare--inflight (make-hash-table :test #'equal)
  "The lookups under way, by git directory and key, each a cell holding the
functions to call when it lands.")

(defvar ygg-git-compare-candidates-refreshed-hook nil
  "Functions called when a background lookup of pull or merge requests lands.")

(defun ygg-git-compare--cache-file (gitdir)
  (expand-file-name "ygg-forge-cache.eld" gitdir))

(defconst ygg-git-compare--cache-max-age (* 7 86400))
(defconst ygg-git-compare--cache-max-rows 500)

(defun ygg-git-compare--plists-p (value)
  (and (proper-list-p value)
       (seq-every-p (lambda (item) (and (proper-list-p item) (cl-evenp (length item))))
                    value)))

(defun ygg-git-compare--value-valid-p (key value)
  "Whether VALUE is the kind of answer KEY asks for."
  (pcase (car-safe key)
    ((or 'pulls 'mrs) (ygg-git-compare--plists-p value))
    ((or 'head 'gh-url) (stringp value))
    ('pr (or (null value) (ygg-git-compare--plists-p (list value))))
    (_ t)))

(defun ygg-git-compare--cache-row (row)
  "ROW as read from a cache file, as (KEY :value V :time T) with a time no
later than now, or nil when it is not one."
  (when (and (consp row)
             (proper-list-p row)
             (proper-list-p (car row))
             (symbolp (car-safe (car row)))
             (car-safe (car row))
             (proper-list-p (cdr row))
             (cl-evenp (length (cdr row))))
    (let ((value (plist-get (cdr row) :value))
          (time (plist-get (cdr row) :time))
          (now (float-time)))
      (when (and (numberp time)
                 (= time time)
                 (> time (- now ygg-git-compare--cache-max-age))
                 (ygg-git-compare--value-valid-p (car row) value))
        (list (car row) :value value :time (min time now))))))

(defun ygg-git-compare--cache-rows (gitdir)
  "The rows of GITDIR's cache file that are sound; none when it is unreadable."
  (let ((file (ygg-git-compare--cache-file gitdir)))
    (unless (file-remote-p gitdir)
      (let ((data (condition-case nil
                      (and (file-readable-p file)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (read (current-buffer))))
                    (error nil))))
        (and (proper-list-p data)
             (delq nil (mapcar #'ygg-git-compare--cache-row data)))))))

(defun ygg-git-compare--cache-table (gitdir)
  "GITDIR's answers, read from its file the first time."
  (or (gethash gitdir ygg-git-compare--caches)
      (let ((table (make-hash-table :test #'equal)))
        (dolist (row (ygg-git-compare--cache-rows gitdir))
          (puthash (car row) (cdr row) table))
        (puthash gitdir table ygg-git-compare--caches))))

(defun ygg-git-compare--locked (file body)
  "Call BODY holding FILE's lock; skip it if the lock stays taken for a second."
  (let ((lock (concat file ".lock"))
        (deadline (+ (float-time) 1))
        held)
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

(defun ygg-git-compare--cache-save (gitdir table)
  "Write TABLE's answers into GITDIR's file, merged with what other Emacs
sessions wrote there since, the newest of each kept; the old and the
excess dropped."
  (unless (file-remote-p gitdir)
    (ignore-errors
      (let ((file (ygg-git-compare--cache-file gitdir))
            (floor (- (float-time) ygg-git-compare--cache-max-age))
            rows)
        (ygg-git-compare--locked
         file
         (lambda ()
           (pcase-dolist (`(,key . ,entry) (ygg-git-compare--cache-rows gitdir))
             (let ((mine (plist-get (gethash key table) :time)))
               (unless (and (numberp mine) (>= mine (plist-get entry :time)))
                 (puthash key entry table))))
           (maphash (lambda (key entry)
                      (let ((time (plist-get entry :time)))
                        (when (and (numberp time) (> time floor))
                          (push (list key :value (plist-get entry :value) :time time)
                                rows))))
                    table)
           (setq rows (seq-take (sort rows (lambda (a b) (> (plist-get (cdr a) :time)
                                                            (plist-get (cdr b) :time))))
                                ygg-git-compare--cache-max-rows))
           (let ((temp (make-temp-file file))
                 (print-length nil) (print-level nil) (coding-system-for-write 'utf-8))
             (with-temp-file temp
               (prin1 rows (current-buffer))
               (insert "\n"))
             (rename-file temp file t))))))))

(defun ygg-git-compare--cache-entry (key)
  "What is kept under KEY here, as (:value V :time T), or nil."
  (when-let* ((gitdir (magit-gitdir)))
    (gethash key (ygg-git-compare--cache-table gitdir))))

(defun ygg-git-compare--cache-value (key)
  (plist-get (ygg-git-compare--cache-entry key) :value))

(defun ygg-git-compare--cache-drop (key)
  (when-let* ((gitdir (magit-gitdir)))
    (remhash key (ygg-git-compare--cache-table gitdir))))

(defun ygg-git-compare--refreshing-p (&rest kinds)
  "Whether a lookup here is under way whose key starts with one of KINDS."
  (when-let* ((gitdir (magit-gitdir)))
    (let (found)
      (maphash (lambda (id _cell)
                 (when (and (equal (car id) gitdir) (memq (car-safe (cdr id)) kinds))
                   (setq found t)))
               ygg-git-compare--inflight)
      found)))

(defun ygg-git-compare--cache-stale-p (entry ttl &optional retry)
  (let ((now (float-time)))
    (cond ((null entry) t)
          ((numberp (plist-get entry :failed))
           (> (- now (plist-get entry :failed)) (or retry ygg-git-compare-forge-retry)))
          ((numberp (plist-get entry :time))
           (> (- now (plist-get entry :time)) ttl))
          (t t))))

(defun ygg-git-compare--landed (gitdir table key cell value err)
  (remhash (cons gitdir key) ygg-git-compare--inflight)
  (let ((old (gethash key table))
        (now (float-time)))
    (if err
        (puthash key (list :value (plist-get old :value) :time (plist-get old :time)
                           :failed now :error err)
                 table)
      (puthash key (list :value value :time now) table)
      (ygg-git-compare--cache-save gitdir table)))
  (dolist (waiter (reverse (car cell)))
    (with-demoted-errors "Forge answer: %S"
      (funcall waiter value err))))

(defun ygg-git-compare--cached (key ttl fetch-fn on-fresh &optional retry)
  "What was last found under KEY here, or nil, at once.  When that is older
than TTL seconds, or failed more than RETRY, by default
`ygg-git-compare-forge-retry', seconds ago, FETCH-FN is called with a
function taking (VALUE ERR) to say what it found, once however many ask;
ON-FRESH is then called with the same."
  (let* ((gitdir (or (magit-gitdir) (user-error "Not in a git repository")))
         (table (ygg-git-compare--cache-table gitdir))
         (id (cons gitdir key)))
    (when (ygg-git-compare--cache-stale-p (gethash key table) ttl retry)
      (if-let* ((cell (gethash id ygg-git-compare--inflight)))
          (when on-fresh (push on-fresh (car cell)))
        (let ((cell (list (and on-fresh (list on-fresh)))))
          (puthash id cell ygg-git-compare--inflight)
          (let ((land (lambda (value &optional err)
                        (ygg-git-compare--landed gitdir table key cell value err))))
            (condition-case failure
                (funcall fetch-fn land)
              (error (funcall land nil (error-message-string failure))))))))
    (plist-get (gethash key table) :value)))

(defun ygg-git-compare--json (text)
  (json-parse-string text :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun ygg-git-compare--url-key (url)
  "URL as host/owner/name, however git or gh spells it."
  (thread-last url
               (replace-regexp-in-string "\\`\\([a-z+]+://\\(?:[^@/]+@\\)?[^/:]+\\):[0-9]+" "\\1")
               (replace-regexp-in-string "\\`[a-z+]+://" "")
               (replace-regexp-in-string "\\`[^@/]+@" "")
               (replace-regexp-in-string "\\`\\([^/:]+\\):" "\\1/")
               (replace-regexp-in-string "\\(\\.git\\)?/*\\'" "")
               downcase))

(defun ygg-git-compare--gh-repo-url ()
  "The URL of the repository gh last said it takes pull requests from, or nil."
  (ygg-git-compare--cache-value '(gh-url)))

(defun ygg-git-compare--refine-pr-remote ()
  "Ask gh in the background which repository it takes pull requests from,
when this is a GitHub repository; a failure is kept as long as an answer."
  (when (and (executable-find "gh")
             (eq (car (ignore-errors
                        (ygg-git-compare--forge-repo (ygg-git-compare--remote))))
                 'github))
    (ygg-git-compare--cached
     '(gh-url) 86400
     (lambda (done)
       (ygg-git-compare--forge-async
        "gh" '("repo" "view" "--json" "url" "--jq" ".url")
        (lambda (status text err)
          (ygg-git-compare--answer done "gh" status text err #'string-trim))))
     nil
     (and (file-remote-p default-directory) 86400))))

(defun ygg-git-compare--pr-remote ()
  "The remote of the repository gh last said it lists pull requests for,
else the primary one."
  (or (when-let* ((url (ygg-git-compare--gh-repo-url))
                  (key (ygg-git-compare--url-key url)))
        (seq-find (lambda (remote)
                    (when-let* ((remote-url (magit-get "remote" remote "url")))
                      (equal (ygg-git-compare--url-key remote-url) key)))
                  (magit-list-remotes)))
      (ygg-git-compare--remote)))

(defun ygg-git-compare--pr-base (base &optional remote)
  "BASE, a pull request's target branch, as REMOTE has it when it can."
  (let ((remote-base (when-let* ((remote (or remote (ygg-git-compare--remote))))
                       (concat remote "/" base))))
    (if (and remote-base (magit-rev-verify remote-base)) remote-base base)))

;;; Sides

(defun ygg-git-compare--head-key (remote number)
  (list 'head remote number))

(defun ygg-git-compare--side-outdated-p (spec side)
  "Whether SIDE, the resolved SPEC, is waiting for a pull request's commit,
says its head is old, or stands at another commit than the one last fetched."
  (and (eq (car-safe spec) 'pr)
       (not (plist-get (cdr spec) :sha))
       (or (plist-get side :pending)
           (plist-get side :note)
           (let ((sha (ygg-git-compare--cache-value
                       (ygg-git-compare--head-key
                        (or (plist-get (cdr spec) :remote) (ygg-git-compare--remote))
                        (plist-get (cdr spec) :number)))))
             (and sha (magit-commit-p sha)
                  (not (equal sha (plist-get side :diff))))))))

(defun ygg-git-compare--pr-landed (&rest _)
  "Draw again every compare whose pull request's commit has come or changed."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'ygg-git-compare--a-spec buffer))
      (with-current-buffer buffer
        (condition-case failure
            (let ((default-directory (or ygg-git-compare--root default-directory)))
              (when (or (ygg-git-compare--side-outdated-p ygg-git-compare--a-spec
                                                          ygg-git-compare--a)
                        (ygg-git-compare--side-outdated-p ygg-git-compare--b-spec
                                                          ygg-git-compare--b))
                (ygg-git-compare--redraw)))
          (user-error (message "%s" (error-message-string failure))))))))

(defun ygg-git-compare-fetch-pr (number &optional remote)
  "Pull request NUMBER's head commit, once REMOTE's has been fetched without
a branch for it; nil until then, the fetch running in the background."
  (let* ((remote (or remote (ygg-git-compare--remote)
                     (user-error "No remote to fetch from")))
         (key (ygg-git-compare--head-key remote number))
         (dir default-directory))
    (when-let* ((sha (ygg-git-compare--cache-value key))
                ((not (magit-commit-p sha))))
      (ygg-git-compare--cache-drop key))
    (let ((sha (ygg-git-compare--cached
                key ygg-git-compare-pr-ttl
                (lambda (done)
                  (let ((default-directory dir))
                    (ygg-git-compare--fetch-ref remote (format "pull/%s/head" number) done)))
                #'ygg-git-compare--pr-landed)))
      (and sha (magit-commit-p sha) sha))))

(defun ygg-git-compare--pending-note (number remote)
  (let ((entry (ygg-git-compare--cache-entry
                (ygg-git-compare--head-key (or remote (ygg-git-compare--remote)) number))))
    (if-let* ((err (plist-get entry :error)))
        (format "PR #%s not fetched: %s" number err)
      (format "fetching PR #%s…" number))))

(defun ygg-git-compare--age (seconds)
  (cond ((< seconds 120) (format "%ds" seconds))
        ((< seconds 7200) (format "%dm" (/ seconds 60)))
        ((< seconds 172800) (format "%dh" (/ seconds 3600)))
        (t (format "%dd" (/ seconds 86400)))))

(defun ygg-git-compare--head-note (number remote)
  "What the header says of a pull request's head when it is older than
`ygg-git-compare-pr-ttl', else nil."
  (let* ((entry (ygg-git-compare--cache-entry
                 (ygg-git-compare--head-key (or remote (ygg-git-compare--remote)) number)))
         (time (plist-get entry :time)))
    (when (and (numberp time) (> (- (float-time) time) ygg-git-compare-pr-ttl))
      (format "head as of %s; %s" (ygg-git-compare--age (truncate (- (float-time) time)))
              (if (plist-get entry :failed)
                  (format "not refreshed: %s" (plist-get entry :error))
                "refreshing…")))))

(defun ygg-git-compare--dirty-p (dir)
  (let ((default-directory dir))
    (not (string-empty-p (ygg-git-compare--git "--no-optional-locks" "status"
                                               "--porcelain")))))

(defun ygg-git-compare-resolve (spec)
  "SPEC as the commits it stands for, without touching any checkout or ref.
A plist: :label, :diff the commit diffs are taken from, :log the commit
history is read from, and for a worktree :dir, its checkout, with
:uncommitted when it has changes over its HEAD, untracked files included.
A pull request not fetched yet has no :diff, only :pending, what the
header says of it; it is fetched in the background and drawn when here."
  (pcase spec
    (`(worktree . ,dir)
     (let* ((default-directory (file-name-as-directory dir))
            (head (ygg-git-compare--commit "HEAD"))
            (branch (magit-get-current-branch)))
       (list :label (format "%s [%s]"
                            (file-name-nondirectory (directory-file-name dir))
                            (or branch (magit-rev-abbrev head)))
             :diff head
             :log head
             :dir default-directory
             :uncommitted (ygg-git-compare--dirty-p default-directory))))
    (`(pr . ,pr)
     (let ((sha (or (plist-get pr :sha)
                    (ygg-git-compare-fetch-pr (plist-get pr :number)
                                              (plist-get pr :remote)))))
       (list :label (format "#%s %s" (plist-get pr :number)
                            (or (plist-get pr :head) ""))
             :diff sha :log sha
             :note (when (and sha (not (plist-get pr :sha)))
                     (ygg-git-compare--head-note (plist-get pr :number)
                                                 (plist-get pr :remote)))
             :pending (unless sha
                        (ygg-git-compare--pending-note (plist-get pr :number)
                                                       (plist-get pr :remote))))))
    (`(rev . ,rev)
     (let ((sha (or (ignore-errors (ygg-git-compare--commit rev))
                    (user-error "No commit called %s" rev))))
       (list :label rev :diff sha :log sha)))
    (_ (error "Not a side: %S" spec))))

;;; Picking sides

(defun ygg-git-compare--announce (&rest _)
  (run-hooks 'ygg-git-compare-candidates-refreshed-hook))

(defun ygg-git-compare--pulls (repo)
  "The open pull requests gh last listed, or nil; gh is asked again in the
background when they are old.  REPO is a (FORGE HOST PATH), or nil when
it is not known."
  (let ((dir default-directory))
    (when (executable-find "gh")
      (ygg-git-compare--cached
       (list 'pulls repo) ygg-git-compare-list-ttl
       (lambda (done)
         (let ((default-directory dir))
           (ygg-git-compare--forge-async
            "gh" (append '("pr" "list" "--json" "number,title,headRefName,baseRefName")
                         (when (eq (car repo) 'github)
                           (list "--repo" (concat (nth 1 repo) "/" (nth 2 repo)))))
            (lambda (status text err)
              (ygg-git-compare--answer done "gh" status text err #'ygg-git-compare--json)))))
       #'ygg-git-compare--announce))))

(defun ygg-git-compare--group (label group &optional note)
  (propertize label 'ygg-git-compare-group group 'ygg-git-compare-note note))

(defun ygg-git-compare-candidates (&optional with-prs)
  "Every side this repository offers, as (LABEL . SPEC), grouped by kind.
WITH-PRS adds the open pull requests gh last listed and asks it again in
the background."
  (ygg-git-compare--with-git-cache
   (let ((here (magit-toplevel))
         (cands nil))
     (cl-flet ((add (label group spec &optional note)
                 (while (assoc label cands) (setq label (concat label "'")))
                 (push (cons (ygg-git-compare--group label group note) spec) cands)))
       (pcase-dolist (`(,path ,commit ,branch ,bare ,_detached ,_locked ,prunable)
                      (magit-list-worktrees))
         (unless (or bare prunable)
           (add (format "%s [%s]" (file-name-nondirectory (directory-file-name path))
			(or branch (magit-rev-abbrev commit)))
		"Worktrees" (cons 'worktree (file-name-as-directory path))
		(concat (abbreviate-file-name path)
			(when (and here (file-equal-p path here)) "  (here)")))))
       (dolist (b (magit-list-local-branch-names))
         (add b "Branches" (cons 'rev b)))
       (dolist (b (magit-list-remote-branch-names))
         (unless (string-suffix-p "/HEAD" b)
           (add b "Remote branches" (cons 'rev b))))
       (let* ((repo (and with-prs (ignore-errors (ygg-git-compare--forge-repo))))
              (pulls (and with-prs (not (eq (car repo) 'gitlab))
                          (progn (ygg-git-compare--refine-pr-remote)
                                 (ygg-git-compare--pulls repo))))
              (remote (and pulls (ygg-git-compare--pr-remote))))
         (dolist (pr pulls)
           (add (format "#%s %s" (plist-get pr :number) (plist-get pr :title))
		"Pull requests"
		(cons 'pr (list :number (plist-get pr :number)
				:title (plist-get pr :title)
				:head (plist-get pr :headRefName)
				:base (plist-get pr :baseRefName)
				:remote remote))
		(format "%s → %s" (plist-get pr :headRefName)
			(plist-get pr :baseRefName)))))
       (dolist (line (magit-git-lines "log" "--format=%h %s" "-n" "30"))
         (add line "Recent commits" (cons 'rev (car (split-string line " "))))))
     (nreverse cands))))

(defun ygg-git-compare--label (spec cands)
  "The label of SPEC among CANDS, or a string that names it when it is not one."
  (or (car (seq-find (lambda (c) (equal (cdr c) spec)) cands))
      (pcase spec
        (`(rev . ,rev) rev)
        (`(worktree . ,dir) dir))))

(defun ygg-git-compare--here-spec ()
  (cons 'worktree (file-name-as-directory
                   (or (magit-toplevel) (user-error "Not in a git repository")))))

(defun ygg-git-compare-default-a (b)
  "The side A defaults to when B is picked: a pull request's base, HEAD for
this worktree when it has uncommitted changes, else the repository's
default branch."
  (ygg-git-compare--with-git-cache
   (cond ((eq (car b) 'pr)
          (cons 'rev (ygg-git-compare--pr-base (plist-get (cdr b) :base)
                                               (plist-get (cdr b) :remote))))
         ((and (eq (car b) 'worktree)
               (file-equal-p (cdr b) (cdr (ygg-git-compare--here-spec)))
               (ygg-git-compare--dirty-p (cdr b)))
          '(rev . "HEAD"))
         (t (cons 'rev (ygg-git-compare--default-branch))))))

(defun ygg-git-compare-table (cands category)
  "A completion table of CANDS, (LABEL . VALUE) in the order given, or a
function answering them, of CATEGORY; each LABEL's group and note are read
from its text properties."
  (lambda (str pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . ,category)
          (group-function
           . ,(lambda (cand transform)
                (if transform cand
                  (get-text-property 0 'ygg-git-compare-group cand))))
          (annotation-function
           . ,(lambda (cand)
                (when-let* ((note (get-text-property 0 'ygg-git-compare-note cand)))
                  (concat "  " (propertize note 'face 'completions-annotations)))))
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action (if (functionp cands) (funcall cands) cands)
                            str pred))))

(defun ygg-git-compare--nudge (table)
  "Have vertico read TABLE again when it is what the open minibuffer completes."
  (when-let* ((window (active-minibuffer-window))
              ((fboundp 'vertico--exhibit)))
    (with-current-buffer (window-buffer window)
      (when (and (bound-and-true-p vertico--input)
                 (eq minibuffer-completion-table table))
        (setq vertico--input nil)
        (vertico--exhibit)))))

(defconst ygg-git-compare--refreshing-suffix
  (propertize " (refreshing PRs…)" 'ygg-git-compare-refreshing t))

(defun ygg-git-compare--clear-refreshing (table)
  "Take the \"refreshing\" words off the open minibuffer's prompt when it is
what completes TABLE."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (when (eq minibuffer-completion-table table)
        (let ((inhibit-read-only t)
              (start (text-property-any (point-min) (point-max)
                                        'ygg-git-compare-refreshing t)))
          (when start
            (delete-region start (or (next-single-property-change
                                      start 'ygg-git-compare-refreshing)
                                     (point-max)))))))))

(defvar ygg-git-compare--last-candidates nil
  "What the side last read was chosen among.")

(defun ygg-git-compare--read (prompt cands default &optional known)
  "Read a side with PROMPT among CANDS, a list or a function answering one,
DEFAULT the spec RET takes.  KNOWN is what CANDS answers now when that
has just been worked out.  A function is called again when pull requests
land while the prompt is open."
  (let* ((dir default-directory)
         (current (or known (if (functionp cands) (funcall cands) cands)))
         (default-label (ygg-git-compare--label default current))
         (table (ygg-git-compare-table (lambda () current) 'ygg-review-side))
         (refresh (lambda ()
                    (when (functionp cands)
                      (setq current (let ((default-directory dir)) (funcall cands)))
                      (ygg-git-compare--nudge table)
                      (unless (let ((default-directory dir))
                                (ygg-git-compare--refreshing-p 'pulls 'mrs))
                        (ygg-git-compare--clear-refreshing table)))))
         (choice nil))
    (add-hook 'ygg-git-compare-candidates-refreshed-hook refresh)
    (unwind-protect
        (setq choice
              (completing-read
               (format-prompt (concat prompt
                                      (and (ygg-git-compare--with-git-cache
                                             (ygg-git-compare--refreshing-p 'pulls 'mrs))
                                           ygg-git-compare--refreshing-suffix))
                              default-label)
               table nil nil nil nil default-label))
      (remove-hook 'ygg-git-compare-candidates-refreshed-hook refresh)
      (setq ygg-git-compare--last-candidates current))
    (or (cdr (assoc choice current))
        (and (equal choice default-label) default)
        (cons 'rev choice))))

(defun ygg-git-compare--read-sides ()
  "Read side B, then side A defaulting to what B is compared against."
  (let* ((cands (apply-partially #'ygg-git-compare-candidates t))
         (b (ygg-git-compare--read "Compare (B)" cands (ygg-git-compare--here-spec)))
         (known ygg-git-compare--last-candidates)
         (a (ygg-git-compare--read (format "Against (A) for %s"
                                           (ygg-git-compare--label b known))
                                   cands (ygg-git-compare-default-a b) known)))
    (list a b)))

;;; The two panes

(defconst ygg-git-compare--quiet-args
  '("--no-optional-locks" "-c" "diff.autoRefreshIndex=false")
  "Git's arguments that leave a worktree's index unwritten, stat cache and all.")

(defun ygg-git-compare--global-args ()
  "Magit's global git arguments, kept from refreshing a worktree's index."
  (append ygg-git-compare--quiet-args (default-value 'magit-git-global-arguments)))

(defun ygg-git-compare--planned (a b dots root)
  "How sides A and B are diffed with DOTS, as a plist: :work the side whose
working tree stands in for its commit, :reverse when that is A, :dir git
runs in, its checkout or ROOT, :range magit diffs there, and :untracked
the files it adds that git does not ignore.  B's working tree is taken
when it has uncommitted changes, else A's when DOTS is \"..\": A...B
leaves A's tip out, and with both dirty A stands at its HEAD."
  (let* ((work (cond ((plist-get b :uncommitted) b)
                     ((and (plist-get a :uncommitted) (equal dots "..")) a)))
         (dir (or (plist-get work :dir) root))
         (default-directory dir)
         (magit-git-global-arguments (ygg-git-compare--global-args)))
    (list :work work
          :reverse (and work (eq work a))
          :dir dir
          :range (cond ((not work)
                        (concat (plist-get a :diff) dots (plist-get b :diff)))
                       ((eq work a) (plist-get b :diff))
                       ((equal dots "...")
                        (or (magit-git-string "merge-base" (plist-get a :diff)
                                              (plist-get b :diff))
                            (user-error "%s and %s share no history"
                                        (plist-get a :label) (plist-get b :label))))
                       (t (plist-get a :diff)))
          :untracked (and work (magit-git-items "ls-files" "-z" "--others"
                                                "--exclude-standard")))))

(defun ygg-git-compare-agent-path (dir)
  "DIR as an agent running where the repository lives reads it: over TRAMP
without the method and host, which name nothing on the far side."
  (if (file-remote-p dir) (file-local-name dir) (abbreviate-file-name dir)))

(defun ygg-git-compare--range ()
  "The compared range for a reader: a side diffed with what it has not
committed is named by its checkout."
  (cl-flet ((end (side)
              (if (eq side (plist-get ygg-git-compare--plan :work))
                  (ygg-git-compare-agent-path (plist-get side :dir))
                (plist-get side :diff))))
    (concat (end ygg-git-compare--a) ygg-git-compare--dots (end ygg-git-compare--b))))

(defun ygg-git-compare--diff-args (plan)
  "What git diff takes for PLAN's tracked changes, before any option of magit's."
  (cons (plist-get plan :range) (and (plist-get plan :reverse) '("-R"))))

(defun ygg-git-compare--large-p (plan)
  "Whether PLAN changes more lines than a status buffer would wash unasked."
  (let* ((default-directory (plist-get plan :dir))
         (magit-git-global-arguments (ygg-git-compare--global-args))
         (counts (mapcar #'string-to-number
                         (split-string (or (magit-git-string "diff" "--shortstat"
                                                             (plist-get plan :range))
                                           "")
                                       "[^0-9]+" t))))
    (> (apply #'+ (cdr counts))
       (if (boundp 'ygg-magit-diff-line-limit) ygg-magit-diff-line-limit 2000))))

(defun ygg-git-compare--args (plan)
  "Magit's diff arguments for PLAN with its diffstat; only the diffstat when
PLAN is too large to wash, since --no-patch cancels what comes before it."
  (let ((args (append (cl-set-difference (car (magit-diff-arguments 'magit-diff-mode))
                                         '("--stat" "--numstat" "--no-patch" "-R")
                                         :test #'equal)
                      (cdr (ygg-git-compare--diff-args plan)))))
    (if (ygg-git-compare--large-p plan)
        (append '("--no-patch" "--numstat" "--stat") args)
      (cons "--stat" args))))

(defun ygg-git-compare--insert-untracked ()
  "The files the plan's worktree adds untracked, each a file section to follow."
  (when-let* ((files (plist-get ygg-git-compare--plan :untracked)))
    (magit-insert-section (ygg-git-compare-untracked nil)
      (magit-insert-heading (length files) "Untracked files")
      (dolist (file files)
        (if (ygg-git-compare--nested-p file)
            (magit-insert-section (ygg-git-compare-nested file)
              (insert (propertize file 'font-lock-face 'magit-filename)
                      (propertize "  nested repository, not diffed" 'font-lock-face 'shadow)
                      "\n"))
          (magit-insert-section (file file)
            (insert (propertize file 'font-lock-face 'magit-filename) "\n"))))
      (insert "\n"))))

(defun ygg-git-compare--nested-p (file)
  "Whether untracked FILE is a repository of its own, listed as a directory."
  (string-suffix-p "/" file))

(defun ygg-git-compare--sections-hook ()
  (append (default-value 'magit-diff-sections-hook)
          (list #'ygg-git-compare--insert-untracked)))

(defun ygg-git-compare--display-in (window)
  (lambda (buffer) (set-window-buffer window buffer) window))

(defmacro ygg-git-compare--display (window &rest body)
  "Run BODY with magit showing its buffer in WINDOW, or nowhere when nil.
Nothing is selected and no window layout is remembered for magit's q."
  (declare (indent 1))
  `(let ((magit-display-buffer-noselect t)
         (magit-pre-display-buffer-hook nil)
         (magit-post-display-buffer-hook nil)
         (magit-display-buffer-function
          (if (window-live-p ,window) (ygg-git-compare--display-in ,window) #'ignore)))
     ,@body))

(defun ygg-git-compare--side-label (side)
  (concat (plist-get side :label)
          (cond ((eq side (plist-get ygg-git-compare--plan :work))
                 " + uncommitted (incl. untracked)")
                ((plist-get side :uncommitted) " (uncommitted not shown)"))))

(defun ygg-git-compare--header ()
  (magit-set-header-line-format
   (concat "A: " (ygg-git-compare--side-label ygg-git-compare--a)
           " ↔ B: " (ygg-git-compare--side-label ygg-git-compare--b)
           (if (equal ygg-git-compare--dots "...") "  A...B" "  A..B")
           (when-let* ((note (or ygg-git-compare--note
                                 (plist-get ygg-git-compare--a :pending)
                                 (plist-get ygg-git-compare--b :pending)
                                 (plist-get ygg-git-compare--a :note)
                                 (plist-get ygg-git-compare--b :note))))
             (concat "  " note)))))

(defun ygg-git-compare--fold ()
  "Fold every file to its heading, the diffstat and untracked files left open,
point on the first file."
  (magit-section-show-level-1-all)
  (let* ((top (oref magit-root-section children))
         (open (seq-filter (lambda (s) (memq (oref s type)
                                             '(diffstat ygg-git-compare-untracked)))
                           top)))
    (mapc #'magit-section-show open)
    (goto-char (if-let* ((file (seq-some (lambda (s) (car (oref s children))) open)))
                   (oref file start)
                 (point-min)))))

(defun ygg-git-compare--settled (a b)
  "Sides A and B, one still being fetched standing where the other does."
  (let ((stand (or (plist-get a :diff) (plist-get b :diff) (ygg-git-compare--commit "HEAD"))))
    (mapcar (lambda (side)
              (if (plist-get side :diff) side (append (list :diff stand :log stand) side)))
            (list a b))))

(defun ygg-git-compare--redraw (&optional a-spec b-spec dots)
  "Resolve sides A-SPEC and B-SPEC afresh, worktrees as they stand, and diff
them here with DOTS; each nil keeps the compare's own.  When they cannot
be diffed, the compare stays as it was."
  (when (or a-spec b-spec) (setq ygg-git-compare--note nil))
  (pcase-let* ((a-spec (or a-spec ygg-git-compare--a-spec))
               (b-spec (or b-spec ygg-git-compare--b-spec))
               (dots (or dots ygg-git-compare--dots))
               (`(,a ,b) (let ((default-directory ygg-git-compare--root))
                           (ygg-git-compare--settled (ygg-git-compare-resolve a-spec)
                                                     (ygg-git-compare-resolve b-spec))))
               (plan (ygg-git-compare--planned a b dots ygg-git-compare--root))
               (args (ygg-git-compare--args plan)))
    (setq ygg-git-compare--a-spec a-spec
          ygg-git-compare--b-spec b-spec
          ygg-git-compare--dots dots
          ygg-git-compare--a a
          ygg-git-compare--b b
          ygg-git-compare--plan plan
          ygg-git-compare--shown nil
          default-directory (plist-get plan :dir)
          magit-buffer-diff-range (plist-get plan :range)
          magit-buffer-diff-args args))
  (magit-refresh-buffer)
  (ygg-git-compare--fold)
  (ygg-git-compare--follow (current-buffer)))

(defun ygg-git-compare--file-buffer-name ()
  (format "magit-diff: compare file of %s" (buffer-name)))

(defun ygg-git-compare--untracked-pair (file reverse)
  "FILE added from nothing, or the reverse when REVERSE, for git diff --no-index."
  (if reverse (list file "/dev/null") (list "/dev/null" file)))

(defun ygg-git-compare--oversized-p (file)
  (> (or (file-attribute-size (file-attributes (expand-file-name file))) 0)
     ygg-git-compare-untracked-max-size))

(defun ygg-git-compare--renamed-from (file)
  "What FILE was called on the old side when the compare renames or copies
it, else nil.  Read from its diff, or from git when only the diffstat shows."
  (let ((section (seq-find (lambda (s) (and (eq (oref s type) 'file)
                                            (equal (oref s value) file)))
                           (oref magit-root-section children))))
    (if section
        (oref section source)
      (let ((items (apply #'magit-git-items "diff" "-z" "--name-status" "-M"
                          (ygg-git-compare--diff-args ygg-git-compare--plan)))
            found)
        (while (and items (not found))
          (let ((status (pop items)))
            (if (string-match-p "\\`[RC]" status)
                (let ((old (pop items)) (new (pop items)))
                  (when (equal new file) (setq found old)))
              (pop items))))
        found))))

(defun ygg-git-compare--show-file (file)
  "Show FILE's diff alone in the right pane, from its old name when renamed;
an untracked one against nothing."
  (let* ((list (current-buffer))
         (range magit-buffer-diff-range)
         (untracked (member file (plist-get ygg-git-compare--plan :untracked)))
         (source (and (not untracked) (ygg-git-compare--renamed-from file)))
         (args (cl-set-difference magit-buffer-diff-args
                                  '("--stat" "--numstat" "--no-patch")
                                  :test #'equal))
         (window ygg-git-compare--file-window))
    (when untracked
      (setq args (remove "-R" args))
      (when (ygg-git-compare--oversized-p file)
        (setq args (append '("--no-patch" "--numstat" "--stat") args))))
    (setq ygg-git-compare--shown (cons range file))
    (with-current-buffer
        (ygg-git-compare--display window
          (magit-setup-buffer #'magit-diff-mode nil
            :buffer (ygg-git-compare--file-buffer-name)
            :directory default-directory
            (magit-buffer-diff-range (and (not untracked) range))
            (magit-buffer-diff-typearg (and untracked "--no-index"))
            (magit-buffer-diff-type 'committed)
            (magit-buffer-diff-args args)
            (magit-buffer-diff-files
             (if untracked
                 (ygg-git-compare--untracked-pair
                  file (plist-get ygg-git-compare--plan :reverse))
               (if source (list source file) (list file))))
            (magit-buffer-diff-files-suspended nil)
            (magit-git-global-arguments (ygg-git-compare--global-args))))
      (setq magit-buffer-locked-p t
            ygg-git-compare--list-buffer list)
      (ygg-git-compare-mode 1)
      (current-buffer))))

(defun ygg-git-compare--follow (buffer)
  "Make BUFFER's right pane show the file at point, unless it already does."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ygg-git-compare--timer nil)
      (when-let* ((file (magit-file-at-point))
                  ((window-live-p ygg-git-compare--file-window))
                  ((not (equal ygg-git-compare--shown
                               (cons magit-buffer-diff-range file)))))
        (ygg-git-compare--show-file file)))))

(defun ygg-git-compare--schedule ()
  (when ygg-git-compare--timer (cancel-timer ygg-git-compare--timer))
  (setq ygg-git-compare--timer
        (run-with-idle-timer ygg-git-compare-follow-delay nil
                             #'ygg-git-compare--follow (current-buffer))))

;;; What magit lacks

(defun ygg-git-compare--list ()
  "The diff of every file this command acts on, from either pane."
  (or (and ygg-git-compare--a-spec (current-buffer))
      (and (buffer-live-p ygg-git-compare--list-buffer) ygg-git-compare--list-buffer)
      (user-error "Not in a compare")))

(defun ygg-git-compare-refresh ()
  "Resolve both sides again, a worktree's changes as they stand now."
  (interactive)
  (with-current-buffer (ygg-git-compare--list) (ygg-git-compare--redraw)))

(defun ygg-git-compare-swap ()
  "Swap sides A and B."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (ygg-git-compare--redraw ygg-git-compare--b-spec ygg-git-compare--a-spec)))

(defun ygg-git-compare-switch-base ()
  "Pick another side A, the base B is compared against."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (let ((cands (apply-partially #'ygg-git-compare-candidates t)))
      (ygg-git-compare--redraw
       (ygg-git-compare--read (format "Compare %s against (A)"
                                      (ygg-git-compare--label ygg-git-compare--b-spec
                                                              (funcall cands)))
                              cands (ygg-git-compare-default-a ygg-git-compare--b-spec))))))

(defun ygg-git-compare-toggle-dots ()
  "Diff A...B, from where the sides parted, or A..B, one against the other."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (ygg-git-compare--redraw nil nil (if (equal ygg-git-compare--dots "...") ".." "..."))))

(defun ygg-git-compare-log ()
  "Show the commits only in A and only in B, marked < and >, in the right pane."
  (interactive)
  (with-current-buffer (ygg-git-compare--list)
    (let ((window ygg-git-compare--file-window)
          (list (current-buffer)))
      (setq ygg-git-compare--shown nil)
      (with-current-buffer
          (ygg-git-compare--display window
            (magit-log-setup-buffer
             (list (concat (plist-get ygg-git-compare--a :log) "..."
                           (plist-get ygg-git-compare--b :log)))
             (cons "--left-right" (car (magit-log-arguments 'magit-log-mode)))
             nil t))
        (setq ygg-git-compare--list-buffer list)
        (ygg-git-compare-mode 1)
        (when (window-live-p window) (select-window window))))))

(defun ygg-git-compare--close (list)
  "Kill LIST and the panes it opened; return the windows from before it."
  (let ((config (buffer-local-value 'ygg-git-compare--window-config list))
        (timer (buffer-local-value 'ygg-git-compare--timer list)))
    (when timer (cancel-timer timer))
    (dolist (b (buffer-list))
      (when (or (eq b list)
                (eq (buffer-local-value 'ygg-git-compare--list-buffer b) list))
        (kill-buffer b)))
    config))

(defun ygg-git-compare-quit ()
  "Leave the compare and bring back the windows it replaced; its review
comments are kept for the next time it is opened."
  (interactive)
  (let ((list (ygg-git-compare--list)))
    (when-let* ((config (ygg-git-compare--close list)))
      (set-window-configuration config))))

;;; Review

(defcustom ygg-git-compare-review-instructions
  "Review the change from A to B below as a strict reviewer.  First read
the \"Review guidelines\" or \"Code Review Rules\" sections of AGENTS.md
and AGENTS.override.md where they exist.  Deliver your findings by
calling the MCP tool review_submit with the branch named below and
comments: [{file, line, side \"new\"|\"old\", start_line?, level?, type?,
priority 0-3, confidence 0-1, title, text}], a short imperative title and,
in text, what is wrong and a concrete fix; add one comment of level
\"review\" whose correctness is \"patch is correct\" or \"patch is
incorrect\", with its confidence and why.  Then summarise them.  My
comments, each anchored to a file and line with the lines around it, are
instructions, not suggestions: work out the change each asks for, and ask
me where one leaves a choice open.  Do not change any files."
  "What an agent is asked to do with a compare it is sent."
  :type 'string)

(defcustom ygg-git-compare-review-max-chars 60000
  "How much of the compared log and diff a review carries before it is cut."
  :type 'natnum)

(defconst ygg-git-compare-review-preset "review"
  "The preset a new session that plans from a review runs under.")

(defun ygg-git-compare--capped (text)
  (if (> (length text) ygg-git-compare-review-max-chars)
      (concat (substring text 0 ygg-git-compare-review-max-chars)
              (format "\n… %d more characters left out"
                      (- (length text) ygg-git-compare-review-max-chars)))
    text))

(defun ygg-git-compare--range-label ()
  (format "A: %s ↔ B: %s  %s" (ygg-git-compare--side-label ygg-git-compare--a)
          (ygg-git-compare--side-label ygg-git-compare--b) (ygg-git-compare--range)))

(defun ygg-git-compare--agent-comments ()
  "The checked comments held for an agent, oldest first."
  (and ygg-git-compare--comments
       (seq-remove #'ygg-git-compare--forge-p (ygg-git-compare-comments-list))))

(defun ygg-git-compare-review-prompt ()
  "This compare's comments for an agent, its commits and its diff, as one
message.  A comment made on another range than the one sent says which."
  (concat
   ygg-git-compare-review-instructions
   (when-let* ((comments (ygg-git-compare--agent-comments)))
     (concat "\n\n<review-comments>\n"
             (mapconcat (lambda (c)
                          (ygg-git-compare--for-prompt c (ygg-git-compare--range-label)))
                        comments "\n\n")
             "\n</review-comments>"))
   "\n\n" (ygg-git-compare-compare-block)))

(defun ygg-git-compare-compare-block ()
  "The compared range, its commits and its diff, as an agent reads them."
  (concat
   "<compare>\nrepository " (ygg-git-compare-agent-path default-directory)
   (when-let* ((branch (ignore-errors (ygg-git-compare--b-branch))))
     (concat "\nbranch " branch))
   "\n" (ygg-git-compare--range-label) "\n\n"
   (ygg-git-compare--capped
    (concat (ygg-git-compare--git "log" "--no-color" "--format=%h %an: %s" "-n" "200"
                                  (concat (plist-get ygg-git-compare--a :log) ".."
                                          (plist-get ygg-git-compare--b :log)))
            "\n\n"
            (apply #'ygg-git-compare--git
                   (append ygg-git-compare--quiet-args
                           (list "diff" "--no-ext-diff" "--no-color")
                           (ygg-git-compare--diff-args ygg-git-compare--plan)))
            (mapconcat (lambda (file)
                         (concat "\n" (ygg-git-compare--untracked-patch
                                       file (plist-get ygg-git-compare--plan :reverse))))
                       (plist-get ygg-git-compare--plan :untracked))))
   "\n</compare>"))

(defun ygg-git-compare--untracked-patch (file reverse)
  "Untracked FILE as a patch adding it, or removing it when REVERSE; a note
in its place when it is a nested repository or too large to send."
  (cond
   ((ygg-git-compare--nested-p file)
    (format "untracked %s: a nested repository, not diffed" file))
   ((ygg-git-compare--oversized-p file)
    (format "untracked %s: %d bytes, left out"
            file (file-attribute-size (file-attributes (expand-file-name file)))))
   (t
    (with-temp-buffer
      (apply #'process-file "git" nil '(t nil) nil
             "diff" "--no-index" "--no-ext-diff" "--no-color" "--"
             (ygg-git-compare--untracked-pair file reverse))
      (string-trim-right (buffer-string))))))

(defun ygg-git-compare--reviewers ()
  "Labels to what a review goes to: a new session under the review preset,
a live session, or a new agent as (new . NAME); each label notes its
group and a live session's state."
  (let (labels)
    (cl-flet ((add (label group note value)
                (while (assoc label labels) (setq label (concat label "'")))
                (push (cons (ygg-git-compare--group label group note) value) labels)))
      (add (format "new: review plan (preset %s)" ygg-git-compare-review-preset)
           "New session" "plans from the review" 'review)
      (dolist (s (aob-live-sessions))
        (add (aob-session-name s) "Live sessions"
             (format "%s · %s" (aob-session-state s) (or (aob-session-cwd s) "?"))
             s))
      (dolist (a aob-acp-agents)
        (add (concat "new: " (car a)) "New agent" nil (cons 'new (car a)))))
    (nreverse labels)))

(defun ygg-git-compare--spawn-review (root text)
  "A new session in ROOT under the review preset, TEXT its first turn.
Only the preset is expanded, never a name TEXT happens to mention."
  (unless (and (bound-and-true-p aob-compose-spawn-function)
               (fboundp 'ygg-aob--expand-presets))
    (user-error "The review preset needs layer-aob"))
  (let* ((aob-compose--dir root)
         (ygg-aob--draft-tree nil)
         (default-directory root)
         (at (concat "@" ygg-git-compare-review-preset))
         (preset (ygg-aob--expand-presets at)))
    (unless (and preset (string-search (format "<preset name=\"%s\">"
                                               ygg-git-compare-review-preset)
                                       preset))
      (user-error "No %s preset here" ygg-git-compare-review-preset))
    (or (let ((aob-prompt-typed t))
          (funcall aob-compose-spawn-function
                   (concat at " " text (substring preset (length at)))))
        (user-error "Could not start a review session"))))

(defun ygg-git-compare-send-to-reviewer (root text)
  "Send TEXT to a reviewer picked among `ygg-git-compare--reviewers', one
started in ROOT when new; answer its session."
  (let* ((reviewers (ygg-git-compare--reviewers))
         (choice (cdr (assoc (completing-read
                              "Review by: "
                              (ygg-git-compare-table reviewers 'ygg-review-reviewer)
                              nil t)
                             reviewers)))
         (session
          (pcase choice
            ('nil (user-error "No reviewer picked"))
            ('review (ygg-git-compare--spawn-review root text))
            (`(new . ,agent)
             (let ((aob-acp-start-dir root))
               (or (aob-acp-spawn agent) (user-error "Could not start %s" agent))))
            (_ choice))))
    (unless (eq choice 'review)
      (let ((aob-prompt-typed t))
        (aob-prompt session text)))
    session))

(defun ygg-git-compare-review ()
  "Send the review comments held for an agent and the compared range to one."
  (interactive)
  (require 'aob)
  (require 'aob-acp)
  (with-current-buffer (ygg-git-compare--list)
    (let* ((sent (ygg-git-compare--agent-comments))
           (session (ygg-git-compare-send-to-reviewer default-directory
                                                      (ygg-git-compare-review-prompt))))
      (when sent
        (ygg-git-compare-comments-drop (mapcar (lambda (c) (plist-get c :id)) sent)))
      (aob-trace session))))

;;; Posting to the pull request

(defun ygg-git-compare--forge-run (program &rest args)
  "What PROGRAM ARGS prints, run on this machine; a user error naming its
complaint on failure."
  (with-temp-buffer
    (let* ((default-directory (if (file-remote-p default-directory)
                                  temporary-file-directory
                                default-directory))
           (err (make-temp-file "ygg-git-compare-forge-")))
      (unwind-protect
          (let ((status (condition-case failure
                            (apply #'call-process program nil (list t err) nil args)
                          (file-error (user-error "%s did not start: %s" program
                                                  (error-message-string failure))))))
            (unless (eql status 0)
              (user-error "%s %s: %s" program (car args)
                          (string-trim (concat (with-temp-buffer
                                                 (insert-file-contents err)
                                                 (buffer-string))
                                               "\n" (buffer-string))))))
        (delete-file err)))
    (buffer-string)))

(defun ygg-git-compare--forge-json (program body &rest args)
  "What PROGRAM ARGS prints, read as JSON; BODY, when non-nil, is posted
as JSON through api's --input."
  (let ((input (and body (let ((coding-system-for-write 'utf-8))
                           (make-temp-file "ygg-git-compare-body-" nil ".json"
                                           (json-serialize body))))))
    (unwind-protect
        (let ((out (string-trim
                    (apply #'ygg-git-compare--forge-run program
                           (append args
                                   (and input (list "--method" "POST"
                                                    "--header" "Content-Type: application/json"
                                                    "--input" input)))))))
          (unless (string-empty-p out)
            (ygg-git-compare--json out)))
      (when input (delete-file input)))))

(defun ygg-git-compare--lab-host ()
  (car (split-string (ygg-git-compare--url-key (or (getenv "LAB_HOST")
                                                    (bound-and-true-p ygg-lab-host)
                                                    "https://gitlab.com"))
                     "/")))

(defvar ygg-git-compare--ssh-hostnames (make-hash-table :test #'equal))

(defun ygg-git-compare--unquote (string)
  (string-trim string "[\"']+" "[\"']+"))

(defun ygg-git-compare--bare-host (value)
  (car (split-string (ygg-git-compare--url-key (ygg-git-compare--unquote value)) "/")))

(defun ygg-git-compare--host-with-port (value)
  (car (split-string (replace-regexp-in-string
                      "\\`\\(?:[a-z+]+://\\)?\\(?:[^@/]+@\\)?" ""
                      (downcase (ygg-git-compare--unquote value)))
                     "/")))

(defun ygg-git-compare--glab-global-config ()
  (seq-find #'file-exists-p
            (delq nil (list (when-let* ((dir (getenv "GLAB_CONFIG_DIR")))
                              (expand-file-name "config.yml" dir))
                            (when-let* ((dir (getenv "XDG_CONFIG_HOME")))
                              (expand-file-name "glab-cli/config.yml" dir))
                            "~/.config/glab-cli/config.yml"
                            "~/Library/Application Support/glab-cli/config.yml"))))

(defun ygg-git-compare--glab-hosts ()
  "Each host glab is configured for, as (KEY API-HOST SSH-HOST ...)."
  (let (hosts)
    (dolist (file (list (when-let* ((gitdir (magit-gitdir)))
                          (expand-file-name "glab-cli/config.yml" gitdir))
                        (ygg-git-compare--glab-global-config)))
      (when (and file (file-readable-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward "^hosts:" nil t)
            (let (current
                  (end (or (save-excursion
                             (and (re-search-forward "^[^ \n#]" nil t) (match-beginning 0)))
                           (point-max))))
              (while (re-search-forward
                      "^\\(?:    \\([^ \n]+\\): *\\(?:#.*\\)?$\\|        \\(?:api\\|ssh\\)_host: *\\([^ \n#]+\\)\\)"
                      end t)
                (if (match-string 1)
                    (let ((key (downcase (ygg-git-compare--unquote (match-string 1)))))
                      (setq current (list key (ygg-git-compare--bare-host key)))
                      (push current hosts))
                  (when current
                    (push (ygg-git-compare--bare-host (match-string 2)) (cdr current))))))))))
    (dolist (var '("GITLAB_HOST" "GL_HOST"))
      (when-let* ((value (getenv var)))
        (push (list (ygg-git-compare--host-with-port value) (ygg-git-compare--bare-host value))
              hosts)))
    (nreverse hosts)))

(defun ygg-git-compare--gh-hosts ()
  (when-let* ((file (seq-find #'file-readable-p
                              (delq nil (list (when-let* ((dir (getenv "GH_CONFIG_DIR")))
                                                (expand-file-name "hosts.yml" dir))
                                              (when-let* ((dir (getenv "XDG_CONFIG_HOME")))
                                                (expand-file-name "gh/hosts.yml" dir))
                                              "~/.config/gh/hosts.yml")))))
    (with-temp-buffer
      (insert-file-contents file)
      (let (hosts)
        (while (re-search-forward "^\\([^ #\n]+\\):" nil t)
          (push (ygg-git-compare--bare-host (match-string 1)) hosts))
        hosts))))

(defun ygg-git-compare--ssh-hostname (alias)
  "The HostName ssh resolves ALIAS to, nil when it cannot say."
  (let ((hostname (gethash alias ygg-git-compare--ssh-hostnames)))
    (unless hostname
      (setq hostname
            (or (ignore-errors
                  (let ((default-directory temporary-file-directory))
                    (with-temp-buffer
                      (and (zerop (call-process "ssh" nil t nil "-G" alias))
                           (progn (goto-char (point-min))
                                  (re-search-forward "^hostname \\(.+\\)$" nil t))
                           (downcase (match-string 1))))))
                :none))
      (puthash alias hostname ygg-git-compare--ssh-hostnames))
    (and (stringp hostname) hostname)))

(defun ygg-git-compare--configured-forge (host)
  "(FORGE . NAME) for HOST from glab's, gh's or the lab host's settings, NAME being
what glab or gh calls it; nil if none knows it."
  (if-let* ((entry (seq-find (lambda (entry) (member host entry))
                             (ygg-git-compare--glab-hosts))))
      (cons 'gitlab (car entry))
    (cond ((member host (ygg-git-compare--gh-hosts)) (cons 'github host))
          ((equal host (ygg-git-compare--lab-host)) (cons 'gitlab host)))))

(defun ygg-git-compare--named-forge (host)
  "(FORGE . HOST) when HOST's name says which forge it is."
  (cond ((string-search "gitlab" host) (cons 'gitlab host))
        ((string-search "github" host) (cons 'github host))))

(defvar ygg-git-compare--forge-repos (make-hash-table :test #'equal)
  "Each remote's repository, by checkout and URL.")

(defun ygg-git-compare--forge-repo (&optional remote)
  "REMOTE's repository as (FORGE HOST PATH), FORGE github or gitlab;
REMOTE defaults to the one pull requests are taken from."
  (let* ((remote (or remote (ygg-git-compare--pr-remote)
                     (user-error "No remote to post to")))
         (url (or (magit-get "remote" remote "url")
                  (user-error "Remote %s has no URL" remote)))
         (memo (cons (magit-toplevel) url)))
    (or (gethash memo ygg-git-compare--forge-repos)
        (let* ((key (ygg-git-compare--url-key url))
               (host (car (split-string key "/")))
               (path (substring key (min (length key) (1+ (length host)))))
               (hosts (delq nil (list host (ygg-git-compare--ssh-hostname host))))
               (forge (or (seq-some #'ygg-git-compare--configured-forge hosts)
                          (seq-some #'ygg-git-compare--named-forge hosts)
                          (user-error "%s is neither GitHub nor GitLab" host))))
          (puthash memo (list (car forge) (cdr forge) path) ygg-git-compare--forge-repos)))))

(defun ygg-git-compare--forge-pr-fetch (repo selector done)
  "Ask REPO's forge for the open pull or merge request SELECTOR names, a
number or a source branch, then call DONE with it as `--forge-pr' gives it
but for :base on GitHub, or with nil when a branch has none, or with nil
and what went wrong."
  (pcase-let ((`(,forge ,host ,path) repo)
              (by-branch (stringp selector)))
    (pcase forge
      ('github
       (ygg-git-compare--forge-async
        "gh" (list "pr" "view" (format "%s" selector) "--repo" (concat host "/" path)
                   "--json" "number,state,baseRefOid,headRefOid,baseRefName,url")
        (lambda (status text err)
          (if (and by-branch (eql status 1)
                   (string-search "no pull requests found" err))
              (funcall done nil)
            (ygg-git-compare--answer
             done "gh" status text err
             (lambda (text)
               (let ((pr (ygg-git-compare--json text)))
                 (when (or (not by-branch) (equal (plist-get pr :state) "OPEN"))
                   (list :forge 'github :host host :path path
                         :number (plist-get pr :number)
                         :head (plist-get pr :headRefOid)
                         :start (plist-get pr :baseRefOid)
                         :base-ref (plist-get pr :baseRefName)
                         :url (plist-get pr :url))))))))))
      ('gitlab
       (let* ((project (concat "projects/" (url-hexify-string path)))
              (view (lambda (iid)
                      (ygg-git-compare--forge-async
                       "glab" (list "api" "--hostname" host
                                    (format "%s/merge_requests/%s" project iid))
                       (lambda (status text err)
                         (ygg-git-compare--answer
                          done "glab" status text err
                          (lambda (text)
                            (let* ((mr (ygg-git-compare--json text))
                                   (refs (plist-get mr :diff_refs)))
                              (list :forge 'gitlab :host host :path path :number iid
                                    :head (plist-get refs :head_sha)
                                    :base (plist-get refs :base_sha)
                                    :start (plist-get refs :start_sha)
                                    :base-ref (plist-get mr :target_branch)
                                    :url (plist-get mr :web_url))))))))))
         (if (not by-branch)
             (funcall view selector)
           (ygg-git-compare--forge-async
            "glab" (list "api" "--hostname" host
                         (format "%s/merge_requests?state=opened&source_branch=%s"
                                 project (url-hexify-string selector)))
            (lambda (status text err)
              (ygg-git-compare--answer
               (lambda (iid &optional failure)
                 (cond (failure (funcall done nil failure))
                       (iid (funcall view iid))
                       (t (funcall done nil))))
               "glab" status text err
               (lambda (text) (plist-get (car (ygg-git-compare--json text)) :iid)))))))))))

(defun ygg-git-compare--pr-key (repo selector)
  (list 'pr repo selector))

(defun ygg-git-compare--with-base (pr)
  "PR with :base, the commit it parted from its target, as far as it can be
told here."
  (if (or (null pr) (plist-get pr :base))
      pr
    (append (list :base (magit-git-string "merge-base" (plist-get pr :start)
                                          (plist-get pr :head)))
            pr)))

(defun ygg-git-compare--forge-pr (repo selector &optional on-fresh)
  "The open pull or merge request of REPO, as `--forge-repo' gives it, that
SELECTOR names, a number or a source branch, as last found; nil when a
branch has none or nothing is known yet.  A plist: :forge :host :path
:number :head :base, the commit where it parted from its target, :start,
the target's tip, :base-ref and :url.  The forge is asked again in the
background when the answer is older than `ygg-git-compare-pr-ttl', and
ON-FRESH then called with (PR ERR)."
  (let ((dir default-directory))
    (ygg-git-compare--with-base
     (ygg-git-compare--cached
      (ygg-git-compare--pr-key repo selector) ygg-git-compare-pr-ttl
      (lambda (done)
        (let ((default-directory dir))
          (ygg-git-compare--forge-pr-fetch repo selector done)))
      (and on-fresh
           (lambda (pr err)
             (let ((default-directory dir))
               (funcall on-fresh (ygg-git-compare--with-base pr) err))))))))

(defun ygg-git-compare--known-pr (repo selector what)
  "The request SELECTOR names as last found; a user error saying why when
there is none, WHAT being how it is called."
  (or (ygg-git-compare--forge-pr repo selector)
      (let ((entry (ygg-git-compare--cache-entry (ygg-git-compare--pr-key repo selector))))
        (user-error "%s" (cond ((plist-get entry :error)
                                (format "Could not read %s: %s" what (plist-get entry :error)))
                               (entry (format "No open pull request for %s" what))
                               (t (format "Looking up %s; try again in a moment" what)))))))

(defun ygg-git-compare--pr-name (pr)
  (format (if (eq (plist-get pr :forge) 'gitlab) "%s MR !%s" "%s PR #%s")
          (plist-get pr :forge) (plist-get pr :number)))

(defun ygg-git-compare--b-branch ()
  "The branch side B stands for, as its remote names it."
  (pcase ygg-git-compare--b-spec
    (`(pr . ,pr) (plist-get pr :head))
    (`(worktree . ,dir) (let ((default-directory dir)) (magit-get-current-branch)))
    (`(rev . ,rev) (cond ((magit-local-branch-p rev) rev)
                         ((magit-remote-branch-p rev)
                          (cdr (magit-split-branch-name rev)))))))

(defun ygg-git-compare--this-pr ()
  "The pull or merge request this compare is the range of; a user error
saying why when it is not."
  (let* ((spec (cdr-safe ygg-git-compare--b-spec))
         (pr-spec (eq (car ygg-git-compare--b-spec) 'pr))
         (repo (ygg-git-compare--forge-repo (and pr-spec (plist-get spec :remote))))
         (pr (if pr-spec
                 (ygg-git-compare--known-pr repo (plist-get spec :number)
                                            (format "#%s" (plist-get spec :number)))
               (let ((branch (or (ygg-git-compare--b-branch)
                                 (user-error "B is not a branch, so it has no pull request"))))
                 (ygg-git-compare--known-pr repo branch branch))))
         (name (ygg-git-compare--pr-name pr)))
    (cond ((not (equal ygg-git-compare--dots "..."))
           (user-error "%s is diffed A...B; this compare is A..B" name))
          ((plist-get ygg-git-compare--plan :work)
           (user-error "This compare shows uncommitted changes %s does not have" name))
          ((not (equal (plist-get ygg-git-compare--b :diff) (plist-get pr :head)))
           (user-error "B is at %s, %s's head at %s"
                       (plist-get ygg-git-compare--b :diff) name (plist-get pr :head)))
          ((not (plist-get pr :base))
           (user-error "%s's base %s is not here; fetch it" name (plist-get pr :start)))
          ((not (equal (magit-git-string "merge-base" (plist-get ygg-git-compare--a :diff)
                                         (plist-get ygg-git-compare--b :diff))
                       (plist-get pr :base)))
           (user-error "A does not part from B where %s's base does" name)))
    pr))

(defun ygg-git-compare--gitlab-position (comment pr)
  (append (list :position_type "text"
                :base_sha (plist-get pr :base)
                :start_sha (plist-get pr :start)
                :head_sha (plist-get pr :head)
                :old_path (plist-get comment :old-path)
                :new_path (plist-get comment :new-path))
          (if (eq (plist-get comment :side) 'old)
              (list :old_line (plist-get comment :line))
            (append (list :new_line (plist-get comment :line))
                    (when-let* ((old (plist-get comment :old-line)))
                      (list :old_line old))))))

(autoload 'ygg-git-compare-visit-b "ygg-git-compare-explain" nil t)
(autoload 'ygg-git-compare-explain "ygg-git-compare-explain" nil t)
(autoload 'ygg-git-compare-mark-file-reviewed "ygg-git-compare-marks" nil t)
(autoload 'ygg-git-compare-mark-hunk-reviewed "ygg-git-compare-marks" nil t)
(autoload 'ygg-git-compare-next-unreviewed "ygg-git-compare-marks" nil t)
(autoload 'ygg-git-compare-previous-unreviewed "ygg-git-compare-marks" nil t)
(autoload 'ygg-git-compare-toggle-unreviewed "ygg-git-compare-marks" nil t)
(autoload 'ygg-git-compare-interdiff "ygg-git-compare-interdiff" nil t)
(autoload 'ygg-git-compare-export-markdown "ygg-git-compare-submit" nil t)
(autoload 'ygg-git-compare-submit "ygg-git-compare-submit" nil t)
(autoload 'ygg-git-compare-comment "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-file "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-select-lines "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-next "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-previous "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-edit "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-append "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-delete "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-copy "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comment-accept "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comments-summary "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-dispatch "ygg-git-compare-comments" nil t)
(autoload 'ygg-git-compare-comments-receive "ygg-git-compare-comments")
(declare-function ygg-git-compare-comments-list "ygg-git-compare-comments"
                  (&optional include-pending))
(declare-function ygg-git-compare-comments-drop "ygg-git-compare-comments" (ids))
(declare-function ygg-git-compare--forge-p "ygg-git-compare-comments" (comment))
(declare-function ygg-git-compare--for-prompt "ygg-git-compare-comments" (comment here))
(declare-function ygg-git-compare--load-comments "ygg-git-compare-comments" ())
(declare-function ygg-git-compare--draw-comments "ygg-git-compare-comments" ())

(defun ygg-git-compare--reveal ()
  "Open every folded section point is in."
  (let ((section (magit-current-section)))
    (while section
      (when (oref section hidden) (magit-section-show section))
      (setq section (oref section parent)))))

(defun ygg-git-compare--goto-section (pred back)
  "Go to the next section PRED holds for, or the one before with BACK."
  (let ((here (line-beginning-position))
        starts)
    (magit-map-sections (lambda (s) (when (funcall pred s) (push (oref s start) starts))))
    (setq starts (sort starts #'<))
    (goto-char (or (if back
                       (car (last (seq-filter (lambda (p) (< p here)) starts)))
                     (seq-find (lambda (p) (> p here)) starts))
                   (user-error "No %s one" (if back "previous" "next"))))
    (ygg-git-compare--reveal)))

(defun ygg-git-compare--unwashed-p ()
  "Whether this diff shows only its diffstat, too large to wash into hunks."
  (not (seq-some (lambda (s) (eq (oref s type) 'file)) (oref magit-root-section children))))

(defun ygg-git-compare--diffed-file-p (section)
  (and (magit-section-match 'file section)
       (or (eq (oref section parent) magit-root-section)
           (and (eq (oref (oref section parent) type) 'diffstat)
                (ygg-git-compare--unwashed-p)))))

(defun ygg-git-compare--hunk-across-files (list back)
  "From LIST's right pane, go to the next hunk, or the one before with BACK,
on to the following file's when the one shown has none left."
  (with-current-buffer list (ygg-git-compare--follow list))
  (let ((window (buffer-local-value 'ygg-git-compare--file-window list)))
    (unless (window-live-p window) (user-error "No right pane"))
    (select-window window)
    (condition-case nil
        (ygg-git-compare--goto-section #'ygg-git-compare--hunk-p back)
      (user-error
       (with-current-buffer list
         (ygg-git-compare--goto-section #'ygg-git-compare--diffed-file-p back)
         (ygg-git-compare--follow list))
       (select-window window)
       (goto-char (if back (point-max) (point-min)))
       (ygg-git-compare--goto-section #'ygg-git-compare--hunk-p back)))))

(defun ygg-git-compare--hunk-p (section)
  (magit-section-match 'hunk section))

(defun ygg-git-compare--goto-hunk (back)
  "Go to the next hunk, or the one before with BACK, across files."
  (let ((list (ygg-git-compare--list)))
    (if (with-current-buffer list (ygg-git-compare--unwashed-p))
        (ygg-git-compare--hunk-across-files list back)
      (ygg-git-compare--goto-section #'ygg-git-compare--hunk-p back))))

(defun ygg-git-compare-next-hunk ()
  "Go to the next hunk."
  (interactive)
  (ygg-git-compare--goto-hunk nil))

(defun ygg-git-compare-previous-hunk ()
  "Go to the hunk before."
  (interactive)
  (ygg-git-compare--goto-hunk t))

(defun ygg-git-compare-next-file ()
  "Go to the next file's diff."
  (interactive)
  (ygg-git-compare--goto-section #'ygg-git-compare--diffed-file-p nil))

(defun ygg-git-compare-previous-file ()
  "Go to the diff of the file before."
  (interactive)
  (ygg-git-compare--goto-section #'ygg-git-compare--diffed-file-p t))

(defun ygg-git-compare-search-next (&optional back)
  "Go to the next match of the last search, or the one before with BACK;
around past the end."
  (interactive)
  (let* ((regexp (or (car regexp-search-ring) (user-error "Nothing searched yet")))
         (at (save-excursion
               (if back
                   (or (re-search-backward regexp nil t)
                       (progn (goto-char (point-max)) (re-search-backward regexp nil t)))
                 (unless (eobp) (forward-char))
                 (or (and (re-search-forward regexp nil t) (match-beginning 0))
                     (progn (goto-char (point-min))
                            (and (re-search-forward regexp nil t) (match-beginning 0))))))))
    (goto-char (or at (user-error "No match for %s" regexp)))
    (ygg-git-compare--reveal)))

(defun ygg-git-compare-search-previous ()
  "Go to the match of the last search before point, around past the start."
  (interactive)
  (ygg-git-compare-search-next t))

(defvar-keymap ygg-git-compare-next-map
  "c" (cons "next hunk" #'ygg-git-compare-next-hunk)
  "f" (cons "next file" #'ygg-git-compare-next-file)
  "u" (cons "next unreviewed hunk" #'ygg-git-compare-next-unreviewed)
  "m" (cons "next comment" #'ygg-git-compare-comment-next))

(defvar-keymap ygg-git-compare-previous-map
  "c" (cons "previous hunk" #'ygg-git-compare-previous-hunk)
  "f" (cons "previous file" #'ygg-git-compare-previous-file)
  "u" (cons "previous unreviewed hunk" #'ygg-git-compare-previous-unreviewed)
  "m" (cons "previous comment" #'ygg-git-compare-comment-previous))

(defvar-keymap ygg-git-compare-delete-map
  "d" (cons "delete comment" #'ygg-git-compare-comment-delete))

(defun ygg-git-compare-read-only ()
  "Refuse to change the worktree or index from a compare."
  (interactive)
  (user-error "Read-only compare: stage, discard and apply from magit status"))

(defvar-keymap ygg-git-compare-mode-map
  "~" #'ygg-git-compare-swap
  "b" #'ygg-git-compare-switch-base
  "." #'ygg-git-compare-toggle-dots
  "#" #'ygg-git-compare-log
  "q" #'ygg-git-compare-quit
  "]" (cons "next" ygg-git-compare-next-map)
  "[" (cons "previous" ygg-git-compare-previous-map)
  "}" #'ygg-git-compare-next-file
  "{" #'ygg-git-compare-previous-file
  "C-j" #'ygg-git-compare-next-hunk
  "<remap> <magit-diff-visit-worktree-file>" #'ygg-git-compare-next-hunk
  "C-k" #'ygg-git-compare-previous-hunk
  "m" #'ygg-git-compare-comment-next
  "M" #'ygg-git-compare-comment-previous
  "/" #'isearch-forward-regexp
  "n" #'ygg-git-compare-search-next
  "N" #'ygg-git-compare-search-previous
  "c" #'ygg-git-compare-comment
  "C" #'ygg-git-compare-comment-file
  "<remap> <magit-commit-add-log>" #'ygg-git-compare-comment-file
  "v" #'ygg-git-compare-select-lines
  "V" #'ygg-git-compare-select-lines
  "x" #'ygg-git-compare-select-lines
  "i" #'ygg-git-compare-comment-edit
  "A" #'ygg-git-compare-comment-append
  "d" (cons "delete" ygg-git-compare-delete-map)
  "K" #'ygg-git-compare-comment-delete
  "y" #'ygg-git-compare-export-markdown
  "Y" #'ygg-git-compare-comment-copy
  "a" #'ygg-git-compare-comment-accept
  "s" #'ygg-git-compare-read-only
  "S" #'ygg-git-compare-read-only
  "u" #'ygg-git-compare-read-only
  "U" #'ygg-git-compare-read-only
  "r" #'ygg-git-compare-mark-file-reviewed
  "R" #'ygg-git-compare-mark-hunk-reviewed
  "I" #'ygg-git-compare-interdiff
  "e" #'ygg-git-compare-visit-b
  "'" #'ygg-git-compare-visit-b
  ";" #'ygg-git-compare-dispatch
  "?" #'ygg-git-compare-dispatch
  "@" #'ygg-git-compare-review
  "&" #'ygg-git-compare-submit
  "<remap> <magit-do-async-shell-command>" #'ygg-git-compare-submit
  "<remap> <magit-refresh>" #'ygg-git-compare-refresh)

(define-minor-mode ygg-git-compare-mode
  "A magit buffer that is part of a compare of two sides.
\\<ygg-git-compare-mode-map>
\\[ygg-git-compare-swap] swaps the sides.
\\[ygg-git-compare-switch-base] picks another base, side A.
\\[ygg-git-compare-toggle-dots] switches between A...B and A..B.
\\[ygg-git-compare-log] shows the commits only in A and only in B.
\\[ygg-git-compare-refresh] diffs worktrees as they stand now.
] c and [ c go to the next and previous hunk, ] f and [ f (or } and {)
to the next and previous file, ] u and [ u to the next and previous
unreviewed hunk, and
] m and [ m (or \\[ygg-git-compare-comment-next] and \\[ygg-git-compare-comment-previous]) to the next and previous comment.
\\[isearch-forward-regexp] searches the diff; \\[ygg-git-compare-search-next] and \\[ygg-git-compare-search-previous] repeat it forward and back.
\\[ygg-git-compare-comment] comments on the line at point, the lines selected or the file.
v, x or V select lines for a range comment; x, j and k extend it, Esc ends it.
\\[ygg-git-compare-comment-file] comments on the file at point.
On a comment, i and A edit it, d d and K delete it, Y copies it, and a
accepts one an agent proposed; off one they say so.  A comment's type
decides where it goes: those in `ygg-git-compare-agent-types' to an
agent, the rest to the pull request.  Staging, discarding and applying are
refused: a compare is read-only.
\\[ygg-git-compare-export-markdown] copies the review as markdown.
\\[ygg-git-compare-mark-file-reviewed] and \\[ygg-git-compare-mark-hunk-reviewed] mark the file or hunk reviewed.
\\[ygg-git-compare-interdiff] shows what changed since the last review.
\\[ygg-git-compare-visit-b] opens B's own file at the line, for the language server.
\\[ygg-git-compare-dispatch] comments on the review, lists, checks and sends the comments.
\\[ygg-git-compare-review] sends the comments for an agent and the compare to one.
\\[ygg-git-compare-submit] submits the comments to the pull request.
\\[ygg-git-compare-quit] leaves, bringing back the windows from before."
  :lighter " Compare"
  (if (not ygg-git-compare-mode)
      (progn (remove-function (local 'imenu-create-index-function)
                              #'ygg-git-compare--imenu-index)
             (remove-hook 'imenu-after-jump-hook #'ygg-git-compare--reveal t))
    (require 'ygg-git-compare-comments)
    (add-function :override (local 'imenu-create-index-function)
                  #'ygg-git-compare--imenu-index)
    (add-hook 'imenu-after-jump-hook #'ygg-git-compare--reveal nil t)
    (add-hook 'magit-refresh-buffer-hook #'ygg-git-compare--draw-comments nil t)
    (ygg-git-compare--draw-comments)))

(defun ygg-git-compare--imenu-comments (beg end texts)
  "The comments shown from BEG to END as (LABEL . POS), TEXTS their texts by id."
  (let (items)
    (dolist (ov (overlays-in beg end))
      (when (and (>= (overlay-start ov) beg) (< (overlay-start ov) end))
        (dolist (id (overlay-get ov 'ygg-git-compare-comments))
          (push (cons (concat "▎ " (car (split-string (or (gethash id texts) id) "\n")))
                      (overlay-start ov))
                items))))
    (sort items (lambda (a b) (< (cdr a) (cdr b))))))

(defun ygg-git-compare--imenu-index ()
  "The diffed files, their hunks and the comments under each, for imenu."
  (when-let* ((root (bound-and-true-p magit-root-section)))
    (let ((texts (make-hash-table :test #'equal)))
      (when-let* ((list (ignore-errors (ygg-git-compare--list))))
        (dolist (c (buffer-local-value 'ygg-git-compare--comments list))
          (puthash (plist-get c :id) (plist-get c :text) texts)))
      (seq-keep
       (lambda (file)
         (when (magit-section-match 'file file)
           (let ((head-end (or (oref file content) (oref file end))))
             (cons (oref file value)
                   (or
                    (append
                     (ygg-git-compare--imenu-comments (oref file start) head-end texts)
                     (mapcar (lambda (hunk)
                               (let ((name (save-excursion
                                             (goto-char (oref hunk start))
                                             (buffer-substring-no-properties
                                              (point) (line-end-position))))
                                     (comments (ygg-git-compare--imenu-comments
                                                (oref hunk start) (oref hunk end) texts)))
                                 (if comments
                                     (cons name (cons (cons name (oref hunk start)) comments))
                                   (cons name (oref hunk start)))))
                             (seq-filter (lambda (s) (magit-section-match 'hunk s))
                                         (oref file children))))
                    (oref file start))))))
       (oref root children)))))

;;; Opening

(defun ygg-git-compare-buffer (root a b)
  "Magit's diff of sides A and B in ROOT, folded to its files, not yet shown."
  (let* ((default-directory root)
         (sides (ygg-git-compare--settled (ygg-git-compare-resolve a)
                                          (ygg-git-compare-resolve b)))
         (a-side (car sides))
         (b-side (cadr sides))
         (dots (if (magit-git-string "merge-base" (plist-get a-side :diff)
                                    (plist-get b-side :diff))
                   "..." ".."))
         (plan (ygg-git-compare--planned a-side b-side dots root))
         (default-directory (plist-get plan :dir)))
    (with-current-buffer
        (ygg-git-compare--display nil
          (magit-setup-buffer #'magit-diff-mode t
            (magit-buffer-diff-range (plist-get plan :range))
            (magit-buffer-diff-typearg nil)
            (magit-buffer-diff-type 'committed)
            (magit-buffer-diff-args (ygg-git-compare--args plan))
            (magit-buffer-diff-files nil)
            (magit-buffer-diff-files-suspended nil)
            (magit-git-global-arguments (ygg-git-compare--global-args))
            (magit-diff-sections-hook (ygg-git-compare--sections-hook))
            (ygg-git-compare--root root)
            (ygg-git-compare--plan plan)
            (ygg-git-compare--a-spec a)
            (ygg-git-compare--b-spec b)
            (ygg-git-compare--a a-side)
            (ygg-git-compare--b b-side)
            (ygg-git-compare--dots dots)))
      (ygg-git-compare-mode 1)
      (ygg-git-compare--load-comments)
      (add-hook 'magit-refresh-buffer-hook #'ygg-git-compare--header nil t)
      (add-hook 'post-command-hook #'ygg-git-compare--schedule nil t)
      (ygg-git-compare--header)
      (ygg-git-compare--fold)
      (current-buffer))))

(defun ygg-git-compare-open (a b)
  "Compare sides A and B of this repository in the whole frame.
Its review comments are the ones kept for these sides."
  (let* ((old (and (bound-and-true-p ygg-git-compare-mode) (ygg-git-compare--list)))
         (root (or (and old (buffer-local-value 'ygg-git-compare--root old))
                   (magit-toplevel)
                   (user-error "Not in a git repository")))
         (config (if old
                     (buffer-local-value 'ygg-git-compare--window-config old)
                   (current-window-configuration)))
         (buffer (ygg-git-compare-buffer root a b)))
    (when (and old (buffer-live-p old) (not (eq old buffer)))
      (ygg-git-compare--close old))
    (delete-other-windows)
    (switch-to-buffer buffer)
    (let ((right (split-window nil (round (* ygg-git-compare-list-width
                                             (window-total-width)))
                               'right)))
      (setq ygg-git-compare--window-config config
            ygg-git-compare--file-window right)
      (if-let* ((file (magit-file-at-point)))
          (ygg-git-compare--show-file file)
        (save-selected-window (ygg-git-compare-log))))
    buffer))

;;;###autoload
(defun ygg-git-compare (a b)
  "Compare side A with side B of this repository in the whole frame.
A side is a worktree with what it has not committed, a branch, a commit
or an open pull request.  B is read first and defaults to this worktree;
A defaults to a pull request's base, HEAD when B is this worktree with
uncommitted changes, else the repository's default branch."
  (interactive (progn (require 'magit) (ygg-git-compare--read-sides)))
  (ygg-git-compare-open a b))

;;;###autoload
(defun ygg-git-compare-at-point ()
  "Compare the worktree, branch or commit at point against a side read for A."
  (interactive)
  (require 'magit)
  (let ((b (if-let* ((dir (magit-section-value-if 'worktree)))
               (cons 'worktree (file-name-as-directory dir))
             (when-let* ((rev (magit-branch-or-commit-at-point)))
               (cons 'rev rev)))))
    (if (not b)
        (call-interactively #'ygg-git-compare)
      (let* ((cands (ygg-git-compare-candidates))
             (a (ygg-git-compare--read (format "Compare %s against (A)"
                                               (ygg-git-compare--label b cands))
                                       cands (ygg-git-compare-default-a b))))
        (ygg-git-compare-open a b)))))

(defun ygg-git-compare--merge-requests (repo)
  "REPO's open merge requests as `ygg-git-compare-candidates' lists pull
requests, as last found; glab is asked again in the background when they
are old."
  (pcase-let ((`(,_forge ,host ,path) repo)
              (dir default-directory))
    (ygg-git-compare--cached
     (list 'mrs repo) ygg-git-compare-list-ttl
     (lambda (done)
       (let ((default-directory dir))
         (ygg-git-compare--forge-async
          "glab" (list "api" "--hostname" host
                       (format "projects/%s/merge_requests?state=opened&per_page=100"
                               (url-hexify-string path)))
          (lambda (status text err)
            (ygg-git-compare--answer
             done "glab" status text err
             (lambda (text)
               (mapcar (lambda (mr)
                         (list :number (plist-get mr :iid) :title (plist-get mr :title)
                               :headRefName (plist-get mr :source_branch)
                               :baseRefName (plist-get mr :target_branch)))
                       (ygg-git-compare--json text))))))))
     #'ygg-git-compare--announce)))

(defun ygg-git-compare--review-targets ()
  "Branches and open pull or merge requests to review, as (LABEL . SPEC)."
  (let* ((repo (ignore-errors (ygg-git-compare--forge-repo)))
         (gitlab (eq (car repo) 'gitlab))
         (cands (seq-filter
                 (lambda (c) (member (get-text-property 0 'ygg-git-compare-group (car c))
                                     '("Branches" "Remote branches" "Pull requests")))
                 (ygg-git-compare-candidates (not gitlab)))))
    (append cands
            (mapcar (lambda (mr)
                      (cons (ygg-git-compare--group
                             (format "!%s %s" (plist-get mr :number) (plist-get mr :title))
                             "Merge requests"
                             (format "%s → %s" (plist-get mr :headRefName)
                                     (plist-get mr :baseRefName)))
                            (cons 'pr (list :number (plist-get mr :number)
                                            :head (plist-get mr :headRefName)
                                            :base (plist-get mr :baseRefName)))))
                    (and gitlab (ygg-git-compare--merge-requests repo))))))

(defun ygg-git-compare--review-specs (pr name remote)
  "The sides of PR's compare as (A . B) when its commits are here, else nil."
  (let ((head (plist-get pr :head))
        (start (plist-get pr :start)))
    (when (and (magit-commit-p head) (magit-commit-p start))
      (cons (cons 'rev (or (plist-get (ygg-git-compare--with-base pr) :base)
                           (ygg-git-compare--pr-base (plist-get pr :base-ref) remote)))
            (cons 'pr (list :number (plist-get pr :number) :sha head :head name
                            :base (plist-get pr :base-ref) :remote remote))))))

(defun ygg-git-compare--review-fetch (pr remote dir done)
  "Fetch what PR's compare lacks here, then call DONE with what went wrong,
or nil."
  (let ((refs (delq nil
                    (list (unless (magit-commit-p (plist-get pr :head))
                            (format (if (eq (plist-get pr :forge) 'gitlab)
                                        "merge-requests/%s/head"
                                      "pull/%s/head")
                                    (plist-get pr :number)))
                          (unless (magit-commit-p (plist-get pr :start))
                            (plist-get pr :base-ref))))))
    (cl-labels ((next (refs)
                  (if (null refs)
                      (funcall done nil)
                    (let ((default-directory dir))
                      (ygg-git-compare--fetch-ref
                       remote (car refs)
                       (lambda (_sha &optional err)
                         (if err (funcall done err) (next (cdr refs)))))))))
      (next refs))))

(defun ygg-git-compare--review-note (buffer note)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ygg-git-compare--note note)
      (ygg-git-compare--header))))

(defun ygg-git-compare--review-show (buffer specs)
  (with-current-buffer buffer
    (setq ygg-git-compare--note nil)
    (if (and (equal (car specs) ygg-git-compare--a-spec)
             (equal (cdr specs) ygg-git-compare--b-spec))
        (ygg-git-compare--header)
      (ygg-git-compare--redraw (car specs) (cdr specs)))))

(defun ygg-git-compare--review-none (buffer name selector err base)
  "Say in BUFFER's header and in the echo area why the review it was opened
for has no pull request: ERR being what the forge said, BASE what it is
compared with instead."
  (let ((text (cond ((and (integerp selector) err
                          (not (string-match-p "Could not resolve\\|not found\\|404" err)))
                     (format "Could not read PR #%s (%s)" selector err))
                    ((integerp selector) (format "PR #%s not found" selector))
                    (t (format "No open pull request for %s%s; comparing it with %s"
                               name (if err (format " (%s)" err) "") base)))))
    (ygg-git-compare--review-note buffer text)
    (message "%s" text)))

(defun ygg-git-compare--review-settle (buffer token dir name remote selector pr err)
  "Bring BUFFER, a compare opened for a review before its pull request was
known here, to PR: its sides once their commits are here, fetched first.
Without PR, say why it stays as it is, ERR being what the forge said.
Nothing is done when BUFFER was opened for another review since TOKEN."
  (when (and (buffer-live-p buffer)
             (eq token (buffer-local-value 'ygg-git-compare--review-token buffer))
             (with-current-buffer buffer
               (or ygg-git-compare--note
                   (and pr (eq (car-safe ygg-git-compare--b-spec) 'pr)
                        (equal (plist-get (cdr ygg-git-compare--b-spec) :number)
                               (plist-get pr :number))))))
    (let ((default-directory dir)
          (number (plist-get pr :number)))
      (cond
       ((null pr)
        (ygg-git-compare--review-none buffer name selector err
                                      (cdr (buffer-local-value 'ygg-git-compare--a-spec buffer))))
       ((ygg-git-compare--review-specs pr name remote)
        (ygg-git-compare--review-show buffer (ygg-git-compare--review-specs pr name remote)))
       (t
        (ygg-git-compare--review-note buffer (format "fetching PR #%s…" number))
        (ygg-git-compare--review-fetch
         pr remote dir
         (lambda (failure)
           (when (and (buffer-live-p buffer)
                      (eq token (buffer-local-value 'ygg-git-compare--review-token buffer)))
             (let ((default-directory dir))
               (if-let* (((not failure))
                         (specs (ygg-git-compare--review-specs pr name remote)))
                   (ygg-git-compare--review-show buffer specs)
                 (ygg-git-compare--review-note
                  buffer (format "PR #%s not fetched: %s" number
                                 (or failure "its commits are not here")))))))))))))

;;;###autoload
(defun ygg-git-compare-review-branch (&optional target)
  "Compare TARGET as its open pull or merge request has it, so held
comments go there; a branch without one, against the default branch.
TARGET is a branch or a (pr . PLIST) side; nil takes the branch at
point, else the current one.  Interactively, pick among branches and
open pull and merge requests, the branch at point first.  The compare
opens at once from what is known; the request and its commits are
fetched in the background and the compare drawn again when they are here."
  (interactive
   (progn
     (require 'magit)
     (let ((here (or (magit-branch-at-point) (magit-get-current-branch))))
       (list (pcase (ygg-git-compare--read "Review" #'ygg-git-compare--review-targets
                                           (and here (cons 'rev here)))
               (`(rev . ,(pred string-empty-p)) (user-error "No branch here"))
               (`(rev . ,branch) branch)
               (spec spec))))))
  (require 'magit)
  (let* ((by-number (eq (car-safe target) 'pr))
         (branch (cond (by-number (plist-get (cdr target) :head))
                       (target)
                       ((or (magit-branch-at-point) (magit-get-current-branch)))
                       (t (user-error "No branch here"))))
         (name (if (and branch (magit-remote-branch-p branch))
                   (cdr (magit-split-branch-name branch))
                 branch))
         (selector (if by-number (plist-get (cdr target) :number) name))
         (remote (ygg-git-compare--pr-remote))
         (dir default-directory)
         (why nil)
         (repo (condition-case err (ygg-git-compare--forge-repo remote)
                 (user-error (setq why (error-message-string err)) nil)))
         (cell (list nil))
         (token (list nil))
         (pr (and repo
                  (progn
                    (ygg-git-compare--refine-pr-remote)
                    (ygg-git-compare--forge-pr
                     repo selector
                     (lambda (pr err)
                       (ygg-git-compare--review-settle (car cell) token dir name remote
                                                       selector pr err))))))
         (entry (and repo (ygg-git-compare--cache-entry
                           (ygg-git-compare--pr-key repo selector))))
         (specs (and pr (ygg-git-compare--review-specs pr name remote)))
         (base (if pr
                   (ygg-git-compare--pr-base (plist-get pr :base-ref) remote)
                 (ygg-git-compare--default-branch)))
         (opened (or specs
                     (cons (cons 'rev base)
                           (cons 'rev (if (and by-number (not (magit-rev-verify name)))
                                          base
                                        branch))))))
    (setcar cell (ygg-git-compare-open (car opened) (cdr opened)))
    (when (buffer-live-p (car cell))
      (with-current-buffer (car cell)
        (setq ygg-git-compare--review-token token)))
    (unless specs
      (ygg-git-compare--review-note (car cell) "looking up the pull request…")
      (cond (pr (ygg-git-compare--review-settle (car cell) token dir name remote
                                                selector pr nil))
            ((or entry (not repo))
             (ygg-git-compare--review-none (car cell) name selector
                                           (or (plist-get entry :error) why)
                                           (cdr (car opened))))))
    (car cell)))

(provide 'ygg-git-compare)
;;; ygg-git-compare.el ends here
