;;; ygg-git-compare-pr-info.el --- a pull request's conversation and CI checks in its compare -*- lexical-binding: t; -*-

;;; Commentary:
;; A compare of a pull or merge request puts what is said about it in two
;; sections ahead of the diff: Conversation, its top-level comments and
;; review summaries, and Checks, each CI job with its state and duration.
;; Both are fetched in the background through the compare's shared forge
;; helper and cache; the checks are kept per head commit and asked again
;; every `ygg-git-compare-checks-running-ttl' seconds while the compare is
;; on screen and any is still running.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'ygg-git-compare)
(require 'ygg-git-compare-threads)

(defcustom ygg-git-compare-checks-ttl 300
  "Seconds the checks of a pull request that has none running are kept."
  :type 'number
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-checks-running-ttl 30
  "Seconds between asking the forge again while a check is running."
  :type 'number
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-conversation-fold-over 5
  "The Conversation is folded when it holds more comments than this."
  :type 'natnum
  :group 'ygg-git-compare)

(defvar-local ygg-git-compare--pr-info-sig nil
  "What the sections of this compare were last drawn from.")

(defvar-local ygg-git-compare--checks-timer nil)

(defvar-local ygg-git-compare--checks-waiting nil)

(defvar-local ygg-git-compare--checks-errors 0
  "How many times in a row the forge has failed to answer for the checks.")

(defconst ygg-git-compare--checks-max-errors 3)

(defconst ygg-git-compare--pr-info-order '(ygg-git-compare-conversation ygg-git-compare-checks))

(defconst ygg-git-compare--check-order '(failed cancelled warning running pending passed skipped))

(defconst ygg-git-compare--check-marks
  '((passed "✓" success) (failed "✗" error) (cancelled "⊘" warning)
    (warning "!" warning) (running "●" warning) (pending "○" warning) (skipped "–" shadow)))

;;; Reading

(defun ygg-git-compare--check-time (stamp)
  (and (stringp stamp) (not (string-prefix-p "0001" stamp))
       (ygg-git-compare--remote-time stamp)))

(defun ygg-git-compare--check-blank-p (value)
  (or (null value) (equal value "")))

(defun ygg-git-compare--check-github (item)
  "ITEM, a CheckRun or StatusContext of gh's statusCheckRollup, as a check."
  (if (or (equal (plist-get item :__typename) "StatusContext") (plist-get item :context))
      (list :name (or (plist-get item :context) "status")
            :state (pcase (upcase (or (plist-get item :state) ""))
                     ("SUCCESS" 'passed)
                     ((or "FAILURE" "ERROR") 'failed)
                     (_ 'pending))
            :url (plist-get item :targetUrl))
    (let* ((conclusion (plist-get item :conclusion))
           (status (upcase (or (plist-get item :status) "")))
           (state (cond ((equal status "IN_PROGRESS") 'running)
                        ((not (equal status "COMPLETED")) 'pending)
                        ((ygg-git-compare--check-blank-p conclusion) 'skipped)
                        (t (pcase (upcase conclusion)
                             ("SUCCESS" 'passed)
                             ((or "FAILURE" "TIMED_OUT" "STARTUP_FAILURE" "ACTION_REQUIRED") 'failed)
                             ("CANCELLED" 'cancelled)
                             (_ 'skipped))))))
      (list :name (or (plist-get item :name) "check") :state state
            :started (ygg-git-compare--check-time (plist-get item :startedAt))
            :finished (and (memq state '(passed failed cancelled skipped))
                           (ygg-git-compare--check-time (plist-get item :completedAt)))
            :url (plist-get item :detailsUrl)
            :where (let ((workflow (plist-get item :workflowName)))
                     (and (not (ygg-git-compare--check-blank-p workflow)) workflow))))))

(defun ygg-git-compare--checks-parse-github (text)
  (let ((rollup (plist-get (ygg-git-compare--json text) :statusCheckRollup)))
    (unless (proper-list-p rollup) (error "no rollup"))
    (list :checks (mapcar #'ygg-git-compare--check-github rollup))))

(defun ygg-git-compare--check-gitlab (job)
  (list :name (or (plist-get job :name) "job")
        :state (pcase (plist-get job :status)
                 ("success" 'passed)
                 ("failed" (if (plist-get job :allow_failure) 'warning 'failed))
                 ((or "canceled" "canceling") 'cancelled)
                 ("running" 'running)
                 ((or "skipped" "manual") 'skipped)
                 (_ 'pending))
        :started (ygg-git-compare--check-time (plist-get job :started_at))
        :finished (ygg-git-compare--check-time (plist-get job :finished_at))
        :url (plist-get job :web_url)
        :where (plist-get job :stage)))

(defun ygg-git-compare--checks-parse-gitlab-jobs (text pipeline)
  (let ((jobs (ygg-git-compare--remote-json text)))
    (list :checks (mapcar #'ygg-git-compare--check-gitlab jobs)
          :url (plist-get pipeline :web_url))))

(defun ygg-git-compare--checks-fetch-gitlab (pr done)
  (let ((host (plist-get pr :host))
        (path (url-hexify-string (plist-get pr :path))))
    (ygg-git-compare--forge-async
     "glab" (list "api" "--hostname" host
                  (format "projects/%s/merge_requests/%s" path (plist-get pr :number)))
     (lambda (status text err)
       (ygg-git-compare--answer
        (lambda (pipeline &optional failure)
          (cond (failure (funcall done nil failure))
                ((null pipeline) (funcall done (list :checks nil)))
                (t (ygg-git-compare--forge-async
                    "glab" (list "api" "--hostname" host "--paginate"
                                 (format "projects/%s/pipelines/%s/jobs?per_page=100"
                                         path (plist-get pipeline :id)))
                    (lambda (status text err)
                      (ygg-git-compare--answer
                       done "glab" status text err
                       (lambda (text)
                         (ygg-git-compare--checks-parse-gitlab-jobs text pipeline))))))))
        "glab" status text err
        (lambda (text)
          (let ((pipeline (plist-get (ygg-git-compare--json text) :head_pipeline)))
            (and pipeline (plist-get pipeline :id) pipeline))))))))

(defun ygg-git-compare--checks-fetch (pr done)
  "Ask the forge for PR's checks, then call DONE with them, or with nil
and why not."
  (if (eq (plist-get pr :forge) 'gitlab)
      (ygg-git-compare--checks-fetch-gitlab pr done)
    (ygg-git-compare--forge-async
     "gh" (list "pr" "view" (format "%s" (plist-get pr :number))
                "--repo" (format "%s/%s" (plist-get pr :host) (plist-get pr :path))
                "--json" "statusCheckRollup")
     (lambda (status text err)
       (ygg-git-compare--answer done "gh" status text err
                                #'ygg-git-compare--checks-parse-github)))))

;;; Keeping

(defun ygg-git-compare--checks-sha ()
  (or (plist-get (cdr-safe ygg-git-compare--b-spec) :sha)
      (plist-get ygg-git-compare--b :diff)))

(defun ygg-git-compare--checks-key (pr)
  (list 'checks (plist-get pr :forge) (plist-get pr :host) (plist-get pr :path)
        (plist-get pr :number) (ygg-git-compare--checks-sha)))

(defun ygg-git-compare--check-valid-p (check)
  (and (proper-list-p check)
       (cl-evenp (length check))
       (stringp (plist-get check :name))
       (assq (plist-get check :state) ygg-git-compare--check-marks)
       (seq-every-p (lambda (key)
                      (let ((value (plist-get check key)))
                        (or (null value) (stringp value))))
                    '(:url :where))
       (seq-every-p (lambda (key)
                      (let ((value (plist-get check key)))
                        (or (null value) (numberp value))))
                    '(:started :finished))))

(defun ygg-git-compare--checks-valid-p (value)
  (and (proper-list-p value)
       (cl-evenp (length value))
       (proper-list-p (plist-get value :checks))
       (seq-every-p #'ygg-git-compare--check-valid-p (plist-get value :checks))))

(defun ygg-git-compare--checks-value (key)
  (let ((value (ignore-errors (ygg-git-compare--cache-value key))))
    (and (ygg-git-compare--checks-valid-p value) value)))

(defun ygg-git-compare--checks-active-p (value)
  (seq-some (lambda (check) (memq (plist-get check :state) '(running pending)))
            (plist-get value :checks)))

(defun ygg-git-compare--checks-sorted (checks)
  (sort (copy-sequence checks)
        (lambda (a b)
          (< (seq-position ygg-git-compare--check-order (plist-get a :state))
             (seq-position ygg-git-compare--check-order (plist-get b :state))))))

(defun ygg-git-compare--checks-counts (checks)
  "CHECKS counted as (PASSED BAD ACTIVE WARNED): a cancelled check is bad, as
the status row's merge check counts it, a pending one is active, and a failed
one allowed to fail is warned."
  (let ((passed 0) (bad 0) (active 0) (warned 0))
    (dolist (check checks)
      (pcase (plist-get check :state)
        ('passed (cl-incf passed))
        ((or 'failed 'cancelled) (cl-incf bad))
        ('warning (cl-incf warned))
        ((or 'running 'pending) (cl-incf active))))
    (list passed bad active warned)))

(defun ygg-git-compare--checks-summary (checks)
  "CHECKS' counts as ✓ 12 · ✗ 1 · ! 1 · ● 2, those that are none left out."
  (pcase-let ((`(,passed ,bad ,active ,warned) (ygg-git-compare--checks-counts checks)))
    (string-join
     (delq nil
           (list (and (> passed 0) (propertize (format "✓ %d" passed) 'font-lock-face 'success))
                 (and (> bad 0) (propertize (format "✗ %d" bad) 'font-lock-face 'error))
                 (and (> warned 0) (propertize (format "! %d" warned) 'font-lock-face 'warning))
                 (and (> active 0) (propertize (format "● %d" active) 'font-lock-face 'warning))))
     " · ")))

(defun ygg-git-compare--checks-view ()
  "What this compare shows of the checks: (:value V :error E :fetching F)."
  (let* ((pr (ygg-git-compare--remote-pr))
         (key (and pr (ygg-git-compare--checks-key pr)))
         (value (and key (ygg-git-compare--checks-value key)))
         (failed (and key (plist-get (ignore-errors (ygg-git-compare--cache-entry key)) :error))))
    (list :value value :error failed
          :fetching (and key (null value) (null failed)
                         (ygg-git-compare--refreshing-p 'checks)
                         t))))

(defun ygg-git-compare--conversation-p (comment)
  (and (eq (plist-get comment :level) 'review)
       (not (plist-get comment :heading))))

(defun ygg-git-compare--pr-info-state ()
  (list :conversation
        (seq-filter #'ygg-git-compare--conversation-p
                    (ygg-git-compare--remote-shown (current-buffer) t))
        :checks (ygg-git-compare--checks-view)))

(defun ygg-git-compare--pr-info-p ()
  (and (eq (car-safe ygg-git-compare--b-spec) 'pr)
       (ygg-git-compare--remote-pr)))

;;; Fetching

(defun ygg-git-compare--checks-visible-p (buffer)
  (and (buffer-live-p buffer) (get-buffer-window buffer 'visible) t))

(defun ygg-git-compare--checks-stop ()
  (when (timerp ygg-git-compare--checks-timer)
    (cancel-timer ygg-git-compare--checks-timer))
  (setq ygg-git-compare--checks-timer nil))

(defun ygg-git-compare--checks-arm (buffer)
  "Ask again in BUFFER after `ygg-git-compare-checks-running-ttl' seconds, if
a check is running and BUFFER is on screen; else stop."
  (with-current-buffer buffer
    (ygg-git-compare--checks-stop)
    (when-let* ((pr (ygg-git-compare--remote-pr))
                ((ygg-git-compare--checks-active-p
                  (ygg-git-compare--checks-value (ygg-git-compare--checks-key pr))))
                ((< ygg-git-compare--checks-errors ygg-git-compare--checks-max-errors))
                ((ygg-git-compare--checks-visible-p buffer)))
      (add-hook 'kill-buffer-hook #'ygg-git-compare--checks-stop nil t)
      (add-hook 'window-buffer-change-functions #'ygg-git-compare--checks-resume nil t)
      (setq ygg-git-compare--checks-timer
            (run-at-time ygg-git-compare-checks-running-ttl nil
                         #'ygg-git-compare--checks-poll buffer)))))

(defun ygg-git-compare--checks-poll (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ygg-git-compare--checks-timer nil)
      (when (ygg-git-compare--checks-visible-p buffer)
        (condition-case nil
            (ygg-git-compare--checks-start t)
          (error nil))))))

(defun ygg-git-compare--checks-resume (&rest _)
  "Ask again, and keep asking, now this compare is on screen once more."
  (when (and (null ygg-git-compare--checks-timer)
             (ygg-git-compare--checks-visible-p (current-buffer)))
    (condition-case nil
        (progn (ygg-git-compare--checks-start)
               (ygg-git-compare--checks-arm (current-buffer)))
      (error nil))))

(defun ygg-git-compare--section-bounds (section)
  (cons (list section
              (marker-position (oref section start))
              (and (oref section content) (marker-position (oref section content)))
              (marker-position (oref section end)))
        (mapcan #'ygg-git-compare--section-bounds (oref section children))))

(defun ygg-git-compare--pr-info-anchor (section pos)
  "Where point is in SECTION as (KEY LINES COLUMN): the check or the comment
it is on, and its line and column within the section from POS."
  (list (or (and (eq (oref section type) 'ygg-git-compare-checks)
                 (when-let* ((check (magit-section-value-if 'ygg-git-compare-check)))
                   (cons 'check (plist-get check :name))))
            (when-let* ((id (get-text-property (point) 'ygg-git-compare-conversation-id)))
              (cons 'comment id)))
        (count-lines pos (line-beginning-position))
        (- (point) (line-beginning-position))))

(defun ygg-git-compare--pr-info-anchored (anchor new pos)
  "Point for ANCHOR, from `ygg-git-compare--pr-info-anchor', in NEW drawn at POS."
  (pcase-let ((`(,key ,lines ,column) anchor))
    (goto-char
     (or (and new
              (pcase key
                (`(check . ,name)
                 (when-let* ((row (seq-find (lambda (s)
                                              (equal (plist-get (oref s value) :name) name))
                                            (oref new children))))
                   (marker-position (oref row start))))
                (`(comment . ,id)
                 (cadr (assoc id (ygg-git-compare--conversation-spans))))))
         (progn (goto-char pos)
                (forward-line (if new
                                  (min lines (max 0 (1- (count-lines pos (oref new end)))))
                                0))
                (point))))
    (forward-char (min column (- (line-end-position) (point))))))

(defun ygg-git-compare--pr-info-replace (type draw)
  "Draw the section of TYPE again where it is, by calling DRAW there, leaving
the rest of the buffer, the diff included, as it is.  If DRAW fails the old
section is put back and the error signalled."
  (let* ((root magit-root-section)
         (children (oref root children))
         (rank (seq-position ygg-git-compare--pr-info-order type))
         (old (seq-find (lambda (s) (eq (oref s type) type)) children))
         (next (and (not old)
                    (seq-find (lambda (s)
                                (let ((other (seq-position ygg-git-compare--pr-info-order
                                                           (oref s type))))
                                  (or (null other) (> other rank))))
                              children)))
         (anchor (or old next))
         (index (or (seq-position children anchor) (length children)))
         (pos (cond (old (marker-position (oref old start)))
                    (next (marker-position (oref next start)))
                    (children (marker-position (oref (car (last children)) end)))
                    (t (point-min))))
         (inside (and old (>= (point) pos) (< (point) (oref old end))))
         (spot (and inside (ygg-git-compare--pr-info-anchor old pos)))
         (saved (and old (buffer-substring pos (oref old end))))
         (bounds (and old (ygg-git-compare--section-bounds old)))
         (origin (point))
         (here (copy-marker (point) t))
         (tail (copy-marker pos t))
         (inhibit-read-only t)
         (magit-section-cache-visibility nil)
         new)
    (when old (delete-region pos (oref old end)))
    (goto-char pos)
    (condition-case failure
        (let ((magit-insert-section--parent root)
              (magit-insert-section--oldroot root))
          (setq new (funcall draw)))
      (error
       (delete-region pos tail)
       (goto-char pos)
       (when old
         (insert saved)
         (pcase-dolist (`(,section ,start ,content ,end) bounds)
           (set-marker (oref section start) start)
           (when content (set-marker (oref section content) content))
           (set-marker (oref section end) end))
         (if (oref old hidden) (magit-section-hide old) (magit-section-show old)))
       (goto-char origin)
       (set-marker here nil)
       (set-marker tail nil)
       (signal (car failure) (cdr failure))))
    (let ((rest (remq new (remq old (oref root children)))))
      (oset root children (append (seq-take rest index) (and new (list new))
                                  (seq-drop rest index))))
    (when (> (oref root start) pos) (set-marker (oref root start) pos))
    (if inside
        (ygg-git-compare--pr-info-anchored spot new pos)
      (goto-char here))
    (set-marker here nil)
    (set-marker tail nil)
    (when new (if (oref new hidden) (magit-section-hide new) (magit-section-show new)))
    new))

(defun ygg-git-compare--pr-info-redraw (state keys)
  "Draw the sections of KEYS, :conversation or :checks, again from STATE."
  (dolist (key keys)
    (if (eq key :conversation)
        (ygg-git-compare--pr-info-replace
         'ygg-git-compare-conversation
         (lambda ()
           (when-let* ((comments (plist-get state :conversation)))
             (ygg-git-compare--insert-conversation comments))))
      (ygg-git-compare--pr-info-replace
       'ygg-git-compare-checks
       (lambda () (ygg-git-compare--insert-checks (plist-get state :checks)))))
    (setq ygg-git-compare--pr-info-sig
          (plist-put (copy-sequence ygg-git-compare--pr-info-sig) key (plist-get state key))))
  (ygg-git-compare--header))

(defun ygg-git-compare--pr-info-refresh (buffer &rest forced)
  "Draw BUFFER's sections again, in place, where what they show has changed or
FORCED, their keys, says so; the keys drawn."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'magit-diff-mode)
                 (bound-and-true-p magit-root-section)
                 (ygg-git-compare--pr-info-p))
        (let* ((state (ygg-git-compare--pr-info-state))
               (keys (seq-filter (lambda (key)
                                   (or (memq key forced)
                                       (not (equal (plist-get state key)
                                                   (plist-get ygg-git-compare--pr-info-sig key)))))
                                 '(:conversation :checks))))
          (when keys
            (condition-case nil (ygg-git-compare--pr-info-redraw state keys) (error nil)))
          keys)))))

(defun ygg-git-compare--checks-landed (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ygg-git-compare--checks-waiting nil)
      (setq ygg-git-compare--checks-errors
            (if-let* ((pr (ygg-git-compare--remote-pr))
                      ((plist-get (ignore-errors
                                    (ygg-git-compare--cache-entry (ygg-git-compare--checks-key pr)))
                                  :error)))
                (1+ ygg-git-compare--checks-errors)
              0))
      (ygg-git-compare--checks-arm buffer)
      (let ((active (and (ygg-git-compare--pr-info-p)
                         (ygg-git-compare--checks-active-p
                          (plist-get (ygg-git-compare--checks-view) :value)))))
        (when (memq :conversation
                    (apply #'ygg-git-compare--pr-info-refresh buffer (and active '(:checks))))
          (ygg-git-compare--redraw-comments buffer))))))

(defun ygg-git-compare--checks-start (&optional force)
  "Fetch this compare's checks in the background unless they are fresh or
being fetched; with FORCE, whatever their age."
  (when-let* ((pr (ygg-git-compare--remote-pr)))
    (let* ((buffer (current-buffer))
           (dir default-directory)
           (key (ygg-git-compare--checks-key pr))
           (ttl (cond (force 0)
                      ((ygg-git-compare--checks-active-p (ygg-git-compare--checks-value key))
                       ygg-git-compare-checks-running-ttl)
                      (t ygg-git-compare-checks-ttl)))
           (retry (and force 0))
           (waiter (and (not ygg-git-compare--checks-waiting)
                        (ygg-git-compare--cache-stale-p
                         (ignore-errors (ygg-git-compare--cache-entry key)) ttl retry)
                        (lambda (&rest _)
                          (run-at-time 0 nil #'ygg-git-compare--checks-landed buffer)))))
      (when waiter (setq ygg-git-compare--checks-waiting t))
      (condition-case failure
          (ygg-git-compare--cached
           key ttl
           (lambda (done)
             (let ((default-directory dir))
               (ygg-git-compare--checks-fetch pr done)))
           waiter retry)
        (error (setq ygg-git-compare--checks-waiting nil)
               (signal (car failure) (cdr failure)))))))

(defun ygg-git-compare--pr-info-refetch ()
  "Ask the forge afresh for this compare's comments and checks."
  (when (ygg-git-compare--pr-info-p)
    (setq ygg-git-compare--checks-errors 0)
    (condition-case nil
        (progn
          (ygg-git-compare--checks-start t)
          (let ((ygg-git-compare-threads-ttl 0))
            (ygg-git-compare--remote-comments (current-buffer))))
      (error nil))))

(add-hook 'ygg-git-compare-redraw-hook #'ygg-git-compare--pr-info-refetch)

;;; Drawing

(defun ygg-git-compare--check-duration (check)
  (when-let* ((started (plist-get check :started))
              (end (if (eq (plist-get check :state) 'running)
                       (float-time)
                     (plist-get check :finished))))
    (let ((seconds (max 0 (round (- end started)))))
      (cond ((< seconds 60) (format "%ds" seconds))
            ((< seconds 3600) (format "%dm %02ds" (/ seconds 60) (% seconds 60)))
            (t (format "%dh %02dm" (/ seconds 3600) (% (/ seconds 60) 60)))))))

(defun ygg-git-compare--check-line (check width)
  (pcase-let* ((state (plist-get check :state))
               (`(,mark ,face) (cdr (assq state ygg-git-compare--check-marks))))
    (concat "  "
            (propertize mark 'font-lock-face face) " "
            (truncate-string-to-width (plist-get check :name) width nil ?\s "…") "  "
            (propertize (format "%-9s" state) 'font-lock-face face) " "
            (propertize (format "%8s" (or (ygg-git-compare--check-duration check) ""))
                        'font-lock-face 'shadow)
            (if-let* ((where (plist-get check :where)))
                (propertize (concat "  " where) 'font-lock-face 'shadow)
              ""))))

(defun ygg-git-compare--conversation-count (comments)
  (seq-count (lambda (c) (not (plist-get c :notice))) comments))

(defun ygg-git-compare--insert-conversation (comments)
  (magit-insert-section (ygg-git-compare-conversation nil
                                                      (> (ygg-git-compare--conversation-count comments)
                                                         ygg-git-compare-conversation-fold-over))
    (magit-insert-heading (if (zerop (ygg-git-compare--conversation-count comments))
                              "Conversation"
                            (format "Conversation (%d)"
                                    (ygg-git-compare--conversation-count comments))))
    (dolist (comment comments)
      (let ((start (point)))
        (insert (ygg-git-compare--comment-block comment) "\n")
        (put-text-property start (point) 'ygg-git-compare-conversation-id
                           (plist-get comment :id))))
    (insert "\n")))

(defun ygg-git-compare--insert-checks (view)
  (let* ((value (plist-get view :value))
         (checks (ygg-git-compare--checks-sorted (plist-get value :checks)))
         (failed (plist-get view :error))
         (width (min 48 (apply #'max 8 (mapcar (lambda (c) (string-width (plist-get c :name)))
                                               checks)))))
    (when (or checks failed)
      (magit-insert-section (ygg-git-compare-checks nil
                                                    (and checks (not failed)
                                                         (not (seq-some (lambda (c)
                                                                          (not (eq (plist-get c :state)
                                                                                   'passed)))
                                                                        checks))))
        (magit-insert-heading
          (concat (propertize (if checks (format "Checks (%d)" (length checks)) "Checks")
                              'font-lock-face 'magit-section-heading)
                  (and checks (concat "  " (ygg-git-compare--checks-summary checks)))))
        (dolist (check checks)
          (magit-insert-section (ygg-git-compare-check check)
            (insert (ygg-git-compare--check-line check width) "\n")))
        (when failed
          (insert (propertize (format "  checks: %s%s" failed
                                      (if value " (showing the last fetch)" ""))
                              'font-lock-face 'error)
                  "\n"))
        (insert "\n")))))

(defun ygg-git-compare--insert-pr-info ()
  "The Conversation and Checks of the pull request this compare shows, ahead
of its diff; nothing for any other compare."
  (when (and (eq (car-safe ygg-git-compare--b-spec) 'pr) (ygg-git-compare--remote-pr))
    (let ((buffer (current-buffer)))
      (condition-case err
          (progn
            (ygg-git-compare--remote-watch-width)
            (condition-case nil (ygg-git-compare--checks-start) (error nil))
            (let ((state (ygg-git-compare--pr-info-state)))
              (setq ygg-git-compare--pr-info-sig state)
              (when-let* ((comments (plist-get state :conversation)))
                (ygg-git-compare--insert-conversation comments))
              (ygg-git-compare--insert-checks (plist-get state :checks)))
            (unless ygg-git-compare--checks-timer
              (ygg-git-compare--checks-arm buffer)))
        (error
         (insert (propertize (format "pull request: %s\n\n" (error-message-string err))
                             'font-lock-face 'error)))))))

(defun ygg-git-compare--checks-badge ()
  "The state of the checks in a few characters for the header line, or nil."
  (when (ygg-git-compare--pr-info-p)
    (let* ((view (ygg-git-compare--checks-view))
           (checks (plist-get (plist-get view :value) :checks)))
      (cond (checks (concat "CI " (ygg-git-compare--checks-summary checks)))
            ((plist-get view :value) nil)
            ((plist-get view :error) (propertize "CI ?" 'face 'error))
            ((plist-get view :fetching) (propertize "CI …" 'face 'shadow))))))

(defun ygg-git-compare--conversation-section ()
  (when-let* ((root (bound-and-true-p magit-root-section)))
    (seq-find (lambda (s) (eq (oref s type) 'ygg-git-compare-conversation))
              (oref root children))))

(defun ygg-git-compare--conversation-spans ()
  "The comments of the Conversation as (ID BEG END) in this buffer."
  (when-let* ((section (ygg-git-compare--conversation-section))
              (start (oref section content))
              (end (oref section end)))
    (let ((pos (marker-position start))
          (limit (marker-position end))
          spans)
      (while (< pos limit)
        (let ((next (or (next-single-property-change
                         pos 'ygg-git-compare-conversation-id nil limit)
                        limit))
              (id (get-text-property pos 'ygg-git-compare-conversation-id)))
          (when id (push (list id pos next) spans))
          (setq pos next)))
      (nreverse spans))))

;;; Acting on a check

(defun ygg-git-compare--check-at-point ()
  (or (magit-section-value-if 'ygg-git-compare-check)
      (user-error "No check here")))

(defun ygg-git-compare-check-open ()
  "Open the log of the check at point in the browser."
  (interactive)
  (browse-url (or (plist-get (ygg-git-compare--check-at-point) :url)
                  (user-error "This check has no page"))))

(defun ygg-git-compare-check-copy ()
  "Copy the address of the log of the check at point."
  (interactive)
  (let ((url (or (plist-get (ygg-git-compare--check-at-point) :url)
                 (user-error "This check has no page"))))
    (kill-new url)
    (message "Copied: %s" url)))

(defvar-keymap magit-ygg-git-compare-check-section-map
  "RET" #'ygg-git-compare-check-open
  "<return>" #'ygg-git-compare-check-open
  "o" #'ygg-git-compare-check-open
  "y" #'ygg-git-compare-check-copy)

(provide 'ygg-git-compare-pr-info)
;;; ygg-git-compare-pr-info.el ends here
