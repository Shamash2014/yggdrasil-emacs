;;; ygg-git-compare-job.el --- a CI job's steps and log in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; RET on a check of a compare opens its job here: the header, on GitHub the
;; steps, and the log, fetched in the background with the compare's forge
;; helper.  The log is cut to its last `ygg-git-compare-job-log-max'
;; characters, GitLab's section markers are dropped, ANSI colors are drawn
;; and GitHub's groups become step anchors.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'ansi-color)
(require 'ygg-git-compare)
(require 'ygg-git-compare-threads)
(require 'ygg-git-compare-pr-info)

(defcustom ygg-git-compare-job-log-max (* 4 1024 1024)
  "Characters of a job's log kept, counted from its end."
  :type 'natnum
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-job-log-timeout 120
  "Seconds the forge may take to hand over a job's log."
  :type 'number
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-job-strip-timestamps t
  "Whether GitHub's timestamp at the start of each log line is left out."
  :type 'boolean
  :group 'ygg-git-compare)

(defconst ygg-git-compare-job--error-regexp
  "##\\[error\\]\\|\\berror:\\|\\bFAILED\\b\\|^ERROR:")

(defconst ygg-git-compare-job--timestamp-regexp
  "\\`\\(?:\ufeff\\)?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9:.]+Z ?")

(defconst ygg-git-compare-job--section-regexp
  "section_\\(?:start\\|end\\):[0-9]+:[^\r\n]*\r\e\\[0K")

(defvar ygg-modal-special-modes)
(declare-function yggdrasil-define-mode-keys "yggdrasil-core")

(defvar-local ygg-git-compare-job--pr nil)
(defvar-local ygg-git-compare-job--id nil)
(defvar-local ygg-git-compare-job--check nil)
(defvar-local ygg-git-compare-job--details nil)
(defvar-local ygg-git-compare-job--details-error nil)
(defvar-local ygg-git-compare-job--log nil)
(defvar-local ygg-git-compare-job--log-error nil)
(defvar-local ygg-git-compare-job--generation 0)
(defvar-local ygg-git-compare-job--timer nil)
(defvar-local ygg-git-compare-job--errors 0)
(defvar-local ygg-git-compare-job--placed nil)
(defvar-local ygg-git-compare-job--strip nil)
(defvar-local ygg-git-compare-job--log-start nil)

;;; Reading

(defun ygg-git-compare-job-id (pr url)
  "The id of the job URL, a check's page, names on PR's forge, or nil."
  (when (stringp url)
    (let ((regexp (concat "\\`https?://" (regexp-quote (or (plist-get pr :host) ""))
                          (if (eq (plist-get pr :forge) 'gitlab)
                              "/.*/-/jobs/\\([0-9]+\\)"
                            "/[^/]+/[^/]+/actions/runs/[0-9]+/job/\\([0-9]+\\)")
                          "\\(?:\\'\\|[?#/]\\)")))
      (and (string-match regexp url) (match-string 1 url)))))

(defun ygg-git-compare-job--command (pr id suffix)
  (let ((host (plist-get pr :host)))
    (if (eq (plist-get pr :forge) 'gitlab)
        (list "glab" (list "api" "--hostname" host
                           (format "projects/%s/jobs/%s%s"
                                   (url-hexify-string (plist-get pr :path)) id suffix)))
      (list "gh" (list "api" "--hostname" host
                       (format "repos/%s/actions/jobs/%s%s" (plist-get pr :path) id suffix))))))

(defun ygg-git-compare-job--github-state (item)
  (ygg-git-compare--check-github
   (list :name (plist-get item :name) :status (plist-get item :status)
         :conclusion (plist-get item :conclusion)
         :startedAt (plist-get item :started_at)
         :completedAt (plist-get item :completed_at)
         :detailsUrl (plist-get item :html_url)
         :workflowName (plist-get item :workflow_name))))

(defun ygg-git-compare-job--parse (pr text)
  "The job in TEXT, the forge's answer for PR, as (:check C :steps S :conclusion X)."
  (let ((item (ygg-git-compare--json text)))
    (unless (keywordp (car-safe item)) (error "no job"))
    (if (eq (plist-get pr :forge) 'gitlab)
        (list :check (ygg-git-compare--check-gitlab item)
              :conclusion (or (plist-get item :failure_reason)
                              (and (plist-get item :allow_failure)
                                   (equal (plist-get item :status) "failed")
                                   "allowed to fail"))
              :stage (plist-get item :stage))
      (list :check (ygg-git-compare-job--github-state item)
            :conclusion (plist-get item :conclusion)
            :steps (mapcar (lambda (step)
                             (append (ygg-git-compare-job--github-state step)
                                     (list :number (plist-get step :number)
                                           :result (plist-get step :conclusion))))
                           (plist-get item :steps))))))

;;; Log

(defun ygg-git-compare-job--cap (text max)
  "TEXT kept from a line start within its last MAX characters, as (TEXT . DROPPED)."
  (if (<= (length text) max)
      (cons text 0)
    (let* ((cut (- (length text) max))
           (nl (string-search "\n" text cut))
           (start (if nl (1+ nl) cut)))
      (cons (substring text start) start))))

(defun ygg-git-compare-job--line (line forge strip)
  "LINE as it is drawn for FORGE, or nil to leave it out."
  (if (eq forge 'gitlab)
      (car (last (split-string line "\r")))
    (let ((stamp (and (string-match ygg-git-compare-job--timestamp-regexp line)
                      (prog1 (match-string 0 line)
                        (setq line (substring line (match-end 0)))))))
      (cond ((string-prefix-p "##[endgroup]" line) nil)
            ((string-match "\\`##\\[group\\]\\(.*\\)" line)
             (let ((name (match-string 1 line)))
               (propertize name 'ygg-git-compare-job-group name 'face 'bold)))
            (t (setq line (car (last (split-string line "\r"))))
               (concat (and stamp (not strip) stamp)
                       (cond ((string-prefix-p "##[error]" line) (propertize line 'face 'error))
                             ((string-prefix-p "##[warning]" line) (propertize line 'face 'warning))
                             (t line))))))))

(defun ygg-git-compare-job--ansi-face (beg end face)
  (when face (put-text-property beg end 'face face)))

(defun ygg-git-compare-job--log-text (text forge strip)
  "TEXT, a job's log on FORGE, readied to be shown: GitLab's section markers
dropped, GitHub's groups marked, ANSI colors applied."
  (when (eq forge 'gitlab)
    (setq text (replace-regexp-in-string ygg-git-compare-job--section-regexp "" text t t)))
  (setq text (replace-regexp-in-string "\r\n" "\n" text t t))
  (let ((lines (delq nil (mapcar (lambda (line) (ygg-git-compare-job--line line forge strip))
                                 (split-string text "\n")))))
    (with-temp-buffer
      (insert (string-join lines "\n"))
      (let ((ansi-color-apply-face-function #'ygg-git-compare-job--ansi-face))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (buffer-string))))

;;; Drawing

(defun ygg-git-compare-job--timestamp (time)
  (if time (format-time-string "%F %T" time) "-"))

(defun ygg-git-compare-job--field (label value)
  (insert (propertize (format "%-11s" label) 'face 'shadow) value "\n"))

(defun ygg-git-compare-job--state-text (check)
  (pcase-let ((`(,mark ,face) (cdr (assq (plist-get check :state) ygg-git-compare--check-marks))))
    (propertize (format "%s %s" mark (plist-get check :state)) 'face face)))

(defun ygg-git-compare-job--active-p ()
  (memq (plist-get (or (plist-get ygg-git-compare-job--details :check)
                       ygg-git-compare-job--check)
                   :state)
        '(running pending)))

(defun ygg-git-compare-job--insert-header ()
  (let* ((job ygg-git-compare-job--details)
         (check (or (plist-get job :check) ygg-git-compare-job--check))
         (gitlab (eq (plist-get ygg-git-compare-job--pr :forge) 'gitlab)))
    (insert (propertize (plist-get check :name) 'face 'bold) "\n")
    (ygg-git-compare-job--field (if gitlab "Stage" "Workflow") (or (plist-get check :where) "-"))
    (ygg-git-compare-job--field "Status" (ygg-git-compare-job--state-text check))
    (ygg-git-compare-job--field "Conclusion" (or (plist-get job :conclusion) "-"))
    (ygg-git-compare-job--field "Started"
                                (ygg-git-compare-job--timestamp (plist-get check :started)))
    (ygg-git-compare-job--field "Duration" (or (ygg-git-compare--check-duration check) "-"))
    (ygg-git-compare-job--field "URL" (or (plist-get ygg-git-compare-job--check :url)
                                          (plist-get check :url) "-"))
    (when ygg-git-compare-job--details-error
      (insert (propertize (format "job: %s\n" ygg-git-compare-job--details-error) 'face 'error)))))

(defun ygg-git-compare-job--insert-steps ()
  (when-let* ((steps (plist-get ygg-git-compare-job--details :steps)))
    (insert "\n" (propertize "Steps" 'face 'magit-section-heading) "\n")
    (dolist (step steps)
      (pcase-let ((`(,mark ,face) (cdr (assq (plist-get step :state) ygg-git-compare--check-marks)))
                  (start (point)))
        (insert "  " (propertize mark 'face face)
                (format " %2s " (or (plist-get step :number) ""))
                (plist-get step :name)
                (propertize (format "  %s  %s" (plist-get step :state)
                                    (or (ygg-git-compare--check-duration step) ""))
                            'face 'shadow)
                "\n")
        (put-text-property start (point) 'ygg-git-compare-job-step (plist-get step :name))))))

(defun ygg-git-compare-job--insert-log ()
  (insert "\n" (propertize "Log" 'face 'magit-section-heading) "\n")
  (cond (ygg-git-compare-job--log
         (when (> (plist-get ygg-git-compare-job--log :dropped) 0)
           (insert (propertize (format "log cut: showing the last %d of %d characters\n"
                                       (length (plist-get ygg-git-compare-job--log :raw))
                                       (+ (length (plist-get ygg-git-compare-job--log :raw))
                                          (plist-get ygg-git-compare-job--log :dropped)))
                               'face 'warning)))
         (setq ygg-git-compare-job--log-start (point))
         (insert (ygg-git-compare-job--log-text
                  (plist-get ygg-git-compare-job--log :raw)
                  (plist-get ygg-git-compare-job--pr :forge)
                  ygg-git-compare-job--strip)
                 "\n"))
        (ygg-git-compare-job--log-error
         (insert (propertize (format "log: %s\n" ygg-git-compare-job--log-error)
                             'face (if (ygg-git-compare-job--active-p) 'shadow 'error))))
        (t (insert (propertize "loading...\n" 'face 'shadow)))))

(defun ygg-git-compare-job--place ()
  "Put point at the first error of the log of a job that failed, else its first
failed step, else leave it."
  (let ((failed (eq (plist-get (or (plist-get ygg-git-compare-job--details :check)
                                   ygg-git-compare-job--check)
                               :state)
                    'failed)))
    (when (and failed ygg-git-compare-job--log-start)
      (goto-char ygg-git-compare-job--log-start)
      (let ((case-fold-search nil))
        (if (re-search-forward ygg-git-compare-job--error-regexp nil t)
            (beginning-of-line)
          (when-let* ((step (seq-find (lambda (s) (eq (plist-get s :state) 'failed))
                                      (plist-get ygg-git-compare-job--details :steps))))
            (ygg-git-compare-job--goto-group (plist-get step :name))))))))

(defun ygg-git-compare-job--draw ()
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (first (not ygg-git-compare-job--placed)))
    (erase-buffer)
    (setq ygg-git-compare-job--log-start nil)
    (ygg-git-compare-job--insert-header)
    (ygg-git-compare-job--insert-steps)
    (ygg-git-compare-job--insert-log)
    (goto-char (point-min))
    (if (and first ygg-git-compare-job--log
             (or ygg-git-compare-job--details ygg-git-compare-job--details-error))
        (progn (setq ygg-git-compare-job--placed t)
               (ygg-git-compare-job--place))
      (forward-line (1- line)))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window (point)))))

;;; Fetching

(defun ygg-git-compare-job--stop ()
  (when (timerp ygg-git-compare-job--timer) (cancel-timer ygg-git-compare-job--timer))
  (setq ygg-git-compare-job--timer nil))

(defun ygg-git-compare-job--arm (buffer)
  "Ask again after `ygg-git-compare-checks-running-ttl' seconds while the job
runs and BUFFER is on screen."
  (with-current-buffer buffer
    (ygg-git-compare-job--stop)
    (when (and (ygg-git-compare-job--active-p)
               (< ygg-git-compare-job--errors ygg-git-compare--checks-max-errors)
               (get-buffer-window buffer 'visible))
      (setq ygg-git-compare-job--timer
            (run-at-time ygg-git-compare-checks-running-ttl nil
                         #'ygg-git-compare-job--tick buffer)))))

(defun ygg-git-compare-job--tick (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ygg-git-compare-job--timer nil)
      (when (get-buffer-window buffer 'visible)
        (ygg-git-compare-job--fetch buffer)))))

(defun ygg-git-compare-job--landed (buffer generation kind value failure)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation ygg-git-compare-job--generation)
        (let ((pending (and failure (eq kind 'log)
                            (not (eq (plist-get ygg-git-compare-job--pr :forge) 'gitlab))
                            (ygg-git-compare-job--active-p))))
        (pcase kind
          ('details (setq ygg-git-compare-job--details-error failure)
                    (when value (setq ygg-git-compare-job--details value)))
          ('log (setq ygg-git-compare-job--log-error
                      (if pending "not ready yet" failure))
                (when value (setq ygg-git-compare-job--log value))))
        (unless pending
          (setq ygg-git-compare-job--errors (if failure (1+ ygg-git-compare-job--errors) 0)))
        (ygg-git-compare-job--draw)
        (ygg-git-compare-job--arm buffer))))))

(defun ygg-git-compare-job--fetch (buffer)
  "Ask the forge for the job of BUFFER and its log, in the background."
  (with-current-buffer buffer
    (let* ((pr ygg-git-compare-job--pr)
           (id ygg-git-compare-job--id)
           (generation (cl-incf ygg-git-compare-job--generation))
           (details (ygg-git-compare-job--command pr id ""))
           (log (ygg-git-compare-job--command
                 pr id (if (eq (plist-get pr :forge) 'gitlab) "/trace" "/logs"))))
      (ygg-git-compare--forge-async
       (car details) (cadr details)
       (lambda (status text err)
         (ygg-git-compare--answer
          (lambda (value &optional failure)
            (ygg-git-compare-job--landed buffer generation 'details value failure))
          (car details) status text err
          (lambda (text) (ygg-git-compare-job--parse pr text)))))
      (ygg-git-compare--forge-async
       (car log) (cadr log)
       (lambda (status text err)
         (ygg-git-compare--answer
          (lambda (value &optional failure)
            (ygg-git-compare-job--landed buffer generation 'log value failure))
          (car log) status text err
          (lambda (text)
            (let ((capped (ygg-git-compare-job--cap text ygg-git-compare-job-log-max)))
              (list :raw (car capped) :dropped (cdr capped))))))
       ygg-git-compare-job-log-timeout))))

;;; Mode

(defun ygg-git-compare-job-refresh ()
  "Fetch the job and its log again."
  (interactive)
  (setq ygg-git-compare-job--errors 0)
  (ygg-git-compare-job--fetch (current-buffer)))

(defun ygg-git-compare-job-browse ()
  "Open the job in the browser."
  (interactive)
  (browse-url (or (ygg-git-compare-job--url) (user-error "This job has no page"))))

(defun ygg-git-compare-job--url ()
  (or (plist-get ygg-git-compare-job--check :url)
      (plist-get (plist-get ygg-git-compare-job--details :check) :url)))

(defun ygg-git-compare-job-copy ()
  "Copy the address of the job."
  (interactive)
  (let ((url (or (ygg-git-compare-job--url) (user-error "This job has no page"))))
    (kill-new url)
    (message "Copied: %s" url)))

(defun ygg-git-compare-job-toggle-timestamps ()
  "Show or leave out the timestamps GitHub puts on each log line."
  (interactive)
  (setq ygg-git-compare-job--strip (not ygg-git-compare-job--strip))
  (ygg-git-compare-job--draw))

(defun ygg-git-compare-job--error-line (direction)
  (unless ygg-git-compare-job--log-start (user-error "No log"))
  (let ((case-fold-search nil)
        (origin (point)))
    (if (> direction 0)
        (progn (if (< (point) ygg-git-compare-job--log-start)
                   (goto-char ygg-git-compare-job--log-start)
                 (end-of-line))
               (if (re-search-forward ygg-git-compare-job--error-regexp nil t)
                   (beginning-of-line)
                 (goto-char origin)
                 (user-error "No more errors")))
      (beginning-of-line)
      (if (and (>= (point) ygg-git-compare-job--log-start)
               (re-search-backward ygg-git-compare-job--error-regexp
                                   ygg-git-compare-job--log-start t))
          (beginning-of-line)
        (goto-char origin)
        (user-error "No earlier errors")))))

(defun ygg-git-compare-job-next-error ()
  "Move to the next error line of the log."
  (interactive)
  (ygg-git-compare-job--error-line 1))

(defun ygg-git-compare-job-previous-error ()
  "Move to the previous error line of the log."
  (interactive)
  (ygg-git-compare-job--error-line -1))

(defun ygg-git-compare-job--goto-group (name)
  "Move to the log's group NAME, or to the one it begins; nil if none."
  (when ygg-git-compare-job--log-start
    (let ((pos ygg-git-compare-job--log-start)
          (end (point-max))
          found)
      (while (and (not found) pos (< pos end))
        (let ((group (get-text-property pos 'ygg-git-compare-job-group)))
          (if (and group (or (equal group name) (string-prefix-p name group)))
              (setq found pos)
            (setq pos (next-single-property-change pos 'ygg-git-compare-job-group nil end)))))
      (when found (goto-char found)))))

(defun ygg-git-compare-job-visit ()
  "Jump to the log of the step at point."
  (interactive)
  (let ((name (get-text-property (line-beginning-position) 'ygg-git-compare-job-step)))
    (unless name (user-error "No step here"))
    (unless (ygg-git-compare-job--goto-group name)
      (user-error "No log for this step"))))

(defun ygg-git-compare-job-quit ()
  "Close the job and its buffer."
  (interactive)
  (quit-window t))

(defvar-keymap ygg-git-compare-job--keys
  "g" #'ygg-git-compare-job-refresh
  "o" #'ygg-git-compare-job-browse
  "y" #'ygg-git-compare-job-copy
  "q" #'ygg-git-compare-job-quit
  "t" #'ygg-git-compare-job-toggle-timestamps
  "] e" #'ygg-git-compare-job-next-error
  "[ e" #'ygg-git-compare-job-previous-error
  "RET" #'ygg-git-compare-job-visit
  "<return>" #'ygg-git-compare-job-visit)

(defvar-keymap ygg-git-compare-job-mode-map
  :parent special-mode-map
  :keymap ygg-git-compare-job--keys)

(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-git-compare-job-mode)
  (yggdrasil-define-mode-keys 'ygg-git-compare-job-mode 'normal ygg-git-compare-job--keys))

(define-derived-mode ygg-git-compare-job-mode special-mode "Job"
  "A CI job's header, steps and log."
  (setq-local revert-buffer-function (lambda (&rest _) (ygg-git-compare-job-refresh)))
  (setq-local truncate-lines t)
  (add-hook 'kill-buffer-hook #'ygg-git-compare-job--stop nil t))

;;; Opening

(defun ygg-git-compare-job-show (pr check)
  "Show CHECK of the pull request PR in a window beside the compare and fetch
it; nil when it names no job."
  (when-let* ((id (ygg-git-compare-job-id pr (plist-get check :url))))
    (let* ((dir default-directory)
           (buffer (get-buffer-create
                    (format "*ygg-job: %s #%s*" (plist-get check :name) id))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'ygg-git-compare-job-mode)
          (ygg-git-compare-job-mode))
        (setq default-directory dir
              ygg-git-compare-job--pr pr
              ygg-git-compare-job--id id
              ygg-git-compare-job--check check
              ygg-git-compare-job--strip ygg-git-compare-job-strip-timestamps
              ygg-git-compare-job--placed nil
              ygg-git-compare-job--details nil
              ygg-git-compare-job--log nil)
        (ygg-git-compare-job--draw))
      (when-let* ((window (display-buffer
                           buffer '((display-buffer-reuse-window display-buffer-in-side-window)
                                    (side . bottom) (slot . 0) (window-height . 0.4)))))
        (select-window window))
      (ygg-git-compare-job--fetch buffer)
      buffer)))

(provide 'ygg-git-compare-job)
;;; ygg-git-compare-job.el ends here
