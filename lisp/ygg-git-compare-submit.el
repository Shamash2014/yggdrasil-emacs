;;; ygg-git-compare-submit.el --- send a compare's review comments, or export them -*- lexical-binding: t; -*-

;;; Commentary:
;; The held review comments of a compare, at line, range, file or review
;; level and typed or not, sent where the work goes on: a GitHub review or
;; GitLab notes with an event (comment, approve, request changes, draft),
;; an agent, or markdown as tuicr writes it, copied, shown or saved.
;; Only what was delivered is dropped; a pending agent comment never leaves.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'transient)
(require 'ygg-git-compare)

(defvar transient--original-buffer)
(declare-function ygg-git-compare-comments-list "ygg-git-compare-comments"
                  (&optional include-pending))
(declare-function ygg-git-compare-comments-drop "ygg-git-compare-comments" (ids))
(declare-function ygg-git-compare--forge-p "ygg-git-compare-comments" (comment))
(declare-function aob-trace "aob-trace")

(defcustom ygg-git-compare-export-intro
  "I reviewed your code and have the following comments. Please address them."
  "The line a markdown export opens with; empty leaves it out."
  :type 'string :group 'ygg-git-compare)

(defcustom ygg-git-compare-export-quote nil
  "Whether a markdown export quotes the lines under each comment."
  :type 'boolean :group 'ygg-git-compare)

(defcustom ygg-git-compare-submit-timeout 60
  "Seconds each call to the forge may take before it is given up."
  :type 'number :group 'ygg-git-compare)

(defvar-local ygg-git-compare-submit--busy nil
  "Whether a review is being posted from this compare.")

(defcustom ygg-git-compare-submit-type-format "**%s:** "
  "How a comment's type leads its text on a pull request."
  :type 'string :group 'ygg-git-compare)

;;; Which comments

(defun ygg-git-compare-submit--pending-p (comment)
  (eq (plist-get comment :status) 'pending))

(defun ygg-git-compare-submit--pending-count ()
  (seq-count #'ygg-git-compare-submit--pending-p (ygg-git-compare-comments-list t)))

(defun ygg-git-compare-submit--pending-note ()
  (let ((n (ygg-git-compare-submit--pending-count)))
    (if (zerop n) ""
      (format "%d pending agent comment%s not included — check them first"
              n (if (= n 1) "" "s")))))

(defun ygg-git-compare-submit--within-p (comment max-priority)
  "Whether COMMENT is as urgent as MAX-PRIORITY: P0 to it, or the user's own
without one.  Every comment is when MAX-PRIORITY is nil."
  (let ((priority (plist-get comment :priority)))
    (or (null max-priority)
        (if priority (<= priority max-priority) (null (plist-get comment :author))))))

(defun ygg-git-compare-submit--select (&optional to max-priority)
  "The checked comments going TO `forge' or `agent', all when nil, oldest
first, those past MAX-PRIORITY left out."
  (seq-filter (lambda (c) (and (not (ygg-git-compare-submit--pending-p c))
                               (pcase to
                                 ('forge (ygg-git-compare--forge-p c))
                                 ('agent (not (ygg-git-compare--forge-p c)))
                                 (_ t))
                               (ygg-git-compare-submit--within-p c max-priority)))
              (ygg-git-compare-comments-list)))

(defun ygg-git-compare-submit--args ()
  "The submit menu's filter as (MAX-PRIORITY), nil when unset."
  (let ((max (transient-arg-value "--max-priority=" (transient-args 'ygg-git-compare-submit))))
    (list (and max (string-to-number max)))))

(defun ygg-git-compare-submit--level (comment)
  (or (plist-get comment :level) (if (plist-get comment :file) 'line 'review)))

;;; Markdown

(defun ygg-git-compare-submit--location (comment)
  (let* ((path (plist-get comment :file))
         (old (if (eq (plist-get comment :side) 'old) "~" ""))
         (start-old (if (eq (plist-get comment :start-side) 'old) "~" "")))
    (pcase (ygg-git-compare-submit--level comment)
      ('review "`Review comment`")
      ('file (format "`%s`" path))
      ('range (format "`%s:%s%d-%s%d`" path start-old (plist-get comment :start-line)
                      old (plist-get comment :line)))
      (_ (format "`%s:%s%d`" path old (plist-get comment :line))))))

(defun ygg-git-compare-submit--order (comments)
  "COMMENTS review level first, then by path, file level before its lines."
  (let ((review (seq-filter (lambda (c) (eq (ygg-git-compare-submit--level c) 'review))
                            comments)))
    (append review
            (sort (seq-difference comments review #'eq)
                  (lambda (a b)
                    (let ((pa (plist-get a :file)) (pb (plist-get b :file)))
                      (if (equal pa pb)
                          (< (or (plist-get a :line) 0) (or (plist-get b :line) 0))
                        (string< pa pb))))))))

(defun ygg-git-compare-submit--finding (comment)
  "COMMENT's priority and title as [P1] title, or nil without either."
  (let ((priority (plist-get comment :priority))
        (title (plist-get comment :title)))
    (when (or priority title)
      (string-join (delq nil (list (and priority (format "[P%d]" priority)) title)) " "))))

(defun ygg-git-compare-submit--verdict (comment)
  (when-let* ((correctness (plist-get comment :correctness)))
    (concat "Verdict: " correctness
            (when-let* ((confidence (plist-get comment :confidence)))
              (format " (confidence %s)" confidence)))))

(defun ygg-git-compare-submit--item (n comment)
  (let* ((marker (format "%d." n))
         (indent (make-string (1+ (length marker)) ?\s))
         (lines (split-string (plist-get comment :text) "\r?\n"))
         (type (plist-get comment :type))
         (author (plist-get comment :author))
         (quote (plist-get comment :quote)))
    (concat marker " "
            (when-let* ((finding (ygg-git-compare-submit--finding comment)))
              (format "**%s** " finding))
            (when type (format "**[%s]** " (upcase (format "%s" type))))
            (ygg-git-compare-submit--location comment)
            " - " (car lines)
            (when author (concat " — by " author))
            "\n"
            (mapconcat (lambda (l) (concat indent l "\n")) (cdr lines))
            (when (and ygg-git-compare-export-quote quote
                       (not (string-empty-p quote)))
              (mapconcat (lambda (l) (concat indent l "\n"))
                         (append '("```diff") (split-string quote "\n") '("```")))))))

(defun ygg-git-compare-markdown (comments)
  "COMMENTS as a numbered markdown review, the way tuicr exports one.
Pending agent comments are left out."
  (let* ((comments (ygg-git-compare-submit--order
                    (seq-remove #'ygg-git-compare-submit--pending-p comments)))
         (ranges (delete-dups (delq nil (mapcar (lambda (c) (plist-get c :range))
                                                comments))))
         (n 0))
    (concat
     (unless (string-empty-p ygg-git-compare-export-intro)
       (concat ygg-git-compare-export-intro "\n\n"))
     (when (= (length ranges) 1) (format "Reviewing %s\n\n" (car ranges)))
     (mapconcat (lambda (c) (when-let* ((verdict (ygg-git-compare-submit--verdict c)))
                              (concat verdict "\n\n")))
                comments)
     (mapconcat (lambda (c)
                  (unless (and (ygg-git-compare-submit--verdict c)
                               (string-empty-p (string-trim (or (plist-get c :text) ""))))
                    (ygg-git-compare-submit--item (cl-incf n) c)))
                comments))))

(defun ygg-git-compare-markdown-for-range (dir range-key)
  "The markdown of the comments kept on disk in DIR's repository under
RANGE-KEY, such as \"branch NAME\", for batch use and agents."
  (let* ((default-directory dir)
         (file (expand-file-name "ygg-review-comments.eld"
                                 (or (magit-gitdir nil t)
                                     (error "Not in a git repository: %s" dir))))
         (alist (and (file-readable-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (ignore-errors (read (current-buffer)))))))
    (ygg-git-compare-markdown (cdr (assoc range-key alist)))))

(defun ygg-git-compare-submit--export-text (max-priority)
  (with-current-buffer (ygg-git-compare--list)
    (let ((comments (or (ygg-git-compare-submit--select nil max-priority)
                        (user-error "No review comments to export"))))
      (cons (length comments) (ygg-git-compare-markdown comments)))))

(defun ygg-git-compare-export-markdown (&optional show)
  "Copy the review comments as markdown; with SHOW, a prefix, show them."
  (interactive "P")
  (pcase-let ((`(,n . ,text) (apply #'ygg-git-compare-submit--export-text
                                    (ygg-git-compare-submit--args))))
    (if show
        (with-current-buffer (get-buffer-create "*ygg-review-markdown*")
          (let ((inhibit-read-only t)) (erase-buffer) (insert text))
          (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))
      (kill-new text)
      (message "%d review comment%s copied as markdown" n (if (= n 1) "" "s")))))

(defun ygg-git-compare-export-markdown-file (file)
  "Write the review comments as markdown to FILE."
  (interactive
   (list (read-file-name "Write review to: " (magit-toplevel) nil nil "REVIEW.md")))
  (pcase-let ((`(,n . ,text) (apply #'ygg-git-compare-submit--export-text
                                    (ygg-git-compare-submit--args))))
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil file))
    (message "%d review comment%s written to %s" n (if (= n 1) "" "s")
             (abbreviate-file-name file))))

;;; Forge bodies

(defun ygg-git-compare-submit--text (comment)
  (string-trim-right
   (concat (when-let* ((verdict (ygg-git-compare-submit--verdict comment)))
            (concat verdict "\n\n"))
          (when-let* ((finding (ygg-git-compare-submit--finding comment)))
            (concat finding "\n\n"))
          (when-let* ((type (plist-get comment :type)))
            (format ygg-git-compare-submit-type-format type))
          (plist-get comment :text))))

(defun ygg-git-compare-submit--summary (comments)
  "The review-level COMMENTS, then the file-level ones as `path' - text."
  (string-join
   (delq nil
         (mapcar (lambda (c)
                   (pcase (ygg-git-compare-submit--level c)
                     ('review (ygg-git-compare-submit--text c))
                     ('file (format "`%s` - %s" (plist-get c :file)
                                    (ygg-git-compare-submit--text c)))))
                 comments))
   "\n\n"))

(defun ygg-git-compare-submit--inline-p (comment)
  (memq (ygg-git-compare-submit--level comment) '(line range)))

(defun ygg-git-compare-submit--github-side (side)
  (if (eq side 'old) "LEFT" "RIGHT"))

(defun ygg-git-compare-submit--github-comment (comment)
  (append (list :path (plist-get comment :new-path)
                :line (plist-get comment :line)
                :side (ygg-git-compare-submit--github-side (plist-get comment :side))
                :body (ygg-git-compare-submit--text comment))
          (when (eq (ygg-git-compare-submit--level comment) 'range)
            (list :start_line (plist-get comment :start-line)
                  :start_side (ygg-git-compare-submit--github-side
                               (plist-get comment :start-side))))))

(defconst ygg-git-compare-submit--github-events
  '((comment . "COMMENT") (approve . "APPROVE") (request-changes . "REQUEST_CHANGES")))

(defun ygg-git-compare-submit--github-review (pr event comments)
  "The body of PR's review with EVENT of COMMENTS; no event leaves it pending."
  (let* ((event-name (alist-get event ygg-git-compare-submit--github-events))
         (summary (ygg-git-compare-submit--summary comments))
         (body (if (and (string-empty-p summary) (memq event '(comment request-changes)))
                   (format "%d comment%s" (length comments)
                           (if (= (length comments) 1) "" "s"))
                 summary)))
    (append (list :commit_id (plist-get pr :head))
            (and event-name (list :event event-name))
            (unless (string-empty-p body) (list :body body))
            (list :comments (vconcat (mapcar #'ygg-git-compare-submit--github-comment
                                             (seq-filter #'ygg-git-compare-submit--inline-p
                                                         comments)))))))

(defun ygg-git-compare-submit--api (program body args done)
  "Run PROGRAM with ARGS without waiting, BODY, when non-nil, posted as JSON
through api's --input; call DONE with (VALUE ERR REFUSED): what it printed
read as JSON, or nil and what went wrong, REFUSED when the forge said no."
  (let ((input (and body (let ((coding-system-for-write 'utf-8))
                           (make-temp-file "ygg-git-compare-body-" nil ".json"
                                           (json-serialize body))))))
    (condition-case failure
        (ygg-git-compare--forge-async
         program
         (append args (and input (list "--method" "POST"
                                       "--header" "Content-Type: application/json"
                                       "--input" input)))
         (lambda (status text err)
           (when input (ignore-errors (delete-file input)))
           (cond ((null status) (funcall done nil (format "%s not found" program)))
                 ((eq status 'timeout) (funcall done nil (format "%s timed out" program)))
                 ((/= status 0)
                  (funcall done nil (if (string-empty-p err) (format "%s failed" program) err) t))
                 (t (let ((value (condition-case nil
                                     (let ((out (string-trim text)))
                                       (list (unless (string-empty-p out)
                                               (ygg-git-compare--json out))))
                                   (error nil))))
                      (if value
                          (funcall done (car value) nil)
                        (funcall done nil (format "%s answered nothing readable" program)))))))
         ygg-git-compare-submit-timeout)
      (error (when input (ignore-errors (delete-file input)))
             (signal (car failure) (cdr failure))))))

(defun ygg-git-compare-submit--github-steps (pr event comments)
  (list (list comments nil
              (lambda (next)
                (ygg-git-compare-submit--api
                 "gh" (ygg-git-compare-submit--github-review pr event comments)
                 (list "api" "--hostname" (plist-get pr :host)
                       (format "repos/%s/pulls/%s/reviews"
                               (plist-get pr :path) (plist-get pr :number)))
                 (lambda (_value err &rest _) (funcall next err)))))))

(defun ygg-git-compare-submit--line-code (path side line old-line)
  "GitLab's code for the diff LINE of PATH on SIDE, OLD-LINE a context line's
old number, and the line's type, nil for context."
  (let ((hash (sha1 path)))
    (cond ((eq side 'old) (list (format "%s_%d_0" hash line) "old"))
          (old-line (list (format "%s_%d_%d" hash old-line line) nil))
          (t (list (format "%s_0_%d" hash line) "new")))))

(defun ygg-git-compare-submit--gitlab-endpoint (path side line old-line)
  (pcase-let ((`(,code ,type) (ygg-git-compare-submit--line-code path side line old-line)))
    (append (list :line_code code) (and type (list :type type)))))

(defun ygg-git-compare-submit--context-old-line (diff line)
  "The old number of the context line of DIFF, a file's diff lines, that is
new LINE; nil when LINE is not a context line there."
  (let (old new found)
    (dolist (text diff)
      (cond ((string-match "\\`@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" text)
             (setq old (string-to-number (match-string 1 text))
                   new (string-to-number (match-string 2 text))))
            ((null old))
            ((string-prefix-p "-" text) (cl-incf old))
            ((string-prefix-p "+" text) (cl-incf new))
            ((string-prefix-p " " text)
             (when (eql new line) (setq found old))
             (cl-incf old)
             (cl-incf new))))
    found))

(defun ygg-git-compare-submit--with-old-lines (comment pr)
  "COMMENT, the old numbers of the context lines it sits on added from PR's diff."
  (let (diff)
    (cl-flet ((old-line (side line known)
                (or known
                    (and line (not (eq side 'old))
                         (ygg-git-compare-submit--context-old-line
                          (or diff
                              (setq diff (ignore-errors
                                           (magit-git-lines
                                            "diff" "--no-color" "--no-ext-diff"
                                            (plist-get pr :base) (plist-get pr :head) "--"
                                            (plist-get comment :new-path)))))
                          line)))))
      (append (list :old-line (old-line (plist-get comment :side) (plist-get comment :line)
                                        (plist-get comment :old-line))
                    :start-old-line (old-line (plist-get comment :start-side)
                                              (plist-get comment :start-line)
                                              (plist-get comment :start-old-line)))
              comment))))

(defun ygg-git-compare-submit--gitlab-position (comment pr &optional single)
  "COMMENT's position on PR, a range spanning its lines unless SINGLE."
  (let* ((comment (ygg-git-compare-submit--with-old-lines comment pr))
         (position (ygg-git-compare--gitlab-position comment pr))
        (path (plist-get comment :new-path)))
    (if (or single (not (eq (ygg-git-compare-submit--level comment) 'range)))
        position
      (append position
              (list :line_range
                    (list :start (ygg-git-compare-submit--gitlab-endpoint
                                  path (plist-get comment :start-side)
                                  (plist-get comment :start-line)
                                  (plist-get comment :start-old-line))
                          :end (ygg-git-compare-submit--gitlab-endpoint
                                path (plist-get comment :side) (plist-get comment :line)
                                (plist-get comment :old-line))))))))

(defun ygg-git-compare-submit--range-prefix (comment)
  (format "L%d–%d: " (plist-get comment :start-line) (plist-get comment :line)))

(defun ygg-git-compare-submit--gitlab-api (pr body path-and-args done)
  (ygg-git-compare-submit--api
   "glab" body (append (list "api" "--hostname" (plist-get pr :host)) path-and-args) done))

(defun ygg-git-compare-submit--gitlab-mr (pr what)
  (format "projects/%s/merge_requests/%s/%s"
          (url-hexify-string (plist-get pr :path)) (plist-get pr :number) what))

(defun ygg-git-compare-submit--gitlab-note (pr draft text position done)
  (ygg-git-compare-submit--gitlab-api
   pr (append (list (if draft :note :body) text) (and position (list :position position)))
   (list (ygg-git-compare-submit--gitlab-mr pr (cond (draft "draft_notes")
                                                     (position "discussions")
                                                     (t "notes"))))
   done))

(defun ygg-git-compare-submit--gitlab-inline (pr draft comment next)
  "Post COMMENT on its lines, then call NEXT with what went wrong, if
anything; a range GitLab refuses goes on its last line."
  (let ((text (ygg-git-compare-submit--text comment)))
    (ygg-git-compare-submit--gitlab-note
     pr draft text (ygg-git-compare-submit--gitlab-position comment pr)
     (lambda (_value err &optional refused)
       (if (and err refused (eq (ygg-git-compare-submit--level comment) 'range))
           (ygg-git-compare-submit--gitlab-note
            pr draft (concat (ygg-git-compare-submit--range-prefix comment) text)
            (ygg-git-compare-submit--gitlab-position comment pr t)
            (lambda (_value err &rest _) (funcall next err)))
         (funcall next err))))))

(defconst ygg-git-compare-submit--request-changes-query
  "mutation($projectPath: ID!, $iid: String!) { mergeRequestRequestChanges(input: { projectPath: $projectPath, iid: $iid }) { errors } }")

(defun ygg-git-compare-submit--gitlab-request-changes (pr next)
  (ygg-git-compare-submit--gitlab-api
   pr nil
   (list "graphql"
         "-f" (concat "query=" ygg-git-compare-submit--request-changes-query)
         "-f" (concat "projectPath=" (plist-get pr :path))
         "-f" (format "iid=%s" (plist-get pr :number)))
   (lambda (out err &rest _)
     (let ((errors (append (plist-get out :errors)
                           (plist-get (plist-get (plist-get out :data)
                                                 :mergeRequestRequestChanges)
                                      :errors))))
       (funcall next
                (or err
                    (when errors
                      (format "glab graphql: %s"
                              (mapconcat (lambda (e) (if (stringp e) e (or (plist-get e :message)
                                                                           (format "%S" e))))
                                         errors "; ")))))))))

(defun ygg-git-compare-submit--gitlab-steps (pr event comments)
  "The notes that post COMMENTS to PR, drafts when EVENT is draft, and a
function of the first failure for the approval or request for changes."
  (let* ((draft (eq event 'draft))
         (summary-comments (seq-remove #'ygg-git-compare-submit--inline-p comments))
         (steps (append
                 (when summary-comments
                   (list (list summary-comments nil
                               (lambda (next)
                                 (ygg-git-compare-submit--gitlab-note
                                  pr draft (ygg-git-compare-submit--summary summary-comments)
                                  nil (lambda (_value err &rest _) (funcall next err)))))))
                 (mapcar (lambda (c)
                           (list (list c) nil
                                 (lambda (next)
                                   (ygg-git-compare-submit--gitlab-inline pr draft c next))))
                         (seq-filter #'ygg-git-compare-submit--inline-p comments)))))
    (cons steps
          (lambda (failure)
            (unless failure
              (pcase event
                ('approve
                 (list (list nil "Approving"
                             (lambda (next)
                               (ygg-git-compare-submit--gitlab-api
                                pr nil (list "--method" "POST"
                                             (ygg-git-compare-submit--gitlab-mr pr "approve"))
                                (lambda (_value err &rest _) (funcall next err)))))))
                ('request-changes
                 (list (list nil "Requesting changes"
                             (lambda (next)
                               (ygg-git-compare-submit--gitlab-request-changes pr next)))))))))))

;;; Submitting

(defun ygg-git-compare-submit--sequence (steps final progress done)
  "Run STEPS, each (COMMENTS LABEL RUN), one after another, RUN being called
with a function taking what went wrong, if anything; a failure does not
stop the rest.  Then the steps FINAL, a function of the first failure,
gives.  PROGRESS is called with the comments handled and a step's LABEL
before each; DONE with the comments posted and the first failure."
  (let ((posted nil) (failure nil) (handled 0))
    (letrec ((advance
              (lambda (queue)
                (if (null queue)
                    (let ((more (and final (prog1 (funcall final failure) (setq final nil)))))
                      (if more
                          (funcall advance more)
                        (funcall done posted failure)))
                  (pcase-let ((`(,comments ,label ,run) (car queue))
                              (over nil))
                    (let ((next (lambda (&optional err)
                                  (unless over
                                    (setq over t)
                                    (if err
                                        (unless failure (setq failure err))
                                      (setq posted (append posted comments)))
                                    (cl-incf handled (length comments))
                                    (funcall advance (cdr queue))))))
                      (funcall progress handled label)
                      (condition-case err
                          (funcall run next)
                        (error (if over
                                   (signal (car err) (cdr err))
                                 (funcall next (error-message-string err))))))))))) 
      (funcall advance steps))))

(defconst ygg-git-compare-submit--event-past
  '((comment . "Posted") (approve . "Posted") (request-changes . "Posted") (draft . "Drafted")))

(defconst ygg-git-compare-submit--event-also
  '((approve . " and approved it") (request-changes . " and requested changes")))

(declare-function ygg-git-compare--remote-key "ygg-git-compare-threads" (pr))

(defun ygg-git-compare-submit--say (buffer text)
  (message "%s" text)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ignore-errors (ygg-git-compare--review-note buffer text)))))

(defun ygg-git-compare-submit--settle (buffer name pr event comments posted failure prior)
  "Drop what was POSTED of COMMENTS and say how it went."
  (let* ((n (length comments))
         (posted (seq-filter (lambda (c) (memq c posted)) comments))
         (kept (- n (length posted)))
         (text (cond ((and failure (> kept 0))
                      (format "Posted %d of %d; %d kept: %s" (length posted) n kept failure))
                     (failure
                      (format "Posted %d comment%s to %s, then failed: %s" n (if (= n 1) "" "s")
                              name failure))
                     ((zerop n) (format "Approved %s" name))
                     (t (format "%s %d comment%s to %s%s (%s)"
                                (alist-get event ygg-git-compare-submit--event-past)
                                n (if (= n 1) "" "s") name
                                (alist-get event ygg-git-compare-submit--event-also "")
                                (plist-get pr :url))))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq ygg-git-compare-submit--busy nil)
        (ygg-git-compare-submit--drop posted)
        (when (and posted (fboundp 'ygg-git-compare--remote-key))
          (ignore-errors (ygg-git-compare--cache-drop (ygg-git-compare--remote-key pr))))))
    (ygg-git-compare-submit--say buffer text)
    (run-at-time 8 nil
                 (lambda ()
                   (when (and (buffer-live-p buffer)
                              (eq (buffer-local-value 'ygg-git-compare--note buffer) text))
                     (ygg-git-compare--review-note buffer prior))))))

(defconst ygg-git-compare-submit--event-names
  '((comment . "Comment with") (approve . "Approve with")
    (request-changes . "Request changes with") (draft . "Draft")))

(defun ygg-git-compare-submit--drop (comments)
  (when comments
    (ygg-git-compare-comments-drop (mapcar (lambda (c) (plist-get c :id)) comments))))

(defun ygg-git-compare-submit--held (event max-priority)
  "The comments for the pull request, a user error when there is nothing to
send or one was made on another range."
  (let* ((comments (ygg-git-compare-submit--select 'forge max-priority))
         (here (ygg-git-compare--range-label))
         (stale (seq-find (lambda (c) (and (plist-get c :range)
                                           (not (equal (plist-get c :range) here))))
                          comments)))
    (unless (or comments (eq event 'approve))
      (user-error "No comments held for the pull request"))
    (when stale
      (user-error "A comment was made on %s; drop it or send it to an agent"
                  (plist-get stale :range)))
    comments))

(defun ygg-git-compare-submit--post (event max-priority pr)
  "Ask, then post the comments to PR in the background."
  (when ygg-git-compare-submit--busy
    (user-error "A review is already being posted; wait for it to land"))
  (let* ((comments (ygg-git-compare-submit--held event max-priority))
         (name (ygg-git-compare--pr-name pr))
         (n (length comments))
         (pending (ygg-git-compare-submit--pending-note))
         (buffer (current-buffer))
         (prior ygg-git-compare--note))
    (unless (y-or-n-p (format "%s %d comment%s on %s%s? "
                              (alist-get event ygg-git-compare-submit--event-names)
                              n (if (= n 1) "" "s") name
                              (if (string-empty-p pending) "" (concat " (" pending ")"))))
      (user-error "Nothing sent"))
    (setq ygg-git-compare-submit--busy t)
    (condition-case failure
        (pcase-let* ((gitlab (eq (plist-get pr :forge) 'gitlab))
                     (`(,steps . ,final)
                      (if gitlab
                          (ygg-git-compare-submit--gitlab-steps pr event comments)
                        (cons (ygg-git-compare-submit--github-steps pr event comments) nil))))
          (ygg-git-compare-submit--sequence
           steps final
           (lambda (handled label)
             (ygg-git-compare-submit--say
              buffer (cond (label (format "%s…" label))
                           (gitlab (format "Posting %d/%d…" (min n (1+ handled)) n))
                           (t (format "Posting %d comment%s to %s…" n (if (= n 1) "" "s")
                                      name)))))
           (lambda (posted failure)
             (ygg-git-compare-submit--settle buffer name pr event comments posted failure
                                             prior))))
      (error (when (buffer-live-p buffer)
               (with-current-buffer buffer (setq ygg-git-compare-submit--busy nil)))
             (signal (car failure) (cdr failure))))))

(defun ygg-git-compare-submit-forge (event &optional max-priority)
  "Send the comments for the pull or merge request as a review with EVENT:
comment, approve, request-changes or draft.  The forge is asked in the
background, the pull request first when it is not known yet.
MAX-PRIORITY leaves out findings less urgent than it."
  (with-current-buffer (ygg-git-compare--list)
    (when ygg-git-compare-submit--busy
      (user-error "A review is already being posted; wait for it to land"))
    (ygg-git-compare-submit--held event max-priority)
    (ygg-git-compare--this-pr
     (lambda (pr) (ygg-git-compare-submit--post event max-priority pr)))))

(defun ygg-git-compare-submit--agent-prompt (comments)
  (concat ygg-git-compare-review-instructions
          "\n\n<review-comments>\n" (ygg-git-compare-markdown comments) "</review-comments>\n\n"
          (ygg-git-compare-compare-block)))

(defun ygg-git-compare-submit-agent (&optional max-priority)
  "Send the agent-typed comments with the compare, written as the markdown
export writes them.  MAX-PRIORITY leaves
out findings less urgent than it."
  (require 'aob)
  (require 'aob-acp)
  (with-current-buffer (ygg-git-compare--list)
    (let* ((comments (ygg-git-compare-submit--select 'agent max-priority))
           (_ (unless comments (user-error "No comments held for an agent")))
           (text (ygg-git-compare-submit--agent-prompt comments))
           (session (ygg-git-compare-send-to-reviewer default-directory text)))
      (ygg-git-compare-submit--drop comments)
      (message "%d comment%s sent to an agent%s" (length comments)
               (if (= (length comments) 1) "" "s")
               (let ((pending (ygg-git-compare-submit--pending-note)))
                 (if (string-empty-p pending) "" (concat "; " pending))))
      (aob-trace session))))

;;; Menu

(defun ygg-git-compare-submit-comment ()
  "Post the comments as a review that comments."
  (interactive)
  (apply #'ygg-git-compare-submit-forge 'comment (ygg-git-compare-submit--args)))

(defun ygg-git-compare-submit-approve ()
  "Post the comments as a review that approves."
  (interactive)
  (apply #'ygg-git-compare-submit-forge 'approve (ygg-git-compare-submit--args)))

(defun ygg-git-compare-submit-request-changes ()
  "Post the comments as a review that requests changes."
  (interactive)
  (apply #'ygg-git-compare-submit-forge 'request-changes (ygg-git-compare-submit--args)))

(defun ygg-git-compare-submit-draft ()
  "Post the comments as a pending review, or draft notes, to submit there."
  (interactive)
  (apply #'ygg-git-compare-submit-forge 'draft (ygg-git-compare-submit--args)))

(defun ygg-git-compare-submit-to-agent ()
  "Send the comments and the compare to an agent."
  (interactive)
  (apply #'ygg-git-compare-submit-agent (ygg-git-compare-submit--args)))

(defun ygg-git-compare-export-markdown-buffer ()
  "Show the review comments as markdown in a buffer."
  (interactive)
  (ygg-git-compare-export-markdown t))

(defun ygg-git-compare-submit--description ()
  (with-current-buffer (if (buffer-live-p (bound-and-true-p transient--original-buffer))
                           transient--original-buffer
                         (current-buffer))
    (or (ignore-errors
          (with-current-buffer (ygg-git-compare--list)
            (let* ((comments (ygg-git-compare-submit--select nil))
                   (forge (seq-count (lambda (c) (ygg-git-compare--forge-p c)) comments))
                   (pending (ygg-git-compare-submit--pending-note)))
              (concat (format "Review comments: %d for the pull request, %d for an agent"
                              forge (- (length comments) forge))
                      (unless (string-empty-p pending)
                        (concat "\n" (propertize pending 'face 'warning)))))))
        "Review comments")))

;;;###autoload (autoload 'ygg-git-compare-submit "ygg-git-compare-submit" nil t)
(transient-define-prefix ygg-git-compare-submit ()
  "Send or export the compare's review comments."
  [:description ygg-git-compare-submit--description
   ("-p" "Only findings up to priority" "--max-priority=" :choices ("0" "1" "2" "3"))]
  [["Pull request"
    ("c" "Comment" ygg-git-compare-submit-comment)
    ("a" "Approve" ygg-git-compare-submit-approve)
    ("r" "Request changes" ygg-git-compare-submit-request-changes)
    ("d" "Draft" ygg-git-compare-submit-draft)]
   ["Agent"
    ("@" "Send to agent" ygg-git-compare-submit-to-agent)]
   ["Markdown"
    ("y" "Copy" ygg-git-compare-export-markdown)
    ("b" "Show in buffer" ygg-git-compare-export-markdown-buffer)
    ("w" "Write file" ygg-git-compare-export-markdown-file)]])

(provide 'ygg-git-compare-submit)
;;; ygg-git-compare-submit.el ends here
