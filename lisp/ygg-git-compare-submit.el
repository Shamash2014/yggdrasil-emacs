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
(require 'ygg-git-compare-comments)

(defvar transient--original-buffer)
(declare-function ygg-git-compare-comments-list "ygg-git-compare-comments"
                  (&optional include-pending))
(declare-function ygg-git-compare-comments-drop "ygg-git-compare-comments" (ids))

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

(defvar ygg-git-compare-markdown-with-threads nil
  "Whether the markdown puts the thread a comment was made from under it.")

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
                    (let ((item (ygg-git-compare-submit--item (cl-incf n) c)))
                      (if-let* ((thread (and ygg-git-compare-markdown-with-threads
                                             (ygg-git-compare--discussion-source c))))
                          (concat (string-trim-right item "\n+") "\n\n<thread>\n" thread
                                  "\n</thread>\n\n")
                        item))))
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

(defun ygg-git-compare-submit--written-head (comment)
  "The commit COMMENT's range ends at, nil when it ends at a checkout."
  (when-let* ((range (plist-get comment :range))
              (end (car (last (split-string range))))
              ((string-match "\\.\\.\\.?\\([0-9a-f]\\{40\\}\\)\\'" end)))
    (match-string 1 end)))

(defvar ygg-git-compare-submit--memo nil
  "What git said of PR diffs since this submit began, by question; nil outside one.")

(defun ygg-git-compare-submit--memoized (key fn)
  "What FN answers, asked once per KEY in a submit."
  (if (not ygg-git-compare-submit--memo)
      (funcall fn)
    (let ((hit (gethash key ygg-git-compare-submit--memo 'none)))
      (if (eq hit 'none)
          (puthash key (funcall fn) ygg-git-compare-submit--memo)
        hit))))

(defun ygg-git-compare-submit--diff-lines (pr path &optional old-path)
  "The lines of PR's diff of PATH, renamed from OLD-PATH, with three lines of
context."
  (ygg-git-compare-submit--memoized
   (list 'lines (magit-toplevel) (plist-get pr :base) (plist-get pr :head) path old-path)
   (lambda ()
     (magit-git-lines "diff" "--no-color" "--no-ext-diff" "-U3"
                      (plist-get pr :base) (plist-get pr :head) "--" path old-path))))

(defun ygg-git-compare-submit--forge-hunks (pr path &optional old-path)
  "The (OLD-START OLD-LEN NEW-START NEW-LEN) of each hunk of PR's diff of PATH,
renamed from OLD-PATH, as the forge draws it, with three lines of context;
`unknown' when git cannot say."
  (ygg-git-compare-submit--memoized
   (list 'hunks (magit-toplevel) (plist-get pr :base) (plist-get pr :head) path old-path)
   (lambda ()
     (let ((lines (ygg-git-compare-submit--diff-lines pr path old-path)))
       (cond (lines
              (delq nil (mapcar #'ygg-git-compare--hunk-header lines)))
             ((eql 0 (magit-git-exit-code "diff" "--quiet" (plist-get pr :base)
                                          (plist-get pr :head) "--" path old-path))
              nil)
             (t 'unknown))))))

(defun ygg-git-compare-submit--in-hunks-p (hunks side line)
  (seq-some (lambda (h)
              (pcase-let ((`(,old ,old-len ,new ,new-len) h))
                (if (eq side 'old)
                    (and (>= line old) (< line (+ old old-len)))
                  (and (>= line new) (< line (+ new new-len))))))
            hunks))

(defun ygg-git-compare-submit--outside-p (comment pr)
  "Whether COMMENT, an inline one, sits on a line outside the hunks the forge
draws for PR."
  (when (ygg-git-compare-submit--inline-p comment)
    (let ((hunks (ygg-git-compare-submit--forge-hunks pr (plist-get comment :new-path)
                                                      (plist-get comment :old-path))))
      (and (listp hunks)
           (not (and (ygg-git-compare-submit--in-hunks-p
                      hunks (plist-get comment :side) (plist-get comment :line))
                     (or (not (eq (ygg-git-compare-submit--level comment) 'range))
                         (ygg-git-compare-submit--in-hunks-p
                          hunks (plist-get comment :start-side)
                          (plist-get comment :start-line)))))))))

(defun ygg-git-compare-submit--moved-heads (comments pr)
  "The (COMMENT . SHA) of each of COMMENTS written on a commit other than
PR's head, and (COMMENT) of each inline one on a line outside PR's hunks."
  (setq ygg-git-compare-submit--memo (make-hash-table :test 'equal))
  (delq nil
        (mapcar (lambda (c)
                  (let ((sha (ygg-git-compare-submit--written-head c)))
                    (cond ((and sha (not (equal sha (plist-get pr :head)))) (cons c sha))
                          ((ygg-git-compare-submit--outside-p c pr) (list c)))))
                comments)))

(defun ygg-git-compare-submit--inline-heads (heads)
  (seq-filter (lambda (h) (ygg-git-compare-submit--inline-p (car h))) heads))

(defun ygg-git-compare-submit--written-prefix (sha)
  (if sha (format "(written on %s) " (substring sha 0 7)) ""))

(defun ygg-git-compare-submit--moved-where (comment)
  (format "`%s:%s`%s" (plist-get comment :file)
          (if (eq (ygg-git-compare-submit--level comment) 'range)
              (format "%s–%s" (plist-get comment :start-line) (plist-get comment :line))
            (plist-get comment :line))
          (if (eq (plist-get comment :side) 'old) " (removed line)" "")))

(defun ygg-git-compare-submit--moved-note (comment sha)
  "COMMENT, an inline one written on SHA, as text that says where."
  (concat (ygg-git-compare-submit--written-prefix sha)
          (ygg-git-compare-submit--moved-where comment) " "
          (ygg-git-compare-submit--text comment)))

(defun ygg-git-compare-submit--summary (comments &optional heads)
  "The review-level COMMENTS, then the file-level ones as `path' - text.
HEADS says where moved ones were written; an inline one in it is text too."
  (string-join
   (delq nil
         (mapcar (lambda (c)
                   (let* ((sha (alist-get c heads nil nil #'eq))
                          (prefix (ygg-git-compare-submit--written-prefix sha)))
                     (pcase (ygg-git-compare-submit--level c)
                       ('review (concat prefix (ygg-git-compare-submit--text c)))
                       ('file (format "%s`%s` - %s" prefix (plist-get c :file)
                                      (ygg-git-compare-submit--text c)))
                       (_ (and (assq c heads) (ygg-git-compare-submit--moved-note c sha))))))
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

(defun ygg-git-compare-submit--github-review (pr event comments &optional heads)
  "The body of PR's review with EVENT of COMMENTS; no event leaves it pending.
An inline one with a head in HEADS is text in the body, with where it was
written."
  (let* ((event-name (alist-get event ygg-git-compare-submit--github-events))
         (summary (ygg-git-compare-submit--summary comments heads))
         (body (cond ((not (string-empty-p summary)) summary)
                     ((not (memq event '(comment request-changes))) nil)
                     (comments (format "%d comment%s" (length comments)
                                       (if (= (length comments) 1) "" "s")))
                     ((eq event 'request-changes) "Changes requested"))))
    (append (list :commit_id (plist-get pr :head))
            (and event-name (list :event event-name))
            (and body (list :body body))
            (list :comments
                  (vconcat (mapcar #'ygg-git-compare-submit--github-comment
                                   (seq-filter
                                    (lambda (c)
                                      (and (ygg-git-compare-submit--inline-p c)
                                           (not (assq c heads))))
                                    comments)))))))

(defun ygg-git-compare-submit--github-refusal (text err)
  "What GitHub said in the body TEXT of a refused call, as gh: STATUS DETAIL;
nil when it said nothing readable.  ERR's HTTP code stands in for no status."
  (when-let* ((body (ignore-errors (ygg-git-compare--json (string-trim text))))
              ((plistp body))
              (detail (or (and (consp (plist-get body :errors))
                               (mapconcat (lambda (e)
                                            (if (stringp e) e
                                              (or (and (plistp e) (plist-get e :message))
                                                  (format "%s" e))))
                                          (plist-get body :errors) "; "))
                          (plist-get body :message)))
              ((stringp detail))
              ((not (string-empty-p detail))))
    (let ((status (or (plist-get body :status)
                      (and (string-match "HTTP \\([0-9]+\\)" err) (match-string 1 err)))))
      (format "gh: %s" (string-join (delq nil (list (and status (format "%s" status)) detail))
                                    " ")))))

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
                  (funcall done nil
                           (or (and (equal program "gh")
                                    (ygg-git-compare-submit--github-refusal text err))
                               (if (string-empty-p err) (format "%s failed" program) err))
                           t))
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

(defun ygg-git-compare-submit--own-pr (pr event done)
  "Call DONE with whether PR is yours on a forge that refuses your own
approval or request for changes: GitHub, for those two events."
  (if (not (and (eq (plist-get pr :forge) 'github) (memq event '(approve request-changes))))
      (funcall done nil)
    (let ((host (plist-get pr :host)))
      (ygg-git-compare-submit--api
       "gh" nil (list "api" "--hostname" host
                      (format "repos/%s/pulls/%s" (plist-get pr :path) (plist-get pr :number)))
       (lambda (value err &rest _)
         (let ((author (and (not err) (plist-get (plist-get value :user) :login)))
               (known (and (boundp 'ygg-git-review-requests--users)
                           (gethash host ygg-git-review-requests--users))))
           (cond ((null author) (funcall done nil))
                 (known (funcall done (equal known author)))
                 (t (ygg-git-compare-submit--api
                     "gh" nil (list "api" "--hostname" host "user")
                     (lambda (value err &rest _)
                       (funcall done (and (not err)
                                          (equal (plist-get value :login) author)))))))))))))

(defun ygg-git-compare-submit--github-steps (pr event comments)
  "The review of COMMENTS, then each inline one written on another commit
posted alone on that commit, as a note on the pull request when refused."
  (let* ((moved (ygg-git-compare-submit--moved-heads comments pr))
         (late (and (not (eq event 'draft))
                    (seq-filter #'cdr (ygg-git-compare-submit--inline-heads moved))))
         (carried (seq-remove (lambda (c) (assq c late)) comments))
         (review (ygg-git-compare-submit--github-review pr event carried moved))
         (host (plist-get pr :host))
         (endpoint (lambda (kind what)
                     (list "api" "--hostname" host
                           (format "repos/%s/%s/%s/%s" (plist-get pr :path) kind
                                   (plist-get pr :number) what)))))
    (append
     (unless (and (eq event 'comment)
                  (not (plist-get review :body))
                  (zerop (length (plist-get review :comments))))
       (list (list carried nil
                   (lambda (next)
                     (ygg-git-compare-submit--api
                      "gh" review
                      (funcall endpoint "pulls" "reviews")
                      (lambda (_value err &rest _) (funcall next err)))))))
     (mapcar (lambda (h)
               (list (list (car h)) nil
                     (lambda (next)
                       (ygg-git-compare-submit--api
                        "gh" (append (ygg-git-compare-submit--github-comment (car h))
                                     (list :commit_id (cdr h)))
                        (funcall endpoint "pulls" "comments")
                        (lambda (_value err &optional refused)
                          (if (and err refused)
                              (ygg-git-compare-submit--api
                               "gh" (list :body (ygg-git-compare-submit--moved-note
                                                 (car h) (cdr h)))
                               (funcall endpoint "issues" "comments")
                               (lambda (_value err &rest _) (funcall next err)))
                            (funcall next err)))))))
             late))))

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
      (cond ((when-let* ((header (ygg-git-compare--hunk-header text)))
               (setq old (nth 0 header)
                     new (nth 2 header))))
            ((null old))
            ((string-prefix-p "-" text) (cl-incf old))
            ((string-prefix-p "+" text) (cl-incf new))
            ((string-prefix-p " " text)
             (when (eql new line) (setq found old))
             (cl-incf old)
             (cl-incf new))))
    found))

(defun ygg-git-compare-submit--with-old-lines (comment pr)
  "COMMENT, the old numbers of the context lines it sits on added from PR's
base to head diff."
  (let (diff)
    (cl-flet ((old-line (side line known)
                (or known
                    (and line (not (eq side 'old))
                         (ygg-git-compare-submit--context-old-line
                          (or diff
                              (setq diff (ignore-errors
                                           (ygg-git-compare-submit--diff-lines
                                            pr (plist-get comment :new-path)
                                            (plist-get comment :old-path)))))
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

(defun ygg-git-compare-submit--on-version (pr version)
  "PR placed on the base, start and head of the diff VERSION."
  (append (list :base (plist-get version :base_commit_sha)
                :start (plist-get version :start_commit_sha)
                :head (plist-get version :head_commit_sha))
          pr))

(defun ygg-git-compare-submit--gitlab-inline (pr draft comment next &optional fallback)
  "Post COMMENT on its lines, then call NEXT with what went wrong, if anything;
a range GitLab refuses goes on its last line, and what it still refuses goes
as the text FALLBACK, when non-nil, in a note without a position."
  (let* ((text (ygg-git-compare-submit--text comment))
         (settle (lambda (err refused)
                   (if (and err refused fallback)
                       (ygg-git-compare-submit--gitlab-note
                        pr draft fallback nil (lambda (_value err &rest _) (funcall next err)))
                     (funcall next err)))))
    (ygg-git-compare-submit--gitlab-note
     pr draft text (ygg-git-compare-submit--gitlab-position comment pr)
     (lambda (_value err &optional refused)
       (if (and err refused (eq (ygg-git-compare-submit--level comment) 'range))
           (ygg-git-compare-submit--gitlab-note
            pr draft (concat (ygg-git-compare-submit--range-prefix comment) text)
            (ygg-git-compare-submit--gitlab-position comment pr t)
            (lambda (_value err &optional refused) (funcall settle err refused)))
         (funcall settle err refused))))))

(defun ygg-git-compare-submit--gitlab-reviewer-state (pr done)
  "Call DONE with (STATE ERR): the state GitLab holds for you as a reviewer of
PR, STATE nil when you are not one."
  (ygg-git-compare-submit--gitlab-api
   pr nil (list "user")
   (lambda (me err &rest _)
     (if err
         (funcall done nil err)
       (ygg-git-compare-submit--gitlab-api
        pr nil (list (ygg-git-compare-submit--gitlab-mr pr "reviewers"))
        (lambda (reviewers err &rest _)
          (funcall done
                   (and (not err)
                        (plist-get
                         (seq-find (lambda (r)
                                     (equal (plist-get (plist-get r :user) :id)
                                            (plist-get me :id)))
                                   reviewers)
                         :state))
                   err)))))))

(defun ygg-git-compare-submit--gitlab-request-changes (pr next)
  (ygg-git-compare-submit--gitlab-reviewer-state
   pr
   (lambda (state err)
     (cond (err (funcall next err))
           ((null state)
            (funcall next "GitLab records requested changes only from a reviewer: add yourself as a reviewer of this merge request"))
           (t
            (ygg-git-compare-submit--gitlab-note
             pr nil "/request_changes" nil
             (lambda (_value err &rest _)
               (if err
                   (funcall next err)
                 (ygg-git-compare-submit--gitlab-reviewer-state
                  pr
                  (lambda (state err)
                    (funcall next
                             (cond (err err)
                                   ((not (equal state "requested_changes"))
                                    "GitLab did not record the request for changes")))))))))))))

(defun ygg-git-compare-submit--gitlab-steps (pr event comments)
  "The notes that post COMMENTS to PR, drafts when EVENT is draft, and a
function of the first failure for the approval or request for changes.  A
moved inline comment with no diff version of its own is text in the summary."
  (let* ((draft (eq event 'draft))
         (summary-comments (seq-remove #'ygg-git-compare-submit--inline-p comments))
         (heads (ygg-git-compare-submit--moved-heads comments pr))
         (inline-heads (ygg-git-compare-submit--inline-heads heads))
         (versions nil)
         (summary-failure nil)
         (version-of (lambda (c)
                       (when-let* ((sha (alist-get c heads nil nil #'eq)))
                         (seq-find (lambda (v) (equal (plist-get v :head_commit_sha) sha))
                                   versions))))
         (steps (append
                 (when (seq-some #'cdr inline-heads)
                   (list (list nil "Reading versions"
                               (lambda (next)
                                 (ygg-git-compare-submit--gitlab-api
                                  pr nil
                                  (list (concat (ygg-git-compare-submit--gitlab-mr pr "versions")
                                                "?per_page=100"))
                                  (lambda (value err &rest _)
                                    (unless err (setq versions value))
                                    (funcall next nil)))))))
                 (when (or summary-comments inline-heads)
                   (list (list summary-comments nil
                               (lambda (next)
                                 (let* ((text-moved (seq-remove
                                                     (lambda (h) (funcall version-of (car h)))
                                                     inline-heads))
                                        (text (ygg-git-compare-submit--summary
                                               (seq-filter
                                                (lambda (c)
                                                  (or (not (ygg-git-compare-submit--inline-p c))
                                                      (assq c text-moved)))
                                                comments)
                                               heads)))
                                   (if (string-empty-p text)
                                       (funcall next nil)
                                     (ygg-git-compare-submit--gitlab-note
                                      pr draft text nil
                                      (lambda (_value err &rest _)
                                        (setq summary-failure err)
                                        (funcall next err)))))))))
                 (mapcar (lambda (c)
                           (list (list c) nil
                                 (lambda (next)
                                   (let ((sha (alist-get c heads nil nil #'eq))
                                         (version (funcall version-of c)))
                                     (if (and (assq c heads) (not version))
                                         (funcall next summary-failure)
                                       (ygg-git-compare-submit--gitlab-inline
                                        (if version
                                            (ygg-git-compare-submit--on-version pr version)
                                          pr)
                                        draft c next
                                        (and sha (ygg-git-compare-submit--moved-note c sha))))))))
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

(defconst ygg-git-compare-submit--event-failed
  '((comment . "Comment") (approve . "Approve") (request-changes . "Request changes")
    (draft . "Draft")))

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
                     ((and failure (null posted))
                      (format "%s failed on %s: %s"
                              (alist-get event ygg-git-compare-submit--event-failed) name failure))
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
                                           (not (equal (plist-get c :range) here))
                                           (not (ygg-git-compare-submit--written-head c))))
                          comments)))
    (unless (or comments (eq event 'approve))
      (user-error "No comments for the pull request%s"
                  (if-let* ((n (length (ygg-git-compare-submit--select 'agent max-priority)))
                            ((> n 0)))
                      (format ": %d %s to the agent; give %s a type other than todo or fix to send it here"
                              n (if (= n 1) "goes" "go") (if (= n 1) "it" "one"))
                    "")))
    (when stale
      (user-error "A comment was made on %s; drop it or send it to an agent"
                  (plist-get stale :range)))
    comments))

(defun ygg-git-compare-submit--post (event max-priority pr)
  "Ask, then post the comments to PR in the background."
  (when ygg-git-compare-submit--busy
    (user-error "A review is already being posted; wait for it to land"))
  (ygg-git-compare-submit--held event max-priority)
  (let ((buffer (current-buffer)))
    (ygg-git-compare-submit--own-pr
     pr event
     (lambda (own)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (ygg-git-compare-submit--confirm event max-priority pr own)))))))

(defun ygg-git-compare-submit--confirm (event max-priority pr own)
  "Ask, then post the comments to PR; on your OWN pull request only a comment."
  (when ygg-git-compare-submit--busy
    (user-error "A review is already being posted; wait for it to land"))
  (let* ((why (and own (format "GitHub refuses %s on your own pull request; "
                               (if (eq event 'approve) "an approval" "a request for changes"))))
         (event (if own 'comment event))
         (comments (condition-case failure
                       (ygg-git-compare-submit--held event max-priority)
                     (user-error
                      (user-error "%s" (concat why (cadr failure))))))
         (name (ygg-git-compare--pr-name pr))
         (n (length comments))
         (pending (ygg-git-compare-submit--pending-note))
         (buffer (current-buffer))
         (prior ygg-git-compare--note))
    (unless (y-or-n-p (format "%s%s %d comment%s on %s%s? "
                              (or why "")
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
  (let* ((facts (ygg-git-compare--discussion-facts))
         (discussion (plist-get facts :block)))
    (concat ygg-git-compare-fix-instructions
            (and (plist-get facts :threads)
                 (concat "\n\n" (if (plist-get facts :open)
                                      ygg-git-compare-act-threads-instructions
                                    ygg-git-compare-act-context-instructions)))
            "\n\n<fix-comments>\n"
            (ygg-git-compare--capped (let ((ygg-git-compare-markdown-with-threads t))
                                       (ygg-git-compare-markdown comments))
                                     ygg-git-compare-review-comments-max-chars)
            "</fix-comments>\n\n"
            (and discussion (concat discussion "\n\n"))
            (ygg-git-compare-compare-block))))

(defun ygg-git-compare-submit-agent (&optional max-priority)
  "Send the agent-typed comments with the compare, written as the markdown
export writes them.  MAX-PRIORITY leaves
out findings less urgent than it."
  (require 'aob)
  (require 'aob-acp)
  (let ((list (ygg-git-compare--list)))
    (with-current-buffer list
      (let ((comments (ygg-git-compare-submit--select 'agent max-priority)))
        (unless comments
          (user-error "No comments for the agent%s"
                      (if-let* ((n (length (ygg-git-compare-submit--select 'forge max-priority)))
                                ((> n 0)))
                          (format ": %d %s to the PR; give %s the todo or fix type to send it here"
                                  n (if (= n 1) "goes" "go") (if (= n 1) "it" "one"))
                        "")))
        (ygg-git-compare--discussion-guard list)
        (let ((choice (ygg-git-compare-pick-reviewer comments)))
          (ygg-git-compare--when-discussion
           list
           (lambda ()
             (if-let* ((comments (ygg-git-compare-submit--select 'agent max-priority)))
                 (let ((session (ygg-git-compare-send-to-reviewer
                                 default-directory (ygg-git-compare-submit--agent-prompt comments)
                                 choice)))
                   (ygg-git-compare-submit--drop comments)
                   (message "%d comment%s sent to an agent%s" (length comments)
                            (if (= (length comments) 1) "" "s")
                            (let ((pending (ygg-git-compare-submit--pending-note)))
                              (if (string-empty-p pending) "" (concat "; " pending))))
                   (ygg-git-compare-show-trace session))
               (message "Review not sent: the comments for the agent are gone")))))))))

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
    ("d" "Draft" ygg-git-compare-submit-draft)
    ("m" "Merge…" ygg-git-pr-merge)]
   ["Agent"
    ("@" "Fix with agent" ygg-git-compare-submit-to-agent)]
   ["Markdown"
    ("y" "Copy" ygg-git-compare-export-markdown)
    ("b" "Show in buffer" ygg-git-compare-export-markdown-buffer)
    ("w" "Write file" ygg-git-compare-export-markdown-file)]])

(provide 'ygg-git-compare-submit)
;;; ygg-git-compare-submit.el ends here
