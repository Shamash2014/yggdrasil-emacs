;;; ygg-git-compare-threads.el --- the forge's comments on a pull request, in its compare -*- lexical-binding: t; -*-

;;; Commentary:
;; A compare of a pull or merge request shows every comment the forge
;; already holds, as tuicr does: threads under the diff line they are on,
;; file comments at the file, review and conversation comments at the top.
;; They are fetched in the background through the compare's shared forge
;; helper and cache, and are read-only: they never enter the comments of
;; the compare, so submitting, exporting and sending to an agent never
;; meet them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'map)
(require 'ygg-git-compare-comments)

(defvar ygg-git-compare--b)
(autoload 'ygg-git-compare--pr-info-refresh "ygg-git-compare-pr-info")
(autoload 'ygg-git-compare--conversation-section "ygg-git-compare-pr-info")
(declare-function ygg-git-compare-comment-new "ygg-git-compare-comments")
(declare-function ygg-markdown-fences-mode "ygg-markdown-fences" (lang))
(declare-function ygg-markdown-fences--fontify "ygg-markdown-fences" (mode body))

(defcustom ygg-git-compare-threads-ttl 120
  "Seconds the forge's comments on a pull request are kept before they are
fetched again in the background."
  :type 'number
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-threads-width nil
  "Columns the bodies of the forge's comments wrap to, or nil for the window's."
  :type '(choice (const :tag "The window's" nil) natnum)
  :group 'ygg-git-compare)

(defface ygg-git-compare-remote-author '((t :inherit (font-lock-function-name-face bold)))
  "The author of a comment from the forge."
  :group 'ygg-git-compare)

(defface ygg-git-compare-remote-code '((t :inherit (fixed-pitch font-lock-string-face)))
  "Code in a comment from the forge that has no language to colour it by."
  :group 'ygg-git-compare)

(defface ygg-git-compare-remote-code-block '((t :inherit fixed-pitch))
  "Code in a comment from the forge, under the colours of its language."
  :group 'ygg-git-compare)

(defvar-local ygg-git-compare--hide-resolved nil
  "Whether threads the forge marks resolved are left out.")

(defvar-local ygg-git-compare--thread-folds nil
  "Thread to `folded' or `open', for those the user folded or opened.")

(defvar-local ygg-git-compare--remote-waiting nil
  "Whether this compare is already waiting for the forge's comments to land.")

(defvar-local ygg-git-compare--remote-wrapped-width nil
  "The width the comments of the forge were last wrapped to here.")

(defvar-local ygg-git-compare--remote-rewrap-timer nil)

(defconst ygg-git-compare--remote-threads-query
  "query($owner:String!,$name:String!,$number:Int!,$endCursor:String){repository(owner:$owner,name:$name){pullRequest(number:$number){reviewThreads(first:100,after:$endCursor){pageInfo{hasNextPage endCursor}nodes{isResolved isOutdated comments(first:100){nodes{databaseId}}}}}}}")

(defun ygg-git-compare--remote-resolved-p (comment)
  (eq (plist-get comment :resolved) t))

(defun ygg-git-compare--remote-unknown-p (comment)
  (eq (plist-get comment :resolved) 'unknown))

(defun ygg-git-compare--remote-time (iso)
  (and iso (ignore-errors (float-time (date-to-time iso)))))

(defun ygg-git-compare--remote-age (time)
  (let ((seconds (max 0 (- (float-time) time))))
    (cond ((< seconds 60) "now")
          ((< seconds 3600) (format "%dm ago" (/ seconds 60)))
          ((< seconds 86400) (format "%dh ago" (/ seconds 3600)))
          ((< seconds (* 30 86400)) (format "%dd ago" (/ seconds 86400)))
          ((< seconds (* 365 86400)) (format "%dmo ago" (/ seconds (* 30 86400))))
          (t (format "%dy ago" (/ seconds (* 365 86400)))))))

(defun ygg-git-compare--remote-fold-p (comment)
  (pcase (and ygg-git-compare--thread-folds
               (gethash (plist-get comment :thread) ygg-git-compare--thread-folds))
    ('open nil)
    ('folded t)
    (_ (ygg-git-compare--remote-resolved-p comment))))

(defun ygg-git-compare--remote-width ()
  (max 24 (or ygg-git-compare-threads-width
              (when-let* ((window (get-buffer-window (current-buffer) t)))
                (window-body-width window))
              80)))

;;; Drawing

(defun ygg-git-compare--remote-words (text limit)
  "The words of TEXT, any wider than LIMIT columns cut into pieces."
  (let (words)
    (dolist (word (split-string text))
      (while (> (string-width word) limit)
        (let ((head (truncate-string-to-width word limit)))
          (push head words)
          (setq word (substring word (length head)))))
      (unless (string-empty-p word) (push word words)))
    (nreverse words)))

(defun ygg-git-compare--remote-wrap (text width &optional first rest)
  "TEXT as lines of at most WIDTH columns, FIRST before the first line and
REST, by default as wide, before the later ones."
  (let* ((first (or first ""))
         (rest (or rest first))
         (prefix first)
         (limit (max 8 (- width (string-width rest))))
         lines current used)
    (dolist (word (ygg-git-compare--remote-words text limit))
      (let ((size (string-width word)))
        (if (and current (> (+ used 1 size) (- width (string-width prefix))))
            (progn (push (concat prefix current) lines)
                   (setq prefix rest current word used size))
          (setq used (if current (+ used 1 size) size)
                current (if current (concat current " " word) word)))))
    (if current
        (push (concat prefix current) lines)
      (unless (string-empty-p (string-trim first)) (push (string-trim-right first) lines)))
    (nreverse lines)))

(defun ygg-git-compare--remote-inline (line)
  "LINE with its `code` and **bold** spans drawn, the marks left out."
  (let ((start 0))
    (while (string-match "`\\([^`\n]+\\)`" line start)
      (let ((code (propertize (match-string 1 line) 'face 'ygg-git-compare-remote-code)))
        (setq line (replace-match code t t line)
              start (+ (match-beginning 0) (length code)))))
    (setq start 0)
    (while (string-match "\\*\\*\\([^*\n]+\\)\\*\\*" line start)
      (let ((bold (propertize (match-string 1 line) 'face 'bold)))
        (setq line (replace-match bold t t line)
              start (+ (match-beginning 0) (length bold)))))
    line))

(defun ygg-git-compare--remote-dim (line)
  (let ((line (copy-sequence line)))
    (add-face-text-property 0 (length line) 'shadow t line)
    line))

(defun ygg-git-compare--remote-suggestion (comment lines)
  "LINES, a suggestion on COMMENT's line or lines, as the diff it makes, the
old lines those of the hunk the new file has."
  (let* ((span (if-let* ((start (plist-get comment :start-line)))
                   (1+ (- (or (plist-get comment :line) start) start))
                 1))
         (hunk (split-string (or (plist-get comment :diff-hunk) "") "\n"))
         (hunk (if (string-prefix-p "@@" (or (car hunk) "")) (cdr hunk) hunk))
         (hunk (seq-remove (lambda (l) (or (string-prefix-p "-" l) (string-prefix-p "\\" l)))
                           (if (equal (car (last hunk)) "") (butlast hunk) hunk))))
    (append (mapcar (lambda (l) (propertize (concat "- " (substring l (min 1 (length l))))
                                            'face 'diff-removed))
                    (last hunk span))
            (mapcar (lambda (l) (propertize (concat "+ " l) 'face 'diff-added)) lines))))

(defun ygg-git-compare--remote-highlight (lang lines)
  "LINES as code of LANG, in the colours of its mode when there is one."
  (let* ((mode (and (not (string-empty-p lang))
                    (require 'ygg-markdown-fences nil t)
                    (ignore-errors (ygg-markdown-fences-mode lang))))
         (faced (and mode (ignore-errors
                            (ygg-markdown-fences--fontify mode (string-join lines "\n")))))
         (out (if faced
                  (mapcar (lambda (line)
                            (let ((line (copy-sequence line)))
                              (add-face-text-property 0 (length line)
                                                      'ygg-git-compare-remote-code-block t line)
                              line))
                          (split-string faced "\n"))
                (mapcar (lambda (line) (propertize line 'face 'ygg-git-compare-remote-code))
                        lines))))
    (if (= (length out) (length lines)) out
      (mapcar (lambda (line) (propertize line 'face 'ygg-git-compare-remote-code)) lines))))

(defun ygg-git-compare--remote-hard-wrap (line width)
  "LINE cut into pieces of at most WIDTH columns, nothing joined."
  (if (<= (string-width line) width)
      (list line)
    (let (pieces)
      (while (> (string-width line) width)
        (let ((head (truncate-string-to-width line width)))
          (push head pieces)
          (setq line (substring line (length head)))))
      (nreverse (if (string-empty-p line) pieces (cons line pieces))))))

(defun ygg-git-compare--remote-code (comment lang block width)
  "BLOCK, the lines of a fenced block of LANG on COMMENT, as lines to show."
  (let ((lines (apply #'append
                      (mapcar (lambda (l) (ygg-git-compare--remote-hard-wrap l width))
                              (if (equal lang "suggestion")
                                  (ygg-git-compare--remote-suggestion comment block)
                                (ygg-git-compare--remote-highlight lang block))))))
    (if (string-empty-p lang) lines
      (cons (propertize lang 'face '(shadow italic)) lines))))

(defconst ygg-git-compare--remote-fence-re
  "\\`[ \t]*\\(`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*\\(.*?\\)[ \t]*\\'")

(defun ygg-git-compare--remote-closes-p (line fence)
  (and (string-match "\\`[ \t]*\\(`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*\\'" line)
       (eq (aref (match-string 1 line) 0) (aref fence 0))
       (>= (length (match-string 1 line)) (length fence))))

(defun ygg-git-compare--remote-lang (info)
  (let ((word (or (car (split-string info "[ \t,:]+" t)) "")))
    (string-trim word "[{.]+" "[}]+")))

(defun ygg-git-compare--remote-text-lines (line width hang)
  "LINE, a line of prose, as (LINES . HANG): wrapped to WIDTH, a quote marked
on every line, a list item's later lines hanging under its text, HANG the
indent a line after an item continues at."
  (let ((line (replace-regexp-in-string "\t" "    " line)))
    (cond
     ((string-match "\\`[ ]*\\(\\(?:>[ ]*\\)+\\)\\(.*\\)\\'" line)
      (let* ((depth (cl-count ?> (match-string 1 line)))
             (prefix (apply #'concat (make-list depth "> ")))
             (text (match-string 2 line)))
        (cons (if (string-blank-p text)
                  (list (string-trim-right prefix))
                (ygg-git-compare--remote-wrap
                 (ygg-git-compare--remote-inline text) width prefix))
              nil)))
     ((string-match "\\`\\(#+\\)[ ]+\\(.*\\)\\'" line)
      (cons (mapcar (lambda (l) (propertize l 'face 'bold))
                    (ygg-git-compare--remote-wrap (match-string 2 line) width))
            nil))
     ((string-match "\\`\\( *\\)\\([-*+]\\|[0-9]+[.)]\\) +\\(.*\\)\\'" line)
      (let* ((indent (min (length (match-string 1 line)) (/ width 3)))
             (lead (concat (make-string indent ?\s) (match-string 2 line) " "))
             (rest (make-string (string-width lead) ?\s)))
        (cons (ygg-git-compare--remote-wrap
               (ygg-git-compare--remote-inline (match-string 3 line)) width lead rest)
              (string-width lead))))
     ((and hang (string-match "\\`  +\\(.*\\)\\'" line))
      (cons (ygg-git-compare--remote-wrap
             (ygg-git-compare--remote-inline (match-string 1 line)) width
             (make-string hang ?\s))
            hang))
     ((string-match "\\`[ ]*|" line)
      (cons (ygg-git-compare--remote-hard-wrap (string-trim-right line) width) nil))
     (t (cons (ygg-git-compare--remote-wrap (ygg-git-compare--remote-inline line) width)
              nil)))))

(defun ygg-git-compare--remote-body (comment width)
  "COMMENT's text as lines: wrapped to WIDTH, code and suggestions apart."
  (let (out fence lang block hang)
    (dolist (line (split-string (or (plist-get comment :text) "") "\r?\n"))
      (cond
       (fence
        (if (ygg-git-compare--remote-closes-p line fence)
            (setq out (append (reverse (ygg-git-compare--remote-code
                                        comment lang (nreverse block) width))
                              out)
                  fence nil block nil)
          (push line block)))
       ((string-match ygg-git-compare--remote-fence-re line)
        (setq fence (match-string 1 line)
              lang (ygg-git-compare--remote-lang (match-string 2 line))
              hang nil))
       ((string-blank-p line) (push "" out) (setq hang nil))
       (t (let ((made (ygg-git-compare--remote-text-lines line width hang)))
            (setq out (append (reverse (car made)) out)
                  hang (cdr made))))))
    (when fence
      (setq out (append (reverse (ygg-git-compare--remote-code
                                  comment lang (nreverse block) width))
                        out)))
    (nreverse out)))

(defun ygg-git-compare--remote-state (comment)
  (cond ((plist-get comment :state) (downcase (plist-get comment :state)))
        ((ygg-git-compare--remote-unknown-p comment) "state unknown")
        ((ygg-git-compare--remote-resolved-p comment) "resolved")
        ((plist-get comment :outdated)
         (if-let* ((line (plist-get comment :orig-line)))
             (format "outdated · was L%s" line)
           "outdated"))
        ((memq (plist-get comment :level) '(line range file)) "open")))

(defun ygg-git-compare--remote-plain (text)
  "TEXT without the markdown marks that make no words."
  (thread-last (or text "")
               (replace-regexp-in-string "^[ \t]*\\(?:```\\|~~~\\).*$" "")
               (replace-regexp-in-string "^[ \t]*#+[ \t]+" "")
               (replace-regexp-in-string "[`*]+\\|~~" "")
               (replace-regexp-in-string "\\(^\\|[[:space:]]\\)_+\\|_+\\($\\|[[:space:]]\\)" "\\1\\2")
               (replace-regexp-in-string "[ \t\n\r]+" " ")
               string-trim))

(defun ygg-git-compare--remote-label (comment)
  "COMMENT's author and the first words of its text."
  (let ((words (split-string (ygg-git-compare--remote-plain (plist-get comment :text)))))
    (truncate-string-to-width
     (string-join (delq nil (list (plist-get comment :author)
                                  (and words (string-join (seq-take words 8) " "))))
                  ": ")
     60 nil nil "…")))

(defun ygg-git-compare--remote-block (comment)
  "COMMENT as lines to show: a thread's first comment under a header with its
author, age, replies and state, a reply indented under it with no state."
  (condition-case nil
      (ygg-git-compare--remote-draw comment)
    (error (propertize "(a comment from the forge that could not be shown)" 'face 'shadow))))

(defun ygg-git-compare--remote-draw (comment)
  (let* ((depth (or (plist-get comment :depth) 0))
         (dim (or (ygg-git-compare--remote-resolved-p comment) (plist-get comment :outdated)))
         (folded (plist-get comment :folded))
         (bar (concat (make-string (* 2 depth) ?\s)
                      (propertize "▎ " 'face (if dim 'shadow 'font-lock-doc-face))))
         (width (- (ygg-git-compare--remote-width) (string-width bar)))
         (replies (plist-get comment :replies))
         (state (and (zerop depth) (ygg-git-compare--remote-state comment)))
         (head (concat
                (propertize (cond ((> depth 0) "") (folded "▸ ") (t "▾ ")) 'face 'shadow)
                (string-join
                 (delq nil
                       (list (propertize (or (plist-get comment :author) "")
                                         'face 'ygg-git-compare-remote-author)
                             (when-let* ((created (plist-get comment :created)))
                               (propertize (ygg-git-compare--remote-age created) 'face 'shadow))
                             (and replies (> replies 0)
                                  (propertize (format "%d repl%s" replies (if (= replies 1) "y" "ies"))
                                              'face 'shadow))
                             (and state (propertize state 'face
                                                    (cond ((ygg-git-compare--remote-unknown-p comment) 'warning)
                                                          (dim 'shadow)
                                                          (t 'font-lock-keyword-face))))))
                 " · "))))
    (cond ((plist-get comment :notice) (propertize (plist-get comment :text) 'face 'shadow))
          ((plist-get comment :heading) (propertize (plist-get comment :text) 'face 'bold))
          ((and folded (> depth 0)) "")
          (t (mapconcat (lambda (line) (concat bar line))
                        (cons head (unless folded
                                     (mapcar (lambda (line) (if dim (ygg-git-compare--remote-dim line) line))
                                             (ygg-git-compare--remote-body comment width))))
                        "\n")))))

(defun ygg-git-compare--remote-resized (&optional arg)
  "Wrap the comments again once the window has stopped changing size."
  (let ((buffer (if (windowp arg) (window-buffer arg) (current-buffer))))
    (when (and (buffer-live-p buffer) (not (minibufferp buffer)))
      (with-current-buffer buffer
        (when (and ygg-git-compare--remote-wrapped-width
                   (/= (ygg-git-compare--remote-width) ygg-git-compare--remote-wrapped-width))
          (when (timerp ygg-git-compare--remote-rewrap-timer)
            (cancel-timer ygg-git-compare--remote-rewrap-timer))
          (setq ygg-git-compare--remote-rewrap-timer
                (run-with-idle-timer 0.2 nil #'ygg-git-compare--remote-rewrap buffer)))))))

(defun ygg-git-compare--remote-rewrap (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ygg-git-compare--remote-rewrap-timer nil)
      (when (ygg-git-compare--conversation-section)
        (ygg-git-compare--pr-info-refresh buffer :conversation))
      (ignore-errors (ygg-git-compare--draw-comments)))))

(defun ygg-git-compare--remote-watch-width ()
  (setq ygg-git-compare--remote-wrapped-width (ygg-git-compare--remote-width))
  (add-hook 'window-size-change-functions #'ygg-git-compare--remote-resized nil t))

;;; Reading

(defun ygg-git-compare--remote-json (string)
  "Every JSON value in STRING, the elements of an array each as a value."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (values)
      (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
        (let ((value (json-parse-buffer :object-type 'plist :array-type 'list
                                        :null-object nil :false-object nil)))
          (setq values (append values (if (keywordp (car-safe value)) (list value) value)))))
      values)))

(defun ygg-git-compare--remote-failure (program status err)
  (cond ((null status) (format "%s not found" program))
        ((eq status 'timeout) (format "%s timed out" program))
        ((string-empty-p err) (format "%s failed" program))
        (t (car (split-string err "\n" t)))))

(defun ygg-git-compare--remote-values (results key)
  "The JSON values request KEY answered in RESULTS; an error saying why it
failed."
  (pcase (cdr (assq key results))
    (`(0 ,text ,_) (ygg-git-compare--remote-json text))
    (`(,status ,_ ,err) (error "%s" (ygg-git-compare--remote-failure
                                     (if (eq key 'discussions) "glab" "gh") status (or err ""))))))

(defun ygg-git-compare--remote-threads (comments)
  "COMMENTS grouped by thread, oldest thread first, a thread's replies after
its first comment and one level in."
  (let* ((groups (mapcar (lambda (group)
                           (sort (copy-sequence (cdr group))
                                 (lambda (a b) (< (or (plist-get a :created) 0)
                                                  (or (plist-get b :created) 0)))))
                         (seq-group-by (lambda (c) (plist-get c :thread)) comments))))
    (mapcan (lambda (group)
              (cons (car group)
                    (mapcar (lambda (c) (plist-put (copy-sequence c) :depth 1)) (cdr group))))
            (sort groups (lambda (a b) (< (or (plist-get (car a) :created) 0)
                                          (or (plist-get (car b) :created) 0)))))))

(defun ygg-git-compare--remote-github-states (values)
  "A table of comment id to (RESOLVED . OUTDATED) from the review threads of
the GraphQL answers VALUES, or nil when an answer is not one."
  (let ((states (make-hash-table :test #'eql)))
    (catch 'bad
      (dolist (value values)
        (let ((pull (thread-first value (plist-get :data) (plist-get :repository)
                                  (plist-get :pullRequest))))
          (unless (and (null (plist-get value :errors)) pull) (throw 'bad nil))
          (dolist (node (thread-first pull (plist-get :reviewThreads) (plist-get :nodes)))
            (dolist (comment (plist-get (plist-get node :comments) :nodes))
              (puthash (plist-get comment :databaseId)
                       (cons (plist-get node :isResolved) (plist-get node :isOutdated))
                       states)))))
      states)))

(defun ygg-git-compare--remote-github-inline (raw states old)
  "RAW's review comments, STATES their threads' states or nil when they could
not be had, and OLD the comments held before, whose states stand then."
  (mapcar
   (lambda (c)
     (let* ((id (plist-get c :id))
            (kept (let ((was (gethash (format "remote:gh:%s" id) old)))
                    (and was (not (ygg-git-compare--remote-unknown-p was)) was)))
            (held (and kept (cons (plist-get kept :resolved)
                                  (plist-get kept :outdated))))
            (state (if states (or (gethash id states) held) held))
            (line (plist-get c :line))
            (file-subject (equal (plist-get c :subject_type) "file"))
            (anchored (and line (not (cdr state))))
            (start (and anchored (plist-get c :start_line)))
            (path (plist-get c :path)))
       (list :remote t :id (format "remote:gh:%s" id)
             :thread (or (plist-get c :in_reply_to_id) id)
             :author (plist-get (plist-get c :user) :login)
             :created (ygg-git-compare--remote-time (plist-get c :created_at))
             :text (plist-get c :body) :url (plist-get c :html_url)
             :diff-hunk (plist-get c :diff_hunk)
             :file path :new-path path
             :level (cond ((not anchored) 'file) ((and start (not (eql start line))) 'range) (t 'line))
             :side (if (equal (plist-get c :side) "LEFT") 'old 'new)
             :line (and anchored line) :start-line start
             :orig-line (or (plist-get c :original_line) line)
             :resolved (if state (car state) 'unknown)
             :outdated (or (cdr state) (and (null line) (not file-subject))))))
   raw))

(defun ygg-git-compare--remote-github-summaries (reviews notes)
  (append
   (seq-keep
    (lambda (r)
      (let ((body (plist-get r :body))
            (state (plist-get r :state)))
        (unless (or (equal state "PENDING")
                    (and (equal state "COMMENTED") (string-empty-p (string-trim (or body "")))))
          (list :remote t :id (format "remote:gh-review:%s" (plist-get r :id))
                :thread (format "review-%s" (plist-get r :id))
                :author (plist-get (plist-get r :user) :login)
                :created (ygg-git-compare--remote-time (plist-get r :submitted_at))
                :text (if (string-empty-p (string-trim (or body ""))) "(no summary)" body)
                :state (replace-regexp-in-string "_" " " (or state ""))
                :url (plist-get r :html_url) :level 'review))))
    reviews)
   (mapcar (lambda (n)
             (list :remote t :id (format "remote:gh-note:%s" (plist-get n :id))
                   :thread (format "note-%s" (plist-get n :id))
                   :author (plist-get (plist-get n :user) :login)
                   :created (ygg-git-compare--remote-time (plist-get n :created_at))
                   :text (plist-get n :body) :url (plist-get n :html_url) :level 'review))
           notes)))

(defun ygg-git-compare--remote-gitlab (discussions pr)
  "The notes of DISCUSSIONS, system notes left out, on pull request PR."
  (mapcan
   (lambda (d)
     (let* ((notes (seq-remove (lambda (n) (plist-get n :system)) (plist-get d :notes)))
            (position (plist-get (car notes) :position))
            (new (plist-get position :new_line))
            (old (plist-get position :old_line))
            (path (or (plist-get position :new_path) (plist-get position :old_path)))
            (resolvable (seq-filter (lambda (n) (plist-get n :resolvable)) notes))
            (resolved (and resolvable (seq-every-p (lambda (n) (plist-get n :resolved)) resolvable))))
       (mapcar
        (lambda (n)
          (list :remote t :id (format "remote:gl:%s" (plist-get n :id))
                :thread (plist-get d :id)
                :author (plist-get (plist-get n :author) :username)
                :created (ygg-git-compare--remote-time (plist-get n :created_at))
                :text (plist-get n :body)
                :url (format "https://%s/%s/-/merge_requests/%s#note_%s"
                             (plist-get pr :host) (plist-get pr :path)
                             (plist-get pr :number) (plist-get n :id))
                :file path :new-path path
                :level (cond ((not position) 'review) ((or new old) 'line) (t 'file))
                :side (if new 'new 'old) :line (or new old)
                :head-sha (plist-get position :head_sha)
                :resolved resolved))
        notes)))
   discussions))

(defun ygg-git-compare--remote-parse (pr results old)
  "The comments PR's requests answered in RESULTS, as threads, OLD being those
held before, as (:comments LIST) with :states-error when the resolved state of
GitHub's threads could not be had."
  (pcase (plist-get pr :forge)
    ('gitlab
     (list :comments (ygg-git-compare--remote-threads
                      (ygg-git-compare--remote-gitlab
                       (ygg-git-compare--remote-values results 'discussions) pr))))
    (_
     (let* ((inline (ygg-git-compare--remote-values results 'inline))
            (answer (cdr (assq 'threads results)))
            (states (and (eql (car answer) 0)
                         (condition-case nil
                             (ygg-git-compare--remote-github-states
                              (ygg-git-compare--remote-json (cadr answer)))
                           (error nil))))
            (table (make-hash-table :test #'equal)))
       (dolist (c old) (puthash (plist-get c :id) c table))
       (append
        (list :comments
              (ygg-git-compare--remote-threads
               (append (ygg-git-compare--remote-github-inline inline states table)
                       (ygg-git-compare--remote-github-summaries
                        (ygg-git-compare--remote-values results 'reviews)
                        (ygg-git-compare--remote-values results 'notes)))))
        (unless states
          (list :states-error (if (eql (car answer) 0)
                                  "unreadable answer"
                                (ygg-git-compare--remote-failure
                                 "gh" (car answer) (or (nth 2 answer) ""))))))))))

;;; Fetching

(defun ygg-git-compare--remote-requests (pr)
  "PR's requests as (KEY PROGRAM . ARGS)."
  (pcase-let (((map :host :path :number) pr))
    (pcase (plist-get pr :forge)
      ('gitlab
       `((discussions "glab" "api" "--hostname" ,host "--paginate"
                      ,(format "projects/%s/merge_requests/%s/discussions?per_page=100"
                               (url-hexify-string path) number))))
      (_
       (let ((owner+name (split-string path "/")))
         (cl-flet ((request (key suffix)
                     (list key "gh" "api" "--hostname" host "--paginate"
                           (format "repos/%s/%s?per_page=100" path suffix))))
           (list (request 'inline (format "pulls/%s/comments" number))
                 (request 'reviews (format "pulls/%s/reviews" number))
                 (request 'notes (format "issues/%s/comments" number))
                 (list 'threads "gh" "api" "graphql" "--paginate" "--hostname" host
                       "-f" (concat "query=" ygg-git-compare--remote-threads-query)
                       "-f" (concat "owner=" (car owner+name))
                       "-f" (concat "name=" (cadr owner+name))
                       "-F" (format "number=%s" number)))))))))

(defun ygg-git-compare--remote-fetch (pr old done)
  "Fetch PR's comments, OLD being those held before, then call DONE with the
value, or with nil and why not."
  (let* ((requests (ygg-git-compare--remote-requests pr))
         (left (length requests))
         results)
    (dolist (request requests)
      (ygg-git-compare--forge-async
       (cadr request) (cddr request)
       (lambda (status text err)
         (push (cons (car request) (list status text err)) results)
         (when (zerop (cl-decf left))
           (let ((value (condition-case failure
                            (ygg-git-compare--remote-parse pr results old)
                          (error (error-message-string failure)))))
             (if (stringp value) (funcall done nil value) (funcall done value)))))))))

;;; Keeping

(defun ygg-git-compare--remote-pr ()
  "The pull request side B is, as (:forge :host :path :number), as its side
was made with them; nil when it does not say."
  (let ((spec (cdr ygg-git-compare--b-spec)))
    (when (and (plist-get spec :forge) (plist-get spec :host)
               (plist-get spec :path) (plist-get spec :number))
      (list :forge (plist-get spec :forge) :host (plist-get spec :host)
            :path (plist-get spec :path) :number (plist-get spec :number)))))

(defun ygg-git-compare--remote-key (pr)
  (list 'threads (plist-get pr :forge) (plist-get pr :host)
        (plist-get pr :path) (plist-get pr :number)))

(defun ygg-git-compare--remote-comment-valid-p (comment)
  (and (proper-list-p comment)
       (cl-evenp (length comment))
       (stringp (plist-get comment :id))
       (memq (plist-get comment :level) '(line range file review))
       (seq-every-p (lambda (key)
                      (let ((value (plist-get comment key)))
                        (or (null value) (stringp value))))
                    '(:text :author :url :file :new-path :diff-hunk))
       (seq-every-p (lambda (key)
                      (let ((value (plist-get comment key)))
                        (or (null value) (numberp value))))
                    '(:created :line :start-line :orig-line))))

(defun ygg-git-compare--remote-valid-p (value)
  "Whether VALUE is what the cache keeps for a pull request's comments."
  (and (proper-list-p value)
       (cl-evenp (length value))
       (proper-list-p (plist-get value :comments))
       (seq-every-p #'ygg-git-compare--remote-comment-valid-p (plist-get value :comments))))

(defun ygg-git-compare--remote-value (key)
  "What is kept for KEY, nil when nothing sound is."
  (let ((value (ignore-errors (ygg-git-compare--cache-value key))))
    (and (ygg-git-compare--remote-valid-p value) value)))

(defun ygg-git-compare--remote-start (list pr key)
  "Fetch PR's comments in the background unless they are fresh or being
fetched, redrawing LIST's compare when they land."
  (let* ((dir default-directory)
         (stale (ygg-git-compare--cache-stale-p
                 (ignore-errors (ygg-git-compare--cache-entry key))
                 ygg-git-compare-threads-ttl))
         (waiter (and stale (not ygg-git-compare--remote-waiting)
                      (lambda (&rest _)
                        (when (buffer-live-p list)
                          (with-current-buffer list (setq ygg-git-compare--remote-waiting nil))
                          (ygg-git-compare--pr-info-refresh list)
                          (ygg-git-compare--redraw-comments list))))))
    (when waiter (setq ygg-git-compare--remote-waiting t))
    (condition-case failure
        (ygg-git-compare--cached
         key ygg-git-compare-threads-ttl
         (lambda (done)
           (let ((default-directory dir))
             (ygg-git-compare--remote-fetch
              pr (plist-get (ygg-git-compare--remote-value key) :comments) done)))
         waiter)
      (error (setq ygg-git-compare--remote-waiting nil)
             (signal (car failure) (cdr failure))))))

(defun ygg-git-compare--remote-outdate (comment)
  "COMMENT as one on a line the diff no longer has, kept at its file."
  (let ((comment (copy-sequence comment)))
    (setq comment (plist-put comment :orig-line (plist-get comment :line)))
    (dolist (change '((:outdated . t) (:level . file) (:line) (:start-line)))
      (setq comment (plist-put comment (car change) (cdr change))))
    comment))

(defun ygg-git-compare--remote-annotate (comments)
  "COMMENTS with each thread's reply count on its first comment, whether it
is folded on every one, and a GitLab note made on another head outdated."
  (let ((counts (mapcar (lambda (group) (cons (car group) (1- (length (cdr group)))))
                        (seq-group-by (lambda (c) (plist-get c :thread)) comments)))
        (head (plist-get ygg-git-compare--b :diff)))
    (mapcar (lambda (c)
              (let ((c (if (and head (plist-get c :head-sha)
                                (not (equal (plist-get c :head-sha) head))
                                (not (plist-get c :outdated)))
                           (ygg-git-compare--remote-outdate c)
                         c)))
                (append (list :folded (ygg-git-compare--remote-fold-p c))
                        (and (zerop (or (plist-get c :depth) 0))
                             (list :replies (cdr (assoc (plist-get c :thread) counts))))
                        c)))
            comments)))

(defun ygg-git-compare--remote-notice (text)
  (list :remote t :notice t :id "remote:notice" :level 'review :text text))

(defun ygg-git-compare--remote-shown (list &optional notice)
  "The comments from the forge LIST's compare shows, led by a line saying why
there are none or how they are late when NOTICE."
  (with-current-buffer list
    (when (eq (car-safe ygg-git-compare--b-spec) 'pr)
      (when-let* ((pr (ygg-git-compare--remote-pr)))
        (let* ((key (ygg-git-compare--remote-key pr))
               (value (ygg-git-compare--remote-value key))
               (failed (plist-get (ignore-errors (ygg-git-compare--cache-entry key)) :error))
               (comments (plist-get value :comments)))
          (append
           (and notice
                (cond (failed (list (ygg-git-compare--remote-notice
                                    (format "forge comments: %s%s" failed
                                            (if value " (showing the last fetch)" "")))))
                      ((and (null value) (ygg-git-compare--refreshing-p 'threads))
                       (list (ygg-git-compare--remote-notice "(fetching comments…)")))
                      ((plist-get value :states-error)
                       (list (ygg-git-compare--remote-notice
                              (format "forge comments: resolved state %s: %s"
                                      (if (seq-some #'ygg-git-compare--remote-unknown-p comments)
                                          "unknown" "not refreshed")
                                      (plist-get value :states-error)))))))
           (ygg-git-compare--remote-annotate
            (if ygg-git-compare--hide-resolved
                (seq-remove #'ygg-git-compare--remote-resolved-p comments)
              comments))))))))

(defun ygg-git-compare--remote-comments (list)
  "The comments from the forge to show in LIST's compare, fetching them in
the background when they are not kept or have gone stale.  Never signals and
waits for nothing."
  (with-current-buffer list
    (when (eq (car-safe ygg-git-compare--b-spec) 'pr)
      (condition-case err
          (progn
            (when-let* ((pr (ygg-git-compare--remote-pr)))
              (ygg-git-compare--remote-watch-width)
              (ygg-git-compare--remote-start list pr (ygg-git-compare--remote-key pr)))
            (ygg-git-compare--remote-shown list t))
        (error
         (list (ygg-git-compare--remote-notice
                (concat "forge comments: " (error-message-string err)))))))))

;;; Acting on one

(defun ygg-git-compare--remote-at-point (&optional by-thread)
  "The comment from the forge shown on this line, asked for among several;
with BY-THREAD, the first comment of a thread, asked for among threads."
  (let* ((ids (seq-mapcat (lambda (ov) (overlay-get ov 'ygg-git-compare-comments))
                          (overlays-in (line-beginning-position) (line-end-position))))
         (comments (seq-filter (lambda (c) (member (plist-get c :id) ids))
                               (ignore-errors
                                 (ygg-git-compare--remote-shown (ygg-git-compare--list))))))
    (when by-thread
      (setq comments (seq-uniq comments (lambda (a b) (equal (plist-get a :thread)
                                                              (plist-get b :thread))))))
    (if (cdr comments)
        (let ((rows (mapcar (lambda (c)
                              (cons (format "%s: %s" (plist-get c :author)
                                            (car (split-string (or (plist-get c :text) "") "\n")))
                                    c))
                            comments)))
          (cdr (assoc (completing-read "Comment: " rows nil t) rows)))
      (or (car comments) (user-error "No comment from the forge here")))))

(defun ygg-git-compare-threads-open ()
  "Open the forge's comment at point in the browser."
  (interactive)
  (browse-url (or (plist-get (ygg-git-compare--remote-at-point) :url)
                  (user-error "This comment has no page"))))

(defun ygg-git-compare-threads-copy ()
  "Copy the body of the forge's comment at point."
  (interactive)
  (let ((text (or (plist-get (ygg-git-compare--remote-at-point) :text) "")))
    (kill-new text)
    (message "Copied: %s" (truncate-string-to-width text 60 nil nil "…"))))

(defun ygg-git-compare-threads-reply ()
  "Start a draft comment where the forge's comment at point is."
  (interactive)
  (if (eq (plist-get (ygg-git-compare--remote-at-point t) :level) 'review)
      (ygg-git-compare-comment-review)
    (ygg-git-compare-comment-new)))

(defun ygg-git-compare-threads-toggle-fold ()
  "Fold the thread of the forge's comment at point, or open it."
  (interactive)
  (let* ((comment (ygg-git-compare--remote-at-point t))
         (list (ygg-git-compare--list)))
    (with-current-buffer list
      (unless ygg-git-compare--thread-folds
        (setq ygg-git-compare--thread-folds (make-hash-table :test #'equal)))
      (puthash (plist-get comment :thread)
               (if (plist-get comment :folded) 'open 'folded)
               ygg-git-compare--thread-folds))
    (ygg-git-compare--pr-info-refresh list)
    (ygg-git-compare--redraw-comments list)))

(defun ygg-git-compare-threads-toggle-resolved ()
  "Hide the threads the forge marks resolved, or show them again."
  (interactive)
  (let ((list (ygg-git-compare--list)))
    (with-current-buffer list
      (setq ygg-git-compare--hide-resolved (not ygg-git-compare--hide-resolved))
      (message "Resolved threads %s" (if ygg-git-compare--hide-resolved "hidden" "shown")))
    (ygg-git-compare--redraw-comments list)))

(provide 'ygg-git-compare-threads)
;;; ygg-git-compare-threads.el ends here
