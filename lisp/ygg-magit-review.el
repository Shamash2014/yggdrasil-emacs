;;; ygg-magit-review.el --- send a commit from magit to an agent for review -*- lexical-binding: t; -*-

;;; Commentary:
;; The commit at point, or a marked range of them in a log, goes to an agent
;; picked each time: a live session working in this repository, another
;; live session, or a new one started at the repository root.  The agent
;; is asked to review, never to change anything.
;;
;; Review comments written on diff lines are held per repository and shown
;; under their lines; the same send then carries them all in one message,
;; to a session or to a new one that turns them into a plan.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)

(declare-function aob-live-sessions "aob")
(declare-function aob-session-name "aob")
(declare-function aob-session-state "aob")
(declare-function aob-session-dir "aob")
(declare-function aob-session-project "aob")
(declare-function aob-session-cwd "aob")
(declare-function aob-prompt "aob")
(declare-function aob-trace "aob-trace")
(declare-function aob-acp-spawn "aob-acp")
(declare-function aob-compose "aob")
(declare-function aob-compose-send "aob")
(defvar aob-compose--label)
(defvar aob-compose--tags)
(declare-function ygg-aob--expand-presets "layer-aob" (text))
(defvar aob-prompt-typed)
(defvar aob-acp-agents)
(defvar aob-acp-start-dir)
(defvar aob-compose--dir)
(defvar aob-compose-before-send-functions)
(defvar aob-compose-spawn-function)
(defvar ygg-aob--draft-tree)

(defgroup ygg-magit-review nil
  "Send commits from magit to an agent for review."
  :group 'magit-extensions :prefix "ygg-magit-review-")

(defcustom ygg-magit-review-instructions
  "Review this commit as a strict reviewer.  List each finding as
path:line, a severity (blocker, major, minor, nit), what is wrong and a
concrete fix.  Check correctness, error handling, tests, naming and
security.  Do not change any files.  End with a one-line verdict."
  "What an agent is asked to do with the commit it is sent."
  :type 'string)

(defcustom ygg-magit-review-comments-instructions
  "These are my review comments, each anchored to a file and line with
the lines around it, followed by the patch they were made on.  Each
comment is an instruction, not a suggestion: work out the change it
asks for.  Where a comment leaves a choice open, ask me before deciding."
  "What an agent is told about the review comments it is sent."
  :type 'string)

(defconst ygg-magit-review-preset "review"
  "The preset a new session that plans from review comments runs under.")

(defcustom ygg-magit-review-max-chars 60000
  "How much of a commit's diff a review carries before it is cut."
  :type 'natnum)

(defvar ygg-magit-review--held (make-hash-table :test #'equal)
  "Repository root to its held review comments, newest first.")

(defvar ygg-magit-review--sent (make-hash-table :test #'equal)
  "Repository root to the comments its last send cleared, for one undo.")

(defvar ygg-magit-review--last-id 0)

(defun ygg-magit-review--git (root &rest args)
  "What git ARGS prints in ROOT."
  (with-temp-buffer
    (let* ((default-directory root)
           (err (make-temp-file "ygg-magit-review-"))
           (status (apply #'call-process "git" nil (list t err) nil args)))
      (unwind-protect
          (unless (eql status 0)
            (user-error "git %s: %s" (car args)
                        (string-trim (with-temp-buffer
                                       (insert-file-contents err)
                                       (buffer-string)))))
        (delete-file err)))
    (buffer-string)))

(defun ygg-magit-review--capped (text)
  (if (> (length text) ygg-magit-review-max-chars)
      (concat (substring text 0 ygg-magit-review-max-chars)
              (format "\n… %d more characters left out"
                      (- (length text) ygg-magit-review-max-chars)))
    text))

(defun ygg-magit-review--range (hashes)
  "HASHES, two or more marked commits, as oldest^..newest."
  (let ((newest (car hashes))
        (oldest (car (last hashes))))
    (unless (magit-git-success "merge-base" "--is-ancestor" oldest newest)
      (cl-rotatef newest oldest))
    (format "%s^..%s" oldest newest)))

(defun ygg-magit-review--diff-range ()
  "The range a diff buffer shows, when it is one and not a single commit."
  (and (derived-mode-p 'magit-diff-mode)
       (not (derived-mode-p 'magit-revision-mode))
       magit-buffer-diff-range
       (string-match-p "\\.\\." magit-buffer-diff-range)
       (not (magit-section-value-if 'commit))
       magit-buffer-diff-range))

(defun ygg-magit-review--target ()
  "The commit hash, or a range string, the review is about."
  (let ((marked (magit-region-values 'commit t)))
    (cond
     (marked
      (ygg-magit-review--range
       (mapcar (lambda (rev) (magit-commit-oid rev t)) marked)))
     ((ygg-magit-review--diff-range))
     ((when-let* ((rev (or (magit-commit-at-point)
                           (magit-branch-or-commit-at-point))))
        (magit-commit-oid rev t)))
     (t (user-error "No commit at point")))))

(defun ygg-magit-review--range-p (target)
  (string-match-p "\\.\\." target))

(defun ygg-magit-review--marked-range-p (target)
  "Whether TARGET is oldest^..newest, as marked commits make it."
  (string-match-p "\\^\\.\\." target))

(defun ygg-magit-review--range-block (root target)
  "Any other range TARGET in ROOT: the commits it adds, and its diff.
A...B diffs from where the sides parted, so its log is A..B alone."
  (concat "<commits>\nrepository " root " · range " target "\n"
          (ygg-magit-review--git root "log" "--format=fuller"
                                 (string-replace "..." ".." target))
          "\n"
          (ygg-magit-review--capped (ygg-magit-review--git root "diff" target))
          "</commits>"))

(defun ygg-magit-review--block (root target)
  "The commit TARGET in ROOT as git tells it, its diff cut to size."
  (cond
   ((and (ygg-magit-review--range-p target)
         (not (ygg-magit-review--marked-range-p target)))
    (ygg-magit-review--range-block root target))
   ((ygg-magit-review--range-p target)
    (pcase-let* ((`(,oldest ,newest) (split-string target "\\^\\.\\."))
                 (from-root (let ((default-directory root))
                              (not (magit-commit-parents oldest))))
                 (base (if from-root
                           (string-trim (ygg-magit-review--git
                                         root "hash-object" "-t" "tree"
                                         null-device))
                         (concat oldest "^"))))
      (concat "<commits>\nrepository " root " · range " target "\n"
              (ygg-magit-review--git root "log" "--format=fuller"
                                     (if from-root newest target))
              "\n"
              (ygg-magit-review--capped
               (ygg-magit-review--git root "diff" base newest))
              "</commits>")))
   (t
    (concat "<commit>\nrepository " root " · commit " target "\n"
            (ygg-magit-review--capped
             (ygg-magit-review--git root "show" "--stat" "--patch"
                                    "--format=fuller" target))
            "</commit>"))))

(defun ygg-magit-review-prompt (root target &optional elsewhere)
  "The review prompt for TARGET in ROOT.
ELSEWHERE, when non-nil, is where the receiving session works instead."
  (concat ygg-magit-review-instructions
          (when elsewhere
            (format "\n\nYou work in %s, not in %s where this commit lives, \
so its files may not be there to read.  Review from the patch below."
                    elsewhere root))
          "\n\n" (ygg-magit-review--block root target)))

(defun ygg-magit-review--trees (root)
  "ROOT and every worktree of its repository, as directories."
  (let ((default-directory root))
    (delete-dups
     (cons (file-name-as-directory root)
           (mapcar (lambda (w) (file-name-as-directory (car w)))
                   (magit-list-worktrees))))))

(defun ygg-magit-review--same-repo-p (s trees)
  (seq-some (lambda (d)
              (and d (seq-some (lambda (tree) (file-in-directory-p d tree))
                               trees)))
            (list (aob-session-dir s) (aob-session-project s))))

(defun ygg-magit-review-candidates (root)
  "Labels to what they send to: a new review plan, same-repo sessions,
other sessions, new agents.  A session is the session itself; a new
agent is (new . NAME); the review plan is the symbol review."
  (let* ((trees (ygg-magit-review--trees root))
         (sessions (aob-live-sessions))
         (here (seq-filter (lambda (s) (ygg-magit-review--same-repo-p s trees))
                           sessions))
         (away (seq-difference sessions here #'eq))
         labels)
    (append
     (list (cons (format "new: review plan (preset %s)" ygg-magit-review-preset)
                 'review))
     (mapcar (lambda (s)
               (let ((label (format "%s · %s · %s" (aob-session-name s)
                                    (aob-session-state s)
                                    (or (aob-session-cwd s) "?"))))
                 (while (member label labels) (setq label (concat label "'")))
                 (push label labels)
                 (cons label s)))
             (append here away))
     (mapcar (lambda (a) (cons (concat "new: " (car a)) (cons 'new (car a))))
             aob-acp-agents))))

(defun ygg-magit-review--pick (root)
  (let* ((choices (ygg-magit-review-candidates root))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action choices str pred)))))
    (or (cdr (assoc (completing-read "Review by: " table nil t) choices))
        (user-error "No session picked"))))

(defun ygg-magit-review--spawn (root agent)
  (let ((default-directory root)
        (aob-acp-start-dir root))
    (or (aob-acp-spawn agent)
        (user-error "Could not start %s" agent))))

(defun ygg-magit-review--spawn-review (root text)
  "A new session in ROOT under the review preset, TEXT its first turn.
The preset rides the way a draft naming it carries it, so its limits
reach the session; only the preset is expanded, never a name the
comments or the patch happen to mention."
  (unless (and (bound-and-true-p aob-compose-spawn-function)
               (fboundp 'ygg-aob--expand-presets))
    (user-error "The review preset needs layer-aob"))
  (let* ((aob-compose--dir root)
         (ygg-aob--draft-tree nil)
         (default-directory root)
         (at (concat "@" ygg-magit-review-preset))
         (preset (ygg-aob--expand-presets at)))
    (unless (and preset
                 (string-search (format "<preset name=\"%s\">"
                                        ygg-magit-review-preset)
                                preset))
      (user-error "No %s preset here" ygg-magit-review-preset))
    (or (let ((aob-prompt-typed t))
          (funcall aob-compose-spawn-function
                   (concat at " " text (substring preset (length at)))))
        (user-error "Could not start a review session"))))

;;;###autoload
(defun ygg-magit-review-commit ()
  "Send this repository's held review comments to an agent.
When none are held, send the commit at point, or the marked commits, to
review instead."
  (interactive)
  (require 'aob)
  (require 'aob-acp)
  (let* ((root (ygg-magit-review--root))
         (held (gethash root ygg-magit-review--held))
         (target (unless held (ygg-magit-review--target)))
         (text (lambda (elsewhere)
                 (if held
                     (ygg-magit-review-comments-prompt root held elsewhere)
                   (ygg-magit-review-prompt root target elsewhere))))
         (choice (ygg-magit-review--pick root))
         (s (cond ((eq choice 'review)
                   (ygg-magit-review--spawn-review root (funcall text nil)))
                  ((eq (car-safe choice) 'new)
                   (ygg-magit-review--spawn root (cdr choice)))
                  (t choice))))
    (unless (eq choice 'review)
      (let ((elsewhere (unless (ygg-magit-review--same-repo-p
                                s (ygg-magit-review--trees root))
                         (or (aob-session-dir s) (aob-session-project s))))
            (aob-prompt-typed t))
        (aob-prompt s (funcall text elsewhere))))
    (when held (ygg-magit-review--clear-sent root held))
    (aob-trace s)))

;;; Comments on diff lines, held per repository until a send

(defun ygg-magit-review--root ()
  (file-name-as-directory
   (expand-file-name (or (magit-toplevel) (user-error "Not in a repository")))))

(defun ygg-magit-review--hunk-lines (hunk)
  "Each diff line of HUNK as (POS SIDE LINE); a removed line is on the old side.
Nil for a combined hunk, whose lines have no single side."
  (let ((from (oref hunk from-range))
        (to (oref hunk to-range)))
    (unless (or (oref hunk combined) (null from) (null to))
      (save-excursion
        (goto-char (oref hunk content))
        (let ((old (car from)) (new (car to)) (end (oref hunk end)) lines)
          (while (< (point) end)
            (pcase (char-after)
              (?- (push (list (point) 'old old) lines) (cl-incf old))
              (?+ (push (list (point) 'new new) lines) (cl-incf new))
              (?\s (push (list (point) 'new new) lines)
                   (cl-incf old) (cl-incf new)))
            (forward-line))
          (nreverse lines))))))

(defun ygg-magit-review--source (hunk)
  "What HUNK is a diff of, as (LABEL . GIT-DIFF-ARGS).
No args means LABEL is a commit."
  (pcase (magit-diff-type hunk)
    ('unstaged (list "working tree" "diff"))
    ('staged (list "staged" "diff" "--cached"))
    ('committed
     (let ((range magit-buffer-diff-range))
       (cond ((derived-mode-p 'magit-revision-mode)
              (list (or magit-buffer-revision-oid
                        (magit-commit-oid magit-buffer-revision t))))
             ((and range (string-match "\\`\\(.+\\)\\^\\.\\.\\1\\'" range))
              (list (magit-commit-oid (match-string 1 range) t)))
             (range (list range "diff" range)))))))

(defun ygg-magit-review--file (hunk side)
  (let ((file (oref hunk parent)))
    (if (eq side 'old)
        (or (oref file source) (oref file value))
      (oref file value))))

(defun ygg-magit-review--anchor ()
  "Where a comment made at point sits: file, line, side, commit and quote."
  (let ((hunk (magit-current-section)))
    (unless (and hunk (magit-section-match 'hunk hunk))
      (user-error "Not on a diff line"))
    (let* ((lines (ygg-magit-review--hunk-lines hunk))
           (at (or (cl-position (line-beginning-position) lines :key #'car)
                   (user-error "Not on a diff line")))
           (side (nth 1 (nth at lines)))
           (source (or (ygg-magit-review--source hunk)
                       (user-error "Cannot tell what this diff is of")))
           (from (car (nth (max 0 (- at 2)) lines)))
           (to (save-excursion
                 (goto-char (car (nth (min (1- (length lines)) (+ at 2)) lines)))
                 (line-end-position))))
      (list :file (ygg-magit-review--file hunk side)
            :line (nth 2 (nth at lines)) :side side
            :commit (car source) :diff (cdr source)
            :quote (buffer-substring-no-properties from to)))))

(defun ygg-magit-review--draft (name root initial take)
  "A compose draft called NAME in ROOT whose send hands its words to TAKE.
INITIAL, when given, replaces whatever an earlier draft of NAME held."
  (when-let* ((old (and initial (get-buffer (format "compose:%s" name)))))
    (kill-buffer old))
  (let ((buf (aob-compose (lambda (text _atts) (funcall take text))
                          initial name root)))
    (with-current-buffer buf
      ;; a comment is kept as typed: no preset, diff or held comment rides it
      (setq-local aob-compose-before-send-functions nil))
    buf))

(defvar-keymap ygg-magit-review-comment-mode-map
  "C-<return>" #'ygg-magit-review-comment-send-now
  "s-<return>" #'ygg-magit-review-comment-send-now)

(define-minor-mode ygg-magit-review-comment-mode
  "A compose draft that is a review comment on a diff line.
Its send holds the comment; C-return holds it and sends every comment
held for the repository, the way a trace comment box does."
  :lighter nil)

;;;###autoload
(defun ygg-magit-review-comment ()
  "Write a review comment on the diff line at point, held until the next send.
In the draft ZZ holds it and C-return holds it and sends all held."
  (interactive)
  (require 'aob)
  (let* ((root (ygg-magit-review--root))
         (anchor (ygg-magit-review--anchor))
         (where (format "%s:%d" (plist-get anchor :file) (plist-get anchor :line)))
         (buf (ygg-magit-review--draft
               (concat "review:" where) root nil
               (lambda (text) (ygg-magit-review--hold root anchor text)))))
    (with-current-buffer buf
      (ygg-magit-review-comment-mode 1)
      (setq aob-compose--label (concat "comment on: " where))
      (setq aob-compose--tags '("ZZ holds" "C-RET sends all"))
      (force-mode-line-update))
    buf))

(defun ygg-magit-review-comment-send-now ()
  "Hold this comment, then send every comment held for its repository."
  (interactive)
  (let ((root default-directory))
    (aob-compose-send)
    (let ((default-directory root))
      (ygg-magit-review-commit))))

(defun ygg-magit-review--hold (root anchor text)
  "Hold TEXT as a comment at ANCHOR in ROOT, and show it."
  (when (string-empty-p (string-trim text)) (user-error "Empty comment"))
  (puthash root
           (cons (append (list :id (cl-incf ygg-magit-review--last-id)
                               :text (string-trim text))
                         anchor)
                 (gethash root ygg-magit-review--held))
           ygg-magit-review--held)
  (ygg-magit-review--changed root))

(defun ygg-magit-review--clear-sent (root held)
  (puthash root held ygg-magit-review--sent)
  (remhash root ygg-magit-review--held)
  (ygg-magit-review--changed root))

(defun ygg-magit-review--changed (root)
  "Redraw ROOT's magit buffers and its comment list."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and (or (derived-mode-p 'magit-mode)
                     (derived-mode-p 'ygg-magit-review-list-mode))
                 (equal (file-name-as-directory (expand-file-name default-directory))
                        root))
        (if (derived-mode-p 'magit-mode)
            (ygg-magit-review--draw)
          (ygg-magit-review--list-render))))))

;;; Drawn under their lines

(defvar-local ygg-magit-review--drawn nil)

(defun ygg-magit-review--shadow (text)
  (concat "\n" (propertize (mapconcat (lambda (l) (concat "    » " l))
                                      (split-string text "\n") "\n")
                           'face 'shadow)))

(defun ygg-magit-review--draw ()
  "Show this buffer's repository's held comments under their diff lines."
  (when ygg-magit-review--drawn
    (remove-overlays (point-min) (point-max) 'ygg-magit-review t)
    (setq ygg-magit-review--drawn nil))
  (when-let* (((> (hash-table-count ygg-magit-review--held) 0))
              ((bound-and-true-p magit-root-section))
              (cs (gethash (file-name-as-directory (expand-file-name default-directory))
                           ygg-magit-review--held)))
    (magit-map-sections
     (lambda (hunk)
       (when-let* (((magit-section-match 'hunk hunk))
                   (file (oref hunk parent))
                   (mine (seq-filter
                          (lambda (c) (member (plist-get c :file)
                                              (list (oref file value)
                                                    (oref file source))))
                          cs))
                   (label (car (ygg-magit-review--source hunk))))
         (pcase-dolist (`(,pos ,side ,line) (ygg-magit-review--hunk-lines hunk))
           (dolist (c mine)
             (when (and (eq side (plist-get c :side))
                        (eql line (plist-get c :line))
                        (equal label (plist-get c :commit))
                        (equal (plist-get c :file)
                               (ygg-magit-review--file hunk side)))
               (let ((o (make-overlay pos (save-excursion
                                            (goto-char pos) (line-end-position)))))
                 (overlay-put o 'ygg-magit-review t)
                 (overlay-put o 'ygg-magit-review-id (plist-get c :id))
                 (overlay-put o 'after-string
                              (ygg-magit-review--shadow (plist-get c :text)))
                 (setq ygg-magit-review--drawn t)))))))
     magit-root-section)))

(add-hook 'magit-refresh-buffer-hook #'ygg-magit-review--draw)

;;; Sent in one go

(defun ygg-magit-review--comment-text (c)
  (concat (format "%s:%d%s · %s\n" (plist-get c :file) (plist-get c :line)
                  (if (eq (plist-get c :side) 'old) " (removed line)" "")
                  (plist-get c :commit))
          (mapconcat (lambda (l) (concat "> " l))
                     (split-string (plist-get c :quote) "\n") "\n")
          "\n" (plist-get c :text)))

(defun ygg-magit-review--source-block (root source)
  "SOURCE, (LABEL . GIT-DIFF-ARGS), as the patch it stands for in ROOT."
  (if (cdr source)
      (concat "<diff>\nrepository " root " · " (car source) "\n"
              (ygg-magit-review--capped
               (apply #'ygg-magit-review--git root (cdr source)))
              "</diff>")
    (ygg-magit-review--block root (car source))))

(defun ygg-magit-review-comments-prompt (root comments &optional elsewhere)
  "One message carrying COMMENTS made in ROOT, grouped by file.
Each patch they were made on rides once.  ELSEWHERE, when non-nil, is
where the receiving session works instead."
  (let* ((cs (sort (copy-sequence comments)
                   (lambda (a b)
                     (let ((fa (plist-get a :file)) (fb (plist-get b :file)))
                       (or (string< fa fb)
                           (and (equal fa fb)
                                (< (plist-get a :line) (plist-get b :line))))))))
         (files (delete-dups (mapcar (lambda (c) (plist-get c :file)) cs)))
         (sources (delete-dups (mapcar (lambda (c) (cons (plist-get c :commit)
                                                         (plist-get c :diff)))
                                       cs))))
    (concat ygg-magit-review-comments-instructions
            (when elsewhere
              (format "\n\nYou work in %s, not in %s where these comments \
were made, so its files may not be there to read.  Work from the quotes \
and patches below." elsewhere root))
            "\n\n<review-comments>\n"
            (mapconcat
             (lambda (f)
               (concat "## " f "\n\n"
                       (mapconcat #'ygg-magit-review--comment-text
                                  (seq-filter (lambda (c) (equal (plist-get c :file) f))
                                              cs)
                                  "\n\n")))
             files "\n\n")
            "\n</review-comments>\n\n"
            (mapconcat (lambda (src) (ygg-magit-review--source-block root src))
                       sources "\n\n"))))

;;; The list of held comments

(defvar-keymap ygg-magit-review-list-mode-map
  "RET" #'ygg-magit-review-list-visit
  "j" #'next-line
  "k" #'previous-line
  "d" #'ygg-magit-review-list-drop
  "e" #'ygg-magit-review-list-edit
  "D" #'ygg-magit-review-list-drop-all
  "u" #'ygg-magit-review-list-undo
  "@" #'ygg-magit-review-commit)

(define-derived-mode ygg-magit-review-list-mode special-mode "Review"
  "The review comments held for one repository, one row each."
  (setq-local revert-buffer-function
              (lambda (&rest _) (ygg-magit-review--list-render)))
  (setq header-line-format
        (propertize " RET visit · e edit · d drop · D drop all · u undo send · @ send"
                    'face 'shadow)))

(defun ygg-magit-review--short (commit)
  (if (string-match-p "\\`[0-9a-f]\\{40,\\}\\'" commit) (substring commit 0 8) commit))

(defun ygg-magit-review--list-render ()
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (cs (reverse (gethash root ygg-magit-review--held)))
         (sent (gethash root ygg-magit-review--sent))
         (line (line-number-at-pos))
         (inhibit-read-only t))
    (erase-buffer)
    (dolist (c cs)
      (insert (propertize
               (format "%s:%d%s  %s  %s\n" (plist-get c :file) (plist-get c :line)
                       (if (eq (plist-get c :side) 'old) " (old)" "")
                       (propertize (ygg-magit-review--short (plist-get c :commit))
                                   'face 'shadow)
                       (car (split-string (plist-get c :text) "\n")))
               'ygg-magit-review-id (plist-get c :id))))
    (unless cs
      (insert (propertize (if sent
                              (format "No review comments held; u brings back the %d sent.\n"
                                      (length sent))
                            "No review comments held.\n")
                          'face 'shadow)))
    (goto-char (point-min))
    (forward-line (1- (min line (max 1 (length cs)))))))

;;;###autoload
(defun ygg-magit-review-list ()
  "List the review comments held for this repository."
  (interactive)
  (let* ((root (ygg-magit-review--root))
         (buf (get-buffer-create
               (format "*review: %s*"
                       (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buf
      (unless (derived-mode-p 'ygg-magit-review-list-mode)
        (ygg-magit-review-list-mode))
      (setq default-directory root)
      (ygg-magit-review--list-render))
    (pop-to-buffer buf)))

(defun ygg-magit-review--list-root ()
  (file-name-as-directory (expand-file-name default-directory)))

(defun ygg-magit-review--at-point ()
  "The held comment on this row."
  (let ((id (or (get-text-property (line-beginning-position) 'ygg-magit-review-id)
                (user-error "No comment here"))))
    (seq-find (lambda (c) (eql (plist-get c :id) id))
              (gethash (ygg-magit-review--list-root) ygg-magit-review--held))))

(defun ygg-magit-review--drawn-at (root id)
  "The overlay showing comment ID in one of ROOT's magit buffers, shown ones first."
  (let (found)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (and (derived-mode-p 'magit-mode)
                   (equal (ygg-magit-review--list-root) root))
          (when-let* ((o (seq-find (lambda (o) (eql (overlay-get o 'ygg-magit-review-id) id))
                                   (overlays-in (point-min) (point-max)))))
            (push o found)))))
    (or (seq-find (lambda (o) (get-buffer-window (overlay-buffer o))) found)
        (car found))))

(defun ygg-magit-review-list-visit ()
  "Go to the diff line this comment is on, or to its line in the file."
  (interactive)
  (let* ((c (ygg-magit-review--at-point))
         (root (ygg-magit-review--list-root)))
    (if-let* ((o (ygg-magit-review--drawn-at root (plist-get c :id))))
        (progn (pop-to-buffer (overlay-buffer o))
               (goto-char (overlay-start o)))
      (find-file-other-window (expand-file-name (plist-get c :file) root))
      (goto-char (point-min))
      (forward-line (1- (plist-get c :line))))))

(defun ygg-magit-review-list-drop ()
  "Drop the comment on this row."
  (interactive)
  (let ((c (ygg-magit-review--at-point))
        (root (ygg-magit-review--list-root)))
    (puthash root (delq c (gethash root ygg-magit-review--held))
             ygg-magit-review--held)
    (ygg-magit-review--changed root)))

(defun ygg-magit-review-list-edit ()
  "Rewrite the comment on this row in a draft."
  (interactive)
  (let ((c (ygg-magit-review--at-point))
        (root (ygg-magit-review--list-root)))
    (ygg-magit-review--draft
     (format "review-edit:%d" (plist-get c :id)) root (plist-get c :text)
     (lambda (text)
       (when (string-empty-p (string-trim text)) (user-error "Empty comment"))
       (plist-put c :text (string-trim text))
       (ygg-magit-review--changed root)))))

(defun ygg-magit-review-list-drop-all ()
  "Drop every comment held for this repository."
  (interactive)
  (let ((root (ygg-magit-review--list-root)))
    (when (and (gethash root ygg-magit-review--held)
               (y-or-n-p "Drop every review comment held here? "))
      (remhash root ygg-magit-review--held)
      (ygg-magit-review--changed root))))

(defun ygg-magit-review-list-undo ()
  "Bring back the comments the last send cleared, once."
  (interactive)
  (let* ((root (ygg-magit-review--list-root))
         (sent (or (gethash root ygg-magit-review--sent)
                   (user-error "No sent comments to bring back"))))
    (puthash root (append (gethash root ygg-magit-review--held) sent)
             ygg-magit-review--held)
    (remhash root ygg-magit-review--sent)
    (ygg-magit-review--changed root)))

(keymap-set magit-commit-section-map "@" #'ygg-magit-review-commit)
(keymap-set magit-log-mode-map "@" #'ygg-magit-review-commit)
(keymap-set magit-diff-mode-map "@" #'ygg-magit-review-commit)
(keymap-set magit-revision-mode-map "@" #'ygg-magit-review-commit)
(keymap-set magit-status-mode-map "@" #'ygg-magit-review-commit)
;; hunk and file maps inherit this one; add-log moves aside to stay reachable
(keymap-set magit-diff-section-map "C" #'ygg-magit-review-comment)
(keymap-set magit-diff-section-map "," #'magit-commit-add-log)
(keymap-set magit-mode-map ";" #'ygg-magit-review-list)

(unless (ignore-errors (transient-get-suffix 'magit-dispatch "@"))
  (transient-append-suffix 'magit-dispatch "!"
    '("@" "Review by agent, or send held comments" ygg-magit-review-commit)))
(unless (ignore-errors (transient-get-suffix 'magit-dispatch ";"))
  (transient-append-suffix 'magit-dispatch "@"
    '(";" "Review comments" ygg-magit-review-list)))

(provide 'ygg-magit-review)
;;; ygg-magit-review.el ends here
