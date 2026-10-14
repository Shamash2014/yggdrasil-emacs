;;; ygg-git-compare-comments.el --- review comments on a compare -*- lexical-binding: t; -*-

;;; Commentary:
;; Comments on a diff line, a range of lines, a file or the review as a
;; whole, shown under what each is on and kept in the repository's git
;; dir for the compare, so they wait for the next time it is opened.  An
;; agent may propose comments of its own; they stay pending until checked.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'transient)
(require 'ygg-git-compare)
(require 'ygg-comment)

(declare-function yggdrasil-define-mode-keys "yggdrasil-core")
(declare-function ygg-insert-state "yggdrasil-core")

(defcustom ygg-git-compare-comment-types '(issue suggestion nit question praise)
  "The types a comment can be given; a comment is untyped until one is picked."
  :type '(repeat symbol)
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-agent-types '(todo fix)
  "The comment types that go to an agent; every other comment goes to the PR."
  :type '(repeat symbol)
  :group 'ygg-git-compare)

(defvar-local ygg-git-compare--store nil
  "(FILE . KEY) this compare's comments are kept under.")

(defun ygg-git-compare-comment-types ()
  "The types a comment can be given, the agent ones included."
  (seq-union ygg-git-compare-comment-types ygg-git-compare-agent-types))

;;; Where comments are kept

(defun ygg-git-compare--store-file (&optional dir)
  "The file DIR's repository keeps review comments in, shared by its worktrees."
  (expand-file-name "ygg-review-comments.eld"
                    (or (magit-gitdir dir t) (user-error "Not in a git repository"))))

(defun ygg-git-compare--branch-review-key (branch)
  "The key BRANCH's review comments are kept under, however a remote names it."
  (concat "branch "
          (if-let* ((remote (seq-find (lambda (r) (string-prefix-p (concat r "/") branch))
                                      (magit-list-remotes))))
              (substring branch (1+ (length remote)))
            branch)))

(defun ygg-git-compare--compare-key ()
  "The key this compare's comments are kept under: B's branch when it is one."
  (if-let* ((branch (ygg-git-compare--b-branch)))
      (ygg-git-compare--branch-review-key branch)
    (format "%s ↔ %s" (plist-get ygg-git-compare--a :label)
            (plist-get ygg-git-compare--b :label))))

(defun ygg-git-compare--store-read (file &optional lenient)
  "What FILE keeps, by key.  A file that cannot be read is copied aside and
is a user error, or with LENIENT keeps nothing."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (condition-case nil
             (and (re-search-forward "[^[:space:]]" nil t)
                  (progn (goto-char (point-min)) (read (current-buffer))))
           (error
            (unless lenient
              (let ((aside (format "%s.corrupt-%s" file
                                   (format-time-string "%Y%m%dT%H%M%S"))))
                (copy-file file aside t)
                (user-error "Review comments unreadable, kept as %s" aside))))))))

(defun ygg-git-compare--store-update (file key fn)
  "Give FN KEY's comments in FILE, oldest first, keep what it answers and
answer that."
  (let* ((stored (ygg-git-compare--store-read file))
         (comments (funcall fn (cdr (assoc key stored))))
         (alist (assoc-delete-all key stored)))
    (let ((print-length nil) (print-level nil) (coding-system-for-write 'utf-8))
      (let ((temp (make-temp-file file)))
        (with-temp-file temp
          (prin1 (if comments (cons (cons key comments) alist) alist) (current-buffer))
          (insert "\n"))
        (rename-file temp file t)))
    comments))

(defun ygg-git-compare--reload ()
  (setq ygg-git-compare--comments
        (reverse (cdr (assoc (cdr ygg-git-compare--store)
                             (ygg-git-compare--store-read (car ygg-git-compare--store) t))))))

(defun ygg-git-compare--load-comments ()
  "Read the comments kept for this compare and show them."
  (setq ygg-git-compare--store (cons (ygg-git-compare--store-file)
                                     (ygg-git-compare--compare-key)))
  (ygg-git-compare--reload)
  (ygg-git-compare--redraw-comments (current-buffer)))

(defun ygg-git-compare--change (fn)
  "Give FN the compare's comments as kept, oldest first; keep and show what
it answers."
  (with-current-buffer (ygg-git-compare--list)
    (setq ygg-git-compare--comments
          (reverse (ygg-git-compare--store-update (car ygg-git-compare--store)
                                                  (cdr ygg-git-compare--store) fn)))
    (ygg-git-compare--redraw-comments (current-buffer))))

(defun ygg-git-compare--put (comment)
  "Keep COMMENT, in place of the one with its id."
  (let ((id (plist-get comment :id)))
    (ygg-git-compare--change
     (lambda (comments)
       (if (seq-find (lambda (c) (equal (plist-get c :id) id)) comments)
           (mapcar (lambda (c) (if (equal (plist-get c :id) id) comment c)) comments)
         (append comments (list comment)))))))

(defun ygg-git-compare-comments-list (&optional include-pending)
  "The comments of this compare, oldest first; those pending a check only
with INCLUDE-PENDING."
  (with-current-buffer (ygg-git-compare--list)
    (seq-remove (lambda (c) (and (not include-pending) (ygg-git-compare--pending-p c)))
                (reverse ygg-git-compare--comments))))

(defun ygg-git-compare-comments-drop (ids)
  "Drop the comments with IDS from this compare."
  (ygg-git-compare--change
   (lambda (comments) (seq-remove (lambda (c) (member (plist-get c :id) ids)) comments))))

;;; Comments

(defvar ygg-git-compare--id-count 0
  "Comment ids made this session.")

(defun ygg-git-compare--new-id ()
  (format "%x%04x%x" (truncate (* 1000 (float-time))) (random #x10000)
          (cl-incf ygg-git-compare--id-count)))

(defun ygg-git-compare--pending-p (comment)
  (eq (plist-get comment :status) 'pending))

(defun ygg-git-compare--stale-p (comment)
  "Whether COMMENT was made on another range than this compare's."
  (when-let* ((range (plist-get comment :range)))
    (not (equal range (with-current-buffer (ygg-git-compare--list)
                        (ygg-git-compare--range-label))))))

(defun ygg-git-compare--forge-p (comment)
  "Whether COMMENT goes to the pull request rather than an agent."
  (not (memq (plist-get comment :type) ygg-git-compare-agent-types)))

(defun ygg-git-compare--audience (comment)
  "Where COMMENT goes: \"PR\" or \"agent\"."
  (if (ygg-git-compare--forge-p comment) "PR" "agent"))

(defun ygg-git-compare--flipped (comment)
  "COMMENT sent to the other of the pull request and the agent, the type it had
for the pull request kept to give back."
  (if (ygg-git-compare--forge-p comment)
      (plist-put (plist-put (copy-sequence comment) :pr-type (plist-get comment :type))
                 :type (car ygg-git-compare-agent-types))
    (plist-put (plist-put (copy-sequence comment) :type (plist-get comment :pr-type))
               :pr-type nil)))

(defun ygg-git-compare--new-comment (level anchor)
  (append (list :id (ygg-git-compare--new-id) :level level :type nil :text nil)
          anchor
          (list :range (with-current-buffer (ygg-git-compare--list)
                         (ygg-git-compare--range-label))
                :created (float-time))))

(defun ygg-git-compare--hunk-lines (hunk)
  "Each diff line of HUNK as (POS SIDE LINE), a removed line on the old side;
a context line also carries its old line, as (POS new LINE OLD-LINE)."
  (save-excursion
    (goto-char (oref hunk content))
    (let ((old (car (oref hunk from-range)))
          (new (car (oref hunk to-range)))
          lines)
      (while (< (point) (oref hunk end))
        (pcase (char-after)
          (?- (push (list (point) 'old old) lines) (cl-incf old))
          (?+ (push (list (point) 'new new) lines) (cl-incf new))
          (?\s (push (list (point) 'new new old) lines) (cl-incf old) (cl-incf new)))
        (forward-line))
      (nreverse lines))))

(defun ygg-git-compare--anchor ()
  "The diff line at point as a comment's place: file, side, line and quote,
the file's path on each side, and a context line's old line."
  (let* ((hunk (magit-current-section))
         (lines (and hunk (magit-section-match 'hunk hunk)
                     (oref hunk from-range) (oref hunk to-range)
                     (ygg-git-compare--hunk-lines hunk)))
         (at (or (cl-position (line-beginning-position) lines :key #'car)
                 (user-error "Not on a diff line")))
         (side (nth 1 (nth at lines)))
         (file (oref hunk parent))
         (old-path (or (oref file source) (oref file value))))
    (list :file (if (eq side 'old) old-path (oref file value))
          :old-path old-path
          :new-path (oref file value)
          :side side
          :line (nth 2 (nth at lines))
          :old-line (nth 3 (nth at lines))
          :quote (buffer-substring-no-properties
                  (car (nth (max 0 (- at 2)) lines))
                  (save-excursion
                    (goto-char (car (nth (min (1- (length lines)) (+ at 2)) lines)))
                    (line-end-position))))))

(defun ygg-git-compare--range-anchor (beg end)
  "The diff lines from BEG's to END's, both included, as a range's place."
  (let ((start (save-excursion (goto-char beg) (ygg-git-compare--anchor)))
        (stop (save-excursion (goto-char end) (ygg-git-compare--anchor))))
    (unless (equal (plist-get start :new-path) (plist-get stop :new-path))
      (user-error "A range stays within one file"))
    (append (list :start-side (plist-get start :side)
                  :start-line (plist-get start :line)
                  :start-old-line (plist-get start :old-line))
            (cl-loop for (key value) on stop by #'cddr
                     unless (eq key :quote) append (list key value))
            (list :quote (buffer-substring-no-properties
                          (save-excursion (goto-char beg) (line-beginning-position))
                          (save-excursion (goto-char end) (line-end-position)))))))

(defun ygg-git-compare--hunk-anchor ()
  "The hunk around point as a comment's level and place: a range from its first
changed line to its last, a line when it changed only one."
  (let* ((hunk (let ((section (magit-current-section)))
                 (while (and section (not (magit-section-match 'hunk section)))
                   (setq section (oref section parent)))
                 section))
         (changed (and hunk (oref hunk from-range) (oref hunk to-range)
                       (seq-remove (lambda (line) (nth 3 line))
                                   (ygg-git-compare--hunk-lines hunk))))
         (beg (car (car changed)))
         (end (car (car (last changed)))))
    (unless changed (user-error "Not in a hunk with changes"))
    (if (= beg end)
        (cons 'line (save-excursion (goto-char beg) (ygg-git-compare--anchor)))
      (cons 'range (ygg-git-compare--range-anchor beg end)))))

(defun ygg-git-compare--file-anchor (section)
  (let ((new (oref section value)))
    (list :file new
          :old-path (or (and (slot-exists-p section 'source) (oref section source)) new)
          :new-path new)))

(defun ygg-git-compare--where (comment)
  (let ((level (plist-get comment :level))
        (file (plist-get comment :file)))
    (concat (pcase level
              ('review "review")
              ('file file)
              ('range (format "%s:%s-%s" file (plist-get comment :start-line)
                              (plist-get comment :line)))
              (_ (format "%s:%s" file (plist-get comment :line))))
            (if (and (memq level '(nil line range)) (eq (plist-get comment :side) 'old))
                " (removed line)" ""))))

(defun ygg-git-compare--for-prompt (comment here)
  "COMMENT as an agent reads it in a review of HERE, a range label."
  (let ((range (plist-get comment :range))
        (quote (plist-get comment :quote))
        (title (plist-get comment :title)))
    (concat (when-let* ((type (plist-get comment :type))) (format "[%s] " type))
            (ygg-git-compare--where comment)
            (when (and range (not (equal range here))) (concat " · made on " range))
            "\n"
            (when quote (concat (replace-regexp-in-string "^" "> " quote) "\n"))
            (when title (concat title "\n"))
            (plist-get comment :text))))

;;; Writing a comment

(declare-function aob-comment-box "aob")
(defvar aob-compose--tags)

(defvar-local ygg-git-compare--draft nil "The comment this box writes, a plist.")
(defvar-local ygg-git-compare--draft-list nil "The compare the draft is kept on.")
(defvar-local ygg-git-compare--draft-existing nil "Whether the draft edits a comment already kept.")

(defvar-keymap ygg-git-compare-box-mode-map)

(define-minor-mode ygg-git-compare-box-mode
  "A comment box that writes a review comment, typed with TAB in normal state; \\ d sends it to the PR or the agent."
  :lighter nil)

(defvar-local ygg-git-compare--draft-files nil "The compare's changed files, once read.")
(defvar-local ygg-git-compare--draft-words nil "The compare's words, once read.")
(defvar cape-dabbrev-buffer-function)
(defvar corfu-auto-prefix)
(declare-function cape-dabbrev "cape" (&optional interactive))

(defun ygg-git-compare--draft-buffers ()
  "The live panes of the compare the draft is kept on."
  (when-let* ((list ygg-git-compare--draft-list)
              ((buffer-live-p list)))
    (cons list (seq-filter (lambda (b) (eq (buffer-local-value 'ygg-git-compare--list-buffer b)
                                           list))
                           (buffer-list)))))

(defun ygg-git-compare--draft-put (draft)
  "Make DRAFT the comment this box writes and say so in its tags."
  (setq ygg-git-compare--draft draft)
  (setq aob-compose--tags (ygg-git-compare--box-tags))
  (force-mode-line-update))

(defun ygg-git-compare--draft-set-type (name status)
  "Give the draft the type NAME, completed at its start, in place of the word."
  (when (memq status '(finished exact sole))
    (save-excursion
      (goto-char (point-min))
      (when (looking-at (concat (regexp-quote name) "[[:blank:]]*"))
        (delete-region (match-beginning 0) (match-end 0))))
    (ygg-git-compare--draft-put (plist-put ygg-git-compare--draft :type (intern name)))))

(defun ygg-git-compare--draft-lead-type (text)
  "TEXT without the type word it starts with, which types an untyped draft."
  (if (and (not (plist-get ygg-git-compare--draft :type))
           (string-match (concat "\\`" (regexp-opt (mapcar #'symbol-name (ygg-git-compare-comment-types)) t)
                                 "\\(?::[[:blank:]]*\\|[[:blank:]]+\\)")
                         text))
      (prog1 (substring text (match-end 0))
        (ygg-git-compare--draft-put
         (plist-put ygg-git-compare--draft :type (intern (downcase (match-string 1 text))))))
    text))

(defun ygg-git-compare-draft-type-capf ()
  "Complete the comment's type in the word the draft starts with."
  (when (save-excursion (skip-chars-backward "[:alpha:]") (bobp))
    (list (point-min)
          (save-excursion (skip-chars-forward "[:alpha:]") (point))
          (ygg-git-compare-table (mapcar (lambda (type) (list (symbol-name type)))
                                         (ygg-git-compare-comment-types))
                                 'ygg-review-comment-type)
          :annotation-function (lambda (_) " type")
          :exit-function #'ygg-git-compare--draft-set-type
          :exclusive 'no)))

(defun ygg-git-compare--draft-changed-files ()
  (with-memoization ygg-git-compare--draft-files
    (when-let* ((list (car (ygg-git-compare--draft-buffers))))
      (with-current-buffer list
        (delete-dups
         (append (and magit-root-section
                      (seq-keep (lambda (s) (and (magit-section-match 'file s) (oref s value)))
                                (oref magit-root-section children)))
                 (plist-get ygg-git-compare--plan :untracked)))))))

(defun ygg-git-compare-draft-file-capf ()
  "Complete a path the compare changes."
  (let ((beg (save-excursion (skip-chars-backward "^[:space:]`'\"()") (point)))
        (end (save-excursion (skip-chars-forward "^[:space:]`'\"()") (point))))
    (when (and (< beg end) (ygg-git-compare--draft-changed-files))
      (list beg end
            (ygg-git-compare-table (mapcar #'list (ygg-git-compare--draft-changed-files))
                                   'file)
            :exclusive 'no))))

(defun ygg-git-compare--draft-compare-words ()
  (with-memoization ygg-git-compare--draft-words
    (let ((words (make-hash-table :test #'equal)))
      (dolist (buffer (ygg-git-compare--draft-buffers))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "[[:alpha:]_][[:alnum:]_-]\\{2,\\}" nil t)
              (puthash (match-string-no-properties 0) t words)))))
      (hash-table-keys words))))

(defun ygg-git-compare-draft-word-capf ()
  "Complete a word or identifier from the compare's diffs."
  (if (fboundp 'cape-dabbrev)
      (cape-dabbrev)
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
      (list (car bounds) (cdr bounds)
            (ygg-git-compare-table (mapcar #'list (ygg-git-compare--draft-compare-words))
                                   'ygg-review-word)
            :exclusive 'no))))

(with-eval-after-load 'yggdrasil-core
  (yggdrasil-define-mode-keys 'ygg-git-compare-box-mode 'normal
                              "TAB" #'ygg-git-compare-draft-cycle-type
                              "<tab>" #'ygg-git-compare-draft-cycle-type
                              "<backtab>" #'ygg-git-compare-draft-cycle-type-back))

(with-eval-after-load 'yggdrasil-localleader
  (yggdrasil-localleader-def 'aob-compose-mode "d" #'ygg-git-compare-draft-flip
                             "comment: PR ⇄ agent"))

(defun ygg-git-compare--type-face (type)
  (pcase type
    ('issue 'error)
    ('suggestion 'warning)
    ('question 'font-lock-keyword-face)
    ('praise 'success)
    ('nit 'shadow)
    (_ 'font-lock-type-face)))

(defun ygg-git-compare--box-tags ()
  "What the comment box says after its title: the type, then the keys."
  (list (if-let* ((type (plist-get ygg-git-compare--draft :type)))
            (format "[%s]" type)
          "untyped")
        (concat "→ " (ygg-git-compare--audience ygg-git-compare--draft))
        "TAB type" "ZZ saves" "empty drops it"))

(defun ygg-git-compare--keep (comment list text &optional existing)
  "Keep COMMENT with TEXT on the compare LIST; with no text, drop it.
EXISTING says COMMENT was already kept when its box opened."
  (unless (buffer-live-p list) (user-error "Its compare is gone"))
  (with-current-buffer list
    (let ((id (plist-get comment :id)))
      (when (and existing
                 (not (seq-find (lambda (c) (equal (plist-get c :id) id))
                                (ygg-git-compare-comments-list t))))
        (user-error "That comment is gone"))
      (cond ((not (string-empty-p text))
             (ygg-git-compare--put
              (ygg-git-compare--accepted (plist-put (copy-sequence comment) :text text))))
            ((seq-find (lambda (c) (equal (plist-get c :id) id))
                       (ygg-git-compare-comments-list t))
             (ygg-git-compare-comments-drop (list id)))
            (t (user-error "Empty comment, ZQ drops it"))))))

(defun ygg-git-compare--box-name (comment list)
  "The name of the box that writes COMMENT on the compare LIST.
A new comment is keyed on its compare and place, so asking again there
finds the draft."
  (if (ygg-git-compare--kept-p comment list)
      (format "review:%s" (plist-get comment :id))
    (format "review:new:%s"
            (md5 (prin1-to-string
                  (cons (buffer-name list)
                        (mapcar (lambda (key) (plist-get comment key))
                                '(:level :file :side :line :start-side :start-line))))))))

(defun ygg-git-compare--kept-p (comment list)
  (with-current-buffer list
    (seq-find (lambda (c) (equal (plist-get c :id) (plist-get comment :id)))
              (ygg-git-compare-comments-list t))))

(defun ygg-git-compare--compose (comment)
  "Write COMMENT's text in the comment box under point and answer the box.
Saving keeps the comment on the compare; saving nothing drops it."
  (require 'aob)
  (let* ((list (ygg-git-compare--list))
         (name (ygg-git-compare--box-name comment list))
         (existing (and (ygg-git-compare--kept-p comment list) t))
         (fresh (not (get-buffer (format "compose:%s" name))))
         (box (aob-comment-box
               name
               (concat "comment on: " (ygg-git-compare--where comment))
               (lambda (text)
                 (let ((text (ygg-git-compare--draft-lead-type text)))
                   (ygg-git-compare--keep ygg-git-compare--draft
                                          ygg-git-compare--draft-list text
                                          ygg-git-compare--draft-existing)))
               :initial (plist-get comment :text)
               :placeholder "Write the comment; saving it empty drops it")))
    (when fresh
      (with-current-buffer box
        (setq ygg-git-compare--draft (copy-sequence comment)
              ygg-git-compare--draft-list list
              ygg-git-compare--draft-existing existing)
        (ygg-git-compare-box-mode 1)
        (add-hook 'completion-at-point-functions #'ygg-git-compare-draft-type-capf -30 t)
        (add-hook 'completion-at-point-functions #'ygg-git-compare-draft-file-capf -20 t)
        (add-hook 'completion-at-point-functions #'ygg-git-compare-draft-word-capf -10 t)
        (setq-local cape-dabbrev-buffer-function #'ygg-git-compare--draft-buffers)
        (setq aob-compose--tags (ygg-git-compare--box-tags))))
    box))

(defun ygg-git-compare--next-type (type step)
  (let ((types (cons nil (ygg-git-compare-comment-types))))
    (nth (mod (+ (or (cl-position type types) 0) step) (length types)) types)))

(defun ygg-git-compare-draft-cycle-type (&optional step)
  "Give the comment the next type, or STEP types on; untyped comes between."
  (interactive)
  (ygg-git-compare--draft-put
   (plist-put ygg-git-compare--draft :type
              (ygg-git-compare--next-type (plist-get ygg-git-compare--draft :type)
                                          (or step 1)))))

(defun ygg-git-compare-draft-flip ()
  "Send the comment to the other of the pull request and the agent."
  (interactive)
  (unless (bound-and-true-p ygg-git-compare-box-mode)
    (user-error "Not a review comment"))
  (ygg-git-compare--draft-put (ygg-git-compare--flipped ygg-git-compare--draft)))

(defun ygg-git-compare-draft-cycle-type-back ()
  "Give the comment the type before."
  (interactive)
  (ygg-git-compare-draft-cycle-type -1))

;;; Commenting

(defun ygg-git-compare-comment ()
  "Comment on the diff line at point, on the lines selected, on the file whose
heading point is on, or on the hunk whose heading it is.  On a comment of
yours, write it again in the same box; saving it empty deletes it."
  (interactive)
  (if-let* ((own (and (not (region-active-p)) (ygg-git-compare--own-here))))
      (ygg-git-compare--compose own)
    (when (derived-mode-p 'ygg-git-compare-comments-summary-mode)
      (user-error "No comment here"))
    (ygg-git-compare-comment-new)))

(defun ygg-git-compare--own-here ()
  "The comment of yours shown on this line, asked for among several.
A comment on the review as a whole is edited from the summary or the dispatch."
  (let ((summary (derived-mode-p 'ygg-git-compare-comments-summary-mode)))
    (when (seq-some (lambda (c) (or summary (not (eq (plist-get c :level) 'review))))
                    (ygg-git-compare--comments-at-point))
      (ygg-git-compare--comment-at-point
       (lambda (c) (or summary (not (eq (plist-get c :level) 'review))))))))

(defun ygg-git-compare-comment-new ()
  "Start a comment on the diff line at point, on the lines selected, on the file
whose heading point is on, or on the hunk whose heading it is."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((region-active-p)
      (let ((beg (region-beginning)) (end (region-end)))
        (deactivate-mark)
        (if (= (save-excursion (goto-char beg) (line-beginning-position))
               (save-excursion (goto-char end) (line-beginning-position)))
            (save-excursion
              (goto-char beg)
              (ygg-git-compare--compose
               (ygg-git-compare--new-comment 'line (ygg-git-compare--anchor))))
          (ygg-git-compare--compose
           (ygg-git-compare--new-comment 'range (ygg-git-compare--range-anchor beg end))))))
     ((and section (magit-section-match 'file section))
      (ygg-git-compare--compose
       (ygg-git-compare--new-comment 'file (ygg-git-compare--file-anchor section))))
     ((and section (magit-section-match 'hunk section)
           (= (line-beginning-position) (oref section start)))
      (ygg-git-compare-comment-hunk))
     (t (ygg-git-compare--compose
         (ygg-git-compare--new-comment 'line (ygg-git-compare--anchor)))))))

(defun ygg-git-compare-comment-hunk ()
  "Comment on the hunk at point as a whole."
  (interactive)
  (pcase-let ((`(,level . ,anchor) (ygg-git-compare--hunk-anchor)))
    (ygg-git-compare--compose (ygg-git-compare--new-comment level anchor))))

(defun ygg-git-compare-comment-file ()
  "Comment on the file at point as a whole."
  (interactive)
  (let ((section (magit-current-section)))
    (while (and section (not (magit-section-match 'file section)))
      (setq section (oref section parent)))
    (ygg-git-compare--compose
     (ygg-git-compare--new-comment 'file (ygg-git-compare--file-anchor
                                          (or section (user-error "Not on a file")))))))

(defun ygg-git-compare-comment-review ()
  "Write the comment on the review as a whole, or edit the one written."
  (interactive)
  (ygg-git-compare--compose
   (or (seq-find (lambda (c) (eq (plist-get c :level) 'review))
                 (ygg-git-compare-comments-list))
       (ygg-git-compare--new-comment 'review nil))))

(defvar-keymap ygg-git-compare-selection-map
  "j" #'ygg-git-compare-select-down
  "k" #'ygg-git-compare-select-up
  "x" #'ygg-git-compare-select-down
  "<down>" #'ygg-git-compare-select-down
  "<up>" #'ygg-git-compare-select-up
  "v" #'ygg-git-compare-select-lines
  "V" #'ygg-git-compare-select-lines
  "<escape>" #'ygg-git-compare-select-lines
  "C" #'ygg-git-compare-comment
  "RET" #'ygg-git-compare-comment)

(defvar ygg--modeline-tag)
(defvar ygg--tags)

(defvar-local ygg-git-compare--tag-before nil
  "The mode line tag a selection replaced.")

(defun ygg-git-compare--selection-tag (on)
  "Show the modal layer's VISUAL tag while ON, the tag from before after."
  (when (boundp 'ygg--modeline-tag)
    (if on
        (setq-local ygg-git-compare--tag-before ygg--modeline-tag
                    ygg--modeline-tag (or (alist-get 'visual (bound-and-true-p ygg--tags))
                                          " VISUAL "))
      (setq-local ygg--modeline-tag (or ygg-git-compare--tag-before "")))
    (force-mode-line-update)))

(defvar-local ygg-git-compare--selection-exit nil
  "Drops the selection's keys.")

(defun ygg-git-compare--selection-end ()
  "Drop the selection's keys now, not after the next key has used them."
  (remove-hook 'deactivate-mark-hook #'ygg-git-compare--selection-end t)
  (when-let* ((exit (prog1 ygg-git-compare--selection-exit
                      (setq ygg-git-compare--selection-exit nil))))
    (funcall exit)))

(defun ygg-git-compare-select-lines ()
  "Select diff lines from this one for a range comment; again, v or Esc
ends it."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (let ((buffer (current-buffer)))
      (beginning-of-line)
      (push-mark (point) t t)
      (ygg-git-compare--selection-tag t)
      (add-hook 'deactivate-mark-hook #'ygg-git-compare--selection-end nil t)
      (setq ygg-git-compare--selection-exit
            (set-transient-map ygg-git-compare-selection-map
                               (lambda () (and (eq (current-buffer) buffer)
                                               (bound-and-true-p ygg-git-compare-mode)
                                               (region-active-p)))
                               (lambda () (when (buffer-live-p buffer)
                                            (with-current-buffer buffer
                                              (ygg-git-compare--selection-tag nil)))))))))

(defun ygg-git-compare-select-down ()
  "Take the next diff line into the selection."
  (interactive)
  (forward-line 1))

(defun ygg-git-compare-select-up ()
  "Take the diff line before into the selection."
  (interactive)
  (forward-line -1))

;;; Showing comments

(defun ygg-git-compare--priority-face (priority)
  (pcase priority ((or 0 1) 'error) (2 'warning) (_ 'shadow)))

(defun ygg-git-compare--verdict (comment)
  (when-let* ((correctness (plist-get comment :correctness)))
    (let ((wrong (string-search "incorrect" correctness))
          (confidence (plist-get comment :confidence)))
      (propertize (concat (if wrong "✗ " "✓ ") correctness
                          (and confidence (format " · %s" confidence)))
                  'face (if wrong 'error 'success)))))

(defun ygg-git-compare--remote-id-p (id)
  (and (stringp id) (string-prefix-p "remote:" id)))

(defvar ygg-git-compare--mark-drafts nil
  "Whether the comments of the compare show as drafts, as they do beside
those of the forge.")

(declare-function ygg-git-compare--remote-comments "ygg-git-compare-threads" (list))
(declare-function ygg-git-compare--remote-block "ygg-git-compare-threads" (comment))
(autoload 'ygg-git-compare--remote-comments "ygg-git-compare-threads")
(autoload 'ygg-git-compare--conversation-spans "ygg-git-compare-pr-info")
(autoload 'ygg-git-compare--remote-block "ygg-git-compare-threads")
(defvar ygg-git-compare--pr-info-order)
(autoload 'ygg-git-compare--remote-shown "ygg-git-compare-threads")
(autoload 'ygg-git-compare--goto-remote "ygg-git-compare-threads")
(autoload 'ygg-git-compare-threads-toggle-resolved "ygg-git-compare-threads" nil t)
(autoload 'ygg-git-compare-threads-open "ygg-git-compare-threads" nil t)
(autoload 'ygg-git-compare-threads-copy "ygg-git-compare-threads" nil t)
(autoload 'ygg-git-compare-threads-reply "ygg-git-compare-threads" nil t)
(autoload 'ygg-git-compare-threads-toggle-fold "ygg-git-compare-threads" nil t)

(defun ygg-git-compare--remote-here-p ()
  "Whether a comment from the forge is shown on this line, wherever on it
point is."
  (ignore-errors
    (seq-some (lambda (ov)
                (seq-some #'ygg-git-compare--remote-id-p
                          (overlay-get ov 'ygg-git-compare-comments)))
              (overlays-in (line-beginning-position) (line-end-position)))))

(defun ygg-git-compare--bind-on-remote (key command)
  "Bind KEY in the compare's map to COMMAND on a line a comment from the forge
is on, anywhere on the line, and to what it was otherwise."
  (let* ((bound (keymap-lookup ygg-git-compare-mode-map key))
         (original (if (eq (car-safe bound) 'menu-item) (nth 2 bound) bound)))
    (keymap-set ygg-git-compare-mode-map key
                `(menu-item "" ,original
                            :filter ,(lambda (original)
                                       (if (ygg-git-compare--remote-here-p) command original))))))

(dolist (binding '(("o" . ygg-git-compare-threads-open)
                   ("y" . ygg-git-compare-threads-copy)
                   ("r" . ygg-git-compare-threads-reply)
                   ("TAB" . ygg-git-compare-threads-toggle-fold)
                   ("<tab>" . ygg-git-compare-threads-toggle-fold)))
  (ygg-git-compare--bind-on-remote (car binding) (cdr binding)))
(keymap-set ygg-git-compare-mode-map "H" #'ygg-git-compare-threads-toggle-resolved)

(defun ygg-git-compare--place-chips (comment)
  "COMMENT's place as its file's name with the lines, and the directory apart."
  (let* ((level (plist-get comment :level))
         (file (plist-get comment :file))
         (old (eq (plist-get comment :side) 'old))
         (lines (and (memq level '(nil line range))
                     (concat ":" (if old "-" "")
                             (if (eq level 'range)
                                 (format "%s–%s" (plist-get comment :start-line)
                                         (plist-get comment :line))
                               (format "%s" (plist-get comment :line))))))
         (dir (and file (file-name-directory file))))
    (cons (and file (propertize (concat (file-name-nondirectory file) lines)
                                'face 'bold))
          (and dir (propertize (directory-file-name dir) 'face 'shadow)))))

(defun ygg-git-compare--comment-face (comment)
  (let ((priority (plist-get comment :priority))
        (type (plist-get comment :type)))
    (cond (priority (ygg-git-compare--priority-face priority))
          (type (ygg-git-compare--type-face type))
          (t 'font-lock-doc-face))))

(defun ygg-git-compare--comment-block (comment &optional placed)
  "COMMENT as lines to show, leading with its place when PLACED."
  (let* ((type (plist-get comment :type))
         (priority (plist-get comment :priority))
         (pending (ygg-git-compare--pending-p comment))
         (face (ygg-git-compare--comment-face comment))
         (bar (propertize "▎ " 'face face))
         (place (and placed (ygg-git-compare--place-chips comment)))
         (meta (delq nil
                     (list (car place)
                           (ygg-git-compare--verdict comment)
                           (and priority (propertize (format "[P%s]" priority) 'face face))
                           (and type (propertize (format "[%s]" type)
                                                 'face (ygg-git-compare--type-face type)))
                           (propertize (ygg-git-compare--audience comment) 'face 'shadow)
                           (and (not placed) (eq (plist-get comment :level) 'range)
                                (propertize (format "L%s–L%s" (plist-get comment :start-line)
                                                    (plist-get comment :line))
                                            'face 'shadow))
                           (when-let* ((author (plist-get comment :author)))
                             (propertize author 'face 'shadow))
                           (and ygg-git-compare--mark-drafts (not pending)
                                (propertize "draft" 'face '(warning italic)))
                           (and pending (propertize "pending" 'face 'shadow))
                           (and (plist-get comment :outdated) (plist-get comment :relocated)
                                (propertize (ygg-git-compare--outdated-label comment)
                                            'face 'warning))
                           (and (not (plist-get comment :relocated))
                                (ygg-git-compare--stale-p comment)
                                (propertize (format "stale · made on %s"
                                                    (plist-get comment :range))
                                            'face 'warning))
                           (and (not (plist-get comment :correctness))
                                (plist-get comment :confidence)
                                (propertize (format "%s" (plist-get comment :confidence))
                                            'face 'shadow)))))
         (title (plist-get comment :title))
         (head (string-join (append meta (and title (list (propertize title 'face 'bold)))
                                    (and (cdr place) (list (cdr place))))
                            " "))
         (body (split-string (or (plist-get comment :text) "") "\n")))
    (if (plist-get comment :remote)
        (ygg-git-compare--remote-block comment)
      (mapconcat #'identity
                 (append (unless (string-empty-p head) (list (concat bar head)))
                         (mapcar (lambda (line)
                                   (let ((shown (concat bar (if pending
                                                                (propertize line 'face '(shadow italic))
                                                              line))))
                                     (add-face-text-property 0 (length shown) 'ygg-comment t shown)
                                     shown))
                                 body))
                 "\n"))))

(defun ygg-git-compare--line-hit (file side line)
  "The diff line of FILE's section on SIDE at LINE, as ygg-git-compare--hunk-lines
gives it, or nil."
  (seq-some (lambda (hunk)
              (when-let* (((magit-section-match 'hunk hunk))
                          ((oref hunk from-range))
                          ((oref hunk to-range)))
                (seq-find (lambda (l) (ygg-git-compare--line-match l side line))
                          (ygg-git-compare--hunk-lines hunk))))
            (oref file children)))

(defun ygg-git-compare--line-match (entry side line)
  (if (and (eq side 'old) (nth 3 entry))
      (eql (nth 3 entry) line)
    (and (eq (nth 1 entry) side) (eql (nth 2 entry) line))))

(defcustom ygg-git-compare-excerpt-context 3
  "Diff lines shown around the lines of a comment away from its hunk."
  :type 'natnum
  :group 'ygg-git-compare)

(defvar ygg-git-compare--excerpt-cache nil
  "The diffs fetched while comments are drawn, by (DIR ARGS PATHS).")

(defun ygg-git-compare--diff-entries (lines)
  "The hunks of the diff LINES, each as the entries (TEXT SIDE LINE OLD-LINE)
that ygg-git-compare--hunk-lines gives with the line's text for its position."
  (let (hunks hunk old new)
    (dolist (text lines)
      (if-let* ((header (ygg-git-compare--hunk-header text)))
          (progn (when hunk (push (nreverse hunk) hunks))
                 (setq hunk nil old (nth 0 header) new (nth 2 header)))
        (when old
          (pcase (and (> (length text) 0) (aref text 0))
            (?- (push (list text 'old old) hunk) (cl-incf old))
            (?+ (push (list text 'new new) hunk) (cl-incf new))
            (?\s (push (list text 'new new old) hunk) (cl-incf old) (cl-incf new))))))
    (when hunk (push (nreverse hunk) hunks))
    (nreverse hunks)))

(defun ygg-git-compare--excerpt-hunks (args paths)
  (let* ((key (list default-directory args paths))
         (hit (gethash key ygg-git-compare--excerpt-cache 'miss)))
    (if (eq hit 'miss)
        (puthash key
                 (ygg-git-compare--diff-entries
                  (apply #'magit-git-lines "diff" "--no-color" "--no-ext-diff"
                         (format "-U%d" ygg-git-compare-excerpt-context)
                         (append args (cons "--" paths))))
                 ygg-git-compare--excerpt-cache)
      hit)))

(defun ygg-git-compare--excerpt-args (comment away)
  "What git diff takes for the range COMMENT's lines are read in: its own when
AWAY, else this compare's; nil when its commits are unknown."
  (if away
      (when-let* ((ends (ygg-git-compare--range-ends (plist-get comment :range)))
                  (old (ygg-git-compare--old-commit (plist-get comment :range))))
        (list old (cdr ends)))
    (ygg-git-compare--diff-args ygg-git-compare--plan)))

(defun ygg-git-compare--excerpt (comment away)
  "The diff lines COMMENT is on with their context, led by its bar, or nil."
  (when-let* ((args (ygg-git-compare--excerpt-args comment away))
              (new-path (plist-get comment :new-path))
              (side (plist-get comment :side))
              (line (plist-get comment :line))
              (hunks (ygg-git-compare--excerpt-hunks
                      args (delete-dups (list (or (plist-get comment :old-path) new-path)
                                              new-path))))
              (hunk (seq-find (lambda (h) (seq-some (lambda (l) (ygg-git-compare--line-match l side line)) h))
                              hunks)))
    (cl-flet ((index (side line)
                (cl-position-if (lambda (l) (ygg-git-compare--line-match l side line)) hunk)))
      (let* ((last (index side line))
             (first (or (and (eq (plist-get comment :level) 'range)
                             (plist-get comment :start-line)
                             (index (or (plist-get comment :start-side) side)
                                    (plist-get comment :start-line)))
                        last))
             (low (min first last))
             (high (max first last))
             (bar (propertize "▎ " 'face (ygg-git-compare--comment-face comment)))
             (from (max 0 (- low ygg-git-compare-excerpt-context)))
             (to (min (1- (length hunk)) (+ high ygg-git-compare-excerpt-context)))
             (i (1- from)))
        (mapconcat
         (lambda (l)
           (let* ((text (copy-sequence (car l)))
                  (face (pcase (aref text 0)
                          (?+ 'magit-diff-added)
                          (?- 'magit-diff-removed)
                          (_ 'magit-diff-context))))
             (cl-incf i)
             (add-face-text-property
              0 (length text) (if (<= low i high) (list 'ygg-git-compare-comment-target face) face)
              nil text)
             (concat bar text)))
         (cl-subseq hunk from (1+ to)) "\n")))))

;;; Comments written on an older head

(defvar ygg-git-compare--remap-cache (make-hash-table :test 'equal)
  "How a file's lines went between two commits, by (REPO OLD NEW PATH).")

(autoload 'ygg-git-compare-submit--written-head "ygg-git-compare-submit")

(defvar ygg-git-compare--b-head-memo nil
  "(LIST . HEAD) while LIST's comments are drawn, HEAD `unset' until asked for.")

(defun ygg-git-compare--read-b-head (list)
  (with-current-buffer list
    (when-let* (((not (eq ygg-git-compare--b (plist-get ygg-git-compare--plan :work))))
                (rev (plist-get ygg-git-compare--b :diff)))
      (magit-rev-parse "--verify" "-q" (concat rev "^{commit}")))))

(defun ygg-git-compare--b-head (list)
  "The commit LIST's side B is at, nil when it is a checkout; asked of git once
while LIST's comments are drawn."
  (let ((memo ygg-git-compare--b-head-memo))
    (if (eq (car memo) list)
        (progn (when (eq (cdr memo) 'unset)
                 (setcdr memo (ygg-git-compare--read-b-head list)))
               (cdr memo))
      (ygg-git-compare--read-b-head list))))

(defvar ygg-git-compare--unavailable-cache (make-hash-table :test 'equal)
  "The keys of `ygg-git-compare--remap-cache' whose commits were not here, until
the next compare is drawn again.")

(defun ygg-git-compare--forget-unavailable ()
  (clrhash ygg-git-compare--unavailable-cache))

(add-hook 'ygg-git-compare-redraw-hook #'ygg-git-compare--forget-unavailable)

(defun ygg-git-compare--diff-hunks (old new path)
  "The (OLD-START OLD-COUNT NEW-COUNT) of each hunk of PATH between OLD and NEW."
  (delq nil
        (mapcar (lambda (line)
                  (when-let* ((hunk (ygg-git-compare--hunk-header line)))
                    (list (nth 0 hunk) (nth 1 hunk) (nth 3 hunk))))
                (magit-git-lines "diff" "--unified=0" "--no-color" "--no-ext-diff"
                                 old new "--" path))))

(defun ygg-git-compare--remap (old new path)
  "How PATH's lines went from commit OLD to NEW: (ok . HUNKS), gone, or
unavailable."
  (let ((key (list (magit-toplevel) old new path)))
    (cond ((gethash key ygg-git-compare--unavailable-cache) 'unavailable)
          ((gethash key ygg-git-compare--remap-cache))
          (t (let ((map (cond ((not (magit-git-success "cat-file" "-e" (concat old "^{commit}")))
                               'unavailable)
                              ((not (magit-git-success "cat-file" "-e" (concat new ":" path)))
                               'gone)
                              ((not (magit-git-success "cat-file" "-e" (concat old ":" path)))
                               'unavailable)
                              (t (cons 'ok (ygg-git-compare--diff-hunks old new path))))))
               (if (eq map 'unavailable)
                   (puthash key t ygg-git-compare--unavailable-cache)
                 (puthash key map ygg-git-compare--remap-cache))
               map)))))

(defun ygg-git-compare--map-line (hunks line)
  "Where LINE goes through HUNKS, nil when a hunk changes it."
  (let ((offset 0))
    (catch 'done
      (pcase-dolist (`(,start ,old ,new) hunks)
        (cond ((if (zerop old) (<= line start) (< line start))
               (throw 'done (+ line offset)))
              ((and (> old 0) (< line (+ start old)))
               (throw 'done nil)))
        (cl-incf offset (- new old)))
      (+ line offset))))

(defun ygg-git-compare--range-ends (range)
  "The (BASE . HEAD) commits RANGE's last token names, nil when it is not two."
  (when (and range
             (string-match "\\([0-9a-f]\\{40\\}\\)\\.\\.\\.?\\([0-9a-f]\\{40\\}\\)\\'" range))
    (cons (match-string 1 range) (match-string 2 range))))

(defun ygg-git-compare--b-label (range)
  (when (and range (string-match "↔ B: \\(.*\\)  [^ ]+\\'" range))
    (match-string 1 range)))

(defun ygg-git-compare--old-commit (range)
  "The commit the old side of RANGE's last token is read from: where its sides
parted for A...B, A for A..B."
  (when-let* ((ends (ygg-git-compare--range-ends range)))
    (if (string-match-p "\\.\\.\\.[0-9a-f]\\{40\\}\\'" range)
        (let ((key (list 'merge-base (magit-toplevel) (car ends) (cdr ends))))
          (or (gethash key ygg-git-compare--remap-cache)
              (when-let* ((base (magit-git-string "merge-base" (car ends) (cdr ends))))
                (puthash key base ygg-git-compare--remap-cache))))
      (car ends))))

(defun ygg-git-compare--side-map (comment side was now)
  "How the lines of COMMENT's SIDE went from the commits of the range WAS to
those of the range NOW."
  (let* ((old (eq side 'old))
         (path (if old
                   (or (plist-get comment :old-path) (plist-get comment :new-path))
                 (plist-get comment :new-path)))
         (from (if old (ygg-git-compare--old-commit was) (cdr (ygg-git-compare--range-ends was))))
         (to (if old (ygg-git-compare--old-commit now) (cdr (ygg-git-compare--range-ends now)))))
    (cond ((not (and path from to)) 'unavailable)
          ((equal from to) '(ok))
          (t (ygg-git-compare--remap from to path)))))

(defun ygg-git-compare--relocate (comment head here)
  "COMMENT written on other commits than HERE's, moved to where its lines went,
or marked :outdated when they did not survive; nil when it is not one, or
was made on another B than HERE's."
  (when-let* (((memq (plist-get comment :level) '(line range)))
              (range (plist-get comment :range))
              (was (ygg-git-compare--range-ends range))
              (now (ygg-git-compare--range-ends here))
              ((equal head (cdr now)))
              ((equal (ygg-git-compare--b-label range) (ygg-git-compare--b-label here)))
              ((not (equal was now))))
    (let* ((side (plist-get comment :side))
           (start (plist-get comment :start-line))
           (start-side (or (plist-get comment :start-side) side))
           (moved (plist-put (copy-sequence comment) :relocated (cdr was)))
           (map (ygg-git-compare--side-map comment side range here))
           (start-map (cond ((not start) map)
                            ((eq start-side side) map)
                            (t (ygg-git-compare--side-map comment start-side range here)))))
      (cond
       ((or (eq map 'gone) (eq start-map 'gone))
        (plist-put (plist-put moved :outdated t) :gone t))
       ((or (eq map 'unavailable) (eq start-map 'unavailable))
        (plist-put moved :outdated t))
       (t
        (let ((line (ygg-git-compare--map-line (cdr map) (plist-get comment :line)))
              (first (and start (ygg-git-compare--map-line (cdr start-map) start))))
          (if (and line (or first (not start)))
              (plist-put (plist-put moved :line line) :start-line first)
            (plist-put moved :outdated t))))))))

(defun ygg-git-compare--outdated-label (comment)
  (format "outdated · written on %s" (substring (plist-get comment :relocated) 0 7)))

(defun ygg-git-compare--line-pos (file comment)
  "Where in FILE's section the line COMMENT is on ends, or nil."
  (when-let* ((hit (ygg-git-compare--line-hit file (plist-get comment :side)
                                              (plist-get comment :line))))
    (save-excursion (goto-char (car hit)) (line-end-position))))

(defface ygg-git-compare-comment-target '((t :inherit highlight))
  "The diff lines of the comment point is on."
  :group 'ygg-git-compare)

(defun ygg-git-compare--comment-span (file comment)
  "The start of the first and the end of the last diff line COMMENT is on in
FILE's section, as (BEG . END), or nil."
  (when-let* ((end (ygg-git-compare--line-hit file (plist-get comment :side)
                                              (plist-get comment :line)))
              (start (if (eq (plist-get comment :level) 'range)
                         (ygg-git-compare--line-hit file (plist-get comment :start-side)
                                                    (plist-get comment :start-line))
                       end)))
    (let ((beg (min (car start) (car end)))
          (last (max (car start) (car end))))
      (cons beg (save-excursion (goto-char last) (line-end-position))))))

(defun ygg-git-compare--target-overlays ()
  (seq-filter (lambda (ov) (overlay-get ov 'ygg-git-compare-target))
              (overlays-in (point-min) (point-max))))

(defun ygg-git-compare--clear-targets (buffer)
  (with-current-buffer buffer
    (mapc #'delete-overlay (ygg-git-compare--target-overlays))))

(defun ygg-git-compare--mark-comments (buffer comments)
  "Mark the diff lines COMMENTS are on in BUFFER; answer where the first starts."
  (with-current-buffer buffer
    (ygg-git-compare--clear-targets buffer)
    (let (first)
      (dolist (c comments first)
        (when-let* ((file (seq-find (lambda (s) (and (magit-section-match 'file s)
                                                     (equal (oref s value) (plist-get c :new-path))))
                                    (oref magit-root-section children)))
                    (span (ygg-git-compare--comment-span file c)))
          (let ((ov (make-overlay (car span) (cdr span))))
            (overlay-put ov 'face 'ygg-git-compare-comment-target)
            (overlay-put ov 'ygg-git-compare-target t)
            (overlay-put ov 'evaporate t))
          (setq first (or first (car span))))))))

(defun ygg-git-compare--diff-files ()
  (let (files)
    (magit-map-sections
     (lambda (s) (when (magit-section-match 'file s) (push (oref s value) files))))
    files))

(defun ygg-git-compare--comment-targets ()
  "The comments on this line that sit on a file this diff has."
  (when-let* ((comments (seq-remove #'ygg-git-compare--stale-p
                                    (ygg-git-compare--comments-at-point)))
              (files (ygg-git-compare--diff-files)))
    (seq-filter (lambda (c) (member (plist-get c :new-path) files)) comments)))

(defvar-local ygg-git-compare--aimed nil
  "The file and comments the right pane was last scrolled to.")

(defun ygg-git-compare--aim-comments (file comments)
  "Mark COMMENTS' lines in the right pane's FILE, scrolling there when they
are new; with none, clear the marks."
  (let ((buffer (get-buffer (ygg-git-compare--file-buffer-name)))
        (window ygg-git-compare--file-window)
        (key (cons file (mapcar (lambda (c) (plist-get c :id)) comments))))
    (when buffer (ygg-git-compare--clear-targets buffer))
    (if (null comments)
        (setq ygg-git-compare--aimed nil)
      (when-let* ((buffer)
                  (start (ygg-git-compare--mark-comments buffer comments)))
        (unless (equal key ygg-git-compare--aimed)
          (setq ygg-git-compare--aimed key)
          (when (eq (window-buffer window) buffer)
            (with-selected-window window
              (goto-char start)
              (ygg-git-compare--reveal)
              (recenter (round (/ (window-body-height) 3))))))))))

(defun ygg-git-compare--release-jump ()
  (let ((marks (ygg-git-compare--target-overlays)))
    (unless (seq-some (lambda (ov) (<= (overlay-start ov) (point) (overlay-end ov))) marks)
      (mapc #'delete-overlay marks)
      (remove-hook 'post-command-hook #'ygg-git-compare--release-jump t))))

(defun ygg-git-compare--jumped (comments)
  "Mark COMMENTS' lines here until point leaves them, and show their file now."
  (ygg-git-compare--mark-comments (current-buffer) comments)
  (add-hook 'post-command-hook #'ygg-git-compare--release-jump nil t)
  (ygg-git-compare--follow (current-buffer)))

(defun ygg-git-compare--shown-text (shown)
  "The blocks of SHOWN, newest first as (ID BLOCK), oldest first, those a
folded thread leaves empty left out."
  (mapconcat #'cadr (seq-remove (lambda (entry) (string-empty-p (cadr entry))) (reverse shown))
             "\n"))

(defun ygg-git-compare--draw-comments ()
  "Show the compare's comments under what each is on in this diff; those
it has no place for at the top of the diff of every file."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'ygg-git-compare-comments) (delete-overlay ov)))
  (when-let* (((derived-mode-p 'magit-diff-mode))
              (list (ignore-errors (ygg-git-compare--list)))
              (comments (append (ygg-git-compare--remote-comments list)
                                (reverse (buffer-local-value 'ygg-git-compare--comments list))))
              ((bound-and-true-p magit-root-section)))
    (when-let* ((spans (and (eq (current-buffer) list)
                            (ygg-git-compare--conversation-spans))))
      (pcase-dolist (`(,id ,beg ,end) spans)
        (overlay-put (make-overlay beg end) 'ygg-git-compare-comments (list id)))
      (setq comments (seq-remove (lambda (c) (assoc (plist-get c :id) spans)) comments)))
    (let ((ygg-git-compare--mark-drafts
           (seq-find (lambda (c) (and (plist-get c :remote) (not (plist-get c :notice))))
                     comments))
          (files (seq-filter (lambda (s) (magit-section-match 'file s))
                             (if (ygg-git-compare--unwashed-p)
                                 (seq-mapcat (lambda (s) (oref s children))
                                             (seq-filter (lambda (s) (eq (oref s type) 'diffstat))
                                                         (oref magit-root-section children)))
                               (oref magit-root-section children))))
          (head (and (seq-some (lambda (c) (not (plist-get c :remote))) comments)
                     (string-prefix-p "branch " (with-current-buffer list
                                                    (ygg-git-compare--compare-key)))
                     (ygg-git-compare--b-head list)))
          (here (with-current-buffer list (ygg-git-compare--range-label)))
          (default-directory (buffer-local-value 'default-directory list))
          (magit-git-global-arguments (with-current-buffer list (ygg-git-compare--global-args)))
          (ygg-git-compare--excerpt-cache (make-hash-table :test 'equal))
          (places nil)
          (stale nil)
          (gone nil)
          (top nil))
      (dolist (original comments)
        (let* ((level (plist-get original :level))
               (c (or (and (ygg-git-compare--stale-p original)
                           (ygg-git-compare--relocate original head here))
                      original))
               (outdated (plist-get c :outdated))
               (file (and (plist-get c :new-path) (not (plist-get c :gone))
                          (seq-find (lambda (s) (equal (oref s value) (plist-get c :new-path)))
                                    files)))
               (stale-p (and (not (plist-get c :relocated))
                             (ygg-git-compare--stale-p c)))
               (pos (and file (not stale-p) (not outdated) (memq level '(line range))
                         (ygg-git-compare--line-pos file c)))
               (place (cond (stale-p nil)
                            (pos pos)
                            (file (save-excursion (goto-char (oref file start))
                                                  (line-end-position)))))
               (block (ygg-git-compare--comment-block
                       c (not (or pos (memq level '(file review))))))
               (excerpt (and (eq (current-buffer) list) (not pos)
                             (memq level '(line range)) (plist-get c :new-path)
                             (not (string-empty-p block))
                             (ygg-git-compare--excerpt c (or stale-p outdated)))))
          (when excerpt (setq block (concat excerpt "\n" block)))
          (cond (stale-p
                 (when (eq (current-buffer) list)
                   (push (list (plist-get c :id) block) stale)))
                (place (push (list (plist-get c :id) block)
                             (alist-get place places nil nil #'eql)))
                ((eq (current-buffer) list)
                 (push (list (plist-get c :id) block)
                       (if outdated gone top))))))
      (pcase-dolist (`(,pos . ,shown) places)
        (let ((ov (make-overlay (save-excursion (goto-char pos) (line-beginning-position)) pos)))
          (overlay-put ov 'ygg-git-compare-comments (mapcar #'car shown))
          (overlay-put ov 'after-string
                       (concat "\n" (ygg-git-compare--shown-text shown)))))
      (when (or stale gone top)
        (let* ((first (seq-find (lambda (s)
                                  (not (memq (oref s type) (bound-and-true-p ygg-git-compare--pr-info-order))))
                                (oref magit-root-section children)))
               (begin (if first (marker-position (oref first start)) (point-min)))
               (ov (make-overlay begin
                                 (if (seq-some #'ygg-git-compare--remote-id-p
                                               (mapcar #'car top))
                                     (save-excursion (goto-char begin) (line-end-position))
                                   begin))))
          (overlay-put ov 'ygg-git-compare-comments (mapcar #'car (append stale gone top)))
          (overlay-put ov 'before-string
                       (concat (when stale
                                 (concat (propertize "Made on another range" 'face 'warning)
                                         "\n" (mapconcat #'cadr (reverse stale) "\n") "\n"))
                               (when gone
                                 (concat (propertize "Outdated comments" 'face 'warning)
                                         "\n" (mapconcat #'cadr (reverse gone) "\n") "\n"))
                               (when top
                                 (concat (ygg-git-compare--shown-text top) "\n")))))))))

(defvar ygg-git-compare-comments-changed-functions nil
  "Functions called with a compare's list buffer after its comments redraw.")

(defun ygg-git-compare--redraw-comments (list)
  "Show LIST's comments again wherever they are shown."
  (let ((ygg-git-compare--b-head-memo (cons list 'unset)))
    (dolist (buffer (buffer-list))
      (when (or (eq buffer list)
                (eq (buffer-local-value 'ygg-git-compare--list-buffer buffer) list))
        (with-current-buffer buffer
          (cond ((derived-mode-p 'ygg-git-compare-comments-summary-mode)
                 (ygg-git-compare--summary-insert))
                ((bound-and-true-p ygg-git-compare-mode)
                 (ygg-git-compare--draw-comments)))))))
  (run-hook-with-args 'ygg-git-compare-comments-changed-functions list))

;;; Acting on the comment at point

(defun ygg-git-compare--comments-at-point ()
  "The comments shown on this line, or the one this section of a summary is."
  (let ((ids (if (derived-mode-p 'ygg-git-compare-comments-summary-mode)
                 (ensure-list (magit-section-value-if 'ygg-git-compare-comment))
               (seq-mapcat (lambda (ov) (overlay-get ov 'ygg-git-compare-comments))
                           (overlays-in (line-beginning-position) (line-end-position))))))
    (and ids (seq-filter (lambda (c) (member (plist-get c :id) ids))
                         (ygg-git-compare-comments-list t)))))

(defun ygg-git-compare--first-line (comment)
  "The first line of COMMENT's text."
  (car (split-string (or (plist-get comment :text) "") "\r?\n")))

(defun ygg-git-compare--comment-tags (comment)
  "COMMENT's type, priority and author, as far as it has them."
  (delq nil (list (when-let* ((type (plist-get comment :type))) (format "[%s]" type))
                  (when-let* ((p (plist-get comment :priority))) (format "P%s" p))
                  (plist-get comment :author))))

(defun ygg-git-compare--comment-candidates (comments)
  "COMMENTS as (LABEL . COMMENT), each label its text's first line, grouped
by file, those pending by author, and noted with type, priority, author
and place."
  (let (rows)
    (dolist (c comments (nreverse rows))
      (let ((label (truncate-string-to-width
                    (ygg-git-compare--first-line c) 60 nil nil "…"))
            (group (if (ygg-git-compare--pending-p c)
                       (format "Pending · %s" (or (plist-get c :author) "agent"))
                     (if (memq (plist-get c :level) '(nil line range file))
                         (plist-get c :new-path)
                       "Review")))
            (note (string-join
                   (append (ygg-git-compare--comment-tags c) (list (ygg-git-compare--where c)))
                   " ")))
        (while (assoc label rows) (setq label (concat label "'")))
        (push (cons (ygg-git-compare--group label (or group "Review") note) c) rows)))))

(defun ygg-git-compare--comment-at-point (&optional pred)
  "The comment at point that PRED holds for, asked for among several."
  (let ((comments (seq-filter (or pred #'identity) (ygg-git-compare--comments-at-point))))
    (if (cdr comments)
        (let ((rows (ygg-git-compare--comment-candidates comments)))
          (cdr (assoc (completing-read "Comment: "
                                       (ygg-git-compare-table rows 'ygg-review-comment)
                                       nil t)
                      rows)))
      (or (car comments) (user-error "No comment here")))))

(defun ygg-git-compare--accepted (comment)
  "COMMENT checked: no longer pending, and made on this compare's range when
it was made on none."
  (let ((comment (plist-put (copy-sequence comment) :status nil)))
    (if (plist-get comment :range)
        comment
      (plist-put comment :range (with-current-buffer (ygg-git-compare--list)
                                  (ygg-git-compare--range-label))))))

(defun ygg-git-compare--delete-comment (comment)
  "Delete COMMENT, asking first unless it is pending a check."
  (when (or (ygg-git-compare--pending-p comment)
            (y-or-n-p (format "Delete the comment on %s? "
                              (ygg-git-compare--where comment))))
    (ygg-git-compare-comments-drop (list (plist-get comment :id)))))

(defun ygg-git-compare--copy-comment (comment)
  "Copy the text of COMMENT."
  (let ((text (or (plist-get comment :text) "")))
    (kill-new text)
    (message "Copied: %s" (truncate-string-to-width text 60 nil nil "…"))))

(defun ygg-git-compare--accept-comment (comment)
  "Accept COMMENT that an agent proposed."
  (ygg-git-compare--put (ygg-git-compare--accepted comment)))

(defun ygg-git-compare--flip-comment (comment)
  "Send COMMENT to the other of the pull request and the agent."
  (when (ygg-git-compare--remote-id-p (plist-get comment :id))
    (user-error "That comment is already on the pull request"))
  (ygg-git-compare--put (ygg-git-compare--flipped comment)))

(defun ygg-git-compare-comment-flip ()
  "Send the comment at point to the other of the pull request and the agent."
  (interactive)
  (ygg-git-compare--flip-comment (ygg-git-compare--comment-at-point)))

(defun ygg-git-compare-comment-delete ()
  "Delete the comment at point, asking first unless it is pending a check."
  (interactive)
  (ygg-git-compare--delete-comment (ygg-git-compare--comment-at-point)))

(defun ygg-git-compare-comment-copy ()
  "Copy the text of the comment at point."
  (interactive)
  (ygg-git-compare--copy-comment (ygg-git-compare--comment-at-point)))

(defun ygg-git-compare-comment-accept ()
  "Accept the comment at point that an agent proposed."
  (interactive)
  (ygg-git-compare--accept-comment
   (ygg-git-compare--comment-at-point #'ygg-git-compare--pending-p)))

(defun ygg-git-compare-comments-accept-all ()
  "Accept every comment pending a check."
  (interactive)
  (ygg-git-compare--change
   (lambda (comments)
     (mapcar (lambda (c) (if (ygg-git-compare--pending-p c) (ygg-git-compare--accepted c) c))
             comments))))

(defun ygg-git-compare-comments-dismiss-all ()
  "Dismiss every comment pending a check."
  (interactive)
  (let ((pending (seq-filter #'ygg-git-compare--pending-p (ygg-git-compare-comments-list t))))
    (unless pending (user-error "No comments pending a check"))
    (when (y-or-n-p (format "Dismiss %d pending comment%s? " (length pending)
                            (if (cdr pending) "s" "")))
      (ygg-git-compare-comments-drop (mapcar (lambda (c) (plist-get c :id)) pending)))))

(defun ygg-git-compare--step-comment (back pred none)
  "Go to the next comment overlay whose ids satisfy PRED, or the one before
with BACK, around at the ends; answer its place as (INDEX . TOTAL).  NONE is
the error when there is no such overlay."
  (let* ((starts (or (sort (delete-dups
                            (mapcar #'overlay-start
                                    (seq-filter (lambda (ov)
                                                  (when-let* ((ids (overlay-get ov 'ygg-git-compare-comments)))
                                                    (funcall pred ids)))
                                                (overlays-in (point-min) (point-max)))))
                           #'<)
                     (user-error "%s" none)))
         (here (line-beginning-position))
         (target (if back
                     (or (car (last (seq-filter (lambda (p) (< p here)) starts)))
                         (car (last starts)))
                   (or (seq-find (lambda (p) (> p here)) starts) (car starts)))))
    (goto-char target)
    (ygg-git-compare--reveal)
    (ygg-git-compare--jumped (ygg-git-compare--comments-at-point))
    (cons (1+ (seq-position starts target)) (length starts))))

(defun ygg-git-compare-comment-next (&optional back)
  "Go to the next comment shown here, or the one before with BACK; around
past the last."
  (interactive)
  (ygg-git-compare--step-comment back #'identity "No comments here"))

(defun ygg-git-compare-comment-previous ()
  "Go to the comment shown before point, around past the first."
  (interactive)
  (ygg-git-compare-comment-next t))

(defun ygg-git-compare--step-agent-comment (back)
  (let* ((agent (seq-filter (lambda (c) (plist-get c :author))
                            (ygg-git-compare-comments-list t)))
         (pending (seq-filter #'ygg-git-compare--pending-p agent))
         (ids (mapcar (lambda (c) (plist-get c :id)) (or pending agent)))
         (place (ygg-git-compare--step-comment
                 back (lambda (shown) (seq-intersection shown ids)) "No agent comments here")))
    (message "agent comment %d/%d%s" (car place) (cdr place) (if pending " (pending)" ""))))

(defun ygg-git-compare-agent-comment-next ()
  "Go to the next comment an agent wrote, those pending a check first;
around past the last."
  (interactive)
  (ygg-git-compare--step-agent-comment nil))

(defun ygg-git-compare-agent-comment-previous ()
  "Go to the agent comment shown before point, around past the first."
  (interactive)
  (ygg-git-compare--step-agent-comment t))

(defun ygg-git-compare--goto-comment (id)
  (goto-char (or (seq-some (lambda (ov)
                             (and (member id (overlay-get ov 'ygg-git-compare-comments))
                                  (overlay-start ov)))
                           (overlays-in (point-min) (point-max)))
                 (point-min)))
  (ygg-git-compare--reveal)
  (ygg-git-compare--jumped
   (seq-filter (lambda (c) (equal (plist-get c :id) id)) (ygg-git-compare-comments-list t))))

(defun ygg-git-compare--file-sections ()
  (seq-filter (lambda (s) (magit-section-match 'file s))
              (oref magit-root-section children)))

(defun ygg-git-compare--find-file-section (path)
  "The file section whose path is PATH, or ends in /PATH, or whose name is PATH."
  (let ((files (ygg-git-compare--file-sections)))
    (cl-flet ((named (fn) (seq-find (lambda (s)
                                      (seq-some fn (delq nil (list (oref s value)
                                                                   (oref s source)))))
                                    files)))
      (or (named (lambda (p) (equal p path)))
          (named (lambda (p) (string-suffix-p (concat "/" path) p)))
          (named (lambda (p) (equal (file-name-nondirectory p) path)))))))

(defun ygg-git-compare--file-lines (file)
  "Each diff line drawn in the file section FILE, as `--hunk-lines' lists them."
  (mapcan (lambda (hunk)
            (and (magit-section-match 'hunk hunk)
                 (oref hunk from-range)
                 (oref hunk to-range)
                 (ygg-git-compare--hunk-lines hunk)))
          (oref file children)))

(defun ygg-git-compare--entry-line (entry side)
  "The number of the diff line ENTRY on SIDE, new or old, nil when it is not there."
  (if (and (eq side 'old) (nth 3 entry))
      (nth 3 entry)
    (and (eq (nth 1 entry) side) (nth 2 entry))))

(defun ygg-git-compare--line-position (path side line)
  "The start of the diff line of PATH on SIDE, new or old, numbered LINE, or nil
when no hunk shows it."
  (when-let* ((file (ygg-git-compare--find-file-section path)))
    (car (seq-find (lambda (entry) (eql (ygg-git-compare--entry-line entry side) line))
                   (ygg-git-compare--file-lines file)))))

(defun ygg-git-compare--section-up (type)
  "The section at point or the nearest one above it of TYPE."
  (let ((section (magit-current-section)))
    (while (and section (not (magit-section-match type section)))
      (setq section (oref section parent)))
    section))

(defun ygg-git-compare--section-path-at-point ()
  (when-let* ((file (ygg-git-compare--section-up 'file)))
    (oref file value)))

(defun ygg-git-compare--parse-place (input)
  "INPUT, \"PATH:LINE\", \"PATH LINE\" or \"LINE\", as (PATH SIDE LINE), PATH nil
without one and SIDE old for a LINE with a leading -."
  (let ((input (string-trim input)))
    (unless (string-match "\\`\\(?:\\(.+?\\)[: ]+\\)?\\(-\\)?\\([0-9]+\\)\\'" input)
      (user-error "Not PATH:LINE: %s" input))
    (list (match-string 1 input)
          (if (match-string 2 input) 'old 'new)
          (string-to-number (match-string 3 input)))))

(defun ygg-git-compare-goto-line (place)
  "Go to PLACE, \"PATH:LINE\" or \"LINE\" of the file at point, in the diff; a LINE
with a leading - counts the old side."
  (interactive
   (let ((input (completing-read "Go to path:line: "
                                 (mapcar (lambda (s) (concat (oref s value) ":"))
                                         (ygg-git-compare--file-sections)))))
     (list (if (ignore-errors (ygg-git-compare--parse-place input))
               input
             (let ((path (string-trim-right input "[: ]+")))
               (concat (unless (string-empty-p path) (concat path ":"))
                       (read-string (if (string-empty-p path) "Line: "
                                      (format "Line in %s: " path)))))))))
  (pcase-let* ((`(,path ,side ,line) (ygg-git-compare--parse-place place))
               (path (or path (ygg-git-compare--section-path-at-point)
                         (user-error "No file at point"))))
    (unless (with-current-buffer (ygg-git-compare--list)
              (ygg-git-compare--find-file-section path))
      (user-error "%s is not in the diff" path))
    (let ((marker (or (ygg-git-compare-show-line path side line)
                      (user-error "%s of %s is outside the diff"
                                  (if (eq side 'old) (- line) line) path))))
      (unless (eq (marker-buffer marker) (current-buffer))
        (pop-to-buffer (marker-buffer marker)))
      (goto-char marker)
      (ygg-git-compare--reveal))))

;;; Summary

(defvar-local ygg-git-compare--summary-max-priority nil
  "The least urgent priority the summary lists, or nil for every comment.")

(defvar-keymap ygg-git-compare-comments-summary-mode-map
  "RET" #'ygg-git-compare-comment-goto
  "C" #'ygg-git-compare-comment
  "d" (cons "delete" ygg-git-compare-delete-map)
  "a" #'ygg-git-compare-comment-accept
  "D" #'ygg-git-compare-comment-flip
  "Y" #'ygg-git-compare-comment-copy
  "f" #'ygg-git-compare-comments-filter
  "A" #'ygg-git-compare-comments-accept-all)

(define-derived-mode ygg-git-compare-comments-summary-mode magit-section-mode
  "Review comments"
  "The comments of a compare, by file, those pending a check first.")

(defun ygg-git-compare--by-priority (comments)
  (seq-sort-by (lambda (c) (or (plist-get c :priority) 4)) #'< comments))

(defun ygg-git-compare--summary-insert-files (comments)
  (pcase-dolist (`(,file . ,in-file)
                 (seq-group-by (lambda (c) (and (not (eq (plist-get c :level) 'review))
                                                (plist-get c :new-path)))
                               comments))
    (magit-insert-section (ygg-git-compare-comments-file file)
      (magit-insert-heading
        (propertize (or file "Review") 'font-lock-face 'magit-diff-file-heading)
        (propertize (format " (%d)" (length in-file)) 'font-lock-face 'shadow))
      (dolist (c (ygg-git-compare--by-priority in-file))
        (magit-insert-section (ygg-git-compare-comment (plist-get c :id))
          (insert (ygg-git-compare--comment-block
                   c (not (memq (plist-get c :level) '(file review))))
                  "\n")))
      (insert "\n"))))

(defun ygg-git-compare--summary-insert ()
  (let* ((inhibit-read-only t)
         (line (line-number-at-pos))
         (max ygg-git-compare--summary-max-priority)
         (comments (seq-filter (lambda (c) (or (not max)
                                               (and (plist-get c :priority)
                                                    (<= (plist-get c :priority) max))))
                               (ygg-git-compare-comments-list t)))
         (stale (seq-filter #'ygg-git-compare--stale-p comments))
         (current (seq-difference comments stale #'eq))
         (pending (seq-filter #'ygg-git-compare--pending-p current)))
    (erase-buffer)
    (magit-insert-section (ygg-git-compare-comments-summary)
      (magit-insert-heading
        (format "%d review comment%s%s" (length comments) (if (= (length comments) 1) "" "s")
                (if max (format ", P0–P%d" max) "")))
      (when pending
        (magit-insert-section (ygg-git-compare-comments-pending)
          (magit-insert-heading
            (propertize (format "Pending your check (%d)" (length pending))
                        'font-lock-face 'warning))
          (ygg-git-compare--summary-insert-files pending)))
      (when stale
        (magit-insert-section (ygg-git-compare-comments-stale)
          (magit-insert-heading
            (propertize (format "Made on another range (%d)" (length stale))
                        'font-lock-face 'warning))
          (ygg-git-compare--summary-insert-files stale)))
      (ygg-git-compare--summary-insert-files (seq-remove #'ygg-git-compare--pending-p current)))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ygg-git-compare-comments-summary ()
  "List the comments of this compare by file, those pending a check first."
  (interactive)
  (let ((list (ygg-git-compare--list)))
    (with-current-buffer (get-buffer-create (format "*review comments: %s*" (buffer-name list)))
      (unless (derived-mode-p 'ygg-git-compare-comments-summary-mode)
        (ygg-git-compare-comments-summary-mode))
      (setq ygg-git-compare--list-buffer list)
      (ygg-git-compare--summary-insert)
      (pop-to-buffer (current-buffer))
      (current-buffer))))

(defun ygg-git-compare--priority-candidates ()
  "The summary's filters as (LABEL . MAX), each noted with how many comments
it lists."
  (let ((comments (ygg-git-compare-comments-list t)))
    (mapcar (lambda (max)
              (let ((n (seq-count (lambda (c) (or (not max)
                                                  (and (plist-get c :priority)
                                                       (<= (plist-get c :priority) max))))
                                  comments)))
                (cons (ygg-git-compare--group (if max (format "P%d" max) "all") "Priority"
                                              (format "%d comment%s" n (if (= n 1) "" "s")))
                      max)))
            '(nil 0 1 2 3))))

(defun ygg-git-compare-comments-filter (max)
  "List only comments of priority MAX or more urgent; nil lists them all."
  (interactive (let ((rows (ygg-git-compare--priority-candidates)))
                 (list (cdr (assoc (completing-read
                                    "Up to priority: "
                                    (ygg-git-compare-table rows 'ygg-review-priority)
                                    nil t)
                                   rows)))))
  (setq ygg-git-compare--summary-max-priority max)
  (ygg-git-compare--summary-insert))

(defun ygg-git-compare-comment-goto ()
  "Show the comment at point in the diff of every file."
  (interactive)
  (let ((id (plist-get (ygg-git-compare--comment-at-point) :id)))
    (pop-to-buffer (ygg-git-compare--list))
    (ygg-git-compare--goto-comment id)))

;;; In the quickfix

(declare-function ygg-qf-define-kind "layer-quickfix" (kind &rest spec))
(declare-function ygg-qf-show-kind "layer-quickfix" (kind &rest args))
(declare-function ygg-qf-kind-refresh "layer-quickfix" (buf token))
(declare-function ygg-qf-kind-target-id "layer-quickfix" (&optional target))

(defun ygg-git-compare-comments-qf--all (list)
  "The comments of LIST's compare as kept, then those from the forge."
  (unless (buffer-live-p list) (user-error "That comment is gone"))
  (append (with-current-buffer list (ygg-git-compare-comments-list t))
          (seq-filter (lambda (c) (ygg-git-compare--remote-id-p (plist-get c :id)))
                      (ignore-errors (ygg-git-compare--remote-shown list)))))

(defun ygg-git-compare-comments-qf--find (id)
  (or (seq-find (lambda (c) (equal (plist-get c :id) (cdr id)))
                (ygg-git-compare-comments-qf--all (car id)))
      (user-error "That comment is gone")))

(defun ygg-git-compare-comments-qf--rows (list)
  (mapcar (lambda (c)
            (list (cons list (plist-get c :id))
                  (format "%s  %s" (ygg-git-compare--where c)
                          (ygg-git-compare--first-line c))
                  (string-join
                   (cons (if (ygg-git-compare--remote-id-p (plist-get c :id))
                             "forge"
                           (ygg-git-compare--audience c))
                         (ygg-git-compare--comment-tags c))
                   " ")))
          (ygg-git-compare-comments-qf--all list)))

(defun ygg-git-compare-comments-qf-goto (id)
  "Show the comment ID, a row id, in the diff of every file."
  (interactive (list (ygg-qf-kind-target-id)))
  (let ((comment (ygg-git-compare-comments-qf--find id)))
    (pop-to-buffer (car id))
    (if (plist-get comment :remote)
        (ygg-git-compare--goto-remote comment)
      (ygg-git-compare--goto-comment (cdr id)))))

(defun ygg-git-compare-comments-qf-copy (id)
  "Copy the text of the comment ID, a row id."
  (interactive (list (ygg-qf-kind-target-id)))
  (ygg-git-compare--copy-comment (ygg-git-compare-comments-qf--find id)))

(defun ygg-git-compare-comments-qf-accept (id)
  "Accept the comment ID, a row id, that an agent proposed."
  (interactive (list (ygg-qf-kind-target-id)))
  (let ((comment (ygg-git-compare-comments-qf--find id)))
    (unless (ygg-git-compare--pending-p comment) (user-error "Not pending a check"))
    (with-current-buffer (car id)
      (ygg-git-compare--accept-comment comment))))

(defun ygg-git-compare-comments-qf-flip (id)
  "Send the comment ID, a row id, to the other of the pull request and the agent."
  (interactive (list (ygg-qf-kind-target-id)))
  (let ((comment (ygg-git-compare-comments-qf--find id)))
    (with-current-buffer (car id)
      (ygg-git-compare--flip-comment comment))))

(defun ygg-git-compare-comments-qf-open (id)
  "Open the forge's comment ID, a row id, in the browser."
  (interactive (list (ygg-qf-kind-target-id)))
  (browse-url (or (plist-get (ygg-git-compare-comments-qf--find id) :url)
                  (user-error "This comment has no page"))))

(defun ygg-git-compare-comments-qf-drop (id)
  "Delete the comment ID, a row id, asking first unless it is pending a check."
  (interactive (list (ygg-qf-kind-target-id)))
  (let ((comment (ygg-git-compare-comments-qf--find id)))
    (when (ygg-git-compare--remote-id-p (plist-get comment :id))
      (user-error "That comment is the forge's"))
    (with-current-buffer (car id)
      (ygg-git-compare--delete-comment comment))))

(defvar ygg-git-compare-comments-qf-map
  (let ((m (make-sparse-keymap)))
    (define-key m "v" #'ygg-git-compare-comments-qf-goto)
    (define-key m "y" #'ygg-git-compare-comments-qf-copy)
    (define-key m "a" #'ygg-git-compare-comments-qf-accept)
    (define-key m "o" #'ygg-git-compare-comments-qf-open)
    (define-key m "d" #'ygg-git-compare-comments-qf-drop)
    (define-key m "D" #'ygg-git-compare-comments-qf-flip)
    m)
  "What embark offers on a comment row of the quickfix.")

(defun ygg-git-compare-comments-qf--arm (buf token list)
  (letrec ((follow (lambda (changed)
                     (when (eq changed list)
                       (ignore-errors
                         (unless (ygg-qf-kind-refresh buf token)
                           (funcall disarm))))))
           (leave (lambda () (run-at-time 0 nil follow list)))
           (disarm (lambda ()
                     (remove-hook 'ygg-git-compare-comments-changed-functions follow)
                     (when (buffer-live-p list)
                       (with-current-buffer list
                         (remove-hook 'kill-buffer-hook leave t))))))
    (add-hook 'ygg-git-compare-comments-changed-functions follow)
    (with-current-buffer list (add-hook 'kill-buffer-hook leave nil t))
    disarm))

(with-eval-after-load 'layer-quickfix
  (ygg-qf-define-kind 'compare-comments
                      :collect #'ygg-git-compare-comments-qf--rows
                      :action #'ygg-git-compare-comments-qf-goto
                      :map 'ygg-git-compare-comments-qf-map
                      :drop #'ygg-git-compare-comments-qf-drop
                      :live-p #'buffer-live-p
                      :arm #'ygg-git-compare-comments-qf--arm))

(defun ygg-git-compare-comments-qf ()
  "List every comment of this compare in the quickfix, those kept first and
then the forge's."
  (interactive)
  (require 'layer-quickfix)
  (ygg-qf-show-kind 'compare-comments (ygg-git-compare--list)))

;;; From an agent

(defun ygg-git-compare--received (comment author)
  "COMMENT as an agent sent it, keys and symbols as strings or not, made a
comment from AUTHOR pending a check."
  (let* ((c (cl-loop for (key value) on comment by #'cddr
                     for name = (string-replace "_" "-" (string-remove-prefix
                                                         ":" (format "%s" key)))
                     append (list (intern (concat ":" name))
                                  (if (and value
                                           (member name '("side" "start-side" "type"
                                                          "level")))
                                      (let ((text (format "%s" value)))
                                        (and (not (string-empty-p text))
                                             (intern (if (equal name "type")
                                                         (downcase text)
                                                       text))))
                                    value))))
         (file (plist-get c :file))
         (line (plist-get c :line))
         (start (plist-get c :start-line)))
    (list :id (or (plist-get c :id) (ygg-git-compare--new-id))
          :level (or (plist-get c :level)
                     (cond (start 'range) (line 'line) (file 'file) (t 'review)))
          :type (plist-get c :type)
          :text (or (plist-get c :text) "")
          :title (plist-get c :title)
          :priority (plist-get c :priority)
          :confidence (plist-get c :confidence)
          :correctness (plist-get c :correctness)
          :file file
          :old-path (or (plist-get c :old-path) file)
          :new-path (or (plist-get c :new-path) file)
          :side (and line (or (plist-get c :side) 'new))
          :line line
          :old-line (plist-get c :old-line)
          :start-side (and start (or (plist-get c :start-side) (plist-get c :side) 'new))
          :start-line start
          :start-old-line (plist-get c :start-old-line)
          :quote (plist-get c :quote)
          :range (plist-get c :range)
          :created (or (plist-get c :created) (float-time))
          :author author
          :status 'pending)))

;;;###autoload
(defun ygg-git-compare-comments-receive (dir branch comments author)
  "Keep COMMENTS, AUTHOR's review of BRANCH in DIR's repository, pending a
check in the compare of BRANCH.  Answer (COUNT . KEY)."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (key (ygg-git-compare--branch-review-key branch))
         (file (ygg-git-compare--store-file))
         (new (mapcar (lambda (c) (ygg-git-compare--received c author))
                      (append comments nil))))
    (ygg-git-compare--store-update file key (lambda (old) (append old new)))
    (dolist (buffer (buffer-list))
      (when (equal (buffer-local-value 'ygg-git-compare--store buffer) (cons file key))
        (with-current-buffer buffer
          (ygg-git-compare--reload)
          (ygg-git-compare--redraw-comments buffer))))
    (message "%d review comment%s from %s on %s — SPC g r to check"
             (length new) (if (= (length new) 1) "" "s") author branch)
    (cons (length new) key)))

;;; Dispatch

(defun ygg-git-compare-comments--review-description ()
  (format "agent review (+ %s)"
          (mapconcat #'symbol-name ygg-git-compare-agent-types ", ")))

(transient-define-prefix ygg-git-compare-dispatch ()
  "Comment on the compare, check what agents proposed, and send it."
  [["Comment"
    ("c" "on the review" ygg-git-compare-comment-review)
    ("l" "on the line or lines" ygg-git-compare-comment)
    ("h" "on the hunk" ygg-git-compare-comment-hunk)
    ("f" "on the file" ygg-git-compare-comment-file)
    ("L""list them" ygg-git-compare-comments-summary)
    ("Q" "list in quickfix" ygg-git-compare-comments-qf)
    ("D" "flip between PR and agent" ygg-git-compare-comment-flip)
    ("A" "accept all pending" ygg-git-compare-comments-accept-all)
    ("X" "dismiss all pending" ygg-git-compare-comments-dismiss-all)]
   ["Navigate"
    ("m" "next comment" ygg-git-compare-comment-next)
    ("M" "previous comment" ygg-git-compare-comment-previous)
    (":" "go to file:line" ygg-git-compare-goto-line)

    ("] a" "next agent comment" ygg-git-compare-agent-comment-next)
    ("[ a" "previous agent comment" ygg-git-compare-agent-comment-previous)
    ("]" "next unreviewed" ygg-git-compare-next-unreviewed)
    ("[" "previous unreviewed" ygg-git-compare-previous-unreviewed)]
   ["Review marks"
    ("r" "file reviewed" ygg-git-compare-mark-file-reviewed)
    ("R" "hunk reviewed" ygg-git-compare-mark-hunk-reviewed)
    ("u" "only unreviewed" ygg-git-compare-toggle-unreviewed)]
   ["Send"
    ("y" "copy as markdown" ygg-git-compare-export-markdown)
    ("&" "submit to PR" ygg-git-compare-submit)
    ("@" ygg-git-compare-comments--review-description ygg-git-compare-review)]
   ["View"
    ("H" "hide or show resolved threads" ygg-git-compare-threads-toggle-resolved)
    ("I" "interdiff since last review" ygg-git-compare-interdiff)
    ("x" "explain the change" ygg-git-compare-explain)
    ("t" "guided review tour" ygg-git-compare-tour)
    ("W" "new tour (replaces the kept one)" ygg-git-compare-tour-refresh)
    ("w" "tour agent's trace" ygg-git-compare-tour-trace)
    ("b" "switch base" ygg-git-compare-switch-base)
    ("~" "swap sides" ygg-git-compare-swap)
    ("." "A...B or A..B" ygg-git-compare-toggle-dots)
    ("#" "commits of each side" ygg-git-compare-log)
    ("+" "more context" ygg-git-compare-expand-context)
    ("-" "less context" ygg-git-compare-shrink-context)]])

(provide 'ygg-git-compare-comments)
;;; ygg-git-compare-comments.el ends here
