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

(defvar-keymap ygg-git-compare-box-mode-map
  "C-c C-t" #'ygg-git-compare-draft-cycle-type)

(define-minor-mode ygg-git-compare-box-mode
  "A comment box that writes a review comment, typed with TAB or \\[ygg-git-compare-draft-cycle-type]."
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

(defun ygg-git-compare--draft-set-type (name status)
  "Give the draft the type NAME, completed at its start, in place of the word."
  (when (eq status 'finished)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at (concat (regexp-quote name) "[[:blank:]]*"))
        (delete-region (match-beginning 0) (match-end 0))))
    (setq ygg-git-compare--draft (plist-put ygg-git-compare--draft :type (intern name)))
    (setq aob-compose--tags (ygg-git-compare--box-tags))
    (force-mode-line-update)))

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
                 (ygg-git-compare--keep ygg-git-compare--draft
                                        ygg-git-compare--draft-list text
                                        ygg-git-compare--draft-existing))
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
  (setq ygg-git-compare--draft
        (plist-put ygg-git-compare--draft :type
                   (ygg-git-compare--next-type (plist-get ygg-git-compare--draft :type)
                                               (or step 1))))
  (setq aob-compose--tags (ygg-git-compare--box-tags))
  (force-mode-line-update))

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

(defun ygg-git-compare--comment-block (comment &optional where)
  "COMMENT as lines to show, naming WHERE it is on when given."
  (let* ((type (plist-get comment :type))
         (priority (plist-get comment :priority))
         (pending (ygg-git-compare--pending-p comment))
         (face (cond (priority (ygg-git-compare--priority-face priority))
                     (type (ygg-git-compare--type-face type))
                     (t 'font-lock-doc-face)))
         (bar (propertize "▎ " 'face face))
         (meta (delq nil
                     (list (ygg-git-compare--verdict comment)
                           (and priority (propertize (format "[P%s]" priority) 'face face))
                           (and type (propertize (format "[%s]" type)
                                                 'face (ygg-git-compare--type-face type)))
                           (and (eq (plist-get comment :level) 'range)
                                (propertize (format "L%s–L%s" (plist-get comment :start-line)
                                                    (plist-get comment :line))
                                            'face 'shadow))
                           (and where (propertize where 'face 'shadow))
                           (when-let* ((author (plist-get comment :author)))
                             (propertize author 'face 'shadow))
                           (and ygg-git-compare--mark-drafts (not pending)
                                (propertize "draft" 'face '(warning italic)))
                           (and pending (propertize "pending" 'face 'shadow))
                           (and (ygg-git-compare--stale-p comment)
                                (propertize (format "stale · made on %s"
                                                    (plist-get comment :range))
                                            'face 'warning))
                           (and (not (plist-get comment :correctness))
                                (plist-get comment :confidence)
                                (propertize (format "%s" (plist-get comment :confidence))
                                            'face 'shadow)))))
         (title (plist-get comment :title))
         (head (string-join (append meta (and title (list (propertize title 'face 'bold))))
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

(defun ygg-git-compare--line-pos (file comment)
  "Where in FILE's section the line COMMENT is on ends, or nil."
  (let ((side (plist-get comment :side))
        (line (plist-get comment :line)))
    (seq-some (lambda (hunk)
                (when-let* (((magit-section-match 'hunk hunk))
                            ((oref hunk from-range))
                            ((oref hunk to-range))
                            (hit (seq-find (lambda (l) (if (and (eq side 'old) (nth 3 l))
                                                           (eql (nth 3 l) line)
                                                         (and (eq (nth 1 l) side)
                                                              (eql (nth 2 l) line))))
                                           (ygg-git-compare--hunk-lines hunk))))
                  (save-excursion (goto-char (car hit)) (line-end-position))))
              (oref file children))))

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
                             (oref magit-root-section children)))
          (places nil)
          (stale nil)
          (top nil))
      (dolist (c comments)
        (let* ((level (plist-get c :level))
               (file (and (plist-get c :new-path)
                          (seq-find (lambda (s) (equal (oref s value) (plist-get c :new-path)))
                                    files)))
               (stale-p (ygg-git-compare--stale-p c))
               (pos (and file (not stale-p) (memq level '(line range))
                         (ygg-git-compare--line-pos file c)))
               (place (cond (stale-p nil)
                            (pos pos)
                            (file (save-excursion (goto-char (oref file start))
                                                  (line-end-position)))))
               (block (ygg-git-compare--comment-block
                       c (unless (or pos (memq level '(file review))) (ygg-git-compare--where c)))))
          (cond (stale-p
                 (when (eq (current-buffer) list)
                   (push (list (plist-get c :id) block) stale)))
                (place (push (list (plist-get c :id) block)
                             (alist-get place places nil nil #'eql)))
                ((eq (current-buffer) list)
                 (push (list (plist-get c :id) block) top)))))
      (pcase-dolist (`(,pos . ,shown) places)
        (let ((ov (make-overlay (save-excursion (goto-char pos) (line-beginning-position)) pos)))
          (overlay-put ov 'ygg-git-compare-comments (mapcar #'car shown))
          (overlay-put ov 'after-string
                       (concat "\n" (ygg-git-compare--shown-text shown)))))
      (when (or stale top)
        (let* ((first (seq-find (lambda (s)
                                  (not (memq (oref s type)
                                             '(ygg-git-compare-conversation ygg-git-compare-checks))))
                                (oref magit-root-section children)))
               (begin (if first (marker-position (oref first start)) (point-min)))
               (ov (make-overlay begin
                                 (if (seq-some #'ygg-git-compare--remote-id-p
                                               (mapcar #'car top))
                                     (save-excursion (goto-char begin) (line-end-position))
                                   begin))))
          (overlay-put ov 'ygg-git-compare-comments (mapcar #'car (append stale top)))
          (overlay-put ov 'before-string
                       (concat (when stale
                                 (concat (propertize "Made on another range" 'face 'warning)
                                         "\n" (mapconcat #'cadr (reverse stale) "\n") "\n"))
                               (when top
                                 (concat (ygg-git-compare--shown-text top) "\n")))))))))

(defun ygg-git-compare--redraw-comments (list)
  "Show LIST's comments again wherever they are shown."
  (dolist (buffer (buffer-list))
    (when (or (eq buffer list)
              (eq (buffer-local-value 'ygg-git-compare--list-buffer buffer) list))
      (with-current-buffer buffer
        (cond ((derived-mode-p 'ygg-git-compare-comments-summary-mode)
               (ygg-git-compare--summary-insert))
              ((bound-and-true-p ygg-git-compare-mode)
               (ygg-git-compare--draw-comments)))))))

;;; Acting on the comment at point

(defun ygg-git-compare--comments-at-point ()
  "The comments shown on this line, or the one this section of a summary is."
  (let ((ids (if (derived-mode-p 'ygg-git-compare-comments-summary-mode)
                 (ensure-list (magit-section-value-if 'ygg-git-compare-comment))
               (seq-mapcat (lambda (ov) (overlay-get ov 'ygg-git-compare-comments))
                           (overlays-in (line-beginning-position) (line-end-position))))))
    (and ids (seq-filter (lambda (c) (member (plist-get c :id) ids))
                         (ygg-git-compare-comments-list t)))))

(defun ygg-git-compare--comment-candidates (comments)
  "COMMENTS as (LABEL . COMMENT), each label its text's first line, grouped
by file, those pending by author, and noted with type, priority, author
and place."
  (let (rows)
    (dolist (c comments (nreverse rows))
      (let ((label (truncate-string-to-width
                    (car (split-string (or (plist-get c :text) "") "\n")) 60 nil nil "…"))
            (group (if (ygg-git-compare--pending-p c)
                       (format "Pending · %s" (or (plist-get c :author) "agent"))
                     (if (memq (plist-get c :level) '(nil line range file))
                         (plist-get c :new-path)
                       "Review")))
            (note (string-join
                   (delq nil (list (when-let* ((type (plist-get c :type))) (format "[%s]" type))
                                   (when-let* ((p (plist-get c :priority))) (format "P%s" p))
                                   (plist-get c :author)
                                   (ygg-git-compare--where c)))
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

(defun ygg-git-compare-comment-delete ()
  "Delete the comment at point, asking first unless it is pending a check."
  (interactive)
  (let ((comment (ygg-git-compare--comment-at-point)))
    (when (or (ygg-git-compare--pending-p comment)
              (y-or-n-p (format "Delete the comment on %s? "
                                (ygg-git-compare--where comment))))
      (ygg-git-compare-comments-drop (list (plist-get comment :id))))))

(defun ygg-git-compare-comment-copy ()
  "Copy the text of the comment at point."
  (interactive)
  (let ((text (plist-get (ygg-git-compare--comment-at-point) :text)))
    (kill-new text)
    (message "Copied: %s" (truncate-string-to-width text 60 nil nil "…"))))

(defun ygg-git-compare-comment-accept ()
  "Accept the comment at point that an agent proposed."
  (interactive)
  (ygg-git-compare--put (ygg-git-compare--accepted
                         (ygg-git-compare--comment-at-point #'ygg-git-compare--pending-p))))

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

(defun ygg-git-compare-comment-next (&optional back)
  "Go to the next comment shown here, or the one before with BACK; around
past the last."
  (interactive)
  (let* ((starts (or (sort (delete-dups
                            (mapcar #'overlay-start
                                    (seq-filter (lambda (ov)
                                                  (overlay-get ov 'ygg-git-compare-comments))
                                                (overlays-in (point-min) (point-max)))))
                           #'<)
                     (user-error "No comments here")))
         (here (line-beginning-position)))
    (goto-char (if back
                   (or (car (last (seq-filter (lambda (p) (< p here)) starts)))
                       (car (last starts)))
                 (or (seq-find (lambda (p) (> p here)) starts) (car starts))))
    (ygg-git-compare--reveal)))

(defun ygg-git-compare-comment-previous ()
  "Go to the comment shown before point, around past the first."
  (interactive)
  (ygg-git-compare-comment-next t))

(defun ygg-git-compare--goto-comment (id)
  (goto-char (or (seq-some (lambda (ov)
                             (and (member id (overlay-get ov 'ygg-git-compare-comments))
                                  (overlay-start ov)))
                           (overlays-in (point-min) (point-max)))
                 (point-min)))
  (ygg-git-compare--reveal))

;;; Summary

(defvar-local ygg-git-compare--summary-max-priority nil
  "The least urgent priority the summary lists, or nil for every comment.")

(defvar-keymap ygg-git-compare-comments-summary-mode-map
  "RET" #'ygg-git-compare-comment-goto
  "C" #'ygg-git-compare-comment
  "d" (cons "delete" ygg-git-compare-delete-map)
  "a" #'ygg-git-compare-comment-accept
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
                   c (unless (memq (plist-get c :level) '(file review))
                       (ygg-git-compare--where c)))
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
    ("A" "accept all pending" ygg-git-compare-comments-accept-all)
    ("X" "dismiss all pending" ygg-git-compare-comments-dismiss-all)]
   ["Navigate"
    ("m" "next comment" ygg-git-compare-comment-next)
    ("M" "previous comment" ygg-git-compare-comment-previous)
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
    ("#" "commits of each side" ygg-git-compare-log)]])

(provide 'ygg-git-compare-comments)
;;; ygg-git-compare-comments.el ends here
