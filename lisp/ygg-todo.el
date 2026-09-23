;;; ygg-todo.el --- lists a person and an agent both keep -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(defvar ygg-todo-changed-functions nil
  "Abnormal hook run with (FILE WHAT ITEM) after successful writes.
WHAT is one of add, done, undone, rewrite, remove, create.")

(defvar ygg-todo-by nil
  "Callers bind this to user or agent; hooks read it.")

(defvar ygg-todo--cache (make-hash-table :test 'equal)
  "Cache of parsed todo lists, keyed by file path.")

(defun ygg-todo--cache-key (file)
  "Return a cache key for FILE that includes its mtime and size."
  (let ((attrs (file-attributes file)))
    (when attrs
      (cons (file-attribute-modification-time attrs)
            (file-attribute-size attrs)))))

(defun ygg-todo--parse-items (content)
  "Parse CONTENT and return (TITLE SECTIONS ITEMS).
ITEMS is a list with keys id, section, text, done, line."
  (let ((lines (split-string content "\n"))
        (title nil)
        (section-info (list))
        (items (list))
        (line-num 0))
    (dolist (line lines)
      (setq line-num (1+ line-num))
      (cond
       ((and (null title) (string-match "^[ \t]*#[ \t]+\\(.*?\\)[ \t]*$" line))
        (setq title (match-string 1 line)))
       ((string-match "^[ \t]*##[ \t]*#*[ \t]+\\(.*?\\)[ \t]*$" line)
        (push (cons (match-string 1 line) line-num) section-info))))
    (setq section-info (nreverse section-info))
    (let ((section-names (mapcar #'car section-info))
          (section-starts (mapcar #'cdr section-info)))
      (setq line-num 0)
      (dolist (line lines)
        (setq line-num (1+ line-num))
        (when (string-match
               "^[ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\[\\([ xX]\\)\\][ \t]*\\(.*?\\)[ \t]*$"
               line)
          (let* ((done-char (match-string 1 line))
                 (text (match-string 2 line))
                 (section-idx 0)
                 (section-name nil))
            (dotimes (i (length section-starts))
              (let ((current-section-start (nth i section-starts))
                    (next-section-start (and (< (1+ i) (length section-starts))
                                             (nth (1+ i) section-starts))))
                (when (and (>= line-num current-section-start)
                           (or (null next-section-start)
                               (< line-num next-section-start)))
                  (setq section-idx (1+ i))
                  (setq section-name (nth i section-names)))))
            (let* ((items-in-section
                    (seq-filter (lambda (item)
                                  (let ((idx (string-to-number
                                             (car (split-string (plist-get item :id) "\\.")))))
                                    (= idx section-idx)))
                                items))
                   (item-num (1+ (length items-in-section)))
                   (id (format "%d.%d" section-idx item-num)))
              (push `(:id ,id
                          :section ,section-name
                          :text ,text
                          :done ,(member done-char '("x" "X"))
                          :line ,line-num)
                    items)))))
      (list title section-names (nreverse items)))))

(defun ygg-todo-read (file)
  "Read and parse FILE. Return list plist with cached results.
Signal user-error if FILE does not exist."
  (unless (file-exists-p file)
    (user-error "todo: file not found: %s" file))
  (let* ((abs-path (expand-file-name file))
         (key (ygg-todo--cache-key abs-path))
         (cached (and key (gethash abs-path ygg-todo--cache))))
    (if (and cached (equal (car cached) key))
        (cdr cached)
      (let* ((content (with-temp-buffer
                       (insert-file-contents abs-path)
                       (buffer-string)))
             (parsed (ygg-todo--parse-items content))
             (title (car parsed))
             (sections (cadr parsed))
             (items (caddr parsed))
             (result `(:file ,abs-path :title ,title :items ,items :sections ,sections)))
        (when key
          (puthash abs-path (cons key result) ygg-todo--cache))
        result))))

(defun ygg-todo-create (dir slug &optional title sections)
  "Create a new todo file in DIR with SLUG. Return absolute path.
DIR is created if needed. File is DIR/SLUG/tasks.md; if exists, try higher suffixes.
SECTIONS is a list of (NAME . LIST-OF-ITEM-TEXTS).
Never overwrites an existing file."
  (let* ((normalized-slug
          (let ((s (downcase slug)))
            (setq s (replace-regexp-in-string "[^a-z0-9-]" "-" s))
            (setq s (replace-regexp-in-string "-+" "-" s))
            (setq s (replace-regexp-in-string "^-\\|-$" "" s))
            (if (string-empty-p s) "tasks" s)
            (if (equal s "tasks") "tasks" s)))
         (slug-var normalized-slug)
         (counter 1)
         (target-dir (expand-file-name slug-var dir)))
    (while (file-exists-p (expand-file-name "tasks.md" target-dir))
      (setq counter (1+ counter))
      (setq slug-var (format "%s-%d" normalized-slug counter))
      (setq target-dir (expand-file-name slug-var dir)))
    (make-directory target-dir t)
    (let ((file-path (expand-file-name "tasks.md" target-dir)))
      (with-temp-buffer
        (when title
          (insert "# " title "\n\n"))
        (dolist (section sections)
          (let ((name (car section))
                (items (cdr section)))
            (insert "## " name "\n\n")
            (dolist (item-text items)
              (insert "- [ ] " item-text "\n"))
            (insert "\n")))
        (write-region (point-min) (point-max) file-path nil 'silent))
      (remhash (expand-file-name file-path) ygg-todo--cache)
      (let* ((list-plist (ygg-todo-read file-path))
             (item (car (plist-get list-plist :items))))
        (run-hook-with-args 'ygg-todo-changed-functions file-path 'create item))
      file-path)))

(defconst ygg-todo--item-line-re
  "^[ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\[[ xX]\\]"
  "A line holding a checklist item.")

(defun ygg-todo--add-at (lines section)
  "Where in LINES a new item for SECTION goes, as (INDEX . HOW), or nil.
INDEX is the line the item follows, or precedes when HOW is before; nil
means SECTION is absent.  A section takes the item after its last item,
or after its heading; items under no section go after the last of them,
else right before the first heading."
  (let ((n (length lines)) (i 0) found last)
    (if section
        (let ((head (format "^[ \t]*##+[ \t]+%s[ \t]*$" (regexp-quote section))))
          (while (and (< i n) (not found))
            (when (string-match head (nth i lines))
              (setq found t last i)
              (let ((j (1+ i)))
                (while (and (< j n) (not (string-match "^[ \t]*#" (nth j lines))))
                  (when (string-match ygg-todo--item-line-re (nth j lines))
                    (setq last j))
                  (setq j (1+ j)))))
            (setq i (1+ i)))
          (and found (cons last 'after)))
      (let (first-head)
        (while (and (< i n) (not first-head))
          (cond ((string-match "^[ \t]*##" (nth i lines)) (setq first-head i))
                ((string-match ygg-todo--item-line-re (nth i lines)) (setq last i)))
          (setq i (1+ i)))
        (cond (last (cons last 'after))
              (first-head (cons first-head 'before))
              (t nil))))))

(defun ygg-todo-add (file text &optional section)
  "Add TEXT as an open item to FILE, under SECTION when given, and return it.
A SECTION the file lacks is started at its end.  Every other byte stays."
  (let* ((abs-path (expand-file-name file))
         (content (with-temp-buffer (insert-file-contents abs-path) (buffer-string)))
         (lines (split-string content "\n"))
         (at (ygg-todo--add-at lines section))
         (entry (concat "- [ ] " text)))
    (with-temp-buffer
      (insert content)
      (cond
       ((and at (eq (cdr at) 'after))
        (goto-char (point-min))
        (forward-line (car at))
        (end-of-line)
        (insert "\n" entry))
       ((and at (eq (cdr at) 'before))
        (goto-char (point-min))
        (forward-line (car at))
        (insert entry "\n\n"))
       (t
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (if section
            (progn
              (unless (or (bobp) (looking-back "\n\n" nil)) (insert "\n"))
              (insert "## " section "\n\n" entry "\n"))
          (insert entry "\n"))))
      (write-region (point-min) (point-max) abs-path nil 'silent))
    (remhash abs-path ygg-todo--cache)
    (let ((item (car (last (seq-filter (lambda (it) (equal (plist-get it :text) text))
                                       (plist-get (ygg-todo-read abs-path) :items))))))
      (run-hook-with-args 'ygg-todo-changed-functions abs-path 'add item)
      item)))

(defun ygg-todo--find-item (items id-or-text)
  "Find item in ITEMS by :id or :text. Return item or nil."
  (seq-find (lambda (item)
              (or (equal (plist-get item :id) id-or-text)
                  (equal (plist-get item :text) id-or-text)))
            items))

(defun ygg-todo--safe-write (file id done-or-text expect what)
  "Safe write helper. Modifies line WHAT (done or rewrite).
Returns updated item or signals user-error."
  (let* ((abs-path (expand-file-name file))
         (content (with-temp-buffer
                   (insert-file-contents abs-path)
                   (buffer-string)))
         (parsed (ygg-todo--parse-items content))
         (items (caddr parsed))
         (item (ygg-todo--find-item items id)))
    (unless item
      (user-error "todo: item %s not found" id))
    (when (and expect (not (equal expect (plist-get item :text))))
      (let ((item-by-expect (ygg-todo--find-item items expect)))
        (unless item-by-expect
          (user-error "todo: item %s changed since it was read" id))
        (setq item item-by-expect)))
    (let* ((line-num (plist-get item :line))
           (lines (split-string content "\n"))
           (line-idx (1- line-num))
           (old-line (nth line-idx lines))
           (new-line
            (cond
             ((eq what 'done)
              (replace-regexp-in-string "\\[[ xX]\\]" "[x]" old-line))
             ((eq what 'undone)
              (replace-regexp-in-string "\\[[ xX]\\]" "[ ]" old-line))
             ((eq what 'rewrite)
              (replace-regexp-in-string
               "\\(^[ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\[[ xX]\\][ \t]*\\).*$"
               (concat "\\1" done-or-text)
               old-line)))))
      (setf (nth line-idx lines) new-line)
      (with-temp-buffer
        (insert (string-join lines "\n"))
        (unless (string-suffix-p "\n" content)
          (delete-char -1))
        (write-region (point-min) (point-max) abs-path nil 'silent))
      (remhash abs-path ygg-todo--cache)
      (let* ((list-plist (ygg-todo-read abs-path))
             (items (plist-get list-plist :items))
             (result-item (ygg-todo--find-item items id)))
        (run-hook-with-args 'ygg-todo-changed-functions abs-path what result-item)
        result-item))))

(defun ygg-todo-set-done (file id done &optional expect)
  "Toggle checkbox on item ID. Return updated item.
EXPECT is the text the caller last saw; if it differs, search for it by text."
  (ygg-todo--safe-write file id done expect (if done 'done 'undone)))

(defun ygg-todo-rewrite (file id text &optional expect)
  "Replace item TEXT on its line. Return updated item."
  (ygg-todo--safe-write file id text expect 'rewrite))

(defun ygg-todo-remove (file id &optional expect)
  "Delete the line for item ID. Return t."
  (let* ((abs-path (expand-file-name file))
         (content (with-temp-buffer
                   (insert-file-contents abs-path)
                   (buffer-string)))
         (parsed (ygg-todo--parse-items content))
         (items (caddr parsed))
         (item (ygg-todo--find-item items id)))
    (unless item
      (user-error "todo: item %s not found" id))
    (when (and expect (not (equal expect (plist-get item :text))))
      (let ((item-by-expect (ygg-todo--find-item items expect)))
        (unless item-by-expect
          (user-error "todo: item %s changed since it was read" id))
        (setq item item-by-expect)))
    (let* ((line-num (plist-get item :line))
           (lines (split-string content "\n"))
           (line-idx (1- line-num)))
      (setq lines (append (seq-subseq lines 0 line-idx)
                         (seq-subseq lines (1+ line-idx))))
      (with-temp-buffer
        (insert (string-join lines "\n"))
        (unless (string-suffix-p "\n" content)
          (unless (string-empty-p (buffer-string))
            (delete-char -1)))
        (write-region (point-min) (point-max) abs-path nil 'silent))
      (remhash abs-path ygg-todo--cache)
      (run-hook-with-args 'ygg-todo-changed-functions abs-path 'remove item))
    t))

(defun ygg-todo--normalize-text (text)
  "Normalize TEXT for matching."
  (let ((s text))
    (setq s (downcase s))
    (setq s (replace-regexp-in-string "^[0-9]+\\(?:\\.[0-9]+\\)?[.)]?[ \t]+" "" s))
    (setq s (replace-regexp-in-string "[ \t]+" " " s))
    (setq s (string-trim s))
    (setq s (replace-regexp-in-string "[.:;]+$" "" s))
    s))

(defun ygg-todo-tick-matching (file texts)
  "Tick every open item whose normalized text equals one of TEXTS."
  (let* ((abs-path (expand-file-name file))
         (list-plist (ygg-todo-read abs-path))
         (items (plist-get list-plist :items))
         (normalized-texts (mapcar #'ygg-todo--normalize-text texts))
         (to-tick (seq-filter
                   (lambda (item)
                     (and (not (plist-get item :done))
                          (member (ygg-todo--normalize-text (plist-get item :text))
                                  normalized-texts)))
                   items))
         (ticked (list)))
    (dolist (item to-tick)
      (let ((id (plist-get item :id)))
        (ygg-todo-set-done abs-path id t)
        (let* ((updated-list (ygg-todo-read abs-path))
               (updated-items (plist-get updated-list :items))
               (updated-item (ygg-todo--find-item updated-items id)))
          (push updated-item ticked))))
    (nreverse ticked)))

(defun ygg-todo-progress (file)
  "Return (DONE . TOTAL) progress for FILE."
  (let* ((list-plist (ygg-todo-read file))
         (items (plist-get list-plist :items)))
    (cons (length (seq-filter (lambda (item) (plist-get item :done)) items))
          (length items))))

(defun ygg-todo-format (file &optional all)
  "FILE as an agent reads it, in as few tokens as it can carry.
A first line with the path, relative to the project when it can be, and
the done count; section names; each open item as its id and text; the
finished ones of a section as one line of ids.  ALL spells out the
finished items too."
  (let* ((list-plist (ygg-todo-read file))
         (abs-path (plist-get list-plist :file))
         (items (plist-get list-plist :items))
         (root (locate-dominating-file abs-path ".git"))
         (done-n (seq-count (lambda (it) (plist-get it :done)) items))
         (lines (list (format "%s %d/%d"
                              (if root (file-relative-name abs-path root) abs-path)
                              done-n (length items))))
         (groups nil))
    (dolist (item items)
      (let ((sec (plist-get item :section)))
        (if (and groups (equal (caar groups) sec))
            (push item (cdar groups))
          (push (list sec item) groups))))
    (dolist (group (nreverse groups))
      (let ((sec (car group))
            (its (reverse (cdr group))))
        (when sec (push (format "## %s" sec) lines))
        (dolist (it its)
          (when (or all (not (plist-get it :done)))
            (push (format "%s%s %s" (if (plist-get it :done) "x " "")
                          (plist-get it :id) (plist-get it :text))
                  lines)))
        (unless all
          (when-let* ((done (seq-filter (lambda (it) (plist-get it :done)) its)))
            (push (concat "done " (mapconcat (lambda (it) (plist-get it :id)) done " "))
                  lines)))))
    (string-join (nreverse lines) "\n")))

(defalias 'ygg-todo-normalize #'ygg-todo--normalize-text)

(declare-function aob-session-get "aob" (id))
(declare-function aob-session-p "aob" (s))
(declare-function aob-session-ref "aob" (s key))
(declare-function aob-session-put "aob" (s key val))
(declare-function aob-session-project "aob" (s))
(declare-function aob-session-dir "aob" (s))
(declare-function aob--dirty "aob" (&optional s))
(declare-function aob-session-name "aob" (s))

(defun ygg-todo--session (s)
  "S when it is a session, the session with id S when it is a string, else nil."
  (cond ((stringp s) (and (fboundp 'aob-session-get) (aob-session-get s)))
        ((and (fboundp 'aob-session-p) (aob-session-p s)) s)))

(defun ygg-todo-session-file (s)
  "The list bound to session S when it still exists, else nil."
  (when-let* ((s (ygg-todo--session s))
              (file (aob-session-ref s :todo-file)))
    (and (file-exists-p file) file)))

(defun ygg-todo-session-bind (s file)
  "Bind FILE as the list of session S."
  (when-let* ((s (ygg-todo--session s)))
    (aob-session-put s :todo-file (expand-file-name file))
    (when (fboundp 'aob--dirty) (aob--dirty s))
    ;; sessions are otherwise written only on exit, and a crash would
    ;; leave the list without the session it belongs to
    (when (fboundp 'aob-acp--persist) (ignore-errors (aob-acp--persist)))
    file))

(defun ygg-todo-session-dir (s)
  "Where new lists for session S are created."
  (when-let* ((s (ygg-todo--session s)))
    (expand-file-name ".aob/tasks/"
                      (or (aob-session-project s) (aob-session-dir s)
                          default-directory))))

;;; A session's list: its agent's plan carried in, the user's edits told back

(defvar ygg-todo--pending (make-hash-table :test #'equal)
  "List file to the user changes its agent has not read, newest first.")

(defvar ygg-todo--dropped (make-hash-table :test #'equal)
  "List file to the item keys the user removed or reworded away.")

(defvar ygg-todo--owned (make-hash-table :test #'equal)
  "List file to the item keys its agent's last plan put there.")

(defvar ygg-todo--seen (make-hash-table :test #'equal)
  "List file to its items as last written through here, (KEY TEXT . DONE) each.")

(defconst ygg-todo--verbs
  '((add . "added") (done . "ticked") (undone . "unticked")
    (rewrite . "reworded") (remove . "removed")))

(defun ygg-todo--note-change (file what text)
  "Hold the user change WHAT to the item TEXT of FILE for its agent."
  (push (cons what text) (gethash file ygg-todo--pending))
  (when (eq what 'remove)
    (cl-pushnew (ygg-todo-normalize text) (gethash file ygg-todo--dropped)
                :test #'equal)))

(defun ygg-todo--record (file what item)
  "Hold a change the user made through the list view."
  (when (and (eq ygg-todo-by 'user) item (assq what ygg-todo--verbs))
    (ygg-todo--note-change (expand-file-name file) what (plist-get item :text))))

(add-hook 'ygg-todo-changed-functions #'ygg-todo--record)

(defun ygg-todo--items-seen (file)
  (mapcar (lambda (it)
            (cons (ygg-todo-normalize (plist-get it :text))
                  (cons (plist-get it :text) (and (plist-get it :done) t))))
          (plist-get (ygg-todo-read file) :items)))

(defun ygg-todo--seen-update (file &rest _)
  (let ((file (expand-file-name file)))
    (when (file-exists-p file)
      (puthash file (ygg-todo--items-seen file) ygg-todo--seen))))

(add-hook 'ygg-todo-changed-functions #'ygg-todo--seen-update 90)

(defun ygg-todo--reconcile (file)
  "Hold as the user's whatever changed in FILE outside this library."
  (let ((now (ygg-todo--items-seen file))
        (seen (gethash file ygg-todo--seen 'unknown)))
    (unless (eq seen 'unknown)
      (dolist (n now)
        (let ((was (assoc (car n) seen)))
          (cond ((null was) (ygg-todo--note-change file 'add (cadr n)))
                ((not (eq (cddr was) (cddr n)))
                 (ygg-todo--note-change file (if (cddr n) 'done 'undone) (cadr n))))))
      (dolist (w seen)
        (unless (assoc (car w) now)
          (ygg-todo--note-change file 'remove (cadr w)))))
    (puthash file now ygg-todo--seen)))

(defun ygg-todo-session-note (s)
  "The user changes to S's list its agent has not read, as a note; then forgotten.
Nil when there are none."
  (when-let* ((file (ygg-todo-session-file s))
              (file (expand-file-name file)))
    (ygg-todo--reconcile file)
    (when-let* ((changes (gethash file ygg-todo--pending)))
      (remhash file ygg-todo--pending)
      (concat (format "Todo list changes by the user since you last read it (%s):"
                      file)
              (mapconcat (lambda (c)
                           (format "\n- %s: %s"
                                   (alist-get (car c) ygg-todo--verbs)
                                   (cdr c)))
                         (reverse changes) "")))))

(defun ygg-todo-note-read (s file)
  "Forget the user changes to FILE pending for session S, whose agent read it."
  (when (ygg-todo--session s)
    (let ((file (expand-file-name file)))
      (when (file-exists-p file) (ygg-todo--reconcile file))
      (remhash file ygg-todo--pending))))

(defun ygg-todo--plan-entries (u)
  "The entries of plan update U that say something, as (:content :status)."
  (seq-remove
   (lambda (e) (string-empty-p (plist-get e :content)))
   (mapcar (lambda (e)
             (list :content (string-trim
                             (replace-regexp-in-string
                              "[\n\r]+" " " (or (plist-get e :content) "")))
                   :status (plist-get e :status)))
           (plist-get u :entries))))

(defun ygg-todo--keyed (file key)
  "FILE's item whose text normalizes to KEY, or nil."
  (seq-find (lambda (it) (equal (ygg-todo-normalize (plist-get it :text)) key))
            (plist-get (ygg-todo-read file) :items)))

(defun ygg-todo-mirror-plan (s u)
  "Make the agent's own items in S's list match plan snapshot U.
The snapshot replaces what the agent's plan put there before: its
entries are added, reworded, ticked and unticked to match, and items it
no longer names go.  What the user added or changed stays and waits for
the agent as a note; an item the user removed is not put back.  A list
is started for a first plan when S has none."
  (let* ((ygg-todo-by 'agent)
         (entries (ygg-todo--plan-entries u))
         (file (or (ygg-todo-session-file s)
                   (when entries
                     (let ((path (ygg-todo-create (ygg-todo-session-dir s)
                                                  (aob-session-name s)
                                                  (aob-session-name s) nil)))
                       (ygg-todo-session-bind s path)
                       path)))))
    (when file
      (let ((file (expand-file-name file)))
        (ygg-todo--reconcile file)
        (dolist (k (gethash file ygg-todo--owned))
          (unless (assoc k (gethash file ygg-todo--seen))
            (cl-pushnew k (gethash file ygg-todo--dropped) :test #'equal)))
        (let* ((key (lambda (e) (ygg-todo-normalize (plist-get e :content))))
               (have (mapcar #'car (gethash file ygg-todo--seen)))
               (held (mapcar (lambda (c) (ygg-todo-normalize (cdr c)))
                             (gethash file ygg-todo--pending)))
               (dropped (gethash file ygg-todo--dropped))
               (want (seq-uniq (seq-remove (lambda (e) (member (funcall key e) dropped))
                                           entries)
                               (lambda (a b) (equal (funcall key a) (funcall key b)))))
               (keys (mapcar key want))
               (stale (seq-filter (lambda (k) (and (member k have)
                                                   (not (member k keys))
                                                   (not (member k held))))
                                  (gethash file ygg-todo--owned)))
               (fresh (seq-remove (lambda (e) (member (funcall key e) have)) want)))
          (while (and stale fresh)
            (when-let* ((it (ygg-todo--keyed file (pop stale))))
              (ygg-todo-rewrite file (plist-get it :id) (plist-get (pop fresh) :content)
                                (plist-get it :text))))
          (dolist (k stale)
            (when-let* ((it (ygg-todo--keyed file k)))
              (ygg-todo-remove file (plist-get it :id) (plist-get it :text))))
          (dolist (e fresh)
            (ygg-todo-add file (plist-get e :content)))
          (dolist (e want)
            (let ((done (equal (plist-get e :status) "completed")))
              (when-let* (((not (member (funcall key e) held)))
                          (it (ygg-todo--keyed file (funcall key e)))
                          ((not (eq done (and (plist-get it :done) t)))))
                (ygg-todo-set-done file (plist-get it :id) done (plist-get it :text)))))
          (puthash file (seq-filter (lambda (k) (ygg-todo--keyed file k)) keys)
                   ygg-todo--owned))))))

(provide 'ygg-todo)
;;; ygg-todo.el ends here
