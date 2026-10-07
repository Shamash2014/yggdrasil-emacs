;;; aob-transcript.el --- a conversation that ended, opened as itself -*- lexical-binding: t; -*-

;;; Commentary:
;; A session that ended is still a session.  It has a name, a folder, a
;; conversation and an id that can bring its agent back — everything a
;; running one has except the process.  So it opens as one: the same
;; object, the same trace, the same keys.  There is no second kind of
;; window for old work.
;;
;; Nothing is started by opening it.  The turns are read from the file
;; the CLI wrote as they happened, under the config home the session ran
;; with.  The agent comes back when you write to it, and not before.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob)
(require 'aob-trace)

(declare-function ygg-agent--config-env "ygg-agent-conf" (preset cmd project &optional isolate))
(declare-function ygg-agent--own-home "ygg-agent-conf" (kind repo &optional isolate))
(declare-function ygg-agent--repo-home "ygg-agent-conf" (project))
(declare-function aob-transcript-pi-p "aob-transcript-pi" (agent))
(declare-function aob-transcript-pi--dirs "aob-transcript-pi" (agent project where))
(declare-function aob-transcript-pi--head "aob-transcript-pi" (file))
(declare-function aob-transcript-pi-file "aob-transcript-pi" (id dir homes))
(declare-function aob-transcript-pi--active "aob-transcript-pi" (file recs))
(declare-function aob-transcript-pi--turn "aob-transcript-pi" (rec))
(declare-function aob-transcript-pi--tools "aob-transcript-pi" (rec))
(declare-function aob-transcript-pi-forget "aob-transcript-pi" ())
(declare-function aob-transcript-pi-move "aob-transcript-pi" (file where then))
(declare-function aob-transcript-pi-restore "aob-transcript-pi" (entry))
(declare-function aob-acp-resume-entry "aob-acp" (e &optional pref))
(declare-function aob-acp-delete-entry "aob-acp" (entry &optional then))
(declare-function aob-acp--tool-title "aob-acp" (u raw))

(defgroup aob-transcript nil
  "Conversations that already happened."
  :group 'aob :prefix "aob-transcript-")

(defvar aob-transcript--homes (make-hash-table :test 'equal)
  "(AGENT . DIR) to config home: working it out reads settings and
keychains, and every row of the sidebar asks for it.")

(defun aob-transcript--home (agent dir)
  "The config home AGENT used in DIR, or its default one."
  (let ((key (cons agent dir)))
    (or (gethash key aob-transcript--homes)
        (puthash key (aob-transcript--home-1 agent dir) aob-transcript--homes))))

(defun aob-transcript--home-1 (agent dir)
  "Work out AGENT's config home in DIR from its configuration."
  (or (when (fboundp 'ygg-agent--config-env)
        (when-let* ((kind (aob-transcript--kind agent))
                    (entry (ygg-agent--config-env kind kind dir))
                    ((string-match "=\\(.*\\)\\'" entry)))
          (match-string 1 entry)))
      (aob-transcript--default-home agent)))

(defun aob-transcript--kind (agent)
  "The agent kind AGENT\='s config is filed under: pi for any alias of it."
  (if (aob-transcript-pi-p agent) "pi" agent))

(defun aob-transcript--default-home (agent)
  "The config home AGENT\='s CLI uses when nothing points it elsewhere."
  (expand-file-name (cond ((equal agent "codex") "~/.codex")
                          ((aob-transcript-pi-p agent) "~/.pi/agent")
                          (t "~/.claude"))))

(defun aob-transcript--slug (dir)
  "DIR as the CLI spells it when naming a folder.
Every character that is not a letter or a digit becomes a dash — the
dots included, which is how a worktree of justfin.git is filed under
justfin-git and not under justfin.git."
  (replace-regexp-in-string "[^A-Za-z0-9]" "-"
                            (directory-file-name (expand-file-name dir))))

(defun aob-transcript--own-home (agent dir)
  "The home this config keeps for DIR, whether or not sessions use it.
A home whose login has lapsed is passed over when a session starts, and
still holds everything it was written before that."
  (let ((key (list 'own agent dir)))
    (let ((known (gethash key aob-transcript--homes)))
      (unless known
        (setq known (or (and (fboundp 'ygg-agent--own-home)
                             (bound-and-true-p ygg-agent-conf-root)
                             (ignore-errors
                               (ygg-agent--own-home
                                (aob-transcript--kind agent)
                                (ygg-agent--repo-home dir))))
                        'none))
        (puthash key known aob-transcript--homes))
      (unless (eq known 'none) known))))

(defun aob-transcript--homes (agent dir)
  "Every config home AGENT may have written DIR\='s conversations under.
The one a session from here runs with, the one this config keeps for
the project, and the one the CLI uses when you start it yourself — a
project has a history from all three."
  (delete-dups
   (delq nil (list (aob-transcript--home agent dir)
                   (aob-transcript--own-home agent dir)
                   (aob-transcript--default-home agent)))))

(defvar aob-transcript--codex-files (make-hash-table :test 'equal)
  "Codex session id to its file: finding one walks a folder per day.")

(defun aob-transcript--codex-file (id homes)
  "The file Codex wrote session ID to under one of HOMES.
Codex files by the day it started, not by project, as
sessions/YYYY/MM/DD/rollout-<time>-ID.jsonl."
  (let ((known (gethash id aob-transcript--codex-files)))
    (if (and known (file-readable-p known))
        known
      (when-let* ((file (seq-some
                         (lambda (home)
                           (car (append
                                 (file-expand-wildcards
                                  (expand-file-name
                                   (format "sessions/*/*/*/rollout-*-%s.jsonl" id) home))
                                 (file-expand-wildcards
                                  (expand-file-name
                                   (format "archived_sessions/rollout-*-%s.jsonl" id) home)))))
                         homes)))
        (puthash id file aob-transcript--codex-files)))))

(defun aob-transcript-file (entry)
  "Where ENTRY's conversation was written, if it is still there."
  (or (when-let* ((file (plist-get entry :file)) ((file-readable-p file))) file)
      (when-let* ((id (plist-get entry :acp-id))
                  (dir (or (plist-get entry :dir) (plist-get entry :project))))
        (let* ((agent (or (plist-get entry :agent) "claude"))
               (homes (aob-transcript--homes agent dir)))
          (cond
           ((equal agent "codex")
            (aob-transcript--codex-file id homes))
           ((aob-transcript-pi-p agent)
            (aob-transcript-pi-file id dir homes))
           (t
            (seq-some
             (lambda (home)
               (let ((file (expand-file-name
                            (format "projects/%s/%s.jsonl" (aob-transcript--slug dir) id)
                            home)))
                 (and (file-readable-p file) file)))
             homes)))))))

(defvar aob-transcript--titles (make-hash-table :test 'equal)
  "File to (MTIME . TITLE): reading the head of one is not free.")

(defvar aob-transcript--found (make-hash-table :test 'equal)
  "Project to (MTIME . ENTRIES): the listing is redone when the folder moves.")

(defvar aob-transcript--stat-memo nil
  "File to its modification time while one drawing asks, else nil.
A sidebar redraw asks after the same files from several rows.")

(defun aob-transcript--mtime (file)
  (let ((stat (lambda ()
                (float-time (file-attribute-modification-time (file-attributes file))))))
    (if aob-transcript--stat-memo
        (with-memoization (gethash file aob-transcript--stat-memo) (funcall stat))
      (funcall stat))))

(defvar aob-transcript-titles-hook nil
  "Run with no arguments when titles read in the background land.")

(defvar aob-transcript--queue nil
  "Files whose opening line is still to be read.")

(defvar aob-transcript--timer nil)

(defcustom aob-transcript-titles-per-tick 6
  "Conversations whose opening line is read in one go of the idle timer."
  :type 'natnum :group 'aob-transcript)

(defun aob-transcript--title-read-p (file)
  "Non-nil when FILE\='s opening has been read since it last changed.
A file with no opening line to read counts as read, or every draw would
queue it again and the reader would never settle."
  (let ((cell (gethash file aob-transcript--titles)))
    (and cell (equal (car cell) (aob-transcript--mtime file)))))

(defun aob-transcript--title-cached (file)
  "FILE\='s opening line if it has already been read, else nil."
  (when (aob-transcript--title-read-p file)
    (cdr (gethash file aob-transcript--titles))))

(defun aob-transcript--title (file)
  "What the conversation in FILE opened with, reading it if need be.
Cached against the file\='s own clock: this runs for every row of the
sidebar, and a row is drawn whenever anything moves."
  (or (aob-transcript--title-cached file)
      (let ((title (aob-transcript--title-1 file)))
        (puthash file (cons (aob-transcript--mtime file) title)
                 aob-transcript--titles)
        title)))

(defun aob-transcript--want-title (file)
  "Ask for FILE\='s opening line to be read when there is a moment."
  (unless (member file aob-transcript--queue)
    (setq aob-transcript--queue (append aob-transcript--queue (list file))))
  (unless aob-transcript--timer
    (setq aob-transcript--timer
          (run-with-idle-timer 0.2 t #'aob-transcript--read-some))))

(defun aob-transcript--read-some ()
  "Read the next few queued openings, then let whoever draws know."
  (let ((n aob-transcript-titles-per-tick)
        (any nil))
    (while (and (> n 0) aob-transcript--queue)
      (let ((file (pop aob-transcript--queue)))
        (when (file-readable-p file)
          (aob-transcript--title file)
          (setq any t)))
      (setq n (1- n)))
    (when any
      ;; the listing was built from what was cached at the time
      (clrhash aob-transcript--found)
      (run-hooks 'aob-transcript-titles-hook))
    (unless aob-transcript--queue
      (when aob-transcript--timer
        (cancel-timer aob-transcript--timer)
        (setq aob-transcript--timer nil)))))

(defun aob-transcript--title-1 (file)
  "Read FILE\='s title off the disk, a little of it at a time.
The name the conversation was given where it has one, else its opening
line."
  (or (aob-transcript--named-in file 65536)
      (aob-transcript--title-in file 16384)
      (and (> (or (file-attribute-size (file-attributes file)) 0) 16384)
           (aob-transcript--title-in file 262144))))

(defun aob-transcript--named-in (file bytes)
  "The name FILE\='s conversation was given, looking only at its last BYTES.
Claude appends the name you set and the one it makes up itself after
turns, and again as they change, so the newest of each is near the end;
yours wins.  Codex appends its thread name the same way, and pi a
session_info entry."
  (let ((size (or (file-attribute-size (file-attributes file)) 0))
        names)
    (with-temp-buffer
      (ignore-errors (insert-file-contents file nil (max 0 (- size bytes)) size))
      (goto-char (point-min))
      (while (re-search-forward
              "\"type\":\"\\(custom-title\\|ai-title\\|thread_name_updated\\|session_info\\)\"" nil t)
        (when-let* ((kind (match-string 1))
                    (rec (ignore-errors
                           (json-parse-string
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))
                            :object-type 'alist :null-object nil :false-object nil)))
                    (name (or (alist-get 'customTitle rec) (alist-get 'aiTitle rec)
                              (alist-get 'thread_name (alist-get 'payload rec))
                              (and (equal kind "session_info") (alist-get 'name rec))))
                    ((stringp name))
                    (name (string-trim name))
                    ((not (string-empty-p name))))
          (setf (alist-get kind names nil nil #'equal) name))
        (forward-line 1)))
    (when-let* ((name (or (cdr (assoc "custom-title" names))
                          (cdr (assoc "ai-title" names))
                          (cdr (assoc "thread_name_updated" names))
                          (cdr (assoc "session_info" names)))))
      (truncate-string-to-width name 44 nil nil t))))

(defun aob-transcript--title-in (file bytes)
  "FILE\='s opening line, looking only at its first BYTES."
  (with-temp-buffer
    ;; the opening line is at the top: a small read first, and the
    ;; larger one only where a preamble pushed it down
    (ignore-errors (insert-file-contents file nil 0 bytes))
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (rec (and (not (string-empty-p line))
                         (ignore-errors
                           (json-parse-string line :object-type 'alist
                                              :null-object nil
                                              :false-object nil)))))
          (when-let* ((summary (and (equal (alist-get 'type rec) "summary")
                                    (alist-get 'summary rec)))
                      ((stringp summary)))
            ;; older Claude opened its file with what the talk was about
            (throw 'found (truncate-string-to-width summary 44 nil nil t)))
          (when-let* ((turn (and rec (aob-transcript--turn rec)))
                      ((equal (car turn) "user")))
            (when-let* ((text (cdr turn))
                        ;; the harness writes its own preamble in as a
                        ;; user turn; the first thing a person said is
                        ;; what this is after
                        ((not (string-prefix-p "<" text)))
                        ((not (string-prefix-p "Caveat:" text)))
                        ((not (string-prefix-p "# AGENTS.md" text))))
              (throw 'found
                     (truncate-string-to-width
                      (car (split-string text "\n" t)) 44 nil nil t)))))
        (forward-line 1))
      nil)))

;;;###autoload
(defun aob-transcript-found (project &optional agent where)
  "Conversations AGENT left on disk for PROJECT, newest first.
The CLI writes one file per conversation under its config home.  What
this Emacs knows about is what it started itself, which for a project
you have only just taken in is none of them."
  (let ((agent (or agent (bound-and-true-p aob-acp-default-agent) "claude")))
    (if (equal agent "codex")
        (aob-transcript--codex-found project where)
      (let* ((dirs (if (aob-transcript-pi-p agent)
                       (aob-transcript-pi--dirs agent project where)
                     (seq-filter
                      #'file-directory-p
                      (mapcar (lambda (home)
                                (expand-file-name
                                 (format "projects/%s%s" (aob-transcript--slug project)
                                         (if where (concat "/" where) ""))
                                 home))
                              (aob-transcript--homes agent project)))))
             (key (list agent dirs where)))
        (when dirs
          ;; the folder's own clock says when a conversation was added to it
          ;; or written to; until it moves, the listing stands
          (let ((stamp (mapcar #'aob-transcript--mtime dirs))
                (cell (gethash key aob-transcript--found)))
            (if (and cell (equal (car cell) stamp))
                (cdr cell)
              (let ((entries (aob-transcript--found-1 project agent dirs where)))
                (puthash key (cons stamp entries) aob-transcript--found)
                entries))))))))

(defun aob-transcript--found-1 (project agent dirs &optional where)
  "Read DIRS, which hold AGENT\='s conversations about PROJECT.
WHERE, when given, is the folder they were put away in."
  (aob-transcript--entries
   project agent where
   (let ((files (seq-mapcat (lambda (dir) (directory-files dir t "\\.jsonl\\'")) dirs)))
     (if (aob-transcript-pi-p agent)
         (delq nil (mapcar #'aob-transcript-pi--head files))
       (mapcar (lambda (file) (list file (file-name-base file) project)) files)))))

(defun aob-transcript--entries (project agent where found)
  "AGENT\='s conversations about PROJECT as entries, newest first.
FOUND holds (FILE ID DIR [NAME]) for each, DIR the folder it ran in and
NAME what it was called, where it was; WHERE, when given, is the folder
they were put away in."
  (let ((found (sort (mapcar (lambda (row) (cons (aob-transcript--mtime (car row)) row))
                             (if where found
                               (seq-remove #'aob-transcript--discarded-p found)))
                     (lambda (a b) (> (car a) (car b)))))
        seen)
    (mapcar (lambda (row)
              (pcase-let* ((`(,ts ,file ,id ,dir . ,given) row)
                           (dir (file-name-as-directory (expand-file-name dir)))
                           (entry (list :agent agent
                                        :acp-id id
                                        :project (file-name-as-directory
                                                  (expand-file-name project))
                                        :dir dir
                                        :ts ts
                                        ;; which home it came out of is
                                        ;; not derivable from the entry
                                        :file file
                                        :found t
                                        :archived (and where t)))
                           ;; the list first: an opening line is a read,
                           ;; and a hundred reads is not a listing.  What
                           ;; has been read is used, the rest is asked
                           ;; for and arrives on a later draw
                           (name (or (and (car given)
                                          (truncate-string-to-width (car given) 44 nil nil t))
                                     (aob-transcript--title-cached file)
                                     (progn (unless (aob-transcript--title-read-p file)
                                              (aob-transcript--want-title file))
                                            (aob-transcript--name entry file))))
                           (name (if (member name seen)
                                     (format "%s %s" name (substring id 0 4))
                                   name)))
                (push name seen)
                (plist-put entry :name name)))
            found)))

(defun aob-transcript--discarded-p (row)
  "Whether ROW\='s file already has its copy in the discarded folder beside it.
The original goes only once its agent answers the delete; until then it
is gone as far as the listing is concerned."
  (let ((file (car row)))
    (file-exists-p (expand-file-name (file-name-nondirectory file)
                                     (expand-file-name "discarded"
                                                       (file-name-directory file))))))

(defvar aob-transcript--codex-heads (make-hash-table :test 'equal)
  "Rollout file to (MTIME ID . CWD), for a home with no thread index.")

(defun aob-transcript--codex-found (project where)
  "Codex conversations held in PROJECT or a folder under it, newest first.
Codex files by day, not by project, so every home is asked which of its
threads ran here: its thread index where it keeps one, the head of each
rollout where it does not.  WHERE is as for `aob-transcript-found'; a
thread Codex archived itself counts as put away in \"archive\"."
  (let* ((root (file-name-as-directory (expand-file-name project)))
         (homes (seq-filter #'file-directory-p (aob-transcript--homes "codex" root)))
         (key (list "codex" root where))
         (stamp (mapcar (lambda (home)
                          (mapcar (lambda (db) (and (file-exists-p db) (aob-transcript--mtime db)))
                                  (list (expand-file-name "state_5.sqlite" home)
                                        (expand-file-name "state_5.sqlite-wal" home))))
                        homes))
         (indexed (seq-every-p #'car stamp))
         (cell (gethash key aob-transcript--found)))
    (if (and indexed cell (equal (car cell) stamp))
        (cdr cell)
      (let* ((rows (seq-mapcat (lambda (home) (aob-transcript--codex-rows home root where))
                               homes))
             (entries (aob-transcript--entries
                       root "codex" where (seq-uniq rows (lambda (a b) (equal (nth 1 a) (nth 1 b)))))))
        (when indexed (puthash key (cons stamp entries) aob-transcript--found))
        entries))))

(defun aob-transcript--codex-names (home)
  "HOME\='s thread names by thread id.
Codex keeps a name you gave a thread, or one it gave it, in its thread
index and in session_index.jsonl beside it; either may be missing."
  (let ((names (make-hash-table :test 'equal))
        (db (expand-file-name "state_5.sqlite" home))
        (index (expand-file-name "session_index.jsonl" home)))
    (when (and (file-exists-p db) (sqlite-available-p))
      (condition-case nil
          (let ((conn (sqlite-open db t)))
            (unwind-protect
                (pcase-dolist (`(,id ,name)
                               (sqlite-select conn "select id, name from threads
                                                     where name <> ''"))
                  (puthash id name names))
              (sqlite-close conn)))
        (error nil)))
    (when (file-readable-p index)
      (with-temp-buffer
        (insert-file-contents index)
        (dolist (line (split-string (buffer-string) "\n" t))
          (when-let* ((rec (ignore-errors
                             (json-parse-string line :object-type 'alist
                                                :null-object nil :false-object nil)))
                      (id (alist-get 'id rec))
                      (name (alist-get 'thread_name rec))
                      ((stringp name))
                      ((not (string-empty-p (string-trim name)))))
            (puthash id (string-trim name) names)))))
    names))

(defun aob-transcript--codex-rows (home root where)
  "(FILE ID CWD NAME) for each of HOME\='s threads that ran in ROOT or under it."
  (let ((names (aob-transcript--codex-names home)))
    (mapcar (lambda (row) (append row (list (gethash (nth 1 row) names))))
            (aob-transcript--codex-rows-1 home root where))))

(defun aob-transcript--codex-rows-1 (home root where)
  "(FILE ID CWD) for each of HOME\='s threads that ran in ROOT or under it."
  (let ((db (expand-file-name "state_5.sqlite" home)))
    (if (and (file-exists-p db) (sqlite-available-p))
        (delq nil
              (mapcar
               (pcase-lambda (`(,id ,path ,cwd ,archived))
                 ;; the index still points where the file was before
                 ;; it was put away beside it
                 (let ((file (if where
                                 (expand-file-name
                                  (file-name-nondirectory path)
                                  (expand-file-name where (file-name-directory path)))
                               path)))
                   (cond ((and (not where) (eq archived 1)) nil)
                         ((file-readable-p file) (list file id cwd))
                         ((and (equal where "archive") (eq archived 1)
                               (file-readable-p path))
                          (list path id cwd)))))
               (condition-case nil
                   (let ((conn (sqlite-open db t)))
                     (unwind-protect
                         (sqlite-select
                          conn
                          "select id, rollout_path, cwd, archived from threads
                           where cwd = ?1 or substr(cwd, 1, length(?2)) = ?2"
                          (list (directory-file-name root) root))
                       (sqlite-close conn)))
                 (error nil))))
      (seq-filter
       (lambda (row) (string-prefix-p root (file-name-as-directory (nth 2 row))))
       (delq nil
             (mapcar #'aob-transcript--codex-head
                     (seq-mapcat
                      (lambda (glob) (file-expand-wildcards (expand-file-name glob home)))
                      (if where
                          (delq nil (list (format "sessions/*/*/*/%s/rollout-*.jsonl" where)
                                          (format "archived_sessions/%s/rollout-*.jsonl" where)
                                          (and (equal where "archive")
                                               "archived_sessions/rollout-*.jsonl")))
                        (list "sessions/*/*/*/rollout-*.jsonl")))))))))

(defun aob-transcript--codex-head (file)
  "(FILE ID CWD) from the session_meta line FILE opens with.
That line carries the whole of Codex\='s instructions, so only its start
is read, and once per change of the file."
  (let* ((mtime (aob-transcript--mtime file))
         (cell (gethash file aob-transcript--codex-heads)))
    (unless (equal (car cell) mtime)
      (setq cell
            (puthash file
                     (cons mtime
                           (with-temp-buffer
                             (ignore-errors (insert-file-contents file nil 0 4096))
                             (let ((field (lambda (name)
                                            (goto-char (point-min))
                                            (when (re-search-forward
                                                   (format "[{,]\"%s\":\"\\([^\"]+\\)\"" name)
                                                   (line-end-position) t)
                                              (match-string 1)))))
                               (let ((id (funcall field "id"))
                                     (cwd (funcall field "cwd")))
                                 (and id cwd (cons id cwd))))))
                     aob-transcript--codex-heads)))
    (when (cdr cell)
      (list file (cadr cell) (cddr cell)))))

(defcustom aob-transcript-delete-wait 10
  "Seconds a discard waits for its agent to answer the delete.
Past that the original is removed anyway: the copy is already made, and
an agent that never answers should not keep it listed."
  :type 'number :group 'aob-transcript)

(defun aob-transcript-move (entry where &optional then)
  "Move ENTRY\='s conversation into the WHERE folder beside it.
Nothing is destroyed here: archiving and discarding are both a move, and
a folder the listing does not read is what \"gone\" means here.  A
discard copies first and tells a running agent that can delete to let
it go, since its delete removes the file it keeps; whatever that
leaves behind is removed here, once it answers or at once when nobody
is asked, so the copy is all that remains.  An agent that has not
answered within `aob-transcript-delete-wait' seconds is not waited for.
THEN is called when the move is done."
  (when-let* ((file (aob-transcript-file entry)))
    (if (aob-transcript-pi-p (plist-get entry :agent))
        (aob-transcript-pi-move file where then)
      (aob-transcript--move file entry where then))))

(defun aob-transcript--move (file entry where then)
  "Move FILE, ENTRY\='s conversation, into WHERE beside it.
THEN is called when it is done; see `aob-transcript-move\='."
  (let* ((dir (expand-file-name where (file-name-directory file)))
         (to (expand-file-name (file-name-nondirectory file) dir))
         (done nil)
         (timer nil)
         (finish (lambda (&rest _)
                   (unless done
                     (setq done t)
                     (when timer (cancel-timer timer))
                     (when (file-exists-p file) (delete-file file))
                     (aob-transcript-forget)
                     (when then (funcall then))))))
    (make-directory dir t)
    (if (not (equal where "discarded"))
        (progn (rename-file file to t)
               (aob-transcript-forget)
               (when then (funcall then)))
      (copy-file file to t t)
      (aob-transcript-forget)
      (if (and (fboundp 'aob-acp-delete-entry)
               (ignore-errors (aob-acp-delete-entry entry finish)))
          (unless done
            (setq timer (run-at-time aob-transcript-delete-wait nil finish)))
        (funcall finish)))
    to))

(defun aob-transcript-restore (entry)
  "Put ENTRY\='s conversation back where its agent looks for it, if it was put away.
Only pi keeps what it put away out of reach of its own lookup; the
rest read it where it lies."
  (when (aob-transcript-pi-p (plist-get entry :agent))
    (aob-transcript-pi-restore entry)))

;;;###autoload
(defun aob-transcript-forget ()
  "Drop what was read from disk, so the next look reads it again.
The folder\='s clock catches a conversation being written; a config
home moving is a change of mind, and nothing on disk says when."
  (interactive)
  (clrhash aob-transcript--homes)
  (clrhash aob-transcript--found)
  (clrhash aob-transcript--titles)
  (clrhash aob-transcript--codex-files)
  (clrhash aob-transcript--codex-heads)
  (aob-transcript-pi-forget)
  (setq aob-transcript--queue nil))

(defun aob-transcript--text (content)
  "The words in CONTENT, whatever shape the record used for it."
  (cond
   ((stringp content) content)
   ;; a json array parses to a vector, not a list
   ((seqp content)
    (string-join
     (append (delq nil
                   (seq-map (lambda (part)
                              (let ((type (alist-get 'type part)))
                                (cond
                                 ((member type '("text" "input_text" "output_text"))
                                  (alist-get 'text part))
                                 (t nil))))
                            content))
             nil)
     "\n"))
   (t nil)))

(defun aob-transcript--insert-tail (file lines)
  "Insert the last LINES lines of FILE into the current buffer.
A conversation's log is written whole — tool results and all — and runs
to tens of megabytes; only its end is ever shown, so only its end is
read.  The window is widened until it holds enough lines or reaches the
start of the file."
  (let* ((size (file-attribute-size (file-attributes file)))
         (span (min size 262144))
         (whole nil))
    (while (progn
             (erase-buffer)
             (setq whole (>= span size))
             (insert-file-contents file nil (- size span) size)
             (and (not whole)
                  (< (count-lines (point-min) (point-max)) (1+ lines))
                  (setq span (min size (* span 8))))))
    (goto-char (point-min))
    ;; the first line of a window that starts mid-file is half a record
    (unless whole
      (forward-line 1)
      (delete-region (point-min) (point)))
    (let ((extra (- (count-lines (point-min) (point-max)) lines)))
      (when (> extra 0)
        (goto-char (point-min))
        (forward-line extra)
        (delete-region (point-min) (point))))))

(defun aob-transcript--tail-records (file lines)
  "The last LINES records of FILE, parsed, oldest first."
  (let (recs)
    (with-temp-buffer
      (aob-transcript--insert-tail file lines)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when-let* (((not (string-empty-p line)))
                      (rec (ignore-errors
                             (json-parse-string line :object-type 'alist
                                                :null-object nil :false-object nil)))
                      ((listp rec)))
            (push rec recs)))
        (forward-line 1)))
    (nreverse recs)))

(defun aob-transcript-turns (file &optional tools)
  "FILE as a list of (WHO . TEXT), oldest first.
WHO is \"user\" or \"assistant\", and with TOOLS also \"tool\", a
tool\='s TEXT the line naming what it did.  Only its last records: a
session keeps `aob-event-cap\=' events and drops the older half past
that, so reading further back is work thrown away.  A pi file is a tree
of records, and what it shows is the branch the session ended on."
  (let (out)
    (dolist (rec (aob-transcript-pi--active
                  file (aob-transcript--tail-records file aob-event-cap)))
      (when-let* ((turn (aob-transcript--turn rec)))
        (push turn out))
      (dolist (tool (and tools (aob-transcript--tools rec)))
        (push (cons "tool" tool) out)))
    (nreverse out)))

(defun aob-transcript--harness-text-p (text)
  "Whether TEXT in a user turn was written in by the harness, not typed."
  (string-match-p
   "\\`\\(?:<\\|\\[workspace: \\|# AGENTS\\.md\\|\\[Request interrupted by user\\(?: for tool use\\)?]\\'\\)"
   text))

(defun aob-transcript--harness-part-p (part)
  "Whether PART of a user turn was written in by the harness, not typed."
  (when-let* ((text (alist-get 'text part)))
    (aob-transcript--harness-text-p text)))

(defun aob-transcript--command (text)
  "The slash or shell command TEXT is Claude\='s record of, as typed, or nil."
  (cond
   ((string-match "<command-name>\\([^<]*\\)</command-name>" text)
    (let ((name (match-string 1 text)))
      (string-trim
       (if (string-match "<command-args>\\(\\(?:.\\|\n\\)*?\\)</command-args>" text)
           (concat name " " (match-string 1 text))
         name))))
   ((string-match "\\`<bash-input>\\(\\(?:.\\|\n\\)*?\\)</bash-input>" text)
    (concat "!" (match-string 1 text)))))

(defun aob-transcript--turn (rec)
  "REC as (WHO . TEXT) when it is something said, else nil.
Claude writes a turn as {type: user|assistant, message: {content}};
Codex as {type: response_item, payload: {type: message, role, content}};
pi as {type: message, message: {role, content}}."
  (if (equal (alist-get 'type rec) "message")
      (aob-transcript-pi--turn rec)
    (aob-transcript--turn-1 rec)))

(defun aob-transcript--turn-1 (rec)
  "REC as (WHO . TEXT) when it is something Claude or Codex said, else nil."
  (let* ((codex (equal (alist-get 'type rec) "response_item"))
         (body (alist-get (if codex 'payload 'message) rec))
         (who (cond (codex (and (equal (alist-get 'type body) "message")
                                (alist-get 'role body)))
                    ;; written to the model, not by you
                    ((or (alist-get 'isMeta rec) (alist-get 'isCompactSummary rec)) nil)
                    (t (alist-get 'type rec))))
         (user (equal who "user")))
    (when-let* (((member who '("user" "assistant")))
                (content (alist-get 'content body))
                (content (if (and user (vectorp content))
                             (seq-remove #'aob-transcript--harness-part-p content)
                           content))
                (text (aob-transcript--text content))
                (text (string-trim text))
                (text (or (and user (aob-transcript--command text)) text))
                ((not (string-empty-p text)))
                ((not (and user (aob-transcript--harness-text-p text)))))
      (cons who text))))

(defun aob-transcript--tools (rec)
  "The tools REC calls, each as the line a live session would show.
Claude puts a call among an answer\='s parts, as does pi; Codex writes
each as its own record, its input a string of json or of code."
  (let ((body (alist-get 'payload rec))
        (message (alist-get 'message rec)))
    (cond
     ((equal (alist-get 'type rec) "message")
      (aob-transcript-pi--tools rec))
     ((equal (alist-get 'type rec) "response_item")
      (pcase (alist-get 'type body)
        ("function_call"
         (list (aob-transcript--tool-title
                (alist-get 'name body)
                (ignore-errors
                  (json-parse-string (alist-get 'arguments body)
                                     :object-type 'alist :null-object nil
                                     :false-object nil)))))
        ("custom_tool_call"
         (let ((input (or (alist-get 'input body) "")))
           (list (aob-transcript--tool-title
                  (alist-get 'name body)
                  (cond
                   ((string-match "\"cmd\":\\(\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\)" input)
                    `((cmd . ,(ignore-errors (json-parse-string (match-string 1 input))))))
                   ((string-match "^\\*\\*\\* \\(?:Add\\|Update\\|Delete\\) File: \\(.+\\)" input)
                    `((path . ,(match-string 1 input))))
                   (t `((cmd . ,input))))))))))
     ((and (equal (alist-get 'type rec) "assistant")
           (not (alist-get 'isMeta rec))
           (vectorp (alist-get 'content message)))
      (delq nil
            (seq-map (lambda (part)
                       (when (equal (alist-get 'type part) "tool_use")
                         (aob-transcript--tool-title (alist-get 'name part)
                                                     (alist-get 'input part))))
                     (alist-get 'content message)))))))

(defun aob-transcript--tool-title (name input)
  "The line for a call of tool NAME with INPUT, an alist, as live ones read."
  (let* ((name (or name "tool"))
         (raw (seq-mapcat (lambda (cell)
                            (let ((v (cdr cell)))
                              (list (if (eq (car cell) 'cmd) :command
                                      (intern (format ":%s" (car cell))))
                                    (if (vectorp v) (mapconcat (lambda (x) (format "%s" x)) v " ") v))))
                          (and (consp input) input))))
    (or (and (fboundp 'aob-acp--tool-title)
             (aob-acp--tool-title (list :title name) raw))
        name)))

;;; Opening one

(defun aob-transcript--name (entry file)
  "What to call ENTRY's session, told apart from the others.
Every conversation with one agent is called the same thing, and a trace
is named after its session — so without this, opening a second one
walks into the first one's buffer."
  (let* ((base (or (plist-get entry :name) (plist-get entry :agent) "session"))
         (day (and file (format-time-string
                         "%b %-d"
                         (file-attribute-modification-time
                          (file-attributes file)))))
         (name (if day (format "%s · %s" base day) base)))
    ;; two on the same day still need telling apart
    (if (seq-find (lambda (s) (equal (aob-session-name s) name)) (aob-sessions))
        (format "%s %s" name (substring (or (plist-get entry :acp-id) "") 0 4))
      name)))

(defun aob-transcript--session (entry file)
  "ENTRY as a session object read from FILE, its turns already in it.
Asleep: it carries the id its agent answers to, and no process."
  (let* ((id (concat "acp:" (plist-get entry :acp-id)))
         (existing (aob-session-get id)))
    (or existing
        (let ((s (aob-create-session
                  :id id :backend 'acp
                  :name (if (or (plist-get entry :found)
                                ;; already named for its day when it was
                                ;; found, or named by you: no day added
                                (plist-get entry :named-by-user))
                            (plist-get entry :name)
                          (aob-transcript--name entry file))
                  :project (or (plist-get entry :project) (plist-get entry :dir))
                  :dir (or (plist-get entry :dir) (plist-get entry :project))
                  :state 'done)))
          (aob-session-put s :agent (plist-get entry :agent))
          (aob-session-put s :acp-id (plist-get entry :acp-id))
          (aob-session-put s :model-id (plist-get entry :model))
          (aob-session-put s :mode-id (plist-get entry :mode))
          (aob-session-put s :asleep entry)
          (aob-session-put s :named-by-user (plist-get entry :named-by-user))
          (aob-session-put s :auto-named (plist-get entry :auto-named))
          (dolist (turn (aob-transcript-turns file t))
            (if (equal (car turn) "tool")
                (aob-event s 'tool :title (cdr turn) :status "completed")
              (aob-event s (if (equal (car turn) "user") 'prompt 'message)
                         :text (cdr turn)
                         ;; the user turns of a written conversation are yours
                         :typed (equal (car turn) "user"))))
          s))))

;;;###autoload
(defun aob-transcript-view (entry)
  "Open ENTRY's conversation as the session it was.
Nothing is started: writing to it is what brings its agent back."
  (interactive
   (list (let* ((entries (append (and (fboundp 'aob-acp-resumable-entries)
                                      (aob-acp-resumable-entries))
                                 (and (fboundp 'aob-acp-archived-entries)
                                      (aob-acp-archived-entries))))
                (rows (mapcar (lambda (e)
                                (cons (format "%s · %s"
                                              (or (plist-get e :name) "session")
                                              (abbreviate-file-name
                                               (or (plist-get e :dir) "")))
                                      e))
                              entries)))
           (unless rows (user-error "aob: no past conversations"))
           (cdr (assoc (completing-read "Open: " (mapcar #'car rows) nil t) rows)))))
  (let* ((file (or (aob-transcript-file entry)
                   (user-error "aob: no transcript on disk for %s"
                               (or (plist-get entry :name) "that session"))))
         (s (aob-transcript--session entry file))
         (buf (aob-trace-buffer s))
         ;; creating the session already put its trace on screen; showing
         ;; it again is how one conversation ends up in two windows
         (win (or (get-buffer-window buf 'visible)
                  (display-buffer buf (or (bound-and-true-p ygg-aob-trace-action)
                                          t)))))
    (when (window-live-p win) (select-window win))
    s))

(defun aob-transcript-asleep-p (s)
  "Whether S is a conversation whose agent is not running."
  (and (aob-session-ref s :asleep) t))

(declare-function aob-trace--name "aob-trace" (s))

(defun aob-transcript--revive (s)
  "Bring asleep S's agent back; return the session that takes S\='s place.
The resume succeeds S: its trace, still showing what you were reading in
the windows it was in, its row in every list, and its events until the
agent says more."
  (let ((live (aob-acp-resume-entry (aob-session-ref s :asleep))))
    (unless live
      (user-error "aob: %s would not come back" (aob-session-name s)))
    live))

(defun aob-transcript--wake (fn s &rest args)
  "Bring S's agent back before sending to it, if it is asleep."
  (if (aob-session-ref s :asleep)
      (apply fn (aob-transcript--revive s) args)
    (apply fn s args)))

(advice-add 'aob-prompt :around #'aob-transcript--wake)

;;;###autoload
(defun aob-transcript-wake (s)
  "Bring S's agent back without saying anything to it.
Writing to a sleeping conversation wakes it anyway; this is for when
you want it awake first — to set a mode or a model, or just to have it
there."
  (interactive (list (aob-target)))
  (unless (aob-session-ref s :asleep)
    (user-error "aob: %s is already awake" (aob-session-name s)))
  (let ((live (aob-transcript--revive s)))
    (when (and (fboundp 'aob-trace)
               (not (get-buffer-window (aob-trace--name live) 'visible)))
      (aob-trace live))
    live))

(provide 'aob-transcript)
(require 'aob-transcript-pi)
;;; aob-transcript.el ends here
