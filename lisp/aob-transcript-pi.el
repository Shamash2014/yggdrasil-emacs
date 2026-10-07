;;; aob-transcript-pi.el --- pi's conversations on disk -*- lexical-binding: t; -*-

;;; Commentary:
;; pi files a conversation per working folder under its config home as
;; sessions/--<path>--/<time>_<id>.jsonl, one json record a line.  The
;; first line is a header carrying the id the adapter answers to and the
;; folder it ran in; every other record has an id and a parent, so a
;; session is a tree, and the branch it ended on is the one that runs
;; from the last record written back to the root.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar aob-event-cap)
(declare-function ygg-pi-agent-p "ygg-pi" (agent))
(declare-function aob-transcript--homes "aob-transcript" (agent dir))
(declare-function aob-transcript--mtime "aob-transcript" (file))
(declare-function aob-transcript--text "aob-transcript" (content))
(declare-function aob-transcript--tool-title "aob-transcript" (name input))
(declare-function aob-transcript--harness-text-p "aob-transcript" (text))
(declare-function aob-transcript--tail-records "aob-transcript" (file lines))
(declare-function aob-transcript-file "aob-transcript" (entry))
(declare-function aob-transcript-forget "aob-transcript" ())

(defun aob-transcript-pi-p (agent)
  "Whether AGENT is pi: named pi or pi-*, or launched through its binary."
  (and (stringp agent)
       (or (string-match-p "\\`pi\\(?:-\\|\\'\\)" agent)
           (and (fboundp 'ygg-pi-agent-p)
                (ignore-errors (ygg-pi-agent-p agent))))))

(defun aob-transcript-pi--slug (dir)
  "DIR as pi names the folder holding its sessions.
The leading slash goes, every slash and colon becomes a dash, and the
whole is wrapped in a pair of them; dots stay."
  (concat "--"
          (replace-regexp-in-string
           "[/\\:]" "-"
           (string-remove-prefix "/" (directory-file-name (expand-file-name dir))))
          "--"))

(defun aob-transcript-pi--folder (home slug where)
  "Where HOME keeps SLUG's sessions, those put away in WHERE when it is given.
What is put away leaves pi's sessions tree, which its adapter lists, loads
and deletes from; beside it, pi never sees it."
  (expand-file-name (if where (format "aob-%s/%s" where slug) (concat "sessions/" slug))
                    home))

(defun aob-transcript-pi--dirs (agent project where)
  "The folders under AGENT's homes that hold PROJECT's sessions, put away in WHERE."
  (let ((slug (aob-transcript-pi--slug project)))
    (seq-filter
     #'file-directory-p
     (mapcar (lambda (home) (aob-transcript-pi--folder home slug where))
             (aob-transcript--homes agent project)))))

(defun aob-transcript-pi-file (id dir homes)
  "The file pi wrote session ID for DIR to, under one of HOMES, put away or not."
  (let ((slug (aob-transcript-pi--slug dir))
        (tail (format "_%s\\.jsonl\\'" (regexp-quote id))))
    (seq-some (lambda (where)
                (seq-some (lambda (home)
                            (let ((folder (aob-transcript-pi--folder home slug where)))
                              (car (and (file-directory-p folder)
                                        (directory-files folder t tail)))))
                          homes))
              '(nil "archive" "discarded"))))

(defun aob-transcript-pi--home-of (file)
  "The config home FILE, a session of pi's, lies under."
  (expand-file-name "../../.." file))

(defun aob-transcript-pi-move (file where then)
  "Move session FILE out of pi's sessions tree into its WHERE folder.
THEN is called after.
Archiving and discarding are the same move, and pi is never asked to
delete: its delete finds a session by walking the tree, and would take
whichever copy it met first."
  (let* ((slug (file-name-nondirectory (directory-file-name (file-name-directory file))))
         (dir (aob-transcript-pi--folder (aob-transcript-pi--home-of file) slug where))
         (to (expand-file-name (file-name-nondirectory file) dir)))
    (make-directory dir t)
    (rename-file file to t)
    (aob-transcript-forget)
    (when then (funcall then))
    to))

(defun aob-transcript-pi-restore (entry)
  "Move ENTRY\='s session back into pi's sessions tree if it was put away.
Its adapter reloads a session from the path it recorded for it, and a
missing one opens as a new, empty session."
  (when-let* ((file (aob-transcript-file entry))
              (home (aob-transcript-pi--home-of file))
              (slug (file-name-nondirectory (directory-file-name (file-name-directory file))))
              ((not (string-prefix-p (expand-file-name "sessions/" home) file))))
    (let ((to (expand-file-name (file-name-nondirectory file)
                                (aob-transcript-pi--folder home slug nil))))
      (make-directory (file-name-directory to) t)
      (rename-file file to)
      (aob-transcript-forget)
      to)))

(defvar aob-transcript-pi--heads (make-hash-table :test 'equal)
  "Session file to (MTIME ID . CWD), read from its header line.")

(defun aob-transcript-pi-forget ()
  (clrhash aob-transcript-pi--heads))

(defun aob-transcript-pi--head (file)
  "(FILE ID CWD) from the header line FILE opens with, or nil.
Read once per change of the file."
  (let* ((mtime (aob-transcript--mtime file))
         (cell (gethash file aob-transcript-pi--heads)))
    (unless (equal (car cell) mtime)
      (setq cell
            (puthash file
                     (cons mtime
                           (with-temp-buffer
                             (ignore-errors (insert-file-contents file nil 0 8192))
                             (goto-char (point-min))
                             (when-let* ((rec (ignore-errors
                                                (json-parse-string
                                                 (buffer-substring-no-properties
                                                  (point) (line-end-position))
                                                 :object-type 'alist :null-object nil
                                                 :false-object nil)))
                                         ((equal (alist-get 'type rec) "session"))
                                         (id (alist-get 'id rec))
                                         (cwd (alist-get 'cwd rec))
                                         ((and (stringp id) (stringp cwd))))
                               (cons id cwd))))
                     aob-transcript-pi--heads)))
    (when (cdr cell)
      (list file (cadr cell) (cddr cell)))))

(defun aob-transcript-pi--tree-p (rec)
  (and (equal (alist-get 'type rec) "message") (assq 'parentId rec)))

(defun aob-transcript-pi--chain (recs)
  "The records of RECS on the branch that ends at the last of them, oldest first."
  (let ((by-id (make-hash-table :test 'equal))
        leaf chain)
    (dolist (rec recs)
      (when-let* ((id (alist-get 'id rec))
                  ((stringp id))
                  ((assq 'parentId rec)))
        (puthash id rec by-id)
        (setq leaf id)))
    (while-let ((rec (and leaf (gethash leaf by-id))))
      (remhash leaf by-id)
      (push rec chain)
      (setq leaf (alist-get 'parentId rec)))
    chain))

(defun aob-transcript-pi--active (file recs)
  "RECS, the last records of FILE, narrowed to its last branch when FILE is pi's.
A window that stops short of the root of that branch is widened until it
reaches it, holds a cap's worth of it, or is the whole file."
  (if (not (seq-some #'aob-transcript-pi--tree-p recs))
      recs
    (let ((lines aob-event-cap)
          (chain (aob-transcript-pi--chain recs)))
      (while (and chain
                  (alist-get 'parentId (car chain))
                  (< (length chain) aob-event-cap)
                  (>= (length recs) lines)
                  (< lines (* 16 aob-event-cap)))
        (setq lines (* lines 4)
              recs (aob-transcript--tail-records file lines)
              chain (aob-transcript-pi--chain recs)))
      chain)))

(defconst aob-transcript-pi--note-re
  (concat "\\`\\[workspace: .*? · branch \\(?:detached HEAD\\|[^] \n]+\\)"
          "\\(?: · linked worktree of .*?\\)?"
          "\\(?: · other worktrees: .*?\\)?\\]")
  "The workspace note as `aob-acp--place-note' writes it, with nothing after it.")

(defun aob-transcript-pi--typed (text)
  "TEXT without what aob put in front of what was typed.
The adapter joins every block of a prompt into one string, so the
workspace note and the project maps arrive glued to the first message."
  (let ((text (string-trim-left text)))
    (when (string-match aob-transcript-pi--note-re text)
      (setq text (string-trim-left (substring text (match-end 0)))))
    (when (and (string-prefix-p "<project-maps>" text)
               (string-search "</project-maps>" text))
      (setq text (string-trim-left
                  (substring text (+ (string-search "</project-maps>" text)
                                     (length "</project-maps>"))))))
    text))

(defun aob-transcript-pi--turn (rec)
  "REC, a pi message record, as (WHO . TEXT) when it is something said, else nil."
  (condition-case nil
      (let* ((message (alist-get 'message rec))
             (who (alist-get 'role message)))
        (when-let* (((member who '("user" "assistant")))
                    (text (aob-transcript--text (alist-get 'content message)))
                    (text (string-trim text))
                    (text (if (equal who "user") (aob-transcript-pi--typed text) text))
                    ((not (string-empty-p text)))
                    ((not (and (equal who "user") (aob-transcript--harness-text-p text)))))
          (cons who text)))
    (wrong-type-argument nil)))

(defun aob-transcript-pi--tools (rec)
  "The tools the pi message REC calls, each as the line a live session shows."
  (condition-case nil
      (let ((message (alist-get 'message rec)))
        (when (and (equal (alist-get 'role message) "assistant")
                   (vectorp (alist-get 'content message)))
          (delq nil
                (seq-map (lambda (part)
                           (when (equal (alist-get 'type part) "toolCall")
                             (aob-transcript--tool-title (alist-get 'name part)
                                                         (alist-get 'arguments part))))
                         (alist-get 'content message)))))
    (wrong-type-argument nil)))

(provide 'aob-transcript-pi)
;;; aob-transcript-pi.el ends here
