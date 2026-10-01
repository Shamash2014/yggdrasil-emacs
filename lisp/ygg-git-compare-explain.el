;;; ygg-git-compare-explain.el --- ask about a compare without leaving it -*- lexical-binding: t; -*-

;;; Commentary:
;; From inside a compare: open side B's own file where a diff line puts
;; it, so the language server answers about the code itself; or start an
;; agent session on the compared range to explain the change and draw the
;; calls it touches, its trace shown in the compare's right pane.  The
;; session is an ordinary one, listed, resumable and killed like any
;; other; leaving the compare leaves it running.

;;; Code:

(require 'subr-x)
(require 'magit)
(require 'ygg-git-compare)

(defvar aob-acp-start-dir)
(defvar aob-acp-show-trace)
(defvar aob-acp-default-agent)
(declare-function aob-acp-spawn "aob-acp" (agent &optional intent atts name tree))
(declare-function aob-acp-preset "aob-acp" (name))
(declare-function aob-trace-buffer "aob-trace" (s))

(defcustom ygg-git-compare-explain-preset nil
  "The preset a session asked about a compare runs under.
Nil is `aob-acp-default-agent'.  A preset with a worktree of its own is
refused, since the agent is to read the sides where they are."
  :type '(choice (const :tag "The default agent" nil) string)
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-explain-instructions
  "Explain this change to a reviewer who has not seen it, in this order:
1. What and why: what the change does and the problem it answers, from \
the commit messages, the docs the repository keeps and the code.
2. Requirements: what has to hold for the change to be right, as a short list.
3. Design: the shape of the solution as one diagram in a fenced mermaid \
block; then the calls the change touches as a second one, its nodes the \
functions it adds, changes or removes and those that call them or that \
they call, from the diff and the code at B, the changed ones marked apart \
with classDef; then a few sentences on both.
4. Implementation in reading order: the files and functions to read, in \
the order that makes them make sense, each with a path:line link.
5. Read-back: every claim above you could not check against the code, \
the history or the docs, with what you tried."
  "What a session explaining a compare is asked for, after the range it is given."
  :type 'string
  :group 'ygg-git-compare)

;;; B's own file

(defun ygg-git-compare-explain--b-dir (list)
  "The checkout side B of LIST stands in, or nil when B is no worktree."
  (pcase (buffer-local-value 'ygg-git-compare--b-spec list)
    (`(worktree . ,dir) (file-name-as-directory dir))))

(defun ygg-git-compare-explain-b-position ()
  "Where the diff line at point is in side B's checkout, as (FILE LINE COLUMN).
A removed line answers the line that took its place.  A user error says
so when B is a branch, commit or pull request, which no checkout holds."
  (let* ((list (ygg-git-compare--list))
         (dir (or (ygg-git-compare-explain--b-dir list)
                  (user-error "B is %s, not a worktree: no checkout holds its files"
                              (plist-get (buffer-local-value 'ygg-git-compare--b list)
                                         :label))))
         (file (or (magit-file-at-point) (user-error "No file at point")))
         (section (magit-current-section))
         (in-hunk (magit-section-match 'hunk section))
         (on-line (and in-hunk (>= (point) (oref section content)))))
    (list (expand-file-name file dir)
          (or (and in-hunk (magit-diff-hunk-line section nil)) 1)
          (if (and on-line (not (eq (char-after (line-beginning-position)) ?-)))
              (max 0 (1- (current-column)))
            0))))

(defun ygg-git-compare-visit-b ()
  "Open side B's own file at the diff line at point, in the right pane.
The language server attaches to it as to any file, so hover, definitions,
references and the call hierarchy answer about B as it stands.  Moving in
the diff of every file brings the file's diff back."
  (interactive)
  (pcase-let* ((list (ygg-git-compare--list))
               (`(,file ,line ,column) (ygg-git-compare-explain-b-position))
               (window (buffer-local-value 'ygg-git-compare--file-window list))
               (buffer (if (file-exists-p file)
                           (find-file-noselect file)
                         (user-error "%s is not in B's checkout" (file-name-nondirectory file)))))
    (with-current-buffer list (setq ygg-git-compare--shown nil))
    (if (window-live-p window)
        (progn (select-window window) (switch-to-buffer buffer nil t))
      (pop-to-buffer buffer))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

;;; Sessions on the range

(defun ygg-git-compare-explain--short (side)
  (let ((label (plist-get side :label)))
    (if (string-match "\\[\\(.+\\)\\]\\'" label) (match-string 1 label) label)))

(defun ygg-git-compare-explain--dir (list)
  "Where a session on LIST's range starts: B's checkout, else the compare's."
  (or (ygg-git-compare-explain--b-dir list)
      (buffer-local-value 'default-directory list)))

(defun ygg-git-compare-explain-context (list)
  "The range LIST compares, written out for an agent to read for itself."
  (with-current-buffer list
    (let* ((a ygg-git-compare--a)
           (b ygg-git-compare--b)
           (plan ygg-git-compare--plan)
           (work (plist-get plan :work))
           (side (lambda (name s)
                   (format "%s: %s, commit %s%s\n" name (plist-get s :label)
                           (plist-get s :log)
                           (cond ((eq s work)
                                  ", with what its worktree has not committed, \
untracked files included")
                                 ((plist-get s :uncommitted)
                                  ", what it has not committed left out")
                                 (t ""))))))
      (concat
       (format "Repository: %s\n"
               (ygg-git-compare-agent-path (ygg-git-compare-explain--dir list)))
       (funcall side "A" a)
       (funcall side "B" b)
       (if work
           (format "The change, run in %s: git diff%s %s for tracked files, staged \
or not, and git ls-files --others --exclude-standard for the untracked files it %s\n"
                   (ygg-git-compare-agent-path (plist-get work :dir))
                   (if (plist-get plan :reverse) " -R" "")
                   (plist-get plan :range) (if (plist-get plan :reverse) "drops" "adds"))
         (format "The change: git diff %s%s%s\n" (plist-get a :diff) ygg-git-compare--dots
                 (plist-get b :diff)))
       (format "Its commits: git log --left-right %s...%s (> marks B's)\n"
               (plist-get a :log) (plist-get b :log))
       "Read these yourself with git and the repository's docs.  Read only: \
change no file, commit, branch or ref.\n"))))

(defun ygg-git-compare-explain--preset ()
  (let ((preset (or ygg-git-compare-explain-preset aob-acp-default-agent)))
    (when (plist-get (aob-acp-preset preset) :worktree)
      (user-error "Preset %s makes a worktree of its own; pick one that reads in place"
                  preset))
    preset))

(defun ygg-git-compare-explain--ask (verb instructions)
  "Start a session asked INSTRUCTIONS about this compare's range, named VERB A…B.
Its trace takes the right pane; the session outlives the compare."
  (require 'aob)
  (require 'aob-acp)
  (require 'aob-trace nil t)
  (let* ((list (ygg-git-compare--list))
         (aob-acp-start-dir (ygg-git-compare-explain--dir list))
         (aob-acp-show-trace nil)
         (name (format "%s %s…%s" verb
                       (ygg-git-compare-explain--short
                        (buffer-local-value 'ygg-git-compare--a list))
                       (ygg-git-compare-explain--short
                        (buffer-local-value 'ygg-git-compare--b list))))
         (prompt (concat (ygg-git-compare-explain-context list) "\n" instructions))
         (session (or (aob-acp-spawn (ygg-git-compare-explain--preset) prompt nil name)
                      (user-error "Could not start %s" name)))
         (window (buffer-local-value 'ygg-git-compare--file-window list)))
    (when (fboundp 'aob-trace-buffer)
      (with-current-buffer list (setq ygg-git-compare--shown nil))
      (if (window-live-p window)
          (progn (select-window window)
                 (switch-to-buffer (aob-trace-buffer session) nil t))
        (pop-to-buffer (aob-trace-buffer session))))
    session))

;;;###autoload
(defun ygg-git-compare-explain ()
  "Ask a new agent session to explain the compared change, trace on the right.
What and why, requirements, the design and the calls it touches as
diagrams, the implementation in reading order with file:line links, then
what it could not check; see `ygg-git-compare-explain-instructions'."
  (interactive)
  (ygg-git-compare-explain--ask "explain" ygg-git-compare-explain-instructions))

(provide 'ygg-git-compare-explain)
;;; ygg-git-compare-explain.el ends here
