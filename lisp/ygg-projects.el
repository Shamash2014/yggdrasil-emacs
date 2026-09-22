;;; ygg-projects.el --- the projects sidebar, drawn with vui -*- lexical-binding: t; -*-

;;; Commentary:
;; One side window listing the projects this config knows, the selected one
;; opened to show what it is running now: agents, their subagents, the
;; commands its justfile offers, its terminals, its worktrees.
;;
;; The tree is a vui component, so a refresh is a props update that vui
;; reconciles, rather than the buffer being erased and rebuilt under the
;; cursor.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'hl-line)
(require 'project)
(require 'ygg-ui)
(require 'ygg-project-commands nil t)
(require 'vui nil t)

(declare-function ygg-project-roots "ygg-project-scan" (&optional refresh))
(declare-function aob-sessions "aob")
(declare-function aob-session-project "aob" (s))
(declare-function aob-session-state "aob" (s))
(declare-function ygg-aob-session-subagents "layer-aob" (s))
(declare-function aob-acp-resumable-entries "aob-acp" ())
(declare-function aob-acp-resume-entry "aob-acp" (e &optional pref))
(declare-function aob-acp-archive-entry "aob-acp" (e))
(declare-function aob-acp-forget-entry "aob-acp" (e))
(declare-function aob-acp-delete-session "aob-acp" (s))
(declare-function aob-transcript-view "aob-transcript" (entry))
(declare-function ygg-task--locate-justfile "layer-tasks" (start))
(declare-function ygg-task--justfile-recipes-text "layer-tasks" (file))
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function ygg-space-open "yggdrasil-spacetree" (uri))
(declare-function vui-component "vui")
(declare-function vui-mount "vui")
(declare-function vui-update-props "vui")
(declare-function vui-vstack "vui")
(declare-function vui-region "vui")
(declare-function vui-text "vui")
(declare-function yggdrasil-local-mode "yggdrasil-core")

(defgroup ygg-projects nil
  "The projects sidebar."
  :group 'tools :prefix "ygg-projects-")

(defcustom ygg-projects-width 34
  "Columns the projects sidebar takes."
  :type 'natnum :group 'ygg-projects)

(defcustom ygg-projects-limit 8
  "How many projects the sidebar lists: the busy ones, then the rest."
  :type 'natnum :group 'ygg-projects)

(defcustom ygg-projects-buffer-name "*projects*"
  "Name of the sidebar buffer."
  :type 'string :group 'ygg-projects)

(defcustom ygg-projects-gutter 10
  "Pixels of dark set between the sidebar and the window beside it."
  :type 'natnum :group 'ygg-projects)

(defface ygg-projects-base
  '((t :height 1.05))
  "Face sizing the whole sidebar and lifting it off the ground."
  :group 'ygg-projects)

(defface ygg-projects-name '((t :weight bold))
  "Face for a project name." :group 'ygg-projects)

(defface ygg-projects-path '((t :inherit shadow))
  "Face for a project path." :group 'ygg-projects)

(defface ygg-projects-count '((t :inherit shadow))
  "Face for a row's count." :group 'ygg-projects)

(defface ygg-projects-live '((t :inherit success))
  "Face for the dot of a project with something running." :group 'ygg-projects)

(defface ygg-projects-idle '((t :inherit shadow))
  "Face for the dot of a quiet project." :group 'ygg-projects)

(defface ygg-projects-card '((t :extend t))
  "The open project's extent.
Carries no fill: a surface is told apart by what it holds and by the
quiet around it, and the one filled thing on screen should be the line
you are on." :group 'ygg-projects)

(defface ygg-projects-current
  '((((background dark)) :background "#1c1c1c" :extend t)
    (t :background "#e4e4e4" :extend t))
  "Face behind the line point is on: the only fill in the sidebar.
Untinted, and a full step off the ground — a grey nudged by less than
#0a reads as a rendering artefact rather than a choice."
  :group 'ygg-projects)

(defface ygg-projects-label '((t :inherit default))
  "Face for a row's name." :group 'ygg-projects)

(defface ygg-projects-entry '((t :inherit shadow :slant italic))
  "Face for one thing a row stands for." :group 'ygg-projects)

(defface ygg-projects-accent '((t :inherit success))
  "Face of the bar marking the open project." :group 'ygg-projects)

(defface ygg-projects-gutter
  '((((background dark)) :background "#000000")
    (t :background "#dcdee3"))
  "Face of the dark run between the sidebar and its neighbour."
  :group 'ygg-projects)

(defvar ygg-projects--open nil
  "Root of the project currently expanded, or nil.")

(defvar ygg-projects--here nil
  "Root of the project the sidebar was opened from.
Held because `project-current' answers about whichever buffer a
refresh runs in, and a refresh runs in the sidebar's own.")

(defvar-local ygg-projects--instance nil
  "The mounted vui root of this sidebar.")

(defvar ygg-projects--open-row nil
  "Cons of (ROOT . KIND) whose entries are listed, or nil.")

;;; What each project is running

(defun ygg-projects--sessions (root)
  (when (fboundp 'aob-sessions)
    (seq-filter (lambda (s)
                  (when-let* ((p (aob-session-project s)))
                    (equal (file-name-as-directory (expand-file-name p)) root)))
                (aob-sessions))))

(defun ygg-projects--past (root)
  "Conversations in ROOT that ended but can be picked up again."
  (when (fboundp 'aob-acp-resumable-entries)
    (seq-filter (lambda (e)
                  (equal root (file-name-as-directory
                               (expand-file-name (or (plist-get e :project)
                                                     (plist-get e :dir) "/")))))
                (ignore-errors (aob-acp-resumable-entries)))))

(defun ygg-projects--agents (root)
  "What ROOT has going, and everything it could go back to.
A subagent counts as work in flight; a conversation that ended still
counts as one the project has, since it can be resumed."
  (let ((all (ygg-projects--sessions root))
        (subs (ygg-projects--subagents root))
        (past (length (ygg-projects--past root))))
    (cons (+ (seq-count (lambda (s) (not (memq (aob-session-state s) '(dead done))))
                        all)
             (car subs))
          (+ (length all) (cdr subs) past))))

(defun ygg-projects--roots ()
  "Every project root worth listing, in an order that does not move.
Neither opening a project nor an agent starting in one reorders the
list: a row that changes place under the hand that reached for it is
worse than a row in an inconvenient place.  The open project and the
one the sidebar was opened from are kept whatever the cap, since a
list that can drop the thing it is showing is a list that shows
nothing."
  (let* ((scanned (seq-uniq (mapcar #'file-name-as-directory
                                    (delq nil (ignore-errors (ygg-project-roots))))
                            #'equal))
         (pinned (delq nil (list ygg-projects--here ygg-projects--open)))
         (all (append scanned (seq-remove (lambda (r) (member r scanned)) pinned)))
         (picked (seq-take all (max 1 ygg-projects-limit))))
    (dolist (r pinned)
      (unless (member r picked) (setq picked (append picked (list r)))))
    picked))

(defun ygg-projects--subagents (root)
  (if (not (fboundp 'ygg-aob-session-subagents))
      (cons 0 0)
    (let ((n (apply #'+ (mapcar (lambda (s) (length (ygg-aob-session-subagents s)))
                                (ygg-projects--sessions root)))))
      (cons n n))))

(defun ygg-projects--commands (root)
  "Commands running in ROOT now, out of what its build systems offer.
The offer is read from a cache that never blocks; a scan is asked for
in the background and redraws this when it settles."
  (let ((total (length (and (fboundp 'ygg-project-commands)
                            (ygg-project-commands root))))
        (running (seq-count
                  (lambda (b)
                    (and (process-live-p (get-buffer-process b))
                         (with-current-buffer b
                           (derived-mode-p 'compilation-mode 'comint-mode))
                         (string-prefix-p
                          root (expand-file-name
                                (buffer-local-value 'default-directory b)))))
                  (buffer-list))))
    (when (fboundp 'ygg-project-commands-refresh)
      (ygg-project-commands-refresh
       root (lambda (_root) (ygg-projects-refresh))))
    (cons running (max total running))))

(defun ygg-projects--terminals (root)
  (let ((n (seq-count
            (lambda (b)
              (and (provided-mode-derived-p
                    (buffer-local-value 'major-mode b) 'ghostel-mode)
                   (string-prefix-p root (expand-file-name
                                          (buffer-local-value
                                           'default-directory b)))))
            (buffer-list))))
    (cons n n)))

(defun ygg-projects--folders (root)
  "Every folder ROOT covers, the checkout itself first.
The root is where its agents already stand; the rest is what they were
additionally given to see."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (delete-dups
     (append (list root)
             (mapcar #'file-name-as-directory
                     (ignore-errors (ygg-project-folders root)))
             ;; a monorepo's members are folders of the project whether or
             ;; not anybody listed them by hand
             (mapcar #'file-name-as-directory
                     (and (fboundp 'ygg-project-workspaces)
                          (ignore-errors (ygg-project-workspaces root))))))))

(defun ygg-projects--worktrees (root)
  "Worktrees ROOT has besides the checkout itself."
  (let ((n (or (ignore-errors
                 (with-temp-buffer
                   (let ((default-directory root))
                     (when (zerop (call-process "git" nil t nil
                                                "worktree" "list" "--porcelain"))
                       (goto-char (point-min))
                       (how-many "^worktree ")))))
               0)))
    (cons (max 0 (1- n)) (max 0 (1- n)))))


;;; What a row holds, when you open it

(defun ygg-projects--entries (root kind)
  "The things ROOT's KIND row stands for: (LABEL . PAYLOAD) each."
  (pcase kind
    ('agents (let ((all (ygg-projects--sessions root))
                   out)
               ;; no result form here: `nreverse' would rewire the list
               ;; and leave OUT pointing at what is now its last cell,
               ;; which is one session however many are running
               (dolist (s (seq-remove (lambda (x)
                                        (and (fboundp 'aob-subagent-p)
                                             (aob-subagent-p x)))
                                      all))
                 (push (cons (format "%s · %s" (aob-session-name s)
                                     (aob-session-state s))
                             s)
                       out)
                 ;; what it sent, under it, marked rather than indented: a
                 ;; row this narrow has no columns to spare.  A spawned one
                 ;; is a session of its own; a reported one is a plist the
                 ;; agent mentioned and nothing can be done with
                 (dolist (kid (and (fboundp 'aob-subagent-children)
                                   (aob-subagent-children s)))
                   (push (cons (format "↳ %s · %s" (aob-session-name kid)
                                       (aob-session-state kid))
                               kid)
                         out))
                 (dolist (sub (and (fboundp 'ygg-aob-session-subagents)
                                   (ygg-aob-session-subagents s)))
                   (push (cons (format "↳ %s" (or (plist-get sub :title)
                                                  (plist-get sub :name)
                                                  "subagent"))
                               (cons s sub))
                         out)))
               ;; ended, but the conversation is still there to pick up
               (dolist (e (ygg-projects--past root))
                 (push (cons (format "%s · ended" (or (plist-get e :name)
                                                      (plist-get e :agent)
                                                      "session"))
                             e)
                       out))
               (setq out (nreverse out))))
    ('commands (mapcar (lambda (c)
                         (cons (format "%s  %s" (plist-get c :name)
                                       (propertize (format "%s" (plist-get c :source))
                                                   'font-lock-face 'ygg-projects-count))
                               c))
                       (and (fboundp 'ygg-project-commands)
                            (ygg-project-commands root))))
    ('terminals (mapcar (lambda (b) (cons (buffer-name b) b))
                        (seq-filter
                         (lambda (b)
                           (and (provided-mode-derived-p
                                 (buffer-local-value 'major-mode b) 'ghostel-mode)
                                (string-prefix-p
                                 root (expand-file-name
                                       (buffer-local-value 'default-directory b)))))
                         (buffer-list))))
    ('folders (mapcar (lambda (d) (cons (abbreviate-file-name
                                        (directory-file-name d))
                                       d))
                      (ygg-projects--folders root)))
    ('worktrees (ignore-errors
                  (with-temp-buffer
                    (let ((default-directory root))
                      (when (zerop (call-process "git" nil t nil
                                                 "worktree" "list" "--porcelain"))
                        (goto-char (point-min))
                        (let (out)
                          (while (re-search-forward "^worktree \\(.*\\)$" nil t)
                            (let ((dir (match-string 1)))
                              (unless (equal (file-name-as-directory dir) root)
                                (push (cons (file-name-nondirectory dir) dir) out))))
                          (nreverse out)))))))
    (_ nil)))

(defun ygg-projects--entry-text (label root kind payload)
  (propertize (concat "        "
                      (propertize "·" 'font-lock-face 'ygg-projects-idle)
                      "  "
                      (propertize label 'font-lock-face 'ygg-projects-entry)
                      (ygg-projects--right ""))
              'ygg-project root 'ygg-row kind 'ygg-entry payload))

(defun ygg-projects--entry-nodes (root kind)
  (mapcar (lambda (cell)
            (vui-text (ygg-projects--entry-text (car cell) root kind (cdr cell))))
          (or (ygg-projects--entries root kind)
              (list (cons "— none —" nil)))))

;;; Drawing

(defun ygg-projects--icon (name fallback)
  "NAME as a nerd-icon, or FALLBACK.
No :face is passed: nerd-icons carries its own font family there, and
overriding it hands the glyph to a font that has no such character."
  (or (and (or (featurep 'nerd-icons) (require 'nerd-icons nil t))
           (ignore-errors (nerd-icons-mdicon name)))
      fallback))

(defun ygg-projects--width ()
  (- ygg-projects-width 4))

(defun ygg-projects--right (text)
  "TEXT pushed to the right edge, with the line padded out behind it.
The target is the window edge, not a column count: a nerd-icon glyph is
drawn wider than the one column it measures, so a numeric `align-to'
leaves icon rows long and iconless rows short, and the card behind them
ends in a ragged edge.  Two columns are held back so the last glyph
cannot spill past the text area and mark every line truncated."
  (concat (propertize " " 'display
                      `(space :align-to (- right ,(+ 2 (string-width text)))))
          text
          (propertize " " 'display '(space :align-to right))))

(defun ygg-projects--pad ()
  "A blank line as wide as a card."
  (ygg-projects--right ""))

(defun ygg-projects--counts (live total)
  (cond ((and (zerop live) (zerop total)) "0")
        ((= live total) (format "%d" total))
        (t (format "%d/%d" live total))))

(defun ygg-projects--bar ()
  (propertize "▌" 'font-lock-face 'ygg-projects-accent))

(defun ygg-projects--row-text (icon label count root kind)
  "One row of an open project, carrying what it stands for."
  (propertize (concat "    " icon "   "
                      (propertize label 'font-lock-face 'ygg-projects-label)
                      (ygg-projects--right
                       (propertize count 'font-lock-face 'ygg-projects-count)))
              'ygg-project root 'ygg-row kind))

(defun ygg-projects--head-text (root)
  "ROOT's own line."
  (let* ((raw (file-name-nondirectory (directory-file-name root)))
         (agents (ygg-projects--agents root))
         (dot (propertize "●" 'font-lock-face
                          (if (> (car agents) 0)
                              'ygg-projects-live 'ygg-projects-idle)))
         ;; the basename is the name already: what is worth saying is
         ;; which folder it sits in
         (full (abbreviate-file-name
                (directory-file-name
                 (file-name-directory (directory-file-name root)))))
         ;; the name is never sacrificed for the path: what is left after
         ;; it decides whether the folder is spelled out, shortened to its
         ;; own basename, or left off
         (avail (- (ygg-projects--width) 7))
         (name (if (<= (string-width raw) avail) raw
                 (truncate-string-to-width raw avail nil nil t)))
         (room (- avail (string-width name) 2))
         (path (cond ((<= (string-width full) room) full)
                     ((let ((short (concat "…/" (file-name-nondirectory full))))
                        (and (<= (string-width short) room) short)))
                     (t ""))))
    (propertize (concat " " dot "  "
                        (propertize name 'font-lock-face 'ygg-projects-name)
                        (ygg-projects--right
                         (propertize path 'font-lock-face 'ygg-projects-path)))
                'ygg-project root 'ygg-row 'project)))

(defun ygg-projects--row-specs (root)
  "ROOT's rows as (KIND ICON LABEL COUNT)."
  (let ((agents (ygg-projects--agents root))
        (cmds (ygg-projects--commands root))
        (terms (ygg-projects--terminals root))
        (wts (ygg-projects--worktrees root)))
    (list (list 'agents (ygg-projects--icon "nf-md-account_outline" "A")
                "Sessions" (ygg-projects--counts (car agents) (cdr agents)))
          (list 'commands (ygg-projects--icon "nf-md-console" ">")
                "Commands" (ygg-projects--counts (car cmds) (cdr cmds)))
          (list 'terminals (ygg-projects--icon "nf-md-console_line" "T")
                "Terminals" (ygg-projects--counts (car terms) (cdr terms)))
          (list 'worktrees (ygg-projects--icon "nf-md-source_branch" "W")
                "Worktrees" (ygg-projects--counts (car wts) (cdr wts)))
          (let ((n (length (ygg-projects--folders root))))
            (list 'folders (ygg-projects--icon "nf-md-folder_multiple_outline" "F")
                  "Folders" (ygg-projects--counts n n))))))

(defun ygg-projects--rows (root)
  "ROOT's rows, the opened one followed by what it holds."
  (let ((open-root (car-safe ygg-projects--open-row))
        (open-kind (cdr-safe ygg-projects--open-row))
        (out nil))
    (pcase-dolist (`(,kind ,icon ,label ,count) (ygg-projects--row-specs root))
      (push (vui-text (ygg-projects--row-text icon label count root kind)) out)
      (when (and (equal root open-root) (eq kind open-kind))
        (dolist (node (ygg-projects--entry-nodes root kind)) (push node out))))
    (nreverse out)))

(with-eval-after-load 'vui
  (vui-defcomponent ygg-projects-card (root open)
    "One project: a line in the list, or an opened block when OPEN.
Nothing is drawn above the head line: a row that slid down as it
opened would take the hand that pressed TAB with it."
    :render
    (if (not open)
        (vui-text (ygg-projects--head-text root))
      (vui-region
       :face 'ygg-projects-card
       (apply #'vui-vstack
              (cons (vui-text (ygg-projects--head-text root))
                    (ygg-projects--rows root))))))

  (vui-defcomponent ygg-projects-view (roots open)
    "The projects as a list, the open one lifted out of it as a card."
    :render
    (apply #'vui-vstack
           (apply #'append
                  (mapcar (lambda (root)
                            (let ((card (vui-component 'ygg-projects-card
                                                       :key root :root root
                                                       :open (equal root open))))
                              (list card)))
                          roots)))))

(defun ygg-projects--row-at-point ()
  "What the line point is on stands for, as (ROOT . KIND)."
  (cons (get-text-property (line-beginning-position) 'ygg-project)
        (get-text-property (line-beginning-position) 'ygg-row)))

(defun ygg-projects--goto-row (cell)
  "Put point back on the row CELL names, if it is still drawn."
  (when (car cell)
    (let ((target nil))
      (save-excursion
        (goto-char (point-min))
        (while (and (not target) (not (eobp)))
          (if (and (equal (get-text-property (line-beginning-position) 'ygg-project)
                          (car cell))
                   (eq (get-text-property (line-beginning-position) 'ygg-row)
                       (cdr cell)))
              (setq target (line-beginning-position))
            (forward-line 1))))
      (when target (goto-char target)))))

(defun ygg-projects-refresh ()
  "Redraw the sidebar from what the projects are running now.
Point is kept on the row it was on rather than at the offset that row
used to occupy: a redraw that lands the cursor somewhere else reads as
the sidebar moving on its own."
  (interactive)
  (when-let* ((buf (get-buffer ygg-projects-buffer-name)))
    (with-current-buffer buf
      (when ygg-projects--instance
        (let* ((row (ygg-projects--row-at-point))
               (win (get-buffer-window buf 'visible))
               (start (and (window-live-p win) (window-start win))))
          (vui-update-props ygg-projects--instance
                            (list :roots (ygg-projects--roots)
                                  :open ygg-projects--open))
          (ygg-projects--goto-row row)
          (when (and (window-live-p win) start (<= start (point-max)))
            (set-window-start win start t)))))))

;;; Moving and acting

(defun ygg-projects--goto (dir &optional project-only)
  "Move DIR rows, stopping only on rows, or only on project heads."
  (let ((moved 0))
    (while (and (zerop moved)
                (zerop (forward-line dir))
                (not (eobp)))
      (when (and (get-text-property (line-beginning-position) 'ygg-project)
                 (or (not project-only)
                     (eq (get-text-property (line-beginning-position) 'ygg-row)
                         'project)))
        (setq moved 1)))
    (beginning-of-line)))

(declare-function ygg-project-commands "ygg-project-commands" (root))
(declare-function ygg-project-commands-refresh "ygg-project-commands" (root &optional cb))
(declare-function ygg-project-commands-run "ygg-project-commands" (command))
(declare-function ygg-project-workspaces "ygg-project-commands" (root))
(declare-function ygg-project-folders "ygg-project-scan" (root))
(declare-function ygg-project-add-folder "ygg-project-scan" (root dir))
(declare-function ygg-project-add "ygg-project-scan" (dir))
(declare-function ygg-project-remove "ygg-project-scan" (dir))

(defun ygg-projects-add (dir)
  "Remember DIR and show it in the sidebar."
  (interactive "DProject: ")
  (ygg-project-add dir)
  (ygg-projects-refresh))

(defun ygg-projects-remove (&optional root)
  "Stop offering ROOT, the project on this line by default."
  (interactive)
  (let ((root (or root
                  (get-text-property (line-beginning-position) 'ygg-project)
                  (completing-read "Forget project: "
                                   (mapcar #'abbreviate-file-name
                                           (ygg-projects--roots))
                                   nil t))))
    (when (yes-or-no-p (format "Stop offering %s? "
                               (abbreviate-file-name root)))
      (ygg-project-remove root)
      (when (equal (file-name-as-directory (expand-file-name root))
                   ygg-projects--open)
        (setq ygg-projects--open nil ygg-projects--open-row nil))
      (ygg-projects-refresh))))

(defun ygg-projects--entry-at-point ()
  (get-text-property (line-beginning-position) 'ygg-entry))

(defun ygg-projects-delete ()
  "Remove what this line stands for: a session for good, or a project.
A session that ran here is a conversation, so deleting it is deleting
that; a project row is only the list this sidebar keeps."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (cond
     ((and (consp entry) (plist-member entry :acp-id))
      (aob-acp-forget-entry entry)
      (ygg-projects-refresh))
     ((aob-session-p entry)
      (aob-acp-delete-session entry)
      (ygg-projects-refresh))
     (t (ygg-projects-remove)))))

(defun ygg-projects-resume ()
  "Start the conversation on this line up again."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (unless (and (consp entry) (plist-member entry :acp-id))
      (user-error "projects: no ended conversation on this line"))
    (when (y-or-n-p (format "Resume %s? " (or (plist-get entry :name)
                                              (plist-get entry :agent))))
      (aob-acp-resume-entry entry)
      (ygg-projects-refresh))))

(defun ygg-projects-archive ()
  "Put the conversation on this line away, keeping it resumable."
  (interactive)
  (let ((entry (ygg-projects--entry-at-point)))
    (cond
     ((and (consp entry) (plist-member entry :acp-id))
      (aob-acp-archive-entry entry)
      (ygg-projects-refresh))
     ((aob-session-p entry)
      (user-error "projects: that one is still running — end it first"))
     (t (user-error "projects: no conversation on this line")))))

(defun ygg-projects-next () (interactive) (ygg-projects--goto 1))
(defun ygg-projects-prev () (interactive) (ygg-projects--goto -1))
(defun ygg-projects-next-project () (interactive) (ygg-projects--goto 1 t))
(defun ygg-projects-prev-project () (interactive) (ygg-projects--goto -1 t))

(defun ygg-projects-toggle ()
  "Open what this line stands for: a project's rows, or a row's entries.
The line keeps its place on screen; what opens, opens below it."
  (interactive)
  (let* ((root (get-text-property (line-beginning-position) 'ygg-project))
         (kind (get-text-property (line-beginning-position) 'ygg-row))
         (win (get-buffer-window (current-buffer) 'visible))
         (above (and (window-live-p win)
                     (- (line-number-at-pos (point))
                        (line-number-at-pos (window-start win))))))
    (cond
     ((null root) nil)
     ((eq kind 'project)
      (setq ygg-projects--open (unless (equal root ygg-projects--open) root)
            ygg-projects--open-row nil))
     (t (setq ygg-projects--open-row
              (unless (equal ygg-projects--open-row (cons root kind))
                (cons root kind)))))
    (ygg-projects-refresh)
    ;; and when the list is long enough to scroll, hold the row against
    ;; the same screen line rather than letting the view slide
    (when (and (window-live-p win) above)
      (save-excursion
        (forward-line (- above))
        (set-window-start win (line-beginning-position) t)))))

(defun ygg-projects-open ()
  "Open the project on this line as its own space."
  (interactive)
  (let ((root (get-text-property (line-beginning-position) 'ygg-project)))
    (unless root (user-error "projects: no project on this line"))
    (setq ygg-projects--open root)
    (ygg-projects-refresh)
    (ygg-projects--keeping
      (if (fboundp 'ygg-space-open)
          (ygg-space-open (directory-file-name root))
        (dired root)))))

(defun ygg-projects-visit ()
  "Act on the row under point."
  (interactive)
  (let* ((root (get-text-property (line-beginning-position) 'ygg-project))
         (row (get-text-property (line-beginning-position) 'ygg-row))
         (entry (get-text-property (line-beginning-position) 'ygg-entry)))
    (unless root (user-error "projects: nothing on this line"))
    (ygg-projects--keeping
    (if entry
        (pcase row
          ;; a session is a struct, a delegation the pair it came from
          ('agents
           (cond
            ;; a persisted conversation is a plist; a live one a struct,
            ;; and a delegation the pair it came from
            ;; reading what was said costs nothing; resuming starts an
            ;; agent, so that is a different key
            ((and (consp entry) (plist-member entry :acp-id))
             (aob-transcript-view entry))
            ((consp entry) (aob-subagents (car entry)))
            (t (aob-trace entry))))
          ('commands (if (fboundp 'ygg-project-commands-run)
                         (ygg-project-commands-run entry)
                       (let ((default-directory root))
                         (compile (format "just %s" entry)))))
          ('folders (dired entry))
          ('terminals (pop-to-buffer entry))
          ('worktrees (if (fboundp 'ygg-space-open)
                          (ygg-space-open entry)
                        (dired entry))))
      (pcase row
      ('project (ygg-projects-open))
      ('agents (if (fboundp 'ygg-aob-pick) (ygg-aob-pick)
                 (user-error "projects: no agent picker")))
      ('commands (let ((default-directory root))
                   (if (fboundp 'ygg-task-run) (call-interactively #'ygg-task-run)
                     (user-error "projects: no task runner"))))
      ('folders (call-interactively #'ygg-project-add-folder))
      ('terminals (let ((default-directory root))
                    (if (fboundp 'ghostel) (call-interactively #'ghostel)
                      (user-error "projects: no terminal"))))
      ('worktrees (let ((default-directory root))
                    (cond ((fboundp 'ygg-wt-list) (call-interactively #'ygg-wt-list))
                          ((fboundp 'magit-worktree)
                           (call-interactively #'magit-worktree))
                          (t (user-error "projects: no worktree list"))))))))))

(defvar ygg-projects-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ygg-projects-visit)
    (define-key map (kbd "<return>") #'ygg-projects-visit)
    (define-key map (kbd "TAB") #'ygg-projects-toggle)
    ;; a GUI frame sends <tab>, and vui-mode binds it to vui-forward:
    ;; without this the disclosure key never reaches us
    (define-key map (kbd "<tab>") #'ygg-projects-toggle)
    (define-key map (kbd "<backtab>") #'ygg-projects-toggle)
    (define-key map (kbd "S-<tab>") #'ygg-projects-toggle)
    (define-key map "j" #'ygg-projects-next)
    (define-key map "k" #'ygg-projects-prev)
    (define-key map "J" #'ygg-projects-next-project)
    (define-key map "K" #'ygg-projects-prev-project)
    (define-key map "n" #'ygg-projects-next)
    (define-key map "p" #'ygg-projects-prev)
    (define-key map "g" #'ygg-projects-refresh)
    (define-key map "+" #'project-switch-project)
    (define-key map "A" #'ygg-projects-add)
    (define-key map "D" #'ygg-projects-delete)
    (define-key map "-" #'ygg-projects-archive)
    (define-key map "R" #'ygg-projects-resume)
    (define-key map "q" #'quit-window)
    map)
  "The sidebar's own verbs, ahead of yggdrasil's normal state.")

(defvar-local ygg-projects--modal nil)
(defvar ygg-projects--emulation-alist
  (list (cons 'ygg-projects--modal ygg-projects-map)))
(add-to-list 'emulation-mode-map-alists 'ygg-projects--emulation-alist)

(defun ygg-projects--drop-selection ()
  "Moving is not selecting: normal state leaves no region behind,
and no state gets to put its cursor back."
  (setq cursor-type nil)
  (when (and (bound-and-true-p ygg--normal-p) mark-active)
    (set-mark (point))
    (deactivate-mark)))

(defun ygg-projects--display (buf)
  "Put BUF in the sidebar's own window and return it."
  (display-buffer buf `((display-buffer-in-side-window)
                        (side . left) (slot . 0)
                        (window-width . ,ygg-projects-width)
                        (window-parameters . ((no-delete-other-windows . t))))))

(defmacro ygg-projects--keeping (&rest body)
  "Run BODY, and put the sidebar back if BODY took it away.
Opening a space restores a window configuration recorded without this
side window, so acting on a row would otherwise close the sidebar the
row was picked from."
  (declare (indent 0) (debug t))
  `(let ((had (and (get-buffer-window ygg-projects-buffer-name 'visible) t)))
     (prog1 (progn ,@body)
       (when-let* ((had)
                   (buf (get-buffer ygg-projects-buffer-name))
                   ((not (get-buffer-window buf 'visible)))
                   (win (ygg-projects--display buf)))
         (set-window-dedicated-p win t)
         (with-current-buffer buf (ygg-projects--trim-window))))))

(defun ygg-projects-keep-open (fn &rest args)
  "Call FN with ARGS and put the sidebar back if it went away.
Usable as :around advice on anything that rearranges windows."
  (ygg-projects--keeping (apply fn args)))

(defun ygg-projects--dismiss (win)
  "Take the sidebar off WIN.
A window that is its frame's last cannot be deleted, and a dedicated one
refuses to change buffer, so it is undedicated and handed back what it
showed before.  Its side parameters go too, or the next open reclaims
the whole frame instead of a side window."
  (when (window-live-p win)
    (if (not (eq win (frame-root-window win)))
        (delete-window win)
      (set-window-dedicated-p win nil)
      (dolist (p '(window-side window-slot no-delete-other-windows))
        (set-window-parameter win p nil))
      (switch-to-prev-buffer win)
      (when (and (window-live-p win)
                 (eq (window-buffer win) (get-buffer ygg-projects-buffer-name)))
        (set-window-buffer win (other-buffer (window-buffer win)))))))

(defun ygg-projects--trim-window ()
  "Nothing to the left of a card, a dark run to its right."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (set-window-fringes win 0 ygg-projects-gutter)))

(defun ygg-projects--setup (buf)
  "Make BUF read like a sidebar and answer to the modal layer."
  (with-current-buffer buf
    (setq truncate-lines t)
    (setq-local cursor-type nil)
    (setq-local left-margin-width 0)
    (setq-local line-spacing 0)
    (setq-local fringe-indicator-alist (cons '(truncation nil nil)
                                             fringe-indicator-alist))
    (setq ygg-projects--modal t)
    ;; a panel is not a document: it has no position, no encoding and no
    ;; name worth repeating under itself
    (setq-local mode-line-format nil)
    (buffer-face-set 'ygg-projects-base)
    ;; nothing paints a selection here.  remapping these to a flat colour
    ;; punches that colour through whatever card the line sits on, so they
    ;; are cut off from their global definitions and contribute nothing
    (dolist (f '(region ygg-secondary-selection ygg-secondary-cursor
                        ygg-sel ygg-fake-cursor))
      (when (facep f) (face-remap-set-base f nil)))
    (face-remap-add-relative 'fringe 'ygg-projects-gutter)
    (when (fboundp 'yggdrasil-local-mode) (yggdrasil-local-mode 1))
    ;; after the modal layer, which sets a cursor per state
    (setq-local cursor-type nil)
    (setq-local hl-line-face 'ygg-projects-current)
    (hl-line-mode 1)
    (add-hook 'post-command-hook #'ygg-projects--drop-selection 90 t)
    (add-hook 'window-configuration-change-hook #'ygg-projects--trim-window nil t)))

;;;###autoload
(defun ygg-projects-sidebar ()
  "Show the projects sidebar, or close it when it is already up."
  (interactive)
  (unless (or (featurep 'vui) (require 'vui nil t))
    (user-error "projects: vui is not installed"))
  (if-let* ((win (get-buffer-window ygg-projects-buffer-name 'visible)))
      ;; already up, on this frame or another: the key closes it rather
      ;; than opening a second one
      (ygg-projects--dismiss win)
    (when-let* ((pr (project-current nil)))
      (setq ygg-projects--here
            (file-name-as-directory (expand-file-name (project-root pr)))))
    (unless ygg-projects--open (setq ygg-projects--open ygg-projects--here))
    (let ((buf (get-buffer ygg-projects-buffer-name)))
      (unless (and buf (buffer-local-value 'ygg-projects--instance buf))
        ;; vui-mount ends in `switch-to-buffer', which would leave the
        ;; sidebar showing in the main window as well as its own
        (let ((inst (save-window-excursion
                      (vui-mount (vui-component 'ygg-projects-view
                                                :roots (ygg-projects--roots)
                                                :open ygg-projects--open)
                                 ygg-projects-buffer-name))))
          (setq buf (get-buffer ygg-projects-buffer-name))
          (with-current-buffer buf (setq ygg-projects--instance inst))))
      (ygg-projects--setup buf)
      (ygg-projects-refresh)
      (let ((win (ygg-projects--display buf)))
        ;; dedicated: whatever the sidebar opens goes to the main area,
        ;; never into the sidebar's own window
        (when (window-live-p win)
          (set-window-dedicated-p win t)
          ;; one sidebar: any other window that ended up on this buffer goes
          (dolist (other (get-buffer-window-list buf nil 'visible))
            (unless (eq other win) (ygg-projects--dismiss other)))
          (select-window win)))
      (ygg-projects--trim-window))))

(provide 'ygg-projects)
;;; ygg-projects.el ends here
