;;; layer-sessions.el --- resession.nvim-style sessions + tree workspaces -*- lexical-binding: t; -*-

;; Built-ins wrapped: kill-emacs-hook, subr-x (hash-table-keys), tab-bar
;; (via yggdrasil-spacetree — a space IS a native tab).
;; Third-party: easysession (MELPA) for named sessions.
;; Custom: SPC p zones map + SPC TAB, SPC q session bindings, per-space
;; buffer scoping for SPC b b, id-counter reseed on session restore.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-leader)
(require 'yggdrasil-spacetree)
(require 'subr-x)
(require 'seq)

(declare-function easysession-save "easysession")
(declare-function easysession-switch-to "easysession")
(declare-function easysession-delete "easysession")
(declare-function easysession-get-session-name "easysession")
(declare-function easysession-set-current-session-name "easysession")
(declare-function easysession-add-save-handler "easysession")
(declare-function easysession-add-load-handler "easysession")
(defvar easysession-save-interval)
(defvar easysession-after-load-hook)

;;; Sessions (resession.nvim feel)

(declare-function easysession-save-mode "easysession")

(when (fboundp 'elpaca)
  (elpaca easysession
    (setq easysession-save-interval (* 5 60))
    (easysession-save-mode 1)
    (add-hook 'kill-emacs-hook #'ygg-session--save-on-exit)))

(defun ygg-session--save-on-exit ()
  "Autosave on exit, resession.nvim style: the active session when one
exists (save-mode also covers this), else this project's session."
  (cond
   ((not (fboundp 'easysession-get-session-name)))
   ((easysession-get-session-name) (easysession-save))
   ((project-current) (ygg-session-save-project))))

(defun ygg-session-save ()
  "Save the current session under its existing name (quick save).
`easysession-save' alone never marks a session current — only load
and switch-to do — so this prompts once on first use and remembers
the name itself; later calls save quietly under it."
  (interactive)
  (require 'easysession) ; get-session-name has no autoload cookie
  (let ((name (or (easysession-get-session-name)
                  (read-string "Save session as: "))))
    (when (string-empty-p name)
      (user-error "Session name must not be empty"))
    (easysession-save name)
    (easysession-set-current-session-name name)
    (message "[ygg-session] Saved session: %s" name)))

(defun ygg-session-save-as ()
  "Save the current session under a newly prompted name (save-as).
Always prompts, even when a session is already active, and that name
becomes the new active session."
  (interactive)
  (require 'easysession)
  (let ((name (read-string "Save session as: " (easysession-get-session-name))))
    (when (string-empty-p name)
      (user-error "Session name must not be empty"))
    (easysession-save name)
    (easysession-set-current-session-name name)
    (message "[ygg-session] Saved session: %s" name)))

;; q l belongs to the loclist (nvim parity); sessions load via SPC p m/r
(yggdrasil-define-keys 'ygg-leader-quit-map
  "s" #'ygg-session-save :label "save session"
  "S" #'ygg-session-save-as :label "save session as"
  "d" #'easysession-delete :label "delete session")

;;; Project-keyed sessions (nvim sessions.lua feel: the cwd IS the name)

(declare-function easysession-get-session-file-path "easysession")
(declare-function project-root "project")
(declare-function project-current "project")

(defun ygg-session--project-name ()
  "Derive a session name from the project root, nvim-style path encoding."
  (let ((root (or (when-let* ((proj (project-current)))
                    (project-root proj))
                  default-directory)))
    (string-replace "/" "%" (abbreviate-file-name (directory-file-name root)))))

(defun ygg-session-save-project ()
  "Save the session under a name derived from the current project."
  (interactive)
  (require 'easysession)
  (let ((name (ygg-session--project-name)))
    (easysession-save name)
    (easysession-set-current-session-name name)
    (message "[ygg-session] Saved project session: %s" name)))

(defvar ygg-session--deferred-vc nil
  "Buffers restored with version control put off, refreshed on display.")

(defun ygg-session--vc-on-display (frame)
  "Give a restored buffer its version-control state the first time it
is shown, so a session of a hundred files pays for the ones you look at."
  (dolist (window (window-list frame 'no-minibuf))
    (let ((buffer (window-buffer window)))
      (when (memq buffer ygg-session--deferred-vc)
        (setq ygg-session--deferred-vc (delq buffer ygg-session--deferred-vc))
        (with-current-buffer buffer
          (when buffer-file-name
            (ignore-errors (vc-refresh-state))
            (when (fboundp 'magit-auto-revert-mode-enable-in-buffer)
              (ignore-errors (magit-auto-revert-mode-enable-in-buffer)))))))))

(defun ygg-session--load-fast (orig &rest args)
  "Around easysession's load: restore the buffers without asking git
about each one, and ask on first display instead.
Version control and magit's auto-revert cost most of what a restored
buffer costs, a tenth of a second each, and a session is many buffers."
  (let* ((before (buffer-list))
         (vc-handled-backends nil)
         (after-change-major-mode-hook
          (remq 'magit-auto-revert-mode-enable-in-buffer
                after-change-major-mode-hook))
         (find-file-hook (remq 'vc-refresh-state find-file-hook)))
    (prog1 (apply orig args)
      (dolist (buffer (buffer-list))
        (unless (memq buffer before)
          (when (buffer-local-value 'buffer-file-name buffer)
            (push buffer ygg-session--deferred-vc))))
      (add-hook 'window-buffer-change-functions #'ygg-session--vc-on-display)
      (ygg-session--vc-on-display (selected-frame)))))

(with-eval-after-load 'easysession
  (advice-add 'easysession-switch-to :around #'ygg-session--load-fast))

(defun ygg-session-load-project ()
  "Load this project's session if one was saved before."
  (interactive)
  (require 'easysession)
  (let ((name (ygg-session--project-name)))
    (if (file-exists-p (easysession-get-session-file-path name))
        (easysession-switch-to name)
      (user-error "No session saved for this project (SPC p w to create)"))))

;;; Annotated session picker: readable path + save time per candidate

(defvar easysession-directory)

(defun ygg-session--labels (sessions)
  "SESSIONS as (LABEL . NAME): the project's own name, its parent when two
projects share one, since a session file spells the path with percent
signs and nobody reads a path that way."
  (let* ((decode (lambda (name)
                   (directory-file-name (string-replace "%" "/" name))))
         (base (lambda (name) (file-name-nondirectory (funcall decode name))))
         (counts (make-hash-table :test #'equal)))
    (dolist (name sessions)
      (cl-incf (gethash (funcall base name) counts 0)))
    (mapcar (lambda (name)
              (let ((short (funcall base name)))
                (cons (if (> (gethash short counts) 1)
                          (concat (file-name-nondirectory
                                   (directory-file-name
                                    (file-name-directory (funcall decode name))))
                                  "/" short)
                        short)
                      name)))
            sessions)))

(defun ygg-session-pick ()
  "Pick a session by its project's name; the path and the save time beside it."
  (interactive)
  (require 'easysession)
  (let* ((sessions (and (file-directory-p easysession-directory)
                        (seq-remove (lambda (f) (string-prefix-p "." f))
                                    (directory-files easysession-directory))))
         (labels (ygg-session--labels sessions))
         (annotate
          (lambda (label)
            (let* ((name (cdr (assoc label labels)))
                   (file (expand-file-name name easysession-directory)))
              (format "  %-40s %s"
                      (propertize (string-replace "%" "/" name)
                                  'face 'font-lock-comment-face)
                      (propertize
                       (format-time-string
                        "%Y-%m-%d %H:%M"
                        (file-attribute-modification-time (file-attributes file)))
                       'face 'font-lock-comment-face)))))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function . ,annotate)
                                 (category . ygg-session))
                    (complete-with-action action (mapcar #'car labels) str pred)))))
    (unless sessions (user-error "No saved sessions yet (SPC p w / SPC q s)"))
    (easysession-switch-to
     (cdr (assoc (completing-read "Session: " table nil t) labels)))))

(autoload 'ygg-project-init "ygg-project" nil t)
(autoload 'ygg-project-report "ygg-project" nil t)

;;; Workspaces — native tab-bar spaces (yggdrasil-spacetree)

(ygg-spacetree-setup)

;; The tab-bar tabs (each space + its window-state) persist inside the
;; easysession frameset for free; on restore we only reseed the id counter
;; so freshly created spaces never collide with restored ones.
(declare-function ygg-agent-respawn-persisted "layer-agent")
(declare-function ygg-task-tree-session-select-space "ygg-task-tree")

(defun ygg--agents-respawn-after-load ()
  ;; let the frameset settle before the y-or-n-p respawn offer
  (when (fboundp 'ygg-agent-respawn-persisted)
    (run-at-time 1 nil #'ygg-agent-respawn-persisted)))

(with-eval-after-load 'easysession
  (add-hook 'easysession-after-load-hook #'ygg-space-reseed-ids)
  (add-hook 'easysession-after-load-hook #'ygg-task-tree-session-select-space t)
  (add-hook 'easysession-after-load-hook #'ygg-space-tree-adopt)
  (add-hook 'easysession-after-load-hook #'ygg--agents-respawn-after-load))

;;; Sidebar contributions — spacetree exposes one state and one detail
;; slot, so every layer that wants a say has to share them here rather
;; than clobbering whoever set them last.

(defconst ygg-space-state-rank '((error . 3) (success . 2) (warning . 1))
  "Urgency of each sidebar state-dot face; the highest-ranked wins.")

(defvar ygg-space-state-functions nil
  "Functions (SPACE-ID) -> face or nil for that space's state dot.
The most urgent face across all of them wins, per `ygg-space-state-rank'.")

(defvar ygg-space-detail-functions nil
  "Functions (SPACE-ID) -> propertized lines rendered under that space's row.")

(defun ygg-space--state-merged (space-id)
  "Worst state face any contributor reports for SPACE-ID, or nil."
  (car (seq-sort-by (lambda (f) (or (alist-get f ygg-space-state-rank) 0)) #'>
                    (delq nil (mapcar (lambda (f) (funcall f space-id))
                                      ygg-space-state-functions)))))

(defun ygg-space--details-merged (space-id)
  "Every contributor's detail lines for SPACE-ID, in registration order."
  (apply #'append
         (mapcar (lambda (f) (funcall f space-id)) ygg-space-detail-functions)))

(setq ygg-space-tree-state-function #'ygg-space--state-merged
      ygg-space-tree-detail-function #'ygg-space--details-merged)

;;; SPC p zones — a space is the project it sits in, so the project verbs
;;; and the sessions that restore them answer to the same prefix.  Rename,
;;; pin folder, clone layout, where am I and agent rows are read monthly:
;;; they answer to the colon line and to M-x instead of to a letter.
(defvar ygg-leader-workspace-map (make-sparse-keymap) "The p prefix: zones (spaces).")

(yggdrasil-define-keys 'ygg-leader-workspace-map
  "c" #'ygg-space-child :label "new child (nest deeper)"
  "s" #'ygg-space-sibling :label "new sibling"
  "j" #'ygg-space-down :label "down (first child)"
  "k" #'ygg-space-up :label "up (parent)"
  "h" #'ygg-space-prev-sibling :label "prev sibling"
  "l" #'ygg-space-next-sibling :label "next sibling"
  "o" #'ygg-space-open :label "space for a place, here or on a host"
  "d" #'ygg-space-close :label "close subtree"
  "z" #'ygg-space-pick :label "pick/jump"
  "t" #'ygg-space-tree :label "tree sidebar"
  "p" #'project-switch-project :label "switch project"
  "i" #'ygg-project-init :label "project init"
  "I" #'ygg-project-report :label "project doctor: tools and checks"
  "m" #'ygg-session-pick :label "session picker"
  "w" #'ygg-session-save-project :label "save project session"
  "r" #'ygg-session-load-project :label "resume project session")

(yggdrasil-leader-def "p" ygg-leader-workspace-map "zones")

;; toggle to the last space (nvim <leader><Tab>)
(yggdrasil-leader-def "TAB" #'ygg-space-toggle "toggle last space")

;;; SPC b b scoped to the current space's buffers (narrow `b' for all)

(declare-function ygg--job-buffer-p "layer-completion")
(declare-function consult--buffer-state "consult")
(declare-function consult-buffer "consult")
(defvar consult-source-buffer)
(defvar consult-buffer-sources)

(defvar ygg--space-buffers (make-hash-table :test 'eql)
  "space-id -> buffers touched in that space.")

(defun ygg--space-buffer-owner (buf)
  "Space id owning BUF, or nil if no space claims it yet."
  (catch 'owner
    (maphash (lambda (sid bufs) (when (memq buf bufs) (throw 'owner sid)))
             ygg--space-buffers)
    nil))

(defun ygg--space-track-buffer (&optional _frame)
  (when-let* ((id (ygg-space--current-id)))
    (dolist (win (window-list nil 'no-minibuf))
      (let ((buf (window-buffer win)))
        (when (and (or (buffer-file-name buf)
                       (and (fboundp 'ygg--job-buffer-p) (ygg--job-buffer-p buf))
                       (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                                '(xwidget-webkit-mode eww-mode)))
                   ;; exclusive: a buffer stays with its space — the MRU buffer
                   ;; that fills a window after a kill must not get adopted here
                   (memql (ygg--space-buffer-owner buf) (list nil id)))
          (cl-pushnew buf (gethash id ygg--space-buffers)))))))

(defun ygg--space-forget-buffer ()
  "Drop the dying buffer from every space's bucket."
  (let ((buf (current-buffer)))
    (maphash (lambda (id bufs)
               (puthash id (delq buf bufs) ygg--space-buffers))
             ygg--space-buffers)))

(add-hook 'kill-buffer-hook #'ygg--space-forget-buffer)

(defvar ygg-agent--registry)

(defun ygg--space-agent-buffers ()
  "Buffers of agents spawned in the current space, per the agent registry."
  (when (boundp 'ygg-agent--registry)
    (let ((id (ygg-space--current-id)))
      (seq-filter #'buffer-live-p
                  (mapcar (lambda (e) (plist-get e :buffer))
                          (seq-filter (lambda (e) (eql (plist-get e :space) id))
                                      ygg-agent--registry))))))

(defvar ygg-embr--space-buffers)

(defun ygg--space-browser-buffers ()
  "Live browser buffers to surface in this space's `b b' list.
xwidget/eww are global — a browser or preview is reachable from every space's
`b b', not just the one that happened to open it; embr comes from its own
per-space table so it stays isolated to the space that opened it."
  (let ((id (ygg-space--current-id)))
    (append
     (seq-filter (lambda (b)
                   (provided-mode-derived-p (buffer-local-value 'major-mode b)
                                            '(xwidget-webkit-mode eww-mode)))
                 (buffer-list))
     (let ((b (and id (boundp 'ygg-embr--space-buffers)
                   (gethash id ygg-embr--space-buffers))))
       (and (buffer-live-p b) (list b))))))

(defun ygg--space-buffer-names ()
  (ygg--space-track-buffer)
  (let ((bufs (append (seq-filter #'buffer-live-p
                                  (gethash (ygg-space--current-id) ygg--space-buffers))
                      (ygg--space-agent-buffers)
                      (ygg--space-browser-buffers))))
    (mapcar #'buffer-name (delete-dups bufs))))

(add-hook 'window-buffer-change-functions #'ygg--space-track-buffer)

(defun ygg-space-claim-buffer (buf id &optional move)
  "Assign BUF to space ID, before a window can claim it for another.
A buffer belongs to the space whose work it is about, and that is not
always the space it is first shown in: a Work card is filed under the
config and opened from wherever you were standing.  A buffer some space
already holds is left where it is — claiming is not stealing — unless
MOVE, for a buffer whose space is worked out from what it is about
rather than from where it was opened, and so cannot be wrong twice."
  (when (and id (buffer-live-p buf)
             (or move (null (ygg--space-buffer-owner buf))))
    (when move
      (maphash (lambda (sid bufs)
                 (unless (eql sid id) (puthash sid (delq buf bufs) ygg--space-buffers)))
               ygg--space-buffers))
    (cl-pushnew buf (gethash id ygg--space-buffers))
    id))

(defun ygg--space-adopt-agent (entry)
  "Durably assign a freshly spawned agent's buffer to its space."
  (when-let* ((id (or (plist-get entry :space) (ygg-space--current-id))))
    (cl-pushnew (plist-get entry :buffer) (gethash id ygg--space-buffers))))

(add-hook 'ygg-agent-spawn-hook #'ygg--space-adopt-agent)

(declare-function aob-sessions "aob" ())
(declare-function aob-session-ref "aob" (s key))
(declare-function ygg-cockpit-rename-buffers "ygg-cockpit" (session))
(declare-function aob-session-put "aob" (s key val))
(declare-function aob--call "aob" (s op &rest args))

(defun ygg--space-drop-task-agents (tab)
  "Kill the agents working the task TAB is dedicated to; say how many.
Closing a task's space is how that task is put down — by hand, or by
archiving, which closes the whole subtree.  An agent that outlives it
has nowhere left to belong and nobody watching it, so it stops here.
The conversation is not lost: `aob-acp' keeps every session it opened,
so `ygg-task-resume' brings any of them back."
  (when-let* ((key (alist-get 'ygg-task tab))
              ((fboundp 'aob-sessions)))
    (let ((doomed (seq-filter (lambda (s) (equal (aob-session-ref s :task) key))
                              (aob-sessions))))
      (dolist (s doomed) (ignore-errors (aob--call s :kill)))
      (length doomed))))

(defun ygg--space-reassign-on-close (tab _only)
  "A closing space drops its task's agents and wills the rest to an ancestor.
Without the second half, the closed id orphans buffers and freeform
agents out of every space's lists."
  (let ((dropped (ygg--space-drop-task-agents tab)))
    (when (and dropped (> dropped 0))
      (message "space: %d agent(s) dropped with the task" dropped)))
  (when-let* ((gone (ygg-space--id-of tab)))
    (let* ((parent (ygg-space--parent-of tab))
           (heir (or (and parent (ygg-space--tab-by-id parent) parent)
                     (ygg-space--current-id))))
      (unless (eql heir gone)
        (dolist (buf (seq-filter #'buffer-live-p (gethash gone ygg--space-buffers)))
          (cl-pushnew buf (gethash heir ygg--space-buffers)))
        (remhash gone ygg--space-buffers)
        ;; ACP agents are aob sessions, not registry entries: without this
        ;; the whole reassignment misses every agent the space actually held
        (when (fboundp 'aob-sessions)
          (dolist (s (aob-sessions))
            (when (eql (aob-session-ref s :space) gone)
              (aob-session-put s :space heir)
              (when (fboundp 'ygg-cockpit-rename-buffers)
                (ygg-cockpit-rename-buffers s)))))
        (when (boundp 'ygg-agent--registry)
          (let (moved)
            (dolist (e ygg-agent--registry)
              (when (eql (plist-get e :space) gone)
                (plist-put e :space heir)
                (setq moved t)))
            (when (and moved (fboundp 'ygg-agent--persist))
              (ygg-agent--persist))))))))

(add-hook 'tab-bar-tab-pre-close-functions #'ygg--space-reassign-on-close)

;;; q dismisses within the space: windows close while there are several;
;;; the sole window switches to the space's own previous buffer — never
;;; a foreign space's — and the space's very last buffer stays put

(defun ygg-space-quit ()
  "Dismiss this window or buffer without leaving the space.
Delete the window when Emacs will actually let it go; otherwise — a sole
window, or the main window a side window is anchored to — show the
space's previous buffer here and bury this one."
  (interactive)
  (cond
   ((and (or (window-parameter (selected-window) 'window-side)
             (cdr (seq-remove (lambda (w) (window-parameter w 'window-side))
                              (window-list nil 'no-minibuf))))
         (window-deletable-p (selected-window)))
    (delete-window))
   (t
    (let* ((cur (current-buffer))
           (names (ygg--space-buffer-names))
           (prev (seq-find (lambda (b) (and (not (eq b cur))
                                            (member (buffer-name b) names)))
                           (buffer-list))))
      (if prev (progn (switch-to-buffer prev) (bury-buffer cur))
        (bury-buffer cur))))))

;; every buffer whose q means quit-window (special modes, dired, help,
;; traces) picks this up through the remap
(global-set-key [remap quit-window] #'ygg-space-quit)

(defun ygg-buffer-space ()
  "Switch buffer, scoped to the current space; narrow `b' reveals all."
  (interactive)
  (require 'consult)
  (let* ((src (list :name "Space" :narrow ?s :category 'buffer
                    :face 'consult-buffer :history 'buffer-name-history
                    :state #'consult--buffer-state
                    :items #'ygg--space-buffer-names))
         (consult-buffer-sources
          (list src (append consult-source-buffer '(:hidden t)))))
    (consult-buffer)))

(yggdrasil-leader-def "b b" #'ygg-buffer-space "buffers (space)")

;;; Hard scoping: cycling and kill-fallback never land on another space's buffer

(defun ygg--space-agent-buffer-owner (buf)
  "Space id of the agent that owns BUF, per the agent registry."
  (when (boundp 'ygg-agent--registry)
    (cl-loop for e in ygg-agent--registry
             when (eq (plist-get e :buffer) buf)
             return (plist-get e :space))))

(defun ygg--space-foreign-buffer-p (buf)
  "BUF is claimed by another space; unclaimed buffers are never foreign."
  (when-let* ((id (ygg-space--current-id))
              (owner (or (ygg--space-buffer-owner buf)
                         (ygg--space-agent-buffer-owner buf))))
    (not (eql owner id))))

(defun ygg--space-skip-foreign (_window buffer _bury-or-kill)
  (ygg--space-foreign-buffer-p buffer))

;; covers next/prev-buffer AND the window fallback after a kill
(setq switch-to-prev-buffer-skip #'ygg--space-skip-foreign)

(defun ygg-buffer-space-other ()
  "Toggle to the most recent other buffer of the current space."
  (interactive)
  (let* ((bufs (append (seq-filter #'buffer-live-p
                                   (gethash (ygg-space--current-id) ygg--space-buffers))
                       (ygg--space-agent-buffers)))
         (other (seq-find (lambda (b) (and (memq b bufs)
                                           (not (eq b (current-buffer)))))
                          (buffer-list (selected-frame)))))
    (if other (switch-to-buffer other) (mode-line-other-buffer))))

(yggdrasil-define-keys 'ygg-goto-map
  "a" #'ygg-buffer-space-other :label "other buffer (space)")

(yggdrasil-define-keys 'normal
  "] t" #'ygg-space-next-sibling :label "next space"
  "[ t" #'ygg-space-prev-sibling :label "prev space"
  "g t" #'ygg-space-goto :label "next space (Ngt = space N)"
  "g T" #'ygg-space-prev-sibling :label "prev space")

(provide 'layer-sessions)
;;; layer-sessions.el ends here
