;;; ygg-embark.el --- embark on the things the config draws -*- lexical-binding: t; -*-

;; Every row the config draws is a thing with verbs of its own, and until
;; now only an agent answered embark.  Each finder here names what point
;; stands on and each keymap offers the commands that thing already has,
;; so C-. and SPC . reach them without a key of their own.  A target is
;; the string you would copy, which is why copying is the general map's w
;; and dropping a mention is its DEL rather than a verb written here.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)

(defvar embark-general-map)
(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-target-injection-hooks)
(defvar embark-around-action-hooks)
(defvar embark-indicators)
(defvar ygg-aob-agent-map)
(defvar ygg-context-quickfix-window)
(defvar ygg-trace--task)

(declare-function embark--ignore-target "embark" (&rest _))
(declare-function embark--truncate-target "embark" (target))
(declare-function embark-verbose-indicator "embark" ())
(declare-function embark-highlight-indicator "embark" ())
(declare-function embark-isearch-highlight-indicator "embark" ())
(declare-function embark-cycle "embark" (arg))
(declare-function which-key--show-keymap "which-key"
                  (name keymap &optional prior all no-paging filter))
(declare-function which-key--hide-popup-ignore-command "which-key" ())
(declare-function ygg-task-list "ygg-task" ())
(declare-function ygg-task--candidates "ygg-task" (tasks))
(declare-function ygg-task-here-or-read "ygg-task" ())
(declare-function ygg-task-root-here "ygg-task" ())
(declare-function ygg-qf--row-place "layer-quickfix" ())
(declare-function ygg-qf--row-file "layer-quickfix" ())
(declare-function ygg-qf-filter "layer-quickfix" (query))
(declare-function ygg-context-add "ygg-context"
                  (task path &optional tier &rest props))
(declare-function ygg-context-entry "ygg-context" (task path))
(declare-function ygg-context-tier "ygg-context" (task path &optional tier))
(declare-function ygg-context-redraw "ygg-context" ())
(declare-function ygg-context--buffer-task "ygg-context" ())
(declare-function ygg-context--root "ygg-context" (task))
(declare-function ygg-context--mention-root "ygg-context" ())
(declare-function ygg-to-compose "ygg-to-compose" ())
(declare-function ygg-to-compose-mention-parse "ygg-to-compose" (line))
(declare-function ygg-preset-get "ygg-preset" (name &optional root))
(declare-function ygg-preset-files "ygg-preset" (d))
(declare-function ygg-space--current-id "yggdrasil-spacetree" ())
(declare-function ygg-space--goto-id "yggdrasil-spacetree" (id))
(declare-function ygg-space--id-of "yggdrasil-spacetree" (tab))
(declare-function ygg-space--name "yggdrasil-spacetree" (tab))
(declare-function ygg-space--dir-of "yggdrasil-spacetree" (tab))
(declare-function ygg-space--infer-dir "yggdrasil-spacetree" (tab))
(declare-function ygg-space--tab-by-id "yggdrasil-spacetree" (id))
(declare-function ygg-space--for-dir "yggdrasil-spacetree" (dir))
(declare-function ygg-space-root "yggdrasil-spacetree" (dir))
(declare-function ygg-space-close "yggdrasil-spacetree" ())
(declare-function ygg-term-new "layer-terminal" (name))
(declare-function ygg-daemon-launch "ygg-daemon" (&optional task default-root))
(declare-function ygg-daemon-oneshot "ygg-daemon" (&optional default-root))
(declare-function aob-compose "aob" (&optional target initial name dir))
(declare-function aob-session-at-point "aob" ())
(declare-function aob-session-id "aob" (session))
(declare-function magit-current-section "magit-section" ())
(declare-function ygg-trace-show "ygg-trace" (&optional task))
(declare-function ygg-trace-open "ygg-trace" ())
(declare-function ygg-trace-rerun-check "ygg-trace" ())
(declare-function ygg-trace-toggle-breakpoint "ygg-trace" ())
(declare-function ygg-trace-remove-item "ygg-trace" ())
(declare-function ygg-daemon-start "ygg-daemon" (task &optional brief))
(declare-function ygg-daemon-oneshot-task "ygg-daemon" (task))
(declare-function ygg-daemon-continue "ygg-daemon" (task &optional note))
(declare-function ygg-daemon-stop "ygg-daemon" (task &optional note))
(declare-function ygg-daemon-edit-composed "ygg-daemon" (task))
(declare-function ygg-task-archive "ygg-task" (task))
(declare-function ygg-task-open-root "ygg-task-space" (task &optional root))
(declare-function ygg-task-space "ygg-task-space" (task))
(declare-function ygg-space-tree "yggdrasil-spacetree" ())
(declare-function ygg-qf-open "layer-quickfix" ())
(declare-function ygg-qf-drop "layer-quickfix" ())
(declare-function ygg-findings-to-context "ygg-findings" ())
(declare-function ygg-findings-to-quickfix "ygg-findings" ())
(declare-function ygg-findings-to-compose "ygg-findings" ())
(declare-function ygg-findings-open-row "ygg-findings" ())
(declare-function ygg-cm-open "ygg-context-manager" ())
(declare-function ygg-cm-narrow "ygg-context-manager" (query))
(declare-function ygg-cm-filter-pop "ygg-context-manager" ())
(declare-function ygg-cm-to-quickfix "ygg-context-manager" ())
(declare-function ygg-cm-all-to-quickfix "ygg-context-manager" ())
(declare-function ygg-cm-affected-here "ygg-context-manager" ())
(declare-function ygg-context-open-row "ygg-context" ())
(declare-function ygg-context-drop-row "ygg-context" ())
(declare-function ygg-context-visit-mention "ygg-context" ())
(declare-function ygg-actions-text "ygg-actions" (&optional initial))
(declare-function ygg-actions-instead "ygg-actions" ())
(declare-function ygg-actions-reason "ygg-actions" ())
(declare-function ygg-actions-trace "ygg-actions" ())
(declare-function ygg-actions-press "ygg-actions" ())

;;; What point stands on

(defun ygg-embark--line-bounds ()
  "The row point is on, as the bounds embark highlights."
  (cons (line-beginning-position) (line-end-position)))

(defun ygg-embark--property-bounds (property)
  "Where the run of PROPERTY around point begins and ends."
  (let ((pos (if (get-text-property (point) property)
                 (point)
               (line-beginning-position))))
    (cons (or (previous-single-property-change
               (min (point-max) (1+ pos)) property)
              (point-min))
          (or (next-single-property-change pos property) (point-max)))))

(defun ygg-embark--target (type string bounds)
  "TYPE and STRING as a target embark highlights over BOUNDS."
  (when (and type (stringp string) (not (string-empty-p string)))
    (cons type (cons string bounds))))

(defun ygg-embark--section (type)
  "The value of the section at point when it is one of TYPE, else nil.
TYPE is a symbol or a list of them, so a thing drawn under two names in
two places is found by the one finder."
  (when (fboundp 'magit-current-section)
    (when-let* ((section (magit-current-section))
                (types (if (listp type) type (list type)))
                ((memq (oref section type) types)))
      (list (oref section value)))))

(defun ygg-embark--row-task ()
  "The task the row at point carries, under either of its names."
  (or (get-text-property (point) 'ygg-task)
      (get-text-property (line-beginning-position) 'ygg-task)
      (get-text-property (point) 'ygg-task-tree-task)
      (get-text-property (point) 'ygg-actions-task)))

(defun ygg-embark--task ()
  "The task point stands on, or the one this trace is about."
  (or (ygg-embark--row-task)
      (and (boundp 'ygg-trace--task) ygg-trace--task)))

(defun ygg-embark--task-named (slug)
  "The task the picker offers as SLUG, read afresh rather than cached."
  (when (and (stringp slug) (fboundp 'ygg-task-list))
    (cdr (assoc slug (ygg-task--candidates (ygg-task-list))))))

(defun ygg-embark--space-by-id (id)
  "Space ID as (:id :name :root), or nil when no tab answers to it."
  (when-let* (((fboundp 'ygg-space--tab-by-id))
              (tab (ygg-space--tab-by-id id)))
    (let ((dir (or (ygg-space--dir-of tab) (ygg-space--infer-dir tab))))
      (list :id id :name (ygg-space--name tab)
            :root (and dir (ygg-space-root dir))))))

(defun ygg-embark--space-by-root (root)
  "The space standing on ROOT as (:id :name :root), the root alone else."
  (let* ((tab (and (fboundp 'ygg-space--for-dir) (ygg-space--for-dir root)))
         (id (and tab (ygg-space--id-of tab))))
    (list :id id
          :name (or (and tab (ygg-space--name tab))
                    (file-name-nondirectory (directory-file-name root)))
          :root root)))

(defun ygg-embark--space ()
  "The space the row at point is about, in the tree or in the sidebar."
  (if-let* ((id (get-text-property (line-beginning-position) 'ygg-id)))
      (ygg-embark--space-by-id id)
    (when-let* ((root (or (get-text-property (point) 'ygg-task-tree-project)
                          (get-text-property (line-beginning-position)
                                             'ygg-task-tree-project))))
      (ygg-embark--space-by-root root))))

(defun ygg-embark--qf-row ()
  "The quickfix row at point as (FILE . LINE), as written and as read."
  (when (and (get-text-property (line-beginning-position) 'ygg-qf-row)
             (fboundp 'ygg-qf--row-place))
    (when-let* ((place (ygg-qf--row-place)))
      (cons (or (and (fboundp 'ygg-qf--row-file) (ygg-qf--row-file))
                (car place))
            (cdr place)))))

(defconst ygg-embark--mention-rx
  "@@?\\([^@[:space:]\n,;:)#]+\\)\\(?::\\([0-9]+\\)-\\([0-9]+\\)\\)?"
  "A mention as a draft writes one: a path, a preset or a word.
The line range is read so the bounds cover the whole mention rather
than stopping where the path does.")

(defun ygg-embark--mention ()
  "The mention point stands in as (WORD BEG . END), or nil for none."
  (unless (minibufferp)
   (save-excursion
    (let ((here (point))
          (found nil))
      (goto-char (line-beginning-position))
      (while (and (not found)
                  (re-search-forward ygg-embark--mention-rx
                                     (line-end-position) t))
        (when (and (<= (match-beginning 0) here) (<= here (match-end 0)))
          (setq found (list (match-string-no-properties 1)
                            (match-beginning 0) (match-end 0)))))
      (when found
        (cons (car found) (cons (nth 1 found) (nth 2 found))))))))

;;; The finders

(defun ygg-embark-target-task ()
  "Tell embark point stands on a task, wherever the task is drawn."
  (when-let* ((task (ygg-embark--task)))
    (ygg-embark--target 'ygg-task (plist-get task :slug)
                        (and (ygg-embark--row-task)
                             (ygg-embark--line-bounds)))))

(defun ygg-embark-target-space ()
  "Tell embark point stands on a space, in the tree or the sidebar."
  (when-let* ((space (ygg-embark--space)))
    (ygg-embark--target 'ygg-space (plist-get space :name)
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-qf-row ()
  "Tell embark point stands on a quickfix row, named as FILE:LINE."
  (when-let* ((row (ygg-embark--qf-row)))
    (ygg-embark--target 'ygg-qf-row
                        (format "%s:%d" (car row) (cdr row))
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-context-entry ()
  "Tell embark point stands on a context entry, or a file under one."
  (when-let* ((value (car (ygg-embark--section
                           '(ygg-context-entry ygg-context-file)))))
    (ygg-embark--target 'ygg-context-entry
                        (if (stringp value) value (plist-get value :path))
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-finding ()
  "Tell embark point stands on a finding, named as FILE:LINE."
  (when-let* ((row (car (ygg-embark--section 'ygg-findings-row))))
    (ygg-embark--target 'ygg-finding
                        (format "%s:%d" (plist-get row :file)
                                (plist-get row :line))
                        (ygg-embark--line-bounds))))

(defun ygg-embark--cm-row ()
  "The map row at point as (FILE . LINE), or nil when it is on none."
  (when (fboundp 'magit-current-section)
    (when-let* ((section (magit-current-section))
                (value (oref section value)))
      (pcase (oref section type)
        ('ygg-cm-def (cons (plist-get value :file) (plist-get value :line)))
        ((or 'ygg-cm-fact 'ygg-cm-owner 'ygg-cm-row)
         (cons (plist-get value :file) 1))
        ('ygg-cm-file (and (stringp value) (cons value 1)))))))

(defun ygg-embark-target-map-row ()
  "Tell embark point stands on a row of the map, named as FILE:LINE."
  (when-let* ((row (ygg-embark--cm-row))
              ((stringp (car row))))
    (ygg-embark--target 'ygg-map-row
                        (format "%s:%d" (car row) (or (cdr row) 1))
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-mention ()
  "Tell embark point stands on a mention in a draft."
  (when-let* ((mention (ygg-embark--mention)))
    (ygg-embark--target 'ygg-mention (car mention) (cdr mention))))

(defun ygg-embark-target-criterion ()
  "Tell embark point stands on a criterion, proposed or pinned."
  (when-let* ((cmd (car (ygg-embark--section
                         '(criterion proposal-criterion)))))
    (ygg-embark--target 'ygg-criterion cmd (ygg-embark--line-bounds))))

(defun ygg-embark-target-breakpoint ()
  "Tell embark point stands on a breakpoint row."
  (when-let* ((bp (car (ygg-embark--section 'breakpoint-item))))
    (ygg-embark--target 'ygg-breakpoint (plist-get bp :id)
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-action-card ()
  "Tell embark point stands on a card of the actions panel."
  (when-let* ((task (get-text-property (point) 'ygg-actions-task)))
    (ygg-embark--target 'ygg-action-card (plist-get task :slug)
                        (ygg-embark--property-bounds 'ygg-actions-task))))

(defun ygg-embark-target-action-button ()
  "Tell embark point stands on a button of a card."
  (when (get-text-property (point) 'ygg-actions-action)
    (let ((bounds (ygg-embark--property-bounds 'ygg-actions-action)))
      (ygg-embark--target
       'ygg-action-button
       (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))
       bounds))))

(defconst ygg-embark-target-finders
  '(ygg-embark-target-action-button
    ygg-embark-target-action-card
    ygg-embark-target-task
    ygg-embark-target-space
    ygg-embark-target-qf-row
    ygg-embark-target-context-entry
    ygg-embark-target-finding
    ygg-embark-target-map-row
    ygg-embark-target-mention
    ygg-embark-target-criterion
    ygg-embark-target-breakpoint)
  "Every finder this module adds, the narrowest thing first.")

;;; The verbs nothing else spells: a space, a row, an entry, a mention

(defun ygg-embark--space-here ()
  "The space a verb here acts on, refusing when point names none."
  (or (ygg-embark--space) (user-error "No space here")))

(defun ygg-embark-space-switch ()
  "Switch to the space the row at point names."
  (interactive)
  (let ((space (ygg-embark--space-here)))
    (unless (and (plist-get space :id)
                 (ygg-space--goto-id (plist-get space :id)))
      (user-error "%s has no tab open" (plist-get space :name)))))

(defun ygg-embark-space-close ()
  "Close the space the row at point names, standing in it first."
  (interactive)
  (ygg-embark-space-switch)
  (ygg-space-close))

(defun ygg-embark--space-root ()
  "The checkout the space at point stands on, refusing when it has none."
  (let ((space (ygg-embark--space-here)))
    (or (plist-get space :root)
        (user-error "%s stands on no checkout" (plist-get space :name)))))

(defun ygg-embark-space-terminal ()
  "Open a terminal in the checkout the space at point stands on."
  (interactive)
  (let ((default-directory (file-name-as-directory (ygg-embark--space-root))))
    (call-interactively #'ygg-term-new)))

(defun ygg-embark-space-launch ()
  "Launch a task in the checkout the space at point stands on."
  (interactive)
  (ygg-daemon-launch nil (ygg-embark--space-root)))

(defun ygg-embark-space-oneshot ()
  "Compose a one-turn task in the checkout the space at point stands on."
  (interactive)
  (ygg-daemon-oneshot (ygg-embark--space-root)))

(defun ygg-embark--qf-here ()
  "The quickfix row a verb here acts on, refusing when point names none."
  (or (ygg-embark--qf-row) (user-error "No quickfix row here")))

(defun ygg-embark--send-place (file line)
  "Send LINE of FILE into the draft the way the place you stand in goes."
  (with-current-buffer (find-file-noselect file)
    (save-mark-and-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (push-mark (line-beginning-position) t t)
      (goto-char (line-end-position))
      (ygg-to-compose))))

(defun ygg-embark-qf-to-compose ()
  "Send the quickfix row at point into the draft as its place and words."
  (interactive)
  (let ((row (ygg-embark--qf-here)))
    (ygg-embark--send-place (expand-file-name (car row)) (cdr row))))

(defun ygg-embark-qf-to-context ()
  "Take the quickfix row at point into the context as an excerpt."
  (interactive)
  (let* ((row (ygg-embark--qf-here))
         (task (ygg-context--buffer-task))
         (window (if (boundp 'ygg-context-quickfix-window)
                     ygg-context-quickfix-window
                   0))
         (line (cdr row)))
    (ygg-context-add task (car row) 'excerpt
                     :lines (cons (max 1 (- line window)) (+ line window)))
    (ygg-context-redraw)))

(defun ygg-embark-qf-filter-file ()
  "Narrow the list to the file the row at point names."
  (interactive)
  (ygg-qf-filter (car (ygg-embark--qf-here))))

(defun ygg-embark-qf-filter-without-file ()
  "Narrow the list to everything but the file the row at point names."
  (interactive)
  (ygg-qf-filter (concat "!" (car (ygg-embark--qf-here)))))

(defun ygg-embark--entry-path ()
  "The path of the context entry at point, refusing when there is none."
  (let ((value (car (ygg-embark--section
                     '(ygg-context-entry ygg-context-file)))))
    (cond ((stringp value) value)
          (value (plist-get value :path))
          (t (user-error "No context row here")))))

(defun ygg-embark--retier (tier)
  "Set the tier of the entry at point to TIER."
  (let ((path (ygg-embark--entry-path)))
    (ygg-context-tier (ygg-context--buffer-task) path tier)
    (ygg-context-redraw)))

(defun ygg-embark-context-full ()
  "Carry the whole of the entry at point."
  (interactive)
  (ygg-embark--retier 'full))

(defun ygg-embark-context-outline ()
  "Carry only the outline of the entry at point."
  (interactive)
  (ygg-embark--retier 'outline))

(defun ygg-embark-context-excerpt ()
  "Carry only the excerpt of the entry at point."
  (interactive)
  (ygg-embark--retier 'excerpt))

(defun ygg-embark-context-pin ()
  "Pin the entry at point, so eviction leaves it where it is."
  (interactive)
  (let* ((task (ygg-context--buffer-task))
         (path (ygg-embark--entry-path))
         (entry (ygg-context-entry task path)))
    (ygg-context-add task path (plist-get entry :tier) :pinned t)
    (ygg-context-redraw)))

(defun ygg-embark-context-to-compose ()
  "Send the entry at point into the draft as a mention of its file."
  (interactive)
  (let* ((task (ygg-context--buffer-task))
         (path (ygg-embark--entry-path)))
    (with-current-buffer (find-file-noselect
                          (expand-file-name path (ygg-context--root task)))
      (ygg-to-compose))))

(defun ygg-embark--mention-here ()
  "The word the mention at point names, refusing when there is none."
  (or (car (ygg-embark--mention)) (user-error "No mention here")))

(defun ygg-embark-mention-to-context ()
  "Take the file the mention at point names into the context whole."
  (interactive)
  (ygg-context-add (ygg-context--buffer-task) (ygg-embark--mention-here) 'full)
  (ygg-context-redraw))

(defun ygg-embark-mention-preset-file ()
  "Open the file the preset the mention at point names is written in."
  (interactive)
  (let* ((name (ygg-embark--mention-here))
         (root (and (fboundp 'ygg-context--mention-root)
                    (ygg-context--mention-root)))
         (preset (ygg-preset-get name root))
         (file (car (and preset (ygg-preset-files preset)))))
    (unless file (user-error "No preset called %s" name))
    (find-file file)))

(defun ygg-embark-session-compose ()
  "Open a draft aimed at the session the row at point is about."
  (interactive)
  (let ((session (or (and (fboundp 'aob-session-at-point)
                          (aob-session-at-point))
                     (user-error "No session here"))))
    (aob-compose (aob-session-id session))))

;;; What each thing answers to

(defun ygg-embark--map (parent &rest bindings)
  "A keymap under PARENT taking BINDINGS as key and command pairs."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    (while bindings
      (define-key map (kbd (pop bindings)) (pop bindings)))
    map))

(defvar ygg-embark-task-map
  (ygg-embark--map nil
                   "t" #'ygg-trace-show
                   "d" #'ygg-daemon-start
                   "1" #'ygg-daemon-oneshot-task
                   "c" #'ygg-daemon-continue
                   "s" #'ygg-daemon-stop
                   "e" #'ygg-daemon-edit-composed
                   "X" #'ygg-task-archive
                   "D" #'ygg-task-open-root
                   "p" #'ygg-task-space)
  "What you can do to the task under point, or the one you picked.
Copying the slug is the general map's w: the target is the slug.")

(defvar ygg-embark-space-map
  (ygg-embark--map nil
                   "s" #'ygg-embark-space-switch
                   "k" #'ygg-embark-space-close
                   "T" #'ygg-space-tree
                   "t" #'ygg-embark-space-terminal
                   "l" #'ygg-embark-space-launch
                   "1" #'ygg-embark-space-oneshot)
  "What you can do to the space under point.")

(defvar ygg-embark-qf-map
  (ygg-embark--map nil
                   "o" #'ygg-qf-open
                   "c" #'ygg-embark-qf-to-compose
                   "x" #'ygg-embark-qf-to-context
                   "d" #'ygg-qf-drop
                   "f" #'ygg-embark-qf-filter-file
                   "F" #'ygg-embark-qf-filter-without-file)
  "What you can do to the quickfix row under point.
Copying it as FILE:LINE is the general map's w: the target is that.")

(defvar ygg-embark-context-map
  (ygg-embark--map nil
                   "o" #'ygg-context-open-row
                   "1" #'ygg-embark-context-full
                   "2" #'ygg-embark-context-outline
                   "3" #'ygg-embark-context-excerpt
                   "p" #'ygg-embark-context-pin
                   "d" #'ygg-context-drop-row
                   "c" #'ygg-embark-context-to-compose)
  "What you can do to the context entry under point.")

(defvar ygg-embark-finding-map
  (ygg-embark--map nil
                   "o" #'ygg-findings-open-row
                   "x" #'ygg-findings-to-context
                   "d" #'ygg-findings-to-quickfix
                   "c" #'ygg-findings-to-compose)
  "What you can do to the finding under point: the three places it goes.
Copying it as FILE:LINE is the general map's w: the target is that.")

(defvar ygg-embark-map-row-map
  (ygg-embark--map nil
                   "o" #'ygg-cm-open
                   "d" #'ygg-cm-to-quickfix
                   "D" #'ygg-cm-all-to-quickfix
                   "f" #'ygg-cm-narrow
                   "F" #'ygg-cm-filter-pop
                   "a" #'ygg-cm-affected-here)
  "What you can do to the map row under point: narrow the map to what the
row is about and send what the filter leaves to the quickfix.
Copying it as FILE:LINE is the general map's w: the target is that.")

(defvar ygg-embark-mention-map
  (ygg-embark--map nil
                   "o" #'ygg-context-visit-mention
                   "a" #'ygg-embark-mention-to-context
                   "f" #'ygg-embark-mention-preset-file)
  "What you can do to the mention under point.
Dropping it is the general map's DEL: the bounds cover the mention.")

(defvar ygg-embark-criterion-map
  (ygg-embark--map nil
                   "r" #'ygg-trace-rerun-check
                   "o" #'ygg-trace-open)
  "What you can do to the criterion under point.
Copying the command is the general map's w: the target is the command.")

(defvar ygg-embark-breakpoint-map
  (ygg-embark--map nil
                   "t" #'ygg-trace-toggle-breakpoint
                   "o" #'ygg-trace-open
                   "d" #'ygg-trace-remove-item)
  "What you can do to the breakpoint under point.")

(defvar ygg-embark-card-map
  (ygg-embark--map nil
                   "a" #'ygg-actions-text
                   "I" #'ygg-actions-instead
                   "n" #'ygg-actions-reason
                   "o" #'ygg-actions-trace)
  "What you can do to the card under point.")

(defvar ygg-embark-button-map
  (ygg-embark--map ygg-embark-card-map
                   "RET" #'ygg-actions-press
                   "p" #'ygg-actions-press)
  "What you can do to the button under point, over the card's own verbs.")

(defvar ygg-embark-session-map
  (ygg-embark--map nil "c" #'ygg-embark-session-compose)
  "To compose aimed at the session, over the agent verbs it inherits.")

(defvar-keymap ygg-embark-conversation-map
  :doc "What can be done to a conversation, one or fifty at a time."
  "a" #'ygg-conversation-archive
  "d" #'ygg-conversation-discard
  "o" #'ygg-conversation-open
  "RET" #'ygg-conversation-open)

(declare-function ygg-conversation-archive "ygg-projects" (candidate))
(declare-function ygg-conversation-discard "ygg-projects" (candidate))
(declare-function ygg-conversation-open "ygg-projects" (candidate))

(defconst ygg-embark-keymaps
  '((ygg-conversation . ygg-embark-conversation-map)
    (ygg-task . ygg-embark-task-map)
    (ygg-space . ygg-embark-space-map)
    (ygg-qf-row . ygg-embark-qf-map)
    (ygg-context-entry . ygg-embark-context-map)
    (ygg-finding . ygg-embark-finding-map)
    (ygg-map-row . ygg-embark-map-row-map)
    (ygg-mention . ygg-embark-mention-map)
    (ygg-criterion . ygg-embark-criterion-map)
    (ygg-breakpoint . ygg-embark-breakpoint-map)
    (ygg-action-card . ygg-embark-card-map)
    (ygg-action-button . ygg-embark-button-map)
    (ygg-agent . ygg-embark-session-map)
    (aob-session . ygg-embark-session-map))
  "Which keymap each thing answers to, minibuffer categories included.")

(defun ygg-embark-commands (map)
  "The commands MAP binds itself, leaving what it inherits alone."
  (let ((tail (cdr map))
        (parent (keymap-parent map))
        (found nil))
    (while (and (consp tail) (not (eq tail parent)))
      (let ((entry (car tail)))
        (when (and (consp entry) (cdr entry) (symbolp (cdr entry)))
          (push (cdr entry) found)))
      (setq tail (cdr tail)))
    (nreverse found)))

(defun ygg-embark-verbs ()
  "Every command the maps here bind, each one once."
  (seq-uniq (seq-mapcat (lambda (cell)
                          (ygg-embark-commands (symbol-value (cdr cell))))
                        ygg-embark-keymaps)))

;;; The target is what the verb acts on, never what the prompt reads

(cl-defun ygg-embark--as-task (&rest rest &key run type bounds target
                                     &allow-other-keys)
  "Run the action on the task TARGET names rather than on the row at point.
A target found at point comes with BOUNDS and needs nothing: embark
already runs the action where it was found.  A candidate picked in the
minibuffer has none, and there the slug is the only answer there is."
  (let ((task (and (null bounds) (eq type 'ygg-task)
                   (ygg-embark--task-named target))))
    (if task
        (cl-letf (((symbol-function 'ygg-task-here) (lambda () task)))
          (apply run rest))
      (apply run rest))))

;;; The menu: the verbs shown the way a prefix key is shown

(defun ygg-embark--menu-target (target)
  "TARGET as short as the one line over the menu can hold it."
  (if (fboundp 'embark--truncate-target)
      (embark--truncate-target target)
    (format "%s" target)))

(defun ygg-embark-menu-title (targets)
  "The one line over the menu: what is acted on, and what else waits.
TARGETS are embark's own, the thing acted on first."
  (let ((target (car targets)))
    (if (eq (plist-get target :type) 'embark-become)
        "become"
      (format "act on %s %s%s"
              (plist-get target :type)
              (ygg-embark--menu-target (plist-get target :target))
              (if (cdr targets) " …" "")))))

(defun ygg-embark--menu-keymap (keymap prefix)
  "KEYMAP as the menu shows it, or what PREFIX leads to inside it."
  (if prefix
      (pcase (lookup-key keymap prefix 'accept-default)
        ((and (pred keymapp) nested) nested)
        (_ (key-binding prefix 'accept-default)))
    keymap))

(defun ygg-embark--menu-shown-p (binding)
  "Whether BINDING is a verb rather than one of the argument keys."
  (not (string-suffix-p "-argument" (cdr binding))))

(defun ygg-embark-menu-indicator ()
  "Show the verbs of the thing acted on the way which-key shows a prefix.
Falls back to the table embark draws itself where which-key is not
there to draw one."
  (let ((fallback (unless (fboundp 'which-key--show-keymap)
                    (embark-verbose-indicator))))
    (lambda (&optional keymap targets prefix)
      (cond
       (fallback (funcall fallback keymap targets prefix))
       ((null keymap) (which-key--hide-popup-ignore-command))
       (t (which-key--show-keymap (ygg-embark-menu-title targets)
                                  (ygg-embark--menu-keymap keymap prefix)
                                  nil nil t
                                  #'ygg-embark--menu-shown-p))))))

(defun ygg-embark-install ()
  "Let embark know every thing, every keymap and every verb here."
  (dolist (finder (reverse ygg-embark-target-finders))
    (add-to-list 'embark-target-finders finder))
  (dolist (cell ygg-embark-keymaps)
    (add-to-list 'embark-keymap-alist cell))
  (dolist (cell ygg-embark-keymaps)
    (let ((map (symbol-value (cdr cell))))
      (unless (keymap-parent map)
        (set-keymap-parent map embark-general-map))))
  (when (and (boundp 'ygg-aob-agent-map)
             (not (keymap-parent ygg-aob-agent-map)))
    (set-keymap-parent ygg-aob-agent-map embark-general-map))
  (when (boundp 'ygg-aob-agent-map)
    (set-keymap-parent ygg-embark-session-map ygg-aob-agent-map))
  (dolist (command (ygg-embark-verbs))
    (add-to-list 'embark-target-injection-hooks
                 (list command #'embark--ignore-target)))
  (dolist (command (ygg-embark-commands ygg-embark-task-map))
    (add-to-list 'embark-around-action-hooks
                 (list command #'ygg-embark--as-task)))
  (setq embark-indicators (list #'ygg-embark-menu-indicator
                                #'embark-highlight-indicator
                                #'embark-isearch-highlight-indicator))
  (define-key embark-general-map (kbd "C-.") #'embark-cycle))

(with-eval-after-load 'embark (ygg-embark-install))

(provide 'ygg-embark)
;;; ygg-embark.el ends here
