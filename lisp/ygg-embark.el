;;; ygg-embark.el --- embark on the things the config draws -*- lexical-binding: t; -*-

;; Every row the config draws is a thing with verbs of its own, and until
;; now only an agent answered embark.  Each finder here names what point
;; stands on and each keymap offers the commands that thing already has,
;; so C-. and SPC . reach them without a key of their own.  A target is
;; the string you would copy, which is why copying is the general map's w
;; and dropping a mention is its DEL rather than a verb written here.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar embark-general-map)
(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-target-injection-hooks)
(defvar embark-indicators)
(defvar ygg-aob-agent-map)
(declare-function embark--ignore-target "embark" (&rest _))
(declare-function embark--truncate-target "embark" (target))
(declare-function embark-verbose-indicator "embark" ())
(declare-function embark-highlight-indicator "embark" ())
(declare-function embark-isearch-highlight-indicator "embark" ())
(declare-function embark-cycle "embark" (arg))
(declare-function which-key--show-keymap "which-key"
                  (name keymap &optional prior all no-paging filter))
(declare-function which-key--hide-popup-ignore-command "which-key" ())
(declare-function ygg-qf--row-place "layer-quickfix" ())
(declare-function ygg-qf--row-file "layer-quickfix" ())
(declare-function ygg-qf-filter "layer-quickfix" (query))
(declare-function ygg-preset-get "ygg-preset" (name &optional root))
(declare-function ygg-preset-files "ygg-preset" (d))
(declare-function ygg-space--current-id "yggdrasil-spacetree" ())
(declare-function ygg-space--goto-id "yggdrasil-spacetree" (id))
(declare-function ygg-space--name "yggdrasil-spacetree" (tab))
(declare-function ygg-space--dir-of "yggdrasil-spacetree" (tab))
(declare-function ygg-space--infer-dir "yggdrasil-spacetree" (tab))
(declare-function ygg-space--tab-by-id "yggdrasil-spacetree" (id))
(declare-function ygg-space-root "yggdrasil-spacetree" (dir))
(declare-function ygg-space-close "yggdrasil-spacetree" ())
(declare-function ygg-term-new "layer-terminal" (name &optional dir))
(declare-function ygg-term--project-root "layer-terminal" (dir))
(declare-function aob-compose "aob" (&optional target initial name dir))
(declare-function aob-session-at-point "aob" ())
(declare-function aob-session-id "aob" (session))
(declare-function ygg-space-tree "yggdrasil-spacetree" ())
(declare-function ygg-qf-open "layer-quickfix" ())
(declare-function ygg-qf-kind-type "layer-quickfix" (kind))
(declare-function ygg-qf-kind-at-point "layer-quickfix" ())
(declare-function ygg-qf--kind-embark-around "layer-quickfix" (&rest args))
(defvar ygg-qf-kinds)
(defvar embark-around-action-hooks)
(declare-function ygg-qf-drop "layer-quickfix" ())
;;; What point stands on

(defun ygg-embark--line-bounds ()
  "The row point is on, as the bounds embark highlights."
  (cons (line-beginning-position) (line-end-position)))

(defun ygg-embark--target (type string bounds)
  "TYPE and STRING as a target embark highlights over BOUNDS."
  (when (and type (stringp string) (not (string-empty-p string)))
    (cons type (cons string bounds))))

(defun ygg-embark--space-by-id (id)
  "Space ID as (:id :name :root), or nil when no tab answers to it."
  (when-let* (((fboundp 'ygg-space--tab-by-id))
              (tab (ygg-space--tab-by-id id)))
    (let ((dir (or (ygg-space--dir-of tab) (ygg-space--infer-dir tab))))
      (list :id id :name (ygg-space--name tab)
            :root (and dir (ygg-space-root dir))))))

(defun ygg-embark--space ()
  "The space the row at point is about, in the space tree."
  (when-let* ((id (get-text-property (line-beginning-position) 'ygg-id)))
    (ygg-embark--space-by-id id)))

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

(defun ygg-embark-target-space ()
  "Tell embark point stands on a space in the space tree."
  (when-let* ((space (ygg-embark--space)))
    (ygg-embark--target 'ygg-space (plist-get space :name)
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-qf-kind ()
  "Tell embark point stands on a row of a kind of quickfix list, as KIND and ID."
  (when-let* (((fboundp 'ygg-qf-kind-at-point))
              (at (ygg-qf-kind-at-point)))
    (ygg-embark--target (ygg-qf-kind-type (car at))
                        (propertize (format "%s" (cdr at))
                                    'ygg-qf-kind (car at) 'ygg-qf-id (cdr at))
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-qf-row ()
  "Tell embark point stands on a quickfix row, named as FILE:LINE."
  (when-let* ((row (ygg-embark--qf-row)))
    (ygg-embark--target 'ygg-qf-row
                        (format "%s:%d" (car row) (cdr row))
                        (ygg-embark--line-bounds))))

(defun ygg-embark-target-mention ()
  "Tell embark point stands on a mention in a draft."
  (when-let* ((mention (ygg-embark--mention)))
    (ygg-embark--target 'ygg-mention (car mention) (cdr mention))))

(defconst ygg-embark-target-finders
  '(ygg-embark-target-space
    ygg-embark-target-qf-kind
    ygg-embark-target-qf-row
    ygg-embark-target-mention)
  "Every finder this module adds, the narrowest thing first.")

;;; The verbs nothing else spells: a space, a row, a mention

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
  (let ((dir (file-name-as-directory (ygg-embark--space-root))))
    (ygg-term-new (read-string "Terminal name: ") (ygg-term--project-root dir))))

(defun ygg-embark--qf-here ()
  "The quickfix row a verb here acts on, refusing when point names none."
  (or (ygg-embark--qf-row) (user-error "No quickfix row here")))

(defun ygg-embark-qf-filter-file ()
  "Narrow the list to the file the row at point names."
  (interactive)
  (ygg-qf-filter (car (ygg-embark--qf-here))))

(defun ygg-embark-qf-filter-without-file ()
  "Narrow the list to everything but the file the row at point names."
  (interactive)
  (ygg-qf-filter (concat "!" (car (ygg-embark--qf-here)))))

(defun ygg-embark--mention-here ()
  "The word the mention at point names, refusing when there is none."
  (or (car (ygg-embark--mention)) (user-error "No mention here")))

(defun ygg-embark-mention-preset-file ()
  "Open the file the preset the mention at point names is written in."
  (interactive)
  (let* ((name (ygg-embark--mention-here))
         (preset (ygg-preset-get name))
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

(defvar ygg-embark-space-map
  (ygg-embark--map nil
                   "s" #'ygg-embark-space-switch
                   "k" #'ygg-embark-space-close
                   "T" #'ygg-space-tree
                   "t" #'ygg-embark-space-terminal)
  "What you can do to the space under point.")

(defvar ygg-embark-qf-map
  (ygg-embark--map nil
                   "o" #'ygg-qf-open
                   "d" #'ygg-qf-drop
                   "f" #'ygg-embark-qf-filter-file
                   "F" #'ygg-embark-qf-filter-without-file)
  "What you can do to the quickfix row under point.
Copying it as FILE:LINE is the general map's w: the target is that.")

(defvar ygg-embark-mention-map
  (ygg-embark--map nil
                   "f" #'ygg-embark-mention-preset-file)
  "What you can do to the mention under point.
Dropping it is the general map's DEL: the bounds cover the mention.")

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
(declare-function ygg-conversation-discard "ygg-projects" (candidate &optional ask))
(declare-function ygg-conversation-open "ygg-projects" (candidate))

(defconst ygg-embark-keymaps
  '((ygg-conversation . ygg-embark-conversation-map)
    (ygg-space . ygg-embark-space-map)
    (ygg-qf-row . ygg-embark-qf-map)
    (ygg-mention . ygg-embark-mention-map)
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

(defun ygg-embark--kind-cells ()
  "The (TYPE . KEYMAP) of every kind of quickfix row that has a keymap."
  (delq nil (mapcar (lambda (kind)
                      (when-let* ((map (plist-get (cdr kind) :map)))
                        (cons (ygg-qf-kind-type (car kind)) map)))
                    (bound-and-true-p ygg-qf-kinds))))

(defun ygg-embark-verbs ()
  "Every command the maps here bind, each one once."
  (seq-uniq (seq-mapcat (lambda (cell)
                          (ygg-embark-commands (symbol-value (cdr cell))))
                        (append ygg-embark-keymaps (ygg-embark--kind-cells)))))

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

(defun ygg-embark-add-map (type map)
  "Let embark offer MAP, a keymap symbol, on targets of TYPE.
Its verbs take the target as the id it carries and are not given it again
as input, so one that prompts keeps its own prompt."
  (add-to-list 'embark-keymap-alist (cons type map))
  (unless (keymap-parent (symbol-value map))
    (set-keymap-parent (symbol-value map) embark-general-map))
  (dolist (command (ygg-embark-commands (symbol-value map)))
    (add-to-list 'embark-target-injection-hooks
                 (list command #'embark--ignore-target))
    (add-to-list 'embark-around-action-hooks
                 (list command #'ygg-qf--kind-embark-around))))

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
  (pcase-dolist (`(,type . ,map) (ygg-embark--kind-cells))
    (ygg-embark-add-map type map))
  (setq embark-indicators (list #'ygg-embark-menu-indicator
                                #'embark-highlight-indicator
                                #'embark-isearch-highlight-indicator))
  (define-key embark-general-map (kbd "C-.") #'embark-cycle))

(with-eval-after-load 'embark (ygg-embark-install))

(provide 'ygg-embark)
;;; ygg-embark.el ends here
