;;; yggdrasil-actions.el --- action groups: ; / C-; span cycling -*- lexical-binding: t; -*-

;; Built-ins wrapped: pre/post-command-hook, markers.
;; Custom: consecutive same-category commands (ww, jj) auto-group into
;; spans; ; selects the latest group's span and cycles older ones on
;; repeat, C-; pulls the anchor back to the current group's start.
;; paste/replace record the pasted/replaced text so ; re-selects it.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-selection)

(defcustom ygg-action-categories
  '((word ygg-w ygg-e ygg-b ygg-W ygg-E ygg-B)
    ;; ygg--vmove masquerades this-command as next/previous-line
    (line ygg-j ygg-k next-line previous-line)
    (char ygg-h ygg-l)
    (find ygg-find-forward ygg-till-forward ygg-find-backward
          ygg-till-backward ygg-repeat-find)
    (search ygg-search-next ygg-search-prev)
    (paste ygg-paste-after ygg-paste-before)
    (replace ygg-replace-char ygg-replace-with-kill))
  "Commands that form action groups, by category; drop entries to disable."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'yggdrasil)

(defcustom ygg-action-max-groups 32
  "How many action groups to remember per buffer."
  :type 'natnum :group 'yggdrasil)

(defvar-local ygg--action-groups nil
  "Newest-first list of (CATEGORY START-MARKER END-MARKER).")
(defvar-local ygg--action-pre-point 1)
(defvar-local ygg--action-last-cat nil)
(defvar-local ygg--action-cycle-index 0)

(defvar ygg--action-cat-cache nil)
(defvar ygg--action-cat-cache-key nil)

(defun ygg--action-category (cmd)
  (unless (eq ygg--action-cat-cache-key ygg-action-categories)
    (setq ygg--action-cat-cache-key ygg-action-categories
          ygg--action-cat-cache (make-hash-table :test 'eq))
    (pcase-dolist (`(,cat . ,cmds) ygg-action-categories)
      (dolist (c cmds) (puthash c cat ygg--action-cat-cache))))
  (gethash cmd ygg--action-cat-cache))

(defun ygg--action-push (cat beg end)
  (push (list cat (copy-marker beg) (copy-marker end t)) ygg--action-groups)
  (let ((tail (nthcdr (1- ygg-action-max-groups) ygg--action-groups)))
    (when (cdr tail)
      (dolist (g (cdr tail))
        (set-marker (nth 1 g) nil)
        (set-marker (nth 2 g) nil))
      (setcdr tail nil))))

(defun ygg--action-pre ()
  (setq ygg--action-pre-point (point)))

(defun ygg--action-cursor-gap ()
  ;; set-selection speaks gap positions; point sits ON the cursor cell
  (pcase-let ((`(,beg ,end ,dir) (ygg-selection-effective-bounds)))
    (if (> dir 0) end beg)))

(defun ygg--action-post ()
  (let ((cat (ygg--action-category this-command)))
    (cond
     ((null cat) (setq ygg--action-last-cat nil))
     ((memq cat '(paste replace))
      (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
        (ygg--action-push cat beg end))
      (setq ygg--action-last-cat nil))
     ((and (eq cat ygg--action-last-cat) ygg--action-groups)
      (set-marker (nth 2 (car ygg--action-groups)) (ygg--action-cursor-gap)))
     (t
      (ygg--action-push cat ygg--action-pre-point (ygg--action-cursor-gap))
      (setq ygg--action-last-cat cat)))))

(defun ygg--action-setup ()
  (if yggdrasil-local-mode
      (progn
        (add-hook 'pre-command-hook #'ygg--action-pre nil t)
        (add-hook 'post-command-hook #'ygg--action-post nil t))
    (remove-hook 'pre-command-hook #'ygg--action-pre t)
    (remove-hook 'post-command-hook #'ygg--action-post t)))

(add-hook 'yggdrasil-local-mode-hook #'ygg--action-setup)

(defun ygg--action-group-span (g)
  (let ((beg (marker-position (nth 1 g)))
        (end (marker-position (nth 2 g))))
    (if (= beg end) (cons beg (min (1+ end) (point-max))) (cons beg end))))

(defun ygg-action-cycle ()
  "Select the latest action group's span; repeat to cycle older groups."
  (interactive)
  (setq ygg--action-groups
        (seq-filter (lambda (g) (marker-position (nth 1 g))) ygg--action-groups))
  (unless ygg--action-groups (user-error "No action groups yet"))
  (setq ygg--action-cycle-index
        (if (eq last-command 'ygg-action-cycle)
            (mod (1+ ygg--action-cycle-index) (length ygg--action-groups))
          0))
  (pcase-let* ((g (nth ygg--action-cycle-index ygg--action-groups))
               (`(,beg . ,end) (ygg--action-group-span g)))
    (ygg-set-selection beg end)
    (let (message-log-max)
      (message "action: %s %d/%d" (nth 0 g)
               (1+ ygg--action-cycle-index) (length ygg--action-groups)))))

(defun ygg-action-mark-start ()
  "Pull the selection's anchor back to the current group's start."
  (interactive)
  (let ((g (car ygg--action-groups)))
    (unless (and g (marker-position (nth 1 g)))
      (user-error "No action groups yet"))
    (let ((start (marker-position (nth 1 g))))
      (push-mark start t nil)
      (ygg-set-selection start (ygg--action-cursor-gap)))))

;; ; is vim's repeat-find here, so cycling joins the selections family
(yggdrasil-define-keys 'ygg-selections-map
  "v" #'ygg-action-cycle :label "cycle action groups")

(yggdrasil-define-keys 'normal
  "C-;" #'ygg-action-mark-start :label "mark action start")

(provide 'yggdrasil-actions)
;;; yggdrasil-actions.el ends here
