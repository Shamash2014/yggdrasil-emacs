;;; yggdrasil-leader.el --- Leader key layer -*- lexical-binding: t; -*-

;; Built-ins wrapped: keymaps, which-key label conses, project.el,
;; windmove, restart-emacs, help-command.
;; Custom: leader map, extension API.

;;; Code:

(require 'yggdrasil-core)

(declare-function ygg-qf-buffer-create "layer-quickfix" (&optional list))
(declare-function ygg-window-left "yggdrasil-verbs")
(declare-function ygg-window-down "yggdrasil-verbs")
(declare-function ygg-window-up "yggdrasil-verbs")
(declare-function ygg-window-right "yggdrasil-verbs")
(declare-function ygg-window-zoom-toggle "yggdrasil-verbs")
(declare-function ygg-window-minimize "yggdrasil-verbs")

(defvar ygg-leader-map (make-sparse-keymap)
  "Leader map bound to SPC in normal/visual state and M-SPC globally.")

(defvar ygg-leader-file-map (make-sparse-keymap) "The f prefix: files.")
(defvar ygg-leader-buffer-map (make-sparse-keymap) "The b prefix: buffers.")
(defvar ygg-leader-insert-map (make-sparse-keymap) "The i prefix: insert.")
(defvar ygg-leader-help-map (make-sparse-keymap) "The h prefix: help.")
(defvar ygg-leader-help-keys-map (make-sparse-keymap) "The h B prefix: which-key views.")
(defvar ygg-leader-window-map (make-sparse-keymap) "The w prefix: windows.")
(defvar ygg-leader-quit-map (make-sparse-keymap) "The q prefix: quit.")

(defun yggdrasil-leader-def (key def &optional label)
  "Bind KEY (kbd string like \"f f\") in the leader map; LABEL feeds which-key."
  (define-key ygg-leader-map (kbd key) (if label (cons label def) def)))

(defun yggdrasil-leader--split-below ()
  "Split window below and select the new one."
  (interactive)
  (select-window (split-window-below)))

(defun yggdrasil-leader--split-right ()
  "Split window right and select the new one."
  (interactive)
  (select-window (split-window-right)))

(yggdrasil-define-keys 'normal "SPC" ygg-leader-map :label "leader")
(global-set-key (kbd "M-SPC") ygg-leader-map)

;; the leader and ex command line must also work where yggdrasil states
;; are off: special buffers, dired, magit, and terminals in emacs-mode
;; (jk gets you there); while typing M-SPC / M-x cover it
(autoload 'ygg-ex "yggdrasil-ex" nil t)
(define-key special-mode-map (kbd "SPC") ygg-leader-map)
(define-key special-mode-map (kbd ":") #'ygg-ex)
(with-eval-after-load 'dired
  (define-key (symbol-value 'dired-mode-map) (kbd "SPC") ygg-leader-map)
  (define-key (symbol-value 'dired-mode-map) (kbd ":") #'ygg-ex))
(with-eval-after-load 'magit-section
  (define-key (symbol-value 'magit-section-mode-map) (kbd "SPC") ygg-leader-map))
(with-eval-after-load 'magit-mode
  (define-key (symbol-value 'magit-mode-map) (kbd "SPC") ygg-leader-map))
;; magit keeps SPC and DEL for paging a diff, which is the one place the
;; leader was unreachable; C-f and C-b already page there
(with-eval-after-load 'magit-diff
  (dolist (map '(magit-diff-mode-map magit-revision-mode-map))
    (when (boundp map)
      (define-key (symbol-value map) (kbd "SPC") ygg-leader-map)
      (define-key (symbol-value map) (kbd ":") #'ygg-ex))))

(defun yggdrasil-leader--yank-file-path ()
  "Copy the current buffer's abbreviated file path."
  (interactive)
  (if-let* ((file (or buffer-file-name default-directory)))
      (progn (kill-new (abbreviate-file-name file))
             (message "%s" (abbreviate-file-name file)))
    (user-error "Buffer visits no file")))

(yggdrasil-define-keys 'ygg-leader-file-map
  "s" #'save-buffer :label "save"
  "y" #'yggdrasil-leader--yank-file-path :label "yank path"
  "p" #'project-find-file :label "project file")

(defun yggdrasil-leader--kill-buffer-detach ()
  "Detach any running process, then kill the current buffer without prompting.
Detaching (a terminal's shell/agent) means the \"process is running; kill
anyway?\" prompt never fires."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (set-process-query-on-exit-flag proc nil)
      (delete-process proc)))
  (set-buffer-modified-p nil)
  (kill-current-buffer))

(defun yggdrasil-leader--kill-buffer-force ()
  "Kill the current buffer without prompting, discarding edits."
  (interactive)
  (yggdrasil-leader--kill-buffer-detach))

(defun yggdrasil-leader--kill-buffer ()
  "Kill the current buffer.
A read-only buffer (terminal, special, or view) holds nothing to save, so
kill it outright, detaching any process; otherwise kill with the usual prompts."
  (interactive)
  (if buffer-read-only
      (yggdrasil-leader--kill-buffer-detach)
    (kill-current-buffer)))

(defun yggdrasil-leader--kill-choose (names)
  "Pick one or more buffers by name and kill them (modified files still prompt)."
  (interactive
   (list (completing-read-multiple
          "Kill buffer(s): "
          (seq-remove (lambda (n) (string-prefix-p " " n))
                      (mapcar #'buffer-name (buffer-list))))))
  (dolist (name names)
    (when-let* ((buf (get-buffer name)))
      (with-current-buffer buf (yggdrasil-leader--kill-buffer)))))

(yggdrasil-define-keys 'ygg-leader-buffer-map
  "d" #'yggdrasil-leader--kill-buffer :label "kill"
  "k" #'yggdrasil-leader--kill-choose :label "kill (choose)"
  "n" #'next-buffer :label "next"
  "p" #'previous-buffer :label "previous"
  "s" #'save-buffer :label "save"
  "S" #'save-some-buffers :label "save all"
  "r" #'revert-buffer :label "revert"
  "R" #'rename-buffer :label "rename buffer"
  "l" #'mode-line-other-buffer :label "last buffer"
  "x" #'scratch-buffer :label "scratch"
  "z" #'bury-buffer :label "bury"
  "m" #'bookmark-set :label "bookmark set"
  "M" #'ygg-leader--bookmark-jump :label "bookmark jump")

(autoload 'consult-bookmark "consult" nil t)

(defun ygg-leader--bookmark-jump ()
  "Jump to a bookmark through consult when it is installed."
  (interactive)
  (call-interactively (if (fboundp 'consult-bookmark) #'consult-bookmark #'bookmark-jump)))

(yggdrasil-define-keys 'ygg-leader-help-keys-map
  "b" #'which-key-show-top-level :label "top level"
  "m" #'which-key-show-major-mode :label "major mode"
  "k" #'which-key-show-keymap :label "keymap")

(yggdrasil-define-keys 'ygg-leader-help-map
  "f" #'describe-function :label "function"
  "v" #'describe-variable :label "variable"
  "k" #'describe-key :label "key"
  "F" #'describe-face :label "face"
  "m" #'describe-mode :label "mode"
  "t" #'load-theme :label "theme"
  "p" #'describe-package :label "package"
  "a" #'apropos :label "apropos"
  "i" #'info :label "info"
  "B" ygg-leader-help-keys-map :label "which-key")
(set-keymap-parent ygg-leader-help-map help-map)

(yggdrasil-define-keys 'ygg-leader-window-map
  "s" #'yggdrasil-leader--split-below :label "split below"
  "v" #'yggdrasil-leader--split-right :label "split right"
  "d" #'delete-window :label "delete"
  "o" #'delete-other-windows :label "only"
  "w" #'other-window :label "other"
  "=" #'balance-windows :label "balance"
  "z" #'ygg-window-zoom-toggle :label "zoom (maximize)"
  "-" #'ygg-window-minimize :label "minimize (sliver)"
  "+" #'balance-windows :label "restore share"
  "h" #'ygg-window-left :label "left"
  "j" #'ygg-window-down :label "down"
  "k" #'ygg-window-up :label "up"
  "l" #'ygg-window-right :label "right"
  "H" #'shrink-window-horizontally :label "narrower"
  "L" #'enlarge-window-horizontally :label "wider"
  "J" #'shrink-window :label "shorter"
  "K" #'enlarge-window :label "taller")

(declare-function ygg-qf-ensure-buffer "layer-quickfix")

(defun ygg-quickfix-toggle ()
  "Open the *quickfix* panel and put point in it; if already in it, hide it.
Always the canonical *quickfix* buffer that SPC q c / D / t / v / e feed —
so opening is predictable, never whatever compile or task last ran (push
those in with SPC q e).  Created empty on first open; never refuses.
Stepping through errors is separate: ] q / [ q follow the next-error list."
  (interactive)
  (let* ((buf (if (fboundp 'ygg-qf-ensure-buffer)
                  (ygg-qf-ensure-buffer)
                (ygg-qf-buffer-create)))
         (win (get-buffer-window buf)))
    (cond
     ;; already standing in it → this press means hide
     ((eq win (selected-window)) (delete-window win))
     ;; visible elsewhere → jump to it rather than close it out from under
     (win (select-window win))
     (t (select-window (display-buffer buf))))))

(yggdrasil-define-keys 'ygg-leader-quit-map
  "q" #'save-buffers-kill-terminal :label "quit"
  "r" #'restart-emacs :label "restart")

;;; Discovery: SPC ? — searchable cheatsheet of every yggdrasil binding

(defun ygg--keys-collect (map &optional prefix)
  "Flatten MAP into (KEY LABEL COMMAND) entries, recursing into sub-keymaps.
Bound-symbol keymaps (like `help-command') become one prefix entry
instead of flooding the list with foreign bindings."
  (let (acc)
    (map-keymap
     (lambda (event def)
       (unless (or (consp event) (memq event '(remap menu-bar tool-bar)))
         (let ((key (concat prefix (and prefix " ")
                            (key-description (vector event))))
               label)
           (when (and (consp def) (stringp (car def)))
             (setq label (car def) def (cdr def)))
           (when (and (consp def) (eq (car def) 'menu-item))
             (setq label (or label (cadr def)) def (caddr def)))
           (cond
            ((and (symbolp def) (get def 'ygg-keymap)
                  (keymapp (symbol-value (get def 'ygg-keymap))))
             (setq acc (nconc (ygg--keys-collect
                               (symbol-value (get def 'ygg-keymap)) key)
                              acc)))
            ((and (symbolp def) (keymapp def))
             (push (list key (or label (symbol-name def)) def) acc))
            ((keymapp def)
             (setq acc (nconc (ygg--keys-collect def key) acc)))
            ((or (commandp def) label)
             (push (list key label def) acc))))))
     map)
    acc))

(defun ygg--keys-collect-own (map &optional prefix)
  "Like `ygg--keys-collect' but without the parent keymap's bindings."
  (let ((copy (copy-sequence map)))
    (set-keymap-parent copy nil)
    (ygg--keys-collect copy prefix)))

(defun ygg-keys ()
  "Browse every yggdrasil binding; selecting one describes its command."
  (interactive)
  (let (rows)
    (pcase-dolist (`(,tag . ,map) `(("" . ,ygg-normal-map)
                                    ("[V] " . ,ygg-visual-map)
                                    ("[I] " . ,ygg-insert-map)))
      (dolist (e (ygg--keys-collect-own map))
        (push (cons (concat tag (nth 0 e)) (cdr e)) rows)))
    (setq rows (sort rows (lambda (a b) (string< (car a) (car b)))))
    (let* ((annotate
            (lambda (cand)
              (pcase-let ((`(,label ,cmd) (cdr (assoc cand rows))))
                (concat
                 (propertize " " 'display '(space :align-to 20))
                 (propertize (or label
                                 (and (symbolp cmd) (symbol-name cmd))
                                 "anonymous command")
                             'face 'shadow)
                 (when (and label (symbolp cmd) cmd)
                   (propertize (format "  %s" cmd) 'face 'shadow))))))
           (table (lambda (str pred action)
                    (if (eq action 'metadata)
                        `(metadata (annotation-function . ,annotate)
                                   (category . ygg-key)
                                   (display-sort-function . ,#'identity))
                      (complete-with-action action (mapcar #'car rows) str pred))))
           (choice (completing-read "Key: " table nil t))
           (cmd (cadr (cdr (assoc choice rows)))))
      (if (and (symbolp cmd) (fboundp cmd))
          (describe-function cmd)
        (message "%s: %s" choice (or (car (cdr (assoc choice rows))) "bound"))))))

(defun ygg-tutor ()
  "Open the Yggdrasil tutor — a vimtutor-style hands-on tour of the config.
Practice directly in the buffer; edits never touch the source file."
  (interactive)
  (let ((file (expand-file-name "tutor/yggdrasil-tutor.txt" user-emacs-directory))
        (buf (get-buffer-create "*Yggdrasil Tutor*")))
    (unless (file-readable-p file)
      (user-error "Tutor file missing: %s" file))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (when (fboundp 'yggdrasil-local-mode) (yggdrasil-local-mode 1)))
    (pop-to-buffer-same-window buf)))

(yggdrasil-leader-def "SPC" #'execute-extended-command "M-x")
(yggdrasil-leader-def "?" #'ygg-keys "keys cheatsheet")
(yggdrasil-leader-def ":" #'ygg-ex "ex command")
(autoload 'ygg-ex "yggdrasil-ex" nil t)
(yggdrasil-leader-def "f" ygg-leader-file-map "files")
(yggdrasil-leader-def "b" ygg-leader-buffer-map "buffers")
(yggdrasil-leader-def "*" #'ygg-select-all-word "select all occurrences of word")
(declare-function ygg-select-all-word "yggdrasil-selection")
(yggdrasil-leader-def "%" #'ygg-select-buffer "select whole buffer")
(declare-function ygg-select-buffer "yggdrasil-selection")
(yggdrasil-leader-def "w" ygg-leader-window-map "windows")
(yggdrasil-leader-def "q" ygg-leader-quit-map "quit")
(yggdrasil-leader-def "h" ygg-leader-help-map "help")
(yggdrasil-leader-def "i" ygg-leader-insert-map "insert")
(provide 'yggdrasil-leader)
;;; yggdrasil-leader.el ends here
