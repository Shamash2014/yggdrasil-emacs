;;; layer-quickfix.el --- vim quickfix/loclist on next-error + wgrep -*- lexical-binding: t; -*-

;; Built-ins wrapped: the next-error framework (grep/compile/xref buffers
;; ARE the quickfix list) and flymake. Third-party: wgrep for editing
;; result buffers in place (quicker.nvim edit feel: i to edit, ZZ writes
;; changes back to every file). Quickfix and the Zed-style multibuffer
;; are ONE concept here: the *quickfix* buffer holds context excerpts.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-leader)
(require 'yggdrasil-localleader)
(require 'ygg-ui)

(declare-function wgrep-change-to-wgrep-mode "wgrep")
(declare-function wgrep-finish-edit "wgrep")
(declare-function wgrep-abort-changes "wgrep")
(declare-function compilation-next-error "compile")
(declare-function compilation-previous-error "compile")
(declare-function compilation-display-error "compile")
(declare-function compile-goto-error "compile")
(declare-function xref-query-replace-in-results "xref")
(declare-function xref-change-to-xref-edit-mode "xref")
(declare-function xref-edit-save-changes "xref")
(declare-function flymake--project-diagnostics "flymake")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-buffer "flymake")
(declare-function flymake-diagnostic-beg "flymake")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function ygg-scroll-half-down "yggdrasil-motions")
(declare-function ygg-scroll-half-up "yggdrasil-motions")
(declare-function ygg--verb-regions "yggdrasil-verbs")
(declare-function ygg-ai-why "ygg-ai")
(declare-function ygg-task-root-here "ygg-task" (&optional dir))
(declare-function ygg-ex--line-range-bounds "yggdrasil-ex" (range))

(defvar grep-mode-map)
(defvar compilation-mode-map)
(defvar compilation-minor-mode-map)
(defvar wgrep-mode-map)
(defvar wgrep-auto-save-buffer)
(defvar xref-edit-mode-map)

(when (fboundp 'elpaca)
  (elpaca wgrep
    (setq wgrep-auto-save-buffer t)))

;; Emacs 30 made `grep-mode' a strict 0-arg subr.  Some caller `apply's it
;; WITH a spurious argument on the current buffer (the "#<subr grep-mode>,
;; 1" error).  Running it there would turn an unrelated buffer — a compose,
;; a code file — into grep-mode, breaking its keys (e.g. ZZ).  A mode-setup
;; function is never legitimately called with args, so REFUSE the arg-call
;; (don't corrupt the buffer); real 0-arg grep-mode in the quickfix/grep is
;; untouched.
;; belt-and-suspenders (the display-buffer condition below is the real
;; fix): a call WITH args is always a spurious mis-call — refuse it
;; silently so it can neither error nor turn the current buffer into
;; grep-mode.  Legitimate 0-arg grep-mode is untouched.
(defun ygg-qf--grep-mode-guard (orig &rest args) (unless args (funcall orig)))
(with-eval-after-load 'grep
  (advice-add 'grep-mode :around #'ygg-qf--grep-mode-guard))

;;; Vim keys in every quickfix-ish buffer

;; compilation-minor-mode-map is the shared parent: grep-mode-map derives
;; from it, not from compilation-mode-map
(with-eval-after-load 'compile
  (dolist (map (list compilation-mode-map compilation-minor-mode-map))
    (define-key map "j" #'compilation-next-error)
    (define-key map "k" #'compilation-previous-error)
    (define-key map "o" #'compilation-display-error)
    (define-key map (kbd "C-d") #'ygg-scroll-half-down)
    (define-key map (kbd "C-u") #'ygg-scroll-half-up)))

(with-eval-after-load 'grep
  (define-key grep-mode-map "i" #'wgrep-change-to-wgrep-mode)
  ;; rg -C context rows (file-12-text) open their file too — every row
  ;; is a location, vim-style.  Type 0 (info) keeps j/k error-walk on
  ;; the real matches; the .ext anchor keeps dashy filenames unambiguous
  (add-to-list 'grep-regexp-alist
               '("^\\([^ \t\n]+?\\.[[:alnum:]]+\\)-\\([0-9]+\\)-" 1 2 nil 0)
               t)
  ;; vim quickfix rows (file|lnum col N| text) — curated content is
  ;; formatted this way by `ygg-qf--vimify'; keep them clickable
  (add-to-list 'grep-regexp-alist
               '("^\\([^|\n]+\\)|\\([0-9]+\\)\\(?: col \\([0-9]+\\)\\)?| "
                 1 2 3)
               t))

;;; bqf-style live preview: moving through entries shows the location in
;;; another window (built-in follow mode); p toggles it, off while editing

(declare-function next-error-no-select "simple")
(defvar compilation-context-lines)
(defvar compilation-current-error)

(defvar ygg-qf-current nil
  "The list in hand, one character, or nil for the unnamed one.
Vim has twenty-six registers and this layer had one list, so a second
piece of work could only arrive by destroying the first.")

(defun ygg-qf-name (&optional list)
  "The buffer name of LIST, or of the list in hand."
  (let ((l (or list ygg-qf-current)))
    (if l (format "*quickfix:%c*" l) "*quickfix*")))

(defconst ygg-qf--name-re "\\`\\*quickfix\\(:.\\)?\\*\\'")

(defun ygg-qf-buffer (&optional list)
  (get-buffer (ygg-qf-name list)))

(defun ygg-qf-buffer-create (&optional list)
  (get-buffer-create (ygg-qf-name list)))

(defun ygg-qf-lists ()
  "Every list that exists, the unnamed one first."
  (sort (seq-filter (lambda (b) (string-match-p ygg-qf--name-re (buffer-name b)))
                    (buffer-list))
        (lambda (a b) (< (length (buffer-name a)) (length (buffer-name b))))))

;;;###autoload
(defun ygg-qf-switch (list)
  "Take LIST in hand, the way a register is taken before a yank."
  (interactive
   (list (let ((have (mapconcat #'buffer-name (ygg-qf-lists) " ")))
           (read-char (format "list (SPC for the unnamed one) — have: %s: "
                              (if (string-empty-p have) "none" have))))))
  (setq ygg-qf-current (unless (memq list '(?\s ?\r)) list))
  (message "quickfix: %s" (ygg-qf-name)))

(defcustom ygg-qf-preview-delay 0.12
  "Idle seconds before the quickfix previews the entry at point.
Debounced, so rapid j/k navigation stays instant — the source is visited
once you settle on a row, not on every keystroke."
  :type 'number :group 'yggdrasil)

(defcustom ygg-qf-preview nil
  "Whether a quickfix panel previews the row at point as you move.
Off, a row opens on RET and nothing else moves; p turns the preview on
for the panel in hand."
  :type 'boolean :group 'yggdrasil)

(defvar-local ygg-qf--preview-on nil)
(defvar-local ygg-qf--preview-timer nil)

(declare-function pulse-momentary-highlight-one-line "pulse")

(defvar ygg-qf-kinds nil
  "The kinds of list that are not locations, as (KIND . SPEC).
SPEC is a plist: :collect (a function of the arguments `ygg-qf-show-kind'
got, returning rows of (ID TEXT NOTE)), :action (a function of a row's ID,
run when it is opened), :map (the symbol of the keymap embark offers on a
row), :glyph (the mark in the gutter), :drop (a function of a row's ID that
removes it at its source), :live-p (a function of those arguments, nil once
the list should stop following) and :arm.  :arm is a function of the buffer,
a token and those arguments that calls `ygg-qf-kind-refresh' with the token
whenever the rows may have changed, and returns a function of no arguments
that undoes what it set up.  That one is called once, when another writer
replaces the list, the kind is shown again, or a refresh says the list
stopped following.")

(defvar-local ygg-qf--kind nil
  "How this list follows its kind, as a plist.
:token names this showing, :kind and :args what filled the list and
:disarm undoes the arm.  Nil when no kind owns the list.")

(defvar ygg-qf--act-target nil
  "The embark target a verb of a kind is running on.")

(declare-function ygg-embark-add-map "ygg-embark")
(defvar embark-around-action-hooks)

(defun ygg-qf-kind-type (kind)
  "The embark target type of a row of KIND."
  (intern (format "ygg-qf-%s" kind)))

(defun ygg-qf--kind-embark-around (&rest args)
  "Run an embark verb with the id of the row it was offered on in hand."
  (let ((ygg-qf--act-target (plist-get args :orig-target)))
    (apply (plist-get args :run) args)))

(defun ygg-qf-define-kind (kind &rest spec)
  "Register KIND, a list of rows that are objects, not places in files."
  (setf (alist-get kind ygg-qf-kinds) spec)
  (when-let* ((map (plist-get spec :map)))
    (with-eval-after-load 'ygg-embark
      (with-eval-after-load 'embark
        (ygg-embark-add-map (ygg-qf-kind-type kind) map)))))

(defun ygg-qf-kind-at-point ()
  "The (KIND . ID) of the row at point, or nil off a row of a kind."
  (when-let* ((kind (get-text-property (line-beginning-position) 'ygg-qf-kind)))
    (cons kind (get-text-property (line-beginning-position) 'ygg-qf-id))))

(defun ygg-qf--kind-list-p ()
  "Whether this list holds rows of a kind."
  (text-property-not-all (point-min) (point-max) 'ygg-qf-kind nil))

(defun ygg-qf-kind-list-p (&optional buffer)
  "Whether BUFFER, or this one, holds rows of a kind."
  (with-current-buffer (or buffer (current-buffer))
    (and (ygg-qf--kind-list-p) t)))

(defun ygg-qf--id-by-string (string)
  "The id of the row of any list that reads as STRING, or nil."
  (catch 'found
    (dolist (buf (ygg-qf-lists))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when-let* ((at (ygg-qf-kind-at-point))
                        ((equal (format "%s" (cdr at)) string)))
              (throw 'found (cdr at)))
            (forward-line 1)))))))

(defun ygg-qf-kind-target-id (&optional target)
  "The id of the row an embark TARGET stands for.
TARGET defaults to the one being acted on, then to the row at point.  The
id rides on the target string; once embark has copied the string bare it is
looked up by how it reads."
  (let ((target (or target ygg-qf--act-target)))
    (cond ((and (stringp target) (> (length target) 0)
                (get-text-property 0 'ygg-qf-id target)))
          ((stringp target) (ygg-qf--id-by-string target))
          (t (cdr (ygg-qf-kind-at-point))))))

(defun ygg-qf--row-action ()
  "The function of no arguments the row at point runs in place of a visit."
  (when-let* ((at (ygg-qf-kind-at-point))
              (action (plist-get (alist-get (car at) ygg-qf-kinds) :action)))
    (lambda () (funcall action (cdr at)))))

(defun ygg-qf--row-place ()
  "The row at point as (FILE . LINE), in either shape the list holds them.
Grep writes `file:line:\'; a curated row is vimified to `file|line| \'.
A row of a kind names no place."
  (unless (ygg-qf-kind-at-point)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (when (string-match "\\`\\(.+?\\)[:|]\\([0-9]+\\)\\(?: col [0-9]+\\)?[:|]" line)
        (let ((file (expand-file-name (match-string 1 line) default-directory)))
          (when (file-readable-p file)
            (cons file (string-to-number (match-string 2 line)))))))))

(defun ygg-qf--preview-target ()
  "The window a preview belongs in: the biggest one that is not this list.
Never the panel, a side window, or another quickfix.  Nil when the frame
holds no such window: splitting the panel would make one showing the
panel\'s own buffer, which fails this same test, so the next row splits
again and walking the list eats the frame."
  (let* ((panel (selected-window))
         (win (car (sort (seq-filter
                          (lambda (w)
                            (and (not (eq w panel))
                                 (not (window-parameter w 'window-side))
                                 (not (window-dedicated-p w))
                                 (not (ygg-qf--quickfix-buffer-p
                                       (window-buffer w)))))
                          (window-list nil 'never))
                         (lambda (a b) (> (* (window-width a) (window-height a))
                                          (* (window-width b) (window-height b))))))))
    win))

(defun ygg-qf--preview-window (buffer alist)
  "Put BUFFER in the preview window."
  (when-let* ((win (ygg-qf--preview-target)))
    (window--display-buffer buffer win 'reuse alist)))

(defun ygg-qf--preview-tick (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and ygg-qf--preview-on
                 (derived-mode-p 'grep-mode 'compilation-mode)
                 (get-buffer-window buf))
        (condition-case nil
            ;; the window is chosen here and the buffer put in it directly.
            ;; `next-error' reaches `compilation-goto-locus', which displays
            ;; by paths of its own that no overriding action reliably wins,
            ;; and every row opened a third window
            (when-let* ((place (ygg-qf--row-place))
                        (target (ygg-qf--preview-target))
                        (src (find-file-noselect (car place) t)))
              (set-window-buffer target src)
              (with-selected-window target
                (goto-char (point-min))
                (forward-line (1- (cdr place)))
                ;; land in the middle of the file, not wherever the window
                ;; happened to be scrolled — a preview you have to hunt in
                ;; is worse than none
                (recenter)
                (require 'pulse nil t)
                (when (fboundp 'pulse-momentary-highlight-one-line)
                  (pulse-momentary-highlight-one-line (point)))))
          (error nil))))))

(defun ygg-qf--preview-schedule ()
  (when (timerp ygg-qf--preview-timer) (cancel-timer ygg-qf--preview-timer))
  (setq ygg-qf--preview-timer
        (run-with-idle-timer ygg-qf-preview-delay nil
                             #'ygg-qf--preview-tick (current-buffer))))

(defun ygg-qf-preview-toggle ()
  "Toggle the debounced live preview in this quickfix buffer."
  (interactive)
  (setq ygg-qf--preview-on (not ygg-qf--preview-on))
  (message "quickfix preview %s" (if ygg-qf--preview-on "on" "off")))

(defun ygg-qf--settle ()
  "Drop the mark after a command in a list buffer, outside visual state.
Compilation's motions push and activate it, which paints every row
between there and point as a region; a list is read, not selected."
  (when (and mark-active
             (not (and (boundp 'ygg--visual-p) ygg--visual-p))
             (not (bound-and-true-p rectangle-mark-mode)))
    (deactivate-mark)))

(defun ygg-qf--step (n)
  "Move N entries on, back when N is negative.
A list of a kind has no locations for compilation to walk, so its rows are
found by what they carry."
  (if (not (ygg-qf--kind-list-p))
      (if (< n 0) (compilation-previous-error (- n)) (compilation-next-error n))
    (let ((dir (if (< n 0) -1 1))
          (left (abs n))
          (from (point)))
      (while (and (> left 0) (zerop (forward-line dir)) (not (eobp)))
        (when (get-text-property (line-beginning-position) 'ygg-qf-kind)
          (setq left (1- left))))
      (when (> left 0)
        (goto-char from)
        (user-error "quickfix: no more rows")))))

(defun ygg-qf-next ()
  "Move to the next entry and show it, entries and never lines.
Line motion walks over banners and context rows and leaves the preview
looking at whatever was under it last; this is what a quickfix means by
down."
  (interactive)
  (ygg-qf--step 1)
  (ygg-qf--settle)
  (ygg-qf--preview-tick (current-buffer)))

(defun ygg-qf-prev ()
  "Move to the previous entry and show it."
  (interactive)
  (ygg-qf--step -1)
  (ygg-qf--settle)
  (ygg-qf--preview-tick (current-buffer)))

(defun ygg-qf-open ()
  "Open the entry at point and leave the panel for it."
  (interactive)
  (if-let* ((action (ygg-qf--row-action)))
      (funcall action)
    (let ((display-buffer-overriding-action
           '((display-buffer-reuse-window ygg-qf--preview-window)
             (inhibit-same-window . t))))
      (compile-goto-error)
      (recenter))))

(defun ygg-qf-display ()
  "Show the entry at point and stay in the panel."
  (interactive)
  (if-let* ((action (ygg-qf--row-action)))
      (funcall action)
    (compilation-display-error)))

(defun ygg-qf--goto-error-action (orig &optional event)
  "Run the action of the row clicked or at point, else visit it as ORIG does."
  (when (and event (mouse-event-p event))
    (posn-set-point (event-end event)))
  (if-let* ((action (and (ygg-qf--quickfix-buffer-p (current-buffer))
                         (ygg-qf--row-action))))
      (funcall action)
    (funcall orig event)))

(advice-add 'compile-goto-error :around #'ygg-qf--goto-error-action)

(defun ygg-qf--row-file ()
  "The path the row at point opens with, as written, or nil off a row.
A row of a kind opens no path."
  (when (and (get-text-property (line-beginning-position) 'ygg-qf-row)
             (not (get-text-property (line-beginning-position) 'ygg-qf-kind)))
    (let ((text (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (and (string-match "\\`\\([^|:\n]+\\)[|:]" text)
           (match-string 1 text)))))

(defun ygg-qf--file-step (dir)
  "Move DIR rows at a time until the row names another file than this one."
  (let ((here (ygg-qf--row-file))
        (from (point)))
    (while (and (zerop (forward-line dir))
                (not (bobp))
                (or (not (get-text-property (line-beginning-position) 'ygg-qf-row))
                    (equal (ygg-qf--row-file) here))))
    (unless (and (get-text-property (line-beginning-position) 'ygg-qf-row)
                 (not (equal (ygg-qf--row-file) here)))
      (goto-char from)
      (user-error "quickfix: no other file that way"))))

(defun ygg-qf-next-file ()
  "Move to the first row of the next file in the list."
  (interactive)
  (ygg-qf--file-step 1))

(defun ygg-qf-prev-file ()
  "Move to a row of the previous file in the list."
  (interactive)
  (ygg-qf--file-step -1))

(defun ygg-qf-window-top ()
  "Move to the first row on screen."
  (interactive)
  (move-to-window-line 0)
  (unless (get-text-property (line-beginning-position) 'ygg-qf-row)
    (ignore-errors (ygg-qf-next))))

(defun ygg-qf-window-middle ()
  "Move to the row in the middle of the screen."
  (interactive)
  (move-to-window-line nil))

(defun ygg-qf-window-bottom ()
  "Move to the last row on screen."
  (interactive)
  (move-to-window-line -1)
  (unless (get-text-property (line-beginning-position) 'ygg-qf-row)
    (ignore-errors (ygg-qf-prev))))

(defvar ygg-qf-panel-map
  (let ((m (make-sparse-keymap)))
    (define-key m "j" #'ygg-qf-next)
    (define-key m "k" #'ygg-qf-prev)
    (define-key m "i" #'wgrep-change-to-wgrep-mode)
    (define-key m (kbd "RET") #'ygg-qf-open)
    (define-key m "o" #'ygg-qf-display)
    (define-key m "p" #'ygg-qf-preview-toggle)
    (define-key m "q" #'quit-window)
    (define-key m "g g" #'ygg-qf-first)
    (define-key m "G" #'ygg-qf-last)
    (define-key m "d" #'ygg-qf-drop)
    (define-key m "f" #'ygg-qf-filter)
    (define-key m "F" #'ygg-qf-filter-pop)
    (define-key m "/" #'isearch-forward)
    (define-key m "}" #'ygg-qf-next-file)
    (define-key m "{" #'ygg-qf-prev-file)
    (define-key m "H" #'ygg-qf-window-top)
    (define-key m "M" #'ygg-qf-window-middle)
    (define-key m "L" #'ygg-qf-window-bottom)
    (define-key m (kbd "C-d") #'ygg-scroll-half-down)
    (define-key m (kbd "C-u") #'ygg-scroll-half-up)
    (define-key m (kbd "C-f") #'scroll-up-command)
    (define-key m (kbd "C-b") #'scroll-down-command)
    (define-key m "zz" #'recenter)
    (define-key m "zt" (lambda () (interactive) (recenter 0)))
    (define-key m "zb" (lambda () (interactive) (recenter -1)))
    m)
  "The keys a quickfix panel keeps above the modal states.
Without lifting these, normal state answers j and k with line motion and
the panel stops behaving like a list.")

(yggdrasil-define-mode-keys 'grep-mode 'normal ygg-qf-panel-map)

(defun ygg-qf--enable-preview ()
  (when (derived-mode-p 'grep-mode 'compilation-mode)
    (hl-line-mode 1)
    (setq-local truncate-lines t)
    (setq ygg-qf--preview-on ygg-qf-preview)
    (add-hook 'post-command-hook #'ygg-qf--settle nil t)
    (add-hook 'post-command-hook #'ygg-qf--preview-schedule nil t)))

(add-hook 'grep-mode-hook #'ygg-qf--enable-preview)

(with-eval-after-load 'compile
  (dolist (map (list compilation-mode-map compilation-minor-mode-map))
    (define-key map "p" #'ygg-qf-preview-toggle)))

(add-to-list 'ygg-modal-special-modes 'xref--xref-buffer-mode)
(add-to-list 'ygg-modal-special-mode-keep
             '(xref--xref-buffer-mode ("j" . xref-next-line) ("k" . xref-prev-line)
                                      ("<tab>" . "TAB") ("] f" . "N") ("[ f" . "P")))

(with-eval-after-load 'xref
  (define-key xref-edit-mode-map [remap ygg-save-and-kill-buffer] #'xref-edit-save-changes)
  (add-hook 'xref-edit-mode-hook
            (lambda ()
              (when yggdrasil-local-mode
                (setq ygg--special-keep-map nil)
                (ygg--mode-keys-refresh)
                (ygg-normal-state))))
  (advice-add 'xref-edit-save-changes :after
              (lambda (&rest _)
                (when (derived-mode-p 'xref--xref-buffer-mode)
                  (ygg--modalize-special)))))

(yggdrasil-localleader-def 'xref--xref-buffer-mode "r" #'xref-query-replace-in-results "replace in results")
(yggdrasil-localleader-def 'xref--xref-buffer-mode "e" #'xref-change-to-xref-edit-mode "edit results")

;; wgrep edit sessions get the full modal engine, wdired-style round trip
(with-eval-after-load 'wgrep
  ;; editing text shouldn't yank preview windows around; restore on exit
  (add-hook 'wgrep-setup-hook (lambda () (yggdrasil-local-mode 1)
                                (setq-local ygg-qf--preview-on nil)))
  (advice-add 'wgrep-finish-edit :after (lambda (&rest _) (ygg-normal-state)
                                          (ygg-qf--enable-preview)))
  (advice-add 'wgrep-abort-changes :after (lambda (&rest _) (ygg-normal-state)
                                            (ygg-qf--enable-preview)))
  (define-key wgrep-mode-map [remap ygg-save-and-kill-buffer] #'wgrep-finish-edit)
  (define-key wgrep-mode-map [remap ygg-kill-buffer-no-save] #'wgrep-abort-changes))

;;; Multibuffer = quickfix: one canonical *quickfix* buffer. Search with
;;; context excerpts and diagnostics both land there, SPC q f/l toggles
;;; it, i edits any excerpt, ZZ writes all files back.

(declare-function project-root "project")
(declare-function project-current "project")

(defun ygg-search-multibuffer (pattern)
  "Project-wide ripgrep with context excerpts into the *quickfix* buffer."
  (interactive (list (read-string "multibuffer search: ")))
  (unless (executable-find "rg")
    (user-error "ygg-search-multibuffer needs ripgrep"))
  (let ((default-directory (or (when-let* ((p (project-current)))
                                 (project-root p))
                               default-directory)))
    (compilation-start
     (format "rg -nH --no-heading -C 3 -e %s ." (shell-quote-argument pattern))
     #'grep-mode
     (lambda (_) (ygg-qf-name)))))

(defcustom ygg-qf-todo-keywords '("TODO" "FIXME" "HACK" "XXX" "BUG")
  "Comment keywords `ygg-qf-todos' collects from the project."
  :type '(repeat string) :group 'yggdrasil)

(defun ygg-qf-todos ()
  "Scan the project root for TODO/FIXME-style comments into *quickfix*."
  (interactive)
  (unless (executable-find "rg")
    (user-error "ygg-qf-todos needs ripgrep"))
  (let ((default-directory (or (when-let* ((p (project-current)))
                                 (project-root p))
                               default-directory)))
    (compilation-start
     (format "rg -nH --no-heading -w -e %s ."
             (shell-quote-argument
              (concat "(" (mapconcat #'identity ygg-qf-todo-keywords "|") ")")))
     #'grep-mode
     (lambda (_) (ygg-qf-name)))))

;;; hl-todo — highlight the same keywords SPC q t collects, inline in code

(defvar hl-todo-keyword-faces)
(declare-function global-hl-todo-mode "hl-todo")

(when (fboundp 'elpaca)
  (elpaca hl-todo
    (with-eval-after-load 'hl-todo
      (dolist (kw ygg-qf-todo-keywords)
        (unless (assoc kw hl-todo-keyword-faces)
          (push (cons kw 'hl-todo) hl-todo-keyword-faces))))
    (global-hl-todo-mode 1)))

(with-eval-after-load 'layer-completion
  (yggdrasil-define-keys 'ygg-leader-search-map
    "c" #'ygg-search-multibuffer :label "multibuffer (context)"))

;;; One display rule: the quickfix is ALWAYS a bottom list panel.
;;; Covers *quickfix*, grep/rg, compile results, embark exports —
;;; display-buffer-alist outranks the ACTION argument of any caller.
;;; `*task:…*' jobs are the exception — layer-terminal splits for each.

;; A plain (derived-mode . MODE) condition makes Emacs 30's buffer-match-p
;; *call* grep-mode (a strict 0-arg subr) with the buffer as an argument —
;; "#<subr grep-mode>, 1" — on EVERY display-buffer, corrupting whatever
;; buffer is current.  A function condition sidesteps that entirely.
(defun ygg-qf--quickfix-buffer-p (buffer-or-name &optional _action)
  (let ((b (get-buffer buffer-or-name)))
    (and b
         (not (string-prefix-p "*task:" (buffer-name b)))
         (or (provided-mode-derived-p
              (buffer-local-value 'major-mode b)
              '(grep-mode compilation-mode))
             (string-match-p ygg-qf--name-re (buffer-name b))))))

(add-to-list 'display-buffer-alist
             '(ygg-qf--quickfix-buffer-p
               ;; at-bottom (not a side window) so it spans the FULL frame
               ;; width even when the agent trace holds a right side window
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)
               (preserve-size . (nil . t))
               ;; a panel keeps its size while you walk it, and previews
               ;; open above it rather than inside it
               (window-parameters . ((no-delete-other-windows . t)
                                     (no-other-window . nil)))))

;;; Quickfix: first/last entry (vim [Q / ]Q)

(defun ygg-qf-first ()
  "Move to the first row of this list, opening nothing."
  (interactive)
  (goto-char (point-min))
  (unless (get-text-property (line-beginning-position) 'ygg-qf-row)
    (condition-case nil (ygg-qf--step 1)
      (error (user-error "Quickfix is empty"))))
  (ygg-qf--settle)
  (ygg-qf--preview-tick (current-buffer)))

(defun ygg-qf-last ()
  "Move to the last row of this list, opening nothing."
  (interactive)
  (goto-char (point-max))
  (condition-case nil (ygg-qf--step -1)
    (error (user-error "Quickfix is empty")))
  (ygg-qf--settle)
  (ygg-qf--preview-tick (current-buffer)))

;;; Push into the *quickfix* list: comint/compile errors, or a selection

(declare-function ygg-selection-effective-bounds "yggdrasil-selection")

(defconst ygg-qf--loc-re "^[^ \t\n:][^:\n]*:[0-9]+\\(?::[0-9]+\\)?:"
  "A grep/compiler location line: file:line[:col]:.")

(defun ygg-qf--banner (&optional n subtitle)
  "The banner line atop a *quickfix* buffer: what the list is, and how many.
SUBTITLE is the caller's own title for the list; without one the panel
names itself.  The count sits at the right edge, where a number is read
when it is wanted and skipped when it is not.
Kept as a real first line because compilation treats line 1 as a non-error
header — without it the FIRST entry is unreachable by `first-error' /
`compilation-next-error'."
  (let ((label (and n (> n 0) (number-to-string n))))
    (concat
     (propertize " ▌ " 'face 'ygg-qf-bullet 'ygg-qf-banner t)
     (propertize (or subtitle "quickfix") 'face 'ygg-qf-title)
     (if label
         (concat (propertize " " 'face 'ygg-qf-header
                             'display `(space :align-to (- right ,(+ 1 (length label)))))
                 (propertize label 'face 'ygg-qf-count))
       "")
     (propertize "\n" 'face 'ygg-qf-header))))

(defun ygg-qf--subtitle ()
  "The line the caller titled this list with, when it gave one.
Read back off the buffer rather than remembered, so a list drawn without
a title cannot inherit the last one's."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (when (get-text-property (point) 'ygg-qf-subtitle)
      (string-trim (buffer-substring-no-properties
                    (point) (line-end-position))))))

(defun ygg-qf--claim-subtitle (beg)
  "Fold a leading non-entry line at BEG into the banner.
Whatever fills the list titles it by leading with a line that is not a
location — two rows of chrome over a panel a third of the frame high is
one row too many.  The line stays in the buffer, it just reads from up
in the banner instead of from a row of its own."
  (save-excursion
    (goto-char beg)
    (when (and (get-text-property (point-min) 'ygg-qf-banner)
               (= beg (save-excursion (goto-char (point-min))
                                      (line-beginning-position 2)))
               (not (get-text-property beg 'ygg-qf-row))
               (> (line-end-position) beg))
      (put-text-property beg (line-end-position) 'ygg-qf-subtitle t)
      (put-text-property beg (min (point-max) (1+ (line-end-position)))
                         'invisible t))))

(defun ygg-qf--count-rows ()
  "Count styled entry rows in the current quickfix buffer."
  (let ((n 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (line-beginning-position) 'ygg-qf-row)
          (setq n (1+ n)))
        (forward-line 1)))
    n))

(defun ygg-qf--refresh-banner ()
  "Rewrite the banner line so its count matches the entries below."
  (save-excursion
    (goto-char (point-min))
    (when (get-text-property (point) 'ygg-qf-banner)
      (let* ((inhibit-read-only t)
             (token (get-text-property (point) 'ygg-qf-token))
             (title (ygg-qf--subtitle))
             (queries (append ygg-qf--filters
                              (unless (string-empty-p ygg-qf--filter-query)
                                (list ygg-qf--filter-query))))
             (subtitle (if queries
                           (format "%s / %s" (or title "quickfix")
                                   (string-join queries " / "))
                         title)))
        (delete-region (point) (min (1+ (line-end-position)) (point-max)))
        (insert (ygg-qf--banner (ygg-qf--count-rows) subtitle))
        (when token
          (put-text-property (point-min) (1+ (point-min)) 'ygg-qf-token token))))))

(defun ygg-qf-drop ()
  "Drop the row under point from the list.
Curating is half of what a list is for, and the other way to do it —
wgrep — opens every file the rows point at to remove one of them.  A row
of a kind goes at its source, by the kind's :drop, and the list follows."
  (interactive)
  (unless (get-text-property (line-beginning-position) 'ygg-qf-row)
    (user-error "quickfix: not on a row"))
  (if-let* ((at (ygg-qf-kind-at-point)))
      (let ((drop (plist-get (alist-get (car at) ygg-qf-kinds) :drop)))
        (unless drop (user-error "quickfix: a %s row cannot be dropped" (car at)))
        (funcall drop (cdr at))
        (when ygg-qf--kind
          (ygg-qf-kind-refresh (current-buffer) (plist-get ygg-qf--kind :token))))
    (let ((inhibit-read-only t))
      (delete-region (line-beginning-position)
                     (min (1+ (line-end-position)) (point-max))))
    (ygg-qf--refresh-banner)
    (unless (get-text-property (line-beginning-position) 'ygg-qf-row)
      (ignore-errors (ygg-qf-prev)))))

(defvar-local ygg-qf--filter-source nil
  "The rows the list held before a filter narrowed it, raw text each.
Nil while the list is whole: a filter narrows from here and an empty
query puts them back.")

(defvar-local ygg-qf--filters nil
  "The queries this list stands narrowed by, oldest first.
Each one narrows what the ones before it left, so f stacks a filter on a
filter and F takes the last one off.")

(defvar-local ygg-qf--filter-query ""
  "The query being typed now, over the stacked filters, empty when none.")

(defun ygg-qf--rows-raw ()
  "The rows of this list as raw text, banner and title left out."
  (let (rows)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (line-beginning-position) 'ygg-qf-row)
          (push (if (ygg-qf-kind-at-point)
                    (buffer-substring (line-beginning-position) (line-end-position))
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))
                rows))
        (forward-line 1)))
    (nreverse rows)))

(defun ygg-qf--rows-start ()
  "Where the rows of this list begin, or point-max when it has none."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (get-text-property (line-beginning-position) 'ygg-qf-row)))
      (forward-line 1))
    (point)))

(defun ygg-qf-filter-terms (query)
  "QUERY as a list of terms, each a cons of whether it excludes and its text.
Terms are split on blanks; one opening with ! names what a row must not
say.  A term is a regexp when it reads as one and plain text otherwise."
  (mapcar (lambda (word)
            (if (and (> (length word) 1) (string-prefix-p "!" word))
                (cons t (substring word 1))
              (cons nil word)))
          (split-string query nil t)))

(defun ygg-qf--term-hit-p (term row)
  "Whether the text of TERM is found in ROW, regardless of case."
  (let ((case-fold-search t))
    (condition-case nil
        (string-match-p term row)
      (invalid-regexp (string-search (downcase term) (downcase row))))))

(defun ygg-qf-filter-match-p (row terms)
  "Whether ROW says every plain term of TERMS and none of the excluded."
  (seq-every-p (lambda (term)
                 (let ((hit (ygg-qf--term-hit-p (cdr term) row)))
                   (if (car term) (not hit) hit)))
               terms))

(defun ygg-qf--refill (rows)
  "Make ROWS the rows of this list, restyled, banner and title kept."
  (let ((inhibit-read-only t)
        (start (ygg-qf--rows-start)))
    (remove-overlays start (point-max) 'ygg-qf t)
    (setq ygg-qf--context-drawn 0)
    (delete-region start (point-max))
    (goto-char start)
    (dolist (row rows) (insert row "\n"))
    (ygg-qf--style-rows start)
    (ygg-qf--style-grep start (point-max))
    (ygg-qf--refresh-banner)
    (goto-char (point-min))
    (ignore-errors (ygg-qf--step 1))))

(defun ygg-qf--filtered-rows (queries)
  "The rows of the whole list that every query of QUERIES keeps."
  (let ((rows ygg-qf--filter-source))
    (dolist (query queries)
      (when-let* ((terms (ygg-qf-filter-terms query)))
        (setq rows (seq-filter (lambda (row) (ygg-qf-filter-match-p row terms))
                               rows))))
    rows))

(defun ygg-qf--filter-apply (query)
  "Show the rows the stacked filters and QUERY keep, from the whole list.
QUERY is the one being typed; it is not on the stack until it is kept."
  (unless ygg-qf--filter-source
    (setq ygg-qf--filter-source (ygg-qf--rows-raw)))
  (setq ygg-qf--filter-query (string-trim query))
  (ygg-qf--refill (ygg-qf--filtered-rows
                   (append ygg-qf--filters (list ygg-qf--filter-query))))
  (when (and (null ygg-qf--filters) (string-empty-p ygg-qf--filter-query))
    (setq ygg-qf--filter-source nil)))

(defun ygg-qf--filter-keep (query)
  "Put QUERY on the stack of filters and show what they keep together."
  (ygg-qf--filter-apply query)
  (unless (string-empty-p ygg-qf--filter-query)
    (setq ygg-qf--filters (append ygg-qf--filters (list ygg-qf--filter-query))))
  (setq ygg-qf--filter-query "")
  (ygg-qf--refresh-banner))

(defun ygg-qf-filter-pop ()
  "Take the last filter off the stack; with none left the list is whole."
  (interactive)
  (unless ygg-qf--filters (user-error "quickfix: no filter to take off"))
  (setq ygg-qf--filters (butlast ygg-qf--filters))
  (ygg-qf--filter-apply ""))

(defun ygg-qf-filters ()
  "The filters the list in hand stands narrowed by, oldest first."
  (when-let* ((buffer (ygg-qf-buffer)))
    (buffer-local-value 'ygg-qf--filters buffer)))

(defun ygg-qf-filter (query)
  "Narrow the list to the rows QUERY keeps, as the query is typed.
Every blank-separated term must be found in a row, over its path and
its text alike, and a term opening with ! must not be: !mock leaves
the mock files out.  RET keeps the filter and a second f narrows what it
left, F takes the last one off, and C-g puts back what stood before."
  (interactive
   (let ((panel (current-buffer)))
     (unless (ygg-qf--quickfix-buffer-p panel)
       (user-error "quickfix: not a quickfix buffer"))
     (list
      (condition-case nil
          (minibuffer-with-setup-hook
              (lambda ()
                (add-hook 'after-change-functions
                          (lambda (&rest _)
                            (let ((text (minibuffer-contents-no-properties)))
                              (when (buffer-live-p panel)
                                (with-current-buffer panel
                                  (ygg-qf--filter-apply text)))))
                          nil t))
            (read-string "filter (!term excludes): " ygg-qf--filter-query))
        (quit (when (buffer-live-p panel)
                (with-current-buffer panel (ygg-qf--filter-apply "")))
              (signal 'quit nil))))))
  (ygg-qf--filter-keep query))

(defun ygg-qf-row-location (row)
  "ROW, whichever shape it wears, as FILE:LINE:COL: TEXT, or nil."
  (cond ((string-match "\\`\\([^|\n]+\\)|\\([0-9]+\\)\\(?: col \\([0-9]+\\)\\)?| ?\\(.*\\)\\'" row)
         (format "%s:%s:%s: %s" (match-string 1 row) (match-string 2 row)
                 (or (match-string 3 row) "1") (match-string 4 row)))
        ((string-match "\\`\\([^:\n]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?[ \t]*\\(.*\\)\\'" row)
         (format "%s:%s:%s: %s" (match-string 1 row) (match-string 2 row)
                 (or (match-string 3 row) "1") (match-string 4 row)))))

(defun ygg-qf-locations (&optional list)
  "The rows of LIST, or of the list in hand, as FILE:LINE:COL: TEXT.
What stands after the filters, since those are the rows on show; a
buffer that was filled by hand, with no styled row, is read line by line."
  (when-let* ((buffer (ygg-qf-buffer list)))
    (with-current-buffer buffer
      (delq nil (mapcar (lambda (row)
                          (unless (get-text-property 0 'ygg-qf-kind row)
                            (ygg-qf-row-location row)))
                        (or (ygg-qf--rows-raw)
                            (split-string (buffer-substring-no-properties
                                           (point-min) (point-max))
                                          "\n" t)))))))

(defun ygg-qf-ensure-buffer ()
  "Return the list in hand, creating an empty one if it has none.
Lets `ygg-quickfix-toggle' open the panel before anything has fed it."
  (or (ygg-qf-buffer)
      (let ((buf (ygg-qf-buffer-create)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (unless (derived-mode-p 'grep-mode) (grep-mode))
            (when (= (point-min) (point-max)) (insert (ygg-qf--banner)))))
        (setq next-error-last-buffer buf))))

(defun ygg-qf--vimify (line dir)
  "Recast grep-format LINE (file:lnum[:col]:text) into vim quickfix style
\(relpath|lnum col N| text), relative to DIR — absolute only when the
file sits outside DIR, so clicking still resolves.  Other lines pass."
  (if (string-match
       "\\`\\(.+?\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?[ \t]*\\(.*\\)\\'" line)
      (let* ((file (match-string 1 line))
             (lnum (match-string 2 line))
             (col (match-string 3 line))
             (text (match-string 4 line))
             (short (if (string-prefix-p (expand-file-name dir)
                                         (expand-file-name file))
                        (file-relative-name file dir)
                      file)))
        (format "%s|%s%s| %s" short lnum
                (if col (concat " col " col) "") text))
    line))

(defun ygg-qf--fit-path (path width)
  "PATH's tail: as many whole segments as fit WIDTH, the rest elided `…/'.
Cutting a directory mid-word to buy three columns leaves something that
reads as a different name, so only whole directories go; a file name too
long even alone loses its middle, which is where a name says least.  The
full PATH stays in the buffer for clicking."
  (let* ((parts (seq-remove (lambda (s) (equal s ".")) (split-string path "/" t)))
         (shown (or (car (last parts)) path))
         (rest (cdr (nreverse parts))))
    (when (> (string-width shown) width)
      (let* ((keep (max 1 (1- width)))
             (head (/ (1+ keep) 2)))
        (setq shown (concat (substring shown 0 head) "…"
                            (substring shown (- (length shown) (- keep head))))
              rest nil)))
    (while (and rest
                (<= (+ (string-width (car rest)) 1 (string-width shown)
                       (if (cdr rest) 2 0))
                    width))
      (setq shown (concat (car rest) "/" shown) rest (cdr rest)))
    (if rest (concat "…/" shown) shown)))

;; `defface' is a no-op for a face this session already defined, so a
;; reload would keep whatever colours the last version handed out; forget
;; them first, the way the leader maps take their keys back.
(dolist (f '(ygg-qf-header ygg-qf-title ygg-qf-count ygg-qf-bullet
             ygg-qf-dir ygg-qf-loc ygg-qf-lnum ygg-qf-context))
  (put f 'face-defface-spec nil))

;; Inheriting the stock faces put the panel's chrome ABOVE its content on
;; this palette: `header-line' is lighter than `hl-line' (the banner
;; outshone the row you are on), `highlight' made the count a teal block,
;; and the near-monochrome overrides flatten `font-lock-constant-face' and
;; `shadow' to within a shade of body text — a line number with no colour
;; and a path as loud as the match.  These name the palette directly so
;; the hierarchy holds: text strongest, path quiet, number in bold.
(defface ygg-qf-header
  '((((background dark)) :background "#121212" :extend t)
    (t :background "#efebe3" :extend t))
  "The strip the quickfix title sits on, a shade under `hl-line' so the
row you are on stays the strongest thing in the panel." :group 'yggdrasil)
(defface ygg-qf-title
  '((((background dark)) :inherit ygg-qf-header :foreground "#e0e0e0" :weight bold)
    (t :inherit ygg-qf-header :foreground "#000000" :weight bold))
  "What the list is: the question that filled it, else `quickfix'."
  :group 'yggdrasil)
(defface ygg-qf-count
  '((((background dark)) :inherit ygg-qf-header :foreground "#8a8a8a")
    (t :inherit ygg-qf-header :foreground "#474747"))
  "How many entries the list holds, off in its corner." :group 'yggdrasil)
(defface ygg-qf-bullet '((t :inherit ygg-qf-title))
  "The bar opening the title line, in the title's ink." :group 'yggdrasil)
(defface ygg-qf-dir
  '((((background dark)) :foreground "#7a7a7a" :underline nil)
    (t :foreground "#5c5a55" :underline nil))
  "Directories in a quickfix row — context, not the answer."
  :group 'yggdrasil)
(defface ygg-qf-loc
  '((((background dark)) :foreground "#969696" :underline nil)
    (t :foreground "#3f3f3f" :underline nil))
  "The file name in a quickfix row." :group 'yggdrasil)
(defface ygg-qf-lnum
  '((((background dark)) :foreground "#e0e0e0" :weight bold :underline nil)
    (t :foreground "#141414" :weight bold :underline nil))
  "The line number: the one bold thing in a grey panel, so the eye drops
down that column instead of reading the gutter." :group 'yggdrasil)

(defcustom ygg-qf-loc-width 26
  "Width of the path column in the quickfix gutter.
Wide enough for the tail of a path, narrow enough that the text it
points at still gets most of the line."
  :type 'natnum :group 'yggdrasil)

(defconst ygg-qf--lnum-width 4 "Width of the line-number column.")

(defvar ygg-qf--prev-path nil
  "The path of the row above, while a panel is being styled.
A run of hits in one file says that file's name once — repeating it down
twenty rows is noise you read past to get at the line numbers.")

(defun ygg-qf--pad-left (text width)
  "TEXT right-aligned in WIDTH — the line-number column, so digits of
different lengths still end under one another."
  (let ((w (string-width text)))
    (cond ((= w width) text)
          ((< w width) (concat (make-string (- width w) ?\s) text))
          (t (concat "…" (substring text (- (length text) (1- width))))))))

(defcustom ygg-qf-context-rows 400
  "How many rows of a list get the source line read beside them.
Past this the rows carry only their own text, so a list of thousands
is drawn without reading thousands of files."
  :type 'natnum :group 'yggdrasil)

(defvar-local ygg-qf--file-lines (make-hash-table :test #'equal)
  "Lines of the files this list points at, read once per fill.")

(defvar-local ygg-qf--context-drawn 0
  "How many rows of this list carry their source line.")

(defface ygg-qf-context
  '((((background dark)) :foreground "#7a7a7a")
    (t :foreground "#5c5a55"))
  "The source line drawn after a row, the context the row sits in."
  :group 'yggdrasil)

(defun ygg-qf--file-line (path n)
  "Line N of PATH, trimmed, or nil when the file cannot be read.
The file is read once for the list and its lines kept."
  (let* ((file (expand-file-name path))
         (lines (or (gethash file ygg-qf--file-lines)
                    (puthash file
                             (if (and (file-regular-p file) (file-readable-p file))
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (vconcat (split-string (buffer-string)
                                                          "\n")))
                               [])
                             ygg-qf--file-lines))))
    (when (and (> n 0) (<= n (length lines)))
      (string-trim (aref lines (1- n))))))

(defun ygg-qf--context (from to path line)
  "Draw the source LINE of PATH after the row FROM..TO when it adds to
the row: a row that already reads as its line gets nothing."
  (when (< ygg-qf--context-drawn ygg-qf-context-rows)
    (let* ((own (string-trim (buffer-substring-no-properties
                              to (save-excursion (goto-char from)
                                                 (line-end-position)))))
           (source (ygg-qf--file-line path (string-to-number line))))
      (when (and source (not (string-empty-p source))
                 (not (string-search source own))
                 (not (string-search own source)))
        (setq ygg-qf--context-drawn (1+ ygg-qf--context-drawn))
        (let ((overlay (make-overlay (save-excursion (goto-char from)
                                                     (line-end-position))
                                     (save-excursion (goto-char from)
                                                     (line-end-position)))))
          (overlay-put overlay 'ygg-qf t)
          (overlay-put overlay 'after-string
                       (propertize (concat "   " (ygg-ui-cut source 80))
                                   'face 'ygg-qf-context)))))))

(defun ygg-qf--decorate (from to path line)
  "Recast a row as a quiet gutter and the text it points at.
The raw file and line prefix is hidden behind as much of the path as
fits and its line number, both right-aligned so the text starts at one
column and the eye can run down it.  A file repeated from the row above
is blank: saying it twenty times is noise you read past.  No rules, no
bars: grep underlines the location it parsed, and every part of the
gutter says :underline nil so the display it wears inherits none of it.
Only the display changes, so grep still parses the row and jumps to it."
  (put-text-property from (1+ from) 'ygg-qf-row t)
  (let* ((same (equal path ygg-qf--prev-path))
         (fit (if same "" (ygg-qf--fit-path path ygg-qf-loc-width)))
         (cut (string-match "/[^/]*\\'" fit))
         (dir (if cut (substring fit 0 (1+ cut)) ""))
         (base (if cut (substring fit (1+ cut)) fit)))
    (put-text-property
     from to 'display
     (concat (propertize (make-string (max 0 (- ygg-qf-loc-width
                                                 (string-width fit)))
                                      ?\s)
                         'face '(:underline nil))
             (propertize dir 'face 'ygg-qf-dir)
             (propertize base 'face 'ygg-qf-loc)
             (propertize " " 'face '(:underline nil))
             (propertize (ygg-qf--pad-left line ygg-qf--lnum-width)
                         'face 'ygg-qf-lnum)
             (propertize "  " 'face '(:underline nil))))
    (ygg-qf--context from to path line)
    (setq ygg-qf--prev-path path)))

(defun ygg-qf--style-kind (beg)
  "Hang the note of every row of a kind from BEG on at the right margin."
  (save-excursion
    (goto-char beg)
    (while (not (eobp))
      (let ((bol (line-beginning-position)))
        (when-let* (((get-text-property bol 'ygg-qf-kind))
                    (note (get-text-property bol 'ygg-qf-note)))
          (let ((overlay (make-overlay (line-end-position) (line-end-position))))
            (overlay-put overlay 'ygg-qf t)
            (overlay-put overlay 'after-string
                         (concat (propertize
                                  " " 'display
                                  `(space :align-to (- right ,(1+ (string-width note)))))
                                 (propertize note 'face 'shadow))))))
      (forward-line 1))))

(defun ygg-qf--style-rows (beg)
  "Style curated `file|line col N| text' rows from BEG (quicker.nvim look)."
  (setq ygg-qf--prev-path nil)
  (save-excursion
    (goto-char beg)
    (while (< (point) (point-max))
      (let ((bol (line-beginning-position))
            (lend (line-end-position)))
        (when (not (or (get-text-property bol 'ygg-qf-banner)
                       (get-text-property bol 'ygg-qf-kind)))
          (goto-char bol)
          (when (re-search-forward
                 "^\\([^|\n]+\\)|\\([0-9]+\\)\\(?: col [0-9]+\\)?| " lend t)
            (ygg-qf--decorate bol (match-end 0)
                              (match-string-no-properties 1)
                              (match-string-no-properties 2))))
        (goto-char bol))
      (forward-line 1)))
  (ygg-qf--style-kind beg)
  (ygg-qf--claim-subtitle beg))

(defun ygg-qf--style-grep (beg end &optional require-hit)
  "Style grep-native `file:line[:col]:' rows in [BEG,END) the same way.
Display-only over the native prefix, so compilation's own position props
survive and rows stay clickable.  With REQUIRE-HIT, only lines grep marked
as locations (a `mouse-face') are touched — this skips grep's own header."
  (setq ygg-qf--prev-path nil)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((bol (line-beginning-position))
            (lend (line-end-position)))
        (when (and (not (get-text-property bol 'ygg-qf-row))
                   (not (get-text-property bol 'ygg-qf-banner)))
          (goto-char bol)
          (when (and (re-search-forward
                      "^\\([^ \t\n:][^:\n]*\\):\\([0-9]+\\):\\(?:[0-9]+:\\)?" lend t)
                     (or (not require-hit)
                         (text-property-not-all bol (match-end 0) 'mouse-face nil)))
            (ygg-qf--decorate bol (match-end 0)
                              (match-string-no-properties 1)
                              (match-string-no-properties 2))))
        (goto-char bol))
      (forward-line 1))))

(defun ygg-qf--style-on-finish (buffer _status)
  "Give a finished *quickfix* grep the same styled rows as curated pushes."
  (when (and (buffer-live-p buffer)
             (ygg-qf--quickfix-buffer-p buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (ygg-qf--style-grep (point-min) (point-max) t)))))

(add-hook 'compilation-finish-functions #'ygg-qf--style-on-finish)

(defun ygg-qf--style-on-finish-keys (buffer _status)
  "Give a finished compile that is not a grep the panel's keys too."
  (when (and (buffer-live-p buffer)
             (ygg-qf--quickfix-buffer-p buffer))
    (with-current-buffer buffer
      (unless (derived-mode-p 'grep-mode)
        (yggdrasil-define-local-keys 'normal ygg-qf-panel-map)))))

(add-hook 'compilation-finish-functions #'ygg-qf--style-on-finish-keys)

(defun ygg-qf--disown ()
  "End the claim a kind has on this list, undoing what its arm set up."
  (when ygg-qf--kind
    (let ((disarm (plist-get ygg-qf--kind :disarm)))
      (setq ygg-qf--kind nil)
      (when (eq next-error-function #'ygg-qf--kind-next-error)
        (setq next-error-function #'compilation-next-error-function))
      (when disarm (funcall disarm)))))

(defun ygg-qf-reset ()
  "Empty this list for a writer that fills it from scratch.
Every writer that erases a list calls this first, so no kind goes on
following rows that are no longer there."
  (ygg-qf--disown)
  (let ((inhibit-read-only t))
    (remove-overlays (point-min) (point-max) 'ygg-qf t)
    (clrhash ygg-qf--file-lines)
    (setq ygg-qf--context-drawn 0)
    (setq ygg-qf--filter-source nil ygg-qf--filters nil
          ygg-qf--filter-query "")
    (erase-buffer)))

(defun ygg-qf--collect (lines &optional replace list)
  "Append LINES (grep-format strings) to LIST, or to the one in hand.
With REPLACE, clear it first."
  (unless lines (user-error "Nothing to push"))
  (let ((dir default-directory)
        (buf (ygg-qf-buffer-create list)))
    (with-current-buffer buf
      (let ((inhibit-read-only t) (start nil) (fresh nil))
        (ygg-qf--disown)
        (unless (derived-mode-p 'grep-mode) (grep-mode))
        ;; the location hangs off the right margin; a wrapped row would
        ;; carry it somewhere it means nothing
        (setq-local truncate-lines t)
        (setq default-directory dir)
        (when replace (ygg-qf-reset))
        (setq fresh (= (point-min) (point-max)))
        (when fresh (insert (ygg-qf--banner)))
        (goto-char (point-max))
        (setq start (point))
        (dolist (l lines)
          (insert (ygg-qf--vimify l dir) "\n"))
        (ygg-qf--style-rows start)
        (ygg-qf--refresh-banner)
        ;; a list drawn from scratch opens at its first entry: point left at
        ;; the end scrolls the panel past its own banner to a blank line,
        ;; and the hit you would read first is the one off the top
        (when fresh
          (goto-char (point-min))
          (ignore-errors (compilation-next-error 1)))))
    (setq next-error-last-buffer buf)
    (select-window (display-buffer buf))))

;;; Kinds — lists of objects (subagents, todos…) that open by their action

(defconst ygg-qf--kind-prefix ":| "
  "What every row of a kind opens with, shown as its glyph.
None of the shapes grep, a curated row or a context row reads a location
from can begin with it, so no text after it is taken for one.")

(defun ygg-qf--kind-rows (kind args)
  "The rows of KIND for ARGS, each carrying its kind, id and note."
  (let* ((spec (alist-get kind ygg-qf-kinds))
         (glyph (propertize (concat (or (plist-get spec :glyph) "▪") " ")
                            'face 'ygg-qf-bullet)))
    (mapcar
     (pcase-lambda (`(,id ,text ,note))
       (let ((row (propertize
                   (concat ygg-qf--kind-prefix
                           (replace-regexp-in-string "[\n\0]" " " text))
                   'ygg-qf-kind kind 'ygg-qf-id id 'ygg-qf-note note
                   'ygg-qf-row t)))
         (put-text-property 0 (length ygg-qf--kind-prefix) 'display glyph row)
         row))
     (apply (plist-get spec :collect) args))))

(defun ygg-qf--kind-key (row)
  "What of ROW decides whether a redraw would show anything new."
  (list (substring-no-properties row)
        (get-text-property 0 'ygg-qf-kind row)
        (get-text-property 0 'ygg-qf-id row)
        (get-text-property 0 'ygg-qf-note row)))

(defun ygg-qf--kind-same-p (rows)
  "Whether ROWS are the rows the list already holds."
  (equal (mapcar #'ygg-qf--kind-key rows)
         (mapcar #'ygg-qf--kind-key (or ygg-qf--filter-source (ygg-qf--rows-raw)))))

(defun ygg-qf--place-at (pos)
  "Where POS stands, as the id of its row and the index of its line."
  (save-excursion
    (goto-char pos)
    (cons (cdr (ygg-qf-kind-at-point)) (1- (line-number-at-pos pos)))))

(defun ygg-qf--place-pos (place)
  "The start of the row PLACE names, else of the row that took its line."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (car place) (not found) (not (eobp)))
        (if (equal (car place) (cdr (ygg-qf-kind-at-point)))
            (setq found t)
          (forward-line 1)))
      (unless found
        (goto-char (point-min))
        (forward-line (min (cdr place) (max 0 (1- (count-lines (point-min) (point-max))))))
        (unless (get-text-property (line-beginning-position) 'ygg-qf-kind)
          (or (ignore-errors (ygg-qf--step 1)) (ignore-errors (ygg-qf--step -1)))))
      (line-beginning-position))))

(defun ygg-qf--kind-redraw (rows)
  "Make ROWS the rows on show, each window keeping its row and its scroll."
  (let ((saved (mapcar (lambda (w)
                         (list w (ygg-qf--place-at (window-point w))
                               (1- (line-number-at-pos (window-start w)))))
                       (get-buffer-window-list nil nil t)))
        (here (ygg-qf--place-at (point))))
    (if ygg-qf--filter-source
        (progn (setq ygg-qf--filter-source rows)
               (ygg-qf--refill (ygg-qf--filtered-rows
                                (append ygg-qf--filters (list ygg-qf--filter-query)))))
      (ygg-qf--refill rows))
    (pcase-dolist (`(,w ,at ,top) saved)
      (set-window-start w (save-excursion
                            (goto-char (point-min))
                            (forward-line (min top (max 0 (1- (count-lines
                                                               (point-min) (point-max))))))
                            (point))
                        t)
      (set-window-point w (ygg-qf--place-pos at)))
    (goto-char (ygg-qf--place-pos here))))

(defun ygg-qf--kind-owned-p (token)
  "Whether the rows on show are still the ones TOKEN's showing drew."
  (and (eq token (plist-get ygg-qf--kind :token))
       (eq token (save-restriction
                   (widen)
                   (get-text-property (point-min) 'ygg-qf-token)))))

(defun ygg-qf--kind-next-error (n &optional reset)
  "Run the action of the row N rows on, from the top with RESET.
This is the list's `next-error-function' while a kind fills it."
  (if (not (ygg-qf--kind-list-p))
      (progn (setq next-error-function #'compilation-next-error-function)
             (compilation-next-error-function n reset))
    (when reset (goto-char (point-min)))
    (unless (zerop n) (ygg-qf--step n))
    (dolist (w (get-buffer-window-list nil nil t))
      (set-window-point w (point)))
    (funcall (or (ygg-qf--row-action) (user-error "quickfix: not on a row")))))

(defun ygg-qf-show-kind (kind &rest args)
  "Fill the list in hand with KIND's rows for ARGS, select it and keep it current.
Whatever kind filled the list before stops following it."
  (let* ((spec (or (alist-get kind ygg-qf-kinds)
                   (user-error "quickfix: no kind called %s" kind)))
         (rows (ygg-qf--kind-rows kind args)))
    (unless rows (user-error "quickfix: nothing to list as %s" kind))
    (let ((dir default-directory)
          (buf (ygg-qf-buffer-create))
          (token (list kind)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (ygg-qf-reset)
          (unless (derived-mode-p 'grep-mode) (grep-mode))
          (setq default-directory dir)
          (setq-local truncate-lines t)
          (insert (ygg-qf--banner))
          (put-text-property (point-min) (1+ (point-min)) 'ygg-qf-token token)
          (let ((start (point)))
            (dolist (row rows) (insert row "\n"))
            (ygg-qf--style-rows start))
          (ygg-qf--refresh-banner)
          (goto-char (point-min))
          (ygg-qf--step 1))
        (setq next-error-function #'ygg-qf--kind-next-error)
        (add-hook 'kill-buffer-hook #'ygg-qf--disown nil t)
        (setq ygg-qf--kind (list :token token :kind kind :args args))
        (when-let* ((arm (plist-get spec :arm)))
          (plist-put ygg-qf--kind :disarm (apply arm buf token args))))
      (setq next-error-last-buffer buf)
      (select-window (display-buffer buf)))))

(defun ygg-qf-kind-refresh (buf token)
  "Bring BUF up to date with the kind that filled it, while TOKEN is its showing.
Non-nil while the list still follows that kind.  Nil, with BUF left as it
stands, when TOKEN is not the showing any more, whether another writer
replaced the rows or the kind was shown again; nil as well, and the arm
undone, when the kind says it has nothing left to follow."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and token (eq token (plist-get ygg-qf--kind :token)))
        (let* ((kind (plist-get ygg-qf--kind :kind))
               (args (plist-get ygg-qf--kind :args))
               (live (plist-get (alist-get kind ygg-qf-kinds) :live-p)))
          (if (not (and (ygg-qf--kind-owned-p token)
                        (or (null live) (apply live args))))
              (progn (ygg-qf--disown) nil)
            (let ((inhibit-read-only t)
                  (rows (ygg-qf--kind-rows kind args)))
              (unless (ygg-qf--kind-same-p rows)
                (ygg-qf--kind-redraw rows)))
            t))))))

(defun ygg-qf--lines (beg end filter)
  "Lines between BEG and END; when FILTER, only file:line locations."
  (let (lines)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((l (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
          (when (and (not (string-empty-p (string-trim l)))
                     (or (not filter) (string-match-p ygg-qf--loc-re l)))
            (push l lines)))
        (forward-line 1)))
    (nreverse lines)))

(defun ygg-qf-from-comint ()
  "Push the current buffer's error locations into *quickfix* (after a run)."
  (interactive)
  (ygg-qf--collect (ygg-qf--lines (point-min) (point-max) t)))

;;; Diagnostics → quickfix (project-wide when flymake knows the project)

(defun ygg--diag-qf-line (diag)
  (let* ((buf (flymake-diagnostic-buffer diag))
         (file (or (and (bufferp buf) (buffer-file-name buf))
                   (and (stringp buf) buf)))
         (pos (flymake-diagnostic-beg diag)))
    (when (and file (number-or-marker-p pos))
      (with-current-buffer (if (bufferp buf) buf (find-file-noselect file))
        (save-excursion
          (goto-char pos)
          (format "%s:%d:%d: %s"
                  (file-relative-name file)
                  (line-number-at-pos)
                  (1+ (current-column))
                  (car (split-string (flymake-diagnostic-text diag) "\n"))))))))

(defun ygg-qf-diagnostics ()
  "Dump flymake diagnostics into a navigable *quickfix* buffer.
Project-wide when flymake has project state, else the current buffer's."
  (interactive)
  (require 'flymake)
  (let* ((diags (or (and (fboundp 'flymake--project-diagnostics)
                         (ignore-errors (flymake--project-diagnostics)))
                    (and (bound-and-true-p flymake-mode) (flymake-diagnostics))))
         (lines (delq nil (mapcar #'ygg--diag-qf-line diags))))
    (unless lines (user-error "No diagnostics"))
    (let ((dir default-directory)
          (buf (ygg-qf-buffer-create)))
      (with-current-buffer buf
        (unless (derived-mode-p 'grep-mode) (grep-mode))
        (let ((inhibit-read-only t))
          (setq default-directory dir)
          (ygg-qf-reset)
          (insert (ygg-qf--banner))
          (let ((start (point)))
            (insert (mapconcat #'identity lines "\n") "\n")
            (ygg-qf--style-grep start (point-max)))
          (ygg-qf--refresh-banner)
          (goto-char (point-min))
          (ignore-errors (compilation-next-error 1))))
      (setq next-error-last-buffer buf)
      (select-window (display-buffer buf '((display-buffer-at-bottom)))))))

;;; Text into the list: anything that can print file:line answers to it

(defconst ygg-qf--text-loc-re
  "\\`\\([^ \t\n:][^:\n]*\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?:[ \t]*\\(.*\\)\\'"
  "A location line a command printed: FILE:LINE:COL: REST, or no column.")

(defun ygg-qf--text-row (line)
  "LINE as a location row with an absolute file, or nil when it is neither.
The file is resolved against default-directory here, where the caller's
directory is still in hand, so a row cannot be read against another one
later."
  (let ((line (string-trim-right line "[ \t\r]+")))
    (when (string-match ygg-qf--text-loc-re line)
      (let ((file (expand-file-name (match-string 1 line)))
            (lnum (match-string 2 line))
            (col (match-string 3 line))
            (rest (match-string 4 line)))
        (concat file ":" lnum (if col (concat ":" col) "") ": " rest)))))

;;;###autoload
(defun ygg-qf-from-text (text &optional name replace list)
  "Fill the quickfix from the location lines in TEXT and return how many.
Every line shaped FILE:LINE:COL: REST or FILE:LINE: REST becomes a row;
everything else is dropped, so a tool's own chatter cannot enter the
list.  NAME titles the list, in the banner, when the list is drawn
fresh.  With REPLACE the old rows go first.  LIST is the register the
rows go to, a character, and nil is the list in hand.  Nothing is drawn
when no line was a location, and the count is of rows, never of the
title."
  (let ((rows (delq nil (mapcar #'ygg-qf--text-row (split-string text "\n")))))
    (when rows
      (ygg-qf--collect (if name (cons name rows) rows) replace list))
    (length rows)))

(defun ygg-qf-take (list)
  "Take LIST in hand and say so, after rows landed there from a reader.
What stood in the list in hand before is kept as it was."
  (setq ygg-qf-current list)
  (message "quickfix: %s in hand, the old list kept" (ygg-qf-name list)))

(defconst ygg-qf--loose-loc-re
  (concat "\\(?:^\\|[ \t(\\[\"'`,]\\)"
          "\\(~?/?[[:alnum:]_.@-]*\\(?:/[[:alnum:]_.@-]+\\)*"
          "\\.[[:alnum:]]+\\)"
          "\\(?::\\([0-9]+\\)\\)?\\(?::\\([0-9]+\\)\\)?")
  "A path with an extension anywhere in a line, with a line and column
after it when the writer gave them.")

(defun ygg-qf--loose-rows (line)
  "Every place LINE mentions, as rows, with the whole line as their text.
A mention is a path the checkout holds, read against default-directory,
with :LINE and :COL after it when they are there and line one when they
are not; a line naming no file the checkout holds yields nothing, so
prose around the places never enters the list."
  (let ((text (string-trim line)) (start 0) rows)
    (while (string-match ygg-qf--loose-loc-re line start)
      (let* ((path (match-string 1 line))
             (file (expand-file-name path))
             (lnum (or (match-string 2 line) "1"))
             (col (match-string 3 line)))
        (setq start (match-end 0))
        (when (file-regular-p file)
          (push (concat file ":" lnum (if col (concat ":" col) "") ": " text)
                rows))))
    (nreverse rows)))

(defun ygg-qf-from-lines (beg end &optional name replace)
  "Fill the quickfix from every place the lines between BEG and END name.
A line shaped like a tool's own location row is taken as it stands, and
any other line is read for the paths it mentions.  NAME titles the list;
with REPLACE the old rows go first.  Returns how many rows landed."
  (let* ((lines (split-string (buffer-substring-no-properties beg end) "\n"))
         (rows (seq-mapcat (lambda (line)
                             (or (when-let* ((row (ygg-qf--text-row line)))
                                   (list row))
                                 (ygg-qf--loose-rows line)))
                           lines)))
    (when rows
      (ygg-qf--collect (if name (cons name rows) rows) replace))
    (length rows)))

(defun ygg-qf--selection-lines ()
  "The whole lines the selection covers, or the whole buffer without one."
  (if (or (use-region-p) (and (boundp 'ygg--visual-p) ygg--visual-p))
      (let ((bounds (if (fboundp 'ygg-selection-effective-bounds)
                        (ygg-selection-effective-bounds)
                      (list (region-beginning) (region-end)))))
        (save-excursion
          (list (progn (goto-char (car bounds)) (line-beginning-position))
                (progn (goto-char (max (car bounds) (1- (cadr bounds))))
                       (line-end-position)))))
    (list (point-min) (point-max))))

;;;###autoload
(defun ygg-qf-from-selection ()
  "Send every place the selected lines name to the quickfix.
Any output will do, a trace, an agent's reply, a findings buffer, a
shell buffer: select the lines and the paths in them become rows, each
opening where the line said.  Without a selection the whole buffer is
read.  The rows are appended under the buffer's name, so a list is
built from several selections."
  (interactive)
  (let* ((bounds (ygg-qf--selection-lines))
         (default-directory (or (ignore-errors (ygg-task-root-here))
                                default-directory))
         (n (ygg-qf-from-lines (car bounds) (cadr bounds)
                               (buffer-name) nil)))
    (when (fboundp 'deactivate-mark) (deactivate-mark))
    (if (zerop n)
        (message "quickfix: no place named in the selection")
      (message "quickfix: %d row%s from %s" n (if (= n 1) "" "s")
               (buffer-name)))))

(defun ygg-qf--last-line (buffer)
  "The last line of BUFFER with anything on it, or nil when it has none."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (car (last (split-string (string-trim (buffer-string)) "\n" t))))))

(defun ygg-qf--command-done (proc out err title dir)
  "Turn PROC's output in OUT into rows under TITLE, and say what landed.
DIR is the directory the run was started in, which is what its relative
paths mean.  A run that found nothing and failed is reported by its last
line on ERR, which is where a tool says why it could not run at all."
  (let* ((status (process-exit-status proc))
         (text (if (buffer-live-p out)
                   (with-current-buffer out (buffer-string))
                 ""))
         (n (let ((default-directory dir))
              (ygg-qf-from-text text title t)))
         (trouble (and (zerop n) (not (zerop status))
                       (ygg-qf--last-line err))))
    (if trouble
        (message "quickfix: %s" trouble)
      (message "quickfix: %d row%s from %s" n (if (= n 1) "" "s") title))
    (dolist (buf (list out err))
      (when (buffer-live-p buf)
        (when-let* ((p (get-buffer-process buf))) (delete-process p))
        (kill-buffer buf)))))

;;;###autoload
(defun ygg-qf-from-command (command &optional name)
  "Run COMMAND in the shell and fill the quickfix with the locations it prints.
The run is asynchronous, so a long search does not hold the frame, and
colour is turned off in its environment because an escape sequence in a
path is a row that points nowhere.  NAME titles the list; without one
the command line does.  Returns the process."
  (interactive "sCommand: ")
  (let* ((dir default-directory)
         (title (or name command))
         (out (generate-new-buffer " *ygg-qf-out*"))
         (err (generate-new-buffer " *ygg-qf-err*"))
         (process-environment (append '("NO_COLOR=1" "TERM=dumb")
                                      process-environment)))
    (make-process
     :name "ygg-qf-command"
     :buffer out
     :stderr err
     :noquery t
     :connection-type 'pipe
     :command (list shell-file-name shell-command-switch command)
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (ygg-qf--command-done proc out err title dir))))))

(defun ygg-qf-clear ()
  "Empty the list in hand, leaving the panel there to be filled again."
  (interactive)
  (when-let* ((buf (ygg-qf-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (ygg-qf-reset)
        (insert (ygg-qf--banner)))))
  (message "quickfix: %s is empty" (ygg-qf-name)))

;;; The colon line: one verb over the list

(defvar ygg-ex--commands)

(defun ygg-ex--cmd-qf (range _bang args)
  "Drive the quickfix: ARGS runs a command, empties the list, or is empty.
A leading bang is the shell, the way it is everywhere else on the colon
line, which makes an alias such as todogrep = qf !rg -n TODO one line.
With a RANGE and no ARGS the places those lines name become rows."
  (let ((args (string-trim args)))
    (cond
     ((and range (string-empty-p args))
      (let* ((bounds (ygg-ex--line-range-bounds range))
             (default-directory (or (ignore-errors (ygg-task-root-here))
                                    default-directory))
             (n (ygg-qf-from-lines (car bounds) (cadr bounds)
                                   (buffer-name) nil)))
        (message "quickfix: %d row%s from %s" n (if (= n 1) "" "s")
                 (buffer-name))))
     ((string-prefix-p "!" args)
      (let ((command (string-trim (substring args 1))))
        (when (string-empty-p command) (user-error "usage: :qf !COMMAND"))
        (ygg-qf-from-command command)))
     ((string-empty-p args)
      (select-window (display-buffer (ygg-qf-ensure-buffer))))
     ((equal args "clear") (ygg-qf-clear))
     (t (user-error "usage: :qf [!COMMAND|clear]")))))

(with-eval-after-load 'yggdrasil-ex
  (setf (alist-get "qf" ygg-ex--commands nil nil #'equal)
        'ygg-ex--cmd-qf))

;;; Keys

(declare-function ygg-next-error-any "yggdrasil-motions")
(declare-function ygg-prev-error-any "yggdrasil-motions")
(declare-function ygg-quickfix-toggle "yggdrasil-leader")

(yggdrasil-define-keys 'normal
  "] Q" #'ygg-qf-last :label "last qf entry"
  "[ Q" #'ygg-qf-first :label "first qf entry"
  "] l" #'ygg-next-error-any :label "next qf entry"
  "[ l" #'ygg-prev-error-any :label "prev qf entry")

;;; Errors out of the things that produce them: a process, a shell, an agent

(defun ygg-qf--output-buffers ()
  "Buffers that hold the output of something that ran."
  (seq-filter
   (lambda (b)
     (with-current-buffer b
       (or (derived-mode-p 'compilation-mode 'comint-mode 'shell-mode
                           'eshell-mode 'term-mode)
           (and (fboundp 'ghostel-mode) (derived-mode-p 'ghostel-mode))
           (get-buffer-process b))))
   (buffer-list)))

;;;###autoload
(defun ygg-qf-from-buffer (buffer &optional name)
  "Read every place BUFFER names into the quickfix.
Whatever wrote it — a compile, a shell, a container's log — a line
that names a file and a line is a place to go, and the rest is
chatter."
  (interactive
   (list (get-buffer
          (completing-read "Errors from: "
                           (mapcar #'buffer-name (ygg-qf--output-buffers))
                           nil t nil nil (buffer-name)))))
  (let* ((buffer (or buffer (current-buffer)))
         (name (or name (buffer-name buffer)))
         (n (with-current-buffer buffer
              (ygg-qf-from-lines (point-min) (point-max) name t))))
    (message "quickfix: %d from %s" (or n 0) name)
    n))

;;;###autoload
(defun ygg-qf-from-process ()
  "Read the output of something that is running, or has run."
  (interactive)
  (let ((buffers (ygg-qf--output-buffers)))
    (unless buffers (user-error "quickfix: nothing has run here"))
    (ygg-qf-from-buffer
     (get-buffer (completing-read "Errors from: "
                                  (mapcar #'buffer-name buffers) nil t)))))

(declare-function aob-target "aob")
(declare-function aob-session-events "aob" (s))
(declare-function aob-session-name "aob" (s))
(declare-function aob-event-text "aob" (ev))

;;;###autoload
(defun ygg-qf-from-session (session)
  "Read the places SESSION mentioned into the quickfix.
What an agent says about a file is a place to go like any other: the
list is what it said, read for paths the checkout holds."
  (interactive (list (if (fboundp 'aob-target) (aob-target)
                       (user-error "quickfix: no agents here"))))
  (let* ((text (mapconcat
                (lambda (ev)
                  (or (ignore-errors (aob-event-text ev))
                      (plist-get ev :title) ""))
                (seq-filter (lambda (ev)
                              (memq (plist-get ev :type)
                                    '(message error tool prompt)))
                            (aob-session-events session))
                "\n"))
         (name (aob-session-name session))
         (n (with-temp-buffer
              (insert text)
              (setq default-directory
                    (or (ignore-errors (aob-session-dir session))
                        default-directory))
              (ygg-qf-from-lines (point-min) (point-max) name t))))
    (message "quickfix: %d from %s" (or n 0) name)
    n))

(yggdrasil-define-keys 'ygg-leader-quit-map
  "l" #'ygg-quickfix-toggle :label "quickfix"
  "\"" #'ygg-qf-switch :label "which list"
  "c" #'ygg-search-multibuffer :label "search → quickfix"
  "t" #'ygg-qf-todos :label "TODOs → quickfix"
  "e" #'ygg-qf-from-comint :label "buffer errors → quickfix"
  "p" #'ygg-qf-from-process :label "process output → quickfix"
  "b" #'ygg-qf-from-buffer :label "a buffer → quickfix"
  "A" #'ygg-qf-from-session :label "agent said → quickfix"
  "v" #'ygg-qf-from-selection :label "selection → quickfix"
  "D" #'ygg-qf-diagnostics :label "diagnostics → quickfix")

(provide 'layer-quickfix)
;;; layer-quickfix.el ends here
