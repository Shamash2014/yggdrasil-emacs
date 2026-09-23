;;; yggdrasil-core.el --- Yggdrasil state machine -*- lexical-binding: t; -*-

;; Built-ins wrapped: emulation-mode-map-alists, cursor-type, mode-line-format,
;; read-event (jk timer), define-globalized-minor-mode.
;; Custom: three-state machine, binding API, activation predicate.

;;; Code:

(defgroup yggdrasil nil
  "Helix-first modal editing with a vim/neovim blend."
  :group 'editing
  :prefix "ygg-")

(defcustom ygg-escape-delay 0.3
  "Seconds to wait for the k of a jk escape chord."
  :type 'number)

(defcustom ygg-deny-modes
  '(vterm-mode eshell-mode term-mode)
  "Major modes that never get Yggdrasil states.
comint/shell and REPLs stay modal (insert types at the prompt, normal
gives vim scroll and output navigation); true terminals are denied."
  :type '(repeat symbol))

(defcustom ygg-modal-special-modes
  '(compilation-mode image-mode xwidget-webkit-mode
    help-mode Info-mode messages-buffer-mode apropos-mode occur-mode eww-mode)
  "Special-mode families that get the full modal layer anyway.
Their buffers gain motions, visual state, and the yank operator; the
mode keeps only the keys in `ygg-modal-special-keep'."
  :type '(repeat symbol))

(defcustom ygg-modal-special-keep '("q" "RET" "<backtab>")
  "Keys whose mode-local binding stays on top in those buffers.
TAB is intentionally absent: it is the C-i character, and jumplist
forward (C-i) must win everywhere, so compilation's next-error yields."
  :type '(repeat string))

(defcustom ygg-modal-special-mode-keep
  '((help-mode ("<tab>" . "TAB") ("[ h" . "l") ("] h" . "r"))
    (Info-mode ("<tab>" . "TAB") "u" ("] n" . "n") ("[ p" . "p")
               ("[ h" . "l") ("] h" . "r"))
    (apropos-mode ("<tab>" . "TAB"))
    (occur-mode ("i" . "e"))
    (eww-mode ("<tab>" . "TAB") ("[ h" . "l") ("] h" . "r")))
  "Extra keys kept per mode family, on top of the shared keep list.
Each entry is (MODE . SPECS).  A SPEC is a KEY keeping its mode binding,
\(KEY . FROM) moving the mode binding of FROM to KEY, or (KEY . COMMAND).
The <tab> key is the GUI tab event, so C-i stays jumplist forward."
  :type '(alist :key-type symbol :value-type (repeat sexp)))

;;; Regex dialect (PCRE via pcre2el)

(defcustom ygg-pcre-regexps t
  "Convert PCRE syntax to Elisp regexps at Yggdrasil's search/selection prompts."
  :type 'boolean)

(declare-function rxt-pcre-to-elisp "pcre2el")

(defun ygg-regexp (pattern)
  "Convert PATTERN from PCRE to Elisp regexp syntax via pcre2el.
Falls back to PATTERN unchanged when `ygg-pcre-regexps' is nil, pcre2el
is unavailable, or PATTERN fails to parse as PCRE."
  (if (and ygg-pcre-regexps (require 'pcre2el nil t))
      (condition-case nil (rxt-pcre-to-elisp pattern) (error pattern))
    pattern))

;;; States

(defvar-local ygg--state nil
  "Current state: `normal', `visual', `insert', or nil when disabled.")
(defvar-local ygg--normal-p nil)
(defvar-local ygg--visual-p nil)
(defvar-local ygg--insert-p nil)

(defvar ygg-normal-map (make-sparse-keymap))
(defvar ygg-visual-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ygg-normal-map)
    map)
  "Visual-state map; inherits everything from `ygg-normal-map'.")
(defvar ygg-insert-map (make-sparse-keymap)
  "Insert-state map; binds only escape hatches, all else falls through.")

(defvar ygg-goto-map (make-sparse-keymap) "The g prefix.")
(defvar ygg-view-map (make-sparse-keymap) "The z prefix.")
(defvar ygg-window-map (make-sparse-keymap) "The C-w prefix (vim window map).")
(defvar ygg-z-cap-map (make-sparse-keymap) "The Z prefix (ZZ/ZQ).")
(defvar ygg-selections-map (make-sparse-keymap)
  "The V prefix: what to do with several selections at once.
Vim has no multiple cursors, so there is nothing to inherit here; V is
free because this layer selects lines Kakoune-style with x.")

(defvar ygg--emulation-alist
  `((ygg--visual-p . ,ygg-visual-map)
    (ygg--normal-p . ,ygg-normal-map)
    (ygg--insert-p . ,ygg-insert-map)))
(add-to-list 'emulation-mode-map-alists 'ygg--emulation-alist)

(defvar ygg--mode-keys (make-hash-table :test #'equal)
  "State-scoped mode keymaps, keyed by (MODE . STATE).
Each value is a list of keymaps: the one bindings go into, then the
keymaps lifted whole.")

(defvar-local ygg--local-keys nil
  "This buffer's own state-scoped keymaps, as (STATE . KEYMAPS).")

(defvar-local ygg--special-keep-map nil
  "Mode keys a ygg-modal-special-modes buffer keeps in normal and visual.")

(defvar-local ygg--mode-keys-alist nil
  "Emulation alist of this buffer's mode keys, one entry per state.
Built by ygg--mode-keys-refresh.")
;; prepended after ygg's own alist, so mode keys win over the states
(add-to-list 'emulation-mode-map-alists 'ygg--mode-keys-alist)

(defconst ygg--state-vars
  '((visual . ygg--visual-p) (normal . ygg--normal-p) (insert . ygg--insert-p)))

(defun ygg--mode-keys-state-maps (state)
  "Keymaps lifted in STATE here, strongest first.
Buffer keys, then active minor modes, then the major mode and its
parents, then the kept special-mode keys."
  (let ((chain (derived-mode-all-parents major-mode))
        minor major)
    (maphash (lambda (key maps)
               (when (eq (cdr key) state)
                 (let ((mode (car key)))
                   (cond ((memq mode chain) (push (cons mode maps) major))
                         ((and (boundp mode) (symbol-value mode))
                          (setq minor (append minor maps)))))))
             ygg--mode-keys)
    (append (alist-get state ygg--local-keys)
            minor
            (mapcan (lambda (mode) (copy-sequence (alist-get mode major)))
                    chain)
            (and ygg--special-keep-map (memq state '(normal visual))
                 (list ygg--special-keep-map)))))

(defun ygg--mode-keys-refresh ()
  "Recompute this buffer's mode keys for every state."
  (setq ygg--mode-keys-alist
        (let (alist)
          (pcase-dolist (`(,state . ,var) ygg--state-vars)
            (when-let* ((maps (ygg--mode-keys-state-maps state)))
              (push (cons var (make-composed-keymap maps)) alist)))
          (nreverse alist))))

(defun ygg--mode-keys-refresh-all ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when ygg--state (ygg--mode-keys-refresh)))))

(defun ygg--mode-keys-hook (mode)
  "The function MODE's hook runs to recompute mode keys."
  (or (get mode 'ygg--mode-keys-hook)
      (put mode 'ygg--mode-keys-hook
           (lambda ()
             (if (and (boundp mode) (not (local-variable-p mode)))
                 (ygg--mode-keys-refresh-all)
               (ygg--mode-keys-refresh))))))

(defun ygg--bind-into (maps bindings)
  "Put BINDINGS into MAPS and return MAPS, grown when a keymap was lifted.
BINDINGS are repeating KEY DEF bound in the first of MAPS, or one
keymap added to MAPS whole."
  (if (and (keymapp (car bindings)) (null (cdr bindings)))
      (if (memq (car bindings) maps) maps (append maps bindings))
    (while bindings
      (define-key (car maps) (kbd (pop bindings)) (pop bindings)))
    maps))

(defun yggdrasil-define-mode-keys (mode states &rest bindings)
  "Bind BINDINGS for MODE in each of STATES, above yggdrasil's own maps.
MODE is a major or minor mode, loaded yet or not.  STATES is a state
symbol or a list of them.  BINDINGS are repeating KEY DEF, or one
keymap lifted whole.  A major mode's keys reach the modes derived from
it, the more derived mode winning; an active minor mode's keys win over
any major mode's, and yggdrasil-define-local-keys over both."
  (dolist (state (ensure-list states))
    (let* ((key (cons mode state))
           (old (gethash key ygg--mode-keys))
           (new (ygg--bind-into (or old (list (make-sparse-keymap))) bindings)))
      (unless (eq new old)
        (puthash key new ygg--mode-keys)
        (add-hook (intern (format "%s-hook" mode)) (ygg--mode-keys-hook mode))
        (ygg--mode-keys-refresh-all)))))

(defun yggdrasil-define-local-keys (states &rest bindings)
  "Bind BINDINGS in this buffer only, in each of STATES, above all mode keys.
STATES and BINDINGS are as in yggdrasil-define-mode-keys.  A major
mode change drops them, so a mode hook is the place to call this."
  (dolist (state (ensure-list states))
    (setf (alist-get state ygg--local-keys)
          (ygg--bind-into (or (alist-get state ygg--local-keys)
                              (list (make-sparse-keymap)))
                          bindings)))
  (ygg--mode-keys-refresh))

(defun ygg--modalize-special ()
  "Give this special-mode buffer the modal layer, keeping select mode keys."
  (let ((keep (make-sparse-keymap)))
    (dolist (spec (append ygg-modal-special-keep
                          (mapcan (lambda (entry)
                                    (and (derived-mode-p (car entry))
                                         (copy-sequence (cdr entry))))
                                  ygg-modal-special-mode-keep)))
      (let* ((key (if (consp spec) (car spec) spec))
             (from (if (consp spec) (cdr spec) spec))
             (def (if (stringp from) (local-key-binding (kbd from)) from)))
        (when def (define-key keep (kbd key) def))))
    (setq ygg--special-keep-map keep))
  (yggdrasil-local-mode 1))

(defvar ygg-normal-entry-hook nil)
(defvar ygg-visual-entry-hook nil)
(defvar ygg-visual-exit-hook nil)
(defvar ygg-insert-entry-hook nil)
(defvar ygg-insert-exit-hook nil)

(defvar-local ygg--modeline-tag ""
  "Pre-propertized state indicator; recomputed only on state change.")
(put 'ygg--modeline-tag 'risky-local-variable t)

(defvar-local ygg--macro-tag ""
  "Pre-propertized \"recording @x\" indicator; set by yggdrasil-verbs.el.")
(put 'ygg--macro-tag 'risky-local-variable t)

(defface ygg-state-normal '((t :inherit ygg-modeline-pill)) "Normal tag.")
(defface ygg-state-visual '((t :inherit ygg-modeline-pill)) "Visual tag.")
(defface ygg-state-insert '((t :inherit ygg-modeline-pill)) "Insert tag.")

(defconst ygg--tags
  `((normal . ,(propertize " NORMAL " 'face 'ygg-state-normal))
    (visual . ,(propertize " VISUAL " 'face 'ygg-state-visual))
    (insert . ,(propertize " INSERT " 'face 'ygg-state-insert))))

(defun ygg-state () ygg--state)
(defun ygg-normal-p () ygg--normal-p)
(defun ygg-visual-p () ygg--visual-p)
(defun ygg-insert-p () ygg--insert-p)

(defvar-local ygg--last-insert nil
  "Marker at the point where insert state last exited (vim gi).")

(defcustom ygg-echo-state t
  "Echo vim-style \"-- INSERT --\" state notices in the echo area."
  :type 'boolean :group 'yggdrasil)

(defvar ygg--state-echoed nil)

(defun ygg--echo-state (state)
  (when (and ygg-echo-state (not noninteractive)
             (not (minibufferp)) (not executing-kbd-macro))
    (let (message-log-max)
      (pcase state
        ('insert (setq ygg--state-echoed t) (message "-- INSERT --"))
        ('visual (setq ygg--state-echoed t) (message "-- VISUAL --"))
        (_ (when ygg--state-echoed
             (setq ygg--state-echoed nil)
             (message nil)))))))

(defun ygg--switch-state (state)
  "Enter STATE, running exit/entry hooks and updating visuals."
  (let ((old ygg--state))
    (unless (eq old state)
      ;; before exit hooks: they shift point left (hel cursor), vim's ^
      ;; mark records where the insertion actually stopped
      (when (eq old 'insert)
        (setq ygg--last-insert (point-marker)))
      (cond ((eq old 'insert) (run-hooks 'ygg-insert-exit-hook))
            ((eq old 'visual) (run-hooks 'ygg-visual-exit-hook)))
      (setq ygg--state state
            ygg--normal-p (eq state 'normal)
            ygg--visual-p (eq state 'visual)
            ygg--insert-p (eq state 'insert)
            ygg--modeline-tag (or (alist-get state ygg--tags) "")
            ;; box where a cell must read as selected — insert edits it, visual
            ;; marks it (a bar leaves the cursor cell looking unselected); bar
            ;; in normal
            cursor-type (pcase state
                          ((or 'insert 'visual) 'box)
                          ('nil t)
                          (_ 'bar)))
      (pcase state
        ('normal (run-hooks 'ygg-normal-entry-hook))
        ('visual (run-hooks 'ygg-visual-entry-hook))
        ('insert (run-hooks 'ygg-insert-entry-hook)))
      (ygg--echo-state state)
      (force-mode-line-update))))

(defun ygg-normal-state ()
  "Enter normal state."
  (interactive)
  (ygg--switch-state 'normal))

(defun ygg-visual-state ()
  "Enter visual state."
  (interactive)
  (ygg--switch-state 'visual))

(defun ygg-insert-state ()
  "Enter insert state."
  (interactive)
  (ygg--switch-state 'insert))

(defun ygg-toggle-visual ()
  "Toggle between visual and normal state."
  (interactive)
  (if ygg--visual-p (ygg-normal-state) (ygg-visual-state)))

(defun ygg-goto-last-insert ()
  "Jump to where insert state last exited and enter insert (vim gi)."
  (interactive)
  (unless (and ygg--last-insert (marker-position ygg--last-insert))
    (user-error "No previous insert in this buffer"))
  (goto-char ygg--last-insert)
  (ygg-insert-state))

(defun ygg-insert-one-command ()
  "Run one normal-state command, then come back to insert (vim C-o)."
  (interactive)
  (ygg-normal-state)
  (add-hook 'post-command-hook #'ygg--insert-one-command-return nil t))

(defun ygg--insert-one-command-return ()
  (unless (eq this-command 'ygg-insert-one-command)
    (remove-hook 'post-command-hook #'ygg--insert-one-command-return t)
    (when ygg--normal-p (ygg-insert-state))))

(declare-function ygg--register-read "yggdrasil-verbs")

(defun ygg-insert-register ()
  "Insert a register's contents at point (vim insert-state C-r)."
  (interactive)
  (let ((reg (read-char "register: ")))
    (insert (if (eq reg ?\")
                (or (current-kill 0 t) "")
              (mapconcat #'identity (ygg--register-read reg) "\n")))))

(defun ygg-insert-kill-word ()
  "Delete the word before point (vim insert-state C-w)."
  (interactive)
  (delete-region (point) (progn (forward-word -1) (point))))

(defun ygg-insert-kill-to-bol ()
  "Delete back to the first non-blank, else line start (vim insert-state C-u)."
  (interactive)
  (let ((indent (save-excursion (back-to-indentation) (point))))
    (if (> (point) indent)
        (delete-region indent (point))
      (delete-region (line-beginning-position) (point)))))

;;; jk escape (native two-key timer)

(defvar-local ygg-jk-forward-function nil
  "Function of one char routing a jk-escape's j somewhere other than the buffer.
Read-only modal buffers that send keys to another target (e.g. embr, which
types into a web page) set this so j reaches that target instead of being
self-inserted — where `insert' would signal `buffer-read-only'.")

(defun ygg--jk-escape ()
  "Insert j; if k follows within `ygg-escape-delay', escape to normal.
With `ygg-jk-forward-function' set, route j through it rather than inserting,
so a read-only page buffer still receives the keystroke and k still escapes."
  (interactive)
  (if ygg-jk-forward-function
      (let ((evt (unless (or executing-kbd-macro defining-kbd-macro)
                   (read-event nil nil ygg-escape-delay))))
        (if (eq evt ?k)
            (ygg-normal-state)
          (funcall ygg-jk-forward-function ?j)
          (when evt (push evt unread-command-events))))
    (insert "j")
    (unless (or executing-kbd-macro defining-kbd-macro)
      (let ((evt (read-event nil nil ygg-escape-delay)))
        (cond ((null evt))
              ((eq evt ?k) (delete-char -1) (ygg-normal-state))
              (t (push evt unread-command-events)))))))

;;; Binding API

(defun yggdrasil-define-keys (state &rest bindings)
  "Bind BINDINGS in STATE's keymap.
STATE is `normal', `visual', `insert', or a keymap symbol like
`ygg-goto-map'.  BINDINGS are repeating KEY DEF [:label LABEL] where
LABEL names the binding for which-key."
  (let ((map (pcase state
               ('normal ygg-normal-map)
               ('visual ygg-visual-map)
               ('insert ygg-insert-map)
               ((pred keymapp) state)
               ((pred symbolp) (symbol-value state))
               (_ (error "Unknown state %S" state)))))
    (while bindings
      (let ((key (pop bindings))
            (def (pop bindings))
            (label (when (eq (car bindings) :label)
                     (pop bindings)
                     (pop bindings))))
        (define-key map (kbd key) (if label (cons label def) def))))))

(defalias 'yggdrasil-key #'yggdrasil-define-keys
  "User-facing alias, applied after module defaults load, so it wins.
Example: (yggdrasil-key \\='normal \"x\" #\\='my-command)")

;;; Local & global modes

(define-minor-mode yggdrasil-local-mode
  "Yggdrasil modal editing in this buffer."
  :init-value nil
  (if yggdrasil-local-mode
      (progn (ygg--mode-keys-refresh)
             (ygg--switch-state 'normal))
    (setq ygg--normal-p nil ygg--visual-p nil ygg--insert-p nil
          ygg--state nil ygg--modeline-tag ""
          cursor-type t)))

(defun ygg--maybe-activate ()
  (when (and (not (minibufferp))
             (not (memq major-mode ygg-deny-modes)))
    (cond
     ((apply #'derived-mode-p ygg-modal-special-modes)
      (ygg--modalize-special))
     ;; comint/shell/REPLs stay modal even though comint-mode carries
     ;; mode-class `special' — normal scrolls output, `i' types at the
     ;; prompt (true terminals are denied above)
     ((or (derived-mode-p 'comint-mode)
          (and (not (derived-mode-p 'special-mode))
               ;; dired-style modes only set mode-class, not a parent
               (not (eq (get major-mode 'mode-class) 'special))))
      (yggdrasil-local-mode 1)))))

;;;###autoload
(define-globalized-minor-mode yggdrasil-global-mode
  yggdrasil-local-mode ygg--maybe-activate)

;; buffers born in fundamental-mode never run a major-mode function, so
;; the globalized after-change-major-mode-hook misses them (C-x b new,
;; get-buffer-create); catch them the first time they land in a window
(defun ygg--activate-on-display (frame)
  (when yggdrasil-global-mode
    (dolist (win (window-list frame 'no-minibuf))
      (with-current-buffer (window-buffer win)
        (when (and (null ygg--state) (not yggdrasil-local-mode))
          (ygg--maybe-activate))))))

(add-hook 'window-buffer-change-functions #'ygg--activate-on-display)

;;; Core default bindings (state plumbing only; modules add their own)

(yggdrasil-define-keys 'insert
  "<escape>" #'ygg-normal-state
  "C-g" #'ygg-normal-state
  "j" #'ygg--jk-escape
  "C-o" #'ygg-insert-one-command :label "one normal cmd"
  "C-r" #'ygg-insert-register :label "insert register"
  "C-w" #'ygg-insert-kill-word :label "delete word back"
  "C-u" #'ygg-insert-kill-to-bol :label "delete to line start"
  "<home>" #'beginning-of-line
  "<end>" #'end-of-line)

;; g i stays LSP find-implementation (nvim habit, layer-lsp)
(yggdrasil-define-keys 'ygg-goto-map
  "I" #'ygg-goto-last-insert :label "last insert")

(yggdrasil-define-keys 'normal
  "v" #'ygg-toggle-visual :label "visual"
  "g" ygg-goto-map :label "goto"
  "z" ygg-view-map :label "view"
  "C-w" ygg-window-map :label "window"
  "Z" ygg-z-cap-map :label "quit"
  "V" ygg-selections-map :label "selections")

;; Unbound printable keys must never fall through to self-insert.
(define-key ygg-normal-map [remap self-insert-command] #'undefined)

(yggdrasil-define-keys 'visual
  "v" #'ygg-normal-state
  "<escape>" #'ygg-normal-state)

(require 'yggdrasil-modeline)

;;; Universal escape — ESC backs out of anything, anywhere.
;; Not `keyboard-escape-quit': its fallthrough deletes other windows.

(defun ygg-escape-everything ()
  "Abort the minibuffer, drop the region, or quit, never touching windows.
An active minibuffer is aborted from its own window: pressed from any
other window while a prompt stands, the abort otherwise signals that
point is not in a minibuffer and the prompt survives."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (with-selected-window (minibuffer-window)
           (abort-minibuffers)))
        ((region-active-p) (deactivate-mark))
        (t (keyboard-quit))))

(global-set-key [escape] #'ygg-escape-everything)

(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-isearch-map
                   read--expression-map))
  (define-key map [escape] #'abort-minibuffers))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map [escape] #'isearch-cancel))

(provide 'yggdrasil-core)
;;; yggdrasil-core.el ends here
