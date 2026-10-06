;;; layer-terminal.el --- toggleterm.nvim-style terminal layer -*- lexical-binding: t; -*-

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-leader)
(require 'yggdrasil-localleader)
(require 'project)
(require 'cl-lib)

(declare-function ghostel "ghostel" (&optional arg))
(declare-function ghostel-semi-char-mode "ghostel")
(declare-function ghostel-emacs-mode "ghostel")
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ygg-space-claim-buffer "layer-sessions" (buf id &optional move))
(declare-function ygg-space--current-id "yggdrasil-spacetree" ())
(declare-function ygg-space--current "yggdrasil-spacetree" ())
(declare-function ygg-space--dir-of "yggdrasil-spacetree" (tab))
(declare-function ygg-window-left "yggdrasil-verbs")
(declare-function ygg-window-down "yggdrasil-verbs")
(declare-function ygg-window-up "yggdrasil-verbs")
(declare-function ygg-window-right "yggdrasil-verbs")
(declare-function ygg-search-forward "yggdrasil-motions")
(declare-function ghostel--copy-all-text "ghostel-module" (term))

(defvar ghostel-buffer-name)
(defvar ghostel-buffer-name-function)
(defvar ghostel-semi-char-mode-map)
(defvar ghostel-readonly-mode-map)
(defvar ghostel--term)
(defvar ghostel-shell)
(defvar ghostel-macos-login-shell)
(defvar ghostel-pre-spawn-hook)
(declare-function ygg-agent--terminal-env "layer-aob" ())
(defvar ygg-leader-map)

(when (fboundp 'elpaca)
  (elpaca (ghostel :host github :repo "dakra/ghostel"
                   :files (:defaults "etc" "src" "vendor"
                                     "build.zig" "build.zig.zon" "symbols.map"))))

(add-to-list 'ygg-deny-modes 'ghostel-mode)

(declare-function ygg-jump-back "yggdrasil-motions")
(declare-function ygg-jump-forward "yggdrasil-motions")

;; jumplist nav inside terminals: the Tab key keeps its terminal job via
;; the distinct <tab> event; C-o / C-i (the TAB character) jump
(with-eval-after-load 'ghostel
  (dolist (sym '(ghostel-mode-map ghostel-semi-char-mode-map))
    (when-let* ((map (and (boundp sym) (symbol-value sym)))
                ((keymapp map)))
      (let ((tab (lookup-key map (kbd "TAB"))))
        (when (and (commandp tab) (not (lookup-key map (kbd "<tab>"))))
          (define-key map (kbd "<tab>") tab)))
      (define-key map (kbd "C-o") #'ygg-jump-back)
      (define-key map (kbd "C-i") #'ygg-jump-forward))))

(defconst ygg-term-buffer-name "*ygg-term*")

(defvar ygg-term-height-fraction 0.3
  "Fraction of frame height a bottom terminal split gets.")

(defvar ygg-term-width-fraction 0.4
  "Fraction of frame width a right-side terminal split gets.")

(defcustom ygg-term-split 'right
  "Side a terminal split opens on: `right' (vsplit) or `below'.
`ygg-term-rotate-split' moves the visible terminal and leaves the new
side as the default for the next one."
  :type '(choice (const right) (const below))
  :group 'yggdrasil)

(defun ygg--term-split-window ()
  "Split the frame for a terminal, on the side `ygg-term-split' names."
  (if (eq ygg-term-split 'below)
      (split-window (frame-root-window)
                    (- (max 1 (round (* (frame-height) ygg-term-height-fraction))))
                    'below)
    (split-window (frame-root-window)
                  (- (max 30 (round (* (frame-width) ygg-term-width-fraction))))
                  'right)))

(defun ygg--term-display-window (buffer alist)
  "Show BUFFER in a fresh terminal split, as a `display-buffer' action."
  (window--display-buffer buffer (ygg--term-split-window) 'window alist))

(defvar ygg-term-display-action
  '((display-buffer-reuse-window ygg--term-display-window))
  "How any terminal or job buffer is displayed, wherever it comes from.
Keeps `split-window-sensibly' — which prefers a bottom split — off every
path that shows one.")

(defun ygg--task-buffer-p (buffer-or-name &optional _action)
  (let ((b (get-buffer buffer-or-name)))
    (and b (string-prefix-p "*task:" (buffer-name b)))))

;; a job is a terminal, not a results list: its own split, never the panel
(add-to-list 'display-buffer-alist
             (cons #'ygg--task-buffer-p ygg-term-display-action))

(defun ygg-call-with-buffer-env (thunk &optional extra-env)
  "Call THUNK with the invoking buffer's env visible to spawned processes.
envrc/mise set `process-environment' and `exec-path' buffer-locally, so a
process started inside a fresh terminal buffer would otherwise see the
globals; binding the default values carries the captured (plus EXTRA-ENV)
environment across the buffer switch."
  (let ((env (append extra-env process-environment))
        (path exec-path))
    (cl-letf (((default-value 'process-environment) env)
              ((default-value 'exec-path) path))
      (funcall thunk))))

(defcustom ygg-inject-mise t
  "Wrap terminals and agents in `mise exec' so they inherit the project's
mise tool env, as the nvim config does.  The wrap runs inside the login
shell (after macOS `path_helper'), so mise re-adds its dirs on top of the
resolved PATH.  Needs the `mise' binary; a no-op without it."
  :type 'boolean :group 'yggdrasil)

(defun ygg-mise-prefix ()
  "Shell-command prefix injecting the project mise env, or an empty string."
  (if (and ygg-inject-mise (executable-find "mise")) "mise exec -- " ""))

(defun ygg--term-buffer ()
  "Return the dedicated terminal buffer if it is alive, else nil."
  (let ((buf (get-buffer ygg-term-buffer-name)))
    (and buf (buffer-live-p buf) buf)))

(defun ygg--term-window ()
  "Return the window showing the dedicated terminal buffer, if any."
  (let ((buf (ygg--term-buffer)))
    (and buf (get-buffer-window buf))))

(defconst ygg-term--ssh-methods '("ssh" "scp" "sshx" "scpx")
  "TRAMP methods whose host an ssh login shell can be opened on.")

(defun ygg-term--remote-command (dir)
  "The argv of a login shell on DIR's host, sitting in DIR, or nil.
Nil for a folder here, and nil for a remote method that is not reached
over ssh: there is no shell to open on the far side of those."
  (when-let* ((dir dir)
              (method (file-remote-p dir 'method))
              ((member method ygg-term--ssh-methods))
              (host (file-remote-p dir 'host)))
    (let ((user (file-remote-p dir 'user))
          (path (or (file-remote-p dir 'localname) "/")))
      (list "ssh" "-t" (if user (concat user "@" host) host)
            (format "cd %s && exec $SHELL -l" (shell-quote-argument path))))))

(defun ygg--ghostel-shell (name &optional dir)
  "Spawn a fresh ghostel shell claimed as buffer NAME; caller displays it.
DIR is the folder the shell starts in: a remote one over ssh puts the
shell on its host instead, where the project mise env is not ours to
inject.  Runs under `save-window-excursion' because `ghostel' pops its
buffer, and disables title-driven renaming so NAME stays stable for
lookups."
  (require 'ghostel)
  (when (and dir (file-remote-p dir) (not (ygg-term--remote-command dir)))
    (message "terminal: local shell, %s has no shell to open on"
             (file-remote-p dir 'method)))
  (ygg-call-with-buffer-env
   (lambda ()
     (let* ((sh (or (getenv "SHELL") "/bin/zsh"))
            (over-there (ygg-term--remote-command dir))
            (ghostel-pre-spawn-hook (if over-there
                                        (remq #'ygg-agent--terminal-env
                                              ghostel-pre-spawn-hook)
                                      ghostel-pre-spawn-hook))
            (default-directory (if (and dir (not (file-remote-p dir))
                                        (file-directory-p dir))
                                   (file-name-as-directory dir)
                                 default-directory))
            (mise (if over-there "" (ygg-mise-prefix)))
            ;; when injecting mise, drive the shell ourselves: a login shell
            ;; (past path_helper) execs `mise exec' then the interactive shell
            (ghostel-shell (cond (over-there over-there)
                                 ((string-empty-p mise)
                                  (and (boundp 'ghostel-shell) ghostel-shell))
                                 (t (list sh "-l" "-c"
                                          (concat "exec " mise sh " -i")))))
            (ghostel-macos-login-shell (if (and (not over-there)
                                                (string-empty-p mise))
                                           (and (boundp 'ghostel-macos-login-shell)
                                                ghostel-macos-login-shell)
                                         nil))
            (buf (save-window-excursion
                  (let ((ghostel-buffer-name name))
                    (ghostel '(4))))))
       (with-current-buffer buf
         (setq-local ghostel-buffer-name-function nil)
         (setq-local mode-line-format nil)
         (add-hook 'kill-buffer-hook #'ygg--term-delete-window nil t)
         (unless (equal (buffer-name) name)
           (rename-buffer name t)))
       buf))
   (and dir (file-remote-p dir) (fboundp 'ygg-agent-terminal-env)
        (ygg-agent-terminal-env dir))))

(defun ygg--term-delete-window ()
  "Delete the windows showing this terminal when its buffer is killed."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (when (and (window-live-p win) (not (window-minibuffer-p win))
               (not (frame-root-window-p win)))
      (ignore-errors (delete-window win)))))

(defun ygg--term-create ()
  "Start a fresh ghostel shell and claim it as the dedicated terminal buffer."
  (ygg--ghostel-shell ygg-term-buffer-name))

(defun ygg-term--space-name ()
  "Default terminal buffer name, scoped to the current space.
So each space toggles its OWN terminal: the home space keeps its
terminal, a freshly-created space gets a new one on first toggle."
  (if-let* (((fboundp 'ygg-space--current-id))
            (id (ygg-space--current-id)))
      (format "*ygg-term:sp%s*" id)
    ygg-term-buffer-name))

(defun ygg-term--space-dir ()
  "The folder the current space stands on, read off the tab and no further.
Read as it was pinned, never probed: a remote folder must not cost a
connection on a keypress."
  (and (fboundp 'ygg-space--current)
       (ygg-space--dir-of (ygg-space--current))))

(defun ygg-terminal-toggle (&optional n)
  "Toggle a terminal split, toggleterm.nvim-style.
A numeric prefix gives terminal N (`*ygg-term:N*') its OWN split, so
numbered terminals stack as separate windows; without one, the current
space's own terminal, which is claimed for this space whatever space
first showed it.  Hides that terminal's split when already visible."
  (interactive "P")
  (let* ((num (and n (prefix-numeric-value n)))
         (name (if num (format "*ygg-term:%d*" num) (ygg-term--space-name)))
         (buf (get-buffer name))
         (shown (and buf (get-buffer-window buf))))
    (if shown
        (delete-window shown)
      (let ((b (or buf (ygg--ghostel-shell name (and (not num)
                                                     (ygg-term--space-dir)))))
            (win (ygg--term-split-window)))
        (when (and (not num) (fboundp 'ygg-space-claim-buffer)
                   (fboundp 'ygg-space--current-id))
          (ygg-space-claim-buffer b (ygg-space--current-id) t))
        (set-window-buffer win b)
        (select-window win)
        (ghostel-semi-char-mode)))))

;;; Named terminals + jobs (tmux-window feel; everything async)

(defun ygg--term-display (buf)
  "Show BUF in its window if visible, else a new split; enter semi-char."
  (let ((win (get-buffer-window buf)))
    (if win
        (select-window win)
      (let ((new-win (ygg--term-split-window)))
        (set-window-buffer new-win buf)
        (select-window new-win)))
    (when (fboundp 'ghostel-semi-char-mode) (ghostel-semi-char-mode))))

(defun ygg--term-buffers ()
  (seq-filter (lambda (b)
                (provided-mode-derived-p (buffer-local-value 'major-mode b) 'ghostel-mode))
              (buffer-list)))

(defun ygg-term-new (name)
  "Spawn a named terminal `*ygg-term:NAME*' in a terminal split."
  (interactive "sTerminal name: ")
  (when (string-empty-p name)
    (setq name (number-to-string (1+ (length (ygg--term-buffers))))))
  (ygg--term-display (ygg--ghostel-shell (format "*ygg-term:%s*" name))))

(defun ygg-term-pick ()
  "Pick any live terminal (named, toggle, agent) and show it in a split."
  (interactive)
  (let ((bufs (mapcar #'buffer-name (ygg--term-buffers))))
    (unless bufs (user-error "No terminals; SPC o t or SPC o n"))
    (ygg--term-display (get-buffer (completing-read "Terminal: " bufs nil t)))))

(defun ygg--job-candidates ()
  (mapcar (lambda (proc)
            (cons (format "%-18s %-8s %-22s %s"
                          (process-name proc)
                          (process-status proc)
                          (if (process-buffer proc)
                              (buffer-name (process-buffer proc))
                            "-")
                          (mapconcat #'identity (or (process-command proc) '("")) " "))
                  proc))
          (process-list)))

(defun ygg-jobs ()
  "List every live process; selecting one jumps to its buffer in a split."
  (interactive)
  (let* ((cands (ygg--job-candidates))
         (_ (unless cands (user-error "No running jobs")))
         (proc (cdr (assoc (completing-read "Job: " (mapcar #'car cands) nil t) cands)))
         (buf (and proc (process-buffer proc))))
    (if (and buf (buffer-live-p buf))
        (select-window (display-buffer buf ygg-term-display-action))
      (message "job has no buffer"))))

(defun ygg-job-kill ()
  "Pick a live process and kill it."
  (interactive)
  (let* ((cands (ygg--job-candidates))
         (_ (unless cands (user-error "No running jobs")))
         (proc (cdr (assoc (completing-read "Kill job: " (mapcar #'car cands) nil t) cands))))
    (delete-process proc)
    (message "killed %s" (process-name proc))))

(defvar ghostel-compile-view-mode-map)

(defun ygg-task-quit ()
  "Quit a task buffer, killing it once the job it ran has finished.
A job still running is only buried: `q' is not how work gets ended."
  (interactive)
  (quit-window (not (get-buffer-process (current-buffer)))))

(with-eval-after-load 'ghostel-compile
  (define-key ghostel-compile-view-mode-map "q" #'ygg-task-quit))

;; tmux bell analog: background compiles announce themselves instead of
;; requiring the user to watch the buffer
(defun ygg--compile-notify (buffer status)
  (let ((msg (format "%s: %s" (buffer-name buffer) (string-trim status)))
        (ok (string-prefix-p "finished" status)))
    (if (fboundp 'ygg-notify)
        (ygg-notify msg (if ok 'info 'error))
      (message "%s" msg))))

(defun ygg--compile-cleanup (buffer status)
  "Tidy a finished compile: bury a clean build, drop an interrupted one.
Only real `compilation-mode' buffers are touched — grep and *quickfix*
ride the same hook but must stay put — and a build that exited with
errors is left visible so those errors stay reachable."
  (when (eq (buffer-local-value 'major-mode buffer) 'compilation-mode)
    (let ((win (get-buffer-window buffer 0)))
      (cond
       ((string-prefix-p "finished" status)
        (when (window-live-p win) (quit-window nil win)))
       ((not (string-prefix-p "exited abnormally" status))
        (if (window-live-p win) (quit-window t win) (kill-buffer buffer)))))))

(add-hook 'compilation-finish-functions #'ygg--compile-notify)
(add-hook 'compilation-finish-functions #'ygg--compile-cleanup t)

(setq async-shell-command-buffer 'new-buffer)

(defun ygg-ghostty-here ()
  "Open Ghostty.app at the current project root, or `default-directory'."
  (interactive)
  (let* ((proj (project-current))
         (dir (expand-file-name (if proj (project-root proj) default-directory))))
    (start-process "ghostty" nil "open" "-na" "Ghostty"
                   "--args" (concat "--working-directory=" dir))))

(defun ygg-ghostel-jk ()
  "j reaches the pty instantly; only a fast k erases it and exits insert.
Plain letters must go out as text — the key encoder emits nothing for a
bare unmodified letter (ghostel's own self-insert sends strings too)."
  (interactive)
  (ghostel-send-string "j")
  (unless executing-kbd-macro
    (let ((ev (read-event nil nil 0.3)))
      (cond
       ((eq ev ?k)
        (ghostel-send-key "backspace")
        (ghostel-emacs-mode))
       (ev (push ev unread-command-events))))))

(with-eval-after-load 'ghostel
  (define-key ghostel-semi-char-mode-map "j" #'ygg-ghostel-jk)
  (define-key ghostel-semi-char-mode-map (kbd "C-h") #'ygg-window-left)
  (define-key ghostel-semi-char-mode-map (kbd "C-j") #'ygg-window-down)
  (define-key ghostel-semi-char-mode-map (kbd "C-k") #'ygg-window-up)
  (define-key ghostel-semi-char-mode-map (kbd "C-l") #'ygg-window-right) ; shadows shell's clear; `clear` still works
  ;; <escape> keeps ghostel's send binding so TUIs receive raw ESC; jk is the only exit
  (define-key ghostel-readonly-mode-map (kbd "i") #'ghostel-semi-char-mode)
  (define-key ghostel-readonly-mode-map (kbd "SPC") ygg-leader-map))

;;; Yggdrasil states in terminals: semi-char IS insert (no state maps, so
;;; every key incl. ESC reaches the pty), ghostel emacs mode IS normal
;;; (full grammar over the scrollback: hjkl, v visual, y, search).

(defvar ygg--ghostel-insert-tag
  (propertize " ⟨I⟩ " 'face 'ygg-state-insert))

(defun ygg--ghostel-state-insert (&rest _)
  (when (derived-mode-p 'ghostel-mode)
    (when yggdrasil-local-mode (yggdrasil-local-mode -1))
    (setq ygg--modeline-tag ygg--ghostel-insert-tag)
    (force-mode-line-update)))

(defun ygg--ghostel-state-normal (&rest _)
  (when (and (derived-mode-p 'ghostel-mode) (not yggdrasil-local-mode))
    (yggdrasil-local-mode 1)))

(defun ygg--ghostel-insert-redirect ()
  ;; i/a/o in a terminal mean "type at the prompt": hand off to the pty
  (when (and (derived-mode-p 'ghostel-mode)
             (fboundp 'ghostel-semi-char-mode))
    (run-with-timer 0 nil
                    (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf (ghostel-semi-char-mode))))
                    (current-buffer))))

(defun ygg--ghostel-insert-elsewhere ()
  (setq-local ygg-insert-elsewhere t))

(with-eval-after-load 'ghostel
  (advice-add 'ghostel-semi-char-mode :after #'ygg--ghostel-state-insert)
  (advice-add 'ghostel-emacs-mode :after #'ygg--ghostel-state-normal)
  (when (fboundp 'ghostel-copy-mode)
    (advice-add 'ghostel-copy-mode :after #'ygg--ghostel-state-normal))
  (add-hook 'ygg-insert-entry-hook #'ygg--ghostel-insert-redirect)
  (add-hook 'ghostel-mode-hook #'ygg--ghostel-insert-elsewhere))

;;; comint shells/REPLs: land in insert at the prompt so typing is
;;; immediate; jk/ESC drops to normal for vim scroll + output nav.

(defun ygg--comint-enter-insert ()
  (when (derived-mode-p 'comint-mode)
    (let ((buf (current-buffer)))
      (run-with-timer 0 nil
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (when (and yggdrasil-local-mode (fboundp 'ygg-insert-state))
                              (ygg-insert-state)))))))))

(add-hook 'comint-mode-hook #'ygg--comint-enter-insert)

(defvar-local ygg-term-find--terminal nil
  "The terminal buffer this find buffer was read from, and q puts back.")

(defvar ygg-term-find-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'ygg-term-find-quit)
    map)
  "Keys of a terminal find buffer, over the modal layer's own.")

(defun ygg-term--scrollback-text ()
  "The current terminal's whole scrollback as text without properties.
Asks the terminal for it when there is a live one to ask; a scrollback
is also materialized as the buffer text, which is the answer otherwise."
  (if (and (boundp 'ghostel--term) ghostel--term
           (fboundp 'ghostel--copy-all-text))
      (or (ghostel--copy-all-text ghostel--term) "")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ygg-term-find ()
  "Search this terminal's scrollback in a buffer that the / grammar reaches.
The scrollback goes into a read-only buffer named for the terminal, shown
in the terminal's own window with point at the end, and the search verb
starts there; q gives the window back to the terminal."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not a terminal"))
  (let* ((term (current-buffer))
         (text (ygg-term--scrollback-text))
         (win (or (get-buffer-window term) (selected-window)))
         (buf (get-buffer-create (format "*find: %s*" (buffer-name term)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (setq buffer-read-only t)
      (setq ygg-term-find--terminal term)
      (yggdrasil-define-local-keys 'normal ygg-term-find-map)
      (unless yggdrasil-local-mode (yggdrasil-local-mode 1))
      (goto-char (point-max)))
    (set-window-buffer win buf)
    (select-window win)
    (ygg-search-forward)))

(defun ygg-term-find-quit ()
  "Put the terminal back in this window and drop the find buffer."
  (interactive)
  (let ((term ygg-term-find--terminal)
        (buf (current-buffer))
        (win (selected-window)))
    (when (buffer-live-p term)
      (set-window-buffer win term))
    (kill-buffer buf)))

(yggdrasil-localleader-def 'ghostel-mode "f" #'ygg-term-find
                           "find in scrollback")

(defvar ygg-leader-open-map (make-sparse-keymap) "The o prefix: open external things.")
(defvar ygg-leader-terminal-map (make-sparse-keymap) "The o t prefix: terminals.")
(defvar ygg-leader-jobs-map (make-sparse-keymap) "The o j prefix: jobs.")

(defun ygg-term-rotate-split ()
  "Move the visible terminal between a right vsplit and the bottom split.
The side it lands on becomes the default for the next terminal."
  (interactive)
  (let* ((buf (or (seq-find #'ygg--job-buffer-p
                            (mapcar #'window-buffer (window-list)))
                  (user-error "No visible terminal")))
         (win (get-buffer-window buf)))
    (setq ygg-term-split (if (window-full-height-p win) 'below 'right))
    (delete-window win)
    (let ((new (ygg--term-split-window)))
      (set-window-buffer new buf)
      (select-window new))))

(declare-function ygg--job-buffer-p "layer-completion")

(yggdrasil-define-keys 'ygg-leader-open-map
  "t" ygg-leader-terminal-map :label "terminal"
  "j" ygg-leader-jobs-map :label "jobs"
  "u" #'vundo :label "undo tree"
  "d" #'eldoc-doc-buffer :label "hover docs"
  "H" #'ygg-tutor :label "tutor"
  "D" #'docker :label "docker")

(yggdrasil-define-keys 'ygg-leader-terminal-map
  "t" #'ygg-terminal-toggle :label "terminal"
  "n" #'ygg-term-new :label "new terminal"
  "p" #'ygg-term-pick :label "pick terminal"
  "v" #'ygg-term-rotate-split :label "terminal ⇄ vsplit"
  "T" #'ygg-ghostty-here :label "ghostty here")

(yggdrasil-define-keys 'ygg-leader-jobs-map
  "j" #'ygg-jobs :label "jobs"
  "k" #'ygg-job-kill :label "kill job")

(declare-function vundo "vundo")
(declare-function ygg-tutor "yggdrasil-leader")
(declare-function docker "docker")

(when (fboundp 'elpaca)
  (elpaca docker))

(declare-function ygg-list-vim-keys "yggdrasil-verbs")
(declare-function tablist-do-kill-lines "tablist")
(defvar tablist-minor-mode-map)

;; tablist's minor map outranks docker's; its k moves to x, free in every menu
(with-eval-after-load 'tablist
  (ygg-list-vim-keys tablist-minor-mode-map)
  (define-key tablist-minor-mode-map "x" #'tablist-do-kill-lines))

(declare-function docker-compose-run-docker-compose-async-with-buffer "docker-compose" (action &rest args))

(defun ygg-docker-compose-ps ()
  "What the stack has, running or not — the one thing its transient lacks."
  (interactive)
  (docker-compose-run-docker-compose-async-with-buffer "ps" "-a"))

(declare-function ygg-output-bound-process "layer-tasks" (process))

(defun ygg-docker--bound-output (process &rest _)
  "Keep what docker streams into a buffer from filling it."
  (when (fboundp 'ygg-output-bound-process) (ygg-output-bound-process process))
  process)

(with-eval-after-load 'docker-process
  ;; docker's own output buffers are special-mode, not comint: the hook
  ;; that bounds a shell does not reach them
  (advice-add 'docker-run-start-file-process-shell-command
              :filter-return #'ygg-docker--bound-output))

(with-eval-after-load 'docker-compose
  ;; after Config, in the group it belongs to; the lower-case keys are
  ;; the arguments and the upper-case ones the verbs
  (ignore-errors
    (transient-append-suffix 'docker-compose "V"
      '("A" "Ps (all)" ygg-docker-compose-ps))))

(yggdrasil-leader-def "o" ygg-leader-open-map "open")

(provide 'layer-terminal)
;;; layer-terminal.el ends here
