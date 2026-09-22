;;; layer-tasks.el --- justfile/package.json task-runner picker -*- lexical-binding: t; -*-

;;; Code:

(require 'compile)
(require 'yggdrasil-core)

(declare-function ygg--ghostel-shell "layer-terminal" (name))
(declare-function ygg--term-display "layer-terminal" (buf))
(declare-function ygg-call-with-buffer-env "layer-terminal" (thunk &optional extra-env))
(declare-function ghostel-compile-global-mode "ghostel-compile")
(declare-function ghostel-comint-global-mode "ghostel-comint")
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

(defvar ygg-leader-open-map)
(defvar ygg-dired-goto-map)

(defun ygg-task--read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun ygg-task--locate-justfile (start)
  (let ((dir (locate-dominating-file
              start
              (lambda (d)
                (or (file-exists-p (expand-file-name "justfile" d))
                    (file-exists-p (expand-file-name "Justfile" d)))))))
    (when dir
      (let ((jf (expand-file-name "justfile" dir)))
        (if (file-exists-p jf) jf (expand-file-name "Justfile" dir))))))

(defun ygg-task--locate-package-json (start)
  (let ((dir (locate-dominating-file start "package.json")))
    (when dir (expand-file-name "package.json" dir))))

(defun ygg-task--justfile-recipes-text (file)
  "Fallback parser used when the `just' binary is unavailable."
  (let (names)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (string-match "\\`\\([[:alnum:]_-][[:alnum:]_:.-]*\\)[ \t]*[^:=\n]*:" line)
            (push (match-string 1 line) names)))
        (forward-line 1)))
    (nreverse names)))

(defvar ygg-task--recipe-cache (make-hash-table :test #'equal)
  "justfile path -> last known recipe list, primed asynchronously by `just'.")

(defun ygg-task--refresh-recipes (dir file)
  "Prime FILE's recipe cache with `just --summary' in the background."
  (let ((pname (format "ygg-just:%s" file)))
    (when (and (executable-find "just") (not (get-process pname)))
      (let ((default-directory dir))
        (make-process
         :name pname :buffer (generate-new-buffer " *ygg-just*")
         :command '("just" "--summary") :noquery t
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (when (and (eq (process-status proc) 'exit)
                        (= (process-exit-status proc) 0))
               (puthash file
                        (split-string (with-current-buffer (process-buffer proc)
                                        (buffer-string))
                                      "[ \t\n]+" t)
                        ygg-task--recipe-cache))
             (kill-buffer (process-buffer proc)))))))))

(defun ygg-task--just-recipes (dir file)
  "Recipe names for FILE — never blocking: cache or text-parse now, `just' async."
  (ygg-task--refresh-recipes dir file)
  (or (gethash file ygg-task--recipe-cache)
      (ygg-task--justfile-recipes-text file)))

(defun ygg-task--npm-scripts (file)
  (let* ((parsed (ignore-errors (json-parse-string (ygg-task--read-file file) :object-type 'alist)))
         (scripts (and (consp parsed) (alist-get 'scripts parsed))))
    (when (consp scripts)
      (mapcar (lambda (kv) (symbol-name (car kv))) scripts))))

(defun ygg-task--js-runner (dir)
  "Package-manager run prefix for DIR, picked by its lockfile."
  (cond ((file-exists-p (expand-file-name "pnpm-lock.yaml" dir)) "pnpm")
        ((file-exists-p (expand-file-name "yarn.lock" dir)) "yarn")
        ((file-exists-p (expand-file-name "bun.lockb" dir)) "bun")
        (t "npm")))

(defun ygg-task--locate (start &rest names)
  "Absolute path of the first of NAMES found dominating START."
  (catch 'hit
    (dolist (n names)
      (when-let* ((dir (locate-dominating-file start n)))
        (throw 'hit (expand-file-name n dir))))))

(defun ygg-task--make-targets (file)
  "Explicit single-target rule names in Makefile FILE."
  (let (names)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([a-zA-Z0-9][a-zA-Z0-9_./-]*\\)[ \t]*:\\([^=]\\|$\\)" nil t)
        (push (match-string 1) names)))
    (nreverse (delete-dups names))))

(defun ygg-task--deno-tasks (file)
  (let* ((parsed (ignore-errors (json-parse-string (ygg-task--read-file file) :object-type 'alist)))
         (tasks (and (consp parsed) (alist-get 'tasks parsed))))
    (when (consp tasks)
      (mapcar (lambda (kv) (symbol-name (car kv))) tasks))))

(defvar ygg-task--toolchains
  '(("Cargo.toml" "cargo"
     ("build" . "build") ("test" . "test") ("run" . "run")
     ("check" . "check") ("clippy" . "clippy"))
    ("go.mod" "go"
     ("build" . "build ./...") ("test" . "test ./...")
     ("run" . "run .") ("vet" . "vet ./...")))
  "Manifest file -> (RUNNER (LABEL . ARGS)...): built-in subcommands to offer.")

(defcustom ygg-task-bin-dirs '("bin" "scripts")
  "Directories scanned for personal CLIs to offer as tasks.
Relative names resolve against the project root; ~ and absolute
names are taken literally.  Add \"~/.local/bin\" to surface global
scripts (skipped on remote hosts to avoid per-file round trips)."
  :type '(repeat string) :group 'yggdrasil)

(defun ygg-task--bin-scripts (root)
  "Executable files across `ygg-task-bin-dirs', resolved against ROOT.
Skipped when ROOT is remote — a stat per file would hammer TRAMP."
  (when (file-remote-p root) (setq root nil))
  (let (seen items)
    (when root (dolist (spec ygg-task-bin-dirs)
      (let ((d (if (file-name-absolute-p spec)
                   (expand-file-name spec)
                 (expand-file-name spec root))))
        (when (file-accessible-directory-p d)
          (dolist (f (directory-files d t "\\`[^.]"))
            (let ((name (file-name-nondirectory f)))
              (when (and (not (member name seen))
                         (file-regular-p f) (file-executable-p f))
                (push name seen)
                (push (cons name f) items))))))))
    (nreverse items)))

(defun ygg-task--collect (&optional start)
  (let* ((start (or start default-directory))
         (justfile (ygg-task--locate-justfile start))
         (package-json (ygg-task--locate-package-json start))
         (makefile (ygg-task--locate start "Makefile" "makefile" "GNUmakefile"))
         (deno (ygg-task--locate start "deno.json" "deno.jsonc"))
         (items nil))
    (when justfile
      (let ((dir (file-name-directory justfile)))
        (dolist (name (sort (ygg-task--just-recipes dir justfile) #'string<))
          (push (list :label (format "just: %s" name)
                       :command (format "just %s" name)
                       :directory dir)
                items))))
    (when package-json
      (let* ((dir (file-name-directory package-json))
             (runner (ygg-task--js-runner dir)))
        (dolist (name (sort (ygg-task--npm-scripts package-json) #'string<))
          (push (list :label (format "%s: %s" runner name)
                       :command (format "%s run %s" runner name)
                       :directory dir)
                items))))
    (when makefile
      (let ((dir (file-name-directory makefile)))
        (dolist (name (sort (ygg-task--make-targets makefile) #'string<))
          (push (list :label (format "make: %s" name)
                       :command (format "make %s" name)
                       :directory dir)
                items))))
    (dolist (tc ygg-task--toolchains)
      (when-let* ((mf (ygg-task--locate start (car tc))))
        (let ((dir (file-name-directory mf)) (runner (cadr tc)))
          (dolist (v (cddr tc))
            (push (list :label (format "%s: %s" runner (car v))
                         :command (format "%s %s" runner (cdr v))
                         :directory dir)
                  items)))))
    (when deno
      (let ((dir (file-name-directory deno)))
        (dolist (name (sort (ygg-task--deno-tasks deno) #'string<))
          (push (list :label (format "deno: %s" name)
                       :command (format "deno task %s" name)
                       :directory dir)
                items))))
    (let ((root (or (when-let* ((proj (project-current))) (project-root proj)) start)))
      (dolist (kv (ygg-task--bin-scripts root))
        (push (list :label (format "bin: %s" (car kv))
                     :command (shell-quote-argument (cdr kv))
                     :directory root)
              items)))
    (nreverse items)))

(defvar ygg-task--last-command nil)
(defvar ygg-task--last-directory nil)
(defvar ygg-task--last-name nil)

(defcustom ygg-task-comint t
  "Run tasks in an interactive comint buffer so input reaches the job."
  :type 'boolean :group 'yggdrasil)

(defun ygg-task--buffer-name (display)
  "A fresh unique buffer name per run, so multiple jobs run concurrently."
  (generate-new-buffer-name
   (format "*task:%s*" (truncate-string-to-width display 40 nil nil "…"))))

(defun ygg-task--exec (command directory &optional name)
  "Run COMMAND in DIRECTORY; NAME (a label) names the job buffer."
  (setq ygg-task--last-command command
        ygg-task--last-directory directory
        ygg-task--last-name name)
  (ygg-call-with-buffer-env
   (lambda ()
     (let ((default-directory directory)
           (compilation-always-kill t)
           (compilation-buffer-name-function
            (lambda (_mode) (ygg-task--buffer-name (or name command)))))
       (compile command ygg-task-comint)))))

(defun ygg-task-run ()
  "Locate just/npm tasks upward from `default-directory' and run the chosen one."
  (interactive)
  (let* ((items (ygg-task--collect))
         (alist (mapcar (lambda (it) (cons (plist-get it :label) it)) items)))
    (unless alist (user-error "tasks: no justfile or package.json found"))
    (let* ((choice (completing-read "Task: " alist nil t))
           (item (cdr (assoc choice alist))))
      (ygg-task--exec (plist-get item :command) (plist-get item :directory)
                      (plist-get item :label)))))

(defun ygg-task-repeat ()
  "Rerun the last task started by `ygg-task-run' without prompting."
  (interactive)
  (unless ygg-task--last-command (user-error "tasks: no previous task to repeat"))
  (ygg-task--exec ygg-task--last-command ygg-task--last-directory ygg-task--last-name))

(defvar ygg-run-async--history nil)

(defun ygg-task--project-root ()
  (if-let* ((proj (project-current))) (project-root proj) default-directory))

(defun ygg-run-async (command)
  "Run any COMMAND as a background comint job at the project root.
Lands in a live `*task:…*' buffer, tracked by `ygg-jobs'/`ygg-job-kill'."
  (interactive (list (read-shell-command "Async run: " nil 'ygg-run-async--history)))
  (ygg-task--exec command (ygg-task--project-root)))

(defun ygg-terminal-here-dired ()
  "Open a ghostel terminal in the directory at point, oil.nvim `!'-style."
  (interactive)
  (let* ((file (ignore-errors (dired-get-filename nil t)))
         (dir (expand-file-name (if (and file (file-directory-p file)) file default-directory))))
    (let ((default-directory dir))
      (ygg--term-display
       (ygg--ghostel-shell
        (generate-new-buffer-name
         (format "*ygg-term:%s*" (file-name-nondirectory (directory-file-name dir)))))))))

;;; Unified command panel: tasks to run + running jobs + every command,
;;; one picker (merges the old SPC o c palette and SPC o r task runner).

(defun ygg--panel-job-buffers ()
  (seq-uniq
   (delq nil
         (mapcar (lambda (p)
                   (let ((b (process-buffer p)))
                     (and (buffer-live-p b) (process-live-p p) b)))
                 (process-list)))))

(defun ygg--panel-commands ()
  (let (cmds) (mapatoms (lambda (s) (when (commandp s) (push (symbol-name s) cmds)))) cmds))

(defun ygg-command-panel ()
  "One panel: run a task, jump to a running job, or run any command."
  (interactive)
  (let ((actions (make-hash-table :test 'equal))
        (special '()))
    (dolist (it (ygg-task--collect))
      (let ((s (format "▶ run  %s" (plist-get it :label))))
        (puthash s (cons 'task it) actions) (push s special)))
    ;; what this project can be told to run — the justfile, the package
    ;; scripts, the mix tasks — which the sidebar knows and the panel did
    ;; not, so the two lists disagreed about what exists
    (dolist (c (and (fboundp 'ygg-project-commands)
                    (when-let* ((pr (project-current nil)))
                      (ignore-errors (ygg-project-commands (project-root pr))))))
      (let ((s (format "▶ %-6s %s" (plist-get c :source) (plist-get c :name))))
        (puthash s (cons 'project c) actions) (push s special)))
    (dolist (b (ygg--panel-job-buffers))
      (let ((s (format "⚙ job  %s" (buffer-name b))))
        (puthash s (cons 'job b) actions) (push s special)))
    (setq special (nreverse special))
    (let* ((all (append special (ygg--panel-commands)))
           (table (lambda (str pred action)
                    (if (eq action 'metadata)
                        '(metadata (category . command)
                                   (display-sort-function . identity))
                      (complete-with-action action all str pred))))
           (choice (completing-read "▶ " table nil nil))
           (entry (gethash choice actions)))
      (cond
       ((eq (car-safe entry) 'task)
        (ygg-task--exec (plist-get (cdr entry) :command) (plist-get (cdr entry) :directory)
                        (plist-get (cdr entry) :label)))
       ((eq (car-safe entry) 'project)
        (if (fboundp 'ygg-project-commands-run)
            (ygg-project-commands-run (cdr entry))
          (user-error "projects: nothing to run it with")))
       ((eq (car-safe entry) 'job)
        (select-window (display-buffer (cdr entry)
                                       '((display-buffer-reuse-window
                                          display-buffer-pop-up-window)))))
       ((commandp (intern-soft choice)) (command-execute (intern choice) 'record))
       ((or (null choice) (string-empty-p choice)) nil)
       (t (user-error "Unknown: %s" choice))))))

;;; Output is a stream, not a document: nothing keeps all of it

(defcustom ygg-output-max-lines 4000
  "Lines an output buffer keeps before the top goes.
A log is read at its end.  Fifty of them keeping everything is a
machine spending its afternoon on text nobody will scroll back to."
  :type 'natnum :group 'yggdrasil)

(defun ygg-output--bound-comint ()
  "Keep this comint buffer to `ygg-output-max-lines'."
  (setq-local comint-buffer-maximum-size ygg-output-max-lines)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil t))

(defun ygg-output--bound-compilation ()
  "Keep this compilation buffer to `ygg-output-max-lines'."
  (save-excursion
    (let ((inhibit-read-only t)
          (keep (- (line-number-at-pos (point-max)) ygg-output-max-lines)))
      (when (> keep 0)
        (goto-char (point-min))
        (forward-line keep)
        (delete-region (point-min) (point))))))

;;;###autoload
(defun ygg-output-bound-process (process)
  "Keep PROCESS\='s buffer to `ygg-output-max-lines\=', whatever wrote it.
Not every stream is a comint: a plain process buffer has no filter
hook to hang truncation on, so the truncation goes on the filter."
  (when (processp process)
    (let ((filter (or (process-filter process)
                      #'internal-default-process-filter)))
      (set-process-filter
       process
       (lambda (proc chunk)
         (funcall filter proc chunk)
         (when-let* ((buffer (process-buffer proc))
                     ((buffer-live-p buffer)))
           (with-current-buffer buffer (ygg-output--bound-compilation))))))
    process))

(add-hook 'comint-mode-hook #'ygg-output--bound-comint)
(add-hook 'compilation-filter-hook #'ygg-output--bound-compilation)

(yggdrasil-define-keys 'ygg-leader-open-map
  "c" #'ygg-command-panel :label "command panel"
  "!" #'ygg-run-async :label "run async command"
  "R" #'ygg-task-repeat :label "repeat task")

(with-eval-after-load 'dired
  (define-key ygg-dired-goto-map "t" #'ygg-terminal-here-dired))

;;; Compile/tasks render in a ghostel TTY; grep-mode stays stock so wgrep
;;; and the *quickfix* buffer keep working (ghostel excludes it by default)

(defun ygg--tasks-enable-ghostel ()
  (when (require 'ghostel-compile nil t)
    (ghostel-compile-global-mode 1))
  (when (require 'ghostel-comint nil t)
    (ghostel-comint-global-mode 1)))

(when (fboundp 'elpaca)
  (run-with-idle-timer 2 nil #'ygg--tasks-enable-ghostel)
  (with-eval-after-load 'ghostel (ygg--tasks-enable-ghostel)))

;;; mise — per-project tool env (the nvim config wraps every task in
;;; `mise exec`; global-mise-mode gives buffers the same resolved env)

(declare-function global-mise-mode "mise")
(declare-function mise-update-dir "mise" (&optional all))
(defvar mise-trust)

;; mise shells out for every lookup and `mise--update' asks three times per
;; buffer, so a restored session pays ~200ms x every buffer before it draws
(defvar ygg-mise--configs (make-hash-table :test #'equal))
(defvar ygg-mise--ensured (make-hash-table :test #'equal))

(defun ygg-mise--memo (table thunk)
  (let* ((key (or default-directory ""))
         (hit (gethash key table 'miss)))
    (if (eq hit 'miss) (puthash key (funcall thunk) table) hit)))

(defun ygg-mise--configs-memo (orig) (ygg-mise--memo ygg-mise--configs orig))
(defun ygg-mise--ensure-memo (orig) (ygg-mise--memo ygg-mise--ensured orig))

(defun ygg-mise-forget (&rest _)
  "Re-read mise configs on the next lookup; a config file added or removed
since Emacs started is invisible until this runs."
  (interactive)
  (clrhash ygg-mise--configs)
  (clrhash ygg-mise--ensured))

(when (and (fboundp 'elpaca) (executable-find "mise"))
  (elpaca mise
    (run-with-idle-timer
     1 nil
     (lambda ()
       (require 'mise)
       ;; `ask' is a yes-or-no-p reached from after-change-major-mode-hook:
       ;; in a daemon it can block on a frame nobody is looking at
       (setq mise-trust t)
       (advice-add 'mise--detect-configs :around #'ygg-mise--configs-memo)
       (advice-add 'mise--ensure :around #'ygg-mise--ensure-memo)
       (advice-add 'mise-update-dir :before #'ygg-mise-forget)
       (global-mise-mode 1)))))

(declare-function envrc-global-mode "envrc")
(defvar envrc-show-summary-in-minibuffer)

(defun ygg-envrc--status-quietly (orig &rest args)
  "Set the direnv status without saying so, where the summary is silent.
The knob that silences the summary leaves the status line alone, and
that line is said once for every buffer that enters an env: opening a
project means it in the echo area over and over, on top of whatever the
owner was reading.  The log keeps it either way."
  (let ((inhibit-message (not envrc-show-summary-in-minibuffer)))
    (apply orig args)))

(when (and (fboundp 'elpaca) (executable-find "direnv"))
  ;; loaded from source: envrc.el expands `envrc--with-direnv-buffer' 130
  ;; lines above the macro it expands into, so the .elc calls a macro as a
  ;; function and every direnv run dies with "Invalid function"
  (elpaca (envrc :build (:not elpaca-build-compile))
    (run-with-idle-timer
     1 nil
     (lambda ()
       (require 'envrc)
       ;; anything but t runs a `sit-for' wait loop, and `sit-for' returns
       ;; at once while input is pending — during a restore that is a busy
       ;; spin at 100% CPU per buffer, not a wait
       (setq envrc-async t
             envrc-show-summary-in-minibuffer nil)
       ;; the summary is the only message that knob silences; the status
       ;; line is said again for every buffer that enters an env
       (advice-add 'envrc--direnv-set-status :around
                   #'ygg-envrc--status-quietly)
       (envrc-global-mode 1)))))

(provide 'layer-tasks)
;;; layer-tasks.el ends here
