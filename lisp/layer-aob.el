;;; layer-aob.el --- agent-objects glue: ACP agents in spaces -*- lexical-binding: t; -*-

;;; Code:

(require 'yggdrasil-leader)
(require 'cl-lib)
(require 'ygg-agent-conf)
(require 'ygg-pi)
(require 'ygg-agent-maps)
(require 'ygg-projects)
(require 'aob-context)
(require 'aob-deliver)
(require 'aob-btw)
(require 'aob-handoff)
(require 'aob-answer)
(require 'ygg-agent-skills)
(require 'aob-subagent)
(require 'aob-transcript)
(require 'aob-mcp-host)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'ygg-todo)
(require 'aob-todo-view)
(require 'aob-schedule)
(require 'aob-shells)
(require 'aob-workflow)
(require 'ygg-ui)

(declare-function ygg-notify "layer-ui" (msg &optional level))
(declare-function ygg-mise-prefix "layer-terminal")
(declare-function ygg-jump-back "yggdrasil-motions")
(declare-function ygg-jump-forward "yggdrasil-motions")
(declare-function aob-session-awaiting-answer "aob" ())

;; jumplist nav works in agent buffers too; Tab keeps expanding via the
;; distinct <tab> event while C-i (the TAB character) jumps forward
(define-key aob-object-map (kbd "C-o") #'ygg-jump-back)
(define-key aob-trace-mode-map (kbd "<tab>") #'aob-trace-tab)
(define-key aob-trace-mode-map (kbd "C-i") #'ygg-jump-forward)
(define-key aob-trace-mode-map "Q" #'aob-btw)

;; ...and something for them to jump between: opening an agent's view is a
;; jump, but nothing was recording where you left, so C-o from a trace had
;; no origin to return to and C-i never led back into one.
(declare-function ygg--jump-push "yggdrasil-motions" ())
(defun ygg-aob--push-jump (&rest _)
  (when (fboundp 'ygg--jump-push) (ygg--jump-push)))

(dolist (cmd '(aob-trace aob-plan aob-focus))
  (advice-add cmd :before #'ygg-aob--push-jump))

;; the rest of vim nav in agent buffers: goto/view prefixes, half-page
;; scroll, search.  g stops meaning rerender (the buffer renders itself)
(declare-function ygg-goto-last-line "yggdrasil-motions")
(declare-function ygg-scroll-half-down "yggdrasil-motions")
(declare-function ygg-scroll-half-up "yggdrasil-motions")
(declare-function ygg-search-forward "yggdrasil-motions")
(declare-function ygg-search-next "yggdrasil-motions")
(declare-function ygg-search-prev "yggdrasil-motions")
(declare-function ygg-h "yggdrasil-motions")
(defvar ygg-goto-map)
(defvar ygg-view-map)

(define-key aob-object-map "g" ygg-goto-map)
(define-key aob-object-map "z" ygg-view-map)
(define-key aob-object-map "G" #'ygg-goto-last-line)
;; vim finishes a write with ZZ; in a trace that means send what is typed
(define-key aob-trace-mode-map [remap ygg-save-and-kill-buffer] #'aob-trace-send)
(define-key aob-trace-mode-map [remap ygg-kill-buffer-no-save] #'aob-trace-decline)
(define-key aob-object-map (kbd "C-d") #'ygg-scroll-half-down)
(define-key aob-object-map (kbd "C-u") #'ygg-scroll-half-up)
(define-key aob-object-map "/" #'ygg-search-forward)
(define-key aob-object-map "n" #'ygg-search-next)
(define-key aob-object-map "N" #'ygg-search-prev)
;; special-mode-map (a parent) binds h to describe-mode; keep it vim left-motion
(define-key aob-object-map "h" #'ygg-h)

;; the full modal layer, not cherry-picked keys: yggdrasil runs in agent
;; buffers, and each buffer's own verbs sit above ygg's maps in normal and
;; visual state only, so insert state types into the trace's input line
(pcase-dolist (`(,mode . ,map)
               `((aob-trace-mode . ,aob-trace-mode-map)
                 (aob-plan-mode . ,aob-plan-mode-map)
                 (aob-acp-mcp-mode . ,aob-acp-mcp-mode-map)))
  (yggdrasil-define-mode-keys mode '(normal visual) map))

(defvar ygg-aob--trace-local-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<tab>") #'aob-trace-tab)
    (define-key m (kbd "<s-return>") #'aob-trace-send)
    (define-key m (kbd "<C-return>") #'aob-trace-send)
    m)
  "The trace's local map: no printing key, so insert state self-inserts.")

;; the composed parent pulled in ALL of special-mode-map, leaking its nav keys
;; (h ? < > g digits) ahead of ygg; keep only read-scroll + quit so every
;; other key falls through to yggdrasil normal state.  SPC is deliberately
;; NOT kept: yggdrasil rebinds it to the leader, so it must fall through —
;; binding it here to scroll shadowed the leader (SPC b D et al.) in traces.
(defvar ygg-aob--special-keep
  (let ((m (make-sparse-keymap)))
    (define-key m "q" #'quit-window)
    (define-key m (kbd "DEL") #'scroll-down-command)
    (define-key m (kbd "S-SPC") #'scroll-down-command)
    m)
  "The only special-mode keys agent buffers keep; the rest is yggdrasil's.")

(dolist (map (list aob-trace-mode-map aob-plan-mode-map))
  (set-keymap-parent map (make-composed-keymap aob-object-map ygg-aob--special-keep)))
(dolist (map (list aob-acp-mcp-mode-map))
  (set-keymap-parent map ygg-aob--special-keep))

(defun ygg-aob--modalize ()
  "Put this buffer in the modal layer; its mode keys are lifted above it."
  (yggdrasil-local-mode 1)
  (add-hook 'ygg-visual-exit-hook #'ygg--drop-region 90 t))

(defun ygg-aob--modalize-trace ()
  (ygg-aob--modalize)
  (use-local-map ygg-aob--trace-local-map))

(add-hook 'aob-trace-mode-hook #'ygg-aob--modalize-trace)
(dolist (hook '(aob-plan-mode-hook aob-acp-mcp-mode-hook))
  (add-hook hook #'ygg-aob--modalize))

;; agent mode/model under the localleader — moved off m/M so those keys stay
;; vim set-mark / middle-of-screen in trace and plan buffers
(require 'yggdrasil-localleader)
(require 'ygg-project-scan)
(declare-function aob-acp-set-mode "aob-acp")
(declare-function aob-acp-model "aob-acp")
(declare-function aob-acp-backend "aob-acp")
(declare-function aob-acp-goal "aob-acp")
(declare-function aob-transcript-wake "aob-transcript" (s))
(declare-function aob-acp-cycle-mode "aob-acp")
(declare-function aob-acp-compact "aob-acp")
(declare-function aob-acp-clear "aob-acp")
(declare-function aob-acp-new "aob-acp")
(declare-function aob-acp-config "aob-acp")
(declare-function aob-acp-mcp "aob-acp")
(declare-function aob-deliver-to "aob-deliver")
(declare-function aob-acp-worker-effort "aob-acp" (s level))
;; how an agent answers is a property of the one in front of you, so it
;; is set from its own buffer: the localleader already knows which
;; session that is, where the global leader has to ask
(dolist (mode '(aob-trace-mode aob-plan-mode))
  (yggdrasil-localleader-def mode "S" #'aob-cancel "stop turn (twice: drop queue)")
  (yggdrasil-localleader-def mode "x" #'aob-acp-command "command")
  (yggdrasil-localleader-def mode "z" #'aob-acp-compact "compact context")
  (yggdrasil-localleader-def mode "Z" #'aob-acp-clear "clear context")
  (yggdrasil-localleader-def mode "N" #'aob-acp-new "new session here")
  (yggdrasil-localleader-def mode "d" #'aob-todo "todo list")
  (yggdrasil-localleader-def mode "a" #'ygg-aob-activity "activity → quickfix")
  (yggdrasil-localleader-def mode "y" #'aob-resolve "answer the decision")
  (yggdrasil-localleader-def mode "n" #'aob-rename-session "rename")
  (yggdrasil-localleader-def mode "f" #'aob-dired "files (dired)")
  (yggdrasil-localleader-def mode "m" #'aob-acp-set-mode "mode")
  (yggdrasil-localleader-def mode "M" #'aob-acp-cycle-mode "next mode")
  (yggdrasil-localleader-def mode "l" #'aob-acp-model "model")
  (yggdrasil-localleader-def mode "L" #'aob-acp-backend "same model, other backend")
  (yggdrasil-localleader-def mode "E" #'aob-acp-config "effort / options")
  (yggdrasil-localleader-def mode "c" #'aob-acp-mcp "mcp servers")
  (yggdrasil-localleader-def mode "g" #'aob-acp-goal "goal")
  (yggdrasil-localleader-def mode "r" #'aob-transcript-wake "wake it (resume acp)")
  (yggdrasil-localleader-def mode "t" #'ygg-aob-subagents "subagents → quickfix")
  (yggdrasil-localleader-def mode "F" #'aob-acp-add-folder "add a folder"))
(dolist (mode '(aob-plan-mode))
  (yggdrasil-localleader-def mode "p" #'aob-compose "compose")
  (yggdrasil-localleader-def mode "k" #'aob-kill-session "kill session")
  (yggdrasil-localleader-def mode "w" #'aob-deliver-to "answer goes…"))

;; a queued message is still yours until it goes: change it or take it
;; back, from the line it is drawn on
(yggdrasil-localleader-def 'aob-trace-mode "e" #'aob-trace-queue-edit "queued: rewrite")
(yggdrasil-localleader-def 'aob-trace-mode "X" #'aob-trace-queue-drop "queued: drop")
(yggdrasil-localleader-def 'aob-trace-mode "s" #'aob-trace-queue-steer "queued: say it now (idle: send held)")
(yggdrasil-localleader-def 'aob-trace-mode "RET" #'aob-trace-queue-send-now "queued: send now (stops the turn)")
(yggdrasil-localleader-def 'aob-trace-mode "K" #'aob-trace-queue-earlier "queued: move earlier")
(yggdrasil-localleader-def 'aob-trace-mode "J" #'aob-trace-queue-later "queued: move later")
(yggdrasil-localleader-def 'aob-trace-mode "u" #'aob-trace-usage "usage: time, tokens, cost")
(yggdrasil-localleader-def 'aob-trace-mode "A" #'aob-answer "answer its questions")
(yggdrasil-localleader-def 'aob-trace-mode "H" #'aob-handoff "hand off to a fresh session")
(yggdrasil-localleader-def 'aob-trace-mode "b" #'ygg-aob-browser "preview in a browser pane")
(autoload 'ygg-projects-toggle-pin "ygg-projects" nil t)
(dolist (mode '(aob-trace-mode aob-plan-mode))
  (yggdrasil-localleader-def mode "P" #'ygg-projects-toggle-pin "pin session")
  (yggdrasil-localleader-def mode "W" #'aob-acp-worker-effort "worker effort"))

(declare-function ygg-ex--cmd-write "yggdrasil-ex" (range bang args))
(declare-function aob-trace-send "aob-trace")
(declare-function aob-trace-comment-send-now "aob-trace")
(declare-function aob-trace--name "aob-trace" (s))

(defun ygg-aob--write-sends (fn &rest args)
  "Make :w send, where the buffer is a prompt rather than a file.
A draft and the line at the foot of a trace are both things you finish
and let go of; the key that means \"I am done with this text\" is
already in the hand."
  (cond ((derived-mode-p 'aob-compose-mode) (aob-compose-send))
        ((derived-mode-p 'aob-trace-mode) (aob-trace-send))
        (t (apply fn args))))

(defun ygg-aob--wq-sends (fn &rest args)
  "Make :wq in a comment hold it and send every one held, as C-return does."
  (if (bound-and-true-p aob-trace-comment-mode)
      (aob-trace-comment-send-now)
    (apply #'ygg-aob--write-sends fn args)))

(with-eval-after-load 'yggdrasil-ex
  (advice-add 'ygg-ex--cmd-write :around #'ygg-aob--write-sends)
  (advice-add 'ygg-ex--cmd-wq :around #'ygg-aob--wq-sends)
  (advice-add 'ygg-ex--cmd-quit :around #'ygg-aob--quit-cancels))

(defun ygg-aob--quit-cancels (fn &rest args)
  "Make :q drop a draft or comment box, or decline what a trace's agent
waits on, as ZQ does."
  (cond ((derived-mode-p 'aob-compose-mode) (aob-compose-abort))
        ((and (derived-mode-p 'aob-trace-mode)
              (aob-trace-waiting-decision (aob-session-get aob-trace--session-id)))
         (aob-trace-decline))
        (t (apply fn args))))

(defvar ygg-quickscope-inhibit)

;; the trace is prose, not code: f and t still jump, but their preview
;; is a one-cell mark designed for a monospace grid
(defun ygg-aob--no-quickscope ()
  (setq-local ygg-quickscope-inhibit t))

(dolist (hook '(aob-trace-mode-hook aob-plan-mode-hook))
  (add-hook hook #'ygg-aob--no-quickscope))

(defun ygg-aob--draft-target ()
  "The session this draft is going to, when it is going to one."
  (when-let* ((tgt (bound-and-true-p aob-compose--target))
              ((stringp tgt)))
    (aob-session-get tgt)))

(defun ygg-compose-transient ()
  "Set what this draft runs under.
A draft to a session that is already up sets that session: its mode,
its model, the options it answers under.  A draft that will spawn one
picks the preset it spawns under, which is where cwd, permissions and
the servers it is handed are decided."
  (interactive)
  (if-let* ((s (ygg-aob--draft-target)))
      (pcase (completing-read (format "%s runs under: " (aob-session-name s))
                              '("mode" "next mode" "model" "backend" "effort / options"
                                "add folder")
                              nil t)
        ("mode" (aob-acp-set-mode s))
        ("next mode" (aob-acp-cycle-mode s))
        ("model" (aob-acp-model s))
        ("backend" (aob-acp-backend s))
        ("add folder" (ygg-aob--add-folder s))
        (_ (aob-acp-config s)))
    (let ((root (ygg-aob--draft-root)))
      (if (and (aob-acp--worktree-choices root)
               (equal (completing-read "Draft spawns under: " '("preset" "worktree")
                                       nil t)
                      "worktree"))
          (ygg-aob--set-draft-tree (aob-acp-read-worktree root))
        (let ((preset (completing-read "Draft spawns under: " (aob-acp-names)
                                       nil t nil nil aob-acp-default-agent)))
          (setq aob-compose--target (cons 'new preset)
                aob-compose--label (concat "→ new " preset))
          (force-mode-line-update)
          (message "aob: this draft spawns %s" preset))))))

(defvar-local ygg-aob--draft-tree nil
  "The worktree this draft's spawn works in, as aob-acp-read-worktree answers.")

(defun ygg-aob--draft-root ()
  "The repository this draft's spawn would start in."
  (let ((aob-acp-start-dir (bound-and-true-p aob-compose--dir)))
    (aob-acp--project)))

(defun ygg-aob--set-draft-tree (tree)
  "Spawn this draft in TREE, nil for its own, and say so among the title's tags."
  (setq ygg-aob--draft-tree tree
        aob-compose--tags
        (append (when tree
                  (list (concat "⌥ " (file-name-nondirectory
                                      (directory-file-name
                                       (if (consp tree) (car tree) tree))))))
                (seq-remove (lambda (tag) (string-prefix-p "⌥ " tag)) aob-compose--tags)))
  (force-mode-line-update))

(defun ygg-aob-draft-add-folder ()
  "Let the session this draft goes to see one more folder."
  (interactive)
  (ygg-aob--add-folder
   (or (ygg-aob--draft-target)
       (user-error "aob: this draft spawns its session; pick its worktree under modes"))))

(defun ygg-aob--add-folder (s)
  "Ask for a folder and let S see it, refusing before asking when S cannot."
  (aob-acp--can-add-folder s)
  (aob-acp-add-folder s (aob-acp--read-folder s)))

(yggdrasil-localleader-def 'aob-compose-mode "m" #'ygg-compose-transient "modes")
(yggdrasil-localleader-def 'aob-compose-mode "F" #'ygg-aob-draft-add-folder "add a folder")
(yggdrasil-localleader-def 'aob-compose-mode "q" #'aob-compose-hide "hide the box")
(yggdrasil-localleader-def 'aob-compose-mode "p" #'ygg-preset-edit "edit a preset")
(setq aob-compose-panel-hint "\\ m modes")

;; \ a on an agent → its file activity as a quickfix.  Built from ACP tool
;; `:locations' (the universal field every adapter populates), so it works
;; for claude, codex, and hermes alike — where claude-only subagent
;; nesting could not — and inherits clickable rows, ]l/[l nav, wgrep, and
;; the SPC q l toggle for free.
(declare-function ygg-qf--collect "layer-quickfix")
(declare-function ygg-qf-buffer "layer-quickfix")

(defun ygg-aob-activity (s)
  "Collect S's file activity into the quickfix, each row labelled by the
subagent (Task) that did it — its NAME, not a repeated path.  Main-agent
actions keep their tool title.  The short path stays the clickable target."
  (interactive (list (aob-target)))
  (let ((names (make-hash-table :test 'equal))
        lines)
    ;; tool-id → subagent name: a Task event carries :children and :title
    (dolist (ev (aob-session-events s))
      (when (or (plist-get ev :subagent) (plist-get ev :children))
        (puthash (plist-get ev :tool-id) (plist-get ev :title) names)))
    (dolist (ev (reverse (aob-session-events s)))
      (dolist (loc (plist-get ev :locations))
        (when-let* ((path (plist-get loc :path)))
          (let* ((sub (and (plist-get ev :parent)
                           (gethash (plist-get ev :parent) names)))
                 (label (cond
                         (sub (concat "└ " (aob--first-line sub 60)))
                         ((plist-get ev :title)
                          (aob--first-line (plist-get ev :title) 60))
                         (t (or (plist-get ev :kind) "tool")))))
            (push (format "%s:%d: %s" path (or (plist-get loc :line) 1) label)
                  lines))))
      ;; a finished subagent's children were collapsed off the ring; their
      ;; locations ride the surviving Task, still under its name
      (when-let* ((title (and (plist-get ev :children) (plist-get ev :title)))
                  (label (concat "└ " (aob--first-line title 60))))
        (dolist (loc (plist-get ev :child-locs))
          (when-let* ((path (plist-get loc :path)))
            (push (format "%s:%d: %s" path (or (plist-get loc :line) 1) label)
                  lines)))))
    (unless lines
      (user-error "aob: %s has no located activity yet" (aob-session-name s)))
    (unless (fboundp 'ygg-qf--collect) (require 'layer-quickfix))
    (let ((default-directory (or (aob-session-dir s) (aob-session-project s)
                                 default-directory)))
      (ygg-qf--collect (nreverse lines) t))))

;; \ t on an agent → its subagents as a quickfix, one row each; a row opens
;; the subagent's own trace.  The list follows the session's tick for as
;; long as nothing else has replaced it, so a status never goes stale.  When
;; the session goes the list stays as a last snapshot: its rows say so.
(declare-function ygg-qf-define-kind "layer-quickfix")
(declare-function ygg-qf-show-kind "layer-quickfix")
(declare-function ygg-qf-kind-refresh "layer-quickfix")
(declare-function ygg-qf-kind-at-point "layer-quickfix")
(declare-function ygg-qf-kind-target-id "layer-quickfix")
(declare-function ygg-qf-kind-list-p "layer-quickfix")
(declare-function aob-subagents--lines "aob-trace")
(declare-function aob-subagents--status "aob-trace")
(declare-function aob-subagents--open "aob-trace")
(declare-function aob-subagents--goto "aob-trace")
(defvar aob--views)

(defun ygg-aob--subagents-collect (s)
  (cl-mapcar (lambda (row ev)
               (list (cons (aob-session-id s) (plist-get ev :seq))
                     row (aob-session-name s)))
             (aob-subagents--lines s) (aob-session-subagents s)))

(defun ygg-aob--subagents-live-p (s)
  (eq s (aob-session-get (aob-session-id s))))

(defun ygg-aob--subagent-open (id)
  (if-let* ((s (aob-session-get (car id))))
      (aob-subagents--open s (cdr id))
    (user-error "aob: that session is gone")))

(defun ygg-aob-subagent-open-trace (&optional id)
  "Open the trace of the subagent ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (ygg-aob--subagent-open (or id (user-error "aob: no subagent here"))))

(defun ygg-aob-subagent-jump-call (&optional id)
  "Go to the call of the subagent ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (unless id (user-error "aob: no subagent here"))
  (aob-subagents--goto (or (aob-session-get (car id))
                           (user-error "aob: that session is gone"))
                       (cdr id)))

(defvar ygg-aob-subagent-map
  (let ((m (make-sparse-keymap)))
    (define-key m "o" #'ygg-aob-subagent-open-trace)
    (define-key m "c" #'ygg-aob-subagent-jump-call)
    m)
  "What embark offers on a subagent row of the quickfix.")

(defvar ygg-aob--subagent-timers nil
  "Alist of (BUFFER . TIMER) for the lists whose running rows count seconds.")

(defun ygg-aob--subagents-running-p (s)
  (seq-some (lambda (ev) (equal (aob-subagents--status ev) "running"))
            (aob-session-subagents s)))

(defun ygg-aob--subagents-stop (buf)
  (when-let* ((timer (alist-get buf ygg-aob--subagent-timers nil nil #'eq)))
    (cancel-timer timer))
  (setq ygg-aob--subagent-timers (assq-delete-all buf ygg-aob--subagent-timers)))

(defun ygg-aob--subagents-tick (buf token s)
  (cond ((not (ygg-aob--subagents-running-p s)) (ygg-aob--subagents-stop buf))
        ((not (get-buffer-window buf t)))
        ((not (ygg-qf-kind-refresh buf token)) (ygg-aob--subagents-stop buf))))

(defun ygg-aob--subagents-start (buf token s)
  "Count seconds on the running rows of BUF, one timer however often it is asked."
  (when (and (ygg-aob--subagents-running-p s)
             (not (alist-get buf ygg-aob--subagent-timers nil nil #'eq)))
    (push (cons buf (run-with-timer 1 1 #'ygg-aob--subagents-tick buf token s))
          ygg-aob--subagent-timers)))

(defun ygg-aob--subagents-arm (buf token s)
  (letrec ((render (lambda ()
                     (if (ygg-qf-kind-refresh buf token)
                         (ygg-aob--subagents-start buf token s)
                       (funcall disarm))))
           (disarm (lambda ()
                     (ygg-aob--subagents-stop buf)
                     (when (eq (alist-get buf aob--views nil nil #'eq) render)
                       (setq aob--views (assq-delete-all buf aob--views))))))
    (aob-register-view buf render)
    (ygg-aob--subagents-start buf token s)
    disarm))

(with-eval-after-load 'layer-quickfix
  (ygg-qf-define-kind 'subagents
                      :collect #'ygg-aob--subagents-collect
                      :action #'ygg-aob--subagent-open
                      :map 'ygg-aob-subagent-map
                      :arm #'ygg-aob--subagents-arm
                      :live-p #'ygg-aob--subagents-live-p))

(defun ygg-aob-subagents (s)
  "Collect every subagent S called into the quickfix; a row opens its trace."
  (interactive (list (aob-target)))
  (unless (aob-session-subagents s)
    (user-error "aob: %s has called no subagents" (aob-session-name s)))
  (require 'layer-quickfix)
  (let ((default-directory (or (aob-session-dir s) (aob-session-project s)
                               default-directory)))
    (ygg-qf-show-kind 'subagents s)))

(defvar aob-buffer-session-id)


;; same env treatment as the SPC a a terminals: per-project config home
;; (CLAUDE_CONFIG_DIR via marker file / ~/.agents-conf) and the mise tool
;; env inside a login shell
(setq aob-acp-environment-function
      (lambda (agent project _dir &optional _isolate)
        ;; the project's config home, the same one SPC a s and SPC a a
        ;; hand their agents.  An isolated connection is a process of its
        ;; own, not a login of its own: a home per worker is a worker
        ;; that has never authenticated, which is an agent that cannot
        ;; start
        (when-let* ((env (ygg-agent--known-config-env agent agent project)))
          (list env))))

(setq aob-acp-session-env-function #'ygg-pi-session-env)

(setq aob-acp-prepare-function
      (lambda (agent project &optional _isolate)
        (ygg-agent--config-env agent agent project)))

(setq aob-acp-command-function
      (lambda (argv)
        (list (or (getenv "SHELL") "/bin/zsh") "-l" "-c"
              (concat (if (fboundp 'ygg-mise-prefix) (ygg-mise-prefix) "")
                      (mapconcat #'shell-quote-argument argv " ")))))

;; a draft whose caller named a folder spawns there, not in the space the
;; owner wandered into while writing it; one that named none falls to the
;; resolver rather than freezing whatever buffer it was opened from
(setq aob-compose-spawn-function
      (lambda (text &optional agent atts)
        (let ((aob-acp-start-dir aob-compose--dir)
              (aob-acp-start-worktree ygg-aob--draft-tree)
              (aob-acp-session-refs (append (ygg-aob--preset-limits text)
                                            (bound-and-true-p aob-acp-session-refs))))
          (aob-acp-spawn (or agent aob-acp-default-agent) text atts))))

;; an agent belongs to the work it was started on: the task in front of the
;; owner, else the checkout the buffer is in, else the space — a space pinned
;; at home must not send an agent there while a repository is on screen
(declare-function ygg-task-root-of-here "ygg-task")
(declare-function ygg-space-dir "yggdrasil-spacetree" (&optional tab))
(declare-function ygg-space-new-on "yggdrasil-spacetree" (dir))
(declare-function ygg-space-root "yggdrasil-spacetree" (dir))
(declare-function ygg-space--dir-of "yggdrasil-spacetree" (tab))
(defun ygg-aob--buffer-repo ()
  "The repository this buffer is in, or nil outside any checkout."
  (when-let* ((root (locate-dominating-file default-directory ".git")))
    (file-name-as-directory (expand-file-name root))))

(setq aob-acp-start-dir-function
      (lambda ()
        (or (and (fboundp 'ygg-task-root-of-here)
                 (ignore-errors (ygg-task-root-of-here)))
            (ygg-aob--buffer-repo)
            ;; outside any checkout, the project open in the sidebar is
            ;; the one being worked on, and a session started there is
            ;; one the sidebar can show
            (bound-and-true-p ygg-projects--open)
            (and (fboundp 'ygg-space-dir) (ygg-space-dir)))))

;; compose and artifact review speak vim only: ZZ sends / approves,
;; ZA attaches an image, ZQ aborts.  ZZ/ZQ ride command remaps; ZA
;; lives on the normal-state Z prefix — a chord in the major-mode map
;; would swallow the letter Z while typing in insert state
(define-key aob-compose-mode-map [remap ygg-save-and-kill-buffer] #'aob-compose-send)
(define-key aob-compose-mode-map [remap ygg-kill-buffer-no-save] #'aob-compose-abort)
(define-key aob-artifact-mode-map [remap ygg-save-and-kill-buffer] #'aob-artifact-approve)

;; p in a compose buffer attaches a clipboard image before it means
;; kill-ring paste — a screenshot lands as 🖼 instead of noise
(defun ygg-aob-compose-paste-after ()
  (interactive)
  (if-let* ((f (aob-compose--clipboard-image)))
      (progn (aob-compose-attach f)
             (message "aob: clipboard image attached"))
    (call-interactively #'ygg-paste-after)))
(define-key aob-compose-mode-map [remap ygg-paste-after] #'ygg-aob-compose-paste-after)

(defun ygg-aob-z-attach ()
  "ZA: attach an image to the compose buffer at hand."
  (interactive)
  (if (derived-mode-p 'aob-compose-mode)
      (call-interactively #'aob-compose-attach)
    (user-error "ZA attaches images in compose buffers")))

(yggdrasil-define-keys 'ygg-z-cap-map
  "A" #'ygg-aob-z-attach :label "attach (compose)")

(setq aob-compose-hint "ZZ send · ZA attach · ZQ abort")

;; the draft floats at the foot of the frame, where a prompt line belongs;
;; the window action below is what a terminal frame falls back to
(setq aob-compose-float t)
(setq aob-compose-float-poshandler #'posframe-poshandler-frame-bottom-center)

(defun ygg-aob--compose-display (buffer alist)
  "Show BUFFER as a box over the conversation it is going to.
Above that window rather than across the frame: a draft belongs to one
conversation, and the sidebar and whatever else is open are not its to
rearrange."
  (when-let* ((win (ygg-aob--conversation-window)))
    (with-selected-window win
      (display-buffer-in-direction buffer (cons '(direction . above) alist)))))

(setq aob-compose-display-action
      '((display-buffer-reuse-window
         ygg-aob--compose-display
         display-buffer-at-bottom)
        (window-height . 12)
        (preserve-size . (nil . t))))

;; a buffer opened to be written starts in insert state (after
;; yggdrasil-local-mode has already forced normal on mode change)
(defun ygg-aob--compose-in-insert (buffer)
  "Put the compose BUFFER in insert state, wherever the call stood."
  (when (and (bufferp buffer) (buffer-live-p buffer) (fboundp 'ygg-insert-state))
    (with-current-buffer buffer (ygg-insert-state)))
  buffer)

(advice-add 'aob-compose :filter-return #'ygg-aob--compose-in-insert)

(defun ygg-aob--compose-shown-in-insert (buffer &optional no-focus)
  "Put BUFFER in insert state when the box is shown with focus.
A draft hidden and shown again, or opened by a mode, is for typing into,
and the leader must not answer a space typed into it."
  (when (and (not no-focus) (bufferp buffer) (buffer-live-p buffer)
             (fboundp 'ygg-insert-state))
    (with-current-buffer buffer
      (when (derived-mode-p 'aob-compose-mode) (ygg-insert-state)))))

(advice-add 'aob-compose-show :after #'ygg-aob--compose-shown-in-insert)

(defun ygg-aob-compose-space ()
  "A space typed into the box in normal state: enter insert and type it."
  (interactive)
  (ygg-insert-state)
  (insert " "))

(defvar ygg-aob--compose-normal-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'ygg-aob-compose-space)
    map)
  "Keys lifted above normal state in the compose box: a space is a space.")

(yggdrasil-define-mode-keys 'aob-compose-mode 'normal ygg-aob--compose-normal-map)

;;; corfu under the word, not over the box

(defun ygg-aob--corfu-lift (x y height lh yb child top-height)
  "Where the popup goes on the top frame, as (X . Y).
X and Y are what corfu worked out inside CHILD, the box's own frame, and
YB is the pixel row under the word; corfu flips the popup above the word
whenever the box is too short for it, so the popup is put back under
the word when the top frame, TOP-HEIGHT tall, has room for HEIGHT there,
and above it otherwise; LH is the line height.  CHILD's position is
added so the popup lands where the word is on the screen."
  (let* ((pos (frame-position child))
         (cx (or (car pos) 0))
         (cy (or (cdr pos) 0))
         (under (+ cy yb))
         (ny (if (<= (+ under height) top-height)
                 under
               (max 0 (- (+ cy yb) lh height)))))
    (ignore y)
    (cons (+ cx x) ny)))

(defun ygg-aob--corfu-make-frame (orig frame x y width height)
  "Make corfu's popup a child of the top frame when it stands in a box.
ORIG is corfu's own maker; FRAME, X, Y, WIDTH and HEIGHT are its
arguments, X and Y relative to the frame the window is on."
  ;; corfu calls this from its own popup buffer, so the box is the window's
  (let ((child (window-frame))
        (box (window-buffer)))
    (if (or (not (frame-parent child))
            (not (with-current-buffer box
                   (derived-mode-p 'aob-compose-mode))))
        (funcall orig frame x y width height)
      (let* ((top (ygg-ui-main-frame child))
             (at (with-current-buffer box
                   (let* ((lh (default-line-height))
                          (yb (+ (cadr (window-inside-pixel-edges))
                                 (or (cdr (posn-x-y (posn-at-point))) 0) lh)))
                     (ygg-aob--corfu-lift x y height lh yb child
                                          (frame-pixel-height top))))))
        (cl-letf (((symbol-function 'window-frame) (lambda (&optional _w) top)))
          (funcall orig frame (car at) (cdr at) width height))))))

(with-eval-after-load 'corfu
  (advice-add 'corfu--make-frame :around #'ygg-aob--corfu-make-frame))

;; corfu pops the instant / or @ is typed (global prefix is 2)
(add-hook 'aob-compose-mode-hook
          (lambda ()
            (when (boundp 'corfu-auto-prefix)
              (setq-local corfu-auto-prefix 1))))

;;; Spaces — a session belongs to the space it was spawned in; the
;;; sidebar dot covers terminals and ACP alike

(defun ygg-aob--remember-space (s)
  "Stamp S with the space it belongs to, unless it already named one.
A search or a Work names the space its tree belongs to, which is not
always the space you started it from — the board aims at one tree while
standing in another."
  (when (and (fboundp 'ygg-space--current-id)
             (null (aob-session-ref s :space)))
    (aob-session-put s :space (ygg-space--current-id))))

(defun ygg-aob-session-subagents (s)
  "The subagents S is running now, in the order it sent them.
The tree shows what is still working; `ygg-aob-subagents' keeps every
delegation, finished ones included."
  (seq-filter (lambda (ev) (member (plist-get ev :status)
                                   '("pending" "in_progress")))
              (aob-session-subagents s)))

(defun ygg-aob--space-live-p (id)
  "Non-nil when ID still names a space."
  (and id (fboundp 'ygg-space--tabs)
       (seq-some (lambda (tab) (eql id (ygg-space--id-of tab))) (ygg-space--tabs))))

(defun ygg-aob--task-space-id (s)
  "The space dedicated to S's task, when its task has one."
  (when-let* ((key (aob-session-ref s :task))
              ((fboundp 'ygg-space--tabs)))
    (seq-some (lambda (tab)
                (and (equal key (alist-get 'ygg-task tab)) (ygg-space--id-of tab)))
              (ygg-space--tabs))))

(defun ygg-aob--own-space (s)
  "The live space S belongs to, re-attaching it when its own has closed.
A space closes while its agent is still working — archiving a task closes
the whole subtree — and an agent left filed under an id nothing answers
to is invisible in every list that groups by space.  Asked rather than
swept, so an orphan heals the moment anything looks for it."
  (let ((id (aob-session-ref s :space)))
    (if (ygg-aob--space-live-p id)
        id
      (let ((heir (or (ygg-aob--task-space-id s)
                      (and (fboundp 'ygg-space--current-id)
                           (ygg-space--current-id)))))
        (when heir
          (aob-session-put s :space heir)
          (when (fboundp 'ygg-cockpit-rename-buffers)
            (ygg-cockpit-rename-buffers s)))
        heir))))

(defun ygg-aob-session-space (s)
  "The live space S works in: its lead's, since a subagent works there."
  (let* ((lead (aob-subagent-lead s))
         (space (ygg-aob--own-space lead)))
    (when (and space (not (eq lead s)) (not (eql space (aob-session-ref s :space))))
      (aob-session-put s :space space)
      (when (fboundp 'ygg-cockpit-rename-buffers)
        (ygg-cockpit-rename-buffers s)))
    space))

(add-hook 'aob-session-created-hook #'ygg-aob--remember-space)

;;; A space per agent — an agent gets a child space under the project it
;;; was sent into, so its trace, plan and terminal live together and the
;;; project's own space keeps the work the owner was doing.

(defcustom ygg-aob-space-per-agent t
  "Give every agent a child space of its own when it starts."
  :type 'boolean :group 'aob)

(declare-function ygg-space-child "yggdrasil-spacetree")
(declare-function ygg-space-rename "yggdrasil-spacetree" (name))

(declare-function ygg-space--tabs "yggdrasil-spacetree" ())
(declare-function ygg-space--id-of "yggdrasil-spacetree" (tab))
(declare-function ygg-space-task-p "yggdrasil-spacetree" (tab))
(declare-function ygg-space--spawn "yggdrasil-spacetree" (parent))
(declare-function ygg-space--set "yggdrasil-spacetree" (tab key val))
(declare-function ygg-space--tab-by-id "yggdrasil-spacetree" (id))
(declare-function ygg-space--goto-id "yggdrasil-spacetree" (id))
(declare-function ygg-space--current-id "yggdrasil-spacetree" ())

(defun ygg-aob--project-space-id (dir)
  "The space standing for DIR\='s tree, if one does.
An agent\='s space hangs off its project, not off whichever agent space
the last spawn left you standing in: that is how ten agents end up ten
levels deep, each a child of the one before it."
  (when-let* ((root (and (fboundp 'ygg-space-root) (ygg-space-root dir)))
              ((fboundp 'ygg-space--tabs))
              ((fboundp 'ygg-space--dir-of)))
    (seq-some (lambda (tab)
                (and (not (and (fboundp 'ygg-space-task-p) (ygg-space-task-p tab)))
                     ;; an agent's own space stands on the same tree and
                     ;; is not the tree: hanging the next agent off it is
                     ;; the staircase this is here to stop
                     (not (alist-get 'ygg-agent tab))
                     (when-let* ((home (ygg-space--dir-of tab)))
                       (equal (file-name-as-directory (expand-file-name home))
                              (file-name-as-directory (expand-file-name root))))
                     (ygg-space--id-of tab)))
              (ygg-space--tabs))))

(defvar ygg-aob--resuming-acp-id nil
  "The conversation id a resume is bringing back, while its session opens.")

(defun ygg-aob--bind-resumed-acp-id (fn entry &rest args)
  "Let the session ENTRY resumes know its conversation id before it opens.
The id reaches the session only once the agent answers, and by then its
space has been chosen."
  (let ((ygg-aob--resuming-acp-id (plist-get entry :acp-id)))
    (apply fn entry args)))

(advice-add 'aob-acp-resume-entry :around #'ygg-aob--bind-resumed-acp-id)

(defun ygg-aob--acp-id (s)
  (or (aob-session-ref s :acp-id) ygg-aob--resuming-acp-id))

(defun ygg-aob--top-session (s)
  "The session at the head of S's line of senders, or S itself."
  (let ((seen (list s)) parent)
    (while (and (setq parent (aob-session-get (aob-session-ref s :parent-session)))
                (not (memq parent seen)))
      (push parent seen)
      (setq s parent))
    s))

(defun ygg-aob--agent-tab (s)
  "The live space made for S, or nil.
The one a saved session restored for S's conversation, else the one made
for S in this Emacs.  Names are reused after a restart and conversation
ids never are, so a restored space is matched by the id alone."
  (when (fboundp 'ygg-space--tabs)
    (let ((acp (ygg-aob--acp-id s))
          (tabs (ygg-space--tabs)))
      (or (and acp (seq-find (lambda (tab) (equal (alist-get 'ygg-agent-acp tab) acp))
                             tabs))
          (seq-find (lambda (tab)
                      (and (eql (ygg-space--id-of tab) (aob-session-ref s :space))
                           (equal (alist-get 'ygg-agent tab) (aob-session-id s))
                           (null (alist-get 'ygg-agent-acp tab))))
                    tabs)))))

(defun ygg-aob--claim-tab (s tab)
  "File S and the subagents it sent under TAB, tag TAB for S; return its id."
  (let ((id (ygg-space--id-of tab)))
    (ygg-space--set tab 'ygg-agent (aob-session-id s))
    (when-let* ((acp (ygg-aob--acp-id s)))
      (ygg-space--set tab 'ygg-agent-acp acp))
    (dolist (each (aob-sessions))
      (when (eq (ygg-aob--top-session each) s)
        (aob-session-put each :space id)))
    id))

(defun ygg-aob--stamp-space (s &rest _)
  "Write S's conversation id on the space made for it, once it has one."
  (when-let* ((acp (aob-session-ref s :acp-id))
              (id (aob-session-ref s :space))
              ((fboundp 'ygg-space--set)))
    (dolist (frame (frame-list))
      (dolist (tab (funcall tab-bar-tabs-function frame))
        (when (and (eql (ygg-space--id-of tab) id)
                   (equal (alist-get 'ygg-agent tab) (aob-session-id s))
                   (null (alist-get 'ygg-agent-acp tab)))
          (ygg-space--set tab 'ygg-agent-acp acp))))))

(add-hook 'aob-state-change-hook #'ygg-aob--stamp-space)

(defun ygg-aob--spawn-space (s)
  "Nest a fresh space for S under its project, land in it; return its id."
  (let* ((default-directory (or (aob-session-dir s)
                                (aob-session-project s)
                                default-directory))
         ;; the project's space, else the root: anything but the space
         ;; the last agent made, which is where you are standing when its
         ;; spawn has just finished
         (parent (or (ygg-aob--project-space-id default-directory)
                     (bound-and-true-p ygg-space--root-id)
                     (ygg-space--current-id)))
         (id (ygg-aob--claim-tab s (ygg-space--spawn parent))))
    (ygg-space-rename (or (aob-session-name s) "agent"))
    id))

(defun ygg-aob--space-for-agent (s)
  "Give S its space: the one restored for its conversation, else a new one
nested under its project and named after it.
A conversation opened for reading gets none: it has no process, and a
space is where a process works.  A subagent gets none either: it works
in the space of the session that sent it, and opening it must not take
you out of that one."
  (when-let* (((ygg-aob--subagent-p s))
              (parent (aob-session-get (aob-session-ref s :parent-session)))
              (space (aob-session-ref parent :space)))
    (aob-session-put s :space space))
  (when (and ygg-aob-space-per-agent
             (not (ygg-aob--subagent-p s))
             (not (aob-session-ref s :hidden))
             (fboundp 'ygg-space--spawn)
             (fboundp 'ygg-space-rename)
             ;; a session that is created already finished is a
             ;; conversation opened for reading: its `:asleep' entry is
             ;; put on it a line after this hook runs, so the state is
             ;; what there is to go on
             (not (aob-session-ref s :asleep))
             (not (memq (aob-session-state s) '(done dead failed))))
    (condition-case err
        (let ((read-here (ygg-aob--trace-on-screen-p s)))
          (if-let* ((tab (ygg-aob--agent-tab s)))
              (progn (unless read-here (ygg-space--goto-id (ygg-space--id-of tab)))
                     (ygg-aob--claim-tab s tab))
            (unless read-here (ygg-aob--spawn-space s))))
      (error (message "aob: no space for %s (%s)"
                      (aob-session-name s) (error-message-string err))))))

(add-hook 'aob-session-created-hook #'ygg-aob--space-for-agent 90)

(defun ygg-aob--trace-on-screen-p (s)
  "Whether S already has a trace in a window here: one it took over.
A conversation brought back while you read it stays where you read it;
landing in a space made for it is the trace going out from under you.
Its space is made when it is next opened, as for a session whose own
space is gone."
  (when-let* (((fboundp 'aob-trace--name))
              (buf (get-buffer (aob-trace--name s))))
    (and (get-buffer-window buf) t)))

(defun ygg-aob-ensure-space (s)
  "The space S works in, made now when S is top-level and its own is gone.
A subagent answers with the space of the session that sent it.  Only
opening a session asks this: a list heals an orphan into wherever you
stand, and a list that made spaces would make one per agent per redraw."
  (let ((top (ygg-aob--top-session s)))
    (when (and ygg-aob-space-per-agent
               (fboundp 'ygg-space--spawn)
               (not (aob-session-ref top :asleep)))
      (if-let* ((tab (ygg-aob--agent-tab top)))
          (ygg-aob--claim-tab top tab)
        (unless (memq (aob-session-state top) '(done dead failed))
          (ygg-aob--spawn-space top))))))

;; a trace stands beside the work, not over it.  ygg-ui-show hands a
;; reader the main window, which is right for something you go and read
;; and wrong for something that streams while you keep working: the
;; buffer you were in would be the thing that disappeared.
(defconst ygg-agent-session-env-vars
  '("PI_ACP_PI_COMMAND" "AOB_PI_MCP_SERVERS" "AOB_PI_APPROVE")
  "Variables only an aob session's own process may carry.
What `ygg-pi-session-env' hands a connection; a shell or task started
from an agent buffer inherits them and has to drop them.")

(defun ygg-agent-terminal-env (&optional project)
  "The environment entries a shell or task in PROJECT should start with.
One config-home entry per kind of CLI that keeps a home, pointing at the
same home the agents spawned from here are given, so a CLI run by hand
and one run by aob are the same install, logged in once.  A project on
the shared home gets none, as an agent there does: the CLI keeps what it
inherits.  A remote project gets those variables unset, since a local
home means nothing there.  The per-session variables are unset in every
case.  An unset is a bare name, which Emacs passes on as removal."
  (let ((project (or project default-directory)))
    (append
     (if (file-remote-p project)
         (delete-dups (mapcar (lambda (home) (plist-get (cdr home) :var))
                              ygg-agent--config-homes))
       (delq nil (mapcar (lambda (kind) (ygg-agent--config-env kind kind project))
                         (mapcar #'car ygg-agent--config-homes))))
     ygg-agent-session-env-vars)))

(defun ygg-agent--terminal-env ()
  "Point a terminal's CLI agents at its own project's config home.
Every terminal, however it was opened: a claude run by hand and one
spawned by aob are then the same install, logged in once."
  (setq process-environment
        (append (ygg-agent-terminal-env default-directory) process-environment)))

(add-hook 'ghostel-pre-spawn-hook #'ygg-agent--terminal-env)

(defcustom ygg-aob-trace-action
  '((display-buffer-reuse-window ygg-aob--trace-window display-buffer-pop-up-window)
    (inhibit-same-window . t))
  "How a session's trace is put on screen."
  :type 'sexp :group 'aob)

(defun ygg-aob--trace-window (buffer alist)
  "Put BUFFER where a conversation can be read: the widest window there
is, split when it can afford two and taken over when it cannot.
Splitting whatever window happens to be selected is how a trace ends
up nineteen columns wide in a frame with room for four of them."
  (let* ((cands (seq-remove
                 (lambda (w)
                   (or (window-parameter w 'window-side)
                       ;; the caller said not this one, and handing it
                       ;; back anyway is how this returns nothing and
                       ;; the fallback splits something in half
                       (and (cdr (assq 'inhibit-same-window alist))
                            (eq w (selected-window)))))
                 (window-list nil 'no-minibuf)))
         (widest (car (sort cands (lambda (a b) (> (window-total-width a)
                                                   (window-total-width b)))))))
    (when (window-live-p widest)
      (if (>= (window-total-width widest) (* 2 ygg-aob-trace-min-width))
          ;; room for both: the conversation takes its columns off the
          ;; right of the widest window and leaves the rest of it
          (when-let* ((new (ignore-errors
                             (split-window widest (- ygg-aob-trace-min-width)
                                           'right))))
            (window--display-buffer buffer new 'window alist))
        ;; no room for both: the conversation takes that window whole,
        ;; which is better than two windows too narrow to read
        (window--display-buffer buffer widest 'reuse alist)))))

(setq aob-acp-show-trace nil)
(setq aob-acp-native-subagents t
      aob-acp-async-tasks t)

(defvar ygg-aob--compose-sending nil
  "Non-nil while a draft is on its way out.
A trace that opens because something started in the background should
not take the point; one that opens because you just sent to it should.")

(defun ygg-aob--conversation-window ()
  "A window already given over to a conversation, if the frame has one.
One window holds whatever conversation you are reading: a resumed or
forked session is a new buffer, and without this each one splits the
frame again until the sidebar is squeezed out of it."
  (seq-find (lambda (w)
              (and (not (window-parameter w 'window-side))
                   (not (window-dedicated-p w))
                   (buffer-local-value 'aob-buffer-session-id (window-buffer w))))
            (window-list nil 'no-minibuf)))

(defcustom ygg-aob-subagent-action
  '((display-buffer-reuse-window
     ygg-aob--beside-conversation
     display-buffer-below-selected)
    (window-height . 0.4))
  "How a subagent's trace is put on screen: beside the one that sent it."
  :type 'sexp :group 'aob)

(defun ygg-aob--subagent-p (s)
  "Whether S is a session another session sent."
  (and (fboundp 'aob-session-ref) (aob-session-ref s :parent-session) t))

(defun ygg-aob--beside-conversation (buffer alist)
  "Put BUFFER under the conversation window, leaving it where it is."
  (when-let* ((win (ygg-aob--conversation-window)))
    (with-selected-window win
      (display-buffer-below-selected buffer alist))))

(defcustom ygg-aob-trace-min-width 80
  "Columns a conversation gets, where the frame has them to give.
A trace holds commands, diffs and paths as well as prose; thirty
columns of it breaks words in half and reads as a fault."
  :type 'natnum :group 'aob)

(defun ygg-aob--widen-trace (win)
  "Give WIN `ygg-aob-trace-min-width\=' columns, taking them from its
neighbours — never from a side window, which is pinned."
  (when (and (window-live-p win)
             (> ygg-aob-trace-min-width (window-total-width win))
             (not (window-parameter win 'window-side)))
    ;; columns, not pixels: the fifth argument is PIXELWISE, and a
    ;; delta of forty-two pixels is five columns of nothing
    (ignore-errors
      (window-resize win (- ygg-aob-trace-min-width (window-total-width win))
                     t))))

(defun ygg-aob--show-trace (s)
  "Show S's trace in the window conversations are read in.
A window already showing it is where it stays.  A subagent opens
beside the conversation that sent it instead: what it was sent to do
is read against what was being done, and taking the window would hide
the one you were reading."
  (unless noninteractive
    (when-let* ((buf (ignore-errors (aob-trace-buffer s)))
                (win (or (get-buffer-window buf)
                         (and (ygg-aob--subagent-p s)
                              (ignore-errors
                                (display-buffer buf ygg-aob-subagent-action)))
                         (when-let* ((w (and (not (ygg-aob--subagent-p s))
                                             (ygg-aob--conversation-window))))
                           (unless (eq (window-buffer w) buf)
                             (set-window-buffer w buf))
                           w)
                         (ignore-errors (display-buffer buf ygg-aob-trace-action)))))
      ;; every window showing it, not only the one just chosen: a trace
      ;; can be on screen twice, and the cramped one is the one you are
      ;; looking at
      (dolist (w (get-buffer-window-list buf nil nil)) (ygg-aob--widen-trace w))
      (when (and ygg-aob--compose-sending (window-live-p win))
        (select-window win))
      win)))

(defun ygg-aob--compose-opens-trace (fn &rest args)
  "Put the conversation a draft went to on screen, and stand in it.
The target is read first: sending kills the draft, and with it the
buffer-local that says where the words were going."
  (let* ((tgt (and (boundp 'aob-compose--target)
                   (not (bound-and-true-p aob-compose--anchor))
                   aob-compose--target))
         (ygg-aob--compose-sending t))
    (prog1 (apply fn args)
      (when-let* ((s (and (stringp tgt) (aob-session-get tgt))))
        (ygg-aob--show-trace s)))))

(advice-add 'aob-compose-send :around #'ygg-aob--compose-opens-trace)

(defun ygg-aob--show-new-trace (s)
  "Show a new session's trace; a subagent its agent runs opens only when asked."
  (unless (or (aob-session-ref s :native-tool-id) (aob-session-ref s :workflow-agent)
              (aob-session-ref s :hidden))
    (ygg-aob--show-trace s)))

(add-hook 'aob-session-created-hook #'ygg-aob--show-new-trace 95)

(defun ygg-aob--trace-of-subagent (fn s &rest args)
  "Open a subagent's trace beside the conversation, not over it."
  (if (ygg-aob--subagent-p s)
      (ygg-aob--show-trace s)
    (apply fn s args)))

(advice-add 'aob-trace :around #'ygg-aob--trace-of-subagent)

;; a project is not always one checkout.  whatever folders it carries
;; are handed to its agents as additional directories, so work that
;; spans repositories does not need a session per repository
(declare-function ygg-project-folders "ygg-project-scan" (root))

(defun ygg-aob--with-project-dirs (fn &rest args)
  "Give the session being spawned the folders its project carries."
  (let* ((root (or (bound-and-true-p aob-acp-start-dir)
                   (ignore-errors (project-root (project-current nil)))
                   default-directory))
         (dirs (ignore-errors (ygg-project-folders root)))
         (aob-acp-session-refs
          (if dirs
              (append (list :extra-dirs dirs)
                      (bound-and-true-p aob-acp-session-refs))
            (bound-and-true-p aob-acp-session-refs))))
    (apply fn args)))

(advice-add 'aob-acp-spawn :around #'ygg-aob--with-project-dirs)

;; and the skills it carries.  These cannot ride in the config home —
;; its skills entry is the shared link to the user's — so they are put
;; where the CLI looks for a project's own, beside the work
(defun ygg-aob--with-project-skills (fn &rest args)
  "Give the session being spawned the skills its project carries."
  (let ((root (or (bound-and-true-p aob-acp-start-dir)
                  (ignore-errors (project-root (project-current nil)))
                  default-directory)))
    (ignore-errors (ygg-agent-link-project-skills root)))
  (apply fn args))

(advice-add 'aob-acp-spawn :around #'ygg-aob--with-project-skills)

;; and what it is running inside.  An agent that does not know the editor
;; has an MCP server shells out for what the editor already knows, and
;; delegates inside its own context instead of opening a session
(declare-function ygg-agent-remove-instructions "ygg-agent-conf" (home))
(defvar ygg-agent-instructions)

;; what this editor offers goes out with every session, in the request
;; that opens it, and not in a memory file only some config homes carry
(setq aob-acp-system-append (lambda (_s) ygg-agent-instructions))

(defun ygg-aob--with-instructions (fn agent &rest args)
  "Clear the old written copy of the instructions from the home the session uses.
The system prompt carries them now; left in the memory file they are
read twice."
  (when (and (fboundp 'ygg-agent--config-env)
             (fboundp 'ygg-agent-remove-instructions))
    (ignore-errors
      (let* ((root (or (bound-and-true-p aob-acp-start-dir)
                       (ignore-errors (project-root (project-current nil)))
                       default-directory))
             (entry (ygg-agent--config-env agent agent root))
             (home (and (stringp entry) (string-match "=\\(.*\\)\\'" entry)
                        (match-string 1 entry))))
        (ygg-agent-remove-instructions home))))
  (apply fn agent args))

(advice-add 'aob-acp-spawn :around #'ygg-aob--with-instructions)

;; and every server its own configuration declares.  A session gets what
;; session/new carries and nothing else, so an agent started from here
;; would otherwise reach fewer tools than the same agent started by hand
(declare-function ygg-agent-user-mcp-servers "ygg-agent-conf" (agent &optional project))

(defun ygg-aob--with-user-mcp (fn agent name project &rest rest)
  "Hand the session AGENT's own configured servers, beside ours."
  (let ((aob-acp-mcp-servers
         (append (bound-and-true-p aob-acp-mcp-servers)
                 (ignore-errors (ygg-agent-user-mcp-servers agent project)))))
    (apply fn agent name project rest)))

(advice-add 'aob-acp--open :around #'ygg-aob--with-user-mcp)

(defun ygg-aob--worktree-skills (fn project dir done &rest rest)
  "Link PROJECT's skills into worktree DIR the moment it exists.
The spawn cannot do this itself: the worktree is made on its way out."
  (apply fn project dir
         (lambda (err)
           (unless err (ignore-errors (ygg-agent-link-project-skills dir)))
           (funcall done err))
         rest))

(advice-add 'aob-acp--worktree-make :around #'ygg-aob--worktree-skills)

;; whatever a compose box does to the window layout on its way in, the
;; sidebar is not collateral: it goes back if it went
(advice-add 'aob-compose :around #'ygg-projects-keep-open)

(defun ygg-aob--sidebar-shows-clock-p (s)
  "Whether the sidebar is on screen with a row drawn for S.
Only the open card draws its sessions' clocks."
  (when-let* ((buf (get-buffer ygg-projects-buffer-name))
              ((get-buffer-window buf 'visible)))
    (with-current-buffer buf
      (text-property-any (point-min) (point-max) 'ygg-entry s))))

(add-hook 'aob-clock-shown-functions #'ygg-aob--sidebar-shows-clock-p)


;; trace buffers join their session's space bucket so SPC b b lists
;; them; layer-sessions' kill/close machinery then manages them free
(defvar aob-trace--session-id)
(defvar ygg--space-buffers)

(defun ygg-aob--space-of (s)
  "The space S's buffers belong in: its lead's, since a subagent works there."
  (or (aob-session-ref (aob-subagent-lead s) :space)
      (aob-session-ref s :space)))

(defun ygg-aob--adopt-trace (buf)
  (with-current-buffer buf
    ;; where the buffer stands is the trace's own call: it follows the
    ;; agent into the repository it works in when its folder is none
    (when-let* ((s (aob-session-get aob-trace--session-id)))
      (when (boundp 'ygg--space-buffers)
        (when-let* ((id (ygg-aob--space-of s)))
          (cl-pushnew buf (gethash id ygg--space-buffers))))))
  buf)

(advice-add 'aob-trace-buffer :filter-return #'ygg-aob--adopt-trace)

(defvar aob-plan--session-id)

(defun ygg-aob--adopt-plan (buf)
  (when (boundp 'ygg--space-buffers)
    (with-current-buffer buf
      (when-let* ((s (aob-session-get aob-plan--session-id))
                  (id (ygg-aob--space-of s)))
        (cl-pushnew buf (gethash id ygg--space-buffers)))))
  buf)

(advice-add 'aob-plan-buffer :filter-return #'ygg-aob--adopt-plan)

;;; Keep agent trace/plan windows out of the persisted session — a killed
;;; agent's window otherwise restores a stale buffer into a space's layout.

(defun ygg-aob--agent-window-p (win)
  "Non-nil if WIN shows an agent trace/plan buffer (live or a stale restore)."
  (let ((buf (window-buffer win)))
    (or (buffer-local-value 'aob-buffer-session-id buf)
        (string-match-p "\\`[ *]*\\(Old buffer \\)?\\(trace\\|plan\\):"
                        (buffer-name buf)))))

(defun ygg-aob--prune-agent-windows ()
  "Delete every window showing an agent buffer, keeping a frame's sole window."
  (dolist (frame (frame-list))
    (dolist (win (window-list frame 'no-minibuf))
      (when (and (window-live-p win)
                 (not (eq win (frame-root-window frame)))
                 (ygg-aob--agent-window-p win))
        (ignore-errors (delete-window win))))))

(defvar ygg-aob--saved-wconf nil)

(defun ygg-aob--before-session-save ()
  "Hide agent windows so a killed agent's trace is not baked into the layout.
The save is synchronous, so restoring the config in the after-save hook
leaves the live frame unchanged with no redisplay in between."
  (setq ygg-aob--saved-wconf
        (delq nil (mapcar (lambda (f)
                            (and (frame-live-p f)
                                 (cons f (with-selected-frame f
                                           (current-window-configuration)))))
                          (frame-list))))
  (mapc #'ygg-aob--stamp-space (aob-live-sessions))
  (ygg-aob--prune-agent-windows))

(defun ygg-aob--after-session-save ()
  (dolist (fc ygg-aob--saved-wconf)
    (when (frame-live-p (car fc))
      (with-selected-frame (car fc)
        (set-window-configuration (cdr fc)))))
  (setq ygg-aob--saved-wconf nil))

(defun ygg-aob--after-session-load ()
  "Drop agent windows a stale session restored, and refresh the sidebar."
  (ygg-aob--prune-agent-windows)
  (when (fboundp 'ygg-space-tree--sync) (ygg-space-tree--sync)))

(with-eval-after-load 'easysession
  (add-hook 'easysession-before-save-hook #'ygg-aob--before-session-save)
  (add-hook 'easysession-after-save-hook #'ygg-aob--after-session-save)
  (add-hook 'easysession-after-load-hook #'ygg-aob--after-session-load))

(defun ygg-aob--score (s)
  ;; a failed subagent nudges its session up, but never above blocked
  (+ (pcase (aob-session-state s)
       ('blocked 4)
       ((or 'working 'starting) 2)
       ('idle 1)
       (_ 0))
     (if (> (or (aob-session-ref s :turn-fails) 0) 0) 1 0)))

(defun ygg-aob--space-state-face (space-id)
  "Face for SPACE-ID's sidebar dot from its worst ACP session state, or nil."
  (let (faces)
    (dolist (s (aob-live-sessions))
      (when (eql (ygg-aob-session-space s) space-id)
        (push (pcase (aob-session-state s)
                ('blocked 'error)
                ((or 'working 'starting) 'warning))
              faces)))
    (car (seq-sort-by (lambda (f) (or (alist-get f ygg-space-state-rank) 0)) #'>
                      (delq nil faces)))))

(add-to-list 'ygg-space-state-functions #'ygg-aob--space-state-face)

(defun ygg-aob-goto-space (s)
  "Go to the space agent S works in, making one when its own is gone.
A subagent's space is the one of the session that sent it."
  (when-let* ((id (ygg-aob-ensure-space s))
              ((not (eql id (ygg-space--current-id)))))
    (ygg-space--goto-id id)))

(defun ygg-aob--goto (s)
  "Go to agent S's space and show its trace there.
An already-visible trace is refocused; otherwise it opens in place."
  (ygg-aob-goto-space s)
  (let ((buf (aob-trace-buffer s)))
    (if-let* ((win (get-buffer-window buf)))
        (select-window win)
      (ygg-ui-show buf))))

;;; Agents in spaces — each space's sidebar row grows one line per live
;;; agent (state glyph, name, ctx); RET on a line jumps to it.  Rows are
;;; ambient (on by default, only render when agents exist); SPC p t
;;; toggles the panel itself, SPC p a the rows.

(defvar ygg-aob--tree-agents-on t)
(defvar ygg-space-pick-all)
(defvar ygg-space-tree-width)
(defvar ygg-space-tree--on)
(declare-function ygg-space-tree "yggdrasil-spacetree")

(defvar ygg-aob--tree-line-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'ygg-aob-tree-goto)
    m))

(defun ygg-aob-tree-goto ()
  "Jump to the agent on this sidebar row."
  (interactive)
  (when-let* ((id (get-text-property (point) 'aob-session))
              (s (aob-session-get id)))
    (when-let* ((window (ygg-ui-main-window)))
      (select-window window))
    (ygg-aob--goto s)))

(defun ygg-aob--tree-glyph (s)
  (pcase (aob-session-state s)
    ('blocked (propertize "■" 'face 'error))
    ((or 'working 'starting) (propertize "●" 'face 'warning))
    (_ (propertize "○" 'face 'shadow))))

(defface ygg-aob-tree-task '((t :inherit link :underline nil))
  "Face for an ACP agent that is working a task.
The theme's blue, without the underline a link wears.")

(defun ygg-aob--tree-name (s)
  "S's name, blue when it is an ACP agent with a task behind it.
Everything else in a space is ad hoc — a shell, a question, a session
started to look at something.  An agent holding a task is the one whose
work is filed somewhere, and the sidebar should say so at a glance."
  (let ((name (aob-session-name s)))
    (if (and (eq (aob-session-backend s) 'acp)
             (aob-session-ref s :task))
        (propertize name 'face 'ygg-aob-tree-task)
      name)))

(defun ygg-aob--tree-line (s depth)
  (let ((line (truncate-string-to-width
               (format "   %s%s %s%s %s"
                       (make-string (* 2 depth) ?\s)
                       (concat (ygg-aob--tree-glyph s)
                               (when-let* ((quiet (aob-session-quiet s)))
                                 (propertize (concat " " quiet) 'face 'shadow)))
                       (ygg-aob--tree-name s)
                       (let ((q (length (aob-session-ref s :queued))))
                         (if (> q 0)
                             (propertize (format " »%d" q) 'face 'shadow)
                           ""))
                       (propertize
                        (or (car (split-string (or (aob-session-ctx s) "") "/")) "")
                        'face 'shadow))
               (or (bound-and-true-p ygg-space-tree-width) 18))))
    (propertize (if (ygg-projects--ended-subagent-p s)
                    (propertize line 'face 'shadow)
                  line)
                'aob-session (aob-session-id s)
                'keymap ygg-aob--tree-line-map
                'mouse-face 'highlight)))

(defvar ygg-aob--tree-finished-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'ygg-aob-tree-toggle-finished)
    m))

(defun ygg-aob-tree-toggle-finished ()
  "Show or fold the finished subagents this sidebar row stands for."
  (interactive)
  (when-let* ((id (get-text-property (point) 'aob-finished-of)))
    (ygg-aob-toggle-finished id)
    (when (fboundp 'ygg-space-tree--queue) (ygg-space-tree--queue))))

(defun ygg-aob--tree-finished-line (row depth)
  (propertize (truncate-string-to-width
               (concat "   " (make-string (* 2 depth) ?\s) (ygg-aob--finished-label row))
               (or (bound-and-true-p ygg-space-tree-width) 18))
              'aob-finished-of (nth 1 row)
              'keymap ygg-aob--tree-finished-map
              'mouse-face 'highlight))

(defun ygg-aob--tree-details (space-id)
  (when ygg-aob--tree-agents-on
    (let* ((here (seq-filter (lambda (s)
                               (eql (ygg-aob-session-space s) space-id))
                             (ygg-aob--listed-sessions)))
           (tops (seq-sort-by #'ygg-aob--score #'>
                              (seq-filter (lambda (s) (ygg-aob--pick-top-p s here)) here))))
      (mapcan (lambda (tree)
                (mapcar (lambda (row)
                          (if (aob-session-p (car row))
                              (ygg-aob--tree-line (car row) (cdr row))
                            (ygg-aob--tree-finished-line (car row) (cdr row))))
                        (cdr tree)))
              (ygg-aob--forest tops here)))))

(add-to-list 'ygg-space-detail-functions #'ygg-aob--tree-details)

(defun ygg-aob-tree-agents ()
  "Toggle agent rows inside the space sidebar."
  (interactive)
  (setq ygg-aob--tree-agents-on (not ygg-aob--tree-agents-on))
  (when (and ygg-aob--tree-agents-on
             (not (bound-and-true-p ygg-space-tree--on))
             (fboundp 'ygg-space-tree))
    (ygg-space-tree))
  (when (fboundp 'ygg-space-tree--queue) (ygg-space-tree--queue))
  (message "aob: agents in spaces %s"
           (if ygg-aob--tree-agents-on "on" "off")))

;;; One targeting gesture: live sessions worst-attention-first, plus
;;; "+ agent" rows to spawn a new one — agent choice happens here,
;;; before any composing

(defun ygg-aob--read-target (&optional include-new)
  "Pick a live session (worst first), a resumable \"⟲\" older session, or —
with INCLUDE-NEW — a \"+ agent\" row.
Returns a session, (resume . ENTRY), or (new . AGENT-NAME)."
  (let* ((aob--read-map
          (append
           (mapcar (lambda (s) (cons (aob-session-name s) s))
                   (seq-sort-by #'ygg-aob--score #'>
                                (seq-remove #'ygg-aob--subagent-p (aob-live-sessions))))
           (mapcar (lambda (e)
                     (cons (format "⟲ %s · %s" (plist-get e :name)
                                   (file-name-nondirectory
                                    (directory-file-name (plist-get e :project))))
                           (cons 'resume e)))
                   (aob-acp-resumable-entries))
           (when include-new
             (mapcar (lambda (a) (cons (concat "+ " (car a)) (cons 'new (car a))))
                     aob-acp-agents))))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . aob-session)
                                 (annotation-function . aob--annotate)
                                 (display-sort-function . identity))
                    (complete-with-action action (mapcar #'car aob--read-map)
                                          str pred))))
         (choice (completing-read "Agent: " table nil t)))
    (cdr (assoc choice aob--read-map))))

(defun ygg-aob--region-seed ()
  (when (use-region-p)
    (format "```\n%s\n```\n\n"
            (substring-no-properties
             (filter-buffer-substring (region-beginning) (region-end))))))

(defvar ygg-aob--home-map nil
  "Label to (KIND . VALUE) while the picker is open, for its affixation.")

(defun ygg-aob--folder-icon ()
  (if (fboundp 'nerd-icons-faicon)
      (condition-case nil (nerd-icons-faicon "nf-fa-folder_o") (error "⌂"))
    "⌂"))

(defun ygg-aob--homes ()
  "Where a new agent could go, as (LABEL . (KIND . VALUE)).
A label is the name alone — what the folder is, and whether the row joins
a space or opens one, are affixes, so typing filters on the name and not
on the path behind it.  The space you are in leads, because most of the
time the answer is here."
  (let* ((tabs (and (fboundp 'ygg-space--tabs) (ygg-space--tabs)))
         (here (and (fboundp 'ygg-space--current-id) (ygg-space--current-id)))
         (mine (seq-find (lambda (tb) (eql here (ygg-space--id-of tb))) tabs))
         (ordered (if mine (cons mine (remq mine tabs)) tabs))
         (seen (make-hash-table :test #'equal))
         homes)
    (dolist (tab ordered)
      ;; two spaces may carry one name, and the picker answers by label
      (let* ((name (ygg-space--name tab))
             (label (if (gethash name seen)
                        (format "%s #%s" name (ygg-space--id-of tab))
                      name)))
        (puthash name t seen)
        (push (cons label (cons 'space tab)) homes)))
    (dolist (root (ygg-project-roots))
      (push (cons (abbreviate-file-name root) (cons 'folder root)) homes))
    (nreverse homes)))

(defun ygg-aob--home-affix (labels)
  "Give LABELS their icon and their folder, for marginalia to align."
  (let ((icon (concat (ygg-aob--folder-icon) " ")))
    (mapcar
     (lambda (label)
       (list label icon
             (propertize
              (pcase (cdr (assoc label ygg-aob--home-map))
                (`(space . ,tab)
                 (if-let* ((d (ygg-space--dir-of tab)))
                     (concat "   " (abbreviate-file-name d))
                   "   no folder"))
                (_ "   opens a new space"))
              'face 'completions-annotations)))
     labels)))

(defun ygg-aob--read-home ()
  "Ask where a new agent goes.  Answers, and does not act — the caller
takes every answer before anything is created, so quitting leaves no
half-made space behind."
  (let* ((ygg-aob--home-map (ygg-aob--homes))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . ygg-agent-home)
                                 (affixation-function . ygg-aob--home-affix)
                                 (display-sort-function . identity))
                    (complete-with-action
                     action (mapcar #'car ygg-aob--home-map) str pred))))
         (pick (completing-read "Space or folder: " table nil nil))
         (found (cdr (assoc pick ygg-aob--home-map))))
    (or found (cons 'folder (expand-file-name pick)))))

(defun ygg-aob--home-dir (home)
  "HOME's folder, without entering it."
  (pcase home
    (`(space . ,tab) (or (ygg-space-dir tab) default-directory))
    (`(folder . ,dir) (if (fboundp 'ygg-space-root)
                          (ygg-space-root dir)
                        (expand-file-name dir)))))

(defun ygg-aob--enter-home (home)
  "Go to HOME, opening a space for it when it names a folder."
  (pcase home
    (`(space . ,tab) (ygg-space--goto-id (ygg-space--id-of tab)))
    (`(folder . ,dir) (ygg-space-new-on dir))))

(defun ygg-aob-talk-new ()
  "Compose to a fresh agent slot: pick a definition (default preselected,
RET is the fast path) or a ⟲ persisted session, which resumes with its
whole conversation.  Always another concurrent agent.
Asks where it goes — a space it joins, or a folder that gets a space of
its own — then which agent.  Both answers are taken before anything is
created, so quitting either prompt leaves no space behind, and the
selection is read before the space switch that would otherwise lose it."
  (interactive)
  (let* ((seed (ygg-aob--region-seed))
         (home (ygg-aob--read-home))
         (dir (ygg-aob--home-dir home))
         (default-directory dir)
         (map (append
               (mapcar (lambda (a) (cons (car a) (cons 'new (car a))))
                       aob-acp-agents)
               (mapcar (lambda (e)
                         (cons (format "⟲ %s · %s" (plist-get e :name)
                                       (file-name-nondirectory
                                        (directory-file-name
                                         (plist-get e :project))))
                               (cons 'resume e)))
                       (aob-acp-resumable-entries))))
         (choice (completing-read "Agent: " (mapcar #'car map)
                                  nil t nil nil aob-acp-default-agent))
         (tree (and (eq (car-safe (cdr (assoc choice map))) 'new)
                    (aob-acp-read-worktree dir))))
    (ygg-aob--enter-home home)
    ;; the spawn happens on send, from the folder the draft names —
    ;; `aob-compose' takes it from here, and a resumed row from its session
    (let ((buf (aob-compose (ygg-aob--materialize (cdr (assoc choice map))) seed nil dir)))
      (when tree
        (with-current-buffer buf (ygg-aob--set-draft-tree tree))))))

(defun ygg-aob-resume-pick ()
  "Resume a stored session, in a new space on a folder you choose.
Which folder decides whose history you are offered: an agent lists the
sessions it stored for that tree, so browsing from the wrong one shows an
empty list and looks like the sessions are gone.
Both answers are taken before the space is made.  The session list itself
arrives from the agent afterwards, so quitting at that last prompt does
leave the new space behind."
  (interactive)
  (let* ((home (ygg-aob--read-home))
         (dir (ygg-aob--home-dir home))
         (agent (completing-read "Agent: " (mapcar #'car aob-acp-agents)
                                 nil t nil nil aob-acp-default-agent))
         (aob-acp-start-dir-function (lambda () dir)))
    (ygg-aob--enter-home home)
    (aob-acp-resume-from-list agent)))

(defun ygg-aob--materialize (target)
  "A picked TARGET becomes composable: resume rows respawn their session."
  (pcase target
    (`(resume . ,e) (aob-acp-resume-entry e))
    (_ target)))

(defun ygg-aob-talk-existing ()
  "Compose to an agent: at point, the sole live one, or pick — older
persisted sessions appear as ⟲ rows and resume on selection."
  (interactive)
  (let ((live (seq-remove #'ygg-aob--subagent-p (aob-live-sessions)))
        (resumable (aob-acp-resumable-entries)))
    (unless (or live resumable) (user-error "no agents — c starts one"))
    (aob-compose (or (aob-session-at-point)
                     (if (and live (null (cdr live)) (null resumable))
                         (car live)
                       (ygg-aob--materialize (ygg-aob--read-target))))
                 (ygg-aob--region-seed))))


(defcustom ygg-aob-pick-width 30
  "Most columns the picker gives one agent to say what it is on.
Ten agents at this width would not fit the echo area, so it is a ceiling
and the line divides what the frame actually has between them."
  :type 'natnum :group 'aob)

(defun ygg-aob--pick-room (n)
  "Columns each of N entries gets: the frame's, share and share alike."
  (max 12 (min ygg-aob-pick-width
               (- (/ (max 40 (- (frame-width) 6)) (max n 1)) 4))))

(defun ygg-aob--task-slug (s)
  "The slug of the task S is on, read off the directory that keys it.
The key is the task's directory and the slug is its name, so answering
costs no scan of the roots."
  (when-let* ((key (aob-session-ref s :task)))
    (file-name-nondirectory key)))

(defun ygg-aob--put-down-p (s)
  "Non-nil when the task S is on has been put down.
An archived task is history, and so is its agent, whatever it is still
holding open.  Archiving moves the task's directory, so a key that names
no directory is the whole answer."
  (when-let* ((key (aob-session-ref s :task)))
    (not (file-directory-p key))))

(defun ygg-aob--doing (s &optional width)
  "What S is on, the way a picker has to read it: its task, else its name.
An agent started for a task is named for it, so the two agree — but one
adopted by a task after the fact is not, and `a claude   s claude-2' is
not a list anyone can pick out of."
  (ygg-ui-cut (or (ygg-aob--task-slug s) (aob-session-name s))
              (or width ygg-aob-pick-width)))

(defun ygg-aob-switch ()
  "Go to any live session, whatever project or space it is in.
The most in need of you first, each with its state and its project, so
a name two projects share is told apart by where it is."
  (interactive)
  (let* ((live (seq-sort-by #'ygg-aob--score #'>
                            (seq-remove #'ygg-aob--put-down-p (aob-live-sessions))))
         (cands (mapcar (lambda (s) (cons (ygg-aob--switch-label s) s)) live)))
    (unless cands (user-error "no live sessions"))
    (ygg-aob--goto
     (cdr (assoc (completing-read "Session: "
                                  (lambda (str pred action)
                                    (if (eq action 'metadata)
                                        '(metadata (display-sort-function . identity))
                                      (complete-with-action action cands str pred)))
                                  nil t)
                 cands)))))

(defun ygg-aob--switch-label (s)
  "S as one picker row: name, state, clock and spend, then whose it is."
  (format "%-28s %-8s %s %s"
          (truncate-string-to-width (aob-session-name s) 28 nil nil "…")
          (aob-session-state s)
          (propertize (format "%-12s %-8s"
                              (or (aob-session-clock s t) "")
                              (or (aob-session-spend s) ""))
                      'face 'shadow)
          (or (aob-subagent-of s)
              (abbreviate-file-name
               (directory-file-name
                (or (aob-session-project s) (aob-session-dir s) ""))))))

;;; Agents in the zone picker — SPC p z lists each space's agents under it

(defun ygg-aob--listed-sessions ()
  "The live sessions, and every subagent of a live lead, finished or not.
A put-down lead takes its subagents with it."
  (let* ((live (aob-live-sessions))
         (open (seq-remove #'ygg-aob--put-down-p live)))
    (seq-filter (lambda (s)
                  (let ((lead (aob-subagent-lead s)))
                    (if (and (not (eq lead s)) (memq lead live))
                        (and (memq lead open)
                             (not (aob-session-ref s :hidden)))
                      (memq s live))))
                (aob-sessions))))

(defun ygg-aob--pick-sessions (space)
  "Agents filed under SPACE, or under no live space when SPACE is nil.
A subagent is filed where its lead is, since that is where it works."
  (seq-filter (lambda (s)
                (and (not (ygg-aob--put-down-p s))
                     (let ((id (ygg-aob--space-of s)))
                       (if space
                           (eql id space)
                         (not (ygg-aob--space-live-p id))))))
              (ygg-aob--listed-sessions)))

(defcustom ygg-aob-finished-subagents-shown 5
  "How many of a parent's finished subagents its rows show, newest first.
The rest fold into one row that unfolds them; a working one always shows."
  :type 'integer :group 'aob)

(defcustom ygg-aob-pick-subagents nil
  "Whether the picker lists subagents.  SPC p Z lists them for one call;
a pinned subagent always shows."
  :type 'boolean :group 'aob)

(defvar ygg-aob--finished-unfolded nil
  "Ids of the parents whose finished subagents all show.")

(defun ygg-aob-toggle-finished (id)
  "Show all of the finished subagents of the parent ID, or fold them again."
  (setq ygg-aob--finished-unfolded
        (if (member id ygg-aob--finished-unfolded)
            (delete id ygg-aob--finished-unfolded)
          (cons id ygg-aob--finished-unfolded))))

(defun ygg-aob--forest (tops here)
  "Each of TOPS as (TOP . ROWS): TOP and what it sent among HERE, oldest
sending first, a level deeper per sending, each row (SESSION . DEPTH).
Past `ygg-aob-finished-subagents-shown' a parent's older finished
subagents fold into one row ((finished ID N) . DEPTH), N of them, nil
N once unfolded.  A session already drawn is not drawn again, so a loop
in the refs ends."
  (let ((seen nil)
        (oldest-first (reverse here)))
    (cl-labels ((sent (s) (seq-filter (lambda (kid) (eq (aob-subagent-parent kid) s))
                                      oldest-first))
                (busy-p (s trail)
                  (and (not (memq s trail))
                       (or (not (ygg-projects--ended-subagent-p s))
                           (seq-some (lambda (kid) (busy-p kid (cons s trail)))
                                     (sent s)))))
                (tree (s depth)
                  (unless (memq s seen)
                    (push s seen)
                    (let* ((kids (sent s))
                           (finished (seq-remove (lambda (kid) (busy-p kid nil)) kids))
                           (folded (seq-difference
                                    finished
                                    (last finished (max 0 ygg-aob-finished-subagents-shown))
                                    #'eq))
                           (id (aob-session-id s))
                           (unfolded (member id ygg-aob--finished-unfolded)))
                      (cons (cons s depth)
                            (append
                             (mapcan (lambda (kid) (tree kid (1+ depth)))
                                     (if unfolded
                                         kids
                                       (seq-difference kids folded #'eq)))
                             (when folded
                               (list (cons (list 'finished id
                                                 (unless unfolded (length folded)))
                                           (1+ depth))))))))))
      (mapcar (lambda (s) (cons s (tree s 0))) tops))))

(defun ygg-aob--finished-label (row)
  "What the folding ROW, (finished ID N), says."
  (propertize (if-let* ((n (nth 2 row)))
                  (format "+%d finished" n)
                "− fold finished")
              'face 'shadow))

(defun ygg-aob--entry-space (e)
  "The space for the longest folder holding persisted entry E's, or nil."
  (let ((dirs (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
                      (delq nil (list (plist-get e :dir) (plist-get e :project)))))
        (best nil)
        (best-length -1))
    (dolist (tab (ygg-space--tabs))
      (when-let* ((root (ygg-space-dir tab))
                  (root (file-name-as-directory (expand-file-name root)))
                  ((> (length root) best-length))
                  ((seq-some (lambda (d) (string-prefix-p root d)) dirs)))
        (setq best (ygg-space--id-of tab)
              best-length (length root))))
    best))

(defun ygg-aob--pinned-ended ()
  "Pinned conversations no session is holding, as (RANK . ENTRY)."
  (when-let* ((pins (ygg-projects--pins)))
    (delq nil (mapcar (lambda (e)
                        (when-let* ((rank (seq-position pins (plist-get e :acp-id))))
                          (cons rank e)))
                      (ignore-errors (aob-acp-resumable-entries))))))

(defun ygg-aob--pick-agent-row (s mark)
  (let ((label (concat mark (ygg-aob--switch-label s))))
    (cons (if (ygg-projects--ended-subagent-p s)
              (propertize label 'face 'shadow)
            label)
          (lambda () (ygg-aob--goto s)))))

(defun ygg-aob--pick-entry-row (e mark)
  (cons (concat mark
                (format "%-28s %-8s %s"
                        (truncate-string-to-width
                         (or (plist-get e :name) (plist-get e :agent) "session")
                         28 nil nil "…")
                        (propertize "⟲" 'face 'shadow)
                        (abbreviate-file-name
                         (directory-file-name
                          (or (plist-get e :project) (plist-get e :dir) "")))))
        (lambda () (aob-acp-resume-entry e))))

(defun ygg-aob--pick-rows (space)
  "Rows for the agents in SPACE: the pinned first in pin order, ended
pinned conversations among them to resume, then the rest most in need
first, each with what it sent under it, a level deeper per sending."
  (let* ((pin (propertize "⊤ " 'face 'shadow))
         (all (or ygg-aob-pick-subagents ygg-space-pick-all))
         (listed (ygg-aob--pick-sessions space))
         (here (seq-filter (lambda (s) (or all
                                           (not (memq (aob-subagent-parent s) listed))
                                           (ygg-projects--pinned-p s)))
                           listed))
         (tops (seq-filter (lambda (s) (or (ygg-aob--pick-top-p s here)
                                           (and (not all) (ygg-projects--pinned-p s))))
                           here))
         (pinned (seq-filter #'ygg-projects--pinned-p tops))
         (ended (seq-filter (lambda (p) (eql (ygg-aob--entry-space (cdr p)) space))
                            (ygg-aob--pinned-ended)))
         (heads (sort (append (mapcar (lambda (s) (cons (ygg-projects--pin-rank s) s)) pinned)
                              ended)
                      (lambda (a b) (< (car a) (car b)))))
         (rest (seq-sort-by #'ygg-aob--score #'> (seq-difference tops pinned #'eq)))
         (trees (ygg-aob--forest (append (seq-filter #'aob-session-p (mapcar #'cdr heads))
                                         rest)
                                 here)))
    (cl-flet ((rows (s mark)
                (mapcar (lambda (row)
                          (let ((indent (concat (if (eq (car row) s) mark "  ")
                                                (make-string (* 2 (cdr row)) ?\s))))
                            (if (aob-session-p (car row))
                                (ygg-aob--pick-agent-row (car row) indent)
                              (cons (concat indent (ygg-aob--finished-label (car row))
                                            (propertize
                                             (format " of %s"
                                                     (aob-session-name
                                                      (aob-session-get (nth 1 (car row)))))
                                             'face 'shadow))
                                    (lambda ()
                                      (ygg-aob-toggle-finished (nth 1 (car row)))
                                      (let ((ygg-space-pick-all all))
                                        (ygg-space-pick)))))))
                        (alist-get s trees))))
      (append
       (mapcan (lambda (p)
                 (if (aob-session-p (cdr p))
                     (rows (cdr p) pin)
                   (list (ygg-aob--pick-entry-row (cdr p) pin))))
               heads)
       (mapcan (lambda (s) (rows s "  ")) rest)))))

(defun ygg-aob--pick-top-p (s here)
  "Whether S heads a tree among HERE: nothing in HERE sent it, or the
chain that sent it loops back to S, which must not hide the whole loop."
  (let ((parent (aob-subagent-parent s)))
    (or (not (memq parent here))
        (let ((p parent) (seen nil))
          (while (and p (memq p here) (not (eq p s)) (not (memq p seen)))
            (push p seen)
            (setq p (aob-subagent-parent p)))
          (eq p s)))))

(add-hook 'ygg-space-pick-rows-functions #'ygg-aob--pick-rows)

(defun ygg-aob-pick ()
  "Go to an agent IN THE CURRENT SPACE, flash-style: labeled hints in the
echo area, one keypress jumps to that agent's own space.  Only this
space's agents are offered; worst attention sits on `a'; a sole agent
needs no key."
  (interactive)
  (let* ((space (and (fboundp 'ygg-space--current-id) (ygg-space--current-id)))
         (live (seq-sort-by
                #'ygg-aob--score #'>
                (seq-filter (lambda (s)
                              (and (not (ygg-aob--put-down-p s))
                                   (or (null space)
                                       ;; asked, not read: an agent whose
                                       ;; space has closed is re-attached
                                       ;; rather than dropping out of every
                                       ;; list at once
                                       (eql (ygg-aob-session-space s) space))))
                            (aob-live-sessions)))))
    (cond
     ((null live) (user-error "no agents in this space"))
     ((null (cdr live)) (ygg-aob--goto (car live)))
     (t (let* ((keys "asdfjkl;gh")
               (pairs (cl-loop for s in live
                               for i from 0
                               while (< i (length keys))
                               collect (cons (aref keys i) s)))
               (room (ygg-aob--pick-room (length pairs)))
               (hint (mapconcat
                      (lambda (p)
                        (format "%s %s%s"
                                (propertize (char-to-string (car p))
                                            'face 'error)
                                (ygg-aob--doing (cdr p) room)
                                (pcase (aob-session-state (cdr p))
                                  ('blocked (propertize " ■" 'face 'error))
                                  ('working (propertize " ●" 'face 'warning))
                                  (_ ""))))
                      pairs "   "))
               (ch (read-char (concat hint "  » ")))
               (hit (alist-get ch pairs)))
          (if hit
              (ygg-aob--goto hit)
            (user-error "no agent on %c" ch)))))))

(defun ygg-aob-resolve-next ()
  "Answer the one pending Decision, or list them all when there are several."
  (interactive)
  (if-let* ((s (aob-session-awaiting-answer)))
      (if (cdr (ygg-aob--decisions-collect))
          (ygg-aob-decisions)
        (aob-resolve s))
    (user-error "no pending decisions")))

(defun ygg-aob--attention-refresh (&rest _)
  (when (fboundp 'ygg-space-tree--queue) (ygg-space-tree--queue)))

(add-hook 'aob-state-change-hook #'ygg-aob--attention-refresh)
(add-hook 'aob-session-removed-hook #'ygg-aob--attention-refresh)
(add-hook 'aob-session-created-hook #'ygg-aob--attention-refresh)
(add-hook 'aob-queue-change-hook #'ygg-aob--attention-refresh)

(defun ygg-aob--say (msg &optional level)
  "Say MSG both ways a session's turn is announced, wherever either exists.
Neither surface is required from here, so a configuration without the UI
layer stays quiet rather than dying inside a state-change hook."
  (when (fboundp 'ygg-notify) (ygg-notify msg level))
  (when (fboundp 'ygg-agent--mac-notify) (ygg-agent--mac-notify msg)))

(defun ygg-aob--notify (s old new)
  (let ((name (aob-session-name s)))
    (pcase (and (not (aob-session-ref s :hidden)) new)
      ('blocked (ygg-aob--say (format "%s needs input" name) 'warn))
      ((and 'idle (guard (eq old 'working)))
       (ygg-aob--say (format "%s done" name))))))

(add-hook 'aob-state-change-hook #'ygg-aob--notify)

;;; Preview — what the agent is building, in a browser beside its trace

(autoload 'ygg-browser-pane "layer-browser")
(autoload 'ygg-browser-pane-reload "layer-browser")
(declare-function aob-trace--shell-output "aob-trace" (ev))

(defcustom ygg-aob-browser-reload t
  "Non-nil reloads a session's preview when its turn ends, if it is on screen."
  :type 'boolean :group 'aob)

(defcustom ygg-aob-browser-auto-open nil
  "Non-nil opens a session's preview the first time it names a local server.
Asked when a turn ends, of what the agent said and its commands printed."
  :type 'boolean :group 'aob)

(defconst ygg-aob--url-re
  (rx "http" (? "s") "://" (+ (not (any space "\"'<>`()[]{}|\\"))))
  "A web address, up to the first character that ends one in prose.")

(defconst ygg-aob--local-url-re
  (rx "http" (? "s") "://" (or "localhost" "127.0.0.1" "0.0.0.0" "[::1]")
      ":" (+ digit) (* (not (any space "\"'<>`()[]{}|\\"))))
  "A local server's address: this machine, with a port.")

(defun ygg-aob--event-words (ev)
  "What EV said or printed: an agent's message, or a command's output."
  (pcase (plist-get ev :type)
    ('message (aob-event-text ev))
    ('tool (and (equal (plist-get ev :kind) "execute")
                (string-join (aob-trace--shell-output ev) "\n")))))

(defun ygg-aob--event-urls (ev regexp)
  "REGEXP's matches in what EV said or printed, the last one first."
  (when-let* ((words (ygg-aob--event-words ev)))
    (let ((start 0) urls)
      (while (string-match regexp words start)
        (setq start (match-end 0))
        (push (string-trim-right (match-string 0 words) "[.,;:!?*_~]+") urls))
      urls)))

(defun ygg-aob--local-url (s)
  "The local server address S named or printed last, or nil."
  (seq-some (lambda (ev) (car (ygg-aob--event-urls ev ygg-aob--local-url-re)))
            (aob-session-events s)))

(defun ygg-aob--preview-url (s)
  "Where S's preview points: the URL at point, the one S was last shown,
the local server S named last, or one read with S's URLs to complete."
  (or (thing-at-point 'url t)
      (aob-session-ref s :preview-url)
      (ygg-aob--local-url s)
      (let ((url (completing-read
                  "Preview URL: "
                  (delete-dups (mapcan (lambda (ev) (ygg-aob--event-urls ev ygg-aob--url-re))
                                       (aob-session-events s))))))
        (if (string-empty-p url) (user-error "aob: no URL to preview") url))))

(defun ygg-aob-browser (s url)
  "Show S's preview at URL in a browser pane beside its trace.
URL is the one at point, else the one S was last shown, else the local
server S named last, else read.  S keeps the URL and the pane's buffer,
so the next preview of S reuses both."
  (interactive (let ((s (aob-target))) (list s (ygg-aob--preview-url s))))
  (let ((buf (aob-session-ref s :preview-buffer)))
    (aob-session-put s :preview-url url)
    (aob-session-put s :preview-buffer
                     (ygg-browser-pane url (if (buffer-live-p buf) buf
                                             (format "*preview: %s*" (aob-session-name s)))))))

(defun ygg-aob--preview-turn-end (s old new)
  "When S's turn ends, reload its preview if it is on screen, or open it
the first time S names a local server when `ygg-aob-browser-auto-open'."
  (when (and (eq old 'working) (eq new 'idle))
    (let ((buf (aob-session-ref s :preview-buffer)))
      (ignore-errors
        (cond ((buffer-live-p buf)
               (when (and ygg-aob-browser-reload (get-buffer-window buf t))
                 (ygg-browser-pane-reload buf)))
              ((and ygg-aob-browser-auto-open
                    (not (aob-session-ref s :preview-url))
                    (not (aob-session-ref s :hidden)))
               (when-let* ((url (ygg-aob--local-url s)))
                 (ygg-aob-browser s url))))))))

(add-hook 'aob-state-change-hook #'ygg-aob--preview-turn-end)

(defun ygg-aob--preview-forget (s)
  "Close S's preview with S: a page left open keeps hitting its server."
  (when-let* ((buf (aob-session-ref s :preview-buffer))
              ((buffer-live-p buf)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf))))

(add-hook 'aob-session-removed-hook #'ygg-aob--preview-forget)

(aob-modeline-mode 1)

;; the terminal layer's SPC a keys stay exactly as layer-agent defines
;; them; the whole ACP layer is five keys under SPC a c
;;; Embark on an agent — what you do to one agent, where the agent is.
;;; Point already answers `aob-target', so every verb here works on
;;; whatever row, trace or subagent line the cursor is on.  Workflows are
;;; not here: they run a fleet, not the agent under your cursor.

(declare-function aob-session-at-point "aob")
(declare-function aob-session-id "aob")
(defvar ygg-aob-agent-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'aob-acp-fork)
    (define-key map "e" #'aob-acp-config)
    (define-key map "!" #'aob-acp-restart)
    (define-key map "p" #'aob-compose-recall)
    (define-key map "g" #'aob-acp-goal)
    (define-key map "M" #'aob-acp-model)
    (define-key map "k" #'aob-kill-session)
    map)
  "What you can do to the agent under point.")

(defun ygg-aob--embark-agent ()
  "Tell embark the cursor is on an agent, when it is."
  (when-let* ((session (aob-session-at-point)))
    (cons 'ygg-agent (aob-session-name session))))

(with-eval-after-load 'embark
  (defvar embark-target-finders)
  (defvar embark-keymap-alist)
  (add-to-list 'embark-target-finders #'ygg-aob--embark-agent)
  (add-to-list 'embark-keymap-alist '(ygg-agent . ygg-aob-agent-map)))

(defun ygg-aob-force-kill-or-delete ()
  "Delete the agent this buffer belongs to, else force kill the buffer.
In a trace, a compose or a terminal of an ACP session, the session goes
for good: killed, forgotten by the resume list, every buffer of it closed."
  (interactive)
  (if-let* ((session (and (fboundp 'aob-session-at-point) (aob-session-at-point))))
      (aob-acp-delete-session session)
    (yggdrasil-leader--kill-buffer-force)))

(with-eval-after-load 'yggdrasil-leader
  (yggdrasil-define-keys 'ygg-leader-buffer-map
    "D" #'ygg-aob-force-kill-or-delete :label "force kill · delete agent"))

(defvar ygg-leader-acp-map (make-sparse-keymap) "The a c prefix: ACP sessions.")

;; Starting, reaching and ending an agent.  Fork, restart, config, prompt
;; recall and the four workflow verbs are real but occasional, and a panel
;; you read every day should not have to list them: they answer to M-x and
;; to `:', which is where a verb used twice a month belongs.
(yggdrasil-define-keys 'ygg-leader-acp-map
  "C" #'ygg-aob-talk-existing :label "talk existing"
  "R" #'ygg-aob-resume-pick :label "resume a folder"
  "o" #'ygg-aob-pick :label "go to"
  "r" #'ygg-aob-resolve-next :label "resolve"
  "q" #'aob-kill-session :label "kill")

(declare-function ygg-transient-acp "ygg-transient")

(defvar ygg-leader-agent-map (make-sparse-keymap) "The a prefix: AI agents.")
(yggdrasil-leader-def "a" ygg-leader-agent-map "agents")
(declare-function ygg-project-import "ygg-project-scan" (root &optional callback))
(declare-function ygg-projects-add "ygg-projects" (dir))
(declare-function ygg-projects-toggle-archived "ygg-projects" ())
(declare-function ygg-projects-toggle-past "ygg-projects" ())
(declare-function ygg-conversations "ygg-projects" (&optional all))

(yggdrasil-define-keys 'ygg-leader-agent-map
  "d" #'ygg-projects-sidebar :label "projects"
  ;; the verbs the sidebar has, where `:' and the leader can find them:
  ;; a command reachable only by a key in one buffer is a command you
  ;; have to already know about
  "v" #'ygg-conversations :label "conversations"
  "z" #'ygg-projects-toggle-past :label "old sessions on/off"
  "Z" #'ygg-projects-toggle-archived :label "archived on/off")

(yggdrasil-define-keys 'ygg-leader-agent-map
  ;; works on the visual selection: region is what the answer replaces
  "e" #'aob-deliver-to :label "answer goes…"
  "u" #'ygg-preset-edit :label "edit a preset"
  "U" #'ygg-preset-new :label "new preset"
  "s" #'ygg-agent-skill-install :label "install skills"
  "S" #'ygg-agent-skill-uninstall :label "uninstall a skill"
  "m" #'ygg-agent-maps-generate :label "repo + feature map")

(yggdrasil-define-keys 'ygg-leader-acp-map
  ;; c is the way in: agent, project, model, then the first turn
  "c" #'aob-acp-spawn-with :label "spawn: agent, project, model"
  "W" #'aob-ask-to :label "ask, answer goes…"
  "x" #'aob-context-add :label "context: add region"
  "X" #'aob-context-list :label "context: list"
  "s" #'aob-schedule :label "schedule a prompt"
  "S" #'aob-schedule-list :label "schedules"
  "p" #'aob-shells :label "running commands")

(unless noninteractive (aob-schedule-start))

(yggdrasil-define-keys 'ygg-leader-agent-map
  "c" ygg-leader-acp-map :label "sessions")

(defvar ygg-leader-quit-map)

(with-eval-after-load 'yggdrasil-leader
  (yggdrasil-define-keys 'ygg-leader-quit-map
    "a" #'save-buffers-kill-emacs :label "quit all: save and exit"))

;; SPC p is the zones prefix from layer-sessions and it is full of verbs
;; you use daily; showing the agent rows is monthly, so it answers to the
;; colon line and to M-x rather than taking a letter there.

;; every session this Emacs opens is told about the sidecar; the sidecar
;; itself is not started until the first spawn asks for it
(aob-mcp-host-mode 1)

;;; Skills after a slash, presets after an at — the words compose knew

(require 'ygg-preset nil t)
(declare-function ygg-preset-list "ygg-preset" (&optional root))
(declare-function ygg-preset-name "ygg-preset" (d))
(declare-function ygg-preset-body "ygg-preset" (d))
(declare-function ygg-preset-field "ygg-preset" (d key))
(declare-function ygg-preset-tools "ygg-preset" (d))
(declare-function ygg-preset-thinking "ygg-preset" (d))
(declare-function ygg-preset-subagent-refs "ygg-preset" (presets &optional known))
(autoload 'ygg-preset-edit "ygg-preset" nil t)
(autoload 'ygg-preset-new "ygg-preset" nil t)
(declare-function ygg-preset-skill-files "ygg-preset" (root))
(declare-function ygg-preset--parse "ygg-preset" (file))
(defvar aob-capf-command-functions)
(defvar aob-capf-mention-functions)
(defvar aob-compose-before-send-functions)

(defvar ygg-aob--preset-cache (make-hash-table :test #'equal)
  "Root to (TIME SKILLS . PRESETS): three hundred skill files are not read
on every keystroke of a popup.")

(defun ygg-aob--skills-of (root)
  "ROOT's skills as commands, the name and the line that says what each is for."
  (seq-uniq
   (mapcar (lambda (file)
             (let ((fields (car (ygg-preset--parse file))))
               (list :name (or (plist-get fields :name)
                               (file-name-nondirectory
                                (directory-file-name (file-name-directory file))))
                     :description (or (plist-get fields :description) ""))))
           (ygg-preset-skill-files root))
   (lambda (a b) (equal (plist-get a :name) (plist-get b :name)))))

(defun ygg-aob--presets-of (dir)
  "(SKILLS . PRESETS) for the project DIR is in, read at most every half minute."
  (when (featurep 'ygg-preset)
    (let* ((root (file-name-as-directory
                  (expand-file-name (or (and dir (locate-dominating-file dir ".git"))
                                        dir default-directory))))
           (hit (gethash root ygg-aob--preset-cache)))
      (if (and hit (< (- (float-time) (car hit)) 30))
          (cdr hit)
        (let ((val (cons (ignore-errors (ygg-aob--skills-of root))
                         (ignore-errors (ygg-preset-list root)))))
          (puthash root (cons (float-time) val) ygg-aob--preset-cache)
          val)))))

(defun ygg-aob--skill-commands (dir)
  "DIR's skills, for the popup a slash opens."
  (car (ygg-aob--presets-of dir)))

(defun ygg-aob--preset-mentions (dir)
  "DIR's presets, for the popup an at opens."
  (mapcar (lambda (p) (cons (ygg-preset-name p) "  preset"))
          (cdr (ygg-aob--presets-of dir))))

(defun ygg-aob--expand-presets (text)
  "TEXT with every preset it names carried after it in the preset's own words.
Named as @NAME, or as /NAME the way a skill is: the slash form is taken
out of the words, since the agent would try to run it as a command.  A
file or a skill of the same name is that, and is left to the agent."
  (let* ((dir (or (bound-and-true-p aob-compose--dir) default-directory))
         (known (ygg-aob--presets-of dir))
         (skills (mapcar (lambda (k) (plist-get k :name)) (car known)))
         (end "\\(?:[^[:alnum:]_-]\\|\\'\\)")
         blocks)
    (dolist (p (cdr known))
      (let* ((name (ygg-preset-name p))
             (at (string-match-p (concat "@" (regexp-quote name) end) text))
             (slash (and (not (member name skills))
                         (string-match-p (concat "\\(?:^\\|[[:space:]]\\)/"
                                                 (regexp-quote name) end)
                                         text))))
        (when (and (or at slash)
                   (not (file-exists-p (expand-file-name name dir))))
          (when slash
            (setq text (string-trim
                        (replace-regexp-in-string
                         (concat "\\(^\\|[[:space:]]\\)/" (regexp-quote name)
                                 "\\([^[:alnum:]_-]\\|\\'\\)")
                         "\\1\\2" text))))
          (push (format "<preset name=\"%s\">\n%s\n</preset>"
                        name (string-trim (or (ygg-preset-body p) "")))
                blocks))))
    (when blocks
      (concat text "\n\n" (string-join (nreverse blocks) "\n\n")))))

(defun ygg-aob--preset-limits (text)
  "What the presets TEXT carries ask of a new session's tools and thinking.
As session refs; the first preset settling each one decides it."
  (when (featurep 'ygg-preset)
    (let* ((known (cdr (ygg-aob--presets-of
                        (or (bound-and-true-p aob-compose--dir) default-directory))))
           (named (let ((start 0) out)
                    (while (string-match "<preset name=\"\\([^\"]+\\)\">" text start)
                      (push (match-string 1 text) out)
                      (setq start (match-end 0)))
                    (nreverse out)))
           (presets (delq nil (mapcar (lambda (name)
                                        (seq-find (lambda (p) (equal (ygg-preset-name p) name))
                                                  known))
                                      named)))
           (tools (seq-some #'ygg-preset-tools presets))
           (thinking (seq-some #'ygg-preset-thinking presets)))
      (append (and tools (list :want-tools tools))
              (and thinking (list :want-thinking thinking))
              (ygg-preset-subagent-refs presets known)))))

(defun ygg-aob--preset-commands (dir)
  "DIR's presets for the popup a slash opens, marked as presets."
  (mapcar (lambda (p)
            (list :name (ygg-preset-name p)
                  :description
                  (concat "preset · "
                          (or (ygg-preset-field p :description)
                              (car (split-string
                                    (string-trim
                                     (replace-regexp-in-string
                                      "^#+[ \t]*" "" (or (ygg-preset-body p) "")))
                                    "\n" t))
                              ""))))
          (cdr (ygg-aob--presets-of dir))))

(defcustom ygg-aob-diff-max-chars 60000
  "How much of a checkout's diff an @diff carries before it is cut."
  :type 'natnum :group 'aob)

(defvar ygg-aob--dirty-cache (make-hash-table :test #'equal)
  "Root to (TIME . DIRTY): git is asked at most every few seconds, not per key.")

(defun ygg-aob--repo (dir)
  (when-let* ((top (and dir (locate-dominating-file dir ".git"))))
    (file-name-as-directory (expand-file-name top))))

(defun ygg-aob--dirty-p (root)
  "Whether ROOT's checkout has changes against HEAD, as git said lately."
  (let ((hit (gethash root ygg-aob--dirty-cache)))
    (if (and hit (< (- (float-time) (car hit)) 5))
        (cdr hit)
      (let ((dirty (not (zerop (let ((default-directory root))
                                 (call-process "git" nil nil nil "diff" "--quiet" "HEAD"))))))
        (puthash root (cons (float-time) dirty) ygg-aob--dirty-cache)
        dirty))))

(defun ygg-aob--diff-mention (dir)
  "diff, for the popup an at opens, when the checkout has changed anything."
  (when-let* ((root (ygg-aob--repo dir))
              ((ygg-aob--dirty-p root)))
    (list (cons "diff" "  what the checkout changed"))))

(defun ygg-aob--diff-dir ()
  "The checkout an @diff reads: the target session's worktree, else the draft's."
  (if-let* ((s (ygg-aob--draft-target)))
      (or (aob-session-dir s) (aob-session-project s))
    (or (bound-and-true-p aob-compose--dir) default-directory)))

(defun ygg-aob--worktree-line (root)
  "One line naming ROOT's worktree folder and branch."
  (let ((branch (with-temp-buffer
                  (let ((default-directory root))
                    (call-process "git" nil t nil "symbolic-ref" "-q" "--short" "HEAD"))
                  (string-trim (buffer-string)))))
    (format "worktree %s · branch %s"
            (file-name-nondirectory (directory-file-name root))
            (if (string-empty-p branch) "detached HEAD" branch))))

(defun ygg-aob--expand-diff (text)
  "TEXT with the checkout's diff carried after it when it says @diff."
  (when-let* (((string-match-p "@diff\\(?:[^[:alnum:]_-]\\|\\'\\)" text))
              (root (ygg-aob--repo (ygg-aob--diff-dir)))
              (diff (with-temp-buffer
                      (let ((default-directory root))
                        (call-process "git" nil t nil "diff" "HEAD"))
                      (buffer-string)))
              ((not (string-empty-p (string-trim diff)))))
    (concat text "\n\n<diff>\n" (ygg-aob--worktree-line root) "\n"
            (if (> (length diff) ygg-aob-diff-max-chars)
                (concat (substring diff 0 ygg-aob-diff-max-chars)
                        (format "\n… %d more characters left out"
                                (- (length diff) ygg-aob-diff-max-chars)))
              diff)
            "</diff>")))

(with-eval-after-load 'aob
  (add-hook 'aob-capf-command-functions #'ygg-aob--skill-commands)
  (add-hook 'aob-capf-command-functions #'ygg-aob--preset-commands t)
  (add-hook 'aob-capf-mention-functions #'ygg-aob--preset-mentions)
  (add-hook 'aob-capf-mention-functions #'ygg-aob--diff-mention)
  (add-hook 'aob-compose-before-send-functions #'ygg-aob--expand-presets)
  (add-hook 'aob-compose-before-send-functions #'ygg-aob--expand-diff))

;;; A session's todo list: its plan carried in, your edits told back

(advice-add 'aob-acp--plan :after #'ygg-todo-mirror-plan)

(defun ygg-aob--todo-redraw (file &rest _)
  "Redraw the sessions whose list is FILE, so their counts follow it."
  (let ((file (expand-file-name file)))
    (dolist (s (aob-sessions))
      (when (equal (aob-session-ref s :todo-file) file)
        (aob--dirty s)))))

(add-hook 'ygg-todo-changed-functions #'ygg-aob--todo-redraw)

(defun ygg-aob--todo-note-compose (text)
  "TEXT followed by the user changes to the target session's list, if any."
  (when-let* ((id (and (stringp aob-compose--target) aob-compose--target))
              (note (ygg-todo-session-note id)))
    (concat text "\n\n" note)))

(add-hook 'aob-compose-before-send-functions #'ygg-aob--todo-note-compose t)

(declare-function aob-trace--held "aob-trace" (s))

(defun ygg-aob--comments-compose (text)
  "TEXT with the comments held for the target session in front of it,
and their images to carry.  They were held until the next message; a
message from compose is one."
  (when-let* ((id (and (stringp aob-compose--target) aob-compose--target))
              (s (aob-session-get id))
              ((fboundp 'aob-trace--held))
              (held (aob-trace--held s)))
    (aob-session-put s :comments nil)
    (cons (concat (car held) "\n\n" text) (cdr held))))

(add-hook 'aob-compose-before-send-functions #'ygg-aob--comments-compose)
(require 'aob-diag-push)
(require 'aob-decisions-qf)
(require 'aob-bang)

(defun ygg-aob--todo-note-say (args)
  "ARGS of a message sent from a trace, the todo note after its text."
  (pcase-let ((`(,s ,text . ,files) args))
    (if-let* ((note (ygg-todo-session-note s)))
        (cons s (cons (concat text "\n\n" note) files))
      args)))

(advice-add 'aob-trace--say :filter-args #'ygg-aob--todo-note-say)

(provide 'layer-aob)
;;; layer-aob.el ends here
