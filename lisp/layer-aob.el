;;; layer-aob.el --- agent-objects glue: ACP agents in spaces -*- lexical-binding: t; -*-

;;; Code:

(require 'yggdrasil-leader)
(require 'cl-lib)
(require 'ygg-agent-conf)
(require 'ygg-projects)
(require 'aob-context)
(require 'aob-deliver)
(require 'ygg-agent-skills)
(require 'aob-subagent)
(require 'aob-transcript)
(require 'aob-mcp-host)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'aob-workflow)
(require 'ygg-ui)

(declare-function ygg-notify "layer-ui" (msg &optional level))
(declare-function ygg-mise-prefix "layer-terminal")
(declare-function ygg-jump-back "yggdrasil-motions")
(declare-function ygg-jump-forward "yggdrasil-motions")

;; jumplist nav works in agent buffers too; Tab keeps expanding via the
;; distinct <tab> event while C-i (the TAB character) jumps forward
(define-key aob-object-map (kbd "C-o") #'ygg-jump-back)
(define-key aob-trace-mode-map (kbd "<tab>") #'aob-trace-tab)
(define-key aob-trace-mode-map (kbd "C-i") #'ygg-jump-forward)

;; ...and something for them to jump between: opening an agent's view is a
;; jump, but nothing was recording where you left, so C-o from a trace had
;; no origin to return to and C-i never led back into one.
(declare-function ygg--jump-push "yggdrasil-motions" ())
(defun ygg-aob--push-jump (&rest _)
  (when (fboundp 'ygg--jump-push) (ygg--jump-push)))

(dolist (cmd '(aob-trace aob-plan aob-subagents aob-subagents-open aob-focus))
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
(define-key aob-object-map (kbd "Z Z") #'aob-trace-send)
(define-key aob-object-map (kbd "C-d") #'ygg-scroll-half-down)
(define-key aob-object-map (kbd "C-u") #'ygg-scroll-half-up)
(define-key aob-object-map "/" #'ygg-search-forward)
(define-key aob-object-map "n" #'ygg-search-next)
(define-key aob-object-map "N" #'ygg-search-prev)
;; special-mode-map (a parent) binds h to describe-mode; keep it vim left-motion
(define-key aob-object-map "h" #'ygg-h)

;; the full modal layer, not cherry-picked keys: yggdrasil normal state
;; runs in agent buffers, with the object verbs emulation-mapped above it
;; (this alist is consulted before ygg's, so p/i/c/t/x/y stay verbs and
;; q/SPC keep their special-mode meanings; everything else — w b f } %
;; visual state, marks, jumps — is yggdrasil's)
(defvar-local ygg-aob--trace-modal nil)
(defvar-local ygg-aob--plan-modal nil)
(defvar-local ygg-aob--subs-modal nil)
(defvar ygg-aob--emulation-alist
  (list (cons 'ygg-aob--trace-modal aob-trace-mode-map)
        (cons 'ygg-aob--plan-modal aob-plan-mode-map)
        (cons 'ygg-aob--subs-modal aob-subagents-mode-map)))
(add-to-list 'emulation-mode-map-alists 'ygg-aob--emulation-alist)

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

(dolist (map (list aob-trace-mode-map aob-plan-mode-map aob-subagents-mode-map))
  (set-keymap-parent map (make-composed-keymap aob-object-map ygg-aob--special-keep)))

;; read-only agent buffers: region in visual only.  normal collapses the mark
;; to point (verb bounds stay the cell) so no stale Helix span highlights.
(defun ygg-aob--visual-only-selection ()
  (when (and (bound-and-true-p ygg--normal-p) mark-active)
    (set-mark (point))
    (deactivate-mark)))

(defun ygg-aob--modalize ()
  (yggdrasil-local-mode 1)
  (add-hook 'post-command-hook #'ygg-aob--visual-only-selection 90 t))

(defun ygg-aob--modalize-trace ()
  (setq ygg-aob--trace-modal t)
  (ygg-aob--modalize))

(defun ygg-aob--modalize-plan ()
  (setq ygg-aob--plan-modal t)
  (ygg-aob--modalize))

(defun ygg-aob--modalize-subs ()
  (setq ygg-aob--subs-modal t)
  (ygg-aob--modalize))

(add-hook 'aob-trace-mode-hook #'ygg-aob--modalize-trace)
(add-hook 'aob-plan-mode-hook #'ygg-aob--modalize-plan)
(add-hook 'aob-subagents-mode-hook #'ygg-aob--modalize-subs)

;; agent mode/model under the localleader — moved off m/M so those keys stay
;; vim set-mark / middle-of-screen in trace and plan buffers
(require 'yggdrasil-localleader)
(require 'ygg-project-scan)
(declare-function aob-acp-set-mode "aob-acp")
(declare-function aob-acp-model "aob-acp")
(declare-function aob-acp-goal "aob-acp")
(declare-function aob-acp-cycle-mode "aob-acp")
(declare-function aob-acp-config "aob-acp")
(declare-function aob-deliver-to "aob-deliver")
;; how an agent answers is a property of the one in front of you, so it
;; is set from its own buffer: the localleader already knows which
;; session that is, where the global leader has to ask
(dolist (mode '(aob-trace-mode aob-plan-mode aob-subagents-mode))
  (yggdrasil-localleader-def mode "m" #'aob-acp-set-mode "mode")
  (yggdrasil-localleader-def mode "M" #'aob-acp-cycle-mode "next mode")
  (yggdrasil-localleader-def mode "l" #'aob-acp-model "model")
  (yggdrasil-localleader-def mode "E" #'aob-acp-config "effort / options")
  (yggdrasil-localleader-def mode "g" #'aob-acp-goal "goal")
  (yggdrasil-localleader-def mode "w" #'aob-deliver-to "answer goes…")
  (yggdrasil-localleader-def mode "t" #'aob-subagents "subagents"))

(defvar ygg-quickscope-inhibit)

;; the trace is prose, not code: f and t still jump, but their preview
;; is a one-cell mark designed for a monospace grid
(defun ygg-aob--no-quickscope ()
  (setq-local ygg-quickscope-inhibit t))

(dolist (hook '(aob-trace-mode-hook aob-plan-mode-hook aob-subagents-mode-hook))
  (add-hook hook #'ygg-aob--no-quickscope))

(autoload 'ygg-compose-transient "ygg-task-compose" nil t)
(yggdrasil-localleader-def 'aob-compose-mode "m" #'ygg-compose-transient "modes")
(yggdrasil-localleader-def 'aob-compose-mode "q" #'aob-compose-hide "hide the box")
(setq aob-compose-panel-hint "\\ m modes")

;; T on an agent → its file activity as a quickfix.  Built from ACP tool
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
      (when (plist-get ev :children)
        (puthash (plist-get ev :tool-id) (plist-get ev :title) names)))
    (dolist (ev (reverse (aob-session-events s)))
      (dolist (loc (plist-get ev :locations))
        (when-let* ((path (plist-get loc :path)))
          (let* ((sub (and (plist-get ev :parent)
                           (gethash (plist-get ev :parent) names)))
                 (label (cond
                         (sub (concat "↳ " (aob--first-line sub 60)))
                         ((plist-get ev :title)
                          (aob--first-line (plist-get ev :title) 60))
                         (t (or (plist-get ev :kind) "tool")))))
            (push (format "%s:%d: %s" path (or (plist-get loc :line) 1) label)
                  lines))))
      ;; a finished subagent's children were collapsed off the ring; their
      ;; locations ride the surviving Task, still under its name
      (when-let* ((title (and (plist-get ev :children) (plist-get ev :title)))
                  (label (concat "↳ " (aob--first-line title 60))))
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

(defvar aob-buffer-session-id)

(define-key aob-object-map "T" (cons "activity → quickfix" #'ygg-aob-activity))

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
        (when-let* ((env (ygg-agent--config-env agent agent project)))
          (list env))))

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
        (let ((aob-acp-start-dir aob-compose--dir))
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

(defun ygg-aob--compose-no-leader ()
  "Keep the leader off the space bar in this compose buffer."
  (setq ygg--special-lift-alist
        (list (cons 'ygg--normal-p ygg-aob--compose-normal-map))))

(add-hook 'aob-compose-mode-hook #'ygg-aob--compose-no-leader)

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
  "Make corfu's popup a child of the top frame when it stands in the box.
ORIG is corfu's own maker; FRAME, X, Y, WIDTH and HEIGHT are its
arguments, X and Y relative to the frame the window is on."
  (let ((child (window-frame)))
    (if (or (not (frame-parent child))
            (not (derived-mode-p 'aob-compose-mode)))
        (funcall orig frame x y width height)
      (let* ((top (ygg-ui-main-frame child))
             (lh (default-line-height))
             (yb (+ (cadr (window-inside-pixel-edges))
                    (or (cdr (posn-x-y (posn-at-point))) 0) lh))
             (at (ygg-aob--corfu-lift x y height lh yb child
                                      (frame-pixel-height top))))
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
The tree shows what is still working; `aob-subagents' under the trace
keeps every delegation, finished ones included."
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

(defun ygg-aob-session-space (s)
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

(add-hook 'aob-session-created-hook #'ygg-aob--remember-space)

;;; A space per agent — an agent gets a child space under the project it
;;; was sent into, so its trace, plan and terminal live together and the
;;; project's own space keeps the work the owner was doing.

(defcustom ygg-aob-space-per-agent t
  "Give every agent a child space of its own when it starts."
  :type 'boolean :group 'aob)

(declare-function ygg-space-child "yggdrasil-spacetree")
(declare-function ygg-space-rename "yggdrasil-spacetree" (name))

(defun ygg-aob--space-for-agent (s)
  "Nest a child space under the current one and name it after S."
  (when (and ygg-aob-space-per-agent
             (fboundp 'ygg-space-child)
             (fboundp 'ygg-space-rename))
    (condition-case err
        (let ((default-directory (or (aob-session-dir s)
                                     (aob-session-project s)
                                     default-directory)))
          (ygg-space-child)
          (ygg-space-rename (or (aob-session-name s) "agent"))
          (aob-session-put s :space (ygg-space--current-id)))
      (error (message "aob: no space for %s (%s)"
                      (aob-session-name s) (error-message-string err))))))

(add-hook 'aob-session-created-hook #'ygg-aob--space-for-agent 90)

;; a trace stands beside the work, not over it.  ygg-ui-show hands a
;; reader the main window, which is right for something you go and read
;; and wrong for something that streams while you keep working: the
;; buffer you were in would be the thing that disappeared.
(defun ygg-agent-terminal-env (&optional project)
  "The config-home variables a shell in PROJECT should carry.
One entry per kind of CLI that keeps a home — CLAUDE_CONFIG_DIR,
CODEX_HOME — pointing at the same home the agents spawned from here
are given, so a CLI run by hand and one run by aob are the same
install, logged in once."
  (let ((project (or project default-directory)))
    (delq nil (mapcar (lambda (kind) (ygg-agent--config-env kind kind project))
                      (mapcar #'car ygg-agent--config-homes)))))

(defun ygg-agent--terminal-env ()
  "Point a terminal's CLI agents at its own project's config home.
Every terminal, however it was opened: a claude run by hand and one
spawned by aob are then the same install, logged in once."
  (dolist (entry (ygg-agent-terminal-env default-directory))
    (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" entry)
      (setenv (match-string 1 entry) (match-string 2 entry)))))

(add-hook 'ghostel-pre-spawn-hook #'ygg-agent--terminal-env)

(defcustom ygg-aob-trace-action
  '((display-buffer-reuse-window display-buffer-in-direction)
    (direction . right)
    (window-width . 0.5)
    (inhibit-same-window . t))
  "How a session's trace is put on screen."
  :type 'sexp :group 'aob)

(setq aob-acp-show-trace nil)

(defun ygg-aob--show-trace (s)
  "Show S's trace in a split of its own, leaving point where it is."
  (unless noninteractive
    (when-let* ((buf (ignore-errors (aob-trace-buffer s))))
      (ignore-errors (display-buffer buf ygg-aob-trace-action)))))

(add-hook 'aob-session-created-hook #'ygg-aob--show-trace 95)

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


;; trace buffers join their session's space bucket so SPC b b lists
;; them; layer-sessions' kill/close machinery then manages them free
(defvar aob-trace--session-id)
(defvar ygg--space-buffers)

(defun ygg-aob--adopt-trace (buf)
  (with-current-buffer buf
    (when-let* ((s (aob-session-get aob-trace--session-id)))
      ;; the buffer stands where its agent works, so magit, the shell and
      ;; every project command opened from it answer for the agent's tree
      ;; and not for whatever folder the window happened to inherit
      (when-let* ((dir (or (aob-session-dir s) (aob-session-project s)))
                  ((file-directory-p dir)))
        (setq default-directory (file-name-as-directory dir)))
      (when (boundp 'ygg--space-buffers)
        (when-let* ((id (aob-session-ref s :space)))
          (cl-pushnew buf (gethash id ygg--space-buffers))))))
  buf)

(advice-add 'aob-trace-buffer :filter-return #'ygg-aob--adopt-trace)

(defvar aob-plan--session-id)

(defun ygg-aob--adopt-plan (buf)
  (when (boundp 'ygg--space-buffers)
    (with-current-buffer buf
      (when-let* ((s (aob-session-get aob-plan--session-id))
                  (id (aob-session-ref s :space)))
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

(defun ygg-aob--goto (s)
  "Show agent S's trace — space-agnostic: no workspace switch, no sidebar.
An already-visible trace is refocused; otherwise it opens in place."
  (let ((buf (aob-trace-buffer s)))
    (if-let* ((win (get-buffer-window buf)))
        (select-window win)
      (ygg-ui-show buf))))

;;; Agents in spaces — each space's sidebar row grows one line per live
;;; agent (state glyph, name, ctx); RET on a line jumps to it.  Rows are
;;; ambient (on by default, only render when agents exist); SPC p t
;;; toggles the panel itself, SPC p a the rows.

(defvar ygg-aob--tree-agents-on t)
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
    ('blocked (propertize "✋" 'face 'error))
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

(defun ygg-aob--tree-details (space-id)
  (when ygg-aob--tree-agents-on
    (mapcar
     (lambda (s)
       (propertize
        (truncate-string-to-width
         (format "   %s %s%s %s"
                 (ygg-aob--tree-glyph s)
                 (ygg-aob--tree-name s)
                 (let ((q (length (aob-session-ref s :queued))))
                   (if (> q 0)
                       (propertize (format " »%d" q) 'face 'shadow)
                     ""))
                 (propertize
                  (or (car (split-string (or (aob-session-ctx s) "") "/")) "")
                  'face 'shadow))
         (or (bound-and-true-p ygg-space-tree-width) 18))
        'aob-session (aob-session-id s)
        'keymap ygg-aob--tree-line-map
        'mouse-face 'highlight))
     (seq-sort-by #'ygg-aob--score #'>
                  (seq-filter
                   (lambda (s) (eql (ygg-aob-session-space s) space-id))
                   (aob-live-sessions))))))

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
                   (seq-sort-by #'ygg-aob--score #'> (aob-live-sessions)))
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
            (buffer-substring-no-properties (region-beginning) (region-end)))))

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
                                  nil t nil nil aob-acp-default-agent)))
    (ygg-aob--enter-home home)
    ;; the spawn happens on send, from the folder the draft names —
    ;; `aob-compose' takes it from here, and a resumed row from its session
    (aob-compose (ygg-aob--materialize (cdr (assoc choice map))) seed nil dir)))

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
  (let ((live (aob-live-sessions))
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

(defun ygg-aob-pick ()
  "Go to an agent IN THE CURRENT SPACE, flash-style: labeled hints in the
echo area, one keypress jumps.  Only this space's agents are offered;
worst attention sits on `a'; a sole agent needs no key."
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
                                  ('blocked (propertize " ✋" 'face 'error))
                                  ('working (propertize " ●" 'face 'warning))
                                  (_ ""))))
                      pairs "   "))
               (ch (read-char (concat hint "  » ")))
               (hit (alist-get ch pairs)))
          (if hit
              (ygg-aob--goto hit)
            (user-error "no agent on %c" ch)))))))

(declare-function ygg-task-dispatch-answer-next "ygg-task-dispatch" ())

(defun ygg-aob-resolve-next ()
  "Answer the first pending Decision, then the first parked question.
Permissions come first because a worker holding one is stopped where it
stands; with none left the same key goes on to the daemon's parked
questions, so one queue empties under one key."
  (interactive)
  (if-let* ((s (seq-find #'aob-session-decisions (aob-sessions))))
      (aob-resolve s)
    (unless (fboundp 'ygg-task-dispatch-answer-next)
      (user-error "no pending decisions"))
    (ygg-task-dispatch-answer-next)))

;;; Resolution queue → *quickfix*, live while it is open.  Decision
;;; lines carry their own RET (resolve) and vanish as they are answered;
;;; the user's grep results in the same buffer are never touched.

(defvar ygg-aob--qf-line-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'ygg-aob-qf-resolve)
    m))

(defun ygg-aob-qf-resolve ()
  "Resolve the decision on this quickfix line."
  (interactive)
  (when-let* ((id (get-text-property (point) 'aob-decision))
              (s (aob-session-get id)))
    (aob-resolve s)))

(defun ygg-aob--qf-refresh (&rest _)
  (when-let* (((fboundp 'ygg-qf-buffer))
              (buf (ygg-qf-buffer))
              ((get-buffer-window buf t)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (if (get-text-property (point) 'aob-decision)
                (delete-region (point) (min (point-max)
                                            (1+ (line-end-position))))
              (forward-line 1)))
          (goto-char (point-min))
          (unless (bobp) (goto-char (point-min)))
          (when (> (point-max) (point-min)) (forward-line 1))
          (dolist (s (aob-sessions))
            (dolist (d (aob-session-decisions s))
              (insert (propertize
                       (format "✋ %s — %s%s"
                               (aob-session-name s)
                               (plist-get d :title)
                               (if-let* ((det (plist-get d :detail)))
                                   (format "  [%s]" det)
                                 ""))
                       'aob-decision (aob-session-id s)
                       'face 'error
                       'keymap ygg-aob--qf-line-map)
                      "\n"))))))))

(add-hook 'aob-state-change-hook #'ygg-aob--qf-refresh)
(add-hook 'aob-session-removed-hook #'ygg-aob--qf-refresh)

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
    (pcase new
      ('blocked (ygg-aob--say (format "%s needs input" name) 'warn))
      ((and 'idle (guard (eq old 'working)))
       (ygg-aob--say (format "%s done" name))))))

(add-hook 'aob-state-change-hook #'ygg-aob--notify)

(aob-modeline-mode 1)

;; the terminal layer's SPC a keys stay exactly as layer-agent defines
;; them; the whole ACP layer is five keys under SPC a c
;;; Embark on an agent — what you do to one agent, where the agent is.
;;; Point already answers `aob-target', so every verb here works on
;;; whatever row, trace or subagent line the cursor is on.  Workflows are
;;; not here: they run a fleet, not the agent under your cursor.

(declare-function aob-session-at-point "aob")
(declare-function aob-session-id "aob")
(declare-function ygg-task-adopt "ygg-task-adopt" (session &optional brief))
(defvar ygg-aob-agent-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'aob-acp-fork)
    (define-key map "e" #'aob-acp-config)
    (define-key map "!" #'aob-acp-restart)
    (define-key map "p" #'aob-compose-recall)
    (define-key map "g" #'aob-acp-goal)
    (define-key map "M" #'aob-acp-model)
    (define-key map "k" #'aob-kill-session)
    (define-key map "t" #'ygg-task-adopt)
    (define-key map "S" #'ygg-share-session)
    (define-key map "Q" #'ygg-pending-show)
    map)
  "What you can do to the agent under point.")

(autoload 'ygg-share-session "ygg-share" nil t)
(autoload 'ygg-share-open "ygg-share" nil t)
(autoload 'ygg-pending-show "ygg-pending" nil t)

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

(declare-function ygg-daemon-oneshot "ygg-daemon" (&optional default-root))
(declare-function ygg-daemon-inspect "ygg-daemon" (task))
(declare-function ygg-qa-compose "ygg-qa" (task &optional note))
(autoload 'ygg-qa-compose "ygg-qa" nil t)
(autoload 'ygg-daemon-inspect "ygg-daemon" nil t)
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
  "t" #'ygg-task-adopt :label "task from this chat"
  "c" #'ygg-daemon-oneshot :label "compose: a draft, its mode on \\ m"
  "q" #'aob-kill-session :label "kill")

(declare-function ygg-transient-acp "ygg-transient")

(defvar ygg-leader-agent-map (make-sparse-keymap) "The a prefix: AI agents.")
(yggdrasil-leader-def "a" ygg-leader-agent-map "agents")
(yggdrasil-define-keys 'ygg-leader-agent-map
  "d" #'ygg-projects-sidebar :label "projects")

(yggdrasil-define-keys 'ygg-leader-agent-map
  ;; works on the visual selection: region is what the answer replaces
  "e" #'aob-deliver-to :label "answer goes…"
  "s" #'ygg-agent-skill-install :label "install skills"
  "S" #'ygg-agent-skill-uninstall :label "uninstall a skill")

(yggdrasil-define-keys 'ygg-leader-acp-map
  ;; c is the way in: agent, project, model, then the first turn
  "c" #'aob-acp-spawn-with :label "spawn: agent, project, model"
  "W" #'aob-ask-to :label "ask, answer goes…"
  "x" #'aob-context-add :label "context: add region"
  "X" #'aob-context-list :label "context: list")

(yggdrasil-define-keys 'ygg-leader-agent-map
  "c" ygg-leader-acp-map :label "sessions")

(declare-function ygg-qa-compose "ygg-qa" (task &optional note))
(declare-function ygg-qa-score "ygg-qa" (task callback))
(declare-function ygg-qa-show "ygg-qa" (task &optional text))
(defvar ygg-leader-quit-map)

(defun ygg-qa-score-here ()
  "Score the mutants the last QA proposed for the task at hand."
  (interactive)
  (require 'ygg-qa)
  (ygg-qa-score (ygg-task-here-or-read)
                (lambda (score)
                  (message "qa: %s" (if (consp score)
                                        (format "%s killed of %s" (car score) (cdr score))
                                      score)))))

(defun ygg-qa-show-here ()
  "Open the last QA report of the task at hand."
  (interactive)
  (require 'ygg-qa)
  (ygg-ui-show (ygg-qa-show (ygg-task-here-or-read))))

(with-eval-after-load 'yggdrasil-leader
  (yggdrasil-define-keys 'ygg-leader-quit-map
    "a" #'save-buffers-kill-emacs :label "quit all: save and exit"))

;; SPC p is the zones prefix from layer-sessions and it is full of verbs
;; you use daily; showing the agent rows is monthly, so it answers to the
;; colon line and to M-x rather than taking a letter there.

;; every session this Emacs opens is told about the sidecar; the sidecar
;; itself is not started until the first spawn asks for it
(aob-mcp-host-mode 1)

(provide 'layer-aob)
;;; layer-aob.el ends here
