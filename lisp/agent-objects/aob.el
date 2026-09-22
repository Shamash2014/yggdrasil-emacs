;;; aob.el --- Agent objects: registry, events, verbs -*- lexical-binding: t; -*-

;;; Commentary:
;; Backend-agnostic core: a registry of agent sessions with typed event
;; streams, a coalescing render loop, and the verb layer.  Backends
;; (ACP, terminal) feed events in; views (status board, trace) render
;; the visible slice out.  Events never render directly: ingestion is
;; O(1), drawing happens on one debounced timer.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ygg-ui)

(defgroup aob nil "Agent objects." :group 'tools :prefix "aob-")

(cl-defstruct (aob-session (:constructor aob-session--create) (:copier nil))
  id backend name project dir state summary
  events nevents decisions buffer conn started extra)

(defvar aob--sessions (make-hash-table :test #'equal))
(defvar aob--order nil
  "Session ids, newest first.")

(defvar aob-state-change-hook nil
  "Run with (SESSION OLD NEW) when a session changes state.")

(defvar aob-session-created-hook nil
  "Run with the session right after it enters the registry.")

(defvar aob-queue-change-hook nil
  "Run with the session when its pending-prompt queue grows or flushes.")

(defvar aob-session-removed-hook nil
  "Run with the session after it leaves the registry.")

(defun aob-sessions ()
  (delq nil (mapcar (lambda (id) (gethash id aob--sessions)) aob--order)))

(defun aob-live-sessions ()
  (seq-remove (lambda (s) (memq (aob-session-state s) '(dead failed)))
              (aob-sessions)))

(defun aob-session-get (id)
  (gethash id aob--sessions))

(defun aob-create-session (&rest args)
  (let ((s (apply #'aob-session--create
                  (append args (list :started (current-time) :nevents 0)))))
    (puthash (aob-session-id s) s aob--sessions)
    (setq aob--order (cons (aob-session-id s)
                           (delete (aob-session-id s) aob--order)))
    (run-hook-with-args 'aob-session-created-hook s)
    (aob--dirty)
    s))

(defun aob-remove-session (s)
  (remhash (aob-session-id s) aob--sessions)
  (setq aob--order (delete (aob-session-id s) aob--order))
  (run-hook-with-args 'aob-session-removed-hook s)
  ;; gc: views of the dead object die with it, and the struct sheds its
  ;; bulk so closures still holding it keep only a husk
  (dolist (b (buffer-list))
    (when (and (equal (buffer-local-value 'aob-buffer-session-id b)
                      (aob-session-id s))
               (buffer-live-p b))
      (kill-buffer b)))
  (setf (aob-session-events s) nil)
  (setf (aob-session-decisions s) nil)
  (aob-session-put s :queued nil)
  (aob--dirty))

(defun aob-session-put (s key val)
  (setf (aob-session-extra s) (plist-put (aob-session-extra s) key val)))

(defun aob-session-ref (s key)
  (plist-get (aob-session-extra s) key))

(defun aob-session-queued-p (s)
  "Non-nil when S has prompts waiting to go out when this turn settles.
Asked by whoever would send a turn of its own on the idle: the queue
flushes as soon as the turn ends, and two prompts cannot be in flight at
once, so the work that would follow waits for the queued turn instead."
  (and (aob-session-ref s :queued) t))

(defun aob-set-state (s new)
  (let ((old (aob-session-state s)))
    (unless (eq old new)
      (setf (aob-session-state s) new)
      (run-hook-with-args 'aob-state-change-hook s old new)
      (aob--dirty s))))

;;; Events — append-only per session, capped, mutable in place so
;;; streaming updates (chunks, tool status) stay O(1)

(defcustom aob-event-cap 600
  "Events kept per session; older half is dropped past this."
  :type 'natnum :group 'aob)

(defvar aob--seq 0)

(defvar aob-event-functions nil
  "Abnormal hook run with (SESSION EVENT) after an event is stored on the session.")

(defun aob-event (s type &rest props)
  "Append an event of TYPE with PROPS to S; return the mutable plist."
  (let ((ev (append (list :type type :seq (cl-incf aob--seq) :ts (float-time))
                    props)))
    (push ev (aob-session-events s))
    (when (> (cl-incf (aob-session-nevents s)) aob-event-cap)
      (let ((keep (/ aob-event-cap 2)))
        (setf (aob-session-events s)
              (aob--evict (aob-session-events s) keep)
              (aob-session-nevents s) keep)))
    (aob-refresh-summary s ev)
    (dolist (fn aob-event-functions)
      (condition-case err (funcall fn s ev)
        (error (message "aob-event-functions: %S" err))))
    ev))

(defun aob--evict (evs keep)
  "Cut newest-first EVS down to KEEP entries.
Subagent detail is expendable before the top-level narrative: finished
children go first, then any children, then non-prompts.  Prompts go
last of all — they are the session's recordable shape."
  (let ((doomed (- (length evs) keep))
        (oldest (reverse evs)))
    (dolist (pref (list (lambda (ev)
                          (and (plist-get ev :parent)
                               (not (member (plist-get ev :status)
                                            '("pending" "in_progress")))))
                        (lambda (ev) (plist-get ev :parent))
                        (lambda (ev) (not (eq (plist-get ev :type) 'prompt)))
                        #'identity))
      (dolist (ev oldest)
        (when (and (> doomed 0) (not (plist-get ev :doomed)) (funcall pref ev))
          (plist-put ev :doomed t)
          (cl-decf doomed))))
    (seq-remove (lambda (ev) (plist-get ev :doomed)) evs)))

(defun aob-refresh-summary (s ev)
  "Make EV the current-activity summary of S and schedule a redraw.
Only the event pointer is stored — chunk streams land here dozens of
times a second, so the string builds lazily in `aob-session-blurb'.
A subagent's step is never the session's activity: the Task that sent it
is, so the line stays the work you asked for and not five borrowed
voices taking turns in it."
  (unless (plist-get ev :parent)
    (setf (aob-session-summary s) ev))
  (aob--dirty s))

(defun aob-session-subagents (s)
  "Every subagent S delegated to, in the order it sent them.
A subagent is the Task event other tool calls name as their parent —
what the trace already rolls up, read back as a list."
  (seq-filter (lambda (e) (and (eq (plist-get e :type) 'tool)
                               (or (plist-get e :subagent)
                                   (plist-get e :children))))
              (reverse (aob-session-events s))))

(defun aob-session-blurb (s)
  "S's current-activity line, built on demand."
  (let ((x (aob-session-summary s)))
    (cond ((stringp x) x)
          (x (aob-event-summary x)))))

(defun aob-tokens-short (n)
  (cond ((null n) nil)
        ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
        ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
        (t (number-to-string n))))

(defun aob-session-cwd (s)
  "Where S actually works: its worktree when isolated, else the project.
Marked ⌥ when that is not the project checkout itself."
  (let ((dir (or (aob-session-dir s) (aob-session-project s))))
    (when dir
      (concat (when (and (aob-session-project s)
                         (not (equal (file-name-as-directory dir)
                                     (file-name-as-directory
                                      (aob-session-project s)))))
                "⌥ ")
              (file-name-nondirectory (directory-file-name dir))))))

(defun aob-session-ctx (s)
  "Context reading: live used/size when the agent streams it, else the
last turn's total."
  (let ((used (or (aob-session-ref s :ctx-used)
                  (plist-get (aob-session-ref s :usage) :totalTokens)))
        (size (aob-session-ref s :ctx-size)))
    (when used
      (concat (aob-tokens-short used)
              (and size (concat "/" (aob-tokens-short size)))))))

(defun aob--first-line (text limit)
  (let* ((line (car (split-string (or text "") "\n" t)))
         (line (or line "")))
    (if (> (length line) limit) (concat (substring line 0 limit) "…") line)))

;; streaming text accumulates as O(1) chunk pushes in :parts; :head is a
;; bounded prefix for summaries; the full join happens lazily on demand
;; (TAB expand), never per chunk — per-chunk concat is quadratic and
;; falls over on the long outputs 1M-context sessions produce

(defcustom aob-event-live-prefix 4000
  "Characters of a streaming answer kept joined as it arrives.
Views cut a block well before this, so the prefix is all any of them
can show; the rest waits in `:parts' until the turn settles."
  :type 'natnum :group 'aob)

(defun aob-event-push-text (ev text)
  (if (and (null (plist-get ev :parts))
           (< (length (or (plist-get ev :text) "")) aob-event-live-prefix))
      (plist-put ev :text (concat (or (plist-get ev :text) "") text))
    (plist-put ev :parts (cons text (plist-get ev :parts))))
  (plist-put ev :line nil)
  (when (< (length (or (plist-get ev :head) "")) 60)
    (plist-put ev :head (aob--first-line
                         (concat (or (plist-get ev :head) "") text) 60))))

(defun aob-event-text (ev)
  (if-let* ((parts (plist-get ev :parts)))
      (let ((txt (concat (or (plist-get ev :text) "")
                         (mapconcat #'identity (nreverse parts) ""))))
        (plist-put ev :parts nil)
        (plist-put ev :text txt)
        txt)
    (or (plist-get ev :text) "")))

(defun aob-event-text-so-far (ev)
  "EV's text as far as it has been joined, without draining `:parts'.
At most `aob-event-live-prefix' characters — what a still-arriving
answer can be drawn from without re-joining it ten times a second.
`aob-event-text' is for once the turn has settled."
  (or (plist-get ev :text) ""))

(defun aob-event-head (ev &optional limit)
  (let ((head (or (plist-get ev :head)
                  (aob--first-line (plist-get ev :text) 60))))
    (if (and limit (> (length head) limit))
        (concat (substring head 0 limit) "…")
      head)))

(defun aob-event-summary (ev)
  (pcase (plist-get ev :type)
    ('tool (format "%s%s%s"
                   (if (plist-get ev :subagent) "↳ "
                     (concat (or (plist-get ev :kind) "tool") " "))
                   (or (plist-get ev :title) "")
                   (if-let* ((st (plist-get ev :stat))) (concat "  " st) "")))
    ('message (aob-event-head ev 48))
    ('thought (concat "… " (aob-event-head ev 44)))
    ('prompt (concat "» " (aob-event-head ev 44)))
    ('permission (concat "✋ " (or (plist-get ev :title) "permission")))
    ('plan (or (plist-get ev :title) "plan"))
    ('stop (format "done (%s)%s"
                   (or (plist-get ev :reason) "end")
                   (if-let* ((tk (aob-tokens-short (plist-get ev :tokens))))
                       (format " · %s ctx" tk)
                     "")))
    ('error (concat "error: " (or (plist-get ev :title) "?")))
    (_ (or (plist-get ev :title) (symbol-name (plist-get ev :type))))))

;;; Render loop — views register once; any mutation sets one timer;
;;; the tick redraws every live view.  Cost scales with visible rows,
;;; never with event rate or agent count.

(defcustom aob-render-interval 0.1
  "Seconds the render timer coalesces registry mutations over.
10Hz reads as smooth for streamed text while cutting render passes ~20%
vs 12.5Hz; raise further to trade streaming smoothness for less CPU."
  :type 'number :group 'aob)

(defvar aob--views nil
  "Alist of (BUFFER . RENDER-FN).")

(defvar aob--render-timer nil)
(defvar aob--tick 0)
(defvar-local aob--rendered-tick -1)

(defvar-local aob-buffer-session-id nil
  "Session this whole buffer is about; views set it so verbs resolve
even from unpropertized spots (point sits at the trailing newline
under tail-follow).")

(defun aob-register-view (buffer fn)
  (setf (alist-get buffer aob--views nil nil #'eq) fn))

(defun aob--dirty (&optional s)
  ;; the global tick schedules the timer; the per-session tick lets a
  ;; view skip redraws when *its* session didn't change
  (cl-incf aob--tick)
  (when s
    (aob-session-put s :tick (1+ (or (aob-session-ref s :tick) 0))))
  (unless aob--render-timer
    (setq aob--render-timer
          (run-with-timer aob-render-interval nil #'aob--render-all))))

(defun aob--view-tick ()
  "The tick the current buffer's view must be level with to be current.
A view about one session follows that session's own tick, so a burst on
a neighbour leaves it alone: rewriting a buffer whose content did not
change costs a full relayout of every line on screen.  A view about the
fleet has no session of its own and follows the global tick."
  (if-let* ((s (and aob-buffer-session-id
                    (aob-session-get aob-buffer-session-id))))
      (or (aob-session-ref s :tick) 0)
    aob--tick))

(defun aob--render-view (v)
  (with-current-buffer (car v)
    (let ((tick (aob--view-tick)))
      (unless (= aob--rendered-tick tick)
        (setq aob--rendered-tick tick)
        (let ((inhibit-read-only t))
          (ignore-errors (funcall (cdr v))))))))

(defun aob--render-all ()
  (setq aob--render-timer nil)
  (setq aob--views (seq-filter (lambda (v) (buffer-live-p (car v))) aob--views))
  ;; buried views cost nothing: only windows showing a view render now;
  ;; a buried one catches up the moment it is displayed
  (dolist (v aob--views)
    (when (get-buffer-window (car v) t)
      (aob--render-view v))))

(defun aob--render-on-display (_frame)
  (dolist (win (window-list nil 'no-minibuf))
    (when-let* ((v (assq (window-buffer win) aob--views)))
      (aob--render-view v))))

(add-hook 'window-buffer-change-functions #'aob--render-on-display)

(defun aob-rerender ()
  "Force an immediate redraw of all views."
  (interactive)
  (aob--render-all))

;;; Backends — a symbol mapped to a plist of verb implementations

(defvar aob--backends nil)

(defun aob-register-backend (name plist)
  (setf (alist-get name aob--backends) plist))

(defun aob-backend-fn (s key)
  (plist-get (alist-get (aob-session-backend s) aob--backends) key))

(defun aob--call (s key &rest args)
  (let ((fn (aob-backend-fn s key)))
    (unless fn
      (user-error "aob: %s backend does not support %s"
                  (aob-session-backend s) key))
    (apply fn s args)))

;;; Targets — the object at point, else the sole session, else a pick

(defun aob-session-at-point ()
  (when-let* ((id (or (get-text-property (point) 'aob-session)
                      aob-buffer-session-id)))
    (aob-session-get id)))

(defun aob-target ()
  (or (aob-session-at-point)
      (let ((live (aob-live-sessions)))
        (cond ((null live) (user-error "aob: no agents"))
              ((null (cdr live)) (car live))
              (t (aob-read-session "Agent: " live))))))

(defun aob--age (s)
  (let ((secs (float-time (time-subtract nil (aob-session-started s)))))
    (cond ((< secs 60) (format "%ds" (truncate secs)))
          ((< secs 3600) (format "%dm" (truncate secs 60)))
          (t (format "%dh" (truncate secs 3600))))))

(defvar aob--read-map nil)

(defun aob--annotate (cand)
  (when-let* ((s (cdr (assoc cand aob--read-map)))
              ((aob-session-p s)))
    (concat (propertize " " 'display '(space :align-to 28))
            (propertize (format "%-8s %-11s %-20s %-30s %s"
                                (or (aob-session-state s) "")
                                (or (aob-session-ctx s) "")
                                (truncate-string-to-width
                                 (or (aob-session-cwd s) "") 20)
                                (truncate-string-to-width
                                 (or (aob-session-ref s :info-title) "") 30)
                                (or (aob-session-blurb s) ""))
                        'face 'completions-annotations))))

(defun aob-read-session (prompt &optional sessions)
  (let* ((aob--read-map
          (mapcar (lambda (s) (cons (aob-session-name s) s))
                  (or sessions (aob-live-sessions))))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . aob-session)
                                 (annotation-function . aob--annotate))
                    (complete-with-action action (mapcar #'car aob--read-map)
                                          str pred))))
         (choice (completing-read prompt table nil t)))
    (cdr (assoc choice aob--read-map))))

;;; Verbs

(defun aob-prompt (s text &optional attachments)
  "Send TEXT (plus image ATTACHMENTS) as a new prompt turn to S."
  (interactive (let ((s (aob-target)))
                 (list s (read-string (format "%s » " (aob-session-name s))))))
  (aob--call s :prompt text attachments))

(defun aob-cancel (s)
  "Cancel S's current turn; press c again (cc) to fully cancel.
The first c stops the turn — queued prompts survive.  A second,
consecutive c drops the queue too, so nothing flushes back in.  Work
already produced always survives."
  (interactive (list (aob-target)))
  (let ((full (eq last-command 'aob-cancel)))
    (aob--call s :cancel full)
    (message "aob: %s %s" (aob-session-name s)
             (if full "fully cancelled — queue dropped" "cancelled (cc to drop queue)"))))

(defun aob-interject (s text)
  "Say TEXT to S now, into the turn it is running when it takes one."
  (interactive (let ((s (aob-target)))
                 (list s (read-string (format "%s ⇄ " (aob-session-name s))))))
  (if-let* ((fn (aob-backend-fn s :interject)))
      (funcall fn s text)
    (aob--call s :cancel)
    (aob--call s :prompt text)))

(defvar aob-compose--steer)

(defun aob-steer (s)
  "Steer S: compose a correction and hand it to the turn already running.
Nothing is cancelled here.  An agent that takes steering adapts without
losing the work in flight; one that does not falls back to cancelling
when the text is actually sent, which is late enough to still be your
decision to make."
  (interactive (list (aob-target)))
  (aob-compose s)
  (setq aob-compose--steer t))

(defun aob-rename-session (s name)
  "Call S NAME from now on.
The name is what every list shows; the id is what buffers, views and the
persist file key on, and it does not move — a session renamed mid-turn
keeps writing into the trace it was already writing into."
  (interactive
   (let ((s (aob-target)))
     (list s (read-string "aob: name " nil nil (aob-session-name s)))))
  (setq name (string-trim name))
  (when (string-empty-p name) (user-error "aob: a session needs a name"))
  (setf (aob-session-name s) name)
  (when (fboundp 'ygg-cockpit-rename-buffers) (ygg-cockpit-rename-buffers s))
  (aob--dirty s)
  name)

(defun aob-kill-session (s)
  "Kill S's process and drop it from the registry."
  (interactive (list (aob-target)))
  (when (y-or-n-p (format "Kill %s? " (aob-session-name s)))
    (aob--call s :kill)))

(defun aob-focus (s)
  "Show S's primary surface (terminal or trace)."
  (interactive (list (aob-target)))
  (aob--call s :focus))

(defun aob-resolve (s)
  "Answer S's pending Decision — a permission, or an agent's question."
  (interactive (list (aob-target)))
  (let ((d (car (aob-session-decisions s))))
    (unless d (user-error "aob: %s has no pending decision" (aob-session-name s)))
    (if (eq (plist-get d :kind) 'elicitation)
        (aob--resolve-question s d)
      ;; least-commitment option first, so a reflexive RET grants once,
      ;; never forever
      (let* ((rank '(("allow_once" . 0) ("allow_always" . 1)
                     ("reject_once" . 2) ("reject_always" . 3)))
             (opts (mapcar (lambda (o) (cons (plist-get o :name) (plist-get o :optionId)))
                           (seq-sort-by
                            (lambda (o) (or (cdr (assoc (plist-get o :kind) rank)) 4))
                            #'< (plist-get d :options))))
             ;; show the evidence, not just the label — you approve what you can see
             (pick (completing-read
                    (format "%s%s: "
                            (or (plist-get d :title) "decision")
                            (if-let* ((detail (plist-get d :detail)))
                                (format "  [%s]" detail)
                              ""))
                    (mapcar #'car opts) nil t)))
        (aob--call s :resolve d (cdr (assoc pick opts)))))))

(defun aob--resolve-question (s d)
  "Walk D's questions; typing beyond the options is a custom answer."
  (let (content)
    (dolist (q (plist-get d :questions))
      (let ((prompt (format "%s: " (aob--first-line
                                    (or (plist-get q :text)
                                        (plist-get d :title) "answer")
                                    72)))
            (labels (plist-get q :options)))
        (if (plist-get q :multi)
            (push (cons (plist-get q :key)
                        (completing-read-multiple prompt labels))
                  content)
          (let ((ans (completing-read prompt labels)))
            (push (if (member ans labels)
                      (cons (plist-get q :key) ans)
                    (cons (concat (plist-get q :key) "_custom") ans))
                  content)))))
    (aob--call s :resolve d (nreverse content))))

(defconst aob--reject-kinds '("reject_once" "reject_always")
  "Permission option kinds that refuse a call, the least lasting first.")

(defun aob-reject (s &optional decision)
  "Refuse DECISION on S, or the permission S has pending, with its own no.
The no that lasts least is taken: a refusal is about the call that was
made, never about every call shaped like it.  A decision offering no
refusal at all is left standing, since granting is not the fallback for
a refusal.  Returns the option id sent, nil when nothing was."
  (when-let* ((d (or decision
                     (seq-find (lambda (one)
                                 (not (eq (plist-get one :kind) 'elicitation)))
                               (aob-session-decisions s))))
              (options (plist-get d :options))
              (id (seq-some
                   (lambda (kind)
                     (plist-get (seq-find (lambda (option)
                                            (equal (plist-get option :kind) kind))
                                          options)
                                :optionId))
                   aob--reject-kinds)))
    (aob--call s :resolve d id)
    id))

(defun aob-dired (s)
  "Open S's working directory (its worktree when isolated)."
  (interactive (list (aob-target)))
  (dired (or (aob-session-dir s) (aob-session-project s)))
  (ygg-ui-plain-layout))

;;; Compose — multi-line intent editing; sends to the target session,
;;; or hands the text to `aob-compose-spawn-function' when no session
;;; exists yet.  The draft reads as one input box: a title row saying
;;; where it goes, the text under it, and a footer naming the keys and
;;; what it carries.  The map names those keys; a host idiom (this one
;;; speaks vim) binds its own beside them and says so in the hint.

(defvar aob-compose-spawn-function nil
  "Function (TEXT &optional AGENT ATTACHMENTS) spawning an agent on compose send.
AGENT names an agent definition when the target was picked as (new . AGENT);
ATTACHMENTS are image file paths riding along with the first prompt.")

(defvar-local aob-compose--target nil)

(defvar-local aob-compose--dir nil
  "The folder the caller named for this draft, or nil when it named none.
A spawn on send belongs where the caller said; where nobody said, the
buffer's own folder is a fallback and not an answer, so a host that
resolves one for itself needs to tell the two apart.")

(defvar aob-compose-before-send-functions nil
  "Abnormal hook run with the prompt text just before it is sent.
Where a host turns what was written into what it means, a mention of a
path into a file the turn carries, while the words are still on screen.
A function that returns a string hands back the text the turn sends
instead; any other return leaves the text as it was.")

(defun aob-compose--rewritten (text)
  "TEXT once every before-send function has had its say."
  (run-hook-wrapped 'aob-compose-before-send-functions
                    (lambda (fn)
                      (let ((out (funcall fn text)))
                        (when (stringp out) (setq text out)))
                      nil))
  text)

(defface aob-compose-title
  '((t :foreground "#0091FF" :weight bold))
  "The one loud thing on the box: what the draft is for.
The palette's accent, spent on the target and on nothing else here."
  :group 'aob)

(defface aob-compose-tag
  '((((background dark)) :foreground "#626262")
    (t :foreground "#a3a3a3"))
  "The small tags beside the title, the keys and the weight: tertiary grey."
  :group 'aob)

(defface aob-compose-rule
  '((((background dark)) :underline "#333333")
    (t :underline "#d9d9d9"))
  "The thin line under the title row, which is the top edge of the box."
  :group 'aob)

(defvar aob-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'aob-compose-send)
    (define-key map (kbd "C-c C-k") #'aob-compose-abort)
    (define-key map (kbd "C-c C-a") #'aob-compose-attach)
    map)
  "The keys the footer names; a host idiom binds its own beside them.")

(defvar aob-compose-hint "C-c C-c send · C-c C-a attach · C-c C-k abort"
  "Keys shown in the compose footer; the host config overrides it.")

(defcustom aob-compose-display-action
  '((display-buffer-at-bottom) (window-height . 10))
  "How the compose box is shown: an action for pop-to-buffer.
A box at the bottom of the frame by default, ten lines high and growing
with the draft, the way a command line sits under the work."
  :type 'sexp :group 'aob)

(defcustom aob-compose-float nil
  "Whether the compose box floats as a child frame over the top centre.
Off, which is the default, or without a display that can, it is a window
placed by aob-compose-display-action, a box at the bottom."
  :type 'boolean :group 'aob)

(defcustom aob-compose-float-width 0.7
  "How wide the floating box is, as a share of the frame's columns."
  :type 'number :group 'aob)

(defcustom aob-compose-float-height 12
  "How many lines the floating box shows when the draft is short."
  :type 'natnum :group 'aob)

(defcustom aob-compose-float-max-height 32
  "How many lines the floating box may grow to as the draft gets longer."
  :type 'natnum :group 'aob)

(declare-function posframe-show "posframe")
(declare-function posframe-delete "posframe")
(declare-function posframe-poshandler-frame-top-center "posframe")
(declare-function posframe-poshandler-frame-bottom-center "posframe")

(defcustom aob-compose-float-poshandler #'posframe-poshandler-frame-top-center
  "Where the floating box stands: a posframe poshandler."
  :type 'function :group 'aob)

(defun aob-compose--float-p ()
  "Whether the box can float here."
  (and aob-compose-float (display-graphic-p) (fboundp 'posframe-show)))

(defun aob-compose-frame-buffer (frame)
  "The buffer FRAME floats for posframe, or nil when it is no float.
Posframe keeps the buffer under its name in the parameter."
  (let ((held (frame-parameter frame 'posframe-buffer)))
    (cond ((bufferp held) held)
          ((and (consp held) (bufferp (cdr held))) (cdr held)))))

(defun aob-compose--float-frame (buffer)
  "The child frame BUFFER floats in, or nil when it is not floating.
Found by posframe's own mark on the frame, else by the window showing
BUFFER when that window's frame is a child frame."
  (or (seq-find (lambda (frame)
                  (and (frame-parent frame)
                       (eq (aob-compose-frame-buffer frame) buffer)))
                (frame-list))
      (when-let* ((window (get-buffer-window buffer t))
                  (frame (window-frame window))
                  ((frame-parent frame)))
        frame)))

(defun aob-compose--top-frame (frame)
  "The top-level frame FRAME stands on, itself when it is one.
A box opened from inside another float must hang off the real frame,
never off the float."
  (while (frame-parent frame) (setq frame (frame-parent frame)))
  frame)

(defun aob-compose-show (buffer &optional no-focus)
  "Show BUFFER as the compose box, floating when it can, and give it
focus unless NO-FOCUS.  Returns the window it stands in."
  (if (aob-compose--float-p)
      (let* ((parent (aob-compose--top-frame (selected-frame)))
             (frame (with-selected-frame parent
                      (posframe-show
                       buffer
                       :poshandler aob-compose-float-poshandler
                     :width (max 40 (round (* aob-compose-float-width
                                              (frame-width parent))))
                     :height aob-compose-float-height
                     :min-height aob-compose-float-height
                     :border-width 0
                     :border-color (face-attribute 'vertical-border
                                                   :foreground nil t)
                     :respect-header-line t
                       :respect-mode-line t
                       :accept-focus t))))
        (unless no-focus
          (select-frame-set-input-focus frame)
          (select-window (frame-root-window frame)))
        (frame-root-window frame))
    (if no-focus
        (display-buffer buffer aob-compose-display-action)
      (pop-to-buffer buffer aob-compose-display-action)
      (get-buffer-window buffer))))

(defun aob-compose--wanted-height ()
  "The lines the box wants for what the draft holds now, within bounds."
  (let ((lines (count-screen-lines (point-min) (point-max) nil
                                   (get-buffer-window (current-buffer) t))))
    (max aob-compose-float-height
         (min aob-compose-float-max-height (+ lines 2)))))

(defun aob-compose--autogrow ()
  "Grow or shrink the box to fit the draft, after each command.
A floating box is a frame whose height is set; a box at the bottom is a
window resized within the same bounds, and never one the frame cannot
spare."
  (let ((frame (aob-compose--float-frame (current-buffer))))
    (if (and frame (frame-live-p frame))
        (let ((wanted (aob-compose--wanted-height)))
          (unless (= wanted (frame-height frame))
            (set-frame-height frame wanted)))
      (when-let* ((window (get-buffer-window (current-buffer)))
                  ((window-live-p window))
                  ((not (window-full-height-p window))))
        (let ((wanted (aob-compose--wanted-height)))
          (unless (= wanted (window-height window))
            (ignore-errors
              (window-resize window (- wanted (window-height window))))))))))

(defun aob-compose--keep-float-clean (frame)
  "Move any buffer but the draft out of a compose float FRAME.
A key pressed in the float that shows a sidebar, a panel or a file
would otherwise open it inside the box; it belongs to the frame under
it, where it is shown instead."
  (when-let* (((frame-live-p frame))
              (parent (frame-parent frame))
              (draft (aob-compose-frame-buffer frame))
              ((buffer-live-p draft))
              ((with-current-buffer draft (derived-mode-p 'aob-compose-mode))))
    (dolist (window (window-list frame 'no-minibuf))
      (let ((buffer (window-buffer window)))
        (unless (or (eq buffer draft)
                    (string-prefix-p " " (buffer-name buffer))
                    (minibufferp buffer))
          (ignore-errors (delete-window window))
          (with-selected-frame parent
            (if (fboundp 'ygg-ui-show)
                (ygg-ui-show buffer t)
              (display-buffer buffer))))))))

(add-hook 'window-buffer-change-functions #'aob-compose--keep-float-clean)

(defun aob-compose--hide-when-left (frame)
  "Hide a compose float once the owner works in the frame under it.
The draft is kept; the next compose to the same target, or a window
motion up into it, shows it again.  FRAME is the frame whose selected
window changed."
  (when (and (not (frame-parent frame))
             (eq frame (selected-frame)))
    (dolist (float (frame-list))
      (when (and (frame-live-p float)
                 (eq (frame-parent float) frame)
                 (frame-visible-p float)
                 (not (eq float (selected-frame))))
        (when-let* ((buffer (aob-compose-frame-buffer float))
                    ((buffer-live-p buffer))
                    ((with-current-buffer buffer
                       (derived-mode-p 'aob-compose-mode))))
          (posframe-hide buffer))))))

(add-hook 'window-selection-change-functions #'aob-compose--hide-when-left)

(declare-function posframe-hide "posframe")

(defun aob-compose-hide ()
  "Take the compose box off the screen and keep the draft.
The words stay in the buffer, and the next compose to the same target
opens on them again."
  (interactive)
  (let ((buffer (current-buffer)))
    (if-let* ((frame (aob-compose--float-frame buffer)))
        (progn
          (when-let* ((parent (frame-parent frame)))
            (select-frame-set-input-focus parent))
          (posframe-hide buffer))
      (quit-window))))

(defun aob-compose-close (&optional draft)
  "Take the compose box off the screen, killing its buffer.
DRAFT is the draft to close, the current buffer when nil: a send that
opened other buffers on the way still closes the box it was sent from."
  (let ((buffer (or draft (current-buffer))))
    (if-let* ((frame (aob-compose--float-frame buffer)))
        (progn
          (when-let* ((parent (frame-parent frame)))
            (select-frame-set-input-focus parent))
          (posframe-delete buffer)
          (kill-buffer buffer))
      (if-let* ((window (get-buffer-window buffer t)))
          (quit-window t window)
        (kill-buffer buffer)))))

(defvar aob-compose-placeholder
  (concat "Write what you want done — @ a file, a folder, a preset "
          "or the quickfix, / a skill")
  "What the empty box says it wants, in the dimmed face.")

(defvar aob-compose-status-function nil
  "Function returning what the footer says on the right, or nil.
Where a host weighs what the draft carries, that line stands here.")

(defconst aob-compose--mention-rx "@@?[^@[:space:]\n,;:)]+"
  "What an at-mention looks like when the footer counts them.")

(defvar-local aob-compose--label nil
  "What this draft is for, as the title row says it.")

(defvar-local aob-compose--tags nil
  "The small tags after the title: the mode, the root, the presets, the model.")

(defvar aob-compose-panel-hint nil
  "The key opening what the draft runs under, as the footer's left says it.
One word and the key that reaches it; nil where a host binds no such
key, and the footer then names only what the draft carries.")

(defvar-local aob-compose--placeholder nil
  "The overlay carrying the placeholder, which is never buffer text.")

(defun aob-compose--plain (text)
  "TEXT with the per-cent a header or mode line would read as its own."
  (replace-regexp-in-string "%" "%%" text t t))

(defun aob-compose--header ()
  "The title row: what the draft is for, and the tags it carries."
  (concat " "
          (propertize (aob-compose--plain (or aob-compose--label "new agent"))
                      'face 'aob-compose-title)
          (when aob-compose--tags
            (propertize (aob-compose--plain
                         (concat "   " (string-join aob-compose--tags " · ")))
                        'face 'aob-compose-tag))))

(defun aob-compose--carried ()
  "The attachments and the mentions the draft carries, named for the footer."
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (attached (seq-count (lambda (a)
                                (string-search (format "[[Image%d]]" (car a))
                                               text))
                              aob-compose--attachments))
         (mentions 0)
         (start 0)
         parts)
    (while (string-match aob-compose--mention-rx text start)
      (setq mentions (1+ mentions)
            start (match-end 0)))
    (when (> attached 0) (push (format "%d attached" attached) parts))
    (when (> mentions 0) (push (format "%d mention(s)" mentions) parts))
    (string-join (nreverse parts) " · ")))

(defun aob-compose--draft-tokens ()
  "What the draft itself weighs, as N tokens, the words alone."
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (tokens (if (fboundp 'ygg-ui-tokens)
                     (ygg-ui-tokens text)
                   (ceiling (length text) 4))))
    (format "%d token%s" tokens (if (= tokens 1) "" "s"))))

(defun aob-compose--footer ()
  "The pinned last row: on the left the key that opens what the draft
runs under and what the draft carries; on the right the draft's own
tokens, then what the host weighs the whole turn at."
  (let* ((left (concat " " (string-join
                            (seq-remove #'string-empty-p
                                        (list (or aob-compose-panel-hint "")
                                              (aob-compose--carried)))
                            " · ")))
         (status (and aob-compose-status-function
                      (funcall aob-compose-status-function)))
         (right (concat (aob-compose--draft-tokens)
                        (if (and status (not (string-empty-p status)))
                            (concat " · " status)
                          "")
                        " "))
         (pad (max 1 (- (window-width) (string-width left)
                        (string-width right)))))
    (concat (propertize (aob-compose--plain left) 'face 'aob-compose-tag)
            (make-string pad ?\s)
            (propertize (aob-compose--plain right) 'face 'aob-compose-tag))))

(defun aob-compose--placeholder-refresh (&rest _)
  "Say what the box wants while it is empty, and nothing once it is not."
  (when (overlayp aob-compose--placeholder)
    (overlay-put aob-compose--placeholder 'before-string
                 (and (= (point-min) (point-max))
                      (propertize aob-compose-placeholder
                                  'face 'shadow 'cursor t)))))

(define-derived-mode aob-compose-mode text-mode "aob-compose"
  "Write a multi-line prompt; send with `aob-compose-send'."
  (ygg-ui-plain-layout)
  ;; ordered ahead of the global tail so the clamped dabbrev wins over the
  ;; unclamped one; commands/@file still take precedence over both
  (add-hook 'post-command-hook #'aob-compose--autogrow nil t)
  (add-hook 'completion-at-point-functions #'aob-compose-capf -20 t)
  (add-hook 'completion-at-point-functions #'aob-compose--dabbrev-capf -10 t)
  ;; eager, and mid-word: this is a scratchpad for prose, not code
  (setq-local corfu-auto-prefix 1
              tab-always-indent 'complete)
  (font-lock-add-keywords nil '(("\\[\\[Image[0-9]+\\]\\]" 0 'success prepend)))
  (face-remap-add-relative 'header-line 'aob-compose-rule)
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (when (facep face)
      (face-remap-add-relative face '(:inherit default :box nil
                                      :underline nil :overline nil))))
  (setq mode-line-format '((:eval (aob-compose--footer))))
  (dolist (old (overlays-in (point-min) (point-max)))
    (when (overlay-get old 'aob-compose-placeholder) (delete-overlay old)))
  (setq aob-compose--placeholder
        (make-overlay (point-min) (point-min) nil t nil))
  (overlay-put aob-compose--placeholder 'aob-compose-placeholder t)
  (aob-compose--placeholder-refresh)
  (add-hook 'after-change-functions #'aob-compose--placeholder-refresh nil t))

(defun aob-compose (&optional target initial name dir)
  "Compose a multi-line prompt.
TARGET is a session, (new . AGENT) to spawn AGENT on send, a function
called with the text and attachments on send, or nil for the default
agent.  INITIAL seeds the buffer — selections, refs — with point after
it, ready for your words.  NAME, when given, is what the buffer is
called: a function target has no name of its own to take one from.

The buffer stands where the prompt is going: DIR when the caller names
one, else a session target's own folder, else the folder of whatever
opened it.  It is set on every call, not only on creation — the buffer
for a target is reused, and a reused draft that kept yesterday's folder
completes `@file' and `/skill' against the wrong tree."
  (interactive (list (or (aob-session-at-point)
                         (and (aob-live-sessions) (aob-read-session "To: ")))))
  ;; one compose buffer per target: drafts to different agents coexist,
  ;; and ZZ always sends to the agent named in this buffer's header
  (let ((named dir)
        (dir (or dir
                 (and (aob-session-p target)
                      (or (aob-session-dir target) (aob-session-project target)))
                 default-directory))
        (buf (get-buffer-create
              (cond (name (format "compose:%s" name))
                    ((aob-session-p target)
                     (format "compose:%s" (aob-session-name target)))
                    ((and (consp target) (not (functionp target)))
                     (format "compose:new-%s" (cdr target)))
                    (t "compose:new")))))
    (with-current-buffer buf
      (aob-compose-mode)
      (setq default-directory (file-name-as-directory (expand-file-name dir)))
      (setq aob-compose--dir
            (and named (file-name-as-directory (expand-file-name named))))
      (setq aob-compose--target
            (if (aob-session-p target) (aob-session-id target) target))
      (setq aob-compose--label
            (concat "→ "
                    (cond ((aob-session-p target) (aob-session-name target))
                          ((functionp target) (or name "caller"))
                          ((consp target) (format "new %s" (cdr target)))
                          (t "new agent"))))
      (setq aob-compose--tags nil)
      (setq header-line-format '((:eval (aob-compose--header))))
      (when initial (insert initial) (goto-char (point-max))))
    (aob-compose-show buf)
    buf))

(defvar aob-compose-history nil
  "Sent prompts, newest first.")

(defvar-local aob-compose--attachments nil
  "Alist of (N . FILE); [[ImageN]] in the text is what keeps FILE aboard.")

(defvar-local aob-compose-allow-empty nil
  "Whether this draft may be sent with nothing written.
A draft whose send is a shot with the words as an optional note, the
QA draft, sets it; a prompt to an agent never does.")

(defvar-local aob-compose-after-send nil
  "Function called once the draft is sent and the compose buffer is gone.
Where a send leaves you is the sender's business — a Work card, a
quickfix — and it cannot be opened from inside the draft: the window it
would land in is the one about to quit itself, taking the buffer with it.")

(defvar-local aob-compose--steer nil
  "Non-nil when this draft is a correction for the turn already running.
Set by `aob-steer'; re-entering the mode clears it, so a plain compose
to the same agent queues as it always did.")

(defun aob-compose-attach (file)
  "Attach an image FILE as an [[ImageN]] token at point.
The token is plain editable text, and deleting it drops the attachment;
a FILE already attached is the token it has, never a second copy."
  (interactive "fAttach image: ")
  (let* ((path (expand-file-name file))
         (held (car (rassoc path aob-compose--attachments)))
         (n (or held
                (1+ (apply #'max 0 (mapcar #'car aob-compose--attachments))))))
    (unless held
      (push (cons n path) aob-compose--attachments))
    (insert (format "[[Image%d]]" n))))

(defun aob-compose--clipboard-image ()
  "The clipboard's image written to a temp png, or nil when it holds none."
  (when (executable-find "pngpaste")
    (let ((f (make-temp-file "aob-clip-" nil ".png")))
      (if (eq 0 (call-process "pngpaste" nil nil nil f))
          f
        (delete-file f)
        nil))))

(defun aob-compose-paste ()
  "Paste into the compose buffer: a clipboard image attaches, text yanks."
  (interactive)
  (if-let* ((f (aob-compose--clipboard-image)))
      (progn (aob-compose-attach f)
             (message "aob: clipboard image attached as [[Image%d]]"
                      (caar aob-compose--attachments)))
    (yank)))

(define-key aob-compose-mode-map [remap yank] #'aob-compose-paste)

(defun aob-compose--harvest (raw)
  "RAW compose text split into (TEXT . FILES): tokens stripped, and only
attachments whose [[ImageN]] survived the user's editing ride along."
  (let (files)
    (dolist (a (reverse aob-compose--attachments))
      (when (string-match-p (regexp-quote (format "[[Image%d]]" (car a))) raw)
        (push (cdr a) files)))
    ;; #name became a mention while writing; on the way out it becomes the
    ;; code, so the agent reads what you meant rather than going to find it
    (setq raw (replace-regexp-in-string
               "#\\([^#[:space:],.;:)]+\\)"
               (lambda (m)
                 ;; reading the definition moves point and matches: without
                 ;; this the replacement lands against stale match data
                 (let ((name (match-string 1 m)))
                   (or (save-match-data
                         (ignore-errors (aob-compose--code-block name)))
                       m)))
               raw t t))
    (cons (string-trim (replace-regexp-in-string
                        "[ \t]*\\[\\[Image[0-9]+\\]\\][ \t]*" " " raw))
          (nreverse files))))

(defun aob-compose-send ()
  (interactive)
  (pcase-let* ((draft (current-buffer))
               (raw (buffer-substring-no-properties (point-min) (point-max)))
               (raw (aob-compose--rewritten raw))
               (`(,text . ,atts) (aob-compose--harvest raw))
               (tgt aob-compose--target)
               (session (and (stringp tgt) (aob-session-get tgt))))
    (when (and (string-empty-p text) (null atts)
               (not aob-compose-allow-empty))
      (user-error "aob: empty prompt"))
    (add-to-history 'aob-compose-history text)
    ;; send before killing the buffer — a refused send must not eat the text
    ;; steering carries text only; a draft with images queues rather than
    ;; sending as a correction that silently lost its attachments
    (cond ((and session aob-compose--steer (null atts))
           (aob-interject session text))
          (session (aob-prompt session text atts))
          ((stringp tgt) (user-error "aob: target session is gone"))
          ;; a function target wants the words themselves rather than a
          ;; session to send them to — a caller composing a brief, say
          ((functionp tgt) (funcall tgt text atts))
          (aob-compose-spawn-function
           (funcall aob-compose-spawn-function text (and (consp tgt) (cdr tgt))
                    atts))
          (t (user-error "aob: no session and no spawn function")))
    ;; read after the send: a refused send leaves you in the draft, and a
    ;; spawn function is free to say where this one should land
    (let ((after aob-compose-after-send))
      (aob-compose-close draft)
      (when after (funcall after)))))

(defun aob-compose-abort ()
  (interactive)
  (aob-compose-close))

(defun aob-compose-recall (&optional n)
  "Reopen a sent prompt to keep editing it — `aob-compose-history' as
numbered registers.  With numeric prefix N, recall the Nth most-recent
\(1 = last); otherwise pick one.  Inside a compose buffer the text lands
at point; elsewhere it opens a fresh compose buffer to edit and resend."
  (interactive "P")
  (unless aob-compose-history (user-error "aob: no prompt history"))
  (let ((text
         (if n
             (or (nth (1- (prefix-numeric-value n)) aob-compose-history)
                 (user-error "aob: only %d prompt(s) in history"
                             (length aob-compose-history)))
           (let ((cands (cl-loop for p in aob-compose-history for i from 1
                                 collect (cons (format "%2d  %s" i (aob--first-line p 70))
                                               p))))
             (cdr (assoc (completing-read "Recall prompt: " (mapcar #'car cands) nil t)
                         cands))))))
    (if (derived-mode-p 'aob-compose-mode)
        (insert text)
      (aob-compose (and (aob-live-sessions) (aob-read-session "Recall to: ")) text))))

;;; Completion — /commands advertised by agents (skills surface there)
;;; and @file refs, as one capf; Corfu/TAB render it wherever it's live

(declare-function cape-dabbrev "cape")
(defvar corfu-auto-prefix)

(defun aob-compose--trim-suffix (_cand status)
  "Drop the stale word suffix left behind after a mid-word completion."
  (when (eq status 'finished)
    (delete-region (point) (save-excursion (skip-syntax-forward "w_") (point)))))

(defun aob-compose--dabbrev-capf ()
  "Word completion that fires mid-word and still rewrites the whole word.
Narrowing to point makes `cape-dabbrev' offer prefix matches with the
cursor inside a word; `aob-compose--trim-suffix' then clears the tail so
accepting a candidate replaces the word rather than splicing into it."
  (when (fboundp 'cape-dabbrev)
    (pcase (save-restriction
             (narrow-to-region (point-min) (point))
             (cape-dabbrev))
      (`(,beg ,end ,table . ,plist)
       `(,beg ,end ,table :exit-function aob-compose--trim-suffix ,@plist)))))

(defvar aob--files-cache (make-hash-table :test #'equal))

(defun aob--project-files (dir)
  "DIR's tracked files, asked of git once.
A miss is cached as itself: outside a repo the answer is no files, and
`or' over a nil entry would fork git again on every keystroke."
  (let ((known (gethash dir aob--files-cache 'miss)))
    (if (eq known 'miss)
        (puthash dir (ignore-errors (process-lines "git" "-C" dir "ls-files"))
                 aob--files-cache)
      known)))

(defun aob-files-cache-clear ()
  "Drop the @file completion cache (it goes stale as files are added)."
  (interactive)
  (clrhash aob--files-cache))

(defun aob--capf-file-table (dir)
  "Completion table for @refs, rooted at DIR.
A bare word fuzzy-matches the whole tracked tree; once a `/' appears the
token switches to directory-aware path completion — descending dirs,
untracked files, and absolute or ~ paths all resolve live off disk."
  (lambda (string pred action)
    (if (string-search "/" string)
        (let ((default-directory dir))
          (complete-with-action action #'completion-file-name-table string pred))
      (complete-with-action action (aob--project-files dir) string pred))))

(defun aob--capf-session ()
  (and (stringp aob-compose--target) (aob-session-get aob-compose--target)))

(defvar aob-capf-command-functions nil
  "Functions called with the compose directory, each returning command plists.
What they return completes after `/' beside the session's own commands.")

(defun aob--capf-commands ()
  "Commands of the target session, else the union across live sessions,
and whatever `aob-capf-command-functions' add for the compose directory."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (c (append
                (seq-mapcat (lambda (s) (aob-session-ref s :commands))
                            (delq nil (cons (aob--capf-session)
                                            (aob-live-sessions))))
                (seq-mapcat (lambda (fn) (ignore-errors (funcall fn (aob--capf-dir))))
                            aob-capf-command-functions)))
      (let ((n (plist-get c :name)))
        (unless (gethash n seen)
          (puthash n c seen)
          (push c out))))
    (nreverse out)))

(defun aob--capf-dir ()
  (or (when-let* ((s (aob--capf-session))) (aob-session-dir s))
      default-directory))

(defun aob-compose-capf ()
  "Complete /commands at input start and @file refs anywhere.
Each branch declares its prefix length whole, so the popup stands the
moment the sign is typed rather than one character after it."
  (let ((anchor (if (minibufferp) (minibuffer-prompt-end)
                  (line-beginning-position))))
    (cond
     ;; the /command token — only as the very first thing typed
     ((and (> (point) anchor)
           (eq (char-after anchor) ?/)
           (not (string-match-p "[[:space:]]"
                                (buffer-substring anchor (point)))))
      (when-let* ((cmds (aob--capf-commands)))
        (list (1+ anchor) (point)
              (mapcar (lambda (c) (plist-get c :name)) cmds)
              :company-prefix-length t
              :annotation-function
              (lambda (cand)
                (when-let* ((c (seq-find (lambda (x)
                                           (equal (plist-get x :name) cand))
                                         cmds)))
                  (concat "  " (or (plist-get c :description) ""))))
              :exclusive 'no)))
     ;; an @file token
     ((save-excursion
        (re-search-backward "@\\([^@[:space:]]*\\)\\="
                            (max anchor (- (point) 200)) t))
      (let ((dir (aob--capf-dir)))
        (list (1+ (match-beginning 0)) (point)
              (aob--capf-file-table dir)
              :company-prefix-length t
              :exclusive 'no)))
     ;; a #definition token — the code itself, not a path to go read
     ((save-excursion
        (re-search-backward "#\\([^#[:space:]]*\\)\\="
                            (max anchor (- (point) 200)) t))
      (let ((defs (aob-compose--definitions)))
        (list (1+ (match-beginning 0)) (point)
              (mapcar #'car defs)
              :company-prefix-length t
              :annotation-function
              (lambda (cand)
                (when-let* ((d (assoc cand defs)))
                  (concat "  " (file-name-nondirectory (cadr d)))))
              :exclusive 'no))))))

(defun aob-compose--definitions ()
  "Named definitions in the file buffers you have open, as (NAME FILE POS).
`imenu' is what every major mode already answers this with, so this
knows about whatever languages your Emacs does and nothing more."
  (let (out)
    (dolist (b (buffer-list) (nreverse out))
      (when (and (buffer-file-name b) (buffer-live-p b))
        (with-current-buffer b
          (dolist (entry (ignore-errors (aob--imenu-flat)))
            (push (list (car entry) (buffer-file-name b) (cdr entry)) out)))))))

(defun aob--imenu-flat (&optional index prefix)
  "This buffer's imenu as a flat ((NAME . POS)...), nested entries included."
  (require 'imenu)
  (let (out)
    (dolist (item (or index (imenu--make-index-alist t)) (nreverse out))
      (cond
       ((not (consp item)))
       ((and (consp (cdr item)) (not (number-or-marker-p (cdr item))))
        (setq out (nconc (nreverse (aob--imenu-flat (cdr item) (car item))) out)))
       ((number-or-marker-p (cdr item))
        (unless (equal (car item) "*Rescan*")
          (push (cons (if prefix (format "%s/%s" prefix (car item)) (car item))
                      (cdr item))
                out)))))))

(defun aob-compose--code-block (name)
  "NAME's definition as a fenced block headed by where it came from, or nil."
  (when-let* ((d (assoc name (aob-compose--definitions)))
              (buf (get-file-buffer (cadr d))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (caddr d))
        ;; imenu points at the name; from the end of that line the enclosing
        ;; definition is the one behind you, not the one before it
        (let* ((beg (progn (end-of-line) (beginning-of-defun) (point)))
               (end (progn (goto-char beg) (end-of-defun) (point)))
               (lang (string-remove-suffix "-mode" (symbol-name major-mode))))
          (format "```%s %s:%d\n%s\n```"
                  (string-remove-suffix "-ts" lang)
                  (file-name-nondirectory (cadr d))
                  (line-number-at-pos beg)
                  (string-trim-right (buffer-substring-no-properties beg end))))))))

;;; Artifacts — any deliverable an agent writes as a file for review
;;; (spec, plan, report, ADR…) instead of acting directly.  One flow:
;;; ask in your own words, the turn stops at the file, the file opens,
;;; you edit, `aob-continue' resumes from the reviewed artifact.

(defcustom aob-artifact-directory ".aob"
  "Repo-relative directory agents write reviewable artifacts into."
  :type 'string :group 'aob)

(defcustom aob-artifact-auto-open t
  "Pop an artifact open for editing the moment its draft turn ends."
  :type 'boolean :group 'aob)

(defun aob--slug (text)
  (let ((s (downcase (replace-regexp-in-string
                      "[^[:alnum:]]+" "-" (car (split-string text "\n"))))))
    (string-trim (substring s 0 (min 40 (length s))) "-" "-")))

(defun aob-artifact-notice (s path)
  "Tag PATH as S's reviewable artifact when it lands in the artifact dir.
This is the whole artifact mechanism: no special send, no keyword — ask
for a spec/plan/report in your own words; any file the agent writes
under `aob-artifact-directory' becomes the reviewable artifact."
  (let ((root (file-name-as-directory
               (expand-file-name aob-artifact-directory (aob-session-dir s))))
        (abs (expand-file-name path (aob-session-dir s))))
    (when (and (string-prefix-p root abs)
               (not (and (equal (aob-session-ref s :artifact-file) abs)
                         (eq (aob-session-ref s :artifact-stage) 'approved))))
      (aob-session-put s :artifact-file abs)
      (aob-session-put s :artifact-stage 'draft))))

(defvar-local aob-artifact--session-id nil)

(defvar aob-artifact-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'aob-artifact-approve)
    map))

(define-minor-mode aob-artifact-mode
  "Reviewing an agent's artifact; approving resumes the agent."
  :lighter " artifact")

(defun aob-artifact-approve ()
  "Finish the review where it happens: save, close, resume the agent."
  (interactive)
  (let ((s (and aob-artifact--session-id
                (aob-session-get aob-artifact--session-id))))
    (unless s (user-error "aob: artifact session is gone"))
    (save-buffer)
    (quit-window)
    (aob-continue s)))

(defun aob--artifact-watch (s _old new)
  (when (and (eq new 'idle)
             (eq (aob-session-ref s :artifact-stage) 'draft))
    (let ((f (aob-session-ref s :artifact-file)))
      (cond
       ((and f (file-exists-p f))
        (aob-session-put s :artifact-stage 'review)
        (when aob-artifact-auto-open
          (with-current-buffer (find-file-other-window f)
            (aob-artifact-mode 1)
            (setq aob-artifact--session-id (aob-session-id s))))
        (message "aob: artifact ready — review, approve to continue (%s)"
                 (aob-session-name s)))
       (f (message "aob: %s ended its turn without writing %s"
                   (aob-session-name s) (file-name-nondirectory f)))))))

(add-hook 'aob-state-change-hook #'aob--artifact-watch)

(defun aob-continue (s)
  "Resume S from its reviewed artifact."
  (interactive
   (list (let ((with-art (seq-filter (lambda (s) (aob-session-ref s :artifact-file))
                                     (aob-live-sessions))))
           (cond ((null with-art) (user-error "aob: no artifact sessions"))
                 ((null (cdr with-art)) (car with-art))
                 (t (aob-read-session "Continue: " with-art))))))
  (let ((f (aob-session-ref s :artifact-file)))
    (when-let* ((buf (find-buffer-visiting f)))
      (with-current-buffer buf (save-buffer)))
    (aob-session-put s :artifact-stage 'approved)
    (aob-prompt s (format "The artifact at %s is reviewed and possibly edited.
Proceed based on it."
                          (file-relative-name f (aob-session-dir s))))))


;;; Modeline — a cached segment for `global-mode-string'; recomputed on
;;; state transitions only, never on the event stream

(defvar aob-modeline-string "")
(put 'aob-modeline-string 'risky-local-variable t)

(defun aob--modeline-refresh (&rest _)
  (let ((blocked 0) (working 0) (queued 0) (subs 0))
    (dolist (s (aob-sessions))
      (cl-incf queued (length (aob-session-ref s :queued)))
      (let ((sub (and (aob-session-ref s :parent-session) t)))
        (pcase (aob-session-state s)
          ('blocked (cl-incf blocked))
          ((or 'working 'starting)
           ;; a delegated agent is counted as itself, not as its sender:
           ;; three working agents and nine they sent is not twelve peers
           (if sub (cl-incf subs) (cl-incf working))))))
    (setq aob-modeline-string
          (concat
           (when (> blocked 0)
             (propertize (format " ✋%d" blocked) 'face 'error))
           (when (> working 0)
             (propertize (format " ●%d" working) 'face 'warning))
           (when (> subs 0)
             (propertize (format " ↳%d" subs) 'face 'warning))
           (when (> queued 0)
             (propertize (format " »%d" queued) 'face 'shadow))))
    (force-mode-line-update t)))

(define-minor-mode aob-modeline-mode
  "Show agent attention counts (✋ blocked, ● working) in the modeline."
  :global t :group 'aob
  (if aob-modeline-mode
      (progn
        (add-hook 'aob-state-change-hook #'aob--modeline-refresh)
        (add-hook 'aob-session-created-hook #'aob--modeline-refresh)
        (add-hook 'aob-session-removed-hook #'aob--modeline-refresh)
        (add-hook 'aob-queue-change-hook #'aob--modeline-refresh)
        (unless (memq 'aob-modeline-string global-mode-string)
          ;; a symbol-headed list reads as the (SYMBOL THEN ELSE)
          ;; conditional and renders *invalid* — keep a string first
          (setq global-mode-string
                (append (or global-mode-string '("")) '(aob-modeline-string))))
        (aob--modeline-refresh))
    (remove-hook 'aob-state-change-hook #'aob--modeline-refresh)
    (remove-hook 'aob-session-created-hook #'aob--modeline-refresh)
    (remove-hook 'aob-session-removed-hook #'aob--modeline-refresh)
    (remove-hook 'aob-queue-change-hook #'aob--modeline-refresh)
    (setq global-mode-string (delq 'aob-modeline-string global-mode-string))))

(defvar aob-object-map
  (let ((map (make-sparse-keymap)))
    ;; j/k stay motions everywhere — cancel lives on c, never on k
    (define-key map "j" #'next-line)
    (define-key map "k" #'previous-line)
    (define-key map "p" #'aob-compose)
    (define-key map "i" #'aob-steer)
    (define-key map "c" #'aob-cancel)
    (define-key map "K" #'aob-kill-session)
    (define-key map "r" #'aob-resolve)
    (define-key map "R" #'aob-rename-session)
    (define-key map "E" #'aob-dired)
    (define-key map "g" #'aob-rerender)
    (define-key map (kbd "RET") #'aob-focus)
    map)
  "Verbs shared by every buffer that renders sessions.")

(provide 'aob)
;;; aob.el ends here
