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
(require 'ygg-comment)

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
  "Every session still to be reached for; a subagent only while it runs.
A session marked :hidden is never one: it runs for a caller, not for you."
  (seq-remove (lambda (s) (or (aob-session-ref s :hidden)
                              (memq (aob-session-state s)
                                    (if (aob-session-ref s :parent-session)
                                        '(dead failed done)
                                      '(dead failed)))))
              (aob-sessions)))

(defun aob-session-get (id)
  (gethash id aob--sessions))

(defun aob--free-id (id)
  "ID, or ID with a suffix no session is using.
Two conversations can be called the same thing — a title, an agent and
a number after a restart — and the registry is keyed by id: a second
session taking the first one's key is a session that disappears."
  (if (not (gethash id aob--sessions))
      id
    (let ((n 2))
      (while (gethash (format "%s#%d" id n) aob--sessions)
        (setq n (1+ n)))
      (format "%s#%d" id n))))

(defun aob-create-session (&rest args)
  "Make and register a session from ARGS; :refs are put on it before the hook."
  (let* ((refs (plist-get args :refs))
         (args (cl-loop for (k v) on args by #'cddr
                        unless (eq k :refs) append (list k v)))
         (s (apply #'aob-session--create
                   (append args (list :started (current-time) :nevents 0)))))
    ;; before the hook, which is where a session's id is written down
    (setf (aob-session-id s) (aob--free-id (aob-session-id s)))
    (puthash (aob-session-id s) s aob--sessions)
    (setq aob--order (cons (aob-session-id s)
                           (delete (aob-session-id s) aob--order)))
    ;; and what the session is — its agent, who sent it — for the same
    ;; reason: a hook that decides by it must not find it missing
    (cl-loop for (k v) on refs by #'cddr do (aob-session-put s k v))
    (run-hook-with-args 'aob-session-created-hook s)
    (aob--dirty)
    s))

(defun aob-remove-session (s)
  (remhash (aob-session-id s) aob--sessions)
  (aob--retire s))

(defun aob--retire (s &optional heir)
  "See S, already out of the registry, out of every list and view.
HEIR, the session that took S\='s place under S\='s own id, keeps that
id in the order and every view that answers to it."
  (let ((id (aob-session-id s))
        (same (and heir (equal (aob-session-id heir) (aob-session-id s)))))
    (unless same (setq aob--order (delete id aob--order)))
    (run-hook-with-args 'aob-session-removed-hook s)
    ;; gc: views of the dead object die with it, and the struct sheds its
    ;; bulk so closures still holding it keep only a husk
    (unless same
      (dolist (b (buffer-list))
        (when (and (equal (buffer-local-value 'aob-buffer-session-id b) id)
                   (buffer-live-p b))
          (kill-buffer b))))
    (setf (aob-session-events s) nil)
    (setf (aob-session-decisions s) nil)
    (aob-session-put s :queued nil)
    (aob--dirty)))

(defvar aob-session-succession-functions nil
  "Abnormal hook run with OLD and NEW as NEW is made to take OLD\='s place.
It runs before anything else hears of NEW, so a view of OLD handed over
here is the one that shows NEW: the same buffer, in the same windows.")

(defvar aob--succeeding nil
  "The session the next one made takes the place of, inside `aob-succeed'.")

(defvar aob--successor nil
  "The session made to take another\='s place, inside `aob-succeed'.")

(defun aob-succeed (old make)
  "Call MAKE for the session that takes OLD\='s place, and return it.
The one MAKE makes is handed OLD\='s events, marked :seeded, and through
`aob-session-succession-functions' OLD\='s views, so a conversation
brought back reads as it did until its agent says more.  OLD\='s id is
free while MAKE runs, for a successor that keeps OLD\='s name.  OLD goes
only once MAKE has answered with a session; when it signals or answers
anything else, OLD is where it was, its views handed back, and every
list still has it."
  (let* ((id (aob-session-id old))
         (next (cadr (member id aob--order)))
         (aob--successor nil)
         (made nil))
    (remhash id aob--sessions)
    (unwind-protect
        (let ((aob--succeeding old))
          (setq made (funcall make)))
      (if (and (aob-session-p made) (not (eq made old)))
          (progn (aob--retire old made)
                 (aob--order-before (aob-session-id made) next))
        (when-let* ((heir aob--successor))
          (run-hook-with-args 'aob-session-succession-functions heir old)
          (remhash (aob-session-id heir) aob--sessions)
          (aob--retire heir old))
        (puthash id old aob--sessions)
        (aob--order-before id next)))
    made))

(defun aob--order-before (id next)
  "Put ID in `aob--order' right before NEXT, or last when NEXT is nil."
  (let ((order (delete id aob--order)))
    (setq aob--order
          (if-let* ((tail (and next (member next order))))
              (append (seq-take order (- (length order) (length tail)))
                      (cons id tail))
            (append order (list id))))))

(defun aob--take-place (s)
  "Give S the events and views of the session it succeeds, if any."
  (when-let* ((old aob--succeeding))
    (setq aob--succeeding nil
          aob--successor s)
    (unless (aob-session-events s)
      (setf (aob-session-events s)
            (mapcar (lambda (ev) (plist-put (copy-sequence ev) :seeded t))
                    (aob-session-events old))
            (aob-session-nevents s) (length (aob-session-events s))))
    (run-hook-with-args 'aob-session-succession-functions old s)))

(add-hook 'aob-session-created-hook #'aob--take-place -90)

(defun aob-session-put (s key val)
  (setf (aob-session-extra s) (plist-put (aob-session-extra s) key val)))

(defun aob-session-ref (s key)
  (plist-get (aob-session-extra s) key))

(defcustom aob-dedupe-context t
  "Non-nil tells each session a context entry or an @file only once.
Held context goes out again only when it is new or changed, and a file
mentioned again unchanged goes as a link.  A compact or clear forgets
what was told, so the next message tells it again."
  :type 'boolean :group 'aob)

(defun aob-told (s key)
  "Alist of NAME to fingerprint that S was told under KEY.
Kept with the conversation id it was told in, so a session that now
talks to another conversation starts from nothing."
  (let ((rec (aob-session-ref s key)))
    (and (equal (car rec) (aob-session-ref s :acp-id)) (cdr rec))))

(defun aob-told-p (s key name print)
  "Non-nil when S was last told NAME under KEY as PRINT."
  (equal (cdr (assoc name (aob-told s key))) print))

(defun aob-tell (s key name print)
  "Note that S has now been told NAME under KEY as PRINT."
  (let ((told (copy-alist (aob-told s key))))
    (setf (alist-get name told nil nil #'equal) print)
    (aob-session-put s key (cons (aob-session-ref s :acp-id) told))))

(defvar aob-told-pending nil
  "What the prompt now being sent tells: a list of (KEY NAME PRINT).
A backend notes it on the session once the prompt lands, and carries it
with the prompt while it waits in the queue.")

(defun aob-tell-all (s pending)
  "Note on S every (KEY NAME PRINT) in PENDING."
  (pcase-dolist (`(,key ,name ,print) pending)
    (aob-tell s key name print)))

(defun aob-session-queued-p (s)
  "Non-nil when S has prompts waiting to go out when this turn settles.
Asked by whoever would send a turn of its own on the idle: the queue
flushes as soon as the turn ends, and two prompts cannot be in flight at
once, so the work that would follow waits for the queued turn instead."
  (and (aob-session-ref s :queued) t))

(defun aob-queue-move (s entry by)
  "Move ENTRY, queued for S, BY places: negative goes earlier, positive later.
Queued prompts go out as one message in queue order, so the order is the
order they are read in."
  (let* ((q (aob-session-ref s :queued))
         (at (or (seq-position q entry #'eq)
                 (user-error "aob: that message has already gone")))
         (to (max 0 (min (1- (length q)) (+ at by)))))
    (unless (= at to)
      (let ((rest (delq entry (copy-sequence q))))
        (aob-session-put s :queued (append (seq-take rest to) (list entry)
                                           (nthcdr to rest))))
      (run-hook-with-args 'aob-queue-change-hook s)
      (aob--dirty s))
    to))

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

(defvar aob-event-change-functions nil
  "Run with (SESSION EVENT) each time EVENT is made or changes in place.")

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
  (run-hook-with-args 'aob-event-change-functions s ev)
  (aob--dirty s))

(defun aob-session-native-child (s ev)
  "The session S's subagent call EV is traced in, or nil for none."
  (when-let* ((root (or (aob-session-get (aob-session-ref s :native-root)) s))
              (kids (aob-session-ref root :native-kids))
              (id (gethash (plist-get ev :tool-id) kids)))
    (aob-session-get id)))

(declare-function aob-trace--sub-tail "aob-trace" (s ev))

(defun aob-session-kid-changed (kid)
  "Redraw the owner's Task line when KID's model or tokens change what it shows."
  (when-let* ((_ (aob-session-ref kid :native-tool-id))
              (owner (aob-session-get (aob-session-ref kid :parent-session)))
              (_ (fboundp 'aob-trace--sub-tail))
              (tid (aob-session-ref kid :native-tool-id))
              (ev (seq-find (lambda (e) (and (equal tid (plist-get e :tool-id))
                                             (plist-get e :subagent)))
                            (aob-session-events owner)))
              (tail (aob-trace--sub-tail owner ev))
              ((not (equal tail (plist-get ev :tail-drawn)))))
    (plist-put ev :tail-drawn tail)
    (plist-put ev :line nil)
    (aob--dirty owner)))

(defun aob-session-subagents (s)
  "Every subagent S delegated to, in the order it sent them.
A subagent is the Task event other tool calls name as their parent —
what the trace already rolls up, read back as a list."
  (seq-filter (lambda (e) (and (eq (plist-get e :type) 'tool)
                               (or (plist-get e :subagent)
                                   (plist-get e :children))))
              (reverse (aob-session-events s))))

(declare-function aob-subagent--native-sync "aob-subagent" (kid ev))
(declare-function aob-subagent-announced-end "aob-subagent" (kid state &optional why))

(defun aob-event-stopped-p (ev)
  "Whether tool call EV was left unfinished by the turn or agent that ran it."
  (and (equal (plist-get ev :ended) "stopped")
       (member (plist-get ev :status) '(nil "pending" "in_progress"))
       t))

(defun aob-event-revive (s ev)
  "Let tool call EV of S run again after it was settled as stopped."
  (when (equal (plist-get ev :ended) "stopped")
    (plist-put ev :ended nil)
    (plist-put ev :done-ts nil)
    (plist-put ev :line nil)
    (when-let* ((pid (plist-get ev :parent))
                (parent (seq-find (lambda (e) (equal (plist-get e :tool-id) pid))
                                  (aob-session-events s))))
      (plist-put parent :child-live (1+ (or (plist-get parent :child-live) 0)))
      (plist-put parent :line nil))
    (when-let* ((kid (aob-session-native-child s ev))
                ((fboundp 'aob-subagent--native-sync)))
      (aob-subagent--native-sync kid ev))
    (aob--dirty s)))

(defun aob-session--settle-call (s ev)
  "Mark tool call EV of S stopped, and its unfinished steps with it."
  (plist-put ev :ended "stopped")
  (unless (plist-get ev :done-ts)
    (plist-put ev :done-ts (float-time)))
  (plist-put ev :line nil)
  (plist-put ev :child-live 0)
  (when-let* ((tid (plist-get ev :tool-id)))
    (dolist (c (aob-session-events s))
      (when (and (equal (plist-get c :parent) tid)
                 (member (plist-get c :status) '("pending" "in_progress"))
                 (not (plist-get c :ended)))
        (plist-put c :ended "stopped")
        (plist-put c :line nil)))))

(defun aob-session-settle-subagents (s &optional gone)
  "Mark S's subagent calls that can no longer be running as stopped.
A turn that ended, or an agent that died, never sends their last update.
A call whose announced subagent is still working, or that the agent ran
in the background, outlives the turn; GONE says the agent itself is gone."
  (dolist (ev (aob-session-subagents s))
    (let ((kid (aob-session-native-child s ev)))
      (when (and gone kid (aob-session-ref kid :announced)
                 (fboundp 'aob-subagent-announced-end)
                 (memq (aob-session-state kid) '(working blocked starting)))
        (aob-subagent-announced-end kid 'done "cancelled"))
      (when (and (member (plist-get ev :status) '(nil "pending" "in_progress"))
                 (not (plist-get ev :ended))
                 (or gone
                     (and (not (plist-get ev :stand-in))
                          (not (let ((raw (plist-get ev :raw)))
                                 (and (listp raw)
                                      (eq t (plist-get raw :run_in_background)))))
                          (not (and kid (aob-session-ref kid :announced)
                                    (memq (aob-session-state kid)
                                          '(working blocked starting)))))))
        (aob-session--settle-call s ev)
        (when (and kid (fboundp 'aob-subagent--native-sync))
          (aob-subagent--native-sync kid ev))
        (aob--dirty s)))))

(defun aob-session-blurb (s)
  "S's current-activity line, built on demand."
  (let ((x (aob-session-summary s)))
    (cond ((stringp x) x)
          (x (aob-event-summary x)))))

(defun aob-session-model-now (s)
  "The model S is running on now, or nil."
  (or (aob-session-ref s :model-live)
      (aob-session-ref s :model-id)
      (aob-session-ref s :model-name)))

(defun aob-tokens-short (n)
  (cond ((null n) nil)
        ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
        ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
        (t (number-to-string n))))

;;; Meter — what a turn and a session cost in time, tokens and money.
;;; Only what the agent reports: cost when it sends one, tokens always.

(defvar aob-meter-change-hook nil
  "Run with a session when its clock or spend would read differently.")

(defun aob-duration-short (secs &optional exact)
  "SECS as a span: seconds under a minute, then minutes, then hours.
Coarse unless EXACT, so a live label changes once a minute, not once a
second.  EXACT keeps the seconds a settled turn took."
  (let ((secs (max 0 (floor (or secs 0)))))
    (cond ((< secs 60) (format "%ds" secs))
          ((< secs 3600)
           (if (and exact (> (mod secs 60) 0))
               (format "%dm %ds" (/ secs 60) (mod secs 60))
             (format "%dm" (/ secs 60))))
          (t (format "%dh%02dm" (/ secs 3600) (/ (mod secs 3600) 60))))))

(defun aob-cost-short (amount &optional currency)
  "AMOUNT of CURRENCY rounded up to the cent, so a fraction never reads as free."
  (when amount
    (concat (if (member currency '(nil "USD")) "$" (concat currency " "))
            (format "%.2f" (/ (ceiling (- (* amount 100) 1e-9)) 100.0)))))

(defvar aob--clock-timer nil)

(defvar aob-clock-shown-functions nil
  "Functions called with a session, non-nil when its clock is on screen.
A running clock ticks only while one of them says it is seen.")

(defun aob--clock-start ()
  (unless (timerp aob--clock-timer)
    (setq aob--clock-timer (run-with-timer 1 1 #'aob--clock-tick))))

(defun aob-turn-begin (s)
  "Start S's turn clock; return the stamp that owns it."
  (let ((stamp (float-time)))
    (aob-session-put s :turn-start stamp)
    (aob-session-put s :turn-cost nil)
    (aob--clock-start)
    stamp))

(defun aob-turn-end (s &optional stamp)
  "Stop the turn clock S started at STAMP; return the seconds it ran.
An answer to a turn a newer one has already replaced adds only the time
before that newer turn began, so no second is counted twice."
  (let* ((live (aob-session-ref s :turn-start))
         (stamp (or stamp live))
         (now (float-time)))
    (when (and stamp live)
      (let ((own (eql stamp live)))
        (aob-session-put s :work-secs
                         (+ (or (aob-session-ref s :work-secs) 0)
                            (max 0 (- (if own now live) stamp))))
        (when own
          (aob-session-put s :turn-start nil)
          (aob-session-put s :turn-secs (- now stamp)))
        (run-hook-with-args 'aob-meter-change-hook s)
        (- now stamp)))))

(defun aob-session-turn-secs (s)
  "Seconds S's running turn has taken so far, or nil between turns."
  (when-let* ((start (aob-session-ref s :turn-start)))
    (- (float-time) start)))

(defun aob-session-work-secs (s)
  "Seconds S has spent in turns, the running one included."
  (+ (or (aob-session-ref s :work-secs) 0) (or (aob-session-turn-secs s) 0)))

(defun aob-session-clock (s &optional with-total)
  "S's clock: the running turn, marked live, else the total it has worked.
WITH-TOTAL puts the total after a running turn where there is room."
  (if-let* ((secs (aob-session-turn-secs s)))
      (concat (aob-duration-short secs) "…"
              (when with-total
                (concat " / " (aob-duration-short (aob-session-work-secs s)))))
    (let ((total (aob-session-work-secs s)))
      (and (> total 0) (aob-duration-short total)))))

(defun aob--clock-tick ()
  "Move every running clock that is on screen; stop when none is."
  (let ((seen nil))
    (dolist (s (aob-sessions))
      (when (and (aob-session-ref s :turn-start)
                 (run-hook-with-args-until-success 'aob-clock-shown-functions s))
        (setq seen t)
        (let ((label (aob-session-clock s)))
          (unless (equal label (aob-session-ref s :clock-shown))
            (aob-session-put s :clock-shown label)
            (run-hook-with-args 'aob-meter-change-hook s)))))
    (unless seen
      (when (timerp aob--clock-timer) (cancel-timer aob--clock-timer))
      (setq aob--clock-timer nil))))

(defun aob--clock-on-display (&rest _)
  "Wind the clock again when a window or a frame brings a running one into view."
  (unless (timerp aob--clock-timer)
    (when (seq-some (lambda (s) (aob-session-ref s :turn-start)) (aob-sessions))
      (aob--clock-start)
      (aob--clock-tick))))

(add-hook 'window-buffer-change-functions #'aob--clock-on-display)
(add-function :after after-focus-change-function #'aob--clock-on-display)

(defun aob--clock-on-state (s _old new)
  (when (memq new '(idle done dead failed))
    (aob-turn-end s)))

(add-hook 'aob-state-change-hook #'aob--clock-on-state)

(defcustom aob-quiet-minutes 10
  "Minutes a working session may go without progress before it reads quiet.
Progress is a new message, or a tool call starting or ending; words
streaming into one message are not."
  :type 'natnum :group 'aob)

(defvar aob--quiet-timer nil)

(defun aob-session-quiet (s)
  "\"quiet 12m\" while working S has shown no progress for a while, else nil."
  (when-let* (((eq (aob-session-state s) 'working))
              (at (aob-session-ref s :progress-at))
              (secs (- (float-time) at))
              ((>= secs (* 60 aob-quiet-minutes))))
    (concat "quiet " (aob-duration-short secs))))

(defun aob--quiet-show (s label)
  (aob-session-put s :quiet-shown label)
  (run-hook-with-args 'aob-meter-change-hook s)
  (when (bound-and-true-p aob-modeline-mode) (aob--modeline-refresh)))

(defun aob-note-progress (s)
  "Say S moved just now, which lifts a quiet mark."
  (aob-session-put s :progress-at (float-time))
  (when (aob-session-ref s :quiet-shown)
    (aob--quiet-show s nil)))

(defun aob--quiet-tick ()
  (let ((working nil))
    (dolist (s (aob-sessions))
      (when (eq (aob-session-state s) 'working)
        (setq working t))
      (let ((label (aob-session-quiet s)))
        (unless (equal label (aob-session-ref s :quiet-shown))
          (aob--quiet-show s label))))
    (unless working
      (cancel-timer aob--quiet-timer)
      (setq aob--quiet-timer nil))))

(defun aob--quiet-on-state (s _old new)
  (when (eq new 'working)
    (aob-note-progress s)
    (unless (timerp aob--quiet-timer)
      (setq aob--quiet-timer (run-with-timer 30 30 #'aob--quiet-tick)))))

(add-hook 'aob-state-change-hook #'aob--quiet-on-state)

(defun aob-usage-note-cost (s amount &optional currency autonomous)
  "Take AMOUNT, S's running cost as the agent reports it, in CURRENCY.
The figure is cumulative and falls back when the agent's own count
restarts; a fall banks what came before.  What it grew by counts toward
the running turn unless AUTONOMOUS work the agent did on its own spent
it.  A first figure for a conversation resumed from disk carries turns
from before, so it is no turn's own, and so is a fork's.  A zero says
nothing: a crashed or failed start reports one before the count goes on."
  (when (and (numberp amount) (> amount 0))
    (let* ((prev (aob-session-ref s :cost-reading))
           (grew (cond ((null prev)
                        (unless (or (aob-session-ref s :restored-by)
                                    (aob-session-ref s :cost-inherited))
                          amount))
                       ((< amount prev)
                        (aob-session-put s :cost-banked
                                         (+ (or (aob-session-ref s :cost-banked) 0) prev))
                        amount)
                       (t (- amount prev)))))
      (aob-session-put s :cost-reading amount)
      (when currency (aob-session-put s :cost-currency currency))
      (when (and grew (not autonomous) (aob-session-ref s :turn-start))
        (aob-session-put s :turn-cost (+ (or (aob-session-ref s :turn-cost) 0) grew)))
      (run-hook-with-args 'aob-meter-change-hook s))))

(defun aob-session-cost (s)
  "What S has cost so far, as the agent counts it, or nil when it never said."
  (when-let* ((reading (aob-session-ref s :cost-reading)))
    (+ reading (or (aob-session-ref s :cost-banked) 0))))

(defun aob-usage-note-turn (s usage)
  "Add one turn's token USAGE, as the prompt response reports it, to S's totals."
  (dolist (k '(:inputTokens :outputTokens :cachedReadTokens
               :cachedWriteTokens :thoughtTokens :totalTokens))
    (when-let* ((n (plist-get usage k)))
      (when (numberp n)
        (aob-session-put s :tokens
                         (plist-put (aob-session-ref s :tokens) k
                                    (+ n (or (plist-get (aob-session-ref s :tokens) k) 0)))))))
  (aob-session-put s :turns (1+ (or (aob-session-ref s :turns) 0)))
  (aob-session-kid-changed s)
  (run-hook-with-args 'aob-meter-change-hook s))

(defun aob-session-spend (s)
  "S's spend in a word: its cost when the agent reports one, else its tokens."
  (let ((cost (aob-session-cost s)))
    (if (and cost (> cost 0))
        (aob-cost-short cost (aob-session-ref s :cost-currency))
      (when-let* ((n (plist-get (aob-session-ref s :tokens) :totalTokens))
                  ((> n 0)))
        (concat (aob-tokens-short n) " tok")))))

(defun aob-turn-meter (ev &optional currency)
  "The muted tail of stop event EV: how long the turn took and what it spent."
  (let ((out (plist-get (plist-get ev :usage) :outputTokens))
        (secs (plist-get ev :secs))
        (cost (plist-get ev :cost)))
    (mapconcat #'identity
               (delq nil (list (and secs (aob-duration-short secs t))
                               (and out (> out 0) (concat (aob-tokens-short out) " out"))
                               (and cost (> cost 0) (aob-cost-short cost currency))))
               " · ")))

(defun aob-usage-describe (s)
  "S's spend, whole: time, every token count the agent sent, and cost."
  (let ((tk (aob-session-ref s :tokens)))
    (mapconcat
     #'identity
     (delq nil
           (list (format "%s: %d turn%s in %s" (aob-session-name s)
                         (or (aob-session-ref s :turns) 0)
                         (if (eql (aob-session-ref s :turns) 1) "" "s")
                         (aob-duration-short (aob-session-work-secs s) t))
                 (when tk
                   (mapconcat
                    #'identity
                    (delq nil
                          (mapcar (lambda (kv)
                                    (when-let* ((n (plist-get tk (car kv))) ((> n 0)))
                                      (concat (aob-tokens-short n) " " (cdr kv))))
                                  '((:inputTokens . "in") (:outputTokens . "out")
                                    (:thoughtTokens . "thought")
                                    (:cachedReadTokens . "cache read")
                                    (:cachedWriteTokens . "cache write")
                                    (:totalTokens . "total"))))
                    " · "))
                 (when-let* ((cost (aob-session-cost s)))
                   (aob-cost-short cost (aob-session-ref s :cost-currency)))
                 (when-let* ((ctx (aob-session-ctx s))) (concat ctx " ctx"))))
     " · ")))

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
                   (if (plist-get ev :subagent) "└ "
                     (concat (or (plist-get ev :kind) "tool") " "))
                   (or (plist-get ev :title) "")
                   (if-let* ((st (plist-get ev :stat))) (concat "  " st) "")))
    ('message (aob-event-head ev 48))
    ('thought (concat "… " (aob-event-head ev 44)))
    ('prompt (concat "» " (aob-event-head ev 44)))
    ('permission (concat "■ " (or (plist-get ev :title) "permission")))
    ('plan (or (plist-get ev :title) "plan"))
    ('stop (format "%s%s"
                   (if-let* ((warning (plist-get ev :warning)))
                       (concat "stopped: " warning)
                     (format "done (%s)" (or (plist-get ev :reason) "end")))
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

(defun aob--line-start (n)
  "Where line N of this buffer begins, or the last line when it has fewer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- n))
    (when (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (point)))

(defun aob--redraw-keeping-lines (redraw)
  "Call REDRAW, then put point and each window on this buffer back on its line."
  (let ((here (line-number-at-pos))
        (wins (mapcar (lambda (w) (cons w (line-number-at-pos (window-point w))))
                      (get-buffer-window-list nil nil t))))
    (prog1 (funcall redraw)
      (dolist (w wins)
        (set-window-point (car w) (aob--line-start (cdr w))))
      (goto-char (aob--line-start here)))))

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

(defvar aob-prompt-typed nil
  "Non-nil while sending a prompt the owner typed themselves.
A trace shows whose turn it is by a mark in the gutter, and a prompt a
tool or a workflow put there is nobody standing at the keyboard.")

(defun aob-prompt (s text &optional attachments)
  "Send TEXT (plus image ATTACHMENTS) as a new prompt turn to S."
  (interactive (let ((s (aob-target)))
                 (list s (read-string (format "%s » " (aob-session-name s))))))
  (aob--call s :prompt text attachments))

(defun aob-cancel (s)
  "Cancel S's current turn; cancel again right away to fully cancel.
The first cancel stops the turn — queued prompts survive.  A second,
consecutive one drops the queue too, so nothing flushes back in.  Work
already produced always survives."
  (interactive (list (aob-target)))
  (let ((full (eq last-command 'aob-cancel)))
    (aob--call s :cancel full)
    (message "aob: %s %s" (aob-session-name s)
             (if full "fully cancelled — queue dropped" "cancelled (again to drop queue)"))))

(defun aob-interject (s text &optional atts)
  "Say TEXT to S now, into the turn it is running when it takes one.
ATTS are image files that go with it."
  (interactive (let ((s (aob-target)))
                 (list s (read-string (format "%s ⇄ " (aob-session-name s))))))
  (if-let* ((fn (aob-backend-fn s :interject)))
      (funcall fn s text atts)
    (aob--call s :cancel)
    (aob--call s :prompt text atts)))

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
  (aob-session-put s :named-by-user t)
  (aob--set-name s name)
  (when-let* ((renamed (aob-backend-fn s :rename)))
    (funcall renamed s)))

(declare-function aob--buffer-name "aob-trace" (kind s &optional suffix))

(defun aob--set-name (s name)
  (let ((id (aob-session-id s))
        (was (format "\\`\\([^:]+\\):%s\\(?:/\\(.*\\)\\)?\\'"
                     (regexp-quote (aob-session-name s)))))
    (setf (aob-session-name s) name)
    (dolist (buf (buffer-list))
      (let ((old (buffer-name buf)))
        (when (and old
                   (equal (buffer-local-value 'aob-buffer-session-id buf) id)
                   (string-match was old)
                   (fboundp 'aob--buffer-name))
          (let* ((new (aob--buffer-name (match-string 1 old) s (match-string 2 old)))
                 (stray (get-buffer new)))
            (when (and stray (not (eq stray buf))
                       (equal (buffer-local-value 'aob-buffer-session-id stray) id))
              (kill-buffer stray))
            (with-current-buffer buf (rename-buffer new t)))))))
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

(defun aob-session-awaiting-answer ()
  "The first session still able to take an answer to a pending Decision."
  (seq-find (lambda (s)
              (and (aob-session-decisions s)
                   (not (eq (aob-session-state s) 'dead))))
            (aob-sessions)))

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
                    (mapcar #'car opts) nil t nil nil (caar opts)))
             (id (cdr (assoc pick opts))))
        (aob--call s :resolve d id)
        (when (aob--rejects-p d id)
          (aob-ask-reject-reason s))))))

(defconst aob-other-choice "Other…"
  "The last of every question's choices: it reads an answer of your own.")

(defun aob--choice-note (label notes)
  "What NOTES say about LABEL beside it, or nil: its description, else a
title that is not the label itself."
  (when-let* ((note (cdr (assoc label notes)))
              (said (or (plist-get note :description)
                        (and (not (equal (plist-get note :title) label))
                             (plist-get note :title))))
              ((not (string-empty-p said))))
    (concat "  " (propertize (string-join (split-string said "\n" t " +") " ")
                             'face 'shadow))))

(defun aob--choice-table (labels &optional notes)
  "LABELS then aob-other-choice, kept in the order the agent gave them,
each annotated with what NOTES, as (LABEL :title :description), say of it."
  (let ((all (append labels (list aob-other-choice)))
        (metadata `(metadata (display-sort-function . identity)
                             (cycle-sort-function . identity)
                             ,@(when notes
                                 (list (cons 'annotation-function
                                             (lambda (label)
                                               (aob--choice-note label notes))))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action all str pred)))))

(defun aob--read-choices (prompt labels multi &optional notes)
  "Read an answer to PROMPT from LABELS, as (PICKS . TYPED).
Picking aob-other-choice reads TYPED, and so does typing past LABELS.
NOTES annotate LABELS, see aob--choice-table."
  (let* ((table (aob--choice-table labels notes))
         (said (if multi
                   (completing-read-multiple prompt table)
                 (list (completing-read prompt table))))
         (typed (seq-remove (lambda (x) (or (member x labels)
                                            (equal x aob-other-choice)
                                            (string-empty-p x)))
                            said)))
    (when (member aob-other-choice said)
      (let ((own (string-trim (read-string (concat prompt aob-other-choice " ")))))
        (unless (string-empty-p own)
          (setq typed (append typed (list own))))))
    (cons (seq-filter (lambda (x) (member x labels)) said)
          (string-join typed "\n"))))

(defun aob--question-content (q picks typed)
  "Q's share of an answer, as ((FIELD . VALUE)...), from PICKS and TYPED.
TYPED goes to the field Q keeps for words of its own; without one it
joins a multi-select's PICKS, or stands in for a single pick."
  (let* ((key (plist-get q :key))
         (custom (plist-get q :custom))
         (multi (plist-get q :multi))
         (own (and typed (not (string-empty-p typed)) typed))
         (value (cond ((or custom (not own)) picks)
                      (multi (append picks (list own)))
                      (t (list own)))))
    (append (when value (list (cons key (if multi value (car (last value))))))
            (when (and custom own) (list (cons custom own))))))

(defun aob--loose-answer-p (d content)
  "Non-nil when CONTENT answers a question of D outside the options it
offers, and the question keeps no field for words of its own."
  (seq-some (lambda (q)
              (when-let* ((options (plist-get q :options))
                          ((not (plist-get q :custom)))
                          (given (assoc (plist-get q :key) content)))
                (seq-some (lambda (v) (not (member v options)))
                          (ensure-list (cdr given)))))
            (plist-get d :questions)))

(defun aob--answer-question (s d content)
  "Send CONTENT, the ((FIELD . VALUE)...) answer to question D, to S.
An answer the form cannot hold is declined, and every answer is said as
the next message instead, so the words still reach the agent."
  (if (not (aob--loose-answer-p d content))
      (aob--call s :resolve d content)
    (aob--call s :resolve d 'decline)
    (let ((aob-prompt-typed t))
      (aob-prompt
       s (mapconcat
          (lambda (q)
            (format "%s: %s"
                    (or (plist-get q :text) (plist-get d :title) (plist-get q :key))
                    (mapconcat (lambda (v) (format "%s" v))
                               (append (ensure-list (cdr (assoc (plist-get q :key) content)))
                                       (ensure-list (cdr (assoc (plist-get q :custom) content))))
                               ", ")))
          (seq-filter (lambda (q) (or (assoc (plist-get q :key) content)
                                      (assoc (plist-get q :custom) content)))
                      (plist-get d :questions))
          "\n")))))

(defun aob--wrap-line (line width)
  "LINE broken at spaces into lines no wider than WIDTH, where words allow."
  (let (lines current)
    (dolist (word (split-string line " +" t))
      (if (and current (> (+ (string-width current) 1 (string-width word)) width))
          (progn (push current lines) (setq current word))
        (setq current (if current (concat current " " word) word))))
    (nreverse (cons (or current "") lines))))

(defun aob--question-prompt (text &optional width)
  "TEXT whole, every line kept and wrapped to WIDTH, with the answer
read on a line of its own below it."
  (let ((width (or width
                   ;; vertico draws its count before the first line
                   (max 20 (min 72 (- (window-width (minibuffer-window)) 8))))))
    (concat (mapconcat (lambda (line) (string-join (aob--wrap-line line width) "\n"))
                       (split-string (string-trim (or text "")) "\n")
                       "\n")
            "\n> ")))

(defun aob--resolve-question (s d)
  "Walk D's questions; each with options ends in aob-other-choice."
  (let ((max-mini-window-height 1.0)
        content)
    (dolist (q (plist-get d :questions))
      (let* ((text (or (plist-get q :text) (plist-get d :title) "answer"))
             (header (plist-get q :header))
             (prompt (aob--question-prompt
                      (if (and (stringp header) (not (string-empty-p header))
                               (not (equal (string-trim header) (string-trim text))))
                          (concat header "\n" text)
                        text)))
             (labels (plist-get q :options)))
        (setq content
              (append content
                      (if labels
                          (let ((said (aob--read-choices prompt labels (plist-get q :multi)
                                                         (plist-get q :notes))))
                            (aob--question-content q (car said) (cdr said)))
                        (list (cons (plist-get q :key)
                                    (if (plist-get q :multi)
                                        (completing-read-multiple prompt nil)
                                      (read-string prompt)))))))))
    (aob--answer-question s d content)))

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

(defcustom aob-reject-asks-reason t
  "Whether refusing a permission opens the compose box for the reason.
What is written there is said to the agent the way any prompt is, into
the running turn where it takes steering and queued otherwise, so a no
becomes something it can adjust to.  Closing the box empty says nothing."
  :type 'boolean :group 'aob)

(defconst aob-reject-reason-placeholder
  "Why you refused — the agent reads this and adjusts; close empty to say nothing"
  "What the compose box asks for when it opens after a refusal.")

(defvar-local aob-compose--purpose nil
  "What this draft answers, when it is more than a prompt: reject-reason.")

(defvar aob-compose--tags)
(defvar aob-compose-placeholder)

(defun aob--rejects-p (d id)
  "Non-nil when option ID of permission D is one that refuses it."
  (member (plist-get (seq-find (lambda (o) (equal (plist-get o :optionId) id))
                               (plist-get d :options))
                     :kind)
          aob--reject-kinds))

(defun aob-ask-reject-reason (s)
  "Open S's compose box for why a call was just refused.
The draft is an ordinary prompt to S, only labelled as the reason; see
aob-reject-asks-reason.  Returns the compose buffer, nil when not asked."
  (when aob-reject-asks-reason
    (let ((buf (aob-compose s)))
      (with-current-buffer buf
        (setq aob-compose--purpose 'reject-reason
              aob-compose--tags (cons "reason for the rejection" aob-compose--tags))
        (setq-local aob-compose-placeholder aob-reject-reason-placeholder)
        (aob-compose--placeholder-refresh))
      buf)))

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

(defvar-local aob-compose--anchor nil
  "(WINDOW POS HOLD) for a draft anchored to a line, else nil.
It floats small under the line POS is on in WINDOW, and its send hands
the words and attachments to HOLD instead of sending them.")

(defcustom aob-compose-anchored-height 4
  "How many lines an anchored draft shows when it is short."
  :type 'natnum :group 'aob)

(defcustom aob-compose-anchored-max-height 16
  "How many lines an anchored draft may grow to."
  :type 'natnum :group 'aob)

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
instead; one that returns (TEXT . FILES) also hands the turn FILES to
carry; one that returns :consumed has dealt with the draft itself, so
nothing is sent, the later functions never run, and the draft closes;
any other return leaves the text as it was.")

(defun aob-compose--rewritten (text)
  "(TEXT . FILES) once every before-send function has had its say.
Nil when one of them consumed the draft."
  (let (files consumed)
    (run-hook-wrapped 'aob-compose-before-send-functions
                      (lambda (fn)
                        (let ((out (funcall fn text)))
                          (cond ((eq out :consumed) (setq consumed t))
                                ((stringp out) (setq text out))
                                ((stringp (car-safe out))
                                 (setq text (car out)
                                       files (append files (cdr out))))))
                        consumed))
    (unless consumed (cons text files))))

(defface aob-compose-title
  '((t :inherit default :weight bold))
  "The one loud thing on the box: what the draft is for.
Loud by weight alone: the ink is the text's own."
  :group 'aob)

(defface aob-compose-tag
  '((t :inherit shadow))
  "The small tags beside the title, the keys and the weight: the quiet grey."
  :group 'aob)

(defface aob-compose-rule
  '((((background dark)) :underline "#333333")
    (t :underline "#c9c3b6"))
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

(defcustom aob-compose-float-width 0.75
  "How wide the floating box is, as a share of the frame's columns."
  :type 'number :group 'aob)

(defcustom aob-compose-float-height 8
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
  (if-let* ((anchor (buffer-local-value 'aob-compose--anchor buffer))
            ((window-live-p (car anchor)))
            ((display-graphic-p))
            ((fboundp 'posframe-show)))
      (aob-compose--show-anchored buffer anchor no-focus)
    (aob-compose--show-box buffer no-focus)))

(defun aob-compose--show-anchored (buffer anchor no-focus)
  "Float BUFFER under the line ANCHOR's position is on, as wide as its window.
It leaves the trace where it is: it stands under what it is about."
  (let* ((win (car anchor))
         (lines (buffer-local-value 'aob-compose-float-height buffer))
         (frame (with-selected-window win
                  (posframe-show buffer
                                 :position (cadr anchor)
                                 :parent-window win
                                 :width (max 30 (- (window-body-width win) 4))
                                 :height lines
                                 :min-height lines
                                 :border-width 1
                                 :border-color (face-attribute 'vertical-border
                                                               :foreground nil t)
                                 :respect-header-line t
                                 :respect-mode-line t
                                 :accept-focus t))))
    (unless no-focus
      (select-frame-set-input-focus frame)
      (select-window (frame-root-window frame)))
    (with-current-buffer buffer (aob-compose--autogrow))
    (frame-root-window frame)))

(defun aob-compose--show-box (buffer no-focus)
  "Show BUFFER as the compose box, floating when it can; see aob-compose-show."
  (if (aob-compose--float-p)
      (let* ((parent (aob-compose--top-frame (selected-frame)))
             (frame (with-selected-frame parent
                      (posframe-show
                       buffer
                       :poshandler aob-compose-float-poshandler
                     :width (max 40 (round (* aob-compose-float-width
                                              (frame-width parent))))
                     :height (with-current-buffer buffer (aob-compose--wanted-height))
                     :min-height aob-compose-float-height
                     :border-width 0
                     :border-color (face-attribute 'vertical-border
                                                   :foreground nil t)
                     :respect-header-line t
                       :respect-mode-line t
                       :accept-focus t))))
        ;; the box stands over the foot of the frame, and the end of the
        ;; conversation under it is what the draft answers
        (when (fboundp 'aob-trace-uncover-all)
          (with-selected-frame parent (aob-trace-uncover-all)))
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
            (set-frame-height frame wanted))
          (when aob-compose--anchor (aob-compose--keep-inside frame)))
      (when-let* ((window (get-buffer-window (current-buffer)))
                  ((window-live-p window))
                  ((not (window-full-height-p window))))
        (let ((wanted (aob-compose--wanted-height)))
          (unless (= wanted (window-height window))
            (ignore-errors
              (window-resize window (- wanted (window-height window))))))))))

(defun aob-compose--keep-inside (frame)
  "Lift FRAME, grown under a line, so its foot stays inside its parent."
  (when-let* ((parent (frame-parent frame))
              (pos (frame-position frame))
              (over (- (+ (cdr pos) (frame-pixel-height frame))
                       (frame-pixel-height parent)))
              ((> over 0)))
    (set-frame-position frame (car pos) (max 0 (- (cdr pos) over)))))

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
             (eq frame (selected-frame))
             ;; a prompt the box asked, its modes or its preset, reads in
             ;; the frame under it: that is still working in the box
             (not (active-minibuffer-window)))
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

(defun aob-compose--project ()
  "The name of the project this draft's agent works in.
Its session's when it goes to one, else the folder the draft was opened
for — read where it actually is, so a tree reached through a link is
named once."
  (let* ((s (and (stringp aob-compose--target)
                 (aob-session-get aob-compose--target)))
         (dir (or (and s (or (aob-session-project s) (aob-session-dir s)))
                  aob-compose--dir
                  default-directory)))
    (and dir (not (file-remote-p dir))
         (file-name-nondirectory (directory-file-name (file-truename dir))))))

(defun aob-compose--header ()
  "The title row: what the draft is for, where, and the tags it carries."
  (concat " "
          (propertize (aob-compose--plain (or aob-compose--label "new agent"))
                      'face 'aob-compose-title)
          (when-let* ((project (aob-compose--project)))
            (propertize (aob-compose--plain (concat "  in " project))
                        'face 'aob-compose-tag))
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

(defun aob-compose (&optional target initial name dir anchor)
  "Compose a multi-line prompt.
TARGET is a session, (new . AGENT) to spawn AGENT on send, a function
called with the text and attachments on send, or nil for the default
agent.  INITIAL seeds the buffer — selections, refs — with point after
it, ready for your words.  NAME, when given, is what the buffer is
called: a function target has no name of its own to take one from.
ANCHOR, (WINDOW POS HOLD), makes it a small draft under a line whose
send holds rather than sends; see aob-compose--anchor.

The buffer stands where the prompt is going: DIR when the caller names
one, else a session target's own folder, else the folder of whatever
opened it.  It is set on every call, not only on creation — the buffer
for a target is reused, and a reused draft that kept yesterday's folder
completes `@file' and `/skill' against the wrong tree."
  (interactive (list (or (aob-session-at-point)
                         (and (aob-live-sessions) (aob-read-session "To: ")))))
  ;; one compose buffer per target: drafts to different agents coexist,
  ;; and ZZ always sends to the agent named in this buffer's header
  (let* ((named dir)
         (dir (or dir
                  (and (aob-session-p target)
                       (or (aob-session-dir target) (aob-session-project target)))
                  default-directory))
         ;; the new buffer inherits this; a removed worktree breaks every process it starts
         (default-directory
          (file-name-as-directory
           (expand-file-name
            (or (seq-find #'file-directory-p
                          (delq nil (list dir
                                          (and (aob-session-p target)
                                               (aob-session-project target))
                                          default-directory)))
                "~/"))))
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
      (when anchor
        (setq aob-compose--anchor anchor)
        (setq-local aob-compose-float-height aob-compose-anchored-height
                    aob-compose-float-max-height aob-compose-anchored-max-height))
      (when initial (insert initial) (goto-char (point-max))))
    (aob-compose-show buf)
    buf))

(defvar-local aob-compose--raw nil
  "Non-nil in a comment box: its words go out as typed, nothing expanded.")

(cl-defun aob-comment-box (name label save &key initial pos tags placeholder)
  "Open the box every comment is written in, and answer its buffer.
It floats under the line POS, point by default, and stands at the foot
where nothing can float.  SAVE is called with the words, trimmed and
empty when the box was cleared, once the owner saves; an error from it
keeps the box open.  NAME keys the draft, LABEL titles it and TAGS follow
the title.  INITIAL fills the box.  A box already open under NAME is
shown as it is, so typed words are never replaced."
  (let* ((origin (current-buffer))
         (line (save-excursion (goto-char (or pos (point))) (line-beginning-position)))
         (buffer-name (format "compose:%s" name)))
    (if-let* ((old (get-buffer buffer-name))
              ((buffer-live-p old)))
        (progn (save-current-buffer (aob-compose-show old)) old)
      (let ((buf (save-current-buffer
                   (aob-compose nil initial name nil
                                (list (get-buffer-window origin) line
                                      (lambda (text _files)
                                        (funcall save (string-trim text))))))))
        (with-current-buffer buf
          (setq aob-compose--raw t)
          (setq aob-compose--label label
                aob-compose--tags tags)
          (setq-local aob-compose-allow-empty t
                      aob-compose-placeholder (or placeholder "Write the comment"))
          (aob-compose--placeholder-refresh)
          (goto-char (point-max))
          (when-let* ((win (get-buffer-window buf t)))
            (set-window-point win (point-max)))
          (force-mode-line-update))
        buf))))

(defvar aob-compose-history nil
  "Sent prompts, newest first.")

(defvar-local aob-compose--attachments nil
  "Alist of (N . FILE); [[ImageN]] in the text is what keeps FILE aboard.
It outlives the mode being set again, as a draft reopened for its
target is, so a kept draft keeps its images.")
(put 'aob-compose--attachments 'permanent-local t)

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
               (hold (nth 2 aob-compose--anchor))
               (raw (buffer-substring-no-properties (point-min) (point-max)))
               ;; a held comment is not a turn: what rides a turn joins it later
               (rewritten (if hold (list raw) (aob-compose--rewritten raw)))
               (`(,text . ,atts) (if aob-compose--raw
                                     (cons (string-trim raw) nil)
                                   (aob-compose--harvest (or (car rewritten) raw))))
               (atts (append atts (cdr rewritten)))
               (tgt aob-compose--target)
               (session (and (stringp tgt) (aob-session-get tgt))))
    (when (and (string-empty-p text) (null atts)
               (not aob-compose-allow-empty))
      (user-error "aob: empty prompt"))
    (unless aob-compose--raw (add-to-history 'aob-compose-history text))
    ;; send before killing the buffer — a refused send must not eat the text
    ;; whatever the draft goes to — a session, a spawn, a caller — it is
    ;; words the owner typed, and a spawn queues its first turn right here
    (let ((aob-prompt-typed t))
      (cond ((null rewritten))
            (hold (funcall hold text atts))
            ((and session
                  (or aob-compose--steer
                      ;; a turn that takes words mid-way gets them now: a
                      ;; subagent can keep a turn open for an hour, and a
                      ;; queued message waits behind all of it
                      (and (eq (aob-session-state session) 'working)
                           (fboundp 'aob-acp--steers-p)
                           (ignore-errors (aob-acp--steers-p session)))))
             (aob-interject session text atts))
            (session (aob-prompt session text atts))
            ((stringp tgt) (user-error "aob: target session is gone"))
            ;; a function target wants the words themselves rather than a
            ;; session to send them to — a caller composing a brief, say
            ((functionp tgt) (funcall tgt text atts))
            (aob-compose-spawn-function
             (funcall aob-compose-spawn-function text (and (consp tgt) (cdr tgt))
                      atts))
            (t (user-error "aob: no session and no spawn function"))))
    ;; read after the send: a refused send leaves you in the draft, and a
    ;; spawn function is free to say where this one should land
    (let ((after (and rewritten aob-compose-after-send)))
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

(defvar aob--folders-cache (make-hash-table :test #'equal)
  "DIR to (FILES . FOLDERS): the folders, and the file list they came from.")

(defun aob--project-folders (dir)
  "DIR's folders that hold a tracked file, each ending in its slash.
Every folder on the way up from a file, so a parent is offered beside
the leaf it holds."
  (let ((files (aob--project-files dir))
        (known (gethash dir aob--folders-cache)))
    (if (and known (eq (car known) files))
        (cdr known)
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (rel files)
          (let ((d (file-name-directory rel)))
            (while (and d (not (gethash d seen)))
              (puthash d t seen)
              (setq d (file-name-directory (directory-file-name d))))))
        (cdr (puthash dir (cons files (sort (hash-table-keys seen) #'string<))
                      aob--folders-cache))))))

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

(defvar aob-capf-mention-functions nil
  "Functions of the compose directory returning more @-words, as (NAME . WHAT).
WHAT is what the popup says beside NAME: a preset, say.")

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
     ;; a /command token — at the start of the input, or after a space
     ;; anywhere in it, where a skill is named mid-sentence; a path has
     ;; no space before its slashes, so it is left to the @ branch
     ((save-excursion
        (re-search-backward "\\(?:^\\|[[:space:]]\\)\\(/\\)[^/[:space:]]*\\="
                            (max anchor (- (point) 200)) t))
      (when-let* ((cmds (aob--capf-commands)))
        (list (match-end 1) (point)
              (mapcar (lambda (c) (plist-get c :name)) cmds)
              :company-prefix-length t
              :annotation-function
              (lambda (cand)
                (when-let* ((c (seq-find (lambda (x)
                                           (equal (plist-get x :name) cand))
                                         cmds)))
                  ;; a skill's description is a paragraph; the popup
                  ;; has room for its first line
                  (concat "  " (truncate-string-to-width
                                (car (split-string (or (plist-get c :description) "")
                                                   "[.\n]" t))
                                48 nil nil "…"))))
              :exclusive 'no)))
     ;; an @file token
     ((save-excursion
        (re-search-backward "@\\([^@[:space:]]*\\)\\="
                            (max anchor (- (point) 200)) t))
      (let* ((dir (aob--capf-dir))
             (words (seq-mapcat (lambda (fn) (ignore-errors (funcall fn dir)))
                                aob-capf-mention-functions)))
        (list (1+ (match-beginning 0)) (point)
              (completion-table-merge (mapcar #'car words)
                                      (aob--project-folders dir)
                                      (aob--capf-file-table dir))
              :company-prefix-length t
              ;; a preset, a folder and a file are written the same way,
              ;; so the popup says which one a word is
              :annotation-function
              (lambda (cand)
                (cond ((cdr (assoc cand words)))
                      ((string-suffix-p "/" cand) "  folder")
                      (t "  file")))
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

(defface aob-modeline-blocked
  '((((background dark)) :foreground "#E05A5D" :weight bold)
    (t :inherit error :weight bold))
  "Agents waiting on you." :group 'aob)
(defface aob-modeline-working '((t :weight bold))
  "Agents at work." :group 'aob)
(defface aob-modeline-sub '((t :inherit shadow))
  "Delegated agents at work." :group 'aob)
(defface aob-modeline-queued '((t :inherit shadow))
  "Messages waiting for a turn." :group 'aob)

(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function ygg-aob-switch "layer-aob")

(defvar aob-modeline--map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
                (lambda () (interactive)
                  (when (fboundp 'ygg-aob-switch)
                    (call-interactively #'ygg-aob-switch))))
    map))

(defun aob--modeline-pill (icon fallback n face tip)
  (propertize
   (concat "  "
           (or (and (require 'nerd-icons nil t)
                    (ignore-errors (nerd-icons-mdicon icon :face face)))
               (propertize fallback 'face face))
           (propertize (format " %d" n) 'face face))
   'help-echo (format "%d %s — mouse-1: switch agent" n tip)
   'mouse-face 'mode-line-highlight
   'local-map aob-modeline--map))

(defun aob--modeline-refresh (&rest _)
  (let ((blocked 0) (working 0) (queued 0) (subs 0) (quietest nil))
    (dolist (s (seq-remove (lambda (s) (aob-session-ref s :hidden)) (aob-sessions)))
      (cl-incf queued (length (aob-session-ref s :queued)))
      (when-let* ((label (aob-session-quiet s))
                  ((or (null quietest)
                       (< (aob-session-ref s :progress-at) (car quietest)))))
        (setq quietest (cons (aob-session-ref s :progress-at) label)))
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
             (aob--modeline-pill "nf-md-hand_back_right_outline" "■" blocked
                                 'aob-modeline-blocked "waiting on you"))
           (when (> working 0)
             (aob--modeline-pill "nf-md-robot_outline" "●" working
                                 'aob-modeline-working "agents working"))
           (when (> subs 0)
             (aob--modeline-pill "nf-md-source_branch" "└" subs
                                 'aob-modeline-sub "subagents working"))
           (when quietest
             (propertize (concat " " (cdr quietest)) 'face 'shadow))
           (when (> queued 0)
             (aob--modeline-pill "nf-md-tray_full" "»" queued
                                 'aob-modeline-queued "messages queued"))
           (when (> (+ blocked working subs queued) 0) "  ")))
    (force-mode-line-update t)))

(define-minor-mode aob-modeline-mode
  "Show agent attention counts (■ blocked, ● working) in the modeline."
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
    ;; a letter here shadows a motion or operator; the other verbs live
    ;; under the host's localleader
    (define-key map "i" #'aob-steer)
    (define-key map (kbd "RET") #'aob-focus)
    map)
  "Verbs shared by every buffer that renders sessions.")

(provide 'aob)
;;; aob.el ends here
