;;; aob-subagent.el --- a delegation you can hold, not only watch -*- lexical-binding: t; -*-

;;; Commentary:
;; A subagent is the Agent or Task call an agent makes inside its own
;; turn (claude), or the thread codex names for one.  Each becomes a
;; session of its own, read-only, whose trace holds the steps it took.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'aob)

(declare-function aob-trace "aob-trace" (s))

(defun aob-subagent-parent (s)
  "The session that sent S, or nil when nobody did."
  (when-let* ((id (aob-session-ref s :parent-session)))
    (aob-session-get id)))

(defun aob-subagent-lead (s)
  "The session at the head of the chain that sent S, S itself when none did."
  (let ((lead s) (seen (list s)) p)
    (while (and (setq p (aob-subagent-parent lead)) (not (memq p seen)))
      (push p seen)
      (setq lead p))
    lead))

(defun aob-subagent-p (s)
  "Whether S was sent by another session."
  (and (aob-session-ref s :parent-session) t))

(defun aob-subagent-children (s)
  "Every session S sent, newest first, the finished ones included."
  (let ((id (aob-session-id s)))
    (seq-filter (lambda (other)
                  (equal id (aob-session-ref other :parent-session)))
                (aob-sessions))))

(defun aob-subagent-of (s)
  "What a picker says of S when another session sent it, else nil."
  (when-let* ((p (aob-subagent-parent s)))
    (format "subagent of %s" (aob-session-name p))))

(defun aob-subagent-live-count (s)
  "How many of S's subagents are still working."
  (seq-count (lambda (c) (memq (aob-session-state c) '(working blocked)))
             (aob-subagent-children s)))

(defconst aob-subagent-brief-headings
  '("GOAL" "SCOPE" "CONTEXT" "ACCEPTANCE" "VERIFY" "REPORT")
  "The headings a worker's brief opens its parts with, each at a line start.")

(defun aob-subagent-brief-missing (text)
  "The brief headings TEXT opens no line with, in brief order."
  (let ((case-fold-search nil))
    (seq-remove (lambda (h)
                  (string-match-p (concat "^[ \t]*\\(?:#+[ \t]*\\)?" h "\\b")
                                  text))
                aob-subagent-brief-headings)))

(defun aob-subagent--check-cap (s)
  "Say in S's trace when its working subagents outnumber its :subagent-cap.
The agent made the call before Emacs heard of it and nothing here can
refuse it: the warning in the trace is all Emacs can do."
  (when-let* ((cap (aob-session-ref s :subagent-cap))
              (live (aob-subagent-live-count s))
              ((> live cap)))
    (aob-event s 'state :title (format "subagent cap passed: %d working, cap %d"
                                       live cap))))

(defun aob-subagent--check-brief (kid text)
  "Say in the trace of KID's sender which brief headings TEXT lacks.
Only a sender whose preset orchestrates, :subagent-briefs, asks for one."
  (when-let* ((p (aob-subagent-parent kid))
              ((aob-session-ref p :subagent-briefs))
              (missing (aob-subagent-brief-missing text)))
    (aob-event p 'state :title (format "brief for %s lacks %s"
                                       (aob-session-name kid)
                                       (string-join missing " ")))))

(defun aob-subagent-native-p (s)
  "Whether S is a subagent its agent runs inside its own turn."
  (and (aob-session-ref s :native-tool-id) t))

(defun aob-subagent--native-kids (root)
  "ROOT's table of subagent call id to the session tracing it."
  (or (aob-session-ref root :native-kids)
      (let ((h (make-hash-table :test #'equal)))
        (aob-session-put root :native-kids h)
        h)))

(defun aob-subagent--native-name (ev)
  (aob--first-line (or (plist-get ev :title) "subagent") 60))

(defun aob-subagent--native-state (ev &optional sending)
  "The state the subagent call EV puts its session in.
SENDING is non-nil while the turn that made the call is still going."
  (let ((status (plist-get ev :status))
        (raw (plist-get ev :raw)))
    (cond ((equal status "failed") 'failed)
          ((or (member status '(nil "pending" "in_progress"))
               (> (or (plist-get ev :child-live) 0) 0)
               ;; codex's spawn completes once the thread exists
               (aob-subagent--codex-running-p raw)
               ;; a background call completes when it launches, not when it ends
               (and sending (listp raw) (eq (plist-get raw :run_in_background) t)))
           'working)
          (t 'done))))

(defun aob-subagent--native-open (root owner ev)
  "A session for the subagent call EV, made by OWNER on ROOT's stream."
  (let* ((tid (plist-get ev :tool-id))
         (kid (aob-create-session
               :id (format "%s/%s" (aob-session-id owner) tid)
               :backend 'native-subagent
               :name (aob-subagent--native-name ev)
               :project (aob-session-project owner)
               :dir (aob-session-dir owner)
               :state 'working
               :refs (list :parent-session (aob-session-id owner)
                           :native-root (aob-session-id root)
                           :native-tool-id tid
                           :agent (aob-session-ref root :agent)))))
    (puthash tid (aob-session-id kid) (aob-subagent--native-kids root))
    (aob-turn-begin kid)
    (aob-subagent--check-cap owner)
    kid))

(defun aob-subagent--native-prompt (kid ev)
  "Give KID the prompt EV sent it, once, as the first thing in its trace."
  (when-let* (((not (aob-session-ref kid :prompted)))
              (raw (plist-get ev :raw))
              ((listp raw))
              (text (plist-get raw :prompt))
              ((stringp text)))
    (aob-session-put kid :prompted t)
    (let ((p (aob-event kid 'prompt)))
      (aob-event-push-text p text)
      (setf (aob-session-events kid)
            (append (delq p (aob-session-events kid)) (list p))))
    (aob-subagent--check-brief kid text)))

(defun aob-subagent--codex-states (raw)
  "The (THREAD . STATUS) pairs codex's collab call RAW reports."
  (when-let* (((listp raw))
              (states (plist-get raw :agentsStates))
              ((listp states)))
    (cl-loop for (key state) on states by #'cddr
             when (keywordp key)
             collect (cons (substring (symbol-name key) 1) (plist-get state :status)))))

(defun aob-subagent--codex-running-p (raw)
  (and (seq-find (lambda (pair) (member (cdr pair) '("pendingInit" "running")))
                 (aob-subagent--codex-states raw))
       t))

(defun aob-subagent--codex-settle (s ev)
  "End S's codex subagents whose threads the collab call EV reports finished."
  (dolist (pair (aob-subagent--codex-states (plist-get ev :raw)))
    (when-let* ((state (pcase (cdr pair)
                         ((or "completed" "shutdown") 'done)
                         ((or "errored" "interrupted" "notFound") 'failed)))
                (kid (seq-find (lambda (c) (equal (aob-session-ref c :codex-thread) (car pair)))
                               (aob-subagent-children s)))
                ((eq (aob-session-state kid) 'working)))
      (aob-turn-end kid)
      (aob-set-state kid state))))

(defun aob-subagent--native-sync (kid ev)
  "Bring KID's name, prompt and state level with its call EV."
  (let ((name (aob-subagent--native-name ev)))
    (unless (equal name (aob-session-name kid))
      (aob-rename-session kid name)))
  (aob-subagent--native-prompt kid ev)
  (when-let* ((raw (plist-get ev :raw))
              ((listp raw))
              (kind (or (plist-get ev :subagent-type) (plist-get raw :subagent_type)))
              ((stringp kind)))
    (aob-session-put kid :subagent-type kind))
  (when-let* ((raw (plist-get ev :raw))
              ((listp raw))
              (thread (car (plist-get raw :receiverThreadIds))))
    (aob-session-put kid :codex-thread thread))
  (let ((new (aob-subagent--native-state
              ev (when-let* ((p (aob-subagent-parent kid)))
                   (memq (aob-session-state p) '(working blocked))))))
    (unless (eq new (aob-session-state kid))
      (if (eq new 'working) (aob-turn-begin kid) (aob-turn-end kid))
      (aob-set-state kid new)))
  (aob--dirty kid))

(defun aob-subagent--native-take (kid ev)
  "Put the step EV into KID's own events, once."
  (unless (plist-get ev :native-routed)
    (plist-put ev :native-routed t)
    (push ev (aob-session-events kid))
    (when (> (cl-incf (aob-session-nevents kid)) aob-event-cap)
      (let ((keep (/ aob-event-cap 2)))
        (setf (aob-session-events kid) (seq-take (aob-session-events kid) keep)
              (aob-session-nevents kid) keep)))))

(defun aob-subagent--plan-items (ev)
  "The items of the plan EV states, or t when EV states none.
A plan update lists entries; a TodoWrite call lists todos in its input."
  (pcase (plist-get ev :type)
    ('plan (plist-get ev :entries))
    ('tool (let ((todos (and (listp (plist-get ev :raw))
                             (plist-get (plist-get ev :raw) :todos))))
             (if (and (consp todos)
                      (seq-every-p (lambda (e) (and (listp e) (plist-get e :status))) todos))
                 todos
               t)))
    (_ t)))

(defvar aob-subagent-progress-functions nil
  "Called with a subagent whose :plan-progress just changed.")

(defun aob-subagent--plan-note (kid ev)
  "Keep KID's :plan-progress, (DONE . TOTAL), level with the plan EV states."
  (let ((items (aob-subagent--plan-items ev)))
    (unless (eq items t)
      (aob-session-put kid :plan-progress
                       (and items
                            (cons (seq-count (lambda (e) (equal (plist-get e :status) "completed"))
                                             items)
                                  (length items))))
      (aob--dirty kid)
      (run-hook-with-args 'aob-subagent-progress-functions kid))))

(defun aob-subagent--own-plan-note (s ev)
  "A subagent's own step EV, made on S itself, as a workflow's are."
  (when (and (aob-subagent-p s) (not (plist-get ev :parent)))
    (aob-subagent--plan-note s ev)))

(add-hook 'aob-event-change-functions #'aob-subagent--own-plan-note)

(defun aob-subagent--native-note (s ev)
  "Keep the subagent EV belongs to, or is, in step with EV of stream S."
  (unless (aob-subagent-native-p s)
    (let* ((kids (aob-session-ref s :native-kids))
           (pid (plist-get ev :parent))
           (owner (or (and kids pid (aob-session-get (gethash pid kids))) s)))
      (unless (eq owner s)
        (aob-subagent--native-take owner ev)
        (aob-subagent--plan-note owner ev)
        (aob--dirty owner))
      (when (and (eq (plist-get ev :type) 'tool) (plist-get ev :subagent))
        (aob-subagent--native-sync
         (or (aob-session-native-child s ev)
             (aob-subagent--native-open s owner ev))
         ev))
      (when (and (eq (plist-get ev :type) 'tool) (not (plist-get ev :subagent)))
        (aob-subagent--codex-settle s ev)))))

(add-hook 'aob-event-change-functions #'aob-subagent--native-note)

(defun aob-subagent--native-settle (s _old new)
  "Settle S's running subagents when S's turn ends: no update will close them.
Gone, they fail with it; idle, they are done, since the turn ending is
the last the adapter says of a subagent sent in the background."
  (when (memq new '(dead failed idle done))
    (dolist (c (aob-subagent-children s))
      (when (and (aob-subagent-native-p c) (eq (aob-session-state c) 'working)
                 ;; a codex thread outlives the turn that spawned it; its own report settles it
                 (or (memq new '(dead failed)) (not (aob-session-ref c :codex-thread))))
        (aob-turn-end c)
        (aob-set-state c (if (memq new '(dead failed)) 'failed 'done))))))

(add-hook 'aob-state-change-hook #'aob-subagent--native-settle)

(defun aob-subagent--native-drop (s)
  "Take S's subagents out of the registry with it."
  (dolist (c (aob-subagent-children s))
    (when (aob-subagent-native-p c)
      (aob-remove-session c))))

(add-hook 'aob-session-removed-hook #'aob-subagent--native-drop)

(defun aob-subagent--native-refuse (s &rest _)
  (user-error "aob: %s is a subagent its agent runs and takes no messages; talk to %s"
              (aob-session-name s)
              (if-let* ((p (aob-subagent-parent s)))
                  (aob-session-name p)
                "the agent that sent it")))

(defun aob-subagent--native-forget (s &rest _)
  (when-let* ((root (aob-session-get (aob-session-ref s :native-root)))
              (kids (aob-session-ref root :native-kids)))
    (remhash (aob-session-ref s :native-tool-id) kids))
  (aob-remove-session s))

(defun aob-subagent--native-focus (s &rest _)
  (aob-trace s))

(aob-register-backend
 'native-subagent
 (list :prompt #'aob-subagent--native-refuse
       :interject #'aob-subagent--native-refuse
       :cancel #'aob-subagent--native-refuse
       :flush #'ignore
       :kill #'aob-subagent--native-forget
       :focus #'aob-subagent--native-focus))

;;; Workflow agents: the ones a Workflow script starts, read from its journal

(defcustom aob-subagent-workflow-poll-secs 2
  "Seconds between reads of a followed workflow's journal."
  :type 'number :group 'aob)

(defcustom aob-subagent-workflow-settle-secs 10
  "Seconds a workflow whose agents have all answered is still read.
A script that awaits one agent before starting the next leaves every
started agent answered between the two."
  :type 'number :group 'aob)

(defcustom aob-subagent-workflow-idle-secs 1800
  "Seconds without a new journal line after which a workflow is let go."
  :type 'number :group 'aob)

(defvar aob-acp-tool-detail-keys)

(cl-defstruct (aob-subagent--wf (:constructor aob-subagent--wf-make)
                                (:copier nil))
  lead dir (offset 0) tool-ids heard timer
  (agents (make-hash-table :test #'equal))
  (steps (make-hash-table :test #'equal)))

(defvar aob-subagent--workflows (make-hash-table :test #'equal)
  "Each workflow run dir followed, to what follows it.")

(defun aob-subagent--workflow-dir (ev)
  "The local run dir the Workflow call EV names, or nil."
  (when-let* (((eq (plist-get ev :type) 'tool))
              ((equal (plist-get ev :title) "Workflow"))
              (text (mapconcat
                     (lambda (c)
                       (let ((inner (and (listp c) (plist-get c :content))))
                         (or (and (listp inner) (stringp (plist-get inner :text))
                                  (plist-get inner :text))
                             "")))
                     (plist-get ev :content) "\n"))
              (text (if (stringp (plist-get ev :rawOutput))
                        (concat text "\n" (plist-get ev :rawOutput))
                      text))
              ((string-match "^Transcript dir: \\(.+\\)$" text))
              (dir (string-trim (match-string 1 text)))
              ((file-name-absolute-p dir))
              ((not (file-remote-p dir))))
    (file-name-as-directory dir)))

(defun aob-subagent--workflow-note (s ev)
  "Follow the run a Workflow call EV of S names, once per call."
  (when-let* ((dir (aob-subagent--workflow-dir ev)))
    (let ((wf (or (gethash dir aob-subagent--workflows)
                  (puthash dir (aob-subagent--wf-make :lead (aob-session-id s) :dir dir)
                           aob-subagent--workflows)))
          (tid (plist-get ev :tool-id)))
      (unless (member tid (aob-subagent--wf-tool-ids wf))
        (push tid (aob-subagent--wf-tool-ids wf))
        (unless (timerp (aob-subagent--wf-timer wf))
          (setf (aob-subagent--wf-heard wf) (float-time)
                (aob-subagent--wf-timer wf)
                (run-with-timer aob-subagent-workflow-poll-secs
                                aob-subagent-workflow-poll-secs
                                #'aob-subagent--workflow-tick wf)))))))

(add-hook 'aob-event-change-functions #'aob-subagent--workflow-note)

(defun aob-subagent--workflow-read (file offset)
  "The whole lines FILE holds past byte OFFSET, and the offset after them."
  (when-let* ((size (file-attribute-size (file-attributes file)))
              ((> size offset)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil offset size)
      (goto-char (point-max))
      (when (search-backward "\n" nil t)
        (cons (mapcar (lambda (l) (decode-coding-string l 'utf-8))
                      (split-string (buffer-substring-no-properties 1 (point)) "\n" t))
              (+ offset (point)))))))

(defun aob-subagent--workflow-parse (line)
  (condition-case nil
      (json-parse-string line :object-type 'plist :array-type 'list
                         :null-object nil :false-object nil)
    (error nil)))

(defun aob-subagent--workflow-step-title (name input)
  (let ((detail (seq-some (lambda (k) (let ((v (plist-get input k))) (and (stringp v) v)))
                          (bound-and-true-p aob-acp-tool-detail-keys))))
    (if detail (format "%s %s" name (aob--first-line detail 80)) name)))

(defun aob-subagent--workflow-step (kid o)
  "Put the transcript line O into KID's trace when it is a prompt or a tool call."
  (let ((content (plist-get (plist-get o :message) :content)))
    (pcase (plist-get o :type)
      ("user"
       (when (and (stringp content) (not (aob-session-ref kid :prompted)))
         (aob-session-put kid :prompted t)
         (aob-event-push-text (aob-event kid 'prompt) content)))
      ("assistant"
       (dolist (c (and (listp content) content))
         (when (equal (plist-get c :type) "tool_use")
           (aob-event kid 'tool :tool-id (plist-get c :id) :kind "other"
                      :title (aob-subagent--workflow-step-title
                              (or (plist-get c :name) "tool") (plist-get c :input))
                      :raw (plist-get c :input) :status "completed")))))))

(defun aob-subagent--workflow-steps (wf id kid)
  "Bring KID's trace level with the transcript of workflow agent ID.
Non-nil when the transcript had new lines."
  (when-let* ((off (gethash id (aob-subagent--wf-steps wf)))
              (got (aob-subagent--workflow-read
                    (expand-file-name (format "agent-%s.jsonl" id) (aob-subagent--wf-dir wf))
                    off)))
    (puthash id (cdr got) (aob-subagent--wf-steps wf))
    (dolist (line (car got))
      (when-let* ((o (aob-subagent--workflow-parse line)))
        (aob-subagent--workflow-step kid o)))
    (aob--dirty kid)
    t))

(defun aob-subagent--workflow-open (wf lead id o)
  "A session for the workflow agent ID the journal line O started for LEAD."
  (let* ((label (plist-get o :label))
         (phase (and (stringp (plist-get o :phase)) (plist-get o :phase)))
         (kid (aob-create-session
               :id (format "%s/%s" (aob-session-id lead) id)
               :backend 'workflow-subagent
               :name (if (and (stringp label) (not (string-empty-p label)))
                         label
                       (string-join (delq nil (list phase id)) " "))
               :project (aob-session-project lead)
               :dir (aob-session-dir lead)
               :state 'working
               :refs (list :parent-session (aob-session-id lead)
                           :agent (aob-session-ref lead :agent)
                           :subagent-type (if phase (format "workflow · %s" phase) "workflow")
                           :workflow-dir (aob-subagent--wf-dir wf)
                           :workflow-agent id))))
    (puthash id (aob-session-id kid) (aob-subagent--wf-agents wf))
    (puthash id 0 (aob-subagent--wf-steps wf))
    (aob-turn-begin kid)
    (aob-subagent--check-cap lead)
    kid))

(defun aob-subagent--workflow-kid (wf lead id o)
  "The session of workflow agent ID, opened when the journal first names it.
Nil once the session has been killed."
  (let ((sid (gethash id (aob-subagent--wf-agents wf) 'none)))
    (if (eq sid 'none)
        (aob-subagent--workflow-open wf lead id o)
      (aob-session-get sid))))

(defun aob-subagent--workflow-settle (wf lead id o state)
  "End workflow agent ID in STATE, its answer from line O last in its trace."
  (when-let* ((kid (aob-subagent--workflow-kid wf lead id o))
              ((memq (aob-session-state kid) '(working blocked))))
    (aob-subagent--workflow-steps wf id kid)
    (puthash id nil (aob-subagent--wf-steps wf))
    (when-let* ((text (seq-find #'stringp (list (plist-get o :result) (plist-get o :error)))))
      (aob-event-push-text (aob-event kid 'message) text))
    (aob-turn-end kid)
    (aob-set-state kid state)
    (aob--dirty kid)))

(defun aob-subagent--workflow-line (wf lead o)
  "Act on the journal line O of WF, sent by LEAD."
  (when-let* ((id (plist-get o :agentId))
              ((stringp id)))
    (let ((type (plist-get o :type)))
      (cond ((or (plist-get o :error) (plist-get o :failed)
                 (member type '("failed" "failure" "error")))
             (aob-subagent--workflow-settle wf lead id o 'failed))
            ((equal type "started") (aob-subagent--workflow-kid wf lead id o))
            ((equal type "result") (aob-subagent--workflow-settle wf lead id o 'done))))))

(defun aob-subagent--workflow-working (wf)
  "WF's agent sessions still working."
  (let (live)
    (maphash (lambda (_id sid)
               (when-let* ((kid (aob-session-get sid))
                           ((memq (aob-session-state kid) '(working blocked))))
                 (push kid live)))
             (aob-subagent--wf-agents wf))
    live))

(defun aob-subagent--workflow-stop (wf)
  (when (timerp (aob-subagent--wf-timer wf))
    (cancel-timer (aob-subagent--wf-timer wf)))
  (setf (aob-subagent--wf-timer wf) nil))

(defun aob-subagent--workflow-poll (wf)
  "Read what WF's journal gained since the last poll; stop when it is over."
  (let ((lead (aob-session-get (aob-subagent--wf-lead wf)))
        (now (float-time)))
    (if (not lead)
        (aob-subagent--workflow-stop wf)
      (when-let* ((got (aob-subagent--workflow-read
                        (expand-file-name "journal.jsonl" (aob-subagent--wf-dir wf))
                        (aob-subagent--wf-offset wf))))
        (setf (aob-subagent--wf-offset wf) (cdr got)
              (aob-subagent--wf-heard wf) now)
        (dolist (line (car got))
          (when-let* ((o (aob-subagent--workflow-parse line)))
            (aob-subagent--workflow-line wf lead o))))
      (let ((working (aob-subagent--workflow-working wf))
            (quiet (- now (or (aob-subagent--wf-heard wf) now))))
        (dolist (kid working)
          (when (aob-subagent--workflow-steps wf (aob-session-ref kid :workflow-agent) kid)
            (setf (aob-subagent--wf-heard wf) now quiet 0)))
        (cond ((and (null working)
                    (> (hash-table-count (aob-subagent--wf-agents wf)) 0)
                    (>= quiet aob-subagent-workflow-settle-secs))
               (aob-subagent--workflow-stop wf))
              ((>= quiet aob-subagent-workflow-idle-secs)
               (dolist (kid working)
                 (aob-event kid 'state :title "workflow went quiet")
                 (aob-turn-end kid)
                 (aob-set-state kid 'failed))
               (aob-subagent--workflow-stop wf)))))))

(defun aob-subagent--workflow-tick (wf)
  (condition-case nil
      (aob-subagent--workflow-poll wf)
    (error (aob-subagent--workflow-stop wf))))

(defun aob-subagent--workflow-drop (s)
  "Let go of the workflows S sent, and take their agents out with it."
  (let ((id (aob-session-id s)))
    (maphash (lambda (dir wf)
               (when (equal id (aob-subagent--wf-lead wf))
                 (aob-subagent--workflow-stop wf)
                 (remhash dir aob-subagent--workflows)))
             aob-subagent--workflows)
    (dolist (c (aob-subagent-children s))
      (when (aob-session-ref c :workflow-agent)
        (aob-remove-session c)))))

(add-hook 'aob-session-removed-hook #'aob-subagent--workflow-drop)

(defun aob-subagent--workflow-forget (s &rest _)
  (aob-remove-session s))

(aob-register-backend
 'workflow-subagent
 (list :prompt #'aob-subagent--native-refuse
       :interject #'aob-subagent--native-refuse
       :cancel #'aob-subagent--native-refuse
       :flush #'ignore
       :kill #'aob-subagent--workflow-forget
       :focus #'aob-subagent--native-focus))

(provide 'aob-subagent)
;;; aob-subagent.el ends here
