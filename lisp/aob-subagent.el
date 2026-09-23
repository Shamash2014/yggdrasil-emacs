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

(defun aob-subagent-p (s)
  "Whether S was sent by another session."
  (and (aob-session-ref s :parent-session) t))

(defun aob-subagent-children (s)
  "Every session S sent, newest first, the finished ones included."
  (let ((id (aob-session-id s)))
    (seq-filter (lambda (other)
                  (equal id (aob-session-ref other :parent-session)))
                (aob-sessions))))

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

(defun aob-subagent--native-state (ev)
  "The state the subagent call EV puts its session in."
  (let ((status (plist-get ev :status)))
    (cond ((equal status "failed") 'failed)
          ((or (member status '(nil "pending" "in_progress"))
               (> (or (plist-get ev :child-live) 0) 0))
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
            (append (delq p (aob-session-events kid)) (list p))))))

(defun aob-subagent--native-sync (kid ev)
  "Bring KID's name, prompt and state level with its call EV."
  (let ((name (aob-subagent--native-name ev)))
    (unless (equal name (aob-session-name kid))
      (aob-rename-session kid name)))
  (aob-subagent--native-prompt kid ev)
  (let ((new (aob-subagent--native-state ev)))
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

(defun aob-subagent--native-note (s ev)
  "Keep the subagent EV belongs to, or is, in step with EV of stream S."
  (unless (aob-subagent-native-p s)
    (let* ((kids (aob-session-ref s :native-kids))
           (pid (plist-get ev :parent))
           (owner (or (and kids pid (aob-session-get (gethash pid kids))) s)))
      (unless (eq owner s)
        (aob-subagent--native-take owner ev)
        (aob--dirty owner))
      (when (and (eq (plist-get ev :type) 'tool) (plist-get ev :subagent))
        (aob-subagent--native-sync
         (or (aob-session-native-child s ev)
             (aob-subagent--native-open s owner ev))
         ev)))))

(add-hook 'aob-event-change-functions #'aob-subagent--native-note)

(defun aob-subagent--native-settle (s _old new)
  "Fail S's running subagents when S is gone: no update will close them."
  (when (memq new '(dead failed))
    (dolist (c (aob-subagent-children s))
      (when (and (aob-subagent-native-p c) (eq (aob-session-state c) 'working))
        (aob-turn-end c)
        (aob-set-state c 'failed)))))

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

(provide 'aob-subagent)
;;; aob-subagent.el ends here
