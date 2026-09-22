;;; aob-subagent.el --- a delegation you can hold, not only watch -*- lexical-binding: t; -*-

;;; Commentary:
;; aob has always known about subagents the way a log knows about them: a
;; tool call went out with a name on it, and the trace says so.  That is
;; an observation, not a handle.  Nothing can be asked how it is getting
;; on, told to stop, or answered when it finishes, because there is no
;; object — only events that mention one.
;;
;; A spawned subagent here is an ordinary aob session that remembers who
;; sent it.  Everything sessions already do — a trace of its own, a
;; worktree, permissions, resume — it does, and the parent link is one
;; ref rather than a second kind of thing to maintain.  The observed
;; delegations stay: an agent that farms work out internally still
;; reports it, and those remain read-only rows.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob)

(declare-function aob-acp-spawn "aob-acp" (agent &optional intent atts name))
(declare-function aob-trace "aob-trace" (s))
(defvar aob-acp-session-refs)
(defvar aob-acp-start-dir)

(defgroup aob-subagent nil
  "Agents sent by other agents."
  :group 'aob :prefix "aob-subagent-")

(defcustom aob-subagent-agent nil
  "Which agent a delegation uses when the caller names none.
Nil means whatever `aob-acp-default-agent' says."
  :type '(choice (const :tag "the default agent" nil) string)
  :group 'aob-subagent)

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

(defun aob-subagent-live-children (s)
  "The sessions S sent that are still going."
  (seq-remove (lambda (c) (memq (aob-session-state c) '(dead done)))
              (aob-subagent-children s)))

(defun aob-subagent-descendants (s)
  "Everything below S, however deep it was delegated."
  (let (out (queue (aob-subagent-children s)))
    (while queue
      (let ((c (pop queue)))
        (unless (memq c out)
          (push c out)
          (setq queue (append queue (aob-subagent-children c))))))
    (nreverse out)))

;;;###autoload
(defun aob-subagent-spawn (parent intent &optional agent dir model)
  "Send INTENT to a new agent on PARENT's behalf and return the session.
The session is real from the moment it is made: it has an id before it
has connected, so whoever asked can be told which one it is rather than
that one is coming."
  (require 'aob-acp)
  (let* ((parent-id (if (stringp parent) parent (aob-session-id parent)))
         (owner (aob-session-get parent-id))
         (dir (file-name-as-directory
               (expand-file-name (or dir
                                     (and owner (aob-session-dir owner))
                                     default-directory))))
         (agent (or agent aob-subagent-agent
                    (and owner (aob-session-ref owner :agent))
                    (bound-and-true-p aob-acp-default-agent)))
         (default-directory dir)
         (aob-acp-start-dir dir)
         (aob-acp-session-refs (append (list :parent-session parent-id)
                                       (and model (list :want-model model))
                                       aob-acp-session-refs))
         (s (aob-acp-spawn agent intent)))
    (when (and s model) (aob-session-put s :want-model model))
    s))

;;;###autoload
(defun aob-subagent-kill (s)
  "Stop S and everything it sent.
A delegation outliving the hand that stopped it is the failure this
exists to prevent: children go first, so none is left talking to a
model with nobody reading."
  (interactive (list (aob-target)))
  (dolist (c (aob-subagent-descendants s))
    (ignore-errors (aob--call c :kill)))
  (ignore-errors (aob--call s :kill))
  s)

(defun aob-subagent-status (s)
  "What S is doing, as a plist fit to hand back over a wire."
  (list :id (aob-session-id s)
        :name (aob-session-name s)
        :state (format "%s" (aob-session-state s))
        :dir (aob-session-dir s)
        :parent (aob-session-ref s :parent-session)
        :children (mapcar #'aob-session-id (aob-subagent-children s))
        :last (or (ignore-errors (aob-session-blurb s)) "")))

(defun aob-subagent--orphan (s _old new)
  "Stop S's children when S reaches NEW, so none is left running alone."
  (when (memq new '(dead done))
    (dolist (c (aob-subagent-live-children s))
      (ignore-errors (aob--call c :kill)))))

(add-hook 'aob-state-change-hook #'aob-subagent--orphan)

(provide 'aob-subagent)
;;; aob-subagent.el ends here
