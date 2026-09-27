;;; aob-handoff.el --- a fresh session that starts where another left off -*- lexical-binding: t; -*-

;;; Commentary:
;; Instead of compacting a long conversation, ask a hidden fork of it to
;; write the prompt a new agent would need, then open a fresh session of
;; the same agent in the same place with that prompt waiting in compose.
;; Nothing is sent until the owner sends it; the source is left as it was.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'aob)
(require 'aob-acp)
(require 'aob-btw)

(defgroup aob-handoff nil
  "Hand the work of one session to a fresh one."
  :group 'aob :prefix "aob-handoff-")

(defcustom aob-handoff-instructions
  "Write a prompt for a new agent that takes over this work.  It has no memory of this conversation: the prompt must stand on its own.
Cover, briefly and concretely:
- the goal
- the constraints and the owner's preferences
- decisions made, and why, including approaches ruled out
- the current state: what is done, what is in progress, and the git state (branch, uncommitted changes)
- the exact paths of the relevant files
- the next step
End with the owner's next task, stated below.
Answer from what you already know; change no files.
Answer with only the prompt text: no preamble, no closing remarks."
  "What a hidden fork of the session is asked, ahead of the next task."
  :type 'string :group 'aob-handoff)

(defcustom aob-handoff-continue-task "Continue the current work."
  "The next task when none is written."
  :type 'string :group 'aob-handoff)

(defun aob-handoff--task (task)
  "TASK as written, or the task of carrying on when nothing was."
  (let ((task (string-trim (or task ""))))
    (if (string-empty-p task) aob-handoff-continue-task task)))

(defun aob-handoff--question (task)
  "The fork's question: the instructions, then TASK."
  (format "%s\n\nNext task: %s" aob-handoff-instructions (aob-handoff--task task)))

(defun aob-handoff--presets (preset)
  "The presets, with PRESET kept out of a fresh worktree.
The handoff works where the source did, uncommitted edits and all."
  (let ((spec (cdr (assoc preset aob-acp-presets))))
    (if (plist-get spec :worktree)
        (cons (cons preset (cl-loop for (k v) on spec by #'cddr
                                    unless (eq k :worktree) append (list k v)))
              aob-acp-presets)
      aob-acp-presets)))

(defun aob-handoff--spawn (source text)
  "Open a fresh session of SOURCE's agent where SOURCE works, TEXT in compose."
  (let* ((preset (or (aob-session-ref source :preset)
                     (aob-session-ref source :agent)))
         (dir (or (aob-session-dir source) (aob-session-project source)
                  default-directory))
         (default-directory (file-name-as-directory (expand-file-name dir)))
         (aob-acp-start-dir (or (aob-session-project source) dir))
         (aob-acp-presets (aob-handoff--presets preset))
         (aob-acp-session-refs
          (append (when-let* ((task (aob-session-ref source :task)))
                    (list :task task))
                  (when-let* ((dirs (aob-session-ref source :extra-dirs)))
                    (list :extra-dirs dirs))
                  (aob-session-ref source :limits)
                  aob-acp-session-refs))
         (new (aob-acp-spawn preset nil nil nil dir)))
    (when new
      (aob-session-put new :handoff-from (aob-session-name source))
      (aob-compose new text))
    new))

(defun aob-handoff-ask (source task)
  "Ask a hidden fork of SOURCE for a handoff prompt for TASK, then open it.
Returns the fork, or nil when none opened."
  (aob-btw-ask source (aob-handoff--question task)
               (lambda (text) (aob-handoff--spawn source text))
               (format "handoff: %s" (aob-handoff--task task))))

;;;###autoload
(defun aob-handoff (source)
  "Write the next task for a fresh session that takes over from SOURCE.
Nothing written means carry on with the current work."
  (interactive (list (aob-target)))
  (let ((buf (aob-compose (lambda (text _atts) (aob-handoff-ask source text))
                          nil (format "handoff:%s" (aob-session-name source)))))
    (with-current-buffer buf
      (setq-local aob-compose-allow-empty t))
    buf))

(provide 'aob-handoff)
;;; aob-handoff.el ends here
