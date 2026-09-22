;;; aob-workflow.el --- recorded per-repo agent workflows -*- lexical-binding: t; -*-

;; A workflow is a markdown file in the repo (.aob/workflows/NAME.md):
;; a list of steps, readable by humans, editable in place, versioned
;; with the code.  `- ` items are steps; `## ` headings group items into
;; stages — a stage's items fan out to parallel agents and the next
;; stage waits for all of them (a barrier graph).  Without headings the
;; items simply run in order on one session.  Recording distills a
;; session's prompt turns into such a file; `aob-workflow-brief' skips
;; the machine entirely and hands the whole file to one agent to run.

;;; Code:

(require 'aob)
(require 'aob-acp)

(defvar aob-workflow-directory ".aob/workflows"
  "Repo-relative directory holding recorded workflows.")

(defun aob-workflow--project ()
  (expand-file-name
   (or (locate-dominating-file default-directory ".git") default-directory)))

(defun aob-workflow--file (project name)
  (expand-file-name (concat name ".md")
                    (expand-file-name aob-workflow-directory project)))

(defun aob-workflow-names (project)
  "Workflow names recorded in PROJECT."
  (let ((dir (expand-file-name aob-workflow-directory project)))
    (when (file-directory-p dir)
      (mapcar #'file-name-sans-extension
              (directory-files dir nil "\\.md\\'")))))

(defun aob-workflow--parse (text)
  "Workflow markdown TEXT → (:agent A :stages ((STEP...) ...)).
`- ' items are steps, two-space-indented lines continue them; `## '
headings make the following items one parallel stage; before any
heading each item is its own sequential stage.  Prose is description
and is ignored."
  (let (agent stages stage step headed)
    (cl-flet* ((end-step ()
                 (when step
                   (push (string-join (nreverse step) "\n") stage)
                   (setq step nil)))
               (end-stage ()
                 (end-step)
                 (when stage
                   (push (nreverse stage) stages)
                   (setq stage nil))))
      (dolist (line (split-string text "\n"))
        (cond
         ((and (not step) (not agent)
               (string-match "\\`agent:[ \t]*\\(.+?\\)[ \t]*\\'" line))
          (setq agent (match-string 1 line)))
         ((string-prefix-p "## " line)
          (end-stage)
          (setq headed t))
         ((string-match "\\`- \\(.*\\)\\'" line)
          (if headed (end-step) (end-stage))
          (push (match-string 1 line) step))
         ((and step (string-match "\\`  \\(.*\\)\\'" line))
          (push (match-string 1 line) step))
         (t (end-step))))
      (end-stage)
      (list :agent agent :stages (nreverse stages)))))

(defun aob-workflow-read (project name)
  (let ((file (aob-workflow--file project name)))
    (unless (file-readable-p file)
      (user-error "aob: no workflow %s in %s" name project))
    (aob-workflow--parse
     (with-temp-buffer (insert-file-contents file) (buffer-string)))))

;;; Recording — distill a session's ring into steps

;;;###autoload
(defun aob-workflow-record (s name)
  "Distill S's prompt turns into workflow NAME in its repo."
  (interactive (list (aob-target) (read-string "Record workflow as: ")))
  (let ((steps (let (acc)
                 (dolist (ev (aob-session-events s))
                   (when (eq (plist-get ev :type) 'prompt)
                     (push (aob-event-text ev) acc)))
                 acc))
        (file (aob-workflow--file (aob-session-project s) name)))
    (unless steps
      (user-error "aob: %s has no prompt turns to record" (aob-session-name s)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "# " name "\n")
      (when-let* ((agent (aob-session-ref s :agent)))
        (insert "agent: " agent "\n"))
      (insert "\n")
      (dolist (step steps)
        (let ((lines (split-string step "\n")))
          (insert "- " (car lines) "\n")
          (dolist (l (cdr lines)) (insert "  " l "\n")))))
    (message "aob: recorded %d steps → %s" (length steps)
             (file-relative-name file (aob-session-project s)))
    file))

;;; Graph exec — a state machine riding `aob-state-change-hook'.  The
;;; coordinator session runs single-step stages as its own turns; a
;;; multi-step stage fans out one worker session per step and the
;;; barrier releases the next stage when the last worker settles.

(defun aob-workflow--halt (s why)
  (aob-event s 'state :title (format "workflow %s %s"
                                     (aob-session-ref s :wf-name) why))
  (aob-session-put s :wf-name nil)
  (aob-session-put s :wf-stages nil)
  (aob-session-put s :wf-waiting nil))

(defun aob-workflow--next-stage (s)
  (let ((stages (aob-session-ref s :wf-stages)))
    (if (null stages)
        (progn
          (aob-event s 'state :title (format "workflow %s done"
                                             (aob-session-ref s :wf-name)))
          (aob-session-put s :wf-name nil))
      (let ((stage (car stages)))
        (aob-session-put s :wf-stages (cdr stages))
        (if (null (cdr stage))
            (aob-prompt s (car stage))
          (aob-session-put s :wf-waiting (length stage))
          (aob-session-put s :wf-fails nil)
          ;; the hook runs with an arbitrary current buffer — workers
          ;; must spawn in the coordinator's project, not wherever
          ;; point happens to be, and not wherever a host's start-dir
          ;; function thinks the owner is looking
          (let* ((default-directory (aob-session-project s))
                 (aob-acp-start-dir default-directory))
            (dolist (step stage)
              (aob-session-put (aob-acp-spawn (aob-session-ref s :wf-agent)
                                              step)
                               :wf-boss (aob-session-id s)))))))))

(defun aob-workflow--barrier (boss failed)
  (when-let* ((n (aob-session-ref boss :wf-waiting)))
    (aob-session-put boss :wf-waiting (1- n))
    (when failed
      (aob-session-put boss :wf-fails
                       (1+ (or (aob-session-ref boss :wf-fails) 0))))
    (when (<= n 1)
      (aob-session-put boss :wf-waiting nil)
      (if (aob-session-ref boss :wf-fails)
          (aob-workflow--halt boss "halted (worker failed)")
        (aob-workflow--next-stage boss)))))

(defun aob-workflow--settled-p (s old new)
  "A turn truly ended: idle reached with nothing queued.  The opened
handshake blips idle before its queue flushes, and human interjections
queue through — neither is a settled turn."
  (and (eq new 'idle)
       (memq old '(working starting))
       (null (aob-session-ref s :queued))))

(defun aob-workflow--advance (s old new)
  ;; a worker reports to its barrier exactly once, however it ends
  (when-let* ((boss-id (aob-session-ref s :wf-boss)))
    (when (or (memq new '(dead failed))
              (aob-workflow--settled-p s old new))
      (aob-session-put s :wf-boss nil)
      (when-let* ((boss (aob-session-get boss-id)))
        (aob-workflow--barrier boss (or (aob-session-ref s :turn-error)
                                        (memq new '(dead failed)))))))
  (when (aob-session-ref s :wf-name)
    (cond
     ((memq new '(dead failed))
      (aob-session-put s :wf-name nil)
      (aob-session-put s :wf-stages nil)
      (aob-session-put s :wf-waiting nil))
     ((and (aob-workflow--settled-p s old new)
           ;; mid-barrier the coordinator is parked: a human turn on it
           ;; must not release the next stage early
           (null (aob-session-ref s :wf-waiting)))
      (if (aob-session-ref s :turn-error)
          (aob-workflow--halt s "halted")
        (aob-workflow--next-stage s))))))

(add-hook 'aob-state-change-hook #'aob-workflow--advance)

;;;###autoload
(defun aob-workflow-run (name &optional agent)
  "Run recorded workflow NAME on a fresh AGENT session; return it.
The coordinator's opening handshake settling to idle triggers the
first stage — the whole run is driven by state transitions."
  (interactive
   (let ((p (aob-workflow--project)))
     (list (completing-read "Workflow: "
                            (or (aob-workflow-names p)
                                (user-error "aob: no workflows in %s" p))
                            nil t))))
  (let* ((wf (aob-workflow-read (aob-workflow--project) name))
         (stages (plist-get wf :stages))
         (agent (or agent (plist-get wf :agent) aob-acp-default-agent))
         (s (progn (unless stages
                     (user-error "aob: workflow %s has no steps" name))
                   (aob-acp-spawn agent nil))))
    (aob-session-put s :wf-name name)
    (aob-session-put s :wf-agent agent)
    (aob-session-put s :wf-stages stages)
    s))

;;;###autoload
(defun aob-workflow-matrix (name agents)
  "Run workflow NAME on several AGENTS at once — the fleet as one gesture.
Isolated agent definitions each get their own worktree, so the runs
never touch the same checkout."
  (interactive
   (list (completing-read "Workflow: "
                          (or (aob-workflow-names (aob-workflow--project))
                              (user-error "aob: no workflows here"))
                          nil t)
         (completing-read-multiple "Agents (comma-separated): "
                                   (mapcar #'car aob-acp-agents))))
  (mapcar (lambda (a) (aob-workflow-run name a)) agents))

;;;###autoload
(defun aob-workflow-brief (name &optional agent)
  "Hand workflow NAME's whole file to one AGENT to run as it sees fit.
The mechanical replay above follows the graph exactly; this trusts the
agent to read the description and drive itself through it."
  (interactive
   (let ((p (aob-workflow--project)))
     (list (completing-read "Brief workflow: "
                            (or (aob-workflow-names p)
                                (user-error "aob: no workflows in %s" p))
                            nil t))))
  (let* ((project (aob-workflow--project))
         (wf (aob-workflow-read project name))
         (text (with-temp-buffer
                 (insert-file-contents (aob-workflow--file project name))
                 (buffer-string))))
    (aob-acp-spawn (or agent (plist-get wf :agent) aob-acp-default-agent)
                   (concat "Execute this workflow now, completing every step;"
                           " steps under one heading may be interleaved,"
                           " stages run in order.  Report when done.\n\n"
                           text))))

;;;###autoload
(defun aob-workflow-stop (s)
  "Abandon S's remaining workflow steps; the session stays alive."
  (interactive (list (aob-target)))
  (aob-session-put s :wf-name nil)
  (aob-session-put s :wf-stages nil)
  (aob-session-put s :wf-waiting nil)
  (message "aob: workflow stopped for %s" (aob-session-name s)))

(provide 'aob-workflow)
;;; aob-workflow.el ends here
