;;; aob-mcp-host.el --- run the sidecar, and hand it to the agents -*- lexical-binding: t; -*-

;;; Commentary:
;; The editing Emacs starts the headless one, keeps it alive, and names
;; it to every session it spawns.  A session is told a URL carrying a
;; token rather than its own id: the id does not exist until session/new
;; answers, and the servers a session gets are part of that call.  The
;; token is put on the session as it is created, so a tool call arriving
;; later can be traced back to whoever is making it.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob-mcp)
(require 'url-util)

;; declared, not merely bound: these live in aob-acp, and a file compiled
;; without knowing they are special binds them lexically — the `let' below
;; then holds a value nothing else can see, and every session opens with
;; no servers and no token
(defvar aob-acp-mcp-servers)
(defvar aob-acp-session-refs)

(defcustom aob-mcp-host-name "aob"
  "What the server is called in a session's server list."
  :type 'string :group 'aob-mcp)

(defun aob-mcp-host--emacsclient ()
  "The emacsclient belonging to this Emacs.
PATH is the last resort, not the first: on a machine with more than one
Emacs installed the linked one wins there, and it is not necessarily
the one running this daemon.  Inside a NS bundle the sibling lives in
the prefix above Emacs.app, not beside the executable."
  (or (seq-find #'file-executable-p
                (list (expand-file-name "emacsclient" invocation-directory)
                      (expand-file-name "../../../bin/emacsclient"
                                        invocation-directory)
                      (expand-file-name "../bin/emacsclient"
                                        invocation-directory)))
      (executable-find "emacsclient")
      "emacsclient"))

(defvar aob-mcp-host--key nil
  "The secret this Emacs and its sidecar share for this run.")

(defun aob-mcp-host--key-file ()
  (expand-file-name "aob-mcp-key" (locate-user-emacs-file "var/")))

(defun aob-mcp-host--skill-index-file ()
  (expand-file-name "skill-index.json" (locate-user-emacs-file "var/")))

(defun aob-mcp-host--write-key ()
  "Make a secret for this run and leave it where only its owner reads it.
One secret for the life of this Emacs: every session was handed it when
it opened, and a sidecar restarted on a new one answers them all 401."
  (setq aob-mcp-host--key (or aob-mcp-host--key (aob-mcp-host--token)))
  (let ((file (aob-mcp-host--key-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert aob-mcp-host--key))
    (set-file-modes file #o600)
    file))

(defun aob-mcp-host--command ()
  "How to start the headless Emacs.
It is told the port, the socket to call back on and where emacsclient
is: a -Q child inherits none of that, and would otherwise reach for
whatever Emacs happens to be first on PATH."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch"
        ;; -Q reads no init, so the child does not inherit the one setting
        ;; that stops a stale .elc from beating the source beside it
        "--eval" "(setq load-prefer-newer t)"
        "-L" (aob-mcp-host--lisp-dir)
        "-l" "aob-mcp"
        "--eval" (format "%S" `(progn
                                 (setq aob-mcp-port ,aob-mcp-port
                                       aob-mcp-server-name ,(or (bound-and-true-p server-name)
                                                                aob-mcp-server-name)
                                       aob-mcp-key-file ,(aob-mcp-host--key-file)
                                       aob-mcp-emacsclient ,(aob-mcp-host--emacsclient)
                                       ygg-skill-index-file ,(aob-mcp-host--skill-index-file))
                                 ;; the tool set is optional: a server with no
                                 ;; tools still answers, and says so
                                 (require 'aob-mcp-tools nil t)))
        "-f" "aob-mcp-run"))

(defvar aob-mcp-host--process nil)

(defun aob-mcp-host--lisp-dir ()
  (file-name-directory (or (locate-library "aob-mcp") "")))

(defun aob-mcp-host-live-p ()
  (process-live-p aob-mcp-host--process))

;;;###autoload
(defun aob-mcp-host-start ()
  "Start the headless Emacs that serves MCP, unless it is already up."
  (interactive)
  (unless (aob-mcp-host-live-p)
    (aob-mcp-host--write-key)
    (let ((buf (get-buffer-create " *aob-mcp-host*")))
      (setq aob-mcp-host--process
            (make-process
             :name "aob-mcp-host" :buffer buf :noquery t
             :command (aob-mcp-host--command)
             :sentinel
             (lambda (p _e)
               (unless (process-live-p p)
                 (setq aob-mcp-host--process nil)))))))
  aob-mcp-host--process)

(defun aob-mcp-host-stop ()
  "Stop the headless Emacs."
  (interactive)
  (when (process-live-p aob-mcp-host--process)
    (delete-process aob-mcp-host--process))
  (setq aob-mcp-host--process nil)
  (setq aob-mcp-host--key nil)
  (when (file-exists-p (aob-mcp-host--key-file))
    (ignore-errors (delete-file (aob-mcp-host--key-file)))))

(defun aob-mcp-host-restart ()
  "Stop and start, to pick up edited tools, keeping the run's secret."
  (interactive)
  (when (process-live-p aob-mcp-host--process)
    (delete-process aob-mcp-host--process))
  (setq aob-mcp-host--process nil)
  (aob-mcp-host-start))

(add-hook 'kill-emacs-hook #'aob-mcp-host-stop)

;;; Naming it to a session

(defvar aob-mcp-host--tokens (make-hash-table :test 'equal)
  "Token to session id, for calls that arrive after the session opens.")

(defun aob-mcp-host--token ()
  (format "%x%x" (random (expt 2 28)) (float-time)))

(defun aob-mcp-host-session (token)
  "The session TOKEN was spawned for, if it still exists.
The table is a cache, not the record: a session carries its own token,
and it is put there after the session is created — which is one line
too late for `aob-session-created-hook\=' to have seen it."
  (when (fboundp 'aob-session-get)
    (or (when-let* ((id (gethash token aob-mcp-host--tokens)))
          (aob-session-get id))
        (when-let* ((s (seq-find (lambda (s)
                                   (equal token (aob-session-ref s :mcp-token)))
                                 (aob-sessions))))
          (puthash token (aob-session-id s) aob-mcp-host--tokens)
          s))))

(defun aob-mcp-host--claim (session)
  "Record SESSION under the token it was spawned with."
  (when-let* ((token (and (fboundp 'aob-session-ref)
                          (aob-session-ref session :mcp-token))))
    (puthash token (aob-session-id session) aob-mcp-host--tokens)))

(with-eval-after-load 'aob
  (add-hook 'aob-session-created-hook #'aob-mcp-host--claim))

(defun aob-mcp-host-spec (token &optional project)
  "The server entry a session spawned under TOKEN, working in PROJECT, is handed.
The token says which session is calling; the key says it may call at
all, and only sessions this Emacs opened are given it.  The directory
says whose skills a search covers: a worktree\='s, not its main checkout\='s."
  (list :name aob-mcp-host-name
        :type "http"
        :url (concat (if aob-mcp-host--key
                         (format "%s?key=%s&session=%s" (aob-mcp-url)
                                 (url-hexify-string aob-mcp-host--key) token)
                       (format "%s?session=%s" (aob-mcp-url) token))
                     (if (stringp project)
                         (concat "&project="
                                 (url-hexify-string (expand-file-name project)))
                       ""))))

(defun aob-mcp-host--around-spawn (fn &rest args)
  "Give every session this Emacs opens the sidecar, under its own token."
  (aob-mcp-host-start)
  (let* ((token (aob-mcp-host--token))
         (aob-acp-mcp-servers (cons (aob-mcp-host-spec token (or (nth 3 args) (nth 2 args)))
                                    (bound-and-true-p aob-acp-mcp-servers)))
         (aob-acp-session-refs (append (list :mcp-token token)
                                       (bound-and-true-p aob-acp-session-refs))))
    (apply fn args)))

;;;###autoload
(define-minor-mode aob-mcp-host-mode
  "Hand the sidecar to every ACP session this Emacs opens."
  :global t :group 'aob-mcp
  (if aob-mcp-host-mode
      ;; every way in, not just `aob-acp-spawn': a resumed conversation
      ;; and a forked one are sessions of this Emacs too, and a draft
      ;; that spawns goes through the same funnel
      (advice-add 'aob-acp--open :around #'aob-mcp-host--around-spawn)
    (advice-remove 'aob-acp--open #'aob-mcp-host--around-spawn)
    (aob-mcp-host-stop)))

;;; Talking to another session

(declare-function aob-session-id "aob")
(declare-function aob-session-ref "aob")
(declare-function aob-session-name "aob")
(declare-function aob-session-state "aob")
(declare-function aob-session-events "aob")
(declare-function aob-session-get "aob")
(declare-function aob-sessions "aob")
(declare-function aob-prompt "aob")
(declare-function aob-interject "aob")
(declare-function aob-acp--steers-p "aob-acp")

(defcustom aob-mcp-host-session-targets nil
  "Regexps matching the names of sessions an agent may say to, read or ask.
Nil leaves every session reachable."
  :type '(repeat regexp) :group 'aob-mcp)

(defun aob-mcp-host-target-allowed-p (session)
  "Non-nil when an agent may reach SESSION by `aob-mcp-host-session-targets'."
  (or (null aob-mcp-host-session-targets)
      (let ((name (aob-session-name session)))
        (seq-some (lambda (re) (string-match-p re name))
                  aob-mcp-host-session-targets))))

(defun aob-mcp-host-say (session text)
  "Say TEXT to SESSION, into its running turn if it takes steering, else queued.
Return a line saying how it went."
  (let ((name (aob-session-name session)))
    (condition-case err
        (if (and (eq (aob-session-state session) 'working)
                 (fboundp 'aob-acp--steers-p)
                 (ignore-errors (aob-acp--steers-p session)))
            (progn (aob-interject session text)
                   (format "said to %s, into the turn it is running" name))
          (aob-prompt session text nil)
          (format "said to %s (%s)" name (aob-session-state session)))
      (error (format "%s would not take it: %s" name (error-message-string err))))))

(defvar aob-mcp-host--asks nil
  "Questions awaiting an answer, as (ASKER-ID TARGET-ID SENT-TEXT).")

(defun aob-mcp-host--ask-for (asker target)
  (seq-find (lambda (ask) (and (equal (car ask) (aob-session-id asker))
                               (equal (cadr ask) (aob-session-id target))))
            aob-mcp-host--asks))

(defun aob-mcp-host-ask (asker target question)
  "Put QUESTION from ASKER to TARGET; its answer comes back to ASKER as a message.
Return a line saying how it went."
  (cond
   ((null asker) "cannot tell which session is asking")
   ((eq asker target) "that is you: ask another session")
   ((aob-mcp-host--ask-for asker target)
    (format "%s has not answered your last question yet; its reply will arrive as a message"
            (aob-session-name target)))
   (t
    (let* ((sent (format "Question from %s (your reply to this turn goes back to them on its own, so answer here):\n\n%s"
                         (aob-session-name asker) question))
           (ask (list (aob-session-id asker) (aob-session-id target) sent)))
      (push ask aob-mcp-host--asks)
      (let ((said (aob-mcp-host-say target sent)))
        (if (string-search "would not take it" said)
            (progn (setq aob-mcp-host--asks (delq ask aob-mcp-host--asks))
                   said)
          (format "asked %s; its reply will arrive as a message, so end your turn instead of polling"
                  (aob-session-name target))))))))

(defun aob-mcp-host--answer (target sent)
  "The last thing TARGET said in the turn that took SENT, or nil before it began."
  (let ((answer nil)
        (began nil))
    (catch 'done
      (dolist (ev (aob-session-events target))
        (cond
         ((and (eq (plist-get ev :type) 'prompt)
               (equal (plist-get ev :text) sent)
               (not (equal (plist-get ev :status) "queued")))
          (setq began t)
          (throw 'done nil))
         ((and (null answer) (eq (plist-get ev :type) 'message)
               (null (plist-get ev :parent)))
          (setq answer (string-trim
                        (substring-no-properties
                         (concat (or (plist-get ev :text) "")
                                 (apply #'concat (reverse (plist-get ev :parts)))))))))))
    (and began (or answer ""))))

(defun aob-mcp-host--deliver (ask body)
  "Tell ASK's asker BODY, unless it is gone, and stop waiting on ASK."
  (setq aob-mcp-host--asks (delq ask aob-mcp-host--asks))
  (when-let* ((asker (aob-session-get (car ask))))
    (aob-mcp-host-say asker body)))

(defun aob-mcp-host--on-state (session _old new)
  "Return answers when SESSION's turn ends, and say so when it ends for good."
  (dolist (ask (seq-filter (lambda (ask) (equal (cadr ask) (aob-session-id session)))
                           aob-mcp-host--asks))
    (let ((name (aob-session-name session)))
      (cond
       ((memq new '(dead failed))
        (aob-mcp-host--deliver ask (format "No reply from %s: it ended before answering." name)))
       ((eq new 'idle)
        (when-let* ((answer (aob-mcp-host--answer session (nth 2 ask))))
          (aob-mcp-host--deliver
           ask (format "Reply from %s:\n\n%s" name
                       (if (string-empty-p answer) "(it finished without a message)" answer)))))))))

(defun aob-mcp-host--on-removed (session)
  "Forget what SESSION was asked or was waiting on."
  (let* ((id (aob-session-id session))
         (heir (aob-session-get id)))
    (unless (and heir (not (eq heir session)))
      (dolist (ask (copy-sequence aob-mcp-host--asks))
        (cond
         ((equal (car ask) id) (setq aob-mcp-host--asks (delq ask aob-mcp-host--asks)))
         ((equal (cadr ask) id)
          (aob-mcp-host--deliver
           ask (format "No reply from %s: it ended before answering." (aob-session-name session)))))))))

(with-eval-after-load 'aob
  (add-hook 'aob-state-change-hook #'aob-mcp-host--on-state)
  (add-hook 'aob-session-removed-hook #'aob-mcp-host--on-removed))

(provide 'aob-mcp-host)

;;; aob-mcp-host.el ends here
