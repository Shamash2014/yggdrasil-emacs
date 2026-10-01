;;; aob-shells.el --- every command the agents have running, and a way to stop one -*- lexical-binding: t; -*-

;;; Commentary:
;; Agents run commands with their own tools; this lists the ones still
;; running in any session, and stops one through its agent when the agent
;; can, else by ending its own process.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)

(defconst aob-shells--buffer "*aob shells*")

(defvar-local aob-shells--pairs nil
  "Each listed command's event seq to (SESSION . EVENT); a row's id is the seq.")

(defun aob-shells--process-table ()
  "Every local process as a list of (PID PPID ARGS START)."
  (delq nil (mapcar (lambda (pid)
                      (when-let* ((a (process-attributes pid)))
                        (list pid (alist-get 'ppid a)
                              (or (alist-get 'args a) (alist-get 'comm a) "")
                              (alist-get 'start a))))
                    (list-system-processes))))

(defun aob-shells--below (root table)
  "The rows of TABLE that descend from pid ROOT, ROOT itself not among them."
  (let ((children (make-hash-table)) (out nil) (todo (list root)))
    (dolist (row table)
      (push row (gethash (nth 1 row) children)))
    (while todo
      (dolist (row (gethash (pop todo) children))
        (unless (memq row out)
          (push row out)
          (push (car row) todo))))
    out))

(defun aob-shells--needles (command)
  "What a process running COMMAND has in its arguments, surest first.
An adapter hands the command to a shell, quoted its own way; the longest
stretch of it with no quote in it survives any quoting."
  (let* ((whole (string-trim command))
         (bare (car (sort (split-string whole "[\"'\\\\\n]" t)
                          (lambda (a b) (> (length a) (length b)))))))
    (delete-dups (delq nil (list whole (and bare (>= (length (string-trim bare)) 6)
                                            (string-trim bare)))))))

(defun aob-shells--runs-p (needle args)
  "Non-nil when ARGS hold NEEDLE as a whole word of a command line."
  (string-match-p (concat "\\(?:\\`\\|[ \t'\"(;&|]\\)" (regexp-quote needle)
                          "\\(?:\\'\\|[ \t'\";&|)]\\)")
                  args))

(defun aob-shells--find (s ev table)
  "The row of TABLE running EV's command for S, or a string saying why none.
Only a process below S's agent can be it, never the agent itself, and
only one started after the call that ran the command: the adapter and
the CLI under it are older, whatever their arguments say.  Of the
processes that match, the one no other match is above."
  (let ((conn (aob-acp--proc-of s))
        (since (- (plist-get ev :ts) 5)))
    (cond
     ((not (process-live-p conn)) "its agent is not running")
     ((process-get conn 'aob-remote) "its agent runs on another host")
     (t
      (let ((below (seq-filter (lambda (row) (and (nth 3 row)
                                                  (>= (float-time (nth 3 row)) since)))
                               (aob-shells--below (process-id conn) table)))
            (found nil))
        (dolist (needle (aob-shells--needles (aob-trace--shell-command ev)))
          (unless found
            (let ((hits (seq-filter (lambda (row) (aob-shells--runs-p needle (nth 2 row)))
                                    below)))
              (setq found (seq-remove (lambda (row) (assq (nth 1 row) hits)) hits)))))
        (pcase (length found)
          (0 "no process of its agent runs it")
          (1 (car found))
          (n (format "%d processes of its agent run it" n))))))))

(defun aob-shells--kill (pid table)
  "Signal PID and every process below it in TABLE to end; return the pids."
  (let ((pids (cons pid (mapcar #'car (aob-shells--below pid table)))))
    (dolist (p pids) (ignore-errors (signal-process p 'TERM)))
    pids))

(defun aob-shells--live ()
  "Every running command as (SESSION . EVENT), newest first."
  (let (out)
    (dolist (s (aob-sessions))
      (dolist (ev (aob-session-events s))
        (when (aob-trace-shell-live-p ev)
          (push (cons s ev) out))))
    (nreverse out)))

(defun aob-shells--settle (s ev table)
  "EV's pid in TABLE, or nil; end EV when what ran it is gone.
A command claude left in the background is never said to end, so one
whose process was seen and is no longer there has ended."
  (let ((conn (aob-acp--proc-of s)))
    (if (not (process-live-p conn))
        (progn (aob-acp-shell-end s ev 'gone) nil)
      (let ((row (aob-shells--find s ev table)))
        (cond ((consp row) (plist-put ev :pid (car row)) (car row))
              ((and (plist-get ev :pid) (stringp (plist-get ev :background))
                    (not (plist-get ev :task-id))
                    (not (process-get conn 'aob-remote)))
               (aob-acp-shell-end s ev 'gone)
               nil))))))

(defun aob-shells--last-line (ev)
  "The last line EV's command printed, read from its output file when it has one."
  (let* ((file (plist-get ev :output-file))
         (text (if (and file (not (file-remote-p file)) (file-readable-p file))
                   (with-temp-buffer
                     (let ((size (file-attribute-size (file-attributes file))))
                       (insert-file-contents file nil (max 0 (- size 4096)) size))
                     (buffer-string))
                 (string-join (aob-trace--shell-output ev) "\n"))))
    (or (car (last (split-string text "\n" t "[ \t\r]+"))) "")))

(defun aob-shells--entries ()
  (let ((table (aob-shells--process-table))
        (rows nil))
    (setq aob-shells--pairs (make-hash-table))
    (dolist (pair (aob-shells--live))
      (let* ((s (car pair)) (ev (cdr pair))
             (pid (aob-shells--settle s ev table)))
        (when (aob-trace-shell-live-p ev)
          (puthash (plist-get ev :seq) pair aob-shells--pairs)
          (push (list (plist-get ev :seq)
                      (vector (aob-session-name s)
                              (if (plist-get ev :background) "background" "running")
                              (or (aob-trace--shell-elapsed ev) "")
                              (if pid (number-to-string pid) "")
                              (car (split-string (aob-trace--shell-command ev) "\n"))
                              (abbreviate-file-name
                               (or (plist-get ev :cwd) (aob-session-dir s)
                                   (aob-session-project s) ""))
                              (aob-shells--last-line ev)))
                rows))))
    (setq tabulated-list-entries (nreverse rows))))

(defun aob-shells--at-point ()
  (or (and aob-shells--pairs (gethash (tabulated-list-get-id) aob-shells--pairs))
      (user-error "aob: no command here")))

(defun aob-shells-visit (pair)
  "Show the trace row of the command in PAIR, a session and its event."
  (interactive (list (aob-shells--at-point)))
  (pop-to-buffer (aob-trace-buffer (car pair)))
  (if-let* ((at (aob-trace--event-bounds (plist-get (cdr pair) :seq))))
      (goto-char (car at))
    (message "aob: that command is not drawn in the trace")))

(defun aob-shells-output (pair)
  "Show all the command in PAIR has printed.
Its output file, followed as it grows, when it has one."
  (interactive (list (aob-shells--at-point)))
  (let* ((ev (cdr pair))
         (file (plist-get ev :output-file)))
    (if (and file (file-readable-p file))
        (with-current-buffer (find-file-noselect file)
          (auto-revert-tail-mode 1)
          (pop-to-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create
                            (format "*aob shell: %s*"
                                    (truncate-string-to-width
                                     (car (split-string (aob-trace--shell-command ev) "\n"))
                                     40)))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (string-join (aob-trace--shell-output ev) "\n"))
          (special-mode))
        (pop-to-buffer (current-buffer))))))

(defun aob-shells-kill (s ev)
  "End the process running EV's command for S, and what it started, once asked.
Only that process is signalled, and the ones below it: the agent and its
other commands are left running."
  (let* ((table (aob-shells--process-table))
         (row (aob-shells--find s ev table)))
    (when (stringp row) (user-error "aob: cannot end it: %s" row))
    (when (y-or-n-p (format "End pid %d, %s? " (car row)
                            (truncate-string-to-width (nth 2 row) 60 nil nil "…")))
      (let ((pids (aob-shells--kill (car row) table)))
        (aob-acp-shell-end s ev 'stopped)
        (message "aob: ended %s" (mapconcat #'number-to-string pids " "))
        pids))))

(defun aob-shells-stop (pair)
  "Stop the command in PAIR: by its agent when it can, else by ending its process.
An agent that says it did not stop it leaves the next stop to end the process."
  (interactive (list (aob-shells--at-point)))
  (let ((s (car pair)) (ev (cdr pair)))
    (if (and (aob-acp-task-stoppable-p s ev) (not (plist-get ev :stop-refused)))
        (aob-acp-stop-task
         s ev (lambda (stopped err)
                (if stopped
                    (message "aob: the agent stopped it")
                  (plist-put ev :stop-refused t)
                  (message "aob: the agent did not stop it%s; stop again to end its process"
                           (if err (format " (%s)" (plist-get err :message)) "")))
                (aob-shells--redraw)))
      (aob-shells-kill s ev))
    (aob-shells--redraw)))

(defun aob-shells--redraw ()
  (when-let* ((buf (get-buffer aob-shells--buffer)))
    (with-current-buffer buf
      (aob-shells--entries)
      (tabulated-list-print t))))

(defvar-keymap aob-shells-mode-map
  :parent tabulated-list-mode-map
  "RET" #'aob-shells-visit
  "o" #'aob-shells-output
  "x" #'aob-shells-stop)

(define-derived-mode aob-shells-mode tabulated-list-mode "Shells"
  "Every command an agent has running, in any session."
  (setq tabulated-list-format [("Session" 18 t) ("State" 10 t) ("Time" 8 nil)
                               ("Pid" 7 nil) ("Command" 40 nil) ("Cwd" 24 t)
                               ("Output" 0 nil)])
  (add-hook 'tabulated-list-revert-hook #'aob-shells--entries nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun aob-shells ()
  "List every command the agents have running.
RET shows its trace row, o its output, and x stops it.
From a trace, point lands on the command at point."
  (interactive)
  (let ((seq (and (derived-mode-p 'aob-trace-mode)
                  (get-text-property (line-beginning-position) 'aob-event))))
    (with-current-buffer (get-buffer-create aob-shells--buffer)
      (aob-shells-mode)
      (aob-shells--entries)
      (tabulated-list-print)
      (when seq
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not (eql seq (tabulated-list-get-id))))
          (forward-line 1))
        (when (eobp) (goto-char (point-min))))
      (pop-to-buffer (current-buffer)))))

(provide 'aob-shells)
;;; aob-shells.el ends here
