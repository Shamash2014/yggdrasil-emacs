;;; aob-bang.el --- shell lines in a compose draft -*- lexical-binding: t; -*-

;;; Commentary:
;; A line of a draft that starts with ! is a shell command: on send it
;; runs in the session's folder and the command with what it printed
;; goes to the agent in its place.  A draft that is nothing but !!cmd
;; runs cmd for you alone: the output shows in a popup and the agent
;; is sent nothing.

;;; Code:

(require 'subr-x)
(require 'aob)

(defgroup aob-bang nil
  "Shell commands run from a compose draft."
  :group 'aob :prefix "aob-bang-")

(defcustom aob-bang t
  "Whether lines starting with ! in a draft run as shell commands on send."
  :type 'boolean :group 'aob-bang)

(defcustom aob-bang-timeout 30
  "Seconds a command from a draft may run before it is stopped."
  :type 'number :group 'aob-bang)

(defcustom aob-bang-max-lines 200
  "Lines of a command's output kept; the rest are left out with a note."
  :type 'natnum :group 'aob-bang)

(defcustom aob-bang-max-chars 20000
  "Characters of a command's output kept; the rest are left out with a note."
  :type 'natnum :group 'aob-bang)

(defcustom aob-bang-display-action
  '((display-buffer-in-side-window) (side . bottom) (slot . 0)
    (window-height . 0.3))
  "How the output of a !! command is put on screen."
  :type 'sexp :group 'aob-bang)

(defconst aob-bang-buffer-name "*aob-bang*")

(define-derived-mode aob-bang-mode special-mode "bang"
  "The output of a command run from a draft.  The agent never saw it."
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defvar ygg-modal-special-modes)
(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'aob-bang-mode))

(defun aob-bang--dir ()
  "The folder the draft's commands run in: its session's, else the draft's."
  (let* ((s (and (stringp aob-compose--target)
                 (aob-session-get aob-compose--target)))
         (dir (or (and s (or (aob-session-dir s) (aob-session-project s)))
                  aob-compose--dir)))
    (if (and dir (file-directory-p dir))
        (file-name-as-directory (expand-file-name dir))
      default-directory)))

(defun aob-bang--run (command dir)
  "Run COMMAND with the shell in DIR, as (OUTPUT . STATUS).
STATUS is the exit code, (signal . N) when a signal ended it, or timeout."
  (with-temp-buffer
    (let* ((default-directory dir)
           (deadline (+ (float-time) aob-bang-timeout))
           (proc (make-process :name "aob-bang" :buffer (current-buffer)
                               :command (list shell-file-name
                                              shell-command-switch command)
                               :connection-type 'pipe
                               :noquery t :sentinel #'ignore)))
      (process-send-eof proc)
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output proc 0.05))
      (if (process-live-p proc)
          (progn (delete-process proc) (cons "" 'timeout))
        (while (accept-process-output proc 0))
        (cons (buffer-string)
              (if (eq (process-status proc) 'signal)
                  (cons 'signal (process-exit-status proc))
                (process-exit-status proc)))))))

(defun aob-bang--capped (output)
  "OUTPUT cut to the line and character caps, with a note of what went."
  (let* ((lines (split-string (string-trim-right output "\n+") "\n"))
         (lines-cut (max 0 (- (length lines) aob-bang-max-lines)))
         (text (string-join (take aob-bang-max-lines lines) "\n"))
         (chars-cut (max 0 (- (length text) aob-bang-max-chars))))
    (concat (substring text 0 (min (length text) aob-bang-max-chars))
            (when (> lines-cut 0)
              (format "\n… %d more lines left out" lines-cut))
            (when (> chars-cut 0)
              (format "\n… %d more characters left out" chars-cut)))))

(defun aob-bang--status-line (status)
  "What STATUS says about how the command ended, or nil for a clean exit."
  (pcase status
    ('timeout (format "timed out after %s seconds" aob-bang-timeout))
    (`(signal . ,n) (format "killed by signal %d" n))
    (0 nil)
    (n (format "exit status %d" n))))

(defun aob-bang--result (command dir)
  "(OUTPUT . NOTE) for COMMAND run in DIR, OUTPUT capped, NOTE how it ended."
  (pcase-let ((`(,out . ,status) (aob-bang--run command dir)))
    (cons (if (eq status 'timeout) "" (aob-bang--capped out))
          (aob-bang--status-line status))))

(defun aob-bang--block (command dir)
  "COMMAND and what it printed in DIR, marked as command output."
  (pcase-let ((`(,out . ,note) (aob-bang--result command dir)))
    (concat "<shell>\n$ " command "\n"
            (unless (string-empty-p out) (concat out "\n"))
            (when note (concat note "\n"))
            "</shell>")))

(defun aob-bang--command (line)
  "The command LINE runs, or nil when LINE is not one."
  (when (and (string-prefix-p "!" line)
             (not (string-prefix-p "!!" line))
             (not (string-prefix-p "![" line)))
    (let ((cmd (string-trim (substring line 1))))
      (unless (string-empty-p cmd) cmd))))

(defun aob-bang--expand (text dir)
  "TEXT with each command line outside a fence replaced by its block."
  (let (fenced ran)
    (let ((lines (mapcar
                  (lambda (line)
                    (cond ((string-match-p "\\` \\{0,3\\}\\(?:```\\|~~~\\)" line)
                           (setq fenced (not fenced))
                           line)
                          (fenced line)
                          ((aob-bang--command line)
                           (setq ran t)
                           (aob-bang--block (aob-bang--command line) dir))
                          (t line)))
                  (split-string text "\n"))))
      (when ran (string-join lines "\n")))))

(defun aob-bang--show (command dir)
  "Run COMMAND in DIR and show what it printed in the popup."
  (pcase-let ((`(,out . ,note) (aob-bang--result command dir))
              (buf (get-buffer-create aob-bang-buffer-name)))
    (with-current-buffer buf
      (aob-bang-mode)
      (setq default-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (concat "$ " command) 'face 'bold) "\n")
        (unless (string-empty-p out) (insert out "\n"))
        (when note (insert (propertize note 'face 'shadow) "\n"))
        (goto-char (point-min))))
    ;; the draft may sit in a child frame that is about to be deleted
    (with-selected-frame (or (frame-parent) (selected-frame))
      (display-buffer buf aob-bang-display-action))
    buf))

(defun aob-bang-compose (text)
  "TEXT with its command lines run and replaced by what they printed.
A TEXT that is only !!cmd runs cmd for the user alone and is consumed."
  (when aob-bang
    (let ((whole (string-trim text)))
      (if (string-match "\\`!!\\([^\n]+\\)\\'" whole)
          (let ((cmd (string-trim (match-string 1 whole))))
            (unless (string-empty-p cmd)
              (aob-bang--show cmd (aob-bang--dir))
              :consumed))
        (aob-bang--expand text (aob-bang--dir))))))

;; first, so only what was typed runs: held comments and context come later
(add-hook 'aob-compose-before-send-functions #'aob-bang-compose -90)

(provide 'aob-bang)
;;; aob-bang.el ends here
