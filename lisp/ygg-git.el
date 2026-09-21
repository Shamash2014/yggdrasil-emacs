;;; ygg-git.el --- one way the harness asks git -*- lexical-binding: t; -*-

;;; Commentary:

;; Every git call the harness makes runs through here, so what git is
;; asked is one thing to read, one thing to stand in for in a test, and
;; one place the verbs that rewrite a checkout are refused.

;;; Code:

(require 'seq)
(require 'subr-x)

(defconst ygg-git-guarded-verbs
  '("checkout" "switch" "stash" "reset" "rebase" "merge" "commit" "push"
    "clone")
  "Verbs a caller must name in :allow before git is asked them.
The owner keeps the harness out of the working tree it is standing in:
nothing here moves HEAD, writes a commit or publishes one behind the
owner's back.  The clone that makes a checkout of its own says so at the
call site.")

(defconst ygg-git--stderr-prefix "ygg-git"
  "Prefix of the temporary file git's diagnostics are read back from.")

(defun ygg-git--split (args)
  "ARGS as a cons of its option plist and the argv left for git.
:allow names a guarded verb the caller is entitled to and :input is the
text handed to git on its standard input."
  (let (opts argv)
    (while args
      (let ((head (car args)))
        (if (memq head '(:allow :input))
            (progn (setq opts (plist-put opts head (cadr args)))
                   (setq args (cddr args)))
          (push head argv)
          (setq args (cdr args)))))
    (cons opts (nreverse argv))))

(defun ygg-git--verb (argv)
  "The subcommand ARGV asks for: its first word that is not an option."
  (seq-find (lambda (arg) (not (string-prefix-p "-" arg))) argv))

(defun ygg-git--guard (verb allow)
  "Refuse VERB unless it is unguarded or ALLOW names it."
  (when (and (member verb ygg-git-guarded-verbs)
             (not (equal allow verb)))
    (user-error "git %s: the harness does not run this without :allow" verb)))

(defun ygg-git--ready (root verb)
  "Refuse when git is missing or ROOT is no directory to ask VERB in."
  (unless (executable-find "git")
    (user-error "git %s: git is not on the path" (or verb "")))
  (unless (and root (file-directory-p root))
    (user-error "git %s: %s is not a directory" (or verb "") root)))

(defun ygg-git--argv (argv)
  "ARGV with the pager turned off, whatever the owner's config says."
  (cons "--no-pager" argv))

(defmacro ygg-git--in (root &rest body)
  "Run BODY with ROOT current, colour off and the coding pinned."
  (declare (indent 1))
  `(let ((default-directory (file-name-as-directory (expand-file-name ,root)))
         (coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (process-environment (cons "NO_COLOR=1" process-environment)))
     ,@body))

(defun ygg-git--said (file)
  "What git wrote to FILE, trimmed."
  (string-trim (with-temp-buffer
                 (ignore-errors (insert-file-contents file))
                 (buffer-string))))

(defconst ygg-git-own-exclude '("--" "." ":(exclude).aob")
  "The pathspec that leaves the harness's own files out of a diff.
A task's directory, its reports and its handoff live under .aob inside
the checkout, and none of it is work the worker did.")

(defun ygg-git (root &rest args)
  "Run git in ROOT with ARGS and return what it wrote to standard output.
A call git refuses signals a user-error carrying what git said on
standard error, so a caller that wants nil instead wraps this in
ignore-errors.  ARGS may carry :input TEXT, handed to git on its
standard input, and :allow VERB, entitling the call to one of
ygg-git-guarded-verbs."
  (pcase-let* ((`(,opts . ,argv) (ygg-git--split args))
               (verb (ygg-git--verb argv)))
    (ygg-git--guard verb (plist-get opts :allow))
    (ygg-git--ready root verb)
    (let ((errfile (make-temp-file ygg-git--stderr-prefix)))
      (unwind-protect
          (ygg-git--in root
            (with-temp-buffer
              (let ((status (apply #'call-process-region
                                   (or (plist-get opts :input) "") nil
                                   "git" nil (list t errfile) nil
                                   (ygg-git--argv argv))))
                (if (eq status 0)
                    (buffer-string)
                  (user-error "git %s: %s" (string-join argv " ")
                              (ygg-git--said errfile))))))
        (delete-file errfile)))))

(defun ygg-git-lines (root &rest args)
  "The non-empty lines git ARGS print in ROOT.
Signals the way ygg-git does."
  (split-string (apply #'ygg-git root args) "\n" t))

(defun ygg-git-async (root args callback)
  "Run git ARGS in ROOT without waiting; CALLBACK gets its output and exit.
Both of git's streams reach CALLBACK as one string, the way a shell
would show them, and the exit code follows it.  ARGS is a list and may
carry :allow VERB."
  (pcase-let* ((`(,opts . ,argv) (ygg-git--split args))
               (verb (ygg-git--verb argv)))
    (ygg-git--guard verb (plist-get opts :allow))
    (ygg-git--ready root verb)
    (let ((buffer (generate-new-buffer " *ygg-git*")))
      (ygg-git--in root
        (make-process
         :name "ygg-git" :buffer buffer :noquery t
         :command (cons "git" (ygg-git--argv argv))
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((exit (process-exit-status process))
                   (out (with-current-buffer buffer (buffer-string))))
               (kill-buffer buffer)
               (funcall callback out exit)))))))))

(provide 'ygg-git)
;;; ygg-git.el ends here
