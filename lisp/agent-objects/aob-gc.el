;;; aob-gc.el --- sweep merged worktrees and branches -*- lexical-binding: t; -*-

;;; Commentary:
;; A branch is merged when the default upstream branch has its work: as an
;; ancestor, as one squashed commit, or as a merged GitHub PR.  Only merged,
;; clean, unoccupied worktrees and their branches are ever removed.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'project)
(require 'aob)
(require 'aob-acp)
(require 'ygg-git)

(defgroup aob-gc nil "Sweeping merged agent worktrees and branches."
  :group 'aob)

(defcustom aob-gc-fetch t
  "Non-nil runs a quiet git fetch --prune before each project is swept."
  :type 'boolean :group 'aob-gc)

(defcustom aob-gc-fetch-timeout 60
  "Seconds a sweep's git fetch may take before it is killed."
  :type 'integer :group 'aob-gc)

(defcustom aob-gc-branches 'aob
  "Which merged local branches a sweep deletes: only aob/*, or any."
  :type '(choice (const :tag "aob/* only" aob)
                 (const :tag "every merged branch" all-merged))
  :group 'aob-gc)

(defcustom aob-gc-interval 900
  "Seconds between background sweeps."
  :type 'integer :group 'aob-gc)

(defcustom aob-gc-grace 300
  "Seconds after Emacs starts or resumes before the first sweep."
  :type 'integer :group 'aob-gc)

(defconst aob-gc--env '("GIT_TERMINAL_PROMPT=0")
  "Environment that keeps git from prompting.")

(defcustom aob-gc-precious-ignored
  '("\\(\\`\\|/\\)\\.env[^/]*/?\\'" "\\.local/?\\'" "\\.db/?\\'"
    "\\.sqlite[^/]*/?\\'" "\\.pem/?\\'" "\\.key/?\\'"
    "credentials[^/]*/?\\'" "secret[^/]*/?\\'")
  "Regexps for ignored files whose worktree is never removed."
  :type '(repeat regexp) :group 'aob-gc)

(defconst aob-gc--terminal-modes
  '(shell-mode eshell-mode vterm-mode eat-mode ghostel-mode compilation-mode
    comint-mode term-mode)
  "Major modes whose buffers keep a worktree in use through `default-directory'.")

(defvar aob-gc--busy (make-hash-table :test #'equal)
  "Repository git dir to the token of the sweep holding it.")

(defvar aob-gc--verdicts (make-hash-table :test #'equal)
  "(BRANCH-SHA . TARGET-SHA) to merge verdicts.")

(defvar aob-gc--prs (make-hash-table :test #'equal)
  "Project to the merged pull requests gh listed for it in the current sweep.")

(defvar aob-gc--guard nil
  "Function handed the error of the sweep whose step is running, or nil.")

(defvar aob-gc--timer nil)
(defvar aob-gc--idle nil)
(defvar aob-gc--last-tick nil)
(defvar aob-gc--expected nil
  "Seconds the armed timer was due to wait before its next tick.")
(defvar aob-gc-mode)

(defun aob-gc--guarded (fn)
  "FN as a callback that keeps the current guard and hands it any error."
  (let ((guard aob-gc--guard))
    (lambda (&rest args)
      (let ((aob-gc--guard guard))
        (condition-case err
            (apply fn args)
          (error (if guard
                     (funcall guard err)
                   (signal (car err) (cdr err)))))))))

(defun aob-gc--git (root args callback)
  "Run git ARGS in ROOT; CALLBACK gets output and exit, 127 if git cannot start."
  (let* ((ran nil)
         (run (aob-gc--guarded (lambda (out code)
                                 (setq ran t)
                                 (funcall callback out code)))))
    (condition-case err
        (let ((process-environment (append aob-gc--env process-environment)))
          (ygg-git-async root args run))
      (user-error (if ran
                      (signal (car err) (cdr err))
                    (funcall run "" 127))))))

(defun aob-gc--first-line (text)
  "The first non-empty line of TEXT, or a note that git said nothing."
  (or (car (split-string text "\n" t "[ \t]+")) "git gave no reason"))

(defun aob-gc--each (items step done)
  "Call STEP on each of ITEMS with a continuation, then DONE."
  (if (null items)
      (funcall done)
    (funcall step (car items)
             (lambda () (aob-gc--each (cdr items) step done)))))

(defun aob-gc--first-ref (root refs callback)
  "CALLBACK gets (REF . SHA) for the first of REFS that resolves in ROOT, or nil."
  (if (null refs)
      (funcall callback nil)
    (aob-gc--git root (list "rev-parse" "--verify" "--quiet"
                            (concat (car refs) "^{commit}"))
                 (lambda (out code)
                   (if (zerop code)
                       (funcall callback (cons (car refs) (string-trim out)))
                     (aob-gc--first-ref root (cdr refs) callback))))))

(defun aob-gc--default (root callback)
  "CALLBACK gets ROOT's default branch as (:ref REF :sha SHA :local NAME), or nil."
  (aob-gc--git
   root '("symbolic-ref" "-q" "refs/remotes/origin/HEAD")
   (lambda (out code)
     (let ((remote (string-trim out)))
       (aob-gc--first-ref
        root (append (and (zerop code) (string-prefix-p "refs/remotes/" remote)
                          (list remote))
                     '("refs/heads/main" "refs/heads/master"))
        (lambda (found)
          (funcall callback
                   (and found
                        (list :ref (car found) :sha (cdr found)
                              :local (replace-regexp-in-string
                                      "\\`refs/\\(?:heads\\|remotes/[^/]+\\)/" ""
                                      (car found)))))))))))

(defun aob-gc--targets (root head-too callback)
  "CALLBACK gets (REF . SHA) of ROOT's default branch, and HEAD with HEAD-TOO."
  (aob-gc--default
   root
   (lambda (default)
     (let ((base (and default (list (cons (plist-get default :ref)
                                          (plist-get default :sha))))))
       (if (not head-too)
           (funcall callback base)
         (aob-gc--first-ref root '("HEAD")
                            (lambda (found)
                              (funcall callback (append base (and found (list found)))))))))))

(defun aob-gc--tip (root branch callback)
  "CALLBACK gets the commit local BRANCH of ROOT is at, or nil when it is gone."
  (aob-gc--first-ref root (list (concat "refs/heads/" branch))
                     (lambda (found) (funcall callback (cdr found)))))

(defun aob-gc--pr-merged-p (prs branch sha)
  "Non-nil when PRS lists a merged PR of this repository from BRANCH at SHA."
  (and (seq-some (lambda (pr)
                   (and (equal (alist-get 'headRefName pr) branch)
                        (equal (alist-get 'headRefOid pr sha) sha)
                        (not (eq (alist-get 'isCrossRepository pr) t))))
                 prs)
       t))

(defun aob-gc--merged-prs (root callback)
  "CALLBACK gets the merged PRs gh lists for ROOT, asked once per sweep."
  (let ((hit (gethash root aob-gc--prs 'unseen)))
    (cond
     ((not (eq hit 'unseen)) (funcall callback hit))
     ((not (executable-find "gh")) (funcall callback nil))
     (t
      (aob-gc--git
       root '("remote" "get-url" "origin")
       (lambda (url code)
         (if (not (and (zerop code) (string-match-p "github\\.com" url)))
             (funcall callback nil)
           (condition-case nil
               (let ((buffer (generate-new-buffer " *aob-gc-gh*"))
                     (callback (aob-gc--guarded callback))
                     (default-directory (file-name-as-directory root)))
                 (make-process
                  :name "aob-gc-gh" :buffer buffer :noquery t
                  :command (list "gh" "pr" "list" "--state" "merged" "--limit" "200"
                                 "--json" "headRefName,headRefOid,isCrossRepository")
                  :sentinel
                  (lambda (process _event)
                    (unless (process-live-p process)
                      (let ((out (with-current-buffer buffer (buffer-string)))
                            (ok (zerop (process-exit-status process))))
                        (kill-buffer buffer)
                        (let ((prs (and ok (ignore-errors
                                             (json-parse-string out :object-type 'alist
                                                                :array-type 'list)))))
                          (puthash root prs aob-gc--prs)
                          (funcall callback prs)))))))
             (error (funcall callback nil))))))))))

(defun aob-gc--gh-merged-p (root branch sha callback)
  "CALLBACK gets non-nil when a PR of this repository merged BRANCH at SHA."
  (aob-gc--merged-prs root (lambda (prs) (funcall callback (aob-gc--pr-merged-p prs branch sha)))))

(defun aob-gc--local-merged (root sha target callback)
  "CALLBACK gets non-nil when SHA is in TARGET (REF . SHA), squashed or not.
A branch counts as squashed when merging it into TARGET changes nothing."
  (let* ((tsha (cdr target))
         (key (cons sha tsha))
         (hit (gethash key aob-gc--verdicts 'unseen)))
    (if (not (eq hit 'unseen))
        (funcall callback hit)
      (let ((settle (lambda (v) (puthash key v aob-gc--verdicts) (funcall callback v))))
        (aob-gc--git
         root (list "merge-base" "--is-ancestor" sha tsha)
         (lambda (_out code)
           (cond
            ((zerop code) (funcall settle t))
            ((/= code 1) (funcall callback nil))
            (t
             (aob-gc--git
              root (list "merge-tree" "--write-tree" "--no-messages" tsha sha)
              (lambda (merged mcode)
                (cond
                 ((= mcode 1) (funcall settle nil))
                 ((/= mcode 0) (funcall callback nil))
                 (t
                  (aob-gc--git
                   root (list "rev-parse" (concat tsha "^{tree}"))
                   (lambda (tree tcode)
                     (if (zerop tcode)
                         (funcall settle (equal (car (split-string merged "\n" t))
                                                (string-trim tree)))
                       (funcall callback nil))))))))))))))))

(defun aob-gc--any (items step done)
  "Call STEP on ITEMS until one gives a non-nil verdict; DONE gets it."
  (if (null items)
      (funcall done nil)
    (funcall step (car items)
             (lambda (v)
               (if v
                   (funcall done v)
                 (aob-gc--any (cdr items) step done))))))

(defun aob-gc--merged-into (root branch sha targets callback)
  "CALLBACK gets non-nil when BRANCH at SHA is in a TARGET or merged by a PR."
  (if (null targets)
      (funcall callback nil)
    (aob-gc--any targets
                 (lambda (target k) (aob-gc--local-merged root sha target k))
                 (lambda (merged)
                   (if merged
                       (funcall callback t)
                     (aob-gc--gh-merged-p root branch sha callback))))))

(defun aob-gc--inside-p (path dir)
  "Non-nil when PATH is a local file at or below DIR."
  (and (stringp path) (not (file-remote-p path)) (file-in-directory-p path dir)))

(defun aob-gc--in-use-p (dir except)
  "Non-nil when an aob session other than EXCEPT, in any state, is in DIR."
  (condition-case nil
      (and (seq-some (lambda (s)
                       (and (not (eq s except))
                            (aob-gc--inside-p (aob-session-dir s) dir)))
                     (aob-sessions))
           t)
    (error t)))

(defun aob-gc--saved (dir)
  "Persisted conversations with a path at or below DIR."
  (seq-filter
   (lambda (e)
     (seq-some (lambda (path) (aob-gc--inside-p path dir))
               (append (mapcar (lambda (k) (plist-get e k))
                               '(:dir :project :task :todo-file))
                       (plist-get e :extra-dirs))))
   (aob-acp--persisted-entries)))

(defun aob-gc--true-inside (path dir)
  "PATH spelled through real directories when it is at or below the real DIR.
DIR may be gone already, which `file-in-directory-p' cannot judge."
  (let ((true (and (stringp path) (not (file-remote-p path)) (file-truename path))))
    (if (and true (string-prefix-p (file-name-as-directory dir) (file-name-as-directory true)))
        true
      path)))

(defun aob-gc--moved (entry dir main)
  "ENTRY with its paths below the removed DIR moved to the same places under MAIN."
  (let ((dir (file-truename dir))
        (entry (copy-sequence entry)))
    (dolist (key '(:dir :project :task :todo-file))
      (when-let* ((path (plist-get entry key)))
        (setq entry (plist-put entry key (aob-gc--true-inside path dir)))))
    (when-let* ((dirs (plist-get entry :extra-dirs)))
      (setq entry (plist-put entry :extra-dirs
                             (mapcar (lambda (d) (aob-gc--true-inside d dir)) dirs))))
    (let ((where (plist-get entry :dir)))
      (plist-put (aob-acp--rebase-entry entry dir main)
                 :dir (aob-acp--rebase where dir main)))))

(defun aob-gc--rehome (ids dir main)
  "Point the saved conversations IDS that were in the removed DIR at MAIN."
  (let ((main (file-name-as-directory main)))
    (dolist (id ids)
      (aob-acp--rewrite id (lambda (entry) (aob-gc--moved entry dir main))))))

(defun aob-gc--saved-ids (dir)
  "The acp ids of the persisted conversations with a path at or below DIR."
  (delq nil (mapcar (lambda (e) (plist-get e :acp-id)) (aob-gc--saved dir))))

(defun aob-gc--visited-p (dir)
  "Non-nil when an Emacs buffer visits, edits or runs a shell in DIR."
  (condition-case nil
      (and (seq-some
            (lambda (b)
              (with-current-buffer b
                (or (aob-gc--inside-p buffer-file-name dir)
                    (and (aob-gc--inside-p default-directory dir)
                         (or (buffer-modified-p)
                             (get-buffer-process b)
                             (apply #'derived-mode-p aob-gc--terminal-modes))))))
            (buffer-list))
           t)
    (error t)))

(defun aob-gc--precious-p (file)
  "Non-nil when FILE, or a folder it lies in, matches `aob-gc-precious-ignored'."
  (let ((parts (split-string file "/" t))
        (path "")
        found)
    (while (and parts (not found))
      (setq path (concat path (and (not (string-empty-p path)) "/") (pop parts)))
      (setq found (seq-some (lambda (re) (string-match-p re path)) aob-gc-precious-ignored)))
    found))

(defun aob-gc--tree-verdict (dir branch callback)
  "CALLBACK gets nil when DIR is on BRANCH, clean and holds nothing precious."
  (aob-gc--git
   dir '("symbolic-ref" "-q" "HEAD")
   (lambda (head hcode)
     (if (not (and (zerop hcode)
                   (equal (string-trim head) (concat "refs/heads/" branch))))
         (funcall callback "not on its branch")
       (aob-gc--git
        dir '("status" "--porcelain" "-z")
        (lambda (out code)
          (if (not (and (zerop code) (string-empty-p out)))
              (funcall callback "dirty")
            (aob-gc--git
             dir '("ls-files" "--others" "--ignored" "--exclude-standard" "-z")
             (lambda (ignored icode)
               (let ((precious (seq-filter #'aob-gc--precious-p
                                           (split-string ignored "\0" t))))
                 (funcall callback
                          (cond ((not (zerop icode)) "dirty")
                                (precious
                                 (format "ignored files worth keeping: %s"
                                         (string-join precious ", ")))))))))))))))

(defun aob-gc--locked (project fn &optional on-error)
  "Call FN with a release function if PROJECT's repository is free, else with nil.
An error in a step frees the lock and goes to ON-ERROR, or is signalled."
  (aob-gc--git
   project '("rev-parse" "--path-format=absolute" "--git-common-dir")
   (lambda (out code)
     (let ((key (string-trim out)))
       (if (or (not (zerop code)) (gethash key aob-gc--busy))
           (funcall fn nil)
         (let* ((token (list key))
                (release (lambda ()
                           (when (eq (gethash key aob-gc--busy) token)
                             (remhash key aob-gc--busy)))))
           (puthash key token aob-gc--busy)
           (remhash project aob-gc--prs)
           (let ((aob-gc--guard (lambda (err)
                                  (funcall release)
                                  (if on-error
                                      (funcall on-error err)
                                    (signal (car err) (cdr err))))))
             (funcall (aob-gc--guarded fn) release))))))))

(defun aob-gc--classify (project targets candidates here except done)
  "Call DONE with CANDIDATES of PROJECT marked with :skip and why they stay.
A candidate is (:dir DIR :branch BRANCH); kept ones carry :skip, the rest the
:sha their branch is at.  TARGETS are what merged means; HERE's worktree and
EXCEPT's sessions are spared."
  (let (items)
    (aob-gc--each
     candidates
     (lambda (c next)
       (let* ((dir (plist-get c :dir))
              (branch (plist-get c :branch))
              (keep (lambda (why)
                      (push (append c (list :skip why)) items)
                      (funcall next)))
              (check-merged
               (lambda ()
                 (aob-gc--tip
                  project branch
                  (lambda (sha)
                    (if (not sha)
                        (funcall keep "no branch")
                      (aob-gc--merged-into
                       project branch sha targets
                       (lambda (merged)
                         (if merged
                             (progn (push (append c (list :sha sha
                                                          :saved (and dir (length (aob-gc--saved dir)))))
                                          items)
                                    (funcall next))
                           (funcall keep "not merged"))))))))))
         (cond
          ((and dir (aob-gc--in-use-p dir except)) (funcall keep "in use"))
          ((and dir (aob-gc--visited-p dir)) (funcall keep "open in Emacs"))
          ((and dir here (file-in-directory-p here dir)) (funcall keep "current"))
          (dir (aob-gc--tree-verdict dir branch
                                     (lambda (why)
                                       (if why (funcall keep why) (funcall check-merged)))))
          (t (funcall check-merged)))))
     (lambda () (funcall done (nreverse items))))))

(defun aob-gc--candidates (wts branches default mode)
  "The worktrees and branches in WTS and BRANCHES a MODE sweep may remove."
  (let* ((root (file-name-as-directory (file-truename aob-acp-worktree-root)))
         (local (plist-get default :local))
         (checked (delq nil (mapcar #'cdr wts)))
         (aob (lambda (b) (string-prefix-p "aob/" b)))
         (wanted (lambda (b) (and (not (equal b local))
                                  (or (memq mode '(project all-merged))
                                      (funcall aob b)))))
         found)
    (dolist (w (cdr wts))
      (when (and (cdr w) (not (equal (cdr w) local))
                 (or (eq mode 'project)
                     (and (funcall aob (cdr w))
                          (file-in-directory-p (file-truename (car w)) root))))
        (push (list :dir (car w) :branch (cdr w) :main (caar wts)) found)))
    (dolist (b branches)
      (when (and (not (member b checked)) (funcall wanted b))
        (push (list :branch b) found)))
    (nreverse found)))

(defun aob-gc--fetch (project callback)
  "Fetch and prune PROJECT without prompting, then CALLBACK; kill a slow one."
  (let* ((ssh (or (getenv "GIT_SSH_COMMAND") "ssh"))
         (process-environment (cons (concat "GIT_SSH_COMMAND=" ssh " -o BatchMode=yes")
                                    process-environment))
         (ended nil)
         (proc (aob-gc--git project '("fetch" "--prune" "--quiet")
                            (lambda (&rest _) (setq ended t) (funcall callback)))))
    (when (and (not ended) (processp proc))
      (run-at-time aob-gc-fetch-timeout nil
                   (lambda () (when (process-live-p proc) (kill-process proc)))))))

(defun aob-gc--plan (project mode here done &optional offline)
  "Call DONE with what a MODE sweep of PROJECT would remove or keep, and why.
MODE is aob, all-merged or project; the worktree holding HERE is spared.
OFFLINE skips the fetch."
  (let ((go
         (lambda ()
           (aob-gc--default
            project
            (lambda (default)
              (if (not default)
                  (funcall done nil)
                (aob-gc--git
                 project '("worktree" "list" "--porcelain")
                 (lambda (wout wcode)
                   (aob-gc--git
                    project '("for-each-ref" "--format=%(refname)" "refs/heads")
                    (lambda (bout bcode)
                      (if (not (and (zerop wcode) (zerop bcode)))
                          (funcall done nil)
                        (aob-gc--classify
                         project
                         (list (cons (plist-get default :ref) (plist-get default :sha)))
                         (aob-gc--candidates
                          (aob-acp--parse-worktrees wout)
                          (mapcar (lambda (r) (string-remove-prefix "refs/heads/" r))
                                  (split-string bout "\n" t))
                          default mode)
                         here nil done))))))))))))
    (if (and aob-gc-fetch (not offline))
        (aob-gc--fetch project go)
      (funcall go))))

(defun aob-gc--remove (project items except done &optional on-keep)
  "Re-check, then remove the unskipped ITEMS of PROJECT; DONE gets their branches.
EXCEPT's sessions do not count as use.  Each branch goes only while it still
sits at the planned :sha.  ON-KEEP gets an item whose worktree would not go
and why."
  (let (removed)
    (aob-gc--each
     (seq-remove (lambda (i) (plist-get i :skip)) items)
     (lambda (item next)
       (let* ((dir (plist-get item :dir))
              (branch (plist-get item :branch))
              (sha (plist-get item :sha))
              (fail (lambda (why)
                      (when on-keep (funcall on-keep item why))
                      (funcall next)))
              (drop (lambda (counted)
                      (aob-gc--git
                       project '("worktree" "list" "--porcelain")
                       (lambda (out code)
                         (if (or (not (zerop code))
                                 (rassoc branch (aob-acp--parse-worktrees out)))
                             (progn (when counted (push branch removed))
                                    (funcall next))
                           (aob-gc--git
                            project (list "update-ref" "-d" (concat "refs/heads/" branch) sha)
                            (lambda (_o c)
                              (when (or counted (zerop c)) (push branch removed))
                              (funcall next))))))))
              (unplug (lambda ()
                        (aob-gc--tree-verdict
                         dir branch
                         (lambda (why)
                           (if why
                               (funcall next)
                             (aob-gc--git
                              project '("worktree" "list" "--porcelain")
                              (lambda (lout lcode)
                                (let ((main (caar (aob-acp--parse-worktrees lout)))
                                      (ids (aob-gc--saved-ids dir)))
                                  (if (not (and (zerop lcode) main))
                                      (funcall fail "main checkout not found")
                                    (aob-gc--git
                                     project (list "worktree" "remove" "--force" dir)
                                     (lambda (rout c)
                                       (if (not (zerop c))
                                           (funcall fail (aob-gc--first-line rout))
                                         (aob-gc--rehome ids dir main)
                                         (funcall drop t))))))))))))))
         (if (not sha)
             (funcall next)
           (aob-gc--tip
            project branch
            (lambda (tip)
              (cond ((not (equal tip sha)) (funcall next))
                    ((not dir) (funcall drop nil))
                    ((or (aob-gc--in-use-p dir except) (aob-gc--visited-p dir))
                     (funcall next))
                    (t (funcall unplug))))))))
     (lambda () (funcall done (nreverse removed))))))

(defun aob-gc-reap (project dir branch session)
  "Remove DIR and BRANCH of PROJECT once SESSION is done, if merged and unused.
Merged means into the default branch or PROJECT's checked-out HEAD."
  (aob-gc--locked
   project
   (lambda (release)
     (if (not release)
         (message "aob: kept %s - a sweep is running" dir)
       (aob-gc--targets
        project t
        (lambda (targets)
          (aob-gc--classify
           project targets (list (list :dir dir :branch branch)) nil session
           (lambda (items)
             (if (plist-get (car items) :skip)
                 (progn (funcall release)
                        (message "aob: kept %s - %s" dir (plist-get (car items) :skip)))
               (aob-gc--remove project items session (lambda (_) (funcall release))))))))))))

(defun aob-gc--projects ()
  "Existing local project directories aob sessions, live or saved, know about."
  (let (seen found)
    (dolist (p (append (mapcar #'aob-session-project (aob-sessions))
                       (mapcar (lambda (e) (plist-get e :project))
                               (aob-acp--persisted-entries))))
      (when (and (stringp p) (not (file-remote-p p)) (file-directory-p p))
        (let ((true (file-truename p)))
          (unless (member true seen)
            (push true seen)
            (push p found)))))
    (nreverse found)))

(defun aob-gc--sweep (mode dry done)
  "Plan, and unless DRY remove, MODE across every known project.
DONE gets (PROJECT . ITEMS) pairs; removed items carry no :skip, kept ones
:skip and removals git refused :failed, and a project that errored has one
item with :error."
  (let (results)
    (aob-gc--each
     (aob-gc--projects)
     (lambda (project next)
       (let* ((settled nil)
              (settle (lambda (items)
                        (setq settled t)
                        (push (cons project items) results)
                        (funcall next))))
         (aob-gc--locked
          project
          (lambda (release)
            (if (not release)
                (funcall next)
              (aob-gc--plan
               project mode nil
               (lambda (items)
                 (let ((finish (lambda (items)
                                 (funcall release)
                                 (funcall settle items))))
                   (if dry
                       (funcall finish items)
                     (let (kept)
                       (aob-gc--remove
                        project items nil
                        (lambda (removed)
                          (funcall finish
                                   (mapcar (lambda (i)
                                             (if-let* ((why (cdr (assq i kept))))
                                                 (append i (list :skip why :failed t))
                                               i))
                                           (seq-filter
                                            (lambda (i)
                                              (or (plist-get i :skip)
                                                  (assq i kept)
                                                  (member (plist-get i :branch) removed)))
                                            items))))
                        (lambda (item why) (push (cons item why) kept)))))))
               dry)))
          (lambda (err)
            (if settled
                (signal (car err) (cdr err))
              (funcall settle (list (list :error (error-message-string err)))))))))
     (lambda () (funcall done (nreverse results))))))

(defun aob-gc--label (item)
  "ITEM as one line naming its worktree and branch."
  (if-let* ((dir (plist-get item :dir)))
      (format "%s (%s)" (abbreviate-file-name dir) (plist-get item :branch))
    (format "branch %s" (plist-get item :branch))))

(defun aob-gc--moves (item)
  "Text saying how many saved conversations ITEM's removal moves, and where."
  (let ((n (or (plist-get item :saved) 0)))
    (if (and (> n 0) (plist-get item :main))
        (format " - %d saved conversation%s move to %s" n (if (= n 1) "" "s")
                (abbreviate-file-name (plist-get item :main)))
      "")))

(defun aob-gc--show (results heading)
  "List RESULTS under HEADING in the *aob-gc* buffer, kept items with why."
  (with-current-buffer (get-buffer-create "*aob-gc*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert heading "\n")
      (dolist (r results)
        (insert "\n" (abbreviate-file-name (car r)) "\n")
        (dolist (i (cdr r))
          (cond ((plist-get i :error) (insert "  failed  " (plist-get i :error) "\n"))
                ((not (plist-get i :skip))
                 (insert "  remove  " (aob-gc--label i) (aob-gc--moves i) "\n"))
                ((not (equal (plist-get i :skip) "not merged"))
                 (insert "  keep    " (aob-gc--label i) " - " (plist-get i :skip) "\n")))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

(defun aob-gc--sweep-mode ()
  "The sweep mode `aob-gc-branches' asks for."
  (if (eq aob-gc-branches 'all-merged) 'all-merged 'aob))

(defun aob-gc--removed (results)
  "Labels of the items in RESULTS that are being or were removed."
  (mapcar #'aob-gc--label
          (seq-remove (lambda (i) (plist-get i :skip)) (apply #'append (mapcar #'cdr results)))))

(defun aob-gc--refused (results)
  "Lines for the removals RESULTS shows git refused and the projects that failed."
  (let (lines)
    (dolist (r results)
      (dolist (i (cdr r))
        (cond ((plist-get i :error)
               (push (format "failed: %s: %s" (abbreviate-file-name (car r)) (plist-get i :error))
                     lines))
              ((plist-get i :failed)
               (push (format "kept %s - %s" (aob-gc--label i) (plist-get i :skip)) lines)))))
    (nreverse lines)))

(defun aob-gc--run (report)
  "Sweep now; one message says what went, and with REPORT also when nothing did."
  (aob-gc--sweep (aob-gc--sweep-mode) nil
                 (lambda (results)
                   (let ((gone (aob-gc--removed results))
                         (refused (aob-gc--refused results)))
                     (cond ((or gone refused)
                            (message "aob-gc: %s"
                                     (string-join
                                      (append (and gone (list (concat "removed " (string-join gone ", "))))
                                              refused)
                                      "; ")))
                           (report (message "aob-gc: nothing to remove")))))))

;;;###autoload
(defun aob-gc-now ()
  "Remove merged, clean, unoccupied agent worktrees and branches now."
  (interactive)
  (aob-gc--run t))

;;;###autoload
(defun aob-gc-preview ()
  "List what `aob-gc-now' would remove and what it keeps; remove nothing."
  (interactive)
  (aob-gc--sweep (aob-gc--sweep-mode) t
                 (lambda (results)
                   (if (aob-gc--removed results)
                       (aob-gc--show results "aob-gc preview: nothing is removed")
                     (message "aob-gc: nothing to remove")))))

(defun aob-gc--project-root ()
  "The repository root the current buffer is in."
  (or (when-let* ((p (project-current))) (project-root p))
      (locate-dominating-file default-directory ".git")
      (user-error "aob-gc: not in a project")))

;;;###autoload
(defun aob-gc-project ()
  "Delete this project's merged branches and their clean worktrees, after one y/n."
  (interactive)
  (let ((project (aob-gc--project-root))
        (here default-directory))
    (aob-gc--locked
     project
     (lambda (release)
       (if (not release)
           (message "aob-gc: a sweep is already running here")
         (aob-gc--plan
          project 'project here
          (lambda (items)
            (funcall release)
            (let* ((go (seq-remove (lambda (i) (plist-get i :skip)) items))
                   (trees (seq-count (lambda (i) (plist-get i :dir)) go)))
              (if (null go)
                  (aob-gc--show (list (cons project items)) "aob-gc: nothing merged to remove")
                (aob-gc--show (list (cons project items)) "aob-gc: merged branches")
                (run-at-time
                 0 nil
                 (lambda ()
                   (if (condition-case nil
                           (y-or-n-p (format "Remove %d merged branch%s and %d worktree%s? "
                                             (length go) (if (= (length go) 1) "" "es")
                                             trees (if (= trees 1) "" "s")))
                         (quit nil))
                       (aob-gc--locked
                        project
                        (lambda (again)
                          (if (not again)
                              (message "aob-gc: a sweep is already running here")
                            (let (refused)
                              (aob-gc--remove
                               project items nil
                               (lambda (removed)
                                 (funcall again)
                                 (message "aob-gc: %s"
                                          (string-join
                                           (cons (concat "removed " (string-join removed ", "))
                                                 (nreverse refused))
                                           "; ")))
                               (lambda (item why)
                                 (push (format "kept %s - %s" (aob-gc--label item) why)
                                       refused)))))))
                     (message "aob-gc: kept everything")))))))))))))

(defun aob-gc--arm (delay)
  "Start the repeating sweep timer, first firing after DELAY seconds."
  (when aob-gc--timer (cancel-timer aob-gc--timer))
  (setq aob-gc--last-tick (float-time)
        aob-gc--expected delay)
  (setq aob-gc--timer (run-with-timer delay aob-gc-interval #'aob-gc--tick)))

(defun aob-gc--when-idle ()
  "Sweep now if the mode is still on."
  (when aob-gc-mode (aob-gc--run nil)))

(defun aob-gc--tick ()
  "Sweep when Emacs is idle, or re-arm the grace after a long sleep."
  (let ((gap (- (float-time) aob-gc--last-tick))
        (expected (or aob-gc--expected aob-gc-interval)))
    (setq aob-gc--last-tick (float-time)
          aob-gc--expected aob-gc-interval)
    (cond ((> gap (+ expected aob-gc-interval)) (aob-gc--arm aob-gc-grace))
          ((current-idle-time) (aob-gc--run nil))
          ((not (and aob-gc--idle (memq aob-gc--idle timer-idle-list)))
           (setq aob-gc--idle (run-with-idle-timer 5 nil #'aob-gc--when-idle))))))

;;;###autoload
(define-minor-mode aob-gc-mode
  "Sweep merged agent worktrees and branches in the background."
  :global t
  :group 'aob-gc
  (when aob-gc--timer
    (cancel-timer aob-gc--timer)
    (setq aob-gc--timer nil))
  (when aob-gc--idle
    (cancel-timer aob-gc--idle)
    (setq aob-gc--idle nil))
  (when aob-gc-mode
    (aob-gc--arm aob-gc-grace)))

(provide 'aob-gc)
;;; aob-gc.el ends here
