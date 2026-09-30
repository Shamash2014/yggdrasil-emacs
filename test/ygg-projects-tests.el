;;; ygg-projects-tests.el --- Tests for the projects sidebar rows -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ygg-projects)

(defvar ygg-projects-tests--state 'working
  "The state the stand-in session reports.")

(defvar ygg-projects-tests--acp-id nil
  "The conversation id the stand-in session reports.")

(defmacro ygg-projects-tests--with-session (clock spend progress &rest body)
  "Run BODY with the symbol session standing for an agent session.
It reports CLOCK, SPEND and todo PROGRESS as a (DONE . TOTAL) cons, and
ygg-projects-tests--state as its state."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'aob-session-p) (lambda (s) (eq s 'session)))
             ((symbol-value 'ygg-projects--pin-list) nil)
             ((symbol-function 'aob-session-state) (lambda (_) ygg-projects-tests--state))
             ((symbol-function 'aob-session-ref)
              (lambda (_ key) (and (eq key :acp-id) ygg-projects-tests--acp-id)))
             ((symbol-function 'aob-session-clock) (lambda (_) ,clock))
             ((symbol-function 'aob-session-quiet) #'ignore)
             ((symbol-function 'aob-session-spend) (lambda (_) ,spend))
             ((symbol-function 'ygg-todo-session-file) (lambda (_) "todo"))
             ((symbol-function 'ygg-todo-progress) (lambda (_) ,progress)))
     ,@body))

(defun ygg-projects-tests--row (label)
  "LABEL's row for the session, as plain text with its alignment spaces gone."
  (let ((row (ygg-projects--entry-text label "/tmp/p/" 'agents 'session)))
    (should-not (string-search "\n" row))
    (let ((i 0) (out ""))
      (while (< i (length row))
        (let ((next (next-single-property-change i 'display row (length row))))
          (unless (get-text-property i 'display row)
            (setq out (concat out (substring-no-properties row i next))))
          (setq i next)))
      out)))

(defun ygg-projects-tests--fits (text)
  "Non-nil when TEXT leaves at least a column for the gap the badge sits after."
  (< (string-width text) (ygg-projects--width)))

(ert-deftest ygg-projects-row-keeps-a-short-name-and-the-whole-meter ()
  "A short name and a full meter share one row at the stock width."
  (let ((ygg-projects-width 40))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "claude:9" row))
        (should (string-search "3h15m… $160.85 18/20" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-sheds-spend-before-cutting-a-name-short ()
  "A long name costs the meter its spend, then is cut, never wrapped."
  (let ((ygg-projects-width 40))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "a-conversation-with-a-very-long-name")))
        (should-not (string-search "$160.85" row))
        (should (string-search "3h15m… 18/20" row))
        (should (string-search "…" (car (split-string row "3h15m"))))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-keeps-progress-last-when-narrow ()
  "In a narrow sidebar the progress is the part of the meter that stays."
  (let ((ygg-projects-width 30))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "claude:9" row))
        (should (string-search "18/20" row))
        (should-not (string-search "3h15m" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-keeps-a-waiting-state-last ()
  "A session waiting on you keeps saying so when the meter has to shrink."
  (let ((ygg-projects-width 34)
        (ygg-projects-tests--state 'blocked))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "blocked" row))
        (should-not (string-search "$160.85" row))
        (should-not (string-search "3h15m" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-keeps-the-end-of-a-long-path ()
  "A folder too long for its row loses its head and keeps its own name."
  (let* ((ygg-projects-width 40)
         (label "~/work/clients/some-client/repositories/the-folder-name")
         (row (ygg-projects--entry-text label "/tmp/p/" 'folders "/tmp/x/")))
    (should-not (string-search "\n" row))
    (should (string-search "…" row))
    (should (string-search "the-folder-name" row))
    (should (< (string-width (substring-no-properties row)) (+ 2 (ygg-projects--width))))))

(defun ygg-projects-tests--schedule (target minutes &rest more)
  "A schedule for TARGET due in MINUTES, with MORE of its plist."
  (append (list :id minutes :prompt "hi" :target target :when nil
                :next (+ (float-time) (* 60 minutes) 30))
          more))

(ert-deftest ygg-projects-row-marks-the-soonest-schedule ()
  "A scheduled conversation says so, with when the soonest one runs."
  (require 'aob-schedule)
  (let ((ygg-projects-width 40)
        (ygg-projects-tests--acp-id "sched-acp")
        (aob-schedule--list
         (list (ygg-projects-tests--schedule '(:acp-id "sched-acp") 45)
               (ygg-projects-tests--schedule '(:acp-id "sched-acp") 20)
               (ygg-projects-tests--schedule '(:acp-id "elsewhere") 5))))
    (ygg-projects-tests--with-session nil nil '(1 . 2)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "◷ 20m" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-keeps-the-schedule-over-spend ()
  (require 'aob-schedule)
  (let ((ygg-projects-width 40)
        (ygg-projects-tests--acp-id "sched-acp")
        (aob-schedule--list
         (list (ygg-projects-tests--schedule '(:acp-id "sched-acp") 20))))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "◷ 20m 3h15m… 18/20" row))
        (should-not (string-search "$160.85" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-row-marks-a-paused-schedule-apart ()
  (require 'aob-schedule)
  (let ((ygg-projects-width 40)
        (ygg-projects-tests--acp-id "sched-acp")
        (aob-schedule--list
         (list (ygg-projects-tests--schedule '(:acp-id "sched-acp") 20 :paused t))))
    (ygg-projects-tests--with-session nil nil '(1 . 2)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "⏸ 20m" row))
        (should-not (string-search "◷" row))))))

(ert-deftest ygg-projects-row-sheds-the-schedule-after-spend ()
  "A row short of room gives up its spend, then its schedule mark, then its clock."
  (require 'aob-schedule)
  (let ((ygg-projects-width 30)
        (ygg-projects-tests--acp-id "sched-acp")
        (aob-schedule--list
         (list (ygg-projects-tests--schedule '(:acp-id "sched-acp") 20))))
    (ygg-projects-tests--with-session "3h15m…" "$160.85" '(18 . 20)
      (let ((row (ygg-projects-tests--row "claude:9")))
        (should (string-search "claude:9" row))
        (should (string-search "18/20" row))
        (should-not (string-search "◷" row))
        (should (ygg-projects-tests--fits row))))))

(ert-deftest ygg-projects-ended-row-marks-its-schedule-only-when-it-fits ()
  (require 'aob-schedule)
  (let ((entry (list :acp-id "sched-acp" :name "old" :ts (- (float-time) 7200)))
        (aob-schedule--list
         (list (ygg-projects-tests--schedule '(:acp-id "sched-acp") 20)))
        (ygg-projects--pin-list nil))
    (let ((ygg-projects-width 40))
      (should (string-search "◷ 20m 2h" (ygg-projects--entry-text "old" "/tmp/p/" 'agents entry))))
    (let* ((ygg-projects-width 24)
           (row (ygg-projects--entry-text "a-long-conversation-name" "/tmp/p/" 'agents entry)))
      (should-not (string-search "◷" row))
      (should (string-search "a-long-c" row)))))

(ert-deftest ygg-projects-head-counts-the-project-schedules ()
  "A project's head counts its schedules, a new session's among them,
and leaves those of a project nested in it to that project."
  (require 'aob-schedule)
  (let* ((tmp (file-name-as-directory (file-truename temporary-file-directory)))
         (proj (concat tmp "proj/"))
         (child (concat proj "child/"))
         (ygg-projects-width 40)
         (aob-schedule--list
          (list (ygg-projects-tests--schedule (list :acp-id "a" :project proj) 20)
                (ygg-projects-tests--schedule
                 (list :agent "claude" :project (directory-file-name proj)) 30)
                (ygg-projects-tests--schedule (list :agent "claude" :project child) 30)
                (ygg-projects-tests--schedule (list :agent "claude" :project (concat tmp "other/")) 30))))
    (cl-letf (((symbol-function 'ygg-projects--sessions) #'ignore)
              ((symbol-function 'ygg-projects--roots) (lambda () (list proj child))))
      (should (string-search "◷ 2" (ygg-projects--head-text proj)))
      (should (string-search "◷ 1" (ygg-projects--head-text child)))
      (should-not (string-search "◷" (ygg-projects--head-text (concat tmp "none/")))))))

(defconst ygg-projects-tests--porcelain
  (concat "worktree /r/main\nHEAD 1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\nbranch refs/heads/master\n\n"
          "worktree /r/main/.trees/feat\nHEAD 2222222bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\nbranch refs/heads/feat/x\n\n"
          "worktree /r/spike\nHEAD abcdef0123456789abcdef0123456789abcdef01\ndetached\n")
  "What git worktree list --porcelain says of a main checkout, a linked
worktree nested inside it, and a detached one beside it.")

(ert-deftest ygg-projects-tree-note-leaves-the-main-checkout-bare ()
  "A session in the main checkout, or in a repository of one worktree, gets no line."
  (should-not (ygg-projects--tree-note ygg-projects-tests--porcelain "/r/main/lisp"))
  (should-not (ygg-projects--tree-note
               "worktree /r/main\nHEAD 1111111aaaa\nbranch refs/heads/master\n" "/r/main"))
  (should-not (ygg-projects--tree-note ygg-projects-tests--porcelain "/elsewhere")))

(ert-deftest ygg-projects-tree-note-names-a-linked-worktree-and-its-branch ()
  "The deepest worktree holding the folder wins, named by folder and branch."
  (should (equal (ygg-projects--tree-note ygg-projects-tests--porcelain "/r/main/.trees/feat/src")
                 "⌥ feat · feat/x")))

(ert-deftest ygg-projects-tree-note-gives-a-detached-worktree-its-short-sha ()
  (should (equal (ygg-projects--tree-note ygg-projects-tests--porcelain "/r/spike")
                 "⌥ spike · abcdef0")))

(defmacro ygg-projects-tests--with-git (calls reply &rest body)
  "Run BODY with the worktree cache empty and git a stand-in.
Each lookup is counted in CALLS and its callback kept in REPLY."
  (declare (indent 2))
  `(let ((ygg-projects--tree-notes (make-hash-table :test #'equal))
         (ygg-projects--tree-notes-pending (make-hash-table :test #'equal))
         (,calls 0) (,reply nil))
     (cl-letf (((symbol-function 'ygg-git-async)
                (lambda (_root _args callback) (cl-incf ,calls) (setq ,reply callback) t))
               ((symbol-function 'ygg-projects-refresh) #'ignore))
       ,@body)))

(ert-deftest ygg-projects-session-tree-asks-git-once-per-folder ()
  "A redraw reads the cache; git is asked again only once the answer is old."
  (let ((dir (file-name-as-directory (file-truename temporary-file-directory))))
    (ygg-projects-tests--with-git calls reply
      (should-not (ygg-projects--session-tree dir))
      (should-not (ygg-projects--session-tree dir))
      (should (= calls 1))
      (funcall reply (concat "worktree /r/main\n\nworktree " dir "\nbranch refs/heads/wip\n") 0)
      (should (equal (ygg-projects--session-tree dir)
                     (format "⌥ %s · wip" (file-name-nondirectory (directory-file-name dir)))))
      (should (= calls 1))
      (setcar (gethash dir ygg-projects--tree-notes)
              (- (float-time) ygg-projects--tree-note-ttl 1))
      (ygg-projects--session-tree dir)
      (should (= calls 2)))))

(ert-deftest ygg-projects-session-tree-leaves-a-missing-folder-alone ()
  (ygg-projects-tests--with-git calls _reply
    (should-not (ygg-projects--session-tree "/no/such/folder/"))
    (should-not (ygg-projects--session-tree nil))
    (should (= calls 0))))

(ert-deftest ygg-projects-tree-text-is-one-grey-line-point-passes-over ()
  (let* ((ygg-projects-width 40)
         (text (ygg-projects--tree-text "⌥ feat · feat/x" "/tmp/p/" 'agents 'session)))
    (should-not (string-search "\n" text))
    (should (string-search "⌥ feat · feat/x" text))
    (should (eq (get-text-property (string-search "⌥" text) 'font-lock-face text)
                'ygg-projects-count))
    (should (get-text-property 0 'ygg-cont text))
    (should (eq (get-text-property 0 'ygg-entry text) 'session))))

(ert-deftest ygg-projects-rows-hold-their-place-while-sessions-stream ()
  "Rows sort by when a session started, so a new event moves nothing."
  (require 'aob-subagent)
  (let ((ygg-projects-show-past nil) (ygg-projects-show-subagents t)
        (ygg-projects--pin-list nil)
        (root (file-name-as-directory (file-truename temporary-file-directory)))
        made)
    (unwind-protect
        (let ((old (aob-create-session :id "p-old" :backend 'acp :name "old"
                                       :project root :state 'working
                                       :started (time-subtract nil 60)))
              (new (aob-create-session :id "p-new" :backend 'acp :name "new"
                                       :project root :state 'working)))
          (setq made (list old new))
          (cl-letf (((symbol-function 'ygg-projects--roots) (lambda () (list root)))
                    ((symbol-function 'ygg-projects--past) #'ignore))
            (should (equal (mapcar #'car (ygg-projects--entries root 'agents)) '("new" "old")))
            (aob-event old 'message :title "streaming")
            (should (equal (mapcar #'car (ygg-projects--entries root 'agents)) '("new" "old")))))
      (mapc #'aob-remove-session made))))

(ert-deftest ygg-projects-subagent-row-lives-as-long-as-its-work ()
  "A running subagent is a row under its sender and counted once; done,
failed or killed it is neither."
  (require 'aob-subagent)
  (let ((ygg-projects-show-past nil) (ygg-projects-show-subagents t)
        (root (file-name-as-directory (file-truename temporary-file-directory))))
    (dolist (end '(done failed killed))
      (let* ((parent (aob-create-session :id "p" :backend 'acp :name "lead"
                                         :project root :state 'working))
             (kid (aob-create-session :id "p/k" :backend 'native-subagent :name "scout"
                                      :project root :state 'working
                                      :refs (list :parent-session "p"))))
        (unwind-protect
            (cl-letf (((symbol-function 'ygg-projects--roots) (lambda () (list root)))
                      ((symbol-function 'ygg-projects--past) #'ignore))
              (should (equal (mapcar #'car (ygg-projects--entries root 'agents))
                             '("lead" "└ scout")))
              (should (equal (ygg-projects--agents root) '(2 . 2)))
              (if (eq end 'killed)
                  (aob-remove-session kid)
                (aob-set-state kid end))
              (should (equal (mapcar #'car (ygg-projects--entries root 'agents))
                             '("lead")))
              (should (equal (ygg-projects--agents root) '(1 . 1))))
          (ignore-errors (aob-remove-session kid))
          (aob-remove-session parent))))))

(ert-deftest ygg-projects-subagent-row-shows-its-plan-progress ()
  "A subagent keeps no list; its row counts its own plan instead."
  (require 'aob-subagent)
  (let* ((root (file-name-as-directory (file-truename temporary-file-directory)))
         (parent (aob-create-session :id "p" :backend 'acp :name "lead"
                                     :project root :state 'working))
         (kid (aob-create-session :id "p/k" :backend 'native-subagent :name "scout"
                                  :project root :state 'working
                                  :refs (list :parent-session "p"))))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-todo-session-file) #'ignore)
                  ((symbol-function 'aob-session-clock) #'ignore)
                  ((symbol-function 'aob-session-spend) #'ignore)
                  ((symbol-function 'aob-session-quiet) #'ignore))
          (should-not (string-search "/" (ygg-projects--badge kid 40)))
          (aob-session-put kid :plan-progress '(2 . 5))
          (should (equal (substring-no-properties (ygg-projects--badge kid 40))
                         "2/5 working"))
          (should-not (string-search "2/5" (ygg-projects--badge parent 40))))
      (mapc #'aob-remove-session (list kid parent)))))

(ert-deftest ygg-projects-subagent-row-says-its-kind-underneath ()
  (require 'aob-subagent)
  (let* ((root (file-name-as-directory (file-truename temporary-file-directory)))
         (parent (aob-create-session :id "p" :backend 'acp :name "lead"
                                     :project root :state 'working
                                     :refs (list :agent "claude")))
         (typed (aob-create-session :id "p/t" :backend 'native-subagent :name "scout"
                                    :project root :state 'working
                                    :refs (list :parent-session "p" :agent "claude")))
         (plain (aob-create-session :id "p/u" :backend 'native-subagent :name "helper"
                                    :project root :state 'working
                                    :refs (list :parent-session "p" :agent "claude"))))
    (unwind-protect
        (progn
          (aob-subagent--native-sync
           typed '(:title "scout" :status "in_progress"
                   :raw (:description "scout" :subagent_type "Explore")))
          (should (equal (ygg-projects--entry-tree typed) "claude · Explore"))
          (should (equal (ygg-projects--entry-tree plain) "claude"))
          (should (equal (ygg-projects--entry-tree parent) "claude · ▸ 2")))
      (mapc #'aob-remove-session (list typed plain parent)))))

;;; Acting on a visual selection of rows

(defvar ygg-projects-tests--calls nil
  "What the stand-in backend was asked to do, newest first.")

(defmacro ygg-projects-tests--with-rows (&rest body)
  "Run BODY in a sidebar-like buffer over two sessions and a subagent.
Lines: project head, Sessions title, alpha, alpha's worktree note, beta,
beta's subagent scout.  a, b and kid are bound to the sessions."
  (declare (indent 0))
  `(progn
     (require 'aob-acp)
     (require 'aob-trace)
     (require 'aob-subagent)
     (aob-register-backend
      'ygg-projects-test
      (list :kill (lambda (s) (push (list :kill (aob-session-name s)) ygg-projects-tests--calls)
                    (aob-remove-session s))
            :cancel (lambda (s &rest _) (push (list :cancel (aob-session-name s))
                                              ygg-projects-tests--calls))
            :prompt (lambda (s text &rest _)
                      (push (list :prompt (aob-session-name s) text aob-prompt-typed)
                            ygg-projects-tests--calls))))
     (let* ((ygg-projects-tests--calls nil)
            (aob-acp-persist-file (make-temp-file "ygg-projects-tests-persist"))
            (root "/tmp/p/")
            (a (aob-create-session :id "t:alpha" :backend 'ygg-projects-test
                                   :name "alpha" :state 'working))
            (b (aob-create-session :id "t:beta" :backend 'ygg-projects-test
                                   :name "beta" :state 'idle))
            (kid (aob-create-session :id "t:beta/k" :backend 'ygg-projects-test
                                     :name "scout" :state 'working
                                     :refs (list :parent-session "t:beta"))))
       (unwind-protect
           (with-temp-buffer
             (dolist (line (list (propertize " ● p" 'ygg-project root 'ygg-row 'project)
                                 (propertize "  Sessions" 'ygg-project root 'ygg-row 'agents)
                                 (propertize "  · alpha" 'ygg-project root 'ygg-row 'agents
                                             'ygg-entry a)
                                 (propertize "    ⌥ feat · feat/x" 'ygg-project root
                                             'ygg-row 'agents 'ygg-entry a 'ygg-cont t)
                                 (propertize "  · beta" 'ygg-project root 'ygg-row 'agents
                                             'ygg-entry b)
                                 (propertize "  · └ scout" 'ygg-project root 'ygg-row 'agents
                                             'ygg-entry kid)))
               (insert line "\n"))
             (setq-local ygg--visual-p nil)
             (cl-letf (((symbol-function 'ygg-normal-state)
                        (lambda () (setq ygg--visual-p nil))))
               ,@body))
         (dolist (s (list kid a b))
           (when (aob-session-get (aob-session-id s)) (aob-remove-session s)))
         (delete-file aob-acp-persist-file)))))

(defun ygg-projects-tests--select (from to)
  "Visual state from line FROM to line TO, both counted from 1."
  (goto-char (point-min))
  (forward-line (1- from))
  (set-mark (point))
  (goto-char (point-min))
  (forward-line (1- to))
  (setq ygg--visual-p t))

(ert-deftest ygg-projects-selection-keeps-only-session-rows ()
  "Title, session, worktree note, session: the two sessions, once each."
  (ygg-projects-tests--with-rows
    (ygg-projects-tests--select 2 5)
    (should (equal (ygg-projects--selected-entries) (list a b)))
    (ygg-projects-tests--select 5 1)
    (should (equal (ygg-projects--selected-entries) (list a b)))))

(ert-deftest ygg-projects-kill-selection-asks-once-and-ends-both ()
  (ygg-projects-tests--with-rows
    (ygg-projects-tests--select 3 5)
    (let ((asked nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (push prompt asked) t)))
        (ygg-projects-archive-ask))
      (should (= (length asked) 1))
      (should (string-search "alpha, beta" (car asked))))
    (should (equal (reverse ygg-projects-tests--calls)
                   '((:kill "alpha") (:kill "beta"))))
    (should-not (aob-session-get "t:alpha"))
    (should-not (aob-session-get "t:beta"))
    (should-not ygg--visual-p)
    (should (= (line-number-at-pos) 3))))

(ert-deftest ygg-projects-delete-selection-asks-once ()
  (ygg-projects-tests--with-rows
    (ygg-projects-tests--select 2 5)
    (let ((asked 0))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (cl-incf asked) t)))
        (ygg-projects-delete))
      (should (= asked 1)))
    (should-not (aob-session-get "t:alpha"))
    (should-not (aob-session-get "t:beta"))))

(ert-deftest ygg-projects-say-sends-one-text-to-each ()
  (ygg-projects-tests--with-rows
    (ygg-projects-tests--select 3 5)
    (let ((read 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (cl-incf read) "carry on")))
        (call-interactively #'ygg-projects-say))
      (should (= read 1)))
    (should (equal (reverse ygg-projects-tests--calls)
                   '((:prompt "alpha" "carry on" t) (:prompt "beta" "carry on" t))))
    (should-not ygg--visual-p)))

(ert-deftest ygg-projects-selection-skips-a-subagent-for-writes ()
  (ygg-projects-tests--with-rows
    (ygg-projects-tests--select 5 6)
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (ygg-projects-cancel))
      (should (string-search "1 subagent skipped" said)))
    (should (equal ygg-projects-tests--calls '((:cancel "beta"))))
    (setq ygg-projects-tests--calls nil)
    (ygg-projects-tests--select 6 6)
    (should-error (ygg-projects-cancel) :type 'user-error)
    (should-not ygg-projects-tests--calls)
    (should (aob-session-get "t:beta/k"))))

(ert-deftest ygg-projects-visit-in-visual-takes-the-row-at-point ()
  "Opening a trace is one session's act: the row at point, never the range."
  (ygg-projects-tests--with-rows
    (ygg-projects-tests--select 3 5)
    (let ((opened nil))
      (cl-letf (((symbol-function 'ygg-aob-goto-space) (lambda (s) (push s opened)))
                ((symbol-function 'aob-trace) (lambda (s) (push s opened))))
        (ygg-projects-visit))
      (should-not ygg--visual-p)
      (should (memq b opened))
      (should-not (memq a opened)))))

;;; A session in a linked worktree

(defmacro ygg-projects-tests--with-trees (roots sessions &rest body)
  "Run BODY with ROOTS on show and SESSIONS as (ID DIR MAIN) each.
MAIN is the main checkout git named for DIR, nil for a main checkout or
a folder outside git; the lookup is seeded, so no git runs."
  (declare (indent 2))
  `(progn
     (require 'aob-subagent)
     (let ((ygg-projects-show-past nil) (ygg-projects-show-subagents t)
           (ygg-projects--tree-notes (make-hash-table :test #'equal))
           (ygg-projects--tree-mains (make-hash-table :test #'equal))
           (made nil))
       (pcase-dolist (`(,id ,dir ,main) ,sessions)
         (puthash dir (cons (float-time) (and main "⌥ wt · feat"))
                  ygg-projects--tree-notes)
         (when main (puthash dir main ygg-projects--tree-mains))
         (push (aob-create-session :id id :backend 'acp :name id
                                   :project dir :dir dir :state 'working)
               made))
       (unwind-protect
           (cl-letf (((symbol-function 'ygg-projects--roots) (lambda () ,roots))
                     ((symbol-function 'ygg-projects--past) #'ignore)
                     ((symbol-function 'ygg-git-async)
                      (lambda (&rest _) (error "git must not run"))))
             ,@body)
         (mapc #'aob-remove-session made)))))

(defun ygg-projects-tests--names (root)
  (mapcar #'car (ygg-projects--entries root 'agents)))

(ert-deftest ygg-projects-linked-worktree-session-sits-under-its-main-project ()
  (ygg-projects-tests--with-trees '("/ygg-t/repo/")
      '(("feat" "/ygg-t/repo-feat/" "/ygg-t/repo/"))
    (should (equal (ygg-projects-tests--names "/ygg-t/repo/") '("feat")))
    (should (equal (ygg-projects--agents "/ygg-t/repo/") '(1 . 1)))
    (should (equal (ygg-projects--entry-tree (aob-session-get "feat"))
                   "⌥ wt · feat"))
    (aob-session-put (aob-session-get "feat") :agent "codex")
    (should (equal (ygg-projects--entry-tree (aob-session-get "feat"))
                   "codex · ⌥ wt · feat"))))

(ert-deftest ygg-projects-linked-worktree-that-is-a-project-is-listed-once ()
  "The main project has it; the worktree's own card does not repeat it."
  (ygg-projects-tests--with-trees '("/ygg-t/repo/" "/ygg-t/repo-feat/")
      '(("feat" "/ygg-t/repo-feat/" "/ygg-t/repo/"))
    (should (equal (ygg-projects-tests--names "/ygg-t/repo/") '("feat")))
    (should-not (ygg-projects--sessions "/ygg-t/repo-feat/"))
    (should (equal (ygg-projects--agents "/ygg-t/repo-feat/") '(0 . 0)))))

(ert-deftest ygg-projects-main-checkout-and-plain-folder-sessions-stay-put ()
  (ygg-projects-tests--with-trees '("/ygg-t/repo/" "/ygg-t/plain/")
      '(("main" "/ygg-t/repo/" nil) ("plain" "/ygg-t/plain/" nil))
    (should (equal (ygg-projects-tests--names "/ygg-t/repo/") '("main")))
    (should (equal (ygg-projects-tests--names "/ygg-t/plain/") '("plain")))
    (should-not (ygg-projects--entry-tree (aob-session-get "main")))))

(ert-deftest ygg-projects-main-worktree-is-the-first-listed ()
  (should (equal (ygg-projects--main-worktree
                  "worktree /r/repo\nHEAD abc\nbranch refs/heads/main\n\nworktree /r/repo-feat\nHEAD def\nbranch refs/heads/feat\n")
                 "/r/repo/")))

;;; Pinning, and the tree of what a session sent

(defvar aob-acp-persist-file)

(defvar ygg-projects-tests--ts nil
  "Each stand-in session's last word, as (ID . SECONDS).")

(defvar ygg-projects-tests--made nil
  "The sessions the tree fixture made, for a test that makes more.")

(defun ygg-projects-tests--call-with-tree (specs body)
  "Call BODY with the project root, over sessions made from SPECS.
Each spec is (ID PARENT TS); a session with no PARENT is a lead.  Pins
start empty and live in a file of their own."
  (require 'aob-subagent)
  (let* ((ygg-projects-show-past nil) (ygg-projects-show-subagents t)
         (aob-acp-persist-file nil)
         (ygg-projects-pins-file (make-temp-file "ygg-projects-pins"))
         (ygg-projects--pin-list 'unread)
         (root (file-name-as-directory (file-truename temporary-file-directory)))
         (ygg-projects-tests--ts (mapcar (lambda (spec) (cons (nth 0 spec) (nth 2 spec)))
                                         specs))
         (ygg-projects-tests--made
          (mapcar (lambda (spec)
                    (aob-create-session
                     :id (nth 0 spec) :name (nth 0 spec)
                     :backend (if (nth 1 spec) 'native-subagent 'acp)
                     :project root :state 'working
                     :refs (and (nth 1 spec) (list :parent-session (nth 1 spec)))))
                  specs)))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-projects--roots) (lambda () (list root)))
                  ((symbol-function 'ygg-projects--past) #'ignore)
                  ((symbol-function 'ygg-projects--session-ts)
                   (lambda (s) (cdr (assoc (aob-session-id s) ygg-projects-tests--ts)))))
          (funcall body root))
      (mapc (lambda (s) (ignore-errors (aob-remove-session s))) ygg-projects-tests--made)
      (delete-file ygg-projects-pins-file))))

(defmacro ygg-projects-tests--with-tree (specs &rest body)
  "Run BODY with root bound, over the sessions SPECS describe."
  (declare (indent 1))
  (list 'ygg-projects-tests--call-with-tree (list 'quote specs)
        (cons 'lambda (cons '(root) body))))

(defun ygg-projects-tests--pin (id)
  "Toggle the pin of session ID from a buffer showing it."
  (with-temp-buffer
    (setq-local aob-buffer-session-id id)
    (ygg-projects-toggle-pin)))

(ert-deftest ygg-projects-pinned-sorts-first-whatever-its-recency ()
  (ygg-projects-tests--with-tree (("old" nil 1) ("mid" nil 2) ("new" nil 3))
    (should (equal (ygg-projects-tests--names root) '("new" "mid" "old")))
    (ygg-projects-tests--pin "old")
    (ygg-projects-tests--pin "mid")
    (should (equal (ygg-projects-tests--names root) '("old" "mid" "new")))))

(ert-deftest ygg-projects-pin-twice-unpins ()
  (ygg-projects-tests--with-tree (("old" nil 1) ("new" nil 3))
    (ygg-projects-tests--pin "old")
    (should (ygg-projects--pinned-p (aob-session-get "old")))
    (ygg-projects-tests--pin "old")
    (should-not (ygg-projects--pinned-p (aob-session-get "old")))
    (should (equal (ygg-projects-tests--names root) '("new" "old")))))

(ert-deftest ygg-projects-pin-survives-a-refresh-and-a-restart ()
  "A pin is kept by conversation: read back from its file, it holds a
session resumed under another id."
  (ygg-projects-tests--with-tree (("old" nil 1) ("new" nil 3))
    (aob-session-put (aob-session-get "old") :acp-id "conv-1")
    (ygg-projects-tests--pin "old")
    (ygg-projects-refresh)
    (should (equal (ygg-projects-tests--names root) '("old" "new")))
    (aob-remove-session (aob-session-get "old"))
    (let ((again (aob-create-session :id "old<2>" :name "old<2>" :backend 'acp
                                     :project root :state 'working
                                     :refs (list :acp-id "conv-1"))))
      (push again ygg-projects-tests--made)
      (push (cons "old<2>" 0) ygg-projects-tests--ts)
      (setq ygg-projects--pin-list 'unread)
      (should (ygg-projects--pinned-p again))
      (should (equal (ygg-projects-tests--names root) '("old<2>" "new"))))))

(ert-deftest ygg-projects-pin-holds-when-the-conversation-arrives-later ()
  (ygg-projects-tests--with-tree (("old" nil 1) ("new" nil 3))
    (ygg-projects-tests--pin "old")
    (aob-session-put (aob-session-get "old") :acp-id "conv-late")
    (should (ygg-projects--pinned-p (aob-session-get "old")))
    (should (equal (ygg-projects-tests--names root) '("old" "new")))
    (ygg-projects-tests--pin "old")
    (should-not (ygg-projects--pins))))

(ert-deftest ygg-projects-pin-by-session-id-never-reaches-the-file ()
  "Session ids start again from 1 after a restart, so only a conversation
key is written; a pin taken early moves onto the conversation once it exists."
  (ygg-projects-tests--with-tree (("acp:claude:1" nil 1))
    (ygg-projects-tests--pin "acp:claude:1")
    (should (ygg-projects--pinned-p (aob-session-get "acp:claude:1")))
    (setq ygg-projects--pin-list 'unread)
    (should-not (ygg-projects--pins))
    (ygg-projects-tests--pin "acp:claude:1")
    (aob-session-put (aob-session-get "acp:claude:1") :acp-id "conv-9")
    (should (ygg-projects--pinned-p (aob-session-get "acp:claude:1")))
    (setq ygg-projects--pin-list 'unread)
    (should (equal (ygg-projects--pins) '("conv-9")))))

(ert-deftest ygg-projects-pinned-conversation-stays-on-top-after-a-restart ()
  "Ended by a restart, a pinned conversation still leads its project, to
resume and to unpin, even with ended conversations otherwise hidden."
  (ygg-projects-tests--with-tree (("old" nil 1) ("new" nil 3))
    (aob-session-put (aob-session-get "old") :acp-id "conv-1")
    (ygg-projects-tests--pin "old")
    (aob-remove-session (aob-session-get "old"))
    (setq ygg-projects--pin-list 'unread)
    (let ((ended (list :acp-id "conv-1" :name "old" :dir root)))
      (cl-letf (((symbol-function 'ygg-projects--past) (lambda (_) (list ended))))
        (should (equal (ygg-projects-tests--names root) '("old" "new")))
        (should (eq (cdar (ygg-projects--entries root 'agents)) ended))
        (cl-letf (((symbol-function 'ygg-projects--in-sidebar-p) (lambda () t))
                  ((symbol-function 'ygg-projects--selecting-p) #'ignore)
                  ((symbol-function 'ygg-projects--entry-at-point) (lambda () ended))
                  ((symbol-function 'ygg-projects--leave-selection) #'ignore))
          (ygg-projects-toggle-pin))
        (should-not (ygg-projects--pins))
        (should (equal (ygg-projects-tests--names root) '("new")))))))

(ert-deftest ygg-projects-every-pinned-conversation-stays-worktrees-too ()
  "Every pin outlasts its session, one made in a linked worktree under
the main checkout's project as a running session would be."
  (ygg-projects-tests--with-tree (("new" nil 3))
    (let* ((tree (file-name-as-directory (make-temp-file "ygg-wt" t)))
           (ygg-projects--tree-mains (make-hash-table :test #'equal))
           (entries (list (list :acp-id "c1" :name "here" :dir root)
                          (list :acp-id "c2" :name "in-tree" :dir tree)
                          (list :acp-id "c3" :name "loose" :dir root))))
      (unwind-protect
          (progn
            (puthash tree root ygg-projects--tree-mains)
            (setq ygg-projects--pin-list (list "c2" "c1"))
            (cl-letf (((symbol-function 'aob-acp-resumable-entries) (lambda () entries))
                      ((symbol-function 'ygg-projects--session-tree) #'ignore))
              (should (equal (ygg-projects-tests--names root) '("in-tree" "here" "new")))))
        (delete-directory tree)))))

(ert-deftest ygg-projects-archiving-a-pinned-conversation-unpins-it ()
  (ygg-projects-tests--with-tree (("new" nil 3))
    (let ((ended (list :acp-id "c1" :name "here" :dir root))
          archived)
      (setq ygg-projects--pin-list (list "c1" "c2"))
      (cl-letf (((symbol-function 'ygg-projects--entry-at-point) (lambda () ended))
                ((symbol-function 'ygg-projects--selecting-p) #'ignore)
                ((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'aob-acp-archive-entry) (lambda (e) (push e archived)))
                ((symbol-function 'aob-transcript-move) #'ignore)
                ((symbol-function 'aob-transcript-forget) #'ignore)
                ((symbol-function 'ygg-projects-refresh) #'ignore))
        (ygg-projects-archive-ask))
      (should (equal archived (list ended)))
      (should (equal (ygg-projects--pins) '("c2"))))))

(ert-deftest ygg-projects-tree-nests-three-levels-by-recency ()
  (ygg-projects-tests--with-tree (("lead" nil 5)
                                  ("a" "lead" 1) ("b" "lead" 2)
                                  ("a1" "a" 1) ("a2" "a" 3)
                                  ("a2x" "a2" 1))
    (should (equal (ygg-projects-tests--names root)
                   '("lead" "└ b" "└ a" "  └ a2" "    └ a2x" "  └ a1")))
    (let ((note (lambda (id) (ygg-projects--tree-text "claude" root 'agents
                                                      (aob-session-get id)))))
      (should (< (string-search "claude" (funcall note "a"))
                 (string-search "claude" (funcall note "a2"))
                 (string-search "claude" (funcall note "a2x")))))))

(ert-deftest ygg-projects-subagents-fold-until-tab-opens-their-lead ()
  "A lead's subagents are hidden by default and counted on its grey line;
TAB on the lead shows them, TAB on one of them folds them back."
  (ygg-projects-tests--with-tree (("lead" nil 5) ("a" "lead" 1) ("b" "lead" 2) ("solo" nil 1))
    (let ((ygg-projects-show-subagents nil)
          (ygg-projects--expanded nil))
      (should (equal (ygg-projects-tests--names root) '("lead" "solo")))
      (should (string-suffix-p "▸ 2" (or (ygg-projects--entry-tree (aob-session-get "lead")) "")))
      (should-not (string-search "▸" (or (ygg-projects--entry-tree (aob-session-get "solo")) "")))
      (cl-letf (((symbol-function 'ygg-projects-refresh) #'ignore))
        (with-temp-buffer
          (insert (propertize "lead" 'ygg-project root 'ygg-row 'agents
                              'ygg-entry (aob-session-get "lead")))
          (goto-char (point-min))
          (ygg-projects-toggle)
          (should (equal (ygg-projects-tests--names root) '("lead" "└ b" "└ a" "solo")))
          (should-not (string-search "▸" (or (ygg-projects--entry-tree (aob-session-get "lead")) "")))
          (erase-buffer)
          (insert (propertize "└ a" 'ygg-project root 'ygg-row 'agents
                              'ygg-entry (aob-session-get "a")))
          (goto-char (point-min))
          (ygg-projects-toggle)
          (should (equal (ygg-projects-tests--names root) '("lead" "solo"))))))))

(ert-deftest ygg-projects-tab-on-a-lead-without-subagents-keeps-its-old-use ()
  (ygg-projects-tests--with-tree (("solo" nil 1))
    (let ((ygg-projects-show-subagents nil) (ygg-projects--expanded nil))
      (should-not (ygg-projects--toggle-subagents (aob-session-get "solo")))
      (should-not ygg-projects--expanded))))

(ert-deftest ygg-projects-tree-hides-an-ended-grandchild ()
  (ygg-projects-tests--with-tree (("lead" nil 5) ("a" "lead" 1) ("a1" "a" 1))
    (aob-set-state (aob-session-get "a1") 'done)
    (should (equal (ygg-projects-tests--names root) '("lead" "└ a")))))

(ert-deftest ygg-projects-pinned-lead-keeps-its-children-under-it ()
  (ygg-projects-tests--with-tree (("old" nil 1) ("new" nil 9)
                                  ("kid" "old" 2) ("grand" "kid" 3))
    (ygg-projects-tests--pin "grand")
    (should (ygg-projects--pinned-p (aob-session-get "old")))
    (should (equal (ygg-projects-tests--names root)
                   '("old" "└ kid" "  └ grand" "new")))))

(ert-deftest ygg-projects-pin-row-carries-a-grey-mark ()
  (ygg-projects-tests--with-tree (("old" nil 1))
    (ygg-projects-tests--pin "old")
    (let* ((s (aob-session-get "old"))
           (row (ygg-projects--entry-text "old" root 'agents s))
           (at (string-search "⊤" row)))
      (should at)
      (should (eq (get-text-property at 'font-lock-face row) 'ygg-projects-count)))))

(ert-deftest ygg-projects-pin-outside-the-sidebar-needs-a-session ()
  (with-temp-buffer
    (should-error (ygg-projects-toggle-pin) :type 'user-error)))

(ert-deftest ygg-projects-lead-count-says-live-against-cap ()
  (ygg-projects-tests--with-tree (("lead" nil 1))
    (let ((s (aob-session-get "lead")))
      (aob-session-put s :agent "claude")
      (cl-letf (((symbol-function 'ygg-projects--session-tree) #'ignore)
                ((symbol-function 'aob-subagent-live-count) (lambda (_) 3)))
        (should (equal (ygg-projects--entry-tree s) "claude"))
        (aob-session-put s :subagent-cap 6)
        (should (equal (ygg-projects--entry-tree s) "claude · 3/6"))
        (cl-letf (((symbol-function 'aob-subagent-live-count) nil))
          (should (equal (ygg-projects--entry-tree s) "claude")))))))

(ert-deftest ygg-projects-selection-pins-and-unpins-all ()
  (ygg-projects-tests--with-rows
    (let ((ygg-projects-pins-file (make-temp-file "ygg-projects-pins"))
          (ygg-projects--pin-list 'unread))
      (unwind-protect
          (progn
            (setq-local ygg-projects--modal t)
            (ygg-projects-tests--select 3 6)
            (ygg-projects-toggle-pin)
            (should (ygg-projects--pinned-p a))
            (should (ygg-projects--pinned-p b))
            (should-not ygg--visual-p)
            (should (equal (ygg-projects--pins) '("t:alpha" "t:beta")))
            (ygg-projects-tests--select 5 6)
            (ygg-projects-toggle-pin)
            (should (ygg-projects--pinned-p a))
            (should-not (ygg-projects--pinned-p b))
            (ygg-projects-tests--select 3 3)
            (ygg-projects-toggle-pin)
            (should-not (ygg-projects--pins)))
        (delete-file ygg-projects-pins-file)))))

(provide 'ygg-projects-tests)
;;; ygg-projects-tests.el ends here
