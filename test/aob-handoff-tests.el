;;; aob-handoff-tests.el --- handing a session's work to a fresh one -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'aob-btw)
(require 'aob-handoff)

(setq aob-acp-persist-file (make-temp-file "aob-handoff-sessions-" nil ".eld"))

(defvar aob-handoff-tests--requests nil
  "Requests a session would have sent, newest first, as (METHOD PARAMS CALLBACK).")

(defvar aob-handoff-tests--spawns nil
  "Spawns asked for, newest first, as plists of what each was given.")

(defun aob-handoff-tests--fake-fork (_source)
  (let ((fork (aob-create-session :id "acp:handoff-fork" :backend 'acp
                                  :name "handoff-fork" :project "/tmp/proj/"
                                  :dir "/tmp/proj/wt/" :state 'starting
                                  :refs aob-acp-session-refs)))
    (aob-session-put fork :acp-id "fork-acp")
    fork))

(defun aob-handoff-tests--fake-spawn (agent &optional intent atts name tree)
  (push (list :agent agent :intent intent :atts atts :name name :tree tree
              :start-dir aob-acp-start-dir :default-directory default-directory
              :refs aob-acp-session-refs
              :preset (aob-acp-preset agent))
        aob-handoff-tests--spawns)
  (aob-create-session :id "acp:handoff-new" :backend 'acp
                      :name "handoff-new" :project aob-acp-start-dir
                      :dir tree :state 'starting :refs aob-acp-session-refs))

(defmacro aob-handoff-tests--with (vars &rest body)
  "Bind (SOURCE) in VARS to a live session around BODY, fork and spawn stubbed."
  (declare (indent 1))
  (let ((src (car vars)))
    `(let* ((aob-acp--opened-any t)
            (aob-acp-persist-file (make-temp-file "aob-handoff-persist-" nil ".eld"))
            (aob-handoff-tests--requests nil)
            (aob-handoff-tests--spawns nil)
            (aob-acp-presets '(("builder" :agent "claude" :worktree t :mode "plan")))
            (,src (aob-create-session :id "acp:handoff-src" :backend 'acp
                                      :name "src" :project "/tmp/proj/"
                                      :dir "/tmp/proj/wt/" :state 'idle)))
       (aob-session-put ,src :acp-id "src-acp")
       (aob-session-put ,src :agent "claude")
       (aob-session-put ,src :preset "builder")
       (aob-session-put ,src :task "T-7")
       (aob-session-put ,src :extra-dirs '("/tmp/other/"))
       (aob-session-put ,src :limits '(:max-turns 9))
       (unwind-protect
           (cl-letf (((symbol-function 'aob-acp-fork) #'aob-handoff-tests--fake-fork)
                     ((symbol-function 'aob-acp-spawn) #'aob-handoff-tests--fake-spawn)
                     ((symbol-function 'aob-acp--request)
                      (lambda (s method params cb)
                        (push (list method params cb (aob-session-id s))
                              aob-handoff-tests--requests)))
                     ((symbol-function 'aob-acp--auto-name) #'ignore)
                     ((symbol-function 'aob-transcript-move) #'ignore)
                     ((symbol-function 'aob-compose-show) #'ignore)
                     ((symbol-function 'message) #'ignore))
             ,@body)
         (dolist (id '("acp:handoff-src" "acp:handoff-fork" "acp:handoff-new"))
           (when-let* ((s (aob-session-get id))) (aob-remove-session s)))
         (dolist (name '("compose:handoff-new" "compose:handoff:src" "*aob-btw*"))
           (when-let* ((buf (get-buffer name))) (kill-buffer buf)))
         (delete-file aob-acp-persist-file)))))

(defun aob-handoff-tests--open (fork)
  "Let FORK finish opening, as the adapter's answer to session/fork does."
  (aob-set-state fork 'idle)
  (aob-acp--flush-queue fork))

(defun aob-handoff-tests--prompt ()
  "The prompt the fork was sent, printed, or nil."
  (when-let* ((req (seq-find (lambda (r) (equal (car r) "session/prompt"))
                             aob-handoff-tests--requests)))
    (format "%S" (cadr req))))

(defun aob-handoff-tests--end-turn ()
  (funcall (nth 2 (seq-find (lambda (r) (equal (car r) "session/prompt"))
                            aob-handoff-tests--requests))
           '(:stopReason "end_turn") nil))

(defun aob-handoff-tests--compose-text ()
  (when-let* ((buf (get-buffer "compose:handoff-new")))
    (with-current-buffer buf (buffer-string))))

(ert-deftest aob-handoff-the-fork-hears-the-instructions-and-the-task ()
  (aob-handoff-tests--with (src)
    (let ((fork (aob-handoff-ask src "do it for teams too")))
      (should (aob-session-ref fork :hidden))
      (aob-handoff-tests--open fork)
      (let ((prompt (aob-handoff-tests--prompt)))
        (should (string-match-p (regexp-quote "Write a prompt for a new agent") prompt))
        (should (string-match-p "Next task: do it for teams too" prompt))))))

(ert-deftest aob-handoff-no-task-means-carry-on ()
  (aob-handoff-tests--with (src)
    (aob-handoff-tests--open (aob-handoff-ask src "  "))
    (should (string-match-p "Next task: Continue the current work"
                            (aob-handoff-tests--prompt)))))

(ert-deftest aob-handoff-the-answer-opens-a-fresh-session-with-it-in-compose ()
  (aob-handoff-tests--with (src)
    (let ((before (copy-sequence (aob-session-events src)))
          (fork (aob-handoff-ask src "next")))
      (aob-handoff-tests--open fork)
      (aob-event fork 'message :text "## Context\nwe did X\n## Task\nnext")
      (aob-handoff-tests--end-turn)
      (should (= (length aob-handoff-tests--spawns) 1))
      (let ((spawn (car aob-handoff-tests--spawns))
            (new (aob-session-get "acp:handoff-new")))
        (should (equal (plist-get spawn :agent) "builder"))
        (should-not (plist-get spawn :intent))
        (should (equal (plist-get spawn :tree) "/tmp/proj/wt/"))
        (should (equal (plist-get spawn :default-directory) "/tmp/proj/wt/"))
        (should (equal (plist-get spawn :start-dir) "/tmp/proj/"))
        (should-not (plist-get (plist-get spawn :preset) :worktree))
        (should (equal (plist-get (plist-get spawn :preset) :mode) "plan"))
        (should (equal (aob-session-ref new :task) "T-7"))
        (should (equal (aob-session-ref new :extra-dirs) '("/tmp/other/")))
        (should (equal (aob-session-ref new :max-turns) 9))
        (should-not (aob-session-ref new :hidden))
        (should-not (aob-session-ref new :btw))
        (should (equal (aob-session-ref new :handoff-from) "src"))
        (should (equal (aob-handoff-tests--compose-text)
                       "## Context\nwe did X\n## Task\nnext"))
        (with-current-buffer "compose:handoff-new"
          (should (equal aob-compose--target "acp:handoff-new")))
        (should-not (seq-find (lambda (r) (equal (nth 3 r) "acp:handoff-new"))
                              aob-handoff-tests--requests))
        (should-not (aob-session-ref new :queued)))
      (should-not (aob-session-get "acp:handoff-fork"))
      (should (equal (aob-session-events src) before))
      (should (eq (aob-session-state src) 'idle)))))

(ert-deftest aob-handoff-a-failed-fork-spawns-nothing-and-says-why ()
  (aob-handoff-tests--with (src)
    (let ((fork (aob-handoff-ask src "next")))
      (aob-handoff-tests--open fork)
      (funcall (nth 2 (car aob-handoff-tests--requests)) nil '(:message "rate limited"))
      (should-not aob-handoff-tests--spawns)
      (let ((popup (with-current-buffer aob-btw-buffer-name (buffer-string))))
        (should (string-match-p "failed" popup))
        (should (string-match-p "rate limited" popup))
        (should (string-match-p "handoff: next" popup))
        (should-not (string-match-p "Write a prompt" popup)))
      (should-not (aob-session-get "acp:handoff-fork")))))

(ert-deftest aob-handoff-a-fork-that-signals-spawns-nothing ()
  (aob-handoff-tests--with (src)
    (cl-letf (((symbol-function 'aob-acp-fork)
               (lambda (_s) (error "No fork here"))))
      (should-not (aob-handoff-ask src "next"))
      (should-not aob-handoff-tests--spawns)
      (should (string-match-p "No fork here"
                              (with-current-buffer aob-btw-buffer-name
                                (buffer-string)))))))

(ert-deftest aob-handoff-no-answer-spawns-nothing ()
  (aob-handoff-tests--with (src)
    (let ((fork (aob-handoff-ask src "next")))
      (aob-handoff-tests--open fork)
      (aob-handoff-tests--end-turn)
      (should-not aob-handoff-tests--spawns)
      (should (string-match-p "no answer"
                              (with-current-buffer aob-btw-buffer-name
                                (buffer-string)))))))

(ert-deftest aob-handoff-the-command-takes-an-empty-task ()
  (aob-handoff-tests--with (src)
    (let (asked)
      (cl-letf (((symbol-function 'aob-handoff-ask)
                 (lambda (s task) (setq asked (list s task)))))
        (with-current-buffer (aob-handoff src)
          (should aob-compose-allow-empty)
          (aob-compose-send)))
      (should (eq (car asked) src))
      (should (equal (cadr asked) "")))))

(provide 'aob-handoff-tests)
;;; aob-handoff-tests.el ends here
