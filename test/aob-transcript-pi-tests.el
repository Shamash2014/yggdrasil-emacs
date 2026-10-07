;;; aob-transcript-pi-tests.el --- pi's conversations on disk -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'aob-transcript)
(require 'aob-transcript-pi)

(defvar ygg-agent-conf-root)
(declare-function aob-transcript-restore "aob-transcript" (entry))

(defconst aob-transcript-pi-tests--project "/tmp/pi-proj-x/")

(defmacro aob-transcript-pi-tests--with-world (&rest body)
  "Run BODY with HOME and config homes in a temp tree: ROOT, CONF, HOME."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "aob-pi-world" t)))
          (conf (expand-file-name "conf/pi/" root))
          (home (expand-file-name "home/" root))
          (process-environment (cons (concat "HOME=" (directory-file-name home))
                                     process-environment))
          (ygg-agent-conf-root (expand-file-name "conf" root))
          (aob-transcript--queue nil)
          (aob-transcript--timer nil))
     (make-directory home t)
     (make-directory conf t)
     (aob-transcript-forget)
     (unwind-protect
         (cl-letf (((symbol-function 'ygg-agent--config-env)
                    (lambda (preset _cmd _project &optional _)
                      (and (string-prefix-p "pi" preset)
                           (concat "PI_CODING_AGENT_DIR=" (directory-file-name conf)))))
                   ((symbol-function 'ygg-agent--repo-home) #'identity)
                   ((symbol-function 'ygg-agent--own-home) (lambda (&rest _) nil)))
           ,@body)
       (when (timerp aob-transcript--timer) (cancel-timer aob-transcript--timer))
       (aob-transcript-forget)
       (delete-directory root t))))

(defun aob-transcript-pi-tests--write (home project id lines &optional where cwd)
  "Write pi session ID for PROJECT under HOME, LINES after its header."
  (let* ((dir (expand-file-name
               (concat "sessions/" (aob-transcript-pi--slug project)
                       (and where (concat "/" where)))
               home))
         (file (expand-file-name (format "2026-10-01T10-00-00-000Z_%s.jsonl" id) dir)))
    (make-directory dir t)
    (with-temp-file file
      (insert (format "{\"type\":\"session\",\"version\":3,\"id\":\"%s\",\"timestamp\":\"2026-10-01T10:00:00.000Z\",\"cwd\":\"%s\"}\n"
                      id (directory-file-name (or cwd project))))
      (dolist (line lines) (insert line "\n")))
    file))

(defun aob-transcript-pi-tests--msg (id parent role content &rest extra)
  (format "{\"type\":\"message\",\"id\":\"%s\",\"parentId\":%s,\"timestamp\":\"2026-10-01T10:00:01.000Z\",\"message\":{\"role\":\"%s\",\"content\":%s%s,\"timestamp\":1}}"
          id (if parent (format "\"%s\"" parent) "null") role content
          (if extra (concat "," (car extra)) "")))

(defun aob-transcript-pi-tests--text (text)
  (format "[{\"type\":\"text\",\"text\":%s}]" (json-encode-string text)))

(defun aob-transcript-pi-tests--name (id parent name)
  (format "{\"type\":\"session_info\",\"id\":\"%s\",\"parentId\":\"%s\",\"timestamp\":\"2026-10-01T10:00:09.000Z\",\"name\":%s}"
          id parent (json-encode-string name)))

(defconst aob-transcript-pi-tests--user-a
  (aob-transcript-pi-tests--msg "a1" nil "user" "\"first question\""))

(ert-deftest aob-transcript-pi-names-its-folder-the-way-pi-does ()
  (should (equal (aob-transcript-pi--slug "/Users/x/a.b_c:d/")
                 "--Users-x-a.b_c-d--"))
  (should (equal (aob-transcript--slug "/a/b.git") "-a-b-git")))

(ert-deftest aob-transcript-pi-recognises-its-agents ()
  (should (aob-transcript-pi-p "pi"))
  (should (aob-transcript-pi-p "pi-work"))
  (should-not (aob-transcript-pi-p "claude"))
  (should-not (aob-transcript-pi-p "codex"))
  (should-not (aob-transcript-pi-p "pix"))
  (should-not (aob-transcript-pi-p nil)))

(ert-deftest aob-transcript-pi-lists-a-project-from-its-own-home-and-the-default ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (mine (aob-transcript-pi-tests--write
                  conf project "11111111-aaaa" (list aob-transcript-pi-tests--user-a)))
           (theirs (aob-transcript-pi-tests--write
                    (expand-file-name ".pi/agent/" home) project "22222222-bbbb"
                    (list aob-transcript-pi-tests--user-a)))
           (elsewhere (aob-transcript-pi-tests--write
                       conf "/tmp/other-proj/" "33333333-cccc"
                       (list aob-transcript-pi-tests--user-a)))
           (headless (expand-file-name "2026-10-01T10-00-00-000Z_headless.jsonl"
                                       (file-name-directory mine))))
      (with-temp-file headless (insert "not a header\n"))
      (should (equal (aob-transcript--homes "pi" project)
                     (list (directory-file-name conf)
                           (expand-file-name ".pi/agent" home))))
      (let ((found (aob-transcript-found project "pi")))
        (should (equal (sort (mapcar (lambda (e) (plist-get e :acp-id)) found) #'string<)
                       '("11111111-aaaa" "22222222-bbbb")))
        (should (equal (sort (mapcar (lambda (e) (plist-get e :file)) found) #'string<)
                       (sort (list mine theirs) #'string<)))
        (dolist (e found)
          (should (equal (plist-get e :agent) "pi"))
          (should (equal (plist-get e :dir) project))
          (should-not (plist-get e :archived))))
      (should-not (member elsewhere
                          (mapcar (lambda (e) (plist-get e :file))
                                  (aob-transcript-found project "pi")))))))

(ert-deftest aob-transcript-pi-a-pi-prefixed-agent-reads-the-same-folders ()
  (aob-transcript-pi-tests--with-world
    (aob-transcript-pi-tests--write
     conf aob-transcript-pi-tests--project "44444444-dddd"
     (list aob-transcript-pi-tests--user-a))
    (should (equal (mapcar (lambda (e) (plist-get e :acp-id))
                           (aob-transcript-found aob-transcript-pi-tests--project "pi-work"))
                   '("44444444-dddd")))))

(ert-deftest aob-transcript-pi-titles-by-name-else-by-first-message ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (named (aob-transcript-pi-tests--write
                   conf project "55555555-eeee"
                   (list aob-transcript-pi-tests--user-a
                         (aob-transcript-pi-tests--msg
                          "a2" "a1" "assistant" (aob-transcript-pi-tests--text "ok"))
                         (aob-transcript-pi-tests--name "n1" "a2" "Old name")
                         (aob-transcript-pi-tests--name "n2" "n1" "Refactor auth module"))))
           (padding (make-string 30000 ?x))
           (unnamed (aob-transcript-pi-tests--write
                     conf project "66666666-ffff"
                     (list (aob-transcript-pi-tests--msg
                            "s0" nil "system" "\"\""
                            (format "\"sections\":{\"preamble\":\"%s\"}" padding))
                           (aob-transcript-pi-tests--msg
                            "a1" "s0" "user"
                            (json-encode-string
                             (concat "[workspace: /tmp/pi-proj-x · branch main]\n\n"
                                     "<project-maps>\n<repo-map>\nmap\n</repo-map>\n</project-maps>"
                                     "fix the flaky test\nsecond line")))))))
      (should (equal (aob-transcript--title-1 named) "Refactor auth module"))
      (should (equal (aob-transcript--title-1 unnamed) "fix the flaky test"))
      (let ((found (aob-transcript-found project "pi")))
        (should (= 2 (length found)))
        (should (member named aob-transcript--queue))
        (should (member unnamed aob-transcript--queue)))
      (aob-transcript--read-some)
      (let ((names (mapcar (lambda (e) (plist-get e :name))
                           (aob-transcript-found project "pi"))))
        (should (member "Refactor auth module" names))
        (should (member "fix the flaky test" names))))))

(ert-deftest aob-transcript-pi-shows-only-the-branch-the-session-ended-on ()
  (aob-transcript-pi-tests--with-world
    (let* ((text #'aob-transcript-pi-tests--text)
           (file (aob-transcript-pi-tests--write
                  conf aob-transcript-pi-tests--project "77777777-0000"
                  (list (aob-transcript-pi-tests--msg "a" nil "user" "\"A question\"")
                        (aob-transcript-pi-tests--msg "a1" "a" "assistant" (funcall text "A answer"))
                        (aob-transcript-pi-tests--msg "b" "a1" "user" "\"B dead question\"")
                        (aob-transcript-pi-tests--msg "b1" "b" "assistant" (funcall text "B dead answer"))
                        (aob-transcript-pi-tests--msg "c" "a1" "user" "\"C question\"")
                        (aob-transcript-pi-tests--msg "c1" "c" "assistant" (funcall text "C answer"))))))
      (should (equal (aob-transcript-turns file)
                     '(("user" . "A question") ("assistant" . "A answer")
                       ("user" . "C question") ("assistant" . "C answer")))))))

(ert-deftest aob-transcript-pi-reaches-the-root-past-a-dead-branch-longer-than-the-window ()
  (aob-transcript-pi-tests--with-world
    (let* ((aob-event-cap 6)
           (lines (list (aob-transcript-pi-tests--msg "a" nil "user" "\"root question\"")
                        (aob-transcript-pi-tests--msg
                         "a1" "a" "assistant" (aob-transcript-pi-tests--text "root answer"))))
           (prev "b0"))
      (setq lines (append lines (list (aob-transcript-pi-tests--msg "b0" "a1" "user" "\"dead 0\""))))
      (dotimes (i 12)
        (let ((id (format "b%d" (1+ i))))
          (setq lines (append lines (list (aob-transcript-pi-tests--msg
                                           id prev "assistant"
                                           (aob-transcript-pi-tests--text (format "dead %d" (1+ i))))))
                prev id)))
      (setq lines (append lines (list (aob-transcript-pi-tests--msg "c" "a1" "user" "\"live question\""))))
      (let ((file (aob-transcript-pi-tests--write
                   conf aob-transcript-pi-tests--project "88888888-1111" lines)))
        (should (equal (aob-transcript-turns file)
                       '(("user" . "root question") ("assistant" . "root answer")
                         ("user" . "live question"))))))))

(ert-deftest aob-transcript-pi-renders-tool-calls-and-skips-their-results ()
  (aob-transcript-pi-tests--with-world
    (let ((file (aob-transcript-pi-tests--write
                 conf aob-transcript-pi-tests--project "99999999-2222"
                 (list (aob-transcript-pi-tests--msg "u" nil "user" "\"is the tree clean?\"")
                       (aob-transcript-pi-tests--msg
                        "t" "u" "assistant"
                        "[{\"type\":\"thinking\",\"thinking\":\"hm\"},{\"type\":\"text\",\"text\":\"Checking.\"},{\"type\":\"toolCall\",\"id\":\"call_1\",\"name\":\"bash\",\"arguments\":{\"command\":\"git status\"}},{\"type\":\"toolCall\",\"id\":\"call_2\",\"name\":\"read\",\"arguments\":{\"path\":\"lisp/aob.el\"}}]")
                       (aob-transcript-pi-tests--msg
                        "r" "t" "toolResult" (aob-transcript-pi-tests--text "nothing to commit")
                        "\"toolCallId\":\"call_1\",\"toolName\":\"bash\",\"isError\":false")
                       (aob-transcript-pi-tests--msg
                        "z" "r" "assistant" (aob-transcript-pi-tests--text "Clean."))))))
      (should (equal (aob-transcript-turns file t)
                     '(("user" . "is the tree clean?")
                       ("assistant" . "Checking.")
                       ("tool" . "bash  git status")
                       ("tool" . "read  lisp/aob.el")
                       ("assistant" . "Clean."))))
      (should-not (assoc "tool" (aob-transcript-turns file))))))

(ert-deftest aob-transcript-pi-entry-and-file-round-trip ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (file (aob-transcript-pi-tests--write
                  conf project "abcdef12-3333" (list aob-transcript-pi-tests--user-a)))
           (entry (car (aob-transcript-found project "pi"))))
      (should (equal (aob-transcript-file entry) file))
      (should (equal (aob-transcript-file
                      (list :agent "pi" :acp-id "abcdef12-3333" :dir project))
                     file))
      (should (equal (aob-transcript-file
                      (list :agent "pi-work" :acp-id "abcdef12-3333" :project project))
                     file))
      (should-not (aob-transcript-file
                   (list :agent "pi" :acp-id "nope" :dir project))))))

(defun aob-transcript-pi-tests--under (home where project file)
  (expand-file-name (file-name-nondirectory file)
                    (expand-file-name (format "aob-%s/%s" where (aob-transcript-pi--slug project))
                                      home)))

(ert-deftest aob-transcript-pi-archives-and-discards-outside-pis-sessions-tree ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (file (aob-transcript-pi-tests--write
                  conf project "feedbeef-4444" (list aob-transcript-pi-tests--user-a)))
           (keep (aob-transcript-pi-tests--write
                  conf project "feedbeef-5555" (list aob-transcript-pi-tests--user-a)))
           (entry (seq-find (lambda (e) (equal (plist-get e :acp-id) "feedbeef-4444"))
                            (aob-transcript-found project "pi")))
           (archived (aob-transcript-pi-tests--under conf "archive" project file))
           (gone (aob-transcript-pi-tests--under conf "discarded" project keep))
           (asked nil)
           done)
      (cl-letf (((symbol-function 'aob-acp-delete-entry)
                 (lambda (&rest _) (setq asked t) t)))
        (should (equal (aob-transcript-move entry "archive" (lambda () (setq done t))) archived))
        (should done)
        (should (file-exists-p archived))
        (should-not (file-exists-p file))
        (should (equal (mapcar (lambda (e) (plist-get e :acp-id))
                               (aob-transcript-found project "pi"))
                       '("feedbeef-5555")))
        (let ((away (aob-transcript-found project "pi" "archive")))
          (should (equal (mapcar (lambda (e) (plist-get e :file)) away) (list archived)))
          (should (plist-get (car away) :archived)))
        (aob-transcript-move (car (aob-transcript-found project "pi")) "discarded")
        (should-not asked)
        (should (file-exists-p gone))
        (should-not (file-exists-p keep))
        (should-not (aob-transcript-found project "pi"))
        (should (equal (mapcar (lambda (e) (plist-get e :file))
                               (aob-transcript-found project "pi" "discarded"))
                       (list gone)))
        (should (file-exists-p archived))
        (should (equal (directory-files
                        (expand-file-name (concat "sessions/" (aob-transcript-pi--slug project)) conf)
                        nil "\\.jsonl\\'")
                       nil))
        (should (equal (aob-transcript-file (list :agent "pi" :acp-id "feedbeef-5555" :dir project))
                       gone))))))

(ert-deftest aob-transcript-pi-resuming-a-put-away-session-moves-it-back-before-loading ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (file (aob-transcript-pi-tests--write
                  conf project "beadfeed-7777" (list aob-transcript-pi-tests--user-a)))
           (other (aob-transcript-pi-tests--write
                   conf project "beadfeed-8888" (list aob-transcript-pi-tests--user-a)))
           (seen nil))
      (aob-transcript-move (list :agent "pi" :acp-id "beadfeed-7777" :dir project) "archive")
      (aob-transcript-move (list :agent "pi" :acp-id "beadfeed-8888" :dir project) "discarded")
      (should-not (file-exists-p file))
      (cl-letf (((symbol-function 'aob-acp--resume-entry)
                 (lambda (e _pref)
                   (push (cons (plist-get e :acp-id)
                               (file-exists-p (if (equal (plist-get e :acp-id) "beadfeed-7777")
                                                  file other)))
                         seen)
                   nil)))
        (aob-acp-resume-entry (list :agent "pi" :acp-id "beadfeed-7777" :dir project :archived t))
        (aob-acp-resume-entry (list :agent "pi" :acp-id "beadfeed-8888" :dir project)))
      (should (equal (sort seen (lambda (a b) (string< (car a) (car b))))
                     '(("beadfeed-7777" . t) ("beadfeed-8888" . t))))
      (should (file-exists-p other))
      (should-not (file-exists-p (aob-transcript-pi-tests--under conf "archive" project file)))
      (should-not (file-exists-p (aob-transcript-pi-tests--under conf "discarded" project other)))
      (should (equal (mapcar (lambda (e) (plist-get e :file))
                             (aob-transcript-found project "pi"))
                     (sort (list file other) (lambda (a b) (> (aob-transcript--mtime a)
                                                                 (aob-transcript--mtime b)))))))))

(ert-deftest aob-transcript-pi-restore-leaves-claude-alone ()
  (aob-transcript-pi-tests--with-world
    (should-not (aob-transcript-restore (list :agent "claude" :acp-id "x" :dir "/tmp/pi-proj-x/")))))

(ert-deftest aob-transcript-pi-an-alias-reads-the-projects-own-home ()
  (let* ((root (file-name-as-directory (make-temp-file "aob-pi-alias" t)))
         (home (expand-file-name "home/" root))
         (process-environment (cons (concat "HOME=" (directory-file-name home))
                                    process-environment))
         (ygg-agent-conf-root (expand-file-name "conf" root))
         (project "/tmp/pi-proj-x/"))
    (require 'ygg-agent-conf)
    (make-directory home t)
    (aob-transcript-forget)
    (unwind-protect
        (let* ((own (expand-file-name "conf/pi-proj-x/pi" root))
               (file (aob-transcript-pi-tests--write
                      own project "aaaabbbb-9999" (list aob-transcript-pi-tests--user-a))))
          (should (equal (aob-transcript--own-home "pi-foo" project) own))
          (should (member own (aob-transcript--homes "pi-foo" project)))
          (should (equal (mapcar (lambda (e) (plist-get e :file))
                                 (aob-transcript-found project "pi-foo"))
                         (list file))))
      (aob-transcript-forget)
      (delete-directory root t))))

(ert-deftest aob-transcript-pi-skips-records-shaped_wrong ()
  (aob-transcript-pi-tests--with-world
    (let ((file (aob-transcript-pi-tests--write
                 conf aob-transcript-pi-tests--project "ccccdddd-0001"
                 (list "5" "[1,2]" "\"str\"" "null" "{}"
                       "{\"type\":\"message\",\"id\":\"m1\",\"parentId\":null,\"message\":\"str\"}"
                       "{\"type\":\"message\",\"id\":\"m2\",\"parentId\":null,\"message\":{\"role\":\"user\",\"content\":{\"a\":1}}}"
                       "{\"type\":\"message\",\"id\":\"m3\",\"parentId\":null,\"message\":{\"role\":\"assistant\",\"content\":[5,\"x\"]}}"
                       "{\"type\":\"message\",\"id\":\"m4\",\"parentId\":null,\"message\":5}"
                       (aob-transcript-pi-tests--msg "ok" "m4" "user" "\"still here\"")))))
      (should (equal (aob-transcript-turns file t) '(("user" . "still here"))))
      (should-not (aob-transcript-turns (aob-transcript-pi-tests--write
                                         conf aob-transcript-pi-tests--project "ccccdddd-0002"
                                         (list "5" "[1,2]" "\"str\""))
                                        t)))))

(ert-deftest aob-transcript-pi-opens-asleep-with-its-turns-and-the-header-id ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (file (aob-transcript-pi-tests--write
                  conf project "cafe0000-6666"
                  (list aob-transcript-pi-tests--user-a
                        (aob-transcript-pi-tests--msg
                         "a2" "a1" "assistant" (aob-transcript-pi-tests--text "answer")))))
           (entry (car (aob-transcript-found project "pi")))
           (s (aob-transcript--session entry file)))
      (unwind-protect
          (progn
            (should (equal (aob-session-ref s :acp-id) "cafe0000-6666"))
            (should (equal (aob-session-ref s :agent) "pi"))
            (should (aob-transcript-asleep-p s))
            (should (equal (plist-get (aob-session-ref s :asleep) :acp-id) "cafe0000-6666"))
            (should (= 2 (length (aob-session-events s)))))
        (aob-remove-session s)))))

(ert-deftest aob-transcript-pi-leaves-claude-and-codex-as-they-were ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (claude-dir (expand-file-name
                        (concat ".claude/projects/" (aob-transcript--slug project))
                        home))
           (claude-file (expand-file-name "c-1.jsonl" claude-dir))
           (codex-id "01a0ee9c-f365-7e22-90e3-ff308a4b9ec7")
           (codex-dir (expand-file-name ".codex/sessions/2026/09/29" home))
           (codex-file (expand-file-name
                        (format "rollout-2026-09-29T22-20-58-%s.jsonl" codex-id) codex-dir)))
      (make-directory claude-dir t)
      (make-directory codex-dir t)
      (with-temp-file claude-file
        (insert "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"hello claude\"}}\n"
                "{\"type\":\"assistant\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"},{\"type\":\"tool_use\",\"name\":\"Bash\",\"input\":{\"command\":\"ls\"}}]}}\n"))
      (with-temp-file codex-file
        (insert "{\"type\":\"session_meta\",\"payload\":{\"id\":\"x\"}}\n"
                "{\"type\":\"response_item\",\"payload\":{\"type\":\"message\",\"role\":\"user\",\"content\":[{\"type\":\"input_text\",\"text\":\"fix it\"}]}}\n"))
      (should (equal (aob-transcript--home-1 "claude" project)
                     (expand-file-name "~/.claude")))
      (should (equal (aob-transcript--home-1 "codex" project)
                     (expand-file-name "~/.codex")))
      (should (equal (aob-transcript--home-1 "pi" project)
                     (directory-file-name conf)))
      (should (equal (aob-transcript--homes "claude" project)
                     (list (expand-file-name ".claude" home))))
      (should (equal (aob-transcript-file (list :agent "claude" :acp-id "c-1" :dir project))
                     claude-file))
      (should (equal (aob-transcript-file (list :agent nil :acp-id "c-1" :dir project))
                     claude-file))
      (should (equal (aob-transcript-file (list :agent "codex" :acp-id codex-id :dir project))
                     codex-file))
      (should (equal (mapcar (lambda (e) (plist-get e :acp-id))
                             (aob-transcript-found project "claude"))
                     '("c-1")))
      (should (equal (aob-transcript-turns claude-file t)
                     '(("user" . "hello claude") ("assistant" . "hi") ("tool" . "Bash  ls"))))
      (should (equal (aob-transcript-turns codex-file) '(("user" . "fix it")))))))

(ert-deftest aob-transcript-pi-strips-the-note-in-every-shape-it-arrives-in ()
  (let ((maps "<project-maps>\nm\n</project-maps>"))
    (dolist (case `(("[workspace: /r · branch m]fix it" . "fix it")
                    ("[workspace: /r · branch m]\n\nfix it" . "fix it")
                    (,(concat "[workspace: /r · branch m]\n\n" maps "fix it") . "fix it")
                    (,(concat "[workspace: /r · branch m]" maps "fix it") . "fix it")
                    (,(concat maps "fix it") . "fix it")
                    ("fix it" . "fix it")
                    ("[workspace: /a]b/r · branch m]fix it" . "fix it")
                    ("[workspace: /r · branch m · linked worktree of /x]fix it" . "fix it")
                    ("[workspace: /r · branch m · other worktrees: a, b, …]fix it" . "fix it")
                    ("[workspace: /r · branch m · linked worktree of /x · other worktrees: a]fix it"
                     . "fix it")
                    ("[workspace: /r · branch detached HEAD]fix it" . "fix it")
                    (,(concat "[workspace: /r · branch detached HEAD]\n\n" maps "fix it") . "fix it")
                    ("[workspace: /r · branch detached HEAD · linked worktree of /x · other worktrees: a (detached)]fix it"
                     . "fix it")
                    ("[workspace: /r · branch m][2] fix it" . "[2] fix it")))
      (should (equal (aob-transcript-pi--typed (car case)) (cdr case))))))

(ert-deftest aob-transcript-pi-a-glued-note-leaves-the-turn-in-the-transcript ()
  (aob-transcript-pi-tests--with-world
    (let ((file (aob-transcript-pi-tests--write
                 conf aob-transcript-pi-tests--project "glued000-0001"
                 (list (aob-transcript-pi-tests--msg
                        "g1" nil "user" "\"[workspace: /a]b · branch m]fix it\"")))))
      (should (equal (aob-transcript-turns file t) '(("user" . "fix it")))))))

(ert-deftest aob-transcript-pi-restore-never-overwrites-a-session-pi-recreated ()
  (aob-transcript-pi-tests--with-world
    (let* ((project aob-transcript-pi-tests--project)
           (file (aob-transcript-pi-tests--write
                  conf project "keep0000-0001" (list aob-transcript-pi-tests--user-a)))
           (entry (list :agent "pi" :acp-id "keep0000-0001" :dir project)))
      (aob-transcript-move entry "archive")
      (let ((stub (aob-transcript-pi-tests--write conf project "keep0000-0001" nil)))
        (should (equal stub file))
        (should-error (aob-transcript-restore
                       (append (list :file (aob-transcript-pi-tests--under
                                            conf "archive" project file))
                               entry))
                      :type 'file-already-exists)
        (should (= 0 (length (aob-transcript-turns stub t))))
        (should (file-exists-p (aob-transcript-pi-tests--under conf "archive" project file)))))))

(ert-deftest aob-transcript-pi-a-failed-restore-stops-the-resume-and-names-the-file ()
  (aob-transcript-pi-tests--with-world
    (let ((loaded nil))
      (cl-letf (((symbol-function 'aob-transcript-restore)
                 (lambda (_) (signal 'file-error '("Renaming" "no such" "/x/f.jsonl"))))
                ((symbol-function 'aob-transcript-file) (lambda (_) "/x/f.jsonl"))
                ((symbol-function 'aob-acp--resume-entry) (lambda (&rest _) (setq loaded t))))
        (let ((err (should-error
                    (aob-acp-resume-entry (list :agent "pi" :acp-id "z" :dir "/x/"))
                    :type 'user-error)))
          (should (string-match-p "/x/f\\.jsonl" (cadr err))))
        (should-not loaded)))))

(provide 'aob-transcript-pi-tests)
;;; aob-transcript-pi-tests.el ends here
