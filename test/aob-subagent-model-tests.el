;;; aob-subagent-model-tests.el --- subagents always show a model and tokens -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'aob-subagent)

(defmacro aob-sub-model-tests--with (svar &rest body)
  (declare (indent 1))
  `(let* ((aob-trace-icons nil)
          (,svar (aob-create-session :id "acp:submodel:1" :backend 'acp
                                     :name "submodel" :project "/tmp/proj/"
                                     :dir "/tmp/proj/" :state 'working)))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (string-match-p "\\`\\(subs\\|trace\\):" (buffer-name b))
           (kill-buffer b)))
       (when (aob-session-get (aob-session-id ,svar))
         (aob-remove-session ,svar)))))

(defun aob-sub-model-tests--task (s &rest props)
  (apply #'aob-event s 'tool :tool-id "T1" :kind "think" :subagent t
         :title "Count files" :status "in_progress" props))

(defun aob-sub-model-tests--trace-text (s)
  (with-current-buffer (aob-trace-buffer s)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun aob-sub-model-tests--subs-text (s)
  (with-current-buffer (aob-subagents-buffer s)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun aob-sub-model-tests--call (s &rest u)
  (aob-acp--tool-call
   s (append (list :toolCallId "T1" :kind "think" :title "Task" :status "in_progress"
                   :_meta (list :claudeCode (list :toolName "Task")))
             u))
  (gethash "T1" (aob-acp--tools s)))

(ert-deftest aob-subagent-model-explicit-in-trace-and-list ()
  (aob-sub-model-tests--with s
    (aob-session-put s :model-id "opus")
    (aob-sub-model-tests--call s :rawInput (list :description "Count files" :model "haiku"))
    (should (string-match-p "Count files.* haiku" (aob-sub-model-tests--trace-text s)))
    (should-not (string-match-p "haiku ↑" (aob-sub-model-tests--trace-text s)))
    (should (string-match-p "Count files.* haiku" (aob-sub-model-tests--subs-text s)))))

(ert-deftest aob-subagent-model-inherited-is-captured-at-spawn ()
  (aob-sub-model-tests--with s
    (aob-session-put s :model-id "opus")
    (aob-session-put s :model-live "claude-opus-4-7")
    (aob-sub-model-tests--call s :rawInput (list :description "Count files"))
    (aob-session-put s :model-live nil)
    (aob-session-put s :model-id "haiku")
    (aob--dirty s)
    (should (string-match-p "Count files.* claude-opus-4-7 ↑" (aob-sub-model-tests--trace-text s)))
    (should (string-match-p "Count files.* claude-opus-4-7 ↑" (aob-sub-model-tests--subs-text s)))
    (let ((pos (string-match "claude-opus-4-7 ↑" (aob-sub-model-tests--trace-text s))))
      (with-current-buffer (aob-trace-buffer s)
        (should (memq 'shadow (ensure-list (get-text-property (1+ pos) 'face))))))))

(ert-deftest aob-subagent-model-unknown-in-trace-and-list ()
  (aob-sub-model-tests--with s
    (aob-sub-model-tests--call s :rawInput (list :description "Count files"))
    (should (string-match-p "Count files.* · \\?" (aob-sub-model-tests--trace-text s)))
    (should (string-match-p "Count files.* · \\?" (aob-sub-model-tests--subs-text s)))))

(ert-deftest aob-subagent-model-in-native-subagent-header ()
  (aob-sub-model-tests--with s
    (let ((kid (aob-create-session :id "acp:submodel:1/T1" :backend 'native-subagent
                                   :name "kid" :state 'working
                                   :refs (list :native-root (aob-session-id s)
                                               :parent-session (aob-session-id s)
                                               :parent-model "opus"))))
      (unwind-protect
          (let ((aob-trace--own-parent t))
            (should (string-match-p "opus ↑" (aob-trace--header kid)))
            (aob-session-put s :model-id "sonnet")
            (should (string-match-p "opus ↑" (aob-trace--header kid)))
            (aob-session-put kid :model-id "haiku")
            (let ((h (aob-trace--header kid)))
              (should (string-match-p "haiku" h))
              (should-not (string-match-p "↑" h)))
            (aob-session-put kid :model-id nil)
            (aob-session-put kid :parent-model nil)
            (should (string-match-p " · \\? · " (aob-trace--header kid))))
        (aob-remove-session kid)))))

(ert-deftest aob-subagent-unknown-tokens-show-a-dash ()
  (aob-sub-model-tests--with s
    (aob-sub-model-tests--call s :rawInput (list :description "Count files"))
    (should (string-match-p "· —\\'" (string-trim-right (aob-sub-model-tests--subs-text s))))
    (should (string-match-p "Count files.* —" (aob-sub-model-tests--trace-text s)))))

(ert-deftest aob-subagent-model-ignores-agent-definition-files ()
  (let ((dir (make-temp-file "aob-agents" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".claude/agents" dir) t)
          (with-temp-file (expand-file-name ".claude/agents/quick.md" dir)
            (insert "---\nmodel: haiku\n---\n"))
          (aob-sub-model-tests--with s
            (setf (aob-session-project s) dir (aob-session-dir s) dir)
            (aob-session-put s :model-id "opus")
            (let ((ev (aob-sub-model-tests--call
                       s :rawInput (list :description "d" :subagent_type "quick"))))
              (should (equal '("opus" . t) (aob-trace--sub-model-info s ev))))))
      (delete-directory dir t))))

(ert-deftest aob-subagent-parent-model-stays-as-at-spawn ()
  (aob-sub-model-tests--with s
    (let ((ev (aob-sub-model-tests--call s :rawInput (list :description "d"))))
      (should-not (plist-get ev :parent-model))
      (aob-session-put s :model-id "opus")
      (aob-acp--tool-update s (list :toolCallId "T1" :status "completed"))
      (should-not (plist-get ev :parent-model)))))

(ert-deftest aob-subagent-native-kid-shows-context-without-spend ()
  (aob-sub-model-tests--with s
    (aob-session-put s :model-id "opus")
    (let* ((ev (aob-sub-model-tests--call s :rawInput (list :description "Count files")))
           (kid (aob-create-session :id "acp:submodel:1/T1" :backend 'native-subagent
                                    :name "kid" :state 'working
                                    :refs (list :native-root (aob-session-id s)
                                                :parent-session (aob-session-id s)))))
      (unwind-protect
          (progn
            (puthash "T1" (aob-session-id kid) (aob-subagent--native-kids s))
            (aob-session-put kid :ctx-used 12300)
            (should (string-match-p "· 12\\.3k ctx\\'" (substring-no-properties
                                                        (aob-trace--sub-tail s ev))))
            (aob-session-put kid :tokens (list :totalTokens 50000))
            (should (string-match-p "50\\.0k tok" (aob-trace--sub-tail s ev))))
        (aob-remove-session kid)))))

(defmacro aob-sub-model-tests--with-kid (s ev kid &rest body)
  (declare (indent 3))
  `(aob-sub-model-tests--with ,s
     (aob-session-put ,s :model-id "opus")
     (let* ((,ev (aob-sub-model-tests--call ,s :rawInput (list :description "Count files")))
            (,kid (aob-create-session :id "acp:submodel:1/T1" :backend 'native-subagent
                                      :name "kid" :state 'working
                                      :refs (list :native-root (aob-session-id ,s)
                                                  :parent-session (aob-session-id ,s)
                                                  :native-tool-id "T1"))))
       (unwind-protect
           (progn (puthash "T1" (aob-session-id ,kid) (aob-subagent--native-kids ,s))
                  ,@body)
         (aob-remove-session ,kid)))))

(ert-deftest aob-subagent-kid-tokens-update-parent-trace-line ()
  (aob-sub-model-tests--with-kid s ev kid
    (aob-session-put kid :tokens (list :totalTokens 50000))
    (aob-session-kid-changed kid)
    (should (string-match-p "50\\.0k tok" (aob-sub-model-tests--trace-text s)))
    (let ((before (aob-session-ref s :tick)))
      (aob-usage-note-turn kid (list :totalTokens 40000))
      (should (string-match-p "90\\.0k tok" (aob-sub-model-tests--trace-text s)))
      (should-not (string-match-p "50\\.0k tok" (aob-sub-model-tests--trace-text s)))
      (should (> (aob-session-ref s :tick) before)))))

(ert-deftest aob-subagent-kid-model-change-updates-parent-line ()
  (aob-sub-model-tests--with-kid s ev kid
    (aob-session-kid-changed kid)
    (should (string-match-p "Count files.* opus ↑" (aob-sub-model-tests--trace-text s)))
    (aob-session-put kid :model-id "haiku")
    (aob-session-kid-changed kid)
    (should (string-match-p "Count files.* haiku" (aob-sub-model-tests--trace-text s)))
    (should-not (string-match-p "opus ↑" (aob-sub-model-tests--trace-text s)))))

(ert-deftest aob-subagent-kid-unchanged-tail-leaves-owner-alone ()
  (aob-sub-model-tests--with-kid s ev kid
    (aob-session-put kid :tokens (list :totalTokens 50000))
    (aob-session-kid-changed kid)
    (aob-sub-model-tests--trace-text s)
    (let ((tick (aob-session-ref s :tick)))
      (plist-put ev :line "sentinel")
      (aob-session-put kid :turns 3)
      (aob-session-kid-changed kid)
      (should (equal tick (aob-session-ref s :tick)))
      (should (equal "sentinel" (plist-get ev :line))))))

(ert-deftest aob-subagent-kid-change-recomputes-the-list ()
  (aob-sub-model-tests--with-kid s ev kid
    (should (string-match-p "· —" (aob-sub-model-tests--subs-text s)))
    (aob-usage-note-turn kid (list :totalTokens 90000))
    (should (string-match-p "90\\.0k tok" (aob-sub-model-tests--subs-text s)))))

(ert-deftest aob-subagent-resent-tool-call-keeps-parent-model ()
  (aob-sub-model-tests--with s
    (aob-session-put s :model-id "opus")
    (let ((ev (aob-sub-model-tests--call s :rawInput (list :description "d"))))
      (aob-session-put s :model-id "haiku")
      (aob-acp--on-notification
       s "session/update"
       (list :update (list :sessionUpdate "tool_call" :toolCallId "T1" :kind "think"
                           :title "Task" :status "in_progress"
                           :rawInput (list :description "d"))))
      (should (eq ev (gethash "T1" (aob-acp--tools s))))
      (should (equal "opus" (plist-get ev :parent-model))))))

(ert-deftest aob-subagent-card-task-shows-model-and-tokens ()
  (aob-sub-model-tests--with-kid s ev kid
    (plist-put ev :kind "execute")
    (plist-put ev :line nil)
    (aob--dirty s)
    (should (aob-trace--card-p ev))
    (should (string-match-p "\\$ .*opus ↑ · —" (aob-sub-model-tests--trace-text s)))
    (should (equal (aob-trace--sub-tail s ev) (plist-get ev :tail-drawn)))
    (aob-session-put kid :model-id "haiku")
    (aob-session-put kid :tokens (list :totalTokens 50000))
    (aob-session-kid-changed kid)
    (should (string-match-p " haiku · 50\\.0k tok" (aob-sub-model-tests--trace-text s)))
    (should-not (string-match-p "opus ↑" (aob-sub-model-tests--trace-text s)))))

(defun aob-sub-model-tests--head (s ev width)
  (let ((aob-trace--session-id (aob-session-id s))
        (aob-trace--width width))
    (car (split-string (substring-no-properties (aob-trace--shell-card ev)) "\n"))))

(ert-deftest aob-subagent-card-head-fits-narrow-windows-and-keeps-model ()
  (aob-sub-model-tests--with s
    (let ((ev (aob-event s 'tool :tool-id "T1" :kind "execute" :subagent t
                         :title "t" :status "in_progress"
                         :parent-model "claude-opus-4-7-extended-context"
                         :raw (list :command (make-string 120 ?x)))))
      (dolist (w '(40 60 100))
        (let ((head (aob-sub-model-tests--head s ev w)))
          (should (<= (string-width head) w))
          (should (string-match-p " · claude-" head))))
      (should (string-match-p "claude-opus-4-7-extended-context ↑ · —"
                              (aob-sub-model-tests--head s ev 100)))
      (let ((head (aob-sub-model-tests--head s ev 60)))
        (should (string-match-p "claude-opus-4-7-extended-con[^ ]*…" head))
        (should-not (string-match-p "—" head))))))

(ert-deftest aob-subagent-card-tail-is-model-and-tokens-only ()
  (aob-sub-model-tests--with s
    (let ((ev (aob-event s 'tool :tool-id "T1" :kind "execute" :subagent t
                         :title "t" :status "in_progress" :children 3
                         :parent-model "opus" :raw (list :command "ls"))))
      (should (string-match-p "ls · opus ↑ · —" (aob-sub-model-tests--head s ev 100)))
      (should (equal (aob-trace--sub-tail s ev) (plist-get ev :tail-drawn))))))

(ert-deftest aob-plain-shell-card-is-unchanged ()
  (aob-sub-model-tests--with s
    (let ((ev (aob-event s 'tool :tool-id "X" :kind "execute" :title "t" :status "completed"
                         :raw (list :command "echo some quite long command line that will be cut for width reasons here")))
          (aob-trace--session-id (aob-session-id s)))
      (let ((aob-trace--width 40))
        (should (equal-including-properties
                 (aob-trace--shell-card ev)
                 #("$ echo some quite long command line…" 0 2 (wrap-prefix #("  " 0 2 (face aob-trace-small)) font-lock-face (shadow aob-trace-small)) 2 36 (wrap-prefix #("  " 0 2 (face aob-trace-small)) font-lock-face (aob-trace-small))))))
      (let ((aob-trace--width 60))
        (should (equal-including-properties
                 (aob-trace--shell-card ev)
                 #("$ echo some quite long command line that will be cut fo…" 0 2 (wrap-prefix #("  " 0 2 (face aob-trace-small)) font-lock-face (shadow aob-trace-small)) 2 56 (wrap-prefix #("  " 0 2 (face aob-trace-small)) font-lock-face (aob-trace-small)))))))))

(ert-deftest aob-subagent-diff-card-truncates-long-name-before-tail ()
  (aob-sub-model-tests--with s
    (let* ((ev (aob-event s 'tool :tool-id "D1" :kind "edit" :subagent t
                          :title "e" :status "completed" :parent-model "opus"
                          :content (list (list :type "diff"
                                               :path (concat "/tmp/proj/" (make-string 80 ?n) ".el")
                                               :oldText "a" :newText "b"))))
           (aob-trace--session-id (aob-session-id s))
           (aob-trace--width 40)
           (head (car (split-string (substring-no-properties (aob-trace--diff-card ev)) "\n"))))
      (should (<= (string-width head) 40))
      (should (string-match-p "opus ↑" head)))))

(provide 'aob-subagent-model-tests)
;;; aob-subagent-model-tests.el ends here
