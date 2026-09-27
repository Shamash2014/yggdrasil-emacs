;;; aob-compact-reserve-tests.el --- Autocompact by space left, and overflow retry -*- lexical-binding: t; -*-

(require 'ert)
(require 'aob)
(defvar aob-acp-persist-file)
(setq aob-acp-persist-file (make-temp-file "aob-compact-reserve-sessions-" nil ".eld"))
(require 'aob-acp)

(defmacro aob-compact-reserve-tests--with (var &rest body)
  "Bind VAR to an idle session offering /compact, with prompts captured."
  (declare (indent 1))
  `(let* ((,var (aob-create-session :id (format "acp:compact:%d" (random 1000000))
                                    :backend 'acp :name "compact" :dir "/tmp/proj/"
                                    :project "/tmp/proj/" :state 'idle))
          (aob-acp-autocompact-reserve 16384)
          (aob-acp-autocompact-ratio nil)
          (aob-acp-autocompact-overrides nil)
          (sent nil))
     (unwind-protect
         (cl-letf (((symbol-function 'aob-acp--prompt-1)
                    (lambda (_s text &rest _) (push text sent))))
           (aob-session-put ,var :commands '((:name "compact")))
           ,@body)
       (aob-remove-session ,var))))

(defun aob-compact-reserve-tests--fill (s size used)
  (aob-session-put s :ctx-size size)
  (aob-session-put s :ctx-used used)
  (aob-acp--autocompact-check s))

(ert-deftest aob-compact-reserve-200k-window ()
  (aob-compact-reserve-tests--with s
    (aob-compact-reserve-tests--fill s 200000 183000)
    (should-not sent)
    (aob-compact-reserve-tests--fill s 200000 184000)
    (should (equal sent '("/compact")))))

(ert-deftest aob-compact-reserve-1m-window ()
  (aob-compact-reserve-tests--with s
    (aob-compact-reserve-tests--fill s 1000000 950000)
    (should-not sent)
    (aob-compact-reserve-tests--fill s 1000000 984000)
    (should (equal sent '("/compact")))))

(ert-deftest aob-compact-reserve-ratio-ceiling ()
  (aob-compact-reserve-tests--with s
    (let ((aob-acp-autocompact-ratio 0.5))
      (aob-compact-reserve-tests--fill s 1000000 490000)
      (should-not sent)
      (aob-compact-reserve-tests--fill s 1000000 500000)
      (should sent))))

(ert-deftest aob-compact-reserve-override-by-model ()
  (aob-compact-reserve-tests--with s
    (let ((aob-acp-autocompact-overrides '(("sonnet" :reserve 100000)
                                           ("opus" :ratio 0.3 :reserve nil))))
      (aob-session-put s :model-id "claude-sonnet-4")
      (aob-compact-reserve-tests--fill s 200000 110000)
      (should (equal sent '("/compact")))
      (setq sent nil)
      (aob-session-put s :autocompact-fired nil)
      (aob-session-put s :model-id "claude-opus-5")
      (aob-compact-reserve-tests--fill s 1000000 290000)
      (should-not sent)
      (aob-compact-reserve-tests--fill s 1000000 300000)
      (should sent)
      (setq sent nil)
      (aob-session-put s :autocompact-fired nil)
      (aob-session-put s :model-id "gpt-5")
      (aob-compact-reserve-tests--fill s 1000000 300000)
      (should-not sent))))

(ert-deftest aob-compact-reserve-override-reads-config-options ()
  (aob-compact-reserve-tests--with s
    (let ((aob-acp-autocompact-overrides '(("haiku" :reserve 150000))))
      (aob-session-put s :config-options
                       (list (list :id "model" :currentValue "haiku-4" :options [])))
      (aob-compact-reserve-tests--fill s 200000 60000)
      (should sent))))

(ert-deftest aob-compact-reserve-rearms-once-per-fill ()
  (aob-compact-reserve-tests--with s
    (aob-compact-reserve-tests--fill s 200000 190000)
    (should (= (length sent) 1))
    (aob-compact-reserve-tests--fill s 200000 195000)
    (should (= (length sent) 1))
    ;; trigger 183616, re-arm below 163616
    (aob-compact-reserve-tests--fill s 200000 170000)
    (should (aob-session-ref s :autocompact-fired))
    (aob-compact-reserve-tests--fill s 200000 160000)
    (should-not (aob-session-ref s :autocompact-fired))
    (aob-compact-reserve-tests--fill s 200000 190000)
    (should (= (length sent) 2))))

(ert-deftest aob-compact-reserve-disabled-when-both-nil ()
  (aob-compact-reserve-tests--with s
    (let ((aob-acp-autocompact-reserve nil))
      (aob-compact-reserve-tests--fill s 200000 199999)
      (should-not sent)
      (should-not (aob-acp--autocompact-trigger s)))))

(defun aob-compact-reserve-tests--update (s update)
  (aob-acp--compact-note s "session/update" (list :update update)))

(defun aob-compact-reserve-tests--tool (s kind &rest paths)
  (aob-compact-reserve-tests--update
   s (list :sessionUpdate "tool_call" :toolCallId (format "t%d" (random 1000000))
           :kind kind
           :locations (vconcat (mapcar (lambda (p) (list :path p)) paths)))))

(ert-deftest aob-compact-reserve-note-is-hooked ()
  (should (memq #'aob-acp--compact-note aob-acp-notification-functions)))

(ert-deftest aob-compact-reserve-instructions-list-files ()
  (aob-compact-reserve-tests--with s
    (aob-session-put s :agent "claude")
    (aob-compact-reserve-tests--tool s "read" "/tmp/proj/src/a.el" "/tmp/proj/README")
    (aob-compact-reserve-tests--tool s "edit" "/tmp/proj/src/a.el")
    (aob-compact-reserve-tests--update
     s (list :sessionUpdate "tool_call" :toolCallId "d1" :kind "edit"
             :content (vector (list :type "diff" :path "/tmp/proj/lib/b.el"))))
    (aob-compact-reserve-tests--tool s "execute" "/tmp/proj/ignored.sh")
    (let ((p (aob-acp--compact-prompt s)))
      (should (string-prefix-p "/compact Summarize" p))
      (should (string-match-p "Constraints and Preferences" p))
      (should (string-match-p "^Files read: README$" p))
      (should (string-match-p "^Files modified: lib/b\\.el, src/a\\.el$" p))
      (should-not (string-match-p "ignored" p)))))

(ert-deftest aob-compact-reserve-files-survive-ring-and-take-updates ()
  (aob-compact-reserve-tests--with s
    (aob-session-put s :agent "claude")
    (puthash "sub1" (list :kind "edit") (aob-acp--tools s))
    (aob-compact-reserve-tests--update
     s (list :sessionUpdate "tool_call_update" :toolCallId "sub1"
             :locations (vector (list :path "/tmp/proj/deep/child.el"))))
    (setf (aob-session-events s) nil)
    (should (equal (cdr (aob-acp--touched-files s)) '("deep/child.el")))))

(ert-deftest aob-compact-reserve-instructions-cap-files ()
  (aob-compact-reserve-tests--with s
    (aob-session-put s :agent "claude")
    (dotimes (i 260) (aob-compact-reserve-tests--tool s "edit" (format "m%d.el" i)))
    (dotimes (i 50) (aob-compact-reserve-tests--tool s "read" (format "r%d.el" i)))
    (should (= (length (aob-session-ref s :compact-modified)) 200))
    (should (equal (car (aob-session-ref s :compact-modified)) "m259.el"))
    (let ((files (aob-acp--touched-files s)))
      (should (= (length (car files)) 40))
      (should (= (length (cdr files)) 40))
      (should (member "m259.el" (cdr files))))))

(ert-deftest aob-compact-reserve-instructions-only-for-listed-agents ()
  (aob-compact-reserve-tests--with s
    (aob-session-put s :agent "codex")
    (aob-compact-reserve-tests--tool s "edit" "a.el")
    (should (equal (aob-acp--compact-prompt s) "/compact"))))

(defmacro aob-compact-reserve-tests--wire (var &rest body)
  "Session VAR whose prompts are held in CALLS as (TEXT . CALLBACK)."
  (declare (indent 1))
  `(let* ((,var (aob-create-session :id (format "acp:overflow:%d" (random 1000000))
                                    :backend 'acp :name "overflow" :dir "/tmp/proj/"
                                    :project "/tmp/proj/" :state 'idle))
          (calls nil))
     (unwind-protect
         (cl-letf (((symbol-function 'aob-acp--request)
                    (lambda (_s _method params cb)
                      (let ((b (aref (plist-get params :prompt) 0)))
                        (push (cons (plist-get b :text) cb) calls))))
                   ((symbol-function 'aob-acp--auto-name) #'ignore)
                   ((symbol-function 'aob-acp--place-block) #'ignore))
           (aob-session-put ,var :acp-id "sess-overflow")
           (aob-session-put ,var :commands '((:name "compact")))
           ,@body)
       (aob-remove-session ,var))))

(defun aob-compact-reserve-tests--reply (calls &optional err)
  (funcall (cdr (car calls)) (unless err (list :stopReason "end_turn")) err))

(defun aob-compact-reserve-tests--states (s)
  (delq nil (mapcar (lambda (e) (and (eq (plist-get e :type) 'state) (plist-get e :title)))
                    (aob-session-events s))))

(ert-deftest aob-compact-reserve-overflow-compacts-then-resends-once ()
  (aob-compact-reserve-tests--wire s
    (aob-acp--prompt-1 s "fix the bug")
    (aob-compact-reserve-tests--reply calls (list :code -32603 :message "Prompt is too long"))
    (should (equal (car (car calls)) "/compact"))
    (should (member "context full: compacting and retrying"
                    (aob-compact-reserve-tests--states s)))
    (should (eq (aob-session-state s) 'working))
    (aob-compact-reserve-tests--reply calls)
    (should (equal (car (car calls)) "fix the bug"))
    (should (= (length calls) 3))
    (aob-compact-reserve-tests--reply calls)
    (should (eq (aob-session-state s) 'idle))
    (should (= (length calls) 3))))

(ert-deftest aob-compact-reserve-second-overflow-does-not-loop ()
  (aob-compact-reserve-tests--wire s
    (aob-acp--prompt-1 s "fix the bug")
    (aob-compact-reserve-tests--reply calls (list :message "input exceeds the context window"))
    (aob-compact-reserve-tests--reply calls)
    (should (= (length calls) 3))
    (aob-compact-reserve-tests--reply calls (list :message "prompt is too long"))
    (should (= (length calls) 3))
    (should (eq (aob-session-state s) 'idle))
    (should (aob-session-ref s :turn-error))))

(ert-deftest aob-compact-reserve-compact-overflow-not-retried ()
  (aob-compact-reserve-tests--wire s
    (aob-acp--prompt-1 s "/compact")
    (aob-compact-reserve-tests--reply calls (list :message "prompt is too long"))
    (should (= (length calls) 1))
    (should (eq (aob-session-state s) 'idle))))

(ert-deftest aob-compact-reserve-rate-limits-are-not-overflow ()
  (should (aob-acp--overflow-p (list :message "Internal error: Prompt is too long")))
  (should (aob-acp--overflow-p (list :message "x" :data "maximum context length is 200000")))
  (should-not (aob-acp--overflow-p (list :message "too many tokens, please wait")))
  (should-not (aob-acp--overflow-p (list :message "token limit exceeded")))
  (should-not (aob-acp--overflow-p (list :message "429: context window rate limit")))
  (should-not (aob-acp--overflow-p (list :message "Overloaded: prompt is too long")))
  (should-not (aob-acp--overflow-p (list :message "rate_limit_error" :data "context length"))))

(ert-deftest aob-compact-reserve-compact-clears-file-lists ()
  (aob-compact-reserve-tests--wire s
    (aob-compact-reserve-tests--tool s "edit" "a.el")
    (aob-compact-reserve-tests--tool s "read" "b.el")
    (aob-acp--prompt-1 s "/compact")
    (funcall (cdr (car calls)) (list :stopReason "cancelled") nil)
    (should (aob-session-ref s :compact-modified))
    (aob-acp--prompt-1 s "/compact")
    (aob-compact-reserve-tests--reply calls (list :message "boom"))
    (should (aob-session-ref s :compact-read))
    (aob-acp--prompt-1 s "/compact")
    (aob-compact-reserve-tests--reply calls)
    (should-not (aob-session-ref s :compact-modified))
    (should-not (aob-session-ref s :compact-read))))

(ert-deftest aob-compact-reserve-clear-resets-file-lists ()
  (aob-compact-reserve-tests--wire s
    (aob-compact-reserve-tests--tool s "edit" "a.el")
    (aob-compact-reserve-tests--tool s "read" "b.el")
    (aob-acp--prompt-1 s "/clear")
    (aob-compact-reserve-tests--reply calls)
    (should-not (aob-session-ref s :compact-modified))
    (should-not (aob-session-ref s :compact-read))))

(ert-deftest aob-compact-reserve-ordinary-turn-keeps-file-lists ()
  (aob-compact-reserve-tests--wire s
    (aob-compact-reserve-tests--tool s "edit" "a.el")
    (aob-acp--prompt-1 s "go on")
    (aob-compact-reserve-tests--reply calls)
    (should (equal (aob-session-ref s :compact-modified) '("a.el")))))

(ert-deftest aob-compact-reserve-other-errors-untouched ()
  (aob-compact-reserve-tests--wire s
    (aob-acp--prompt-1 s "fix the bug")
    (aob-compact-reserve-tests--reply calls (list :message "Internal error: rate limited"))
    (should (= (length calls) 1))
    (should (eq (aob-session-state s) 'idle))
    (should (aob-session-ref s :turn-error))
    (should-not (member "context full: compacting and retrying"
                        (aob-compact-reserve-tests--states s)))))

(ert-deftest aob-compact-reserve-overflow-needs-compact-command ()
  (aob-compact-reserve-tests--wire s
    (aob-session-put s :commands nil)
    (aob-acp--prompt-1 s "fix the bug")
    (aob-compact-reserve-tests--reply calls (list :message "prompt is too long"))
    (should (= (length calls) 1))
    (should (aob-session-ref s :turn-error))))

(provide 'aob-compact-reserve-tests)
;;; aob-compact-reserve-tests.el ends here
