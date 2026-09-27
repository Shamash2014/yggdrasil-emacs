;;; aob-dedupe-tests.el --- a session is told a thing once -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'aob-context)
(setq aob-acp-persist-file (make-temp-file "aob-dedupe-sessions-" nil ".eld"))

(defmacro aob-dedupe-tests--with-session (var id &rest body)
  "Bind VAR to a fake ACP session named ID over a cat connection."
  (declare (indent 2))
  `(let* ((proc (make-process :name "aob-test-cat" :command '("cat")
                              :connection-type 'pipe :noquery t))
          (,var (aob-create-session :id (concat "acp:test:" ,id) :backend 'acp
                                    :name ,id :project "/tmp/proj/"
                                    :dir "/tmp/proj/" :state 'idle)))
     (unwind-protect
         (progn
           (process-put proc 'aob-sessions (make-hash-table :test #'equal))
           (process-put proc 'aob-next-id (list 0))
           (process-put proc 'aob-pending (make-hash-table :test #'eql))
           (process-put proc 'aob-json-buf (generate-new-buffer " *aob-test-json*"))
           (setf (aob-session-conn ,var) proc)
           (aob-acp--register proc (concat "sess-" ,id) ,var)
           (aob-session-put ,var :acp-id (concat "sess-" ,id))
           ,@body)
       (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
       (ignore-errors (delete-process proc))
       (when (aob-session-get (aob-session-id ,var))
         (aob-remove-session ,var)))))

(defmacro aob-dedupe-tests--answering (var &rest body)
  "Run BODY with every prompt's params pushed onto VAR and answered at once."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'aob-acp--request)
                (lambda (_s _method params cb)
                  (push params ,var)
                  (funcall cb '(:stopReason "end_turn") nil))))
       ,@body)))

(defun aob-dedupe-tests--item (file text)
  (list :file file :text text))

(defun aob-dedupe-tests--tell (s)
  "The context S is sent, noted as though the prompt landed."
  (let ((untold (aob-context-untold s)))
    (aob-tell-all s (cdr untold))
    (car untold)))

(ert-deftest aob-dedupe-context-told-once ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (should (string-match-p "<context /ctx/one>\nfirst" (aob-dedupe-tests--tell s)))
      (should-not (aob-dedupe-tests--tell s)))))

(ert-deftest aob-dedupe-context-changed-item-told-again ()
  (aob-dedupe-tests--with-session s "a"
    (let* ((aob-dedupe-context t)
           (item (aob-dedupe-tests--item "/ctx/one" "first"))
           (aob-context--items (list item)))
      (aob-dedupe-tests--tell s)
      (setq aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "second")))
      (should (string-match-p "second" (aob-dedupe-tests--tell s)))
      (should-not (aob-dedupe-tests--tell s))
      (setq aob-context--items (list item))
      (should (string-match-p "first" (aob-dedupe-tests--tell s))))))

(ert-deftest aob-dedupe-context-new-item-alone ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (aob-dedupe-tests--tell s)
      (push (aob-dedupe-tests--item "/ctx/two" "second") aob-context--items)
      (let ((told (aob-dedupe-tests--tell s)))
        (should (string-match-p "<context /ctx/two>\nsecond" told))
        (should-not (string-match-p "/ctx/one" told))))))

(ert-deftest aob-dedupe-context-dropped-item-says-nothing ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/two" "b")
                                    (aob-dedupe-tests--item "/ctx/one" "a"))))
      (aob-dedupe-tests--tell s)
      (setq aob-context--items (cdr aob-context--items))
      (should-not (aob-dedupe-tests--tell s)))))

(ert-deftest aob-dedupe-context-cut-entry-goes-again ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context-max-chars 60)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/two" (make-string 80 ?b))
                                    (aob-dedupe-tests--item "/ctx/one" "a"))))
      (let ((told (aob-dedupe-tests--tell s)))
        (should (string-match-p "chars left off" told))
        (should (<= (length (car (split-string told "\n…"))) 60)))
      (let ((again (aob-dedupe-tests--tell s)))
        (should (string-match-p "/ctx/two" again))
        (should-not (string-match-p "/ctx/one" again))))))

(ert-deftest aob-dedupe-context-reset-after-compact-and-clear ()
  (dolist (cmd (list "/compact" "/clear"))
    (aob-dedupe-tests--with-session s "a"
      (let ((aob-dedupe-context t)
            (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
        (aob-dedupe-tests--tell s)
        (should-not (aob-dedupe-tests--tell s))
        (aob-dedupe-tests--answering sent
          (aob-acp--prompt-1 s cmd)
          (should sent))
        (should (string-match-p "first" (aob-dedupe-tests--tell s)))))))

(ert-deftest aob-dedupe-context-cancelled-compact-keeps-record ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (aob-dedupe-tests--tell s)
      (cl-letf (((symbol-function 'aob-acp--request)
                 (lambda (_s _m _p cb) (funcall cb '(:stopReason "cancelled") nil))))
        (aob-acp--prompt-1 s "/compact"))
      (should-not (aob-dedupe-tests--tell s)))))

(ert-deftest aob-dedupe-context-per-session ()
  (aob-dedupe-tests--with-session a "a"
    (aob-dedupe-tests--with-session b "b"
      (let ((aob-dedupe-context t)
            (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
        (should (aob-dedupe-tests--tell a))
        (should-not (aob-dedupe-tests--tell a))
        (should (aob-dedupe-tests--tell b))
        (should-not (aob-dedupe-tests--tell b))))))

(ert-deftest aob-dedupe-context-new-conversation-starts-empty ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (aob-dedupe-tests--tell s)
      (aob-session-put s :acp-id "sess-other")
      (should (aob-dedupe-tests--tell s)))))

(ert-deftest aob-dedupe-context-off-repeats ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context nil)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (should (equal (aob-dedupe-tests--tell s) (aob-context-text)))
      (should (equal (aob-dedupe-tests--tell s) (aob-context-text))))))

(defun aob-dedupe-tests--prompt-text (params)
  (plist-get (aref (plist-get params :prompt) 0) :text))

(defun aob-dedupe-tests--trace-send (s typed)
  "The text a prompt carries when TYPED is sent from S's trace, or nil
when nothing went out.  A prompt that goes out is answered at once."
  (let (said)
    (with-temp-buffer
      (setq-local aob-trace--session-id (aob-session-id s))
      (insert typed)
      (cl-letf (((symbol-function 'aob-trace--content-end) (lambda () (point-min)))
                ((symbol-function 'aob-trace-waiting-decision) #'ignore)
                ((symbol-function 'aob-trace--held) #'ignore)
                ((symbol-function 'aob-acp--place-block) #'ignore)
                ((symbol-function 'aob-acp--request)
                 (lambda (_s _m params cb)
                   (setq said (aob-dedupe-tests--prompt-text params))
                   (funcall cb '(:stopReason "end_turn") nil))))
        (aob-trace-send)))
    said))

(ert-deftest aob-dedupe-trace-send-queued-then-dropped-tells-again ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (aob-set-state s 'working)
      (should-not (aob-dedupe-tests--trace-send s "later"))
      (should (string-match-p "first" (car (car (aob-session-ref s :queued)))))
      (cl-letf (((symbol-function 'aob-acp--notify) #'ignore))
        (aob-acp--cancel s t))
      (should-not (aob-session-ref s :queued))
      (aob-set-state s 'idle)
      (should (equal (aob-dedupe-tests--trace-send s "hello")
                     "<context /ctx/one>\nfirst\n</context>\n\nhello")))))

(ert-deftest aob-dedupe-trace-send-queued-then-flushed-is-told ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (aob-set-state s 'working)
      (should-not (aob-dedupe-tests--trace-send s "later"))
      (should (car (aob-context-untold s)))
      (aob-set-state s 'idle)
      (aob-dedupe-tests--answering sent
        (aob-acp--flush-queue s)
        (should (string-match-p "first" (aob-dedupe-tests--prompt-text (car sent)))))
      (should (equal (aob-dedupe-tests--trace-send s "hello") "hello")))))

(ert-deftest aob-dedupe-trace-send-failed-prompt-tells-again ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (cl-letf (((symbol-function 'aob-acp--request)
                 (lambda (_s _m _p cb) (funcall cb nil '(:message "boom")))))
        (with-temp-buffer
          (setq-local aob-trace--session-id (aob-session-id s))
          (insert "hello")
          (cl-letf (((symbol-function 'aob-trace--content-end) (lambda () (point-min)))
                    ((symbol-function 'aob-trace-waiting-decision) #'ignore)
                    ((symbol-function 'aob-trace--held) #'ignore))
            (aob-trace-send))))
      (should (string-match-p "first" (aob-dedupe-tests--trace-send s "again"))))))

(ert-deftest aob-dedupe-trace-send-tells-context-once ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (should (equal (aob-dedupe-tests--trace-send s "hello")
                     "<context /ctx/one>\nfirst\n</context>\n\nhello"))
      (should (equal (aob-dedupe-tests--trace-send s "again") "again")))))

(ert-deftest aob-dedupe-trace-send-keeps-a-slash-command-first ()
  (aob-dedupe-tests--with-session s "a"
    (let ((aob-dedupe-context t)
          (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first"))))
      (should (equal (aob-dedupe-tests--trace-send s "/compact") "/compact"))
      (should (equal (aob-dedupe-tests--trace-send s "/clear now") "/clear now"))
      (should (string-match-p "first" (aob-dedupe-tests--trace-send s "/tmp/x.el fails"))))))

(defmacro aob-dedupe-tests--with-file (dir abs &rest body)
  "Bind DIR to a temp directory holding ABS, note.py, while BODY runs."
  (declare (indent 2))
  `(let* ((,dir (file-name-as-directory (make-temp-file "aob-dedupe" t)))
          (,abs (expand-file-name "note.py" ,dir)))
     (unwind-protect
         (progn (write-region "one\n" nil ,abs) ,@body)
       (delete-directory ,dir t))))

(defun aob-dedupe-tests--mention (sent)
  "The block the newest prompt in SENT carried for its mention."
  (let ((blocks (plist-get (car sent) :prompt)))
    (aref blocks (1- (length blocks)))))

(defun aob-dedupe-tests--embedding (s dir)
  (setf (aob-session-dir s) dir)
  (aob-session-put s :agent-caps '(:promptCapabilities (:embeddedContext t))))

(ert-deftest aob-dedupe-file-embedded-once ()
  (aob-dedupe-tests--with-session s "a"
    (aob-dedupe-tests--with-file dir abs
      (let ((aob-dedupe-context t))
        (aob-dedupe-tests--embedding s dir)
        (aob-dedupe-tests--answering sent
          (aob-acp--prompt-1 s "read @note.py")
          (let ((b (aob-dedupe-tests--mention sent)))
            (should (equal (plist-get b :type) "resource"))
            (should (equal (plist-get (plist-get b :resource) :text) "one\n")))
          (aob-acp--prompt-1 s "and @note.py again")
          (let ((b (aob-dedupe-tests--mention sent)))
            (should (equal (plist-get b :type) "resource_link"))
            (should (equal (plist-get b :uri) (concat "file://" abs)))
            (should (equal (plist-get b :name) "note.py")))
          (write-region "two\n" nil abs)
          (aob-acp--prompt-1 s "now @note.py")
          (let ((b (aob-dedupe-tests--mention sent)))
            (should (equal (plist-get b :type) "resource"))
            (should (equal (plist-get (plist-get b :resource) :text) "two\n"))))))))

(ert-deftest aob-dedupe-file-reset-after-compact-and-clear ()
  (dolist (cmd (list "/compact" "/clear"))
    (aob-dedupe-tests--with-session s "a"
      (aob-dedupe-tests--with-file dir abs
        (let ((aob-dedupe-context t))
          (aob-dedupe-tests--embedding s dir)
          (aob-dedupe-tests--answering sent
            (aob-acp--prompt-1 s "read @note.py")
            (aob-acp--prompt-1 s cmd)
            (aob-acp--prompt-1 s "read @note.py")
            (should (equal (plist-get (aob-dedupe-tests--mention sent) :type)
                           "resource"))))))))

(ert-deftest aob-dedupe-file-failed-prompt-not-noted ()
  (aob-dedupe-tests--with-session s "a"
    (aob-dedupe-tests--with-file dir abs
      (let ((aob-dedupe-context t) sent)
        (aob-dedupe-tests--embedding s dir)
        (cl-letf (((symbol-function 'aob-acp--request)
                   (lambda (_s _m params cb)
                     (push params sent)
                     (funcall cb nil '(:message "boom")))))
          (aob-acp--prompt-1 s "read @note.py"))
        (aob-dedupe-tests--answering sent
          (aob-acp--prompt-1 s "read @note.py")
          (should (equal (plist-get (aob-dedupe-tests--mention sent) :type)
                         "resource")))))))

(ert-deftest aob-dedupe-file-per-session ()
  (aob-dedupe-tests--with-session a "a"
    (aob-dedupe-tests--with-session b "b"
      (aob-dedupe-tests--with-file dir abs
        (let ((aob-dedupe-context t))
          (aob-dedupe-tests--embedding a dir)
          (aob-dedupe-tests--embedding b dir)
          (aob-dedupe-tests--answering sent
            (aob-acp--prompt-1 a "read @note.py")
            (aob-acp--prompt-1 b "read @note.py")
            (should (equal (plist-get (aob-dedupe-tests--mention sent) :type)
                           "resource"))))))))

(ert-deftest aob-dedupe-file-over-limit-is-a-link ()
  (should (= (default-value 'aob-acp-embed-limit) 60000))
  (aob-dedupe-tests--with-session s "a"
    (aob-dedupe-tests--with-file dir abs
      (let ((aob-dedupe-context t))
        (write-region (make-string 60001 ?x) nil abs)
        (aob-dedupe-tests--embedding s dir)
        (aob-dedupe-tests--answering sent
          (aob-acp--prompt-1 s "read @note.py")
          (should (equal (plist-get (aob-dedupe-tests--mention sent) :type)
                         "resource_link"))
          (should-not (aob-told s :embeds-told)))))))

(ert-deftest aob-dedupe-file-off-embeds-every-time ()
  (aob-dedupe-tests--with-session s "a"
    (aob-dedupe-tests--with-file dir abs
      (let ((aob-dedupe-context nil))
        (aob-dedupe-tests--embedding s dir)
        (aob-dedupe-tests--answering sent
          (aob-acp--prompt-1 s "read @note.py")
          (aob-acp--prompt-1 s "read @note.py")
          (should (equal (plist-get (aob-dedupe-tests--mention sent) :type)
                         "resource")))))))

(ert-deftest aob-dedupe-file-without-session-embeds ()
  (aob-dedupe-tests--with-file dir abs
    (let ((aob-dedupe-context t))
      (dotimes (_ 2)
        (should (equal (plist-get (aref (aob-acp--content-blocks
                                         "see @note.py" nil dir t)
                                        1)
                                  :type)
                       "resource"))))))

(defmacro aob-dedupe-tests--holding (calls &rest body)
  "Run BODY with each request pushed onto CALLS as (METHOD PARAMS CALLBACK)."
  (declare (indent 1))
  `(let ((,calls nil))
     (cl-letf (((symbol-function 'aob-acp--request)
                (lambda (_s method params cb) (push (list method params cb) ,calls)))
               ((symbol-function 'aob-acp--place-block) #'ignore)
               ((symbol-function 'aob-acp--notify) #'ignore))
       ,@body)))

(defun aob-dedupe-tests--reply (calls res &optional err)
  (funcall (nth 2 (car calls)) res err))

(ert-deftest aob-dedupe-overflow-retry-tells-once-it-lands ()
  (aob-dedupe-tests--with-session s "a"
    (let* ((aob-dedupe-context t)
           (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first")))
           (untold (aob-context-untold s)))
      (aob-session-put s :commands '((:name "compact")))
      (aob-dedupe-tests--holding calls
        (let ((aob-told-pending (cdr untold)))
          (aob-acp--prompt-1 s (concat (car untold) "\n\nfix the bug")))
        (aob-dedupe-tests--reply calls nil '(:code -32603 :message "Prompt is too long"))
        (should (aob-acp--compact-text-p
                 (aob-dedupe-tests--prompt-text (nth 1 (car calls)))))
        (aob-dedupe-tests--reply calls '(:stopReason "end_turn"))
        (should (string-match-p "first\n</context>\n\nfix the bug"
                                (aob-dedupe-tests--prompt-text (nth 1 (car calls)))))
        (should (car (aob-context-untold s)))
        (aob-dedupe-tests--reply calls '(:stopReason "end_turn"))
        (should (= (length calls) 3))
        (should-not (car (aob-context-untold s)))))))

(ert-deftest aob-dedupe-overflow-retry-that-fails-tells-nothing ()
  (aob-dedupe-tests--with-session s "a"
    (let* ((aob-dedupe-context t)
           (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first")))
           (untold (aob-context-untold s)))
      (aob-session-put s :commands '((:name "compact")))
      (aob-dedupe-tests--holding calls
        (let ((aob-told-pending (cdr untold)))
          (aob-acp--prompt-1 s (concat (car untold) "\n\nfix the bug")))
        (aob-dedupe-tests--reply calls nil '(:code -32603 :message "Prompt is too long"))
        (aob-dedupe-tests--reply calls '(:stopReason "end_turn"))
        (aob-dedupe-tests--reply calls nil '(:code -32603 :message "Prompt is too long"))
        (should (car (aob-context-untold s)))))))

(defun aob-dedupe-tests--steering (s dir)
  (aob-dedupe-tests--embedding s dir)
  (aob-session-put s :agent-meta '(:steering (:supported t)))
  (aob-set-state s 'working))

(defun aob-dedupe-tests--steer-mention (calls)
  (let ((blocks (plist-get (nth 1 (car calls)) :prompt)))
    (should (equal (car (car calls)) "_session/steering"))
    (plist-get (aref blocks (1- (length blocks))) :type)))

(ert-deftest aob-dedupe-steer-that-lands-tells ()
  (dolist (outcome (list "injected" "startedNewTurn"))
    (aob-dedupe-tests--with-session s "a"
      (aob-dedupe-tests--with-file dir abs
        (let* ((aob-dedupe-context t)
               (aob-context--items (list (aob-dedupe-tests--item "/ctx/one" "first")))
               (untold (aob-context-untold s)))
          (aob-dedupe-tests--steering s dir)
          (aob-dedupe-tests--holding calls
            (let ((aob-told-pending (cdr untold)))
              (aob-acp--interject s (concat (car untold) "\n\nsee @note.py")))
            (should (equal (aob-dedupe-tests--steer-mention calls) "resource"))
            (should (car (aob-context-untold s)))
            (aob-dedupe-tests--reply calls (list :outcome outcome))
            (should-not (car (aob-context-untold s)))
            (aob-set-state s 'working)
            (aob-acp--interject s "again @note.py")
            (should (equal (aob-dedupe-tests--steer-mention calls) "resource_link"))
            (write-region "two\n" nil abs)
            (aob-acp--interject s "now @note.py")
            (should (equal (aob-dedupe-tests--steer-mention calls) "resource"))))))))

(ert-deftest aob-dedupe-steer-that-fails-tells-nothing ()
  (aob-dedupe-tests--with-session s "a"
    (aob-dedupe-tests--with-file dir abs
      (let ((aob-dedupe-context t))
        (aob-dedupe-tests--steering s dir)
        (aob-dedupe-tests--holding calls
          (aob-acp--interject s "see @note.py")
          (aob-dedupe-tests--reply calls nil '(:message "no"))
          (should (aob-session-ref s :queued))
          (should-not (aob-told s :embeds-told)))))))

(provide 'aob-dedupe-tests)
;;; aob-dedupe-tests.el ends here
