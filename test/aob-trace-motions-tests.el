;;; aob-trace-motions-tests.el --- Turn, tool and hunk motions and objects in the trace -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-trace-motions-sessions-" nil ".eld"))
(require 'aob-trace)
(require 'layer-aob)

(defun aob-trace-motions-tests--feed (s update)
  (aob-acp--filter
   (aob-session-conn s)
   (concat (json-encode `((jsonrpc . "2.0") (method . "session/update")
                          (params . ((update . ,update)))))
           "\n")))

(defun aob-trace-motions-tests--say (s text)
  (aob-trace-motions-tests--feed
   s `((sessionUpdate . "agent_message_chunk") (content . ((type . "text") (text . ,text))))))

(defun aob-trace-motions-tests--tool (s id title kind content)
  (aob-trace-motions-tests--feed
   s `((sessionUpdate . "tool_call") (toolCallId . ,id) (title . ,title) (kind . ,kind)
       (status . "completed") (content . ,content))))

(defun aob-trace-motions-tests--ask (s text)
  (aob-acp--break-accum s)
  (aob-event s 'prompt :text text :typed t))

(defun aob-trace-motions-tests--fill (s)
  "Three turns: a fence and two tool calls, a two-hunk edit, a bare reply."
  (aob-trace-motions-tests--ask s "first question")
  (aob-trace-motions-tests--say s "Reply one.\n\n```elisp\n(+ 1 2)\n```\n\nafter fence")
  (aob-trace-motions-tests--tool
   s "t1" "Edit a.ts" "edit"
   [((type . "diff") (path . "a.ts") (oldText . "a\nb") (newText . "a\nb\nc\nd"))])
  (aob-trace-motions-tests--tool
   s "t2" "ls" "execute"
   [((type . "content") (content . ((type . "text") (text . "file1\nfile2"))))])
  (aob-trace-motions-tests--say s "Done one.")
  (aob-trace-motions-tests--ask s "second question")
  (aob-trace-motions-tests--say s "Reply two.")
  (aob-trace-motions-tests--tool
   s "t3" "Edit b.ts" "edit"
   [((type . "diff") (path . "b.ts")
     (oldText . "a\nb\nc\nd\ne\nf\ng\nh\ni\nj")
     (newText . "a\nB\nc\nd\ne\nf\ng\nh\nI\nj"))])
  (aob-trace-motions-tests--ask s "third question")
  (aob-trace-motions-tests--say s "Reply three."))

(defmacro aob-trace-motions-tests--in-trace (&rest body)
  "Run BODY in the selected window on a rendered fake trace, point at the top."
  (declare (indent 0))
  `(let* ((proc (make-process :name "aob-test-cat" :command '("cat")
                              :connection-type 'pipe :noquery t))
          (s (aob-create-session :id "acp:test:1" :backend 'acp :name "test:1"
                                 :project "/tmp/proj/" :dir "/tmp/proj/" :state 'starting))
          (aob-trace-icons nil)
          (kill-ring nil))
     (unwind-protect
         (progn
           (process-put proc 'aob-sessions (make-hash-table :test #'equal))
           (process-put proc 'aob-next-id (list 0))
           (process-put proc 'aob-pending (make-hash-table :test #'eql))
           (process-put proc 'aob-json-buf (generate-new-buffer " *aob-test-json*"))
           (setf (aob-session-conn s) proc)
           (aob-acp--register proc "sess-test" s)
           (aob-trace-motions-tests--fill s)
           (let ((trace (aob-trace-buffer s)))
             (unwind-protect
                 (progn
                   (switch-to-buffer trace)
                   (let ((inhibit-read-only t)) (aob-trace--render t))
                   (goto-char (point-min))
                   ,@body)
               (kill-buffer trace))))
       (ignore-errors (kill-buffer (process-get proc 'aob-json-buf)))
       (ignore-errors (delete-process proc))
       (when (aob-session-get (aob-session-id s))
         (aob-remove-session s)))))

(defun aob-trace-motions-tests--keys (keys)
  (execute-kbd-macro (kbd keys)))

(defun aob-trace-motions-tests--at (text)
  "Line start of the first line holding TEXT."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (line-beginning-position)))

(defun aob-trace-motions-tests--walk (keys from stops)
  "Press KEYS once per entry of STOPS from FROM, each time landing on that line start."
  (goto-char from)
  (dolist (stop stops)
    (aob-trace-motions-tests--keys keys)
    (should (= (point) stop))))

(defmacro aob-trace-motions-tests--deftest (name &rest body)
  (declare (indent 1))
  `(ert-deftest ,name ()
     (aob-trace-motions-tests--in-trace ,@body)))

(aob-trace-motions-tests--deftest aob-trace-motions-turns
  (let ((one (point-min))
        (two (aob-trace-motions-tests--at "second question"))
        (three (aob-trace-motions-tests--at "third question")))
    (aob-trace-motions-tests--walk "] ]" one (list two three three))
    (aob-trace-motions-tests--walk "[ [" (aob-trace-motions-tests--at "Reply three.")
                                   (list three two one one))
    (goto-char (aob-trace-motions-tests--at "Done one."))
    (aob-trace-motions-tests--keys "[ [")
    (should (= (point) one))))

(aob-trace-motions-tests--deftest aob-trace-motions-tool-calls
  (let ((edit (aob-trace-motions-tests--at "a.ts"))
        (ls (aob-trace-motions-tests--at "$ ls"))
        (edit2 (aob-trace-motions-tests--at "b.ts")))
    (aob-trace-motions-tests--walk "] t" (point-min) (list edit ls edit2 edit2))
    (aob-trace-motions-tests--walk "[ t" (point-max) (list edit2 ls edit edit))
    (goto-char (aob-trace-motions-tests--at "file2"))
    (aob-trace-motions-tests--keys "[ t")
    (should (= (point) ls))))

(aob-trace-motions-tests--deftest aob-trace-motions-default-to-one-unit
  (let ((two (aob-trace-motions-tests--at "second question"))
        (edit (aob-trace-motions-tests--at "a.ts"))
        (hunk-stop (progn (goto-char (point-min)) (aob-trace-hunk-next) (point))))
    (goto-char (point-min))
    (aob-trace-turn-next)
    (should (= (point) two))
    (aob-trace-turn-prev)
    (should (= (point) (point-min)))
    (aob-trace-tool-next)
    (should (= (point) edit))
    (aob-trace-tool-prev)
    (should (= (point) edit))
    (goto-char (point-min))
    (aob-trace-hunk-next)
    (should (= (point) hunk-stop))
    (goto-char (point-max))
    (aob-trace-hunk-prev)
    (should (< (point) (point-max)))))

(aob-trace-motions-tests--deftest aob-trace-motions-diff-hunks
  (let ((add (aob-trace-motions-tests--at "+ c"))
        (b (aob-trace-motions-tests--at "− b"))
        (i (aob-trace-motions-tests--at "− i")))
    (aob-trace-motions-tests--walk "] c" (point-min) (list add b i i))
    (aob-trace-motions-tests--walk "[ c" (point-max) (list i b add add))
    (goto-char (aob-trace-motions-tests--at "+ B"))
    (aob-trace-motions-tests--keys "[ c")
    (should (= (point) b))))

(aob-trace-motions-tests--deftest aob-trace-motions-counts-and-jumps
  (let ((three (aob-trace-motions-tests--at "third question")))
    (aob-trace-motions-tests--keys "2 ] ]")
    (should (= (point) three))
    (should (= (marker-position ygg--mark-last-jump-pos) (point-min)))
    (aob-trace-motions-tests--keys "9 [ [")
    (should (= (point) (point-min)))
    (aob-trace-motions-tests--keys "2 ] t")
    (should (= (point) (aob-trace-motions-tests--at "$ ls")))))

(aob-trace-motions-tests--deftest aob-trace-motions-extend-in-visual
  (aob-trace-motions-tests--keys "v ] ]")
  (should (= (point) (aob-trace-motions-tests--at "second question")))
  (should (equal (ygg-selection-effective-bounds)
                 (list (point-min) (1+ (point)) 1))))

(aob-trace-motions-tests--deftest aob-trace-motions-keep-queue-bracket
  (should (eq (lookup-key aob-trace-mode-map (kbd "] p")) 'aob-trace-queued-next))
  (should (eq (lookup-key aob-trace-mode-map (kbd "] c")) 'aob-trace-hunk-next))
  (should (eq (lookup-key aob-trace-mode-map (kbd "[ [")) 'aob-trace-turn-prev)))

(defun aob-trace-motions-tests--yank (keys from)
  "What yanking with KEYS from the line holding FROM puts on the kill ring."
  (setq kill-ring nil)
  (goto-char (aob-trace-motions-tests--at from))
  (set-mark (point))
  (aob-trace-motions-tests--keys keys)
  (car kill-ring))

(defun aob-trace-motions-tests--unmoved-p (keys from)
  "Whether KEYS from the line holding FROM leave a one-character selection."
  (goto-char (aob-trace-motions-tests--at from))
  (set-mark (point))
  (aob-trace-motions-tests--keys keys)
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (= 1 (- end beg))))

(defconst aob-trace-motions-tests--reply-one
  "Reply one.\n\n```elisp\n(+ 1 2)\n```\n\nafter fence\na.ts  +2 −0\n  a\n  b\n+ c\n+ d\n$ ls\n  file1\n  file2\nDone one.")

(aob-trace-motions-tests--deftest aob-trace-motions-turn-objects
  (should (equal (aob-trace-motions-tests--yank "m i t y" "Reply one.")
                 aob-trace-motions-tests--reply-one))
  (should (equal (aob-trace-motions-tests--yank "m i t y" "file2")
                 aob-trace-motions-tests--reply-one))
  (should (equal (aob-trace-motions-tests--yank "m a t y" "(+ 1 2)")
                 (concat "first question\n" aob-trace-motions-tests--reply-one)))
  (should (equal (aob-trace-motions-tests--yank "m t y" "Reply two.")
                 "Reply two."))
  (should (equal (aob-trace-motions-tests--yank "m a t y" "second question")
                 "second question\nReply two.\nb.ts  +2 −2\n  a\n− b\n+ B\n  c\n  d\n  ⋯\n  g\n  h\n− i\n+ I\n  j"))
  (should (equal (aob-trace-motions-tests--yank "m i t y" "third question") "Reply three."))
  (should (equal (aob-trace-motions-tests--yank "m a t y" "Reply three.")
                 "third question\nReply three.")))

(aob-trace-motions-tests--deftest aob-trace-motions-tool-objects
  (should (equal (aob-trace-motions-tests--yank "m i c y" "+ c") "  a\n  b\n+ c\n+ d"))
  (should (equal (aob-trace-motions-tests--yank "m a c y" "+ d") "a.ts  +2 −0\n  a\n  b\n+ c\n+ d"))
  (should (equal (aob-trace-motions-tests--yank "m i c y" "$ ls") "  file1\n  file2"))
  (should (equal (aob-trace-motions-tests--yank "m a c y" "file1") "$ ls\n  file1\n  file2"))
  (should (aob-trace-motions-tests--unmoved-p "m a c" "Reply two."))
  (should (aob-trace-motions-tests--unmoved-p "m i c" "second question")))

(aob-trace-motions-tests--deftest aob-trace-motions-fence-objects
  (remove-from-invisibility-spec 'markdown-markup)
  (should (equal (aob-trace-motions-tests--yank "m i f y" "(+ 1 2)") "(+ 1 2)"))
  (should (equal (aob-trace-motions-tests--yank "m a f y" "(+ 1 2)") "```elisp\n(+ 1 2)\n```"))
  (should (aob-trace-motions-tests--unmoved-p "m i f" "after fence"))
  (should (aob-trace-motions-tests--unmoved-p "m a f" "file1")))

(ert-deftest aob-trace-motions-objects-elsewhere-unchanged ()
  (with-temp-buffer
    (text-mode)
    (insert "alpha beta")
    (goto-char 3)
    (should (equal (ygg-match--textobject-bounds ?w 'inside) '(1 . 6)))
    (should-not (aob-trace--textobject-bounds #'ignore ?t 'inside))))

(defconst aob-trace-motions-tests--perf-events 5000)

(defmacro aob-trace-motions-tests--in-long-trace (&rest body)
  "Run BODY in a buffer drawn from a synthetic session of thousands of events."
  (declare (indent 0))
  `(let* ((s (aob-create-session :id "acp:perf:1" :backend 'acp :name "perf:1"
                                 :project "/tmp/proj/" :dir "/tmp/proj/" :state 'idle))
          (types [prompt message tool tool message]))
     (unwind-protect
         (with-temp-buffer
           (setq aob-trace--session-id (aob-session-id s))
           (setf (aob-session-nevents s) aob-trace-motions-tests--perf-events
                 (aob-session-events s)
                 (cl-loop for i from aob-trace-motions-tests--perf-events downto 1
                          collect (list :type (aref types (mod (1- i) 5)) :seq i)))
           (dotimes (i aob-trace-motions-tests--perf-events)
             (insert (propertize (format "event %d line one\nline two\n" (1+ i))
                                 'aob-event (1+ i))))
           (goto-char (point-min))
           ,@body)
       (aob-remove-session s))))

(ert-deftest aob-trace-motions-long-trace-scans-nothing-whole ()
  (aob-trace-motions-tests--in-long-trace
    (let ((scans 0))
      (advice-add 'text-property-not-all :before (lambda (&rest _) (cl-incf scans)) '((name . perf-count)))
      (unwind-protect
          (progn
            (dotimes (_ 200) (aob-trace-tool-next))
            (dotimes (_ 200) (aob-trace-tool-prev))
            (dotimes (_ 50) (aob-trace-turn-next))
            (goto-char (/ (point-max) 2))
            (dotimes (_ 200)
              (aob-trace--turn-bounds 'around)
              (aob-trace--tool-bounds 'inside)))
        (advice-remove 'text-property-not-all 'perf-count))
      (should (= scans 0)))
    (goto-char (point-min))
    (aob-trace-tool-next)
    (should (looking-at "event 3 "))
    (aob-trace-turn-next)
    (should (looking-at "event 6 "))
    (goto-char (point-max))
    (aob-trace-tool-prev)
    (should (looking-at "event 4999 "))))

;;; aob-trace-motions-tests.el ends here
