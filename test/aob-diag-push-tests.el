;;; aob-diag-push-tests.el --- Tests for aob-diag-push -*- lexical-binding: t; -*-

(require 'ert)
(require 'flymake)
(require 'aob)
(setq aob-acp-persist-file (make-temp-file "aob-diag-push-sessions-" nil ".eld"))
(require 'aob-acp)
(require 'aob-diag-push)

(defvar aob-diag-push-tests--diags nil
  "Buffer to the diagnostics the stubbed flymake reports there.")

(defmacro aob-diag-push-tests--with (bindings &rest body)
  "BINDINGS of (VAR LINES) temp files and a session S in their folder."
  (declare (indent 1))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (file-name-as-directory (make-temp-file "aob-diag-push-" t)))
            (s (aob-create-session :id (format "diag-%s" (random 100000))
                                   :backend 'acp :name "diag" :dir ,dir))
            (aob-diag-push-tests--diags nil)
            ,@(mapcar (lambda (b)
                        `(,(car b)
                          (let ((f (expand-file-name ,(symbol-name (car b)) ,dir)))
                            (with-temp-file f
                              (dotimes (i ,(cadr b)) (insert (format "line %d\n" i))))
                            f)))
                      bindings))
       (unwind-protect
           (cl-letf (((symbol-function 'flymake-diagnostics)
                      (lambda (&rest _)
                        (cdr (assq (current-buffer) aob-diag-push-tests--diags)))))
             ,@body)
         (dolist (b (buffer-list))
           (when (and (buffer-file-name b)
                      (string-prefix-p ,dir (buffer-file-name b)))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (aob-remove-session s)
         (delete-directory ,dir t)))))

(defun aob-diag-push-tests--visit (file &rest diags)
  "Visit FILE with flymake on, reporting DIAGS as (LINE TYPE TEXT)."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (setq-local flymake-mode t)
      (push (cons buf
                  (mapcar (pcase-lambda (`(,line ,type ,text))
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line (1- line))
                              (flymake-make-diagnostic
                               buf (point) (line-end-position) type text)))
                          diags))
            aob-diag-push-tests--diags))
    buf))

(defun aob-diag-push-tests--edit (s id &rest paths)
  "Tell S an edit tool call ID touched PATHS."
  (aob-diag-push--note
   s "session/update"
   (list :update (list :sessionUpdate "tool_call" :toolCallId id :kind "edit"
                       :locations (mapcar (lambda (p) (list :path p)) paths)))))

(defun aob-diag-push-tests--send (s text)
  (with-temp-buffer
    (setq-local aob-compose--target (aob-session-id s))
    (car (aob-compose--rewritten text))))

(ert-deftest aob-diag-push-is-hooked-in ()
  (should (memq #'aob-diag-push--note aob-acp-notification-functions))
  (should (memq #'aob-diag-push-compose aob-compose-before-send-functions)))

(ert-deftest aob-diag-push-appends-an-edited-files-errors ()
  (aob-diag-push-tests--with ((a.el 5))
    (aob-diag-push-tests--visit a.el '(3 :error "void variable x")
                                '(2 :warning "unused y"))
    (aob-diag-push-tests--edit s "t1" "a.el")
    (let ((out (aob-diag-push-tests--send s "go on")))
      (should (string-prefix-p "go on\n\n<diagnostics>\n" out))
      (should (string-match-p "^a\\.el:3: void variable x$" out))
      (should-not (string-match-p "unused y" out)))))

(ert-deftest aob-diag-push-learns-an-edit-from-a-later-update ()
  (aob-diag-push-tests--with ((b.el 3))
    (aob-diag-push-tests--visit b.el '(1 :error "boom"))
    (puthash "t2" (list :tool-id "t2" :kind "edit") (aob-acp--tools s))
    (aob-diag-push--note
     s "session/update"
     (list :update (list :sessionUpdate "tool_call_update" :toolCallId "t2"
                         :content (list (list :type "diff" :path b.el)))))
    (should (string-match-p "^b\\.el:1: boom$" (aob-diag-push-tests--send s "x")))))

(ert-deftest aob-diag-push-ignores-other-tool-kinds ()
  (aob-diag-push-tests--with ((r.el 3))
    (aob-diag-push-tests--visit r.el '(1 :error "boom"))
    (aob-diag-push--note
     s "session/update"
     (list :update (list :sessionUpdate "tool_call" :toolCallId "t3" :kind "read"
                         :locations (list (list :path "r.el")))))
    (should (equal (aob-diag-push-tests--send s "x") "x"))))

(ert-deftest aob-diag-push-warnings-alone-add-nothing ()
  (aob-diag-push-tests--with ((w.el 4))
    (aob-diag-push-tests--visit w.el '(1 :warning "meh") '(2 :note "fyi"))
    (aob-diag-push-tests--edit s "t1" "w.el")
    (should (equal (aob-diag-push-tests--send s "hello") "hello"))))

(ert-deftest aob-diag-push-skips-an-unvisited-file ()
  (aob-diag-push-tests--with ((u.el 2))
    (aob-diag-push-tests--edit s "t1" u.el)
    (should (equal (aob-diag-push-tests--send s "hello") "hello"))))

(ert-deftest aob-diag-push-skips-a-modified-buffer ()
  (aob-diag-push-tests--with ((m.el 3))
    (with-current-buffer (aob-diag-push-tests--visit m.el '(1 :error "boom"))
      (goto-char (point-max))
      (insert "unsaved"))
    (aob-diag-push-tests--edit s "t1" "m.el")
    (should (equal (aob-diag-push-tests--send s "hello") "hello"))))

(ert-deftest aob-diag-push-skips-a-buffer-stale-against-disk ()
  (aob-diag-push-tests--with ((d.el 3))
    (aob-diag-push-tests--visit d.el '(1 :error "boom"))
    (set-file-times d.el (time-add (current-time) 100))
    (aob-diag-push-tests--edit s "t1" "d.el")
    (should (equal (aob-diag-push-tests--send s "hello") "hello"))))

(ert-deftest aob-diag-push-caps-at-twenty-lines ()
  (aob-diag-push-tests--with ((c1.el 15) (c2.el 15))
    (apply #'aob-diag-push-tests--visit c1.el
           (mapcar (lambda (i) (list (1+ i) :error (format "one %d" i))) (number-sequence 0 14)))
    (apply #'aob-diag-push-tests--visit c2.el
           (mapcar (lambda (i) (list (1+ i) :error (format "two %d" i))) (number-sequence 0 14)))
    (aob-diag-push-tests--edit s "t1" "c1.el" "c2.el")
    (let* ((out (aob-diag-push-tests--send s "x"))
           (rows (seq-filter (lambda (l) (string-match-p "\\`c[12]\\.el:[0-9]+: " l))
                             (split-string out "\n"))))
      (should (= (length rows) 20))
      (should (equal (car rows) "c1.el:1: one 0"))
      (should (equal (car (last rows)) "c2.el:5: two 4")))))

(ert-deftest aob-diag-push-clears-the-set-after-a-send ()
  (aob-diag-push-tests--with ((a.el 3))
    (aob-diag-push-tests--visit a.el '(2 :error "boom"))
    (aob-diag-push-tests--edit s "t1" "a.el")
    (should (string-match-p "boom" (aob-diag-push-tests--send s "first")))
    (should-not (aob-session-ref s :diag-touched))
    (should (equal (aob-diag-push-tests--send s "second") "second"))))

(ert-deftest aob-diag-push-off-sends-nothing ()
  (aob-diag-push-tests--with ((a.el 3))
    (aob-diag-push-tests--visit a.el '(2 :error "boom"))
    (aob-diag-push-tests--edit s "t1" "a.el")
    (let ((aob-diag-push nil))
      (should (equal (aob-diag-push-tests--send s "hi") "hi")))))

;;; aob-diag-push-tests.el ends here
