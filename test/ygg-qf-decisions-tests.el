;;; ygg-qf-decisions-tests.el --- pending decisions in the quickfix -*- lexical-binding: t; -*-

;;; Code:

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'grep)
(yggdrasil-global-mode 1)
(eval-and-compile
  (defvar ygg-space-state-functions nil)
  (defvar ygg-space-detail-functions nil)
  (defvar ygg-leader-open-map (make-sparse-keymap))
  (defvar aob-acp-persist-file)
  (setq aob-acp-persist-file (make-temp-file "aob-qfdec-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'layer-quickfix)
(require 'layer-aob)
(require 'ygg-embark)

(defmacro ygg-qf-dec-tests--with (&rest body)
  (declare (indent 0))
  `(let* ((aob-trace-icons nil)
          (a (aob-create-session :id "acp:qfdec:a" :backend 'acp :name "alpha"
                                 :project "/tmp/proj/" :dir "/tmp/proj/"
                                 :state 'working))
          (b (aob-create-session :id "acp:qfdec:b" :backend 'acp :name "beta"
                                 :project "/tmp/proj/" :dir "/tmp/proj/"
                                 :state 'working)))
     (unwind-protect (progn ,@body)
       (when-let* ((buf (get-buffer "*quickfix*"))) (kill-buffer buf))
       (dolist (s (list a b))
         (when (aob-session-get (aob-session-id s)) (aob-remove-session s))))))

(defun ygg-qf-dec-tests--ask (s title &optional detail)
  (let* ((d (list :kind nil :title title :detail detail
                  :options (list (list :optionId "ok" :name "Allow" :kind "allow_once"))))
         (ev (aob-event s 'permission :title title)))
    (plist-put ev :ts (- (float-time) 300))
    (nconc d (list :seq (plist-get ev :seq)))
    (push d (aob-session-decisions s))
    d))

(defun ygg-qf-dec-tests--clear (s d)
  (setf (aob-session-decisions s) (remq d (aob-session-decisions s)))
  (run-hook-with-args 'aob-state-change-hook s 'blocked 'working))

(defun ygg-qf-dec-tests--text ()
  (with-current-buffer (ygg-qf-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ygg-qf-dec-tests--goto (text)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward text)
    (beginning-of-line)))

(defun ygg-qf-dec-tests--id (s d)
  (cons (aob-session-id s) (plist-get d :seq)))

(ert-deftest ygg-qf-decisions-one-row-per-decision-across-sessions ()
  (ygg-qf-dec-tests--with
    (ygg-qf-dec-tests--ask a "Bash" "git push")
    (ygg-qf-dec-tests--ask b "Edit foo.el")
    (ygg-aob-decisions)
    (with-current-buffer (ygg-qf-buffer)
      (should (= 2 (ygg-qf--count-rows))))
    (should (string-match-p "alpha · Bash  \\[git push\\]" (ygg-qf-dec-tests--text)))
    (should (string-match-p "beta · Edit foo.el" (ygg-qf-dec-tests--text)))
    (should (equal "permission · 5m"
                   (get-text-property (progn (ygg-qf-dec-tests--goto "alpha")
                                             (line-beginning-position))
                                      'ygg-qf-note)))))

(ert-deftest ygg-qf-decisions-empty-is-a-user-error ()
  (ygg-qf-dec-tests--with
    (should-error (ygg-aob-decisions) :type 'user-error)))

(ert-deftest ygg-qf-decisions-default-action-answers-that-decision-not-the-first ()
  (ygg-qf-dec-tests--with
    (let ((old (ygg-qf-dec-tests--ask a "Old"))
          (new (ygg-qf-dec-tests--ask a "New"))
          asked)
      (ygg-aob-decisions)
      (ygg-qf-dec-tests--goto "alpha · Old")
      (cl-letf (((symbol-function 'aob-resolve)
                 (lambda (_s d) (push d asked)))
                ((symbol-function 'compile-goto-error)
                 (lambda (&rest _) (ert-fail "visited a location"))))
        (with-current-buffer (ygg-qf-buffer) (ygg-qf-open)))
      (should (equal asked (list old)))
      (should (equal (aob-session-decisions a) (list new old))))))

(ert-deftest ygg-qf-decisions-quitting-the-answer-keeps-the-order ()
  (ygg-qf-dec-tests--with
    (let ((old (ygg-qf-dec-tests--ask a "Old"))
          (new (ygg-qf-dec-tests--ask a "New")))
      (cl-letf (((symbol-function 'aob-resolve) (lambda (&rest _) (signal 'quit nil))))
        (condition-case nil
            (ygg-aob--decision-answer (ygg-qf-dec-tests--id a old))
          (quit nil)))
      (should (equal (aob-session-decisions a) (list new old))))))

(ert-deftest ygg-qf-decisions-arrival-during-the-prompt-survives ()
  (ygg-qf-dec-tests--with
    (let ((old (ygg-qf-dec-tests--ask a "Old")) arrived)
      (cl-letf (((symbol-function 'aob-resolve)
                 (lambda (&rest _)
                   (setq arrived (ygg-qf-dec-tests--ask a "New"))
                   (signal 'quit nil))))
        (condition-case nil
            (ygg-aob--decision-answer (ygg-qf-dec-tests--id a old))
          (quit nil)))
      (should (equal (aob-session-decisions a) (list arrived old))))))

(ert-deftest ygg-qf-decisions-resolve-takes-the-decision-asked-for ()
  (ygg-qf-dec-tests--with
    (let ((old (ygg-qf-dec-tests--ask a "Old"))
          (new (ygg-qf-dec-tests--ask a "New"))
          picked)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _) (push prompt picked) "Allow"))
                ((symbol-function 'aob--call) #'ignore))
        (aob-resolve a old)
        (aob-resolve a))
      (should (equal picked '("New: " "Old: ")))
      (should (equal (aob-session-decisions a) (list new old)))
      (should-error (aob-resolve a (list :title "stranger")) :type 'user-error))))

(ert-deftest ygg-qf-decisions-resolve-next-answers-one-lists-many ()
  (ygg-qf-dec-tests--with
    (ygg-qf-dec-tests--ask a "Bash")
    (let (answered listed)
      (cl-letf (((symbol-function 'aob-resolve) (lambda (s &rest _) (push s answered)))
                ((symbol-function 'ygg-aob-decisions) (lambda () (setq listed t))))
        (ygg-aob-resolve-next)
        (should (equal answered (list a)))
        (should-not listed)
        (ygg-qf-dec-tests--ask b "Edit")
        (ygg-aob-resolve-next)
        (should listed)
        (should (= 1 (length answered)))))))

(ert-deftest ygg-qf-decisions-y-bindings-are-gone ()
  (should-not (lookup-key ygg-leader-acp-map "y")))

(ert-deftest ygg-qf-decisions-verbs-act-on-the-id-not-on-point ()
  (ygg-qf-dec-tests--with
    (let ((da (ygg-qf-dec-tests--ask a "Bash"))
          (db (ygg-qf-dec-tests--ask b "Edit"))
          answered opened)
      (ygg-aob-decisions)
      (ygg-qf-dec-tests--goto "alpha · Bash")
      (cl-letf (((symbol-function 'aob-resolve)
                 (lambda (s _d) (push (aob-session-name s) answered)))
                ((symbol-function 'aob-subagents--goto)
                 (lambda (s seq) (push (list (aob-session-name s) seq) opened))))
        (with-current-buffer (ygg-qf-buffer)
          (ygg-aob-decision-answer (ygg-qf-dec-tests--id b db))
          (ygg-aob-decision-open-trace (ygg-qf-dec-tests--id b db))))
      (should (equal answered '("beta")))
      (should (equal opened (list (list "beta" (plist-get db :seq)))))
      (should (memq da (aob-session-decisions a))))))

(ert-deftest ygg-qf-decisions-verbs-are-on-the-embark-map ()
  (should (eq #'ygg-aob-decision-answer (lookup-key ygg-aob-decision-map "y")))
  (should (eq #'ygg-aob-decision-open-trace (lookup-key ygg-aob-decision-map "o")))
  (require 'embark)
  (should (eq 'ygg-aob-decision-map
              (alist-get 'ygg-qf-decisions embark-keymap-alist))))

(ert-deftest ygg-qf-decisions-gone-session-or-answered-decision-is-a-user-error ()
  (ygg-qf-dec-tests--with
    (let ((d (ygg-qf-dec-tests--ask a "Bash")))
      (ygg-qf-dec-tests--clear a d)
      (should-error (ygg-aob--decision-answer (ygg-qf-dec-tests--id a d))
                    :type 'user-error)
      (should-error (ygg-aob--decision-answer (cons "acp:qfdec:none" 1))
                    :type 'user-error))))

(ert-deftest ygg-qf-decisions-refresh-on-hooks-keeps-point-by-id ()
  (ygg-qf-dec-tests--with
    (let ((da (ygg-qf-dec-tests--ask a "Bash")))
      (ygg-qf-dec-tests--ask b "Edit")
      (ygg-aob-decisions)
      (ygg-qf-dec-tests--goto "beta · Edit")
      (let ((dc (ygg-qf-dec-tests--ask a "Fetch")))
        (run-hook-with-args 'aob-state-change-hook a 'working 'blocked)
        (should (string-match-p "alpha · Fetch" (ygg-qf-dec-tests--text)))
        (should (equal (cdr (with-current-buffer (ygg-qf-buffer)
                              (ygg-qf-kind-at-point)))
                       (ygg-qf-dec-tests--id b (car (aob-session-decisions b)))))
        (ygg-qf-dec-tests--clear a dc)
        (ygg-qf-dec-tests--clear a da)
        (should-not (string-match-p "alpha" (ygg-qf-dec-tests--text)))
        (should (string-match-p "beta · Edit" (ygg-qf-dec-tests--text)))
        (should (equal (cdr (with-current-buffer (ygg-qf-buffer)
                              (ygg-qf-kind-at-point)))
                       (ygg-qf-dec-tests--id b (car (aob-session-decisions b)))))))))

(ert-deftest ygg-qf-decisions-removed-session-drops-its-rows ()
  (ygg-qf-dec-tests--with
    (ygg-qf-dec-tests--ask a "Bash")
    (ygg-qf-dec-tests--ask b "Edit")
    (ygg-aob-decisions)
    (aob-remove-session a)
    (should-not (string-match-p "alpha" (ygg-qf-dec-tests--text)))
    (should (string-match-p "beta" (ygg-qf-dec-tests--text)))))

(ert-deftest ygg-qf-decisions-another-writer-ends-the-following ()
  (ygg-qf-dec-tests--with
    (ygg-qf-dec-tests--ask a "Bash")
    (ygg-aob-decisions)
    (let ((hooks (length aob-state-change-hook)))
      (with-current-buffer (ygg-qf-buffer) (ygg-qf-reset))
      (should (= (1- hooks) (length aob-state-change-hook))))))

(ert-deftest ygg-qf-decisions-old-insertion-is-gone ()
  (should-not (fboundp 'ygg-aob--qf-refresh))
  (should-not (fboundp 'ygg-aob-qf-resolve))
  (should-not (boundp 'ygg-aob--qf-line-map))
  (should-not (memq 'ygg-aob--qf-refresh aob-state-change-hook))
  (should-not (memq 'ygg-aob--qf-refresh aob-session-removed-hook)))

(provide 'ygg-qf-decisions-tests)
;;; ygg-qf-decisions-tests.el ends here
