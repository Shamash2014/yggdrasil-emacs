;;; aob-one-key-tests.el --- one-key permission answers and mode cycling -*- lexical-binding: t; -*-

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
  (setq aob-acp-persist-file (make-temp-file "aob-onekey-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'layer-quickfix)
(require 'layer-aob)

(defconst aob-one-key-tests--options
  [(:optionId "r1" :name "Deny once" :kind "reject_once")
   (:optionId "a2" :name "Always" :kind "allow_always")
   (:optionId "o3" :name "Once" :kind "allow_once")
   (:optionId "r4" :name "Never" :kind "reject_always")])

(defmacro aob-one-key-tests--with (&rest body)
  (declare (indent 0))
  `(let* ((aob-trace-icons nil)
          (s (aob-create-session :id "acp:onekey:a" :backend 'acp :name "alpha"
                                 :project "/tmp/proj/" :dir "/tmp/proj/"
                                 :state 'working)))
     (unwind-protect (progn ,@body)
       (when-let* ((buf (get-buffer "*quickfix*"))) (kill-buffer buf))
       (when (aob-session-get (aob-session-id s)) (aob-remove-session s)))))

(defun aob-one-key-tests--ask (s options)
  (let* ((d (list :kind nil :title "Bash" :options (append options nil)))
         (ev (aob-event s 'permission :title "Bash")))
    (nconc d (list :seq (plist-get ev :seq)))
    (push d (aob-session-decisions s))
    d))

(defmacro aob-one-key-tests--answers (var &rest body)
  (declare (indent 1))
  `(let (,var)
     (cl-letf (((symbol-function 'aob--call)
                (lambda (_s key _d id) (when (eq key :resolve) (push id ,var))))
               ((symbol-function 'aob-ask-reject-reason) #'ignore))
       ,@body
       (car ,var))))

(defun aob-one-key-tests--press (char fn)
  (let ((last-command-event char)) (funcall fn)))

(defun aob-one-key-tests--on-row ()
  (ygg-aob-decisions)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward "alpha")
    (beginning-of-line)))

(ert-deftest aob-one-key-digit-picks-the-nth-as-sent ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s aob-one-key-tests--options)
    (aob-one-key-tests--on-row)
    (with-current-buffer (ygg-qf-buffer)
      (should (equal "o3" (aob-one-key-tests--answers got
                            (aob-one-key-tests--press ?3 #'ygg-aob-decision-key))))
      (should (equal "r1" (aob-one-key-tests--answers got
                            (aob-one-key-tests--press ?1 #'ygg-aob-decision-key)))))))

(ert-deftest aob-one-key-letters-pick-by-kind ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s aob-one-key-tests--options)
    (aob-one-key-tests--on-row)
    (with-current-buffer (ygg-qf-buffer)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (cl-loop for (ch . id) in '((?y . "o3") (?Y . "a2") (?n . "r1") (?N . "r4"))
                 do (should (equal id (aob-one-key-tests--answers got
                                        (aob-one-key-tests--press ch #'ygg-aob-decision-key)))))))))

(ert-deftest aob-one-key-missing-kind-and-big-digit-name-the-offer ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s [(:optionId "o" :name "Once" :kind "allow_once")])
    (aob-one-key-tests--on-row)
    (with-current-buffer (ygg-qf-buffer)
      (aob-one-key-tests--answers got
        (let ((err (should-error (aob-one-key-tests--press ?N #'ygg-aob-decision-key)
                                 :type 'user-error)))
          (should (string-match-p "reject_always.*1 Once" (cadr err))))
        (should-error (aob-one-key-tests--press ?2 #'ygg-aob-decision-key)
                      :type 'user-error)
        (should-not got)))))

(ert-deftest aob-one-key-decision-rows-show-the-digits ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s aob-one-key-tests--options)
    (ygg-aob-decisions)
    (let ((text (with-current-buffer (ygg-qf-buffer)
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (should (string-match-p "1 Deny once · 2 Always · 3 Once · 4 Never" text)))))

(ert-deftest aob-one-key-qf-keys-are-live-in-the-decisions-buffer ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s aob-one-key-tests--options)
    (aob-one-key-tests--on-row)
    (with-current-buffer (ygg-qf-buffer)
      (dolist (k '("1" "9" "y" "Y" "n" "N"))
        (should (eq 'ygg-aob-decision-key (aob-one-key-tests--bound k)))))))

(defmacro aob-one-key-tests--trace (&rest body)
  (declare (indent 0))
  `(aob-one-key-tests--with
     (let ((d (aob-one-key-tests--ask s aob-one-key-tests--options)))
       (with-temp-buffer
         (insert (propertize "Bash wants to run\n" 'aob-event (plist-get d :seq))
                 "agent said hello world\n")
         (aob-trace-mode)
         (setq-local aob-trace--session-id (aob-session-id s))
         (ygg-normal-state)
         ,@body))))

(defun aob-one-key-tests--at (needle)
  (goto-char (point-min))
  (search-forward needle)
  (goto-char (match-beginning 0)))

(defun aob-one-key-tests--bound (key)
  (key-binding (kbd key)))

(defmacro aob-one-key-tests--confirming (answer &rest body)
  (declare (indent 1))
  `(let (asked)
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (p) (push p asked) ,answer)))
       ,@body
       (nreverse asked))))

(ert-deftest aob-one-key-trace-digits-answer-on-the-permission-block ()
  (aob-one-key-tests--trace
    (aob-one-key-tests--at "Bash")
    (should (eq 'ygg-aob-trace-key (aob-one-key-tests--bound "2")))
    (should (equal "o3" (aob-one-key-tests--answers got
                          (aob-one-key-tests--press ?3 #'ygg-aob-trace-key))))
    (should (equal "r4" (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                          (aob-one-key-tests--answers got
                            (aob-one-key-tests--press ?4 #'ygg-aob-trace-key)))))))

(ert-deftest aob-one-key-trace-letters-stay-modal-on-the-block ()
  (aob-one-key-tests--trace
    (let (off)
      (aob-one-key-tests--at "hello")
      (setq off (mapcar #'aob-one-key-tests--bound '("y" "Y" "n" "N")))
      (aob-one-key-tests--at "Bash")
      (should (equal off (mapcar #'aob-one-key-tests--bound '("y" "Y" "n" "N"))))
      (should (eq 'ygg-search-next (aob-one-key-tests--bound "n")))
      (should (eq 'ygg-search-prev (aob-one-key-tests--bound "N"))))))

(ert-deftest aob-one-key-trace-allow-always-asks-first ()
  (aob-one-key-tests--trace
    (aob-one-key-tests--at "Bash")
    (let (got)
      (should (equal '("Allow always: Always? ")
                     (aob-one-key-tests--confirming t
                       (setq got (aob-one-key-tests--answers r
                                   (aob-one-key-tests--press ?2 #'ygg-aob-trace-key))))))
      (should (equal "a2" got))
      (setq got nil)
      (should (equal '("Allow always: Always? ")
                     (aob-one-key-tests--confirming nil
                       (aob-one-key-tests--answers r
                         (should-error (aob-one-key-tests--press ?2 #'ygg-aob-trace-key)
                                       :type 'user-error)
                         (setq got r)))))
      (should-not got))))

(ert-deftest aob-one-key-trace-other-options-do-not-ask ()
  (aob-one-key-tests--trace
    (aob-one-key-tests--at "Bash")
    (should-not (aob-one-key-tests--confirming t
                  (aob-one-key-tests--answers got
                    (aob-one-key-tests--press ?3 #'ygg-aob-trace-key))))))

(ert-deftest aob-one-key-trace-keys-fall-through-off-the-block ()
  (aob-one-key-tests--trace
    (aob-one-key-tests--at "hello")
    (dolist (k '("y" "Y" "n" "N" "1" "3"))
      (should-not (eq 'ygg-aob-trace-key (aob-one-key-tests--bound k))))
    (should (eq (lookup-key ygg-normal-map "y") (aob-one-key-tests--bound "y")))
    (should (eq 'ygg-search-next (aob-one-key-tests--bound "n")))
    (should (eq 'ygg-search-prev (aob-one-key-tests--bound "N")))
    (should (eq (lookup-key ygg-normal-map "3") (aob-one-key-tests--bound "3")))))

(ert-deftest aob-one-key-trace-yank-operator-never-answers ()
  (aob-one-key-tests--trace
    (aob-one-key-tests--at "Bash")
    (kill-new "")
    (let (resolved)
      (cl-letf (((symbol-function 'aob--call)
                 (lambda (&rest a) (push a resolved))))
        (setq unread-command-events (list ?i ?w))
        (let ((last-command-event ?y))
          (call-interactively (aob-one-key-tests--bound "y"))))
      (should-not resolved)
      (should (string-prefix-p "Bash" (car kill-ring))))))

(ert-deftest aob-one-key-trace-visual-digits-do-not-answer-on-the-block ()
  (aob-one-key-tests--trace
    (aob-one-key-tests--at "Bash")
    (set-mark (point))
    (goto-char (+ (point) 4))
    (activate-mark)
    (should-not (eq 'ygg-aob-trace-key (aob-one-key-tests--bound "2")))))

(ert-deftest aob-one-key-qf-keys-fall-through-once-the-kind-is-gone ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s aob-one-key-tests--options)
    (aob-one-key-tests--on-row)
    (with-current-buffer (ygg-qf-buffer)
      (should (eq 'ygg-aob-decision-key (aob-one-key-tests--bound "y")))
      (ygg-qf-reset)
      (let ((inhibit-read-only t)) (erase-buffer) (insert "other.el:1:x\n"))
      (goto-char (point-min))
      (should (eq (lookup-key ygg-normal-map "y") (aob-one-key-tests--bound "y")))
      (should (eq (lookup-key ygg-normal-map "3") (aob-one-key-tests--bound "3"))))))

(ert-deftest aob-one-key-trace-question-block-is-not-answered-by-keys ()
  (aob-one-key-tests--with
    (let ((d (aob-one-key-tests--ask s nil)))
      (plist-put d :kind 'elicitation)
      (with-temp-buffer
        (insert (propertize "Question\n" 'aob-event (plist-get d :seq)))
        (aob-trace-mode)
        (setq-local aob-trace--session-id (aob-session-id s))
        (ygg-normal-state)
        (goto-char (point-min))
        (should-not (eq 'ygg-aob-trace-key (aob-one-key-tests--bound "y")))))))

(defvar aob-one-key-tests--modes
  '((:id "ask" :name "Ask") (:id "plan" :name "Plan") (:id "go" :name "Go")))

(defmacro aob-one-key-tests--modes-with (now &rest body)
  (declare (indent 1))
  `(aob-one-key-tests--with
     (aob-session-put s :modes (list :availableModes aob-one-key-tests--modes))
     (aob-session-put s :mode-id ,now)
     (let (put)
       (cl-letf (((symbol-function 'aob-acp--put-mode)
                  (lambda (_s id) (push id put) (aob-session-put s :mode-id id)))
                 ((symbol-function 'message) #'ignore))
         ,@body
         (nreverse put)))))

(ert-deftest aob-one-key-mode-next-wraps ()
  (should (equal '("plan" "go" "ask")
                 (aob-one-key-tests--modes-with "ask"
                   (dotimes (_ 3) (aob-mode-next s))))))

(ert-deftest aob-one-key-mode-prev-wraps ()
  (should (equal '("go" "plan" "ask")
                 (aob-one-key-tests--modes-with "ask"
                   (dotimes (_ 3) (aob-mode-prev s))))))

(ert-deftest aob-one-key-mode-echoes-the-name ()
  (aob-one-key-tests--with
    (aob-session-put s :modes (list :availableModes aob-one-key-tests--modes))
    (aob-session-put s :mode-id "ask")
    (let (said)
      (cl-letf (((symbol-function 'aob-acp--put-mode) #'ignore)
                ((symbol-function 'message) (lambda (f &rest a) (setq said (apply #'format f a)))))
        (aob-mode-next s))
      (should (string-match-p "Plan" said)))))

(ert-deftest aob-one-key-no-modes-is-a-user-error ()
  (aob-one-key-tests--with
    (aob-session-put s :agent "nothing-known")
    (should-error (aob-mode-next s) :type 'user-error)
    (should-error (aob-mode-prev s) :type 'user-error)))

(ert-deftest aob-one-key-leader-alias-is-repeatable ()
  (let ((this-command 'aob-acp-cycle-mode))
    (should (eq 'aob-mode-repeat-map (repeat--command-property 'repeat-map)))))

(ert-deftest aob-one-key-mode-commands-are-repeatable ()
  (dolist (cmd '(aob-mode-next aob-mode-prev aob-acp-cycle-mode))
    (should (eq 'aob-mode-repeat-map (function-get cmd 'repeat-map))))
  (should (eq 'aob-mode-next (lookup-key aob-mode-repeat-map "n")))
  (should (eq 'aob-mode-prev (lookup-key aob-mode-repeat-map "p"))))

(defun aob-one-key-tests--leader (mode key)
  (let ((def (lookup-key (ygg-localleader--get-map mode) (kbd key))))
    (if (and (consp def) (stringp (car def))) (cdr def) def)))

(ert-deftest aob-one-key-localleader-keys-resolve ()
  (dolist (mode '(aob-trace-mode aob-plan-mode))
    (should (eq 'aob-acp-cycle-mode (aob-one-key-tests--leader mode "m n")))
    (should (eq 'aob-mode-prev (aob-one-key-tests--leader mode "m p")))))

(provide 'aob-one-key-tests)
;;; aob-one-key-tests.el ends here

(defun aob-one-key-tests--always-cases (fn)
  (dolist (case '((?Y "Allow always: Always? " "a2")
                  (?N "Reject always: Never? " "r4")
                  (?2 "Allow always: Always? " "a2")
                  (?4 "Reject always: Never? " "r4")))
    (pcase-let ((`(,key ,prompt ,id) case)
                (got nil))
      (should (equal (list prompt)
                     (aob-one-key-tests--confirming t
                       (setq got (aob-one-key-tests--answers r
                                   (aob-one-key-tests--press key fn))))))
      (should (equal id got))
      (setq got :none)
      (should (equal (list prompt)
                     (aob-one-key-tests--confirming nil
                       (aob-one-key-tests--answers r
                         (should-error (aob-one-key-tests--press key fn) :type 'user-error)
                         (setq got r)))))
      (should-not got))))

(ert-deftest aob-one-key-trace-always-options-ask-first ()
  (aob-one-key-tests--trace
    (aob-one-key-tests--at "Bash")
    (aob-one-key-tests--always-cases #'ygg-aob-trace-key)))

(ert-deftest aob-one-key-qf-always-options-ask-first ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s aob-one-key-tests--options)
    (aob-one-key-tests--on-row)
    (with-current-buffer (ygg-qf-buffer)
      (aob-one-key-tests--always-cases #'ygg-aob-decision-key))))

(ert-deftest aob-one-key-qf-once-options-do-not-ask ()
  (aob-one-key-tests--with
    (aob-one-key-tests--ask s aob-one-key-tests--options)
    (aob-one-key-tests--on-row)
    (with-current-buffer (ygg-qf-buffer)
      (should-not (aob-one-key-tests--confirming t
                    (dolist (ch '(?y ?n ?1 ?3))
                      (aob-one-key-tests--answers got
                        (aob-one-key-tests--press ch #'ygg-aob-decision-key))))))))
