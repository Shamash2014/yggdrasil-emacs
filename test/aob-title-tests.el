;;; aob-title-tests.el --- Tests for generated session titles -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(defvar aob-acp-persist-file)
(setq aob-acp-persist-file (make-temp-file "aob-title-sessions-" nil ".eld"))
(require 'aob)
(require 'aob-acp)

(defvar aob-title-tests--runs nil)

(defconst aob-title-tests--models
  [(:id "model" :category "model" :currentValue "prov/big-1"
    :options [(:value "prov/big-1" :name "prov/Big")
              (:value "prov/claude-haiku-4" :name "prov/Haiku 4")])])

(defmacro aob-title-tests--with (&rest body)
  (declare (indent 0))
  `(let ((aob-title-tests--runs nil)
         (aob-acp-title-model "haiku")
         (aob-acp-title-timeout 30))
     (cl-letf (((symbol-function 'aob--set-name)
                (lambda (s name) (setf (aob-session-name s) name)))
               ((symbol-function 'aob-live-sessions) (lambda () nil))
               ((symbol-function 'executable-find) (lambda (&rest _) "/bin/pi"))
               ((symbol-function 'aob-acp--title-run)
                (lambda (_s argv done) (push argv aob-title-tests--runs) (funcall done "Fix login"))))
       ,@body)))

(defun aob-title-tests--session (&optional models)
  (let ((s (aob-session--create :name "pi:1" :dir "/tmp/proj/" :project "/tmp/proj")))
    (aob-session-put s :agent "pi")
    (aob-session-put s :config-options (if (eq models 'none) nil (or models aob-title-tests--models)))
    s))

(defun aob-title-tests--ask (s &optional text)
  (aob-acp--title-ask s (or text "is it safe to merge this")))

(ert-deftest aob-title-pi-argv-with-resolved-model ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-title-tests--ask s)
      (let ((argv (car aob-title-tests--runs)))
        (should (equal (cl-subseq argv 0 3) '("pi" "-p" "--no-session")))
        (should (member "--no-tools" argv))
        (should (equal (cadr (member "--model" argv)) "prov/claude-haiku-4"))
        (should (equal (cadr (member "--" argv))
                       (car (last argv))))
        (should (string-match-p "3-6 word title" (car (last argv))))
        (should (string-match-p "is it safe to merge this" (car (last argv))))))))

(ert-deftest aob-title-pi-argv-falls-back-to-current-model ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)) (aob-acp-title-model "luna"))
      (aob-title-tests--ask s)
      (should (equal (cadr (member "--model" (car aob-title-tests--runs))) "prov/big-1")))))

(ert-deftest aob-title-pi-argv-omits-model-when-none-offered ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session 'none)))
      (aob-title-tests--ask s)
      (let ((argv (car aob-title-tests--runs)))
        (should-not (member "--model" argv))
        (should (equal (last argv 2) (list "--" (car (last argv)))))))))

(ert-deftest aob-title-runs-in-session-dir-with-profile-environment ()
  (let (seen-env seen-dir)
    (let ((aob-acp-title-model "haiku")
          (aob-acp-environment-function
           (lambda (&rest _) (list "PI_CODING_AGENT_DIR=/profile/dir")))
          (aob-acp-title-timeout 30))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq seen-env process-environment seen-dir default-directory)
                   (make-pipe-process :name "aob-title-fake" :noquery t)))
                ((symbol-function 'process-send-eof) #'ignore))
        (let ((s (aob-title-tests--session)))
          (aob-acp--title-run s '("pi" "-p") #'ignore)
          (should (seq-some (lambda (v) (string-prefix-p "PI_CODING_AGENT_DIR=" v)) seen-env))
          (should (seq-some (lambda (v) (string-prefix-p "PATH=" v)) seen-env))
          (should (equal seen-dir "/tmp/proj/")))))
    (mapc #'delete-process (seq-filter (lambda (p) (equal (process-name p) "aob-title-fake"))
                                       (process-list)))))

(ert-deftest aob-title-reply-applied-as-generated ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-acp--auto-name s 'prompt "is it safe to merge this")
      (aob-title-tests--ask s)
      (should (equal (aob-session-name s) "pi: Fix login"))
      (should (eq (aob-session-ref s :auto-named) 'generated)))))

(ert-deftest aob-title-real-runner-takes-first-output-line ()
  (let ((aob-acp-title-timeout 30) got)
    (let ((s (aob-title-tests--session)))
      (aob-acp--title-run s '("sh" "-c" "printf '\\nFix the login test\\nmore\\n'")
                          (lambda (title) (setq got (list title))))
      (with-timeout (5 nil) (while (null got) (accept-process-output nil 0.05)))
      (should (equal got '("Fix the login test"))))))

(ert-deftest aob-title-adapter-title-overrides-generated ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-acp--auto-name s 'prompt "is it safe")
      (aob-title-tests--ask s)
      (aob-acp--auto-name s 'title "Merge safety")
      (should (equal (aob-session-name s) "pi: Merge safety"))
      (aob-acp--auto-name s 'generated "Other words")
      (should (equal (aob-session-name s) "pi: Merge safety")))))

(ert-deftest aob-title-generated-beats-goal ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-acp--auto-name s 'goal "Ship the release")
      (aob-title-tests--ask s)
      (should (equal (aob-session-name s) "pi: Fix login")))))

(ert-deftest aob-title-not-asked-when-adapter-titled ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-acp--auto-name s 'title "Adapter title")
      (aob-title-tests--ask s)
      (should-not aob-title-tests--runs))))

(ert-deftest aob-title-not-asked-when-user-named ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-session-put s :named-by-user t)
      (aob-title-tests--ask s)
      (should-not aob-title-tests--runs))))

(ert-deftest aob-title-not-asked-when-model-nil ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)) (aob-acp-title-model nil))
      (aob-title-tests--ask s)
      (should-not aob-title-tests--runs))))

(ert-deftest aob-title-not-asked-for-an-agent-that-titles-itself ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-session-put s :agent "claude")
      (aob-title-tests--ask s)
      (should-not aob-title-tests--runs))))

(ert-deftest aob-title-not-asked-when-auto-name-off ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)) (aob-auto-name nil))
      (aob-title-tests--ask s)
      (should-not aob-title-tests--runs))))

(ert-deftest aob-title-not-asked-for-a-remote-dir ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (setf (aob-session-dir s) "/ssh:host:/home/me/proj/")
      (aob-title-tests--ask s)
      (should-not aob-title-tests--runs))))

(ert-deftest aob-title-not-asked-without-the-executable ()
  (aob-title-tests--with
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (let ((s (aob-title-tests--session)))
        (aob-title-tests--ask s)
        (should-not aob-title-tests--runs)))))

(ert-deftest aob-title-asked-once ()
  (aob-title-tests--with
    (let ((s (aob-title-tests--session)))
      (aob-title-tests--ask s)
      (aob-title-tests--ask s "again")
      (should (= 1 (length aob-title-tests--runs))))))

(ert-deftest aob-title-failure-leaves-name-unchanged ()
  (aob-title-tests--with
    (cl-letf (((symbol-function 'aob-acp--title-run)
               (lambda (_s _argv done) (funcall done nil))))
      (let ((s (aob-title-tests--session)))
        (aob-acp--auto-name s 'prompt "is it safe")
        (aob-title-tests--ask s "is it safe")
        (should (equal (aob-session-name s) "pi: is it safe"))
        (should (eq (aob-session-ref s :auto-named) 'prompt))))))

(ert-deftest aob-title-timeout-kills-and-leaves-name ()
  (let ((aob-acp-title-timeout 0.2) (got 'unset))
    (cl-letf (((symbol-function 'aob--set-name)
               (lambda (s name) (setf (aob-session-name s) name))))
      (let ((s (aob-title-tests--session)))
        (aob-acp--auto-name s 'prompt "is it safe")
        (aob-acp--title-run s '("sleep" "30") (lambda (title) (setq got title)))
        (with-timeout (5 nil) (while (eq got 'unset) (accept-process-output nil 0.05)))
        (should (null got))
        (should (equal (aob-session-name s) "pi: is it safe"))))))

(provide 'aob-title-tests)
