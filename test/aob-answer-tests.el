;;; aob-answer-tests.el --- the questions in a reply, answered as a form -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-answer-sessions-" nil ".eld"))
(require 'aob-answer)

(defmacro aob-answer-tests--with-session (var reply &rest body)
  "Bind VAR to an idle session whose last reply is REPLY around BODY."
  (declare (indent 2))
  `(let ((,var (aob-create-session :id "acp:answer:1" :backend 'acp
                                   :name "answer:1" :project "/tmp/"
                                   :dir "/tmp/" :state 'idle)))
     (unwind-protect
         (progn
           (aob-event ,var 'prompt :text "go")
           (aob-event ,var 'message :text "an earlier reply? it asked this")
           (aob-event ,var 'message :text "a subagent asks? here" :parent "tool-1")
           (when ,reply (aob-event ,var 'message :text ,reply))
           ,@body)
       (ignore-errors (kill-buffer "compose:answer:1"))
       (ignore-errors (kill-buffer "compose:answer:2"))
       (when (aob-session-get (aob-session-id ,var))
         (aob-remove-session ,var)))))

(defmacro aob-answer-tests--sending (var &rest body)
  "Run BODY with each prompt sent collected newest first into VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'aob-prompt)
                (lambda (_s text &rest _) (push text ,var)))
               ((symbol-function 'aob-interject)
                (lambda (_s text) (push text ,var))))
       ,@body)))

(ert-deftest aob-answer-finds-plain-question-lines ()
  (should (equal (aob-answer--questions-in
                  "Done with the parser.\nShould I also update the docs?\nThat is all.")
                 '("Should I also update the docs?"))))

(ert-deftest aob-answer-keeps-list-labels ()
  (should (equal (aob-answer--questions-in
                  "Two things:\n1. Which database do you want?\n2) Keep the old API?\n- Is dark mode needed?\n* A note, not a question.\n+ Ship it today?")
                 '("1. Which database do you want?" "2) Keep the old API?"
                   "- Is dark mode needed?" "+ Ship it today?"))))

(ert-deftest aob-answer-takes-only-the-question-sentence ()
  (should (equal (aob-answer--questions-in
                  "I fixed the bug. Should I add a test? It would take a minute.")
                 '("Should I add a test?")))
  (should (equal (aob-answer--questions-in
                  "The config lives in init.el. Rename it? Or keep it? Your call.")
                 '("Rename it?" "Or keep it?"))))

(ert-deftest aob-answer-skips-fenced-code ()
  (should (equal (aob-answer--questions-in
                  "Here:\n```elisp\n(if (foo?) bar)\nis it nil?\n```\n~~~\nwhat?\n~~~\nDoes this look right?")
                 '("Does this look right?"))))

(ert-deftest aob-answer-drops-duplicates ()
  (should (equal (aob-answer--questions-in
                  "Should I push?\nLater.\n1. Should  I push?\nShould I push?")
                 '("Should I push?"))))

(ert-deftest aob-answer-with-no-questions-errors ()
  (aob-answer-tests--with-session s "All done. Tests pass.\n```\nwhy?\n```"
    (should-error (aob-answer s) :type 'user-error)
    (should-not (get-buffer "compose:answer:1"))))

(ert-deftest aob-answer-with-no-reply-errors ()
  (aob-answer-tests--with-session s nil
    (setf (aob-session-events s)
          (seq-remove (lambda (e) (eq (plist-get e :type) 'message))
                      (aob-session-events s)))
    (should-error (aob-answer s) :type 'user-error)))

(ert-deftest aob-answer-lays-out-the-form ()
  (aob-answer-tests--with-session s "Two things. Postgres or sqlite?\n1. Keep the old API?"
    (let ((buf (aob-answer s)))
      (with-current-buffer buf
        (should (derived-mode-p 'aob-compose-mode))
        (should (equal aob-compose--target (aob-session-id s)))
        (should (equal (buffer-string)
                       "> Postgres or sqlite?\n\n\n> 1. Keep the old API?\n"))
        (should (= (line-number-at-pos) 2))
        (should (eolp))
        (should (bolp))
        (should (member "answers · 2 questions" aob-compose--tags))))))

(ert-deftest aob-answer-reads-the-last-top-level-reply ()
  (aob-answer-tests--with-session s "Merge now?"
    (with-current-buffer (aob-answer s)
      (should (equal aob-answer--questions '("Merge now?"))))))

(ert-deftest aob-answer-send-prunes-unanswered ()
  (aob-answer-tests--with-session s "Postgres or sqlite?\n1. Keep the old API?\n2. Ship today?"
    (let ((buf (aob-answer s)))
      (with-current-buffer buf
        (insert "sqlite")
        (goto-char (point-max))
        (insert "no, friday")
        (aob-answer-tests--sending sent
          (aob-compose-send)
          (should (equal sent
                         '("> Postgres or sqlite?\nsqlite\n\n> 2. Ship today?\nno, friday")))))
      (should-not (buffer-live-p buf)))))

(ert-deftest aob-answer-send-with-nothing-answered-sends-nothing ()
  (aob-answer-tests--with-session s "Postgres or sqlite?\nShip today?"
    (let ((buf (aob-answer s)))
      (with-current-buffer buf
        (aob-answer-tests--sending sent
          (should-error (aob-compose-send) :type 'user-error)
          (should-not sent)))
      (should (buffer-live-p buf)))))

(ert-deftest aob-answer-asked-again-adds-no-second-form ()
  (aob-answer-tests--with-session s "Postgres or sqlite?\nShip today?"
    (let ((buf (aob-answer s)))
      (with-current-buffer buf (insert "sqlite"))
      (aob-answer s)
      (with-current-buffer buf
        (should (equal (buffer-string) "> Postgres or sqlite?\nsqlite\n\n> Ship today?\n"))
        (should (= (point) (point-max)))
        (should (equal aob-answer--questions '("Postgres or sqlite?" "Ship today?")))))))

(ert-deftest aob-answer-asked-again-with-nothing-answered-sends-nothing ()
  (aob-answer-tests--with-session s "Postgres or sqlite?\nShip today?"
    (let ((buf (aob-answer s)))
      (aob-answer s)
      (with-current-buffer buf
        (should (equal (buffer-string) "> Postgres or sqlite?\n\n\n> Ship today?\n"))
        (aob-answer-tests--sending sent
          (should-error (aob-compose-send) :type 'user-error)
          (should-not sent))))))

(ert-deftest aob-answer-leaves-other-compose-buffers-alone ()
  (aob-answer-tests--with-session s "Ship today?"
    (let ((buf (aob-compose s)))
      (with-current-buffer buf
        (insert "> Ship today?\n\nsomething else")
        (should-not aob-answer--questions)
        (aob-answer-tests--sending sent
          (aob-compose-send)
          (should (equal sent '("> Ship today?\n\nsomething else"))))))))

(ert-deftest aob-answer-form-forgotten-when-the-box-is-reopened ()
  (aob-answer-tests--with-session s "Ship today?"
    (let ((buf (aob-answer s)))
      (with-current-buffer buf (aob-compose-abort))
      (with-current-buffer (aob-compose s)
        (should-not aob-answer--questions)
        (should (string-empty-p (buffer-string)))))))

(provide 'aob-answer-tests)
;;; aob-answer-tests.el ends here
