;;; ygg-git-compare-tour-tests.el --- a compare walked in an agent's order -*- lexical-binding: t; -*-

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (builds (expand-file-name "../elpaca/builds/" here)))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds)))
  (add-to-list 'load-path (expand-file-name "../lisp/agent-objects" here)))

(with-suppressed-warnings ((lexical features)) (defvar features))
(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'aob-mcp)
(require 'aob-mcp-tools)
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'ygg-git-compare)
(require 'ygg-git-compare-marks)
(require 'ygg-git-compare-tour)

(defun ygg-git-compare-tour-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun ygg-git-compare-tour-tests--text (&rest changes)
  "Forty numbered lines, each (N . TEXT) of CHANGES putting TEXT at line N."
  (mapconcat (lambda (n) (concat (or (alist-get n changes) (format "line %d" n)) "\n"))
             (number-sequence 1 40) ""))

(defmacro ygg-git-compare-tour-tests--with-compare (root &rest body)
  "BODY in a compare of main against branch feature in the repository ROOT.
Feature changes a.txt at lines 5, 20 and 35 and b.txt at line 2."
  (declare (indent 1))
  `(let* ((process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (,root (file-name-as-directory
                  (file-truename (make-temp-file "ygg-git-compare-tour-" t))))
          (default-directory ,root)
          (magit-refresh-verbose nil)
          (inhibit-message t))
     (unwind-protect
         (cl-flet ((git (&rest args) (apply #'ygg-git-compare-tour-tests--git ,root args))
                   (write (file text) (with-temp-file (expand-file-name file ,root)
                                        (insert text))))
           (git "init" "-q" "-b" "main")
           (git "config" "user.name" "Tour Test")
           (git "config" "user.email" "tour@example.invalid")
           (git "config" "commit.gpgsign" "false")
           (write "a.txt" (ygg-git-compare-tour-tests--text))
           (write "b.txt" "one\ntwo\nthree\n")
           (git "add" ".")
           (git "commit" "-q" "-m" "base")
           (git "checkout" "-q" "-b" "feature")
           (write "a.txt" (ygg-git-compare-tour-tests--text
                           '(5 . "five") '(20 . "twenty") '(35 . "thirty-five")))
           (write "b.txt" "one\nTWO\nthree\n")
           (git "commit" "-q" "-am" "feature")
           (git "checkout" "-q" "main")
           (with-current-buffer (ygg-git-compare-buffer ,root '(rev . "main")
                                                        '(rev . "feature"))
             ,@body))
       (dolist (b (buffer-list))
         (when (and (with-current-buffer b (derived-mode-p 'magit-mode))
                    (file-in-directory-p (buffer-local-value 'default-directory b) ,root))
           (kill-buffer b)))
       (delete-directory ,root t))))

(defconst ygg-git-compare-tour-tests--steps
  '((:title "core" :check "five and twenty agree" :risk "medium"
     :hunks ((:file "a.txt" :start 5 :end 5 :side "new")
             (:file "a.txt" :start 20 :end 20 :side "new")))
    (:title "b side" :check "two is upper case" :risk nil
     :hunks ((:file "b.txt" :start 2 :end 2 :side "new"))))
  "A tour leaving a.txt's third hunk out.")

(defun ygg-git-compare-tour-tests--hunk (text)
  (seq-find (lambda (h) (string-match-p (regexp-quote text)
                                        (buffer-substring (oref h content) (oref h end))))
            (ygg-git-compare-marks--hunks)))

(defun ygg-git-compare-tour-tests--open (text)
  (let ((hunk (ygg-git-compare-tour-tests--hunk text)))
    (not (or (oref hunk hidden) (oref (oref hunk parent) hidden)))))

(defun ygg-git-compare-tour-tests--deliver (root &optional steps)
  (ygg-git-compare-tour-receive root "feature" (or steps ygg-git-compare-tour-tests--steps)
                                "tester"))

(defun ygg-git-compare-tour-tests--rev (root rev)
  (ygg-git-compare-tour-tests--git root "rev-parse" rev))

(defun ygg-git-compare-tour-tests--file (root)
  (expand-file-name ".git/reviews/feature.tour.json" root))

;;; The MCP tool

(defun ygg-git-compare-tour-tests--call (args)
  "Call review_tour with ARGS: (ANSWER RELAYED RECEIVED)."
  (let ((handler (plist-get (gethash "review_tour" aob-mcp--tools) :handler))
        form received)
    (cl-letf (((symbol-function 'aob-mcp-relay)
               (lambda (_conn _id f &rest _) (setq form f) aob-mcp-deferred))
              ((symbol-function 'aob-mcp-host-session) (lambda (_) nil))
              ((symbol-function 'ygg-git-compare-tour-receive)
               (lambda (&rest got) (setq received got) (cons (length (nth 2 got)) 0))))
      (let ((direct (funcall handler args nil 1))
            (features (cons 'ygg-git-compare features)))
        (list (if form (eval form t) direct) (and form t) received)))))

(ert-deftest ygg-git-compare-tour-tool-delivers-validated-steps ()
  (pcase-let ((`(,answer ,relayed (,dir ,branch ,steps ,author))
               (ygg-git-compare-tour-tests--call
                '(:dir "/repo" :branch "feat" :author "me"
                  :steps ((:title "core" :check "agree" :risk "high"
                           :hunks ((:file "a.el" :start 5 :end 9)
                                   (:file "b.el" :start "3" :side "old"))))))))
    (should relayed)
    (should (equal (list dir branch author) '("/repo" "feat" "me")))
    (should (equal steps '((:title "core" :check "agree" :risk "high"
                            :hunks ((:file "a.el" :start 5 :end 9 :side "new")
                                    (:file "b.el" :start 3 :end 3 :side "old"))))))
    (should (string-match-p "1 step delivered for feat" (car answer)))))

(ert-deftest ygg-git-compare-tour-tool-parses-json-steps ()
  (pcase-let ((`(,_ ,_ (,_ ,_ ,steps ,_))
               (ygg-git-compare-tour-tests--call
                '(:dir "/repo" :branch "feat"
                  :steps "[{\"title\":\"t\",\"check\":\"c\",\"hunks\":[{\"file\":\"a.el\",\"start\":2}]}]"))))
    (should (equal (plist-get (car steps) :hunks)
                   '((:file "a.el" :start 2 :end 2 :side "new"))))))

(ert-deftest ygg-git-compare-tour-tool-refuses-bad-steps ()
  (dolist (case '(((:steps ((:check "c" :hunks ((:file "a" :start 1))))) . "title missing")
                  ((:steps ((:title "t" :hunks ((:file "a" :start 1))))) . "check missing")
                  ((:steps ((:title "t" :check "c" :hunks nil))) . "non-empty array")
                  ((:steps ((:title "t" :check "c" :risk "dire" :hunks ((:file "a" :start 1)))))
                   . "risk")
                  ((:steps ((:title "t" :check "c" :hunks ((:file "a" :start 4 :end 2)))))
                   . "end")
                  ((:steps ((:title "t" :check "c" :hunks ((:file "a" :start 1 :side "up")))))
                   . "side")
                  ((:steps nil) . "no steps")))
    (pcase-let ((`(,answer ,relayed ,received)
                 (ygg-git-compare-tour-tests--call
                  (append '(:dir "/repo" :branch "feat") (car case)))))
      (should-not relayed)
      (should-not received)
      (should (string-match-p (regexp-quote (cdr case)) answer))))
  (should (string-match-p "which branch" (car (ygg-git-compare-tour-tests--call
                                               '(:dir "/repo" :steps nil))))))

(ert-deftest ygg-git-compare-tour-steps-carry-notes-only ()
  (pcase-let ((`(,_ ,_ (,_ ,_ ,steps ,_))
               (ygg-git-compare-tour-tests--call
                '(:dir "/repo" :branch "feat"
                  :steps ((:title "t" :check "c" :comments ((:text "nit")) :prompt "do it"
                           :hunks ((:file "a.el" :start 2 :text "x" :type "fix")))))))
              (tool (gethash "review_tour" aob-mcp--tools)))
    (should (equal (cl-loop for (k _) on (car steps) by #'cddr collect k)
                   '(:title :check :risk :hunks)))
    (should (equal (cl-loop for (k _) on (car (plist-get (car steps) :hunks)) by #'cddr
                            collect k)
                   '(:file :start :end :side)))
    (should-not (seq-find (lambda (a) (member (plist-get a :name) '("comments" "prompt")))
                          (plist-get tool :args)))))

;;; The compare

(ert-deftest ygg-git-compare-tour-steps-resolve-to-hunks ()
  (ygg-git-compare-tour-tests--with-compare root
    (should (equal '(2 . 0) (ygg-git-compare-tour-tests--deliver root)))
    (pcase-let ((`(,core ,other) ygg-git-compare-tour--steps))
      (should-not (plist-get core :stale))
      (should (equal '("a.txt" "a.txt") (mapcar (lambda (e) (plist-get e :file))
                                                (plist-get core :hunks))))
      (should (equal '(2 17) (mapcar (lambda (e) (plist-get e :start))
                                     (plist-get core :hunks))))
      (should (equal '("b.txt") (mapcar (lambda (e) (plist-get e :file))
                                        (plist-get other :hunks)))))
    (should (equal (list (ygg-git-compare-tour-tests--hunk "five")
                         (ygg-git-compare-tour-tests--hunk "twenty"))
                   (ygg-git-compare-tour--sections (car ygg-git-compare-tour--steps))))))

(ert-deftest ygg-git-compare-tour-unmatched-lines-make-a-stale-step ()
  (ygg-git-compare-tour-tests--with-compare root
    (should (equal '(1 . 1)
                   (ygg-git-compare-tour-tests--deliver
                    root '((:title "nowhere" :check "c"
                            :hunks ((:file "a.txt" :start 12 :end 13 :side "new")))))))
    (should (plist-get (car ygg-git-compare-tour--steps) :stale))))

(ert-deftest ygg-git-compare-tour-next-and-previous-narrow-to-the-step ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (ygg-git-compare-tour-next)
    (should (= 1 ygg-git-compare-tour--index))
    (should (ygg-git-compare-tour-tests--open "five"))
    (should (ygg-git-compare-tour-tests--open "twenty"))
    (should-not (ygg-git-compare-tour-tests--open "thirty-five"))
    (should-not (ygg-git-compare-tour-tests--open "TWO"))
    (should (string-match-p "step 1/3 — core  risk: medium  check: five and twenty agree"
                            ygg-git-compare--tour-status))
    (ygg-git-compare-tour-next)
    (should (= 2 ygg-git-compare-tour--index))
    (should (ygg-git-compare-tour-tests--open "TWO"))
    (should-not (ygg-git-compare-tour-tests--open "five"))
    (should (string-match-p "step 2/3 — b side" ygg-git-compare--tour-status))
    (ygg-git-compare-tour-previous)
    (should (= 1 ygg-git-compare-tour--index))
    (should (ygg-git-compare-tour-tests--open "five"))
    (should-error (ygg-git-compare-tour-previous) :type 'user-error)
    (should (string-match-p "step 1/3" (format "%s" (ygg-git-compare--header))))))

(ert-deftest ygg-git-compare-tour-leaving-a-step-marks-its-hunks-reviewed ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (ygg-git-compare-tour-next)
    (let ((marks (ygg-git-compare-marks--read)))
      (should-not (gethash (ygg-git-compare-marks--key
                            (ygg-git-compare-tour-tests--hunk "five"))
                           marks)))
    (ygg-git-compare-tour-next)
    (let ((marks (ygg-git-compare-marks--read)))
      (dolist (text '("five" "twenty"))
        (should (gethash (ygg-git-compare-marks--key (ygg-git-compare-tour-tests--hunk text))
                         marks)))
      (should-not (gethash (ygg-git-compare-marks--key
                            (ygg-git-compare-tour-tests--hunk "TWO"))
                           marks)))
    (ygg-git-compare-tour-next)
    (should (gethash (ygg-git-compare-marks--key (ygg-git-compare-tour-tests--hunk "TWO"))
                     (ygg-git-compare-marks--read)))))

(ert-deftest ygg-git-compare-tour-leftover-step-lists-uncovered-hunks-last ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (let* ((steps (ygg-git-compare-tour-steps))
           (left (car (last steps))))
      (should (= 3 (length steps)))
      (should (plist-get left :leftover))
      (should (string-match-p "not in any step (1 hunk)" (plist-get left :title)))
      (should (equal (list (ygg-git-compare-tour-tests--hunk "thirty-five"))
                     (ygg-git-compare-tour--sections left))))
    (ygg-git-compare-tour-goto 3)
    (should (ygg-git-compare-tour-tests--open "thirty-five"))
    (should-not (ygg-git-compare-tour-tests--open "five"))
    (should (string-match-p "step 3/3 — not in any step" ygg-git-compare--tour-status))
    (ygg-git-compare-tour-next)
    (should-not ygg-git-compare-tour--index)
    (should-not ygg-git-compare--tour-status)))

(ert-deftest ygg-git-compare-tour-leave-folds-the-compare-again ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (ygg-git-compare-tour-goto 1)
    (ygg-git-compare-tour-leave)
    (should-not ygg-git-compare-tour--index)
    (should-not ygg-git-compare--tour-status)
    (should-not (ygg-git-compare-tour-tests--open "twenty"))))

(ert-deftest ygg-git-compare-tour-persists-per-base-and-head ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (let* ((file (ygg-git-compare-tour-tests--file root))
           (kept (ygg-git-compare-tour--read file))
           (key (ygg-git-compare-tour--key)))
      (should (equal key (format "%s..%s" (plist-get kept :base) (plist-get kept :head))))
      (should (equal (ygg-git-compare-tour-tests--rev root "main") (plist-get kept :base)))
      (should (equal (ygg-git-compare-tour-tests--rev root "feature") (plist-get kept :head)))
      (should (equal '("core" "b side")
                     (mapcar (lambda (s) (plist-get s :title)) (plist-get kept :steps))))
      (should (plist-get (car (plist-get (car (plist-get kept :steps)) :hunks)) :key))
      (setq ygg-git-compare-tour--steps nil
            ygg-git-compare-tour--loaded nil)
      (ygg-git-compare-tour--ensure)
      (should (= 2 (length ygg-git-compare-tour--steps)))
      (should (equal key ygg-git-compare-tour--loaded))
      (should-not (seq-some (lambda (s) (plist-get s :stale)) ygg-git-compare-tour--steps)))))

(ert-deftest ygg-git-compare-tour-delivered-before-a-compare-opens-is-adopted ()
  (ygg-git-compare-tour-tests--with-compare root
    (let ((file (ygg-git-compare-tour-tests--file root)))
      (setq ygg-git-compare--store nil)
      (ygg-git-compare-tour-receive root "feature" ygg-git-compare-tour-tests--steps "tester")
      (should (file-exists-p file))
      (should-not (plist-get (ygg-git-compare-tour--read file) :base))
      (setq ygg-git-compare-tour--steps nil ygg-git-compare-tour--loaded nil)
      (ygg-git-compare-tour--ensure)
      (should (= 2 (length ygg-git-compare-tour--steps)))
      (should (plist-get (ygg-git-compare-tour--read file) :base)))))

(ert-deftest ygg-git-compare-tour-head-move-marks-unmatched-steps-stale ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (ygg-git-compare-tour-tests--git root "checkout" "-q" "feature")
    (with-temp-file (expand-file-name "a.txt" root)
      (insert (ygg-git-compare-tour-tests--text
               '(5 . "five") '(20 . "twenty again") '(35 . "thirty-five"))))
    (ygg-git-compare-tour-tests--git root "commit" "-q" "-am" "move head")
    (ygg-git-compare-tour-tests--git root "checkout" "-q" "main")
    (let ((old (ygg-git-compare-tour--key)))
      (ygg-git-compare-refresh)
      (should-not (equal old (ygg-git-compare-tour--key)))
      (should (equal (ygg-git-compare-tour--key) ygg-git-compare-tour--loaded)))
    (pcase-let ((`(,core ,other) ygg-git-compare-tour--steps))
      (should (plist-get core :stale))
      (should-not (plist-get other :stale)))
    (let ((left (car (last (ygg-git-compare-tour-steps)))))
      (should (plist-get left :leftover))
      (should (equal (list (ygg-git-compare-tour-tests--hunk "five")
                           (ygg-git-compare-tour-tests--hunk "twenty again")
                           (ygg-git-compare-tour-tests--hunk "thirty-five"))
                     (ygg-git-compare-tour--sections left))))
    (should (equal (plist-get (ygg-git-compare-tour--read (ygg-git-compare-tour-tests--file root))
                              :head)
                   (ygg-git-compare-tour-tests--rev root "feature")))))

(ert-deftest ygg-git-compare-tour-key-asks-the-agent-when-there-is-no-tour ()
  (ygg-git-compare-tour-tests--with-compare root
    (let (asked)
      (cl-letf (((symbol-function 'ygg-git-compare-explain--ask)
                 (lambda (verb instructions &rest _) (setq asked (list verb instructions)) nil)))
        (ygg-git-compare-tour)
        (should (equal "tour" (car asked)))
        (should (string-match-p "review_tour" (cadr asked)))
        (should (string-match-p "branch \"feature\"" (cadr asked)))
        (should (string-match-p "do not propose comments" (cadr asked)))
        (ygg-git-compare-tour-tests--deliver root)
        (setq asked nil)
        (ygg-git-compare-tour)
        (should-not asked)
        (should (= 1 ygg-git-compare-tour--index))
        (ygg-git-compare-tour)
        (should-not asked)
        (should (= 1 ygg-git-compare-tour--index))))))

(ert-deftest ygg-git-compare-tour-tool-refuses-branches-git-would-refuse ()
  (dolist (branch '("../../escape" "a/../b" "/abs" "a//b" "x.lock" "a.lock/b" "a/.b" ".hidden"
                    "a." "a/" "-x" "a b" "a\nb" "a~1" "a:b" "a?b" "a*b" "a[b" "a\\b" "a@{1}" "@" "HEAD"))
    (pcase-let ((`(,answer ,relayed ,received)
                 (ygg-git-compare-tour-tests--call
                  `(:dir "/repo" :branch ,branch
                    :steps ((:title "t" :check "c" :hunks ((:file "a" :start 1))))))))
      (should-not relayed)
      (should-not received)
      (should (string-match-p "not a branch name" answer))))
  (dolist (branch '("feature" "feat/x-1" "origin/feat.v2" "user@host"))
    (should (ygg-git-compare-tour-tests--call
             `(:dir "/repo" :branch ,branch
               :steps ((:title "t" :check "c" :hunks ((:file "a" :start 1))))))))
  (should (nth 1 (ygg-git-compare-tour-tests--call
                  '(:dir "/repo" :branch "feat/x"
                    :steps ((:title "t" :check "c" :hunks ((:file "a" :start 1)))))))))

(ert-deftest ygg-git-compare-tour-path-stays-inside-the-reviews-directory ()
  (ygg-git-compare-tour-tests--with-compare root
    (let ((reviews (expand-file-name ".git/reviews/" root)))
      (should (string-prefix-p reviews (ygg-git-compare-tour--path "feature" root)))
      (should (string-prefix-p reviews (ygg-git-compare-tour--path "feat/x" root)))
      (dolist (branch '("../../escape" "a/../../b" "../x" "a/../../b"))
        (should-error (ygg-git-compare-tour--path branch root) :type 'user-error)
        (should-error (ygg-git-compare-tour-receive root branch ygg-git-compare-tour-tests--steps "t")
                      :type 'user-error))
      (should-not (file-exists-p (expand-file-name "escape.tour.json" root))))))

(ert-deftest ygg-git-compare-tour-for-another-base-is-walked-and-kept ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (let* ((file (ygg-git-compare-tour-tests--file root))
           (kept (ygg-git-compare-tour--read file)))
      (ygg-git-compare-tour--write file "0123456789abcdef" (plist-get kept :head)
                                   (plist-get kept :steps))
      (setq ygg-git-compare-tour--steps nil ygg-git-compare-tour--loaded nil)
      (let (asked)
        (cl-letf (((symbol-function 'ygg-git-compare-explain--ask)
                   (lambda (&rest _) (setq asked t) nil)))
          (ygg-git-compare-tour))
        (should-not asked))
      (should (= 2 (length ygg-git-compare-tour--steps)))
      (should (= 1 ygg-git-compare-tour--index))
      (ygg-git-compare-tour--reanchor)
      (should (equal "0123456789abcdef" (plist-get (ygg-git-compare-tour--read file) :base)))
      (ygg-git-compare-tour-tests--deliver root)
      (should (equal (ygg-git-compare-tour-tests--rev root "main")
                     (plist-get (ygg-git-compare-tour--read file) :base))))))

(defun ygg-git-compare-tour-tests--other-branch (root)
  (ygg-git-compare-tour-tests--git root "branch" "other" "main")
  (ygg-git-compare-tour-tests--git root "checkout" "-q" "other")
  (with-temp-file (expand-file-name "c.txt" root) (insert "c\n"))
  (ygg-git-compare-tour-tests--git root "add" ".")
  (ygg-git-compare-tour-tests--git root "commit" "-q" "-m" "other")
  (ygg-git-compare-tour-tests--git root "checkout" "-q" "main"))

(defun ygg-git-compare-tour-tests--request ()
  (cl-letf (((symbol-function 'ygg-git-compare-explain--ask) #'ignore))
    (ygg-git-compare-tour)))

(ert-deftest ygg-git-compare-tour-receive-installs-into-the-compare-that-asked ()
  (ygg-git-compare-tour-tests--with-compare root
    (let ((first (current-buffer))
          (default-directory root))
      (ygg-git-compare-tour-tests--other-branch root)
      (with-current-buffer (ygg-git-compare-buffer root '(rev . "other") '(rev . "feature"))
        (let ((second (current-buffer)))
          (dolist (case (list (cons first t) (cons second t) (cons first nil) (cons second nil)))
          (let ((asker (car case)))
            (dolist (b (list first second))
              (with-current-buffer b (setq ygg-git-compare-tour--steps nil
                                           ygg-git-compare-tour--loaded nil)))
            (ignore-errors (delete-file (ygg-git-compare-tour-tests--file root)))
            (bury-buffer (if (cdr case) asker (if (eq asker first) second first)))
            (with-current-buffer asker (ygg-git-compare-tour-tests--request))
            (ygg-git-compare-tour-tests--deliver root)
            (should (buffer-local-value 'ygg-git-compare-tour--steps asker))
            (should-not (buffer-local-value 'ygg-git-compare-tour--steps
                                            (if (eq asker first) second first)))
            (should (equal (ygg-git-compare-tour-tests--rev
                            root (if (eq asker first) "main" "other"))
                           (plist-get (ygg-git-compare-tour--read
                                       (ygg-git-compare-tour-tests--file root))
                                      :base))))))))))

(ert-deftest ygg-git-compare-tour-receive-keys-the-file-to-the-asked-base-when-none-is-open ()
  (ygg-git-compare-tour-tests--with-compare root
    (let ((default-directory root))
      (ygg-git-compare-tour-tests--request)
      (setq ygg-git-compare--store nil)
      (ygg-git-compare-tour-tests--deliver root)
      (should (equal (ygg-git-compare-tour-tests--rev root "main")
                     (plist-get (ygg-git-compare-tour--read
                                 (ygg-git-compare-tour-tests--file root))
                                :base))))))

(ert-deftest ygg-git-compare-tour-a-foreign-compare-never-rewrites-the-tour ()
  (ygg-git-compare-tour-tests--with-compare root
    (let ((main (current-buffer))
          (file (ygg-git-compare-tour-tests--file root))
          (default-directory root))
      (ygg-git-compare-tour-tests--deliver root)
      (ygg-git-compare-tour-tests--other-branch root)
      (let ((before (with-temp-buffer (insert-file-contents-literally file) (buffer-string))))
        (with-current-buffer (ygg-git-compare-buffer root '(rev . "other") '(rev . "feature"))
          (ygg-git-compare-tour--ensure)
          (should ygg-git-compare-tour--steps)
          (ygg-git-compare-tour--reanchor)
          (ygg-git-compare--redraw))
        (should (equal before (with-temp-buffer (insert-file-contents-literally file)
                                                (buffer-string))))
        (with-current-buffer main
          (setq ygg-git-compare-tour--steps nil ygg-git-compare-tour--loaded nil)
          (ygg-git-compare-tour--ensure)
          (should (= 2 (length ygg-git-compare-tour--steps))))))))

(ert-deftest ygg-git-compare-tour-leaving-restores-the-folds-before-it ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (magit-section-show (car (ygg-git-compare-tour--files)))
    (dolist (text '("twenty" "thirty-five"))
      (magit-section-show (ygg-git-compare-tour-tests--hunk text)))
    (magit-section-hide (ygg-git-compare-tour-tests--hunk "five"))
    (ygg-git-compare-tour-goto 2)
    (should-not (ygg-git-compare-tour-tests--open "twenty"))
    (ygg-git-compare-tour-goto 3)
    (ygg-git-compare-tour-leave)
    (should (ygg-git-compare-tour-tests--open "twenty"))
    (should (ygg-git-compare-tour-tests--open "thirty-five"))
    (should-not (ygg-git-compare-tour-tests--open "five"))
    (should-not ygg-git-compare-tour--folds)
    (ygg-git-compare-tour-goto 1)
    (ygg-git-compare-tour-leave)
    (should (ygg-git-compare-tour-tests--open "twenty"))))

(ert-deftest ygg-git-compare-tour-a-step-shows-its-reviewed-hunks-under-the-filter ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (ygg-git-compare-tour-next)
    (ygg-git-compare-tour-next)
    (ygg-git-compare-toggle-unreviewed)
    (should ygg-git-compare-marks--unreviewed-only)
    (ygg-git-compare-tour-goto 1)
    (dolist (text '("five" "twenty"))
      (let ((hunk (ygg-git-compare-tour-tests--hunk text)))
        (should (ygg-git-compare-tour-tests--open text))
        (should-not (seq-some (lambda (o) (overlay-get o 'invisible))
                              (overlays-at (oref hunk start))))))
    (ygg-git-compare-tour-leave)
    (should (seq-some (lambda (o) (overlay-get o 'invisible))
                      (overlays-at (oref (ygg-git-compare-tour-tests--hunk "five") start))))))

(ert-deftest ygg-git-compare-tour-hunk-in-two-steps-shows-reviewed-the-second-time ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver
     root '((:title "one" :check "c" :hunks ((:file "a.txt" :start 5 :end 5 :side "new")))
            (:title "two" :check "c" :hunks ((:file "a.txt" :start 5 :end 5 :side "new")
                                             (:file "a.txt" :start 20 :end 20 :side "new")))))
    (ygg-git-compare-tour-next)
    (ygg-git-compare-tour-next)
    (let ((five (ygg-git-compare-tour-tests--hunk "five")))
      (should (= 2 ygg-git-compare-tour--index))
      (should (ygg-git-compare-tour-tests--open "five"))
      (should (seq-some (lambda (o) (overlay-get o 'before-string))
                        (overlays-at (oref five start))))
      (should-not (gethash (ygg-git-compare-marks--key
                            (ygg-git-compare-tour-tests--hunk "twenty"))
                           (ygg-git-compare-marks--read)))
      (ygg-git-compare-tour-next)
      (should (gethash (ygg-git-compare-marks--key five) (ygg-git-compare-marks--read)))
      (should (gethash (ygg-git-compare-marks--key (ygg-git-compare-tour-tests--hunk "twenty"))
                       (ygg-git-compare-marks--read))))))

(ert-deftest ygg-git-compare-tour-builds-the-hunk-index-once-per-drawn-diff ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (let ((built 0)
          (orig (symbol-function 'ygg-git-compare-marks--key)))
      (cl-letf (((symbol-function 'ygg-git-compare-marks--key)
                 (lambda (h) (cl-incf built) (funcall orig h))))
        (setq ygg-git-compare-tour--cache nil)
        (ygg-git-compare-tour--keyed)
        (let ((once built))
          (ygg-git-compare-tour--keyed)
          (ygg-git-compare-tour-steps)
          (ygg-git-compare-tour--sections (car ygg-git-compare-tour--steps))
          (should (= once built)))
        (ygg-git-compare-refresh)
        (let ((before built))
          (ygg-git-compare-tour--keyed)
          (should (= before built)))))))

(ert-deftest ygg-git-compare-tour-non-ascii-text-round-trips-without-a-prompt ()
  (let* ((dir (make-temp-file "ygg-git-compare-tour-" t))
         (file (expand-file-name "t/feature.tour.json" dir))
         (steps (list (list :title "core — api" :check "agree → ship" :risk "high"
                            :hunks (list (list :file "a.txt" :start 1 :end 2 :side "new" :key "k"))))))
    (unwind-protect
        (progn
          (ygg-git-compare-tour--write file "base" "head" steps)
          (let ((kept (ygg-git-compare-tour--read file)))
            (should (equal (plist-get (car (plist-get kept :steps)) :title) "core — api"))
            (should (equal (plist-get (car (plist-get kept :steps)) :check) "agree → ship"))))
      (delete-directory dir t))))

(defmacro ygg-git-compare-tour-tests--spawning (spawned trace &rest body)
  "BODY with sessions made in SPAWNED and their trace buffer TRACE, no agent started."
  (declare (indent 2))
  `(let ((,trace (generate-new-buffer " *compare-tour-trace*"))
         (aob--sessions (make-hash-table :test #'equal))
         (aob--order nil))
     (unwind-protect
         (cl-letf (((symbol-function 'ygg-git-compare-explain--preset) (lambda () "claude"))
                   ((symbol-function 'aob-acp-spawn)
                    (lambda (_agent &optional _intent _atts name _tree)
                      (let ((s (aob-create-session :id (concat "acp:" name) :backend 'acp
                                                   :name name :project default-directory
                                                   :state 'starting)))
                        (push s ,spawned)
                        s)))
                   ((symbol-function 'aob-trace-buffer) (lambda (_s) ,trace)))
           (switch-to-buffer (current-buffer))
           (delete-other-windows)
           ,@body)
       (when (buffer-live-p ,trace) (kill-buffer ,trace)))))

(ert-deftest ygg-git-compare-tour-stale-tour-walks-with-the-stale-marked ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (ygg-git-compare-tour-tests--git root "checkout" "-q" "feature")
    (with-temp-file (expand-file-name "a.txt" root)
      (insert (ygg-git-compare-tour-tests--text
               '(5 . "five") '(20 . "twenty again") '(35 . "thirty-five"))))
    (ygg-git-compare-tour-tests--git root "commit" "-q" "-am" "move head")
    (ygg-git-compare-tour-tests--git root "checkout" "-q" "main")
    (ygg-git-compare-refresh)
    (let (asked)
      (cl-letf (((symbol-function 'ygg-git-compare-explain--ask)
                 (lambda (&rest _) (setq asked t) nil)))
        (ygg-git-compare-tour))
      (should-not asked))
    (should (= 1 ygg-git-compare-tour--index))
    (should (string-match-p "step 1/3 .*stale: 2 hunks moved to the last step"
                            ygg-git-compare--tour-status))
    (should (string-match-p "stale: 2 hunks" (format "%s" header-line-format)))
    (ygg-git-compare-tour-next)
    (should-not (string-match-p "stale" ygg-git-compare--tour-status))
    (ygg-git-compare-tour-next)
    (should (equal 3 ygg-git-compare-tour--index))))

(ert-deftest ygg-git-compare-tour-refresh-asks-again-and-replaces-the-kept-tour ()
  (ygg-git-compare-tour-tests--with-compare root
    (ygg-git-compare-tour-tests--deliver root)
    (let (asked (aob--sessions (make-hash-table :test #'equal)) (aob--order nil))
      (cl-letf (((symbol-function 'ygg-git-compare-explain--ask)
                 (lambda (verb &rest _)
                   (push verb asked)
                   (aob-create-session :id "acp:tour" :backend 'acp :name "tour"
                                       :project default-directory :state 'working))))
        (ygg-git-compare-tour-refresh)
        (should (ygg-git-compare-tour--generating-p))
        (should-error (ygg-git-compare-tour-refresh) :type 'user-error)
        (should (equal '("tour") asked)))
      (should (string-match-p "tour: generating…" (format "%s" header-line-format))))
    (let ((message-log-max nil) said)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (ygg-git-compare-tour-tests--deliver
         root '((:title "only" :check "c" :risk nil
                 :hunks ((:file "b.txt" :start 2 :end 2 :side "new"))))))
      (should (equal "tour ready — t to walk" said)))
    (should-not (ygg-git-compare-tour--generating-p))
    (should-not (string-match-p "generating" (format "%s" header-line-format)))
    (should (string-match-p "tour ready — t to walk" (format "%s" header-line-format)))
    (ygg-git-compare-tour)
    (should-not (string-match-p "tour ready" (format "%s" header-line-format)))
    (should (equal '("only")
                   (mapcar (lambda (s) (plist-get s :title))
                           (plist-get (ygg-git-compare-tour--read
                                       (ygg-git-compare-tour-tests--file root))
                                      :steps))))
    (should (= 2 (length (ygg-git-compare-tour-steps))))))

(ert-deftest ygg-git-compare-tour-asking-keeps-the-trace-closed-until-asked-for ()
  (ygg-git-compare-tour-tests--with-compare root
    (let ((compare (current-buffer)) spawned)
      (ygg-git-compare-tour-tests--spawning spawned trace
        (let ((window (selected-window)))
          (should-error (ygg-git-compare-tour-trace) :type 'user-error)
          (ygg-git-compare-tour)
          (should (= 1 (length spawned)))
          (should-not (get-buffer-window trace))
          (should (eq (window-buffer window) compare))
          (should (ygg-git-compare-tour--generating-p))
          (ygg-git-compare-tour)
          (should (= 1 (length spawned)))
          (ygg-git-compare-tour-trace)
          (should (eq (window-buffer (get-buffer-window trace)) trace))
          (should (window-live-p window))
          (should (eq (window-buffer window) compare))
          (should (= 2 (length (window-list))))
          (with-selected-window (get-buffer-window trace) (quit-window))
          (should-not (get-buffer-window trace))
          (should (= 1 (length (window-list))))
          (should (eq (window-buffer window) compare))
          (should-error (ygg-git-compare-tour-refresh) :type 'user-error)
          (should (= 1 (length spawned)))
          (setf (aob-session-state (car spawned)) 'dead)
          (ygg-git-compare-tour-refresh)
          (should (= 2 (length spawned)))
          (should-not (get-buffer-window trace)))))))

(ert-deftest ygg-git-compare-tour-idle-agent-without-a-tour-is-not-generating ()
  (ygg-git-compare-tour-tests--with-compare root
    (let (spawned)
      (ygg-git-compare-tour-tests--spawning spawned trace
        (ygg-git-compare-tour)
        (should (ygg-git-compare-tour--generating-p))
        (setf (aob-session-state (car spawned)) 'idle)
        (ygg-git-compare--header)
        (should-not (ygg-git-compare-tour--generating-p))
        (should (string-match-p "tour: agent finished without a tour — W asks again, w shows it"
                                (format "%s" header-line-format)))
        (ygg-git-compare-tour-refresh)
        (should (= 2 (length spawned)))
        (should (string-match-p "tour: generating…" (format "%s" header-line-format)))))))

(ert-deftest ygg-git-compare-tour-ready-survives-reopening-until-walked ()
  (ygg-git-compare-tour-tests--with-compare root
    (let ((default-directory root)
          (shown (lambda () (format "%s" header-line-format))))
      (ygg-git-compare-tour-tests--request)
      (ygg-git-compare-tour-tests--deliver root)
      (kill-buffer)
      (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "feature"))
        (should (string-match-p "tour ready — t to walk" (funcall shown)))
        (ygg-git-compare-tour)
        (should-not (string-match-p "tour ready" (funcall shown)))
        (kill-buffer))
      (with-current-buffer (ygg-git-compare-buffer root '(rev . "main") '(rev . "feature"))
        (should-not (string-match-p "tour ready" (funcall shown)))
        (ygg-git-compare-tour-tests--request)
        (ygg-git-compare-tour-tests--deliver root)
        (should (string-match-p "tour ready — t to walk" (funcall shown)))))))

(provide 'ygg-git-compare-tour-tests)
;;; ygg-git-compare-tour-tests.el ends here
