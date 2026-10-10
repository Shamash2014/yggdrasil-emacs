;;; aob-edits-qf-tests.el --- the edited-files quickfix of an aob session -*- lexical-binding: t; -*-

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
  (setq aob-acp-persist-file (make-temp-file "aob-edits-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'layer-quickfix)
(require 'layer-aob)
(require 'aob-edits-qf)

(defvar aob-edits-tests--dir nil)
(defvar aob-edits-tests--depth 0)

(defun aob-edits-tests--write (name text)
  (let ((f (expand-file-name name aob-edits-tests--dir)))
    (write-region text nil f nil 'silent)
    f))

(defmacro aob-edits-tests--with (svar &rest body)
  (declare (indent 1))
  `(let* ((aob-edits-tests--dir (file-name-as-directory (make-temp-file "aob-edits-repo" t)))
          (,svar (aob-create-session :id "acp:edits:1" :backend 'acp :name "edits"
                                     :project aob-edits-tests--dir
                                     :dir aob-edits-tests--dir :state 'idle)))
     (unwind-protect
         (progn
           (aob-edits-tests--write "a.el" "one\ntwo\n")
           (aob-edits-tests--write "b.el" "x\n")
           ,@body)
       (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))
       (when (aob-session-get (aob-session-id ,svar)) (aob-remove-session ,svar))
       (delete-directory aob-edits-tests--dir t))))

(defun aob-edits-tests--edit (s id path old new &optional line status)
  (aob-event s 'tool :tool-id id :kind "edit" :title (concat "Edit " path)
             :status (or status "completed")
             :locations (and line (list (list :path path :line line)))
             :content (list (list :type "diff" :path path :oldText old :newText new))))

(defun aob-edits-tests--three (s)
  (aob-edits-tests--edit s "e1" "a.el" "one\ntwo\n" "one\nTWO\n" 2)
  (aob-edits-tests--edit s "e2" "b.el" "x\n" "x\ny\n" 1)
  (aob-edits-tests--edit s "e3" "a.el" "one\nTWO\n" "one\nTWO\nthree\n" 3))

(defun aob-edits-tests--rows (s) (aob-edits--rows s))

(ert-deftest aob-edits-rows-one-per-file ()
  (aob-edits-tests--with s
    (aob-edits-tests--three s)
    (let ((rows (aob-edits-tests--rows s)))
      (should (= 2 (length rows)))
      (should (equal (cdr (car (nth 0 rows))) (expand-file-name "a.el" aob-edits-tests--dir)))
      (should (string-match-p "\\`a\\.el  \\+5 −4  ×2\\'" (nth 1 (nth 0 rows))))
      (should (string-match-p "\\`b\\.el  \\+2 −1\\'" (nth 1 (nth 1 rows))))
      (should (string-match-p "completed" (nth 2 (nth 0 rows)))))))

(ert-deftest aob-edits-missing-data-shows-dash ()
  (aob-edits-tests--with s
    (aob-event s 'tool :tool-id "e9" :kind "edit" :title "Edit"
               :locations (list (list :path "b.el" :line 1)))
    (let ((row (car (aob-edits-tests--rows s))))
      (should (string-match-p "—" (nth 1 row)))
      (should (string-match-p "\\?" (nth 2 row))))))

(ert-deftest aob-edits-action-opens-file-at-location ()
  (aob-edits-tests--with s
    (aob-edits-tests--three s)
    (let ((id (car (nth 0 (aob-edits-tests--rows s)))) buf)
      (save-window-excursion
        (aob-edits-visit id)
        (setq buf (current-buffer))
        (should (equal (file-truename buffer-file-name)
                       (file-truename (expand-file-name "a.el" aob-edits-tests--dir))))
        (should (= 2 (line-number-at-pos))))
      (kill-buffer buf))))

(ert-deftest aob-edits-compare-shows-each-edit ()
  (aob-edits-tests--with s
    (aob-edits-tests--three s)
    (save-window-excursion
      (aob-edits-compare (car (nth 0 (aob-edits-tests--rows s))))
      (should (derived-mode-p 'diff-mode))
      (should (string-match-p "^\\+TWO" (buffer-string)))
      (should (string-match-p "^\\+three" (buffer-string)))
      (kill-buffer (current-buffer)))))

(ert-deftest aob-edits-accept-marks-note ()
  (aob-edits-tests--with s
    (aob-edits-tests--three s)
    (let ((id (car (nth 0 (aob-edits-tests--rows s)))))
      (aob-edits-accept id)
      (should (string-match-p "accepted" (nth 2 (nth 0 (aob-edits-tests--rows s)))))
      (should-not (string-match-p "accepted" (nth 2 (nth 1 (aob-edits-tests--rows s)))))
      (aob-edits-tests--edit s "e4" "a.el" "one\nTWO\nthree\n" "four\n")
      (should-not (string-match-p "accepted" (nth 2 (nth 0 (aob-edits-tests--rows s))))))))

(defun aob-edits-tests--read (name)
  (aob-edits--file-text (expand-file-name name aob-edits-tests--dir)))

(defun aob-edits-tests--first-id (s) (car (car (aob-edits-tests--rows s))))

(defmacro aob-edits-tests--yes (&rest body)
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))) ,@body))

(defun aob-edits-tests--snippets (s)
  (aob-edits-tests--write "s.el" "alpha\nBETA\nGAMMA\nDELTA\n")
  (aob-edits-tests--edit s "s1" "s.el" "beta" "BETA" 2)
  (aob-edits-tests--edit s "s2" "s.el" "delta" "DELTA" 4)
  (aob-edits-tests--edit s "s3" "s.el" "BETA\ngamma" "BETA\nGAMMA" 2))

(ert-deftest aob-edits-revert-undoes-snippet-edits-in-one-undo-group ()
  (aob-edits-tests--with s
    (aob-edits-tests--snippets s)
    (let* ((file (expand-file-name "s.el" aob-edits-tests--dir))
           (buf (find-file-noselect file)))
      (unwind-protect
          (progn
            (aob-edits-tests--yes (aob-edits-revert (aob-edits-tests--first-id s)))
            (should (equal "alpha\nbeta\ngamma\ndelta\n" (aob-edits-tests--read "s.el")))
            (should-not (buffer-modified-p buf))
            (with-current-buffer buf
              (undo-boundary)
              (let ((last-command nil)) (undo))
              (should (equal "alpha\nBETA\nGAMMA\nDELTA\n" (buffer-string)))))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest aob-edits-revert-saves-without-running-the-formatter ()
  (require 'apheleia)
  (aob-edits-tests--with s
    (aob-edits-tests--snippets s)
    (let* ((file (expand-file-name "s.el" aob-edits-tests--dir))
           (buf (find-file-noselect file))
           (formatted nil)
           (apheleia-formatters '((upcase-it . ("tr" "a-z" "A-Z"))))
           (apheleia-mode-alist '((fundamental-mode . upcase-it) (emacs-lisp-mode . upcase-it))))
      (unwind-protect
          (progn
            (with-current-buffer buf (apheleia-mode 1))
            (advice-add 'apheleia-format-buffer :before
                        (lambda (&rest _) (setq formatted t)) '((name . aob-edits-test)))
            (aob-edits-tests--yes (aob-edits-revert (aob-edits-tests--first-id s)))
            (sleep-for 0.5)
            (should-not formatted)
            (should (equal "alpha\nbeta\ngamma\ndelta\n" (aob-edits-tests--read "s.el")))
            (with-current-buffer buf
              (should (equal "alpha\nbeta\ngamma\ndelta\n" (buffer-string)))
              (should (verify-visited-file-modtime buf))))
        (advice-remove 'apheleia-format-buffer 'aob-edits-test)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest aob-edits-revert-snippets-without-a-visiting-buffer ()
  (aob-edits-tests--with s
    (aob-edits-tests--snippets s)
    (aob-edits-tests--yes (aob-edits-revert (aob-edits-tests--first-id s)))
    (should (equal "alpha\nbeta\ngamma\ndelta\n" (aob-edits-tests--read "s.el")))
    (when-let* ((b (find-buffer-visiting (expand-file-name "s.el" aob-edits-tests--dir))))
      (kill-buffer b))))

(ert-deftest aob-edits-revert-refuses-when-agent-text-is-gone ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "s1" "s.el" "beta" "BETA" 2)
    (aob-edits-tests--write "s.el" "alpha\nbeta\nhuman\n")
    (let ((asked nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (setq asked t))))
        (let ((err (should-error (aob-edits-revert (aob-edits-tests--first-id s))
                                 :type 'user-error)))
          (should (string-match-p "edit 1: can't find the agent's text" (cadr err)))))
      (should-not asked)
      (should (equal "alpha\nbeta\nhuman\n" (aob-edits-tests--read "s.el"))))))

(ert-deftest aob-edits-revert-refuses-when-agent-text-is-ambiguous ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "s1" "s.el" "beta" "BETA" 2)
    (aob-edits-tests--write "s.el" "BETA\nx\nBETA\n")
    (let ((err (aob-edits-tests--yes
                (should-error (aob-edits-revert (aob-edits-tests--first-id s))
                              :type 'user-error))))
      (should (string-match-p "ambiguous — the agent's text appears 2 times" (cadr err))))
    (should (equal "BETA\nx\nBETA\n" (aob-edits-tests--read "s.el")))))

(ert-deftest aob-edits-revert-restores-whole-file-diffs ()
  (aob-edits-tests--with s
    (aob-edits-tests--three s)
    (aob-edits-tests--write "a.el" "one\nTWO\nthree\n")
    (aob-edits-tests--yes (aob-edits-revert (aob-edits-tests--first-id s)))
    (should (equal "one\ntwo\n" (aob-edits-tests--read "a.el")))
    (when-let* ((b (find-buffer-visiting (expand-file-name "a.el" aob-edits-tests--dir))))
      (kill-buffer b))))

(ert-deftest aob-edits-revert-asks-naming-the-file-and-declining-keeps-it ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "e1" "b.el" "x\n" "x\ny\n" 1)
    (aob-edits-tests--write "b.el" "x\ny\n")
    (let ((prompt nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq prompt p) nil)))
        (should-error (aob-edits-revert (aob-edits-tests--first-id s)) :type 'user-error))
      (should (string-match-p "b\\.el" prompt))
      (should (equal "x\ny\n" (aob-edits-tests--read "b.el"))))))

(ert-deftest aob-edits-revert-created-file-needs-a-separate-delete-confirm ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "w1" "n.el" nil "fresh\n" 1)
    (aob-edits-tests--write "n.el" "fresh\n")
    (let ((prompt nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq prompt p) nil)))
        (should-error (aob-edits-revert (aob-edits-tests--first-id s)) :type 'user-error))
      (should (string-match-p "\\`Delete file n\\.el the agent created" prompt))
      (should (file-exists-p (expand-file-name "n.el" aob-edits-tests--dir))))
    (aob-edits-tests--yes (aob-edits-revert (aob-edits-tests--first-id s)))
    (should-not (file-exists-p (expand-file-name "n.el" aob-edits-tests--dir)))))

(ert-deftest aob-edits-revert-created-file-refused-when-changed ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "w1" "n.el" nil "fresh\n" 1)
    (aob-edits-tests--write "n.el" "fresh\nhuman\n")
    (let ((asked nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (setq asked t))))
        (should-error (aob-edits-revert (aob-edits-tests--first-id s)) :type 'user-error))
      (should-not asked)
      (should (equal "fresh\nhuman\n" (aob-edits-tests--read "n.el"))))))

(ert-deftest aob-edits-revert-refuses-when-file-changes-during-prompt ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "e1" "b.el" "x\n" "x\ny\n" 1)
    (aob-edits-tests--write "b.el" "x\ny\n")
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_) (aob-edits-tests--write "b.el" "x\ny\nlate\n") t)))
      (should-error (aob-edits-revert (aob-edits-tests--first-id s)) :type 'user-error))
    (should (equal "x\ny\nlate\n" (aob-edits-tests--read "b.el")))))

(ert-deftest aob-edits-revert-refuses-while-session-runs ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "e1" "b.el" "x\n" "x\ny\n" 1)
    (aob-edits-tests--write "b.el" "x\ny\n")
    (aob-set-state s 'working)
    (aob-edits-tests--yes
     (should-error (aob-edits-revert (aob-edits-tests--first-id s)) :type 'user-error))
    (should (equal "x\ny\n" (aob-edits-tests--read "b.el")))))

(ert-deftest aob-edits-revert-all-asks-per-file ()
  (aob-edits-tests--with s
    (aob-edits-tests--three s)
    (aob-edits-tests--write "a.el" "one\nTWO\nthree\n")
    (aob-edits-tests--write "b.el" "x\ny\n")
    (let ((asked 0))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (cl-incf asked) t)))
        (aob-edits-revert-all (aob-edits-tests--first-id s)))
      (should (= 2 asked))
      (should (equal "one\ntwo\n" (aob-edits-tests--read "a.el")))
      (should (equal "x\n" (aob-edits-tests--read "b.el"))))
    (dolist (n '("a.el" "b.el"))
      (when-let* ((b (find-buffer-visiting (expand-file-name n aob-edits-tests--dir))))
        (kill-buffer b)))))

(ert-deftest aob-edits-revert-all-names-each-kept-file ()
  (aob-edits-tests--with s
    (aob-edits-tests--write "c.el" "c\n")
    (aob-edits-tests--write "d.el" "d\n")
    (aob-edits-tests--edit s "e1" "a.el" "one\ntwo\n" "one\nTWO\n" 2)
    (aob-edits-tests--edit s "e2" "c.el" "c\n" "c\nc2\n" 1)
    (aob-edits-tests--edit s "e3" "d.el" "d\n" "d\nd2\n" 1)
    (aob-edits-tests--write "a.el" "one\nhuman\n")
    (aob-edits-tests--write "c.el" "c\nc2\n")
    (aob-edits-tests--write "d.el" "d\nd2\n")
    (let (msg)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (and (string-match-p "d\\.el" p) t)))
                ((symbol-function 'message)
                 (lambda (f &rest a) (setq msg (apply #'format f a)))))
        (aob-edits-revert-all (aob-edits-tests--first-id s)))
      (should (string-match-p "a\\.el: edit 1: can't find" msg))
      (should (string-match-p "c\\.el: you declined" msg))
      (should (string-match-p "reverted 1" msg))
      (should (equal "d\n" (aob-edits-tests--read "d.el"))))
    (when-let* ((b (find-buffer-visiting (expand-file-name "d.el" aob-edits-tests--dir))))
      (kill-buffer b))))

(ert-deftest aob-edits-revert-all-lists-many-kept-in-a-buffer ()
  (aob-edits-tests--with s
    (dolist (n '("a.el" "b.el" "c.el" "d.el" "e.el"))
      (aob-edits-tests--write n "changed\n")
      (aob-edits-tests--edit s (concat "t" n) n "q\n" "r\n" 1))
    (save-window-excursion
      (aob-edits-revert-all (aob-edits-tests--first-id s))
      (let ((txt (with-current-buffer "*aob-edits: kept*" (buffer-string))))
        (should (string-match-p "a\\.el" txt))
        (should (string-match-p "e\\.el" txt)))
      (kill-buffer "*aob-edits: kept*"))))

(ert-deftest aob-edits-revert-refuses-outside-project ()
  (aob-edits-tests--with s
    (let* ((out (file-name-as-directory (make-temp-file "aob-edits-out" t)))
           (f (expand-file-name "o.el" out)))
      (unwind-protect
          (progn
            (write-region "o\np\n" nil f nil 'silent)
            (aob-edits-tests--edit s "e1" f "o\n" "o\np\n" 1)
            (should (string-match-p "outside project" (nth 2 (car (aob-edits-tests--rows s)))))
            (aob-edits-tests--yes
             (should-error (aob-edits-revert (aob-edits-tests--first-id s)) :type 'user-error))
            (should (equal "o\np\n" (aob-edits--file-text f))))
        (delete-directory out t)))))

(ert-deftest aob-edits-rows-truename-the-root-once ()
  (aob-edits-tests--with s
    (aob-edits-tests--three s)
    (let ((outer nil)
          (root (file-name-as-directory (expand-file-name aob-edits-tests--dir))))
      (advice-add 'file-truename :around
                  (lambda (fn f &rest r)
                    (when (= aob-edits-tests--depth 0) (push (file-name-as-directory (expand-file-name f)) outer))
                    (let ((aob-edits-tests--depth (1+ aob-edits-tests--depth)))
                      (apply fn f r)))
                  '((name . aob-edits-tests-count)))
      (unwind-protect (aob-edits-tests--rows s)
        (advice-remove 'file-truename 'aob-edits-tests-count))
      (should (= 1 (cl-count root outer :test #'equal)))
      (should (<= (length outer) 3)))))

(ert-deftest aob-edits-arm-refreshes-on-new-tool-call ()
  (aob-edits-tests--with s
    (aob-edits-tests--edit s "e1" "b.el" "x\n" "x\ny\n" 1)
    (save-window-excursion
      (aob-edits s)
      (let ((buf (ygg-qf-buffer)))
        (should (= 1 (with-current-buffer buf (ygg-qf--count-rows))))
        (aob-edits-tests--edit s "e2" "a.el" "one\ntwo\n" "one\n" 1)
        (funcall (alist-get buf aob--views nil nil #'eq))
        (should (= 2 (with-current-buffer buf (ygg-qf--count-rows))))))))

(ert-deftest aob-edits-live-p-follows-session ()
  (aob-edits-tests--with s
    (should (aob-edits--live-p s))
    (aob-remove-session s)
    (should-not (aob-edits--live-p s))))

(ert-deftest aob-edits-leader-key-bound ()
  (should (eq 'aob-edits (lookup-key ygg-leader-acp-map (kbd "e")))))

(provide 'aob-edits-qf-tests)
