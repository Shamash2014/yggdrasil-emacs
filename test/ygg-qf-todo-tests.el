;;; ygg-qf-todo-tests.el --- \ d lists the todo items in the quickfix -*- lexical-binding: t; -*-

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
  (setq aob-acp-persist-file (make-temp-file "aob-qftodo-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'ygg-todo)
(require 'layer-quickfix)
(require 'layer-aob)
(require 'ygg-embark)

(defmacro ygg-qf-todo-tests--with (svar fvar &rest body)
  (declare (indent 2))
  `(let* ((dir (file-name-as-directory (make-temp-file "qftodo-" t)))
          (,svar (aob-create-session :id "acp:qftodo:1" :backend 'acp
                                     :name "qftodo" :project dir
                                     :dir dir :state 'working))
          (,fvar (ygg-todo-create (ygg-todo-session-dir ,svar) "t" "t"
                                  '(("Alpha" "first" "second") ("Beta" "third")))))
     (ygg-todo-session-bind ,svar ,fvar)
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))
       (when (aob-session-get (aob-session-id ,svar))
         (aob-remove-session ,svar))
       (delete-directory dir t))))

(defun ygg-qf-todo-tests--text ()
  (with-current-buffer (ygg-qf-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ygg-qf-todo-tests--goto-row (text)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward text)
    (beginning-of-line)))

(defun ygg-qf-todo-tests--id (file text)
  (cons "acp:qftodo:1"
        (plist-get (seq-find (lambda (it) (equal (plist-get it :text) text))
                             (plist-get (ygg-todo-read file) :items))
                   :id)))

(defun ygg-qf-todo-tests--refresh ()
  (aob--render-view (assq (ygg-qf-buffer) aob--views)))

(ert-deftest ygg-qf-todo-fills-quickfix-one-row-each ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (with-current-buffer (ygg-qf-buffer)
      (should (= 3 (ygg-qf--count-rows))))
    (should (string-match-p "\\[ \\] [0-9.]+ first" (ygg-qf-todo-tests--text)))
    (should (string-match-p "\\[ \\] [0-9.]+ third" (ygg-qf-todo-tests--text)))))

(ert-deftest ygg-qf-todo-note-is-the-section ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (ygg-qf-todo-tests--goto-row "third")
    (with-current-buffer (ygg-qf-buffer)
      (should (equal "Beta" (get-text-property (point) 'ygg-qf-note))))))

(ert-deftest ygg-qf-todo-step-in-hand-leads-as-a-muted-row ()
  (ygg-qf-todo-tests--with s file
    (aob-session-put s :plan-ev (list :entries (list (list :content "Write it"
                                                           :status "in_progress"))))
    (aob-todo s)
    (with-current-buffer (ygg-qf-buffer)
      (should (= 4 (ygg-qf--count-rows))))
    (should (string-match-p "◐ now: Write it" (ygg-qf-todo-tests--text)))
    (ygg-qf-todo-tests--goto-row "now: Write it")
    (with-current-buffer (ygg-qf-buffer)
      (search-forward "now: Write it")
      (should (eq 'shadow (get-text-property (match-beginning 0) 'face))))
    (should-error (aob-todo-toggle-done (cons "acp:qftodo:1" :now)) :type 'user-error)))

(ert-deftest ygg-qf-todo-done-items-are-shadowed ()
  (ygg-qf-todo-tests--with s file
    (let ((ygg-todo-by 'user))
      (ygg-todo-set-done file (cdr (ygg-qf-todo-tests--id file "first")) t "first"))
    (aob-todo s)
    (ygg-qf-todo-tests--goto-row "[x]")
    (with-current-buffer (ygg-qf-buffer)
      (let ((pos (+ (point) (- (string-match "first" (buffer-substring (point) (line-end-position)))
                               0))))
        (should (eq 'shadow (get-text-property pos 'face)))))))

(ert-deftest ygg-qf-todo-without-items-declined-is-a-user-error ()
  (let ((s (aob-create-session :id "acp:qftodo:2" :backend 'acp :name "bare"
                               :project "/tmp/proj/" :dir "/tmp/proj/")))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (should-error (aob-todo s) :type 'user-error))
      (aob-remove-session s))))

(ert-deftest ygg-qf-todo-without-items-offers-to-add-the-first ()
  (ygg-qf-todo-tests--with s file
    (let ((ygg-todo-by 'user))
      (while-let ((it (car (plist-get (ygg-todo-read file) :items))))
        (ygg-todo-remove file (plist-get it :id) (plist-get it :text))))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'read-string) (lambda (&rest _) "fresh")))
      (aob-todo s))
    (should (string-match-p "fresh" (ygg-qf-todo-tests--text)))))

(ert-deftest ygg-qf-todo-duplicate-section-names-do-not-duplicate-rows ()
  (ygg-qf-todo-tests--with s file
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      (insert "\n## Alpha\n- [ ] 9.9 dup tail\n")
      (write-region nil nil file))
    (let ((ids (mapcar #'car (aob-todo--collect s))))
      (should (= (length ids) (length (delete-dups (copy-sequence ids))))))))

(ert-deftest ygg-qf-todo-now-row-opens-the-file-at-the-step ()
  (ygg-qf-todo-tests--with s file
    (aob-session-put s :plan-ev (list :entries (list (list :content "second"
                                                           :status "in_progress"))))
    (let (opened)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f) (push f opened) (set-buffer (find-file-noselect f)))))
        (aob-todo-open-at-line (cons "acp:qftodo:1" :now)))
      (should (equal opened (list file))))))

(ert-deftest ygg-qf-todo-row-opens-the-file-at-its-line ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (ygg-qf-todo-tests--goto-row "second")
    (let (opened)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f) (push f opened) (set-buffer (find-file-noselect f)))))
        (with-current-buffer (ygg-qf-buffer) (ygg-qf-open)))
      (should (equal opened (list file)))
      (with-current-buffer (find-file-noselect file)
        (should (= (line-number-at-pos)
                   (plist-get (seq-find (lambda (it) (equal (plist-get it :text) "second"))
                                        (plist-get (ygg-todo-read file) :items))
                              :line)))))))

(ert-deftest ygg-qf-todo-verbs-act-on-an-id-away-from-point ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (ygg-qf-todo-tests--goto-row "first")
    (let ((id (ygg-qf-todo-tests--id file "third")))
      (aob-todo-toggle-done id)
      (should (plist-get (seq-find (lambda (it) (equal (plist-get it :text) "third"))
                                   (plist-get (ygg-todo-read file) :items))
                         :done))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "third, edited")))
        (aob-todo-edit id))
      (should (member "third, edited"
                      (mapcar (lambda (it) (plist-get it :text))
                              (plist-get (ygg-todo-read file) :items))))
      (should (member "first"
                      (mapcar (lambda (it) (plist-get it :text))
                              (plist-get (ygg-todo-read file) :items)))))))

(ert-deftest ygg-qf-todo-add-works-with-no-row-and-lands-in-the-rows-section ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "fourth")))
      (with-current-buffer (ygg-qf-buffer) (aob-todo-add))
      (aob-todo-add (ygg-qf-todo-tests--id file "third")))
    (let ((items (plist-get (ygg-todo-read file) :items)))
      (should (= 2 (cl-count "fourth" items
                             :key (lambda (it) (plist-get it :text)) :test #'equal)))
      (should (equal "Beta" (plist-get (seq-find (lambda (it) (equal (plist-get it :text) "fourth"))
                                                 (reverse items))
                                       :section))))))

(ert-deftest ygg-qf-todo-remove-asks-first ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (let ((id (ygg-qf-todo-tests--id file "third")))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-error (aob-todo-remove-item id) :type 'user-error))
      (should (= 3 (length (plist-get (ygg-todo-read file) :items))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (aob-todo-remove-item id))
      (should (= 2 (length (plist-get (ygg-todo-read file) :items)))))))

(ert-deftest ygg-qf-todo-comment-quotes-the-item ()
  (ygg-qf-todo-tests--with s file
    (let (got)
      (cl-letf (((symbol-function 'aob-trace-comment-on)
                 (lambda (sess quote) (setq got (list sess quote)))))
        (aob-todo-comment (ygg-qf-todo-tests--id file "third")))
      (should (eq s (car got)))
      (should (string-match-p "tasks.md:[0-9]+ third" (cadr got))))))

(ert-deftest ygg-qf-todo-drop-removes-the-item-and-the-row ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (ygg-qf-todo-tests--goto-row "second")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (with-current-buffer (ygg-qf-buffer) (ygg-qf-drop)))
    (should-not (string-match-p "second" (ygg-qf-todo-tests--text)))
    (should (= 2 (with-current-buffer (ygg-qf-buffer) (ygg-qf--count-rows))))))

(ert-deftest ygg-qf-todo-rows-follow-the-file-and-keep-point-by-id ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (ygg-qf-todo-tests--goto-row "third")
    (let ((ygg-todo-by 'user))
      (ygg-todo-add file "newcomer" "Alpha"))
    (should (string-match-p "newcomer" (ygg-qf-todo-tests--text)))
    (with-current-buffer (ygg-qf-buffer)
      (should (string-match-p "third" (buffer-substring-no-properties
                                       (line-beginning-position) (line-end-position)))))))

(ert-deftest ygg-qf-todo-rows-follow-the-plan-tick ()
  (ygg-qf-todo-tests--with s file
    (aob-todo s)
    (aob-session-put s :plan-ev (list :entries (list (list :content "Now step"
                                                           :status "in_progress"))))
    (ygg-qf-todo-tests--refresh)
    (should (string-match-p "now: Now step" (ygg-qf-todo-tests--text)))))

(ert-deftest ygg-qf-todo-hook-is-removed-when-the-list-is-replaced ()
  (ygg-qf-todo-tests--with s file
    (let ((before (length ygg-todo-changed-functions)))
      (aob-todo s)
      (should (= (1+ before) (length ygg-todo-changed-functions)))
      (ygg-qf-clear)
      (should (= before (length ygg-todo-changed-functions))))))

(ert-deftest ygg-qf-todo-old-panel-is-gone ()
  (should-not (fboundp 'aob-todo-mode))
  (should-not (fboundp 'aob-todo-buffer))
  (should-not (fboundp 'aob-todo-refresh))
  (should-not (boundp 'aob-todo-mode-map)))

;;; ygg-qf-todo-tests.el ends here
