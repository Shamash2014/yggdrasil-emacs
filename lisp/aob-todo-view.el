;;; aob-todo-view.el --- a session's todo list, kept by you and its agent -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'aob)
(require 'ygg-todo nil t)

(declare-function ygg-qf-define-kind "layer-quickfix")
(declare-function ygg-qf-show-kind "layer-quickfix")
(declare-function ygg-qf-kind-refresh "layer-quickfix")
(declare-function ygg-qf-kind-target-id "layer-quickfix")
(declare-function aob-trace-comment-on "aob-trace")
(defvar ygg-qf--kind)
(defvar aob--views)
(defvar ygg-todo-by)

(defun aob-todo--glyph (done)
  "Return glyph for a todo item: [ ] if not done, [x] if done."
  (if done "[x]" "[ ]"))

(defun aob-todo--file (s)
  (and (fboundp 'ygg-todo-session-file) (ygg-todo-session-file s)))

(defun aob-todo--data (s)
  (when-let* ((file (aob-todo--file s)))
    (ygg-todo-read file)))

(defun aob-todo--now (s)
  (seq-find (lambda (e) (equal (plist-get e :status) "in_progress"))
            (plist-get (aob-session-ref s :plan-ev) :entries)))

(defun aob-todo--collect (s)
  (let ((sid (aob-session-id s))
        (items (plist-get (aob-todo--data s) :items))
        (sections (plist-get (aob-todo--data s) :sections))
        rows)
    (when-let* ((now (and items (aob-todo--now s))))
      (push (list (cons sid :now)
                  (propertize (format "◐ now: %s" (plist-get now :content)) 'face 'shadow)
                  (aob-session-name s))
            rows))
    (dolist (section (delete-dups (cons "" sections)))
      (dolist (item (seq-filter (lambda (it) (equal (or (plist-get it :section) "") section))
                                items))
        (let ((text (format "%s %s %s" (aob-todo--glyph (plist-get item :done))
                            (plist-get item :id) (plist-get item :text))))
          (push (list (cons sid (plist-get item :id))
                      (if (plist-get item :done) (propertize text 'face 'shadow) text)
                      section)
                rows))))
    (nreverse rows)))

(defun aob-todo--live-p (s)
  (eq s (aob-session-get (aob-session-id s))))

(defun aob-todo--session (id)
  (or (and id (aob-session-get (car id)))
      (and (boundp 'ygg-qf--kind) (car (plist-get ygg-qf--kind :args)))
      (aob-target)))

(defun aob-todo--item (id)
  "The (SESSION FILE ITEM) the row ID stands for."
  (let* ((s (or (and id (aob-session-get (car id))) (user-error "aob: that session is gone")))
         (file (or (aob-todo--file s) (user-error "No todo file")))
         (item (and (not (eq (cdr id) :now))
                    (seq-find (lambda (it) (equal (plist-get it :id) (cdr id)))
                              (plist-get (ygg-todo-read file) :items)))))
    (list s file (or item (user-error "Not a todo item")))))

(defun aob-todo--visit (file line)
  (find-file-other-window file)
  (goto-char (point-min))
  (when line
    (forward-line (1- line))))

(defun aob-todo--open-now (sid)
  (let* ((s (or (aob-session-get sid) (user-error "aob: that session is gone")))
         (file (or (aob-todo--file s) (user-error "No todo file")))
         (content (plist-get (aob-todo--now s) :content))
         (item (seq-find (lambda (it) (equal (plist-get it :text) content))
                         (plist-get (ygg-todo-read file) :items))))
    (aob-todo--visit file (plist-get item :line))))

(defun aob-todo-open-at-line (&optional id)
  "Open the todo file at the line of the item ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (if (and id (eq (cdr id) :now))
      (aob-todo--open-now (car id))
    (pcase-let ((`(,_ ,file ,item) (aob-todo--item id)))
      (aob-todo--visit file (plist-get item :line)))))

(defun aob-todo-add (&optional id)
  "Add a todo item, in the section of the item ID when there is one."
  (interactive (list (ygg-qf-kind-target-id)))
  (let* ((s (aob-todo--session id))
         (file (or (aob-todo--file s)
                   (and (fboundp 'ygg-todo-create)
                        (fboundp 'ygg-todo-session-dir)
                        (fboundp 'ygg-todo-session-bind)
                        (let ((path (ygg-todo-create (ygg-todo-session-dir s)
                                                     (aob-session-name s)
                                                     (aob-session-name s) nil)))
                          (ygg-todo-session-bind s path)
                          path))
                   (user-error "No todo file and cannot create one")))
         (section (and id (not (eq (cdr id) :now))
                       (plist-get (nth 2 (aob-todo--item id)) :section)))
         (text (read-string "Todo: ")))
    (let ((ygg-todo-by 'user))
      (ygg-todo-add file text section))
    (message "Added todo item")))

(defun aob-todo-edit (&optional id)
  "Edit the text of the item ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let* ((`(,_ ,file ,item) (aob-todo--item id))
               (text (plist-get item :text))
               (new-text (read-string "Edit: " text)))
    (let ((ygg-todo-by 'user))
      (ygg-todo-rewrite file (plist-get item :id) new-text text))
    (message "Updated todo item")))

(defun aob-todo-toggle-done (&optional id)
  "Toggle done on the item ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let ((`(,_ ,file ,item) (aob-todo--item id)))
    (let ((ygg-todo-by 'user))
      (ygg-todo-set-done file (plist-get item :id) (not (plist-get item :done))
                         (plist-get item :text)))
    (message "Toggled todo item")))

(defun aob-todo-remove-item (&optional id)
  "Remove the item ID, by default the one on this row, once asked."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let ((`(,_ ,file ,item) (aob-todo--item id)))
    (unless (y-or-n-p "Delete this item? ")
      (user-error "Cancelled"))
    (let ((ygg-todo-by 'user))
      (ygg-todo-remove file (plist-get item :id) (plist-get item :text)))
    (message "Removed todo item")))

(defun aob-todo-comment (&optional id)
  "Open a comment box for the item ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (pcase-let ((`(,s ,file ,item) (aob-todo--item id)))
    (unless (fboundp 'aob-trace-comment-on)
      (user-error "Comments need aob-trace"))
    (aob-trace-comment-on s (format "%s:%d %s"
                                    (file-name-nondirectory file)
                                    (or (plist-get item :line) 0)
                                    (plist-get item :text)))))

(defvar aob-todo-map
  (let ((m (make-sparse-keymap)))
    (define-key m "o" #'aob-todo-open-at-line)
    (define-key m "a" #'aob-todo-add)
    (define-key m "c" #'aob-todo-edit)
    (define-key m "x" #'aob-todo-toggle-done)
    (define-key m "d" #'aob-todo-remove-item)
    (define-key m "C" #'aob-todo-comment)
    m)
  "What embark offers on a todo row of the quickfix.")

(defun aob-todo--same-file-p (a b)
  (and a b (equal (file-truename a) (file-truename b))))

(defun aob-todo--arm (buf token s)
  (letrec ((render (lambda ()
                     (unless (ygg-qf-kind-refresh buf token)
                       (funcall disarm))))
           (on-change (lambda (file &rest _)
                        (when (aob-todo--same-file-p file (aob-todo--file s))
                          (ignore-errors (funcall render)))))
           (disarm (lambda ()
                     (remove-hook 'ygg-todo-changed-functions on-change)
                     (when (eq (alist-get buf aob--views nil nil #'eq) render)
                       (setq aob--views (assq-delete-all buf aob--views))))))
    (aob-register-view buf render)
    (add-hook 'ygg-todo-changed-functions on-change)
    disarm))

(with-eval-after-load 'layer-quickfix
  (ygg-qf-define-kind 'todo
                      :collect #'aob-todo--collect
                      :action #'aob-todo-open-at-line
                      :map 'aob-todo-map
                      :arm #'aob-todo--arm
                      :live-p #'aob-todo--live-p
                      :drop #'aob-todo-remove-item
                      :glyph "☐"))

;;;###autoload
(defun aob-todo (s)
  "Collect the todo list of session S into the quickfix; a row opens the file."
  (interactive (list (aob-target)))
  (unless (plist-get (aob-todo--data s) :items)
    (unless (y-or-n-p (format "%s has no todo items; add the first? " (aob-session-name s)))
      (user-error "aob: %s has no todo items" (aob-session-name s)))
    (aob-todo-add (cons (aob-session-id s) :now)))
  (require 'layer-quickfix)
  (let ((default-directory (or (aob-session-dir s) (aob-session-project s)
                               default-directory)))
    (ygg-qf-show-kind 'todo s)))

(provide 'aob-todo-view)
;;; aob-todo-view.el ends here
