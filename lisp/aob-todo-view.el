;;; aob-todo-view.el --- a session's todo list, kept by you and its agent -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'aob)
(require 'ygg-todo nil t)

(defvar aob-todo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap aob-object-map special-mode-map))
    (define-key map (kbd "o") #'aob-todo-add)
    (define-key map (kbd "a") #'aob-todo-add)
    (define-key map (kbd "c") #'aob-todo-edit)
    (define-key map (kbd "x") #'aob-todo-toggle-done)
    (define-key map (kbd "d d") #'aob-todo-remove-item)
    (define-key map (kbd "RET") #'aob-todo-open-at-line)
    (define-key map (kbd "C") #'aob-todo-comment)
    (define-key map (kbd "g r") #'aob-todo-refresh)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode aob-todo-mode special-mode "aob-todo"
  "Session's todo list, editable by you and the agent."
  (setq truncate-lines nil)
  (visual-line-mode 1))

(defvar-local aob-todo--session-id nil)
(defvar-local aob-todo--file nil)
(defvar-local aob-todo--tick 0)

(defun aob-todo--glyph (done)
  "Return glyph for a todo item: [ ] if not done, [x] if done."
  (if done "[x]" "[ ]"))

(defun aob-todo--render (&optional force)
  "Render the current session's todo list, under the step its agent is on."
  (when-let* ((s (aob-session-get aob-todo--session-id)))
    (let* ((file (and (fboundp 'ygg-todo-session-file)
                      (ygg-todo-session-file s)))
           (tick (concat (or file "") (number-to-string (or (aob-session-ref s :plan-tick) 0)))))
      (unless (and (not force) (equal aob-todo--tick tick))
        (setq aob-todo--tick tick)
        (let ((inhibit-read-only t)
              (todo-data (and file (fboundp 'ygg-todo-read) (ygg-todo-read file))))
          (setq aob-todo--file file)
          (setq header-line-format
                (if todo-data
                    (let* ((items (plist-get todo-data :items))
                           (done (seq-filter (lambda (it) (plist-get it :done)) items))
                           (total (length items)))
                      (format " %s · todo %d/%d · %s"
                              (aob-session-name s)
                              (length done)
                              total
                              (file-name-nondirectory file)))
                  (format " %s · no list yet (o adds the first item)" (aob-session-name s))))
          (let ((keep (plist-get (get-text-property (line-beginning-position) 'aob-todo-item) :id)))
          (aob--redraw-keeping-lines
           (lambda ()
             (erase-buffer)
             (when-let* ((now (seq-find (lambda (e) (equal (plist-get e :status) "in_progress"))
                                        (plist-get (aob-session-ref s :plan-ev) :entries))))
               (insert (propertize (format "◐ now: %s\n" (plist-get now :content))
                                   'font-lock-face 'shadow)))
             (when todo-data
               (let* ((items (plist-get todo-data :items))
                      (sections (plist-get todo-data :sections))
                      (items-by-section (make-hash-table :test 'equal)))
                 (dolist (item items)
                   (let* ((section (plist-get item :section))
                          (key (or section "")))
                     (puthash key (append (gethash key items-by-section) (list item)) items-by-section)))
                 (let ((sections-ordered (cons "" (or sections nil))))
                   (dolist (section sections-ordered)
                     (when-let* ((section-items (gethash section items-by-section)))
                       (when (not (equal section ""))
                         (insert (propertize (format "%s\n" section) 'font-lock-face 'bold)))
                       (dolist (item section-items)
                         (let ((id (plist-get item :id))
                               (text (plist-get item :text))
                               (done (plist-get item :done)))
                           (let ((item-line (format "  %s %s %s\n"
                                                    (aob-todo--glyph done)
                                                    id
                                                    text)))
                             (if done
                                 (insert (propertize item-line 'font-lock-face 'shadow))
                               (insert item-line))
                             (let* ((line-start (line-beginning-position 0))
                                    (line-end (line-end-position 0)))
                               (put-text-property line-start line-end 'aob-todo-item item)
                               (put-text-property line-start line-end 'aob-todo-file file))))))))))))
          (when-let* ((keep)
                      (item (seq-find (lambda (it) (equal (plist-get it :id) keep))
                                      (and todo-data (plist-get todo-data :items))))
                      (pos (text-property-any (point-min) (point-max) 'aob-todo-item item)))
            (goto-char pos))
          (set-buffer-modified-p nil)))))))

(defun aob-todo-buffer (s)
  "Get or create the todo buffer for session S."
  (let ((buf (get-buffer-create (aob--buffer-name "todo" s))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aob-todo-mode)
        (aob-todo-mode))
      (setq aob-todo--session-id (aob-session-id s)
            aob-buffer-session-id (aob-session-id s))
      (aob-register-view buf #'aob-todo--render)
      (aob-todo--render t))
    buf))

;;;###autoload
(defun aob-todo (s)
  "Open the todo list view for session S."
  (interactive (list (aob-target)))
  (pop-to-buffer (aob-todo-buffer s)))

(defun aob-todo-refresh ()
  "Refresh the todo view."
  (interactive)
  (aob-todo--render t))

(defun aob-todo--get-item-at-point ()
  "Get the todo item plist at point, or signal an error."
  (let ((item (get-text-property (line-beginning-position) 'aob-todo-item)))
    (unless item
      (user-error "Not a todo item"))
    item))

(defun aob-todo-add ()
  "Add a new todo item."
  (interactive)
  (let* ((s (aob-session-get aob-todo--session-id))
         (file (or aob-todo--file
                   (and (fboundp 'ygg-todo-create)
                        (fboundp 'ygg-todo-session-dir)
                        (fboundp 'ygg-todo-session-bind)
                        (let ((path (ygg-todo-create (ygg-todo-session-dir s)
                                                     (aob-session-name s)
                                                     (aob-session-name s) nil)))
                          (ygg-todo-session-bind s path)
                          path)))))
    (unless file
      (user-error "No todo file and cannot create one"))
    (when (fboundp 'ygg-todo-add)
      (let* ((text (read-string "Todo: "))
             (item-at-point (get-text-property (line-beginning-position) 'aob-todo-item))
             (section (and item-at-point (plist-get item-at-point :section))))
        (let ((ygg-todo-by 'user))
          (ygg-todo-add file text section))
        (message "Added todo item")))
    (aob-todo--render t)))

(defun aob-todo-edit ()
  "Edit the todo item at point."
  (interactive)
  (let* ((item (aob-todo--get-item-at-point))
         (file aob-todo--file)
         (id (plist-get item :id))
         (text (plist-get item :text))
         (new-text (read-string "Edit: " text)))
    (unless file
      (user-error "No todo file"))
    (when (fboundp 'ygg-todo-rewrite)
      (let ((ygg-todo-by 'user))
        (ygg-todo-rewrite file id new-text text))
      (message "Updated todo item"))
    (aob-todo--render t)))

(defun aob-todo-toggle-done ()
  "Toggle done status of the item at point."
  (interactive)
  (let* ((item (aob-todo--get-item-at-point))
         (file aob-todo--file)
         (id (plist-get item :id))
         (done (plist-get item :done))
         (text (plist-get item :text)))
    (unless file
      (user-error "No todo file"))
    (when (fboundp 'ygg-todo-set-done)
      (let ((ygg-todo-by 'user))
        (ygg-todo-set-done file id (not done) text))
      (message "Toggled todo item"))
    (aob-todo--render t)))

(defun aob-todo-remove-item ()
  "Remove the item at point."
  (interactive)
  (let* ((item (aob-todo--get-item-at-point))
         (file aob-todo--file)
         (id (plist-get item :id))
         (text (plist-get item :text)))
    (unless file
      (user-error "No todo file"))
    (unless (y-or-n-p "Delete this item? ")
      (user-error "Cancelled"))
    (when (fboundp 'ygg-todo-remove)
      (let ((ygg-todo-by 'user))
        (ygg-todo-remove file id text))
      (message "Removed todo item"))
    (aob-todo--render t)))

(defun aob-todo-open-at-line ()
  "Open the todo file at the current item's line."
  (interactive)
  (let* ((item (aob-todo--get-item-at-point))
         (file aob-todo--file)
         (line (plist-get item :line)))
    (unless file
      (user-error "No todo file"))
    (find-file-other-window file)
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun aob-todo-comment ()
  "Open comment box for the item at point."
  (interactive)
  (let* ((item (aob-todo--get-item-at-point))
         (file aob-todo--file)
         (line (plist-get item :line))
         (text (plist-get item :text))
         (s (aob-session-get aob-todo--session-id)))
    (unless (fboundp 'aob-trace-comment-on)
      (user-error "Comments need aob-trace"))
    (aob-trace-comment-on s (format "%s:%d %s"
                                     (file-name-nondirectory file)
                                     (or line 0)
                                     text))))

(defun aob-todo--on-file-changed (file _what _item)
  "Re-render all live todo buffers for FILE."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'aob-todo-mode)
                 (equal aob-todo--file file))
        (aob-todo--render t)))))

(add-hook 'ygg-todo-changed-functions #'aob-todo--on-file-changed)

(provide 'aob-todo-view)
;;; aob-todo-view.el ends here
