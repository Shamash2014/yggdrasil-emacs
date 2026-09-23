;;; aob-mcp-tools.el --- the questions the sidecar puts to the editing Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; The sidecar is emacs -Q: no buffers, no project, no major modes, no
;; checkers.  Nothing here can answer for itself, so a tool is not code
;; that runs but code that is written: each handler builds one form and
;; hands it to `aob-mcp-relay'.  The editing Emacs is the only place the
;; form makes sense, and it is also the only place that has never loaded
;; this file, so no form may call anything defined in it.
;;
;; A relayed form runs on the thread that draws the user's screen.  It is
;; a fast read or it is a frozen editor: buffers already open first, one
;; file at most, no search that is not bounded, and nothing that can put
;; a question of its own to the user.
;;
;; A form answers with a list of plain strings.  emacsclient prints back
;; what it is given, and a list prints one string to a line where a
;; single string of those same lines comes back with its newlines
;; escaped.  Properties are stripped for the same reason: a propertized
;; summary prints its faces alongside its text.

;;; Code:

(require 'aob-mcp)

(defconst aob-mcp-tools-limit 200
  "How many lines an answer may run to before it is cut short.")

(defun aob-mcp-tools--int (value)
  "VALUE as an integer, however the caller spelled it."
  (cond ((integerp value) value)
        ((floatp value) (truncate value))
        ((and (stringp value) (string-match-p "\\`-?[0-9]+\\'" value))
         (string-to-number value))))

(defun aob-mcp-tools--capped (items formatter &optional empty)
  "A form rendering the list ITEMS answers with through FORMATTER.
At most `aob-mcp-tools-limit' of them, counted and cut before they are
formatted so a thousand hits cost a thousand formats of nobody's time.
EMPTY is what to say when there are none."
  `(let* ((all ,items)
          (n (length all))
          (lines (mapcar ,formatter (take ,aob-mcp-tools-limit all))))
     (cond ((null lines) ,(or empty "nothing found"))
           ((> n ,aob-mcp-tools-limit)
            (append lines
                    (list (format "(%d more, not shown)"
                                  (- n ,aob-mcp-tools-limit)))))
           (t lines))))

(defun aob-mcp-tools--in-file (file body &optional only-if-open)
  "A form evaluating BODY in a buffer on FILE and answering with strings.
A buffer the user already had is used and left alone; anything else is
opened quietly and killed again.  With ONLY-IF-OPEN a file nobody has
open is refused rather than read."
  `(condition-case err
       (let* ((path (expand-file-name ,file))
              (existing (find-buffer-visiting path)))
         (cond
          ((not (file-readable-p path)) (format "no such file: %s" path))
          ((and (null existing) ,(and only-if-open t))
           (format "%s is not open in Emacs" path))
          (t
           (let* ((enable-local-variables :safe)
                  (large-file-warning-threshold nil)
                  (find-file-hook nil)
                  (inhibit-message t)
                  (buf (or existing (find-file-noselect path t))))
             (unwind-protect
                 (with-current-buffer buf ,body)
               (unless existing
                 (with-current-buffer buf (set-buffer-modified-p nil))
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buf))))))))
     (error (format "that question failed: %s" (error-message-string err)))))

;;; xref

(defun aob-mcp-tools--xref-form (call)
  "A form rendering the xref items CALL answers with as file:line:text."
  (aob-mcp-tools--capped
   call
   '(lambda (hit)
      (let ((loc (xref-item-location hit)))
        (format "%s:%s:%s"
                (xref-location-group loc)
                (or (xref-location-line loc) "?")
                (string-trim
                 (substring-no-properties (xref-item-summary hit))))))))

(defun aob-mcp-tools--xref (file call)
  "A form asking the xref backend of FILE the question CALL."
  (aob-mcp-tools--in-file
   file
   `(progn
      (require 'xref)
      (require 'apropos)
      (require 'find-func)
      (require 'project nil t)
      ;; a prompt would hold the editing Emacs until somebody answered it
      (let* ((project-prompter (lambda () default-directory))
             (backend (xref-find-backend)))
        (if (null backend)
            (format "no xref backend for this buffer (%s)" major-mode)
          ,(aob-mcp-tools--xref-form call))))))

(aob-mcp-deftool
 :name "xref_references"
 :description "References to a symbol, as file:line:text, at most 200 of them.
The major mode of FILE picks the xref backend, so name the file the symbol
belongs to.  Costs a project-wide search in the user's Emacs and can take
seconds; it opens FILE if no buffer has it already."
 :args '((:name "file" :type string
          :description "Absolute path of a file in the project, whose major mode picks the backend.")
         (:name "symbol" :type string
          :description "The identifier to find references to, spelled as it appears in the code."))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    (aob-mcp-tools--xref
     (plist-get args :file)
     `(xref-backend-references backend ,(plist-get args :symbol))))))

(aob-mcp-deftool
 :name "xref_apropos"
 :description "Definitions across the project whose names match a pattern, as
file:line:text, at most 200 of them.  The major mode of FILE picks the xref
backend.  Costs a project-wide search in the user's Emacs and can take seconds;
it opens FILE if no buffer has it already."
 :args '((:name "file" :type string
          :description "Absolute path of a file in the project, whose major mode picks the backend.")
         (:name "pattern" :type string
          :description "Words matched against definition names; the backend decides how loosely."))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    (aob-mcp-tools--xref
     (plist-get args :file)
     `(xref-backend-apropos backend ,(plist-get args :pattern))))))

;;; imenu

(aob-mcp-deftool
 :name "imenu_symbols"
 :description "The symbol outline of one file from imenu, a line each as
\"kind: name:line\", nested kinds joined with a slash.  At most 200.  Works on a
file nobody has open, which costs opening it and running its major mode; a file
already open costs almost nothing."
 :args '((:name "file" :type string
          :description "Absolute path of the file to outline."))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    (aob-mcp-tools--in-file
     (plist-get args :file)
     `(progn
        (require 'imenu)
        ,(aob-mcp-tools--capped
          '(letrec
               ((walk
                 (lambda (alist kind)
                   (mapcan
                    (lambda (entry)
                      (cond
                       ((not (consp entry)) nil)
                       ((equal (car entry) "*Rescan*") nil)
                       ((imenu--subalist-p entry)
                        (funcall walk (cdr entry)
                                 (if kind
                                     (concat kind "/" (car entry))
                                   (car entry))))
                       (t
                        (let* ((where (if (consp (cdr entry))
                                          (cadr entry)
                                        (cdr entry)))
                               (pos (cond ((markerp where)
                                           (marker-position where))
                                          ((integerp where) where))))
                          (list (format "%s: %s:%s"
                                        (or kind "symbol")
                                        (substring-no-properties (car entry))
                                        (if pos
                                            (line-number-at-pos pos t)
                                          "?")))))))
                    alist))))
             (funcall walk (imenu--make-index-alist t) nil))
          '#'identity
          "imenu found nothing in this file"))))))

;;; tree-sitter

(defun aob-mcp-tools--treesit-probe (line column)
  "A form describing the node at LINE and COLUMN, or the whole file if no LINE."
  (if (null line)
      '(let ((root (treesit-buffer-root-node)))
         (append
          (list (format "parsers: %s"
                        (mapconcat (lambda (p)
                                     (format "%s" (treesit-parser-language p)))
                                   (treesit-parser-list) " "))
                (format "root: %s, %d named children"
                        (treesit-node-type root)
                        (treesit-node-child-count root t)))
          (or (mapcar (lambda (n)
                        (format "top: %s at line %d"
                                (treesit-node-type n)
                                (line-number-at-pos (treesit-node-start n) t)))
                      (treesit-node-children root t))
              (list "top: nothing"))))
    `(let* ((pos (save-excursion
                   (goto-char (point-min))
                   (forward-line ,(1- line))
                   (min (+ (point) ,(or column 0)) (line-end-position))))
            (node (treesit-node-at pos)))
       (if (null node)
           (format "no node at line %d" ,line)
         (append
          (list (format "node: %s%s"
                        (treesit-node-type node)
                        (if (treesit-node-check node 'named) "" " (anonymous)"))
                (format "bytes: %d-%d"
                        (position-bytes (treesit-node-start node))
                        (position-bytes (treesit-node-end node)))
                (format "lines: %d-%d"
                        (line-number-at-pos (treesit-node-start node) t)
                        (line-number-at-pos (treesit-node-end node) t)))
          (let ((chain nil)
                (up (treesit-node-parent node)))
            (while up
              (push (format "ancestor: %s" (treesit-node-type up)) chain)
              (setq up (treesit-node-parent up)))
            (nreverse chain))
          (or (mapcar (lambda (n)
                        (format "child: %s [%d-%d]"
                                (treesit-node-type n)
                                (treesit-node-start n)
                                (treesit-node-end n)))
                      (treesit-node-children node))
              (list "child: none")))))))

(aob-mcp-deftool
 :name "treesit_info"
 :description "What tree-sitter makes of a file.  With no LINE: the parsers, the
root node and the top-level nodes with their lines.  With a LINE, and optionally
a COLUMN: the node at that position, whether it is named, its byte range, its
line range, every ancestor up to the root, and its immediate children.  Says so
plainly when this Emacs has no tree-sitter or the buffer has no parser.  Opens
the file if nobody has it open."
 :args '((:name "file" :type string
          :description "Absolute path of the file to parse.")
         (:name "line" :type integer :optional t
          :description "1-based line; without it the answer is a whole-file summary.")
         (:name "column" :type integer :optional t
          :description "0-based column on LINE, counted in characters.  Default 0."))
 :handler
 (lambda (args conn id)
   (let ((line (aob-mcp-tools--int (plist-get args :line)))
         (column (aob-mcp-tools--int (plist-get args :column))))
     (aob-mcp-relay
      conn id
      (aob-mcp-tools--in-file
       (plist-get args :file)
       `(if (not (treesit-available-p))
            "this Emacs was built without tree-sitter"
          (require 'treesit)
          (if (null (treesit-parser-list))
              (format "no tree-sitter parser in this buffer (%s)" major-mode)
            ,(aob-mcp-tools--treesit-probe line column))))))))

;;; diagnostics

(aob-mcp-deftool
 :name "diagnostics"
 :description "What a checker has to say about one file, a line each as
\"severity line:column message\", at most 200.  Reads whichever of flymake or
flycheck is turned on in that buffer, and names both states when neither is.
The file must already be open: nothing has checked a file nobody opened, so this
never opens one, and it is cheap."
 :args '((:name "file" :type string
          :description "Absolute path of a file already open in the user's Emacs.")
         (:name "checker" :type string :optional t
          :description "Which checker to read; auto takes whichever one is on."
          :enum ["auto" "flymake" "flycheck"]))
 :handler
 (lambda (args conn id)
   (let ((want (or (plist-get args :checker) "auto")))
     (aob-mcp-relay
      conn id
      (aob-mcp-tools--in-file
       (plist-get args :file)
       `(cond
         ((and (bound-and-true-p flymake-mode)
               (member ,want '("auto" "flymake")))
          ,(aob-mcp-tools--capped
            '(flymake-diagnostics)
            '(lambda (d)
               (let ((beg (flymake-diagnostic-beg d)))
                 (format "%s %d:%d %s"
                         (flymake-diagnostic-type d)
                         (line-number-at-pos beg t)
                         (save-excursion (goto-char beg) (current-column))
                         (substring-no-properties
                          (flymake-diagnostic-text d)))))
            "flymake has nothing against this buffer"))
         ((and (bound-and-true-p flycheck-mode)
               (member ,want '("auto" "flycheck")))
          ,(aob-mcp-tools--capped
            'flycheck-current-errors
            '(lambda (e)
               (format "%s %s:%s %s"
                       (flycheck-error-level e)
                       (or (flycheck-error-line e) "?")
                       (or (flycheck-error-column e) 0)
                       (substring-no-properties
                        (or (flycheck-error-message e) ""))))
            "flycheck has nothing against this buffer"))
         (t (format "no checker to read here: flymake is %s, flycheck is %s"
                    (if (bound-and-true-p flymake-mode) "on" "off")
                    (if (bound-and-true-p flycheck-mode) "on" "off"))))
       'only-if-open)))))

;;; this server

(aob-mcp-deftool
 :name "tool_names"
 :description "The names of the tools this server offers, one to a line, for
building an allowed-tools list.  Answered in the sidecar, so it costs nothing
and never reaches the user's Emacs."
 :args '((:name "prefix" :type string :optional t
          :description "Only the names starting with this."))
 :handler
 (lambda (args _conn _id)
   (let ((names (aob-mcp-tool-names (plist-get args :prefix))))
     (if names
         (mapconcat #'identity names "\n")
       "no tools registered"))))


;;; Other conversations

;; Who is calling rides in the URL as a token, because a session has no
;; id until session/new answers and the server list is part of that call.
;; The editing Emacs is the only side that can turn one back into a
;; session, so every form here is handed the token rather than a name.

(defun aob-mcp-tools--parent-form (token)
  "A form yielding the session TOKEN was spawned for, or nil."
  `(and (fboundp 'aob-mcp-host-session) (aob-mcp-host-session ,token)))

(aob-mcp-deftool
 :name "session_list"
 :description "Every conversation open in this editor, yours and everyone
else's: id, state, name and folder. The ids are what session_say takes."
 :args nil
 :handler
 (lambda (_args conn id)
   (aob-mcp-relay
    conn id
    `(if (not (fboundp 'aob-sessions))
         (list "no sessions here")
       (or (mapcar (lambda (s)
                     (format "%s  %s  %s  %s"
                             (aob-session-id s)
                             (aob-session-state s)
                             (aob-session-name s)
                             (or (aob-session-dir s) (aob-session-project s) "")))
                   (aob-sessions))
           (list "none open"))))))

(aob-mcp-deftool
 :name "session_say"
 :description "Say something to another conversation in this editor.
It lands in that conversation's
turn where its agent takes steering, and is queued for its next turn
where it does not. Returns which of the two happened. Use session_list
for the id."
 :args '((:name "id" :type string
          :description "the session id or name, as session_list prints it")
         (:name "text" :type string
          :description "what to say to it"))
 :handler
 (lambda (args conn id)
   (let ((text (plist-get args :text))
         (who (plist-get args :id)))
     (cond
      ((or (null who) (string-empty-p (string-trim who)))
       "which session? pass id")
      ((or (null text) (string-empty-p (string-trim text)))
       "nothing to say: pass text")
      (t
       (aob-mcp-relay
        conn id
        `(let* ((who ,who)
                (by-id (and (fboundp 'aob-session-get) (aob-session-get who)))
                ;; a name is what a person reads off a list, and what an
                ;; agent will send back — but two conversations can carry
                ;; one name, and guessing which is how a message goes to
                ;; the wrong agent
                (by-name (unless by-id
                           (and (fboundp 'aob-sessions)
                                (seq-filter (lambda (x)
                                              (equal (aob-session-name x) who))
                                            (aob-sessions)))))
                (s (or by-id (and (= (length by-name) 1) (car by-name)))))
           (cond
            ((and (null s) (cdr by-name))
             (cons (format "%d conversations are called %s — say which, by id:"
                           (length by-name) who)
                   (mapcar (lambda (x) (format "  %s  %s" (aob-session-id x)
                                               (or (aob-session-dir x) "")))
                           by-name)))
            ((null s) (list (format "no session called %s" who)))
            ((not (fboundp 'aob-prompt)) (list "no way to talk to it here"))
            ((and (eq (aob-session-state s) 'working)
                  (fboundp 'aob-acp--steers-p)
                  (ignore-errors (aob-acp--steers-p s))
                  (fboundp 'aob-interject))
             (condition-case err (progn (aob-interject s ,text)
                                        (list (format "said to %s, into the turn it is running"
                                                      (aob-session-name s))))
               (error (list (format "%s would not take it: %s"
                                    (aob-session-name s)
                                    (error-message-string err))))))
            (t
             ;; a conversation with no process behind it cannot be told
             ;; anything, and saying so beats saying nothing
             (condition-case err (progn (aob-prompt s ,text nil)
                                        (list (format "said to %s (%s)"
                                                      (aob-session-name s)
                                                      (aob-session-state s))))
               (error (list (format "%s would not take it: %s"
                                    (aob-session-name s)
                                    (error-message-string err))))))))))))))

;;; todo list

(aob-mcp-deftool
 :name "todo_write"
 :description "Create a new todo list (a tasks.md file the editor keeps) for this session, and make it the session's current list. Returns its path."
 :args '((:name "title" :type string :optional t
          :description "Title for the list; defaults to blank.")
         (:name "slug" :type string :optional t
          :description "Short name, used in the path; defaults from the title.")
         (:name "sections" :type string :optional t
          :description "JSON array of {name, items: [text]}, e.g. [{\"name\": \"Now\", \"items\": [\"item 1\"]}]"))
 :handler
 (lambda (args conn id)
   (let* ((title (let ((v (plist-get args :title))) (and v (not (string-empty-p v)) v)))
          (slug (let ((v (plist-get args :slug))) (if (and v (not (string-empty-p v))) v (or title "tasks"))))
          (sections-json (plist-get args :sections))
          (sections
           (if sections-json
               (condition-case _err
                   (let* ((parsed (json-parse-string sections-json
                                                      :object-type 'plist
                                                      :array-type 'list))
                          (result nil))
                     (dolist (item parsed (nreverse result))
                       (let ((name (plist-get item :name))
                             (items (plist-get item :items)))
                         (push (cons name items) result))))
                 (error 'bad))
             nil)))
     (if (eq sections 'bad)
         "sections is not a JSON array of {name, items}; nothing was created"
     (aob-mcp-relay
      conn id
      `(condition-case err
           (let* ((ygg-todo-by 'agent)
                  (dir (or (ygg-todo-session-dir ,(aob-mcp-tools--parent-form aob-mcp-session))
                           (error "no calling session: cannot tell whose list this is")))
                  (path (ygg-todo-create dir ,slug ,title ',sections)))
             (ygg-todo-session-bind ,(aob-mcp-tools--parent-form aob-mcp-session) path)
             (split-string (ygg-todo-format path) "\n"))
         (error (list (error-message-string err)))))))))

(aob-mcp-deftool
 :name "todo_list"
 :description "The current todo list of this session with each item's id, section and state; call it before changing items."
 :args '((:name "file" :type string :optional t
          :description "Absolute path of another list to read; the session's current list if not given.")
         (:name "all" :type string :optional t
          :description "\"true\" to spell out finished items; otherwise they are listed by id only."))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    `(condition-case err
         (let* ((file (or ,(plist-get args :file)
                         (ygg-todo-session-file ,(aob-mcp-tools--parent-form aob-mcp-session)))))
           (if (null file)
               (list "no todo list yet: create one with todo_write")
             (progn
               (ygg-todo-note-read ,(aob-mcp-tools--parent-form aob-mcp-session) file)
               (split-string (ygg-todo-format file ,(equal (plist-get args :all) "true")) "\n"))))
       (error (list (error-message-string err)))))))

(aob-mcp-deftool
 :name "todo_add"
 :description "Add an item to a list and return it as [done] id text plus the list's path."
 :args '((:name "text" :type string
          :description "The item text.")
         (:name "section" :type string :optional t
          :description "The section name; the default section if not given.")
         (:name "file" :type string :optional t
          :description "Absolute path of the list; the session's current list if not given."))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    `(condition-case err
         (let* ((file (or ,(plist-get args :file)
                         (ygg-todo-session-file ,(aob-mcp-tools--parent-form aob-mcp-session))))
                (ygg-todo-by 'agent)
                (item (ygg-todo-add file ,(plist-get args :text)
                                    ,(plist-get args :section)))
                (id (plist-get item :id))
                (text (plist-get item :text))
                (done (plist-get item :done)))
           (ignore done text)
           (list (format "added %s" id)))
       (error (list (error-message-string err)))))))

(aob-mcp-deftool
 :name "todo_update"
 :description "Update an item: mark it done or rewrite its text. At least one of done or text is required. Answers the updated item line."
 :args '((:name "id" :type string
          :description "The item id (e.g., S.1).")
         (:name "done" :type string :optional t
          :description "\"true\" to mark done, \"false\" to mark undone.")
         (:name "text" :type string :optional t
          :description "New item text; the old text if not given.")
         (:name "expect" :type string :optional t
          :description "The item text you last saw, for safety.")
         (:name "file" :type string :optional t
          :description "Absolute path of the list; the session's current list if not given."))
 :handler
 (lambda (args conn id)
   (let ((done-str (plist-get args :done))
         (text (plist-get args :text)))
     (if (not (or text (member done-str '("true" "false"))))
         "nothing to change: pass done (\"true\" or \"false\") or text"
       (aob-mcp-relay
        conn id
        `(condition-case err
             (let* ((file (or ,(plist-get args :file)
                             (ygg-todo-session-file ,(aob-mcp-tools--parent-form aob-mcp-session))))
                    (ygg-todo-by 'agent)
                    (item-id ,(plist-get args :id))
                    (expect ,(plist-get args :expect))
                    (item nil))
               (when ,text
                 (setq item (ygg-todo-rewrite file item-id ,text expect)
                       item-id (plist-get item :id)
                       expect ,text))
               (when ,(and (member done-str '("true" "false")) t)
                 (setq item (ygg-todo-set-done file item-id ,(equal done-str "true") expect)))
               (list (format "%s %s" (plist-get item :id)
                             (if (plist-get item :done) "done" "open"))))
           (error (list (error-message-string err)))))))))

(aob-mcp-deftool
 :name "todo_remove"
 :description "Remove an item from a list. Answers removed ID."
 :args '((:name "id" :type string
          :description "The item id (e.g., S.1).")
         (:name "expect" :type string :optional t
          :description "The item text you last saw, for safety.")
         (:name "file" :type string :optional t
          :description "Absolute path of the list; the session's current list if not given."))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    `(condition-case err
         (let* ((file (or ,(plist-get args :file)
                         (ygg-todo-session-file ,(aob-mcp-tools--parent-form aob-mcp-session))))
                (ygg-todo-by 'agent))
           (ygg-todo-remove file ,(plist-get args :id) ,(plist-get args :expect))
           (list (format "removed %s" ,(plist-get args :id))))
       (error (list (error-message-string err)))))))

(provide 'aob-mcp-tools)
;;; aob-mcp-tools.el ends here
