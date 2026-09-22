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


;;; Delegation — the caller's own subagents

;; Who is calling rides in the URL as a token, because a session has no
;; id until session/new answers and the server list is part of that call.
;; The editing Emacs is the only side that can turn one back into a
;; session, so every form here is handed the token rather than a name.

(defun aob-mcp-tools--parent-form (token)
  "A form yielding the session TOKEN was spawned for, or nil."
  `(and (fboundp 'aob-mcp-host-session) (aob-mcp-host-session ,token)))

(defun aob-mcp-tools--session-form (id)
  "A form yielding the session ID names, or nil."
  `(and (fboundp 'aob-session-get) (aob-session-get ,id)))

(aob-mcp-deftool
 :name "subagent_spawn"
 :description "Delegate work to a new agent of your own, in this project.
Returns the new subagent's id straight away, before it has connected —
poll subagent_status with that id to see how it is getting on. The
subagent is a session in its own right: it has its own trace and its own
permissions, and it is stopped when you are stopped."
 :args '((:name "intent" :type string
          :description "what the subagent is being asked to do; its first turn")
         (:name "agent" :type string :optional t
          :description "which agent to use; yours by default")
         (:name "dir" :type string :optional t
          :description "directory to work in; yours by default")
         (:name "model" :type string :optional t
          :description "model for the subagent; the agent's default otherwise"))
 :handler
 (lambda (args conn id)
   (let ((intent (plist-get args :intent)))
     (if (or (null intent) (string-empty-p (string-trim intent)))
         "a subagent needs something to do: pass intent"
       (aob-mcp-relay
        conn id
        `(let ((parent ,(aob-mcp-tools--parent-form aob-mcp-session)))
           (cond
            ((not (fboundp 'aob-subagent-spawn)) (list "no subagent support here"))
            ((null parent) (list "no calling session: cannot tell who is delegating"))
            (t (let ((kid (aob-subagent-spawn parent ,intent
                                              ,(plist-get args :agent)
                                              ,(plist-get args :dir)
                                              ,(plist-get args :model))))
                 (if kid
                     (list (format "spawned %s" (aob-session-id kid))
                           (format "name %s" (aob-session-name kid)))
                   (list "the spawn returned nothing")))))))))))

(aob-mcp-deftool
 :name "subagent_list"
 :description "The subagents you have sent, with their ids and states.
Cheap: reads the session registry, opens nothing."
 :args nil
 :handler
 (lambda (_args conn id)
   (aob-mcp-relay
    conn id
    `(let ((parent ,(aob-mcp-tools--parent-form aob-mcp-session)))
       (cond
        ((not (fboundp 'aob-subagent-children)) (list "no subagent support here"))
        ((null parent) (list "no calling session"))
        (t (or (mapcar (lambda (k)
                         (format "%s  %s  %s" (aob-session-id k)
                                 (aob-session-state k) (aob-session-name k)))
                       (aob-subagent-children parent))
               (list "none sent yet"))))))))

(aob-mcp-deftool
 :name "subagent_status"
 :description "How one subagent is getting on: state, directory, its own
subagents, and the last thing it said."
 :args '((:name "id" :type string
          :description "the subagent id returned by subagent_spawn"))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    `(let ((s ,(aob-mcp-tools--session-form (plist-get args :id))))
       (if (and s (fboundp 'aob-subagent-status))
           (let ((st (aob-subagent-status s)))
             (list (format "id %s" (plist-get st :id))
                   (format "name %s" (plist-get st :name))
                   (format "state %s" (plist-get st :state))
                   (format "dir %s" (plist-get st :dir))
                   (format "children %s"
                           (or (string-join (plist-get st :children) " ") "none"))
                   (format "last %s" (plist-get st :last))))
         (list "no such subagent"))))))

(aob-mcp-deftool
 :name "subagent_kill"
 :description "Stop a subagent you sent, and anything it sent in turn.
Only your own: a session you did not delegate is refused."
 :args '((:name "id" :type string :description "the subagent id to stop"))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    `(let ((s ,(aob-mcp-tools--session-form (plist-get args :id)))
           (parent ,(aob-mcp-tools--parent-form aob-mcp-session)))
       (cond
        ((not (fboundp 'aob-subagent-kill)) (list "no subagent support here"))
        ((null s) (list "no such subagent"))
        ;; a tool that could stop any session would let one agent reach
        ;; into another's work
        ((not (and parent (equal (aob-session-id parent)
                                 (aob-session-ref s :parent-session))))
         (list "that subagent is not yours"))
        (t (aob-subagent-kill s)
           (list (format "stopped %s and its own" (aob-session-id s)))))))))

(provide 'aob-mcp-tools)
;;; aob-mcp-tools.el ends here
