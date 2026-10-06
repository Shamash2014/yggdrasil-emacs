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
(require 'ygg-skill-index)

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
 :description "References to a symbol across the project, as file:line:text."
 :args '((:name "file" :type string
          :description "Absolute path of a file the symbol belongs to; its mode picks the backend.")
         (:name "symbol" :type string
          :description "Identifier as spelled in the code."))
 :handler
 (lambda (args conn id)
   (aob-mcp-relay
    conn id
    (aob-mcp-tools--xref
     (plist-get args :file)
     `(xref-backend-references backend ,(plist-get args :symbol))))))

(aob-mcp-deftool
 :name "xref_apropos"
 :description "Definitions across the project whose names match a pattern, as file:line:text."
 :args '((:name "file" :type string
          :description "Absolute path of a project file; its mode picks the backend.")
         (:name "pattern" :type string
          :description "Words matched against definition names."))
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
 :description "A file's symbol outline from imenu, one \"kind: name:line\" per line."
 :args '((:name "file" :type string
          :description "Absolute path."))
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
 :description "Tree-sitter parse of a file: without line, parsers and top-level nodes; with line, the node there, its ancestors and children."
 :args '((:name "file" :type string
          :description "Absolute path.")
         (:name "line" :type integer :optional t
          :description "1-based line.")
         (:name "column" :type integer :optional t
          :description "0-based column in characters; default 0."))
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
 :description "Flymake or flycheck diagnostics for a file open in the editor, one \"severity line:column message\" per line."
 :args '((:name "file" :type string
          :description "Absolute path of an open file.")
         (:name "checker" :type string :optional t
          :description "auto takes whichever is on."
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
 :description "Names of this server's tools, one per line."
 :args '((:name "prefix" :type string :optional t
          :description "Only names starting with this."))
 :handler
 (lambda (args _conn _id)
   (let ((names (aob-mcp-tool-names (plist-get args :prefix))))
     (if names
         (mapconcat #'identity names "\n")
       "no tools registered"))))

;;; Skills

(defconst aob-mcp-tools-always-load '(:anthropic/alwaysLoad t)
  "The _meta that keeps a tool out from behind the client's tool search.")

(aob-mcp-deftool
 :name "skill_search"
 :description "Rank the skills this agent can use against a task; best first."
 :args '((:name "query" :type string
          :description "The task in your own words: goal, domain, tools involved.")
         (:name "k" :type integer :optional t
          :description "How many results; 5 if omitted."))
 :meta aob-mcp-tools-always-load
 :instructions "Before starting any non-trivial task, call skill_search with a query written from the task. When a result fits, use it before anything else: call the Skill tool with its name if you have one, otherwise call skill_load with the name and follow what it returns."
 :handler
 (lambda (args _conn _id)
   (aob-mcp-structured
    (list :results
          (vconcat (ygg-skill-index-search (plist-get args :query)
                                           (aob-mcp-tools--int (plist-get args :k))
                                           aob-mcp-project))))))

(aob-mcp-deftool
 :name "skill_load"
 :description "A skill's instructions and the names of the files beside them."
 :args '((:name "name" :type string
          :description "Skill name as skill_search gave it."))
 :meta aob-mcp-tools-always-load
 :handler
 (lambda (args _conn _id)
   (let* ((name (plist-get args :name))
          (found (ygg-skill-index-find name aob-mcp-project)))
     (if-let* ((skill (car found)))
         (let ((files (ygg-skill-index-files skill)))
           (concat (ygg-skill-index-body skill)
                   (format "\n\n---\nskill %s, in %s\n" (plist-get skill :name)
                           (file-name-directory (plist-get skill :path)))
                   (if files
                       (concat "files beside it (read them when the skill points at them):\n"
                               (mapconcat (lambda (f) (concat "- " f)) files "\n"))
                     "no other files")))
       (format "no skill named %s; closest: %s" name
               (string-join (cdr found) ", "))))))


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
 :description "Every conversation open in this editor: id, state, name, folder."
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
                   (seq-remove (lambda (s) (aob-session-ref s :hidden))
                               (aob-sessions)))
           (list "none open"))))))

(defun aob-mcp-tools--session-form (who found)
  "A form finding the session WHO names, by id or by a name only one carries.
FOUND is the form answering once it is found, with it bound to s."
  `(let* ((who ,who)
          (by-id (when-let* (((fboundp 'aob-session-get))
                             (x (aob-session-get who))
                             ((not (aob-session-ref x :hidden))))
                   x))
          ;; a name is what a person reads off a list, and what an
          ;; agent will send back — but two conversations can carry
          ;; one name, and guessing which is how a message goes to
          ;; the wrong agent
          (by-name (unless by-id
                     (and (fboundp 'aob-sessions)
                          (seq-filter (lambda (x)
                                        (and (equal (aob-session-name x) who)
                                             (not (aob-session-ref x :hidden))))
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
      (t ,found))))

(aob-mcp-deftool
 :name "session_say"
 :description "Send text to another conversation: into its running turn if it takes steering, else queued for its next."
 :args '((:name "id" :type string
          :description "Id or name from session_list.")
         (:name "text" :type string
          :description "What to say."))
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
        (aob-mcp-tools--session-form
         who
         `(cond
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

(aob-mcp-deftool
 :name "session_read"
 :description "Tail of another conversation, newest last, to diagnose one stuck or wrong. Not for polling: to wait, end your turn."
 :args '((:name "id" :type string
          :description "Id or name from session_list."))
 :handler
 (lambda (args conn id)
   (let ((who (plist-get args :id)))
     (if (or (null who) (string-empty-p (string-trim who)))
         "which session? pass id"
       (aob-mcp-relay
        conn id
        (aob-mcp-tools--session-form
         who
         '(if (fboundp 'aob-session-tail)
              (split-string (aob-session-tail s) "\n")
            (list "no way to read it here"))))))))

;;; review

(defun aob-mcp-tools--json (value)
  "VALUE, or the JSON it spells when it is a string; `unparsed' if it is not JSON."
  (if (stringp value)
      (condition-case nil
          (json-parse-string value :object-type 'plist :array-type 'list
                             :null-object nil :false-object nil)
        (error 'unparsed))
    value))

(defun aob-mcp-tools--object-p (value)
  (and (consp value) (keywordp (car value))))

(defun aob-mcp-tools--repo-relative (file dir)
  "FILE relative to DIR, symlinks resolved; nil when it lies outside.
A relative FILE is returned as it is."
  (if (not (file-name-absolute-p file))
      file
    (let ((root (file-name-as-directory (file-truename dir)))
          (path (file-truename file)))
      (and (string-prefix-p root path) (file-relative-name path root)))))

(defun aob-mcp-tools--review-comment (raw &optional dir)
  "RAW, one comment as the agent sent it, as the plist the compare view takes.
Absolute files are made relative to DIR.  A string instead says what is wrong
with it."
  (let* ((text (plist-get raw :text))
         (file-raw (plist-get raw :file))
         (file (if (and (stringp file-raw) (stringp dir))
                   (aob-mcp-tools--repo-relative file-raw dir)
                 file-raw))
         (line-raw (plist-get raw :line))
         (start-raw (plist-get raw :start_line))
         (line (aob-mcp-tools--int line-raw))
         (start (aob-mcp-tools--int start-raw))
         (side (or (plist-get raw :side) "new"))
         (type (plist-get raw :type))
         (title (plist-get raw :title))
         (priority-raw (plist-get raw :priority))
         (priority (aob-mcp-tools--int priority-raw))
         (confidence (plist-get raw :confidence))
         (level (or (plist-get raw :level)
                    (cond ((and (null file) (null line-raw)) "review")
                          ((null line-raw) "file")
                          ((and start-raw (not (eql start line))) "range")
                          (t "line"))))
         (problems
          (delq nil
                (list
                 (unless (and (stringp text) (not (string-blank-p text))) "text missing")
                 (unless (member level '("line" "range" "file" "review"))
                   (format "level %S is not line, range, file or review" level))
                 (unless (or (equal level "review") (and (stringp file-raw) (not (string-blank-p file-raw))))
                   "file missing")
                 (when (and (stringp file-raw) (null file)) "file is outside the repository")
                 (when (and line-raw (not (and line (> line 0)))) "line is not a positive integer")
                 (when (and start-raw (not (and start (> start 0)))) "start_line is not a positive integer")
                 (when (and (member level '("line" "range")) (null line-raw)) "line missing")
                 (when (and (equal level "range") (null start-raw)) "start_line missing")
                 (when (and line start (> start line)) "start_line is after line")
                 (unless (member side '("new" "old")) (format "side %S is not new or old" side))
                 (when (and type (not (stringp type))) "type is not a string")
                 (when (and title (not (stringp title))) "title is not a string")
                 (when (and priority-raw (not (and priority (<= 0 priority 3))))
                   "priority is not 0 to 3")
                 (when (and confidence (not (and (numberp confidence) (<= 0 confidence 1))))
                   "confidence is not a number from 0 to 1")))))
    (if problems
        (string-join problems ", ")
      (let ((side (intern side)))
        (list :level (intern level)
              :type (and type (not (string-blank-p type)) (intern type))
              :text text
              :file file :new-path file
              :side side :line line
              :start-line start :start-side side
              :title title :priority priority :confidence confidence)))))

(defun aob-mcp-tools--review-verdict (raw)
  "RAW, a verdict on the whole patch, as a review-level comment, or why not."
  (let ((correctness (plist-get raw :correctness))
        (made (aob-mcp-tools--review-comment
               (list :level "review" :text (plist-get raw :explanation)
                     :confidence (plist-get raw :confidence)))))
    (cond ((not (member correctness '("patch is correct" "patch is incorrect")))
           "correctness is not \"patch is correct\" or \"patch is incorrect\"")
          ((stringp made) made)
          (t (append made (list :correctness correctness))))))

(defun aob-mcp-tools--codex-finding (finding)
  "FINDING from a Codex review as an agent's comment."
  (let* ((where (plist-get finding :code_location))
         (path (plist-get where :absolute_file_path))
         (range (plist-get where :line_range))
         (start (plist-get range :start))
         (end (or (plist-get range :end) start)))
    (list :file path
          :line end
          :start_line (and start (not (equal start end)) start)
          :side "new"
          :title (plist-get finding :title)
          :text (plist-get finding :body)
          :priority (plist-get finding :priority)
          :confidence (plist-get finding :confidence_score))))

(defun aob-mcp-tools--review-entries (args)
  "Every comment ARGS carries as (LABEL . RAW), LABEL naming it in a complaint.
A string stands where an argument could not be read."
  (let ((comments (aob-mcp-tools--json (plist-get args :comments)))
        (verdict (aob-mcp-tools--json (plist-get args :verdict)))
        (codex (aob-mcp-tools--json (plist-get args :codex_review)))
        entries)
    (cond ((eq comments 'unparsed) (push "comments is not a JSON array" entries))
          ((not (listp comments)) (push "comments is not an array" entries))
          (t (seq-do-indexed (lambda (raw i) (push (cons (format "comment %d" (1+ i)) raw) entries))
                             comments)))
    (when verdict
      (push (cons "verdict" (and (aob-mcp-tools--object-p verdict) (list :verdict verdict)))
            entries))
    (when codex
      (if (not (aob-mcp-tools--object-p codex))
          (push "codex_review is not a JSON object" entries)
        (seq-do-indexed
         (lambda (finding i)
           (push (cons (format "finding %d" (1+ i))
                       (and (aob-mcp-tools--object-p finding)
                            (aob-mcp-tools--codex-finding finding)))
                 entries))
         (plist-get codex :findings))
        (when (plist-get codex :overall_correctness)
          (push (cons "codex verdict"
                      (list :verdict (list :correctness (plist-get codex :overall_correctness)
                                           :explanation (plist-get codex :overall_explanation)
                                           :confidence (plist-get codex :overall_confidence_score))))
                entries))))
    (nreverse entries)))

(defun aob-mcp-tools--review-comments (args)
  "The comments ARGS carries as (GOOD . BAD).
GOOD are compare-view plists; BAD are lines naming each rejected entry."
  (let (good bad)
    (dolist (entry (aob-mcp-tools--review-entries args))
      (let* ((raw (cdr-safe entry))
             (made (cond ((stringp entry) entry)
                         ((not (aob-mcp-tools--object-p raw)) "not an object")
                         ((plist-member raw :verdict)
                          (aob-mcp-tools--review-verdict (plist-get raw :verdict)))
                         (t (aob-mcp-tools--review-comment raw (plist-get args :dir))))))
        (cond ((stringp entry) (push entry bad))
              ((stringp made) (push (format "%s: %s" (car entry) made) bad))
              (t (push made good)))))
    (when (and (null good) (null bad)) (push "no comments given" bad))
    (cons (nreverse good) (nreverse bad))))

(aob-mcp-deftool
 :name "review_submit"
 :description "After reviewing a branch's diff, submit your comments; the user checks them as pending drafts, nothing is posted anywhere."
 :args '((:name "dir" :type string
          :description "Absolute path of the repository.")
         (:name "branch" :type string
          :description "Branch reviewed.")
         (:name "comments" :type array :items (:type "object") :optional t
          :description "[{file, line (end), side: new|old, start_line, level: line|range|file|review, type: issue|nit|question|todo|fix (todo and fix go back to the agent, not the PR), title, priority: 0-3, confidence: 0-1, text}]; only text required for a review-level comment.")
         (:name "verdict" :type object :optional t
          :description "{correctness: \"patch is correct\"|\"patch is incorrect\", explanation, confidence}.")
         (:name "codex_review" :type object :optional t
          :description "A Codex review output as-is, instead of or beside comments.")
         (:name "author" :type string :optional t
          :description "Who reviewed; your session's name if omitted."))
 :handler
 (lambda (args conn id)
   (let* ((dir (plist-get args :dir))
          (branch (plist-get args :branch))
          (parsed (and (stringp dir) (aob-mcp-tools--review-comments args)))
          (good (car parsed))
          (bad (cdr parsed))
          (author (plist-get args :author)))
     (cond
      ((not (and (stringp dir) (not (string-blank-p dir)))) "which repository? pass dir")
      ((not (and (stringp branch) (not (string-blank-p branch)))) "which branch? pass branch")
      ((null good)
       (string-join (cons "nothing submitted:" bad) "\n"))
      (t
       (aob-mcp-relay
        conn id
        `(progn
           (ignore-errors (require 'ygg-git-compare))
           (if (not (fboundp 'ygg-git-compare-comments-receive))
               (list "review comments not available in this Emacs")
             (condition-case err
                 (let* ((author (or ,(and (stringp author) (not (string-blank-p author)) author)
                                    (when-let* ((s ,(aob-mcp-tools--parent-form aob-mcp-session)))
                                      (aob-session-name s))
                                    "agent"))
                        (got (ygg-git-compare-comments-receive ,dir ,branch ',good author)))
                   (cons (let ((n (if (integerp (car-safe got)) (car got) ,(length good))))
                           (format "%d comment%s submitted for review on %s; the user will check them"
                                   n (if (= n 1) "" "s") ,branch))
                         ',(and bad (cons "skipped:" bad))))
               (error (list (error-message-string err))))))))))))

;;; todo list

(aob-mcp-deftool
 :name "todo_write"
 :description "Set this session's todo list and return it: a new tasks.md, or an existing one given file."
 :args '((:name "file" :type string :optional t
          :description "Existing tasks.md in the project to bind; title, slug and sections are then ignored.")
         (:name "title" :type string :optional t
          :description "List title.")
         (:name "slug" :type string :optional t
          :description "Path name; defaults from the title.")
         (:name "sections" :type string :optional t
          :description "JSON [{\"name\": \"Now\", \"items\": [\"text\"]}]."))
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
     (cond
      ((let ((f (plist-get args :file))) (and f (not (string-empty-p f))))
       (aob-mcp-relay
        conn id
        `(condition-case err
             (split-string
              (ygg-todo-format
               (ygg-todo-session-adopt ,(aob-mcp-tools--parent-form aob-mcp-session)
                                       ,(plist-get args :file)))
              "\n")
           (error (list (error-message-string err))))))
      ((eq sections 'bad)
       "sections is not a JSON array of {name, items}; nothing was created")
      (t
       (aob-mcp-relay
        conn id
        `(condition-case err
             (let* ((ygg-todo-by 'agent)
                    (dir (or (ygg-todo-session-dir ,(aob-mcp-tools--parent-form aob-mcp-session))
                             (error "no calling session: cannot tell whose list this is")))
                    (path (ygg-todo-create dir ,slug ,title ',sections)))
               (ygg-todo-session-bind ,(aob-mcp-tools--parent-form aob-mcp-session) path)
               (split-string (ygg-todo-format path) "\n"))
           (error (list (error-message-string err))))))))))

(aob-mcp-deftool
 :name "todo_list"
 :description "This session's todo list with item ids, sections and states; read it before changing items."
 :args '((:name "file" :type string :optional t
          :description "Absolute path of another list.")
         (:name "all" :type string :optional t
          :description "\"true\" to show finished items' text."))
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
 :description "Add an item to a todo list; returns its id."
 :args '((:name "text" :type string
          :description "Item text.")
         (:name "section" :type string :optional t
          :description "Section name; default section if omitted.")
         (:name "file" :type string :optional t
          :description "List path; the session's list if omitted."))
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
 :description "Mark a todo item done or undone, or rewrite it; pass done or text."
 :args '((:name "id" :type string
          :description "Item id, e.g. S.1.")
         (:name "done" :type string :optional t
          :description "\"true\" or \"false\".")
         (:name "text" :type string :optional t
          :description "New item text.")
         (:name "expect" :type string :optional t
          :description "Item text you last saw; finds the item if ids moved.")
         (:name "file" :type string :optional t
          :description "List path; the session's list if omitted."))
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
 :description "Remove a todo item."
 :args '((:name "id" :type string
          :description "Item id, e.g. S.1.")
         (:name "expect" :type string :optional t
          :description "Item text you last saw; finds the item if ids moved.")
         (:name "file" :type string :optional t
          :description "List path; the session's list if omitted."))
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
