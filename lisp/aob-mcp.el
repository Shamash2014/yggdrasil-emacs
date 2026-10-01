;;; aob-mcp.el --- an MCP server that lives beside Emacs, not inside it -*- lexical-binding: t; -*-

;;; Commentary:
;; The agents aob spawns want to ask Emacs things.  An MCP server hosted
;; in the editing Emacs answers them on the same thread that draws the
;; screen, so a slow tool is a frozen editor.  This one runs in its own
;; headless Emacs: it owns the socket, the framing and the tool table,
;; and hands anything needing live buffers to the editing Emacs as a
;; child process whose sentinel sends the reply.
;;
;; A reply is therefore never the return value of a handler.  Handlers
;; that answer at once return their value; handlers that cannot return
;; `aob-mcp-deferred', either carrying a key some later hook completes,
;; or having already arranged a sentinel of their own.
;;
;; The transport is HTTP because that is what the ACP side accepts: a
;; stdio server handed to a session is taken by the adapter and never
;; reaches the model.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(defgroup aob-mcp nil
  "The MCP server aob's agents call back into."
  :group 'tools :prefix "aob-mcp-")

(defcustom aob-mcp-port 17690
  "Port the server listens on, loopback only."
  :type 'natnum :group 'aob-mcp)

(defcustom aob-mcp-emacsclient
  (or (executable-find "emacsclient") "emacsclient")
  "The emacsclient that reaches the editing Emacs."
  :type 'string :group 'aob-mcp)

(defcustom aob-mcp-server-name "e31"
  "Server name of the editing Emacs."
  :type 'string :group 'aob-mcp)

(defcustom aob-mcp-relay-timeout 20
  "Seconds a question put to the editing Emacs may take."
  :type 'number :group 'aob-mcp)

(defcustom aob-mcp-output-max-lines 2000
  "Lines of a tool result sent back before the middle is cut to a file."
  :type 'natnum :group 'aob-mcp)

(defcustom aob-mcp-output-max-bytes 51200
  "Bytes of a tool result sent back before the middle is cut to a file."
  :type 'natnum :group 'aob-mcp)

(defconst aob-mcp--output-keep-days 7
  "Days a cut result's full text is kept before the next cut deletes it.")

(defconst aob-mcp-protocol-versions '("2025-06-18" "2025-03-26" "2024-11-05")
  "Protocol versions this server speaks, newest first.
A client that asks for one it knows gets that one back.  Answering
2024-11-05 to a client that asked for a streamable-HTTP version tells
it to fall back to the transport that version had, which is not the
one it is already talking over.")

(defconst aob-mcp-protocol-version (car aob-mcp-protocol-versions))

(defvar aob-mcp--tools (make-hash-table :test 'equal)
  "Tool name to spec.  Keyed, so re-registering replaces rather than shadows.")

(defvar aob-mcp--pending (make-hash-table :test 'equal)
  "Deferral key to a plist of the connection, id and expiry timer.")

(defvar aob-mcp--server nil)
(defvar aob-mcp--partial (make-hash-table :test 'eq))

(defvar aob-mcp-session nil
  "Which agent session is calling, bound for the length of one call.")

(defvar aob-mcp-project nil
  "The project the calling session was opened in, bound like `aob-mcp-session'.")

(defconst aob-mcp-deferred '&aob-mcp-deferred
  "Returned by a handler that will answer later.")

;;; Tools

(cl-defun aob-mcp-deftool (&key name description args handler meta instructions)
  "Register HANDLER as the tool NAME.
ARGS is a list of plists: :name :type :description, and optionally
:optional, :enum, :items, :properties.  HANDLER is called with one
plist, keyword per argument — never positionally, so an argument left
out is absent rather than a nil in the wrong seat.  META is the tool's
_meta in the listing; INSTRUCTIONS is what the server tells a client
about the tool as it connects."
  (unless (and name handler) (error "aob-mcp: a tool needs a name and a handler"))
  (puthash name (list :name name :description (or description "")
                      :args args :handler handler
                      :meta meta :instructions instructions)
           aob-mcp--tools)
  name)

(defun aob-mcp-tool-names (&optional prefix)
  "Every registered tool, or those starting with PREFIX."
  (let (out)
    (maphash (lambda (k _v)
               (when (or (null prefix) (string-prefix-p prefix k)) (push k out)))
             aob-mcp--tools)
    (sort out #'string<)))

(defun aob-mcp--arg-schema (arg)
  (let ((s (list :type (format "%s" (or (plist-get arg :type) "string"))
                 :description (or (plist-get arg :description) ""))))
    (dolist (k '(:enum :items :properties))
      (when-let* ((v (plist-get arg k)))
        (setq s (plist-put s k v))))
    s))

(defun aob-mcp--schema (spec)
  (let ((props nil) (required nil))
    (dolist (arg (plist-get spec :args))
      (setq props (plist-put props
                             (intern (concat ":" (plist-get arg :name)))
                             (aob-mcp--arg-schema arg)))
      (unless (plist-get arg :optional)
        (push (plist-get arg :name) required)))
    (list :type "object"
          :properties (or props (list))
          :required (vconcat (nreverse required)))))

(defun aob-mcp--listing ()
  (vconcat
   (mapcar (lambda (name)
             (let ((spec (gethash name aob-mcp--tools)))
               (append (list :name name
                             :description (plist-get spec :description)
                             :inputSchema (aob-mcp--schema spec))
                       (when-let* ((meta (plist-get spec :meta)))
                         (list :_meta meta)))))
           (aob-mcp-tool-names))))

(defun aob-mcp--instructions ()
  "What the tools have to tell a client as it connects, or nil."
  (when-let* ((said (delq nil (mapcar (lambda (name)
                                        (plist-get (gethash name aob-mcp--tools)
                                                   :instructions))
                                      (aob-mcp-tool-names)))))
    (string-join said "\n\n")))

;;; Answering

(defun aob-mcp--send (conn obj)
  "Write OBJ to CONN as one HTTP response, if CONN is still there.
A peer that half-closed took its connection with it: Emacs deletes the
process, and writing to it would signal rather than reach anyone."
  (when (process-live-p conn)
    (let ((body (encode-coding-string (json-serialize obj) 'utf-8 t)))
      (ignore-errors
        (process-send-string
         conn (concat "HTTP/1.1 200 OK\r\n"
                      "Content-Type: application/json\r\n"
                      (format "Content-Length: %d\r\n" (length body))
                      "Connection: close\r\n\r\n" body))
        (process-send-eof conn)))))

(defun aob-mcp--accepted (conn)
  "Answer CONN with an empty 202.
A notification carries no id and wants no result, but it arrived as an
HTTP request and the client waits for a response to it like any other:
saying nothing is how a server that works reads as one that hangs."
  (when (process-live-p conn)
    (ignore-errors
      (process-send-string conn (concat "HTTP/1.1 202 Accepted\r\n"
                                        "Content-Length: 0\r\n"
                                        "Connection: close\r\n\r\n"))
      (process-send-eof conn))))

(defun aob-mcp--not-allowed (conn)
  "Tell CONN this server takes POST and nothing else.
A streamable-HTTP client opens a GET to listen for what the server has
to say on its own; this one never says anything on its own, and a
clean refusal is what lets the client get on with posting."
  (when (process-live-p conn)
    (ignore-errors
      (process-send-string conn (concat "HTTP/1.1 405 Method Not Allowed\r\n"
                                        "Allow: POST\r\n"
                                        "Content-Length: 0\r\n"
                                        "Connection: close\r\n\r\n"))
      (process-send-eof conn))))

(defun aob-mcp--result (conn id value)
  (aob-mcp--send conn `(:jsonrpc "2.0" :id ,id :result ,value)))

(defun aob-mcp--error (conn id code message)
  (aob-mcp--send conn `(:jsonrpc "2.0" :id ,id
                        :error (:code ,code :message ,message))))

(defun aob-mcp--lines (text)
  "TEXT as lines, when emacsclient printed a list of strings.
The printed list carries a quote and a paren per line, which an agent
pays for in tokens and reads around."
  (or (and (stringp text)
           (string-prefix-p "(" (string-trim-left text))
           (condition-case nil
               (let ((v (car (read-from-string text))))
                 (and (consp v) (seq-every-p #'stringp v)
                      (string-join v "\n")))
             (error nil)))
      (format "%s" text)))

(defun aob-mcp--output-dir ()
  (expand-file-name "aob-mcp-output" temporary-file-directory))

(defun aob-mcp--spill (text)
  "Write TEXT to a fresh file, first deleting spilled files gone stale."
  (let ((dir (aob-mcp--output-dir))
        (cutoff (time-subtract nil (days-to-time aob-mcp--output-keep-days)))
        (coding-system-for-write 'utf-8-unix))
    (make-directory dir t)
    (dolist (file (directory-files dir t directory-files-no-dot-files-regexp))
      (when (time-less-p (file-attribute-modification-time (file-attributes file))
                         cutoff)
        (ignore-errors (delete-file file))))
    (make-temp-file (expand-file-name "out-" dir) nil ".txt" text)))

(defun aob-mcp--fit (lines max-lines max-bytes)
  "The leading LINES that fit in MAX-LINES and MAX-BYTES."
  (cl-loop for line in lines
           for count from 1
           sum (1+ (string-bytes line)) into bytes
           while (and (<= count max-lines) (<= bytes max-bytes))
           collect line))

(defun aob-mcp--clip (line max-bytes from-end)
  "LINE cut to at most MAX-BYTES, keeping its start, or its end with FROM-END."
  (let* ((piece (lambda (chars)
                  (if from-end (substring line (- (length line) chars))
                    (substring line 0 chars))))
         (chars (min (length line) max-bytes))
         (over (- (string-bytes (funcall piece chars)) max-bytes)))
    (while (> over 0)
      (setq chars (max 0 (- chars over))
            over (- (string-bytes (funcall piece chars)) max-bytes)))
    (funcall piece chars)))

(defun aob-mcp--cap (text)
  "TEXT, or its head and tail around a line naming the file with all of it."
  (let* ((lines (split-string text "\n"))
         (total (length lines)))
    (if (and (<= total aob-mcp-output-max-lines)
             (<= (string-bytes text) aob-mcp-output-max-bytes))
        text
      (let* ((max-lines (/ aob-mcp-output-max-lines 2))
             (max-bytes (/ aob-mcp-output-max-bytes 2))
             (head (aob-mcp--fit lines max-lines max-bytes))
             (tail (nreverse (aob-mcp--fit (reverse lines) max-lines max-bytes)))
             (cut (- total (length head) (length tail)))
             (file (aob-mcp--spill text)))
        ;; a line longer than the whole half-budget still shows its ends
        (string-join
         (append (or head (list (aob-mcp--clip (car lines) max-bytes nil)))
                 (list (format "[... %d lines cut; full output in %s ...]" cut file))
                 (or tail (list (aob-mcp--clip (car (last lines)) max-bytes t))))
         "\n")))))

(defun aob-mcp--content (text)
  `(:content [(:type "text" :text ,(aob-mcp--cap (aob-mcp--lines text)))]))

(defun aob-mcp-structured (data)
  "A handler's value answering with DATA, a JSON object, as structured content."
  (list 'aob-mcp--structured data))

(defun aob-mcp--answer (value)
  "The result a handler's VALUE becomes."
  (if (eq (car-safe value) 'aob-mcp--structured)
      (let ((data (cadr value)))
        `(:content [(:type "text" :text ,(aob-mcp--cap (decode-coding-string (json-serialize data) 'utf-8)))]
          :structuredContent ,data))
    (aob-mcp--content value)))

(defun aob-mcp-defer (conn id &optional timeout)
  "Park CONN and ID under a fresh key for `aob-mcp-complete' to answer.
Nothing is parked forever: whatever was going to complete the call may
never happen, and a caller told nothing waits as long as its own
patience rather than ours."
  (let* ((key (format "%s-%s" id (random (expt 2 24))))
         (timer (run-at-time (or timeout aob-mcp-relay-timeout) nil
                             #'aob-mcp--expire key)))
    (puthash key (list :conn conn :id id :timer timer) aob-mcp--pending)
    key))

(defun aob-mcp--take (key)
  "Remove KEY from the pending table and stop its timer."
  (when-let* ((cell (gethash key aob-mcp--pending)))
    (remhash key aob-mcp--pending)
    (when-let* ((timer (plist-get cell :timer))) (cancel-timer timer))
    cell))

(defun aob-mcp--expire (key)
  (when-let* ((cell (aob-mcp--take key)))
    (aob-mcp--error (plist-get cell :conn) (plist-get cell :id)
                    -32000 "timed out waiting for an answer")))

(defun aob-mcp-complete (key value)
  "Answer the call parked under KEY with VALUE."
  (when-let* ((cell (aob-mcp--take key)))
    (aob-mcp--result (plist-get cell :conn) (plist-get cell :id)
                     (aob-mcp--content value))))

(defun aob-mcp-pending-count ()
  "How many calls are waiting on something."
  (hash-table-count aob-mcp--pending))

(defun aob-mcp-relay (conn id form &optional on-reply)
  "Ask the editing Emacs FORM, answer CONN when it replies.
The question is a child process, so this Emacs keeps serving while the
other one thinks.  With ON-REPLY, call it with whether the editor
answered and the text instead of answering CONN."
  (let ((out (generate-new-buffer " *aob-mcp-relay*"))
        (reply (or on-reply
                   (lambda (ok text)
                     (if ok
                         (aob-mcp--result conn id (aob-mcp--content text))
                       (aob-mcp--error conn id -32000 text)))))
        (settled nil)
        proc timer)
    (setq proc
          (make-process
           :name "aob-mcp-relay" :buffer out :noquery t
           :command (list aob-mcp-emacsclient "-s" aob-mcp-server-name
                          "--eval" (prin1-to-string form))
           :sentinel
           (lambda (p _event)
             (when (and (not settled) (memq (process-status p) '(exit signal)))
               (setq settled t)
               (when timer (cancel-timer timer))
               (let ((text (if (buffer-live-p out)
                               (with-current-buffer out
                                 (string-trim (buffer-string)))
                             ""))
                     (ok (eq 0 (process-exit-status p))))
                 (when (buffer-live-p out) (kill-buffer out))
                 (funcall reply ok
                          (if ok text
                            (format "the editing Emacs did not answer: %s"
                                    text))))))))
    ;; an editor sitting on a prompt never returns, and the child would
    ;; wait on it as long as the agent was willing to
    (setq timer
          (run-at-time
           aob-mcp-relay-timeout nil
           (lambda ()
             (unless settled
               (setq settled t)
               (when (process-live-p proc) (delete-process proc))
               (when (buffer-live-p out) (kill-buffer out))
               (funcall reply nil
                        (format "the editing Emacs did not answer within %ss"
                                aob-mcp-relay-timeout))))))
    aob-mcp-deferred))

;;; Dispatch

(defun aob-mcp--call (conn id params)
  (let* ((name (plist-get params :name))
         (spec (gethash name aob-mcp--tools))
         (args (or (plist-get params :arguments) nil)))
    (if (null spec)
        (aob-mcp--error conn id -32601 (format "no such tool: %s" name))
      (condition-case err
          (let ((value (funcall (plist-get spec :handler) args conn id)))
            (unless (eq value aob-mcp-deferred)
              (aob-mcp--result conn id (aob-mcp--answer value))))
        (error (aob-mcp--error conn id -32000 (error-message-string err)))))))

(defun aob-mcp--dispatch (conn req)
  (let ((id (plist-get req :id))
        (method (plist-get req :method))
        (params (plist-get req :params)))
    (pcase method
      ("initialize"
       (aob-mcp--result
        conn id (append
                 `(:protocolVersion ,(let ((want (plist-get params :protocolVersion)))
                                       (if (member want aob-mcp-protocol-versions)
                                           want
                                         aob-mcp-protocol-version))
                   :capabilities (:tools (:listChanged :false))
                   :serverInfo (:name "aob" :version "0.1"))
                 (when-let* ((said (aob-mcp--instructions)))
                   (list :instructions said)))))
      ("notifications/initialized" (aob-mcp--accepted conn))
      ("ping" (aob-mcp--result conn id (list)))
      ("tools/list" (aob-mcp--result conn id `(:tools ,(aob-mcp--listing))))
      ("tools/call" (aob-mcp--call conn id params))
      (_ (if (null id)
             ;; any other notification: nothing to answer, but something
             ;; to say, or the client waits out its own timeout
             (aob-mcp--accepted conn)
           (aob-mcp--error conn id -32601 (format "no such method: %s" method)))))))

;;; Transport

(defvar aob-mcp-key nil
  "The secret a caller presents as ?key=, or nil to answer anyone.")

(defvar aob-mcp-key-file nil
  "File holding the secret, read once at startup.
Not the command line: argv is world-readable on this machine, and a
token that scopes which session is calling is not a password.")

(defun aob-mcp--read-key ()
  (when (and (null aob-mcp-key) aob-mcp-key-file
             (file-readable-p aob-mcp-key-file))
    (setq aob-mcp-key
          (string-trim (with-temp-buffer
                         (insert-file-contents aob-mcp-key-file)
                         (buffer-string))))))

(defun aob-mcp--unauthorized (conn)
  "Refuse CONN: it did not present the secret."
  (when (process-live-p conn)
    (ignore-errors
      (process-send-string conn (concat "HTTP/1.1 401 Unauthorized\r\n"
                                        "Content-Length: 0\r\n"
                                        "Connection: close\r\n\r\n"))
      (process-send-eof conn))))

(defun aob-mcp--query (target key)
  (when (string-match (concat "[?&]" (regexp-quote key) "=\\([^&]*\\)") target)
    (url-unhex-string (match-string 1 target))))

(defun aob-mcp--filter (conn chunk)
  (let* ((buf (concat (gethash conn aob-mcp--partial "") chunk))
         (head (string-match "\r\n\r\n" buf)))
    (puthash conn buf aob-mcp--partial)
    (when head
      (let* ((headers (substring buf 0 head))
             (body (substring buf (match-end 0)))
             (len (and (string-match "[Cc]ontent-[Ll]ength: *\\([0-9]+\\)" headers)
                       (string-to-number (match-string 1 headers)))))
        ;; a body still arriving is not a malformed one
        (when (or (null len) (>= (string-bytes body) len))
          (remhash conn aob-mcp--partial)
          (let* ((verb (and (string-match "^\\([A-Z]+\\) " headers)
                            (match-string 1 headers)))
                 (target (and (string-match "^[A-Z]+ +\\([^ ]+\\)" headers)
                              (match-string 1 headers)))
                 (aob-mcp-session (and target (aob-mcp--query target "session")))
                 (aob-mcp-project (and target (aob-mcp--query target "project")))
                 (req (condition-case nil
                          (json-parse-string body :object-type 'plist
                                             :array-type 'list
                                             :false-object nil :null-object nil)
                        (error nil))))
            (cond ((and aob-mcp-key
                        (not (equal (and target (aob-mcp--query target "key"))
                                    aob-mcp-key)))
                   ;; every local process can reach this port; only the
                   ;; ones this Emacs handed the secret to may use it
                   (aob-mcp--unauthorized conn))
                  ((and verb (not (equal verb "POST")))
                   (aob-mcp--not-allowed conn))
                  (req (aob-mcp--dispatch conn req))
                  (t (aob-mcp--error conn nil -32700 "that was not JSON")))))))))

(defun aob-mcp--sentinel (conn _event)
  (unless (process-live-p conn) (remhash conn aob-mcp--partial)))

;;;###autoload
(defun aob-mcp-start ()
  "Listen on `aob-mcp-port'."
  (interactive)
  (aob-mcp-stop)
  (setq aob-mcp--server
        (make-network-process
         :name "aob-mcp" :server t :host "127.0.0.1" :service aob-mcp-port
         :family 'ipv4 :coding 'binary :noquery t
         :filter #'aob-mcp--filter :sentinel #'aob-mcp--sentinel))
  aob-mcp--server)

(defun aob-mcp-stop ()
  "Stop listening."
  (interactive)
  (when (process-live-p aob-mcp--server) (delete-process aob-mcp--server))
  (setq aob-mcp--server nil))

(defun aob-mcp-url ()
  "What an agent is told to call."
  (format "http://127.0.0.1:%d/mcp" aob-mcp-port))

(defun aob-mcp-run ()
  "Serve forever.  The entry point of the headless Emacs."
  (aob-mcp--read-key)
  (aob-mcp-start)
  ;; the top of the loop, not inside a filter: waiting for output here is
  ;; what serving is, while the same call inside a handler would re-enter
  ;; the filters it was called from
  (while t (accept-process-output nil 1)))

(provide 'aob-mcp)
;;; aob-mcp.el ends here
