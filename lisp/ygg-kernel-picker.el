;;; ygg-kernel-picker.el --- Pick the jupyter session a buffer runs in -*- lexical-binding: t; -*-

;; Third-party: emacs-jupyter, nerd-icons; drawn by vertico-posframe.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'eieio)
(require 'url-parse)
(require 'url-util)
(require 'iso8601)

(declare-function jupyter-available-kernelspecs "jupyter-kernelspec")
(declare-function jupyter-kernelspec-name "jupyter-kernelspec")
(declare-function jupyter-kernelspec-plist "jupyter-kernelspec")
(declare-function jupyter-kernelspec-resource-directory "jupyter-kernelspec")
(declare-function make-jupyter-kernelspec "jupyter-kernelspec")
(declare-function jupyter-kernel-spec "jupyter-kernel")
(declare-function jupyter-kernel "jupyter-kernel")
(declare-function jupyter-client "jupyter-client")
(declare-function jupyter-kernel-action "jupyter-client")
(declare-function jupyter-kernel-language-mode "jupyter-client")
(declare-function jupyter-bootstrap-repl "jupyter-repl")
(declare-function jupyter-run-repl "jupyter-repl")
(declare-function jupyter-repl-associate-buffer "jupyter-repl")
(declare-function jupyter-repl-available-repl-buffers "jupyter-repl")
(declare-function jupyter-connect-repl "jupyter-repl")
(declare-function jupyter-command "jupyter-env")
(declare-function jupyter-kernelspecs "jupyter-kernelspec")
(declare-function jupyter-server "jupyter-server-kernel")
(declare-function jupyter-server-kernel-p "jupyter-server-kernel")
(declare-function jupyter-server-kernel-server "jupyter-server-kernel")
(declare-function jupyter-api-get-kernel "jupyter-rest-api")
(declare-function jupyter-api-server-exists-p "jupyter-rest-api")
(declare-function delete-instance "eieio-base")
(declare-function jupyter-run-server-repl "jupyter-server")
(declare-function jupyter-connect-server-repl "jupyter-server")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function ygg-nb--language "layer-notebook")
(declare-function ygg-nb--client "layer-notebook")
(declare-function ygg-nb--remember "layer-notebook")
(defvar jupyter-current-client)
(defvar jupyter-use-zmq)
(defvar jupyter-kernel-language-mode-properties)
(defvar jupyter--servers)
(defvar jupyter-api-authentication-method)
(defvar ygg-nb--kernels)
(defvar ygg-nb-kernels)
(defvar ygg-term-display-action)
(defvar vertico-group-format)

(defgroup ygg-kernel-picker nil
  "Choose the jupyter session a buffer evaluates in."
  :group 'tools
  :prefix "ygg-kernel-picker-")

(defcustom ygg-kernel-picker-display-action nil
  "Display action for a chosen session's REPL; nil uses the terminal split."
  :type '(choice (const :tag "Terminal split" nil) sexp))

(defcustom ygg-kernel-picker-venv-names '(".venv")
  "Directories under the project root that hold a project Python venv."
  :type '(repeat string))

(defcustom ygg-kernel-picker-modeline t
  "Non-nil shows the buffer's session in the mode line."
  :type 'boolean)

(defface ygg-kernel-picker-group
  '((t :inherit default :weight bold :slant normal :height 0.85))
  "Section titles in the picker: small bold ink, no rule.")

(defface ygg-kernel-picker-current '((t :weight bold))
  "The word marking the session this buffer is attached to.")

(defconst ygg-kernel-picker--active "Active sessions")
(defconst ygg-kernel-picker--new "New session")
(defconst ygg-kernel-picker--running "Running on %s")

(defvar ygg-kernel-picker-version-function #'ygg-kernel-picker--probe-version
  "Called with LANGUAGE and INTERPRETER; returns a version string or nil.")

;;; Interpreters

(defun ygg-kernel-picker--language (language)
  "LANGUAGE as a lowercase string, whatever jupyter handed over."
  (and language (downcase (format "%s" language))))

(defun ygg-kernel-picker--language-label (language)
  (pcase language
    ("python" "Python")
    ("r" "R")
    ((pred stringp) (capitalize language))))

(defun ygg-kernel-picker--flavour (display-name)
  "The parenthesised tail of a kernelspec DISPLAY-NAME, e.g. uv."
  (and display-name
       (string-match "(\\([^()]+\\))\\s-*\\'" display-name)
       (match-string 1 display-name)))

(defun ygg-kernel-picker--version (version)
  "The dotted number in VERSION, which ark reports as a sentence."
  (and (stringp version)
       (if (string-match "[0-9]+\\(?:\\.[0-9]+\\)+" version)
           (match-string 0 version)
         version)))

(defun ygg-kernel-picker--display-name (language version flavour fallback)
  "Language, VERSION and FLAVOUR, or FALLBACK when no VERSION is known."
  (let ((label (ygg-kernel-picker--language-label language))
        (version (ygg-kernel-picker--version version)))
    (if (and label version)
        (string-join (delq nil (list label version (and flavour (format "(%s)" flavour))))
                     " ")
      (or fallback label "kernel"))))

(defun ygg-kernel-picker--venv-root (interpreter)
  "The venv INTERPRETER lives in, when it has a pyvenv.cfg."
  (when (and interpreter (file-name-absolute-p interpreter))
    (let ((root (file-name-directory
                 (directory-file-name (file-name-directory interpreter)))))
      (and (file-exists-p (expand-file-name "pyvenv.cfg" root)) root))))

(defun ygg-kernel-picker--pyvenv (root)
  "Alist of pyvenv.cfg keys under ROOT."
  (let ((cfg (expand-file-name "pyvenv.cfg" root)))
    (when (file-readable-p cfg)
      (with-temp-buffer
        (insert-file-contents cfg)
        (cl-loop for line in (split-string (buffer-string) "\n" t)
                 when (string-match "\\`\\s-*\\([^=]+?\\)\\s-*=\\s-*\\(.*?\\)\\s-*\\'" line)
                 collect (cons (match-string 1 line) (match-string 2 line)))))))

(defun ygg-kernel-picker--venv-facts (root)
  "Version and flavour of the venv at ROOT, from its pyvenv.cfg."
  (let ((cfg (ygg-kernel-picker--pyvenv root)))
    (list :version (or (alist-get "version_info" cfg nil nil #'equal)
                       (alist-get "version" cfg nil nil #'equal))
          :flavour (if (assoc "uv" cfg) "uv" "venv"))))

(defun ygg-kernel-picker--env-executable (plist binary)
  "BINARY on the PATH a kernelspec PLIST sets, or nil."
  (when-let* ((path (plist-get (plist-get plist :env) :PATH)))
    (cl-loop for dir in (split-string (substitute-env-vars path) path-separator t)
             for file = (expand-file-name binary dir)
             when (file-executable-p file) return file)))

(defun ygg-kernel-picker--interpreter (language plist)
  "The interpreter a kernelspec PLIST for LANGUAGE runs code in."
  (let ((exe (car (append (plist-get plist :argv) nil))))
    (or (and (equal language "r") (ygg-kernel-picker--env-executable plist "R"))
        (and exe (not (file-name-absolute-p exe)) (executable-find exe))
        exe)))

(defvar ygg-kernel-picker--versions (make-hash-table :test #'equal)
  "Language and interpreter to probed version, or the symbol none.")

(defun ygg-kernel-picker--run-version (language interpreter)
  (ignore-errors
    (pcase language
      ("python"
       (car (process-lines interpreter "-c"
                           "import platform; print(platform.python_version())")))
      ("r"
       (let ((line (car (process-lines-ignore-status interpreter "--version"))))
         (and line (string-match "R version \\([0-9.]+\\)" line)
              (match-string 1 line)))))))

(defun ygg-kernel-picker--probe-version (language interpreter)
  "Version of INTERPRETER, read once per interpreter and remembered."
  (when (and interpreter (not (file-remote-p default-directory))
             (file-executable-p interpreter))
    (let ((known (gethash (cons language interpreter) ygg-kernel-picker--versions)))
      (unless known
        (setq known (or (and-let* (((equal language "python"))
                                   (root (ygg-kernel-picker--venv-root interpreter)))
                          (plist-get (ygg-kernel-picker--venv-facts root) :version))
                        (ygg-kernel-picker--run-version language interpreter)
                        'none))
        (puthash (cons language interpreter) known ygg-kernel-picker--versions))
      (and (stringp known) known))))

(defun ygg-kernel-picker--where (interpreter)
  "INTERPRETER's venv when it has one, else the interpreter, abbreviated."
  (and interpreter
       (abbreviate-file-name
        (directory-file-name (or (ygg-kernel-picker--venv-root interpreter) interpreter)))))

;;; Rows

(defun ygg-kernel-picker--argv0 (plist)
  (car (append (plist-get plist :argv) nil)))

(defun ygg-kernel-picker--spec-row (name plist &optional host)
  "A New session row for the kernelspec NAME with PLIST, on HOST when remote.
A remote kernelspec's paths are the host's, so nothing here probes them."
  (let* ((language (ygg-kernel-picker--language (plist-get plist :language)))
         (interpreter (if host
                          (ygg-kernel-picker--argv0 plist)
                        (ygg-kernel-picker--interpreter language plist)))
         (display (plist-get plist :display_name)))
    (list :section ygg-kernel-picker--new :kind 'spec :language language
          :name (ygg-kernel-picker--display-name
                 language
                 (unless host (funcall ygg-kernel-picker-version-function language interpreter))
                 (ygg-kernel-picker--flavour display) display)
          :path (if host interpreter (ygg-kernel-picker--where interpreter))
          :host host :interpreter interpreter :spec-name name)))

(defun ygg-kernel-picker--age (time &optional now)
  "How long before NOW TIME was, in words."
  (let ((seconds (truncate (float-time (time-subtract (or now (current-time)) time)))))
    (cl-flet ((ago (n unit) (format "%d %s%s ago" n unit (if (= n 1) "" "s"))))
      (cond ((< seconds 60) "just now")
            ((< seconds 3600) (format "%d min ago" (/ seconds 60)))
            ((< seconds 86400) (ago (/ seconds 3600) "hour"))
            (t (ago (/ seconds 86400) "day"))))))

(defun ygg-kernel-picker--running-row (kernel specs host)
  "A row for KERNEL, already running on HOST, whose kernelspec SPECS may name.
KERNEL is a plist of :kernel-name, :time, :state, and :file for a
connection file or :server and :id for a kernel behind a server."
  (let* ((kernel-name (plist-get kernel :kernel-name))
         (plist (cdr (assoc kernel-name specs)))
         (language (ygg-kernel-picker--language (plist-get plist :language)))
         (display (plist-get plist :display_name)))
    (list :section (format ygg-kernel-picker--running host) :kind 'running
          :language language
          :name (ygg-kernel-picker--display-name
                 language nil (ygg-kernel-picker--flavour display)
                 (or display (unless (member kernel-name '(nil "")) kernel-name)))
          :host host :time (plist-get kernel :time) :state (plist-get kernel :state)
          :file (plist-get kernel :file)
          :server (plist-get kernel :server) :id (plist-get kernel :id))))

(defun ygg-kernel-picker--venv-row (root)
  "A New session row for the project venv at ROOT."
  (let ((facts (ygg-kernel-picker--venv-facts root))
        (python (expand-file-name "bin/python" root)))
    (list :section ygg-kernel-picker--new :kind 'venv :language "python"
          :name (ygg-kernel-picker--display-name
                 "python" (plist-get facts :version) (plist-get facts :flavour) "Python")
          :path (abbreviate-file-name (directory-file-name root))
          :interpreter python :venv root
          :missing (unless (file-expand-wildcards
                            (expand-file-name "lib/python*/site-packages/ipykernel" root))
                     "no ipykernel"))))

(defun ygg-kernel-picker--active-row (facts current)
  "An Active session row from a live client's FACTS; CURRENT marks it."
  (let* ((language (ygg-kernel-picker--language (plist-get facts :language)))
         (plist (plist-get facts :spec-plist))
         (host (plist-get facts :host))
         (interpreter (and plist (if host
                                     (ygg-kernel-picker--argv0 plist)
                                   (ygg-kernel-picker--interpreter language plist)))))
    (list :section ygg-kernel-picker--active :kind 'active :language language
          :name (ygg-kernel-picker--display-name
                 language (plist-get facts :version)
                 (ygg-kernel-picker--flavour (plist-get plist :display_name))
                 (plist-get plist :display_name))
          :path (if host interpreter (ygg-kernel-picker--where interpreter))
          :host host
          :state (plist-get facts :state)
          :buffers (or (plist-get facts :buffers) 0)
          :current (and current (eq (plist-get facts :client) current))
          :client (plist-get facts :client))))

(defun ygg-kernel-picker--rows (facts specs venvs current
                                       &optional language preferred running host)
  "Rows for live client FACTS, kernelspec SPECS and venv roots VENVS.
SPECS are (NAME . PLIST), on HOST when that is non-nil.  CURRENT is
the buffer's client.  A LANGUAGE keeps only the rows in it, and the
RUNNING rows whose language is unknown; a venv whose python a spec
already runs is dropped.  Specs named in PREFERRED lead the new ones
and are defaults.  RUNNING rows sit between the live and the new."
  (let* ((new (mapcar (lambda (spec)
                        (let ((row (ygg-kernel-picker--spec-row (car spec) (cdr spec) host)))
                          (if (member (car spec) preferred) (plist-put row :default t) row)))
                      specs))
         (new (append (cl-remove-if-not (lambda (row) (plist-get row :default)) new)
                      (cl-remove-if (lambda (row) (plist-get row :default)) new)))
         (taken (and venvs
                     (delq nil (mapcar (lambda (row)
                                         (ignore-errors
                                           (expand-file-name (plist-get row :interpreter))))
                                       new))))
         (rows (append
                (mapcar (lambda (f) (ygg-kernel-picker--active-row f current)) facts)
                running
                new
                (cl-loop for root in venvs
                         for row = (ygg-kernel-picker--venv-row root)
                         unless (member (ignore-errors
                                          (expand-file-name (plist-get row :interpreter)))
                                        taken)
                         collect row))))
    (if language
        (cl-remove-if-not (lambda (row)
                            (let ((own (plist-get row :language)))
                              (or (equal own language)
                                  (and (null own) (eq (plist-get row :kind) 'running)))))
                          rows)
      rows)))

(defun ygg-kernel-picker--candidates (rows)
  "ROWS as (STRING . ROW), each STRING unique by an invisible tail."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar (lambda (row)
              (let* ((name (plist-get row :name))
                     (n (puthash name (1+ (gethash name seen -1)) seen)))
                (cons (if (zerop n)
                          name
                        (concat name (propertize (string (+ #x100000 n)) 'invisible t)))
                      row)))
            rows)))

;;; Look

(defun ygg-kernel-picker--glyph (language)
  "A shadow-grey glyph for LANGUAGE."
  (propertize
   (or (and (require 'nerd-icons nil t)
            (ignore-errors
              (nerd-icons-mdicon (pcase language
                                   ("python" "nf-md-language_python")
                                   ("r" "nf-md-language_r")
                                   ((or "ruby" "rust" "go" "kotlin" "java")
                                    (concat "nf-md-language_" language))
                                   (_ "nf-md-console")))))
       (pcase language ("python" "py") ("r" "R") (_ "›")))
   'face 'shadow))

(defun ygg-kernel-picker--annotation (row)
  "The text after ROW's name: where it runs, its state, and whether it is ours."
  (let* ((path (plist-get row :path))
         (buffers (plist-get row :buffers))
         (running (eq (plist-get row :kind) 'running))
         (host (and (not running) (plist-get row :host))))
    (concat
     (and host (propertize host 'face 'shadow))
     (and host path "  ")
     (and path (propertize path 'face 'shadow))
     (and running
          (string-join
           (delq nil (list (plist-get row :state)
                           (and (plist-get row :time)
                                (propertize (ygg-kernel-picker--age (plist-get row :time))
                                            'face 'shadow))))
           "  "))
     (and (eq (plist-get row :kind) 'active)
          (concat "  " (or (plist-get row :state) "starting")
                  (propertize (format "  %d buffer%s" buffers (if (= buffers 1) "" "s"))
                              'face 'shadow)))
     (and (plist-get row :missing)
          (propertize (concat "  " (plist-get row :missing)) 'face 'shadow))
     (and (plist-get row :current)
          (propertize "  current" 'face 'ygg-kernel-picker-current)))))

(defun ygg-kernel-picker--affixation (candidates)
  "Glyph before and annotation after each of CANDIDATES, in one column."
  (let ((column (+ 4 (cl-loop for (string . _) in candidates
                              maximize (string-width string)))))
    (lambda (strings)
      (mapcar (lambda (string)
                (let ((row (cdr (assoc string candidates))))
                  (list string
                        (concat (ygg-kernel-picker--glyph (plist-get row :language)) " ")
                        (concat (propertize " " 'display `(space :align-to ,column))
                                (ygg-kernel-picker--annotation row)))))
              strings))))

(defun ygg-kernel-picker--grouper (candidates)
  (lambda (string transform)
    (if transform
        string
      (propertize (plist-get (cdr (assoc string candidates)) :section)
                  'face 'ygg-kernel-picker-group))))

(defun ygg-kernel-picker--table (candidates)
  "A completion table over CANDIDATES that keeps their order and sections."
  (let ((strings (mapcar #'car candidates)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . ygg-kernel)
                     (group-function . ,(ygg-kernel-picker--grouper candidates))
                     (affixation-function . ,(ygg-kernel-picker--affixation candidates))
                     (display-sort-function . identity)
                     (cycle-sort-function . identity))
        (complete-with-action action strings string pred)))))

;;; Live state

(defun ygg-kernel-picker--buffer-language ()
  "The language code here is in: the chunk's, else the major mode's."
  (ygg-kernel-picker--language
   (or (and (fboundp 'ygg-nb--language) (ignore-errors (ygg-nb--language)))
       (cond ((derived-mode-p 'python-base-mode 'python-mode) "python")
             ((derived-mode-p 'r-ts-mode 'ess-r-mode) "r")))))

(defun ygg-kernel-picker--repl-buffer-p ()
  (derived-mode-p 'jupyter-repl-mode))

(defun ygg-kernel-picker-current-client ()
  "The client this buffer evaluates in, or nil."
  (let ((language (ygg-kernel-picker--buffer-language)))
    (or (and language (fboundp 'ygg-nb--client) (ignore-errors (ygg-nb--client language)))
        (and (local-variable-p 'jupyter-current-client) jupyter-current-client))))

(defun ygg-kernel-picker--attached-p (buffer client)
  (or (and (local-variable-p 'jupyter-current-client buffer)
           (eq (buffer-local-value 'jupyter-current-client buffer) client))
      (and (local-variable-p 'ygg-nb--kernels buffer)
           (rassq client (buffer-local-value 'ygg-nb--kernels buffer)))))

(defun ygg-kernel-picker--buffer-count (client)
  "How many file buffers evaluate in CLIENT, its REPL not counted."
  (let ((repl (ignore-errors (slot-value client 'buffer))))
    (cl-loop for buffer in (buffer-list)
             count (and (not (eq buffer repl))
                        (not (buffer-base-buffer buffer))
                        (not (string-prefix-p " " (buffer-name buffer)))
                        (ygg-kernel-picker--attached-p buffer client)))))

(defun ygg-kernel-picker--client-facts (client)
  "What the picker shows of a live REPL CLIENT, read without a round trip."
  (let ((info (plist-get (ignore-errors (slot-value client 'kernel-info)) :language_info))
        (spec (ignore-errors (jupyter-kernel-action client #'jupyter-kernel-spec))))
    (list :client client
          :language (plist-get info :name)
          :version (plist-get info :version)
          :spec-plist (and spec (jupyter-kernelspec-plist spec))
          :state (ignore-errors (slot-value client 'execution-state))
          :host (ygg-kernel-picker--client-host client)
          :buffers (ygg-kernel-picker--buffer-count client))))

(defun ygg-kernel-picker--live-facts ()
  (mapcar (lambda (buffer)
            (ygg-kernel-picker--client-facts
             (buffer-local-value 'jupyter-current-client buffer)))
          (jupyter-repl-available-repl-buffers)))

(defun ygg-kernel-picker--specs ()
  "Every kernelspec as (NAME . PLIST), or nil when jupyter cannot list them."
  (condition-case err
      (mapcar (lambda (spec) (cons (jupyter-kernelspec-name spec)
                                   (jupyter-kernelspec-plist spec)))
              (jupyter-available-kernelspecs))
    (error (message "No kernelspecs: %s" (error-message-string err)) nil)))

(defun ygg-kernel-picker--project-venvs ()
  "Project-local venvs with a python in them."
  (unless (file-remote-p default-directory)
    (let ((root (or (and-let* ((project (project-current))) (project-root project))
                    default-directory)))
      (cl-loop for name in ygg-kernel-picker-venv-names
               for dir = (file-name-as-directory (expand-file-name name root))
               when (file-executable-p (expand-file-name "bin/python" dir))
               collect dir))))

;;; Hosts

(defvar ygg-kernel-picker--hosts (make-hash-table :test #'eq :weakness 'key)
  "Client to the TRAMP prefix of its kernel's host, for clients with no REPL.")

(defvar ygg-kernel-picker--joined (make-hash-table :test #'equal)
  "Connection file, or server URL and kernel id, to the client joined to it.")

(defvar ygg-kernel-picker--runtime-dirs (make-hash-table :test #'equal)
  "TRAMP prefix to that host's jupyter runtime directory.")

(defvar ygg-kernel-picker--server-history nil)

(defvar ygg-kernel-picker--server-tokens (make-hash-table :test #'equal)
  "Server base URL to the token it was pasted with, for this session only.")

(defconst ygg-kernel-picker--running-probe
  "import glob, json, os, socket, sys
found = []
for path in glob.glob(os.path.join(sys.argv[1], 'kernel-*.json')):
    try:
        with open(path) as f:
            info = json.load(f)
        socket.create_connection((info.get('ip') or '127.0.0.1', info['shell_port']), 0.5).close()
        found.append({'file': path, 'kernel_name': info.get('kernel_name', ''),
                      'mtime': os.path.getmtime(path)})
    except Exception:
        pass
print(json.dumps(found))"
  "Python run on a host: its runtime dir's connection files whose kernel answers.")

(defun ygg-kernel-picker-note-host (client remote)
  "Remember that CLIENT's kernel runs on the host of TRAMP prefix REMOTE."
  (when remote (puthash client remote ygg-kernel-picker--hosts))
  client)

(defun ygg-kernel-picker-server-url (client)
  "URL of the Jupyter server CLIENT's kernel sits behind, or nil."
  (and (fboundp 'jupyter-server-kernel-p)
       (ignore-errors
         (jupyter-kernel-action
          client (lambda (kernel)
                   (and (jupyter-server-kernel-p kernel)
                        (slot-value (jupyter-server-kernel-server kernel) 'url)))))))

(defun ygg-kernel-picker--repl-remote (client)
  "TRAMP prefix of CLIENT's REPL directory, where its kernel was started."
  (or (gethash client ygg-kernel-picker--hosts)
      (let ((repl (ignore-errors (slot-value client 'buffer))))
        (and (buffer-live-p repl)
             (file-remote-p (buffer-local-value 'default-directory repl))))))

(defun ygg-kernel-picker-remote (client)
  "TRAMP prefix of the host CLIENT's kernel runs on, or nil when it is local.
A kernel behind a server URL is reached over HTTP, so it counts as local."
  (and (not (ygg-kernel-picker-server-url client))
       (ygg-kernel-picker--repl-remote client)))

(defun ygg-kernel-picker--server-label (url)
  "HOST:PORT of a server URL."
  (let ((parsed (url-generic-parse-url url)))
    (format "%s:%s" (url-host parsed) (url-port parsed))))

(defun ygg-kernel-picker--client-host (client)
  (if-let* ((url (ygg-kernel-picker-server-url client)))
      (ygg-kernel-picker--server-label url)
    (and-let* ((remote (ygg-kernel-picker-remote client)))
      (file-remote-p remote 'host))))

(defun ygg-kernel-picker--parse-running (json remote)
  "Kernels in the running probe's JSON output, files under REMOTE, newest first."
  (sort (mapcar (lambda (entry)
                  (list :file (concat remote (plist-get entry :file))
                        :kernel-name (plist-get entry :kernel_name)
                        :time (seconds-to-time (plist-get entry :mtime))))
                (json-parse-string json :object-type 'plist :array-type 'list))
        (lambda (a b) (time-less-p (plist-get b :time) (plist-get a :time)))))

(defun ygg-kernel-picker--runtime-dir (remote)
  (with-memoization (gethash remote ygg-kernel-picker--runtime-dirs)
    (jupyter-command "--runtime-dir")))

(defun ygg-kernel-picker--host-kernels ()
  "Kernels answering on this buffer's remote host, from its runtime directory."
  (when-let* ((remote (file-remote-p default-directory))
              (dir (ignore-errors (ygg-kernel-picker--runtime-dir remote))))
    (with-temp-buffer
      (and (eql 0 (ignore-errors
                    (process-file "python3" nil '(t nil) nil
                                  "-c" ygg-kernel-picker--running-probe dir)))
           (ignore-errors (ygg-kernel-picker--parse-running (buffer-string) remote))))))

(defun ygg-kernel-picker--iso-time (string)
  (and (stringp string) (ignore-errors (encode-time (iso8601-parse string)))))

(defun ygg-kernel-picker--server-kernels (server)
  "The kernels running behind SERVER, as running-row plists."
  (mapcar (lambda (kernel)
            (list :server server :id (plist-get kernel :id)
                  :kernel-name (plist-get kernel :name)
                  :state (plist-get kernel :execution_state)
                  :time (ygg-kernel-picker--iso-time (plist-get kernel :last_activity))))
          (append (jupyter-api-get-kernel server) nil)))

(defun ygg-kernel-picker--server-specs (server)
  (mapcar (lambda (spec) (cons (jupyter-kernelspec-name spec) (jupyter-kernelspec-plist spec)))
          (jupyter-kernelspecs server)))

(defun ygg-kernel-picker--joined-keys (row)
  "Keys ROW's kernel is known by: its file, or its server's URL and id and the
file name a server gives that kernel in its runtime directory."
  (if-let* ((server (plist-get row :server)))
      (list (concat (slot-value server 'url) "#" (plist-get row :id))
            (format "kernel-%s.json" (plist-get row :id)))
    (list (plist-get row :file) (file-name-nondirectory (plist-get row :file)))))

(defun ygg-kernel-picker--joined-p (row)
  "Non-nil when this Emacs already has a live REPL on ROW's kernel."
  (cl-some (lambda (key)
             (let ((client (gethash key ygg-kernel-picker--joined)))
               (and client (ignore-errors (buffer-live-p (slot-value client 'buffer))))))
           (ygg-kernel-picker--joined-keys row)))

(defun ygg-kernel-picker--server-running-rows (server)
  (let ((specs (ygg-kernel-picker--server-specs server))
        (label (ygg-kernel-picker--server-label (slot-value server 'url))))
    (cl-remove-if #'ygg-kernel-picker--joined-p
                  (mapcar (lambda (kernel) (ygg-kernel-picker--running-row kernel specs label))
                          (ygg-kernel-picker--server-kernels server)))))

(defun ygg-kernel-picker--running-rows (specs host)
  "Rows for kernels running on HOST, named by SPECS, and behind known servers."
  (append
   (cl-remove-if #'ygg-kernel-picker--joined-p
                 (mapcar (lambda (kernel) (ygg-kernel-picker--running-row kernel specs host))
                         (and host (ygg-kernel-picker--host-kernels))))
   (cl-loop for server in (bound-and-true-p jupyter--servers)
            append (ignore-errors
                     (let ((jupyter-api-authentication-method nil))
                       (ygg-kernel-picker--server-running-rows server))))))

;;; Choosing

(defun ygg-kernel-picker--kernelspec (row)
  "The jupyter-kernelspec a New ROW starts."
  (pcase (plist-get row :kind)
    ('venv
     (make-jupyter-kernelspec
      :name (format "venv:%s" (plist-get row :venv))
      :plist (list :argv (vector (plist-get row :interpreter) "-m" "ipykernel_launcher"
                                 "-f" "{connection_file}")
                   :display_name (format "Python (%s)"
                                         (plist-get (ygg-kernel-picker--venv-facts
                                                     (plist-get row :venv))
                                                    :flavour))
                   :language "python"
                   :interrupt_mode "signal")))
    (_ (cl-find (plist-get row :spec-name) (jupyter-available-kernelspecs)
                :key #'jupyter-kernelspec-name :test #'equal))))

(defun ygg-kernel-picker--start (row)
  "Start the kernel a New ROW names and return its REPL client."
  (when-let* ((missing (plist-get row :missing)))
    (user-error "%s has %s; add it with: uv add --dev ipykernel"
                (plist-get row :path) missing))
  (let ((spec (or (ygg-kernel-picker--kernelspec row)
                  (user-error "Kernelspec %s is gone" (plist-get row :spec-name)))))
    (cond (jupyter-use-zmq
           (jupyter-bootstrap-repl
            (jupyter-client (jupyter-kernel :spec spec) 'jupyter-repl-client)))
          ((eq (plist-get row :kind) 'venv)
           (user-error "A venv kernel needs emacs-zmq"))
          ((file-remote-p default-directory)
           (user-error "A kernel on %s needs emacs-zmq" (file-remote-p default-directory 'host)))
          (t (jupyter-run-repl (concat (regexp-quote (jupyter-kernelspec-name spec)) "$"))))))

(defun ygg-kernel-picker--join (row)
  "Connect a new REPL to the running kernel ROW names and return its client."
  (let* ((server (plist-get row :server))
         (file (plist-get row :file))
         (client (cond
                  (server
                   ;; The kernel's files live with the server, not this buffer's host.
                   (let ((default-directory (expand-file-name "~/")))
                     (jupyter-connect-server-repl server (plist-get row :id))))
                  ((not jupyter-use-zmq)
                   (user-error "Joining a kernel by its connection file needs emacs-zmq"))
                  (t (let ((default-directory (file-name-directory file)))
                       (jupyter-connect-repl file))))))
    (if server
        (dolist (key (ygg-kernel-picker--joined-keys row))
          (puthash key client ygg-kernel-picker--joined))
      (puthash file client ygg-kernel-picker--joined))
    client))

(defun ygg-kernel-picker--open (row)
  "The client for ROW: its live one, a joined one, or a new kernel's."
  (pcase (plist-get row :kind)
    ('active (plist-get row :client))
    ('running (ygg-kernel-picker--join row))
    ('server-spec (let ((default-directory (expand-file-name "~/")))
                    (jupyter-run-server-repl (plist-get row :server) (plist-get row :spec-name))))
    (_ (ygg-kernel-picker--start row))))

(defun ygg-kernel-picker--client-language (client)
  (ygg-kernel-picker--language
   (plist-get (plist-get (slot-value client 'kernel-info) :language_info) :name)))

(defun ygg-kernel-picker--pin-mode (client)
  "Make this buffer's major mode the one CLIENT's language maps to."
  (let ((key (plist-get (plist-get (slot-value client 'kernel-info) :language_info) :name)))
    (unless (eq major-mode (jupyter-kernel-language-mode client))
      (setf (alist-get key jupyter-kernel-language-mode-properties nil nil #'equal)
            (list major-mode (syntax-table))))))

(defun ygg-kernel-picker--display (client)
  (when-let* ((repl (slot-value client 'buffer)))
    (display-buffer repl (or ygg-kernel-picker-display-action
                             (bound-and-true-p ygg-term-display-action)))))

(defun ygg-kernel-picker--attach (client)
  "Make this buffer evaluate in CLIENT and show its REPL."
  (let ((language (ygg-kernel-picker--client-language client)))
    (when (and (not (ygg-kernel-picker--repl-buffer-p))
               (equal language (ygg-kernel-picker--buffer-language)))
      (if (fboundp 'ygg-nb--remember)
          (ygg-nb--remember language client)
        (setq-local jupyter-current-client client))
      (unless (buffer-base-buffer)
        (when (derived-mode-p 'prog-mode)
          (ygg-kernel-picker--pin-mode client))
        (when (eq major-mode (jupyter-kernel-language-mode client))
          (jupyter-repl-associate-buffer client))))
    (ygg-kernel-picker--display client)
    client))

(defun ygg-kernel-picker--picked-language ()
  "The language this buffer's rows are kept to, none in a REPL."
  (unless (ygg-kernel-picker--repl-buffer-p)
    (ygg-kernel-picker--buffer-language)))

(defun ygg-kernel-picker--read ()
  "Ask for a session; return its row."
  (let* ((host (file-remote-p default-directory 'host))
         (specs (ygg-kernel-picker--specs)))
    (ygg-kernel-picker--choose
     (ygg-kernel-picker--rows (ygg-kernel-picker--live-facts)
                              specs
                              (ygg-kernel-picker--project-venvs)
                              (ygg-kernel-picker-current-client)
                              (ygg-kernel-picker--picked-language)
                              (mapcar #'cdr (bound-and-true-p ygg-nb-kernels))
                              (ygg-kernel-picker--running-rows specs host)
                              host))))

(defun ygg-kernel-picker--choose (rows)
  "Ask for one of ROWS in the picker; return it."
  (let* ((language (ygg-kernel-picker--picked-language))
         (candidates (ygg-kernel-picker--candidates rows))
         (default (car (or (cl-find-if (lambda (c) (plist-get (cdr c) :current)) candidates)
                           (unless (cl-find-if (lambda (c) (plist-get (cdr c) :client)) candidates)
                             (cl-find-if (lambda (c) (plist-get (cdr c) :default)) candidates))))))
    (unless candidates
      (user-error "No kernels for %s" (or language "this buffer")))
    (minibuffer-with-setup-hook
        (lambda () (setq-local vertico-group-format "%s"))
      (cdr (assoc (completing-read "Session: " (ygg-kernel-picker--table candidates)
                                   nil t nil nil default)
                  candidates)))))

;;;###autoload
(defun ygg-kernel-picker ()
  "Choose the jupyter session this buffer evaluates in, starting one if new."
  (interactive)
  (require 'jupyter)
  (require 'jupyter-repl)
  (ygg-kernel-picker--attach (ygg-kernel-picker--open (ygg-kernel-picker--read))))

;;; Jupyter servers

(defun ygg-kernel-picker--split-url (url)
  "URL pasted from a browser as (BASE . TOKEN).
BASE drops the query, a token path segment and a trailing lab or tree
page; TOKEN is the token the query or that segment carried, or nil."
  (let* ((parsed (url-generic-parse-url (string-trim url)))
         (path-and-query (url-path-and-query parsed))
         (path (or (car path-and-query) ""))
         (query (cdr path-and-query))
         (token (or (cadr (assoc "token" (and query (url-parse-query-string query))))
                    (and (string-match "/token/\\([^/]+\\)" path)
                         (match-string 1 path)))))
    (setf (url-filename parsed)
          (replace-regexp-in-string
           "\\(?:/\\(?:lab\\|tree\\)\\(?:/.*\\)?\\)?/*\\'" ""
           (replace-regexp-in-string "/token/[^/]+" "" path)))
    (cons (url-recreate-url parsed) token)))

(defun ygg-kernel-picker--without-token (url)
  "URL with any token it carries, as query or path segment, removed."
  (let ((case-fold-search t))
    (thread-last url
                 (replace-regexp-in-string "/token/[^/?#]*" "")
                 (replace-regexp-in-string "\\([?&]\\)token=[^&#]*&?" "\\1")
                 (replace-regexp-in-string "[?&]+\\(#\\|\\'\\)" "\\1"))))

(defun ygg-kernel-picker--scrub-history ()
  (setq ygg-kernel-picker--server-history
        (delete-dups (mapcar #'ygg-kernel-picker--without-token
                             ygg-kernel-picker--server-history))))

(ygg-kernel-picker--scrub-history)
(add-hook 'savehist-save-hook #'ygg-kernel-picker--scrub-history)

(defun ygg-kernel-picker--read-server-url ()
  "Read a server URL; keep its token in memory and return the URL without it."
  (ygg-kernel-picker--scrub-history)
  (let ((url (read-string "Jupyter server: " (car ygg-kernel-picker--server-history)
                          'ygg-kernel-picker--server-history)))
    (ygg-kernel-picker--scrub-history)
    (pcase-let ((`(,base . ,token) (ygg-kernel-picker--split-url url)))
      (when token (puthash base token ygg-kernel-picker--server-tokens)))
    (ygg-kernel-picker--without-token url)))

(defun ygg-kernel-picker--server (url)
  "The jupyter-server at URL, authenticated by the token URL carries.
A URL without one uses the token its server was last reached with."
  (pcase-let* ((`(,base . ,pasted) (ygg-kernel-picker--split-url url))
               (token (or pasted (gethash base ygg-kernel-picker--server-tokens)))
               (server (jupyter-server :url base)))
    (when token
      (puthash base token ygg-kernel-picker--server-tokens)
      (setf (slot-value server 'auth) `(("Authorization" . ,(concat "token " token)))))
    (unless (ignore-errors (jupyter-api-server-exists-p server))
      ;; Building the server registered it; a dead one would stall every later picker.
      (delete-instance server)
      (user-error "No Jupyter server answers at %s" base))
    server))

(defun ygg-kernel-picker--server-rows (server)
  "SERVER's running kernels, then a New session row for each of its kernelspecs."
  (let ((specs (ygg-kernel-picker--server-specs server))
        (label (ygg-kernel-picker--server-label (slot-value server 'url))))
    (append
     (cl-remove-if #'ygg-kernel-picker--joined-p
                   (mapcar (lambda (kernel) (ygg-kernel-picker--running-row kernel specs label))
                           (ygg-kernel-picker--server-kernels server)))
     (mapcar (lambda (spec)
               (append (list :kind 'server-spec :server server)
                       (ygg-kernel-picker--spec-row (car spec) (cdr spec) label)))
             specs))))

;;;###autoload
(defun ygg-kernel-picker-server (url)
  "Choose a session on the Jupyter server at URL: a running kernel or a new one.
URL may be the one jupyter prints, token and all."
  (interactive (list (ygg-kernel-picker--read-server-url)))
  (require 'jupyter)
  (require 'jupyter-repl)
  (require 'jupyter-server)
  (let* ((server (ygg-kernel-picker--server url))
         (language (ygg-kernel-picker--picked-language))
         (rows (ygg-kernel-picker--server-rows server)))
    (ygg-kernel-picker--attach
     (ygg-kernel-picker--open
      (ygg-kernel-picker--choose
       (if language
           (cl-remove-if-not (lambda (row) (member (plist-get row :language) (list nil language)))
                             rows)
         rows))))))

;;; Mode line

(defun ygg-kernel-picker-modeline ()
  "The session this buffer evaluates in, as dim plain text."
  (when-let* ((ygg-kernel-picker-modeline)
              (client (and (local-variable-p 'jupyter-current-client)
                           jupyter-current-client))
              (info (ignore-errors (plist-get (slot-value client 'kernel-info) :language_info))))
    (propertize (format "  %s%s %s"
                        (ygg-kernel-picker--display-name
                         (ygg-kernel-picker--language (plist-get info :name))
                         (plist-get info :version) nil "kernel")
                        (if-let* ((remote (ygg-kernel-picker--repl-remote client)))
                            (concat " on " (file-remote-p remote 'host))
                          "")
                        (or (ignore-errors (slot-value client 'execution-state)) ""))
                'face 'shadow)))

(defvar ygg-kernel-picker--modeline-entry '(:eval (ygg-kernel-picker-modeline)))
(put 'ygg-kernel-picker--modeline-entry 'risky-local-variable t)
(add-to-list 'mode-line-misc-info 'ygg-kernel-picker--modeline-entry t)

(provide 'ygg-kernel-picker)
;;; ygg-kernel-picker.el ends here
