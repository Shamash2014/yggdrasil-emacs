;;; ygg-dap-kotlin.el --- Kotlin debugging through kotlin-lsp's own adapter -*- lexical-binding: t; -*-

;; kotlin-lsp opens a DAP port per session on request; dape ships no entry for it.

;;; Code:

(require 'seq)
(require 'map)
(require 'cl-lib)
(require 'subr-x)
(require 'treesit)

(defvar dape-configs)
(defvar dape-history)
(defvar dape--connections)
(defvar dape--compile-after-fn)
(declare-function dape "dape")
(declare-function dape--config "dape")
(declare-function dape-config-get "dape")
(declare-function dape--config-eval "dape")
(declare-function dape--live-connections-root "dape")
(declare-function dape--kill-busy-wait "dape")
(declare-function eglot-current-server "eglot")
(declare-function eglot-execute-command "eglot")
(declare-function eglot-path-to-uri "eglot")
(declare-function eglot--project "eglot")
(declare-function eglot--capabilities "eglot")
(declare-function jsonrpc--process "jsonrpc")
(declare-function project-root "project")
(declare-function ygg-device-current "ygg-device")

(defgroup ygg-dap-kotlin nil
  "Kotlin debugging with kotlin-lsp through dape."
  :group 'tools
  :prefix "ygg-dap-kotlin-")

(defcustom ygg-dap-kotlin-adb "adb"
  "The adb executable."
  :type 'string)

(defconst ygg-dap-kotlin--modes '(kotlin-mode kotlin-ts-mode))

(defconst ygg-dap-kotlin--loopback '("localhost" "127.0.0.1" "::1"))

;;; The adapter inside kotlin-lsp

(defun ygg-dap-kotlin--adapter-p (server)
  "Whether eglot SERVER can open a debug adapter."
  (seq-contains-p (plist-get (plist-get (eglot--capabilities server) :executeCommandProvider)
                             :commands)
                  "start_debug_server"))

(defun ygg-dap-kotlin-server (&optional file)
  "The kotlin-lsp server managing FILE, else the current buffer."
  (let ((buffer (if file (find-file-noselect file) (current-buffer))))
    (or (with-current-buffer buffer
          (when-let* ((server (and (featurep 'eglot) (eglot-current-server))))
            (and (ygg-dap-kotlin--adapter-p server) server)))
        (user-error "No kotlin-lsp server in %s yet; let eglot connect first"
                    (buffer-name buffer)))))

(defun ygg-dap-kotlin--command (server command &rest arguments)
  (eglot-execute-command server command (vconcat arguments)))

(defun ygg-dap-kotlin--adapter-port (server)
  "A fresh adapter port from SERVER, one per session."
  (let ((port (ygg-dap-kotlin--command
               server "start_debug_server"
               (eglot-path-to-uri (project-root (eglot--project server))))))
    (unless (and (natnump port) (< 0 port 65536))
      (user-error "kotlin-lsp returned no debug port: %S" port))
    port))

(defvar ygg-dap-kotlin--compiling nil
  "Adapter ports handed to configs dape compiles before it connects.
Dape runs fn again after compiling; any other port in a config is a
restart's and belongs to a dead session.")

(defun ygg-dap-kotlin--resumed-p (config)
  "Whether CONFIG comes back from dape's compile step, already resolved."
  (when-let* ((port (plist-get config 'port))
              ((memql port ygg-dap-kotlin--compiling)))
    (setq ygg-dap-kotlin--compiling (delq port ygg-dap-kotlin--compiling))
    t))

(defun ygg-dap-kotlin--stop-adapter (port)
  "End the adapter on PORT, which closes after serving one connection."
  (ignore-errors
    (delete-process (open-network-stream "ygg-kotlin-adapter" nil "127.0.0.1" port))))

(defun ygg-dap-kotlin--release-compiling ()
  "Stop the adapters whose compile never came back to connect."
  (mapc #'ygg-dap-kotlin--stop-adapter ygg-dap-kotlin--compiling)
  (setq ygg-dap-kotlin--compiling nil))

(defun ygg-dap-kotlin--compile-finished (buffer status)
  "Release waiting adapters when dape's compile in BUFFER fails with STATUS."
  (when (and ygg-dap-kotlin--compiling
             (not (equal status "finished\n"))
             (buffer-local-value 'dape--compile-after-fn buffer))
    (ygg-dap-kotlin--release-compiling)))

(add-hook 'compilation-finish-functions #'ygg-dap-kotlin--compile-finished)

(defun ygg-dap-kotlin-connect (config)
  "CONFIG pointed at a fresh adapter, unless it returns from compiling."
  (if (ygg-dap-kotlin--resumed-p config)
      config
    (let* ((server (ygg-dap-kotlin-server (plist-get config :filePath)))
           (port (ygg-dap-kotlin--adapter-port server)))
      (when (plist-get config 'compile)
        (ygg-dap-kotlin--release-compiling)
        (push port ygg-dap-kotlin--compiling))
      (thread-first config
                    (plist-put 'host "127.0.0.1")
                    (plist-put 'port port)))))

(defun ygg-dap-kotlin-ensure (config)
  "Refuse early when no kotlin-lsp server manages the file."
  (ygg-dap-kotlin-server (dape-config-get config :filePath)))

;;; Main class at point

(defun ygg-dap-kotlin--child (node type)
  (seq-find (lambda (child) (equal (treesit-node-type child) type))
            (treesit-node-children node t)))

(defun ygg-dap-kotlin--name (node)
  (when-let* ((id (or (ygg-dap-kotlin--child node "simple_identifier")
                      (ygg-dap-kotlin--child node "type_identifier"))))
    (treesit-node-text id t)))

(defun ygg-dap-kotlin--package (root)
  (when-let* ((header (ygg-dap-kotlin--child root "package_header"))
              (id (ygg-dap-kotlin--child header "identifier")))
    (replace-regexp-in-string "[ \t\n]" "" (treesit-node-text id t))))

(defun ygg-dap-kotlin-facade-class (file &optional jvm-name)
  "The class Kotlin compiles FILE's top-level functions into."
  (or jvm-name
      (let ((base (replace-regexp-in-string "[^[:alnum:]_$]" "_" (file-name-base file))))
        (concat (upcase (substring base 0 1)) (substring base 1) "Kt"))))

(defun ygg-dap-kotlin--jvm-name (root)
  (seq-some (lambda (annotation)
              (let ((text (treesit-node-text annotation t)))
                (and (string-match "JvmName(\\s-*\"\\([^\"]+\\)\"" text)
                     (match-string 1 text))))
            (seq-filter (lambda (child) (equal (treesit-node-type child) "file_annotation"))
                        (treesit-node-children root t))))

(defun ygg-dap-kotlin--static-p (function)
  (when-let* ((modifiers (ygg-dap-kotlin--child function "modifiers")))
    (string-match-p "@JvmStatic\\_>" (treesit-node-text modifiers t))))

(defun ygg-dap-kotlin--owner (function)
  "The binary name of the class a static FUNCTION lives on, or nil."
  (let ((node (treesit-node-parent function))
        names)
    (while (and node (not (equal (treesit-node-type node) "source_file")))
      (pcase (treesit-node-type node)
        ((or "object_declaration" "class_declaration")
         (push (ygg-dap-kotlin--name node) names))
        ((or "class_body" "companion_object"))
        (_ (setq names nil node nil)))
      (when node (setq node (treesit-node-parent node))))
    (and names (string-join names "$"))))

(defun ygg-dap-kotlin--mains (root)
  (let (mains)
    (treesit-search-subtree
     root
     (lambda (node)
       (when (and (equal (treesit-node-type node) "function_declaration")
                  (equal (ygg-dap-kotlin--name node) "main"))
         (push node mains))
       nil))
    (nreverse mains)))

(defun ygg-dap-kotlin-main-class ()
  "The JVM class whose main is at point, else the file's first main, else nil."
  (unless (treesit-language-available-p 'kotlin)
    (user-error "No kotlin tree-sitter grammar"))
  (let* ((root (treesit-parser-root-node (treesit-parser-create 'kotlin)))
         (file (or (buffer-file-name) (buffer-name)))
         (classes
          (delq nil
                (mapcar (lambda (main)
                          (when-let* ((owner
                                       (if (equal (treesit-node-type (treesit-node-parent main))
                                                  "source_file")
                                           (ygg-dap-kotlin-facade-class
                                            file (ygg-dap-kotlin--jvm-name root))
                                         (and (ygg-dap-kotlin--static-p main)
                                              (ygg-dap-kotlin--owner main)))))
                            (cons main owner)))
                        (ygg-dap-kotlin--mains root))))
         (here (or (seq-find (lambda (class)
                               (<= (treesit-node-start (car class)) (point)
                                   (treesit-node-end (car class))))
                             classes)
                   (car classes)))
         (package (ygg-dap-kotlin--package root)))
    (when here
      (if package (concat package "." (cdr here)) (cdr here)))))

(defun ygg-dap-kotlin--main-class-or-error ()
  (or (ygg-dap-kotlin-main-class)
      (user-error "No main here; give one, as in kotlin :mainClass \"pkg.MainKt\"")))

;;; Launch

(defun ygg-dap-kotlin--build-tool-target (server uri main)
  (condition-case nil
      (ygg-dap-kotlin--command server "intellij.java.resolveBuildToolLaunch"
                               (list :uri uri :mainClass main))
    (error nil)))

(defun ygg-dap-kotlin--put-missing (config &rest pairs)
  "CONFIG with each key of PAIRS set to its value where CONFIG has none."
  (cl-loop for (key value) on pairs by #'cddr
           unless (or (plist-get config key) (null value))
           do (setq config (plist-put config key value)))
  config)

(defun ygg-dap-kotlin--gradle (config uri target)
  "CONFIG run by Gradle, which builds before it launches."
  (ygg-dap-kotlin--put-missing
   (plist-put config :type "intellij_gradle")
   :buildToolTarget (cons :uri
                          (cons uri
                                (cl-loop for key in '(:moduleName :projectPath :sourceSet)
                                         when (plist-get target key)
                                         append (list key (plist-get target key)))))
   :classPaths (plist-get target :scopeClassPaths)))

(defun ygg-dap-kotlin--jvm (config server uri)
  "CONFIG run as a plain JVM, built first with the project's own command."
  (let ((paths (condition-case err
                   (ygg-dap-kotlin--command server "intellij.java.resolveLaunch"
                                            (list :uri uri :overrides (make-hash-table)))
                 (error (user-error "kotlin-lsp cannot launch this file (%s); it debugs Gradle and Maven modules only"
                                    (error-message-string err)))))
        (build (ignore-errors
                 (ygg-dap-kotlin--command server "intellij.java.resolveBuildCommand"
                                          (list :uri uri)))))
    (setq config (ygg-dap-kotlin--put-missing
                  (plist-put config :type "intellij_jvm")
                  :classPaths (plist-get paths :classpath)
                  :modulePaths (plist-get paths :modulePath)
                  :javaExec (plist-get paths :javaExec)
                  :cwd (plist-get paths :workingDirectory)))
    (when (and (eq (plist-get build :supported) t)
               (not (plist-get config 'compile)))
      (setq config (plist-put config 'compile
                              (mapconcat #'shell-quote-argument (plist-get build :command) " ")))
      (when-let* ((cwd (plist-get build :cwd)))
        (setq config (plist-put config 'command-cwd cwd))))
    config))

(defun ygg-dap-kotlin-resolve-launch (config)
  "CONFIG with classpath, build and adapter resolved by kotlin-lsp."
  (if (ygg-dap-kotlin--resumed-p config)
      config
    (let* ((file (or (plist-get config :filePath)
                     (user-error "Launch needs a Kotlin file")))
           (server (ygg-dap-kotlin-server file))
           (uri (eglot-path-to-uri file))
           (target (ygg-dap-kotlin--build-tool-target server uri (plist-get config :mainClass))))
      (ygg-dap-kotlin-connect
       (if (equal (plist-get target :tool) "gradle")
           (ygg-dap-kotlin--gradle config uri target)
         (ygg-dap-kotlin--jvm config server uri))))))

;;; Attach

(defun ygg-dap-kotlin-attach-config (port &optional host)
  "A dape config attaching to the JVM debug port PORT on HOST.
kotlin-lsp only attaches on loopback, so a remote JVM needs a forward."
  (unless (or (null host) (member host ygg-dap-kotlin--loopback))
    (user-error "kotlin-lsp attaches on loopback only; forward %s:%s to a local port" host port))
  (list 'modes ygg-dap-kotlin--modes
        'ensure #'ygg-dap-kotlin-ensure
        'fn #'ygg-dap-kotlin-connect
        :type "intellij_jvm" :request "attach" :port port))

;;; Android: attach over adb

(defun ygg-dap-kotlin--adb (serial &rest args)
  "Output of adb ARGS on device SERIAL, or a user-error with it."
  (with-temp-buffer
    (let ((status (apply #'call-process ygg-dap-kotlin-adb nil t nil
                         (append (and serial (list "-s" serial)) args))))
      (unless (eql status 0)
        (user-error "adb %s failed: %s" (string-join args " ") (string-trim (buffer-string))))
      (buffer-string))))

(defun ygg-dap-kotlin-adb-parse-devices (output)
  "Serials of the ready devices in adb devices OUTPUT."
  (cl-loop for line in (split-string output "\n" t)
           when (string-match "\\`\\(\\S-+\\)\tdevice\\'" line)
           collect (match-string 1 line)))

(defun ygg-dap-kotlin-adb-parse-processes (jdwp ps)
  "Alist of name to pid for the debuggable pids in JDWP, named from PS output."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (line (split-string ps "\n" t))
      (when (string-match "\\`\\s-*\\([0-9]+\\)\\s-+\\(\\S-+\\)" line)
        (puthash (match-string 1 line) (match-string 2 line) names)))
    (cl-loop for pid in (split-string jdwp "\n" t "[ \t\r]+")
             when (string-match-p "\\`[0-9]+\\'" pid)
             collect (cons (format "%s (%s)" (gethash pid names "?") pid) pid))))

(defun ygg-dap-kotlin--selected-android ()
  "The serial of the device picked in ygg-device, when it is an Android one."
  (when-let* (((fboundp 'ygg-device-current))
              (device (ygg-device-current))
              ((eq (plist-get device :platform) 'android)))
    (plist-get device :id)))

(defun ygg-dap-kotlin--adb-serial (config)
  (or (plist-get config 'adb-serial)
      (ygg-dap-kotlin--selected-android)
      (let ((serials (ygg-dap-kotlin-adb-parse-devices (ygg-dap-kotlin--adb nil "devices"))))
        (pcase serials
          ('() (user-error "No adb device is ready"))
          (`(,only) only)
          (_ (completing-read "Device: " serials nil t))))))

(defun ygg-dap-kotlin--adb-jdwp (serial)
  "The debuggable pids adb jdwp lists on SERIAL; it streams, so read once and stop."
  (with-temp-buffer
    (let ((process (make-process :name "ygg-adb-jdwp" :buffer (current-buffer)
                                 :command (list ygg-dap-kotlin-adb "-s" serial "jdwp")
                                 :connection-type 'pipe :noquery t)))
      (unwind-protect
          (let ((deadline (+ (float-time) 3)))
            (while (and (process-live-p process)
                        (string-empty-p (buffer-string))
                        (< (float-time) deadline))
              (accept-process-output process 0.2))
            (accept-process-output process 0.3))
        (delete-process process))
      (buffer-string))))

(defun ygg-dap-kotlin--adb-pid (serial package)
  "PACKAGE's pid on SERIAL, else a debuggable process picked by name.
The pick also covers a PACKAGE that runs under an applicationIdSuffix."
  (or (and package
           (car (split-string (or (ignore-errors (ygg-dap-kotlin--adb serial "shell" "pidof" package))
                                  ""))))
      (let ((choices (ygg-dap-kotlin-adb-parse-processes
                      (ygg-dap-kotlin--adb-jdwp serial)
                      (ygg-dap-kotlin--adb serial "shell" "ps" "-A" "-o" "PID,NAME"))))
        (unless choices
          (user-error "No debuggable process on %s; is the app a debug build?" serial))
        (cdr (assoc (completing-read "Process: " choices nil t) choices)))))

(defun ygg-dap-kotlin--free-port ()
  (let ((server (make-network-process :name "ygg-free-port" :server t
                                      :host "127.0.0.1" :service t :noquery t)))
    (prog1 (process-contact server :service)
      (delete-process server))))

(defun ygg-dap-kotlin-adb-unforward (serial port)
  (ignore-errors (ygg-dap-kotlin--adb serial "forward" "--remove" (format "tcp:%d" port))))

(defun ygg-dap-kotlin--unforward-when-closed (process serial port)
  "Remove the forward of SERIAL's PORT once PROCESS dies."
  (add-function :after (process-sentinel process)
                (lambda (proc _event)
                  (unless (process-live-p proc)
                    (ygg-dap-kotlin-adb-unforward serial port)))))

(defun ygg-dap-kotlin--session-process (port)
  "The adapter connection of the dape session attached to PORT, or nil."
  (when-let* ((conn (seq-find (lambda (conn) (eql (plist-get (dape--config conn) :port) port))
                              dape--connections)))
    (jsonrpc--process conn)))

(defun ygg-dap-kotlin--unforward-with-session (serial port &optional tries)
  "Tie the forward to the session dape opens on PORT, or drop it if none comes.
Dape waits for the adapter while it connects, so the session shows up later."
  (run-at-time
   0.2 nil
   (lambda ()
     (let ((process (ygg-dap-kotlin--session-process port))
           (tries (or tries 0)))
       (cond ((process-live-p process)
              (ygg-dap-kotlin--unforward-when-closed process serial port))
             ((and (null process) (< tries 50))
              (ygg-dap-kotlin--unforward-with-session serial port (1+ tries)))
             (t (ygg-dap-kotlin-adb-unforward serial port)))))))

(defun ygg-dap-kotlin-application-id ()
  "The applicationId the project's Gradle scripts declare, or nil."
  (when-let* ((root (locate-dominating-file default-directory
                                            (lambda (dir)
                                              (seq-some (lambda (name) (file-exists-p (expand-file-name name dir)))
                                                        '("settings.gradle.kts" "settings.gradle"))))))
    (seq-some (lambda (script)
                (with-temp-buffer
                  (insert-file-contents script)
                  (let ((case-fold-search nil))
                    (when (re-search-forward
                           "^[ \t]*applicationId[ \t]*=?[ \t]*\"\\([^\"]+\\)\"" nil t)
                      (match-string 1)))))
              (directory-files-recursively root "\\`build\\.gradle\\(\\.kts\\)?\\'" nil
                                           (lambda (dir) (not (string-match-p "/\\(build\\|\\.gradle\\)\\'" dir)))))))

(defun ygg-dap-kotlin-adb-resolve (config)
  "CONFIG attached to an app's JVM through a fresh adb forward."
  (let* ((serial (ygg-dap-kotlin--adb-serial config))
         (pid (ygg-dap-kotlin--adb-pid serial (plist-get config 'adb-package)))
         (port (ygg-dap-kotlin--free-port)))
    (ygg-dap-kotlin--adb serial "forward" (format "tcp:%d" port) (format "jdwp:%s" pid))
    (condition-case err
        (prog1 (ygg-dap-kotlin-connect (plist-put config :port port))
          (ygg-dap-kotlin--unforward-with-session serial port))
      (error (ygg-dap-kotlin-adb-unforward serial port)
             (signal (car err) (cdr err))))))

;;; Entries

(defun ygg-dap-kotlin-install ()
  "Add the Kotlin launch, attach and adb attach entries to dape."
  (setf (alist-get 'kotlin dape-configs)
        (list 'modes ygg-dap-kotlin--modes
              'ensure #'ygg-dap-kotlin-ensure
              'fn #'ygg-dap-kotlin-resolve-launch
              :type "intellij_jvm" :request "launch"
              :filePath #'buffer-file-name
              :mainClass #'ygg-dap-kotlin--main-class-or-error))
  (setf (alist-get 'kotlin-attach dape-configs)
        (ygg-dap-kotlin-attach-config 5005))
  (setf (alist-get 'kotlin-adb dape-configs)
        (map-merge 'plist (ygg-dap-kotlin-attach-config 0)
                   (list 'fn #'ygg-dap-kotlin-adb-resolve
                         'adb-serial nil
                         'adb-package #'ygg-dap-kotlin-application-id))))

(with-eval-after-load 'dape
  (ygg-dap-kotlin-install))

(provide 'ygg-dap-kotlin)
;;; ygg-dap-kotlin.el ends here
