;;; ygg-compose-preview.el --- Compose @Preview renders in a side window -*- lexical-binding: t; -*-

;; Third-party: the compose-preview CLI (compose-ai-tools) and the preview
;; daemon its Gradle plugin describes.  The first render of a file goes
;; through the CLI; after it a warm daemon is brought up for the module, and
;; saves recompile with Gradle and re-render through the daemon.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jsonrpc)
(require 'ygg-jdk)
(require 'yggdrasil-localleader nil t)

(declare-function yggdrasil-localleader-def "yggdrasil-localleader" (mode key def &optional label))

(defgroup ygg-compose-preview nil
  "Jetpack Compose and Compose Multiplatform previews."
  :group 'tools)

(defcustom ygg-compose-preview-program "compose-preview"
  "The compose-preview CLI, a name on the exec path or an absolute path."
  :type 'string)

(defcustom ygg-compose-preview-on-save t
  "Re-render on save while the preview window is showing."
  :type 'boolean)

(defcustom ygg-compose-preview-use-daemon t
  "Bring up the warm preview daemon after the first CLI render."
  :type 'boolean)

(defcustom ygg-compose-preview-a11y t
  "Ask the daemon for accessibility findings, one line under each image."
  :type 'boolean)

(defcustom ygg-compose-preview-mise-exec t
  "Run the CLI and Gradle under mise exec when mise is installed."
  :type 'boolean)

(defcustom ygg-compose-preview-window-width 0.4
  "Width of the preview side window, a fraction of the frame."
  :type 'number)

(defconst ygg-compose-preview--buffer "*compose preview*")
(defconst ygg-compose-preview--protocol-version 2)

(cl-defstruct (ygg-compose-preview--session (:constructor ygg-compose-preview--session-create)
                                            (:copier nil))
  root module-dir gradle-path file source
  ids previews
  init-script conn daemon subscribed launch
  busy pending started waiting watchdog error)

(defvar ygg-compose-preview--sessions (make-hash-table :test #'equal)
  "Module directory -> its session.")

(defvar-local ygg-compose-preview--shown nil
  "The session the preview buffer is drawing.")

;;; Pure helpers

(defun ygg-compose-preview--gradle-root (file)
  "The directory holding FILE's settings.gradle, or nil."
  (locate-dominating-file
   file (lambda (dir)
          (or (file-exists-p (expand-file-name "settings.gradle.kts" dir))
              (file-exists-p (expand-file-name "settings.gradle" dir))))))

(defun ygg-compose-preview--module-dir (file root)
  "The nearest directory at or under ROOT that has a Gradle build file for FILE."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (locate-dominating-file
     file (lambda (dir)
            (and (string-prefix-p root (file-name-as-directory (expand-file-name dir)))
                 (or (file-exists-p (expand-file-name "build.gradle.kts" dir))
                     (file-exists-p (expand-file-name "build.gradle" dir))))))))

(defun ygg-compose-preview--gradle-path (module-dir root)
  "The Gradle project path of MODULE-DIR under ROOT, like :feature:ui."
  (let ((rel (directory-file-name
              (file-relative-name (expand-file-name module-dir) (expand-file-name root)))))
    (if (member rel '("." "")) ":" (concat ":" (string-replace "/" ":" rel)))))

(defun ygg-compose-preview--package (text)
  "The package declared in Kotlin source TEXT, or nil."
  (when (string-match "^[ \t]*package[ \t]+\\([[:alnum:]_.]+\\)" text)
    (match-string 1 text)))

(defun ygg-compose-preview--file-class (text file)
  "The JVM facade class of Kotlin FILE with source TEXT."
  (let ((pkg (ygg-compose-preview--package text))
        (name (if (string-match "@file:JvmName(\"\\([^\"]+\\)\")" text)
                  (match-string 1 text)
                (let ((base (file-name-base file)))
                  (concat (upcase (substring base 0 1)) (substring base 1) "Kt")))))
    (if pkg (concat pkg "." name) name)))

(defun ygg-compose-preview--common-prefix (strings)
  "The longest common prefix of STRINGS."
  (let ((completion-ignore-case nil)
        (prefix (try-completion "" strings)))
    (if (stringp prefix) prefix (car strings))))

(defun ygg-compose-preview--selector (ids fallback)
  "CLI selector: --id for one of IDS, a --filter for several, else FALLBACK."
  (cond ((null ids) (list "--filter" fallback))
        ((null (cdr ids)) (list "--id" (car ids)))
        (t (list "--filter" (ygg-compose-preview--common-prefix ids)))))

(defun ygg-compose-preview--show-args (gradle-path selector)
  "Arguments for a JSON show of GRADLE-PATH narrowed by SELECTOR."
  (append (list "show" "--json" "--module" gradle-path) selector))

(defun ygg-compose-preview--parse-json (text)
  "Parse the JSON document TEXT into plists and lists."
  (json-parse-string text :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun ygg-compose-preview--same-file-p (a b)
  (string= (file-truename a) (file-truename b)))

(defun ygg-compose-preview--preview-file (preview module-dir)
  "The absolute source of PREVIEW, from its projectDirectory or MODULE-DIR."
  (when-let* ((src (plist-get preview :sourceFile)))
    (expand-file-name src (or (plist-get preview :projectDirectory) module-dir))))

(defun ygg-compose-preview--for-file (previews file module-dir)
  "The PREVIEWS whose source is FILE."
  (seq-filter (lambda (p)
                (when-let* ((src (ygg-compose-preview--preview-file p module-dir)))
                  (ygg-compose-preview--same-file-p src file)))
              previews))

(defun ygg-compose-preview--entry (preview)
  "The display entry of a parsed PREVIEW: id, function, name, png."
  (let ((capture (car (plist-get preview :captures))))
    (list :id (plist-get preview :id)
          :function (plist-get preview :functionName)
          :name (plist-get (plist-get preview :params) :name)
          :png (or (plist-get capture :pngPath) (plist-get preview :pngPath)))))

(defun ygg-compose-preview--label (entry)
  "The dim label of ENTRY: function name, then the @Preview name when it has one."
  (let ((fn (or (plist-get entry :function) (plist-get entry :id)))
        (name (plist-get entry :name)))
    (if (and name (not (string-empty-p name))) (format "%s · %s" fn name) fn)))

(defun ygg-compose-preview--manifest-file (module-dir)
  (expand-file-name "build/compose-previews/previews.json" module-dir))

(defun ygg-compose-preview--manifest (module-dir)
  "The discovered previews of MODULE-DIR from its last Gradle run, or nil."
  (let ((file (ygg-compose-preview--manifest-file module-dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (plist-get (ygg-compose-preview--parse-json (buffer-string)) :previews)))))

(defun ygg-compose-preview--candidates (entries)
  "Completion candidates for ENTRIES as (label . id), labels made unique."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar (lambda (e)
              (let* ((label (ygg-compose-preview--label e))
                     (n (cl-incf (gethash label seen 0))))
                (cons (if (> n 1) (format "%s <%d>" label n) label) (plist-get e :id))))
            entries)))

(defun ygg-compose-preview--a11y-line (findings)
  "One line summarising accessibility FINDINGS, the ATF check and its level."
  (if (seq-empty-p findings)
      "a11y · no findings"
    (format "a11y · %d · %s" (length findings)
            (mapconcat (lambda (f)
                         (string-join
                          (delq nil (list (or (plist-get f :type) (plist-get f :checkId))
                                          (when-let* ((l (plist-get f :label))) (format "“%s”" l))
                                          (when-let* ((s (or (plist-get f :level) (plist-get f :severity))))
                                            (downcase s))))
                          " "))
                       findings "; "))))

(defun ygg-compose-preview--a11y-of (data-products)
  "The findings list in DATA-PRODUCTS, or :none when a11y/atf is absent."
  (if-let* ((atf (seq-find (lambda (d) (equal (plist-get d :kind) "a11y/atf")) data-products)))
      (append (plist-get (plist-get atf :payload) :findings) nil)
    :none))

(defun ygg-compose-preview--daemon-command (descriptor argfile)
  "The java command launching the daemon of DESCRIPTOR, its classpath in ARGFILE."
  (append (list (or (plist-get descriptor :javaLauncher) "java"))
          (plist-get descriptor :jvmArgs)
          (cl-loop for (k v) on (plist-get descriptor :systemProperties) by #'cddr
                   when v collect (format "-D%s=%s" (substring (symbol-name k) 1) v))
          (list (concat "@" argfile) (plist-get descriptor :mainClass))))

(defun ygg-compose-preview--argfile-text (classpath)
  "Java argfile text putting CLASSPATH on -cp."
  (format "-cp\n\"%s\"\n" (string-join classpath path-separator)))

;;; Processes

(defun ygg-compose-preview--mise ()
  (and ygg-compose-preview-mise-exec (executable-find "mise")))

(defun ygg-compose-preview--program ()
  "The CLI as a command word; mise exec resolves it when mise is in use."
  (cond ((ygg-compose-preview--mise) ygg-compose-preview-program)
        ((executable-find ygg-compose-preview-program))
        (t (user-error "compose-preview: CLI not found; install it with mise"))))

(defun ygg-compose-preview--wrap (command)
  "COMMAND run under mise exec, so the CLI is found, on the buffer's JDK.
mise exec would otherwise set its own JAVA_HOME over one direnv gave."
  (if (ygg-compose-preview--mise)
      (append '("mise" "exec" "--")
              (when-let* ((home (ygg-jdk-home))) (list "env" (concat "JAVA_HOME=" home)))
              command)
    command))

(defun ygg-compose-preview--quiet (fmt &rest args)
  (let ((message-log-max nil))
    (apply #'message (concat "compose preview: " fmt) args)))

(defun ygg-compose-preview--run (session name command callback)
  "Run COMMAND for SESSION at its root; call CALLBACK with exit code and stdout."
  (let* ((source (ygg-compose-preview--session-source session))
         (out (generate-new-buffer (format " *compose-preview %s*" name)))
         (err (get-buffer-create (format " *compose-preview %s stderr*" name))))
    (with-current-buffer err (let ((inhibit-read-only t)) (erase-buffer)))
    (with-current-buffer (if (buffer-live-p source) source (current-buffer))
      (let ((default-directory (ygg-compose-preview--session-root session)))
        (make-process
         :name (format "compose-preview-%s" name)
         :buffer out :stderr err :command (ygg-compose-preview--wrap command) :noquery t
         :connection-type 'pipe
         :sentinel (lambda (proc _event)
                     (unless (process-live-p proc)
                       (let ((text (with-current-buffer out (buffer-string))))
                         (kill-buffer out)
                         (funcall callback (process-exit-status proc) text)))))))))

(defun ygg-compose-preview--gradle-command (session &rest args)
  (let ((wrapper (expand-file-name "gradlew" (ygg-compose-preview--session-root session))))
    (append (list (if (file-executable-p wrapper) wrapper "gradle") "-q") args)))

;;; Sessions

(defun ygg-compose-preview--session-for (file)
  "The session of FILE's module, created on first use."
  (let* ((root (or (ygg-compose-preview--gradle-root file)
                   (user-error "compose-preview: %s is not in a Gradle project" file)))
         (module-dir (or (ygg-compose-preview--module-dir file root) root))
         (key (file-truename module-dir))
         (session (or (gethash key ygg-compose-preview--sessions)
                      (puthash key (ygg-compose-preview--session-create
                                    :root (expand-file-name root)
                                    :module-dir (expand-file-name module-dir)
                                    :gradle-path (ygg-compose-preview--gradle-path module-dir root))
                               ygg-compose-preview--sessions))))
    (unless (and (ygg-compose-preview--session-file session)
                 (ygg-compose-preview--same-file-p (ygg-compose-preview--session-file session) file))
      (setf (ygg-compose-preview--session-ids session) nil
            (ygg-compose-preview--session-previews session) nil))
    (setf (ygg-compose-preview--session-file session) file
          (ygg-compose-preview--session-source session) (current-buffer))
    session))

(defun ygg-compose-preview--file-ids (session)
  "The ids of the previews declared in SESSION's file, from the module manifest."
  (mapcar (lambda (p) (plist-get p :id))
          (ygg-compose-preview--for-file
           (ygg-compose-preview--manifest (ygg-compose-preview--session-module-dir session))
           (ygg-compose-preview--session-file session)
           (ygg-compose-preview--session-module-dir session))))

(defun ygg-compose-preview--fallback-filter (session)
  (let ((source (ygg-compose-preview--session-source session)))
    (with-current-buffer source
      (let ((pkg (ygg-compose-preview--package (buffer-string))))
        (if pkg (concat pkg ".")
          (ygg-compose-preview--file-class (buffer-string) (ygg-compose-preview--session-file session)))))))

(defun ygg-compose-preview--daemon-ready-p (session)
  (and (eq (ygg-compose-preview--session-daemon session) 'ready)
       (ygg-compose-preview--session-conn session)
       (jsonrpc-running-p (ygg-compose-preview--session-conn session))))

(defmacro ygg-compose-preview--guard (session &rest body)
  "Run BODY; an error in it ends SESSION's render instead of leaving it busy."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (setf (ygg-compose-preview--session-error ,session) (error-message-string err)
            (ygg-compose-preview--session-waiting ,session) nil)
      (ygg-compose-preview--done ,session "error"))))

(defun ygg-compose-preview--refresh (session)
  "Render SESSION's previews, queueing one more run when a render is in flight."
  (if (ygg-compose-preview--session-busy session)
      (setf (ygg-compose-preview--session-pending session) t)
    (setf (ygg-compose-preview--session-busy session) t
          (ygg-compose-preview--session-pending session) nil
          (ygg-compose-preview--session-error session) nil
          (ygg-compose-preview--session-started session) (float-time))
    (ygg-compose-preview--guard session
      (if (ygg-compose-preview--daemon-ready-p session)
          (ygg-compose-preview--daemon-refresh session)
        (ygg-compose-preview--cli-refresh session)))))

(defun ygg-compose-preview--done (session lane)
  "Finish a render of SESSION through LANE: redraw, report, run what queued."
  (when (timerp (ygg-compose-preview--session-watchdog session))
    (cancel-timer (ygg-compose-preview--session-watchdog session)))
  (setf (ygg-compose-preview--session-busy session) nil
        (ygg-compose-preview--session-watchdog session) nil)
  (ignore-errors (ygg-compose-preview--draw session))
  (let ((took (- (float-time) (or (ygg-compose-preview--session-started session) (float-time))))
        (err (ygg-compose-preview--session-error session)))
    (if err
        (ygg-compose-preview--quiet "%s" err)
      (ygg-compose-preview--quiet "%d rendered in %.1fs (%s)"
                                  (length (ygg-compose-preview--session-previews session)) took lane)))
  (cond ((not (ygg-compose-preview--shown-p session))
         (setf (ygg-compose-preview--session-pending session) nil))
        ((ygg-compose-preview--session-pending session)
         (ygg-compose-preview--refresh session))
        ((and ygg-compose-preview-use-daemon
              (null (ygg-compose-preview--session-daemon session)))
         (ygg-compose-preview--daemon-start session))))

;;; CLI lane

(defun ygg-compose-preview--cli-refresh (session)
  (let* ((ids (or (ygg-compose-preview--session-ids session)
                  (ygg-compose-preview--file-ids session)))
         (args (ygg-compose-preview--show-args
                (ygg-compose-preview--session-gradle-path session)
                (ygg-compose-preview--selector ids (ygg-compose-preview--fallback-filter session)))))
    (ygg-compose-preview--quiet "rendering…")
    (ygg-compose-preview--run
     session "render" (cons (ygg-compose-preview--program) args)
     (lambda (code out)
       (ygg-compose-preview--guard session
         (ygg-compose-preview--cli-finished session code out)
         (ygg-compose-preview--done session "cli"))))))

(defun ygg-compose-preview--cli-finished (session code out)
  "Take the CLI's exit CODE and JSON OUT into SESSION."
  (let* ((parsed (and (string-prefix-p "{" (string-trim-left out))
                      (ignore-errors (ygg-compose-preview--parse-json out))))
         (mine (ygg-compose-preview--for-file
                (plist-get parsed :previews)
                (ygg-compose-preview--session-file session)
                (ygg-compose-preview--session-module-dir session)))
         (wanted (ygg-compose-preview--session-ids session))
         (mine (if wanted
                   (seq-filter (lambda (p) (member (plist-get p :id) wanted)) mine)
                 mine)))
    (cond (mine
           (setf (ygg-compose-preview--session-previews session)
                 (mapcar (lambda (p)
                           (let ((old (ygg-compose-preview--find session (plist-get p :id))))
                             (append (ygg-compose-preview--entry p)
                                     (list :a11y (plist-get old :a11y)))))
                         mine)))
          ((eq code 3)
           (setf (ygg-compose-preview--session-error session) "no @Preview in this file"))
          (t
           (setf (ygg-compose-preview--session-error session)
                 (format "render failed (exit %s); see buffer \" *compose-preview render stderr*\""
                         code))))
    (when (and mine (eq code 2))
      (setf (ygg-compose-preview--session-error session) "some previews failed to render"))))

(defun ygg-compose-preview--find (session id)
  (seq-find (lambda (e) (equal (plist-get e :id) id))
            (ygg-compose-preview--session-previews session)))

;;; Daemon lane

(defun ygg-compose-preview--daemon-start (session)
  "Bring up the warm daemon of SESSION's module in the background."
  (let ((launch (list 'launch)))
    (setf (ygg-compose-preview--session-daemon session) 'starting
          (ygg-compose-preview--session-launch session) launch)
    (ygg-compose-preview--run
     session "init-script" (list (ygg-compose-preview--program) "init-script" "--path")
     (lambda (code script)
       (when (eq launch (ygg-compose-preview--session-launch session))
         (if (/= code 0)
             (setf (ygg-compose-preview--session-daemon session) 'failed)
           (setf (ygg-compose-preview--session-init-script session) (string-trim script))
           (ygg-compose-preview--run
            session "daemon-start"
            (ygg-compose-preview--gradle-command
             session "--init-script" (ygg-compose-preview--session-init-script session)
             (ygg-compose-preview--task session "composePreviewDaemonStart"))
            (lambda (code _out)
              (when (eq launch (ygg-compose-preview--session-launch session))
                (if (or (/= code 0)
                        (not (ignore-errors (ygg-compose-preview--daemon-spawn session) t)))
                    (setf (ygg-compose-preview--session-daemon session) 'failed)))))))))))

(defun ygg-compose-preview--task (session task)
  "The Gradle path of TASK in SESSION's module."
  (concat (string-remove-suffix ":" (ygg-compose-preview--session-gradle-path session)) ":" task))

(defun ygg-compose-preview--daemon-spawn (session)
  (let* ((module-dir (ygg-compose-preview--session-module-dir session))
         (file (expand-file-name "build/compose-previews/daemon-launch.json" module-dir))
         (descriptor (with-temp-buffer
                       (insert-file-contents file)
                       (json-parse-buffer :object-type 'plist :array-type 'list
                                          :null-object nil :false-object nil))))
    (if (not (plist-get descriptor :enabled))
        (setf (ygg-compose-preview--session-daemon session) 'failed)
      (let* ((argfile (make-temp-file "compose-preview-cp" nil ".args"
                                      (ygg-compose-preview--argfile-text
                                       (plist-get descriptor :classpath))))
             (default-directory (or (plist-get descriptor :workingDirectory) module-dir))
             (proc (make-process
                    :name "compose-preview-daemon"
                    :command (ygg-compose-preview--daemon-command descriptor argfile)
                    :connection-type 'pipe :coding 'utf-8-emacs-unix :noquery t
                    :stderr (get-buffer-create " *compose-preview daemon stderr*")))
             (conn (jsonrpc-process-connection
                    :name "compose-preview-daemon"
                    :process proc
                    :events-buffer-config '(:size 0)
                    :notification-dispatcher
                    (lambda (_conn method params)
                      (ygg-compose-preview--daemon-notified session method params))
                    :request-dispatcher (lambda (&rest _) nil)
                    :on-shutdown
                    (lambda (conn)
                      (ignore-errors (delete-file argfile))
                      (ygg-compose-preview--daemon-gone session conn)))))
        (setf (ygg-compose-preview--session-conn session) conn)
        (jsonrpc-async-request
         conn :initialize
         (list :protocolVersion ygg-compose-preview--protocol-version
               :clientVersion "ygg-compose-preview"
               :workspaceRoot (directory-file-name (ygg-compose-preview--session-root session))
               :moduleId (ygg-compose-preview--session-gradle-path session)
               :moduleProjectDir (directory-file-name module-dir)
               :capabilities (list :visibility t :metrics :json-false))
         :timeout 180
         :success-fn (lambda (_)
                       (jsonrpc-notify conn :initialized :jsonrpc-omit)
                       (if ygg-compose-preview-a11y
                           (jsonrpc-async-request
                            conn :extensions/enable (list :ids ["a11y"])
                            :success-fn (lambda (_) (ygg-compose-preview--daemon-ready session))
                            :error-fn (lambda (_) (ygg-compose-preview--daemon-ready session)))
                         (ygg-compose-preview--daemon-ready session)))
         :error-fn (lambda (_) (jsonrpc-shutdown conn))
         :timeout-fn (lambda () (jsonrpc-shutdown conn)))))))

(defun ygg-compose-preview--daemon-gone (session conn)
  "CONN of SESSION shut down on its own; reopening the preview restarts it."
  (when (and (eq conn (ygg-compose-preview--session-conn session))
             (not (eq (ygg-compose-preview--session-daemon session) 'stopping)))
    (let ((crashed (eq (ygg-compose-preview--session-daemon session) 'ready)))
      (setf (ygg-compose-preview--session-conn session) nil
            (ygg-compose-preview--session-subscribed session) nil
            (ygg-compose-preview--session-daemon session) (if crashed 'crashed 'failed))
      (ignore-errors (ygg-compose-preview--draw session))
      (when (ygg-compose-preview--session-waiting session)
        (setf (ygg-compose-preview--session-waiting session) nil
              (ygg-compose-preview--session-error session)
              (if crashed "warm daemon exited; rendering through the CLI" "daemon exited"))
        (ygg-compose-preview--done session "daemon")))))

(defun ygg-compose-preview--daemon-retire (session state)
  "Forget SESSION's daemon and a render waiting on it; its slot is left at STATE."
  (when (timerp (ygg-compose-preview--session-watchdog session))
    (cancel-timer (ygg-compose-preview--session-watchdog session)))
  (when (ygg-compose-preview--session-waiting session)
    (setf (ygg-compose-preview--session-busy session) nil
          (ygg-compose-preview--session-pending session) nil))
  (setf (ygg-compose-preview--session-conn session) nil
        (ygg-compose-preview--session-subscribed session) nil
        (ygg-compose-preview--session-launch session) nil
        (ygg-compose-preview--session-waiting session) nil
        (ygg-compose-preview--session-watchdog session) nil
        (ygg-compose-preview--session-daemon session) state))

(defun ygg-compose-preview--daemon-ready (session)
  (setf (ygg-compose-preview--session-daemon session) 'ready)
  (ygg-compose-preview--quiet "warm daemon ready")
  (when ygg-compose-preview-a11y
    (ygg-compose-preview--refresh session)))

(defun ygg-compose-preview--daemon-failed (session conn)
  "Retire CONN of SESSION for good and render through the CLI instead."
  (when (and (eq conn (ygg-compose-preview--session-conn session))
             (not (eq (ygg-compose-preview--session-daemon session) 'stopping)))
    (setf (ygg-compose-preview--session-waiting session) nil
          (ygg-compose-preview--session-daemon session) 'failed
          (ygg-compose-preview--session-conn session) nil)
    (ignore-errors (jsonrpc-shutdown conn t))
    (ygg-compose-preview--guard session (ygg-compose-preview--cli-refresh session))))

(defun ygg-compose-preview--render-ids (session)
  (or (ygg-compose-preview--session-ids session)
      (ygg-compose-preview--file-ids session)
      (mapcar (lambda (e) (plist-get e :id)) (ygg-compose-preview--session-previews session))))

(defun ygg-compose-preview--daemon-refresh (session)
  "Recompile SESSION's module with Gradle, then re-render through the daemon."
  (ygg-compose-preview--quiet "compiling…")
  (ygg-compose-preview--run
   session "compile"
   (ygg-compose-preview--gradle-command
    session "--init-script" (ygg-compose-preview--session-init-script session)
    (ygg-compose-preview--task session "composePreviewCompile"))
   (lambda (code _out)
     (ygg-compose-preview--guard session
       (cond ((/= code 0)
              (setf (ygg-compose-preview--session-error session)
                    "compile failed; see buffer \" *compose-preview compile stderr*\"")
              (ygg-compose-preview--done session "daemon"))
             ((ygg-compose-preview--daemon-ready-p session)
              (ygg-compose-preview--daemon-render session))
             (t (ygg-compose-preview--cli-refresh session)))))))

(defconst ygg-compose-preview--render-timeout 120)

(defun ygg-compose-preview--daemon-render (session)
  (let ((conn (ygg-compose-preview--session-conn session))
        (ids (ygg-compose-preview--render-ids session)))
    (if (null ids)
        (ygg-compose-preview--cli-refresh session)
      (setf (ygg-compose-preview--session-previews session)
            (mapcar (lambda (id) (or (ygg-compose-preview--find session id) (list :id id))) ids))
      (jsonrpc-notify conn :fileChanged
                      (list :path (ygg-compose-preview--session-file session)
                            :kind "source" :changeType "modified"))
      (jsonrpc-notify conn :setVisible (list :ids (vconcat ids)))
      (when ygg-compose-preview-a11y
        (dolist (id ids)
          (unless (member id (ygg-compose-preview--session-subscribed session))
            (push id (ygg-compose-preview--session-subscribed session))
            (jsonrpc-async-request conn :data/subscribe (list :previewId id :kind "a11y/atf")
                                   :success-fn #'ignore :error-fn #'ignore))))
      (setf (ygg-compose-preview--session-waiting session) (copy-sequence ids)
            (ygg-compose-preview--session-watchdog session)
            (run-at-time ygg-compose-preview--render-timeout nil
                         (lambda ()
                           (when (ygg-compose-preview--session-waiting session)
                             (ygg-compose-preview--daemon-failed session conn)))))
      (ygg-compose-preview--quiet "rendering…")
      (jsonrpc-async-request
       conn :renderNow (list :previews (vconcat ids) :tier "full" :reason "save")
       :success-fn
       (lambda (result)
         (let ((rejected (mapcar (lambda (r) (plist-get r :id)) (plist-get result :rejected))))
           (when rejected
             (setf (ygg-compose-preview--session-waiting session)
                   (seq-difference (ygg-compose-preview--session-waiting session) rejected))
             (unless (ygg-compose-preview--session-waiting session)
               (ygg-compose-preview--daemon-failed session conn)))))
       :error-fn (lambda (_) (ygg-compose-preview--daemon-failed session conn))))))

(defun ygg-compose-preview--daemon-notified (session method params)
  "Take the daemon notification METHOD with PARAMS into SESSION."
  (pcase method
    ((or 'renderFinished 'renderFailed)
     (let* ((id (plist-get params :id))
            (entry (ygg-compose-preview--find session id)))
       (when entry
         (if (eq method 'renderFinished)
             (let ((a11y (ygg-compose-preview--a11y-of (plist-get params :dataProducts))))
               (plist-put entry :png (plist-get params :pngPath))
               (plist-put entry :failed nil)
               (unless (eq a11y :none) (plist-put entry :a11y (or a11y 'clean))))
           (plist-put entry :failed (plist-get (plist-get params :error) :message))))
       (when (member id (ygg-compose-preview--session-waiting session))
         (setf (ygg-compose-preview--session-waiting session)
               (delete id (ygg-compose-preview--session-waiting session)))
         (unless (ygg-compose-preview--session-waiting session)
           (ygg-compose-preview--done session "warm daemon")))))))

(defun ygg-compose-preview--stop-daemons (&optional state)
  "Shut down or cancel every warm daemon, leaving each daemon slot at STATE."
  (maphash (lambda (_ session)
             (let ((conn (ygg-compose-preview--session-conn session)))
               (setf (ygg-compose-preview--session-daemon session) 'stopping)
               (when (and (jsonrpc-connection-p conn) (jsonrpc-running-p conn))
                 (ignore-errors (jsonrpc-notify conn :exit :jsonrpc-omit))
                 (ignore-errors (jsonrpc-shutdown conn t)))
               (ygg-compose-preview--daemon-retire session state)))
           ygg-compose-preview--sessions))

;;; Drawing

(define-derived-mode ygg-compose-preview-mode special-mode "Compose preview"
  "Rendered @Preview images of one Kotlin file."
  (setq-local mode-line-format nil
              header-line-format nil
              cursor-type nil
              truncate-lines t)
  (add-hook 'window-size-change-functions #'ygg-compose-preview--resized nil t)
  (add-hook 'kill-buffer-hook #'ygg-compose-preview--stop-daemons nil t))

(defvar ygg-modal-special-modes)
(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-compose-preview-mode))

(defun ygg-compose-preview--resized (window)
  (with-current-buffer (window-buffer window)
    (when ygg-compose-preview--shown
      (ygg-compose-preview--draw ygg-compose-preview--shown))))

(defun ygg-compose-preview--image (png width)
  "An image of PNG no wider than WIDTH pixels, read fresh from disk."
  (when (and png (file-readable-p png))
    (create-image (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally png)
                    (buffer-string))
                  'png t :max-width width)))

(defun ygg-compose-preview--draw (session)
  "Draw SESSION's previews into the preview buffer, sized to its window."
  (when-let* ((buf (get-buffer ygg-compose-preview--buffer)))
    (with-current-buffer buf
      (when (eq ygg-compose-preview--shown session)
        (let* ((win (get-buffer-window buf t))
               (width (max 64 (- (if win (window-body-width win t) 480)
                                 (* 2 (frame-char-width)))))
               (inhibit-read-only t))
          (erase-buffer)
          (insert "\n")
          (when (eq (ygg-compose-preview--session-daemon session) 'crashed)
            (insert " " (propertize
                         "warm daemon exited · rendering through the CLI until the preview is reopened"
                         'face '(:inherit shadow :height 0.85 :slant italic))
                    "\n\n"))
          (dolist (entry (ygg-compose-preview--session-previews session))
            (insert " ")
            (if-let* ((img (ygg-compose-preview--image (plist-get entry :png) width)))
                (insert-image img (ygg-compose-preview--label entry))
              (insert (propertize "no image" 'face 'shadow)))
            (insert "\n " (propertize (ygg-compose-preview--label entry)
                                      'face '(:inherit shadow :height 0.85))
                    "\n")
            (when-let* ((failed (plist-get entry :failed)))
              (insert " " (propertize (concat "failed · " (car (split-string failed "\n")))
                                      'face '(:inherit shadow :height 0.85 :slant italic))
                      "\n"))
            (when-let* ((a11y (plist-get entry :a11y)))
              (insert " " (propertize (ygg-compose-preview--a11y-line (unless (eq a11y 'clean) a11y))
                                      'face '(:inherit shadow :height 0.85))
                      "\n"))
            (insert "\n"))
          (goto-char (point-min)))))))

(defun ygg-compose-preview--show (session)
  "Show the preview buffer for SESSION in the right side window."
  (let ((buf (get-buffer-create ygg-compose-preview--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ygg-compose-preview-mode) (ygg-compose-preview-mode))
      (setq ygg-compose-preview--shown session))
    (display-buffer buf `(display-buffer-in-side-window
                          (side . right) (slot . 0)
                          (window-width . ,ygg-compose-preview-window-width)
                          (preserve-size . (t . nil))))
    (ygg-compose-preview--draw session)))

(defun ygg-compose-preview--shown-p (session)
  (when-let* ((buf (get-buffer ygg-compose-preview--buffer)))
    (eq (buffer-local-value 'ygg-compose-preview--shown buf) session)))

(defun ygg-compose-preview--visible-p (session)
  (when-let* ((buf (get-buffer ygg-compose-preview--buffer)))
    (and (get-buffer-window buf t)
         (eq (buffer-local-value 'ygg-compose-preview--shown buf) session))))

(defun ygg-compose-preview--after-save ()
  (when (and ygg-compose-preview-on-save buffer-file-name)
    (when-let* ((root (ygg-compose-preview--gradle-root buffer-file-name))
                (module-dir (or (ygg-compose-preview--module-dir buffer-file-name root) root))
                (session (gethash (file-truename module-dir) ygg-compose-preview--sessions)))
      (when (ygg-compose-preview--visible-p session)
        (ygg-compose-preview--refresh session)))))

;;; Commands

(defun ygg-compose-preview--start (ids)
  (let* ((file (or buffer-file-name (user-error "Buffer visits no file")))
         (session (ygg-compose-preview--session-for file)))
    (setf (ygg-compose-preview--session-ids session) ids)
    (when (memq (ygg-compose-preview--session-daemon session) '(failed stopped crashed))
      (setf (ygg-compose-preview--session-daemon session) nil))
    (when ids
      (setf (ygg-compose-preview--session-previews session)
            (seq-filter (lambda (e) (member (plist-get e :id) ids))
                        (ygg-compose-preview--session-previews session))))
    (add-hook 'after-save-hook #'ygg-compose-preview--after-save nil t)
    (ygg-compose-preview--show session)
    (ygg-compose-preview--refresh session)))

(defun ygg-compose-preview ()
  "Render the @Previews of this Kotlin file into the side window."
  (interactive)
  (ygg-compose-preview--start nil))

(defun ygg-compose-preview-pick ()
  "Render one @Preview of this Kotlin file, picked by name."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Buffer visits no file")))
         (session (ygg-compose-preview--session-for file))
         (previews (ygg-compose-preview--for-file
                    (ygg-compose-preview--manifest (ygg-compose-preview--session-module-dir session))
                    file (ygg-compose-preview--session-module-dir session))))
    (if (null previews)
        (ygg-compose-preview--list-then-pick session)
      (ygg-compose-preview--pick-from session previews))))

(defun ygg-compose-preview--pick-from (session previews)
  (let* ((candidates (ygg-compose-preview--candidates
                      (mapcar #'ygg-compose-preview--entry previews)))
         (choice (if (cdr candidates)
                     (completing-read "Preview: " candidates nil t)
                   (caar candidates))))
    (with-current-buffer (ygg-compose-preview--session-source session)
      (ygg-compose-preview--start (list (cdr (assoc choice candidates)))))))

(defun ygg-compose-preview--list-then-pick (session)
  (ygg-compose-preview--quiet "discovering previews…")
  (ygg-compose-preview--run
   session "list"
   (list (ygg-compose-preview--program) "list" "--json"
         "--module" (ygg-compose-preview--session-gradle-path session))
   (lambda (_code out)
     (let ((previews (ygg-compose-preview--for-file
                      (plist-get (ignore-errors (ygg-compose-preview--parse-json out)) :previews)
                      (ygg-compose-preview--session-file session)
                      (ygg-compose-preview--session-module-dir session))))
       (if previews
           (ygg-compose-preview--pick-from session previews)
         (ygg-compose-preview--quiet "no @Preview in this file"))))))

(defun ygg-compose-preview-rerender ()
  "Render the shown previews again."
  (interactive)
  (if-let* ((session (buffer-local-value 'ygg-compose-preview--shown
                                         (or (get-buffer ygg-compose-preview--buffer)
                                             (user-error "No preview shown")))))
      (ygg-compose-preview--refresh session)
    (user-error "No preview shown")))

(defun ygg-compose-preview-toggle-on-save ()
  "Toggle re-rendering on save while the preview window shows."
  (interactive)
  (setq ygg-compose-preview-on-save (not ygg-compose-preview-on-save))
  (message "compose preview on save %s" (if ygg-compose-preview-on-save "on" "off")))

(defun ygg-compose-preview-stop ()
  "Shut down the warm preview daemons."
  (interactive)
  (ygg-compose-preview--stop-daemons 'stopped))

(when (fboundp 'yggdrasil-localleader-def)
  (dolist (mode '(kotlin-ts-mode kotlin-mode))
    (yggdrasil-localleader-def mode "P" #'ygg-compose-preview-pick "compose preview pick"))
  (yggdrasil-localleader-def 'ygg-compose-preview-mode "r" #'ygg-compose-preview-rerender "render again")
  (yggdrasil-localleader-def 'ygg-compose-preview-mode "t" #'ygg-compose-preview-toggle-on-save "render on save")
  (yggdrasil-localleader-def 'ygg-compose-preview-mode "k" #'ygg-compose-preview-stop "stop warm daemon"))

(provide 'ygg-compose-preview)
;;; ygg-compose-preview.el ends here
