;;; ygg-dap-java.el --- Java tests and Android attach on dape's jdtls entry -*- lexical-binding: t; -*-

;; An adb forward and a test result socket live exactly as long as their session.

;;; Code:

(require 'seq)
(require 'map)
(require 'cl-lib)
(require 'subr-x)
(require 'jsonrpc)

(defvar dape-configs)
(defvar dape-history)
(declare-function project-root "project")
(declare-function dape "dape")
(declare-function dape-config-get "dape")
(declare-function dape--config "dape")
(declare-function dape--config-eval "dape")
(declare-function dape--live-connections-root "dape")
(declare-function dape--kill-busy-wait "dape")
(declare-function eglot-current-server "eglot")
(declare-function eglot-execute-command "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-path-to-uri "eglot")
(declare-function ygg-lsp--newest-version-dir "layer-lsp")

(defgroup ygg-dap-java nil
  "Java test debugging and Android attach through dape and jdtls."
  :group 'tools
  :prefix "ygg-dap-java-")

(defcustom ygg-dap-java-adb-program
  (or (executable-find "adb") (expand-file-name "~/Library/Android/sdk/platform-tools/adb"))
  "The adb that forwards an app's JDWP to a local port."
  :type 'file)

(defvar ygg-dap-java-adb-function #'ygg-dap-java--adb
  "Called with adb's arguments; returns what it printed.")

(defvar ygg-dap-java-adb-processes-function #'ygg-dap-java--adb-processes
  "Called with a device serial; returns its debuggable processes as (NAME . PID).")

(defconst ygg-dap-java--results-buffer "*java test results*")

(defconst ygg-dap-java--testng-launcher "com.microsoft.java.test.runner.Launcher")

;;; Resources that end with the session

(defvar ygg-dap-java--unclaimed nil
  "Releases whose session never made a connection to end them.")

(defun ygg-dap-java--own (config release)
  "CONFIG carrying RELEASE, run once when the session's connection shuts down."
  (let* ((done nil)
         (once (lambda (&rest _)
                 (unless done
                   (setq done t)
                   (funcall release)))))
    (push once ygg-dap-java--unclaimed)
    (plist-put config 'ygg-dap-java-release once)))

(defun ygg-dap-java--release-unclaimed ()
  (mapc #'funcall ygg-dap-java--unclaimed)
  (setq ygg-dap-java--unclaimed nil))

(defun ygg-dap-java--claim (conn)
  "Tie the release in CONN's config to CONN's shutdown."
  (when-let* ((release (plist-get (dape--config conn) 'ygg-dap-java-release)))
    (setq ygg-dap-java--unclaimed (delq release ygg-dap-java--unclaimed))
    (add-function :after (jsonrpc--on-shutdown conn) release)))

(defun ygg-dap-java--server ()
  (or (eglot-current-server)
      (user-error "No jdtls in %s; start eglot first" (buffer-name))))

(defun ygg-dap-java--file ()
  (or buffer-file-name (user-error "Visit a Java file first")))

(defun ygg-dap-java--start-session (server)
  "Port of a fresh java-debug session in SERVER."
  (eglot-execute-command server "vscode.java.startDebugSession" nil))

;;; Tests at point

(defun ygg-dap-java--number (item key)
  "ITEM's KEY as a number; the plugin sends its enums as numeric strings."
  (let ((value (plist-get item key)))
    (if (stringp value) (string-to-number value) value)))

(defun ygg-dap-java-test-pick (items line)
  "The innermost test in ITEMS whose range spans zero-based LINE."
  (seq-some (lambda (item)
              (let ((range (plist-get item :range)))
                (when (and range
                           (<= (map-nested-elt range '(:start :line))
                               line
                               (map-nested-elt range '(:end :line))))
                  (or (ygg-dap-java-test-pick (plist-get item :children) line)
                      item))))
            items))

(defun ygg-dap-java--declaration-line ()
  "Zero-based line of the declaration at point, below any annotation-only lines."
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at-p "[ \t]*@[[:alnum:]_.]+\\(([^)]*)\\)?[ \t]*$")
                (zerop (forward-line 1))))
    (1- (line-number-at-pos))))

(defun ygg-dap-java-test-at-point-item ()
  "The test class or method at point, as the test plugin describes it."
  (let* ((server (ygg-dap-java--server))
         (items (eglot-execute-command server "vscode.java.test.findTestTypesAndMethods"
                                       (vector (eglot-path-to-uri (ygg-dap-java--file))))))
    (or (ygg-dap-java-test-pick items (ygg-dap-java--declaration-line))
        (user-error "No test at point; jdtls may still be importing the project"))))

(defun ygg-dap-java-test-request (item)
  "The junit.argument request that runs ITEM."
  (let ((class (eql (ygg-dap-java--number item :testLevel) 5)))
    (decode-coding-string
     (json-serialize
      (list :projectName (plist-get item :projectName)
            :testLevel (ygg-dap-java--number item :testLevel)
            :testKind (ygg-dap-java--number item :testKind)
            :testNames (vector (plist-get item (if class :fullName :jdtHandler)))
            :testHandles []))
     'utf-8)))

(defun ygg-dap-java-test-with-port (args port)
  "The runner's ARGS reporting to PORT; the plugin's own port is a placeholder."
  (let ((at (seq-position args "-port"))
        (port (number-to-string port)))
    (if (and at (< (1+ at) (length args)))
        (append (seq-take args (1+ at)) (list port) (seq-drop args (+ at 2)))
      (append args (list "-port" port)))))

(defun ygg-dap-java-testng-names (item)
  "The TestNG methods ITEM covers, as class#method."
  (if (eql (ygg-dap-java--number item :testLevel) 6)
      (list (plist-get item :fullName))
    (seq-mapcat #'ygg-dap-java-testng-names (plist-get item :children) 'list)))

(defun ygg-dap-java--testng-runner ()
  (when-let* ((version (ygg-lsp--newest-version-dir
                        (expand-file-name "~/.local/share/mise/installs/http-java-test/*"))))
    (expand-file-name "extension/server/com.microsoft.java.test.runner-jar-with-dependencies.jar"
                      version)))

(defun ygg-dap-java--results-filter (_proc text)
  (with-current-buffer (get-buffer-create ygg-dap-java--results-buffer)
    (goto-char (point-max))
    (insert text)
    (when (string-match-p "^%RUNTIME" text)
      (message "Java tests: %d run, %d failed (%s)"
               (how-many "^%TESTS " (point-min) (point-max))
               (how-many "^%\\(FAILED\\|ERROR\\) " (point-min) (point-max))
               ygg-dap-java--results-buffer))))

(defun ygg-dap-java--results-listener ()
  "A local socket the test runner reports to, its output kept in a buffer."
  (with-current-buffer (get-buffer-create ygg-dap-java--results-buffer)
    (erase-buffer))
  (make-network-process :name "java-test-results" :server t :host "127.0.0.1"
                        :service t :noquery t :coding 'utf-8
                        :filter #'ygg-dap-java--results-filter))

(defun ygg-dap-java-test-launch (config launch results-port testng-names runner)
  "CONFIG launching the runner that LAUNCH describes, reporting to RESULTS-PORT.
TESTNG-NAMES and RUNNER are set for a TestNG run."
  (let ((vm-args (append (plist-get launch :vmArguments) nil)))
    (thread-first
      config
      (plist-put :projectName (plist-get launch :projectName))
      (plist-put :mainClass (if runner ygg-dap-java--testng-launcher (plist-get launch :mainClass)))
      (plist-put :classPaths (vconcat (plist-get launch :classpath) (and runner (list runner))))
      (plist-put :modulePaths (vconcat (plist-get launch :modulepath)))
      (plist-put :cwd (plist-get launch :workingDirectory))
      (plist-put :vmArgs (string-trim (concat (plist-get config :vmArgs) " "
                                              (combine-and-quote-strings vm-args))))
      (plist-put :args (combine-and-quote-strings
                        (if runner
                            (append (list (number-to-string results-port) "testng") testng-names)
                          (ygg-dap-java-test-with-port
                           (append (plist-get launch :programArguments) nil) results-port)))))))

(defun ygg-dap-java-test-resolve (config)
  "CONFIG launching its test under the runner, with a java-debug session port."
  (ygg-dap-java--release-unclaimed)
  (with-current-buffer (find-file-noselect (dape-config-get config :filePath))
    (let* ((server (ygg-dap-java--server))
           (item (plist-get config 'ygg-dap-java-test))
           (testng (eql (ygg-dap-java--number item :testKind) 2))
           (response (eglot-execute-command server "vscode.java.test.junit.argument"
                                            (vector (ygg-dap-java-test-request item))))
           (launch (or (plist-get response :body)
                       (user-error "Test launch not resolved: %s" (plist-get response :errorMessage))))
           (port (ygg-dap-java--start-session server))
           (results (ygg-dap-java--results-listener)))
      (thread-first
        (ygg-dap-java-test-launch config launch (process-contact results :service)
                                  (and testng (ygg-dap-java-testng-names item))
                                  (and testng (ygg-dap-java--testng-runner)))
        (plist-put 'port port)
        (ygg-dap-java--own (lambda () (delete-process results)))))))

;;; Android attach over adb

(defun ygg-dap-java--adb (&rest args)
  (with-temp-buffer
    (unless (zerop (apply #'call-process ygg-dap-java-adb-program nil t nil args))
      (user-error "adb %s: %s" (string-join args " ") (string-trim (buffer-string))))
    (buffer-string)))

(defun ygg-dap-java--adb-jdwp-pids (serial)
  "Pids adb jdwp lists on SERIAL; it never exits, so read until it goes quiet."
  (let* ((proc (start-process "adb-jdwp" nil ygg-dap-java-adb-program "-s" serial "jdwp"))
         (out ""))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc (lambda (_ text) (setq out (concat out text))))
    (unwind-protect
        (while (accept-process-output proc 0.5))
      (delete-process proc))
    (split-string out "\n" t "[ \t\r]+")))

(defun ygg-dap-java--adb-processes (serial)
  (let ((pids (ygg-dap-java--adb-jdwp-pids serial)))
    (cl-loop for line in (cdr (split-string (funcall ygg-dap-java-adb-function "-s" serial "shell"
                                                     "ps" "-A" "-o" "PID,NAME")
                                            "\n" t))
             for (pid name) = (split-string line)
             when (member pid pids) collect (cons name pid))))

(declare-function ygg-device-android-serial "ygg-device")

(defun ygg-dap-java-adb-serial ()
  "The selected Android device, else the attached one, asking among several."
  (or (and (fboundp 'ygg-device-android-serial) (ygg-device-android-serial))
      (ygg-dap-java--attached-serial)))

(defun ygg-dap-java--attached-serial ()
  (let ((serials (cl-loop for line in (split-string (funcall ygg-dap-java-adb-function "devices") "\n" t)
                          when (string-match "\\`\\(\\S-+\\)\tdevice\\'" line)
                          collect (match-string 1 line))))
    (pcase serials
      ('nil (user-error "No device attached to adb"))
      (`(,only) only)
      (_ (completing-read "Device: " serials nil t)))))

(defun ygg-dap-java-application-id (root)
  "The applicationId or manifest package under ROOT, if any."
  (seq-some (lambda (file)
              (with-temp-buffer
                (insert-file-contents file)
                (when (re-search-forward
                       "\\(?:applicationId\\s-*=?\\s-*\\|package=\\)\"\\([^\"]+\\)\"" nil t)
                  (match-string 1))))
            (seq-mapcat (lambda (glob) (file-expand-wildcards (expand-file-name glob root)))
                        '("build.gradle*" "*/build.gradle*"
                          "src/main/AndroidManifest.xml" "*/src/main/AndroidManifest.xml" "AndroidManifest.xml")
                        'list)))

(defun ygg-dap-java-adb-pick (processes app-id)
  "The process in PROCESSES to attach to, APP-ID's when it is running."
  (or (assoc app-id processes)
      (pcase processes
        ('nil (user-error "No debuggable process on the device"))
        (`(,only) only)
        (_ (assoc (completing-read "Process: " processes nil t) processes)))))

(defun ygg-dap-java--free-port ()
  (let ((probe (make-network-process :name "port-probe" :server t :host "127.0.0.1" :service t)))
    (prog1 (process-contact probe :service)
      (delete-process probe))))

(defun ygg-dap-java-adb-attach (config serial pid port session-port root)
  "CONFIG attaching over SESSION-PORT to PID on SERIAL, forwarded to local PORT."
  (let ((local (format "tcp:%d" port)))
    (funcall ygg-dap-java-adb-function "-s" serial "forward" local (concat "jdwp:" pid))
    (thread-first
      config
      (plist-put 'port session-port)
      (plist-put :hostName "localhost")
      (plist-put :port port)
      (plist-put :sourcePaths
                 (vconcat (seq-mapcat (lambda (glob) (file-expand-wildcards (expand-file-name glob root)))
                                      '("src/main/java" "*/src/main/java") 'list)))
      (ygg-dap-java--own (lambda ()
                           (ignore-errors
                             (funcall ygg-dap-java-adb-function "-s" serial "forward" "--remove" local)))))))

(defun ygg-dap-java-adb-resolve (config)
  "CONFIG attaching to a debuggable app process through an adb forward."
  (ygg-dap-java--release-unclaimed)
  (with-current-buffer (find-file-noselect (dape-config-get config :filePath))
    (let* ((server (ygg-dap-java--server))
           (root (if-let* ((project (project-current))) (project-root project) default-directory))
           (serial (ygg-dap-java-adb-serial))
           (process (ygg-dap-java-adb-pick (funcall ygg-dap-java-adb-processes-function serial)
                                           (ygg-dap-java-application-id root))))
      (ygg-dap-java-adb-attach config serial (cdr process) (ygg-dap-java--free-port)
                               (ygg-dap-java--start-session server) root))))

(defun ygg-dap-java-attach-resolve (config)
  "CONFIG attaching to the JVM at its hostName and port through a fresh session."
  (with-current-buffer (find-file-noselect (dape-config-get config :filePath))
    (plist-put config 'port (ygg-dap-java--start-session (ygg-dap-java--server)))))

;;; Entries derived from dape's jdtls

(defvar ygg-dap-java--jdtls-ensure nil
  "The ensure dape's jdtls entry shipped with.")

(defun ygg-dap-java-test-ensure (config)
  "Dape's jdtls check, and that jdtls loaded the test plugin."
  (funcall ygg-dap-java--jdtls-ensure config)
  (with-current-buffer (find-file-noselect (dape-config-get config :filePath))
    (unless (seq-contains-p (eglot-server-capable :executeCommandProvider :commands)
                            "vscode.java.test.junit.argument")
      (user-error "jdtls has no vscode-java-test; mise install http:java-test, then restart eglot"))))

(defun ygg-dap-java-adb-ensure (config)
  "Dape's jdtls check, and that adb runs."
  (funcall ygg-dap-java--jdtls-ensure config)
  (unless (file-executable-p ygg-dap-java-adb-program)
    (user-error "No adb at %s" ygg-dap-java-adb-program)))

(defun ygg-dap-java-install ()
  "Derive the test and adb attach entries from dape's jdtls entry."
  (when-let* ((jdtls (alist-get 'jdtls dape-configs)))
    (setq ygg-dap-java--jdtls-ensure (plist-get jdtls 'ensure))
    (let ((base (map-delete (copy-sequence jdtls) 'fn)))
      (setf (alist-get 'jdtls-test dape-configs)
            (map-merge 'plist
                       (cl-reduce (lambda (plist key) (map-delete plist key))
                                  '(:mainClass :projectName :args)
                                  :initial-value (copy-sequence base))
                       '(fn ygg-dap-java-test-resolve ensure ygg-dap-java-test-ensure
                         :filePath ygg-dap-java--file
                         ygg-dap-java-test ygg-dap-java-test-at-point-item)))
      (setf (alist-get 'jdtls-adb dape-configs)
            (map-merge 'plist
                       (cl-reduce (lambda (plist key) (map-delete plist key))
                                  '(:mainClass :projectName :args :vmArgs :console :stopOnEntry)
                                  :initial-value (copy-sequence base))
                       '(fn ygg-dap-java-adb-resolve ensure ygg-dap-java-adb-ensure
                         :filePath ygg-dap-java--file :request "attach")))
      (setf (alist-get 'jdtls-attach dape-configs)
            (map-merge 'plist
                       (cl-reduce (lambda (plist key) (map-delete plist key))
                                  '(:mainClass :projectName :args :vmArgs :console :stopOnEntry)
                                  :initial-value (copy-sequence base))
                       '(fn ygg-dap-java-attach-resolve
                         :filePath ygg-dap-java--file :request "attach"
                         :hostName "localhost" :port 5005))))))

(with-eval-after-load 'dape
  (cl-defmethod initialize-instance :after ((conn dape-connection) &rest _)
    (ygg-dap-java--claim conn))
  (ygg-dap-java-install))

(provide 'ygg-dap-java)
;;; ygg-dap-java.el ends here
