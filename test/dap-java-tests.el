;;; dap-java-tests.el --- Tests for Java test and adb attach configs -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dape)
(require 'ygg-dap-java)

(defconst ygg-dap-java-tests--items
  [(:label "CalcTest" :fullName "demo.CalcTest" :testLevel "5" :testKind "0" :projectName "demo"
    :jdtHandler "=demo/src\\/test\\/java<demo{CalcTest.java[CalcTest"
    :range (:start (:line 4 :character 13) :end (:line 20 :character 1))
    :children [(:label "adds()" :fullName "demo.CalcTest#adds" :testLevel "6" :testKind "0"
                :projectName "demo" :jdtHandler "=demo/src\\/test\\/java<demo{CalcTest.java[CalcTest~adds"
                :range (:start (:line 7 :character 9) :end (:line 10 :character 5)))
               (:label "subtracts()" :fullName "demo.CalcTest#subtracts" :testLevel "6" :testKind "0"
                :projectName "demo" :jdtHandler "=demo/src\\/test\\/java<demo{CalcTest.java[CalcTest~subtracts"
                :range (:start (:line 13 :character 9) :end (:line 16 :character 5)))])]
  "What findTestTypesAndMethods answers for a class with two tests.")

(defun ygg-dap-java-tests--method (name)
  (seq-find (lambda (item) (equal (plist-get item :label) name))
            (plist-get (aref ygg-dap-java-tests--items 0) :children)))

(ert-deftest ygg-dap-java-test-pick-finds-innermost ()
  (should (equal (plist-get (ygg-dap-java-test-pick ygg-dap-java-tests--items 8) :label) "adds()"))
  (should (equal (plist-get (ygg-dap-java-test-pick ygg-dap-java-tests--items 13) :label) "subtracts()"))
  (should (equal (plist-get (ygg-dap-java-test-pick ygg-dap-java-tests--items 4) :label) "CalcTest"))
  (should (equal (plist-get (ygg-dap-java-test-pick ygg-dap-java-tests--items 11) :label) "CalcTest"))
  (should-not (ygg-dap-java-test-pick ygg-dap-java-tests--items 2)))

(ert-deftest ygg-dap-java-declaration-line-skips-annotations ()
  (with-temp-buffer
    (insert "class A {\n    @Test\n    @DisplayName(\"x y\")\n    void adds() {}\n    @Test void inline() {}\n}\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (= (ygg-dap-java--declaration-line) 3))
    (forward-line 3)
    (should (= (ygg-dap-java--declaration-line) 4))))

(ert-deftest ygg-dap-java-test-request-names-method-by-handle-and-class-by-name ()
  (let ((method (json-parse-string (ygg-dap-java-test-request (ygg-dap-java-tests--method "adds()"))
                                   :object-type 'plist))
        (class (json-parse-string (ygg-dap-java-test-request (aref ygg-dap-java-tests--items 0))
                                  :object-type 'plist)))
    (should (equal (plist-get method :testLevel) 6))
    (should (equal (plist-get method :testKind) 0))
    (should (equal (plist-get method :testNames)
                   ["=demo/src\\/test\\/java<demo{CalcTest.java[CalcTest~adds"]))
    (should (equal (plist-get method :testHandles) []))
    (should (equal (plist-get class :testLevel) 5))
    (should (equal (plist-get class :testNames) ["demo.CalcTest"]))
    (should (equal (plist-get class :projectName) "demo"))))

(ert-deftest ygg-dap-java-test-with-port-replaces-placeholder ()
  (should (equal (ygg-dap-java-test-with-port '("-version" "3" "-port" "1234" "-test" "A:b") 5555)
                 '("-version" "3" "-port" "5555" "-test" "A:b")))
  (should (equal (ygg-dap-java-test-with-port '("-test" "A:b") 5555)
                 '("-test" "A:b" "-port" "5555"))))

(ert-deftest ygg-dap-java-testng-names-walks-children ()
  (should (equal (ygg-dap-java-testng-names (aref ygg-dap-java-tests--items 0))
                 '("demo.CalcTest#adds" "demo.CalcTest#subtracts")))
  (should (equal (ygg-dap-java-testng-names (ygg-dap-java-tests--method "adds()"))
                 '("demo.CalcTest#adds"))))

(ert-deftest ygg-dap-java-test-launch-builds-java-debug-strings ()
  (let* ((launch '(:projectName "demo" :mainClass "org.eclipse.jdt.internal.junit.runner.RemoteTestRunner"
                   :classpath ["/a/classes" "/b dir/x.jar"] :modulepath []
                   :workingDirectory "/demo" :vmArguments ["-ea" "-Dx=a b"]
                   :programArguments ["-version" "3" "-port" "1" "-testNameFile" "/t m/names"]))
         (config (ygg-dap-java-test-launch (list :vmArgs " -XX:+ShowCodeDetailsInExceptionMessages")
                                           launch 4242 nil nil)))
    (should (equal (plist-get config :mainClass) "org.eclipse.jdt.internal.junit.runner.RemoteTestRunner"))
    (should (equal (plist-get config :classPaths) ["/a/classes" "/b dir/x.jar"]))
    (should (equal (plist-get config :cwd) "/demo"))
    (should (equal (plist-get config :vmArgs)
                   "-XX:+ShowCodeDetailsInExceptionMessages -ea \"-Dx=a b\""))
    (should (equal (plist-get config :args)
                   "-version 3 -port 4242 -testNameFile \"/t m/names\""))))

(ert-deftest ygg-dap-java-test-launch-runs-testng-under-the-plugin-launcher ()
  (let ((config (ygg-dap-java-test-launch nil '(:projectName "demo" :classpath ["/c"])
                                          4242 '("demo.T#a") "/runner.jar")))
    (should (equal (plist-get config :mainClass) "com.microsoft.java.test.runner.Launcher"))
    (should (equal (plist-get config :classPaths) ["/c" "/runner.jar"]))
    (should (equal (plist-get config :args) "4242 testng demo.T#a"))))

(ert-deftest ygg-dap-java-install-derives-from-dape-jdtls ()
  (let* ((dape-configs (list (cons 'jdtls (list 'modes '(java-mode java-ts-mode) 'ensure #'ignore
                                                'fn #'ignore :filePath "f" :mainClass "m"
                                                :projectName "p" :args "" :stopOnEntry nil
                                                :type "java" :request "launch" :vmArgs " -X"
                                                :console "integratedConsole"))))
         (_ (ygg-dap-java-install))
         (test (alist-get 'jdtls-test dape-configs))
         (adb (alist-get 'jdtls-adb dape-configs)))
    (should (eq ygg-dap-java--jdtls-ensure #'ignore))
    (should (equal (plist-get test 'modes) '(java-mode java-ts-mode)))
    (should (eq (plist-get test 'fn) 'ygg-dap-java-test-resolve))
    (should (eq (plist-get test 'ensure) 'ygg-dap-java-test-ensure))
    (should (equal (plist-get test :vmArgs) " -X"))
    (should (equal (plist-get test :request) "launch"))
    (should-not (plist-member test :mainClass))
    (should (eq (plist-get test :filePath) 'ygg-dap-java--file))
    (should (eq (plist-get adb 'fn) 'ygg-dap-java-adb-resolve))
    (should (equal (plist-get adb :request) "attach"))
    (should (equal (plist-get adb :type) "java"))
    (dolist (key '(:mainClass :projectName :args :vmArgs :console :stopOnEntry))
      (should-not (plist-member adb key)))
    (should (plist-get (alist-get 'jdtls dape-configs) 'fn))))

(ert-deftest ygg-dap-java-attach-goes-to-the-jvm-it-is-given ()
  (let* ((dape-configs (list (cons 'jdtls (list 'modes '(java-mode java-ts-mode) 'ensure #'ignore
                                                'fn #'ignore :filePath "f" :mainClass "m"
                                                :projectName "p" :type "java" :request "launch"))))
         (_ (ygg-dap-java-install))
         (attach (alist-get 'jdtls-attach dape-configs))
         (file (make-temp-file "Main" nil ".java")))
    (should (eq (plist-get attach 'fn) 'ygg-dap-java-attach-resolve))
    (should (equal (plist-get attach :request) "attach"))
    (should (equal (plist-get attach :hostName) "localhost"))
    (should (equal (plist-get attach :port) 5005))
    (should-not (plist-member attach :mainClass))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-dap-java--server) (lambda () 'jdtls))
                  ((symbol-function 'ygg-dap-java--start-session)
                   (lambda (server) (should (eq server 'jdtls)) 47001)))
          (let ((config (ygg-dap-java-attach-resolve
                         (list :filePath file :hostName "10.0.0.7" :port 8000))))
            (should (equal (plist-get config 'port) 47001))
            (should (equal (plist-get config :hostName) "10.0.0.7"))
            (should (equal (plist-get config :port) 8000))))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest ygg-dap-java-test-resolve-asks-jdtls-and-owns-its-listener ()
  (let* ((file (make-temp-file "CalcTest" nil ".java"))
         (ygg-dap-java--unclaimed nil)
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'server))
                  ((symbol-function 'eglot-execute-command)
                   (lambda (_server command args)
                     (push (cons command args) calls)
                     (pcase command
                       ("vscode.java.test.junit.argument"
                        '(:body (:projectName "demo" :mainClass "Runner" :classpath ["/c"]
                                 :programArguments ["-port" "1"])))
                       ("vscode.java.startDebugSession" 7777)))))
          (let* ((config (ygg-dap-java-test-resolve
                          (list :filePath file 'ygg-dap-java-test (ygg-dap-java-tests--method "adds()"))))
                 (listener (get-process "java-test-results")))
            (should (equal (plist-get config 'port) 7777))
            (should (equal (plist-get config :mainClass) "Runner"))
            (should (equal (plist-get config :args)
                           (format "-port %d" (process-contact listener :service))))
            (should (equal (car (assoc "vscode.java.test.junit.argument" calls))
                           "vscode.java.test.junit.argument"))
            (funcall (plist-get config 'ygg-dap-java-release))
            (should-not (process-live-p listener))))
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

(ert-deftest ygg-dap-java-adb-serial-and-pick ()
  (let ((ygg-dap-java-adb-function
         (lambda (&rest _) "List of devices attached\nemulator-5554\tdevice\nR58\toffline\n\n")))
    (should (equal (ygg-dap-java-adb-serial) "emulator-5554")))
  (let ((ygg-dap-java-adb-function (lambda (&rest _) "List of devices attached\n\n")))
    (should-error (ygg-dap-java-adb-serial) :type 'user-error))
  (let ((processes '(("com.android.systemui" . "900") ("dev.scratch.java" . "4321"))))
    (should (equal (ygg-dap-java-adb-pick processes "dev.scratch.java") '("dev.scratch.java" . "4321")))
    (should (equal (ygg-dap-java-adb-pick (list (cadr processes)) nil) '("dev.scratch.java" . "4321")))
    (should-error (ygg-dap-java-adb-pick nil nil) :type 'user-error)))

(ert-deftest ygg-dap-java-adb-processes-keeps-debuggable-ones ()
  (cl-letf (((symbol-function 'ygg-dap-java--adb-jdwp-pids) (lambda (_) '("4321")))
            (ygg-dap-java-adb-function
             (lambda (&rest _) "  PID NAME\n  900 com.android.systemui\n 4321 dev.scratch.java\n")))
    (should (equal (ygg-dap-java--adb-processes "emulator-5554") '(("dev.scratch.java" . "4321"))))))

(ert-deftest ygg-dap-java-application-id-from-gradle-or-manifest ()
  (let ((root (file-name-as-directory (make-temp-file "ygg-dap-java-" t))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "app/src/main" root) t)
          (with-temp-file (expand-file-name "app/src/main/AndroidManifest.xml" root)
            (insert "<manifest package=\"dev.from.manifest\"/>"))
          (should (equal (ygg-dap-java-application-id root) "dev.from.manifest"))
          (with-temp-file (expand-file-name "app/build.gradle.kts" root)
            (insert "android {\n  defaultConfig {\n    applicationId = \"dev.scratch.java\"\n  }\n}\n"))
          (should (equal (ygg-dap-java-application-id root) "dev.scratch.java")))
      (delete-directory root t))))

(ert-deftest ygg-dap-java-adb-attach-forwards-and-removes-once ()
  (let* ((root (file-name-as-directory (make-temp-file "ygg-dap-java-" t)))
         (ygg-dap-java--unclaimed nil)
         (calls nil)
         (ygg-dap-java-adb-function (lambda (&rest args) (push args calls) "")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "app/src/main/java" root) t)
          (let* ((config (ygg-dap-java-adb-attach (list :request "attach") "emulator-5554" "4321"
                                                  40123 7777 root))
                 (release (plist-get config 'ygg-dap-java-release)))
            (should (equal (plist-get config 'port) 7777))
            (should (equal (plist-get config :port) 40123))
            (should (equal (plist-get config :hostName) "localhost"))
            (should (equal (plist-get config :sourcePaths)
                           (vector (expand-file-name "app/src/main/java" root))))
            (should (equal calls '(("-s" "emulator-5554" "forward" "tcp:40123" "jdwp:4321"))))
            (should (memq release ygg-dap-java--unclaimed))
            (funcall release)
            (funcall release)
            (should (equal (car calls) '("-s" "emulator-5554" "forward" "--remove" "tcp:40123")))
            (should (= (length calls) 2))))
      (delete-directory root t))))

(ert-deftest ygg-dap-java-release-follows-connection-shutdown ()
  (let* ((ygg-dap-java--unclaimed nil)
         (released 0)
         (config (ygg-dap-java--own (list :request "attach") (lambda () (cl-incf released))))
         (process (make-pipe-process :name "fake-adapter" :noquery t))
         (conn (make-instance 'jsonrpc-process-connection :name "fake" :process process
                              :on-shutdown #'ignore)))
    (cl-letf (((symbol-function 'dape--config) (lambda (_) config)))
      (ygg-dap-java--claim conn))
    (should-not ygg-dap-java--unclaimed)
    (ygg-dap-java--release-unclaimed)
    (should (= released 0))
    (delete-process process)
    (with-timeout (5 nil)
      (while (zerop released)
        (accept-process-output nil 0.05)))
    (should (= released 1))))

(ert-deftest ygg-dap-java-unclaimed-release-runs-before-next-session ()
  (let* ((ygg-dap-java--unclaimed nil)
         (released 0))
    (ygg-dap-java--own nil (lambda () (cl-incf released)))
    (ygg-dap-java--release-unclaimed)
    (should (= released 1))
    (should-not ygg-dap-java--unclaimed)))

(ert-deftest ygg-dap-java-test-request-keeps-non-ascii-names-as-text ()
  (let ((request (ygg-dap-java-test-request
                  (plist-put (copy-sequence (ygg-dap-java-tests--method "adds()")) :projectName "démo→"))))
    (should (multibyte-string-p request))
    (should (equal (plist-get (json-parse-string request :object-type 'plist) :projectName) "démo→"))
    (should (json-serialize (vector request)))))

(provide 'dap-java-tests)
;;; dap-java-tests.el ends here
