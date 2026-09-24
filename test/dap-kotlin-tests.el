;;; dap-kotlin-tests.el --- Tests for Kotlin debug configs -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dape)
(require 'ygg-dap-kotlin)

(defmacro ygg-dap-kotlin-test--in-file (name text &rest body)
  "Run BODY in a kotlin-ts buffer visiting NAME holding TEXT."
  (declare (indent 2))
  `(let* ((dir (file-name-as-directory (make-temp-file "ygg-dap-kotlin-" t)))
          (path (expand-file-name ,name dir)))
     (unwind-protect
         (with-temp-buffer
           (insert ,text)
           (setq buffer-file-name path)
           (goto-char (point-min))
           ,@body)
       (delete-directory dir t))))

(defun ygg-dap-kotlin-test--main-at (name text needle)
  (ygg-dap-kotlin-test--in-file name text
    (when needle (search-forward needle))
    (ygg-dap-kotlin-main-class)))

(defconst ygg-dap-kotlin-test--mains
  "package com.acme.app

object Tool {
    @JvmStatic
    fun main(args: Array<String>) {}
}

class Host {
    companion object {
        @JvmStatic fun main(args: Array<String>) { println(1) }
    }
    object Inner {
        @JvmStatic fun main(args: Array<String>) {}
    }
}

object Plain {
    fun main(args: Array<String>) {}
}

fun main() { println(2) }
")

(ert-deftest ygg-dap-kotlin-top-level-main-lives-on-the-file-facade ()
  (skip-unless (treesit-language-available-p 'kotlin))
  (should (equal (ygg-dap-kotlin-test--main-at "Main.kt" "package demo\n\nfun main() {}\n" nil)
                 "demo.MainKt"))
  (should (equal (ygg-dap-kotlin-test--main-at "app.kt" "fun main() {}\n" nil) "AppKt")))

(ert-deftest ygg-dap-kotlin-file-jvm-name-renames-the-facade ()
  (skip-unless (treesit-language-available-p 'kotlin))
  (should (equal (ygg-dap-kotlin-test--main-at
                  "Main.kt" "@file:JvmName(\"Launcher\")\npackage a.b\n\nfun main() {}\n" nil)
                 "a.b.Launcher")))

(ert-deftest ygg-dap-kotlin-the-main-under-point-wins ()
  (skip-unless (treesit-language-available-p 'kotlin))
  (let ((at (lambda (needle)
              (ygg-dap-kotlin-test--main-at "Main.kt" ygg-dap-kotlin-test--mains needle))))
    (should (equal (funcall at nil) "com.acme.app.Tool"))
    (should (equal (funcall at "println(1") "com.acme.app.Host"))
    (should (equal (funcall at "object Inner {\n        @JvmStatic fun main") "com.acme.app.Host$Inner"))
    (should (equal (funcall at "println(2") "com.acme.app.MainKt"))))

(ert-deftest ygg-dap-kotlin-a-main-that-is-not-static-is-no-entry-point ()
  (skip-unless (treesit-language-available-p 'kotlin))
  (should (equal (ygg-dap-kotlin-test--main-at "Main.kt" ygg-dap-kotlin-test--mains
                                               "object Plain {\n    fun main")
                 "com.acme.app.Tool"))
  (should-not (ygg-dap-kotlin-test--main-at
               "Main.kt" "object Plain {\n    fun main(args: Array<String>) {}\n}\n" nil)))

(ert-deftest ygg-dap-kotlin-facade-names-follow-kotlinc ()
  (should (equal (ygg-dap-kotlin-facade-class "/p/app-util.kt") "App_utilKt"))
  (should (equal (ygg-dap-kotlin-facade-class "/p/Main.kt" "Boot") "Boot")))

(defmacro ygg-dap-kotlin-test--with-server (replies &rest body)
  "Run BODY with kotlin-lsp answering commands from the alist REPLIES.
Binds calls to the commands sent, newest first."
  (declare (indent 1))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'ygg-dap-kotlin-server) (lambda (&rest _) 'server))
               ((symbol-function 'eglot-path-to-uri) (lambda (path) (concat "file://" path)))
               ((symbol-function 'eglot--project) (lambda (_) '(transient . "/p/")))
               ((symbol-function 'eglot-execute-command)
                (lambda (_server command arguments)
                  (push (cons command arguments) calls)
                  (let ((reply (cdr (assoc command ,replies))))
                    (if (functionp reply) (funcall reply) reply)))))
       ,@body)))

(ert-deftest ygg-dap-kotlin-gradle-projects-launch-through-gradle ()
  (ygg-dap-kotlin-test--with-server
      '(("intellij.java.resolveBuildToolLaunch"
         . (:tool "gradle" :projectPath ":" :sourceSet "main" :scopeClassPaths ["/p/build"]))
        ("start_debug_server" . 41234))
    (let ((config (ygg-dap-kotlin-resolve-launch
                   (list :type "intellij_jvm" :request "launch"
                         :filePath "/p/src/Main.kt" :mainClass "demo.MainKt"))))
      (should (equal (plist-get config :type) "intellij_gradle"))
      (should (equal (plist-get config :buildToolTarget)
                     '(:uri "file:///p/src/Main.kt" :projectPath ":" :sourceSet "main")))
      (should (equal (plist-get config :classPaths) ["/p/build"]))
      (should (equal (plist-get config 'port) 41234))
      (should (equal (plist-get config 'host) "127.0.0.1"))
      (should-not (plist-get config 'compile))
      (should (equal (cdr (assoc "start_debug_server" calls)) ["file:///p/"])))))

(ert-deftest ygg-dap-kotlin-other-projects-build-first-and-launch-a-jvm ()
  (ygg-dap-kotlin-test--with-server
      '(("intellij.java.resolveBuildToolLaunch" . (lambda () (error "No build tool")))
        ("intellij.java.resolveLaunch"
         . (:javaExec "/jdk/bin/java" :classpath ["/p/target/classes"] :modulePath []
                      :workingDirectory "/p"))
        ("intellij.java.resolveBuildCommand"
         . (:supported t :cwd "/p" :command ["mvn" "-q" "compile"]))
        ("start_debug_server" . 41235))
    (let ((config (ygg-dap-kotlin-resolve-launch
                   (list :type "intellij_jvm" :request "launch" :cwd "/elsewhere"
                         :filePath "/p/src/Main.kt" :mainClass "demo.MainKt"))))
      (should (equal (plist-get config :type) "intellij_jvm"))
      (should (equal (plist-get config :classPaths) ["/p/target/classes"]))
      (should (equal (plist-get config :javaExec) "/jdk/bin/java"))
      (should (equal (plist-get config :cwd) "/elsewhere"))
      (should (equal (plist-get config 'compile) "mvn -q compile"))
      (should (equal (plist-get config 'command-cwd) "/p"))
      (should (equal (plist-get config 'port) 41235)))))

(ert-deftest ygg-dap-kotlin-a-file-outside-any-module-says-why ()
  (ygg-dap-kotlin-test--with-server
      '(("intellij.java.resolveBuildToolLaunch" . (lambda () (error "No module")))
        ("intellij.java.resolveLaunch" . (lambda () (error "No module found for uri"))))
    (should-error (ygg-dap-kotlin-resolve-launch
                   (list :filePath "/tmp/Solo.kt" :mainClass "SoloKt"))
                  :type 'user-error)
    (should-not (assoc "start_debug_server" calls))))

(ert-deftest ygg-dap-kotlin-a-config-back-from-compiling-keeps-its-adapter ()
  (let ((ygg-dap-kotlin--compiling nil)
        (next 41000))
    (ygg-dap-kotlin-test--with-server
        `(("start_debug_server" . ,(lambda () (cl-incf next))))
      (let* ((config (ygg-dap-kotlin-connect (list 'compile "mvn compile" :filePath "/p/Main.kt")))
             (port (plist-get config 'port)))
        (should (equal port 41001))
        (should (eq (ygg-dap-kotlin-resolve-launch config) config))
        (should-not ygg-dap-kotlin--compiling)
        (should (equal (plist-get (ygg-dap-kotlin-connect config) 'port) 41002))
        (should (equal ygg-dap-kotlin--compiling '(41002)))))))

(ert-deftest ygg-dap-kotlin-a-restart-asks-for-a-fresh-adapter ()
  (let ((ygg-dap-kotlin--compiling nil)
        (next 42000))
    (ygg-dap-kotlin-test--with-server
        `(("start_debug_server" . ,(lambda () (cl-incf next))))
      (let ((first (ygg-dap-kotlin-connect (list :port 5005))))
        (should (equal (plist-get first 'port) 42001))
        (should (equal (plist-get (ygg-dap-kotlin-connect first) 'port) 42002))
        (should-not ygg-dap-kotlin--compiling)))))

(defmacro ygg-dap-kotlin-test--stopping (&rest body)
  "Run BODY with adapter stops recorded in stopped, newest first, and none waiting."
  (declare (indent 0))
  `(let ((ygg-dap-kotlin--compiling nil)
         (stopped nil))
     (cl-letf (((symbol-function 'ygg-dap-kotlin--stop-adapter)
                (lambda (port) (push port stopped))))
       ,@body)))

(ert-deftest ygg-dap-kotlin-a-failed-compile-releases-its-adapter ()
  (ygg-dap-kotlin-test--stopping
    (let ((next 43000))
      (ygg-dap-kotlin-test--with-server
          `(("start_debug_server" . ,(lambda () (cl-incf next))))
        (ygg-dap-kotlin-connect (list 'compile "false" :filePath "/p/Main.kt"))))
    (should (equal ygg-dap-kotlin--compiling '(43001)))
    (let* ((buffer nil)
           (dape-compile-function (lambda (command) (setq buffer (compile command)))))
      (dape--compile (list 'compile "exit 3") #'ignore)
      (unwind-protect
          (let ((deadline (+ (float-time) 10)))
            (while (and ygg-dap-kotlin--compiling (< (float-time) deadline))
              (accept-process-output nil 0.1)))
        (let (kill-buffer-query-functions) (kill-buffer buffer))))
    (should-not ygg-dap-kotlin--compiling)
    (should (equal stopped '(43001)))))

(ert-deftest ygg-dap-kotlin-only-a-failed-dape-compile-releases ()
  (ygg-dap-kotlin-test--stopping
    (setq ygg-dap-kotlin--compiling (list 43005))
    (with-temp-buffer
      (ygg-dap-kotlin--compile-finished (current-buffer) "exited abnormally with code 1\n")
      (setq-local dape--compile-after-fn #'ignore)
      (ygg-dap-kotlin--compile-finished (current-buffer) "finished\n"))
    (should (equal ygg-dap-kotlin--compiling '(43005)))
    (should-not stopped)))

(ert-deftest ygg-dap-kotlin-a-new-compile-releases-the-one-that-never-came-back ()
  (ygg-dap-kotlin-test--stopping
    (let ((next 44000))
      (ygg-dap-kotlin-test--with-server
          `(("start_debug_server" . ,(lambda () (cl-incf next))))
        (dotimes (_ 3)
          (ygg-dap-kotlin-connect (list 'compile "gradle build" :filePath "/p/Main.kt")))))
    (should (equal ygg-dap-kotlin--compiling '(44003)))
    (should (equal stopped '(44002 44001)))))

(ert-deftest ygg-dap-kotlin-stopping-an-adapter-connects-to-it ()
  (let* ((accepted nil)
         (server (make-network-process :name "ygg-dap-kotlin-adapter-test" :server t
                                       :host "127.0.0.1" :service t :noquery t
                                       :log (lambda (_server client _message)
                                              (setq accepted t)
                                              (delete-process client)))))
    (unwind-protect
        (let ((deadline (+ (float-time) 5)))
          (ygg-dap-kotlin--stop-adapter (process-contact server :service))
          (while (and (not accepted) (< (float-time) deadline))
            (accept-process-output nil 0.1))
          (should accepted))
      (delete-process server))
    (should-not (ygg-dap-kotlin--stop-adapter 1))))

(ert-deftest ygg-dap-kotlin-a-bad-adapter-port-is-an-error ()
  (ygg-dap-kotlin-test--with-server '(("start_debug_server" . nil))
    (should-error (ygg-dap-kotlin-connect (list :port 5005)) :type 'user-error)))

(ert-deftest ygg-dap-kotlin-attach-config-targets-loopback-only ()
  (let ((config (ygg-dap-kotlin-attach-config 5005 "127.0.0.1")))
    (should (equal (plist-get config :request) "attach"))
    (should (equal (plist-get config :type) "intellij_jvm"))
    (should (equal (plist-get config :port) 5005))
    (should (eq (plist-get config 'fn) #'ygg-dap-kotlin-connect))
    (should-not (plist-member config 'port)))
  (should (ygg-dap-kotlin-attach-config 5005))
  (should-error (ygg-dap-kotlin-attach-config 5005 "10.0.0.2") :type 'user-error))

(ert-deftest ygg-dap-kotlin-entries-are-in-dape-for-kotlin-buffers ()
  (dolist (name '(kotlin kotlin-attach kotlin-adb))
    (let ((entry (alist-get name dape-configs)))
      (should entry)
      (should (memq 'kotlin-ts-mode (plist-get entry 'modes)))
      (should-not (plist-member entry 'command))))
  (should (eq (plist-get (alist-get 'kotlin-adb dape-configs) 'fn) #'ygg-dap-kotlin-adb-resolve))
  (should (equal (plist-get (alist-get 'kotlin-attach dape-configs) :port) 5005)))

(ert-deftest ygg-dap-kotlin-without-a-server-the-entry-is-refused ()
  (with-temp-buffer
    (should-error (dape--config-ensure (alist-get 'kotlin-attach dape-configs) t)
                  :type 'user-error)))

(ert-deftest ygg-dap-kotlin-the-prompt-checks-the-visited-file ()
  (let (asked)
    (cl-letf (((symbol-function 'ygg-dap-kotlin-server)
               (lambda (&optional file) (push file asked) 'server)))
      (with-temp-buffer
        (setq buffer-file-name "/p/src/Main.kt")
        (should (dape--config-ensure (alist-get 'kotlin dape-configs)))
        (should (equal asked '("/p/src/Main.kt")))))))

(ert-deftest ygg-dap-kotlin-only-ready-devices-are-offered ()
  (should (equal (ygg-dap-kotlin-adb-parse-devices
                  "List of devices attached\nemulator-5554\tdevice\nemulator-5556\toffline\nR5CT\tunauthorized\n\n")
                 '("emulator-5554"))))

(ert-deftest ygg-dap-kotlin-debuggable-pids-are-named-from-ps ()
  (should (equal (ygg-dap-kotlin-adb-parse-processes
                  "1203\n4410\r\n"
                  "  PID NAME\n    1 init\n 1203 com.android.systemui\n 4410 dev.scratch.kt\n")
                 '(("com.android.systemui (1203)" . "1203")
                   ("dev.scratch.kt (4410)" . "4410")))))

(ert-deftest ygg-dap-kotlin-the-selected-android-device-is-used ()
  (cl-letf (((symbol-function 'ygg-device-current)
             (lambda () '(:platform android :id "emulator-5554" :name "Pixel"))))
    (should (equal (ygg-dap-kotlin--adb-serial nil) "emulator-5554")))
  (cl-letf (((symbol-function 'ygg-device-current)
             (lambda () '(:platform ios :id "ABC" :name "iPhone")))
            ((symbol-function 'ygg-dap-kotlin--adb)
             (lambda (&rest _) "List of devices attached\nemulator-5556\tdevice\n")))
    (should (equal (ygg-dap-kotlin--adb-serial nil) "emulator-5556"))))

(ert-deftest ygg-dap-kotlin-adb-attach-forwards-then-releases-on-failure ()
  (let (adb)
    (cl-letf (((symbol-function 'ygg-dap-kotlin--adb)
               (lambda (&rest args) (push args adb) "4410\n"))
              ((symbol-function 'ygg-dap-kotlin--free-port) (lambda () 50123))
              ((symbol-function 'ygg-dap-kotlin-connect)
               (lambda (_) (user-error "No kotlin-lsp server"))))
      (should-error (ygg-dap-kotlin-adb-resolve
                     (list 'adb-serial "emulator-5554" 'adb-package "dev.scratch.kt"))
                    :type 'user-error)
      (should (member '("emulator-5554" "forward" "tcp:50123" "jdwp:4410") adb))
      (should (equal (car adb) '("emulator-5554" "forward" "--remove" "tcp:50123"))))))

(ert-deftest ygg-dap-kotlin-a-closed-session-removes-its-forward ()
  (let (removed
        (process (make-process :name "ygg-dap-kotlin-test" :command '("sleep" "30")
                               :noquery t)))
    (cl-letf (((symbol-function 'ygg-dap-kotlin-adb-unforward)
               (lambda (serial port) (push (list serial port) removed))))
      (ygg-dap-kotlin--unforward-when-closed process "emulator-5554" 50123)
      (delete-process process)
      (should (equal removed '(("emulator-5554" 50123)))))))

(ert-deftest ygg-dap-kotlin-the-forward-waits-for-a-session-that-connects-late ()
  (let ((process (make-process :name "ygg-dap-kotlin-late" :command '("sleep" "30") :noquery t))
        (polls 0)
        removed)
    (cl-letf (((symbol-function 'ygg-dap-kotlin--session-process)
               (lambda (_port) (and (> (cl-incf polls) 2) process)))
              ((symbol-function 'ygg-dap-kotlin-adb-unforward)
               (lambda (serial port) (push (list serial port) removed))))
      (ygg-dap-kotlin--unforward-with-session "emulator-5554" 50123)
      (let ((deadline (+ (float-time) 5)))
        (while (and (<= polls 2) (< (float-time) deadline))
          (accept-process-output nil 0.1)))
      (should-not removed)
      (delete-process process)
      (should (equal removed '(("emulator-5554" 50123)))))))

(ert-deftest ygg-dap-kotlin-a-session-that-never-comes-drops-the-forward ()
  (let (removed)
    (cl-letf (((symbol-function 'ygg-dap-kotlin--session-process) (lambda (_port) nil))
              ((symbol-function 'ygg-dap-kotlin-adb-unforward)
               (lambda (serial port) (push (list serial port) removed))))
      (ygg-dap-kotlin--unforward-with-session "emulator-5554" 50123 49)
      (let ((deadline (+ (float-time) 5)))
        (while (and (not removed) (< (float-time) deadline))
          (accept-process-output nil 0.1)))
      (should (equal removed '(("emulator-5554" 50123)))))))

(ert-deftest ygg-dap-kotlin-a-package-that-is-not-running-falls-back-to-the-picker ()
  (cl-letf (((symbol-function 'ygg-dap-kotlin--adb)
             (lambda (_serial &rest args)
               (if (equal (car args) "shell")
                   (if (equal (cadr args) "pidof") (user-error "adb pidof failed") " 1 init\n 77 dev.x.debug\n")
                 "")))
            ((symbol-function 'ygg-dap-kotlin--adb-jdwp) (lambda (_serial) "77\n"))
            ((symbol-function 'completing-read) (lambda (_prompt choices &rest _) (caar choices))))
    (should (equal (ygg-dap-kotlin--adb-pid "emulator-5554" "dev.x") "77"))))

(ert-deftest ygg-dap-kotlin-application-id-is-read-from-the-gradle-script ()
  (let ((root (file-name-as-directory (make-temp-file "ygg-dap-kotlin-app-" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "settings.gradle.kts" root))
          (make-directory (expand-file-name "app/src" root) t)
          (with-temp-file (expand-file-name "app/build.gradle.kts" root)
            (insert "android {\n  defaultConfig {\n    testApplicationId = \"dev.scratch.kt.test\"\n    // applicationId = \"dev.old\"\n    applicationId = \"dev.scratch.kt\"\n  }\n}\n"))
          (let ((default-directory (expand-file-name "app/src/" root)))
            (should (equal (ygg-dap-kotlin-application-id) "dev.scratch.kt"))))
      (delete-directory root t))))

;;; dap-kotlin-tests.el ends here
