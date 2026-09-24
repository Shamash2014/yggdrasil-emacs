;;; compose-preview-tests.el --- Tests for ygg-compose-preview -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-compose-preview)

(defconst compose-preview-tests--show-json
  "{\"schema\":\"compose-preview-show/v2\",\"previews\":[
 {\"id\":\"com.example.hello.GreetingKt.GreetingPreview\",\"module\":\"app\",
  \"projectDirectory\":\"/w/hello/app\",\"functionName\":\"GreetingPreview\",
  \"className\":\"com.example.hello.GreetingKt\",
  \"sourceFile\":\"src/main/java/com/example/hello/Greeting.kt\",
  \"params\":{\"name\":null,\"widthDp\":null},
  \"captures\":[{\"pngPath\":\"/w/hello/app/build/compose-previews/renders/GreetingPreview-848e4640.png\",\"changed\":true}],
  \"pngPath\":\"/w/hello/app/build/compose-previews/renders/GreetingPreview-848e4640.png\",
  \"changed\":true,\"dataExtensions\":{}},
 {\"id\":\"com.example.hello.GreetingKt.GreetingWidePreview_Wide\",\"module\":\"app\",
  \"projectDirectory\":\"/w/hello/app\",\"functionName\":\"GreetingWidePreview\",
  \"className\":\"com.example.hello.GreetingKt\",
  \"sourceFile\":\"src/main/java/com/example/hello/Greeting.kt\",
  \"params\":{\"name\":\"Wide\",\"widthDp\":320},
  \"captures\":[{\"pngPath\":\"/w/hello/app/build/compose-previews/renders/GreetingWidePreview_Wide-36add390.png\",\"changed\":false}],
  \"changed\":false,\"dataExtensions\":{}},
 {\"id\":\"com.example.hello.OtherKt.OtherPreview\",\"module\":\"app\",
  \"projectDirectory\":\"/w/hello/app\",\"functionName\":\"OtherPreview\",
  \"className\":\"com.example.hello.OtherKt\",
  \"sourceFile\":\"src/main/java/com/example/hello/Other.kt\",
  \"params\":{\"name\":null},\"captures\":[],\"dataExtensions\":{}}],
 \"counts\":{\"total\":3}}")

(defmacro compose-preview-tests--in-project (&rest body)
  "Run BODY with root bound to a scratch Gradle project holding an app module."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "cp-proj" t)))
          (app (expand-file-name "app/" root))
          (src (expand-file-name "src/main/java/com/example/hello/Greeting.kt" app)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory src) t)
           (write-region "" nil (expand-file-name "settings.gradle.kts" root))
           (write-region "" nil (expand-file-name "build.gradle.kts" root))
           (write-region "" nil (expand-file-name "build.gradle.kts" app))
           (write-region "package com.example.hello\n" nil src)
           ,@body)
       (delete-directory root t))))

(ert-deftest compose-preview-show-args-target-one-id-exactly ()
  (should (equal (ygg-compose-preview--show-args
                  ":app" (ygg-compose-preview--selector '("a.BKt.One") "a."))
                 '("show" "--json" "--module" ":app" "--id" "a.BKt.One"))))

(ert-deftest compose-preview-show-args-filter-several-ids-by-their-common-prefix ()
  (should (equal (ygg-compose-preview--selector
                  '("com.example.hello.GreetingKt.GreetingPreview"
                    "com.example.hello.GreetingKt.GreetingWidePreview_Wide")
                  "com.example.hello.")
                 '("--filter" "com.example.hello.GreetingKt.Greeting"))))

(ert-deftest compose-preview-show-args-fall-back-to-the-package-without-a-manifest ()
  (should (equal (ygg-compose-preview--selector nil "com.example.hello.")
                 '("--filter" "com.example.hello."))))

(ert-deftest compose-preview-file-class-follows-kotlin-facade-naming ()
  (should (equal (ygg-compose-preview--file-class "package a.b\n\nfun x() {}" "/p/greeting.kt")
                 "a.b.GreetingKt"))
  (should (equal (ygg-compose-preview--file-class
                  "@file:JvmName(\"Screens\")\npackage a.b\n" "/p/Greeting.kt")
                 "a.b.Screens"))
  (should (equal (ygg-compose-preview--package "// c\npackage com.x.y\n") "com.x.y")))

(ert-deftest compose-preview-gradle-path-of-nested-and-root-modules ()
  (should (equal (ygg-compose-preview--gradle-path "/w/p/feature/ui/" "/w/p/") ":feature:ui"))
  (should (equal (ygg-compose-preview--gradle-path "/w/p/app" "/w/p") ":app"))
  (should (equal (ygg-compose-preview--gradle-path "/w/p/" "/w/p/") ":")))

(ert-deftest compose-preview-finds-root-and-module-of-a-source-file ()
  (compose-preview-tests--in-project
    (should (equal (ygg-compose-preview--gradle-root src) root))
    (should (equal (file-name-as-directory (ygg-compose-preview--module-dir src root)) app))))

(ert-deftest compose-preview-keeps-only-the-previews-of-the-file ()
  (let* ((parsed (ygg-compose-preview--parse-json compose-preview-tests--show-json))
         (mine (cl-letf (((symbol-function 'file-truename) #'identity))
                 (ygg-compose-preview--for-file
                  (plist-get parsed :previews)
                  "/w/hello/app/src/main/java/com/example/hello/Greeting.kt" "/w/hello/app"))))
    (should (equal (mapcar (lambda (p) (plist-get p :functionName)) mine)
                   '("GreetingPreview" "GreetingWidePreview")))))

(ert-deftest compose-preview-matches-files-through-symlinked-directories ()
  (compose-preview-tests--in-project
    (let ((link (concat (directory-file-name root) "-link")))
      (make-symbolic-link root link)
      (unwind-protect
          (should (ygg-compose-preview--for-file
                   (list (list :sourceFile "src/main/java/com/example/hello/Greeting.kt"
                               :projectDirectory (expand-file-name "app" link)))
                   src app))
        (delete-file link)))))

(ert-deftest compose-preview-parses-entries-with-label-and-png ()
  (let* ((parsed (ygg-compose-preview--parse-json compose-preview-tests--show-json))
         (entries (mapcar #'ygg-compose-preview--entry (plist-get parsed :previews))))
    (should (equal (plist-get (nth 1 entries) :png)
                   "/w/hello/app/build/compose-previews/renders/GreetingWidePreview_Wide-36add390.png"))
    (should (equal (mapcar #'ygg-compose-preview--label entries)
                   '("GreetingPreview" "GreetingWidePreview · Wide" "OtherPreview")))
    (should (null (plist-get (nth 2 entries) :png)))))

(ert-deftest compose-preview-manifest-previews-resolve-against-the-module ()
  (compose-preview-tests--in-project
    (let ((manifest (expand-file-name "build/compose-previews/previews.json" app)))
      (make-directory (file-name-directory manifest) t)
      (write-region "{\"module\":\"app\",\"previews\":[{\"id\":\"com.example.hello.GreetingKt.P\",
\"functionName\":\"P\",\"sourceFile\":\"src/main/java/com/example/hello/Greeting.kt\"}]}"
                    nil manifest)
      (should (equal (mapcar (lambda (p) (plist-get p :id))
                             (ygg-compose-preview--for-file
                              (ygg-compose-preview--manifest app) src app))
                     '("com.example.hello.GreetingKt.P"))))))

(ert-deftest compose-preview-pick-returns-the-chosen-preview-id ()
  (let* ((parsed (ygg-compose-preview--parse-json compose-preview-tests--show-json))
         (candidates (ygg-compose-preview--candidates
                      (mapcar #'ygg-compose-preview--entry (plist-get parsed :previews))))
         (started nil)
         (session (ygg-compose-preview--session-create :source (current-buffer))))
    (should (equal (cdr (assoc "GreetingWidePreview · Wide" candidates))
                   "com.example.hello.GreetingKt.GreetingWidePreview_Wide"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _) (car (nth 1 table))))
              ((symbol-function 'ygg-compose-preview--start)
               (lambda (ids) (setq started ids))))
      (ygg-compose-preview--pick-from session (plist-get parsed :previews)))
    (should (equal started '("com.example.hello.GreetingKt.GreetingWidePreview_Wide")))))

(ert-deftest compose-preview-candidates-keep-duplicate-labels-apart ()
  (should (equal (mapcar #'car (ygg-compose-preview--candidates
                                '((:id "a" :function "P") (:id "b" :function "P"))))
                 '("P" "P <2>"))))

(ert-deftest compose-preview-a11y-line-summarises-findings ()
  (should (equal (ygg-compose-preview--a11y-line nil) "a11y · no findings"))
  (should (equal (ygg-compose-preview--a11y-line
                  '((:checkId "TouchTargetSize" :severity "ERROR" :label "Submit")))
                 "a11y · 1 · TouchTargetSize “Submit” error"))
  (should (equal (ygg-compose-preview--a11y-line
                  [(:level "ERROR" :type "SpeakableTextPresentCheck" :message "No label.")])
                 "a11y · 1 · SpeakableTextPresentCheck error")))

(ert-deftest compose-preview-a11y-read-from-daemon-data-products ()
  (should (eq (ygg-compose-preview--a11y-of nil) :none))
  (should (null (ygg-compose-preview--a11y-of
                 '((:kind "a11y/atf" :schemaVersion 1 :payload (:findings nil))))))
  (should (equal (ygg-compose-preview--a11y-of
                  [(:kind "a11y/atf" :payload (:findings [(:type "X")]))])
                 '((:type "X")))))

(ert-deftest compose-preview-daemon-command-follows-the-launch-descriptor ()
  (let ((descriptor (ygg-compose-preview--parse-json
                     "{\"javaLauncher\":\"/jdk/bin/java\",\"mainClass\":\"ee.D\",
\"jvmArgs\":[\"-Xmx1024m\"],\"classpath\":[\"/a.jar\",\"/b.jar\"],
\"systemProperties\":{\"composeai.x\":\"1\",\"composeai.unset\":null}}")))
    (should (equal (ygg-compose-preview--daemon-command descriptor "/tmp/cp.args")
                   '("/jdk/bin/java" "-Xmx1024m" "-Dcomposeai.x=1" "@/tmp/cp.args" "ee.D")))
    (should (equal (ygg-compose-preview--argfile-text (plist-get descriptor :classpath))
                   (format "-cp\n\"/a.jar%s/b.jar\"\n" path-separator)))))

(ert-deftest compose-preview-cli-exit-codes-become-quiet-errors ()
  (let ((session (ygg-compose-preview--session-create
                  :file "/w/hello/app/src/main/java/com/example/hello/Greeting.kt"
                  :module-dir "/w/hello/app")))
    (cl-letf (((symbol-function 'file-truename) #'identity))
      (ygg-compose-preview--cli-finished session 3 "")
      (should (equal (ygg-compose-preview--session-error session) "no @Preview in this file"))
      (setf (ygg-compose-preview--session-error session) nil)
      (ygg-compose-preview--cli-finished session 0 compose-preview-tests--show-json)
      (should (null (ygg-compose-preview--session-error session)))
      (should (= 2 (length (ygg-compose-preview--session-previews session)))))))

(ert-deftest compose-preview-save-during-a-render-queues-one-more ()
  (let ((session (ygg-compose-preview--session-create :busy t))
        (runs 0))
    (cl-letf (((symbol-function 'ygg-compose-preview--cli-refresh) (lambda (_) (cl-incf runs))))
      (ygg-compose-preview--refresh session)
      (ygg-compose-preview--refresh session)
      (should (= runs 0))
      (should (ygg-compose-preview--session-pending session)))))

(ert-deftest compose-preview-commands-run-under-mise-exec-when-mise-exists ()
  (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name)))
            ((symbol-function 'ygg-jdk-home) (lambda () nil)))
    (let ((ygg-compose-preview-mise-exec t))
      (should (equal (ygg-compose-preview--wrap (list (ygg-compose-preview--program) "list"))
                     '("mise" "exec" "--" "compose-preview" "list")))
      (cl-letf (((symbol-function 'ygg-jdk-home) (lambda () "/direnv/jdk")))
        (should (equal (ygg-compose-preview--wrap (list (ygg-compose-preview--program) "list"))
                       '("mise" "exec" "--" "env" "JAVA_HOME=/direnv/jdk" "compose-preview" "list")))))
    (let ((ygg-compose-preview-mise-exec nil))
      (should (equal (ygg-compose-preview--wrap (list (ygg-compose-preview--program) "list"))
                     '("/bin/compose-preview" "list"))))))

(defmacro compose-preview-tests--daemon-stubs (&rest body)
  "Run BODY with jsonrpc and process calls recorded in sent, cli and shut."
  (declare (indent 0))
  `(let ((sent nil) (cli 0) (shut 0) (reply nil))
     (cl-letf (((symbol-function 'jsonrpc-notify)
                (lambda (_c method params) (push (cons method params) sent)))
               ((symbol-function 'jsonrpc-async-request)
                (lambda (_c method params &rest args)
                  (push (cons method params) sent)
                  (when (and (eq method :renderNow) reply)
                    (funcall (plist-get args :success-fn) reply))))
               ((symbol-function 'jsonrpc-shutdown) (lambda (&rest _) (cl-incf shut)))
               ((symbol-function 'ygg-compose-preview--cli-refresh) (lambda (_) (cl-incf cli)))
               ((symbol-function 'ygg-compose-preview--draw) #'ignore)
               ((symbol-function 'ygg-compose-preview--file-ids) (lambda (_) nil)))
       ,@body)))

(ert-deftest compose-preview-daemon-with-no-ids-renders-through-the-cli ()
  (compose-preview-tests--daemon-stubs
    (let ((session (ygg-compose-preview--session-create :conn 'c :daemon 'ready :busy t)))
      (ygg-compose-preview--daemon-render session)
      (should (= cli 1))
      (should (null sent)))))

(ert-deftest compose-preview-daemon-rejecting-every-id-is-retired-not-restarted ()
  (compose-preview-tests--daemon-stubs
    (setq reply '(:queued [] :rejected [(:id "a" :reason "unknown")]))
    (let ((session (ygg-compose-preview--session-create :conn 'c :daemon 'ready :busy t
                                                        :ids '("a"))))
      (ygg-compose-preview--daemon-render session)
      (should (eq (ygg-compose-preview--session-daemon session) 'failed))
      (should (= cli 1))
      (should (= shut 1))
      (when (timerp (ygg-compose-preview--session-watchdog session))
        (cancel-timer (ygg-compose-preview--session-watchdog session))))))

(ert-deftest compose-preview-render-finishes-when-every-id-reports ()
  (compose-preview-tests--daemon-stubs
    (let ((session (ygg-compose-preview--session-create :conn 'c :daemon 'ready :busy t
                                                        :ids '("a" "b") :started (float-time))))
      (ygg-compose-preview--daemon-render session)
      (ygg-compose-preview--daemon-notified session 'renderFinished '(:id "a" :pngPath "/a.png"))
      (ygg-compose-preview--daemon-notified session 'renderFinished '(:id "stale" :pngPath "/s.png"))
      (should (ygg-compose-preview--session-busy session))
      (ygg-compose-preview--daemon-notified session 'renderFailed '(:id "b" :error (:message "boom")))
      (should-not (ygg-compose-preview--session-busy session))
      (should-not (ygg-compose-preview--session-watchdog session))
      (should (equal (mapcar (lambda (e) (plist-get e :id))
                             (ygg-compose-preview--session-previews session))
                     '("a" "b")))
      (should (equal (plist-get (ygg-compose-preview--find session "b") :failed) "boom")))))

(ert-deftest compose-preview-a-stale-connection-shutting-down-leaves-the-new-one ()
  (compose-preview-tests--daemon-stubs
    (let ((session (ygg-compose-preview--session-create :conn 'new :daemon 'starting)))
      (ygg-compose-preview--daemon-gone session 'old)
      (should (eq (ygg-compose-preview--session-daemon session) 'starting))
      (ygg-compose-preview--daemon-gone session 'new)
      (should (eq (ygg-compose-preview--session-daemon session) 'failed)))))

(ert-deftest compose-preview-a-daemon-dying-mid-compile-falls-back-to-the-cli ()
  (compose-preview-tests--daemon-stubs
    (let ((session (ygg-compose-preview--session-create :daemon nil :busy t)))
      (cl-letf (((symbol-function 'ygg-compose-preview--run)
                 (lambda (_s _n _c callback) (funcall callback 0 ""))))
        (ygg-compose-preview--daemon-refresh session))
      (should (= cli 1)))))

(ert-deftest compose-preview-an-error-in-a-render-ends-the-busy-state ()
  (compose-preview-tests--daemon-stubs
    (let ((session (ygg-compose-preview--session-create :daemon 'failed :started (float-time))))
      (cl-letf (((symbol-function 'ygg-compose-preview--cli-refresh)
                 (lambda (_) (error "Buffer killed"))))
        (ygg-compose-preview--refresh session))
      (should-not (ygg-compose-preview--session-busy session))
      (should (equal (ygg-compose-preview--session-error session) "Buffer killed")))))

(ert-deftest compose-preview-stopping-daemons-keeps-sessions-for-save ()
  (let ((ygg-compose-preview--sessions (make-hash-table :test #'equal))
        (session (ygg-compose-preview--session-create :daemon 'ready)))
    (puthash "/m" session ygg-compose-preview--sessions)
    (ygg-compose-preview--stop-daemons 'stopped)
    (should (eq (gethash "/m" ygg-compose-preview--sessions) session))
    (should (eq (ygg-compose-preview--session-daemon session) 'stopped))))

(defmacro compose-preview-tests--with-sessions (session &rest body)
  "Run BODY with SESSION the only session."
  (declare (indent 1))
  `(let ((ygg-compose-preview--sessions (make-hash-table :test #'equal)))
     (puthash "/m" ,session ygg-compose-preview--sessions)
     ,@body))

(defmacro compose-preview-tests--shown (session &rest body)
  "Run BODY with the preview buffer showing SESSION."
  (declare (indent 1))
  `(let ((buf (get-buffer-create ygg-compose-preview--buffer)))
     (unwind-protect
         (progn (with-current-buffer buf (setq ygg-compose-preview--shown ,session))
                ,@body)
       (kill-buffer buf))))

(ert-deftest compose-preview-stopping-a-starting-daemon-spawns-nothing ()
  (dolist (stop-after '(0 1))
    (let ((session (ygg-compose-preview--session-create :root "/w/" :gradle-path ":app"))
          (callbacks nil) (spawned 0))
      (cl-letf (((symbol-function 'ygg-compose-preview--program) (lambda () "compose-preview"))
                ((symbol-function 'ygg-compose-preview--run)
                 (lambda (_s _n _c callback) (setq callbacks (append callbacks (list callback)))))
                ((symbol-function 'ygg-compose-preview--daemon-spawn)
                 (lambda (_) (cl-incf spawned))))
        (compose-preview-tests--with-sessions session
          (ygg-compose-preview--daemon-start session)
          (when (= stop-after 1) (funcall (pop callbacks) 0 "/init.gradle"))
          (ygg-compose-preview-stop)
          (while callbacks (funcall (pop callbacks) 0 "/init.gradle"))))
      (should (= spawned 0))
      (should (eq (ygg-compose-preview--session-daemon session) 'stopped)))))

(ert-deftest compose-preview-stopping-drops-the-render-waiting-on-the-daemon ()
  (compose-preview-tests--daemon-stubs
    (let* ((session (ygg-compose-preview--session-create :conn 'c :daemon 'ready :busy t
                                                         :ids '("a") :started (float-time)))
           (render-error nil)
           (watchdog nil))
      (cl-letf (((symbol-function 'jsonrpc-async-request)
                 (lambda (_c method _params &rest args)
                   (when (eq method :renderNow) (setq render-error (plist-get args :error-fn)))))
                ((symbol-function 'jsonrpc-connection-p) (lambda (c) (eq c 'c)))
                ((symbol-function 'jsonrpc-running-p) (lambda (c) (eq c 'c)))
                ((symbol-function 'jsonrpc-shutdown)
                 (lambda (conn &rest _)
                   (funcall render-error '(:code -1 :message "Server died"))
                   (ygg-compose-preview--daemon-gone session conn))))
        (ygg-compose-preview--daemon-render session)
        (setq watchdog (ygg-compose-preview--session-watchdog session))
        (should (timerp watchdog))
        (compose-preview-tests--with-sessions session
          (ygg-compose-preview-stop)))
      (should-not (ygg-compose-preview--session-waiting session))
      (should-not (ygg-compose-preview--session-busy session))
      (should-not (ygg-compose-preview--session-watchdog session))
      (should-not (memq watchdog timer-list))
      (should-not (ygg-compose-preview--session-conn session))
      (should (eq (ygg-compose-preview--session-daemon session) 'stopped))
      (should (= cli 0)))))

(ert-deftest compose-preview-no-queued-render-runs-once-the-buffer-is-gone ()
  (compose-preview-tests--daemon-stubs
    (let ((session (ygg-compose-preview--session-create :busy t :pending t :started (float-time)))
          (starts 0))
      (cl-letf (((symbol-function 'ygg-compose-preview--daemon-start) (lambda (_) (cl-incf starts))))
        (ygg-compose-preview--done session "cli"))
      (should (= cli 0))
      (should (= starts 0))
      (should-not (ygg-compose-preview--session-pending session)))))

(ert-deftest compose-preview-a-daemon-that-keeps-dying-is-not-restarted ()
  (compose-preview-tests--daemon-stubs
    (let ((session (ygg-compose-preview--session-create :conn 'c :daemon 'ready :busy t
                                                        :waiting '("a") :started (float-time)))
          (starts 0))
      (cl-letf (((symbol-function 'ygg-compose-preview--daemon-start) (lambda (_) (cl-incf starts))))
        (compose-preview-tests--shown session
          (ygg-compose-preview--daemon-gone session 'c)
          (should (eq (ygg-compose-preview--session-daemon session) 'crashed))
          (should (equal (ygg-compose-preview--session-error session)
                         "warm daemon exited; rendering through the CLI"))
          (dotimes (_ 3)
            (ygg-compose-preview--refresh session)
            (ygg-compose-preview--done session "cli"))))
      (should (= starts 0))
      (should (= cli 3))
      (should (eq (ygg-compose-preview--session-daemon session) 'crashed)))))

(ert-deftest compose-preview-a-crashed-daemon-says-so-in-the-preview ()
  (let ((session (ygg-compose-preview--session-create :daemon 'crashed)))
    (compose-preview-tests--shown session
      (ygg-compose-preview--draw session)
      (should (string-match-p "warm daemon exited"
                              (with-current-buffer ygg-compose-preview--buffer (buffer-string)))))))

(ert-deftest compose-preview-render-ids-follow-the-manifest-over-old-entries ()
  (let ((session (ygg-compose-preview--session-create :previews '((:id "picked")))))
    (cl-letf (((symbol-function 'ygg-compose-preview--file-ids) (lambda (_) '("a" "b"))))
      (should (equal (ygg-compose-preview--render-ids session) '("a" "b"))))))

(ert-deftest compose-preview-pick-stays-local-and-preview-moves-to-leader ()
  (skip-unless (featurep 'yggdrasil-localleader))
  (dolist (mode '(kotlin-ts-mode kotlin-mode))
    (let ((map (gethash mode ygg-localleader--maps)))
      (should-not (lookup-key map "p"))
      (should (eq (lookup-key map "P") #'ygg-compose-preview-pick)))))

;;; compose-preview-tests.el ends here
