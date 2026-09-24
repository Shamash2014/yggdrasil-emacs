;;; device-log-tests.el --- Tests for the live device log -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-lsp)
(require 'ygg-device-log)
(require 'ygg-code-verbs)

(defconst device-log-tests--logcat
  "--------- beginning of main
09-24 10:47:10.558   571   615 W IPCThreadState: Sending oneway calls to frozen process.
09-24 10:47:10.845   571  1752 I ActivityManager: Force stopping com.czk.bliq appid=10224
09-24 10:47:10.853   960   960 D CarrierSvcBindHelper: onHandleForceStop: [com.czk.bliq]
09-24 10:47:10.900  2001  2001 E AndroidRuntime: FATAL EXCEPTION: main
09-24 10:47:10.901  2001  2001 V Chatty  : spam
09-24 10:47:10.902  2001  2001 F libc    : Fatal signal 6
")

(defconst device-log-tests--oslog
  "Timestamp               Ty Process[PID:TID]
2026-09-24 10:47:08.176 Db locationd[44158:260863c] [com.apple.locationd.Position:Position] #vha
2026-09-24 10:47:09.430 Df proactiveeventtrackerd[68245:260984b] PET daemon has launched
2026-09-24 10:47:09.431 I  proactiveeventtrackerd[68245:260984b] (ProactiveSupport) found
2026-09-24 10:47:09.435 E  rnscratch[500:1a] [dev.ygg.rnscratch:net] setting new value (
    \"en-001\"
) for key AppleLanguages
2026-09-24 10:47:09.436 F  rnscratch[500:1a] boom
")

(defmacro device-log-tests--in-buffer (format &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (ygg-device-log-mode)
     (setq ygg-device-log--format ,format
           ygg-device-log--device '(:platform ios :id "92BF" :name "iPhone 18 Pro"))
     (ygg-device-log--update-spec)
     ,@body))

(defun device-log-tests--visible-lines ()
  (let (lines)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (invisible-p (point))
          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines))
        (forward-line 1)))
    (nreverse lines)))

(ert-deftest device-log-parses-logcat-threadtime ()
  (let ((lines (split-string device-log-tests--logcat "\n" t)))
    (should (equal (mapcar (lambda (line) (car (ygg-device-log-parse-line line 'logcat))) lines)
                   '(meta warn info debug error verbose fault)))
    (let ((line (nth 1 lines)))
      (should (equal (cdr (ygg-device-log-parse-line line 'logcat))
                     (list (cons 0 (length "09-24 10:47:10.558   571   615"))))))
    (should-not (ygg-device-log-parse-line "\tat com.x.Main.run(Main.kt:12)" 'logcat))))

(ert-deftest device-log-parses-oslog-compact ()
  (let ((lines (split-string device-log-tests--oslog "\n" t)))
    (should (equal (mapcar (lambda (line) (car (ygg-device-log-parse-line line 'oslog))) lines)
                   '(meta debug info info error nil nil fault)))
    (let* ((line (nth 1 lines))
           (ranges (cdr (ygg-device-log-parse-line line 'oslog))))
      (should (equal (substring line (caar ranges) (cdar ranges)) "2026-09-24 10:47:08.176 Db"))
      (should (equal (substring line (car (cadr ranges)) (cdr (cadr ranges))) "[44158:260863c]")))))

(ert-deftest device-log-parses-idevicesyslog ()
  (let ((parsed (ygg-device-log-parse-line
                 "Sep 24 10:12:33 iPhone rnscratch(Foundation)[1234] <Warning>: slow" 'syslog)))
    (should (eq (car parsed) 'warn))
    (should (equal (cadr parsed) (cons 0 15))))
  (should (eq (car (ygg-device-log-parse-line
                    "Sep 24 10:12:33 iPhone rnscratch[1234] <Notice>: hi" 'syslog))
              'info)))

(ert-deftest device-log-continuation-lines-keep-the-level ()
  (device-log-tests--in-buffer 'oslog
    (ygg-device-log--insert device-log-tests--oslog)
    (ygg-device-log-set-level 'error)
    (should (equal (device-log-tests--visible-lines)
                   '("Timestamp               Ty Process[PID:TID]"
                     "2026-09-24 10:47:09.435 E  rnscratch[500:1a] [dev.ygg.rnscratch:net] setting new value ("
                     "    \"en-001\""
                     ") for key AppleLanguages"
                     "2026-09-24 10:47:09.436 F  rnscratch[500:1a] boom")))))

(ert-deftest device-log-level-threshold-hides-and-shows-again ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log--insert device-log-tests--logcat)
    (should (= (length (device-log-tests--visible-lines)) 7))
    (ygg-device-log-set-level 'warn)
    (should (equal buffer-invisibility-spec '(verbose debug info)))
    (should (equal (mapcar (lambda (line) (substring line 31 32))
                           (cdr (device-log-tests--visible-lines)))
                   '("W" "E" "F")))
    (ygg-device-log-set-level 'verbose)
    (should (= (length (device-log-tests--visible-lines)) 7))
    (should (null buffer-invisibility-spec))))

(ert-deftest device-log-faces-are-weight-and-grey ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log--insert device-log-tests--logcat)
    (should-not font-lock-mode)
    (goto-char (point-min))
    (forward-line 1)
    (should (eq (get-text-property (point) 'face) 'ygg-device-log-meta))
    (should (eq (get-text-property (+ (point) 31) 'face) 'ygg-device-log-warn))
    (forward-line 1)
    (should-not (get-text-property (+ (point) 31) 'face))
    (dolist (face '(ygg-device-log-debug ygg-device-log-warn ygg-device-log-error
                    ygg-device-log-fault ygg-device-log-meta))
      (should (eq (face-attribute face :foreground) 'unspecified))
      (should (eq (face-attribute face :background) 'unspecified))
      (should (memq (face-attribute face :inherit) '(shadow bold))))))

(ert-deftest device-log-text-filter-keeps-or-hides ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log--insert device-log-tests--logcat)
    (ygg-device-log-keep "bliq")
    (should (= (length (device-log-tests--visible-lines)) 2))
    (ygg-device-log--insert "09-24 10:47:11.000  1  1 I X: bliq again\n09-24 10:47:11.000  1  1 I X: other\n")
    (should (= (length (device-log-tests--visible-lines)) 3))
    (ygg-device-log-hide "bliq")
    (should (= (length (device-log-tests--visible-lines)) 6))
    (ygg-device-log-keep "")
    (should (= (length (device-log-tests--visible-lines)) 9))))

(ert-deftest device-log-keeps-a-partial-line-for-the-next-chunk ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log--insert "09-24 10:47:10.558   571   615 W Tag: hal")
    (should (= (buffer-size) 0))
    (ygg-device-log--insert "f\n09-24")
    (should (equal (buffer-string) "09-24 10:47:10.558   571   615 W Tag: half\n"))
    (should (equal ygg-device-log--pending "09-24"))))

(ert-deftest device-log-caps-lines-from-the-top ()
  (device-log-tests--in-buffer 'logcat
    (let ((ygg-device-log-max-lines 10)
          (ygg-device-log-trim-lines 6))
      (dotimes (i 12)
        (ygg-device-log--insert (format "09-24 10:47:10.%03d   1   1 I T: line %d\n" i i)))
      (should (= ygg-device-log--lines (count-lines (point-min) (point-max))))
      (should (<= ygg-device-log--lines 10))
      (goto-char (point-max))
      (forward-line -1)
      (should (string-suffix-p "line 11" (ygg-device-log--line-string)))
      (goto-char (point-min))
      (should (string-suffix-p "line 5" (ygg-device-log--line-string))))))

(ert-deftest device-log-pause-queues-and-flushes ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log-pause)
    (let ((process (make-pipe-process :name "device-log-test" :buffer (current-buffer) :noquery t)))
      (unwind-protect
          (progn
            (setq ygg-device-log--process process)
            (ygg-device-log--filter-output process "09-24 10:47:10.558   1   1 I T: queued\n")
            (should (= (buffer-size) 0))
            (should (> ygg-device-log--queued 0))
            (ygg-device-log-pause)
            (should (string-suffix-p "queued\n" (buffer-string)))
            (should (= ygg-device-log--queued 0)))
        (delete-process process)))))

(ert-deftest device-log-pause-drops-the-oldest-past-the-cap ()
  (device-log-tests--in-buffer 'logcat
    (let ((ygg-device-log-pause-max-chars 10))
      (ygg-device-log--insert "half")
      (setq ygg-device-log--paused t)
      (ygg-device-log--enqueue "aaaaaa\nb")
      (ygg-device-log--enqueue "bbbbb\ncc\n")
      (should (= ygg-device-log--queued 9))
      (should (= ygg-device-log--dropped 8))
      (ygg-device-log-pause)
      (should (equal (buffer-string) "[log 8 chars dropped while paused]\ncc\n")))))

(ert-deftest device-log-notes-and-restarts-leave-a-partial-line-alone ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log--insert "09-24 10:47:10.558   1   1 I T: hal")
    (ygg-device-log--note "restarting")
    (should (equal (buffer-string) "[log restarting]\n"))
    (ygg-device-log--insert "f\n")
    (should (string-suffix-p "T: half\n" (buffer-string)))))

(ert-deftest device-log-an-older-device-answer-is-ignored ()
  (device-log-tests--in-buffer 'android
    (setq ygg-device-log--device '(:platform android :id "emulator-5554" :name "Pixel")
          ygg-device-log--app '(:package "dev.x"))
    (let (callbacks spawned)
      (cl-letf (((symbol-function 'ygg-device-log--resolve)
                 (lambda (_device _app callback) (push callback callbacks)))
                ((symbol-function 'ygg-device-log--spawn)
                 (lambda () (push ygg-device-log--resolved spawned))))
        (ygg-device-log--start)
        (ygg-device-log--start)
        (funcall (cadr callbacks) '(:uid 1))
        (funcall (car callbacks) '(:uid 2))
        (should (equal spawned '((:uid 2))))))))

(ert-deftest device-log-header-shows-percent-signs-as-typed ()
  (device-log-tests--in-buffer 'logcat
    (setq ygg-device-log--filter '(keep . "100%"))
    (should (string-match-p "keep 100%%" (ygg-device-log--header)))
    (should-not (string-match-p "[^%]%[^%]" (ygg-device-log--header)))))

(ert-deftest device-log-follows-only-a-point-at-the-tail ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log--insert "09-24 10:47:10.558   1   1 I T: one\n")
    (should (= (point) (point-max)))
    (goto-char (point-min))
    (ygg-device-log--insert "09-24 10:47:10.558   1   1 I T: two\n")
    (should (= (point) (point-min)))
    (goto-char (point-max))
    (ygg-device-log-follow)
    (ygg-device-log--insert "09-24 10:47:10.558   1   1 I T: three\n")
    (should-not (= (point) (point-max)))))

(ert-deftest device-log-uid-needs-the-exact-package ()
  (let ((output "package:com.softconstruct.vbet.nl uid:10215
package:com.softconstruct.vbet uid:10214
package:com.softconstruct.vbet.kg uid:10211
"))
    (should (= (ygg-device-log-parse-uid output "com.softconstruct.vbet") 10214))
    (should (= (ygg-device-log-parse-uid output "com.softconstruct.vbet.kg") 10211))
    (should-not (ygg-device-log-parse-uid output "com.softconstruct"))
    (should-not (ygg-device-log-parse-uid "" "com.x"))
    (should-not (ygg-device-log-parse-uid nil "com.x"))))

(ert-deftest device-log-reads-the-app-from-project-files ()
  (should (equal (ygg-device-log-parse-gradle-package
                  "android {\n    namespace = \"dev.ygg.ns\"\n    defaultConfig {\n        applicationId = \"dev.ygg.app\"\n")
                 "dev.ygg.app"))
  (should (equal (ygg-device-log-parse-gradle-package "android {\n    namespace 'dev.ygg.ns'\n}")
                 "dev.ygg.ns"))
  (should (equal (ygg-device-log-parse-gradle-package "    applicationId \"com.example.groovy\"\n")
                 "com.example.groovy"))
  (should (equal (ygg-device-log-parse-pbxproj-bundle
                  "PRODUCT_BUNDLE_IDENTIFIER = com.example.app.RunnerTests;
PRODUCT_BUNDLE_IDENTIFIER = \"$(PRODUCT_NAME)\";
PRODUCT_BUNDLE_IDENTIFIER = com.example.app;
PRODUCT_BUNDLE_IDENTIFIER = com.example.app;")
                 "com.example.app"))
  (should (equal (ygg-device-log-parse-appinfo-executable
                  "{\n    CFBundleDisplayName = rnscratch;\n    CFBundleExecutable = rnscratch;\n}")
                 "rnscratch"))
  (should-not (ygg-device-log-parse-appinfo-executable "No such app"))
  (let ((root (file-name-as-directory (make-temp-file "device-log-" t))))
    (unwind-protect
        (progn
          (write-region "{\"expo\": {\"ios\": {\"bundleIdentifier\": \"dev.ygg.rn\"}, \"android\": {\"package\": \"dev.ygg.rn.droid\"}}}"
                        nil (expand-file-name "app.json" root))
          (should (equal (ygg-device-log--project-app root '(:platform android :id "e"))
                         '(:package "dev.ygg.rn.droid")))
          (should (equal (plist-get (ygg-device-log--project-app root '(:platform ios :id "s")) :bundle)
                         "dev.ygg.rn"))
          (make-directory (expand-file-name "android/app" root) t)
          (write-region "android { defaultConfig {\n  applicationId \"dev.ygg.gradle\"\n} }"
                        nil (expand-file-name "android/app/build.gradle" root))
          (should (equal (ygg-device-log--project-app root '(:platform android :id "e"))
                         '(:package "dev.ygg.gradle"))))
      (delete-directory root t))))

(ert-deftest device-log-predicate-quotes-and-joins ()
  (should (equal (ygg-device-log-predicate "rnscratch" "dev.ygg.rnscratch")
                 "process == \"rnscratch\" OR subsystem BEGINSWITH \"dev.ygg.rnscratch\""))
  (should (equal (ygg-device-log-predicate nil "dev.ygg.x") "subsystem BEGINSWITH \"dev.ygg.x\""))
  (should (equal (ygg-device-log-predicate "a\"b\\c" nil) "process == \"a\\\"b\\\\c\""))
  (should-not (ygg-device-log-predicate nil nil)))

(ert-deftest device-log-commands-per-platform ()
  (cl-letf (((symbol-function 'ygg-device--adb) (lambda () "/sdk/adb"))
            ((symbol-function 'ygg-device--xcrun) (lambda () "/usr/bin/xcrun"))
            ((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name))))
    (should (equal (ygg-device-log-command '(:platform android :id "emulator-5554")
                                           '(:package "dev.x") '(:uid 10221))
                   '("/sdk/adb" "-s" "emulator-5554" "logcat" "-v" "threadtime" "-T" "1"
                     "-b" "main,system,crash" "--uid=10221")))
    (should (equal (ygg-device-log-command '(:platform android :id "emulator-5554") '(:package "dev.x") nil)
                   '("/sdk/adb" "-s" "emulator-5554" "logcat" "-v" "threadtime" "-T" "1"
                     "-b" "main,system,crash")))
    (should (equal (ygg-device-log-command '(:platform ios :id "92BF")
                                           '(:bundle "dev.ygg.rn" :exe "Guess") '(:exe "rn"))
                   '("/usr/bin/xcrun" "simctl" "spawn" "92BF" "log" "stream" "--style" "compact"
                     "--level" "debug" "--predicate"
                     "process == \"rn\" OR subsystem BEGINSWITH \"dev.ygg.rn\"")))
    (should (equal (ygg-device-log-command '(:platform ios :id "92BF") nil nil)
                   '("/usr/bin/xcrun" "simctl" "spawn" "92BF" "log" "stream" "--style" "compact"
                     "--level" "debug")))
    (should (equal (ygg-device-log-command '(:platform ios-device :id "0008") '(:bundle "b" :exe "App") nil)
                   '("/bin/idevicesyslog" "-u" "0008" "-K" "-p" "App")))
    (should (equal (car (ygg-device-log-command '(:platform macos :id "mac") nil nil)) "/usr/bin/log"))
    (should-error (ygg-device-log-command '(:platform web :id "chrome") nil nil) :type 'user-error)
    (should (equal (ygg-device-log-clear-command '(:platform android :id "emulator-5554"))
                   '("/sdk/adb" "-s" "emulator-5554" "logcat" "-c")))
    (should-not (ygg-device-log-clear-command '(:platform ios :id "92BF")))))

(ert-deftest device-log-frames-per-language ()
  (let ((frame (lambda (line) (car (ygg-device-log-frames line)))))
    (should (equal (cl-subseq (funcall frame "\tat com.x.Main$1.run(Main.kt:42)") 0 6)
                   '(:kind jvm :path "Main.kt" :line 42)))
    (should (equal (plist-get (funcall frame "at java.lang.Thread.run(Thread.java:1012)") :path) "Thread.java"))
    (let ((swift (funcall frame "Fatal error: bad: file /Users/me/App/ContentView.swift:17:9")))
      (should (equal (list (plist-get swift :kind) (plist-get swift :path) (plist-get swift :line)
                           (plist-get swift :col))
                     '(swift "/Users/me/App/ContentView.swift" 17 9))))
    (should (null (plist-get (funcall frame "precondition at /a/b.swift:3") :col)))
    (let ((dart (funcall frame "#0      main.<anon> (package:myapp/src/home.dart:12:5)")))
      (should (equal (list (plist-get dart :kind) (plist-get dart :path) (plist-get dart :line))
                     '(dart "package:myapp/src/home.dart" 12))))
    (should (equal (plist-get (funcall frame "#1 f (file:///Users/me/app/lib/x.dart:3:1)") :path)
                   "file:///Users/me/app/lib/x.dart"))
    (let ((js (funcall frame "    at App (/Users/me/rn/src/App.tsx:10:4)")))
      (should (equal (list (plist-get js :kind) (plist-get js :path) (plist-get js :line) (plist-get js :col))
                     '(js "/Users/me/rn/src/App.tsx" 10 4))))
    (should-not (ygg-device-log-frames "at anonymous (http://localhost:8081/index.bundle//&platform=ios:1234:56)"))
    (should-not (ygg-device-log-frames "at x (http://localhost:8081/src/App.js:1:2)"))
    (should (= (length (ygg-device-log-frames "a (/x/a.ts:1:2) b (/x/b.js:3:4)")) 2))))

(ert-deftest device-log-dart-package-maps-to-lib ()
  (should (equal (ygg-device-log-dart-path "package:myapp/src/home.dart" "myapp" "/p/")
                 "/p/lib/src/home.dart"))
  (should-not (ygg-device-log-dart-path "package:flutter/src/x.dart" "myapp" "/p/"))
  (should (equal (ygg-device-log-dart-path "file:///p/lib/a%20b.dart" "myapp" "/p/") "/p/lib/a b.dart")))

(ert-deftest device-log-next-frame-skips-hidden-lines ()
  (device-log-tests--in-buffer 'logcat
    (ygg-device-log--insert "09-24 10:47:10.558   1   1 I T: start
09-24 10:47:10.558   1   1 V T: \tat a.B.c(Hidden.kt:1)
09-24 10:47:10.558   1   1 E T: \tat a.B.c(Shown.kt:2)
")
    (ygg-device-log-set-level 'info)
    (goto-char (point-min))
    (ygg-device-log-next-frame)
    (should (looking-at "at a.B.c(Shown.kt:2)"))
    (should-error (ygg-device-log-next-frame) :type 'user-error)
    (ygg-device-log-set-level 'verbose)
    (ygg-device-log-previous-frame)
    (should (looking-at "at a.B.c(Hidden.kt:1)"))))

(ert-deftest device-log-localleader-and-leader-keys ()
  (pcase-dolist (`(,key . ,command)
                 '(("l" . ygg-device-log-set-level) ("f" . ygg-device-log-keep)
                   ("h" . ygg-device-log-hide) ("p" . ygg-device-log-pause)
                   ("t" . ygg-device-log-follow) ("c" . ygg-device-log-clear)
                   ("r" . ygg-device-log-restart) ("s" . ygg-device-log-save)
                   ("n" . ygg-device-log-next-frame) ("N" . ygg-device-log-previous-frame)))
    (should (eq (lookup-key (ygg-localleader--get-map 'ygg-device-log-mode) (kbd key)) command)))
  (should (eq (lookup-key ygg-device-log-mode-map (kbd "RET")) 'ygg-device-log-visit))
  (should (eq (lookup-key ygg-device-log-mode-map [remap ygg-goto-file]) 'ygg-device-log-visit))
  (should (memq 'ygg-device-log-mode ygg-modal-special-modes))
  (should (eq (lookup-key ygg-leader-code-map "L") 'ygg-code-logs)))

(ert-deftest device-log-code-verb-per-family ()
  (let ((default-directory temporary-file-directory))
    (should (eq (ygg-code-verbs-resolve 'logs 'dart-mode) 'ygg-device-log))
    (should (eq (ygg-code-verbs-resolve 'logs 'kotlin-ts-mode) 'ygg-device-log))
    (should (eq (ygg-code-verbs-resolve 'logs 'swift-ts-mode) 'ygg-device-log))
    (should-not (ygg-code-verbs-resolve 'logs 'python-ts-mode))))

(ert-deftest device-log-code-verb-outside-a-project-mode-streams-the-whole-device ()
  (let (called)
    (cl-letf (((symbol-function 'ygg-device-log)
               (lambda (&optional whole) (interactive "P") (setq called (list 'whole whole)))))
      (with-temp-buffer
        (fundamental-mode)
        (ygg-code-logs)))
    (should (equal called '(whole unless-streaming)))))

;;; device-log-tests.el ends here
