;;; device-capture-tests.el --- Tests for device screenshots and recordings -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-lsp)
(require 'ygg-device-capture)
(require 'ygg-code-verbs)

(defconst device-capture-tests--sim '(:platform ios :id "92BF" :name "iPhone 18 Pro"))
(defconst device-capture-tests--phone '(:platform ios-device :id "00008" :name "Real Phone"))
(defconst device-capture-tests--emulator '(:platform android :id "emulator-5554" :name "Pixel_9"))
(defconst device-capture-tests--pixel '(:platform android :id "R58M" :name "SM G973F"))

(defmacro device-capture-tests--with-env (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "device-capture-" t)))
          (default-directory root)
          (ygg-device-capture-directory "captures")
          (ygg-device-capture--recording nil)
          (ygg-device-capture--reserved nil)
          (calls nil)
          (ygg-device-run-function
           (lambda (program args _timeout callback)
             (push (cons program args) calls)
             (funcall callback "")))
          (kill-ring nil))
     (cl-letf (((symbol-function 'ygg-device--adb) (lambda () "/sdk/adb"))
               ((symbol-function 'ygg-device--xcrun) (lambda () "/usr/bin/xcrun"))
               ((symbol-function 'project-current) (lambda (&rest _) nil)))
       (unwind-protect (progn ,@body)
         (delete-directory root t)))))

(ert-deftest device-capture-file-names-carry-device-and-time ()
  (should (equal (ygg-device-capture-file-name device-capture-tests--sim "png" 0)
                 (format-time-string "iPhone-18-Pro-%Y%m%d-%H%M%S.png" 0)))
  (should (string-prefix-p "device-" (ygg-device-capture-file-name '(:name nil) "mp4"))))

(ert-deftest device-capture-screenshot-commands ()
  (cl-letf (((symbol-function 'ygg-device--adb) (lambda () "/sdk/adb"))
            ((symbol-function 'ygg-device--xcrun) (lambda () "/usr/bin/xcrun")))
    (should (equal (ygg-device-screenshot-command device-capture-tests--sim "/p/a.png")
                   '("/usr/bin/xcrun" "simctl" "io" "92BF" "screenshot" "/p/a.png")))
    (should (equal (ygg-device-screenshot-command device-capture-tests--phone "/p/a.png")
                   '("/usr/bin/xcrun" "devicectl" "device" "capture" "screenshot"
                     "--device" "00008" "--destination" "/p/a.png" "-q")))
    (should (equal (ygg-device-screenshot-command '(:platform macos :id "mac") "/p/a.png")
                   '("/usr/sbin/screencapture" "-x" "/p/a.png")))
    (should (equal (ygg-device-screenshot-command device-capture-tests--pixel "/p/a b.png")
                   '("/bin/sh" "-c" "/sdk/adb -s R58M exec-out screencap -p > /p/a\\ b.png")))
    (let ((script (nth 2 (ygg-device-screenshot-command device-capture-tests--emulator "/p/a.png"))))
      (should (string-match "\\`/sdk/adb -s emulator-5554 emu screenrecord screenshot \\(\\S-+\\) && mv \\1/\\*\\.png /p/a\\.png ; rm -rf \\1 ; test -s /p/a\\.png || /sdk/adb -s emulator-5554 exec-out screencap -p > /p/a\\.png\\'"
                                script))
      (delete-directory (match-string 1 script)))
    (should-error (ygg-device-screenshot-command '(:platform web :id "chrome") "/p/a.png")
                  :type 'user-error)))

(ert-deftest device-capture-record-commands ()
  (cl-letf (((symbol-function 'ygg-device--adb) (lambda () "/sdk/adb"))
            ((symbol-function 'ygg-device--xcrun) (lambda () "/usr/bin/xcrun")))
    (should (equal (ygg-device-record-command device-capture-tests--sim "/p/a.mov")
                   '("/usr/bin/xcrun" "simctl" "io" "92BF" "recordVideo" "--codec=h264" "--force" "/p/a.mov")))
    (should (equal (ygg-device-record-command device-capture-tests--phone "/p/a.mp4")
                   '("/usr/bin/xcrun" "devicectl" "device" "capture" "screen-record"
                     "--device" "00008" "--destination" "/p/a.mp4" "--codec" "h264" "-q")))
    (should (equal (ygg-device-record-command device-capture-tests--emulator "/p/a.mp4" "/sdcard/ygg-a.mp4" t)
                   '("/sdk/adb" "-s" "emulator-5554" "shell"
                     "echo $$; exec screenrecord --time-limit 0 /sdcard/ygg-a.mp4")))
    (should (equal (car (last (ygg-device-record-command device-capture-tests--pixel "/p/a.mp4" "/sdcard/ygg-a.mp4")))
                   "echo $$; exec screenrecord /sdcard/ygg-a.mp4"))
    (should-error (ygg-device-record-command '(:platform macos :id "mac") "/p/a.mov") :type 'user-error)))

(ert-deftest device-capture-parses-pid-and-time-limit ()
  (should (= (ygg-device-capture-parse-pid "12345\r\n") 12345))
  (should-not (ygg-device-capture-parse-pid ""))
  (should-not (ygg-device-capture-parse-pid nil))
  (should (ygg-device-capture-parse-unlimited "Default is 180. Set to 0\n    to remove the time limit."))
  (should-not (ygg-device-capture-parse-unlimited "Default is 180, max is 180.")))

(ert-deftest device-capture-screenshot-saves-copies-and-shows ()
  (device-capture-tests--with-env
    (let (shown)
      (cl-letf (((symbol-function 'ygg-device-require) (lambda () device-capture-tests--sim))
                ((symbol-function 'ygg-device-capture--show-image) (lambda (file) (setq shown file)))
                (ygg-device-run-function
                 (lambda (program args _timeout callback)
                   (push (cons program args) calls)
                   (write-region "png" nil (car (last args)))
                   (funcall callback ""))))
        (ygg-device-screenshot))
      (should (string-match-p "\\`captures/iPhone-18-Pro-[0-9]\\{8\\}-[0-9]\\{6\\}\\.png\\'"
                              (file-relative-name shown root)))
      (should (equal (car kill-ring) shown))
      (should (equal (car calls) (list "/usr/bin/xcrun" "simctl" "io" "92BF" "screenshot" shown))))))

(ert-deftest device-capture-screenshot-failure-shows-nothing ()
  (device-capture-tests--with-env
    (let (shown)
      (cl-letf (((symbol-function 'ygg-device-require) (lambda () device-capture-tests--sim))
                ((symbol-function 'ygg-device-capture--show-image) (lambda (file) (setq shown file))))
        (ygg-device-screenshot))
      (should-not shown)
      (should-not kill-ring))))

(ert-deftest device-capture-failed-screenshot-leaves-no-empty-file ()
  (device-capture-tests--with-env
    (let (said)
      (cl-letf (((symbol-function 'ygg-device-require) (lambda () device-capture-tests--sim))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format-message format args))))
                (ygg-device-run-function
                 (lambda (_program args _timeout callback)
                   (write-region "" nil (car (last args)))
                   (funcall callback "No devices are booted."))))
        (ygg-device-screenshot))
      (should (equal (directory-files (expand-file-name "captures" root) nil "\\.png\\'") nil))
      (should (equal said "Screenshot of iPhone 18 Pro failed: No devices are booted.")))))

(ert-deftest device-capture-unsupported-target-leaves-no-folder ()
  (device-capture-tests--with-env
    (cl-letf (((symbol-function 'ygg-device-require) (lambda () '(:platform web :id "chrome" :name "Chrome"))))
      (should-error (ygg-device-screenshot) :type 'user-error)
      (should-error (ygg-device-record-toggle) :type 'user-error))
    (should-not (file-exists-p (expand-file-name "captures" root)))))

(ert-deftest device-capture-new-files-never-overwrite ()
  (device-capture-tests--with-env
    (let* ((ygg-device-capture--reserved nil)
           (first (car (ygg-device-capture--new-file device-capture-tests--sim "png")))
           (second (car (ygg-device-capture--new-file device-capture-tests--sim "png"))))
      (should-not (equal first second))
      (ygg-device-capture--release first)
      (ygg-device-capture--release second)
      (write-region "" nil first)
      (should-not (equal first (car (ygg-device-capture--new-file device-capture-tests--sim "png")))))))

(ert-deftest device-capture-record-toggle-shows-rec-and-finishes ()
  (device-capture-tests--with-env
    (let ((script (expand-file-name "rec.sh" root)))
      (write-region "trap 'echo done > \"$1\"; exit 0' INT; while :; do sleep 0.05; done" nil script)
      (cl-letf (((symbol-function 'ygg-device-require) (lambda () device-capture-tests--sim))
                ((symbol-function 'ygg-device-record-command)
                 (lambda (_device file &rest _) (list "/bin/sh" script file))))
        (with-temp-buffer
          (setq major-mode 'dart-mode)
          (should-not (ygg-device-capture-modeline))
          (ygg-code-record)
          (fundamental-mode)
          (should (ygg-device-capture-recording-p))
          (should (equal (ygg-device-capture-modeline) "  REC"))
          (should-not (text-properties-at 0 (ygg-device-capture-modeline)))
          (sleep-for 0.3)
          (let ((file (plist-get ygg-device-capture--recording :file)))
            (should (string-suffix-p ".mov" file))
            (ygg-code-record)
            (with-timeout (5 (ert-fail "recording did not stop"))
              (while (ygg-device-capture-recording-p) (accept-process-output nil 0.05)))
            (should (ygg-device-capture--saved-p file))
            (should (equal (car kill-ring) file))))))))

(ert-deftest device-capture-android-stop-signals-the-remote-pid-then-pulls ()
  (device-capture-tests--with-env
    (let* ((file (expand-file-name "a.mp4" root))
           (ygg-device-run-function
            (lambda (program args _timeout callback)
              (push (cons program args) calls)
              (let ((pull (string-match-p " pull " (car (last args)))))
                (when pull (write-region "mp4" nil file))
                (funcall callback (if pull "/sdcard/ygg-a.mp4: 1 file pulled, 0 skipped." "")))))
           (process (make-process :name "fake-adb-shell" :command '("/bin/sh" "-c" "sleep 5") :noquery t))
           (recording (list :device device-capture-tests--emulator :file file
                            :remote "/sdcard/ygg-a.mp4" :pid 4242 :process process
                            :stopping nil :unlimited t :output "")))
      (setq ygg-device-capture--recording recording)
      (ygg-device-record-toggle)
      (should (member '("/sdk/adb" "-s" "emulator-5554" "shell" "kill" "-INT" "4242") calls))
      (delete-process process)
      (ygg-device-capture--finished recording)
      (should-not ygg-device-capture--recording)
      (should (member (ygg-device-capture-pull-command "emulator-5554" "/sdcard/ygg-a.mp4" file) calls))
      (should (member '("/sdk/adb" "-s" "emulator-5554" "shell" "rm" "-f" "/sdcard/ygg-a.mp4") calls))
      (should (equal (car kill-ring) file)))))

(ert-deftest device-capture-failed-pull-keeps-the-device-copy ()
  (device-capture-tests--with-env
    (let* ((file (expand-file-name "a.mp4" root))
           (recording (list :device device-capture-tests--emulator :file file
                            :remote "/sdcard/ygg-a.mp4" :pid 4242 :process nil
                            :stopping t :unlimited t :output "" :pull-failed nil :exiting nil)))
      (write-region "partial" nil file)
      (ygg-device-capture--finished recording)
      (should (assoc "/bin/sh" calls))
      (should-not (seq-find (lambda (call) (member "rm" call)) calls))
      (should-not kill-ring))))

(ert-deftest device-capture-android-waits-for-its-pid-before-stopping ()
  (device-capture-tests--with-env
    (let ((process (make-process :name "fake-adb-shell" :command '("/bin/sh" "-c" "sleep 5") :noquery t)))
      (unwind-protect
          (progn
            (setq ygg-device-capture--recording
                  (list :device device-capture-tests--emulator :file "/p/a.mp4"
                        :remote "/sdcard/ygg-a.mp4" :pid nil :process process :stopping nil))
            (ygg-device-record-toggle)
            (should (process-live-p process))
            (should-not (plist-get ygg-device-capture--recording :stopping))
            (setq ygg-device-capture--recording (list :device device-capture-tests--emulator :starting t))
            (ygg-device-record-toggle)
            (should (plist-get ygg-device-capture--recording :starting))
            (should-not calls))
        (delete-process process)))))

(ert-deftest device-capture-pull-reads-stderr ()
  (cl-letf (((symbol-function 'ygg-device--adb) (lambda () "/sdk/adb")))
    (should (equal (ygg-device-capture-pull-command "emulator-5554" "/sdcard/a.mp4" "/p/a b.mp4")
                   '("/bin/sh" "-c" "/sdk/adb -s emulator-5554 pull /sdcard/a.mp4 /p/a\\ b.mp4 2>&1")))))

(ert-deftest device-capture-pull-output-is-read ()
  (should (ygg-device-capture-pulled-p "/sdcard/x.mp4: 1 file pulled, 0 skipped. 30 MB/s"))
  (should-not (ygg-device-capture-pulled-p "adb: error: failed to stat remote object"))
  (should-not (ygg-device-capture-pulled-p nil)))

(ert-deftest device-capture-verbs-resolve-for-every-mobile-language ()
  (let ((default-directory temporary-file-directory))
    (dolist (mode '(dart-mode kotlin-ts-mode java-ts-mode swift-ts-mode))
      (should (eq (ygg-code-verbs-resolve 'screenshot mode) 'ygg-device-screenshot))
      (should (eq (ygg-code-verbs-resolve 'record mode) 'ygg-device-record-toggle)))
    (cl-letf (((symbol-function 'ygg-rn-root) (lambda () "/rn/")))
      (should (eq (ygg-code-verbs-resolve 'screenshot 'tsx-ts-mode) 'ygg-device-screenshot))
      (should (eq (ygg-code-verbs-resolve 'record 'tsx-ts-mode) 'ygg-device-record-toggle)))
    (should-not (ygg-code-verbs-resolve 'screenshot 'python-ts-mode))))

(ert-deftest device-capture-keys-sit-on-the-code-leader ()
  (should (eq (lookup-key ygg-leader-code-map "y") 'ygg-code-screenshot))
  (should (eq (lookup-key ygg-leader-code-map "v") 'ygg-code-record)))

;;; device-capture-tests.el ends here
