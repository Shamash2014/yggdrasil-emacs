;;; device-tests.el --- Tests for the shared device selection -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dape)
(require 'ygg-device)
(require 'yggdrasil-localleader)
(require 'ygg-code-verbs)
(require 'ygg-dap-java)

(defconst device-tests--adb
  "* daemon not running; starting now at tcp:5037
List of devices attached
emulator-5556          device product:sdk_gphone64_arm64 model:sdk_gphone64_arm64 device:emu64a transport_id:13
R58M123ABC             device usb:1-1 product:beyond1 model:SM_G973F device:beyond1 transport_id:2
emulator-5558          offline transport_id:4

")

(defconst device-tests--simctl
  "{\"devices\" : {
  \"com.apple.CoreSimulator.SimRuntime.iOS-27-0\" : [
    {\"udid\" : \"92BF\", \"isAvailable\" : true, \"state\" : \"Booted\", \"name\" : \"iPhone 18 Pro\"},
    {\"udid\" : \"AF28\", \"isAvailable\" : true, \"state\" : \"Shutdown\", \"name\" : \"iPhone Air\"},
    {\"udid\" : \"DEAD\", \"isAvailable\" : false, \"state\" : \"Shutdown\", \"name\" : \"iPhone Old\"}],
  \"com.apple.CoreSimulator.SimRuntime.watchOS-12-0\" : [
    {\"udid\" : \"WA7C\", \"isAvailable\" : true, \"state\" : \"Shutdown\", \"name\" : \"Apple Watch\"}]}}")

(defconst device-tests--devicectl
  "{\"info\" : {}, \"result\" : {\"devices\" : [
  {\"identifier\" : \"41A1\", \"connectionProperties\" : {\"pairingState\" : \"paired\", \"tunnelState\" : \"disconnected\"},
   \"deviceProperties\" : {\"name\" : \"Roman's iPad\"},
   \"hardwareProperties\" : {\"platform\" : \"iOS\", \"reality\" : \"physical\", \"udid\" : \"00008030-000D\"}},
  {\"identifier\" : \"92BF\", \"connectionProperties\" : {\"pairingState\" : \"paired\", \"tunnelState\" : \"connected\"},
   \"deviceProperties\" : {\"name\" : \"iPhone 18 Pro\"},
   \"hardwareProperties\" : {\"platform\" : \"iOS\", \"reality\" : \"simulated\", \"udid\" : \"92BF\"}}]}}")

(defconst device-tests--flutter
  "Resolving dependencies...
[
  {\"name\": \"sdk gphone64 arm64\", \"id\": \"emulator-5556\", \"isSupported\": true, \"targetPlatform\": \"android-arm64\", \"emulator\": true},
  {\"name\": \"iPhone 18 Pro\", \"id\": \"92BF\", \"isSupported\": true, \"targetPlatform\": \"ios\", \"emulator\": true},
  {\"name\": \"Roman's iPad\", \"id\": \"00008030-000D\", \"isSupported\": true, \"targetPlatform\": \"ios\", \"emulator\": false},
  {\"name\": \"macOS\", \"id\": \"macos\", \"isSupported\": true, \"targetPlatform\": \"darwin\", \"emulator\": false},
  {\"name\": \"Chrome\", \"id\": \"chrome\", \"isSupported\": true, \"targetPlatform\": \"web-javascript\", \"emulator\": false}
]")

(defun device-tests--answers (program args)
  "What each listing prints, keyed on PROGRAM and ARGS."
  (let ((line (string-join (cons (file-name-nondirectory program) args) " ")))
    (cond ((equal line "adb devices -l") device-tests--adb)
          ((equal line "adb -s emulator-5556 emu avd name") "masterapp_dev2\r\nOK\r\n")
          ((string-prefix-p "emulator -list-avds" line) "INFO    | Storing crashdata\nPixel_9\nmasterapp_dev2\n")
          ((string-prefix-p "xcrun simctl list" line) device-tests--simctl)
          ((string-prefix-p "xcrun devicectl" line) device-tests--devicectl)
          ((string-prefix-p "flutter devices" line) device-tests--flutter))))

(defmacro device-tests--with-world (&rest body)
  "BODY with a fresh selection store and cache, tools answering from fixtures."
  (declare (indent 0))
  `(let* ((ygg-device-selections nil)
          (ygg-device--listings nil)
          (ygg-device--pending nil)
          (runs nil)
          (starts nil)
          (ygg-device-run-function
           (lambda (program args _timeout callback)
             (push (cons program args) runs)
             (funcall callback (device-tests--answers (or program "missing") args))))
          (ygg-device-start-function (lambda (&rest command) (push command starts)))
          (ygg-device-android-sdk "/sdk")
          (ygg-device-picker-wait 0))
     (cl-letf (((symbol-function 'ygg-device--adb) (lambda () "/sdk/platform-tools/adb"))
               ((symbol-function 'ygg-device--emulator) (lambda () "/sdk/emulator/emulator"))
               ((symbol-function 'ygg-device--xcrun) (lambda () "/usr/bin/xcrun"))
               ((symbol-function 'ygg-device--flutter) (lambda () "/mise/flutter"))
               ((symbol-function 'run-at-time) #'ignore))
       (with-temp-buffer
         (setq ygg-device--buffer-key "/work/app/")
         ,@body))))

(defun device-tests--find (devices platform key value)
  (seq-find (lambda (device) (and (eq (plist-get device :platform) platform)
                                  (equal (plist-get device key) value)))
            devices))

(ert-deftest device-parse-adb-reads-state-model-and-emulators ()
  (let ((devices (ygg-device-parse-adb device-tests--adb)))
    (should (equal (mapcar (lambda (d) (plist-get d :id)) devices)
                   '("emulator-5556" "R58M123ABC" "emulator-5558")))
    (should (equal (mapcar (lambda (d) (plist-get d :state)) devices) '("booted" "connected" "offline")))
    (should (equal (plist-get (nth 1 devices) :name) "SM G973F"))
    (should-not (plist-get (nth 1 devices) :emulator))))

(ert-deftest device-parse-avds-and-avd-name ()
  (should (equal (ygg-device-parse-avds "INFO    | Storing crashdata\nPixel_9\nmasterapp_dev2\n")
                 '("Pixel_9" "masterapp_dev2")))
  (should (equal (ygg-device-parse-avd-name "Pixel_9\r\nOK\r\n") "Pixel_9"))
  (should-not (ygg-device-parse-avd-name "KO: unknown command\r\n"))
  (should-not (ygg-device-parse-avd-name nil)))

(ert-deftest device-parse-simctl-keeps-available-ios ()
  (let ((sims (ygg-device-parse-simctl device-tests--simctl)))
    (should (equal (mapcar (lambda (d) (plist-get d :name)) sims) '("iPhone 18 Pro" "iPhone Air")))
    (should (equal (plist-get (car sims) :state) "booted"))
    (should (equal (plist-get (cadr sims) :state) "shutdown"))
    (should (equal (plist-get (car sims) :os) "iOS 27.0"))
    (should (equal (plist-get (car sims) :flutter-id) "92BF"))))

(ert-deftest device-parse-devicectl-keeps-physical-with-flutter-udid ()
  (let ((devices (ygg-device-parse-devicectl device-tests--devicectl)))
    (should (= (length devices) 1))
    (should (equal (car devices) '(:platform ios-device :id "00008030-000D" :flutter-id "00008030-000D"
                                   :name "Roman's iPad" :state "available")))))

(ert-deftest device-parse-flutter-maps-platforms-past-a-banner ()
  (let ((devices (ygg-device-parse-flutter device-tests--flutter)))
    (should (equal (mapcar (lambda (d) (plist-get d :platform)) devices)
                   '(android ios ios-device macos web)))))

(ert-deftest device-merge-offline-avds-and-flutter-only-targets ()
  (device-tests--with-world
    (ygg-device-refresh)
    (should-not ygg-device--pending)
    (let ((devices (ygg-device-devices)))
      (should (equal (plist-get (device-tests--find devices 'android :id "emulator-5556") :avd) "masterapp_dev2"))
      (should (equal (plist-get (device-tests--find devices 'android :avd "Pixel_9") :state) "offline"))
      (should (= (seq-count (lambda (d) (equal (plist-get d :avd) "masterapp_dev2")) devices) 1))
      (should (device-tests--find devices 'macos :id "macos"))
      (should (device-tests--find devices 'web :id "chrome"))
      (should (= (seq-count (lambda (d) (equal (plist-get d :flutter-id) "92BF")) devices) 1))
      (should (equal (seq-uniq (mapcar (lambda (d) (plist-get d :platform)) devices))
                     '(android ios ios-device macos web))))))

(ert-deftest device-merge-keeps-the-last-listing-when-a-source-fails ()
  (device-tests--with-world
    (ygg-device-refresh)
    (let ((ygg-device-run-function (lambda (_p _a _t callback) (funcall callback nil))))
      (ygg-device-refresh))
    (should (device-tests--find (ygg-device-devices) 'macos :id "macos"))))

(ert-deftest device-picker-groups-annotates-and-selects ()
  (device-tests--with-world
    (let (table)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq table collection)
                   "iPhone 18 Pro  iOS 27.0")))
        (should (equal (ygg-device-pick)
                       '(:platform ios :id "92BF" :flutter-id "92BF" :name "iPhone 18 Pro"))))
      (let* ((metadata (cdr (funcall table "" nil 'metadata)))
             (group (alist-get 'group-function metadata))
             (annotate (alist-get 'annotation-function metadata))
             (candidates (all-completions "" table)))
        (should (member "masterapp_dev2  emulator-5556" candidates))
        (should (member "Pixel_9" candidates))
        (should (member "macOS" candidates))
        (should (equal (funcall group "Pixel_9" nil) "Android"))
        (should (equal (funcall group "iPhone Air  iOS 27.0" nil) "iOS simulator"))
        (should (equal (funcall group "Roman's iPad" nil) "iOS device"))
        (should (equal (substring-no-properties (funcall annotate "Pixel_9")) "  offline"))
        (should (eq (get-text-property 2 'face (funcall annotate "Pixel_9")) 'shadow))))
    (should-not starts)))

(ert-deftest device-selection-is-per-project-with-global-fallback ()
  (device-tests--with-world
    (ygg-device-select '(:platform android :id "emulator-5556" :flutter-id "emulator-5556"
                         :name "masterapp_dev2" :avd "masterapp_dev2" :state "booted"))
    (should (equal (ygg-device-android-serial) "emulator-5556"))
    (let ((ygg-device--buffer-key "/work/other/"))
      (should (equal (plist-get (ygg-device-current) :id) "emulator-5556"))
      (ygg-device-select '(:platform ios :id "92BF" :flutter-id "92BF" :name "iPhone 18 Pro" :state "booted"))
      (should (equal (plist-get (ygg-device-current) :id) "92BF"))
      (should-not (ygg-device-android-serial)))
    (should (equal (plist-get (ygg-device-current) :id) "emulator-5556"))
    (let ((ygg-device--buffer-key nil))
      (should (equal (plist-get (ygg-device-current) :id) "92BF")))))

(ert-deftest device-selection-runs-the-changed-hook ()
  (device-tests--with-world
    (let* ((seen nil)
           (ygg-device-changed-hook (list (lambda () (push (ygg-device-current) seen)))))
      (ygg-device-select '(:platform macos :id "macos" :flutter-id "macos" :name "macOS" :state "available"))
      (should (equal (plist-get (car seen) :flutter-id) "macos")))))

(ert-deftest device-selecting-an-offline-avd-boots-it-and-fills-the-serial ()
  (device-tests--with-world
    (let ((hooked 0))
      (add-hook 'ygg-device-changed-hook (lambda () (cl-incf hooked)) nil t)
      (let ((device-tests--adb
             (concat device-tests--adb "emulator-5554          device product:x model:sdk_gphone64_arm64\n")))
        (cl-letf (((symbol-function 'device-tests--answers)
                   (let ((answers (symbol-function 'device-tests--answers)))
                     (lambda (program args)
                       (if (equal args '("-s" "emulator-5554" "emu" "avd" "name"))
                           "Pixel_9\r\nOK\r\n"
                         (funcall answers program args))))))
          (ygg-device-select '(:platform android :id nil :flutter-id nil :name "Pixel_9"
                               :avd "Pixel_9" :state "offline"))))
      (should (equal starts '(("/sdk/emulator/emulator" "-avd" "Pixel_9" "-no-snapshot-save"))))
      (should (equal (ygg-device-current)
                     '(:platform android :id "emulator-5554" :flutter-id "emulator-5554"
                       :name "Pixel_9" :avd "Pixel_9")))
      (should (= hooked 2)))))

(ert-deftest device-require-refuses-a-device-still-booting ()
  (device-tests--with-world
    (setq ygg-device-selections '((t :platform android :id nil :name "Pixel_9" :avd "Pixel_9")))
    (should-error (ygg-device-require) :type 'user-error)))

(ert-deftest device-selecting-a-shutdown-simulator-boots-it ()
  (device-tests--with-world
    (ygg-device-select '(:platform ios :id "AF28" :flutter-id "AF28" :name "iPhone Air" :state "shutdown"))
    (should (member '("/usr/bin/xcrun" "simctl" "boot" "AF28") runs))
    (should (equal starts '(("/usr/bin/open" "-a" "Simulator"))))))

(ert-deftest device-simulator-app-is-opened-only-when-not-running ()
  (device-tests--with-world
    (cl-letf (((symbol-function 'device-tests--answers)
               (lambda (program _args) (and (equal program "/usr/bin/pgrep") "4242\n"))))
      (ygg-device-select '(:platform ios :id "AF28" :flutter-id "AF28" :name "iPhone Air" :state "shutdown")))
    (should-not starts)))

(ert-deftest device-require-prompts-when-nothing-is-selected ()
  (device-tests--with-world
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "macOS")))
      (should (equal (ygg-device-flutter-id) "macos")))
    (should (equal (plist-get (ygg-device-current) :name) "macOS"))))

(ert-deftest device-flutter-run-passes-the-selected-id ()
  (device-tests--with-world
    (setq ygg-device-selections '((t :platform android :id "emulator-5556" :flutter-id "emulator-5556" :name "masterapp_dev2")))
    (should (equal (ygg-localleader--flutter-run-command) "flutter run -d emulator-5556"))))

(ert-deftest device-dape-flutter-entry-launches-on-the-selection ()
  (device-tests--with-world
    (setq ygg-device-selections '((t :platform ios :id "92BF" :flutter-id "92BF" :name "iPhone 18 Pro")))
    (let* ((dape-configs (copy-tree dape-configs))
           (entry (assq 'flutter dape-configs)))
      (should (equal (plist-get (cdr entry) :toolArgs) ["-d" "all"]))
      (ygg-device-dape-flutter entry)
      (ygg-device-dape-flutter entry)
      (let ((config (dape--config-eval 'flutter nil)))
        (should (equal (plist-get config :toolArgs) ["-d" "92BF"]))
        (should (equal (plist-get config :deviceId) "92BF"))
        (should (equal (plist-get config 'command) "flutter"))))))

(ert-deftest device-gradle-install-targets-the-selected-serial ()
  (device-tests--with-world
    (setq ygg-device-selections '((t :platform android :id "emulator-5556" :flutter-id "emulator-5556" :name "masterapp_dev2")))
    (let (seen)
      (cl-letf (((symbol-function 'ygg-localleader--compile)
                 (lambda (command) (push (cons command (getenv "ANDROID_SERIAL")) seen)))
                ((symbol-function 'ygg-localleader--root) (lambda () temporary-file-directory)))
        (ygg-localleader--gradle "installDebug")
        (ygg-localleader--gradle "build"))
      (should (equal (cdr (assoc "gradle installDebug" seen)) "emulator-5556"))
      (should-not (equal (cdr (assoc "gradle build" seen)) "emulator-5556")))))

(ert-deftest device-java-adb-uses-the-selection-without-asking ()
  (device-tests--with-world
    (setq ygg-device-selections '((t :platform android :id "emulator-5556" :flutter-id "emulator-5556" :name "masterapp_dev2")))
    (let ((ygg-dap-java-adb-function
           (lambda (&rest _) "List of devices attached\nemulator-5554\tdevice\nemulator-5556\tdevice\n")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Prompted"))))
        (should (equal (ygg-dap-java-adb-serial) "emulator-5556"))))
    (setq ygg-device-selections '((t :platform ios :id "92BF" :flutter-id "92BF" :name "iPhone 18 Pro")))
    (let ((ygg-dap-java-adb-function (lambda (&rest _) "List of devices attached\nemulator-5554\tdevice\n")))
      (should (equal (ygg-dap-java-adb-serial) "emulator-5554")))))

(ert-deftest device-modeline-shows-the-selection-in-device-modes-only ()
  (device-tests--with-world
    (setq ygg-device-selections '((t :platform ios :id "92BF" :flutter-id "92BF" :name "iPhone 18 Pro")))
    (should-not (ygg-device-modeline))
    (let ((major-mode 'kotlin-ts-mode))
      (should (equal (substring-no-properties (ygg-device-modeline)) "  iPhone 18 Pro"))
      (should (eq (get-text-property 2 'face (ygg-device-modeline)) 'shadow)))))

(ert-deftest device-key-is-on-the-code-leader-for-every-device-mode ()
  (should (eq (lookup-key ygg-leader-code-map "m") #'ygg-code-device))
  (dolist (mode ygg-device-modes)
    (should-not (lookup-key (ygg-localleader--get-map mode) "@"))
    (should (eq (ygg-code-verbs-resolve 'device mode) #'ygg-device-pick)))
  (should-not (ygg-code-verbs-resolve 'device 'python-ts-mode)))

(provide 'device-tests)
;;; device-tests.el ends here
