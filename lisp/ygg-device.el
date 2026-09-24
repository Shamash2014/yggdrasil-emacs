;;; ygg-device.el --- One device selection for Flutter, Android and iOS -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)

(defvar savehist-additional-variables)

(defgroup ygg-device nil
  "The device Flutter, Android and iOS runs and debug sessions target."
  :group 'tools
  :prefix "ygg-device-")

(defcustom ygg-device-android-sdk (expand-file-name "~/Library/Android/sdk")
  "The Android SDK holding adb and the emulator."
  :type 'directory)

(defcustom ygg-device-list-timeout 8
  "Seconds a device listing may run before it is killed."
  :type 'number)

(defcustom ygg-device-flutter-timeout 30
  "Seconds flutter devices may run before it is killed."
  :type 'number)

(defcustom ygg-device-picker-wait 3
  "Seconds the picker waits for listings still running before it opens."
  :type 'number)

(defcustom ygg-device-boot-timeout 240
  "Seconds to watch adb for an emulator that was asked to boot."
  :type 'number)

(defconst ygg-device-modes
  '(dart-mode dart-ts-mode kotlin-mode kotlin-ts-mode java-mode java-ts-mode swift-mode swift-ts-mode)
  "Modes that run on a device, with the device key and mode line segment.")

(defvar ygg-device-changed-hook nil
  "Run with no arguments after the selected device changes or finishes booting.")

(defvar ygg-device-selections nil
  "Alist of project root, or t for every other project, to the selected device.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'ygg-device-selections))

(defvar ygg-device-run-function #'ygg-device--run
  "Called with a program, its arguments, a timeout and a callback.
The callback gets the output, or nil when it could not run or timed out.")

(defvar ygg-device-start-function #'ygg-device--start
  "Called with a program and its arguments; starts it detached from Emacs.")

;;; Programs

(defun ygg-device--sdk-program (path fallback)
  (let ((file (expand-file-name path ygg-device-android-sdk)))
    (if (file-executable-p file) file (executable-find fallback))))

(defun ygg-device--adb () (ygg-device--sdk-program "platform-tools/adb" "adb"))
(defun ygg-device--emulator () (ygg-device--sdk-program "emulator/emulator" "emulator"))
(defun ygg-device--xcrun () (executable-find "xcrun"))

(defun ygg-device--flutter ()
  (or (executable-find "flutter")
      (let ((shim (expand-file-name "~/.local/share/mise/shims/flutter")))
        (and (file-executable-p shim) shim))))

(defun ygg-device--run (program args timeout callback)
  (if (not (and program (file-executable-p program)))
      (funcall callback nil)
    (let* ((default-directory (if (file-remote-p default-directory) "~/" default-directory))
           (out (generate-new-buffer " *ygg-device*"))
           (err (generate-new-buffer " *ygg-device-stderr*"))
           (done nil)
           (process
            (make-process
             :name "ygg-device" :command (cons program args) :buffer out :stderr err
             :connection-type 'pipe :noquery t :file-handler nil
             :sentinel (lambda (process _event)
                         (when (and (not done) (memq (process-status process) '(exit signal)))
                           (setq done t)
                           (let ((output (and (eq (process-status process) 'exit)
                                              (with-current-buffer out (buffer-string)))))
                             (kill-buffer out)
                             (when (buffer-live-p err) (kill-buffer err))
                             (funcall callback output)))))))
      (run-at-time timeout nil (lambda () (when (process-live-p process) (delete-process process)))))))

(defun ygg-device--start (program &rest args)
  (let ((default-directory temporary-file-directory))
    (make-process
     :name "ygg-device-start" :connection-type 'pipe :noquery t :file-handler nil
     :command (list "/bin/sh" "-c"
                    (concat "nohup " (mapconcat #'shell-quote-argument (cons program args) " ")
                            " >/dev/null 2>&1 &")))))

;;; Parsing

(defun ygg-device--json (output)
  (when-let* ((start (and output (string-match "^[[{]" output))))
    (ignore-errors
      (json-parse-string (substring output start) :object-type 'plist :array-type 'list
                         :null-object nil :false-object nil))))

(defun ygg-device-parse-adb (output)
  "The devices adb devices -l printed in OUTPUT."
  (cl-loop for line in (split-string (or output "") "\n" t "[ \t\r]+")
           when (string-match "\\`\\([^ \t*]\\S-*\\)[ \t]+\\(device\\|offline\\|unauthorized\\|recovery\\|sideload\\|bootloader\\|authorizing\\|connecting\\)\\b\\(.*\\)\\'"
                              line)
           collect (let* ((serial (match-string 1 line))
                          (state (match-string 2 line))
                          (rest (match-string 3 line))
                          (emulator (string-prefix-p "emulator-" serial)))
                     (list :platform 'android :id serial :flutter-id serial
                           :name (if (string-match "model:\\(\\S-+\\)" rest)
                                     (string-replace "_" " " (match-string 1 rest))
                                   serial)
                           :state (if (equal state "device") (if emulator "booted" "connected") state)
                           :emulator emulator :avd nil))))

(defun ygg-device-parse-avd-name (output)
  "The AVD name adb emu avd name printed in OUTPUT."
  (let ((line (car (split-string (or output "") "\n" t "[ \t\r]+"))))
    (and line (not (member line '("OK" "KO"))) (not (string-prefix-p "KO" line))
         (not (string-match-p "error" line))
         line)))

(defun ygg-device-parse-avds (output)
  "The AVD names emulator -list-avds printed in OUTPUT."
  (seq-filter (lambda (line) (string-match-p "\\`[[:alnum:]._-]+\\'" line))
              (split-string (or output "") "\n" t "[ \t\r]+")))

(defun ygg-device--runtime (runtime)
  "The OS name and version in the simctl RUNTIME keyword, like iOS 27.0."
  (let ((tail (car (last (split-string (substring (symbol-name runtime) 1) "\\.")))))
    (if (string-match "\\`\\([[:alpha:]]+\\)-\\(.*\\)\\'" tail)
        (concat (match-string 1 tail) " " (string-replace "-" "." (match-string 2 tail)))
      tail)))

(defun ygg-device-parse-simctl (output)
  "The available iOS simulators in simctl list devices -j OUTPUT."
  (cl-loop for (runtime devices) on (plist-get (ygg-device--json output) :devices) by #'cddr
           for os = (ygg-device--runtime runtime)
           when (string-prefix-p "iOS " os)
           append (cl-loop for device in devices
                           when (plist-get device :isAvailable)
                           collect (list :platform 'ios
                                         :id (plist-get device :udid)
                                         :flutter-id (plist-get device :udid)
                                         :name (plist-get device :name)
                                         :os os
                                         :state (downcase (plist-get device :state))))))

(defun ygg-device-parse-devicectl (output)
  "The physical iOS devices in devicectl list devices JSON OUTPUT.
Each is known by its UDID, which devicectl, xcodebuild and flutter all take."
  (cl-loop for device in (plist-get (plist-get (ygg-device--json output) :result) :devices)
           for hardware = (plist-get device :hardwareProperties)
           for connection = (plist-get device :connectionProperties)
           when (and (equal (plist-get hardware :reality) "physical")
                     (member (plist-get hardware :platform) '("iOS" "iPadOS")))
           collect (list :platform 'ios-device
                         :id (plist-get hardware :udid)
                         :flutter-id (plist-get hardware :udid)
                         :name (plist-get (plist-get device :deviceProperties) :name)
                         :state (cond ((equal (plist-get connection :tunnelState) "connected") "connected")
                                      ((equal (plist-get connection :pairingState) "paired") "available")
                                      (t "unavailable")))))

(defun ygg-device--flutter-platform (target emulator)
  (cond ((string-prefix-p "android" target) 'android)
        ((equal target "ios") (if emulator 'ios 'ios-device))
        ((equal target "darwin") 'macos)
        ((string-prefix-p "web" target) 'web)
        ((string-prefix-p "linux" target) 'linux)
        ((string-prefix-p "windows" target) 'windows)
        (t (intern target))))

(defun ygg-device-parse-flutter (output)
  "The devices flutter devices --machine printed in OUTPUT."
  (cl-loop for device in (ygg-device--json output)
           when (plist-get device :isSupported)
           collect (list :platform (ygg-device--flutter-platform (plist-get device :targetPlatform)
                                                                 (plist-get device :emulator))
                         :id (plist-get device :id)
                         :flutter-id (plist-get device :id)
                         :name (plist-get device :name)
                         :state "available")))

;;; Listing

(defvar ygg-device--listings nil
  "Alist of source to the devices it last listed.")

(defvar ygg-device--pending nil
  "Sources whose listing is still running.")

(defconst ygg-device--sources '(adb avds simctl devicectl flutter))

(defun ygg-device--list-adb (callback)
  (let ((adb (ygg-device--adb)))
    (funcall ygg-device-run-function adb '("devices" "-l") ygg-device-list-timeout
             (lambda (output)
               (if (not output)
                   (funcall callback :failed)
                 (let* ((devices (ygg-device-parse-adb output))
                        (emulators (seq-filter (lambda (device)
                                                 (and (plist-get device :emulator)
                                                      (equal (plist-get device :state) "booted")))
                                               devices))
                        (left (length emulators)))
                   (if (zerop left)
                       (funcall callback devices)
                     (dolist (device emulators)
                       (funcall ygg-device-run-function adb
                                (list "-s" (plist-get device :id) "emu" "avd" "name")
                                ygg-device-list-timeout
                                (lambda (name-output)
                                  (when-let* ((avd (ygg-device-parse-avd-name name-output)))
                                    (setf (plist-get device :avd) avd
                                          (plist-get device :name) avd))
                                  (when (zerop (cl-decf left))
                                    (funcall callback devices))))))))))))

(defun ygg-device--source-command (source)
  "Program, arguments, timeout and parser listing SOURCE."
  (pcase source
    ('avds (list (ygg-device--emulator) '("-list-avds") ygg-device-list-timeout #'ygg-device-parse-avds))
    ('simctl (list (ygg-device--xcrun) '("simctl" "list" "devices" "-j")
                   ygg-device-list-timeout #'ygg-device-parse-simctl))
    ('devicectl (list (ygg-device--xcrun) '("devicectl" "list" "devices" "--json-output" "-" "-q" "--timeout" "5")
                      ygg-device-list-timeout #'ygg-device-parse-devicectl))
    ('flutter (list (ygg-device--flutter) '("devices" "--machine" "--device-timeout" "3")
                    ygg-device-flutter-timeout #'ygg-device-parse-flutter))))

(defun ygg-device--list (source callback)
  "Call CALLBACK with what SOURCE lists, or :failed."
  (if (eq source 'adb)
      (ygg-device--list-adb callback)
    (pcase-let ((`(,program ,args ,timeout ,parse) (ygg-device--source-command source)))
      (funcall ygg-device-run-function program args timeout
               (lambda (output) (funcall callback (if output (funcall parse output) :failed)))))))

(defun ygg-device--store (source devices)
  (unless (eq devices :failed)
    (setf (alist-get source ygg-device--listings) devices))
  (setq ygg-device--pending (delq source ygg-device--pending)))

(defun ygg-device-refresh ()
  "Relist devices from every source in the background."
  (interactive)
  (dolist (source ygg-device--sources)
    (unless (memq source ygg-device--pending)
      (push source ygg-device--pending)
      (ygg-device--list source (lambda (devices) (ygg-device--store source devices))))))

(defun ygg-device--await (seconds)
  "Wait up to SECONDS for listings still running; quitting stops the wait."
  (let ((deadline (+ (float-time) seconds)))
    (while (and ygg-device--pending (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(defun ygg-device--prefetch ()
  (unless (or ygg-device--listings ygg-device--pending)
    (ygg-device-refresh)))

(defconst ygg-device--platform-order '(android ios ios-device macos web linux windows))

(defun ygg-device-merge (listings)
  "One list of devices from the per-source LISTINGS, ordered by platform."
  (let* ((adb (alist-get 'adb listings))
         (running (delq nil (mapcar (lambda (device) (plist-get device :avd)) adb)))
         (offline (cl-loop for avd in (alist-get 'avds listings)
                           unless (member avd running)
                           collect (list :platform 'android :id nil :flutter-id nil
                                         :name avd :avd avd :state "offline")))
         (known (append adb offline (alist-get 'simctl listings) (alist-get 'devicectl listings)))
         (ids (delq nil (mapcar (lambda (device) (plist-get device :flutter-id)) known)))
         (flutter-only (seq-remove (lambda (device) (member (plist-get device :flutter-id) ids))
                                   (alist-get 'flutter listings)))
         (rank (lambda (device)
                 (or (seq-position ygg-device--platform-order (plist-get device :platform))
                     (length ygg-device--platform-order)))))
    (seq-sort-by rank #'< (append known flutter-only))))

(defun ygg-device-devices ()
  "Every device the last listings found."
  (ygg-device-merge ygg-device--listings))

;;; Selection

(defvar-local ygg-device--buffer-key 'unset)

(defun ygg-device--project-key ()
  (when (eq ygg-device--buffer-key 'unset)
    (setq ygg-device--buffer-key
          (when-let* ((project (project-current)))
            (expand-file-name (project-root project)))))
  ygg-device--buffer-key)

(defun ygg-device-current ()
  "The selected device as a plist of :platform, :id, :flutter-id and :name.
The project's own selection, else the last one made anywhere; nil when none."
  (cdr (or (and-let* ((key (ygg-device--project-key))) (assoc key ygg-device-selections))
           (assq t ygg-device-selections))))

(defun ygg-device--selection (device)
  (let ((selection (list :platform (plist-get device :platform)
                         :id (plist-get device :id)
                         :flutter-id (plist-get device :flutter-id)
                         :name (plist-get device :name))))
    (if-let* ((avd (plist-get device :avd)))
        (append selection (list :avd avd))
      selection)))

(defun ygg-device--same-p (a b)
  (and a b (eq (plist-get a :platform) (plist-get b :platform))
       (if (plist-get a :id)
           (equal (plist-get a :id) (plist-get b :id))
         (and (plist-get a :avd) (equal (plist-get a :avd) (plist-get b :avd))))))

(defun ygg-device--boot-avd (avd)
  (funcall ygg-device-start-function (ygg-device--emulator) "-avd" avd "-no-snapshot-save")
  (ygg-device--watch-boot avd (+ (float-time) ygg-device-boot-timeout)))

(defun ygg-device--booted (avd serial)
  "Give every selection of AVD still waiting for its boot the adb SERIAL."
  (dolist (cell ygg-device-selections)
    (when (and (equal (plist-get (cdr cell) :avd) avd) (not (plist-get (cdr cell) :id)))
      (setcdr cell (ygg-device--selection (append (list :id serial :flutter-id serial) (cdr cell))))))
  (run-hooks 'ygg-device-changed-hook)
  (message "%s is up as %s" avd serial))

(defun ygg-device--watch-boot (avd deadline)
  (ygg-device--list-adb
   (lambda (devices)
     (let ((up (and (listp devices)
                    (seq-find (lambda (device)
                                (and (equal (plist-get device :avd) avd)
                                     (equal (plist-get device :state) "booted")))
                              devices))))
       (cond (up (ygg-device--booted avd (plist-get up :id)))
             ((< (float-time) deadline)
              (run-at-time 3 nil #'ygg-device--watch-boot avd deadline))
             (t (message "%s did not come up within %ds" avd ygg-device-boot-timeout)))))))

(defun ygg-device--boot-simulator (udid)
  (funcall ygg-device-run-function (ygg-device--xcrun) (list "simctl" "boot" udid) 60 #'ignore)
  (funcall ygg-device-run-function "/usr/bin/pgrep" '("-x" "Simulator") ygg-device-list-timeout
           (lambda (output)
             (when (string-empty-p (string-trim (or output "")))
               (funcall ygg-device-start-function "/usr/bin/open" "-a" "Simulator")))))

(defun ygg-device-select (device)
  "Select DEVICE here and as the fallback elsewhere, booting it when it is down."
  (let ((selection (ygg-device--selection device)))
    (dolist (key (delq nil (list (ygg-device--project-key) t)))
      (setf (alist-get key ygg-device-selections nil nil #'equal) (copy-sequence selection)))
    (pcase (plist-get device :state)
      ((and "offline" (guard (plist-get device :avd)) (guard (not (plist-get device :id))))
       (ygg-device--boot-avd (plist-get device :avd)))
      ((and "shutdown" (guard (eq (plist-get device :platform) 'ios)))
       (ygg-device--boot-simulator (plist-get device :id))))
    (run-hooks 'ygg-device-changed-hook)
    selection))

;;; Picker

(defconst ygg-device--group-titles
  '((android . "Android") (ios . "iOS simulator") (ios-device . "iOS device")
    (macos . "macOS") (web . "Web") (linux . "Linux") (windows . "Windows")))

(defun ygg-device--group-title (platform)
  (or (alist-get platform ygg-device--group-titles) (capitalize (symbol-name platform))))

(defun ygg-device--label (device)
  (let ((name (plist-get device :name)))
    (pcase (plist-get device :platform)
      ('android (if-let* ((serial (plist-get device :id))) (concat name "  " serial) name))
      ('ios (if-let* ((os (plist-get device :os))) (concat name "  " os) name))
      (_ name))))

(defun ygg-device-candidates (devices)
  "Alist of a unique picker line to each of DEVICES."
  (let ((labels (mapcar #'ygg-device--label devices)))
    (cl-mapcar (lambda (label device)
                 (cons (if (> (seq-count (apply-partially #'equal label) labels) 1)
                           (concat label "  " (or (plist-get device :id) ""))
                         label)
                       device))
               labels devices)))

(defun ygg-device--annotation (device current)
  (propertize (concat "  " (plist-get device :state)
                      (if (ygg-device--same-p device current) "  selected" ""))
              'face 'shadow))

(defun ygg-device--table (candidates)
  (let ((current (ygg-device-current)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata (category . ygg-device)
                     (display-sort-function . identity)
                     (cycle-sort-function . identity)
                     (group-function
                      . ,(lambda (candidate transform)
                           (if transform
                               candidate
                             (ygg-device--group-title
                              (plist-get (cdr (assoc candidate candidates)) :platform)))))
                     (annotation-function
                      . ,(lambda (candidate)
                           (ygg-device--annotation (cdr (assoc candidate candidates)) current))))
        (complete-with-action action candidates string predicate)))))

(defun ygg-device-pick ()
  "Choose the device this project's Flutter, Android and iOS runs use."
  (interactive)
  (ygg-device-refresh)
  (ygg-device--await ygg-device-picker-wait)
  (let* ((candidates (or (ygg-device-candidates (ygg-device-devices))
                         (user-error "No devices, emulators or simulators found")))
         (choice (completing-read "Device: " (ygg-device--table candidates) nil t))
         (selection (ygg-device-select (cdr (assoc choice candidates)))))
    (message "Device: %s%s" (plist-get selection :name)
             (if (plist-get selection :id) "" " (booting)"))
    selection))

;;; Consumers

(defun ygg-device-require ()
  "The selected device, asking for one when there is none."
  (let ((device (or (ygg-device-current) (ygg-device-pick))))
    (unless (plist-get device :id)
      (user-error "%s is still booting" (plist-get device :name)))
    device))

(defun ygg-device-flutter-id ()
  "The flutter -d id of the selected device, asking for one when there is none."
  (let ((device (ygg-device-require)))
    (or (plist-get device :flutter-id)
        (user-error "%s is not a Flutter target" (plist-get device :name)))))

(defun ygg-device-android-serial ()
  "The adb serial of the selected device when it is an Android one that is up."
  (let ((device (ygg-device-current)))
    (and (eq (plist-get device :platform) 'android) (plist-get device :id))))

(defun ygg-device--without-device-flag (args)
  (pcase args
    ('nil nil)
    (`(,(or "-d" "--device-id") ,_ . ,rest) (ygg-device--without-device-flag rest))
    (`(,arg . ,rest) (cons arg (ygg-device--without-device-flag rest)))))

(defun ygg-device-flutter-tool-args (args)
  "ARGS with its device flag naming the selected device."
  (vconcat (ygg-device--without-device-flag (append args nil))
           (list "-d" (ygg-device-flutter-id))))

(defun ygg-device-dape-flutter (entry)
  "Point dape's flutter ENTRY at the selected device, in place.
The adapter reads the device from toolArgs; deviceId is what Dart-Code names it."
  (let* ((config (cdr entry))
         (args (plist-get config :toolArgs)))
    (when (or (null args) (vectorp args))
      (setq config (plist-put config :toolArgs (list 'ygg-device-flutter-tool-args args))))
    (setcdr entry (plist-put config :deviceId '(ygg-device-flutter-id)))
    entry))

;;; Mode line and key

(defun ygg-device-modeline ()
  "The selected device in a mode that runs on one, as dim plain text."
  (when-let* (((derived-mode-p ygg-device-modes))
              (device (ygg-device-current)))
    (propertize (concat "  " (plist-get device :name)) 'face 'shadow)))

(defvar ygg-device--modeline-entry '(:eval (ygg-device-modeline)))
(put 'ygg-device--modeline-entry 'risky-local-variable t)
(add-to-list 'mode-line-misc-info 'ygg-device--modeline-entry t)

(dolist (mode ygg-device-modes)
  (add-hook (intern (format "%s-hook" mode)) #'ygg-device--prefetch))

(provide 'ygg-device)
;;; ygg-device.el ends here
