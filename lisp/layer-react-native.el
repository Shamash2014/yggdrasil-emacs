;;; layer-react-native.el --- React Native and Expo projects -*- lexical-binding: t; -*-

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)
(require 'comint)
(require 'compile)
(require 'treesit)
(require 'url)
(require 'ygg-device)
(require 'yggdrasil-localleader)

(defvar dape-configs)
(declare-function dape "dape")
(declare-function dape--config-eval "dape")
(declare-function dape--live-connections "dape")
(defvar apheleia-formatter)
(defvar apheleia-inhibit)

(defgroup ygg-rn nil
  "React Native and Expo projects."
  :group 'tools
  :prefix "ygg-rn-")

(defcustom ygg-rn-metro-port 8081
  "The port Metro serves bundles and its inspector on."
  :type 'natnum)

(defcustom ygg-rn-storybook-variable "EXPO_PUBLIC_STORYBOOK_ENABLED"
  "Set to true in Metro's environment to show on-device Storybook."
  :type 'string)

(defconst ygg-rn-modes '(js-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
  "Modes a React Native project is written in.")

;;; The project

(defvar ygg-rn--packages (make-hash-table :test #'equal)
  "package.json file -> its modification time and parsed contents.")

(defun ygg-rn--package (directory)
  "DIRECTORY's package.json as an alist, or nil."
  (let ((file (expand-file-name "package.json" directory)))
    (when-let* ((attributes (file-attributes file)))
      (let ((mtime (file-attribute-modification-time attributes))
            (cached (gethash file ygg-rn--packages)))
        (if (equal (car cached) mtime)
            (cdr cached)
          (let ((package (ignore-errors
                           (with-temp-buffer
                             (insert-file-contents file)
                             (json-parse-buffer :object-type 'alist :null-object nil
                                                :false-object nil)))))
            (puthash file (cons mtime package) ygg-rn--packages)
            package))))))

(defun ygg-rn--depends-p (package name)
  (seq-some (lambda (field) (assq (intern name) (alist-get field package)))
            '(dependencies devDependencies)))

(defun ygg-rn-root (&optional directory)
  "The nearest directory from DIRECTORY up using react-native or expo."
  (let ((directory (or directory default-directory)))
    (unless (file-remote-p directory)
      (when-let* ((root (locate-dominating-file
                         directory
                         (lambda (dir)
                           (when-let* ((package (ygg-rn--package dir)))
                             (or (ygg-rn--depends-p package "react-native")
                                 (ygg-rn--depends-p package "expo")))))))
        (file-name-as-directory (expand-file-name root))))))

(defun ygg-rn--require-root ()
  (or (ygg-rn-root) (user-error "Not in a React Native project")))

(defun ygg-rn-expo-p (root)
  "Whether the project at ROOT runs through the Expo CLI."
  (ygg-rn--depends-p (ygg-rn--package root) "expo"))

(defconst ygg-rn--package-managers
  '(("pnpm-lock.yaml" :exec ("pnpm" "exec") :run ("pnpm" "run"))
    ("yarn.lock" :exec ("yarn") :run ("yarn" "run"))
    ("bun.lock" :exec ("bunx") :run ("bun" "run"))
    ("bun.lockb" :exec ("bunx") :run ("bun" "run"))
    ("package-lock.json" :exec ("npx") :run ("npm" "run")))
  "Lockfile -> how its package manager runs a project binary and a script.")

(defun ygg-rn-package-manager (root)
  "How the package manager whose lockfile is nearest ROOT runs things, as a plist."
  (let ((best nil) (depth nil))
    (pcase-dolist (`(,lockfile . ,manager) ygg-rn--package-managers)
      (when-let* ((dir (locate-dominating-file root lockfile))
                  (length (length (expand-file-name dir))))
        (when (or (null depth) (> length depth))
          (setq best manager depth length))))
    (or best (cdr (assoc "package-lock.json" ygg-rn--package-managers)))))

(defun ygg-rn-exec (root &rest args)
  "The command running the project binary and ARGS at ROOT."
  (append (plist-get (ygg-rn-package-manager root) :exec) args))

(defun ygg-rn--bin (root name)
  "The project's own NAME binary, installed at ROOT or a workspace above it."
  (when-let* ((dir (locate-dominating-file root (concat "node_modules/.bin/" name))))
    (expand-file-name (concat "node_modules/.bin/" name) dir)))

(defun ygg-rn--shell (command)
  (mapconcat #'shell-quote-argument command " "))

(defun ygg-rn--name (root)
  (file-name-nondirectory (directory-file-name root)))

;;; Metro

(define-derived-mode ygg-rn-metro-mode comint-mode "Metro"
  "Metro's console; the keys it reads go straight through."
  (setq-local comint-process-echoes nil))

(defun ygg-rn--metro-buffer-name (root)
  (format "*metro: %s*" (abbreviate-file-name (directory-file-name root))))

(defun ygg-rn--metro-buffer (root)
  (get-buffer-create (ygg-rn--metro-buffer-name root)))

(defun ygg-rn-metro-process (root)
  "The Metro started here for ROOT, when it is running."
  (when-let* ((buffer (get-buffer (ygg-rn--metro-buffer-name root)))
              (process (get-buffer-process buffer)))
    (and (process-live-p process) process)))

(defun ygg-rn-metro-command (root &optional clear)
  "The command starting Metro at ROOT, dropping its cache when CLEAR."
  (let ((port (number-to-string ygg-rn-metro-port)))
    (if (ygg-rn-expo-p root)
        (apply #'ygg-rn-exec root "expo" "start" "--port" port (and clear '("--clear")))
      (apply #'ygg-rn-exec root "react-native" "start" "--port" port
             (and clear '("--reset-cache"))))))

(defun ygg-rn-parse-status (response)
  "What an HTTP RESPONSE to /status says serves Metro's port.
The project root a Metro names, t for a Metro naming none, or other."
  (let ((case-fold-search t))
    (cond ((not (string-match-p "packager-status:running" response)) 'other)
          ((string-match "^X-React-Native-Project-Root: *\\([^\r\n]+\\)" response)
           (file-name-as-directory (match-string 1 response)))
          (t t))))

(defun ygg-rn-metro-status ()
  "Nil when nothing serves Metro's port, else what ygg-rn-parse-status says."
  (when-let* ((curl (executable-find "curl")))
    (with-temp-buffer
      (when (eql 0 (call-process curl nil t nil "-s" "-m" "2" "-D" "-"
                                 (format "http://%s/status" (ygg-rn--metro-host))))
        (ygg-rn-parse-status (buffer-string))))))

(defun ygg-rn--metro-host ()
  (format "127.0.0.1:%d" ygg-rn-metro-port))

(defvar-local ygg-rn--metro-environment nil
  "What was added to the environment of the Metro in this buffer.")

(defun ygg-rn--metro-stop (root)
  "Stop the Metro started here for ROOT and wait for its port to free."
  (when-let* ((process (ygg-rn-metro-process root)))
    (set-process-sentinel process #'ignore)
    (delete-process process)
    (let ((deadline (+ (float-time) 5)))
      (while (and (ygg-rn-metro-status) (< (float-time) deadline))
        (sleep-for 0.2)))))

(defun ygg-rn-metro-start (root &optional clear environment)
  "Start Metro for ROOT in its console, CLEAR of cache, with ENVIRONMENT added."
  (let ((buffer (ygg-rn--metro-buffer root))
        (command (ygg-rn-metro-command root clear)))
    (ygg-rn--metro-stop root)
    (when (ygg-rn-metro-status)
      (user-error "Port %d is taken by a server not started here; stop it first"
                  ygg-rn-metro-port))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ygg-rn-metro-mode)
        (ygg-rn-metro-mode))
      (setq default-directory root)
      (setq ygg-rn--metro-environment environment)
      (goto-char (point-max))
      (let ((process-environment (append environment process-environment)))
        (apply #'make-comint-in-buffer "metro" buffer (car command) nil (cdr command))))
    (display-buffer buffer '(nil (inhibit-same-window . t)))
    (ygg-rn-watch)
    buffer))

(defun ygg-rn-metro-ensure (root)
  "Start Metro for ROOT unless one serving ROOT already runs."
  (unless (ygg-rn-metro-process root)
    (pcase (ygg-rn-metro-status)
      ('nil (ygg-rn-metro-start root))
      ('other (user-error "Port %d is taken by a server that is not Metro"
                          ygg-rn-metro-port))
      ((and (pred stringp) other (guard (not (file-equal-p other root))))
       (user-error "Port %d serves Metro for %s" ygg-rn-metro-port other)))))

(defun ygg-rn-metro ()
  "Show this project's Metro, starting it when nothing serves its port."
  (interactive)
  (let ((root (ygg-rn--require-root)))
    (ygg-rn-metro-ensure root)
    (if-let* ((process (ygg-rn-metro-process root)))
        (pop-to-buffer (process-buffer process))
      (message "Metro on port %d was not started here" ygg-rn-metro-port))))

(defun ygg-rn--metro-send (key)
  (let ((process (ygg-rn-metro-process (ygg-rn--require-root))))
    (unless process
      (user-error "No Metro started here; SPC c l starts one"))
    (process-send-string process key)))

(defun ygg-rn-metro-reload ()
  "Reload the app from Metro."
  (interactive)
  (ygg-rn--metro-send "r"))

(defun ygg-rn-metro-dev-menu ()
  "Open the app's developer menu from Metro."
  (interactive)
  (ygg-rn--metro-send (if (ygg-rn-expo-p (ygg-rn--require-root)) "m" "d")))

(defun ygg-rn-devtools ()
  "Open React Native DevTools on the app; it takes the page over from dape."
  (interactive)
  (let* ((target (ygg-rn--pick-target))
         (url-request-method "POST")
         (buffer (url-retrieve-synchronously
                  (format "http://%s/open-debugger?target=%s" (ygg-rn--metro-host)
                          (url-hexify-string (plist-get target :id)))
                  t t 5)))
    (when buffer (kill-buffer buffer))
    (message "DevTools opened on %s" (plist-get target :deviceName))))

(defun ygg-rn-pod-install ()
  "Install the iOS pods of this project."
  (interactive)
  (let ((default-directory (expand-file-name "ios/" (ygg-rn--require-root))))
    (unless (file-exists-p "Podfile")
      (user-error "No ios/Podfile; the native project is not generated yet"))
    (compilation-start "pod install" nil (lambda (_) "*pod install*"))))

(defun ygg-rn-expo-config ()
  "Show the app config Expo resolves, as read-only JSON."
  (interactive)
  (let* ((root (ygg-rn--require-root))
         (default-directory root)
         (buffer (get-buffer-create (format "*expo config: %s*" (ygg-rn--name root)))))
    (unless (ygg-rn-expo-p root)
      (user-error "Not an Expo project"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'call-process (car (ygg-rn-exec root)) nil '(t nil) nil
               (append (cdr (ygg-rn-exec root))
                       '("expo" "config" "--type" "public" "--json")))
        (goto-char (point-min))
        (when (fboundp 'json-ts-mode) (ignore-errors (json-ts-mode)))
        (setq buffer-read-only t)))
    (pop-to-buffer buffer)))

;;; Build and run, clean, preview

(defun ygg-rn-run-command (root device)
  "The command building ROOT's app and launching it on DEVICE, a device plist."
  (let ((id (plist-get device :id))
        (port (number-to-string ygg-rn-metro-port))
        (expo (ygg-rn-expo-p root)))
    (pcase (plist-get device :platform)
      ('android
       (if expo
           (ygg-rn-exec root "expo" "run:android" "--device"
                        (or (plist-get device :avd) (plist-get device :name))
                        "--port" port)
         (ygg-rn-exec root "react-native" "run-android" "--deviceId" id
                      "--port" port "--no-packager")))
      ((or 'ios 'ios-device)
       (if expo
           (ygg-rn-exec root "expo" "run:ios" "--device" id "--port" port)
         (ygg-rn-exec root "react-native" "run-ios" "--udid" id "--port" port "--no-packager")))
      (platform (user-error "%s (%s) is not a React Native target"
                            (plist-get device :name) platform)))))

(defcustom ygg-rn-metro-start-timeout 90
  "Seconds a build waits for a Metro it started to answer."
  :type 'number)

(defun ygg-rn--when-metro-up (root deadline then)
  "Call THEN once Metro serves ROOT, checking until DEADLINE."
  (let ((status (ygg-rn-metro-status)))
    (cond ((and (stringp status) (file-equal-p status root)) (funcall then))
          ((eq status t) (funcall then))
          ((< (float-time) deadline)
           (run-with-timer 1 nil #'ygg-rn--when-metro-up root deadline then))
          (t (message "Metro did not come up within %ds; see its buffer"
                      ygg-rn-metro-start-timeout)))))

(defun ygg-rn-run ()
  "Build the app and launch it on the selected device, with Metro serving it."
  (interactive)
  (let* ((root (ygg-rn--require-root))
         (command (ygg-rn-run-command root (ygg-device-require))))
    (ygg-rn-metro-ensure root)
    (ygg-rn-watch)
    (ygg-rn--when-metro-up
     root (+ (float-time) ygg-rn-metro-start-timeout)
     (lambda ()
       (let ((default-directory root))
         (compilation-start (ygg-rn--shell command) nil
                            (lambda (_) (format "*rn run: %s*" (ygg-rn--name root)))))))))

(defun ygg-rn-clean ()
  "Restart Metro with its cache cleared; nothing on disk is removed."
  (interactive)
  (let ((root (ygg-rn--require-root)))
    (ygg-rn-metro-start root t (and (ygg-rn-metro-process root)
                                    (buffer-local-value 'ygg-rn--metro-environment
                                                        (ygg-rn--metro-buffer root))))
    (message "Metro restarted with a cleared cache")))

(defun ygg-rn-preview ()
  "Run the project's Storybook script, or toggle on-device Storybook in Metro."
  (interactive)
  (let* ((root (ygg-rn--require-root))
         (package (ygg-rn--package root))
         (default-directory root))
    (cond
     ((assq 'storybook (alist-get 'scripts package))
      (compilation-start (ygg-rn--shell (append (plist-get (ygg-rn-package-manager root) :run)
                                                '("storybook")))
                         t (lambda (_) (format "*storybook: %s*" (ygg-rn--name root)))))
     ((ygg-rn--depends-p package "@storybook/react-native")
      (let* ((on (not (and (ygg-rn-metro-process root)
                           (buffer-local-value 'ygg-rn--metro-environment
                                               (ygg-rn--metro-buffer root)))))
             (environment (and on (list (concat ygg-rn-storybook-variable "=true")))))
        (ygg-rn-metro-start root nil environment)
        (message "Storybook %s; reload the app" (if on "on" "off"))))
     (t (user-error "No preview: this project has no Storybook")))))

;;; Jest

(defconst ygg-rn--jest-blocks
  '("describe" "fdescribe" "xdescribe" "it" "fit" "xit" "test" "xtest")
  "Calls that name a Jest block.")

(defun ygg-rn--jest-block (node)
  "The kind and name of the Jest block NODE opens, as a cons, or nil."
  (when (equal (treesit-node-type node) "call_expression")
    (let* ((callee (treesit-node-child-by-field-name node "function"))
           (callee (if (equal (treesit-node-type callee) "member_expression")
                       (treesit-node-child-by-field-name callee "object")
                     callee))
           (kind (and (equal (treesit-node-type callee) "identifier")
                      (treesit-node-text callee t)))
           (title (treesit-node-child
                   (treesit-node-child-by-field-name node "arguments") 0 t)))
      (when (and (member kind ygg-rn--jest-blocks)
                 (member (treesit-node-type title) '("string" "template_string")))
        (cons kind (substring (treesit-node-text title t) 1 -1))))))

(defun ygg-rn-jest-names (&optional position)
  "The Jest blocks around POSITION, outermost first, as (KIND . NAME) conses."
  (let ((node (and (treesit-parser-list) (treesit-node-at (or position (point)))))
        (blocks nil))
    (while node
      (when-let* ((block (ygg-rn--jest-block node)))
        (push block blocks))
      (setq node (treesit-node-parent node)))
    blocks))

(defun ygg-rn--js-regexp-quote (string)
  (replace-regexp-in-string "[][\\\\^$.|?*+(){}/]" "\\\\\\&" string))

(defun ygg-rn-jest-pattern (blocks)
  "The -t pattern matching BLOCKS, a test exactly or a describe and all under it."
  (when blocks
    (concat "^" (ygg-rn--js-regexp-quote (mapconcat #'cdr blocks " "))
            (if (member (car (car (last blocks))) '("describe" "fdescribe" "xdescribe")) " " "$"))))

(defun ygg-rn-test-file-p (file)
  "Whether FILE is where Jest looks for tests by default."
  (string-match-p "\\(?:/__tests__/\\|[.-]\\(?:test\\|spec\\)\\.[cm]?[jt]sx?\\'\\)" file))

(defun ygg-rn-jest-args (root file &optional pattern)
  "Arguments running Jest once at ROOT over FILE, only tests matching PATTERN."
  (append '("--watchAll=false" "--coverage=false")
          (when file
            (list (if (ygg-rn-test-file-p file) "--runTestsByPath" "--findRelatedTests")
                  (file-relative-name file root)))
          (when pattern (list "-t" pattern))))

(defun ygg-rn--jest (args)
  (let* ((root (ygg-rn--require-root))
         (default-directory root))
    (compilation-start (ygg-rn--shell (apply #'ygg-rn-exec root "jest" args)) nil
                       (lambda (_) (format "*jest: %s*" (ygg-rn--name root))))))

(defun ygg-rn--file ()
  (or buffer-file-name (user-error "Buffer visits no file")))

(defun ygg-rn-test-at-point ()
  "Run the Jest test or describe around point."
  (interactive)
  (let ((pattern (or (ygg-rn-jest-pattern (ygg-rn-jest-names))
                     (user-error "No describe, it or test around point"))))
    (ygg-rn--jest (ygg-rn-jest-args (ygg-rn--require-root) (ygg-rn--file) pattern))))

(defun ygg-rn-test-file ()
  "Run this test file, or the tests related to this source file."
  (interactive)
  (ygg-rn--jest (ygg-rn-jest-args (ygg-rn--require-root) (ygg-rn--file))))

(defun ygg-rn-test-all ()
  "Run every Jest test of the project once."
  (interactive)
  (ygg-rn--jest (ygg-rn-jest-args (ygg-rn--require-root) nil)))

;;; Formatting

(defun ygg-rn--setup-buffer ()
  "Format with the project's own prettier, and with nothing when it has none.
Watch for apps connecting to a Metro that is already running."
  (when-let* ((root (ygg-rn-root)))
    (when buffer-file-name
      (ygg-rn-watch))
    (if (not (ygg-rn--bin root "prettier"))
        (setq-local apheleia-inhibit t)
      (when (derived-mode-p 'json-mode 'json-ts-mode 'js-json-mode)
        (setq-local apheleia-formatter 'prettier-json)))))

(dolist (mode (append ygg-rn-modes '(json-mode json-ts-mode js-json-mode)))
  (add-hook (intern (format "%s-hook" mode)) #'ygg-rn--setup-buffer))

;;; Debugging

(defun ygg-rn--without (plist &rest keys)
  (let ((result nil))
    (while plist
      (unless (memq (car plist) keys)
        (setq result (append result (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    result))

(defun ygg-rn--fetch-json (url)
  "URL's body parsed as JSON, or nil when nothing answers."
  (when-let* ((buffer (ignore-errors (url-retrieve-synchronously url t t 3))))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "\r?\n\r?\n" nil t)
            (ignore-errors
              (json-parse-buffer :object-type 'plist :array-type 'list
                                 :null-object nil :false-object nil))))
      (kill-buffer buffer))))

(defun ygg-rn--page (target)
  (let ((url (or (plist-get target :webSocketDebuggerUrl) "")))
    (if (string-match "[?&]page=\\([0-9]+\\)" url)
        (string-to-number (match-string 1 url))
      0)))

(defun ygg-rn-hermes-targets (targets)
  "The app runtimes among Metro's inspector TARGETS, the newest page per device."
  (let ((pages nil))
    (dolist (target targets)
      (when (and (or (plist-get (plist-get (plist-get target :reactNative) :capabilities)
                                :nativePageReloads)
                     (equal (plist-get target :title)
                            "React Native Experimental (Improved Chrome Reloads)"))
                 (not (equal (plist-get target :description)
                             "Reanimated UI runtime [C++ connection]")))
        (let* ((device (or (plist-get target :deviceName) (plist-get target :title)))
               (kept (assoc device pages)))
          (if (null kept)
              (push (cons device target) pages)
            (when (>= (ygg-rn--page target) (ygg-rn--page (cdr kept)))
              (setcdr kept target))))))
    (nreverse pages)))

(defvar ygg-rn--target nil
  "The inspector target the next Hermes attach takes, instead of asking.")

(defun ygg-rn--pick-target ()
  "The app runtime Metro's inspector lists, asked for when there are several."
  (or ygg-rn--target
      (ygg-rn--ask-target)))

(defun ygg-rn--ask-target ()
  (let* ((pages (or (ygg-rn-hermes-targets
                     (ygg-rn--fetch-json (format "http://%s/json/list" (ygg-rn--metro-host))))
                    (user-error "No app is connected to Metro on %s" (ygg-rn--metro-host)))))
    (cdr (if (cdr pages)
             (assoc (completing-read "App: " pages nil t) pages)
           (car pages)))))

(defun ygg-rn--put-absent (config &rest pairs)
  "CONFIG with each key of PAIRS set to its value, unless CONFIG already has it."
  (while pairs
    (let ((key (pop pairs))
          (value (pop pairs)))
      (unless (plist-member config key)
        (setq config (append config (list key value))))))
  config)

(defun ygg-rn-hermes-resolve (config)
  "CONFIG attached to the app runtime Metro's inspector lists."
  (let* ((host (ygg-rn--metro-host))
         (target (ygg-rn--pick-target))
         (root (ygg-rn--require-root)))
    (message "Attached to %s; opening DevTools on it detaches dape"
             (plist-get target :deviceName))
    (ygg-rn--put-absent
     config
     :websocketAddress (concat (plist-get target :webSocketDebuggerUrl)
                               "&type=vscode&userAgent="
                               (url-hexify-string (format "Emacs/%s dape" emacs-version)))
     :remoteHostHeader host
     :cwd root
     :localRoot root
     :remoteRoot (concat "http://" host)
     :sourceMapPathOverrides (list (intern ":/[metro-project]/*") (concat root "*")))))

(defun ygg-rn-jest-resolve (config)
  "CONFIG running Jest in band over this file, only the test around point."
  (let* ((root (ygg-rn--require-root))
         (jest (or (locate-dominating-file root "node_modules/jest/bin/jest.js")
                   (user-error "Jest is not installed in this project")))
         (file (ygg-rn--file))
         (pattern (ygg-rn-jest-pattern (ygg-rn-jest-names))))
    (ygg-rn--put-absent
     config
     :cwd root
     :program (expand-file-name "node_modules/jest/bin/jest.js" jest)
     :args (vconcat '("--runInBand") (ygg-rn-jest-args root file pattern)))))

(defun ygg-rn-dape-install ()
  "Derive the Hermes attach and the Jest entries from dape's js-debug ones."
  (when-let* ((attach (alist-get 'js-debug-node-attach dape-configs)))
    (setf (alist-get 'js-debug-react-native dape-configs)
          (append `(modes ,ygg-rn-modes fn ygg-rn-hermes-resolve)
                  (ygg-rn--without attach 'modes 'fn :port)
                  '(:sourceMaps t :pauseForSourceMap t :outFiles []
                    :attachExistingChildren t :enableTurboSourcemaps t :continueOnAttach t
                    :resolveSourceMapLocations
                    ["**" "!**/__prelude__/**" "!webpack:**" "!**/node_modules/!(expo)/**"
                     "!**/node_modules/react-devtools-core/**"]))))
  (when-let* ((node (alist-get 'js-debug-node dape-configs)))
    (setf (alist-get 'js-debug-jest dape-configs)
          (append `(modes ,ygg-rn-modes fn ygg-rn-jest-resolve)
                  (ygg-rn--without node 'modes 'fn :program :cwd :runtimeExecutable)))))

(with-eval-after-load 'dape
  (ygg-rn-dape-install))

;;; Attaching when an app connects

(defcustom ygg-rn-auto-attach 'ask
  "What to do when an app connects to Metro: ask, attach, or nothing."
  :type '(choice (const :tag "Ask" ask) (const :tag "Attach" t) (const :tag "Nothing" nil)))

(defcustom ygg-rn-watch-interval 3
  "Seconds between looks at the apps connected to Metro."
  :type 'number)

(defvar ygg-rn--watch-timer nil)

(defvar ygg-rn--offered nil
  "Inspector target ids already offered for attaching.")

(defun ygg-rn--project-buffer (root)
  "A live buffer of the React Native project at ROOT, of any when ROOT is t."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (when-let* (((and buffer-file-name (derived-mode-p ygg-rn-modes)))
                            (own (ygg-rn-root)))
                  (or (eq root t) (file-equal-p own root)))))
            (buffer-list)))

(defun ygg-rn--debugging-p ()
  (and (fboundp 'dape--live-connections) (dape--live-connections) t))

(defun ygg-rn-attach (target)
  "Attach dape to the app runtime TARGET, a Metro inspector target."
  (let ((ygg-rn--target target))
    (dape (dape--config-eval 'js-debug-react-native nil))))

(defun ygg-rn-offer-attach (targets buffer)
  "Offer to attach to each of TARGETS not offered before, from BUFFER."
  (pcase-dolist (`(,device . ,target) targets)
    (let ((id (plist-get target :id)))
      (unless (or (member id ygg-rn--offered) (active-minibuffer-window))
        (push id ygg-rn--offered)
        (when (and ygg-rn-auto-attach
                   (not (ygg-rn--debugging-p))
                   (or (eq ygg-rn-auto-attach t)
                       (y-or-n-p (format "%s connected to Metro; attach the debugger? " device))))
          (with-current-buffer buffer
            (ygg-rn-attach target)))))))

(defun ygg-rn--fetch-async (path callback)
  "Call CALLBACK with the response to Metro's PATH, headers included, or nil."
  (url-retrieve (format "http://%s%s" (ygg-rn--metro-host) path)
                (lambda (status)
                  (let ((response (unless (plist-get status :error) (buffer-string))))
                    (kill-buffer)
                    (funcall callback response)))
                nil t t))

(defun ygg-rn--response-json (response)
  (when (and response (string-match "\r?\n\r?\n" response))
    (ignore-errors
      (json-parse-string (substring response (match-end 0))
                         :object-type 'plist :array-type 'list
                         :null-object nil :false-object nil))))

(defun ygg-rn--watch-tick ()
  (if (not (ygg-rn--project-buffer t))
      (ygg-rn-watch-stop)
    (ygg-rn--fetch-async
     "/status"
     (lambda (response)
       (let ((root (and response (ygg-rn-parse-status response))))
         (if (not root)
             (setq ygg-rn--offered nil)
           (when-let* (((not (eq root 'other)))
                       (buffer (ygg-rn--project-buffer root)))
             (ygg-rn--fetch-async
              "/json/list"
              (lambda (response)
                (when (buffer-live-p buffer)
                  (ygg-rn-offer-attach
                   (ygg-rn-hermes-targets (ygg-rn--response-json response))
                   buffer)))))))))))

(defun ygg-rn-watch ()
  "Look for apps connecting to Metro while a project buffer is open."
  (when (and ygg-rn-auto-attach (not (timerp ygg-rn--watch-timer)))
    (setq ygg-rn--watch-timer
          (run-with-timer ygg-rn-watch-interval ygg-rn-watch-interval #'ygg-rn--watch-tick))))

(defun ygg-rn-watch-stop ()
  "Stop looking for apps; the next Metro starts afresh."
  (when (timerp ygg-rn--watch-timer)
    (cancel-timer ygg-rn--watch-timer))
  (setq ygg-rn--watch-timer nil
        ygg-rn--offered nil))

;;; Keys unique to React Native, only inside a project

(defun ygg-rn--in-project (command)
  (and (ygg-rn-root) command))

(dolist (mode ygg-rn-modes)
  (pcase-dolist (`(,key ,command ,label)
                 '(("r" ygg-rn-metro-reload "reload app")
                   ("d" ygg-rn-metro-dev-menu "dev menu")
                   ("j" ygg-rn-devtools "open DevTools")
                   ("M" ygg-rn-metro "Metro")
                   ("p" ygg-rn-pod-install "pod install")
                   ("c" ygg-rn-expo-config "expo config")))
    (define-key (ygg-localleader--get-map mode) (kbd key)
                `(menu-item ,label (,label . ,command) :filter ygg-rn--in-project))))

(defvar which-key-replacement-alist)

;; a key whose filter says no reads as nil to which-key; hide it
(with-eval-after-load 'which-key
  (add-to-list 'which-key-replacement-alist '((nil . "\\`nil\\'") . ignore)))

(provide 'layer-react-native)
;;; layer-react-native.el ends here
