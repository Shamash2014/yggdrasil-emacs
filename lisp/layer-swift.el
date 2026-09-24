;;; layer-swift.el --- Swift: tree-sitter mode, xcodebuild, simctl, SwiftPM and Xcode previews -*- lexical-binding: t; -*-

;; Built-ins wrapped: compile (a compilation mode per build), make-process
;; (the app console, the Xcode tool bridge), json.
;; Third-party: swift-ts-mode on the alex-pinkus grammar, xcodebuild, xcrun
;; simctl/devicectl, swift, xcode-build-server, and Xcode's tool server
;; (xcrun mcpbridge), whose RenderPreview draws a #Preview to a PNG.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'compile)
(require 'treesit)
(require 'yggdrasil-localleader)

(declare-function ygg-device-current "ygg-device")
(declare-function ygg-device-require "ygg-device")
(declare-function ygg-insert-state "yggdrasil-core")

(defgroup ygg-swift nil
  "Building, running, testing and previewing Swift."
  :group 'tools
  :prefix "ygg-swift-")

(defcustom ygg-swift-configuration "Debug"
  "Build configuration xcodebuild uses."
  :type 'string)

(defcustom ygg-swift-preview-on-save t
  "Render the preview again on save while its window shows."
  :type 'boolean)

;;; Major mode

(add-to-list 'treesit-language-source-alist
             '(swift "https://github.com/alex-pinkus/tree-sitter-swift" "with-generated-files"))

;; main holds no parser.c; with-generated-files does, so no tree-sitter CLI is needed
(when (fboundp 'elpaca)
  (elpaca swift-ts-mode
    (unless (treesit-ready-p 'swift t)
      (ignore-errors (treesit-install-language-grammar 'swift)))
    (when (treesit-ready-p 'swift t)
      (add-to-list 'major-mode-remap-alist '(swift-mode . swift-ts-mode)))))

;;; Project

(defun ygg-swift--containers (dir)
  "Workspaces, then projects, then Package.swift directly in DIR."
  (append (directory-files dir t "\\.xcworkspace\\'")
          (directory-files dir t "\\.xcodeproj\\'")
          (and (file-exists-p (expand-file-name "Package.swift" dir))
               (list (expand-file-name "Package.swift" dir)))))

(defun ygg-swift-project (&optional dir)
  "The Swift project around DIR as (:root :kind :container), or nil.
KIND is xcode or spm; CONTAINER the .xcworkspace or .xcodeproj."
  (when-let* ((root (locate-dominating-file
                     (or dir default-directory)
                     (lambda (d) (and (file-directory-p d) (ygg-swift--containers d)))))
              (found (car (ygg-swift--containers (expand-file-name root)))))
    (if (string-suffix-p "Package.swift" found)
        (list :root (file-name-as-directory (expand-file-name root)) :kind 'spm)
      (list :root (file-name-as-directory (expand-file-name root)) :kind 'xcode
            :container found))))

(defun ygg-swift--project-or-error ()
  (or (ygg-swift-project) (user-error "No .xcodeproj, .xcworkspace or Package.swift above here")))

(defun ygg-swift--container-args (project)
  (let ((container (plist-get project :container)))
    (list (if (string-suffix-p ".xcworkspace" container) "-workspace" "-project")
          (file-name-nondirectory container))))

(defun ygg-swift--run (dir program &rest args)
  "Output of PROGRAM with ARGS run in DIR, or signal with its stderr."
  (let ((default-directory dir)
        (err (make-temp-file "ygg-swift-err")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process program nil (list t err) nil args)))
            (unless (eql status 0)
              (error "%s %s: %s" program (car args)
                     (string-trim (with-temp-buffer (insert-file-contents err) (buffer-string)))))
            (buffer-string)))
      (delete-file err))))

(defun ygg-swift--json (string)
  (json-parse-string (substring string (or (string-match-p "[[{]" string) 0))
                     :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defvar ygg-swift--listings (make-hash-table :test #'equal)
  "Container -> its xcodebuild -list answer.")

(defun ygg-swift--listing (project)
  (let ((container (plist-get project :container)))
    (or (gethash container ygg-swift--listings)
        (puthash container
                 (let ((json (ygg-swift--json
                              (apply #'ygg-swift--run (plist-get project :root) "xcodebuild"
                                     "-list" "-json" (ygg-swift--container-args project)))))
                   (or (plist-get json :project) (plist-get json :workspace)))
                 ygg-swift--listings))))

;;; Scheme and buildServer.json

(defvar ygg-swift--schemes (make-hash-table :test #'equal)
  "Container -> the scheme chosen for it.")

(defun ygg-swift--build-server-scheme (project)
  "The scheme a buildServer.json at PROJECT's root was written for."
  (let ((file (expand-file-name "buildServer.json" (plist-get project :root))))
    (when (file-readable-p file)
      (ignore-errors
        (plist-get (ygg-swift--json (with-temp-buffer (insert-file-contents file) (buffer-string)))
                   :scheme)))))

(defun ygg-swift--write-build-server (project scheme)
  "Have xcode-build-server write buildServer.json for PROJECT's SCHEME."
  (if (not (executable-find "xcode-build-server"))
      (message "No xcode-build-server on PATH; sourcekit-lsp will not see the Xcode build")
    (let ((default-directory (plist-get project :root)))
      (make-process
       :name "xcode-build-server"
       :command (append '("xcode-build-server" "config")
                        (ygg-swift--container-args project)
                        (list "-scheme" scheme))
       :buffer (get-buffer-create " *xcode-build-server*")
       :noquery t
       :sentinel (lambda (proc _event)
                   (unless (process-live-p proc)
                     (message (if (zerop (process-exit-status proc))
                                  "buildServer.json written for %s"
                                "xcode-build-server failed for %s; see  *xcode-build-server*")
                              scheme)))))))

(defun ygg-swift-select-scheme (&optional project)
  "Choose PROJECT's scheme and write buildServer.json for it."
  (interactive)
  (let* ((project (or project (ygg-swift--project-or-error)))
         (_ (unless (eq (plist-get project :kind) 'xcode)
              (user-error "SwiftPM packages have no schemes")))
         (schemes (plist-get (ygg-swift--listing project) :schemes))
         (scheme (if (and (not (called-interactively-p 'any)) (length= schemes 1))
                     (car schemes)
                   (completing-read "Scheme: " schemes nil t))))
    (unless (equal scheme (ygg-swift--build-server-scheme project))
      (ygg-swift--write-build-server project scheme))
    (puthash (plist-get project :container) scheme ygg-swift--schemes)))

(defun ygg-swift--scheme (project)
  (or (gethash (plist-get project :container) ygg-swift--schemes)
      (when-let* ((scheme (ygg-swift--build-server-scheme project)))
        (puthash (plist-get project :container) scheme ygg-swift--schemes))
      (ygg-swift-select-scheme project)))

;;; Destination

(defun ygg-swift--booted-simulator ()
  "The first booted iOS simulator as a device plist, or nil."
  (let ((devices (plist-get (ygg-swift--json
                             (ygg-swift--run temporary-file-directory "xcrun"
                                             "simctl" "list" "-j" "devices" "booted"))
                            :devices)))
    (cl-loop for (runtime list) on devices by #'cddr
             when (string-match-p "iOS" (symbol-name runtime))
             thereis (when-let* ((dev (seq-find (lambda (d) (equal (plist-get d :state) "Booted")) list)))
                       (list :platform 'ios :id (plist-get dev :udid) :name (plist-get dev :name))))))

(defun ygg-swift--apple-p (device)
  (memq (intern (format "%s" (plist-get device :platform))) '(ios ios-device macos)))

(defun ygg-swift-destination ()
  "The device builds, runs, tests and debugging go to."
  (let ((device (or (seq-find #'ygg-swift--apple-p
                              (list (and (fboundp 'ygg-device-current) (ygg-device-current))))
                    (ygg-swift--booted-simulator)
                    (seq-find #'ygg-swift--apple-p
                              (list (and (fboundp 'ygg-device-require) (ygg-device-require))))
                    (user-error "No booted iOS simulator and no device selected"))))
    (plist-put (copy-sequence device) :platform
               (intern (format "%s" (plist-get device :platform))))))

(defun ygg-swift--destination-arg (device)
  (pcase (plist-get device :platform)
    ('ios (format "platform=iOS Simulator,id=%s" (plist-get device :id)))
    ('ios-device (format "platform=iOS,id=%s" (plist-get device :id)))
    ('macos "platform=macOS")))

(defun ygg-swift--xcodebuild-args (project device &rest action)
  (append action (ygg-swift--container-args project)
          (list "-scheme" (ygg-swift--scheme project)
                "-configuration" ygg-swift-configuration
                "-destination" (ygg-swift--destination-arg device))))

;;; Build settings of the app

(defvar ygg-swift--settings (make-hash-table :test #'equal)
  "(container scheme destination) -> the app target's build settings.")

(defun ygg-swift--app-settings (project device)
  "Build settings of the application target PROJECT's scheme builds for DEVICE."
  (let ((key (list (plist-get project :container) (ygg-swift--scheme project)
                   (ygg-swift--destination-arg device))))
    (or (gethash key ygg-swift--settings)
        (puthash key
                 (or (plist-get
                      (seq-find (lambda (entry)
                                  (equal (plist-get (plist-get entry :buildSettings) :WRAPPER_EXTENSION)
                                         "app"))
                                (ygg-swift--json
                                 (apply #'ygg-swift--run (plist-get project :root) "xcodebuild"
                                        (ygg-swift--xcodebuild-args project device
                                                                    "-showBuildSettings" "-json"))))
                      :buildSettings)
                     (user-error "Scheme %s builds no app" (ygg-swift--scheme project)))
                 ygg-swift--settings))))

(defun ygg-swift--app-path (settings)
  (expand-file-name (plist-get settings :FULL_PRODUCT_NAME)
                    (plist-get settings :BUILT_PRODUCTS_DIR)))

(defun ygg-swift--executable (settings)
  (expand-file-name (plist-get settings :EXECUTABLE_PATH)
                    (plist-get settings :BUILT_PRODUCTS_DIR)))

;;; Compilation

(defconst ygg-swift-error-regexp-alist
  '(("^\\(/[^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(?:\\(?:fatal \\)?error\\|\\(warning\\)\\|\\(note\\)\\):"
     1 2 3 (4 . 5))
    ("^\\(/[^:\n]+\\):\\([0-9]+\\): error: " 1 2))
  "swiftc, clang and XCTest locations as xcodebuild and swift print them.")

(define-compilation-mode ygg-swift-build-mode "Swift"
  "Compilation mode for xcodebuild and swift output."
  (setq-local compilation-error-regexp-alist ygg-swift-error-regexp-alist))

(defun ygg-swift--compile (dir command name &optional on-success)
  "Run COMMAND in DIR into buffer *swift NAME*; call ON-SUCCESS when it passes."
  (let* ((default-directory dir)
         (buf (compilation-start command #'ygg-swift-build-mode
                                 (lambda (_) (format "*swift %s*" name)))))
    (when on-success
      (with-current-buffer buf
        (add-hook 'compilation-finish-functions
                  (lambda (_buf status)
                    (when (string-prefix-p "finished" status) (funcall on-success)))
                  nil t)))
    buf))

(defun ygg-swift--shell (args)
  (mapconcat #'shell-quote-argument args " "))

(defun ygg-swift-build (&optional on-success)
  "Build the scheme for the destination, or the package; then ON-SUCCESS."
  (interactive)
  (let ((project (ygg-swift--project-or-error)))
    (ygg-swift--compile
     (plist-get project :root)
     (if (eq (plist-get project :kind) 'spm)
         "swift build"
       (ygg-swift--shell (ygg-swift--xcodebuild-args project (ygg-swift-destination)
                                                     "xcodebuild" "build" "-quiet")))
     "build" on-success)))

(defun ygg-swift-clean ()
  "Clean the build folder of the scheme, or the package's .build."
  (interactive)
  (let ((project (ygg-swift--project-or-error)))
    (ygg-swift--compile
     (plist-get project :root)
     (if (eq (plist-get project :kind) 'spm)
         "swift package clean"
       (ygg-swift--shell (ygg-swift--xcodebuild-args project (ygg-swift-destination)
                                                     "xcodebuild" "clean" "-quiet")))
     "build")))

;;; Run

(defconst ygg-swift--console "*swift app*")

(defvar ygg-swift--running nil
  "(device . bundle-id) of the app last launched.")

(defun ygg-swift--install (project device)
  "Install PROJECT's built app on DEVICE and return its build settings."
  (let* ((settings (ygg-swift--app-settings project device))
         (app (ygg-swift--app-path settings)))
    (unless (file-directory-p app) (user-error "No build at %s; build first" app))
    (pcase (plist-get device :platform)
      ('ios (ygg-swift--run temporary-file-directory "xcrun" "simctl" "install"
                            (plist-get device :id) app))
      ('ios-device (ygg-swift--run temporary-file-directory "xcrun" "devicectl" "device" "install"
                                   "app" "--device" (plist-get device :id) app)))
    settings))

(defun ygg-swift--console-buffer ()
  (let ((buf (get-buffer-create ygg-swift--console)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (special-mode))
    buf))

(defun ygg-swift--launch (project device)
  "Install and launch PROJECT's app on DEVICE, its output in *swift app*."
  (let* ((settings (ygg-swift--install project device))
         (bundle (plist-get settings :PRODUCT_BUNDLE_IDENTIFIER))
         (buf (ygg-swift--console-buffer)))
    (when-let* ((old (get-buffer-process buf))) (delete-process old))
    (make-process
     :name "swift-app" :buffer buf :noquery t :connection-type 'pty
     :command (pcase (plist-get device :platform)
                ('ios (list "xcrun" "simctl" "launch" "--console-pty" "--terminate-running-process"
                            (plist-get device :id) bundle))
                ('ios-device (list "xcrun" "devicectl" "device" "process" "launch" "--console"
                                   "--terminate-existing" "--device" (plist-get device :id) bundle))
                ('macos (list (ygg-swift--executable settings)))))
    (setq ygg-swift--running (cons device bundle))
    (display-buffer buf)))

(defun ygg-swift-run-without-build ()
  "Install and launch the last build, or swift run for a package."
  (interactive)
  (let ((project (ygg-swift--project-or-error)))
    (if (eq (plist-get project :kind) 'spm)
        (ygg-swift--compile (plist-get project :root) "swift run" "run")
      (ygg-swift--launch project (ygg-swift-destination)))))

(defun ygg-swift-run ()
  "Build, then install and launch; swift run for a package."
  (interactive)
  (let ((project (ygg-swift--project-or-error)))
    (if (eq (plist-get project :kind) 'spm)
        (ygg-swift--compile (plist-get project :root) "swift run" "run")
      (let ((device (ygg-swift-destination)))
        (ygg-swift-build (lambda () (ygg-swift--launch project device)))))))

(defun ygg-swift-stop ()
  "Terminate the app last launched."
  (interactive)
  (pcase-let ((`(,device . ,bundle) ygg-swift--running))
    (when (eq (plist-get device :platform) 'ios)
      (ignore-errors
        (ygg-swift--run temporary-file-directory "xcrun" "simctl" "terminate"
                        (plist-get device :id) bundle)))
    (when-let* ((buf (get-buffer ygg-swift--console))
                (proc (get-buffer-process buf)))
      (delete-process proc))
    (setq ygg-swift--running nil)
    (message "Stopped %s" (or bundle "nothing"))))

(defun ygg-swift-show-console ()
  "Show the app's output."
  (interactive)
  (display-buffer (get-buffer-create ygg-swift--console)))

;;; Tests

(defun ygg-swift--test-at-point ()
  "(:type TYPE :func NAME :swift-testing BOOL) of the test around point."
  (save-excursion
    (end-of-line)
    (unless (re-search-backward "\\_<func[ \t]+\\([[:alnum:]_]+\\)" nil t)
      (user-error "No test function above point"))
    (let* ((func (match-string-no-properties 1))
           (swift-testing (save-excursion
                            (and (or (string-match-p "@Test\\_>" (buffer-substring (line-beginning-position) (point)))
                                     (progn (forward-line -1) (looking-at-p "[ \t]*@Test\\_>")))
                                 t)))
           (open (nth 1 (syntax-ppss)))
           (type (when open
                   (goto-char open)
                   (when (re-search-backward
                          "\\_<\\(?:class\\|struct\\|actor\\|enum\\|extension\\)[ \t]+\\([[:alnum:]_]+\\)"
                          (line-beginning-position -3) t)
                     (match-string-no-properties 1)))))
      (list :type type :func func :swift-testing swift-testing))))

(defun ygg-swift--test-target (project file)
  "The test target FILE belongs to: the nearest enclosing directory named like one."
  (let ((targets (plist-get (ygg-swift--listing project) :targets))
        (dirs (split-string (file-relative-name (file-name-directory file) (plist-get project :root))
                            "/" t)))
    (or (seq-find (lambda (d) (member d targets)) (reverse dirs))
        (car dirs)
        (user-error "Cannot tell which target %s is in" file))))

(defun ygg-swift--only-testing (project file test)
  (string-join (delq nil (list (ygg-swift--test-target project file)
                               (plist-get test :type)
                               (concat (plist-get test :func)
                                       (and (plist-get test :swift-testing) "()"))))
               "/"))

(defun ygg-swift--spm-filter (test)
  (if (plist-get test :type)
      (format "%s/%s\\b" (plist-get test :type) (plist-get test :func))
    (format "\\.%s\\b" (plist-get test :func))))

;; parallel testing clones the simulator, and cloning shuts the original down
(defun ygg-swift--test-args (project)
  (append (ygg-swift--xcodebuild-args project (ygg-swift-destination) "xcodebuild" "test")
          '("-parallel-testing-enabled" "NO")))

(defun ygg-swift-test-at-point ()
  "Run only the test method or @Test function at point."
  (interactive)
  (let* ((project (ygg-swift--project-or-error))
         (file (ygg-localleader--require-file))
         (test (ygg-swift--test-at-point)))
    (ygg-swift--compile
     (plist-get project :root)
     (if (eq (plist-get project :kind) 'spm)
         (ygg-swift--shell (list "swift" "test" "--filter" (ygg-swift--spm-filter test)))
       (ygg-swift--shell (append (ygg-swift--test-args project)
                                 (list (concat "-only-testing:"
                                               (ygg-swift--only-testing project file test))))))
     "test")))

(defun ygg-swift-test-all ()
  "Run every test of the scheme, or of the package."
  (interactive)
  (let ((project (ygg-swift--project-or-error)))
    (ygg-swift--compile
     (plist-get project :root)
     (if (eq (plist-get project :kind) 'spm)
         "swift test"
       (ygg-swift--shell (ygg-swift--test-args project)))
     "test")))

;;; Launch for a debugger

(defun ygg-swift-build-command ()
  "Shell command building the scheme for the destination, from the project root."
  (let ((project (ygg-swift--project-or-error)))
    (when (eq (plist-get project :kind) 'spm)
      (user-error "ios-simulator debugs Xcode apps; use lldb-dap for a package"))
    (format "cd %s && %s" (shell-quote-argument (plist-get project :root))
            (ygg-swift--shell (ygg-swift--xcodebuild-args project (ygg-swift-destination)
                                                          "xcodebuild" "build" "-quiet")))))

(defun ygg-swift-launch-waiting-for-debugger ()
  "Install and launch the app on the simulator, stopped for a debugger.
Return (:pid PID :program EXECUTABLE)."
  (let* ((project (ygg-swift--project-or-error))
         (device (ygg-swift-destination))
         (_ (unless (eq (plist-get device :platform) 'ios)
              (user-error "%s is not a simulator" (plist-get device :name))))
         (settings (ygg-swift--install project device))
         (bundle (plist-get settings :PRODUCT_BUNDLE_IDENTIFIER))
         (out (ygg-swift--run temporary-file-directory "xcrun" "simctl" "launch"
                              "--wait-for-debugger" "--terminate-running-process"
                              (plist-get device :id) bundle)))
    (unless (string-match ": \\([0-9]+\\)" out)
      (user-error "simctl launch gave no pid: %s" out))
    (setq ygg-swift--running (cons device bundle))
    (list :pid (string-to-number (match-string 1 out))
          :program (ygg-swift--executable settings))))

;;; Previews through Xcode's tool server

(defvar ygg-swift--mcp nil "The xcrun mcpbridge process.")

(defun ygg-swift--mcp-send (proc message)
  (process-send-string proc (concat (json-serialize message) "\n")))

(defun ygg-swift--mcp-filter (proc chunk)
  (let ((lines (split-string (concat (process-get proc 'partial) chunk) "\n")))
    (process-put proc 'partial (car (last lines)))
    (dolist (line (butlast lines))
      (when-let* ((message (ignore-errors (ygg-swift--json line)))
                  (id (plist-get message :id))
                  (callback (gethash id (process-get proc 'pending))))
        (remhash id (process-get proc 'pending))
        (funcall callback message)))))

(defun ygg-swift--mcp-request (proc method params callback)
  (let ((id (1+ (process-get proc 'id))))
    (process-put proc 'id id)
    (puthash id callback (process-get proc 'pending))
    (ygg-swift--mcp-send proc (list :jsonrpc "2.0" :id id :method method :params params))))

(defun ygg-swift--mcp-process ()
  "The live bridge, started and initialized on first use."
  (if (process-live-p ygg-swift--mcp)
      ygg-swift--mcp
    (let ((proc (make-process :name "xcode-mcp" :command '("xcrun" "mcpbridge")
                              :connection-type 'pipe :noquery t
                              :stderr (get-buffer-create " *xcode-mcp*")
                              :filter #'ygg-swift--mcp-filter)))
      (process-put proc 'partial "")
      (process-put proc 'id 0)
      (process-put proc 'pending (make-hash-table))
      (process-put proc 'queue nil)
      (ygg-swift--mcp-request
       proc "initialize"
       (list :protocolVersion "2025-06-18" :capabilities (make-hash-table)
             :clientInfo (list :name "emacs" :version emacs-version))
       (lambda (_)
         (ygg-swift--mcp-send proc (list :jsonrpc "2.0" :method "notifications/initialized"))
         (process-put proc 'ready t)
         (mapc #'funcall (reverse (process-get proc 'queue)))
         (process-put proc 'queue nil)))
      (setq ygg-swift--mcp proc))))

(defun ygg-swift--xcode-tool (tool arguments callback)
  "Call Xcode TOOL with ARGUMENTS; CALLBACK gets its structured result."
  (let* ((proc (ygg-swift--mcp-process))
         (send (lambda ()
                 (ygg-swift--mcp-request
                  proc "tools/call" (list :name tool :arguments arguments)
                  (lambda (message)
                    (let ((result (plist-get message :result)))
                      (if (or (plist-get message :error) (plist-get result :isError))
                          (message "Xcode %s: %s" tool
                                   (or (plist-get (plist-get message :error) :message)
                                       (plist-get (car (plist-get result :content)) :text)))
                        (funcall callback (plist-get result :structuredContent)))))))))
    (if (process-get proc 'ready)
        (funcall send)
      (process-put proc 'queue (cons send (process-get proc 'queue))))))

(defconst ygg-swift--preview-buffer "*swift preview*")

(defun ygg-swift--preview-index ()
  "Zero-based index of the #Preview or PreviewProvider at or above point."
  (save-excursion
    (end-of-line)
    (let ((count 0) (re "^[ \t]*\\(?:#Preview\\_>\\|.*:[ \t]*PreviewProvider\\_>\\)"))
      (while (re-search-backward re nil t) (cl-incf count))
      (max 0 (1- count)))))

(defun ygg-swift--show-preview (png title)
  (let ((buf (get-buffer-create ygg-swift--preview-buffer)))
    (with-current-buffer buf
      (special-mode)
      (let* ((win (display-buffer buf '(display-buffer-in-side-window
                                        (side . right) (window-width . 0.4))))
             (inhibit-read-only t))
        (erase-buffer)
        (insert "\n " title "\n\n ")
        (insert-image (create-image (with-temp-buffer
                                      (set-buffer-multibyte nil)
                                      (insert-file-contents-literally png)
                                      (buffer-string))
                                    'png t
                                    :max-width (- (window-body-width win t) (* 2 (frame-char-width)))
                                    :max-height (window-body-height win t)))
        (goto-char (point-min))))))

(defvar-local ygg-swift--preview-shown nil
  "Index of the preview of this buffer last rendered.")

(defun ygg-swift--preview-scheme (project file)
  "The scheme to preview FILE with: its package target, or the chosen scheme."
  (if (eq (plist-get project :kind) 'spm)
      (let ((dirs (split-string (file-relative-name file (plist-get project :root)) "/")))
        (and (equal (car dirs) "Sources") (cadr dirs)))
    (gethash (plist-get project :container) ygg-swift--schemes)))

(defun ygg-swift--render (file index)
  (let* ((project (ygg-swift--project-or-error))
         (container (if (eq (plist-get project :kind) 'spm)
                        (directory-file-name (plist-get project :root))
                      (plist-get project :container)))
         (scheme (ygg-swift--preview-scheme project file)))
    (message "Rendering preview %d of %s..." (1+ index) (file-name-nondirectory file))
    (ygg-swift--xcode-tool
     "XcodeOpenWorkspace" (list :path container)
     (lambda (workspace)
       (let* ((id (plist-get workspace :workspaceIdentifier))
              (render
               (lambda (&rest _)
                 (ygg-swift--xcode-tool
                  "RenderPreview"
                  (list :workspaceIdentifier id
                        :sourceFilePath (file-relative-name file (plist-get project :root))
                        :previewDefinitionIndexInFile index)
                  (lambda (result)
                    (if-let* ((png (plist-get result :previewSnapshotPath)))
                        (progn
                          (ygg-swift--show-preview
                           png (format "%s · %s" (plist-get result :displayName)
                                       (plist-get (plist-get result :renderedDestination)
                                                  :deviceModelName)))
                          (message nil))
                      (message "Preview failed: %S" (plist-get result :errors))))))))
         (if (and scheme (not (equal scheme (plist-get workspace :activeScheme))))
             (ygg-swift--xcode-tool "XcodeSwitchScheme"
                                    (list :workspaceIdentifier id :schemeName scheme) render)
           (funcall render)))))))

(defun ygg-swift--preview-after-save ()
  (when (and ygg-swift-preview-on-save ygg-swift--preview-shown
             (get-buffer-window ygg-swift--preview-buffer t))
    (ygg-swift--render buffer-file-name ygg-swift--preview-shown)))

(defun ygg-swift-preview ()
  "Render the #Preview at point through Xcode and show the image."
  (interactive)
  (let ((file (ygg-localleader--require-file)))
    (let ((ygg-swift-preview-on-save nil)) (save-buffer))
    (setq ygg-swift--preview-shown (ygg-swift--preview-index))
    (add-hook 'after-save-hook #'ygg-swift--preview-after-save nil t)
    (ygg-swift--render file ygg-swift--preview-shown)))

;;; Edits sourcekit-lsp has no code action for

(defun ygg-swift--lines ()
  "Whole lines of the region, or the current line, as (BEG . END)."
  (let ((beg (if (use-region-p) (region-beginning) (point)))
        (end (if (use-region-p) (region-end) (point))))
    (save-excursion
      (cons (progn (goto-char beg) (line-beginning-position))
            (progn (goto-char end)
                   (if (and (bolp) (> end beg)) (point) (line-beginning-position 2)))))))

(defun ygg-swift--wrap (open close)
  (pcase-let ((`(,beg . ,end) (ygg-swift--lines)))
    (deactivate-mark)
    (save-excursion
      (goto-char end)
      (unless (bolp) (insert "\n"))
      (insert close "\n")
      (let ((end (point-marker)))
        (goto-char beg)
        (insert open " {\n")
        (indent-region beg end)))))

(defun ygg-swift-wrap (name)
  "Wrap the selected lines, or the line, in a NAME { } block."
  (interactive "sWrap in: ")
  (ygg-swift--wrap name "}"))

(defun ygg-swift-wrap-do-catch ()
  "Wrap the selected lines, or the line, in do { } catch { }."
  (interactive)
  (ygg-swift--wrap "do" "} catch {\nprint(error)\n}"))

(defun ygg-swift-drop-init ()
  "Turn Type.init(...) into Type(...) in the selected lines, or the line."
  (interactive)
  (pcase-let ((`(,beg . ,end) (ygg-swift--lines)))
    (deactivate-mark)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\([[:alnum:]_>]\\)\\.init(" end t)
        (unless (member (save-match-data
                          (save-excursion
                            (goto-char (match-beginning 1))
                            (thing-at-point 'symbol t)))
                        '("self" "super"))
          (replace-match "\\1(")
          (setq end (- end 5)))))))

(defun ygg-swift-print-at-point ()
  "Print the name at point, on a new line below."
  (interactive)
  (let ((name (or (thing-at-point 'symbol t) (user-error "No name at point"))))
    (end-of-line)
    (newline-and-indent)
    (insert (format "print(\"%s: \\(%s)\")" name name))))

(defun ygg-swift-split-arguments ()
  "Put each argument or parameter of the call or declaration on its own line."
  (interactive)
  (let ((node (treesit-parent-until
               (treesit-node-at (point))
               (lambda (n) (member (treesit-node-type n)
                                   '("value_arguments" "function_declaration" "init_declaration")))
               t)))
    (unless node (user-error "No call or declaration here"))
    (let* ((items (or (seq-filter (lambda (n) (member (treesit-node-type n) '("value_argument" "parameter")))
                                  (treesit-node-children node))
                      (user-error "Nothing to split")))
           (close (if (equal (treesit-node-type node) "value_arguments")
                      (1- (treesit-node-end node))
                    (save-excursion
                      (goto-char (treesit-node-end (car (last items))))
                      (skip-chars-forward "^)")
                      (point))))
           (markers (mapcar (lambda (n) (copy-marker (treesit-node-start n) t)) items))
           (start (copy-marker (treesit-node-start node)))
           (close (copy-marker close t)))
      (save-excursion
        (dolist (pos (cons close markers))
          (goto-char pos)
          (delete-horizontal-space)
          (unless (save-excursion (skip-chars-backward " \t") (bolp)) (insert "\n")))
        (indent-region start (1+ close))
        (let ((column (progn (goto-char start) (current-indentation))))
          (goto-char close)
          (indent-line-to column))))))

(defun ygg-swift--insert-above (text)
  (beginning-of-line)
  (insert text "\n")
  (forward-line -1)
  (indent-according-to-mode)
  (end-of-line)
  (when (fboundp 'ygg-insert-state) (ygg-insert-state)))

(defun ygg-swift-insert-todo ()
  "Open a TODO comment above the line."
  (interactive)
  (ygg-swift--insert-above "// TODO: "))

(defun ygg-swift-insert-mark ()
  "Open a MARK section comment above the line."
  (interactive)
  (ygg-swift--insert-above "// MARK: - "))

;;; Keys

(dolist (mode '(swift-mode swift-ts-mode))
  (yggdrasil-localleader-def mode "R" #'ygg-swift-run-without-build "run (no build)")
  (yggdrasil-localleader-def mode "k" #'ygg-swift-stop "stop app")
  (yggdrasil-localleader-def mode "o" #'ygg-swift-show-console "app output")
  (yggdrasil-localleader-def mode "s" #'ygg-swift-select-scheme "scheme")
  (yggdrasil-localleader-def mode "f w" #'ygg-swift-wrap "wrap in block")
  (yggdrasil-localleader-def mode "f t" #'ygg-swift-wrap-do-catch "wrap in do/catch")
  (yggdrasil-localleader-def mode "f i" #'ygg-swift-drop-init "drop .init")
  (yggdrasil-localleader-def mode "f s" #'ygg-swift-split-arguments "split arguments")
  (yggdrasil-localleader-def mode "f p" #'ygg-swift-print-at-point "print name at point")
  (yggdrasil-localleader-def mode "f T" #'ygg-swift-insert-todo "insert TODO")
  (yggdrasil-localleader-def mode "f m" #'ygg-swift-insert-mark "insert MARK"))

(provide 'layer-swift)
;;; layer-swift.el ends here
