;;; react-native-tests.el --- Tests for React Native and Expo support -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dape)
(require 'eglot)
(require 'yggdrasil)
(require 'layer-lsp)
(require 'layer-rass)
(require 'yggdrasil-localleader)
(require 'ygg-dap-js)
(require 'ygg-code-verbs)
(require 'layer-react-native)
(require 'ygg-json-lsp)

(defconst rn-tests--config-dir
  (file-name-directory (directory-file-name (file-name-directory
                                             (or load-file-name buffer-file-name)))))

(add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" rn-tests--config-dir))

(defmacro rn-tests--project (files &rest body)
  "Run BODY in a fresh directory holding FILES, an alist of name to contents.
The directory is bound to root."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (file-truename (make-temp-file "rn-tests-" t))))
          (default-directory root))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,contents) ,files)
             (let ((file (expand-file-name name root)))
               (make-directory (file-name-directory file) t)
               (write-region contents nil file)
               (when (string-prefix-p "node_modules/.bin/" name)
                 (set-file-modes file #o755))))
           ,@body)
       (delete-directory root t))))

(defconst rn-tests--expo
  "{\"dependencies\": {\"expo\": \"~53.0.0\", \"react-native\": \"0.79.5\"}}")

(defconst rn-tests--bare
  "{\"dependencies\": {\"react-native\": \"0.79.5\"}}")

(defconst rn-tests--web
  "{\"dependencies\": {\"react\": \"19.0.0\"}}")

(defconst rn-tests--test-file
  "describe(\"greetingFor\", () => {
  it(\"trims the name\", () => {
    const text = greetingFor(\"  Ada \");
    expect(text).toBe(\"Hello, Ada!\");
  });

  test.only(`keeps (an) empty name`, () => {
    expect(1).toBe(1);
  });
});
")

;;; The project

(ert-deftest rn-root-is-the-package-using-react-native-or-expo ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo) ("src/app/x.tsx" . ""))
    (should (equal (ygg-rn-root (expand-file-name "src/app/" root)) root))
    (should (ygg-rn-expo-p root)))
  (rn-tests--project `(("package.json" . ,rn-tests--bare))
    (should (equal (ygg-rn-root) root))
    (should-not (ygg-rn-expo-p root)))
  (rn-tests--project `(("package.json" . ,rn-tests--web))
    (should-not (ygg-rn-root))))

(ert-deftest rn-root-follows-package-json-edits ()
  (rn-tests--project `(("package.json" . ,rn-tests--web))
    (should-not (ygg-rn-root))
    (write-region rn-tests--bare nil (expand-file-name "package.json" root))
    (set-file-times (expand-file-name "package.json" root) (time-add nil 10))
    (should (equal (ygg-rn-root) root))))

(ert-deftest rn-package-manager-comes-from-the-nearest-lockfile ()
  (pcase-dolist (`(,lockfile . ,exec)
                 '(("pnpm-lock.yaml" "pnpm" "exec" "jest")
                   ("yarn.lock" "yarn" "jest")
                   ("bun.lockb" "bunx" "jest")
                   ("bun.lock" "bunx" "jest")
                   ("package-lock.json" "npx" "jest")))
    (rn-tests--project `(("package.json" . ,rn-tests--expo) (,lockfile . ""))
      (should (equal (ygg-rn-exec root "jest") exec))))
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (should (equal (ygg-rn-exec root "jest") '("npx" "jest"))))
  (rn-tests--project `(("yarn.lock" . "") ("apps/mobile/package.json" . ,rn-tests--expo)
                       ("apps/mobile/pnpm-lock.yaml" . ""))
    (should (equal (ygg-rn-exec (expand-file-name "apps/mobile/" root) "expo")
                   '("pnpm" "exec" "expo")))))

;;; Build and run, Metro

(ert-deftest rn-run-targets-the-selected-device ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo) ("pnpm-lock.yaml" . ""))
    (should (equal (ygg-rn-run-command root '(:platform ios :id "UDID-1" :name "iPhone 17"))
                   '("pnpm" "exec" "expo" "run:ios" "--device" "UDID-1" "--port" "8081")))
    (should (equal (ygg-rn-run-command root '(:platform android :id "emulator-5554"
                                              :name "Pixel_9" :avd "Pixel_9"))
                   '("pnpm" "exec" "expo" "run:android" "--device" "Pixel_9" "--port" "8081")))
    (should-error (ygg-rn-run-command root '(:platform macos :id "mac" :name "Mac"))
                  :type 'user-error))
  (rn-tests--project `(("package.json" . ,rn-tests--bare) ("yarn.lock" . ""))
    (should (equal (ygg-rn-run-command root '(:platform ios-device :id "UDID-2" :name "Phone"))
                   '("yarn" "react-native" "run-ios" "--udid" "UDID-2" "--port" "8081" "--no-packager")))
    (should (equal (ygg-rn-run-command root '(:platform android :id "R58M" :name "SM G973F"))
                   '("yarn" "react-native" "run-android" "--deviceId" "R58M" "--port" "8081" "--no-packager")))))

(ert-deftest rn-metro-clear-drops-only-its-cache ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo) ("pnpm-lock.yaml" . ""))
    (should (equal (ygg-rn-metro-command root)
                   '("pnpm" "exec" "expo" "start" "--port" "8081")))
    (should (equal (ygg-rn-metro-command root t)
                   '("pnpm" "exec" "expo" "start" "--port" "8081" "--clear"))))
  (rn-tests--project `(("package.json" . ,rn-tests--bare))
    (should (equal (ygg-rn-metro-command root t)
                   '("npx" "react-native" "start" "--port" "8081" "--reset-cache")))))

(ert-deftest rn-run-starts-metro-only-when-none-answers ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo) ("pnpm-lock.yaml" . ""))
    (let (started compiled)
      (cl-letf (((symbol-function 'ygg-device-require) (lambda () '(:platform ios :id "U")))
                ((symbol-function 'ygg-rn-metro-status) (lambda () root))
                ((symbol-function 'ygg-rn-metro-start) (lambda (&rest _) (setq started t)))
                ((symbol-function 'compilation-start)
                 (lambda (command &rest _) (setq compiled command))))
        (ygg-rn-run)
        (should-not started)
        (should (equal compiled "pnpm exec expo run\\:ios --device U --port 8081")))
      (cl-letf (((symbol-function 'ygg-device-require) (lambda () '(:platform ios :id "U")))
                ((symbol-function 'ygg-rn-metro-status) (lambda () nil))
                ((symbol-function 'ygg-rn-metro-start) (lambda (&rest _) (setq started t)))
                ((symbol-function 'compilation-start) #'ignore))
        (ygg-rn-run)
        (should started)))))

(ert-deftest rn-preview-is-storybook-or-nothing ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (should (equal (cadr (should-error (ygg-rn-preview) :type 'user-error))
                   "No preview: this project has no Storybook")))
  (rn-tests--project '(("package.json" . "{\"dependencies\": {\"expo\": \"53\"},
 \"devDependencies\": {\"@storybook/react-native\": \"^8\"}}"))
    (let (environment)
      (cl-letf (((symbol-function 'ygg-rn-metro-start)
                 (lambda (_root _clear env) (setq environment env))))
        (ygg-rn-preview)
        (should (equal environment '("EXPO_PUBLIC_STORYBOOK_ENABLED=true")))))))

(ert-deftest rn-run-refuses-a-port-held-by-someone-else ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (dolist (status '(other "/elsewhere/"))
      (cl-letf (((symbol-function 'ygg-rn-metro-status) (lambda () status))
                ((symbol-function 'ygg-rn-metro-start) (lambda (&rest _) (error "Not started"))))
        (should-error (ygg-rn-metro-ensure root) :type 'user-error)))))

(ert-deftest rn-status-tells-metro-from-other-servers ()
  (should (eq (ygg-rn-parse-status "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n<!doctype html>")
              'other))
  (should (equal (ygg-rn-parse-status
                  "HTTP/1.1 200 OK\r\nx-react-native-project-root: /p/app\r\n\r\npackager-status:running")
                 "/p/app/"))
  (should (eq (ygg-rn-parse-status "HTTP/1.1 200 OK\r\n\r\npackager-status:running") t)))

;;; Jest

(defmacro rn-tests--in-tsx (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (tsx-ts-mode)
     (goto-char (point-min))
     ,@body))

(ert-deftest rn-jest-names-the-blocks-around-point ()
  (skip-unless (treesit-ready-p 'tsx t))
  (rn-tests--in-tsx rn-tests--test-file
    (search-forward "toBe(\"Hello")
    (should (equal (ygg-rn-jest-names) '(("describe" . "greetingFor") ("it" . "trims the name"))))
    (should (equal (ygg-rn-jest-pattern (ygg-rn-jest-names)) "^greetingFor trims the name$"))
    (search-forward "expect(1)")
    (should (equal (ygg-rn-jest-pattern (ygg-rn-jest-names))
                   "^greetingFor keeps \\(an\\) empty name$"))
    (goto-char (point-min))
    (search-forward "describe(")
    (should (equal (ygg-rn-jest-pattern (ygg-rn-jest-names)) "^greetingFor "))
    (goto-char (point-max))
    (should-not (ygg-rn-jest-names))))

(ert-deftest rn-jest-runs-a-test-file-by-path-and-a-source-by-relation ()
  (should (equal (ygg-rn-jest-args "/p/" "/p/app/[id].test.tsx" "^a$")
                 '("--watchAll=false" "--coverage=false" "--runTestsByPath" "app/[id].test.tsx"
                   "-t" "^a$")))
  (should (equal (ygg-rn-jest-args "/p/" "/p/__tests__/Greeting.tsx")
                 '("--watchAll=false" "--coverage=false" "--runTestsByPath" "__tests__/Greeting.tsx")))
  (should (equal (ygg-rn-jest-args "/p/" "/p/components/Greeting.tsx")
                 '("--watchAll=false" "--coverage=false" "--findRelatedTests"
                   "components/Greeting.tsx")))
  (should (equal (ygg-rn-jest-args "/p/" nil) '("--watchAll=false" "--coverage=false"))))

(ert-deftest rn-test-at-point-runs-one-test-through-the-package-manager ()
  (skip-unless (treesit-ready-p 'tsx t))
  (rn-tests--project `(("package.json" . ,rn-tests--expo) ("pnpm-lock.yaml" . ""))
    (rn-tests--in-tsx rn-tests--test-file
      (setq buffer-file-name (expand-file-name "__tests__/g.test.tsx" root))
      (search-forward "toBe(\"Hello")
      (let (command)
        (cl-letf (((symbol-function 'compilation-start) (lambda (c &rest _) (setq command c))))
          (ygg-rn-test-at-point))
        (should (equal command
                       (concat "pnpm exec jest --watchAll\\=false --coverage\\=false "
                               "--runTestsByPath __tests__/g.test.tsx "
                               "-t \\^greetingFor\\ trims\\ the\\ name\\$"))))
      (setq buffer-file-name nil))))

;;; Verbs and keys

(ert-deftest rn-verbs-resolve-only-inside-a-project ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (pcase-dolist (`(,concept ,command)
                   '((run ygg-rn-run) (clean ygg-rn-clean) (test-at-point ygg-rn-test-at-point)
                     (test-file ygg-rn-test-file) (test-all ygg-rn-test-all)
                     (preview ygg-rn-preview) (device ygg-device-pick) (build nil)))
      (dolist (mode '(tsx-ts-mode typescript-ts-mode js-ts-mode))
        (should (eq (ygg-code-verbs-resolve concept mode) command)))))
  (rn-tests--project `(("package.json" . ,rn-tests--web))
    (should-not (ygg-code-verbs-resolve 'run 'tsx-ts-mode))
    (should-not (ygg-code-verbs-resolve 'device 'tsx-ts-mode))))

(defun rn-tests--local (mode key)
  (let ((binding (lookup-key (ygg-localleader--get-map mode) (kbd key))))
    (if (eq (car-safe binding) 'menu-item)
        (let ((filtered (funcall (plist-get (nthcdr 3 binding) :filter) (nth 2 binding))))
          (if (consp filtered) (cdr filtered) filtered))
      binding)))

(ert-deftest rn-localleader-keys-exist-only-in-a-project ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (pcase-dolist (`(,key ,command)
                   '(("r" ygg-rn-metro-reload) ("d" ygg-rn-metro-dev-menu) ("j" ygg-rn-devtools)
                     ("M" ygg-rn-metro) ("p" ygg-rn-pod-install) ("c" ygg-rn-expo-config)))
      (dolist (mode '(tsx-ts-mode typescript-ts-mode js-ts-mode))
        (should (eq (rn-tests--local mode key) command)))))
  (rn-tests--project `(("package.json" . ,rn-tests--web))
    (should-not (rn-tests--local 'tsx-ts-mode "r"))))

(ert-deftest rn-metro-keys-need-a-metro-started-here ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (should-error (ygg-rn-metro-reload) :type 'user-error)
    (let (sent)
      (cl-letf (((symbol-function 'ygg-rn-metro-process) (lambda (_) 'metro))
                ((symbol-function 'process-send-string) (lambda (_ s) (push s sent))))
        (ygg-rn-metro-reload)
        (ygg-rn-metro-dev-menu))
      (should (equal sent '("m" "r"))))))

(ert-deftest rn-which-key-shows-rn-keys-only-in-a-project ()
  (require 'which-key)
  (let ((listed (lambda () (mapcar #'car (which-key--get-keymap-bindings
                                          (ygg-localleader--get-map 'tsx-ts-mode))))))
    (rn-tests--project `(("package.json" . ,rn-tests--web))
      (should-not (which-key--format-and-replace
                   (seq-filter (lambda (b) (member (car b) '("r" "M")))
                               (which-key--get-keymap-bindings (ygg-localleader--get-map 'tsx-ts-mode))))))
    (rn-tests--project `(("package.json" . ,rn-tests--expo))
      (should (member "M" (funcall listed))))))

;;; Formatting

(ert-deftest rn-buffers-format-with-the-project-prettier-only ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo) ("node_modules/.bin/prettier" . ""))
    (with-temp-buffer
      (setq default-directory root)
      (json-ts-mode)
      (should (eq apheleia-formatter 'prettier-json))
      (should-not (bound-and-true-p apheleia-inhibit))))
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (with-temp-buffer
      (setq default-directory root)
      (js-ts-mode)
      (should apheleia-inhibit))))

;;; Debugging

(defconst rn-tests--targets
  '((:id "a-1" :title "React Native Bridgeless [C++ connection]" :deviceName "iPhone 17"
     :description "React Native Bridgeless [C++ connection]"
     :webSocketDebuggerUrl "ws://127.0.0.1:8081/inspector/debug?device=a&page=1"
     :reactNative (:capabilities (:nativePageReloads t)))
    (:id "a-3" :title "React Native Bridgeless [C++ connection]" :deviceName "iPhone 17"
     :description "React Native Bridgeless [C++ connection]"
     :webSocketDebuggerUrl "ws://127.0.0.1:8081/inspector/debug?device=a&page=3"
     :reactNative (:capabilities (:nativePageReloads t)))
    (:id "a-2" :title "Reanimated" :deviceName "iPhone 17"
     :description "Reanimated UI runtime [C++ connection]"
     :webSocketDebuggerUrl "ws://127.0.0.1:8081/inspector/debug?device=a&page=4"
     :reactNative (:capabilities (:nativePageReloads t)))
    (:id "b-1" :title "Hermes" :deviceName "Pixel_9"
     :webSocketDebuggerUrl "ws://127.0.0.1:8081/inspector/debug?device=b&page=1"
     :reactNative (:capabilities nil))))

(ert-deftest rn-hermes-targets-keep-the-newest-app-page-per-device ()
  (should (equal (mapcar (lambda (page) (cons (car page) (plist-get (cdr page) :id)))
                         (ygg-rn-hermes-targets rn-tests--targets))
                 '(("iPhone 17" . "a-3")))))

(defmacro rn-tests--with-configs (&rest body)
  (declare (indent 0))
  `(let ((dape-configs (copy-tree dape-configs)))
     (ygg-dap-js-install)
     (ygg-rn-dape-install)
     ,@body))

(ert-deftest rn-dape-entries-derive-from-js-debug ()
  (rn-tests--with-configs
    (let ((attach (alist-get 'js-debug-react-native dape-configs))
          (jest (alist-get 'js-debug-jest dape-configs)))
      (should (equal (plist-get attach 'modes) ygg-rn-modes))
      (should (eq (plist-get attach 'fn) 'ygg-rn-hermes-resolve))
      (should (equal (plist-get attach :request) "attach"))
      (should (equal (plist-get attach :type) "pwa-node"))
      (should-not (plist-member attach :port))
      (should (eq (plist-get attach 'ensure) #'ygg-dap-js-ensure))
      (should (eq (plist-get jest 'fn) 'ygg-rn-jest-resolve))
      (should-not (plist-member jest :program))
      (should-not (plist-member jest :runtimeExecutable)))))

(ert-deftest rn-hermes-attach-maps-bundle-sources-to-the-project ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo))
    (cl-letf (((symbol-function 'ygg-rn--fetch-json) (lambda (_) rn-tests--targets)))
      (let ((config (ygg-rn-hermes-resolve '(:type "pwa-node" :request "attach"))))
        (should (string-prefix-p "ws://127.0.0.1:8081/inspector/debug?device=a&page=3&type=vscode&userAgent="
                                 (plist-get config :websocketAddress)))
        (should (equal (plist-get config :remoteHostHeader) "127.0.0.1:8081"))
        (should (equal (plist-get config :localRoot) root))
        (should (equal (plist-get config :remoteRoot) "http://127.0.0.1:8081"))
        (should (equal (plist-get config :sourceMapPathOverrides)
                       (list (intern ":/[metro-project]/*") (concat root "*"))))
        (should (equal (json-serialize (plist-get config :sourceMapPathOverrides))
                       (format "{\"/[metro-project]/*\":\"%s*\"}" root)))))
    (cl-letf (((symbol-function 'ygg-rn--fetch-json) (lambda (_) nil)))
      (should-error (ygg-rn-hermes-resolve nil) :type 'user-error))))

(ert-deftest rn-jest-debug-runs-the-test-at-point-in-band ()
  (skip-unless (treesit-ready-p 'tsx t))
  (rn-tests--project `(("package.json" . ,rn-tests--expo) ("node_modules/jest/bin/jest.js" . ""))
    (rn-tests--in-tsx rn-tests--test-file
      (setq buffer-file-name (expand-file-name "__tests__/g.test.tsx" root))
      (search-forward "toBe(\"Hello")
      (let ((config (ygg-rn-jest-resolve '(:type "pwa-node"))))
        (should (equal (plist-get config :program)
                       (expand-file-name "node_modules/jest/bin/jest.js" root)))
        (should (equal (plist-get config :args)
                       ["--runInBand" "--watchAll=false" "--coverage=false" "--runTestsByPath"
                        "__tests__/g.test.tsx" "-t" "^greetingFor trims the name$"])))
      (setq buffer-file-name nil))))

;;; ESLint beside the TypeScript server

(ert-deftest rn-eslint-config-kind-is-found-upward ()
  (rn-tests--project '(("eslint.config.js" . "") ("src/a.ts" . ""))
    (should (eq (ygg-rass-eslint-config (expand-file-name "src/" root)) 'flat)))
  (rn-tests--project '((".eslintrc.js" . ""))
    (should (eq (ygg-rass-eslint-config root) 'legacy)))
  (rn-tests--project '(("package.json" . "{\"eslintConfig\": {}}"))
    (should (eq (ygg-rass-eslint-config root) 'legacy)))
  (rn-tests--project '(("package.json" . "{}"))
    (should-not (ygg-rass-eslint-config root))))

(ert-deftest rn-typescript-server-pairs-with-the-project-eslint ()
  (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name))))
    (rn-tests--project '(("eslint.config.js" . "") ("node_modules/.bin/eslint" . ""))
      (should (equal (ygg-rass-with-eslint
                      '("typescript-language-server" "--stdio" :initializationOptions (:a 1)))
                     `("rass" "--no-stream-diagnostics" ,ygg-rass-typescript-preset
                       "--" "typescript-language-server" "--stdio"
                       "--" "vscode-eslint-language-server" "--stdio"
                       :initializationOptions (:a 1))))
      (should (equal (ygg-rass-with-eslint '("/opt/tsgo/bin/tsc" "--lsp" "--stdio"))
                     '("/opt/tsgo/bin/tsc" "--lsp" "--stdio"))))
    (rn-tests--project '(("eslint.config.js" . ""))
      (should (equal (ygg-rass-with-eslint '("vtsls" "--stdio")) '("vtsls" "--stdio"))))))

(ert-deftest rn-eslint-pairing-wraps-the-typescript-entry-and-unwraps ()
  (let* ((eglot-server-programs
          (list (cons '((tsx-ts-mode :language-id "typescriptreact") (js-ts-mode))
                      '("tsserver-x" "--stdio"))))
         (ygg-rass--wrapped-contacts nil))
    (ygg-rass-eslint-enable)
    (should (functionp (cdar eglot-server-programs)))
    (let ((default-directory "/"))
      (should (equal (funcall (cdar eglot-server-programs) nil) '("tsserver-x" "--stdio"))))
    (ygg-rass-disable)
    (should (equal (cdar eglot-server-programs) '("tsserver-x" "--stdio")))))

(ert-deftest rn-eslint-pairing-wraps-a-contact-function ()
  (let* ((eglot-server-programs
          (list (cons '((tsx-ts-mode :language-id "typescriptreact")) 'ygg-lsp-ts-contact)))
         (ygg-rass--wrapped-contacts nil)
         (ygg-lsp-ts-server 'tsls))
    (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name)))
              ((symbol-function 'ygg-lsp--executable) (lambda (name) (concat "/bin/" name))))
      (ygg-rass-eslint-enable)
      (rn-tests--project '(("eslint.config.js" . "") ("node_modules/.bin/eslint" . ""))
        (let ((contact (funcall (cdar eglot-server-programs) nil nil)))
          (should (equal (seq-take contact 4)
                         `("rass" "--no-stream-diagnostics" ,ygg-rass-typescript-preset "--")))
          (should (equal (nth 4 contact) "typescript-language-server"))
          (should (member :initializationOptions contact))))
      (let ((wrapped (cdar eglot-server-programs)))
        (ygg-rass-eslint-enable)
        (should (eq (cdar eglot-server-programs) wrapped)))
      (ygg-rass-disable)
      (should (eq (cdar eglot-server-programs) 'ygg-lsp-ts-contact)))))

(ert-deftest rn-eslintrc-projects-turn-flat-config-off ()
  (cl-letf (((symbol-function 'eglot--major-modes) (lambda (_) '(tsx-ts-mode))))
    (rn-tests--project '((".eslintrc.js" . ""))
      (should (equal (ygg-rass-typescript-configuration 'server)
                     (list (intern ":") '(:useFlatConfig :json-false)))))
    (rn-tests--project '(("eslint.config.mjs" . ""))
      (should-not (ygg-rass-typescript-configuration 'server)))))

(ert-deftest rn-eslint-settings-join-the-typescript-server-settings ()
  (cl-letf (((symbol-function 'eglot--major-modes) (lambda (_) '(tsx-ts-mode)))
            ((symbol-function 'ygg-lsp--server-binary) (lambda (_) "rass"))
            ((symbol-function 'ygg-lsp-ts-server-kind) (lambda (_) 'tsls)))
    (rn-tests--project '((".eslintrc.js" . ""))
      (should (equal (ygg-lsp-workspace-configuration 'server)
                     (list :completions '(:completeFunctionCalls t)
                           (intern ":") '(:useFlatConfig :json-false)))))))

;;; Attaching when an app connects

(ert-deftest rn-offers-each-new-app-once-and-not-while-debugging ()
  (let ((ygg-rn--offered nil) (ygg-rn-auto-attach 'ask) (asked 0) (attached nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (cl-incf asked) t))
              ((symbol-function 'ygg-rn--debugging-p) (lambda () nil))
              ((symbol-function 'ygg-rn-attach) (lambda (target) (push (plist-get target :id) attached))))
      (ygg-rn-offer-attach (ygg-rn-hermes-targets rn-tests--targets) (current-buffer))
      (ygg-rn-offer-attach (ygg-rn-hermes-targets rn-tests--targets) (current-buffer))
      (should (= asked 1))
      (should (equal attached '("a-3"))))
    (setq ygg-rn--offered nil attached nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (error "Not asked while debugging")))
              ((symbol-function 'ygg-rn--debugging-p) (lambda () t))
              ((symbol-function 'ygg-rn-attach) (lambda (target) (push target attached))))
      (ygg-rn-offer-attach (ygg-rn-hermes-targets rn-tests--targets) (current-buffer))
      (should-not attached))
    (setq ygg-rn--offered nil)
    (let ((ygg-rn-auto-attach t))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (error "Not asked when automatic")))
                ((symbol-function 'ygg-rn--debugging-p) (lambda () nil))
                ((symbol-function 'ygg-rn-attach) (lambda (target) (push (plist-get target :id) attached))))
        (ygg-rn-offer-attach (ygg-rn-hermes-targets rn-tests--targets) (current-buffer))
        (should (equal attached '("a-3")))))))

(ert-deftest rn-attach-takes-the-offered-target-without-asking ()
  (let (config)
    (cl-letf (((symbol-function 'dape) (lambda (c) (setq config c)))
              ((symbol-function 'dape--config-eval)
               (lambda (_key _options) (list 'fn #'ygg-rn-hermes-resolve)))
              ((symbol-function 'completing-read) (lambda (&rest _) (error "Not asked"))))
      (ygg-rn-attach '(:id "x" :deviceName "iPhone 17"))
      (should config))
    (let ((ygg-rn--target '(:id "x")))
      (should (equal (ygg-rn--pick-target) '(:id "x"))))))

(ert-deftest rn-watch-runs-while-project-buffers-live ()
  (let ((ygg-rn--watch-timer nil) (ygg-rn--offered '("a-3")) (ygg-rn-auto-attach nil))
    (ygg-rn-watch)
    (should-not ygg-rn--watch-timer)
    (setq ygg-rn-auto-attach 'ask)
    (ygg-rn-watch)
    (should (timerp ygg-rn--watch-timer))
    (cl-letf (((symbol-function 'ygg-rn--project-buffer) (lambda (_) nil)))
      (ygg-rn--watch-tick))
    (should-not ygg-rn--watch-timer)
    (should-not ygg-rn--offered)))

(ert-deftest rn-watch-forgets-offers-when-metro-goes-away ()
  (let ((ygg-rn--offered '("a-3")) offered)
    (cl-letf (((symbol-function 'ygg-rn--project-buffer) (lambda (_) (current-buffer)))
              ((symbol-function 'ygg-rn--fetch-async)
               (lambda (path callback)
                 (funcall callback
                          (pcase path
                            ("/status" "HTTP/1.1 200 OK\r\n\r\npackager-status:running")
                            ("/json/list" (concat "HTTP/1.1 200 OK\r\n\r\n"
                                                  (json-serialize (vconcat rn-tests--targets))))))))
              ((symbol-function 'ygg-rn-offer-attach)
               (lambda (targets _) (setq offered (mapcar #'car targets)))))
      (ygg-rn--watch-tick)
      (should (equal offered '("iPhone 17"))))
    (cl-letf (((symbol-function 'ygg-rn--project-buffer) (lambda (_) (current-buffer)))
              ((symbol-function 'ygg-rn--fetch-async) (lambda (_ callback) (funcall callback nil))))
      (setq ygg-rn--offered '("a-3"))
      (ygg-rn--watch-tick)
      (should-not ygg-rn--offered))))

(ert-deftest rn-no-offer-while-the-minibuffer-is-in-use ()
  (let ((ygg-rn--offered nil) (ygg-rn-auto-attach 'ask))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'window))
              ((symbol-function 'y-or-n-p) (lambda (_) (error "Not asked"))))
      (ygg-rn-offer-attach (ygg-rn-hermes-targets rn-tests--targets) (current-buffer))
      (should-not ygg-rn--offered))))

(ert-deftest rn-jest-names-need-a-tree ()
  (with-temp-buffer
    (insert rn-tests--test-file)
    (goto-char (point-min))
    (should-not (ygg-rn-jest-names))))

(ert-deftest rn-metro-buffers-are-per-root-not-per-name ()
  (should-not (equal (ygg-rn--metro-buffer-name "/a/apps/mobile/")
                     (ygg-rn--metro-buffer-name "/b/apps/mobile/"))))

(ert-deftest rn-watch-offers-from-a-buffer-of-the-metro-project ()
  (rn-tests--project `(("a/package.json" . ,rn-tests--expo) ("b/package.json" . ,rn-tests--expo)
                       ("a/x.tsx" . "") ("b/y.tsx" . ""))
    (let ((a (find-file-noselect (expand-file-name "a/x.tsx" root)))
          (b (find-file-noselect (expand-file-name "b/y.tsx" root))))
      (unwind-protect
          (progn
            (should (eq (ygg-rn--project-buffer (expand-file-name "b/" root)) b))
            (should (eq (ygg-rn--project-buffer (expand-file-name "a/" root)) a))
            (should-not (ygg-rn--project-buffer "/nowhere/")))
        (kill-buffer a)
        (kill-buffer b)))))

;;; JSON and YAML schemas

(defun rn-tests--schema-urls (settings)
  (mapcar (lambda (schema) (plist-get schema :url)) (plist-get settings :schemas)))

(ert-deftest rn-expo-app-schema-only-in-expo-projects ()
  (let ((xdl "https://raw.githubusercontent.com/expo/vscode-expo/schemas/schema/expo-xdl.json"))
    (rn-tests--project `(("package.json" . ,rn-tests--expo))
      (let ((urls (rn-tests--schema-urls (ygg-json-lsp-json-settings (list root)))))
        (should (member xdl urls))
        (should (member "https://json.schemastore.org/package.json" urls))))
    (rn-tests--project `(("package.json" . ,rn-tests--web))
      (let ((urls (rn-tests--schema-urls (ygg-json-lsp-json-settings (list root)))))
        (should-not (member xdl urls))
        (should (member "https://raw.githubusercontent.com/expo/vscode-expo/schemas/schema/eas.json"
                        urls))))))

(ert-deftest rn-expo-app-schema-reaches-a-project-below-the-server-root ()
  (rn-tests--project `(("apps/mobile/package.json" . ,rn-tests--expo) ("apps/mobile/app.json" . "{}"))
    (let* ((mobile (expand-file-name "apps/mobile/" root))
           (buffer (generate-new-buffer " app.json")))
      (unwind-protect
          (progn
            (with-current-buffer buffer (setq default-directory mobile))
            (cl-letf (((symbol-function 'eglot--major-modes) (lambda (_) '(json-ts-mode)))
                      ((symbol-function 'eglot--managed-buffers) (lambda (_) (list buffer))))
              (let* ((schemas (plist-get (plist-get (ygg-json-lsp-configuration 'server) :json)
                                         :schemas))
                     (expo (seq-find (lambda (schema)
                                       (string-match-p "expo-xdl" (plist-get schema :url)))
                                     schemas)))
                (should expo)
                (should (seq-contains-p (plist-get expo :fileMatch)
                                        (expand-file-name "app.json" mobile))))))
        (kill-buffer buffer)))))

(ert-deftest rn-json-and-yaml-servers-get-their-own-settings ()
  (cl-letf (((symbol-function 'eglot--major-modes) (lambda (_) '(json-ts-mode)))
            ((symbol-function 'eglot--managed-buffers) (lambda (_) nil)))
    (should (plist-get (ygg-json-lsp-configuration 'server) :json)))
  (cl-letf (((symbol-function 'eglot--major-modes) (lambda (_) '(yaml-ts-mode))))
    (let ((yaml (plist-get (ygg-json-lsp-configuration 'server) :yaml)))
      (should (equal (plist-get yaml :schemaStore) '(:enable t)))
      (should (equal (json-serialize (plist-get yaml :schemas))
                     "{\"https://raw.githubusercontent.com/expo/vscode-expo/schemas/schema/eas-workflow.json\":[\"**/.eas/workflows/*.yml\",\"**/.eas/workflows/*.yaml\"]}"))))
  (cl-letf (((symbol-function 'eglot--major-modes) (lambda (_) '(python-ts-mode))))
    (should-not (ygg-json-lsp-configuration 'server))))

(ert-deftest rn-expo-app-schema-follows-the-installed-sdk ()
  (rn-tests--project `(("package.json" . ,rn-tests--expo)
                       ("node_modules/expo/package.json" . "{\"version\": \"53.0.27\"}"))
    (let ((ygg-json-lsp-expo-schema-cache (expand-file-name "cache/" root)))
      (should (equal (ygg-json-lsp--expo-sdk root) "53.0.0"))
      (should (string-suffix-p "expo-xdl.json" (ygg-json-lsp-expo-schema-url root)))
      (ygg-json-lsp-write-expo-schema
       "{\"data\": {\"schema\": {\"definitions\": {\"A\": {\"type\": \"string\"}}, \"type\": \"object\", \"properties\": {\"splash\": {\"type\": \"object\"}}}}}"
       (expand-file-name "cache/53.0.0.json" root))
      (should (equal (ygg-json-lsp-expo-schema-url root)
                     (concat "file://" (expand-file-name "cache/53.0.0.json" root))))
      (let ((schema (json-parse-string (with-temp-buffer
                                         (insert-file-contents (expand-file-name "cache/53.0.0.json" root))
                                         (buffer-string))
                                       :object-type 'plist :array-type 'list)))
        (should (plist-get (plist-get schema :definitions) :A))
        (should (equal (plist-get (car (plist-get schema :oneOf)) :required) '("expo")))
        (should (plist-get (plist-get (cadr (plist-get schema :oneOf)) :properties) :splash))))))

;;; react-native-tests.el ends here
