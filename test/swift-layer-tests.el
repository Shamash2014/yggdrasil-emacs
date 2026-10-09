;;; swift-layer-tests.el --- Tests for layer-swift -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'layer-swift)

(defvar dape-configs)
(declare-function swift-ts-mode "swift-ts-mode")
(declare-function ygg-match--textobject-bounds "yggdrasil-match")
(declare-function ygg-dap-ios-install "ygg-dap-ios")
(declare-function ygg-dap-ios-launch "ygg-dap-ios")

(defconst swift-layer-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun swift-layer-test--grammar-path ()
  (cons (expand-file-name "tree-sitter" swift-layer-test--root) treesit-extra-load-path))

(defun swift-layer-test--ts-mode-p ()
  (add-to-list 'load-path (expand-file-name "elpaca/builds/swift-ts-mode" swift-layer-test--root))
  (and (require 'swift-ts-mode nil t)
       (let ((treesit-extra-load-path (swift-layer-test--grammar-path)))
         (treesit-ready-p 'swift t))))

(defmacro swift-layer-test--in-dir (files &rest body)
  "Run BODY in a temp dir holding FILES; a trailing slash makes a directory."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "swift-layer-" t)))
          (default-directory root))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (let ((path (expand-file-name file root)))
               (if (string-suffix-p "/" file)
                   (make-directory path t)
                 (make-directory (file-name-directory path) t)
                 (with-temp-file path (insert "")))))
           ,@body)
       (delete-directory root t))))

(defmacro swift-layer-test--source (text &rest body)
  "Run BODY in a swift-ish buffer holding TEXT, point at the | marker."
  (declare (indent 1))
  `(with-temp-buffer
     (with-syntax-table (let ((table (make-syntax-table)))
                          (modify-syntax-entry ?{ "(}" table)
                          (modify-syntax-entry ?} "){" table)
                          (modify-syntax-entry ?/ ". 124b" table)
                          (modify-syntax-entry ?\n "> b" table)
                          table)
       (insert ,text)
       (goto-char (point-min))
       (search-forward "|")
       (delete-char -1)
       ,@body)))

(ert-deftest swift-layer-workspace-wins-over-project ()
  (swift-layer-test--in-dir '("App.xcodeproj/" "App.xcworkspace/" "App/Sources/")
    (let ((project (ygg-swift-project (expand-file-name "App/Sources/" root))))
      (should (eq (plist-get project :kind) 'xcode))
      (should (equal (plist-get project :root) root))
      (should (string-suffix-p "App.xcworkspace" (plist-get project :container)))
      (should (equal (ygg-swift--container-args project) '("-workspace" "App.xcworkspace"))))))

(ert-deftest swift-layer-package-is-spm ()
  (swift-layer-test--in-dir '("Package.swift" "Tests/PkgTests/")
    (let ((project (ygg-swift-project (expand-file-name "Tests/PkgTests/" root))))
      (should (eq (plist-get project :kind) 'spm))
      (should (equal (plist-get project :root) root)))))

(ert-deftest swift-layer-nearest-root-wins ()
  (swift-layer-test--in-dir '("Outer.xcodeproj/" "Pkg/Package.swift" "Pkg/Sources/")
    (should (eq (plist-get (ygg-swift-project (expand-file-name "Pkg/Sources/" root)) :kind) 'spm))))

(ert-deftest swift-layer-no-project-is-nil ()
  (swift-layer-test--in-dir '("src/")
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (dir pred)
                 (let ((d (expand-file-name dir)))
                   (and (string-prefix-p root d) (funcall pred d) d)))))
      (should-not (ygg-swift-project (expand-file-name "src/" root))))))

(ert-deftest swift-layer-xctest-method-at-point ()
  (swift-layer-test--source
      "import XCTest\n\nfinal class LoginTests: XCTestCase {\n    func testOne() {\n        XCT|AssertTrue(true)\n    }\n    func testTwo() {}\n}\n"
    (should (equal (ygg-swift--test-at-point)
                   '(:type "LoginTests" :func "testOne" :swift-testing nil)))))

(ert-deftest swift-layer-swift-testing-in-suite ()
  (swift-layer-test--source
      "import Testing\n\n@Suite struct MathTests {\n    @Test\n    func adds() {\n        #ex|pect(1 + 1 == 2)\n    }\n}\n"
    (should (equal (ygg-swift--test-at-point)
                   '(:type "MathTests" :func "adds" :swift-testing t)))))

(ert-deftest swift-layer-swift-testing-on-one-line ()
  (swift-layer-test--source "import Testing\n\n@Test func example() async throws {\n    #ex|pect(true)\n}\n"
    (should (equal (ygg-swift--test-at-point)
                   '(:type nil :func "example" :swift-testing t)))))

(ert-deftest swift-layer-free-swift-testing-function ()
  (swift-layer-test--source "import Testing\n\n@Test\nfunc example() {\n    #ex|pect(true)\n}\n"
    (should (equal (ygg-swift--test-at-point)
                   '(:type nil :func "example" :swift-testing t)))))

(ert-deftest swift-layer-only-testing-names-target-class-method ()
  (swift-layer-test--in-dir '("App.xcodeproj/" "AppTests/")
    (let ((project (ygg-swift-project))
          (file (expand-file-name "AppTests/LoginTests.swift" root)))
      (cl-letf (((symbol-function 'ygg-swift--listing)
                 (lambda (_) '(:schemes ("App") :targets ("App" "AppTests")))))
        (should (equal (ygg-swift--only-testing project file '(:type "LoginTests" :func "testOne"))
                       "AppTests/LoginTests/testOne"))
        (should (equal (ygg-swift--only-testing project file
                                                '(:type "MathTests" :func "adds" :swift-testing t))
                       "AppTests/MathTests/adds()"))))))

(ert-deftest swift-layer-spm-filter ()
  (should (equal (ygg-swift--spm-filter '(:type "LoginTests" :func "testOne")) "LoginTests/testOne\\b"))
  (should (equal (ygg-swift--spm-filter '(:type nil :func "example")) "\\.example\\b")))

(ert-deftest swift-layer-preview-index-counts-definitions-above ()
  (swift-layer-test--source
      "struct V: View {}\n#Preview {\n  V()\n}\n\n#Preview(\"Dark\") {\n  V()|\n}\n"
    (should (= (ygg-swift--preview-index) 1)))
  (swift-layer-test--source "|struct V: View {}\n#Preview { V() }\n"
    (should (= (ygg-swift--preview-index) 0)))
  (swift-layer-test--source
      "struct V_Previews: PreviewProvider {}\n#Preview { V() }|\n"
    (should (= (ygg-swift--preview-index) 1))))

(ert-deftest swift-layer-error-regexp-finds-swiftc-and-xctest ()
  (with-temp-buffer
    (insert "/tmp/App/ContentView.swift:12:9: error: cannot find 'x' in scope\n"
            "/tmp/App/ContentView.swift:3:1: warning: unused\n"
            "/tmp/AppTests/T.swift:14: error: -[AppTests.T testOne] : XCTAssertTrue failed\n"
            "** BUILD FAILED **\n")
    (ygg-swift-build-mode)
    (compilation--ensure-parse (point-max))
    (goto-char (point-min))
    (let ((types nil))
      (while (not (eobp))
        (when-let* ((msg (get-text-property (point) 'compilation-message)))
          (push (compilation--message->type msg) types))
        (forward-line 1))
      (should (equal (nreverse types) '(2 1 2))))))

(ert-deftest swift-layer-destination-prefers-shared-device ()
  (cl-letf (((symbol-function 'ygg-device-current)
             (lambda () '(:platform ios :id "UDID-1" :name "iPhone 18 Pro")))
            ((symbol-function 'ygg-swift--booted-simulator) (lambda () (error "Not reached"))))
    (let ((device (ygg-swift-destination)))
      (should (equal (plist-get device :id) "UDID-1"))
      (should (equal (ygg-swift--destination-arg device) "platform=iOS Simulator,id=UDID-1")))))

(ert-deftest swift-layer-destination-skips-android-for-booted-simulator ()
  (cl-letf (((symbol-function 'ygg-device-current)
             (lambda () '(:platform "android" :id "emulator-5554")))
            ((symbol-function 'ygg-swift--run)
             (lambda (&rest _)
               "{\"devices\":{\"com.apple.CoreSimulator.SimRuntime.watchOS-12-0\":[{\"state\":\"Booted\",\"udid\":\"W\",\"name\":\"Watch\"}],\"com.apple.CoreSimulator.SimRuntime.iOS-27-0\":[{\"state\":\"Booted\",\"udid\":\"S\",\"name\":\"iPhone 17\"}]}}")))
    (should (equal (ygg-swift-destination) '(:platform ios :id "S" :name "iPhone 17")))))

(ert-deftest swift-layer-device-destinations ()
  (should (equal (ygg-swift--destination-arg '(:platform ios-device :id "D")) "platform=iOS,id=D"))
  (should (equal (ygg-swift--destination-arg '(:platform macos)) "platform=macOS")))

(ert-deftest swift-layer-scheme-read-from-build-server-json ()
  (swift-layer-test--in-dir '("App.xcodeproj/")
    (with-temp-file (expand-file-name "buildServer.json" root)
      (insert "{\"name\":\"xcode build server\",\"scheme\":\"App\",\"kind\":\"xcode\"}"))
    (let ((ygg-swift--schemes (make-hash-table :test #'equal)))
      (should (equal (ygg-swift--scheme (ygg-swift-project)) "App")))))

(ert-deftest swift-layer-scheme-choice-writes-build-server ()
  (swift-layer-test--in-dir '("App.xcodeproj/")
    (let ((ygg-swift--schemes (make-hash-table :test #'equal))
          (written nil))
      (cl-letf (((symbol-function 'ygg-swift--listing) (lambda (_) '(:schemes ("App"))))
                ((symbol-function 'ygg-swift--write-build-server)
                 (lambda (_project scheme) (push scheme written))))
        (should (equal (ygg-swift--scheme (ygg-swift-project)) "App"))
        (should (equal written '("App")))
        (ygg-swift--scheme (ygg-swift-project))
        (should (equal written '("App")))))))

(ert-deftest swift-layer-app-settings-pick-the-app-target ()
  (swift-layer-test--in-dir '("App.xcodeproj/")
    (let ((ygg-swift--settings (make-hash-table :test #'equal))
          (ygg-swift--schemes (make-hash-table :test #'equal))
          (calls 0))
      (puthash (plist-get (ygg-swift-project) :container) "App" ygg-swift--schemes)
      (cl-letf (((symbol-function 'ygg-swift--run)
                 (lambda (&rest _)
                   (cl-incf calls)
                   "Command line invocation:\n[{\"target\":\"AppTests\",\"buildSettings\":{\"WRAPPER_EXTENSION\":\"xctest\"}},{\"target\":\"App\",\"buildSettings\":{\"WRAPPER_EXTENSION\":\"app\",\"BUILT_PRODUCTS_DIR\":\"/b/Debug-iphonesimulator\",\"FULL_PRODUCT_NAME\":\"App.app\",\"EXECUTABLE_PATH\":\"App.app/App\",\"PRODUCT_BUNDLE_IDENTIFIER\":\"dev.x.App\"}}]")))
        (let* ((device '(:platform ios :id "S"))
               (settings (ygg-swift--app-settings (ygg-swift-project) device)))
          (should (equal (ygg-swift--app-path settings) "/b/Debug-iphonesimulator/App.app"))
          (should (equal (ygg-swift--executable settings) "/b/Debug-iphonesimulator/App.app/App"))
          (ygg-swift--app-settings (ygg-swift-project) device)
          (should (= calls 1)))))))

(ert-deftest swift-layer-launch-for-debugger-gives-pid-and-program ()
  (swift-layer-test--in-dir '("App.xcodeproj/")
    (let (launched)
      (cl-letf (((symbol-function 'ygg-swift-destination) (lambda () '(:platform ios :id "S" :name "iPhone")))
                ((symbol-function 'ygg-swift--install)
                 (lambda (&rest _) '(:PRODUCT_BUNDLE_IDENTIFIER "dev.x.App" :BUILT_PRODUCTS_DIR "/b"
                                     :EXECUTABLE_PATH "App.app/App")))
                ((symbol-function 'ygg-swift--run)
                 (lambda (_dir &rest args) (setq launched args) "dev.x.App: 4242\n")))
        (should (equal (ygg-swift-launch-waiting-for-debugger) '(:pid 4242 :program "/b/App.app/App")))
        (should (member "--wait-for-debugger" launched))
        (should (equal (last launched 2) '("S" "dev.x.App")))))))

(ert-deftest swift-layer-build-command-runs-from-the-root ()
  (swift-layer-test--in-dir '("App.xcodeproj/")
    (let ((ygg-swift--schemes (make-hash-table :test #'equal)))
      (puthash (plist-get (ygg-swift-project) :container) "App" ygg-swift--schemes)
      (cl-letf (((symbol-function 'ygg-swift-destination) (lambda () '(:platform ios :id "S"))))
        (should (equal (ygg-swift-build-command)
                       (concat "cd " (shell-quote-argument root)
                               " && xcodebuild build -quiet -project App.xcodeproj -scheme App"
                               " -configuration Debug -destination "
                               (shell-quote-argument "platform=iOS Simulator,id=S"))))))))

(ert-deftest swift-layer-dape-ios-entry-builds-then-attaches ()
  (require 'ygg-dap-ios)
  (let ((dape-configs '((lldb-dap modes (c-mode) ensure dape-ensure-command command "lldb-dap"
                                  command-cwd dape-command-cwd :type "lldb-dap" :cwd "." :program "a.out"))))
    (ygg-dap-ios-install)
    (let ((entry (alist-get 'ios-simulator dape-configs)))
      (should (equal (plist-get entry :type) "lldb-dap"))
      (should (equal (plist-get entry :request) "attach"))
      (should-not (plist-member entry :program))
      (should (eq (plist-get entry 'compile) 'ygg-dap-ios-build))
      (should (equal (plist-get entry 'modes) '(swift-mode swift-ts-mode))))
    (let ((first (ygg-dap-ios-launch '(compile "xcodebuild" :request "attach"))))
      (should (plist-get first 'built))
      (should-not (plist-get first :pid))
      (cl-letf (((symbol-function 'ygg-swift-launch-waiting-for-debugger)
                 (lambda () '(:pid 7 :program "/b/App"))))
        (let ((second (ygg-dap-ios-launch first)))
          (should (= (plist-get second :pid) 7))
          (should (equal (plist-get second :program) "/b/App")))))))

(ert-deftest swift-layer-mcp-filter-joins-partial-lines ()
  (let ((proc (make-pipe-process :name "swift-layer-test" :noquery t))
        (got nil))
    (unwind-protect
        (progn
          (process-put proc 'partial "")
          (process-put proc 'pending (make-hash-table))
          (puthash 3 (lambda (m) (push m got)) (process-get proc 'pending))
          (ygg-swift--mcp-filter proc "{\"jsonrpc\":\"2.0\",\"id\":3,\"res")
          (should-not got)
          (ygg-swift--mcp-filter proc "ult\":{\"ok\":true}}\n{\"id\":")
          (should (equal (plist-get (plist-get (car got) :result) :ok) t))
          (should (equal (process-get proc 'partial) "{\"id\":")))
      (delete-process proc))))

(ert-deftest swift-layer-treesit-objects-in-swift-ts-mode ()
  (let ((treesit-extra-load-path (swift-layer-test--grammar-path)))
    (skip-unless (swift-layer-test--ts-mode-p))
    (require 'yggdrasil-match)
    (with-temp-buffer
      (insert "protocol Loader {\n    func load() -> Int\n}\n\nstruct Probe {\n    func fetch(url: String, retries: Int) -> Int {\n        return g(url, retries)\n    }\n}\n")
      (swift-ts-mode)
      (cl-flet ((text (c at)
                  (goto-char (point-min))
                  (search-forward at)
                  (let ((b (ygg-match--textobject-bounds c 'around)))
                    (and b (buffer-substring-no-properties (car b) (cdr b))))))
        (should (string-prefix-p "func fetch(" (text ?f "retr")))
        (should (string-prefix-p "struct Probe" (text ?t "retr")))
        (should (string-prefix-p "protocol Loader" (text ?t "load()")))
        (should (equal (text ?P "retr") "retries: Int"))))))

(ert-deftest swift-layer-split-arguments ()
  (let ((treesit-extra-load-path (swift-layer-test--grammar-path)))
    (skip-unless (swift-layer-test--ts-mode-p))
    (with-temp-buffer
      (insert "func f() {\n    load(url: u, retries: 2)\n}\n")
      (swift-ts-mode)
      (goto-char (point-min))
      (search-forward "retries")
      (ygg-swift-split-arguments)
      (should (equal (buffer-string)
                     "func f() {\n    load(\n        url: u,\n        retries: 2\n    )\n}\n")))))

(ert-deftest swift-layer-wrap-and-drop-init ()
  (with-temp-buffer
    (insert "let x = Foo.init(a: 1)\n")
    (goto-char (point-min))
    (ygg-swift-drop-init)
    (should (equal (buffer-string) "let x = Foo(a: 1)\n"))
    (save-excursion (insert "self.init(a: 1); super.init()\n"))
    (ygg-swift-drop-init)
    (should (looking-at-p "self.init(a: 1); super.init()"))
    (delete-region (point) (line-beginning-position 2))
    (ygg-swift-wrap-do-catch)
    (should (equal (buffer-string) "do {\nlet x = Foo(a: 1)\n} catch {\nprint(error)\n}\n"))))

(ert-deftest swift-layer-insert-todo-above ()
  (with-temp-buffer
    (insert "let x = 1\n")
    (goto-char (point-min))
    (ygg-swift-insert-todo)
    (should (equal (buffer-string) "// TODO: \nlet x = 1\n"))
    (should (eolp))))

(ert-deftest swift-layer-localleader-keys ()
  (dolist (mode '(swift-mode swift-ts-mode))
    (let ((map (ygg-localleader--get-map mode)))
      (dolist (pair '(("R" . ygg-swift-run-without-build) ("k" . ygg-swift-stop)
                      ("o" . ygg-swift-show-console) ("s" . ygg-swift-select-scheme)
                      ("f w" . ygg-swift-wrap)))
        (let ((binding (lookup-key map (kbd (car pair)))))
          (should (eq (if (consp binding) (cdr binding) binding) (cdr pair)))))
      (dolist (key '("D" "m d" "b" "r" "K" "t" "T" "p"))
        (should-not (commandp (cdr-safe (lookup-key map (kbd key)))))))))

;;; swift-layer-tests.el ends here
