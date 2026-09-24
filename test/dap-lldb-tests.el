;;; dap-lldb-tests.el --- Tests for lldb-dap debug configs -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dape)
(require 'ygg-dap-lldb)

(defmacro ygg-dap-lldb-test--in-project (files &rest body)
  "Run BODY in a temp project holding FILES, an alist of relative path to text."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "ygg-dap-lldb-" t)))
          (default-directory root)
          (dape-cwd-function (lambda () root)))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (let ((path (expand-file-name (car file) root)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))))
           ,@body)
       (delete-directory root t))))

(defmacro ygg-dap-lldb-test--with-tools (path-hit xcrun-answers &rest body)
  "Run BODY with lldb-dap on PATH at PATH-HIT and xcrun answering XCRUN-ANSWERS.
Binds xcrun-calls to the number of times xcrun ran."
  (declare (indent 2))
  `(let ((ygg-dap-lldb--xcrun-command nil)
         (answers ,xcrun-answers)
         (xcrun-calls 0))
     (cl-letf (((symbol-function 'executable-find)
                (lambda (name &optional _remote) (and (equal name "lldb-dap") ,path-hit)))
               ((symbol-function 'ygg-dap-lldb--xcrun)
                (lambda () (cl-incf xcrun-calls) (pop answers))))
       ,@body)))

(ert-deftest ygg-dap-lldb-path-wins-over-xcrun ()
  (ygg-dap-lldb-test--with-tools "/opt/llvm/bin/lldb-dap" '("/xcode/lldb-dap")
    (should (equal (ygg-dap-lldb-local-command) "/opt/llvm/bin/lldb-dap"))
    (should (= xcrun-calls 0))))

(ert-deftest ygg-dap-lldb-xcrun-answer-is-cached ()
  (ygg-dap-lldb-test--with-tools nil '("/xcode/lldb-dap")
    (should (equal (ygg-dap-lldb-local-command) "/xcode/lldb-dap"))
    (should (equal (ygg-dap-lldb-local-command) "/xcode/lldb-dap"))
    (should (= xcrun-calls 1))))

(ert-deftest ygg-dap-lldb-xcrun-failure-is-retried ()
  (ygg-dap-lldb-test--with-tools nil '(nil "/xcode/lldb-dap")
    (should (equal (ygg-dap-lldb-local-command) "lldb-dap"))
    (should (equal (ygg-dap-lldb-local-command) "/xcode/lldb-dap"))
    (should (= xcrun-calls 2))))

(ert-deftest ygg-dap-lldb-remote-launch-never-asks-xcrun ()
  (ygg-dap-lldb-test--with-tools nil '("/xcode/lldb-dap")
    (let ((default-directory "/ssh:box:/src/"))
      (should-error (ygg-dap-lldb-command) :type 'user-error)
      (should (= xcrun-calls 0)))))

(ert-deftest ygg-dap-lldb-remote-launch-uses-the-hosts-lldb-dap ()
  (ygg-dap-lldb-test--with-tools "/usr/bin/lldb-dap" nil
    (let ((default-directory "/ssh:box:/src/"))
      (should (equal (ygg-dap-lldb-command) "/usr/bin/lldb-dap")))))

(ert-deftest ygg-dap-lldb-program-is-the-crate-binary ()
  (ygg-dap-lldb-test--in-project
      '(("Cargo.toml" . "[workspace]\nmembers = []\n\n[package]\nversion = \"0.1.0\"\nname = \"hello-cli\"\n"))
    (should (equal (ygg-dap-lldb-program) "target/debug/hello-cli"))))

(ert-deftest ygg-dap-lldb-program-is-the-swiftpm-product ()
  (ygg-dap-lldb-test--in-project
      '(("Package.swift" . "let package = Package(\n  name: \"Pkg\",\n  targets: [.executableTarget(name: \"tool\")]\n)\n"))
    (should (equal (ygg-dap-lldb-program) ".build/debug/tool"))))

(ert-deftest ygg-dap-lldb-program-is-the-binary-beside-the-file ()
  (ygg-dap-lldb-test--in-project '(("main.c" . "") ("main" . ""))
    (set-file-modes (expand-file-name "main" root) #o755)
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "main.c" root))
      (should (equal (ygg-dap-lldb-program) "main")))))

(ert-deftest ygg-dap-lldb-program-falls-back-to-a-out ()
  (ygg-dap-lldb-test--in-project '(("main.c" . ""))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "main.c" root))
      (should (equal (ygg-dap-lldb-program) "a.out")))))

(ert-deftest ygg-dap-lldb-install-extends-the-shipped-entry ()
  (let ((dape-configs (copy-tree dape-configs)))
    (ygg-dap-lldb-install)
    (let ((entry (alist-get 'lldb-dap dape-configs)))
      (should (memq 'swift-mode (plist-get entry 'modes)))
      (should (memq 'swift-ts-mode (plist-get entry 'modes)))
      (should (memq 'c-ts-mode (plist-get entry 'modes)))
      (should (eq (plist-get entry 'command) 'ygg-dap-lldb-command))
      (should (eq (plist-get entry 'ensure) 'dape-ensure-command))
      (should (equal (plist-get entry :type) "lldb-dap")))))

(ert-deftest ygg-dap-lldb-install-leaves-lldb-vscode-alone-and-is-idempotent ()
  (let* ((dape-configs (copy-tree dape-configs))
         (vscode (copy-tree (alist-get 'lldb-vscode dape-configs))))
    (ygg-dap-lldb-install)
    (ygg-dap-lldb-install)
    (should (equal (alist-get 'lldb-vscode dape-configs) vscode))
    (should (= 1 (cl-count 'swift-mode (plist-get (alist-get 'lldb-dap dape-configs) 'modes))))
    (should (= 1 (cl-count 'lldb-dap-docker dape-configs :key #'car)))))

(ert-deftest ygg-dap-lldb-attach-entries-come-after-the-launch-entry ()
  (let ((dape-configs (copy-tree dape-configs)))
    (ygg-dap-lldb-install)
    (let ((names (mapcar #'car dape-configs)))
      (should (< (cl-position 'lldb-dap names) (cl-position 'lldb-dap-remote names)))
      (should (< (cl-position 'lldb-dap names) (cl-position 'lldb-dap-docker names))))))

(ert-deftest ygg-dap-lldb-attach-entries-run-lldb-dap-locally ()
  (let ((dape-configs (copy-tree dape-configs)))
    (ygg-dap-lldb-install)
    (dolist (name '(lldb-dap-remote lldb-dap-docker))
      (let ((entry (alist-get name dape-configs)))
        (should (equal (plist-get entry :request) "attach"))
        (should (eq (plist-get entry 'command) 'ygg-dap-lldb-local-command))
        (should (eq (plist-get entry 'command-cwd) 'ygg-dap-lldb-attach-cwd))
        (should (eq (plist-get entry 'fn) 'ygg-dap-lldb-attach-resolve))
        (should-not (plist-member entry :cwd))))
    (should (eq (plist-get (alist-get 'lldb-dap-docker dape-configs) 'remote-root)
                'ygg-dap-lldb-docker-root))
    (should (eq (plist-get (alist-get 'lldb-dap-remote dape-configs) 'remote-root)
                'ygg-dap-lldb-ssh-root))))

(ert-deftest ygg-dap-lldb-local-checkout-maps-container-sources-with-source-map ()
  (should (equal (ygg-dap-lldb--attach-settings "/docker:app:/src/" "/Users/me/app/" "/Users/me/app/")
                 '(:gdb-remote-hostname "localhost"
                   :sourceMap [["/src" "/Users/me/app"]]))))

(ert-deftest ygg-dap-lldb-local-checkout-reaches-an-ssh-host-by-name ()
  (should (equal (ygg-dap-lldb--attach-settings "/ssh:dev@box:/home/dev/app" "/Users/me/app/" "/Users/me/app")
                 '(:gdb-remote-hostname "box"
                   :sourceMap [["/home/dev/app" "/Users/me/app"]]))))

(ert-deftest ygg-dap-lldb-visited-tramp-sources-open-through-tramp ()
  (should (equal (ygg-dap-lldb--attach-settings "/docker:app:/src/" "/docker:app:/src/lib/" "/src/")
                 '(:gdb-remote-hostname "localhost"
                   prefix-local "/docker:app:" prefix-remote ""))))

(ert-deftest ygg-dap-lldb-remote-root-comes-from-the-dir-local ()
  (with-temp-buffer
    (setq default-directory "/tmp/")
    (setq-local ygg-dap-lldb-remote-root "/docker:app:/src")
    (should (equal (ygg-dap-lldb-docker-root) "/docker:app:/src"))
    (should-not (ygg-dap-lldb-ssh-root))
    (setq-local ygg-dap-lldb-remote-root "/ssh:box:/src")
    (should (equal (ygg-dap-lldb-ssh-root) "/ssh:box:/src"))
    (should-not (ygg-dap-lldb-docker-root))))

(ert-deftest ygg-dap-lldb-remote-root-prefers-the-visited-tramp-project ()
  (with-temp-buffer
    (setq default-directory "/docker:app:/src/lib/")
    (setq-local ygg-dap-lldb-remote-root "/docker:other:/x")
    (let ((dape-cwd-function (lambda () "/docker:app:/src/")))
      (should (equal (ygg-dap-lldb-docker-root) "/docker:app:/src/"))
      (should (equal (ygg-dap-lldb-attach-cwd) temporary-file-directory)))))

(ert-deftest ygg-dap-lldb-attach-without-a-root-says-how-to-give-one ()
  (should-error (ygg-dap-lldb-attach-resolve '(remote-root nil :request "attach"))
                :type 'user-error))

(ert-deftest ygg-dap-lldb-attach-keeps-given-options-and-a-local-program ()
  (ygg-dap-lldb-test--in-project '(("main.linux" . ""))
    (let* ((program (expand-file-name "main.linux" root))
           (config (ygg-dap-lldb-attach-resolve
                    `(remote-root "/docker:app:/src" :program ,program
                      :gdb-remote-hostname "10.0.0.5"))))
      (should (equal (plist-get config :program) program))
      (should (equal (plist-get config :gdb-remote-hostname) "10.0.0.5"))
      (should (equal (plist-get config :sourceMap)
                     `[["/src" ,(directory-file-name root)]])))))

(ert-deftest ygg-dap-lldb-attach-fetches-the-remote-program ()
  (let (copied)
    (cl-letf (((symbol-function 'file-regular-p) (lambda (_) t))
              ((symbol-function 'copy-file)
               (lambda (from to &rest _) (setq copied (list from to)))))
      (let ((local (ygg-dap-lldb--fetch "build/app" "/docker:app:/src/")))
        (should (equal (car copied) "/docker:app:/src/build/app"))
        (should (equal (cadr copied) local))
        (should-not (file-remote-p local))))))

;;; dap-lldb-tests.el ends here
