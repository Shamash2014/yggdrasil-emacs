;;; eglot-x-tests.el --- Tests for the eglot-x integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'eglot)
(require 'eglot-x)
(require 'dape)
(require 'ygg-eglot-x)
(require 'eglot-tempel)

(defun eglot-x-test--runnable (label cargo-args executable-args &optional range)
  `(:label ,label :kind "cargo"
    ,@(when range `(:location (:targetUri "file:///p/src/main.rs" :targetRange ,range)))
    :args (:environment (:RUSTC_TOOLCHAIN "/tc")
           :cwd "/p" :workspaceRoot "/p" :overrideCargo nil
           :cargoArgs ,(vconcat cargo-args)
           :executableArgs ,(vconcat executable-args))))

(defconst eglot-x-test--test-fn
  (eglot-x-test--runnable "test tests::adds" '("test" "--package" "p" "--bin" "p")
                          '("tests::adds" "--exact" "--nocapture") '(14 . 18)))

(defconst eglot-x-test--test-mod
  (eglot-x-test--runnable "test-mod tests" '("test" "--package" "p" "--bin" "p")
                          '("tests" "--nocapture") '(10 . 25)))

(defconst eglot-x-test--main
  (eglot-x-test--runnable "run p" '("run" "--package" "p" "--bin" "p") '() '(5 . 8)))

(defconst eglot-x-test--workspace-test
  (eglot-x-test--runnable "cargo test -p p --all-targets" '("test" "--package" "p" "--all-targets") '()))

(defun eglot-x-test--extent (runnable)
  (plist-get (plist-get runnable :location) :targetRange))

(ert-deftest eglot-x-files-extension-stays-off-to-keep-tramp ()
  (let (eglot-x-enable-files)
    (setq eglot-x-enable-files t)
    (ygg-eglot-x-configure)
    (should-not eglot-x-enable-files)))

(ert-deftest eglot-x-chosen-extensions ()
  (ygg-eglot-x-configure)
  (should eglot-x-enable-encoding-negotiation)
  (should eglot-x-enable-server-status)
  (should eglot-x-enable-local-docs-support)
  (should-not eglot-x-enable-snippet-text-edit)
  (should-not eglot-x-enable-colored-diagnostics)
  (should-not eglot-x-enable-hover-actions)
  (should-not eglot-x-client-commands))

(ert-deftest eglot-x-innermost-runnable-picks-the-test-fn-over-its-module ()
  (should (eq (ygg-eglot-x--innermost
               (list eglot-x-test--test-mod eglot-x-test--test-fn eglot-x-test--workspace-test)
               16 #'eglot-x-test--extent)
              eglot-x-test--test-fn)))

(ert-deftest eglot-x-innermost-runnable-in-main-is-the-binary ()
  (should (eq (ygg-eglot-x--innermost
               (list eglot-x-test--main eglot-x-test--test-mod eglot-x-test--workspace-test)
               7 #'eglot-x-test--extent)
              eglot-x-test--main)))

(ert-deftest eglot-x-runnables-without-a-location-or-for-doctests-are-skipped ()
  (let ((doctest (eglot-x-test--runnable "doctest" '("test" "--doc" "--package" "p") '("add") '(1 . 3))))
    (should-not (ygg-eglot-x--innermost (list eglot-x-test--workspace-test doctest)
                                        2 #'eglot-x-test--extent))))

(ert-deftest eglot-x-test-builds-without-running ()
  (should (equal (ygg-eglot-x--build-args eglot-x-test--test-fn)
                 '("test" "--package" "p" "--bin" "p" "--no-run"))))

(ert-deftest eglot-x-run-becomes-build ()
  (should (equal (ygg-eglot-x--build-args eglot-x-test--main)
                 '("build" "--package" "p" "--bin" "p"))))

(ert-deftest eglot-x-test-filter-is-exact-and-uncaptured ()
  (should (equal (ygg-eglot-x--program-args eglot-x-test--test-fn)
                 '("tests::adds" "--exact" "--nocapture")))
  (let ((shown (eglot-x-test--runnable "t" '("test") '("tests::adds" "--exact" "--show-output") '(1 . 2))))
    (should (equal (ygg-eglot-x--program-args shown)
                   '("tests::adds" "--exact" "--show-output" "--nocapture")))))

(defconst eglot-x-test--cargo-output
  (mapconcat
   #'identity
   '("{\"reason\":\"compiler-artifact\",\"target\":{\"kind\":[\"lib\"]},\"profile\":{\"test\":false},\"executable\":null}"
     "{\"reason\":\"compiler-artifact\",\"target\":{\"kind\":[\"bin\"]},\"profile\":{\"test\":true},\"executable\":\"/p/target/debug/deps/p-abc\"}"
     "{\"reason\":\"compiler-artifact\",\"target\":{\"kind\":[\"bin\"]},\"profile\":{\"test\":false},\"executable\":\"/p/target/debug/p\"}"
     "{\"reason\":\"build-finished\",\"success\":true}")
   "\n"))

(ert-deftest eglot-x-executable-for-a-test-is-the-harness ()
  (should (equal (ygg-eglot-x--executable eglot-x-test--cargo-output t)
                 "/p/target/debug/deps/p-abc")))

(ert-deftest eglot-x-executable-for-a-run-is-the-binary ()
  (should (equal (ygg-eglot-x--executable eglot-x-test--cargo-output nil)
                 "/p/target/debug/p")))

(ert-deftest eglot-x-executable-for-an-example-run ()
  (should (equal (ygg-eglot-x--executable
                  "{\"reason\":\"compiler-artifact\",\"target\":{\"kind\":[\"example\"]},\"profile\":{\"test\":false},\"executable\":\"/p/target/debug/examples/demo\"}"
                  nil)
                 "/p/target/debug/examples/demo")))

(ert-deftest eglot-x-environment-as-assignments ()
  (should (equal (ygg-eglot-x--environment eglot-x-test--test-fn)
                 '("RUSTC_TOOLCHAIN=/tc"))))

(defmacro eglot-x-test--with-lldb-dap (&rest body)
  (declare (indent 0))
  `(let ((dape-configs '((lldb-dap modes (rust-ts-mode) command "/xc/lldb-dap"
                                   ensure dape-ensure-command
                                   :type "lldb-dap" :cwd "." :program "a.out")
                         (rust-test modes (rust-mode rust-ts-mode) fn ygg-eglot-x-rust-test-config)))
         (ygg-eglot-x--compiling nil))
     (cl-letf (((symbol-function 'ygg-eglot-x--rust-formatter-commands) (lambda (_) ["import"]))
               ((symbol-function 'ygg-eglot-x--runnable-at-point) (lambda () eglot-x-test--test-fn)))
       ,@body)))

(ert-deftest eglot-x-rust-test-derives-from-lldb-dap-and-compiles-first ()
  (eglot-x-test--with-lldb-dap
    (let ((config (ygg-eglot-x-rust-test-config (dape--config-eval 'rust-test nil))))
      (should (equal (plist-get config 'command) "/xc/lldb-dap"))
      (should (equal (plist-get config :type) "lldb-dap"))
      (should (eq (plist-get config 'fn) 'ygg-eglot-x-rust-test-config))
      (should (equal (plist-get config 'compile)
                     "env RUSTC_TOOLCHAIN\\=/tc cargo test --package p --bin p --no-run"))
      (should (equal (plist-get config 'command-cwd) "/p/"))
      (should (equal (plist-get config :args) ["tests::adds" "--exact" "--nocapture"]))
      (should (equal (plist-get config :cwd) "/p/"))
      (should (equal (plist-get config :env) ["RUSTC_TOOLCHAIN=/tc"]))
      (should (equal (plist-get config :initCommands) ["import"]))
      (should (memql (plist-get config 'ygg-eglot-x-build) ygg-eglot-x--compiling)))))

(ert-deftest eglot-x-rust-test-after-compiling-launches-the-built-executable ()
  (eglot-x-test--with-lldb-dap
    (let ((prepared (ygg-eglot-x-rust-test-config (dape--config-eval 'rust-test nil))))
      (cl-letf (((symbol-function 'ygg-eglot-x--built-executable)
                 (lambda (runnable) (and (equal runnable eglot-x-test--test-fn) "/p/target/debug/deps/p-abc"))))
        (let ((resumed (ygg-eglot-x-rust-test-config (copy-tree prepared))))
          (should (equal (plist-get resumed :program) "/p/target/debug/deps/p-abc"))
          (should-not ygg-eglot-x--compiling))))))

(ert-deftest eglot-x-rust-test-restart-rebuilds-without-asking-the-server ()
  (eglot-x-test--with-lldb-dap
    (let* ((prepared (ygg-eglot-x-rust-test-config (dape--config-eval 'rust-test nil)))
           (finished (plist-put (copy-tree prepared) :program "/p/target/debug/deps/p-abc")))
      (setq ygg-eglot-x--compiling nil)
      (cl-letf (((symbol-function 'ygg-eglot-x--runnable-at-point)
                 (lambda () (error "Restart asked rust-analyzer"))))
        (let ((restarted (ygg-eglot-x-rust-test-config finished)))
          (should (plist-get restarted 'compile))
          (should (memql (plist-get restarted 'ygg-eglot-x-build) ygg-eglot-x--compiling)))))))

(ert-deftest eglot-x-moved-item-keeps-its-text-verbatim ()
  (with-temp-buffer
    (insert "fn first() {}\n\nfn tricky() { \"a\\\\b $1 ${2:x} }\" }\n")
    (let ((moved "fn tricky() { \"a\\\\b $1 ${2:x} }\" }"))
      (ygg-eglot-x--apply-as-text
       (vector `(:range (:start (:line 0 :character 0) :end (:line 0 :character 13))
                 :newText ,(concat "fn tricky$0" (substring moved 9)) :insertTextFormat 2)
               `(:range (:start (:line 2 :character 3) :end (:line 2 :character 9))
                 :newText "first" :insertTextFormat 2)
               `(:range (:start (:line 2 :character 12) :end (:line 2 :character ,(length moved)))
                 :newText "{}" :insertTextFormat 2)))
      (should (equal (buffer-string) (concat moved "\n\nfn first() {}\n")))
      (should (looking-at "() {")))))

(defmacro eglot-x-test--with-tempel (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (eglot-tempel-mode 1)
              (ygg-eglot-x-install)
              ,@body)
     (advice-remove 'eglot-x--apply-text-edits #'ygg-eglot-x--apply-as-text)
     (eglot-tempel-mode -1)))

(ert-deftest eglot-x-snippet-expansion-is-tempel-backed ()
  (eglot-x-test--with-tempel
    (should (eq (eglot--snippet-expansion-fn) #'eglot-tempel-expand-yas-snippet))
    (should (advice-member-p #'ygg-eglot-x--apply-as-text 'eglot-x--apply-text-edits))))

(ert-deftest eglot-x-moved-macro-keeps-its-dollars-under-tempel ()
  (with-temp-buffer
    (let ((moved "macro_rules! m { ($x:expr) => { $x + ${1} }; }"))
      (insert "fn first() {}\n\n" moved "\n")
      (eglot-x-test--with-tempel
        (eglot-x--apply-text-edits
         (vector `(:range (:start (:line 0 :character 0) :end (:line 0 :character 13))
                   :newText ,(concat "$0" moved) :insertTextFormat 2)
                 `(:range (:start (:line 2 :character 0) :end (:line 2 :character ,(length moved)))
                   :newText "fn first() {}" :insertTextFormat 2))))
      (should (equal (buffer-string) (concat moved "\n\nfn first() {}\n")))
      (should (bobp))
      (should-not (bound-and-true-p tempel--active)))))

(ert-deftest eglot-x-keys-live-under-m-in-rust ()
  (dolist (mode '(rust-ts-mode rust-mode))
    (let ((map (ygg-localleader--get-map mode)))
      (should-not (lookup-key map (kbd "m T")))
      (should (eq (lookup-key map (kbd "m r")) #'eglot-x-ask-runnables))
      (should (eq (lookup-key map (kbd "m e")) #'eglot-x-expand-macro))
      (should (eq (lookup-key map (kbd "m k")) #'eglot-x-move-item-up))
      (should (eq (lookup-key map (kbd "m j")) #'eglot-x-move-item-down))
      (should (eq (lookup-key map (kbd "m J")) #'eglot-x-join-lines))
      (should-not (lookup-key map (kbd "T"))))))

(ert-deftest eglot-x-keys-leave-cargo-keys-alone ()
  (let ((map (ygg-localleader--get-map 'rust-ts-mode)))
    (should-not (lookup-key map (kbd "r")))
    (should-not (lookup-key map (kbd "t")))
    (should (eq (lookup-key map (kbd "c")) #'ygg-localleader-rust-check))))

;;; eglot-x-tests.el ends here
