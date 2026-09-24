;;; debugpy-tests.el --- Tests for Python debug configs -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dape)
(require 'ygg-debugpy)

(defmacro ygg-debugpy-test--in-project (files &rest body)
  "Run BODY in a temp project holding FILES, an alist of relative path to text."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "ygg-debugpy-" t)))
          (default-directory root)
          (project-find-functions (list (lambda (_dir) (cons 'transient root)))))
     (unwind-protect
         (progn
           (dolist (file ,files)
             (let ((path (expand-file-name (car file) root)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))
               (when (string-suffix-p "bin/python" path)
                 (set-file-modes path #o755))))
           ,@body)
       (delete-directory root t))))

(defmacro ygg-debugpy-test--with-debugpy (pythons &rest body)
  "Run BODY believing only the interpreters in PYTHONS carry debugpy."
  (declare (indent 1))
  `(let ((ygg-debugpy-importable-function (lambda (python) (member python ,pythons)))
         (ygg-debugpy-adapter-pythons '("/tool/bin/python" "/kernel/bin/python")))
     ,@body))

(ert-deftest ygg-debugpy-program-runs-on-the-project-venv ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . "") ("app.py" . ""))
    (should (equal (ygg-debugpy--python (ygg-debugpy-venv))
                   (expand-file-name ".venv/bin/python" root)))))

(ert-deftest ygg-debugpy-venv-with-debugpy-runs-the-adapter-too ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . ""))
    (let ((venv (expand-file-name ".venv/bin/python" root)))
      (ygg-debugpy-test--with-debugpy (list venv "/tool/bin/python")
        (should (equal (ygg-debugpy-adapter-python venv) venv))))))

(ert-deftest ygg-debugpy-venv-without-debugpy-borrows-the-first-adapter-that-has-it ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . ""))
    (ygg-debugpy-test--with-debugpy '("/kernel/bin/python")
      (let ((config (ygg-debugpy-resolve '(command "python" :request "launch"))))
        (should (equal (plist-get config 'command) "/kernel/bin/python"))
        (should (equal (plist-get config :python)
                       (expand-file-name ".venv/bin/python" root)))
        (should (equal (plist-get (plist-get config :env) :VIRTUAL_ENV)
                       (expand-file-name ".venv" root)))))))

(ert-deftest ygg-debugpy-the-adapter-listens-on-loopback-only ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . ""))
    (ygg-debugpy-test--with-debugpy '("/tool/bin/python")
      (let ((args (plist-get (ygg-debugpy-resolve
                              `(command "python" :request "launch"
                                        command-args ,(plist-get (alist-get 'debugpy dape-configs)
                                                                 'command-args)))
                             'command-args)))
        (should (member "127.0.0.1" args))
        (should-not (member "0.0.0.0" args))))))

(ert-deftest ygg-debugpy-without-venv-runs-python3-from-path ()
  (ygg-debugpy-test--in-project '(("app.py" . ""))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &rest _) (and (equal name "python3") "/usr/bin/python3"))))
      (ygg-debugpy-test--with-debugpy '("/tool/bin/python")
        (let ((config (ygg-debugpy-resolve '(command "python" :request "launch"))))
          (should (equal (plist-get config :python) "/usr/bin/python3"))
          (should (equal (plist-get config 'command) "/tool/bin/python"))
          (should-not (plist-member config :env)))))))

(ert-deftest ygg-debugpy-without-any-debugpy-says-how-to-get-one ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . ""))
    (ygg-debugpy-test--with-debugpy nil
      (should-error (ygg-debugpy-resolve '(command "python")) :type 'user-error))))

(ert-deftest ygg-debugpy-a-venv-above-the-project-is-not-the-projects ()
  (let* ((outer (file-name-as-directory (make-temp-file "ygg-debugpy-outer-" t)))
         (inner (file-name-as-directory (expand-file-name "proj" outer))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".venv/bin" outer) t)
          (with-temp-file (expand-file-name ".venv/bin/python" outer))
          (set-file-modes (expand-file-name ".venv/bin/python" outer) #o755)
          (make-directory inner t)
          (let ((default-directory inner)
                (project-find-functions (list (lambda (_dir) (cons 'transient inner)))))
            (should-not (ygg-debugpy-venv))))
      (delete-directory outer t))))

(ert-deftest ygg-debugpy-attach-probes-no-interpreter ()
  (ygg-debugpy-test--in-project '(("app.py" . ""))
    (cl-letf (((symbol-function 'ygg-debugpy--python)
               (lambda (_) (error "Attach looked for an interpreter"))))
      (should (ygg-debugpy-resolve '(host "127.0.0.1" port 5678 :request "attach"))))))

(ert-deftest ygg-debugpy-launch-from-a-remote-buffer-is-refused ()
  (let ((default-directory "/ssh:nowhere:/srv/app/"))
    (should-error (ygg-debugpy-resolve '(command "python" :request "launch"))
                  :type 'user-error)))

(ert-deftest ygg-debugpy-launch-json-base-functions-are-called-and-its-target-wins ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . "") ("app.py" . ""))
    (ygg-debugpy-test--with-debugpy '("/tool/bin/python")
      (let ((config (ygg-debugpy-resolve
                     '(command "python" :request "launch" :cwd dape-cwd
                               :program dape-buffer-default :module "app" :args []))))
        (should (equal (plist-get config :cwd) root))
        (should (equal (plist-get config :module) "app"))
        (should-not (plist-member config :program))
        (should (equal (plist-get config :args) []))))))

(ert-deftest ygg-debugpy-a-failed-probe-is-tried-again ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . "#!/bin/sh\n[ -f \"$0.ok\" ] && dirname \"$0\"\n"))
    (let ((ygg-debugpy--importable (make-hash-table :test #'equal))
          (python (expand-file-name ".venv/bin/python" root)))
      (should-not (ygg-debugpy--importable-p python))
      (with-temp-file (concat python ".ok"))
      (should (ygg-debugpy--importable-p python))
      (delete-file (concat python ".ok"))
      (should (ygg-debugpy--importable-p python)))))

(ert-deftest ygg-debugpy-a-cached-success-lapses-when-its-debugpy-is-gone ()
  (let ((ygg-debugpy--importable (make-hash-table :test #'equal)))
    (puthash "/gone/bin/python" "/gone/lib/debugpy" ygg-debugpy--importable)
    (should-not (ygg-debugpy--importable-p "/gone/bin/python"))
    (should-not (gethash "/gone/bin/python" ygg-debugpy--importable))))

(ert-deftest ygg-debugpy-launch-json-attach-sheds-the-launch-keys ()
  (ygg-debugpy-test--with-debugpy '("/tool/bin/python")
    (with-temp-buffer
      (let ((config (ygg-debugpy-resolve
                     '(command "python" :request "attach" :program dape-buffer-default
                               :args [] :console "integratedTerminal"
                               :connect (:host "127.0.0.1" :port 5678)))))
        (should-not (plist-member config :program))
        (should-not (plist-member config :args))
        (should-not (plist-member config :console))
        (should (equal (plist-get config 'command) "/tool/bin/python"))))))

(ert-deftest ygg-debugpy-a-module-typed-at-the-prompt-wins-over-the-file ()
  (should (equal (ygg-debugpy--settle '(:program "a.py" :module "pkg" :args []))
                 '(:module "pkg" :args []))))

(ert-deftest ygg-debugpy-a-launch-json-path-is-kept ()
  (let ((env (ygg-debugpy--venv-env "/p/.venv/" '(:PATH "/opt/tools/bin"))))
    (should (equal (plist-get env :PATH) "/opt/tools/bin"))
    (should (equal (plist-get env :VIRTUAL_ENV) "/p/.venv"))
    (should (= (length env) 4))))

(ert-deftest ygg-debugpy-launch-json-python-is-kept-unless-it-is-a-vscode-command ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . ""))
    (ygg-debugpy-test--with-debugpy '("/tool/bin/python")
      (should (equal (plist-get (ygg-debugpy-resolve '(:python "/opt/py")) :python)
                     "/opt/py"))
      (should (equal (plist-get (ygg-debugpy-resolve
                                 '(:python "${command:python.interpreterPath}"))
                                :python)
                     (expand-file-name ".venv/bin/python" root))))))

(ert-deftest ygg-debugpy-attach-spawns-nothing-and-sets-no-interpreter ()
  (ygg-debugpy-test--in-project '((".venv/bin/python" . ""))
    (ygg-debugpy-test--with-debugpy '("/tool/bin/python")
      (let ((config (ygg-debugpy-resolve '(host "127.0.0.1" port 5678 :request "attach"))))
        (should-not (plist-member config 'command))
        (should-not (plist-member config :python))))))

(ert-deftest ygg-debugpy-just-my-code-follows-the-toggle-unless-the-config-says ()
  (ygg-debugpy-test--in-project '(("app.py" . ""))
    (with-current-buffer (find-file-noselect (expand-file-name "app.py" root))
      (unwind-protect
          (let ((ygg-debugpy-just-my-code nil))
            (should-not (plist-get (dape--config-eval 'debugpy nil) :justMyCode))
            (should (eq (plist-get (dape--config-eval 'debugpy '(:justMyCode t)) :justMyCode) t))
            (should (eq (plist-get (ygg-debugpy--settle
                                    '(:request "attach" :justMyCode ygg-debugpy-just-my-code))
                                   :justMyCode)
                        nil)))
        (kill-buffer)))))

(ert-deftest ygg-debugpy-module-name-drops-src-and-main ()
  (ygg-debugpy-test--in-project '(("src/pkg/sub/mod.py" . "") ("pkg/__main__.py" . "")
                                  ("pkg/__init__.py" . "") ("__main__.py" . ""))
    (with-current-buffer (find-file-noselect (expand-file-name "pkg/__init__.py" root))
      (unwind-protect (should (equal (ygg-debugpy-module-name) "pkg.__init__"))
        (kill-buffer)))
    (with-current-buffer (find-file-noselect (expand-file-name "__main__.py" root))
      (unwind-protect (should-error (ygg-debugpy-module-name) :type 'user-error)
        (kill-buffer)))
    (with-current-buffer (find-file-noselect (expand-file-name "src/pkg/sub/mod.py" root))
      (unwind-protect (should (equal (ygg-debugpy-module-name) "pkg.sub.mod"))
        (kill-buffer)))
    (with-current-buffer (find-file-noselect (expand-file-name "pkg/__main__.py" root))
      (unwind-protect (should (equal (ygg-debugpy-module-name) "pkg"))
        (kill-buffer)))))

(defconst ygg-debugpy-test--source
  "import pytest

def helper():
    return 1

@pytest.mark.parametrize(\"n\", [1, 2])
def test_top(n):
    def inner():
        return n
    assert inner() == n

class TestGroup:
    def test_method(self):
        assert helper() == 1

    class TestNested:
        def test_deep(self):
            pass
")

(defun ygg-debugpy-test--node-id-at (needle)
  "The node id with point at the start of NEEDLE in the sample test file."
  (ygg-debugpy-test--in-project `((".venv/bin/python" . "")
                                  ("tests/test_sample.py" . ,ygg-debugpy-test--source))
    (with-current-buffer (find-file-noselect (expand-file-name "tests/test_sample.py" root))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (search-forward needle)
            (goto-char (match-beginning 0))
            (ygg-debugpy-test-node-id))
        (kill-buffer)))))

(ert-deftest ygg-debugpy-node-id-names-the-test-under-point ()
  (skip-unless (treesit-language-available-p 'python))
  (should (equal (ygg-debugpy-test--node-id-at "assert inner")
                 "tests/test_sample.py::test_top"))
  (should (equal (ygg-debugpy-test--node-id-at "return n")
                 "tests/test_sample.py::test_top"))
  (should (equal (ygg-debugpy-test--node-id-at "@pytest")
                 "tests/test_sample.py::test_top"))
  (should (equal (ygg-debugpy-test--node-id-at "assert helper")
                 "tests/test_sample.py::TestGroup::test_method"))
  (should (equal (ygg-debugpy-test--node-id-at "class TestGroup")
                 "tests/test_sample.py::TestGroup"))
  (should (equal (ygg-debugpy-test--node-id-at "pass")
                 "tests/test_sample.py::TestGroup::TestNested::test_deep"))
  (should (equal (ygg-debugpy-test--node-id-at "import pytest")
                 "tests/test_sample.py")))

(ert-deftest ygg-debugpy-node-id-from-leading-whitespace ()
  (skip-unless (treesit-language-available-p 'python))
  (should (equal (ygg-debugpy-test--node-id-at "    def test_method")
                 "tests/test_sample.py::TestGroup::test_method")))

(defun ygg-debugpy-test--dape-entry (name)
  "NAME's entry as dape ships it."
  (alist-get name (eval (car (get 'dape-configs 'standard-value)) t)))

(defun ygg-debugpy-test--without (plist keys)
  (cl-loop for (key value) on plist by #'cddr
           unless (memq key keys) append (list key value)))

(ert-deftest ygg-debugpy-dape-entries-stay-dapes-with-our-fn-composed ()
  (dolist (name '(debugpy debugpy-module))
    (let ((ours (alist-get name dape-configs)))
      (should (equal (ygg-debugpy-test--without ours '(fn ensure :justMyCode :module))
                     (ygg-debugpy-test--without (ygg-debugpy-test--dape-entry name)
                                                '(ensure :justMyCode :module))))
      (should (equal (plist-get ours 'fn) '(ygg-debugpy-resolve)))
      (should (eq (plist-get ours 'ensure) 'ygg-debugpy-ensure))
      (should (eq (plist-get ours :justMyCode) 'ygg-debugpy-just-my-code))))
  (should (eq (plist-get (alist-get 'debugpy-module dape-configs) :module)
              'ygg-debugpy-module-name)))

(ert-deftest ygg-debugpy-install-twice-composes-once ()
  (let ((dape-configs (copy-tree dape-configs))
        (ygg-debugpy--dape-ensure ygg-debugpy--dape-ensure))
    (ygg-debugpy-install)
    (should (equal (plist-get (alist-get 'debugpy dape-configs) 'fn) '(ygg-debugpy-resolve)))
    (should-not (eq ygg-debugpy--dape-ensure 'ygg-debugpy-ensure))))

(ert-deftest ygg-debugpy-test-and-attach-are-derived-from-dapes-entries ()
  (let ((test (alist-get 'debugpy-test dape-configs))
        (attach (alist-get 'debugpy-attach dape-configs)))
    (should (equal (ygg-debugpy-test--without test '(:module :args))
                   (ygg-debugpy-test--without (alist-get 'debugpy dape-configs)
                                              '(:program :args))))
    (should (equal (plist-get test :module) "pytest"))
    (should (eq (plist-get test :args) 'ygg-debugpy-test-args))
    (should (equal (plist-get attach 'host) (plist-get (ygg-debugpy-test--dape-entry 'attach) 'host)))
    (should (equal (plist-get attach 'port) 5678))
    (should-not (plist-member attach 'command))))

(ert-deftest ygg-debugpy-the-prompt-probes-no-python ()
  (cl-letf (((symbol-function 'process-file) (lambda (&rest _) (error "Probed")))
            ((symbol-function 'call-process) (lambda (&rest _) (error "Probed"))))
    (dolist (name '(debugpy debugpy-module debugpy-test))
      (should (dape--config-ensure (alist-get name dape-configs) t)))))

(ert-deftest ygg-debugpy-a-chosen-adapter-command-gets-dapes-check ()
  (should-error (dape--config-ensure (plist-put (copy-sequence (alist-get 'debugpy dape-configs))
                                                'command "/nowhere/bin/python")
                                     t)
                :type 'user-error))

;;; debugpy-tests.el ends here
