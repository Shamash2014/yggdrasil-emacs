;;; ygg-jdk-tests.el --- Tests for the pinned JDK and per-project overrides -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ygg-jdk)
(defvar ygg-leader-open-map (make-sparse-keymap))
(require 'layer-tasks)
(require 'layer-rass)

(defvar mise--cache)
(defvar eglot-server-programs)

(defun ygg-jdk-tests--fake-jdk (root name major)
  "A JDK home named NAME under ROOT with a runnable java and a MAJOR release."
  (let ((home (expand-file-name name root)))
    (make-directory (expand-file-name "bin" home) t)
    (with-temp-file (expand-file-name "bin/java" home) (insert "#!/bin/sh\n"))
    (set-file-modes (expand-file-name "bin/java" home) #o755)
    (with-temp-file (expand-file-name "release" home)
      (insert (format "JAVA_VERSION=\"%d.0.1\"\n" major)))
    home))

(defmacro ygg-jdk-tests--with-jdks (bindings &rest body)
  "Run BODY with each (VAR NAME MAJOR) in BINDINGS bound to a fake JDK home."
  (declare (indent 1))
  (let ((root (make-symbol "root")))
    `(let* ((,root (make-temp-file "ygg-jdk" t))
            ,@(mapcar (pcase-lambda (`(,var ,name ,major))
                        `(,var (ygg-jdk-tests--fake-jdk ,root ,name ,major)))
                      bindings))
       (unwind-protect (progn ,@body)
         (delete-directory ,root t)))))

(ert-deftest ygg-jdk-pinned-version-reads-the-exact-java-pin ()
  (let ((config (make-temp-file "mise" nil ".toml"
                                "[tools]\nbun = \"latest\"\njava = \"temurin-21.0.12+101.0.LTS\"\nnode = \"26\"\n")))
    (unwind-protect
        (should (equal (ygg-jdk--pinned-version config) "temurin-21.0.12+101.0.LTS"))
      (delete-file config))
    (should-not (ygg-jdk--pinned-version "/nonexistent/config.toml"))))

(ert-deftest ygg-jdk-home-prefers-the-buffer-java-home ()
  (ygg-jdk-tests--with-jdks ((pin "pin-21" 21) (project "project-17" 17))
    (let ((ygg-jdk-default-home pin))
      (with-temp-buffer
        (setq-local process-environment (cons (concat "JAVA_HOME=" project "/")
                                              process-environment))
        (should (equal (ygg-jdk-home) project))
        (should (eql (ygg-jdk-major (ygg-jdk-home)) 17))))))

(ert-deftest ygg-jdk-home-falls-back-to-the-pin ()
  (ygg-jdk-tests--with-jdks ((pin "pin-21" 21))
    (let ((ygg-jdk-default-home pin))
      (with-temp-buffer
        (setq-local process-environment (cons "JAVA_HOME=/no/such/jdk" process-environment))
        (should (equal (ygg-jdk-home) pin)))
      (with-temp-buffer
        (setq-local process-environment (cons "JAVA_HOME" process-environment))
        (should (equal (ygg-jdk-home) pin))))))

(ert-deftest ygg-jdk-export-default-leaves-a-set-java-home-alone ()
  (ygg-jdk-tests--with-jdks ((pin "pin-21" 21))
    (let ((ygg-jdk-default-home pin))
      (cl-letf (((default-value 'process-environment) '("JAVA_HOME=/theirs" "PATH=/usr/bin"))
                ((default-value 'exec-path) '("/usr/bin")))
        (ygg-jdk-export-default)
        (should (equal (getenv-internal "JAVA_HOME" (default-value 'process-environment)) "/theirs"))
        (should (equal (default-value 'exec-path) '("/usr/bin"))))
      (cl-letf (((default-value 'process-environment) '("PATH=/usr/bin"))
                ((default-value 'exec-path) '("/usr/bin")))
        (ygg-jdk-export-default)
        (should (equal (getenv-internal "JAVA_HOME" (default-value 'process-environment)) pin))
        (should (equal (getenv-internal "PATH" (default-value 'process-environment))
                       (concat pin "/bin:/usr/bin")))
        (should (equal (car (default-value 'exec-path)) (concat pin "/bin")))))))

(ert-deftest ygg-jdk-jupyter-kernels-run-on-the-buffer-jdk ()
  (ygg-jdk-tests--with-jdks ((pin "pin-21" 21) (project "project-17" 17))
    (let ((ygg-jdk-default-home pin))
      (with-temp-buffer
        (setq-local process-environment (cons (concat "JAVA_HOME=" project) process-environment))
        (should (equal (ygg-jdk-jupyter-environment
                        (list (concat "KOTLIN_JUPYTER_JAVA_HOME=" pin) (concat "JAVA_HOME=" pin) "LANG=C"))
                       (list (concat "KOTLIN_JUPYTER_JAVA_HOME=" project)
                             (concat "JAVA_HOME=" project) "LANG=C")))
        (should (equal (ygg-jdk-jupyter-argv (list (concat pin "/bin/java") "-jar" "k.jar"))
                       (list (concat project "/bin/java") "-jar" "k.jar")))
        (should (equal (ygg-jdk-jupyter-argv '("/venv/bin/python" "-m" "k"))
                       '("/venv/bin/python" "-m" "k")))))))

;; mise.el and envrc each set the whole environment; the buffer keeps both
(ert-deftest ygg-jdk-direnv-java-home-layers-over-mise ()
  (let ((mise--cache (make-hash-table :test 'equal)))
    (puthash "key" '(("JAVA_HOME" . "/mise/jdk") ("PATH" . "/mise/jdk/bin:/mise/maven/bin:/usr/bin"))
             mise--cache)
    (cl-letf (((symbol-function 'mise--detect-dir) (lambda () "/p"))
              ((symbol-function 'mise--cache-key) (lambda (_) "key"))
              ((default-value 'process-environment) '("PATH=/usr/bin" "LANG=C")))
      (with-temp-buffer
        (setq-local mise-mode t)
        (setq-local envrc-mode t)
        (ygg-env--compose)
        (should (equal (getenv "JAVA_HOME") "/mise/jdk"))
        (ygg-env--keep-direnv (current-buffer)
                              '(("JAVA_HOME" . "/direnv/jdk") ("PATH" . "/direnv/jdk/bin:/usr/bin")))
        (should (equal (getenv "JAVA_HOME") "/direnv/jdk"))
        (should (equal (getenv "PATH")
                       (concat "/direnv/jdk/bin:/mise/jdk/bin:/mise/maven/bin:/usr/bin:"
                               ygg-env--mise-shims)))
        (should (equal (car exec-path) "/direnv/jdk/bin"))
        (ygg-env--compose)
        (should (equal (getenv "JAVA_HOME") "/direnv/jdk"))
        (ygg-env--keep-direnv (current-buffer) 'none)
        (should (equal (getenv "JAVA_HOME") "/mise/jdk"))
        (should (equal (getenv "LANG") "C"))))))

;; kotlin-lsp's command is a function now, and must still pair with harper
(ert-deftest ygg-jdk-kotlin-contact-still-goes-through-rass ()
  (let ((contact (funcall (lambda (_interactive _project)
                            '("env" "JAVA_HOME=/jdk" "kotlin-lsp" "--stdio"))
                          nil nil))
        (default-directory "/tmp/"))
    (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name))))
      (should (equal (nth 3 (ygg-rass--wrap-guess
                             (list '(kotlin-ts-mode) nil 'eglot-lsp-server contact '("kotlin"))))
                     `("rass" "--no-stream-diagnostics" "--log-level" "warn" ,ygg-rass-harper-preset "--"
                       "env" "JAVA_HOME=/jdk" "kotlin-lsp" "--stdio"
                       "--" "harper-ls" "--stdio"))))))

(provide 'ygg-jdk-tests)
;;; ygg-jdk-tests.el ends here
