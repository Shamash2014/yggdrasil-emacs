;;; ice-runner-stacks-tests.el --- Flutter/Dart, Kotlin/Gradle, Swift detection in ice-runner -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'cl-lib)

(defconst ice-stacks-tests--root-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst ice-stacks-tests--runner
  (expand-file-name "../etc/ice/ice-runner" ice-stacks-tests--root-dir))

(defun ice-stacks-tests--write (path text)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text)))

(defun ice-stacks-tests--git (root &rest args)
  (with-temp-buffer
    (let ((default-directory root))
      (cons (apply #'call-process "git" nil t nil args) (buffer-string)))))

(defun ice-stacks-tests--run (root &rest args)
  "Run ice-runner ARGS in ROOT, returning (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory root))
      (cons (apply #'call-process "python3" nil t nil ice-stacks-tests--runner args) (buffer-string)))))

(defun ice-stacks-tests--detect (root)
  (let ((out (ice-stacks-tests--run root "detect" root)))
    (should (= 0 (car out)))
    (with-temp-buffer
      (insert (cdr out))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :null-object nil :array-type 'list))))

(defmacro ice-stacks-tests--with-repo (var &rest body)
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (file-truename (make-temp-file "ice-stacks-repo" t)))))
     (unwind-protect
         (progn
           (should (= 0 (car (ice-stacks-tests--git ,var "init" "-q" "-b" "main"))))
           ,@body)
       (delete-directory ,var t))))

;; --- detect(): Kotlin/Gradle ---

(ert-deftest ice-runner-detects-gradle-with-wrapper-preferred ()
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--write (expand-file-name "build.gradle.kts" root) "plugins { kotlin(\"jvm\") }\n")
    (ice-stacks-tests--write (expand-file-name "gradlew" root) "#!/bin/sh\n")
    (let ((found (ice-stacks-tests--detect root)))
      (should (equal (alist-get 'runner found) "gradle"))
      (should (equal (alist-get 'evidence found) "build.gradle.kts"))
      (should (string-prefix-p "./gradlew test --tests=" (alist-get 'test_cmd found)))
      (should (string-match-p "{filter}" (alist-get 'test_cmd found)))
      (should-not (alist-get 'report_path found))
      (should (cl-some (lambda (a) (string-match-p "writes no JUnit report" a)) (alist-get 'asks found))))))

(ert-deftest ice-runner-flags-android-gradle-test-as-an-aggregate-task ()
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--write (expand-file-name "build.gradle.kts" root)
                             "plugins { id(\"com.android.application\") }\n")
    (let ((found (ice-stacks-tests--detect root)))
      (should (equal (alist-get 'runner found) "gradle"))
      (should (cl-some (lambda (a) (string-match-p "Android plugin" a)) (alist-get 'asks found))))))

;; --- detect(): Swift ---

(ert-deftest ice-runner-detects-swiftpm-with-xunit-report ()
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--write (expand-file-name "Package.swift" root) "// swift-tools-version:5.9\n")
    (let ((found (ice-stacks-tests--detect root)))
      (should (equal (alist-get 'runner found) "swift"))
      (should (equal (alist-get 'evidence found) "Package.swift"))
      (should (equal (alist-get 'test_cmd found) "swift test --filter={filter} --xunit-output={report}"))
      (should (equal (alist-get 'report_path found) ".ice/state/junit.xml")))))

(ert-deftest ice-runner-detects-xcodeproj-without-package-swift ()
  (ice-stacks-tests--with-repo root
    (make-directory (expand-file-name "App.xcodeproj" root) t)
    (ice-stacks-tests--write (expand-file-name "App.xcodeproj/x" root) "")
    (let ((found (ice-stacks-tests--detect root)))
      (should (equal (alist-get 'runner found) "xcodebuild"))
      (should (equal (alist-get 'evidence found) "App.xcodeproj"))
      (should (string-match-p "-scheme App " (alist-get 'test_cmd found)))
      (should (string-match-p "-only-testing:{filter}" (alist-get 'test_cmd found)))
      (should (cl-some (lambda (a) (string-match-p "-destination" a)) (alist-get 'asks found)))
      (should (cl-some (lambda (a) (string-match-p "writes no JUnit report" a)) (alist-get 'asks found))))))

(ert-deftest ice-runner-prefers-package-swift-over-an-xcodeproj ()
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--write (expand-file-name "Package.swift" root) "// swift-tools-version:5.9\n")
    (make-directory (expand-file-name "App.xcodeproj" root) t)
    (should (equal (alist-get 'runner (ice-stacks-tests--detect root)) "swift"))))

;; --- detect(): flutter is still first among the mobile runners ---

(ert-deftest ice-runner-still-detects-flutter-before-gradle-or-swift ()
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--write (expand-file-name "pubspec.yaml" root) "name: app\ndependencies:\n  flutter:\n    sdk: flutter\n")
    (ice-stacks-tests--write (expand-file-name "build.gradle.kts" root) "plugins { kotlin(\"jvm\") }\n")
    (should (equal (alist-get 'runner (ice-stacks-tests--detect root)) "flutter"))))

;; --- mutate_plan(): a stub tool on PATH sets mutate_cmd, its absence asks ---

(defmacro ice-stacks-tests--with-bin (var &rest body)
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "ice-stacks-bin" t))))
     (unwind-protect (progn ,@body) (delete-directory ,var t))))

(defun ice-stacks-tests--stub (bin name)
  (let ((path (expand-file-name name bin)))
    (ice-stacks-tests--write path "#!/bin/sh\nexit 0\n")
    (set-file-modes path #o755)))

(ert-deftest ice-runner-sets-mutate-cmd-for-dart-when-mutation-test-declared-and-dart-on-path ()
  (skip-unless (executable-find "git"))
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--with-bin bin
      (ice-stacks-tests--write (expand-file-name "pubspec.yaml" root)
                               "name: app\ndependencies:\n  flutter:\n    sdk: flutter\ndev_dependencies:\n  mutation_test: ^1.0.0\n")
      (ice-stacks-tests--stub bin "dart")
      (let* ((process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
             (out (ice-stacks-tests--run root "config" root)))
        (should (= 0 (car out)))
        (should (string-match-p "mutate_cmd set to mutation_test" (cdr out)))))))

(ert-deftest ice-runner-leaves-dart-mutate-cmd-unset-without-the-dev-dependency ()
  (skip-unless (executable-find "git"))
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--with-bin bin
      (ice-stacks-tests--write (expand-file-name "pubspec.yaml" root)
                               "name: app\ndependencies:\n  flutter:\n    sdk: flutter\n")
      (ice-stacks-tests--stub bin "dart")
      (let* ((process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
             (out (ice-stacks-tests--run root "config" root)))
        (should (= 0 (car out)))
        (should (string-match-p "mutation testing for flutter has no installed tool" (cdr out)))))))

(ert-deftest ice-runner-sets-mutate-cmd-for-gradle-when-pitest-plugin-declared ()
  (skip-unless (executable-find "git"))
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--write (expand-file-name "build.gradle.kts" root)
                             "plugins {\n  id(\"info.solidsoft.pitest\") version \"1.15.0\"\n}\n")
    (let ((out (ice-stacks-tests--run root "config" root)))
      (should (= 0 (car out)))
      (should (string-match-p "mutate_cmd set to pitest" (cdr out))))))

(ert-deftest ice-runner-leaves-gradle-mutate-cmd-unset-without-the-pitest-plugin ()
  (skip-unless (executable-find "git"))
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--write (expand-file-name "build.gradle.kts" root) "plugins { kotlin(\"jvm\") }\n")
    (let ((out (ice-stacks-tests--run root "config" root)))
      (should (= 0 (car out)))
      (should (string-match-p "mutation testing for gradle has no installed tool" (cdr out))))))

(ert-deftest ice-runner-sets-mutate-cmd-for-swift-when-muter-is-on-path ()
  (skip-unless (executable-find "git"))
  (ice-stacks-tests--with-repo root
    (ice-stacks-tests--with-bin bin
      (ice-stacks-tests--write (expand-file-name "Package.swift" root) "// swift-tools-version:5.9\n")
      (ice-stacks-tests--stub bin "muter")
      (let* ((process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
             (out (ice-stacks-tests--run root "config" root)))
        (should (= 0 (car out)))
        (should (string-match-p "mutate_cmd set to muter" (cdr out)))))))

(ert-deftest ice-runner-leaves-swift-mutate-cmd-unset-without-muter ()
  (skip-unless (executable-find "git"))
  (ice-stacks-tests--with-repo root
    (let* ((bare-path (mapconcat #'file-name-directory
                                 (delq nil (list (executable-find "git") (executable-find "python3")))
                                 ":")))
      (ice-stacks-tests--write (expand-file-name "Package.swift" root) "// swift-tools-version:5.9\n")
      (let ((process-environment (cons (concat "PATH=" bare-path ":/bin:/usr/bin") process-environment)))
        (skip-unless (not (executable-find "muter")))
        (let ((out (ice-stacks-tests--run root "config" root)))
          (should (= 0 (car out)))
          (should (string-match-p "mutation testing for swift has no installed tool" (cdr out))))))))

;; --- TEMPLATE is unchanged ---

(ert-deftest ice-runner-template-constant-is-unchanged-from-head ()
  (skip-unless (executable-find "git"))
  (let* ((repo-root (expand-file-name ".." ice-stacks-tests--root-dir))
         (head (with-temp-buffer
                 (call-process "git" nil t nil "-C" repo-root "show" "HEAD:etc/ice/ice-runner")
                 (buffer-string)))
         (current (with-temp-buffer
                    (insert-file-contents ice-stacks-tests--runner)
                    (buffer-string)))
         (extract (lambda (text)
                    (when (string-match "TEMPLATE = \"\"\"\\(\\(.\\|\n\\)*?\\)\"\"\"" text)
                      (match-string 1 text)))))
    (should (funcall extract head))
    (should (equal (funcall extract head) (funcall extract current)))))

(provide 'ice-runner-stacks-tests)
;;; ice-runner-stacks-tests.el ends here
