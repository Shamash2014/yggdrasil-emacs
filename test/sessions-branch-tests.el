;;; sessions-branch-tests.el --- Per-branch project sessions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'layer-sessions)

(defmacro sessions-branch-tests--in-repo (var &rest body)
  "Run BODY in a fresh repository on branch trunk, VAR bound to its root."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "ygg-sess" t))))
          (default-directory ,var)
          (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment)))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "init" "-q" "-b" "trunk")
           (call-process "git" nil nil nil "-c" "user.email=t@t" "-c" "user.name=t"
                         "commit" "-q" "--allow-empty" "-m" "0")
           ,@body)
       (delete-directory ,var t))))

(ert-deftest ygg-session-name-carries-the-branch ()
  (sessions-branch-tests--in-repo root
    (let ((base (ygg-session--base-name root)))
      (should (equal (ygg-session--project-name root) (concat base "%%trunk")))
      (should (equal (ygg-session--project-name root "feat/x") (concat base "%%feat%x")))
      (call-process "git" nil nil nil "checkout" "-q" "--detach")
      (should (equal (ygg-session--project-name root) base)))))

(ert-deftest ygg-session-labels-read-path-and-branch ()
  (should (equal (ygg-session--labels '("~%code%app%%trunk" "~%code%app%%feat%x"
                                        "~%code%app" "~%a%lib" "~%b%lib"))
                 '(("app @ trunk" . "~%code%app%%trunk")
                   ("app @ feat/x" . "~%code%app%%feat%x")
                   ("app" . "~%code%app")
                   ("a/lib" . "~%a%lib")
                   ("b/lib" . "~%b%lib")))))

(ert-deftest ygg-session-follows-a-checkout-only-for-its-own-project ()
  (sessions-branch-tests--in-repo root
    (let* ((base (ygg-session--base-name root))
           (dir (make-temp-file "ygg-sess-files" t))
           (current (concat base "%%trunk"))
           saved switched)
      (cl-letf (((symbol-function 'easysession-get-session-name) (lambda () current))
                ((symbol-function 'easysession-get-session-file-path)
                 (lambda (n) (expand-file-name n dir)))
                ((symbol-function 'easysession-save) (lambda (n) (push n saved)))
                ((symbol-function 'easysession-set-current-session-name)
                 (lambda (n) (setq current n)))
                ((symbol-function 'easysession-switch-to)
                 (lambda (n) (push n switched) (setq current n)))
                ((symbol-function 'project-current) (lambda (&rest _) nil)))
        (unwind-protect
            (progn
              (ygg-session-follow-branch)
              (should-not saved)
              (call-process "git" nil nil nil "checkout" "-q" "-b" "feat")
              (ygg-session-follow-branch)
              (should (equal saved (list (concat base "%%feat") (concat base "%%trunk"))))
              (should (equal current (concat base "%%feat")))
              (with-temp-file (expand-file-name (concat base "%%trunk") dir))
              (call-process "git" nil nil nil "checkout" "-q" "trunk")
              (ygg-session-follow-branch)
              (should (equal switched (list (concat base "%%trunk"))))
              (setq saved nil switched nil)
              (call-process "git" nil nil nil "checkout" "-q" "--detach")
              (ygg-session-follow-branch)
              (should-not (or saved switched))
              (setq current "~%elsewhere%%trunk")
              (call-process "git" nil nil nil "checkout" "-q" "feat")
              (ygg-session-follow-branch)
              (should-not (or saved switched)))
          (delete-directory dir t))))))
