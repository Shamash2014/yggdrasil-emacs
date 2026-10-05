;;; ygg-git-compare-glab-host-tests.el --- forge hosts taken from glab's configuration -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'ygg-git-compare)

(defconst ygg-git-compare-glab-host-tests--global
  "host: gitlab.com
hosts:
    gitlab.com:
        api_protocol: https
        api_host: gitlab.com
        ssh_host:
    code.corp.example:
        api_host: api.corp.example
        ssh_host: ssh.corp.example
        git_protocol: ssh
aliases:
    co: pr checkout
")

(defmacro ygg-git-compare-glab-host-tests--with-remote (url global local &rest body)
  (declare (indent 3))
  `(let* ((root (file-name-as-directory (make-temp-file "ygg-glab-host-" t)))
          (config-dir (file-name-as-directory (make-temp-file "ygg-glab-config-" t)))
          (default-directory root)
          (process-environment
           (append (list (concat "GLAB_CONFIG_DIR=" config-dir) (concat "GH_CONFIG_DIR=" config-dir)
                         "GITLAB_HOST" "GL_HOST" "LAB_HOST"
                         "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment)))
     (unwind-protect
         (progn
           (call-process "git" nil nil nil "init" "-q" "-b" "main")
           (call-process "git" nil nil nil "remote" "add" "origin" ,url)
           (when ,global
             (with-temp-file (expand-file-name "config.yml" config-dir) (insert ,global)))
           (when ,local
             (make-directory (expand-file-name ".git/glab-cli" root) t)
             (with-temp-file (expand-file-name ".git/glab-cli/config.yml" root)
               (insert ,local)))
           (cl-letf (((symbol-function 'ygg-git-compare--ssh-hostname) #'ignore))
             ,@body))
       (delete-directory root t)
       (delete-directory config-dir t))))

(ert-deftest ygg-git-compare-glab-host-in-global-config ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@code.corp.example:g/p.git" ygg-git-compare-glab-host-tests--global nil
    (should (equal (ygg-git-compare--forge-repo "origin")
                   '(gitlab "code.corp.example" "g/p")))))

(ert-deftest ygg-git-compare-glab-ssh-host-maps-to-its-config-key ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@ssh.corp.example:g/p.git" ygg-git-compare-glab-host-tests--global nil
    (should (equal (ygg-git-compare--forge-repo "origin")
                   '(gitlab "code.corp.example" "g/p")))))

(ert-deftest ygg-git-compare-glab-host-in-repo-config ()
  (ygg-git-compare-glab-host-tests--with-remote
      "https://scm.local/g/p.git" nil "hosts:\n    scm.local:\n        api_host: scm.local\n"
    (should (equal (ygg-git-compare--forge-repo "origin")
                   '(gitlab "scm.local" "g/p")))))

(ert-deftest ygg-git-compare-glab-ssh-alias-resolves-to-a-configured-host ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@work:g/p.git" ygg-git-compare-glab-host-tests--global nil
    (cl-letf (((symbol-function 'ygg-git-compare--ssh-hostname)
               (lambda (alias) (and (equal alias "work") "code.corp.example"))))
      (should (equal (ygg-git-compare--forge-repo "origin")
                     '(gitlab "code.corp.example" "g/p"))))))

(ert-deftest ygg-git-compare-glab-config-sections-other-than-hosts-are-not-hosts ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@co:g/p.git" ygg-git-compare-glab-host-tests--global nil
    (should-error (ygg-git-compare--forge-repo "origin") :type 'user-error)))

(ert-deftest ygg-git-compare-gh-enterprise-host-is-github ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@ghe.corp.example:o/r.git" nil nil
    (with-temp-file (expand-file-name "hosts.yml" (getenv "GH_CONFIG_DIR"))
      (insert "ghe.corp.example:\n    user: someone\n"))
    (should (equal (ygg-git-compare--forge-repo "origin") '(github "ghe.corp.example" "o/r")))))

(ert-deftest ygg-git-compare-glab-config-beats-the-name ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@github.corp.example:g/p.git" nil "hosts:\n    github.corp.example:\n"
    (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "github.corp.example" "g/p")))))

(ert-deftest ygg-git-compare-glab-host-leaves-github-alone ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@github.com:o/r.git" ygg-git-compare-glab-host-tests--global nil
    (should (equal (ygg-git-compare--forge-repo "origin") '(github "github.com" "o/r")))))

(ert-deftest ygg-git-compare-glab-unknown-host-is-an-error ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@elsewhere.example:g/p.git" ygg-git-compare-glab-host-tests--global nil
    (should-error (ygg-git-compare--forge-repo "origin") :type 'user-error)))

(ert-deftest ygg-git-compare-glab-config-of-the-ssh-hostname-beats-the-alias-name ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@gitlab-work:g/p.git" "hosts:\n    gitlab.company.com:\n" nil
    (cl-letf (((symbol-function 'ygg-git-compare--ssh-hostname)
               (lambda (_) "gitlab.company.com")))
      (should (equal (ygg-git-compare--forge-repo "origin")
                     '(gitlab "gitlab.company.com" "g/p"))))))

(ert-deftest ygg-git-compare-glab-config-of-the-ssh-hostname-beats-a-github-alias ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@github-x:g/p.git" "hosts:\n    code.co:\n" nil
    (cl-letf (((symbol-function 'ygg-git-compare--ssh-hostname)
               (lambda (_) "code.co")))
      (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "code.co" "g/p"))))))

(ert-deftest ygg-git-compare-glab-quoted-yaml-is-unquoted ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@ssh.co:g/p.git" "hosts:\n    \"code.co\":\n        ssh_host: 'ssh.co'\n" nil
    (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "code.co" "g/p")))))

(ert-deftest ygg-git-compare-gh-quoted-host-is-found ()
  (ygg-git-compare-glab-host-tests--with-remote "git@ghe.co:o/r.git" nil nil
    (with-temp-file (expand-file-name "hosts.yml" (getenv "GH_CONFIG_DIR"))
      (insert "\"ghe.co\":\n    user: someone\n"))
    (should (equal (ygg-git-compare--forge-repo "origin") '(github "ghe.co" "o/r")))))

(ert-deftest ygg-git-compare-glab-key-keeps-its-port ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@code.co:g/p.git" "hosts:\n    code.co:8443:\n" nil
    (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "code.co:8443" "g/p")))))

(ert-deftest ygg-git-compare-glab-api-host-with-port-or-scheme-matches ()
  (dolist (api '("code.co:8443" "https://code.co"))
    (ygg-git-compare-glab-host-tests--with-remote
        "git@code.co:g/p.git" (format "hosts:\n    api.co:\n        api_host: %s\n" api) nil
      (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "api.co" "g/p"))))))

(ert-deftest ygg-git-compare-glab-key-with-a-trailing-comment-is-a-key ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@code.co:o/r" "hosts:\n    code.co: # x\n        api_host: x\naliases:\n" nil
    (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "code.co" "o/r")))))

(ert-deftest ygg-git-compare-glab-host-line-before-any-key-is-ignored ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@code.co:o/r" "hosts:\n        api_host: x # y\n    code.co:\n" nil
    (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "code.co" "o/r")))))

(ert-deftest ygg-git-compare-glab-host-line-before-any-key-ignores-earlier-files-key ()
  (ygg-git-compare-glab-host-tests--with-remote
      "git@b.co:o/r" "hosts:\n        api_host: b.co\n" "hosts:\n    a.co:\n"
    (should-error (ygg-git-compare--forge-repo "origin") :type 'user-error)))

(ert-deftest ygg-git-compare-glab-env-host-keeps-its-port ()
  (ygg-git-compare-glab-host-tests--with-remote "git@code.co:o/r" nil nil
    (let ((process-environment (cons "GITLAB_HOST=https://u@code.co:8443/x" process-environment)))
      (should (equal (ygg-git-compare--forge-repo "origin") '(gitlab "code.co:8443" "o/r"))))))

(ert-deftest ygg-git-compare-ssh-hostname-caches-a-failure ()
  (let ((ygg-git-compare--ssh-hostnames (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (cl-incf calls) 255)))
      (should-not (ygg-git-compare--ssh-hostname "nowhere"))
      (should-not (ygg-git-compare--ssh-hostname "nowhere"))
      (should (= calls 1)))))

(provide 'ygg-git-compare-glab-host-tests)
;;; ygg-git-compare-glab-host-tests.el ends here
