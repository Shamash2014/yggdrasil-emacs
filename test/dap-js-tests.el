;;; dap-js-tests.el --- Tests for Node debug configs -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dape)
(require 'ygg-dap-js)

(defmacro ygg-dap-js-test--with-configs (&rest body)
  "Run BODY on a private copy of dape's entries, installed."
  (declare (indent 0))
  `(let ((dape-configs (copy-tree dape-configs))
         (ygg-dap-js--dape-ensure ygg-dap-js--dape-ensure))
     (ygg-dap-js-install)
     ,@body))

(defmacro ygg-dap-js-test--in (directory &rest body)
  "Run BODY as if visiting a file under DIRECTORY, the project root."
  (declare (indent 1))
  `(let ((default-directory ,directory)
         (dape-cwd-function (lambda () ,directory)))
     ,@body))

(ert-deftest ygg-dap-js-server-is-the-newest-version-not-an-alias ()
  (let* ((installs (file-name-as-directory (make-temp-file "ygg-dap-js-" t)))
         (ygg-dap-js-installs installs))
    (unwind-protect
        (progn
          (dolist (version '("1.9.0" "1.140.0"))
            (let ((server (expand-file-name (concat version "/src/dapDebugServer.js") installs)))
              (make-directory (file-name-directory server) t)
              (write-region "" nil server)))
          (make-symbolic-link (expand-file-name "1.9.0" installs) (expand-file-name "latest" installs))
          (should (equal (ygg-dap-js-server)
                         (expand-file-name "1.140.0/src/dapDebugServer.js" installs))))
      (delete-directory installs t))))

(ert-deftest ygg-dap-js-server-is-nil-when-nothing-is-installed ()
  (let ((ygg-dap-js-installs (make-temp-file "ygg-dap-js-empty-" t)))
    (unwind-protect (should-not (ygg-dap-js-server))
      (delete-directory ygg-dap-js-installs t))))

(ert-deftest ygg-dap-js-every-entry-runs-the-installed-server ()
  (let ((ygg-dap-js-installs (make-temp-file "ygg-dap-js-" t)))
    (unwind-protect
        (let ((server (expand-file-name "1.140.0/src/dapDebugServer.js" ygg-dap-js-installs)))
          (make-directory (file-name-directory server) t)
          (write-region "" nil server)
          (ygg-dap-js-test--with-configs
            (dolist (name '(js-debug-node js-debug-ts-node js-debug-tsx js-debug-node-attach
                            js-debug-chrome js-debug-node-attach-remote js-debug-node-attach-docker))
              (let ((plist (alist-get name dape-configs)))
                (should (equal (plist-get plist 'command-args) (list server :autoport)))
                (should (eq (plist-get plist 'ensure) #'ygg-dap-js-ensure))))))
      (delete-directory ygg-dap-js-installs t))))

(ert-deftest ygg-dap-js-install-twice-keeps-dape-own-check ()
  (ygg-dap-js-test--with-configs
    (let ((dape-check ygg-dap-js--dape-ensure))
      (should (functionp dape-check))
      (should-not (eq dape-check #'ygg-dap-js-ensure))
      (ygg-dap-js-install)
      (should (eq ygg-dap-js--dape-ensure dape-check)))))

(ert-deftest ygg-dap-js-launch-from-a-remote-buffer-is-refused ()
  (let ((default-directory "/ssh:deploy@web1:/srv/app/"))
    (should-error (ygg-dap-js-ensure '(command "node")) :type 'user-error)))

(ert-deftest ygg-dap-js-attach-variants-derive-from-dape-attach ()
  (ygg-dap-js-test--with-configs
    (dolist (name '(js-debug-node-attach-remote js-debug-node-attach-docker))
      (let ((plist (alist-get name dape-configs)))
        (should (equal (plist-get plist :request) "attach"))
        (should (equal (plist-get plist :type) "pwa-node"))
        (should (eql (plist-get plist :port) 9229))
        (should (eq (plist-get plist 'command-cwd) 'ygg-dap-js-local-cwd))))))

(ert-deftest ygg-dap-js-adapter-runs-locally-from-a-remote-buffer ()
  (let ((default-directory "/ssh:deploy@web1:/srv/app/"))
    (should-not (file-remote-p (ygg-dap-js-local-cwd)))))

(ert-deftest ygg-dap-js-ssh-buffer-attaches-to-its-host-with-its-paths ()
  (ygg-dap-js-test--in "/ssh:deploy@web1:/srv/app/"
    (let ((config (ygg-dap-js-remote-resolve (list :port 9229 :request "attach"))))
      (should (equal (plist-get config :address) "web1"))
      (should (equal (plist-get config 'prefix-local) "/ssh:deploy@web1:"))
      (should (equal (plist-get config 'prefix-remote) ""))
      (should (equal (plist-get config :localRoot) "/srv/app/"))
      (should (equal (plist-get config :remoteRoot) "/srv/app/")))))

(ert-deftest ygg-dap-js-given-keys-are-kept ()
  (ygg-dap-js-test--in "/ssh:web1:/srv/app/"
    (let ((config (ygg-dap-js-remote-resolve (list :address "10.0.0.5" :remoteRoot "/opt/x"))))
      (should (equal (plist-get config :address) "10.0.0.5"))
      (should (equal (plist-get config :remoteRoot) "/opt/x")))))

(ert-deftest ygg-dap-js-local-checkout-maps-to-the-prompted-remote-path ()
  (ygg-dap-js-test--in "/Users/me/app/"
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _) (if (string-prefix-p "Host" prompt) "web1" "/srv/app"))))
      (let ((config (ygg-dap-js-remote-resolve (list :port 9229))))
        (should (equal (plist-get config :address) "web1"))
        (should (equal (plist-get config :localRoot) "/Users/me/app/"))
        (should (equal (plist-get config :remoteRoot) "/srv/app"))
        (should-not (plist-member config 'prefix-local))))))

(ert-deftest ygg-dap-js-docker-buffer-uses-its-container-and-published-port ()
  (ygg-dap-js-test--in "/docker:api:/app/"
    (cl-letf (((symbol-function 'ygg-dap-js--docker)
               (lambda (&rest args)
                 (should (equal args '("port" "api" "9229/tcp")))
                 "0.0.0.0:49153\n[::]:49153")))
      (let ((config (ygg-dap-js-docker-resolve (list :port 9229))))
        (should (equal (plist-get config 'container) "api"))
        (should (equal (plist-get config :address) "127.0.0.1"))
        (should (eql (plist-get config :port) 49153))
        (should (equal (plist-get config 'prefix-local) "/docker:api:"))
        (should (equal (plist-get config :localRoot) "/app/"))
        (should (equal (plist-get config :remoteRoot) "/app/"))))))

(ert-deftest ygg-dap-js-docker-without-a-published-port-uses-the-container-ip ()
  (ygg-dap-js-test--in "/docker:api:/app/"
    (cl-letf (((symbol-function 'ygg-dap-js--docker)
               (lambda (&rest args) (and (equal (car args) "inspect") "172.17.0.3 "))))
      (let ((config (ygg-dap-js-docker-resolve (list :port 9229))))
        (should (equal (plist-get config :address) "172.17.0.3"))
        (should (eql (plist-get config :port) 9229))))))

(ert-deftest ygg-dap-js-bind-mounted-checkout-maps-to-its-container-path ()
  (let ((root (file-name-as-directory (make-temp-file "ygg-dap-js-proj-" t))))
    (unwind-protect
        (ygg-dap-js-test--in root
          (cl-letf (((symbol-function 'ygg-dap-js--docker-mounts)
                     (lambda (container)
                       (should (equal container "api"))
                       (list (cons (file-truename root) "/app")))))
            (let ((config (ygg-dap-js-docker-resolve
                           (list 'container "api" :address "127.0.0.1" :port 9229))))
              (should (equal (plist-get config :localRoot) root))
              (should (equal (plist-get config :remoteRoot) "/app"))
              (should-not (plist-member config 'prefix-local)))))
      (delete-directory root t))))

(ert-deftest ygg-dap-js-docker-endpoint-reads-the-first-mapping ()
  (should (equal (ygg-dap-js-docker-endpoint "0.0.0.0:49153\n[::]:49153") '("127.0.0.1" . 49153)))
  (should (equal (ygg-dap-js-docker-endpoint "[::]:9229") '("127.0.0.1" . 9229)))
  (should (equal (ygg-dap-js-docker-endpoint "192.168.5.2:9230") '("192.168.5.2" . 9230)))
  (should-not (ygg-dap-js-docker-endpoint ""))
  (should-not (ygg-dap-js-docker-endpoint nil)))

(ert-deftest ygg-dap-js-mounted-path-prefers-the-deepest-mount ()
  (let ((mounts '(("/Users/me" . "/home") ("/Users/me/app" . "/app"))))
    (should (equal (ygg-dap-js-mounted-path "/Users/me/app/" mounts) "/app"))
    (should (equal (ygg-dap-js-mounted-path "/Users/me/app/web" mounts) "/app/web"))
    (should (equal (ygg-dap-js-mounted-path "/Users/me/lib" mounts) "/home/lib"))
    (should-not (ygg-dap-js-mounted-path "/Users/me-too/app" mounts))
    (should-not (ygg-dap-js-mounted-path "/opt/app" mounts))))

(provide 'dap-js-tests)
;;; dap-js-tests.el ends here
