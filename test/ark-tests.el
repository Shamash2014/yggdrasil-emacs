;;; ark-tests.el --- Tests for ark's comm messages -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-ark)

(unless (fboundp 'r-ts-mode)
  (define-derived-mode r-ts-mode prog-mode "R"))

(ert-deftest ygg-ark-comm-open-asks-the-kernel-to-bind-on-loopback ()
  "A server comm_open carries only the address ark binds its port on."
  (should (equal (ygg-ark--comm-open-data) '(:ip_address "127.0.0.1"))))

(ert-deftest ygg-ark-comm-targets-name-the-kernel-server-handlers ()
  "The LSP and DAP targets are the keys ark registers its servers under."
  (should (equal ygg-ark-lsp-target "lsp"))
  (should (equal ygg-ark-dap-target "ark_dap")))

(ert-deftest ygg-ark-reads-the-port-from-a-server-started-rpc ()
  "The method and params shape, sent for lsp and ark_dap."
  (should (= (ygg-ark--server-port
              '(:method "server_started" :params (:port 53117)))
             53117)))

(ert-deftest ygg-ark-reads-the-port-from-the-legacy-lsp-shape ()
  "The msg_type and content shape, sent for positron.lsp."
  (should (= (ygg-ark--server-port
              '(:msg_type "server_started" :content (:port 40001)))
             40001)))

(ert-deftest ygg-ark-finds-no-port-in-other-comm-messages ()
  "Debug events and malformed ports are not a server start."
  (should-not (ygg-ark--server-port '(:method "start_debug" :params nil)))
  (should-not (ygg-ark--server-port '(:method "server_started" :params (:port 0))))
  (should-not (ygg-ark--server-port '(:method "server_started" :params (:port "1"))))
  (should-not (ygg-ark--server-port nil)))

(ert-deftest ygg-ark-reads-the-console-command-a-dap-step-asks-for ()
  "A step on the DAP comes back as an execute rpc with an R browser command."
  (should (equal (ygg-ark--execute-command
                  '(:method "execute" :params (:command "n")))
                 "n"))
  (should-not (ygg-ark--execute-command '(:method "stop_debug" :params nil))))

(ert-deftest ygg-ark-dape-entry-attaches-over-tcp-once-dape-loads ()
  "Loading dape adds an ark entry that attaches, its port filled at start."
  (defvar dape-configs)
  (let ((dape-configs nil)
        (loaded (featurep 'dape)))
    (unwind-protect
        (progn
          (provide 'dape)
          (let ((config (alist-get 'ark dape-configs)))
            (should (equal (plist-get config 'host) "127.0.0.1"))
            (should (equal (plist-get config :request) "attach"))
            (should (eq (plist-get config 'fn) 'ygg-ark--dape-config))
            (should-not (plist-get config 'command))))
      (unless loaded (setq features (delq 'dape features))))))

(defvar jupyter-kernel-language-mode-properties nil)

(ert-deftest ygg-ark-maps-the-kernel-language-r-to-r-ts-mode ()
  "Jupyter looks up the language name ark reports and finds r-ts-mode."
  (let ((jupyter-kernel-language-mode-properties '((R fundamental-mode nil))))
    (ygg-ark--pin-r-mode)
    (dolist (name '(R "R"))
      (should (eq (cadr (assoc name jupyter-kernel-language-mode-properties)) 'r-ts-mode))
      (should (syntax-table-p (nth 2 (assoc name jupyter-kernel-language-mode-properties)))))))

(ert-deftest ygg-ark-eglot-serves-r-ts-mode-from-the-kernel ()
  "Eglot finds ark's contact function first for r-ts-mode."
  (require 'eglot)
  (let ((entry (seq-find (lambda (e) (memq 'r-ts-mode (ensure-list (car e))))
                         eglot-server-programs)))
    (should (eq (cdr entry) #'ygg-ark-eglot-contact))))

(ert-deftest ygg-ark-forwards-through-the-host-plain-ssh-reaches ()
  "The destination is user@host, as emacs-jupyter names it for kernel channels."
  (require 'tramp)
  (should (equal (ygg-ark-ssh-destination "/ssh:root@jtest:") "root@jtest"))
  (should (equal (ygg-ark-ssh-destination "/ssh:jtest:") "jtest"))
  (should-error (ygg-ark-ssh-destination "/docker:box:") :type 'user-error))

(ert-deftest ygg-ark-a-local-kernel-needs-no-forward ()
  (cl-letf (((symbol-function 'ygg-kernel-picker-remote) #'ignore))
    (should (= (ygg-ark-reach 'client 40001) 40001))))

(ert-deftest ygg-ark-a-remote-port-is-forwarded-from-loopback-to-the-hosts-loopback ()
  "The tunnel is ssh -f -L 127.0.0.1:LOCAL:127.0.0.1:PORT to the host, and reach
answers the local end once ssh has backgrounded."
  (unless (require 'jupyter-base nil t) (ert-skip "emacs-jupyter is not on the load path"))
  (require 'tramp)
  (let (argv)
    (cl-letf (((symbol-function 'ygg-kernel-picker-remote) (lambda (_) "/ssh:root@jtest:"))
              ((symbol-function 'jupyter-available-local-ports) (lambda (_) '(50123)))
              ((symbol-function 'start-process)
               (lambda (name buffer &rest command)
                 (setq argv command)
                 (make-process :name name :buffer buffer :command '("true")))))
      (should (= (ygg-ark-reach 'client 40001) 50123)))
    (should (equal argv '("ssh" "-f" "-o ExitOnForwardFailure=yes"
                          "-L" "127.0.0.1:50123:127.0.0.1:40001" "root@jtest" "sleep 60")))))

(ert-deftest ygg-ark-a-forward-ssh-refuses-is-an-error ()
  (unless (require 'jupyter-base nil t) (ert-skip "emacs-jupyter is not on the load path"))
  (cl-letf (((symbol-function 'start-process)
             (lambda (name buffer &rest _)
               (make-process :name name :buffer buffer :command '("false")))))
    (should-error (ygg-ark--tunnel 50123 40001 "jtest"))))

(ert-deftest ygg-ark-each-host-gets-its-own-headless-ark ()
  "A local R buffer never reuses the headless ark a remote buffer started, nor the reverse."
  (let ((ygg-ark--background (make-hash-table :test #'equal))
        (made 0))
    (cl-letf (((symbol-function 'ygg-ark--require) #'ignore)
              ((symbol-function 'jupyter-kernel-alive-p) (lambda (_) t))
              ((symbol-function 'jupyter-client)
               (lambda (_spec) (list 'client (cl-incf made)))))
      (let* ((far (let ((default-directory "/ssh:jtest:/root/")) (ygg-ark--background-client)))
             (near (let ((default-directory "/tmp/")) (ygg-ark--background-client)))
             (far-again (let ((default-directory "/ssh:jtest:/srv/")) (ygg-ark--background-client))))
        (should-not (equal far near))
        (should (eq far far-again))
        (should (equal (ygg-kernel-picker-remote far) "/ssh:jtest:"))
        (should-not (ygg-kernel-picker-remote near))))))

(ert-deftest ygg-ark-eglot-contact-names-its-class-first ()
  (cl-letf (((symbol-function 'ygg-ark--require) #'ignore)
            ((symbol-function 'ygg-ark--buffer-client) (lambda () 'client)))
    (let ((contact (ygg-ark-eglot-contact)))
      (should (eq (car contact) 'eglot-lsp-server))
      (should (eq (cadr contact) :process))
      (should (functionp (caddr contact))))))

(defmacro ark-tests--with-eglot (&rest body)
  "Run BODY with eglot and jupyter stubbed around a live stand-in LSP connection.
The connection is a pipe process tagged with its kernel client, as
ygg-ark--lsp-stream tags it; shut down and reaped are recorded."
  (declare (indent 0))
  `(let ((ygg-ark--background (make-hash-table :test #'equal))
         (ygg-ark-auto-lsp t)
         connection shut-down reaped)
     (cl-letf* ((connect (lambda (client)
                           (setq connection (make-pipe-process :name "ark-lsp-test" :noquery t))
                           (process-put connection 'ygg-ark-client client)))
                ((symbol-function 'ygg-ark--require) #'ignore)
                ((symbol-function 'jupyter-kernel-alive-p) (lambda (_) t))
                ((symbol-function 'jupyter-client) (lambda (_spec) (list 'headless)))
                ((symbol-function 'jupyter-kernel-info)
                 (lambda (_) (list :implementation "ark")))
                ((symbol-function 'jupyter-shutdown-kernel) (lambda (c) (push c reaped)))
                ((symbol-function 'eglot-current-server)
                 (lambda () (and (process-live-p connection) 'server)))
                ((symbol-function 'jsonrpc-running-p) (lambda (_) t))
                ((symbol-function 'jsonrpc--process) (lambda (_) connection))
                ((symbol-function 'eglot-shutdown)
                 (lambda (_) (push (process-get connection 'ygg-ark-client) shut-down)
                   (delete-process connection)))
                ((symbol-function 'ygg-ark-lsp)
                 (lambda ()
                   (funcall connect (or (ygg-ark--buffer-client) (ygg-ark--background-client))))))
       (unwind-protect
           (with-temp-buffer
             (r-ts-mode)
             ,@body)
         (when (processp connection) (delete-process connection))))))

(ert-deftest ygg-ark-lsp-moves-to-a-kernel-attached-after-headless ()
  (ark-tests--with-eglot
    (ygg-ark-lsp)
    (let ((headless (ygg-ark--eglot-client))
          (kernel (list 'kernel)))
      (should (equal headless '(headless)))
      (setq-local jupyter-current-client kernel)
      (ygg-ark--auto-lsp)
      (should (equal shut-down (list headless)))
      (should (eq (ygg-ark--eglot-client) kernel))
      (ygg-ark--reap-background)
      (should (equal reaped (list headless)))
      (should (zerop (hash-table-count ygg-ark--background)))
      (ygg-ark--auto-lsp)
      (should (equal shut-down (list headless))))))

(ert-deftest ygg-ark-plain-r-buffer-without-a-kernel-keeps-headless ()
  (ark-tests--with-eglot
    (ygg-ark-lsp)
    (ygg-ark--auto-lsp)
    (ygg-ark--reap-background)
    (should (equal (ygg-ark--eglot-client) '(headless)))
    (should-not shut-down)
    (should-not reaped)))

(provide 'ark-tests)
;;; ark-tests.el ends here
