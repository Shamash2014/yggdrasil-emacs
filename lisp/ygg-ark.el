;;; ygg-ark.el --- ark's LSP and DAP from a live R kernel -*- lexical-binding: t; -*-

;; Third-party: emacs-jupyter (the kernel client), eglot, dape.  ark serves
;; its LSP and DAP from inside the running R session: a comm_open whose
;; data names an ip_address makes the kernel bind a port and answer on the
;; comm with server_started.  Eglot and dape then connect over TCP.  An R
;; buffer with no kernel gets a headless ark for the LSP alone.  For a
;; kernel on a TRAMP host the port is the host's: each connection gets its
;; own ssh forward from emacs-jupyter's tunnel helper, the one its kernel
;; channels use, which exits once that connection closes.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'eieio)
(require 'ygg-kernel-picker)

(declare-function jupyter-run-with-state "jupyter-monads")
(declare-function jupyter-sent "jupyter-monads")
(declare-function jupyter-subscribe "jupyter-monads")
(declare-function jupyter-subscriber "jupyter-monads")
(declare-function jupyter-kernel-io "jupyter-client")
(declare-function jupyter-kernel-info "jupyter-client")
(declare-function jupyter-client "jupyter-client")
(declare-function jupyter-shutdown-kernel "jupyter-client")
(declare-function jupyter-comm-open "jupyter-messages")
(declare-function jupyter-execute-request "jupyter-messages")
(declare-function jupyter-message-type "jupyter-messages")
(declare-function jupyter-message-content "jupyter-messages")
(declare-function jupyter-repl-replace-cell-code "jupyter-repl")
(declare-function jupyter-repl-execute-cell "jupyter-repl")
(declare-function jupyter-repl-cell-code "jupyter-repl")
(declare-function jupyter-kernel-alive-p "jupyter-client")
(declare-function jupyter-make-ssh-tunnel "jupyter-base")
(declare-function jupyter-available-local-ports "jupyter-base")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-method "tramp")
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function eglot "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function jsonrpc-running-p "jsonrpc")
(declare-function jsonrpc--process "jsonrpc")
(declare-function dape "dape")
(declare-function yggdrasil-localleader-def "yggdrasil-localleader")
(defvar jupyter-current-client)
(defvar jupyter-repl-interaction-mode)
(defvar jupyter-kernel-language-mode-properties)
(defvar eglot-server-programs)
(defvar dape-configs)

(defgroup ygg-ark nil
  "ark's LSP and DAP served from a live R kernel."
  :group 'tools)

(defcustom ygg-ark-kernelspec "ark-console"
  "Kernelspec started headless when an R buffer has no ark kernel."
  :type 'string)

(defcustom ygg-ark-auto-lsp t
  "Non-nil starts the LSP when an R buffer is associated with an ark kernel."
  :type 'boolean)

(defcustom ygg-ark-timeout 20
  "Seconds to wait for ark to report a server's port."
  :type 'number)

(defconst ygg-ark-host "127.0.0.1")

(defconst ygg-ark-ssh-methods '("ssh" "sshx" "scp" "scpx" "rsync")
  "TRAMP methods whose host a plain ssh reaches, as emacs-jupyter's tunnels need.")

(defconst ygg-ark-lsp-target "lsp"
  "Comm target for the LSP; ark also accepts positron.lsp.")

(defconst ygg-ark-dap-target "ark_dap")

(defconst ygg-ark-r-modes '(r-ts-mode))

(defvar ygg-ark--comms (make-hash-table :test #'equal)
  "Comm id to plist of :client :target :port.")

(defvar ygg-ark--watched (make-hash-table :test #'eq :weakness 'key))

(defvar ygg-ark--background (make-hash-table :test #'equal)
  "Host's TRAMP prefix, nil for this machine, to the headless ark client
serving the LSP to that host's buffers with no kernel.")

;;; Messages

(defun ygg-ark--comm-open-data ()
  "Data for a server comm_open: where the kernel should bind."
  (list :ip_address ygg-ark-host))

(defun ygg-ark--comm-rpc (data)
  "Return (METHOD . PARAMS) from comm DATA in either of ark's shapes.
Positron's own LSP target answers with msg_type and content, every other
server comm with method and params."
  (cond ((plist-get data :method)
         (cons (plist-get data :method) (plist-get data :params)))
        ((plist-get data :msg_type)
         (cons (plist-get data :msg_type) (plist-get data :content)))))

(defun ygg-ark--server-port (data)
  "The port in a server_started comm DATA, or nil."
  (pcase (ygg-ark--comm-rpc data)
    (`("server_started" . ,params)
     (let ((port (plist-get params :port)))
       (and (natnump port) (< 0 port 65536) port)))))

(defun ygg-ark--execute-command (data)
  "The console command in an execute comm DATA, or nil."
  (pcase (ygg-ark--comm-rpc data)
    (`("execute" . ,params) (plist-get params :command))))

;;; Kernel

(defun ygg-ark--r-mode-properties ()
  "Jupyter's (MODE SYNTAX-TABLE) for the kernel language R."
  (let ((mode (car ygg-ark-r-modes)))
    (list mode (or (ignore-errors
                     (with-temp-buffer (delay-mode-hooks (funcall mode)) (syntax-table)))
                   (standard-syntax-table)))))

(defun ygg-ark--require ()
  (unless (require 'jupyter-repl nil t)
    (user-error "emacs-jupyter is not installed")))

(defun ygg-ark--pin-r-mode ()
  "Make jupyter associate R kernels with our R mode, whatever .R maps to."
  (when (and (boundp 'jupyter-kernel-language-mode-properties)
             (fboundp (car ygg-ark-r-modes)))
    (let ((properties (ygg-ark--r-mode-properties)))
      ;; Jupyter keys this by the kernel's language name, read as a symbol.
      (dolist (name '(R "R"))
        (setf (alist-get name jupyter-kernel-language-mode-properties nil nil #'equal)
              properties)))))

(defun ygg-ark--ark-client-p (client)
  (and client
       (equal (plist-get (jupyter-kernel-info client) :implementation) "ark")))

(defun ygg-ark--buffer-client ()
  "The ark kernel client this buffer is associated with, or nil."
  (let ((client (ygg-kernel-picker-current-client)))
    (and (ygg-ark--ark-client-p client) client)))

(defun ygg-ark--forget (client &optional target)
  (cl-loop for id being the hash-keys of ygg-ark--comms using (hash-values comm)
           when (and (eq (plist-get comm :client) client)
                     (or (null target) (equal (plist-get comm :target) target)))
           do (remhash id ygg-ark--comms)))

(defun ygg-ark--execute (client code)
  "Run CODE as console input to CLIENT's kernel, in its REPL when it has one."
  (let ((repl (and (object-of-class-p client 'jupyter-repl-client)
                   (slot-value client 'buffer))))
    (if (and (buffer-live-p repl)
             (with-current-buffer repl (string-blank-p (jupyter-repl-cell-code))))
        (with-current-buffer repl
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (jupyter-repl-replace-cell-code code)
            (jupyter-repl-execute-cell client)))
      (jupyter-run-with-state client
        (jupyter-sent (jupyter-execute-request :code code :store-history nil))))))

(defun ygg-ark--on-message (client msg)
  (when (and (equal (jupyter-message-type msg) "status")
             (equal (plist-get (jupyter-message-content msg) :execution_state)
                    "starting"))
    (ygg-ark--forget client))
  (when (equal (jupyter-message-type msg) "comm_msg")
    (let* ((content (jupyter-message-content msg))
           (comm (gethash (plist-get content :comm_id) ygg-ark--comms))
           (data (plist-get content :data)))
      (when comm
        (if-let* ((port (ygg-ark--server-port data)))
            (puthash (plist-get content :comm_id) (plist-put comm :port port)
                     ygg-ark--comms)
          (when-let* ((command (ygg-ark--execute-command data)))
            ;; Out of the I/O callback: the REPL sends and waits.
            (run-at-time 0 nil #'ygg-ark--execute client command)))))))

(defun ygg-ark--watch (client)
  "See every comm_msg CLIENT's kernel sends, whatever request it answers."
  (unless (gethash client ygg-ark--watched)
    (puthash client t ygg-ark--watched)
    (jupyter-run-with-state (jupyter-kernel-io client)
      (jupyter-subscribe
       (jupyter-subscriber
        (lambda (msg)
          (with-demoted-errors "ygg-ark: %S"
            (ygg-ark--on-message client msg))))))))

(defun ygg-ark--live-port (client target)
  (cl-loop for comm being the hash-values of ygg-ark--comms
           when (and (eq (plist-get comm :client) client)
                     (equal (plist-get comm :target) target))
           return (plist-get comm :port)))

(defun ygg-ark-server-port (client target &optional fresh)
  "Port of CLIENT's kernel server for comm TARGET, opening the comm once.
FRESH opens a new comm even when one is open."
  (ygg-ark--require)
  (or (and (not fresh) (ygg-ark--live-port client target))
      (let ((id (format "ygg-ark-%s-%s" target (md5 (format "%s%s" (random) (float-time)))))
            (deadline (+ (float-time) ygg-ark-timeout)))
        (ygg-ark--watch client)
        (ygg-ark--forget client target)
        (puthash id (list :client client :target target) ygg-ark--comms)
        (jupyter-run-with-state client
          (jupyter-sent (jupyter-comm-open :id id :target-name target
                                           :data (ygg-ark--comm-open-data))))
        (while (and (not (plist-get (gethash id ygg-ark--comms) :port))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (or (plist-get (gethash id ygg-ark--comms) :port)
            (progn (remhash id ygg-ark--comms)
                   (error "ark did not start %s within %ss" target ygg-ark-timeout))))))

(defun ygg-ark--background-client ()
  "The headless ark on this buffer's host, started when it has none."
  (ygg-ark--require)
  (let* ((remote (file-remote-p default-directory))
         (client (gethash remote ygg-ark--background)))
    (unless (and client (ignore-errors (jupyter-kernel-alive-p client)))
      (when client (ygg-ark--forget client))
      (setq client (ygg-kernel-picker-note-host (jupyter-client ygg-ark-kernelspec) remote))
      (puthash remote client ygg-ark--background))
    client))

;;; Forwarding

(defun ygg-ark-ssh-destination (remote)
  "The ssh destination for TRAMP prefix REMOTE, named as emacs-jupyter names it."
  (let ((file (tramp-dissect-file-name remote)))
    (unless (member (tramp-file-name-method file) ygg-ark-ssh-methods)
      (user-error "ark on %s needs an ssh host to forward its port"
                  (tramp-file-name-method file)))
    (if-let* ((user (tramp-file-name-user file)))
        (concat user "@" (tramp-file-name-host file))
      (tramp-file-name-host file))))

(defun ygg-ark--tunnel (local port destination)
  "Forward LOCAL to PORT on DESTINATION's loopback; return LOCAL once it is open."
  (let ((tunnel (jupyter-make-ssh-tunnel local port destination ygg-ark-host))
        (deadline (+ (float-time) ygg-ark-timeout)))
    ;; ssh -f exits once the forward is up; its child keeps it until the connection ends.
    (while (and (process-live-p tunnel) (< (float-time) deadline))
      (accept-process-output tunnel 0.05))
    (unless (and (not (process-live-p tunnel)) (zerop (process-exit-status tunnel)))
      (delete-process tunnel)
      (error "No ssh forward to port %s on %s" port destination))
    local))

(defun ygg-ark-reach (client port)
  "A local port that reaches PORT of CLIENT's kernel: PORT itself when local."
  (if-let* ((remote (ygg-kernel-picker-remote client)))
      (ygg-ark--tunnel (car (jupyter-available-local-ports 1)) port
                       (ygg-ark-ssh-destination remote))
    port))

(defun ygg-ark--lsp-stream (client)
  "A connection to a new LSP in CLIENT's kernel, tagged with CLIENT."
  ;; ark's LSP exits with its client, so every connection needs a new one.
  (let* ((port (ygg-ark-reach client (ygg-ark-server-port client ygg-ark-lsp-target t)))
         (process (open-network-stream "ygg-ark-lsp" nil ygg-ark-host port)))
    (process-put process 'ygg-ark-client client)
    process))

(defun ygg-ark-eglot-contact (&optional _interactive _project)
  "Eglot contact for ark's LSP in this buffer's kernel, or a headless one."
  (ygg-ark--require)
  (let ((client (or (ygg-ark--buffer-client) (ygg-ark--background-client))))
    ;; eglot reads a leading symbol as the server class
    (list 'eglot-lsp-server :process (lambda () (ygg-ark--lsp-stream client)))))

(defun ygg-ark--eglot-client ()
  "Kernel client serving this buffer's eglot server, or nil."
  (and-let* (((fboundp 'eglot-current-server))
             (server (eglot-current-server))
             ((jsonrpc-running-p server)))
    (process-get (jsonrpc--process server) 'ygg-ark-client)))

(defun ygg-ark--reap-background (&rest _)
  "Shut each headless kernel down once no LSP connection uses it."
  (cl-loop for remote being the hash-keys of ygg-ark--background using (hash-values client)
           unless (cl-some (lambda (process)
                             (and (process-live-p process)
                                  (eq (process-get process 'ygg-ark-client) client)))
                           (process-list))
           collect (cons remote client) into idle
           finally do (pcase-dolist (`(,remote . ,client) idle)
                        (remhash remote ygg-ark--background)
                        (ygg-ark--forget client)
                        (jupyter-shutdown-kernel client))))

(defun ygg-ark--reap-soon (&rest _)
  ;; Eglot shuts its server down after this runs; look once it has.
  (run-at-time 2 nil #'ygg-ark--reap-background))

;;; Commands

(defun ygg-ark-lsp ()
  "Connect eglot to ark's LSP in this buffer's kernel, or a headless one."
  (interactive)
  (require 'eglot)
  (eglot (list major-mode) (or (project-current) (cons 'transient default-directory))
         'eglot-lsp-server (cdr (ygg-ark-eglot-contact)) (list "r")))

(defun ygg-ark--dape-config (config)
  "Fill CONFIG with the port of this buffer's kernel DAP."
  (let ((client (or (ygg-ark--buffer-client)
                    (user-error "No ark kernel is associated with this buffer"))))
    (plist-put (copy-tree config) 'port
               (ygg-ark-reach client (ygg-ark-server-port client ygg-ark-dap-target)))))

(defun ygg-ark-debug ()
  "Attach dape to the DAP inside this buffer's ark kernel."
  (interactive)
  (ygg-ark--require)
  (require 'dape)
  (dape (copy-tree (alist-get 'ark dape-configs))))

(defun ygg-ark--auto-lsp ()
  "Serve this buffer's LSP from its ark kernel once it has one.
A server already running from another kernel, such as the headless one,
is shut down first; the headless kernel is then reaped."
  (when-let* ((ygg-ark-auto-lsp)
              ((memq major-mode ygg-ark-r-modes))
              (client (ygg-ark--buffer-client))
              ((not (eq (ygg-ark--eglot-client) client))))
    (with-demoted-errors "ygg-ark: %S"
      (when-let* (((fboundp 'eglot-current-server))
                  (server (eglot-current-server)))
        ;; eglot-reconnect would reuse the old contact, which names the old kernel.
        (eglot-shutdown server))
      (ygg-ark-lsp))))

(ygg-ark--pin-r-mode)
(with-eval-after-load 'ygg-r-mode (ygg-ark--pin-r-mode))
(with-eval-after-load 'jupyter-client (ygg-ark--pin-r-mode))

(add-hook 'jupyter-repl-interaction-mode-hook #'ygg-ark--auto-lsp)
(add-hook 'ygg-nb-kernel-hook #'ygg-ark--auto-lsp)
(add-hook 'eglot-managed-mode-hook #'ygg-ark--reap-soon)
(advice-add 'eglot-shutdown :after #'ygg-ark--reap-soon)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               (cons ygg-ark-r-modes #'ygg-ark-eglot-contact)))

(when (executable-find "ark")
  (dolist (mode ygg-ark-r-modes)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure)))

(with-eval-after-load 'dape
  (setf (alist-get 'ark dape-configs)
        `(modes ,ygg-ark-r-modes
                fn ygg-ark--dape-config
                host ,ygg-ark-host
                :type "ark"
                :request "attach")))

(provide 'ygg-ark)
;;; ygg-ark.el ends here
