;;; ygg-dap-js.el --- Node debugging on dape's js-debug configs -*- lexical-binding: t; -*-

;; The adapter always runs here; remote and container Node is reached by attaching to its inspector.

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)

(defvar dape-configs)
(declare-function dape-cwd "dape")
(declare-function dape-command-cwd "dape")

(defgroup ygg-dap-js nil
  "Node debugging with js-debug through dape."
  :group 'tools
  :prefix "ygg-dap-js-")

(defcustom ygg-dap-js-installs "~/.local/share/mise/installs/http-js-debug/"
  "Where mise keeps the js-debug adapter versions."
  :type 'directory)

(defcustom ygg-dap-js-docker-program "docker"
  "The container CLI asked for ports, addresses and mounts."
  :type 'string)

(defcustom ygg-dap-js-inspector-port 9229
  "The port node --inspect listens on inside the remote host or container."
  :type 'natnum)

;;; The adapter

(defun ygg-dap-js-server ()
  "The newest installed dapDebugServer.js, or nil."
  (when-let* ((versions (seq-filter
                         (lambda (dir)
                           (and (not (file-symlink-p dir))
                                (string-match-p "\\`[0-9][0-9.]*\\'" (file-name-nondirectory dir))))
                         (file-expand-wildcards (expand-file-name "*" ygg-dap-js-installs))))
              (newest (car (sort versions (lambda (a b)
                                            (version< (file-name-nondirectory b)
                                                      (file-name-nondirectory a))))))
              (server (expand-file-name "src/dapDebugServer.js" newest)))
    (and (file-exists-p server) server)))

(defvar ygg-dap-js--dape-ensure nil
  "The ensure dape's js-debug entries shipped with.")

(defun ygg-dap-js-ensure (config)
  "Refuse to start the adapter on a remote host, else run dape's check on CONFIG."
  (when (file-remote-p default-directory)
    (user-error "Remote Node launch is not supported; run node --inspect there and use js-debug-node-attach-remote or -docker"))
  (when ygg-dap-js--dape-ensure
    (funcall ygg-dap-js--dape-ensure config)))

;;; Where the inspector is

(defun ygg-dap-js-local-cwd ()
  "A local directory for the adapter: the project root, or home when remote."
  (if (file-remote-p default-directory)
      (expand-file-name "~/")
    (dape-command-cwd)))

(defun ygg-dap-js-tramp-target (directory)
  "Remote DIRECTORY's TRAMP :prefix, :method, :user and :host, as a plist."
  (when-let* ((prefix (file-remote-p directory)))
    (list :prefix prefix
          :method (file-remote-p directory 'method)
          :user (file-remote-p directory 'user)
          :host (file-remote-p directory 'host))))

(defun ygg-dap-js-docker-endpoint (output)
  "Address and port, as a cons, of the first mapping in docker port OUTPUT."
  (when-let* ((line (car (split-string (or output "") "\n" t " +")))
              ((string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" line)))
    (let ((address (match-string 1 line))
          (port (string-to-number (match-string 2 line))))
      (cons (if (member address '("0.0.0.0" "[::]" "::" "")) "127.0.0.1" address)
            port))))

(defun ygg-dap-js-mounted-path (directory mounts)
  "Where DIRECTORY appears in a container with MOUNTS.
MOUNTS is a list of (SOURCE . DESTINATION)."
  (let ((directory (file-name-as-directory directory)))
    (seq-some (lambda (mount)
                (let ((source (file-name-as-directory (car mount))))
                  (when (string-prefix-p source directory)
                    (directory-file-name
                     (concat (file-name-as-directory (cdr mount))
                             (string-remove-prefix source directory))))))
              (seq-sort-by (lambda (mount) (length (car mount))) #'> mounts))))

(defun ygg-dap-js--docker (&rest args)
  "Output of the container CLI run with ARGS, or nil when it fails."
  (let ((default-directory (expand-file-name "~/")))
    (with-temp-buffer
      (when (eql 0 (apply #'call-process ygg-dap-js-docker-program nil '(t nil) nil args))
        (string-trim (buffer-string))))))

(defun ygg-dap-js--docker-mounts (container)
  "CONTAINER's bind mounts as (SOURCE . DESTINATION), sources resolved."
  (when-let* ((json (ygg-dap-js--docker "inspect" "-f" "{{json .Mounts}}" container)))
    (mapcar (lambda (mount)
              (cons (file-truename (alist-get 'Source mount)) (alist-get 'Destination mount)))
            (json-parse-string json :object-type 'alist :array-type 'list))))

(defun ygg-dap-js--docker-address (container port)
  "How to reach CONTAINER's PORT from here: its published port, else its own IP."
  (or (ygg-dap-js-docker-endpoint
       (ygg-dap-js--docker "port" container (format "%s/tcp" port)))
      (when-let* ((ip (ygg-dap-js--docker
                       "inspect" "-f"
                       "{{range .NetworkSettings.Networks}}{{.IPAddress}} {{end}}" container))
                  ((not (string-empty-p ip))))
        (cons (car (split-string ip)) port))
      (user-error "Container %s neither publishes port %s nor has an address" container port)))

(defun ygg-dap-js--read-container ()
  "Pick a running container."
  (let ((names (split-string (or (ygg-dap-js--docker "ps" "--format" "{{.Names}}") "") "\n" t)))
    (unless names
      (user-error "No running containers"))
    (completing-read "Container running node --inspect: " names nil t nil nil (car names))))

(defun ygg-dap-js--put-absent (config &rest pairs)
  "CONFIG with each key of PAIRS set to its value, unless CONFIG already has it.
A value that is a function is called only when its key is needed."
  (while pairs
    (let ((key (pop pairs))
          (value (pop pairs)))
      (unless (plist-member config key)
        (setq config (plist-put config key (if (functionp value) (funcall value) value))))))
  config)

(defun ygg-dap-js--tramp-roots (config target)
  "CONFIG translating through TARGET's TRAMP prefix, roots in the host's paths."
  (let ((root (dape-cwd)))
    (ygg-dap-js--put-absent config
                            'prefix-local (plist-get target :prefix)
                            'prefix-remote ""
                            :localRoot root
                            :remoteRoot root)))

(defun ygg-dap-js-remote-resolve (config)
  "CONFIG attaching to node --inspect on this file's host, or one asked for."
  (if-let* ((target (ygg-dap-js-tramp-target default-directory)))
      (ygg-dap-js--put-absent (ygg-dap-js--tramp-roots config target)
                              :address (plist-get target :host))
    (let ((root (dape-cwd)))
      (ygg-dap-js--put-absent
       config
       :address (lambda () (read-string "Host running node --inspect: "))
       :localRoot root
       :remoteRoot (lambda () (read-string (format "Path of %s on that host: " root) root))))))

(defun ygg-dap-js-docker-resolve (config)
  "CONFIG attaching to node --inspect in a container.
In a TRAMP container buffer that container, else one picked; a local
checkout maps to the container path it is bind-mounted at."
  (let* ((target (ygg-dap-js-tramp-target default-directory))
         (config (ygg-dap-js--put-absent
                  config
                  'container (lambda () (or (plist-get target :host)
                                            (ygg-dap-js--read-container)))))
         (container (plist-get config 'container)))
    (unless (plist-member config :address)
      (let ((endpoint (ygg-dap-js--docker-address
                       container (or (plist-get config :port) ygg-dap-js-inspector-port))))
        (setq config (plist-put config :address (car endpoint)))
        (setq config (plist-put config :port (cdr endpoint)))))
    (if target
        (ygg-dap-js--tramp-roots config target)
      (let ((root (dape-cwd)))
        (ygg-dap-js--put-absent
         config
         :localRoot root
         :remoteRoot (lambda ()
                       (or (ygg-dap-js-mounted-path (file-truename root)
                                                    (ygg-dap-js--docker-mounts container))
                           (read-string (format "Path of %s in %s: " root container)))))))))

;;; Extending dape's entries

(defun ygg-dap-js--js-debug-names ()
  "Names of dape's js-debug entries."
  (seq-filter (lambda (name) (string-prefix-p "js-debug" (symbol-name name)))
              (mapcar #'car dape-configs)))

(defun ygg-dap-js-install ()
  "Point dape's js-debug entries at the installed adapter; derive attach entries."
  (let ((server (ygg-dap-js-server)))
    (dolist (name (ygg-dap-js--js-debug-names))
      (let ((plist (alist-get name dape-configs)))
        (unless (eq (plist-get plist 'ensure) #'ygg-dap-js-ensure)
          (setq ygg-dap-js--dape-ensure (plist-get plist 'ensure)))
        (setf (alist-get name dape-configs)
              (map-merge 'plist plist
                         `(ensure ygg-dap-js-ensure
                           ,@(when server `(command-args (,server :autoport)))))))))
  (when-let* ((attach (alist-get 'js-debug-node-attach dape-configs)))
    (dolist (variant '((js-debug-node-attach-remote . ygg-dap-js-remote-resolve)
                       (js-debug-node-attach-docker . ygg-dap-js-docker-resolve)))
      (setf (alist-get (car variant) dape-configs)
            (map-merge 'plist attach
                       `(command-cwd ygg-dap-js-local-cwd
                         fn ,(cdr variant)
                         :port ,ygg-dap-js-inspector-port))))))

(with-eval-after-load 'dape
  (ygg-dap-js-install))

(provide 'ygg-dap-js)
;;; ygg-dap-js.el ends here
