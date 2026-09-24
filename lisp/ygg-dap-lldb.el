;;; ygg-dap-lldb.el --- C, C++, Rust and Swift debugging on dape's lldb-dap config -*- lexical-binding: t; -*-

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)

(defvar dape-configs)
(declare-function dape-cwd "dape")
(declare-function dape-command-cwd "dape")
(declare-function ygg-swift-build-command "layer-swift")
(declare-function ygg-swift-launch-waiting-for-debugger "layer-swift")

(defgroup ygg-dap-lldb nil
  "Native debugging with lldb-dap through dape."
  :group 'tools
  :prefix "ygg-dap-lldb-")

(defcustom ygg-dap-lldb-gdb-remote-port 1234
  "Port the gdbserver or lldb-server gdbserver listens on for the attach configs."
  :type 'natnum)

(defcustom ygg-dap-lldb-container-methods '("docker" "podman")
  "TRAMP methods whose hosts are containers reached through a published local port."
  :type '(repeat string))

(defvar-local ygg-dap-lldb-remote-root nil
  "TRAMP directory the debuggee was built in, as /docker:app:/src or /ssh:box:/src.
Set it in .dir-locals.el of a local checkout of those sources.")
(put 'ygg-dap-lldb-remote-root 'safe-local-variable #'stringp)

(defvar ygg-dap-lldb--xcrun-command nil
  "Path xcrun gave for lldb-dap, kept once it answered.")

(defun ygg-dap-lldb--xcrun ()
  "Ask xcrun for the lldb-dap inside the selected Xcode."
  (with-temp-buffer
    (when (eql 0 (ignore-errors (call-process "xcrun" nil '(t nil) nil "-f" "lldb-dap")))
      (let ((path (string-trim (buffer-string))))
        (and (file-executable-p path) path)))))

(defun ygg-dap-lldb-local-command ()
  "The lldb-dap on this machine: PATH first, then Xcode's."
  (let ((default-directory temporary-file-directory))
    (or (executable-find "lldb-dap")
        ygg-dap-lldb--xcrun-command
        (setq ygg-dap-lldb--xcrun-command (ygg-dap-lldb--xcrun))
        "lldb-dap")))

(defun ygg-dap-lldb-command ()
  "The lldb-dap for a launch where the project is."
  (if-let* ((remote (file-remote-p default-directory)))
      (or (executable-find "lldb-dap" t)
          (user-error "No lldb-dap on %s; run gdbserver there and use lldb-dap-remote or lldb-dap-docker"
                      remote))
    (ygg-dap-lldb-local-command)))

(defun ygg-dap-lldb--read (file regexp)
  "First REGEXP group in FILE, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (when (re-search-forward regexp nil t)
        (match-string 1)))))

(defun ygg-dap-lldb-program ()
  "The binary a build of the project at point leaves, relative to the project root."
  (let ((root (dape-command-cwd)))
    (cond
     ((when-let* ((crate (ygg-dap-lldb--read
                          (expand-file-name "Cargo.toml" root)
                          "^\\[package\\][^[]*?\nname *= *\"\\([^\"]+\\)\"")))
        (concat "target/debug/" crate)))
     ((file-exists-p (expand-file-name "Package.swift" root))
      (concat ".build/debug/"
              (or (ygg-dap-lldb--read (expand-file-name "Package.swift" root)
                                      "\\.\\(?:executable\\|executableTarget\\)( *name: *\"\\([^\"]+\\)\"")
                  (file-name-nondirectory (directory-file-name root)))))
     ((when-let* ((file buffer-file-name)
                  (binary (file-name-sans-extension file))
                  ((not (equal binary file)))
                  ((file-regular-p binary))
                  ((file-executable-p binary)))
        (file-relative-name binary root)))
     ("a.out"))))

(defun ygg-dap-lldb--container-p (remote)
  "Whether REMOTE, a TRAMP name, is a container."
  (member (file-remote-p remote 'method) ygg-dap-lldb-container-methods))

(defun ygg-dap-lldb--remote-root (container)
  "The TRAMP root the sources were built in, in a CONTAINER or else on a host."
  (seq-find (lambda (dir)
              (and (stringp dir)
                   (file-remote-p dir)
                   (eq (not container) (not (ygg-dap-lldb--container-p dir)))))
            (list (and (file-remote-p default-directory) (dape-command-cwd))
                  ygg-dap-lldb-remote-root)))

(defun ygg-dap-lldb-docker-root ()
  "The container sources root, from the visited TRAMP file or the dir-local."
  (ygg-dap-lldb--remote-root t))

(defun ygg-dap-lldb-ssh-root ()
  "The remote host sources root, from the visited TRAMP file or the dir-local."
  (ygg-dap-lldb--remote-root nil))

(defun ygg-dap-lldb-attach-cwd ()
  "A local directory, so lldb-dap runs here and not beside the debuggee."
  (if (file-remote-p default-directory)
      temporary-file-directory
    (dape-command-cwd)))

(defun ygg-dap-lldb--attach-settings (remote-root visited-dir local-root)
  "Hostname and path translation for sources built under REMOTE-ROOT.
VISITED-DIR is where the session starts; LOCAL-ROOT is the local checkout."
  (let ((prefix (file-remote-p remote-root)))
    `(:gdb-remote-hostname ,(if (ygg-dap-lldb--container-p remote-root)
                                "localhost"
                              (file-remote-p remote-root 'host))
      ,@(if (equal (file-remote-p visited-dir) prefix)
            `(prefix-local ,prefix prefix-remote "")
          `(:sourceMap [[,(directory-file-name (file-local-name remote-root))
                         ,(directory-file-name (expand-file-name local-root))]])))))

(defun ygg-dap-lldb--fetch (program remote-root)
  "A local copy of PROGRAM under REMOTE-ROOT, since lldb reads symbols locally."
  (if (and (file-name-absolute-p program)
           (not (file-remote-p program))
           (file-regular-p program))
      program
    (let* ((remote (if (file-remote-p program)
                       program
                     (concat (file-remote-p remote-root)
                             (expand-file-name program (file-local-name remote-root)))))
           (local (expand-file-name (format "ygg-dap-lldb-%s-%s"
                                            (substring (md5 remote) 0 8)
                                            (file-name-nondirectory remote))
                                    temporary-file-directory)))
      (unless (file-regular-p remote)
        (user-error "No %s to debug; build it or pass :program" remote))
      (copy-file remote local t)
      local)))

(defun ygg-dap-lldb-attach-resolve (config)
  "Fill CONFIG's host, program copy and path translation from its remote-root."
  (let ((remote-root (plist-get config 'remote-root)))
    (unless (and (stringp remote-root) (file-remote-p remote-root))
      (user-error "Say where the sources were built: remote-root \"/docker:CONTAINER:/src\" or \"/ssh:HOST:/src\""))
    (let ((config (copy-sequence config)))
      (map-do (lambda (key value)
                (unless (plist-member config key)
                  (setq config (plist-put config key value))))
              (ygg-dap-lldb--attach-settings remote-root default-directory (dape-cwd)))
      (plist-put config :program
                 (ygg-dap-lldb--fetch (or (plist-get config :program) "a.out") remote-root)))))

(defun ygg-dap-lldb--extend (plist)
  "PLIST, dape's lldb-dap entry, with the command resolved and Swift added."
  (map-merge 'plist plist
             `(modes ,(seq-uniq (append (plist-get plist 'modes) '(swift-mode swift-ts-mode)))
               command ygg-dap-lldb-command
               :program ygg-dap-lldb-program)))

(defun ygg-dap-lldb--attach (plist root)
  "An attach entry derived from PLIST that finds its sources with ROOT."
  (map-merge 'plist (map-delete (copy-sequence plist) :cwd)
             `(fn ygg-dap-lldb-attach-resolve
               command ygg-dap-lldb-local-command
               command-cwd ygg-dap-lldb-attach-cwd
               remote-root ,root
               :request "attach"
               :gdb-remote-port ygg-dap-lldb-gdb-remote-port)))

(defun ygg-dap-lldb--put (name plist)
  "Set dape config NAME to PLIST, appended so launch entries stay the default."
  (if (assq name dape-configs)
      (setf (alist-get name dape-configs) plist)
    (setq dape-configs (append dape-configs (list (cons name plist))))))

(defun ygg-dap-lldb-install ()
  "Extend dape's lldb-dap entry and derive remote and container attach entries."
  (when-let* ((base (alist-get 'lldb-dap dape-configs)))
    (let ((local (ygg-dap-lldb--extend (copy-tree base))))
      (setf (alist-get 'lldb-dap dape-configs) local)
      (ygg-dap-lldb--put 'lldb-dap-remote
                         (ygg-dap-lldb--attach local 'ygg-dap-lldb-ssh-root))
      (ygg-dap-lldb--put 'lldb-dap-docker
                         (ygg-dap-lldb--attach local 'ygg-dap-lldb-docker-root)))))

(with-eval-after-load 'dape
  (ygg-dap-lldb-install))

(provide 'ygg-dap-lldb)
;;; ygg-dap-lldb.el ends here
