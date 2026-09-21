;;; layer-tramp.el --- Helix/vim-style remote editing over TRAMP -*- lexical-binding: t; -*-

;; Built-ins wrapped: tramp + tramp-container (docker/podman/kubernetes
;; methods ship in Emacs 30), recentf, shell.  Remote is where a file
;; lives, not a world of its own, so the "connect / edit as root / clean
;; up" verbs sit under the files prefix beside the local ones.

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-leader)

(declare-function tramp-cleanup-all-connections "tramp" nil t)
(declare-function tramp-cleanup-all-buffers "tramp" nil t)

(defun ygg-remote--ssh-hosts ()
  "Host aliases from ~/.ssh/config (wildcards skipped)."
  (let (hosts (cfg (expand-file-name "~/.ssh/config")))
    (when (file-readable-p cfg)
      (with-temp-buffer
        (insert-file-contents cfg)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*Host[ \t]+\\(.+\\)$" nil t)
          (dolist (h (split-string (match-string 1)))
            (unless (string-match-p "[*?]" h) (push h hosts))))))
    (delete-dups (nreverse hosts))))

(defun ygg-remote-ssh (host)
  "Open HOST's home directory over ssh."
  (interactive (list (completing-read "ssh host: " (ygg-remote--ssh-hosts))))
  (when (string-empty-p host) (user-error "No host"))
  (find-file (format "/ssh:%s:" host)))

(defun ygg-remote--containers (method)
  "Running container names for METHOD (\"docker\" or \"podman\")."
  (when-let* ((bin (executable-find method)))
    (with-temp-buffer
      (when (eq 0 (process-file bin nil t nil "ps" "--format" "{{.Names}}"))
        (split-string (buffer-string) "\n" t)))))

(defun ygg-remote-container ()
  "Open a running container's filesystem over TRAMP (docker or podman)."
  (interactive)
  (let* ((method (cond ((executable-find "docker") "docker")
                       ((executable-find "podman") "podman")
                       (t (user-error "Neither docker nor podman on PATH"))))
         (names (ygg-remote--containers method)))
    (unless names (user-error "No running %s containers" method))
    (find-file (format "/%s:%s:/" method (completing-read "container: " names nil t)))))

(defun ygg-remote-sudo ()
  "Re-open the current file (or dir) as root via sudo, multi-hop aware."
  (interactive)
  (let* ((f (or buffer-file-name
                (and (derived-mode-p 'dired-mode) default-directory)
                (user-error "Buffer visits no file")))
         (target
          (if (file-remote-p f)
              ;; already remote: hop sudo on the same host (public component API)
              (format "/%s:%s|sudo:%s:%s"
                      (file-remote-p f 'method) (file-remote-p f 'host)
                      (file-remote-p f 'host) (file-remote-p f 'localname))
            (concat "/sudo::" (expand-file-name f)))))
    (find-alternate-file target)))

(defvar ygg-term-display-action)

(defun ygg-remote-shell ()
  "Open a shell on the current buffer's host (works on remote dirs)."
  (interactive)
  (let ((display-buffer-overriding-action ygg-term-display-action))
    (shell (generate-new-buffer-name
            (format "*shell:%s*" (or (file-remote-p default-directory 'host) "local"))))))

(defun ygg-remote-recent ()
  "Pick from recently opened remote (TRAMP) files."
  (interactive)
  (require 'recentf)
  (let ((remotes (seq-filter #'file-remote-p recentf-list)))
    (unless remotes (user-error "No recent remote files"))
    (find-file (completing-read "remote recent: " remotes nil t))))

(defun ygg-remote-cleanup ()
  "Drop every TRAMP connection and its buffers."
  (interactive)
  (require 'tramp)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections)
  (message "tramp: all connections closed"))

(defvar ygg-remote-lsp-servers
  '((python-mode . "pyright") (python-ts-mode . "pyright")
    (js-mode . "typescript-language-server")
    (js-ts-mode . "typescript-language-server")
    (typescript-ts-mode . "typescript-language-server")
    (tsx-ts-mode . "typescript-language-server")
    (go-mode . "gopls") (go-ts-mode . "gopls")
    (rust-mode . "rust-analyzer") (rust-ts-mode . "rust-analyzer")
    (sh-mode . "bash-language-server") (bash-ts-mode . "bash-language-server")
    (c-mode . "clangd") (c-ts-mode . "clangd")
    (c++-mode . "clangd") (c++-ts-mode . "clangd")
    (lua-mode . "lua-language-server"))
  "Major mode -> language-server binary expected on the remote host.")

(defvar ygg-remote-lsp-recipes
  '(("pyright" (npm "npm install -g pyright"))
    ("typescript-language-server"
     (npm "npm install -g typescript-language-server typescript"))
    ("gopls" (go "go install golang.org/x/tools/gopls@latest"))
    ("rust-analyzer" (rustup "rustup component add rust-analyzer")
     (brew "brew install rust-analyzer"))
    ("bash-language-server" (npm "npm install -g bash-language-server"))
    ("clangd" (apt-get "sudo -n apt-get install -y clangd")
     (dnf "sudo -n dnf install -y clang-tools-extra") (brew "brew install llvm"))
    ("lua-language-server" (brew "brew install lua-language-server")
     (apk "apk add lua-language-server")))
  "Latest-install commands per server, keyed by remote package manager.")

(defun ygg-remote--managers ()
  "Package managers present on the current buffer's remote host."
  (with-temp-buffer
    (when (eq 0 (process-file
                 "sh" nil t nil "-c"
                 "for m in npm pip pipx go rustup brew apt-get dnf apk pacman zypper; do command -v $m >/dev/null 2>&1 && echo $m; done"))
      (split-string (buffer-string) "\n" t))))

(defun ygg-remote-provision-lsp (&optional server)
  "Install SERVER (default: this buffer's language server) on the remote host."
  (interactive)
  (unless (file-remote-p default-directory)
    (user-error "Not on a remote host"))
  (let* ((server (or server
                     (cdr (assq major-mode ygg-remote-lsp-servers))
                     (completing-read "Provision server: "
                                      (mapcar #'car ygg-remote-lsp-recipes) nil t)))
         (host (file-remote-p default-directory 'host))
         (recipes (cdr (assoc server ygg-remote-lsp-recipes))))
    (unless recipes (user-error "No install recipe for %s" server))
    (if (executable-find server t)
        (message "%s already on %s" server host)
      (let* ((managers (ygg-remote--managers))
             (recipe (seq-find (lambda (r) (member (symbol-name (car r)) managers))
                               recipes)))
        (unless recipe
          (user-error "No usable package manager on %s for %s" host server))
        (when (y-or-n-p (format "%s: run `%s'? " host (cadr recipe)))
          (with-temp-buffer
            (if (eq 0 (process-file "sh" nil t nil "-c" (cadr recipe)))
                (message "%s: installed %s" host server)
              (user-error "install failed: %s"
                          (string-trim (buffer-string))))))))))

(yggdrasil-define-keys 'ygg-leader-file-map
  "h" #'ygg-remote-ssh :label "ssh host"
  "d" #'ygg-remote-container :label "docker/podman container"
  "u" #'ygg-remote-sudo :label "sudo edit (root)"
  "o" #'ygg-remote-recent :label "recent remote file"
  "c" #'ygg-remote-cleanup :label "close all connections")

(defvar ygg-leader-code-map)
(defvar ygg-leader-terminal-map)

(with-eval-after-load 'layer-lsp
  (yggdrasil-define-keys 'ygg-leader-code-map
    "p" #'ygg-remote-provision-lsp :label "provision LSP server"))

(with-eval-after-load 'layer-terminal
  (yggdrasil-define-keys 'ygg-leader-terminal-map
    "r" #'ygg-remote-shell :label "remote shell"))

;; sane remote defaults: reuse ssh ControlMaster, cache stats, stay quiet
(with-eval-after-load 'tramp
  ;; resolve the remote user's real PATH so ~/.local, cargo, go, brew bins are found
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-default-method "ssh"
        tramp-verbose 1
        remote-file-name-inhibit-cache 60
        tramp-use-ssh-controlmaster-options t
        ;; a stalled connect errors instead of hanging; ServerAlive catches drops
        tramp-connection-timeout 30
        tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto -o ControlPath='tramp.%%C' "
                "-o ControlPersist=no "
                "-o ServerAliveInterval=15 -o ServerAliveCountMax=3")))

;; know your host at a glance: badge the mode line on remote buffers
(add-to-list 'mode-line-misc-info
             '(:eval (when-let* ((h (file-remote-p default-directory 'host)))
                       (propertize (format " @%s" h) 'face 'mode-line-emphasis
                                   'help-echo (abbreviate-file-name default-directory))))
             t)

(provide 'layer-tramp)
;;; layer-tramp.el ends here
