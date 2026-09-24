;;; ygg-jdk.el --- One pinned JDK, overridden per project by mise or direnv -*- lexical-binding: t; -*-

;; The exact java in mise's global config is the JDK wherever a project
;; names none; a JAVA_HOME that mise.el or envrc gives a buffer wins.

;;; Code:

(require 'subr-x)

(defconst ygg-jdk--mise-config (expand-file-name "~/.config/mise/config.toml")
  "The mise config whose java pin is the default JDK.")

(defconst ygg-jdk--mise-java-installs (expand-file-name "~/.local/share/mise/installs/java/")
  "Where mise keeps each installed JDK.")

(defun ygg-jdk--pinned-version (config)
  "The java version the mise CONFIG file pins, or nil."
  (with-temp-buffer
    (ignore-errors (insert-file-contents config))
    (when (re-search-forward "^java[ \t]*=[ \t]*\"\\([^\"]+\\)\"" nil t)
      (match-string 1))))

(defun ygg-jdk--home-p (home)
  (and (stringp home) (file-executable-p (expand-file-name "bin/java" home))))

(defun ygg-jdk--mise-home (version)
  "The mise install of java VERSION, or nil when it is not installed."
  (when-let* ((version)
              (home (expand-file-name version ygg-jdk--mise-java-installs))
              ((ygg-jdk--home-p home)))
    home))

(defcustom ygg-jdk-default-home
  (ygg-jdk--mise-home (ygg-jdk--pinned-version ygg-jdk--mise-config))
  "The JDK used wherever a project names none.
Defaults to the exact java version pinned in mise's global config."
  :type '(choice directory (const nil))
  :group 'tools)

(defun ygg-jdk-home ()
  "The JDK for the current buffer: its JAVA_HOME, else `ygg-jdk-default-home'.
mise.el and envrc make JAVA_HOME buffer-local for a project that sets one."
  (let ((home (getenv "JAVA_HOME")))
    (if (ygg-jdk--home-p home)
        (directory-file-name (expand-file-name home))
      ygg-jdk-default-home)))

(defvar envrc--running)

(defun ygg-jdk-settled-home (&optional timeout)
  "`ygg-jdk-home' once direnv is done with this buffer, waiting TIMEOUT seconds.
direnv runs async; a server started before it answers would run on the
pin for the whole session."
  (let ((deadline (+ (float-time) (or timeout 2))))
    (while (and (bound-and-true-p envrc--running) (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (ygg-jdk-home))

(defun ygg-jdk-major (home)
  "The major Java version of the JDK at HOME, read from its release file."
  (with-temp-buffer
    (ignore-errors (insert-file-contents (expand-file-name "release" home)))
    (when (re-search-forward "^JAVA_VERSION=\"\\(?:1\\.\\)?\\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun ygg-jdk-export-default ()
  "Put the pin first on the global JAVA_HOME and PATH, unless JAVA_HOME is set.
The daemon's PATH is frozen at build time with whatever JDK was current then."
  (let ((env (default-value 'process-environment)))
    (when (and ygg-jdk-default-home
               (not (getenv-internal "JAVA_HOME" env)))
      (let ((bin (expand-file-name "bin" ygg-jdk-default-home))
            (path (getenv-internal "PATH" env)))
        (setq-default exec-path (cons bin (default-value 'exec-path)))
        (setq-default process-environment
                      (append (list (concat "JAVA_HOME=" ygg-jdk-default-home)
                                    (concat "PATH=" bin (and path (concat path-separator path))))
                              env))))))

(ygg-jdk-export-default)

(defun ygg-jdk-jupyter-environment (env)
  "ENV from a kernelspec with its JDK variables pointed at `ygg-jdk-home'."
  (let ((home (ygg-jdk-home)))
    (mapcar (lambda (entry)
              (if (and home (string-match "\\`\\(JAVA_HOME\\|KOTLIN_JUPYTER_JAVA_HOME\\)=" entry))
                  (concat (match-string 1 entry) "=" home)
                entry))
            env)))

(defun ygg-jdk-jupyter-argv (argv)
  "ARGV with a kernel launched by a JDK's own java run on `ygg-jdk-home' instead."
  (if-let* ((home (ygg-jdk-home))
            ((string-suffix-p "/bin/java" (car argv))))
      (cons (expand-file-name "bin/java" home) (cdr argv))
    argv))

(with-eval-after-load 'jupyter-kernelspec
  (advice-add 'jupyter-process-environment :filter-return #'ygg-jdk-jupyter-environment)
  (advice-add 'jupyter-kernel-argv :filter-return #'ygg-jdk-jupyter-argv))

(provide 'ygg-jdk)
;;; ygg-jdk.el ends here
