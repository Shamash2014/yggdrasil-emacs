;;; ygg-project-commands.el --- what a project can be told to run -*- lexical-binding: t; -*-

;; The sidebar asks what a project can run every time it draws, and it
;; draws while a key is held down.  So nothing that touches the disk runs
;; when it is asked: the manifests are read once from an idle timer, just
;; is asked through a process and a sentinel, and the drawing side only
;; ever sees the cache the last scan left behind.

;;; Code:

(require 'compile)
(require 'seq)
(require 'subr-x)

(defconst ygg-project-commands--manifests
  '("justfile" "Justfile" "Makefile" "makefile" "GNUmakefile" "package.json"
    "pnpm-lock.yaml" "yarn.lock" "pnpm-workspace.yaml" "Cargo.toml"
    "go.mod" "go.work" "pyproject.toml" "poetry.lock")
  "Files whose presence and mtime decide what a project can run.")

(defconst ygg-project-commands--cargo '("build" "test" "run" "check" "clippy" "fmt"))

(defconst ygg-project-commands--mix '("deps.get" "compile" "test" "format")
  "Mix tasks every Elixir project has, whatever it is.")

(defconst ygg-project-commands--mix-deps
  '(("phoenix" "phx.server" "phx.routes")
    ("ecto" "ecto.migrate" "ecto.rollback" "ecto.reset")
    ("credo" "credo")
    ("dialyxir" "dialyzer"))
  "Tasks worth offering only where the dependency that provides them is.")

(defconst ygg-project-commands--go
  '(("build" . "build ./...") ("test" . "test ./...") ("vet" . "vet ./...")))

(defvar ygg-project-commands--cache (make-hash-table :test #'equal)
  "Root -> (:commands LIST :workspaces LIST :stamp STAMP), as last scanned.")

(defvar ygg-project-commands--pending (make-hash-table :test #'equal)
  "Root -> callbacks waiting on a scan that is already under way.")


;;; Reading files, never fatally

(defun ygg-project-commands--key (dir)
  "DIR in the one spelling everything here compares by."
  (file-name-as-directory (expand-file-name dir)))

(defun ygg-project-commands--file (dir name)
  "NAME under DIR if it is there to be read."
  (let ((path (expand-file-name name dir)))
    (and (file-regular-p path) path)))

(defun ygg-project-commands--first (dir &rest names)
  "The first of NAMES present under DIR."
  (seq-some (lambda (name) (ygg-project-commands--file dir name)) names))

(defun ygg-project-commands--lines (path)
  "PATH split into lines, or nil if it cannot be read."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents path)
      (split-string (buffer-string) "\n"))))

(defun ygg-project-commands--json (path)
  "PATH parsed into alists, or nil if it is missing or malformed.
The false object is nil on purpose: the default is the keyword :false,
which is true everywhere anyone would test it."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil))))

(defun ygg-project-commands--unquote (s)
  "S without its surrounding quotes, its trailing comment or its padding."
  (let ((s (string-trim (replace-regexp-in-string "[ \t]+#.*\\'" "" s))))
    (if (string-match "\\`[\"']\\(.*\\)[\"']\\'" s) (match-string 1 s) s)))

(defun ygg-project-commands--stamp (dirs)
  "Every manifest under DIRS and when it was last written.
Comparing this against the last one catches a file edited, added or
deleted, which is the whole of what a rescan is for."
  (let (out)
    (dolist (dir dirs (nreverse out))
      (dolist (name ygg-project-commands--manifests)
        (let* ((path (expand-file-name name dir))
               (attrs (file-attributes path)))
          (when attrs
            (push (cons path (file-attribute-modification-time attrs)) out)))))))


;;; Monorepo members

(defun ygg-project-commands--expand (patterns dir)
  "PATTERNS resolved against DIR, keeping the directories.
Globbing goes one level: two stars are read as one, since a member
nested deeper than its own glob admits is not a shape worth guessing."
  (let ((default-directory dir) out)
    (dolist (pattern patterns (nreverse out))
      (unless (or (string-empty-p pattern) (string-prefix-p "!" pattern))
        (dolist (hit (ignore-errors
                       (file-expand-wildcards
                        (replace-regexp-in-string "\\*\\*" "*" pattern) t)))
          (when (file-directory-p hit)
            (push (ygg-project-commands--key hit) out)))))))

(defun ygg-project-commands--npm-workspaces (dir)
  "Members from DIR's package.json, in either the array or the packages form."
  (when-let* ((path (ygg-project-commands--file dir "package.json"))
              (json (ygg-project-commands--json path))
              (declared (alist-get 'workspaces json))
              (patterns (if (and (consp declared) (consp (car declared)))
                            (alist-get 'packages declared)
                          declared)))
    (ygg-project-commands--expand (seq-filter #'stringp patterns) dir)))

(defun ygg-project-commands--pnpm-workspaces (dir)
  "Members listed under packages: in DIR's pnpm-workspace.yaml.
Deliberately shallow: the entries are read off the lines with no YAML
behind them, so anything past a flat list of globs is not seen."
  (when-let* ((path (ygg-project-commands--file dir "pnpm-workspace.yaml"))
              (lines (ygg-project-commands--lines path)))
    (let (patterns inside)
      (dolist (line lines)
        (cond ((string-match "\\`packages:[ \t]*\\'" line) (setq inside t))
              ((not inside) nil)
              ((string-match "\\`[ \t]+-[ \t]*\\(.*\\)\\'" line)
               (push (ygg-project-commands--unquote (match-string 1 line)) patterns))
              ((string-match-p "\\`[ \t]*\\(#.*\\)?\\'" line) nil)
              (t (setq inside nil))))
      (ygg-project-commands--expand (nreverse patterns) dir))))

(defun ygg-project-commands--toml-section (lines head)
  "The body of TOML section HEAD within LINES.
Deliberately shallow: headers are matched as written and nothing is
unescaped, which is enough for the handful of keys read here."
  (let (out inside)
    (dolist (line lines (nreverse out))
      (cond ((string-match "\\`[ \t]*\\[\\(.*\\)\\][ \t]*\\'" line)
             (setq inside (equal (string-trim (match-string 1 line)) head)))
            (inside (push line out))))))

(defun ygg-project-commands--toml-keys (lines)
  "The keys assigned in LINES, a TOML section body."
  (let (out)
    (dolist (line lines (nreverse out))
      (when (string-match "\\`[ \t]*[\"']?\\([[:alnum:]_.-]+\\)[\"']?[ \t]*=" line)
        (push (match-string 1 line) out)))))

(defun ygg-project-commands--cargo-workspaces (dir)
  "Members of the workspace DIR's Cargo.toml declares, globs and all."
  (when-let* ((path (ygg-project-commands--file dir "Cargo.toml"))
              (lines (ygg-project-commands--lines path))
              (body (string-join (ygg-project-commands--toml-section lines "workspace")
                                 "\n")))
    (let (patterns)
      (when (string-match "members[ \t]*=[ \t]*\\[\\(\\(?:.\\|\n\\)*?\\)\\]" body)
        (let ((listed (match-string 1 body)) (from 0))
          (while (string-match "[\"']\\([^\"']+\\)[\"']" listed from)
            (push (match-string 1 listed) patterns)
            (setq from (match-end 0)))))
      (ygg-project-commands--expand (nreverse patterns) dir))))

(defun ygg-project-commands--go-workspaces (dir)
  "Directories DIR's go.work uses, in the block form and the one-line form."
  (when-let* ((path (ygg-project-commands--file dir "go.work"))
              (lines (ygg-project-commands--lines path)))
    (let (patterns inside)
      (dolist (line lines)
        (let ((line (string-trim (replace-regexp-in-string "//.*\\'" "" line))))
          (cond ((string-match-p "\\`use[ \t]*(\\'" line) (setq inside t))
                ((and inside (string-prefix-p ")" line)) (setq inside nil))
                ((and inside (not (string-empty-p line)))
                 (push (ygg-project-commands--unquote line) patterns))
                ((string-match "\\`use[ \t]+\\(.+\\)\\'" line)
                 (push (ygg-project-commands--unquote (match-string 1 line))
                       patterns)))))
      (ygg-project-commands--expand (nreverse patterns) dir))))

(defun ygg-project-commands--mix-apps (root)
  "The apps of an umbrella, which is how Elixir spells a monorepo."
  (when-let* ((mix (ygg-project-commands--file root "mix.exs"))
              (dir (expand-file-name "apps" root))
              ((file-directory-p dir)))
    (seq-filter (lambda (d) (ygg-project-commands--file d "mix.exs"))
                (mapcar #'file-name-as-directory
                        (directory-files dir t "\\`[^.]")))))

(defun ygg-project-commands--members (root)
  "Every monorepo member ROOT declares, whichever manifest declares it."
  (seq-remove
   (lambda (dir) (equal dir root))
   (delete-dups (append (ygg-project-commands--npm-workspaces root)
                        (ygg-project-commands--pnpm-workspaces root)
                        (ygg-project-commands--cargo-workspaces root)
                        (ygg-project-commands--go-workspaces root)
                        (ygg-project-commands--mix-apps root)))))


;;; What one directory can run

(defun ygg-project-commands--entry (name dir root source command)
  "One runnable thing, named for a sidebar that shows ROOT as a whole."
  (list :name (if (equal dir root)
                  name
                (format "%s: %s" (file-name-nondirectory (directory-file-name dir))
                        name))
        :dir dir :command command :source source))

(defun ygg-project-commands--text (path)
  "PATH as a string, or nil."
  (when (file-readable-p path)
    (with-temp-buffer (insert-file-contents path) (buffer-string))))

(defun ygg-project-commands--mix-extra (text)
  "Tasks TEXT\='s dependencies bring with them."
  (let (out)
    (pcase-dolist (`(,dep . ,tasks) ygg-project-commands--mix-deps)
      (when (string-match-p (format ":%s\\_>" (regexp-quote dep)) text)
        (setq out (append out tasks))))
    out))

(defun ygg-project-commands--mix-aliases (text)
  "The aliases TEXT defines, which is where a project keeps its own verbs."
  (when (string-match "aliases[ \t]*do\\(\\(?:.\\|\n\\)*?\\)\n[ \t]*end" text)
    (let ((body (match-string 1 text))
          (start 0)
          out)
      (while (string-match "^[ \t]*\"?\\([a-z][A-Za-z0-9_.?!-]*\\)\"?:[ \t]*\\[" body start)
        (push (match-string 1 body) out)
        (setq start (match-end 0)))
      (nreverse out))))

(defun ygg-project-commands--justfile (dir)
  (ygg-project-commands--first dir "justfile" "Justfile"))

(defun ygg-project-commands--just-recipes (path)
  "Recipe names read off PATH, for when just itself cannot be asked."
  (let (names)
    (dolist (line (ygg-project-commands--lines path)
                  (nreverse (delete-dups names)))
      (when (string-match
             "\\`\\([[:alnum:]_-][[:alnum:]_.-]*\\)\\([ \t]+[^:=\n]*\\)?:\\([^=]\\|\\'\\)"
             line)
        (push (match-string 1 line) names)))))

(defun ygg-project-commands--make-targets (path)
  "Named targets in PATH, without the pattern rules, the variable
assignments and the dot-targets that are make talking to itself."
  (let (names)
    (dolist (line (ygg-project-commands--lines path)
                  (nreverse (delete-dups names)))
      (when (string-match "\\`\\([^\t#=:][^#=:]*\\):\\([^=]\\|\\'\\)" line)
        (dolist (token (split-string (match-string 1 line) "[ \t]+" t))
          (when (string-match-p "\\`[[:alnum:]][[:alnum:]_.+/-]*\\'" token)
            (push token names)))))))

(defun ygg-project-commands--js-runner (dir root)
  "The package manager DIR's scripts should go through.
A member of a monorepo has no lockfile of its own, so the root answers
for it."
  (cond ((file-exists-p (expand-file-name "pnpm-lock.yaml" dir)) 'pnpm)
        ((file-exists-p (expand-file-name "yarn.lock" dir)) 'yarn)
        ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) 'pnpm)
        ((file-exists-p (expand-file-name "yarn.lock" root)) 'yarn)
        (t 'npm)))

(defun ygg-project-commands--python (dir root)
  "Console scripts DIR's pyproject.toml installs, if it installs any."
  (when-let* ((path (ygg-project-commands--file dir "pyproject.toml"))
              (lines (ygg-project-commands--lines path)))
    (let* ((poetry (ygg-project-commands--toml-section lines "tool.poetry.scripts"))
           (section (or poetry
                        (ygg-project-commands--toml-section lines "project.scripts"))))
      (mapcar (lambda (name)
                (ygg-project-commands--entry
                 name dir root 'python
                 (if poetry (format "poetry run %s" name) name)))
              (ygg-project-commands--toml-keys section)))))

(defun ygg-project-commands--in-dir (dir root)
  "Everything DIR can be told to run, named as ROOT's sidebar wants it."
  (let (out)
    (when-let* ((justfile (ygg-project-commands--justfile dir)))
      (dolist (name (ygg-project-commands--just-recipes justfile))
        (push (ygg-project-commands--entry name dir root 'just
                                           (format "just %s" name))
              out)))
    (when-let* ((makefile (ygg-project-commands--first
                           dir "Makefile" "makefile" "GNUmakefile")))
      (dolist (name (ygg-project-commands--make-targets makefile))
        (push (ygg-project-commands--entry name dir root 'make
                                           (format "make %s" name))
              out)))
    (when-let* ((package (ygg-project-commands--file dir "package.json")))
      (let ((runner (ygg-project-commands--js-runner dir root))
            (scripts (alist-get 'scripts (ygg-project-commands--json package))))
        (dolist (pair scripts)
          (when (symbolp (car pair))
            (let ((name (symbol-name (car pair))))
              (push (ygg-project-commands--entry
                     name dir root runner (format "%s run %s" runner name))
                    out))))))
    (when (ygg-project-commands--file dir "Cargo.toml")
      (dolist (name ygg-project-commands--cargo)
        (push (ygg-project-commands--entry name dir root 'cargo
                                           (format "cargo %s" name))
              out)))
    (when (ygg-project-commands--file dir "go.mod")
      (pcase-dolist (`(,name . ,args) ygg-project-commands--go)
        (push (ygg-project-commands--entry name dir root 'go (format "go %s" args))
              out)))
    (when-let* ((mix (ygg-project-commands--file dir "mix.exs")))
      (let ((text (ygg-project-commands--text mix)))
        ;; an alias may shadow a task of the same name — it is still one
        ;; thing to run, and one row
        (dolist (name (delete-dups
                       (append ygg-project-commands--mix
                               (ygg-project-commands--mix-extra text)
                               (ygg-project-commands--mix-aliases text))))
          (push (ygg-project-commands--entry name dir root 'mix
                                             (format "mix %s" name))
                out))))
    (dolist (command (ygg-project-commands--python dir root))
      (push command out))
    (nreverse out)))


;;; Scanning, off the path that draws

(defun ygg-project-commands--order (commands dirs)
  "COMMANDS in a settled order: the root's first, then by runner and name."
  (let ((rank (make-hash-table :test #'equal))
        (place 0))
    (dolist (dir dirs) (puthash dir place rank) (setq place (1+ place)))
    (sort (copy-sequence commands)
          (lambda (a b)
            (let ((ra (gethash (plist-get a :dir) rank most-positive-fixnum))
                  (rb (gethash (plist-get b :dir) rank most-positive-fixnum))
                  (sa (symbol-name (or (plist-get a :source) 'zz)))
                  (sb (symbol-name (or (plist-get b :source) 'zz))))
              (cond ((/= ra rb) (< ra rb))
                    ((not (equal sa sb)) (string< sa sb))
                    (t (string< (plist-get a :name) (plist-get b :name)))))))))

(defun ygg-project-commands--finish (key entry)
  "Settle KEY on ENTRY and let go of everyone waiting on it."
  (when entry (puthash key entry ygg-project-commands--cache))
  (let ((waiting (gethash key ygg-project-commands--pending)))
    (remhash key ygg-project-commands--pending)
    (dolist (callback (nreverse waiting))
      (condition-case nil (funcall callback key) (error nil)))))

(defun ygg-project-commands--ask-just (key dirs members stamp commands)
  "Let just have the last word on the recipes read out of the justfiles.
One process per justfile, all answering into one list; the last
sentinel to land is the one that settles KEY."
  (let* ((asking (and (executable-find "just")
                      (seq-filter #'ygg-project-commands--justfile dirs)))
         (left (length asking))
         (found commands)
         (settle (lambda ()
                   (ygg-project-commands--finish
                    key (list :commands (ygg-project-commands--order found dirs)
                              :workspaces members :stamp stamp)))))
    (if (zerop left)
        (funcall settle)
      (dolist (each asking)
        ;; dolist reuses one binding for its variable, and these closures
        ;; outlive the loop
        (let* ((dir each)
               (buffer (generate-new-buffer " *ygg-just*"))
               (done (lambda ()
                       (when (buffer-live-p buffer) (kill-buffer buffer))
                       (setq left (1- left))
                       (when (<= left 0) (funcall settle)))))
          (condition-case nil
              (let ((default-directory dir))
                (make-process
                 :name "ygg-project-just" :buffer buffer :noquery t
                 :command '("just" "--summary")
                 :sentinel
                 (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (when (and (eq (process-status process) 'exit)
                                (zerop (process-exit-status process))
                                (buffer-live-p buffer))
                       (let ((names (split-string
                                     (with-current-buffer buffer (buffer-string))
                                     "[ \t\n]+" t)))
                         (when names
                           (setq found
                                 (append
                                  (seq-remove
                                   (lambda (command)
                                     (and (eq (plist-get command :source) 'just)
                                          (equal (plist-get command :dir) dir)))
                                   found)
                                  (mapcar
                                   (lambda (name)
                                     (ygg-project-commands--entry
                                      name dir key 'just (format "just %s" name)))
                                   names))))))
                     (funcall done)))))
            (error (funcall done))))))))

(defun ygg-project-commands--scan (key)
  "Look at KEY's manifests and settle its cache.  Runs off an idle timer."
  (condition-case nil
      (let* ((members (ygg-project-commands--members key))
             (dirs (cons key members))
             (stamp (ygg-project-commands--stamp dirs))
             (known (gethash key ygg-project-commands--cache)))
        (if (and known (equal stamp (plist-get known :stamp)))
            (ygg-project-commands--finish key known)
          (ygg-project-commands--ask-just
           key dirs members stamp
           (seq-mapcat (lambda (dir) (ygg-project-commands--in-dir dir key)) dirs))))
    (error (ygg-project-commands--finish key (gethash key ygg-project-commands--cache)))))


;;; What the sidebar calls

(defun ygg-project-commands (root)
  "What ROOT was last seen able to run, and nothing fresher.
The cache and only the cache, so this is safe on a drawing path;
ygg-project-commands-refresh is the half that goes and looks."
  (plist-get (gethash (ygg-project-commands--key root)
                      ygg-project-commands--cache)
             :commands))

(defun ygg-project-workspaces (root)
  "ROOT's monorepo members, as the last scan found them.  Cache only."
  (plist-get (gethash (ygg-project-commands--key root)
                      ygg-project-commands--cache)
             :workspaces))

(defun ygg-project-commands-refresh (root &optional callback)
  "Go and look at what ROOT can run, without holding anything up.
CALLBACK is called with ROOT once the scan settles, whether or not
anything had changed.  A scan already under way takes CALLBACK on
rather than starting a second one."
  (let* ((key (ygg-project-commands--key root))
         (waiting (gethash key ygg-project-commands--pending 'idle)))
    (if (listp waiting)
        (when callback
          (puthash key (cons callback waiting) ygg-project-commands--pending))
      (puthash key (and callback (list callback)) ygg-project-commands--pending)
      (run-with-idle-timer 0 nil #'ygg-project-commands--scan key))
    key))

(defun ygg-project-commands-run (command)
  "Run COMMAND, one plist out of ygg-project-commands, where it belongs."
  (let ((line (plist-get command :command))
        (dir (plist-get command :dir)))
    (unless (and (stringp line) (not (string-empty-p line)))
      (user-error "commands: nothing to run"))
    (let ((default-directory (or dir default-directory))
          (compilation-always-kill t)
          (compilation-buffer-name-function
           (lambda (_mode)
             (generate-new-buffer-name
              (format "*task:%s*" (or (plist-get command :name) line))))))
      (compile line t))))

(provide 'ygg-project-commands)
;;; ygg-project-commands.el ends here
