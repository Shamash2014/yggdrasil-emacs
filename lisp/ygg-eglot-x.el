;;; ygg-eglot-x.el --- rust-analyzer extensions, and debugging the runnable at point -*- lexical-binding: t; no-byte-compile: t -*-

;; Built-ins wrapped: eglot, jsonrpc, call-process, json-parse-string.
;; Packages wrapped: eglot-x (nemethf), dape's lldb-dap entry.
;; Custom: a rust-test dape entry that picks rust-analyzer's runnable at
;; point, builds it in dape's compile step, and launches the executable
;; cargo reports.

;;; Code:

(require 'yggdrasil-localleader)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'map)

(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot-range-region "eglot")
(declare-function eglot--apply-text-edits "eglot")
(declare-function eglot-x-setup "eglot-x")
(declare-function jsonrpc-request "jsonrpc")
(declare-function dape--config-eval "dape")

(defvar eglot-x-enable-files)
(defvar eglot-x-enable-snippet-text-edit)
(defvar eglot-x-enable-colored-diagnostics)
(defvar eglot-x-enable-hover-actions)
(defvar eglot-x-enable-ff-related-file-integration)
(defvar eglot-x-enable-open-server-logs)
(defvar eglot-x-client-commands)
(defvar eglot-x-enable-encoding-negotiation)
(defvar eglot-x-enable-server-status)
(defvar eglot-x-enable-local-docs-support)
(defvar eglot-x-enable-menu)
(defvar dape-configs)

(defvar ygg-eglot-x-lldb-config 'lldb-dap
  "The dape-configs entry a Rust runnable is launched through.")

(defun ygg-eglot-x-configure ()
  "Choose which eglot-x extensions are on, before any server connects."
  (setq eglot-x-enable-files nil
        eglot-x-enable-snippet-text-edit nil
        eglot-x-enable-colored-diagnostics nil
        eglot-x-enable-hover-actions nil
        eglot-x-enable-ff-related-file-integration nil
        eglot-x-enable-open-server-logs nil
        eglot-x-client-commands nil
        eglot-x-enable-encoding-negotiation t
        eglot-x-enable-server-status t
        eglot-x-enable-local-docs-support t
        eglot-x-enable-menu t))

(defconst ygg-eglot-x--final-stop (string #xE000)
  "Stands in for a snippet's final stop while its edit is applied as text.")

(defun ygg-eglot-x--plain-edit (edit)
  "EDIT as a plain TextEdit, its first final stop replaced by the stand-in.
rust-analyzer does not escape the text around the stop it inserts."
  (let ((text (plist-get edit :newText)))
    (list :range (plist-get edit :range)
          :newText (if (eql (plist-get edit :insertTextFormat) 2)
                       (let ((stop (string-search "$0" text)))
                         (if stop
                             (concat (substring text 0 stop) ygg-eglot-x--final-stop
                                     (substring text (+ stop 2)))
                           text))
                     text))))

(defun ygg-eglot-x--apply-as-text (edits &optional version silent)
  "Apply snippet EDITS at VERSION as literal text, point on the final stop.
A snippet engine would parse the unescaped dollars of moved code such as
macro_rules as stops.  SILENT is passed to eglot."
  (atomic-change-group
    (eglot--apply-text-edits (vconcat (mapcar #'ygg-eglot-x--plain-edit edits))
                             version silent)
    (goto-char (point-min))
    (when (search-forward ygg-eglot-x--final-stop nil t)
      (delete-char -1))))

(defun ygg-eglot-x-install ()
  "Load eglot-x with the chosen extensions and turn it on."
  (require 'eglot-x)
  (ygg-eglot-x-configure)
  (eglot-x-setup)
  (advice-add 'eglot-x--apply-text-edits :override #'ygg-eglot-x--apply-as-text))

(when (fboundp 'elpaca)
  (elpaca (eglot-x :host github :repo "nemethf/eglot-x")
    (with-eval-after-load 'eglot
      (ygg-eglot-x-install))))

(defun ygg-eglot-x--runnable-cargo-args (runnable)
  (append (plist-get (plist-get runnable :args) :cargoArgs) nil))

(defun ygg-eglot-x--debuggable-p (runnable)
  "Whether RUNNABLE builds an executable lldb can launch."
  (let ((cargo-args (ygg-eglot-x--runnable-cargo-args runnable)))
    (and (equal (plist-get runnable :kind) "cargo")
         (plist-get runnable :location)
         (member (car cargo-args) '("test" "run"))
         (not (member "--doc" cargo-args)))))

(defun ygg-eglot-x--runnable-extent (runnable)
  "Buffer region RUNNABLE's target covers, as (BEG . END)."
  (eglot-range-region
   (plist-get (plist-get runnable :location) :targetRange)))

(defun ygg-eglot-x--innermost (runnables point extent-fn)
  "The debuggable one of RUNNABLES whose extent holds POINT most tightly.
EXTENT-FN maps a runnable to its (BEG . END)."
  (let ((holding (seq-filter
                  (lambda (runnable)
                    (pcase-let ((`(,beg . ,end) (funcall extent-fn runnable)))
                      (<= beg point end)))
                  (seq-filter #'ygg-eglot-x--debuggable-p runnables))))
    (car (seq-sort-by (lambda (runnable)
                        (pcase-let ((`(,beg . ,end) (funcall extent-fn runnable)))
                          (- end beg)))
                      #'< holding))))

(defun ygg-eglot-x--runnable-at-point ()
  (let ((runnables (append (jsonrpc-request (eglot--current-server-or-lose)
                                            :experimental/runnables
                                            (eglot--TextDocumentPositionParams))
                           nil)))
    (or (ygg-eglot-x--innermost runnables (point) #'ygg-eglot-x--runnable-extent)
        (user-error "No test or binary at point"))))

(defun ygg-eglot-x--build-args (runnable)
  "Cargo arguments that build RUNNABLE's executable without running it."
  (let ((extra (append (plist-get (plist-get runnable :args) :cargoExtraArgs) nil)))
    (pcase (ygg-eglot-x--runnable-cargo-args runnable)
      (`("test" . ,rest) `("test" ,@rest ,@extra "--no-run"))
      (`("run" . ,rest) `("build" ,@rest ,@extra)))))

(defun ygg-eglot-x--cargo (runnable)
  (or (plist-get (plist-get runnable :args) :overrideCargo) "cargo"))

(defun ygg-eglot-x--test-p (runnable)
  (equal (car (ygg-eglot-x--runnable-cargo-args runnable)) "test"))

(defun ygg-eglot-x--program-args (runnable)
  "Arguments the executable of RUNNABLE is launched with."
  (let ((args (append (plist-get (plist-get runnable :args) :executableArgs) nil)))
    (if (and (ygg-eglot-x--test-p runnable) (not (member "--nocapture" args)))
        (append args '("--nocapture"))
      args)))

(defun ygg-eglot-x--executable (cargo-output testp)
  "The executable the last matching compiler-artifact in CARGO-OUTPUT names.
TESTP picks the test harness, else a binary target."
  (let (found)
    (dolist (line (split-string cargo-output "\n" t))
      (when (string-prefix-p "{" line)
        (let ((message (json-parse-string line :object-type 'plist
                                          :null-object nil :false-object nil)))
          (when (and (equal (plist-get message :reason) "compiler-artifact")
                     (plist-get message :executable)
                     (if testp
                         (plist-get (plist-get message :profile) :test)
                       (seq-intersection '("bin" "example")
                                         (plist-get (plist-get message :target) :kind))))
            (setq found (plist-get message :executable))))))
    found))

(defun ygg-eglot-x--directory (runnable)
  (let ((args (plist-get runnable :args)))
    (file-name-as-directory
     (or (plist-get args :cwd) (plist-get args :workspaceRoot) default-directory))))

(defun ygg-eglot-x--environment (runnable)
  "RUNNABLE's environment as NAME=VALUE strings."
  (let ((env (plist-get (plist-get runnable :args) :environment)) pairs)
    (while env
      (push (format "%s=%s" (substring (symbol-name (car env)) 1) (cadr env)) pairs)
      (setq env (cddr env)))
    (nreverse pairs)))

(defun ygg-eglot-x--rust-formatter-commands (directory)
  "lldb commands loading rustc's pretty printers for the toolchain in DIRECTORY."
  (when-let* ((default-directory directory)
              (sysroot (ignore-errors (car (process-lines "rustc" "--print" "sysroot"))))
              (etc (expand-file-name "lib/rustlib/etc/" sysroot))
              ((file-exists-p (expand-file-name "lldb_lookup.py" etc))))
    (vector (format "command script import \"%slldb_lookup.py\"" etc)
            (format "command source -s 1 \"%slldb_commands\"" etc))))

(defun ygg-eglot-x--compile-command (runnable)
  "Shell command that builds RUNNABLE in dape's compile step."
  (mapconcat #'shell-quote-argument
             (append (list "env")
                     (ygg-eglot-x--environment runnable)
                     (list (ygg-eglot-x--cargo runnable))
                     (ygg-eglot-x--build-args runnable))
             " "))

(defun ygg-eglot-x--built-executable (runnable)
  "The executable cargo reports for RUNNABLE once its build is fresh."
  (let ((default-directory (ygg-eglot-x--directory runnable))
        (process-environment (append (ygg-eglot-x--environment runnable)
                                     process-environment)))
    (with-temp-buffer
      (apply #'call-process (ygg-eglot-x--cargo runnable) nil '(t nil) nil
             (append (ygg-eglot-x--build-args runnable) '("--message-format=json")))
      (or (ygg-eglot-x--executable (buffer-string) (ygg-eglot-x--test-p runnable))
          (user-error "cargo built no executable for %s" (plist-get runnable :label))))))

(defvar ygg-eglot-x--compiling nil
  "Build tokens of rust-test configs waiting on dape's compile step.
Dape runs fn again after compiling; a config holding one of these comes
back from its build, any other is a fresh start or a restart.")

(defvar ygg-eglot-x--build-count 0)

(defun ygg-eglot-x--prepare (config)
  "CONFIG built on the lldb-dap entry for the runnable at point, compiling first."
  (let* ((runnable (or (plist-get config 'ygg-eglot-x-runnable)
                       (ygg-eglot-x--runnable-at-point)))
         (directory (ygg-eglot-x--directory runnable))
         (token (cl-incf ygg-eglot-x--build-count))
         (base (let ((default-directory directory))
                 (dape--config-eval
                  ygg-eglot-x-lldb-config
                  `(command-cwd ,directory
                    :cwd ,directory
                    :program ""
                    :args ,(vconcat (ygg-eglot-x--program-args runnable))
                    :env ,(vconcat (ygg-eglot-x--environment runnable))
                    ,@(when-let* ((commands (ygg-eglot-x--rust-formatter-commands directory)))
                        `(:initCommands ,commands)))))))
    (push token ygg-eglot-x--compiling)
    (map-merge 'plist base
               `(modes ,(plist-get config 'modes)
                 fn ,(plist-get config 'fn)
                 compile ,(ygg-eglot-x--compile-command runnable)
                 ygg-eglot-x-runnable ,runnable
                 ygg-eglot-x-build ,token))))

(defun ygg-eglot-x-rust-test-config (config)
  "Dape fn for rust-test: build the test or binary at point, then launch it."
  (let ((token (plist-get config 'ygg-eglot-x-build)))
    (if (memql token ygg-eglot-x--compiling)
        (progn
          (setq ygg-eglot-x--compiling (delq token ygg-eglot-x--compiling))
          (plist-put config :program
                     (ygg-eglot-x--built-executable (plist-get config 'ygg-eglot-x-runnable))))
      (ygg-eglot-x--prepare config))))

(with-eval-after-load 'dape
  (setf (alist-get 'rust-test dape-configs)
        '(modes (rust-mode rust-ts-mode) fn ygg-eglot-x-rust-test-config)))

(autoload 'eglot-x-ask-runnables "eglot-x" nil t)
(autoload 'eglot-x-ask-related-tests "eglot-x" nil t)
(autoload 'eglot-x-expand-macro "eglot-x" nil t)
(autoload 'eglot-x-structural-search-replace "eglot-x" nil t)
(autoload 'eglot-x-join-lines "eglot-x" nil t)
(autoload 'eglot-x-move-item-down "eglot-x" nil t)
(autoload 'eglot-x-move-item-up "eglot-x" nil t)
(autoload 'eglot-x-open-external-documentation "eglot-x" nil t)
(autoload 'eglot-x-reload-workspace "eglot-x" nil t)
(autoload 'eglot-x-show-server-status "eglot-x" nil t)
(autoload 'eglot-x-view-crate-graph "eglot-x" nil t)

(dolist (mode '(rust-ts-mode rust-mode))
  (yggdrasil-localleader-def mode "m r" #'eglot-x-ask-runnables "runnables")
  (yggdrasil-localleader-def mode "m t" #'eglot-x-ask-related-tests "related tests")
  (yggdrasil-localleader-def mode "m e" #'eglot-x-expand-macro "expand macro")
  (yggdrasil-localleader-def mode "m s" #'eglot-x-structural-search-replace "structural replace")
  (yggdrasil-localleader-def mode "m J" #'eglot-x-join-lines "join lines")
  (yggdrasil-localleader-def mode "m j" #'eglot-x-move-item-down "move item down")
  (yggdrasil-localleader-def mode "m k" #'eglot-x-move-item-up "move item up")
  (yggdrasil-localleader-def mode "m d" #'eglot-x-open-external-documentation "external docs")
  (yggdrasil-localleader-def mode "m w" #'eglot-x-reload-workspace "reload workspace")
  (yggdrasil-localleader-def mode "m S" #'eglot-x-show-server-status "server status")
  (when (executable-find "dot")
    (yggdrasil-localleader-def mode "m g" #'eglot-x-view-crate-graph "crate graph")))

(provide 'ygg-eglot-x)
;;; ygg-eglot-x.el ends here
