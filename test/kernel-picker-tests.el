;;; kernel-picker-tests.el --- Tests for the jupyter session picker -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-kernel-picker)

(defconst kernel-picker-test-specs
  '(("python3" . (:argv ["/opt/py/bin/python" "-m" "ipykernel_launcher" "-f" "{connection_file}"]
                  :display_name "Python 3 (uv)" :language "python"))
    ("ark" . (:argv ["/opt/ark" "--connection_file" "{connection_file}"]
              :display_name "Ark R Kernel" :language "R"
              :env (:PATH "/opt/r/bin")))))

(defun kernel-picker-test-versions (language _interpreter)
  (pcase language ("python" "3.13.15") ("r" "4.6.1")))

(defun kernel-picker-test-facts (client language version spec buffers &optional state)
  (list :client client :language language :version version
        :spec-plist (cdr (assoc spec kernel-picker-test-specs))
        :state (or state "idle") :buffers buffers))

(defmacro kernel-picker-test-with-versions (&rest body)
  (declare (indent 0))
  `(let ((ygg-kernel-picker-version-function #'kernel-picker-test-versions))
     ,@body))

(defun kernel-picker-test-rows (&optional current language)
  (kernel-picker-test-with-versions
    (ygg-kernel-picker--rows
     (list (kernel-picker-test-facts 'r-client 'R "4.6.1" "ark" 2)
           (kernel-picker-test-facts 'py-client 'python "3.13.15" "python3" 1 "busy"))
     kernel-picker-test-specs nil current language)))

(defclass kernel-picker-test-client ()
  ((buffer :initarg :buffer :initform nil)))

(defun kernel-picker-test-venv (dir &optional uv ipykernel)
  (make-directory (expand-file-name "bin" dir) t)
  (with-temp-file (expand-file-name "bin/python" dir) (insert "#!/bin/sh\n"))
  (set-file-modes (expand-file-name "bin/python" dir) #o755)
  (with-temp-file (expand-file-name "pyvenv.cfg" dir)
    (insert "home = /usr/bin\n" (if uv "uv = 0.8.11\n" "") "version_info = 3.12.4\n"))
  (when ipykernel
    (make-directory (expand-file-name "lib/python3.12/site-packages/ipykernel" dir) t))
  (file-name-as-directory dir))

(ert-deftest kernel-picker-names-a-kernelspec-by-language-version-and-flavour ()
  (kernel-picker-test-with-versions
    (let ((python (ygg-kernel-picker--spec-row "python3" (cdr (assoc "python3" kernel-picker-test-specs))))
          (ark (ygg-kernel-picker--spec-row "ark" (cdr (assoc "ark" kernel-picker-test-specs)))))
      (should (equal (plist-get python :name) "Python 3.13.15 (uv)"))
      (should (equal (plist-get ark :name) "R 4.6.1"))
      (should (equal (plist-get ark :language) "r"))
      (should (eq (plist-get ark :kind) 'spec)))))

(ert-deftest kernel-picker-falls-back-to-the-display-name-without-a-version ()
  (let ((ygg-kernel-picker-version-function #'ignore))
    (should (equal (plist-get (ygg-kernel-picker--spec-row
                               "ark" (cdr (assoc "ark" kernel-picker-test-specs)))
                              :name)
                   "Ark R Kernel"))))

(ert-deftest kernel-picker-finds-r-on-the-path-the-kernelspec-sets ()
  (let* ((dir (make-temp-file "kp-r" t))
         (r (expand-file-name "R" dir)))
    (unwind-protect
        (progn
          (with-temp-file r (insert "#!/bin/sh\n"))
          (set-file-modes r #o755)
          (should (equal (ygg-kernel-picker--interpreter
                          "r" (list :argv ["/opt/ark"] :env (list :PATH (concat dir ":${PATH}"))))
                         r)))
      (delete-directory dir t))))

(ert-deftest kernel-picker-puts-live-sessions-before-new-ones ()
  (let ((rows (kernel-picker-test-rows)))
    (should (equal (mapcar (lambda (row) (plist-get row :section)) rows)
                   '("Active sessions" "Active sessions" "New session" "New session")))
    (should (equal (mapcar (lambda (row) (plist-get row :name)) rows)
                   '("R 4.6.1" "Python 3.13.15 (uv)" "Python 3.13.15 (uv)" "R 4.6.1")))))

(ert-deftest kernel-picker-marks-only-the-buffers-own-session-current ()
  (let ((rows (kernel-picker-test-rows 'py-client)))
    (should (equal (mapcar (lambda (row) (and (plist-get row :current) t)) rows)
                   '(nil t nil nil)))))

(ert-deftest kernel-picker-keeps-only-the-buffers-language ()
  (let ((rows (kernel-picker-test-rows nil "r")))
    (should (equal (mapcar (lambda (row) (list (plist-get row :kind) (plist-get row :name))) rows)
                   '((active "R 4.6.1") (spec "R 4.6.1"))))))

(ert-deftest kernel-picker-annotates-a-live-session-with-place-state-and-buffers ()
  (let* ((row (car (kernel-picker-test-rows 'r-client)))
         (text (substring-no-properties (ygg-kernel-picker--annotation row))))
    (should (equal text "/opt/ark  idle  2 buffers  current"))
    (should (eq (get-text-property 0 'face (ygg-kernel-picker--annotation row)) 'shadow))))

(ert-deftest kernel-picker-annotates-one-buffer-in-the-singular ()
  (let ((row (nth 1 (kernel-picker-test-rows))))
    (should (equal (substring-no-properties (ygg-kernel-picker--annotation row))
                   "/opt/py/bin/python  busy  1 buffer"))))

(ert-deftest kernel-picker-annotates-a-new-kernelspec-with-its-interpreter-only ()
  (let ((row (nth 2 (kernel-picker-test-rows))))
    (should (equal (substring-no-properties (ygg-kernel-picker--annotation row))
                   "/opt/py/bin/python"))))

(ert-deftest kernel-picker-makes-colliding-names-distinct-but-alike ()
  (let* ((candidates (ygg-kernel-picker--candidates (kernel-picker-test-rows)))
         (strings (mapcar #'car candidates)))
    (should (= (length (delete-dups (copy-sequence strings))) 4))
    (should (string-prefix-p "Python 3.13.15 (uv)" (nth 2 strings)))
    (should (get-text-property (1- (length (nth 2 strings))) 'invisible (nth 2 strings)))
    (should (eq (plist-get (cdr (assoc (substring-no-properties (nth 2 strings)) candidates)) :kind)
                'spec))))

(ert-deftest kernel-picker-table-groups-and-keeps-order ()
  (let* ((candidates (ygg-kernel-picker--candidates (kernel-picker-test-rows 'r-client)))
         (table (ygg-kernel-picker--table candidates))
         (metadata (cdr (funcall table "" nil 'metadata)))
         (group (alist-get 'group-function metadata))
         (affix (alist-get 'affixation-function metadata)))
    (should (eq (alist-get 'category metadata) 'ygg-kernel))
    (should (eq (alist-get 'display-sort-function metadata) 'identity))
    (should (equal (mapcar (lambda (c) (funcall group (car c) nil)) candidates)
                   '("Active sessions" "Active sessions" "New session" "New session")))
    (should (eq (get-text-property 0 'face (funcall group (caar candidates) nil))
                'ygg-kernel-picker-group))
    (should (equal (funcall group (caar candidates) t) (caar candidates)))
    (pcase-let ((`((,string ,prefix ,suffix)) (funcall affix (list (caar candidates)))))
      (should (equal string "R 4.6.1"))
      (should (eq (get-text-property 0 'face prefix) 'shadow))
      (should (string-suffix-p "current" (substring-no-properties suffix))))
    (should (equal (all-completions "" table) (mapcar #'car candidates)))))

(ert-deftest kernel-picker-offers-a-uv-venv-and-flags-a-missing-ipykernel ()
  (let* ((root (make-temp-file "kp-proj" t))
         (ready (kernel-picker-test-venv (expand-file-name ".venv" root) t t))
         (bare (kernel-picker-test-venv (expand-file-name "bare" root))))
    (unwind-protect
        (let ((uv (ygg-kernel-picker--venv-row ready))
              (plain (ygg-kernel-picker--venv-row bare)))
          (should (equal (plist-get uv :name) "Python 3.12.4 (uv)"))
          (should-not (plist-get uv :missing))
          (should (equal (plist-get plain :name) "Python 3.12.4 (venv)"))
          (should (equal (plist-get plain :missing) "no ipykernel"))
          (should (string-suffix-p "no ipykernel"
                                   (substring-no-properties (ygg-kernel-picker--annotation plain)))))
      (delete-directory root t))))

(ert-deftest kernel-picker-drops-a-venv-a-kernelspec-already-runs ()
  (let* ((root (make-temp-file "kp-proj" t))
         (venv (kernel-picker-test-venv (expand-file-name ".venv" root) t t))
         (spec (list (cons "local" (list :argv (vector (expand-file-name "bin/python" venv))
                                         :display_name "Local" :language "python")))))
    (unwind-protect
        (kernel-picker-test-with-versions
          (should (equal (mapcar (lambda (row) (plist-get row :kind))
                                 (ygg-kernel-picker--rows nil spec (list venv) nil))
                         '(spec)))
          (should (equal (mapcar (lambda (row) (plist-get row :kind))
                                 (ygg-kernel-picker--rows nil nil (list venv) nil))
                         '(venv))))
      (delete-directory root t))))

(ert-deftest kernel-picker-finds-the-project-venv ()
  (let* ((root (file-name-as-directory (make-temp-file "kp-proj" t)))
         (venv (kernel-picker-test-venv (expand-file-name ".venv" root) t t)))
    (unwind-protect
        (let ((default-directory root))
          (cl-letf (((symbol-function 'project-current) #'ignore))
            (should (equal (ygg-kernel-picker--project-venvs) (list venv)))))
      (delete-directory root t))))

(ert-deftest kernel-picker-reads-a-venv-python-version-without-running-it ()
  (let* ((root (make-temp-file "kp-proj" t))
         (venv (kernel-picker-test-venv (expand-file-name ".venv" root) t))
         (ygg-kernel-picker--versions (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-lines) (lambda (&rest _) (error "Ran a process"))))
          (should (equal (ygg-kernel-picker--probe-version
                          "python" (expand-file-name "bin/python" venv))
                         "3.12.4")))
      (delete-directory root t))))

(ert-deftest kernel-picker-gives-a-launcher-venv-no-version-for-another-language ()
  (let* ((root (make-temp-file "kp-proj" t))
         (venv (kernel-picker-test-venv (expand-file-name ".venv" root) t))
         (ygg-kernel-picker--versions (make-hash-table :test #'equal)))
    (unwind-protect
        (let ((python (expand-file-name "bin/python" venv)))
          (should-not (ygg-kernel-picker--probe-version "kotlin" python))
          (should (equal (ygg-kernel-picker--probe-version "python" python) "3.12.4"))
          (should-not (ygg-kernel-picker--probe-version "kotlin" python)))
      (delete-directory root t))))

(ert-deftest kernel-picker-shows-the-venv-not-its-python-as-the-place ()
  (let* ((root (make-temp-file "kp-proj" t))
         (venv (kernel-picker-test-venv (expand-file-name ".venv" root) t)))
    (unwind-protect
        (should (equal (ygg-kernel-picker--where (expand-file-name "bin/python" venv))
                       (abbreviate-file-name (directory-file-name venv))))
      (delete-directory root t))))

(ert-deftest kernel-picker-reads-flavour-from-the-display-name-tail ()
  (should (equal (ygg-kernel-picker--flavour "Python 3 (uv)") "uv"))
  (should-not (ygg-kernel-picker--flavour "Ark R Kernel")))

(ert-deftest kernel-picker-counts-buffers-that-evaluate-in-a-client ()
  (let ((client (make-symbol "client"))
        (a (generate-new-buffer "kp-a"))
        (b (generate-new-buffer "kp-b"))
        (c (generate-new-buffer "kp-c")))
    (unwind-protect
        (progn
          (defvar ygg-nb--kernels)
          (with-current-buffer a (setq-local jupyter-current-client client))
          (with-current-buffer b (setq-local ygg-nb--kernels (list (cons "r" client))))
          (with-current-buffer c (setq-local jupyter-current-client 'other))
          (should (= (ygg-kernel-picker--buffer-count client) 2)))
      (mapc #'kill-buffer (list a b c)))))

(ert-deftest kernel-picker-puts-the-configured-kernel-first-and-defaults-to-it ()
  (let* ((specs (append kernel-picker-test-specs
                        '(("ark-console" . (:argv ["/opt/ark"] :display_name "Ark R Kernel (console)"
                                            :language "R")))))
         (rows (kernel-picker-test-with-versions
                 (ygg-kernel-picker--rows nil specs nil nil "r" '("python3" "ark-console")))))
    (should (equal (mapcar (lambda (row) (plist-get row :name)) rows)
                   '("R 4.6.1 (console)" "R 4.6.1")))
    (should (equal (mapcar (lambda (row) (and (plist-get row :default) t)) rows)
                   '(t nil)))))

(ert-deftest kernel-picker-reads-the-number-out-of-a-sentence-version ()
  (should (equal (ygg-kernel-picker--display-name "r" "R version 4.6.1 (2026-06-24)" nil nil)
                 "R 4.6.1"))
  (should (equal (ygg-kernel-picker--display-name "python" "3.13.15" "uv" nil)
                 "Python 3.13.15 (uv)")))

(ert-deftest kernel-picker-keeps-a-venv-whose-python-links-to-a-spec-interpreter ()
  (let* ((root (make-temp-file "kp-proj" t))
         (base (expand-file-name "base-python" root))
         (venv (file-name-as-directory (expand-file-name ".venv" root))))
    (unwind-protect
        (progn
          (with-temp-file base (insert "#!/bin/sh\n"))
          (set-file-modes base #o755)
          (make-directory (expand-file-name "bin" venv) t)
          (make-symbolic-link base (expand-file-name "bin/python" venv))
          (with-temp-file (expand-file-name "pyvenv.cfg" venv) (insert "uv = 0.8\nversion_info = 3.12.4\n"))
          (kernel-picker-test-with-versions
            (should (equal (mapcar (lambda (row) (plist-get row :kind))
                                   (ygg-kernel-picker--rows
                                    nil (list (cons "py" (list :argv (vector base) :language "python")))
                                    (list venv) nil))
                           '(spec venv)))))
      (delete-directory root t))))

(ert-deftest kernel-picker-names-a-remote-kernelspec-without-probing-the-host ()
  (let* ((ygg-kernel-picker-version-function (lambda (&rest _) (error "Probed a remote path")))
         (row (ygg-kernel-picker--spec-row
               "python3" '(:argv ["python" "-m" "ipykernel_launcher" "-f" "{connection_file}"]
                           :display_name "Python 3 (ipykernel)" :language "python")
               "jtest"))
         (annotation (ygg-kernel-picker--annotation row)))
    (should (equal (plist-get row :name) "Python 3 (ipykernel)"))
    (should (equal (plist-get row :section) "New session"))
    (should (equal (plist-get row :path) "python"))
    (should (equal (substring-no-properties annotation) "jtest  python"))
    (should (eq (get-text-property 0 'face annotation) 'shadow))))

(ert-deftest kernel-picker-reads-live-kernels-off-the-host-probe-newest-first ()
  (let ((kernels (ygg-kernel-picker--parse-running
                  "[{\"file\": \"/run/kernel-a.json\", \"kernel_name\": \"python3\", \"mtime\": 100.0},
                    {\"file\": \"/run/kernel-b.json\", \"kernel_name\": \"\", \"mtime\": 200.5}]"
                  "/ssh:jtest:")))
    (should (equal (mapcar (lambda (k) (plist-get k :file)) kernels)
                   '("/ssh:jtest:/run/kernel-b.json" "/ssh:jtest:/run/kernel-a.json")))
    (should (equal (plist-get (cadr kernels) :kernel-name) "python3"))
    (should (= (float-time (plist-get (car kernels) :time)) 200.5))))

(ert-deftest kernel-picker-names-a-running-kernel-by-its-kernelspec-under-its-host ()
  (let* ((specs '(("python3" . (:display_name "Python 3 (ipykernel)" :language "python"))))
         (now (current-time))
         (known (ygg-kernel-picker--running-row
                 (list :file "/ssh:jtest:/run/kernel-a.json" :kernel-name "python3"
                       :time (time-subtract now 600))
                 specs "jtest"))
         (unknown (ygg-kernel-picker--running-row
                   (list :file "/ssh:jtest:/run/kernel-b.json" :kernel-name "") specs "jtest")))
    (should (equal (plist-get known :section) "Running on jtest"))
    (should (equal (plist-get known :name) "Python 3 (ipykernel)"))
    (should (equal (plist-get known :language) "python"))
    (should (equal (plist-get known :file) "/ssh:jtest:/run/kernel-a.json"))
    (should (equal (substring-no-properties (ygg-kernel-picker--annotation known)) "10 min ago"))
    (should (eq (get-text-property 0 'face (ygg-kernel-picker--annotation known)) 'shadow))
    (should (equal (plist-get unknown :name) "kernel"))
    (should-not (plist-get unknown :language))))

(ert-deftest kernel-picker-puts-running-kernels-between-live-and-new ()
  (let* ((running (list (ygg-kernel-picker--running-row
                         '(:file "/ssh:jtest:/run/kernel-a.json" :kernel-name "python3")
                         kernel-picker-test-specs "jtest")
                        (ygg-kernel-picker--running-row
                         '(:file "/ssh:jtest:/run/kernel-b.json" :kernel-name "julia")
                         kernel-picker-test-specs "jtest")))
         (rows (kernel-picker-test-with-versions
                 (ygg-kernel-picker--rows
                  (list (kernel-picker-test-facts 'py 'python "3.12.4" "python3" 1))
                  kernel-picker-test-specs nil nil "python" nil running "jtest"))))
    (should (equal (mapcar (lambda (row) (plist-get row :section)) rows)
                   '("Active sessions" "Running on jtest" "Running on jtest" "New session")))
    (should (equal (plist-get (nth 3 rows) :host) "jtest"))))

(ert-deftest kernel-picker-leaves-out-kernels-this-emacs-already-joined ()
  "By connection file, or by the file name a server gives a kernel it runs."
  (let* ((ygg-kernel-picker--joined (make-hash-table :test #'equal))
         (repl (generate-new-buffer " kp-repl"))
         (client (kernel-picker-test-client :buffer repl)))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-kernel-picker--host-kernels)
                   (lambda () '((:file "/ssh:jtest:/run/kernel-a.json" :kernel-name "python3")
                                (:file "/ssh:jtest:/run/kernel-b.json" :kernel-name "python3")))))
          (puthash "/ssh:jtest:/run/kernel-a.json" client ygg-kernel-picker--joined)
          (should (equal (mapcar (lambda (row) (plist-get row :file))
                                 (ygg-kernel-picker--running-rows kernel-picker-test-specs "jtest"))
                         '("/ssh:jtest:/run/kernel-b.json")))
          (puthash "kernel-b.json" client ygg-kernel-picker--joined)
          (should-not (ygg-kernel-picker--running-rows kernel-picker-test-specs "jtest")))
      (kill-buffer repl))))

(ert-deftest kernel-picker-finds-a-kernel-remote-by-its-repl-directory ()
  (let ((remote (generate-new-buffer " kp-remote"))
        (local (generate-new-buffer " kp-local")))
    (unwind-protect
        (progn
          (with-current-buffer remote (setq default-directory "/ssh:jtest:/root/"))
          (with-current-buffer local (setq default-directory "/tmp/"))
          (should (equal (ygg-kernel-picker-remote (kernel-picker-test-client :buffer remote))
                         "/ssh:jtest:"))
          (should-not (ygg-kernel-picker-remote (kernel-picker-test-client :buffer local)))
          (let ((headless (kernel-picker-test-client)))
            (ygg-kernel-picker-note-host headless "/ssh:jtest:")
            (should (equal (ygg-kernel-picker-remote headless) "/ssh:jtest:"))))
      (kill-buffer remote)
      (kill-buffer local))))

(ert-deftest kernel-picker-splits-a-pasted-server-url-into-base-and-token ()
  (should (equal (ygg-kernel-picker--split-url "http://127.0.0.1:8888/lab?token=abc123")
                 '("http://127.0.0.1:8888" . "abc123")))
  (should (equal (ygg-kernel-picker--split-url " https://hub.example/user/me/tree/notes ")
                 '("https://hub.example/user/me" . nil)))
  (should (equal (ygg-kernel-picker--split-url "http://gpu:8888/")
                 '("http://gpu:8888" . nil)))
  (should (equal (ygg-kernel-picker--server-label "http://gpu:8888") "gpu:8888")))

(ert-deftest kernel-picker-says-how-long-ago-in-words ()
  (let ((now (current-time)))
    (should (equal (ygg-kernel-picker--age (time-subtract now 5) now) "just now"))
    (should (equal (ygg-kernel-picker--age (time-subtract now 60) now) "1 min ago"))
    (should (equal (ygg-kernel-picker--age (time-subtract now 7300) now) "2 hours ago"))
    (should (equal (ygg-kernel-picker--age (time-subtract now 86400) now) "1 day ago"))))

(ert-deftest kernel-picker-keeps-a-capitalised-kernel-language ()
  (let* ((specs (append kernel-picker-test-specs
                        '(("rust" . (:argv ["/opt/evcxr_jupyter" "--control_file" "{connection_file}"]
                                     :display_name "Rust" :language "rust")))))
         (rows (kernel-picker-test-with-versions
                 (ygg-kernel-picker--rows
                  (list (list :client 'rust-client :language 'Rust :version "1.90.0" :buffers 0))
                  specs nil nil "rust" '("rust")))))
    (should (equal (mapcar (lambda (row) (list (plist-get row :kind) (plist-get row :name))) rows)
                   '((active "Rust 1.90.0") (spec "Rust"))))))

(defclass kernel-picker-test-server () ((auth :initform nil)))

(ert-deftest kernel-picker-server-history-never-keeps-the-token ()
  (let ((ygg-kernel-picker--server-history nil)
        (ygg-kernel-picker--server-tokens (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt _initial history &rest _)
                 (let ((url "http://127.0.0.1:8888/lab?token=abc123"))
                   (add-to-history history url)
                   url))))
      (let ((url (ygg-kernel-picker--read-server-url)))
        (should-not (string-search "abc123" url))
        (should (equal ygg-kernel-picker--server-history '("http://127.0.0.1:8888/lab")))
        (should (equal (gethash "http://127.0.0.1:8888" ygg-kernel-picker--server-tokens)
                       "abc123"))))))

(ert-deftest kernel-picker-server-history-is-scrubbed-before-saving ()
  (let ((ygg-kernel-picker--server-history
         '("http://h:8888/?token=old" "https://hub/user/me/token/zzz/lab"
           "http://h:8888/tree?a=1&token=x&b=2" "http://h:8888/")))
    (run-hooks 'savehist-save-hook)
    (should (equal ygg-kernel-picker--server-history
                   '("http://h:8888/" "https://hub/user/me/lab" "http://h:8888/tree?a=1&b=2")))))

(ert-deftest kernel-picker-server-reuses-the-session-token ()
  (let ((ygg-kernel-picker--server-tokens (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'jupyter-server)
               (lambda (&rest _) (make-instance 'kernel-picker-test-server)))
              ((symbol-function 'jupyter-api-server-exists-p) (lambda (_) t)))
      (should (equal (ygg-kernel-picker--split-url "https://hub/user/me/token/zzz/lab")
                     '("https://hub/user/me" . "zzz")))
      (ygg-kernel-picker--server "http://gpu:8888/lab?token=abc123")
      (should (equal (slot-value (ygg-kernel-picker--server "http://gpu:8888/lab") 'auth)
                     '(("Authorization" . "token abc123")))))))

(provide 'kernel-picker-tests)
;;; kernel-picker-tests.el ends here
