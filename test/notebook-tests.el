;;; notebook-tests.el --- Tests for cells, chunks and polymode scope -*- lexical-binding: t; -*-

(require 'ert)

(let ((builds (expand-file-name "../elpaca/builds/"
                                (file-name-directory (or load-file-name buffer-file-name)))))
  (dolist (package '("markdown-mode" "code-cells" "polymode" "poly-markdown" "jupyter"))
    (let ((dir (expand-file-name package builds)))
      (add-to-list 'load-path dir)
      (load (expand-file-name (concat package "-autoloads") dir) nil t))))

(require 'yggdrasil)
(require 'layer-notebook)
(require 'ygg-kernel-picker)
(require 'aob)
(require 'aob-trace)

(defvar jupyter-kernel-language-mode-properties)
(defvar jupyter-repl-history)

(defconst notebook-tests--percent "import os\n# %%\na = 1\n# %%\nb = 2\n")

(defconst notebook-tests--chunks
  "---\ntitle: t\n---\n\nProse.\n\n```{python}\nx = 1\n```\n\nMore.\n\n```{r setup}\ny <- 2\n```\n")

(defmacro notebook-tests--in (mode text &rest body)
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (,mode)
     (goto-char (point-min))
     ,@body))

(defun notebook-tests--visit (name text)
  "Visit a fresh file NAME holding TEXT and return its buffer."
  (let ((file (expand-file-name name (make-temp-file "nb" t))))
    (with-temp-file file (insert text))
    (find-file-noselect file)))

(ert-deftest notebook-percent-cells-motion ()
  (notebook-tests--in python-mode notebook-tests--percent
    (let ((first (ygg-nb--boundary 1)))
      (should (= first (save-excursion (search-forward "# %%") (match-beginning 0))))
      (goto-char first)
      (let ((second (ygg-nb--boundary 1)))
        (should (string= (buffer-substring second (+ second 4)) "# %%"))
        (should (> second first))
        (goto-char second)
        (should (= (ygg-nb--boundary -1) first))
        (goto-char (point-max))
        (should-not (ygg-nb--boundary 1))))))

(ert-deftest notebook-percent-cells-bounds ()
  (notebook-tests--in python-mode notebook-tests--percent
    (search-forward "a = 1")
    (pcase-let ((`(,beg . ,end) (ygg-nb-cell-bounds)))
      (should (string= (buffer-substring beg end) "a = 1\n")))
    (pcase-let ((`(,beg . ,end) (ygg-nb-cell-bounds t)))
      (should (string= (buffer-substring beg end) "# %%\na = 1\n")))
    (should (equal (ygg-nb--language) "python"))))

(ert-deftest notebook-chunk-motion-and-bounds ()
  (notebook-tests--in markdown-mode notebook-tests--chunks
    (goto-char (ygg-nb--boundary 1))
    (should (looking-at "```{python}"))
    (forward-line 1)
    (should (equal (ygg-nb--language) "python"))
    (pcase-let ((`(,beg . ,end) (ygg-nb-cell-bounds)))
      (should (string= (buffer-substring beg end) "x = 1\n")))
    (pcase-let ((`(,beg . ,end) (ygg-nb-cell-bounds t)))
      (should (string= (buffer-substring beg end) "```{python}\nx = 1\n```\n")))
    (goto-char (ygg-nb--boundary 1))
    (should (looking-at "```{r setup}"))
    (forward-line 1)
    (should (equal (ygg-nb--language) "r"))
    (search-backward "More.")
    (should-not (ygg-nb-cell-bounds))
    (should-not (ygg-nb--language))))

(ert-deftest notebook-motion-commands-move-point ()
  (notebook-tests--in python-mode notebook-tests--percent
    (yggdrasil-local-mode 1)
    (ygg-nb-next-cell)
    (should (looking-at "# %%\na"))
    (ygg-nb-next-cell)
    (should (looking-at "# %%\nb"))
    (ygg-nb-prev-cell)
    (should (looking-at "# %%\na"))))

(ert-deftest notebook-cell-textobject ()
  (require 'yggdrasil-match)
  (notebook-tests--in markdown-mode notebook-tests--chunks
    (search-forward "x = 1")
    (should (equal (ygg-match--textobject-bounds ?% 'inside) (ygg-nb-cell-bounds)))
    (should (equal (ygg-match--textobject-bounds ?% 'around) (ygg-nb-cell-bounds t)))))

(ert-deftest notebook-polymode-for-rmd-and-qmd-only ()
  (should-not (rassq 'poly-markdown-mode
                     (seq-filter (lambda (e) (equal (car e) "\\.md\\'")) auto-mode-alist)))
  (dolist (name '("doc.qmd" "doc.Rmd" "doc.md"))
    (let ((buffer (notebook-tests--visit name notebook-tests--chunks)))
      (unwind-protect
          (with-current-buffer buffer
            (if (string-suffix-p ".md" name)
                (progn (should-not (bound-and-true-p polymode-mode))
                       (should (derived-mode-p 'markdown-mode)))
              (should (bound-and-true-p polymode-mode))))
        (kill-buffer buffer)))))

(ert-deftest notebook-polymode-toggle-in-markdown ()
  (let ((buffer (notebook-tests--visit "plain.md" notebook-tests--chunks)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((mode major-mode))
            (ygg-nb-toggle-polymode)
            (should (bound-and-true-p polymode-mode))
            (ygg-nb-toggle-polymode)
            (should-not (bound-and-true-p polymode-mode))
            (should (eq major-mode mode))))
      (kill-buffer buffer))))

(ert-deftest notebook-polymode-never-in-agent-buffers ()
  (dolist (mode '(aob-trace-mode aob-compose-mode))
    (with-temp-buffer
      (insert notebook-tests--chunks)
      (funcall mode)
      (should-not (bound-and-true-p polymode-mode))
      (should-error (ygg-nb-toggle-polymode) :type 'user-error)
      (should-not (bound-and-true-p polymode-mode)))))

(ert-deftest notebook-chunk-faces-carry-no-background ()
  (require 'polymode)
  (require 'poly-markdown)
  (let ((inner poly-markdown-fenced-code-innermode))
    (should-not (pm-get-adjust-face inner 'body))
    (dolist (type '(head tail))
      (let ((face (pm-get-adjust-face inner type)))
        (should (facep face))
        (should (eq (face-attribute face :background nil t) 'unspecified)))))
  (require 'code-cells)
  (should (eq (face-attribute 'code-cells-header-line :background nil t) 'unspecified)))

(ert-deftest notebook-bindings-present ()
  (should (eq (lookup-key ygg-normal-map (kbd "] %")) #'ygg-nb-next-cell))
  (should (eq (lookup-key ygg-normal-map (kbd "[ %")) #'ygg-nb-prev-cell))
  (dolist (pair '(("x" . ygg-nb-eval-cell) ("n" . ygg-nb-eval-cell-and-next)
                  ("b" . ygg-nb-eval-buffer) ("P" . ygg-nb-history-previous)
                  ("N" . ygg-nb-history-next) ("/" . ygg-nb-history-search)))
    (should (eq (lookup-key ygg-leader-jupyter-map (car pair)) (cdr pair))))
  (dolist (mode '(markdown-mode gfm-mode))
    (let ((map (ygg-localleader--get-map mode)))
      (should (eq (lookup-key map "R") #'ygg-nb-quarto-render))
      (should (eq (lookup-key map "P") #'ygg-nb-quarto-preview))
      (should (eq (lookup-key map "M") #'ygg-nb-toggle-polymode)))))

(ert-deftest notebook-chunk-buffer-reaches-both-key-sets ()
  (let ((buffer (notebook-tests--visit "keys.qmd" notebook-tests--chunks)))
    (unwind-protect
        (with-current-buffer buffer
          (search-forward "x = 1")
          (pm-set-buffer (point))
          (let ((inner (current-buffer)))
            (with-current-buffer inner
              (should (buffer-base-buffer))
              (yggdrasil-local-mode 1)
              (should (eq (key-binding (kbd "\\ R")) #'ygg-nb-quarto-render))
              (should (eq (key-binding (kbd "] %")) #'ygg-nb-next-cell))
              (should (eq (key-binding (kbd "SPC r x")) #'ygg-nb-eval-cell))
              (should-not (eq (key-binding (kbd "\\ e")) #'ygg-nb-eval-cell)))))
      (kill-buffer buffer))))

;;; Kernel languages beyond python and R

(defconst notebook-tests--slash "package main\n\n// %%\na := 1 + 1\n# %% not here\n// %%\nb := 2\n")

(ert-deftest notebook-kernel-language-from-major-mode ()
  (notebook-tests--in ruby-mode "" (should (equal (ygg-nb--language) "ruby")))
  (notebook-tests--in java-mode "" (should (equal (ygg-nb--language) "java")))
  (when (treesit-language-available-p 'go)
    (notebook-tests--in go-ts-mode "" (should (equal (ygg-nb--language) "go"))))
  (when (treesit-language-available-p 'rust)
    (notebook-tests--in rust-ts-mode "" (should (equal (ygg-nb--language) "rust")))))

(ert-deftest notebook-slash-percent-cells ()
  (dolist (mode (if (treesit-language-available-p 'go) '(java-mode go-ts-mode) '(java-mode)))
    (notebook-tests--in (lambda () (funcall mode)) notebook-tests--slash
      (should code-cells-mode)
      (let ((first (ygg-nb--boundary 1)))
        (should (= first (save-excursion (search-forward "// %%") (match-beginning 0))))
        (goto-char first)
        (goto-char (ygg-nb--boundary 1))
        (should (looking-at "// %%\nb"))
        (should (= (ygg-nb--boundary -1) first)))
      (search-backward "a := 1")
      (pcase-let ((`(,beg . ,end) (ygg-nb-cell-bounds)))
        (should (string= (buffer-substring beg end) "a := 1 + 1\n# %% not here\n")))
      (should (equal (ygg-nb--textobject-bounds ?% 'around)
                     (cons (save-excursion (search-backward "// %%"))
                           (save-excursion (search-forward "// %%") (match-beginning 0))))))))

(ert-deftest notebook-hash-percent-cells-in-ruby ()
  (notebook-tests--in ruby-mode "x = 0\n# %%\na = 1 + 1\n# %%\nb = 2\n"
    (should code-cells-mode)
    (search-forward "a = 1")
    (pcase-let ((`(,beg . ,end) (ygg-nb-cell-bounds)))
      (should (string= (buffer-substring beg end) "a = 1 + 1\n")))))

(ert-deftest notebook-kernel-mode-prefers-tree-sitter-with-a-grammar ()
  (cl-letf (((symbol-function 'treesit-language-available-p) #'ignore))
    (should (eq (ygg-nb-kernel-mode 'ruby) 'ruby-mode))
    (should (eq (ygg-nb-kernel-mode "java") 'java-mode)))
  (cl-letf (((symbol-function 'treesit-language-available-p) #'always))
    (should (eq (ygg-nb-kernel-mode 'java) 'java-ts-mode)))
  (should-not (ygg-nb-kernel-mode 'julia)))

(ert-deftest notebook-kernel-language-seeds-the-repl-mode ()
  (let ((jupyter-kernel-language-mode-properties nil))
    (cl-letf (((symbol-function 'jupyter-kernel-language) (lambda (_) 'Java))
              ((symbol-function 'treesit-language-available-p) #'ignore))
      (ygg-nb--seed-language-mode 'client)
      (should (eq (cadr (assq 'Java jupyter-kernel-language-mode-properties)) 'java-mode))
      (should (syntax-table-p (nth 2 (assq 'Java jupyter-kernel-language-mode-properties))))
      (let ((kept (assq 'Java jupyter-kernel-language-mode-properties)))
        (ygg-nb--seed-language-mode 'client)
        (should (eq (assq 'Java jupyter-kernel-language-mode-properties) kept))))
    (cl-letf (((symbol-function 'jupyter-kernel-language) (lambda (_) 'julia)))
      (ygg-nb--seed-language-mode 'client)
      (should-not (assq 'julia jupyter-kernel-language-mode-properties)))))

(ert-deftest notebook-kernel-keys-leave-the-localleader ()
  (cl-flet ((command (mode key)
              (let ((binding (lookup-key (ygg-localleader--get-map mode) (kbd key))))
                (if (consp binding) (cdr binding) binding))))
    (dolist (mode (append '(python-mode python-ts-mode r-ts-mode markdown-mode gfm-mode
                            poly-fallback-mode jupyter-repl-mode)
                          ygg-nb--kernel-language-modes))
      (dolist (key '("e" "n" "b" "B" "v" "d" "D"))
        (should-not (memq (command mode key)
                          '(ygg-nb-eval-cell ygg-nb-eval-cell-and-next ygg-nb-eval-buffer
                            ygg-kernel-vars-toggle ygg-visidata-view
                            ygg-visidata-open-file)))))
    (should (eq (command 'rust-ts-mode "c") 'ygg-localleader-rust-check))
    (dolist (mode '(kotlin-ts-mode kotlin-mode))
      (should (eq (command mode "a") 'ygg-localleader-kotlin-assemble))
      (should (eq (command mode "i") 'ygg-localleader-kotlin-install)))))

(ert-deftest notebook-jupyter-leader-has-no-chords ()
  (should (eq (lookup-key ygg-leader-map (kbd "r")) ygg-leader-jupyter-map))
  (let (keys)
    (map-keymap (lambda (event def)
                  (push (key-description (vector event)) keys)
                  (should (commandp (if (consp def) (cdr def) def))))
                ygg-leader-jupyter-map)
    (should (= (length keys) 23))
    (dolist (key keys)
      (should-not (string-match-p "\\`[CMsH]-\\|[CMsH]-." key)))))

(ert-deftest notebook-repl-buffer-is-modal ()
  (should (memq 'jupyter-repl-mode ygg-modal-special-modes)))

(defmacro notebook-tests--in-repl (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (setq major-mode 'jupyter-repl-mode)
     ,@body))

(ert-deftest notebook-repl-cell-motion ()
  (notebook-tests--in-repl "In [1]: a\nOut: 1\nIn [2]: b\n"
    (goto-char 20)
    (cl-letf (((symbol-function 'jupyter-repl-forward-cell) (lambda () (goto-char 28)))
              ((symbol-function 'jupyter-repl-backward-cell) (lambda () (goto-char 9))))
      (should (= (ygg-nb--boundary 1) 28))
      (should (= (point) 20))
      (should (= (ygg-nb--boundary -1) 9)))
    (cl-letf (((symbol-function 'jupyter-repl-forward-cell) #'ignore)
              ((symbol-function 'jupyter-repl-backward-cell) (lambda () (error "No cell"))))
      (should-not (ygg-nb--boundary 1))
      (should-not (ygg-nb--boundary -1)))))

(ert-deftest notebook-repl-insert-lands-at-the-prompt ()
  (notebook-tests--in-repl "In [1]: a\nOut: 1\nIn [2]: b"
    (cl-letf (((symbol-function 'jupyter-repl-cell-code-beginning-position)
               (lambda () 26)))
      (goto-char 3)
      (ygg-nb--repl-insert-at-prompt)
      (should (= (point) (point-max)))
      (goto-char 26)
      (ygg-nb--repl-insert-at-prompt)
      (should (= (point) 26)))))

(ert-deftest notebook-repl-history-search-offers-newest-first ()
  (require 'ring)
  (notebook-tests--in-repl "In [1]: "
    (let ((jupyter-repl-history (make-ring 5))
          offered replaced)
      (ring-insert jupyter-repl-history 'jupyter-repl-history)
      (ring-insert jupyter-repl-history "old = 1")
      (ring-insert jupyter-repl-history "new = 2")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq offered (all-completions "" table))
                   (car offered)))
                ((symbol-function 'jupyter-repl-replace-cell-code)
                 (lambda (code) (setq replaced code))))
        (ygg-nb--history-pick))
      (should (equal offered '("new = 2" "old = 1")))
      (should (equal replaced "new = 2")))))

(ert-deftest notebook-send-evaluates-the-line-in-a-source-buffer ()
  (notebook-tests--in python-mode "a = 1\nb = 2\n"
    (forward-line 1)
    (let (sent)
      (cl-letf (((symbol-function 'ygg-nb-eval-region)
                 (lambda (beg end) (setq sent (buffer-substring beg end)))))
        (ygg-nb-send))
      (should (equal sent "b = 2")))))

(ert-deftest notebook-jupyter-display-buffers-turn-modal ()
  (with-temp-buffer
    (special-mode)
    (let ((yggdrasil-global-mode t))
      (should (eq (ygg-nb--modal-display-buffer (current-buffer)) (current-buffer))))
    (should yggdrasil-local-mode)
    (should (eq (ygg-state) 'normal))
    (should (eq (lookup-key ygg--special-keep-map "q") 'quit-window))))

(defun notebook-tests--kernel-message (id type content)
  (list :header (list :msg_id (format "%s-%s" id type) :msg_type type)
        :parent_header (list :msg_id id :msg_type "execute_request")
        :content content))

(defun notebook-tests--quiet-kernel (implementation expressions sent)
  "A client whose kernel answers like IPython after a stored cell ending in ;.
A plain execute gets no execute_result, except from a non-IPython
IMPLEMENTATION.  EXPRESSIONS maps user expression code to its reply.
Each request's content is pushed onto the list in SENT."
  (let* ((client (make-instance 'jupyter-kernel-client))
         io)
    (setq io (jupyter-publisher
               (lambda (value)
                 (pcase value
                   (`(send ,_ ,_ ,content ,id)
                    (push content (car sent))
                    (let* ((asked (plist-get content :user_expressions))
                           (replies (and (consp asked)
                                         (cl-loop for (key code) on asked by #'cddr
                                                  append (list key (cdr (assoc code expressions))))))
                           (result (and (not (consp asked))
                                        (not (equal implementation "ipython"))
                                        (list (notebook-tests--kernel-message
                                               id "execute_result"
                                               (list :data (list :text/plain "[1] 42")))))))
                      (dolist (msg (append
                                    (list (notebook-tests--kernel-message
                                           id "status" '(:execution_state "busy")))
                                    result
                                    (list (notebook-tests--kernel-message
                                           id "execute_reply"
                                           (list :status "ok" :execution_count 1
                                                 :user_expressions replies))
                                          (notebook-tests--kernel-message
                                           id "status" '(:execution_state "idle")))))
                        (jupyter-run-with-io io (jupyter-publish msg))))
                    nil)
                   (_ (jupyter-content value))))))
    (oset client io (list io nil))
    (oset client kernel-info (list :implementation implementation
                                   :language_info (list :name 'python)))
    client))

(ert-deftest notebook-eval-survives-a-quiet-ipython ()
  (skip-unless (require 'jupyter-client nil t))
  (let* ((sent (list nil))
         (jupyter-current-client
          (notebook-tests--quiet-kernel
           "ipython"
           '(("6*7" :status "ok" :data (:text/plain "42"))
             ("x = 1" :status "error" :ename "SyntaxError" :evalue "invalid syntax")
             ("nope" :status "error" :ename "NameError" :evalue "name 'nope' is not defined"))
           sent)))
    (should (equal (jupyter-eval "6*7") "42"))
    (should (equal (plist-get (car (car sent)) :silent) t))
    (should (equal (plist-get (car (car sent)) :store_history) :json-false))
    (should-error (jupyter-eval "nope"))
    (setcar sent nil)
    (should-not (jupyter-eval "x = 1"))
    (should (equal (mapcar (lambda (content) (plist-get content :code)) (car sent))
                   '("x = 1" ""))))
  (let* ((sent (list nil))
         (jupyter-current-client (notebook-tests--quiet-kernel "ark" nil sent)))
    (should (equal (jupyter-eval "6*7") "[1] 42"))
    (should (equal (mapcar (lambda (content) (plist-get content :code)) (car sent))
                   '("6*7")))))

(defclass notebook-tests--client () ((buffer :initarg :buffer)))

(ert-deftest notebook-chunk-buffer-resolves-its-kernel ()
  (require 'ygg-kernel-vars)
  (require 'ygg-ark)
  (let ((buffer (notebook-tests--visit "vars.qmd" notebook-tests--chunks))
        (repl (generate-new-buffer " *repl*")))
    (unwind-protect
        (let ((python (notebook-tests--client :buffer repl))
              (ark (notebook-tests--client :buffer repl)))
          (cl-letf (((symbol-function 'jupyter-kernel-info)
                     (lambda (client)
                       (list :implementation (if (eq client ark) "ark" "ipython")))))
            (with-current-buffer buffer
              (search-forward "x = 1")
              (pm-set-buffer (point))
              (ygg-nb--remember "python" python)
              (should (eq jupyter-current-client python))
              (should (eq (ygg-kernel-vars--client) python))
              (should-not (local-variable-p 'jupyter-current-client (buffer-base-buffer)))
              (goto-char (point-min))
              (search-forward "y <- 2")
              (pm-set-buffer (point))
              (should-not (ygg-ark--buffer-client))
              (ygg-nb--remember "r" ark)
              (should (eq (ygg-ark--buffer-client) ark))
              (should (eq (ygg-kernel-vars--client) ark)))))
      (kill-buffer buffer)
      (kill-buffer repl))))

(ert-deftest notebook-plain-markdown-keeps-no-single-client ()
  (let ((repl (generate-new-buffer " *repl*")))
    (unwind-protect
        (notebook-tests--in markdown-mode notebook-tests--chunks
          (ygg-nb--remember "python" (notebook-tests--client :buffer repl))
          (should-not (local-variable-p 'jupyter-current-client)))
      (kill-buffer repl))))

(ert-deftest notebook-preview-url-waits-for-the-whole-line ()
  (let* ((ygg-nb--previews (make-hash-table :test #'equal))
         (buffer (generate-new-buffer " *quarto preview*"))
         (process (make-pipe-process :name "quarto-preview-test" :buffer buffer :noquery t))
         (filter (ygg-nb--preview-filter "doc.qmd"))
         opened)
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-nb--open-url) (lambda (url) (push url opened))))
          (puthash "doc.qmd" (list process) ygg-nb--previews)
          (funcall filter process "Watching files for changes\nBrowse at http://localhost:43")
          (should-not opened)
          (funcall filter process "21/\n")
          (should (equal opened '("http://localhost:4321/")))
          (should (equal (cdr (gethash "doc.qmd" ygg-nb--previews)) "http://localhost:4321/")))
      (delete-process process)
      (kill-buffer buffer))))

(ert-deftest notebook-remember-runs-the-kernel-hook-in-the-chunk ()
  (let ((buffer (notebook-tests--visit "hook.qmd" notebook-tests--chunks))
        (repl (generate-new-buffer " *repl*"))
        seen)
    (unwind-protect
        (let ((client (notebook-tests--client :buffer repl))
              (ygg-nb-kernel-hook
               (list (lambda () (push (cons (current-buffer) (symbol-value (quote jupyter-current-client))) seen)))))
          (with-current-buffer buffer
            (search-forward "x = 1")
            (pm-set-buffer (point))
            (ygg-nb--remember "python" client)
            (should (equal seen (list (cons (current-buffer) client))))))
      (kill-buffer buffer)
      (kill-buffer repl))))

(provide 'notebook-tests)
;;; notebook-tests.el ends here
