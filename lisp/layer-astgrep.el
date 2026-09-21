;;; layer-astgrep.el --- ast-grep structural search -*- lexical-binding: t; -*-

;;; Code:

(require 'compile)
(require 'grep)
(require 'project)
(require 'ygg-ui)

(declare-function consult--read "consult")
(declare-function consult--lookup-member "consult")
(declare-function consult--grep-state "consult")
(declare-function consult--process-collection "consult")
(declare-function consult--async-transform "consult")
(declare-function consult--prefix-group "consult")
(declare-function yggdrasil-define-keys "yggdrasil-core")

(defvar ygg-leader-search-map)

(defvar ygg-ast-grep-history nil
  "History for `ygg-ast-grep' pattern prompts.")

(defconst ygg-ast-grep--lang-alist
  '((elixir-mode . "elixir") (elixir-ts-mode . "elixir")
    (python-mode . "python") (python-ts-mode . "python")
    (rust-mode . "rust") (rust-ts-mode . "rust") (rustic-mode . "rust")
    (typescript-mode . "typescript") (typescript-ts-mode . "typescript")
    (tsx-ts-mode . "tsx")
    (js-mode . "javascript") (js-ts-mode . "javascript")
    (js2-mode . "javascript") (javascript-mode . "javascript")
    (go-mode . "go") (go-ts-mode . "go")
    (java-mode . "java") (java-ts-mode . "java")
    (c-mode . "c") (c-ts-mode . "c")
    (c++-mode . "cpp") (c++-ts-mode . "cpp")
    (kotlin-mode . "kotlin") (kotlin-ts-mode . "kotlin")
    (ruby-mode . "ruby") (ruby-ts-mode . "ruby") (enh-ruby-mode . "ruby")
    (php-mode . "php") (php-ts-mode . "php")
    (swift-mode . "swift") (swift-ts-mode . "swift")
    (lua-mode . "lua") (lua-ts-mode . "lua")
    (scala-mode . "scala") (scala-ts-mode . "scala")
    (dart-mode . "dart") (dart-ts-mode . "dart")
    (csharp-mode . "csharp") (csharp-ts-mode . "csharp")
    (json-mode . "json") (json-ts-mode . "json")
    (yaml-mode . "yaml") (yaml-ts-mode . "yaml")
    (sh-mode . "bash") (bash-ts-mode . "bash")
    (html-mode . "html") (mhtml-mode . "html") (html-ts-mode . "html")
    (css-mode . "css") (css-ts-mode . "css"))
  "Major mode to ast-grep --lang id mapping.")

(defconst ygg-ast-grep--langs
  '("bash" "c" "cpp" "csharp" "css" "dart" "elixir" "go" "html" "java"
    "javascript" "json" "kotlin" "lua" "php" "python" "ruby" "rust"
    "scala" "swift" "tsx" "typescript" "yaml")
  "ast-grep language ids — the fallback picker and mode-name inference set.")

(defconst ygg-ast-grep--ext-alist
  '(("ex" . "elixir") ("exs" . "elixir") ("py" . "python") ("pyi" . "python")
    ("rs" . "rust") ("go" . "go") ("java" . "java")
    ("ts" . "typescript") ("mts" . "typescript") ("cts" . "typescript")
    ("tsx" . "tsx") ("js" . "javascript") ("jsx" . "javascript")
    ("mjs" . "javascript") ("cjs" . "javascript")
    ("kt" . "kotlin") ("kts" . "kotlin") ("rb" . "ruby") ("php" . "php")
    ("swift" . "swift") ("lua" . "lua") ("scala" . "scala") ("sc" . "scala")
    ("dart" . "dart") ("cs" . "csharp") ("json" . "json")
    ("yaml" . "yaml") ("yml" . "yaml") ("sh" . "bash") ("bash" . "bash")
    ("c" . "c") ("h" . "c") ("cpp" . "cpp") ("cc" . "cpp") ("cxx" . "cpp")
    ("hpp" . "cpp") ("hh" . "cpp") ("html" . "html") ("htm" . "html")
    ("css" . "css"))
  "File-extension to ast-grep --lang id, used when the mode isn't mapped.")

(defun ygg-ast-grep--lang-from-mode-name (mode)
  "Strip the `-mode'/`-ts-mode' suffix off MODE and keep it if a known lang."
  (car (member (replace-regexp-in-string "\\(-ts\\)?-mode\\'" "" (symbol-name mode))
               ygg-ast-grep--langs)))

(defun ygg-ast-grep--lang-from-file ()
  "Infer the lang from the current buffer's file extension, if any."
  (when-let* ((name (buffer-file-name))
              (ext (file-name-extension name)))
    (cdr (assoc (downcase ext) ygg-ast-grep--ext-alist))))

(defun ygg-ast-grep--lang (mode)
  "Infer an ast-grep language id from MODE, else the file, else prompt."
  (or (alist-get mode ygg-ast-grep--lang-alist)
      (ygg-ast-grep--lang-from-mode-name mode)
      (ygg-ast-grep--lang-from-file)
      (completing-read "ast-grep lang: " ygg-ast-grep--langs nil t)))

(defun ygg-ast-grep--root ()
  "Project root, falling back to `default-directory'."
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun ygg-ast-grep--ensure ()
  (unless (executable-find "ast-grep")
    (user-error "ast-grep not found on exec-path; install via `brew install ast-grep`")))

(defun ygg-ast-grep--read-pattern ()
  (read-string "ast-grep pattern: " nil 'ygg-ast-grep-history))

(defun ygg-ast-grep--command (pattern lang &optional rewrite)
  "The ast-grep command for PATTERN in LANG, streaming a JSON line a match.
REWRITE, when given, asks ast-grep what each match becomes."
  (append (list "ast-grep" "run" "--pattern" pattern "--lang" lang
                "--color" "never" "--json=stream")
          (when rewrite (list "--rewrite" rewrite))
          (list ".")))

(defun ygg-ast-grep--record (line)
  "LINE of ast-grep JSON as a match record, or nil when it is not one.
A record carries the file, the one-based line, the zero-based column,
the matched text and the line holding it, the byte range the match
covers, and the replacement when one was asked for."
  (when (string-prefix-p "{" line)
    (when-let* ((match (ignore-errors
                         (json-parse-string line :object-type 'plist
                                            :array-type 'list)))
                (file (plist-get match :file))
                (range (plist-get match :range))
                (start (plist-get range :start)))
      (let ((bytes (or (plist-get match :replacementOffsets)
                       (plist-get range :byteOffset))))
        (list :file file
              :line (1+ (plist-get start :line))
              :column (plist-get start :column)
              :text (plist-get match :text)
              :lines (plist-get match :lines)
              :start (plist-get bytes :start)
              :end (plist-get bytes :end)
              :replacement (plist-get match :replacement))))))

(defun ygg-ast-grep--records (lines)
  "The match records among LINES of ast-grep output."
  (delq nil (mapcar #'ygg-ast-grep--record lines)))

(defun ygg-ast-grep--first-line (text)
  "The first line of TEXT, since a list of matches shows a line a match."
  (car (split-string (or text "") "\n")))

(defun ygg-ast-grep--head (record)
  "The one line of RECORD a list of matches can show."
  (ygg-ast-grep--first-line (or (plist-get record :lines)
                                (plist-get record :text))))

(defun ygg-ast-grep--candidate (record)
  "RECORD as a candidate shaped the way consult--grep shapes its own.
The text properties mirror consult--grep-format so consult--grep-position
and consult--prefix-group (file-header grouping) both work unmodified."
  (let* ((file (plist-get record :file))
         (lineno (number-to-string (plist-get record :line)))
         (str (concat file ":" lineno ":" (ygg-ast-grep--head record))))
    (add-text-properties 0 (length file)
                         `(face consult-file consult--prefix-group ,file) str)
    (put-text-property (1+ (length file)) (+ 1 (length file) (length lineno))
                       'face 'consult-line-number str)
    str))

(defun ygg-ast-grep--candidates (lines)
  "The candidates the match records among LINES stand for."
  (mapcar #'ygg-ast-grep--candidate (ygg-ast-grep--records lines)))

(defun ygg-ast-grep--collect (pattern lang dir rewrite done)
  "Stream ast-grep for PATTERN in LANG under DIR and hand DONE the records.
REWRITE, when given, asks what each match becomes.  DONE is called with
the records and the exit status once ast-grep is done; nothing waits for
it."
  (let ((buffer (generate-new-buffer " *ast-grep-json*"))
        (default-directory dir))
    (make-process
     :name "ast-grep" :noquery t :buffer buffer :connection-type 'pipe
     :command (ygg-ast-grep--command pattern lang rewrite)
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let ((out (process-buffer proc))
               (status (process-exit-status proc)))
           (unwind-protect
               (funcall done
                        (when (buffer-live-p out)
                          (with-current-buffer out
                            (ygg-ast-grep--records
                             (split-string (buffer-string) "\n" t))))
                        status)
             (when (buffer-live-p out) (kill-buffer out)))))))))

(defun ygg-ast-grep--consult (pattern lang dir)
  "Consult picker over one streaming ast-grep run for PATTERN, LANG, DIR.
The run is fixed: ast-grep patterns (the dollar captures, the spaces)
fight consult's per-keystroke command splitting, so what is typed in the
minibuffer filters the matches as they arrive instead of re-searching."
  (require 'consult)
  (let* ((default-directory dir)
         (command (ygg-ast-grep--command pattern lang)))
    (consult--read
     (consult--process-collection (lambda (_input) command)
       :min-input 0
       :transform (consult--async-transform #'ygg-ast-grep--candidates))
     :prompt (format "ast-grep [%s]: " lang)
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :category 'consult-grep
     :group #'consult--prefix-group
     :sort nil
     :require-match t)))

(defun ygg-ast-grep--list-show (pattern lang dir records)
  "Show RECORDS of PATTERN in LANG under DIR where next-error can walk them."
  (let ((buf (get-buffer-create "*ast-grep*")))
    (with-current-buffer buf
      (grep-mode)
      (setq default-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "ast-grep --pattern %s --lang %s\n\n" pattern lang))
        (dolist (record records)
          (insert (format "%s:%s:%s\n" (plist-get record :file)
                          (plist-get record :line)
                          (ygg-ast-grep--head record))))
        (insert (format "\nast-grep finished with %d matches\n"
                        (length records))))
      (goto-char (point-min)))
    (ygg-ui-show buf)))

(defun ygg-ast-grep--preview (records)
  "What RECORDS change, a match and the replacement taking its place."
  (mapconcat
   (lambda (record)
     (format "%s:%s\n- %s\n+ %s\n" (plist-get record :file)
             (plist-get record :line)
             (ygg-ast-grep--first-line (plist-get record :text))
             (ygg-ast-grep--first-line (plist-get record :replacement))))
   records "\n"))

(defun ygg-ast-grep--edits (records)
  "The edits RECORDS ask for, grouped by file, the last of a file first.
Each edit is the byte range a match covers and the replacement taking
its place; applying a file back to front keeps the earlier ranges true."
  (let (groups)
    (dolist (record records)
      (when-let* ((replacement (plist-get record :replacement))
                  (file (plist-get record :file))
                  (start (plist-get record :start))
                  (end (plist-get record :end)))
        (let ((cell (assoc file groups)))
          (unless cell
            (setq cell (cons file nil))
            (push cell groups))
          (setcdr cell (cons (list :start start :end end
                                   :replacement replacement)
                             (cdr cell))))))
    (mapcar (lambda (cell)
              (cons (car cell)
                    (sort (cdr cell)
                          (lambda (a b) (> (plist-get a :start)
                                           (plist-get b :start))))))
            (nreverse groups))))

(defun ygg-ast-grep--apply (dir edits)
  "Write EDITS under DIR back into their files, and say how many took one."
  (dolist (group edits)
    (let ((path (expand-file-name (car group) dir)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (dolist (edit (cdr group))
          (delete-region (1+ (plist-get edit :start))
                         (1+ (plist-get edit :end)))
          (goto-char (1+ (plist-get edit :start)))
          (insert (encode-coding-string (plist-get edit :replacement) 'utf-8)))
        (let ((coding-system-for-write 'binary))
          (write-region (point-min) (point-max) path nil 'silent)))))
  (length edits))

(defun ygg-ast-grep--rewrite-offer (dir records)
  "Show what RECORDS change under DIR and write it when told to."
  (let ((buf (get-buffer-create "*ast-grep-rewrite*")))
    (with-current-buffer buf
      (setq default-directory dir)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (ygg-ast-grep--preview records)))
      (goto-char (point-min))
      (view-mode 1))
    (ygg-ui-show buf t)
    (if (yes-or-no-p (format "Apply this rewrite across %s? "
                             (abbreviate-file-name dir)))
        (let ((files (ygg-ast-grep--apply dir
                                          (ygg-ast-grep--edits records))))
          (message "ast-grep: rewrote %d file%s (revert buffers to refresh)"
                   files (if (= files 1) "" "s"))
          (when (buffer-live-p buf) (kill-buffer buf)))
      (message "ast-grep: rewrite cancelled"))))

(defun ygg-ast-grep ()
  "Structural search: consult picker when consult is loadable, else a list."
  (interactive)
  (ygg-ast-grep--ensure)
  (let* ((mode major-mode)
         (dir (ygg-ast-grep--root))
         (pattern (ygg-ast-grep--read-pattern))
         (lang (ygg-ast-grep--lang mode)))
    (if (require 'consult nil t)
        (ygg-ast-grep--consult pattern lang dir)
      (ygg-ast-grep--list-run pattern lang dir))))

(defun ygg-ast-grep--list-run (pattern lang dir)
  "Stream PATTERN in LANG under DIR into a list next-error can walk."
  (ygg-ast-grep--collect
   pattern lang dir nil
   (lambda (records status)
     (cond
      (records (ygg-ast-grep--list-show pattern lang dir records))
      ((> status 1) (message "ast-grep failed (%s)" status))
      (t (message "ast-grep: no matches for %s (%s)" pattern lang))))))

(defun ygg-ast-grep-list ()
  "Structural search into a navigable list, so next-error walks the matches."
  (interactive)
  (ygg-ast-grep--ensure)
  (let* ((mode major-mode)
         (dir (ygg-ast-grep--root))
         (pattern (ygg-ast-grep--read-pattern))
         (lang (ygg-ast-grep--lang mode)))
    (ygg-ast-grep--list-run pattern lang dir)))

(defun ygg-ast-grep-rewrite ()
  "Structural search and replace: see what changes, then let it be written.
Prompts for a search pattern and a rewrite, streams the matches with the
replacement each one gets, and on confirmation writes those replacements
back over the byte ranges ast-grep gave."
  (interactive)
  (ygg-ast-grep--ensure)
  (let* ((mode major-mode)
         (dir (ygg-ast-grep--root))
         (pattern (read-string "ast-grep search: " nil 'ygg-ast-grep-history))
         (rewrite (read-string "ast-grep rewrite: " nil 'ygg-ast-grep-history))
         (lang (ygg-ast-grep--lang mode)))
    (ygg-ast-grep--collect
     pattern lang dir rewrite
     (lambda (records status)
       (cond
        (records (ygg-ast-grep--rewrite-offer dir records))
        ((> status 1) (message "ast-grep failed (%s)" status))
        (t (message "ast-grep: no matches for %s" pattern)))))))

(with-eval-after-load 'layer-completion
  (yggdrasil-define-keys 'ygg-leader-search-map
    "a" #'ygg-ast-grep :label "ast-grep"
    "A" #'ygg-ast-grep-list :label "ast-grep list"))

(provide 'layer-astgrep)
;;; layer-astgrep.el ends here
