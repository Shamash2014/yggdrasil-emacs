;;; ygg-ice.el --- Intent, context, expectation: lat.md, OpenSpec, LikeC4 and the docs around them -*- lexical-binding: t; -*-

;; Wraps lat, openspec and likec4 (through mise) with compile, xref and markdown-mode's wiki links.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'compile)
(require 'xref)
(require 'json)
(require 'ansi-color)
(require 'dom)
(require 'ygg-todo)

(declare-function yggdrasil-leader-def "yggdrasil-leader" (key def &optional label))
(declare-function yggdrasil-define-mode-keys "yggdrasil-core" (mode states &rest bindings))
(declare-function ygg-normal-state "yggdrasil-core")
(declare-function ygg-diagram-toggle "ygg-diagram")
(declare-function ygg-qf-from-text "layer-quickfix" (text &optional name replace list))
(declare-function aob-context--render "aob-context")
(declare-function eglot-ensure "eglot")
(declare-function ygg--jump-push "yggdrasil-motions")
(declare-function aob--capf-dir "aob")
(declare-function markdown-follow-wiki-link "markdown-mode" (name &optional other))
(defvar ygg-modal-special-modes)
(defvar ygg--visual-p)
(defvar ygg-visual-entry-hook)
(defvar aob-context--items)
(defvar eglot-server-programs)
(defvar markdown-enable-wiki-links)
(defvar markdown-wiki-link-alias-first)

(defgroup ygg-ice nil
  "Intent, context and expectation docs: lat.md, OpenSpec, LikeC4, ADRs."
  :group 'tools)

(defcustom ygg-ice-lat-program "lat"
  "The lat CLI, a name looked up on exec-path and in mise's shims, or a path."
  :type 'string)

(defcustom ygg-ice-likec4-program "likec4"
  "The likec4 CLI, a name looked up on exec-path and in mise's shims, or a path."
  :type 'string)

(defcustom ygg-ice-check-script (locate-user-emacs-file "etc/ice/ice-check")
  "The ice-check script: intent, expect and plan checks of a change."
  :type 'file)

(defcustom ygg-ice-wire-script (locate-user-emacs-file "etc/ice/ice-wire.sh")
  "The ice-wire script, which sets a repository up for ICE."
  :type 'file)

(defcustom ygg-ice-c4-drift-script (locate-user-emacs-file "etc/ice/ice-c4-drift")
  "The ice-c4-drift script: C4 elements whose code paths are gone."
  :type 'file)

(defcustom ygg-ice-lat-drift-script (locate-user-emacs-file "etc/ice/ice-lat-drift")
  "The ice-lat-drift script: lat.md left behind by the code it links."
  :type 'file)

(defcustom ygg-ice-compact-script (locate-user-emacs-file "etc/ice/ice-compact")
  "The ice-compact script: file archived changes into lat.md, remove old ones."
  :type 'file)

(defcustom ygg-ice-compact-days 30
  "Archived changes older than this many days are offered for removal."
  :type 'natnum)

(defcustom ygg-ice-arch-dirs '("docs/arch" "doc/arch")
  "Where a repository keeps its LikeC4 model, first found wins."
  :type '(repeat string))

(defcustom ygg-ice-lat-timeout 10
  "Seconds a lookup that has to answer at once waits for lat."
  :type 'number)

(defcustom ygg-ice-search-limit 20
  "How many sections lat search is asked for."
  :type 'natnum)

(defface ygg-ice-heading '((t :weight bold))
  "A group's name in the ICE views.")

(defface ygg-ice-dim '((t :inherit shadow))
  "Paths, counts and rails in the ICE views.")

(defconst ygg-ice--link-re "\\[\\[\\([^]|\n]+?\\)\\(?:|[^]\n]*\\)?\\]\\]"
  "A wiki link; group 1 is its target.")

;;; Where things are

(defun ygg-ice--program (name)
  "NAME as a runnable path, from exec-path or mise's shims, else nil."
  (or (executable-find name)
      (let ((shim (expand-file-name name "~/.local/share/mise/shims")))
        (and (not (file-name-absolute-p name)) (file-executable-p shim) shim))))

(defun ygg-ice-root (&optional dir)
  "The repository DIR is in, found by its ICE docs or its git folder."
  (let ((dir (expand-file-name (or dir default-directory))))
    (file-name-as-directory
     (or (locate-dominating-file
          dir (lambda (d)
                (or (file-directory-p (expand-file-name "lat.md" d))
                    (file-directory-p (expand-file-name "openspec" d))
                    (file-exists-p (expand-file-name ".git" d)))))
         dir))))

(defun ygg-ice--docs-dir (root)
  "ROOT's docs folder as ice-wire picks it: docs, else an existing doc."
  (file-name-as-directory
   (expand-file-name (if (and (not (file-directory-p (expand-file-name "docs" root)))
                              (file-directory-p (expand-file-name "doc" root)))
                         "doc" "docs")
                     root)))

(defun ygg-ice--arch-dir (root)
  "ROOT's LikeC4 folder: its docs folder's arch, else one of ygg-ice-arch-dirs."
  (seq-find #'file-directory-p
            (cons (file-name-as-directory (expand-file-name "arch" (ygg-ice--docs-dir root)))
                  (mapcar (lambda (d) (file-name-as-directory (expand-file-name d root)))
                          ygg-ice-arch-dirs))))

(defun ygg-ice--adr-dir (root)
  (file-name-as-directory (expand-file-name "adr" (ygg-ice--docs-dir root))))

(defun ygg-ice--mtime (file)
  (file-attribute-modification-time (file-attributes file)))

(defvar ygg-ice--codex-home nil
  "The folder the last ICE run was given as CODEX_HOME.")

(defun ygg-ice--codex-environment ()
  "The environment with CODEX_HOME on an empty folder of its own.
openspec init deletes files in CODEX_HOME, and in ~/.codex without one."
  (when (and ygg-ice--codex-home (file-directory-p ygg-ice--codex-home))
    (delete-directory ygg-ice--codex-home t))
  (setq ygg-ice--codex-home (make-temp-file "ice-codex" t))
  (cons (concat "CODEX_HOME=" ygg-ice--codex-home) process-environment))

;;; lat.md, read here

(defun ygg-ice--lat-files (root)
  (let ((dir (expand-file-name "lat.md" root)))
    (when (file-directory-p dir)
      (sort (seq-remove (lambda (f)
                          (string-match-p "\\(?:\\`\\|/\\)\\." (file-relative-name f dir)))
                        (directory-files-recursively dir "\\.md\\'"))
            #'string<))))

(defun ygg-ice--lat-signature (root)
  (mapcar (lambda (f) (cons f (ygg-ice--mtime f))) (ygg-ice--lat-files root)))

(defconst ygg-ice--entities
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'") ("nbsp" . " "))
  "The named character references a heading's text is decoded from.")

(defun ygg-ice--entity (name)
  "The character the reference &NAME; stands for, or nil when it names none."
  (or (cdr (assoc name ygg-ice--entities))
      (when (fboundp 'libxml-parse-html-region)
        (with-temp-buffer
          (insert "<p>&" name ";</p>")
          (let ((text (dom-inner-text (libxml-parse-html-region (point-min) (point-max)))))
            (unless (equal text (concat "&" name ";")) text))))))

(defun ygg-ice--punct-p (char)
  (and char (string-match-p "[[:punct:]]" (string char))))

(defun ygg-ice--space-p (char)
  (or (null char) (memq char '(?\s ?\t ?\n ? ))))

(defun ygg-ice--bracket-end (s i open close)
  "Index after the CLOSE matching the OPEN at I in S, escapes skipped, else nil."
  (let ((depth 0) (n (length s)) end)
    (while (and (< i n) (not end))
      (let ((c (aref s i)))
        (cond ((eq c ?\\) (setq i (1+ i)))
              ((eq c open) (setq depth (1+ depth)))
              ((eq c close) (setq depth (1- depth))
               (when (= depth 0) (setq end (1+ i))))))
      (setq i (1+ i)))
    end))

(defun ygg-ice--inline-link-end (s i)
  "Index after the inline link whose text opens at I in S, else nil."
  (when-let* ((text-end (ygg-ice--bracket-end s i ?\[ ?\]))
              ((< text-end (length s)))
              ((eq (aref s text-end) ?\()))
    (ygg-ice--bracket-end s text-end ?\( ?\))))

(defun ygg-ice--delimiter (s i)
  "The emphasis run at I in S as (delim CHAR COUNT OPEN CLOSE)."
  (let* ((char (aref s i))
         (end (let ((j i)) (while (and (< j (length s)) (eq (aref s j) char)) (setq j (1+ j))) j))
         (before (and (> i 0) (aref s (1- i))))
         (after (and (< end (length s)) (aref s end)))
         (left (and (not (ygg-ice--space-p after))
                    (or (not (ygg-ice--punct-p after))
                        (ygg-ice--space-p before) (ygg-ice--punct-p before))))
         (right (and (not (ygg-ice--space-p before))
                     (or (not (ygg-ice--punct-p before))
                         (ygg-ice--space-p after) (ygg-ice--punct-p after)))))
    (list 'delim char (- end i)
          (if (eq char ?*) left (and left (or (not right) (ygg-ice--punct-p before))))
          (if (eq char ?*) right (and right (or (not left) (ygg-ice--punct-p after)))))))

(defun ygg-ice--inline-tokens (s)
  "S as (text STRING) and (delim CHAR COUNT OPEN CLOSE); code, links, html gone."
  (let ((i 0) (n (length s)) tokens)
    (while (< i n)
      (let ((c (aref s i)) (rest (substring s i)))
        (cond
         ((and (eq c ?\\) (< (1+ i) n) (ygg-ice--punct-p (aref s (1+ i)))
               (< (aref s (1+ i)) 128))
          (push (list 'text (string (aref s (1+ i)))) tokens)
          (setq i (+ i 2)))
         ((eq c ?`)
          (let* ((run (progn (string-match "\\``+" rest) (match-end 0)))
                 (fence (make-string run ?`))
                 (close (string-match (concat "\\(?:[^`]\\|\\`\\)\\(" fence "\\)\\(?:[^`]\\|\\'\\)")
                                      s (+ i run))))
            (if close
                (setq i (match-end 1))
              (push (list 'text fence) tokens)
              (setq i (+ i run)))))
         ((string-match "\\`\\[\\[[^]\n|]*?[^]\n| \t][^]\n]*?\\]\\]" rest)
          (setq i (+ i (match-end 0))))
         ((and (eq c ?!) (string-prefix-p "![" rest) (ygg-ice--inline-link-end s (1+ i)))
          (setq i (ygg-ice--inline-link-end s (1+ i))))
         ((and (eq c ?\[) (ygg-ice--inline-link-end s i))
          (setq i (ygg-ice--inline-link-end s i)))
         ((string-match "\\`<\\(?:[A-Za-z][A-Za-z0-9+.-]\\{1,31\\}:[^ <>\n]*\\|[^ <>@\n]+@[^ <>\n]+\\|/?[A-Za-z][A-Za-z0-9-]*\\(?:[ \t\n][^<>]*\\)?/?\\|!--.*?--\\)>" rest)
          (setq i (+ i (match-end 0))))
         ((string-match "\\`&\\(?:#\\([0-9]\\{1,7\\}\\)\\|#[xX]\\([0-9a-fA-F]\\{1,6\\}\\)\\|\\([A-Za-z][A-Za-z0-9]\\{1,31\\}\\)\\);" rest)
          (let ((decoded (cond ((match-string 1 rest) (string (string-to-number (match-string 1 rest))))
                               ((match-string 2 rest) (string (string-to-number (match-string 2 rest) 16)))
                               (t (ygg-ice--entity (match-string 3 rest))))))
            (push (list 'text (or decoded (match-string 0 rest))) tokens)
            (setq i (+ i (match-end 0)))))
         ((memq c '(?* ?_))
          (let ((d (ygg-ice--delimiter s i)))
            (push d tokens)
            (setq i (+ i (nth 2 d)))))
         (t (push (list 'text (string c)) tokens)
            (setq i (1+ i))))))
    (nreverse tokens)))

(defun ygg-ice--heading-text (title)
  "TITLE as lat names its section: only the heading's own text nodes.
Code, emphasis, strong, links and wiki links go whole, spaces around them kept."
  (let* ((tokens (vconcat (mapcar #'copy-sequence (ygg-ice--inline-tokens title))))
         (dropped (make-bool-vector (length tokens) nil))
         (openers nil))
    (dotimes (k (length tokens))
      (let ((tok (aref tokens k)))
        (when (eq (car tok) 'delim)
          (let ((searching (nth 4 tok)))
            (while (and searching (> (nth 2 tok) 0))
              (let ((opener (seq-find
                             (lambda (o)
                               (let ((ot (aref tokens o)))
                                 (and (eq (nth 1 ot) (nth 1 tok)) (> (nth 2 ot) 0)
                                      (not (and (or (nth 4 ot) (nth 3 tok))
                                                (= 0 (% (+ (nth 2 ot) (nth 2 tok)) 3))
                                                (not (and (= 0 (% (nth 2 ot) 3))
                                                          (= 0 (% (nth 2 tok) 3)))))))))
                             openers)))
                (if (not opener)
                    (setq searching nil)
                  (let* ((ot (aref tokens opener))
                         (use (if (and (>= (nth 2 ot) 2) (>= (nth 2 tok) 2)) 2 1)))
                    (cl-loop for m from (1+ opener) below k do (aset dropped m t))
                    (setq openers (seq-filter (lambda (o) (<= o opener)) openers))
                    (setf (nth 2 ot) (- (nth 2 ot) use)
                          (nth 2 tok) (- (nth 2 tok) use))
                    (when (= 0 (nth 2 ot)) (setq openers (delq opener openers)))))))
            (when (and (nth 3 tok) (> (nth 2 tok) 0))
              (push k openers))))))
    (mapconcat (lambda (k)
                 (let ((tok (aref tokens k)))
                   (cond ((aref dropped k) "")
                         ((eq (car tok) 'text) (nth 1 tok))
                         (t (make-string (nth 2 tok) (nth 1 tok))))))
               (number-sequence 0 (1- (length tokens))) "")))

(defun ygg-ice--parse-headings (file stem)
  "FILE's sections as plists, ids under STEM the way lat writes them."
  (let (stack out (fence nil) (n 0))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (setq n (1+ n))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))
          (cond
           ((string-match-p "\\`[ \t]*\\(?:```\\|~~~\\)" line) (setq fence (not fence)))
           ((and (not fence)
                 (string-match "\\` \\{0,3\\}\\(#\\{1,6\\}\\)\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*\\'" line))
            (let* ((level (length (match-string 1 line)))
                   (title (replace-regexp-in-string "\\(?:\\`\\|[ \t]+\\)#+[ \t]*\\'" ""
                                                    (or (match-string 2 line) ""))))
              (while (and stack (>= (caar stack) level)) (pop stack))
              (push (cons level (ygg-ice--heading-text title)) stack)
              (let ((path (mapcar #'cdr (reverse stack))))
                (push (list :id (concat stem "#" (string-join path "#"))
                            :heading title :level level :path path
                            :file file :line n :end nil :first nil :links nil)
                      out))))
           ((and (not fence) out)
            (let ((sec (car out)) (start 0))
              (when (and (null (plist-get sec :first)) (not (string-blank-p line)))
                (plist-put sec :first (string-trim line)))
              (while (string-match ygg-ice--link-re line start)
                (setq start (match-end 0))
                (plist-put sec :links (cons (cons (match-string 1 line) n)
                                            (plist-get sec :links))))))))
        (forward-line 1)))
    (let ((secs (nreverse out)))
      (cl-loop for (sec . rest) on secs
               do (plist-put sec :links (nreverse (plist-get sec :links)))
               (plist-put sec :end
                          (let ((next (seq-find (lambda (s) (<= (plist-get s :level)
                                                                (plist-get sec :level)))
                                                rest)))
                            (if next (1- (plist-get next :line)) n))))
      secs)))

(defvar ygg-ice--sections-cache (make-hash-table :test #'equal)
  "Root to (SIGNATURE . SECTIONS), read again when a lat.md file changes.")

(defun ygg-ice-sections (root)
  "Every section in ROOT's lat.md, in file order."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (sig (ygg-ice--lat-signature root))
         (hit (gethash root ygg-ice--sections-cache)))
    (if (and hit (equal (car hit) sig))
        (cdr hit)
      (let ((secs (mapcan (lambda (f)
                            (ygg-ice--parse-headings
                             f (file-name-sans-extension (file-relative-name f root))))
                          (mapcar #'car sig))))
        (puthash root (cons sig secs) ygg-ice--sections-cache)
        secs))))

(defun ygg-ice--stem-p (section head)
  "Non-nil when HEAD names SECTION's file the way lat accepts it."
  (let ((stem (car (split-string (plist-get section :id) "#"))))
    (seq-some (lambda (s) (string-equal-ignore-case s head))
              (list stem (string-remove-prefix "lat.md/" stem)
                    (file-name-nondirectory stem)))))

(defun ygg-ice--suffix-p (parts path)
  (and parts (<= (length parts) (length path))
       (cl-every #'string-equal-ignore-case parts (last path (length parts)))))

(defun ygg-ice-section (root target)
  "The section wiki link TARGET names in ROOT's lat.md, or nil."
  (let* ((secs (ygg-ice-sections root))
         (parts (split-string target "#"))
         (in-file (seq-filter (lambda (s) (ygg-ice--stem-p s (car parts))) secs)))
    (cond ((and in-file (cdr parts))
           (seq-find (lambda (s) (ygg-ice--suffix-p (cdr parts) (plist-get s :path)))
                     in-file))
          (in-file (car in-file))
          (t (seq-find (lambda (s) (ygg-ice--suffix-p parts (plist-get s :path))) secs)))))

(defun ygg-ice--section-by-id (root id)
  (seq-find (lambda (s) (equal (plist-get s :id) id)) (ygg-ice-sections root)))

(defun ygg-ice--symbol-line (file symbol)
  "The line FILE defines SYMBOL on, else the first line naming it, else 1."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((sym (concat "\\_<" (regexp-quote symbol) "\\_>")))
      (goto-char (point-min))
      (if (or (re-search-forward
               (concat "\\_<\\(?:function\\|def\\|defun\\|defvar\\|defmacro\\|class\\|const\\|let\\|var\\|type\\|interface\\|enum\\|struct\\|fn\\|func\\|val\\|fun\\)\\_>[^\n]*?"
                       sym)
               nil t)
              (progn (goto-char (point-min)) (re-search-forward sym nil t)))
          (line-number-at-pos (match-beginning 0))
        1))))

(defun ygg-ice--code-target (root target)
  "(:file :line) for a link into code such as src/x.ts#handle, else nil."
  (let* ((parts (split-string target "#"))
         (file (expand-file-name (car parts) root)))
    (when (and (file-name-extension (car parts))
               (not (string-equal-ignore-case (file-name-extension (car parts)) "md"))
               (file-regular-p file))
      (list :file file :id target
            :line (if (cdr parts) (ygg-ice--symbol-line file (car (last parts))) 1)))))

;;; lat, run

(defvar ygg-ice--lat-memo (make-hash-table :test #'equal)
  "(ROOT ARGS) to (SIGNATURE TIME . OUTPUT) of lookups that had to answer at once.")

(defun ygg-ice--lat-command (args)
  (when-let* ((lat (ygg-ice--program ygg-ice-lat-program)))
    (append (list lat "--no-color") args)))

(defun ygg-ice--lat-async (root args callback)
  "Run lat ARGS in ROOT and call CALLBACK with its output and exit status."
  (let ((command (or (ygg-ice--lat-command args)
                     (user-error "ice: lat is not installed")))
        (buf (generate-new-buffer " *ice-lat*"))
        (default-directory root))
    (make-process
     :name "ice-lat" :buffer buf :command command :noquery t
     :connection-type 'pipe
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (let ((out (with-current-buffer buf (buffer-string))))
                     (kill-buffer buf)
                     (funcall callback out (process-exit-status proc))))))))

(defun ygg-ice--lat-sync (root &rest args)
  "lat ARGS's output in ROOT, waited for up to ygg-ice-lat-timeout seconds.
Kept until lat.md changes or a minute passes, since code refs move too."
  (let* ((key (list root args))
         (sig (ygg-ice--lat-signature root))
         (hit (gethash key ygg-ice--lat-memo)))
    (if (and hit (equal (car hit) sig) (< (- (float-time) (cadr hit)) 60))
        (cddr hit)
      (when-let* ((command (ygg-ice--lat-command args)))
        (let* ((buf (generate-new-buffer " *ice-lat*"))
               (default-directory root)
               (proc nil)
               (deadline (+ (float-time) ygg-ice-lat-timeout)))
          (unwind-protect
              (progn
                (setq proc (make-process :name "ice-lat" :buffer buf :command command
                                         :noquery t :connection-type 'pipe))
                (while (and (process-live-p proc) (< (float-time) deadline))
                  (accept-process-output proc 0.05))
                (unless (process-live-p proc)
                  (accept-process-output proc 0)
                  (let ((out (with-current-buffer buf (buffer-string))))
                    (puthash key (cons sig (cons (float-time) out)) ygg-ice--lat-memo)
                    out)))
            (when (and proc (process-live-p proc)) (delete-process proc))
            (kill-buffer buf)))))))

(defun ygg-ice--parse-lat-sections (out root)
  "The sections lat's OUT lists, as (:id :file :line :text), files under ROOT."
  (let ((start 0) hits)
    (while (string-match "^\\* Section: \\[\\[\\([^]\n]+\\)\\]\\][^\n]*\n[ \t]+Defined in \\([^:\n]+\\):\\([0-9]+\\)"
                         out start)
      (let* ((id (match-string 1 out))
             (file (expand-file-name (match-string 2 out) root))
             (line (string-to-number (match-string 3 out)))
             (from (match-end 0))
             (to (or (string-match "^\\(?:\\* \\|## \\)" out from) (length out)))
             (block (substring out from to)))
        (setq start to)
        (push (list :id id :file file :line line
                    :text (and (string-match "^[ \t]*> \\(.*\\)$" block)
                               (match-string 1 block)))
              hits)))
    (nreverse hits)))

(defun ygg-ice--parse-lat-refs (out root)
  "lat refs OUT as (SECTIONS . CODE), code refs as (:file :line :text)."
  (let* ((cut (string-match "^## Code references:" out))
         (sections (ygg-ice--parse-lat-sections (substring out 0 cut) root))
         (code nil))
    (when cut
      (let ((start cut))
        (while (string-match "^\\* \\([^:\n]+\\):\\([0-9]+\\)" out start)
          (setq start (match-end 0))
          (push (list :file (expand-file-name (match-string 1 out) root)
                      :line (string-to-number (match-string 2 out))
                      :text (match-string 1 out))
                code))))
    (cons sections (nreverse code))))

(defun ygg-ice--resolve-here (root target)
  "Where wiki link TARGET points in ROOT by reading alone, or nil."
  (or (when-let* ((sec (ygg-ice-section root target)))
        (list :file (plist-get sec :file) :line (plist-get sec :line)
              :id (plist-get sec :id)))
      (ygg-ice--code-target root target)))

(defun ygg-ice--located (out root)
  "The first section lat locate's OUT names, as (:file :line :id), or nil."
  (when-let* ((hit (car (ygg-ice--parse-lat-sections out root))))
    (list :file (plist-get hit :file) :line (plist-get hit :line)
          :id (plist-get hit :id))))

(defun ygg-ice-resolve (root target)
  "Where wiki link TARGET points in ROOT, as (:file :line :id), or nil.
Sections are read here; lat locate is asked only when that finds none."
  (or (ygg-ice--resolve-here root target)
      (when-let* ((out (ygg-ice--lat-sync root "locate" target)))
        (ygg-ice--located out root))))

(defun ygg-ice-refs (root id)
  "lat refs of section ID in ROOT, as (SECTIONS . CODE)."
  (when-let* ((out (ygg-ice--lat-sync root "refs" id)))
    (ygg-ice--parse-lat-refs out root)))

;;; Following links

(defun ygg-ice-link-at-point ()
  "The target of the wiki link point is on, or nil."
  (let ((pos (point)))
    (save-excursion
      (goto-char (line-beginning-position))
      (catch 'found
        (while (re-search-forward ygg-ice--link-re (line-end-position) t)
          (when (and (>= pos (match-beginning 0)) (< pos (match-end 0)))
            (throw 'found (match-string-no-properties 1))))))))

(defun ygg-ice--lat-buffer-p ()
  "Non-nil where wiki links name lat sections: a repo with lat.md, or openspec."
  (when-let* ((file buffer-file-name)
              ((not (file-remote-p file)))
              (root (ygg-ice-root (file-name-directory file))))
    (or (file-directory-p (expand-file-name "lat.md" root))
        (string-prefix-p (expand-file-name "openspec/" root) (expand-file-name file)))))

(defun ygg-ice--visit (file line)
  (when (fboundp 'ygg--jump-push) (ygg--jump-push))
  (find-file file)
  (goto-char (point-min))
  (forward-line (1- (max 1 line))))

(defun ygg-ice-follow (&optional target)
  "Open where the wiki link TARGET, or the one at point, points."
  (interactive)
  (let* ((target (or target (ygg-ice-link-at-point)
                     (user-error "ice: no wiki link at point")))
         (root (ygg-ice-root))
         (to (or (ygg-ice-resolve root target)
                 (user-error "ice: nothing named %s" target))))
    (ygg-ice--visit (plist-get to :file) (plist-get to :line))))

(defun ygg-ice--follow-wiki-link (orig name &optional other)
  "Where lat is in use a wiki link names a lat section; else, or with none, a file."
  (if-let* (((ygg-ice--lat-buffer-p))
            (to (ygg-ice-resolve (ygg-ice-root) name)))
      (ygg-ice--visit (plist-get to :file) (plist-get to :line))
    (funcall orig name other)))

(defun ygg-ice-section-at-point ()
  "The section point is in, in a lat.md file, or the one the link at point names."
  (let ((root (ygg-ice-root)))
    (or (when-let* ((target (ygg-ice-link-at-point))
                    (to (ygg-ice-resolve root target)))
          (ygg-ice--section-by-id root (plist-get to :id)))
        (when-let* ((file buffer-file-name)
                    (file (expand-file-name file))
                    (line (line-number-at-pos)))
          (car (last (seq-filter (lambda (s) (and (equal (plist-get s :file) file)
                                                  (<= (plist-get s :line) line)))
                                 (ygg-ice-sections root))))))))

(defun ygg-ice-read-section (root &optional prompt)
  "A section of ROOT's lat.md, picked by id."
  (let* ((secs (or (ygg-ice-sections root) (user-error "ice: no lat.md here")))
         (id (completing-read (or prompt "Section: ")
                              (mapcar (lambda (s) (plist-get s :id)) secs) nil t)))
    (ygg-ice--section-by-id root id)))

;;; xref: gd and gr in the docs

(defun ygg-ice-xref-backend ()
  "The ICE backend on a wiki link or a lat.md heading, else nobody's turn."
  (and (ygg-ice--lat-buffer-p)
       (or (ygg-ice-link-at-point)
           (and (string-prefix-p (expand-file-name "lat.md/" (ygg-ice-root))
                                 (expand-file-name buffer-file-name))
                (save-excursion (beginning-of-line) (looking-at-p "#+[ \t]"))))
       'ygg-ice))

(cl-defmethod xref-backend-identifier-at-point ((_ (eql ygg-ice)))
  (or (ygg-ice-link-at-point)
      (when-let* ((sec (ygg-ice-section-at-point))) (plist-get sec :id))))

(cl-defmethod xref-backend-identifier-completion-table ((_ (eql ygg-ice)))
  (mapcar (lambda (s) (plist-get s :id)) (ygg-ice-sections (ygg-ice-root))))

(cl-defmethod xref-backend-definitions ((_ (eql ygg-ice)) identifier)
  (when-let* ((to (ygg-ice-resolve (ygg-ice-root) identifier)))
    (list (xref-make (or (plist-get to :id) identifier)
                     (xref-make-file-location (plist-get to :file) (plist-get to :line) 0)))))

(cl-defmethod xref-backend-references ((_ (eql ygg-ice)) identifier)
  (let* ((root (ygg-ice-root))
         (id (or (plist-get (ygg-ice-resolve root identifier) :id) identifier))
         (refs (ygg-ice-refs root id)))
    (append
     (mapcar (lambda (h) (xref-make (format "%s  %s" (plist-get h :id) (or (plist-get h :text) ""))
                                    (xref-make-file-location (plist-get h :file) (plist-get h :line) 0)))
             (car refs))
     (mapcar (lambda (c) (xref-make (format "@lat %s" (plist-get c :text))
                                    (xref-make-file-location (plist-get c :file) (plist-get c :line) 0)))
             (cdr refs)))))

(defun ygg-ice--markdown-setup ()
  "Wiki links and gd/gr through lat where lat is in use."
  (when (ygg-ice--lat-buffer-p)
    (setq-local markdown-enable-wiki-links t)
    (setq-local markdown-wiki-link-alias-first nil)
    (add-hook 'xref-backend-functions #'ygg-ice-xref-backend -90 t)))

(add-hook 'markdown-mode-hook #'ygg-ice--markdown-setup)
(add-hook 'gfm-mode-hook #'ygg-ice--markdown-setup)
(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-follow-wiki-link :around #'ygg-ice--follow-wiki-link))

;;; OpenSpec

(defun ygg-ice--removed-slices (dir)
  "How many slices of the change in DIR ice-verify --done removed, per the ledger."
  (let ((ledger (expand-file-name "../../../.ice/ledger.tsv" (file-name-as-directory dir)))
        (name (file-name-nondirectory (directory-file-name dir)))
        slices)
    (when (file-readable-p ledger)
      (with-temp-buffer
        (insert-file-contents ledger)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let ((row (split-string line "\t")))
            (when (and (equal (nth 1 row) name) (equal (nth 4 row) "slice-done") (nth 7 row))
              (cl-pushnew (nth 7 row) slices :test #'equal))))))
    (length slices)))

(defun ygg-ice-changes (root)
  "ROOT's open OpenSpec changes as (:name :dir :tasks :done :total)."
  (let ((dir (expand-file-name "openspec/changes" root)))
    (when (file-directory-p dir)
      (delq nil
            (mapcar (lambda (d)
                      (when (and (file-directory-p d)
                                 (not (equal (file-name-nondirectory d) "archive")))
                        (let* ((tasks (expand-file-name "tasks.md" d))
                               (tasks (and (file-exists-p tasks) tasks))
                               (removed (ygg-ice--removed-slices d))
                               (progress (if tasks (ygg-todo-progress tasks) '(0 . 0))))
                          (list :name (file-name-nondirectory d)
                                :dir (file-name-as-directory d) :tasks tasks
                                :done (+ removed (car progress)) :total (+ removed (cdr progress))))))
                    (directory-files dir t "\\`[^.]"))))))

(defun ygg-ice--change-files (change)
  (let ((dir (plist-get change :dir)))
    (mapcar (lambda (f) (file-relative-name f dir))
            (sort (directory-files-recursively dir "\\.md\\'") #'string<))))

(defun ygg-ice--change-place (change)
  "The file standing for CHANGE: its tasks, else its intent, else its proposal."
  (let ((dir (plist-get change :dir)))
    (or (plist-get change :tasks)
        (seq-find #'file-exists-p (mapcar (lambda (f) (expand-file-name f dir))
                                          '("intent.md" "proposal.md")))
        dir)))

(defun ygg-ice-read-change (root &optional prompt)
  "An open change of ROOT, picked by name, its progress beside it."
  (let* ((changes (or (ygg-ice-changes root) (user-error "ice: no open changes here")))
         (completion-extra-properties
          (list :annotation-function
                (lambda (name)
                  (when-let* ((c (seq-find (lambda (c) (equal (plist-get c :name) name)) changes)))
                    (format "  %d/%d" (plist-get c :done) (plist-get c :total))))))
         (name (completing-read (or prompt "Change: ")
                                (mapcar (lambda (c) (plist-get c :name)) changes) nil t)))
    (seq-find (lambda (c) (equal (plist-get c :name) name)) changes)))

(defun ygg-ice-open-change (change)
  "Open one of CHANGE's files, intent and proposal offered first."
  (interactive (list (ygg-ice-read-change (ygg-ice-root))))
  (let* ((files (ygg-ice--change-files change))
         (first (seq-filter (lambda (f) (member f files)) '("intent.md" "proposal.md" "tasks.md")))
         (pick (if (cdr files)
                   (completing-read (format "%s: " (plist-get change :name))
                                    (append first (seq-difference files first)) nil t)
                 (car files))))
    (if pick
        (find-file (expand-file-name pick (plist-get change :dir)))
      (dired (plist-get change :dir)))))

;;; Views: plain text trees, rows carrying what they stand for

(defvar-local ygg-ice--root nil)
(defvar-local ygg-ice--redraw nil)

(defun ygg-ice-view-refresh ()
  "Draw this view again from what is on disk."
  (interactive)
  (when ygg-ice--redraw (funcall ygg-ice--redraw)))

(defun ygg-ice--item-at (pos)
  (get-text-property pos 'ygg-ice-item))

(defun ygg-ice-view-visit ()
  "Open what the row under point stands for."
  (interactive)
  (let ((item (or (ygg-ice--item-at (line-beginning-position))
                  (user-error "ice: nothing on this line"))))
    (ygg-ice-visit-item item)))

(defun ygg-ice-visit-item (item)
  "Open ITEM: a change through its files, anything else at its line."
  (pcase (plist-get item :kind)
    ('change (ygg-ice-open-change (plist-get item :change)))
    ('task (ygg-ice--visit (plist-get item :file) (plist-get item :line)))
    (_ (let ((file (plist-get item :file)))
         (if (file-directory-p file) (dired file)
           (ygg-ice--visit file (or (plist-get item :line) 1)))))))

(defun ygg-ice--selected-items ()
  "The items of the rows a visual selection holds, else of the row at point."
  (let* ((visual (and (bound-and-true-p ygg--visual-p) (mark t)))
         (beg (if visual (min (point) (mark t)) (point)))
         (end (if visual (max (point) (mark t)) (point)))
         items)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (and (<= (point) end) (not (eobp)))
        (when-let* ((item (ygg-ice--item-at (point))))
          (unless (member item items) (push item items)))
        (forward-line 1)))
    (when (and visual (fboundp 'ygg-normal-state)) (ygg-normal-state))
    (nreverse items)))

(defun ygg-ice--heading-end (file line)
  "The last line of the heading's section at LINE of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((level (and (looking-at "\\(#+\\)[ \t]") (length (match-string 1))))
          (fence nil))
      (if (not level)
          line
        (forward-line 1)
        (catch 'end
          (while (not (eobp))
            (cond ((looking-at-p "[ \t]*\\(?:```\\|~~~\\)") (setq fence (not fence)))
                  ((and (not fence) (looking-at "\\(#+\\)[ \t]")
                        (<= (length (match-string 1)) level))
                   (throw 'end (1- (line-number-at-pos)))))
            (forward-line 1))
          (goto-char (point-max))
          (if (and (bolp) (not (bobp)))
              (1- (line-number-at-pos))
            (line-number-at-pos)))))))

(defun ygg-ice-item-location (item)
  "ITEM's place as (FILE . LINE)."
  (cons (plist-get item :file) (or (plist-get item :line) 1)))

(defun ygg-ice-send-quickfix (items)
  "Put ITEMS in the quickfix, a row each at its file and line."
  (let ((rows (mapconcat (lambda (it)
                           (let ((loc (ygg-ice-item-location it)))
                             (format "%s:%d: %s %s" (car loc) (cdr loc)
                                     (plist-get it :kind) (plist-get it :label))))
                         (seq-filter (lambda (it) (file-regular-p (plist-get it :file))) items)
                         "\n")))
    (unless (fboundp 'ygg-qf-from-text) (require 'layer-quickfix nil t))
    (let ((n (if (string-empty-p rows) 0 (ygg-qf-from-text rows "ice"))))
      (message "quickfix: %d row%s from ice" n (if (= n 1) "" "s"))
      n)))

(defun ygg-ice-context-entry (item)
  "ITEM as an aob context entry: a section's own text, else the whole file."
  (let ((file (plist-get item :file)))
    (when (file-regular-p file)
      (if (plist-get item :section)
          (let* ((beg (plist-get item :line))
                 (end (ygg-ice--heading-end file beg)))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (forward-line (1- beg))
              (let ((from (point)))
                (forward-line (1+ (- end beg)))
                (list :file file :text (buffer-substring-no-properties from (point))
                      :beg beg :end end))))
        (list :file file
              :text (with-temp-buffer (insert-file-contents file) (buffer-string)))))))

(defun ygg-ice-send-context (items)
  "Add ITEMS to the agent context, the list SPC a X shows."
  (unless (boundp 'aob-context--items) (require 'aob-context nil t))
  (let ((entries (delq nil (mapcar #'ygg-ice-context-entry items))))
    (dolist (e entries) (push e aob-context--items))
    (when-let* ((buf (get-buffer "*aob-context*")))
      (with-current-buffer buf (aob-context--render)))
    (message "aob context: %d from ice (%d entries)" (length entries)
             (length aob-context--items))
    (length entries)))

(defun ygg-ice-view-quickfix ()
  "Send the selected rows, or the row at point, to the quickfix."
  (interactive)
  (ygg-ice-send-quickfix (ygg-ice--selected-items)))

(defun ygg-ice-view-context ()
  "Add the selected rows, or the row at point, to the agent context."
  (interactive)
  (ygg-ice-send-context (ygg-ice--selected-items)))

(defvar ygg-ice-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ygg-ice-view-visit)
    (define-key map (kbd "<return>") #'ygg-ice-view-visit)
    (define-key map "Q" #'ygg-ice-view-quickfix)
    (define-key map "c" #'ygg-ice-view-context)
    (define-key map "o" #'ygg-ice-view-visit)
    (define-key map (kbd "g r") #'ygg-ice-view-refresh)
    (define-key map "q" #'quit-window)
    map))

(define-derived-mode ygg-ice-view-mode special-mode "ICE"
  "An ICE view: RET opens a row, Q sends rows to the quickfix,
c to the agent context."
  (setq truncate-lines t)
  (add-hook 'ygg-visual-entry-hook #'ygg-ice--anchor nil t))

(defun ygg-ice--anchor ()
  "Begin a selection of rows on the row it was started from."
  (set-marker (mark-marker) (line-beginning-position)))

(defun ygg-ice--line (text item &optional note)
  "TEXT as a row standing for ITEM, NOTE in grey after it."
  (insert (propertize (concat text (if note (concat "  " (propertize note 'font-lock-face 'ygg-ice-dim)) "")
                              "\n")
                      'ygg-ice-item item)))

(defun ygg-ice--group (title rows)
  "A heading TITLE and ROWS under it on a rail; ROWS are (TEXT ITEM NOTE)."
  (insert (propertize title 'font-lock-face 'ygg-ice-heading) "\n")
  (if (eq rows 'pending)
      (insert (propertize "  └ …\n" 'font-lock-face 'ygg-ice-dim))
    (if (null rows)
        (insert (propertize "  └ none\n" 'font-lock-face 'ygg-ice-dim))
      (cl-loop for (row . more) on rows
               do (ygg-ice--line (concat (propertize (if more "  ├ " "  └ ")
                                                     'font-lock-face 'ygg-ice-dim)
                                         (nth 0 row))
                                 (nth 1 row) (nth 2 row)))))
  (insert "\n"))

(defun ygg-ice--view (name root redraw)
  "The view buffer NAME for ROOT, drawn by REDRAW."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ygg-ice-view-mode) (ygg-ice-view-mode))
      (setq default-directory root ygg-ice--root root ygg-ice--redraw redraw)
      (funcall redraw))
    buf))

(defmacro ygg-ice--drawing (&rest body)
  "Run BODY on an emptied view, point kept on its line."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t) (line (line-number-at-pos)))
     (erase-buffer)
     ,@body
     (goto-char (point-min))
     (forward-line (1- line))))

;;; Connections

(defun ygg-ice--rel (root file)
  (file-relative-name file root))

(defun ygg-ice--changes-linking (root id)
  "Places in open changes whose wiki links name section ID."
  (mapcan
   (lambda (change)
     (mapcan
      (lambda (file)
        (let (hits (n 0))
          (with-temp-buffer
            (insert-file-contents file)
            (dolist (line (split-string (buffer-string) "\n"))
              (setq n (1+ n))
              (let ((start 0))
                (while (string-match ygg-ice--link-re line start)
                  (setq start (match-end 0))
                  (when (equal (plist-get (ygg-ice-section root (match-string 1 line)) :id) id)
                    (push (list :kind 'change-link :change (plist-get change :name)
                                :file file :line n
                                :label (plist-get change :name))
                          hits))))))
          (nreverse hits)))
      (directory-files-recursively (plist-get change :dir) "\\.md\\'")))
   (ygg-ice-changes root)))

(defconst ygg-ice--c4-owner-re
  "^[ \t]*\\(?:\\(?:dynamic \\|deployment \\)?view[ \t]+\\([[:alnum:]_.-]+\\)\\|\\([[:alnum:]_]+\\)[ \t]*=[ \t]*[[:alnum:]_]+\\)"
  "A C4 view or element being defined; group 1 or 2 is its name.")

(defun ygg-ice--c4-files (root)
  (when-let* ((arch (ygg-ice--arch-dir root)))
    (sort (directory-files-recursively arch "\\.\\(?:c4\\|likec4\\)\\'") #'string<)))

(defun ygg-ice--c4-linking (root section)
  "C4 lines that point at SECTION: its file by path, or a wiki link to it."
  (let ((rel (file-relative-name (plist-get section :file) root))
        (id (plist-get section :id))
        hits)
    (dolist (file (ygg-ice--c4-files root))
      (let ((n 0) owner)
        (with-temp-buffer
          (insert-file-contents file)
          (dolist (line (split-string (buffer-string) "\n"))
            (setq n (1+ n))
            (when (string-match ygg-ice--c4-owner-re line)
              (setq owner (or (match-string 1 line) (match-string 2 line))))
            (when (or (string-match-p (regexp-quote rel) line)
                      (and (string-match ygg-ice--link-re line)
                           (equal (plist-get (ygg-ice-section root (match-string 1 line)) :id) id)))
              (push (list :kind 'c4 :file file :line n :label (or owner (string-trim line)))
                    hits))))))
    (nreverse hits)))

(defun ygg-ice-connections-data (root id &optional refs located)
  "What section ID of ROOT connects to, as a plist of item lists.
REFS is lat refs parsed, (SECTIONS . CODE); without it incoming and
code are nil.  LOCATED maps a link only lat can place to where lat
put it; lat is never run from here."
  (let ((sec (or (ygg-ice--section-by-id root id) (error "ice: no section %s" id))))
    (list
     :section sec
     :outgoing (mapcar (lambda (link)
                         (let ((to (or (ygg-ice--resolve-here root (car link))
                                       (and located (gethash (car link) located)))))
                           (list :kind (if to 'section 'broken) :label (car link)
                                 :id (plist-get to :id)
                                 :file (or (plist-get to :file) (plist-get sec :file))
                                 :line (or (plist-get to :line) (cdr link))
                                 :section (and to (not (ygg-ice--code-target root (car link)))))))
                       (plist-get sec :links))
     :incoming (mapcar (lambda (h) (list :kind 'section :label (plist-get h :id) :id (plist-get h :id)
                                         :file (plist-get h :file) :line (plist-get h :line)
                                         :section t))
                       (car refs))
     :code (mapcar (lambda (c) (list :kind 'code :label (plist-get c :text)
                                     :file (plist-get c :file) :line (plist-get c :line)))
                   (cdr refs))
     :changes (ygg-ice--changes-linking root id)
     :views (ygg-ice--c4-linking root sec))))

(defun ygg-ice--draw-connections (root id refs &optional located)
  (let* ((data (ygg-ice-connections-data root id (unless (eq refs 'pending) refs) located))
         (sec (plist-get data :section))
         (row (lambda (it) (list (plist-get it :label) it
                                 (format "%s:%d" (ygg-ice--rel root (plist-get it :file))
                                         (plist-get it :line))))))
    (ygg-ice--drawing
      (ygg-ice--line (propertize (plist-get sec :heading) 'font-lock-face 'ygg-ice-heading)
                     (list :kind 'section :label id :id id :file (plist-get sec :file)
                           :line (plist-get sec :line) :section t)
                     (format "%s:%d" (ygg-ice--rel root (plist-get sec :file)) (plist-get sec :line)))
      (insert (propertize (concat id "\n\n") 'font-lock-face 'ygg-ice-dim))
      (ygg-ice--group "links to" (mapcar row (plist-get data :outgoing)))
      (ygg-ice--group "linked from" (if (eq refs 'pending) 'pending
                                      (mapcar row (plist-get data :incoming))))
      (ygg-ice--group "code" (if (eq refs 'pending) 'pending
                               (mapcar row (plist-get data :code))))
      (ygg-ice--group "changes" (mapcar row (plist-get data :changes)))
      (ygg-ice--group "c4" (mapcar row (plist-get data :views))))))

(defun ygg-ice-connections (section)
  "Show what SECTION links to, what links to it, and what else names it.
Code, changes and C4 views are that else.  At a heading or link in
lat.md that section; elsewhere, or with a prefix, one picked."
  (interactive
   (list (or (and (not current-prefix-arg) (ygg-ice-section-at-point))
             (ygg-ice-read-section (ygg-ice-root) "Connections of: "))))
  (let* ((root (ygg-ice-root))
         (id (plist-get section :id))
         (refs 'pending)
         (located (make-hash-table :test #'equal))
         (buf nil))
    (setq buf (ygg-ice--view (format "*ice: %s*" id) root
                             (lambda () (ygg-ice--draw-connections root id refs located))))
    (when (ygg-ice--lat-command nil)
      (dolist (target (seq-uniq (mapcar #'car (plist-get (ygg-ice--section-by-id root id) :links))))
        (unless (ygg-ice--resolve-here root target)
          (ygg-ice--lat-async root (list "locate" target)
                              (lambda (out _status)
                                (when-let* ((to (ygg-ice--located out root)))
                                  (puthash target to located)
                                  (when (buffer-live-p buf)
                                    (with-current-buffer buf (ygg-ice-view-refresh)))))))))
    (if (ygg-ice--lat-command nil)
        (ygg-ice--lat-async root (list "refs" id)
                            (lambda (out _status)
                              (setq refs (ygg-ice--parse-lat-refs out root))
                              (when (buffer-live-p buf)
                                (with-current-buffer buf (ygg-ice-view-refresh)))))
      (setq refs nil)
      (with-current-buffer buf (ygg-ice-view-refresh)))
    (pop-to-buffer buf)))

(defun ygg-ice-connections-pick ()
  "Show the connections of a section picked by id."
  (interactive)
  (ygg-ice-connections (ygg-ice-read-section (ygg-ice-root) "Connections of: ")))

;;; Changes and their tasks

(defun ygg-ice--change-item (root change)
  (list :kind 'change :label (plist-get change :name) :change change
        :file (ygg-ice--change-place change) :line 1 :root root))

(defun ygg-ice-changes-list ()
  "List the open OpenSpec changes with their task progress."
  (interactive)
  (let ((root (ygg-ice-root)))
    (pop-to-buffer
     (ygg-ice--view
      "*ice: changes*" root
      (lambda ()
        (ygg-ice--drawing
          (insert (propertize "openspec changes" 'font-lock-face 'ygg-ice-heading)
                  (propertize (concat "  " (abbreviate-file-name root)) 'font-lock-face 'ygg-ice-dim)
                  "\n\n")
          (let ((changes (ygg-ice-changes root)))
            (if (null changes)
                (insert (propertize "  none open\n" 'font-lock-face 'ygg-ice-dim))
              (let ((width (apply #'max (mapcar (lambda (c) (length (plist-get c :name))) changes))))
                (dolist (c changes)
                  (ygg-ice--line (concat "  " (string-pad (plist-get c :name) width))
                                 (ygg-ice--change-item root c)
                                 (if (plist-get c :tasks)
                                     (format "%d/%d" (plist-get c :done) (plist-get c :total))
                                   "no tasks"))))))
          (insert "\n" (propertize "RET open  t tasks  Q quickfix  c context"
                                   'font-lock-face 'ygg-ice-dim) "\n")))))))

(defun ygg-ice-view-tasks ()
  "The task list of the change on this row."
  (interactive)
  (let ((item (ygg-ice--item-at (line-beginning-position))))
    (if (eq (plist-get item :kind) 'change)
        (ygg-ice-tasks (plist-get item :change))
      (call-interactively #'ygg-ice-tasks))))

(defun ygg-ice-tasks (change)
  "CHANGE's tasks.md as a list with ticks; RET goes to the task's line."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Tasks of: ")))
  (let ((file (or (plist-get change :tasks)
                  (user-error "ice: %s has no tasks.md" (plist-get change :name))))
        (root (ygg-ice-root (plist-get change :dir))))
    (pop-to-buffer
     (ygg-ice--view
      (format "*ice: tasks %s*" (plist-get change :name)) root
      (lambda ()
        (let* ((list (ygg-todo-read file))
               (items (plist-get list :items))
               (removed (ygg-ice--removed-slices (plist-get change :dir)))
               (done (+ removed (seq-count (lambda (it) (plist-get it :done)) items)))
               section)
          (ygg-ice--drawing
            (insert (propertize (plist-get change :name) 'font-lock-face 'ygg-ice-heading)
                    (propertize (format "  %d/%d" done (+ removed (length items))) 'font-lock-face 'ygg-ice-dim)
                    "\n")
            (dolist (it items)
              (unless (equal (plist-get it :section) section)
                (setq section (plist-get it :section))
                (insert "\n" (propertize (or section "") 'font-lock-face 'ygg-ice-heading) "\n"))
              (ygg-ice--line (concat (if (plist-get it :done) "  [x] " "  [ ] ")
                                     (plist-get it :text))
                             (list :kind 'task :label (plist-get it :text)
                                   :file file :line (plist-get it :line)))))))))))

(define-key ygg-ice-view-mode-map "t" #'ygg-ice-view-tasks)

;;; ADRs, glossary, C4

(defun ygg-ice-adrs (root)
  "ROOT's ADRs, adr/NNNN-*.md in its docs folder, as items."
  (let ((dir (ygg-ice--adr-dir root)))
    (when (file-directory-p dir)
      (mapcar (lambda (f)
                (let ((base (file-name-base f)))
                  (list :kind 'adr :file f :line 1 :slug base
                        :label (if (string-match "\\`\\([0-9]+\\)-\\(.*\\)\\'" base)
                                   (concat (match-string 1 base) " "
                                           (string-replace "-" " " (match-string 2 base)))
                                 base))))
              (directory-files dir t "\\`[0-9]+.*\\.md\\'")))))

(defun ygg-ice-glossary (root)
  (let ((file (expand-file-name "CONTEXT.md" root)))
    (and (file-exists-p file)
         (list :kind 'glossary :file file :line 1 :label "CONTEXT.md"))))

(defun ygg-ice-c4-views (root)
  "ROOT's C4 views: README.md's view headings, else the view lines in the model."
  (when-let* ((arch (ygg-ice--arch-dir root)))
    (let ((readme (expand-file-name "README.md" arch)))
      (or (when (file-exists-p readme)
            (with-temp-buffer
              (insert-file-contents readme)
              (let (views)
                (goto-char (point-min))
                (while (re-search-forward "^#\\{2,\\}[ \t]+\\(.*?\\)[ \t]*$" nil t)
                  (let ((title (match-string-no-properties 1))
                        (line (line-number-at-pos)))
                    (when (save-excursion
                            (forward-line 1)
                            (skip-chars-forward " \t\n")
                            (looking-at-p "```mermaid"))
                      (push (list :kind 'c4 :label title :file readme :line line :section t)
                            views))))
                (nreverse views))))
          (mapcan (lambda (file)
                    (let ((n 0) views)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (dolist (line (split-string (buffer-string) "\n"))
                          (setq n (1+ n))
                          (when (string-match "^[ \t]*\\(?:dynamic \\|deployment \\)?view[ \t]+\\([[:alnum:]_.-]+\\)" line)
                            (push (list :kind 'c4 :label (match-string 1 line) :file file :line n)
                                  views))))
                      (nreverse views)))
                  (ygg-ice--c4-files root))))))

(defun ygg-ice-open-adr (root)
  "Open one of ROOT's ADRs."
  (interactive (list (ygg-ice-root)))
  (let* ((adrs (or (ygg-ice-adrs root) (user-error "ice: no ADRs here")))
         (pick (completing-read "ADR: " (mapcar (lambda (a) (plist-get a :label)) adrs) nil t)))
    (find-file (plist-get (seq-find (lambda (a) (equal (plist-get a :label) pick)) adrs) :file))))

(defun ygg-ice-open-glossary (root)
  "Open ROOT's CONTEXT.md."
  (interactive (list (ygg-ice-root)))
  (find-file (or (plist-get (ygg-ice-glossary root) :file)
                 (user-error "ice: no CONTEXT.md here"))))

;;; Running the tools

(defvar ygg-ice-compile-error-regexps
  '((ygg-ice-lat "^- \\([^:\n]+?\\):\\([0-9]+\\): " 1 2)
    (ygg-ice-check "^ice-check \\(intent\\|expect\\|plan\\) \\(.+?\\): \\(?:\\(ok$\\)\\|\\(expect: \\)\\)?"
                   ygg-ice--check-file nil nil (nil . 3)))
  "How lat check and ice-check name a place.")

(defun ygg-ice--check-file ()
  "The file of the change an ice-check line is about.
A plan's expect gaps are in expectations.md."
  (expand-file-name (pcase (match-string 1)
                      ("intent" "intent.md")
                      ("expect" "expectations.md")
                      (_ (if (match-beginning 4) "expectations.md" "tasks.md")))
                    (match-string 2)))

(define-compilation-mode ygg-ice-compile-mode "ICE"
  "Output of the ICE tools, each place they name one next-error away."
  (setq-local compilation-error-regexp-alist-alist
              (append ygg-ice-compile-error-regexps compilation-error-regexp-alist-alist))
  (setq-local compilation-error-regexp-alist
              (append (mapcar #'car ygg-ice-compile-error-regexps) '(gnu)))
  (setq truncate-lines nil word-wrap t))

(defun ygg-ice--compile (root command what &optional codex quiet)
  "Run COMMAND in ROOT in the ICE compilation buffer named for WHAT.
With CODEX, CODEX_HOME points at an empty folder of its own.  QUIET keeps
the buffer out of sight and says in one line how it ended."
  (let ((default-directory root)
        (display-buffer-overriding-action
         (if quiet '(display-buffer-no-window (allow-no-window . t))
           display-buffer-overriding-action))
        (compilation-environment (if codex
                                     (cons (car (ygg-ice--codex-environment))
                                           compilation-environment)
                                   compilation-environment)))
    (let ((buf (compilation-start command 'ygg-ice-compile-mode
                                  (lambda (_) (format "*ice: %s*" what)))))
      (when quiet
        (with-current-buffer buf
          (add-hook 'compilation-finish-functions #'ygg-ice--say-finished nil t)))
      buf)))

(defun ygg-ice--say-finished (buf how)
  "One line on how the hidden ICE run in BUF ended, and where to read it."
  (let ((fresh (with-current-buffer buf
                 (save-excursion
                   (goto-char (point-min))
                   (when (re-search-forward "^Owner decides.*\n" nil t)
                     (let ((n 0))
                       (while (looking-at "- \\[ \\]") (setq n (1+ n)) (forward-line 1))
                       n))))))
    (message "ice: %s %s%s (%s)" (buffer-name buf) (string-trim how)
             (if (and fresh (> fresh 0)) (format ", %d new decision%s" fresh (if (= fresh 1) "" "s")) "")
             "C-x b to read it")))

(defun ygg-ice--script (script)
  "SCRIPT expanded: a quoted leading tilde never reaches the shell as home."
  (if (file-executable-p script) (expand-file-name script)
    (user-error "ice: %s is not there yet" (abbreviate-file-name script))))

(defun ygg-ice-wire (&optional root rebaseline)
  "Run ice-wire.sh on ROOT, this repository by default.
With REBASELINE the test suite's baseline is recorded again.  A project
imported with SPC p i is offered this as the import's ice step."
  (interactive)
  (let ((root (or root (ygg-ice-root))))
    (ygg-ice--compile root (format "%s %s%s" (shell-quote-argument (ygg-ice--script ygg-ice-wire-script))
                                   (if rebaseline "--rebaseline " "")
                                   (shell-quote-argument (directory-file-name (expand-file-name root))))
                      "wire" t)))

(defun ygg-ice-import-step (root)
  "The ice extra of importing ROOT: wire it, or refresh a wired one's
scripts, skills and baseline.  It runs only when the import picked it."
  (let ((root (or root (ygg-ice-root))))
    (ygg-ice--compile root (format "%s %s%s" (shell-quote-argument (ygg-ice--script ygg-ice-wire-script))
                                   (if (file-directory-p (expand-file-name ".ice" root)) "--rebaseline " "")
                                   (shell-quote-argument (directory-file-name (expand-file-name root))))
                      "wire" t t))
  t)

(defun ygg-ice-check (kind change)
  "Run ice-check KIND on CHANGE: intent, expect or plan."
  (let ((root (ygg-ice-root (plist-get change :dir))))
    (ygg-ice--compile root (format "%s %s %s" (shell-quote-argument (ygg-ice--script ygg-ice-check-script))
                                   kind (shell-quote-argument (directory-file-name (plist-get change :dir))))
                      (format "check %s %s" kind (plist-get change :name)) t)))

(defun ygg-ice-check-intent (change)
  "Check CHANGE's intent.md."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Check intent of: ")))
  (ygg-ice-check "intent" change))

(defun ygg-ice-check-expect (change)
  "Check CHANGE's expectations."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Check expectations of: ")))
  (ygg-ice-check "expect" change))

(defun ygg-ice-check-plan (change)
  "Check CHANGE's plan, its tasks.md."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Check plan of: ")))
  (ygg-ice-check "plan" change))

(defun ygg-ice-lat-check ()
  "Run lat check on this repository."
  (interactive)
  (let ((command (or (ygg-ice--lat-command '("check"))
                     (user-error "ice: lat is not installed"))))
    (ygg-ice--compile (ygg-ice-root) (mapconcat #'shell-quote-argument command " ") "lat check")))

(defun ygg-ice-c4-drift ()
  "Run ice-c4-drift: C4 elements whose code is gone."
  (interactive)
  (let* ((root (ygg-ice-root))
         (arch (or (ygg-ice--arch-dir root) (user-error "ice: no C4 arch folder here"))))
    (ygg-ice--compile root (format "%s %s" (shell-quote-argument (ygg-ice--script ygg-ice-c4-drift-script))
                                   (shell-quote-argument (directory-file-name arch)))
                      "c4 drift")))

(defun ygg-ice-lat-drift (change)
  "Run ice-lat-drift on CHANGE: lat.md sections its code left behind."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Lat drift of: ")))
  (ygg-ice--compile (ygg-ice-root (plist-get change :dir))
                    (format "%s %s" (shell-quote-argument (ygg-ice--script ygg-ice-lat-drift-script))
                            (shell-quote-argument (directory-file-name (plist-get change :dir))))
                    "lat drift"))

(defun ygg-ice--compact-command (root &optional apply)
  "The ice-compact command line for ROOT, a dry run unless APPLY."
  (format "%s --older-than %d%s %s" (shell-quote-argument (ygg-ice--script ygg-ice-compact-script))
          ygg-ice-compact-days (if apply " --apply" "")
          (shell-quote-argument (directory-file-name (expand-file-name root)))))

(defvar-local ygg-ice--compact-root nil
  "The repository a dry-run ice-compact buffer belongs to.")

(defun ygg-ice-compact (&optional root)
  "Dry-run ice-compact on ROOT, then offer to remove what it would remove.
Archived changes are filed into lat.md either way; only those older than
ygg-ice-compact-days are removed, and nothing is committed."
  (interactive)
  (let* ((root (or root (ygg-ice-root)))
         (buf (ygg-ice--compile root (ygg-ice--compact-command root) "compact")))
    (with-current-buffer buf
      (setq ygg-ice--compact-root root)
      (add-hook 'compilation-finish-functions #'ygg-ice--compact-finished nil t))
    buf))

(defun ygg-ice--compact-finished (buf how)
  "After the dry run in BUF ended as HOW, offer the removal it planned."
  (let ((root (buffer-local-value 'ygg-ice--compact-root buf))
        (n (with-current-buffer buf
             (save-excursion
               (goto-char (point-min))
               (how-many "^to remove: ")))))
    (when (and root (string-prefix-p "finished" how) (> n 0) (not noninteractive))
      ;; out of the process sentinel before prompting
      (run-at-time 0 nil #'ygg-ice--compact-offer root n))))

(defun ygg-ice--compact-offer (root n)
  "Ask to remove N archived changes of ROOT; on yes rerun ice-compact --apply."
  (when (y-or-n-p (format "Remove %d archived changes older than %d days? " n ygg-ice-compact-days))
    (ygg-ice--compile root (ygg-ice--compact-command root t) "compact")))

(defun ygg-ice--likec4-rows (json root)
  "likec4 validate JSON as FILE:LINE:COL: error: MESSAGE rows, lines one-based."
  (let* ((data (json-parse-string json :object-type 'plist :array-type 'list))
         (errors (plist-get data :errors)))
    (cons (eq (plist-get data :valid) t)
          (mapcar (lambda (e)
                    (let ((start (plist-get (plist-get e :range) :start)))
                      (format "%s:%d:%d: error: %s"
                              (file-relative-name (plist-get e :file) root)
                              (1+ (or (plist-get e :line) 0))
                              (1+ (or (plist-get start :character) 0))
                              (plist-get e :message))))
                  errors))))

(defun ygg-ice-c4-validate ()
  "Validate the C4 model, errors one next-error away."
  (interactive)
  (let* ((root (ygg-ice-root))
         (arch (or (ygg-ice--arch-dir root) (user-error "ice: no C4 arch folder here")))
         (likec4 (or (ygg-ice--program ygg-ice-likec4-program)
                     (user-error "ice: likec4 is not installed")))
         (out (generate-new-buffer " *ice-likec4*"))
         (err (generate-new-buffer " *ice-likec4-log*"))
         (default-directory root))
    (message "likec4: validating %s…" (ygg-ice--rel root arch))
    (make-process
     :name "ice-likec4" :buffer out :stderr err :noquery t
     :command (list likec4 "validate" "--json" "--no-layout" (directory-file-name arch))
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let* ((text (with-current-buffer out (buffer-string)))
                (start (string-search "{" text))
                (parsed (and start (ignore-errors (ygg-ice--likec4-rows (substring text start) root))))
                (buf (get-buffer-create "*ice: likec4 validate*")))
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (erase-buffer)
               (setq default-directory root)
               (insert (format "likec4 validate %s\n\n" (ygg-ice--rel root arch)))
               (cond ((null parsed)
                      (insert (with-current-buffer err (buffer-string)) text))
                     ((car parsed) (insert "valid\n"))
                     (t (insert (string-join (cdr parsed) "\n") "\n"))))
             (ygg-ice-compile-mode)
             (setq default-directory root))
           (kill-buffer out)
           (kill-buffer err)
           (display-buffer buf)))))))

(defvar ygg-ice--previews (make-hash-table :test #'equal)
  "Root to (PROCESS . URL) of its running likec4 start.")

(defun ygg-ice-c4-preview ()
  "Preview the C4 views: likec4 start in the background, its page in the browser."
  (interactive)
  (let* ((root (ygg-ice-root))
         (hit (gethash root ygg-ice--previews)))
    (cond
     ((and hit (process-live-p (car hit)) (cdr hit)) (browse-url (cdr hit)))
     ((and hit (process-live-p (car hit)))
      (message "likec4: the preview is still starting; its page opens when it is up"))
     (t
      (let* ((arch (or (ygg-ice--arch-dir root) (user-error "ice: no C4 arch folder here")))
             (likec4 (or (ygg-ice--program ygg-ice-likec4-program)
                         (user-error "ice: likec4 is not installed")))
             (default-directory root)
             (cell (cons nil nil))
             (buf (with-current-buffer
                      (get-buffer-create (format "*ice: likec4 start %s*"
                                                 (file-name-nondirectory (directory-file-name root))))
                    (unless (derived-mode-p 'ygg-ice-view-mode) (ygg-ice-view-mode))
                    (current-buffer))))
        (puthash root cell ygg-ice--previews)
        (setcar cell
                (make-process
                 :name "ice-likec4-start" :buffer buf :noquery t
                 :command (list likec4 "start" (directory-file-name arch))
                 :filter (lambda (proc text)
                           (when (buffer-live-p (process-buffer proc))
                             (with-current-buffer (process-buffer proc)
                               (let ((inhibit-read-only t))
                                 (goto-char (point-max))
                                 (insert (ansi-color-filter-apply text)))))
                           (when (and (null (cdr cell))
                                      (string-match "https?://\\(?:localhost\\|127\\.0\\.0\\.1\\)[:0-9]*/?"
                                                    (ansi-color-filter-apply text)))
                             (setcdr cell (match-string 0 (ansi-color-filter-apply text)))
                             (browse-url (cdr cell))))))
        (message "likec4: starting the preview…"))))))

(declare-function ygg-diagram--clear "ygg-diagram")

(defun ygg-ice--visit-fresh (file)
  "FILE's buffer, reread from disk without a prompt when it is unmodified.
A frameless daemon hangs on the changed-on-disk question, so none is asked."
  (if-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (unless (or (buffer-modified-p) (verify-visited-file-modtime buf))
          (revert-buffer t t t))
        buf)
    (find-file-noselect file)))

(defun ygg-ice--show-readme (readme)
  "Show README with its Mermaid views drawn afresh."
  (pop-to-buffer-same-window (ygg-ice--visit-fresh readme))
  (when (or (featurep 'ygg-diagram) (require 'ygg-diagram nil t))
    (when (fboundp 'ygg-diagram--clear) (ygg-diagram--clear))
    (condition-case err (ygg-diagram-toggle)
      (user-error (message "%s" (error-message-string err))))))

(defun ygg-ice-c4-readme ()
  "Export the C4 views to the arch folder's README.md, then show it drawn.
likec4 export markdown runs in the background; without likec4 the README
on disk is shown as it is."
  (interactive)
  (let* ((root (ygg-ice-root))
         (arch (or (ygg-ice--arch-dir root) (user-error "ice: no C4 arch folder here")))
         (readme (expand-file-name "README.md" arch))
         (likec4 (ygg-ice--program ygg-ice-likec4-program)))
    (cond
     (likec4
      (let ((log (generate-new-buffer " *ice-likec4-export*"))
            (default-directory root))
        (message "likec4: exporting %s…" (ygg-ice--rel root arch))
        (make-process
         :name "ice-likec4-export" :buffer log :noquery t :connection-type 'pipe
         :command (list likec4 "export" "markdown" (directory-file-name arch))
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (let ((ok (zerop (process-exit-status proc)))
                   (out (string-trim (ansi-color-filter-apply
                                      (with-current-buffer log (buffer-string))))))
               (kill-buffer log)
               (cond ((not (file-exists-p readme))
                      (message "likec4 export markdown wrote no README.md: %s" out))
                     (t (unless ok
                          (message "likec4 export markdown failed, the README is as it was: %s" out))
                        (ygg-ice--show-readme readme)))))))))
     ((file-exists-p readme) (ygg-ice--show-readme readme))
     (t (user-error "ice: likec4 is not installed and %s has no README.md" arch)))))

;;; lat search

(defun ygg-ice--pick-hit (hits prompt)
  (let* ((cands (mapcar (lambda (h)
                          (cons (concat (plist-get h :id)
                                        (propertize (concat "  " (or (plist-get h :text) ""))
                                                    'face 'shadow))
                                h))
                        hits))
         (pick (cdr (assoc (completing-read prompt cands nil t) cands))))
    (when pick
      (ygg-ice--visit (plist-get pick :file) (plist-get pick :line)))))

(defun ygg-ice-lat-search (query)
  "Search lat.md for QUERY and jump to the section picked from what it finds."
  (interactive (list (read-string "lat search: ")))
  (let ((root (ygg-ice-root)))
    (message "lat: searching…")
    (ygg-ice--lat-async
     root (list "search" query "--limit" (number-to-string ygg-ice-search-limit))
     (lambda (out _status)
       (let ((hits (ygg-ice--parse-lat-sections out root)))
         (run-at-time 0 nil
                      (lambda ()
                        (if hits
                            (ygg-ice--pick-hit hits (format "lat %s: " query))
                          (message "lat: nothing for %s" query)))))))))

;;; The Context cache: what the sidebar and the compose popup read

(defvar ygg-ice--context-cache (make-hash-table :test #'equal)
  "Root to its ICE docs as ygg-ice-context-collect read them.")

(defvar ygg-ice--context-pending (make-hash-table :test #'equal)
  "Roots with a read already queued.")

(defvar ygg-ice-context-changed-functions nil
  "Abnormal hook run with ROOT when its cached ICE docs changed.")

(defun ygg-ice--present-p (root)
  "Non-nil when ROOT has any ICE doc at all; stats only."
  (unless (file-remote-p root)
    (or (file-directory-p (expand-file-name "openspec/changes" root))
        (file-directory-p (expand-file-name "lat.md" root))
        (file-directory-p (ygg-ice--adr-dir root))
        (file-exists-p (expand-file-name "CONTEXT.md" root))
        (ygg-ice--arch-dir root))))

(defun ygg-ice--context-signature (root)
  "What ROOT's ICE docs look like to stat: changing when any of them does."
  (let* ((changes (expand-file-name "openspec/changes" root))
         (adr (directory-file-name (ygg-ice--adr-dir root)))
         (arch (ygg-ice--arch-dir root))
         (files (append (list changes adr (expand-file-name "CONTEXT.md" root))
                        (and (file-directory-p changes)
                             (mapcan (lambda (d)
                                       (and (file-directory-p d)
                                            (not (equal (file-name-nondirectory d) "archive"))
                                            (cons d (directory-files-recursively d "" t))))
                                     (directory-files changes t "\\`[^.]")))
                        (and arch (append (list arch (expand-file-name "README.md" arch))
                                          (ygg-ice--c4-files root))))))
    (cons (ygg-ice--lat-signature root)
          (mapcar (lambda (f) (cons f (ygg-ice--mtime f))) files))))

(defun ygg-ice-top-sections (root)
  "The sections lat.md/lat.md links, else each file's first section."
  (let* ((index (expand-file-name "lat.md/lat.md" root))
         (from-index
          (and (file-exists-p index)
               (delq nil
                     (mapcar (lambda (target)
                               (when-let* ((sec (ygg-ice-section root target)))
                                 (unless (equal (plist-get sec :file) index) sec)))
                             (seq-uniq (mapcan (lambda (s) (mapcar #'car (plist-get s :links)))
                                               (seq-filter (lambda (s) (equal (plist-get s :file) index))
                                                           (ygg-ice-sections root)))))))))
    (seq-uniq
     (or from-index
         (seq-filter (lambda (s) (= (plist-get s :level) 1)) (ygg-ice-sections root))))))

(defun ygg-ice--slug (text)
  (replace-regexp-in-string "[[:space:]]+" "-" (string-trim text)))

(defun ygg-ice--section-item (sec)
  (list :kind 'section :label (plist-get sec :heading) :id (plist-get sec :id)
        :file (plist-get sec :file) :line (plist-get sec :line) :section t))

(defun ygg-ice-context-collect (root)
  "Read ROOT's ICE docs: changes, lat.md, ADRs, the glossary, C4 views."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (sections (ygg-ice-sections root)))
    (list :sig (ygg-ice--context-signature root)
          :present (and (ygg-ice--present-p root) t)
          :changes (mapcar (lambda (c) (ygg-ice--change-item root c)) (ygg-ice-changes root))
          :top (mapcar #'ygg-ice--section-item (ygg-ice-top-sections root))
          :sections (mapcar #'ygg-ice--section-item sections)
          :adrs (ygg-ice-adrs root)
          :glossary (ygg-ice-glossary root)
          :views (ygg-ice-c4-views root))))

(defun ygg-ice-context-update (root)
  "Read ROOT's ICE docs again when stat says they changed; t when they had."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (was (gethash root ygg-ice--context-cache)))
    (unless (and was (equal (plist-get was :sig) (ygg-ice--context-signature root)))
      (let ((now (ygg-ice-context-collect root)))
        (puthash root now ygg-ice--context-cache)
        (unless (equal (plist-put (copy-sequence was) :sig nil)
                       (plist-put (copy-sequence now) :sig nil))
          (run-hook-with-args 'ygg-ice-context-changed-functions root)
          t)))))

(defun ygg-ice-context-scan (root)
  "Queue a read of ROOT's ICE docs, off whatever path asked for it."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (unless (or (file-remote-p root) (gethash root ygg-ice--context-pending))
      (puthash root t ygg-ice--context-pending)
      (run-at-time 0 nil (lambda ()
                           (unwind-protect (ignore-errors (ygg-ice-context-update root))
                             (remhash root ygg-ice--context-pending)))))))

(defun ygg-ice-context (root)
  "ROOT's cached ICE docs, or nil before the first read; never reads."
  (gethash (file-name-as-directory (expand-file-name root)) ygg-ice--context-cache))

(defun ygg-ice-context-present-p (root)
  "Non-nil when ROOT has ICE docs; before the first read, a stat decides."
  (if-let* ((cached (ygg-ice-context root)))
      (plist-get cached :present)
    (ygg-ice-context-scan root)
    (ygg-ice--present-p root)))

(defun ygg-ice-context-count (root)
  "How many OpenSpec changes ROOT has open, as last read."
  (length (plist-get (ygg-ice-context root) :changes)))

(defun ygg-ice--badge (item)
  (pcase (plist-get item :kind)
    ('change (let ((c (plist-get item :change)))
               (if (plist-get c :tasks) (format "%d/%d" (plist-get c :done) (plist-get c :total))
                 "change")))
    ('section "lat")
    ('adr "adr")
    ('glossary "glossary")
    ('c4 "c4")
    (_ "")))

(defun ygg-ice-context-entries (root)
  "ROOT's Context row as (LABEL . ITEM), from the cache only."
  (when-let* ((ctx (ygg-ice-context root)))
    (mapcar (lambda (item)
              (cons (plist-get item :label)
                    (append (list :ice t :badge (ygg-ice--badge item)) item)))
            (append (plist-get ctx :changes)
                    (plist-get ctx :top)
                    (plist-get ctx :adrs)
                    (and (plist-get ctx :glossary) (list (plist-get ctx :glossary)))
                    (plist-get ctx :views)))))

(defun ygg-ice--on-save ()
  "Read this file's repository's ICE docs again when it is one of them."
  (when-let* ((file buffer-file-name)
              ((not (file-remote-p file)))
              (root (ygg-ice-root (file-name-directory file)))
              (rel (file-relative-name file root))
              ((or (string-match-p "\\`\\(?:openspec\\|lat\\.md\\|docs\\|doc\\)/" rel)
                   (equal rel "CONTEXT.md"))))
    (ygg-ice-context-scan root)))

(defun ygg-ice--on-todo (file &rest _)
  (when (string-match-p "/openspec/changes/" (expand-file-name file))
    (ygg-ice-context-scan (ygg-ice-root (file-name-directory file)))))

(add-hook 'after-save-hook #'ygg-ice--on-save)
(add-hook 'ygg-todo-changed-functions #'ygg-ice--on-todo)

;;; Compose: @ names an ICE doc, and the send carries where it is

(defun ygg-ice-mentions-of (root)
  "ROOT's ICE docs as (NAME ANNOTATION POINTER), from the cache only."
  (when-let* ((ctx (ygg-ice-context root)))
    (let ((rel (lambda (f) (file-relative-name f root))))
      (append
       (mapcar (lambda (s)
                 (let ((id (plist-get s :id)))
                   (list (concat "lat:" (ygg-ice--slug (string-remove-prefix "lat.md/" id)))
                         "  lat section"
                         (format "lat section \"%s\" in %s:%d, heading %s; read it with lat section \"%s\" or follow [[%s]]"
                                 (plist-get s :label) (funcall rel (plist-get s :file))
                                 (plist-get s :line)
                                 (plist-get s :label) id id))))
               (plist-get ctx :sections))
       (mapcar (lambda (c)
                 (let ((change (plist-get c :change)))
                   (list (concat "change:" (plist-get change :name))
                         (format "  change %d/%d" (plist-get change :done) (plist-get change :total))
                         (format "openspec change %s in %s"
                                 (plist-get change :name) (funcall rel (plist-get change :dir))))))
               (plist-get ctx :changes))
       (mapcar (lambda (a)
                 (list (concat "adr:" (plist-get a :slug)) "  adr"
                       (format "ADR %s" (funcall rel (plist-get a :file)))))
               (plist-get ctx :adrs))
       (when-let* ((g (plist-get ctx :glossary)))
         (list (list "glossary" "  glossary"
                     (format "glossary %s (terms, definitions, words to avoid)"
                             (funcall rel (plist-get g :file))))))
       (mapcar (lambda (v)
                 (list (concat "c4:" (downcase (ygg-ice--slug (plist-get v :label))))
                       "  c4 view"
                       (format "C4 view \"%s\" in %s:%d" (plist-get v :label)
                               (funcall rel (plist-get v :file)) (plist-get v :line))))
               (plist-get ctx :views))))))

(defun ygg-ice-mentions (dir)
  "ICE docs for the popup an at opens, from the cache; a miss queues a read."
  (let ((root (ygg-ice-root dir)))
    (if (ygg-ice-context root)
        (mapcar (lambda (m) (cons (nth 0 m) (nth 1 m))) (ygg-ice-mentions-of root))
      (when (ygg-ice--present-p root) (ygg-ice-context-scan root))
      nil)))

(defun ygg-ice-expand-mentions (text)
  "TEXT with a pointer after it for each ICE doc it names with @, never the doc."
  (let* ((dir (if (fboundp 'aob--capf-dir) (aob--capf-dir) default-directory))
         (root (ygg-ice-root dir))
         (named (seq-filter
                 (lambda (m)
                   (string-match-p (concat "@" (regexp-quote (car m))
                                           "[.,;:!?)]*\\(?:[[:space:]]\\|\\'\\)")
                                   text))
                 (ygg-ice-mentions-of root))))
    (when named
      (concat text "\n\n<ice-context>\n"
              (mapconcat (lambda (m) (format "- @%s: %s" (nth 0 m) (nth 2 m))) named "\n")
              "\n</ice-context>"))))

(defvar aob-capf-mention-functions)
(defvar aob-compose-before-send-functions)
(with-eval-after-load 'aob
  (add-hook 'aob-capf-mention-functions #'ygg-ice-mentions t)
  (add-hook 'aob-compose-before-send-functions #'ygg-ice-expand-mentions t))

;;; LikeC4 source

(defvar ygg-likec4-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    table))

(defconst ygg-likec4--keywords
  '("specification" "model" "views" "view" "of" "element" "relationship" "tag"
    "include" "exclude" "style" "title" "description" "technology" "link" "icon"
    "color" "shape" "extend" "deployment" "dynamic" "global" "import" "from"
    "autoLayout" "with" "where" "metadata" "navigateTo" "instanceOf" "parallel"))

(define-derived-mode ygg-likec4-mode prog-mode "LikeC4"
  "LikeC4 architecture models, with likec4 lsp for the rest."
  :syntax-table ygg-likec4-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local font-lock-defaults
              `(((,(regexp-opt ygg-likec4--keywords 'symbols) . font-lock-keyword-face)
                 ("\\_<\\([[:alnum:]_]+\\)[ \t]*=" 1 font-lock-variable-name-face)
                 ("->\\|-\\[\\|\\]->" . font-lock-builtin-face)))))

(defun ygg-ice--likec4-eglot ()
  "likec4 lsp for the model, where likec4 is installed."
  (when (and (ygg-ice--program ygg-ice-likec4-program) (fboundp 'eglot-ensure))
    (eglot-ensure)))

(add-hook 'ygg-likec4-mode-hook #'ygg-ice--likec4-eglot)

(unless (or (assoc-default "x.c4" auto-mode-alist #'string-match)
            (assoc-default "x.likec4" auto-mode-alist #'string-match))
  (add-to-list 'auto-mode-alist '("\\.\\(?:c4\\|likec4\\)\\'" . ygg-likec4-mode)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((ygg-likec4-mode :language-id "likec4") . ("likec4" "lsp" "--stdio"))))

;;; Sessions under a preset: the gap detector and the map's upkeep

(defvar aob-compose-spawn-function)
(defvar aob-compose--dir)
(defvar ygg-aob--draft-tree)
(declare-function aob-live-sessions "aob" ())
(declare-function aob-session-dir "aob" (s))
(declare-function aob-session-project "aob" (s))
(declare-function ygg-aob--expand-presets "layer-aob" (text))
(declare-function ygg-preset-get "ygg-preset" (name &optional root))
(declare-function ygg-preset-skills "ygg-preset" (d))
(declare-function ygg-preset-skill-body "ygg-preset" (name root))

(defun ygg-ice--preset-skills (preset root)
  "The skills PRESET names, each body in a block the session reads whole.
A skill that forbids model invocation reaches the agent no other way."
  (when-let* (((require 'ygg-preset nil t))
              (d (ygg-preset-get preset root)))
    (mapconcat (lambda (name)
                 (when-let* ((body (ygg-preset-skill-body name root)))
                   (format "\n\n<skill name=\"%s\">\n%s\n</skill>" name (string-trim body))))
               (ygg-preset-skills d) "")))

(defun ygg-ice--spawn-under (preset root goal)
  "Open an aob session in ROOT under PRESET, GOAL its first turn.
The preset rides the way a draft naming @PRESET carries it, so its
tools, thinking and subagents limit the session, and the skills it
names ride after it."
  (unless (and (bound-and-true-p aob-compose-spawn-function)
               (fboundp 'ygg-aob--expand-presets))
    (user-error "ice: aob is not loaded"))
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (aob-compose--dir root)
         (ygg-aob--draft-tree nil)
         (default-directory root)
         (text (ygg-aob--expand-presets (format "@%s %s" preset goal))))
    (unless (and text (string-search (format "<preset name=\"%s\">" preset) text))
      (user-error "ice: no %s preset here" preset))
    (funcall aob-compose-spawn-function
             (concat text (ygg-ice--preset-skills preset root)))))

(defun ygg-ice--intent-file (change)
  (let ((file (expand-file-name "intent.md" (plist-get change :dir))))
    (if (file-exists-p file) file
      (user-error "ice: %s has no intent.md" (plist-get change :name)))))

(defun ygg-ice-gaps (change)
  "Run the gap detector on CHANGE: a read-only session under the gaps preset.
It reads intent.md and the repo and writes gaps.md beside it."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Gaps of: ")))
  (let* ((intent (ygg-ice--intent-file change))
         (root (ygg-ice-root (plist-get change :dir)))
         (dir (file-relative-name (plist-get change :dir) root)))
    (ygg-ice--spawn-under
     "gaps" root
     (format "Find the gaps in the intent of change %s: read %s and write %sgaps.md. Never edit intent.md."
             (plist-get change :name) (file-relative-name intent root) dir))))

(defun ygg-ice--live-sessions-in (root)
  "The live agent sessions working inside ROOT."
  (let ((root (file-truename (file-name-as-directory (expand-file-name root)))))
    (and (fboundp 'aob-live-sessions)
         (seq-filter (lambda (s)
                       (when-let* ((dir (or (aob-session-dir s) (aob-session-project s))))
                         (string-prefix-p root (file-truename (file-name-as-directory (expand-file-name dir))))))
                     (aob-live-sessions)))))

(defun ygg-ice-maintain (root)
  "Keep ROOT's verification skill and lat.md feature map honest.
A session under the maintain preset; it ends clean, changed or blocked.
Skipped while an agent session is live in ROOT: its edits would land in
the middle of that session's change."
  (interactive (list (ygg-ice-root)))
  (when-let* ((live (ygg-ice--live-sessions-in root)))
    (user-error "ice: maintain skipped in %s: %d live agent session%s there"
                (abbreviate-file-name (directory-file-name (expand-file-name root)))
                (length live) (if (cdr live) "s" "")))
  (ygg-ice--spawn-under
   "maintain" root
   (format "Run the maintain pass on %s: every feature section of lat.md/features.md from source, then live."
           (abbreviate-file-name (directory-file-name (expand-file-name root))))))

;;; The restate-back gate: the owner's Confirmed line

(defconst ygg-ice--confirmed-re "^[ \t]*Confirmed:.*$"
  "The owner's line under Restated.")

(defun ygg-ice--section-bounds (title-re)
  "The body of the h2 section whose title matches TITLE-RE, as (BEG . END)."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward (concat "^##[ \t]+" title-re "[ \t]*$") nil t)
        (let ((beg (min (point-max) (1+ (point)))))
          (cons beg (if (re-search-forward "^##[ \t]" nil t)
                        (match-beginning 0)
                      (point-max))))))))

(defun ygg-ice--restated-bounds ()
  "The body of this buffer's Restated section as (BEG . END), or nil."
  (ygg-ice--section-bounds "Restated"))

(defun ygg-ice--section-canonical (title-re &optional stamp-re)
  "The section under TITLE-RE as ice-check hashes it.
Lines right-trimmed, the owner's STAMP-RE lines out (Confirmed lines by
default), blank lines at the ends dropped."
  (let ((bounds (ygg-ice--section-bounds title-re))
        (stamp-re (or stamp-re ygg-ice--confirmed-re))
        (case-fold-search nil))
    (string-trim
     (mapconcat #'string-trim-right
                (seq-remove (lambda (line) (string-match-p stamp-re line))
                            (and bounds (split-string (buffer-substring-no-properties (car bounds) (cdr bounds))
                                                      "\n")))
                "\n")
     "\n+" "\n+")))

(defun ygg-ice--intent-hash ()
  "The sha1 prefix the Confirmed line carries.
It covers What is wanted and Restated, as ice-check intent reads them."
  (substring (secure-hash 'sha1 (encode-coding-string
                                 (concat (ygg-ice--section-canonical "What[ \t]+is[ \t]+wanted")
                                         "\n\n"
                                         (ygg-ice--section-canonical "Restated"))
                                 'utf-8))
             0 8))

(defun ygg-ice--restated-text ()
  "The Restated section's words in this buffer, its Confirmed line left out."
  (when-let* ((bounds (ygg-ice--restated-bounds)))
    (string-trim
     (replace-regexp-in-string
      "^[ \t]*\\(?:Goals\\|Problem\\|Not the goal\\|Unsure\\):[ \t]*$" ""
      (replace-regexp-in-string ygg-ice--confirmed-re ""
                                (buffer-substring-no-properties (car bounds) (cdr bounds)))))))

(defun ygg-ice--write-confirmed (date)
  "Set the Confirmed line under Restated to DATE and the intent's hash.
It is left the only one there."
  (ygg-ice--write-stamp (or (ygg-ice--restated-bounds) (user-error "ice: no Restated section"))
                        ygg-ice--confirmed-re
                        (concat "Confirmed: " date " sha1:" (ygg-ice--intent-hash))))

(defun ygg-ice--write-stamp (bounds stamp-re line)
  "Leave LINE the only STAMP-RE line in BOUNDS, in place of the first one.
With none there, LINE goes after the section's last words."
  (save-excursion
    (let ((end (copy-marker (cdr bounds))) (case-fold-search nil) placed)
      (goto-char (car bounds))
      (while (re-search-forward stamp-re end t)
        (if placed
            (delete-region (match-beginning 0) (min (point-max) (1+ (match-end 0))))
          (replace-match line t t)
          (setq placed t)))
      (unless placed
        (goto-char end)
        (skip-chars-backward " \t\n" (car bounds))
        (delete-region (point) end)
        (insert "\n\n" line "\n" (if (< end (point-max)) "\n" "")))
      (set-marker end nil))))

(defun ygg-ice-confirm-intent (change)
  "Show CHANGE's Restated intent and, on yes, set its Confirmed line to today.
The line carries the sha1 of What is wanted and Restated; the owner's act:
ice-check intent fails until the line holds a date and a matching hash."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Confirm intent of: ")))
  (let* ((file (ygg-ice--intent-file change))
         (buf (ygg-ice--visit-fresh file))
         (name (plist-get change :name))
         (text (with-current-buffer buf (ygg-ice--restated-text))))
    (when (or (null text) (string-empty-p text))
      (user-error "ice: %s has nothing under Restated yet" name))
    (let* ((shown (get-buffer-create (format "*ice: restated %s*" name)))
           (win (progn
                  (with-current-buffer shown
                    (ygg-ice-view-mode)
                    (setq truncate-lines nil)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert text "\n")
                      (goto-char (point-min))))
                  (display-buffer shown)))
           (yes (unwind-protect (y-or-n-p (format "Confirm the restated intent of %s? " name))
                  (if (window-live-p win) (quit-restore-window win 'kill) (kill-buffer shown)))))
      (when yes
        (let ((date (format-time-string "%Y-%m-%d")))
          (with-current-buffer buf
            (ygg-ice--write-confirmed date)
            (save-buffer))
          (message "ice: %s confirmed %s" name date)
          date)))))

;;; The owner's approval of the checkpoints

(defconst ygg-ice--approved-re "^[ \t]*Approved:.*$"
  "The owner's line under the Checkpoints list.")

(defun ygg-ice--checkpoints-hash ()
  "The sha1 prefix the Approved line carries, over the Checkpoints section.
It matches ice-check plan's checkpoints_hash byte for byte."
  (substring (secure-hash 'sha1 (encode-coding-string
                                 (ygg-ice--section-canonical "Checkpoints" ygg-ice--approved-re)
                                 'utf-8))
             0 8))

(defun ygg-ice-approve-checkpoints (change)
  "Show CHANGE's checkpoints and, on yes, set their Approved line to today.
The line carries the sha1 of the list; the owner's act: ice-check plan
fails until the line holds a date and a matching hash."
  (interactive (list (ygg-ice-read-change (ygg-ice-root) "Approve checkpoints of: ")))
  (let* ((name (plist-get change :name))
         (file (or (plist-get change :tasks) (user-error "ice: %s has no tasks.md" name)))
         (buf (ygg-ice--visit-fresh file))
         (text (with-current-buffer buf
                 (unless (ygg-ice--section-bounds "Checkpoints")
                   (user-error "ice: %s has no Checkpoints section in tasks.md" name))
                 (ygg-ice--section-canonical "Checkpoints" ygg-ice--approved-re))))
    (when (string-empty-p text)
      (user-error "ice: %s has no checkpoints listed yet" name))
    (let* ((shown (get-buffer-create (format "*ice: checkpoints %s*" name)))
           (win (progn
                  (with-current-buffer shown
                    (ygg-ice-view-mode)
                    (setq truncate-lines nil)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert text "\n")
                      (goto-char (point-min))))
                  (display-buffer shown)))
           (yes (unwind-protect (y-or-n-p (format "Approve the checkpoints of %s? " name))
                  (if (window-live-p win) (quit-restore-window win 'kill) (kill-buffer shown)))))
      (when yes
        (let ((date (format-time-string "%Y-%m-%d")))
          (with-current-buffer buf
            (ygg-ice--write-stamp (ygg-ice--section-bounds "Checkpoints") ygg-ice--approved-re
                                  (concat "Approved: " date " sha1:" (ygg-ice--checkpoints-hash)))
            (save-buffer))
          (message "ice: %s checkpoints approved %s" name date)
          date)))))

;;; The daily maintain run, inside Emacs only

(defcustom ygg-ice-maintain-stamp-file (locate-user-emacs-file "var/ice-maintain.eld")
  "Where the day each project last had its maintain run is kept."
  :type 'file)

(defcustom ygg-ice-maintain-idle 300
  "Seconds Emacs sits idle before a due maintain run starts."
  :type 'natnum)

(defvar ygg-ice-maintain-daily)
(defvar ygg-ice--maintain-timer nil
  "The idle timer that starts the daily maintain runs.")

(defun ygg-ice--maintain-stamps ()
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents ygg-ice-maintain-stamp-file)
      (read (current-buffer)))))

(defun ygg-ice--maintain-save-stamps (stamps)
  (make-directory (file-name-directory ygg-ice-maintain-stamp-file) t)
  (with-temp-file ygg-ice-maintain-stamp-file
    (prin1 stamps (current-buffer))))

(defun ygg-ice--maintain-tick ()
  "Start today's maintain run in each of ygg-ice-maintain-daily not yet run today."
  (let ((today (format-time-string "%Y-%m-%d"))
        (stamps (ygg-ice--maintain-stamps))
        started)
    (dolist (root ygg-ice-maintain-daily)
      (let ((root (file-name-as-directory (expand-file-name root))))
        (when (and (file-directory-p root)
                   (not (equal (alist-get root stamps nil nil #'equal) today)))
          (if-let* ((live (ygg-ice--live-sessions-in root)))
              (message "ice: the daily maintain in %s skipped: %d live agent session%s there"
                       (abbreviate-file-name root) (length live) (if (cdr live) "s" ""))
            (condition-case err
                (progn
                  (ygg-ice-maintain root)
                  (setf (alist-get root stamps nil nil #'equal) today)
                  (setq started t))
              (error (message "ice: the daily maintain in %s did not start: %s"
                              (abbreviate-file-name root) (error-message-string err))))))))
    (when started (ygg-ice--maintain-save-stamps stamps))))

(defun ygg-ice--maintain-arm ()
  "Arm the daily maintain timer when projects are named, else disarm it."
  (when (timerp ygg-ice--maintain-timer)
    (cancel-timer ygg-ice--maintain-timer))
  (setq ygg-ice--maintain-timer
        (and (bound-and-true-p ygg-ice-maintain-daily) (not noninteractive)
             (run-with-idle-timer ygg-ice-maintain-idle t #'ygg-ice--maintain-tick))))

(defcustom ygg-ice-maintain-daily nil
  "Projects whose feature map the maintain preset keeps, once a day, or nil.
Each is a repository root; its run starts the first time Emacs is idle
for ygg-ice-maintain-idle seconds on a day it has not run.  Set it with
customize or setopt, so the timer follows."
  :type '(choice (const :tag "Off" nil) (repeat :tag "Projects" directory))
  :initialize #'custom-initialize-default
  :set (lambda (sym val) (set-default sym val) (ygg-ice--maintain-arm)))

(ygg-ice--maintain-arm)

;;; Keys

(defvar ygg-ice-leader-map (make-sparse-keymap)
  "The SPC a k prefix: intent, context and expectation docs.")

(defconst ygg-ice-leader-keys
  '(("R" ygg-ice-confirm-intent "confirm intent")
    ("A" ygg-ice-approve-checkpoints "approve checkpoints")
    ("o" ygg-ice-changes-list "openspec changes")
    ("s" ygg-ice-lat-search "lat search")
    ("c" ygg-ice-connections "connections here")
    ("b" ygg-ice-c4-preview "c4 preview"))
  "Each key under SPC a k, its command and its which-key label.
The owner's two acts and four lookups; the lead runs the rest, and each
other ICE command stays an M-x away.")

(pcase-dolist (`(,key ,cmd ,label) ygg-ice-leader-keys)
  (define-key ygg-ice-leader-map (kbd key) (cons label cmd)))

(defvar ygg-leader-agent-map)
(defun ygg-ice--bind-leader ()
  (define-key ygg-leader-agent-map (kbd "k") (cons "context (ice)" ygg-ice-leader-map)))
(with-eval-after-load 'layer-aob (ygg-ice--bind-leader))

(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-ice-view-mode)
  (yggdrasil-define-mode-keys 'ygg-ice-view-mode '(normal visual) ygg-ice-view-mode-map))

(provide 'ygg-ice)
;;; ygg-ice.el ends here
