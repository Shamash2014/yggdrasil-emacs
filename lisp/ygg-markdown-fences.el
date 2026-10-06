;;; ygg-markdown-fences.el --- fenced code blocks rendered inline by language -*- lexical-binding: t; -*-

;;; Commentary:
;; Fence tags resolve to a major mode when markdown-mode asks for one,
;; tree-sitter first where its grammar is there.  The block is drawn on a
;; subtle background with its fence lines hidden and the language as a dim
;; label.  `ygg-markdown-fences-propertize' does the same for text too long
;; for markdown-mode itself, fontifying nothing but the code.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'treesit nil t)

(defvar markdown-hide-markup)
(defvar treesit-extra-load-path)
(defvar markdown-fontify-code-blocks-natively)

(defgroup ygg-markdown-fences nil "Fenced code blocks drawn inline."
  :group 'markdown)

(defcustom ygg-markdown-fences-inline t
  "Draw fenced blocks on a background with a language label.
Off, the fence lines stay as written."
  :type 'boolean :local t :group 'ygg-markdown-fences)

(defcustom ygg-markdown-fences-code-max 50000
  "Most characters of code fontified in one text too long for markdown-mode."
  :type 'natnum :group 'ygg-markdown-fences)

(defface ygg-markdown-fences-block
  '((t :inherit secondary-selection :extend t))
  "Background of a fenced code block, tinted from the theme as it changes."
  :group 'ygg-markdown-fences)

(defconst ygg-markdown-fences--tint-light 0.04
  "Share of the foreground mixed into a light theme's background.")

(defconst ygg-markdown-fences--tint-dark 0.06
  "Share of the foreground mixed into a dark theme's background.")

(defun ygg-markdown-fences--luminance (rgb)
  "Relative brightness of RGB, 0 to 1."
  (+ (* 0.2126 (nth 0 rgb)) (* 0.7152 (nth 1 rgb)) (* 0.0722 (nth 2 rgb))))

(defun ygg-markdown-fences--rgb (color)
  "COLOR as red, green and blue between 0 and 1, exact for hex specs."
  (when (stringp color)
    (or (when-let* ((values (color-values-from-color-spec color)))
          (mapcar (lambda (v) (/ v 65535.0)) values))
        (color-name-to-rgb color))))

(defun ygg-markdown-fences--tint ()
  "The default background nudged toward the foreground, or nil without one."
  (when-let* ((bg (ygg-markdown-fences--rgb (face-attribute 'default :background nil t))))
    (let* ((dark (< (ygg-markdown-fences--luminance bg) 0.5))
           (fg (or (ygg-markdown-fences--rgb (face-attribute 'default :foreground nil t))
                   (if dark '(1.0 1.0 1.0) '(0.0 0.0 0.0))))
           (share (if dark
                      ygg-markdown-fences--tint-dark
                    ygg-markdown-fences--tint-light)))
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (b f) (+ (* (- 1 share) b) (* share f)))
                                bg fg)
                     '(2))))))

(defun ygg-markdown-fences--restyle (&rest _)
  "Give the block face its tint, or `secondary-selection' with no background."
  (let ((tint (ygg-markdown-fences--tint)))
    (set-face-attribute 'ygg-markdown-fences-block nil
                        :background (or tint 'unspecified)
                        :inherit (if tint 'unspecified 'secondary-selection)
                        :extend t)))

(add-hook 'enable-theme-functions #'ygg-markdown-fences--restyle)
(ygg-markdown-fences--restyle)

(defface ygg-markdown-fences-label
  '((t :inherit shadow :height 0.8))
  "The language named above a fenced code block."
  :group 'ygg-markdown-fences)

(defconst ygg-markdown-fences-languages
  '((("ts" "typescript") typescript-ts-mode typescript)
    (("tsx") tsx-ts-mode tsx)
    (("js" "javascript" "jsx") js-ts-mode javascript js-mode)
    (("py" "python") python-ts-mode python python-mode)
    (("rust" "rs") rust-ts-mode rust rust-mode)
    (("go") go-ts-mode go go-mode)
    (("dart") nil nil dart-mode)
    (("elisp" "emacs-lisp" "el") nil nil emacs-lisp-mode)
    (("json") json-ts-mode json js-json-mode)
    (("yaml" "yml") yaml-ts-mode yaml yaml-mode)
    (("toml") toml-ts-mode toml conf-toml-mode)
    (("kotlin" "kt") kotlin-ts-mode kotlin)
    (("swift") swift-ts-mode swift swift-mode)
    (("sh" "bash" "shell" "zsh") bash-ts-mode bash sh-mode)
    (("css") css-ts-mode css css-mode)
    (("diff" "patch") nil nil diff-mode)
    (("java") java-ts-mode java java-mode)
    (("ruby" "rb") ruby-ts-mode ruby ruby-mode)
    (("lua") lua-ts-mode lua lua-mode)
    (("elixir" "ex") elixir-ts-mode elixir elixir-mode)
    (("dockerfile") dockerfile-ts-mode dockerfile dockerfile-mode)
    (("r") r-ts-mode r R-mode))
  "Fence tags, then the tree-sitter mode, its grammar, and the classic mode.")

(defconst ygg-markdown-fences--failure-ttl 30
  "Seconds a failed lookup is believed unless the load paths change.")

(defvar ygg-markdown-fences--grammars (make-hash-table :test 'eq)
  "Whether each grammar loads; asking the library costs milliseconds.")

(defvar ygg-markdown-fences--modes (make-hash-table :test 'eq)
  "The mode each name resolves to; a failed `require' costs milliseconds.")

(defun ygg-markdown-fences--remembered (table key paths compute)
  "The value of COMPUTE for KEY in TABLE, kept while it is a success.
A failure is kept only while PATHS stay the same and its TTL has not run out."
  (let ((entry (gethash key table)))
    (if (and entry
             (or (car entry)
                 (and (equal (cadr entry) paths)
                      (< (- (float-time) (cddr entry))
                         ygg-markdown-fences--failure-ttl))))
        (car entry)
      (let ((value (funcall compute)))
        (puthash key (cons value (cons paths (float-time))) table)
        value))))

(defun ygg-markdown-fences--grammar-ready (grammar)
  "Non-nil when tree-sitter has GRAMMAR."
  (ygg-markdown-fences--remembered
   ygg-markdown-fences--grammars grammar treesit-extra-load-path
   (lambda () (and (featurep 'treesit) (treesit-ready-p grammar t)))))

(defun ygg-markdown-fences--usable (mode)
  "MODE after `major-mode-remap-alist', or nil when it is not defined."
  (let ((mode (and mode (major-mode-remap mode))))
    (and mode
         (or (fboundp mode)
             (ygg-markdown-fences--remembered
              ygg-markdown-fences--modes mode load-path
              (lambda () (and (require mode nil t) (fboundp mode) mode))))
         mode)))

(defun ygg-markdown-fences-mode (lang)
  "The major mode fence tag LANG stands for, or nil when it is not ours."
  (pcase-let ((`(,_ ,ts ,grammar ,classic)
               (cl-find-if (lambda (entry) (member (downcase lang) (car entry)))
                           ygg-markdown-fences-languages)))
    (or (and ts (ygg-markdown-fences--grammar-ready grammar)
             (ygg-markdown-fences--usable ts))
        (ygg-markdown-fences--usable classic))))

(advice-add 'markdown-get-lang-mode :before-until #'ygg-markdown-fences-mode)

(defconst ygg-markdown-fences--fence-re
  "^[ \t]*\\(```+\\|~~~+\\)[ \t]*{?\\.?\\([^ \t\n`{}]*\\)[^\n]*\n"
  "An opening fence; group 2 is its language tag.")

(defun ygg-markdown-fences--dress (bounds lang hide)
  "Draw the block in BOUNDS as the inline look.
BOUNDS is the opening fence's start, the body's start and end, and the
closing fence's end."
  (pcase-let ((`(,open ,body-beg ,body-end ,close-end) bounds))
    (add-face-text-property open close-end 'ygg-markdown-fences-block t)
    (when hide
      (put-text-property open body-beg 'invisible 'markdown-markup)
      (put-text-property body-end close-end 'invisible 'markdown-markup)
      (unless (string-empty-p lang)
        (remove-text-properties open body-beg '(invisible nil))
        (put-text-property
         open (max open (1- body-beg)) 'display
         (propertize (concat " " lang)
                     'face '(ygg-markdown-fences-label
                             ygg-markdown-fences-block)))))))

(defun ygg-markdown-fences--bare (bounds)
  "The fence lines of BOUNDS shown as written."
  (pcase-let ((`(,open ,body-beg ,body-end ,close-end) bounds))
    (remove-text-properties open body-beg '(invisible nil display nil))
    (remove-text-properties body-end close-end '(invisible nil))))

(defun ygg-markdown-fences--decorate (orig matcher last)
  "Run ORIG on MATCHER and LAST, then give the block it found the look."
  (let ((hit (funcall orig matcher last)))
    (when hit
      (save-excursion
        (save-match-data
          (let* ((body-beg (match-beginning 0))
                 (body-end (match-end 0))
                 (open (progn (goto-char body-beg)
                              (if (bolp) (line-beginning-position 0)
                                (line-beginning-position))))
                 (close-end (progn (goto-char body-end)
                                   (if (bolp) (line-beginning-position 2)
                                     (line-beginning-position 3))))
                 (bounds (list open body-beg body-end close-end))
                 (line (buffer-substring-no-properties open body-beg))
                 (lang (if (string-match ygg-markdown-fences--fence-re line)
                           (match-string 2 line)
                         "")))
            (if ygg-markdown-fences-inline
                (ygg-markdown-fences--dress
                 bounds lang (bound-and-true-p markdown-hide-markup))
              (ygg-markdown-fences--bare bounds))))))
    hit))

(advice-add 'markdown-fontify-code-blocks-generic
            :around #'ygg-markdown-fences--decorate)

(declare-function markdown-match-propertized-text "markdown-mode")
(declare-function markdown-fontify-code-block-natively "markdown-mode")

(defun ygg-markdown-fences--loose-block (begin-prop body-prop end-prop last)
  "Dress the next block of BEGIN-PROP before LAST that has no fontified body.
Those are the empty block and the one never closed, which markdown-mode
hides whole.  BODY-PROP marks a body, END-PROP a closing fence."
  (let (handled)
    (while (and (not handled)
                (markdown-match-propertized-text begin-prop last))
      (save-excursion
        (save-match-data
          (let* ((open (progn (goto-char (match-beginning 0))
                              (line-beginning-position)))
                 (body-beg (progn (forward-line 1) (point)))
                 (line (buffer-substring-no-properties open body-beg))
                 (lang (if (string-match ygg-markdown-fences--fence-re line)
                           (match-string 2 line)
                         "")))
            (when (and (eq (char-before body-beg) ?\n)
                       (not (get-text-property body-beg body-prop)))
              (setq handled t)
              (skip-chars-forward " \t")
              (let* ((closed (get-text-property (point) end-prop))
                     (body-end (if closed body-beg (point-max)))
                     (close-end (if closed (line-beginning-position 2)
                                  (point-max)))
                     (bounds (list open body-beg body-end close-end))
                     (hide (bound-and-true-p markdown-hide-markup)))
                (unless closed
                  (remove-text-properties open body-beg '(invisible nil))
                  (when (and markdown-fontify-code-blocks-natively
                             (not (string-empty-p lang))
                             (< body-beg body-end))
                    (markdown-fontify-code-block-natively
                     lang body-beg body-end)))
                (if ygg-markdown-fences-inline
                    (ygg-markdown-fences--dress
                     bounds lang
                     (and hide (or closed (not (string-empty-p lang)))))
                  (ygg-markdown-fences--bare bounds))))))))
    handled))

(defun ygg-markdown-fences--fontify-loose-gfm (last)
  "Dress backquote blocks without a body before LAST."
  (ygg-markdown-fences--loose-block
   'markdown-gfm-block-begin 'markdown-gfm-code 'markdown-gfm-block-end last))

(defun ygg-markdown-fences--fontify-loose-tilde (last)
  "Dress tilde blocks without a body before LAST."
  (ygg-markdown-fences--loose-block
   'markdown-tilde-fence-begin 'markdown-fenced-code
   'markdown-tilde-fence-end last))

(dolist (mode '(markdown-mode gfm-mode))
  (font-lock-add-keywords
   mode '((ygg-markdown-fences--fontify-loose-gfm)
          (ygg-markdown-fences--fontify-loose-tilde))
   'append))

(defun ygg-markdown-fences-toggle ()
  "Show the fence lines as written, or the inline look again."
  (interactive)
  (setq ygg-markdown-fences-inline (not ygg-markdown-fences-inline))
  (font-lock-flush)
  (message "fences: %s" (if ygg-markdown-fences-inline "inline" "raw")))

(defvar ygg-markdown-fences--buffers nil
  "Hidden buffers, one per major mode, kept for fontifying code.")

(defvar ygg-markdown-fences--cache (make-hash-table :test 'equal)
  "Fontified bodies by mode and text; a streaming answer asks again.")

(defconst ygg-markdown-fences--cache-max 64
  "Bodies kept before the cache is emptied.")

(defun ygg-markdown-fences--buffer (mode)
  "A hidden buffer already in MODE."
  (let ((buffer (get-buffer-create (format " *ygg-fence:%s*" mode))))
    (with-current-buffer buffer
      (unless (eq major-mode mode)
        (delay-mode-hooks (funcall mode))
        (setq-local inhibit-modification-hooks t)
        (buffer-disable-undo)))
    buffer))

(defun ygg-markdown-fences--fontify (mode body)
  "BODY fontified in MODE, plain on any error."
  (let ((key (cons mode body)))
    (or (gethash key ygg-markdown-fences--cache)
        (let ((out (copy-sequence body)))
          (ignore-errors
            (with-current-buffer (ygg-markdown-fences--buffer mode)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert body))
              (syntax-ppss-flush-cache (point-min))
              (syntax-propertize (point-max))
              (font-lock-ensure)
              (let ((pos (point-min)))
                (while (< pos (point-max))
                  (let ((next (next-single-property-change
                               pos 'face nil (point-max)))
                        (face (get-text-property pos 'face)))
                    (when face
                      (put-text-property (1- pos) (1- next) 'face face out))
                    (setq pos next))))))
          (when (>= (hash-table-count ygg-markdown-fences--cache)
                    ygg-markdown-fences--cache-max)
            (clrhash ygg-markdown-fences--cache))
          (puthash key out ygg-markdown-fences--cache)))))

(defun ygg-markdown-fences-propertize (text hide)
  "TEXT with its fenced blocks fontified and drawn, HIDE hiding the fences.
Only the code is looked at, so the cost follows the code, not the text.
A block still open at the end of TEXT runs to its end."
  (let ((inline ygg-markdown-fences-inline))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((budget ygg-markdown-fences-code-max)
            (found nil))
        (while (re-search-forward ygg-markdown-fences--fence-re nil t)
          (let* ((open (match-beginning 0))
                 (body-beg (match-end 0))
                 (lang (match-string 2))
                 (close (concat "^[ \t]*" (regexp-quote (substring (match-string 1) 0 1))
                                "\\{" (number-to-string (length (match-string 1)))
                                ",\\}[ \t]*$"))
                 (closed (re-search-forward close nil t))
                 (body-end (if closed (match-beginning 0) (point-max)))
                 (close-end (if closed (min (point-max) (1+ (match-end 0)))
                              (point-max)))
                 (mode (and (not (string-empty-p lang))
                            (ygg-markdown-fences-mode lang)))
                 (size (- body-end body-beg)))
            (when (and mode (<= size budget))
              (cl-decf budget size)
              (setq found t)
              (let ((faced (ygg-markdown-fences--fontify
                            mode (buffer-substring-no-properties body-beg body-end)))
                    (pos 0))
                (while (< pos (length faced))
                  (let ((next (next-single-property-change pos 'face faced
                                                           (length faced)))
                        (face (get-text-property pos 'face faced)))
                    (when face
                      (put-text-property (+ body-beg pos) (+ body-beg next)
                                         'face face))
                    (setq pos next))))
              (when inline
                (ygg-markdown-fences--dress
                 (list open body-beg body-end close-end) lang hide)))
            (goto-char close-end)))
        (when found
          (let ((pos (point-min)))
            (while (< pos (point-max))
              (let ((next (next-single-property-change pos 'face nil (point-max)))
                    (face (get-text-property pos 'face)))
                (when face
                  (put-text-property pos next 'font-lock-face face))
                (setq pos next)))
            (remove-text-properties (point-min) (point-max) '(face nil))))
        (if found (buffer-string) text)))))

(provide 'ygg-markdown-fences)
;;; ygg-markdown-fences.el ends here
