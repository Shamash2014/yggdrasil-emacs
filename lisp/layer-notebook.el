;;; layer-notebook.el --- jupyter kernels, percent cells, Rmd and Quarto chunks -*- lexical-binding: t; -*-

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-motions)
(require 'yggdrasil-localleader)
(require 'yggdrasil-leader)
(require 'seq)
(require 'ring)
(require 'ansi-color)

(declare-function code-cells--bounds "code-cells")
(declare-function code-cells-mode-maybe "code-cells")
(declare-function jupyter-eval-region "jupyter-client")
(declare-function jupyter-kernel-language "jupyter-client")
(declare-function jupyter-kernel-info "jupyter-client")
(declare-function jupyter-run-with-state "jupyter-monads")
(declare-function jupyter-reply "jupyter-monads")
(declare-function jupyter-execute-request "jupyter-messages")
(declare-function jupyter-message-content "jupyter-messages")
(declare-function jupyter-run-repl "jupyter-repl")
(declare-function jupyter-repl-available-repl-buffers "jupyter-repl")
(declare-function jupyter-repl-restart-kernel "jupyter-repl")
(declare-function jupyter-repl-interrupt-kernel "jupyter-repl")
(declare-function jupyter-repl-clear-cells "jupyter-repl")
(declare-function jupyter-repl-send "jupyter-repl")
(declare-function jupyter-repl-forward-cell "jupyter-repl")
(declare-function jupyter-repl-backward-cell "jupyter-repl")
(declare-function jupyter-repl-cell-code-beginning-position "jupyter-repl")
(declare-function jupyter-repl-replace-cell-code "jupyter-repl")
(declare-function jupyter-repl-history-previous "jupyter-repl")
(declare-function jupyter-repl-history-next "jupyter-repl")
(declare-function jupyter-repl-clear-input "jupyter-repl")
(declare-function jupyter-repl-scratch-buffer "jupyter-repl")
(declare-function jupyter-inspect-at-point "jupyter-client")
(declare-function jupyter-eval-defun "jupyter-client")
(declare-function jupyter-eval-string-command "jupyter-client")
(declare-function jupyter-load-file "jupyter-client")
(declare-function jupyter-eval-remove-overlays "jupyter-client")
(declare-function ygg-kernel-picker "ygg-kernel-picker")
(declare-function ygg-kernel-picker--attach "ygg-kernel-picker")
(declare-function pm-turn-polymode-off "polymode-core")
(declare-function poly-markdown-mode "poly-markdown")
(declare-function python-shell-get-process "python")
(declare-function python-shell-send-string "python")
(declare-function ygg-browser-open "layer-browser")
(declare-function ygg-selection-effective-bounds "yggdrasil-selection")
(declare-function slot-value "eieio")
(defvar code-cells-boundary-regexp)
(defvar code-cells-eval-region-commands)
(defvar jupyter-current-client)
(defvar jupyter-kernel-language-mode-properties)
(defvar jupyter-repl-echo-eval-p)
(defvar jupyter-repl-history)
(defvar polymode-mode-name-aliases)

(defgroup ygg-notebook nil
  "Jupyter kernels, percent cells and literate chunks."
  :group 'yggdrasil
  :prefix "ygg-nb-")

(defcustom ygg-nb-kernels '(("python" . "python3") ("r" . "ark-console")
                            ("ruby" . "ruby3") ("rust" . "rust") ("go" . "gophernotes")
                            ("kotlin" . "kotlin") ("java" . "java"))
  "Jupyter kernelspec started for each cell language."
  :type '(alist :key-type string :value-type string))

(defconst ygg-nb-kernel-languages
  '(("ruby" ruby-ts-mode ruby-mode)
    ("rust" rust-ts-mode rust-mode)
    ("go" go-ts-mode go-mode)
    ("kotlin" kotlin-ts-mode kotlin-mode)
    ("java" java-ts-mode java-mode))
  "Kernel languages besides python and R, with their modes, tree-sitter first.")

(defconst ygg-nb--kernel-language-modes
  (mapcan (lambda (entry) (copy-sequence (cdr entry))) ygg-nb-kernel-languages))

(defconst ygg-nb-slash-cell-regexp
  (rx line-start "//" (* blank) "%" (group-n 1 (+ "%")))
  "A percent cell header in a language whose comments start with two slashes.")

(defcustom ygg-nb-quarto-python
  (expand-file-name "~/.local/share/jupyter-python/bin/python")
  "Python with Jupyter installed, handed to quarto as QUARTO_PYTHON."
  :type 'file)

(defconst ygg-nb-chunk-head-regexp
  "^[ \t]*```+[ \t]*{[ \t]*\\([[:alpha:]][[:alnum:]_-]*\\)[^}\n]*}[ \t]*$"
  "An executable chunk header; group 1 is the language.")

(defconst ygg-nb-chunk-tail-regexp "^[ \t]*```+[ \t]*$")

(defface ygg-nb-chunk-head '((t :inherit shadow :overline t :extend t))
  "Chunk header line: a thin rule in shadow grey.")

(defface ygg-nb-chunk-tail '((t :inherit shadow))
  "Chunk closing fence.")

;;; Packages

(defun ygg-nb--zmq-arm64 (configuration)
  "Name CONFIGURATION the way emacs-zmq names its Apple silicon release."
  (replace-regexp-in-string "\\`aarch64-apple-" "arm64-apple-" configuration))

(advice-add 'zmq--system-configuration :filter-return #'ygg-nb--zmq-arm64)

(defun ygg-nb--drop-md-polymode ()
  "Keep plain .md files in their markdown mode."
  (setq auto-mode-alist
        (seq-remove (lambda (entry) (equal entry '("\\.md\\'" . poly-markdown-mode)))
                    auto-mode-alist)))

(with-eval-after-load 'poly-markdown-autoloads (ygg-nb--drop-md-polymode))
(with-eval-after-load 'poly-markdown (ygg-nb--drop-md-polymode))

(add-to-list 'auto-mode-alist '("\\.[Rr]md\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-markdown-mode))

(when (fboundp 'elpaca)
  (elpaca (zmq :files (:defaults "Makefile" "src" "emacs-zmq.dylib")))
  (elpaca jupyter)
  (elpaca code-cells)
  (elpaca polymode)
  (elpaca poly-markdown))

(unless (or (getenv "QUARTO_PYTHON") (not (file-executable-p ygg-nb-quarto-python)))
  (setenv "QUARTO_PYTHON" ygg-nb-quarto-python))

(setq jupyter-repl-echo-eval-p t)

;;; Look

(defun ygg-nb--chunk-face (_chunkmode type)
  "Rule the chunk head, dim the tail, and leave the body unshaded."
  (pcase type
    ('head 'ygg-nb-chunk-head)
    ('tail 'ygg-nb-chunk-tail)))

(with-eval-after-load 'polymode
  (advice-add 'pm-get-adjust-face :override #'ygg-nb--chunk-face)
  (dolist (alias '((r . r-ts-mode) (R . r-ts-mode)))
    (add-to-list 'polymode-mode-name-aliases alias)))

(with-eval-after-load 'code-cells
  (face-spec-set 'code-cells-header-line
                 '((t :extend t :overline t :inherit shadow))
                 'face-defface-spec)
  (dolist (mode (append '(r-ts-mode python-mode python-base-mode)
                        ygg-nb--kernel-language-modes))
    (push (cons mode #'ygg-nb-eval-region) code-cells-eval-region-commands)))

(with-eval-after-load 'jupyter-repl
  (face-spec-set 'jupyter-repl-input-prompt '((t :inherit shadow)) 'face-defface-spec)
  (face-spec-set 'jupyter-repl-output-prompt '((t :inherit shadow)) 'face-defface-spec)
  (face-spec-set 'jupyter-repl-traceback '((t nil)) 'face-defface-spec))

(defun ygg-nb--usable-mode-p (mode)
  "Non-nil when MODE is defined and, for a tree-sitter mode, its grammar is here."
  (and (fboundp mode)
       (let ((name (symbol-name mode)))
         (or (not (string-suffix-p "-ts-mode" name))
             (treesit-language-available-p
              (intern (string-remove-suffix "-ts-mode" name)))))))

(defun ygg-nb-kernel-mode (language)
  "The major mode code in LANGUAGE is shown in, or nil when none is usable."
  (seq-find #'ygg-nb--usable-mode-p
            (cdr (assoc (downcase (format "%s" language)) ygg-nb-kernel-languages))))

(defun ygg-nb--seed-language-mode (client)
  "Map CLIENT's language to its mode before emacs-jupyter guesses by extension.
JJava reports the extension .jshell, which no mode claims."
  (let ((language (jupyter-kernel-language client)))
    (unless (assq language jupyter-kernel-language-mode-properties)
      (when-let* ((mode (ygg-nb-kernel-mode language)))
        (push (list language mode
                    (with-temp-buffer (delay-mode-hooks (funcall mode)) (syntax-table)))
              jupyter-kernel-language-mode-properties)))))

(defconst ygg-nb--python-compile-errors '("SyntaxError" "IndentationError" "TabError")
  "Errors IPython raises before running any of an expression's code.")

(defun ygg-nb--ipython-p (client)
  (equal (format "%s" (plist-get (jupyter-kernel-info client) :implementation))
         "ipython"))

(defun ygg-nb--eval-as-expression (eval code &optional mime)
  "Evaluate CODE as a user expression in an IPython kernel, else call EVAL.
IPython drops the result of an execute kept out of history when the
last stored cell ends in a semicolon; user expressions skip that check.
Code that is not an expression falls back to EVAL with CODE and MIME."
  (if (not (ygg-nb--ipython-p jupyter-current-client))
      (funcall eval code mime)
    (let* ((reply (jupyter-run-with-state jupyter-current-client
                    (jupyter-reply
                     (jupyter-execute-request
                      :code "" :silent t :store-history nil :allow-stdin nil
                      :user-expressions (list :value code) :handlers nil))))
           (value (plist-get (plist-get (jupyter-message-content reply) :user_expressions)
                             :value)))
      (cond ((equal (plist-get value :status) "ok")
             (plist-get (plist-get value :data) (or mime :text/plain)))
            ((member (plist-get value :ename) ygg-nb--python-compile-errors)
             (funcall eval code mime))
            (t (error "%s" (ansi-color-apply
                            (or (plist-get value :evalue) "evaluation failed"))))))))

(with-eval-after-load 'jupyter-client
  (face-spec-set 'jupyter-eval-overlay '((t :inherit shadow)) 'face-defface-spec)
  (advice-add 'jupyter-kernel-language-mode-properties :before
              #'ygg-nb--seed-language-mode)
  (advice-add 'jupyter-eval :around #'ygg-nb--eval-as-expression))

;;; Cells and chunks

(defun ygg-nb--chunked-p ()
  "Non-nil when cells here are fenced chunks rather than percent comments."
  (or (bound-and-true-p polymode-mode) (derived-mode-p 'markdown-mode)))

(defun ygg-nb--boundary-regexp ()
  (if (ygg-nb--chunked-p)
      ygg-nb-chunk-head-regexp
    (require 'code-cells)
    code-cells-boundary-regexp))

(defun ygg-nb--repl-boundary (direction)
  "Code start of the next REPL cell when DIRECTION is positive, else the previous."
  (save-excursion
    (let ((start (point)))
      (ignore-errors
        (if (> direction 0) (jupyter-repl-forward-cell) (jupyter-repl-backward-cell))
        (and (if (> direction 0) (> (point) start) (< (point) start))
             (point))))))

(defun ygg-nb--boundary (direction)
  "Start of the next cell header when DIRECTION is positive, else the previous."
  (if (derived-mode-p 'jupyter-repl-mode)
      (ygg-nb--repl-boundary direction)
    (let ((regexp (ygg-nb--boundary-regexp))
          (case-fold-search nil))
      (save-excursion
        (if (> direction 0)
            (progn (end-of-line)
                   (and (re-search-forward regexp nil t) (match-beginning 0)))
          (beginning-of-line)
          (and (re-search-backward regexp nil t) (point)))))))

(defun ygg-nb--chunk-at-point ()
  "The chunk around point as (LANGUAGE HEAD BODY TAIL AFTER), or nil."
  (let ((origin (point))
        (case-fold-search nil))
    (save-excursion
      (end-of-line)
      (when (re-search-backward ygg-nb-chunk-head-regexp nil t)
        (let ((head (match-beginning 0))
              (language (downcase (match-string-no-properties 1)))
              (body (line-beginning-position 2)))
          (goto-char body)
          (when (re-search-forward ygg-nb-chunk-tail-regexp nil t)
            (let ((tail (match-beginning 0))
                  (after (min (point-max) (1+ (line-end-position)))))
              (when (< origin after)
                (list language head body tail after)))))))))

(defun ygg-nb-cell-bounds (&optional around)
  "The cell at point as (BEG . END): its code, or with AROUND its fences too."
  (if (ygg-nb--chunked-p)
      (pcase (ygg-nb--chunk-at-point)
        (`(,_ ,head ,body ,tail ,after) (if around (cons head after) (cons body tail))))
    (require 'code-cells)
    (pcase-let ((`(,beg ,end) (code-cells--bounds 1 nil (not around))))
      (cons beg end))))

(defun ygg-nb--language ()
  "Language of the cell at point: the chunk header, else the major mode."
  (if (ygg-nb--chunked-p)
      (car (ygg-nb--chunk-at-point))
    (cond ((derived-mode-p 'python-base-mode 'python-mode) "python")
          ((derived-mode-p 'r-ts-mode 'ess-r-mode) "r")
          (t (car (seq-find (lambda (entry) (apply #'derived-mode-p (cdr entry)))
                            ygg-nb-kernel-languages))))))

(defun ygg-nb-next-cell ()
  "Go to the next cell or chunk header."
  (interactive)
  (ygg--record-bracket-motion 1 "%")
  (ygg--bracketed-goto (lambda () (ygg-nb--boundary 1))))

(defun ygg-nb-prev-cell ()
  "Go to the previous cell or chunk header."
  (interactive)
  (ygg--record-bracket-motion -1 "%")
  (ygg--bracketed-goto (lambda () (ygg-nb--boundary -1))))

(defun ygg-nb--textobject-bounds (c which)
  "The cell object on the percent key, inside or around per WHICH."
  (when (and (eq c ?%) (or (ygg-nb--chunked-p) (bound-and-true-p code-cells-mode)))
    (ygg-nb-cell-bounds (eq which 'around))))

(with-eval-after-load 'yggdrasil-match
  (advice-add 'ygg-match--textobject-bounds :before-until #'ygg-nb--textobject-bounds))

;;; Kernels

(defvar ygg-nb-kernel-hook nil
  "Run in a buffer once it has taken a kernel for a cell language.")

(defvar-local ygg-nb--kernels nil
  "Jupyter clients this buffer evaluates in, as (LANGUAGE . CLIENT).")

(defun ygg-nb--base ()
  (or (buffer-base-buffer) (current-buffer)))

(defun ygg-nb--live-p (client)
  (and client
       (ignore-errors (buffer-live-p (slot-value client 'buffer)))))

(defun ygg-nb--client (language)
  "The live jupyter client for LANGUAGE here, or nil."
  (let ((own (alist-get language (buffer-local-value 'ygg-nb--kernels (ygg-nb--base))
                        nil nil #'equal)))
    (cond ((ygg-nb--live-p own) own)
          ((and (not (ygg-nb--chunked-p)) (boundp 'jupyter-current-client)
                (ygg-nb--live-p jupyter-current-client))
           jupyter-current-client))))

(defun ygg-nb--client-here ()
  (or (ygg-nb--client (ygg-nb--language))
      (user-error "No kernel here; start one with SPC r k")))

(defun ygg-nb--remember (language client)
  (with-current-buffer (ygg-nb--base)
    (setf (alist-get language ygg-nb--kernels nil nil #'equal) client))
  ;; A polymode chunk buffer holds one language; a plain markdown buffer holds many.
  (when (or (not (ygg-nb--chunked-p)) (buffer-base-buffer))
    (setq-local jupyter-current-client client))
  (run-hooks 'ygg-nb-kernel-hook)
  client)

(defun ygg-nb--pick-repl ()
  "A running REPL's client, chosen by buffer name."
  (let ((buffers (mapcar #'buffer-name (jupyter-repl-available-repl-buffers))))
    (unless buffers (user-error "No running jupyter REPL"))
    (buffer-local-value 'jupyter-current-client
                        (get-buffer (completing-read "REPL: " buffers nil t)))))

(defun ygg-nb-kernel (&optional pick)
  "Start a jupyter kernel for the cell language here.
With PICK, join a running REPL instead."
  (interactive "P")
  (require 'jupyter)
  (let* ((language (or (ygg-nb--language) (user-error "No cell language here")))
         (spec (alist-get language ygg-nb-kernels nil nil #'equal))
         (client (if pick
                     (ygg-nb--pick-repl)
                   (unless spec (user-error "No kernel configured for %s" language))
                   (jupyter-run-repl spec nil nil nil
                                     (called-interactively-p 'interactive)))))
    (ygg-nb--remember language client)))

(defun ygg-nb-restart-kernel ()
  "Restart the kernel this cell evaluates in."
  (interactive)
  (jupyter-repl-restart-kernel (ygg-nb--client-here)))

(defun ygg-nb-interrupt-kernel ()
  "Interrupt the kernel this cell evaluates in."
  (interactive)
  (jupyter-repl-interrupt-kernel (ygg-nb--client-here)))

(defun ygg-nb-clear-repl ()
  "Clear the cells of this kernel's REPL buffer."
  (interactive)
  (with-current-buffer (slot-value (ygg-nb--client-here) 'buffer)
    (jupyter-repl-clear-cells)))

;;; Evaluation

(defun ygg-nb--python-send (code)
  (require 'python)
  (python-shell-send-string code (or (python-shell-get-process) (run-python nil nil t))))

(defun ygg-nb--r-send (code)
  "Send CODE to an inferior R, started on first use."
  (let ((process (or (get-buffer-process "*R*")
                     (get-buffer-process
                      (make-comint-in-buffer "R" "*R*" "R" nil "--no-save" "--no-restore"
                                             "--no-readline" "--quiet" "--interactive")))))
    (display-buffer (process-buffer process))
    (comint-send-string process (concat (string-trim-right code) "\n"))))

(defun ygg-nb-eval-region (beg end)
  "Evaluate BEG..END in this buffer's kernel, else an inferior python or R."
  (let* ((language (save-excursion (goto-char beg) (ygg-nb--language)))
         (client (ygg-nb--client language)))
    (cond (client
           (let ((jupyter-current-client client))
             (jupyter-eval-region nil beg end)))
          ((equal language "python")
           (ygg-nb--python-send (buffer-substring-no-properties beg end)))
          ((equal language "r")
           (ygg-nb--r-send (buffer-substring-no-properties beg end)))
          (t (user-error "No kernel or REPL for %s" (or language major-mode))))))

(defun ygg-nb--eval-bounds ()
  "The selection in visual state, else the code of the cell at point."
  (if (ygg-visual-p)
      (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
        (ygg-normal-state)
        (cons beg end))
    (or (ygg-nb-cell-bounds) (user-error "Not in a cell"))))

(defun ygg-nb-eval-cell ()
  "Evaluate the cell at point, or the selection."
  (interactive)
  (pcase-let ((`(,beg . ,end) (ygg-nb--eval-bounds)))
    (ygg-nb-eval-region beg end)))

(defun ygg-nb-eval-cell-and-next ()
  "Evaluate the cell at point and move to the next one."
  (interactive)
  (ygg-nb-eval-cell)
  (goto-char (or (ygg-nb--boundary 1) (point-max))))

(defun ygg-nb-eval-buffer ()
  "Evaluate every chunk in order, or the whole buffer of percent cells."
  (interactive)
  (if (not (ygg-nb--chunked-p))
      (ygg-nb-eval-region (point-min) (point-max))
    (save-excursion
      (goto-char (point-min))
      (let (head)
        (while (setq head (ygg-nb--boundary 1))
          (goto-char head)
          (forward-line 1)
          (pcase (ygg-nb-cell-bounds)
            (`(,beg . ,end) (ygg-nb-eval-region beg end) (goto-char end))))))))

;;; Quarto

(defvar ygg-nb--previews (make-hash-table :test #'equal)
  "Quarto preview per file, as (PROCESS . URL).")

(defun ygg-nb--quarto-file ()
  (let ((base (ygg-nb--base)))
    (unless (executable-find "quarto") (user-error "quarto is not on PATH"))
    (with-current-buffer base
      (unless buffer-file-name (user-error "Buffer visits no file"))
      (save-buffer)
      buffer-file-name)))

(defun ygg-nb-quarto-render ()
  "Render this file with quarto render."
  (interactive)
  (let* ((file (ygg-nb--quarto-file))
         (default-directory (file-name-directory file)))
    (compilation-start (concat "quarto render "
                               (shell-quote-argument (file-name-nondirectory file)))
                       nil (lambda (_) "*quarto render*"))))

(defun ygg-nb--open-url (url)
  (if (fboundp 'ygg-browser-open) (ygg-browser-open url) (browse-url url)))

(defun ygg-nb--preview-filter (file)
  "Process filter opening FILE's preview once quarto names its URL."
  (lambda (process output)
    (internal-default-process-filter process (ansi-color-filter-apply output))
    (let ((entry (gethash file ygg-nb--previews)))
      (unless (cdr entry)
        (with-current-buffer (process-buffer process)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "Browse at \\(https?://[^ \t\r\n]+\\)[ \t]*\r?\n" nil t)
              (setcdr entry (match-string-no-properties 1))
              (ygg-nb--open-url (cdr entry)))))))))

(defun ygg-nb-quarto-preview ()
  "Serve this file with quarto preview and open it in the browser."
  (interactive)
  (let* ((file (ygg-nb--quarto-file))
         (entry (gethash file ygg-nb--previews)))
    (cond
     ((and entry (process-live-p (car entry)) (cdr entry))
      (ygg-nb--open-url (cdr entry)))
     ((and entry (process-live-p (car entry)))
      (message "quarto preview still starting"))
     (t
      (let*((default-directory (file-name-directory file))
             (process-connection-type nil)
             (process (start-process "quarto-preview"
                                     (format "*quarto preview %s*" (file-name-nondirectory file))
                                     "quarto" "preview" (file-name-nondirectory file)
                                     "--no-browser")))
        (puthash file (list process) ygg-nb--previews)
        (set-process-filter process (ygg-nb--preview-filter file))
        (message "quarto preview starting"))))))

;;; Polymode

(defvar-local ygg-nb--pre-poly-mode nil
  "Major mode a markdown buffer had before polymode took it over.")
(put 'ygg-nb--pre-poly-mode 'permanent-local t)

(defun ygg-nb-toggle-polymode ()
  "Turn polymode chunks on or off in a markdown buffer."
  (interactive)
  (cond ((bound-and-true-p polymode-mode)
         (let ((base (ygg-nb--base)))
           (unless (eq base (current-buffer)) (switch-to-buffer base))
           (with-current-buffer base
             (pm-turn-polymode-off (or ygg-nb--pre-poly-mode t)))))
        ((derived-mode-p 'markdown-mode)
         (require 'poly-markdown)
         (setq ygg-nb--pre-poly-mode major-mode)
         (poly-markdown-mode))
        (t (user-error "Polymode only toggles in a markdown buffer"))))

(defun ygg-nb--cells-maybe ()
  "Percent cells in a kernel language's file, never in a chunk's inner buffer."
  (when (and (not (buffer-base-buffer)) (fboundp 'code-cells-mode-maybe))
    (when (string-prefix-p "//" (or comment-start ""))
      (setq-local code-cells-boundary-regexp ygg-nb-slash-cell-regexp))
    (code-cells-mode-maybe)))

(dolist (mode (append '(python-mode python-ts-mode r-ts-mode)
                      ygg-nb--kernel-language-modes))
  (add-hook (intern (format "%s-hook" mode)) #'ygg-nb--cells-maybe))

;;; REPL

(add-to-list 'ygg-modal-special-modes 'jupyter-repl-mode)

(defun ygg-nb--repl-input-start ()
  "Where the code of the REPL's last cell begins, or nil before its first prompt."
  (save-excursion
    (goto-char (point-max))
    (ignore-errors (jupyter-repl-cell-code-beginning-position))))

(defun ygg-nb--repl-insert-at-prompt ()
  "Move to the input when insert starts above it, where output is read-only."
  (when-let* ((start (ygg-nb--repl-input-start)))
    (when (< (point) start) (goto-char (point-max)))))

(defun ygg-nb--repl-open-line-at-prompt ()
  "Put the cursor at the end of the input when point is in read-only output."
  (when-let* ((start (ygg-nb--repl-input-start)))
    (when (< (point) start)
      (goto-char (point-max))
      (ygg-set-selection (point) (point)))))

(defun ygg-nb--repl-setup ()
  "Type at the prompt from the start, as comint REPLs do."
  (setq-local ygg-insert-elsewhere t
              ygg-open-line-redirect-function #'ygg-nb--repl-open-line-at-prompt)
  (add-hook 'ygg-insert-entry-hook #'ygg-nb--repl-insert-at-prompt nil t)
  (let ((buffer (current-buffer)))
    (run-with-timer 0 nil
                    (lambda ()
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (when yggdrasil-local-mode (ygg-insert-state))))))))

(add-hook 'jupyter-repl-mode-hook #'ygg-nb--repl-setup)

(defun ygg-nb--modal-display-buffer (buffer)
  "Make a jupyter output, traceback or pager BUFFER modal; q still quits."
  (with-current-buffer buffer
    (when (and (bound-and-true-p yggdrasil-global-mode)
               (eq major-mode 'special-mode)
               (not yggdrasil-local-mode))
      (ygg--modalize-special)))
  buffer)

(with-eval-after-load 'jupyter-base
  (advice-add 'jupyter-get-buffer-create :filter-return #'ygg-nb--modal-display-buffer))

(defmacro ygg-nb--with-client (&rest body)
  "Run BODY with the kernel this buffer evaluates in as the current client."
  (declare (indent 0))
  `(let ((jupyter-current-client (ygg-nb--client-here)))
     ,@body))

(defun ygg-nb--repl-buffer ()
  (slot-value (ygg-nb--client-here) 'buffer))

(defun ygg-nb--in-repl (command)
  "Call COMMAND in this kernel's REPL, showing the REPL when called elsewhere."
  (let ((repl (ygg-nb--repl-buffer)))
    (unless (eq repl (current-buffer)) (pop-to-buffer repl))
    (call-interactively command)))

(defun ygg-nb--evaluates-in-p (buffer client)
  "Non-nil when the file BUFFER sends its code to CLIENT."
  (and (buffer-file-name (or (buffer-base-buffer buffer) buffer))
       (or (eq (buffer-local-value 'jupyter-current-client buffer) client)
           (rassq client (buffer-local-value 'ygg-nb--kernels
                                             (or (buffer-base-buffer buffer) buffer))))))

(defun ygg-nb-repl ()
  "Show this buffer's REPL, or from the REPL go back to a buffer that uses it.
With no kernel here yet, pick one."
  (interactive)
  (cond
   ((derived-mode-p 'jupyter-repl-mode)
    (let ((client jupyter-current-client))
      (pop-to-buffer
       (or (seq-find (lambda (buffer) (ygg-nb--evaluates-in-p buffer client))
                     (buffer-list))
           (user-error "No file evaluates in this REPL")))))
   ((ygg-nb--client (ygg-nb--language))
    (pop-to-buffer (ygg-nb--repl-buffer))
    (goto-char (point-max)))
   (t (call-interactively #'ygg-kernel-picker))))

(defun ygg-nb-associate ()
  "Make this buffer evaluate in a REPL that is already running."
  (interactive)
  (require 'jupyter-repl)
  (ygg-kernel-picker--attach (ygg-nb--pick-repl)))

(defun ygg-nb-send ()
  "Send the REPL's input; elsewhere evaluate the selection or the line."
  (interactive)
  (if (derived-mode-p 'jupyter-repl-mode)
      (progn
        (goto-char (point-max))
        ;; jupyter skips its kernel-busy refusal only for its own send command
        (let ((this-command 'jupyter-repl-send))
          (jupyter-repl-send)))
    (pcase-let ((`(,beg . ,end) (if (ygg-visual-p)
                                    (ygg-nb--eval-bounds)
                                  (cons (line-beginning-position) (line-end-position)))))
      (ygg-nb-eval-region beg end))))

(defun ygg-nb-eval-defun ()
  "Evaluate the definition around point in this buffer's kernel."
  (interactive)
  (ygg-nb--with-client (jupyter-eval-defun)))

(defun ygg-nb-eval-expression ()
  "Read an expression and evaluate it in this buffer's kernel."
  (interactive)
  (ygg-nb--with-client (call-interactively #'jupyter-eval-string-command)))

(defun ygg-nb-load-file ()
  "Evaluate a whole file in this buffer's kernel."
  (interactive)
  (ygg-nb--with-client (call-interactively #'jupyter-load-file)))

(defun ygg-nb--selection-active-p ()
  "Non-nil only for a visual selection, not the region normal state keeps live."
  (and (ygg-visual-p) mark-active))

(defun ygg-nb-inspect ()
  "Show the kernel's documentation for the code at point, or the selection."
  (interactive)
  (let ((mark-active (ygg-nb--selection-active-p)))
    (ygg-nb--with-client (call-interactively #'jupyter-inspect-at-point))))

(defun ygg-nb-remove-overlays ()
  "Remove the inline results evaluation left in this buffer."
  (interactive)
  (ygg-nb--with-client (jupyter-eval-remove-overlays)))

(defun ygg-nb-scratch ()
  "Open a scratch buffer evaluating in this buffer's kernel."
  (interactive)
  (with-current-buffer (ygg-nb--repl-buffer)
    (jupyter-repl-scratch-buffer)))

(defun ygg-nb-history-previous ()
  "Replace the REPL's input with the previous history entry."
  (interactive)
  (ygg-nb--in-repl #'jupyter-repl-history-previous))

(defun ygg-nb-history-next ()
  "Replace the REPL's input with the next history entry."
  (interactive)
  (ygg-nb--in-repl #'jupyter-repl-history-next))

(defun ygg-nb-clear-input ()
  "Empty the REPL's input."
  (interactive)
  (ygg-nb--in-repl #'jupyter-repl-clear-input))

(defun ygg-nb--history-entries ()
  "The REPL's history, newest first, without the ring's end marker."
  (seq-filter #'stringp (ring-elements jupyter-repl-history)))

(defun ygg-nb--history-pick ()
  "Choose a history entry and make it the REPL's input."
  (interactive)
  (let* ((entries (ygg-nb--history-entries))
         (code (completing-read
                "History: "
                (lambda (string predicate action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity))
                    (complete-with-action action entries string predicate)))
                nil t)))
    (goto-char (point-max))
    (jupyter-repl-replace-cell-code code)))

(defun ygg-nb-history-search ()
  "Search the REPL's history and make the choice its input."
  (interactive)
  (ygg-nb--in-repl #'ygg-nb--history-pick))

;;; Keys

(yggdrasil-define-keys 'normal
  "] %" #'ygg-nb-next-cell :label "next cell"
  "[ %" #'ygg-nb-prev-cell :label "prev cell")

(autoload 'ygg-kernel-vars-toggle "ygg-kernel-vars" nil t)
(autoload 'ygg-visidata-view "ygg-visidata" nil t)
(autoload 'ygg-visidata-open-file "ygg-visidata" nil t)

(defvar ygg-leader-jupyter-map (make-sparse-keymap)
  "The r prefix: the jupyter REPL and kernel, from its REPL or a buffer using it.")

(yggdrasil-define-keys 'ygg-leader-jupyter-map
  "r" #'ygg-nb-repl :label "REPL / back"
  "a" #'ygg-nb-associate :label "attach to running REPL"
  "k" #'ygg-kernel-picker :label "kernel session"
  "x" #'ygg-nb-eval-cell :label "eval cell / selection"
  "n" #'ygg-nb-eval-cell-and-next :label "eval cell, next"
  "b" #'ygg-nb-eval-buffer :label "eval all cells"
  "s" #'ygg-nb-send :label "send input / line / selection"
  "d" #'ygg-nb-eval-defun :label "eval defun"
  "e" #'ygg-nb-eval-expression :label "eval expression"
  "f" #'ygg-nb-load-file :label "eval file"
  "h" #'ygg-nb-inspect :label "inspect at point"
  "v" #'ygg-kernel-vars-toggle :label "variables pane"
  "t" #'ygg-visidata-view :label "view data (vd)"
  "T" #'ygg-visidata-open-file :label "open data file (vd)"
  "o" #'ygg-nb-remove-overlays :label "clear inline results"
  "c" #'ygg-nb-scratch :label "scratch buffer"
  "P" #'ygg-nb-history-previous :label "history previous"
  "N" #'ygg-nb-history-next :label "history next"
  "/" #'ygg-nb-history-search :label "history search"
  "u" #'ygg-nb-clear-input :label "clear input"
  "i" #'ygg-nb-interrupt-kernel :label "interrupt kernel"
  "K" #'ygg-nb-restart-kernel :label "restart kernel"
  "l" #'ygg-nb-clear-repl :label "clear REPL")

(yggdrasil-leader-def "r" ygg-leader-jupyter-map "jupyter")

(defconst ygg-nb--document-keys
  '(("R" ygg-nb-quarto-render "quarto render")
    ("P" ygg-nb-quarto-preview "quarto preview")
    ("M" ygg-nb-toggle-polymode "polymode chunks")))

(pcase-dolist (`(,key ,command ,label) ygg-nb--document-keys)
  (dolist (mode '(markdown-mode gfm-mode))
    (yggdrasil-localleader-def mode key command label)))

(defun ygg-nb--chunk-document-keys ()
  "Reach the document keys from inside a chunk, whose major mode is the code's."
  (when (buffer-base-buffer)
    (apply #'yggdrasil-define-local-keys '(normal visual)
           (mapcan (lambda (spec) (list (concat "\\ " (car spec)) (cadr spec)))
                   ygg-nb--document-keys))))

(add-hook 'polymode-init-inner-hook #'ygg-nb--chunk-document-keys)

(provide 'layer-notebook)
;;; layer-notebook.el ends here
