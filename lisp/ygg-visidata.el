;;; ygg-visidata.el --- VisiData as the data frame viewer -*- lexical-binding: t; -*-

;; Third-party: emacs-jupyter (the kernel client), ghostel (the terminal),
;; VisiData (vd, installed with uv tool install visidata).  The kernel
;; writes the object to a temp file; vd shows it in a ghostel window, and
;; quitting vd takes the window and the file with it.  A kernel on a TRAMP
;; host writes into a temp directory there; the file is copied home over
;; TRAMP and the host's directory removed at once, so vd always runs here
;; with its paper rc and the host needs nothing but the kernel.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'thingatpt)
(require 'ygg-kernel-picker)

(declare-function jupyter-eval "jupyter-client" (code &optional mime))
(declare-function jupyter-kernel-language "jupyter-client" (&optional client))
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ghostel-semi-char-mode "ghostel")
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function ygg-nb--client-here "layer-notebook" ())
(declare-function ygg--term-display-window "layer-terminal" (buffer alist))
(declare-function ygg--term-delete-window "layer-terminal" ())
(defvar jupyter-current-client)
(defvar jupyter-default-timeout)
(defvar ghostel-kill-buffer-on-exit)
(defvar ygg-term-split)
(defvar ygg-term-height-fraction)
(defvar ygg-term-width-fraction)

(defgroup ygg-visidata nil
  "VisiData as the data frame viewer."
  :group 'yggdrasil)

(defcustom ygg-visidata-program "vd"
  "The VisiData executable."
  :type 'string)

(defcustom ygg-visidata-args nil
  "Extra vd arguments, placed before the file."
  :type '(repeat string))

(defcustom ygg-visidata-timeout 120
  "Seconds a kernel may take to write an object out."
  :type 'number)

(defcustom ygg-visidata-split 'below
  "Side the vd window opens on: below or right."
  :type '(choice (const below) (const right)))

(defcustom ygg-visidata-size-fraction 0.5
  "Fraction of the frame the vd window takes, on its side."
  :type 'number)

(defconst ygg-visidata-config
  (expand-file-name "../etc/visidatarc"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The rc vd runs with: monochrome, then the user's own rc on top.")

(defconst ygg-visidata-file-extensions
  '("csv" "tsv" "tab" "parquet" "json" "jsonl" "ndjson" "xlsx" "xls"
    "feather" "arrow" "sqlite" "db")
  "Extensions opened in vd rather than as text.")

;;; Kernel code

(defconst ygg-visidata--python-export
  "def export(obj, directory, stem):
    import os
    base = os.path.join(directory, stem)
    if hasattr(obj, 'write_parquet'):
        obj.write_parquet(base + '.parquet')
        return base + '.parquet'
    import pandas as pd
    frame = obj if isinstance(obj, pd.DataFrame) else pd.DataFrame(obj)
    if not isinstance(frame.index, pd.RangeIndex):
        frame = frame.reset_index()
    frame = frame.rename(columns=str)
    try:
        frame.to_parquet(base + '.parquet', index=False)
        return base + '.parquet'
    except Exception:
        if os.path.exists(base + '.parquet'):
            os.remove(base + '.parquet')
        frame.to_csv(base + '.csv', index=False)
        return base + '.csv'
"
  "Python that writes an object to parquet, or csv without pyarrow.")

(defconst ygg-visidata--r-export
  "local({
  obj <- %s
  base <- file.path(%s, %s)
  if (!is.data.frame(obj)) obj <- as.data.frame(obj)
  if (requireNamespace(\"arrow\", quietly = TRUE)) {
    path <- paste0(base, \".parquet\")
    arrow::write_parquet(obj, path)
  } else if (requireNamespace(\"data.table\", quietly = TRUE)) {
    path <- paste0(base, \".csv\")
    data.table::fwrite(obj, path)
  } else {
    path <- paste0(base, \".csv\")
    utils::write.csv(obj, path, row.names = .row_names_info(obj) > 0)
  }
  path
})"
  "R that writes an object to parquet, or csv without arrow; installs nothing.")

(defconst ygg-visidata--frame-lister
  '(("python" . "[k for k, v in globals().items() if not k.startswith('_') and type(v).__name__ in ('DataFrame', 'Series')]")
    ("r" . "Filter(function(n) is.data.frame(get(n, envir = globalenv())), ls(globalenv()))"))
  "Per kernel language, an expression naming the data frames in the session.")

(defun ygg-visidata--literal (string)
  "STRING as a double-quoted literal both Python and R read back unchanged."
  (json-encode-string string))

(defun ygg-visidata--stem (name)
  "A file stem for the object NAME, safe on any file system."
  (let ((stem (replace-regexp-in-string "[^[:alnum:]_.-]+" "_" name)))
    (if (string-empty-p (string-trim stem "[_.]+" "[_.]+")) "data" stem)))

(defun ygg-visidata--language (client)
  (downcase (format "%s" (jupyter-kernel-language client))))

(defun ygg-visidata--exportable-language (client)
  "CLIENT's language, when VisiData can take data frames from it."
  (let ((language (ygg-visidata--language client)))
    (if (assoc language ygg-visidata--frame-lister)
        language
      (user-error "VisiData: no export for %s kernels" language))))

(defun ygg-visidata-kernel-code (language name directory)
  "Code for a LANGUAGE kernel that writes object NAME into DIRECTORY."
  (let ((dir (ygg-visidata--literal (directory-file-name directory)))
        (stem (ygg-visidata--literal (ygg-visidata--stem name))))
    (pcase (downcase language)
      ("python"
       (format "(lambda __ygg_vd_ns: (exec(%s, __ygg_vd_ns), __ygg_vd_ns['export'](%s, %s, %s))[1])({})"
               (ygg-visidata--literal ygg-visidata--python-export)
               name dir stem))
      ("r" (format ygg-visidata--r-export name dir stem))
      (other (user-error "VisiData: no export for %s kernels" other)))))

;;; Kernel

(defun ygg-visidata--local-url-p (url)
  (member (url-host (url-generic-parse-url url)) '("localhost" "127.0.0.1" "::1" "[::1]")))

(defun ygg-visidata--remote (client)
  "TRAMP prefix of CLIENT's kernel host, or nil when local.
A kernel behind a server on another machine is refused: TRAMP cannot reach it."
  (when-let* ((url (ygg-kernel-picker-server-url client))
              ((not (ygg-visidata--local-url-p url))))
    (user-error "VisiData: the kernel runs behind %s, whose files are out of reach" url))
  (ygg-kernel-picker-remote client))

(defun ygg-visidata--workspace (remote)
  "A new temp directory for the kernel to write into, on REMOTE's host if set."
  (if remote
      (let ((default-directory remote))
        (make-nearby-temp-file "ygg-vd-" t))
    (make-temp-file "ygg-vd-" t)))

(defun ygg-visidata--written (directory)
  "The file the kernel wrote into DIRECTORY, looked up past TRAMP's cache."
  (let ((remote-file-name-inhibit-cache t))
    (car (directory-files directory t "\\`[^.]"))))

(defun ygg-visidata-home-name (file home)
  "Where FILE, written on a kernel's host, lands in the local directory HOME."
  (expand-file-name (file-name-nondirectory file) home))

(defun ygg-visidata--copy-home (file)
  "A copy of the remote FILE in a new local temp directory."
  (let* ((home (make-temp-file "ygg-vd-" t))
         (copy (ygg-visidata-home-name file home)))
    (condition-case err
        (progn (copy-file file copy t) copy)
      (error (ygg-visidata--remove-directory home)
             (signal (car err) (cdr err))))))

(defun ygg-visidata--export (client language name)
  "Have CLIENT's kernel write object NAME out; return the local file to open.
The directory holding the file goes when vd's buffer does."
  (let* ((remote (ygg-visidata--remote client))
         (directory (ygg-visidata--workspace remote))
         file)
    (unwind-protect
        (progn
          (condition-case err
              (ygg-visidata--eval client (ygg-visidata-kernel-code
                                          language name (file-local-name directory)))
            (error (user-error "VisiData: %s" (error-message-string err))))
          (setq file (or (ygg-visidata--written directory)
                         (user-error "VisiData: the kernel wrote nothing for %s" name)))
          (when remote
            (setq file (ygg-visidata--copy-home file))))
      (when (or remote (not file))
        (ygg-visidata--remove-directory directory)))
    file))

(defun ygg-visidata--client ()
  "The kernel client this buffer evaluates in."
  (or (and (fboundp 'ygg-nb--client-here)
           (ignore-errors (ygg-nb--client-here)))
      (bound-and-true-p jupyter-current-client)
      (user-error "No kernel here; start or join one first")))

(defun ygg-visidata--eval (client code)
  "Evaluate CODE in CLIENT's kernel off the record: no echo, no history."
  (require 'jupyter-client)
  (let ((jupyter-current-client client)
        (jupyter-default-timeout ygg-visidata-timeout))
    (jupyter-eval code)))

(defun ygg-visidata--quoted-names (text)
  "The quoted strings in TEXT, a list or vector a kernel printed."
  (let (names (start 0))
    (while (string-match "[\"']\\([^\"']+\\)[\"']" (or text "") start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    (nreverse names)))

(defun ygg-visidata--kernel-frames (client)
  (when-let* ((code (alist-get (ygg-visidata--language client)
                               ygg-visidata--frame-lister nil nil #'equal)))
    (ygg-visidata--quoted-names (ygg-visidata--eval client code))))

(defun ygg-visidata--read-name (client pick)
  "The object to view: the symbol at point, or one chosen from CLIENT when PICK."
  (let ((here (thing-at-point 'symbol t)))
    (if (and here (not pick))
        here
      (let ((frames (ygg-visidata--kernel-frames client)))
        (completing-read (format-prompt "View" (and (member here frames) here))
                         frames nil nil nil nil (and (member here frames) here))))))

;;; Terminal

(defun ygg-visidata--program ()
  (or (executable-find ygg-visidata-program)
      (user-error "VisiData not found; uv tool install visidata")))

(defun ygg-visidata-command-line (file)
  "The argv that opens FILE in vd with the paper rc."
  (append (list (ygg-visidata--program) "--config" ygg-visidata-config)
          ygg-visidata-args
          (list (expand-file-name file))))

(defun ygg-visidata--remove-directory (directory)
  (when (file-directory-p directory)
    (delete-directory directory t)))

(defun ygg-visidata-remove-with-buffer (buffer directory)
  "Delete DIRECTORY, and everything in it, when BUFFER is killed."
  (with-current-buffer buffer
    (add-hook 'kill-buffer-hook
              (lambda () (ygg-visidata--remove-directory directory))
              nil t)))

(defun ygg-visidata--display (buffer)
  (if (fboundp 'ygg--term-display-window)
      (let ((ygg-term-split ygg-visidata-split)
            (ygg-term-height-fraction ygg-visidata-size-fraction)
            (ygg-term-width-fraction ygg-visidata-size-fraction))
        (display-buffer buffer '((ygg--term-display-window))))
    (display-buffer buffer '((display-buffer-at-bottom)
                             (window-height . 0.5)))))

(defun ygg-visidata--run (file title &optional temp-directory)
  "Open FILE in vd in a terminal window named after TITLE.
TEMP-DIRECTORY goes when the window does."
  (require 'ghostel)
  (let* ((argv (ygg-visidata-command-line file))
         ;; a remote default-directory would spawn vd on the far host
         (buffer (let ((default-directory temporary-file-directory))
                   (generate-new-buffer (format "*vd: %s*" title))))
         window)
    (condition-case err
        (progn (setq window (ygg-visidata--display buffer))
               (ghostel-exec buffer (car argv) (cdr argv)))
      (error (when (and (window-live-p window) (not (frame-root-window-p window)))
               (delete-window window))
             (kill-buffer buffer)
             (when temp-directory
               (ygg-visidata--remove-directory temp-directory))
             (signal (car err) (cdr err))))
    ;; ghostel-exec switches the major mode, which clears locals set earlier
    (with-current-buffer buffer
      (setq-local ghostel-kill-buffer-on-exit t)
      (setq-local mode-line-format nil)
      (when (fboundp 'ygg--term-delete-window)
        (add-hook 'kill-buffer-hook #'ygg--term-delete-window nil t)))
    (when temp-directory
      (ygg-visidata-remove-with-buffer buffer temp-directory))
    (when (window-live-p window)
      (select-window window))
    (when (fboundp 'ghostel-semi-char-mode)
      (ghostel-semi-char-mode))
    buffer))

;;; Commands

;;;###autoload
(defun ygg-visidata-view (name &optional client)
  "View the kernel object NAME in VisiData.
NAME is the symbol at point; with a prefix argument, or none at point,
it is picked from the data frames the kernel holds."
  (interactive
   (let ((client (ygg-visidata--client)))
     (ygg-visidata--exportable-language client)
     (list (ygg-visidata--read-name client current-prefix-arg) client)))
  (ygg-visidata--program)
  (let* ((client (or client (ygg-visidata--client)))
         (language (ygg-visidata--exportable-language client))
         (file (ygg-visidata--export client language name)))
    (when (string-suffix-p ".csv" file)
      (message "VisiData: %s written as csv; %s" name
               (if (equal language "r")
                   "R has no arrow package for parquet"
                 "parquet failed in the kernel (pyarrow missing, or mixed-type columns)")))
    (ygg-visidata--run file name (file-name-directory file))))

(defun ygg-visidata--file-here ()
  (let ((file (cond ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
                    ((thing-at-point 'existing-filename t))
                    (buffer-file-name))))
    (and file (file-regular-p file)
         (member (downcase (or (file-name-extension file) ""))
                 ygg-visidata-file-extensions)
         file)))

;;;###autoload
(defun ygg-visidata-open-file (file)
  "Open the data FILE in VisiData: the one at point or in dired, else asked."
  (interactive
   (list (or (ygg-visidata--file-here)
             (read-file-name "Open in VisiData: " nil nil t))))
  (when (file-remote-p file)
    (user-error "VisiData: %s is remote; vd runs here" file))
  (ygg-visidata--run file (file-name-nondirectory file)))

(provide 'ygg-visidata)
;;; ygg-visidata.el ends here
