;;; ygg-ast.el --- search by symbol kind, in the file and the workspace -*- lexical-binding: t; -*-

;;; Code:

(require 'yggdrasil-core)
(require 'layer-quickfix)
(require 'seq)
(require 'subr-x)

(declare-function ygg--imenu-flat "layer-lsp")
(declare-function eglot-current-server "eglot")
(declare-function eglot-uri-to-path "eglot")
(declare-function jsonrpc-request "jsonrpc")

(defvar treesit-simple-imenu-settings)

(defgroup ygg-ast nil
  "Search by symbol kind: function, variable or type."
  :group 'yggdrasil)

(defconst ygg-ast-kinds
  '(("func" :categories ("function" "functions" "method" "methods" "def")
     :symbol-kinds (6 12 9))
    ("var" :categories ("variable" "variables" "field" "constant")
     :symbol-kinds (13 14 8 7))
    ("type" :categories ("class" "classes" "type" "types" "struct" "enum"
                         "interface")
     :symbol-kinds (5 23 10 11 26)))
  "The three kinds the ast verb knows.
Each names the imenu category words that say the kind and the LSP
SymbolKind numbers a workspace symbol must carry to be one.")

(defcustom ygg-ast-request-timeout 5
  "Seconds a workspace symbol request may take before it is given up."
  :type 'number :group 'ygg-ast)

(defcustom ygg-ast-ripgrep-program "rg"
  "The ripgrep the workspace search falls back to without a server."
  :type 'string :group 'ygg-ast)

(defcustom ygg-ast-patterns
  '((elisp
     ("func" . "^\\((cl-)?(def(un|macro|subst|generic|method|alias)|ert-deftest|define-(derived-mode|minor-mode|globalized-minor-mode|advice|error))[[:space:]]+[^[:space:]()]*%s")
     ("var" . "^\\(def(var|var-local|var-keymap|const|custom|face|group)[[:space:]]+[^[:space:]()]*%s")
     ("type" . "^\\((cl-)?def(struct|class|type)[[:space:]]+\\(?[^[:space:]()]*%s"))
    (python
     ("func" . "^[[:space:]]*(async[[:space:]]+)?def[[:space:]]+\\w*%s")
     ("var" . "^[[:space:]]*\\w*%s\\w*[[:space:]]*[:=][^=]")
     ("type" . "^[[:space:]]*class[[:space:]]+\\w*%s"))
    (dart
     ("func" . "^[[:space:]]*((static|final|const|abstract|external|factory|late)[[:space:]]+)*((?P<lead>[A-Za-z_$][\\w<>,?\\[\\]$.]*)[[:space:]]+\\w*%s\\w*[[:space:]]*\\(|[A-Za-z_$][\\w<>,?\\[\\]$.]*[[:space:]]+get[[:space:]]+\\w*%s)")
     ("var" . "^[[:space:]]*(final|const|var|late|static)[[:space:]]+(\\w+[[:space:]]+)?\\w*%s")
     ("type" . "^[[:space:]]*((abstract|sealed|base|final|interface|mixin)[[:space:]]+)*(extension[[:space:]]+type|class|mixin|enum|extension|typedef)[[:space:]]+\\w*%s"))
    (go
     ("func" . "^func[[:space:]]+(\\([^)]*\\)[[:space:]]*)?\\w*%s")
     ("var" . "^[[:space:]]*((var|const)[[:space:]]+\\w*%s|\\w*%s\\w*[[:space:]]*:=)")
     ("type" . "^[[:space:]]*type[[:space:]]+\\w*%s"))
    (typescript
     ("func" . "(function[[:space:]]+\\w*%s|(const|let)[[:space:]]+\\w*%s\\w*[[:space:]]*=[[:space:]]*(async[[:space:]]+)?(\\(|\\w+[[:space:]]*=>)|^[[:space:]]*(public|private|protected|static|async|\\*)?[[:space:]]*\\w*%s\\w*[[:space:]]*\\([^)]*\\)[[:space:]]*[:{])")
     ("var" . "^[[:space:]]*(export[[:space:]]+)?(const|let|var|readonly)[[:space:]]+\\w*%s")
     ("type" . "^[[:space:]]*(export[[:space:]]+)?(declare[[:space:]]+)?(abstract[[:space:]]+)?(class|interface|type|enum)[[:space:]]+\\w*%s"))
    (javascript
     ("func" . "(function[[:space:]]+\\w*%s|(const|let|var)[[:space:]]+\\w*%s\\w*[[:space:]]*=[[:space:]]*(async[[:space:]]+)?(\\(|\\w+[[:space:]]*=>)|^[[:space:]]*(static|async|\\*)?[[:space:]]*\\w*%s\\w*[[:space:]]*\\([^)]*\\)[[:space:]]*\\{)")
     ("var" . "^[[:space:]]*(export[[:space:]]+)?(const|let|var)[[:space:]]+\\w*%s")
     ("type" . "^[[:space:]]*(export[[:space:]]+)?(class)[[:space:]]+\\w*%s"))
    (rust
     ("func" . "^[[:space:]]*(pub[^[:space:]]*[[:space:]]+)?(default[[:space:]]+)?(async[[:space:]]+)?(unsafe[[:space:]]+)?(extern[^[:space:]]*[[:space:]]+)?fn[[:space:]]+\\w*%s")
     ("var" . "^[[:space:]]*(pub[^[:space:]]*[[:space:]]+)?(static|const)[[:space:]]+(mut[[:space:]]+)?\\w*%s")
     ("type" . "^[[:space:]]*(pub[^[:space:]]*[[:space:]]+)?(struct|enum|trait|union|type)[[:space:]]+\\w*%s"))
    (elixir
     ("func" . "^[[:space:]]*def(p|delegate)?[[:space:]]+\\w*%s")
     ("var" . "^[[:space:]]*(@\\w*%s|\\w*%s\\w*[[:space:]]*=[^=])")
     ("type" . "^[[:space:]]*(defmodule|defprotocol|defstruct|defimpl|@type|@typep|@opaque)[[:space:]]*[[:alnum:]_.]*%s")))
  "Ripgrep patterns per language family, used when no server answers.
Every occurrence of %s in a pattern is replaced by the quoted name; the
pattern itself is ripgrep syntax, matched case-insensitively."
  :type '(alist :key-type symbol
                :value-type (alist :key-type string :value-type string))
  :group 'ygg-ast)

(defconst ygg-ast--mode-families
  '(("emacs-lisp\\|lisp-interaction" . elisp)
    ("python" . python)
    ("dart" . dart)
    ("go-\\|go\\'" . go)
    ("tsx\\|typescript" . typescript)
    ("js\\|javascript" . javascript)
    ("rust" . rust)
    ("elixir\\|heex" . elixir))
  "How a major mode name is read as one of the families of patterns.")

(defun ygg-ast--family (&optional mode)
  "The language family MODE belongs to, or nil when none is known."
  (let ((name (symbol-name (or mode major-mode))))
    (cdr (seq-find (lambda (cell) (string-match-p (car cell) name))
                   ygg-ast--mode-families))))

(defun ygg-ast--kind-plist (kind)
  "What KIND stands for, refusing a word that is not one of the three."
  (or (cdr (assoc kind ygg-ast-kinds))
      (user-error "ast: kind is one of %s"
                  (mapconcat #'car ygg-ast-kinds ", "))))

(defun ygg-ast--category-in-p (category names)
  "Whether CATEGORY reads as one of NAMES, either containing the other."
  (let ((c (downcase category)))
    (seq-some (lambda (n)
                (let ((d (downcase n)))
                  (or (string-search d c) (string-search c d))))
              names)))

(defun ygg-ast--kind-categories (kind)
  "The category names KIND answers to in this buffer.
When the buffer builds its imenu from treesit-simple-imenu-settings the
categories are its own, narrowed to the ones that say KIND; a mode whose
category words are none of them keeps the plain list."
  (let* ((words (plist-get (ygg-ast--kind-plist kind) :categories))
         (theirs (and (boundp 'treesit-simple-imenu-settings)
                      (delq nil (mapcar #'car treesit-simple-imenu-settings))))
         (narrowed (seq-filter (lambda (c) (ygg-ast--category-in-p c words))
                               theirs)))
    (or narrowed words)))

(defun ygg-ast--record (file line col text)
  "One result: FILE at LINE and COL, reading as TEXT."
  (list file line col text))

(defun ygg-ast--qf-line (record)
  "RECORD as the FILE:LINE:COL: TEXT line a quickfix list is made of."
  (format "%s:%d:%d: %s" (nth 0 record) (nth 1 record) (nth 2 record)
          (nth 3 record)))

(defun ygg-ast--file-matches (kind name)
  "Every KIND in this buffer whose name has NAME in it.
The buffer's imenu is flattened, and an entry is kept when one of the
categories it sits under says KIND; an entry under no category at all is
kept, so a flat index is searched whole."
  (let ((needle (downcase name))
        (categories (ygg-ast--kind-categories kind))
        (file (or buffer-file-name (buffer-name)))
        records)
    (dolist (entry (ygg--imenu-flat))
      (let* ((path (split-string (cdr entry) "/" t))
             (leaf (car (last path)))
             (above (butlast path)))
        (when (and leaf
                   (string-search needle (downcase leaf))
                   (or (null above)
                       (seq-some
                        (lambda (c) (ygg-ast--category-in-p c categories))
                        above)))
          (save-excursion
            (goto-char (car entry))
            (push (ygg-ast--record file (line-number-at-pos)
                                   (1+ (current-column)) (cdr entry))
                  records)))))
    (nreverse records)))

(defun ygg-ast--symbol-record (item)
  "The result ITEM stands for, or nil when it carries no location."
  (let* ((location (or (plist-get item :location) item))
         (uri (plist-get location :uri))
         (range (plist-get location :range))
         (start (plist-get range :start)))
    (when uri
      (ygg-ast--record (eglot-uri-to-path uri)
                       (1+ (or (plist-get start :line) 0))
                       (1+ (or (plist-get start :character) 0))
                       (or (plist-get item :name) "")))))

(defun ygg-ast--workspace-matches (kind name)
  "Every KIND the server knows whose name has NAME in it."
  (let* ((server (eglot-current-server))
         (kinds (plist-get (ygg-ast--kind-plist kind) :symbol-kinds))
         (reply (jsonrpc-request server :workspace/symbol (list :query name)
                                 :timeout ygg-ast-request-timeout)))
    (delq nil
          (mapcar (lambda (item)
                    (when (memq (plist-get item :kind) kinds)
                      (ygg-ast--symbol-record item)))
                  (append reply nil)))))

(defun ygg-ast--pattern (kind family)
  "The ripgrep pattern for KIND in FAMILY, or nil when there is none."
  (cdr (assoc kind (cdr (assq family ygg-ast-patterns)))))

(defun ygg-ast--rg-lines (root pattern)
  "Ripgrep output for PATTERN under ROOT, one string per hit."
  (unless (executable-find ygg-ast-ripgrep-program)
    (user-error "ast: no server here and no ripgrep to fall back on"))
  (with-temp-buffer
    (let ((default-directory root))
      (process-file ygg-ast-ripgrep-program nil t nil
                    "-n" "-H" "--column" "--no-heading" "-i" "-e" pattern "."))
    (split-string (buffer-string) "\n" t)))

(defconst ygg-ast--rg-line-re
  "\\`\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):[ \t]*\\(.*\\)\\'"
  "How a ripgrep hit with a column is read apart.")

(defun ygg-ast--grep-matches (kind name root)
  "Every KIND under ROOT whose name has NAME in it, found by ripgrep."
  (let* ((family (or (ygg-ast--family)
                     (user-error "ast: no patterns for %s" major-mode)))
         (template (or (ygg-ast--pattern kind family)
                       (user-error "ast: no %s pattern for %s" kind family)))
         (pattern (string-replace "%s" (regexp-quote name) template)))
    (delq nil
          (mapcar (lambda (line)
                    (when (string-match ygg-ast--rg-line-re line)
                      (ygg-ast--record
                       (expand-file-name (match-string 1 line) root)
                       (string-to-number (match-string 2 line))
                       (string-to-number (match-string 3 line))
                       (string-trim (match-string 4 line)))))
                  (ygg-ast--rg-lines root pattern)))))

(defun ygg-ast--jump (record)
  "Go to what RECORD points at, leaving a mark where the search began."
  (let ((file (nth 0 record)))
    (push-mark)
    (when (and (file-name-absolute-p file)
               (not (and buffer-file-name
                         (file-equal-p file buffer-file-name))))
      (find-file file))
    (goto-char (point-min))
    (forward-line (1- (nth 1 record)))
    (move-to-column (max 0 (1- (nth 2 record))))))

(defun ygg-ast--present (records kind name root)
  "Show RECORDS for KIND and NAME, relative to ROOT.
One match is jumped to, several fill the quickfix and open it."
  (cond
   ((null records) (user-error "ast: no %s matching %s" kind name))
   ((null (cdr records)) (ygg-ast--jump (car records)))
   (t (let ((default-directory root))
        (ygg-qf--collect (mapcar #'ygg-ast--qf-line records) t)))))

(defun ygg-ast--root ()
  "The project root the workspace search covers."
  (or (when-let* ((p (project-current))) (project-root p))
      default-directory))

;;;###autoload
(defun ygg-ast-search (kind name &optional workspace)
  "Find every KIND whose name has NAME in it, case-insensitively.
KIND is func, var or type.  Without WORKSPACE the current buffer's own
symbols are searched through its imenu; with it the whole workspace is,
through the language server when one manages the buffer and through
ripgrep over the project root when none does."
  (interactive
   (list (completing-read "kind: " (mapcar #'car ygg-ast-kinds) nil t)
         (read-string "name: ")
         (y-or-n-p "whole workspace? ")))
  (ygg-ast--kind-plist kind)
  (when (string-empty-p (string-trim name))
    (user-error "usage: :ast[!] %s NAME" (mapconcat #'car ygg-ast-kinds "|")))
  (let ((root (ygg-ast--root)))
    (ygg-ast--present
     (if workspace
         (if (and (fboundp 'eglot-current-server) (eglot-current-server))
             (ygg-ast--workspace-matches kind name)
           (ygg-ast--grep-matches kind name root))
       (ygg-ast--file-matches kind name))
     kind name root)))

;;; The ast verb on the colon line, the bang widening it to the workspace

(defvar ygg-ex--commands)

(defun ygg-ex--cmd-ast (_range bang args)
  "Search by symbol kind: ARGS names the kind and then the name.
BANG widens the search from the current file to the whole workspace."
  (let* ((parts (split-string args nil t))
         (kind (car parts))
         (name (string-join (cdr parts) " ")))
    (unless (and kind (not (string-empty-p name)))
      (user-error "usage: :ast[!] %s NAME"
                  (mapconcat #'car ygg-ast-kinds "|")))
    (ygg-ast-search kind name (and bang t))))

(with-eval-after-load 'yggdrasil-ex
  (setf (alist-get "ast" ygg-ex--commands nil nil #'equal)
        'ygg-ex--cmd-ast))

(provide 'ygg-ast)
;;; ygg-ast.el ends here
