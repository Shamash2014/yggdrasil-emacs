;;; ygg-r-mode.el --- Tree-sitter major mode for R -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)

(defgroup r-ts nil
  "R editing on the r-lib tree-sitter grammar."
  :group 'languages)

(defcustom r-ts-mode-indent-offset 2
  "Columns one indentation step adds in R code."
  :type 'integer
  :safe #'integerp)

(add-to-list 'treesit-language-source-alist
             '(r "https://github.com/r-lib/tree-sitter-r" "v1.3.0"))

(defvar r-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (dolist (c '(?% ?+ ?- ?* ?/ ?^ ?< ?> ?= ?! ?& ?| ?~ ?$ ?@ ?: ??))
      (modify-syntax-entry c "." table))
    table)
  "Syntax table for R buffers.")

(defun r-ts-mode--function-assignment-p (node)
  "Non-nil when NODE binds a name to a function definition."
  (and node
       (equal (treesit-node-type node) "binary_operator")
       (member (treesit-node-text (treesit-node-child-by-field-name node "operator") t)
               '("<-" "<<-" "="))
       (equal (treesit-node-type (treesit-node-child-by-field-name node "rhs"))
              "function_definition")))

(defun r-ts-mode--defun-p (node)
  "Non-nil when NODE is a named function binding or an anonymous function."
  (pcase (treesit-node-type node)
    ("binary_operator" (r-ts-mode--function-assignment-p node))
    ("function_definition"
     (not (r-ts-mode--function-assignment-p (treesit-node-parent node))))))

(defun r-ts-mode--defun-name (node)
  "The name NODE binds its function to, or nil for an anonymous one."
  (when (r-ts-mode--function-assignment-p node)
    (treesit-node-text (treesit-node-child-by-field-name node "lhs") t)))

(defun r-ts-mode--statement-p (node)
  "Non-nil when NODE is an expression standing on its own in a body."
  (and (treesit-node-check node 'named)
       (not (equal (treesit-node-type node) "comment"))
       (member (treesit-node-type (treesit-node-parent node))
               '("program" "braced_expression"))))

(defvar r-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'r
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'r
   :feature 'definition
   '((binary_operator
      lhs: (identifier) @font-lock-function-name-face
      operator: ["<-" "<<-" "="]
      rhs: (function_definition)))

   :language 'r
   :feature 'keyword
   '((function_definition name: _ @font-lock-keyword-face)
     ["if" "else" "for" "in" "while" "repeat"] @font-lock-keyword-face
     [(next) (break)] @font-lock-keyword-face
     ((call function: (identifier) @font-lock-keyword-face)
      (:equal @font-lock-keyword-face "return")))

   :language 'r
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'r
   :feature 'constant
   '([(true) (false) (null) (na) (inf) (nan) (dots) (dot_dot_i)]
     @font-lock-constant-face)

   :language 'r
   :feature 'number
   '([(integer) (float) (complex)] @font-lock-number-face)

   :language 'r
   :feature 'function-call
   '((call function: (identifier) @font-lock-function-call-face)
     (call function: (namespace_operator rhs: (identifier)
                                         @font-lock-function-call-face)))

   :language 'r
   :feature 'escape
   :override t
   '((escape_sequence) @font-lock-escape-face))
  "Tree-sitter font-lock settings for R buffers.")

(defun r-ts-mode--bol (pos)
  "Position of the first non-blank character on the line of POS."
  (save-excursion (goto-char pos) (back-to-indentation) (point)))

(defun r-ts-mode--same-line-p (a b)
  "Non-nil when position B sits on the line of the earlier position A."
  (<= b (save-excursion (goto-char a) (line-end-position))))

(defun r-ts-mode--first-inner-child (parent)
  "First child of bracketed PARENT after its opening token, or nil."
  (let ((open (treesit-node-child-by-field-name parent "open")))
    (and open (treesit-node-next-sibling open))))

(defun r-ts-mode--hanging-p (parent)
  "Non-nil when PARENT's contents start on a line below its opener."
  (let ((open (treesit-node-child-by-field-name parent "open"))
        (first (r-ts-mode--first-inner-child parent)))
    (or (null open) (null first)
        (member (treesit-node-type first) '(")" "]" "]]"))
        (not (r-ts-mode--same-line-p (treesit-node-start open)
                                     (treesit-node-start first))))))

(defun r-ts-mode--bracket-anchor (_node parent _bol)
  "Align with the first element, or hang from the opener's line."
  (if (r-ts-mode--hanging-p parent)
      (r-ts-mode--bol (treesit-node-start parent))
    (treesit-node-start (r-ts-mode--first-inner-child parent))))

(defun r-ts-mode--bracket-offset (_node parent _bol)
  "One step when hanging, two for parameters whose closer ends a line."
  (cond
   ((not (r-ts-mode--hanging-p parent)) 0)
   ((and (equal (treesit-node-type parent) "parameters")
         (let ((close (treesit-node-child-by-field-name parent "close")))
           (and close
                (/= (r-ts-mode--bol (treesit-node-start close))
                    (treesit-node-start close)))))
    (* 2 r-ts-mode-indent-offset))
   (t r-ts-mode-indent-offset)))

(defun r-ts-mode--brace-anchor (_node parent _bol)
  "Start of the line that opens the construct PARENT's braces belong to."
  (let ((owner (treesit-node-parent parent)))
    (r-ts-mode--bol
     (treesit-node-start
      (if (member (treesit-node-type owner)
                  '("function_definition" "if_statement" "for_statement"
                    "while_statement" "repeat_statement"))
          owner
        parent)))))

(defun r-ts-mode--chain-top (node)
  "Outermost binary operator in the chain NODE belongs to."
  (while (equal (treesit-node-type (treesit-node-parent node)) "binary_operator")
    (setq node (treesit-node-parent node)))
  node)

(defun r-ts-mode--chain-in-parens-p (chain)
  "Non-nil when CHAIN is the whole content of a parenthesised condition."
  (let ((holder (treesit-node-parent chain)))
    (and holder
         (member (treesit-node-type holder)
                 '("parenthesized_expression" "if_statement" "while_statement"))
         (member (treesit-node-field-name chain) '("body" "condition"))
         (not (r-ts-mode--hanging-p holder)))))

(defun r-ts-mode--chain-anchor (_node parent _bol)
  "Anchor continuation lines of an operator chain."
  (let ((top (r-ts-mode--chain-top parent)))
    (if (r-ts-mode--chain-in-parens-p top)
        (treesit-node-start top)
      (r-ts-mode--bol (treesit-node-start top)))))

(defun r-ts-mode--chain-offset (_node parent _bol)
  "One step for a continued chain, none when aligned inside parentheses."
  (if (r-ts-mode--chain-in-parens-p (r-ts-mode--chain-top parent))
      0
    r-ts-mode-indent-offset))

(defconst r-ts-mode--continued-line-regexp
  (rx (or "|>" (seq "%" (* (not (any "%\n"))) "%")
          "<-" "<<-" "=" "+" "-" "*" "/" "^" "~" "&" "|" ",")
      (* blank) (? "#" (* nonl)) eol)
  "A line ending in this leaves its expression open.")

(defun r-ts-mode--code-line-before (pos)
  "Beginning of the nearest non-blank line above POS, or nil."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p (rx (* blank) eol)))
      (forward-line -1))
    (unless (looking-at-p (rx (* blank) eol)) (point))))

(defun r-ts-mode--line-matches-p (line regexp)
  "Non-nil when the line starting at LINE ends with REGEXP."
  (and line
       (save-excursion
         (goto-char line)
         (re-search-forward regexp (line-end-position) t))))

(defun r-ts-mode--incomplete-indent (bol)
  "Anchor and offset for BOL where the parse tree cannot be trusted."
  (let* ((line (r-ts-mode--code-line-before bol))
         (above (and line (r-ts-mode--code-line-before line)))
         (open (nth 1 (syntax-ppss bol))))
    (cond
     ((and (r-ts-mode--line-matches-p line r-ts-mode--continued-line-regexp)
           (not (r-ts-mode--line-matches-p line (rx "," (* blank) eol))))
      (cons (r-ts-mode--bol line)
            (if (r-ts-mode--line-matches-p above r-ts-mode--continued-line-regexp)
                0
              r-ts-mode-indent-offset)))
     ((and open
           (save-excursion
             (goto-char (1+ open))
             (skip-chars-forward " \t")
             (not (or (eolp) (looking-at-p "#")))))
      (cons (save-excursion (goto-char (1+ open)) (skip-chars-forward " \t") (point))
            0))
     (open (cons (r-ts-mode--bol open) r-ts-mode-indent-offset))
     (line (cons (r-ts-mode--bol line) 0))
     (t (cons (point-min) 0)))))

(defun r-ts-mode--incomplete-anchor (_node _parent bol)
  "Anchor for BOL inside code the parser could not make sense of."
  (car (r-ts-mode--incomplete-indent bol)))

(defun r-ts-mode--incomplete-offset (_node _parent bol)
  "Offset for BOL inside code the parser could not make sense of."
  (cdr (r-ts-mode--incomplete-indent bol)))

(defun r-ts-mode--incomplete-p (_node parent bol)
  "Non-nil when the tree around BOL cannot say how it nests."
  (or (null parent)
      (equal (treesit-node-type parent) "ERROR")
      (and (treesit-node-check parent 'has-error)
           (treesit-parent-until parent (lambda (n) (equal (treesit-node-type n) "ERROR"))))
      (and (equal (treesit-node-type parent) "program")
           (nth 1 (syntax-ppss bol)))))

(defvar r-ts-mode--indent-rules
  `((r
     ((parent-is "\\`string") no-indent 0)
     (r-ts-mode--incomplete-p r-ts-mode--incomplete-anchor r-ts-mode--incomplete-offset)
     ((parent-is "\\`program\\'") column-0 0)
     ((node-is "\\`}\\'") r-ts-mode--brace-anchor 0)
     ((node-is "\\`\\(?:)\\|]\\|]]\\)\\'") parent-bol 0)
     ((parent-is "\\`braced_expression\\'") r-ts-mode--brace-anchor r-ts-mode-indent-offset)
     ((parent-is "\\`\\(?:arguments\\|parameters\\|parenthesized_expression\\)\\'")
      r-ts-mode--bracket-anchor r-ts-mode--bracket-offset)
     ((parent-is "\\`binary_operator\\'") r-ts-mode--chain-anchor r-ts-mode--chain-offset)
     ((node-is "\\`else\\'") parent-bol 0)
     ((node-is "\\`braced_expression\\'") parent-bol 0)
     (no-node r-ts-mode--incomplete-anchor r-ts-mode--incomplete-offset)
     (catch-all parent-bol r-ts-mode-indent-offset)))
  "Tree-sitter indent rules for R buffers.")

(define-derived-mode r-ts-mode prog-mode "R"
  "Major mode for R, on the r-lib tree-sitter grammar."
  :syntax-table r-ts-mode--syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local indent-tabs-mode nil)
  (setq-local electric-indent-chars (append "{}()[]" electric-indent-chars))
  (when (treesit-ready-p 'r)
    (setq treesit-primary-parser (treesit-parser-create 'r))
    (setq-local treesit-font-lock-settings r-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string)
                  (constant number)
                  (function-call escape)))
    (setq-local treesit-simple-indent-rules r-ts-mode--indent-rules)
    (setq-local treesit-thing-settings
                `((r
                   (defun r-ts-mode--defun-p)
                   (sexp (not (or (and named ,(rx bos (or "program" "comment") eos))
                                  (and anonymous ,(rx bos (or "(" ")" "[" "]" "[[" "]]"
                                                              "{" "}" ",")
                                                      eos)))))
                   (list ,(rx bos (or "arguments" "parameters" "braced_expression"
                                      "parenthesized_expression")
                              eos))
                   (sentence r-ts-mode--statement-p)
                   (text ,(rx bos (or "comment" "string") eos))
                   (comment ,(rx bos "comment" eos)))))
    (setq-local treesit-defun-name-function #'r-ts-mode--defun-name)
    (setq-local treesit-simple-imenu-settings
                '(("Function" "\\`binary_operator\\'"
                   r-ts-mode--function-assignment-p nil)))
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . r-ts-mode))
(add-to-list 'auto-mode-alist '("\\.Rprofile\\'" . r-ts-mode))
(add-to-list 'interpreter-mode-alist '("Rscript" . r-ts-mode))

;; Markdown fences, org-src and polymode find a mode by the name R-mode or r-mode.
(unless (fboundp 'R-mode) (defalias 'R-mode #'r-ts-mode))
(unless (fboundp 'r-mode) (defalias 'r-mode #'r-ts-mode))
(add-to-list 'major-mode-remap-alist '(ess-r-mode . r-ts-mode))
(add-to-list 'major-mode-remap-alist '(R-mode . r-ts-mode))

(defvar apheleia-formatters)
(with-eval-after-load 'apheleia
  (setf (alist-get 'air apheleia-formatters) '("air" "format" "--stdin-file-path" filepath)))

(provide 'ygg-r-mode)
;;; ygg-r-mode.el ends here
