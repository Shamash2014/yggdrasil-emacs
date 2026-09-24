;;; r-mode-tests.el --- Tests for r-ts-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'imenu)
(require 'ygg-r-mode)

(defconst r-mode-tests--canonical
  "library(dplyr)

#' Summarise sales by region
summarise_sales <- function(data, group_var, min_n = 10) {
  data |>
    filter(!is.na(amount)) |>
    # keep only the groups we report on
    group_by({{ group_var }}) |>
    summarise(
      total = sum(amount),
      n = n(),
      .groups = \"drop\"
    ) %>%
    filter(n >= min_n)
}

long_function_name <- function(
  a = \"a long argument\",
  b = \"another argument\"
) {
  if (a == b) {
    message(\"same\")
  } else if (is.null(a)) {
    stop(\"a is NULL\")
  } else {
    warning(\"different\")
  }
  x <- a +
    b
  invisible(x)
}

double_indented <- function(
    a = \"a long argument\",
    b = \"another argument\") {
  a
}

aligned <- function(data, group_var,
                    min_n = 10) {
  paste(data, group_var,
        sep = \"-\")
}

results <- lapply(items, function(x) {
  y <- x * 2
  y + 1
})

model <- lm(
  y ~ x1 + x2 +
    x3,
  data = df
)

plot <- ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  labs(title = \"Points\")

for (i in seq_len(10)) {
  if (i %% 2 == 0) {
    next
  }
  print(df[i, ])
}

safe <- tryCatch(
  {
    risky()
  },
  error = function(e) {
    NULL
  }
)

check <- function(x) {
  if (is.numeric(x) &&
      length(x) > 0) {
    TRUE
  } else if (
    is.character(x) &&
      nzchar(x)
  ) {
    NA
  } else {
    FALSE
  }
}

note <- \"a string
  that keeps its own
indentation\"
"
  "Tidyverse-styled R that indentation must leave exactly as it is.")

(defmacro r-mode-tests--with-buffer (text &rest body)
  "Run BODY in an R buffer holding TEXT, point at its start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (let ((treesit-font-lock-level 4))
       (r-ts-mode)
       (should (treesit-parser-list))
       (goto-char (point-min))
       ,@body)))

(defun r-mode-tests--face-at (needle)
  "Face font-lock puts on the first character of NEEDLE."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (match-beginning 0) 'face))

(ert-deftest r-mode-grammar-is-available ()
  (should (treesit-language-available-p 'r)))

(ert-deftest r-mode-is-chosen-for-r-files ()
  (dolist (name '("analysis.R" "script.r" ".Rprofile"))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name name temporary-file-directory))
      (set-auto-mode)
      (should (eq major-mode 'r-ts-mode)))))

(ert-deftest r-mode-is-chosen-for-rscript-scripts ()
  (with-temp-buffer
    (insert "#!/usr/bin/env Rscript\nprint(1)\n")
    (set-auto-mode)
    (should (eq major-mode 'r-ts-mode))))

(ert-deftest r-mode-ess-mode-is-remapped-to-r-ts-mode ()
  (should (eq (major-mode-remap 'ess-r-mode) 'r-ts-mode)))

(ert-deftest r-mode-old-mode-names-resolve-to-r-ts-mode ()
  (with-temp-buffer
    (funcall 'R-mode)
    (should (eq major-mode 'r-ts-mode)))
  (with-temp-buffer
    (funcall 'r-mode)
    (should (eq major-mode 'r-ts-mode))))

(ert-deftest r-mode-fontifies-the-core-faces ()
  (r-mode-tests--with-buffer
      "# a note\nsquare <- function(x) {\n  if (x > 1) return(x ^ 2)\n  \"done\"\n  TRUE\n  42\n}\n"
    (font-lock-ensure)
    (should (eq (r-mode-tests--face-at "# a note") 'font-lock-comment-face))
    (should (eq (r-mode-tests--face-at "square") 'font-lock-function-name-face))
    (should (eq (r-mode-tests--face-at "function") 'font-lock-keyword-face))
    (should (eq (r-mode-tests--face-at "if") 'font-lock-keyword-face))
    (should (eq (r-mode-tests--face-at "return") 'font-lock-keyword-face))
    (should (eq (r-mode-tests--face-at "\"done\"") 'font-lock-string-face))
    (should (eq (r-mode-tests--face-at "TRUE") 'font-lock-constant-face))
    (should (eq (r-mode-tests--face-at "42") 'font-lock-number-face))
    (should-not (r-mode-tests--face-at "x >"))))

(ert-deftest r-mode-leaves-canonical-indentation-unchanged ()
  (r-mode-tests--with-buffer r-mode-tests--canonical
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) r-mode-tests--canonical))))

(ert-deftest r-mode-reindents-flattened-code-to-canonical ()
  (let ((string-start (string-search "\n  that keeps" r-mode-tests--canonical)))
    (r-mode-tests--with-buffer
        (concat (replace-regexp-in-string
                 "^[ \t]+" "" (substring r-mode-tests--canonical 0 string-start))
                (substring r-mode-tests--canonical string-start))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) r-mode-tests--canonical)))))

(ert-deftest r-mode-indents-lines-typed-into-unfinished-code ()
  (dolist (case '(("f <- function(x) {\n  data |>\n@\n}\n" . 4)
                  ("f <- function(x) {\n  data |>\n    filter(x) |>\n@\n}\n" . 4)
                  ("data |>\n@" . 2)
                  ("x <- foo(\n@\n)\n" . 2)
                  ("x <- foo(a,\n@\n" . 9)
                  ("f <- function(x) {\n  if (x) {\n@\n}\n" . 4)))
    (r-mode-tests--with-buffer (car case)
      (search-forward "@")
      (delete-char -1)
      (indent-according-to-mode)
      (should (equal (cons (car case) (current-column)) case)))))

(ert-deftest r-mode-imenu-lists-named-functions-only ()
  (r-mode-tests--with-buffer r-mode-tests--canonical
    (let ((names (mapcar #'car (cdr (assoc "Function" (funcall imenu-create-index-function))))))
      (should (member "summarise_sales" names))
      (should (member "long_function_name" names))
      (should (member "check" names))
      (should-not (member "results" names))
      (should-not (member "model" names)))))

(ert-deftest r-mode-moves-between-function-definitions ()
  (r-mode-tests--with-buffer r-mode-tests--canonical
    (search-forward "filter(n >= min_n)")
    (beginning-of-defun)
    (should (looking-at-p "summarise_sales <- function"))
    (end-of-defun)
    (beginning-of-defun -1)
    (should (looking-at-p "long_function_name <- function"))
    (should (equal (treesit-defun-name (treesit-defun-at-point))
                   "long_function_name"))))

(ert-deftest r-mode-defun-at-point-inside-a-lambda-is-the-lambda ()
  (r-mode-tests--with-buffer r-mode-tests--canonical
    (search-forward "y <- x * 2")
    (should (equal (treesit-node-type (treesit-defun-at-point)) "function_definition"))))

(ert-deftest r-mode-defines-the-treesit-things ()
  (r-mode-tests--with-buffer r-mode-tests--canonical
    (dolist (thing '(defun sexp list sentence text comment))
      (should (treesit-thing-defined-p thing 'r)))
    (search-forward "total = sum")
    (should (equal (treesit-node-type (treesit-thing-at (point) 'list))
                   "arguments"))))

(ert-deftest r-mode-comments-with-a-hash-and-space ()
  (r-mode-tests--with-buffer "x <- 1\n"
    (comment-region (point-min) (point-max))
    (should (equal (buffer-string) "# x <- 1\n"))))

;;; r-mode-tests.el ends here
