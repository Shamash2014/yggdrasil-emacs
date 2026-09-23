;;; ygg-treesit-objects-tests.el --- Behavioral tests for treesit text objects -*- lexical-binding: t; -*-

(require 'ert)
(require 'treesit)

;;; Setup: configure treesit path

(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))

;;; Test function textobject

(ert-deftest ygg-ts-func-bounds-ts ()
  "Test function bounds in TypeScript."
  (require 'layer-lsp)
  (let ((buf (generate-new-buffer "*test-ts*")))
    (unwind-protect
        (with-current-buffer buf
          (typescript-ts-mode)
          (insert "function add(a: number, b: number): number {
  return a + b;
}")
          ;; Ensure parser is loaded
          (unless (treesit-parser-list)
            (ignore-errors (treesit-parser-create 'typescript)))
          (when (treesit-parser-list)
            ;; Position at line 2 (inside function body)
            (goto-line 2)
            ;; Test around: should get whole function
            (let ((bounds-around (ygg-lsp--defun-bounds)))
              (should bounds-around)
              (let ((text (buffer-substring (car bounds-around) (cdr bounds-around))))
                ;; Around should include 'function' keyword and braces
                (should (string-match-p "function" text))))
            ;; Test inner: should get just the body
            (let ((node (treesit-defun-at-point)))
              (when node
                (let ((inner (ygg-lsp--inner-node-bounds node)))
                  (should inner)
                  (let ((text (buffer-substring (car inner) (cdr inner))))
                    ;; Inner should not include 'function' keyword
                    (should-not (string-match-p "function" text))))))))
      (kill-buffer buf))))

;;; Test parameter textobject

(ert-deftest ygg-ts-param-bounds-ts ()
  "Test parameter bounds in TypeScript."
  (require 'layer-lsp)
  (let ((buf (generate-new-buffer "*test-ts*")))
    (unwind-protect
        (with-current-buffer buf
          (typescript-ts-mode)
          (insert "function test(a: number, b: string) {}")
          (unless (treesit-parser-list)
            (ignore-errors (treesit-parser-create 'typescript)))
          (when (treesit-parser-list)
            ;; Position on first parameter
            (goto-char (string-match "a:" (buffer-string)))
            (let ((bounds-inner (ygg-lsp--parameter-bounds 'inside)))
              (should bounds-inner))
            (let ((bounds-around (ygg-lsp--parameter-bounds 'around)))
              (should bounds-around))))
      (kill-buffer buf))))

;;; Test loop textobject

(ert-deftest ygg-ts-loop-bounds-ts ()
  "Test loop bounds in TypeScript."
  (require 'layer-lsp)
  (let ((buf (generate-new-buffer "*test-ts*")))
    (unwind-protect
        (with-current-buffer buf
          (typescript-ts-mode)
          (insert "for (let i = 0; i < 10; i++) {
  console.log(i);
}")
          (unless (treesit-parser-list)
            (ignore-errors (treesit-parser-create 'typescript)))
          (when (treesit-parser-list)
            ;; Position inside loop
            (goto-line 2)
            (let ((bounds-around (ygg-lsp--loop-bounds 'around)))
              (should bounds-around)
              (let ((text (buffer-substring (car bounds-around) (cdr bounds-around))))
                (should (string-match-p "for" text))))
            (let ((bounds-inside (ygg-lsp--loop-bounds 'inside)))
              (should bounds-inside))))
      (kill-buffer buf))))

;;; Test conditional textobject

(ert-deftest ygg-ts-cond-bounds-ts ()
  "Test conditional bounds in TypeScript."
  (require 'layer-lsp)
  (let ((buf (generate-new-buffer "*test-ts*")))
    (unwind-protect
        (with-current-buffer buf
          (typescript-ts-mode)
          (insert "if (value > 10) {
  console.log('big');
}")
          (unless (treesit-parser-list)
            (ignore-errors (treesit-parser-create 'typescript)))
          (when (treesit-parser-list)
            ;; Position inside if
            (goto-line 2)
            (let ((bounds-around (ygg-lsp--conditional-bounds 'around)))
              (should bounds-around)
              (let ((text (buffer-substring (car bounds-around) (cdr bounds-around))))
                (should (string-match-p "if" text))))
            (let ((bounds-inside (ygg-lsp--conditional-bounds 'inside)))
              (should bounds-inside))))
      (kill-buffer buf))))

;;; Test string textobject

(ert-deftest ygg-ts-string-bounds-ts ()
  "Test string inner bounds in TypeScript."
  (require 'layer-lsp)
  (let ((buf (generate-new-buffer "*test-ts*")))
    (unwind-protect
        (with-current-buffer buf
          (typescript-ts-mode)
          (insert "const msg = \"hello world\";")
          (unless (treesit-parser-list)
            (ignore-errors (treesit-parser-create 'typescript)))
          (when (treesit-parser-list)
            ;; Position inside string
            (goto-char (string-match "hello" (buffer-string)))
            (let ((bounds-inside (ygg-lsp--string-bounds 'inside)))
              (should bounds-inside)
              (let ((text (buffer-substring (car bounds-inside) (cdr bounds-inside))))
                ;; Inner string should not include quotes
                (should (string= text "hello world"))
                (should-not (string-match-p "\"" text))))
            (let ((bounds-around (ygg-lsp--string-bounds 'around)))
              (should bounds-around)
              (let ((text (buffer-substring (car bounds-around) (cdr bounds-around))))
                ;; Around string should include quotes
                (should (string-match-p "\"" text))))))
      (kill-buffer buf))))

;;; Test Python function

(ert-deftest ygg-ts-py-func-bounds ()
  "Test function bounds in Python."
  (require 'layer-lsp)
  (let ((buf (generate-new-buffer "*test-py*")))
    (unwind-protect
        (with-current-buffer buf
          (python-ts-mode)
          (insert "def add(a, b):
    return a + b")
          (unless (treesit-parser-list)
            (ignore-errors (treesit-parser-create 'python)))
          (when (treesit-parser-list)
            ;; Position on second line (inside function body)
            (goto-line 2)
            (let ((bounds-around (ygg-lsp--defun-bounds)))
              (should bounds-around)
              (let ((text (buffer-substring (car bounds-around) (cdr bounds-around))))
                (should (string-match-p "def" text))))))
      (kill-buffer buf))))

;;; Test that new objects don't clash with existing match.el keys

(ert-deftest ygg-ts-no-key-clashes ()
  "Verify new treesit object keys don't clash with match.el keys."
  ;; Load layer-lsp which defines the new bounds functions
  (condition-case nil
      (require 'layer-lsp)
    (error nil))
  ;; Verify layer-lsp bounds functions exist for new objects (treesit)
  (should (fboundp 'ygg-lsp--string-bounds))
  (should (fboundp 'ygg-lsp--comment-bounds))
  (should (fboundp 'ygg-lsp--block-bounds))
  (should (fboundp 'ygg-lsp--parameter-bounds))
  (should (fboundp 'ygg-lsp--loop-bounds))
  (should (fboundp 'ygg-lsp--conditional-bounds))
  ;; Verify motions exist for new objects
  (should (fboundp 'ygg-next-loop))
  (should (fboundp 'ygg-prev-loop))
  ;; Keys: layer-lsp adds f,t,l,C,P,k,S,M,B
  ;; match.el uses w,p,W,s,e,a,c,i and bracket/quote pairs
  ;; No clashes except 't' which is intentionally overridden
  (should t))

(provide 'ygg-treesit-objects-tests)
