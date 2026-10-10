;;; ygg-rass-pairings-tests.el --- ruff, biome and tailwind beside the primary server -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'layer-rass)
(require 'yggdrasil)
(require 'layer-lsp)
(require 'layer-format)

(defun rass-pairings--fake-bins (dir names)
  (dolist (name names)
    (let ((file (expand-file-name name dir)))
      (with-temp-file file (insert "#!/bin/sh\n"))
      (set-file-modes file #o755))))

(defmacro rass-pairings--in (bins files &rest body)
  "Run BODY in a temp project holding FILES, with fake BINS first on `exec-path'."
  (declare (indent 2))
  `(let* ((bin (file-name-as-directory (make-temp-file "rass-bin" t)))
          (root (file-name-as-directory (make-temp-file "rass-proj" t)))
          (exec-path (list bin))
          (default-directory root))
     (unwind-protect
         (progn
           (rass-pairings--fake-bins bin ,bins)
           (dolist (file ,files)
             (make-directory (file-name-directory (expand-file-name (car file) root)) t)
             (with-temp-file (expand-file-name (car file) root) (insert (cdr file))))
           ,@body)
       (delete-directory bin t)
       (delete-directory root t))))

(defconst rass-pairings--base '("rass" "harper-ls"))

(defun rass-pairings--segments (contact)
  (let (segments current)
    (dolist (arg (cdr (member "--" contact)))
      (if (equal arg "--")
          (progn (push (nreverse current) segments) (setq current nil))
        (push arg current)))
    (nreverse (cons (nreverse current) segments))))

(ert-deftest rass-pairings-python-gets-ruff-then-harper-once ()
  (rass-pairings--in (append rass-pairings--base '("ruff")) nil
    (let ((segments (rass-pairings--segments
                     (ygg-rass-with-ruff '("basedpyright-langserver" "--stdio")))))
      (should (equal segments '(("basedpyright-langserver" "--stdio")
                                ("ruff" "server")
                                ("harper-ls" "--stdio")))))
    (should (equal (ygg-rass-with-harper (ygg-rass-with-ruff '("x")))
                   (ygg-rass-with-ruff '("x"))))))

(ert-deftest rass-pairings-python-without-ruff-is-unchanged ()
  (rass-pairings--in rass-pairings--base nil
    (should (equal (ygg-rass-with-ruff '("basedpyright-langserver" "--stdio"))
                   '("basedpyright-langserver" "--stdio")))))

(ert-deftest rass-pairings-python-entry-wraps-and-keeps-options-last ()
  (rass-pairings--in (append rass-pairings--base '("ruff")) nil
    (let ((contact (ygg-rass-with-ruff '("pyright-langserver" "--stdio" :initializationOptions (:a 1)))))
      (should (equal (last contact 2) '(:initializationOptions (:a 1))))
      (should (= 1 (cl-count "harper-ls" contact :test #'equal))))))

(ert-deftest rass-pairings-biome-joins-typescript ()
  (rass-pairings--in (append rass-pairings--base '("biome")) '(("biome.jsonc" . "{}") ("src/a.ts" . ""))
    (let ((default-directory (expand-file-name "src/" root)))
      (should (equal (rass-pairings--segments (ygg-rass-with-eslint '("vtsls" "--stdio")))
                     '(("vtsls" "--stdio") ("biome" "lsp-proxy") ("harper-ls" "--stdio")))))))

(ert-deftest rass-pairings-biome-needs-the-binary ()
  (rass-pairings--in rass-pairings--base '(("biome.json" . "{}"))
    (should (equal (ygg-rass-with-eslint '("vtsls" "--stdio")) '("vtsls" "--stdio")))))

(ert-deftest rass-pairings-tailwind-by-config-or-dependency ()
  (dolist (files '((("tailwind.config.js" . ""))
                   (("package.json" . "{\"devDependencies\": {\"tailwindcss\": \"4\"}}"))
                   (("package.json" . "{\"dependencies\": {\"nativewind\": \"4\"}}"))))
    (rass-pairings--in (append rass-pairings--base '("tailwindcss-language-server")) files
      (should (equal (rass-pairings--segments (ygg-rass-with-eslint '("vtsls" "--stdio")))
                     '(("vtsls" "--stdio") ("tailwindcss-language-server" "--stdio")
                       ("harper-ls" "--stdio")))))))

(ert-deftest rass-pairings-tailwind-joins-html-and-css ()
  (rass-pairings--in (append rass-pairings--base '("tailwindcss-language-server"))
      '(("tailwind.config.ts" . ""))
    (should (equal (rass-pairings--segments
                    (ygg-rass-with-harper '("vscode-css-language-server" "--stdio")))
                   '(("vscode-css-language-server" "--stdio")
                     ("tailwindcss-language-server" "--stdio")
                     ("harper-ls" "--stdio"))))))

(ert-deftest rass-pairings-everything-once-harper-last ()
  (rass-pairings--in (append rass-pairings--base
                             '("vscode-eslint-language-server" "biome" "tailwindcss-language-server"))
      '(("eslint.config.js" . "") ("node_modules/.bin/eslint" . "")
        ("biome.json" . "{}") ("tailwind.config.js" . ""))
    (should (equal (rass-pairings--segments
                    (ygg-rass-with-eslint '("vtsls" "--stdio" :initializationOptions (:a 1))))
                   '(("vtsls" "--stdio")
                     ("vscode-eslint-language-server" "--stdio")
                     ("biome" "lsp-proxy")
                     ("tailwindcss-language-server" "--stdio")
                     ("harper-ls" "--stdio" :initializationOptions (:a 1)))))))

(ert-deftest rass-pairings-no-configs-leave-typescript-and-css-alone ()
  (rass-pairings--in (append rass-pairings--base
                             '("vscode-eslint-language-server" "biome" "tailwindcss-language-server"))
      nil
    (should (equal (ygg-rass-with-eslint '("vtsls" "--stdio")) '("vtsls" "--stdio")))
    (should (equal (rass-pairings--segments (ygg-rass-with-harper '("vscode-css-language-server" "--stdio")))
                   '(("vscode-css-language-server" "--stdio") ("harper-ls" "--stdio"))))))

(ert-deftest rass-pairings-ruff-and-biome-prefer-the-project-binary ()
  (rass-pairings--in (append rass-pairings--base '("ruff" "biome"))
      '((".venv/bin/ruff" . "") ("node_modules/.bin/biome" . "") ("biome.json" . "{}"))
    (set-file-modes (expand-file-name ".venv/bin/ruff" root) #o755)
    (set-file-modes (expand-file-name "node_modules/.bin/biome" root) #o755)
    (let ((ruff (rass-pairings--segments (ygg-rass-with-ruff '("pyright-langserver" "--stdio"))))
          (biome (rass-pairings--segments (ygg-rass-with-eslint '("vtsls" "--stdio")))))
      (should (equal (nth 1 ruff) (list (expand-file-name ".venv/bin/ruff" root) "server")))
      (should (equal (nth 1 biome) (list (expand-file-name "node_modules/.bin/biome" root) "lsp-proxy"))))))

(ert-deftest rass-pairings-preset-follows-eslint-presence ()
  (rass-pairings--in (append rass-pairings--base '("biome" "vscode-eslint-language-server"))
      '(("biome.json" . "{}"))
    (should (equal (nth 4 (ygg-rass-with-eslint '("vtsls" "--stdio"))) ygg-rass-harper-preset)))
  (rass-pairings--in (append rass-pairings--base '("vscode-eslint-language-server"))
      '(("eslint.config.js" . "") ("node_modules/.bin/eslint" . ""))
    (should (equal (nth 4 (ygg-rass-with-eslint '("vtsls" "--stdio"))) ygg-rass-typescript-preset))))

(ert-deftest rass-pairings-autoport-is-never-wrapped ()
  (rass-pairings--in (append rass-pairings--base '("ruff")) nil
    (let ((contact '("x" :autoport t)))
      (should (equal (ygg-rass-with-ruff contact) contact))
      (should (equal (ygg-rass-with-harper contact) contact)))))

(ert-deftest rass-pairings-ruff-linter-skipped-when-server-runs-ruff ()
  (dolist (case '((("rass" "p.py" "--" "pyright" "--" "/p/.venv/bin/ruff" "server") . nil)
                  (("pyright-langserver" "--stdio") . t)))
    (with-temp-buffer
      (python-mode)
      (setq-local flymake-diagnostic-functions nil)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglot-current-server) (lambda () 'server))
                ((symbol-function 'jsonrpc--process) (lambda (_) 'proc))
                ((symbol-function 'process-command) (lambda (_) (car case)))
                ((symbol-function 'flymake-start) #'ignore))
        (ygg-format--rejoin-linters)
        (should (eq (and (memq 'flymake-collection-ruff flymake-diagnostic-functions) t)
                    (cdr case)))))))

;;; ygg-rass-pairings-tests.el ends here
