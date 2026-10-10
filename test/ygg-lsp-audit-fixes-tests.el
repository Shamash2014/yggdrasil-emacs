;;; ygg-lsp-audit-fixes-tests.el --- HTML, CSS and harper under rass -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)

(defvar ygg-lsp-audit-fixes-tests--bare (make-temp-file "ygg-bare" t))

(let ((exec-path (list ygg-lsp-audit-fixes-tests--bare)))
  (require 'layer-lsp)
  (require 'layer-rass))

(defun ygg-lsp-audit-fixes-tests--bin-dir (&rest names)
  (let ((dir (make-temp-file "ygg-bin" t)))
    (dolist (name names)
      (let ((file (expand-file-name name dir)))
        (with-temp-file file (insert "#!/bin/sh\nexit 0\n"))
        (set-file-modes file #o755)))
    dir))

(defun ygg-lsp-audit-fixes-tests--ensures (mode &rest bins)
  "How many times entering MODE with only BINS on exec-path asks eglot to start."
  (let ((ensured 0)
        (exec-path (list (apply #'ygg-lsp-audit-fixes-tests--bin-dir bins)))
        (default-directory (make-temp-file "ygg-proj" t)))
    (cl-letf (((symbol-function 'eglot-ensure) (lambda () (cl-incf ensured)))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (funcall mode)))
    ensured))

(ert-deftest ygg-lsp-audit-html-starts-on-the-vscode-server-alone ()
  (dolist (mode '(html-mode mhtml-mode))
    (should (= 1 (ygg-lsp-audit-fixes-tests--ensures mode "vscode-html-language-server")))))

(ert-deftest ygg-lsp-audit-html-starts-on-emmet-alone ()
  (should (= 1 (ygg-lsp-audit-fixes-tests--ensures 'html-mode "emmet-language-server"))))

(ert-deftest ygg-lsp-audit-html-stays-off-with-nothing-runnable ()
  (should (= 0 (ygg-lsp-audit-fixes-tests--ensures 'html-mode))))

(ert-deftest ygg-lsp-audit-css-starts-on-the-vscode-server-alone ()
  (should (= 1 (ygg-lsp-audit-fixes-tests--ensures 'css-mode "vscode-css-language-server"))))

(ert-deftest ygg-lsp-audit-css-starts-on-emmet-alone-and-picks-it ()
  (should (= 1 (ygg-lsp-audit-fixes-tests--ensures 'css-mode "emmet-language-server")))
  (let ((exec-path (list (ygg-lsp-audit-fixes-tests--bin-dir "emmet-language-server"))))
    (should (equal (car (ygg-lsp-css-contact)) "emmet-language-server"))))

(ert-deftest ygg-lsp-audit-css-prefers-the-vscode-server-over-emmet ()
  (let ((exec-path (list (ygg-lsp-audit-fixes-tests--bin-dir
                          "emmet-language-server" "vscode-css-language-server"))))
    (should (equal (car (ygg-lsp-css-contact)) "vscode-css-language-server"))))

(defun ygg-lsp-audit-fixes-tests--configuration (command binary)
  (cl-letf (((symbol-function 'jsonrpc--process) (lambda (_) 'proc))
            ((symbol-function 'process-command) (lambda (_) command))
            ((symbol-function 'ygg-lsp--server-binary) (lambda (_) binary))
            ((symbol-function 'ygg-lsp-typescript-configuration)
             (lambda (_) (and (equal binary "tsserver") '(:typescript (:a 1)))))
            ((symbol-function 'ygg-rass-typescript-configuration) #'ignore))
    (ygg-lsp-workspace-configuration 'server)))

(ert-deftest ygg-lsp-audit-harper-settings-ride-beside-the-rass-primary ()
  (let ((config (ygg-lsp-audit-fixes-tests--configuration
                 '("rass" "--" "tsserver" "--stdio" "--" "harper-ls" "--stdio") "tsserver")))
    (should (equal (plist-get config :typescript) '(:a 1)))
    (should (hash-table-p (plist-get config :harper-ls)))))

(ert-deftest ygg-lsp-audit-harper-settings-reach-a-server-with-none-of-its-own ()
  (let ((config (ygg-lsp-audit-fixes-tests--configuration
                 '("rass" "--" "dart" "language-server" "--" "harper-ls" "--stdio") "dart")))
    (should (hash-table-p (plist-get config :harper-ls)))))

(ert-deftest ygg-lsp-audit-harper-direct-gets-one-section ()
  (let ((config (ygg-lsp-audit-fixes-tests--configuration '("harper-ls" "--stdio") "harper-ls")))
    (should (= (length config) 2))
    (should (hash-table-p (plist-get config :harper-ls)))))

(ert-deftest ygg-lsp-audit-server-without-harper-gets-no-harper-section ()
  (let ((config (ygg-lsp-audit-fixes-tests--configuration '("dart" "language-server") "dart")))
    (should-not (plist-member config :harper-ls))))

(ert-deftest ygg-lsp-audit-rass-contacts-quiet-the-traffic-log ()
  (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name)))
            ((symbol-function 'ygg-rass-eslint-p) (lambda (_) t)))
    (let ((default-directory "/tmp/"))
      (dolist (contact (list (ygg-rass-with-harper '("gopls"))
                             (ygg-rass-with-eslint '("typescript-language-server" "--stdio"))))
        (should (equal (seq-subseq contact 0 4) '("rass" "--no-stream-diagnostics" "--log-level" "warn")))))))

(provide 'ygg-lsp-audit-fixes-tests)
;;; ygg-lsp-audit-fixes-tests.el ends here
