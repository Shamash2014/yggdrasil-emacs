;;; ygg-combobulate-tests.el --- Combobulate activation is idempotent and reaches open buffers -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(load (expand-file-name "test-elpaca" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter/" "~/.emacs.d/"))
(require 'ert)
(require 'cl-lib)
(require 'layer-lsp)
(require 'combobulate nil t)

(defmacro ygg-combobulate-tests--with-buffer (suffix mode language content &rest body)
  (declare (indent 4))
  `(let ((file (make-temp-file "ygg-combobulate" nil ,suffix ,content)))
     (skip-unless (and (featurep 'combobulate) (treesit-language-available-p ',language)))
     (unwind-protect
         (let ((buffer (let ((after-change-major-mode-hook nil)
                             (auto-mode-alist (list (cons (regexp-quote ,suffix) ',mode))))
                         (find-file-noselect file))))
           (unwind-protect
               (with-current-buffer buffer ,@body)
             (kill-buffer buffer)))
       (delete-file file))))

(ert-deftest ygg-combobulate-double-hook-run-stays-on ()
  (ygg-combobulate-tests--with-buffer ".py" python-ts-mode python "def f(x):\n    return x\n"
    (ygg--maybe-combobulate)
    (should (ygg--combobulate-active-p))
    (ygg--maybe-combobulate)
    (should (ygg--combobulate-active-p))
    (should (bound-and-true-p combobulate-python-minor-mode))))

(ert-deftest ygg-combobulate-buffer-opened-before-load-gets-activated ()
  (ygg-combobulate-tests--with-buffer ".json" json-ts-mode json "{\"a\": 1}\n"
    (should-not (ygg--combobulate-active-p))
    (ygg--combobulate-enable-open-buffers)
    (should (bound-and-true-p combobulate-json-minor-mode))
    (ygg--combobulate-enable-open-buffers)
    (should (bound-and-true-p combobulate-json-minor-mode))))

(ert-deftest ygg-combobulate-text-derived-ts-mode-is-reached-by-major-mode-hook ()
  (ygg-combobulate-tests--with-buffer ".yaml" yaml-ts-mode yaml "a: 1\n"
    (should-not (ygg--combobulate-active-p))
    (let ((after-change-major-mode-hook (list #'ygg--maybe-combobulate)))
      (yaml-ts-mode))
    (should (bound-and-true-p combobulate-yaml-minor-mode))))

(ert-deftest ygg-combobulate-buffer-without-parser-is-left-alone ()
  (skip-unless (featurep 'combobulate))
  (with-temp-buffer
    (emacs-lisp-mode)
    (ygg--maybe-combobulate)
    (should-not (ygg--combobulate-active-p))))

;;; ygg-combobulate-tests.el ends here
