;;; ygg-lsp-lazy-hooks-tests.el --- eglot hooks wait for the server binary -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)

(defvar ygg-lsp-lazy-hooks-tests--bare (make-temp-file "ygg-bare" t))

(let ((exec-path (list ygg-lsp-lazy-hooks-tests--bare)))
  (require 'layer-lsp))

(defun ygg-lsp-lazy-hooks-tests--bin-dir (&rest names)
  (let ((dir (make-temp-file "ygg-bin" t)))
    (dolist (name names)
      (let ((file (expand-file-name name dir)))
        (with-temp-file file (insert "#!/bin/sh\nexit 0\n"))
        (set-file-modes file #o755)))
    dir))

(defun ygg-lsp-lazy-hooks-tests--fire (hook path)
  "Run HOOK in a fresh buffer with exec-path PATH; the messages and ensure count."
  (let ((ensured 0) (messages nil)
        (exec-path path)
        (ygg-lsp--warned nil)
        (ygg-lsp-tsgo-program "/nonexistent/tsc"))
    (cl-letf (((symbol-function 'eglot-ensure) (lambda () (cl-incf ensured)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (with-temp-buffer
        (run-hooks hook)
        (run-hooks hook)))
    (list ensured (nreverse messages))))

(ert-deftest ygg-lsp-lazy-hooks-registered-without-binary ()
  (dolist (hook '(dart-mode-hook dart-ts-mode-hook python-mode-hook python-ts-mode-hook
                  js-mode-hook typescript-ts-mode-hook))
    (should (seq-some (lambda (fn) (and (symbolp fn) (string-prefix-p "ygg-lsp--" (symbol-name fn))))
                      (default-value hook)))))

(ert-deftest ygg-lsp-lazy-hooks-dart-starts-once-binary-appears ()
  (let ((dir (ygg-lsp-lazy-hooks-tests--bin-dir "dart")))
    (unwind-protect
        (should (equal (ygg-lsp-lazy-hooks-tests--fire 'dart-mode-hook (list dir))
                       '(2 nil)))
      (delete-directory dir t))))

(ert-deftest ygg-lsp-lazy-hooks-dart-warns-once-without-binary ()
  (let ((result (ygg-lsp-lazy-hooks-tests--fire
                 'dart-ts-mode-hook (list ygg-lsp-lazy-hooks-tests--bare))))
    (should (eq (car result) 0))
    (should (equal (cadr result)
                   '("yggdrasil-lsp: Dart server (dart) not runnable")))))

(ert-deftest ygg-lsp-lazy-hooks-python-follows-exec-path ()
  (let ((dir (ygg-lsp-lazy-hooks-tests--bin-dir "basedpyright-langserver")))
    (unwind-protect
        (progn
          (should (equal (ygg-lsp-lazy-hooks-tests--fire 'python-ts-mode-hook (list dir))
                         '(2 nil)))
          (should (equal (car (ygg-lsp-lazy-hooks-tests--fire
                               'python-ts-mode-hook (list ygg-lsp-lazy-hooks-tests--bare)))
                         0)))
      (delete-directory dir t))))

(ert-deftest ygg-lsp-lazy-hooks-typescript-follows-exec-path ()
  (let ((dir (ygg-lsp-lazy-hooks-tests--bin-dir "vtsls")))
    (unwind-protect
        (progn
          (should (equal (ygg-lsp-lazy-hooks-tests--fire 'js-mode-hook (list dir))
                         '(2 nil)))
          (let ((result (ygg-lsp-lazy-hooks-tests--fire
                         'js-mode-hook (list ygg-lsp-lazy-hooks-tests--bare))))
            (should (eq (car result) 0))
            (should (= (length (cadr result)) 1))))
      (delete-directory dir t))))

(defun ygg-lsp-lazy-hooks-tests--contact (mode)
  (cdr (seq-find (lambda (entry)
                   (seq-some (lambda (m) (eq (if (consp m) (car m) m) mode))
                             (if (listp (car entry)) (car entry) (list (car entry)))))
                 eglot-server-programs)))

(ert-deftest ygg-lsp-lazy-hooks-programs-registered-without-binary ()
  (require 'eglot)
  (should (equal (ygg-lsp-lazy-hooks-tests--contact 'yaml-mode)
                 '("yaml-language-server" "--stdio")))
  (dolist (mode '(dart-mode python-mode js-mode))
    (let ((contact (ygg-lsp-lazy-hooks-tests--contact mode)))
      (should (symbolp contact))
      (should (string-prefix-p "ygg-lsp" (symbol-name contact))))))
