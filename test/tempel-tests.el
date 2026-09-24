;;; tempel-tests.el --- Tests for snippets through tempel and corfu -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'cape)
(require 'tempel)
(require 'tempel-collection)
(require 'python)
(require 'rust-ts-mode)
(require 'typescript-ts-mode)
(require 'layer-completion)
(require 'layer-lsp)

(defconst tempel-test--path
  (expand-file-name "../templates" (file-name-directory (or load-file-name buffer-file-name))))

(defun tempel-test--keys (mode)
  (let ((tempel-path tempel-test--path))
    (with-temp-buffer
      (setq-local major-mode mode)
      (mapcar #'car (tempel--templates)))))

(ert-deftest tempel-templates-load-for-each-language ()
  (should (memq 'defun (tempel-test--keys 'emacs-lisp-mode)))
  (should (memq 'class (tempel-test--keys 'python-ts-mode)))
  (should (memq 'assertEqual (tempel-test--keys 'python-ts-mode)))
  (should (memq 'fnr (tempel-test--keys 'rust-ts-mode)))
  (should (memq 'fof (tempel-test--keys 'tsx-ts-mode)))
  (should (memq 'fof (tempel-test--keys 'js-ts-mode))))

(defun tempel-test--eglot-capf ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (list (car bounds) (cdr bounds) '("classify" "class")
          :exclusive t
          :display-sort-function #'identity)))

(ert-deftest tempel-follows-eglot-in-an-eglot-managed-buffer ()
  (let ((tempel-path tempel-test--path))
    (with-temp-buffer
      (setq-local major-mode 'python-ts-mode)
      (setq-local eglot--managed-mode t)
      (setq-local completion-at-point-functions
                  (list (cape-capf-super #'tempel-test--eglot-capf #'cape-file) #'cape-dabbrev))
      (insert "cla")
      (ygg-tempel--merge-capf)
      (ygg-tempel--merge-capf)
      (should (= 2 (length completion-at-point-functions)))
      (pcase-let* ((`(,beg ,end ,table . ,plist) (funcall (car completion-at-point-functions)))
                   (cands (all-completions (buffer-substring beg end) table))
                   (kind (plist-get plist :company-kind)))
        (should (equal (mapcar #'substring-no-properties (seq-take cands 2)) '("classify" "class")))
        (should (member "class" (nthcdr 2 cands)))
        (should (seq-every-p (lambda (c) (eq 'snippet (funcall kind c))) (nthcdr 2 cands)))
        (should (eq 'identity (alist-get 'display-sort-function
                                         (cdr (completion-metadata "" table nil)))))))))

(ert-deftest tempel-merges-into-a-plain-mode-capf ()
  (let ((tempel-path tempel-test--path))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defu")
      (ygg-tempel--merge-capf)
      (pcase-let ((`(,beg ,end ,table . ,_) (funcall (car completion-at-point-functions))))
        (should (= 2 (cl-count "defun" (all-completions (buffer-substring beg end) table)
                               :test #'equal)))))))

(defun tempel-test--eglot-toggle (on)
  (setq-local eglot--managed-mode on)
  (if on
      (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t))
  (ygg-lsp--wire-cape)
  (ygg-tempel--merge-capf))

(ert-deftest tempel-eglot-shutdown-hands-completion-back-to-the-mode ()
  (let ((tempel-path tempel-test--path))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defu")
      (ygg-tempel--merge-capf)
      (tempel-test--eglot-toggle t)
      (tempel-test--eglot-toggle nil)
      (cl-letf (((symbol-function 'eglot-completion-at-point)
                 (lambda () (error "No current JSON-RPC connection"))))
        (pcase-let ((`(,beg ,end ,table . ,_) (funcall (car completion-at-point-functions))))
          (should (= 2 (cl-count "defun" (all-completions (buffer-substring beg end) table)
                                 :test #'equal)))))
      (cl-letf (((symbol-function 'eglot-completion-at-point)
                 (lambda () (list (- (point) 4) (point) '("defuzz"))))
                ((symbol-function 'eglot-current-server) (lambda () t))
                ((symbol-function 'jsonrpc-running-p) (lambda (_) t)))
        (tempel-test--eglot-toggle t)
        (pcase-let ((`(,beg ,end ,table . ,_) (funcall (car completion-at-point-functions))))
          (should (equal "defuzz" (car (all-completions (buffer-substring beg end) table)))))))))

(ert-deftest tempel-leaves-prose-capfs-alone ()
  (with-temp-buffer
    (text-mode)
    (setq-local completion-at-point-functions (list #'tempel-test--eglot-capf t))
    (ygg-tempel--merge-capf)
    (should (eq #'tempel-test--eglot-capf (car completion-at-point-functions)))))

(ert-deftest tempel-template-ends-when-insert-state-ends ()
  (let ((tempel-path tempel-test--path))
    (with-temp-buffer
      (emacs-lisp-mode)
      (tempel-insert 'defun)
      (should tempel--active)
      (let ((this-command 'ygg-insert-one-command))
        (ygg-tempel--finish-on-exit))
      (should tempel--active)
      (ygg-tempel--finish-on-exit)
      (should-not tempel--active)
      (should (string-prefix-p "(defun" (buffer-string))))))

(ert-deftest tempel-tab-in-a-field-jumps-even-with-the-popup-open ()
  (let ((tempel-path tempel-test--path) completed)
    (with-temp-buffer
      (emacs-lisp-mode)
      (tempel-insert 'defun)
      (insert "ygg-no")
      (cl-letf (((symbol-function 'corfu-quit) #'ignore)
                ((symbol-function 'corfu-complete) (lambda () (setq completed t))))
        (let ((from (point)))
          (ygg-corfu-complete-or-next-field)
          (should (> (point) from))))
      (should-not completed)
      (should (string-prefix-p "(defun ygg-no (" (buffer-string))))))

(ert-deftest tempel-escape-is-left-to-the-modal-layer ()
  (should-not (lookup-key tempel-map (kbd "<escape>")))
  (should-not (lookup-key ygg-insert-map (kbd "TAB"))))

;;; tempel-tests.el ends here
