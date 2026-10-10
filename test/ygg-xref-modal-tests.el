;;; ygg-xref-modal-tests.el --- Xref result buffers are fully modal -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(setq user-emacs-directory (file-name-as-directory (make-temp-file "ygg-xref" t)))
(require 'yggdrasil)
(require 'layer-quickfix)
(require 'xref)

(defmacro ygg-xref-tests--in-buffer (&rest body)
  "Run BODY in a real xref results buffer over a temporary file."
  (declare (indent 0))
  `(let* ((file (make-temp-file "ygg-xref" nil ".txt" "alpha\nbeta\n"))
          (xref-auto-jump-to-first-xref nil)
          (xref-show-xrefs-function #'xref--show-xref-buffer)
          (buf (xref--show-xref-buffer
                (lambda ()
                  (list (xref-make "alpha" (xref-make-file-location file 1 0))
                        (xref-make "beta" (xref-make-file-location file 2 0))))
                '((window . nil)))))
     (unwind-protect
         (with-current-buffer buf (ygg--maybe-activate) ,@body)
       (kill-buffer buf)
       (delete-file file))))

(defun ygg-xref-tests--key (key)
  (let ((map (cdr (assq 'ygg--normal-p ygg--mode-keys-alist))))
    (or (and map (lookup-key map (kbd key)))
        (lookup-key ygg-normal-map (kbd key)))))

(ert-deftest ygg-xref-buffer-gets-the-modal-layer ()
  (ygg-xref-tests--in-buffer
    (should (derived-mode-p 'xref--xref-buffer-mode))
    (should yggdrasil-local-mode)
    (should (eq ygg--state 'normal))))

(ert-deftest ygg-xref-j-and-k-step-and-show-the-location ()
  (ygg-xref-tests--in-buffer
    (should (eq (ygg-xref-tests--key "j") 'xref-next-line))
    (should (eq (ygg-xref-tests--key "k") 'xref-prev-line))))

(ert-deftest ygg-xref-keeps-its-navigation-verbs ()
  (ygg-xref-tests--in-buffer
    (should (eq (ygg-xref-tests--key "RET") 'xref-goto-xref))
    (should (eq (ygg-xref-tests--key "<tab>") 'xref-quit-and-goto-xref))
    (should (eq (ygg-xref-tests--key "] f") 'xref-next-group))
    (should (eq (ygg-xref-tests--key "[ f") 'xref-prev-group))))

(ert-deftest ygg-xref-leaves-the-modal-keys-alone ()
  (ygg-xref-tests--in-buffer
    (dolist (pair '(("n" . ygg-search-next) ("N" . ygg-search-prev)
                    ("p" . ygg-paste-after) ("r" . ygg-replace-char)))
      (should (eq (ygg-xref-tests--key (car pair)) (cdr pair))))))

(ert-deftest ygg-xref-localleader-reaches-replace-and-edit ()
  (ygg-xref-tests--in-buffer
    (let ((map (ygg-localleader--resolve)))
      (should (eq (lookup-key map (kbd "r")) 'xref-query-replace-in-results))
      (should (eq (lookup-key map (kbd "e")) 'xref-change-to-xref-edit-mode)))))

(ert-deftest ygg-xref-edit-mode-finishes-with-zz ()
  (should (eq (lookup-key xref-edit-mode-map [remap ygg-save-and-kill-buffer])
              'xref-edit-save-changes)))

(ert-deftest ygg-xref-edit-mode-drops-the-xref-keys ()
  (ygg-xref-tests--in-buffer
    (xref-change-to-xref-edit-mode)
    (should (derived-mode-p 'xref-edit-mode))
    (should-not (memq (ygg-xref-tests--key "j") '(xref-next-line xref-prev-line)))
    (should-not (memq (ygg-xref-tests--key "k") '(xref-next-line xref-prev-line)))
    (should-not (eq (ygg-xref-tests--key "<tab>") 'xref-quit-and-goto-xref))
    (should (eq (command-remapping 'ygg-save-and-kill-buffer) 'xref-edit-save-changes))))

(ert-deftest ygg-xref-edit-mode-takes-insert-and-text ()
  (ygg-xref-tests--in-buffer
    (xref-change-to-xref-edit-mode)
    (should (eq ygg--state 'normal))
    (ygg-insert-state)
    (should (eq ygg--state 'insert))
    (goto-char (point-max))
    (forward-line -1)
    (end-of-line)
    (insert "x")
    (should (string-suffix-p "x" (buffer-substring (pos-bol) (pos-eol))))))

(ert-deftest ygg-xref-saving-edits-restores-the-xref-keys ()
  (ygg-xref-tests--in-buffer
    (xref-change-to-xref-edit-mode)
    (xref-edit-save-changes)
    (should (derived-mode-p 'xref--xref-buffer-mode))
    (should (eq ygg--state 'normal))
    (should (eq (ygg-xref-tests--key "j") 'xref-next-line))
    (should (eq (ygg-xref-tests--key "<tab>") 'xref-quit-and-goto-xref))))
