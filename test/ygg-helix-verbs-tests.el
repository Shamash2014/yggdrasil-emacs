;;; ygg-helix-verbs-tests.el --- Helix verb bindings -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'yggdrasil)

(defmacro ygg-helix-test-buffer (content &rest body)
  "Run BODY in a modal text buffer holding CONTENT, point at its start.
The buffer is shown in the selected window so keyboard macros drive it."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer "ygg-helix-test")))
     (unwind-protect
         (progn
           (switch-to-buffer buf)
           (text-mode)
           (buffer-enable-undo)
           (yggdrasil-local-mode 1)
           (insert ,content)
           (goto-char (point-min))
           (undo-boundary)
           ,@body)
       (kill-buffer buf))))

(defun ygg-helix-test-keys (keys)
  (execute-kbd-macro (kbd keys)))

;;; M-d: delete without yanking

(ert-deftest ygg-helix-delete-no-yank ()
  "V d should delete selection without yanking."
  (ygg-helix-test-buffer "hello world"
    (let ((orig-ring (copy-sequence kill-ring)))
      (ygg-set-selection 1 6)
      (ygg-helix-test-keys "V d")
      (should (equal (buffer-string) " world"))
      (should (equal kill-ring orig-ring)))))

(ert-deftest ygg-helix-delete-no-yank-preserves-ring ()
  "V d should not affect kill ring."
  (ygg-helix-test-buffer "one two three"
    (let ((orig-ring (copy-sequence kill-ring)))
      (ygg-set-selection 1 4)
      (ygg-helix-test-keys "V d")
      (should (equal (buffer-string) " two three"))
      (should (equal kill-ring orig-ring)))))

;;; V c: change without yanking

(ert-deftest ygg-helix-change-no-yank ()
  "V c should change selection without yanking and enter insert mode."
  (ygg-helix-test-buffer "hello world"
    (let ((orig-ring (copy-sequence kill-ring)))
      (ygg-set-selection 1 6)
      (ygg-helix-test-keys "V c")
      (should (eq ygg--state 'insert))
      (should (equal (buffer-string) " world"))
      (should (equal kill-ring orig-ring)))))


;;; [ SPC: add empty line above

(ert-deftest ygg-helix-add-newline-above ()
  "[ SPC should add empty line above cursor."
  (ygg-helix-test-buffer "hello\nworld"
    (ygg-set-selection 1 1)
    (call-interactively #'ygg-add-newline-above)
    (should (string-prefix-p "\n" (buffer-string)))))

(ert-deftest ygg-helix-add-newline-above-with-count ()
  "[ SPC with count should add multiple lines."
  (ygg-helix-test-buffer "hello\nworld"
    (ygg-set-selection 7 7)
    (ygg-helix-test-keys "2 [ SPC")
    (should (equal (buffer-string) "hello\n\n\nworld"))))

;;; ] SPC: add empty line below

(ert-deftest ygg-helix-add-newline-below ()
  "] SPC should add empty line below cursor without moving."
  (ygg-helix-test-buffer "hello\nworld"
    (ygg-set-selection 1 1)
    (ygg-helix-test-keys "] SPC")
    (should (equal (buffer-string) "hello\n\nworld"))
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (should (equal beg 1)))))

;;; g $: keep pipe

(ert-deftest ygg-helix-keep-pipe-error ()
  "ygg-keep-pipe should keep only selections where command exits 0."
  (ygg-helix-test-buffer "a\nb\nc"
    (ygg-set-selection 1 2)
    (ygg-add-selection 4 5)
    (ygg-add-selection 7 8)
    (call-interactively (lambda () (interactive) (ygg-keep-pipe "grep -q a")))
    (should (= (ygg-selections-count) 1))
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (should (equal (buffer-substring-no-properties beg end) "a")))))

(ert-deftest ygg-helix-keep-pipe-no-survivors ()
  "ygg-keep-pipe should give an error if no selections survive."
  (ygg-helix-test-buffer "a\nb\nc"
    (ygg-set-selection 1 2)
    (ygg-add-selection 4 5)
    (should-error
      (call-interactively (lambda () (interactive) (ygg-keep-pipe "grep -q z"))))))

;;; Window map C-w bindings

(ert-deftest ygg-helix-window-cw-cw ()
  "C-w C-w should move to other window."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (split-window-below)
      (ygg-normal-state)
      (let ((w1 (selected-window)))
        (ygg-helix-test-keys "C-w C-w")
        (should-not (eq (selected-window) w1))
        (ygg-helix-test-keys "C-w C-w")
        (should (eq (selected-window) w1))))))

(ert-deftest ygg-helix-window-cw-cs ()
  "C-w C-s should split below."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (let ((count-before (length (window-list))))
        (ygg-helix-test-keys "C-w C-s")
        (should (= (length (window-list)) (1+ count-before)))))))

(ert-deftest ygg-helix-window-cw-cv ()
  "C-w C-v should split right."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (let ((count-before (length (window-list))))
        (ygg-helix-test-keys "C-w C-v")
        (should (= (length (window-list)) (1+ count-before)))))))

(ert-deftest ygg-helix-window-cw-cq ()
  "C-w C-q should close window."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (split-window-below)
      (let ((count-before (length (window-list))))
        (ygg-helix-test-keys "C-w C-q")
        (should (= (length (window-list)) (1- count-before)))))))

(ert-deftest ygg-helix-window-cw-co ()
  "C-w C-o should delete other windows."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (split-window-below)
      (split-window-right)
      (ygg-helix-test-keys "C-w C-o")
      (should (= (length (window-list)) 1)))))

(ert-deftest ygg-helix-window-cw-ch ()
  "C-w C-h should move left."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (split-window-right)
      (ygg-normal-state)
      (let ((w1 (selected-window)))
        (ygg-helix-test-keys "C-w C-h")
        (should-not (eq (selected-window) w1))))))

(ert-deftest ygg-helix-window-cw-cj ()
  "C-w C-j should move down."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (split-window-below)
      (ygg-normal-state)
      (let ((w1 (selected-window)))
        (ygg-helix-test-keys "C-w C-j")
        (should-not (eq (selected-window) w1))))))

(ert-deftest ygg-helix-window-cw-ck ()
  "C-w C-k should move up."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (split-window-below)
      (ygg-normal-state)
      (other-window 1)
      (let ((w1 (selected-window)))
        (ygg-helix-test-keys "C-w C-k")
        (should-not (eq (selected-window) w1))))))

(ert-deftest ygg-helix-window-cw-cl ()
  "C-w C-l should move right."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (split-window-right)
      (ygg-normal-state)
      (other-window 1)
      (let ((w1 (selected-window)))
        (ygg-helix-test-keys "C-w C-l")
        (should-not (eq (selected-window) w1))))))

(ert-deftest ygg-helix-window-cw-t ()
  "C-w t should transpose windows."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (split-window-below)
      (ygg-normal-state)
      (let ((w1 (selected-window)))
        (ygg-helix-test-keys "C-w t")
        (should-not (eq (selected-window) w1))))))

;;; Test that existing window bindings still work

(ert-deftest ygg-helix-window-map-plain-s ()
  "Plain 's' in C-w map should still work for split-below."
  (let ((window-min-height 1)
        (window-min-width 1))
    (ygg-helix-test-buffer "test"
      (delete-other-windows)
      (let ((count-before (length (window-list))))
        (ygg-helix-test-keys "C-w s")
        (should (= (length (window-list)) (1+ count-before)))))))

(ert-deftest ygg-helix-normal-c-r-is-redo ()
  (should (eq (lookup-key ygg-normal-map (kbd "C-r")) #'ygg-redo))
  (should (eq (lookup-key ygg-normal-map (kbd "U")) #'ygg-redo)))

(ert-deftest ygg-helix-save-selection-on-v-cap-s ()
  (should (eq (lookup-key ygg-normal-map (kbd "V S")) #'ygg-save-selection)))

(ert-deftest ygg-helix-g-s-is-first-non-blank ()
  (should-not (eq (lookup-key ygg-normal-map (kbd "g s")) #'ygg-save-selection))
  (should (eq (lookup-key ygg-normal-map (kbd "g s"))
              (lookup-key ygg-goto-map (kbd "s")))))

(ert-deftest ygg-helix-insert-c-t-c-d-bound ()
  (should (eq (lookup-key ygg-insert-map (kbd "C-t")) #'ygg-insert-indent))
  (should (eq (lookup-key ygg-insert-map (kbd "C-d")) #'ygg-insert-dedent))
  (should (eq (lookup-key ygg-insert-map (kbd "C-r")) #'ygg-insert-register)))

(ert-deftest ygg-helix-insert-c-t-indents-keeping-cursor ()
  (ygg-helix-test-buffer "    foo bar"
    (let ((tab-width 4) (indent-tabs-mode nil) (standard-indent 4))
      (goto-char (+ (point-min) 6))
      (ygg-insert-indent)
      (should (equal (buffer-string) "        foo bar"))
      (should (= (point) (+ (point-min) 10))))))

(ert-deftest ygg-helix-insert-c-d-dedents-and-clamps ()
  (ygg-helix-test-buffer "      foo bar"
    (let ((tab-width 4) (indent-tabs-mode nil) (standard-indent 4))
      (goto-char (point-max))
      (ygg-insert-dedent)
      (should (equal (buffer-string) "    foo bar"))
      (should (eq (point) (point-max)))
      (ygg-insert-dedent)
      (should (equal (buffer-string) "foo bar")))))


(ert-deftest ygg-helix-shift-width-follows-mode ()
  (with-temp-buffer (emacs-lisp-mode) (should (= (ygg-shift-width) 2)))
  (with-temp-buffer (python-mode) (should (= (ygg-shift-width) 4)))
  (with-temp-buffer (js-mode) (setq js-indent-level 2) (should (= (ygg-shift-width) 2))))

(ert-deftest ygg-helix-insert-c-t-uses-mode-shift-width ()
  (ygg-helix-test-buffer "x"
    (emacs-lisp-mode)
    (let ((indent-tabs-mode nil))
      (ygg-insert-indent)
      (should (equal (buffer-string) "  x")))))

(ert-deftest ygg-helix-insert-c-t-c-d-round-to-multiple ()
  (ygg-helix-test-buffer "   x"
    (let ((tab-width 4) (indent-tabs-mode nil) (standard-indent 4))
      (ygg-insert-indent)
      (should (equal (buffer-string) "    x"))
      (erase-buffer) (insert "       x")
      (ygg-insert-indent)
      (should (equal (buffer-string) "        x"))
      (erase-buffer) (insert "       x")
      (ygg-insert-dedent)
      (should (equal (buffer-string) "    x"))
      (ygg-insert-dedent)
      (should (equal (buffer-string) "x")))))

(ert-deftest ygg-helix-insert-c-t-with-tabs ()
  (ygg-helix-test-buffer "x"
    (let ((tab-width 4) (indent-tabs-mode t) (standard-indent 4))
      (ygg-insert-indent)
      (should (equal (buffer-string) "\tx")))))

(ert-deftest ygg-helix-normal-indent-uses-mode-shift-width ()
  (ygg-helix-test-buffer "x"
    (emacs-lisp-mode)
    (let ((indent-tabs-mode nil))
      (ygg-indent-right 1)
      (should (equal (buffer-string) "  x")))))

(ert-deftest ygg-helix-insert-c-t-undoes-in-one-step ()
  (ygg-helix-test-buffer "foo"
    (let ((indent-tabs-mode nil) (standard-indent 4) (tab-width 4))
      (undo-boundary)
      (ygg-insert-indent)
      (undo-boundary)
      (ygg-undo)
      (should (equal (buffer-string) "foo"))
      (should (= (point) 1))
      (ygg-undo)
      (should (equal (buffer-string) "foo")))))

(ert-deftest ygg-helix-insert-c-t-keeps-offset-inside-indentation ()
  (ygg-helix-test-buffer "      x"
    (let ((indent-tabs-mode nil) (standard-indent 4) (tab-width 4))
      (goto-char 3)
      (ygg-insert-indent)
      (should (equal (buffer-string) "        x"))
      (should (= (point) 5)))))

(ert-deftest ygg-helix-shift-width-c-family ()
  (with-temp-buffer (c-mode) (setq c-basic-offset 3) (should (= (ygg-shift-width) 3)))
  (with-temp-buffer (java-mode) (setq c-basic-offset 2) (should (= (ygg-shift-width) 2)))
  (with-temp-buffer (setq-local major-mode 'c-ts-mode)
                    (setq-local c-ts-mode-indent-offset 3)
                    (should (= (ygg-shift-width) 3)))
  (with-temp-buffer (setq-local major-mode 'go-ts-mode)
                    (setq-local go-ts-mode-indent-offset 3)
                    (should (= (ygg-shift-width) 3))))

(provide 'ygg-helix-verbs-tests)
;;; ygg-helix-verbs-tests.el ends here
