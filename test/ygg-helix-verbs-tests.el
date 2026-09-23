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

(provide 'ygg-helix-verbs-tests)
;;; ygg-helix-verbs-tests.el ends here
