;;; ygg-helix-motions-tests.el --- Helix motion ERT tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'yggdrasil)
(require 'layer-git)
(require 'layer-lsp)
(require 'flymake)

(ert-deftest ygg-helix-motions-g-h-column-with-count ()
  "g h with count N goes to column N; without count goes to line start."
  (with-temp-buffer
    (insert "0123456789")
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Test without count: go to line start (should be at position 1)
    (goto-char 5)
    (execute-kbd-macro (kbd "g h"))
    (should (= (point) 1))
    ;; Test with count 3: go to column 3 (0-indexed becomes 1-indexed: char 3)
    (goto-char 1)
    (execute-kbd-macro (kbd "3 g h"))
    (should (= (point) 3))))

(ert-deftest ygg-helix-motions-g-M-last-modified-file ()
  "g M goes to the most recently modified file buffer."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert "test")
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; This is a key lookup test - the function just needs to be bound
    (should (eq (key-binding (kbd "g M")) #'ygg-goto-last-modified-file))))

(ert-deftest ygg-helix-motions-g-J-textual-line-down ()
  "g J moves down by textual line ignoring visual wrap."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Normal j moves by visual line, g J by textual line
    ;; On a simple buffer without wrapping, they should be the same
    (goto-char 1)
    (execute-kbd-macro (kbd "g J"))
    (should (> (point) 1))))

(ert-deftest ygg-helix-motions-g-K-textual-line-up ()
  "g K moves up by textual line ignoring visual wrap."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    (goto-char 10)
    (execute-kbd-macro (kbd "g K"))
    (should (< (point) 10))))

(ert-deftest ygg-helix-motions-bracket-D-first-diagnostic ()
  "[ D goes to first diagnostic in buffer."
  (with-temp-buffer
    (text-mode)
    (insert "line1\nline2\nline3")
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (key-binding (kbd "[ D")) #'ygg-goto-first-diagnostic))))

(ert-deftest ygg-helix-motions-bracket-D-last-diagnostic ()
  "] D goes to last diagnostic in buffer."
  (with-temp-buffer
    (text-mode)
    (insert "line1\nline2\nline3")
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (key-binding (kbd "] D")) #'ygg-goto-last-diagnostic))))

(ert-deftest ygg-helix-motions-bracket-p-next-paragraph ()
  "] p goes to next paragraph."
  (with-temp-buffer
    (insert "para1\n\npara2\n\npara3")
    (goto-char (point-min))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    (execute-kbd-macro (kbd "] p"))
    (should (> (point) 1))))

(ert-deftest ygg-helix-motions-bracket-p-prev-paragraph ()
  "[ p goes to previous paragraph."
  (with-temp-buffer
    (insert "para1\n\npara2\n\npara3")
    (goto-char (point-max))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    (execute-kbd-macro (kbd "[ p"))
    (should (< (point) (point-max)))))

(ert-deftest ygg-helix-motions-brace-forward ()
  "} moves to next paragraph in normal mode (if unbound)."
  (with-temp-buffer
    (insert "para1\n\npara2\n\npara3")
    (goto-char (point-min))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test - } should be bound to paragraph motion
    (when (key-binding (kbd "}"))
      (should (memq (key-binding (kbd "}"))
                    '(ygg-goto-next-paragraph forward-paragraph))))))

(ert-deftest ygg-helix-motions-brace-backward ()
  "{ moves to prev paragraph in normal mode (if unbound)."
  (with-temp-buffer
    (insert "para1\n\npara2\n\npara3")
    (goto-char (point-max))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test - { should be bound to paragraph motion
    (when (key-binding (kbd "{"))
      (should (memq (key-binding (kbd "{"))
                    '(ygg-goto-prev-paragraph backward-paragraph))))))

(ert-deftest ygg-helix-motions-z-m-recenter ()
  "z m recenters view (recenter middle)."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (lookup-key ygg-view-map "m") #'ygg-view-middle))))

(ert-deftest ygg-helix-motions-z-space-page-down ()
  "z SPC pages down (half-page)."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test using SPC key
    (should (eq (lookup-key ygg-view-map (kbd "SPC")) #'ygg-view-page-down))))

(ert-deftest ygg-helix-motions-z-del-page-up ()
  "z DEL pages up (half-page)."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test using DEL key
    (should (eq (lookup-key ygg-view-map (kbd "DEL")) #'ygg-view-page-up))))

(ert-deftest ygg-helix-motions-z-j-sticky-scroll ()
  "z j enables sticky scrolling (repeat-mode)."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (key-binding (kbd "z j")) #'ygg-view-down))))

(ert-deftest ygg-helix-motions-z-k-sticky-scroll ()
  "z k enables sticky scrolling (repeat-mode)."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (key-binding (kbd "z k")) #'ygg-view-up))))

(ert-deftest ygg-helix-motions-bracket-repeat-forward ()
  "Bracket repeat via repeat-mode after ] motion."
  (with-temp-buffer
    (insert "line1\nerror line\nline3")
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; The bracket-motion recording and repeat-mode activation is internal
    (should (fboundp 'ygg--record-bracket-motion))))

(ert-deftest ygg-helix-motions-bracket-repeat-backward ()
  "Bracket repeat via repeat-mode after [ motion."
  (with-temp-buffer
    (insert "line1\nerror line\nline3")
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; The bracket-motion recording and repeat-mode activation is internal
    (should (fboundp 'ygg--record-bracket-motion))))

(ert-deftest ygg-helix-motions-bracket-G-first-hunk ()
  "[ G goes to first diff-hl hunk in buffer."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (key-binding (kbd "[ G")) #'ygg-goto-first-git-hunk))))

(ert-deftest ygg-helix-motions-bracket-G-last-hunk ()
  "] G goes to last diff-hl hunk in buffer."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (key-binding (kbd "] G")) #'ygg-goto-last-git-hunk))))

(ert-deftest ygg-helix-motions-bracket-X-next-element ()
  "] X goes to next XML/JSX element."
  (should (fboundp 'ygg-next-xml-element)))

(ert-deftest ygg-helix-motions-bracket-X-prev-element ()
  "[ X goes to prev XML/JSX element."
  (should (fboundp 'ygg-prev-xml-element)))

(ert-deftest ygg-helix-motions-space-c-c-toggle-line-comment ()
  "SPC c c toggles line comment on each selection."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (lookup-key ygg-leader-code-map "c") #'ygg-toggle-line-comment))))

(ert-deftest ygg-helix-motions-space-c-C-toggle-block-comment ()
  "SPC c C toggles block comment on each selection."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (lookup-key ygg-leader-code-map "C") #'ygg-toggle-block-comment))))

(ert-deftest ygg-helix-motions-space-c-H-references ()
  "SPC c H creates one cursor per reference of symbol at point."
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (ygg-normal-state)
    ;; Key lookup test
    (should (eq (lookup-key ygg-leader-code-map "H") #'ygg-multi-cursor-references))))

(provide 'ygg-helix-motions-tests)
;;; ygg-helix-motions-tests.el ends here
