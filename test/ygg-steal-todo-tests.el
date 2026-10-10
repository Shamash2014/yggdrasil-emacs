;;; ygg-steal-todo-tests.el --- ] o / [ o jump between TODO keywords -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-quickfix)
(require 'hl-todo)

(defconst ygg-steal-todo-tests--text
  ";; TODO one\n(a)\n;; FIXME two\n(b)\n;; TODOS no\n;; todo lower\n(c)\n;; BUG three\n(d)\n")

(defmacro ygg-steal-todo-tests--with (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (dolist (kw ygg-qf-todo-keywords)
       (unless (assoc kw hl-todo-keyword-faces)
         (push (cons kw 'hl-todo) hl-todo-keyword-faces)))
     (insert ygg-steal-todo-tests--text)
     (goto-char (point-min))
     ,@body))

(defun ygg-steal-todo-tests--line ()
  (line-number-at-pos))

(defun ygg-steal-todo-tests--press (n key)
  (let ((current-prefix-arg n))
    (call-interactively (key-binding (kbd key)))))

(ert-deftest ygg-steal-todo-next-lands-on-keyword ()
  (ygg-steal-todo-tests--with
    (ygg-next-todo)
    (should (= (ygg-steal-todo-tests--line) 1))
    (should (looking-at "TODO one"))
    (ygg-next-todo)
    (should (= (ygg-steal-todo-tests--line) 3))
    (should (looking-at "FIXME"))))

(ert-deftest ygg-steal-todo-count-skips ()
  (ygg-steal-todo-tests--with
    (ygg-next-todo 2)
    (should (looking-at "FIXME"))
    (ygg-next-todo 2)
    (should-not (looking-at "TODO"))))

(ert-deftest ygg-steal-todo-previous-goes-back ()
  (ygg-steal-todo-tests--with
    (goto-char (point-max))
    (ygg-prev-todo)
    (should (looking-at "BUG"))
    (ygg-prev-todo)
    (should (looking-at "FIXME"))
    (ygg-prev-todo 1)
    (should (looking-at "TODO one"))))

(ert-deftest ygg-steal-todo-edges-stay-put ()
  (ygg-steal-todo-tests--with
    (goto-char (point-max))
    (ygg-next-todo)
    (should (= (point) (point-max)))
    (goto-char (point-min))
    (ygg-prev-todo)
    (should (= (point) (point-min)))
    (ygg-next-todo 3)
    (ygg-next-todo)
    (should (looking-at "BUG"))
    (let ((here (point)))
      (ygg-next-todo)
      (should (= (point) here)))))

(ert-deftest ygg-steal-todo-matching-follows-hl-todo ()
  (ygg-steal-todo-tests--with
    (ygg-next-todo 2)
    (ygg-next-todo)
    (should (looking-at "BUG"))
    (should-not (cl-some (lambda (l) (string-match-p "TODOS\\|todo lower" l))
                         (list (buffer-substring (point) (line-end-position)))))))

(ert-deftest ygg-steal-todo-keys-resolve-in-normal-and-visual ()
  (dolist (state (list ygg-normal-map ygg-visual-map))
    (should (eq (let ((b (lookup-key state (kbd "] o")))) (if (consp b) (cdr b) b))
                #'ygg-next-todo))
    (should (eq (let ((b (lookup-key state (kbd "[ o")))) (if (consp b) (cdr b) b))
                #'ygg-prev-todo))))

(provide 'ygg-steal-todo-tests)
