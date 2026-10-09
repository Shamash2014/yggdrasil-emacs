;;; ygg-match-tests.el --- Tests for yggdrasil match surround and textobjects -*- lexical-binding: t; -*-

(require 'ert)
(require 'yggdrasil-match)
(require 'yggdrasil-selection)

;;; Test helper: with temporary buffer and yggdrasil mode

(defmacro ygg-with-temp-buffer (content &rest body)
  "Create a temp buffer with CONTENT, execute BODY, cleanup."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((load-prefer-newer t))
       (yggdrasil-local-mode 1))
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Surround pair function tests

(ert-deftest ygg-match--surround-pair-asterisk ()
  "Surround pair for * is (* . *)."
  (let ((pair (ygg-match--surround-pair ?*)))
    (should (equal pair (cons "*" "*")))))

(ert-deftest ygg-match--surround-pair-tight-paren ()
  "Surround pair for ) is ('(' . ')')."
  (let ((pair (ygg-match--surround-pair ?\))))
    (should (equal pair (cons "(" ")")))))

(ert-deftest ygg-match--surround-pair-quote ()
  "Surround pair for \" is ('\"' . '\"')."
  (let ((pair (ygg-match--surround-pair ?\")))
    (should (equal pair (cons "\"" "\"")))))

(ert-deftest ygg-match--surround-pair-escape ()
  "Surround pair for ESC returns nil."
  (let ((pair (ygg-match--surround-pair 27)))
    (should (equal pair nil))))

(ert-deftest ygg-match--surround-pair-tag ()
  "Surround pair for t reads tag."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "div")))
    (let ((pair (ygg-match--surround-pair ?t)))
      (should (equal pair (cons "<div>" "</div>"))))))

(ert-deftest ygg-match--surround-pair-function ()
  "Surround pair for f reads function."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
    (let ((pair (ygg-match--surround-pair ?f)))
      (should (equal pair (cons "foo(" ")"))))))

(ert-deftest ygg-match--surround-pair-bracket ()
  "Surround pair for ] is ('[' . ']')."
  (let ((pair (ygg-match--surround-pair ?\])))
    (should (equal pair (cons "[" "]")))))

(ert-deftest ygg-match--surround-pair-brace ()
  "Surround pair for } is ('{' . '}')."
  (let ((pair (ygg-match--surround-pair ?\})))
    (should (equal pair (cons "{" "}")))))

(ert-deftest ygg-match--surround-pair-underscore ()
  "Surround pair for _ is (_ . _)."
  (let ((pair (ygg-match--surround-pair ?_)))
    (should (equal pair (cons "_" "_")))))

;;; Behavioral surround tests on buffers

(ert-deftest ygg-match-surround-buffer-asterisk ()
  "Surrounding word with * gives *word*."
  (ygg-with-temp-buffer "word"
    (ygg-set-selection 1 5)
    (ygg-match-surround ?*)
    (should (equal (buffer-string) "*word*"))))

(ert-deftest ygg-match-surround-buffer-tag ()
  "Surrounding word with t div gives <div>word</div>."
  (ygg-with-temp-buffer "word"
    (ygg-set-selection 1 5)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "div")))
      (ygg-match-surround ?t))
    (should (equal (buffer-string) "<div>word</div>"))))

(ert-deftest ygg-match-surround-buffer-tag-with-attrs ()
  "Surrounding with tag and attributes preserves attributes."
  (ygg-with-temp-buffer "word"
    (ygg-set-selection 1 5)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "div class=\"x\"")))
      (ygg-match-surround ?t))
    (should (equal (buffer-string) "<div class=\"x\">word</div>"))))

(ert-deftest ygg-match-surround-buffer-function ()
  "Surrounding word with f foo gives foo(word)."
  (ygg-with-temp-buffer "word"
    (ygg-set-selection 1 5)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
      (ygg-match-surround ?f))
    (should (equal (buffer-string) "foo(word)"))))

(ert-deftest ygg-match-surround-buffer-padded-paren ()
  "Surrounding with ( gives ( word )."
  (ygg-with-temp-buffer "word"
    (ygg-set-selection 1 5)
    (ygg-match-surround ?\()
    (should (equal (buffer-string) "( word )"))))

(ert-deftest ygg-match-surround-buffer-tight-paren ()
  "Surrounding with ) gives (word)."
  (ygg-with-temp-buffer "word"
    (ygg-set-selection 1 5)
    (ygg-match-surround ?\))
    (should (equal (buffer-string) "(word)"))))

(ert-deftest ygg-match-delete-surround-buffer-asterisk ()
  "Deleting surround * removes *word*."
  (ygg-with-temp-buffer "*word*"
    (ygg-set-selection 2 6)
    (ygg-match-delete-surround ?*)
    (should (equal (buffer-string) "word"))))

(ert-deftest ygg-match-replace-surround-buffer ()
  "Replacing surround ( with [ on (word) gives [word]."
  (ygg-with-temp-buffer "(word)"
    (ygg-set-selection 2 6)
    (ygg-match-replace-surround ?\( ?\[)
    (should (equal (buffer-string) "[word]"))))

(ert-deftest ygg-match-surround-multi-selections ()
  "Surrounding with two selections wraps both."
  (ygg-with-temp-buffer "a b c"
    (ygg-set-selection 1 2)
    (ygg-add-selection 3 4)
    (ygg-match-surround ?*)
    (should (equal (buffer-string) "*a* *b* c"))))

;;; Behavioral text object tests

(ert-deftest ygg-match-textobj-seek-forward ()
  "i( with point before (a) (b) on the line seeks to a."
  (ygg-with-temp-buffer "x (a) (b)"
    (goto-char 1)
    (ygg-set-selection 1 1)
    (ygg-match-inside ?\()
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (let ((selected (buffer-substring beg end)))
        (should (equal selected "a"))))))

(ert-deftest ygg-match-textobj-seek-backward ()
  "i( with point after last pair on line seeks backward."
  (ygg-with-temp-buffer "(a) (b) x"
    (goto-char 9)
    (ygg-set-selection 9 9)
    (ygg-match-inside ?\()
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (let ((selected (buffer-substring beg end)))
        (should (equal selected "b"))))))

(ert-deftest ygg-match-textobj-count-nested ()
  "2i( on nested ((a) b) with point on a selects (a) b."
  (ygg-with-temp-buffer "((a) b)"
    (ygg-set-selection 3 3)
    (let ((current-prefix-arg 2))
      (ygg-match-inside ?\())
    (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
      (should (equal (buffer-substring beg end) "(a) b")))))

(ert-deftest ygg-match-textobj-growth-repeat ()
  "Repeating i( on inner selection grows to next level."
  (ygg-with-temp-buffer "((a b))"
    (goto-char 4)
    (ygg-set-selection 4 4)
    (ygg-match-inside ?\()
    (let ((first-selection (buffer-substring (car (ygg-selection-effective-bounds))
                                              (cadr (ygg-selection-effective-bounds)))))
      (should (equal first-selection "a b")))
    (ygg-match-inside ?\()
    (let ((second-selection (buffer-substring (car (ygg-selection-effective-bounds))
                                               (cadr (ygg-selection-effective-bounds)))))
      (should (equal second-selection "(a b)")))))

(defun ygg-match-tests--selected ()
  (pcase-let ((`(,beg ,end ,_) (ygg-selection-effective-bounds)))
    (buffer-substring beg end)))

(ert-deftest ygg-match-textobj-around-growth-repeat ()
  "Repeating a( on its own selection grows to the enclosing pair."
  (ygg-with-temp-buffer "((a b))"
    (ygg-set-selection 3 3)
    (ygg-match-around ?\()
    (should (equal (ygg-match-tests--selected) "(a b)"))
    (ygg-match-around ?\()
    (should (equal (ygg-match-tests--selected) "((a b))"))))

(ert-deftest ygg-match-textobj-inside-enclosing-beats-seek ()
  "i( inside a pair selects it rather than a later pair on the line."
  (ygg-with-temp-buffer "(a) (b)"
    (ygg-set-selection 2 2)
    (ygg-match-inside ?\()
    (should (equal (ygg-match-tests--selected) "a"))))

(ert-deftest ygg-match-textobj-seek-stays-on-line ()
  "i( outside any pair does not seek past the current line."
  (ygg-with-temp-buffer "x\n(a)"
    (ygg-set-selection 1 1)
    (ygg-match-inside ?\()
    (should (equal (ygg-match-tests--selected) "x"))))

(ert-deftest ygg-match-textobj-quote-and-word ()
  "i\" and iw keep their HEAD behaviour."
  (ygg-with-temp-buffer "say \"hi there\" now"
    (ygg-set-selection 7 7)
    (ygg-match-inside ?\")
    (should (equal (ygg-match-tests--selected) "hi there"))
    (ygg-set-selection 17 17)
    (ygg-match-inside ?w)
    (should (equal (ygg-match-tests--selected) "now"))))

(ert-deftest ygg-match-textobj-multi-selection ()
  "a( applies to every selection."
  (ygg-with-temp-buffer "(a) (b)"
    (ygg-set-selection 2 2)
    (ygg-add-selection 6 7)
    (ygg-match-around ?\()
    (should (equal (ygg-match-tests--selected) "(a)"))
    (should (equal (mapcar (lambda (ov) (buffer-substring (overlay-start ov) (overlay-end ov)))
                           ygg--secondaries)
                   '("(b)")))))

;;; Delete and replace surround

(ert-deftest ygg-match-delete-surround-quote-and-paren ()
  "md\" and md) remove exactly one delimiter on each side."
  (ygg-with-temp-buffer "f(\"x y\")"
    (ygg-set-selection 4 5)
    (ygg-match-delete-surround ?\")
    (should (equal (buffer-string) "f(x y)"))
    (ygg-set-selection 3 4)
    (ygg-match-delete-surround ?\))
    (should (equal (buffer-string) "fx y"))))

(ert-deftest ygg-match-delete-surround-function ()
  "mdf on foo(word) gives word."
  (ygg-with-temp-buffer "x foo(word) y"
    (ygg-set-selection 7 11)
    (ygg-match-delete-surround ?f)
    (should (equal (buffer-string) "x word y"))))

(ert-deftest ygg-match-delete-surround-tag ()
  "mdt on <div class=\"x\">word</div> gives word."
  (with-temp-buffer
    (html-mode)
    (yggdrasil-local-mode 1)
    (should yggdrasil-local-mode)
    (insert "a <div class=\"x\">word</div> b")
    (ygg-set-selection 18 22)
    (ygg-match-delete-surround ?t)
    (should (equal (buffer-string) "a word b"))))

(ert-deftest ygg-match-delete-surround-multi-selections ()
  "md* with a selection in each of two pairs removes both."
  (ygg-with-temp-buffer "*a* *b*"
    (ygg-set-selection 2 3)
    (ygg-add-selection 6 7)
    (ygg-match-delete-surround ?*)
    (should (equal (buffer-string) "a b"))))

(ert-deftest ygg-match-delete-surround-shared-pair-once ()
  "Two selections inside one pair delete that pair once, not its parent."
  (ygg-with-temp-buffer "((a b))"
    (ygg-set-selection 3 4)
    (ygg-add-selection 5 6)
    (ygg-match-delete-surround ?\()
    (should (equal (buffer-string) "(a b)"))))

(ert-deftest ygg-match-replace-surround-nested-selections ()
  "mr([ on selections in nested pairs rewrites both pairs intact."
  (ygg-with-temp-buffer "((a) b)"
    (ygg-set-selection 3 4)
    (ygg-add-selection 6 7)
    (ygg-match-replace-surround ?\( ?\[)
    (should (equal (buffer-string) "[[a] b]"))))

(ert-deftest ygg-match-replace-surround-adjacent-selections ()
  "mr([ on adjacent pairs keeps each delimiter with its own pair."
  (ygg-with-temp-buffer "(a)(b)"
    (ygg-set-selection 2 3)
    (ygg-add-selection 5 6)
    (ygg-match-replace-surround ?\( ?\[)
    (should (equal (buffer-string) "[a][b]"))))

(ert-deftest ygg-match-replace-surround-paren-to-tag ()
  "mr)t wraps with a tag in place of the parens."
  (ygg-with-temp-buffer "(word)"
    (ygg-set-selection 2 6)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "em")))
      (ygg-match-replace-surround ?\) ?t))
    (should (equal (buffer-string) "<em>word</em>"))))

(ert-deftest ygg-match-replace-surround-tag-to-paren ()
  "mrt) swaps a tag pair for parens."
  (with-temp-buffer
    (html-mode)
    (yggdrasil-local-mode 1)
    (should yggdrasil-local-mode)
    (insert "<em>word</em>")
    (ygg-set-selection 5 9)
    (ygg-match-replace-surround ?t ?\))
    (should (equal (buffer-string) "(word)"))))

(ert-deftest ygg-match-replace-surround-star-to-function ()
  "mr*f swaps a char pair for a function call."
  (ygg-with-temp-buffer "*word*"
    (ygg-set-selection 2 6)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
      (ygg-match-replace-surround ?* ?f))
    (should (equal (buffer-string) "foo(word)"))))

(ert-deftest ygg-match-surround-buffer-padded-brace ()
  "Surrounding with { gives { word }."
  (ygg-with-temp-buffer "word"
    (ygg-set-selection 1 5)
    (ygg-match-surround ?\{)
    (should (equal (buffer-string) "{ word }"))))

(ert-deftest ygg-match-textobj-miss-keeps-selection ()
  "A textobject with nothing to select leaves a multi-cell selection whole."
  (ygg-with-temp-buffer "word more"
    (ygg-set-selection 1 5)
    (ygg-match-inside ?\()
    (should (equal (ygg-match-tests--selected) "word"))))

;;; m m — matching bracket

(ert-deftest ygg-match-jump-normal-collapses ()
  "mm in normal state moves to the matching bracket as a point."
  (ygg-with-temp-buffer "a(b c)d"
    (ygg-set-selection 2 3)
    (ygg-match-jump)
    (should (equal (butlast (ygg-selection-effective-bounds)) '(6 7)))
    (ygg-match-jump)
    (should (equal (butlast (ygg-selection-effective-bounds)) '(2 3)))))

(ert-deftest ygg-match-jump-visual-selects-span ()
  "mm in visual state selects bracket to bracket."
  (ygg-with-temp-buffer "a(b c)d"
    (ygg-visual-state)
    (ygg-set-selection 2 3)
    (ygg-match-jump)
    (should (equal (ygg-match-tests--selected) "(b c)"))))

(ert-deftest ygg-match-jump-miss-keeps-selection ()
  "mm with no bracket on the line leaves the selection whole."
  (ygg-with-temp-buffer "word more"
    (ygg-set-selection 1 5)
    (ygg-match-jump)
    (should (equal (ygg-match-tests--selected) "word"))))

;;; Helix tree-sitter keys

(defmacro ygg-match-tests--with-python (content &rest body)
  (declare (indent 1))
  `(let ((treesit-extra-load-path (list (expand-file-name "~/.emacs.d/tree-sitter"))))
     (skip-unless (treesit-ready-p 'python t))
     (with-temp-buffer
       (python-ts-mode)
       (yggdrasil-local-mode 1)
       (should yggdrasil-local-mode)
       (insert ,content)
       ,@body)))

(defun ygg-match-tests--regions ()
  (mapcar (lambda (r) (buffer-substring (car r) (cadr r))) (ygg--selection-regions)))

(defconst ygg-match-tests--python "def f(a, b):\n    return a + b\n")

(ert-deftest ygg-match-helix-v-keys-bound ()
  "V o V i V p alias m + m - m N, V n stays skip-to-next-match, no Meta key."
  (ygg-match-tests--with-python ygg-match-tests--python
    (dolist (pair '(("V o" . "+") ("V i" . "-") ("V p" . "N")))
      (should (eq (key-binding (kbd (car pair)))
                  (lookup-key ygg-match-map (kbd (cdr pair))))))
    (should (eq (key-binding (kbd "V n")) #'ygg-skip-to-next-match))
    (dolist (key '("M-o" "M-i" "M-p" "M-n" "M-I" "M-a" "M-e" "M-b"))
      (should-not (lookup-key ygg-normal-map (kbd key))))
    (should (eq (key-binding (kbd "VI")) #'ygg-treesit-select-children))
    (should (eq (key-binding (kbd "Va")) #'ygg-treesit-select-siblings))
    (should (eq (key-binding (kbd "Ve")) #'ygg-treesit-parent-node-end))
    (should (eq (key-binding (kbd "Vb")) #'ygg-treesit-parent-node-start))))

(ert-deftest ygg-match-helix-expand-shrink-keys ()
  "V o grows a to the parameter list, V i restores it."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-set-selection 7 8)
    (call-interactively (key-binding (kbd "Vo")))
    (should (equal (ygg-match-tests--selected) "(a, b)"))
    (call-interactively (key-binding (kbd "Vi")))
    (should (equal (ygg-match-tests--selected) "a"))))

(ert-deftest ygg-match-helix-sibling-keys ()
  "m n and V p step between parameters."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-set-selection 7 8)
    (call-interactively (lookup-key ygg-match-map (kbd "n")))
    (should (equal (ygg-match-tests--selected) "b"))
    (call-interactively (key-binding (kbd "Vp")))
    (should (equal (ygg-match-tests--selected) "a"))))

(ert-deftest ygg-match-helix-select-children ()
  "V I on the parameter list gives one selection per parameter."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-set-selection 6 12)
    (call-interactively (key-binding (kbd "VI")))
    (should (equal (ygg-match-tests--regions) '("a" "b")))))

(ert-deftest ygg-match-helix-select-children-leaf-stays ()
  "V I on a node without named children keeps the selection."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-set-selection 7 8)
    (ygg-treesit-select-children)
    (should (equal (ygg-match-tests--regions) '("a")))))

(ert-deftest ygg-match-helix-select-siblings ()
  "V a on one parameter selects every parameter."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-set-selection 7 8)
    (call-interactively (key-binding (kbd "Va")))
    (should (equal (ygg-match-tests--regions) '("a" "b")))))

(ert-deftest ygg-match-helix-parent-node-end ()
  "V e lands just past the covering node."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-set-selection 7 8)
    (call-interactively (key-binding (kbd "Ve")))
    (should (equal (butlast (ygg-selection-effective-bounds)) '(8 9)))))

(ert-deftest ygg-match-helix-parent-node-start ()
  "V b goes to the node start, then to the parent's when already there."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-set-selection 28 29)
    (call-interactively (key-binding (kbd "Vb")))
    (should (equal (butlast (ygg-selection-effective-bounds)) '(25 26)))
    (ygg-set-selection 10 11)
    (call-interactively (key-binding (kbd "Vb")))
    (should (equal (butlast (ygg-selection-effective-bounds)) '(6 7)))))

(ert-deftest ygg-match-helix-parent-node-visual-extends ()
  "V e in visual state keeps the anchor."
  (ygg-match-tests--with-python ygg-match-tests--python
    (ygg-visual-state)
    (ygg-set-selection 7 8)
    (ygg-treesit-parent-node-end)
    (should (equal (ygg-match-tests--selected) "a,"))))

;;; f t T m x textobjects

(defun ygg-match-tests--obj (c which text-before)
  "Text of object C/WHICH with point just after TEXT-BEFORE's first match."
  (goto-char (point-min))
  (search-forward text-before)
  (when-let* ((b (ygg-match--textobject-bounds c which)))
    (buffer-substring (car b) (cdr b))))

(defmacro ygg-match-tests--with-ts (mode lang content &rest body)
  (declare (indent 3))
  `(let ((treesit-extra-load-path '("/Users/shamash/emacs-31/tree-sitter")))
     (skip-unless (treesit-language-available-p ',lang))
     (with-temp-buffer
       (insert ,content)
       (,mode)
       ,@body)))

(ert-deftest ygg-match-textobj-elisp-function ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo (a)\n  \"doc\"\n  (bar a))\n")
    (should (equal (ygg-match-tests--obj ?f 'around "(bar") "(defun foo (a)\n  \"doc\"\n  (bar a))"))
    (should (equal (ygg-match-tests--obj ?f 'inside "(bar") "\"doc\"\n  (bar a)"))))

(ert-deftest ygg-match-textobj-elisp-test ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo () 1)\n\n(ert-deftest my-test ()\n  (should t))\n")
    (should (equal (ygg-match-tests--obj ?T 'inside "(should") "(should t)"))
    (should (equal (ygg-match-tests--obj ?T 'around "(should")
                   "(ert-deftest my-test ()\n  (should t))"))
    (should-not (ygg-match-tests--obj ?T 'inside "foo"))))

(ert-deftest ygg-match-textobj-elisp-type ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defstruct point x y)\n")
    (should (equal (ygg-match-tests--obj ?t 'around "x y") "(cl-defstruct point x y)"))
    (should (equal (ygg-match-tests--obj ?t 'inside "x y") "x y"))))

(ert-deftest ygg-match-textobj-closest-pair ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a [b \"c d\"] e)")
    (should (equal (ygg-match-tests--obj ?m 'around "c ") "\"c d\""))
    (should (equal (ygg-match-tests--obj ?m 'inside "c ") "c d"))
    (should (equal (ygg-match-tests--obj ?m 'inside "[b") "b \"c d\""))
    (should (equal (ygg-match-tests--obj ?m 'around " e") "(a [b \"c d\"] e)"))))

(ert-deftest ygg-match-textobj-closest-pair-grows ()
  (ygg-with-temp-buffer "((a b))"
    (ygg-set-selection 3 3)
    (ygg-match-inside ?m)
    (should (equal (ygg-match-tests--selected) "a b"))
    (ygg-match-inside ?m)
    (should (equal (ygg-match-tests--selected) "(a b)"))))

(ert-deftest ygg-match-textobj-tag-moved-to-x ()
  (with-temp-buffer
    (html-mode)
    (insert "<div><b>hi</b></div>")
    (should (equal (ygg-match-tests--obj ?x 'inside "hi") "hi"))
    (should (equal (ygg-match-tests--obj ?x 'around "hi") "<b>hi</b>"))
    (should-not (ygg-match-tests--obj ?t 'inside "hi"))))

(defconst ygg-match-tests--py-src
  "class A:\n    def test_it(self, x):\n        y = x\n        return y\n\n    # note\n    def other(self):\n        pass\n")

(ert-deftest ygg-match-textobj-ts-python-function ()
  (ygg-match-tests--with-ts python-ts-mode python ygg-match-tests--py-src
    (should (equal (ygg-match-tests--obj ?f 'inside "y = x") "y = x\n        return y"))
    (should (string-prefix-p "def test_it" (ygg-match-tests--obj ?f 'around "y = x")))
    (should (string-suffix-p "return y" (ygg-match-tests--obj ?f 'around "y = x")))
    (should (string-prefix-p "def other" (ygg-match-tests--obj ?f 'around "pass")))))

(ert-deftest ygg-match-textobj-ts-python-type ()
  (ygg-match-tests--with-ts python-ts-mode python ygg-match-tests--py-src
    (should (string-prefix-p "class A:" (ygg-match-tests--obj ?t 'around "y = x")))
    (should (string-prefix-p "def test_it" (ygg-match-tests--obj ?t 'inside "y = x")))
    (should (string-prefix-p "class A:" (ygg-match-tests--obj ?t 'around "note")))))

(ert-deftest ygg-match-textobj-ts-python-test ()
  (ygg-match-tests--with-ts python-ts-mode python ygg-match-tests--py-src
    (should (string-prefix-p "def test_it" (ygg-match-tests--obj ?T 'around "y = x")))
    (should (equal (ygg-match-tests--obj ?T 'inside "y = x") "y = x\n        return y"))
    (should-not (ygg-match-tests--obj ?T 'around "pass"))))

(ert-deftest ygg-match-textobj-ts-python-comment ()
  (ygg-match-tests--with-ts python-ts-mode python ygg-match-tests--py-src
    (should (equal (ygg-match-tests--obj ?c 'inside "# no") "# note"))))

(ert-deftest ygg-match-textobj-ts-typescript-function-braces ()
  (ygg-match-tests--with-ts typescript-ts-mode typescript "function f(a: number) {\n  return a;\n}\n"
    (should (equal (ygg-match-tests--obj ?f 'inside "return") "\n  return a;\n"))
    (should (equal (ygg-match-tests--obj ?f 'around "return")
                   "function f(a: number) {\n  return a;\n}"))))

(ert-deftest ygg-match-textobj-ts-typescript-test-call ()
  (ygg-match-tests--with-ts typescript-ts-mode typescript "it('works', () => {\n  expect(1);\n});\n"
    (should (equal (ygg-match-tests--obj ?T 'inside "expect") "\n  expect(1);\n"))
    (should (string-prefix-p "it('works'" (ygg-match-tests--obj ?T 'around "expect")))))

(ert-deftest ygg-match-textobj-ts-tsx-tag ()
  (ygg-match-tests--with-ts tsx-ts-mode tsx "const a = <div><b>hi</b></div>;\n"
    (should (equal (ygg-match-tests--obj ?x 'around "hi") "<b>hi</b>"))
    (should (equal (ygg-match-tests--obj ?x 'inside "hi") "hi"))))

(ert-deftest ygg-match-textobj-function-grows-on-repeat ()
  (ygg-match-tests--with-ts python-ts-mode python
      "def a():\n    def b():\n        x = 1\n    return b\n"
    (yggdrasil-local-mode 1)
    (goto-char (point-min))
    (search-forward "x = 1")
    (ygg-set-selection (point) (point))
    (ygg-match-around ?f)
    (should (string-prefix-p "def b" (ygg-match-tests--selected)))
    (ygg-match-around ?f)
    (should (string-prefix-p "def a" (ygg-match-tests--selected)))))

(ert-deftest ygg-match-textobj-python-mode-function-inside-is-the-body ()
  (with-temp-buffer
    (python-mode)
    (insert "def f(a,\n      b):\n    x = 1\n    return x\n\ndef g():\n    pass\n")
    (should (equal (ygg-match-tests--obj ?f 'inside "x = 1") "    x = 1\n    return x"))
    (should (string-prefix-p "def f(a," (ygg-match-tests--obj ?f 'around "x = 1")))))

(ert-deftest ygg-match-textobj-elisp-string-and-comment ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo ()\n  \"the doc\"\n  ;; a note\n  1)\n")
    (should (equal (ygg-match-tests--obj ?S 'inside "the d") "the doc"))
    (should (equal (ygg-match-tests--obj ?S 'around "the d") "\"the doc\""))
    (should (equal (ygg-match-tests--obj ?M 'inside "a no") ";; a note"))
    (should-not (ygg-match-tests--obj ?S 'inside "a no"))))

(ert-deftest ygg-match-textobj-combobulate-function ()
  (let ((dir "/Users/shamash/emacs-31/elpaca/builds/combobulate"))
    (skip-unless (and (member ".elc" load-suffixes)
                      (file-exists-p (expand-file-name "combobulate.elc" dir))))
    (add-to-list 'load-path dir)
    (skip-unless (locate-library "combobulate"))
    (ygg-match-tests--with-ts python-ts-mode python ygg-match-tests--py-src
      (require 'combobulate)
      (combobulate-mode 1)
      (goto-char (point-min))
      (search-forward "y = x")
      (let ((fn (ygg-match--combobulate-node 'function)))
        (should (string-prefix-p "def test_it" (treesit-node-text fn t))))
      (should (equal (treesit-node-type (ygg-match--combobulate-node 'type)) "class_definition")))))

(provide 'ygg-match-tests)
;;; ygg-match-tests.el ends here
