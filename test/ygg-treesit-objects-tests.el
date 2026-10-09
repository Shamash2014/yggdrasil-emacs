;;; ygg-treesit-objects-tests.el --- Treesit text objects through the match dispatcher -*- lexical-binding: t; -*-

(require 'ert)
(require 'treesit)
(require 'cl-lib)
(require 'yggdrasil-match)

(setq treesit-extra-load-path
      (list (expand-file-name "../tree-sitter" (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro ygg-ts-objects-tests--in (mode lang text &rest body)
  (declare (indent 3))
  `(progn
     (skip-unless (treesit-ready-p ',lang t))
     (with-temp-buffer
       (insert ,text)
       (,mode)
       ,@body)))

(defun ygg-ts-objects-tests--text (c which at)
  (goto-char (point-min))
  (search-forward at)
  (let ((b (ygg-match--textobject-bounds c which)))
    (and b (buffer-substring-no-properties (car b) (cdr b)))))

(ert-deftest ygg-ts-func-bounds-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript
      "function add(a: number, b: number): number {\n  return a + b;\n}"
    (should (string-prefix-p "function add" (ygg-ts-objects-tests--text ?f 'around "return")))
    (let ((inner (ygg-ts-objects-tests--text ?f 'inside "return")))
      (should (string-match-p "return a \\+ b" inner))
      (should-not (string-match-p "function\\|[{}]" inner)))))

(ert-deftest ygg-ts-py-func-bounds ()
  (ygg-ts-objects-tests--in python-ts-mode python "def add(a, b):\n    return a + b"
    (should (string-prefix-p "def add" (ygg-ts-objects-tests--text ?f 'around "return")))
    (should (equal (ygg-ts-objects-tests--text ?f 'inside "return") "return a + b"))))

(ert-deftest ygg-ts-param-bounds-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript "function test(a: number, b: string) {}"
    (should (equal (ygg-ts-objects-tests--text ?P 'inside "a:") "a: number"))
    (should (equal (ygg-ts-objects-tests--text ?P 'around "a:") "a: number"))))

(ert-deftest ygg-ts-loop-bounds-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript
      "for (let i = 0; i < 10; i++) {\n  console.log(i);\n}"
    (should (string-prefix-p "for" (ygg-ts-objects-tests--text ?l 'around "console")))
    (should (string-match-p "console.log" (ygg-ts-objects-tests--text ?l 'inside "console")))))

(ert-deftest ygg-ts-cond-bounds-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript
      "if (value > 10) {\n  console.log('big');\n}"
    (should (string-prefix-p "if" (ygg-ts-objects-tests--text ?C 'around "console")))
    (should (string-match-p "console.log" (ygg-ts-objects-tests--text ?C 'inside "console")))))

(ert-deftest ygg-ts-string-bounds-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript "const msg = \"hello world\";"
    (should (equal (ygg-ts-objects-tests--text ?S 'inside "hello") "hello world"))
    (should (equal (ygg-ts-objects-tests--text ?S 'around "hello") "\"hello world\""))))

(ert-deftest ygg-ts-call-bounds-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript "run(first, second);"
    (should (equal (ygg-ts-objects-tests--text ?k 'around "first") "run(first, second)"))
    (should (equal (ygg-ts-objects-tests--text ?k 'inside "first") "first, second"))))

(ert-deftest ygg-ts-comment-and-block-bounds-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript
      "function f() {\n  // note here\n  go();\n}"
    (should (equal (ygg-ts-objects-tests--text ?M 'around "note") "// note here"))
    (should (equal (ygg-ts-objects-tests--text ?M 'inside "note") "// note here"))
    (should (string-prefix-p "{" (ygg-ts-objects-tests--text ?B 'around "go()")))
    (let ((inner (ygg-ts-objects-tests--text ?B 'inside "go()")))
      (should (string-match-p "go()" inner))
      (should-not (string-match-p "[{}]" inner)))))

(ert-deftest ygg-ts-python-simple-objects ()
  (ygg-ts-objects-tests--in python-ts-mode python
      "def f(a, b):\n    for x in a:\n        print(\"hi\")  # c\n    if b:\n        pass\n"
    (should (string-prefix-p "for x" (ygg-ts-objects-tests--text ?l 'around "print")))
    (should (string-match-p "print" (ygg-ts-objects-tests--text ?l 'inside "print")))
    (should (string-prefix-p "if b" (ygg-ts-objects-tests--text ?C 'around "pass")))
    (should (equal (ygg-ts-objects-tests--text ?C 'inside "pass") "pass"))
    (should (equal (ygg-ts-objects-tests--text ?S 'inside "hi") "hi"))
    (should (equal (ygg-ts-objects-tests--text ?P 'inside "(a") "a"))
    (should (equal (ygg-ts-objects-tests--text ?k 'inside "\"hi") "\"hi\""))
    (should (equal (ygg-ts-objects-tests--text ?M 'around "# c") "# c"))))

(ert-deftest ygg-ts-type-never-selects-body ()
  (ygg-ts-objects-tests--in python-ts-mode python "class A:\n    def m(self):\n        return 1\n"
    (should (string-prefix-p "class A" (ygg-ts-objects-tests--text ?t 'around "return")))
    (should (string-prefix-p "def m" (ygg-ts-objects-tests--text ?t 'inside "return")))))

(defconst ygg-ts-objects-tests--elixir
  "defmodule M do\n  def run(a, b) do\n    IO.puts(a)\n  end\n\n  test \"x\" do\n    if a do\n      b\n    end\n  end\nend\n")

(ert-deftest ygg-ts-elixir-definitions ()
  (ygg-ts-objects-tests--in elixir-ts-mode elixir ygg-ts-objects-tests--elixir
    (should (equal (ygg-ts-objects-tests--text ?f 'inside "IO.pu") "\n    IO.puts(a)\n  "))
    (should (string-prefix-p "def run(a, b) do" (ygg-ts-objects-tests--text ?f 'around "IO.pu")))
    (should (string-prefix-p "defmodule M do" (ygg-ts-objects-tests--text ?t 'around "IO.pu")))
    (should (string-match-p "def run" (ygg-ts-objects-tests--text ?t 'inside "IO.pu")))
    (should (string-prefix-p "test \"x\" do" (ygg-ts-objects-tests--text ?T 'around "      b")))
    (should (string-prefix-p "if a do" (ygg-ts-objects-tests--text ?C 'around "      b")))
    (should (equal (ygg-ts-objects-tests--text ?B 'inside "      b") "\n      b\n    "))
    (should (string-prefix-p "do" (ygg-ts-objects-tests--text ?B 'around "      b")))))

(ert-deftest ygg-ts-loop-is-the-whole-loop ()
  (ygg-ts-objects-tests--in go-ts-mode go
      "package p\nfunc f() {\n\tfor i := 0; i < 3; i++ {\n\t\tg(i)\n\t}\n}\n"
    (should (string-prefix-p "for i := 0" (ygg-ts-objects-tests--text ?l 'around "i :=")))
    (should (string-suffix-p "}" (ygg-ts-objects-tests--text ?l 'around "i :=")))
    (should (equal (ygg-ts-objects-tests--text ?l 'inside "i :=") "\n\t\tg(i)\n\t"))))

(ert-deftest ygg-ts-type-needs-a-declaration ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript "interface I {\n  x: number;\n}\n"
    (should (string-prefix-p "interface I" (ygg-ts-objects-tests--text ?t 'around "x: nu"))))
  (ygg-ts-objects-tests--in go-ts-mode go "package p\ntype S struct {\n\tA int\n\tB string\n}\n"
    (should (equal (ygg-ts-objects-tests--text ?t 'around "A in")
                   "type S struct {\n\tA int\n\tB string\n}"))
    (should (equal (ygg-ts-objects-tests--text ?t 'inside "A in") "\n\tA int\n\tB string\n"))))

(ert-deftest ygg-ts-count-picks-the-enclosing-match ()
  (ygg-ts-objects-tests--in python-ts-mode python
      "def a():\n    def b():\n        x = 1\n        return x\n    return b\n"
    (goto-char (point-min))
    (search-forward "x = 1")
    (should (string-prefix-p "def b" (let ((ygg-match--textobject-count 1))
                                       (buffer-substring (car (ygg-match--textobject-bounds ?f 'around))
                                                         (cdr (ygg-match--textobject-bounds ?f 'around))))))
    (let* ((ygg-match--textobject-count 2)
           (b (ygg-match--textobject-bounds ?f 'around)))
      (should (string-prefix-p "def a" (buffer-substring (car b) (cdr b)))))))

(ert-deftest ygg-ts-rust-macro-and-bodyless-fn ()
  (ygg-ts-objects-tests--in rust-ts-mode rust
      "trait T { fn m(&self); }\nfn main() {\n    println!(\"a {}\", x);\n}\n"
    (should (equal (ygg-ts-objects-tests--text ?k 'inside "\"a") "\"a {}\", x"))
    (should (equal (ygg-ts-objects-tests--text ?k 'around "\"a") "println!(\"a {}\", x)"))
    (should (equal (ygg-ts-objects-tests--text ?f 'around "fn m") "fn m(&self);"))
    (should (equal (ygg-ts-objects-tests--text ?f 'inside "fn m") "fn m(&self);"))))

(ert-deftest ygg-ts-arguments-in-a-big-buffer-are-quick ()
  (ygg-ts-objects-tests--in python-ts-mode python
      (concat (mapconcat (lambda (i) (format "def f%d(a, b):\n    return a + b + %d\n\n" i i))
                         (number-sequence 1 7000) "")
              "g(first, second)\n")
    (goto-char (point-max))
    (search-backward "second")
    (should (equal (let ((b (ygg-match--textobject-bounds ?a 'inside)))
                     (buffer-substring-no-properties (car b) (cdr b)))
                   "second"))
    (should (< (car (benchmark-run 5 (ygg-match--textobject-bounds ?a 'around))) 0.1))
    (should (< (car (benchmark-run 5 (ygg-match--textobject-bounds ?P 'around))) 0.1))))

(ert-deftest ygg-ts-no-advice-on-dispatcher ()
  (should-not (advice--p (symbol-function 'ygg-match--textobject-bounds))))

(provide 'ygg-treesit-objects-tests)

(ert-deftest ygg-ts-python-block-is-the-body ()
  (ygg-ts-objects-tests--in python-ts-mode python "def f(a):\n    x = 1\n    return x\n"
    (should (string-match-p "\\`x = 1" (ygg-ts-objects-tests--text ?B 'inside "return")))
    (should (string-match-p "return x" (ygg-ts-objects-tests--text ?B 'around "return")))))

(ert-deftest ygg-ts-block-beats-object-literal ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript
      "function f() {\n  const o = {a: g(1)};\n  return o;\n}"
    (let ((around (ygg-ts-objects-tests--text ?B 'around "g(1")))
      (should (string-prefix-p "{\n  const o" around))
      (should (string-suffix-p "return o;\n}" around)))))

(ert-deftest ygg-ts-elixir-call-and-parameter ()
  (ygg-ts-objects-tests--in elixir-ts-mode elixir ygg-ts-objects-tests--elixir
    (should (equal (ygg-ts-objects-tests--text ?k 'around "IO.puts(a") "IO.puts(a)"))
    (should (ygg-ts-objects-tests--text ?P 'inside "run(a"))))

(defconst ygg-ts-objects-tests--swift-mode-dir
  (expand-file-name "../elpaca/builds/swift-ts-mode"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest ygg-ts-swift-loop-call-parameter ()
  (add-to-list 'load-path ygg-ts-objects-tests--swift-mode-dir)
  (skip-unless (and (treesit-ready-p 'swift t) (require 'swift-ts-mode nil t)))
  (ygg-ts-objects-tests--in swift-ts-mode swift
      "func f(a: Int, b: Int) {\n  for i in xs {\n    run(first, second)\n  }\n}\n"
    (should (equal (ygg-ts-objects-tests--text ?l 'inside "run") "\n    run(first, second)\n  "))
    (should (equal (ygg-ts-objects-tests--text ?k 'inside "first") "first, second"))
    (should (equal (ygg-ts-objects-tests--text ?k 'around "first") "run(first, second)"))
    (should (equal (ygg-ts-objects-tests--text ?P 'around "a: In") "a: Int"))
    (should (equal (ygg-ts-objects-tests--text ?P 'around "b: In") "b: Int"))))

(ert-deftest ygg-ts-keyword-selects-whole-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript
      "class A {\n  m() { return 1; }\n}\nfunction add(a: number) {\n  return a;\n}"
    (should (string-prefix-p "class A {" (ygg-ts-objects-tests--text ?t 'around "clas")))
    (should (string-prefix-p "function add" (ygg-ts-objects-tests--text ?f 'around "functio")))))

(ert-deftest ygg-ts-keyword-selects-whole-go ()
  (ygg-ts-objects-tests--in go-ts-mode go "package p\n\nfunc add(a int) int {\n\treturn a\n}\n"
    (should (string-prefix-p "func add" (ygg-ts-objects-tests--text ?f 'around "fun")))))

(ert-deftest ygg-ts-keyword-selects-whole-py ()
  (ygg-ts-objects-tests--in python-ts-mode python "for i in xs:\n    f = lambda x: x\n"
    (should (string-prefix-p "for i in xs" (ygg-ts-objects-tests--text ?l 'around "fo")))
    (should (equal (ygg-ts-objects-tests--text ?f 'around "lambd") "lambda x: x"))))

(ert-deftest ygg-ts-keyword-selects-whole-rust ()
  (ygg-ts-objects-tests--in rust-ts-mode rust "struct S {\n    a: i32,\n}\n"
    (should (string-prefix-p "struct S" (ygg-ts-objects-tests--text ?t 'around "stru")))))

(ert-deftest ygg-ts-object-literal-is-not-a-type-ts ()
  (ygg-ts-objects-tests--in typescript-ts-mode typescript
      "class A { m() { return {aa: 1}; } }"
    (should (string-prefix-p "class A" (ygg-ts-objects-tests--text ?t 'around "{a")))))

(defun ygg-ts-objects-tests--text-on (c which at)
  (goto-char (point-min))
  (search-forward at)
  (goto-char (match-beginning 0))
  (let ((b (ygg-match--textobject-bounds c which)))
    (and b (buffer-substring-no-properties (car b) (cdr b)))))

(ert-deftest ygg-ts-param-on-open-paren ()
  (ygg-ts-objects-tests--in go-ts-mode go "package p\n\nfunc add(a int) int {\n\treturn a\n}\n"
    (should (equal (ygg-ts-objects-tests--text-on ?P 'inside "(a int") "a int")))
  (ygg-ts-objects-tests--in typescript-ts-mode typescript "function add(a: number) {\n  return a;\n}"
    (should (equal (ygg-ts-objects-tests--text-on ?P 'inside "(a: number") "a: number")))
  (ygg-ts-objects-tests--in rust-ts-mode rust "fn add(a: i32) -> i32 {\n    a\n}\n"
    (should (equal (ygg-ts-objects-tests--text-on ?P 'inside "(a: i32") "a: i32")))
  (ygg-ts-objects-tests--in python-ts-mode python "def add(a, b):\n    return a\n"
    (should (equal (ygg-ts-objects-tests--text-on ?P 'inside "(a, b") "a"))))

(ert-deftest ygg-ts-string-not-selected-from-whitespace ()
  (ygg-ts-objects-tests--in go-ts-mode go "package p\n\nfunc f() {\n\ts := \"str\"\n}\n"
    (should-not (equal (ygg-ts-objects-tests--text-on ?S 'around " \"str") "\"str\""))))
