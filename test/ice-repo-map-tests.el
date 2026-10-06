;;; ice-repo-map-tests.el --- Tests for etc/ice/ice-repo-map -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst ice-repo-map-tests--script
  (expand-file-name "../etc/ice/ice-repo-map" (file-name-directory (or load-file-name buffer-file-name))))

(defconst ice-repo-map-tests--files
  '(("lisp/core.el" . ";;; core.el --- Core helpers -*- lexical-binding: t; -*-\n\n(defcustom core-widget-limit 10\n  \"How many widgets.\"\n  :type 'integer)\n\n(defun core-compute-total (items)\n  \"Sum ITEMS.\"\n  (apply #'+ items))\n\n(cl-defun core-merge-records (&key left right)\n  (append left right))\n\n(defun core-normalize-name (name)\n  (downcase name))\n\n(defun core--private-helper ()\n  nil)\n\n(transient-define-prefix core-dispatch ()\n  [(\"x\" \"run\" core-compute-total)])\n\n(define-minor-mode core-watch-mode\n  \"Watch things.\"\n  :global t)\n\n(provide 'core)\n")
    ("lisp/alpha.el" . ";;; alpha.el --- Alpha --- -*- lexical-binding: t; -*-\n\n(defun alpha-run-all (items)\n  (core-compute-total (core-merge-records items items))\n  (core-normalize-name \"x\"))\n\n(defun alpha-limit ()\n  core-widget-limit)\n")
    ("lisp/beta.el" . ";;; beta.el --- Beta -*- lexical-binding: t; -*-\n\n(defun beta-go ()\n  (alpha-run-all (list 1 2))\n  (core-compute-total (list 3))\n  (core-merge-records nil nil))\n")
    ("lisp/gamma.el" . ";;; gamma.el --- Gamma -*- lexical-binding: t; -*-\n\n(defun gamma-sweep ()\n  (core-compute-total nil)\n  (core-normalize-name \"y\"))\n")
    ("lisp/leaf.el" . ";;; leaf.el --- Leaf -*- lexical-binding: t; -*-\n\n(defun leaf-unique-entry ()\n  (core-normalize-name \"z\"))\n")
    ("app/greeter.dart" . "/// Greets people.\nclass Greeter extends Base {\n  Greeter();\n  String greet(String name) => 'hi $name';\n}\n\nvoid runGreeter() {}\n\nenum Color { red, green }\n\nmixin Walker {\n  void walk() {}\n}\n\nextension StrExt on String {\n  int get len => 1;\n}\n")
    ("web/box.ts" . "export function tsHelperOpen(x: number) { return x }\n\nexport class TsBox {\n  open() { return 1 }\n}\n")
    ("tools/thing.py" . "def py_helper_run():\n    return 1\n\n\nclass PyThing:\n    def go(self):\n        return py_helper_run()\n"))
  "A small repository with elisp, dart, ts and py sources.")

(defvar ice-repo-map-tests--extra nil)

(defun ice-repo-map-tests--filler ()
  (mapconcat (lambda (n) (format "(defun gen-fn-%03d (x)\n  (core-compute-total x))\n" n))
             (number-sequence 1 400) "\n"))

(defmacro ice-repo-map-tests--with-repo (var filler &rest body)
  (declare (indent 2))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "ice-repo-map" t))))
          (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment)))
     (unwind-protect
         (progn
           (let ((default-directory ,var))
             (call-process "git" nil nil nil "init" "-q"))
           (pcase-dolist (`(,path . ,text)
                          (append ice-repo-map-tests--files ice-repo-map-tests--extra
                                  (and ,filler `(("lisp/gen.el" . ,(ice-repo-map-tests--filler))))))
             (let ((file (expand-file-name path ,var)))
               (make-directory (file-name-directory file) t)
               (with-temp-file file (insert text))))
           ,@body)
       (delete-directory ,var t))))

(defvar ice-repo-map-tests--cwd nil)

(defun ice-repo-map-tests--call (root args)
  (let ((default-directory (or ice-repo-map-tests--cwd (if (file-directory-p root) root (file-name-directory root))))
        (err (make-temp-file "ice-repo-map-err")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process ice-repo-map-tests--script nil (list (current-buffer) err) nil
                               "--root" root args)))
            (list status (buffer-string) (with-temp-buffer (insert-file-contents err) (buffer-string)))))
      (delete-file err))))

(defun ice-repo-map-tests--run (root &rest args)
  (pcase-let ((`(,status ,out ,err) (ice-repo-map-tests--call root args)))
    (ert-info (err) (should (= 0 status)))
    out))

(defun ice-repo-map-tests--lat-check (root)
  (unless (executable-find "lat") (ert-skip "lat is not on PATH"))
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (call-process "lat" nil t nil "check")))
        (ert-info ((buffer-string)) (should (= 0 status)))))))

(defun ice-repo-map-tests--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun ice-repo-map-tests--json (root &rest args)
  (json-parse-string (apply #'ice-repo-map-tests--run root "--json" args)
                     :object-type 'alist :array-type 'list))

(defun ice-repo-map-tests--file (data path)
  (cl-find path (alist-get 'files data) :key (lambda (f) (alist-get 'path f)) :test #'equal))

(defun ice-repo-map-tests--defs (data path)
  (mapcar (lambda (d) (cons (alist-get 'name d) (alist-get 'kind d)))
          (alist-get 'defs (ice-repo-map-tests--file data path))))

(defun ice-repo-map-tests--def-rank (data path name)
  (alist-get 'rank (cl-find name (alist-get 'defs (ice-repo-map-tests--file data path))
                            :key (lambda (d) (alist-get 'name d)) :test #'equal)))

(defun ice-repo-map-tests--def (data path name)
  (cl-find name (alist-get 'defs (ice-repo-map-tests--file data path))
           :key (lambda (d) (alist-get 'name d)) :test #'equal))

(defun ice-repo-map-tests--def-count (text)
  (length (seq-filter (lambda (l) (string-prefix-p "- " l)) (split-string text "\n"))))

(ert-deftest ice-repo-map-extracts-elisp-forms ()
  (ice-repo-map-tests--with-repo root nil
    (let ((defs (ice-repo-map-tests--defs (ice-repo-map-tests--json root) "lisp/core.el")))
      (should (equal "option" (cdr (assoc "core-widget-limit" defs))))
      (should (equal "function" (cdr (assoc "core-compute-total" defs))))
      (should (equal "function" (cdr (assoc "core-merge-records" defs))))
      (should (equal "function" (cdr (assoc "core--private-helper" defs))))
      (should (equal "transient" (cdr (assoc "core-dispatch" defs))))
      (should (equal "mode" (cdr (assoc "core-watch-mode" defs))))
      (should-not (assoc "provide" defs)))))

(ert-deftest ice-repo-map-extracts-dart-kinds ()
  (ice-repo-map-tests--with-repo root nil
    (let ((defs (ice-repo-map-tests--defs (ice-repo-map-tests--json root) "app/greeter.dart")))
      (should (equal "class" (cdr (assoc "Greeter" defs))))
      (should (equal "constructor" (cdr (assoc "Greeter" (cdr (member '("Greeter" . "class") defs))))))
      (should (equal "function" (cdr (assoc "greet" defs))))
      (should (equal "function" (cdr (assoc "runGreeter" defs))))
      (should (equal "enum" (cdr (assoc "Color" defs))))
      (should (equal "mixin" (cdr (assoc "Walker" defs))))
      (should (equal "extension" (cdr (assoc "StrExt" defs))))
      (should (equal "getter" (cdr (assoc "len" defs)))))))

(ert-deftest ice-repo-map-extracts-ts-and-py ()
  (ice-repo-map-tests--with-repo root nil
    (let* ((data (ice-repo-map-tests--json root))
           (ts (ice-repo-map-tests--defs data "web/box.ts"))
           (py (ice-repo-map-tests--defs data "tools/thing.py")))
      (should (assoc "tsHelperOpen" ts))
      (should (equal "class" (cdr (assoc "TsBox" ts))))
      (should (assoc "open" ts))
      (should (assoc "py_helper_run" py))
      (should (equal "class" (cdr (assoc "PyThing" py))))
      (should (assoc "go" py)))))

(ert-deftest ice-repo-map-ranks-core-first ()
  (ice-repo-map-tests--with-repo root nil
    (let ((files (alist-get 'files (ice-repo-map-tests--json root))))
      (should (equal "lisp/core.el" (alist-get 'path (car files)))))))

(ert-deftest ice-repo-map-focus-lifts-leaf ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (alist-get 'rank (ice-repo-map-tests--file (ice-repo-map-tests--json root) "lisp/leaf.el")))
          (focused (alist-get 'rank (ice-repo-map-tests--file
                                     (ice-repo-map-tests--json root "--focus" "lisp/leaf.el")
                                     "lisp/leaf.el"))))
      (should (> focused plain)))))

(ert-deftest ice-repo-map-mention-boosts-def ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (ice-repo-map-tests--def-rank (ice-repo-map-tests--json root) "lisp/core.el" "core-widget-limit"))
          (boosted (ice-repo-map-tests--def-rank
                    (ice-repo-map-tests--json root "--mention" "core-widget-limit")
                    "lisp/core.el" "core-widget-limit")))
      (should (> boosted plain)))))

(ert-deftest ice-repo-map-fits-budget ()
  (ice-repo-map-tests--with-repo root t
    (let ((counts (mapcar (lambda (budget)
                            (let ((out (ice-repo-map-tests--run root "--budget" (number-to-string budget))))
                              (should (<= (length out) (* budget 4)))
                              (ice-repo-map-tests--def-count out)))
                          '(256 1024 2000))))
      (should (< (nth 0 counts) (nth 1 counts)))
      (should (< (nth 1 counts) (nth 2 counts))))))

(ert-deftest ice-repo-map-write-is-idempotent-and-checks ()
  (ice-repo-map-tests--with-repo root nil
    (ice-repo-map-tests--run root "--write")
    (ice-repo-map-tests--run root "--write")
    (let ((index (expand-file-name "lat.md/lat.md" root)))
      (should (file-exists-p (expand-file-name "lat.md/repo-map.md" root)))
      (should (= 1 (with-temp-buffer
                     (insert-file-contents index)
                     (how-many "^- \\[\\[repo-map\\]\\] — ranked repo map: key files and symbols, generated by ice-repo-map$"
                               (point-min) (point-max)))))
      (ice-repo-map-tests--lat-check root))))

(ert-deftest ice-repo-map-write-keeps-existing-index ()
  (ice-repo-map-tests--with-repo root nil
    (make-directory (expand-file-name "lat.md" root))
    (with-temp-file (expand-file-name "lat.md/lat.md" root)
      (insert "# Project\n\nThe map.\n"))
    (ice-repo-map-tests--run root "--write")
    (with-temp-buffer
      (insert-file-contents (expand-file-name "lat.md/lat.md" root))
      (should (string-prefix-p "# Project\n\nThe map.\n- [[repo-map]]" (buffer-string))))))

(ert-deftest ice-repo-map-is-deterministic ()
  (ice-repo-map-tests--with-repo root nil
    (should (equal (ice-repo-map-tests--run root) (ice-repo-map-tests--run root)))))

(ert-deftest ice-repo-map-cites-by-language ()
  (ice-repo-map-tests--with-repo root nil
    (let ((out (ice-repo-map-tests--run root "--budget" "100000")))
      (should (string-match-p "^- `core-compute-total` (lisp/core\\.el:[0-9]+) — function:" out))
      (should (string-match-p "^- `runGreeter` (app/greeter\\.dart:[0-9]+) — function:" out))
      (should (string-match-p "^- \\[\\[web/box\\.ts#tsHelperOpen\\]\\] — function:" out))
      (should (string-match-p "^- \\[\\[web/box\\.ts#TsBox#open\\]\\] — method:" out))
      (should (string-match-p "^- \\[\\[tools/thing\\.py#py_helper_run\\]\\] — function:" out))
      (should-not (string-match-p "\\[\\[[^]]*\\.\\(el\\|dart\\)#" out)))))

(defconst ice-repo-map-tests--dart-extra
  '(("app/more.dart" . "typedef int Cmp(int a, int b);\n\nint get topGet => 1;\n\nset topSet(int v) {}\n\nextension on int {\n  int double() => this * 2;\n}\n\nclass Repo {\n  Repo();\n  Repo.named();\n  factory Repo.fromJson(Map m) => Repo();\n  const Repo.constant();\n}\n")))

(defmacro ice-repo-map-tests--with-extra (extra &rest body)
  (declare (indent 1))
  `(let ((ice-repo-map-tests--extra ,extra))
     (ice-repo-map-tests--with-repo root nil ,@body)))

(ert-deftest ice-repo-map-budget-defaults-to-2000-and-clamps ()
  (ice-repo-map-tests--with-repo root t
    (let ((default (ice-repo-map-tests--run root))
          (explicit (ice-repo-map-tests--run root "--budget" "2000"))
          (clamped (ice-repo-map-tests--run root "--budget" "9000")))
      (should (<= (string-bytes default) (* 2000 4)))
      (should (<= (string-bytes clamped) (* 2000 4)))
      (should (equal default explicit))
      (should (equal default clamped)))))

(ert-deftest ice-repo-map-write-is-full ()
  (ice-repo-map-tests--with-repo root t
    (ice-repo-map-tests--run root "--write")
    (let ((full (ice-repo-map-tests--slurp (expand-file-name "lat.md/repo-map.md" root)))
          (condensed (ice-repo-map-tests--run root)))
      (should (= 400 (with-temp-buffer (insert full) (how-many "^- `gen-fn-[0-9]+`" (point-min) (point-max)))))
      (should (< (with-temp-buffer (insert condensed) (how-many "^- `gen-fn-[0-9]+`" (point-min) (point-max))) 400))
      (should (> (string-bytes full) (* 2000 4))))
    (ice-repo-map-tests--lat-check root)))

(ert-deftest ice-repo-map-missing-focus-is-ignored ()
  (ice-repo-map-tests--with-repo root nil
    (should (equal (ice-repo-map-tests--run root) (ice-repo-map-tests--run root "--focus" "lisp/nope.el")))
    (let ((json (ice-repo-map-tests--run root "--json" "--focus" "lisp/nope.el")))
      (should (equal json (ice-repo-map-tests--run root "--json")))
      (should-not (string-match-p "null" json)))))

(ert-deftest ice-repo-map-focus-resolves-against-cwd ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (ice-repo-map-tests--run root "--json"))
          (focused (let ((ice-repo-map-tests--cwd (expand-file-name "lisp/" root)))
                     (ice-repo-map-tests--run root "--json" "--focus" "leaf.el"))))
      (should-not (equal plain focused)))))

(ert-deftest ice-repo-map-focus-moves-file-to-top ()
  (ice-repo-map-tests--with-repo root nil
    (let ((paths (lambda (data) (mapcar (lambda (f) (alist-get 'path f)) (alist-get 'files data)))))
      (should-not (member "lisp/leaf.el" (seq-take (funcall paths (ice-repo-map-tests--json root)) 3)))
      (should (member "lisp/leaf.el" (seq-take (funcall paths (ice-repo-map-tests--json root "--focus" "lisp/leaf.el")) 3))))))

(ert-deftest ice-repo-map-files-follow-file-rank ()
  (ice-repo-map-tests--with-repo root nil
    (let ((ranks (mapcar (lambda (f) (alist-get 'rank f)) (alist-get 'files (ice-repo-map-tests--json root)))))
      (should (> (length ranks) 3))
      (should (equal ranks (sort (copy-sequence ranks) #'>))))))

(ert-deftest ice-repo-map-flow-is-split-across-owners ()
  (let* ((one '("lisp/one.el" . "(defun split-shared-name () nil)\n"))
         (user '("lisp/user.el" . "(defun split-user () (split-shared-name))\n"))
         (rank (lambda (extra)
                 (let ((ice-repo-map-tests--files nil))
                   (ice-repo-map-tests--with-extra extra
                     (ice-repo-map-tests--def-rank (ice-repo-map-tests--json root) "lisp/one.el" "split-shared-name")))))
         (single (funcall rank (list one user)))
         (dual (funcall rank (list one '("lisp/two.el" . "(defun split-shared-name () nil)\n") user))))
    (should (< dual single))))

(ert-deftest ice-repo-map-downweights-groups-faces-tests-and-private-names ()
  (ice-repo-map-tests--with-extra
      '(("lisp/heavy.el" . "(defun heavy-used-fn () nil)\n(defgroup heavy-group nil \"g\")\n(defface heavy-face nil \"f\")\n(defun heavy--private-fn () nil)\n")
        ("test/heavy-tests.el" . "(defun heavy-test-helper () (heavy-used-fn))\n")
        ("lisp/user.el" . "(defun heavy-user () (heavy-used-fn) (heavy-group) (heavy-face) (heavy--private-fn) (heavy-test-helper))\n"))
    (let* ((data (ice-repo-map-tests--json root))
           (used (ice-repo-map-tests--def-rank data "lisp/heavy.el" "heavy-used-fn")))
      (should (> used (ice-repo-map-tests--def-rank data "lisp/heavy.el" "heavy-group")))
      (should (> used (ice-repo-map-tests--def-rank data "lisp/heavy.el" "heavy-face")))
      (should (> used (ice-repo-map-tests--def-rank data "lisp/heavy.el" "heavy--private-fn")))
      (should (> used (ice-repo-map-tests--def-rank data "test/heavy-tests.el" "heavy-test-helper"))))))

(ert-deftest ice-repo-map-methods-without-a-local-generic-are-downweighted ()
  (ice-repo-map-tests--with-extra
      '(("lisp/meth.el" . "(cl-defgeneric meth-local (x))\n(cl-defmethod meth-local ((x list)) x)\n(cl-defmethod meth-foreign ((x list)) x)\n")
        ("lisp/user.el" . "(defun meth-user () (meth-local 1) (meth-foreign 1))\n"))
    (let ((data (ice-repo-map-tests--json root)))
      (should (> (ice-repo-map-tests--def-rank data "lisp/meth.el" "meth-local")
                 (ice-repo-map-tests--def-rank data "lisp/meth.el" "meth-foreign"))))))

(ert-deftest ice-repo-map-strings-and-comments-are-not-references ()
  (ice-repo-map-tests--with-extra
      '(("lisp/lib.el" . "(defun lone-referenced-fn () nil)\n(defun real-referenced-fn () nil)\n")
        ("lisp/a.el" . ";; lone-referenced-fn\n(defun a-fn ()\n  \"lone-referenced-fn\"\n  (real-referenced-fn))\n")
        ("web/lib2.ts" . "export function commentOnlyName() {}\nexport function realUsedName() {}\n")
        ("web/c.ts" . "// commentOnlyName\nconst s = \"commentOnlyName\";\nrealUsedName();\n")
        ("tools/lib3.py" . "def comment_only_py():\n    return 1\n\n\ndef real_used_py():\n    return 1\n")
        ("tools/c.py" . "# comment_only_py\ns = 'comment_only_py'\nreal_used_py()\n"))
    (let ((data (ice-repo-map-tests--json root)))
      (dolist (case '(("lisp/lib.el" "real-referenced-fn" "lone-referenced-fn")
                      ("web/lib2.ts" "realUsedName" "commentOnlyName")
                      ("tools/lib3.py" "real_used_py" "comment_only_py")))
        (should (> (ice-repo-map-tests--def-rank data (car case) (nth 1 case))
                   (* 5 (ice-repo-map-tests--def-rank data (car case) (nth 2 case)))))))))

(ert-deftest ice-repo-map-feature-forms-carry-no-weight ()
  (ice-repo-map-tests--with-extra
      '(("lisp/feat.el" . "(defun feat-real-fn () nil)\n(provide 'feat)\n")
        ("lisp/featuser.el" . "(require 'feat)\n(defun featuser-go () (featurep 'feat-real-fn))\n"))
    (let ((data (ice-repo-map-tests--json root)))
      (should (< (ice-repo-map-tests--def-rank data "lisp/feat.el" "feat-real-fn") 0.001)))))

(ert-deftest ice-repo-map-long-elisp-summary-is-capped ()
  (ice-repo-map-tests--with-extra
      `(("lisp/long.el" . ,(concat ";;; long.el --- [[wiki link]] " (make-string 400 ?x) " -*- lexical-binding: t; -*-\n\n(defun long-fn () nil)\n")))
    (ice-repo-map-tests--run root "--write")
    (let* ((map (ice-repo-map-tests--slurp (expand-file-name "lat.md/repo-map.md" root)))
           (summary (and (string-match "^## lisp/long\\.el\n\n\\(.*\\)$" map) (match-string 1 map))))
      (should summary)
      (should (<= (length summary) 240))
      (should-not (string-match-p "\\[\\[" summary)))
    (ice-repo-map-tests--lat-check root)))

(ert-deftest ice-repo-map-singular-summary ()
  (ice-repo-map-tests--with-extra '(("lisp/bare.el" . "(defun bare-only-fn () nil)\n"))
    (let ((out (ice-repo-map-tests--run root)))
      (should (string-match-p "^Defines 1 symbol\\.$" out))
      (should-not (string-match-p "1 symbols" out)))))

(ert-deftest ice-repo-map-non-git-root-is-a-clean-error ()
  (let* ((dir (file-name-as-directory (file-truename (make-temp-file "ice-repo-map-plain" t))))
         (file (expand-file-name "plain.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (dolist (root (list dir file))
            (pcase-let ((`(,status ,_ ,err) (ice-repo-map-tests--call root nil)))
              (should (= 2 status))
              (should (string-match-p "not inside a git repository" err))
              (should-not (string-match-p "node:\\|    at " err)))))
      (delete-directory dir t))))

(ert-deftest ice-repo-map-broken-lat-package-is-a-clean-error ()
  (ice-repo-map-tests--with-repo root nil
    (let ((process-environment (cons "ICE_LAT_PKG=/nonexistent/lat.md" process-environment)))
      (pcase-let ((`(,status ,_ ,err) (ice-repo-map-tests--call root nil)))
        (should (= 2 status))
        (should (string-match-p "cannot load the lat.md package" err))
        (should-not (string-match-p "node:\\|    at " err))))))

(ert-deftest ice-repo-map-without-lat-maps-only-elisp ()
  (let ((bin (file-name-as-directory (file-truename (make-temp-file "ice-repo-map-bin" t)))))
    (unwind-protect
        (ice-repo-map-tests--with-repo root nil
          (make-symbolic-link (executable-find "node") (expand-file-name "node" bin))
          (let ((process-environment (append (list "ICE_LAT_PKG" (concat "PATH=" bin ":/usr/bin:/bin"))
                                             process-environment)))
            (pcase-let ((`(,status ,out ,err) (ice-repo-map-tests--call root nil)))
              (should (= 0 status))
              (should (= 1 (length (split-string (string-trim err) "\n"))))
              (should (string-match-p "lat was not found" err))
              (should (string-match-p "core-compute-total" out))
              (should-not (string-match-p "greet\\|tsHelperOpen\\|py_helper_run" out)))))
      (delete-directory bin t))))

(ert-deftest ice-repo-map-dart-typedef-with-parameters ()
  (ice-repo-map-tests--with-extra ice-repo-map-tests--dart-extra
    (let ((defs (ice-repo-map-tests--defs (ice-repo-map-tests--json root) "app/more.dart")))
      (should (equal "typedef" (cdr (assoc "Cmp" defs))))
      (should-not (assoc "int" defs)))))

(ert-deftest ice-repo-map-dart-named-constructors ()
  (ice-repo-map-tests--with-extra ice-repo-map-tests--dart-extra
    (let ((data (ice-repo-map-tests--json root)))
      (dolist (name '("Repo.named" "Repo.fromJson" "Repo.constant"))
        (let ((def (ice-repo-map-tests--def data "app/more.dart" name)))
          (should def)
          (should (equal "constructor" (alist-get 'kind def)))
          (should (equal "Repo" (alist-get 'parent def))))))))

(ert-deftest ice-repo-map-dart-top-level-accessors ()
  (ice-repo-map-tests--with-extra ice-repo-map-tests--dart-extra
    (let ((defs (ice-repo-map-tests--defs (ice-repo-map-tests--json root) "app/more.dart")))
      (should (equal "getter" (cdr (assoc "topGet" defs))))
      (should (equal "setter" (cdr (assoc "topSet" defs)))))))

(ert-deftest ice-repo-map-dart-unnamed-extension-members ()
  (ice-repo-map-tests--with-extra ice-repo-map-tests--dart-extra
    (let ((def (ice-repo-map-tests--def (ice-repo-map-tests--json root) "app/more.dart" "double")))
      (should def)
      (should (equal "function" (alist-get 'kind def)))
      (should (equal "extension on int" (alist-get 'parent def))))))

(ert-deftest ice-repo-map-elisp-setf-method-name ()
  (ice-repo-map-tests--with-extra
      '(("lisp/setf.el" . "(cl-defgeneric thing (x))\n(cl-defmethod (setf thing) (value (x list))\n  value)\n"))
    (let ((defs (ice-repo-map-tests--defs (ice-repo-map-tests--json root) "lisp/setf.el")))
      (should (equal "method" (cdr (assoc "(setf thing)" defs)))))))

(ert-deftest ice-repo-map-elisp-nested-wrappers ()
  (ice-repo-map-tests--with-extra
      '(("lisp/wrap.el" . "(when t\n  (if t\n      (progn (defun wrapped-nested-fn () nil))))\n(with-eval-after-load 'x (defun wrapped-single-fn () nil))\n(defun (unrelated-form))\n"))
    (let ((data (ice-repo-map-tests--json root)))
      (should (equal "(defun wrapped-nested-fn () nil)"
                     (alist-get 'signature (ice-repo-map-tests--def data "lisp/wrap.el" "wrapped-nested-fn"))))
      (should (equal "(defun wrapped-single-fn () nil)"
                     (alist-get 'signature (ice-repo-map-tests--def data "lisp/wrap.el" "wrapped-single-fn")))))))

;;; ice-repo-map-tests.el ends here
