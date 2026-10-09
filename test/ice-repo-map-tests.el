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
                              (should (<= (length out) (* budget 2.5)))
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
                     (how-many "^- \\[\\[repo-map\\]\\] — ranked repo map: key files and symbols, generated by ice-repo-map <!-- ice-repo-map:generated -->$"
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

(ert-deftest ice-repo-map-write-appends-index-line-once-without-trailing-newline ()
  (ice-repo-map-tests--with-repo root nil
    (make-directory (expand-file-name "lat.md" root))
    (with-temp-file (expand-file-name "lat.md/lat.md" root)
      (insert "# Project\n\nThe map."))
    (ice-repo-map-tests--run root "--write")
    (ice-repo-map-tests--run root "--write")
    (with-temp-buffer
      (insert-file-contents (expand-file-name "lat.md/lat.md" root))
      (should (string-prefix-p "# Project\n\nThe map.\n- [[repo-map]]" (buffer-string)))
      (should (= 1 (how-many "<!-- ice-repo-map:generated -->" (point-min) (point-max))))
      (should (string-suffix-p "<!-- ice-repo-map:generated -->\n" (buffer-string))))))

(ert-deftest ice-repo-map-write-leaves-a-present-index-line-alone ()
  (ice-repo-map-tests--with-repo root nil
    (make-directory (expand-file-name "lat.md" root))
    (let ((text "# Project\n\n- [[repo-map]] — mine\n"))
      (with-temp-file (expand-file-name "lat.md/lat.md" root) (insert text))
      (ice-repo-map-tests--run root "--write")
      (should (equal text (ice-repo-map-tests--slurp (expand-file-name "lat.md/lat.md" root)))))))

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
      (should (<= (length default) (* 2000 2.5)))
      (should (<= (length clamped) (* 2000 2.5)))
      (should (equal default explicit))
      (should (equal default clamped)))))

(ert-deftest ice-repo-map-fit-leaves-a-safety-margin ()
  (ice-repo-map-tests--with-repo root t
    (dolist (budget '(800 2000))
      (let ((out (ice-repo-map-tests--run root "--budget" (number-to-string budget))))
        (should (<= (ceiling (/ (length out) 2.5)) (floor (* budget 0.97))))))))

(ert-deftest ice-repo-map-write-is-full ()
  (ice-repo-map-tests--with-repo root t
    (ice-repo-map-tests--run root "--write")
    (let ((full (ice-repo-map-tests--slurp (expand-file-name "lat.md/repo-map.md" root)))
          (condensed (ice-repo-map-tests--run root)))
      (should (= 400 (apply #'+ (mapcar (lambda (c) (ice-repo-map-tests--def-count (nth 2 c))) (ice-repo-map-tests--chunks root "lisp/gen.el")))))
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

(defconst ice-repo-map-tests--chunk-tokens 128)
(defconst ice-repo-map-tests--chars-per-token 2.5)

(defun ice-repo-map-tests--estimate (text)
  (ceiling (/ (length text) ice-repo-map-tests--chars-per-token)))

(defconst ice-repo-map-tests--big-defs
  '(("aob-context-list" . "Show what the context holds.")
    ("aob-mcp-host--write-key" . "Make a secret for this run and leave it where only its owner")
    ("aob-schedule--read-target" . "Ask for a session to schedule into, or an agent and project")
    ("aob-schedule--session-target" . "S as a target: a session, or a persisted conversation's")
    ("aob-shells-stop" . "Stop the command in PAIR: by its agent when it can, else by")
    ("aob-subagent--native-cancel" . "Stop the turn of the agent that runs S; a subagent has no")
    ("aob-todo-open-at-line" . "Open the todo file at the current item's line.")
    ("aob-todo-refresh" . "Refresh the todo view.")
    ("aob-transcript--found-1" . "Read DIRS, which hold")
    ("aob-transcript-asleep-p" . "Whether S is a conversation whose agent is not running.")
    ("r-ts-mode--chain-anchor" . "Anchor continuation lines of an operator chain.")
    ("ygg--float-frame" . "A child frame floating over this frame that takes focus, or")
    ("ygg--macro-record-collect" . "Append this command's keys to an in-progress recording.")
    ("ygg--select-match-step" . "Make the next unselected occurrence of the primary's word")
    ("ygg--term-buffer" . "Return the dedicated terminal buffer if it is alive, else")
    ("ygg-add-newline-above" . "Insert empty lines above each selection (Helix [[ and")
    ("ygg-agent--replace-json" . "Swap TABLE in for PATH, pretty-printed, by rename.")
    ("ygg-aob--embark-agent" . "Tell embark the cursor is on an agent, when it is.")
    ("ygg-aob--show-new-trace" . "Show a new session's trace; a subagent its agent runs opens")
    ("ygg-aob--sidebar-shows-clock-p" . "Whether the sidebar is on screen with a row drawn for S.")
    ("ygg-aob--task-slug" . "The slug of the task S is on, read off the directory that")
    ("ygg-aob-ensure-space" . "The space S works in, made now when S is top-level and its")
    ("ygg-ark--auto-lsp" . "Serve this buffer's LSP from its ark kernel once it has one.")
    ("ygg-ark--comm-open-data" . "Data for a server comm_open: where the kernel should bind.")
    ("ygg-ast-grep--list-run" . "Stream PATTERN in LANG under DIR into a list next-error can")
    ("ygg-call-graph-next" . "Move to the next node.")
    ("ygg-call-graph-refresh" . "Walk the graph again from its root.")
    ("ygg-change" . "Delete every selection after yanking, then insert at the")
    ("ygg-code-build" . "Build the project the way this buffer's language does.")
    ("ygg-code-run" . "Build and run the project the way this buffer's language")
    ("ygg-code-test-file" . "Run the tests in this file.")
    ("ygg-compose-preview--selector" . "CLI selector: --id for one of IDS, a --filter for several,")
    ("ygg-dap-java--own" . "CONFIG carrying RELEASE, run once when the session's")
    ("ygg-dap-js--read-container" . "Pick a running container.")
    ("ygg-dap-kotlin--adapter-p" . "Whether eglot SERVER can open a debug adapter.")
    ("ygg-dap-lldb-program" . "The binary a build of the project at point leaves, relative")
    ("ygg-db--file-url" . "The usql URL of the database file at PATH under SCHEME.")
    ("ygg-db--redis-scan" . "(KEYS . COMPLETE) of CONN matching the glob PATTERN, by SCAN.")
    ("ygg-db--redis-tree" . "Lines drawing VALUE as a numbered, indented tree.")
    ("ygg-debugpy--venv-env" . "ENV with VENV active, so the program's subprocesses stay in")
    ("ygg-debugpy-adapter-python" . "An interpreter that runs the adapter: PYTHON itself if it")
    ("ygg-device-devices" . "Every device the last listings found.")
    ("ygg-device-log-app-output" . "Show the app's own stdout and stderr, which the device log")
    ("ygg-device-log-stderr" . "Show what the log source printed on stderr.")
    ("ygg-device-screenshot-command" . "The program and arguments writing a PNG of DEVICE to FILE.")
    ("ygg-diagram-image-at-point" . "The image file the line at point names, absolute, or nil.")
    ("ygg-diagram-toggle-any-at-point" . "Draw the fence or the image the line at point names, or")
    ("ygg-eglot-x-configure" . "Choose which eglot-x extensions are on, before any server")
    ("ygg-embark--space" . "The space the row at point is about, in the space tree.")
    ("ygg-embr--enable-modal-io" . "Route the read-only-unsafe jk-escape, insert and paste to")
    ("ygg-embr--side-by-side" . "Call LAUNCH with embr's buffer forced into a right split,")
    ("ygg-ex--expand" . "STRING with Helix's command-line expansions replaced by")
    ("ygg-ex--parse-pattern" . "Parse /re/ or ?re? pattern starting at (car POS-CELL) in")
    ("ygg-ex--workspace-directory" . "The project root this buffer sits in, or where it sits.")
    ("ygg-ex-repeat-last" . "Replay the last ex command COUNT times (for @:).")
    ("ygg-extend-to-line-start" . "Extend every selection to the beginning of its line.")
    ("ygg-flip-selections" . "Exchange anchor and cursor of every selection, keeping")
    ("ygg-git-compare--hunk-lines" . "Each diff line of HUNK as (POS SIDE LINE), a removed line on")
    ("ygg-git-compare--redraw" . "Resolve sides A-SPEC and B-SPEC afresh, worktrees as they")
    ("ygg-git-compare--remote-github-states" . "A table of comment id to (RESOLVED . OUTDATED) from the")
    ("ygg-git-compare--remote-requests" . "PR's requests as (KEY PROGRAM . ARGS).")
    ("ygg-git-worktree--compared-in" . "A live compare with DIR as one of its sides, or nil.")
    ("ygg-ice--markdown-setup" . "Wiki links and gd/gr through lat where lat is in use.")
    ("ygg-ice--preset-skills" . "The skills PRESET names, each body in a block the session")
    ("ygg-ice-approve-checkpoints" . "Show CHANGE's checkpoints and, on yes, set their Approved")
    ("ygg-ice-maintain" . "Keep ROOT's verification skill and lat.md feature map honest.")
    ("ygg-ice-root" . "The repository DIR is in, found by its ICE docs or its git")
    ("ygg-jdk--pinned-version" . "The java version the mise CONFIG file pins, or nil.")
    ("ygg-kernel-picker--buffer-language" . "The language code here is in: the chunk's, else the major")
    ("ygg-kernel-picker--flavour" . "The parenthesised tail of a kernelspec DISPLAY-NAME, e.g. uv.")
    ("ygg-kernel-picker--running-row" . "A row for KERNEL, already running on HOST, whose kernelspec")
    ("ygg-kernel-vars--fit-type" . "TYPE in WIDTH columns, shortening the name before the")
    ("ygg-lsp--block-bounds" . "Bounds of the enclosing block ({ } or similar).")
    ("ygg-lsp--inner-node-bounds" . "Body/content of a node, excluding brackets/keywords/headers.")
    ("ygg-lsp--ts-edge" . "Start of the Nth next (N>0) / previous (N<0) node whose type")
    ("ygg-lsp-code-actions" . "Code actions at point or over the region.")
    ("ygg-magit-keys" . "Show every magit key through which-key.")
    ("ygg-match--unmoved" . "The `ygg-each-selection-update' result that leaves")
    ("ygg-modeline-refresh-path" . "Re-read this buffer's file name against its project into the")
    ("ygg-nb--chunk-face" . "Rule the chunk head, dim the tail, and leave the body")
    ("ygg-nb-eval-cell-and-next" . "Evaluate the cell at point and move to the next one.")
    ("ygg-nb-eval-region" . "Evaluate BEG..END in this buffer's kernel, else an inferior")
    ("ygg-nb-history-next" . "Replace the REPL's input with the next history entry.")
    ("ygg-nb-load-file" . "Evaluate a whole file in this buffer's kernel.")
    ("ygg-number-increment-sequential" . "Vim/evil-numbers g C-a: the i-th selection (buffer order)")
    ("ygg-preset--root" . "The checkout the presets are read for: the draft's, else")
    ("ygg-preset-local-p" . "Whether D was shaped by the checkout rather than taken as")
    ("ygg-preset-workers" . "The worker levels D names, as plists of :name and, when")
    ("ygg-project-commands--just-recipes" . "Recipe names read off PATH, for when just itself cannot be")
    ("ygg-project-commands--npm-workspaces" . "Members from DIR's package.json, in either the array or the")
    ("ygg-project-setup--never" . "The projects that declined, with ADD among them when given.")
    ("ygg-projects--due" . "TS, a time to come, said in a few columns.")
    ("ygg-projects--entries" . "The things ROOT's KIND row stands for: (LABEL . PAYLOAD)")
    ("ygg-projects--selected-entries" . "The session rows between mark and point, each once, top")
    ("ygg-projects--worktree-entries" . "ROOT's other worktrees, from the cache only — safe on a")
    ("ygg-projects-toggle-past" . "List ended conversations under the live sessions, or stop")
    ("ygg-qf--filter-keep" . "Put QUERY on the stack of filters and show what they keep")
    ("ygg-qf-from-comint" . "Push the current buffer's error locations into *quickfix*")
    ("ygg-quickscope--backward-targets" . "Reuse the forward scanner on the reversed pre-point text for")
    ("ygg-rass--wrap-guess" . "GUESS, what eglot--guess-contact returns, with its contact")
    ("ygg-rn--bin" . "The project's own NAME binary, installed at ROOT or a")
    ("ygg-rn--pick-target" . "The app runtime Metro's inspector lists, asked for when")
    ("ygg-rn--when-metro-up" . "Call THEN once Metro serves ROOT, checking until DEADLINE.")
    ("ygg-rn-hermes-resolve" . "CONFIG attached to the app runtime Metro's inspector lists.")
    ("ygg-select-next-match" . "Add the next occurrence of the word under the cursor as the")
    ("ygg-skill-index-files" . "Files beside SKILL's SKILL.md, relative to its directory.")
    ("ygg-skill-index-find" . "The skill called NAME in PROJECT, matching the part after a")
    ("ygg-space--spawn" . "Create a new tab as a child of PARENT (id) and tag it; land")
    ("ygg-space--worktree-space" . "Open PATH as a space named for it and ROOT, and pinned to it.")
    ("ygg-space-root" . "DIR's repository root, or DIR itself when it is not inside")
    ("ygg-space-toggle-tab-bar" . "Show or hide the space-tree tab bar for this session.")
    ("ygg-space-tree-parent" . "Move point to the parent of the space at point.")
    ("ygg-swift-insert-mark" . "Open a MARK section comment above the line.")
    ("ygg-swift-split-arguments" . "Put each argument or parameter of the call or declaration on")
    ("ygg-todo--find-item" . "Find item in ITEMS by :id or :text. Return item or nil.")
    ("ygg-treesit-prev-sibling" . "Select the previous named sibling of the node covering the")
    ("ygg-ui--main-window-p" . "Whether WINDOW is a window of the main area a reader may")
    ("ygg-upcase" . "Upcase every selection; count widens on a bare cursor.")
    ("ygg-visidata--exportable-language" . "CLIENT's language, when VisiData can take data frames from")
    ("ygg-visidata-remove-with-buffer" . "Delete DIRECTORY, and everything in it, when BUFFER is")))

(defun ice-repo-map-tests--big-name (n)
  (car (nth (1- n) ice-repo-map-tests--big-defs)))

(defvar ice-repo-map-tests--big-pad "")

(defun ice-repo-map-tests--big-file ()
  (concat ";;; big.el --- Big file -*- lexical-binding: t; -*-\n\n" ice-repo-map-tests--big-pad
          (mapconcat
           (lambda (def)
             (format "(defun %s (x)\n  \"%s\"\n  (core-compute-total x))\n" (car def) (cdr def)))
           ice-repo-map-tests--big-defs "\n")))

(defmacro ice-repo-map-tests--with-big (var &rest body)
  (declare (indent 1))
  `(ice-repo-map-tests--with-extra (list (cons "lisp/big.el" (ice-repo-map-tests--big-file)))
     (let ((,var root))
       (ice-repo-map-tests--run root "--write")
       ,@body)))

(defun ice-repo-map-tests--map (root)
  (ice-repo-map-tests--slurp (expand-file-name "lat.md/repo-map.md" root)))

(defun ice-repo-map-tests--sections (root)
  (mapcar (lambda (section) (split-string section "\n\n"))
          (cdr (split-string (ice-repo-map-tests--map root) "^## \\|\n## "))))

(defun ice-repo-map-tests--chunks (root path)
  (let ((prefix (concat path " · ")))
    (seq-filter (lambda (section) (string-prefix-p prefix (car section)))
                (ice-repo-map-tests--sections root))))

(defun ice-repo-map-tests--section-text (section)
  (concat "## " (string-join section "\n\n")))

(defun ice-repo-map-tests--line-names (lines)
  (cl-loop for l in (split-string lines "\n" t)
           when (string-match "^- `\\(.*?\\)` " l) collect (match-string 1 l)))

(ert-deftest ice-repo-map-big-file-splits-into-token-sized-flat-h2-chunks ()
  (ice-repo-map-tests--with-big root
    (let ((chunks (ice-repo-map-tests--chunks root "lisp/big.el")))
      (should-not (string-match-p "^### " (ice-repo-map-tests--map root)))
      (should (>= (length chunks) 20))
      (dolist (chunk chunks)
        (should (<= (ice-repo-map-tests--estimate (ice-repo-map-tests--section-text chunk))
                    ice-repo-map-tests--chunk-tokens)))
      (should (= 120 (apply #'+ (mapcar (lambda (c) (ice-repo-map-tests--def-count (nth 2 c))) chunks))))
      (should (equal (length chunks) (length (delete-dups (mapcar #'car (copy-sequence chunks)))))))))

(ert-deftest ice-repo-map-chunk-lead-lists-every-def-name ()
  (ice-repo-map-tests--with-big root
    (dolist (chunk (ice-repo-map-tests--chunks root "lisp/big.el"))
      (should (<= (length (nth 1 chunk)) 250))
      (dolist (name (ice-repo-map-tests--line-names (nth 2 chunk)))
        (should (string-match-p (regexp-quote name) (nth 1 chunk)))))))

(defun ice-repo-map-tests--wordpieces (text)
  (let ((case-fold-search nil) (count 0))
    (dolist (word (split-string text "[^[:alnum:]]+" t))
      (cl-incf count (length (split-string word "\\(?:[a-z]\\)\\(?:[A-Z]\\)\\|[0-9]+\\|\\b" t))))
    (+ count (with-temp-buffer (insert text) (how-many "[^[:alnum:][:space:]]" (point-min) (point-max))))))

(ert-deftest ice-repo-map-chunks-stay-within-wordpiece-budget ()
  (ice-repo-map-tests--with-big root
    (dolist (chunk (ice-repo-map-tests--chunks root "lisp/big.el"))
      (should (<= (ice-repo-map-tests--wordpieces (ice-repo-map-tests--section-text chunk)) 200)))))

(defun ice-repo-map-tests--big-map-of (defs pad)
  (let ((ice-repo-map-tests--big-defs defs) (ice-repo-map-tests--big-pad pad))
    (ice-repo-map-tests--with-big root (ice-repo-map-tests--map root))))

(defun ice-repo-map-tests--changed-sections (a b)
  (let ((x (cdr (split-string a "^## \\|\n## "))) (y (cdr (split-string b "^## \\|\n## "))))
    (length (seq-remove (lambda (section) (member section x)) y))))

(ert-deftest ice-repo-map-chunks-are-stable-under-line-shifts-and-edits ()
  (let* ((defs ice-repo-map-tests--big-defs)
         (base (ice-repo-map-tests--big-map-of defs ""))
         (shifted (ice-repo-map-tests--big-map-of defs ";; shifted by a comment\n\n"))
         (added (ice-repo-map-tests--big-map-of
                 (append (seq-take defs 60) '(("zz-inserted-helper" . "A freshly added definition.")) (nthcdr 60 defs)) ""))
         (renamed (ice-repo-map-tests--big-map-of
                   (append (seq-take defs 60) (list (cons "zz-renamed-helper" (cdr (nth 60 defs)))) (nthcdr 61 defs)) "")))
    (should (= 0 (ice-repo-map-tests--changed-sections base shifted)))
    (should (< (ice-repo-map-tests--changed-sections base added) 6))
    (should (< (ice-repo-map-tests--changed-sections base renamed) 6))))

(ert-deftest ice-repo-map-every-heading-has-a-short-leading-paragraph ()
  (ice-repo-map-tests--with-big root
    (let ((map (ice-repo-map-tests--map root)) (headings 0) (pos 0))
      (while (string-match "^\\(#+\\) .*\n\n\\(.*\\)\n" map pos)
        (setq pos (match-end 0))
        (cl-incf headings)
        (should (<= (length (match-string 2 map)) 250))
        (should-not (string-match-p "\\`\\(#\\|- \\)" (match-string 2 map))))
      (should (> headings 20))
      (should (= headings (with-temp-buffer (insert map) (how-many "^#+ " (point-min) (point-max))))))))

(ert-deftest ice-repo-map-file-section-lists-top-defs-within-budget ()
  (ice-repo-map-tests--with-big root
    (let* ((section (cl-find "lisp/big.el" (ice-repo-map-tests--sections root) :key #'car :test #'equal))
           (top (mapcar (lambda (d) (alist-get 'name d))
                        (alist-get 'defs (ice-repo-map-tests--file (ice-repo-map-tests--json root) "lisp/big.el")))))
      (should (<= (ice-repo-map-tests--estimate (ice-repo-map-tests--section-text section))
                  ice-repo-map-tests--chunk-tokens))
      (let ((names (ice-repo-map-tests--line-names (nth 2 section))))
        (should (<= 1 (length names) 10))
        (should (equal names (seq-take top (length names))))))))

(ert-deftest ice-repo-map-chunked-map-passes-lat-check-and-sections-stay-small ()
  (ice-repo-map-tests--with-big root
    (ice-repo-map-tests--lat-check root)
    (let ((default-directory root))
      (with-temp-buffer
        (should (= 0 (call-process "lat" nil (list t nil) nil "section" "repo-map#Repo map#lisp/big.el")))
        (should (<= (string-bytes (buffer-string)) 1600))
        (should (string-match-p "lisp/big\\.el" (buffer-string)))
        (erase-buffer)
        (let ((heading (substring (car (nth 1 (ice-repo-map-tests--chunks root "lisp/big.el"))) (length "lisp/big.el · "))))
          (should (string-match-p "\\`Functions · " heading))
          (should (= 0 (call-process "lat" nil (list t nil) nil "section" (concat "repo-map#Repo map#lisp/big.el · " heading))))
          (should (string-match-p (regexp-quote heading) (buffer-string))))))))

(defun ice-repo-map-tests--search-hit (map name)
  (with-temp-buffer
    (call-process "lat" nil (list t nil) nil "search" name "--limit" "3")
    (let ((lines (split-string map "\n")) (pos 0) hit)
      (while (and (not hit) (string-match "Defined in [^:\n]+:\\([0-9]+\\)-\\([0-9]+\\)" (buffer-string) pos))
        (setq pos (match-end 0))
        (let ((from (string-to-number (match-string 1 (buffer-string))))
              (to (string-to-number (match-string 2 (buffer-string)))))
          (setq hit (string-match-p (format "`%s`" (regexp-quote name))
                                    (string-join (seq-subseq lines (1- from) to) "\n")))))
      (and hit t))))

(ert-deftest ice-repo-map-search-finds-symbol-chunks ()
  (unless (getenv "ICE_REPO_MAP_SEARCH") (ert-skip "set ICE_REPO_MAP_SEARCH=1 to run the lat search hit rate"))
  (ice-repo-map-tests--with-big root
    (unless (executable-find "lat") (ert-skip "lat is not on PATH"))
    (let ((default-directory root)
          (process-environment (append '("LAT_LLM_KEY=" "LAT_LLM_KEY_FILE=" "LAT_LLM_KEY_HELPER=") process-environment))
          (start (float-time)))
      (should (= 0 (call-process "lat" nil nil nil "reindex")))
      (when (> (- (float-time) start) 120) (ert-skip "lat reindex took over 120s"))
      (let* ((map (ice-repo-map-tests--map root))
             (names (mapcar #'ice-repo-map-tests--big-name '(3 17 29 41 58 66 77 90 110 118)))
             (hits (seq-count (lambda (name) (ice-repo-map-tests--search-hit map name)) names)))
        (should (>= hits 7))))))

(defconst ice-repo-map-tests--js-probes
  '("const q = a / b / c; function foo() {}"
    "x = (a + b) / 2; function foo() {} y = (c) / 3"
    "const r = total\n  / count;\nfunction foo(){}"
    "x = y / 2; z = /re\"/; function foo(){ return \"s\" }"
    "const r = /ab+/g; // comment ' quote\nfunction foo(){}"
    "x = /[/]\"/; function foo(){}"
    "/a'b/.test(s)\nfunction foo(){}"
    "function f(){ return /'/.test(s) }\nfunction foo(){}"
    "const a = <div>hi</div>; function foo(){}"
    "const a = <img src='x' />; function foo(){}"
    "const a = <a href=\"/x/y\">t</a>; function foo(){}"
    "const s = `a ${ `b ${ c } }` } d`; function foo(){}"
    "const s = `a ${ '}' } b`; function foo(){}"
    "const s = `a ${ x.replace(/}/g, '') } b`; function foo(){}"
    "const s = `a ${ /* } */ x } b`; function foo(){}"
    "const s = `a ${ {a:1}.a } b`; function foo(){}"
    "const s = `http://x ${a}`; function foo(){}"
    "a = b /c; function foo(){ 'x' }"
    "i++ / 2; function foo(){}"
    "y = a[0] / 2; z = \"it's\"; function foo(){}"
    "const f = s => /x/.test(s); function foo(){}"
    "x = typeof /a/; function foo(){}"
    "if (x) /a'/.test(y); function foo(){}"
    "const w = h / 2 + \"/\" ; function foo(){}"
    "const o = {} / 2; function foo(){}"
    "f = () => /\"/; function foo(){}"
    "a = b\n/ c / d; function foo(){}"
    "const s = `a ${ x.replace(/}/g, `y`) } b`; function foo(){}"
    "const s = `a ${ x /* ` */ } b`; function foo(){}"
    "const s = `a ${ x // }\n} b`; function foo(){}"
    "const s = `a \\` ${x} b`; function foo(){}"
    "const s = `a \\${ b`; function foo(){}"
    "const s = `a ${ f(\"}\") } b`; function foo(){}"
    "const s = tag`a ${b} 'c`; function foo(){}"
    "const s = `1 ${ `2 ${ `3 ${x}` }` }`; function foo(){}"
    "const s = `it's ${x}`; function foo(){}"
    "x.split(/'/); function foo(){}"
    "y = a + /'/.source; function foo(){}"
    "if (a > /'/.test(b)) {} function foo(){}"
    "switch(x){case /'/.test(y): break} function foo(){}"
    "y = !/'/.test(s); function foo(){}"
    "const p = a / b; const t = 'x'; function foo(){}"
    "const p = a / b;\nconst q = c / d; const t = \"it\"; function foo(){}"
    "x = y / 2; z = /re/; function foo(){}"
    "const n = \"abc\".length / 2; const w = 'q'; function foo(){}"
    "const n = `${a}` / 2 / 3; const w = \"q\"; function foo(){}"
    "x = \"a\" / \"b\" ; function foo(){}"
    "x = a - /'/.source; function foo(){}"
    "x = a * /'/.source; function foo(){}"
    "x = a % /'/.source; function foo(){}"
    "x = a ^ /'/.source; function foo(){}"
    "x = ~/'/.source; function foo(){}"
    "x = a < /'/.source; function foo(){}"
    "x = a in /'/; function foo(){}"
    "for (x of /'/.exec(s)) {} function foo(){}"
    "x = void /'/; function foo(){}"
    "delete /'/.x; function foo(){}"
    "x = a instanceof /'/; function foo(){}"
    "throw /'/; function foo(){}"
    "function* g(){ yield /'/; } function foo(){}"
    "async function g(){ await /'/; } function foo(){}"
    "while (x) /'/.test(y); function foo(){}"
    "for (;;) /'/.test(y); function foo(){}"
    "x = (a)/b; y = \"it's\"; function foo(){}"
    "i-- / 2; z = \"it's\"; function foo(){}")
  "JS snippets that end in a sentinel name which stripping must keep.")

(defconst ice-repo-map-tests--ts-consumer
  "const re = /[\"']/.test(s); rxUsedName();\nconst t = `p ${ `q ${ nestedLeakedName() } ` } r`;\nnestedRealName();\nconst d = 4 / 2; const e = a / b / c; divUsedName();\n")

(ert-deftest ice-repo-map-js-regex-literals-and-nested-templates ()
  (ice-repo-map-tests--with-extra
      `(("web/lib.ts" . "export function rxUsedName() {}\nexport function nestedLeakedName() {}\nexport function nestedRealName() {}\nexport function divUsedName() {}\nexport function nobodyUsesThis() {}\n")
        ("web/consumer.ts" . ,ice-repo-map-tests--ts-consumer))
    (let* ((data (ice-repo-map-tests--json root))
           (rank (lambda (name) (ice-repo-map-tests--def-rank data "web/lib.ts" name)))
           (floor (funcall rank "nobodyUsesThis")))
      (dolist (name '("rxUsedName" "nestedRealName" "divUsedName"))
        (should (> (funcall rank name) (* 5 floor))))
      (should (< (funcall rank "nestedLeakedName") (* 2 floor))))))

(ert-deftest ice-repo-map-js-probes-keep-the-code-after-literals-and-comments ()
  (let* ((names (cl-loop for i from 1 to (length ice-repo-map-tests--js-probes) collect (format "probeName%02d" i)))
         (lib (concat (mapconcat (lambda (n) (format "export function %s() {}\n" n)) names "")
                      "export function nobodyUsesThis() {}\n")))
    (ice-repo-map-tests--with-extra
        (cons (cons "web/probes.js" lib)
              (cl-loop for src in ice-repo-map-tests--js-probes
                       for name in names
                       collect (cons (format "web/%s.js" name) (string-replace "foo" name src))))
      (let* ((data (ice-repo-map-tests--json root))
             (floor (ice-repo-map-tests--def-rank data "web/probes.js" "nobodyUsesThis")))
        (should-not (seq-remove (lambda (name) (> (ice-repo-map-tests--def-rank data "web/probes.js" name) (* 5 floor)))
                                names))))))

(ert-deftest ice-repo-map-signature-double-brackets-cannot-form-links ()
  (ice-repo-map-tests--with-extra '(("lisp/br.el" . "(defvar br-matrix '[[1 2] [3 4]])\n"))
    (let ((out (ice-repo-map-tests--run root "--budget" "100000")))
      (should (string-match-p (regexp-quote "'[ [1 2] [3 4] ]") out))
      (should-not (string-match-p "\\[\\[1\\|4\\]\\]" out)))
    (ice-repo-map-tests--run root "--write")
    (ice-repo-map-tests--lat-check root)))

(ert-deftest ice-repo-map-skips-vendored-and-env-dirs-even-when-untracked ()
  (let ((vendored '(".direnv" ".venv" "venv" "site-packages" "node_modules" "vendor" "target" "build" "dist" ".dart_tool" "Pods")))
    (ice-repo-map-tests--with-extra
        (mapcar (lambda (dir) (cons (format "%s/deep/%s.el" dir (replace-regexp-in-string "[^a-z]" "" dir)) "(defun vend-fn-one () 1)\n"))
                vendored)
      (let ((paths (mapcar (lambda (f) (alist-get 'path f)) (alist-get 'files (ice-repo-map-tests--json root)))))
        (should (member "lisp/core.el" paths))
        (should (equal (cl-remove-if-not (lambda (p) (string-match-p "/deep/" p)) paths) nil))))))

(ert-deftest ice-repo-map-write-migrates-the-legacy-index-marker ()
  (ice-repo-map-tests--with-repo root nil
    (make-directory (expand-file-name "lat.md" root))
    (with-temp-file (expand-file-name "lat.md/lat.md" root)
      (insert "# Project\n\n- [[repo-map]] — ranked repo map: key files and symbols, generated by ice-repo-map <!-- GENERATED -->\n"))
    (ice-repo-map-tests--run root "--write")
    (with-temp-buffer
      (insert-file-contents (expand-file-name "lat.md/lat.md" root))
      (should (string-match-p "<!-- ice-repo-map:generated -->" (buffer-string)))
      (should-not (string-match-p "<!-- GENERATED -->" (buffer-string))))))

(ert-deftest ice-repo-map-closed-stdout-pipe-exits-quietly ()
  (ice-repo-map-tests--with-repo root t
    (let ((err (make-temp-file "ice-repo-map-err")))
      (unwind-protect
          (let ((status (call-process "bash" nil nil nil "-c"
                                      (format "set -o pipefail; %s --root %s --json 2>%s | head -c 1 >/dev/null"
                                              (shell-quote-argument ice-repo-map-tests--script)
                                              (shell-quote-argument root) (shell-quote-argument err)))))
            (should (= 0 status))
            (should (equal "" (ice-repo-map-tests--slurp err))))
        (delete-file err)))))

(defconst ice-repo-map-tests--wiki-script
  "const { DatabaseSync } = require('node:sqlite');
const [path, tables, rows] = [process.argv[1], JSON.parse(process.argv[2]), JSON.parse(process.argv[3])];
const db = new DatabaseSync(path);
const repos = JSON.parse(process.argv[4] || '[]');
if (repos.length) { db.exec('CREATE TABLE repositories (id TEXT, local_path TEXT)'); for (const [id, p] of repos) db.prepare('INSERT INTO repositories VALUES (?,?)').run(id, p); }
if (tables.includes('git_metadata')) db.exec('CREATE TABLE git_metadata (file_path TEXT, commit_count_90d INT, commit_count_total INT)');
if (tables.includes('health_file_metrics')) db.exec('CREATE TABLE health_file_metrics (file_path TEXT, score REAL)');
if (tables.includes('dead_code_findings')) db.exec('CREATE TABLE dead_code_findings (file_path TEXT, kind TEXT)');
if (tables.includes('git_metadata')) for (const [f, n] of rows) db.prepare('INSERT INTO git_metadata VALUES (?,?,?)').run(f, n, n);
db.close();")

(defun ice-repo-map-tests--write-wiki (root rows &optional tables repositories)
  (let ((dir (expand-file-name ".repowise" root)))
    (make-directory dir t)
    (should (= 0 (call-process "timeout" nil nil nil "60" "node" "-e" ice-repo-map-tests--wiki-script
                               (expand-file-name "wiki.db" dir)
                               (json-serialize (vconcat (or tables '("git_metadata" "health_file_metrics" "dead_code_findings"))))
                               (json-serialize (vconcat (mapcar #'vconcat rows)))
                               (json-serialize (vconcat (mapcar #'vconcat repositories))))))))

(defun ice-repo-map-tests--rank-of (root path &rest args)
  (alist-get 'rank (ice-repo-map-tests--file (apply #'ice-repo-map-tests--json root args) path)))

(ert-deftest ice-repo-map-repowise-lifts-recently-changed-file ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (ice-repo-map-tests--rank-of root "lisp/leaf.el")))
      (ice-repo-map-tests--write-wiki root '(("lisp/leaf.el" 90) ("lisp/core.el" 1)))
      (should (> (ice-repo-map-tests--rank-of root "lisp/leaf.el") plain))
      (should (= plain (ice-repo-map-tests--rank-of root "lisp/leaf.el" "--no-repowise"))))))

(ert-deftest ice-repo-map-repowise-leaves-focused-ranking-alone ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (ice-repo-map-tests--run root "--json" "--focus" "lisp/alpha.el")))
      (ice-repo-map-tests--write-wiki root '(("lisp/leaf.el" 90) ("lisp/core.el" 1)))
      (should (equal plain (ice-repo-map-tests--run root "--json" "--focus" "lisp/alpha.el"))))))

(ert-deftest ice-repo-map-repowise-lifts-across-ecosystems ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (ice-repo-map-tests--rank-of root "tools/thing.py")))
      (ice-repo-map-tests--write-wiki root '(("tools/thing.py" 60)))
      (should (> (ice-repo-map-tests--rank-of root "tools/thing.py") plain)))))

(ert-deftest ice-repo-map-repowise-ignores-db-from-another-repo ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (ice-repo-map-tests--run root "--json")))
      (ice-repo-map-tests--write-wiki root '(("lisp/leaf.el" 90)) nil '(("r1" "/nonexistent/other/repo")))
      (should (equal plain (ice-repo-map-tests--run root "--json")))
      (delete-file (expand-file-name ".repowise/wiki.db" root))
      (ice-repo-map-tests--write-wiki root '(("lisp/leaf.el" 90)) nil `(("r1" ,(directory-file-name root))))
      (should-not (equal plain (ice-repo-map-tests--run root "--json"))))))

(ert-deftest ice-repo-map-repowise-without-usable-db-matches-no-db ()
  (ice-repo-map-tests--with-repo root nil
    (let ((plain (ice-repo-map-tests--run root "--json"))
          (text (ice-repo-map-tests--run root)))
      (ice-repo-map-tests--write-wiki root '(("lisp/leaf.el" 90)) '("health_file_metrics"))
      (should (equal plain (ice-repo-map-tests--run root "--json")))
      (should (equal text (ice-repo-map-tests--run root)))
      (delete-file (expand-file-name ".repowise/wiki.db" root))
      (with-temp-file (expand-file-name ".repowise/wiki.db" root)
        (insert "this is not a sqlite database, just garbage bytes\n"))
      (should (equal plain (ice-repo-map-tests--run root "--json")))
      (should (equal text (ice-repo-map-tests--run root))))))

;;; ice-repo-map-tests.el ends here
