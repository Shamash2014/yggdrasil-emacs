;;; ice-feature-map-tests.el --- Tests for the feature map tools -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst ice-feature-map-tests--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))

(defconst ice-feature-map-tests--facts
  (expand-file-name "etc/ice/ice-feature-facts" ice-feature-map-tests--root))

(defconst ice-feature-map-tests--summary
  (expand-file-name "etc/ice/ice-feature-summary" ice-feature-map-tests--root))

(defconst ice-feature-map-tests--features
  (expand-file-name "lat.md/features.md" ice-feature-map-tests--root))

(defconst ice-feature-map-tests--fixtures
  '((next
     ("package.json" . "{\"name\":\"shop\",\"scripts\":{\"dev\":\"next dev\"}}")
     ("app/page.tsx" . "export default function Home() { return null }\n")
     ("app/blog/[slug]/page.tsx" . "export default function Post() { return null }\n")
     ("app/api/users/route.ts" . "export async function GET() {}\n")
     ("pages/about.tsx" . "export default function About() { return null }\n")
     ("components/Button.tsx" . "export const Button = () => null\n")
     ("components/Button.test.tsx" . "import { Button } from './Button'\n"))
    (fastapi
     ("pyproject.toml" . "[project]\nname = \"api\"\n\n[project.scripts]\napi-cli = \"app.cli:main\"\n")
     ("app/main.py" . "from fastapi import FastAPI\napp = FastAPI()\n\n@app.get(\"/items\")\ndef items():\n    return []\n\n@router.post(\"/items/{item_id}\")\ndef add(item_id):\n    return item_id\n")
     ("app/cli.py" . "import argparse\nsub = argparse.ArgumentParser().add_subparsers()\nsub.add_parser(\"migrate\")\n")
     ("tests/test_main.py" . "from app.main import items\n"))
    (cobra
     ("go.mod" . "module example.com/tool\n")
     ("main.go" . "package main\n\nimport \"net/http\"\n\nfunc main() {\n\thttp.HandleFunc(\"/health\", health)\n}\n")
     ("cmd/serve.go" . "package cmd\n\nvar serveCmd = &cobra.Command{\n\tUse:   \"serve [port]\",\n\tShort: \"run\",\n}\n")
     ("cmd/serve_test.go" . "package cmd\n"))
    (flutter
     ("pubspec.yaml" . "name: app\n")
     ("lib/main.dart" . "void main() {\n  runApp(App());\n}\n\nfinal router = GoRouter(routes: [\n  GoRoute(path: '/settings', builder: (c, s) => SettingsPage()),\n]);\n")
     ("lib/screens/home_screen.dart" . "class HomeScreen extends StatelessWidget {}\n")
     ("test/home_screen_test.dart" . "void main() {}\n"))
    (clap
     ("Cargo.toml" . "[package]\nname = \"tool\"\nversion = \"0.1.0\"\n")
     ("src/main.rs" . "use clap::Subcommand;\n\n#[derive(Subcommand)]\nenum Cmd {\n    Init,\n    BuildAll { release: bool },\n}\n\nfn main() {}\n")
     ("src/lib.rs" . "pub fn run() {}\n\n#[cfg(test)]\nmod tests {}\n"))
    (node-comments
     ("package.json" . "{\"name\":\"api\",\"dependencies\":{\"express\":\"4\",\"@nestjs/common\":\"10\"}}")
     ("src/app.js" . "const express = require('express');\nconst app = express();\n// app.get('/commented-line', h);\n/* app.post('/commented-block', h); */\nconst doc = \"app.get('/in-string', h)\";\nconst tpl = `app.get('/in-template', h)`;\napp.get('/real', h);\n")
     ("src/cats.controller.ts" . "import { Controller, Get } from '@nestjs/common';\n\n@Controller('cats')\nexport class Cats {\n  // @Get('ghost')\n  @Get('real')\n  list() {}\n}\n"))
    (py-comments
     ("pyproject.toml" . "[project]\nname = \"api\"\ndependencies = [\"fastapi\"]\n")
     ("app/main.py" . "from fastapi import FastAPI\napp = FastAPI()\n# @app.get(\"/commented\")\n\"\"\"\n@app.get(\"/in-docstring\")\n\"\"\"\n@app.get(\"/real\")\ndef real():\n    return 1\n"))
    (py-includes
     ("pyproject.toml" . "[project]\nname = \"api\"\ndependencies = [\"fastapi\"]\n")
     ("app/main.py" . "from fastapi import FastAPI\nfrom app.routes import router\nAPI_PREFIX = \"/api\"\napp = FastAPI()\napp.include_router(router=router, prefix=API_PREFIX)\n")
     ("app/routes.py" . "from fastapi import APIRouter\nfrom app import users\nrouter = APIRouter()\nrouter.include_router(users.get_users_router(), prefix=\"/users\")\n\n@router.api_route(\"/ping\", methods=[\"GET\", \"POST\"])\ndef ping():\n    return 1\n")
     ("app/users.py" . "from fastapi import APIRouter\n\ndef get_users_router():\n    router = APIRouter()\n\n    @router.get(\"/me\")\n    def me():\n        return 1\n\n    return router\n"))
    (go-custom
     ("go.mod" . "module example.com/tool\n")
     ("main.go" . "package main\n\nfunc main() {\n\tr := NewRouter()\n\tr.GET(\"/custom\", h)\n\tr.Get(\"/custom2\", h)\n}\n"))
    (go-stdlib
     ("go.mod" . "module example.com/tool\n")
     ("main.go" . "package main\n\nimport \"net/http\"\n\nfunc main() {\n\thttp.HandleFunc(\"/std\", h)\n\tmux.Get(\"/not-std\", h)\n\t// http.HandleFunc(\"/gone\", h)\n}\n"))
    (go-gin
     ("go.mod" . "module example.com/tool\n\nrequire github.com/gin-gonic/gin v1.9.0\n")
     ("main.go" . "package main\n\nfunc main() {\n\tr := gin.Default()\n\tr.GET(\"/framework\", h)\n}\n"))
    (dart-nested
     ("pubspec.yaml" . "name: app\nflutter:\n  uses-material-design: true\n")
     ("lib/router.dart" . "class Routes {\n  static const String welcome = 'welcome';\n  static const home = '/home';\n}\nconst kLogin = 'login';\nfinal router = GoRouter(routes: [\n  GoRoute(path: '/', builder: (c, s) => Home()),\n  // GoRoute(path: '/commented', builder: x),\n  GoRoute(\n    name: 'auth',\n    path: '/auth',\n    routes: [\n      GoRoute(path: Routes.welcome, builder: (c, s) => W()),\n      GoRoute(path: kLogin, routes: [GoRoute(path: 'otp')]),\n      GoRoute(path: Routes.missing),\n      GoRoute(path: \"$x/dyn\"),\n    ],\n  ),\n  GoRoute(path: Routes.home),\n]);\n"))
    (clap-args
     ("Cargo.toml" . "[package]\nname = \"srv\"\n\n[dependencies]\nclap = \"4\"\n")
     ("src/main.rs" . "use clap::Parser;\n\n#[derive(Parser)]\nstruct Args {\n    /// bind address\n    #[arg(long, default_value = \"0.0.0.0\")]\n    bind: String,\n    #[arg(short, long = \"port-number\", help = \"the long port\")]\n    port: u16,\n    #[arg(short, help = \"a long way\")]\n    quiet: bool,\n    // #[arg(long)]\n    hidden: bool,\n    #[arg(long)]\n    #[arg(env = \"X\")]\n    pub max_conns: Option<u32>,\n}\n\nfn main() {}\n"))
    (vendored
     ("package.json" . "{\"name\":\"app\",\"dependencies\":{\"express\":\"4\"}}")
     ("src/app.js" . "const express = require('express');\nconst app = express();\napp.get('/mine', h);\n")
     ("node_modules/dep/routes.js" . "const express = require('express');\nconst app = express();\napp.get('/vendored-node', h);\n")
     ("vendor/dep/routes.js" . "const express = require('express');\nconst app = express();\napp.get('/vendored-vendor', h);\n")
     (".venv/lib/x.py" . "from fastapi import FastAPI\napp = FastAPI()\n@app.get(\"/vendored-venv\")\ndef x(): pass\n"))
    (shop
     ("package.json" . "{\"name\":\"shop\",\"dependencies\":{\"express\":\"4\"},\"scripts\":{\"start\":\"node src/server.js\"}}")
     ("src/server.js" . "const express = require('express');\nconst app = express();\napp.get('/items', listItems);\napp.post('/items', createItem);\nfunction listItems() {}\nfunction createItem() {}\n")
     ("src/server.test.js" . "const { listItems } = require('./server');\nfunction only_in_test_helper() {}\n")
     ("src/notes.js" . "function noteText() {}\n"))
    (elisp
     ("lisp/foo.el" . "(defvar-keymap foo-map \"x\" #'foo-x)\n(defvar foo-prefix (make-sparse-keymap))\n(define-key foo-prefix (kbd \"C-c f\") foo-map)\n(keymap-set global-map \"C-c g\" #'foo-run)\n(defcustom foo-limit 3 \"How many.\" :type 'integer)\n(defun foo-run () \"Run foo.\" (interactive) nil)\n(defun foo-x () (interactive) nil)\n(transient-define-prefix foo-dispatch ()\n  \"Dispatch foo.\"\n  [[\"Go\" (\"r\" \"run it\" foo-run)]])\n(provide 'foo)\n")
     ("test/foo-tests.el" . "(require 'foo)\n(ert-deftest foo-a () t)\n")
     ("test/bar-tests.el" . "(require 'foo)\n"))))

(defun ice-feature-map-tests--run (&rest args)
  (with-temp-buffer
    (let ((status (apply #'call-process ice-feature-map-tests--facts nil t nil args)))
      (should (eq status 0))
      (buffer-string))))

(defun ice-feature-map-tests--facts-for (root)
  (json-parse-string (ice-feature-map-tests--run "--root" root) :object-type 'hash-table :array-type 'list))

(defun ice-feature-map-tests--fixture (name)
  (let ((root (file-name-as-directory (file-truename (make-temp-file "ice-feature-map" t)))))
    (dolist (file (alist-get name ice-feature-map-tests--fixtures))
      (let ((path (expand-file-name (car file) root)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert (cdr file)))))
    (let ((default-directory root))
      (call-process "git" nil nil nil "init" "-q")
      (call-process "git" nil nil nil "add" "-A"))
    root))

(defun ice-feature-map-tests--entries (facts)
  (let (out)
    (maphash (lambda (_f v) (setq out (append (gethash "entries" v) out))) (gethash "files" facts))
    out))

(defun ice-feature-map-tests--triggers (facts)
  (let (out)
    (maphash (lambda (k _v) (push k out)) (gethash "triggers" facts))
    out))

(defmacro ice-feature-map-tests--with-fixture (name facts &rest body)
  (declare (indent 2))
  `(let* ((root (ice-feature-map-tests--fixture ,name))
          (,facts (ice-feature-map-tests--facts-for root)))
     (unwind-protect (progn ,@body) (delete-directory root t))))

(defun ice-feature-map-tests--tests-of (facts file)
  (gethash "tests" (gethash file (gethash "files" facts))))

(ert-deftest ice-feature-map-next-routes-and-tests ()
  (ice-feature-map-tests--with-fixture 'next facts
    (let ((triggers (ice-feature-map-tests--triggers facts)))
      (dolist (expected '("/" "/blog/[slug]" "/api/users" "/about" "npm run dev"))
        (should (member expected triggers))))
    (should (equal (ice-feature-map-tests--tests-of facts "components/Button.tsx")
                   '("components/Button.test.tsx")))))

(ert-deftest ice-feature-map-fastapi-routes-commands-tests ()
  (ice-feature-map-tests--with-fixture 'fastapi facts
    (let ((triggers (ice-feature-map-tests--triggers facts)))
      (dolist (expected '("GET /items" "POST /items/{item_id}" "api-cli" "migrate"))
        (should (member expected triggers))))
    (should (equal (ice-feature-map-tests--tests-of facts "app/main.py") '("tests/test_main.py")))))

(ert-deftest ice-feature-map-cobra-commands-routes-tests ()
  (ice-feature-map-tests--with-fixture 'cobra facts
    (let ((triggers (ice-feature-map-tests--triggers facts)))
      (dolist (expected '("serve" "/health"))
        (should (member expected triggers)))
      (should (cl-some (lambda (e) (equal (gethash "file" e) "main.go")) (ice-feature-map-tests--entries facts))))
    (should (equal (ice-feature-map-tests--tests-of facts "cmd/serve.go") '("cmd/serve_test.go")))))

(ert-deftest ice-feature-map-flutter-routes-screens-tests ()
  (ice-feature-map-tests--with-fixture 'flutter facts
    (let ((triggers (ice-feature-map-tests--triggers facts)))
      (dolist (expected '("/settings" "HomeScreen" "flutter run"))
        (should (member expected triggers))))
    (should (equal (ice-feature-map-tests--tests-of facts "lib/screens/home_screen.dart")
                   '("test/home_screen_test.dart")))))

(ert-deftest ice-feature-map-clap-subcommands-tests ()
  (ice-feature-map-tests--with-fixture 'clap facts
    (let ((triggers (ice-feature-map-tests--triggers facts)))
      (dolist (expected '("init" "build-all" "tool"))
        (should (member expected triggers))))
    (should (equal (ice-feature-map-tests--tests-of facts "src/lib.rs") '("src/lib.rs")))))

(ert-deftest ice-feature-map-elisp-keys-commands-tests ()
  (ice-feature-map-tests--with-fixture 'elisp facts
    (let ((triggers (ice-feature-map-tests--triggers facts)))
      (dolist (expected '("C-c f x" "C-c g" "M-x foo-run" "M-x customize-variable foo-limit" "M-x foo-dispatch"))
        (should (member expected triggers))))
    (should (equal (ice-feature-map-tests--tests-of facts "lisp/foo.el") '("test/foo-tests.el")))
    (should (equal (gethash "required_by_tests" (gethash "lisp/foo.el" (gethash "files" facts)))
                   '("test/bar-tests.el")))
    (should-not (cl-some (lambda (e) (equal (gethash "kind" e) "key"))
                         (cl-remove-if-not (lambda (e) (string-prefix-p "SPC" (gethash "trigger" e)))
                                           (ice-feature-map-tests--entries facts))))))

(ert-deftest ice-feature-map-entries-have-one-shape ()
  (dolist (name '(next fastapi cobra flutter clap elisp))
    (ice-feature-map-tests--with-fixture name facts
      (let ((entries (ice-feature-map-tests--entries facts)))
        (should entries)
        (dolist (e entries)
          (dolist (field '("kind" "name" "trigger" "file" "line"))
            (should (gethash field e))))))))

(defun ice-feature-map-tests--route-triggers (facts)
  (sort (cl-remove-if-not
         (lambda (trigger) (string-match-p "\\`\\(?:[A-Z]+ \\)?/" trigger))
         (ice-feature-map-tests--triggers facts))
        #'string<))

(ert-deftest ice-feature-map-node-ignores-comments-and-strings ()
  (ice-feature-map-tests--with-fixture 'node-comments facts
    (should (equal (ice-feature-map-tests--route-triggers facts) '("GET /cats/real" "GET /real")))))

(ert-deftest ice-feature-map-python-ignores-comments-and-docstrings ()
  (ice-feature-map-tests--with-fixture 'py-comments facts
    (should (equal (ice-feature-map-tests--route-triggers facts) '("GET /real")))))

(ert-deftest ice-feature-map-fastapi-keyword-and-factory-includes-and-api-route ()
  (ice-feature-map-tests--with-fixture 'py-includes facts
    (should (equal (ice-feature-map-tests--route-triggers facts)
                   '("GET /api/ping" "GET /api/users/me" "POST /api/ping")))))

(ert-deftest ice-feature-map-go-routes-need-a-framework-or-net-http ()
  (ice-feature-map-tests--with-fixture 'go-custom facts
    (should (equal (ice-feature-map-tests--route-triggers facts) nil)))
  (ice-feature-map-tests--with-fixture 'go-stdlib facts
    (should (equal (ice-feature-map-tests--route-triggers facts) '("/std"))))
  (ice-feature-map-tests--with-fixture 'go-gin facts
    (should (equal (ice-feature-map-tests--route-triggers facts) '("/framework")))))

(ert-deftest ice-feature-map-dart-composes-nested-go-routes-and-counts-the-unresolved ()
  (ice-feature-map-tests--with-fixture 'dart-nested facts
    (should (equal (ice-feature-map-tests--route-triggers facts)
                   '("/" "/auth" "/auth/login" "/auth/login/otp" "/auth/welcome" "/home")))
    (should (= 2 (gethash "dart-unresolved-route-paths" (gethash "skipped" facts))))))

(ert-deftest ice-feature-map-clap-derive-long-args-are-options ()
  (ice-feature-map-tests--with-fixture 'clap-args facts
    (let ((options (sort (mapcar (lambda (e) (gethash "trigger" e))
                                 (cl-remove-if-not (lambda (e) (equal (gethash "kind" e) "option"))
                                                   (ice-feature-map-tests--entries facts)))
                         #'string<)))
      (should (equal options '("--bind" "--max-conns" "--port-number"))))))

(ert-deftest ice-feature-map-vendored-directories-are-skipped-even-untracked ()
  (ice-feature-map-tests--with-fixture 'vendored facts
    (should (equal (ice-feature-map-tests--route-triggers facts) '("GET /mine")))
    (let (names)
      (maphash (lambda (k _v) (push k names)) (gethash "files" facts))
      (should (equal names '("src/app.js"))))))

(defconst ice-feature-map-tests--shop-features
  (mapconcat
   #'identity
   '("# Features" ""
     "The shop."
     ""
     "## Items" ""
     "Create and list items." ""
     "### Sub-features" ""
     "Listing and creating." ""
     "- List items." ""
     "### How to get to it" ""
     "HTTP routes." ""
     "- `GET /items` — list items (`/items`)"
     "- `POST /items` — add one (`/items`)" ""
     "### Driving it" ""
     "Read from the routes." ""
     "1. `GET /items` lists them; `npm run start` serves them." ""
     "### Gotchas" ""
     "Few." ""
     "- See `src/server.js` and `src/*.js`." ""
     "### Code" ""
     "The main files." ""
     "- `src/server.js`"
     "- `src/notes.js`" ""
     "Tests:" ""
     "- `src/server.test.js`" "")
   "\n"))

(defun ice-feature-map-tests--shop-problems (&rest edits)
  (let ((text ice-feature-map-tests--shop-features))
    (pcase-dolist (`(,from . ,to) edits)
      (should (string-search from text))
      (setq text (string-replace from to text)))
    (ice-feature-map-tests--with-fixture 'shop facts
      (let ((index (ice-feature-map-tests--index-with-targets root facts (ice-feature-map-tests--defs-for root))))
        (append (ice-feature-map-tests--structure-problems text)
                (ice-feature-map-tests--guard index facts text)
                (ice-feature-map-tests--bullet-problems index text)
                (mapcar (lambda (f) (format "uncovered: %s" f)) (ice-feature-map-tests--coverage-problems facts text)))))))

(ert-deftest ice-feature-map-non-elisp-features-pass-the-generic-guard ()
  (should (equal (ice-feature-map-tests--shop-problems) nil)))

(ert-deftest ice-feature-map-non-elisp-features-fail-the-generic-guard ()
  (should (cl-some (lambda (p) (string-match-p "route: GET /nope" p))
                   (ice-feature-map-tests--shop-problems (cons "`GET /items` lists them" "`GET /nope` lists them"))))
  (should (cl-some (lambda (p) (string-match-p "path: src/missing.js" p))
                   (ice-feature-map-tests--shop-problems (cons "- `src/notes.js`" "- `src/notes.js`\n- `src/missing.js`"))))
  (should (cl-some (lambda (p) (string-match-p "uncovered: src/notes.js" p))
                   (ice-feature-map-tests--shop-problems (cons "- `src/notes.js`\n" ""))))
  (should (cl-some (lambda (p) (string-match-p "uncovered: src/server.test.js" p))
                   (ice-feature-map-tests--shop-problems (cons "Tests:\n\n- `src/server.test.js`\n" ""))))
  (should (equal (ice-feature-map-tests--shop-problems (cons "`npm run start`" "`npm run start`, `C-c Z`")) nil))
  (should (ice-feature-map-tests--shop-problems (cons "(`/items`)\n- `POST" "(`/nope`)\n- `POST")))
  (should (ice-feature-map-tests--shop-problems (cons "### Gotchas" "### Pitfalls"))))

(ert-deftest ice-feature-map-defs-only-in-a-test-file-need-the-test-listed ()
  (skip-unless (executable-find "lat"))
  (let ((edit (cons "Few." "Few; `only_in_test_helper` is a helper.")))
    (should (equal (ice-feature-map-tests--shop-problems edit) nil))
    (should (cl-some (lambda (p) (string-match-p "symbol: only_in_test_helper" p))
                     (ice-feature-map-tests--shop-problems edit (cons "Tests:\n\n- `src/server.test.js`\n" ""))))))

(defvar ice-feature-map-tests--repo-facts nil)

(defun ice-feature-map-tests--repo-facts ()
  (or ice-feature-map-tests--repo-facts
      (setq ice-feature-map-tests--repo-facts
            (ice-feature-map-tests--facts-for ice-feature-map-tests--root))))

(defun ice-feature-map-tests--trigger-targets (facts trigger)
  (mapcar (lambda (e) (gethash "name" e)) (gethash trigger (gethash "triggers" facts))))

(ert-deftest ice-feature-map-repo-known-keys ()
  (let ((facts (ice-feature-map-tests--repo-facts)))
    (should (member "ygg-space-pick" (ice-feature-map-tests--trigger-targets facts "SPC p z")))
    (should (member "ygg-space-pick-everything" (ice-feature-map-tests--trigger-targets facts "SPC p Z")))
    (should (member "ygg-space-toggle-tab-bar" (ice-feature-map-tests--trigger-targets facts "SPC u T")))
    (should (member "ygg-git-compare-review-branch" (ice-feature-map-tests--trigger-targets facts "= r")))
    (should (member "elisp:yggdrasil" (gethash "ecosystems" facts)))))

(ert-deftest ice-feature-map-repo-transients-and-tests ()
  (let* ((facts (ice-feature-map-tests--repo-facts))
         (file (gethash "lisp/ygg-git-compare-comments.el" (gethash "files" facts)))
         (prefix (cl-find "ygg-git-compare-dispatch" (gethash "entries" file)
                          :key (lambda (e) (gethash "name" e)) :test #'equal)))
    (should (equal (gethash "kind" prefix) "transient"))
    (should (cl-some (lambda (s) (equal (gethash "key" s) "&")) (gethash "suffixes" prefix)))
    (should (member "test/ygg-git-compare-comments-tests.el" (gethash "tests" file)))))

(ert-deftest ice-feature-map-facts-deterministic ()
  (should (equal (ice-feature-map-tests--run "--clusters") (ice-feature-map-tests--run "--clusters"))))

(defun ice-feature-map-tests--features-text ()
  (with-temp-buffer
    (insert-file-contents ice-feature-map-tests--features)
    (buffer-string)))

(defun ice-feature-map-tests--lead (lines)
  (let (acc started)
    (catch 'done
      (dolist (l lines)
        (if (string-empty-p (string-trim l))
            (when started (throw 'done nil))
          (setq started t)
          (push (string-trim l) acc))))
    (mapconcat #'identity (nreverse acc) " ")))

(defun ice-feature-map-tests--parse (text)
  "Headings of TEXT as plists (:level :title :lead :body)."
  (let (out title level lines in-fence)
    (cl-flet ((flush ()
                (when title
                  (let ((body (nreverse lines)))
                    (push (list :level level :title title
                                :lead (ice-feature-map-tests--lead body)
                                :body (mapconcat #'identity body "\n"))
                          out)))
                (setq lines nil)))
      (dolist (line (split-string text "\n"))
        (when (string-prefix-p "```" line) (setq in-fence (not in-fence)))
        (if (and (not in-fence) (string-match "\\`\\(#+\\) \\(.+\\)\\'" line))
            (progn (flush)
                   (setq level (length (match-string 1 line)) title (match-string 2 line)))
          (push line lines)))
      (flush))
    (nreverse out)))

(defconst ice-feature-map-tests--required-h3
  '("Sub-features" "How to get to it" "Driving it" "Gotchas" "Code"))

(defun ice-feature-map-tests--h3s-by-h2 (headings)
  "Alist of (H2-TITLE . H3-TITLES) over HEADINGS."
  (let (out)
    (dolist (h headings)
      (pcase (plist-get h :level)
        (2 (push (list (plist-get h :title)) out))
        (3 (when out (push (plist-get h :title) (cdr (car out)))))))
    (nreverse out)))

(defun ice-feature-map-tests--structure-problems (text)
  (let* ((headings (ice-feature-map-tests--parse text))
         (groups (ice-feature-map-tests--h3s-by-h2 headings))
         problems)
    (unless groups (push "no features" problems))
    (dolist (h headings)
      (unless (string-match-p "[^ ]" (plist-get h :lead))
        (push (format "no lead: %s" (plist-get h :title)) problems))
      (when (> (length (plist-get h :lead)) 250)
        (push (format "lead too long: %s" (plist-get h :title)) problems)))
    (dolist (group groups)
      (dolist (required ice-feature-map-tests--required-h3)
        (unless (member required (cdr group))
          (push (format "%s lacks %s" (car group) required) problems))))
    (nreverse problems)))

(ert-deftest ice-feature-map-features-structure ()
  (should (equal (ice-feature-map-tests--structure-problems (ice-feature-map-tests--features-text)) nil)))

(defun ice-feature-map-tests--code-spans (text)
  (let (out (start 0))
    (while (string-match "`\\([^`\n]+\\)`" text start)
      (push (match-string 1 text) out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun ice-feature-map-tests--collapse (string)
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " string)))

(defun ice-feature-map-tests--regexp (pattern)
  "Emacs regexp for PATTERN, a JS-syntax regex limited to classes, groups, alternation and anchors."
  (let ((i 0) (n (length pattern)) (out ""))
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((eq c ?\[)
          (let ((j (1+ i)))
            (when (eq (aref pattern j) ?^) (setq j (1+ j)))
            (while (not (eq (aref pattern j) ?\])) (setq j (1+ j)))
            (setq out (concat out (substring pattern i (1+ j))) i (1+ j))))
         ((eq c ?\\)
          (let ((d (aref pattern (1+ i))))
            (setq out (concat out (if (memq d '(?\\ ?\[ ?\] ?. ?* ?? ?+ ?^ ?$)) (string ?\\ d) (string d)))
                  i (+ i 2))))
         ((and (eq c ?\() (string-prefix-p "(?:" (substring pattern i)))
          (setq out (concat out "\\(?:") i (+ i 3)))
         ((eq c ?\() (setq out (concat out "\\(") i (1+ i)))
         ((eq c ?\)) (setq out (concat out "\\)") i (1+ i)))
         ((eq c ?|) (setq out (concat out "\\|") i (1+ i)))
         (t (setq out (concat out (string c)) i (1+ i))))))
    out))

(defun ice-feature-map-tests--notation (facts kind)
  (let (out)
    (dolist (item (gethash "notation" facts))
      (when (equal (gethash "kind" item) kind)
        (push (ice-feature-map-tests--regexp (gethash "pattern" item)) out)))
    (nreverse out)))

(defun ice-feature-map-tests--notation-p (facts kind span)
  (let ((case-fold-search nil))
    (cl-some (lambda (re) (string-match-p re span)) (ice-feature-map-tests--notation facts kind))))

(defun ice-feature-map-tests--git-lines (root &rest args)
  (let ((default-directory (file-name-as-directory root)))
    (with-temp-buffer
      (apply #'call-process "git" nil '(t nil) nil args)
      (split-string (buffer-string) "\n" t))))

(defun ice-feature-map-tests--repo-files (root)
  (ice-feature-map-tests--git-lines root "ls-files" "--cached" "--others" "--exclude-standard"))

(defun ice-feature-map-tests--fact-entries (facts)
  (let (out)
    (maphash (lambda (_f v) (setq out (append (gethash "entries" v) out))) (gethash "files" facts))
    out))

(defun ice-feature-map-tests--token-prefixes (trigger)
  (let ((parts (split-string trigger " " t)) acc out)
    (dolist (part (butlast parts))
      (setq acc (if acc (concat acc " " part) part))
      (push acc out))
    out))

(defun ice-feature-map-tests--build-index (root facts defs)
  (let* ((files (ice-feature-map-tests--repo-files root))
         (tests (gethash "tests" facts))
         (entries (ice-feature-map-tests--fact-entries facts))
         (triggers (make-hash-table :test #'equal))
         (prefixes (make-hash-table :test #'equal))
         (names (make-hash-table :test #'equal))
         (stems (make-hash-table :test #'equal))
         (exts (make-hash-table :test #'equal))
         (namespaces (make-hash-table :test #'equal))
         (counts (make-hash-table :test #'equal))
         (corpus (make-hash-table :test #'equal)))
    (dolist (e entries)
      (let ((trigger (ice-feature-map-tests--collapse (gethash "trigger" e))))
        (puthash trigger t triggers)
        (puthash (gethash "name" e) t names)
        (dolist (prefix (ice-feature-map-tests--token-prefixes trigger)) (puthash prefix t prefixes))))
    (maphash (lambda (name list)
               (when (cl-some (lambda (f) (not (member f tests))) list)
                 (puthash name t counts)))
             defs)
    (dolist (file files)
      (let ((base (file-name-nondirectory file)))
        (puthash base t stems)
        (puthash (file-name-sans-extension base) t stems)
        (when (file-name-extension base) (puthash (concat "." (file-name-extension base)) t exts))
        (dolist (dir (split-string (or (file-name-directory file) "") "/" t)) (puthash dir t stems))))
    (let ((tally (make-hash-table :test #'equal)))
      (dolist (name (append (hash-table-keys counts) (hash-table-keys names)))
        (when (string-match "\\`\\([A-Za-z0-9]+\\)[-_]" name)
          (puthash (match-string 1 name) (1+ (gethash (match-string 1 name) tally 0)) tally)))
      (maphash (lambda (k v) (when (and (>= v 5) (>= v (* 0.02 (hash-table-count counts)))) (puthash k t namespaces))) tally))
    (dolist (line (apply #'ice-feature-map-tests--git-lines
                         root "grep" "-I" "-o" "-h" "-E" "--untracked" "[A-Za-z_][A-Za-z0-9_]*([-_]+[A-Za-z0-9_]+)+" "--"
                         "." ":(exclude)lat.md" (mapcar (lambda (test) (concat ":(exclude)" test)) tests)))
      (puthash line t corpus))
    (list :root root :files files :file-set (let ((h (make-hash-table :test #'equal))) (dolist (f files) (puthash f t h)) h)
          :tests tests :triggers triggers :prefixes prefixes :names names :stems stems :exts exts
          :namespaces namespaces :defs defs :corpus corpus)))

(defun ice-feature-map-tests--key-known-p (index span)
  (let ((norm (ice-feature-map-tests--collapse span)))
    (or (gethash norm (plist-get index :triggers))
        (gethash norm (plist-get index :prefixes)))))

(defun ice-feature-map-tests--route-known-p (index facts span)
  (let ((norm (ice-feature-map-tests--collapse span)) hit)
    (ignore facts)
    (maphash (lambda (trigger _v)
               (when (or (equal trigger norm) (string-suffix-p (concat " " norm) trigger)) (setq hit t)))
             (plist-get index :triggers))
    (or hit (gethash norm (plist-get index :names)))))

(defun ice-feature-map-tests--option-known-p (index span)
  (let (hit)
    (maphash (lambda (trigger _v)
               (when (or (equal trigger span) (string-suffix-p (concat " " span) trigger)) (setq hit t)))
             (plist-get index :triggers))
    hit))

(defun ice-feature-map-tests--glob-regexp (path)
  (let ((re (regexp-quote path)))
    (setq re (replace-regexp-in-string "\\\\\\*\\\\\\*" ".*" re))
    (setq re (replace-regexp-in-string "\\\\\\*" "[^/]*" re))
    (setq re (replace-regexp-in-string "\\\\\\?" "[^/]" re))
    (setq re (replace-regexp-in-string "<[^<>/]*>\\|{[^{}/]*}\\|\\$[A-Za-z_]+\\|\\$[{][^}]*[}]" "[^/]+" re))
    (concat "\\(?:\\`\\|/\\)" re "\\'")))

(defun ice-feature-map-tests--path-exists-p (index path)
  (let* ((root (plist-get index :root))
         (clean (replace-regexp-in-string "\\`\\./" "" (replace-regexp-in-string "/+\\'" "" path))))
    (or (gethash clean (plist-get index :file-set))
        (file-exists-p (expand-file-name clean root))
        (let ((default-directory (file-name-as-directory root)))
          (eq 0 (call-process "git" nil nil nil "check-ignore" "-q" "--" clean)))
        (let ((re (ice-feature-map-tests--glob-regexp clean)) hit)
          (dolist (file (plist-get index :files))
            (when (and (not hit) (or (string-match-p re file)
                                     (string-match-p (concat "\\(?:\\`\\|/\\)" (regexp-quote clean) "/") file)))
              (setq hit t)))
          hit))))

(defun ice-feature-map-tests--path-like-p (index span)
  (and (not (string-match-p "[ \t]" span))
       (not (string-match-p "\\`[~-]" span))
       (or (string-search "/" span)
           (let ((ext (file-name-extension span t)))
             (and ext (gethash ext (plist-get index :exts)))))))

(defun ice-feature-map-tests--strip-location (span)
  (replace-regexp-in-string "\\(?::[0-9]+\\(?:-[0-9]+\\)?\\|#[^/]*\\)\\'" "" span))

(defconst ice-feature-map-tests--symbol-re
  "\\`\\(?:M-x \\)?\\([A-Za-z_][A-Za-z0-9_]*\\(?:[-_]+[A-Za-z0-9_*]+\\)+\\|[A-Za-z_][A-Za-z0-9_]*\\(?:::[A-Za-z_][A-Za-z0-9_]*\\)+\\)\\'")

(defun ice-feature-map-tests--defined-p (index symbol allowed-tests)
  (let ((tests (plist-get index :tests)))
    (or (gethash symbol (plist-get index :names))
        (gethash symbol (plist-get index :stems))
        (let ((files (gethash symbol (plist-get index :defs))))
          (cl-some (lambda (f) (or (not (member f tests)) (member f allowed-tests))) files)))))

(defun ice-feature-map-tests--symbol-matches-p (index symbol allowed-tests)
  (if (string-search "*" symbol)
      (let ((re (concat "\\`" (replace-regexp-in-string "\\\\\\*" ".*" (regexp-quote symbol)) "\\'")) hit)
        (dolist (table (list (plist-get index :names) (plist-get index :defs) (plist-get index :stems)))
          (maphash (lambda (k _v) (when (and (not hit) (string-match-p re k)) (setq hit t))) table))
        hit)
    (ice-feature-map-tests--defined-p index symbol allowed-tests)))

(defun ice-feature-map-tests--namespaced-p (index symbol)
  (and (string-match "\\`\\([A-Za-z0-9]+\\)[-_]" symbol)
       (gethash (match-string 1 symbol) (plist-get index :namespaces))))

(defun ice-feature-map-tests--span-problem (index facts allowed-tests span)
  (let* ((norm (ice-feature-map-tests--collapse span))
         (case-fold-search nil))
    (cond
     ((string-search "://" norm) nil)
     ((ice-feature-map-tests--notation-p facts "key" norm)
      (unless (ice-feature-map-tests--key-known-p index norm) "key"))
     ((ice-feature-map-tests--notation-p facts "option" norm)
      (unless (ice-feature-map-tests--option-known-p index norm) "option"))
     ((and (string-prefix-p "/" norm) (not (string-match-p "[ \t]" norm))
           (ice-feature-map-tests--path-exists-p index (substring norm 1)))
      nil)
     ((ice-feature-map-tests--notation-p facts "route" norm)
      (unless (or (ice-feature-map-tests--route-known-p index facts norm)
                  (and (ice-feature-map-tests--path-like-p index norm)
                       (ice-feature-map-tests--path-exists-p index (ice-feature-map-tests--strip-location norm))))
        "route"))
     ((ice-feature-map-tests--path-like-p index norm)
      (unless (ice-feature-map-tests--path-exists-p index (ice-feature-map-tests--strip-location norm)) "path"))
     ((string-match ice-feature-map-tests--symbol-re norm)
      (let ((symbol (match-string 1 norm)))
        (unless (or (ice-feature-map-tests--symbol-matches-p index symbol allowed-tests)
                    (and (not (ice-feature-map-tests--namespaced-p index symbol))
                         (gethash symbol (plist-get index :corpus))))
          "symbol"))))))

(defun ice-feature-map-tests--without-spans (text)
  (replace-regexp-in-string "`[^`\n]+`" (lambda (m) (make-string (length m) ?\s)) text t t))

(defun ice-feature-map-tests--bare-symbol-problems (index allowed-tests text)
  (let ((text (replace-regexp-in-string "`[^`\n]+`" (lambda (m) (if (string-match-p "[/.]" m) " " m)) text t t))
        (case-fold-search nil) (start 0) out)
    (while (string-match "\\(?:\\`\\|[^[:alnum:]_/.-]\\)\\([A-Za-z_][A-Za-z0-9_]*\\(?:[-_]+[A-Za-z0-9_*]+\\)+\\)" text start)
      (let ((symbol (match-string 1 text)) (end (match-end 1)))
        (setq start end)
        (unless (or (string-match-p "\\`\\.[[:alnum:]]" (substring text end (min (length text) (+ end 2))))
                    (not (ice-feature-map-tests--namespaced-p index symbol))
                    (ice-feature-map-tests--symbol-matches-p index symbol allowed-tests))
          (push symbol out))))
    (delete-dups (nreverse out))))

(defun ice-feature-map-tests--prose-key-problems (index facts text)
  (let ((prose (ice-feature-map-tests--without-spans text))
        (case-fold-search nil) out)
    (dolist (re (ice-feature-map-tests--notation facts "key-prose"))
      (let ((start 0))
        (while (string-match re prose start)
          (let* ((token (match-string 1 prose)) (end (match-end 1)))
            (setq start (max end (1+ start)))
            (unless (string-match-p "\\`[[:alnum:]_-]" (substring prose end (min (length prose) (1+ end))))
              (dotimes (_ 3)
                (cond
                 ((eq (string-match " \\([^ a,;:.()]\\)\\(?:[ ,;:.)\n]\\|\\'\\)" prose end) end)
                  (setq token (concat token " " (match-string 1 prose)) end (match-end 1)))
                 ((eq (string-match re prose end) end)
                  (setq token (concat token " " (match-string 1 prose)) end (match-end 1)))))
              (unless (ice-feature-map-tests--key-known-p index token) (push token out)))))))
    (delete-dups (nreverse out))))

(defun ice-feature-map-tests--sections (text)
  "Alist of (FEATURE . BODY) over the h2 sections of TEXT, with \"\" for what precedes the first."
  (let ((out (list (list ""))) )
    (dolist (h (ice-feature-map-tests--parse text))
      (when (= (plist-get h :level) 2) (push (list (plist-get h :title)) out))
      (setcdr (car out) (cons (plist-get h :body) (cdr (car out)))))
    (mapcar (lambda (s) (cons (car s) (mapconcat #'identity (reverse (cdr s)) "\n"))) (nreverse out))))

(defun ice-feature-map-tests--section-tests (text feature)
  (let ((section (assoc feature (ice-feature-map-tests--code-sections text))))
    (append (plist-get (cdr section) :files) (plist-get (cdr section) :tests))))

(defun ice-feature-map-tests--guard (index facts text)
  "Problems of TEXT against INDEX and FACTS as strings \"FEATURE: KIND: SPAN\"."
  (let (problems)
    (dolist (section (ice-feature-map-tests--sections text))
      (let ((allowed (ice-feature-map-tests--section-tests text (car section))))
        (dolist (span (ice-feature-map-tests--code-spans (cdr section)))
          (let ((kind (ice-feature-map-tests--span-problem index facts allowed span)))
            (when kind (push (format "%s: %s: %s" (car section) kind span) problems))))
        (dolist (symbol (ice-feature-map-tests--bare-symbol-problems index allowed (cdr section)))
          (push (format "%s: symbol: %s" (car section) symbol) problems))
        (dolist (key (ice-feature-map-tests--prose-key-problems index facts (cdr section)))
          (push (format "%s: key: %s" (car section) key) problems))))
    (delete-dups (nreverse problems))))

(defun ice-feature-map-tests--bullet-problems (index text)
  "Key bullets of the How to get to it h3s whose key or target (`NAME`) the facts do not have."
  (let (problems)
    (dolist (h (ice-feature-map-tests--parse text))
      (when (equal (plist-get h :title) "How to get to it")
        (dolist (line (split-string (plist-get h :body) "\n"))
          (when (string-match "\\`[-*] `\\([^`]+\\)` — .*(`\\([^`]+\\)`)\\'" line)
            (let ((trigger (ice-feature-map-tests--collapse (match-string 1 line))) (target (match-string 2 line)))
              (unless (and (gethash trigger (plist-get index :triggers))
                           (member target (gethash trigger (plist-get index :target-map))))
                (push line problems)))))))
    (nreverse problems)))

(defun ice-feature-map-tests--facts-targets (facts)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (e (ice-feature-map-tests--fact-entries facts))
      (let ((trigger (ice-feature-map-tests--collapse (gethash "trigger" e))))
        (puthash trigger (cons (gethash "name" e) (gethash trigger table)) table)))
    table))

(defun ice-feature-map-tests--index-with-targets (root facts defs)
  (append (ice-feature-map-tests--build-index root facts defs)
          (list :target-map (ice-feature-map-tests--facts-targets facts))))

(defun ice-feature-map-tests--defs-for (root)
  (let ((names (make-hash-table :test #'equal))
        (map (with-temp-buffer
               (should (eq 0 (call-process (expand-file-name "etc/ice/ice-repo-map" ice-feature-map-tests--root)
                                           nil '(t nil) nil "--root" root "--json" "--budget" "1000000")))
               (goto-char (point-min))
               (json-parse-buffer :object-type 'hash-table :array-type 'list))))
    (dolist (file (gethash "files" map))
      (dolist (def (gethash "defs" file))
        (puthash (gethash "name" def) (cons (gethash "path" file) (gethash (gethash "name" def) names)) names)))
    names))

(defvar ice-feature-map-tests--repo-index nil)

(defun ice-feature-map-tests--repo-index ()
  (or ice-feature-map-tests--repo-index
      (setq ice-feature-map-tests--repo-index
            (ice-feature-map-tests--index-with-targets
             ice-feature-map-tests--root (ice-feature-map-tests--repo-facts)
             (ice-feature-map-tests--defs-for ice-feature-map-tests--root)))))

(defun ice-feature-map-tests--features-copy-with (&rest edits)
  (let ((text (ice-feature-map-tests--features-text)))
    (pcase-dolist (`(,from . ,to) edits)
      (should (string-search from text))
      (setq text (string-replace from to text)))
    text))

(defun ice-feature-map-tests--code-sections (text)
  "Alist of (FEATURE . (:files FILES :tests TESTS)) from the Code h3s of TEXT."
  (let (out feature in-code in-tests in-fence)
    (dolist (line (split-string text "\n"))
      (when (string-prefix-p "```" line) (setq in-fence (not in-fence)))
      (cond
       (in-fence)
       ((string-match "\\`## \\(.+\\)" line)
        (setq feature (match-string 1 line) in-code nil in-tests nil)
        (push (list feature :files nil :tests nil) out))
       ((string-prefix-p "### " line)
        (setq in-code (equal line "### Code") in-tests nil))
       ((and in-code (equal line "Tests:")) (setq in-tests t))
       ((and in-code feature (string-match "\\`- `\\([^`]+\\)`\\'" line))
        (let ((entry (car out)))
          (plist-put (cdr entry) (if in-tests :tests :files)
                     (append (plist-get (cdr entry) (if in-tests :tests :files))
                             (list (match-string 1 line))))))))
    (nreverse out)))

(defun ice-feature-map-tests--listed-files (text)
  (let (out)
    (dolist (section (ice-feature-map-tests--code-sections text))
      (setq out (append (plist-get (cdr section) :files) (plist-get (cdr section) :tests) out)))
    out))

(defun ice-feature-map-tests--coverage-problems (facts text)
  "Source, script and test files of FACTS that no Code h3 of TEXT lists."
  (let ((listed (ice-feature-map-tests--listed-files text)) names)
    (maphash (lambda (k _v) (push k names)) (gethash "files" facts))
    (cl-remove-if (lambda (f) (member f listed)) (append names (gethash "tests" facts)))))

(defun ice-feature-map-tests--mapped-test-problems (facts text)
  (let ((files (gethash "files" facts)) missing)
    (dolist (section (ice-feature-map-tests--code-sections text))
      (let ((listed (append (plist-get (cdr section) :files) (plist-get (cdr section) :tests))))
        (dolist (file (plist-get (cdr section) :files))
          (let ((entry (gethash file files)))
            (when entry
              (dolist (test (gethash "tests" entry))
                (unless (member test listed) (push (cons (car section) test) missing))))))))
    missing))

(ert-deftest ice-feature-map-every-key-exists-in-facts ()
  (let* ((index (ice-feature-map-tests--repo-index))
         (text (ice-feature-map-tests--features-text)))
    (should (> (length (cl-remove-if-not (lambda (l) (string-match-p "\\`[-*] `[^`]+` — .*(`[^`]+`)\\'" l))
                                         (split-string text "\n")))
               100))
    (should (equal (ice-feature-map-tests--bullet-problems index text) nil))))

(ert-deftest ice-feature-map-every-span-exists-in-facts ()
  (should (equal (ice-feature-map-tests--guard (ice-feature-map-tests--repo-index)
                                               (ice-feature-map-tests--repo-facts)
                                               (ice-feature-map-tests--features-text))
                 nil)))

(ert-deftest ice-feature-map-notation-comes-from-facts ()
  (let ((facts (ice-feature-map-tests--repo-facts)))
    (should (ice-feature-map-tests--notation-p facts "key" "C-c f"))
    (should (ice-feature-map-tests--notation-p facts "key" "SPC p z"))
    (should (ice-feature-map-tests--notation-p facts "route" "GET /x"))
    (should-not (ice-feature-map-tests--notation-p facts "key" "lisp/foo.el")))
  (ice-feature-map-tests--with-fixture 'fastapi facts
    (should-not (ice-feature-map-tests--notation facts "key"))
    (should (ice-feature-map-tests--notation-p facts "route" "POST /items"))))

(ert-deftest ice-feature-map-guard-catches-injected-fakes ()
  (let* ((index (ice-feature-map-tests--repo-index))
         (facts (ice-feature-map-tests--repo-facts))
         (anchor "1. In a file buffer press `x`")
         (guard (lambda (line)
                  (ice-feature-map-tests--guard
                   index facts (ice-feature-map-tests--features-copy-with
                                (cons anchor (concat line "\n" anchor)))))))
    (dolist (case '(("0. Press `C-c Z` to fly." "key" "C-c Z")
                    ("0. In compare press `= Q`." "key" "= Q")
                    ("0. Press C-c Z to fly." "key" "C-c Z")
                    ("0. Run `M-x ygg-fake-nope` first (`ygg-fake-nope`)." "symbol" "ygg-fake-nope")
                    ("0. Run `ice-fake-sym` first." "symbol" "ice-fake-sym")
                    ("0. Then aob-fake-sym runs." "symbol" "aob-fake-sym")
                    ("0. See `etc/ice/ice-nonexistent`." "path" "etc/ice/ice-nonexistent")
                    ("0. See `lisp/not-a-file.el`." "path" "lisp/not-a-file.el")
                    ("0. See (`etc/ice/ice-bogus`) for details." "path" "etc/ice/ice-bogus")
                    ("0. `GET /fake` returns nothing." "route" "GET /fake")
                    ))
      (let ((found (funcall guard (car case))))
        (should (cl-some (lambda (p) (string-match-p (concat ": " (cadr case) ": " (regexp-quote (nth 2 case)) "\\'") p)) found))))
    (should (equal (funcall guard "0. Real: `m  s` works, press `x`.") nil))
    (should (ice-feature-map-tests--structure-problems
             (ice-feature-map-tests--features-copy-with
              (cons "### Code\n\nThe main files, then the test files that map to them by name.\n\n- `lisp/yggdrasil.el`"
                    "### Source\n\nThe main files.\n\n- `lisp/yggdrasil.el`"))))
    (should (equal (ice-feature-map-tests--structure-problems (ice-feature-map-tests--features-text)) nil))))

(ert-deftest ice-feature-map-guard-bullets-catch-wrong-key-and-target ()
  (let ((index (ice-feature-map-tests--repo-index)))
    (should (ice-feature-map-tests--bullet-problems
             index (ice-feature-map-tests--features-copy-with
                    (cons "- `x` — select the line, repeat to extend (`ygg-select-line`)"
                          "- `x` — select the line, repeat to extend (`ygg-delete-dwim`)"))))
    (should (ice-feature-map-tests--bullet-problems
             index (ice-feature-map-tests--features-copy-with
                    (cons "- `x` — select the line" "- `x y` — select the line"))))
    (should (ice-feature-map-tests--bullet-problems
             index (ice-feature-map-tests--features-copy-with
                    (cons "- `m s` — surround" "- `M s` — surround"))))
    (should (equal (ice-feature-map-tests--bullet-problems
                    index (ice-feature-map-tests--features-copy-with
                           (cons "- `m s` — surround" "- `m  s` — surround")))
                   nil))))

(ert-deftest ice-feature-map-every-main-file-is-in-a-code-section ()
  (let ((facts (ice-feature-map-tests--repo-facts)))
    (should (> (hash-table-count (gethash "files" facts)) 100))
    (should (equal (cl-remove-if (lambda (f) (member f (gethash "tests" facts)))
                                 (ice-feature-map-tests--coverage-problems facts (ice-feature-map-tests--features-text)))
                   nil))))

(ert-deftest ice-feature-map-every-test-file-is-in-a-code-section ()
  (let* ((facts (ice-feature-map-tests--repo-facts))
         (missing (cl-remove-if-not (lambda (f) (member f (gethash "tests" facts)))
                                    (ice-feature-map-tests--coverage-problems facts (ice-feature-map-tests--features-text)))))
    (should (gethash "tests" facts))
    (should (equal missing nil))))

(ert-deftest ice-feature-map-mapped-tests-are-listed-with-their-feature ()
  (should (equal (ice-feature-map-tests--mapped-test-problems
                  (ice-feature-map-tests--repo-facts) (ice-feature-map-tests--features-text))
                 nil)))

(ert-deftest ice-feature-map-lat-check-passes ()
  (skip-unless (executable-find "lat"))
  (let* ((output (with-temp-buffer
                   (let ((status (call-process "lat" nil t nil "--no-color" "--dir" (directory-file-name ice-feature-map-tests--root) "check")))
                     (cons status (buffer-string)))))
         (lines (cl-remove-if (lambda (l) (or (string-empty-p (string-trim l))
                                              (string-prefix-p "Scanned " l)
                                              (string-match-p "\\`Warning: No init version" l)
                                              (equal l "All checks passed")))
                              (split-string (cdr output) "\n"))))
    (should (equal lines nil))
    (should (eq (car output) 0))))

(defun ice-feature-map-tests--summary (&rest args)
  (with-temp-buffer
    (should (eq 0 (apply #'call-process ice-feature-map-tests--summary nil t nil
                         "--root" ice-feature-map-tests--root args)))
    (buffer-string)))

(ert-deftest ice-feature-map-summary-fits-budget ()
  (dolist (budget '(300 600 1200))
    (let ((out (ice-feature-map-tests--summary "--budget" (number-to-string budget))))
      (should (<= (length out) (* 4 budget)))
      (should (string-match-p "lat_search / lat_section on \\[\\[features\\]\\]" out)))))

(ert-deftest ice-feature-map-summary-default-budget-is-1200 ()
  (let ((out (ice-feature-map-tests--summary)))
    (should (<= (length out) (* 4 1200)))
    (should (equal out (ice-feature-map-tests--summary "--budget" "1200")))
    (should (string-match-p "1200" (with-temp-buffer
                                     (call-process ice-feature-map-tests--summary nil t nil "--help")
                                     (buffer-string))))))

(ert-deftest ice-feature-map-summary-one-line-per-feature ()
  (let* ((h2s (cl-remove-if-not (lambda (h) (= (plist-get h :level) 2))
                                (ice-feature-map-tests--parse (ice-feature-map-tests--features-text))))
         (out (ice-feature-map-tests--summary))
         (lines (cl-remove-if-not (lambda (l) (string-prefix-p "- " l)) (split-string out "\n"))))
    (should (= (length lines) (length h2s)))
    (cl-mapc (lambda (line h) (should (string-prefix-p (concat "- " (plist-get h :title)) line)))
             lines h2s)))

(ert-deftest ice-feature-map-summary-deterministic ()
  (should (equal (ice-feature-map-tests--summary) (ice-feature-map-tests--summary)))
  (should (equal (ice-feature-map-tests--summary "--budget" "300")
                 (ice-feature-map-tests--summary "--budget" "300"))))

(ert-deftest ice-feature-map-summary-is-language-agnostic ()
  (let* ((root (file-name-as-directory (make-temp-file "ice-summary" t)))
         (file (expand-file-name "lat.md/features.md" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "# Features\n\nThe app.\n\n## Blog\n\nPosts and comments.\n\n### How to get to it\n\nRoutes.\n\n- `GET /blog` — list posts\n- `POST /blog` — add one\n\n### Code\n\nFiles.\n\n- `app/blog/page.tsx`\n- `app/blog/page.test.tsx`\n"))
          (let ((out (with-temp-buffer
                       (call-process ice-feature-map-tests--summary nil t nil "--root" root)
                       (buffer-string))))
            (should (string-match-p "- Blog: Posts and comments — keys: GET /blog, POST /blog — code: app/blog/page.tsx" out))
            (should-not (string-match-p "page.test" out))))
      (delete-directory root t))))

(defconst ice-feature-map-tests--wiki-script
  "const { DatabaseSync } = require('node:sqlite');
const [path, tables, rows] = [process.argv[1], JSON.parse(process.argv[2]), JSON.parse(process.argv[3])];
const db = new DatabaseSync(path);
const repos = JSON.parse(process.argv[4] || '[]');
if (repos.length) { db.exec('CREATE TABLE repositories (id TEXT, local_path TEXT, head_commit TEXT)'); for (const [id, p, h] of repos) db.prepare('INSERT INTO repositories VALUES (?,?,?)').run(id, p, h); }
if (tables.includes('git_metadata')) db.exec('CREATE TABLE git_metadata (file_path TEXT, commit_count_90d INT, commit_count_total INT, is_hotspot INT, co_change_partners_json TEXT)');
if (tables.includes('health_file_metrics')) db.exec('CREATE TABLE health_file_metrics (file_path TEXT, score REAL)');
if (tables.includes('dead_code_findings')) db.exec('CREATE TABLE dead_code_findings (file_path TEXT, kind TEXT)');
if (tables.includes('git_metadata')) for (const [f, n, hot, co] of rows) db.prepare('INSERT INTO git_metadata VALUES (?,?,?,?,?)').run(f, n, n, hot, co);
if (tables.includes('health_file_metrics')) db.prepare('INSERT INTO health_file_metrics VALUES (?,?)').run('alpha/one.py', 7.5);
if (tables.includes('dead_code_findings')) db.prepare('INSERT INTO dead_code_findings VALUES (?,?)').run('alpha/one.py', 'unused_function');
db.close();")

(defconst ice-feature-map-tests--wiki-rows
  `(("alpha/one.py" 40 1 ,(json-serialize [(:file_path "beta/two.py" :frequency 0.8)]))
    ("beta/two.py" 1 0 :null)))

(defconst ice-feature-map-tests--wiki-fixture
  '(("alpha/one.py" . "def one():\n    return 1\n")
    ("beta/two.py" . "def two():\n    return 2\n")
    ("gamma/three.el" . ";;; three.el --- t -*- lexical-binding: t; -*-\n(defun three-x () 1)\n")))

(defun ice-feature-map-tests--wiki-repo (&optional tables state repositories no-commit)
  (let ((root (file-name-as-directory (file-truename (make-temp-file "ice-feature-wiki" t)))))
    (pcase-dolist (`(,path . ,text) ice-feature-map-tests--wiki-fixture)
      (let ((file (expand-file-name path root)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file (insert text))))
    (let ((default-directory root))
      (call-process "git" nil nil nil "init" "-q")
      (unless no-commit
        (call-process "git" nil nil nil "add" "-A")
        (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t" "-c" "commit.gpgsign=false" "commit" "-q" "-m" "init")))
    (when tables
      (make-directory (expand-file-name ".repowise" root) t)
      (should (= 0 (call-process "timeout" nil nil nil "60" "node" "-e" ice-feature-map-tests--wiki-script
                                 (expand-file-name ".repowise/wiki.db" root)
                                 (json-serialize (vconcat tables))
                                 (json-serialize (vconcat (mapcar #'vconcat ice-feature-map-tests--wiki-rows)) :null-object :null)
                                 (json-serialize (vconcat (mapcar (lambda (row) (vconcat (mapcar (lambda (v) (if (eq v 'root) (directory-file-name root) v)) row))) repositories))))))
      (when state
        (with-temp-file (expand-file-name ".repowise/state.json" root) (insert state))))
    root))

(defun ice-feature-map-tests--masked (root &rest args)
  (let ((text (replace-regexp-in-string (regexp-quote root) "ROOT/" (apply #'ice-feature-map-tests--run "--root" root args))))
    (replace-regexp-in-string (regexp-quote (file-name-nondirectory (directory-file-name root))) "NAME" text)))

(defun ice-feature-map-tests--cluster-of (facts file)
  (cl-find-if (lambda (c) (member file (gethash "files" c))) (gethash "clusters" facts)))

(defconst ice-feature-map-tests--wiki-tables '("git_metadata" "health_file_metrics" "dead_code_findings"))

(ert-deftest ice-feature-map-repowise-fields-and-co-change-cluster ()
  (let* ((root (ice-feature-map-tests--wiki-repo ice-feature-map-tests--wiki-tables "{\"last_sync_commit\":\"abc123\"}"))
         (plain-root (ice-feature-map-tests--wiki-repo)))
    (unwind-protect
        (let ((facts (ice-feature-map-tests--facts-for root))
              (plain (ice-feature-map-tests--facts-for plain-root)))
          (should-not (equal (ice-feature-map-tests--cluster-of plain "alpha/one.py")
                             (ice-feature-map-tests--cluster-of plain "beta/two.py")))
          (should (equal (gethash "files" (ice-feature-map-tests--cluster-of facts "alpha/one.py"))
                         (gethash "files" (ice-feature-map-tests--cluster-of facts "beta/two.py"))))
          (should-not (member "gamma/three.el" (gethash "files" (ice-feature-map-tests--cluster-of facts "alpha/one.py"))))
          (let ((info (gethash "repowise" (gethash "alpha/one.py" (gethash "files" facts)))))
            (should (= 40 (gethash "commits_90d" info)))
            (should (eq t (gethash "hotspot" info)))
            (should (= 7.5 (gethash "health" info)))
            (should (equal '("unused_function") (gethash "dead_code" info)))
            (should (equal "beta/two.py" (gethash "path" (car (gethash "co_change" info))))))
          (let ((top (gethash "repowise" facts)))
            (should (equal "abc123" (gethash "indexed_commit" top)))
            (should (eq t (gethash "stale" top))))
          (let ((head (string-trim (let ((default-directory root)) (shell-command-to-string "git rev-parse HEAD")))))
            (with-temp-file (expand-file-name ".repowise/state.json" root)
              (insert (format "{\"last_sync_commit\":\"%s\"}" head)))
            (let ((top (gethash "repowise" (ice-feature-map-tests--facts-for root))))
              (should (equal head (gethash "head" top)))
              (should (eq :false (gethash "stale" top)))))
          (should-not (gethash "repowise" plain))
          (should-not (gethash "repowise" (gethash "alpha/one.py" (gethash "files" plain))))
          (should (equal (ice-feature-map-tests--masked root "--no-repowise")
                         (ice-feature-map-tests--masked plain-root))))
      (delete-directory root t)
      (delete-directory plain-root t))))

(defun ice-feature-map-tests--head (root)
  (string-trim (let ((default-directory root)) (shell-command-to-string "git rev-parse HEAD"))))

(ert-deftest ice-feature-map-repowise-unknown-indexed-commit-is-not-stale ()
  (let ((root (ice-feature-map-tests--wiki-repo ice-feature-map-tests--wiki-tables)))
    (unwind-protect
        (let ((top (gethash "repowise" (ice-feature-map-tests--facts-for root))))
          (should (eq :null (gethash "indexed_commit" top)))
          (should (eq :null (gethash "stale" top))))
      (delete-directory root t))))

(ert-deftest ice-feature-map-repowise-abbreviated-sha-matches-head ()
  (let ((root (ice-feature-map-tests--wiki-repo ice-feature-map-tests--wiki-tables)))
    (unwind-protect
        (let ((head (ice-feature-map-tests--head root)))
          (with-temp-file (expand-file-name ".repowise/state.json" root)
            (insert (format "{\"last_sync_commit\":\"%s\"}" (substring head 0 7))))
          (should (eq :false (gethash "stale" (gethash "repowise" (ice-feature-map-tests--facts-for root))))))
      (delete-directory root t))))

(ert-deftest ice-feature-map-repowise-falls-back-to-repositories-head-commit ()
  (let ((root (ice-feature-map-tests--wiki-repo ice-feature-map-tests--wiki-tables nil '(("r1" root "pending")))))
    (unwind-protect
        (let ((top (gethash "repowise" (ice-feature-map-tests--facts-for root))))
          (should (equal "pending" (gethash "indexed_commit" top)))
          (should (eq t (gethash "stale" top))))
      (delete-directory root t))))

(ert-deftest ice-feature-map-repowise-ignores-db-from-another-repo ()
  (let ((foreign (ice-feature-map-tests--wiki-repo ice-feature-map-tests--wiki-tables nil '(("r1" "/nonexistent/other/repo" "abc"))))
        (empty-path (ice-feature-map-tests--wiki-repo ice-feature-map-tests--wiki-tables nil '(("r1" "" "")))))
    (unwind-protect
        (progn
          (should-not (gethash "repowise" (ice-feature-map-tests--facts-for foreign)))
          (should-not (gethash "repowise" (gethash "alpha/one.py" (gethash "files" (ice-feature-map-tests--facts-for foreign)))))
          (should (gethash "repowise" (ice-feature-map-tests--facts-for empty-path))))
      (delete-directory foreign t)
      (delete-directory empty-path t))))

(ert-deftest ice-feature-map-repowise-no-commit-repo-keeps-stderr-empty ()
  (let ((root (ice-feature-map-tests--wiki-repo ice-feature-map-tests--wiki-tables nil nil t))
        (err (make-temp-file "ice-feature-err")))
    (unwind-protect
        (progn
          (should (eq 0 (call-process ice-feature-map-tests--facts nil nil nil "--root" root)))
          (call-process "bash" nil nil nil "-c" (format "%s --root %s >/dev/null 2>%s"
                                                         (shell-quote-argument ice-feature-map-tests--facts)
                                                         (shell-quote-argument root) (shell-quote-argument err)))
          (should (zerop (file-attribute-size (file-attributes err)))))
      (delete-file err)
      (delete-directory root t))))

(ert-deftest ice-feature-map-repowise-without-usable-db-matches-no-db ()
  (let ((plain-root (ice-feature-map-tests--wiki-repo))
        (partial (ice-feature-map-tests--wiki-repo '("health_file_metrics")))
        (garbage (ice-feature-map-tests--wiki-repo)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".repowise" garbage) t)
          (with-temp-file (expand-file-name ".repowise/wiki.db" garbage)
            (insert "not a sqlite database, only garbage\n"))
          (let ((want (ice-feature-map-tests--masked plain-root)))
            (dolist (root (list partial garbage))
              (should (equal want (ice-feature-map-tests--masked root))))))
      (delete-directory plain-root t)
      (delete-directory partial t)
      (delete-directory garbage t))))

(provide 'ice-feature-map-tests)
;;; ice-feature-map-tests.el ends here
