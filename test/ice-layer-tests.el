;;; ice-layer-tests.el --- Tests for the ICE layer: lat.md, OpenSpec, C4 and the Context row -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'yggdrasil-leader)
(require 'ygg-projects)
(require 'ygg-ice)
(require 'aob-context)

(defconst ice-tests--lat
  (car (file-expand-wildcards
        (expand-file-name "~/.local/share/mise/installs/npm-lat-md/*/node_modules/.bin/lat"))))

(defconst ice-tests--check-script
  (expand-file-name "../etc/ice/ice-check" (file-name-directory (or load-file-name buffer-file-name)))
  "This checkout's ice-check.")

(defconst ice-tests--files
  `(("lat.md/lat.md" . "# Project\n\nThe map of this project.\n\n- [[architecture]] - how the pieces fit\n- [[auth]] - login and tokens\n")
    ("lat.md/architecture.md" . "# Architecture\n\nHow the system is built.\n\n## Request Pipeline\n\nRequests go through [[auth#OAuth Flow]] first, then [[src/server.ts#handle]].\n")
    ("lat.md/auth.md" . "# Auth\n\nLogin and token handling.\n\n## OAuth Flow\n\nThe OAuth flow validates tokens. See [[architecture#Request Pipeline|the pipeline]].\n\n```md\n# not a heading\n```\n")
    ("src/server.ts" . ,(concat "// @" "lat: [[architecture#Request Pipeline]]\nexport function handle() { return 1 }\n"))
    ("openspec/changes/add-login/proposal.md" . "## Why\nLogin. Touches [[auth#OAuth Flow]].\n")
    ("openspec/changes/add-login/tasks.md" . "## 1. Build\n- [x] 1.1 Write the form\n- [ ] 1.2 Wire [[auth#OAuth Flow]]\n")
    ("openspec/changes/add-login/specs/auth/spec.md" . "## ADDED Requirements\n### Requirement: Login\n")
    ("openspec/changes/archive/2026-01-01-old/tasks.md" . "- [x] a\n")
    ("docs/adr/0001-use-postgres.md" . "# Use Postgres\n\nBecause.\n")
    ("CONTEXT.md" . "**Token**: a proof of login.\nAvoid: ticket\n")
    ("docs/arch/model.c4" . "model {\n  shop = system 'Shop' {\n    api = container 'API' {\n      link ../../lat.md/auth.md\n    }\n  }\n}\nviews {\n  view index {\n    title 'Landscape'\n    include *\n  }\n}\n")
    ("docs/arch/README.md" . "<!-- Generated -->\n\n# shop\n\n### Landscape\n\n```mermaid\ngraph TB\n  A --> B\n```\n"))
  "A small repository with every kind of ICE doc.")

(defmacro ice-tests--with-repo (var files &rest body)
  "Run BODY with VAR a fresh repository holding FILES, homes pointed away.
HOME and the XDG folders are temp folders, so lat writes nothing to the
real ones, and every cache starts empty."
  (declare (indent 2))
  `(let* ((,var (file-name-as-directory
                 (file-truename (make-temp-file "ice-repo" t))))
          (home (make-temp-file "ice-home" t))
          (process-environment (append (list (concat "HOME=" home)
                                             (concat "XDG_CONFIG_HOME=" home "/.config")
                                             (concat "XDG_CACHE_HOME=" home "/.cache")
                                             (concat "XDG_DATA_HOME=" home "/.local/share")
                                             (concat "CODEX_HOME=" home "/.codex"))
                                       process-environment))
          (ygg-ice-lat-program (or ice-tests--lat "ice-tests-no-lat"))
          (ygg-ice--sections-cache (make-hash-table :test #'equal))
          (ygg-ice--lat-memo (make-hash-table :test #'equal))
          (ygg-ice--context-cache (make-hash-table :test #'equal))
          (ygg-ice--context-pending (make-hash-table :test #'equal))
          (ygg-ice-context-changed-functions nil))
     (unwind-protect
         (progn
           (pcase-dolist (`(,path . ,text) ,files)
             (let ((file (expand-file-name path ,var)))
               (make-directory (file-name-directory file) t)
               (with-temp-file file (insert text))))
           ,@body)
       (delete-directory ,var t)
       (delete-directory home t))))

(defun ice-tests--no-lat ()
  "Bind nothing; a lat that is not there, so only the local reading answers."
  "ice-tests-no-lat")

;;; Wiki links

(ert-deftest ice-link-at-point-reads-the-target-and-drops-an-alias ()
  (with-temp-buffer
    (insert "See [[auth#OAuth Flow|the flow]] and [[src/x.ts#run]].")
    (goto-char (point-min))
    (should-not (ygg-ice-link-at-point))
    (search-forward "OAuth")
    (should (equal (ygg-ice-link-at-point) "auth#OAuth Flow"))
    (search-forward "src/")
    (should (equal (ygg-ice-link-at-point) "src/x.ts#run"))))

(ert-deftest ice-sections-carry-lat-ids-lines-and-links ()
  (ice-tests--with-repo root ice-tests--files
    (let* ((secs (ygg-ice-sections root))
           (flow (ygg-ice--section-by-id root "lat.md/auth#Auth#OAuth Flow")))
      (should (member "lat.md/architecture#Architecture#Request Pipeline"
                      (mapcar (lambda (s) (plist-get s :id)) secs)))
      (should-not (seq-find (lambda (s) (equal (plist-get s :heading) "not a heading")) secs))
      (should (= (plist-get flow :line) 5))
      (should (= (plist-get flow :end) 11))
      (should (equal (mapcar #'car (plist-get flow :links))
                     '("architecture#Request Pipeline"))))))

(ert-deftest ice-links-resolve-to-sections-and-code-without-lat ()
  "Stubbed: lat is not there, so this is the local reading alone."
  (ice-tests--with-repo root ice-tests--files
    (let ((ygg-ice-lat-program (ice-tests--no-lat))
          (at (lambda (target)
                (when-let* ((to (ygg-ice-resolve root target)))
                  (cons (file-relative-name (plist-get to :file) root)
                        (plist-get to :line))))))
      (should (equal (funcall at "auth#OAuth Flow") '("lat.md/auth.md" . 5)))
      (should (equal (funcall at "lat.md/auth#Auth#OAuth Flow") '("lat.md/auth.md" . 5)))
      (should (equal (funcall at "OAuth Flow") '("lat.md/auth.md" . 5)))
      (should (equal (funcall at "architecture") '("lat.md/architecture.md" . 1)))
      (should (equal (funcall at "src/server.ts#handle") '("src/server.ts" . 2)))
      (should-not (funcall at "nowhere#Thing")))))

(ert-deftest ice-lat-locate-answers-what-the-local-reading-cannot ()
  "Real lat: a fuzzy name only lat locate knows."
  (skip-unless ice-tests--lat)
  (ice-tests--with-repo root ice-tests--files
    (should-not (ygg-ice-section root "OAuth Flo"))
    (let ((to (ygg-ice-resolve root "OAuth Flo")))
      (should (equal (plist-get to :id) "lat.md/auth#Auth#OAuth Flow"))
      (should (equal (file-relative-name (plist-get to :file) root) "lat.md/auth.md"))
      (should (= (plist-get to :line) 5)))
    (should-not (directory-files-recursively
                 (getenv "HOME") "" nil
                 (lambda (d) (not (string-match-p "eln-cache" d)))))))

(ert-deftest ice-lat-refs-give-sections-and-code ()
  "Real lat: refs read into sections and code backlinks."
  (skip-unless ice-tests--lat)
  (ice-tests--with-repo root ice-tests--files
    (let ((refs (ygg-ice-refs root "lat.md/architecture#Architecture#Request Pipeline")))
      (should (equal (mapcar (lambda (h) (plist-get h :id)) (car refs))
                     '("lat.md/auth#Auth#OAuth Flow")))
      (should (equal (mapcar (lambda (c) (cons (file-relative-name (plist-get c :file) root)
                                               (plist-get c :line)))
                             (cdr refs))
                     '(("src/server.ts" . 1)))))))

(ert-deftest ice-lat-refs-output-parses ()
  "Stubbed: lat refs output as lat 0.12.2 prints it."
  (let ((refs (ygg-ice--parse-lat-refs
               "\n## References to \"lat.md/a#A\":\n\n* Section: [[lat.md/b#B#C]] (wiki link)\n  Defined in lat.md/b.md:5-7\n\n  > Goes to [[a#A]].\n\n## Code references:\n\n* src/x.ts:3\n* lib/y.py:10\n"
               "/r/")))
    (should (equal (car refs) '((:id "lat.md/b#B#C" :file "/r/lat.md/b.md" :line 5
                                     :text "Goes to [[a#A]]."))))
    (should (equal (cdr refs) '((:file "/r/src/x.ts" :line 3 :text "src/x.ts")
                                (:file "/r/lib/y.py" :line 10 :text "lib/y.py"))))))

(ert-deftest ice-xref-finds-definitions-on-a-link-and-stays-out-elsewhere ()
  (ice-tests--with-repo root ice-tests--files
    (let ((ygg-ice-lat-program (ice-tests--no-lat))
          (file (expand-file-name "lat.md/architecture.md" root)))
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file default-directory (file-name-directory file))
        (goto-char (point-min))
        (search-forward "How the")
        (should-not (ygg-ice-xref-backend))
        (search-forward "[[auth#O")
        (should (eq (ygg-ice-xref-backend) 'ygg-ice))
        (let* ((id (xref-backend-identifier-at-point 'ygg-ice))
               (loc (xref-item-location (car (xref-backend-definitions 'ygg-ice id)))))
          (should (equal id "auth#OAuth Flow"))
          (should (equal (xref-location-group loc) (expand-file-name "lat.md/auth.md" root)))
          (should (= (xref-location-line loc) 5)))
        (goto-char (point-min))
        (search-forward "## Request")
        (should (equal (xref-backend-identifier-at-point 'ygg-ice)
                       "lat.md/architecture#Architecture#Request Pipeline"))
        (set-buffer-modified-p nil)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "docs/adr/0001-use-postgres.md" root))
        (insert "# Use Postgres\n")
        (goto-char (point-min))
        (should-not (ygg-ice-xref-backend))
        (set-buffer-modified-p nil))))

(ert-deftest ice-markdown-wiki-follow-goes-through-lat-in-the-docs ()
  (skip-unless (require 'markdown-mode nil t))
  (should (advice-member-p #'ygg-ice--follow-wiki-link 'markdown-follow-wiki-link)))

;;; Connections

(ert-deftest ice-connections-hold-links-refs-code-changes-and-c4 ()
  "Stubbed refs: the output lat refs gives, parsed; the rest read here."
  (ice-tests--with-repo root ice-tests--files
    (let* ((ygg-ice-lat-program (ice-tests--no-lat))
           (refs (ygg-ice--parse-lat-refs
                  "* Section: [[lat.md/architecture#Architecture#Request Pipeline]] (wiki link)\n  Defined in lat.md/architecture.md:5-7\n\n  > x\n"
                  root))
           (data (ygg-ice-connections-data root "lat.md/auth#Auth#OAuth Flow" refs))
           (places (lambda (key)
                     (mapcar (lambda (it) (cons (file-relative-name (plist-get it :file) root)
                                                (plist-get it :line)))
                             (plist-get data key)))))
      (should (equal (funcall places :outgoing) '(("lat.md/architecture.md" . 5))))
      (should (equal (funcall places :incoming) '(("lat.md/architecture.md" . 5))))
      (should (equal (funcall places :changes)
                     '(("openspec/changes/add-login/proposal.md" . 2)
                       ("openspec/changes/add-login/tasks.md" . 3))))
      (should (equal (funcall places :views) '(("docs/arch/model.c4" . 4))))
      (should (equal (plist-get (car (plist-get data :views)) :label) "api")))))

(ert-deftest ice-connections-code-comes-from-real-lat-refs ()
  (skip-unless ice-tests--lat)
  (ice-tests--with-repo root ice-tests--files
    (let* ((id "lat.md/architecture#Architecture#Request Pipeline")
           (data (ygg-ice-connections-data root id (ygg-ice-refs root id))))
      (should (equal (mapcar (lambda (it) (plist-get it :label)) (plist-get data :code))
                     '("src/server.ts")))
      (should (equal (mapcar (lambda (it) (plist-get it :id)) (plist-get data :outgoing))
                     '("lat.md/auth#Auth#OAuth Flow" "src/server.ts#handle"))))))

(ert-deftest ice-connections-view-draws-a-tree-whose-rows-jump ()
  (ice-tests--with-repo root ice-tests--files
    (let ((ygg-ice-lat-program (ice-tests--no-lat))
          (default-directory root))
      (save-window-excursion
        (ygg-ice-connections (ygg-ice--section-by-id root "lat.md/auth#Auth#OAuth Flow"))
        (with-current-buffer "*ice: lat.md/auth#Auth#OAuth Flow*"
          (should (string-match-p "^links to\n  └ architecture#Request Pipeline" (buffer-string)))
          (should (string-match-p "^changes\n  ├ add-login" (buffer-string)))
          (goto-char (point-min))
          (search-forward "├ add-login")
          (should (equal (plist-get (ygg-ice--item-at (line-beginning-position)) :line) 2))
          (kill-buffer))))))

;;; OpenSpec

(ert-deftest ice-changes-carry-their-task-progress ()
  (ice-tests--with-repo root ice-tests--files
    (let ((changes (ygg-ice-changes root)))
      (should (equal (mapcar (lambda (c) (plist-get c :name)) changes) '("add-login")))
      (should (= (plist-get (car changes) :done) 1))
      (should (= (plist-get (car changes) :total) 2)))))

(ert-deftest ice-tasks-view-ticks-each-task-and-jumps-to-its-line ()
  (ice-tests--with-repo root ice-tests--files
    (save-window-excursion
      (ygg-ice-tasks (car (ygg-ice-changes root)))
      (with-current-buffer "*ice: tasks add-login*"
        (should (string-match-p "add-login  1/2" (buffer-string)))
        (should (string-match-p "  \\[x\\] 1.1 Write the form" (buffer-string)))
        (goto-char (point-min))
        (search-forward "[ ] 1.2")
        (should (equal (plist-get (ygg-ice--item-at (line-beginning-position)) :line) 3))
        (kill-buffer)))))

;;; Running the tools

(ert-deftest ice-compile-buffer-finds-lat-and-ice-check-places ()
  (ice-tests--with-repo root ice-tests--files
    (let ((change (expand-file-name "openspec/changes/add-login" root)))
      (with-temp-buffer
        (setq default-directory root)
        (insert "- lat.md/auth.md:11: broken link [[nowhere#Thing]]\n"
                (format "ice-check intent %s: intent.md is missing\n" change)
                (format "ice-check plan %s: ok\n" change))
        (ygg-ice-compile-mode)
        (setq default-directory root)
        (compilation--ensure-parse (point-max))
        (let ((places nil))
          (goto-char (point-min))
          (while (not (eobp))
            (when-let* ((msg (get-text-property (point) 'compilation-message)))
              (let ((loc (compilation--message->loc msg)))
                (push (list (file-relative-name
                             (expand-file-name (caar (compilation--loc->file-struct loc)) root)
                             root)
                            (compilation--loc->line loc)
                            (compilation--message->type msg))
                      places)))
            (forward-line 1))
          (should (equal (nreverse places)
                         '(("lat.md/auth.md" 11 2)
                           ("openspec/changes/add-login/intent.md" nil 2)
                           ("openspec/changes/add-login/tasks.md" nil 0)))))))))

(ert-deftest ice-likec4-json-becomes-one-based-rows ()
  (let ((rows (ygg-ice--likec4-rows
               "{\"valid\":false,\"errors\":[{\"message\":\"Nope\",\"file\":\"/r/docs/arch/m.c4\",\"line\":14,\"range\":{\"start\":{\"character\":12,\"line\":14}}}]}"
               "/r/")))
    (should-not (car rows))
    (should (equal (cdr rows) '("docs/arch/m.c4:15:13: error: Nope")))))

;;; The Context row

(defmacro ice-tests--quiet-projects (&rest body)
  "Run BODY with the sidebar's other counters standing still."
  `(cl-letf (((symbol-function 'ygg-projects--agents) (lambda (_) '(0 . 0)))
             ((symbol-function 'ygg-projects--commands) (lambda (_) '(0 . 0)))
             ((symbol-function 'ygg-projects--processes) (lambda (_) '(0 . 0)))
             ((symbol-function 'ygg-projects--folders) (lambda (r) (list r)))
             ((symbol-function 'ygg-projects--icon) (lambda (_ fallback) fallback)))
     ,@body))

(ert-deftest ice-sidebar-context-row-counts-open-changes-and-lists-the-docs ()
  (ice-tests--with-repo root ice-tests--files
    (ice-tests--quiet-projects
     (ygg-ice-context-update root)
     (should (equal (ygg-projects--context-spec root) '(context "C" "Context" "1")))
     (should (equal (mapcar #'car (ygg-projects--row-specs root))
                    '(agents context commands processes folders)))
     (let ((entries (ygg-projects--entries root 'context)))
       (should (equal (mapcar #'car entries)
                      '("add-login" "Architecture" "Auth" "0001 use postgres"
                        "CONTEXT.md" "Landscape")))
       (should (equal (mapcar (lambda (e) (ygg-projects--entry-badge (cdr e))) entries)
                      '("1/2" "lat" "lat" "adr" "glossary" "c4")))
       (should (equal (plist-get (cdr (nth 5 entries)) :line) 5))))))

(ert-deftest ice-sidebar-context-row-follows-a-change-on-disk ()
  (ice-tests--with-repo root ice-tests--files
    (let ((changed nil))
      (add-hook 'ygg-ice-context-changed-functions (lambda (r) (push r changed)))
      (should (ygg-ice-context-update root))
      (should-not (ygg-ice-context-update root))
      (let ((tasks (expand-file-name "openspec/changes/add-login/tasks.md" root)))
        (with-temp-file tasks (insert "- [x] a\n- [x] b\n- [ ] c\n"))
        (set-file-times tasks (time-add nil 10)))
      (should (ygg-ice-context-update root))
      (should (equal (ygg-projects--entry-badge (cdar (ygg-ice-context-entries root))) "2/3"))
      (should (equal changed (list root root))))))

(ert-deftest ice-sidebar-shows-nothing-in-a-repo-without-ice-docs ()
  (ice-tests--with-repo root '(("src/main.c" . "int main;\n"))
    (ice-tests--quiet-projects
     (ygg-ice-context-update root)
     (should-not (ygg-ice-context-present-p root))
     (should-not (ygg-projects--context-spec root))
     (should-not (assq 'context (ygg-projects--row-specs root)))
     (should-not (ygg-ice-context-entries root))
     (should-not (ygg-ice-mentions root)))))

(ert-deftest ice-sidebar-row-exists-before-the-first-read-lands ()
  "A stat decides the row, so it never appears late and pushes rows down."
  (ice-tests--with-repo root ice-tests--files
    (ice-tests--quiet-projects
     (cl-letf (((symbol-function 'run-at-time) #'ignore))
       (should (equal (ygg-projects--context-spec root) '(context "C" "Context" "0")))))))

;;; Sending rows to the quickfix and the agent context

(ert-deftest ice-sidebar-selection-sends-context-rows-to-quickfix-and-agent ()
  (ice-tests--with-repo root ice-tests--files
    (ygg-ice-context-update root)
    (let* ((entries (ygg-ice-context-entries root))
           (sent nil)
           (aob-context--items nil))
      (with-temp-buffer
        (insert (propertize "agents\n" 'ygg-project root 'ygg-row 'agents))
        (dolist (e entries)
          (insert (propertize (concat (car e) "\n") 'ygg-project root
                              'ygg-row 'context 'ygg-entry (cdr e))))
        (goto-char (point-min))
        (forward-line 2)
        (set-mark (point))
        (forward-line 2)
        (let ((ygg--visual-p t))
          (should (equal (mapcar (lambda (e) (plist-get e :label))
                                 (ygg-projects--selected-entries 'context))
                         '("Architecture" "Auth" "0001 use postgres")))
          (should-not (ygg-projects--selected-entries)))
        (cl-letf (((symbol-function 'ygg-qf-from-text)
                   (lambda (text &rest _) (push text sent) 1))
                  ((symbol-function 'ygg-projects--in-sidebar-p) #'ignore))
          (goto-char (point-min))
          (forward-line 1)
          (deactivate-mark)
          (let ((ygg--visual-p nil))
            (ygg-projects-context-quickfix)
            (ygg-projects-context-to-agent))
          (should (equal (car sent)
                         (format "%s:1: change add-login"
                                 (expand-file-name "openspec/changes/add-login/tasks.md" root))))
          (should (equal (plist-get (car aob-context--items) :file)
                         (expand-file-name "openspec/changes/add-login/tasks.md" root))))))))

(ert-deftest ice-section-goes-to-the-agent-as-its-own-text ()
  (ice-tests--with-repo root ice-tests--files
    (let* ((aob-context--items nil)
           (sec (ygg-ice--section-item
                 (ygg-ice--section-by-id root "lat.md/architecture#Architecture#Request Pipeline")))
           (view (car (ygg-ice-c4-views root))))
      (should (= (ygg-ice-send-context (list sec view)) 2))
      (let ((pipeline (cadr aob-context--items))
            (landscape (car aob-context--items)))
        (should (equal (plist-get pipeline :beg) 5))
        (should (equal (plist-get pipeline :end) 7))
        (should (string-prefix-p "## Request Pipeline\n" (plist-get pipeline :text)))
        (should-not (string-search "# Architecture\n" (plist-get pipeline :text)))
        (should (string-match-p "\\`### Landscape\n\n```mermaid\ngraph TB\n  A --> B\n```\n\\'"
                                (plist-get landscape :text)))))))

(ert-deftest ice-view-rows-go-to-the-quickfix-one-per-selected-row ()
  (ice-tests--with-repo root ice-tests--files
    (let ((sent nil))
      (cl-letf (((symbol-function 'ygg-qf-from-text)
                 (lambda (text &rest _) (push text sent) (length (split-string text "\n")))))
        (save-window-excursion
          (ygg-ice-tasks (car (ygg-ice-changes root)))
          (with-current-buffer "*ice: tasks add-login*"
            (goto-char (point-min))
            (search-forward "[x]")
            (set-mark (point))
            (search-forward "[ ]")
            (let ((ygg--visual-p t))
              (cl-letf (((symbol-function 'ygg-normal-state) #'ignore))
                (ygg-ice-view-quickfix)))
            (kill-buffer))))
      (let ((file (expand-file-name "openspec/changes/add-login/tasks.md" root)))
        (should (equal (car sent)
                       (format "%s:2: task 1.1 Write the form\n%s:3: task 1.2 Wire [[auth#OAuth Flow]]"
                               file file)))))))

;;; Compose: @ names an ICE doc

(ert-deftest ice-mentions-come-from-the-cache-and-name-every-kind ()
  (ice-tests--with-repo root ice-tests--files
    (should-not (ygg-ice-mentions root))
    (ygg-ice-context-update root)
    (let ((names (mapcar #'car (ygg-ice-mentions root))))
      (dolist (name '("lat:auth#Auth#OAuth-Flow" "lat:architecture#Architecture"
                      "change:add-login" "adr:0001-use-postgres" "glossary" "c4:landscape"))
        (should (member name names)))
      (should (equal (cdr (assoc "change:add-login" (ygg-ice-mentions root))) "  change 1/2")))))

(ert-deftest ice-mention-expands-to-a-pointer-never-the-text ()
  (ice-tests--with-repo root ice-tests--files
    (ygg-ice-context-update root)
    (let* ((default-directory root)
           (out (ygg-ice-expand-mentions
                 "Fix @lat:auth#Auth#OAuth-Flow, see @change:add-login.")))
      (should (string-prefix-p "Fix @lat:auth#Auth#OAuth-Flow, see @change:add-login.\n\n<ice-context>\n" out))
      (should (string-search "lat.md/auth.md:5" out))
      (should (string-search "lat section \"lat.md/auth#Auth#OAuth Flow\"" out))
      (should (string-search "[[lat.md/auth#Auth#OAuth Flow]]" out))
      (should (string-search "- @change:add-login: openspec change add-login in openspec/changes/add-login/" out))
      (should-not (string-search "validates tokens" out))
      (should-not (string-search "- @lat:auth#Auth:" out))
      (should-not (ygg-ice-expand-mentions "nothing named here"))
      (should-not (ygg-ice-expand-mentions "@lat:auth#Auth#OAuth-Flowing")))))

;;; Heading ids as lat builds them

(defconst ice-tests--markup-headings
  '(("The `parse` step" . "The  step")
    ("A *very* **bold** [link](http://x) move" . "A    move")
    ("`lead` code" . " code")
    ("Tail `code`" . "Tail ")
    ("snake_case_name and _em_ here" . "snake_case_name and  here")
    ("Esc \\* star and [[wiki]] link" . "Esc * star and  link")
    ("a *b **c** d* e" . "a  e")
    ("x **y *z* w** v" . "x  v")
    ("<b>html</b> tag &amp; ent" . "html tag & ent")
    ("Image ![alt](p.png) and <http://a.b> auto" . "Image  and  auto")
    ("unmatched ` tick * star _ und [ br" . "unmatched ` tick * star _ und [ br")
    ("foo_bar_ *baz*qux 2*3*4" . "foo_bar_ qux 24"))
  "Heading text and the id part lat 0.12.2 gives it, read off lat's own parser.")

(ert-deftest ice-heading-ids-drop-inline-markup-as-lat-does ()
  (pcase-dolist (`(,title . ,id) ice-tests--markup-headings)
    (should (equal (ygg-ice--heading-text title) id)))
  (ice-tests--with-repo root
      '(("lat.md/n.md" . "# Top\n\n## The `parse` step\n\n## C#\n\n## foo ##\n"))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) (ygg-ice-sections root))
                   '("lat.md/n#Top" "lat.md/n#Top#The  step" "lat.md/n#Top#C#" "lat.md/n#Top#foo")))
    (should (equal (plist-get (ygg-ice-section root "n#The  step") :heading) "The `parse` step"))))

(ert-deftest ice-heading-ids-are-the-ids-real-lat-prints ()
  "Real lat: every id read here is one lat section prints back unchanged."
  (skip-unless ice-tests--lat)
  (ice-tests--with-repo root
      (list (cons "lat.md/notes.md"
                  (concat "# Top\n\n"
                          (mapconcat (lambda (h) (concat "## " (car h) "\n\nx\n")) ice-tests--markup-headings "\n"))))
    (let ((ids (mapcar (lambda (s) (plist-get s :id)) (ygg-ice-sections root))))
      (should (= (length ids) (1+ (length ice-tests--markup-headings))))
      (dolist (id ids)
        (should (equal (cons id (string-prefix-p (concat "[[" id "]] (")
                                                 (ygg-ice--lat-sync root "section" id)))
                       (cons id t)))))))

;;; Wiki links outside lat

(defun ice-tests--follow (root rel target)
  "The wiki follow of TARGET in ROOT's REL: (orig NAME), or the file opened."
  (let ((file (expand-file-name rel root)) (orig-called nil) (opened nil))
    (with-temp-buffer
      (setq buffer-file-name file default-directory (file-name-directory file))
      (cl-letf (((symbol-function 'ygg-ice--visit) (lambda (f _line) (setq opened f))))
        (ygg-ice--follow-wiki-link (lambda (name &optional _other) (setq orig-called name)) target))
      (set-buffer-modified-p nil))
    (if orig-called (list 'orig orig-called) (and opened (file-relative-name opened root)))))

(ert-deftest ice-wiki-links-outside-lat-stay-markdowns ()
  (skip-unless (require 'markdown-mode nil t))
  (ice-tests--with-repo root '(("docs/guide.md" . "See [[setup]].\n") ("docs/setup.md" . "# Setup\n"))
    (let ((ygg-ice-lat-program (ice-tests--no-lat)))
      (should (equal (ice-tests--follow root "docs/guide.md" "setup") '(orig "setup")))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "docs/guide.md" root))
        (ygg-ice--markdown-setup)
        (should-not (local-variable-p 'markdown-enable-wiki-links))
        (should-not (memq #'ygg-ice-xref-backend xref-backend-functions))
        (set-buffer-modified-p nil)))))

(ert-deftest ice-wiki-links-with-lat-open-sections-else-fall-back ()
  (skip-unless (require 'markdown-mode nil t))
  (ice-tests--with-repo root (append '(("docs/guide.md" . "x\n")) ice-tests--files)
    (let ((ygg-ice-lat-program (ice-tests--no-lat)))
      (should (equal (ice-tests--follow root "docs/guide.md" "auth#OAuth Flow") "lat.md/auth.md"))
      (should (equal (ice-tests--follow root "lat.md/auth.md" "nowhere#Thing") '(orig "nowhere#Thing")))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "docs/guide.md" root))
        (ygg-ice--markdown-setup)
        (should (local-variable-p 'markdown-enable-wiki-links))
        (set-buffer-modified-p nil)))
    (should (equal (ice-tests--follow root "openspec/changes/add-login/tasks.md" "architecture")
                   "lat.md/architecture.md"))))

(ert-deftest ice-wiki-links-in-openspec-without-lat-fall-back ()
  (skip-unless (require 'markdown-mode nil t))
  (ice-tests--with-repo root '(("openspec/changes/x/proposal.md" . "[[y]]\n"))
    (let ((ygg-ice-lat-program (ice-tests--no-lat)))
      (should (equal (ice-tests--follow root "openspec/changes/x/proposal.md" "y") '(orig "y"))))))

;;; doc/ as well as docs/

(ert-deftest ice-doc-folder-holds-adrs-and-c4-when-there-is-no-docs ()
  (ice-tests--with-repo root '(("doc/adr/0001-use-sqlite.md" . "# Use SQLite\n")
                               ("doc/arch/model.c4" . "views {\n  view index {\n  }\n}\n"))
    (should (ygg-ice--present-p root))
    (should (equal (ygg-ice--arch-dir root) (expand-file-name "doc/arch/" root)))
    (should (equal (mapcar (lambda (a) (plist-get a :label)) (ygg-ice-adrs root)) '("0001 use sqlite")))
    (ygg-ice-context-update root)
    (should (assoc "adr:0001-use-sqlite" (ygg-ice-mentions root)))
    (should (assoc "c4:index" (ygg-ice-mentions root)))
    (let ((sig (ygg-ice--context-signature root)))
      (with-temp-file (expand-file-name "doc/adr/0002-cache.md" root) (insert "# Cache\n"))
      (should-not (equal sig (ygg-ice--context-signature root))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "0001 use sqlite"))
              ((symbol-function 'find-file) (lambda (f) f)))
      (should (equal (ygg-ice-open-adr root) (expand-file-name "doc/adr/0001-use-sqlite.md" root))))))

(ert-deftest ice-docs-folder-wins-over-doc ()
  (ice-tests--with-repo root '(("docs/readme.md" . "x\n") ("doc/adr/0001-old.md" . "# Old\n"))
    (should (equal (ygg-ice--docs-dir root) (expand-file-name "docs/" root)))
    (should-not (ygg-ice-adrs root))))

;;; ice-check lines in the compilation buffer

(ert-deftest ice-compile-regexp-takes-paths-with-spaces-and-gaps-with-colons ()
  (let* ((root (file-name-as-directory (make-temp-file "ice repo" t)))
         (change (expand-file-name "openspec/changes/add login" root)))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory root)
          (insert (format "ice-check intent %s: no refused alternative: Alternatives refused needs one\n" change)
                  (format "ice-check plan %s: expect: scenario 'x' is missing THEN\n" change)
                  (format "ice-check plan %s: tasks.md is missing\n" change)
                  (format "ice-check plan %s: ok\n" change)
                  "- lat.md/my notes.md:3: broken link\n")
          (ygg-ice-compile-mode)
          (setq default-directory root)
          (compilation--ensure-parse (point-max))
          (let (places)
            (goto-char (point-min))
            (while (not (eobp))
              (when-let* ((msg (get-text-property (point) 'compilation-message)))
                (let ((loc (compilation--message->loc msg)))
                  (push (list (file-relative-name
                               (expand-file-name (caar (compilation--loc->file-struct loc)) root)
                               root)
                              (compilation--message->type msg))
                        places)))
              (forward-line 1))
            (should (equal (nreverse places)
                           '(("openspec/changes/add login/intent.md" 2)
                             ("openspec/changes/add login/expectations.md" 2)
                             ("openspec/changes/add login/tasks.md" 2)
                             ("openspec/changes/add login/tasks.md" 0)
                             ("lat.md/my notes.md" 2))))))
      (delete-directory root t))))

;;; Scripts run from Emacs

(ert-deftest ice-check-command-names-the-script-by-an-absolute-path ()
  (let* ((change (file-name-as-directory (make-temp-file "ice-change" t)))
         (ygg-ice-check-script (concat "~/" (file-relative-name ice-tests--check-script "~")))
         (command nil))
    (unwind-protect
        (cl-letf (((symbol-function 'compilation-start) (lambda (cmd &rest _) (setq command cmd))))
          (ygg-ice-check-expect (list :name "c" :dir change))
          (should (string-prefix-p (concat (shell-quote-argument ice-tests--check-script) " expect ")
                                   command)))
      (delete-directory change t))))

;;; ice-compact from Emacs

(defmacro ice-tests--with-compact (planned &rest body)
  (declare (indent 1))
  `(let* ((ygg-ice-compact-script (expand-file-name "ice-compact"
                                                    (file-name-directory ice-tests--check-script)))
          (ygg-ice-compact-days 45)
          (commands nil)
          (buf (generate-new-buffer " *ice-compact-test*")))
     (unwind-protect
         (cl-letf (((symbol-function 'ygg-ice--compile)
                    (lambda (root command what &rest _)
                      (push (list root command what) commands)
                      (with-current-buffer buf (erase-buffer) (insert ,planned))
                      buf)))
           ,@body)
       (kill-buffer buf))))

(ert-deftest ice-compact-dry-runs-with-the-days-and-applies-on-yes ()
  (ice-tests--with-compact "to remove: 2026-07-28-a (60 days)\nkept: b (too recent, 10 days)\nto remove: 2026-06-01-c (117 days)\n"
    (let ((noninteractive nil) (prompt nil))
      (cl-letf (((symbol-function 'run-at-time) (lambda (_ _ f &rest args) (apply f args)))
                ((symbol-function 'y-or-n-p) (lambda (p) (setq prompt p) t)))
        (ygg-ice-compact "/tmp/cart/")
        (should (equal (caar commands) "/tmp/cart/"))
        (should (equal (nth 2 (car commands)) "compact"))
        (should (equal (nth 1 (car commands))
                       (format "%s --older-than 45 /tmp/cart" (shell-quote-argument ygg-ice-compact-script))))
        (should (memq #'ygg-ice--compact-finished
                      (buffer-local-value 'compilation-finish-functions buf)))
        (ygg-ice--compact-finished buf "finished\n")
        (should (equal prompt "Remove 2 archived changes older than 45 days? "))
        (should (= (length commands) 2))
        (should (equal (nth 1 (car commands))
                       (format "%s --older-than 45 --apply /tmp/cart" (shell-quote-argument ygg-ice-compact-script))))))))

(ert-deftest ice-compact-never-prompts-in-batch-on-failure-or-with-nothing-to-remove ()
  (ice-tests--with-compact "to remove: 2026-07-28-a (60 days)\n"
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) (error "Scheduled a prompt")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Prompted"))))
      (ygg-ice-compact "/tmp/cart/")
      (should noninteractive)
      (ygg-ice--compact-finished buf "finished\n")
      (let ((noninteractive nil))
        (ygg-ice--compact-finished buf "exited abnormally with code 1\n")
        (with-current-buffer buf (erase-buffer) (insert "kept: b (too recent, 10 days)\n"))
        (ygg-ice--compact-finished buf "finished\n"))
      (should (= (length commands) 1)))))

;;; Connections never wait on lat

(ert-deftest ice-connections-draw-without-synchronous-lat-and-resolve-late ()
  (ice-tests--with-repo root
      (append '(("lat.md/extra.md" . "# Extra\n\nSee [[OAuth Flo]] and [[auth#OAuth Flow]].\n"))
              ice-tests--files)
    (let ((located-calls nil) (default-directory root))
      (cl-letf (((symbol-function 'ygg-ice--lat-command) (lambda (_) '("lat")))
                ((symbol-function 'ygg-ice--lat-sync)
                 (lambda (&rest args) (error "Synchronous lat while drawing: %S" args)))
                ((symbol-function 'ygg-ice--lat-async)
                 (lambda (_root args callback) (push (cons args callback) located-calls))))
        (save-window-excursion
          (ygg-ice-connections (ygg-ice--section-by-id root "lat.md/extra#Extra"))
          (with-current-buffer "*ice: lat.md/extra#Extra*"
            (should (string-match-p "├ OAuth Flo  lat.md/extra.md:3" (buffer-string)))
            (should (eq (plist-get (save-excursion (goto-char (point-min)) (search-forward "├ OAuth Flo")
                                                   (ygg-ice--item-at (line-beginning-position)))
                                   :kind)
                        'broken))
            (should (equal (mapcar #'car located-calls) '(("refs" "lat.md/extra#Extra") ("locate" "OAuth Flo"))))
            (funcall (cdr (assoc '("locate" "OAuth Flo") located-calls))
                     "* Section: [[lat.md/auth#Auth#OAuth Flow]] (fuzzy)\n  Defined in lat.md/auth.md:5-11\n" 0)
            (should (string-match-p "├ OAuth Flo  lat.md/auth.md:5" (buffer-string)))
            (kill-buffer)))))))

(ert-deftest ice-lat-sync-cleans-up-when-interrupted ()
  (ice-tests--with-repo root ice-tests--files
    (let ((slow (expand-file-name "slow-lat" root)))
      (with-temp-file slow (insert "#!/bin/sh\nsleep 30\n"))
      (set-file-modes slow #o755)
      (let ((ygg-ice-lat-program slow)
            (ygg-ice-lat-timeout 20))
        (should (eq (with-timeout (0.3 'interrupted) (ygg-ice--lat-sync root "locate" "x"))
                    'interrupted))
        (should-not (seq-find (lambda (p) (and (string-prefix-p "ice-lat" (process-name p))
                                               (process-live-p p)))
                              (process-list)))
        (should-not (seq-find (lambda (b) (string-prefix-p " *ice-lat*" (buffer-name b))) (buffer-list)))
        (should (= 0 (hash-table-count ygg-ice--lat-memo)))))))

;;; Mentions expand where the popup looked

(ert-deftest ice-mentions-expand-from-the-target-sessions-folder ()
  (require 'aob)
  (ice-tests--with-repo root ice-tests--files
    (ygg-ice-context-update root)
    (let ((elsewhere (make-temp-file "ice-elsewhere" t))
          (aob--sessions (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (puthash "ice-s" (aob-session--create :id "ice-s" :dir root) aob--sessions)
            (with-temp-buffer
              (setq default-directory (file-name-as-directory elsewhere))
              (setq-local aob-compose--dir elsewhere)
              (setq-local aob-compose--target "ice-s")
              (should (equal (aob--capf-dir) root))
              (should (assoc "change:add-login" (ygg-ice-mentions (aob--capf-dir))))
              (should (string-search "- @change:add-login: openspec change add-login"
                                     (ygg-ice-expand-mentions "see @change:add-login")))))
        (delete-directory elsewhere t)))))

;;; The signature sees new files

(ert-deftest ice-context-signature-sees-new-change-and-c4-files ()
  (ice-tests--with-repo root ice-tests--files
    (let ((sig (ygg-ice--context-signature root)))
      (with-temp-file (expand-file-name "openspec/changes/add-login/specs/auth/more.md" root) (insert "x\n"))
      (should-not (equal sig (ygg-ice--context-signature root)))
      (setq sig (ygg-ice--context-signature root))
      (with-temp-file (expand-file-name "openspec/changes/add-login/intent.md" root) (insert "x\n"))
      (should-not (equal sig (ygg-ice--context-signature root)))
      (setq sig (ygg-ice--context-signature root))
      (make-directory (expand-file-name "docs/arch/deploy" root))
      (with-temp-file (expand-file-name "docs/arch/deploy/prod.c4" root) (insert "model {}\n"))
      (should-not (equal sig (ygg-ice--context-signature root))))))

;;; ice-check expect

(defun ice-tests--expect (text)
  "ice-check expect on a change whose expectations.md is TEXT, in a temp folder."
  (let ((dir (make-temp-file "ice-check" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (make-directory "c")
          (with-temp-file "c/expectations.md" (insert text))
          (with-temp-buffer
            (let ((status (call-process "python3" nil t nil ice-tests--check-script "expect" "c")))
              (cons status (split-string (buffer-string) "\n" t)))))
      (delete-directory dir t))))

(defconst ice-tests--scenario
  "## Scenario: cart#sum\n- GIVEN a cart\n- WHEN totalled\n- THEN the sum\n- Covers: C1, C2\n")

(ert-deftest ice-check-expect-takes-every-condition-spelling ()
  (should (equal (ice-tests--expect
                  (concat "## Contract\n### Preconditions\n- C1: not empty\n### POST-CONDITIONS\n- C2: the sum\n"
                          "### laws\n- L1: order free\n" ice-tests--scenario
                          "## Scenario: cart#order\n- GIVEN x\n- WHEN y\n- THEN z\n- Covers: L1\n- Check: property\n"))
                 '(0 "ice-check expect c: ok"))))

(ert-deftest ice-check-expect-names-the-headings-for-a-line-under-an-unknown-one ()
  (should (equal (ice-tests--expect
                  (concat "## Contract\n### Pre-conditions\n- C1: not empty\n### Invariants\n- C2: never negative\n"
                          ice-tests--scenario))
                 '(1 "ice-check expect c: contract line under unknown heading '### Invariants': use ### Pre-conditions, ### Post-conditions, ### Undefined inputs, ### Laws: - C2: never negative"))))

(ert-deftest ice-check-expect-reads-bold-and-bare-steps ()
  (should (equal (ice-tests--expect
                  (concat "## Contract\n### Pre-conditions\n- C1: not empty\n"
                          "## Scenario: cart#sum\n- **Given:** a cart\n- **When:** totalled\nThen: the sum\n- **Covers:** C1\n"))
                 '(0 "ice-check expect c: ok"))))

(ert-deftest ice-check-expect-calls-an-unfilled-line-empty ()
  (should (equal (ice-tests--expect
                  (concat "## Contract\n### Pre-conditions\n- C1:\n### Post-conditions\n- C2: the sum\n"
                          "### Undefined inputs\n- negative prices\n" ice-tests--scenario))
                 '(1 "ice-check expect c: contract line C1 is empty"))))

;;; Keys

(ert-deftest ice-leader-keys-are-bound-under-spc-a-k-with-labels ()
  (defvar ygg-leader-agent-map)
  (let ((ygg-leader-agent-map (make-sparse-keymap)))
    (ygg-ice--bind-leader)
    (let ((prefix (lookup-key ygg-leader-agent-map (kbd "k"))))
      (should (eq (if (keymapp prefix) prefix (cdr prefix)) ygg-ice-leader-map))))
  (pcase-dolist (`(,key ,cmd ,label) ygg-ice-leader-keys)
    (let ((def (lookup-key ygg-ice-leader-map (kbd key))))
      (should (eq (if (consp def) (cdr def) def) cmd))
      (should (commandp cmd))
      (should (equal (car (alist-get (aref (kbd key) 0) (cdr ygg-ice-leader-map))) label))))
  (should (eq (lookup-key ygg-projects-map "Q") #'ygg-projects-context-quickfix))
  (should (eq (lookup-key ygg-projects-map "c") #'ygg-projects-context-to-agent))
  (should (eq (lookup-key ygg-ice-view-mode-map "Q") #'ygg-ice-view-quickfix))
  (should (eq (lookup-key ygg-ice-view-mode-map "c") #'ygg-ice-view-context)))

(ert-deftest ice-leader-keys-are-the-owners-two-acts-and-four-lookups ()
  (should (equal (mapcar (lambda (row) (list (car row) (nth 1 row))) ygg-ice-leader-keys)
                 '(("R" ygg-ice-confirm-intent)
                   ("A" ygg-ice-approve-checkpoints)
                   ("o" ygg-ice-changes-list)
                   ("s" ygg-ice-lat-search)
                   ("c" ygg-ice-connections)
                   ("b" ygg-ice-c4-preview))))
  (dolist (key '("w" "i" "e" "p" "G" "M" "l" "f" "C" "O" "t" "a" "g" "v" "d" "r"))
    (should-not (lookup-key ygg-ice-leader-map (kbd key))))
  (dolist (cmd '(ygg-ice-wire ygg-ice-check-intent ygg-ice-check-expect ygg-ice-check-plan
                 ygg-ice-gaps ygg-ice-maintain ygg-ice-lat-check ygg-ice-follow
                 ygg-ice-connections-pick ygg-ice-open-change ygg-ice-tasks ygg-ice-open-adr
                 ygg-ice-open-glossary ygg-ice-c4-validate ygg-ice-c4-drift ygg-ice-c4-readme))
    (should (commandp cmd)))
  (should (eq (lookup-key ygg-ice-view-mode-map "t") #'ygg-ice-view-tasks))
  (should (eq (lookup-key ygg-ice-view-mode-map (kbd "RET")) #'ygg-ice-view-visit)))

;;; ice-check intent: the restate-back gate

(defconst ice-tests--template
  (expand-file-name "../etc/ice/schema/templates/intent.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "This checkout's intent template.")

(defconst ice-tests--intent-body
  (concat "# Intent\n\n## What is wanted\n\nCarts total.\n\n## Caller's view\n\nThe shopper sees a sum.\n\n"
          "## Constraints\n\nCents only.\n\n## Failure scenarios\n\nNo negative total.\n\n"
          "## Success scenarios\n\nThe sum shows.\n\n## Connections\n\nFeature: Cart\n\n"
          "## Out of scope\n\nTax.\n\n## Alternatives refused\n\n- floats: they round\n")
  "An intent with every part but Restated filled.")

(defun ice-tests--intent (text)
  "ice-check intent on a change whose intent.md is TEXT, in a temp folder."
  (let ((dir (make-temp-file "ice-check" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (make-directory "c")
          (with-temp-file "c/intent.md" (insert text))
          (with-temp-buffer
            (let ((status (call-process "python3" nil t nil ice-tests--check-script "intent" "c")))
              (cons status (split-string (buffer-string) "\n" t)))))
      (delete-directory dir t))))

(defconst ice-tests--unconfirmed "ice-check intent c: not confirmed: the owner sets Confirmed: YYYY-MM-DD sha1:XXXXXXXX under Restated (SPC a k R)")
(defconst ice-tests--unhashed "ice-check intent c: not confirmed: the Confirmed line carries no sha1 of What is wanted and Restated; the owner confirms again (SPC a k R)")
(defconst ice-tests--changed "ice-check intent c: intent changed since it was confirmed: the owner confirms again (SPC a k R)")

(defun ice-tests--hash (text)
  "The sha1 prefix ygg-ice writes on the Confirmed line of an intent.md holding TEXT."
  (with-temp-buffer
    (insert text)
    (ygg-ice--intent-hash)))

(defun ice-tests--confirmed (text date)
  "TEXT with a Confirmed line for DATE carrying TEXT's hash."
  (format "%s\nConfirmed: %s sha1:%s\n" text date (ice-tests--hash text)))
(defconst ice-tests--unrestated "ice-check intent c: empty section: Restated: the agent restates the intent in its own words")

(ert-deftest ice-check-intent-passes-only-restated-and-confirmed ()
  (should (equal (ice-tests--intent (ice-tests--confirmed
                                     (concat ice-tests--intent-body "\n## Restated\n\nCarts show a sum in cents.\n")
                                     "2026-09-26"))
                 '(0 "ice-check intent c: ok"))))

(ert-deftest ice-check-intent-fails-a-date-without-a-hash ()
  (should (equal (ice-tests--intent (concat ice-tests--intent-body
                                            "\n## Restated\n\nCarts show a sum in cents.\n\nConfirmed: 2026-09-26\n"))
                 (list 1 ice-tests--unhashed))))

(ert-deftest ice-check-intent-fails-once-the-intent-changes-after-confirmation ()
  (let ((confirmed (ice-tests--confirmed
                    (concat ice-tests--intent-body "\n## Restated\n\nCarts show a sum in cents.\n")
                    "2026-09-26")))
    (should (equal (ice-tests--intent (string-replace "Carts show a sum in cents." "Carts show a sum in euros." confirmed))
                   (list 1 ice-tests--changed)))
    (should (equal (ice-tests--intent (string-replace "Carts total." "Carts total and tax." confirmed))
                   (list 1 ice-tests--changed)))
    (should (equal (ice-tests--intent (string-replace "Cents only." "Euros only." confirmed))
                   '(0 "ice-check intent c: ok")))))

(ert-deftest ice-check-intent-fails-without-a-restated-section ()
  (should (equal (ice-tests--intent ice-tests--intent-body)
                 '(1 "ice-check intent c: missing section: Restated"))))

(ert-deftest ice-check-intent-fails-on-a-fresh-template ()
  (let ((out (ice-tests--intent (with-temp-buffer
                                  (insert-file-contents ice-tests--template)
                                  (buffer-string)))))
    (should (= 1 (car out)))
    (should (member ice-tests--unrestated (cdr out)))
    (should (member ice-tests--unconfirmed (cdr out)))))

(ert-deftest ice-check-intent-fails-when-only-the-confirmed-line-is-there ()
  (should (equal (ice-tests--intent (ice-tests--confirmed (concat ice-tests--intent-body "\n## Restated\n")
                                                          "2026-09-26"))
                 (list 1 ice-tests--unrestated))))

(ert-deftest ice-check-intent-fails-until-confirmed-holds-a-real-date ()
  (dolist (line '("" "Confirmed:\n" "Confirmed: YYYY-MM-DD\n" "Confirmed: 2026-13-40\n"
                  "Confirmed: yes\n"))
    (should (equal (ice-tests--intent (concat ice-tests--intent-body
                                              "\n## Restated\n\nCarts show a sum.\n\n" line))
                   (list 1 ice-tests--unconfirmed)))))

;;; The confirm command writes the owner's line

(defmacro ice-tests--with-intent (var text &rest body)
  "Run BODY with VAR a change whose intent.md is TEXT, in a fresh repo."
  (declare (indent 2))
  `(ice-tests--with-repo root (list (cons "openspec/changes/cart/intent.md" ,text))
     (let ((,var (list :name "cart" :dir (expand-file-name "openspec/changes/cart/" root))))
       (unwind-protect (progn ,@body)
         (when-let* ((buf (find-buffer-visiting
                           (expand-file-name "intent.md" (plist-get ,var :dir)))))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf))))))

(defun ice-tests--intent-of (change)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "intent.md" (plist-get change :dir)))
    (buffer-string)))

(defun ice-tests--check-change (change)
  (with-temp-buffer
    (setq default-directory (plist-get change :dir))
    (call-process "python3" nil t nil ice-tests--check-script "intent"
                  (directory-file-name (plist-get change :dir)))))

(ert-deftest ice-confirm-intent-replaces-the-placeholder-with-today ()
  (ice-tests--with-intent change
      (concat ice-tests--intent-body "\n## Restated\n\nCarts show a sum in cents.\n\nConfirmed: YYYY-MM-DD\n")
    (should (= 1 (ice-tests--check-change change)))
    (let (asked shown)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (setq asked prompt
                         shown (with-current-buffer "*ice: restated cart*" (buffer-string)))
                   t)))
        (should (equal (ygg-ice-confirm-intent change) (format-time-string "%Y-%m-%d"))))
      (should (string-match-p "cart" asked))
      (should (equal shown "Carts show a sum in cents.\n")))
    (should (equal (ice-tests--intent-of change)
                   (ice-tests--confirmed (concat ice-tests--intent-body "\n## Restated\n\nCarts show a sum in cents.\n")
                                         (format-time-string "%Y-%m-%d"))))
    (should (= 0 (ice-tests--check-change change)))))

(ert-deftest ice-confirm-intent-hashes-what-ice-check-hashes ()
  (ice-tests--with-intent change
      (concat (string-replace "Carts total." "Carts total.  \nThe shopper’s sum.\t" ice-tests--intent-body)
              "\n## Restated\n\n  \nThe shopper’s cart shows a sum.   \n\n\nConfirmed: 2020-01-01\n")
    (cl-letf (((symbol-function 'y-or-n-p) #'always))
      (ygg-ice-confirm-intent change))
    (should (string-match-p "^Confirmed: [0-9-]+ sha1:[0-9a-f]\\{8\\}$" (ice-tests--intent-of change)))
    (should (= 0 (ice-tests--check-change change)))
    (with-temp-file (expand-file-name "intent.md" (plist-get change :dir))
      (insert (string-replace "cart shows" "basket shows" (ice-tests--intent-of change))))
    (should (= 1 (ice-tests--check-change change)))
    (cl-letf (((symbol-function 'y-or-n-p) #'always))
      (ygg-ice-confirm-intent change))
    (should (= 0 (ice-tests--check-change change)))))

(ert-deftest ice-confirm-intent-adds-the-line-once-and-keeps-what-follows ()
  (ice-tests--with-intent change
      (concat "## Restated\n\nCarts show a sum.\nConfirmed: 2020-01-01\nConfirmed: 2020-02-02\n\n## Notes\n\nkept\n")
    (cl-letf (((symbol-function 'y-or-n-p) #'always))
      (ygg-ice-confirm-intent change))
    (should (equal (ice-tests--intent-of change)
                   (format "## Restated\n\nCarts show a sum.\nConfirmed: %s sha1:%s\n\n## Notes\n\nkept\n"
                           (format-time-string "%Y-%m-%d") (ice-tests--hash "## Restated\n\nCarts show a sum.\n")))))
  (ice-tests--with-intent change "## Restated\n\nCarts show a sum.\n\n## Notes\n\nkept\n"
    (cl-letf (((symbol-function 'y-or-n-p) #'always))
      (ygg-ice-confirm-intent change))
    (should (equal (ice-tests--intent-of change)
                   (format "## Restated\n\nCarts show a sum.\n\nConfirmed: %s sha1:%s\n\n## Notes\n\nkept\n"
                           (format-time-string "%Y-%m-%d") (ice-tests--hash "## Restated\n\nCarts show a sum.\n"))))))

(ert-deftest ice-confirm-intent-writes-nothing-on-no-and-refuses-an-empty-restatement ()
  (let ((text (concat ice-tests--intent-body "\n## Restated\n\nCarts show a sum.\n\nConfirmed: YYYY-MM-DD\n")))
    (ice-tests--with-intent change text
      (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
        (should-not (ygg-ice-confirm-intent change)))
      (should (equal (ice-tests--intent-of change) text))
      (should-not (get-buffer "*ice: restated cart*"))))
  (ice-tests--with-intent change "## Restated\n\nConfirmed: YYYY-MM-DD\n"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
      (should-error (ygg-ice-confirm-intent change) :type 'user-error))))

;;; Gaps and maintain spawn through the draft's preset path

(defvar aob-compose-spawn-function)
(defvar aob-compose--dir)
(require 'ygg-preset)

(defun ice-tests--host-defun (name)
  "Evaluate the host's own definition of NAME from layer-aob.el."
  (with-temp-buffer
    (insert-file-contents (locate-library "layer-aob.el"))
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (and (eq (car-safe form) 'defun) (eq (cadr form) name))
              (eval form t))))
      (end-of-file nil))))

(defconst ice-tests--skills
  (expand-file-name "../skills/" (file-name-directory (or load-file-name buffer-file-name)))
  "This checkout's skills.")

(defconst ice-tests--presets
  (expand-file-name "../presets/" (file-name-directory (or load-file-name buffer-file-name)))
  "This checkout's presets.")

(defmacro ice-tests--spawns (var presets &rest body)
  "Run BODY with every spawn recorded into VAR as (TEXT DIR DEFAULT-DIR).
Presets come from PRESETS only; the preset path is layer-aob's own."
  (declare (indent 2))
  `(let* ((,var nil)
          (ygg-preset-config-directory ,presets)
          (ygg-preset-user-directory (make-temp-name "/tmp/ice-tests-no-presets-"))
          (ygg-preset-home-dir (make-temp-name "/tmp/ice-tests-no-home-"))
          (ygg-preset-own-skills-dir ice-tests--skills)
          (aob-compose-spawn-function
          (lambda (text &rest _) (push (list text aob-compose--dir default-directory) ,var) 'spawned)))
     (ice-tests--host-defun 'ygg-aob--expand-presets)
     (ice-tests--host-defun 'ygg-aob--preset-limits)
     (cl-letf (((symbol-function 'ygg-aob--presets-of)
                (lambda (_dir) (cons nil (ygg-preset-list)))))
       ,@body)))

(ert-deftest ice-gaps-spawns-a-read-only-session-under-the-gaps-preset ()
  (ice-tests--with-intent change (concat ice-tests--intent-body)
    (ice-tests--spawns spawns ice-tests--presets
      (should (eq (ygg-ice-gaps change) 'spawned))
      (should (= 1 (length spawns)))
      (pcase-let ((`(,text ,dir ,default) (car spawns)))
        (should (equal dir root))
        (should (equal default root))
        (should (string-prefix-p
                 "@gaps Find the gaps in the intent of change cart: read openspec/changes/cart/intent.md and write openspec/changes/cart/gaps.md. Never edit intent.md."
                 text))
        (should (string-search "<preset name=\"gaps\">" text))
        (should (string-search "Ask owner: QUESTION (default: X)" text))
        (should (equal (plist-get (ygg-aob--preset-limits text) :want-tools)
                       '("Read" "Grep" "Glob" "Write")))))))

(ert-deftest ice-gaps-refuses-a-change-without-intent-or-a-missing-preset ()
  (ice-tests--with-repo root '(("openspec/changes/bare/proposal.md" . "## Why\n"))
    (ice-tests--spawns spawns ice-tests--presets
      (should-error (ygg-ice-gaps (list :name "bare" :dir (expand-file-name "openspec/changes/bare/" root)))
                    :type 'user-error)
      (should-not spawns)))
  (ice-tests--with-intent change ice-tests--intent-body
    (ice-tests--spawns spawns (make-temp-name "/tmp/ice-tests-no-presets-")
      (should-error (ygg-ice-gaps change) :type 'user-error)
      (should-not spawns))))

(ert-deftest ice-gaps-and-maintain-spawn-in-the-project-never-a-drafts-worktree ()
  (defvar ygg-aob--draft-tree)
  (ice-tests--with-intent change ice-tests--intent-body
    (let (trees)
      (ice-tests--spawns spawns ice-tests--presets
        (with-temp-buffer
          (setq-local ygg-aob--draft-tree "/other/draft/tree/")
          (let ((aob-compose-spawn-function
                 (lambda (&rest _) (push ygg-aob--draft-tree trees) 'spawned)))
            (should (eq (ygg-ice-gaps change) 'spawned))
            (should (eq (ygg-ice-maintain root) 'spawned)))
          (should (equal ygg-aob--draft-tree "/other/draft/tree/"))))
      (should (equal trees '(nil nil))))))

(require 'aob)

(defmacro ice-tests--live-in (dirs &rest body)
  "Run BODY with aob-live-sessions answering one session per folder in DIRS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'aob-live-sessions)
              (lambda () (mapcar (lambda (dir) (aob-session--create :id "s" :dir dir)) ,dirs))))
     ,@body))

(ert-deftest ice-maintain-refuses-a-project-with-a-live-agent-session ()
  (ice-tests--with-repo root ice-tests--files
    (ice-tests--spawns spawns ice-tests--presets
      (ice-tests--live-in (list (expand-file-name "sub/" root) nil)
        (should-error (ygg-ice-maintain root) :type 'user-error)
        (should-not spawns))
      (ice-tests--live-in (list "/somewhere/else/" nil)
        (should (eq (ygg-ice-maintain root) 'spawned))))))

(ert-deftest ice-maintain-daily-skips-a-project-with-a-live-agent-session ()
  (let* ((stamps (make-temp-file "ice-stamps" nil ".eld"))
         (ygg-ice-maintain-stamp-file stamps)
         (root (file-name-as-directory (make-temp-file "ice-daily" t)))
         (ygg-ice-maintain-daily (list root))
         runs said)
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-ice-maintain) (lambda (r) (push r runs)))
                  ((symbol-function 'message) (lambda (&rest args) (push (apply #'format args) said))))
          (delete-file stamps)
          (ice-tests--live-in (list root)
            (ygg-ice--maintain-tick))
          (should-not runs)
          (should-not (file-exists-p stamps))
          (should (string-match-p "skipped: 1 live agent session there" (car said))))
      (ignore-errors (delete-file stamps))
      (delete-directory root t))))

(ert-deftest ice-maintain-spawns-under-the-maintain-preset-in-the-project ()
  (ice-tests--with-repo root ice-tests--files
    (ice-tests--spawns spawns ice-tests--presets
      (should (eq (ygg-ice-maintain root) 'spawned))
      (pcase-let ((`(,text ,dir ,default) (car spawns)))
        (should (equal dir root))
        (should (equal default root))
        (should (string-prefix-p (format "@maintain Run the maintain pass on %s:"
                                         (abbreviate-file-name (directory-file-name root)))
                                 text))
        (should (string-search "<preset name=\"maintain\">" text))
        (should (string-search "Never commit" text))
        (should (string-search "<skill name=\"maintain-verification-skill\">" text))
        (should (string-search "verified-unreachable" text))
        (should (string-search "lat.md/features.md" (substring text (string-search "<skill " text))))))))

;;; The daily run is off unless projects are named

(ert-deftest ice-maintain-daily-is-off-by-default-with-no-timer ()
  (should (null (eval (car (get 'ygg-ice-maintain-daily 'standard-value)) t)))
  (should (null (default-toplevel-value 'ygg-ice-maintain-daily)))
  (should (null ygg-ice--maintain-timer))
  (let ((ygg-ice-maintain-daily '("/tmp/")))
    (should (null (ygg-ice--maintain-arm)))))

(ert-deftest ice-maintain-daily-starts-each-project-once-a-day ()
  (let* ((stamps (make-temp-file "ice-stamps" nil ".eld"))
         (ygg-ice-maintain-stamp-file stamps)
         (root (file-name-as-directory (make-temp-file "ice-daily" t)))
         (ygg-ice-maintain-daily (list root "/nowhere/at/all/"))
         (runs nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-ice-maintain) (lambda (r) (push r runs))))
          (delete-file stamps)
          (ygg-ice--maintain-tick)
          (ygg-ice--maintain-tick)
          (should (equal runs (list root)))
          (should (equal (ygg-ice--maintain-stamps)
                         (list (cons root (format-time-string "%Y-%m-%d"))))))
      (ignore-errors (delete-file stamps))
      (delete-directory root t))))

(ert-deftest ice-maintain-daily-retries-a-run-that-did-not-start ()
  (let* ((stamps (make-temp-file "ice-stamps" nil ".eld"))
         (ygg-ice-maintain-stamp-file stamps)
         (root (file-name-as-directory (make-temp-file "ice-daily" t)))
         (ygg-ice-maintain-daily (list root))
         (tries 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-ice-maintain)
                   (lambda (_r) (setq tries (1+ tries)) (user-error "ice: aob is not loaded")))
                  ((symbol-function 'message) #'ignore))
          (delete-file stamps)
          (ygg-ice--maintain-tick)
          (ygg-ice--maintain-tick)
          (should (= tries 2))
          (should-not (file-exists-p stamps)))
      (ignore-errors (delete-file stamps))
      (delete-directory root t))))

;;; The C4 readme: exported, then reread without a prompt

(defmacro ice-tests--no-prompts (&rest body)
  "Run BODY failing on any question a changed file could ask."
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Asked yes-or-no")))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked y-or-n")))
             ((symbol-function 'ask-user-about-supersession-threat)
              (lambda (&rest _) (error "Asked about supersession"))))
     ,@body))

(defun ice-tests--touch-later (file text)
  "Write TEXT to FILE behind Emacs's back, its mtime moved on."
  (with-temp-buffer
    (insert text)
    (let ((write-region-inhibit-fsync t))
      (write-region nil nil file nil 'silent)))
  (set-file-times file (time-add nil 10)))

(ert-deftest ice-readme-rereads-an-unmodified-buffer-without-a-prompt ()
  (ice-tests--with-repo root ice-tests--files
    (let* ((readme (expand-file-name "docs/arch/README.md" root))
           (buf (find-file-noselect readme)))
      (unwind-protect
          (progn
            (ice-tests--touch-later readme "# shop\n\nexported again\n")
            (ice-tests--no-prompts
             (should (eq (ygg-ice--visit-fresh readme) buf)))
            (should (equal (with-current-buffer buf (buffer-string)) "# shop\n\nexported again\n"))
            (with-current-buffer buf (insert "mine "))
            (ice-tests--touch-later readme "# shop\n\nthird\n")
            (ice-tests--no-prompts
             (should (eq (ygg-ice--visit-fresh readme) buf)))
            (should (string-prefix-p "mine # shop" (with-current-buffer buf (buffer-string)))))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest ice-c4-readme-exports-then-shows-the-fresh-readme ()
  (require 'ygg-diagram)
  (ice-tests--with-repo root ice-tests--files
    (let* ((readme (expand-file-name "docs/arch/README.md" root))
           (fake (expand-file-name "fake-likec4" root))
           (ygg-ice-likec4-program fake)
           (buf (find-file-noselect readme))
           (drawn nil))
      (with-temp-file fake
        (insert "#!/bin/sh\necho \"$@\" > \"$3/args\"\nsleep 1\nprintf '# shop\\n\\nexported\\n' > \"$3/README.md\"\n"))
      (set-file-modes fake #o755)
      (unwind-protect
          (cl-letf (((symbol-function 'ygg-diagram-toggle)
                     (lambda () (setq drawn (current-buffer))))
                    ((symbol-function 'pop-to-buffer-same-window) #'set-buffer))
            (let ((default-directory root))
              (ice-tests--no-prompts
               (ygg-ice-c4-readme)
               (let ((deadline (+ (float-time) 10)))
                 (while (and (not drawn) (< (float-time) deadline))
                   (accept-process-output nil 0.05)))))
            (should (eq drawn buf))
            (should (equal (with-current-buffer buf (buffer-string)) "# shop\n\nexported\n"))
            (should (equal (with-temp-buffer
                             (insert-file-contents (expand-file-name "docs/arch/args" root))
                             (buffer-string))
                           (format "export markdown %sdocs/arch\n" root))))
        (kill-buffer buf)))))

(ert-deftest ice-likec4-files-open-in-the-likec4-mode ()
  (should (eq (assoc-default "docs/arch/model.c4" auto-mode-alist #'string-match)
              'ygg-likec4-mode)))

;;; The owner's approval of the checkpoints

(defconst ice-tests--checkpoints
  "# Tasks\n\n## Checkpoints\n\n1. Cart shows a sum  \n2. Tax\tline added\n\nApproved:\n\n## Slices\n\n- [ ] 1. a\n  - Pass when: x\n  - Evidence: y\n- [ ] 2. b\n  - Pass when: x\n  - Evidence: y\n"
  "A tasks.md with two checkpoints, trailing blanks and a tab, not yet approved.")

(defmacro ice-tests--with-tasks (var text &rest body)
  "Run BODY with VAR a change whose tasks.md is TEXT, in a fresh repo."
  (declare (indent 2))
  `(ice-tests--with-repo root (list (cons "openspec/changes/cart/tasks.md" ,text))
     (let ((,var (list :name "cart" :dir (expand-file-name "openspec/changes/cart/" root)
                       :tasks (expand-file-name "openspec/changes/cart/tasks.md" root))))
       (unwind-protect (progn ,@body)
         (when-let* ((buf (find-buffer-visiting (plist-get ,var :tasks))))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf))))))

(defun ice-tests--tasks-of (change)
  (with-temp-buffer
    (insert-file-contents (plist-get change :tasks))
    (buffer-string)))

(defun ice-tests--checkpoint-gaps (change)
  "ice-check plan's lines about CHANGE's checkpoints, the expect gaps left out."
  (with-temp-buffer
    (setq default-directory (plist-get change :dir))
    (call-process "python3" nil t nil ice-tests--check-script "plan"
                  (directory-file-name (plist-get change :dir)))
    (seq-filter (lambda (line) (string-match-p "checkpoint\\|approv" line))
                (split-string (buffer-string) "\n" t))))

(defun ice-tests--python-checkpoints-hash (file)
  "ice-check's checkpoints_hash over FILE's Checkpoints section."
  (with-temp-buffer
    (setq default-directory (file-name-directory file))
    (call-process "python3" nil t nil "-c"
                  (concat "import importlib.machinery as m, importlib.util as u, sys\n"
                          "sys.dont_write_bytecode = True\n"
                          "l = m.SourceFileLoader('c', sys.argv[1]); c = u.module_from_spec(u.spec_from_loader('c', l)); l.exec_module(c)\n"
                          "print(c.checkpoints_hash(c.h2_sections(c.read(sys.argv[2]))['checkpoints']))")
                  ice-tests--check-script file)
    (string-trim (buffer-string))))

(ert-deftest ice-approve-checkpoints-hashes-what-ice-check-hashes ()
  (ice-tests--with-tasks change ice-tests--checkpoints
    (should (member (format "ice-check plan %s: not approved: the owner approves the checkpoints with SPC a k A"
                            (directory-file-name (plist-get change :dir)))
                    (ice-tests--checkpoint-gaps change)))
    (let (shown)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) (setq shown (with-current-buffer "*ice: checkpoints cart*" (buffer-string))) t)))
        (should (equal (ygg-ice-approve-checkpoints change) (format-time-string "%Y-%m-%d"))))
      (should (equal shown "1. Cart shows a sum\n2. Tax\tline added\n")))
    (let ((hash (ice-tests--python-checkpoints-hash (plist-get change :tasks))))
      (should (string-match-p "\\`[0-9a-f]\\{8\\}\\'" hash))
      (should (equal (ice-tests--tasks-of change)
                     (string-replace "Approved:\n" (format "Approved: %s sha1:%s\n" (format-time-string "%Y-%m-%d") hash)
                                     ice-tests--checkpoints)))
      (with-current-buffer (find-file-noselect (plist-get change :tasks))
        (should (equal (ygg-ice--checkpoints-hash) hash))))
    (should-not (ice-tests--checkpoint-gaps change))
    (with-temp-file (plist-get change :tasks)
      (insert (string-replace "2. Tax\tline added" "2. Tax line added" (ice-tests--tasks-of change))))
    (should (equal (ice-tests--checkpoint-gaps change)
                   (list (format "ice-check plan %s: checkpoints changed since approval: the owner approves the checkpoints with SPC a k A"
                                 (directory-file-name (plist-get change :dir))))))
    (cl-letf (((symbol-function 'y-or-n-p) #'always))
      (ygg-ice-approve-checkpoints change))
    (should-not (ice-tests--checkpoint-gaps change))
    (should (= 1 (with-temp-buffer
                   (insert (ice-tests--tasks-of change))
                   (how-many "^Approved:" (point-min) (point-max)))))))

(ert-deftest ice-approve-checkpoints-writes-nothing-on-no-and-refuses-without-a-list ()
  (ice-tests--with-tasks change ice-tests--checkpoints
    (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
      (should-not (ygg-ice-approve-checkpoints change)))
    (should (equal (ice-tests--tasks-of change) ice-tests--checkpoints)))
  (ice-tests--with-tasks change "# Tasks\n\n- [ ] 1. a\n"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
      (should-error (ygg-ice-approve-checkpoints change) :type 'user-error))))

(ert-deftest ice-check-plan-holds-the-checkpoint-list-to-its-shape ()
  (ice-tests--with-tasks change
      (concat "# Tasks\n\n## Checkpoints\n\n- [ ] 1. a checkbox\n2. one two three four five six seven eight nine\n\n"
              "Approved:\n\n## Slices\n\n- [ ] 1. a\n  - Pass when: x\n  - Evidence: y\n")
    (let ((dir (directory-file-name (plist-get change :dir))))
      (should (equal (ice-tests--checkpoint-gaps change)
                     (mapcar (lambda (gap) (format "ice-check plan %s: %s" dir gap))
                             '("checkpoint line is not 'N. few words' (never a checkbox): - [ ] 1. a checkbox"
                               "checkpoint 2 has 9 words, at most 8"
                               "checkpoints are not numbered 1..1 in order"
                               "not approved: the owner approves the checkpoints with SPC a k A"
                               "1 checkpoints but 2 slices: one checkpoint per slice"))))))
  (ice-tests--with-tasks change "# Tasks\n\n## Slices\n\n- [ ] 1. a\n\n## Checkpoints\n\n1. a\n"
    (should (equal (ice-tests--checkpoint-gaps change)
                   (list (format "ice-check plan %s: '## Checkpoints' must be the first h2 of tasks.md"
                                 (directory-file-name (plist-get change :dir))))))))

(ert-deftest ice-check-plan-wants-one-checkpoint-per-slice ()
  (ice-tests--with-tasks change (string-replace "2. Tax\tline added\n" "" ice-tests--checkpoints)
    (with-temp-buffer
      (setq default-directory (plist-get change :dir))
      (call-process "python3" nil t nil ice-tests--check-script "plan"
                    (directory-file-name (plist-get change :dir)))
      (should (string-match-p ": 1 checkpoints but 2 slices: one checkpoint per slice$" (buffer-string))))))

(ert-deftest ice-check-reviews-passes-on-two-approvals-in-the-last-round-and-an-approved-ui ()
  (ice-tests--with-repo root
      '(("c/reviews/code-1.md" . "## Round 1, reviewer A\n\nVerdict: blockers 1\n\n## Round 1, reviewer B\n\nVerdict: approve\n\n## Round 2, reviewer A\n\nVerdict: approve\n\n## Round 2, reviewer B\n\nVerdict: approve\n")
        ("c/reviews/code-2.md" . "## Round 1, reviewer A\n\nVerdict: approve\n\n## Round 1, reviewer B\n\nVerdict: blockers 2\n")
        ("c/reviews/code-3.md" . "## Round 1, reviewer A\n\nVerdict: approve\n\n## Round 1, reviewer B\n\nVerdict: approve\n")
        ("c/reviews/ui-3.md" . "## Round 1\n\nVerdict: approve\n\n## Round 2\n\nVerdict: blockers 1\n"))
    (let ((run (lambda (n)
                 (with-temp-buffer
                   (setq default-directory root)
                   (list (call-process "python3" nil t nil ice-tests--check-script "reviews"
                                       (expand-file-name "c" root) n)
                         (string-trim (buffer-string)))))))
      (should (equal (car (funcall run "1")) 0))
      (should (equal (funcall run "2")
                     (list 1 (format "ice-check reviews %sc: code review not approved: the last round of reviews/code-2.md ends on approve, blockers 2; both reviewers must approve" root))))
      (should (equal (funcall run "3")
                     (list 1 (format "ice-check reviews %sc: ui review not approved: the last verdict of reviews/ui-3.md is blockers 1" root))))
      (should (equal (car (funcall run "4")) 1)))))

;;; The test runner: found at wiring, run once as the baseline

(defconst ice-tests--runner-script
  (expand-file-name "../etc/ice/ice-runner" (file-name-directory (or load-file-name buffer-file-name)))
  "This checkout's ice-runner.")

(defun ice-tests--detect (files)
  "What ice-runner detect finds in a fresh repository holding FILES."
  (ice-tests--with-repo root files
    (let ((default-directory root))
      (call-process "git" nil nil nil "init" "-q")
      (with-temp-buffer
        (call-process "python3" nil t nil ice-tests--runner-script "detect" root)
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist :null-object nil)))))

(ert-deftest ice-runner-detects-pytest-vitest-cargo-ert-and-says-when-it-cannot ()
  (let ((pytest (ice-tests--detect '(("tests/test_cart.py" . "def test_a():\n    pass\n"))))
        (vitest (ice-tests--detect '(("package.json" . "{\"devDependencies\": {\"vitest\": \"^2.0.0\"}}\n"))))
        (cargo (ice-tests--detect '(("Cargo.toml" . "[package]\nname = \"cart\"\n"))))
        (ert (ice-tests--detect '(("test/cart-tests.el" . "(ert-deftest a () t)\n"))))
        (unknown (ice-tests--detect '(("README.md" . "cart\n")))))
    (should (equal (alist-get 'runner pytest) "pytest"))
    (should (equal (alist-get 'test_cmd pytest)
                   "python3 -m pytest -q -p no:cacheprovider {file}::{filter} --junitxml={report}"))
    (should (equal (alist-get 'runner vitest) "vitest"))
    (should (string-match-p "\\`npx vitest run {file} --testNamePattern={filter} .*{report}" (alist-get 'test_cmd vitest)))
    (should (equal (alist-get 'runner cargo) "cargo"))
    (should (equal (alist-get 'test_cmd cargo) "cargo test {filter}"))
    (should (equal (alist-get 'lock_paths cargo) "Cargo.toml"))
    (should (string-match-p "writes no JUnit report" (aref (alist-get 'asks cargo) 0)))
    (should (equal (alist-get 'runner ert) "ert"))
    (should (string-match-p "{filter}" (alist-get 'test_cmd ert)))
    (should (string-match-p "{report}" (alist-get 'test_cmd ert)))
    (should-not (alist-get 'runner unknown))
    (should-not (alist-get 'test_cmd unknown))
    (should (string-match-p "\\`no test runner found" (aref (alist-get 'asks unknown) 0)))))

(ert-deftest ice-runner-fills-an-untouched-config-and-never-an-edited-one ()
  (ice-tests--with-repo root '(("tests/test_cart.py" . "def test_a():\n    pass\n"))
    (let ((default-directory root)
          (config (expand-file-name ".ice/config" root)))
      (call-process "git" nil nil nil "init" "-q")
      (should (= 0 (call-process "python3" nil nil nil ice-tests--runner-script "config" root)))
      (should (string-match-p "^test_cmd = python3 -m pytest" (with-temp-buffer (insert-file-contents config) (buffer-string))))
      (with-temp-file config (insert "test_cmd = mine {file} {filter}\n"))
      (call-process "python3" nil nil nil ice-tests--runner-script "config" root)
      (should (equal (with-temp-buffer (insert-file-contents config) (buffer-string))
                     "test_cmd = mine {file} {filter}\n"))))
  (ice-tests--with-repo root '(("README.md" . "cart\n"))
    (let ((default-directory root))
      (call-process "git" nil nil nil "init" "-q")
      (with-temp-buffer
        (call-process "python3" nil t nil ice-tests--runner-script "config" root)
        (should (string-match-p "^ask: no test runner found" (buffer-string))))
      (should-not (with-temp-buffer
                    (insert-file-contents (expand-file-name ".ice/config" root))
                    (re-search-forward "^test_cmd" nil t))))))

(ert-deftest ice-runner-baseline-runs-a-real-pytest-suite-once-inside-the-repo ()
  (skip-unless (= 0 (call-process "python3" nil nil nil "-m" "pytest" "--version")))
  (ice-tests--with-repo root '(("cart.py" . "def total(items):\n    return sum(items)\n")
                               ("tests/conftest.py" . "import os, sys\nsys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))\n")
                               ("tests/test_cart.py" . "from cart import total\n\n\ndef test_total():\n    assert total([1, 2]) == 3\n\n\ndef test_empty():\n    assert total([]) == 0\n"))
    (let ((default-directory root)
          (record (expand-file-name ".ice/state/baseline.json" root)))
      (call-process "git" nil nil nil "init" "-q")
      (call-process "python3" nil nil nil ice-tests--runner-script "config" root)
      (with-temp-buffer
        (call-process "python3" nil t nil ice-tests--runner-script "baseline" root)
        (should (string-match-p "^note: .ice/state/baseline.json recorded: passed (2 tests)" (buffer-string))))
      (let ((json (with-temp-buffer (insert-file-contents record)
                                    (json-parse-buffer :object-type 'alist :null-object nil))))
        (should (equal (alist-get 'status json) "passed"))
        (should (equal (alist-get 'tests (alist-get 'counts json)) 2))
        (should (equal (alist-get 'log json) ".ice/state/baseline.log"))
        (should (file-exists-p (expand-file-name ".ice/state/baseline.log" root))))
      (should-not (file-exists-p (expand-file-name ".ice/state/baseline-sandbox" root)))
      (should-not (directory-files-recursively root "__pycache__\\|\\.pytest_cache" t))
      (let ((before (with-temp-buffer (insert-file-contents record) (buffer-string))))
        (with-temp-buffer
          (call-process "python3" nil t nil ice-tests--runner-script "baseline" root)
          (should (string-match-p "already recorded: passed" (buffer-string))))
        (should (equal before (with-temp-buffer (insert-file-contents record) (buffer-string)))))
      (with-temp-file (expand-file-name "tests/test_cart.py" root)
        (insert "def test_red():\n    assert False\n"))
      (with-temp-buffer
        (call-process "python3" nil t nil ice-tests--runner-script "baseline" root "--force")
        (should (string-match-p "^ask: the baseline suite is failed" (buffer-string)))))))

;;; Import: skills and ice are extras

(ert-deftest ice-import-step-wires-or-rewires-out-of-sight ()
  (dolist (wired (list t nil))
    (ice-tests--with-repo root (if wired '((".ice/config" . "test_cmd = x\n")) '(("README.md" . "x\n")))
      (let (calls)
        (cl-letf (((symbol-function 'ygg-ice--script) #'identity)
                  ((symbol-function 'ygg-ice--compile)
                   (lambda (dir command what &optional codex quiet)
                     (push (list dir (and (string-search "--rebaseline" command) t) what codex quiet) calls))))
          (should (ygg-ice-import-step root))
          (should (equal calls (list (list root wired "wire" t t)))))))))

(ert-deftest ice-quiet-run-says-how-it-ended-and-counts-new-decisions ()
  (with-temp-buffer
    (rename-buffer "*ice: wire*" t)
    (insert "Owner decides (new since the last run):\n- [ ] one\n- [ ] two\n\nCompilation finished\n")
    (let (said)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (ygg-ice--say-finished (current-buffer) "finished\n"))
      (should (string-match-p "finished, 2 new decisions" said)))))

(ert-deftest ice-import-runs-extras-only-when-picked ()
  (require 'ygg-project-scan)
  (let (steps)
    (cl-letf (((symbol-function 'ygg-project-import--run) (lambda (_root s _cb) (setq steps s))))
      (ygg-project-import "/tmp/cart/")
      (should-not (assoc "ice" steps))
      (should-not (assoc "agent skills" steps))
      (should (assoc "skills" steps))
      (ygg-project-import "/tmp/cart/" nil '("ice"))
      (should (equal (car (car (last steps))) "ice"))
      (should-not (assoc "agent skills" steps))
      (ygg-project-import "/tmp/cart/" nil '("skills" "ice"))
      (should (assoc "agent skills" steps))
      (should (assoc "ice" steps)))
    (let (stepped installed)
      (cl-letf (((symbol-function 'ygg-ice-import-step) (lambda (root) (push root stepped)))
                ((symbol-function 'ygg-agent-skills-ensure) (lambda () (setq installed t)))
                ((symbol-function 'ygg-agent-link-project-skills) #'ignore))
        (funcall (cdr (assoc "ice" steps)))
        (funcall (cdr (assoc "agent skills" steps)))
        (funcall (cdr (assoc "skills" steps))))
      (should (equal stepped '("/tmp/cart/")))
      (should installed))))

(ert-deftest ice-import-reinstalls-stale-skills-and-leaves-fresh-ones ()
  (require 'ygg-agent-skills)
  (let* ((src (make-temp-file "ice-skills-src" t))
         (dst (make-temp-file "ice-skills-dst" t))
         (ygg-agent-skills-root src)
         (ygg-agent--shared-skills dst)
         (installs 0))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-agent-skill-install) (lambda () (cl-incf installs)))
                  ((symbol-function 'ygg-agent--notify) #'ignore))
          (dolist (dir (list src dst))
            (make-directory (expand-file-name "cart" dir))
            (with-temp-file (expand-file-name "cart/SKILL.md" dir) (insert "v1\n")))
          (set-file-times (expand-file-name "cart/SKILL.md" src) (time-subtract nil 60))
          (ygg-agent-skills-ensure)
          (should (= installs 0))
          (set-file-times (expand-file-name "cart/SKILL.md" src) (time-add nil 60))
          (should (equal (ygg-agent--skills-stale) '("cart")))
          (ygg-agent-skills-ensure)
          (should (= installs 1)))
      (delete-directory src t)
      (delete-directory dst t))))

;;; Slices removed once their code is written

(defconst ice-tests--ledger-head "ts\tchange\tbranch\tsha\tverdict\tevidence\thash\tslice\tfiles\n")

(defun ice-tests--done-row (change slice files)
  (format "2026-10-01T00:00:00+00:00\t%s\tmain\tabc\tslice-done\t.ice/evidence/x\th\t%s\t%s\n" change slice files))

(ert-deftest ice-check-plan-counts-a-slice-the-ledger-removed-as-done ()
  (ice-tests--with-tasks change (replace-regexp-in-string "- \\[ \\] 2\\. b\n.*\n.*\n" "" ice-tests--checkpoints)
    (should-not (seq-filter (lambda (gap) (string-match-p "slices" gap)) (ice-tests--checkpoint-gaps change)))
    (let ((root (expand-file-name "../../../" (plist-get change :dir))))
      (make-directory (expand-file-name ".ice" root) t)
      (with-temp-file (expand-file-name ".ice/ledger.tsv" root) (insert ice-tests--ledger-head))
      (should (string-match-p ": 2 checkpoints but 1 slices" (string-join (ice-tests--checkpoint-gaps change) "\n")))
      (with-temp-file (expand-file-name ".ice/ledger.tsv" root)
        (insert ice-tests--ledger-head (ice-tests--done-row "cart" "2" "src/tax.py")))
      (should-not (seq-filter (lambda (gap) (string-match-p "slices" gap)) (ice-tests--checkpoint-gaps change)))
      (with-temp-file (plist-get change :tasks)
        (insert (replace-regexp-in-string "- \\[ \\] 1\\. a\n.*\n.*\n" "" (ice-tests--tasks-of change))))
      (should (string-match-p ": 2 checkpoints but 1 slices" (string-join (ice-tests--checkpoint-gaps change) "\n")))
      (with-temp-file (expand-file-name ".ice/ledger.tsv" root)
        (insert ice-tests--ledger-head (ice-tests--done-row "cart" "2" "src/tax.py")
                (ice-tests--done-row "cart" "1" "src/cart.py") (ice-tests--done-row "other" "3" "x")))
      (should-not (seq-filter (lambda (gap) (string-match-p "slices" gap)) (ice-tests--checkpoint-gaps change))))))

(ert-deftest ice-changes-progress-counts-removed-slices-as-done ()
  (ice-tests--with-repo root
      (list (cons "openspec/changes/cart/tasks.md" "## Slices\n\n- [ ] 3. c\n")
            (cons ".ice/ledger.tsv" (concat ice-tests--ledger-head (ice-tests--done-row "cart" "1" "a")
                                            (ice-tests--done-row "cart" "2" "b") (ice-tests--done-row "cart" "2" "b")
                                            (ice-tests--done-row "other" "1" "z"))))
    (let ((cart (car (ygg-ice-changes root))))
      (should (equal (list (plist-get cart :done) (plist-get cart :total)) '(2 3))))))

;;; ice-lat-drift

(defconst ice-tests--lat-drift-script
  (expand-file-name "ice-lat-drift" (file-name-directory ice-tests--check-script)))

(defun ice-tests--lat-drift (root &rest args)
  "ice-lat-drift ARGS in ROOT as (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory root))
      (cons (apply #'call-process "python3" nil t nil ice-tests--lat-drift-script args) (buffer-string)))))

(ert-deftest ice-lat-drift-reports-a-section-left-behind-a-listed-exemption-and-a-missing-path ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-tests--with-repo root
      '(("lat.md/cart.md" . "# Cart\n\nThe cart.\n\n## Total\n\nSums lines, see `lisp/cart.el`.\n\n## Tax\n\nTax is [[lisp/tax.el]] and `lisp/gone.el`.\n")
        ("lat.md/changes.md" . "# Changes\n\n## old\n\n- Where: `lisp/removed.el`, `lisp/cart.el`\n")
        ("lisp/cart.el" . "(defun cart ())\n")
        ("lisp/tax.el" . "(defun tax ())\n")
        ("openspec/changes/cart/intent.md" . "## What is wanted\n\nx\n"))
    (let ((process-environment (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t" "GIT_COMMITTER_NAME=t"
                                         "GIT_COMMITTER_EMAIL=t@t" "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment))
          (default-directory root))
      (should (= 0 (call-process "git" nil nil nil "init" "-q" "-b" "main")))
      (should (= 0 (call-process "git" nil nil nil "add" "-A")))
      (should (= 0 (call-process "git" nil nil nil "commit" "-q" "-m" "base")))
      (with-temp-file (expand-file-name "lisp/cart.el" root) (insert "(defun cart () 2)\n"))
      (with-temp-file (expand-file-name "lisp/tax.el" root) (insert "(defun tax () 2)\n"))
      (should (equal (ice-tests--lat-drift root "cart")
                     (cons 1 (concat "lat.md/cart.md:5: section cart#Total links lisp/cart.el, which changed; lat.md did not (cart)\n"
                                     "lat.md/cart.md:9: section cart#Tax links lisp/tax.el, which changed; lat.md did not (cart)\n"
                                     "lat.md/cart.md:11: `lisp/gone.el` names a path that does not exist\n"))))
      (with-temp-file (expand-file-name "openspec/changes/cart/design.md" root)
        (insert "## Decisions\n\n- lat unchanged: [[cart#Tax]] (a constant moved)\n"))
      (with-temp-file (expand-file-name "lat.md/cart.md" root)
        (insert "# Cart\n\nThe cart.\n\n## Total\n\nSums lines in cents, see `lisp/cart.el`.\n\n## Tax\n\nTax is [[lisp/tax.el]] and `lisp/gone.el`.\n"))
      (should (equal (ice-tests--lat-drift root "cart")
                     '(1 . "lat.md/cart.md:11: `lisp/gone.el` names a path that does not exist\n")))
      (with-temp-file (expand-file-name "lat.md/cart.md" root)
        (insert "# Cart\n\nThe cart.\n\n## Total\n\nSums lines in cents, see `lisp/cart.el`.\n\n## Tax\n\nTax is [[lisp/tax.el]].\n"))
      (should (equal (ice-tests--lat-drift root)
                     '(0 . "ice-lat-drift: ok, 1 lat.md file(s) match the code\n"))))))

(defmacro ice-tests--with-drift-repo (root files &rest body)
  "Run BODY in ROOT, a git repo committing FILES on main; identity and config kept local."
  (declare (indent 2))
  `(ice-tests--with-repo ,root ,files
     (let ((process-environment (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t" "GIT_COMMITTER_NAME=t"
                                          "GIT_COMMITTER_EMAIL=t@t" "GIT_CONFIG_NOSYSTEM=1")
                                        process-environment))
           (coding-system-for-read 'utf-8)
           (default-directory ,root))
       (should (= 0 (call-process "git" nil nil nil "init" "-q" "-b" "main")))
       (should (= 0 (call-process "git" nil nil nil "add" "-A")))
       (should (= 0 (call-process "git" nil nil nil "commit" "-q" "-m" "base")))
       ,@body)))

(ert-deftest ice-lat-drift-without-a-change-honours-an-exemption-from-any-open-change ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-tests--with-drift-repo root
      '(("lat.md/cart.md" . "# Cart\n\nThe cart.\n\n## Tax\n\nTax is `lisp/tax.el`.\n")
        ("lisp/tax.el" . "(defun tax ())\n")
        ("openspec/changes/a/design.md" . "- lat unchanged: [[cart#Tax]] (a constant moved)\n")
        ("openspec/changes/b/design.md" . "# Design\n"))
    (with-temp-file (expand-file-name "lisp/tax.el" root) (insert "(defun tax () 2)\n"))
    (should (equal (ice-tests--lat-drift root) '(0 . "ice-lat-drift: ok, 1 lat.md file(s) match the code\n")))))

(ert-deftest ice-lat-drift-refuses-a-lock-base-git-cannot-resolve ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-tests--with-drift-repo root
      '(("lat.md/cart.md" . "# Cart\n\nThe cart.\n\n## Tax\n\nTax is `lisp/tax.el`.\n")
        ("lisp/tax.el" . "(defun tax ())\n")
        ("openspec/changes/cart/design.md" . "# Design\n"))
    (with-temp-buffer
      (insert "-----BEGIN ICE LOCK RECORD-----\n{\"base\": \"0123456789abcdef0123456789abcdef01234567\"}\n-----END ICE LOCK RECORD-----\n")
      (should (= 0 (call-process-region (point-min) (point-max) "git" nil nil nil
                                        "tag" "-a" "--cleanup=verbatim" "-F" "-" "ice-expect/cart" "HEAD"))))
    (with-temp-file (expand-file-name "lisp/tax.el" root) (insert "(defun tax () 2)\n"))
    (should (= 2 (car (ice-tests--lat-drift root "cart"))))))

(ert-deftest ice-lat-drift-matches-non-ascii-paths ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-tests--with-drift-repo root
      '(("lat.md/cart.md" . "# Cart\n\nThe cart.\n\n## Tax\n\nTax is `lisp/täx.el`.\n")
        ("lisp/täx.el" . "(defun tax ())\n"))
    (with-temp-file (expand-file-name "lisp/täx.el" root) (insert "(defun tax () 2)\n"))
    (should (equal (ice-tests--lat-drift root)
                   '(1 . "lat.md/cart.md:5: section cart#Tax links lisp/täx.el, which changed; lat.md did not\n")))))

(ert-deftest ice-lat-drift-reads-hunks-past-color-and-an-external-diff ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-tests--with-drift-repo root
      '(("lat.md/cart.md" . "# Cart\n\nThe cart.\n\n## Tax\n\nTax is `lisp/tax.el`.\n")
        ("lisp/tax.el" . "(defun tax ())\n"))
    (should (= 0 (call-process "git" nil nil nil "config" "color.ui" "always")))
    (should (= 0 (call-process "git" nil nil nil "config" "diff.external" "true")))
    (with-temp-file (expand-file-name "lisp/tax.el" root) (insert "(defun tax () 2)\n"))
    (with-temp-file (expand-file-name "lat.md/cart.md" root)
      (insert "# Cart\n\nThe cart.\n\n## Tax\n\nTax, in cents, is `lisp/tax.el`.\n"))
    (should (equal (ice-tests--lat-drift root) '(0 . "ice-lat-drift: ok, 1 lat.md file(s) match the code\n")))))

(ert-deftest ice-lat-drift-skips-a-gitignored-path ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-tests--with-drift-repo root
      '((".gitignore" . "dist/\n")
        ("lat.md/cart.md" . "# Cart\n\nBuilt into `dist/app.js`; source `lisp/gone.el`.\n"))
    (should (equal (ice-tests--lat-drift root)
                   '(1 . "lat.md/cart.md:3: `lisp/gone.el` names a path that does not exist\n")))))

(provide 'ice-layer-tests)
;;; ice-layer-tests.el ends here
