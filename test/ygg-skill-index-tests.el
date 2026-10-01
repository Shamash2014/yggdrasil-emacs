;;; ygg-skill-index-tests.el --- the skill index, its tools and the listing diet -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ygg-skill-index)
(require 'ygg-agent-conf)
(require 'aob-mcp)
(require 'aob-mcp-tools)
(require 'aob-mcp-host)

(defvar ygg-skill-index-tests--root nil)

(defmacro ygg-skill-index-tests--with (&rest body)
  "Run BODY against a scratch Claude home, agents dir and index file."
  (declare (indent 0))
  `(let* ((ygg-skill-index-tests--root (make-temp-file "skill-index-" t))
          (ygg-skill-index-claude-home (expand-file-name "claude" ygg-skill-index-tests--root))
          (ygg-skill-index-agents-dir (expand-file-name "agents/skills" ygg-skill-index-tests--root))
          (ygg-skill-index-file (expand-file-name "var/skill-index.json" ygg-skill-index-tests--root))
          (ygg-skill-index--roots nil)
          (ygg-skill-index--terms (make-hash-table :test #'equal))
          (process-environment (cons "CLAUDE_CODE_PLUGIN_CACHE_DIR" process-environment)))
     (make-directory (expand-file-name "skills" ygg-skill-index-claude-home) t)
     (make-directory ygg-skill-index-agents-dir t)
     (unwind-protect (progn ,@body)
       (delete-directory ygg-skill-index-tests--root t))))

(defun ygg-skill-index-tests--file (rel content)
  (let ((path (expand-file-name rel ygg-skill-index-tests--root)))
    (make-directory (file-name-directory path) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file path (insert content)))
    path))

(defun ygg-skill-index-tests--skill (name description &optional body extra)
  "A user skill NAME saying DESCRIPTION; EXTRA is more frontmatter."
  (ygg-skill-index-tests--file
   (format "claude/skills/%s/SKILL.md" name)
   (format "---\nname: %s\ndescription: %s\n%s---\n\n%s\n" name description
           (or extra "") (or body (format "# %s\n\nDo the %s thing." name name)))))

(defun ygg-skill-index-tests--names (&optional project)
  (mapcar (lambda (s) (plist-get s :name)) (ygg-skill-index-skills project)))

(defun ygg-skill-index-tests--touch (file seconds)
  "Move FILE's time SECONDS into the future, as an edit would."
  (set-file-times file (time-add nil seconds)))

(ert-deftest ygg-skill-index-builds-every-root-and-keeps-it ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--skill "alpha" "First skill")
    (ygg-skill-index-tests--file "agents/skills/beta/SKILL.md"
                                 "---\nname: beta\ndescription: >\n  Folded\n  over lines\n---\nbody\n")
    (ygg-skill-index-tests--file "proj/skills/gamma/SKILL.md"
                                 "---\ndescription: \"Quoted — dashed\"\n---\nbody\n")
    (ygg-skill-index-tests--file "claude/skills/synced/acct/delta/SKILL.md"
                                 "---\nname: delta\ndescription: Synced one\n---\nbody\n")
    (ygg-skill-index-tests--file "claude/skills/hidden/SKILL.md"
                                 "---\nname: hidden\ndescription: Not for models\ndisable-model-invocation: true\n---\n")
    (let* ((project (expand-file-name "proj" ygg-skill-index-tests--root))
           (skills (ygg-skill-index-skills project)))
      (should (equal (sort (ygg-skill-index-tests--names project) #'string<)
                     '("alpha" "anthropic-skills:delta" "beta" "gamma")))
      (should (equal "Folded over lines"
                     (plist-get (seq-find (lambda (s) (equal (plist-get s :name) "beta")) skills)
                                :description)))
      (should (equal "Quoted — dashed"
                     (plist-get (seq-find (lambda (s) (equal (plist-get s :name) "gamma")) skills)
                                :description)))
      (should (member "hidden" (mapcar (lambda (s) (plist-get s :name))
                                       (ygg-skill-index-skills project t))))
      (should (file-exists-p ygg-skill-index-file)))))

(ert-deftest ygg-skill-index-rebuilds-only-what-moved ()
  (ygg-skill-index-tests--with
    (let ((file (ygg-skill-index-tests--skill "alpha" "Old words"))
          (reads 0))
      (ygg-skill-index-tests--file "agents/skills/beta/SKILL.md"
                                   "---\nname: beta\ndescription: Steady\n---\n")
      (let ((read (symbol-function 'ygg-skill-index-read)))
        (cl-letf (((symbol-function 'ygg-skill-index-read)
                   (lambda (&rest args) (cl-incf reads) (apply read args))))
          (ygg-skill-index-skills)
          (should (= reads 2))
          (ygg-skill-index-skills)
          (should (= reads 2))
          (setq ygg-skill-index--roots nil)
          (ygg-skill-index-skills)
          (should (= reads 2))
          (ygg-skill-index-tests--file "claude/skills/alpha/SKILL.md"
                                       "---\nname: alpha\ndescription: New words\n---\n")
          (ygg-skill-index-tests--touch file 5)
          (should (equal "New words"
                         (plist-get (car (ygg-skill-index-skills)) :description)))
          (should (= reads 3))
          (ygg-skill-index-tests--skill "added" "Arrived later")
          (ygg-skill-index-tests--touch
           (expand-file-name "skills" ygg-skill-index-claude-home) 10)
          (should (member "added" (ygg-skill-index-tests--names))))))))

(ert-deftest ygg-skill-index-search-ranks-the-fitting-skill-first ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--skill "create-pr" "Prepare and open a pull request for the current branch")
    (ygg-skill-index-tests--skill "debug-mantra" "Use when a test is failing or a stack trace is pasted")
    (ygg-skill-index-tests--skill "dataviz" "Make a chart, graph or plot of data")
    (ygg-skill-index-tests--skill "unslop" "Cut AI tells from any writing")
    (ygg-skill-index-tests--skill "railway" "Deploy services to Railway")
    (should (equal "create-pr"
                   (plist-get (car (ygg-skill-index-search "open a pull request for this branch"))
                              :name)))
    (should (equal "debug-mantra"
                   (plist-get (car (ygg-skill-index-search "this test is failing with a stack trace"))
                              :name)))
    (should (equal "dataviz"
                   (plist-get (car (ygg-skill-index-search "a chart of weekly signups")) :name)))
    (let ((hit (car (ygg-skill-index-search "deploy to railway" 1))))
      (should (equal '(:name :description :path :score)
                     (cl-loop for (k _) on hit by #'cddr collect k)))
      (should (> (plist-get hit :score) 0)))
    (should (= 1 (length (ygg-skill-index-search "deploy a pull request chart" 1))))
    (should-not (ygg-skill-index-search "zebra"))))

(ert-deftest ygg-skill-index-hides-what-the-settings-turn-off ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--skill "kept" "Kept around")
    (ygg-skill-index-tests--skill "gone" "Turned off")
    (ygg-skill-index-tests--skill "asked" "Only when asked")
    (ygg-skill-index-tests--file
     "claude/settings.json"
     "{\"skillOverrides\":{\"gone\":\"off\",\"asked\":\"user-invocable-only\",\"kept\":\"name-only\"}}")
    (should (equal '("kept") (ygg-skill-index-tests--names)))))

(defun ygg-skill-index-tests--call (tool args)
  (let (sent)
    (cl-letf (((symbol-function 'aob-mcp--send) (lambda (_conn obj) (push obj sent))))
      (aob-mcp--call 'conn 7 (list :name tool :arguments args)))
    (plist-get (car sent) :result)))

(defun ygg-skill-index-tests--text (result)
  (plist-get (aref (plist-get result :content) 0) :text))

(ert-deftest ygg-skill-index-load-gives-body-and-files ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--skill "alpha" "First skill" "# Alpha\n\nRead references/guide.md.")
    (ygg-skill-index-tests--file "claude/skills/alpha/references/guide.md" "guide")
    (ygg-skill-index-tests--file "claude/skills/alpha/scripts/run.sh" "echo")
    (let ((text (ygg-skill-index-tests--text
                 (ygg-skill-index-tests--call "skill_load" '(:name "alpha")))))
      (should (string-prefix-p "# Alpha\n\nRead references/guide.md." text))
      (should-not (string-match-p "description: First skill" text))
      (should (string-match-p "^- references/guide\\.md$" text))
      (should (string-match-p "^- scripts/run\\.sh$" text))
      (should-not (string-match-p "^- SKILL\\.md$" text)))))

(ert-deftest ygg-skill-index-load-of-an-unknown-name-offers-five ()
  (ygg-skill-index-tests--with
    (dolist (name '("create-pr" "create-issue" "review" "debug" "deploy" "chart" "zzz-far"))
      (ygg-skill-index-tests--skill name (format "The %s skill" name)))
    (ygg-skill-index-tests--file "claude/skills/synced/acct/docx/SKILL.md"
                                 "---\nname: docx\ndescription: Word files\n---\nWord body\n")
    (let ((text (ygg-skill-index-tests--text
                 (ygg-skill-index-tests--call "skill_load" '(:name "create")))))
      (should (string-prefix-p "no skill named create; closest: " text))
      (let ((offered (split-string (substring text (length "no skill named create; closest: "))
                                   ", ")))
        (should (= 5 (length offered)))
        (should (equal '("create-pr" "create-issue")
                       (sort (seq-take offered 2) #'string>)))
        (should-not (member "zzz-far" offered))))
    (should (string-prefix-p "Word body"
                             (ygg-skill-index-tests--text
                              (ygg-skill-index-tests--call "skill_load" '(:name "docx")))))))

(ert-deftest ygg-skill-index-search-tool-answers-structured ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--skill "create-pr" "Open a pull request — with a dash")
    (let* ((result (ygg-skill-index-tests--call "skill_search" '(:query "pull request" :k 2)))
           (hits (plist-get (plist-get result :structuredContent) :results)))
      (should (vectorp hits))
      (should (equal "create-pr" (plist-get (aref hits 0) :name)))
      (should (string-match-p "\"name\":\"create-pr\"" (ygg-skill-index-tests--text result)))
      (should (string-match-p "—" (ygg-skill-index-tests--text result)))
      (should (string-match-p "—" (decode-coding-string
                                   (json-serialize (list :result result)) 'utf-8))))))

(ert-deftest ygg-skill-index-tools-are-always-loaded ()
  (let (sent)
    (cl-letf (((symbol-function 'aob-mcp--send) (lambda (_conn obj) (push obj sent))))
      (aob-mcp--dispatch 'conn '(:jsonrpc "2.0" :id 1 :method "tools/list")))
    (seq-doseq (tool (plist-get (plist-get (car sent) :result) :tools))
      (should (equal (list (plist-get tool :name)
                           (member (plist-get tool :name) '("skill_search" "skill_load")))
                     (list (plist-get tool :name)
                           (and (string-search "\"_meta\":{\"anthropic/alwaysLoad\":true}"
                                               (json-serialize tool))
                                (member (plist-get tool :name)
                                        '("skill_search" "skill_load")))))))))

(ert-deftest ygg-skill-index-initialize-tells-the-agent-to-search ()
  (let (sent)
    (cl-letf (((symbol-function 'aob-mcp--send) (lambda (_conn obj) (push obj sent))))
      (aob-mcp--dispatch 'conn '(:jsonrpc "2.0" :id 1 :method "initialize"
                                 :params (:protocolVersion "2025-06-18"))))
    (let ((said (plist-get (plist-get (car sent) :result) :instructions)))
      (should (stringp said))
      (should (string-match-p "\\`Before starting any non-trivial task, call skill_search" said))
      (should (string-match-p "Skill tool" said))
      (should (string-match-p "skill_load" said)))))

(ert-deftest ygg-skill-index-session-url-names-its-project ()
  (let ((aob-mcp-host--key nil))
    (should (string-suffix-p "?session=tok&project=%2Ftmp%2Fproj%2F"
                             (plist-get (aob-mcp-host-spec "tok" "/tmp/proj/") :url)))
    (should (string-suffix-p "?session=tok" (plist-get (aob-mcp-host-spec "tok") :url)))))

(defun ygg-skill-index-tests--table (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest ygg-agent-skill-overrides-names-the-tail-only ()
  (let* ((ygg-agent-skill-core '("create-pr" "unslop" "docs"))
         (current (ygg-skill-index-tests--table
                   "unslop" "name-only" "gone" "off" "asked" "user-invocable-only"
                   "pinned" "on" "elsewhere" "off"))
         (skills (list '(:name "create-pr" :kind "user" :dmi :false)
                       '(:name "unslop" :kind "project" :dmi :false)
                       '(:name "tail" :kind "user" :dmi :false)
                       '(:name "gone" :kind "user" :dmi :false)
                       '(:name "asked" :kind "user" :dmi :false)
                       '(:name "pinned" :kind "user" :dmi :false)
                       '(:name "manual" :kind "user" :dmi t)
                       '(:name "chisle:chisle" :kind "plugin" :dmi :false)
                       '(:name "anthropic-skills:docx" :kind "synced" :dmi :false)
                       '(:name "anthropic-skills:docs" :kind "synced" :dmi :false)))
         (out (ygg-agent-skill-overrides current skills)))
    (should (equal "name-only" (gethash "tail" out)))
    (should (equal "name-only" (gethash "anthropic-skills:docx" out)))
    (should-not (gethash "anthropic-skills:docs" out))
    (should-not (gethash "create-pr" out))
    (should-not (gethash "unslop" out))
    (should (equal "off" (gethash "gone" out)))
    (should (equal "user-invocable-only" (gethash "asked" out)))
    (should (equal "on" (gethash "pinned" out)))
    (should (equal "off" (gethash "elsewhere" out)))
    (should-not (gethash "manual" out))
    (should-not (gethash "chisle:chisle" out))
    (should (equal "name-only" (gethash "unslop" current)))))

(ert-deftest ygg-agent-skill-overrides-write-keeps-the-rest-of-the-file ()
  (ygg-skill-index-tests--with
    (let ((ygg-agent-skill-core '("core-one")))
      (ygg-skill-index-tests--skill "core-one" "Always shown")
      (ygg-skill-index-tests--skill "tail-one" "Named only")
      (let ((file (ygg-skill-index-tests--file
                   "real/settings.json"
                   "{\"note\":\"em — dash\",\"skillOverrides\":{\"old\":\"off\"}}")))
        (should (equal '(("tail-one" nil "name-only"))
                       (ygg-agent-write-skill-overrides file)))
        (let ((json (ygg-agent--read-json file)))
          (should (equal "em — dash" (gethash "note" json)))
          (should (equal "off" (gethash "old" (gethash "skillOverrides" json))))
          (should (equal "name-only" (gethash "tail-one" (gethash "skillOverrides" json))))
          (should-not (gethash "core-one" (gethash "skillOverrides" json))))
        (should-not (ygg-agent-write-skill-overrides file))))))

(ert-deftest ygg-agent-skill-overrides-seeding-still-lets-a-home-win ()
  (ygg-skill-index-tests--with
    (let* ((ygg-agent-skill-core nil)
           (home (expand-file-name "real" ygg-skill-index-tests--root))
           (conf (expand-file-name "conf" ygg-skill-index-tests--root)))
      (ygg-skill-index-tests--skill "shared" "In both")
      (ygg-skill-index-tests--skill "global-only" "Only global")
      (ygg-skill-index-tests--file "real/settings.json" "{\"skillOverrides\":{}}")
      (ygg-skill-index-tests--file "conf/settings.json"
                                   "{\"skillOverrides\":{\"shared\":\"on\",\"home-only\":\"off\"}}")
      (ygg-agent-write-skill-overrides (expand-file-name "settings.json" home))
      (ygg-agent--seed-settings
       (append (list :home home) (cdr (assoc "claude" ygg-agent--config-homes)))
       conf)
      (let ((overrides (gethash "skillOverrides"
                                (ygg-agent--read-json (expand-file-name "settings.json" conf)))))
        (should (equal "on" (gethash "shared" overrides)))
        (should (equal "name-only" (gethash "global-only" overrides)))
        (should (equal "off" (gethash "home-only" overrides)))))))

(ert-deftest ygg-agent-codex-skill-policies-merge-and-skip ()
  (ygg-skill-index-tests--with
    (let ((ygg-agent-skill-core '("core")))
      (dolist (name '("plain" "styled" "decided" "core"))
        (ygg-skill-index-tests--file (format "agents/skills/%s/SKILL.md" name)
                                     (format "---\nname: %s\ndescription: d\n---\n" name)))
      (ygg-skill-index-tests--file "agents/skills/styled/agents/openai.yaml"
                                   "interface:\n  display_name: \"Styled\"\n")
      (ygg-skill-index-tests--file "agents/skills/decided/agents/openai.yaml"
                                   "interface:\n  display_name: \"D\"\npolicy:\n  allow_implicit_invocation: true\n")
      (ygg-skill-index-tests--file "elsewhere/linked/SKILL.md" "---\nname: linked\n---\n")
      (make-symbolic-link (expand-file-name "elsewhere/linked" ygg-skill-index-tests--root)
                          (expand-file-name "linked" ygg-skill-index-agents-dir))
      (let ((edits (ygg-agent-codex-skill-policies)))
        (should (equal '("decided" "plain" "styled")
                       (sort (mapcar (lambda (e) (file-name-nondirectory
                                                  (directory-file-name
                                                   (file-name-directory
                                                    (directory-file-name (file-name-directory (car e)))))))
                                     edits)
                             #'string<)))
        (should (equal "interface:\n  display_name: \"Styled\"\npolicy:\n  allow_implicit_invocation: false\n"
                       (nth 2 (seq-find (lambda (e) (string-match-p "/styled/" (car e))) edits))))
        (should (equal "interface:\n  display_name: \"D\"\npolicy:\n  allow_implicit_invocation: false\n"
                       (nth 2 (seq-find (lambda (e) (string-match-p "/decided/" (car e))) edits))))
        (should (equal "policy:\n  allow_implicit_invocation: false\n"
                       (nth 2 (seq-find (lambda (e) (string-match-p "/plain/" (car e))) edits))))
        (should (= 3 (length (ygg-agent-write-codex-skill-policies))))
        (should-not (ygg-agent-codex-skill-policies))
        (should-not (file-exists-p (expand-file-name "elsewhere/linked/agents"
                                                     ygg-skill-index-tests--root)))))))

(ert-deftest ygg-skill-index-reads-what-yaml-would ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--file
     "claude/skills/para/SKILL.md"
     "---\nname: para\ndescription: |\n  First paragraph.\n\n  Second paragraph.\nwhen_to_use: plain words\n  carried on\n---\nbody\n")
    (let ((skill (car (ygg-skill-index-skills))))
      (should (equal "First paragraph.\n\nSecond paragraph." (plist-get skill :description)))
      (should (equal "plain words carried on" (plist-get skill :when_to_use))))))

(ert-deftest ygg-skill-index-an-unofferable-skill-still-shadows-its-name ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--file "proj/.claude/skills/twin/SKILL.md"
                                 "---\nname: twin\ndescription: Project one\ndisable-model-invocation: true\n---\n")
    (ygg-skill-index-tests--skill "twin" "User one")
    (should-not (ygg-skill-index-tests--names (expand-file-name "proj" ygg-skill-index-tests--root)))))

(ert-deftest ygg-skill-index-project-skills-come-from-where-the-session-works ()
  (ygg-skill-index-tests--with
    (ygg-skill-index-tests--file "tree/.git" "gitdir: elsewhere\n")
    (ygg-skill-index-tests--file "tree/.claude/skills/top/SKILL.md" "---\nname: top\n---\n")
    (ygg-skill-index-tests--file "tree/skills/shipped/SKILL.md" "---\nname: shipped\n---\n")
    (ygg-skill-index-tests--file "tree/sub/.claude/skills/nested/SKILL.md" "---\nname: nested\n---\n")
    (ygg-skill-index-tests--file "main/.claude/skills/main-only/SKILL.md" "---\nname: main-only\n---\n")
    (should (equal '("nested" "top" "shipped")
                   (ygg-skill-index-tests--names
                    (expand-file-name "tree/sub" ygg-skill-index-tests--root))))))

(ert-deftest ygg-agent-implicit-off-keeps-the-yaml-valid ()
  (should (equal "policy:\n  allow_implicit_invocation: false\n"
                 (ygg-agent--implicit-off "policy: {}\n")))
  (should (equal "policy:\n    allow_implicit_invocation: false\n    other: 1\n"
                 (ygg-agent--implicit-off "policy:\n    other: 1\n")))
  (should (equal "policy:\n    allow_implicit_invocation: false\n"
                 (ygg-agent--implicit-off "policy:\n    allow_implicit_invocation: true\n")))
  (should (equal "interface:\n  x: 1\npolicy:\n  allow_implicit_invocation: false\n"
                 (ygg-agent--implicit-off "interface:\n  x: 1"))))

(provide 'ygg-skill-index-tests)
;;; ygg-skill-index-tests.el ends here
