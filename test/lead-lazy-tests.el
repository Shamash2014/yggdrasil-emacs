;;; lead-lazy-tests.el --- the lead preset keeps its rules and leaves the how-to to a skill -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ygg-preset)

(defconst lead-lazy-tests--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defun lead-lazy-tests--file (path)
  (expand-file-name path lead-lazy-tests--root))

(defun lead-lazy-tests--text (path)
  (with-temp-buffer
    (insert-file-contents (lead-lazy-tests--file path))
    (buffer-string)))

(ert-deftest lead-lazy-preset-is-short ()
  (should (< (length (lead-lazy-tests--text "presets/lead.md")) 3800)))

(ert-deftest lead-lazy-preset-names-the-howto ()
  (should (string-match-p "skill lead-howto" (lead-lazy-tests--text "presets/lead.md"))))

(ert-deftest lead-lazy-preset-keeps-the-no-paste-rule ()
  (should (string-match-p "do not paste one" (lead-lazy-tests--text "presets/lead.md"))))

(ert-deftest lead-lazy-every-howto-file-the-preset-names-exists ()
  (let ((text (lead-lazy-tests--text "presets/lead.md"))
        (case-fold-search nil)
        (start 0)
        named)
    (while (string-match "lead-howto/\\([a-z-]+\\.md\\)" text start)
      (push (match-string 1 text) named)
      (setq start (match-end 0)))
    (should (>= (length named) 5))
    (dolist (file named)
      (should (file-exists-p (lead-lazy-tests--file (concat "skills/lead-howto/" file)))))))

(ert-deftest lead-lazy-howto-index-is-short-and-links-only-real-files ()
  (let ((text (lead-lazy-tests--text "skills/lead-howto/SKILL.md"))
        (start 0))
    (should (< (length text) 1200))
    (while (string-match "(\\([a-z-]+\\.md\\))" text start)
      (should (file-exists-p (lead-lazy-tests--file
                              (concat "skills/lead-howto/" (match-string 1 text)))))
      (setq start (match-end 0)))))

(ert-deftest lead-lazy-howto-is-not-inlined-by-the-preset ()
  (let ((fields (car (ygg-preset--parse (lead-lazy-tests--file "presets/lead.md")))))
    (should-not (member "lead-howto" (ygg-preset-names (plist-get fields :skills))))))

(ert-deftest lead-lazy-howto-parses-with-a-short-description ()
  (let* ((parsed (ygg-preset--parse (lead-lazy-tests--file "skills/lead-howto/SKILL.md")))
         (description (plist-get (car parsed) :description)))
    (should (equal "lead-howto" (plist-get (car parsed) :name)))
    (should (stringp description))
    (should (<= (length description) 150))
    (should-not (plist-get (car parsed) :disable-model-invocation))
    (should (string-match-p "(briefs\\.md)" (cdr parsed)))))

(ert-deftest lead-lazy-preset-names-skill-daemon ()
  (should (string-match-p "skill daemon" (lead-lazy-tests--text "presets/lead.md"))))

(ert-deftest lead-lazy-daemon-parses-and-is-model-invocable ()
  (let* ((parsed (ygg-preset--parse (lead-lazy-tests--file "skills/daemon/SKILL.md")))
         (fields (car parsed))
         (description (plist-get fields :description)))
    (should (equal "daemon" (plist-get fields :name)))
    (should (stringp description))
    (should (<= (length description) 150))
    (should-not (plist-get fields :disable-model-invocation))))

(defconst lead-lazy-tests--playbooks
  '("feature" "ice" "bugfix" "exploration" "architecture" "testing" "qa" "review" "bughunt"
    "prototype"))

(ert-deftest lead-lazy-daemon-links-every-playbook-and-each-is-short ()
  ;; cap raised 2000 -> 2400 for the evidence-not-authorization and
  ;; parallel-design rules (owner instruction, 2026-09-29)
  (should (< (length (lead-lazy-tests--text "skills/daemon/SKILL.md")) 2400))
  (let ((text (lead-lazy-tests--text "skills/daemon/SKILL.md")))
    (dolist (name lead-lazy-tests--playbooks)
      (let ((path (concat "playbooks/" name ".md")))
        (should (string-match-p (regexp-quote path) text))
        (should (file-exists-p (lead-lazy-tests--file (concat "skills/daemon/" path))))
        (should (< (length (lead-lazy-tests--text (concat "skills/daemon/" path))) 1200))))))

(ert-deftest lead-lazy-daemon-has-evidence-not-authorization-rule ()
  (should (string-match-p "evidence, not\nauthorization to change code"
                          (lead-lazy-tests--text "skills/daemon/SKILL.md"))))

(ert-deftest lead-lazy-daemon-has-no-parallel-design-rule ()
  (should (string-match-p "parallel\ndesign exercise"
                          (lead-lazy-tests--text "skills/daemon/SKILL.md"))))

(defconst lead-lazy-tests--playbook-ends
  '(("architecture" . "owner-approved")
    ("bugfix" . "regression test")
    ("bughunt" . "confirmed bug becomes a new")
    ("exploration" . "answer with evidence, no edits")
    ("feature" . "VERIFY and the review")
    ("ice" . "ice-review-loop verdicts")
    ("prototype" . "code is thrown away")
    ("qa" . "confirmed bug becomes a new")
    ("review" . "findings, or an approved checkpoint plan")
    ("testing" . "0 unexpected")))

(ert-deftest lead-lazy-every-playbook-states-what-it-ends-on ()
  (dolist (entry lead-lazy-tests--playbook-ends)
    (let ((text (lead-lazy-tests--text
                 (concat "skills/daemon/playbooks/" (car entry) ".md"))))
      (should (string-match-p "Ends on:" text))
      (should (string-match-p (regexp-quote (cdr entry)) text)))))

(defconst lead-lazy-tests--playbook-skills
  '(("feature" . ("restate"))
    ("ice" . ("restate" "ice" "ice-checks" "ice-review-loop" "prove-it-works"))
    ("bugfix" . ("debug-mantra" "fix-it" "test-behavior-not-implementation" "prove-it-works"))
    ("exploration" . ("restate" "how" "why" "recall" "explore-do"))
    ("architecture" . ("architect" "domain-modeling" "design-decision" "decision-memo"
                       "structure" "explain-architecture" "interrogate" "plan-review"))
    ("testing" . ("test-behavior-not-implementation" "ice-checks"
                  "create-verification-skill" "maintain-verification-skill"))
    ("qa" . ("prove-it-works" "interrogate" "differential-review" "blast-radius"))
    ("review" . ("scrutinize" "differential-review" "interrogate"))
    ("bughunt" . ("security-scan" "variant-analysis" "differential-review" "blast-radius"))
    ("prototype" . ("prototype" "ice-prototype"))))

(ert-deftest lead-lazy-every-skill-a-playbook-names-exists ()
  (dolist (entry lead-lazy-tests--playbook-skills)
    (dolist (skill (cdr entry))
      (should (file-exists-p
               (lead-lazy-tests--file (concat "skills/" skill "/SKILL.md")))))))

(ert-deftest lead-lazy-security-scan-parses-and-is-model-invocable ()
  (let* ((parsed (ygg-preset--parse (lead-lazy-tests--file "skills/security-scan/SKILL.md")))
         (fields (car parsed))
         (description (plist-get fields :description)))
    (should (equal "security-scan" (plist-get fields :name)))
    (should (stringp description))
    (should (<= (length description) 150))
    (should-not (plist-get fields :disable-model-invocation))))

(ert-deftest lead-lazy-daemon-entry-is-model-invocable-and-names-build ()
  (let* ((parsed (ygg-preset--parse (lead-lazy-tests--file "skills/daemon/SKILL.md")))
         (fields (car parsed))
         (body (cdr parsed))
         (description (plist-get fields :description)))
    (should (equal "daemon" (plist-get fields :name)))
    (should (stringp description))
    (should (<= (length description) 150))
    (should-not (plist-get fields :disable-model-invocation))
    (should (string-match-p "skill build" body))))

(ert-deftest lead-lazy-prototype-skill-parses-and-is-model-invocable ()
  (let* ((parsed (ygg-preset--parse (lead-lazy-tests--file "skills/prototype/SKILL.md")))
         (fields (car parsed))
         (description (plist-get fields :description)))
    (should (equal "prototype" (plist-get fields :name)))
    (should (stringp description))
    (should (<= (length description) 150))
    (should-not (plist-get fields :disable-model-invocation))))

(ert-deftest lead-lazy-ice-skill-mentions-skill-prototype ()
  (should (string-match-p "skill prototype" (lead-lazy-tests--text "skills/ice/SKILL.md"))))

(ert-deftest lead-lazy-build-skill-never-addresses-the-owner ()
  (should (string-match-p "never address the owner"
                          (lead-lazy-tests--text "skills/build/SKILL.md"))))

(ert-deftest lead-lazy-build-skill-blocker-carries-options-and-default ()
  (should (string-match-p "blocker with\noptions and a default"
                          (lead-lazy-tests--text "skills/build/SKILL.md"))))

(ert-deftest lead-lazy-howto-has-holds-and-recovery-and-index-links-it ()
  (should (file-exists-p (lead-lazy-tests--file "skills/lead-howto/holds.md")))
  (should (string-match-p "(holds\\.md)" (lead-lazy-tests--text "skills/lead-howto/SKILL.md")))
  (let ((text (lead-lazy-tests--text "skills/lead-howto/holds.md")))
    (should (string-match-p "hold" text))
    (should (string-match-p "reconcile" text))
    (should (< (length text) 1200))))

(ert-deftest lead-lazy-sending-never-polls ()
  (should (string-match-p "Never poll a worker or a session"
                          (lead-lazy-tests--text "skills/lead-howto/sending.md"))))

(ert-deftest lead-lazy-daemon-names-the-recovery-rule ()
  (should (string-match-p "lead-howto/holds\\.md"
                          (lead-lazy-tests--text "skills/daemon/SKILL.md"))))

(ert-deftest lead-lazy-plan-review-parses-and-is-model-invocable ()
  (let* ((parsed (ygg-preset--parse (lead-lazy-tests--file "skills/plan-review/SKILL.md")))
         (fields (car parsed))
         (description (plist-get fields :description)))
    (should (equal "plan-review" (plist-get fields :name)))
    (should (stringp description))
    (should (<= (length description) 150))
    (should-not (plist-get fields :disable-model-invocation))))

(ert-deftest lead-lazy-architecture-names-plan-review ()
  (let ((text (lead-lazy-tests--text "skills/daemon/playbooks/architecture.md")))
    (should (string-match-p "plan-review" text))
    (should (file-exists-p (lead-lazy-tests--file "skills/plan-review/SKILL.md")))))

(ert-deftest lead-lazy-no-poteto-outside-doc ()
  ;; built at runtime so this assertion's own text never matches its search
  (let ((needle (concat "pot" "eto"))
        (default-directory lead-lazy-tests--root)
        (self (file-name-nondirectory
               (or load-file-name buffer-file-name "lead-lazy-tests.el"))))
    (with-temp-buffer
      (call-process "git" nil t nil "-C" lead-lazy-tests--root
                    "grep" "-l" "-I" needle
                    "--" "skills" "presets" "lisp" "test")
      (let (hits)
        (dolist (line (split-string (buffer-string) "\n" t))
          (unless (or (string-prefix-p "doc/" line)
                      (string-suffix-p self line))
            (push line hits)))
        (should (equal hits nil))))))

(provide 'lead-lazy-tests)
;;; lead-lazy-tests.el ends here
