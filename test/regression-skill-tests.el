;;; regression-skill-tests.el --- the regression skill states the gstack-derived flow -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ygg-preset)

(defconst regression-skill-tests--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defun regression-skill-tests--file (path)
  (expand-file-name path regression-skill-tests--root))

(defun regression-skill-tests--text (path)
  (with-temp-buffer
    (insert-file-contents (regression-skill-tests--file path))
    (buffer-string)))

(ert-deftest regression-skill-parses-with-a-short-description ()
  (let* ((parsed (ygg-preset--parse (regression-skill-tests--file "skills/regression/SKILL.md")))
         (fields (car parsed))
         (body (cdr parsed)))
    (should (equal (plist-get fields :name) "regression"))
    (should (stringp (plist-get fields :description)))
    (should (<= (length (plist-get fields :description)) 150))
    (should (not (string-empty-p (string-trim body))))
    (should (< (length (string-trim body)) 2500))))

(ert-deftest regression-skill-states-the-three-baseline-files ()
  (let ((text (regression-skill-tests--text "skills/regression/SKILL.md")))
    (should (string-match-p (regexp-quote "under .aob/qa/") text))
    (should (string-match-p (regexp-quote "baseline.json") text))
    (should (string-match-p (regexp-quote "perf-baseline.json") text))
    (should (string-match-p (regexp-quote "canary.jsonl") text))))

(ert-deftest regression-skill-states-the-owner-accept-rule ()
  (let ((text (regression-skill-tests--text "skills/regression/SKILL.md")))
    (should (string-match-p "written only when the owner" text))
    (should (string-match-p "accepts the run" text))))

(ert-deftest regression-skill-states-the-perf-thresholds ()
  (let ((text (regression-skill-tests--text "skills/regression/SKILL.md")))
    (should (string-match-p "50%" text))
    (should (string-match-p "500ms" text))
    (should (string-match-p "25%" text))))

(ert-deftest regression-skill-states-the-canary-two-consecutive-rule ()
  (let ((text (regression-skill-tests--text "skills/regression/SKILL.md")))
    (should (string-match-p "two consecutive failed checks" text))))

(ert-deftest regression-skill-states-the-regression-test-naming ()
  (let ((text (regression-skill-tests--text "skills/regression/SKILL.md")))
    (should (string-match-p (regexp-quote "NAME.regression-N.test.EXT") text))))

(defconst regression-skill-tests--surface-files
  '("frontend.md" "backend.md" "mobile.md"))

(ert-deftest regression-skill-surface-files-exist-and-are-linked ()
  (let ((text (regression-skill-tests--text "skills/regression/SKILL.md")))
    (should (string-match-p "Surfaces" text))
    (dolist (name regression-skill-tests--surface-files)
      (should (file-exists-p
               (regression-skill-tests--file (concat "skills/regression/references/" name))))
      (should (string-match-p (regexp-quote (concat "references/" name)) text)))))

(ert-deftest regression-skill-names-detection-signals-per-surface ()
  (let ((text (regression-skill-tests--text "skills/regression/SKILL.md")))
    (should (string-match-p (regexp-quote "package.json") text))
    (should (string-match-p (regexp-quote "OpenAPI") text))
    (should (string-match-p (regexp-quote "pubspec.yaml") text))
    (should (string-match-p (regexp-quote "com.android.application") text))
    (should (string-match-p (regexp-quote "Package.swift") text))
    (should (string-match-p (regexp-quote ".xcodeproj") text))))

(ert-deftest regression-skill-backend-states-contract-rule-and-p95-thresholds ()
  (let ((text (regression-skill-tests--text "skills/regression/references/backend.md")))
    (should (string-match-p "previously passing request" text))
    (should (string-match-p "schema" text))
    (should (string-match-p "p95" text))
    (should (string-match-p "50%" text))
    (should (string-match-p "500ms" text))))

(ert-deftest regression-skill-mobile-names-simbroker-and-device-rule-and-app-size ()
  (let ((text (regression-skill-tests--text "skills/regression/references/mobile.md")))
    (should (string-match-p "simbroker" text))
    (should (string-match-p "never touch a device the owner holds" text))
    (should (string-match-p "10%" text))
    (should (string-match-p "5%" text))))

(ert-deftest regression-skill-baselines-describe-per-surface-keys-and-screens-path ()
  (let ((text (regression-skill-tests--text "skills/regression/references/baselines.md")))
    (should (string-match-p (regexp-quote "\"web\"") text))
    (should (string-match-p (regexp-quote "\"api\"") text))
    (should (string-match-p (regexp-quote "\"mobile\"") text))
    (should (string-match-p (regexp-quote ".aob/qa/screens/") text))))

(defconst regression-skill-tests--qa-playbook-skills
  '("prove-it-works" "interrogate" "differential-review" "blast-radius" "qa-health" "regression"))

(ert-deftest qa-playbook-stays-short-and-names-every-skill-including-regression ()
  (let ((text (regression-skill-tests--text "skills/daemon/playbooks/qa.md")))
    (should (< (length text) 1200))
    (dolist (name regression-skill-tests--qa-playbook-skills)
      (should (string-match-p (regexp-quote name) text))
      (should (file-exists-p (regression-skill-tests--file (concat "skills/" name "/SKILL.md")))))))

(ert-deftest qa-health-and-qa-preset-and-test-scope-name-the-regression-skill ()
  (should (string-match-p "regression" (regression-skill-tests--text "skills/qa-health/SKILL.md")))
  (should (string-match-p "regression" (regression-skill-tests--text "presets/qa.md")))
  (should (string-match-p "regression" (regression-skill-tests--text "skills/test-scope-the-diff/SKILL.md"))))

(provide 'regression-skill-tests)
;;; regression-skill-tests.el ends here
