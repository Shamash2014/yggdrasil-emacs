;;; regrade-qa-tests.el --- the verify level regrades fresh, and the qa baseline is stated -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ygg-preset)

(defconst regrade-qa-tests--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro regrade-qa-tests--with-isolation (&rest body)
  "Run BODY with presets and skills read only from this checkout.
The user and home directories point at names that do not exist, and
default-directory a fresh temp repo, so a lookup done from another
root still finds the skill through the config's own skills dir."
  (declare (indent 0))
  `(let ((ygg-preset-config-directory (expand-file-name "presets/" regrade-qa-tests--root))
         (ygg-preset-own-skills-dir (expand-file-name "skills/" regrade-qa-tests--root))
         (ygg-preset-user-directory (make-temp-name "/tmp/regrade-qa-tests-no-presets-"))
         (ygg-preset-home-dir (make-temp-name "/tmp/regrade-qa-tests-no-home-"))
         (default-directory (file-name-as-directory
                              (make-temp-name "/tmp/regrade-qa-tests-no-root-"))))
     ,@body))

(defun regrade-qa-tests--file (path)
  (expand-file-name path regrade-qa-tests--root))

(defun regrade-qa-tests--text (path)
  (with-temp-buffer
    (insert-file-contents (regrade-qa-tests--file path))
    (buffer-string)))

(ert-deftest regrade-skill-parses-with-a-short-description ()
  (let* ((parsed (ygg-preset--parse (regrade-qa-tests--file "skills/regrade/SKILL.md")))
         (fields (car parsed))
         (body (cdr parsed)))
    (should (equal (plist-get fields :name) "regrade"))
    (should (stringp (plist-get fields :description)))
    (should (<= (length (plist-get fields :description)) 150))
    (should (not (string-empty-p (string-trim body))))
    (should (< (length (string-trim body)) 1500))))

(ert-deftest verify-level-runs-the-regrade-skill-body-and-is-not-read-only ()
  (regrade-qa-tests--with-isolation
    (let* ((known (ygg-preset-list))
           (skill-body (ygg-preset-skill-body "regrade" default-directory))
           (w (ygg-preset--worker-level (list :name "verify" :model "sonnet") known)))
      (should skill-body)
      (should (equal (plist-get w :prompt) (string-trim skill-body)))
      (should (string-prefix-p "# Regrade" (plist-get w :prompt)))
      (should-not (plist-get w :read-only)))))

(ert-deftest lead-preset-runs-verify-on-a-different-model-from-build ()
  (regrade-qa-tests--with-isolation
    (let* ((known (ygg-preset-list))
           (lead (seq-find (lambda (d) (equal (ygg-preset-name d) "lead")) known))
           (workers (ygg-preset-workers lead))
           (build (seq-find (lambda (w) (equal (plist-get w :name) "build")) workers))
           (verify (seq-find (lambda (w) (equal (plist-get w :name) "verify")) workers)))
      (should build)
      (should verify)
      (should (plist-get verify :model))
      (should-not (equal (plist-get build :model) (plist-get verify :model))))))

(ert-deftest checking-howto-names-the-verify-step ()
  (let ((text (regrade-qa-tests--text "skills/lead-howto/checking.md")))
    (should (string-match-p "verify worker" text))
    (should (string-match-p "PASS" text))))

(ert-deftest qa-preset-or-skill-states-the-baseline-real-user-filter-and-claims-table ()
  (let ((qa (regrade-qa-tests--text "presets/qa.md"))
        (health (regrade-qa-tests--text "skills/qa-health/SKILL.md")))
    (should (or (string-match-p (regexp-quote ".aob/qa/baseline.json") qa)
                (string-match-p (regexp-quote ".aob/qa/baseline.json") health)))
    (should (or (string-match-p "would a person hit this" qa)
                (string-match-p "would a person hit this" health)))
    (should (or (string-match-p "## Claims" qa) (string-match-p "## Claims" health)))))

(defconst regrade-qa-tests--playbook-qa-skills
  '("prove-it-works" "interrogate" "differential-review" "blast-radius" "qa-health"))

(ert-deftest qa-playbook-is-short-and-names-every-skill-it-uses ()
  (let ((text (regrade-qa-tests--text "skills/daemon/playbooks/qa.md")))
    (should (< (length text) 1200))
    (dolist (name regrade-qa-tests--playbook-qa-skills)
      (should (string-match-p (regexp-quote name) text))
      (should (file-exists-p (regrade-qa-tests--file (concat "skills/" name "/SKILL.md")))))))

(provide 'regrade-qa-tests)
;;; regrade-qa-tests.el ends here
