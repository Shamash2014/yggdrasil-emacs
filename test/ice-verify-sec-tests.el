;;; ice-verify-sec-tests.el --- Tests for the sec_cmd lane in etc/ice/ice-verify -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-ice)

(defconst ice-sec-tests--root-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst ice-sec-tests--verify-script
  (expand-file-name "../etc/ice/ice-verify" ice-sec-tests--root-dir))

(defconst ice-sec-tests--intent-body
  (concat "# Intent\n\n## What is wanted\n\nCarts total.\n\n## Caller's view\n\nThe shopper sees a sum.\n\n"
          "## Constraints\n\nCents only.\n\n## Failure scenarios\n\nNo negative total.\n\n"
          "## Success scenarios\n\nThe sum shows.\n\n## Connections\n\nFeature: Cart\n\n"
          "## Out of scope\n\nTax.\n\n## Alternatives refused\n\n- floats: they round\n"
          "\n## Restated\n\nCarts show a sum in cents.\n")
  "An intent.md body with every required section, Restated left unconfirmed.")

(defconst ice-sec-tests--expectations
  (concat "## Contract\n### Pre-conditions\n- C1: not empty\n\n"
          "## Scenario: cart#sum\n- GIVEN a cart\n- WHEN totalled\n- THEN the sum\n- Covers: C1\n")
  "expectations.md whose scenario cart#sum covers C1.")

(defconst ice-sec-tests--checkpoints-head "## Checkpoints\n1. Build\n")
(defconst ice-sec-tests--slices
  (concat "\n## Slices\n\n- [ ] 1. Build\n"
          "  - Blocked by: none\n  - Files: tests/test_calc.py\n"
          "  - Scenario: cart#sum\n  - Pass when: total sums correctly\n"
          "  - Evidence: pytest passes\n")
  "tasks.md's Slices section, one slice for checkpoint 1.")

(defun ice-sec-tests--hash (text fn)
  (with-temp-buffer (insert text) (funcall fn)))

(defun ice-sec-tests--intent-text (date)
  "intent.md text, confirmed for DATE with a hash that matches the body."
  (concat ice-sec-tests--intent-body "\nConfirmed: " date " sha1:"
          (ice-sec-tests--hash ice-sec-tests--intent-body #'ygg-ice--intent-hash) "\n"))

(defun ice-sec-tests--tasks-text (date)
  "tasks.md text, checkpoints approved for DATE with the section's hash."
  (let* ((body (concat ice-sec-tests--checkpoints-head ice-sec-tests--slices))
         (hash (ice-sec-tests--hash body #'ygg-ice--checkpoints-hash)))
    (concat ice-sec-tests--checkpoints-head "Approved: " date " sha1:" hash "\n" ice-sec-tests--slices)))

(defconst ice-sec-tests--fixed-calc
  "def total(items):\n    return sum(items)\n"
  "The fixed source the working tree carries, uncommitted, over the base's bug.")

(defconst ice-sec-tests--base-calc
  "def total(items):\n    return 0\n"
  "The buggy source committed at the locked base: fails the check there.")

(defconst ice-sec-tests--test-file
  "from calc import total\n\n\ndef test_cart__sum():\n    assert total([1, 2]) == 3\n")

(defun ice-sec-tests--write (path text)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text)))

(defun ice-sec-tests--git (root &rest args)
  "Run git ARGS in ROOT, returning (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory root))
      (cons (apply #'call-process "git" nil t nil args) (buffer-string)))))

(defun ice-sec-tests--run (root &rest args)
  "Run python3 ARGS in ROOT, returning (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory root))
      (cons (apply #'call-process "python3" nil t nil args) (buffer-string)))))

(defmacro ice-sec-tests--with-repo (var sec-config-line &rest body)
  "Bind VAR to a fresh repo locked for change \"c\", SEC-CONFIG-LINE in
.ice/config (or nil for none), source fixed uncommitted over a failing
base. Runs BODY with process-environment pointed at a temp HOME so no
real git identity or config is touched."
  (declare (indent 2))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "ice-sec-repo" t))))
          (home (make-temp-file "ice-sec-home" t))
          (process-environment (append (list (concat "HOME=" home)
                                             "GIT_CONFIG_NOSYSTEM=1"
                                             "GIT_AUTHOR_NAME=ice sec test"
                                             "GIT_AUTHOR_EMAIL=ice-sec@example.com"
                                             "GIT_COMMITTER_NAME=ice sec test"
                                             "GIT_COMMITTER_EMAIL=ice-sec@example.com")
                                       process-environment)))
     (unwind-protect
         (progn
           (ice-sec-tests--write (expand-file-name "openspec/changes/c/intent.md" ,var)
                                 (ice-sec-tests--intent-text "2026-09-26"))
           (ice-sec-tests--write (expand-file-name "openspec/changes/c/expectations.md" ,var)
                                 ice-sec-tests--expectations)
           (ice-sec-tests--write (expand-file-name "openspec/changes/c/tasks.md" ,var)
                                 (ice-sec-tests--tasks-text "2026-09-26"))
           (ice-sec-tests--write (expand-file-name "tests/test_calc.py" ,var) ice-sec-tests--test-file)
           (ice-sec-tests--write (expand-file-name "tests/calc.py" ,var) ice-sec-tests--base-calc)
           (ice-sec-tests--write (expand-file-name ".ice/config" ,var)
                                 (concat "test_cmd = python3 -m pytest -q {file}::{filter} --junitxml={report}\n"
                                         (if ,sec-config-line (concat ,sec-config-line "\n") "")))
           (should (= 0 (car (ice-sec-tests--git ,var "init" "-q" "-b" "main"))))
           (should (= 0 (car (ice-sec-tests--git ,var "add" "-A"))))
           (should (= 0 (car (ice-sec-tests--git ,var "commit" "-q" "-m" "base"))))
           (let ((lock (ice-sec-tests--run ,var (expand-file-name "../etc/ice/ice-lock" ice-sec-tests--root-dir)
                                           "openspec/changes/c" "lock")))
             (should (= 0 (car lock))))
           ;; the working tree's fix over the locked base, never committed
           (ice-sec-tests--write (expand-file-name "tests/calc.py" ,var) ice-sec-tests--fixed-calc)
           ,@body)
       (delete-directory ,var t)
       (delete-directory home t))))

(defun ice-sec-tests--verify (root)
  (ice-sec-tests--run root ice-sec-tests--verify-script "c"))

(defun ice-sec-tests--evidence-dir (root)
  "The newest .ice/evidence/c/*/* folder under ROOT."
  (car (last (sort (file-expand-wildcards (expand-file-name ".ice/evidence/c/*/*" root)) #'string<))))

(defun ice-sec-tests--ledger-last (root)
  "The last row of ROOT's .ice/ledger.tsv, split on tabs."
  (with-temp-buffer
    (insert-file-contents (expand-file-name ".ice/ledger.tsv" root))
    (split-string (string-trim (car (last (split-string (buffer-string) "\n" t)))) "\t")))

(ert-deftest ice-verify-sec-lane-skips-when-sec-cmd-is-unset ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-sec-tests--with-repo root nil
    (let ((out (ice-sec-tests--verify root)))
      (should (string-match-p "step sec: skipped, no sec_cmd" (cdr out)))
      (should (string-match-p "verdict: unit-verified" (cdr out)))
      (should (= 0 (car out)))
      (should (equal (nth 4 (ice-sec-tests--ledger-last root)) "unit-verified")))))

(ert-deftest ice-verify-sec-lane-runs-and-passes ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-sec-tests--with-repo root "sec_cmd = exit 0"
    (let ((out (ice-sec-tests--verify root)))
      (should (string-match-p "step sec: ok" (cdr out)))
      (should (string-match-p "verdict: unit-verified" (cdr out)))
      (should (= 0 (car out)))
      (should (< (string-match "step coverage:" (cdr out)) (string-match "step sec:" (cdr out))))
      (should (equal (nth 4 (ice-sec-tests--ledger-last root)) "unit-verified"))
      (let ((dir (ice-sec-tests--evidence-dir root)))
        (should dir)
        (should (file-exists-p (expand-file-name "sec.log" dir)))))))

(ert-deftest ice-verify-sec-lane-fails-the-verdict-on-a-nonzero-exit ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-sec-tests--with-repo root "sec_cmd = exit 1"
    (let ((out (ice-sec-tests--verify root)))
      (should (string-match-p "step sec: failed" (cdr out)))
      (should (string-match-p "verdict: failed sec" (cdr out)))
      (should (= 1 (car out)))
      (should (equal (nth 4 (ice-sec-tests--ledger-last root)) "failed sec"))
      (let ((dir (ice-sec-tests--evidence-dir root)))
        (should dir)
        (should (file-exists-p (expand-file-name "sec.log" dir)))))))

(provide 'ice-verify-sec-tests)
;;; ice-verify-sec-tests.el ends here
