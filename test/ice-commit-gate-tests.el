;;; ice-commit-gate-tests.el --- Tests for the ledger commit gate -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-ice)

(defconst ice-gate-tests--root-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst ice-gate-tests--ice-dir
  (expand-file-name "../etc/ice" ice-gate-tests--root-dir))

(defun ice-gate-tests--script (name)
  (expand-file-name name ice-gate-tests--ice-dir))

(defconst ice-gate-tests--intent-body
  (concat "# Intent\n\n## What is wanted\n\nCarts total.\n\n## Caller's view\n\nThe shopper sees a sum.\n\n"
          "## Constraints\n\nCents only.\n\n## Failure scenarios\n\nNo negative total.\n\n"
          "## Success scenarios\n\nThe sum shows.\n\n## Connections\n\nFeature: Cart\n\n"
          "## Out of scope\n\nTax.\n\n## Alternatives refused\n\n- floats: they round\n"
          "\n## Restated\n\nCarts show a sum in cents.\n"))

(defconst ice-gate-tests--expectations
  (concat "## Contract\n### Pre-conditions\n- C1: not empty\n\n"
          "## Scenario: cart#sum\n- GIVEN a cart\n- WHEN totalled\n- THEN the sum\n- Covers: C1\n"))

(defconst ice-gate-tests--checkpoints-head "## Checkpoints\n1. Build\n")
(defconst ice-gate-tests--slices
  (concat "\n## Slices\n\n- [ ] 1. Build\n"
          "  - Blocked by: none\n  - Files: tests/calc.py\n"
          "  - Scenario: cart#sum\n  - Pass when: total sums correctly\n"
          "  - Evidence: pytest passes\n"))

(defun ice-gate-tests--hash (text fn)
  (with-temp-buffer (insert text) (funcall fn)))

(defun ice-gate-tests--intent-text (date)
  (concat ice-gate-tests--intent-body "\nConfirmed: " date " sha1:"
          (ice-gate-tests--hash ice-gate-tests--intent-body #'ygg-ice--intent-hash) "\n"))

(defun ice-gate-tests--tasks-text (date)
  (let* ((body (concat ice-gate-tests--checkpoints-head ice-gate-tests--slices))
         (hash (ice-gate-tests--hash body #'ygg-ice--checkpoints-hash)))
    (concat ice-gate-tests--checkpoints-head "Approved: " date " sha1:" hash "\n" ice-gate-tests--slices)))

(defconst ice-gate-tests--base-calc
  "def total(items):\n    return 0\n"
  "The buggy source committed at the locked base: fails the check there.")
(defconst ice-gate-tests--fixed-calc
  "def total(items):\n    return sum(items)\n"
  "The fixed source the working tree carries, uncommitted, over the base's bug.")
(defconst ice-gate-tests--other-calc
  "def total(items):\n    return sum(items) + 1\n"
  "A different, still-uncommitted diff: neither the base bug nor the verified fix.")
(defconst ice-gate-tests--test-file
  "from calc import total\n\n\ndef test_cart__sum():\n    assert total([1, 2]) == 3\n")

(defun ice-gate-tests--write (path text)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert text)))

(defun ice-gate-tests--git (root &rest args)
  "Run git ARGS in ROOT, returning (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory root))
      (cons (apply #'call-process "git" nil t nil args) (buffer-string)))))

(defun ice-gate-tests--run (root program &rest args)
  "Run PROGRAM ARGS in ROOT, returning (EXIT . OUTPUT)."
  (with-temp-buffer
    (let ((default-directory root))
      (cons (apply #'call-process program nil t nil args) (buffer-string)))))

(defmacro ice-gate-tests--with-repo (var &rest body)
  "Bind VAR to a fresh repo with change \"c\" locked over
tests/calc.py (named on the slice's Files: line, never itself part of
the lock record, matching the sec fixture), and .ice/ice-commit-gate
installed.  Isolated from any real git identity, config or hooksPath."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "ice-gate-repo" t))))
          (home (make-temp-file "ice-gate-home" t))
          (global-config (expand-file-name "gitconfig" home))
          (process-environment (append (list (concat "HOME=" home)
                                             (concat "GIT_CONFIG_GLOBAL=" global-config)
                                             (concat "XDG_CONFIG_HOME=" (expand-file-name "xdg" home))
                                             "GIT_CONFIG_NOSYSTEM=1"
                                             "GIT_AUTHOR_NAME=ice gate test"
                                             "GIT_AUTHOR_EMAIL=ice-gate@example.com"
                                             "GIT_COMMITTER_NAME=ice gate test"
                                             "GIT_COMMITTER_EMAIL=ice-gate@example.com")
                                       process-environment)))
     (unwind-protect
         (progn
           (should (= 0 (car (ice-gate-tests--git ,var "init" "-q" "-b" "main"))))
           (should (equal "" (string-trim (cdr (ice-gate-tests--git ,var "config" "--get" "core.hooksPath")))))
           (ice-gate-tests--write (expand-file-name "openspec/changes/c/intent.md" ,var)
                                  (ice-gate-tests--intent-text "2026-09-26"))
           (ice-gate-tests--write (expand-file-name "openspec/changes/c/expectations.md" ,var)
                                  ice-gate-tests--expectations)
           (ice-gate-tests--write (expand-file-name "openspec/changes/c/tasks.md" ,var)
                                  (ice-gate-tests--tasks-text "2026-09-26"))
           (ice-gate-tests--write (expand-file-name "tests/test_calc.py" ,var) ice-gate-tests--test-file)
           (ice-gate-tests--write (expand-file-name "tests/calc.py" ,var) ice-gate-tests--base-calc)
           (ice-gate-tests--write (expand-file-name ".ice/config" ,var)
                                  "test_cmd = python3 -m pytest -q {file}::{filter} --junitxml={report}\n")
           (make-directory (expand-file-name ".ice" ,var) t)
           (copy-file (ice-gate-tests--script "ice-commit-gate") (expand-file-name ".ice/ice-commit-gate" ,var))
           (copy-file (ice-gate-tests--script "ice-scenarios") (expand-file-name ".ice/ice-scenarios" ,var))
           (set-file-modes (expand-file-name ".ice/ice-commit-gate" ,var) #o755)
           ;; the installed hook's own "[ -x .ice/ice-check ] || exit 0" guard
           ;; needs a real ice-check for the check_lines to run at all
           (ice-gate-tests--write (expand-file-name ".ice/ice-check" ,var) "#!/bin/sh\nexit 0\n")
           (set-file-modes (expand-file-name ".ice/ice-check" ,var) #o755)
           (should (= 0 (car (ice-gate-tests--git ,var "add" "-A"))))
           (should (= 0 (car (ice-gate-tests--git ,var "commit" "-q" "-m" "base"))))
           (let ((lock (ice-gate-tests--run ,var "python3" (ice-gate-tests--script "ice-lock")
                                            "openspec/changes/c" "lock")))
             (should (= 0 (car lock))))
           ;; the working tree's fix over the locked base, never committed
           (ice-gate-tests--write (expand-file-name "tests/calc.py" ,var) ice-gate-tests--fixed-calc)
           ,@body)
       (delete-directory ,var t)
       (delete-directory home t))))

(defun ice-gate-tests--install-hook (root)
  "Install the git pre-commit hook that runs the commit gate, via
etc/ice/ice-hook-install, the same script ice-wire.sh calls."
  (with-temp-buffer
    (let ((default-directory root))
      (call-process-shell-command
       (format "printf '.ice/ice-commit-gate\\n' | %s %s %s %s"
               (shell-quote-argument (ice-gate-tests--script "ice-hook-install"))
               (shell-quote-argument (directory-file-name root))
               (shell-quote-argument "ice-wire: commit gate, C4, lat check and ice-check")
               (shell-quote-argument ".ice/ice-commit-gate"))
       nil t nil))
    (buffer-string)))

(defun ice-gate-tests--verify-c (root)
  (ice-gate-tests--run root "python3" (ice-gate-tests--script "ice-verify") "c"))

(defun ice-gate-tests--commit (root message)
  (ice-gate-tests--git root "commit" "-q" "-m" message))

;; --- hook install: fresh, chains a foreign hook, never overwrites it ---

(ert-deftest ice-gate-hook-install-chains-existing-hook ()
  (skip-unless (executable-find "git"))
  (ice-gate-tests--with-repo root
    (let* ((hooks (expand-file-name ".git/hooks" root))
           (hook (expand-file-name "pre-commit" hooks))
           (legacy (expand-file-name "pre-commit.legacy" hooks))
           (marker (expand-file-name "legacy-ran.txt" root)))
      (make-directory hooks t)
      (ice-gate-tests--write hook (format "#!/bin/sh\necho ran >> %s\nexit 0\n" (shell-quote-argument marker)))
      (set-file-modes hook #o755)
      (ice-gate-tests--install-hook root)
      (should (file-exists-p legacy))
      (should (string-match-p "echo ran" (with-temp-buffer (insert-file-contents legacy) (buffer-string))))
      (should (string-match-p ".ice/ice-commit-gate"
                              (with-temp-buffer (insert-file-contents hook) (buffer-string))))
      ;; the hook chains to the foreign one: a commit touching no locked
      ;; change's files runs the gate (passes) then the legacy hook; the
      ;; working tree's uncommitted fix to tests/calc.py is left alone
      (ice-gate-tests--write (expand-file-name "README.md" root) "hello\n")
      (should (= 0 (car (ice-gate-tests--git root "add" "README.md"))))
      (should (= 0 (car (ice-gate-tests--commit root "second"))))
      (should (file-exists-p marker)))))

(ert-deftest ice-gate-hook-install-idempotent ()
  (skip-unless (executable-find "git"))
  (ice-gate-tests--with-repo root
    (ice-gate-tests--install-hook root)
    (let ((first (with-temp-buffer (insert-file-contents (expand-file-name ".git/hooks/pre-commit" root)) (buffer-string))))
      (ice-gate-tests--install-hook root)
      (should (equal first (with-temp-buffer (insert-file-contents (expand-file-name ".git/hooks/pre-commit" root)) (buffer-string)))))))

;; --- gate script itself: refuse / pass / stale / off / untouched ---

(ert-deftest ice-gate-refuses-without-verified-row ()
  (skip-unless (executable-find "git"))
  (ice-gate-tests--with-repo root
    ;; the macro already wrote the fixed, uncommitted tests/calc.py
    (should (= 0 (car (ice-gate-tests--git root "add" "-A"))))
    (let ((out (ice-gate-tests--run root "python3" (expand-file-name ".ice/ice-commit-gate" root))))
      (should (/= 0 (car out)))
      (should (string-match-p "touches locked change c" (cdr out))))))

(ert-deftest ice-gate-passes-after-a-verified-row-for-this-content ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-gate-tests--with-repo root
    ;; ice-verify reads the working tree directly, no staging needed
    (let ((verify (ice-gate-tests--verify-c root)))
      (should (string-match-p "verdict: unit-verified" (cdr verify))))
    (should (= 0 (car (ice-gate-tests--git root "add" "-A"))))
    (let ((out (ice-gate-tests--run root "python3" (expand-file-name ".ice/ice-commit-gate" root))))
      (should (= 0 (car out))))))

(ert-deftest ice-gate-refuses-a-different-diff-after-verification ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-gate-tests--with-repo root
    (should (string-match-p "verdict: unit-verified" (cdr (ice-gate-tests--verify-c root))))
    ;; a different diff than what was verified, still uncommitted
    (ice-gate-tests--write (expand-file-name "tests/calc.py" root) ice-gate-tests--other-calc)
    (should (= 0 (car (ice-gate-tests--git root "add" "-A"))))
    (let ((out (ice-gate-tests--run root "python3" (expand-file-name ".ice/ice-commit-gate" root))))
      (should (/= 0 (car out)))
      (should (string-match-p "touches locked change c" (cdr out))))))

(ert-deftest ice-gate-commit-gate-off-lets-it-through ()
  (skip-unless (executable-find "git"))
  (ice-gate-tests--with-repo root
    (ice-gate-tests--write (expand-file-name ".ice/config" root)
                           "test_cmd = true\ncommit_gate = off\n")
    (should (= 0 (car (ice-gate-tests--git root "add" "-A"))))
    (let ((out (ice-gate-tests--run root "python3" (expand-file-name ".ice/ice-commit-gate" root))))
      (should (= 0 (car out))))))

(ert-deftest ice-gate-passes-a-commit-touching-no-ice-change ()
  (skip-unless (executable-find "git"))
  (ice-gate-tests--with-repo root
    ;; stage only README, leaving the macro's uncommitted fix to
    ;; tests/calc.py (a locked change's file) out of the index
    (ice-gate-tests--write (expand-file-name "README.md" root) "hello\n")
    (should (= 0 (car (ice-gate-tests--git root "add" "README.md"))))
    (let ((out (ice-gate-tests--run root "python3" (expand-file-name ".ice/ice-commit-gate" root))))
      (should (= 0 (car out))))))

;; --- ice-verify --status exit codes ---

(ert-deftest ice-verify-status-none-before-any-run ()
  (skip-unless (executable-find "git"))
  (ice-gate-tests--with-repo root
    (let ((out (ice-gate-tests--run root "python3" (ice-gate-tests--script "ice-verify") "--status" "c")))
      (should (/= 0 (car out)))
      (should (string-match-p "^none:" (cdr out))))))

(ert-deftest ice-verify-status-exits-0-for-a-matching-verified-row ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-gate-tests--with-repo root
    (should (string-match-p "verdict: unit-verified" (cdr (ice-gate-tests--verify-c root))))
    (let ((out (ice-gate-tests--run root "python3" (ice-gate-tests--script "ice-verify") "--status" "c")))
      (should (= 0 (car out)))
      (should (string-match-p "unit-verified" (cdr out))))))

(ert-deftest ice-verify-status-is-stale-once-content-changes ()
  (skip-unless (and (executable-find "git") (executable-find "python3")))
  (ice-gate-tests--with-repo root
    (should (string-match-p "verdict: unit-verified" (cdr (ice-gate-tests--verify-c root))))
    (ice-gate-tests--write (expand-file-name "tests/calc.py" root) ice-gate-tests--other-calc)
    (let ((out (ice-gate-tests--run root "python3" (ice-gate-tests--script "ice-verify") "--status" "c")))
      (should (/= 0 (car out)))
      (should (string-match-p "^stale:" (cdr out))))))

;; --- mutation detection in ice-runner ---

(ert-deftest ice-runner-sets-mutate-cmd-when-the-tool-is-on-path ()
  (skip-unless (executable-find "git"))
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "ice-runner-mut" t))))
         (bin (file-name-as-directory (make-temp-file "ice-runner-mut-bin" t)))
         (stub (expand-file-name "mutmut" bin)))
    (unwind-protect
        (progn
          (should (= 0 (car (ice-gate-tests--git root "init" "-q" "-b" "main"))))
          (ice-gate-tests--write (expand-file-name "pyproject.toml" root) "[tool.pytest.ini_options]\n")
          (ice-gate-tests--write (expand-file-name "tests/test_x.py" root) "def test_ok():\n    assert True\n")
          (ice-gate-tests--write stub "#!/bin/sh\nexit 0\n")
          (set-file-modes stub #o755)
          (let* ((process-environment (cons (concat "PATH=" bin ":" (getenv "PATH")) process-environment))
                 (out (ice-gate-tests--run root "python3" (ice-gate-tests--script "ice-runner") "config" root)))
            (should (= 0 (car out)))
            (should (string-match-p "mutate_cmd set to mutmut" (cdr out)))
            (should (string-match-p "^mutate_cmd = "
                                    (with-temp-buffer (insert-file-contents (expand-file-name ".ice/config" root)) (buffer-string))))))
      (delete-directory root t)
      (delete-directory bin t))))

(ert-deftest ice-runner-leaves-mutate-cmd-unset-and-prints-advice-without-the-tool ()
  (skip-unless (executable-find "git"))
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "ice-runner-mut" t))))
         ;; a PATH holding only what git/python3 need, never the real one:
         ;; a stray mutmut elsewhere on the machine must not leak in
         (bare-path (mapconcat #'file-name-directory
                               (delq nil (list (executable-find "git") (executable-find "python3")))
                               ":")))
    (unwind-protect
        (progn
          (should (= 0 (car (ice-gate-tests--git root "init" "-q" "-b" "main"))))
          (ice-gate-tests--write (expand-file-name "pyproject.toml" root) "[tool.pytest.ini_options]\n")
          (ice-gate-tests--write (expand-file-name "tests/test_x.py" root) "def test_ok():\n    assert True\n")
          (let* ((process-environment (cons (concat "PATH=" bare-path ":/bin:/usr/bin") process-environment)))
            (skip-unless (not (executable-find "mutmut")))
            (let ((out (ice-gate-tests--run root "python3" (ice-gate-tests--script "ice-runner") "config" root)))
              (should (= 0 (car out)))
              (should (string-match-p "mutation testing for pytest has no installed tool" (cdr out)))
              (should (not (string-match-p "^mutate_cmd = "
                                          (with-temp-buffer (insert-file-contents (expand-file-name ".ice/config" root)) (buffer-string))))))))
      (delete-directory root t))))

(provide 'ice-commit-gate-tests)
;;; ice-commit-gate-tests.el ends here
