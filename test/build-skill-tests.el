;;; build-skill-tests.el --- the build worker level runs the build skill body -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ygg-preset)

(defconst build-skill-tests--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro build-skill-tests--with-isolation (&rest body)
  "Run BODY with presets and skills read only from this checkout.
The user and home directories point at names that do not exist, and
default-directory a fresh temp repo, so a lookup done from another
root still finds the skill through the config's own skills dir."
  (declare (indent 0))
  `(let ((ygg-preset-config-directory (expand-file-name "presets/" build-skill-tests--root))
         (ygg-preset-own-skills-dir (expand-file-name "skills/" build-skill-tests--root))
         (ygg-preset-user-directory (make-temp-name "/tmp/build-skill-tests-no-presets-"))
         (ygg-preset-home-dir (make-temp-name "/tmp/build-skill-tests-no-home-"))
         (default-directory (file-name-as-directory
                              (make-temp-name "/tmp/build-skill-tests-no-root-"))))
     ,@body))

(ert-deftest build-skill-parses-with-a-short-description ()
  (let* ((file (expand-file-name "skills/build/SKILL.md" build-skill-tests--root))
         (parsed (ygg-preset--parse file))
         (fields (car parsed)))
    (should (equal (plist-get fields :name) "build"))
    (should (stringp (plist-get fields :description)))
    (should (<= (length (plist-get fields :description)) 150))
    (should (not (string-empty-p (string-trim (cdr parsed)))))))

(ert-deftest build-skill-level-runs-the-skill-body-from-another-root ()
  (build-skill-tests--with-isolation
    (let* ((known (ygg-preset-list))
           (skill-body (ygg-preset-skill-body "build" default-directory))
           (w (ygg-preset--worker-level (list :name "build" :model "opus") known)))
      (should skill-body)
      (should (equal (plist-get w :prompt) (string-trim skill-body)))
      (should (string-prefix-p "# Build" (plist-get w :prompt))))))

(ert-deftest deep-level-runs-the-same-skill-body-as-build ()
  (build-skill-tests--with-isolation
    (let* ((known (ygg-preset-list))
           (build-w (ygg-preset--worker-level (list :name "build" :model "opus") known))
           (deep-w (ygg-preset--worker-level (list :name "deep" :model "opus") known)))
      (should (equal (plist-get build-w :prompt) (plist-get deep-w :prompt))))))

(ert-deftest quick-gaps-review-ui-levels-are-unchanged ()
  (should (equal (cdr (assoc "quick" ygg-preset-worker-levels)) '(:preset "search" :read-only t)))
  (should (equal (cdr (assoc "gaps" ygg-preset-worker-levels)) '(:preset "gaps")))
  (should (equal (cdr (assoc "review" ygg-preset-worker-levels))
                 '(:skill "ice-review-loop" :read-only t)))
  (should (equal (cdr (assoc "ui" ygg-preset-worker-levels))
                 '(:skill "ice-ui-review" :read-only t))))

(provide 'build-skill-tests)
;;; build-skill-tests.el ends here
