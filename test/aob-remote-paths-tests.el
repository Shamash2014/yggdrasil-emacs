;;; aob-remote-paths-tests.el --- remote path mapping  -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'aob)

(ert-deftest aob-remote-paths-local-dir ()
  (should (equal (aob-session-path "/tmp/proj/" "/home/u/x.el") "/home/u/x.el"))
  (should (equal (aob-session-path "/tmp/proj/" "x.el") "/tmp/proj/x.el")))

(ert-deftest aob-remote-paths-remote-absolute ()
  (should (equal (aob-session-path "/ssh:host:/home/u/proj/" "/home/u/proj/x.el")
                 "/ssh:host:/home/u/proj/x.el")))

(ert-deftest aob-remote-paths-remote-relative ()
  (should (equal (aob-session-path "/ssh:host:/home/u/proj/" "src/x.el")
                 "/ssh:host:/home/u/proj/src/x.el")))

(ert-deftest aob-remote-paths-already-remote ()
  (should (equal (aob-session-path "/ssh:host:/home/u/proj/" "/ssh:host:/home/u/x.el")
                 "/ssh:host:/home/u/x.el")))

(provide 'aob-remote-paths-tests)
