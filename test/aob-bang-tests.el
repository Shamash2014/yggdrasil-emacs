;;; aob-bang-tests.el --- Tests for aob-bang -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defvar aob-acp-persist-file)
(setq aob-acp-persist-file (make-temp-file "aob-bang-sessions-" nil ".eld"))
(require 'aob)
(require 'aob-bang)

(defmacro aob-bang-tests--sending (draft &rest body)
  "Send DRAFT from a compose buffer; BODY sees SENT, what the target got."
  (declare (indent 1))
  `(let (sent shown)
     (cl-letf (((symbol-function 'display-buffer)
                (lambda (buf &rest _) (setq shown buf) nil)))
       (with-temp-buffer
         (aob-compose-mode)
         (setq aob-compose--target (lambda (text _atts) (push text sent)))
         (insert ,draft)
         (aob-compose-send)))
     ,@body))

(defun aob-bang-tests--expand (text)
  (with-temp-buffer
    (setq aob-compose--dir temporary-file-directory)
    (aob-bang-compose text)))

(ert-deftest aob-bang-single-line-expands-with-output ()
  (aob-bang-tests--sending "look at this\n!echo hello-bang\nthanks"
    (should (equal sent '("look at this\n<shell>\n$ echo hello-bang\nhello-bang\n</shell>\nthanks")))
    (should-not shown)))

(ert-deftest aob-bang-several-lines-each-expand ()
  (let ((out (aob-bang-tests--expand "!echo one\nmiddle\n!echo two")))
    (should (equal out (concat "<shell>\n$ echo one\none\n</shell>\nmiddle\n"
                               "<shell>\n$ echo two\ntwo\n</shell>")))))

(ert-deftest aob-bang-fenced-lines-left-alone ()
  (let ((text "```sh\n!echo inside\n```\n~~~\n!echo tilde\n~~~"))
    (should-not (aob-bang-tests--expand text)))
  (let ((out (aob-bang-tests--expand "```\n!echo inside\n```\n!echo outside")))
    (should (string-match-p "^!echo inside$" out))
    (should (string-match-p "^outside$" out))))

(ert-deftest aob-bang-mid-line-bang-left-alone ()
  (should-not (aob-bang-tests--expand "this is great! echo nope\nwow !echo nope"))
  (should-not (aob-bang-tests--expand "![shot](a.png)\n!\n!!echo not-alone\nmore")))

(ert-deftest aob-bang-double-bang-shows-popup-and-sends-nothing ()
  (let ((aob-compose-history nil))
    (aob-bang-tests--sending "!!echo only-for-me"
      (should-not sent)
      (should (eq shown (get-buffer aob-bang-buffer-name)))
      (with-current-buffer shown
        (should (derived-mode-p 'aob-bang-mode))
        (should (derived-mode-p 'special-mode))
        (should buffer-read-only)
        (should (string-match-p "\\`\\$ echo only-for-me\nonly-for-me\n"
                                (buffer-string))))
      (should (equal (car aob-compose-history) "!!echo only-for-me")))))

(ert-deftest aob-bang-nonzero-exit-reported ()
  (let ((out (aob-bang-tests--expand "!echo oops; exit 3")))
    (should (string-match-p "\noops\nexit status 3\n</shell>\\'" out))))

(ert-deftest aob-bang-output-capped ()
  (let* ((out (aob-bang-tests--expand "!seq 1 250"))
         (body (split-string out "\n")))
    (should (member "200" body))
    (should-not (member "201" body))
    (should (string-match-p "… 50 more lines left out" out)))
  (let* ((aob-bang-max-chars 10)
         (out (aob-bang-tests--expand "!printf 'abcdefghijklmnopqrstuvwxyz'")))
    (should (string-match-p "\nabcdefghij\n… 16 more characters left out\n" out))))

(ert-deftest aob-bang-timeout-reported ()
  (let* ((aob-bang-timeout 0.2)
         (start (float-time))
         (out (aob-bang-tests--expand "!echo early; sleep 5")))
    (should (< (- (float-time) start) 3))
    (should (equal out "<shell>\n$ echo early; sleep 5\ntimed out after 0.2 seconds\n</shell>"))))

(ert-deftest aob-bang-off-leaves-text-alone ()
  (let ((aob-bang nil))
    (should-not (aob-bang-tests--expand "!echo hi"))
    (aob-bang-tests--sending "!!echo hi"
      (should (equal sent '("!!echo hi")))
      (should-not shown))))

(ert-deftest aob-bang-runs-in-session-directory ()
  (let* ((dir (file-name-as-directory
               (file-truename (make-temp-file "aob-bang-dir-" t))))
         (s (aob-create-session :id (format "bang-%s" (random 100000))
                                :backend 'acp :name "bang" :dir dir)))
    (unwind-protect
        (with-temp-buffer
          (setq aob-compose--target (aob-session-id s))
          (let ((out (aob-bang-compose "!pwd -P")))
            (should (equal out (format "<shell>\n$ pwd -P\n%s\n</shell>"
                                       (directory-file-name dir))))))
      (aob-remove-session s)
      (delete-directory dir t))))

(ert-deftest aob-bang-runs-before-other-send-functions ()
  (let ((aob-compose-before-send-functions aob-compose-before-send-functions)
        (seen nil))
    (add-hook 'aob-compose-before-send-functions
              (lambda (text) (concat "!echo held\n\n" text)))
    (add-hook 'aob-compose-before-send-functions
              (lambda (text) (setq seen text) nil) t)
    (with-temp-buffer
      (setq aob-compose--dir temporary-file-directory)
      (aob-compose--rewritten "!echo typed"))
    (should (string-match-p "\\`!echo held\n\n<shell>\n\\$ echo typed\ntyped\n</shell>\\'"
                            seen))))

;;; aob-bang-tests.el ends here
