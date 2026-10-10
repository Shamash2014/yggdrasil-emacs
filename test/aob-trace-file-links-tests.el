;;; aob-trace-file-links-tests.el --- Files the agent names open on RET -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-trace-file-links-sessions-" nil ".eld"))
(require 'aob-trace)

(defmacro aob-trace-file-links-tests--in-dir (&rest body)
  `(let* ((dir (file-name-as-directory (make-temp-file "aob-links-" t)))
          (default-directory dir)
          (file (expand-file-name "sub/target.el" dir))
          (aob-trace--session-id nil))
     (make-directory (file-name-directory file))
     (with-temp-file file
       (dotimes (i 30) (insert (format "line %d abcdef\n" (1+ i)))))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun aob-trace-file-links-tests--link-at (text needle)
  (let ((out (aob-trace--file-links text)))
    (get-text-property (string-search needle text) 'aob-link out)))

(ert-deftest aob-trace-file-links-recognised ()
  (aob-trace-file-links-tests--in-dir
   (should (aob-trace-file-links-tests--link-at (format "see %s now" file) "sub/target"))
   (should (aob-trace-file-links-tests--link-at "see sub/target.el now" "sub/target"))
   (should (aob-trace-file-links-tests--link-at "see `sub/target.el` now" "sub/target"))
   (should (aob-trace-file-links-tests--link-at "see [the file](sub/target.el) now" "the file"))
   (should (aob-trace-file-links-tests--link-at "see ./sub/target.el, ok" "sub/target"))
   (should (equal 12 (nth 1 (aob-trace-file-links-tests--link-at "see sub/target.el:12 now" "sub/target"))))
   (should (equal 12 (nth 1 (aob-trace-file-links-tests--link-at "see sub/target.el#L12 now" "sub/target"))))
   (should (equal 12 (nth 1 (aob-trace-file-links-tests--link-at "[x](sub/target.el:12)" "x"))))
   (let ((spec (aob-trace-file-links-tests--link-at "see `sub/target.el:12:3`" "sub/target")))
     (should (equal (list 12 3) (list (nth 1 spec) (nth 3 spec)))))))

(ert-deftest aob-trace-file-links-rejected ()
  (aob-trace-file-links-tests--in-dir
   (should-not (aob-trace-file-links-tests--link-at "see sub/missing.el now" "sub/missing"))
   (should-not (aob-trace-file-links-tests--link-at "see sub now" "sub"))
   (should-not (aob-trace-file-links-tests--link-at "see https://example.com/sub/target.el" "sub/target"))
   (let* ((text "before\n```\nsub/target.el\n```\nafter")
          (out (aob-trace--file-links text)))
     (should-not (get-text-property (string-search "sub/target" text) 'aob-link out)))))

(ert-deftest aob-trace-file-links-open ()
  (aob-trace-file-links-tests--in-dir
   (let* ((out (aob-trace--file-links "see sub/target.el:12:3 now"))
          (at (string-search "sub/target" out)))
     (should (eq (lookup-key (get-text-property at 'keymap out) (kbd "RET"))
                 #'aob-trace-open-link))
     (with-temp-buffer
       (insert out)
       (goto-char (1+ at))
       (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) (set-buffer buf))))
         (aob-trace-open-link))
       (should (equal (file-truename file) (file-truename buffer-file-name)))
       (should (= 12 (line-number-at-pos)))
       (should (= 2 (current-column)))
       (kill-buffer)))))

(ert-deftest aob-trace-file-links-perf ()
  (aob-trace-file-links-tests--in-dir
   (let ((text (mapconcat (lambda (i) (format "word %d sub/target.el:%d and sub/nope%d.el text and more padding words here" i i i))
                          (number-sequence 1 50) "\n")))
     (while (< (length text) 20000) (setq text (concat text "\nfiller text")))
     (let ((start (float-time)))
       (aob-trace--file-links text)
       (message "aob-links-perf %.1fms for %d chars" (* 1000 (- (float-time) start)) (length text))))))

(ert-deftest aob-trace-file-links-wait-for-the-message-to-settle ()
  (aob-trace-file-links-tests--in-dir
   (should-not (text-property-not-all 0 21 'aob-link nil (aob-trace--md "see sub/target.el now" t)))
   (should (get-text-property 4 'aob-link (aob-trace--md "see sub/target.el now")))))

(provide 'aob-trace-file-links-tests)
