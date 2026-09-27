;;; aob-steal-small-tests.el --- compaction instructions and capped MCP output -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(require 'aob-mcp)

(setq aob-acp-persist-file (make-temp-file "aob-steal-sessions-" nil ".eld"))

(defun aob-steal--compact-text (agent)
  "The text autosummarize sends a settled, full session running AGENT."
  (let ((s (aob-session--create :id (format "acp:steal:%s" agent) :backend 'acp
                                :name "steal" :state 'idle))
        (aob-acp-autocompact-ratio 0.85)
        sent)
    (aob-session-put s :agent agent)
    (aob-session-put s :commands '((:name "compact")))
    (aob-session-put s :ctx-size 1000)
    (aob-session-put s :ctx-used 950)
    (cl-letf (((symbol-function 'aob-acp--prompt-1)
               (lambda (_s text &rest _) (setq sent text)))
              ((symbol-function 'aob-event) #'ignore)
              ((symbol-function 'message) #'ignore))
      (aob-acp--autocompact-check s))
    sent))

(ert-deftest aob-steal-compact-claude-gets-instructions ()
  (let ((text (aob-steal--compact-text "claude")))
    (should (equal text (concat "/compact " aob-acp-compact-instructions)))
    (dolist (heading '("Goal" "Constraints and Preferences" "Progress"
                       "Key Decisions" "Next Steps" "Critical Context"
                       "Relevant Files"))
      (should (string-search heading text)))))

(ert-deftest aob-steal-compact-isolated-claude-counts-as-claude ()
  (should (equal (aob-steal--compact-text "claude-isolated")
                 (concat "/compact " aob-acp-compact-instructions))))

(ert-deftest aob-steal-compact-codex-stays-bare ()
  (should (equal (aob-steal--compact-text "codex") "/compact")))

(ert-deftest aob-steal-compact-nil-instructions-stays-bare ()
  (let ((aob-acp-compact-instructions nil))
    (should (equal (aob-steal--compact-text "claude") "/compact"))))

(defmacro aob-steal--in-temp (&rest body)
  (declare (indent 0))
  `(let ((temporary-file-directory
          (file-name-as-directory (make-temp-file "aob-steal-tmp-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory temporary-file-directory t))))

(defun aob-steal--numbered (n &optional width)
  (mapconcat (lambda (i) (format (format "line %%0%dd" (or width 5)) i))
             (number-sequence 1 n) "\n"))

(defun aob-steal--marker-file (text)
  (and (string-match "full output in \\(.*\\) \\.\\.\\.\\]" text)
       (match-string 1 text)))

(defun aob-steal--read (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
    (buffer-string)))

(ert-deftest aob-steal-cap-small-text-untouched ()
  (aob-steal--in-temp
    (let ((text (aob-steal--numbered 10)))
      (should (eq (aob-mcp--cap text) text))
      (should-not (file-exists-p (aob-mcp--output-dir))))))

(ert-deftest aob-steal-cap-long-text-keeps-head-and-tail ()
  (aob-steal--in-temp
    (let* ((aob-mcp-output-max-lines 20)
           (text (aob-steal--numbered 100))
           (capped (aob-mcp--cap text))
           (lines (split-string capped "\n"))
           (file (aob-steal--marker-file capped)))
      (should (= (length lines) 21))
      (should (equal (car lines) "line 00001"))
      (should (equal (nth 9 lines) "line 00010"))
      (should (string-match-p "\\[\\.\\.\\. 80 lines cut; full output in " (nth 10 lines)))
      (should (equal (nth 11 lines) "line 00091"))
      (should (equal (car (last lines)) "line 00100"))
      (should (string-prefix-p (file-name-as-directory (aob-mcp--output-dir)) file))
      (should (equal (aob-steal--read file) text)))))

(ert-deftest aob-steal-cap-byte-limit ()
  (aob-steal--in-temp
    (let* ((aob-mcp-output-max-bytes 1000)
           (text (aob-steal--numbered 200 90))
           (capped (aob-mcp--cap text))
           (file (aob-steal--marker-file capped)))
      (should (< (length (split-string text "\n")) aob-mcp-output-max-lines))
      (should (< (string-bytes capped) (+ 1000 200)))
      (should (string-prefix-p (substring text 0 20) capped))
      (should (string-suffix-p (substring text -20) capped))
      (should (string-match-p " lines cut; full output in " capped))
      (should (equal (aob-steal--read file) text)))))

(ert-deftest aob-steal-cap-single-huge-line-keeps-both-ends ()
  (aob-steal--in-temp
    (let* ((json (concat "{\"start\":\"é"
                         (apply #'concat (make-list 20000 "\"key\":\"v\","))
                         "\"end\":\"ü\"}"))
           (capped (aob-mcp--cap json))
           (lines (split-string capped "\n"))
           (marker (nth 1 lines)))
      (should (> (string-bytes json) 200000))
      (should (= (length lines) 3))
      (should (string-prefix-p "{\"start\":\"é" (car lines)))
      (should (string-suffix-p "\"end\":\"ü\"}" (nth 2 lines)))
      (should (string-match-p "\\[\\.\\.\\. 1 lines cut; full output in " marker))
      (should (<= (string-bytes capped)
                  (+ aob-mcp-output-max-bytes (string-bytes marker) 2)))
      (should (equal (aob-steal--read (aob-steal--marker-file capped)) json)))))

(ert-deftest aob-steal-cap-applies-to-sync-and-deferred-results ()
  (aob-steal--in-temp
    (let ((aob-mcp-output-max-lines 20)
          (aob-mcp--tools (make-hash-table :test 'equal))
          (aob-mcp--pending (make-hash-table :test 'equal))
          (text (aob-steal--numbered 100))
          sent)
      (cl-letf (((symbol-function 'aob-mcp--send)
                 (lambda (_conn obj) (push obj sent))))
        (aob-mcp-deftool :name "big" :description "big" :args nil
                         :handler (lambda (&rest _) text))
        (aob-mcp--call 'conn 1 '(:name "big"))
        (let ((key nil))
          (aob-mcp-deftool :name "later" :description "later" :args nil
                           :handler (lambda (_args conn id)
                                      (setq key (aob-mcp-defer conn id))
                                      aob-mcp-deferred))
          (aob-mcp--call 'conn 2 '(:name "later"))
          (should (= (length sent) 1))
          (aob-mcp-complete key text)))
      (should (= (length sent) 2))
      (dolist (obj sent)
        (let ((out (plist-get (aref (plist-get (plist-get obj :result) :content) 0)
                              :text)))
          (should (string-match-p "80 lines cut" out))
          (should (equal (aob-steal--read (aob-steal--marker-file out)) text)))))))

(ert-deftest aob-steal-cap-prunes-old-files ()
  (aob-steal--in-temp
    (let* ((aob-mcp-output-max-lines 20)
           (dir (aob-mcp--output-dir))
           (old (expand-file-name "out-old.txt" dir))
           (fresh (expand-file-name "out-fresh.txt" dir)))
      (make-directory dir t)
      (write-region "old" nil old nil 'silent)
      (write-region "fresh" nil fresh nil 'silent)
      (set-file-times old (time-subtract nil (days-to-time 8)))
      (set-file-times fresh (time-subtract nil (days-to-time 6)))
      (aob-mcp--cap (aob-steal--numbered 100))
      (should-not (file-exists-p old))
      (should (file-exists-p fresh)))))

;;; aob-steal-small-tests.el ends here
