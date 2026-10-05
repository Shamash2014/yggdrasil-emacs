;;; aob-mcp-review-submit-tests.el --- an agent's review lands as drafts -*- lexical-binding: t; -*-

;;; Code:

(with-suppressed-warnings ((lexical features)) (defvar features))
(require 'ert)
(require 'cl-lib)
(require 'aob-mcp)
(require 'aob-mcp-tools)

(defun aob-mcp-review-submit-tests--call (args &optional session)
  "Call review_submit with ARGS as SESSION's agent would.
Return (ANSWER RELAYED RECEIVED): what the tool said, whether it relayed,
and the arguments the compare view was handed."
  (let ((handler (plist-get (gethash "review_submit" aob-mcp--tools) :handler))
        form received)
    (cl-letf (((symbol-function 'aob-mcp-relay)
               (lambda (_conn _id f &rest _) (setq form f) aob-mcp-deferred))
              ((symbol-function 'aob-mcp-host-session) (lambda (_) session))
              ((symbol-function 'aob-session-name) (lambda (s) (plist-get s :name)))
              ((symbol-function 'ygg-git-compare-comments-receive)
               (lambda (&rest got) (setq received got) (cons (length (nth 2 got)) "key"))))
      (let ((direct (funcall handler args nil 1))
            (features (cons 'ygg-git-compare features)))
        (list (if form (eval form t) direct) (and form t) received)))))

(ert-deftest aob-mcp-review-submit-relays-converted-comments ()
  (pcase-let ((`(,answer ,relayed (,dir ,branch ,comments ,author))
               (aob-mcp-review-submit-tests--call
                '(:dir "/repo" :branch "feat"
                  :comments ((:file "a.el" :line 12 :type "nit" :text "rename")
                             (:file "a.el" :line 20 :start_line 15 :side "old" :text "dead")
                             (:file "b.el" :text "whole file")
                             (:text "overall fine")))
                '(:name "reviewer"))))
    (should relayed)
    (should (equal (list dir branch author) '("/repo" "feat" "reviewer")))
    (should (equal comments
                   '((:level line :type nit :text "rename" :file "a.el" :new-path "a.el"
                      :side new :line 12 :start-line nil :start-side new :title nil :priority nil :confidence nil)
                     (:level range :type nil :text "dead" :file "a.el" :new-path "a.el"
                      :side old :line 20 :start-line 15 :start-side old :title nil :priority nil :confidence nil)
                     (:level file :type nil :text "whole file" :file "b.el" :new-path "b.el"
                      :side new :line nil :start-line nil :start-side new :title nil :priority nil :confidence nil)
                     (:level review :type nil :text "overall fine" :file nil :new-path nil
                      :side new :line nil :start-line nil :start-side new :title nil :priority nil :confidence nil))))
    (should (equal answer '("4 comments submitted for review on feat; the user will check them")))))

(ert-deftest aob-mcp-review-submit-parses-json-and-names-author ()
  (pcase-let ((`(,answer ,_ (,_ ,_ ,comments ,author))
               (aob-mcp-review-submit-tests--call
                '(:dir "/repo" :branch "feat" :author "me"
                  :comments "[{\"file\": \"a.el\", \"line\": \"7\", \"level\": \"line\", \"text\": \"why?\", \"type\": \"question\"}]"))))
    (should (equal author "me"))
    (should (equal comments '((:level line :type question :text "why?" :file "a.el" :new-path "a.el"
                               :side new :line 7 :start-line nil :start-side new
                               :title nil :priority nil :confidence nil))))
    (should (equal (car answer) "1 comment submitted for review on feat; the user will check them"))))

(ert-deftest aob-mcp-review-submit-author-falls-back-to-agent ()
  (should (equal "agent" (nth 3 (nth 2 (aob-mcp-review-submit-tests--call
                                        '(:dir "/r" :branch "b" :comments ((:text "ok")))))))))

(ert-deftest aob-mcp-review-submit-refuses-bad-entries-without-relaying ()
  (pcase-let ((`(,answer ,relayed ,_)
               (aob-mcp-review-submit-tests--call
                '(:dir "/repo" :branch "feat"
                  :comments ((:file "a.el" :line "x" :text "t")
                             (:line 3 :text "no file")
                             (:file "a.el" :line 3 :side "left" :text "t")
                             (:file "a.el" :level "range" :line 3 :text "t")
                             (:file "a.el" :line 3)
                             "plain")))))
    (should-not relayed)
    (should (equal answer
                   (string-join
                    '("nothing submitted:"
                      "comment 1: line is not a positive integer"
                      "comment 2: file missing"
                      "comment 3: side \"left\" is not new or old"
                      "comment 4: start_line missing"
                      "comment 5: text missing"
                      "comment 6: not an object")
                    "\n")))))

(ert-deftest aob-mcp-review-submit-relays-the-good-and-names-the-skipped ()
  (pcase-let ((`(,answer ,relayed (,_ ,_ ,comments ,_))
               (aob-mcp-review-submit-tests--call
                '(:dir "/repo" :branch "feat"
                  :comments ((:file "a.el" :line 1 :text "ok") (:file "a.el" :line 2))))))
    (should relayed)
    (should (= 1 (length comments)))
    (should (equal answer '("1 comment submitted for review on feat; the user will check them"
                            "skipped:" "comment 2: text missing")))))

(ert-deftest aob-mcp-review-submit-refuses-unparsable-json-and-missing-branch ()
  (should (equal (car (aob-mcp-review-submit-tests--call
                       '(:dir "/repo" :branch "feat" :comments "[{")))
                 "nothing submitted:\ncomments is not a JSON array"))
  (should (equal (car (aob-mcp-review-submit-tests--call
                       '(:dir "/repo" :comments ((:text "t")))))
                 "which branch? pass branch")))

(ert-deftest aob-mcp-review-submit-takes-priority-confidence-title-and-verdict ()
  (pcase-let ((`(,answer ,_ (,_ ,_ ,comments ,_))
               (aob-mcp-review-submit-tests--call
                '(:dir "/repo" :branch "feat"
                  :comments ((:file "a.el" :line 4 :title "Off by one" :priority 1
                              :confidence 0.8 :text "loop ends early"))
                  :verdict (:correctness "patch is incorrect" :explanation "one bug"
                            :confidence 0.7)))))
    (should (equal comments
                   '((:level line :type nil :text "loop ends early" :file "a.el" :new-path "a.el"
                      :side new :line 4 :start-line nil :start-side new
                      :title "Off by one" :priority 1 :confidence 0.8)
                     (:level review :type nil :text "one bug" :file nil :new-path nil
                      :side new :line nil :start-line nil :start-side new
                      :title nil :priority nil :confidence 0.7
                      :correctness "patch is incorrect"))))
    (should (equal (car answer) "2 comments submitted for review on feat; the user will check them"))))

(ert-deftest aob-mcp-review-submit-refuses-bad-priority-confidence-and-verdict ()
  (should (equal (car (aob-mcp-review-submit-tests--call
                       '(:dir "/repo" :branch "feat"
                         :comments ((:text "t" :priority 5 :confidence 2 :title 3))
                         :verdict (:correctness "fine" :explanation "x"))))
                 (string-join
                  '("nothing submitted:"
                    "comment 1: title is not a string, priority is not 0 to 3, confidence is not a number from 0 to 1"
                    "verdict: correctness is not \"patch is correct\" or \"patch is incorrect\"")
                  "\n"))))

(defconst aob-mcp-review-submit-tests--codex
  '(:findings ((:title "[P1] Null deref" :body "x may be nil" :confidence_score 0.9 :priority 1
                :code_location (:absolute_file_path "/repo/src/a.el"
                                :line_range (:start 10 :end 14)))
               (:title "[P3] Name" :body "rename y" :confidence_score 0.4 :priority 3
                :code_location (:absolute_file_path "/elsewhere/b.el"
                                :line_range (:start 7 :end 7))))
    :overall_correctness "patch is incorrect"
    :overall_explanation "a nil deref"
    :overall_confidence_score 0.85)
  "A Codex review as codex-rs emits it.")

(defconst aob-mcp-review-submit-tests--codex-converted
  '((:level range :type nil :text "x may be nil" :file "src/a.el" :new-path "src/a.el"
     :side new :line 14 :start-line 10 :start-side new
     :title "[P1] Null deref" :priority 1 :confidence 0.9)
    (:level review :type nil :text "a nil deref" :file nil :new-path nil
     :side new :line nil :start-line nil :start-side new
     :title nil :priority nil :confidence 0.85 :correctness "patch is incorrect"))
  "What the compare view is handed for `aob-mcp-review-submit-tests--codex'.")

(ert-deftest aob-mcp-review-submit-takes-a-codex-review-as-object ()
  (pcase-let ((`(,answer ,relayed (,_ ,_ ,comments ,_))
               (aob-mcp-review-submit-tests--call
                `(:dir "/repo/" :branch "feat" :codex_review ,aob-mcp-review-submit-tests--codex))))
    (should relayed)
    (should (equal comments aob-mcp-review-submit-tests--codex-converted))
    (should (equal answer '("2 comments submitted for review on feat; the user will check them"
                            "skipped:" "finding 2: file is outside the repository")))))

(defconst aob-mcp-review-submit-tests--codex-json
  "{\"findings\": [{\"title\": \"[P1] Null deref\", \"body\": \"x may be nil\", \"confidence_score\": 0.9, \"priority\": 1, \"code_location\": {\"absolute_file_path\": \"/repo/src/a.el\", \"line_range\": {\"start\": 10, \"end\": 14}}}, {\"title\": \"[P3] Name\", \"body\": \"rename y\", \"confidence_score\": 0.4, \"priority\": 3, \"code_location\": {\"absolute_file_path\": \"/elsewhere/b.el\", \"line_range\": {\"start\": 7, \"end\": 7}}}], \"overall_correctness\": \"patch is incorrect\", \"overall_explanation\": \"a nil deref\", \"overall_confidence_score\": 0.85}"
  "`aob-mcp-review-submit-tests--codex' as the JSON text an agent would pass.")

(ert-deftest aob-mcp-review-submit-takes-a-codex-review-as-json ()
  (should (equal (nth 2 (nth 2 (aob-mcp-review-submit-tests--call
                                `(:dir "/repo" :branch "feat"
                                  :codex_review ,aob-mcp-review-submit-tests--codex-json))))
                 aob-mcp-review-submit-tests--codex-converted)))

(ert-deftest aob-mcp-review-submit-refuses-a-start-after-the-line ()
  (pcase-let ((`(,answer ,relayed ,_)
               (aob-mcp-review-submit-tests--call
                '(:dir "/repo" :branch "feat"
                  :comments ((:file "a.el" :line 5 :start_line 9 :text "x"))))))
    (should-not relayed)
    (should (string-search "comment 1: start_line is after line" answer))))

(ert-deftest aob-mcp-review-submit-makes-files-under-dir-relative-through-symlinks ()
  (let* ((real (file-name-as-directory (make-temp-file "repo" t)))
         (link (concat (directory-file-name real) "-link")))
    (unwind-protect
        (progn
          (make-symbolic-link (directory-file-name real) link)
          (pcase-let ((`(,answer ,_ (,_ ,_ ,comments ,_))
                       (aob-mcp-review-submit-tests--call
                        `(:dir ,link :branch "feat"
                          :comments ((:file ,(concat real "src/a.el") :line 3 :text "x")
                                     (:file "/nowhere/else.el" :line 3 :text "y"))
                          :codex_review
                          (:findings ((:title "t" :body "z" :priority 1
                                       :code_location
                                       (:absolute_file_path ,(concat real "src/b.el")
                                        :line_range (:start 1 :end 1)))))))))
            (should (equal (mapcar (lambda (c) (plist-get c :file)) comments)
                           '("src/a.el" "src/b.el")))
            (should (equal (cdr answer)
                           '("skipped:" "comment 2: file is outside the repository")))))
      (delete-file link)
      (delete-directory real t))))

(provide 'aob-mcp-review-submit-tests)
;;; aob-mcp-review-submit-tests.el ends here
