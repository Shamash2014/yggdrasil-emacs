;;; aob-question-prompt-tests.el --- A question asked whole -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-question-prompt-sessions-" nil ".eld"))

(defconst aob-question-prompt-tests--long
  (concat "The payment worktree is gone. The fiscal code does exist in hde, "
          "but only on the release branch, and the migration that adds the "
          "column never ran on staging. Should I recreate the worktree from "
          "release, cherry-pick the migration onto main, or stop here?"))

(defun aob-question-prompt-tests--words (text)
  (split-string text "[ \n]+" t))

(ert-deftest aob-question-prompt/long-line-whole-and-wrapped ()
  (let* ((prompt (aob--question-prompt aob-question-prompt-tests--long 40))
         (lines (split-string prompt "\n")))
    (should-not (string-search "…" prompt))
    (should (equal (aob-question-prompt-tests--words aob-question-prompt-tests--long)
                   (butlast (aob-question-prompt-tests--words prompt))))
    (should (> (length lines) 3))
    (dolist (line (butlast lines))
      (should (<= (string-width line) 40)))
    (should (equal (car (last lines)) "> "))))

(ert-deftest aob-question-prompt/every-line-kept ()
  (let* ((text "Which branch?\n\nmain has the fix.\nrelease has the data.")
         (prompt (aob--question-prompt text 72)))
    (should (string-prefix-p (concat text "\n") prompt))))

(ert-deftest aob-question-prompt/completing-read-gets-full-text ()
  (let* ((seen nil)
         (sent nil)
         (d (list :kind 'elicitation :title "question"
                  :questions (list (list :key "q" :text aob-question-prompt-tests--long
                                         :options '("recreate" "cherry-pick" "stop"))))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _) (push prompt seen) "stop"))
              ((symbol-function 'aob--call)
               (lambda (_s _op _d content) (setq sent content))))
      (aob--resolve-question nil d))
    (should (= (length seen) 1))
    (should-not (string-search "…" (car seen)))
    (should (equal (aob-question-prompt-tests--words aob-question-prompt-tests--long)
                   (butlast (aob-question-prompt-tests--words (car seen)))))
    (should (equal sent '(("q" . "stop"))))))

(ert-deftest aob-question-prompt/read-string-gets-full-text ()
  (let* ((seen nil)
         (d (list :kind 'elicitation :title "question"
                  :questions (list (list :key "q" :text aob-question-prompt-tests--long)))))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _) (push prompt seen) "release"))
              ((symbol-function 'aob--call) #'ignore))
      (aob--resolve-question nil d))
    (dolist (word (aob-question-prompt-tests--words aob-question-prompt-tests--long))
      (should (string-search word (car seen))))))

(defun aob-question-prompt-tests--questions (json)
  (aob-acp--elicit-questions
   (json-parse-string json :object-type 'plist :array-type 'list
                      :null-object nil :false-object nil)))

(defun aob-question-prompt-tests--annotation (table)
  (cdr (assq 'annotation-function (cdr (funcall table "" nil 'metadata)))))

(ert-deftest aob-question-prompt/option-descriptions-annotate ()
  (let* ((q (car (aob-question-prompt-tests--questions
                  "{\"message\":\"Which way?\",\"requestedSchema\":{\"properties\":{\"way\":{\"type\":\"string\",\"title\":\"Route\",\"oneOf\":[{\"const\":\"recreate\",\"title\":\"recreate\",\"description\":\"Rebuild the worktree from release\"},{\"const\":\"stop\",\"title\":\"stop\"}]}}}}")))
         (table (aob--choice-table (plist-get q :options) (plist-get q :notes)))
         (annotate (aob-question-prompt-tests--annotation table)))
    (should (equal (plist-get q :options) '("recreate" "stop")))
    (should annotate)
    (let ((said (funcall annotate "recreate")))
      (should (equal said "  Rebuild the worktree from release"))
      (should (eq (get-text-property 2 'face said) 'shadow)))
    (should-not (funcall annotate "stop"))
    (should-not (funcall annotate aob-other-choice))))

(ert-deftest aob-question-prompt/no-descriptions-no-annotation ()
  (let* ((q (car (aob-question-prompt-tests--questions
                  "{\"message\":\"Which way?\",\"requestedSchema\":{\"properties\":{\"way\":{\"type\":\"string\",\"enum\":[\"recreate\",\"stop\"]}}}}")))
         (table (aob--choice-table (plist-get q :options) (plist-get q :notes))))
    (should-not (plist-get q :notes))
    (should-not (aob-question-prompt-tests--annotation table))))

(ert-deftest aob-question-prompt/pick-stays-the-const ()
  (let* ((sent nil)
         (annotated nil)
         (d (list :kind 'elicitation :title "question"
                  :questions (list (list :key "q" :text "Which way?"
                                         :options '("recreate" "stop")
                                         :notes '(("recreate" :title "Recreate it"
                                                   :description "From release")))))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (setq annotated (funcall (aob-question-prompt-tests--annotation table)
                                          "recreate"))
                 "recreate"))
              ((symbol-function 'aob--call)
               (lambda (_s _op _d content) (setq sent content))))
      (aob--resolve-question nil d))
    (should (equal annotated "  From release"))
    (should (equal sent '(("q" . "recreate"))))))

(ert-deftest aob-question-prompt/header-leads-the-prompt ()
  (let ((seen nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _) (push prompt seen) "stop"))
              ((symbol-function 'aob--call) #'ignore))
      (aob--resolve-question
       nil (list :kind 'elicitation
                 :questions (list (list :key "a" :text "Which way?" :header "Route"
                                        :options '("stop"))
                                  (list :key "b" :text "Same" :header "Same"
                                        :options '("stop"))))))
    (should (string-prefix-p "Route\nWhich way?\n" (cadr seen)))
    (should (string-prefix-p "Same\n> " (car seen)))))

(require 'layer-ui)

(defun aob-question-prompt-tests--posframe-height (prompt)
  (with-temp-buffer
    (insert prompt "typed")
    (let ((end (1+ (length prompt))))
      (cl-letf (((symbol-function 'frame-height) (lambda (&rest _) 50))
                ((symbol-function 'frame-width) (lambda (&rest _) 200))
                ((symbol-function 'minibuffer-prompt-end) (lambda () end)))
        (plist-get (ygg--vertico-posframe-size (current-buffer)) :height)))))

(ert-deftest aob-question-prompt/posframe-grows-with-the-prompt ()
  (should (= (aob-question-prompt-tests--posframe-height "Pick: ") 20))
  (should (= (aob-question-prompt-tests--posframe-height "a\nb\nc\nd\n> ") 24))
  (should (= (aob-question-prompt-tests--posframe-height
              (concat (make-string 60 ?\n) "> "))
             40)))

(provide 'aob-question-prompt-tests)
;;; aob-question-prompt-tests.el ends here
