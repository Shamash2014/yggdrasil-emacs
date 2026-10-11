;;; aob-transcript-search-tests.el --- picking a past conversation by date and by what was said -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'aob)
(require 'aob-acp)
(require 'aob-transcript)

(defun aob-transcript-search-tests--write (dir id user agent)
  (let ((file (expand-file-name (concat id ".jsonl") dir)))
    (with-temp-file file
      (insert (json-serialize `(:type "user" :message (:role "user" :content ,user))) "\n"
              (json-serialize `(:type "assistant"
                                :message (:role "assistant"
                                          :content [(:type "text" :text ,agent)])))
              "\n"))
    file))

(defmacro aob-transcript-search-tests--with-entries (var &rest body)
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "aob-search" t)))
          (old (aob-transcript-search-tests--write dir "old-1" "plan the garden" "tomatoes need sun"))
          (new (aob-transcript-search-tests--write dir "new-2" "fix the boiler" "the pilot light is out"))
          (,var (list (list :agent "claude" :acp-id "new-2" :dir dir :name "boiler" :file new)
                      (list :agent "claude" :acp-id "old-1" :dir dir :name "garden" :file old))))
     (set-file-times old (encode-time 0 0 12 1 10 2026))
     (set-file-times new (encode-time 0 5 14 11 10 2026))
     (unwind-protect
         (cl-letf (((symbol-function 'aob-acp-resumable-entries) (lambda () ,var))
                   ((symbol-function 'aob-acp-archived-entries) (lambda () nil)))
           ,@body)
       (delete-directory dir t))))

(ert-deftest aob-transcript-search-label-carries-the-date-and-lists-newest-first ()
  (aob-transcript-search-tests--with-entries entries
    (let ((past (aob-transcript--past-entries)))
      (should (equal (mapcar (lambda (e) (plist-get e :acp-id)) past) '("new-2" "old-1")))
      (should (string-match-p "\\`10-11 14:05  boiler · "
                              (aob-transcript--label (car past)))))))

(ert-deftest aob-transcript-search-finds-a-word-the-agent-said ()
  (aob-transcript-search-tests--with-entries entries
    (dolist (rg '(t nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) rg)))
        (let ((hits (aob-transcript--search "TOMATOES")))
          (should (equal (mapcar (lambda (h) (plist-get (car h) :acp-id)) hits) '("old-1")))
          (should (string-match-p "tomatoes need sun" (cdr (car hits)))))
        (should-not (aob-transcript--search "pomegranate"))))))

(ert-deftest aob-transcript-search-ignores-record-keys-and-opens-the-pick ()
  (aob-transcript-search-tests--with-entries entries
    (should-not (aob-transcript--search "assistant"))
    (let (opened)
      (cl-letf (((symbol-function 'aob-transcript-view) (lambda (e) (setq opened e)))
                ((symbol-function 'completing-read) (lambda (_p c &rest _) (car c))))
        (aob-transcript-search "boiler"))
      (should (equal (plist-get opened :acp-id) "new-2")))))

(provide 'aob-transcript-search-tests)
