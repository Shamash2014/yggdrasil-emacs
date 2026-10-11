;;; aob-link-tests.el --- ERT tests for aob-link -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(require 'bookmark)
(require 'aob)
(require 'aob-acp)
(require 'aob-link)

(defmacro aob-link-tests--isolated (&rest body)
  (declare (indent 0))
  `(let ((bookmark-default-file (make-temp-file "aob-link-bm-"))
         (aob-acp-persist-file (make-temp-file "aob-link-persist-"))
         (aob-acp--persisted-cache nil))
     ,@body))

(defun aob-link-tests--session (acp-id dir)
  (let ((s (aob-create-session :id (concat "acp:" acp-id) :backend 'acp
                               :name "linked:1" :project dir :dir dir
                               :state 'idle)))
    (aob-session-put s :acp-id acp-id)
    (aob-session-put s :agent "claude")
    s))

(ert-deftest aob-link-round-trip-with-double-colon ()
  (let* ((dir "/tmp/a::b dir/c&d=e")
         (path (aob-link--build "id-1" "claude" dir)))
    (should-not (string-match-p "::" path))
    (should (equal (aob-link--parse path)
                   (list "id-1" "claude" (expand-file-name dir))))))

(ert-deftest aob-link-follow-live-goes-to-session ()
  (aob-link-tests--isolated
    (let ((s (aob-link-tests--session "live-1" temporary-file-directory))
          went)
      (unwind-protect
          (cl-letf (((symbol-function 'ygg-aob--goto) (lambda (x) (setq went x)))
                    ((symbol-function 'aob-acp-resume-entry)
                     (lambda (&rest _) (error "resumed"))))
            (aob-link-org-follow
             (aob-link--build "live-1" "claude" temporary-file-directory))
            (should (eq went s)))
        (aob-remove-session s)))))

(ert-deftest aob-link-follow-persisted-resumes ()
  (aob-link-tests--isolated
    (let ((entry (list :acp-id "old-1" :agent "claude" :name "old:1"
                       :dir temporary-file-directory))
          resumed)
      (with-temp-file aob-acp-persist-file (prin1 (list entry) (current-buffer)))
      (cl-letf (((symbol-function 'ygg-aob--goto)
                 (lambda (&rest _) (error "went")))
                ((symbol-function 'aob-acp-resume-entry)
                 (lambda (e &rest _) (setq resumed e))))
        (aob-link-org-follow
         (aob-link--build "old-1" "claude" temporary-file-directory))
        (should (equal (plist-get resumed :acp-id) "old-1"))))))

(ert-deftest aob-link-follow-missing-everything-errors ()
  (aob-link-tests--isolated
    (should-error
     (aob-link-org-follow (aob-link--build "none" "claude" "/no/such/dir::x"))
     :type 'user-error)))

(ert-deftest aob-link-follow-gone-offers-fresh-session ()
  (aob-link-tests--isolated
    (let (spawned)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'aob-acp-spawn-with)
                 (lambda (agent dir &rest _) (setq spawned (list agent dir)))))
        (aob-link-org-follow
         (aob-link--build "none" "claude" temporary-file-directory))
        (should (equal spawned
                       (list "claude" (expand-file-name temporary-file-directory))))))))

(ert-deftest aob-link-bookmark-record-and-handler ()
  (aob-link-tests--isolated
    (let ((s (aob-link-tests--session "bm-1" temporary-file-directory))
          went)
      (unwind-protect
          (with-temp-buffer
            (setq aob-buffer-session-id (aob-session-id s))
            (aob-link--enable-bookmark)
            (let ((record (funcall bookmark-make-record-function)))
              (should (equal (car record) "linked:1"))
              (should (eq (alist-get 'handler (cdr record))
                          #'aob-link-bookmark-jump))
              (should (equal (alist-get 'acp-id (cdr record)) "bm-1"))
              (should (equal (alist-get 'agent (cdr record)) "claude"))
              (should (equal (alist-get 'filename (cdr record))
                             temporary-file-directory))
              (cl-letf (((symbol-function 'ygg-aob--goto)
                         (lambda (x) (setq went x))))
                (aob-link-bookmark-jump record)
                (should (eq went s)))))
        (aob-remove-session s)))))

(provide 'aob-link-tests)

;;; aob-link-tests.el ends here
