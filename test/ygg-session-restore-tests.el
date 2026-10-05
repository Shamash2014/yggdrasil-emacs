;;; ygg-session-restore-tests.el --- Sessions come back as left -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'layer-sessions)
(require 'easysession)

(defvar ygg-session-restore-tests--dir nil)

(defmacro ygg-session-restore-tests--fresh (&rest body)
  "Run BODY with one tab, empty buckets, no session, and a temp session dir."
  (declare (indent 0))
  `(let* ((easysession-directory (make-temp-file "ygg-es" t))
          (ygg-session-restore-tests--dir (make-temp-file "ygg-es-files" t))
          (easysession-confirm-new-session nil)
          (easysession-quiet t))
     (unwind-protect
         (progn
           (tab-bar-close-other-tabs)
           (clrhash ygg--space-buffers)
           (setq easysession--current-session-name nil
                 easysession--session-loaded nil)
           ,@body)
       (setq easysession--current-session-name nil
             easysession--session-loaded nil)
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p ygg-session-restore-tests--dir (buffer-file-name buf)))
           (kill-buffer buf)))
       (delete-directory easysession-directory t)
       (delete-directory ygg-session-restore-tests--dir t))))

(defun ygg-session-restore-tests--visit (name)
  "Visit file NAME in the test folder here, as a space sees a shown buffer."
  (let ((file (expand-file-name name ygg-session-restore-tests--dir)))
    (unless (file-exists-p file) (write-region name nil file))
    (find-file file)
    (ygg--space-track-buffer)
    (current-buffer)))

(defun ygg-session-restore-tests--tree ()
  (mapcar (lambda (tab) (list (ygg-space--id-of tab) (ygg-space--parent-of tab)))
          (ygg-space--tabs)))

(defun ygg-session-restore-tests--bucket (id)
  (sort (mapcar #'buffer-name (seq-filter #'buffer-live-p (gethash id ygg--space-buffers)))
        #'string<))

(defun ygg-session-restore-tests--saved-paths (name)
  (let ((data (with-temp-buffer
                (insert-file-contents (easysession-get-session-file-path name))
                (read (current-buffer)))))
    (mapcar (lambda (b) (file-name-nondirectory (alist-get 'buffer-path b)))
            (assoc-default "path-buffers" data))))

(ert-deftest ygg-session-switch-back-brings-the-latest-spaces-and-buckets ()
  (ygg-session-restore-tests--fresh
    (easysession-switch-to "A")
    (let ((root (ygg-space--current-id)) child sibling)
      (ygg-session-restore-tests--visit "a1")
      (ygg-session-restore-tests--visit "a1b")
      (setq child (progn (ygg-space-child) (ygg-space--current-id)))
      (ygg-session-restore-tests--visit "a2")
      (easysession-save "A")
      (setq sibling (progn (ygg-space-sibling) (ygg-space--current-id)))
      (ygg-session-restore-tests--visit "a3")
      (let ((tree (ygg-session-restore-tests--tree)))
        (should (equal tree `((,root 0) (,child ,root) (,sibling ,root))))
        (easysession-switch-to "B")
        (ygg-space--goto-id sibling)
        (tab-bar-close-tab)
        (ygg-space--goto-id child)
        (tab-bar-close-tab)
        (ygg-session-restore-tests--visit "b1")
        (easysession-switch-to "A")
        (should (equal (ygg-session-restore-tests--tree) tree))
        (should (eql (ygg-space--current-id) sibling))
        (should (equal (buffer-name (window-buffer)) "a3"))
        (should (equal (ygg-session-restore-tests--bucket root) '("a1" "a1b")))
        (should (equal (ygg-session-restore-tests--bucket child) '("a2")))
        (should (equal (ygg-session-restore-tests--bucket sibling) '("a3")))))))

(ert-deftest ygg-session-buckets-outlive-a-restart ()
  (ygg-session-restore-tests--fresh
    (easysession-switch-to "A")
    (let ((root (ygg-space--current-id)) child)
      (ygg-session-restore-tests--visit "a0")
      (ygg-session-restore-tests--visit "a1")
      (setq child (progn (ygg-space-child) (ygg-space--current-id)))
      (ygg-session-restore-tests--visit "a2")
      (ygg-space--set (ygg-space--current) 'ygg-agent-acp "acp-1")
      (easysession-save "A")
      (tab-bar-close-other-tabs)
      (mapc #'kill-buffer '("a0" "a1" "a2"))
      (clrhash ygg--space-buffers)
      (setq easysession--current-session-name nil
            easysession--session-loaded nil)
      (easysession-switch-to "A")
      (should (equal (ygg-session-restore-tests--tree) `((,root 0) (,child ,root))))
      (should (equal (alist-get 'ygg-agent-acp (ygg-space--tab-by-id child)) "acp-1"))
      (should (equal (ygg-session-restore-tests--bucket root) '("a0" "a1")))
      (should (equal (ygg-session-restore-tests--bucket child) '("a2"))))))

(ert-deftest ygg-session-buckets-hold-their-own-copy-of-a-shared-name ()
  (ygg-session-restore-tests--fresh
    (let ((a (expand-file-name "a/" ygg-session-restore-tests--dir))
          (b (expand-file-name "b/" ygg-session-restore-tests--dir)))
      (make-directory a) (make-directory b)
      (write-region "a" nil (expand-file-name "x.el" a))
      (write-region "b" nil (expand-file-name "x.el" b))
      (easysession-switch-to "B")
      (find-file (expand-file-name "x.el" b))
      (ygg--space-track-buffer)
      (easysession-save "B")
      (kill-buffer (current-buffer))
      (setq easysession--current-session-name nil
            easysession--session-loaded nil)
      (easysession-switch-to "A")
      (find-file (expand-file-name "x.el" a))
      (ygg--space-track-buffer)
      (easysession-switch-to "B")
      (let ((files (mapcar #'buffer-file-name
                           (gethash (ygg-space--current-id) ygg--space-buffers))))
        (should files)
        (should (seq-every-p (lambda (f) (string-prefix-p b f)) files))))))

(ert-deftest ygg-session-file-keeps-only-its-own-buffers ()
  (ygg-session-restore-tests--fresh
    (easysession-switch-to "B")
    (ygg-session-restore-tests--visit "b-only")
    (easysession-save "B")
    (easysession-switch-to "A")
    (ygg-session-restore-tests--visit "a-only")
    (easysession-switch-to "B")
    (easysession-save "B")
    (should (member "b-only" (ygg-session-restore-tests--saved-paths "B")))
    (should-not (member "a-only" (ygg-session-restore-tests--saved-paths "B")))))

(ert-deftest ygg-session-exit-leaves-an-unloaded-project-session-alone ()
  (ygg-session-restore-tests--fresh
    (let* ((name "proj")
           (file (expand-file-name name easysession-directory)))
      (write-region "(saved earlier)" nil file)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'proj))
                ((symbol-function 'ygg-session--project-name) (lambda (&rest _) name)))
        (ygg-session--save-on-exit))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                     "(saved earlier)")))))

(ert-deftest ygg-session-saved-by-name-is-autosaved-after ()
  (ygg-session-restore-tests--fresh
    (cl-letf (((symbol-function 'ygg-session--project-name) (lambda (&rest _) "proj"))
              ((symbol-function 'easysession--frame-list) (lambda () (list (selected-frame)))))
      (ygg-session-save-project)
      (delete-file (easysession-get-session-file-path "proj"))
      (easysession--auto-save)
      (should (file-exists-p (easysession-get-session-file-path "proj"))))))

;; with no user frame left the frameset is empty, and loading it keeps whatever tabs are up
(ert-deftest ygg-session-exit-without-a-frame-keeps-the-saved-layout ()
  (ygg-session-restore-tests--fresh
    (let ((file (expand-file-name "A" easysession-directory)))
      (write-region "(saved earlier)" nil file)
      (setq easysession--current-session-name "A"
            easysession--session-loaded t)
      (cl-letf (((symbol-function 'easysession--frame-list) #'ignore))
        (ygg-session--save-on-exit))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                     "(saved earlier)")))))

(ert-deftest ygg-session-failed-load-leaves-no-buckets-for-a-new-session ()
  (ygg-session-restore-tests--fresh
    (easysession-switch-to "A")
    (ygg-session-restore-tests--visit "a1")
    (get-buffer-create "stale-bucket-buffer")
    (setq ygg--space-buffers-loaded
          `(((,(ygg-space--current-id) (nil . "stale-bucket-buffer")))))
    (easysession-switch-to "Fresh")
    (should (equal (ygg-session-restore-tests--bucket (ygg-space--current-id))
                   '("a1")))
    (kill-buffer "stale-bucket-buffer")))

(ert-deftest ygg-session-restore-skips-a-namesake-another-space-holds ()
  (ygg-session-restore-tests--fresh
    (easysession-switch-to "A")
    (let ((root (ygg-space--current-id))
          (namesake (get-buffer-create "shared-name")))
      (ygg-space-child)
      (puthash (ygg-space--current-id) (list namesake) ygg--space-buffers)
      (setq ygg--space-buffers-loaded `(((,root (nil . "shared-name")))))
      (ygg--space-buffers-restore)
      (should-not (gethash root ygg--space-buffers))
      (kill-buffer namesake))))

;;; ygg-session-restore-tests.el ends here
