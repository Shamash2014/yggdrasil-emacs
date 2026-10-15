;;; ygg-project-import-read-tests.el --- Picking a folder to import -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'ygg-project-scan)

(defmacro ygg-project-import-read-tests--with (walked pick &rest body)
  "Run BODY with the walk finding WALKED and the prompt answering PICK.
`rows' is bound to what the prompt was offered, `note' to its annotator."
  (declare (indent 2))
  `(let (rows note browsed)
     (cl-letf (((symbol-function 'ygg-project-roots) (lambda (&rest _) '("/a/known/")))
               ((symbol-function 'ygg-project-scan--walk) (lambda () ,walked))
               ((symbol-function 'ygg-project-scan--save) #'identity)
               ((symbol-function 'read-directory-name)
                (lambda (&rest args) (setq browsed args) "/c/browsed/"))
               ((symbol-function 'completing-read)
                (lambda (_p coll &rest _)
                  (setq rows (all-completions "" coll)
                        note (alist-get 'annotation-function
                                        (cdr (funcall coll "" nil 'metadata))))
                  ,pick)))
       (let ((ygg-project-scan--found nil))
         ,@body))))

(ert-deftest ygg-project-import-read-offers-found-then-imported-then-browse ()
  (ygg-project-import-read-tests--with '("/a/known/" "/b/new/") ygg-project-import--browse
    (ygg-project-import--read-root)
    (should (equal rows (list "/b/new/" "/a/known/" ygg-project-import--browse)))
    (should (string-match-p "found" (funcall note "/b/new/")))
    (should (string-match-p "/b/" (funcall note "/b/new/")))
    (should (string-match-p "imported" (funcall note "/a/known/")))))

(ert-deftest ygg-project-import-read-browse-asks-for-any-folder ()
  (ygg-project-import-read-tests--with '("/b/new/") ygg-project-import--browse
    (should (equal (ygg-project-import--read-root) "/c/browsed/"))
    (should (equal (nth 1 browsed) "~/"))
    (should (eq (nth 3 browsed) t))))

(ert-deftest ygg-project-import-read-accepts-a-typed-folder ()
  (let ((dir (file-name-as-directory (make-temp-file "ygg-read" t))))
    (unwind-protect
        (ygg-project-import-read-tests--with '("/b/new/") dir
          (should (equal (ygg-project-import--read-root) dir)))
      (delete-directory dir t))))

(ert-deftest ygg-project-import-read-refuses-a-typed-non-folder ()
  (ygg-project-import-read-tests--with '("/b/new/") "/no/such/place-xyz"
    (should-error (ygg-project-import--read-root) :type 'user-error)))

;; a repository cloned after the saved scan was taken is not in it
(ert-deftest ygg-project-import-read-offers-a-clone-made-after-the-saved-scan ()
  (let* ((parent (file-name-as-directory (make-temp-file "ygg-clones" t)))
         (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment))
         (ygg-project-search-paths (list parent))
         (ygg-project-scan-cache-file (expand-file-name "scan.eld" parent))
         (ygg-project-scan--found nil)
         (clone (expand-file-name "fresh/" parent))
         rows)
    (unwind-protect
        (progn
          (ygg-project-scan--save (list (expand-file-name "old/" parent)))
          (call-process "git" nil nil nil "init" "-q" (expand-file-name "origin" parent))
          (call-process "git" nil nil nil "clone" "-q"
                        (expand-file-name "origin" parent) clone)
          (cl-letf (((symbol-function 'ygg-project-roots) (lambda (&rest _) nil))
                    ((symbol-function 'completing-read)
                     (lambda (_p coll &rest _)
                       (setq rows (all-completions "" coll))
                       ygg-project-import--browse))
                    ((symbol-function 'read-directory-name) (lambda (&rest _) "/x/")))
            (ygg-project-import--read-root))
          (should (member (abbreviate-file-name clone) rows)))
      (delete-directory parent t))))

(ert-deftest ygg-project-import-completes-for-a-fresh-clone ()
  (let* ((parent (file-name-as-directory (make-temp-file "ygg-clones" t)))
         (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment))
         (project-list-file (expand-file-name "projects.eld" parent))
         (project--list nil)
         (clone (expand-file-name "fresh/" parent))
         done)
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "init" "-q" (expand-file-name "origin" parent))
          (call-process "git" nil nil nil "clone" "-q"
                        (expand-file-name "origin" parent) clone)
          (cl-letf (((symbol-function 'ygg-project-commands-refresh)
                     (lambda (root cb) (funcall cb root))))
            (ygg-project-import clone (lambda (&rest _) (setq done t)))
            (let ((deadline (+ (float-time) 10)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.05))))
          (should done)
          (should (member (file-name-as-directory (file-truename clone))
                          (mapcar #'file-truename (ygg-project-roots)))))
      (delete-directory parent t))))

;; importing a folder that is not on the list yet puts it there, so the sidebar shows it
(ert-deftest ygg-project-import-remembers-a-new-repository ()
  (let* ((dir (file-name-as-directory (make-temp-file "ygg-import" t)))
         (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment))
         (project-list-file (make-temp-file "ygg-projects-list"))
         (project--list nil)
         ran)
    (unwind-protect
        (progn
          (let ((default-directory dir)) (call-process "git" nil nil nil "init" "-q"))
          (cl-letf (((symbol-function 'ygg-project-import--run)
                     (lambda (root &rest _) (setq ran root)))
                    ((symbol-function 'project--write-project-list) #'ignore))
            (ygg-project-import dir))
          (should ran)
          (should (member (file-name-as-directory (file-truename dir))
                          (mapcar (lambda (d) (file-name-as-directory (file-truename d)))
                                  (ygg-project-roots)))))
      (delete-directory dir t)
      (delete-file project-list-file))))

