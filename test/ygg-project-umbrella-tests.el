;;; ygg-project-umbrella-tests.el --- A plain folder of repos as a project -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ygg-project-scan)

(defmacro ygg-umbrella-tests--with (umbrella imported &rest body)
  "Run BODY with UMBRELLA a temp folder of two repos and a plain folder.
The project list is empty and private to BODY, and IMPORTED collects
every root `ygg-project-import' is asked for."
  (declare (indent 2))
  `(let* ((,umbrella (file-name-as-directory
                      (file-truename (make-temp-file "ygg-umbrella" t))))
          (,imported nil)
          (project--list nil)
          (project-list-file (expand-file-name "projects.eld" ,umbrella))
          (ygg-project--children (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (dolist (child '("alpha" "beta"))
             (let ((default-directory (expand-file-name child ,umbrella)))
               (make-directory default-directory)
               (call-process "git" nil nil nil "init" "-q")))
           (make-directory (expand-file-name "plain" ,umbrella))
           (cl-letf (((symbol-function 'ygg-project-import)
                      (lambda (root &optional callback _extras)
                        (push (ygg-project--key root) ,imported)
                        (when callback (funcall callback root)))))
             ,@body))
       (delete-directory ,umbrella t))))

(ert-deftest ygg-umbrella-add-imports-umbrella-and-children ()
  (ygg-umbrella-tests--with dir imported
    (should (equal (ygg-project-add dir) dir))
    (should (equal (sort (copy-sequence imported) #'string<)
                   (sort (list dir (concat dir "alpha/") (concat dir "beta/"))
                         #'string<)))
    (should (member (concat dir "alpha/") (ygg-project-roots)))
    (should (member dir (ygg-project-roots)))))

(ert-deftest ygg-umbrella-project-current-by-place ()
  (ygg-umbrella-tests--with dir _imported
    (ygg-project-add dir)
    (should (equal (project-root (project-current nil (concat dir "plain/")))
                   dir))
    (let ((child (project-current nil (concat dir "alpha/"))))
      (should (eq (car child) 'vc))
      (should (equal (ygg-project--key (project-root child))
                     (concat dir "alpha/"))))))

(ert-deftest ygg-umbrella-children-are-the-repos ()
  (ygg-umbrella-tests--with dir _imported
    (ygg-project-add dir)
    (should (equal (sort (ygg-project-children dir) #'string<)
                   (list (concat dir "alpha/") (concat dir "beta/"))))
    (should-not (ygg-project-children (concat dir "alpha/")))))

(ert-deftest ygg-umbrella-remove-unregisters-but-keeps-children ()
  (ygg-umbrella-tests--with dir _imported
    (ygg-project-add dir)
    (ygg-project-remove dir)
    (should-not (ygg-project-children dir))
    (should-not (project-current nil (concat dir "plain/")))
    (should (member (concat dir "beta/") (ygg-project-roots)))))

(ert-deftest ygg-umbrella-folder-without-repos-still-errors ()
  (ygg-umbrella-tests--with dir imported
    (should-error (ygg-project-add (concat dir "plain/")) :type 'user-error)
    (should-not imported)))

;;; ygg-project-umbrella-tests.el ends here
