;;; ygg-project-import-read-tests.el --- Picking a folder to import -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'ygg-project-scan)

(defmacro ygg-project-import-read-tests--with (fresh pick &rest body)
  "Run BODY with the scan cache at FRESH and the prompt answering PICK.
`rows' is bound to what the prompt was offered, `note' to its annotator."
  (declare (indent 2))
  `(let ((asyncs 0) rows note browsed)
     (cl-letf (((symbol-function 'ygg-project-roots) (lambda (&rest _) '("/a/known/")))
               ((symbol-function 'ygg-project-scan--load) (lambda (&optional any-age) (or ,fresh (and any-age '("/c/stale/")))))
               ((symbol-function 'ygg-project-scan-async)
                (lambda (&rest _) (cl-incf asyncs)))
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

(ert-deftest ygg-project-import-read-stale-cache-refreshes-behind-the-prompt ()
  (ygg-project-import-read-tests--with nil ygg-project-import--browse
    (ygg-project-import--read-root)
    (should (= asyncs 1))
    (should (equal rows (list "/c/stale/" "/a/known/" ygg-project-import--browse)))))

(ert-deftest ygg-project-import-read-fresh-cache-starts-no-walk ()
  (ygg-project-import-read-tests--with '("/b/new/") ygg-project-import--browse
    (ygg-project-import--read-root)
    (should (= asyncs 0))))
