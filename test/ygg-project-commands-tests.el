;;; ygg-project-commands-tests.el --- CI in the commands picker -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(require 'ygg-project-commands)
(require 'ygg-git-ci)

(defconst ygg-project-commands-tests--items
  '((:label "workflow: deploy" :kind workflow :name "deploy"
     :workflow (:name "deploy" :id 7 :file "/x/deploy.yml"))
    (:label "job: e2e" :kind job :name "e2e" :row (:name "e2e" :playable t))
    (:label "new pipeline: main" :kind pipeline :name "main")))

(defmacro ygg-project-commands-tests--with-root (var workflow &rest body)
  (declare (indent 2))
  `(let* ((,var (file-name-as-directory (make-temp-file "ygg-pc" t)))
          (ygg-project-commands--ci (make-hash-table :test #'equal))
          (ygg-project-commands--cache (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (when ,workflow
             (make-directory (expand-file-name ".github/workflows" ,var) t)
             (with-temp-file (expand-file-name ".github/workflows/ship.yml" ,var)
               (insert "name: Ship\non:\n  workflow_dispatch:\n")))
           ,@body)
       (delete-directory ,var t))))

(defun ygg-project-commands-tests--names (root)
  (mapcar (lambda (c) (plist-get c :name)) (ygg-project-commands root)))

(ert-deftest ygg-project-commands-ci-appears-after-async-fill ()
  (ygg-project-commands-tests--with-root root nil
    (let (callback)
      (cl-letf (((symbol-function 'ygg-git-ci-startable)
                 (lambda (_root cb) (setq callback cb))))
        (ygg-project-commands-refresh-ci root)
        (should-not (ygg-project-commands root))
        (funcall callback ygg-project-commands-tests--items)
        (should (equal (ygg-project-commands-tests--names root)
                       '("ci: workflow deploy" "ci: job e2e" "ci: new pipeline")))
        (should (cl-every (lambda (c) (eq (plist-get c :source) 'ci))
                          (ygg-project-commands root)))))))

(ert-deftest ygg-project-commands-ci-run-starts-the-item ()
  (ygg-project-commands-tests--with-root root nil
    (let (started)
      (cl-letf (((symbol-function 'ygg-git-ci-startable)
                 (lambda (_root cb) (funcall cb ygg-project-commands-tests--items)))
                ((symbol-function 'ygg-git-ci-start)
                 (lambda (r item) (setq started (list r item))))
                ((symbol-function 'compile)
                 (lambda (&rest _) (error "ran a shell command"))))
        (ygg-project-commands-refresh-ci root)
        (ygg-project-commands-run (cadr (ygg-project-commands root)))
        (should (equal started
                       (list root (nth 1 ygg-project-commands-tests--items))))))))

(ert-deftest ygg-project-commands-ci-local-workflows-need-no-network ()
  (ygg-project-commands-tests--with-root root t
    (cl-letf (((symbol-function 'ygg-git-ci-startable) (lambda (&rest _) nil)))
      (ygg-project-commands-refresh-ci root)
      (should (equal (ygg-project-commands-tests--names root) '("ci: workflow Ship")))
      (let ((item (plist-get (car (ygg-project-commands root)) :ci)))
        (should (equal (plist-get (plist-get item :workflow) :id) "ship.yml"))))))

(ert-deftest ygg-project-commands-ci-fetched-replaces-local ()
  (ygg-project-commands-tests--with-root root t
    (let (callback)
      (cl-letf (((symbol-function 'ygg-git-ci-startable)
                 (lambda (_root cb) (setq callback cb))))
        (ygg-project-commands-refresh-ci root)
        (funcall callback (list (car ygg-project-commands-tests--items)))
        (should (equal (ygg-project-commands-tests--names root)
                       '("ci: workflow deploy")))))))

(ert-deftest ygg-project-commands-ci-none-without-a-forge ()
  (ygg-project-commands-tests--with-root root t
    (cl-letf (((symbol-function 'ygg-git-ci-startable)
               (lambda (&rest _) (user-error "ci: not a forge remote"))))
      (ygg-project-commands-refresh-ci root)
      (should-not (ygg-project-commands root)))))

(ert-deftest ygg-project-commands-ci-never-waits-on-the-forge ()
  (ygg-project-commands-tests--with-root root nil
    (let ((calls 0))
      (cl-letf (((symbol-function 'ygg-git-ci-startable)
                 (lambda (&rest _) (cl-incf calls))))
        (let ((start (float-time)))
          (ygg-project-commands-refresh-ci root)
          (ygg-project-commands-refresh-ci root)
          (should (< (- (float-time) start) 1))
          (should-not (ygg-project-commands root))
          (should (= calls 1)))))))

(ert-deftest ygg-project-commands-ci-silent-forge-is-asked-again-after-the-ttl ()
  (ygg-project-commands-tests--with-root root nil
    (let ((calls 0))
      (cl-letf (((symbol-function 'ygg-git-ci-startable)
                 (lambda (&rest _) (cl-incf calls))))
        (ygg-project-commands-refresh-ci root)
        (let ((key (ygg-project-commands--key root)))
          (puthash key (plist-put (gethash key ygg-project-commands--ci) :asked
                                  (- (float-time) ygg-project-commands--ci-ttl 1))
                   ygg-project-commands--ci))
        (ygg-project-commands-refresh-ci root)
        (should (= calls 2))))))

(ert-deftest ygg-project-commands-ci-failed-refresh-keeps-what-it-had ()
  (ygg-project-commands-tests--with-root root nil
    (cl-letf (((symbol-function 'ygg-git-ci-startable)
               (lambda (_root cb) (funcall cb ygg-project-commands-tests--items))))
      (ygg-project-commands-refresh-ci root))
    (let ((key (ygg-project-commands--key root)))
      (puthash key (plist-put (gethash key ygg-project-commands--ci) :asked 0)
               ygg-project-commands--ci))
    (cl-letf (((symbol-function 'ygg-git-ci-startable)
               (lambda (&rest _) (error "forge down"))))
      (ygg-project-commands-refresh-ci root))
    (should (equal (length (ygg-project-commands root)) 3))))

(provide 'ygg-project-commands-tests)
;;; ygg-project-commands-tests.el ends here
