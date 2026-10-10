;;; ygg-dap-picker-tests.el --- Adapter picker through completing-read -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-dap)
(require 'dape)

(defmacro ygg-dap-picker-tests--with (&rest body)
  `(with-temp-buffer
     (emacs-lisp-mode)
     (let ((dape-configs '((good-a modes (emacs-lisp-mode) command "a-bin")
                           (good-b modes (emacs-lisp-mode) command "b-bin")
                           (bad-c modes (python-mode) command "c-bin")))
           (dape-command nil)
           (dape-history '("good-b :program \"x\"" "good-a :program \"y\""))
           (dape-history-add 'input)
           (dape-read-config-hook nil))
       ,@body)))

(defun ygg-dap-picker-tests--collect (table)
  (let ((all (all-completions "" table)))
    (list all (completion-metadata "" table nil))))

(ert-deftest ygg-dap-picker-tests-candidates-order-and-category ()
  (ygg-dap-picker-tests--with
   (let (seen)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_p table &rest _) (setq seen table) ygg-dape--edit-entry)))
       (ygg-dape--read-config-picker (lambda () 'orig)))
     (pcase-let ((`(,cands ,meta) (ygg-dap-picker-tests--collect seen)))
       (should (equal cands '("good-b :program \"x\"" "good-a" "good-b"
                              "good-a :program \"y\"" "✎ edit…")))
       (should (eq (completion-metadata-get meta 'category) 'ygg-dape-config))
       (should (equal (funcall (completion-metadata-get meta 'annotation-function) "good-a")
                      "  suggested  a-bin"))))))

(ert-deftest ygg-dap-picker-tests-invalid-config-absent ()
  (ygg-dap-picker-tests--with
   (let ((cands (mapcar #'car (ygg-dape--candidates))))
     (should-not (member "bad-c" cands))
     (should (member "good-a" cands)))))

(ert-deftest ygg-dap-picker-tests-choice-matches-original-parse ()
  (ygg-dap-picker-tests--with
   (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "good-a")))
     (let* ((expected (pcase-let ((`(,key ,config) (dape--config-from-string "good-a")))
                        (dape--config-eval key config)))
            (got (ygg-dape--read-config-picker (lambda () 'orig))))
       (should (equal got expected))))))

(ert-deftest ygg-dap-picker-tests-pushes-history-unless-input ()
  (ygg-dap-picker-tests--with
   (let ((dape-history-add t)
         (dape-history nil))
     (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "good-a")))
       (ygg-dape--read-config-picker (lambda () 'orig)))
     (should (= (length dape-history) 1)))))

(ert-deftest ygg-dap-picker-tests-edit-calls-original ()
  (ygg-dap-picker-tests--with
   (cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) ygg-dape--edit-entry)))
     (should (eq (ygg-dape--read-config-picker (lambda () 'orig)) 'orig)))))

(ert-deftest ygg-dap-picker-tests-edit-sentinel-never-in-history ()
  (ygg-dap-picker-tests--with
   (cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _)
                (when history-add-new-input
                  (add-to-history 'dape-history ygg-dape--edit-entry))
                ygg-dape--edit-entry)))
     (ygg-dape--read-config-picker (lambda () 'orig))
     (should-not (member ygg-dape--edit-entry dape-history)))))

(ert-deftest ygg-dap-picker-tests-hook-runs-once-on-both-paths ()
  (dolist (choice (list ygg-dape--edit-entry "good-a"))
    (ygg-dap-picker-tests--with
     (let* ((count 0)
            (dape-read-config-hook (list (lambda () (cl-incf count)))))
       (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) choice)))
         (ygg-dape--read-config-picker
          (lambda () (run-hooks 'dape-read-config-hook) 'orig)))
       (should (= count 1))))))

(ert-deftest ygg-dap-picker-tests-invalid-history-entry-absent ()
  (ygg-dap-picker-tests--with
   (let ((dape-history (cons "bad-c :program \"z\"" dape-history)))
     (should-not (member "bad-c :program \"z\"" (mapcar #'car (ygg-dape--candidates))))
     (should (member "good-b :program \"x\"" (mapcar #'car (ygg-dape--candidates)))))))

(ert-deftest ygg-dap-picker-tests-input-mode-adds-chosen-string ()
  (ygg-dap-picker-tests--with
   (let ((dape-history nil))
     (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "good-a")))
       (ygg-dape--read-config-picker (lambda () 'orig)))
     (should (equal dape-history '("good-a"))))))

(ert-deftest ygg-dap-picker-tests-continue-reaches-picker ()
  (ygg-dap-picker-tests--with
   (let (picked got)
     (cl-letf (((symbol-function 'dape--live-connections) (lambda () nil))
               ((symbol-function 'completing-read)
                (lambda (&rest _) (setq picked t) "good-a"))
               ((symbol-function 'dape--config-ensure) (lambda (&rest _) t))
               ((symbol-function 'dape)
                (lambda (config)
                  (interactive (list (dape--read-config)))
                  (setq got config))))
       (ygg-dape-continue)
       (should picked)
       (should got)))))

(provide 'ygg-dap-picker-tests)
;;; ygg-dap-picker-tests.el ends here
