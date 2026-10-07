;;; ygg-command-panel-tests.el --- The command panel lists tasks and commands -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar ygg-leader-open-map (make-sparse-keymap))
(require 'layer-tasks)

(defun ygg-command-panel-tests--candidates ()
  (let (shown)
    (cl-letf (((symbol-function 'ygg-task--collect)
               (lambda (&rest _) (list (list :label "just: build" :command "just build" :directory "/"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _) (setq shown (all-completions "" table)) "")))
      (ignore-errors (ygg-command-panel)))
    shown))

(require 'prescient nil t)
(defvar vertico-sort-function nil)

(defmacro ygg-command-panel-tests--with-prescient (&rest body)
  `(let ((prescient--history (make-hash-table :test 'equal))
         (prescient--frequency (make-hash-table :test 'equal))
         (prescient-persist-mode nil)
         (vertico-sort-function #'prescient-completion-sort))
     ,@body))

(defun ygg-command-panel-tests--sorted (input &optional jobs)
  (let (table)
    (cl-letf (((symbol-function 'ygg--panel-input) (lambda () input))
              ((symbol-function 'ygg-task--collect)
               (lambda (&rest _) (list (list :label "just: build" :command "just build" :directory "/"))))
              ((symbol-function 'ygg--panel-job-buffers) (lambda () jobs))
              ((symbol-function 'completing-read)
               (lambda (_prompt tbl &rest _) (setq table tbl) "")))
      (ignore-errors (ygg-command-panel))
      (let ((sorter (completion-metadata-get (completion-metadata "" table nil) 'display-sort-function))
            (cands (seq-filter (lambda (c) (string-match-p (regexp-quote input) c))
                               (all-completions "" table))))
        (funcall sorter cands)))))

(ert-deftest ygg-command-panel/lists-tasks-first-and-every-command ()
  (let ((shown (ygg-command-panel-tests--candidates)))
    (should (equal (car shown) "▶ run  just: build"))
    (should (member "find-file" shown))))

(ert-deftest ygg-command-panel/specials-precede-commands ()
  (ygg-command-panel-tests--with-prescient
   (let* ((job (generate-new-buffer "panel-job"))
          (sorted (ygg-command-panel-tests--sorted "" (list job))))
     (unwind-protect
         (progn
           (should (equal (seq-take sorted 2) (list "▶ run  just: build" "⚙ job  panel-job")))
           (should (member "find-file" sorted)))
       (kill-buffer job)))))

(ert-deftest ygg-command-panel/remembered-command-ranks-before-untouched ()
  (ygg-command-panel-tests--with-prescient
   (prescient-remember "ygg-task-run")
   (let ((sorted (cdr (ygg-command-panel-tests--sorted ""))))
     (should (equal (car sorted) "ygg-task-run")))))

(ert-deftest ygg-command-panel/typed-input-ranks-like-m-x ()
  (ygg-command-panel-tests--with-prescient
   (prescient-remember "ygg-task-run")
   (let* ((job (generate-new-buffer "*task:build*"))
          (sorted (unwind-protect (ygg-command-panel-tests--sorted "task" (list job))
                    (kill-buffer job)))
          (expected (prescient-completion-sort
                     (cons "⚙ job  *task:build*"
                           (seq-filter (lambda (c) (string-match-p "task" c))
                                       (ygg--panel-commands))))))
     (should (equal (car sorted) "ygg-task-run"))
     (should (equal sorted expected)))))

(ert-deftest ygg-command-panel/job-list-skips-internal-process-buffers ()
  (let* ((hidden (generate-new-buffer " *ygg-device*"))
         (named (generate-new-buffer "*Async-native-compile-log*"))
         (shown (generate-new-buffer "*task:build*"))
         (procs (mapcar (lambda (b) (make-pipe-process :name (buffer-name b) :buffer b :noquery t))
                        (list hidden named shown))))
    (unwind-protect
        (let ((jobs (ygg--panel-job-buffers)))
          (should (memq shown jobs))
          (should-not (memq hidden jobs))
          (should-not (memq named jobs)))
      (mapc #'delete-process procs)
      (mapc #'kill-buffer (list hidden named shown)))))

(provide 'ygg-command-panel-tests)
;;; ygg-command-panel-tests.el ends here
