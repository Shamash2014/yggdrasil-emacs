;;; ygg-aob-kill-all-tests.el --- closing every agent in a space -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'layer-aob)

(defmacro ygg-aob-kill-all-tests--with (var spaces &rest body)
  (declare (indent 2))
  `(let ((,var (cl-loop for sp in ,spaces for i from 0
                        collect (aob-create-session
                                 :id (format "kill-all:%d" i) :backend 'kill-all-test
                                 :name (format "ka%d" i) :state 'idle))))
     (cl-loop for s in ,var for sp in ,spaces do (aob-session-put s :space sp))
     (unwind-protect (progn ,@body)
       (mapc #'aob-remove-session ,var))))

(ert-deftest ygg-aob-kill-all-kills-only-this-space ()
  (let (killed prompt)
    (aob-register-backend 'kill-all-test
                          (list :kill (lambda (s &rest _) (push (aob-session-name s) killed))))
    (ygg-aob-kill-all-tests--with ss '(1 1 2)
      (cl-letf (((symbol-function 'ygg-space--current-id) (lambda () 1))
                ((symbol-function 'ygg-aob--space-live-p) (lambda (_) t))
                ((symbol-function 'yes-or-no-p) (lambda (p) (setq prompt p) t)))
        (ygg-aob-kill-all))
      (should (equal (sort killed #'string<) '("ka0" "ka1")))
      (should (string-match-p "ka0" prompt))
      (should-not (string-match-p "ka2" prompt)))))

(ert-deftest ygg-aob-kill-all-declines-when-asked-no ()
  (let (killed)
    (aob-register-backend 'kill-all-test
                          (list :kill (lambda (s &rest _) (push s killed))))
    (ygg-aob-kill-all-tests--with ss '(1 1)
      (cl-letf (((symbol-function 'ygg-space--current-id) (lambda () 1))
                ((symbol-function 'ygg-aob--space-live-p) (lambda (_) t))
                ((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (ygg-aob-kill-all))
      (should-not killed))))

(ert-deftest ygg-aob-kill-all-prompt-counts-queued-prompts ()
  (let (prompt reasked)
    (aob-register-backend 'kill-all-test (list :kill #'ignore))
    (ygg-aob-kill-all-tests--with ss '(1 1)
      (aob-session-put (car ss) :queued '("a" "b" "c"))
      (aob-session-put (cadr ss) :queued '("d"))
      (cl-letf (((symbol-function 'ygg-space--current-id) (lambda () 1))
                ((symbol-function 'ygg-aob--space-live-p) (lambda (_) t))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) (setq reasked t)))
                ((symbol-function 'yes-or-no-p) (lambda (p) (setq prompt p) t)))
        (ygg-aob-kill-all))
      (should (string-match-p "drop 4 queued prompts" prompt))
      (should-not reasked)
      (should-not (aob-session-ref (car ss) :queued)))))

;;; ygg-aob-kill-all-tests.el ends here
