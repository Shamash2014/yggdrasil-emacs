;;; ygg-qf-schedules-tests.el --- SPC a c S lists schedules in the quickfix -*- lexical-binding: t; -*-

;;; Code:

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'grep)
(yggdrasil-global-mode 1)
(eval-and-compile
  (defvar ygg-space-state-functions nil)
  (defvar ygg-space-detail-functions nil)
  (defvar ygg-leader-open-map (make-sparse-keymap))
  (defvar aob-acp-persist-file)
  (setq aob-acp-persist-file nil))
(require 'aob)
(require 'aob-acp)
(require 'aob-schedule)
(require 'layer-quickfix)
(require 'ygg-embark)

(defmacro ygg-qf-sched-tests--with (&rest body)
  (declare (indent 0))
  `(let ((aob-schedule-file (make-temp-file "qfsched-" nil ".eld"))
         (aob-schedule--timer nil)
         (aob-schedule-changed-hook nil)
         (target '(:acp-id "c" :name "alpha" :project "/tmp/p/"))
         (aob-schedule--list nil))
     (setq aob-schedule--list
           (list (list :id 2 :prompt "later" :target target :when "every 1h"
                       :next 2000000000.0)
                 (list :id 1 :prompt "soon\nmulti" :target target :when nil
                       :next 1900000000.0 :paused t)))
     (unwind-protect (progn ,@body)
       (when aob-schedule--timer (cancel-timer aob-schedule--timer))
       (delete-file aob-schedule-file)
       (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b)))))

(defun ygg-qf-sched-tests--text ()
  (with-current-buffer (ygg-qf-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ygg-qf-sched-tests--goto (text)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward text)
    (beginning-of-line)))

(ert-deftest ygg-qf-schedules-rows-and-notes ()
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (with-current-buffer (ygg-qf-buffer)
      (should (= 2 (ygg-qf--count-rows))))
    (should (string-match-p "alpha · once · soon multi" (ygg-qf-sched-tests--text)))
    (should (string-match-p "alpha · every 1h · later" (ygg-qf-sched-tests--text)))
    (should (< (string-match "soon" (ygg-qf-sched-tests--text))
               (string-match "later" (ygg-qf-sched-tests--text))))
    (ygg-qf-sched-tests--goto "soon")
    (should (string-suffix-p " · paused"
                             (get-text-property (line-beginning-position) 'ygg-qf-note)))
    (should-not (string-match-p "paused"
                                (progn (ygg-qf-sched-tests--goto "later")
                                       (get-text-property (line-beginning-position)
                                                          'ygg-qf-note))))))

(ert-deftest ygg-qf-schedules-default-action-only-describes ()
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (ygg-qf-sched-tests--goto "later")
    (let (msg)
      (cl-letf (((symbol-function 'aob-schedule--fire)
                 (lambda (&rest _) (ert-fail "ran")))
                ((symbol-function 'message)
                 (lambda (f &rest a) (setq msg (apply #'format f a)))))
        (with-current-buffer (ygg-qf-buffer) (ygg-qf-open)))
      (should (string-match-p "alpha: every 1h, next" msg))
      (should (= 2 (length aob-schedule--list))))))

(ert-deftest ygg-qf-schedules-empty-is-a-user-error ()
  (ygg-qf-sched-tests--with
    (setq aob-schedule--list nil)
    (should-error (aob-schedule-list) :type 'user-error)))

(ert-deftest ygg-qf-schedules-verbs-take-the-targeted-id-away-from-point ()
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (ygg-qf-sched-tests--goto "soon")
    (let ((ygg-qf--act-target (propertize "x" 'ygg-qf-id 2)) fired)
      (with-current-buffer (ygg-qf-buffer)
        (cl-letf (((symbol-function 'aob-schedule--fire)
                   (lambda (s) (push (plist-get s :id) fired))))
          (call-interactively #'aob-schedule-run-now)
          (should (equal fired '(2)))
          (call-interactively #'aob-schedule-toggle-pause)
          (should (plist-get (aob-schedule--by-id 2) :paused))
          (should (plist-get (aob-schedule--by-id 1) :paused))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (call-interactively #'aob-schedule-delete))
          (should-not (aob-schedule--by-id 2))
          (should (aob-schedule--by-id 1)))))))

(ert-deftest ygg-qf-schedules-edit-verb-edits-the-targeted-one ()
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (ygg-qf-sched-tests--goto "soon")
    (let ((ygg-qf--act-target (propertize "x" 'ygg-qf-id 2)))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "changed"))
                ((symbol-function 'aob-schedule--read-when)
                 (lambda (&rest _) "every 2h")))
        (with-current-buffer (ygg-qf-buffer) (call-interactively #'aob-schedule-edit)))
      (should (equal "changed" (plist-get (aob-schedule--by-id 2) :prompt)))
      (should (equal "soon\nmulti" (plist-get (aob-schedule--by-id 1) :prompt))))))

(ert-deftest ygg-qf-schedules-change-refreshes-keeping-point-on-the-row ()
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (ygg-qf-sched-tests--goto "later")
    (setf (plist-get (aob-schedule--by-id 2) :prompt) "renamed")
    (aob-schedule--changed)
    (should (string-match-p "renamed" (ygg-qf-sched-tests--text)))
    (with-current-buffer (ygg-qf-buffer)
      (should (equal (ygg-qf-kind-at-point) '(schedules . 2))))))

(ert-deftest ygg-qf-schedules-drop-deletes-after-confirming ()
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (ygg-qf-sched-tests--goto "later")
    (with-current-buffer (ygg-qf-buffer)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (ygg-qf-drop))
      (should (= 2 (length aob-schedule--list)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (ygg-qf-drop))
      (should (equal '(1) (mapcar (lambda (s) (plist-get s :id)) aob-schedule--list)))
      (should (= 1 (ygg-qf--count-rows))))))

(ert-deftest ygg-qf-schedules-follow-stops-when-replaced ()
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (should aob-schedule-changed-hook)
    (ygg-qf--collect (list "/tmp/foo.el:3: text") t)
    (should-not aob-schedule-changed-hook)))

(ert-deftest ygg-qf-schedules-old-panel-is-gone ()
  (should-not (fboundp 'aob-schedule-list-mode))
  (should-not (boundp 'aob-schedule-list-mode-map))
  (should-not (fboundp 'aob-schedule--entries))
  (should (eq 'aob-schedule-map (plist-get (alist-get 'schedules ygg-qf-kinds) :map))))

(ert-deftest ygg-qf-schedules-embark-offers-the-map ()
  (require 'embark)
  (should (eq 'aob-schedule-map (alist-get 'ygg-qf-schedules embark-keymap-alist)))
  (should (eq embark-general-map (keymap-parent aob-schedule-map)))
  (should (memq 'aob-schedule-run-now
                (ygg-embark-commands aob-schedule-map)))
  (ygg-qf-sched-tests--with
    (aob-schedule-list)
    (ygg-qf-sched-tests--goto "later")
    (with-current-buffer (ygg-qf-buffer)
      (let ((target (car (embark--targets))))
        (should (eq 'ygg-qf-schedules (plist-get target :type)))
        (should (equal "2" (substring-no-properties (plist-get target :target))))))))

(provide 'ygg-qf-schedules-tests)
;;; ygg-qf-schedules-tests.el ends here
