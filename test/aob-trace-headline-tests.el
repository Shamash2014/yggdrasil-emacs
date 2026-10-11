;;; aob-trace-headline-tests.el --- The trace's two-line headline -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'aob)
(require 'aob-acp)
(setq aob-acp-persist-file (make-temp-file "aob-trace-headline-sessions-" nil ".eld"))
(require 'aob-trace)

(defmacro aob-trace-headline-tests--with (var &rest body)
  (declare (indent 1))
  `(let* ((,var (aob-create-session :id "trace:headline" :backend 'acp
                                    :name "headline" :state 'idle))
          (buf (aob-trace-buffer ,var)))
     (unwind-protect
         (with-current-buffer buf ,@body)
       (kill-buffer buf)
       (aob-remove-session ,var))))

(defun aob-trace-headline-tests--meter ()
  (overlay-get aob-trace--meter-overlay 'before-string))

(ert-deftest aob-trace-headline-has-a-title-line-and-a-meter-line ()
  (aob-trace-headline-tests--with s
    (aob-session-put s :ctx-used 213400)
    (aob-session-put s :ctx-size 1000000)
    (aob-session-put s :cost-reading 1.5)
    (aob-session-put s :work-secs 125)
    (aob-trace--render t)
    (should (string-match-p "\\` headline" header-line-format))
    (should-not (string-match-p "\n" header-line-format))
    (let ((meter (aob-trace-headline-tests--meter)))
      (should (string-match-p "213k/1\\.0M ctx" meter))
      (should (string-match-p "\\$1\\.50" meter))
      (should (string-match-p "2m" meter))
      (should (= 1 (cl-count ?\n meter)))
      (should (string-suffix-p "\n" meter)))))

(ert-deftest aob-trace-headline-keeps-the-other-header-parts-on-line-one ()
  (aob-trace-headline-tests--with s
    (aob-session-put s :approve-all t)
    (aob-session-put s :mode-id "plan")
    (aob-session-put s :model-name "opus")
    (aob-trace--render t)
    (dolist (part '("approve-all" "plan" "opus" "idle"))
      (should (string-search part header-line-format))
      (should-not (string-search part (aob-trace-headline-tests--meter))))))

(ert-deftest aob-trace-headline-unknown-values-are-a-dash-not-an-error ()
  (aob-trace-headline-tests--with s
    (aob-trace--render t)
    (should (equal " —\n" (substring-no-properties (aob-trace-headline-tests--meter))))
    (aob-session-put s :ctx-size 1000000)
    (aob-session-put s :cost-reading 0)
    (should (equal " —\n" (substring-no-properties (aob-trace--meter-line s))))
    (aob-session-put s :cost-reading 2.0)
    (should (string-match-p "\\`  ?\\$2\\.00\n\\'" (substring-no-properties
                                                   (aob-trace--meter-line s))))))

(ert-deftest aob-trace-headline-is-not-padded-to-the-window ()
  (aob-trace-headline-tests--with s
    (aob-session-put s :ctx-used 5000)
    (aob-session-put s :cost-reading 0.25)
    (aob-trace--render t)
    (let ((meter (aob-trace-headline-tests--meter)))
      (should-not (string-search "  " meter))
      (should-not (next-single-property-change 0 'display meter))
      (should (< (string-width meter) 40)))
    (should-not (get-text-property 0 'display header-line-format))
    (should-not (string-search ":align-to" (format "%S" header-line-format)))
    (should-not (string-search ":align-to" (format "%S" (aob-trace-headline-tests--meter))))
    (should-not (overlay-get aob-trace--meter-overlay 'after-string))))

(ert-deftest aob-trace-headline-meter-follows-the-session-without-a-redraw ()
  (aob-trace-headline-tests--with s
    (aob-trace--render t)
    (aob-session-put s :cost-reading 3.0)
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (aob-trace--meter-changed s)
      (should (string-match-p "\\$3\\.00" (aob-trace-headline-tests--meter))))))

(ert-deftest aob-trace-headline-line-two-survives-a-full-redraw ()
  (aob-trace-headline-tests--with s
    (aob-session-put s :cost-reading 4.0)
    (aob-trace--render t)
    (let ((inhibit-read-only t)) (erase-buffer))
    (aob-trace--render t)
    (should (= (point-min) (overlay-start aob-trace--meter-overlay)))
    (should (string-match-p "\\$4\\.00" (aob-trace-headline-tests--meter)))))

(provide 'aob-trace-headline-tests)
