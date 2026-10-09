;;; ygg-aob-localleader-tests.el --- Agent trace/plan localleader layout -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defvar ygg-space-state-functions nil)
(defvar ygg-space-detail-functions nil)
(require 'yggdrasil)
(require 'layer-aob)

(defconst ygg-aob-ll-tests--both
  '(("S" . aob-cancel) ("x" . aob-acp-command) ("d" . aob-todo)
    ("f" . aob-dired) ("n" . aob-rename-session) ("P" . ygg-projects-toggle-pin)
    ("m m" . aob-acp-set-mode) ("m n" . aob-acp-cycle-mode) ("m l" . aob-acp-model)
    ("m L" . aob-acp-backend) ("m e" . aob-acp-config) ("m w" . aob-acp-worker-effort)
    ("m c" . aob-acp-mcp) ("m g" . aob-acp-goal) ("m r" . aob-transcript-wake)
    ("m f" . aob-acp-add-folder)
    ("z z" . aob-acp-compact) ("z Z" . aob-acp-clear) ("z n" . aob-acp-new)
    ("i a" . ygg-aob-activity) ("i t" . ygg-aob-subagents)))

(defconst ygg-aob-ll-tests--trace-only
  '(("A" . aob-answer) ("z h" . aob-handoff) ("i u" . aob-trace-usage)
    ("q e" . aob-trace-queue-edit) ("q d" . aob-trace-queue-drop)
    ("q s" . aob-trace-queue-steer) ("q RET" . aob-trace-queue-send-now)
    ("q k" . aob-trace-queue-earlier) ("q j" . aob-trace-queue-later)))

(defun ygg-aob-ll-tests--lookup (mode key)
  (let ((def (lookup-key (ygg-localleader--get-map mode) (kbd key))))
    (cond ((numberp def) nil)
          ((and (consp def) (stringp (car def))) (cdr def))
          (t def))))

(ert-deftest ygg-aob-localleader-both-modes ()
  (dolist (mode '(aob-trace-mode aob-plan-mode))
    (pcase-dolist (`(,key . ,command) ygg-aob-ll-tests--both)
      (should (eq (ygg-aob-ll-tests--lookup mode key) command)))))

(ert-deftest ygg-aob-localleader-trace-only ()
  (pcase-dolist (`(,key . ,command) ygg-aob-ll-tests--trace-only)
    (should (eq (ygg-aob-ll-tests--lookup 'aob-trace-mode key) command))
    (should-not (ygg-aob-ll-tests--lookup 'aob-plan-mode key))))

(ert-deftest ygg-aob-localleader-prefix-labels ()
  (dolist (mode '(aob-trace-mode aob-plan-mode))
    (let ((map (ygg-localleader--get-map mode)))
      (pcase-dolist (`(,key . ,label) '(("m" . "settings") ("z" . "context") ("i" . "info")))
        (should (equal (car (cdr (assq (aref key 0) map))) label)))))
  (should (equal (car (cdr (assq ?q (ygg-localleader--get-map 'aob-trace-mode)))) "queued"))
  (should-not (assq ?q (ygg-localleader--get-map 'aob-plan-mode))))

(ert-deftest ygg-aob-localleader-old-keys-gone ()
  (dolist (mode '(aob-trace-mode aob-plan-mode))
    (dolist (key '("y" "b" "p" "k" "w" "a" "t" "u" "e" "X" "s" "J" "K" "l" "L" "E" "c" "g" "r" "F" "W" "N" "H" "M" "Z" "z"))
      (let ((def (ygg-aob-ll-tests--lookup mode key)))
        (should-not (and def (symbolp def)))))))

(provide 'ygg-aob-localleader-tests)
;;; ygg-aob-localleader-tests.el ends here
