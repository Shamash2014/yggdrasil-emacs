;;; aob-decisions-qf.el --- pending decisions of every agent in the quickfix -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'aob)

(declare-function ygg-qf-define-kind "layer-quickfix")
(declare-function ygg-qf-show-kind "layer-quickfix")
(declare-function ygg-qf-kind-refresh "layer-quickfix")
(declare-function ygg-qf-kind-target-id "layer-quickfix")
(declare-function aob-subagents--goto "aob-trace")
(defvar aob--views)

(defun ygg-aob--decision-age (s d)
  (if-let* ((ev (seq-find (lambda (e) (eql (plist-get e :seq) (plist-get d :seq)))
                          (aob-session-events s)))
            (ts (plist-get ev :ts)))
      (let ((secs (- (float-time) ts)))
        (cond ((< secs 60) "<1m")
              ((< secs 3600) (format "%dm" (truncate secs 60)))
              (t (format "%dh" (truncate secs 3600)))))
    "?"))

(defun ygg-aob--decision-kind-name (d)
  (pcase (plist-get d :kind)
    ('elicitation "question")
    ('auth "login")
    ('url "page")
    (kind (if kind (symbol-name kind) "permission"))))

(defun ygg-aob--decisions-collect ()
  (cl-loop for s in (aob-sessions)
           unless (eq (aob-session-state s) 'dead)
           append (mapcar
                   (lambda (d)
                     (list (cons (aob-session-id s) (plist-get d :seq))
                           (format "%s · %s%s"
                                   (aob-session-name s)
                                   (or (plist-get d :title) "decision")
                                   (if-let* ((det (plist-get d :detail)))
                                       (format "  [%s]" det)
                                     ""))
                           (format "%s · %s"
                                   (ygg-aob--decision-kind-name d)
                                   (ygg-aob--decision-age s d))))
                   (aob-session-decisions s))))

(defun ygg-aob--decision-of (id)
  "The session and decision a row ID stands for."
  (let* ((s (or (aob-session-get (car id)) (user-error "aob: that session is gone")))
         (d (or (seq-find (lambda (d) (eql (plist-get d :seq) (cdr id)))
                          (aob-session-decisions s))
                (user-error "aob: %s no longer waits on that answer"
                            (aob-session-name s)))))
    (cons s d)))

(defun ygg-aob--decision-answer (id)
  (pcase-let ((`(,s . ,d) (ygg-aob--decision-of id)))
    (aob-resolve s d)))

(defun ygg-aob--decision-open (id)
  (pcase-let ((`(,s . ,d) (ygg-aob--decision-of id)))
    (aob-subagents--goto s (plist-get d :seq))))

(defun ygg-aob-decision-answer (&optional id)
  "Answer the decision ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (ygg-aob--decision-answer (or id (user-error "aob: no decision here"))))

(defun ygg-aob-decision-open-trace (&optional id)
  "Show the trace of the decision ID, by default the one on this row, at its event."
  (interactive (list (ygg-qf-kind-target-id)))
  (ygg-aob--decision-open (or id (user-error "aob: no decision here"))))

(defvar ygg-aob-decision-map
  (let ((m (make-sparse-keymap)))
    (define-key m "y" #'ygg-aob-decision-answer)
    (define-key m "o" #'ygg-aob-decision-open-trace)
    m)
  "What embark offers on a decision row of the quickfix.")

(defun ygg-aob--decisions-arm (buf token)
  (letrec ((render (lambda ()
                     (unless (ygg-qf-kind-refresh buf token)
                       (funcall disarm))))
           (hook (lambda (&rest _)
                   (unless (ygg-qf-kind-refresh buf token)
                     (funcall disarm))))
           (disarm (lambda ()
                     (remove-hook 'aob-state-change-hook hook)
                     (remove-hook 'aob-session-removed-hook hook)
                     (when (eq (alist-get buf aob--views nil nil #'eq) render)
                       (setq aob--views (assq-delete-all buf aob--views))))))
    (add-hook 'aob-state-change-hook hook)
    (add-hook 'aob-session-removed-hook hook)
    (aob-register-view buf render)
    disarm))

(with-eval-after-load 'layer-quickfix
  (ygg-qf-define-kind 'decisions
                      :collect #'ygg-aob--decisions-collect
                      :action #'ygg-aob--decision-answer
                      :map 'ygg-aob-decision-map
                      :arm #'ygg-aob--decisions-arm
                      :glyph "■"))

(defun ygg-aob-decisions ()
  "List every agent's pending decisions in the quickfix; a row answers one."
  (interactive)
  (unless (ygg-aob--decisions-collect) (user-error "no pending decisions"))
  (require 'layer-quickfix)
  (ygg-qf-show-kind 'decisions))

(provide 'aob-decisions-qf)
;;; aob-decisions-qf.el ends here
