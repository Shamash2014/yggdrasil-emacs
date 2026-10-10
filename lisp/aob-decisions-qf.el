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
(declare-function ygg-qf-kind-at-point "layer-quickfix")
(declare-function aob-ask-reject-reason "aob")
(defvar aob-trace--session-id)
(declare-function yggdrasil-define-local-keys "yggdrasil-core")
(declare-function aob-trace--pending "aob-trace")
(defvar aob--views)
(defvar ygg--normal-p)

(defconst ygg-aob--kind-keys
  '((?y . "allow_once") (?Y . "allow_always") (?n . "reject_once") (?N . "reject_always"))
  "The letter that answers with the option of each ACP permission kind.")

(defun ygg-aob--offered (d)
  (let ((i 0))
    (mapconcat (lambda (o) (format "%d %s" (cl-incf i) (plist-get o :name)))
               (plist-get d :options) " · ")))

(defun ygg-aob--option-for (d spec)
  "The option of permission D that SPEC, a position or an ACP kind, names."
  (let ((options (plist-get d :options)))
    (or (if (integerp spec)
            (nth (1- spec) options)
          (seq-find (lambda (o) (equal (plist-get o :kind) spec)) options))
        (user-error "aob: no %s option; offered: %s"
                    (if (integerp spec) (format "number %d" spec) spec)
                    (ygg-aob--offered d)))))

(defun ygg-aob--pick (s d spec)
  "Answer S's permission D with the option SPEC names."
  (when (or (eq (plist-get d :kind) 'elicitation) (null (plist-get d :options)))
    (user-error "aob: this is a question, not a permission; answer it with RET"))
  (let* ((o (ygg-aob--option-for d spec))
         (id (plist-get o :optionId))
         (prompt (pcase (plist-get o :kind)
                   ("allow_always" "Allow always: %s? ")
                   ("reject_always" "Reject always: %s? "))))
    (when (and prompt (not (y-or-n-p (format prompt (plist-get o :name)))))
      (user-error "aob: not answered"))
    (aob--call s :resolve d id)
    (when (aob--rejects-p d id)
      (aob-ask-reject-reason s))))

(defun ygg-aob--key-spec ()
  (let ((c last-command-event))
    (if (<= ?1 c ?9) (- c ?0) (cdr (assq c ygg-aob--kind-keys)))))

(defun ygg-aob-decision-key ()
  "Answer the permission on this quickfix row with the option its key names.
1-9 pick by position, y/Y/n/N by kind: allow once, allow always, reject once,
reject always.  An always option asks first."
  (interactive)
  (let ((at (ygg-qf-kind-at-point)))
    (unless (eq (car at) 'decisions) (user-error "aob: no decision here"))
    (pcase-let ((`(,s . ,d) (ygg-aob--decision-of (cdr at))))
      (ygg-aob--pick s d (ygg-aob--key-spec)))))

(defun ygg-aob--decision-key-filter (command)
  "COMMAND on a decision row of the quickfix, else nil to fall through."
  (and (eq (car (ygg-qf-kind-at-point)) 'decisions) command))

(defun ygg-aob--trace-permission ()
  "The session and permission the block under point in a trace is, or nil."
  (when-let* ((s (and (boundp 'aob-trace--session-id) (aob-session-get aob-trace--session-id)))
              (seq (or (get-text-property (point) 'aob-item)
                       (get-text-property (point) 'aob-event)))
              (d (aob-trace--pending s seq))
              ((plist-get d :options))
              ((not (eq (plist-get d :kind) 'elicitation))))
    (cons s d)))

(defun ygg-aob-trace-key ()
  "Answer the permission under point with the option its digit names.
An always option asks first."
  (interactive)
  (let ((sd (or (ygg-aob--trace-permission) (user-error "aob: no permission here"))))
    (ygg-aob--pick (car sd) (cdr sd) (ygg-aob--key-spec))))

(defun ygg-aob--trace-key-filter (command)
  "COMMAND in normal state on a permission block, else nil to fall through."
  (and (bound-and-true-p ygg--normal-p)
       (not (use-region-p))
       (ygg-aob--trace-permission)
       command))

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
                           (format "%s · %s%s%s"
                                   (aob-session-name s)
                                   (or (plist-get d :title) "decision")
                                   (if-let* ((det (plist-get d :detail)))
                                       (format "  [%s]" det)
                                     "")
                                   (if (plist-get d :options)
                                       (format "  ‹%s›" (ygg-aob--offered d))
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
    (with-current-buffer buf
      (apply #'yggdrasil-define-local-keys 'normal
             (cl-loop for k in '("1" "2" "3" "4" "5" "6" "7" "8" "9" "y" "Y" "n" "N")
                      append (list k '(menu-item "" ygg-aob-decision-key
                                                 :filter ygg-aob--decision-key-filter)))))
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
