;;; ygg-qf-shells-tests.el --- SPC a c p lists running commands in the quickfix -*- lexical-binding: t; -*-

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
  (setq aob-acp-persist-file (make-temp-file "aob-qfshells-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'aob-shells)
(require 'layer-quickfix)
(require 'layer-aob)
(require 'ygg-embark)

(defmacro ygg-qf-shells-tests--with (svar &rest body)
  (declare (indent 1))
  `(let* ((aob-trace-icons nil)
          (,svar (aob-create-session :id "acp:qfshells:1" :backend 'acp
                                     :name "qfshells" :project "/tmp/proj/"
                                     :dir "/tmp/proj/" :state 'working)))
     (cl-letf (((symbol-function 'aob-shells--settle) #'ignore))
       (unwind-protect (progn ,@body)
         (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))
         (when (aob-session-get (aob-session-id ,svar))
           (aob-remove-session ,svar))))))

(defun ygg-qf-shells-tests--cmd (s id command &optional status)
  (aob-event s 'tool :tool-id id :kind "execute" :title command
             :status (or status "in_progress") :ts (- (float-time) 100)
             :raw-input (list :command command)))

(defun ygg-qf-shells-tests--two (s)
  (list (ygg-qf-shells-tests--cmd s "C1" "make watch")
        (ygg-qf-shells-tests--cmd s "C2" "cargo test")))

(defun ygg-qf-shells-tests--text ()
  (with-current-buffer (ygg-qf-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ygg-qf-shells-tests--goto-row (text)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward text)
    (beginning-of-line)))

(defun ygg-qf-shells-tests--id (ev s)
  (cons (aob-session-id s) (plist-get ev :seq)))

(ert-deftest ygg-qf-shells-fills-quickfix-one-row-each ()
  (ygg-qf-shells-tests--with s
    (ygg-qf-shells-tests--two s)
    (ygg-qf-shells-tests--cmd s "C3" "ls" "completed")
    (aob-shells)
    (with-current-buffer (ygg-qf-buffer)
      (should (= 2 (ygg-qf--count-rows))))
    (should (string-match-p "make watch" (ygg-qf-shells-tests--text)))
    (should-not (string-match-p "^:| ls" (ygg-qf-shells-tests--text)))
    (let ((note (nth 2 (car (aob-shells--rows)))))
      (should (string-match-p "\\`qfshells · .* · running\\'" note)))))

(ert-deftest ygg-qf-shells-background-note ()
  (ygg-qf-shells-tests--with s
    (let ((ev (ygg-qf-shells-tests--cmd s "C1" "make watch")))
      (plist-put ev :background "bx1")
      (should (string-suffix-p "background" (nth 2 (car (aob-shells--rows))))))))

(ert-deftest ygg-qf-shells-empty-is-a-user-error ()
  (ygg-qf-shells-tests--with s
    (should-error (aob-shells) :type 'user-error)))

(ert-deftest ygg-qf-shells-row-visits-trace-not-a-file ()
  (ygg-qf-shells-tests--with s
    (let ((evs (ygg-qf-shells-tests--two s)) visited)
      (aob-shells)
      (ygg-qf-shells-tests--goto-row "cargo test")
      (cl-letf (((symbol-function 'aob-shells-visit)
                 (lambda (id) (push id visited)))
                ((symbol-function 'compile-goto-error)
                 (lambda (&rest _) (ert-fail "visited a location"))))
        (with-current-buffer (ygg-qf-buffer) (ygg-qf-open)))
      (should (equal visited (list (ygg-qf-shells-tests--id (cadr evs) s)))))))

(ert-deftest ygg-qf-shells-visit-goes-to-the-trace-row ()
  (ygg-qf-shells-tests--with s
    (let ((evs (ygg-qf-shells-tests--two s)) shown)
      (cl-letf (((symbol-function 'aob-trace-buffer) (lambda (x) (setq shown x) (current-buffer)))
                ((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'aob-trace--event-bounds) (lambda (_) nil)))
        (aob-shells-visit (ygg-qf-shells-tests--id (car evs) s)))
      (should (eq shown s)))))

(ert-deftest ygg-qf-shells-verbs-take-an-id-away-from-point ()
  (ygg-qf-shells-tests--with s
    (let ((evs (ygg-qf-shells-tests--two s)) stopped)
      (aob-shells)
      (ygg-qf-shells-tests--goto-row "make watch")
      (let ((id (ygg-qf-shells-tests--id (cadr evs) s)))
        (cl-letf (((symbol-function 'aob-shells-kill)
                   (lambda (_s ev) (push (plist-get ev :seq) stopped))))
          (with-current-buffer (ygg-qf-buffer)
            (aob-shells-stop id)))
        (should (equal stopped (list (plist-get (cadr evs) :seq))))
        (should (eq (cdr (aob-shells--pair id)) (cadr evs)))
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'aob-trace-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'aob-trace--event-bounds)
                   (lambda (seq) (push seq stopped) nil)))
          (aob-shells-visit id))
        (should (eql (car stopped) (plist-get (cadr evs) :seq)))
        (let ((buf (aob-shells-output id)))
          (should buf))))))

(ert-deftest ygg-qf-shells-embark-map-offers-the-verbs ()
  (should (eq (lookup-key aob-shells-map "x") #'aob-shells-stop))
  (should (eq (lookup-key aob-shells-map "o") #'aob-shells-output))
  (should (eq (lookup-key aob-shells-map "v") #'aob-shells-visit))
  (should (eq (plist-get (alist-get 'shells ygg-qf-kinds) :map) 'aob-shells-map)))

(ert-deftest ygg-qf-shells-gone-command-is-a-user-error ()
  (should-error (aob-shells-stop (cons "acp:nobody:1" 99)) :type 'user-error))

(ert-deftest ygg-qf-shells-refresh-keeps-point-by-id ()
  (ygg-qf-shells-tests--with s
    (let ((evs (ygg-qf-shells-tests--two s)))
      (aob-shells)
      (ygg-qf-shells-tests--goto-row "cargo test")
      (plist-put (car evs) :status "completed")
      (aob--dirty s)
      (let ((buf (ygg-qf-buffer)))
        (aob--render-view (assq buf aob--views))
        (should-not (string-match-p "make watch" (ygg-qf-shells-tests--text)))
        (with-current-buffer buf
          (should (= 1 (ygg-qf--count-rows)))
          (should (string-match-p "cargo test"
                                  (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position)))))))))

(ert-deftest ygg-qf-shells-stops-following-when-nothing-runs ()
  (ygg-qf-shells-tests--with s
    (let ((evs (ygg-qf-shells-tests--two s)) (buf nil))
      (aob-shells)
      (setq buf (ygg-qf-buffer))
      (dolist (ev evs) (plist-put ev :status "completed"))
      (aob--render-view (assq buf aob--views))
      (should-not (assq buf aob--views))
      (should-not (assq buf aob-shells--timers)))))

(ert-deftest ygg-qf-shells-last-command-ending-empties-the-rows ()
  (ygg-qf-shells-tests--with s
    (let ((evs (ygg-qf-shells-tests--two s)))
      (aob-shells)
      (dolist (ev evs) (plist-put ev :status "completed"))
      (aob--render-view (assq (ygg-qf-buffer) aob--views))
      (should-not (string-match-p "make watch\\|cargo test" (ygg-qf-shells-tests--text)))
      (with-current-buffer (ygg-qf-buffer) (should (= 0 (ygg-qf--count-rows)))))))

(ert-deftest ygg-qf-shells-streaming-render-skips-the-process-scan ()
  (ygg-qf-shells-tests--with s
    (ygg-qf-shells-tests--two s)
    (let ((scans 0))
      (cl-letf (((symbol-function 'aob-shells--process-table)
                 (lambda () (cl-incf scans) nil)))
        (aob-shells)
        (should (= 1 scans))
        (aob--render-view (assq (ygg-qf-buffer) aob--views))
        (should (= 1 scans))
        (let ((buf (ygg-qf-buffer)))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
            (funcall (timer--function (cdr (assq buf aob-shells--timers)))))
          (should (= 2 scans)))))))

(ert-deftest ygg-qf-shells-timer-tick-settles-vanished-commands-with-the-real-settle ()
  (let ((aob-trace-icons nil)
        (s (aob-create-session :id "acp:qfshells:2" :backend 'acp :name "qfshells"
                               :project "/tmp/proj/" :dir "/tmp/proj/" :state 'working)))
    (unwind-protect
        (progn
          (ygg-qf-shells-tests--two s)
          (cl-letf (((symbol-function 'aob-shells--settle) #'ignore))
            (aob-shells))
          (let ((buf (ygg-qf-buffer)))
            (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
              (funcall (timer--function (cdr (assq buf aob-shells--timers)))))
            (should-not (string-match-p "make watch\\|cargo test" (ygg-qf-shells-tests--text)))
            (should-not (assq buf aob-shells--timers))
            (should-not (assq buf aob--views))))
      (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))
      (when (aob-session-get (aob-session-id s)) (aob-remove-session s)))))

(ert-deftest ygg-qf-shells-landing-from-a-trace-selects-its-command ()
  (ygg-qf-shells-tests--with s
    (let* ((evs (ygg-qf-shells-tests--two s)) at)
      (with-temp-buffer
        (insert (propertize "row\n" 'aob-event (plist-get (cadr evs) :seq)))
        (goto-char (point-min))
        (let ((major-mode 'aob-trace-mode))
          (aob-shells))
        (setq at (with-current-buffer (ygg-qf-buffer)
                   (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))))
      (should (string-match-p "cargo test" at)))))

(ert-deftest ygg-qf-shells-timer-does-not-stack-and-disarm-cancels ()
  (ygg-qf-shells-tests--with s
    (ygg-qf-shells-tests--two s)
    (aob-shells)
    (aob-shells)
    (let ((buf (ygg-qf-buffer)))
      (should (= 1 (length (seq-filter (lambda (c) (eq (car c) buf)) aob-shells--timers))))
      (ygg-qf--collect (list "/tmp/foo.el:3: text") t)
      (should-not (assq buf aob-shells--timers))
      (should-not (assq buf aob--views)))))

(ert-deftest ygg-qf-shells-old-panel-is-gone ()
  (should-not (fboundp 'aob-shells-mode))
  (should-not (boundp 'aob-shells-mode-map))
  (should-not (fboundp 'aob-shells--entries))
  (should-not (fboundp 'aob-shells--redraw))
  (should-not (get-buffer "*aob shells*")))

(provide 'ygg-qf-shells-tests)
;;; ygg-qf-shells-tests.el ends here
