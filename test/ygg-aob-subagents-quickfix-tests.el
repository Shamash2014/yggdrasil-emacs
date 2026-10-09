;;; ygg-aob-subagents-quickfix-tests.el --- \ t lists subagents in the quickfix -*- lexical-binding: t; -*-

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
  (setq aob-acp-persist-file (make-temp-file "aob-qfsubs-sessions-" nil ".eld")))
(require 'aob)
(require 'aob-acp)
(require 'aob-trace)
(require 'layer-quickfix)
(require 'layer-aob)
(require 'ygg-embark)

(defmacro ygg-aob-subs-tests--with (svar &rest body)
  (declare (indent 1))
  `(let* ((aob-trace-icons nil)
          (,svar (aob-create-session :id "acp:qfsubs:1" :backend 'acp
                                     :name "qfsubs" :project "/tmp/proj/"
                                     :dir "/tmp/proj/" :state 'working)))
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))
       (when (aob-session-get (aob-session-id ,svar))
         (aob-remove-session ,svar)))))

(defun ygg-aob-subs-tests--task (s id title status)
  (aob-event s 'tool :tool-id id :kind "think" :subagent t
             :title title :status status :ts (- (float-time) 100)))

(defun ygg-aob-subs-tests--two (s)
  (list (ygg-aob-subs-tests--task s "T1" "Count files" "in_progress")
        (ygg-aob-subs-tests--task s "T2" "Read schema" "completed")))

(defun ygg-aob-subs-tests--text ()
  (with-current-buffer (ygg-qf-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ygg-aob-subs-tests--goto-row (title)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward title)
    (beginning-of-line)))

(ert-deftest ygg-aob-subagents-fills-quickfix-one-row-each ()
  (ygg-aob-subs-tests--with s
    (ygg-aob-subs-tests--two s)
    (ygg-aob-subagents s)
    (with-current-buffer (ygg-qf-buffer)
      (should (= 2 (ygg-qf--count-rows))))
    (should (string-match-p "^:| running .*Count files" (ygg-aob-subs-tests--text)))
    (should (string-match-p "^:| done .*Read schema" (ygg-aob-subs-tests--text)))))

(ert-deftest ygg-aob-subagents-without-any-is-a-user-error ()
  (ygg-aob-subs-tests--with s
    (should-error (ygg-aob-subagents s) :type 'user-error)))

(ert-deftest ygg-aob-subagents-row-opens-trace-not-a-file ()
  (ygg-aob-subs-tests--with s
    (let ((evs (ygg-aob-subs-tests--two s)) opened)
      (ygg-aob-subagents s)
      (ygg-aob-subs-tests--goto-row "Read schema")
      (cl-letf (((symbol-function 'aob-subagents--open)
                 (lambda (sess seq) (push (list sess seq) opened)))
                ((symbol-function 'compile-goto-error)
                 (lambda (&rest _) (ert-fail "visited a location"))))
        (with-current-buffer (ygg-qf-buffer) (ygg-qf-open)))
      (should (equal opened (list (list s (plist-get (cadr evs) :seq))))))))

(ert-deftest ygg-aob-subagents-click-runs-the-action ()
  (ygg-aob-subs-tests--with s
    (let (opened)
      (ygg-aob-subs-tests--two s)
      (ygg-aob-subagents s)
      (ygg-aob-subs-tests--goto-row "Count files")
      (cl-letf (((symbol-function 'aob-subagents--open)
                 (lambda (&rest args) (push args opened))))
        (with-current-buffer (ygg-qf-buffer) (compile-goto-error)))
      (should (= 1 (length opened))))))

(ert-deftest ygg-aob-subagents-action-survives-a-filter ()
  (ygg-aob-subs-tests--with s
    (let ((evs (ygg-aob-subs-tests--two s)) opened)
      (ygg-aob-subagents s)
      (with-current-buffer (ygg-qf-buffer)
        (ygg-qf--filter-apply "Count")
        (should (= 1 (ygg-qf--count-rows)))
        (ygg-aob-subs-tests--goto-row "Count files")
        (cl-letf (((symbol-function 'aob-subagents--open)
                   (lambda (sess seq) (push (list sess seq) opened))))
          (ygg-qf-open))
        (should (equal opened (list (list s (plist-get (car evs) :seq)))))
        (should-not (ygg-qf--row-place))))))

(ert-deftest ygg-aob-subagents-grep-row-still-visits ()
  (let (visited)
    (unwind-protect
        (progn
          (ygg-qf--collect (list "/tmp/foo.el:3: text") t)
          (with-current-buffer (ygg-qf-buffer)
            (goto-char (point-min))
            (search-forward "text")
            (beginning-of-line)
            (cl-letf (((symbol-function 'compile-goto-error)
                       (lambda (&rest _) (setq visited t)))
                      ((symbol-function 'recenter) #'ignore))
              (ygg-qf-open)))
          (should visited))
      (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b)))))

(ert-deftest ygg-aob-subagents-rows-follow-the-session ()
  (ygg-aob-subs-tests--with s
    (let ((evs (ygg-aob-subs-tests--two s)))
      (ygg-aob-subagents s)
      (ygg-aob-subs-tests--goto-row "Read schema")
      (plist-put (car evs) :status "completed")
      (aob--dirty s)
      (let ((buf (ygg-qf-buffer)))
        (aob--render-view (assq buf aob--views))
        (should-not (string-match-p "running" (ygg-aob-subs-tests--text)))
        (should (= 2 (with-current-buffer buf (ygg-qf--count-rows))))
        (with-current-buffer buf
          (should (looking-at-p (regexp-quote
                                 (format "%s"
                                         (buffer-substring-no-properties
                                          (point) (line-end-position))))))
          (should (string-match-p "Read schema"
                                  (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position)))))))))

(ert-deftest ygg-aob-subagents-refresh-stops-when-list-is-replaced ()
  (ygg-aob-subs-tests--with s
    (ygg-aob-subs-tests--two s)
    (ygg-aob-subagents s)
    (let ((buf (ygg-qf-buffer)))
      (ygg-qf--collect (list "/tmp/foo.el:3: text") t)
      (should-not (assq buf aob--views))
      (aob--dirty s)
      (should-not (string-match-p "Count files" (ygg-aob-subs-tests--text))))))

(ert-deftest ygg-qf-kind-registry-round-trip ()
  (let ((ygg-qf-kinds nil) opened (map (make-sparse-keymap)))
    (defvar ygg-qf-tests--map)
    (setq ygg-qf-tests--map map)
    (ygg-qf-define-kind 'things
                        :collect (lambda (&rest a) (mapcar (lambda (n) (list n (format "thing %s" n) "note")) a))
                        :action (lambda (id) (push id opened))
                        :map 'ygg-qf-tests--map)
    (unwind-protect
        (let ((default-directory "/tmp/"))
          (ygg-qf-show-kind 'things 7 8)
          (with-current-buffer (ygg-qf-buffer)
            (should (equal '(things . 7) (progn (goto-char (point-min))
                                                (search-forward "thing 7")
                                                (ygg-qf-kind-at-point))))
            (ygg-qf-open)
            (should (equal opened '(7)))
            (should (eq 'ygg-qf-things (car (ygg-embark-target-qf-kind))))
            (should (equal "7" (cadr (ygg-embark-target-qf-kind))))
            (should (assq 'things ygg-qf-kinds))))
      (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b)))))

(ert-deftest ygg-qf-kind-embark-finder-skips-grep-rows ()
  (unwind-protect
      (progn
        (ygg-qf--collect (list "/tmp/foo.el:3: text") t)
        (with-current-buffer (ygg-qf-buffer)
          (goto-char (point-min))
          (search-forward "text")
          (should-not (ygg-qf-kind-at-point))
          (should-not (ygg-embark-target-qf-kind))))
    (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))))

(ert-deftest ygg-aob-subagents-kind-targets-and-keymap ()
  (ygg-aob-subs-tests--with s
    (let ((evs (ygg-aob-subs-tests--two s)))
      (ygg-aob-subagents s)
      (ygg-aob-subs-tests--goto-row "Read schema")
      (with-current-buffer (ygg-qf-buffer)
        (should (equal (ygg-qf-kind-at-point)
                       (cons 'subagents (cons (aob-session-id s)
                                              (plist-get (cadr evs) :seq)))))
        (should (eq 'ygg-qf-subagents (car (ygg-embark-target-qf-kind)))))
      (should (eq (lookup-key ygg-aob-subagent-map "o") #'ygg-aob-subagent-open-trace)))))

(defmacro ygg-qf-kind-tests--with (&rest body)
  (declare (indent 0))
  `(let ((ygg-qf-kinds nil)
         (default-directory "/tmp/"))
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))
       (set-window-buffer (selected-window) (get-buffer-create "*scratch*")))))

(defun ygg-qf-kind-tests--define (kind items &rest spec)
  "Register KIND whose rows are the cells of ITEMS, a variable holding ids."
  (apply #'ygg-qf-define-kind kind
         :collect (lambda () (mapcar (lambda (id) (list id (format "%s %s" kind id) "note"))
                                     (symbol-value items)))
         spec))

(defvar ygg-qf-kind-tests--ids nil)
(defvar ygg-qf-kind-tests--log nil)

(defun ygg-qf-kind-tests--token ()
  (with-current-buffer (ygg-qf-buffer) (plist-get ygg-qf--kind :token)))

(defun ygg-qf-kind-tests--goto (text)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward text)
    (beginning-of-line)))

(ert-deftest ygg-qf-kind-rows-are-not-locations ()
  (ygg-qf-kind-tests--with
    (ygg-qf-define-kind 'odd
                        :collect (lambda () (list (list 1 "a.el:12: looks like a hit" "n")
                                                  (list 2 "b|3| and a pipe" nil)
                                                  (list 3 "c.el-4-context" nil)
                                                  (list 4 "|5| x" nil))))
    (ygg-qf-show-kind 'odd)
    (with-current-buffer (ygg-qf-buffer)
      (font-lock-mode 1)
      (font-lock-ensure)
      (compilation--ensure-parse (point-max))
      (should (= 4 (ygg-qf--count-rows)))
      (should-not (text-property-not-all (point-min) (point-max)
                                         'compilation-message nil))
      (goto-char (point-min))
      (while (not (eobp))
        (should-not (ygg-qf--row-file))
        (should-not (ygg-qf--row-place))
        (forward-line 1))
      (should-not (ygg-qf-locations))
      (ygg-qf-kind-tests--goto "a.el")
      (should (equal "▪ " (substring-no-properties
                           (get-text-property (point) 'display))))
      (should (seq-some (lambda (o) (overlay-get o 'ygg-qf)) (overlays-in (point-min) (point-max))))
      (should-error (ygg-qf-next-file) :type 'user-error))))

(ert-deftest ygg-qf-kind-next-error-runs-the-action-never-a-directory ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids '(1 2 3)) opened)
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids
                                 :action (lambda (id) (push id opened)))
      (ygg-qf-show-kind 'things)
      (with-current-buffer (ygg-qf-buffer)
        (should (eq next-error-function #'ygg-qf--kind-next-error)))
      (with-temp-buffer
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _) (ert-fail "visited a file")))
                  ((symbol-function 'compilation-goto-locus)
                   (lambda (&rest _) (ert-fail "went to a locus")))
                  ((symbol-function 'dired) (lambda (&rest _) (ert-fail "dired")))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "asked"))))
          (setq next-error-last-buffer (ygg-qf-buffer))
          (next-error)
          (next-error)
          (previous-error)
          (ygg-next-error-any)
          (should (equal (reverse opened) '(2 3 2 3)))
          (should-error (next-error) :type 'user-error)))
      (with-current-buffer (ygg-qf-buffer)
        (goto-char (point-min))
        (next-error 1 t)
        (should (equal (car opened) 1))))))

(ert-deftest ygg-qf-kind-clear-stops-refresh ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids '(1 2)) (ygg-qf-kind-tests--log nil))
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids
                                 :arm (lambda (_b _tok) (lambda () (push 'disarm ygg-qf-kind-tests--log))))
      (ygg-qf-show-kind 'things)
      (let ((buf (ygg-qf-buffer)) (token (ygg-qf-kind-tests--token)))
        (ygg-qf-clear)
        (should (equal ygg-qf-kind-tests--log '(disarm)))
        (should-not (ygg-qf-kind-refresh buf token))
        (should (= 0 (with-current-buffer buf (ygg-qf--count-rows))))
        (with-current-buffer buf
          (should-not (eq next-error-function #'ygg-qf--kind-next-error)))))))

(ert-deftest ygg-qf-kind-diagnostics-writer-stops-refresh ()
  (require 'flymake)
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids '(1 2)))
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids)
      (ygg-qf-show-kind 'things)
      (let ((buf (ygg-qf-buffer)) (token (ygg-qf-kind-tests--token)))
        (cl-letf (((symbol-function 'flymake--project-diagnostics) (lambda () '(d)))
                  ((symbol-function 'ygg--diag-qf-line)
                   (lambda (_) "/tmp/x.el:1:1: boom"))
                  ((symbol-function 'display-buffer) (lambda (b &rest _) (get-buffer-window b t)))
                  ((symbol-function 'select-window) #'ignore))
          (ygg-qf-diagnostics))
        (should-not (ygg-qf-kind-refresh buf token))
        (with-current-buffer buf
          (should (string-match-p "boom" (buffer-string)))
          (should-not (string-match-p "things" (buffer-string)))
          (should-not (ygg-qf--kind-list-p)))))))

(ert-deftest ygg-qf-kind-appended-rows-end-the-claim ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids '(1)))
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids)
      (ygg-qf-show-kind 'things)
      (let ((buf (ygg-qf-buffer)) (token (ygg-qf-kind-tests--token)))
        (ygg-qf--collect (list "/tmp/foo.el:3: text"))
        (should-not (ygg-qf-kind-refresh buf token))
        (should (string-match-p "text" (with-current-buffer buf (buffer-string))))))))

(ert-deftest ygg-qf-kind-superseded-kind-stops ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids '(1 2)) (ygg-qf-kind-tests--log nil) tokens)
      (dolist (k '(alpha beta))
        (let ((k k))
          (ygg-qf-kind-tests--define
           k 'ygg-qf-kind-tests--ids
           :arm (lambda (_b tok) (push (cons k tok) tokens)
                  (lambda () (push (list 'disarm k) ygg-qf-kind-tests--log))))))
      (ygg-qf-show-kind 'alpha)
      (ygg-qf-show-kind 'beta)
      (let ((buf (ygg-qf-buffer)))
        (should (equal ygg-qf-kind-tests--log '((disarm alpha))))
        (should-not (ygg-qf-kind-refresh buf (cdr (assq 'alpha tokens))))
        (should (string-match-p "beta 1" (with-current-buffer buf (buffer-string))))
        (should (ygg-qf-kind-refresh buf (cdr (assq 'beta tokens))))
        (should (equal ygg-qf-kind-tests--log '((disarm alpha))))))))

(ert-deftest ygg-aob-subagents-session-removal-keeps-the-buffer ()
  (ygg-aob-subs-tests--with s
    (ygg-aob-subs-tests--two s)
    (ygg-aob-subagents s)
    (let ((buf (ygg-qf-buffer)))
      (with-current-buffer buf
        (should-not aob-buffer-session-id))
      (aob-remove-session s)
      (should (buffer-live-p buf))
      (aob--render-view (assq buf aob--views))
      (should-not (assq buf aob--views))
      (should (= 2 (with-current-buffer buf (ygg-qf--count-rows))))
      (should-error (ygg-aob--subagent-open (cons (aob-session-id s) 1))
                    :type 'user-error))))

(ert-deftest ygg-qf-kind-embark-target-round-trips-the-id ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids (list (cons "acp:x:1" 1) (cons "acp:x:1" 2))) got)
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids
                                 :action #'ignore)
      (ygg-qf-show-kind 'things)
      (ygg-qf-kind-tests--goto "things (acp:x:1 . 2)")
      (with-current-buffer (ygg-qf-buffer)
        (let* ((target (cadr (ygg-embark-target-qf-kind)))
               (bare (substring-no-properties target)))
          (should (equal (ygg-qf-kind-target-id target) '("acp:x:1" . 2)))
          (should (equal (ygg-qf-kind-target-id bare) '("acp:x:1" . 2)))
          (goto-char (point-min))
          (ygg-qf--kind-embark-around :orig-target target
                                      :run (lambda (&rest _)
                                             (setq got (ygg-qf-kind-target-id))))
          (should (equal got '("acp:x:1" . 2))))))))

(ert-deftest ygg-qf-kind-verbs-are-registered-with-embark ()
  (require 'embark)
  (ygg-qf-kind-tests--with
    (let ((embark-keymap-alist nil) (embark-target-injection-hooks nil)
          (embark-around-action-hooks nil)
          (map (make-sparse-keymap)))
      (defvar ygg-qf-kind-tests--map)
      (setq ygg-qf-kind-tests--map map)
      (define-key map "z" #'ygg-qf-kind-tests--verb)
      (ygg-qf-define-kind 'zing :collect #'ignore :map 'ygg-qf-kind-tests--map)
      (should (eq 'ygg-qf-kind-tests--map (alist-get 'ygg-qf-zing embark-keymap-alist)))
      (should (assq 'ygg-qf-kind-tests--verb embark-target-injection-hooks))
      (should (assq 'ygg-qf-kind-tests--verb embark-around-action-hooks))
      (should (memq 'ygg-qf-kind-tests--verb (ygg-embark-verbs))))))

(defun ygg-qf-kind-tests--verb ()
  (interactive))

(ert-deftest ygg-qf-kind-timer-arm-does-not-stack-and-is-cancelled ()
  (ygg-aob-subs-tests--with s
    (let ((count (lambda () (seq-count (lambda (tm) (eq (timer--function tm)
                                                        #'ygg-aob--subagents-tick))
                                       timer-list))))
      (ygg-aob-subs-tests--two s)
      (ygg-aob-subagents s)
      (ygg-aob-subagents s)
      (should (= 1 (funcall count)))
      (let ((buf (ygg-qf-buffer)))
        (aob--render-view (assq buf aob--views))
        (should (= 1 (funcall count))))
      (ygg-qf-clear)
      (should (= 0 (funcall count)))
      (should-not (assq (ygg-qf-buffer) aob--views))
      (ygg-aob-subagents s)
      (should (= 1 (funcall count)))
      (ygg-qf--collect (list "/tmp/foo.el:3: text") t)
      (should (= 0 (funcall count))))))

(ert-deftest ygg-qf-kind-timer-stops-when-nothing-runs ()
  (ygg-aob-subs-tests--with s
    (let ((evs (ygg-aob-subs-tests--two s)))
      (ygg-aob-subagents s)
      (plist-put (car evs) :status "completed")
      (ygg-aob--subagents-tick (ygg-qf-buffer) (ygg-qf-kind-tests--token) s)
      (should-not (alist-get (ygg-qf-buffer) ygg-aob--subagent-timers)))))

(ert-deftest ygg-qf-kind-running-seconds-tick-while-visible ()
  (ygg-aob-subs-tests--with s
    (ygg-aob-subs-tests--two s)
    (ygg-aob-subagents s)
    (let ((buf (ygg-qf-buffer)) (token (ygg-qf-kind-tests--token)) before)
      (set-window-buffer (selected-window) buf)
      (setq before (with-current-buffer buf (buffer-string)))
      (cl-letf (((symbol-function 'aob-subagents--secs)
                 (lambda (_) 999)))
        (ygg-aob--subagents-tick buf token s))
      (should-not (equal before (with-current-buffer buf (buffer-string)))))))

(ert-deftest ygg-qf-kind-click-runs-the-action-from-the-event ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids '(1 2)) opened)
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids
                                 :action (lambda (id) (push id opened)))
      (ygg-qf-show-kind 'things)
      (let ((buf (ygg-qf-buffer)) pos)
        (set-window-buffer (selected-window) buf)
        (ygg-qf-kind-tests--goto "things 2")
        (setq pos (with-current-buffer buf (point)))
        (with-current-buffer buf (goto-char (point-min)))
        (with-current-buffer buf
          (compile-goto-error
           (list 'mouse-1 (list (selected-window) pos '(0 . 0) 0))))
        (should (equal opened '(2)))))))

(ert-deftest ygg-qf-kind-unchanged-rows-are-not-rewritten ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids '(1 2)))
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids)
      (ygg-qf-show-kind 'things)
      (let* ((buf (ygg-qf-buffer)) (token (ygg-qf-kind-tests--token))
             (tick (buffer-chars-modified-tick buf)))
        (should (ygg-qf-kind-refresh buf token))
        (should (= tick (buffer-chars-modified-tick buf)))
        (setq ygg-qf-kind-tests--ids '(1 2 3))
        (should (ygg-qf-kind-refresh buf token))
        (should-not (= tick (buffer-chars-modified-tick buf)))
        (should (= 3 (with-current-buffer buf (ygg-qf--count-rows))))))))

(ert-deftest ygg-qf-kind-refresh-keeps-each-window-on-its-row ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids (number-sequence 1 80)))
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids)
      (ygg-qf-show-kind 'things)
      (let* ((buf (ygg-qf-buffer)) (token (ygg-qf-kind-tests--token))
             (win (selected-window)))
        (set-window-buffer win buf)
        (ygg-qf-kind-tests--goto "things 40")
        (set-window-point win (with-current-buffer buf (point)))
        (set-window-start win (with-current-buffer buf
                                (save-excursion (goto-char (point))
                                                (forward-line -3) (point))))
        (let ((start (with-current-buffer buf
                       (line-number-at-pos (window-start win)))))
          (setq ygg-qf-kind-tests--ids (cons 0 ygg-qf-kind-tests--ids))
          (should (ygg-qf-kind-refresh buf token))
          (with-current-buffer buf
            (should (equal 40 (cdr (ygg-qf-kind-at-point-pos (window-point win)))))
            (should (= start (line-number-at-pos (window-start win))))))))))

(defun ygg-qf-kind-at-point-pos (pos)
  (save-excursion (goto-char pos) (ygg-qf-kind-at-point)))

(ert-deftest ygg-qf-kind-drop-needs-a-drop-hook ()
  (ygg-qf-kind-tests--with
    (let ((ygg-qf-kind-tests--ids (list 1 2)) dropped)
      (ygg-qf-kind-tests--define 'things 'ygg-qf-kind-tests--ids)
      (ygg-qf-show-kind 'things)
      (ygg-qf-kind-tests--goto "things 1")
      (with-current-buffer (ygg-qf-buffer)
        (should-error (ygg-qf-drop) :type 'user-error)
        (should (= 2 (ygg-qf--count-rows))))
      (ygg-qf-kind-tests--define 'gone 'ygg-qf-kind-tests--ids
                                 :drop (lambda (id)
                                         (push id dropped)
                                         (setq ygg-qf-kind-tests--ids
                                               (remq id ygg-qf-kind-tests--ids))))
      (ygg-qf-show-kind 'gone)
      (ygg-qf-kind-tests--goto "gone 1")
      (with-current-buffer (ygg-qf-buffer) (ygg-qf-drop))
      (should (equal dropped '(1)))
      (should (= 1 (with-current-buffer (ygg-qf-buffer) (ygg-qf--count-rows)))))))

(ert-deftest ygg-aob-qf-decision-refresh-leaves-a-kind-list-alone ()
  (ygg-aob-subs-tests--with s
    (ygg-aob-subs-tests--two s)
    (ygg-aob-subagents s)
    (set-window-buffer (selected-window) (ygg-qf-buffer))
    (let ((before (ygg-aob-subs-tests--text)))
      (aob-session-put s :decisions nil)
      (ygg-aob--qf-refresh)
      (should (equal before (ygg-aob-subs-tests--text))))))

(provide 'ygg-aob-subagents-quickfix-tests)
;;; ygg-aob-subagents-quickfix-tests.el ends here
