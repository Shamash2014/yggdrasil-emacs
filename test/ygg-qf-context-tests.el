;;; ygg-qf-context-tests.el --- SPC a c X lists the context in the quickfix -*- lexical-binding: t; -*-

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
  (defvar ygg-leader-open-map (make-sparse-keymap)))
(require 'aob)
(require 'aob-context)
(require 'layer-quickfix)
(require 'layer-aob)
(require 'ygg-embark)

(defmacro ygg-qf-context-tests--with (&rest body)
  (declare (indent 0))
  `(let ((aob-context--items
          (list (list :file "/tmp/ctx-c.el" :text "ccc" :beg 3 :end 9)
                (list :file "/tmp/ctx-b.el" :text "bb")
                (list :file "/tmp/ctx-a.el" :text "a"))))
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b)))))

(defun ygg-qf-context-tests--text ()
  (with-current-buffer (ygg-qf-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ygg-qf-context-tests--goto (s)
  (with-current-buffer (ygg-qf-buffer)
    (goto-char (point-min))
    (search-forward s)
    (beginning-of-line)))

(defun ygg-qf-context-tests--notes ()
  (with-current-buffer (ygg-qf-buffer)
    (let (notes)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let* ((n (get-text-property (line-beginning-position) 'ygg-qf-note)))
            (push n notes))
          (forward-line 1)))
      (nreverse notes))))

(ert-deftest ygg-qf-context-rows-are-entries-oldest-first-with-notes ()
  (ygg-qf-context-tests--with
    (aob-context-list)
    (let ((text (ygg-qf-context-tests--text)))
      (should (= 3 (with-current-buffer (ygg-qf-buffer) (ygg-qf--count-rows))))
      (should (< (string-match "ctx-a.el" text) (string-match "ctx-b.el" text)
                 (string-match "ctx-c.el:3-9" text)))
      (should (equal (ygg-qf-context-tests--notes)
                     '("file, 1 chars" "file, 2 chars" "region, 3 chars"))))))

(ert-deftest ygg-qf-context-default-action-visits-the-file-and-line ()
  (let ((file (make-temp-file "ctx-visit-")) (aob-context--items nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "1\n2\n3\n4\n"))
          (setq aob-context--items (list (list :file file :text "3\n" :beg 3 :end 4)))
          (aob-context-list)
          (ygg-qf-context-tests--goto ":3-4")
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (f) (set-buffer (find-file-noselect f))))
                    ((symbol-function 'compile-goto-error)
                     (lambda (&rest _) (ert-fail "visited a location"))))
            (with-current-buffer (ygg-qf-buffer) (ygg-qf-open)))
          (with-current-buffer (find-buffer-visiting file)
            (should (= 3 (line-number-at-pos)))))
      (when-let* ((b (find-buffer-visiting file))) (kill-buffer b))
      (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b))
      (delete-file file))))

(ert-deftest ygg-qf-context-missing-file-is-a-user-error ()
  (ygg-qf-context-tests--with
    (should-error (aob-context-visit (aob-context--id (nth 2 aob-context--items)))
                  :type 'user-error)
    (should-error (aob-context-visit 9999) :type 'user-error)))

(ert-deftest ygg-qf-context-verbs-act-on-the-id-away-from-point ()
  (ygg-qf-context-tests--with
    (let ((real (make-temp-file "ctx-real-")) visited)
      (setf (plist-get (nth 1 aob-context--items) :file) real)
      (aob-context-list)
      (ygg-qf-context-tests--goto "ctx-a.el")
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f) (setq visited f))))
        (aob-context-visit (aob-context--id (nth 1 aob-context--items))))
      (delete-file real)
      (should (equal visited real))
      (with-current-buffer (ygg-qf-buffer)
        (let ((target (cadr (save-excursion (ygg-qf-context-tests--goto "ctx-real")
                                            (ygg-embark-target-qf-kind)))))
          (ygg-qf-context-tests--goto "ctx-a.el")
          (ygg-qf--kind-embark-around
           :orig-target target
           :run (lambda (&rest _) (call-interactively #'aob-context-drop)))))
      (should (equal (mapcar (lambda (i) (plist-get i :file)) aob-context--items)
                     '("/tmp/ctx-c.el" "/tmp/ctx-a.el"))))))

(ert-deftest ygg-qf-context-verbs-are-in-the-embark-map ()
  (should (eq (lookup-key aob-context-row-map "o") #'aob-context-visit))
  (should (eq (lookup-key aob-context-row-map "d") #'aob-context-drop))
  (should (eq 'aob-context-row-map (plist-get (alist-get 'context ygg-qf-kinds) :map))))

(ert-deftest ygg-qf-context-refresh-keeps-point-by-id ()
  (ygg-qf-context-tests--with
    (aob-context-list)
    (ygg-qf-context-tests--goto "ctx-b.el")
    (push (list :file "/tmp/ctx-d.el" :text "d") aob-context--items)
    (accept-process-output nil 0.1)
    (should (string-match-p "ctx-d.el" (ygg-qf-context-tests--text)))
    (with-current-buffer (ygg-qf-buffer)
      (should (string-match-p "ctx-b.el" (buffer-substring-no-properties
                                          (line-beginning-position) (line-end-position)))))))

(ert-deftest ygg-qf-context-clear-empties-the-list-and-add-extends-it ()
  (ygg-qf-context-tests--with
    (aob-context-list)
    (aob-context-clear)
    (accept-process-output nil 0.1)
    (should (= 0 (with-current-buffer (ygg-qf-buffer) (ygg-qf--count-rows))))
    (push (list :file "/tmp/ctx-z.el" :text "z") aob-context--items)
    (accept-process-output nil 0.1)
    (should (string-match-p "ctx-z.el" (ygg-qf-context-tests--text)))))

(ert-deftest ygg-qf-context-drop-key-removes-the-row-at-its-source ()
  (ygg-qf-context-tests--with
    (aob-context-list)
    (ygg-qf-context-tests--goto "ctx-b.el")
    (with-current-buffer (ygg-qf-buffer) (ygg-qf-drop))
    (should (= 2 (length aob-context--items)))
    (should-not (string-match-p "ctx-b.el" (ygg-qf-context-tests--text)))
    (should (string-match-p "ctx-a.el" (ygg-qf-context-tests--text)))))

(ert-deftest ygg-qf-context-ids-survive-dropping-an-older-duplicate ()
  (let ((aob-context--items
         (list (list :file "/tmp/dup.el" :text "new")
               (list :file "/tmp/dup.el" :text "old"))))
    (unwind-protect
        (progn
          (aob-context-list)
          (let ((newer-id (aob-context--id (car aob-context--items)))
                (older-id (aob-context--id (cadr aob-context--items))))
            (should-not (equal newer-id older-id))
            (aob-context-drop older-id)
            (should (equal "new" (plist-get (aob-context--find newer-id) :text)))
            (should-error (aob-context--find older-id) :type 'user-error)
            (aob-context-drop newer-id)
            (should-not aob-context--items)))
      (when-let* ((b (get-buffer "*quickfix*"))) (kill-buffer b)))))

(ert-deftest ygg-qf-context-visit-without-a-file-is-a-user-error ()
  (let ((aob-context--items (list (list :text "x"))))
    (should-error (aob-context-visit (aob-context--id (car aob-context--items)))
                  :type 'user-error)))

(ert-deftest ygg-qf-context-empty-is-a-user-error ()
  (let ((aob-context--items nil))
    (should-error (aob-context-list) :type 'user-error)))

(ert-deftest ygg-qf-context-old-panel-is-gone ()
  (should-not (fboundp 'aob-context--render))
  (should-not (fboundp 'aob-context-mode))
  (should-not (boundp 'aob-context-mode-map))
  (ygg-qf-context-tests--with
    (aob-context-list)
    (should-not (get-buffer "*aob-context*"))))

(provide 'ygg-qf-context-tests)
;;; ygg-qf-context-tests.el ends here
