;;; ygg-git-compare-space-tests.el --- a compare stays in its space -*- lexical-binding: t; -*-

;;; Code:

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(let ((builds (expand-file-name "../elpaca/builds/"
                                (file-name-directory
                                 (or load-file-name buffer-file-name)))))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds)))
  (add-to-list 'load-path (expand-file-name "../../lisp/agent-objects" builds)))
(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'layer-sessions)
(require 'ygg-git-compare)
(require 'ygg-git-compare-space)

(defmacro ygg-git-compare-space-tests--with-spaces (&rest body)
  (declare (indent 0))
  `(let ((ygg--space-buffers (make-hash-table :test 'eql))
         (tab-bar-mode tab-bar-mode)
         (list (generate-new-buffer "*ygg-compare-space-test*"))
         (file (generate-new-buffer "magit-diff: compare file of space-test")))
     (unwind-protect
         (cl-letf (((symbol-function 'ygg-space--pin) #'ignore)
                   ((symbol-function 'ygg-space--dir-of) (lambda (_) "/tmp/")))
           (tab-bar-mode 1)
           (ygg-space--adopt-current nil)
           (let ((a (ygg-space--current-id))
                 (b (progn (ygg-space--spawn (ygg-space--current-id))
                           (ygg-space--current-id))))
             (ygg-space--goto-id a)
             ,@body))
       (mapc #'kill-buffer (list list file))
       (while (cdr (funcall tab-bar-tabs-function)) (tab-bar-close-tab 1)))))

(defun ygg-git-compare-space-tests--open (list file)
  (delete-other-windows)
  (with-current-buffer list
    (setq ygg-git-compare--a-spec '(rev . "a"))
    (ygg-git-compare-mode 1))
  (switch-to-buffer list)
  (let ((right (split-window nil nil 'right)))
    (set-window-buffer right file)
    (with-current-buffer file
      (setq ygg-git-compare--list-buffer list)
      (ygg-git-compare-mode 1))
    (with-current-buffer list (setq ygg-git-compare--file-window right))
    right))

(ert-deftest ygg-git-compare-space-stays-in-its-space ()
  (ygg-git-compare-space-tests--with-spaces
    (let ((right (ygg-git-compare-space-tests--open list file)))
      (should (eql (ygg--space-buffer-owner list) a))
      (should (eql (ygg--space-buffer-owner file) a))
      (ygg-space--goto-id b)
      (should-not (get-buffer-window list t))
      (should-not (get-buffer-window file t))
      (should-not (window-live-p right))
      (ygg--space-track-buffer)
      (should-not (memq list (gethash b ygg--space-buffers)))
      (should-not (memq file (gethash b ygg--space-buffers)))
      (ygg-space--goto-id a)
      (should (eq (window-buffer (get-buffer-window list)) list))
      (should (eq (window-buffer right) file)))))

(ert-deftest ygg-git-compare-space-leaves-buffer-cycling ()
  (ygg-git-compare-space-tests--with-spaces
    (ygg-git-compare-space-tests--open list file)
    (ygg-space--goto-id b)
    (switch-to-buffer "*scratch*")
    (dotimes (_ 6)
      (previous-buffer)
      (should-not (memq (current-buffer) (list list file))))
    (dotimes (_ 6)
      (next-buffer)
      (should-not (memq (current-buffer) (list list file))))
    (ygg-space--goto-id a)
    (switch-to-buffer "*scratch*")
    (dotimes (_ 6)
      (previous-buffer)
      (should-not (memq (current-buffer) (list list file))))))

(ert-deftest ygg-git-compare-space-idle-follow-does-nothing-elsewhere ()
  (ygg-git-compare-space-tests--with-spaces
    (ygg-git-compare-space-tests--open list file)
    (let (shown)
      (cl-letf (((symbol-function 'ygg-git-compare--show-file)
                 (lambda (&rest _) (setq shown t)))
                ((symbol-function 'magit-file-at-point) (lambda (&rest _) "x.el")))
        (ygg-space--goto-id b)
        (with-current-buffer list
          (setq ygg-git-compare--file-window (selected-window)))
        (let ((windows (window-list)))
          (ygg-git-compare--follow list)
          (should-not shown)
          (should (equal windows (window-list))))
        (ygg-space--goto-id a)
        (with-current-buffer list
          (setq ygg-git-compare--file-window (selected-window)))
        (ygg-git-compare--follow list)
        (should shown)))))

(ert-deftest ygg-git-compare-space-jump-back-does-not-reopen-elsewhere ()
  (ygg-git-compare-space-tests--with-spaces
    (ygg-git-compare-space-tests--open list file)
    (ygg-space--goto-id b)
    (let ((visited (find-file-noselect (make-temp-file "ygg-compare-space"))))
      (unwind-protect
          (progn
            (switch-to-buffer visited)
            (set-window-prev-buffers nil (list (list list (point-min-marker) (point-min-marker))))
            (cl-letf (((symbol-function 'ygg-git-compare--jumper-stays-in-buffer-p) #'ignore))
              (should-not (ygg-git-compare--jump-back)))
            (should-not (get-buffer-window list t))
            (should (eq (window-buffer) visited)))
        (kill-buffer visited)))))

(provide 'ygg-git-compare-space-tests)
;;; ygg-git-compare-space-tests.el ends here
