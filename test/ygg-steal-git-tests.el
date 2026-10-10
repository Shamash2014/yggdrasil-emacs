;;; ygg-steal-git-tests.el --- Q quits a repository's magit buffers -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-git)
(require 'magit)
(require 'git-commit)

(defmacro ygg-steal-git-tests--with-repos (vars &rest body)
  "Run BODY with each of VARS bound to the root of its own fresh repository."
  (declare (indent 1))
  `(let* ((process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment))
          ,@(mapcar (lambda (v)
                      `(,v (file-name-as-directory
                            (file-truename (make-temp-file "ygg-steal" t)))))
                    vars))
     (unwind-protect
         (progn
           (dolist (dir (list ,@vars))
             (let ((default-directory dir))
               (call-process "git" nil nil nil "init" "-q" "-b" "trunk")
               (call-process "git" nil nil nil "-c" "user.email=t@t" "-c" "user.name=t"
                             "commit" "-q" "--allow-empty" "-m" "0")))
           ,@body)
       (dolist (dir (list ,@vars))
         (dolist (buffer (buffer-list))
           (when (equal (buffer-local-value 'default-directory buffer) dir)
             (kill-buffer buffer)))
         (delete-directory dir t)))))

(defun ygg-steal-git-tests--status (dir)
  (let ((default-directory dir)
        (magit-display-buffer-function
         (lambda (buffer) (set-window-buffer (selected-window) buffer) (selected-window))))
    (magit-status-setup-buffer dir)
    (magit-get-mode-buffer 'magit-status-mode)))

(ert-deftest ygg-steal-git-q-in-magit-mode-map ()
  (should (eq (lookup-key magit-mode-map (kbd "Q")) #'ygg-magit-quit-all)))

(ert-deftest ygg-steal-git-q-kills-this-repository-only ()
  (ygg-steal-git-tests--with-repos (a b)
    (let* ((status-a (ygg-steal-git-tests--status a))
           (status-b (ygg-steal-git-tests--status b))
           (plain (with-current-buffer (generate-new-buffer "ygg-steal-plain")
                    (setq default-directory a)
                    (current-buffer))))
      (unwind-protect
          (progn
            (with-current-buffer status-a (ygg-magit-quit-all))
            (should-not (buffer-live-p status-a))
            (should (buffer-live-p status-b))
            (should (buffer-live-p plain)))
        (kill-buffer plain)))))

(ert-deftest ygg-steal-git-q-keeps-an-open-compare ()
  (ygg-steal-git-tests--with-repos (a)
    (let* ((status (ygg-steal-git-tests--status a))
           (compare (with-current-buffer (generate-new-buffer "ygg-steal-compare")
                      (magit-diff-mode)
                      (setq default-directory a
                            magit--default-directory a)
                      (setq-local ygg-git-compare-mode t)
                      (current-buffer))))
      (unwind-protect
          (progn
            (with-current-buffer status (ygg-magit-quit-all))
            (should-not (buffer-live-p status))
            (should (buffer-live-p compare)))
        (kill-buffer compare)))))

(ert-deftest ygg-steal-git-q-works-before-compare-is-loaded ()
  (ygg-steal-git-tests--with-repos (a)
    (let ((status (ygg-steal-git-tests--status a)))
      (with-current-buffer status (kill-local-variable 'ygg-git-compare-mode))
      (setq features (delq 'ygg-git-compare features))
      (makunbound 'ygg-git-compare-mode)
      (should-not (buffer-local-boundp 'ygg-git-compare-mode status))
      (with-current-buffer status (ygg-magit-quit-all))
      (should-not (buffer-live-p status)))))

(ert-deftest ygg-steal-git-q-refuses-while-a-process-runs ()
  (ygg-steal-git-tests--with-repos (a)
    (let* ((status (ygg-steal-git-tests--status a))
           (default-directory a)
           (pbuf (magit-process-buffer t))
           (process (make-process :name "ygg-steal-sleep" :buffer pbuf
                                  :command '("sleep" "30"))))
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag process nil)
            (let ((err (should-error (with-current-buffer status (ygg-magit-quit-all))
                                     :type 'user-error)))
              (should (string-match-p "sleep 30 still running" (cadr err))))
            (should (process-live-p process))
            (should (buffer-live-p status))
            (should (buffer-live-p pbuf)))
        (delete-process process)))))

(ert-deftest ygg-steal-git-q-kills-the-idle-process-buffer ()
  (ygg-steal-git-tests--with-repos (a)
    (let* ((status (ygg-steal-git-tests--status a))
           (default-directory a)
           (pbuf (magit-process-buffer t)))
      (with-current-buffer status (ygg-magit-quit-all))
      (should-not (buffer-live-p status))
      (should-not (buffer-live-p pbuf)))))

(ert-deftest ygg-steal-git-commit-style ()
  (should (= git-commit-summary-max-length 50))
  (should (memq 'overlong-summary-line git-commit-style-convention-checks))
  (with-temp-buffer
    (run-hooks 'git-commit-setup-hook)
    (should (= fill-column 72))))

;;; ygg-steal-git-tests.el ends here
