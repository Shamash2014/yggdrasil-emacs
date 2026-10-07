;;; ygg-term-project-tests.el --- terminals start in their project -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'project)
(require 'ygg-term-env-tests)
(require 'ygg-projects)
(require 'ygg-embark)

(defun ygg-term-ptests--proj (name)
  (let ((root (ygg-term-tests--dir name)))
    (make-directory (expand-file-name ".git" root) t)
    (make-directory (expand-file-name "src/a/b" root) t)
    root))

(defun ygg-term-ptests--file (root)
  (let ((f (expand-file-name "src/a/b/x.el" root)))
    (write-region "" nil f)
    f))

(defmacro ygg-term-ptests--with (&rest body)
  (declare (indent 0))
  `(ygg-term-tests--with
     (let ((project--list nil))
       (cl-letf (((symbol-function 'ghostel-semi-char-mode) #'ignore)
                 ((symbol-function 'ghostel)
                  (let ((orig (symbol-function 'ghostel)))
                    (lambda (&rest args)
                      (let ((buf (apply orig args)))
                        (with-current-buffer buf
                          (set-process-query-on-exit-flag
                           (make-pipe-process :name "ygg-term-test" :buffer buf)
                           nil))
                        buf))))
                 ((symbol-function 'ygg--term-buffers)
                  (lambda ()
                    (seq-filter (lambda (b) (buffer-local-value 'ygg-term--buffer-root b))
                                (buffer-list))))
                 ((symbol-function 'ygg--term-split-window)
                  (lambda () (delete-other-windows) (split-window (selected-window)))))
         (save-window-excursion ,@body)))))

(defun ygg-term-ptests--in (file fn &rest args)
  (with-current-buffer (find-file-noselect file)
    (set-window-buffer (selected-window) (current-buffer))
    (apply fn args)))

(defun ygg-term-ptests--cwd (name)
  (buffer-local-value 'default-directory (get-buffer name)))

(defun ygg-term-ptests--toggle-buffer ()
  (window-buffer (selected-window)))

(ert-deftest ygg-term-toggle-from-subdir-starts-at-root ()
  (ygg-term-ptests--with
    (let* ((root (ygg-term-ptests--proj "p"))
           (file (ygg-term-ptests--file root)))
      (ygg-term-ptests--in file #'ygg-terminal-toggle)
      (should (equal (buffer-local-value 'default-directory (ygg-term-ptests--toggle-buffer))
                     root)))))

(ert-deftest ygg-term-new-from-subdir-starts-at-root ()
  (ygg-term-ptests--with
    (let* ((root (ygg-term-ptests--proj "p"))
           (file (ygg-term-ptests--file root)))
      (ygg-term-ptests--in file #'ygg-term-new "sub-n")
      (should (equal (ygg-term-ptests--cwd "*ygg-term:sub-n*") root)))))

(ert-deftest ygg-term-toggle-is-per-project ()
  (ygg-term-ptests--with
    (let* ((p (ygg-term-ptests--proj "p"))
           (q (ygg-term-ptests--proj "q"))
           (fp (ygg-term-ptests--file p))
           (fq (ygg-term-ptests--file q)))
      (ygg-term-ptests--in fp #'ygg-terminal-toggle)
      (let ((bp (ygg-term-ptests--toggle-buffer)))
        (ygg-term-ptests--in fq #'ygg-terminal-toggle)
        (let ((bq (ygg-term-ptests--toggle-buffer)))
          (should-not (eq bp bq))
          (should (equal (buffer-local-value 'default-directory bq) q))
          (should (equal (buffer-local-value 'default-directory bp) p))
          (ygg-term-ptests--in fp #'ygg-terminal-toggle)
          (should (eq (ygg-term-ptests--toggle-buffer) bp)))))))

(ert-deftest ygg-term-pick-lists-only-this-projects-terminals ()
  (ygg-term-ptests--with
    (let* ((p (ygg-term-ptests--proj "p"))
           (q (ygg-term-ptests--proj "q"))
           (fp (ygg-term-ptests--file p))
           (fq (ygg-term-ptests--file q))
           offered)
      (ygg-term-ptests--in fp #'ygg-term-new "pp")
      (ygg-term-ptests--in fq #'ygg-term-new "qq")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_ cands &rest _) (setq offered cands) (car cands))))
        (ygg-term-ptests--in fp #'ygg-term-pick))
      (should (equal offered '("*ygg-term:pp*"))))))

(ert-deftest ygg-term-outside-a-project-uses-the-space-dir-then-default ()
  (ygg-term-ptests--with
    (let ((plain (ygg-term-tests--dir "plain"))
          (space (ygg-term-tests--dir "space")))
      (cl-letf (((symbol-function 'ygg-term--space-dir) (lambda () space)))
        (with-temp-buffer
          (setq default-directory plain)
          (ygg-term-new "sp-s")))
      (should (equal (ygg-term-ptests--cwd "*ygg-term:sp-s*") space))
      (cl-letf (((symbol-function 'ygg-term--space-dir) (lambda () nil)))
        (with-temp-buffer
          (setq default-directory plain)
          (ygg-term-new "sp-d")))
      (should (equal (ygg-term-ptests--cwd "*ygg-term:sp-d*") plain)))))

(ert-deftest ygg-term-project-beats-the-space-dir ()
  (ygg-term-ptests--with
    (let* ((root (ygg-term-ptests--proj "p"))
           (file (ygg-term-ptests--file root))
           (space (ygg-term-tests--dir "space")))
      (cl-letf (((symbol-function 'ygg-term--space-dir) (lambda () space)))
        (ygg-term-ptests--in file #'ygg-term-new "sp-n")
        (should (equal (ygg-term-ptests--cwd "*ygg-term:sp-n*") root))))))

(ert-deftest ygg-term-dired-here-stays-here ()
  (ygg-term-ptests--with
    (defvar ygg-dired-goto-map (make-sparse-keymap))
    (require 'layer-tasks)
    (let* ((root (ygg-term-ptests--proj "p"))
           (sub (file-name-as-directory (expand-file-name "src/a" root)))
           (at-point (file-name-as-directory (expand-file-name "b" sub))))
      (with-current-buffer (dired-noselect sub)
        (set-window-buffer (selected-window) (current-buffer))
        (ygg-terminal-here-dired))
      (should (equal (buffer-local-value 'default-directory (window-buffer (selected-window)))
                     at-point)))))

(ert-deftest ygg-term-env-homes-match-the-root ()
  (ygg-term-ptests--with
    (let* ((p (ygg-term-ptests--proj "p"))
           (q (ygg-term-ptests--proj "q"))
           (fp (ygg-term-ptests--file p))
           (fq (ygg-term-ptests--file q)))
      (ygg-term-ptests--in fp #'ygg-terminal-toggle)
      (ygg-term-ptests--in fq #'ygg-terminal-toggle)
      (let ((env-q (ygg-term-tests--last))
            (env-p (cadr ygg-term-tests--spawned)))
        (dolist (var ygg-term-tests--homes)
          (should (equal (ygg-term-tests--value var env-p)
                         (ygg-term-tests--value
                          var (ygg-agent-terminal-env p))))
          (should (equal (ygg-term-tests--value var env-q)
                         (ygg-term-tests--value
                          var (ygg-agent-terminal-env q)))))))))

(ert-deftest ygg-term-remote-project-root-is-the-remote-root ()
  (ygg-term-ptests--with
    (let ((remote "/ssh:box:/srv/app/"))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) (cons 'transient remote)))
                ((symbol-function 'project-root) #'cdr))
        (let ((default-directory remote))
          (should (equal (ygg-term--root) remote)))))))

;; in-terminal cd, symlinks, remote space dir, dead terminal

(ert-deftest ygg-term-toggle-inside-the-terminal-hides-after-cd ()
  (ygg-term-ptests--with
    (let* ((root (ygg-term-ptests--proj "p"))
           (file (ygg-term-ptests--file root)))
      (ygg-term-ptests--in file #'ygg-terminal-toggle)
      (let ((term (ygg-term-ptests--toggle-buffer))
            (before (length (buffer-list))))
        (with-current-buffer term
          (setq default-directory "/tmp/")
          (ygg-terminal-toggle))
        (should-not (get-buffer-window term))
        (should (= (length (buffer-list)) before))
        (should-not (get-buffer "*ygg-term:tmp*"))))))

(ert-deftest ygg-term-new-and-pick-inside-the-terminal-use-its-project ()
  (ygg-term-ptests--with
    (let* ((root (ygg-term-ptests--proj "p"))
           (file (ygg-term-ptests--file root))
           offered)
      (ygg-term-ptests--in file #'ygg-terminal-toggle)
      (with-current-buffer (ygg-term-ptests--toggle-buffer)
        (setq default-directory "/tmp/")
        (ygg-term-new "inner")
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_ cands &rest _) (setq offered cands) (car cands))))
          (ygg-term-pick)))
      (should (equal (ygg-term-ptests--cwd "*ygg-term:inner*") root))
      (should (member "*ygg-term:inner*" offered)))))

(ert-deftest ygg-term-symlinked-and-real-paths-share-one-terminal ()
  (ygg-term-ptests--with
    (let* ((root (ygg-term-ptests--proj "p"))
           (link (file-name-as-directory (expand-file-name "link" ygg-term-tests--root)))
           (via-link (expand-file-name "src/a/b/" link))
           (via-real (expand-file-name "src/a/b/" root)))
      (make-symbolic-link (directory-file-name root) (directory-file-name link))
      (with-temp-buffer
        (setq default-directory via-link)
        (set-window-buffer (selected-window) (current-buffer))
        (ygg-terminal-toggle))
      (let ((first (ygg-term-ptests--toggle-buffer)))
        (should (equal (buffer-local-value 'default-directory first) root))
        (with-temp-buffer
          (setq default-directory via-real)
          (set-window-buffer (selected-window) (current-buffer))
          (ygg-terminal-toggle))
        (should (eq (ygg-term-ptests--toggle-buffer) first))
        (should (= 1 (length (seq-filter
                              (lambda (b) (equal (buffer-local-value 'ygg-term--toggle-root b) root))
                              (buffer-list)))))))))

(ert-deftest ygg-term-remote-space-dir-makes-no-project-call ()
  (ygg-term-ptests--with
    (let ((remote "/ssh:box:/srv/app/")
          (plain (ygg-term-tests--dir "plain"))
          probed)
      (cl-letf (((symbol-function 'ygg-term--space-dir) (lambda () remote))
                ((symbol-function 'project-current)
                 (lambda (&rest _)
                   (when (file-remote-p default-directory) (setq probed t))
                   nil)))
        (with-temp-buffer
          (setq default-directory plain)
          (ygg-term-new "rem")))
      (should-not probed)
      (should (equal (buffer-local-value 'ygg-term--buffer-root (get-buffer "*ygg-term:rem*"))
                     remote)))))

(ert-deftest ygg-term-dead-toggle-terminal-is-recreated ()
  (ygg-term-ptests--with
    (let* ((root (ygg-term-ptests--proj "p"))
           (file (ygg-term-ptests--file root)))
      (ygg-term-ptests--in file #'ygg-terminal-toggle)
      (let ((old (ygg-term-ptests--toggle-buffer)))
        (delete-process (get-buffer-process old))
        (ygg-term-ptests--in file #'ygg-terminal-toggle)
        (let ((new (ygg-term-ptests--toggle-buffer)))
          (should-not (buffer-live-p old))
          (should-not (eq new old))
          (should (process-live-p (get-buffer-process new)))
          (should (equal (buffer-local-value 'ygg-term--toggle-root new) root)))))))

(defun ygg-term-ptests--processes (root)
  (with-temp-buffer
    (insert (propertize "row\n" 'ygg-project root 'ygg-row 'processes))
    (goto-char (point-min))
    (ygg-projects-visit)))

(ert-deftest ygg-term-projects-processes-row-is-per-root ()
  (ygg-term-ptests--with
    (let ((p (ygg-term-ptests--proj "p"))
          (q (ygg-term-ptests--proj "q")))
      (cl-letf (((symbol-function 'ygg-projects--docker-p) (lambda (_) nil)))
        (ygg-term-ptests--processes p)
        (let ((bp (ygg-term-ptests--toggle-buffer)))
          (ygg-term-ptests--processes q)
          (let ((bq (ygg-term-ptests--toggle-buffer)))
            (should-not (eq bp bq))
            (should (equal (buffer-local-value 'ygg-term--buffer-root bp) p))
            (should (equal (buffer-local-value 'ygg-term--buffer-root bq) q))))))))

(ert-deftest ygg-term-embark-space-terminal-starts-at-checkout ()
  (ygg-term-ptests--with
    (let* ((p (ygg-term-ptests--proj "p"))
           (q (ygg-term-ptests--proj "q")))
      (cl-letf (((symbol-function 'ygg-embark--space-root) (lambda () q))
                ((symbol-function 'read-string) (lambda (&rest _) "co")))
        (ygg-term-ptests--in (ygg-term-ptests--file p) #'ygg-terminal-toggle)
        (with-current-buffer (ygg-term-ptests--toggle-buffer)
          (ygg-embark-space-terminal))
        (should (equal (ygg-term-ptests--cwd "*ygg-term:co*") q))
        (should (equal (buffer-local-value 'ygg-term--buffer-root (get-buffer "*ygg-term:co*")) q))))))

(ert-deftest ygg-term-missing-root-falls-back-to-existing-ancestor ()
  (ygg-term-ptests--with
    (let* ((p (ygg-term-ptests--proj "p"))
           (gone (expand-file-name "no/such/dir/" p)))
      (ygg-term-ptests--in (ygg-term-ptests--file p)
                           (lambda () (ygg-term-new "gone" gone)))
      (should (equal (ygg-term-ptests--cwd "*ygg-term:gone*")
                     (ygg-term--normalize p))))))

(provide 'ygg-term-project-tests)
;;; ygg-term-project-tests.el ends here
