;;; ygg-git-compare-comments-qf-tests.el --- a compare's comments as a quickfix list -*- lexical-binding: t; -*-

;;; Code:

(setq load-prefer-newer t)

(let ((builds (expand-file-name "../elpaca/builds/"
                                (file-name-directory
                                 (or load-file-name buffer-file-name)))))
  (dolist (p '("magit" "magit-section" "compat" "dash" "llama" "cond-let"
               "transient" "with-editor"))
    (add-to-list 'load-path (expand-file-name p builds))))

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'layer-quickfix)
(require 'ygg-git-compare)
(require 'ygg-git-compare-comments)

(defun ygg-git-compare-comments-qf-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defmacro ygg-git-compare-comments-qf-tests--with-compare (list &rest body)
  "BODY with LIST bound to the compare of a repo whose feature rewrites a.txt."
  (declare (indent 1))
  `(let* ((process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment))
          (root (file-name-as-directory
                 (file-truename (make-temp-file "ygg-git-compare-comments-qf-" t))))
          (default-directory root)
          (magit-refresh-verbose nil))
     (unwind-protect
         (cl-flet ((git (&rest args) (apply #'ygg-git-compare-comments-qf-tests--git root args))
                   (write (file text) (with-temp-file (expand-file-name file root)
                                        (insert text))))
           (git "init" "-q" "-b" "main")
           (git "config" "user.name" "Comments Test")
           (git "config" "user.email" "comments@example.invalid")
           (git "config" "commit.gpgsign" "false")
           (write "a.txt" "1\n2\n3\n")
           (git "add" ".")
           (git "commit" "-q" "-m" "base")
           (git "checkout" "-q" "-b" "feature")
           (write "a.txt" "1\ntwo\nthree\n")
           (git "add" ".")
           (git "commit" "-q" "-m" "feature")
           (git "checkout" "-q" "main")
           (let ((,list (ygg-git-compare-buffer root '(rev . "main") '(rev . "feature"))))
             (with-current-buffer ,list (magit-section-show-level-4-all))
             ,@body))
       (dolist (b (buffer-list))
         (when (or (string-prefix-p "compose:review:" (buffer-name b))
                   (and (with-current-buffer b (derived-mode-p 'magit-mode))
                        (file-in-directory-p (buffer-local-value 'default-directory b) root)))
           (kill-buffer b)))
       (delete-directory root t))))

(defun ygg-git-compare-comments-qf-tests--add (list line text &optional type)
  (with-current-buffer list
    (goto-char (point-min))
    (search-forward line)
    (beginning-of-line)
    (with-current-buffer (ygg-git-compare-comment)
      (insert text)
      (setq ygg-git-compare--draft (plist-put ygg-git-compare--draft :type type))
      (aob-compose-send))))

(defun ygg-git-compare-comments-qf-tests--comments (list)
  (with-current-buffer list (ygg-git-compare-comments-list t)))

(defun ygg-git-compare-comments-qf-tests--show (list)
  (with-current-buffer list (ygg-git-compare-comments-qf))
  (ygg-qf-buffer-create))

(defun ygg-git-compare-comments-qf-tests--goto-row (text)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(ert-deftest ygg-git-compare-comments-qf-rows-match-comments ()
  (ygg-git-compare-comments-qf-tests--with-compare list
    (ygg-git-compare-comments-qf-tests--add list "+two" "first line\nsecond" 'fix)
    (ygg-git-compare-comments-qf-tests--add list "+three" "other")
    (let ((rows (ygg-git-compare-comments-qf--rows list))
          (comments (ygg-git-compare-comments-qf-tests--comments list)))
      (should (= (length rows) 2))
      (should (equal (mapcar #'car rows)
                     (mapcar (lambda (c) (cons list (plist-get c :id))) comments)))
      (should (equal (cadr (car rows)) "a.txt:2  first line"))
      (should (string-search "agent" (nth 2 (car rows))))
      (should (string-search "[fix]" (nth 2 (car rows))))
      (should (string-search "PR" (nth 2 (cadr rows)))))
    (with-current-buffer (ygg-git-compare-comments-qf-tests--show list)
      (should (string-search "a.txt:2  first line" (buffer-string)))
      (should (string-search "a.txt:3  other" (buffer-string)))
      (should-not (string-search "second" (buffer-string))))))

(ert-deftest ygg-git-compare-comments-qf-row-drops-carriage-returns ()
  (ygg-git-compare-comments-qf-tests--with-compare list
    (ygg-git-compare-comments-qf-tests--add list "+two" "first line\r\nsecond")
    (let ((row (cadr (car (ygg-git-compare-comments-qf--rows list)))))
      (should (equal row "a.txt:2  first line"))
      (should-not (string-search "\r" row)))))

(ert-deftest ygg-git-compare-comments-qf-dead-compare-is-a-user-error ()
  (let ((dead (generate-new-buffer " dead-compare")))
    (kill-buffer dead)
    (should-error (ygg-git-compare-comments-qf--find (cons dead "x")) :type 'user-error)
    (should-error (ygg-git-compare-comments-qf-goto (cons dead "x")) :type 'user-error)
    (should-error (ygg-git-compare-comments-qf-copy (cons dead "x")) :type 'user-error)
    (should-error (ygg-git-compare-comments-qf-drop (cons dead "x")) :type 'user-error)))

(ert-deftest ygg-git-compare-comments-qf-action-lands-on-the-comment ()
  (ygg-git-compare-comments-qf-tests--with-compare list
    (ygg-git-compare-comments-qf-tests--add list "+two" "first")
    (ygg-git-compare-comments-qf-tests--add list "+three" "other")
    (let ((id (plist-get (cadr (ygg-git-compare-comments-qf-tests--comments list)) :id)))
      (with-current-buffer list (goto-char (point-min)))
      (ygg-git-compare-comments-qf-goto (cons list id))
      (should (eq (current-buffer) list))
      (should (member id (seq-mapcat
                          (lambda (ov) (overlay-get ov 'ygg-git-compare-comments))
                          (overlays-in (line-beginning-position) (line-end-position))))))))

(ert-deftest ygg-git-compare-comments-qf-drop-deletes-and-list-follows ()
  (ygg-git-compare-comments-qf-tests--with-compare list
    (ygg-git-compare-comments-qf-tests--add list "+two" "first")
    (ygg-git-compare-comments-qf-tests--add list "+three" "other")
    (with-current-buffer (ygg-git-compare-comments-qf-tests--show list)
      (ygg-git-compare-comments-qf-tests--goto-row "a.txt:2")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (ygg-qf-drop))
      (should (equal (mapcar (lambda (c) (plist-get c :text))
                             (ygg-git-compare-comments-qf-tests--comments list))
                     '("other")))
      (should-not (string-search "a.txt:2" (buffer-string)))
      (should (string-search "a.txt:3  other" (buffer-string))))
    (with-current-buffer list
      (ygg-git-compare-comments-drop
       (list (plist-get (car (ygg-git-compare-comments-qf-tests--comments list)) :id))))
    (with-current-buffer (ygg-qf-buffer-create)
      (should-not (string-search "a.txt:3" (buffer-string))))))

(ert-deftest ygg-git-compare-comments-qf-drop-refuses-a-forge-comment ()
  (should-error (ygg-git-compare-comments-qf-drop (cons (current-buffer) "remote:1"))
                :type 'user-error))

(ert-deftest ygg-git-compare-comments-qf-follows-until-the-compare-dies ()
  (ygg-git-compare-comments-qf-tests--with-compare list
    (ygg-git-compare-comments-qf-tests--add list "+two" "first")
    (let ((qf (ygg-git-compare-comments-qf-tests--show list)))
      (kill-buffer list)
      (sleep-for 0.05)
      (with-current-buffer qf
        (should-not (plist-get ygg-qf--kind :token))))))

(ert-deftest ygg-git-compare-comments-redraw-runs-the-hook-once ()
  (ygg-git-compare-comments-qf-tests--with-compare list
    (let* ((spare (list (generate-new-buffer " spare-1")
                        (generate-new-buffer " spare-2")
                        (generate-new-buffer " spare-3")))
           (calls nil)
           (hook (lambda (l) (push l calls))))
      (unwind-protect
          (progn
            (add-hook 'ygg-git-compare-comments-changed-functions hook)
            (ygg-git-compare--redraw-comments list)
            (should (equal calls (list list))))
        (remove-hook 'ygg-git-compare-comments-changed-functions hook)
        (mapc #'kill-buffer spare)))))

(ert-deftest ygg-git-compare-comments-qf-key-is-bound-in-the-compare-map ()
  (should (eq (keymap-lookup ygg-git-compare-mode-map "Q") #'ygg-git-compare-comments-qf)))

(provide 'ygg-git-compare-comments-qf-tests)
;;; ygg-git-compare-comments-qf-tests.el ends here
