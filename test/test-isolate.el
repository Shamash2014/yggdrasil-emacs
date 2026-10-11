;;; test-isolate.el --- keep test runs off the owner's persisted state -*- lexical-binding: t; -*-

;;; Code:

(defvar test-isolate-root nil)

(unless test-isolate-root
  (let* ((repo (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name))))
         (root (file-name-as-directory (make-temp-file "test-isolate-" t))))
    (setq test-isolate-root root)
    (dolist (name (directory-files repo nil "\\`[^.]"))
      (unless (member name '("var" "test" "transient" "history" "projects.eld" "recentf.eld"
                             "places.eld" "bookmarks" "custom.el" "easysession"))
        (make-symbolic-link (expand-file-name name repo) (expand-file-name name root))))
    (unless (file-exists-p (expand-file-name "elpaca" root))
      (make-symbolic-link (expand-file-name "elpaca" "~/.emacs.d/") (expand-file-name "elpaca" root)))
    (make-directory (expand-file-name "var/" root) t)
    (make-directory (expand-file-name "transient/" root) t)
    (setq user-emacs-directory root
          project-list-file (expand-file-name "projects.eld" root)
          recentf-save-file (expand-file-name "recentf.eld" root)
          savehist-file (expand-file-name "history" root)
          save-place-file (expand-file-name "places.eld" root)
          bookmark-default-file (expand-file-name "bookmarks" root)
          custom-file (expand-file-name "custom.el" root)
          auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" root)
          transient-history-file (expand-file-name "transient/history.el" root)
          transient-levels-file (expand-file-name "transient/levels.el" root)
          transient-values-file (expand-file-name "transient/values.el" root)
          easysession-directory (expand-file-name "easysession/" root))
    (add-hook 'kill-emacs-hook
              (lambda () (ignore-errors (delete-directory root t))))))

(load (expand-file-name "test-elpaca" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(provide 'test-isolate)
;;; test-isolate.el ends here
