;;; test-elpaca.el --- put built elpaca packages on the load path -*- lexical-binding: t; -*-

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (builds (seq-find #'file-directory-p
                         (list (expand-file-name "../elpaca/builds/" here)
                               (expand-file-name "elpaca/builds/" "~/.emacs.d/")))))
  (when builds
    (dolist (dir (directory-files builds t "\\`[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(provide 'test-elpaca)
;;; test-elpaca.el ends here
