;;; md-image-tests.el --- Pasting a clipboard image into markdown and org -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'ygg-md-image)
(require 'markdown-mode)
(require 'org)

(defconst md-image-tests--png "\211PNG\r\n\032\nfake"
  "Bytes standing in for a clipboard image.")

(defmacro md-image-tests--with-file (mode clipboard &rest body)
  "Run BODY in a MODE buffer visiting a file in a fresh directory.
CLIPBOARD is the image the stubbed clipboard holds, nil for none."
  (declare (indent 2))
  `(let* ((dir (file-name-as-directory (make-temp-file "md-image" t)))
          (file (expand-file-name "notes/page.md" dir)))
     (make-directory (file-name-directory file) t)
     (unwind-protect
         (cl-letf (((symbol-function 'ygg-md-image--write-clipboard)
                    (lambda (target)
                      (when ,clipboard
                        (let ((coding-system-for-write 'no-conversion))
                          (write-region ,clipboard nil target nil 'silent))
                        t))))
           (with-temp-buffer
             (setq buffer-file-name file)
             (,mode)
             ,@body))
       (delete-directory dir t))))

(defun md-image-tests--today (slug)
  (format "assets/%s-%s.png" (format-time-string "%Y-%m-%d") slug))

(ert-deftest md-image-pastes-beside-the-file-with-a-relative-link ()
  (md-image-tests--with-file gfm-mode md-image-tests--png
    (insert "see ")
    (let ((target (ygg-md-paste-image "My Shot!")))
      (should (equal (buffer-string)
                     (format "see ![my-shot](%s)" (md-image-tests--today "my-shot"))))
      (should (equal target (expand-file-name (md-image-tests--today "my-shot")
                                              (file-name-directory file))))
      (should (equal (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally target)
                       (buffer-string))
                     md-image-tests--png)))))

(ert-deftest md-image-never-overwrites-an-earlier-paste ()
  (md-image-tests--with-file markdown-mode md-image-tests--png
    (let ((first (ygg-md-paste-image "shot"))
          (second (ygg-md-paste-image "shot")))
      (should-not (equal first second))
      (should (string-suffix-p "-shot-2.png" second))
      (should (file-exists-p first)))))

(ert-deftest md-image-empty-slug-names-by-time ()
  (md-image-tests--with-file markdown-mode md-image-tests--png
    (should (string-match-p "/[0-9-]+-[0-9]\\{6\\}\\.png\\'" (ygg-md-paste-image "")))))

(ert-deftest md-image-refuses-when-the-clipboard-holds-no-image ()
  (md-image-tests--with-file markdown-mode nil
    (let ((err (should-error (ygg-md-paste-image "shot") :type 'user-error)))
      (should (equal (cadr err) "The clipboard holds no image")))
    (should (equal (buffer-string) ""))
    (should-not (file-exists-p (expand-file-name "assets" (file-name-directory file))))))

(ert-deftest md-image-links-org-files-the-org-way ()
  (md-image-tests--with-file org-mode md-image-tests--png
    (ygg-md-paste-image "diagram")
    (should (equal (buffer-string)
                   (format "[[file:%s]]" (md-image-tests--today "diagram"))))))

(ert-deftest md-image-refuses-buffers-it-cannot-link-from ()
  (with-temp-buffer
    (markdown-mode)
    (should-error (ygg-md-paste-image "x") :type 'user-error))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/md-image-tests.txt")
    (text-mode)
    (should-error (ygg-md-paste-image "x") :type 'user-error)))

(ert-deftest md-image-reads-the-clipboard-with-what-is-installed ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name &rest _) (equal name "pngpaste"))))
    (should (eq (ygg-md-image--backend) #'ygg-md-image--via-pngpaste)))
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &rest _) (equal name "osascript"))))
      (should (eq (ygg-md-image--backend) #'ygg-md-image--via-osascript))))
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find) #'ignore))
      (should (eq (ygg-md-image--backend) #'ygg-md-image--via-emacs)))))

(provide 'md-image-tests)
;;; md-image-tests.el ends here
