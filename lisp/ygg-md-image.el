;;; ygg-md-image.el --- Paste a clipboard image into a markdown or org file -*- lexical-binding: t; -*-

;; The clipboard image is saved beside the file, in ygg-md-image-directory,
;; named by date and a slug you type, and linked at point by a relative path.
;; pngpaste reads the clipboard when installed, else osascript, else Emacs.

;;; Code:

(require 'subr-x)

(declare-function gui-get-selection "select" (&optional type data-type))

(defgroup ygg-md-image nil
  "Pasting clipboard images into markdown and org files."
  :group 'markdown)

(defcustom ygg-md-image-directory "assets"
  "Folder, relative to the file being edited, that pasted images go in."
  :type 'string)

(defun ygg-md-image--nonempty-p (file)
  (and (file-exists-p file)
       (> (or (file-attribute-size (file-attributes file)) 0) 0)))

(defun ygg-md-image--via-pngpaste (file)
  (and (zerop (call-process "pngpaste" nil nil nil file))
       (ygg-md-image--nonempty-p file)))

(defun ygg-md-image--via-osascript (file)
  (let ((path (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" file)))
    (and (zerop (call-process
                 "osascript" nil nil nil
                 "-e" "set png to (the clipboard as «class PNGf»)"
                 "-e" (format "set f to open for access POSIX file \"%s\" with write permission" path)
                 "-e" "set eof f to 0"
                 "-e" "write png to f"
                 "-e" "close access f"))
         (ygg-md-image--nonempty-p file))))

(defun ygg-md-image--via-emacs (file)
  (when-let* ((data (ignore-errors (gui-get-selection 'CLIPBOARD 'image/png)))
              ((stringp data))
              ((not (string-empty-p data))))
    (let ((coding-system-for-write 'no-conversion))
      (write-region (string-to-unibyte data) nil file nil 'silent))
    (ygg-md-image--nonempty-p file)))

(defun ygg-md-image--backend ()
  "The function that writes the clipboard image to a file, by what is installed."
  (cond ((executable-find "pngpaste") #'ygg-md-image--via-pngpaste)
        ((and (eq system-type 'darwin) (executable-find "osascript"))
         #'ygg-md-image--via-osascript)
        (t #'ygg-md-image--via-emacs)))

(defun ygg-md-image--write-clipboard (file)
  "Write the clipboard image to FILE as PNG; nil when the clipboard holds none."
  (funcall (ygg-md-image--backend) file))

(defun ygg-md-image--slug (text)
  (let ((slug (string-trim (replace-regexp-in-string
                            "[^a-z0-9]+" "-" (downcase text))
                           "-+" "-+")))
    (if (string-empty-p slug) (format-time-string "%H%M%S") slug)))

(defun ygg-md-image--free-name (dir base)
  "A file name in DIR from BASE that no file holds yet."
  (let ((name (expand-file-name (concat base ".png") dir))
        (n 1))
    (while (file-exists-p name)
      (setq n (1+ n)
            name (expand-file-name (format "%s-%d.png" base n) dir)))
    name))

(defun ygg-md-image--link (path alt)
  (if (derived-mode-p 'org-mode)
      (format "[[file:%s]]" path)
    (format "![%s](%s)" alt path)))

(defun ygg-md-image--check ()
  (unless (derived-mode-p 'markdown-mode 'org-mode)
    (user-error "Not a markdown or org buffer"))
  (unless buffer-file-name (user-error "Save the buffer to a file first")))

(defun ygg-md-paste-image (slug)
  "Save the clipboard image beside this file and link it at point.
SLUG names the image after today's date; empty means the time of day."
  (interactive
   (progn
     (ygg-md-image--check)
     (list (read-string (format "Image name (default %s): "
                                (format-time-string "%H%M%S"))))))
  (ygg-md-image--check)
  (let* ((here (file-name-directory buffer-file-name))
         (dir (expand-file-name ygg-md-image-directory here))
         (slug (ygg-md-image--slug (or slug "")))
         (temp (make-temp-file "ygg-md-image" nil ".png")))
    (unwind-protect
        (progn
          (delete-file temp)
          (unless (ygg-md-image--write-clipboard temp)
            (user-error "The clipboard holds no image"))
          (make-directory dir t)
          (let ((target (ygg-md-image--free-name
                         dir (concat (format-time-string "%Y-%m-%d-") slug))))
            (rename-file temp target)
            (insert (ygg-md-image--link (file-relative-name target here) slug))
            target))
      (when (file-exists-p temp) (delete-file temp)))))

(provide 'ygg-md-image)
;;; ygg-md-image.el ends here
