;;; ygg-theme-tests.el --- Tests for layer-theme -*- lexical-binding: t; -*-

(load (expand-file-name "test-isolate" (file-name-directory (or load-file-name buffer-file-name))) nil t)
(require 'ert)
(require 'cl-lib)
(dolist (root (list user-emacs-directory
                    (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (let ((dir (expand-file-name "elpaca/builds/foxglove-themes" root)))
    (when (file-directory-p dir) (add-to-list 'load-path dir))))
(require 'layer-theme)

(defconst ygg-theme-tests--themes '(modus-operandi modus-vivendi foxglove foxglove-dark))

(defun ygg-theme-tests--palette (theme)
  (if (ygg-theme-dark-p theme) ygg-theme-palette-dark ygg-theme-palette-light))

(defun ygg-theme-tests--entry (theme key)
  (cadr (assq key (ygg-theme-tests--palette theme))))

(defmacro ygg-theme-tests--isolated (&rest body)
  `(let ((ygg-theme-file (make-temp-file "ygg-theme-")))
     (unwind-protect
         (progn (mapc #'disable-theme custom-enabled-themes) ,@body)
       (mapc #'disable-theme custom-enabled-themes)
       (delete-file ygg-theme-file))))

(defun ygg-theme-tests--skip-without-foxglove ()
  (unless (locate-library "foxglove-themes")
    (ert-skip "foxglove-themes not on load-path")))

(defun ygg-theme-tests--file-theme ()
  (string-trim (with-temp-buffer (insert-file-contents ygg-theme-file) (buffer-string))))

(defun ygg-theme-tests--only-p (theme)
  (equal custom-enabled-themes (list theme)))

(defmacro ygg-theme-tests--without-foxglove (&rest body)
  `(cl-letf (((symbol-function 'ygg-theme--ready-p)
              (lambda (theme) (eq (ygg-theme-family theme) 'modus))))
     ,@body))

(defmacro ygg-theme-tests--deffamily (name family themes &rest body)
  (declare (indent 3))
  `(ert-deftest ,(intern (format "%s-%s" name family)) ()
     ,@(when (eq family 'foxglove) '((ygg-theme-tests--skip-without-foxglove)))
     (dolist (theme ',themes)
       (ygg-theme-tests--isolated ,@body))))

(defmacro ygg-theme-tests--deffamilies (name &rest body)
  (declare (indent 1))
  `(progn
     (ygg-theme-tests--deffamily ,name modus (modus-operandi modus-vivendi) ,@body)
     (ygg-theme-tests--deffamily ,name foxglove (foxglove foxglove-dark) ,@body)))

(ygg-theme-tests--deffamilies ygg-theme-loads
  (ygg-theme-set theme)
  (should (eq (ygg-theme-current) theme))
  (should (ygg-theme-tests--only-p theme))
  (should (equal (ygg-theme-tests--file-theme) (symbol-name theme))))

(ygg-theme-tests--deffamilies ygg-theme-palette-colours-reach-theme
  (ygg-theme-set theme)
  (should (equal (ygg-theme-color 'cursor) (ygg-theme-tests--entry theme 'cursor)))
  (should (equal (ygg-theme-color 'bg-main) (ygg-theme-tests--entry theme 'bg-main)))
  (should (equal (ygg-theme-color 'bg-float) (ygg-theme-tests--entry theme 'bg-float))))

(ert-deftest ygg-theme-dark-predicate ()
  (should-not (ygg-theme-dark-p 'modus-operandi))
  (should (ygg-theme-dark-p 'modus-vivendi))
  (should-not (ygg-theme-dark-p 'foxglove))
  (should (ygg-theme-dark-p 'foxglove-dark)))

(ert-deftest ygg-theme-toggle-flips-mode-within-modus ()
  (ygg-theme-tests--isolated
   (ygg-theme-set 'modus-operandi)
   (ygg-theme-toggle)
   (should (ygg-theme-tests--only-p 'modus-vivendi))
   (should (equal (ygg-theme-tests--file-theme) "modus-vivendi"))
   (ygg-theme-toggle)
   (should (ygg-theme-tests--only-p 'modus-operandi))))

(ert-deftest ygg-theme-toggle-flips-mode-within-foxglove ()
  (ygg-theme-tests--skip-without-foxglove)
  (ygg-theme-tests--isolated
   (ygg-theme-set 'foxglove)
   (ygg-theme-toggle)
   (should (ygg-theme-tests--only-p 'foxglove-dark))
   (should (equal (ygg-theme-tests--file-theme) "foxglove-dark"))
   (ygg-theme-toggle)
   (should (ygg-theme-tests--only-p 'foxglove))))

(ert-deftest ygg-theme-toggle-in-fallback-keeps-foxglove-base ()
  (ygg-theme-tests--isolated
   (write-region "foxglove-dark" nil ygg-theme-file nil 'silent)
   (ygg-theme-tests--without-foxglove
    (ygg-theme-load-saved)
    (should (ygg-theme-tests--only-p 'modus-vivendi))
    (ygg-theme-toggle)
    (should (ygg-theme-tests--only-p 'modus-operandi))
    (should (equal (ygg-theme-tests--file-theme) "foxglove")))))

(ert-deftest ygg-theme-toggle-starts-from-enabled-theme ()
  (ygg-theme-tests--isolated
   (ygg-theme-set 'modus-operandi)
   (mapc #'disable-theme custom-enabled-themes)
   (load-theme 'modus-vivendi t)
   (ygg-theme-toggle)
   (should (ygg-theme-tests--only-p 'modus-operandi))
   (should (equal (ygg-theme-tests--file-theme) "modus-operandi"))))

(ert-deftest ygg-theme-switch-base-in-fallback-leaves-foxglove ()
  (ygg-theme-tests--isolated
   (write-region "foxglove-dark" nil ygg-theme-file nil 'silent)
   (ygg-theme-tests--without-foxglove
    (ygg-theme-load-saved)
    (ygg-theme-switch-base)
    (should (ygg-theme-tests--only-p 'modus-vivendi))
    (should (equal (ygg-theme-tests--file-theme) "modus-vivendi")))))

(ert-deftest ygg-theme-switch-base-keeps-mode ()
  (ygg-theme-tests--skip-without-foxglove)
  (ygg-theme-tests--isolated
   (ygg-theme-set 'modus-operandi)
   (ygg-theme-switch-base)
   (should (ygg-theme-tests--only-p 'foxglove))
   (should (equal (ygg-theme-tests--file-theme) "foxglove"))
   (ygg-theme-switch-base)
   (should (ygg-theme-tests--only-p 'modus-operandi))
   (ygg-theme-set 'modus-vivendi)
   (ygg-theme-switch-base)
   (should (ygg-theme-tests--only-p 'foxglove-dark))
   (ygg-theme-switch-base)
   (should (ygg-theme-tests--only-p 'modus-vivendi))))

(ert-deftest ygg-theme-switch-base-without-foxglove-is-user-error ()
  (ygg-theme-tests--isolated
   (ygg-theme-set 'modus-vivendi)
   (ygg-theme-tests--without-foxglove
    (should-error (ygg-theme-switch-base) :type 'user-error))
   (should (ygg-theme-tests--only-p 'modus-vivendi))
   (should (equal (ygg-theme-tests--file-theme) "modus-vivendi"))))

(ert-deftest ygg-theme-saved-reads-back ()
  (ygg-theme-tests--isolated
   (dolist (theme ygg-theme-tests--themes)
     (write-region (symbol-name theme) nil ygg-theme-file nil 'silent)
     (should (eq (ygg-theme-saved) theme)))))

(ert-deftest ygg-theme-saved-falls-back-to-operandi ()
  (ygg-theme-tests--isolated
   (dolist (garbage '("" "wombat" "((" "modus-vivendi-tinted"))
     (write-region garbage nil ygg-theme-file nil 'silent)
     (should (eq (ygg-theme-saved) 'modus-operandi)))
   (delete-file ygg-theme-file)
   (should (eq (ygg-theme-saved) 'modus-operandi))))

(ert-deftest ygg-theme-load-saved-falls-back-to-modus-of-same-mode ()
  (ygg-theme-tests--isolated
   (write-region "foxglove-dark" nil ygg-theme-file nil 'silent)
   (ygg-theme-tests--without-foxglove
    (ygg-theme-load-saved))
   (should (ygg-theme-tests--only-p 'modus-vivendi))
   (should (equal (ygg-theme-tests--file-theme) "foxglove-dark"))))

(ert-deftest ygg-theme-load-saved-skips-reload-when-enabled ()
  (ygg-theme-tests--isolated
   (ygg-theme-set 'modus-vivendi)
   (let ((calls 0))
     (cl-letf (((symbol-function 'ygg-theme--enable) (lambda (_) (cl-incf calls))))
       (ygg-theme-load-saved))
     (should (= calls 0)))))

(ert-deftest ygg-theme-load-saved-upgrades-fallback-once-foxglove-arrives ()
  (ygg-theme-tests--skip-without-foxglove)
  (ygg-theme-tests--isolated
   (write-region "foxglove-dark" nil ygg-theme-file nil 'silent)
   (ygg-theme-tests--without-foxglove
    (ygg-theme-load-saved))
   (should (ygg-theme-tests--only-p 'modus-vivendi))
   (ygg-theme-load-saved)
   (should (ygg-theme-tests--only-p 'foxglove-dark))))

(ygg-theme-tests--deffamilies ygg-theme-float-face-follows-palette
  (set-face-attribute 'ygg-float nil :background 'unspecified)
  (ygg-theme-set theme)
  (should (equal (face-attribute 'ygg-float :background nil)
                 (ygg-theme-tests--entry theme 'bg-float))))

;;; ygg-theme-tests.el ends here
