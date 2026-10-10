;;; ygg-dart-lsp-tests.el --- Flutter closing labels and outline from the Dart server -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'eglot)
(require 'layer-lsp)
(require 'layer-rass)
(require 'ygg-dart-lsp)

(defun ygg-dart-lsp-tests--range (l1 c1 l2 c2)
  `(:start (:line ,l1 :character ,c1) :end (:line ,l2 :character ,c2)))

(defmacro ygg-dart-lsp-tests--with-buffer (uri &rest body)
  (declare (indent 1))
  `(let* ((file (make-temp-file "ygg-dart" nil ".dart"
                                "Widget build() {\n  return Center(\n    child: Text('x'),\n  );\n}\n"))
          (buffer (find-file-noselect file))
          (,uri (concat "file://" file)))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (kill-buffer buffer)
       (delete-file file))))

(defun ygg-dart-lsp-tests--after-strings ()
  (sort (mapcar (lambda (ov) (cons (overlay-start ov) (substring-no-properties (overlay-get ov 'after-string))))
                (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (car a) (car b)))))

(ert-deftest ygg-dart-lsp-closing-labels-sit-after-the-range-end ()
  (ygg-dart-lsp-tests--with-buffer uri
    (ygg-dart-closing-labels-mode 1)
    (ygg-dart-lsp-publish-closing-labels
     uri (vector `(:label "Center" :range ,(ygg-dart-lsp-tests--range 1 9 3 3))))
    (let ((strings (ygg-dart-lsp-tests--after-strings)))
      (should (equal strings (list (cons (save-excursion (goto-char (point-min)) (forward-line 3) (move-to-column 3) (point))
                                         " // Center"))))
      (should (eq (get-text-property 0 'face (overlay-get (car ygg-dart-lsp--label-overlays) 'after-string))
                  'ygg-dart-closing-label-face)))))

(ert-deftest ygg-dart-lsp-closing-labels-replace-and-clear ()
  (ygg-dart-lsp-tests--with-buffer uri
    (ygg-dart-closing-labels-mode 1)
    (ygg-dart-lsp-publish-closing-labels
     uri (vector `(:label "Center" :range ,(ygg-dart-lsp-tests--range 1 9 3 3))
                 `(:label "Text" :range ,(ygg-dart-lsp-tests--range 2 11 2 24))))
    (should (= 2 (length (ygg-dart-lsp-tests--after-strings))))
    (ygg-dart-lsp-publish-closing-labels
     uri (vector `(:label "Text" :range ,(ygg-dart-lsp-tests--range 2 11 2 24))))
    (should (equal (mapcar #'cdr (ygg-dart-lsp-tests--after-strings)) '(" // Text")))
    (ygg-dart-closing-labels-mode -1)
    (should-not (ygg-dart-lsp-tests--after-strings))
    (ygg-dart-lsp-publish-closing-labels
     uri (vector `(:label "Text" :range ,(ygg-dart-lsp-tests--range 2 11 2 24))))
    (should-not (ygg-dart-lsp-tests--after-strings))
    (ygg-dart-closing-labels-mode 1)
    (should (= 1 (length (ygg-dart-lsp-tests--after-strings))))))

(ert-deftest ygg-dart-lsp-handles-the-custom-notifications ()
  (ygg-dart-lsp-tests--with-buffer uri
    (ygg-dart-closing-labels-mode 1)
    (eglot-handle-notification
     nil 'dart/textDocument/publishClosingLabels
     :uri uri :labels (vector `(:label "Center" :range ,(ygg-dart-lsp-tests--range 1 9 3 3))))
    (should (= 1 (length (ygg-dart-lsp-tests--after-strings))))
    (eglot-handle-notification
     nil 'dart/textDocument/publishFlutterOutline
     :uri uri :outline '(:kind "DART_ELEMENT" :range nil :children []))
    (should ygg-dart-lsp--outline)))

(ert-deftest ygg-dart-lsp-unmanaging-clears-labels ()
  (ygg-dart-lsp-tests--with-buffer uri
    (ygg-dart-closing-labels-mode 1)
    (ygg-dart-lsp-publish-closing-labels
     uri (vector `(:label "Center" :range ,(ygg-dart-lsp-tests--range 1 9 3 3))))
    (cl-letf (((symbol-function 'eglot-managed-p) #'ignore))
      (ygg-dart-lsp--managed))
    (should-not ygg-dart-closing-labels-mode)
    (should-not (ygg-dart-lsp-tests--after-strings))
    (should-not ygg-dart-lsp--labels)))

(defconst ygg-dart-lsp-tests--outline
  `(:kind "DART_ELEMENT" :range ,(ygg-dart-lsp-tests--range 0 0 4 1)
    :dartElement (:name "build")
    :children
    [(:kind "NEW_INSTANCE" :className "Center" :range ,(ygg-dart-lsp-tests--range 1 9 3 3)
      :children [(:kind "NEW_INSTANCE" :className "Text" :range ,(ygg-dart-lsp-tests--range 2 11 2 24)
                  :children [])])]))

(ert-deftest ygg-dart-lsp-outline-buffer-lists-widgets-and-jumps ()
  (ygg-dart-lsp-tests--with-buffer uri
    (ygg-dart-lsp-publish-flutter-outline
     uri `(:children [,ygg-dart-lsp-tests--outline]))
    (let ((source (current-buffer)))
      (save-window-excursion
        (ygg-dart-flutter-outline)
        (should (equal (buffer-string)
                       "build\n  Center\n    Text\n"))
        (goto-char (point-min))
        (forward-line 2)
        (ygg-dart-flutter-outline-visit)
        (should (eq (current-buffer) source))
        (should (= (line-number-at-pos) 3))
        (should (= (current-column) 11)))
      (kill-buffer ygg-dart-flutter-outline-buffer))))

(ert-deftest ygg-dart-lsp-outline-refreshes-in-place ()
  (ygg-dart-lsp-tests--with-buffer uri
    (ygg-dart-lsp-publish-flutter-outline uri `(:children [,ygg-dart-lsp-tests--outline]))
    (save-window-excursion
      (ygg-dart-flutter-outline)
      (with-current-buffer ygg-dart-flutter-outline-buffer
        (should (string-match-p "Text" (buffer-string)))))
    (ygg-dart-lsp-publish-flutter-outline
     uri `(:children [(:kind "NEW_INSTANCE" :className "Row" :range ,(ygg-dart-lsp-tests--range 1 9 3 3) :children [])]))
    (with-current-buffer ygg-dart-flutter-outline-buffer
      (should (equal (substring-no-properties (buffer-string)) "Row\n")))
    (kill-buffer ygg-dart-flutter-outline-buffer)))

(ert-deftest ygg-dart-lsp-outline-render-keeps-window-point ()
  (ygg-dart-lsp-tests--with-buffer uri
    (ygg-dart-lsp-publish-flutter-outline uri `(:children [,ygg-dart-lsp-tests--outline]))
    (save-window-excursion
      (ygg-dart-flutter-outline)
      (let ((window (get-buffer-window ygg-dart-flutter-outline-buffer)))
        (with-current-buffer ygg-dart-flutter-outline-buffer
          (goto-char (point-min))
          (forward-line 2)
          (set-window-point window (point)))
        (ygg-dart-lsp-publish-flutter-outline uri `(:children [,ygg-dart-lsp-tests--outline]))
        (with-current-buffer ygg-dart-flutter-outline-buffer
          (should (= (line-number-at-pos (window-point window)) 3)))))
    (kill-buffer ygg-dart-flutter-outline-buffer)))

(ert-deftest ygg-dart-lsp-outline-refresh-needs-the-dart-buffer ()
  (with-temp-buffer
    (ygg-dart-flutter-outline-mode)
    (let ((dead (generate-new-buffer "dead")))
      (kill-buffer dead)
      (setq ygg-dart-flutter-outline--source dead)
      (should-error (ygg-dart-flutter-outline-refresh) :type 'user-error))))

(ert-deftest ygg-dart-lsp-outline-needs-an-outline ()
  (with-temp-buffer
    (should-error (ygg-dart-flutter-outline) :type 'user-error)))

(ert-deftest ygg-dart-lsp-contact-asks-for-labels-and-outline ()
  (let ((contact (ygg-lsp-dart-contact)))
    (should (equal (seq-take (member "language-server" contact) 2)
                   '("language-server" "--protocol=lsp")))
    (should (equal (plist-get (nthcdr (cl-position :initializationOptions contact) contact)
                              :initializationOptions)
                   '(:flutterOutline t :closingLabels t)))))

(ert-deftest ygg-dart-lsp-rass-wrapped-contact-keeps-the-options ()
  (cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name))))
    (let* ((default-directory "/tmp/")
           (wrapped (ygg-rass-with-harper (ygg-lsp-dart-contact))))
      (should (equal (car wrapped) "rass"))
      (should (equal (plist-get (nthcdr (cl-position :initializationOptions wrapped) wrapped)
                                :initializationOptions)
                     '(:flutterOutline t :closingLabels t)))
      (should (equal (nthcdr (- (length wrapped) 2) wrapped)
                     (list :initializationOptions '(:flutterOutline t :closingLabels t)))))))

(ert-deftest ygg-dart-lsp-client-capabilities-carry-closing-labels-for-dart-only ()
  (cl-letf (((symbol-function 'eglot--major-modes) (lambda (s) (if (eq s 'dart) '(dart-mode) '(go-mode)))))
    (let ((dart (ygg-dart-lsp-capabilities 'dart '(:workspace nil :experimental nil)))
          (go (ygg-dart-lsp-capabilities 'go '(:workspace nil :experimental nil))))
      (should (hash-table-p (plist-get (plist-get dart :experimental) :closingLabels)))
      (should-not (plist-get (plist-get go :experimental) :closingLabels)))))

(ert-deftest ygg-dart-lsp-outline-mode-is-modal-special ()
  (require 'yggdrasil-core)
  (should (memq 'ygg-dart-flutter-outline-mode ygg-modal-special-modes)))

(ert-deftest ygg-dart-lsp-localleader-keys ()
  (require 'yggdrasil-localleader)
  (dolist (mode '(dart-mode dart-ts-mode))
    (let ((map (gethash mode ygg-localleader--maps)))
      (should (eq (lookup-key map "o") 'ygg-dart-flutter-outline))
      (should (eq (lookup-key map "L") 'ygg-dart-closing-labels-mode)))))

;;; ygg-dart-lsp-tests.el ends here
