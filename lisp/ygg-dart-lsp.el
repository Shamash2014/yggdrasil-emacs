;;; ygg-dart-lsp.el --- Flutter closing labels and widget outline from the Dart server -*- lexical-binding: t; -*-

;; Needs the closingLabels client capability and the flutterOutline initialization option.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar ygg-modal-special-modes)
(declare-function eglot-current-server "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot--major-modes "eglot")
(declare-function eglot--lsp-position-to-point "eglot" (pos &optional marker))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function yggdrasil-localleader-def "yggdrasil-localleader")

(defconst ygg-dart-lsp-modes '(dart-mode dart-ts-mode))

(defconst ygg-dart-lsp-initialization-options
  '(:flutterOutline t :closingLabels t)
  "closingLabels is the option older SDKs still need beside the capability.")

(defun ygg-dart-lsp-contact-options ()
  "The trailing keyword options of the Dart server's eglot contact."
  (list :initializationOptions ygg-dart-lsp-initialization-options))

(defface ygg-dart-closing-label-face '((t :inherit shadow))
  "The `// Widget' text shown after a closing bracket."
  :group 'eglot)

(defvar ygg-dart-closing-labels-mode)
(defvar-local ygg-dart-lsp--labels nil)
(defvar-local ygg-dart-lsp--label-overlays nil)
(defvar-local ygg-dart-lsp--outline nil)

(defun ygg-dart-lsp--buffer (uri)
  (when-let* ((file (ignore-errors (eglot-uri-to-path uri))))
    (find-buffer-visiting file)))

(defun ygg-dart-lsp--point (pos)
  (save-excursion (eglot--lsp-position-to-point pos)))

(defun ygg-dart-lsp--clear-labels ()
  (mapc #'delete-overlay ygg-dart-lsp--label-overlays)
  (setq ygg-dart-lsp--label-overlays nil))

(defun ygg-dart-lsp--render-labels ()
  (ygg-dart-lsp--clear-labels)
  (when ygg-dart-closing-labels-mode
    (seq-doseq (item ygg-dart-lsp--labels)
      (let* ((end (ygg-dart-lsp--point (plist-get (plist-get item :range) :end)))
             (ov (make-overlay end end nil t nil)))
        (overlay-put ov 'after-string
                     (propertize (concat " // " (plist-get item :label))
                                 'face 'ygg-dart-closing-label-face))
        (overlay-put ov 'ygg-dart-closing-label t)
        (push ov ygg-dart-lsp--label-overlays)))))

;;;###autoload
(define-minor-mode ygg-dart-closing-labels-mode
  "Show Flutter closing labels (// Widget) after the closing brackets."
  :lighter nil
  (ygg-dart-lsp--render-labels))

(defun ygg-dart-lsp--dart-server-p (server)
  (and server (cl-intersection (eglot--major-modes server) ygg-dart-lsp-modes)))

(defun ygg-dart-lsp-capabilities (server caps)
  "CAPS, eglot's client capabilities, plus closingLabels when SERVER is Dart's."
  (if (ygg-dart-lsp--dart-server-p server)
      (plist-put (copy-sequence caps) :experimental
                 (list :closingLabels (make-hash-table :size 0)))
    caps))

(defun ygg-dart-lsp--managed ()
  (if (and (eglot-managed-p) (ygg-dart-lsp--dart-server-p (eglot-current-server)))
      (ygg-dart-closing-labels-mode 1)
    (setq ygg-dart-lsp--labels nil ygg-dart-lsp--outline nil)
    (when ygg-dart-closing-labels-mode (ygg-dart-closing-labels-mode -1))))

(defun ygg-dart-lsp-publish-closing-labels (uri labels)
  "Replace the closing labels of the buffer visiting URI with LABELS."
  (when-let* ((buffer (ygg-dart-lsp--buffer uri)))
    (with-current-buffer buffer
      (setq ygg-dart-lsp--labels labels)
      (ygg-dart-lsp--render-labels))))

(defun ygg-dart-lsp-publish-flutter-outline (uri outline)
  "Keep OUTLINE as the latest widget tree of the buffer visiting URI."
  (when-let* ((buffer (ygg-dart-lsp--buffer uri)))
    (with-current-buffer buffer
      (setq ygg-dart-lsp--outline outline)
      (ygg-dart-lsp--refresh-outline buffer))))

(defvar-local ygg-dart-flutter-outline--source nil)

(defconst ygg-dart-flutter-outline-buffer "*Flutter outline*")

(defvar-keymap ygg-dart-flutter-outline-mode-map
  "RET" #'ygg-dart-flutter-outline-visit
  "g" #'ygg-dart-flutter-outline-refresh)

(define-derived-mode ygg-dart-flutter-outline-mode special-mode "Flutter"
  "The widget tree of a Dart buffer."
  (setq-local truncate-lines t))

(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-dart-flutter-outline-mode))

(defun ygg-dart-flutter-outline--name (node)
  (let ((element (plist-get node :dartElement)))
    (or (plist-get node :className)
        (and element (plist-get element :name))
        (plist-get node :label)
        (plist-get node :kind))))

(defun ygg-dart-flutter-outline--insert (node depth)
  (let ((variable (plist-get node :variableName))
        (name (ygg-dart-flutter-outline--name node)))
    (insert (propertize (concat (make-string (* 2 depth) ?\s) name
                                (if variable (concat " " variable) ""))
                        'ygg-dart-outline-start
                        (plist-get (plist-get node :range) :start))
            "\n")
    (seq-doseq (child (plist-get node :children))
      (ygg-dart-flutter-outline--insert child (1+ depth)))))

(defun ygg-dart-lsp--refresh-outline (source)
  (when-let* ((target (get-buffer ygg-dart-flutter-outline-buffer))
              ((eq (buffer-local-value 'ygg-dart-flutter-outline--source target) source)))
    (ygg-dart-flutter-outline--render target source)))

(defun ygg-dart-flutter-outline--line-column (position)
  (save-excursion
    (goto-char position)
    (list (line-number-at-pos) (current-column))))

(defun ygg-dart-flutter-outline--position (line-column)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (car line-column)))
    (move-to-column (cadr line-column))
    (point)))

(defun ygg-dart-flutter-outline--render (target source)
  (let ((outline (buffer-local-value 'ygg-dart-lsp--outline source)))
    (with-current-buffer target
      (let ((inhibit-read-only t)
            (point (ygg-dart-flutter-outline--line-column (point)))
            (windows (mapcar (lambda (window)
                               (list window
                                     (ygg-dart-flutter-outline--line-column (window-point window))
                                     (ygg-dart-flutter-outline--line-column (window-start window))))
                             (get-buffer-window-list target nil t))))
        (erase-buffer)
        (seq-doseq (child (plist-get outline :children))
          (ygg-dart-flutter-outline--insert child 0))
        (goto-char (ygg-dart-flutter-outline--position point))
        (pcase-dolist (`(,window ,window-point ,window-start) windows)
          (set-window-start window (ygg-dart-flutter-outline--position window-start) t)
          (set-window-point window (ygg-dart-flutter-outline--position window-point)))))))

(defun ygg-dart-flutter-outline-refresh ()
  (interactive)
  (unless (buffer-live-p ygg-dart-flutter-outline--source)
    (user-error "Dart buffer is gone"))
  (ygg-dart-flutter-outline--render (current-buffer) ygg-dart-flutter-outline--source))

(defun ygg-dart-flutter-outline-visit ()
  "Jump to the widget on this line."
  (interactive)
  (let ((start (get-text-property (line-beginning-position) 'ygg-dart-outline-start)))
    (unless (and start (buffer-live-p ygg-dart-flutter-outline--source))
      (user-error "No widget on this line"))
    (pop-to-buffer ygg-dart-flutter-outline--source)
    (goto-char (ygg-dart-lsp--point start))))

;;;###autoload
(defun ygg-dart-flutter-outline ()
  "Show the widget tree of this Dart buffer in a side window."
  (interactive)
  (unless ygg-dart-lsp--outline
    (user-error "No Flutter outline from the Dart server yet"))
  (let ((source (current-buffer))
        (target (get-buffer-create ygg-dart-flutter-outline-buffer)))
    (with-current-buffer target
      (ygg-dart-flutter-outline-mode)
      (setq ygg-dart-flutter-outline--source source)
      (ygg-dart-flutter-outline--render target source))
    (select-window
     (display-buffer-in-side-window target '((side . right) (window-width . 0.3))))))

(with-eval-after-load 'eglot
  (cl-defmethod eglot-client-capabilities :around (server)
    (ygg-dart-lsp-capabilities server (cl-call-next-method)))
  (cl-defmethod eglot-handle-notification
    (_server (_method (eql dart/textDocument/publishClosingLabels))
             &key uri labels &allow-other-keys)
    (ygg-dart-lsp-publish-closing-labels uri labels))
  (cl-defmethod eglot-handle-notification
    (_server (_method (eql dart/textDocument/publishFlutterOutline))
             &key uri outline &allow-other-keys)
    (ygg-dart-lsp-publish-flutter-outline uri outline))
  (add-hook 'eglot-managed-mode-hook #'ygg-dart-lsp--managed))

(with-eval-after-load 'yggdrasil-localleader
  (dolist (mode ygg-dart-lsp-modes)
    (yggdrasil-localleader-def mode "o" #'ygg-dart-flutter-outline "flutter outline")
    (yggdrasil-localleader-def mode "L" #'ygg-dart-closing-labels-mode "closing labels")))

(provide 'ygg-dart-lsp)
;;; ygg-dart-lsp.el ends here
