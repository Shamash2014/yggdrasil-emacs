;;; layer-http.el --- kulala-style HTTP files on request.el -*- lexical-binding: t; -*-

;; .http/.rest files, the kulala workflow: blocks separated by ###,
;; # comments, file-wide `@name = value' variables, {{name}}
;; substitution.  RET on a block sends it via request.el; the response
;; lands in a side window, JSON pretty-printed.

;;; Code:

(require 'subr-x)

;; pinned like dired-single: menu resolution fails on this one — and
;; request-deferred.el would drag in the `deferred' dependency we
;; don't use, so take request.el alone
(when (fboundp 'elpaca)
  (elpaca (request :host github :repo "tkf/emacs-request"
                   :files ("request.el"))))

(declare-function request "request")
(declare-function request-response-status-code "request")
(declare-function request-response-headers "request")
(declare-function request-response-data "request")
(declare-function request-response-error-thrown "request")

(defvar ygg-http-response-buffer "*http-response*")

;;; Parsing

(defun ygg-http--vars ()
  "File-wide @name = value bindings, later definitions winning."
  (let (vars)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^@\\([A-Za-z0-9_-]+\\)[ \t]*=[ \t]*\\(.*\\)$" nil t)
        (push (cons (match-string-no-properties 1)
                    (string-trim (match-string-no-properties 2)))
              vars)))
    vars))

(defun ygg-http--substitute (text vars)
  (replace-regexp-in-string
   "{{\\([^}]+\\)}}"
   (lambda (m)
     (save-match-data
       (string-match "{{\\([^}]+\\)}}" m)
       (or (cdr (assoc (match-string 1 m) vars)) m)))
   text t t))

(defun ygg-http--block-bounds ()
  "The ### block around point; on a separator, the block below it."
  (save-excursion
    (end-of-line)
    (cons (save-excursion
            (if (re-search-backward "^###" nil t)
                (line-beginning-position 2)
              (point-min)))
          (save-excursion
            (if (re-search-forward "^###" nil t)
                (line-beginning-position)
              (point-max))))))

(defun ygg-http--parse-block ()
  "The request at point as (METHOD URL HEADERS BODY), variables applied."
  (pcase-let ((`(,start . ,end) (ygg-http--block-bounds)))
    (let* ((text (ygg-http--substitute
                  (buffer-substring-no-properties start end)
                  (ygg-http--vars)))
           method url headers body-lines in-body)
      (dolist (l (split-string text "\n"))
        (cond
         (in-body (push l body-lines))
         ((and (not method)
               (string-match
                "^\\([A-Z]+\\)[ \t]+\\(.+?\\)\\([ \t]+HTTP/.*\\)?$" l))
          (setq method (match-string 1 l)
                url (string-trim (match-string 2 l))))
         ((and method (string-match "^\\([^:#\n ]+\\):[ \t]*\\(.*\\)$" l))
          (push (cons (match-string 1 l) (match-string 2 l)) headers))
         ((and method (string-match "^[ \t]*$" l))
          (setq in-body t))))
      (unless method (user-error "ygg-http: no request line in this block"))
      (list method url (nreverse headers)
            (let ((b (string-trim (string-join (nreverse body-lines) "\n"))))
              (unless (string-empty-p b) b))))))

;;; Sending

(defun ygg-http--show (method url resp elapsed)
  (with-current-buffer (get-buffer-create ygg-http-response-buffer)
    (let ((inhibit-read-only t)
          (code (and resp (request-response-status-code resp))))
      (erase-buffer)
      (unless (derived-mode-p 'special-mode) (special-mode))
      (insert (propertize (format "%s %s\n" method url) 'face 'bold)
              (propertize (format "%s · %.2fs\n"
                                  (or code "no response") elapsed)
                          'face (if (and code (< code 400))
                                    'success 'error)))
      (when-let* ((err (and resp (request-response-error-thrown resp))))
        (insert (propertize (format "%S\n" err) 'face 'error)))
      (dolist (h (and resp (request-response-headers resp)))
        (insert (propertize (format "%s: %s\n" (car h) (cdr h))
                            'face 'shadow)))
      (insert "\n")
      (let ((body-start (point))
            (body (and resp (request-response-data resp))))
        (when body
          (insert body)
          (when (string-match-p
                 "json" (or (cdr (assq 'content-type
                                       (request-response-headers resp)))
                            ""))
            (ignore-errors (json-pretty-print body-start (point-max))))))
      (goto-char (point-min)))
    (display-buffer (current-buffer)
                    '(display-buffer-in-side-window (side . right)))))

(defun ygg-http-send ()
  "Send the request block at point; the response opens beside."
  (interactive)
  (unless (require 'request nil t)
    (user-error "ygg-http: request.el not installed yet — restart Emacs"))
  (pcase-let ((`(,method ,url ,headers ,body) (ygg-http--parse-block)))
    (message "ygg-http: %s %s…" method url)
    (let ((started (current-time)))
      (request url
        :type method
        :headers headers
        :data (and body (encode-coding-string body 'utf-8))
        :parser #'buffer-string
        :complete
        (cl-function
         (lambda (&key response &allow-other-keys)
           (ygg-http--show method url response
                           (float-time (time-subtract (current-time)
                                                      started)))))))))

;;; The mode

(defvar ygg-http--font-lock
  `(("^###.*$" 0 'font-lock-comment-delimiter-face)
    ("^#[^#].*$" 0 'font-lock-comment-face)
    ("^@\\([A-Za-z0-9_-]+\\)" 1 'font-lock-variable-name-face)
    ("{{[^}]+}}" 0 'font-lock-variable-name-face t)
    (,(concat "^\\(GET\\|POST\\|PUT\\|PATCH\\|DELETE\\|HEAD\\|OPTIONS"
              "\\|TRACE\\|CONNECT\\)\\b")
     1 'font-lock-keyword-face)
    ("^\\([A-Za-z-]+\\):" 1 'font-lock-builtin-face)))

(defun ygg-http-ret ()
  "RET: newline while inserting, send the block at point otherwise."
  (interactive)
  (if (and (fboundp 'ygg-insert-p) (ygg-insert-p))
      (newline)
    (ygg-http-send)))

(define-derived-mode ygg-http-mode prog-mode "http"
  "Kulala-style HTTP request files."
  (setq-local comment-start "# "
              comment-start-skip "#+[ \t]*"
              font-lock-defaults '(ygg-http--font-lock)))

(define-key ygg-http-mode-map (kbd "RET") #'ygg-http-ret)

(add-to-list 'auto-mode-alist '("\\.\\(http\\|rest\\)\\'" . ygg-http-mode))

(provide 'layer-http)
;;; layer-http.el ends here
