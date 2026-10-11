;;; aob-link.el --- Org links and bookmarks to aob sessions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'aob)
(require 'aob-acp)

(defvar aob-compose--target)
(declare-function ygg-aob--goto "layer-aob" (s))
(declare-function org-link-store-props "ol")
(declare-function org-link-set-parameters "ol")
(declare-function bookmark-prop-get "bookmark")
(declare-function bookmark-get-filename "bookmark")

(defun aob-link--encode (string)
  (url-hexify-string string))

(defun aob-link--decode (string)
  (decode-coding-string (url-unhex-string string) 'utf-8))

(defun aob-link--build (acp-id agent dir)
  "The link path for conversation ACP-ID of AGENT that ran in DIR."
  (concat (aob-link--encode acp-id)
          "?"
          (string-join
           (delq nil
                 (list (and agent (concat "agent=" (aob-link--encode agent)))
                       (and dir (concat "dir=" (aob-link--encode
                                                (expand-file-name dir))))))
           "&")))

(defun aob-link--parse (path)
  "The list (ACP-ID AGENT DIR) link PATH stands for."
  (let* ((qpos (string-search "?" path))
         (acp-id (aob-link--decode (if qpos (substring path 0 qpos) path)))
         agent dir)
    (dolist (pair (and qpos (split-string (substring path (1+ qpos)) "&" t)))
      (let* ((eq (string-search "=" pair))
             (val (aob-link--decode (if eq (substring pair (1+ eq)) ""))))
        (pcase (if eq (substring pair 0 eq) pair)
          ("agent" (setq agent val))
          ("dir" (setq dir val)))))
    (list acp-id agent dir)))

(defun aob-link--here ()
  "The session of the current trace or compose buffer, with an ACP id."
  (let ((s (aob-session-get (or aob-buffer-session-id
                                (and (boundp 'aob-compose--target)
                                     (stringp aob-compose--target)
                                     aob-compose--target)))))
    (and s (aob-session-ref s :acp-id) s)))

(defun aob-link--fields (s)
  (list (aob-session-ref s :acp-id)
        (let ((agent (aob-session-ref s :agent)))
          (and agent (format "%s" agent)))
        (aob-session-dir s)))

(defun aob-link--live (acp-id)
  (seq-find (lambda (s)
              (and (equal (aob-session-ref s :acp-id) acp-id)
                   (not (aob-session-ref s :hidden))
                   (not (aob-session-ref s :asleep))
                   (not (memq (aob-session-state s) '(dead failed)))))
            (aob-sessions)))

(defun aob-link--entry (acp-id)
  (or (seq-find (lambda (e) (equal (plist-get e :acp-id) acp-id))
                (aob-acp--persisted-entries))
      (when-let* ((holder (aob-acp--holder acp-id)))
        (aob-acp--entry holder))))

(defun aob-link-open (acp-id agent dir)
  "Go to the session ACP-ID, resume it, or start a fresh AGENT in DIR."
  (unless (and acp-id (not (string-empty-p acp-id)))
    (user-error "aob link has no session id"))
  (cond
   ((aob-link--live acp-id) (ygg-aob--goto (aob-link--live acp-id)))
   ((aob-link--entry acp-id) (aob-acp-resume-entry (aob-link--entry acp-id)))
   ((not (and dir (file-directory-p dir)))
    (user-error "aob: session %s is gone and so is its folder %s" acp-id dir))
   ((y-or-n-p (format "Session gone; start a new %s in %s? "
                      (or agent aob-acp-default-agent)
                      (abbreviate-file-name dir)))
    (aob-acp-spawn-with (or agent aob-acp-default-agent) dir nil))))

(defun aob-link-org-store ()
  "Store an Org link to the aob session of this buffer, or return nil."
  (when-let* ((s (aob-link--here)))
    (pcase-let ((`(,acp-id ,agent ,dir) (aob-link--fields s)))
      (org-link-store-props
       :type "aob"
       :link (concat "aob:" (aob-link--build acp-id agent dir))
       :description (aob-session-name s)))))

(defun aob-link-org-follow (path &optional _arg)
  "Follow the aob Org link PATH."
  (apply #'aob-link-open (aob-link--parse path)))

(defun aob-link-bookmark-make-record ()
  "A bookmark record for the aob session of this buffer."
  (let ((s (or (aob-link--here) (user-error "No aob session in this buffer"))))
    (pcase-let ((`(,acp-id ,agent ,dir) (aob-link--fields s)))
      (list (aob-session-name s)
            (cons 'handler #'aob-link-bookmark-jump)
            (cons 'acp-id acp-id)
            (cons 'agent agent)
            (cons 'filename dir)
            (cons 'location (aob-session-name s))))))

(defun aob-link-bookmark-jump (bookmark)
  "Jump to the aob session BOOKMARK records."
  (require 'bookmark)
  (aob-link-open (bookmark-prop-get bookmark 'acp-id)
                 (bookmark-prop-get bookmark 'agent)
                 (bookmark-get-filename bookmark)))

(defun aob-link--enable-bookmark ()
  (setq-local bookmark-make-record-function #'aob-link-bookmark-make-record))

(add-hook 'aob-trace-mode-hook #'aob-link--enable-bookmark)

(with-eval-after-load 'org
  (org-link-set-parameters "aob"
                           :follow #'aob-link-org-follow
                           :store #'aob-link-org-store))

(provide 'aob-link)

;;; aob-link.el ends here
