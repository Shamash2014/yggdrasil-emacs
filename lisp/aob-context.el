;;; aob-context.el --- what the agent is given to read, kept where you can see it -*- lexical-binding: t; -*-

;;; Commentary:
;; gptel's best small idea: context is a set you build up and can look at,
;; not a thing you retype into every prompt.  Add a region or a file, open
;; the list to prune it, and whatever is in it rides along with the next
;; message you send.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'aob)

(defgroup aob-context nil
  "What the agent is handed to read."
  :group 'aob :prefix "aob-context-")

(defcustom aob-context-max-chars 40000
  "How much context is sent before the tail is left off."
  :type 'natnum :group 'aob-context)

(defvar aob-context--items nil
  "Plists of (:file :beg :end :text), newest first.")

(defface aob-context-path '((t :inherit link))
  "Face for a context entry's file." :group 'aob-context)

(defun aob-context--label (item)
  (let ((file (plist-get item :file)))
    (if (plist-get item :beg)
        (format "%s:%s-%s" (abbreviate-file-name file)
                (plist-get item :beg) (plist-get item :end))
      (abbreviate-file-name file))))

(defun aob-context-size ()
  "Characters the context currently holds."
  (apply #'+ (mapcar (lambda (i) (length (plist-get i :text)))
                     aob-context--items)))

;;;###autoload
(defun aob-context-add (&optional beg end)
  "Add the region, or this whole file, to the context."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (text (substring-no-properties
                (filter-buffer-substring (or beg (point-min)) (or end (point-max)))))
         (item (list :file file :text text
                     :beg (and beg (line-number-at-pos beg))
                     :end (and end (line-number-at-pos end)))))
    (push item aob-context--items)
    (message "aob context: %s (%d entries, %d chars)"
             (aob-context--label item)
             (length aob-context--items) (aob-context-size))))

;;;###autoload
(defun aob-context-add-file (file)
  "Add FILE to the context whole."
  (interactive "fContext file: ")
  (with-temp-buffer
    (insert-file-contents file)
    (push (list :file (expand-file-name file)
                :text (buffer-substring-no-properties (point-min) (point-max)))
          aob-context--items))
  (message "aob context: %s (%d entries)" (abbreviate-file-name file)
           (length aob-context--items)))

;;;###autoload
(defun aob-context-clear ()
  "Empty the context, asking first when called as a command."
  (interactive)
  (when (or (not (called-interactively-p 'any))
            (null aob-context--items)
            (y-or-n-p (format "Drop all %d context entries? " (length aob-context--items))))
    (setq aob-context--items nil)
    (message "aob context: empty")))

;; SPC a c X → the entries as a quickfix, oldest first; a row is the entry's
;; :id, given when first listed so it survives drops.  The list follows the
;; variable, so adding, dropping and clearing from anywhere never leave it stale.
(declare-function ygg-qf-define-kind "layer-quickfix")
(declare-function ygg-qf-show-kind "layer-quickfix")
(declare-function ygg-qf-kind-refresh "layer-quickfix")
(declare-function ygg-qf-kind-target-id "layer-quickfix")

(defvar aob-context--next-id 0)

(defun aob-context--id (item)
  (or (plist-get item :id)
      (let ((id (cl-incf aob-context--next-id)))
        (plist-put item :id id)
        id)))

(defun aob-context--ids ()
  "Every entry as (ID . ITEM), oldest first."
  (mapcar (lambda (item) (cons (aob-context--id item) item))
          (reverse aob-context--items)))

(defun aob-context--find (id)
  (or (cdr (assq id (aob-context--ids)))
      (user-error "aob context: that entry is gone")))

(defun aob-context--collect ()
  (mapcar (lambda (e)
            (let ((item (cdr e)))
              (list (car e)
                    (propertize (aob-context--label item)
                                'font-lock-face 'aob-context-path)
                    (format "%s, %d chars" (if (plist-get item :beg) "region" "file")
                            (length (plist-get item :text))))))
          (aob-context--ids)))

(defun aob-context-drop (&optional id)
  "Drop the context entry ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (let ((item (aob-context--find (or id (user-error "aob context: no entry here")))))
    (setq aob-context--items (delq item aob-context--items))))

(defun aob-context-visit (&optional id)
  "Open the file of the context entry ID, by default the one on this row."
  (interactive (list (ygg-qf-kind-target-id)))
  (let* ((item (aob-context--find (or id (user-error "aob context: no entry here"))))
         (file (plist-get item :file)))
    (unless (and file (file-exists-p file)) (user-error "aob context: no file here"))
    (find-file-other-window file)
    (when-let* ((line (plist-get item :beg)))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defvar aob-context-row-map
  (let ((m (make-sparse-keymap)))
    (define-key m "o" #'aob-context-visit)
    (define-key m "d" #'aob-context-drop)
    m)
  "What embark offers on a context row of the quickfix.")

(defvar embark-general-map)

(with-eval-after-load 'embark
  (set-keymap-parent aob-context-row-map embark-general-map))

(defun aob-context--follow (buf token watch)
  (unless (ygg-qf-kind-refresh buf token)
    (remove-variable-watcher 'aob-context--items watch)))

(defun aob-context--arm (buf token)
  (letrec ((watch (lambda (&rest _)
                    (run-at-time 0 nil #'aob-context--follow buf token watch))))
    (add-variable-watcher 'aob-context--items watch)
    (lambda () (remove-variable-watcher 'aob-context--items watch))))

(with-eval-after-load 'layer-quickfix
  (ygg-qf-define-kind 'context
                      :collect #'aob-context--collect
                      :action #'aob-context-visit
                      :map 'aob-context-row-map
                      :arm #'aob-context--arm
                      :drop #'aob-context-drop))

;;;###autoload
(defun aob-context-list ()
  "Collect the context entries into the quickfix; a row opens its file."
  (interactive)
  (unless aob-context--items
    (user-error "aob context: nothing yet: add a region with SPC a c x"))
  (require 'layer-quickfix)
  (ygg-qf-show-kind 'context))

(defun aob-context--block (item)
  (format "<context %s>\n%s\n</context>"
          (aob-context--label item) (plist-get item :text)))

(defun aob-context--join (items)
  "ITEMS as one block of text, cut at aob-context-max-chars."
  (let ((all (string-join (mapcar #'aob-context--block items) "\n\n")))
    (if (<= (length all) aob-context-max-chars)
        all
      (concat (substring all 0 aob-context-max-chars)
              (format "\n… %d chars left off"
                      (- (length all) aob-context-max-chars))))))

(defun aob-context-text ()
  "The context as one block of text to send, or nil when empty."
  (when aob-context--items
    (aob-context--join (reverse aob-context--items))))

(defun aob-context--prints (label items)
  (mapcar (lambda (i) (secure-hash 'sha1 (plist-get i :text)))
          (seq-filter (lambda (i) (equal (aob-context--label i) label)) items)))

(defun aob-context-untold (s)
  "The context S has not yet been told, as (TEXT . PENDING).
TEXT is nil when S knows it all.  PENDING is what to note on S once the
prompt carrying TEXT lands, as aob-told-pending holds it; an entry the
cut left off is not in it, so it goes again next time.  With
aob-dedupe-context off, the whole context and nothing to note."
  (if (not (and s aob-dedupe-context))
      (list (aob-context-text))
    (let* ((items (reverse aob-context--items))
           (fresh (seq-remove
                   (lambda (label)
                     (aob-told-p s :context-told label
                                 (aob-context--prints label items)))
                   (delete-dups (mapcar #'aob-context--label items))))
           (send (seq-filter (lambda (i) (member (aob-context--label i) fresh))
                             items)))
      (when send
        (let ((end 0) (cut nil))
          (dolist (i send)
            (setq end (+ end (length (aob-context--block i))))
            (when (> end aob-context-max-chars)
              (push (aob-context--label i) cut))
            (setq end (+ end 2)))
          (cons (aob-context--join send)
                (mapcar (lambda (label)
                          (list :context-told label (aob-context--prints label items)))
                        (seq-remove (lambda (label) (member label cut)) fresh))))))))

(provide 'aob-context)
;;; aob-context.el ends here
