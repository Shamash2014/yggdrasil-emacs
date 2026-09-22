;;; aob-deliver.el --- where an answer lands -*- lexical-binding: t; -*-

;;; Commentary:
;; An agent's reply always goes to its trace.  Sometimes the trace is the
;; wrong place: the answer belongs at point, or over the region it was
;; asked about, or on the kill ring.  Name a destination for the next
;; answer and the reply is put there as well as traced.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aob)

(defgroup aob-deliver nil
  "Where an agent's answer is put."
  :group 'aob :prefix "aob-deliver-")

(defconst aob-deliver-destinations
  '(("trace" . trace)
    ("at point" . point)
    ("replace region" . region)
    ("new buffer" . buffer)
    ("kill ring" . kill-ring)
    ("echo" . echo))
  "What can be named as the home of the next answer.")

(defun aob-deliver--last-message (s)
  "The text of the last thing S said."
  (when-let* ((ev (seq-find (lambda (e) (eq (plist-get e :type) 'message))
                            (aob-session-events s))))
    (string-trim (or (aob-event-text ev) ""))))

(defun aob-deliver--put (plan text)
  "Put TEXT where PLAN says."
  (pcase (plist-get plan :where)
    ('kill-ring (kill-new text) (message "aob: answer on the kill ring"))
    ('echo (message "%s" text))
    ('buffer (let ((buf (get-buffer-create "*aob-answer*")))
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (erase-buffer) (insert text) (goto-char (point-min))))
               (display-buffer buf)))
    ((or 'point 'region)
     (let ((buf (plist-get plan :buffer))
           (beg (plist-get plan :beg))
           (end (plist-get plan :end)))
       (if (not (buffer-live-p buf))
           (message "aob: the buffer the answer was meant for is gone")
         (with-current-buffer buf
           (save-excursion
             (when (and end (eq (plist-get plan :where) 'region))
               (delete-region (marker-position beg) (marker-position end)))
             (goto-char (marker-position beg))
             (insert text))))))
    (_ nil)))

(defun aob-deliver--on-event (s ev)
  "Deliver S's answer once its turn ends."
  (when (and (eq (plist-get ev :type) 'stop)
             (aob-session-ref s :deliver))
    (let ((plan (aob-session-ref s :deliver))
          (text (aob-deliver--last-message s)))
      (aob-session-put s :deliver nil)
      (when (and text (not (string-empty-p text)))
        (aob-deliver--put plan text)))))

(add-hook 'aob-event-functions #'aob-deliver--on-event)

;;;###autoload
(defun aob-deliver-to (s where)
  "Say WHERE S's next answer should be put, besides its trace."
  (interactive
   (let* ((s (aob-target))
          (pick (completing-read "Answer goes: "
                                 (mapcar #'car aob-deliver-destinations)
                                 nil t)))
     (list s (cdr (assoc pick aob-deliver-destinations)))))
  (if (eq where 'trace)
      (progn (aob-session-put s :deliver nil)
             (message "aob: answers stay in the trace"))
    (aob-session-put
     s :deliver
     (list :where where
           :buffer (current-buffer)
           :beg (copy-marker (if (and (eq where 'region) (use-region-p))
                                 (region-beginning)
                               (point)))
           :end (and (eq where 'region) (use-region-p)
                     (copy-marker (region-end)))))
    (message "aob: next answer goes %s"
             (car (rassq where aob-deliver-destinations)))))

;;;###autoload
(defun aob-ask-to (s where text)
  "Ask S for TEXT and put the answer WHERE."
  (interactive
   (let* ((s (aob-target))
          (pick (completing-read "Answer goes: "
                                 (mapcar #'car aob-deliver-destinations)
                                 nil t))
          (where (cdr (assoc pick aob-deliver-destinations))))
     (list s where (read-string (format "%s » " (aob-session-name s))))))
  (aob-deliver-to s where)
  (aob-prompt s text nil))

(provide 'aob-deliver)
;;; aob-deliver.el ends here
