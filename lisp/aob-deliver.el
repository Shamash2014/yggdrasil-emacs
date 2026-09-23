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
  (aob-session-put s :deliver (aob-deliver--plan where))
  (message (if (eq where 'trace) "aob: answers stay in the trace" "aob: next answer goes %s")
           (car (rassq where aob-deliver-destinations))))

(defun aob-deliver--plan (where)
  "Where WHERE points from here and now, or nil for the trace alone."
  (unless (eq where 'trace)
    (list :where where
          :buffer (current-buffer)
          :beg (copy-marker (if (and (eq where 'region) (use-region-p))
                                (region-beginning)
                              (point)))
          :end (and (eq where 'region) (use-region-p)
                    (copy-marker (region-end))))))

;;;###autoload
(defun aob-ask-to (s where &optional text)
  "Ask S for TEXT and put the answer WHERE.
Without TEXT the question is written in a draft, and asked when it is sent."
  (interactive
   (let* ((s (aob-target))
          (pick (completing-read "Answer goes: "
                                 (mapcar #'car aob-deliver-destinations)
                                 nil t)))
     (list s (cdr (assoc pick aob-deliver-destinations)))))
  (let* ((plan (aob-deliver--plan where))
         (ask (lambda (words _atts)
                (aob-session-put s :deliver plan)
                (aob-prompt s words nil))))
    (if text
        (funcall ask text nil)
      (aob-compose ask nil (format "ask:%s" (aob-session-name s))))))

(provide 'aob-deliver)
;;; aob-deliver.el ends here
