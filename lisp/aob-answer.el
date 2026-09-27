;;; aob-answer.el --- answer an agent's questions as a form -*- lexical-binding: t; -*-

;;; Commentary:
;; The questions in a session's last reply, pulled out locally and laid
;; out in its compose box: each one quoted, a line under it for the
;; answer.  What goes back is the form as written, less the questions
;; left blank.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'aob)

(defvar-local aob-answer--questions nil
  "The questions this compose buffer's form asks, in order.
Only a draft opened by aob-answer has them, so only its send is pruned.")

(defconst aob-answer--fence-re "^[ \t]*\\(```\\|~~~\\)")

(defconst aob-answer--item-re
  "^\\([0-9]+[.)]\\|[a-zA-Z][.)]\\|[-*+•]\\)[ \t]+\\(.+\\)$")

(defconst aob-answer--sentence-end-re "[.!?]+[\"')*_]*\\([ \t]+\\|\\'\\)")

(defun aob-answer--question-p (sentence)
  (string-match-p "\\?[\"')*_]*\\'" sentence))

(defun aob-answer--sentences (line)
  "The sentences of LINE that ask something."
  (let ((start 0) found)
    (while (and (< start (length line))
                (string-match aob-answer--sentence-end-re line start))
      (let ((sentence (string-trim (substring line start (match-end 0)))))
        (when (aob-answer--question-p sentence)
          (push sentence found))
        (setq start (max (match-end 0) (1+ start)))))
    (let ((rest (string-trim (substring line (min start (length line))))))
      (when (aob-answer--question-p rest)
        (push rest found)))
    (nreverse found)))

(defun aob-answer--key (question)
  (downcase (replace-regexp-in-string "[ \t]+" " " question)))

(defun aob-answer--questions-in (text)
  "The questions TEXT asks, in order and each once, fenced code left out.
A list item that ends in a question keeps its own number or bullet."
  (let ((in-fence nil) (seen nil) found)
    (dolist (raw (split-string (or text "") "\n"))
      (if (string-match-p aob-answer--fence-re raw)
          (setq in-fence (not in-fence))
        (unless in-fence
          (let ((line (string-trim (replace-regexp-in-string
                                    "\\`[ \t]*#+[ \t]+" "" raw))))
            (dolist (pair (if (and (string-match aob-answer--item-re line)
                                   (aob-answer--question-p (match-string 2 line)))
                              (list (cons line (match-string 2 line)))
                            (mapcar (lambda (q) (cons q q))
                                    (aob-answer--sentences line))))
              (let ((key (aob-answer--key (cdr pair))))
                (unless (member key seen)
                  (push key seen)
                  (push (car pair) found))))))))
    (nreverse found)))

(defun aob-answer--last-reply (s)
  "The text of the last top-level message S's agent wrote, or nil."
  (when-let* ((ev (seq-find (lambda (e) (and (eq (plist-get e :type) 'message)
                                             (not (plist-get e :parent))
                                             (not (string-empty-p
                                                   (string-trim (aob-event-text e))))))
                            (aob-session-events s))))
    (aob-event-text ev)))

(defun aob-answer--quote (question)
  (concat "> " question))

(defun aob-answer--form (questions)
  (mapconcat (lambda (q) (concat (aob-answer--quote q) "\n")) questions "\n\n"))

(defun aob-answer (s)
  "Answer the questions in S's last reply in its compose box.
Each question is quoted with a line under it for the answer; those left
blank are dropped when the draft is sent."
  (interactive (list (aob-target)))
  (let* ((reply (or (aob-answer--last-reply s)
                    (user-error "aob: %s has not said anything yet" (aob-session-name s))))
         (questions (or (aob-answer--questions-in reply)
                        (user-error "aob: no questions in %s's last reply"
                                    (aob-session-name s))))
         (old (when-let* ((b (get-buffer (format "compose:%s" (aob-session-name s)))))
                (buffer-local-value 'aob-answer--questions b)))
         (buf (aob-compose s)))
    (with-current-buffer buf
      (let* ((asked (seq-uniq (append old questions)))
             (new (seq-remove (lambda (q) (aob-answer--quote-positions
                                           (buffer-string) (list q)))
                              questions)))
        (goto-char (point-max))
        (when new
          (unless (string-blank-p (buffer-string))
            (insert "\n\n"))
          (insert (aob-answer--form new)))
        (goto-char (or (aob-answer--first-blank-answer (buffer-string) asked)
                       (point-max)))
        (setq aob-answer--questions asked
              aob-compose--tags (cons (format "answers · %d question%s"
                                              (length asked)
                                              (if (cdr asked) "s" ""))
                                      (seq-remove (lambda (tag) (string-prefix-p "answers · " tag))
                                                  aob-compose--tags)))))
    buf))

(defun aob-answer--quote-positions (text questions)
  "Sorted (START . END) of each question's quote line found in TEXT.
END is past the line's newline, where its answer starts."
  (sort (delq nil
              (mapcar (lambda (q)
                        (when (string-match
                               (concat "^" (regexp-quote (aob-answer--quote q))
                                       "[ \t]*\\(\n\\|\\'\\)")
                               text)
                          (cons (match-beginning 0) (match-end 0))))
                      questions))
        (lambda (a b) (< (car a) (car b)))))

(defun aob-answer--first-blank-answer (text questions)
  "Buffer position of the first answer line in TEXT still blank, or nil."
  (let ((quotes (aob-answer--quote-positions text questions)) found)
    (while (and quotes (not found))
      (let ((q (pop quotes)))
        (when (string-blank-p (substring text (cdr q)
                                         (if quotes (caar quotes) (length text))))
          (setq found (1+ (cdr q))))))
    found))

(defun aob-answer--pruned (text questions)
  "TEXT without the QUESTIONS left unanswered; user-error when none is answered."
  (let* ((quotes (aob-answer--quote-positions text questions))
         (kept (list (substring text 0 (if quotes (caar quotes) (length text)))))
         (answered 0))
    (while quotes
      (let* ((q (pop quotes))
             (next (if quotes (caar quotes) (length text))))
        (unless (string-blank-p (substring text (cdr q) next))
          (cl-incf answered)
          (push (substring text (car q) next) kept))))
    (when (zerop answered)
      (user-error "aob: no question answered, nothing sent"))
    (string-trim (apply #'concat (nreverse kept)))))

(defun aob-answer--before-send (text)
  "Prune TEXT when this compose buffer is an answer form."
  (when aob-answer--questions
    (aob-answer--pruned text aob-answer--questions)))

;; first, while the quote lines are still as laid out
(add-hook 'aob-compose-before-send-functions #'aob-answer--before-send -95)

(provide 'aob-answer)
;;; aob-answer.el ends here
