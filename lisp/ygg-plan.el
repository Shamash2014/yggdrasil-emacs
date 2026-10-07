;;; ygg-plan.el --- show-me plans answered in place and sent to the agent -*- lexical-binding: t; -*-

;;; Commentary:
;; A plan is a markdown file in `.aob/plans/' (see skills/show-me/references/plan.md).
;; The buffer opens folded to its level-1 claims.  Decisions are picked with
;; RET, claims struck or commented, and one key builds the response the
;; format asks for and sends it to the agent session working in the project.
;; The answers live beside the buffer, never in the agent's text: picks,
;; opened decisions, strikes and comments are held in buffer-local state and
;; mirrored to `<slug>.answers.eld' next to the plan, so the file lints as
;; the agent wrote it and an answer survives closing the buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'outline)
(require 'markdown-mode)
(require 'yggdrasil-core)
(require 'yggdrasil-localleader)
(require 'ygg-diagram)
(require 'ygg-markdown-fences)

(declare-function aob-live-sessions "aob" ())
(declare-function aob-session-dir "aob" (s))
(declare-function aob-session-project "aob" (s))
(declare-function aob-session-name "aob" (s))
(declare-function aob-session-ref "aob" (s key))
(declare-function aob-read-session "aob" (prompt &optional sessions))
(declare-function aob-prompt "aob" (s text &optional attachments))
(declare-function ygg-markdown-fences--fontify "ygg-markdown-fences" (mode body))
(defvar aob-prompt-typed)
(defvar read-eval)

(defgroup ygg-plan nil "Show-me plans answered in place." :group 'markdown)

(defcustom ygg-plan-source-lines 200
  "Most lines of a file one `src' fence shows."
  :type 'natnum :group 'ygg-plan)

(defcustom ygg-plan-source-bytes 1000000
  "Most bytes read from a file to fill one `src' fence."
  :type 'natnum :group 'ygg-plan)

(defface ygg-plan-struck '((t :inherit shadow :strike-through t))
  "A claim the owner struck." :group 'ygg-plan)

(defface ygg-plan-note '((t :inherit font-lock-doc-face))
  "A comment on a claim." :group 'ygg-plan)

(defface ygg-plan-suggested '((t :inherit shadow))
  "The option the plan suggests." :group 'ygg-plan)

(defface ygg-plan-picked '((t :inherit success))
  "The option the owner picked." :group 'ygg-plan)

(defconst ygg-plan--heading-re "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*$")
(defconst ygg-plan--number-re "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.*\\)\\'")
(defconst ygg-plan--fence-open-re "^[ \t]*\\(```+\\|~~~+\\)[ \t]*\\(.*?\\)[ \t]*$")
(defconst ygg-plan--question-re "^[ \t]*Q:[ \t]*\\(.*?\\)[ \t]*$")
(defconst ygg-plan--option-re "^[ \t]*\\([a-z]\\))[ \t]+\\(.*?\\)[ \t]*$")
(defconst ygg-plan--pick-re "^[ \t]*pick:[ \t]*\\([a-z]\\)\\(?:[ \t]+-[ \t]+\\(.*?\\)\\)?[ \t]*$")

(defvar-local ygg-plan--picks nil "Decision key -> (LETTER . CHOICE) picked.")
(defvar-local ygg-plan--opened nil "Keys of the decisions point has entered.")
(defvar-local ygg-plan--struck nil "Ids of the claims struck, each (KEY . TEXT).")
(defvar-local ygg-plan--comments nil "Claim id -> comments, oldest first.")
(defvar-local ygg-plan--memo nil "The scan of this buffer as (TICK . MODEL).")
(defvar-local ygg-plan--timer nil "The pending redraw after an edit.")
(defvar-local ygg-plan--depth 0 "Index into `ygg-plan--depths' of the whole-buffer fold.")

(defconst ygg-plan--depths '(2 3 4 nil)
  "Heading levels the whole-buffer fold shows in turn; nil shows everything.")

(defun ygg-plan--scan ()
  "The buffer as a plist of :title, :claims, :decisions and :fences."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (title status claims decisions fences claim fence decision)
        (cl-flet ((finish ()
                    (when decision
                      (push (plist-put decision :options
                                       (nreverse (plist-get decision :options)))
                            decisions)
                      (setq decision nil))))
          (when (looking-at-p "---[ \t]*$")
            (forward-line 1)
            (while (and (not (eobp)) (not (looking-at-p "---[ \t]*$")))
              (when (looking-at "status:[ \t]*\\(.*?\\)[ \t]*$")
                (setq status (match-string-no-properties 1)))
              (forward-line 1))
            (forward-line 1))
          (while (not (eobp))
            (let* ((bol (point))
                   (eol (line-end-position))
                   (line (buffer-substring-no-properties bol eol)))
              (cond
               (fence
                (if (string-match-p (concat "\\`[ \t]*" (regexp-quote (plist-get fence :marker))
                                            (regexp-quote (substring (plist-get fence :marker) 0 1))
                                            "*[ \t]*\\'")
                                    line)
                    (progn (push (plist-put (plist-put fence :close bol)
                                            :empty (not (plist-get fence :body)))
                                 fences)
                           (setq fence nil))
                  (unless (string-blank-p line) (plist-put fence :body t))))
               ((string-match ygg-plan--fence-open-re line)
                (finish)
                (setq fence (list :marker (match-string 1 line) :info (match-string 2 line)
                                  :open bol :body nil)))
               ((string-match ygg-plan--heading-re line)
                (finish)
                (let* ((level (length (match-string 1 line)))
                       (text (match-string 2 line)))
                  (if (and (= level 1) (not title))
                      (setq title text)
                    (when (> level 1)
                      (let* ((numbered (string-match ygg-plan--number-re text))
                             (no (and numbered (match-string 1 text)))
                             (words (if numbered (match-string 2 text) text)))
                        (setq claim (list :no no :key (or no words) :level level
                                          :text words :beg bol :eol eol))
                        (push claim claims))))))
               ((string-match ygg-plan--question-re line)
                (finish)
                (setq decision (list :claim (plist-get claim :no)
                                     :question (match-string 1 line)
                                     :key (cons (plist-get claim :key) (match-string 1 line))
                                     :beg bol :end eol :options nil)))
               ((and decision (string-match ygg-plan--option-re line))
                (plist-put decision :options
                           (cons (list :letter (match-string 1 line) :text (match-string 2 line)
                                       :beg bol :eol eol)
                                 (plist-get decision :options)))
                (plist-put decision :end eol))
               ((and decision (string-match ygg-plan--pick-re line))
                (plist-put decision :pick (match-string 1 line))
                (plist-put decision :why (match-string 2 line))
                (plist-put decision :pick-beg bol)
                (plist-put decision :end eol)
                (finish))
               (t (finish))))
            (forward-line 1))
          (finish))
        (list :title title :status status :claims (nreverse claims)
              :decisions (nreverse decisions) :fences (nreverse fences))))))

(defun ygg-plan--model ()
  "The scan of this buffer, held until the text changes."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql (car ygg-plan--memo) tick)
      (setq ygg-plan--memo (cons tick (ygg-plan--scan))))
    (cdr ygg-plan--memo)))

(defun ygg-plan--claim-at (pos)
  "The claim whose heading is the last at or before POS."
  (let (found)
    (dolist (c (plist-get (ygg-plan--model) :claims))
      (when (<= (plist-get c :beg) pos) (setq found c)))
    found))

(defun ygg-plan--decision-at (pos)
  "The decision POS is in, from its question to its last line."
  (seq-find (lambda (d) (<= (plist-get d :beg) pos (1+ (plist-get d :end))))
            (plist-get (ygg-plan--model) :decisions)))

(defun ygg-plan--choice (option)
  "What OPTION offers, without the tradeoff that follows its dash."
  (car (split-string (plist-get option :text) "[ \t]+-[ \t]+")))

(defun ygg-plan--root ()
  "The project the plan belongs to: where `.aob' lives."
  (or (and buffer-file-name (locate-dominating-file buffer-file-name ".aob"))
      default-directory))

(defun ygg-plan--answers-file ()
  (when buffer-file-name
    (concat (file-name-sans-extension buffer-file-name) ".answers.eld")))

(defun ygg-plan--claim-id (claim)
  "What identifies CLAIM across rewrites: its number and its words."
  (cons (plist-get claim :key) (plist-get claim :text)))

(defun ygg-plan--claim-ids ()
  (mapcar #'ygg-plan--claim-id (plist-get (ygg-plan--model) :claims)))

(defun ygg-plan--option (decision letter)
  (seq-find (lambda (o) (equal (plist-get o :letter) letter)) (plist-get decision :options)))

(defun ygg-plan--picked (decision)
  "The letter picked for DECISION while that option still reads as it did."
  (let* ((entry (cdr (assoc (plist-get decision :key) ygg-plan--picks)))
         (option (and (consp entry) (ygg-plan--option decision (car entry)))))
    (and option (equal (cdr entry) (ygg-plan--choice option)) (car entry))))

(defun ygg-plan--reconcile ()
  "Drop the answers whose decision, option or claim the plan no longer has.
Non-nil when something was dropped."
  (let* ((decisions (plist-get (ygg-plan--model) :decisions))
         (keys (mapcar (lambda (d) (plist-get d :key)) decisions))
         (ids (ygg-plan--claim-ids))
         (before (list ygg-plan--picks ygg-plan--opened ygg-plan--struck ygg-plan--comments)))
    (setq ygg-plan--picks
          (seq-filter (lambda (e)
                        (and (consp e)
                             (when-let* ((d (seq-find (lambda (d) (equal (plist-get d :key) (car e)))
                                                      decisions)))
                               (ygg-plan--picked d))))
                      ygg-plan--picks)
          ygg-plan--opened (seq-filter (lambda (k) (member k keys)) ygg-plan--opened)
          ygg-plan--struck (seq-filter (lambda (id) (member id ids)) ygg-plan--struck)
          ygg-plan--comments (seq-filter (lambda (e) (and (consp e) (member (car e) ids)))
                                         ygg-plan--comments))
    (not (equal before (list ygg-plan--picks ygg-plan--opened ygg-plan--struck
                             ygg-plan--comments)))))

(defun ygg-plan--save ()
  "Write the answers beside the plan, or take the file away when none are left."
  (when-let* ((file (ygg-plan--answers-file)))
    (if (or ygg-plan--picks ygg-plan--opened ygg-plan--struck ygg-plan--comments)
        (let ((data (list :status (plist-get (ygg-plan--model) :status)
                          :claims (ygg-plan--claim-ids)
                          :picks ygg-plan--picks :opened ygg-plan--opened
                          :struck ygg-plan--struck :comments ygg-plan--comments))
              (tmp (make-temp-file (concat file "."))))
          (unwind-protect
              (progn
                (with-temp-file tmp
                  (let ((print-length nil) (print-level nil))
                    (prin1 data (current-buffer))))
                (rename-file tmp file t))
            (when (file-exists-p tmp) (delete-file tmp))))
      (when (file-exists-p file) (delete-file file)))))

(defun ygg-plan--load ()
  "Take back the saved answers, or none when the plan moved on."
  (when-let* ((file (ygg-plan--answers-file))
              ((file-readable-p file))
              (data (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (let ((read-eval nil)) (read (current-buffer)))))))
    (when (proper-list-p data)
      (let ((claims (plist-get data :claims))
            (ids (ygg-plan--claim-ids)))
        (if (or (not (equal (plist-get data :status) (plist-get (ygg-plan--model) :status)))
                (and (proper-list-p claims) claims (not (seq-intersection claims ids #'equal))))
            (ignore-errors (delete-file file))
          (cl-flet ((field (key) (let ((v (plist-get data key))) (and (proper-list-p v) v))))
            (setq ygg-plan--picks (field :picks)
                  ygg-plan--opened (field :opened)
                  ygg-plan--struck (field :struck)
                  ygg-plan--comments (field :comments))))))))

(defun ygg-plan--changed-answers ()
  (ygg-plan--save)
  (ygg-plan--refresh))

(defun ygg-plan--line-range (spec)
  "SPEC, \"a-b\" or \"a\", as (FIRST . LAST), or nil."
  (when (and spec (string-match "\\`\\([0-9]+\\)\\(?:-\\([0-9]*\\)\\)?\\'" spec))
    (let* ((first (string-to-number (match-string 1 spec)))
           (last (match-string 2 spec))
           (end (cond ((null last) first)
                      ((string-empty-p last) most-positive-fixnum)
                      (t (string-to-number last)))))
      (and (>= first 1) (>= end first) (cons first end)))))

(defun ygg-plan--source (root src lines lang)
  "The text of SRC under ROOT for LINES, drawn as a code block."
  (let* ((file (expand-file-name src root))
         (range (if lines (ygg-plan--line-range lines) (cons 1 most-positive-fixnum)))
         (mode (and (not (string-empty-p lang)) (ygg-markdown-fences-mode lang)))
         (body
          (cond
           ((not range) nil)
           ((not (file-in-directory-p file root)) nil)
           ((not (file-regular-p file)) nil)
           (t (condition-case nil
                  (with-temp-buffer
                    (insert-file-contents file nil 0 ygg-plan-source-bytes)
                    (goto-char (point-min))
                    (forward-line (1- (car range)))
                    (let ((beg (point)))
                      (forward-line (min (1+ (- (cdr range) (car range))) ygg-plan-source-lines))
                      (unless (= beg (point))
                        (buffer-substring-no-properties beg (point)))))
                (error nil))))))
    (if (not body)
        (propertize (format "  %s: nothing to show\n" src) 'face 'shadow)
      (let ((text (if (and mode (not (string-empty-p body)))
                      (copy-sequence (ygg-markdown-fences--fontify mode body))
                    body)))
        (add-face-text-property 0 (length text) 'ygg-markdown-fences-block t text)
        (if (string-suffix-p "\n" text) text (concat text "\n"))))))

(defun ygg-plan--overlay (beg end &rest props)
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'ygg-plan t)
    (while props (overlay-put ov (pop props) (pop props)))
    ov))

(defun ygg-plan--clear ()
  (remove-overlays (point-min) (point-max) 'ygg-plan t))

(defun ygg-plan--draw-decision (d)
  (let* ((key (plist-get d :key))
         (picked (ygg-plan--picked d))
         (suggested (plist-get d :pick)))
    (ygg-plan--overlay (plist-get d :beg) (plist-get d :beg)
                       'before-string
                       (propertize (if (member key ygg-plan--opened) "◆ " "◇ ")
                                   'face 'shadow))
    (dolist (o (plist-get d :options))
      (let* ((letter (plist-get o :letter))
             (tags (concat (when (equal letter picked)
                             (propertize "  ✓" 'face 'ygg-plan-picked))
                           (when (equal letter suggested)
                             (propertize "  suggested" 'face 'ygg-plan-suggested)))))
        (unless (string-empty-p tags)
          (ygg-plan--overlay (plist-get o :beg) (plist-get o :eol) 'after-string tags))))))

(defun ygg-plan--draw-claim (c)
  (let* ((id (ygg-plan--claim-id c))
         (struck (member id ygg-plan--struck))
         (notes (cdr (assoc id ygg-plan--comments)))
         (tail (concat (when struck (propertize "  ✗ struck" 'face 'ygg-plan-struck))
                       (mapconcat (lambda (n) (propertize (concat "\n    ▎ " (string-replace "\n" "\n      " n))
                                                          'face 'ygg-plan-note))
                                  notes ""))))
    (when (or struck notes)
      (apply #'ygg-plan--overlay (plist-get c :beg) (plist-get c :eol)
             'after-string tail
             (and struck '(face ygg-plan-struck))))))

(defun ygg-plan--draw-fence (f root)
  (when (plist-get f :empty)
    (let ((info (plist-get f :info)))
      (when (string-match "\\<src=\"\\([^\"]+\\)\"" info)
        (let ((src (match-string 1 info))
              (lines (and (string-match "\\<lines=\"\\([^\"]+\\)\"" info) (match-string 1 info)))
              (lang (if (string-match "\\`[^ \t=]+\\(?: \\|\\'\\)" info)
                        (string-trim (match-string 0 info))
                      "")))
          (ygg-plan--overlay (plist-get f :close) (plist-get f :close)
                             'before-string (ygg-plan--source root src lines lang)))))))

(defun ygg-plan--refresh ()
  "Draw the picks, strikes, comments and source lines over the text."
  (condition-case nil
      (save-excursion
        (save-restriction
          (widen)
          (when (ygg-plan--reconcile) (ygg-plan--save))
          (ygg-plan--clear)
          (let ((model (ygg-plan--model)) (root (ygg-plan--root)))
            (mapc #'ygg-plan--draw-decision (plist-get model :decisions))
            (mapc #'ygg-plan--draw-claim (plist-get model :claims))
            (dolist (f (plist-get model :fences)) (ygg-plan--draw-fence f root)))))
    (error nil)))

(defun ygg-plan--changed (&rest _)
  (when (timerp ygg-plan--timer) (cancel-timer ygg-plan--timer))
  (let ((buffer (current-buffer)))
    (setq ygg-plan--timer
          (run-with-idle-timer 0.3 nil
                               (lambda ()
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer (ygg-plan--refresh))))))))

(defun ygg-plan--track ()
  "Count the decision point has entered as opened."
  (when-let* ((d (ygg-plan--decision-at (point)))
              ((not (invisible-p (point))))
              ((not (member (plist-get d :key) ygg-plan--opened))))
    (push (plist-get d :key) ygg-plan--opened)
    (ygg-plan--changed-answers)))

(defun ygg-plan--pick (decision letter)
  "Pick LETTER for DECISION; picking it again takes the pick back."
  (let* ((key (plist-get decision :key))
         (option (or (ygg-plan--option decision letter)
                     (user-error "plan: no option %s here" letter)))
         (same (equal (ygg-plan--picked decision) letter)))
    (setf (alist-get key ygg-plan--picks nil t #'equal)
          (unless same (cons letter (ygg-plan--choice option))))
    (cl-pushnew key ygg-plan--opened :test #'equal)
    (ygg-plan--changed-answers)))

(defun ygg-plan-ret ()
  "Pick the option on this line, or the suggested one on the pick line."
  (interactive)
  (let* ((d (ygg-plan--decision-at (point)))
         (line (line-beginning-position))
         (option (and d (seq-find (lambda (o) (= (plist-get o :beg) line))
                                  (plist-get d :options)))))
    (cond (option (ygg-plan--pick d (plist-get option :letter)))
          ((and d (eql (plist-get d :pick-beg) line))
           (ygg-plan--pick d (plist-get d :pick)))
          (t (condition-case nil
                 (markdown-follow-thing-at-point nil)
               (error (forward-line 1)))))))

(defun ygg-plan-accept ()
  "Take the suggested option of the decision at point."
  (interactive)
  (let ((d (or (ygg-plan--decision-at (point)) (user-error "plan: no decision here"))))
    (unless (plist-get d :pick) (user-error "plan: this decision suggests nothing"))
    (unless (equal (ygg-plan--picked d) (plist-get d :pick))
      (ygg-plan--pick d (plist-get d :pick)))))

(defun ygg-plan-accept-all ()
  "Take the suggested option of every decision that has none picked."
  (interactive)
  (dolist (d (plist-get (ygg-plan--model) :decisions))
    (when-let* (((not (ygg-plan--picked d)))
                (option (ygg-plan--option d (plist-get d :pick))))
      (setf (alist-get (plist-get d :key) ygg-plan--picks nil nil #'equal)
            (cons (plist-get d :pick) (ygg-plan--choice option)))
      (cl-pushnew (plist-get d :key) ygg-plan--opened :test #'equal)))
  (ygg-plan--changed-answers))

(defun ygg-plan-clear-pick ()
  "Take back the pick of the decision at point."
  (interactive)
  (let ((d (or (ygg-plan--decision-at (point)) (user-error "plan: no decision here"))))
    (setf (alist-get (plist-get d :key) ygg-plan--picks nil t #'equal) nil)
    (ygg-plan--changed-answers)))

(defun ygg-plan--claim-here ()
  (or (ygg-plan--claim-at (point)) (user-error "plan: no claim here")))

(defun ygg-plan-strike ()
  "Strike the claim at point, or take the strike back."
  (interactive)
  (let ((id (ygg-plan--claim-id (ygg-plan--claim-here))))
    (setq ygg-plan--struck (if (member id ygg-plan--struck)
                               (delete id ygg-plan--struck)
                             (append ygg-plan--struck (list id))))
    (ygg-plan--changed-answers)))

(defun ygg-plan-comment ()
  "Comment on the claim at point."
  (interactive)
  (let* ((c (ygg-plan--claim-here))
         (text (string-trim (read-string (format "Comment on %s: "
                                                 (or (plist-get c :no) (plist-get c :text)))))))
    (unless (string-empty-p text)
      (setf (alist-get (ygg-plan--claim-id c) ygg-plan--comments nil nil #'equal)
            (append (cdr (assoc (ygg-plan--claim-id c) ygg-plan--comments)) (list text)))
      (ygg-plan--changed-answers))))

(defun ygg-plan-clear-comments ()
  "Drop the comments on the claim at point."
  (interactive)
  (setf (alist-get (ygg-plan--claim-id (ygg-plan--claim-here)) ygg-plan--comments nil t #'equal) nil)
  (ygg-plan--changed-answers))

(defun ygg-plan--label (no)
  (if no (format "[%s] " no) ""))

(defun ygg-plan--decision-lines (n d)
  (let* ((key (plist-get d :key))
         (picked (ygg-plan--picked d))
         (default (plist-get d :pick))
         (head (format "%d. %s%s" n (ygg-plan--label (plist-get d :claim)) (plist-get d :question))))
    (cond
     ((and picked (not (equal picked default)))
      (let ((o (ygg-plan--option d picked)))
        (format "%s\n   → %s) %s%s" head picked (ygg-plan--choice o)
                (if default (format " (was: %s)" default) ""))))
     ((or picked (member key ygg-plan--opened)) (concat head "  _(kept as proposed)_"))
     (t (concat head "  _(not opened; default kept)_")))))

(defun ygg-plan-response ()
  "The owner's answers as the markdown the agent is sent."
  (let* ((model (ygg-plan--model))
         (decisions (plist-get model :decisions))
         (struck (seq-filter (lambda (c) (member (ygg-plan--claim-id c) ygg-plan--struck))
                             (plist-get model :claims)))
         (noted (seq-filter (lambda (c) (cdr (assoc (ygg-plan--claim-id c) ygg-plan--comments)))
                            (plist-get model :claims)))
         (n 0))
    (concat
     (format "# Re: %s\n" (or (plist-get model :title) "plan"))
     (when decisions
       (concat "## Decisions\n"
               (mapconcat (lambda (d) (ygg-plan--decision-lines (cl-incf n) d)) decisions "\n")
               "\n"))
     (when struck
       (concat "## Struck\n"
               (mapconcat (lambda (c) (format "- %s%s" (ygg-plan--label (plist-get c :no))
                                              (plist-get c :text)))
                          struck "\n")
               "\n"))
     (when noted
       (concat "## Comments\n"
               (mapconcat
                (lambda (c)
                  (mapconcat (lambda (note)
                               (format "- %s%s\n  > %s" (ygg-plan--label (plist-get c :no))
                                       (plist-get c :text) (string-replace "\n" "\n  > " note)))
                             (cdr (assoc (ygg-plan--claim-id c) ygg-plan--comments)) "\n"))
                noted "\n")
               "\n")))))

(defun ygg-plan-copy ()
  "Copy the response instead of sending it."
  (interactive)
  (kill-new (ygg-plan-response))
  (message "plan: answers copied"))

(defun ygg-plan--session ()
  "The live agent working where this plan lives, else one the owner picks."
  (require 'aob)
  (let* ((live (seq-remove (lambda (s) (aob-session-ref s :parent-session)) (aob-live-sessions)))
         (root (ygg-plan--root))
         (here (and buffer-file-name
                    (seq-filter (lambda (s)
                                  (when-let* ((dir (or (aob-session-dir s) (aob-session-project s))))
                                    (and (file-in-directory-p dir root)
                                         (file-in-directory-p buffer-file-name dir))))
                                live))))
    (cond ((and here (null (cdr here))) (car here))
          (here (aob-read-session "Plan answers to: " here))
          (live (aob-read-session "Plan answers to: " live))
          (t (user-error "plan: no agent to send the answers to")))))

(defun ygg-plan-send ()
  "Send the response to the agent session."
  (interactive)
  (let ((s (ygg-plan--session))
        (text (ygg-plan-response))
        (aob-prompt-typed t))
    (aob-prompt s text)
    (message "plan: answers sent to %s" (aob-session-name s))))

(defun ygg-plan-fold ()
  "Fold to the level-1 claims."
  (interactive)
  (setq ygg-plan--depth 0)
  (outline-hide-sublevels 2))

(defun ygg-plan-cycle ()
  "Open the claims a level deeper, then everything, then fold again."
  (interactive)
  (setq ygg-plan--depth (mod (1+ ygg-plan--depth) (length ygg-plan--depths)))
  (let ((level (nth ygg-plan--depth ygg-plan--depths)))
    (if level (outline-hide-sublevels level) (outline-show-all))))

(defun ygg-plan--open-p ()
  "Whether every heading under the one at point shows its text."
  (save-excursion
    (let ((end (save-excursion (outline-end-of-subtree) (point)))
          (open t))
      (outline-back-to-heading t)
      (while (and open (< (point) end))
        (when (outline-invisible-p (line-end-position)) (setq open nil))
        (unless (outline-next-heading) (goto-char end)))
      open)))

(defun ygg-plan-tab ()
  "Draw the diagram at point, else open the claim a level at a time.
First its own exhibit and decision with its child claims, then the rest."
  (interactive)
  (if (ygg-diagram-fence-at-point)
      (ygg-diagram-toggle-at-point)
    (save-excursion
      (when (ignore-errors (outline-back-to-heading t))
        (cond ((outline-invisible-p (line-end-position))
               (outline-show-children)
               (outline-show-entry))
              ((ygg-plan--open-p) (outline-hide-subtree))
              (t (outline-show-subtree)))))))

;;;###autoload
(define-derived-mode ygg-plan-mode markdown-mode "Plan"
  "Markdown mode for a show-me plan: folded to its claims, answered in place."
  (outline-minor-mode 1)
  (ygg-plan--load)
  (add-hook 'post-command-hook #'ygg-plan--track nil t)
  (add-hook 'after-change-functions #'ygg-plan--changed nil t)
  (ygg-plan-fold)
  (ygg-plan--refresh))

(yggdrasil-define-mode-keys 'ygg-plan-mode 'normal
  "RET" #'ygg-plan-ret
  "<tab>" #'ygg-plan-tab
  "<backtab>" #'ygg-plan-cycle)

(yggdrasil-localleader-def 'ygg-plan-mode "s" #'ygg-plan-send "send answers to the agent")
(yggdrasil-localleader-def 'ygg-plan-mode "y" #'ygg-plan-copy "copy answers")
(yggdrasil-localleader-def 'ygg-plan-mode "x" #'ygg-plan-strike "strike claim")
(yggdrasil-localleader-def 'ygg-plan-mode "c" #'ygg-plan-comment "comment on claim")
(yggdrasil-localleader-def 'ygg-plan-mode "C" #'ygg-plan-clear-comments "drop comments")
(yggdrasil-localleader-def 'ygg-plan-mode "a" #'ygg-plan-accept "take suggestion")
(yggdrasil-localleader-def 'ygg-plan-mode "A" #'ygg-plan-accept-all "take all suggestions")
(yggdrasil-localleader-def 'ygg-plan-mode "u" #'ygg-plan-clear-pick "take pick back")
(yggdrasil-localleader-def 'ygg-plan-mode "m" #'ygg-diagram-toggle "diagrams & math")
(yggdrasil-localleader-def 'ygg-plan-mode "f" #'ygg-markdown-fences-toggle "raw code fences")

(defconst ygg-plan--auto-entry '("/\\.aob/plans/[^/]+\\.md\\'" . ygg-plan-mode))

(defun ygg-plan--first-in-auto-mode ()
  "Put the plan pattern ahead of markdown's own `.md' entry."
  (setq auto-mode-alist (cons ygg-plan--auto-entry (delete ygg-plan--auto-entry auto-mode-alist))))

(ygg-plan--first-in-auto-mode)

(provide 'ygg-plan)
;;; ygg-plan.el ends here
