;;; yggdrasil-quickscope.el --- quickscope-style f/t target hints -*- lexical-binding: t; -*-

;; Built-ins wrapped: overlays, run-with-idle-timer, post-command-hook,
;; window-buffer-change-functions, window-selection-change-functions,
;; define-minor-mode, display-graphic-p, noninteractive.
;; Custom: per-line unique-char scan that marks the best f/t/F/T target in
;; each word around point; primary (one keystroke) vs. secondary (one
;; repeat) highlight levels.

;;; Code:

(require 'yggdrasil-core)

(defgroup ygg-quickscope nil
  "Quickscope-style f/t/F/T target highlighting."
  :group 'yggdrasil
  :prefix "ygg-quickscope-")

(defcustom ygg-quickscope-idle-delay 0.15
  "Idle seconds before (re)painting quickscope target highlights."
  :type 'number :group 'ygg-quickscope)

(defface ygg-quickscope-primary '((t :inherit isearch))
  "Face for an f/t target reachable in a single keystroke."
  :group 'ygg-quickscope)

(defface ygg-quickscope-secondary '((t :inherit lazy-highlight))
  "Face for an f/t target that needs one repeated keystroke."
  :group 'ygg-quickscope)

(defvar-local ygg-quickscope--overlays nil
  "Live overlays painted on the current line.")
(defvar ygg-quickscope--timer nil)

;;; Pure scanner (exposed for tests)

(defsubst ygg-quickscope--word-char-p (c)
  (or (and (>= c ?a) (<= c ?z))
      (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9))
      (= c ?_)))

(defun ygg-quickscope--pick-target (line start end freq)
  "First char in LINE[START,END) unique per FREQ, else first needing one repeat."
  (let (secondary)
    (catch 'done
      (let ((i start))
        (while (< i end)
          (let ((n (aref freq (aref line i))))
            (cond ((= n 1) (throw 'done (cons i 1)))
                  ((and (= n 2) (not secondary)) (setq secondary i))))
          (setq i (1+ i))))
      (and secondary (cons secondary 2)))))

(defun ygg-quickscope--targets (line cursor-col)
  "f/t targets right of CURSOR-COL in LINE as ((COL . LEVEL) ...).
LEVEL 1 marks a char unique from CURSOR-COL to the end of LINE (one
keystroke); LEVEL 2 marks one that needs a single repeat.  The word
touching CURSOR-COL is never a target."
  (let* ((len (length line))
         (start (max 0 (min cursor-col len)))
         (freq (make-vector 128 0)))
    (let ((i start))
      (while (< i len)
        (let ((c (aref line i)))
          (when (< c 128) (aset freq c (1+ (aref freq c)))))
        (setq i (1+ i))))
    (let (result (in-word nil) (word-start start) (i start))
      (while (<= i len)
        (let ((wc (and (< i len) (ygg-quickscope--word-char-p (aref line i)))))
          (cond
           ((and wc (not in-word)) (setq in-word t word-start i))
           ((and (not wc) in-word)
            (setq in-word nil)
            (when (> word-start start)
              (let ((target (ygg-quickscope--pick-target line word-start i freq)))
                (when target (push target result))))))
          (setq i (1+ i))))
      (nreverse result))))

;;; Buffer-side line scan (forward for f/t, backward for F/T) and painting

(defun ygg-quickscope--clear ()
  (mapc #'delete-overlay ygg-quickscope--overlays)
  (setq ygg-quickscope--overlays nil))

(defun ygg-quickscope--forward-targets (bol)
  ;; cap the scan at the visible window width — off-screen targets aren't
  ;; reachable by eye, and this bounds work on very long/minified lines
  (let* ((end (min (line-end-position) (+ (point) (window-body-width))))
         (line (buffer-substring-no-properties bol end))
         (col (- (point) bol)))
    (mapcar (lambda (tg) (cons (+ bol (car tg)) (cdr tg)))
            (ygg-quickscope--targets line col))))

(defun ygg-quickscope--backward-targets (bol)
  "Reuse the forward scanner on the reversed pre-point text for F/T."
  (let* ((start (max bol (- (point) (window-body-width))))
         (before (buffer-substring-no-properties start (point)))
         (len (length before)))
    (mapcar (lambda (tg) (cons (+ start (- len 1 (car tg))) (cdr tg)))
            (ygg-quickscope--targets (reverse before) 0))))

(defun ygg-quickscope--paint ()
  (setq ygg-quickscope--timer nil)
  (when (and yggdrasil-local-mode (ygg-normal-p) (not (minibufferp)))
    (ygg-quickscope--clear)
    (let ((bol (line-beginning-position)))
      (dolist (tg (nconc (ygg-quickscope--forward-targets bol)
                         (ygg-quickscope--backward-targets bol)))
        (let ((ov (make-overlay (car tg) (1+ (car tg)))))
          (overlay-put ov 'face (if (= (cdr tg) 1)
                                     'ygg-quickscope-primary
                                   'ygg-quickscope-secondary))
          (push ov ygg-quickscope--overlays))))))

;;; Idle recompute wiring

(defun ygg-quickscope--clear-visible (&rest _)
  "Clear stale overlays left behind in any window whose buffer changed."
  (dolist (win (window-list nil 'no-minibuf))
    (with-current-buffer (window-buffer win)
      (ygg-quickscope--clear))))

(defun ygg-quickscope--post-command ()
  (ygg-quickscope--clear)
  (when ygg-quickscope--timer (cancel-timer ygg-quickscope--timer) (setq ygg-quickscope--timer nil))
  (when (and yggdrasil-local-mode (ygg-normal-p) (not (minibufferp)))
    (setq ygg-quickscope--timer
          (run-with-idle-timer ygg-quickscope-idle-delay nil #'ygg-quickscope--paint))))

;;; Global minor mode

(defun ygg-quickscope--enable ()
  (add-hook 'post-command-hook #'ygg-quickscope--post-command)
  (add-hook 'window-buffer-change-functions #'ygg-quickscope--clear-visible)
  (add-hook 'window-selection-change-functions #'ygg-quickscope--clear-visible)
  (add-hook 'ygg-insert-entry-hook #'ygg-quickscope--clear)
  (add-hook 'ygg-visual-entry-hook #'ygg-quickscope--clear))

(defun ygg-quickscope--disable ()
  (remove-hook 'post-command-hook #'ygg-quickscope--post-command)
  (remove-hook 'window-buffer-change-functions #'ygg-quickscope--clear-visible)
  (remove-hook 'window-selection-change-functions #'ygg-quickscope--clear-visible)
  (remove-hook 'ygg-insert-entry-hook #'ygg-quickscope--clear)
  (remove-hook 'ygg-visual-entry-hook #'ygg-quickscope--clear)
  (when ygg-quickscope--timer (cancel-timer ygg-quickscope--timer) (setq ygg-quickscope--timer nil))
  (dolist (buf (buffer-list))
    (with-current-buffer buf (ygg-quickscope--clear))))

;;;###autoload
(define-minor-mode ygg-quickscope-mode
  "Highlight quickscope-style f/t/F/T targets on the current line.
Highlights appear only while a buffer is in Yggdrasil normal state, and
this mode is always a no-op under `noninteractive' (batch)."
  :init-value nil :global t
  (cond
   (noninteractive (setq ygg-quickscope-mode nil))
   (ygg-quickscope-mode (ygg-quickscope--enable))
   (t (ygg-quickscope--disable))))

;; GUI-only default: batch and terminal Emacs never pay for the timer/overlays.
(unless noninteractive
  (when (display-graphic-p)
    (ygg-quickscope-mode 1)))

(provide 'yggdrasil-quickscope)
;;; yggdrasil-quickscope.el ends here
