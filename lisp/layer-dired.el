;;; layer-dired.el --- oil.nvim-style dired layer -*- lexical-binding: t; -*-

;; Built-ins wrapped: dired, dired-x (dired-jump), wdired.
;; Custom: SPC f j / "-" entry points, oil-style dired-mode-map bindings,
;; the wdired <-> yggdrasil-local-mode round trip.

;;; Code:

(require 'yggdrasil-leader)

(defvar dired-mode-map)
(defvar dired-kill-when-opening-new-dired-buffer)
(defvar dired-auto-revert-buffer)
(defvar dired-dwim-target)
(declare-function dired-next-line "dired")
(declare-function dired-previous-line "dired")
(declare-function dired-up-directory "dired")
(declare-function dired-find-file "dired")
(declare-function dired-toggle-read-only "dired")
(declare-function dired-find-file-other-window "dired")
(declare-function dired-isearch-filenames "dired-aux")
(declare-function dired-get-filename "dired")
(declare-function dired-move-to-filename "dired")
(declare-function wdired-change-to-dired-mode "wdired")
(declare-function dired-single-buffer "dired-single")
(declare-function dired-single-up-directory "dired-single")
(declare-function dired-get-file-for-visit "dired")

;;; Oil-nvim feel: single-buffer navigation, live directories, group dirs first

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls))
  (setq dired-listing-switches
        (if gls "-alh --group-directories-first" "-alh")))

(setq dired-kill-when-opening-new-dired-buffer t
      dired-auto-revert-buffer t
      dired-dwim-target t)
(put 'dired-find-alternate-file 'disabled nil)

;; dired-mode isn't derived-mode-p special-mode here, so it'd otherwise go modal.
(add-to-list 'ygg-deny-modes 'dired-mode)

;;; Entry points

(declare-function ygg--jump-push "yggdrasil-motions")
(declare-function ygg-jump-back "yggdrasil-motions")

(defun ygg-dired-jump ()
  "Open dired on this file, leaving a jump so C-o comes back."
  (interactive)
  (ygg--jump-push)
  (call-interactively #'dired-jump))

(declare-function better-jumper-get-jumps "better-jumper")
(declare-function better-jumper-jump-list-struct-idx "better-jumper")

(defun ygg-dired-jump-back ()
  "Jump back (vim C-o); from a fresh listing, to the jump that opened it."
  (interactive)
  ;; a listing is never pushed, so from the head of the list step 0, not 1
  (ygg-jump-back
   (if (and (fboundp 'better-jumper-get-jumps)
            (eql (better-jumper-jump-list-struct-idx (better-jumper-get-jumps)) -1))
       0 1)))

(yggdrasil-define-keys 'ygg-leader-file-map
  "j" #'ygg-dired-jump :label "jump to dired")

(yggdrasil-define-keys 'normal
  "-" #'ygg-dired-jump :label "dired")

(defvar ygg-leader-open-map (make-sparse-keymap) "The o prefix: open external things.")

(defun ygg-finder--reveal (path)
  (unless (eq system-type 'darwin) (user-error "Finder is macOS only"))
  (call-process "open" nil 0 nil "-R" (expand-file-name path)))

(defun ygg-finder-reveal ()
  "Reveal the current file, or the dired directory, in Finder."
  (interactive)
  (ygg-finder--reveal
   (or (if (derived-mode-p 'dired-mode)
           (or (dired-get-filename nil t) default-directory)
         buffer-file-name)
       (user-error "No file to reveal"))))

(defun ygg-finder-open-project ()
  "Open the project root in Finder."
  (interactive)
  (unless (eq system-type 'darwin) (user-error "Finder is macOS only"))
  (call-process "open" nil 0 nil
                (expand-file-name (if-let* ((pr (project-current)))
                                      (project-root pr)
                                    default-directory))))

(yggdrasil-define-keys 'ygg-leader-open-map
  "o" #'ygg-finder-reveal :label "reveal in Finder"
  "O" #'ygg-finder-open-project :label "project root in Finder")

;;; Modal keys, bound straight into dired-mode-map (oil.nvim muscle memory)

;;; / search over filenames with vim n/N repeat (oil is just a vim buffer)

(defvar-local ygg-dired--search nil)

(defun ygg-dired--isearch-done ()
  (when (and (derived-mode-p 'dired-mode)
             (not isearch-mode-end-hook-quit)
             (> (length isearch-string) 0))
    (setq ygg-dired--search isearch-string)))

(add-hook 'isearch-mode-end-hook #'ygg-dired--isearch-done)

(defun ygg-dired--repeat (dir)
  (unless ygg-dired--search (user-error "No previous search"))
  (let ((start (point))
        (case-fold-search t)
        (total (count-lines (point-min) (point-max)))
        (found nil)
        (steps 0))
    (while (and (not found) (< steps total))
      (setq steps (1+ steps))
      (unless (zerop (forward-line dir))
        (goto-char (if (> dir 0) (point-min) (point-max))))
      (when (and (> dir 0) (eobp)) (goto-char (point-min)))
      (let ((name (ignore-errors (dired-get-filename 'no-dir t))))
        (when (and name (string-match-p (regexp-quote ygg-dired--search) name))
          (setq found t))))
    (if found
        (dired-move-to-filename)
      (goto-char start)
      (message "no match: %s" ygg-dired--search))))

(defun ygg-dired-search-next ()
  "Jump to the next filename matching the last / search."
  (interactive)
  (ygg-dired--repeat 1))

(defun ygg-dired-search-prev ()
  "Jump to the previous filename matching the last / search."
  (interactive)
  (ygg-dired--repeat -1))

(defun ygg-dired-first-file ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1)
  (unless (dired-get-filename nil t) (dired-next-line 1)))

(defun ygg-dired-last-file ()
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(defun ygg-dired-open-external ()
  "Open the file at point with the system handler (oil gx)."
  (interactive)
  (if (fboundp 'dired-do-open)
      (dired-do-open)
    (start-process "open" nil "open" (dired-get-filename))))

(defun ygg-dired-copy-path ()
  "Copy the absolute path of the file at point (oil-style yy)."
  (interactive)
  (dired-copy-filename-as-kill 0))

(defun ygg-dired-home ()
  (interactive)
  (dired "~"))

(defun ygg-dired-cd-here ()
  "Set Emacs's working directory to this listing's directory (oil `)."
  (interactive)
  (cd default-directory)
  (message "cwd: %s" (abbreviate-file-name default-directory)))

(defun ygg-dired-find-file-split ()
  "Open the file at point in a horizontal split below (oil <C-h>-ish)."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (select-window (split-window-below))
    (find-file file)))

(defun ygg-dired-find-file-vsplit ()
  "Open the file at point in a vertical split to the right (oil <C-s>-ish)."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (select-window (split-window-right))
    (find-file file)))

(defvar ygg-dired-goto-map (make-sparse-keymap))

(with-eval-after-load 'dired
  (require 'dired-x nil t)
  (setq dired-omit-files "\\`\\.")
  (define-key dired-mode-map "j" #'dired-next-line)
  (define-key dired-mode-map "k" #'dired-previous-line)
  (define-key dired-mode-map "h" #'dired-up-directory)
  (define-key dired-mode-map "l" #'dired-find-file)
  (define-key dired-mode-map "-" #'dired-up-directory)
  (define-key dired-mode-map "i" #'dired-toggle-read-only)
  (define-key dired-mode-map "o" #'dired-find-file-other-window)
  (define-key dired-mode-map "q" #'quit-window)
  (define-key dired-mode-map "/" #'dired-isearch-filenames)
  (define-key dired-mode-map "n" #'ygg-dired-search-next)
  (define-key dired-mode-map "N" #'ygg-dired-search-prev)
  (define-key dired-mode-map "a" #'dired-create-empty-file)
  (define-key dired-mode-map "A" #'dired-create-directory)
  (define-key dired-mode-map "~" #'ygg-dired-home)
  (define-key dired-mode-map "`" #'ygg-dired-cd-here)
  (define-key dired-mode-map "s" #'ygg-dired-find-file-split)
  (define-key dired-mode-map "v" #'ygg-dired-find-file-vsplit)
  ;; G was chgrp, g was revert; vim muscle memory wins — see g r
  (define-key dired-mode-map "G" #'ygg-dired-last-file)
  (define-key dired-mode-map "g" ygg-dired-goto-map)
  (define-key ygg-dired-goto-map "g" #'ygg-dired-first-file)
  (define-key ygg-dired-goto-map "r" #'revert-buffer)
  (define-key ygg-dired-goto-map "?" #'describe-mode)
  (define-key ygg-dired-goto-map "." (if (fboundp 'dired-omit-mode)
                                         #'dired-omit-mode
                                       #'dired-hide-details-mode))
  (define-key ygg-dired-goto-map "x" #'ygg-dired-open-external)
  (define-key ygg-dired-goto-map "s" #'dired-sort-toggle-or-edit)
  (define-key ygg-dired-goto-map "p" #'dired-display-file)
  (define-key dired-mode-map (kbd "C-o") #'ygg-dired-jump-back)
  (define-key dired-mode-map "y" (let ((m (make-sparse-keymap)))
                                   (define-key m "y" #'ygg-dired-copy-path)
                                   m))
  ;; dd deletes now (oil/vim); dired's flag-then-x still works via m + x
  (define-key dired-mode-map "d" (let ((m (make-sparse-keymap)))
                                   (define-key m "d" #'dired-do-delete)
                                   m))
  (define-key dired-mode-map (kbd "C-d") #'ygg-scroll-half-down)
  (define-key dired-mode-map (kbd "C-u") #'ygg-scroll-half-up)
  (define-key dired-mode-map (kbd "C-f") #'ygg-scroll-page-down)
  (define-key dired-mode-map (kbd "C-b") #'ygg-scroll-page-up)
  (define-key dired-mode-map (kbd "C-e") #'scroll-up-line)
  (define-key dired-mode-map (kbd "C-y") #'scroll-down-line)
  (define-key dired-mode-map "z" (let ((m (make-sparse-keymap)))
                                   (define-key m "z" #'recenter)
                                   (define-key m "t" (lambda () (interactive) (recenter 0)))
                                   (define-key m "b" (lambda () (interactive) (recenter -1)))
                                   m)))
;; %/m/u/d/x/C/R/D untouched; dired's n/p line motion is covered by j/k.

;; wdired's exit path sets major-mode by hand, skipping run-mode-hooks, so the
;; globalized mode never refires there — drive the round trip explicitly.
(defun ygg-dired--wdired-edit ()
  "Edit the names at once: the i that opened wdired was the insert."
  (yggdrasil-local-mode 1)
  (ygg-insert-state))

(with-eval-after-load 'wdired
  (add-hook 'wdired-mode-hook #'ygg-dired--wdired-edit)
  (advice-add 'wdired-change-to-dired-mode :after
              (lambda (&rest _) (yggdrasil-local-mode -1))))

;; dired-single: reuse one dired buffer for l/h/- navigation.  Not on MELPA
;; anymore (elpaca can't resolve a recipe), so pin the emacsmirror archive.
(when (fboundp 'elpaca)
  (elpaca (dired-single :host github :repo "emacsattic/dired-single")
    (with-eval-after-load 'dired
      (define-key dired-mode-map "l" #'dired-single-buffer)
      (define-key dired-mode-map "h" #'dired-single-up-directory)
      (define-key dired-mode-map "-" #'dired-single-up-directory)
      (define-key dired-mode-map [remap dired-find-file] #'dired-single-buffer)
      (define-key dired-mode-map [remap dired-up-directory] #'dired-single-up-directory))))

(provide 'layer-dired)
;;; layer-dired.el ends here
