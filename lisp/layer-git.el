;;; layer-git.el --- magit + diff-hl git layer -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)
(require 'ygg-ui)
(require 'yggdrasil-core)
(require 'yggdrasil-leader)

(declare-function ygg-scroll-half-down "yggdrasil-motions")
(declare-function ygg-scroll-half-up "yggdrasil-motions")
(declare-function ygg-scroll-page-down "yggdrasil-motions")
(declare-function ygg-scroll-page-up "yggdrasil-motions")
(declare-function ygg--bracketed-goto "yggdrasil-motions")
(declare-function global-diff-hl-mode "diff-hl")
(declare-function diff-hl-flydiff-mode "diff-hl-flydiff")
(declare-function diff-hl-magit-post-refresh "diff-hl")
(declare-function diff-hl-dired-mode "diff-hl-dired")
(declare-function diff-hl-next-hunk "diff-hl")
(declare-function diff-hl-previous-hunk "diff-hl")
(declare-function magit-section-forward "magit-section")
(declare-function magit-section-backward "magit-section")
(declare-function magit-section-forward-sibling "magit-section")
(declare-function magit-section-backward-sibling "magit-section")
(declare-function magit-refresh "magit-mode")
(declare-function magit-delete-thing "magit-mode")
(declare-function magit-status-jump "magit-status")
(declare-function magit-log-move-to-revision "magit-log")
(declare-function magit-jump-to-diffstat-or-diff "magit-diff")
(declare-function magit-revision-jump "magit-diff")
(declare-function with-editor-finish "with-editor")
(declare-function with-editor-cancel "with-editor")
(declare-function magit-status "magit-status")
(declare-function magit-blame-addition "magit-blame")
(declare-function magit-log-current "magit-log")
(declare-function magit-log-buffer-file "magit-log")
(declare-function magit-diff-buffer-file "magit-diff")
(declare-function magit-diff-unstaged "magit-diff")
(declare-function magit-commit "magit-commit")
(declare-function magit-push "magit-push")
(declare-function magit-pull "magit-pull")
(declare-function magit-branch "magit-branch")
(declare-function magit-file-relative-name "magit-git")
(declare-function magit-stage-files "magit-apply")
(declare-function magit-get-current-branch "magit-git")
(declare-function forge-dispatch "forge-commands")
(declare-function forge-list-pullreqs "forge-topics")
(declare-function forge-list-issues "forge-topics")
(declare-function lab-list-project-merge-requests "lab")
(declare-function lab-act-on-last-project-pipeline "lab")
(declare-function lab-approve-merge-request "lab")
(autoload 'lab-approve-merge-request "lab" nil t)
(autoload 'lab-act-on-last-project-pipeline "lab" nil t)

(defvar magit-section-mode-map)
(defvar magit-mode-map)
(defvar magit-status-mode-map)
(defvar magit-log-mode-map)
(defvar magit-diff-mode-map)
(defvar magit-revision-mode-map)
(defvar with-editor-mode-map)
(defvar forge-topics-mode-map)
(defvar forge-topic-mode-map)
(defvar forge-post-mode-map)
(defvar lab-merge-request-diff-prefix-map)

(defvar magit-status-sections-hook)
(defvar magit-status-headers-hook)
(declare-function magit-auto-revert-repository-buffer-p "magit-autorevert")

(declare-function magit-add-section-hook "magit-section")
(declare-function magit-insert-worktrees "magit-worktree")

;; magit-insert-worktrees inserts nothing when the repository has one worktree
(with-eval-after-load 'magit-status
  (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-worktrees
                          #'magit-insert-status-headers t))

(declare-function magit-blame-next-chunk "magit-blame")
(declare-function magit-blame-previous-chunk "magit-blame")
(declare-function magit-blame-cycle-style "magit-blame")
(declare-function magit-blame-copy-hash "magit-blame")
(declare-function magit-blame-quit "magit-blame")

(when (fboundp 'elpaca)
  ;; magit needs transient >= 0.13; Emacs 30 ships 0.7 and elpaca skips
  ;; built-in deps unless ordered explicitly
  (elpaca transient
    ;; q (not only C-g) quits magit's transient dialogs — the old Magit-Popup
    ;; behavior; conflicting q suffixes shift to Q (transient's own rebind)
    (with-eval-after-load 'transient (transient-bind-q-to-quit)))
  (elpaca magit
    ;; --- perf ---
    ;; don't re-render the status buffer after every command elsewhere
    (setq magit-refresh-status-buffer nil
          ;; word-diff refinement only for the hunk at point, never all
          magit-diff-refine-hunk t
          ;; skip the whitespace/indent repaint passes on every diff
          magit-diff-paint-whitespace nil
          magit-diff-highlight-indentation nil
          magit-diff-highlight-trailing nil
          ;; committing shouldn't also compute and show the full diff
          magit-commit-show-diff nil)
    (with-eval-after-load 'magit-status
      ;; the push-remote up/down sections fire extra git calls; the
      ;; upstream ones already answer "am I ahead/behind"
      (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-pushremote)
      (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-pushremote)
      ;; tags header scans all tags on every refresh
      (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header))
    ;; The repo-only filter (magit-auto-revert-repository-buffer-p) ran
    ;; magit-toplevel — a `git' subprocess — per buffer on every 5s poll:
    ;; ~25ms blocking + a git storm across 50+ buffers. Default nil reverts
    ;; on mtime alone (measured 0.8ms, no git); revert still skips modified
    ;; buffers, so nothing is lost.
    (setq auto-revert-buffer-list-filter nil)
    ;; macOS: file-notify uses kqueue, which watches a file by enumerating +
    ;; stat-ing its whole CONTAINING directory on every revert tick — bursty
    ;; CPU in big project roots. Poll the file's own mtime instead (O(1)).
    (setq auto-revert-use-notify nil))
  (elpaca forge
    (setq forge-database-file (locate-user-emacs-file "var/forge-database.sqlite"))
    (with-eval-after-load 'magit (require 'forge)))
  (elpaca (lab :host github :repo "isamert/lab.el")
    (with-eval-after-load 'lab
      (setq lab-host (or (getenv "LAB_HOST") ygg-lab-host)))))

;;; Huge changes: the status buffer washes every hunk it inserts — ~1s per
;;; 1000 changed lines, paid again on every refresh.  Past a limit, insert
;;; the diff only when the section is opened.

(declare-function magit-git-string "magit-git")
(declare-function magit-bare-repo-p "magit-git")
(declare-function magit--insert-diff "magit-diff")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-stage-modified "magit-apply")
(declare-function magit-unstage-all "magit-apply")
(declare-function magit-discard "magit-apply")
(declare-function magit-section-show "magit-section")
(declare-function magit-current-section "magit-section")

(defvar magit-buffer-diff-args)
(defvar magit-buffer-diff-files)
(defvar magit-unstaged-section-map)
(defvar magit-staged-section-map)
(defvar ygg-magit-large-unstaged-section-map)
(defvar ygg-magit-large-staged-section-map)

(defcustom ygg-magit-diff-line-limit 2000
  "Changed lines above which the status buffer defers inserting a diff."
  :type 'natnum :group 'yggdrasil)

(defcustom ygg-lab-host "https://gitlab.com"
  "GitLab host URL for lab.el integration. Can be overridden by LAB_HOST env var."
  :type 'string :group 'yggdrasil)

(defun ygg-magit-discard-deferred ()
  "Insert the deferred diff, then discard it."
  (interactive)
  (magit-section-show (magit-current-section))
  (call-interactively #'magit-discard))

(with-eval-after-load 'magit-diff
  ;; a collapsed section has no children to apply, so the verbs that mean
  ;; "all of it" have to reach git by name instead — except discarding,
  ;; which needs the diff it is throwing away
  (defvar-keymap ygg-magit-large-unstaged-section-map
    :parent magit-unstaged-section-map
    "<remap> <magit-stage-files>" #'magit-stage-modified
    "<remap> <magit-delete-thing>" #'ygg-magit-discard-deferred)
  (defvar-keymap ygg-magit-large-staged-section-map
    :parent magit-staged-section-map
    "<remap> <magit-unstage-files>" #'magit-unstage-all
    "<remap> <magit-delete-thing>" #'ygg-magit-discard-deferred))

(defun ygg-magit--diff-size (&rest args)
  "Return (FILES . LINES) changed by `git diff ARGS\='."
  (if-let* ((stat (magit-git-string "diff" "--shortstat" args))
            (n (mapcar #'string-to-number (split-string stat "[^0-9]+" t))))
      (cons (car n) (apply #'+ (cdr n)))
    (cons 0 0)))

(defun ygg-magit--diff-large-p (&rest args)
  "Return the size of the diff selected by ARGS when it is over the limit."
  (let ((size (apply #'ygg-magit--diff-size
                     (append args (list magit-buffer-diff-args "--"
                                        magit-buffer-diff-files)))))
    (and (> (cdr size) ygg-magit-diff-line-limit) size)))

(defun ygg-magit-insert-unstaged-changes ()
  "Insert the unstaged section, deferring a diff that is too large to wash."
  (let ((large (ygg-magit--diff-large-p)))
    (magit-insert-section section (unstaged nil large)
      (when large
        (oset section keymap 'ygg-magit-large-unstaged-section-map))
      (if large
          (magit-insert-heading
            (format "Unstaged changes (%d files, %d lines)" (car large) (cdr large)))
        (magit-insert-heading t "Unstaged changes"))
      (magit-insert-section-body
        (magit--insert-diff nil
          "diff" magit-buffer-diff-args "--no-prefix"
          "--" magit-buffer-diff-files)))))

(defun ygg-magit-insert-staged-changes ()
  "Insert the staged section, deferring a diff that is too large to wash."
  (unless (magit-bare-repo-p)
    (let ((large (ygg-magit--diff-large-p "--cached")))
      (magit-insert-section section (staged nil large)
        (when large
          (oset section keymap 'ygg-magit-large-staged-section-map))
        (if large
            (magit-insert-heading
              (format "Staged changes (%d files, %d lines)" (car large) (cdr large)))
          (magit-insert-heading t "Staged changes"))
        (magit-insert-section-body
          (magit--insert-diff nil
            "diff" "--cached" magit-buffer-diff-args "--no-prefix"
            "--" magit-buffer-diff-files))))))

(with-eval-after-load 'magit-diff
  (advice-add 'magit-insert-unstaged-changes :override
              #'ygg-magit-insert-unstaged-changes)
  (advice-add 'magit-insert-staged-changes :override
              #'ygg-magit-insert-staged-changes))

;;; Difftastic — structural (AST-aware) diffs inside magit

(declare-function difftastic-magit-diff "difftastic")
(declare-function difftastic-magit-show "difftastic")

(defun ygg-git-diff ()
  "Diff via difftastic (dwim); falls back to magit when difft is absent."
  (interactive)
  (if (and (executable-find "difft") (fboundp 'difftastic-magit-diff))
      (call-interactively #'difftastic-magit-diff)
    (call-interactively #'magit-diff-working-tree)))

(when (and (fboundp 'elpaca) (executable-find "difft"))
  (elpaca difftastic
    (with-eval-after-load 'magit-diff
      (transient-append-suffix 'magit-diff '(-1 -1)
        [("D" "difftastic diff (dwim)" difftastic-magit-diff)
         ("S" "difftastic show" difftastic-magit-show)]))))

(add-to-list 'ygg-modal-special-modes 'difftastic-mode)
(add-to-list 'ygg-modal-special-mode-keep
             '(difftastic-mode ("<tab>" . "TAB") ("] c" . "n") ("[ c" . "p")
                               ("] f" . "N") ("[ f" . "P")))

(add-to-list 'ygg-modal-special-modes 'forge-topics-mode)
(add-to-list 'ygg-modal-special-modes 'forge-topic-mode)
(add-to-list 'ygg-modal-special-modes 'forge-repository-list-mode)

(when (fboundp 'elpaca)
  (elpaca diff-hl
    (global-diff-hl-mode 1)
    (diff-hl-flydiff-mode 1)
    ;; diff-hl >= 1.11 folded the pre-refresh half into post-refresh
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

;;; Inline blame (Zed) — current line's commit, dimmed after eol.
;; Perf contract: work only after idle; one async git per uncached line;
;; per-line cache dropped O(1) on edit; post-command pays one bol compare.

(defcustom ygg-blame-idle-delay 0.6
  "Idle seconds before the current line's blame appears."
  :type 'number :group 'yggdrasil)

(defvar ygg-blame--timer nil)
(defvar ygg-blame--proc nil)
(defvar ygg-blame--overlay nil)
(defvar ygg-blame--at nil "(BUFFER . BOL) the overlay belongs to.")
(defvar-local ygg-blame--line-cache nil "line -> string | `none'.")
(defvar-local ygg-blame--off nil "Non-nil: file unblameable until next save.")
(defvar-local ygg-blame--wired nil)

(defun ygg-blame--clear ()
  (when ygg-blame--overlay
    (delete-overlay ygg-blame--overlay)
    (setq ygg-blame--overlay nil ygg-blame--at nil)))

(defun ygg-blame--on-move ()
  (when (and ygg-blame--overlay
             (or (and (fboundp 'ygg-insert-p) (ygg-insert-p))
                 (not (and (eq (car ygg-blame--at) (current-buffer))
                           (eq (cdr ygg-blame--at) (line-beginning-position))))))
    (ygg-blame--clear)))

(defun ygg-blame--reset ()
  (setq ygg-blame--line-cache nil ygg-blame--off nil))

(defun ygg-blame--wire ()
  (unless ygg-blame--wired
    (setq ygg-blame--wired t)
    (add-hook 'after-change-functions
              (lambda (&rest _) (setq ygg-blame--line-cache nil)) nil t)
    (add-hook 'after-save-hook #'ygg-blame--reset nil t)))

(defun ygg-blame--age (epoch)
  (let ((d (max 60 (- (float-time) epoch))))
    (cond ((< d 3600) (format "%dm" (/ d 60)))
          ((< d 86400) (format "%dh" (/ d 3600)))
          ((< d 2592000) (format "%dd" (/ d 86400)))
          ((< d 31536000) (format "%dmo" (/ d 2592000)))
          (t (format "%dy" (/ d 31536000))))))

(defun ygg-blame--parse (txt)
  (when (string-match "\\`\\([0-9a-f]\\{40\\}\\)" txt)
    (if (string-match-p "\\`0\\{40\\}" (match-string 1 txt))
        "uncommitted"
      (let ((author (and (string-match "^author \\(.*\\)$" txt)
                         (match-string 1 txt)))
            (time (and (string-match "^author-time \\([0-9]+\\)$" txt)
                       (string-to-number (match-string 1 txt))))
            (summary (and (string-match "^summary \\(.*\\)$" txt)
                          (match-string 1 txt))))
        (format "%s · %s · %s" (or author "?")
                (if time (ygg-blame--age time) "?") (or summary ""))))))

(defun ygg-blame--display (buf info)
  (when (eq buf (window-buffer (selected-window)))
    (with-current-buffer buf
      (ygg-blame--clear)
      (let ((ov (make-overlay (line-end-position) (line-end-position))))
        (overlay-put ov 'after-string
                     (propertize (concat "   " info) 'face 'shadow))
        (setq ygg-blame--overlay ov
              ygg-blame--at (cons buf (line-beginning-position)))))))

(defun ygg-blame--request (buf line)
  "Ask git for LINE of BUF's file.  A request still pending is dropped
without a verdict: a cancelled ask says nothing about the file."
  (when (process-live-p ygg-blame--proc) (delete-process ygg-blame--proc))
  (let ((default-directory (file-name-directory buffer-file-name))
        (file (file-name-nondirectory buffer-file-name)))
    (setq ygg-blame--proc
          (make-process
           :name "ygg-blame" :noquery t
           :buffer (generate-new-buffer " *ygg-blame*")
           :command (list "git" "blame" "-L" (format "%d,%d" line line)
                          "--porcelain" "--" file)
           :sentinel
           (lambda (p _e)
             (when (eq (process-status p) 'signal)
               (kill-buffer (process-buffer p)))
             (when (eq (process-status p) 'exit)
               (let ((ok (zerop (process-exit-status p)))
                     (txt (with-current-buffer (process-buffer p) (buffer-string))))
                 (kill-buffer (process-buffer p))
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let ((info (and ok (ygg-blame--parse txt))))
                       (unless ygg-blame--line-cache
                         (setq ygg-blame--line-cache (make-hash-table :test 'eql)))
                       (puthash line (or info 'none) ygg-blame--line-cache)
                       (unless ok (setq ygg-blame--off t))
                       (when (and info (= line (line-number-at-pos nil t)))
                         (ygg-blame--display buf info))))))))))))

(defun ygg-blame--show ()
  (when (and buffer-file-name
             (file-exists-p buffer-file-name)
             (not ygg-blame--off)
             (not ygg-blame--overlay)
             (not (buffer-modified-p))
             ;; only in normal state — never annotate while editing
             (not (and (fboundp 'ygg-insert-p) (ygg-insert-p)))
             (not (file-remote-p buffer-file-name)))
    (ygg-blame--wire)
    (let* ((line (line-number-at-pos nil t))
           (cached (and ygg-blame--line-cache
                        (gethash line ygg-blame--line-cache))))
      (cond ((eq cached 'none))
            (cached (ygg-blame--display (current-buffer) cached))
            (t (ygg-blame--request (current-buffer) line))))))

(define-minor-mode ygg-inline-blame-mode
  "Zed-style inline blame on the current line after a pause."
  :global t :group 'yggdrasil
  (if ygg-inline-blame-mode
      (progn
        (setq ygg-blame--timer
              (run-with-idle-timer ygg-blame-idle-delay t #'ygg-blame--show))
        (add-hook 'post-command-hook #'ygg-blame--on-move))
    (when ygg-blame--timer (cancel-timer ygg-blame--timer))
    (setq ygg-blame--timer nil)
    (remove-hook 'post-command-hook #'ygg-blame--on-move)
    (when (process-live-p ygg-blame--proc) (delete-process ygg-blame--proc))
    (ygg-blame--clear)))

(unless noninteractive (ygg-inline-blame-mode 1))

(with-eval-after-load 'layer-ui
  (defvar ygg-leader-ui-map)
  (yggdrasil-define-keys 'ygg-leader-ui-map
    "b" #'ygg-inline-blame-mode :label "inline blame"))

;;; Vim nav inside magit section buffers.
;; Yggdrasil states stay off there (magit derives from special-mode), so
;; keys go straight into magit's maps.  Displacements: g -> g r (refresh),
;; k -> K (delete thing), K's magit-file-untrack stays in magit-file-dispatch,
;; per-mode j jump commands -> J.

(defvar ygg-magit-goto-map (make-sparse-keymap)
  "The g prefix inside magit section buffers.")
(define-key ygg-magit-goto-map (kbd "j") (cons "next sibling" #'magit-section-forward-sibling))
(define-key ygg-magit-goto-map (kbd "k") (cons "prev sibling" #'magit-section-backward-sibling))
(define-key ygg-magit-goto-map (kbd "g") (cons "buffer start" #'beginning-of-buffer))
(define-key ygg-magit-goto-map (kbd "e") (cons "buffer end" #'end-of-buffer))
(declare-function which-key-show-keymap "which-key" (keymap &optional no-paging))
(declare-function magit-refresh-all "magit-mode")
;; without this the let below binds lexically and which-key never sees it
(defvar which-key-max-display-columns)

(defun ygg-magit-keys ()
  "Show every magit key through which-key.
`?\' is magit's transient dispatch, which lists what magit can do; this
lists what the keys do, which is the other question.
Not `which-key-show-major-mode\': that reads `magit-status-mode-map\',
which holds almost nothing — magit keeps its bindings in the parent map,
so it rendered two lines for a mode with a hundred keys."
  (interactive)
  ;; the leader panel is one column on purpose; forty-seven magit keys are not
  (let ((which-key-max-display-columns nil))
    ;; no-paging: the paging state waits on a key, which is fine from the
    ;; keyboard and hangs anything that calls this without one
    (which-key-show-keymap 'magit-mode-map t)))

(define-key ygg-magit-goto-map (kbd "r") (cons "refresh" #'magit-refresh))
(define-key ygg-magit-goto-map (kbd "?") (cons "this mode's keys" #'ygg-magit-keys))
;; G is buffer end everywhere else, so refresh-all moves in beside refresh
(define-key ygg-magit-goto-map (kbd "R") (cons "refresh all" #'magit-refresh-all))

(with-eval-after-load 'magit-section
  (define-key magit-section-mode-map (kbd "j") #'magit-section-forward)
  (define-key magit-section-mode-map (kbd "k") #'magit-section-backward)
  (define-key magit-section-mode-map (kbd "g") ygg-magit-goto-map)
  (define-key magit-section-mode-map (kbd "C-d") #'ygg-scroll-half-down)
  (define-key magit-section-mode-map (kbd "C-u") #'ygg-scroll-half-up)
  (define-key magit-section-mode-map (kbd "C-f") #'ygg-scroll-page-down)
  (define-key magit-section-mode-map (kbd "C-b") #'ygg-scroll-page-up)
  (define-key magit-section-mode-map (kbd "C-e") #'scroll-up-line)
  (define-key magit-section-mode-map (kbd "C-y") #'scroll-down-line)
  (define-key magit-section-mode-map (kbd "G") #'end-of-buffer))

(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "j") #'magit-section-forward)
  (define-key magit-mode-map (kbd "k") #'magit-section-backward)
  (define-key magit-mode-map (kbd "K") #'magit-delete-thing)
  ;; P = pull, p = push (k already gives the section-backward that p was)
  (define-key magit-mode-map (kbd "P") #'magit-pull)
  (define-key magit-mode-map (kbd "p") #'magit-push)
  (define-key magit-mode-map (kbd "g") ygg-magit-goto-map)
  ;; magit-mode-map is the child of magit-section-mode-map, so magit's own
  ;; G shadowed the buffer-end this config puts there
  (define-key magit-mode-map (kbd "G") #'end-of-buffer)
  (define-key magit-mode-map (kbd "C-w") ygg-window-map))

(declare-function magit-copy-section-value "magit-mode")
(define-key ygg-magit-goto-map (kbd "y") (cons "copy section value" #'magit-copy-section-value))

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "j") #'magit-section-forward)
  (define-key magit-status-mode-map (kbd "J") #'magit-status-jump))

(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd "j") #'magit-section-forward)
  (define-key magit-log-mode-map (kbd "J") #'magit-log-move-to-revision))

(with-eval-after-load 'magit-diff
  (define-key magit-diff-mode-map (kbd "j") #'magit-section-forward)
  (define-key magit-diff-mode-map (kbd "J") #'magit-jump-to-diffstat-or-diff)
  (define-key magit-revision-mode-map (kbd "j") #'magit-section-forward)
  (define-key magit-revision-mode-map (kbd "J") #'magit-revision-jump))

;;; git blame: navigable session — j/k walk chunks, b cycles the overlay
;;; style (margin ↔ end-of-line heading), y copies the commit hash, q quits.
;;; SPC g a starts it (magit-blame-addition, already bound below).

(declare-function magit-show-commit "magit-diff")

(defvar ygg-magit-blame-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'magit-blame-next-chunk)
    (define-key map (kbd "k") #'magit-blame-previous-chunk)
    (define-key map (kbd "b") #'magit-blame-cycle-style)
    (define-key map (kbd "y") #'magit-blame-copy-hash)
    (define-key map (kbd "q") #'magit-blame-quit)
    (define-key map (kbd "RET") #'magit-show-commit)
    map)
  "Blame keys lifted over normal state; blame is a minor mode in a modal buffer.")

(yggdrasil-define-mode-keys 'magit-blame-mode 'normal ygg-magit-blame-map)

;; ZZ finishes the commit, ZQ cancels it; the minor-mode map remaps the
;; Z-map commands without touching the shared text-mode map.
(with-eval-after-load 'with-editor
  (define-key with-editor-mode-map [remap ygg-save-and-kill-buffer] #'with-editor-finish)
  (define-key with-editor-mode-map [remap ygg-kill-buffer-no-save] #'with-editor-cancel))

;;; Leader: SPC g git submap

(defun ygg-git-stage-file ()
  "Stage the file the current buffer visits."
  (interactive)
  (require 'magit)
  (if-let* ((file (magit-file-relative-name)))
      (magit-stage-files (list file))
    (user-error "Buffer visits no file")))

(defun ygg-git-yank-branch ()
  "Copy the current branch name to the kill ring."
  (interactive)
  (require 'magit)
  (if-let* ((branch (magit-get-current-branch)))
      (progn (kill-new branch) (message "%s" branch))
    (user-error "No current branch")))

(declare-function magit-list-worktrees "magit-git")
(declare-function magit-rev-abbrev "magit-git")

(defun ygg-git--worktree-choices ()
  "Other worktrees of this repository as (LABEL . PATH).
LABEL is the worktree's name, branch and path."
  (let* ((here (magit-toplevel))
         (rows (seq-keep
                (pcase-lambda (`(,path ,commit ,branch ,bare ,_detached ,_locked ,prunable))
                  (let ((path (file-name-as-directory path)))
                    (unless (or bare prunable (and here (file-equal-p path here)))
                      (list (file-name-nondirectory (directory-file-name path))
                            (or branch (magit-rev-abbrev commit))
                            path))))
                (magit-list-worktrees)))
         (name-width (apply #'max 0 (mapcar (lambda (r) (string-width (nth 0 r))) rows)))
         (branch-width (apply #'max 0 (mapcar (lambda (r) (string-width (nth 1 r))) rows))))
    (mapcar (pcase-lambda (`(,name ,branch ,path))
              (cons (format "%s  %s  %s"
                            (string-pad name name-width)
                            (propertize (string-pad branch branch-width) 'face 'shadow)
                            (propertize (abbreviate-file-name path) 'face 'shadow))
                    path))
            rows)))

(defun ygg-git-worktree-status ()
  "Open magit status in another worktree of this repository."
  (interactive)
  (require 'magit)
  (let ((choices (or (ygg-git--worktree-choices)
                     (user-error "No other worktree"))))
    (magit-status (cdr (assoc (completing-read "Worktree: " choices nil t) choices)))))

;;; File reference helpers: copy path:line or code with reference

(defun ygg--file-reference ()
  "Return a `path:line' reference to the selected lines, relative to project root."
  (unless (or buffer-file-name default-directory)
    (user-error "Buffer is not visiting a file"))
  (let* ((file (file-truename (or buffer-file-name default-directory)))
         (proj (or (when-let* ((p (project-current))) (project-root p))
                   default-directory))
         (proj-root (file-truename proj))
         (path (file-relative-name file proj-root)))
    (if (not (use-region-p))
        (format "%s:%d" path (line-number-at-pos nil t))
      (let ((first-line (line-number-at-pos (region-beginning) t))
            (last-line (save-excursion
                         (goto-char (region-end))
                         (if (bolp)
                             (1- (line-number-at-pos nil t))
                           (line-number-at-pos nil t)))))
        (if (<= last-line first-line)
            (format "%s:%d" path first-line)
          (format "%s:%d-%d" path first-line last-line))))))

(defun ygg-copy-file-reference ()
  "Copy a `path:line' reference to the kill ring; include range if region selected."
  (interactive)
  (let ((reference (ygg--file-reference)))
    (kill-new reference)
    (message "%s" reference)))

(defun ygg-copy-code-with-reference ()
  "Copy the selected code as a markdown block under a `path:line' reference."
  (interactive)
  (let* ((reference (ygg--file-reference))
         (language (let ((mode (symbol-name major-mode)))
                     (string-remove-suffix "-mode"
                       (string-remove-suffix "-ts" mode))))
         (code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))
         (trimmed (string-trim-right code)))
    (kill-new (format "%s\n```%s\n%s\n```" reference language trimmed))
    (message "%s" reference)))

;;; Conflicts (git-conflict.nvim feel on built-in smerge)

(autoload 'smerge-next "smerge-mode" nil t)
(autoload 'smerge-prev "smerge-mode" nil t)
(autoload 'smerge-keep-upper "smerge-mode" nil t)
(autoload 'smerge-keep-lower "smerge-mode" nil t)
(autoload 'smerge-keep-all "smerge-mode" nil t)
(autoload 'smerge-keep-current "smerge-mode" nil t)

(defun ygg-git--maybe-smerge ()
  (save-excursion
    (goto-char (point-min))
    ;; conflict markers live near the top; cap the scan so opening a huge
    ;; file doesn't pay a whole-buffer regexp sweep
    (when (re-search-forward "^<<<<<<< " (min (point-max) (+ (point-min) 65536)) t)
      (smerge-mode 1))))

(add-hook 'find-file-hook #'ygg-git--maybe-smerge)

(defun ygg-git--find-next-hunk ()
  (save-excursion
    (let ((start (point)))
      (when (fboundp 'diff-hl-next-hunk)
        (ignore-errors (diff-hl-next-hunk))
        (when (/= (point) start)
          (point))))))

(defun ygg-git--find-prev-hunk ()
  (save-excursion
    (let ((start (point)))
      (when (fboundp 'diff-hl-previous-hunk)
        (ignore-errors (diff-hl-previous-hunk))
        (when (/= (point) start)
          (point))))))

(defun ygg-next-hunk-change ()
  (interactive)
  (ygg--bracketed-goto #'ygg-git--find-next-hunk))

(defun ygg-prev-hunk-change ()
  (interactive)
  (ygg--bracketed-goto #'ygg-git--find-prev-hunk))

(defun ygg-goto-first-git-hunk ()
  "Go to first diff-hl hunk in buffer."
  (interactive)
  (ygg--record-bracket-motion -1 "G")
  (let ((pos (save-excursion (goto-char (point-min))
                              (ygg-git--find-next-hunk))))
    (if pos (ygg--bracketed-goto (lambda () pos))
      (message "no hunks"))))

(defun ygg-goto-last-git-hunk ()
  "Go to last diff-hl hunk in buffer."
  (interactive)
  (ygg--record-bracket-motion 1 "G")
  (let ((pos (save-excursion (goto-char (point-max))
                              (ygg-git--find-prev-hunk))))
    (if pos (ygg--bracketed-goto (lambda () pos))
      (message "no hunks"))))

(yggdrasil-define-keys 'normal
  "] x" #'smerge-next :label "next conflict"
  "[ x" #'smerge-prev :label "prev conflict"
  "] g" #'ygg-next-hunk-change :label "next change"
  "[ g" #'ygg-prev-hunk-change :label "prev change"
  "] G" #'ygg-goto-last-git-hunk :label "last hunk"
  "[ G" #'ygg-goto-first-git-hunk :label "first hunk")

(defvar ygg-git-conflict-map (make-sparse-keymap) "The g x prefix: merge conflicts.")

(yggdrasil-define-keys 'ygg-git-conflict-map
  "o" #'smerge-keep-upper :label "keep ours"
  "t" #'smerge-keep-lower :label "keep theirs"
  "b" #'smerge-keep-all :label "keep both"
  "c" #'smerge-keep-current :label "keep at point"
  "n" #'smerge-next :label "next conflict"
  "p" #'smerge-prev :label "prev conflict")

;;; Leader: SPC g git submap (keys mirror the user's nvim neogit layout:
;;; gb branches, gs status, gd working-tree diff, gH file history)

(declare-function magit-checkout "magit")
(declare-function magit-diff-working-tree "magit-diff")
(declare-function magit-log-all "magit-log")
(declare-function magit-worktree "magit-worktree")

(defvar ygg-leader-git-map (make-sparse-keymap) "The g prefix: git.")

(yggdrasil-define-keys 'ygg-leader-git-map
  "g" #'magit-status :label "status"
  "b" #'magit-checkout :label "branches"
  "d" #'ygg-git-diff :label "diff (difftastic)"
  "D" #'magit-diff-buffer-file :label "diff file"
  "H" #'magit-log-buffer-file :label "file history"
  "l" #'magit-log-current :label "log"
  "L" #'magit-log-all :label "log all"
  "a" #'magit-blame-addition :label "annotate (blame)"
  "S" #'ygg-git-stage-file :label "stage file"
  "c" #'magit-commit :label "commit"
  "p" #'magit-push :label "push"
  "f" #'magit-pull :label "pull/fetch"
  "B" #'magit-branch :label "branch menu"
  "W" #'ygg-git-worktree-status :label "worktree status"
  "x" ygg-git-conflict-map :label "conflicts"
  "y" #'ygg-git-yank-branch :label "yank branch"
  "F" #'forge-dispatch :label "forge menu"
  "I" #'forge-list-issues :label "forge issues"
  "P" #'forge-list-pullreqs :label "forge PRs"
  "M" #'lab-list-project-merge-requests :label "lab MRs"
  "R" #'lab-act-on-last-project-pipeline :label "lab last pipeline"
  "A" #'lab-approve-merge-request :label "lab approve MR")

(yggdrasil-leader-def "g" ygg-leader-git-map "git")

;;; Worktrunk (worktrunk.dev `wt') — worktrees as easy as branches, in magit

(declare-function ygg-notify "layer-ui")
(declare-function magit-toplevel "magit-git")
(declare-function magit-status "magit-status")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function transient-get-suffix "transient" (prefix loc))
(declare-function transient-append-suffix "transient")

(defun ygg-wt--root ()
  (or (and (fboundp 'magit-toplevel) (magit-toplevel))
      (when-let* ((p (project-current))) (project-root p))
      default-directory))

(defun ygg-wt--worktrees ()
  "Parse `wt list --format json' into a list of alists."
  (let ((default-directory (ygg-wt--root)))
    (with-temp-buffer
      (if (zerop (call-process "wt" nil t nil "list" "--format" "json"))
          (progn (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist :array-type 'list
                                    :null-object nil))
        (user-error "wt list failed: %s" (string-trim (buffer-string)))))))

(defun ygg-wt--pick (prompt)
  "Pick a worktree via `completing-read'; return its alist, or the typed string."
  (let* ((wts (ygg-wt--worktrees))
         (cands (mapcar (lambda (w) (cons (alist-get 'branch w) w)) wts))
         (annotate
          (lambda (cand)
            (when-let* ((w (cdr (assoc cand cands)))
                        (m (alist-get 'main w)))
              (format "  %s  ↑%s ↓%s  %s"
                      (or (alist-get 'symbols w) "")
                      (alist-get 'ahead m 0) (alist-get 'behind m 0)
                      (or (alist-get 'message (alist-get 'commit w)) "")))))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function . ,annotate)
                                 (display-sort-function . identity))
                    (complete-with-action action cands str pred)))))
    (let ((choice (completing-read prompt table nil nil)))
      (or (cdr (assoc choice cands)) choice))))

(defun ygg-wt--open (path)
  "Open worktree PATH in magit (falls back to dired)."
  (if (fboundp 'magit-status) (magit-status path) (dired path)))

(define-derived-mode ygg-wt-output-mode special-mode "worktrunk"
  "Read-only output of a worktrunk command.")

(add-to-list 'ygg-modal-special-modes 'ygg-wt-output-mode)

(defun ygg-wt--run (args &optional on-success)
  "Run `wt ARGS' async in the repo (-y skips prompts); ON-SUCCESS on exit 0."
  (require 'ansi-color)
  (let* ((root (ygg-wt--root))
         (buf (get-buffer-create "*worktrunk*")))
    (with-current-buffer buf
      (setq default-directory root)
      (let ((inhibit-read-only t)) (erase-buffer)))
    (set-process-sentinel
     (let ((default-directory root))
       (apply #'start-process "worktrunk" buf "wt" "-y" args))
     (lambda (p _e)
       (when (eq (process-status p) 'exit)
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))
           (ygg-wt-output-mode))
         (if (zerop (process-exit-status p))
             (progn (when on-success (funcall on-success))
                    (ygg-notify (format "worktrunk %s ✓" (car args))))
           (ygg-ui-show buf t)
           (ygg-notify (format "worktrunk %s failed" (car args)) 'error)))))))

(defun ygg-wt-switch ()
  "Switch to a worktree, creating its branch if new; open it in magit."
  (interactive)
  (let ((pick (ygg-wt--pick "Worktree branch: ")))
    (if (consp pick)
        (ygg-wt--open (alist-get 'path pick))
      (ygg-wt--run
       (list "switch" "--create" pick)
       (lambda ()
         (when-let* ((w (seq-find (lambda (x) (equal (alist-get 'branch x) pick))
                                  (ygg-wt--worktrees))))
           (ygg-wt--open (alist-get 'path w))))))))

(defun ygg-wt-list ()
  "Show `wt list' (worktrees + status) in a buffer."
  (interactive)
  (require 'ansi-color)
  (let ((root (ygg-wt--root))
        (buf (get-buffer-create "*worktrunk*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory root)
        (call-process "wt" nil t nil "list")
        (ansi-color-apply-on-region (point-min) (point-max)))
      (goto-char (point-min))
      (ygg-wt-output-mode))
    (ygg-ui-show buf)))

(defun ygg-wt-merge (&optional target)
  "Merge the current worktree's branch into TARGET (squash, rebase, ff, remove).
With a prefix arg, prompt for the target branch."
  (interactive (list (when current-prefix-arg
                       (read-string "Merge into target branch: "))))
  (when (yes-or-no-p (format "wt merge into %s, then remove this worktree? "
                             (or target "the default branch")))
    (ygg-wt--run (append '("merge") (when target (list target))))))

(defun ygg-wt-remove ()
  "Remove a worktree (default current); delete its branch if merged."
  (interactive)
  (let* ((pick (ygg-wt--pick "Remove worktree: "))
         (branch (if (consp pick) (alist-get 'branch pick) pick)))
    (when (yes-or-no-p (format "wt remove worktree %s? " branch))
      (ygg-wt--run (list "remove" branch)))))

(defvar ygg-leader-worktree-map (make-sparse-keymap)
  "The g w prefix: worktrees via worktrunk.")

(if (not (executable-find "wt"))
    (yggdrasil-define-keys 'ygg-leader-git-map
      "w" #'magit-worktree :label "worktrees")
  (yggdrasil-define-keys 'ygg-leader-worktree-map
    "w" #'ygg-wt-switch :label "switch/create (wt)"
    "l" #'ygg-wt-list :label "list (wt)"
    "m" #'ygg-wt-merge :label "merge + remove (wt)"
    "d" #'ygg-wt-remove :label "remove (wt)"
    "W" #'magit-worktree :label "magit worktree menu")
  (yggdrasil-define-keys 'ygg-leader-git-map
    "w" ygg-leader-worktree-map :label "worktrees (wt)")
  (with-eval-after-load 'magit-worktree
    ;; by key, not by (GROUP . INDEX): the numeric location reports success
    ;; and inserts nothing, so the entry was missing from magit's worktree
    ;; menu for as long as it has been written this way
    (ignore-errors
      (unless (ignore-errors (transient-get-suffix 'magit-worktree "W"))
        (transient-append-suffix 'magit-worktree "b"
          '("W" "worktrunk switch" ygg-wt-switch))))))

;;; Git menu = magit's own dispatch dialog (SPC g ?), closable with q

(declare-function magit-dispatch "magit")

(yggdrasil-define-keys 'ygg-leader-git-map
  "?" #'magit-dispatch :label "dispatch (magit menu)")

(provide 'layer-git)
;;; layer-git.el ends here
