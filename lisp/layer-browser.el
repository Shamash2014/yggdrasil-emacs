;;; layer-browser.el --- in-buffer web browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Two in-buffer browser engines under the `o b' prefix:
;; - WebKit (xwidget, falling back to eww) — native, instant; `w'/`W'.
;; - embr — Chromium via CDP screencast (Playwright backend), for full
;;   web-dev / DevTools work; `e' (and `i' incognito).  Needs a one-time
;;   `M-x embr-install-or-update-chromium'.
;; Home for the eventual aob session preview (`\ w' -> dev-server URL).

;;; Code:

(require 'yggdrasil-core)
(require 'yggdrasil-localleader)
(require 'cl-lib)
(defvar ygg-leader-open-map)

(declare-function xwidget-webkit-browse-url "xwidget")

(defun ygg-browser-open (url &optional new)
  "Open URL in an in-buffer browser: WebKit xwidget when available, else eww.
With NEW non-nil (interactively, a prefix arg), spin up a fresh session
instead of reusing the current one, so several browsers can run at once."
  (interactive
   (list (read-string "Browse URL: " (or (thing-at-point 'url t) "https://"))
         current-prefix-arg))
  (if (featurep 'xwidget-internal)
      (xwidget-webkit-browse-url url new)
    (eww url new)))

(defun ygg-browser-open-new (url)
  "Spin up a fresh in-buffer browser at URL, never reusing an open session."
  (interactive
   (list (read-string "Browse URL (new): " (or (thing-at-point 'url t) "https://"))))
  (ygg-browser-open url t))

;;; embr — Chromium rendered in a buffer via CDP screencast.  A second
;;; engine alongside WebKit, kept opt-in: the WebKit keys never depend on it.

(declare-function embr-browse "embr")
(declare-function embr-browse-incognito "embr")
(declare-function embr--send "embr")
(declare-function embr--action-callback "embr")
(declare-function embr-paste "embr")
(declare-function ygg-space--current-id "yggdrasil-spacetree")
(defvar ygg-jk-forward-function)
(defvar ygg-paste-function)
(defvar embr-browser-engine)
(defvar embr-display-method)
(defvar embr-color-scheme)
(defvar embr-viewport-sizing)
(defvar embr-tab-bar)
(defvar embr-home-url)
(defvar embr-session-restore)
(defvar embr--normal-buffer)

(when (fboundp 'elpaca)
  (elpaca (embr :host github :repo "emacs-os/embr.el"
                :files ("*.el" "*.py" "*.sh" "native/*.c" "native/Makefile"))
    ;; chromium + headless is the macOS-viable path — CloakBrowser has no mac
    ;; build, and the xvfb/headed path is Linux-only
    (setq embr-browser-engine 'chromium
          embr-display-method 'headless
          embr-color-scheme 'dark
          embr-viewport-sizing 'dynamic
          embr-tab-bar t
          embr-home-url "about:blank"
          ;; 60fps (default) pins chromium+ffmpeg+Emacs on the screencast; 10 is
          ;; smooth enough for reading/forms and cuts encode/decode load ~6x
          embr-fps 10
          ;; per-space browsers would share one global session.json, so keep
          ;; tab save/restore off — else a new space resurrects another's tabs
          embr-session-restore nil)))

;; embr runs the yggdrasil modal layer like every other buffer (NOT its own
;; embr-vimium, NOT ygg-deny-modes): normal keeps ygg motions/leader, and
;; insert (`i') falls through to `embr-self-insert' so typing reaches the page;
;; mouse click/scroll fall through in either state.  Every browser verb hangs
;; off the local leader so normal-mode keys stay pure yggdrasil.
(dolist (b '(("o" embr-navigate    "open url")
             ("l" embr-follow-hint "follow link")
             ("b" embr-back        "back")
             ("f" embr-forward     "forward")
             ("r" embr-refresh     "reload")
             ("y" embr-copy-url    "copy url")
             ("t" embr-new-tab     "new tab")
             ("x" embr-close-tab   "close tab")
             ("n" embr-next-tab    "next tab")
             ("p" embr-prev-tab    "prev tab")
             ("e" embr-open-in-eww "open in eww")
             ("d" embr-download    "download")
             ("v" embr-view-source "view source")
             ("R" embr-reader      "reader")
             ("s" embr-screenshot  "screenshot")
             ("." embr-dispatch    "menu…")
             ("q" embr-quit        "quit")))
  (yggdrasil-localleader-def 'embr-mode (nth 0 b) (nth 1 b) (nth 2 b)))

;; embr's buffer is read-only, so the generic jk-escape's `insert' would fail;
;; forward the j to the page instead and let k still escape to normal.
(defun ygg-embr--forward-key (char)
  "Send CHAR to the embr page for a read-only-safe jk-escape."
  (ignore-errors
    (embr--send `((cmd . "type") (text . ,(char-to-string char)))
                #'embr--action-callback)))

(defun ygg-embr--enable-modal-io ()
  "Route read-only-unsafe modal actions to the embr page: jk-escape and paste."
  (setq-local ygg-jk-forward-function #'ygg-embr--forward-key
              ygg-paste-function #'embr-paste))

(add-hook 'embr-mode-hook #'ygg-embr--enable-modal-io)

;; A buried embr buffer keeps decoding screencast frames at `embr-fps' forever
;; (a real battery drain); stop its render/hover timers when it has no window
;; and restart them when shown.  The daemon keeps screencasting — this reclaims
;; the Emacs-side CPU only.
(declare-function embr--render-start "embr")
(declare-function embr--render-stop "embr")
(declare-function embr--hover-start "embr")
(declare-function embr--hover-stop "embr")
(defvar embr--render-timer)

(defun ygg-embr--suspend-buried (&rest _)
  (dolist (b (buffer-list))
    (when (provided-mode-derived-p (buffer-local-value 'major-mode b) 'embr-mode)
      (with-current-buffer b
        (if (get-buffer-window b t)
            (unless embr--render-timer (embr--render-start) (embr--hover-start))
          (when embr--render-timer (embr--render-stop) (embr--hover-stop)))))))

(add-hook 'window-buffer-change-functions #'ygg-embr--suspend-buried)

;; embr uses `switch-to-buffer' (whole window, ignores display rules), so
;; redirect that one call to a right-hand split for a side-by-side browser
(defun ygg-embr--side-by-side (launch)
  "Call LAUNCH with embr's buffer forced into a right split, not the whole window."
  (cl-letf (((symbol-function 'switch-to-buffer)
             (lambda (buffer &rest _)
               (pop-to-buffer buffer
                              '((display-buffer-reuse-window display-buffer-in-direction)
                                (direction . right) (window-width . 0.5))))))
    (funcall launch)))

(defvar ygg-embr--space-buffers (make-hash-table :test 'eql)
  "Space id -> that space's embr browser buffer, for per-space isolation.")

(defun ygg-browser-embr ()
  "Open embr side by side, isolated per space: each space keeps its own browser.
Reuses the current space's live browser, or spins up a fresh one and claims it."
  (interactive)
  (let ((id (and (fboundp 'ygg-space--current-id) (ygg-space--current-id))))
    (if (not id)
        (ygg-embr--side-by-side #'embr-browse)
      (let ((buf (gethash id ygg-embr--space-buffers)))
        (setq embr--normal-buffer (and (buffer-live-p buf) buf))
        (ygg-embr--side-by-side #'embr-browse)
        (puthash id embr--normal-buffer ygg-embr--space-buffers)))))

(defun ygg-browser-embr-incognito ()
  "Open an incognito embr side by side with the current buffer."
  (interactive)
  (ygg-embr--side-by-side #'embr-browse-incognito))

(defvar ygg-browser-map (make-sparse-keymap) "The o b prefix: browsers.")

(yggdrasil-define-keys 'ygg-browser-map
  "w" #'ygg-browser-open :label "webkit"
  "W" #'ygg-browser-open-new :label "webkit (new)"
  "e" #'ygg-browser-embr :label "embr (side by side)"
  "i" #'ygg-browser-embr-incognito :label "embr incognito")

(yggdrasil-define-keys 'ygg-leader-open-map
  "b" ygg-browser-map :label "browser")

(provide 'layer-browser)
;;; layer-browser.el ends here
