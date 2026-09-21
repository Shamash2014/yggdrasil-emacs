;;; ygg-tile.el --- windows as tiles -*- lexical-binding: t; -*-

;;; Commentary:
;; The frame is a tiling layout.  A split goes along the longer side of
;; the window being split, the way i3's automatic layout picks its
;; orientation, and the tiles of the main area share it evenly whenever
;; one appears or goes.  Side windows, the tree and its kind, keep the
;; width they were given: room a closed window gives back goes to the
;; tiles, never across to another side window.  Floats and the drawers
;; are not tiles, and a child frame is laid out by whoever raised it.
;; Nothing opens a second frame on its own.

;;; Code:

(defcustom ygg-tile-aspect 1.6
  "Width to height, in pixels, above which a window splits to the right.
Below it the window splits below.  Text cells are wider than tall, so
one is not the balance point: a square-looking window is already wider
in pixels than it is high."
  :type 'number :group 'yggdrasil)

(defun ygg-tile--laid-out-frame-p (frame)
  "Whether FRAME is a frame the tiling lays out.
A child frame is a float, the kind posframe puts up for a preview, a
picker or the colon line's shell: the code that raised it gave it its
place and its size, so nothing here counts its window or splits it."
  (and (frame-live-p frame) (null (frame-parent frame))))

(defcustom ygg-tile-fixed-modes '(aob-compose-mode)
  "Modes whose windows keep the height they were given.
A prompt box popped at the bottom is not a tile: balancing it up to half
the frame is the tiling fighting the owner.  A window showing one of
these takes a fixed height and is left out of every count and balance."
  :type '(repeat symbol) :group 'yggdrasil)

(defun ygg-tile--fixed-p (window)
  "Whether WINDOW shows a buffer of a mode in ygg-tile-fixed-modes."
  (with-current-buffer (window-buffer window)
    (apply #'derived-mode-p ygg-tile-fixed-modes)))

(defun ygg-tile-repair-sides (&optional frame)
  "Clear the side parameter off a window that is no proper side window.
A side window carries both a side and a slot; one left with a side and
no slot, a main window that was once a preview or was reused, breaks
every later side window on that side with a complaint about parents.
Runs on every layout change of FRAME and touches nothing else."
  (dolist (window (window-list frame 'no-minibuf))
    (when (and (window-live-p window)
               (window-parameter window 'window-side)
               (null (window-parameter window 'window-slot)))
      (set-window-parameter window 'window-side nil))))

(defun ygg-tile-pin-fixed (&optional frame)
  "Give every fixed-mode window of FRAME its fixed height.
On window-buffer-change-functions, so a prompt box is pinned the moment
it is shown and before any balance sees it."
  (dolist (window (window-list frame 'no-minibuf))
    (when (and ygg-tile-fixed-modes (ygg-tile--fixed-p window))
      (with-current-buffer (window-buffer window)
        (setq-local window-size-fixed 'height))
      (window-preserve-size window nil t))))

(defun ygg-tile--tile-p (window)
  "Whether WINDOW is one of the tiles of the main area.
The minibuffer is not one, a side window is not one, and neither is a
window the other-window walk skips: a drawer or a float stands over the
tiles rather than among them, and its size is its owner's to choose."
  (and (ygg-tile--laid-out-frame-p (window-frame window))
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (window-parameter window 'no-other-window))
       (not (ygg-tile--fixed-p window))))

(defvar ygg-tile--counts (make-hash-table :test #'equal)
  "How many main-area windows each space had when it was last seen.
Keyed by frame and space, since every space keeps its own layout and a
switch between them is not a split.")

(defun ygg-tile--key (frame)
  "The key FRAME's current space counts under."
  (cons frame (and (fboundp 'ygg-space--current-id) (ygg-space--current-id))))

(defun ygg-tile--wide-p (window)
  "Whether WINDOW is wide enough to split to the right."
  (> (window-pixel-width window)
     (* ygg-tile-aspect (window-pixel-height window))))

(defun ygg-tile-split (&optional window)
  "Split WINDOW along its longer side; the split function tiling prefers.
Returns the new window, or nil when WINDOW cannot be split either way."
  (let ((window (or window (selected-window))))
    (cond
     ((not (ygg-tile--tile-p window)) nil)
     ((ygg-tile--wide-p window)
      (with-selected-window window
        (or (ignore-errors (split-window-right))
            (ignore-errors (split-window-below)))))
     (t
      (with-selected-window window
        (or (ignore-errors (split-window-below))
            (ignore-errors (split-window-right))))))))

(defun ygg-tile--main-windows (&optional frame)
  "The live windows of FRAME's main area, side windows left out."
  (seq-filter #'ygg-tile--tile-p (window-list frame 'no-minibuf)))

(defun ygg-tile-balance (&optional frame)
  "Share FRAME's main area evenly among its tiles when their number changed.
A space seen for the first time is left as it is: the change that
balances is a split or a close inside the space, never a switch to it.
A child frame is not laid out here at all, so a float going up or down
is neither counted nor balanced.  Balancing the frame root would also
equalise the side windows, so only the main window's tree is balanced."
  (let ((frame (or frame (selected-frame))))
    (when (ygg-tile--laid-out-frame-p frame)
      (let* ((key (ygg-tile--key frame))
             (previous (gethash key ygg-tile--counts))
             (count (length (ygg-tile--main-windows frame))))
        (unless (eql count previous)
          (puthash key count ygg-tile--counts)
          (when (and previous (> count 1))
            (with-demoted-errors "ygg-tile: %S"
              (balance-windows (window-main-window frame)))))))))

(defvar ygg-tile--saved nil
  "The settings the mode replaced, put back when it is turned off.")

(define-minor-mode ygg-tile-mode
  "Split along the longer side, keep tiles even, never pop a frame."
  :global t :group 'yggdrasil
  (if ygg-tile-mode
      (progn
        (setq ygg-tile--saved
              (list split-window-preferred-function
                    pop-up-frames
                    even-window-sizes))
        (setq split-window-preferred-function #'ygg-tile-split
              pop-up-frames nil
              even-window-sizes t)
        (clrhash ygg-tile--counts)
        (add-hook 'window-buffer-change-functions #'ygg-tile-pin-fixed)
        (add-hook 'window-configuration-change-hook #'ygg-tile-repair-sides)
        (add-hook 'window-configuration-change-hook #'ygg-tile-balance))
    (remove-hook 'window-buffer-change-functions #'ygg-tile-pin-fixed)
    (remove-hook 'window-configuration-change-hook #'ygg-tile-balance)
    (when ygg-tile--saved
      (setq split-window-preferred-function (nth 0 ygg-tile--saved)
            pop-up-frames (nth 1 ygg-tile--saved)
            even-window-sizes (nth 2 ygg-tile--saved)))))

(provide 'ygg-tile)
;;; ygg-tile.el ends here
