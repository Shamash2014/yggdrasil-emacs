;;; ygg-diagram.el --- inline diagrams & math, below their source -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient, below-the-source rendering of ```mermaid / ```plantuml /
;; ```dot(graphviz) fences and $$…$$ math in markdown/gfm buffers.  The source
;; stays visible; the image is an overlay (never written to the file) that clears
;; on the next edit.  Rendered async and cached by content hash, so re-toggles
;; are instant.  mermaid & latex resolve through npx (auto-install); plantuml and
;; graphviz use their native CLIs — a missing tool shows an install hint, not a
;; crash.  Each backend declares its own I/O shape (file / stdin / arg -> file /
;; stdout) so one renderer drives them all.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ygg-ui)

(declare-function ygg-mise-prefix "layer-agent")

(defgroup ygg-diagram nil "Inline diagrams & math in markdown."
  :group 'markdown)

(defcustom ygg-diagram-theme "dark"
  "Mermaid theme: default, dark, neutral, or forest."
  :type 'string)

(defvar ygg-diagram--cache-dir
  (expand-file-name "var/diagram-cache/" user-emacs-directory))

(defconst ygg-diagram--fence-re
  "^[ \t]*```[ \t]*\\(mermaid\\|plantuml\\|dot\\|graphviz\\)[ \t]*\n\\(\\(?:.*\n\\)*?\\)[ \t]*```[ \t]*$"
  "Matches a fenced diagram block; group 1 is the language, group 2 the source.")

(defconst ygg-diagram--math-re
  "\\$\\$\\(\\(?:.\\|\n\\)*?\\)\\$\\$"
  "Matches a $$…$$ display-math block; group 1 is the LaTeX source.")

(defconst ygg-diagram--mermaid-config
  "{\"htmlLabels\":false,\"flowchart\":{\"htmlLabels\":false}}"
  "Mermaid config that draws labels as SVG text.
Mermaid's default puts labels in foreignObject HTML, which librsvg does
not draw, so Emacs showed the boxes and none of the words.")

(defun ygg-diagram--mermaid-config-file ()
  "The config file mermaid is handed, written once under the cache."
  (let ((file (expand-file-name "mermaid-config.json" ygg-diagram--cache-dir)))
    (unless (file-exists-p file)
      (make-directory ygg-diagram--cache-dir t)
      (with-temp-file file (insert ygg-diagram--mermaid-config)))
    file))

(defun ygg-diagram--spec (lang)
  "Backend for LANG as (INPUT OUTPUT . ARGV), or nil.
INPUT is `file'/`stdin'/`arg'; OUTPUT is `file'/`stdout'.  ARGV holds the
symbols `in'/`out' as placeholders for the temp paths."
  (pcase lang
    ("mermaid"
     `(file file ,@(if (executable-find "mmdc") '("mmdc")
                     '("npx" "-y" "@mermaid-js/mermaid-cli"))
       "-i" in "-o" out "-t" ,ygg-diagram-theme "-b" "transparent"
       "-c" ,(ygg-diagram--mermaid-config-file)))
    ("plantuml"
     `(stdin stdout ,(or (executable-find "plantuml") "plantuml") "-tsvg" "-pipe"))
    ((or "dot" "graphviz")
     `(file file ,(or (executable-find "dot") "dot") "-Tsvg" in "-o" out))
    ("latex"
     `(arg stdout "npx" "-y" "mathjax-node-cli" "tex2svg"))))

(defun ygg-diagram--ext (lang)
  (pcase lang ("plantuml" ".puml") ((or "dot" "graphviz") ".dot") (_ ".mmd")))

(defun ygg-diagram--missing (lang)
  "Install hint when LANG's required CLI is absent, else nil."
  (pcase lang
    ("plantuml" (unless (executable-find "plantuml") "install plantuml (brew install plantuml)"))
    ((or "dot" "graphviz")
     (unless (executable-find "dot") "install graphviz (brew install graphviz)"))))

(defun ygg-diagram--login-argv (argv)
  "Wrap ARGV in a login shell so mise/PATH resolve, as the mermaid path does."
  (list (or (getenv "SHELL") "/bin/zsh") "-l" "-c"
        (concat (if (fboundp 'ygg-mise-prefix) (ygg-mise-prefix) "")
                (mapconcat #'shell-quote-argument argv " "))))

(defvar ygg-diagram--rendering (make-hash-table :test #'equal)
  "SVG path to the callbacks waiting on the render already making it.
A buffer redrawn while a diagram renders asks for it again, and one
renderer per redraw is a process storm on a streaming trace.")

(defun ygg-diagram--render (lang src on-done)
  "Render LANG SRC to an SVG, then call (ON-DONE svg-path).
A cache hit resolves synchronously; a miss renders async so Emacs never blocks."
  (let ((svg (expand-file-name
              (concat (secure-hash 'sha1 (format "%s\0%s\0%s\0svg-text"
                                                lang ygg-diagram-theme src))
                      ".svg")
              ygg-diagram--cache-dir))
        (spec (ygg-diagram--spec lang)))
    (cond
     ((null spec) (funcall on-done nil "no renderer"))
     ((file-exists-p svg) (funcall on-done svg nil))
     ((gethash svg ygg-diagram--rendering)
      (push on-done (gethash svg ygg-diagram--rendering)))
     (t
      (puthash svg (list on-done) ygg-diagram--rendering)
      (condition-case err
          (let* ((_ (make-directory ygg-diagram--cache-dir t))
                 (input (nth 0 spec)) (output (nth 1 spec)) (tpl (nthcdr 2 spec))
                 (in (and (eq input 'file)
                          (make-temp-file "ygg-dia" nil (ygg-diagram--ext lang) src)))
                 (argv (mapcar (lambda (x) (cond ((eq x 'in) in) ((eq x 'out) svg) (t x))) tpl))
                 (argv (if (eq input 'arg) (append argv (list src)) argv))
                 (obuf (generate-new-buffer " *ygg-diagram*"))
                 (proc (make-process
                        :name "ygg-diagram" :noquery t :buffer obuf
                        :command (ygg-diagram--login-argv argv)
                        :sentinel
                        (lambda (p _e)
                          (when (memq (process-status p) '(exit signal))
                            (let ((waiting (gethash svg ygg-diagram--rendering))
                                  (log ""))
                              (remhash svg ygg-diagram--rendering)
                              (ignore-errors
                                (with-current-buffer (process-buffer p)
                                  (when (and (eq output 'stdout)
                                             (eq (process-status p) 'exit)
                                             (zerop (process-exit-status p)))
                                    (write-region (point-min) (point-max) svg nil 'silent))
                                  (setq log (buffer-string))))
                              (ignore-errors (kill-buffer (process-buffer p)))
                              (when in (ignore-errors (delete-file in)))
                              (let ((ok (and (eq (process-status p) 'exit)
                                             (zerop (process-exit-status p))
                                             (file-exists-p svg))))
                                (dolist (done waiting)
                                  (ignore-errors
                                    (if ok (funcall done svg nil)
                                      (funcall done nil
                                               (car (last (split-string (string-trim log) "\n"))))))))))))))
            (when (eq input 'stdin)
              (process-send-string proc src) (process-send-eof proc)))
        (error (remhash svg ygg-diagram--rendering)
               (funcall on-done nil (error-message-string err))))))))

(defun ygg-diagram--image (file width)
  "A display string for the image FILE scaled to at most WIDTH pixels.
The type is read from the file rather than given, so a rendered diagram
and a screenshot on disk both draw through this."
  (propertize " " 'display
              (create-image file nil nil
                            :max-width (max 200 (- width 40)) :scale 1)))

(defun ygg-diagram--overlays ()
  (seq-filter (lambda (o) (overlay-get o 'ygg-diagram))
              (overlays-in (point-min) (point-max))))

(defun ygg-diagram--clear ()
  "Drop every transient diagram overlay in the buffer."
  (mapc #'delete-overlay (ygg-diagram--overlays))
  (remove-hook 'after-change-functions #'ygg-diagram--on-change t))

(defun ygg-diagram--on-change (&rest _)
  ;; a transient preview: any edit dismisses the images, re-toggle to refresh
  (when (ygg-diagram--overlays) (ygg-diagram--clear)))

(defun ygg-diagram--place (end lang src width)
  "Float LANG SRC's rendered image below buffer position END."
  (let ((ov (make-overlay end end)) (hint (ygg-diagram--missing lang)))
    (overlay-put ov 'ygg-diagram t)
    (if hint
        (overlay-put ov 'after-string
                     (concat "\n" (propertize (format "  ⚠ %s" hint) 'face 'warning) "\n"))
      (overlay-put ov 'after-string
                   (concat "\n" (propertize "  ⧗ rendering…" 'face 'shadow) "\n"))
      (ygg-diagram--render
       lang src
       (lambda (svg err)
         (when (overlay-buffer ov)
           (overlay-put ov 'after-string
                        (if svg (concat "\n" (ygg-diagram--image svg width) "\n")
                          (concat "\n" (propertize (format "  ⚠ %s" (or err "render failed"))
                                                   'face 'error) "\n")))))))))

(defun ygg-diagram--show ()
  "Render every diagram fence and $$math$$ block; float images below each."
  (let ((width (window-body-width nil t)) (n 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ygg-diagram--fence-re nil t)
        (setq n (1+ n))
        (ygg-diagram--place (match-end 0) (match-string 1) (match-string 2) width)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ygg-diagram--math-re nil t)
        (setq n (1+ n))
        (ygg-diagram--place (match-end 0) "latex" (string-trim (match-string 1)) width)))
    (if (zerop n)
        (user-error "No diagram fence or $$math$$ in buffer")
      (add-hook 'after-change-functions #'ygg-diagram--on-change nil t)
      (message "diagrams: rendering %d block(s)…" n))))

(defun ygg-diagram-toggle ()
  "Toggle transient diagram/math images below each source block."
  (interactive)
  (if (ygg-diagram--overlays)
      (progn (ygg-diagram--clear) (message "diagrams: images hidden"))
    (ygg-diagram--show)))

(defconst ygg-diagram--fence-marker (make-string 3 ?\x60)
  "The run of three characters that opens and closes a fenced block.")

(defconst ygg-diagram--langs '("mermaid" "plantuml" "dot" "graphviz")
  "The languages a fence may declare, the same set the fence regexp knows.")

(defconst ygg-diagram--open-re
  (concat "^\\([ \t+]*\\)" (regexp-quote ygg-diagram--fence-marker)
          "[ \t]*" (regexp-opt ygg-diagram--langs t) "[ \t]*$")
  "A fence opener read leniently; group 1 is the prefix, group 2 the language.
The prefix is the indentation, or a diff marker and what follows it.")

(defconst ygg-diagram--close-re
  (concat "^[ \t+]*" (regexp-quote ygg-diagram--fence-marker) "[ \t]*$")
  "A bare fence closer, under the same lenient prefix as the opener.")

(defvar-local ygg-diagram--shown nil
  "Fences drawn in this buffer as (LANG . SRC), so a redraw can restore them.")

(defun ygg-diagram--unprefix (line n)
  "LINE without the N characters the opening line carried before its marker.
Those N characters are dropped only when they are whitespace or a diff
marker, so a body line that is indented less than its opener keeps its
own shape rather than losing text."
  (if (and (>= (length line) n)
           (string-match-p "\\`[ \t+]*\\'" (substring line 0 n)))
      (substring line n)
    (string-trim-left line "[ \t]+")))

(defun ygg-diagram-fence-at-point ()
  "The fenced block point is on or in, as (LANG SRC BEG END), or nil.
Point counts on the opening line, the closing line, and every line
between.  SRC is stripped of the prefix the opening line carried, and a
line removed by a diff is left out of it, so a block inside a patch
reads as the after-state."
  (save-excursion
    (beginning-of-line)
    (let ((start (point)) beg lang prefix)
      (if (looking-at ygg-diagram--open-re)
          (setq beg (point))
        (catch 'outside
          (while (and (not beg) (not (bobp)))
            (forward-line -1)
            (cond ((looking-at ygg-diagram--open-re) (setq beg (point)))
                  ((looking-at ygg-diagram--close-re) (throw 'outside nil))))))
      (when beg
        (goto-char beg)
        (looking-at ygg-diagram--open-re)
        (setq lang (match-string-no-properties 2)
              prefix (length (match-string 1)))
        (let (lines end)
          (forward-line 1)
          (while (and (not end) (not (eobp)))
            (if (looking-at ygg-diagram--close-re)
                (setq end (line-end-position))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (unless (string-prefix-p "-" line)
                  (push (ygg-diagram--unprefix line prefix) lines)))
              (forward-line 1)))
          (when (and end (<= start end))
            (list lang
                  (mapconcat (lambda (l) (concat l "\n")) (nreverse lines) "")
                  beg end)))))))

(defun ygg-diagram--overlay-at (pos)
  "The diagram overlay floating at POS, or nil."
  (seq-find (lambda (o) (eql (overlay-start o) pos)) (ygg-diagram--overlays)))

(defvar-local ygg-diagram-image-root nil
  "Directory a relative image path in this buffer is read against.
A trace stands in the tree it traces, so its rows name files from that
root even when the buffer sits somewhere else.")

(defconst ygg-diagram--image-ext-re
  "\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\|bmp\\|tiff\\)\\'"
  "The extensions a token must end in to be read as an image file.")

(defconst ygg-diagram--markdown-image-re
  "!\\[[^]]*\\](\\([^)]+\\))"
  "A markdown image; group 1 is the path it points at.")

(defconst ygg-diagram--html-image-re
  "<img[^>]*src=[\"']\\([^\"']+\\)[\"']"
  "An HTML image element; group 1 is the path it points at.")

(defun ygg-diagram--image-file (token)
  "TOKEN as an absolute readable image file, or nil.
A trailing punctuation mark or closing bracket is dropped first, then
the name is read against the image root when one is set, and against
the directory of the buffer otherwise."
  (let ((name (string-trim-right token "[]),.;:!?\"'>]+"))
        (case-fold-search t))
    (when (string-match-p ygg-diagram--image-ext-re name)
      (seq-some (lambda (root)
                  (let ((path (expand-file-name name root)))
                    (and (file-readable-p path)
                         (not (file-directory-p path))
                         path)))
                (delq nil (list ygg-diagram-image-root default-directory))))))

(defun ygg-diagram-image-at-point ()
  "The image file the line at point names, absolute, or nil.
A markdown image is read first, then an HTML image source, then any
bare token that ends in an image extension, which is how a file heading
in a diff and an evidence row are read.  The line may be indented or
carry a diff marker; a line a diff removed names nothing.  A file that
is not there, or cannot be read, is no name at all."
  (save-excursion
    (let ((raw (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
      (unless (string-prefix-p "-" raw)
        (let ((line (string-trim-left raw "[ \t+]+")))
          (or (and (string-match ygg-diagram--markdown-image-re line)
                   (ygg-diagram--image-file (match-string 1 line)))
              (and (let ((case-fold-search t))
                     (string-match ygg-diagram--html-image-re line))
                   (ygg-diagram--image-file (match-string 1 line)))
              (seq-some #'ygg-diagram--image-file
                        (split-string line "[ \t]+" t))))))))

(defun ygg-diagram--place-image (end path width)
  "Float the image file PATH below buffer position END, scaled to WIDTH."
  (let ((ov (make-overlay end end)))
    (overlay-put ov 'ygg-diagram t)
    (overlay-put ov 'after-string
                 (concat "\n" (ygg-diagram--image path width) "\n"))))

(defun ygg-diagram-toggle-image-at-point ()
  "Draw the image the line at point names below it, or take it away."
  (interactive)
  (let ((path (ygg-diagram-image-at-point))
        (end (line-end-position)))
    (unless path (user-error "Not an image file"))
    (if-let* ((ov (ygg-diagram--overlay-at end)))
        (progn
          (delete-overlay ov)
          (setq ygg-diagram--shown
                (seq-remove (lambda (cell) (equal cell (cons 'image path)))
                            ygg-diagram--shown)))
      (cl-pushnew (cons 'image path) ygg-diagram--shown :test #'equal)
      (ygg-diagram--place-image end path (window-body-width nil t)))))

(defun ygg-diagram-toggle-at-point ()
  "Draw the fence at point below its source, or take that drawing away."
  (interactive)
  (pcase (ygg-diagram-fence-at-point)
    (`(,lang ,src ,_beg ,end)
     (if-let* ((ov (ygg-diagram--overlay-at end)))
         (progn
           (delete-overlay ov)
           (setq ygg-diagram--shown
                 (seq-remove (lambda (cell) (equal cell (cons lang src)))
                             ygg-diagram--shown)))
       (cl-pushnew (cons lang src) ygg-diagram--shown :test #'equal)
       (ygg-diagram--place end lang src (window-body-width nil t))))
    (_ (user-error "No diagram fence here"))))

(defun ygg-diagram-show-images (&optional keep)
  "Mark every image this buffer names as one to draw, and say how many.
KEEP is a predicate of the path, and only the paths it likes are shown:
a trace names files an agent wrote and files it only read about, and
drawing the second kind is drawing whatever the tree happens to hold.

Nothing is drawn here.  This is the set ygg-diagram-replace works
from, so a caller marks the images and redraws once."
  (let ((added 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((path (ygg-diagram-image-at-point))
                    ((or (null keep) (funcall keep path)))
                    ((not (member (cons 'image path) ygg-diagram--shown))))
          (cl-pushnew (cons 'image path) ygg-diagram--shown :test #'equal)
          (cl-incf added))
        (forward-line 1)))
    added))

(defun ygg-diagram-replace ()
  "Draw again every fence this buffer had drawn and still holds.
A redraw leaves overlays behind collapsed rather than gone, so they are
dropped first; a fence that is no longer in the buffer is passed over."
  (mapc #'delete-overlay (ygg-diagram--overlays))
  (when ygg-diagram--shown
    ;; a redraw from a timer runs in whatever window is selected
    (let ((width (window-body-width (get-buffer-window nil t) t)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward ygg-diagram--open-re nil t)
          (beginning-of-line)
          (pcase (ygg-diagram-fence-at-point)
            (`(,lang ,src ,_beg ,end)
             (when (member (cons lang src) ygg-diagram--shown)
               (ygg-diagram--place end lang src width))
             (goto-char end))
            (_ (end-of-line)))))
      (when (seq-some (lambda (c) (eq (car c) 'image)) ygg-diagram--shown)
        (save-excursion
          (goto-char (point-min))
          ;; once, under the first line naming it: a trace names a file in
          ;; its folded row and again in the row opened under it
          (let (placed)
            (while (not (eobp))
              (when-let* ((path (ygg-diagram-image-at-point))
                          ((member (cons 'image path) ygg-diagram--shown))
                          ((not (member path placed))))
                (push path placed)
                (ygg-diagram--place-image (line-end-position) path width))
              (forward-line 1))))))))

(defconst ygg-diagram--md-langs '("" "text" "md" "markdown")
  "The language words a fence renders as prose under, the empty one included.")

(defconst ygg-diagram--md-fence-re
  (concat "^[ \t]*" (regexp-quote ygg-diagram--fence-marker)
          "[ \t]*\\(?2:[^ \t\n]*\\)[ \t]*\n"
          "\\(?1:\\(?:.*\n\\)*?\\)"
          "[ \t]*" (regexp-quote ygg-diagram--fence-marker) "[ \t]*$")
  "Matches any fenced block; group 1 is the body, group 2 the language.
Every fence is paired, not only the prose ones, so a diagram fence is
passed over whole and the bare marker after it reads as an opener
rather than as the closer it also looks like.")

(defun ygg-diagram-md-fence-at-point ()
  "The prose fence point is on or in, as (BODY BEG END), or nil.
BEG is the start of the opening line and END the end of the closing
line, so the whole fence is covered; point counts on either of those
lines and on every line between."
  (save-excursion
    (let ((here (line-beginning-position)) found)
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward ygg-diagram--md-fence-re nil t))
        (let ((body (match-string-no-properties 1))
              (lang (match-string-no-properties 2))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (when (and (member lang ygg-diagram--md-langs) (<= beg here) (< here end))
            (setq found (list body beg end)))))
      found)))

(defun ygg-diagram--md-overlay-in (beg end)
  "The prose overlay standing over the fence between BEG and END, or nil."
  (seq-find (lambda (o) (overlay-get o 'ygg-diagram-md))
            (overlays-in beg end)))

(defun ygg-diagram-toggle-md-at-point ()
  "Show the prose fence at point as rendered markdown, or as itself again.
The buffer text is never touched: one overlay covers the fence and
displays the fontified body in its place."
  (interactive)
  (pcase (ygg-diagram-md-fence-at-point)
    (`(,body ,beg ,end)
     (if-let* ((ov (ygg-diagram--md-overlay-in beg end)))
         (delete-overlay ov)
       (let ((ov (make-overlay beg end)))
         (overlay-put ov 'ygg-diagram t)
         (overlay-put ov 'ygg-diagram-md t)
         (overlay-put ov 'display (concat (ygg-ui-markdown body) "\n")))))
    (_ (user-error "No markdown fence here"))))

(defun ygg-diagram-toggle-any-at-point ()
  "Draw the fence or the image the line at point names, or nothing.
Returns non-nil when there was something to draw or take away, so a key
that means more than one thing can fall through to its other meaning."
  (interactive)
  (cond ((ygg-diagram-fence-at-point) (ygg-diagram-toggle-at-point) t)
        ((ygg-diagram-md-fence-at-point) (ygg-diagram-toggle-md-at-point) t)
        ((ygg-diagram-image-at-point) (ygg-diagram-toggle-image-at-point) t)))

(declare-function magit-section-toggle "magit-section" (section))
(declare-function magit-current-section "magit-section" ())
(defvar magit-diff-mode-map)

(with-eval-after-load 'magit-diff
  (defun ygg-diagram-magit-tab ()
    "Draw the fence or image at point, or fold the section point is on."
    (interactive)
    (unless (ygg-diagram-toggle-any-at-point)
      (magit-section-toggle (magit-current-section))))
  (define-key magit-diff-mode-map (kbd "TAB") #'ygg-diagram-magit-tab))

(provide 'ygg-diagram)
;;; ygg-diagram.el ends here
