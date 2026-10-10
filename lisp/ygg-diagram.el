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
  (concat "^[ \t]*\\(?3:```\\|~~~\\)[ \t]*{?[ \t]*"
          "\\(?1:mermaid\\|plantuml\\|dot\\|graphviz\\)"
          "\\(?:[ \t}\r][^\n]*\\)?\n\\(?2:\\(?:.*\n\\)*?\\)[ \t]*\\3[ \t]*\r?$")
  "Matches a fenced diagram block; group 1 is the language, group 2 the source.
The fence may use backticks or tildes, name the language as {lang}, carry
attributes after it, and end its lines in CRLF.")

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

(defun ygg-diagram--log-summary (log)
  "The line of renderer LOG that says what went wrong, or nil when it is empty."
  (let* ((lines (split-string (string-trim log) "\n" t "[ \t\r]+"))
         (case-fold-search nil)
         (plain (seq-remove (lambda (l) (string-prefix-p "at " l)) lines)))
    (or (car (last (seq-filter
                    (lambda (l) (string-match-p
                                 "Error\\|error:\\|Parse error\\|Expecting\\|Lexical error" l))
                    plain)))
        (car (last plain))
        (car (last lines)))))

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
                                               (ygg-diagram--log-summary log))))))))))))
            (when (eq input 'stdin)
              (process-send-string proc src) (process-send-eof proc)))
        (error (remhash svg ygg-diagram--rendering)
               (funcall on-done nil (error-message-string err))))))))

(defcustom ygg-diagram-scale 1.0
  "Size of inline diagrams, math and images relative to the window width.
Above 1.0 they are drawn wider than the window and clip at its edge
unless lines are truncated."
  :type 'number)

(defcustom ygg-diagram-scale-max 4.0
  "Largest scale `ygg-diagram-enlarge' and `ygg-diagram-scale' can reach."
  :type 'number)

(defvar-local ygg-diagram--scale nil
  "This buffer's diagram scale; nil until resized, then `ygg-diagram-scale'.")

(defconst ygg-diagram--scale-step 1.25)
(defconst ygg-diagram--scale-floor 0.25)

(defun ygg-diagram--clamp-scale (scale)
  (min ygg-diagram-scale-max (max ygg-diagram--scale-floor scale)))

(defun ygg-diagram--effective-scale ()
  (ygg-diagram--clamp-scale (or ygg-diagram--scale ygg-diagram-scale)))

(defun ygg-diagram--make-image (file width scale)
  (let ((base (max 200 (- width 40))))
    (create-image file nil nil
                  :max-width (round (* base scale)) :scale scale
                  :ygg-base-width base)))

(defun ygg-diagram--image (file width)
  "A display string for the image FILE scaled to at most WIDTH pixels.
The type is read from the file rather than given, so a rendered diagram
and a screenshot on disk both draw through this."
  (propertize " " 'display
              (ygg-diagram--make-image file width (ygg-diagram--effective-scale))))

(defun ygg-diagram--rescale-string (str scale)
  "A copy of STR whose image display specs are redrawn at SCALE."
  (let ((out (copy-sequence str)) (pos 0))
    (while (< pos (length out))
      (let ((next (or (next-single-property-change pos 'display out) (length out)))
            (disp (get-text-property pos 'display out)))
        (when (and (eq (car-safe disp) 'image) (plist-get (cdr disp) :ygg-base-width))
          (let ((spec (copy-sequence disp)))
            (setcdr spec (plist-put (copy-sequence (cdr spec)) :scale scale))
            (setcdr spec (plist-put (cdr spec) :max-width
                                    (round (* scale (plist-get (cdr spec) :ygg-base-width)))))
            (put-text-property pos next 'display spec out)))
        (setq pos next)))
    out))

(defun ygg-diagram--set-scale (scale)
  (setq ygg-diagram--scale (ygg-diagram--clamp-scale scale))
  (dolist (ov (ygg-diagram--overlays))
    (when-let* ((str (overlay-get ov 'after-string)))
      (overlay-put ov 'after-string (ygg-diagram--rescale-string str ygg-diagram--scale))))
  (message "diagrams %d%%%s" (round (* 100 ygg-diagram--scale))
           (if (>= ygg-diagram--scale ygg-diagram-scale-max) " (max)" "")))

(defun ygg-diagram-enlarge ()
  "Draw the diagrams and math in this buffer larger, up to `ygg-diagram-scale-max'.
Past the window width they clip at its edge unless lines are truncated."
  (interactive)
  (ygg-diagram--set-scale (* (ygg-diagram--effective-scale) ygg-diagram--scale-step)))

(defun ygg-diagram-shrink ()
  "Draw the diagrams and math in this buffer smaller."
  (interactive)
  (ygg-diagram--set-scale (/ (ygg-diagram--effective-scale) ygg-diagram--scale-step)))

(defun ygg-diagram-scale-reset ()
  "Draw the diagrams and math in this buffer at `ygg-diagram-scale'."
  (interactive)
  (ygg-diagram--set-scale ygg-diagram-scale)
  (setq ygg-diagram--scale nil))

(defvar ygg-diagram-scale-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+" #'ygg-diagram-enlarge)
    (define-key map "-" #'ygg-diagram-shrink)
    (define-key map "0" #'ygg-diagram-scale-reset)
    map)
  "Sticky size keys, entered after any of the diagram size commands.")

(dolist (cmd '(ygg-diagram-enlarge ygg-diagram-shrink ygg-diagram-scale-reset))
  (put cmd 'repeat-map 'ygg-diagram-scale-repeat-map))

(defun ygg-diagram--overlays ()
  (seq-filter (lambda (o) (overlay-get o 'ygg-diagram))
              (overlays-in (point-min) (point-max))))

(defun ygg-diagram--clear ()
  "Drop every transient diagram overlay in the buffer."
  (mapc #'delete-overlay (ygg-diagram--overlays))
  (remove-hook 'after-change-functions #'ygg-diagram--on-change t)
  (remove-hook 'before-revert-hook #'ygg-diagram--clear t))

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
           (with-current-buffer (overlay-buffer ov)
             (overlay-put ov 'after-string
                          (if svg (concat "\n" (ygg-diagram--image svg width) "\n")
                            (concat "\n" (propertize (format "  ⚠ %s" (or err "render failed"))
                                                     'face 'error) "\n"))))))))))

(defun ygg-diagram--show ()
  "Render every diagram fence and $$math$$ block; float images below each."
  (let ((width (window-body-width nil t)) (n 0))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward ygg-diagram--fence-re nil t)
          (setq n (1+ n))
          (ygg-diagram--place (match-end 0) (downcase (match-string 1))
                              (remove ?\r (match-string 2)) width))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ygg-diagram--math-re nil t)
        (setq n (1+ n))
        (ygg-diagram--place (match-end 0) "latex" (string-trim (match-string 1)) width)))
    (if (zerop n)
        (user-error "No diagram fence or $$math$$ in buffer")
      (add-hook 'after-change-functions #'ygg-diagram--on-change nil t)
      (add-hook 'before-revert-hook #'ygg-diagram--clear nil t)
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
  (concat "^\\(?1:[ \t+]*\\)\\(?3:" (regexp-quote ygg-diagram--fence-marker) "\\|~~~\\)"
          "[ \t]*{?[ \t]*\\(?2:" (regexp-opt ygg-diagram--langs) "\\)"
          "\\(?:[ \t}\r][^\r\n]*\\)?\r?$")
  "A fence opener read leniently; group 1 is the prefix, group 2 the language.
The prefix is the indentation, or a diff marker and what follows it.  Group 3
is the marker, backticks or tildes.  Attributes and a CRLF ending are allowed.")

(defvar-local ygg-diagram--shown nil
  "Fences drawn in this buffer as (LANG . SRC), so a redraw can restore them.")

(defcustom ygg-diagram-scan-limit 2000
  "Lines a fence search looks back through before it gives up."
  :type 'integer)

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
line removed by a diff is left out of it when the block sits inside a
patch, so it reads as the after-state."
  (save-excursion
    (beginning-of-line)
    (let ((start (point)) (case-fold-search t) (left ygg-diagram-scan-limit)
          beg lang prefix marker closers in-diff)
      (if (looking-at ygg-diagram--open-re)
          (setq beg (point))
        (while (and (not beg) (not (bobp)) (> left 0) (< (length closers) 2))
          (forward-line -1)
          (cl-decf left)
          (cond ((looking-at ygg-diagram--open-re) (setq beg (point)))
                ((looking-at "[ \t+]*\\(```\\|~~~\\)[ \t]*\r?$")
                 (cl-pushnew (match-string 1) closers :test #'equal)))))
      (when beg
        (goto-char beg)
        (looking-at ygg-diagram--open-re)
        (setq lang (downcase (match-string-no-properties 2))
              prefix (length (match-string 1))
              marker (match-string-no-properties 3)
              in-diff (or (derived-mode-p 'diff-mode 'magit-diff-mode 'magit-revision-mode)
                          (string-search "+" (match-string 1))))
        (let ((close-re (concat "^[ \t+]*" (regexp-quote marker) "[ \t]*\r?$"))
              lines end)
          (forward-line 1)
          (while (and (not end) (not (eobp)))
            (if (looking-at close-re)
                (setq end (line-end-position))
              (let ((line (string-trim-right
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))
                           "\r")))
                (unless (and in-diff (string-prefix-p "-" line))
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
      (when (seq-some (lambda (c) (eq (car c) 'table)) ygg-diagram--shown)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (pcase (and (ygg-diagram--table-row-line-p) (ygg-diagram-table-at-point))
              (`(,beg ,end ,rows)
               (let ((key (cons 'table (buffer-substring-no-properties beg end))))
                 (when (member key ygg-diagram--shown)
                   (ygg-diagram--table-place beg end rows key)))
               (goto-char end)))
            (forward-line 1))))
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

(defconst ygg-diagram--table-delimiter-re
  "^[ \t]*|?[ \t]*:?-+:?[ \t]*\\(?:|[ \t]*:?-+:?[ \t]*\\)*|?[ \t]*\r?$"
  "A GFM delimiter row such as |---|:--:|--:.")

(defun ygg-diagram--table-row-line-p ()
  "Whether the line at point can be a table row: it holds a pipe."
  (and (not (looking-at-p "[ \t]*\r?$"))
       (string-match-p "|" (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))

(defun ygg-diagram--table-cells (line)
  "The cells of the table row LINE, outer pipes dropped and \\| unescaped."
  (let* ((s (string-trim line))
         (cells nil)
         (cell nil)
         (i 0)
         (n (length s)))
    (while (< i n)
      (let ((c (aref s i)))
        (cond ((and (eq c ?\\) (< (1+ i) n) (eq (aref s (1+ i)) ?|))
               (push ?| cell)
               (setq i (+ i 2)))
              ((eq c ?|)
               (push (apply #'string (nreverse cell)) cells)
               (setq cell nil i (1+ i)))
              (t (push c cell) (setq i (1+ i))))))
    (push (apply #'string (nreverse cell)) cells)
    (setq cells (nreverse cells))
    (when (string-prefix-p "|" s) (pop cells))
    (when (and (string-suffix-p "|" s) (not (string-suffix-p "\\|" s))
               (equal (car (last cells)) ""))
      (setq cells (butlast cells)))
    (mapcar #'string-trim cells)))

(defun ygg-diagram--table-align (cell)
  "The alignment a delimiter CELL asks for: left, right or center."
  (let ((left (string-prefix-p ":" cell)) (right (string-suffix-p ":" cell)))
    (cond ((and left right) 'center) (right 'right) (t 'left))))

(defun ygg-diagram--table-plain (cell)
  "CELL with its inline markup shown as the words it styles."
  (let ((s (replace-regexp-in-string "!?\\[\\([^]]*\\)\\]([^)]*)" "\\1" cell)))
    (dolist (mark '("**" "__" "~~" "`"))
      (setq s (string-replace mark "" s)))
    s))

(defun ygg-diagram--table-pad (text width align)
  "TEXT padded with spaces to display WIDTH, placed by ALIGN."
  (let ((gap (- width (string-width text))))
    (pcase align
      ('right (concat (make-string gap ?\s) text))
      ('center (concat (make-string (/ gap 2) ?\s) text
                       (make-string (- gap (/ gap 2)) ?\s)))
      (_ (concat text (make-string gap ?\s))))))

(defun ygg-diagram--table-fit (widths max-width)
  "WIDTHS with the widest columns cut down until the box is MAX-WIDTH wide.
No column goes under three characters; a box that still cannot fit stays wide."
  (let ((widths (copy-sequence widths)))
    (while (and (> (+ (apply #'+ widths) (* 3 (length widths)) 1) max-width)
                (let ((widest (apply #'max widths)))
                  (when (> widest 3)
                    (cl-decf (nth (cl-position widest widths) widths))
                    t))))
    widths))

(defun ygg-diagram--table-window-width ()
  "The columns a table may fill: the window showing this buffer, else `fill-column'."
  (if (eq (window-buffer) (current-buffer))
      (window-body-width)
    fill-column))

(defun ygg-diagram--table-render (rows &optional max-width)
  "ROWS, a list of cell lists headed by the header and the delimiter row, boxed.
The header is bold and every column is as wide as its widest cell, cut to
fit MAX-WIDTH columns when that is given."
  (let* ((header (car rows))
         (aligns (mapcar #'ygg-diagram--table-align (cadr rows)))
         (ncol (length header))
         (body (mapcar (lambda (row)
                         (cl-loop for k below ncol
                                  collect (ygg-diagram--table-plain (or (nth k row) ""))))
                       (cddr rows)))
         (head (mapcar #'ygg-diagram--table-plain header))
         (natural (cl-loop for k below ncol
                           collect (apply #'max 1
                                          (mapcar (lambda (r) (string-width (nth k r)))
                                                  (cons head body)))))
         (widths (if max-width (ygg-diagram--table-fit natural max-width) natural))
         (cut (lambda (row)
                (cl-loop for text in row for w in widths
                         collect (truncate-string-to-width text w 0 nil "…"))))
         (head (funcall cut head))
         (body (mapcar cut body))
         (rule (lambda (l m r)
                 (concat l (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths m) r)))
         (line (lambda (row bold)
                 (concat "│ "
                         (mapconcat
                          #'identity
                          (cl-loop for k below ncol
                                   for text = (ygg-diagram--table-pad
                                               (nth k row) (nth k widths)
                                               (or (nth k aligns) 'left))
                                   collect (if bold (propertize text 'face 'bold) text))
                          " │ ")
                         " │"))))
    (mapconcat #'identity
               (append (list (funcall rule "┌" "┬" "┐")
                             (funcall line head t)
                             (funcall rule "├" "┼" "┤"))
                       (mapcar (lambda (row) (funcall line row nil)) body)
                       (list (funcall rule "└" "┴" "┘")))
               "\n")))

(defconst ygg-diagram--code-fence-re "[ \t]*\\(```\\|~~~\\)"
  "A line that opens or closes a fenced code block; group 1 is the marker.")

(defun ygg-diagram--code-line-p ()
  "Whether the line at point sits in a fenced or indented code block."
  (if (and (derived-mode-p 'markdown-mode) (fboundp 'markdown-code-block-at-point-p))
      (save-excursion
        (syntax-propertize (line-end-position))
        (and (markdown-code-block-at-point-p) t))
    (save-excursion
      (beginning-of-line)
      (let ((target (point)) (marker nil))
        (forward-line (- ygg-diagram-scan-limit))
        (while (< (point) target)
          (when (looking-at ygg-diagram--code-fence-re)
            (let ((m (match-string 1)))
              (cond ((null marker) (setq marker m))
                    ((equal m marker) (setq marker nil)))))
          (forward-line 1))
        (or (and marker t)
            (and (looking-at "\\(?: \\{4\\}\\|\t\\)")
                 (progn
                   (while (and (not (bobp))
                               (progn (forward-line -1)
                                      (looking-at "\\(?: \\{4\\}\\|\t\\|[ \t]*\r?$\\)"))))
                   (not (looking-at "[ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]")))))))))

(defun ygg-diagram--table-line-at (pos)
  "The text of the line holding POS."
  (save-excursion
    (goto-char pos)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun ygg-diagram-table-at-point ()
  "The GFM table point is on or in, as (BEG END ROWS), or nil.
BEG is the start of the header row and END the end of the last row.  ROWS
holds the header cells, the delimiter cells, then each body row's cells."
  (save-excursion
    (beginning-of-line)
    (let ((here (point)) found)
      (when (and (ygg-diagram--table-row-line-p) (not (ygg-diagram--code-line-p)))
        (while (save-excursion
                 (and (zerop (forward-line -1)) (ygg-diagram--table-row-line-p)))
          (forward-line -1))
        (while (and (not found) (ygg-diagram--table-row-line-p))
          (let* ((beg (point))
                 (head (ygg-diagram--table-cells (ygg-diagram--table-line-at beg))))
            (forward-line 1)
            (when (and (looking-at ygg-diagram--table-delimiter-re)
                       (ygg-diagram--table-row-line-p)
                       (= (length head)
                          (length (ygg-diagram--table-cells
                                   (ygg-diagram--table-line-at (point))))))
              (let ((delim (ygg-diagram--table-cells
                            (ygg-diagram--table-line-at (point))))
                    (end (line-end-position))
                    rows)
                (forward-line 1)
                (while (ygg-diagram--table-row-line-p)
                  (push (ygg-diagram--table-cells (ygg-diagram--table-line-at (point)))
                        rows)
                  (setq end (line-end-position))
                  (forward-line 1))
                (setq found (list beg end (cons head (cons delim (nreverse rows))))))))))
      (when (and found (<= (car found) here) (<= here (cadr found)))
        found))))

(defun ygg-diagram--table-overlay (beg)
  "The table overlay standing over the table that starts at BEG, or nil."
  (seq-find (lambda (o) (and (overlay-get o 'ygg-diagram-table)
                             (eql (overlay-start o) beg)))
            (ygg-diagram--overlays)))

(defun ygg-diagram--table-modified (ov after &rest _)
  (unless after
    (let ((key (overlay-get ov 'ygg-diagram-table-key)))
      (delete-overlay ov)
      (setq ygg-diagram--shown (delete key ygg-diagram--shown)))))

(defun ygg-diagram--table-place (beg end rows key)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'ygg-diagram t)
    (overlay-put ov 'ygg-diagram-table t)
    (overlay-put ov 'ygg-diagram-table-key key)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'modification-hooks (list #'ygg-diagram--table-modified))
    (overlay-put ov 'display
                 (ygg-diagram--table-render rows (ygg-diagram--table-window-width)))))

(defun ygg-diagram-toggle-table-at-point ()
  "Show the table at point as an aligned box, or as its source again.
The buffer text is never touched: one overlay covers the table and
displays the box in its place."
  (interactive)
  (pcase (ygg-diagram-table-at-point)
    (`(,beg ,end ,rows)
     (let ((key (cons 'table (buffer-substring-no-properties beg end))))
       (if-let* ((ov (ygg-diagram--table-overlay beg)))
           (progn
             (delete-overlay ov)
             (setq ygg-diagram--shown (delete key ygg-diagram--shown)))
         (cl-pushnew key ygg-diagram--shown :test #'equal)
         (add-hook 'before-revert-hook #'ygg-diagram--clear nil t)
         (ygg-diagram--table-place beg end rows key))))
    (_ (user-error "No table here"))))

(defcustom ygg-diagram-markdown-tab-fallback 'ygg-jump-forward
  "Command TAB runs in a markdown buffer when it is on no diagram or table."
  :type 'function)

(defun ygg-diagram-markdown-tab ()
  "Draw the diagram or table at point, else do what TAB did before."
  (interactive)
  (cond ((ygg-diagram-table-at-point) (ygg-diagram-toggle-table-at-point))
        ((ygg-diagram-toggle-any-at-point))
        (t (call-interactively ygg-diagram-markdown-tab-fallback))))

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
