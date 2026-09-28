;;; ygg-call-graph.el --- The calls around a symbol, as a plain text graph -*- lexical-binding: t; -*-

;; Callers above, the symbol in bold, callees below, each side a tree
;; walked breadth-first through the server's call hierarchy.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'jsonrpc)
(require 'ygg-lsp-calls)

(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function project-root "project" (project))
(declare-function yggdrasil-define-mode-keys "yggdrasil-core")
(defvar ygg-modal-special-modes)

(defgroup ygg-call-graph nil
  "A text call graph of the symbol at point."
  :group 'tools)

(defcustom ygg-call-graph-depth 2
  "How many calls away from the root the graph walks on each side.
The symbol all walks until no new node turns up."
  :type '(choice (const 1) (const 2) (const 3) (const all)))

(defcustom ygg-call-graph-max-nodes 60
  "Most project nodes one walk keeps; the nodes whose calls were cut say more.
Library nodes are not counted, as they are never walked through."
  :type 'natnum)

(defcustom ygg-call-graph-timeout 10
  "Seconds one walk may wait on the language server, all requests together."
  :type 'number)

(defcustom ygg-call-graph-all-max-nodes 300
  "Most project nodes a walk at depth all keeps, in place of the usual cap."
  :type 'natnum)

(defcustom ygg-call-graph-all-timeout 30
  "Seconds a walk at depth all may wait on the language server."
  :type 'number)

(defcustom ygg-call-graph-library-regexp
  (concat "/\\(node_modules\\|site-packages\\|dist-packages\\|\\.venv\\|venv"
          "\\|vendor\\|target\\|typeshed[^/]*\\|go/pkg/mod\\|\\.rustup"
          "\\|\\.cargo/registry\\|toolchains?\\|Toolchains\\)/")
  "Paths under these directories are library code even inside the project."
  :type 'regexp)

(cl-defstruct (ygg-call-graph--node (:constructor ygg-call-graph--node-create))
  key item name detail path line library callers callees
  callers-state callees-state)

(defvar-local ygg-call-graph--server nil)
(defvar-local ygg-call-graph--project-root nil)
(defvar-local ygg-call-graph--root nil "The root node's key.")
(defvar-local ygg-call-graph--nodes nil "Nodes by key.")
(defvar-local ygg-call-graph--walk-depth nil)
(defvar-local ygg-call-graph--show-library nil)
(defvar-local ygg-call-graph--folded nil "Folded rows, keyed by (SIDE . KEY).")
(defvar-local ygg-call-graph--filter nil)
(defvar-local ygg-call-graph--deadline nil)

;;; Data

(defun ygg-call-graph--item-start (item)
  (plist-get (or (plist-get item :selectionRange) (plist-get item :range)) :start))

(defun ygg-call-graph--key (item)
  "ITEM's uri and start, so the same symbol reached twice is one node."
  (let ((start (ygg-call-graph--item-start item)))
    (format "%s#%s:%s" (plist-get item :uri)
            (plist-get start :line) (plist-get start :character))))

(defun ygg-call-graph--library-p (path)
  "Whether PATH lies outside the project or in a dependency directory."
  (or (null path)
      (and ygg-call-graph--project-root
           (not (file-in-directory-p path ygg-call-graph--project-root)))
      (let ((inner (if (and ygg-call-graph--project-root
                            (file-in-directory-p path ygg-call-graph--project-root))
                       (concat "/" (file-relative-name path ygg-call-graph--project-root))
                     path)))
        (string-match-p ygg-call-graph-library-regexp inner))))

(defun ygg-call-graph--node (key)
  (gethash key ygg-call-graph--nodes))

(defun ygg-call-graph--intern (item)
  "The node for ITEM, made when new."
  (let ((key (ygg-call-graph--key item)))
    (or (ygg-call-graph--node key)
        (let ((path (ygg-call-graph--item-path item)))
          (puthash key (ygg-call-graph--node-create
                        :key key :item item :name (plist-get item :name)
                        :detail (plist-get item :detail) :path path
                        :line (1+ (or (plist-get (ygg-call-graph--item-start item) :line) 0))
                        :library (ygg-call-graph--library-p path))
                   ygg-call-graph--nodes)))))

(defun ygg-call-graph--item-path (item)
  (let ((uri (plist-get item :uri)))
    (and (stringp uri) (string-prefix-p "file:" uri) (eglot-uri-to-path uri))))

(defun ygg-call-graph--project-count ()
  (let ((n 0))
    (maphash (lambda (_ node) (unless (ygg-call-graph--node-library node) (cl-incf n)))
             ygg-call-graph--nodes)
    n))

(defun ygg-call-graph--request (method params)
  "Ask the server METHOD with PARAMS, giving up at the walk's deadline."
  (let ((left (- ygg-call-graph--deadline (float-time))))
    (condition-case err
        (if (<= left 0)
            (signal 'jsonrpc-error '((jsonrpc-error-message . "Timed out")))
          (jsonrpc-request ygg-call-graph--server method params :timeout left))
      (jsonrpc-error
       (let ((why (or (alist-get 'jsonrpc-error-message (cdr err)) (format "%S" (cdr err)))))
         (if (equal why "Timed out")
             (let ((budget (ygg-call-graph--timeout-option)))
               (user-error "Call graph: no answer from the server within %ss (%s)"
                           (symbol-value budget) budget))
           (user-error "Call graph: %s" why)))))))

(defun ygg-call-graph--all-p ()
  (eq ygg-call-graph--walk-depth 'all))

(defun ygg-call-graph--timeout-option ()
  (if (ygg-call-graph--all-p) 'ygg-call-graph-all-timeout 'ygg-call-graph-timeout))

(defun ygg-call-graph--cap ()
  (if (ygg-call-graph--all-p) ygg-call-graph-all-max-nodes ygg-call-graph-max-nodes))

(defun ygg-call-graph--start-clock ()
  (setq ygg-call-graph--deadline
        (+ (float-time) (symbol-value (ygg-call-graph--timeout-option)))))

(defun ygg-call-graph--state (node side)
  (if (eq side 'callers) (ygg-call-graph--node-callers-state node)
    (ygg-call-graph--node-callees-state node)))

(defun ygg-call-graph--children (node side)
  (if (eq side 'callers) (ygg-call-graph--node-callers node)
    (ygg-call-graph--node-callees node)))

(defun ygg-call-graph--fetch (node side cap)
  "Link NODE's SIDE calls, new project nodes only while fewer than CAP.
Return the keys of the linked children."
  (let* ((incoming (eq side 'callers))
         (calls (ygg-call-graph--request
                 (if incoming :callHierarchy/incomingCalls :callHierarchy/outgoingCalls)
                 (list :item (ygg-call-graph--node-item node))))
         (keys (ygg-call-graph--children node side))
         (state t))
    (seq-doseq (call calls)
      (let ((item (plist-get call (if incoming :from :to))))
        (if (and cap (not (ygg-call-graph--node (ygg-call-graph--key item)))
                 (not (ygg-call-graph--library-p (ygg-call-graph--item-path item)))
                 (>= (ygg-call-graph--project-count) cap))
            (setq state 'partial)
          (let ((key (ygg-call-graph--node-key (ygg-call-graph--intern item))))
            (unless (member key keys) (setq keys (append keys (list key))))))))
    (if incoming
        (setf (ygg-call-graph--node-callers node) keys
              (ygg-call-graph--node-callers-state node) state)
      (setf (ygg-call-graph--node-callees node) keys
            (ygg-call-graph--node-callees-state node) state))
    keys))

(defun ygg-call-graph--walk ()
  "Walk both sides from the root, breadth-first, to the depth and the cap."
  (let ((queue (list (list 'callers ygg-call-graph--root 0)
                     (list 'callees ygg-call-graph--root 0)))
        (queued (make-hash-table :test #'equal)))
    (ygg-call-graph--start-clock)
    (puthash (cons 'callers ygg-call-graph--root) t queued)
    (puthash (cons 'callees ygg-call-graph--root) t queued)
    (while queue
      (pcase-let* ((`(,side ,key ,level) (pop queue))
                   (node (ygg-call-graph--node key)))
        (when (and (or (ygg-call-graph--all-p) (< level ygg-call-graph--walk-depth))
                   (not (ygg-call-graph--node-library node))
                   (not (ygg-call-graph--state node side))
                   (< (ygg-call-graph--project-count) (ygg-call-graph--cap)))
          (dolist (child (ygg-call-graph--fetch node side (ygg-call-graph--cap)))
            (unless (gethash (cons side child) queued)
              (puthash (cons side child) t queued)
              (setq queue (append queue (list (list side child (1+ level))))))))))))

(defun ygg-call-graph--reset (root-item)
  (setq ygg-call-graph--nodes (make-hash-table :test #'equal)
        ygg-call-graph--folded (make-hash-table :test #'equal)
        ygg-call-graph--root (ygg-call-graph--node-key
                              (ygg-call-graph--intern root-item)))
  (setf (ygg-call-graph--node-library (ygg-call-graph--node ygg-call-graph--root)) nil)
  (ygg-call-graph--walk))

;;; Rendering

(defun ygg-call-graph--shown-p (key)
  (or ygg-call-graph--show-library
      (equal key ygg-call-graph--root)
      (not (ygg-call-graph--node-library (ygg-call-graph--node key)))))

(defun ygg-call-graph--matches-p (side key seen)
  "Whether KEY or a SIDE descendant not in SEEN has the filter in its name."
  (or (null ygg-call-graph--filter)
      (string-match-p (regexp-quote ygg-call-graph--filter)
                      (ygg-call-graph--node-name (ygg-call-graph--node key)))
      (and (not (gethash key seen))
           (progn (puthash key t seen) t)
           (seq-some (lambda (child)
                       (and (ygg-call-graph--shown-p child)
                            (ygg-call-graph--matches-p side child seen)))
                     (ygg-call-graph--children (ygg-call-graph--node key) side)))))

(defun ygg-call-graph--visible-children (side key)
  (seq-filter (lambda (child)
                (and (ygg-call-graph--shown-p child)
                     (ygg-call-graph--matches-p side child (make-hash-table :test #'equal))))
              (ygg-call-graph--children (ygg-call-graph--node key) side)))

(defun ygg-call-graph--location (node)
  (let ((path (ygg-call-graph--node-path node)))
    (format "%s:%d"
            (cond ((null path) (or (plist-get (ygg-call-graph--node-item node) :uri) "?"))
                  ((and ygg-call-graph--project-root
                        (file-in-directory-p path ygg-call-graph--project-root))
                   (file-relative-name path ygg-call-graph--project-root))
                  (t (file-name-nondirectory path)))
            (ygg-call-graph--node-line node))))

(defun ygg-call-graph--detail (node)
  (when-let* ((detail (ygg-call-graph--node-detail node))
              ((stringp detail))
              ((not (string-empty-p detail))))
    (truncate-string-to-width (car (split-string detail "\n")) 28 nil nil "…")))

(defun ygg-call-graph--row (side key parent prefix connector flag)
  "One row: (LEFT-PARTS LOCATION FLAG SIDE KEY)."
  (let* ((node (ygg-call-graph--node key))
         (name (ygg-call-graph--node-name node))
         (detail (ygg-call-graph--detail node)))
    (list (list (propertize (concat prefix connector) 'face 'shadow)
                (propertize name 'ygg-call-graph-name t)
                (when (eq side 'callers)
                  (propertize (concat " ─▶ " (ygg-call-graph--node-name
                                               (ygg-call-graph--node parent)))
                              'face 'shadow))
                (when detail (propertize (concat "  " detail) 'face 'shadow)))
          (ygg-call-graph--location node) flag side key)))

(defun ygg-call-graph--side-rows (side)
  (let ((seen (make-hash-table :test #'equal))
        rows)
    (puthash ygg-call-graph--root t seen)
    (cl-labels
        ((walk (parent prefix path)
           (let ((children (ygg-call-graph--visible-children side parent)))
             (while children
               (let* ((key (pop children))
                      (node (ygg-call-graph--node key))
                      (last (null children))
                      (connector (concat (if last "└─" "├─")
                                         (if (eq side 'callers) " " "▶ ")))
                      (state (ygg-call-graph--state node side))
                      (flag (cond ((member key path) "cycle")
                                  ((gethash key seen) "repeat")
                                  ((gethash (cons side key) ygg-call-graph--folded)
                                   "folded")
                                  ((not (eq state t)) "more"))))
                 (push (ygg-call-graph--row side key parent prefix connector flag) rows)
                 (puthash key t seen)
                 (unless flag
                   (walk key (concat prefix (if last "   " "│  "))
                         (cons key path))))))))
      (walk ygg-call-graph--root "" (list ygg-call-graph--root)))
    (nreverse rows)))

(defun ygg-call-graph--row-width (row)
  (string-width (apply #'concat (delq nil (copy-sequence (car row))))))

(defun ygg-call-graph--insert-row (row column)
  (pcase-let* ((`(,parts ,location ,flag ,side ,key) row)
               (start (point)))
    (apply #'insert (delq nil (copy-sequence parts)))
    (insert (make-string (max 2 (- column (ygg-call-graph--row-width row))) ?\s)
            (propertize location 'face 'shadow)
            (if flag (propertize (concat "  " flag) 'face 'shadow) "")
            "\n")
    (put-text-property start (point) 'ygg-call-graph-row (cons side key))))

(defun ygg-call-graph--section (title rows column side)
  (insert (propertize title 'face 'shadow) "\n")
  (let ((node (ygg-call-graph--node ygg-call-graph--root)))
    (cond (rows (dolist (row rows) (ygg-call-graph--insert-row row column)))
          ((eq (ygg-call-graph--state node side) t)
           (insert (propertize "  none" 'face 'shadow) "\n"))
          (t (insert (propertize "  not fetched" 'face 'shadow) "\n")))))

(defun ygg-call-graph--render ()
  (let* ((inhibit-read-only t)
         (here (get-text-property (point) 'ygg-call-graph-row))
         (callers (ygg-call-graph--side-rows 'callers))
         (callees (ygg-call-graph--side-rows 'callees))
         (root (ygg-call-graph--row 'root ygg-call-graph--root nil "" "" nil))
         (column (min 52 (+ 2 (apply #'max 0 (mapcar #'ygg-call-graph--row-width
                                                     (cons root (append callers callees))))))))
    (setf (nth 1 (car root))
          (propertize (ygg-call-graph--node-name (ygg-call-graph--node ygg-call-graph--root))
                      'face 'bold 'ygg-call-graph-name t))
    (erase-buffer)
    (ygg-call-graph--section "callers" callers column 'callers)
    (insert "\n")
    (ygg-call-graph--insert-row root column)
    (insert "\n")
    (ygg-call-graph--section "callees" callees column 'callees)
    (ygg-call-graph--goto-row (or here (cons 'root ygg-call-graph--root)))))

(defun ygg-call-graph--goto-row (row)
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (if (equal (get-text-property (point) 'ygg-call-graph-row) row)
          (setq found t)
        (forward-line 1)))
    (if found (ygg-call-graph--to-name)
      (ygg-call-graph--goto-row-fallback))))

(defun ygg-call-graph--goto-row-fallback ()
  (goto-char (point-min))
  (let ((root (cons 'root ygg-call-graph--root)))
    (while (and (not (eobp))
                (not (equal (get-text-property (point) 'ygg-call-graph-row) root)))
      (forward-line 1))
    (ygg-call-graph--to-name)))

(defun ygg-call-graph--to-name ()
  (let ((name (text-property-any (line-beginning-position) (line-end-position)
                                 'ygg-call-graph-name t)))
    (when name (goto-char name))))

(defun ygg-call-graph--header ()
  (let ((root (ygg-call-graph--node ygg-call-graph--root)))
    (concat " " (propertize (ygg-call-graph--node-name root) 'face 'bold)
            (propertize
             (format "  %s  depth %s  %d nodes  library calls %s%s"
                     (ygg-call-graph--location root) ygg-call-graph--walk-depth
                     (let ((n 0))
                       (maphash (lambda (key _) (when (ygg-call-graph--shown-p key) (cl-incf n)))
                                ygg-call-graph--nodes)
                       n)
                     (if ygg-call-graph--show-library "shown" "hidden")
                     (if ygg-call-graph--filter
                         (format "  filter %s" ygg-call-graph--filter)
                       ""))
             'face 'shadow))))

;;; Commands

(defun ygg-call-graph--at-point ()
  (or (get-text-property (line-beginning-position) 'ygg-call-graph-row)
      (user-error "No node on this line")))

(defun ygg-call-graph-next ()
  "Move to the next node."
  (interactive)
  (let ((pos (save-excursion
               (forward-line 1)
               (while (and (not (eobp))
                           (not (get-text-property (point) 'ygg-call-graph-row)))
                 (forward-line 1))
               (and (not (eobp)) (point)))))
    (when pos (goto-char pos) (ygg-call-graph--to-name))))

(defun ygg-call-graph-previous ()
  "Move to the previous node."
  (interactive)
  (let ((pos (save-excursion
               (forward-line 0)
               (let (found)
                 (while (and (not found) (not (bobp)))
                   (forward-line -1)
                   (setq found (get-text-property (point) 'ygg-call-graph-row)))
                 (and found (point))))))
    (when pos (goto-char pos) (ygg-call-graph--to-name))))

(defun ygg-call-graph-visit ()
  "Show the source of the node at point in another window."
  (interactive)
  (let* ((node (ygg-call-graph--node (cdr (ygg-call-graph--at-point))))
         (path (or (ygg-call-graph--node-path node) (user-error "This node has no file")))
         (start (ygg-call-graph--item-start (ygg-call-graph--node-item node)))
         (window (display-buffer (find-file-noselect path)
                                 '(nil (inhibit-same-window . t)))))
    (select-window window)
    (goto-char (point-min))
    (forward-line (or (plist-get start :line) 0))
    (forward-char (min (or (plist-get start :character) 0)
                       (- (line-end-position) (point))))))

(defun ygg-call-graph-toggle ()
  "Fold or unfold the node at point; unfolding a cut node fetches its calls."
  (interactive)
  (pcase-let* ((`(,side . ,key) (ygg-call-graph--at-point))
               (node (ygg-call-graph--node key))
               (fold (cons side key)))
    (unless (eq side 'root)
      (cond ((gethash fold ygg-call-graph--folded)
             (remhash fold ygg-call-graph--folded))
            ((not (eq (ygg-call-graph--state node side) t))
             (ygg-call-graph--start-clock)
             (ygg-call-graph--fetch node side nil))
            (t (puthash fold t ygg-call-graph--folded)))
      (ygg-call-graph--render))))

(defun ygg-call-graph--name (node)
  (format "*call graph: %s*" (ygg-call-graph--node-name node)))

(defun ygg-call-graph-recentre ()
  "Make the node at point the root of the graph."
  (interactive)
  (let ((item (ygg-call-graph--node-item
               (ygg-call-graph--node (cdr (ygg-call-graph--at-point))))))
    (ygg-call-graph--reset item)
    (rename-buffer (ygg-call-graph--name (ygg-call-graph--node ygg-call-graph--root)) t)
    (goto-char (point-min))
    (ygg-call-graph--render)))

(defun ygg-call-graph-refresh ()
  "Walk the graph again from its root."
  (interactive)
  (ygg-call-graph--reset (ygg-call-graph--node-item
                          (ygg-call-graph--node ygg-call-graph--root)))
  (ygg-call-graph--render))

(defun ygg-call-graph-set-depth (depth)
  "Walk the graph again to DEPTH, 1 to 3, or 0 for every level."
  (interactive (list (- last-command-event ?0)))
  (setq ygg-call-graph--walk-depth (if (eq depth 0) 'all (max 1 (min 3 depth))))
  (ygg-call-graph-refresh))

(defun ygg-call-graph-unfold-all ()
  "Unfold every folded node."
  (interactive)
  (clrhash ygg-call-graph--folded)
  (ygg-call-graph--render))

(defun ygg-call-graph-toggle-library ()
  "Show or hide calls into libraries and the toolchain."
  (interactive)
  (setq ygg-call-graph--show-library (not ygg-call-graph--show-library))
  (ygg-call-graph--render))

(defun ygg-call-graph-filter (text)
  "Show only rows whose name holds TEXT, with the rows leading to them."
  (interactive (list (read-string "Filter calls by name (empty clears): "
                                  ygg-call-graph--filter)))
  (setq ygg-call-graph--filter (unless (string-empty-p text) text))
  (ygg-call-graph--render))

(defconst ygg-call-graph--keys
  '("j" ygg-call-graph-next "k" ygg-call-graph-previous
    "RET" ygg-call-graph-visit "o" ygg-call-graph-visit
    "TAB" ygg-call-graph-toggle "<tab>" ygg-call-graph-toggle
    "C" ygg-call-graph-recentre "1" ygg-call-graph-set-depth
    "2" ygg-call-graph-set-depth "3" ygg-call-graph-set-depth
    "0" ygg-call-graph-set-depth "A" ygg-call-graph-unfold-all
    "L" ygg-call-graph-toggle-library "/" ygg-call-graph-filter
    "g" ygg-call-graph-refresh "q" quit-window)
  "The graph's keys, for its mode map and for yggdrasil's normal state.")

(defvar ygg-call-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (cl-loop for (key def) on ygg-call-graph--keys by #'cddr
             do (keymap-set map key def))
    map))

(define-derived-mode ygg-call-graph-mode special-mode "Call-Graph"
  "The callers and callees of one symbol, as trees around it."
  (setq truncate-lines t
        header-line-format '(:eval (ygg-call-graph--header))))

(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-call-graph-mode)
  (apply #'yggdrasil-define-mode-keys 'ygg-call-graph-mode 'normal ygg-call-graph--keys))

;;;###autoload
(defun ygg-call-graph ()
  "Show the callers and callees of the symbol at point as a text graph."
  (interactive)
  (let* ((server (ygg-lsp-calls--server :callHierarchyProvider "call hierarchy"))
         (project (project-current))
         (root-dir (expand-file-name (if project (project-root project) default-directory)))
         (params (eglot--TextDocumentPositionParams))
         (buffer (generate-new-buffer "*call graph*")))
    (condition-case err
        (with-current-buffer buffer
          (ygg-call-graph-mode)
          (setq ygg-call-graph--server server
                ygg-call-graph--project-root root-dir
                ygg-call-graph--walk-depth ygg-call-graph-depth)
          (ygg-call-graph--start-clock)
          (let ((items (ygg-call-graph--request :textDocument/prepareCallHierarchy params)))
            (when (seq-empty-p items) (user-error "No call hierarchy here"))
            (ygg-call-graph--reset (seq-first items)))
          (let ((name (ygg-call-graph--name (ygg-call-graph--node ygg-call-graph--root))))
            (when-let* ((old (get-buffer name))) (kill-buffer old))
            (rename-buffer name))
          (ygg-call-graph--render))
      (error (kill-buffer buffer) (signal (car err) (cdr err))))
    (pop-to-buffer buffer)))

(provide 'ygg-call-graph)
;;; ygg-call-graph.el ends here
