;;; call-graph-tests.el --- The call graph over a fake call hierarchy -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'ygg-call-graph)

(declare-function yggdrasil-global-mode "yggdrasil-core")

(defvar call-graph-tests--dir nil)

(defun call-graph-tests--item (name file line &optional detail)
  (list :name name
        :uri (if (string-prefix-p "/" file) (concat "file://" file)
               (concat "file://" call-graph-tests--dir file))
        :detail detail
        :selectionRange (list :start (list :line (1- line) :character 4)
                              :end (list :line (1- line) :character 8))))

(defun call-graph-tests--graph ()
  "Callers and callees by name: a cycle through checkout, main reached
twice, print in a library, and sum calling more than the cap allows."
  (let* ((it #'call-graph-tests--item)
         (total (funcall it "total" "a.py" 10 "Cart"))
         (checkout (funcall it "checkout" "a.py" 20 "Cart"))
         (refund (funcall it "refund" "b.py" 5))
         (main (funcall it "main" "b.py" 1))
         (sum (funcall it "sum" "b.py" 30 "math"))
         (boot (funcall it "boot" "b.py" 55))
         (start (funcall it "start" "b.py" 56))
         (init (funcall it "init" "b.py" 57))
         (print (funcall it "print" "/usr/lib/python3/typeshed/builtins.pyi" 100))
         (small (cl-loop for i from 1 to 8
                         collect (funcall it (format "s%d" i) "b.py" (+ 40 i)))))
    (list :root total
          :callers `(("total" ,checkout ,refund) ("checkout" ,main ,total)
                     ("refund" ,main) ("main" ,boot) ("boot" ,start)
                     ("start" ,init) ("init" ,total))
          :callees `(("total" ,sum ,print ,checkout) ("sum" ,@small)
                     ("checkout" ,total) ("refund" ,total) ("main" ,checkout ,refund)
                     ("boot" ,main) ("start" ,boot) ("init" ,start ,total)))))

(defmacro call-graph-tests--with (&rest body)
  "Run BODY with the fake server, two real files, and a source buffer."
  (declare (indent 0))
  `(let* ((call-graph-tests--dir (file-name-as-directory (make-temp-file "cg" t)))
          (graph (call-graph-tests--graph))
          (requests nil)
          (timeout nil)
          (ygg-call-graph-depth 2)
          (ygg-call-graph-max-nodes 60))
     (with-temp-file (concat call-graph-tests--dir "a.py")
       (dotimes (i 30) (insert (format "line a%d\n" (1+ i)))))
     (with-temp-file (concat call-graph-tests--dir "b.py")
       (dotimes (i 60) (insert (format "line b%d\n" (1+ i)))))
     (unwind-protect
         (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                   ((symbol-function 'eglot-current-server) (lambda () 'server))
                   ((symbol-function 'eglot-server-capable)
                    (lambda (&rest feats) (eq (car feats) :callHierarchyProvider)))
                   ((symbol-function 'eglot--TextDocumentPositionParams)
                    (lambda () '(:position (:line 9 :character 5))))
                   ((symbol-function 'eglot-uri-to-path)
                    (lambda (uri) (string-remove-prefix "file://" uri)))
                   ((symbol-function 'project-current) (lambda (&rest _) nil))
                   ((symbol-function 'jsonrpc-request)
                    (lambda (_server method params &rest _)
                      (when timeout
                        (signal 'jsonrpc-error '("request id=7 failed:"
                                                (jsonrpc-error-message . "Timed out"))))
                      (let ((name (plist-get (plist-get params :item) :name)))
                        (push (cons method name) requests)
                        (pcase method
                          (:textDocument/prepareCallHierarchy
                           (vector (plist-get graph :root)))
                          (:callHierarchy/incomingCalls
                           (vconcat (mapcar (lambda (from) (list :from from :fromRanges []))
                                            (cdr (assoc name (plist-get graph :callers))))))
                          (:callHierarchy/outgoingCalls
                           (vconcat (mapcar (lambda (to) (list :to to :fromRanges []))
                                            (cdr (assoc name (plist-get graph :callees)))))))))))
           (save-window-excursion
             (delete-other-windows)
             (with-temp-buffer
               (setq default-directory call-graph-tests--dir)
               ,@body)))
       (dolist (buf (buffer-list))
         (when (string-prefix-p "*call graph" (buffer-name buf)) (kill-buffer buf))
         (when (and (buffer-file-name buf)
                    (string-prefix-p call-graph-tests--dir (buffer-file-name buf)))
           (kill-buffer buf)))
       (delete-directory call-graph-tests--dir t))))

(defun call-graph-tests--open ()
  (ygg-call-graph)
  (get-buffer "*call graph: total*"))

(defun call-graph-tests--nodes-named (name)
  (let (found)
    (maphash (lambda (_ node) (when (equal (ygg-call-graph--node-name node) name)
                                (push node found)))
             ygg-call-graph--nodes)
    found))

(defun call-graph-tests--goto (text)
  "Put point on the name of the first row holding TEXT."
  (goto-char (point-min))
  (should (search-forward text nil t))
  (ygg-call-graph--to-name))

(defun call-graph-tests--root-line ()
  (save-excursion
    (ygg-call-graph--goto-row (cons 'root ygg-call-graph--root))
    (line-number-at-pos)))

(defun call-graph-tests--line-of (text)
  (save-excursion
    (goto-char (point-min))
    (and (search-forward text nil t) (line-number-at-pos))))

(ert-deftest call-graph-walks-each-side-to-the-depth ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (let ((checkout (car (call-graph-tests--nodes-named "checkout")))
            (main (car (call-graph-tests--nodes-named "main"))))
        (should (eq (ygg-call-graph--node-callers-state checkout) t))
        (should-not (ygg-call-graph--node-callers-state main))
        (should-not (member '(:callHierarchy/incomingCalls . "main") requests)))
      (ygg-call-graph-set-depth 1)
      (should-not (ygg-call-graph--node-callers-state
                   (car (call-graph-tests--nodes-named "checkout"))))
      (should-not (call-graph-tests--nodes-named "main")))))

(ert-deftest call-graph-merges-a-symbol-reached-twice ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (let ((main (call-graph-tests--nodes-named "main")))
        (should (= 1 (length main)))
        (dolist (parent '("checkout" "refund"))
          (should (member (ygg-call-graph--node-key (car main))
                          (ygg-call-graph--node-callers
                           (car (call-graph-tests--nodes-named parent)))))))
      (should (string-match-p "main ─▶ refund.*repeat" (buffer-string))))))

(ert-deftest call-graph-survives-a-cycle ()
  (call-graph-tests--with
    (let ((ygg-call-graph-depth 3))
      (with-current-buffer (call-graph-tests--open)
        (should (= 1 (length (call-graph-tests--nodes-named "total"))))
        (should (string-match-p "total ─▶ checkout.*cycle" (buffer-string)))
        (should (= 1 (cl-count '(:callHierarchy/incomingCalls . "checkout") requests
                               :test #'equal)))))))

(ert-deftest call-graph-caps-the-nodes-and-marks-the-cut-ones ()
  (call-graph-tests--with
    (let ((ygg-call-graph-max-nodes 8))
      (with-current-buffer (call-graph-tests--open)
        (should (<= (ygg-call-graph--project-count) 8))
        (let ((sum (car (call-graph-tests--nodes-named "sum"))))
          (should (eq (ygg-call-graph--node-callees-state sum) 'partial)))
        (should (string-match-p "─▶ sum .*more" (buffer-string)))
        (should-not (string-match-p "s8" (buffer-string)))))))

(ert-deftest call-graph-hides-library-calls-until-asked ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (should (call-graph-tests--nodes-named "print"))
      (should-not (string-match-p "print" (buffer-string)))
      (should (string-match-p "library calls hidden" (ygg-call-graph--header)))
      (funcall (keymap-lookup ygg-call-graph-mode-map "L"))
      (should (string-match-p "─▶ print .*builtins.pyi:100" (buffer-string)))
      (should (string-match-p "library calls shown" (ygg-call-graph--header)))
      (funcall (keymap-lookup ygg-call-graph-mode-map "L"))
      (should-not (string-match-p "print" (buffer-string))))))

(ert-deftest call-graph-draws-callers-above-the-root-above-the-callees ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (let ((callers (call-graph-tests--line-of "callers"))
            (checkout (call-graph-tests--line-of "checkout ─▶ total"))
            (root (call-graph-tests--root-line))
            (callees (call-graph-tests--line-of "callees"))
            (sum (call-graph-tests--line-of "─▶ sum")))
        (should (and callers checkout root callees sum))
        (should (< callers checkout root callees sum)))
      (should (string-match-p "├─ checkout ─▶ total  Cart +a\\.py:20" (buffer-string)))
      (should (string-match-p "│  ├─ main ─▶ checkout +b\\.py:1  more" (buffer-string)))
      (should (string-match-p "└─▶ checkout  Cart +a\\.py:20" (buffer-string)))
      (ygg-call-graph--goto-row (cons 'root ygg-call-graph--root))
      (should (looking-at "total"))
      (should (eq (get-text-property (point) 'face) 'bold))
      (should (string-match-p "total +a\\.py:10 +depth 2 +[0-9]+ nodes"
                              (ygg-call-graph--header))))))

(ert-deftest call-graph-moves-between-nodes ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (should (equal (ygg-call-graph--at-point) (cons 'root ygg-call-graph--root)))
      (ygg-call-graph-previous)
      (should (looking-at "main ─▶ refund"))
      (ygg-call-graph-next)
      (should (looking-at "total"))
      (ygg-call-graph-next)
      (should (looking-at "sum")))))

(ert-deftest call-graph-folds-and-unfolds-a-subtree ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (call-graph-tests--goto "checkout ─▶ total")
      (funcall (keymap-lookup ygg-call-graph-mode-map "TAB"))
      (should (looking-at "checkout"))
      (should-not (string-match-p "main ─▶ checkout" (buffer-string)))
      (should (string-match-p "checkout ─▶ total.*folded" (buffer-string)))
      (ygg-call-graph-toggle)
      (should (string-match-p "main ─▶ checkout.*more" (buffer-string)))
      (call-graph-tests--goto "main ─▶ checkout")
      (ygg-call-graph-toggle)
      (should (member '(:callHierarchy/incomingCalls . "main") requests))
      (should (eq (ygg-call-graph--node-callers-state
                   (car (call-graph-tests--nodes-named "main")))
                  t))
      (should-not (string-match-p "main ─▶ checkout.*more" (buffer-string))))))

(ert-deftest call-graph-recentres-on-the-node-at-point ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (call-graph-tests--goto "refund ─▶ total")
      (funcall (keymap-lookup ygg-call-graph-mode-map "C"))
      (should (equal (buffer-name) "*call graph: refund*"))
      (should (equal (ygg-call-graph--node-name (ygg-call-graph--node ygg-call-graph--root))
                     "refund"))
      (should (string-match-p "main ─▶ refund" (buffer-string)))
      (should (string-match-p "─▶ total" (buffer-string)))
      (should (< (call-graph-tests--line-of "main ─▶ refund")
                 (call-graph-tests--root-line)
                 (call-graph-tests--line-of "─▶ total"))))))

(ert-deftest call-graph-jumps-to-the-source-in-another-window ()
  (call-graph-tests--with
    (let ((graph (call-graph-tests--open)))
      (pop-to-buffer graph)
      (call-graph-tests--goto "─▶ sum")
      (funcall (keymap-lookup ygg-call-graph-mode-map "RET"))
      (should (equal buffer-file-name (concat call-graph-tests--dir "b.py")))
      (should (= (line-number-at-pos) 30))
      (should (looking-at " b30"))
      (should (get-buffer-window graph))
      (should-not (eq (get-buffer-window graph) (selected-window))))))

(ert-deftest call-graph-depth-keys-walk-again ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (should (eq (keymap-lookup ygg-call-graph-mode-map "3") 'ygg-call-graph-set-depth))
      (let ((last-command-event ?3))
        (call-interactively #'ygg-call-graph-set-depth))
      (should (= ygg-call-graph--walk-depth 3))
      (should (member '(:callHierarchy/incomingCalls . "main") requests))
      (should (string-match-p "depth 3" (ygg-call-graph--header)))
      (let ((last-command-event ?1))
        (call-interactively #'ygg-call-graph-set-depth))
      (should-not (string-match-p "main" (buffer-string)))
      (should (string-match-p "checkout ─▶ total.*more" (buffer-string))))))

(ert-deftest call-graph-depth-all-walks-until-nothing-new ()
  (call-graph-tests--with
    (let ((ygg-call-graph-max-nodes 2))
      (with-current-buffer (call-graph-tests--open)
        (should-not (string-match-p "boot" (buffer-string)))
        (let ((last-command-event ?0))
          (call-interactively (keymap-lookup ygg-call-graph-mode-map "0")))
        (should (eq ygg-call-graph--walk-depth 'all))
        (should (string-match-p "depth all" (ygg-call-graph--header)))
        (should (string-match-p "init ─▶ start" (buffer-string)))
        (should (string-match-p "total ─▶ init.*cycle" (buffer-string)))
        (should (eq (ygg-call-graph--node-callers-state
                     (car (call-graph-tests--nodes-named "init")))
                    t))
        (should (= 1 (cl-count '(:callHierarchy/incomingCalls . "init") requests
                               :test #'equal)))
        (should-not (string-match-p "more\\|folded" (buffer-string)))
        (let ((last-command-event ?2))
          (call-interactively #'ygg-call-graph-set-depth))
        (should (string-match-p "depth 2" (ygg-call-graph--header)))
        (should-not (string-match-p "boot" (buffer-string)))))))

(ert-deftest call-graph-depth-all-has-its-own-cap ()
  (call-graph-tests--with
    (let ((ygg-call-graph-all-max-nodes 6))
      (with-current-buffer (call-graph-tests--open)
        (ygg-call-graph-set-depth 0)
        (should (<= (ygg-call-graph--project-count) 6))
        (should (string-match-p "more" (buffer-string)))
        (should-not (string-match-p "init" (buffer-string)))))))

(ert-deftest call-graph-depth-all-names-its-own-timeout ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (setq timeout t)
      (let ((err (should-error (ygg-call-graph-set-depth 0) :type 'user-error)))
        (should (string-match-p "30s (ygg-call-graph-all-timeout)" (cadr err)))))))

(ert-deftest call-graph-unfolds-everything-without-fetching ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (call-graph-tests--goto "checkout ─▶ total")
      (ygg-call-graph-toggle)
      (call-graph-tests--goto "─▶ sum")
      (ygg-call-graph-toggle)
      (should (= 2 (hash-table-count ygg-call-graph--folded)))
      (should-not (string-match-p "s1\\|main ─▶ checkout" (buffer-string)))
      (let ((before (length requests)))
        (funcall (keymap-lookup ygg-call-graph-mode-map "A"))
        (should (= before (length requests))))
      (should (= 0 (hash-table-count ygg-call-graph--folded)))
      (should-not (string-match-p "folded" (buffer-string)))
      (should (string-match-p "main ─▶ checkout" (buffer-string)))
      (should (string-match-p "─▶ s1" (buffer-string))))))

(ert-deftest call-graph-filters-rows-by-name ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (ygg-call-graph-filter "main")
      (should (string-match-p "checkout ─▶ total" (buffer-string)))
      (should (string-match-p "main ─▶ checkout" (buffer-string)))
      (should-not (string-match-p "sum" (buffer-string)))
      (ygg-call-graph-filter "")
      (should (string-match-p "sum" (buffer-string))))))

(ert-deftest call-graph-refresh-walks-again ()
  (call-graph-tests--with
    (with-current-buffer (call-graph-tests--open)
      (setq requests nil)
      (funcall (keymap-lookup ygg-call-graph-mode-map "g"))
      (should (member '(:callHierarchy/incomingCalls . "total") requests)))))

(ert-deftest call-graph-names-the-timeout-when-the-server-is-silent ()
  (call-graph-tests--with
    (setq timeout t)
    (let ((err (should-error (ygg-call-graph) :type 'user-error)))
      (should (string-match-p "ygg-call-graph-timeout" (cadr err))))
    (should-not (seq-some (lambda (buf) (string-prefix-p "*call graph" (buffer-name buf)))
                          (buffer-list)))))

(ert-deftest call-graph-refuses-without-a-server ()
  (call-graph-tests--with
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil)))
      (should-error (ygg-call-graph) :type 'user-error))
    (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) nil)))
      (should-error (ygg-call-graph) :type 'user-error))))

(ert-deftest call-graph-normal-state-keeps-the-graph-keys ()
  (skip-unless (and (require 'yggdrasil nil t) (require 'yggdrasil-localleader nil t)))
  (yggdrasil-global-mode 1)
  (unwind-protect
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (ygg-call-graph-mode)
        (run-hooks 'after-change-major-mode-hook)
        (should (eq (bound-and-true-p ygg--state) 'normal))
        (dolist (spec '(("j" . ygg-call-graph-next) ("k" . ygg-call-graph-previous)
                        ("TAB" . ygg-call-graph-toggle) ("<tab>" . ygg-call-graph-toggle)
                        ("RET" . ygg-call-graph-visit) ("o" . ygg-call-graph-visit)
                        ("C" . ygg-call-graph-recentre) ("1" . ygg-call-graph-set-depth)
                        ("2" . ygg-call-graph-set-depth) ("3" . ygg-call-graph-set-depth)
                        ("0" . ygg-call-graph-set-depth) ("A" . ygg-call-graph-unfold-all)
                        ("L" . ygg-call-graph-toggle-library) ("/" . ygg-call-graph-filter)
                        ("g" . ygg-call-graph-refresh)))
          (should (eq (key-binding (kbd (car spec))) (cdr spec))))
        (should (memq (key-binding (kbd "q")) '(quit-window ygg-space-quit)))
        (should (keymapp (key-binding (kbd "SPC"))))
        (should-not (eq (key-binding (kbd "SPC")) 'scroll-up-command)))
    (yggdrasil-global-mode -1)))

(provide 'call-graph-tests)
;;; call-graph-tests.el ends here
