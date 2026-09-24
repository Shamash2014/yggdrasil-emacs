;;; kernel-vars-tests.el --- Tests for the kernel variables pane -*- lexical-binding: t; -*-

;; Ark fixtures follow posit-dev/ark 0.1.252:
;; crates/amalthea/src/comm/variables_comm.rs (Variable, RefreshParams,
;; UpdateParams, InspectedVariable, the method-tagged event enum),
;; crates/ark/src/variables/variable.rs:503 (the "data.frame [10, 1]" type),
;; crates/ark/src/variables/r_variables.rs:511 and 588 (refresh and update).
;; They are written as emacs-jupyter decodes JSON: plists, vectors, :json-false.
;; The live fixtures were captured from the kernel, as decoded.

(require 'ert)
(require 'json)
(require 'ygg-kernel-vars)

(defun kv-test--ark-var (name type value kind has-children &optional length)
  (list :access_key name :display_name name :display_value value
        :display_type type :type_info "" :size 56 :kind kind
        :length (or length 1) :has_children (if has-children t :json-false)
        :has_viewer :json-false :is_truncated :json-false :updated_time 0))

(defconst kv-test--refresh
  (list :method "refresh"
        :params (list :variables
                      (vector (kv-test--ark-var "x" "dbl" "42" "number" nil)
                              (kv-test--ark-var "d" "data.frame [10, 1]"
                                                "[10 rows x 1 columns] <data.frame>"
                                                "table" t 1))
                      :length 2 :version 1)))

(defconst kv-test--update
  (list :method "update"
        :params (list :assigned (vector (kv-test--ark-var "y" "chr" "\"hi\"" "string" nil))
                      :unevaluated []
                      :removed (vector "x")
                      :version 2)))

(defconst kv-test--live-update
  '(:method "update" :params (:assigned [(:access_key "y" :display_name "y" :display_value "\"hi\"" :display_type "str" :type_info "str" :size 112 :kind "string" :length 1 :has_children :json-false :has_viewer :json-false :is_truncated :json-false :updated_time 1790191844202)] :unevaluated [] :removed ["x"] :version 4))
  "Captured from ark 0.1.252 after y <- \"hi\"; rm(x).")

(defconst kv-test--live-inspect-reply
  '(:method "InspectReply" :result (:children [(:access_key "0" :display_name "a" :display_value "1 2 3 4 5 6 7 8 9 10" :display_type "int [10]" :type_info "int [10]base::compact_intseq" :size 0 :kind "collection" :length 10 :has_children t :has_viewer :json-false :is_truncated :json-false :updated_time 1790191842167)] :length 1))
  "Captured from ark 0.1.252: the reply to inspect with path [\"d\"].")

(defconst kv-test--live-list-reply
  '(:method "ListReply" :result (:variables [(:access_key "d" :display_name "d" :display_value "[10 rows x 1 column] <data.frame>" :display_type "data.frame [10, 1]" :type_info "" :size 1360 :kind "table" :length 1 :has_children t :has_viewer t :is_truncated :json-false :updated_time 1790191852395) (:access_key "y" :display_name "y" :display_value "\"hi\"" :display_type "str" :type_info "str" :size 112 :kind "string" :length 1 :has_children :json-false :has_viewer :json-false :is_truncated :json-false :updated_time 1790191852395)] :length 2 :version 4))
  "Captured from ark 0.1.252: the reply to list.")

(defun kv-test--decode (json)
  (let ((json-object-type 'plist) (json-array-type 'vector))
    (json-read-from-string json)))

(defun kv-test--names (vars) (mapcar (lambda (v) (plist-get v :name)) vars))

(ert-deftest kv-ark-refresh-replaces-every-variable ()
  (let ((vars (ygg-kernel-vars--apply-event
               '((:key "old" :name "old"))
               (ygg-kernel-vars--ark-event kv-test--refresh))))
    (should (equal (kv-test--names vars) '("d" "x")))
    (let ((d (car vars)))
      (should (equal (plist-get d :type) "data.frame [10, 1]"))
      (should (equal (plist-get d :kind) "table"))
      (should (eq (plist-get d :has-children) t)))
    (should-not (plist-get (cadr vars) :has-children))))

(ert-deftest kv-ark-update-merges-assigned-and-removed ()
  (let* ((vars (ygg-kernel-vars--apply-event
                nil (ygg-kernel-vars--ark-event kv-test--refresh)))
         (vars (ygg-kernel-vars--apply-event
                vars (ygg-kernel-vars--ark-event kv-test--update))))
    (should (equal (kv-test--names vars) '("d" "y")))))

(ert-deftest kv-ark-update-replaces-a-reassigned-variable ()
  (let* ((vars (ygg-kernel-vars--apply-event
                nil (ygg-kernel-vars--ark-event kv-test--refresh)))
         (again (list :method "update"
                      :params (list :assigned (vector (kv-test--ark-var "x" "dbl" "7" "number" nil))
                                    :unevaluated [] :removed [] :version 3)))
         (vars (ygg-kernel-vars--apply-event vars (ygg-kernel-vars--ark-event again))))
    (should (equal (kv-test--names vars) '("d" "x")))
    (should (equal (plist-get (cadr vars) :value) "7"))))

(ert-deftest kv-ark-live-messages-parse ()
  (let* ((vars (ygg-kernel-vars--apply-event
                nil (cons 'refresh (ygg-kernel-vars--rpc-result kv-test--live-list-reply))))
         (after (ygg-kernel-vars--apply-event
                 (append vars '((:key "x" :name "x")))
                 (ygg-kernel-vars--ark-event kv-test--live-update)))
         (kids (ygg-kernel-vars--ark-children
                (ygg-kernel-vars--rpc-result kv-test--live-inspect-reply))))
    (should (equal (kv-test--names vars) '("d" "y")))
    (should (plist-get (car vars) :has-children))
    (should-not (plist-get (cadr vars) :has-children))
    (should (equal (kv-test--names after) '("d" "y")))
    (should (equal (plist-get (cadr after) :value) "\"hi\""))
    (should (equal (plist-get (cadr after) :type) "str"))
    (should (equal (kv-test--names kids) '("a")))
    (should (equal (plist-get (car kids) :key) "0"))))

(ert-deftest kv-truncated-children-end-with-a-count ()
  (let ((kids (ygg-kernel-vars--python-children
               '(:children [(:key "i:0" :name "[0]" :type "int" :value "1")] :length 5))))
    (should (= (length kids) 2))
    (should (equal (plist-get (cadr kids) :value) "4 more"))
    (should (plist-get (cadr kids) :more))))

(ert-deftest kv-ark-ignores-other-comm-data ()
  (should-not (ygg-kernel-vars--ark-event '(:method "server_started" :params nil)))
  (should-not (ygg-kernel-vars--ark-event '(:jsonrpc "2.0" :result nil))))

(ert-deftest kv-ark-rpc-result-and-error ()
  (should (equal (ygg-kernel-vars--rpc-result '(:jsonrpc "2.0" :result (:length 0)))
                 '(:length 0)))
  (should-error (ygg-kernel-vars--rpc-result
                 '(:jsonrpc "2.0" :error (:code -32603 :message "boom")))))

(ert-deftest kv-ark-inspect-children ()
  (let ((kids (ygg-kernel-vars--ark-children
               (list :children (vector (kv-test--ark-var "a" "int [10]" "1 2 3 4 5 6 7 8 9 10"
                                                         "number" nil 10))
                     :length 1))))
    (should (equal (kv-test--names kids) '("a")))
    (should (equal (plist-get (car kids) :type) "int [10]"))))

(defconst kv-test--python-list-reply
  (list :status "ok"
        :data (list :text/plain
                    (json-encode
                     '((variables . [((key . "df") (name . "df") (type . "DataFrame")
                                      (shape . [1000 3]) (length . 1000)
                                      (value . "a b c") (kind . "table")
                                      (has_children . t))
                                     ((key . "a") (name . "a") (type . "int")
                                      (shape . nil) (length . 0) (value . "1")
                                      (kind . "number") (has_children . :json-false))])
                       (length . 2))))
        :metadata nil))

(ert-deftest kv-python-list-parses-helper-json ()
  (let ((vars (ygg-kernel-vars--python-list
               (ygg-kernel-vars--python-json kv-test--python-list-reply))))
    (should (equal (kv-test--names vars) '("a" "df")))
    (should (equal (plist-get (cadr vars) :type) "DataFrame [1000, 3]"))
    (should (equal (plist-get (car vars) :type) "int"))
    (should (plist-get (cadr vars) :has-children))
    (should-not (plist-get (car vars) :has-children))))

(ert-deftest kv-python-children-parse ()
  (let ((kids (ygg-kernel-vars--python-children
               (kv-test--decode
                "{\"children\":[{\"key\":\"i:0\",\"name\":\"x\",\"type\":\"Series int64\",\"shape\":[1000],\"length\":1000,\"value\":\"0 0\",\"kind\":\"collection\",\"has_children\":true}],\"length\":1}"))))
    (should (equal (plist-get (car kids) :type) "Series int64 [1000]"))
    (should (equal (plist-get (car kids) :key) "i:0"))))

(ert-deftest kv-python-helper-error-signals ()
  (should-error (ygg-kernel-vars--python-json
                 '(:status "error" :ename "NameError" :evalue "name '_ygg_kv' is not defined"))))

(ert-deftest kv-python-expressions-carry-paths ()
  (let ((exprs (ygg-kernel-vars--python-expressions '(("df") ("d" "i:0")))))
    (should (equal (plist-get exprs :list) "_ygg_kv.list()"))
    (should (string-match-p (regexp-quote "_ygg_kv.inspect([\"df\"],") (plist-get exprs :i0)))
    (should (string-match-p (regexp-quote "[\"d\",\"i:0\"]") (plist-get exprs :i1)))))

(defun kv-test--vars ()
  (ygg-kernel-vars--apply-event nil (ygg-kernel-vars--ark-event kv-test--refresh)))

(defun kv-test--render (width &optional expanded children filter)
  (mapcar #'substring-no-properties
          (ygg-kernel-vars-render (kv-test--vars)
                                  (or children (make-hash-table :test #'equal))
                                  (or expanded (make-hash-table :test #'equal))
                                  width filter)))

(ert-deftest kv-render-groups-under-headings ()
  (let ((lines (kv-test--render 60)))
    (should (equal (nth 0 lines) "DATA"))
    (should (string-match-p "\\`▸ d +data\\.frame \\[10, 1\\] +\\[10 rows" (nth 1 lines)))
    (should (equal (nth 2 lines) ""))
    (should (equal (nth 3 lines) "VALUES"))
    (should (string-match-p "\\`  x +dbl +42\\'" (nth 4 lines)))))

(ert-deftest kv-render-never-exceeds-width ()
  (dolist (width '(12 20 30 45 80))
    (dolist (line (kv-test--render width))
      (should (<= (string-width line) width)))))

(ert-deftest kv-render-truncates-long-values-with-ellipsis ()
  (let ((line (nth 1 (kv-test--render 36))))
    (should (= (string-width line) 36))
    (should (string-suffix-p "…" line))))

(ert-deftest kv-render-expand-and-collapse ()
  (let ((expanded (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal)))
    (puthash '("d") (list (list :key "a" :name "a" :type "int [10]"
                                :value "1 2 3" :kind "number"))
             children)
    (should (= (length (kv-test--render 60 expanded children)) 5))
    (puthash '("d") t expanded)
    (let ((lines (kv-test--render 60 expanded children)))
      (should (= (length lines) 6))
      (should (string-prefix-p "▾ d" (nth 1 lines)))
      (should (string-match-p "\\`    a +int \\[10\\] +1 2 3" (nth 2 lines))))
    (remhash '("d") expanded)
    (should (string-prefix-p "▸ d" (nth 1 (kv-test--render 60 expanded children))))))

(ert-deftest kv-render-rows-carry-their-paths ()
  (let* ((expanded (make-hash-table :test #'equal))
         (children (make-hash-table :test #'equal)))
    (puthash '("d") t expanded)
    (puthash '("d") (list (list :key "a" :name "a" :type "int" :value "1")) children)
    (let ((lines (ygg-kernel-vars-render (kv-test--vars) children expanded 60)))
      (should (equal (get-text-property 0 'ygg-kernel-vars-path (nth 2 lines))
                     '("d" "a"))))))

(ert-deftest kv-narrow-type-keeps-the-shape ()
  (should (equal (ygg-kernel-vars--fit-type "DataFrame [1000, 3]" 14) "Dat… [1000, 3]"))
  (should (equal (ygg-kernel-vars--fit-type "int" 6) "int   "))
  (should (= (string-width (ygg-kernel-vars--fit-type "data.frame [10, 1]" 5)) 5)))

(ert-deftest kv-narrow-pane-still-shows-the-shape ()
  (should (string-match-p "\\[10, 1\\]" (nth 1 (kv-test--render 24)))))

(ert-deftest kv-render-filter-narrows-by-name ()
  (let ((lines (kv-test--render 60 nil nil "X")))
    (should (equal (car lines) "VALUES"))
    (should (= (length lines) 2))))

(ert-deftest kv-toggle-path-expands-and-collapses ()
  (let* ((state (ygg-kernel-vars--make-state :backend 'ark))
         (asked nil))
    (cl-letf (((symbol-function 'ygg-kernel-vars--ark-inspect)
               (lambda (_state path) (push path asked)))
              ((symbol-function 'ygg-kernel-vars--redraw) #'ignore))
      (ygg-kernel-vars-toggle-path state '("d"))
      (should (gethash '("d") (ygg-kernel-vars--state-expanded state)))
      (should (equal asked '(("d"))))
      (ygg-kernel-vars-toggle-path state '("d"))
      (should-not (gethash '("d") (ygg-kernel-vars--state-expanded state))))))

(ert-deftest kv-pane-keys-cover-the-brief ()
  (dolist (spec '(("j" . ygg-kernel-vars-next) ("k" . ygg-kernel-vars-previous)
                  ("TAB" . ygg-kernel-vars-toggle-row) ("RET" . ygg-kernel-vars-view)
                  ("/" . ygg-kernel-vars-filter) ("g r" . ygg-kernel-vars-refresh)
                  ("q" . ygg-kernel-vars-quit)))
    (should (eq (keymap-lookup ygg-kernel-vars-mode-map (car spec)) (cdr spec)))))


(ert-deftest kv-modal-normal-state-keeps-the-pane-keys ()
  (skip-unless (and (require 'yggdrasil nil t) (require 'yggdrasil-localleader nil t)))
  (yggdrasil-global-mode 1)
  (unwind-protect
      (with-temp-buffer
        (set-window-buffer (selected-window) (current-buffer))
        (ygg-kernel-vars-mode)
        (run-hooks 'after-change-major-mode-hook)
        (should (eq (bound-and-true-p ygg--state) 'normal))
        (dolist (spec '(("j" . ygg-kernel-vars-next) ("k" . ygg-kernel-vars-previous)
                        ("TAB" . ygg-kernel-vars-toggle-row)
                        ("<tab>" . ygg-kernel-vars-toggle-row)
                        ("RET" . ygg-kernel-vars-view) ("/" . ygg-kernel-vars-filter)
                        ("g r" . ygg-kernel-vars-refresh) ("q" . ygg-kernel-vars-quit)))
          (should (eq (key-binding (kbd (car spec))) (cdr spec)))))
    (yggdrasil-global-mode -1)))

(ert-deftest kv-toggle-lives-under-the-jupyter-leader ()
  (skip-unless (require 'layer-notebook nil t))
  (should (eq (lookup-key ygg-leader-jupyter-map "v") 'ygg-kernel-vars-toggle))
  (dolist (mode '(python-mode python-ts-mode r-ts-mode jupyter-repl-mode))
    (should-not (eq (lookup-key (ygg-localleader--get-map mode) "v")
                    'ygg-kernel-vars-toggle))))

(defconst kv-test--python
  (expand-file-name "~/.local/share/jupyter-python/bin/python3"))

(defconst kv-test--python-namespace "
import builtins, pandas
class Shell:
    user_ns = {}
    user_ns_hidden = {}
builtins.get_ipython = lambda: Shell
DataFrame = type('DataFrame', (), {'__module__': 'dask.dataframe.core',
                                   'shape': property(lambda self: (object(), 2))})
class Boom:
    @property
    def shape(self):
        raise RuntimeError('boom')
    def __repr__(self):
        raise RuntimeError('boom')
Shell.user_ns.update(frame=pandas.DataFrame({'a': [1, 2, 3], 'b': [4, 5, 6]}),
                     lazy=DataFrame(), boom=Boom(), n=7)
")

(defun kv-test--run-helper (expression)
  "Decode EXPRESSION's JSON from the shipped helper over a stub namespace."
  (let ((script (make-temp-file "kv" nil ".py")))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert kv-test--python-namespace ygg-kernel-vars--python-helper
                    "\n_ygg_kv = get_ipython().user_ns['_ygg_kv']"
                    "\nprint(repr(" expression "))\n"))
          (with-temp-buffer
            (should (eql 0 (call-process kv-test--python nil t nil script)))
            (kv-test--decode (buffer-string))))
      (delete-file script))))

(ert-deftest kv-python-helper-survives-foreign-and-failing-objects ()
  (skip-unless (file-executable-p kv-test--python))
  (let* ((vars (plist-get (kv-test--run-helper "_ygg_kv.list()") :variables))
         (by-name (lambda (name) (seq-find (lambda (v) (equal (plist-get v :name) name))
                                           vars))))
    (should (equal (sort (mapcar (lambda (v) (plist-get v :name)) vars) #'string<)
                   '("boom" "frame" "lazy" "n")))
    (should (equal (plist-get (funcall by-name "frame") :kind) "table"))
    (should (string-prefix-p "[3 rows x 2 columns]"
                             (plist-get (funcall by-name "frame") :value)))
    (should-not (equal (plist-get (funcall by-name "lazy") :kind) "table"))
    (should (equal (plist-get (funcall by-name "boom") :kind) "other"))
    (should (equal (plist-get (funcall by-name "n") :value) "7"))))

(ert-deftest kv-python-helper-inspects-a-pandas-frame ()
  (skip-unless (file-executable-p kv-test--python))
  (let ((json (kv-test--run-helper "_ygg_kv.inspect(['frame'], 10)")))
    (should (equal (plist-get json :length) 2))
    (should (equal (mapcar (lambda (v) (plist-get v :name)) (plist-get json :children))
                   '("a" "b")))))

;;; kernel-vars-tests.el ends here
