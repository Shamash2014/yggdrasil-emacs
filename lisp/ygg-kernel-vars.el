;;; ygg-kernel-vars.el --- A variables pane for Jupyter kernels -*- lexical-binding: t; -*-

;; Third-party: emacs-jupyter.  R kernels (ark) are read through ark's own
;; positron.variables comm: a refresh event on open, an update event after
;; every execution, and list and inspect RPCs.  Python kernels have no such
;; comm, so a silent execute evaluates a small helper through
;; user_expressions after each execution the kernel reports idle.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'json)
(require 'ygg-kernel-picker)

(declare-function jupyter-run-with-state "jupyter-monads")
(declare-function jupyter-sent "jupyter-monads")
(declare-function jupyter-subscribe "jupyter-monads")
(declare-function jupyter-subscriber "jupyter-monads")
(declare-function jupyter-unsubscribe "jupyter-monads")
(declare-function jupyter-kernel-io "jupyter-client")
(declare-function jupyter-kernel-info "jupyter-client")
(declare-function jupyter-inspect "jupyter-client")
(declare-function jupyter-request-id "jupyter-base")
(declare-function jupyter-comm-open "jupyter-messages")
(declare-function jupyter-comm-msg "jupyter-messages")
(declare-function jupyter-execute-request "jupyter-messages")
(declare-function jupyter-message-type "jupyter-messages")
(declare-function jupyter-message-content "jupyter-messages")
(declare-function jupyter-message-parent-id "jupyter-messages")
(declare-function jupyter-message-parent-type "jupyter-messages")
(declare-function jupyter-new-uuid "jupyter-messages")
(declare-function yggdrasil-define-mode-keys "yggdrasil-core")
(declare-function ygg-visidata-view "ygg-visidata")
(defvar jupyter-current-client)
(defvar ygg-modal-special-modes)

(defgroup ygg-kernel-vars nil
  "A variables pane for Jupyter kernels."
  :group 'tools)

(defcustom ygg-kernel-vars-width 0.3
  "Width of the pane's side window, a fraction of the frame or columns."
  :type 'number)

(defcustom ygg-kernel-vars-child-limit 200
  "Most children the Python helper returns for one inspected value."
  :type 'natnum)

(defconst ygg-kernel-vars-ark-target "positron.variables"
  "Comm target of ark's variables pane.")

(defface ygg-kernel-vars-heading
  '((t :inherit shadow :weight bold :height 0.9))
  "Section headings.")

(defface ygg-kernel-vars-type
  '((t :inherit shadow))
  "The type and shape column.")

(defface ygg-kernel-vars-glyph
  '((t :inherit shadow))
  "Expansion glyphs.")

;;; Variables

(defun ygg-kernel-vars--bool (value)
  "VALUE as a Lisp boolean; JSON false decodes to :json-false."
  (and value (not (eq value :json-false))))

(defun ygg-kernel-vars--ark-variable (var)
  "Normalize ark's Variable VAR to the pane's plist."
  (list :key (plist-get var :access_key)
        :name (plist-get var :display_name)
        :type (plist-get var :display_type)
        :value (or (plist-get var :display_value) "")
        :kind (plist-get var :kind)
        :length (plist-get var :length)
        :has-children (ygg-kernel-vars--bool (plist-get var :has_children))))

(defun ygg-kernel-vars--python-type (type shape)
  (if (and shape (> (length shape) 0))
      (format "%s [%s]" type (mapconcat #'number-to-string shape ", "))
    type))

(defun ygg-kernel-vars--python-variable (var)
  "Normalize one variable VAR the Python helper reports."
  (list :key (plist-get var :key)
        :name (plist-get var :name)
        :type (ygg-kernel-vars--python-type (plist-get var :type)
                                            (plist-get var :shape))
        :value (or (plist-get var :value) "")
        :kind (plist-get var :kind)
        :length (plist-get var :length)
        :has-children (ygg-kernel-vars--bool (plist-get var :has_children))))

(defun ygg-kernel-vars--sort (vars)
  (sort vars (lambda (a b) (string< (plist-get a :name) (plist-get b :name)))))

(defun ygg-kernel-vars--ark-event (data)
  "Parse an ark variables comm DATA into (METHOD . PARAMS), or nil."
  (and-let* ((method (plist-get data :method))
             ((member method '("refresh" "update"))))
    (cons (intern method) (plist-get data :params))))

(defun ygg-kernel-vars--apply-event (vars event)
  "VARS after ark EVENT, a (METHOD . PARAMS) from a comm message."
  (pcase event
    (`(refresh . ,params)
     (ygg-kernel-vars--sort
      (mapcar #'ygg-kernel-vars--ark-variable (plist-get params :variables))))
    (`(update . ,params)
     (let* ((assigned (mapcar #'ygg-kernel-vars--ark-variable
                              (plist-get params :assigned)))
            (gone (append (plist-get params :removed)
                          (mapcar (lambda (v) (plist-get v :key)) assigned) nil)))
       (ygg-kernel-vars--sort
        (append assigned
                (cl-remove-if (lambda (v) (member (plist-get v :key) gone))
                              vars)))))
    (_ vars)))

(defun ygg-kernel-vars--rpc-result (data)
  "The result of an RPC reply DATA, or signal its error."
  (if-let* ((err (plist-get data :error)))
      (error "%s" (or (plist-get err :message) err))
    (plist-get data :result)))

(defun ygg-kernel-vars--with-more (children total)
  "CHILDREN, then a row counting the TOTAL minus those shown, if any."
  (let ((more (- (or total 0) (length children))))
    (if (> more 0)
        (append children (list (list :name "…" :type "" :more t
                                     :value (format "%d more" more))))
      children)))

(defun ygg-kernel-vars--ark-children (result)
  "Children in an ark inspect RESULT."
  (ygg-kernel-vars--with-more
   (mapcar #'ygg-kernel-vars--ark-variable (plist-get result :children))
   (plist-get result :length)))

(defun ygg-kernel-vars--python-json (reply-expr)
  "Decode one user expression REPLY-EXPR from an execute_reply."
  (if (equal (plist-get reply-expr :status) "ok")
      (let ((json-object-type 'plist) (json-array-type 'vector))
        (json-read-from-string
         (plist-get (plist-get reply-expr :data) :text/plain)))
    (error "%s" (or (plist-get reply-expr :evalue) "helper failed"))))

(defun ygg-kernel-vars--python-list (json)
  (ygg-kernel-vars--sort
   (mapcar #'ygg-kernel-vars--python-variable (plist-get json :variables))))

(defun ygg-kernel-vars--python-children (json)
  (ygg-kernel-vars--with-more
   (mapcar #'ygg-kernel-vars--python-variable (plist-get json :children))
   (plist-get json :length)))

;;; Python helper

(defconst ygg-kernel-vars--python-helper "\
def _ygg_kv_install():
    import itertools, json, reprlib, types
    ip = get_ipython()
    brief = reprlib.Repr(maxlevel=2, maxlist=20, maxtuple=20, maxset=20, maxfrozenset=20,
                         maxdict=12, maxstring=160, maxlong=60, maxother=160)
    skip = (types.ModuleType, types.FunctionType, types.BuiltinFunctionType,
            types.MethodType, type)
    hidden = set(getattr(ip, 'user_ns_hidden', {})) | {'In', 'Out', 'exit', 'quit', 'get_ipython'}

    def short(v, n=160):
        try:
            r = brief.repr(v)
        except Exception:
            r = '<?>'
        return ' '.join(r[:n * 4].split())[:n]

    def shape(v):
        s = getattr(v, 'shape', None)
        if isinstance(s, tuple) and all(isinstance(i, int) for i in s):
            return list(s)
        if isinstance(v, (list, tuple, dict, set, frozenset, str, bytes)):
            return [len(v)]
        return None

    def attrs(v):
        d = getattr(v, '__dict__', None)
        if not isinstance(d, dict) or isinstance(v, type):
            return []
        return [k for k, x in d.items() if not k.startswith('_') and not callable(x)]

    def pandas(v, name):
        t = type(v)
        return t.__name__ == name and (t.__module__ or '').startswith('pandas')

    def kind(v):
        t = type(v).__name__
        if pandas(v, 'DataFrame') or (t == 'ndarray' and getattr(v, 'ndim', 0) == 2):
            return 'table'
        if isinstance(v, dict):
            return 'map'
        if isinstance(v, (list, tuple, set, frozenset)) or pandas(v, 'Series') or t == 'ndarray':
            return 'collection'
        if isinstance(v, bool):
            return 'boolean'
        if isinstance(v, (int, float, complex)):
            return 'number'
        if isinstance(v, str):
            return 'string'
        return 'other'

    def children(v, limit):
        head = lambda it: list(itertools.islice(it, limit))
        if pandas(v, 'DataFrame'):
            return [('i:%d' % i, str(c), v.iloc[:, i]) for i, c in enumerate(head(v.columns))], v.shape[1]
        if pandas(v, 'Series'):
            return [('i:%d' % i, str(k), x) for i, (k, x) in enumerate(head(v.items()))], len(v)
        if isinstance(v, dict):
            return [('i:%d' % i, k if isinstance(k, str) else repr(k), x)
                    for i, (k, x) in enumerate(head(v.items()))], len(v)
        if isinstance(v, (list, tuple)):
            return [('i:%d' % i, '[%d]' % i, x) for i, x in enumerate(head(v))], len(v)
        if isinstance(v, (set, frozenset)):
            return [('i:%d' % i, '{%d}' % i, x) for i, x in enumerate(head(v))], len(v)
        names = attrs(v)
        return [('a:' + k, k, getattr(v, k)) for k in names[:limit]], len(names)

    def has_children(v):
        if pandas(v, 'Series'):
            return len(v) > 0
        if pandas(v, 'DataFrame'):
            return v.shape[1] > 0
        if isinstance(v, (dict, list, tuple, set, frozenset)):
            return len(v) > 0
        if isinstance(v, (str, bytes, int, float, complex, bool)) or type(v).__name__ == 'ndarray':
            return False
        return bool(attrs(v))

    def value(v):
        if pandas(v, 'DataFrame'):
            return '[%d rows x %d columns] %s' % (v.shape[0], v.shape[1], ', '.join(map(str, v.columns[:20])))
        if pandas(v, 'Series'):
            return short(list(v.head(40)))
        return short(v)

    def describe(key, name, v):
        t = type(v).__name__
        try:
            if pandas(v, 'Series') and hasattr(v, 'dtype'):
                t = 'Series %s' % v.dtype
            s = shape(v)
            return {'key': key, 'name': name, 'type': t, 'shape': s,
                    'length': s[0] if s else 0, 'value': value(v),
                    'kind': kind(v), 'has_children': has_children(v)}
        except Exception:
            return {'key': key, 'name': name, 'type': t, 'shape': None,
                    'length': 0, 'value': short(v), 'kind': 'other', 'has_children': False}

    def child(v, key):
        i = int(key[2:]) if key.startswith('i:') else None
        if pandas(v, 'DataFrame'):
            return v.iloc[:, i]
        if pandas(v, 'Series'):
            return v.iloc[i]
        if isinstance(v, dict):
            return next(itertools.islice(v.values(), i, None))
        if isinstance(v, (list, tuple)):
            return v[i]
        if isinstance(v, (set, frozenset)):
            return next(itertools.islice(v, i, None))
        return getattr(v, key[2:])

    class Json:
        def __init__(self, obj):
            self.text = json.dumps(obj, default=str)
        def __repr__(self):
            return self.text

    class Helper:
        def list(self):
            out = []
            for k, v in list(ip.user_ns.items()):
                if k.startswith('_') or k in hidden or isinstance(v, skip):
                    continue
                out.append(describe(k, k, v))
            return Json({'variables': out, 'length': len(out)})

        def inspect(self, path, limit):
            v = ip.user_ns[path[0]]
            for key in path[1:]:
                v = child(v, key)
            kids, total = children(v, limit)
            return Json({'children': [describe(k, n, x) for k, n, x in kids],
                         'length': total})

    ip.user_ns['_ygg_kv'] = Helper()
    ip.user_ns_hidden['_ygg_kv'] = None

_ygg_kv_install()
del _ygg_kv_install
"
  "Installs _ygg_kv in the kernel; run silent before every read.")

(defun ygg-kernel-vars--python-expressions (paths)
  "User expressions listing the namespace and inspecting each of PATHS."
  (let ((exprs (list :list "_ygg_kv.list()")))
    (cl-loop for path in paths for i from 0
             do (setq exprs
                      (plist-put exprs (intern (format ":i%d" i))
                                 (format "_ygg_kv.inspect(%s, %d)"
                                         (json-encode (vconcat path))
                                         ygg-kernel-vars-child-limit))))
    exprs))

;;; State

(cl-defstruct (ygg-kernel-vars--state (:constructor ygg-kernel-vars--make-state))
  client backend buffer comm-id vars
  (children (make-hash-table :test #'equal))
  (expanded (make-hash-table :test #'equal))
  (pending (make-hash-table :test #'equal))
  (own (make-hash-table :test #'equal))
  filter error)

(defvar ygg-kernel-vars--states (make-hash-table :test #'eq)
  "Kernel client to its pane state.")

(defvar-local ygg-kernel-vars--pane nil
  "The state of the kernel this pane shows.")

(defun ygg-kernel-vars--language (client)
  "CLIENT's kernel language; emacs-jupyter interns it as a symbol."
  (format "%s" (plist-get (plist-get (jupyter-kernel-info client) :language_info)
                          :name)))

(defun ygg-kernel-vars--backend (client)
  "ark by kernel_info implementation, so every ark kernelspec qualifies."
  (cond ((equal (format "%s" (plist-get (jupyter-kernel-info client) :implementation))
                "ark")
         'ark)
        ((equal (ygg-kernel-vars--language client) "python") 'python)
        (t (user-error "No variables pane for the %s kernel"
                       (ygg-kernel-vars--language client)))))

(defun ygg-kernel-vars--send (state request)
  "Send REQUEST on STATE's client and return its message id."
  (jupyter-request-id
   (jupyter-run-with-state (ygg-kernel-vars--state-client state)
     (jupyter-sent request))))

(defun ygg-kernel-vars--expect (state id callback)
  (puthash id callback (ygg-kernel-vars--state-pending state)))

(defun ygg-kernel-vars--ark-rpc (state method params callback)
  "Call ark's variables comm METHOD with PARAMS, then CALLBACK on the result."
  (let ((data (append (list :jsonrpc "2.0" :id (jupyter-new-uuid) :method method)
                      (and params (list :params params)))))
    (ygg-kernel-vars--expect
     state
     (ygg-kernel-vars--send
      state (jupyter-comm-msg :id (ygg-kernel-vars--state-comm-id state)
                              :data data :handlers nil))
     (lambda (msg)
       (when (equal (jupyter-message-type msg) "comm_msg")
         (funcall callback (ygg-kernel-vars--rpc-result
                            (plist-get (jupyter-message-content msg) :data)))
         t)))))

(defun ygg-kernel-vars--ark-open (state)
  (let ((id (format "ygg-kernel-vars-%s" (jupyter-new-uuid))))
    (setf (ygg-kernel-vars--state-comm-id state) id)
    (ygg-kernel-vars--send
     state (jupyter-comm-open :id id :target-name ygg-kernel-vars-ark-target
                              :data nil :handlers nil))))

(defun ygg-kernel-vars--expanded-paths (state)
  (let (paths)
    (maphash (lambda (path _) (push path paths))
             (ygg-kernel-vars--state-expanded state))
    (sort paths (lambda (a b) (< (length a) (length b))))))

(defun ygg-kernel-vars--python-read (state)
  "Run the helper silently, listing the namespace and every expanded path."
  (let* ((paths (ygg-kernel-vars--expanded-paths state))
         (id (ygg-kernel-vars--send
              state (jupyter-execute-request
                     :code ygg-kernel-vars--python-helper :silent t :store-history nil
                     :allow-stdin nil
                     :user-expressions (ygg-kernel-vars--python-expressions paths)
                     :handlers nil))))
    (puthash id t (ygg-kernel-vars--state-own state))
    (ygg-kernel-vars--expect
     state id
     (lambda (msg)
       (when (equal (jupyter-message-type msg) "execute_reply")
         (ygg-kernel-vars--python-apply
          state paths (plist-get (jupyter-message-content msg) :user_expressions))
         t)))))

(defun ygg-kernel-vars--python-apply (state paths exprs)
  (condition-case err
      (let ((children (ygg-kernel-vars--state-children state)))
        (setf (ygg-kernel-vars--state-vars state)
              (ygg-kernel-vars--python-list
               (ygg-kernel-vars--python-json (plist-get exprs :list)))
              (ygg-kernel-vars--state-error state) nil)
        (clrhash children)
        (cl-loop for path in paths for i from 0
                 for reply = (plist-get exprs (intern (format ":i%d" i)))
                 do (if (and reply (equal (plist-get reply :status) "ok"))
                        (puthash path (ygg-kernel-vars--python-children
                                       (ygg-kernel-vars--python-json reply))
                                 children)
                      (remhash path (ygg-kernel-vars--state-expanded state)))))
    (error (setf (ygg-kernel-vars--state-error state) (error-message-string err))))
  (ygg-kernel-vars--redraw state))

(defun ygg-kernel-vars--ark-inspect (state path)
  (ygg-kernel-vars--ark-rpc
   state "inspect" (list :path (vconcat path))
   (lambda (result)
     (puthash path (ygg-kernel-vars--ark-children result)
              (ygg-kernel-vars--state-children state))
     (ygg-kernel-vars--redraw state))))

(defun ygg-kernel-vars--ark-reinspect (state names)
  "Fetch again every expanded path under one of NAMES, dropping the rest."
  (dolist (path (ygg-kernel-vars--expanded-paths state))
    (when (member (car path) names)
      (remhash path (ygg-kernel-vars--state-children state))
      (if (cl-find (car path) (ygg-kernel-vars--state-vars state)
                   :key (lambda (v) (plist-get v :key)) :test #'equal)
          (ygg-kernel-vars--ark-inspect state path)
        (remhash path (ygg-kernel-vars--state-expanded state))))))

(defun ygg-kernel-vars--on-ark-event (state event)
  (setf (ygg-kernel-vars--state-vars state)
        (ygg-kernel-vars--apply-event (ygg-kernel-vars--state-vars state) event))
  (ygg-kernel-vars--ark-reinspect
   state (pcase event
           (`(refresh . ,_) (mapcar #'car (ygg-kernel-vars--expanded-paths state)))
           (`(update . ,params)
            (append (plist-get params :removed)
                    (mapcar (lambda (v) (plist-get v :access_key))
                            (plist-get params :assigned))
                    nil))))
  (ygg-kernel-vars--redraw state))

(defun ygg-kernel-vars-refresh-state (state)
  "Read STATE's kernel variables again."
  (pcase (ygg-kernel-vars--state-backend state)
    ('python (ygg-kernel-vars--python-read state))
    ('ark
     (if (not (ygg-kernel-vars--state-comm-id state))
         (ygg-kernel-vars--ark-open state)
       (ygg-kernel-vars--ark-rpc
        state "list" nil
        (lambda (result)
          (ygg-kernel-vars--on-ark-event state (cons 'refresh result))))))))

(defun ygg-kernel-vars--take (key table)
  "Remove KEY from TABLE; non-nil if it was there."
  (prog1 (gethash key table) (remhash key table)))

(defun ygg-kernel-vars--live-p (state)
  (buffer-live-p (ygg-kernel-vars--state-buffer state)))

(defun ygg-kernel-vars--on-message (state msg)
  (let ((type (jupyter-message-type msg))
        (content (jupyter-message-content msg))
        (pending (ygg-kernel-vars--state-pending state)))
    (when-let* ((callback (gethash (jupyter-message-parent-id msg) pending)))
      (condition-case err
          (when (funcall callback msg)
            (remhash (jupyter-message-parent-id msg) pending))
        (error (remhash (jupyter-message-parent-id msg) pending)
               (setf (ygg-kernel-vars--state-error state) (error-message-string err))
               (ygg-kernel-vars--redraw state))))
    (pcase type
      ("status"
       (pcase (plist-get content :execution_state)
         ("starting"
          (setf (ygg-kernel-vars--state-comm-id state) nil)
          (clrhash pending)
          (clrhash (ygg-kernel-vars--state-own state))
          (run-at-time 0.5 nil #'ygg-kernel-vars-refresh-state state))
         ("idle"
          (when (and (eq (ygg-kernel-vars--state-backend state) 'python)
                     (equal (jupyter-message-parent-type msg) "execute_request")
                     (not (ygg-kernel-vars--take (jupyter-message-parent-id msg)
                                     (ygg-kernel-vars--state-own state))))
            (run-at-time 0 nil #'ygg-kernel-vars-refresh-state state)))))
      ("comm_msg"
       (when (equal (plist-get content :comm_id) (ygg-kernel-vars--state-comm-id state))
         (when-let* ((event (ygg-kernel-vars--ark-event (plist-get content :data))))
           (ygg-kernel-vars--on-ark-event state event))))
      ("comm_close"
       (when (equal (plist-get content :comm_id) (ygg-kernel-vars--state-comm-id state))
         (setf (ygg-kernel-vars--state-comm-id state) nil))))))

(defun ygg-kernel-vars--watch (state)
  (let ((client (ygg-kernel-vars--state-client state)))
    (jupyter-run-with-state (jupyter-kernel-io client)
      (jupyter-subscribe
       (jupyter-subscriber
         (lambda (msg)
           (if (not (eq (gethash client ygg-kernel-vars--states) state))
               (jupyter-unsubscribe)
             (with-demoted-errors "ygg-kernel-vars: %S"
               (ygg-kernel-vars--on-message state msg))
             nil)))))))

(defun ygg-kernel-vars--state (client)
  "CLIENT's pane state, created and watching on first use."
  (or (and-let* ((state (gethash client ygg-kernel-vars--states))
                 ((ygg-kernel-vars--live-p state)))
        state)
      (let* ((backend (ygg-kernel-vars--backend client))
             (state (ygg-kernel-vars--make-state :client client :backend backend)))
        (setf (ygg-kernel-vars--state-buffer state)
              (ygg-kernel-vars--make-buffer state))
        (puthash client state ygg-kernel-vars--states)
        (ygg-kernel-vars--watch state)
        (ygg-kernel-vars-refresh-state state)
        state)))

;;; Rendering

(defun ygg-kernel-vars--section (var)
  (pcase (plist-get var :kind)
    ("table" "DATA")
    ("function" "FUNCTIONS")
    (_ "VALUES")))

(defconst ygg-kernel-vars--sections '("DATA" "VALUES" "FUNCTIONS"))

(defun ygg-kernel-vars--visible (vars filter)
  (if (or (null filter) (string-empty-p filter))
      vars
    (let ((case-fold-search t))
      (cl-remove-if-not (lambda (v) (string-match-p (regexp-quote filter)
                                                    (plist-get v :name)))
                        vars))))

(defun ygg-kernel-vars--rows (vars children expanded &optional depth parent)
  "Flatten VARS into (DEPTH PATH VAR) rows, descending into EXPANDED paths.
CHILDREN maps a path to its fetched children."
  (let ((depth (or depth 0)))
    (cl-loop for var in vars
             for path = (append parent (list (plist-get var :key)))
             collect (list depth path var)
             when (and (gethash path expanded) (gethash path children))
             append (ygg-kernel-vars--rows (gethash path children) children
                                           expanded (1+ depth) path))))

(defun ygg-kernel-vars--fit (string width)
  (truncate-string-to-width (or string "") (max width 0) nil ?\s "…"))

(defun ygg-kernel-vars--fit-type (type width)
  "TYPE in WIDTH columns, shortening the name before the bracketed shape."
  (if (and type (> (string-width type) width)
           (string-match "\\`\\(.*?\\) *\\(\\[[^]]*\\]\\)\\'" type)
           (< (string-width (match-string 2 type)) (- width 1)))
      (let ((shape (match-string 2 type)))
        (concat (ygg-kernel-vars--fit (match-string 1 type)
                                      (- width (string-width shape) 1))
                " " shape))
    (ygg-kernel-vars--fit type width)))

(defun ygg-kernel-vars--columns (rows width)
  "Name and type column widths for ROWS in WIDTH columns."
  (let ((name (cl-loop for (depth _ var) in rows
                       maximize (+ (* 2 depth) 2 (string-width (plist-get var :name)))))
        (type (cl-loop for (_ _ var) in rows
                       maximize (string-width (or (plist-get var :type) "")))))
    (let ((name (min (or name 0) (max 6 (/ (* width 7) 20)))))
      (list name (min (or type 0) (max 4 (- width name 4 (min 8 (/ width 6)))))))))

(defun ygg-kernel-vars--line (row columns width expanded)
  "One pane line for ROW, never wider than WIDTH."
  (pcase-let* ((`(,depth ,path ,var) row)
               (`(,name-width ,type-width) columns)
               (glyph (cond ((not (plist-get var :has-children)) " ")
                            ((gethash path expanded) "▾")
                            (t "▸")))
               (lead (concat (make-string (* 2 depth) ?\s)
                             (propertize glyph 'face 'ygg-kernel-vars-glyph) " "))
               (name (ygg-kernel-vars--fit (plist-get var :name)
                                           (- name-width (string-width lead))))
               (type (propertize (ygg-kernel-vars--fit-type (plist-get var :type) type-width)
                                 'face 'ygg-kernel-vars-type))
               (head (concat lead name "  " type "  "))
               (value (truncate-string-to-width
                       (replace-regexp-in-string "[\n\t]" " " (plist-get var :value))
                       (max 0 (- width (string-width head))) nil nil "…")))
    (if (plist-get var :more)
        (propertize (truncate-string-to-width (concat head value) width) 'face 'shadow)
      (propertize (truncate-string-to-width (concat head value) width)
                  'ygg-kernel-vars-path path 'ygg-kernel-vars-var var))))

(defun ygg-kernel-vars-render (vars children expanded width &optional filter)
  "Pane lines for VARS in WIDTH columns, grouped under section headings.
CHILDREN and EXPANDED are hash tables keyed by path; FILTER narrows the
top level by name."
  (let* ((vars (ygg-kernel-vars--visible vars filter))
         (all (ygg-kernel-vars--rows vars children expanded))
         (columns (ygg-kernel-vars--columns all width))
         lines)
    (dolist (section ygg-kernel-vars--sections)
      (when-let* ((members (cl-remove-if-not
                            (lambda (v) (equal (ygg-kernel-vars--section v) section))
                            vars)))
        (when lines (push "" lines))
        (push (propertize section 'face 'ygg-kernel-vars-heading) lines)
        (dolist (row (ygg-kernel-vars--rows members children expanded))
          (push (ygg-kernel-vars--line row columns width expanded) lines))))
    (nreverse lines)))

(defun ygg-kernel-vars--width (buffer)
  (if-let* ((window (get-buffer-window buffer t)))
      (1- (window-body-width window))
    60))

(defun ygg-kernel-vars--header (state)
  (let ((filter (ygg-kernel-vars--state-filter state))
        (err (ygg-kernel-vars--state-error state)))
    (concat " " (buffer-name (ygg-kernel-vars--state-buffer state))
            (if (and filter (not (string-empty-p filter)))
                (propertize (format "  /%s" filter) 'face 'shadow) "")
            (if err (propertize (format "  %s" err) 'face 'shadow) ""))))

(defun ygg-kernel-vars--redraw (state)
  (when (ygg-kernel-vars--live-p state)
    (with-current-buffer (ygg-kernel-vars--state-buffer state)
      (let* ((inhibit-read-only t)
             (path (get-text-property (point) 'ygg-kernel-vars-path))
             (line (line-number-at-pos))
             (lines (ygg-kernel-vars-render
                     (ygg-kernel-vars--state-vars state)
                     (ygg-kernel-vars--state-children state)
                     (ygg-kernel-vars--state-expanded state)
                     (ygg-kernel-vars--width (current-buffer))
                     (ygg-kernel-vars--state-filter state))))
        (erase-buffer)
        (insert (string-join lines "\n"))
        (setq header-line-format (ygg-kernel-vars--header state))
        (goto-char (point-min))
        (unless (and path (ygg-kernel-vars--goto-path path))
          (forward-line (1- line)))
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (set-window-point window (point)))))))

(defun ygg-kernel-vars--goto-path (path)
  (when-let* ((match (text-property-search-forward 'ygg-kernel-vars-path path #'equal)))
    (goto-char (prop-match-beginning match))))

;;; Pane

(defvar-keymap ygg-kernel-vars-mode-map
  :doc "Keys of the variables pane."
  "j" #'ygg-kernel-vars-next
  "k" #'ygg-kernel-vars-previous
  "TAB" #'ygg-kernel-vars-toggle-row
  "<tab>" #'ygg-kernel-vars-toggle-row
  "RET" #'ygg-kernel-vars-view
  "/" #'ygg-kernel-vars-filter
  "g r" #'ygg-kernel-vars-refresh
  "q" #'ygg-kernel-vars-quit)

(define-derived-mode ygg-kernel-vars-mode special-mode "Variables"
  "The variables of one Jupyter kernel."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  (add-hook 'window-size-change-functions #'ygg-kernel-vars--resized nil t)
  (add-hook 'kill-buffer-hook #'ygg-kernel-vars--forget nil t))

(defun ygg-kernel-vars--resized (_window)
  (when ygg-kernel-vars--pane
    (ygg-kernel-vars--redraw ygg-kernel-vars--pane)))

(defun ygg-kernel-vars--forget ()
  (when-let* ((state ygg-kernel-vars--pane))
    (remhash (ygg-kernel-vars--state-client state) ygg-kernel-vars--states)))

(defun ygg-kernel-vars--buffer-name (state)
  (generate-new-buffer-name
   (format "*variables %s*"
           (ygg-kernel-vars--language (ygg-kernel-vars--state-client state)))))

(defun ygg-kernel-vars--make-buffer (state)
  (with-current-buffer (get-buffer-create (ygg-kernel-vars--buffer-name state))
    (ygg-kernel-vars-mode)
    (setq ygg-kernel-vars--pane state)
    (setq-local jupyter-current-client (ygg-kernel-vars--state-client state))
    (current-buffer)))

(defun ygg-kernel-vars--client ()
  (or (and ygg-kernel-vars--pane (ygg-kernel-vars--state-client ygg-kernel-vars--pane))
      (ygg-kernel-picker-current-client)
      (user-error "No Jupyter kernel is associated with this buffer")))

(defun ygg-kernel-vars--display (buffer)
  (display-buffer-in-side-window
   buffer `((side . right) (slot . 0) (window-width . ,ygg-kernel-vars-width)
            (preserve-size . (t . nil))
            (window-parameters . ((no-delete-other-windows . t))))))

;;;###autoload
(defun ygg-kernel-vars-toggle ()
  "Show or hide the variables pane of this buffer's Jupyter kernel."
  (interactive)
  (unless (require 'jupyter-client nil t)
    (user-error "emacs-jupyter is not installed"))
  (let* ((state (ygg-kernel-vars--state (ygg-kernel-vars--client)))
         (buffer (ygg-kernel-vars--state-buffer state)))
    (if-let* ((window (get-buffer-window buffer)))
        (delete-window window)
      (ygg-kernel-vars--display buffer)
      (ygg-kernel-vars--redraw state))))

(defun ygg-kernel-vars--state-here ()
  (or ygg-kernel-vars--pane (user-error "Not a variables pane")))

(defun ygg-kernel-vars--row ()
  (get-text-property (point) 'ygg-kernel-vars-path))

(defun ygg-kernel-vars--move (n)
  (let ((start (point)))
    (forward-line n)
    (while (and (not (ygg-kernel-vars--row))
                (zerop (forward-line (cl-signum n)))))
    (unless (ygg-kernel-vars--row) (goto-char start))))

(defun ygg-kernel-vars-next ()
  "Move to the next variable."
  (interactive)
  (ygg-kernel-vars--move 1))

(defun ygg-kernel-vars-previous ()
  "Move to the previous variable."
  (interactive)
  (ygg-kernel-vars--move -1))

(defun ygg-kernel-vars-toggle-path (state path)
  "Expand PATH in STATE, fetching its children, or collapse it."
  (let ((expanded (ygg-kernel-vars--state-expanded state)))
    (if (gethash path expanded)
        (progn (remhash path expanded)
               (ygg-kernel-vars--redraw state))
      (puthash path t expanded)
      (pcase (ygg-kernel-vars--state-backend state)
        ('ark (ygg-kernel-vars--ark-inspect state path))
        ('python (ygg-kernel-vars--python-read state))))))

(defun ygg-kernel-vars-toggle-row ()
  "Expand or collapse the variable at point."
  (interactive)
  (let ((path (or (ygg-kernel-vars--row) (user-error "No variable here")))
        (var (get-text-property (point) 'ygg-kernel-vars-var)))
    (if (plist-get var :has-children)
        (ygg-kernel-vars-toggle-path (ygg-kernel-vars--state-here) path)
      (message "%s has no children" (plist-get var :name)))))

(defun ygg-kernel-vars-view ()
  "Open the data frame at point in VisiData, else inspect it in the kernel."
  (interactive)
  (let* ((path (or (ygg-kernel-vars--row) (user-error "No variable here")))
         (var (get-text-property (point) 'ygg-kernel-vars-var))
         (name (car path)))
    (cond ((and (equal (plist-get var :kind) "table") (null (cdr path))
                (fboundp 'ygg-visidata-view))
           (ygg-visidata-view name))
          ((and (cdr path) (plist-get var :has-children))
           (ygg-kernel-vars-toggle-row))
          (t (jupyter-inspect name (length name))))))

(defun ygg-kernel-vars-filter (filter)
  "Show only the variables whose name contains FILTER; empty shows all."
  (interactive (list (read-string "Filter variables: "
                                  (ygg-kernel-vars--state-filter
                                   (ygg-kernel-vars--state-here)))))
  (let ((state (ygg-kernel-vars--state-here)))
    (setf (ygg-kernel-vars--state-filter state) filter)
    (ygg-kernel-vars--redraw state)))

(defun ygg-kernel-vars-refresh ()
  "Read the kernel's variables again."
  (interactive)
  (ygg-kernel-vars-refresh-state (ygg-kernel-vars--state-here)))

(defun ygg-kernel-vars-quit ()
  "Hide the pane."
  (interactive)
  (quit-window))

;;; Keys

(with-eval-after-load 'yggdrasil-core
  (add-to-list 'ygg-modal-special-modes 'ygg-kernel-vars-mode)
  (yggdrasil-define-mode-keys 'ygg-kernel-vars-mode 'normal ygg-kernel-vars-mode-map))

(provide 'ygg-kernel-vars)
;;; ygg-kernel-vars.el ends here
