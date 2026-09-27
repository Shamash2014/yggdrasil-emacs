;;; lsp-calls-tests.el --- Call hierarchy through a stubbed server -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'ygg-lsp-calls)

(defun lsp-calls-tests--range (line character)
  (list :start (list :line line :character character)
        :end (list :line line :character (+ character 3))))

(defconst lsp-calls-tests--item
  (list :name "total" :uri "file:///src/cart.ts"
        :selectionRange (lsp-calls-tests--range 9 4)))

(defconst lsp-calls-tests--responses
  `((:textDocument/prepareCallHierarchy . [,lsp-calls-tests--item])
    (:callHierarchy/incomingCalls
     . [(:from (:name "checkout" :uri "file:///src/pay.ts"
                :selectionRange ,(lsp-calls-tests--range 1 0))
         :fromRanges [,(lsp-calls-tests--range 20 6) ,(lsp-calls-tests--range 31 2)])])
    (:callHierarchy/outgoingCalls
     . [(:to (:name "sum" :uri "file:///src/math.ts"
              :selectionRange ,(lsp-calls-tests--range 4 9))
         :fromRanges [,(lsp-calls-tests--range 11 8)])
        (:to (:name "round" :uri "file:///src/math.ts"
              :selectionRange ,(lsp-calls-tests--range 14 9))
         :fromRanges [])])))

(defmacro lsp-calls-tests--with-server (capable &rest body)
  "Run BODY in a buffer eglot manages, whose server offers the CAPABLE ones.
The rows the quickfix is given land in rows, its title in title."
  (declare (indent 1))
  `(let (rows title shown requests)
     (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
               ((symbol-function 'eglot-current-server) (lambda () 'server))
               ((symbol-function 'eglot-server-capable)
                (lambda (&rest feats) (memq (car feats) ,capable)))
               ((symbol-function 'eglot--TextDocumentPositionParams)
                (lambda () '(:position (:line 9 :character 5))))
               ((symbol-function 'eglot-uri-to-path)
                (lambda (uri) (string-remove-prefix "file://" uri)))
               ((symbol-function 'jsonrpc-request)
                (lambda (_server method params &rest _)
                  (push (cons method params) requests)
                  (alist-get method lsp-calls-tests--responses)))
               ((symbol-function 'ygg-qf-from-text)
                (lambda (text name replace &rest _)
                  (should replace)
                  (setq rows (split-string text "\n") title name)
                  (length rows)))
               ((symbol-function 'eglot-show-call-hierarchy)
                (lambda () (interactive) (setq shown 'calls)))
               ((symbol-function 'eglot-show-type-hierarchy)
                (lambda () (interactive) (setq shown 'types))))
       ,@body)))

(ert-deftest lsp-calls-incoming-lists-every-call-site ()
  (lsp-calls-tests--with-server '(:callHierarchyProvider)
    (ygg-lsp-calls-qf)
    (should (equal title "calls into total"))
    (should (equal rows '("/src/pay.ts:21:7: checkout"
                          "/src/pay.ts:32:3: checkout")))
    (should (equal (plist-get (cdr (assq :callHierarchy/incomingCalls requests)) :item)
                   lsp-calls-tests--item))))

(ert-deftest lsp-calls-outgoing-sites-sit-in-the-callers-file ()
  (lsp-calls-tests--with-server '(:callHierarchyProvider)
    (ygg-lsp-calls-qf t)
    (should (equal title "calls from total"))
    (should (equal rows '("/src/cart.ts:12:9: sum"
                          "/src/math.ts:15:10: round")))))

(ert-deftest lsp-calls-refuse-without-a-server-or-its-capability ()
  (cl-letf (((symbol-function 'eglot-managed-p) #'ignore))
    (should-error (ygg-lsp-calls-qf) :type 'user-error)
    (should-error (ygg-lsp-call-hierarchy) :type 'user-error))
  (lsp-calls-tests--with-server '(:typeHierarchyProvider)
    (should-error (ygg-lsp-calls-qf) :type 'user-error)
    (should-error (ygg-lsp-call-hierarchy) :type 'user-error)
    (should-not shown)
    (ygg-lsp-type-hierarchy)
    (should (eq shown 'types))))

(ert-deftest lsp-calls-trees-open-when-the-server-has-them ()
  (lsp-calls-tests--with-server '(:callHierarchyProvider)
    (ygg-lsp-call-hierarchy)
    (should (eq shown 'calls))
    (should-error (ygg-lsp-type-hierarchy) :type 'user-error)))

(ert-deftest lsp-calls-say-so-when-nothing-calls ()
  (let ((lsp-calls-tests--responses
         `((:textDocument/prepareCallHierarchy . [,lsp-calls-tests--item])
           (:callHierarchy/incomingCalls . []))))
    (lsp-calls-tests--with-server '(:callHierarchyProvider)
      (should-error (ygg-lsp-calls-qf) :type 'user-error)
      (should-not rows))))

(provide 'lsp-calls-tests)
;;; lsp-calls-tests.el ends here
