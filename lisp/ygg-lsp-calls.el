;;; ygg-lsp-calls.el --- Call and type hierarchies, as trees and as quickfix lists -*- lexical-binding: t; -*-

;; The trees are eglot's own; what is here refuses early, with a plain
;; message, where the buffer has no server or the server no hierarchy.
;; The quickfix feeder asks the server for the calls into or out of the
;; symbol at point and lists each call site.

;;; Code:

(require 'cl-lib)
(require 'seq)

(declare-function eglot-managed-p "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-server-capable "eglot" (&rest feats))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function eglot-show-call-hierarchy "eglot")
(declare-function eglot-show-type-hierarchy "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function jsonrpc-request "jsonrpc")
(declare-function ygg-qf-from-text "layer-quickfix" (text &optional name replace list))

(defun ygg-lsp-calls--server (capability what)
  "The buffer's server when it offers CAPABILITY, else a user error naming WHAT."
  (unless (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (user-error "No language server in this buffer"))
  (unless (eglot-server-capable capability)
    (user-error "This language server has no %s" what))
  (eglot-current-server))

(defun ygg-lsp-call-hierarchy ()
  "Show the calls into and out of the symbol at point, as a tree."
  (interactive)
  (ygg-lsp-calls--server :callHierarchyProvider "call hierarchy")
  (call-interactively #'eglot-show-call-hierarchy))

(defun ygg-lsp-type-hierarchy ()
  "Show the supertypes and subtypes of the type at point, as a tree."
  (interactive)
  (ygg-lsp-calls--server :typeHierarchyProvider "type hierarchy")
  (call-interactively #'eglot-show-type-hierarchy))

(defun ygg-lsp-calls--row (uri range name)
  "A FILE:LINE:COL: NAME row for RANGE in URI, one-based."
  (let ((start (plist-get range :start)))
    (format "%s:%d:%d: %s" (eglot-uri-to-path uri)
            (1+ (plist-get start :line)) (1+ (plist-get start :character))
            name)))

(defun ygg-lsp-calls--rows (item call outgoing)
  "The rows CALL gives, a call into or, with OUTGOING, out of ITEM.
An outgoing call happens in ITEM's own file, where its ranges point."
  (let* ((other (plist-get call (if outgoing :to :from)))
         (uri (plist-get (if outgoing item other) :uri))
         (ranges (plist-get call :fromRanges))
         (name (plist-get other :name)))
    (if (seq-empty-p ranges)
        (list (ygg-lsp-calls--row (plist-get other :uri)
                                  (plist-get other :selectionRange) name))
      (mapcar (lambda (range) (ygg-lsp-calls--row uri range name)) ranges))))

(defun ygg-lsp-calls-qf (&optional outgoing)
  "List the call sites of the symbol at point in the quickfix.
With a prefix argument OUTGOING, the calls it makes instead."
  (interactive "P")
  (let* ((server (ygg-lsp-calls--server :callHierarchyProvider "call hierarchy"))
         (items (jsonrpc-request server :textDocument/prepareCallHierarchy
                                 (eglot--TextDocumentPositionParams)))
         (method (if outgoing :callHierarchy/outgoingCalls
                   :callHierarchy/incomingCalls))
         rows)
    (when (seq-empty-p items) (user-error "No call hierarchy here"))
    (seq-doseq (item items)
      (seq-doseq (call (jsonrpc-request server method (list :item item)))
        (setq rows (nconc rows (ygg-lsp-calls--rows item call outgoing)))))
    (unless rows
      (user-error "No %s calls" (if outgoing "outgoing" "incoming")))
    (ygg-qf-from-text (string-join rows "\n")
                      (format "%s %s" (if outgoing "calls from" "calls into")
                              (plist-get (seq-first items) :name))
                      t)))

(provide 'ygg-lsp-calls)
;;; ygg-lsp-calls.el ends here
