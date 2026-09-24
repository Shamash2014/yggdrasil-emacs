;;; ygg-format-linters.el --- linters flymake-collection lacks or mis-binds -*- lexical-binding: t; -*-

;;; Code:

(require 'flymake-collection-define)

(flymake-collection-define-rx ygg-format-swiftlint
  "Swift style checker using SwiftLint, reading the buffer on stdin."
  :title "swiftlint"
  :pre-let ((swiftlint-exec (executable-find "swiftlint")))
  :pre-check (unless swiftlint-exec
               (error "Cannot find swiftlint executable"))
  :write-type 'pipe
  :command (list swiftlint-exec "lint" "--use-stdin" "--quiet" "--reporter" "xcode")
  :regexps
  ((error bol "<nopath>:" line (opt ":" column) ": error: " (message) eol)
   (warning bol "<nopath>:" line (opt ":" column) ": warning: " (message) eol)))

(flymake-collection-define-rx ygg-format-markdownlint
  "Markdown checker using markdownlint-cli, reading the buffer on stdin.
flymake-collection-markdownlint runs the Ruby mdl instead."
  :title "markdownlint"
  :pre-let ((markdownlint-exec (executable-find "markdownlint")))
  :pre-check (unless markdownlint-exec
               (error "Cannot find markdownlint executable"))
  :write-type 'pipe
  :command (list markdownlint-exec "--stdin")
  :regexps
  ((warning bol "stdin:" line (opt ":" column) " " (+ alpha) " "
            (id "MD" (+ digit)) "/" (message) eol)))

(provide 'ygg-format-linters)
;;; ygg-format-linters.el ends here
