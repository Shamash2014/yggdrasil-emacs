;;; ygg-dap-ios.el --- Debugging iOS apps on the simulator with lldb-dap -*- lexical-binding: t; -*-

;;; Code:

(require 'map)
(require 'subr-x)

(defvar dape-configs)
(declare-function ygg-dap-lldb-local-command "ygg-dap-lldb")
(declare-function ygg-swift-build-command "layer-swift")
(declare-function ygg-swift-launch-waiting-for-debugger "layer-swift")

(defun ygg-dap-ios-lldb-dap ()
  "The lldb-dap to attach with, Xcode's when PATH has none."
  (if (fboundp 'ygg-dap-lldb-local-command)
      (ygg-dap-lldb-local-command)
    (or (executable-find "lldb-dap")
        (string-trim (shell-command-to-string "xcrun -f lldb-dap")))))

(defun ygg-dap-ios-build ()
  "The xcodebuild line for the scheme and simulator, from layer-swift."
  (if (fboundp 'ygg-swift-build-command)
      (ygg-swift-build-command)
    (user-error "ios-simulator needs layer-swift")))

(defun ygg-dap-ios-launch (config)
  "CONFIG attached to its app on the simulator, launched once the build passed."
  (cond
   ((and (plist-get config 'compile) (not (plist-get config 'built)))
    (plist-put (plist-put (copy-sequence config) 'built t) 'source-dir default-directory))
   ((fboundp 'ygg-swift-launch-waiting-for-debugger)
    (let* ((default-directory (or (plist-get config 'source-dir) default-directory))
           (launch (ygg-swift-launch-waiting-for-debugger))
           (config (copy-sequence config)))
      (plist-put (plist-put config :pid (plist-get launch :pid))
                 :program (plist-get launch :program))))
   (t (user-error "ios-simulator needs layer-swift"))))

(defun ygg-dap-ios-entry (lldb-dap)
  "Entry from dape's LLDB-DAP that builds, launches on the simulator, attaches."
  (map-merge 'plist (map-delete (copy-sequence lldb-dap) :program)
             '(modes (swift-mode swift-ts-mode)
               command ygg-dap-ios-lldb-dap
               compile ygg-dap-ios-build
               fn ygg-dap-ios-launch
               :request "attach")))

(defun ygg-dap-ios-install ()
  "Add the ios-simulator entry after the lldb-dap ones."
  (when-let* ((lldb-dap (alist-get 'lldb-dap dape-configs)))
    (setq dape-configs (assq-delete-all 'ios-simulator dape-configs))
    (setq dape-configs
          (append dape-configs
                  (list (cons 'ios-simulator (ygg-dap-ios-entry lldb-dap)))))))

(with-eval-after-load 'dape
  (ygg-dap-ios-install))

(provide 'ygg-dap-ios)
;;; ygg-dap-ios.el ends here
