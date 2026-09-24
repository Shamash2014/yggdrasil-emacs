;;; ygg-code-verbs.el --- One SPC c verb per concept languages share -*- lexical-binding: t; -*-

;; A concept two languages share gets one SPC c verb; the rest stays local.

;;; Code:

(require 'layer-lsp)
(require 'ygg-device-capture)

(autoload 'ygg-device-log "ygg-device-log" nil t)

(defconst ygg-code-verbs--concepts
  '((build . "build")
    (run . "build and run")
    (clean . "clean")
    (test-at-point . "test at point")
    (test-file . "test file")
    (test-all . "all tests")
    (preview . "preview")
    (device . "device")
    (screenshot . "screenshot")
    (record . "screen recording")
    (logs . "device logs"))
  "Concept -> how it reads in messages.")

(defconst ygg-code-verbs--backends
  '((swift :modes (swift-mode swift-ts-mode)
           :build ygg-swift-build :run ygg-swift-run :clean ygg-swift-clean
           :test-at-point ygg-swift-test-at-point :test-all ygg-swift-test-all
           :preview ygg-swift-preview)
    (cargo :modes (rust-mode rust-ts-mode)
           :build ygg-localleader-rust-build :run ygg-localleader-rust-run
           :test-all ygg-localleader-rust-test)
    (flutter :modes (dart-mode dart-ts-mode)
             :run ygg-localleader-flutter-run :test-file ygg-localleader-flutter-test
             :preview ygg-localleader-flutter-widget-preview)
    (gradle :modes (kotlin-mode kotlin-ts-mode)
            :build ygg-localleader-kotlin-build :run ygg-localleader-kotlin-run
            :clean ygg-localleader-kotlin-clean :test-all ygg-localleader-kotlin-test
            :preview ygg-compose-preview)
    (gradle-java :modes (java-mode java-ts-mode) :when ygg-code-verbs--gradle-p
                 :build ygg-localleader-kotlin-build :run ygg-localleader-kotlin-run
                 :clean ygg-localleader-kotlin-clean :test-all ygg-localleader-kotlin-test)
    (mix :modes (elixir-mode elixir-ts-mode)
         :test-at-point ygg-localleader-elixir-test-line
         :test-file ygg-localleader-elixir-test)
    (python :modes (python-mode python-ts-mode)
            :run ygg-localleader-python-run :test-file ygg-localleader-python-test)
    (ert :modes (emacs-lisp-mode) :test-all ygg-localleader-elisp-run-tests)
    (markdown :modes (markdown-mode gfm-mode) :preview ygg-localleader-markdown-preview)
    (react-native :modes (js-mode js-ts-mode typescript-ts-mode tsx-ts-mode) :when ygg-rn-root
                  :run ygg-rn-run :clean ygg-rn-clean :test-at-point ygg-rn-test-at-point
                  :test-file ygg-rn-test-file :test-all ygg-rn-test-all :preview ygg-rn-preview
                  :device ygg-device-pick :screenshot ygg-device-screenshot
                  :record ygg-device-record-toggle :logs ygg-device-log)
    (devices :modes (dart-mode dart-ts-mode kotlin-mode kotlin-ts-mode
                     java-mode java-ts-mode swift-mode swift-ts-mode)
             :device ygg-device-pick :screenshot ygg-device-screenshot
             :record ygg-device-record-toggle :logs ygg-device-log))
  "Family -> the modes it serves, an optional :when check, and concept -> command.")

(defun ygg-code-verbs--gradle-p ()
  (seq-some (lambda (marker) (locate-dominating-file default-directory marker))
            '("settings.gradle.kts" "settings.gradle" "build.gradle.kts" "build.gradle")))

(defun ygg-code-verbs-resolve (concept &optional mode)
  "The command doing CONCEPT for MODE (default the buffer's), or nil."
  (let ((mode (or mode major-mode))
        (key (intern (format ":%s" concept))))
    (seq-some (lambda (family)
                (let ((spec (cdr family)))
                  (and (apply #'provided-mode-derived-p mode (plist-get spec :modes))
                       (or (null (plist-get spec :when))
                           (funcall (plist-get spec :when)))
                       (plist-get spec key))))
              ygg-code-verbs--backends)))

(defun ygg-code-verbs--dispatch (concept)
  (let ((command (ygg-code-verbs-resolve concept))
        (label (alist-get concept ygg-code-verbs--concepts)))
    (cond
     ((null command) (user-error "No %s for %s" label major-mode))
     ((not (fboundp command)) (user-error "%s: %s is not loaded" label command))
     (t (call-interactively command)))))

(defun ygg-code-build ()
  "Build the project the way this buffer's language does."
  (interactive)
  (ygg-code-verbs--dispatch 'build))

(defun ygg-code-run ()
  "Build and run the project the way this buffer's language does."
  (interactive)
  (ygg-code-verbs--dispatch 'run))

(defun ygg-code-clean ()
  "Clean the project's build output."
  (interactive)
  (ygg-code-verbs--dispatch 'clean))

(defun ygg-code-test-at-point ()
  "Run the test around point."
  (interactive)
  (ygg-code-verbs--dispatch 'test-at-point))

(defun ygg-code-test-file ()
  "Run the tests in this file."
  (interactive)
  (ygg-code-verbs--dispatch 'test-file))

(defun ygg-code-test-all ()
  "Run every test of the project."
  (interactive)
  (ygg-code-verbs--dispatch 'test-all))

(defun ygg-code-device ()
  "Choose the device, emulator or simulator this project runs on."
  (interactive)
  (ygg-code-verbs--dispatch 'device))

(defun ygg-code-preview ()
  "Preview what this buffer draws: SwiftUI, Compose, Flutter widgets, Markdown."
  (interactive)
  (ygg-code-verbs--dispatch 'preview))

(defun ygg-code-screenshot ()
  "Save, show and copy the path of a screenshot of the device this project runs on."
  (interactive)
  (ygg-code-verbs--dispatch 'screenshot))

(defun ygg-code-record ()
  "Start recording the device this project runs on.
When a recording runs, stop it, from whatever buffer."
  (interactive)
  (if (ygg-device-capture-recording-p)
      (ygg-device-record-toggle)
    (ygg-code-verbs--dispatch 'record)))

(defun ygg-code-logs ()
  "Stream the device log, only this project's app where the mode knows one.
Elsewhere stream everything the selected device logs."
  (interactive)
  (if (ygg-code-verbs-resolve 'logs)
      (ygg-code-verbs--dispatch 'logs)
    (ygg-device-log 'unless-streaming)))

(yggdrasil-define-keys 'ygg-leader-code-map
  "b" #'ygg-code-build :label "build"
  "l" #'ygg-code-run :label "build and run"
  "K" #'ygg-code-clean :label "clean"
  "u" #'ygg-code-test-at-point :label "test at point"
  "U" #'ygg-code-test-file :label "test file"
  "T" #'ygg-code-test-all :label "all tests"
  "P" #'ygg-code-preview :label "preview"
  "m" #'ygg-code-device :label "device"
  "y" #'ygg-code-screenshot :label "screenshot"
  "v" #'ygg-code-record :label "record video"
  "L" #'ygg-code-logs :label "device logs")

(provide 'ygg-code-verbs)
;;; ygg-code-verbs.el ends here
