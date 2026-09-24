;;; ygg-device-log.el --- Live logs of the selected device -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'url-util)
(require 'ygg-device)
(require 'yggdrasil-localleader)

(declare-function ygg-swift-project "layer-swift")
(declare-function ygg-swift--app-settings "layer-swift")
(declare-function ygg-rn-root "layer-react-native")
(defvar ygg-swift--running)
(defvar ygg-swift--console)
(defvar ygg-modal-special-modes)

(defgroup ygg-device-log nil
  "Live logs of the selected device."
  :group 'ygg-device
  :prefix "ygg-device-log-")

(defcustom ygg-device-log-max-lines 25000
  "Lines a log buffer may hold before its oldest lines go."
  :type 'natnum)

(defcustom ygg-device-log-trim-lines 20000
  "Lines a log buffer keeps when it passes the maximum."
  :type 'natnum)

(defcustom ygg-device-log-pause-max-chars 4000000
  "Characters queued while paused before the oldest queued output is dropped."
  :type 'natnum)

(defcustom ygg-device-log-recheck-seconds 10
  "Seconds between checks that the app filter still names the installed app."
  :type 'number)

(defface ygg-device-log-debug '((t :inherit shadow))
  "Verbose and debug lines.")

(defface ygg-device-log-warn '((t :inherit bold))
  "Warnings.")

(defface ygg-device-log-error '((t :inherit bold))
  "Errors.")

(defface ygg-device-log-fault '((t :inherit bold :underline t))
  "Faults, asserts and crashes.")

(defface ygg-device-log-meta '((t :inherit shadow))
  "Timestamps, process and thread ids, and lines the source adds.")

(defconst ygg-device-log-levels '(verbose debug info warn error fault)
  "Log levels from the least to the most severe.")

(defconst ygg-device-log--level-faces
  '((verbose . ygg-device-log-debug) (debug . ygg-device-log-debug)
    (warn . ygg-device-log-warn) (error . ygg-device-log-error)
    (fault . ygg-device-log-fault) (meta . ygg-device-log-meta)))

;;; Lines

(defconst ygg-device-log--logcat-regexp
  "\\`\\([0-9]+-[0-9]+ [0-9:.]+ +[0-9]+ +[0-9]+\\) \\([VDIWEFA]\\) ")

(defconst ygg-device-log--oslog-regexp
  "\\`\\([0-9]+-[0-9]+-[0-9]+ [0-9:.]+ \\([[:alpha:]]\\{1,2\\}\\)\\) *[^[\n]*\\(\\[[0-9]+:[[:xdigit:]]+\\]\\)")

(defconst ygg-device-log--syslog-regexp
  "\\`\\([[:alpha:]]\\{3\\} +[0-9]+ [0-9:.]+\\) [^ \n]+ [^[\n]*\\(\\[[0-9]+\\]\\) <\\([[:alpha:]]+\\)>: ")

(defconst ygg-device-log--meta-regexp
  "\\`\\(?:--------- \\|Timestamp  \\|Filtering the log data\\|\\[log \\)")

(defun ygg-device-log-parse-line (line format)
  "(LEVEL . SHADED-RANGES) of LINE in FORMAT, or nil when LINE continues the last.
FORMAT is logcat, oslog or syslog; meta is the level of lines the source adds."
  (cond
   ((string-match-p ygg-device-log--meta-regexp line) (list 'meta))
   ((eq format 'logcat)
    (when (string-match ygg-device-log--logcat-regexp line)
      (cons (pcase (aref line (match-beginning 2))
              (?V 'verbose) (?D 'debug) (?I 'info) (?W 'warn) (?E 'error) (_ 'fault))
            (list (cons 0 (match-end 1))))))
   ((eq format 'oslog)
    (when (string-match ygg-device-log--oslog-regexp line)
      (cons (pcase (match-string 2 line)
              ("Db" 'debug) ("E" 'error) ("F" 'fault) (_ 'info))
            (list (cons 0 (match-end 1))
                  (cons (match-beginning 3) (match-end 3))))))
   ((eq format 'syslog)
    (when (string-match ygg-device-log--syslog-regexp line)
      (cons (pcase (match-string 3 line)
              ("Debug" 'debug) ((or "Info" "Notice") 'info) ("Warning" 'warn) ("Error" 'error)
              (_ 'fault))
            (list (cons 0 (match-end 1))
                  (cons (match-beginning 2) (match-end 2))))))))

(defun ygg-device-log--hidden-levels (threshold)
  "The levels less severe than THRESHOLD."
  (seq-take-while (lambda (level) (not (eq level threshold))) ygg-device-log-levels))

;;; Buffer state

(defvar-local ygg-device-log--device nil)
(defvar-local ygg-device-log--root nil)
(defvar-local ygg-device-log--app nil
  "What the project names its app: (:package P) or (:bundle B :exe E).")
(defvar-local ygg-device-log--resolved nil
  "What the device says about the app: (:uid N) or (:exe E), or nil.")
(defvar-local ygg-device-log--process nil)
(defvar-local ygg-device-log--timer nil)
(defvar-local ygg-device-log--format 'logcat)
(defvar-local ygg-device-log--pending "")
(defvar-local ygg-device-log--last-level 'info)
(defvar-local ygg-device-log--lines 0)
(defvar-local ygg-device-log--threshold 'verbose)
(defvar-local ygg-device-log--filter nil
  "(keep . REGEXP) or (hide . REGEXP), or nil.")
(defvar-local ygg-device-log--paused nil)
(defvar-local ygg-device-log--queue nil)
(defvar-local ygg-device-log--queued 0)
(defvar-local ygg-device-log--dropped 0)
(defvar-local ygg-device-log--follow t)
(defvar-local ygg-device-log--generation 0
  "Counts starts, so a device answer for an older start is ignored.")

(defun ygg-device-log--update-spec ()
  (setq buffer-invisibility-spec
        (append (ygg-device-log--hidden-levels ygg-device-log--threshold)
                (and ygg-device-log--filter '(ygg-device-log-unmatched))))
  (force-window-update (current-buffer)))

(defun ygg-device-log--unmatched-p (line)
  (pcase ygg-device-log--filter
    (`(keep . ,regexp) (not (string-match-p regexp line)))
    (`(hide . ,regexp) (string-match-p regexp line))))

(defun ygg-device-log--invisible (level line)
  (if (ygg-device-log--unmatched-p line) (list level 'ygg-device-log-unmatched) (list level)))

(defun ygg-device-log--line (line)
  "LINE with its newline, faces and invisibility."
  (let* ((line (string-remove-suffix "\r" line))
         (parsed (ygg-device-log-parse-line line ygg-device-log--format))
         (level (if parsed (car parsed) ygg-device-log--last-level))
         (face (alist-get level ygg-device-log--level-faces))
         (text (concat line "\n")))
    (when (and parsed (not (eq level 'meta)))
      (setq ygg-device-log--last-level level))
    (when face (put-text-property 0 (length text) 'face face text))
    (pcase-dolist (`(,beg . ,end) (cdr parsed))
      (put-text-property beg end 'face 'ygg-device-log-meta text))
    (put-text-property 0 (length text) 'invisible (ygg-device-log--invisible level line) text)
    text))

(defun ygg-device-log--trim ()
  (when (> ygg-device-log--lines ygg-device-log-max-lines)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (forward-line (- ygg-device-log--lines ygg-device-log-trim-lines))
        (delete-region (point-min) (point))))
    (setq ygg-device-log--lines ygg-device-log-trim-lines)))

(defun ygg-device-log--insert (chunk)
  "Add the whole lines of CHUNK to the end, keeping its unfinished last line."
  (let* ((text (concat ygg-device-log--pending chunk))
         (cut (let ((i (length text))) (while (and (> i 0) (/= (aref text (1- i)) ?\n)) (cl-decf i)) i)))
    (setq ygg-device-log--pending (substring text cut))
    (when (> cut 0)
      (ygg-device-log--append (butlast (split-string (substring text 0 cut) "\n"))))))

(defun ygg-device-log--append (lines)
  "Add the whole LINES to the end, moving the windows that follow it."
  (let* ((out (mapconcat #'ygg-device-log--line lines ""))
         (at-end (= (point) (point-max)))
         (tail (and ygg-device-log--follow
                    (seq-filter (lambda (window) (= (window-point window) (point-max)))
                                (get-buffer-window-list nil nil t)))))
    (let ((inhibit-read-only t))
      (save-excursion (goto-char (point-max)) (insert out)))
    (cl-incf ygg-device-log--lines (length lines))
    (ygg-device-log--trim)
    (when (and at-end ygg-device-log--follow) (goto-char (point-max)))
    (dolist (window tail) (set-window-point window (point-max)))))

(defun ygg-device-log--note (format-string &rest args)
  (ygg-device-log--append (list (apply #'format (concat "[log " format-string "]") args))))

(defun ygg-device-log--enqueue (chunk)
  (push chunk ygg-device-log--queue)
  (cl-incf ygg-device-log--queued (length chunk))
  (while (> ygg-device-log--queued ygg-device-log-pause-max-chars)
    (let ((oldest (car (last ygg-device-log--queue))))
      (setq ygg-device-log--queue (butlast ygg-device-log--queue))
      (cl-decf ygg-device-log--queued (length oldest))
      (cl-incf ygg-device-log--dropped (length oldest)))))

(defun ygg-device-log--filter-output (process chunk)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process ygg-device-log--process)
          (if ygg-device-log--paused
              (ygg-device-log--enqueue chunk)
            (ygg-device-log--insert chunk)))))))

(defun ygg-device-log--refilter ()
  "Recompute which lines the text filter hides."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((beg (point))
               (end (progn (forward-line 1) (point)))
               (level (car (get-text-property beg 'invisible))))
          (put-text-property beg end 'invisible
                             (ygg-device-log--invisible
                              level (buffer-substring-no-properties beg (max beg (1- end))))))))))

;;; Sources

(defun ygg-device-log--nspredicate-string (string)
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" string) "\""))

(defun ygg-device-log-predicate (exe bundle)
  "The log stream predicate for the process EXE and the subsystems under BUNDLE."
  (let ((parts (delq nil (list (and exe (concat "process == " (ygg-device-log--nspredicate-string exe)))
                               (and bundle (concat "subsystem BEGINSWITH "
                                                   (ygg-device-log--nspredicate-string bundle)))))))
    (and parts (string-join parts " OR "))))

(defun ygg-device-log--stream-args (predicate)
  (append '("stream" "--style" "compact" "--level" "debug")
          (and predicate (list "--predicate" predicate))))

(defun ygg-device-log-command (device app resolved)
  "The program and arguments streaming DEVICE's log.
APP is what the project names its app, RESOLVED what the device said of it."
  (let ((id (plist-get device :id))
        (exe (or (plist-get resolved :exe) (plist-get app :exe)))
        (bundle (plist-get app :bundle)))
    (pcase (plist-get device :platform)
      ('android
       (append (list (ygg-device--adb) "-s" id "logcat" "-v" "threadtime" "-T" "1"
                     "-b" "main,system,crash")
               (and-let* ((uid (plist-get resolved :uid))) (list (format "--uid=%d" uid)))))
      ('ios
       (append (list (ygg-device--xcrun) "simctl" "spawn" id "log")
               (ygg-device-log--stream-args (and app (ygg-device-log-predicate exe bundle)))))
      ('ios-device
       (append (list (or (executable-find "idevicesyslog") "idevicesyslog") "-u" id "-K")
               (and exe (list "-p" exe))))
      ('macos
       (cons "/usr/bin/log"
             (ygg-device-log--stream-args (and app (ygg-device-log-predicate exe bundle)))))
      (platform (user-error "No log source for %s devices" platform)))))

(defun ygg-device-log-clear-command (device)
  "The program and arguments emptying DEVICE's own log, or nil when it has none."
  (when (eq (plist-get device :platform) 'android)
    (list (ygg-device--adb) "-s" (plist-get device :id) "logcat" "-c")))

(defun ygg-device-log--format-of (device)
  (pcase (plist-get device :platform)
    ('android 'logcat)
    ('ios-device 'syslog)
    (_ 'oslog)))

;;; The app

(defun ygg-device-log-parse-uid (output package)
  "The uid pm list packages -U printed for exactly PACKAGE in OUTPUT, or nil."
  (when (and output
             (string-match (concat "^package:" (regexp-quote package) " uid:\\([0-9]+\\)")
                           output))
    (string-to-number (match-string 1 output))))

(defun ygg-device-log-parse-appinfo-executable (output)
  "The CFBundleExecutable simctl appinfo printed in OUTPUT, or nil."
  (when (and output
             (string-match "^[ \t]*CFBundleExecutable = \"?\\([^\";\n]+\\)\"?;" output))
    (match-string 1 output)))

(defun ygg-device-log-parse-gradle-package (text)
  "The applicationId in Gradle build TEXT, else its namespace, or nil."
  (seq-some (lambda (key)
              (when (string-match (concat "^[ \t]*" key "[ \t]*=?[ \t]*[\"']\\([[:alnum:]_.]+\\)[\"']")
                                  text)
                (match-string 1 text)))
            '("applicationId" "namespace")))

(defun ygg-device-log-parse-pbxproj-bundle (text)
  "The app bundle id in project.pbxproj TEXT, skipping test targets and variables."
  (let ((start 0) found)
    (while (string-match "PRODUCT_BUNDLE_IDENTIFIER = \"?\\([^\";\n]+\\)\"?;" text start)
      (let ((id (match-string 1 text)))
        (unless (or (string-match-p "\\$" id) (string-match-p "Tests\\'" id))
          (push id found)))
      (setq start (match-end 0)))
    (car (last found))))

(defun ygg-device-log--read (file)
  (and (file-readable-p file)
       (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun ygg-device-log--expo (root platform key)
  (when-let* ((text (ygg-device-log--read (expand-file-name "app.json" root)))
              (json (ignore-errors (json-parse-string text :object-type 'plist))))
    (plist-get (plist-get (or (plist-get json :expo) json) platform) key)))

(defun ygg-device-log--android-package (root)
  (or (seq-some (lambda (file)
                  (and-let* ((text (ygg-device-log--read (expand-file-name file root))))
                    (ygg-device-log-parse-gradle-package text)))
                '("app/build.gradle.kts" "app/build.gradle"
                  "android/app/build.gradle.kts" "android/app/build.gradle"))
      (ygg-device-log--expo root :android :package)))

(defun ygg-device-log--xcodeproj (root platform)
  (seq-some (lambda (dir)
              (car (file-expand-wildcards (expand-file-name "*.xcodeproj" (expand-file-name dir root)))))
            (if (eq platform 'macos) '("macos" ".") '("ios" "."))))

(defun ygg-device-log--apple-app-from-files (root platform)
  (let* ((xcodeproj (ygg-device-log--xcodeproj root platform))
         (bundle (or (ygg-device-log--expo root :ios :bundleIdentifier)
                     (and xcodeproj
                          (and-let* ((text (ygg-device-log--read
                                            (expand-file-name "project.pbxproj" xcodeproj))))
                            (ygg-device-log-parse-pbxproj-bundle text))))))
    (and bundle
         (list :bundle bundle
               :exe (and xcodeproj (file-name-base (directory-file-name xcodeproj)))))))

(defun ygg-device-log--swift-app (device)
  (when-let* (((fboundp 'ygg-swift-project))
              (project (ygg-swift-project))
              ((eq (plist-get project :kind) 'xcode)))
    (let ((running (and (boundp 'ygg-swift--running) ygg-swift--running)))
      (if (and running (equal (plist-get (car running) :id) (plist-get device :id))
               (eq (plist-get device :platform) 'ios))
          (list :bundle (cdr running))
        (condition-case nil
            (let ((settings (ygg-swift--app-settings project device)))
              (list :bundle (plist-get settings :PRODUCT_BUNDLE_IDENTIFIER)
                    :exe (plist-get settings :EXECUTABLE_NAME)))
          (error nil))))))

(defun ygg-device-log--root ()
  (or (and (fboundp 'ygg-rn-root) (ygg-rn-root))
      (seq-some (lambda (marker) (locate-dominating-file default-directory marker))
                '("pubspec.yaml" "app.json" "settings.gradle.kts" "settings.gradle"))
      (and-let* ((project (project-current))) (project-root project))))

(defun ygg-device-log--project-app (root device)
  "What the project at ROOT names its app for DEVICE, or nil."
  (let ((root (and root (file-name-as-directory (expand-file-name root)))))
    (pcase (plist-get device :platform)
      ('android (and root (and-let* ((package (ygg-device-log--android-package root)))
                            (list :package package))))
      ((or 'ios 'ios-device 'macos)
       (or (and (not (and root (seq-some (lambda (file) (file-exists-p (expand-file-name file root)))
                                         '("app.json" "pubspec.yaml"))))
                (ygg-device-log--swift-app device))
           (and root (ygg-device-log--apple-app-from-files root (plist-get device :platform))))))))

(defun ygg-device-log--resolve (device app callback)
  "Call CALLBACK with what DEVICE says about APP: (:uid N), (:exe E) or nil."
  (pcase (plist-get device :platform)
    ((and 'android (guard (plist-get app :package)))
     (let ((package (plist-get app :package)))
       (funcall ygg-device-run-function (ygg-device--adb)
                (list "-s" (plist-get device :id) "shell" "pm" "list" "packages" "-U" package)
                ygg-device-list-timeout
                (lambda (output)
                  (funcall callback (and-let* ((uid (ygg-device-log-parse-uid output package)))
                                      (list :uid uid)))))))
    ((and 'ios (guard (plist-get app :bundle)))
     (funcall ygg-device-run-function (ygg-device--xcrun)
              (list "simctl" "appinfo" (plist-get device :id) (plist-get app :bundle))
              ygg-device-list-timeout
              (lambda (output)
                (funcall callback (and-let* ((exe (ygg-device-log-parse-appinfo-executable output)))
                                    (list :exe exe))))))
    (_ (funcall callback nil))))

;;; Process

(defun ygg-device-log--stop ()
  (when ygg-device-log--timer (cancel-timer ygg-device-log--timer))
  (setq ygg-device-log--timer nil)
  (when-let* ((process ygg-device-log--process))
    (setq ygg-device-log--process nil)
    (when (process-live-p process)
      (signal-process process 'TERM)
      (run-at-time 2 nil (lambda () (when (process-live-p process) (delete-process process)))))))

(defun ygg-device-log--stderr-buffer ()
  (get-buffer-create (format " *log stderr: %s*" (plist-get ygg-device-log--device :name))))

(defun ygg-device-log--sentinel (process event)
  (let ((buffer (process-buffer process)))
    (when (memq (process-status process) '(exit signal))
      (when-let* ((stderr (process-get process 'stderr))) (delete-process stderr)))
    (when (and (buffer-live-p buffer) (memq (process-status process) '(exit signal)))
      (with-current-buffer buffer
        (when (eq process ygg-device-log--process)
          (setq ygg-device-log--process nil)
          (when ygg-device-log--timer (cancel-timer ygg-device-log--timer))
          (setq ygg-device-log--timer nil)
          (ygg-device-log--note "source ended: %s" (string-trim event)))))))

(defun ygg-device-log--spawn ()
  (let* ((command (ygg-device-log-command ygg-device-log--device ygg-device-log--app
                                          ygg-device-log--resolved))
         (default-directory temporary-file-directory)
         (stderr (make-pipe-process :name "ygg-device-log-stderr" :noquery t :sentinel #'ignore
                                    :buffer (ygg-device-log--stderr-buffer))))
    (setq ygg-device-log--process
          (condition-case err
              (make-process :name "ygg-device-log" :buffer (current-buffer) :command command
                            :connection-type 'pipe :stderr stderr :noquery t :file-handler nil
                            :coding 'utf-8
                            :filter #'ygg-device-log--filter-output
                            :sentinel #'ygg-device-log--sentinel)
            (error (delete-process stderr)
                   (ygg-device-log--note "could not start %s: %s" (car command)
                                         (error-message-string err))
                   nil)))
    (when ygg-device-log--process
      (process-put ygg-device-log--process 'stderr stderr))
    (when (and ygg-device-log--process ygg-device-log--app (memq (plist-get ygg-device-log--device :platform) '(android ios)))
      (setq ygg-device-log--timer
            (run-at-time ygg-device-log-recheck-seconds ygg-device-log-recheck-seconds
                         #'ygg-device-log--recheck (current-buffer))))
    (force-mode-line-update)))

(defun ygg-device-log--start ()
  "Stop the source, ask the device about the app, then stream."
  (ygg-device-log--stop)
  (setq ygg-device-log--pending "")
  (let ((buffer (current-buffer))
        (generation (cl-incf ygg-device-log--generation)))
    (ygg-device-log--resolve
     ygg-device-log--device ygg-device-log--app
     (lambda (resolved)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation ygg-device-log--generation)
             (setq ygg-device-log--resolved resolved)
             (ygg-device-log--spawn))))))))

(defun ygg-device-log--recheck (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((was ygg-device-log--resolved))
        (ygg-device-log--resolve
         ygg-device-log--device ygg-device-log--app
         (lambda (resolved)
           (when (and (buffer-live-p buffer) resolved (not (equal resolved was)))
             (with-current-buffer buffer
               (when (equal was ygg-device-log--resolved)
                 (ygg-device-log--note "app changed: %S" resolved)
                 (ygg-device-log--start))))))))))

(defun ygg-device-log--on-device-changed ()
  "Give log buffers of a device that came back up under a new id the new id."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'ygg-device-log-mode)
        (let ((current (ygg-device-current))
              (mine ygg-device-log--device))
          (when (and (plist-get current :id)
                     (eq (plist-get current :platform) (plist-get mine :platform))
                     (equal (plist-get current :name) (plist-get mine :name))
                     (or (not (equal (plist-get current :id) (plist-get mine :id)))
                         (not (process-live-p ygg-device-log--process))))
            (setq ygg-device-log--device current)
            (with-demoted-errors "Device log: %S" (ygg-device-log--start))))))))

(add-hook 'ygg-device-changed-hook #'ygg-device-log--on-device-changed)

;;; Header

(defun ygg-device-log--app-label ()
  (let ((app ygg-device-log--app)
        (resolved ygg-device-log--resolved))
    (cond
     ((null app) "whole device")
     ((plist-get app :package)
      (if-let* ((uid (plist-get resolved :uid)))
          (format "%s uid %d" (plist-get app :package) uid)
        (format "%s not installed, whole device" (plist-get app :package))))
     ((and (eq (plist-get ygg-device-log--device :platform) 'ios-device)
           (not (plist-get app :exe)))
      (format "%s has no process name, whole device" (plist-get app :bundle)))
     (t (format "%s %s" (plist-get app :bundle)
                (or (plist-get resolved :exe) (plist-get app :exe) ""))))))

(defun ygg-device-log--header ()
  (let ((dim (lambda (s) (propertize s 'face 'shadow))))
    (string-replace
     "%" "%%"
     (concat " " (plist-get ygg-device-log--device :name)
            (funcall dim "  ") (ygg-device-log--app-label)
            (funcall dim "  from ") (symbol-name ygg-device-log--threshold)
            (pcase ygg-device-log--filter
              (`(,kind . ,regexp) (concat (funcall dim (format "  %s " kind)) regexp)))
            (cond (ygg-device-log--paused
                   (format "  paused, %d chars queued" ygg-device-log--queued))
                  ((not ygg-device-log--follow) "  not following")
                  (t ""))
            (if (process-live-p ygg-device-log--process) "" (funcall dim "  stopped"))))))

;;; Frames

(defconst ygg-device-log--frame-regexps
  '((jvm . "at [[:alnum:]_$.<>]+(\\([[:alnum:]_$]+\\.\\(?:kt\\|java\\)\\):\\([0-9]+\\))")
    (swift . "\\(/[^ :\n\"]+\\.swift\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?")
    (dart . "\\(package:[[:alnum:]_]+/[^ :)\n]+\\.dart\\|file://[^ :)\n]+\\.dart\\|[[:alnum:]_/.-]+\\.dart\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?")
    (js . "\\([^ ()\n\"']+\\.\\(?:[jt]sx?\\|mjs\\)\\):\\([0-9]+\\):\\([0-9]+\\)"))
  "Kind -> regexp whose groups are the path, the line and the column.")

(defun ygg-device-log-frames (line)
  "The source locations in LINE as plists of :kind :path :line :col :start :end."
  (let (frames)
    (pcase-dolist (`(,kind . ,regexp) ygg-device-log--frame-regexps)
      (let ((start 0))
        (while (string-match regexp line start)
          (setq start (match-end 0))
          (let ((path (match-string 1 line)))
            (unless (and (eq kind 'js) (string-match-p "://" path))
              (push (list :kind kind :path path
                          :line (string-to-number (match-string 2 line))
                          :col (and (match-string 3 line) (string-to-number (match-string 3 line)))
                          :start (match-beginning 0) :end (match-end 0))
                    frames))))))
    (let (kept)
      (dolist (frame (sort frames (lambda (a b) (< (plist-get a :start) (plist-get b :start)))))
        (unless (and kept (< (plist-get frame :start) (plist-get (car kept) :end)))
          (push frame kept)))
      (nreverse kept))))

(defun ygg-device-log-dart-path (path package root)
  "The file PATH names, with package:PACKAGE/ as ROOT's lib.
Nil for the other packages."
  (cond
   ((string-prefix-p "file://" path) (url-unhex-string (substring path 7)))
   ((string-match "\\`package:\\([^/]+\\)/\\(.*\\)\\'" path)
    (and package (equal (match-string 1 path) package)
         (expand-file-name (concat "lib/" (match-string 2 path)) root)))
   (t (expand-file-name path root))))

(defun ygg-device-log--pubspec-name (root)
  (and-let* ((text (ygg-device-log--read (expand-file-name "pubspec.yaml" root)))
             ((string-match "^name:[ \t]*\\([[:alnum:]_]+\\)" text)))
    (match-string 1 text)))

(defun ygg-device-log--project-files-named (root suffix)
  (and-let* ((project (project-current nil root)))
    (seq-filter (lambda (file) (string-suffix-p suffix file)) (project-files project))))

(defun ygg-device-log--frame-file (frame root)
  (let* ((path (plist-get frame :path))
         (direct (pcase (plist-get frame :kind)
                   ('jvm nil)
                   ('dart (ygg-device-log-dart-path path (and root (ygg-device-log--pubspec-name root))
                                                    (or root default-directory)))
                   (_ (expand-file-name path (or root default-directory))))))
    (if (and direct (file-exists-p direct))
        direct
      (let* ((name (concat "/" (file-name-nondirectory path)))
             (matches (and root (ygg-device-log--project-files-named root name))))
        (pcase matches
          ('nil (user-error "No %s in %s" (substring name 1) (or root "a project")))
          (`(,one) one)
          (_ (completing-read "File: " matches nil t)))))))

(defun ygg-device-log--line-string ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun ygg-device-log-visit ()
  "Open the source location on this line, the one under point first."
  (interactive)
  (let* ((column (- (point) (line-beginning-position)))
         (frames (or (ygg-device-log-frames (ygg-device-log--line-string))
                     (user-error "No source location on this line")))
         (frame (or (seq-find (lambda (f) (<= (plist-get f :start) column (plist-get f :end))) frames)
                    (car frames)))
         (file (ygg-device-log--frame-file frame ygg-device-log--root)))
    (pop-to-buffer (find-file-noselect file))
    (goto-char (point-min))
    (forward-line (1- (plist-get frame :line)))
    (when-let* ((col (plist-get frame :col))) (move-to-column (max 0 (1- col))))))

(defun ygg-device-log--seek-frame (direction)
  (let ((origin (point)) found)
    (while (and (not found) (zerop (forward-line direction)))
      (unless (invisible-p (point))
        (when-let* ((frame (car (ygg-device-log-frames (ygg-device-log--line-string)))))
          (setq found (+ (line-beginning-position) (plist-get frame :start))))))
    (goto-char (or found origin))
    (unless found (user-error "No more source locations"))))

(defun ygg-device-log-next-frame ()
  "Move to the next shown line naming a source location."
  (interactive)
  (ygg-device-log--seek-frame 1))

(defun ygg-device-log-previous-frame ()
  "Move to the previous shown line naming a source location."
  (interactive)
  (ygg-device-log--seek-frame -1))

;;; Commands

(defun ygg-device-log-set-level (level)
  "Show lines from LEVEL up; the rest stay hidden until the level drops."
  (interactive (list (intern (completing-read "Show from: " (mapcar #'symbol-name ygg-device-log-levels)
                                              nil t nil nil
                                              (symbol-name ygg-device-log--threshold)))))
  (setq ygg-device-log--threshold level)
  (ygg-device-log--update-spec))

(defun ygg-device-log--set-filter (kind regexp)
  (setq ygg-device-log--filter (and regexp (not (string-empty-p regexp)) (cons kind regexp)))
  (ygg-device-log--refilter)
  (ygg-device-log--update-spec))

(defun ygg-device-log-keep (regexp)
  "Show only lines matching REGEXP; empty shows every line again."
  (interactive (list (read-regexp "Keep lines matching (empty for all)")))
  (ygg-device-log--set-filter 'keep regexp))

(defun ygg-device-log-hide (regexp)
  "Hide lines matching REGEXP; empty shows every line again."
  (interactive (list (read-regexp "Hide lines matching (empty for none)")))
  (ygg-device-log--set-filter 'hide regexp))

(defun ygg-device-log-pause ()
  "Stop showing new lines and queue them; again shows what was queued."
  (interactive)
  (if (not ygg-device-log--paused)
      (setq ygg-device-log--paused t)
    (let ((queued (apply #'concat (nreverse ygg-device-log--queue)))
          (dropped ygg-device-log--dropped))
      (setq ygg-device-log--paused nil ygg-device-log--queue nil
            ygg-device-log--queued 0 ygg-device-log--dropped 0)
      (when (> dropped 0)
        (ygg-device-log--note "%d chars dropped while paused" dropped)
        (setq ygg-device-log--pending ""
              queued (substring queued (or (and-let* ((newline (string-search "\n" queued)))
                                             (1+ newline))
                                           (length queued)))))
      (ygg-device-log--insert queued)))
  (force-mode-line-update))

(defun ygg-device-log-follow ()
  "Toggle keeping windows at the newest line."
  (interactive)
  (setq ygg-device-log--follow (not ygg-device-log--follow))
  (when ygg-device-log--follow
    (goto-char (point-max))
    (dolist (window (get-buffer-window-list nil nil t)) (set-window-point window (point-max))))
  (force-mode-line-update))

(defun ygg-device-log-clear (&optional device-too)
  "Empty the buffer; with DEVICE-TOO, also empty an Android device's log."
  (interactive "P")
  (let ((inhibit-read-only t)) (erase-buffer))
  (setq ygg-device-log--lines 0)
  (when-let* ((command (and device-too (ygg-device-log-clear-command ygg-device-log--device))))
    (funcall ygg-device-run-function (car command) (cdr command) ygg-device-list-timeout #'ignore)))

(defun ygg-device-log-restart ()
  "Ask the device about the app again and restart the source."
  (interactive)
  (ygg-device-log--note "restarting")
  (ygg-device-log--start))

(defun ygg-device-log-save (file)
  "Write every line, shown or hidden, to FILE and open it."
  (interactive
   (list (read-file-name "Save log to: " nil nil nil
                         (format "%s-%s.log"
                                 (replace-regexp-in-string "[^[:alnum:]_-]+" "-"
                                                           (plist-get ygg-device-log--device :name))
                                 (format-time-string "%Y%m%d-%H%M%S")))))
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-file file (insert text)))
  (find-file file))

(defun ygg-device-log-app-output ()
  "Show the app's own stdout and stderr, which the device log does not carry."
  (interactive)
  (if-let* ((buffer (and (boundp 'ygg-swift--console) (get-buffer ygg-swift--console))))
      (display-buffer buffer)
    (user-error "No app output buffer; launch the app with SPC c l")))

(defun ygg-device-log-stderr ()
  "Show what the log source printed on stderr."
  (interactive)
  (display-buffer (ygg-device-log--stderr-buffer)))

;;; Mode

(defvar-keymap ygg-device-log-mode-map
  "RET" #'ygg-device-log-visit
  "<remap> <ygg-goto-file>" #'ygg-device-log-visit)

(define-derived-mode ygg-device-log-mode special-mode "Log"
  "Live log of one device."
  (setq-local font-lock-function #'ignore
              font-lock-fontify-region-function #'ignore
              font-lock-unfontify-region-function #'ignore
              buffer-undo-list t
              truncate-lines t
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              buffer-invisibility-spec nil
              header-line-format '(:eval (ygg-device-log--header)))
  (add-hook 'kill-buffer-hook #'ygg-device-log--on-kill nil t)
  (add-hook 'after-change-major-mode-hook #'ygg-device-log--font-lock-off 90 t))

(defun ygg-device-log--font-lock-off ()
  "Undo global font lock, whose refontifying would strip the faces lines carry."
  (font-lock-mode -1))

(defun ygg-device-log--on-kill ()
  (let ((process ygg-device-log--process))
    (ygg-device-log--stop)
    (when process
      (set-process-buffer process nil)
      (set-process-sentinel process
                            (lambda (process _event)
                              (when-let* ((stderr (process-get process 'stderr)))
                                (unless (process-live-p process) (delete-process stderr)))))))
  (when-let* ((stderr (get-buffer (format " *log stderr: %s*" (plist-get ygg-device-log--device :name)))))
    (kill-buffer stderr)))

(add-to-list 'ygg-modal-special-modes 'ygg-device-log-mode)

(pcase-dolist (`(,key ,command ,label)
               '(("l" ygg-device-log-set-level "level")
                 ("f" ygg-device-log-keep "keep matching")
                 ("h" ygg-device-log-hide "hide matching")
                 ("p" ygg-device-log-pause "pause")
                 ("t" ygg-device-log-follow "follow")
                 ("c" ygg-device-log-clear "clear")
                 ("r" ygg-device-log-restart "restart")
                 ("s" ygg-device-log-save "save")
                 ("o" ygg-device-log-app-output "app output")
                 ("e" ygg-device-log-stderr "source stderr")
                 ("n" ygg-device-log-next-frame "next frame")
                 ("N" ygg-device-log-previous-frame "previous frame")))
  (yggdrasil-localleader-def 'ygg-device-log-mode key command label))

;;;###autoload
(defun ygg-device-log (&optional whole-device)
  "Stream the selected device's log, only this project's app when it names one.
With WHOLE-DEVICE, stream everything the device logs.  When WHOLE-DEVICE is
the symbol unless-streaming, a stream already running is shown as it is."
  (interactive "P")
  (let* ((device (ygg-device-require))
         (root (and (not whole-device) (ygg-device-log--root)))
         (app (and (not whole-device) (ygg-device-log--project-app root device)))
         (name (format "*log: %s*" (plist-get device :name)))
         (running (and-let* ((old (get-buffer name)))
                    (process-live-p (buffer-local-value 'ygg-device-log--process old))))
         (_ (ygg-device-log-command device app nil))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (or (and running (eq whole-device 'unless-streaming))
                  (and running
                       (equal ygg-device-log--device device)
                       (equal ygg-device-log--app app)))
        (unless (derived-mode-p 'ygg-device-log-mode)
          (ygg-device-log-mode))
        (setq default-directory (or root default-directory)
              ygg-device-log--device device
              ygg-device-log--root root
              ygg-device-log--app app
              ygg-device-log--resolved nil
              ygg-device-log--format (ygg-device-log--format-of device)
              ygg-device-log--pending "")
        (ygg-device-log--update-spec)
        (ygg-device-log--start)))
    (pop-to-buffer buffer)))

(provide 'ygg-device-log)
;;; ygg-device-log.el ends here
