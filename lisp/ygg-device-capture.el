;;; ygg-device-capture.el --- Screenshots and screen recordings of the selected device -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'ygg-device)

(defcustom ygg-device-capture-directory "captures"
  "Where screenshots and recordings go, relative to the project root.
Nothing edits .gitignore; a capture only says when git does not ignore it."
  :type 'string
  :group 'ygg-device)

(defcustom ygg-device-capture-timeout 30
  "Seconds a screenshot or a pull may run before it is killed."
  :type 'number
  :group 'ygg-device)

(defcustom ygg-device-capture-pull-timeout 600
  "Seconds pulling an Android recording off the device may take."
  :type 'number
  :group 'ygg-device)

(defcustom ygg-device-capture-stop-wait 5
  "Seconds Emacs waits on quit for a recording to finish its file."
  :type 'number
  :group 'ygg-device)

(defvar ygg-device-capture--reserved nil
  "Capture files whose program has not written them yet.")

(defvar ygg-device-capture--recording nil
  "The recording in progress as a plist.
Keys: :device, :file, :process, :remote, :pid and :stopping.")

;;; Files

(defun ygg-device-capture--directory ()
  (expand-file-name ygg-device-capture-directory
                    (if-let* ((project (project-current))) (project-root project) default-directory)))

(defun ygg-device-capture--slug (name)
  (string-trim (replace-regexp-in-string "[^[:alnum:]]+" "-" (or name "device")) "-" "-"))

(defun ygg-device-capture-file-name (device extension &optional time)
  "A file name for a capture of DEVICE with EXTENSION taken at TIME."
  (format "%s-%s.%s" (ygg-device-capture--slug (plist-get device :name))
          (format-time-string "%Y%m%d-%H%M%S" time) extension))

(defun ygg-device-capture--git-note (directory)
  (let ((default-directory directory))
    (when (and (executable-find "git")
               (eq 0 (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree"))
               (eq 1 (call-process "git" nil nil nil "check-ignore" "-q" directory)))
      (format "; git does not ignore %s" (file-name-nondirectory (directory-file-name directory))))))

(defun ygg-device-capture--new-file (device extension)
  "A fresh path for a capture of DEVICE and a note on how git sees its folder."
  (let* ((directory (file-name-as-directory (ygg-device-capture--directory)))
         (note (unless (file-directory-p directory)
                 (make-directory directory t)
                 (ygg-device-capture--git-note directory)))
         (file (expand-file-name (ygg-device-capture-file-name device extension) directory))
         (base (file-name-sans-extension file))
         (n 1))
    (while (or (file-exists-p file) (member file ygg-device-capture--reserved))
      (setq file (format "%s-%d.%s" base (cl-incf n) extension)))
    (push file ygg-device-capture--reserved)
    (cons file note)))

(defun ygg-device-capture--release (file)
  (setq ygg-device-capture--reserved (delete file ygg-device-capture--reserved)))

(defun ygg-device-capture--size (file)
  (and (file-exists-p file) (file-attribute-size (file-attributes file))))

(defun ygg-device-capture--saved-p (file)
  (let ((size (ygg-device-capture--size file)))
    (and size (> size 0))))

;;; Screenshot

(defun ygg-device-capture--check (device platforms what)
  (let ((platform (plist-get device :platform)))
    (unless (memq platform platforms)
      (user-error "No %s of %s targets" what platform))))

(defun ygg-device-capture--sh (&rest words)
  (list "/bin/sh" "-c" (string-join words " ")))

(defun ygg-device-screenshot-command (device file)
  "The program and arguments writing a PNG of DEVICE to FILE."
  (let ((id (plist-get device :id))
        (q #'shell-quote-argument))
    (pcase (plist-get device :platform)
      ('ios (list (ygg-device--xcrun) "simctl" "io" id "screenshot" file))
      ('ios-device (list (ygg-device--xcrun) "devicectl" "device" "capture" "screenshot"
                         "--device" id "--destination" file "-q"))
      ('macos (list "/usr/sbin/screencapture" "-x" file))
      ('android
       (let ((adb (funcall q (ygg-device--adb)))
             (serial (funcall q id))
             (screencap (format "%s -s %s exec-out screencap -p > %s"
                                (funcall q (ygg-device--adb)) (funcall q id) (funcall q file))))
         (if (string-prefix-p "emulator-" id)
             (let ((scratch (funcall q (make-temp-file "ygg-capture-" t))))
               ;; The emulator console grabs the frame in ~0.2 s against ~0.4 s for screencap.
               (ygg-device-capture--sh
                adb "-s" serial "emu" "screenrecord" "screenshot" scratch
                "&& mv" (concat scratch "/*.png") (funcall q file)
                "; rm -rf" scratch "; test -s" (funcall q file) "||" screencap))
           (ygg-device-capture--sh screencap))))
      (platform (user-error "No screenshots of %s targets" platform)))))

(defun ygg-device-capture--show-image (file)
  (let ((buffer (find-file-noselect file)))
    (display-buffer buffer)
    buffer))

(defun ygg-device-screenshot ()
  "Save a PNG of the selected device into the project, show it and copy its path."
  (interactive)
  (let* ((device (ygg-device-require))
         (_ (ygg-device-capture--check device '(ios ios-device android macos) "screenshots"))
         (target (ygg-device-capture--new-file device "png"))
         (file (car target))
         (command (ygg-device-screenshot-command device file)))
    (message "Screenshot of %s..." (plist-get device :name))
    (funcall ygg-device-run-function (car command) (cdr command) ygg-device-capture-timeout
             (lambda (output)
               (ygg-device-capture--release file)
               (if (ygg-device-capture--saved-p file)
                   (progn
                     (kill-new file)
                     (ygg-device-capture--show-image file)
                     (message "Screenshot %s (%s)%s" (abbreviate-file-name file)
                              (file-size-human-readable (ygg-device-capture--size file))
                              (or (cdr target) "")))
                 (when (file-exists-p file) (delete-file file))
                 (message "Screenshot of %s failed%s" (plist-get device :name)
                          (if (string-empty-p (string-trim (or output ""))) ""
                            (concat ": " (string-trim output)))))))))

;;; Recording

(defun ygg-device-capture-parse-pid (output)
  "The process id the Android record shell echoes first in OUTPUT."
  (and output (string-match "\\`[ \t\r\n]*\\([0-9]+\\)" output)
       (string-to-number (match-string 1 output))))

(defun ygg-device-capture-parse-unlimited (help)
  "Whether screenrecord HELP says a zero time limit removes the limit."
  (and help (string-match-p "Set to 0" help)))

(defun ygg-device-record-command (device file &optional remote unlimited)
  "The program and arguments recording DEVICE until interrupted.
iOS writes FILE; Android writes REMOTE on the device, with no time
limit when UNLIMITED."
  (let ((id (plist-get device :id)))
    (pcase (plist-get device :platform)
      ('ios (list (ygg-device--xcrun) "simctl" "io" id "recordVideo" "--codec=h264" "--force" file))
      ('ios-device (list (ygg-device--xcrun) "devicectl" "device" "capture" "screen-record"
                         "--device" id "--destination" file "--codec" "h264" "-q"))
      ('android (list (ygg-device--adb) "-s" id "shell"
                      (concat "echo $$; exec screenrecord "
                              (if unlimited "--time-limit 0 " "")
                              (shell-quote-argument remote))))
      (platform (user-error "No recording of %s targets" platform)))))

(defun ygg-device-capture--extension (device)
  (if (eq (plist-get device :platform) 'ios) "mov" "mp4"))

(defun ygg-device-capture-recording-p ()
  "Whether a recording is running."
  (and ygg-device-capture--recording t))

(defun ygg-device-capture-modeline ()
  "REC while a recording runs, as plain text."
  (and ygg-device-capture--recording "  REC"))

(defun ygg-device-capture--announce (recording limited)
  (let ((file (plist-get recording :file)))
    (ygg-device-capture--release file)
    (if (or (plist-get recording :pull-failed) (not (ygg-device-capture--saved-p file)))
        (message "Recording of %s failed%s" (plist-get (plist-get recording :device) :name)
                 (if-let* ((output (string-trim (plist-get recording :output))) ((not (string-empty-p output))))
                     (concat ": " output) ""))
      (kill-new file)
      (message "Recording %s (%s)%s%s" (abbreviate-file-name file)
               (file-size-human-readable (ygg-device-capture--size file))
               (if limited "; Android stops screenrecord at 3 minutes on this device" "")
               (or (plist-get recording :note) "")))))

(defun ygg-device-capture-pulled-p (output)
  "Whether adb pull OUTPUT reports the file arrived."
  (and output (string-match-p "\\b1 file pulled" output)))

(defun ygg-device-capture-pull-command (serial remote file)
  "The command pulling REMOTE off SERIAL into FILE; adb reports on stderr."
  (ygg-device-capture--sh
   (mapconcat #'shell-quote-argument (list (ygg-device--adb) "-s" serial "pull" remote file) " ")
   "2>&1"))

(defun ygg-device-capture--android-pull (recording limited)
  (let* ((serial (plist-get (plist-get recording :device) :id))
         (remote (plist-get recording :remote))
         (command (ygg-device-capture-pull-command serial remote (plist-get recording :file))))
    (funcall ygg-device-run-function (car command) (cdr command) ygg-device-capture-pull-timeout
             (lambda (output)
               (if (ygg-device-capture-pulled-p output)
                   (funcall ygg-device-run-function (ygg-device--adb)
                            (list "-s" serial "shell" "rm" "-f" remote)
                            ygg-device-capture-timeout #'ignore)
                 (plist-put recording :pull-failed t)
                 (plist-put recording :output
                            (format "%s; the device keeps %s"
                                    (if output (string-trim output) "the pull timed out")
                                    remote)))
               (ygg-device-capture--announce recording limited)))))

(defun ygg-device-capture--finished (recording)
  (let ((limited (and (eq (plist-get (plist-get recording :device) :platform) 'android)
                      (not (plist-get recording :stopping))
                      (not (plist-get recording :unlimited)))))
    (when (eq ygg-device-capture--recording recording)
      (setq ygg-device-capture--recording nil)
      (force-mode-line-update t))
    (cond ((plist-get recording :exiting))
          ((plist-get recording :remote) (ygg-device-capture--android-pull recording limited))
          (t (ygg-device-capture--announce recording nil)))))

(defun ygg-device-capture--launch (device target &optional remote unlimited)
  (let* ((file (car target))
         (command (ygg-device-record-command device file remote unlimited))
         (recording (list :device device :file file :remote remote :unlimited unlimited
                          :note (cdr target) :output "" :pid nil :stopping nil :process nil
                          :pull-failed nil :exiting nil))
         (default-directory (if (file-remote-p default-directory) "~/" default-directory))
         (process
          (make-process
           :name "ygg-device-record" :command command :connection-type 'pipe
           :noquery t :file-handler nil
           :filter (lambda (_process text)
                     (plist-put recording :output (concat (plist-get recording :output) text))
                     (when (and remote (not (plist-get recording :pid)))
                       (plist-put recording :pid (ygg-device-capture-parse-pid
                                                  (plist-get recording :output)))))
           :sentinel (lambda (process _event)
                       (unless (process-live-p process)
                         (ygg-device-capture--finished recording))))))
    (plist-put recording :process process)
    (setq ygg-device-capture--recording recording)
    (force-mode-line-update t)
    (message "Recording %s; press again to stop" (plist-get device :name))
    recording))

(defun ygg-device-capture--start (device)
  (ygg-device-capture--check device '(ios ios-device android) "recording")
  (let ((target (ygg-device-capture--new-file device (ygg-device-capture--extension device))))
    (if (not (eq (plist-get device :platform) 'android))
        (ygg-device-capture--launch device target)
      (setq ygg-device-capture--recording (list :device device :starting t))
      (funcall ygg-device-run-function (ygg-device--adb)
               (list "-s" (plist-get device :id) "shell" "screenrecord --help 2>&1")
               ygg-device-list-timeout
               (lambda (help)
                 (setq ygg-device-capture--recording nil)
                 (ygg-device-capture--launch
                  device target
                  (format "/sdcard/ygg-%s" (file-name-nondirectory (car target)))
                  (ygg-device-capture-parse-unlimited help)))))))

(defun ygg-device-capture--stop (recording &optional sync)
  "Ask RECORDING to finish its file; with SYNC, wait until it has."
  (let ((process (plist-get recording :process)))
    (plist-put recording :stopping t)
    (when (process-live-p process)
      (if-let* ((pid (plist-get recording :pid)))
          (let ((args (list "-s" (plist-get (plist-get recording :device) :id)
                            "shell" "kill" "-INT" (number-to-string pid))))
            (if sync
                (apply #'call-process (ygg-device--adb) nil nil nil args)
              (funcall ygg-device-run-function (ygg-device--adb) args ygg-device-list-timeout #'ignore)))
        (signal-process process 'SIGINT)))
    (when sync
      (let ((deadline (+ (float-time) ygg-device-capture-stop-wait)))
        (while (and (process-live-p process) (< (float-time) deadline))
          (accept-process-output process 0.1)))
      (when-let* (((not (process-live-p process)))
                  (remote (plist-get recording :remote))
                  (serial (plist-get (plist-get recording :device) :id))
                  ((eq 0 (call-process (ygg-device--adb) nil nil nil "-s" serial "pull"
                                       remote (plist-get recording :file)))))
        (call-process (ygg-device--adb) nil nil nil "-s" serial "shell" "rm" "-f" remote)))))

(defun ygg-device-record-toggle ()
  "Start recording the selected device, or stop the recording that runs."
  (interactive)
  (if-let* ((recording ygg-device-capture--recording))
      (let ((name (plist-get (plist-get recording :device) :name)))
        (if (or (plist-get recording :starting)
                (and (plist-get recording :remote) (not (plist-get recording :pid))))
            (message "The recording of %s is still starting" name)
          (ygg-device-capture--stop recording)
          (message "Stopping the recording of %s..." name)))
    (ygg-device-capture--start (ygg-device-require))))

(defun ygg-device-capture--stop-on-exit ()
  (when-let* ((recording ygg-device-capture--recording))
    (setq ygg-device-capture--recording nil)
    (when (plist-get recording :process)
      (plist-put recording :exiting t)
      (ygg-device-capture--stop recording t))))

(add-hook 'kill-emacs-hook #'ygg-device-capture--stop-on-exit)

(defvar ygg-device-capture--modeline-entry '(:eval (ygg-device-capture-modeline)))
(put 'ygg-device-capture--modeline-entry 'risky-local-variable t)
(add-to-list 'mode-line-misc-info 'ygg-device-capture--modeline-entry t)

(provide 'ygg-device-capture)
;;; ygg-device-capture.el ends here
