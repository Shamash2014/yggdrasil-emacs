;;; aob-browser-tests.el --- a session's preview in a browser pane -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (defvar ygg-space-state-functions nil)
  (defvar ygg-space-detail-functions nil)
  (defvar ygg-leader-open-map (make-sparse-keymap))
  (defvar aob-acp-persist-file)
  (setq aob-acp-persist-file (make-temp-file "aob-browser-sessions-" nil ".eld")))

(require 'layer-aob)
(require 'layer-browser)
(require 'eww)

(defvar xwidget-webkit-last-session-buffer)
(defvar aob-browser-tests--visits nil)
(defvar aob-browser-tests--reloads nil)

(defmacro aob-browser-tests--with (&rest body)
  "Run BODY in one window with no sessions, eww as the engine, nothing fetched."
  (declare (indent 0))
  `(let ((aob--sessions (make-hash-table :test #'equal))
         (aob--order nil)
         (aob-session-created-hook nil)
         (aob-browser-tests--visits nil)
         (aob-browser-tests--reloads nil)
         (ygg-aob-browser-reload t)
         (ygg-aob-browser-auto-open nil))
     (cl-letf (((symbol-function 'ygg-browser--webkit-p) #'ignore)
               ((symbol-function 'eww)
                (lambda (url &rest _) (push (cons (current-buffer) url) aob-browser-tests--visits)))
               ((symbol-function 'eww-reload)
                (lambda (&rest _) (push (current-buffer) aob-browser-tests--reloads))))
       (save-window-excursion
         (delete-other-windows)
         (unwind-protect (progn ,@body)
           (dolist (b (buffer-list))
             (when (string-prefix-p "*preview: " (buffer-name b)) (kill-buffer b))))))))

(defun aob-browser-tests--session (name &rest said)
  "A session NAME whose agent said each of SAID, oldest first."
  (let ((s (aob-create-session :id (concat "acp:" name) :backend 'acp :name name
                               :project "/tmp/proj/" :dir "/tmp/proj/" :state 'idle)))
    (dolist (text said) (aob-event s 'message :text text))
    s))

(defun aob-browser-tests--printed (s output)
  "Note on S a command that printed OUTPUT."
  (aob-event s 'tool :kind "execute" :title "`npm run dev`" :status "completed"
             :rawOutput output))

(defun aob-browser-tests--pane ()
  (seq-find (lambda (w) (eq (window-parameter w 'window-side) 'right)) (window-list)))

(ert-deftest ygg-aob-browser-url-at-point-wins ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a" "see http://localhost:3000/")))
      (aob-session-put s :preview-url "http://localhost:9999/")
      (with-temp-buffer
        (insert "open http://localhost:4000/docs now")
        (goto-char 10)
        (should (equal (ygg-aob--preview-url s) "http://localhost:4000/docs"))))))

(ert-deftest ygg-aob-browser-url-remembered-before-events ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a" "see http://localhost:3000/")))
      (aob-session-put s :preview-url "http://localhost:9999/")
      (with-temp-buffer
        (should (equal (ygg-aob--preview-url s) "http://localhost:9999/"))))))

(ert-deftest ygg-aob-browser-url-newest-local-in-events ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session
              "a" "the old one is http://localhost:3000"
              "docs at https://example.com/x, and http://127.0.0.1:5173/app. Also http://0.0.0.0:8080/.")))
      (with-temp-buffer
        (should (equal (ygg-aob--preview-url s) "http://0.0.0.0:8080/")))
      (aob-browser-tests--printed s "  ➜  Local:   http://[::1]:4321/\n")
      (with-temp-buffer
        (should (equal (ygg-aob--preview-url s) "http://[::1]:4321/"))))))

(ert-deftest ygg-aob-browser-url-drops-markdown-around-it ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session
              "a" "up at **http://localhost:5173/** and _http://localhost:4000_")))
      (should (equal (ygg-aob--local-url s) "http://localhost:4000"))
      (should (equal (car (last (ygg-aob--event-urls (car (aob-session-events s))
                                                     ygg-aob--url-re)))
                     "http://localhost:5173/")))))

(ert-deftest ygg-aob-browser-pane-goes-with-its-session ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a")))
      (ygg-aob-browser s "http://localhost:3000/")
      (let ((buf (aob-session-ref s :preview-buffer)))
        (aob-remove-session s)
        (should-not (buffer-live-p buf))
        (should-not (aob-browser-tests--pane))))))

(ert-deftest ygg-aob-browser-url-without-port-is-not-local ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a" "http://localhost/ and http://localhostx:3000")))
      (should-not (ygg-aob--local-url s)))))

(ert-deftest ygg-aob-browser-url-read-with-session-urls ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a" "read https://example.com/a and (https://example.com/b)."))
          offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (setq offered coll) "https://example.com/b")))
        (with-temp-buffer
          (should (equal (ygg-aob--preview-url s) "https://example.com/b"))))
      (should (equal (sort offered #'string<) '("https://example.com/a" "https://example.com/b"))))))

(ert-deftest ygg-aob-browser-pane-beside-trace ()
  (aob-browser-tests--with
    (let* ((s (aob-browser-tests--session "a"))
           (trace (get-buffer-create "*aob-browser-trace*"))
           (tw (selected-window)))
      (set-window-buffer tw trace)
      (ygg-aob-browser s "http://localhost:3000/")
      (let ((pane (aob-browser-tests--pane))
            (buf (aob-session-ref s :preview-buffer)))
        (should (window-live-p pane))
        (should (eq (window-buffer pane) buf))
        (should (equal (buffer-name buf) "*preview: a*"))
        (should (eq (selected-window) tw))
        (should (eq (window-buffer tw) trace))
        (should (equal (aob-session-ref s :preview-url) "http://localhost:3000/"))
        (should (equal aob-browser-tests--visits (list (cons buf "http://localhost:3000/")))))
      (kill-buffer trace))))

(ert-deftest ygg-aob-browser-one-buffer-per-session ()
  (aob-browser-tests--with
    (let ((a (aob-browser-tests--session "a"))
          (b (aob-browser-tests--session "b")))
      (ygg-aob-browser a "http://localhost:3000/")
      (let ((buf (aob-session-ref a :preview-buffer)))
        (ygg-aob-browser a "http://localhost:3001/")
        (should (eq (aob-session-ref a :preview-buffer) buf))
        (should (equal (cdar aob-browser-tests--visits) "http://localhost:3001/"))
        (ygg-aob-browser b "http://localhost:4000/")
        (should-not (eq (aob-session-ref b :preview-buffer) buf))
        (should (= 2 (seq-count (lambda (x) (string-prefix-p "*preview: " (buffer-name x)))
                                (buffer-list))))))))

(ert-deftest ygg-aob-browser-q-closes-pane-only ()
  (aob-browser-tests--with
    (let* ((s (aob-browser-tests--session "a"))
           (tw (selected-window))
           (trace (window-buffer tw)))
      (ygg-aob-browser s "http://localhost:3000/")
      (let ((buf (aob-session-ref s :preview-buffer)))
        (with-selected-window (aob-browser-tests--pane)
          (call-interactively (key-binding "q")))
        (should-not (aob-browser-tests--pane))
        (should (buffer-live-p buf))
        (should (window-live-p tw))
        (should (eq (window-buffer tw) trace))))))

(ert-deftest ygg-aob-browser-reloads-at-turn-end-when-visible ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a")))
      (ygg-aob-browser s "http://localhost:3000/")
      (let ((buf (aob-session-ref s :preview-buffer)))
        (aob-set-state s 'working)
        (should-not aob-browser-tests--reloads)
        (aob-set-state s 'idle)
        (should (equal aob-browser-tests--reloads (list buf)))
        (aob-set-state s 'blocked)
        (aob-set-state s 'idle)
        (should (= 1 (length aob-browser-tests--reloads)))
        (delete-window (aob-browser-tests--pane))
        (aob-set-state s 'working)
        (aob-set-state s 'idle)
        (should (= 1 (length aob-browser-tests--reloads)))))))

(ert-deftest ygg-aob-browser-reload-can-be-turned-off ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a"))
          (ygg-aob-browser-reload nil))
      (ygg-aob-browser s "http://localhost:3000/")
      (aob-set-state s 'working)
      (aob-set-state s 'idle)
      (should-not aob-browser-tests--reloads))))

(ert-deftest ygg-aob-browser-auto-open-first-local-url ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "a")))
      (aob-set-state s 'working)
      (aob-browser-tests--printed s "ready on http://localhost:5173/")
      (aob-set-state s 'idle)
      (should-not (aob-browser-tests--pane))
      (let ((ygg-aob-browser-auto-open t))
        (aob-set-state s 'working)
        (aob-set-state s 'idle)
        (should (aob-browser-tests--pane))
        (should (equal (aob-session-ref s :preview-url) "http://localhost:5173/"))
        (delete-window (aob-browser-tests--pane))
        (aob-browser-tests--printed s "moved to http://localhost:5174/")
        (aob-set-state s 'working)
        (aob-set-state s 'idle)
        (should-not (aob-browser-tests--pane))))))

(ert-deftest ygg-aob-browser-webkit-pane-leaves-webkit-session-alone ()
  (aob-browser-tests--with
    (let ((s (aob-browser-tests--session "50%"))
          (mine (get-buffer-create "*aob-browser-own-webkit*"))
          gone reloaded)
      (require 'xwidget)
      (cl-letf (((symbol-function 'ygg-browser--webkit-p) (lambda () t))
                ((symbol-function 'xwidget-webkit-new-session)
                 (lambda (_url)
                   (switch-to-buffer (generate-new-buffer "*xwidget-webkit: *"))
                   (setq xwidget-webkit-last-session-buffer (current-buffer))))
                ((symbol-function 'xwidget-at) (lambda (_) 'xw))
                ((symbol-function 'xwidget-webkit-goto-uri)
                 (lambda (_ url) (push url gone)))
                ((symbol-function 'xwidget-webkit-reload)
                 (lambda () (push (current-buffer) reloaded))))
        (let ((xwidget-webkit-last-session-buffer mine)
              (tw (selected-window)))
          (ygg-aob-browser s "http://localhost:3000/")
          (let ((buf (aob-session-ref s :preview-buffer)))
            (should (eq xwidget-webkit-last-session-buffer mine))
            (should (eq (window-buffer (aob-browser-tests--pane)) buf))
            (should (eq (selected-window) tw))
            (should (equal (buffer-name buf) "*preview: 50%*"))
            (should (equal (buffer-local-value 'xwidget-webkit-buffer-name-format buf)
                           "*preview: 50%%*"))
            (should-not gone)
            (ygg-aob-browser s "http://localhost:3001/")
            (should (equal gone '("http://localhost:3001/")))
            (aob-set-state s 'working)
            (aob-set-state s 'idle)
            (should (equal reloaded (list buf)))
            (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
      (kill-buffer mine))))

(ert-deftest ygg-aob-browser-keys ()
  (should (eq (lookup-key ygg-browser-map "a") #'ygg-aob-browser))
  (should (eq (lookup-key (ygg-localleader--get-map 'aob-trace-mode) "b")
              #'ygg-aob-browser)))

;;; aob-browser-tests.el ends here
