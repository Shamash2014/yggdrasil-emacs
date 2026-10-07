;;; aob-litellm.el --- overall LiteLLM key spend beside a pi session's usage -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'aob)

(declare-function ygg-agent--known-config-dir "ygg-agent-conf")
(defvar ygg-agent--config-homes)

(defcustom aob-litellm-cache-seconds 300
  "How long one key's spend is reused before it is asked for again."
  :type 'integer :group 'aob)

(defvar aob-litellm-fetch-function #'aob-litellm--fetch-curl
  "Called with BASE, KEY and a callback taking (DATA . ERROR); must not block.")

(defvar aob-litellm--cache (make-hash-table :test #'equal)
  "(BASE . KEY-HASH) to a plist of :time, :data, :error and :waiters.")

(defvar aob-litellm--configs (make-hash-table :test #'equal)
  "Pi home to (MTIMES BASE . RAW-KEY); in memory only.")

(defun aob-litellm--home (s)
  (when-let* ((dir (or (aob-session-project s) (aob-session-dir s))))
    (ignore-errors
      (and (fboundp 'ygg-agent--known-config-dir)
           (ygg-agent--known-config-dir
            "pi" (cdr (assoc "pi" ygg-agent--config-homes)) dir)))))

(defun aob-litellm--read-json (file)
  (when (file-readable-p file)
    (let ((json (ignore-errors
                  (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'alist)))))
      (and (listp json) json))))

(defun aob-litellm--resolve-key (raw)
  (unless (or (null raw) (string-empty-p raw) (string-prefix-p "!" raw))
    (let ((key (or (getenv raw) raw)))
      (unless (or (string-empty-p key)
                  (string-match-p "[\"\\[:cntrl:]]" key))
        key))))

(defun aob-litellm--mtimes (home)
  (mapcar (lambda (f)
            (file-attribute-modification-time
             (file-attributes (expand-file-name f home))))
          '("models.json" "auth.json")))

(defun aob-litellm--read-config (home)
  (let ((providers (alist-get 'providers
                              (aob-litellm--read-json
                               (expand-file-name "models.json" home))))
        (auth (aob-litellm--read-json (expand-file-name "auth.json" home))))
    (when (listp providers)
      (cl-loop for (name . p) in providers
               for base = (and (listp p) (alist-get 'baseUrl p))
               for raw = (and (listp p) (alist-get 'apiKey p))
               when (and (stringp base)
                         (string-match-p "litellm" (downcase (format "%s %s" name base))))
               return (cons (replace-regexp-in-string "\\(/v1\\)?/*\\'" "" base)
                            (if (and (stringp raw) (not (string-empty-p raw)))
                                raw
                              (let ((k (alist-get 'key (alist-get name auth))))
                                (and (stringp k) k))))))))

(defun aob-litellm--config (s)
  "The (BASE . KEY) of the LiteLLM provider S's pi reads, or nil."
  (when (equal (aob-session-ref s :agent) "pi")
    (when-let* ((home (aob-litellm--home s)))
      (let ((mtimes (aob-litellm--mtimes home))
            (hit (gethash home aob-litellm--configs)))
        (unless (and hit (equal (car hit) mtimes))
          (setq hit (cons mtimes (aob-litellm--read-config home)))
          (puthash home hit aob-litellm--configs))
        (when-let* ((config (cdr hit))
                    (key (aob-litellm--resolve-key (cdr config))))
          (cons (car config) key))))))

(defun aob-litellm--parse (body)
  (let* ((json (ignore-errors (json-parse-string body :object-type 'alist :null-object nil)))
         (info (and (listp json) (or (alist-get 'info json) json)))
         (spend (and (listp info) (alist-get 'spend info))))
    (when (numberp spend)
      (list :used spend
            :limit (alist-get 'max_budget info)
            :window (alist-get 'budget_duration info)))))

(defun aob-litellm--fetch-curl (base key callback)
  (let* ((out (generate-new-buffer " *aob-litellm*"))
         (proc (condition-case err
                   (make-process
                    :name "aob-litellm" :buffer out :noquery t :connection-type 'pipe
                    :command (list "curl" "-sS" "--max-time" "15" "-K" "-"
                                   (concat base "/key/info"))
                    :sentinel
                    (lambda (p _)
                      (unless (process-live-p p)
                        (let ((body (with-current-buffer out (buffer-string))))
                          (kill-buffer out)
                          (funcall callback
                                   (and (eq (process-exit-status p) 0)
                                        (aob-litellm--parse body)))))))
                 (error (kill-buffer out) (signal (car err) (cdr err))))))
    (process-send-string proc (format "header = \"Authorization: Bearer %s\"\n" key))
    (process-send-eof proc)))

(defun aob-litellm--entry (base key)
  (let* ((id (cons base (secure-hash 'sha256 key)))
         (entry (gethash id aob-litellm--cache))
         (old (plist-get entry :data)))
    (when (or (null entry)
              (and (null (plist-get entry :waiters))
                   (> (- (float-time) (plist-get entry :time)) aob-litellm-cache-seconds)))
      (puthash id (list :time (float-time) :data old :waiters (list t)) aob-litellm--cache)
      (condition-case nil
          (funcall aob-litellm-fetch-function base key
                   (lambda (data)
                     (let* ((cur (gethash id aob-litellm--cache))
                            (waiters (plist-get cur :waiters)))
                       (puthash id (list :time (float-time)
                                         :data (or data (plist-get cur :data))
                                         :error (null data))
                                aob-litellm--cache)
                       (dolist (w waiters)
                         (when (aob-session-p w) (aob-litellm--refresh w))))))
        (error (puthash id (list :time (float-time) :data old :error t) aob-litellm--cache))))
    (gethash id aob-litellm--cache)))

(defun aob-litellm--refresh (s)
  (run-hook-with-args 'aob-meter-change-hook s))

(defun aob-litellm--note-waiter (base key s)
  (let* ((id (cons base (secure-hash 'sha256 key)))
         (entry (gethash id aob-litellm--cache)))
    (when (and entry (plist-get entry :waiters))
      (cl-pushnew s (plist-get entry :waiters))
      (puthash id entry aob-litellm--cache))))

(defun aob-litellm-usage (s)
  "S's overall usage as (:label :used :limit :window), (:label :error t) or nil."
  (when-let* ((config (aob-litellm--config s)))
    (let ((entry (aob-litellm--entry (car config) (cdr config))))
      (aob-litellm--note-waiter (car config) (cdr config) s)
      (cond ((plist-get entry :data) (append (list :label "LiteLLM") (plist-get entry :data)))
            ((plist-get entry :error) (list :label "LiteLLM" :error t))))))

(defun aob-litellm--money (n &optional trim)
  (let ((s (format "%.2f" n)))
    (while (string-match "\\`\\(-?[0-9]+\\)\\([0-9]\\{3\\}\\(?:,[0-9]\\{3\\}\\)*\\(?:\\.[0-9]+\\)?\\)\\'" s)
      (setq s (concat (match-string 1 s) "," (match-string 2 s))))
    (concat "$" (if trim (string-remove-suffix ".00" s) s))))

(defun aob-overall-usage-string (usage)
  "USAGE, a plist of :label and :used, :limit, :window or :error, in a few words."
  (when usage
    (let ((label (plist-get usage :label)))
      (if (plist-get usage :error)
          (concat label " ?")
        (let ((limit (plist-get usage :limit))
              (window (plist-get usage :window)))
          (concat label " " (aob-litellm--money (plist-get usage :used))
                  (and (numberp limit) (concat " / " (aob-litellm--money limit t)))
                  (and window (concat " · " window))))))))

(defvar aob-overall-usage-functions (list #'aob-litellm-usage)
  "Functions taking a session and returning an overall-usage plist or nil.")

(defun aob-overall-usage-parts (s)
  (delq nil (mapcar (lambda (f)
                      (ignore-errors (aob-overall-usage-string (funcall f s))))
                    aob-overall-usage-functions)))

(provide 'aob-litellm)
