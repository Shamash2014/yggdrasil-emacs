;;; aob-litellm-tests.el --- tests for the overall LiteLLM usage line -*- lexical-binding: t; -*-

(require 'ert)
(require 'aob)
(require 'aob-litellm)
(require 'aob-trace)
(require 'ygg-agent-conf)

(defconst aob-litellm-tests--key "sk-test-SECRET-0123456789")

(defun aob-litellm-tests--home (key)
  (let ((dir (file-name-as-directory (make-temp-file "aob-litellm-" t))))
    (with-temp-file (expand-file-name "models.json" dir)
      (insert (json-serialize
               `(:providers (:litellm (:baseUrl "http://llm.test/v1"
                                       :apiKey ,(or key "")))))))
    dir))

(defmacro aob-litellm-tests--with (key &rest body)
  (declare (indent 2))
  `(let* ((home (aob-litellm-tests--home ,key))
          (aob-litellm--cache (make-hash-table :test #'equal))
          (aob-litellm--configs (make-hash-table :test #'equal))
          (calls nil)
          (pending nil)
          (aob-litellm-fetch-function
           (lambda (base k cb) (push (list base k) calls) (push cb pending))))
     (cl-letf (((symbol-function 'aob-litellm--home) (lambda (_) home)))
       ,@body)))

(defun aob-litellm-tests--project (root name auth-key &optional api-key)
  (let ((home (file-name-as-directory (expand-file-name (concat name "/pi") root)))
        (project (file-name-as-directory (expand-file-name name (expand-file-name "work" root)))))
    (make-directory home t)
    (make-directory project t)
    (with-temp-file (expand-file-name "models.json" home)
      (insert (json-serialize
               `(:providers (:litellm (:api "openai-completions"
                                       :baseUrl "http://llm.test/v1"
                                       ,@(and api-key (list :apiKey api-key))))))))
    (when auth-key
      (with-temp-file (expand-file-name "auth.json" home)
        (insert (json-serialize `(:litellm (:type "api_key" :key ,auth-key))))))
    project))

(defmacro aob-litellm-tests--with-root (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "aob-litellm-root-" t)))
          (ygg-agent-conf-root (expand-file-name "conf" root))
          (aob-litellm--cache (make-hash-table :test #'equal))
          (aob-litellm--configs (make-hash-table :test #'equal))
          (calls nil)
          (pending nil)
          (aob-litellm-fetch-function
           (lambda (base k cb) (push (list base k) calls) (push cb pending))))
     (clrhash ygg-agent--config-dirs)
     (cl-letf (((symbol-function 'getenv) (lambda (&rest _) nil)))
       ,@body)))

(defun aob-litellm-tests--session-in (n project)
  (let ((s (aob-create-session :id (format "litellm:p%d" n) :backend 'acp
                               :name (format "p%d" n) :project project :state 'idle)))
    (aob-session-put s :agent "pi")
    s))

(defun aob-litellm-tests--session (n)
  (let ((s (aob-create-session :id (format "litellm:%d" n) :backend 'acp
                               :name (format "l%d" n) :project "/tmp/p/" :state 'idle)))
    (aob-session-put s :agent "pi")
    s))

(defconst aob-litellm-tests--reply
  (json-serialize '(:info (:spend 2.46 :max_budget 200 :budget_duration "30d"))))

(ert-deftest aob-litellm-renders-spend-budget-and-window ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((s (aob-litellm-tests--session 1)))
      (should (null (aob-overall-usage-parts s)))
      (funcall (car pending) (aob-litellm--parse aob-litellm-tests--reply))
      (should (equal (aob-overall-usage-parts s) '("LiteLLM $2.46 / $200 · 30d"))))))

(ert-deftest aob-litellm-one-request-for-sessions-sharing-a-config ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((a (aob-litellm-tests--session 1))
          (b (aob-litellm-tests--session 2)))
      (aob-overall-usage-parts a)
      (aob-overall-usage-parts b)
      (funcall (car pending) (aob-litellm--parse aob-litellm-tests--reply))
      (aob-overall-usage-parts a)
      (aob-overall-usage-parts b)
      (should (= (length calls) 1))
      (should (equal (caar calls) "http://llm.test")))))

(ert-deftest aob-litellm-missing-key-shows-nothing ()
  (aob-litellm-tests--with nil
    (let ((s (aob-litellm-tests--session 1)))
      (should (null (aob-overall-usage-parts s)))
      (should (null calls)))))

(ert-deftest aob-litellm-error-shows-question-mark ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((s (aob-litellm-tests--session 1)))
      (aob-overall-usage-parts s)
      (funcall (car pending) nil)
      (should (equal (aob-overall-usage-parts s) '("LiteLLM ?")))
      (should (= (length calls) 1)))))

(ert-deftest aob-litellm-plist-without-limit-renders-spend-only ()
  (should (equal (aob-overall-usage-string '(:label "X" :used 1.5))
                 "X $1.50")))

(ert-deftest aob-litellm-key-appears-nowhere-visible ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((s (aob-litellm-tests--session 1)))
      (aob-overall-usage-parts s)
      (funcall (car pending) (aob-litellm--parse aob-litellm-tests--reply))
      (let ((header (aob-trace--header s)))
        (should-not (string-search aob-litellm-tests--key header))
        (should (string-search "LiteLLM $2.46" header)))
      (should-not (string-search aob-litellm-tests--key
                                 (with-current-buffer "*Messages*" (buffer-string))))
      (should-not (string-search aob-litellm-tests--key
                                 (format "%S" aob-litellm--cache))))))

(ert-deftest aob-litellm-key-from-auth-json-of-the-project-home ()
  (aob-litellm-tests--with-root
    (let* ((project (aob-litellm-tests--project
                     (expand-file-name "conf" root) "alpha" aob-litellm-tests--key))
           (s (aob-litellm-tests--session-in 1 project)))
      (aob-overall-usage-parts s)
      (should (equal calls (list (list "http://llm.test" aob-litellm-tests--key))))
      (funcall (car pending) (aob-litellm--parse aob-litellm-tests--reply))
      (should (equal (aob-overall-usage-parts s) '("LiteLLM $2.46 / $200 · 30d"))))))

(ert-deftest aob-litellm-each-project-reads-its-own-home ()
  (aob-litellm-tests--with-root
    (let* ((conf (expand-file-name "conf" root))
           (a (aob-litellm-tests--session-in
               1 (aob-litellm-tests--project conf "alpha" "sk-alpha-key")))
           (b (aob-litellm-tests--session-in
               2 (aob-litellm-tests--project conf "beta" "sk-beta-key"))))
      (aob-overall-usage-parts a)
      (aob-overall-usage-parts b)
      (should (equal (sort (mapcar #'cadr calls) #'string<)
                     '("sk-alpha-key" "sk-beta-key"))))))

(ert-deftest aob-litellm-provider-without-auth-entry-shows-nothing ()
  (aob-litellm-tests--with-root
    (let ((s (aob-litellm-tests--session-in
              1 (aob-litellm-tests--project (expand-file-name "conf" root) "gamma" nil))))
      (should (null (aob-overall-usage-parts s)))
      (should (null calls)))))

(ert-deftest aob-litellm-projects-with-the-same-key-make-one-request ()
  (aob-litellm-tests--with-root
    (let* ((conf (expand-file-name "conf" root))
           (a (aob-litellm-tests--session-in
               1 (aob-litellm-tests--project conf "alpha" aob-litellm-tests--key)))
           (b (aob-litellm-tests--session-in
               2 (aob-litellm-tests--project conf "beta" aob-litellm-tests--key))))
      (aob-overall-usage-parts a)
      (aob-overall-usage-parts b)
      (funcall (car pending) (aob-litellm--parse aob-litellm-tests--reply))
      (should (= (length calls) 1))
      (should (equal (aob-overall-usage-parts b) '("LiteLLM $2.46 / $200 · 30d"))))))

(ert-deftest aob-litellm-expired-entry-keeps-showing-old-data ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((s (aob-litellm-tests--session 1))
          (aob-litellm-cache-seconds 0))
      (aob-overall-usage-parts s)
      (funcall (car pending) (aob-litellm--parse aob-litellm-tests--reply))
      (sleep-for 0.01)
      (should (equal (aob-overall-usage-parts s) '("LiteLLM $2.46 / $200 · 30d")))
      (should (= (length calls) 2))
      (should (equal (aob-overall-usage-parts s) '("LiteLLM $2.46 / $200 · 30d"))))))

(ert-deftest aob-litellm-error-keeps-the-last-good-data ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((s (aob-litellm-tests--session 1))
          (aob-litellm-cache-seconds 0))
      (aob-overall-usage-parts s)
      (funcall (car pending) (aob-litellm--parse aob-litellm-tests--reply))
      (sleep-for 0.01)
      (aob-overall-usage-parts s)
      (funcall (car pending) nil)
      (should (equal (aob-overall-usage-parts s) '("LiteLLM $2.46 / $200 · 30d"))))))

(ert-deftest aob-litellm-fetch-signal-keeps-old-data-or-shows-question-mark ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((s (aob-litellm-tests--session 1))
          (aob-litellm-fetch-function (lambda (&rest _) (error "no curl"))))
      (should (equal (aob-overall-usage-parts s) '("LiteLLM ?"))))))

(ert-deftest aob-litellm-config-is-read-once-until-files-change ()
  (aob-litellm-tests--with-root
    (let* ((project (aob-litellm-tests--project
                     (expand-file-name "conf" root) "alpha" aob-litellm-tests--key))
           (s (aob-litellm-tests--session-in 1 project))
           (reads 0)
           (orig (symbol-function 'aob-litellm--read-json))
           (aob-litellm--configs (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'aob-litellm--read-json)
                 (lambda (f) (cl-incf reads) (funcall orig f))))
        (aob-overall-usage-parts s)
        (let ((first reads))
          (should (> first 0))
          (aob-overall-usage-parts s)
          (aob-overall-usage-parts s)
          (should (= reads first))
          (let* ((home (aob-litellm--home s))
                 (file (expand-file-name "auth.json" home)))
            (set-file-times file (time-add (current-time) 100))
            (aob-overall-usage-parts s)
            (should (> reads first))))
        (should-not (string-search aob-litellm-tests--key
                                   (format "%S" (hash-table-keys aob-litellm--configs))))))))

(ert-deftest aob-litellm-header-survives-an-error-in-usage ()
  (aob-litellm-tests--with aob-litellm-tests--key
    (let ((s (aob-litellm-tests--session 1))
          (aob-overall-usage-functions (list (lambda (_) (error "boom")))))
      (should (null (aob-overall-usage-parts s)))
      (should (stringp (aob-trace--header s))))
    (let ((s (aob-litellm-tests--session 2)))
      (cl-letf (((symbol-function 'aob-overall-usage-parts)
                 (lambda (_) (error "boom"))))
        (should (stringp (aob-trace--header s)))))))

(ert-deftest aob-litellm-curl-failure-kills-its-buffer ()
  (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "no curl"))))
    (should-error (aob-litellm--fetch-curl "http://llm.test" "sk-x" #'ignore)))
  (should-not (seq-some (lambda (b) (string-prefix-p " *aob-litellm*" (buffer-name b)))
                        (buffer-list))))

(ert-deftest aob-litellm-curl-uses-a-pipe ()
  (let (args)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest a) (setq args a) 'proc))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'process-send-eof) #'ignore))
      (aob-litellm--fetch-curl "http://llm.test" "sk-x" #'ignore)
      (should (eq (plist-get args :connection-type) 'pipe))
      (kill-buffer (plist-get args :buffer)))))

(ert-deftest aob-litellm-non-object-json-parses-to-nil ()
  (dolist (body '("[1,2]" "[{\"spend\":1}]" "3" "\"x\"" "null" "{\"info\":[1]}" "{\"info\":\"x\"}" ""))
    (should (null (aob-litellm--parse body))))
  (aob-litellm-tests--with-root
    (let ((file (expand-file-name "a.json" root)))
      (dolist (body '("[1]" "\"s\"" "7"))
        (with-temp-file file (insert body))
        (should (null (aob-litellm--read-json file)))))))

(ert-deftest aob-litellm-money-never-uses-scientific-notation ()
  (should (equal (aob-overall-usage-string '(:label "X" :used 2.46 :limit 200))
                 "X $2.46 / $200"))
  (should (equal (aob-overall-usage-string '(:label "X" :used 1234.5 :limit 1234567))
                 "X $1,234.50 / $1,234,567"))
  (should (equal (aob-overall-usage-string '(:label "X" :used 1 :limit 2.5))
                 "X $1.00 / $2.50"))
  (should-not (string-match-p "e[+0-9]"
                              (aob-overall-usage-string '(:label "X" :used 1 :limit 1e12)))))

(ert-deftest aob-litellm-key-that-could-break-the-curl-line-is-no-key ()
  (dolist (bad (list "sk-a\"b" "sk-a\\b" "sk-a\nb" "sk-a\tb" (string ?s 1 ?k)))
    (aob-litellm-tests--with bad
      (let ((s (aob-litellm-tests--session 1)))
        (should (null (aob-overall-usage-parts s)))
        (should (null calls))))))

(ert-deftest aob-litellm-no-project-does-not-use-the-current-directory ()
  (aob-litellm-tests--with-root
    (let* ((project (aob-litellm-tests--project
                     (expand-file-name "conf" root) "alpha" aob-litellm-tests--key))
           (s (aob-litellm-tests--session-in 1 nil))
           (default-directory project))
      (should (null (aob-overall-usage-parts s)))
      (should (null calls))
      (setf (aob-session-dir s) project)
      (aob-overall-usage-parts s)
      (should (= (length calls) 1)))))

(provide 'aob-litellm-tests)
