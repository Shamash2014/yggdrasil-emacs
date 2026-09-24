;;; visidata-tests.el --- Tests for the VisiData viewer -*- lexical-binding: t; -*-

(require 'ert)
(require 'ygg-visidata)

(ert-deftest ygg-visidata-python-code-writes-the-named-object-into-the-directory ()
  (let ((code (ygg-visidata-kernel-code "python" "df" "/tmp/ygg-vd-x/")))
    (should (string-prefix-p "(lambda __ygg_vd_ns: (exec(" code))
    (should (string-suffix-p
             "__ygg_vd_ns['export'](df, \"/tmp/ygg-vd-x\", \"df\"))[1])({})" code))
    (should (string-match-p "to_parquet" code))
    (should (string-match-p "to_csv" code))))

(ert-deftest ygg-visidata-python-code-is-one-expression-python-parses ()
  (let ((python (or (executable-find "python3") (ert-skip "no python3")))
        (code (ygg-visidata-kernel-code "python" "frame" "/tmp/d")))
    (with-temp-buffer
      (should (zerop (call-process python nil t nil "-c"
                                   "import ast, sys; ast.parse(sys.argv[1], mode='eval')"
                                   code))))))

(ert-deftest ygg-visidata-r-code-prefers-arrow-and-installs-nothing ()
  (let ((code (ygg-visidata-kernel-code "R" "df" "/tmp/ygg-vd-x")))
    (should (string-match-p "\\`local({\n  obj <- df\n" code))
    (should (string-match-p "file.path(\"/tmp/ygg-vd-x\", \"df\")" code))
    (should (string-match-p "requireNamespace(\"arrow\", quietly = TRUE)" code))
    (should (string-match-p "data.table::fwrite" code))
    (should (string-match-p "utils::write.csv" code))
    (should-not (string-match-p "install.packages" code))))

(ert-deftest ygg-visidata-r-code-parses-in-r ()
  (let ((rscript (or (executable-find "Rscript") (ert-skip "no Rscript")))
        (code (ygg-visidata-kernel-code "R" "df" "/tmp/d")))
    (with-temp-buffer
      (should (zerop (call-process rscript nil t nil "-e"
                                   (format "invisible(parse(text = %s))"
                                           (json-encode-string code))))))))

(ert-deftest ygg-visidata-an-expression-gets-a-safe-file-stem ()
  (let ((code (ygg-visidata-kernel-code "python" "df[df.x > 0]" "/tmp/d")))
    (should (string-match-p "__ygg_vd_ns\\['export'\\](df\\[df.x > 0\\], \"/tmp/d\", \"df_df.x_0_\")" code)))
  (should (equal (ygg-visidata--stem "***") "data")))

(ert-deftest ygg-visidata-paths-with-quotes-stay-literal ()
  (let ((code (ygg-visidata-kernel-code "R" "df" "/tmp/it's \"here\"")))
    (should (string-match-p (regexp-quote "\"/tmp/it's \\\"here\\\"\"") code))))

(ert-deftest ygg-visidata-other-kernels-are-refused ()
  (should-error (ygg-visidata-kernel-code "julia" "df" "/tmp/d") :type 'user-error))

(ert-deftest ygg-visidata-reads-frame-names-from-python-and-r-output ()
  (should (equal (ygg-visidata--quoted-names "['df', 'sales_2024']") '("df" "sales_2024")))
  (should (equal (ygg-visidata--quoted-names "[1] \"df\"   \"iris.x\"") '("df" "iris.x")))
  (should (equal (ygg-visidata--quoted-names "character(0)") nil)))

(ert-deftest ygg-visidata-temp-directory-goes-with-the-buffer ()
  (let* ((dir (make-temp-file "ygg-vd-test-" t))
         (file (expand-file-name "df.parquet" dir))
         (buffer (generate-new-buffer "*vd-test*")))
    (write-region "x" nil file)
    (ygg-visidata-remove-with-buffer buffer dir)
    (should (file-exists-p file))
    (kill-buffer buffer)
    (should-not (file-exists-p file))
    (should-not (file-exists-p dir))))

(ert-deftest ygg-visidata-command-line-runs-vd-with-the-paper-rc ()
  (let ((ygg-visidata-args '("--header" "1")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &rest _) (concat "/opt/bin/" name))))
      (should (equal (ygg-visidata-command-line "/data/sales.csv")
                     (list "/opt/bin/vd" "--config" ygg-visidata-config
                           "--header" "1" "/data/sales.csv"))))))

(ert-deftest ygg-visidata-config-is-the-shipped-rc-not-the-users ()
  (should (file-exists-p ygg-visidata-config))
  (should (string-suffix-p "etc/visidatarc" ygg-visidata-config))
  (should-not (string-match-p "\\.visidatarc" ygg-visidata-config)))

(ert-deftest ygg-visidata-missing-vd-is-a-user-error ()
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should-error (ygg-visidata-command-line "/data/x.csv") :type 'user-error)))

(ert-deftest ygg-visidata-cleanup-survives-a-directory-already-gone ()
  (let ((dir (make-temp-file "ygg-vd-test-" t))
        (buffer (generate-new-buffer "*vd-test*")))
    (ygg-visidata-remove-with-buffer buffer dir)
    (delete-directory dir t)
    (kill-buffer buffer)
    (should-not (buffer-live-p buffer))))

(ert-deftest ygg-visidata-kernel-language-symbol-becomes-a-lowercase-key ()
  (cl-letf (((symbol-function 'jupyter-kernel-language) (lambda (&rest _) 'R)))
    (should (equal (ygg-visidata--language 'client) "r"))))

(ert-deftest ygg-visidata-a-file-from-the-host-lands-under-the-local-directory ()
  (should (equal (ygg-visidata-home-name "/ssh:jtest:/tmp/ygg-vd-abc/df.parquet" "/var/tmp/home/")
                 "/var/tmp/home/df.parquet")))

(defmacro ygg-visidata-test-remote (removed evaluated &rest body)
  "Run BODY with a kernel on host jtest stubbed; REMOVED and EVALUATED collect."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'ygg-visidata--remote) (lambda (_) "/ssh:jtest:"))
             ((symbol-function 'ygg-visidata--workspace)
              (lambda (remote) (concat remote "/tmp/ygg-vd-1")))
             ((symbol-function 'ygg-visidata--written)
              (lambda (dir) (concat dir "/df.parquet")))
             ((symbol-function 'ygg-visidata--copy-home)
              (lambda (file) (ygg-visidata-home-name file "/var/tmp/home/")))
             ((symbol-function 'ygg-visidata--remove-directory)
              (lambda (dir) (push dir ,removed)))
             ((symbol-function 'ygg-visidata--eval)
              (lambda (_client code) (push code ,evaluated))))
     ,@body))

(ert-deftest ygg-visidata-a-remote-kernel-writes-to-its-own-path-and-the-copy-comes-home ()
  (let (removed evaluated)
    (ygg-visidata-test-remote removed evaluated
      (should (equal (ygg-visidata--export 'client "python" "df") "/var/tmp/home/df.parquet")))
    (should (string-match-p (regexp-quote "\"/tmp/ygg-vd-1\", \"df\"") (car evaluated)))
    (should-not (string-match-p "/ssh:" (car evaluated)))
    (should (equal removed '("/ssh:jtest:/tmp/ygg-vd-1")))))

(ert-deftest ygg-visidata-a-failed-remote-export-still-clears-the-host ()
  (let (removed evaluated)
    (ygg-visidata-test-remote removed evaluated
      (cl-letf (((symbol-function 'ygg-visidata--eval) (lambda (&rest _) (error "NameError"))))
        (should-error (ygg-visidata--export 'client "python" "df") :type 'user-error)))
    (should (equal removed '("/ssh:jtest:/tmp/ygg-vd-1")))))

(ert-deftest ygg-visidata-a-local-export-keeps-its-directory-for-vd ()
  (let ((dir (make-temp-file "ygg-vd-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'ygg-visidata--remote) #'ignore)
                  ((symbol-function 'ygg-visidata--workspace) (lambda (_) dir))
                  ((symbol-function 'ygg-visidata--eval)
                   (lambda (&rest _) (write-region "a\n1\n" nil (expand-file-name "df.csv" dir)))))
          (should (equal (ygg-visidata--export 'client "python" "df")
                         (expand-file-name "df.csv" dir)))
          (should (file-directory-p dir)))
      (delete-directory dir t))))

(ert-deftest ygg-visidata-copies-a-file-home-and-leaves-nothing-on-failure ()
  (let* ((src (make-temp-file "ygg-vd-src-" t))
         (file (expand-file-name "df.csv" src)))
    (unwind-protect
        (progn
          (write-region "a\n1\n" nil file)
          (let ((copy (ygg-visidata--copy-home file)))
            (unwind-protect
                (progn (should-not (equal copy file))
                       (should (equal (file-name-nondirectory copy) "df.csv"))
                       (should (file-exists-p copy)))
              (delete-directory (file-name-directory copy) t)))
          (let (made)
            (cl-letf* ((real (symbol-function 'make-temp-file))
                       ((symbol-function 'make-temp-file)
                        (lambda (&rest args) (car (push (apply real args) made)))))
              (should-error (ygg-visidata--copy-home (expand-file-name "gone.csv" src))))
            (should-not (file-exists-p (car made)))))
      (delete-directory src t))))

(ert-deftest ygg-visidata-refuses-a-kernel-behind-a-far-server ()
  (cl-letf (((symbol-function 'ygg-kernel-picker-server-url) (lambda (_) "http://gpu:8888")))
    (should-error (ygg-visidata--remote 'client) :type 'user-error))
  (cl-letf (((symbol-function 'ygg-kernel-picker-server-url) (lambda (_) "http://localhost:8888"))
            ((symbol-function 'ygg-kernel-picker-remote) #'ignore))
    (should-not (ygg-visidata--remote 'client))))

(ert-deftest ygg-visidata-refuses-a-kernel-with-no-frames-before-asking ()
  (cl-letf (((symbol-function 'jupyter-kernel-language) (lambda (_) 'ruby)))
    (should-error (ygg-visidata--exportable-language 'client) :type 'user-error))
  (cl-letf (((symbol-function 'jupyter-kernel-language) (lambda (_) 'python)))
    (should (equal (ygg-visidata--exportable-language 'client) "python"))))

(ert-deftest ygg-visidata-keys-live-under-the-jupyter-leader ()
  (skip-unless (require 'layer-notebook nil t))
  (should (eq (lookup-key ygg-leader-jupyter-map "t") 'ygg-visidata-view))
  (should (eq (lookup-key ygg-leader-jupyter-map "T") 'ygg-visidata-open-file))
  (dolist (mode '(python-mode python-ts-mode r-ts-mode jupyter-repl-mode))
    (let ((map (ygg-localleader--get-map mode)))
      (should-not (memq (lookup-key map "d") '(ygg-visidata-view)))
      (should-not (memq (lookup-key map "D") '(ygg-visidata-open-file))))))

(provide 'visidata-tests)
;;; visidata-tests.el ends here
