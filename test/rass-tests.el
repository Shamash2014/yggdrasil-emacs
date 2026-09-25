;;; rass-tests.el --- Every eglot server beside harper -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'layer-rass)

(defmacro rass-tests--with-bins (&rest body)
  `(cl-letf (((symbol-function 'executable-find) (lambda (name &rest _) (concat "/bin/" name))))
     (let ((default-directory "/tmp/"))
       ,@body)))

(ert-deftest rass-wraps-a-stdio-server-and-keeps-its-options-last ()
  (rass-tests--with-bins
   (should (equal (ygg-rass-with-harper '("gopls" :initializationOptions (:a 1)))
                  `("rass" "--no-stream-diagnostics" ,ygg-rass-harper-preset
                    "--" "gopls" "--" "harper-ls" "--stdio"
                    :initializationOptions (:a 1))))))

(ert-deftest rass-leaves-what-it-cannot-carry ()
  (rass-tests--with-bins
   (dolist (contact '(("localhost" 6009)
                      ("server" "--port" :autoport)
                      ("harper-ls" "--stdio")
                      ("rass" "python" "--" "x")))
     (should (equal (ygg-rass-with-harper contact) contact)))
   (let ((default-directory "/ssh:host:/tmp/"))
     (should (equal (ygg-rass-with-harper '("gopls")) '("gopls")))))
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should (equal (ygg-rass-with-harper '("gopls")) '("gopls")))))

(ert-deftest rass-leaves-a-command-typed-at-the-prompt-as-typed ()
  (rass-tests--with-bins
   (let ((guess (lambda (&optional _) (list '(java-mode) nil 'eglot-lsp-server '("pylsp" "-v") nil))))
     (let ((current-prefix-arg '(4)))
       (should (equal (nth 3 (ygg-rass--guess-contact guess t)) '("pylsp" "-v"))))
     (should (equal (car (nth 3 (ygg-rass--guess-contact guess nil))) "rass")))))

(ert-deftest rass-keeps-the-language-server-visible-behind-it ()
  (require 'layer-lsp)
  (rass-tests--with-bins
   (let ((wrapped (ygg-rass-with-harper '("env" "JAVA_HOME=/jdk" "jdtls" "-data" "/x"))))
     (cl-letf (((symbol-function 'jsonrpc--process) (lambda (_) 'proc))
               ((symbol-function 'process-command) (lambda (_) wrapped)))
       (should (equal (ygg-lsp--server-binary 'server) "jdtls"))))
   (should (equal (ygg-lsp--primary-command '("gopls" "serve")) '("gopls" "serve")))))

(provide 'rass-tests)
;;; rass-tests.el ends here
