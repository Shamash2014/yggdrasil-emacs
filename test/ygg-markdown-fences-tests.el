;;; ygg-markdown-fences-tests.el --- fenced code blocks drawn inline by language -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'markdown-mode)
(require 'ygg-ui)
(require 'ygg-markdown-fences)
(require 'layer-markdown)

(defconst ygg-markdown-fences-tests--samples
  '(("ts" "const x = 1;" "const" font-lock-keyword-face)
    ("tsx" "const x = <div/>;" "const" font-lock-keyword-face)
    ("py" "def f(): pass" "def" font-lock-keyword-face)
    ("rust" "fn main() {}" "fn" font-lock-keyword-face)
    ("go" "func main() {}" "func" font-lock-keyword-face)
    ("dart" "class A {}" "class" font-lock-keyword-face)
    ("elisp" "(defun f () 1)" "defun" font-lock-keyword-face)
    ("json" "{\"a\": true}" "true" font-lock-constant-face)
    ("yaml" "a: true" "true" font-lock-constant-face)
    ("kotlin" "fun f() {}" "fun" font-lock-keyword-face)
    ("swift" "func f() {}" "func" font-lock-keyword-face)
    ("sh" "if true; then echo; fi" "if" font-lock-keyword-face)))

(defun ygg-markdown-fences-tests--fenced (tag body)
  (format "Before\n\n```%s\n%s\n```\n\nAfter\n" tag body))

(defun ygg-markdown-fences-tests--face-at (text needle)
  (let ((start (string-match (regexp-quote needle) text
                             (string-match "\n```[^\n]*\n" text))))
    (ensure-list (get-text-property start 'font-lock-face text))))

(dolist (sample ygg-markdown-fences-tests--samples)
  (let ((tag (nth 0 sample)))
    (eval
     `(ert-deftest ,(intern (format "ygg-markdown-fences-%s-highlights" tag)) ()
        (let ((mode (ygg-markdown-fences-mode ,tag)))
          (unless mode (ert-skip (format "no mode for %s" ,tag)))
          (let ((faces (ygg-markdown-fences-tests--face-at
                        (ygg-ui-markdown (ygg-markdown-fences-tests--fenced
                                          ,tag ,(nth 1 sample)))
                        ,(nth 2 sample))))
            (unless (cl-some (lambda (f) (string-prefix-p "font-lock-" (symbol-name f)))
                             faces)
              (ert-skip (format "%s gave no code faces in batch" mode)))
            (should (memq ',(nth 3 sample) faces)))))
     t)))

(ert-deftest ygg-markdown-fences-remap-wins ()
  (let ((major-mode-remap-alist '((sh-mode . fundamental-mode)))
        (ygg-markdown-fences-languages '((("zz") nil nil sh-mode))))
    (should (eq (ygg-markdown-fences-mode "zz") 'fundamental-mode))))

(ert-deftest ygg-markdown-fences-unknown-tag-falls-through ()
  (should-not (ygg-markdown-fences-mode "nonesuch")))

(ert-deftest ygg-markdown-fences-long-text-keeps-code-faces ()
  (let* ((text (concat (make-string (* 2 ygg-ui-markdown-max) ?a)
                       (ygg-markdown-fences-tests--fenced "elisp" "(defun f () 1)")))
         (out (ygg-ui-markdown text)))
    (should (> (length text) ygg-ui-markdown-max))
    (should (memq 'font-lock-keyword-face
                  (ygg-markdown-fences-tests--face-at out "defun")))
    (should (memq 'ygg-markdown-fences-block
                  (ygg-markdown-fences-tests--face-at out "defun")))))

(ert-deftest ygg-markdown-fences-long-text-open-block-is-fontified ()
  (let ((out (ygg-ui-markdown (concat (make-string (* 2 ygg-ui-markdown-max) ?a)
                                      "\n```elisp\n(defun f () 1)"))))
    (should (memq 'font-lock-keyword-face
                  (ygg-markdown-fences-tests--face-at out "defun")))))

(ert-deftest ygg-markdown-fences-code-cap-leaves-the-rest-plain ()
  (let* ((ygg-markdown-fences-code-max 10)
         (text (concat (make-string (* 2 ygg-ui-markdown-max) ?a)
                       (ygg-markdown-fences-tests--fenced "elisp" "(defun f () 1)")))
         (out (ygg-ui-markdown text)))
    (should-not (get-text-property (string-match "defun" out) 'font-lock-face out))))

(ert-deftest ygg-markdown-fences-long-text-hides-fences ()
  (let* ((text (concat (make-string (* 2 ygg-ui-markdown-max) ?a)
                       (ygg-markdown-fences-tests--fenced "elisp" "(defun f () 1)")))
         (fence (string-match "```elisp" text))
         (close (string-match "\n```\n" text)))
    (let* ((ygg-ui-markdown-hide-markup t) (out (ygg-ui-markdown text)))
      (should (get-text-property (+ fence 3) 'display out))
      (should-not (get-text-property (+ fence 3) 'invisible out))
      (should (eq 'markdown-markup (get-text-property (1+ close) 'invisible out))))
    (let* ((ygg-ui-markdown-hide-markup nil) (out (ygg-ui-markdown text)))
      (should-not (get-text-property (+ fence 3) 'display out))
      (should-not (get-text-property (1+ close) 'invisible out)))))

(defun ygg-markdown-fences-tests--buffer-invisible (hide inline)
  (with-temp-buffer
    (insert (ygg-markdown-fences-tests--fenced "elisp" "(defun f () 1)"))
    (let ((markdown-hide-markup hide)
          (markdown-fontify-code-blocks-natively t))
      (delay-mode-hooks (gfm-mode))
      (setq ygg-markdown-fences-inline inline)
      (font-lock-ensure)
      (let ((pos (save-excursion (goto-char (point-min))
                                 (re-search-forward "```elisp\n") (- (point) 4))))
        (list (invisible-p pos)
              (get-text-property (- pos 2) 'display)
              (get-text-property (- pos 1) 'display))))))

(ert-deftest ygg-markdown-fences-buffer-hides-fences-with-markup-hiding ()
  (pcase-let ((`(,hidden ,_ ,_) (ygg-markdown-fences-tests--buffer-invisible t t)))
    (should-not hidden))
  (pcase-let ((`(,_ ,label ,_) (ygg-markdown-fences-tests--buffer-invisible t t)))
    (should label)))

(ert-deftest ygg-markdown-fences-buffer-shows-fences-without-markup-hiding ()
  (pcase-let ((`(,hidden ,label ,_) (ygg-markdown-fences-tests--buffer-invisible nil t)))
    (should-not hidden)
    (should-not label)))

(ert-deftest ygg-markdown-fences-buffer-raw-shows-fences ()
  (pcase-let ((`(,hidden ,label ,_) (ygg-markdown-fences-tests--buffer-invisible t nil)))
    (should-not hidden)
    (should-not label)))

(defun ygg-markdown-fences-tests--buffer-props (text hide)
  (with-temp-buffer
    (insert text)
    (let ((markdown-hide-markup hide)
          (markdown-fontify-code-blocks-natively t))
      (delay-mode-hooks (gfm-mode))
      (font-lock-ensure)
      (let ((fence (progn (goto-char (point-min))
                          (re-search-forward "^```")
                          (match-beginning 0))))
        (list (invisible-p fence)
              (get-text-property fence 'display)
              (save-excursion
                (goto-char fence)
                (and (re-search-forward "defun" nil t)
                     (ensure-list (get-text-property (1- (point)) 'face)))))))))

(ert-deftest ygg-markdown-fences-unclosed-buffer-block-keeps-its-fence ()
  (pcase-let ((`(,hidden ,label ,faces)
               (ygg-markdown-fences-tests--buffer-props
                "A\n\n```elisp\n(defun f () 1)\n" t)))
    (should-not hidden)
    (should label)
    (should (memq 'font-lock-keyword-face faces))
    (should (memq 'ygg-markdown-fences-block faces))))

(ert-deftest ygg-markdown-fences-unclosed-untagged-buffer-block-is-not-hidden ()
  (pcase-let ((`(,hidden ,_ ,_)
               (ygg-markdown-fences-tests--buffer-props
                "A\n\n```\n(defun f () 1)\n" t)))
    (should-not hidden)))

(ert-deftest ygg-markdown-fences-unclosed-buffer-block-raw-is-not-hidden ()
  (let ((ygg-markdown-fences-inline nil))
    (pcase-let ((`(,hidden ,label ,_)
                 (ygg-markdown-fences-tests--buffer-props
                  "A\n\n```elisp\n(defun f () 1)\n" t)))
      (should-not hidden)
      (should-not label))))

(ert-deftest ygg-markdown-fences-empty-tagged-buffer-block-shows-label ()
  (pcase-let ((`(,hidden ,label ,_)
               (ygg-markdown-fences-tests--buffer-props
                "A\n\n```py\n```\n\nB\n" t)))
    (should-not hidden)
    (should (string-match-p "py" label))))

(ert-deftest ygg-markdown-fences-empty-tagged-long-text-block-shows-label ()
  (let* ((text (concat (make-string (* 2 ygg-ui-markdown-max) ?a)
                       "\n\n```py\n```\n\nB\n"))
         (fence (string-match "```py" text))
         (ygg-ui-markdown-hide-markup t)
         (out (ygg-ui-markdown text)))
    (should (get-text-property (+ fence 3) 'display out))))

(ert-deftest ygg-markdown-fences-long-text-respects-inline-off ()
  (let* ((text (concat (make-string (* 2 ygg-ui-markdown-max) ?a)
                       (ygg-markdown-fences-tests--fenced "elisp" "(defun f () 1)")))
         (fence (string-match "```elisp" text))
         (ygg-ui-markdown-hide-markup t)
         (ygg-markdown-fences-inline nil)
         (out (ygg-ui-markdown text)))
    (should-not (get-text-property (+ fence 3) 'display out))
    (should-not (memq 'ygg-markdown-fences-block
                      (ygg-markdown-fences-tests--face-at out "defun")))
    (should (memq 'font-lock-keyword-face
                  (ygg-markdown-fences-tests--face-at out "defun")))))

(defmacro ygg-markdown-fences-tests--fresh-caches (&rest body)
  `(let ((ygg-markdown-fences--grammars (make-hash-table :test 'eq))
         (ygg-markdown-fences--modes (make-hash-table :test 'eq))
         (treesit-extra-load-path treesit-extra-load-path))
     ,@body))

(ert-deftest ygg-markdown-fences-failed-grammar-is-asked-again-after-path-change ()
  (ygg-markdown-fences-tests--fresh-caches
   (let ((ready nil) (asked 0))
     (cl-letf (((symbol-function 'treesit-ready-p)
                (lambda (&rest _) (cl-incf asked) ready)))
       (should-not (ygg-markdown-fences--grammar-ready 'zz))
       (should-not (ygg-markdown-fences--grammar-ready 'zz))
       (should (= asked 1))
       (setq ready t)
       (should-not (ygg-markdown-fences--grammar-ready 'zz))
       (push "/somewhere/new" treesit-extra-load-path)
       (should (ygg-markdown-fences--grammar-ready 'zz))
       (setq ready nil)
       (should (ygg-markdown-fences--grammar-ready 'zz))
       (should (= asked 2))))))

(ert-deftest ygg-markdown-fences-failed-grammar-expires ()
  (ygg-markdown-fences-tests--fresh-caches
   (let ((ready nil) (now (float-time)))
     (cl-letf (((symbol-function 'treesit-ready-p) (lambda (&rest _) ready))
               ((symbol-function 'float-time) (lambda (&rest _) now)))
       (should-not (ygg-markdown-fences--grammar-ready 'zz))
       (setq ready t)
       (cl-incf now (1- ygg-markdown-fences--failure-ttl))
       (should-not (ygg-markdown-fences--grammar-ready 'zz))
       (cl-incf now 2)
       (should (ygg-markdown-fences--grammar-ready 'zz))))))

(ert-deftest ygg-markdown-fences-absent-mode-is-required-once ()
  (ygg-markdown-fences-tests--fresh-caches
   (let* ((asked 0)
           (probe (lambda (feature &rest _)
                   (when (eq feature 'ygg-fences-nonesuch-mode)
                     (cl-incf asked)))))
     (advice-add 'require :before probe)
     (unwind-protect
         (progn
           (should-not (ygg-markdown-fences--usable 'ygg-fences-nonesuch-mode))
           (should-not (ygg-markdown-fences--usable 'ygg-fences-nonesuch-mode))
           (should (= asked 1))
           (let ((load-path (cons "/somewhere/new" load-path)))
             (should-not (ygg-markdown-fences--usable 'ygg-fences-nonesuch-mode)))
           (should (= asked 2)))
       (advice-remove 'require probe)))))

(defmacro ygg-markdown-fences-tests--define-theme (name background foreground)
  `(progn
     (deftheme ,name)
     (custom-theme-set-faces
      ',name
      '(default ((t :background ,background :foreground ,foreground)))
      '(fringe ((t :background unspecified))))
     (provide-theme ',name)))

(ygg-markdown-fences-tests--define-theme ygg-fences-test-light "#f3f0e8" "#141414")
(ygg-markdown-fences-tests--define-theme ygg-fences-test-dark "#080808" "#bcbcbc")
(ygg-markdown-fences-tests--define-theme ygg-fences-test-bare "unspecified-bg" "unspecified-fg")

(defun ygg-markdown-fences-tests--with-theme (theme body)
  (let ((enabled custom-enabled-themes))
    (unwind-protect
        (progn (mapc #'disable-theme custom-enabled-themes)
               (enable-theme theme)
               (funcall body))
      (mapc #'disable-theme custom-enabled-themes)
      (mapc #'enable-theme (reverse enabled))
      (ygg-markdown-fences--restyle))))

(defun ygg-markdown-fences-tests--block-bg ()
  (face-attribute 'ygg-markdown-fences-block :background nil t))

(defun ygg-markdown-fences-tests--default-bg ()
  (face-attribute 'default :background nil t))

(ert-deftest ygg-markdown-fences-block-is-visible-without-a-fringe ()
  (dolist (theme '(ygg-fences-test-light ygg-fences-test-dark))
    (ygg-markdown-fences-tests--with-theme
     theme
     (lambda ()
       (let ((block (ygg-markdown-fences-tests--block-bg)))
         (should (color-name-to-rgb block))
         (should-not (equal block (ygg-markdown-fences-tests--default-bg)))
         (should (ygg-markdown-fences-tests--face-extends-p)))))))

(defun ygg-markdown-fences-tests--face-extends-p ()
  (eq t (face-attribute 'ygg-markdown-fences-block :extend nil t)))

(ert-deftest ygg-markdown-fences-block-follows-the-theme ()
  (let (light dark)
    (ygg-markdown-fences-tests--with-theme
     'ygg-fences-test-light
     (lambda ()
       (setq light (ygg-markdown-fences-tests--block-bg))
       (disable-theme 'ygg-fences-test-light)
       (enable-theme 'ygg-fences-test-dark)
       (setq dark (ygg-markdown-fences-tests--block-bg))))
    (should-not (equal light dark))
    (should (> (apply #'+ (color-name-to-rgb light))
               (apply #'+ (color-name-to-rgb dark))))))

(ert-deftest ygg-markdown-fences-block-falls-back-without-a-background ()
  (ygg-markdown-fences-tests--with-theme
   'ygg-fences-test-bare
   (lambda ()
     (should (eq (face-attribute 'ygg-markdown-fences-block :inherit nil t)
                 'secondary-selection)))))

(ert-deftest ygg-markdown-fences-toggle-key-is-bound ()
  (dolist (mode '(markdown-mode gfm-mode))
    (should (eq (lookup-key (ygg-localleader--get-map mode) (kbd "f"))
                #'ygg-markdown-fences-toggle))))

(ert-deftest ygg-markdown-fences-toggle-flips-the-buffer ()
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (let ((before ygg-markdown-fences-inline))
      (ygg-markdown-fences-toggle)
      (should-not (eq before ygg-markdown-fences-inline))
      (should (default-value 'ygg-markdown-fences-inline)))))

(defconst ygg-markdown-fences-tests--budget-ms 600
  "Most milliseconds 20 renders of a 3k text with 3 fences may take.")

(ert-deftest ygg-markdown-fences-render-timing ()
  (let* ((prose (make-string 600 ?a))
         (text (concat prose
                       (ygg-markdown-fences-tests--fenced "elisp" "(defun f () 1)")
                       prose
                       (ygg-markdown-fences-tests--fenced "py" "def f(): pass")
                       prose
                       (ygg-markdown-fences-tests--fenced "ts" "const x = 1;")
                       prose))
         (start (float-time)))
    (should (<= 2500 (length text) 3500))
    (ygg-ui-markdown text)
    (setq start (float-time))
    (dotimes (i 20)
      (ygg-ui-markdown (concat text (number-to-string i))))
    (should (< (* 1000 (- (float-time) start))
               ygg-markdown-fences-tests--budget-ms))))

(provide 'ygg-markdown-fences-tests)
;;; ygg-markdown-fences-tests.el ends here
