;;; ygg-modal-consistency-tests.el --- The modal keys mean the same in every buffer -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(setq user-emacs-directory (file-name-as-directory (make-temp-file "ygg-modal" t)))
(require 'yggdrasil)
(require 'layer-completion)
(require 'layer-dired)
(require 'layer-terminal)
(require 'layer-git)
(require 'layer-sessions)
(require 'layer-aob)
(require 'ygg-projects)
(require 'magit)
(require 'magit-apply)
(require 'transient)
(require 'corfu)
(require 'ghostel nil t)
(require 'ygg-git-compare nil t)
(setq kill-emacs-hook nil)
(yggdrasil-global-mode 1)

(defun ygg-modal-tests--cmd (key)
  "The command KEY runs here, menu-item and label wrappers taken off."
  (pcase (key-binding (kbd key) t)
    (`(menu-item ,_ ,def . ,_) def)
    ((and `(,label . ,def) (guard (stringp label))) def)
    (def def)))

(defmacro ygg-modal-tests--in (mode text &rest body)
  "Run BODY in a modal MODE buffer holding TEXT, point at its start."
  (declare (indent 2))
  `(with-temp-buffer
     (funcall ,mode)
     (let ((inhibit-read-only t)) (insert ,text))
     (goto-char (point-min))
     (unless ygg--state (ygg--maybe-activate))
     (transient-mark-mode 1)
     ,@body))

;;; Insert verbs refuse read-only text

(ert-deftest ygg-modal-insert-refused-on-read-only-buffers ()
  (dolist (mode '(help-mode messages-buffer-mode Info-mode compilation-mode
                  aob-plan-mode))
    (ygg-modal-tests--in mode "some text\nmore\n"
      (setq buffer-read-only t)
      (should (eq ygg--state 'normal))
      (dolist (key '("i" "a" "A" "I"))
        (should-error (call-interactively (ygg-modal-tests--cmd key))
                      :type 'user-error)
        (should (eq ygg--state 'normal))))))

(ert-deftest ygg-modal-insert-refused-in-the-sessions-sidebar ()
  (with-temp-buffer
    (special-mode)
    (ygg-projects--setup (current-buffer))
    (let ((inhibit-read-only t)) (insert "row\n"))
    (goto-char (point-min))
    (should-error (ygg-insert-before 1) :type 'user-error)
    (should (eq ygg--state 'normal))))

(ert-deftest ygg-modal-insert-allowed-past-read-only-text ()
  (with-temp-buffer
    (insert (propertize "said\n" 'read-only t 'front-sticky '(read-only)
                        'rear-nonsticky '(read-only)))
    (yggdrasil-local-mode 1)
    (goto-char (point-max))
    (ygg-insert-before 1)
    (should (eq ygg--state 'insert))
    (ygg-normal-state)
    (goto-char (point-min))
    (should-error (ygg-insert-before 1) :type 'user-error)))

(ert-deftest ygg-modal-trace-A-still-types-the-prompt ()
  (with-temp-buffer
    (aob-trace-mode)
    (unless ygg--state (ygg--maybe-activate))
    (should (eq (ygg-modal-tests--cmd "A") 'aob-trace-input))
    (let ((inhibit-read-only t))
      (insert (propertize "said\n" 'read-only t 'front-sticky '(read-only)
                          'rear-nonsticky '(read-only))))
    (cl-letf (((symbol-function 'aob-trace--ensure-input) #'ignore))
      (aob-trace-input))
    (should (eq ygg--state 'insert))))

(ert-deftest ygg-modal-insert-elsewhere-enters-insert-on-read-only ()
  (with-temp-buffer
    (yggdrasil-local-mode 1)
    (setq buffer-read-only t)
    (setq-local ygg-insert-elsewhere t)
    (ygg-insert-before 1)
    (should (eq ygg--state 'insert)))
  (when (featurep 'ghostel)
    (should (memq #'ygg--ghostel-insert-elsewhere ghostel-mode-hook))
    (should (memq #'ygg--ghostel-insert-redirect ygg-insert-entry-hook))))

;;; C-[ escapes in graphic frames

(ert-deftest ygg-modal-ctrl-bracket-is-escape-in-graphic-frames ()
  (should-not (equal (lookup-key input-decode-map [?\e]) [escape]))
  (ygg--ctrl-bracket-escapes (selected-frame))
  (should-not (equal (lookup-key input-decode-map [?\e]) [escape]))
  (let ((saved (copy-keymap input-decode-map)))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
          (ygg--ctrl-bracket-escapes (selected-frame))
          (should (equal (lookup-key input-decode-map [?\e]) [escape])))
      (setq input-decode-map saved)))
  (should (memq #'ygg--ctrl-bracket-escapes after-make-frame-functions)))

;;; Leaving visual leaves no region behind in read-only modal buffers

(ert-deftest ygg-modal-visual-exit-drops-the-region-in-help ()
  (ygg-modal-tests--in #'help-mode "hello world\nsecond line\n"
    (ygg-toggle-visual)
    (forward-char 3)
    (should (region-active-p))
    (call-interactively (ygg-modal-tests--cmd "<escape>"))
    (should (eq ygg--state 'normal))
    (should-not (region-active-p))
    (should ygg--last-visual)
    (call-interactively (ygg-modal-tests--cmd "x"))
    (should (region-active-p))
    (should (= (region-beginning) (point-min)))))

(ert-deftest ygg-modal-visual-exit-drops-the-region-in-agent-buffers ()
  (ygg-modal-tests--in #'aob-plan-mode "plan\nsteps\n"
    (should (memq #'ygg--drop-region ygg-visual-exit-hook))
    (ygg-toggle-visual)
    (forward-char 2)
    (ygg-normal-state)
    (should-not (region-active-p))))

;;; Sidebar verbs live in normal and visual only

(ert-deftest ygg-modal-sidebar-keys-stay-out-of-insert ()
  (with-temp-buffer
    (special-mode)
    (ygg-projects--setup (current-buffer))
    (should (eq (ygg-modal-tests--cmd "j") 'ygg-projects-next))
    (ygg--switch-state 'visual)
    (should (eq (ygg-modal-tests--cmd "j") 'ygg-projects-next))
    (should (eq (ygg-modal-tests--cmd "a") 'ygg-projects-say))
    (ygg--switch-state 'insert)
    (should (eq (ygg-modal-tests--cmd "j") 'ygg--jk-escape))
    (dolist (key '("a" "A" "I" "x" "q"))
      (should-not (string-prefix-p "ygg-projects"
                                   (format "%s" (ygg-modal-tests--cmd key)))))))

;;; Magit: v and x select lines, _ reverses, V reverts

(ert-deftest ygg-modal-magit-v-and-x-select-lines ()
  (dolist (mode '(magit-status-mode magit-diff-mode magit-revision-mode
                  magit-log-mode magit-stash-mode magit-refs-mode))
    (with-temp-buffer
      (funcall mode)
      (should (eq (ygg-modal-tests--cmd "v") 'ygg-magit-select-lines))
      (should (eq (ygg-modal-tests--cmd "x") 'ygg-magit-select-lines))
      (should (eq (ygg-modal-tests--cmd "_") 'magit-revert-no-commit))
      (should (eq (ygg-modal-tests--cmd "V") 'magit-revert))))
  (with-temp-buffer
    (magit-status-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "+one\n+two\n+three\n" 'keymap magit-hunk-section-map)))
    (goto-char 2)
    (should (eq (key-binding (kbd "_") nil nil (point)) 'magit-reverse))
    (transient-mark-mode 1)
    (let ((exit (ygg-magit-select-lines)))
      (unwind-protect
          (progn
            (should (region-active-p))
            (should (= (mark) (point-min)))
            (should (memq (ygg-modal-tests--cmd "j") '(next-line magit-next-line)))
            (should (eq (ygg-modal-tests--cmd "<escape>") 'ygg-magit-select-lines))
            (forward-line 1)
            (call-interactively (ygg-modal-tests--cmd "<escape>"))
            (should-not (region-active-p)))
        (funcall exit))))
  (should (eq (plist-get (cdr (transient-get-suffix 'magit-dispatch "v")) :command)
              'magit-reverse)))

(ert-deftest ygg-modal-compare-keeps-its-own-v ()
  (skip-unless (fboundp 'ygg-git-compare-mode))
  (with-temp-buffer
    (magit-diff-mode)
    (ygg-git-compare-mode 1)
    (should-not (eq (ygg-modal-tests--cmd "v") 'ygg-magit-select-lines))))

;;; dired i edits names at once

(ert-deftest ygg-modal-dired-i-lands-in-insert ()
  (let ((dir (make-temp-file "ygg-modal-dired" t)))
    (write-region "" nil (expand-file-name "name" dir))
    (unwind-protect
        (with-current-buffer (dired-noselect dir)
          (unwind-protect
              (progn
                (call-interactively (ygg-modal-tests--cmd "i"))
                (should (derived-mode-p 'wdired-mode))
                (should (eq ygg--state 'insert))
                (ygg-normal-state)
                (goto-char (point-min))
                (dired-goto-file (expand-file-name "name" dir))
                (call-interactively (ygg-modal-tests--cmd "A"))
                (should (eq ygg--state 'insert)))
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (delete-directory dir t))))

;;; Esc quits a transient menu

(ert-deftest ygg-modal-transient-esc-quits ()
  (should (eq (lookup-key transient-map [escape]) 'transient-quit-one))
  (should (eq (lookup-key transient-base-map [escape]) 'transient-quit-one))
  (should (eq (lookup-key transient-sticky-map [escape]) 'transient-quit-seq))
  (should (lookup-key transient-predicate-map [transient-quit-one])))

;;; Corfu's keys win in insert while its popup is up

(ert-deftest ygg-modal-corfu-esc-closes-popup-then-leaves-insert ()
  (should (< (cl-position 'ygg-corfu--keys emulation-mode-map-alists)
             (cl-position 'ygg--mode-keys-alist emulation-mode-map-alists)))
  (should (< (cl-position 'ygg-corfu--keys emulation-mode-map-alists)
             (cl-position 'ygg--emulation-alist emulation-mode-map-alists)))
  (ygg-modal-tests--in #'text-mode "fo"
    (goto-char (point-max))
    (ygg-insert-state)
    (let ((completion-in-region-mode-predicate #'always))
      (corfu--setup (point-min) (point-max) '("foo" "fob") nil))
    (unwind-protect
        (progn
          (should completion-in-region-mode)
          (should (eq (ygg-modal-tests--cmd "<escape>") 'corfu-quit))
          (should (eq (ygg-modal-tests--cmd "C-g") 'corfu-quit))
          (call-interactively (ygg-modal-tests--cmd "<escape>"))
          (should-not completion-in-region-mode)
          (should-not ygg-corfu--keys)
          (should (eq ygg--state 'insert))
          (should (eq (ygg-modal-tests--cmd "<escape>") 'ygg-normal-state)))
      (when completion-in-region-mode (corfu-quit))))
  (with-temp-buffer
    (should-not (eq (let ((completion-in-region-mode t))
                      (ygg-modal-tests--cmd "<escape>"))
                    'corfu-quit))))

;;; ygg-modal-consistency-tests.el ends here
