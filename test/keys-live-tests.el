;;; keys-live-tests.el --- Every key reaches a command that exists -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)

(defvar ygg-leader-map)
(defvar ygg-localleader--maps)
(defvar ygg-ex--commands)
(defvar embark-keymap-alist)

(defconst keys-live-tests--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defvar keys-live-tests--log nil
  "Every leader binding made while init loads, as (MAP KEY DEF FILE).")

(defun keys-live-tests--record (map key def)
  (push (list map (kbd key) def (and load-file-name (file-name-nondirectory load-file-name)))
        keys-live-tests--log))

(defun keys-live-tests--on-define-keys (state &rest bindings)
  (let ((map (cond ((keymapp state) state)
                   ((and (symbolp state) (boundp state)) (symbol-value state)))))
    (while bindings
      (let ((key (pop bindings))
            (def (pop bindings)))
        (when (eq (car bindings) :label)
          (pop bindings)
          (pop bindings))
        (when map (keys-live-tests--record map key def))))))

(defun keys-live-tests--on-leader-def (key def &optional _label)
  (keys-live-tests--record 'leader key def))

(unless (featurep 'yggdrasil)
  (advice-add 'yggdrasil-define-keys :before #'keys-live-tests--on-define-keys)
  (advice-add 'yggdrasil-leader-def :before #'keys-live-tests--on-leader-def)
  (let ((default-directory (file-name-as-directory keys-live-tests--root))
        (inhibit-message t))
    (unwind-protect
        (progn
          (load (expand-file-name "early-init.el" keys-live-tests--root) nil t)
          (load (expand-file-name "init.el" keys-live-tests--root) nil t)
          (when (fboundp 'elpaca-process-queues)
            (elpaca-process-queues)
            (when (fboundp 'elpaca-wait) (elpaca-wait)))
          (require 'embark nil t))
      ;; a batch exit must never save the owner's sessions
      (setq kill-emacs-hook nil)
      (when (fboundp 'easysession-save-mode) (easysession-save-mode -1)))))

(defun keys-live-tests--command (binding)
  "The command BINDING runs once its which-key label is peeled off."
  (pcase binding
    (`(menu-item ,_ ,def . ,_) def)
    ((and `(,label . ,def) (guard (stringp label))) def)
    (_ binding)))

(defun keys-live-tests--prefix-map (def)
  (cond ((keymapp def) (if (symbolp def) (symbol-function def) def))
        ((and (symbolp def) (boundp def) (keymapp (symbol-value def)))
         (symbol-value def))))

(defun keys-live-tests--dead-p (def)
  "Non-nil when DEF names a command that is missing or can never load."
  (and (symbolp def) def
       (not (keymapp def))
       (or (not (fboundp def))
           (let ((fn (symbol-function def)))
             (while (and (symbolp fn) (fboundp fn)) (setq fn (symbol-function fn)))
             (or (and (symbolp fn) (not (fboundp fn)))
                 (and (autoloadp fn) (not (locate-library (cadr fn)))))))))

(defun keys-live-tests--walk (map prefix seen visit &optional own-only)
  "Call VISIT with each key and command under MAP, following prefix maps.
Maps in SEEN are not entered."
  (unless (memq map seen)
    (push map seen)
    (funcall (if own-only #'map-keymap-internal #'map-keymap)
             (lambda (event binding)
               (let* ((key (vconcat prefix (vector event)))
                      (def (keys-live-tests--command binding))
                      (sub (keys-live-tests--prefix-map def)))
                 (if sub
                     (keys-live-tests--walk sub key seen visit)
                   (funcall visit key def))))
             map)))

(defun keys-live-tests--dead-under (map name &optional own-only skip)
  "The dead keys under MAP, named after NAME, leaving the maps in SKIP out."
  (let (dead)
    (keys-live-tests--walk
     map [] skip
     (lambda (key def)
       (when (keys-live-tests--dead-p def)
         (push (format "%s %s -> %s" name (key-description key) def) dead)))
     own-only)
    dead))

(defun keys-live-tests--load-mode (mode)
  "Define MODE the way opening its first buffer would, or nil when nothing can."
  (cond ((autoloadp (symbol-function mode)) (autoload-do-load (symbol-function mode) mode))
        ((not (fboundp mode))
         (require (intern (string-remove-suffix "-mode" (symbol-name mode))) nil t)))
  (fboundp mode))

(defun keys-live-tests--embark-maps ()
  "The embark keymaps this config adds, as (NAME . MAP)."
  (let (maps)
    (dolist (cell (and (boundp 'embark-keymap-alist) embark-keymap-alist))
      (dolist (sym (ensure-list (cdr cell)))
        (when (and (symbolp sym) (string-prefix-p "ygg-" (symbol-name sym))
                   (boundp sym) (keymapp (symbol-value sym)))
          (cl-pushnew (cons sym (symbol-value sym)) maps :key #'car))))
    maps))

(defun keys-live-tests--leader-maps ()
  "Every keymap under the leader, as MAP -> key prefix."
  (let ((found (make-hash-table :test #'eq)))
    (puthash ygg-leader-map [] found)
    (cl-labels ((scan (map prefix)
                  (map-keymap
                   (lambda (event binding)
                     (when-let* ((sub (keys-live-tests--prefix-map
                                       (keys-live-tests--command binding)))
                                 ((not (gethash sub found))))
                       (puthash sub (vconcat prefix (vector event)) found)
                       (scan sub (vconcat prefix (vector event)))))
                   map)))
      (scan ygg-leader-map []))
    found))

(ert-deftest keys-live-leader-and-localleader ()
  (let ((dead (keys-live-tests--dead-under ygg-leader-map "SPC"))
        uninstalled)
    (dolist (state '(ygg-normal-map ygg-visual-map ygg-insert-map))
      (setq dead (nconc dead (keys-live-tests--dead-under
                              (symbol-value state) state nil (list ygg-leader-map)))))
    ;; a mode that is not installed has no buffer to press its keys in
    (maphash (lambda (mode map)
               (if (keys-live-tests--load-mode mode)
                   (setq dead (nconc dead (keys-live-tests--dead-under
                                           map (format "\\ in %s" mode))))
                 (push mode uninstalled)))
             ygg-localleader--maps)
    (message "localleader maps of modes not installed: %S" uninstalled)
    (message "dead leader and localleader keys: %d" (length dead))
    (should-not dead)))

(ert-deftest keys-live-ex-commands ()
  (require 'yggdrasil-ex)
  (let ((dead (seq-keep (lambda (cell)
                          (and (keys-live-tests--dead-p (cdr cell))
                               (format ":%s -> %s" (car cell) (cdr cell))))
                        ygg-ex--commands)))
    (message "dead ex commands: %d" (length dead))
    (should-not dead)))

(ert-deftest keys-live-force-kill-kills-an-ordinary-buffer ()
  (let ((buffer (generate-new-buffer "keys-live-plain")))
    (with-current-buffer buffer
      (insert "unsaved")
      (set-buffer-modified-p t)
      (call-interactively (keys-live-tests--command
                           (lookup-key ygg-leader-map (kbd "b D")))))
    (should-not (buffer-live-p buffer))))

(ert-deftest keys-live-embark-verbs ()
  (should (featurep 'embark))
  (let ((maps (keys-live-tests--embark-maps))
        dead)
    (should maps)
    (dolist (cell maps)
      (setq dead (nconc dead (keys-live-tests--dead-under
                              (cdr cell) (car cell) 'own-only))))
    (message "dead embark verbs: %d" (length dead))
    (should-not dead)))

(ert-deftest keys-live-no-leader-key-bound-twice ()
  (let ((maps (keys-live-tests--leader-maps))
        (bindings (make-hash-table :test #'equal))
        twice)
    (dolist (entry (reverse keys-live-tests--log))
      (pcase-let* ((`(,map ,key ,def ,file) entry)
                   (map (if (eq map 'leader) ygg-leader-map map))
                   (prefix (gethash map maps)))
        (when prefix
          (let* ((full (key-description (vconcat prefix key)))
                 (earlier (gethash full bindings)))
            (when (and earlier (not (equal (car earlier) def)))
              (push (format "SPC %s: %s (%s) then %s (%s)"
                            full (car earlier) (cdr earlier) def file)
                    twice))
            (puthash full (cons def file) bindings)))))
    (message "leader keys bound twice: %d" (length twice))
    (should-not twice)))

(provide 'keys-live-tests)
;;; keys-live-tests.el ends here
