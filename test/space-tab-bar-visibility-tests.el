;;; space-tab-bar-visibility-tests.el --- The space bar hides without turning spaces off -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(eval-and-compile (require 'yggdrasil-spacetree))
(require 'layer-ui)

(defconst space-tab-bar-tests--hooks
  '(tab-bar-tab-post-open-functions tab-bar-tab-pre-close-functions
    tab-bar-tab-post-select-functions tab-bar-tab-post-change-group-functions
    enable-theme-functions disable-theme-functions))

(defconst space-tab-bar-tests--advised
  '((tab-bar-rename-tab ygg-space--format-invalidate ygg-space-tree--queue)
    (tab-bar-move-tab-to ygg-space--format-invalidate)
    (tab-bar-move-tab-to-frame ygg-space--format-invalidate)
    (split-window ygg-space--allow-side-split)
    (tab-bar-select-tab ygg-space--safe-select)))

(defmacro space-tab-bar-tests--with (visible &rest body)
  (declare (indent 1))
  `(let ((ygg-space-tab-bar-visible ,visible)
         (tab-bar-show tab-bar-show)
         (tab-bar-format tab-bar-format)
         (tab-bar-auto-width tab-bar-auto-width)
         (tab-bar-mode tab-bar-mode)
         (default-frame-alist (copy-alist default-frame-alist))
         (added nil))
     (cl-progv space-tab-bar-tests--hooks
         (mapcar #'symbol-value space-tab-bar-tests--hooks)
       (pcase-dolist (`(,fn . ,advices) space-tab-bar-tests--advised)
         (dolist (a advices)
           (unless (advice-member-p a fn) (push (cons fn a) added))))
       (unwind-protect
           (cl-letf (((symbol-function 'ygg-space--ensure-root) #'ignore))
             (ygg-spacetree-setup)
             ,@body)
         (pcase-dolist (`(,fn . ,a) added) (advice-remove fn a))
         (tab-bar-mode -1)))))

(defun space-tab-bar-tests--lines ()
  (frame-parameter nil 'tab-bar-lines))

(ert-deftest space-tab-bar-hidden-by-default-with-the-mode-on ()
  (space-tab-bar-tests--with nil
    (should tab-bar-mode)
    (should (= 0 (space-tab-bar-tests--lines)))))

(ert-deftest space-tab-bar-toggle-shows-then-hides ()
  (space-tab-bar-tests--with nil
    (ygg-space-toggle-tab-bar)
    (should tab-bar-mode)
    (should ygg-space-tab-bar-visible)
    (should (= 1 (space-tab-bar-tests--lines)))
    (ygg-space-toggle-tab-bar)
    (should tab-bar-mode)
    (should (= 0 (space-tab-bar-tests--lines)))))

(ert-deftest space-tab-bar-shown-at-setup-when-the-setting-is-t ()
  (space-tab-bar-tests--with t
    (should tab-bar-mode)
    (should (= 1 (space-tab-bar-tests--lines)))))

(ert-deftest space-tab-bar-customize-set-variable-applies ()
  (space-tab-bar-tests--with nil
    (customize-set-variable 'ygg-space-tab-bar-visible t)
    (should (= 1 (space-tab-bar-tests--lines)))
    (customize-set-variable 'ygg-space-tab-bar-visible nil)
    (should (= 0 (space-tab-bar-tests--lines)))))

(ert-deftest space-tab-bar-setter-is-inert-with-the-mode-off ()
  (let ((ygg-space-tab-bar-visible nil)
        (tab-bar-mode nil)
        (tab-bar-show tab-bar-show))
    (customize-set-variable 'ygg-space-tab-bar-visible t)
    (should ygg-space-tab-bar-visible)
    (should (eq tab-bar-show (default-value 'tab-bar-show)))))

(defun space-tab-bar-tests--round-trip (save-filters restore-filters)
  (setq tab-bar-show t)
  (tab-bar--update-tab-bar-lines t)
  (let ((fs (frameset-save nil :filters save-filters)))
    (setq tab-bar-show nil)
    (tab-bar--update-tab-bar-lines t)
    (frameset-restore fs :reuse-frames t :force-display t
                      :filters restore-filters)
    (space-tab-bar-tests--lines)))

(ert-deftest space-tab-bar-session-filters-keep-tab-bar-lines-out ()
  (require 'layer-sessions)
  (require 'easysession)
  (dolist (var '(easysession--overwrite-frameset-filter-alist
                 easysession--overwrite-frameset-filter-include-geometry-alist))
    (should (eq :never (alist-get 'tab-bar-lines (symbol-value var)))))
  (space-tab-bar-tests--with nil
    (let ((filters (easysession--init-frame-parameters-filters
                    easysession--overwrite-frameset-filter-alist)))
      (should (= 0 (space-tab-bar-tests--round-trip filters filters)))))
  (space-tab-bar-tests--with nil
    (should (= 0 (space-tab-bar-tests--round-trip
                  frameset-persistent-filter-alist
                  (easysession--init-frame-parameters-filters
                   easysession--overwrite-frameset-filter-alist)))))
  (space-tab-bar-tests--with nil
    (should (= 1 (space-tab-bar-tests--round-trip
                  frameset-persistent-filter-alist
                  frameset-persistent-filter-alist)))))

(ert-deftest space-tab-bar-after-load-hook-reapplies-the-setting ()
  (require 'layer-sessions)
  (require 'easysession)
  (should (memq 'ygg-session--keep-tab-bar-setting easysession-after-load-hook))
  (space-tab-bar-tests--with nil
    (setq tab-bar-show t)
    (tab-bar--update-tab-bar-lines t)
    (should (= 1 (space-tab-bar-tests--lines)))
    (ygg-session--keep-tab-bar-setting)
    (should (= 0 (space-tab-bar-tests--lines)))))

(ert-deftest space-tab-bar-install-survives-renamed-easysession-variables ()
  (require 'layer-sessions)
  (require 'easysession)
  (let ((vars '(easysession--overwrite-frameset-filter-alist
                easysession--overwrite-frameset-filter-include-geometry-alist)))
    (dolist (unbound (list (list (car vars)) (cdr vars) vars))
      (let ((saved (mapcar (lambda (v) (cons v (symbol-value v))) vars)))
        (unwind-protect
            (progn
              (remove-hook 'easysession-after-load-hook
                           #'ygg-session--keep-tab-bar-setting)
              (advice-remove 'easysession-switch-to #'ygg-session--load-fast)
              (mapc #'makunbound unbound)
              (should-not (condition-case err
                              (progn (ygg-session--install-easysession) nil)
                            (error err)))
              (should (memq 'ygg-session--keep-tab-bar-setting
                            easysession-after-load-hook))
              (should (advice-member-p #'ygg-session--load-fast
                                       'easysession-switch-to))
              (dolist (v unbound) (should-not (boundp v))))
          (pcase-dolist (`(,v . ,val) saved) (set v val))
          (ygg-session--install-easysession))))))

(ert-deftest space-tab-bar-toggle-is-bound-in-the-ui-map ()
  (should (eq (lookup-key ygg-leader-ui-map (kbd "T"))
              #'ygg-space-toggle-tab-bar)))

(provide 'space-tab-bar-visibility-tests)
;;; space-tab-bar-visibility-tests.el ends here
