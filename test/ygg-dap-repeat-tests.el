;;; ygg-dap-repeat-tests.el --- Sticky debugger stepping -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'ert)
(require 'cl-lib)
(require 'yggdrasil)
(require 'layer-dap)

(defconst ygg-dap-repeat-tests--keys
  '(("c" . ygg-dape-continue)
    ("i" . dape-step-in)
    ("o" . dape-next)
    ("O" . dape-step-out)
    ("R" . dape-restart)))

(ert-deftest ygg-dap-repeat-tests-map-survives-dape-load ()
  (require 'dape)
  (dolist (spec ygg-dap-repeat-tests--keys)
    (should (eq (get (cdr spec) 'repeat-map) 'ygg-dape-step-repeat-map))))

(ert-deftest ygg-dap-repeat-tests-map-binds-leader-letters ()
  (dolist (spec ygg-dap-repeat-tests--keys)
    (should (eq (lookup-key ygg-dape-step-repeat-map (car spec)) (cdr spec)))))

(ert-deftest ygg-dap-repeat-tests-map-matches-leader ()
  (dolist (spec ygg-dap-repeat-tests--keys)
    (should (eq (lookup-key ygg-leader-dape-map (car spec)) (cdr spec)))))

(provide 'ygg-dap-repeat-tests)
;;; ygg-dap-repeat-tests.el ends here
