;;; space-pick-agents-tests.el --- SPC p z lists each zone's agents under it -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (defvar ygg-space-state-functions nil)
  (defvar ygg-space-detail-functions nil)
  (defvar aob-acp-persist-file)
  (setq aob-acp-persist-file (make-temp-file "space-pick-sessions-" nil ".eld")))

(require 'yggdrasil-spacetree)
(require 'layer-aob)

(setq ygg-projects-pins-file nil)

(defconst space-pick-tests--tabs
  '((current-tab (name . "home") (explicit-name . t) (ygg-id . 1) (ygg-dir . "/tmp/home/"))
    (tab (name . "proj") (explicit-name . t) (ygg-id . 2) (ygg-parent . 1) (ygg-dir . "/tmp/proj/"))
    (tab (name . "other") (explicit-name . t) (ygg-id . 3) (ygg-dir . "/tmp/other/"))))

(defmacro space-pick-tests--with (&rest body)
  "Run BODY with the three fake zones, no sessions, no pins, no history."
  (declare (indent 0))
  `(let ((aob--sessions (make-hash-table :test #'equal))
         (aob--order nil)
         (aob-session-created-hook nil)
         (ygg-projects--pin-list nil)
         (ygg-space-pick-rows-functions (list #'ygg-aob--pick-rows))
         (tab-bar-tabs-function (lambda (&optional _) space-pick-tests--tabs)))
     (cl-letf (((symbol-function 'ygg-space-dir)
                (lambda (&optional tab) (alist-get 'ygg-dir tab)))
               ((symbol-function 'aob-acp-resumable-entries) (lambda () nil))
               ((symbol-function 'aob-session-clock) (lambda (&rest _) nil))
               ((symbol-function 'aob-session-spend) (lambda (&rest _) nil)))
       ,@body)))

(defun space-pick-tests--agent (name space &rest refs)
  "A live agent NAME filed under SPACE, with REFS put on it."
  (let ((s (aob-create-session :id (concat "acp:" name) :backend 'acp :name name
                               :project "/tmp/proj/" :dir "/tmp/proj/" :state 'idle)))
    (aob-session-put s :space space)
    (cl-loop for (k v) on refs by #'cddr do (aob-session-put s k v))
    s))

(defun space-pick-tests--labels ()
  (mapcar (lambda (l) (substring-no-properties (car l))) (ygg-space--pick-lines)))

(defun space-pick-tests--index (needle labels)
  (seq-position labels needle (lambda (l n) (string-match-p (regexp-quote n) l))))

(ert-deftest space-pick-agents-under-their-zone-indented ()
  (space-pick-tests--with
    (space-pick-tests--agent "alpha" 2)
    (space-pick-tests--agent "beta" 3)
    (let* ((labels (space-pick-tests--labels))
           (proj (space-pick-tests--index "○ proj" labels))
           (alpha (space-pick-tests--index "alpha" labels))
           (other (space-pick-tests--index "○ other" labels))
           (beta (space-pick-tests--index "beta" labels)))
      (should (equal (list proj alpha other beta) (list 1 2 3 4)))
      (should (string-prefix-p "      alpha" (nth alpha labels)))
      (should (string-prefix-p "    beta" (nth beta labels))))))

(ert-deftest space-pick-agents-no-zone-after-tree ()
  (space-pick-tests--with
    (space-pick-tests--agent "lost" 99)
    (let ((labels (space-pick-tests--labels)))
      (should (equal (nthcdr 3 labels)
                     (list "no zone"
                           (substring-no-properties
                            (concat "  " "  " (ygg-aob--switch-label
                                               (aob-session-get "acp:lost"))))))))))

(ert-deftest space-pick-agents-pick-switches-then-goes ()
  (space-pick-tests--with
    (let ((alpha (space-pick-tests--agent "alpha" 2))
          (calls nil)
          (metadata nil))
      (cl-letf (((symbol-function 'ygg-space--goto-id)
                 (lambda (id) (push (list 'zone id) calls)))
                ((symbol-function 'ygg-aob--goto)
                 (lambda (s) (push (list 'agent s) calls)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (setq metadata (funcall table "" nil 'metadata))
                   (seq-find (lambda (c) (string-match-p "alpha" c))
                             (funcall table "" nil t)))))
        (ygg-space-pick))
      (should (equal metadata '(metadata (display-sort-function . identity))))
      (should (equal (reverse calls) (list '(zone 2) (list 'agent alpha)))))))

(ert-deftest space-pick-agents-no-zone-pick-only-goes ()
  (space-pick-tests--with
    (let ((lost (space-pick-tests--agent "lost" 99))
          (calls nil))
      (cl-letf (((symbol-function 'ygg-space--goto-id)
                 (lambda (id) (push (list 'zone id) calls)))
                ((symbol-function 'ygg-aob--goto)
                 (lambda (s) (push (list 'agent s) calls)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (seq-find (lambda (c) (string-match-p "lost" c))
                             (funcall table "" nil t)))))
        (ygg-space-pick))
      (should (equal calls (list (list 'agent lost)))))))

(ert-deftest space-pick-agents-hidden-and-put-down-left-out ()
  (space-pick-tests--with
    (space-pick-tests--agent "shown" 2)
    (space-pick-tests--agent "secret" 2 :hidden t)
    (space-pick-tests--agent "shelved" 2 :task "/tmp/space-pick-no-such-task-dir")
    (let ((labels (space-pick-tests--labels)))
      (should (space-pick-tests--index "shown" labels))
      (should-not (space-pick-tests--index "secret" labels))
      (should-not (space-pick-tests--index "shelved" labels)))))

(ert-deftest space-pick-agents-colliding-labels-made-unique ()
  (space-pick-tests--with
    (let ((ygg-space-pick-rows-functions
           (list (lambda (id) (when (eql id 2) (list (cons "same" #'ignore)
                                                     (cons "same" #'ignore)))))))
      (let ((labels (space-pick-tests--labels)))
        (should (member "    same" labels))
        (should (member "    same 2" labels))
        (should (equal labels (seq-uniq labels)))))))

(ert-deftest space-pick-agents-no-hook-leaves-picker-as-it-was ()
  (space-pick-tests--with
    (space-pick-tests--agent "alpha" 2)
    (let ((ygg-space-pick-rows-functions nil))
      (should (equal (ygg-space--pick-lines) (ygg-space--tree-lines)))
      (should (equal (mapcar #'car (ygg-space--pick-lines))
                     '("● home" "  ○ proj" "○ other"))))))

(ert-deftest space-pick-agents-subagent-under-its-lead ()
  (space-pick-tests--with
    (let ((lead (space-pick-tests--agent "lead" 2)))
      (space-pick-tests--agent "helper" 2 :parent-session (aob-session-id lead))
      (let* ((labels (space-pick-tests--labels))
             (at (space-pick-tests--index "lead " labels)))
        (should (string-match-p "\\`        helper" (nth (1+ at) labels)))))))

(ert-deftest space-pick-agents-grandchild-a-level-deeper ()
  (space-pick-tests--with
    (let* ((lead (space-pick-tests--agent "lead" 2))
           (kid (space-pick-tests--agent "kid" 2 :parent-session (aob-session-id lead))))
      (space-pick-tests--agent "grandkid" 2 :parent-session (aob-session-id kid))
      (let* ((labels (space-pick-tests--labels))
             (at (space-pick-tests--index "lead " labels)))
        (should (string-match-p "\\`        kid" (nth (1+ at) labels)))
        (should (string-match-p "\\`          grandkid" (nth (+ 2 at) labels)))))))

(ert-deftest space-pick-agents-finished-kid-dimmed-with-its-kids-under-it ()
  (space-pick-tests--with
    (let* ((lead (space-pick-tests--agent "lead" 2))
           (kid (space-pick-tests--agent "kid" 2 :parent-session (aob-session-id lead))))
      (space-pick-tests--agent "grandkid" 2 :parent-session (aob-session-id kid))
      (setf (aob-session-state kid) 'done)
      (let* ((lines (ygg-space--pick-lines))
             (labels (mapcar (lambda (l) (substring-no-properties (car l))) lines))
             (at (space-pick-tests--index " kid " labels)))
        (should (string-match-p "\\`        kid" (nth at labels)))
        (should (eq 'shadow (get-text-property 10 'face (car (nth at lines)))))
        (should (string-match-p "\\`          grandkid" (nth (1+ at) labels)))))))

(ert-deftest space-pick-agents-orphan-goes-top ()
  (space-pick-tests--with
    (space-pick-tests--agent "stray" 2 :parent-session "acp:gone")
    (space-pick-tests--agent "ended" 2 :parent-session "acp:gone")
    (setf (aob-session-state (aob-session-get "acp:ended")) 'done)
    (let ((labels (space-pick-tests--labels)))
      (should (string-match-p "\\`      stray"
                              (nth (space-pick-tests--index "stray" labels) labels)))
      (should-not (space-pick-tests--index "ended" labels)))))

(ert-deftest space-pick-agents-native-and-paired-kids-in-spawn-order ()
  (space-pick-tests--with
    (let ((lead (space-pick-tests--agent "lead" 2)))
      (space-pick-tests--agent "native" 2 :parent-session (aob-session-id lead)
                               :native-tool-id "toolu_1")
      (space-pick-tests--agent "paired" 2 :parent-session (aob-session-id lead)
                               :native-tool-id "sid-2" :announced t :paired-call "toolu_2")
      (let* ((labels (space-pick-tests--labels))
             (at (space-pick-tests--index "lead " labels)))
        (should (string-match-p "\\`        native" (nth (1+ at) labels)))
        (should (string-match-p "\\`        paired" (nth (+ 2 at) labels)))))))

(ert-deftest space-pick-agents-put-down-lead-hides-its-kids ()
  (space-pick-tests--with
    (let ((lead (space-pick-tests--agent "shelved" 2 :task "/tmp/space-pick-no-such-task-dir")))
      (space-pick-tests--agent "busykid" 2 :parent-session (aob-session-id lead))
      (space-pick-tests--agent "donekid" 2 :parent-session (aob-session-id lead))
      (setf (aob-session-state (aob-session-get "acp:donekid")) 'done)
      (let ((labels (space-pick-tests--labels)))
        (should-not (space-pick-tests--index "busykid" labels))
        (should-not (space-pick-tests--index "donekid" labels))))))

(ert-deftest space-pick-agents-kid-row-goes-to-the-kid ()
  (space-pick-tests--with
    (let* ((lead (space-pick-tests--agent "lead" 2))
           (kid (space-pick-tests--agent "helper" 2 :parent-session (aob-session-id lead)))
           (calls nil))
      (setf (aob-session-state kid) 'done)
      (cl-letf (((symbol-function 'ygg-space--goto-id)
                 (lambda (id) (push (list 'zone id) calls)))
                ((symbol-function 'ygg-aob--goto)
                 (lambda (s) (push (list 'agent s) calls)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (seq-find (lambda (c) (string-match-p "helper" c))
                             (funcall table "" nil t)))))
        (ygg-space-pick))
      (should (equal (reverse calls) (list '(zone 2) (list 'agent kid)))))))

(ert-deftest space-pick-agents-zone-list-nests-kids ()
  (space-pick-tests--with
    (let* ((ygg-aob--tree-agents-on t)
           (ygg-space-tree-width 80)
           (lead (space-pick-tests--agent "lead" 2))
           (kid (space-pick-tests--agent "helper" 2 :parent-session (aob-session-id lead))))
      (space-pick-tests--agent "grandkid" 2 :parent-session (aob-session-id kid))
      (space-pick-tests--agent "elsewhere" 3)
      (setf (aob-session-state kid) 'done)
      (cl-letf (((symbol-function 'aob-session-quiet) #'ignore)
                ((symbol-function 'aob-session-ctx) #'ignore))
        (let ((lines (ygg-aob--tree-details 2)))
          (should (equal (mapcar (lambda (l) (get-text-property 0 'aob-session l)) lines)
                         (list "acp:lead" "acp:helper" "acp:grandkid")))
          (should (string-match-p "\\`     ○ helper" (substring-no-properties (nth 1 lines))))
          (should (string-match-p "\\`       ○ grandkid"
                                  (substring-no-properties (nth 2 lines))))
          (should (eq 'shadow (get-text-property 6 'face (nth 1 lines)))))))))

(ert-deftest space-pick-agents-sending-loop-listed-once ()
  (space-pick-tests--with
    (let ((a (space-pick-tests--agent "loopa" 2))
          (b (space-pick-tests--agent "loopb" 2)))
      (aob-session-put a :parent-session (aob-session-id b))
      (aob-session-put b :parent-session (aob-session-id a))
      (let ((labels (space-pick-tests--labels)))
        (should (= 1 (seq-count (lambda (l) (string-match-p "\\` *loopa " l)) labels)))
        (should (= 1 (seq-count (lambda (l) (string-match-p "\\` *loopb " l)) labels)))))))

(ert-deftest space-pick-agents-pinned-first-in-pin-order ()
  (space-pick-tests--with
    (space-pick-tests--agent "busy" 2)
    (let* ((late (space-pick-tests--agent "late" 2))
           (early (space-pick-tests--agent "early" 2))
           (ygg-projects--pin-list (list (aob-session-id early) (aob-session-id late))))
      (setf (aob-session-state (aob-session-get "acp:busy")) 'blocked)
      (let ((labels (space-pick-tests--labels)))
        (should (< (space-pick-tests--index "early" labels)
                   (space-pick-tests--index "late" labels)
                   (space-pick-tests--index "busy" labels)))
        (should (string-prefix-p "    ⊤ early" (nth (space-pick-tests--index "early" labels) labels)))))))

(ert-deftest space-pick-agents-ended-pinned-shown-and-resumed ()
  (space-pick-tests--with
    (let* ((entry (list :acp-id "conv-1" :name "asleep" :dir "/tmp/proj/sub/"
                        :project "/tmp/proj/"))
           (ygg-projects--pin-list (list "conv-1"))
           (resumed nil)
           (switched nil))
      (space-pick-tests--agent "awake" 2)
      (cl-letf (((symbol-function 'aob-acp-resumable-entries) (lambda () (list entry)))
                ((symbol-function 'aob-acp-resume-entry) (lambda (e) (setq resumed e)))
                ((symbol-function 'ygg-space--goto-id) (lambda (id) (setq switched id)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (seq-find (lambda (c) (string-match-p "asleep" c))
                             (funcall table "" nil t)))))
        (let ((labels (space-pick-tests--labels)))
          (should (equal (space-pick-tests--index "asleep" labels) 2))
          (should (string-match-p "⊤ asleep +⟲" (nth 2 labels))))
        (ygg-space-pick))
      (should (eql switched 2))
      (should (eq resumed entry)))))

(ert-deftest space-pick-agents-ended-pinned-with-no-zone ()
  (space-pick-tests--with
    (let ((entry (list :acp-id "conv-2" :name "stray" :dir "/tmp/elsewhere/"))
          (ygg-projects--pin-list (list "conv-2")))
      (cl-letf (((symbol-function 'aob-acp-resumable-entries) (lambda () (list entry))))
        (let ((labels (space-pick-tests--labels)))
          (should (equal (nth 3 labels) "no zone"))
          (should (string-match-p "\\`  ⊤ stray" (nth 4 labels))))))))

(defun space-pick-tests--finished-kids (lead n)
  (dotimes (i n)
    (let ((kid (space-pick-tests--agent (format "done%d" i) 2
                                        :parent-session (aob-session-id lead))))
      (setf (aob-session-state kid) 'done))))

(ert-deftest space-pick-agents-finished-kids-capped-with-fold-row ()
  (space-pick-tests--with
    (let ((ygg-aob--finished-unfolded nil)
          (lead (space-pick-tests--agent "lead" 2)))
      (space-pick-tests--finished-kids lead 8)
      (let* ((labels (space-pick-tests--labels))
             (at (space-pick-tests--index "lead " labels)))
        (should (equal (mapcar (lambda (l) (car (split-string l)))
                               (seq-subseq labels (1+ at) (+ 7 at)))
                       '("done3" "done4" "done5" "done6" "done7" "+3")))
        (should (string-match-p "\\`        \\+3 finished of lead\\'" (nth (+ 6 at) labels)))
        (should-not (space-pick-tests--index "done2" labels))))))

(ert-deftest space-pick-agents-fold-row-unfolds-all ()
  (space-pick-tests--with
    (let ((ygg-aob--finished-unfolded nil)
          (lead (space-pick-tests--agent "lead" 2))
          (prompts 0))
      (space-pick-tests--finished-kids lead 8)
      (cl-letf (((symbol-function 'ygg-space--goto-id) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (cl-incf prompts)
                   (seq-find (lambda (c) (string-match-p (if (= prompts 1) "\\+3 finished" "done0") c))
                             (funcall table "" nil t))))
                ((symbol-function 'ygg-aob--goto) #'ignore))
        (ygg-space-pick))
      (should (= prompts 2))
      (let ((labels (space-pick-tests--labels)))
        (dotimes (i 8)
          (should (space-pick-tests--index (format "done%d " i) labels)))
        (should (space-pick-tests--index "fold finished" labels))
        (should-not (space-pick-tests--index "+3 finished" labels))))))

(ert-deftest space-pick-agents-working-kid-never-folded ()
  (space-pick-tests--with
    (let ((ygg-aob--finished-unfolded nil)
          (lead (space-pick-tests--agent "lead" 2)))
      (space-pick-tests--agent "busy" 2 :parent-session (aob-session-id lead))
      (space-pick-tests--finished-kids lead 8)
      (let ((labels (space-pick-tests--labels)))
        (should (space-pick-tests--index "busy " labels))
        (should (space-pick-tests--index "+3 finished" labels))))))

(ert-deftest space-pick-agents-working-grandkid-keeps-its-chain ()
  (space-pick-tests--with
    (let* ((ygg-aob--finished-unfolded nil)
           (lead (space-pick-tests--agent "lead" 2))
           (old (space-pick-tests--agent "oldkid" 2 :parent-session (aob-session-id lead))))
      (setf (aob-session-state old) 'done)
      (space-pick-tests--agent "busygrandkid" 2 :parent-session (aob-session-id old))
      (space-pick-tests--finished-kids lead 8)
      (let* ((labels (space-pick-tests--labels))
             (at (space-pick-tests--index "oldkid " labels)))
        (should at)
        (should (string-match-p "\\`          busygrandkid" (nth (1+ at) labels)))
        (should (space-pick-tests--index "+3 finished" labels))))))

(ert-deftest space-pick-agents-zone-list-fold-row-toggles ()
  (space-pick-tests--with
    (let ((ygg-aob--finished-unfolded nil)
          (ygg-aob--tree-agents-on t)
          (ygg-space-tree-width 80)
          (lead (space-pick-tests--agent "lead" 2)))
      (space-pick-tests--finished-kids lead 8)
      (cl-letf (((symbol-function 'aob-session-quiet) #'ignore)
                ((symbol-function 'aob-session-ctx) #'ignore)
                ((symbol-function 'ygg-space-tree--queue) #'ignore))
        (let ((fold (car (last (ygg-aob--tree-details 2)))))
          (should (= 7 (length (ygg-aob--tree-details 2))))
          (should (equal "     +3 finished" (substring-no-properties fold)))
          (with-temp-buffer
            (insert fold)
            (goto-char (point-min))
            (should (eq (key-binding (kbd "RET")) #'ygg-aob-tree-toggle-finished))
            (ygg-aob-tree-toggle-finished))
          (should (= 10 (length (ygg-aob--tree-details 2))))
          (should (equal "     − fold finished"
                         (substring-no-properties (car (last (ygg-aob--tree-details 2)))))))))))

(ert-deftest space-pick-subagent-answers-with-its-leads-space ()
  (space-pick-tests--with
    (let* ((lead (space-pick-tests--agent "lead" 2))
           (kid (space-pick-tests--agent "kid" 3 :parent-session "acp:lead"))
           (grandkid (space-pick-tests--agent "grandkid" 99 :parent-session "acp:kid")))
      (aob-set-state kid 'blocked)
      (should (eql 2 (ygg-aob-session-space kid)))
      (should (eql 2 (ygg-aob-session-space grandkid)))
      (should (eql 2 (aob-session-ref grandkid :space)))
      (should (eq 'error (ygg-aob--space-state-face 2)))
      (should-not (ygg-aob--space-state-face 3))
      (ignore lead))))

(provide 'space-pick-agents-tests)
;;; space-pick-agents-tests.el ends here
