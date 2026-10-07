;;; ygg-plan-tests.el --- show-me plans answered in place -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ygg-plan)
(require 'aob)

(defconst ygg-plan-tests--plan
  (concat
   "---\nplan: scheduled-send\nstatus: open\n---\n"
   "# Scheduled Send in PostBox\n\n"
   "> add send later to the composer… it must never go out early\n\n"
   "## 1. A user can pick a send time in the composer.\n\n"
   "```tsx mock\n<Composer>\n  <SendButton menu=\"Send later…\" />\n</Composer>\n```\n\n"
   "Q: How many scheduled messages per user?\n"
   "a) 50 - enough for a week, no paging\n"
   "b) 500 - needs paging in the Scheduled folder\n"
   "pick: a - nobody asked for more\n\n"
   "### 1.1 createScheduled() refuses past times.\n\n"
   "```text calls\ncreateScheduled(msg, at)\n  assertFuture(at)\n```\n\n"
   "Q: Where does the check live?\n"
   "a) store - one place\n"
   "b) route - fails earlier\n"
   "pick: a - one place\n\n"
   "#### 1.1.1 `server/src/scheduled/store.ts:40 · createScheduled`\n\n"
   "```ts src=\"server/src/scheduled/store.ts\" lines=\"2-3\"\n```\n\n"
   "## 2. A user can cancel a scheduled message.\n\n"
   "Q: Keep cancelled rows?\n"
   "a) yes - audit trail\n"
   "b) no - claim 2.1 goes\n"
   "pick: a - audit trail\n\n"
   "### 2.1 runScheduledSends() claims retries.\n\n"
   "## Not changing\n\n- normal send\n\n"
   "## Done\n\n```sh\nnpm test -- scheduled\n```\n"))

(defmacro ygg-plan-tests--with-plan (&rest body)
  "Run BODY in a plan buffer for a file under a temp repo, bound as `root'."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "ygg-plan" t)))
          (file (expand-file-name ".aob/plans/scheduled-send.md" root)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory file) t)
           (make-directory (expand-file-name "server/src/scheduled" root) t)
           (write-region "one\ntwo\nthree\nfour\n" nil
                         (expand-file-name "server/src/scheduled/store.ts" root))
           (write-region ygg-plan-tests--plan nil file)
           (let ((buffer (find-file-noselect file)))
             (unwind-protect
                 (with-current-buffer buffer ,@body)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer (set-buffer-modified-p nil))
                 (kill-buffer buffer)))))
       (delete-directory root t))))

(defun ygg-plan-tests--goto (text)
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(ert-deftest ygg-plan/mode-is-automatic-for-plans ()
  (ygg-plan-tests--with-plan
    (should (eq major-mode 'ygg-plan-mode))
    (should (derived-mode-p 'markdown-mode)))
  (should (eq (cdr (assoc "/proj/.aob/plans/x.md" auto-mode-alist #'string-match-p))
              'ygg-plan-mode))
  (should-not (eq (cdr (assoc "/proj/docs/x.md" auto-mode-alist #'string-match-p))
                  'ygg-plan-mode)))

(ert-deftest ygg-plan/scan-reads-claims-and-decisions ()
  (ygg-plan-tests--with-plan
    (let* ((model (ygg-plan--model))
           (claims (plist-get model :claims))
           (decisions (plist-get model :decisions)))
      (should (equal (plist-get model :title) "Scheduled Send in PostBox"))
      (should (equal (mapcar (lambda (c) (plist-get c :no)) claims)
                     '("1" "1.1" "1.1.1" "2" "2.1" nil nil)))
      (should (equal (plist-get (nth 1 claims) :text) "createScheduled() refuses past times."))
      (should (equal (mapcar (lambda (d) (list (plist-get d :claim) (plist-get d :pick)
                                               (length (plist-get d :options))))
                             decisions)
                     '(("1" "a" 2) ("1.1" "a" 2) ("2" "a" 2))))
      (should (equal (plist-get (car decisions) :question)
                     "How many scheduled messages per user?")))))

(ert-deftest ygg-plan/opens-on-level-one-claims ()
  (ygg-plan-tests--with-plan
    (dolist (shown '("# Scheduled Send" "## 1. A user" "## 2. A user" "## Done"))
      (ygg-plan-tests--goto shown)
      (should-not (invisible-p (point))))
    (dolist (hidden '("### 1.1 createScheduled" "#### 1.1.1" "### 2.1 runScheduled"
                      "Q: How many"))
      (ygg-plan-tests--goto hidden)
      (should (invisible-p (point))))))

(ert-deftest ygg-plan/tab-opens-a-level-at-a-time ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "## 1. A user")
    (ygg-plan-tab)
    (ygg-plan-tests--goto "Q: How many")
    (should-not (invisible-p (point)))
    (ygg-plan-tests--goto "### 1.1 createScheduled")
    (should-not (invisible-p (point)))
    (ygg-plan-tests--goto "Q: Where does")
    (should (invisible-p (point)))
    (ygg-plan-tests--goto "## 1. A user")
    (ygg-plan-tab)
    (ygg-plan-tests--goto "Q: Where does")
    (should-not (invisible-p (point)))
    (ygg-plan-tests--goto "#### 1.1.1")
    (should-not (invisible-p (point)))
    (ygg-plan-tests--goto "## 1. A user")
    (ygg-plan-tab)
    (ygg-plan-tests--goto "### 1.1 createScheduled")
    (should (invisible-p (point)))
    (ygg-plan-cycle)
    (ygg-plan-tests--goto "### 1.1 createScheduled")
    (should-not (invisible-p (point)))
    (ygg-plan-tests--goto "#### 1.1.1")
    (should (invisible-p (point)))
    (ygg-plan-cycle)
    (ygg-plan-tests--goto "#### 1.1.1")
    (should-not (invisible-p (point)))
    (ygg-plan-cycle)
    (ygg-plan-tests--goto "Q: How many")
    (should-not (invisible-p (point)))
    (ygg-plan-cycle)
    (ygg-plan-tests--goto "### 1.1 createScheduled")
    (should (invisible-p (point)))))

(ert-deftest ygg-plan/response-for-a-pick-and-an-unopened-decision ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (ygg-plan-tests--goto "Q: Where does")
    (ygg-plan--track)
    (should (equal (ygg-plan-response)
                   (concat "# Re: Scheduled Send in PostBox\n"
                           "## Decisions\n"
                           "1. [1] How many scheduled messages per user?\n"
                           "   → b) 500 (was: a)\n"
                           "2. [1.1] Where does the check live?  _(not opened; default kept)_\n"
                           "3. [2] Keep cancelled rows?  _(not opened; default kept)_\n")))))

(ert-deftest ygg-plan/opened-decision-is-kept-as-proposed ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "## 2. A user")
    (ygg-plan-tab)
    (ygg-plan-tests--goto "Q: Keep cancelled")
    (ygg-plan--track)
    (should (string-match-p "3\\. \\[2\\] Keep cancelled rows\\?  _(kept as proposed)_"
                            (ygg-plan-response)))
    (should (string-match-p "_(not opened; default kept)_" (ygg-plan-response)))))

(ert-deftest ygg-plan/picking-again-takes-the-pick-back ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (ygg-plan-ret)
    (should-not ygg-plan--picks)
    (ygg-plan-tests--goto "pick: a - nobody")
    (ygg-plan-ret)
    (should (equal (cadar ygg-plan--picks) "a"))))

(ert-deftest ygg-plan/strikes-and-comments-reach-the-response ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "### 2.1 runScheduledSends")
    (ygg-plan-strike)
    (ygg-plan-tests--goto "### 1.1 createScheduled")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "what about time zones?")))
      (ygg-plan-comment))
    (let ((text (ygg-plan-response)))
      (should (string-match-p "## Struck\n- \\[2\\.1\\] runScheduledSends() claims retries\\.\n" text))
      (should (string-match-p
               "## Comments\n- \\[1\\.1\\] createScheduled() refuses past times\\.\n  > what about time zones\\?\n"
               text)))
    (should (seq-some (lambda (ov) (overlay-get ov 'after-string))
                      (overlays-in (point-min) (point-max))))
    (ygg-plan-tests--goto "### 2.1 runScheduledSends")
    (ygg-plan-strike)
    (should-not (string-match-p "## Struck" (ygg-plan-response)))))

(ert-deftest ygg-plan/answers-survive-closing-the-buffer ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (let ((side (ygg-plan--answers-file)))
      (should (file-exists-p side))
      (set-buffer-modified-p nil)
      (kill-buffer)
      (let ((again (find-file-noselect file)))
        (unwind-protect
            (with-current-buffer again
              (should (equal (cadar ygg-plan--picks) "b"))
              (should (string-match-p "→ b) 500" (ygg-plan-response))))
          (kill-buffer again))))))

(ert-deftest ygg-plan/plan-text-is-never-touched-by-answers ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (ygg-plan-accept-all)
    (should-not (buffer-modified-p))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   ygg-plan-tests--plan))))

(ert-deftest ygg-plan/decisions-show-the-suggestion-and-the-pick ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (let ((tags (mapcar (lambda (ov) (substring-no-properties (overlay-get ov 'after-string)))
                        (seq-filter (lambda (ov) (overlay-get ov 'after-string))
                                    (overlays-in (point-min) (point-max))))))
      (should (member "  suggested" tags))
      (should (member "  ✓" tags)))))

(ert-deftest ygg-plan/src-fence-shows-the-referenced-lines ()
  (ygg-plan-tests--with-plan
    (let ((shown (seq-some (lambda (ov)
                             (let ((s (overlay-get ov 'before-string)))
                               (and (stringp s) (string-match-p "two" s) s)))
                           (overlays-in (point-min) (point-max)))))
      (should shown)
      (should (equal (substring-no-properties shown) "two\nthree\n"))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     ygg-plan-tests--plan)))))

(ert-deftest ygg-plan/src-fence-keeps-to-the-project ()
  (ygg-plan-tests--with-plan
    (should (string-match-p "nothing to show"
                            (ygg-plan--source root "../outside.txt" "1-2" "ts")))))

(ert-deftest ygg-plan/send-goes-to-the-session-in-the-project ()
  (ygg-plan-tests--with-plan
    (let (sent)
      (cl-letf (((symbol-function 'ygg-plan--session) (lambda () 'session))
                ((symbol-function 'aob-prompt) (lambda (s text &rest _) (setq sent (list s text))))
                ((symbol-function 'aob-session-name) (lambda (_) "agent")))
        (ygg-plan-tests--goto "b) 500")
        (ygg-plan-ret)
        (ygg-plan-send))
      (should (eq (car sent) 'session))
      (should (string-prefix-p "# Re: Scheduled Send in PostBox\n" (cadr sent))))))

(ert-deftest ygg-plan/session-prefers-the-one-whose-dir-holds-the-plan ()
  (ygg-plan-tests--with-plan
    (let ((here (aob-session--create :id "a" :dir root))
          (elsewhere (aob-session--create :id "b" :dir "/nonexistent/")))
      (cl-letf (((symbol-function 'aob-live-sessions) (lambda () (list elsewhere here)))
                ((symbol-function 'aob-read-session) (lambda (&rest _) (error "asked"))))
        (should (eq (ygg-plan--session) here))))))

(ert-deftest ygg-plan/tab-on-a-mermaid-fence-draws-it ()
  (ygg-plan-tests--with-plan
    (let (drawn)
      (goto-char (point-max))
      (insert "\n```mermaid\nstateDiagram-v2\n  [*] --> A\n```\n")
      (ygg-plan-tests--goto "[*] --> A")
      (cl-letf (((symbol-function 'ygg-diagram-toggle-at-point) (lambda () (setq drawn t))))
        (ygg-plan-tab))
      (should drawn))))

(ert-deftest ygg-plan/keys-live-on-the-localleader ()
  (should (eq (lookup-key (ygg-localleader--get-map 'ygg-plan-mode) "s") #'ygg-plan-send))
  (should (commandp #'ygg-plan-copy)))

(defun ygg-plan-tests--replace (from to)
  (goto-char (point-min))
  (search-forward from)
  (replace-match to t t))

(defun ygg-plan-tests--reopen (text)
  "Kill the plan buffer, write TEXT as the plan file, and return a fresh buffer."
  (let ((file buffer-file-name))
    (set-buffer-modified-p nil)
    (kill-buffer)
    (write-region text nil file)
    (find-file-noselect file)))

(ert-deftest ygg-plan/src-naming-a-directory-shows-nothing ()
  (ygg-plan-tests--with-plan
    (should (string-match-p "nothing to show"
                            (ygg-plan--source root "server/src/scheduled" "1-2" "ts")))))

(ert-deftest ygg-plan/a-failing-draw-never-blocks-saving-answers ()
  (ygg-plan-tests--with-plan
    (cl-letf (((symbol-function 'ygg-plan--draw-claim) (lambda (_) (error "boom"))))
      (ygg-plan-tests--goto "b) 500")
      (ygg-plan-ret))
    (should (file-exists-p (ygg-plan--answers-file)))
    (should (equal (cadar ygg-plan--picks) "b"))))

(ert-deftest ygg-plan/stale-pick-is-dropped-without-signalling ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (ygg-plan-tests--replace "b) 500 - needs paging in the Scheduled folder\n" "")
    (should-not (string-match-p "→" (ygg-plan-response)))
    (ygg-plan--refresh)
    (should-not ygg-plan--picks)
    (ygg-plan-copy)))

(ert-deftest ygg-plan/pick-is-dropped-when-its-option-text-changed ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (should (string-match-p "→ b) 500" (ygg-plan-response)))
    (ygg-plan-tests--replace "b) 500 -" "b) 900 -")
    (should-not (string-match-p "→" (ygg-plan-response)))
    (ygg-plan--refresh)
    (should-not ygg-plan--picks)))

(ert-deftest ygg-plan/strike-does-not-follow-a-renumbered-claim ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "### 2.1 runScheduledSends")
    (ygg-plan-strike)
    (should (string-match-p "## Struck" (ygg-plan-response)))
    (ygg-plan-tests--replace "### 2.1 runScheduledSends() claims retries." "### 2.1 something else entirely.")
    (should-not (string-match-p "## Struck" (ygg-plan-response)))
    (ygg-plan--refresh)
    (should-not ygg-plan--struck)
    (should-not (file-exists-p (ygg-plan--answers-file)))))

(ert-deftest ygg-plan/comment-does-not-follow-a-renumbered-claim ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "### 1.1 createScheduled")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "note")))
      (ygg-plan-comment))
    (ygg-plan-tests--replace "### 1.1 createScheduled()" "### 1.2 createScheduled()")
    (should-not (string-match-p "## Comments" (ygg-plan-response)))
    (ygg-plan--refresh)
    (should-not ygg-plan--comments)))

(ert-deftest ygg-plan/answers-reset-when-the-plan-is-answered ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (let ((side (ygg-plan--answers-file)))
      (should (file-exists-p side))
      (let ((again (ygg-plan-tests--reopen
                    (string-replace "status: open" "status: answered" ygg-plan-tests--plan))))
        (unwind-protect
            (with-current-buffer again
              (should-not ygg-plan--picks)
              (should-not (file-exists-p side)))
          (kill-buffer again))))))

(ert-deftest ygg-plan/answers-reset-when-the-slug-is-reused ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "b) 500")
    (ygg-plan-ret)
    (let ((side (ygg-plan--answers-file))
          (again (ygg-plan-tests--reopen
                  "---\nstatus: open\n---\n# Other\n\n## 1. Something else.\n")))
      (unwind-protect
          (with-current-buffer again
            (should-not ygg-plan--picks)
            (should-not ygg-plan--opened)
            (should-not (file-exists-p side)))
        (kill-buffer again)))))

(ert-deftest ygg-plan/tab-before-the-first-heading-does-nothing ()
  (ygg-plan-tests--with-plan
    (goto-char (point-min))
    (ygg-plan-tab)
    (should (= (point) (point-min)))))

(ert-deftest ygg-plan/bad-line-ranges-show-nothing ()
  (ygg-plan-tests--with-plan
    (dolist (lines '("4-2" "0" "0-2"))
      (should (string-match-p "nothing to show"
                              (ygg-plan--source root "server/src/scheduled/store.ts" lines "ts"))))
    (should (equal (substring-no-properties
                    (ygg-plan--source root "server/src/scheduled/store.ts" "3-" "ts"))
                   "three\nfour\n"))))

(ert-deftest ygg-plan/answers-are-written-by-renaming-a-sibling-file ()
  (ygg-plan-tests--with-plan
    (let (moves (real (symbol-function 'rename-file)))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional ok)
                   (push (list from to ok) moves)
                   (funcall real from to ok))))
        (ygg-plan-tests--goto "b) 500")
        (ygg-plan-ret))
      (let ((move (car moves)))
        (should (equal (cadr move) (ygg-plan--answers-file)))
        (should-not (equal (car move) (cadr move)))
        (should (equal (file-name-directory (car move)) (file-name-directory (cadr move))))
        (should (nth 2 move)))
      (should (file-exists-p (ygg-plan--answers-file))))))

(ert-deftest ygg-plan/session-in-an-ancestor-dir-is-not-the-plans-home ()
  (ygg-plan-tests--with-plan
    (let ((here (aob-session--create :id "a" :dir root))
          (above (aob-session--create :id "c"
                                      :dir (file-name-directory (directory-file-name root)))))
      (cl-letf (((symbol-function 'aob-live-sessions) (lambda () (list above here)))
                ((symbol-function 'aob-read-session) (lambda (&rest _) (error "asked"))))
        (should (eq (ygg-plan--session) here))))))

(ert-deftest ygg-plan/ret-off-an-option-follows-a-link-else-moves-on ()
  (ygg-plan-tests--with-plan
    (let (followed)
      (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
                 (lambda (&rest _) (setq followed t))))
        (ygg-plan-tests--goto "> add send later")
        (ygg-plan-ret))
      (should followed)
      (let ((line (line-number-at-pos)))
        (cl-letf (((symbol-function 'markdown-follow-thing-at-point)
                   (lambda (&rest _) (user-error "nothing"))))
          (ygg-plan-ret))
        (should (= (line-number-at-pos) (1+ line)))))))

(ert-deftest ygg-plan/each-comment-on-a-claim-is-its-own-bullet ()
  (ygg-plan-tests--with-plan
    (ygg-plan-tests--goto "### 1.1 createScheduled")
    (dolist (note '("first" "second"))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) note)))
        (ygg-plan-comment)))
    (should (string-match-p
             (concat "## Comments\n"
                     "- \\[1\\.1\\] createScheduled() refuses past times\\.\n  > first\n"
                     "- \\[1\\.1\\] createScheduled() refuses past times\\.\n  > second\n")
             (ygg-plan-response)))))

(provide 'ygg-plan-tests)
;;; ygg-plan-tests.el ends here
