;;; show-me-plan-lint-tests.el --- plan-lint accepts the spec's plan and names each way it breaks -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst show-me-plan-lint-tests--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defconst show-me-plan-lint-tests--valid
  "---
plan: scheduled-send
status: open
---
# Scheduled Send in PostBox

> add send later to the composer

## 1. A user can pick a send time in the composer.

```tsx mock
<Composer />
```

Q: How many scheduled messages per user?
a) 50 - enough for a week, no paging
b) 500 - needs paging in the Scheduled folder
pick: a - nobody asked for more

### 1.1 createScheduled() refuses past times.

```text calls
createScheduled(msg, at)
```

#### 1.1.1 `server/src/store.ts:40 · createScheduled`

```ts src=\"server/src/store.ts\" lines=\"40-58\"
```

## Shared

```sql
create table scheduled_messages (a int);
```

## Not changing

- normal send

## Done

```sh
npm test
```
")

(defun show-me-plan-lint-tests--with-repo (fn)
  "Call FN with a fake repo's root, its plan path and the name of a sibling dir outside it."
  (let* ((dir (make-temp-file "plan-lint" t))
         (outside (make-temp-file "plan-lint-out" t))
         (store (expand-file-name "server/src/store.ts" dir))
         (plan (expand-file-name ".aob/plans/p.md" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory store) t)
          (make-directory (file-name-directory plan) t)
          (with-temp-file store
            (dotimes (i 80) (insert (format "line %d\n" (1+ i)))))
          (with-temp-file (expand-file-name "secret.ts" outside)
            (dotimes (i 100) (insert (format "secret %d\n" (1+ i)))))
          (make-symbolic-link (expand-file-name "secret.ts" outside)
                              (expand-file-name "server/src/link.ts" dir))
          (funcall fn dir plan (file-name-nondirectory outside)))
      (delete-directory dir t)
      (delete-directory outside t))))

(defun show-me-plan-lint-tests--call (plan dir)
  "Lint PLAN under DIR; return (EXIT STDOUT STDERR)."
  (let ((err (make-temp-file "plan-lint-err")))
    (unwind-protect
        (with-temp-buffer
          (let ((code (call-process
                       (expand-file-name "skills/show-me/scripts/plan-lint"
                                         show-me-plan-lint-tests--root)
                       nil (list t err) nil plan "--root" dir)))
            (list code (buffer-string)
                  (with-temp-buffer (insert-file-contents err) (buffer-string)))))
      (delete-file err))))

(defun show-me-plan-lint-tests--run (text)
  "Lint TEXT in a fake repo; return (EXIT . OUTPUT).
@OUTSIDE@ in TEXT names a sibling directory of the repo."
  (show-me-plan-lint-tests--with-repo
   (lambda (dir plan outside)
     (let ((coding-system-for-write 'utf-8-unix))
       (with-temp-file plan (insert (string-replace "@OUTSIDE@" outside text))))
     (let ((r (show-me-plan-lint-tests--call plan dir)))
       (cons (car r) (cadr r))))))

(defun show-me-plan-lint-tests--edit (from to)
  (let ((text show-me-plan-lint-tests--valid))
    (should (string-search from text))
    (string-replace from to text)))

(defmacro show-me-plan-lint-tests--rejects (name text pattern)
  `(ert-deftest ,(intern (concat "show-me-plan-lint-rejects-" name)) ()
     (let ((result (show-me-plan-lint-tests--run ,text)))
       (should (= 1 (car result)))
       (should (string-match-p ,pattern (cdr result))))))

(ert-deftest show-me-plan-lint-accepts-the-valid-plan ()
  (let ((result (show-me-plan-lint-tests--run show-me-plan-lint-tests--valid)))
    (should (= 0 (car result)))
    (should (string-match-p "0 errors, 0 warnings" (cdr result)))))

(show-me-plan-lint-tests--rejects
 "no-title"
 (show-me-plan-lint-tests--edit "# Scheduled Send in PostBox\n" "")
 "error: first content must be")

(show-me-plan-lint-tests--rejects
 "text-above-title"
 (show-me-plan-lint-tests--edit "# Scheduled" "intro\n\n# Scheduled")
 "error: first content must be")

(show-me-plan-lint-tests--rejects
 "short-title"
 (show-me-plan-lint-tests--edit "# Scheduled Send in PostBox" "# Scheduled Send")
 "title is 2 words")

(show-me-plan-lint-tests--rejects
 "long-title"
 (show-me-plan-lint-tests--edit "# Scheduled Send in PostBox" "# One two three four five six seven eight")
 "title is 8 words")

(show-me-plan-lint-tests--rejects
 "no-quote"
 (show-me-plan-lint-tests--edit "> add send later to the composer\n" "")
 "title must be followed by a `>` quote")

(show-me-plan-lint-tests--rejects
 "unnumbered-claim"
 (show-me-plan-lint-tests--edit "### 1.1 createScheduled" "### createScheduled")
 "must be numbered")

(show-me-plan-lint-tests--rejects
 "wrong-number-depth"
 (show-me-plan-lint-tests--edit "### 1.1 createScheduled" "### 1 createScheduled")
 "does not fit")

(show-me-plan-lint-tests--rejects
 "level-skip"
 (show-me-plan-lint-tests--edit "### 1.1 createScheduled() refuses past times.\n\n```text calls\ncreateScheduled(msg, at)\n```\n\n#### 1.1.1"
                                "#### 1.1.1")
 "skips a level")

(show-me-plan-lint-tests--rejects
 "six-children"
 (show-me-plan-lint-tests--edit
  "## Shared"
  (concat (mapconcat (lambda (n) (format "### 1.%d Child %d works.\n\n```text\nx\n```\n\n" n n))
                     '(2 3 4 5 6) "")
          "## Shared"))
 "more than 5 children")

(show-me-plan-lint-tests--rejects
 "four-claim-levels"
 (show-me-plan-lint-tests--edit "```sql" "##### 1.1.1.1 `a:1 · b`\n\n```text\nx\n```\n\n```sql")
 "at most 3 levels")

(show-me-plan-lint-tests--rejects
 "claim-without-fence"
 (show-me-plan-lint-tests--edit "```text calls\ncreateScheduled(msg, at)\n```\n\n" "")
 "has 0 fences")

(show-me-plan-lint-tests--rejects
 "claim-with-two-fences"
 (show-me-plan-lint-tests--edit "```text calls\ncreateScheduled(msg, at)\n```" "```text\na\n```\n\n```text\nb\n```")
 "has 2 fences")

(show-me-plan-lint-tests--rejects
 "long-claim"
 (show-me-plan-lint-tests--edit "A user can pick a send time in the composer."
                                "A user can pick a send time in the composer of the app every day.")
 "at most 12")

(show-me-plan-lint-tests--rejects
 "bad-where-heading"
 (show-me-plan-lint-tests--edit "`server/src/store.ts:40 · createScheduled`" "the store")
 "must be `path:line")

(show-me-plan-lint-tests--rejects
 "missing-path"
 (show-me-plan-lint-tests--edit "`server/src/store.ts:40 · createScheduled`" "`server/src/nope.ts:40 · x`")
 "does not exist")

(show-me-plan-lint-tests--rejects
 "line-beyond-file"
 (show-me-plan-lint-tests--edit "`server/src/store.ts:40 · createScheduled`" "`server/src/store.ts:81 · x`")
 "beyond the file's 80 lines")

(show-me-plan-lint-tests--rejects
 "src-missing"
 (show-me-plan-lint-tests--edit "src=\"server/src/store.ts\"" "src=\"server/src/gone.ts\"")
 "src server/src/gone.ts does not exist")

(show-me-plan-lint-tests--rejects
 "src-lines-out-of-range"
 (show-me-plan-lint-tests--edit "lines=\"40-58\"" "lines=\"70-99\"")
 "is outside")

(show-me-plan-lint-tests--rejects
 "review-one-option"
 (show-me-plan-lint-tests--edit "b) 500 - needs paging in the Scheduled folder\n" "")
 "1 options")

(show-me-plan-lint-tests--rejects
 "review-no-pick"
 (show-me-plan-lint-tests--edit "pick: a - nobody asked for more\n" "")
 "must end with `pick:")

(show-me-plan-lint-tests--rejects
 "review-pick-names-no-option"
 (show-me-plan-lint-tests--edit "pick: a -" "pick: c -")
 "pick c is not an option")

(show-me-plan-lint-tests--rejects
 "review-option-without-tradeoff"
 (show-me-plan-lint-tests--edit "a) 50 - enough for a week, no paging" "a) 50")
 "must be 2 or 3")

(show-me-plan-lint-tests--rejects
 "review-before-exhibit"
 (show-me-plan-lint-tests--edit
  "```tsx mock\n<Composer />\n```\n\nQ: How many scheduled messages per user?\na) 50 - enough for a week, no paging\nb) 500 - needs paging in the Scheduled folder\npick: a - nobody asked for more\n"
  "Q: How many scheduled messages per user?\na) 50 - enough for a week, no paging\nb) 500 - needs paging in the Scheduled folder\npick: a - nobody asked for more\n\n```tsx mock\n<Composer />\n```\n")
 "must come after the claim's exhibit")

(show-me-plan-lint-tests--rejects
 "review-outside-a-claim"
 (show-me-plan-lint-tests--edit "- normal send" "Q: Why?\na) x - y\nb) x - y\npick: a - z")
 "must sit on a claim")

(show-me-plan-lint-tests--rejects
 "no-claims"
 (concat "# Scheduled Send in PostBox\n\n> words\n\n## Not changing\n\n- a\n\n## Done\n\n```sh\nx\n```\n")
 "plan has no claims")

(show-me-plan-lint-tests--rejects
 "missing-not-changing"
 (show-me-plan-lint-tests--edit "## Not changing\n\n- normal send\n\n" "")
 "missing `## Not changing`")

(show-me-plan-lint-tests--rejects
 "missing-done"
 (show-me-plan-lint-tests--edit "## Done\n\n```sh\nnpm test\n```\n" "")
 "missing `## Done`")

(show-me-plan-lint-tests--rejects
 "done-without-fence"
 (show-me-plan-lint-tests--edit "```sh\nnpm test\n```\n" "run the tests\n")
 "Done must hold a runnable fence")

(ert-deftest show-me-plan-lint-rejects-more-than-five-questions ()
  (let* ((block "\nQ: Another fork?\na) x - y\nb) z - w\npick: a - why\n")
         (result (show-me-plan-lint-tests--run
                  (show-me-plan-lint-tests--edit
                   "### 1.1 createScheduled() refuses past times.\n\n```text calls\ncreateScheduled(msg, at)\n```\n"
                   (concat "### 1.1 createScheduled() refuses past times.\n\n```text calls\ncreateScheduled(msg, at)\n```\n"
                           (mapconcat #'identity (make-list 5 block) ""))))))
    (should (= 1 (car result)))
    (should (string-match-p "error: 6 questions; at most 5" (cdr result)))))

(defconst show-me-plan-lint-tests--claim "## %d. Another claim holds.\n\n```text\nx\n```\n\n")

(defun show-me-plan-lint-tests--claims (&rest numbers)
  (show-me-plan-lint-tests--edit
   "## Shared"
   (concat (mapconcat (lambda (n) (format show-me-plan-lint-tests--claim n)) numbers "") "## Shared")))

(defun show-me-plan-lint-tests--q (text)
  (show-me-plan-lint-tests--edit "Q: How many scheduled messages per user?" (concat "Q: " text)))

(ert-deftest show-me-plan-lint-warns-without-questions ()
  (let ((result (show-me-plan-lint-tests--run
                 (show-me-plan-lint-tests--edit
                  "Q: How many scheduled messages per user?\na) 50 - enough for a week, no paging\nb) 500 - needs paging in the Scheduled folder\npick: a - nobody asked for more\n\n" ""))))
    (should (= 0 (car result)))
    (should (string-match-p "warning: plan has no Review block" (cdr result)))))

(show-me-plan-lint-tests--rejects
 "sixteen-word-question"
 (show-me-plan-lint-tests--q "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen")
 "error: question must be 1 to 15 words")

(show-me-plan-lint-tests--rejects
 "empty-question"
 (show-me-plan-lint-tests--q "")
 "error: question must be 1 to 15 words")

(ert-deftest show-me-plan-lint-accepts-a-fifteen-word-question ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (show-me-plan-lint-tests--q "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen"))))))

(ert-deftest show-me-plan-lint-accepts-five-level-one-claims ()
  (should (= 0 (car (show-me-plan-lint-tests--run (show-me-plan-lint-tests--claims 2 3 4 5))))))

(show-me-plan-lint-tests--rejects
 "six-level-one-claims"
 (show-me-plan-lint-tests--claims 2 3 4 5 6)
 "more than 5 level-1 claims")

(show-me-plan-lint-tests--rejects
 "repeated-level-one-number"
 (show-me-plan-lint-tests--claims 1)
 "number 1 is out of order; expected 2")

(show-me-plan-lint-tests--rejects
 "level-one-gap"
 (show-me-plan-lint-tests--claims 3)
 "number 3 is out of order; expected 2")

(show-me-plan-lint-tests--rejects
 "repeated-child-number"
 (show-me-plan-lint-tests--edit "#### 1.1.1" "### 1.1 Twice over holds.\n\n```text\nx\n```\n\n#### 1.1.1")
 "number 1.1 is out of order; expected 1.2")

(show-me-plan-lint-tests--rejects
 "grandchild-gap"
 (show-me-plan-lint-tests--edit "#### 1.1.1" "#### 1.1.2")
 "number 1.1.2 is out of order; expected 1.1.1")

(ert-deftest show-me-plan-lint-numbering-restarts-under-each-parent ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (show-me-plan-lint-tests--edit
                      "## Shared"
                      "## 2. Second claim holds.\n\n```text\nx\n```\n\n### 2.1 Child holds.\n\n```text\nx\n```\n\n## Shared"))))))

(ert-deftest show-me-plan-lint-accepts-single-quoted-src-and-lines ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (show-me-plan-lint-tests--edit
                      "src=\"server/src/store.ts\" lines=\"40-58\""
                      "src='server/src/store.ts' lines='40-58'"))))))

(show-me-plan-lint-tests--rejects
 "single-quoted-src-missing"
 (show-me-plan-lint-tests--edit "src=\"server/src/store.ts\"" "src='server/src/gone.ts'")
 "src server/src/gone.ts does not exist")

(ert-deftest show-me-plan-lint-accepts-a-single-line ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (show-me-plan-lint-tests--edit "lines=\"40-58\"" "lines=\"5\""))))))

(show-me-plan-lint-tests--rejects
 "single-line-beyond-file"
 (show-me-plan-lint-tests--edit "lines=\"40-58\"" "lines=\"81\"")
 "lines=\"81-81\" is outside")

(show-me-plan-lint-tests--rejects
 "negative-lines"
 (show-me-plan-lint-tests--edit "lines=\"40-58\"" "lines=\"-3-5\"")
 "lines=\"-3-5\" must be")

(show-me-plan-lint-tests--rejects
 "malformed-lines"
 (show-me-plan-lint-tests--edit "lines=\"40-58\"" "lines=\"abc\"")
 "lines=\"abc\" must be")

(show-me-plan-lint-tests--rejects
 "reversed-lines"
 (show-me-plan-lint-tests--edit "lines=\"40-58\"" "lines=\"58-40\"")
 "is outside")

(defconst show-me-plan-lint-tests--decoy-fence-plan
  (lambda (open close)
    (show-me-plan-lint-tests--edit "```tsx mock\n<Composer />\n```"
                                   (concat open "\n```\n## 9. Not a claim\n" close))))

(ert-deftest show-me-plan-lint-nested-longer-fence-hides-a-heading ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (funcall show-me-plan-lint-tests--decoy-fence-plan "````text" "````"))))))

(ert-deftest show-me-plan-lint-tilde-fence-hides-a-heading ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (funcall show-me-plan-lint-tests--decoy-fence-plan "~~~text" "~~~"))))))

(ert-deftest show-me-plan-lint-tilde-fence-needs-matching-length ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (show-me-plan-lint-tests--edit "```tsx mock\n<Composer />\n```"
                                                    "~~~~text\n~~~\n## 9. Not a claim\n~~~~"))))))

(show-me-plan-lint-tests--rejects
 "unclosed-tilde-fence"
 (show-me-plan-lint-tests--edit "```tsx mock\n<Composer />\n```" "~~~text\n<Composer />")
 "fence is never closed")

(show-me-plan-lint-tests--rejects
 "review-options-out-of-order"
 (show-me-plan-lint-tests--edit "a) 50 - enough for a week, no paging\nb) 500 -"
                                "b) 50 - enough for a week, no paging\na) 500 -")
 "lettered a, b, c in order")

(ert-deftest show-me-plan-lint-accepts-three-lettered-options ()
  (should (= 0 (car (show-me-plan-lint-tests--run
                     (show-me-plan-lint-tests--edit "pick: a -" "c) 5000 - needs a queue\npick: a -"))))))

(ert-deftest show-me-plan-lint-accepts-crlf ()
  (let ((result (show-me-plan-lint-tests--run
                 (string-replace "\n" "\r\n" show-me-plan-lint-tests--valid))))
    (should (= 0 (car result)))
    (should (string-match-p "0 errors, 0 warnings" (cdr result)))))

(ert-deftest show-me-plan-lint-crlf-still-finds-errors ()
  (let ((result (show-me-plan-lint-tests--run
                 (string-replace "\n" "\r\n" (show-me-plan-lint-tests--edit "lines=\"40-58\"" "lines=\"70-99\"")))))
    (should (= 1 (car result)))
    (should (string-match-p "is outside" (cdr result)))))

(show-me-plan-lint-tests--rejects
 "review-inside-done"
 (show-me-plan-lint-tests--edit "npm test\n```\n" "npm test\n```\n\nQ: Why?\na) x - y\nb) x - y\npick: a - z\n")
 "must sit on a claim")

(show-me-plan-lint-tests--rejects
 "review-inside-not-changing"
 (show-me-plan-lint-tests--edit "- normal send\n" "- normal send\n\nQ: Why?\na) x - y\nb) x - y\npick: a - z\n")
 "must sit on a claim")

(show-me-plan-lint-tests--rejects
 "review-inside-shared"
 (show-me-plan-lint-tests--edit "(a int);\n```\n" "(a int);\n```\n\nQ: Why?\na) x - y\nb) x - y\npick: a - z\n")
 "must sit on a claim")

(defmacro show-me-plan-lint-tests--escapes (name src)
  `(ert-deftest ,(intern (concat "show-me-plan-lint-guard-" name)) ()
     (let ((result (show-me-plan-lint-tests--run
                    (show-me-plan-lint-tests--edit "server/src/store.ts\" lines" (concat ,src "\" lines")))))
       (should (= 1 (car result)))
       (should (string-match-p "src .+ does not exist under the root"
                               (cdr result))))))

(show-me-plan-lint-tests--escapes "dotdot-etc-passwd" "../../etc/passwd")
(show-me-plan-lint-tests--escapes "dotdot-to-real-sibling-file" "../@OUTSIDE@/secret.ts")
(show-me-plan-lint-tests--escapes "absolute-path" "/etc/passwd")
(show-me-plan-lint-tests--escapes "symlink-out-of-root" "server/src/link.ts")

(ert-deftest show-me-plan-lint-guard-covers-the-where-heading ()
  (dolist (rel '("/etc/passwd:1 · x" "../@OUTSIDE@/secret.ts:1 · x" "server/src/link.ts:1 · x"))
    (let ((result (show-me-plan-lint-tests--run
                   (show-me-plan-lint-tests--edit "server/src/store.ts:40 · createScheduled" rel))))
      (should (= 1 (car result)))
      (should (string-match-p "does not exist under the root" (cdr result))))))

(ert-deftest show-me-plan-lint-missing-file-exits-two-on-stderr ()
  (show-me-plan-lint-tests--with-repo
   (lambda (dir plan _)
     (let ((r (show-me-plan-lint-tests--call (concat plan ".gone") dir)))
       (should (= 2 (car r)))
       (should (string-empty-p (nth 1 r)))
       (should (string-match-p "\\`plan-lint: " (nth 2 r)))
       (should-not (string-match-p "Traceback" (nth 2 r)))))))

(ert-deftest show-me-plan-lint-non-utf8-exits-two-on-stderr ()
  (show-me-plan-lint-tests--with-repo
   (lambda (dir plan _)
     (let ((coding-system-for-write 'no-conversion))
       (write-region "# Bad \377\376 title\n" nil plan nil 'silent))
     (let ((r (show-me-plan-lint-tests--call plan dir)))
       (should (= 2 (car r)))
       (should (string-empty-p (nth 1 r)))
       (should (string-match-p "\\`plan-lint: " (nth 2 r)))
       (should-not (string-match-p "Traceback" (nth 2 r)))))))

(ert-deftest show-me-plan-lint-directory-as-plan-exits-two ()
  (show-me-plan-lint-tests--with-repo
   (lambda (dir _ _)
     (let ((r (show-me-plan-lint-tests--call dir dir)))
       (should (= 2 (car r)))
       (should-not (string-match-p "Traceback" (nth 2 r)))))))


(provide 'show-me-plan-lint-tests)
;;; show-me-plan-lint-tests.el ends here
