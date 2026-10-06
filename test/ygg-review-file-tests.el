;;; ygg-review-file-tests.el --- tests for the review file printer and parser -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'ygg-review-file)

(defvar ygg-git-compare-comment-types)

(defun ygg-review-file-tests--git (dir &rest args)
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string)))
      (buffer-string))))

(defvar ygg-review-file-tests--diffs (make-hash-table :test 'equal))

(defun ygg-review-file-tests--lines (prefix from to &optional skip)
  (mapconcat (lambda (n) (format "%s%d\n" prefix n))
             (seq-remove (lambda (n) (eq n skip)) (number-sequence from to)) ""))

(defun ygg-review-file-tests--diff (context)
  "The diff of a temp repo with CONTEXT lines of context.
a.txt changes at 3, grows after 20 and shrinks at 38; b.txt is added, c.txt
deleted and d.txt renamed to e.txt with an edit."
  (or (gethash context ygg-review-file-tests--diffs)
      (puthash
       context
       (let* ((process-environment
               (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1") process-environment))
              (dir (file-name-as-directory (make-temp-file "ygg-review-file-" t))))
         (unwind-protect
             (cl-flet ((git (&rest args) (apply #'ygg-review-file-tests--git dir args))
                       (write (file text) (with-temp-file (expand-file-name file dir) (insert text))))
               (git "init" "-q" "-b" "main")
               (git "config" "user.name" "T")
               (git "config" "user.email" "t@example.invalid")
               (git "config" "commit.gpgsign" "false")
               (write "a.txt" (ygg-review-file-tests--lines "l" 1 40))
               (write "c.txt" "c1\nc2\n")
               (write "d.txt" "d1\nd2\nd3\nd4\nd5\nd6\n")
               (git "add" ".")
               (git "commit" "-q" "-m" "base")
               (write "a.txt" (concat (ygg-review-file-tests--lines "l" 1 2) "l3 changed\n"
                                      (ygg-review-file-tests--lines "l" 4 20) "new20\n"
                                      (ygg-review-file-tests--lines "l" 21 40 38)))
               (write "b.txt" "b1\nb2\n")
               (delete-file (expand-file-name "c.txt" dir))
               (git "mv" "d.txt" "e.txt")
               (write "e.txt" "d1\nd2\nd3\nd4\nd5\nd6 edited\n")
               (git "add" "-A")
               (git "commit" "-q" "-m" "head")
               (git "diff" "--no-color" "--no-ext-diff" "-M" (format "-U%d" context) "HEAD~1" "HEAD"))
           (delete-directory dir t)))
       ygg-review-file-tests--diffs)))

(defun ygg-review-file-tests--range-anchor (start end)
  (let ((anchor (ygg-review-file--rec-anchor end)))
    (if (and (not (eq start end)) (ygg-review-file--content-p start) (ygg-review-file--content-p end))
        (append (list :start-side (plist-get start :side) :start-line (plist-get start :line)
                      :start-old-line (plist-get start :old-line))
                (plist-put anchor :level 'range))
      anchor)))

(defun ygg-review-file-tests--keys (a b)
  (seq-uniq (append (cl-loop for (k _) on a by #'cddr collect k)
                    (cl-loop for (k _) on b by #'cddr collect k))))

(defun ygg-review-file-tests--differences (want got)
  (cl-loop for k in (ygg-review-file-tests--keys want got)
           unless (equal (plist-get want k) (plist-get got k))
           collect (list k :want (plist-get want k) :got (plist-get got k))))

(defun ygg-review-file-tests--by-id (comments)
  (sort (copy-sequence comments)
        (lambda (a b) (string< (plist-get a :id) (plist-get b :id)))))

(defun ygg-review-file-tests--by-thread (threads)
  (apply #'append
         (sort (ygg-review-file--group threads)
               (lambda (a b) (string< (format "%s" (or (plist-get (car a) :thread) (plist-get (car a) :id)))
                                      (format "%s" (or (plist-get (car b) :thread) (plist-get (car b) :id))))))))

(defun ygg-review-file-tests--compare-lists (what want got)
  (should (equal (length want) (length got)))
  (cl-loop for w in want for g in got
           for diff = (ygg-review-file-tests--differences w g)
           when diff do (ert-fail (list what (or (plist-get w :id) w) diff))))

(defun ygg-review-file-tests--round-trip (review diff)
  (let* ((text (ygg-review-file-print review diff))
         (back (ygg-review-file-parse text)))
    (ert-info (text)
      (let ((top (ygg-review-file-tests--differences
                  (cl-loop for (k v) on review by #'cddr
                           unless (memq k '(:comments :threads)) append (list k v))
                  (cl-loop for (k v) on back by #'cddr
                           unless (memq k '(:comments :threads)) append (list k v)))))
        (when top (ert-fail (list "review fields" top))))
      (should-not (plist-get back :problems))
      (ygg-review-file-tests--compare-lists
       "comment" (ygg-review-file-tests--by-id (plist-get review :comments))
       (ygg-review-file-tests--by-id (plist-get back :comments)))
      (ygg-review-file-tests--compare-lists
       "thread" (ygg-review-file-tests--by-thread (plist-get review :threads))
       (ygg-review-file-tests--by-thread (plist-get back :threads)))
      (should (equal (ygg-review-file-diff text) (and (not (string-empty-p diff)) diff))))
    text))

(defconst ygg-review-file-tests--harvested
  (list
   (list :id "summary" :level 'review :type 'issue :text "Looks off" :range "R")
   (list :id "line" :level 'line :file "a.txt" :old-path "a.txt" :new-path "a.txt" :side 'new
         :line 2 :type 'nit :range "R" :text "line")
   (list :id "range" :level 'range :file "a.txt" :old-path "a.txt" :new-path "a.txt" :side 'new
         :line 18 :start-side 'new :start-line 12 :range "R" :text "range")
   (list :id "whole" :level 'file :file "b.txt" :old-path "b.txt" :new-path "b.txt"
         :range "R" :text "whole")
   (list :id "proposed" :level 'line :file "a.txt" :new-path "a.txt" :side 'new :line 3
         :author "codex" :status 'pending :range "R" :text "proposed")
   (list :id "md-l" :level 'line :file "src/a.rs" :side 'new :line 42 :range "R" :text "Magic number")
   (list :id "md-r" :level 'range :file "src/a.rs" :side 'new :line 55 :start-side 'new
         :start-line 50 :type 'issue :range "R" :text "Refactor\nthis block")
   (list :id "md-f" :level 'file :file "src/a.rs" :range "R" :text "Add tests" :author "codex")
   (list :id "md-o" :level 'line :file "src/a.rs" :side 'old :line 7 :range "R" :text "Gone")
   (list :id "md-s" :level 'review :type 'question :range "R" :text "Why now?")
   (list :id "md-p" :level 'line :file "src/a.rs" :side 'new :line 1 :status 'pending
         :range "R" :text "unchecked")
   (list :id "gl" :level 'line :old-path "old.txt" :new-path "new.txt" :side 'new :line 2
         :old-line 2 :range "R" :text "gl")
   (list :id "gl-range" :level 'range :new-path "a.txt" :side 'new :line 9 :old-line 8
         :start-side 'old :start-line 5 :range "R" :text "gl-range")
   (list :id "start-old" :level 'range :start-side 'new :start-old-line 3 :side 'new :line 9
         :range "R" :text "start-old")
   (list :id "mcp-1" :level 'line :type 'nit :text "from mcp" :file "a.txt" :new-path "a.txt"
         :side 'old :line 2 :start-line nil :start-side 'old :title "Keep it" :priority 2
         :confidence 0.5)
   (list :id "mcp-2" :level 'review :type nil :text "fine" :file nil :new-path nil :side 'new
         :line nil :start-line nil :start-side 'new :title nil :priority nil :confidence 0.7
         :correctness "patch is correct")
   (list :id "empty" :level 'review :text "")
   (list :id "no-text" :level 'review :text nil)
   (list :id "other-range" :level 'review :range "elsewhere" :text "old" :created 1790000000.123456))
  "Comment plists in the shapes the compare comments and submit tests build.")

(defconst ygg-review-file-tests--harvested-threads
  (list
   (list :remote t :author "alice" :created 1790000000.5 :thread 1 :level 'line :text "body"
         :replies 2)
   (list :remote t :author "alice" :created 1790000000.5 :thread 2 :level 'line :text "body"
         :replies 1 :resolved t :folded t)
   (list :remote t :id "remote:gh:8812" :thread "8812" :author "alice" :created 1790000100.0
         :text "Why 5 here?" :url "https://github.com/o/r/pull/1#discussion_r8812"
         :diff-hunk "@@ -1,3 +1,3 @@\n 1\n+two\n+three" :file "a.txt" :new-path "a.txt"
         :level 'range :side 'new :line 3 :start-line 2 :orig-line 3 :resolved nil :outdated nil)
   (list :remote t :id "remote:gh:8813" :thread "8812" :author "bob" :created 1790000200.0
         :text "Matches the config default." :url "https://github.com/o/r/pull/1#discussion_r8813"
         :diff-hunk "@@ -1,3 +1,3 @@\n 1\n+two\n+three" :file "a.txt" :new-path "a.txt"
         :level 'range :side 'new :line 3 :start-line 2 :orig-line 3 :resolved nil :outdated nil)
   (list :remote t :id "remote:gh-review:7" :thread "review-7" :author "carol" :created 1790000300.0
         :text "(no summary)" :state "changes requested" :level 'file)
   (list :remote t :id "remote:gl:1" :thread "d1" :author "dave" :created 1790000400.0
         :text "```suggestion\nbetter\n```" :file "a.txt" :new-path "a.txt" :level 'line
         :side 'old :line 2 :resolved t :outdated t :depth 1))
  "Remote thread plists in the shapes the threads code and tests build.")

(ert-deftest ygg-review-file-round-trips-every-harvested-comment ()
  (dolist (c ygg-review-file-tests--harvested)
    (ygg-review-file-tests--round-trip (list :comments (list c)) (ygg-review-file-tests--diff 3))))

(ert-deftest ygg-review-file-round-trips-all-harvested-comments-and-threads-together ()
  (ygg-review-file-tests--round-trip
   (list :repo "github.com/o/r" :pr 12 :base "9f8e7d6" :head "1a2b3c4" :round 2
         :verdict 'request-changes :range "main...feature" :text "Overall: close."
         :comments ygg-review-file-tests--harvested
         :threads ygg-review-file-tests--harvested-threads)
   (ygg-review-file-tests--diff 3)))

(ert-deftest ygg-review-file-round-trips-an-empty-review-and-diff ()
  (ygg-review-file-tests--round-trip nil "")
  (ygg-review-file-tests--round-trip (list :comments (list (list :id "r" :level 'review :text "x"))) ""))

(ert-deftest ygg-review-file-round-trips-each-thread-on-its-own ()
  (dolist (thread ygg-review-file-tests--harvested-threads)
    (ygg-review-file-tests--round-trip (list :threads (list thread)) (ygg-review-file-tests--diff 3))))

(defun ygg-review-file-tests--comment-on (rec &rest props)
  (append props
          (list :id (format "c%s" (random 100000)))
          (ygg-review-file--rec-anchor rec)
          (list :text "x" :quote (plist-get rec :text))))

(defun ygg-review-file-tests--rec-at (recs side line &optional path)
  (seq-find (lambda (r) (and (ygg-review-file--content-p r) (eq (plist-get r :side) side)
                             (eql (plist-get r :line) line)
                             (equal (plist-get r :new-path) (or path "a.txt"))))
            recs))

(defun ygg-review-file-tests--anchor-of (comment diff)
  (let ((back (ygg-review-file-parse (ygg-review-file-print (list :comments (list comment)) diff))))
    (car (plist-get back :comments))))

(defun ygg-review-file-tests--printed (comment diff)
  (ygg-review-file-print (list :comments (list comment)) diff))

(defun ygg-review-file-tests--placed-p (text id)
  (let ((diff-start (string-search "\n> diff --git" text)))
    (and diff-start (> (string-search (format "#%s" id) text) diff-start))))

(ert-deftest ygg-review-file-anchors-on-an-added-line ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (c (ygg-review-file-tests--comment-on (ygg-review-file-tests--rec-at recs 'new 21)
                                               :id "add")))
    (should (equal (plist-get c :line) 21))
    (should (ygg-review-file-tests--placed-p (ygg-review-file-tests--printed c diff) "add"))
    (should-not (ygg-review-file-tests--differences c (ygg-review-file-tests--anchor-of c diff)))))

(ert-deftest ygg-review-file-anchors-on-a-deleted-line-on-the-old-side ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (c (ygg-review-file-tests--comment-on (ygg-review-file-tests--rec-at recs 'old 38) :id "del")))
    (should (eq (plist-get c :side) 'old))
    (should (equal (plist-get c :file) "a.txt"))
    (should (ygg-review-file-tests--placed-p (ygg-review-file-tests--printed c diff) "del"))
    (should-not (ygg-review-file-tests--differences c (ygg-review-file-tests--anchor-of c diff)))))

(ert-deftest ygg-review-file-anchors-on-a-context-line-with-its-old-line ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (c (ygg-review-file-tests--comment-on (ygg-review-file-tests--rec-at recs 'new 38) :id "ctx")))
    (should (equal (list (plist-get c :line) (plist-get c :old-line)) '(38 37)))
    (should (ygg-review-file-tests--placed-p (ygg-review-file-tests--printed c diff) "ctx"))
    (should-not (ygg-review-file-tests--differences c (ygg-review-file-tests--anchor-of c diff)))))

(ert-deftest ygg-review-file-anchors-a-multi-line-range ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (start (ygg-review-file-tests--rec-at recs 'old 3))
         (end (ygg-review-file-tests--rec-at recs 'new 3))
         (c (append (list :id "rng" :text "x"
                          :quote (string-join (mapcar (lambda (r) (plist-get r :text))
                                                      (list start (ygg-review-file-tests--rec-at recs 'new 3)))
                                              "\n"))
                    (ygg-review-file-tests--range-anchor start end)))
         (text (ygg-review-file-tests--printed c diff)))
    (should (eq (plist-get c :level) 'range))
    (should (equal (list (plist-get c :start-side) (plist-get c :start-line)) '(old 3)))
    (should (string-search "\n> -l3\n> +l3 changed\n::: {.c #rng start-side=old span=2}\nx\n:::\n"
                           text))
    (should-not (ygg-review-file-tests--differences c (ygg-review-file-tests--anchor-of c diff)))))

(ert-deftest ygg-review-file-anchors-a-hunk-level-range ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (start (ygg-review-file-tests--rec-at recs 'new 21))
         (end (ygg-review-file-tests--rec-at recs 'new 21))
         (hunk-start (ygg-review-file-tests--rec-at recs 'old 38))
         (hunk-end (ygg-review-file-tests--rec-at recs 'new 38))
         (c (append (list :id "hunk" :text "whole hunk"
                          :quote (concat (plist-get hunk-start :text) "\n" (plist-get hunk-end :text)))
                    (ygg-review-file-tests--range-anchor hunk-start hunk-end))))
    (should start) (should end)
    (should (eq (plist-get c :level) 'range))
    (should-not (ygg-review-file-tests--differences c (ygg-review-file-tests--anchor-of c diff)))))

(ert-deftest ygg-review-file-anchors-at-file-and-review-level ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (file (list :id "file" :level 'file :file "b.txt" :old-path "b.txt" :new-path "b.txt"
                     :text "whole file"))
         (review (list :id "rev" :level 'review :text "overall"))
         (text (ygg-review-file-tests--printed file diff)))
    (should (string-match-p "> diff --git a/b.txt b/b.txt\n::: {.c #file}\nwhole file\n:::\n> new file mode"
                            text))
    (should-not (ygg-review-file-tests--differences file (ygg-review-file-tests--anchor-of file diff)))
    (should (< (string-search "#rev" (ygg-review-file-tests--printed review diff))
               (string-search "> diff --git" (ygg-review-file-tests--printed review diff))))
    (should-not (ygg-review-file-tests--differences review (ygg-review-file-tests--anchor-of review diff)))))

(ert-deftest ygg-review-file-anchors-on-renamed-and-deleted-files ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (renamed (seq-find (lambda (r) (and (ygg-review-file--content-p r)
                                             (equal (plist-get r :new-path) "e.txt")
                                             (eq (plist-get r :kind) 'add)))
                            recs))
         (deleted (seq-find (lambda (r) (and (eq (plist-get r :kind) 'del)
                                             (equal (plist-get r :old-path) "c.txt")))
                            recs)))
    (dolist (rec (list renamed deleted))
      (let ((c (ygg-review-file-tests--comment-on rec :id "x")))
        (should (ygg-review-file-tests--placed-p (ygg-review-file-tests--printed c diff) "x"))
        (should-not (ygg-review-file-tests--differences c (ygg-review-file-tests--anchor-of c diff)))))
    (should (equal (plist-get renamed :old-path) "d.txt"))))

(ert-deftest ygg-review-file-a-reply-comment-follows-its-thread-and-keeps-its-reply-to ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (thread (list :remote t :id "remote:gh:1" :thread "1" :author "alice" :created 1.5
                       :text "Why 5 here?" :file "a.txt" :new-path "a.txt" :level 'line
                       :side 'new :line 21))
         (c (list :id "l3" :level 'line :file "a.txt" :old-path "a.txt" :new-path "a.txt"
                  :side 'new :line 21 :reply-to "1" :text "Agreed" :quote "+new20"))
         (text (ygg-review-file-print (list :comments (list c) :threads (list thread)) diff)))
    (should (string-search "\n::::\n\n::: {.c #l3 reply-to=\"1\" quote=\"+new20\"}\nAgreed\n:::\n" text))
    (should (string-search "> +new20\n:::: {.thread #1 diff-hunk=null}" text))
    (ygg-review-file-tests--round-trip (list :comments (list c) :threads (list thread)) diff)))

(ert-deftest ygg-review-file-a-comment-inside-a-range-still-round-trips ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (start (ygg-review-file-tests--rec-at recs 'old 3))
         (end (ygg-review-file-tests--rec-at recs 'new 3))
         (range (append (list :id "r" :text "range" :quote "-l3\n+l3 changed")
                        (ygg-review-file-tests--range-anchor start end)))
         (inner (ygg-review-file-tests--comment-on start :id "i")))
    (ygg-review-file-tests--round-trip (list :comments (list inner range)) diff)))

(ert-deftest ygg-review-file-snips-far-context-and-keeps-anchors ()
  (let* ((diff (ygg-review-file-tests--diff 20))
         (recs (ygg-review-file--records diff))
         (near (ygg-review-file-tests--comment-on (ygg-review-file-tests--rec-at recs 'new 38) :id "near"))
         (far (ygg-review-file-tests--comment-on (ygg-review-file-tests--rec-at recs 'new 10) :id "far"))
         (review (list :comments (list near far)))
         (text (ygg-review-file-print review diff))
         (back (ygg-review-file-parse text)))
    (should (string-match-p "^> \\[\\.\\.\\. [0-9]+ lines\\]$" text))
    (should (< (length text) (length (let ((ygg-review-file-snip-threshold nil))
                                       (ygg-review-file-print review diff)))))
    (ygg-review-file-tests--compare-lists "snipped" (ygg-review-file-tests--by-id (list near far))
                                          (ygg-review-file-tests--by-id (plist-get back :comments)))
    (should (equal (ygg-review-file-parse (ygg-review-file-print review diff)) back))
    (let ((recovered (ygg-review-file--records (ygg-review-file-diff text))))
      (should (equal (plist-get (aref recovered (1- (length recovered))) :line)
                     (plist-get (aref recs (1- (length recs))) :line)))
      (should (equal (plist-get (ygg-review-file-tests--rec-at recovered 'new 38) :old-line) 37)))))

(ert-deftest ygg-review-file-snipping-off-keeps-the-diff-verbatim-and-the-text-is-recoverable-from-git ()
  (let* ((diff (ygg-review-file-tests--diff 20))
         (off (let ((ygg-review-file-snip-threshold nil))
                (ygg-review-file-print (list :comments nil) diff)))
         (on (ygg-review-file-print (list :comments nil) diff)))
    (should (equal (ygg-review-file-diff off) diff))
    (should (string-search "[..." on))
    (should-not (equal (ygg-review-file-diff on) diff))))

(ert-deftest ygg-review-file-snips-never-hide-a-range-or-a-thread ()
  (let* ((diff (ygg-review-file-tests--diff 20))
         (recs (ygg-review-file--records diff))
         (start (ygg-review-file-tests--rec-at recs 'new 5))
         (end (ygg-review-file-tests--rec-at recs 'new 15))
         (range (append (list :id "r" :text "wide" :quote (string-join
                                                           (cl-loop for r across recs
                                                                    when (and (ygg-review-file--content-p r)
                                                                              (eq (plist-get r :side) 'new)
                                                                              (<= 5 (plist-get r :line) 15))
                                                                    collect (plist-get r :text))
                                                           "\n"))
                        (ygg-review-file-tests--range-anchor start end)))
         (thread (list :remote t :id "remote:gh:3" :thread "3" :author "a" :text "t" :file "a.txt"
                       :new-path "a.txt" :level 'line :side 'new :line 30)))
    (ygg-review-file-tests--round-trip (list :comments (list range) :threads (list thread)) diff)))

(defconst ygg-review-file-tests--texts
  '("" "plain" "multi\nline" "trailing\n" "\nleading" "\n" "  indented  " "ünïcödé ✓ 日本語 🙂"
    "```elisp\n(setq x 1)\n```" "- [x] done\n- [ ] todo" "[x] at the start" "[id] looks like an id"
    "> a quote\n> another" ">> thread-like" "::: {.c #fake}\nbody\n:::" ":::\nalone" "::::::\nlong"
    "---\nrule\n---" "key: value # not a comment" "\"quoted\" and 'single' and \\back" "tab\there"
    "line1\r\nline2" "`inline` **bold** _it_" "1. one\n2. two" "# heading\ntext" "}{ ] [ , :" "@@ -1 +1 @@")
  "Text bodies that stress the fences, the quoting and markdown.")

(defun ygg-review-file-tests--pick (list) (nth (random (length list)) list))

(defun ygg-review-file-tests--random-comment (recs index)
  (let* ((content (seq-filter #'ygg-review-file--content-p (append recs nil)))
         (rec (ygg-review-file-tests--pick content))
         (level (ygg-review-file-tests--pick '(review file line line line range range)))
         (id (format "%x%04x%d" (random #xffffff) (random #x10000) index))
         (base (append (list :id id :text (ygg-review-file-tests--pick ygg-review-file-tests--texts)
                             :type (ygg-review-file-tests--pick '(nil nit issue todo fix question))
                             :range (ygg-review-file-tests--pick '(nil "main...feature" "a: b"))
                             :created (+ 1790000000 (/ (random 1000000) 1000.0))
                             :author (ygg-review-file-tests--pick '(nil "you" "agent:codex" "Zoë #1")))
                       (and (zerop (random 3)) (list :priority (random 4)))
                       (and (zerop (random 3)) (list :confidence (/ (random 100) 100.0)))
                       (and (zerop (random 4)) (list :status 'pending))
                       (and (zerop (random 4)) (list :title (ygg-review-file-tests--pick ygg-review-file-tests--texts)))
                       (and (zerop (random 5)) (list :correctness "patch: ok"))
                       (and (zerop (random 6)) (list :subject 'odd-symbol))
                       (and (zerop (random 6)) (list :count -3)))))
    (pcase level
      ('review (append base (list :level 'review)))
      ('file (append base (ygg-review-file--rec-anchor (ygg-review-file-tests--header recs rec))))
      ('line (append base (ygg-review-file--rec-anchor rec)
                     (list :quote (ygg-review-file-tests--pick (list (plist-get rec :text) nil "ctx\nwindow")))))
      (_ (let* ((same (seq-filter (lambda (r) (equal (plist-get r :new-path) (plist-get rec :new-path)))
                                  content))
                (a (ygg-review-file-tests--pick same))
                (b (ygg-review-file-tests--pick same)))
           (when (> (cl-position a content) (cl-position b content)) (cl-rotatef a b))
           (append base (ygg-review-file-tests--range-anchor a b)
                   (list :quote (ygg-review-file-tests--pick (list (plist-get a :text) "-x\n+y")))))))))

(defun ygg-review-file-tests--header (recs rec)
  (seq-find (lambda (r) (and (eq (plist-get r :kind) 'diff)
                             (equal (plist-get r :new-path) (plist-get rec :new-path))))
            recs))

(defun ygg-review-file-tests--random-thread (recs n)
  (let* ((content (seq-filter #'ygg-review-file--content-p (append recs nil)))
         (rec (ygg-review-file-tests--pick content))
         (tid (ygg-review-file-tests--pick (list (format "t%d" n) n (format "%d" (+ 1000 n)))))
         (root (append (list :remote t :thread tid :level 'line :file (plist-get rec :new-path)
                             :new-path (plist-get rec :new-path)
                             :side (plist-get rec :side) :line (plist-get rec :line)
                             :resolved (zerop (random 2)) :outdated (zerop (random 3))
                             :url "https://example.test/x?a=1&b=\"2\"" :depth (random 3))
                       (and (zerop (random 3)) (list :orig-line (random 90)))
                       (and (zerop (random 3)) (list :state "changes requested")))))
    (cl-loop for k from 0 to (random 3)
             collect (append (list :id (format "remote:gh:%d-%d" n k) :author (ygg-review-file-tests--pick
                                                                               '("alice" "bob" "carol_9" "dä"))
                                   :created (+ 1790000000 (random 100000) 0.25)
                                   :text (ygg-review-file-tests--pick ygg-review-file-tests--texts))
                             (if (zerop k) root
                               (append (and (zerop (random 3)) (list :outdated t))
                                       (cl-loop for (key val) on root by #'cddr
                                                unless (memq key '(:outdated)) append (list key val))))))))

(defun ygg-review-file-tests--random-review (diff n)
  (let* ((recs (ygg-review-file--records diff))
         (comments (cl-loop for i below (random 9) collect (ygg-review-file-tests--random-comment recs i)))
         (threads (cl-loop for i below (random 4) append (ygg-review-file-tests--random-thread recs (+ i (* 10 n))))))
    (append (list :comments comments :threads threads)
            (and (zerop (random 2)) (list :repo (ygg-review-file-tests--pick '("github.com/o/r" "gitlab:x y"))))
            (and (zerop (random 2)) (list :pr (ygg-review-file-tests--pick '(12 "12" "")))
                 )
            (and (zerop (random 2)) (list :base (ygg-review-file-tests--pick '("9f8e7d6" "1234567" "e5"))
                                          :head "abcdef0"))
            (and (zerop (random 2)) (list :round (random 5)
                                          :verdict (ygg-review-file-tests--pick '(approve request-changes "comment"))
                                          :range "main...feature"))
            (and (zerop (random 2)) (list :text (ygg-review-file-tests--pick ygg-review-file-tests--texts))))))

(ert-deftest ygg-review-file-200-random-reviews-round-trip ()
  (random "ygg-review-file")
  (let ((diff (ygg-review-file-tests--diff 3)))
    (dotimes (n 200)
      (ygg-review-file-tests--round-trip (ygg-review-file-tests--random-review diff n) diff))))

(ert-deftest ygg-review-file-200-random-reviews-round-trip-over-a-snipped-diff ()
  (random "ygg-review-file-snips")
  (let ((diff (ygg-review-file-tests--diff 20)))
    (dotimes (n 200)
      (let ((review (ygg-review-file-tests--random-review diff n)))
        (let* ((text (ygg-review-file-print review diff))
               (back (ygg-review-file-parse text)))
          (ygg-review-file-tests--compare-lists "comment" (ygg-review-file-tests--by-id (plist-get review :comments))
                                                (ygg-review-file-tests--by-id (plist-get back :comments)))
          (ygg-review-file-tests--compare-lists "thread" (ygg-review-file-tests--by-thread (plist-get review :threads))
       (ygg-review-file-tests--by-thread (plist-get back :threads))))))))

(ert-deftest ygg-review-file-comments-keep-their-anchors-in-the-diff-for-real-anchors ()
  (random "ygg-review-file-placed")
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (lines (seq-filter #'ygg-review-file--content-p (append recs nil)))
         (comments (cl-loop for rec in lines for i from 0
                            collect (ygg-review-file-tests--comment-on rec :id (format "p%d" i))))
         (text (ygg-review-file-print (list :comments comments) diff)))
    (should (= (length comments)
               (length (seq-filter (lambda (c) (ygg-review-file-tests--placed-p text (plist-get c :id)))
                                   comments))))))

(defconst ygg-review-file-tests--golden-diff
  (concat "diff --git a/a.txt b/a.txt\n"
          "index 1111111..2222222 100644\n"
          "--- a/a.txt\n"
          "+++ b/a.txt\n"
          "@@ -1,4 +1,5 @@\n"
          " one\n"
          "-two\n"
          "+deux\n"
          "+deux bis\n"
          " three\n"
          " four\n"
          "diff --git a/b.txt b/b.txt\n"
          "new file mode 100644\n"
          "index 0000000..3333333\n"
          "--- /dev/null\n"
          "+++ b/b.txt\n"
          "@@ -0,0 +1 @@\n"
          "+hello\n"))

(defconst ygg-review-file-tests--golden-hunk
  "@@ -1,4 +1,5 @@\n one\n-two\n+deux\n+deux bis\n three")

(defconst ygg-review-file-tests--golden-review
  (list :repo "github.com/owner/repo" :pr 12 :base "9f8e7d6" :head "1a2b3c4" :round 2
        :verdict 'request-changes :range "main...feature" :text "Overall: retry logic is close."
        :comments
        (list (list :id "r1" :level 'review :type 'issue :priority 1 :author "you"
                    :range "main...feature" :created 1790000000.5
                    :text "Must fix the empty-input case.")
              (list :id "f1" :level 'file :file "b.txt" :old-path "b.txt" :new-path "b.txt"
                    :type 'nit :author "you" :range "main...feature" :created 1790000010.25
                    :text "Add a trailing newline?")
              (list :id "l1" :level 'line :file "a.txt" :old-path "a.txt" :new-path "a.txt"
                    :side 'new :line 1 :old-line 1 :type 'nit :author "you"
                    :range "main...feature" :created 1790000020.125
                    :quote " one\n-two\n+deux"
                    :text "Name this constant.\n\n```\nkeep: [x]\n```\n> not a diff line")
              (list :id "l2" :level 'range :file "a.txt" :old-path "a.txt" :new-path "a.txt"
                    :start-side 'new :start-line 2 :side 'new :line 3
                    :type 'issue :priority 1 :confidence 0.7 :author "agent:codex" :status 'pending
                    :range "main...feature" :created 1790000030.0
                    :quote "+deux\n+deux bis" :text "Both lines need a test."))
        :threads
        (list (list :remote t :id "remote:gh:8812" :thread "gh-8812" :author "alice"
                    :created 1790000000.5 :text "Why 5 here?" :level 'line :file "a.txt"
                    :new-path "a.txt" :side 'new :line 4 :resolved nil :outdated t
                    :diff-hunk ygg-review-file-tests--golden-hunk :orig-line 4
                    :url "https://github.com/owner/repo/pull/12#discussion_r8812")
              (list :remote t :id "remote:gh:8813" :thread "gh-8812" :author "bob"
                    :created 1790000100.0 :text "Matches the config default." :level 'line
                    :file "a.txt" :new-path "a.txt" :side 'new :line 4 :resolved nil :outdated t
                    :diff-hunk ygg-review-file-tests--golden-hunk :orig-line 4
                    :url "https://github.com/owner/repo/pull/12#discussion_r8813")))
  "A small review whose printed form is checked in below.")

(defconst ygg-review-file-tests--golden
  (concat
   "---\n"
   "repo: github.com/owner/repo\n"
   "pr: 12\n"
   "base: \"9f8e7d6\"\n"
   "head: \"1a2b3c4\"\n"
   "round: 2\n"
   "verdict: request-changes\n"
   "range: main...feature\n"
   "author: you\n"
   "---\n"
   "::: {.summary}\n"
   "Overall: retry logic is close.\n"
   ":::\n"
   "\n"
   "::: {.c #r1 type=issue priority=1 created=2026-09-21T14:13:20.5Z}\n"
   "Must fix the empty-input case.\n"
   ":::\n"
   "\n"
   "> diff --git a/a.txt b/a.txt\n"
   "> index 1111111..2222222 100644\n"
   "> --- a/a.txt\n"
   "> +++ b/a.txt\n"
   "> @@ -1,4 +1,5 @@\n"
   ">  one\n"
   "::: {.c #l1 type=nit created=2026-09-21T14:13:40.125Z}\n"
   "Name this constant.\n"
   "\n"
   "```\n"
   "keep: [x]\n"
   "```\n"
   "> not a diff line\n"
   ":::\n"
   "> -two\n"
   "> +deux\n"
   "> +deux bis\n"
   "::: {.c #l2 type=issue priority=1 confidence=0.7 created=2026-09-21T14:13:50.0Z author=\"agent:codex\" status=pending span=2}\n"
   "Both lines need a test.\n"
   ":::\n"
   ">  three\n"
   ":::: {.thread #gh-8812 outdated=true orig-line=4}\n"
   "::: {.reply #remote:gh:8812 created=2026-09-21T14:13:20.5Z author=alice url=\"https://github.com/owner/repo/pull/12#discussion_r8812\"}\n"
   "Why 5 here?\n"
   ":::\n"
   "\n"
   "::: {.reply #remote:gh:8813 created=2026-09-21T14:15:00.0Z author=bob url=\"https://github.com/owner/repo/pull/12#discussion_r8813\"}\n"
   "Matches the config default.\n"
   ":::\n"
   "::::\n"
   ">  four\n"
   "> diff --git a/b.txt b/b.txt\n"
   "::: {.c #f1 type=nit created=2026-09-21T14:13:30.25Z}\n"
   "Add a trailing newline?\n"
   ":::\n"
   "> new file mode 100644\n"
   "> index 0000000..3333333\n"
   "> --- /dev/null\n"
   "> +++ b/b.txt\n"
   "> @@ -0,0 +1 @@\n"
   "> +hello\n")
  "The checked-in expected review file.")

(ert-deftest ygg-review-file-golden ()
  (let ((text (ygg-review-file-print ygg-review-file-tests--golden-review
                                     ygg-review-file-tests--golden-diff)))
    (should (equal text ygg-review-file-tests--golden))
    (ygg-review-file-tests--round-trip ygg-review-file-tests--golden-review
                                       ygg-review-file-tests--golden-diff)))

(ert-deftest ygg-review-file-front-matter-holds-only-the-review-fields ()
  (let ((text (ygg-review-file-print ygg-review-file-tests--golden-review
                                     ygg-review-file-tests--golden-diff)))
    (should (string-match "\\`---\n\\(\\(?:.*\n\\)*?\\)---\n" text))
    (should (equal (match-string 1 text)
                   (concat "repo: github.com/owner/repo\npr: 12\nbase: \"9f8e7d6\"\n"
                           "head: \"1a2b3c4\"\nround: 2\nverdict: request-changes\n"
                           "range: main...feature\nauthor: you\n")))))

(defconst ygg-review-file-tests--hand-diff
  (concat "diff --git a/a.txt b/a.txt\n" "index 1..2 100644\n" "--- a/a.txt\n" "+++ b/a.txt\n"
          "@@ -1,5 +1,6 @@\n one\n two\n-three\n+three!\n+three-b\n four\n five\n"))

(defun ygg-review-file-tests--idx (recs side line)
  (cl-position-if (lambda (r) (and (ygg-review-file--content-p r) (eq (plist-get r :side) side)
                                   (eql (plist-get r :line) line)))
                  recs))

(defun ygg-review-file-tests--derived (recs i id span &rest props)
  (append (list :id id) props (car (ygg-review-file--comment-derive recs i span nil))))

(defun ygg-review-file-tests--hand-review ()
  (let ((recs (ygg-review-file--records ygg-review-file-tests--hand-diff)))
    (list :repo "o/r"
          :comments
          (list (ygg-review-file-tests--derived
                 recs (ygg-review-file-tests--idx recs 'new 2) "c1" nil :type 'nit :text "first")
                (ygg-review-file-tests--derived
                 recs (ygg-review-file-tests--idx recs 'new 4) "c2" nil :type 'issue
                 :text "second\nline2")
                (ygg-review-file-tests--derived
                 recs (ygg-review-file-tests--idx recs 'new 4) "r1" 2 :type 'issue :text "range")))))

(defun ygg-review-file-tests--hand-text ()
  (ygg-review-file-print (ygg-review-file-tests--hand-review) ygg-review-file-tests--hand-diff))

(defun ygg-review-file-tests--sub (text from to)
  (let ((at (string-search from text)))
    (should at)
    (concat (substring text 0 at) to (substring text (+ at (length from))))))

(defun ygg-review-file-tests--c (review id)
  (seq-find (lambda (c) (equal (plist-get c :id) id)) (plist-get review :comments)))

(defun ygg-review-file-tests--problem (review regexp &optional line)
  (seq-find (lambda (p) (and (string-match-p regexp (cdr p)) (or (null line) (eql (car p) line))))
            (plist-get review :problems)))

(defun ygg-review-file-tests--line-of (text needle)
  (1+ (cl-count ?\n (substring text 0 (string-search needle text)))))

(defun ygg-review-file-tests--without-problems (review)
  (cl-loop for (k v) on review by #'cddr unless (eq k :problems) append (list k v)))

(ert-deftest ygg-review-file-hand-text-prints-the-compact-form ()
  (should (equal (ygg-review-file-tests--hand-text)
                 (concat "---\nrepo: o/r\n---\n"
                         "> diff --git a/a.txt b/a.txt\n> index 1..2 100644\n> --- a/a.txt\n"
                         "> +++ b/a.txt\n> @@ -1,5 +1,6 @@\n>  one\n>  two\n"
                         "::: {.c #c1 type=nit}\nfirst\n:::\n"
                         "> -three\n> +three!\n> +three-b\n"
                         "::: {.c #c2 type=issue}\nsecond\nline2\n:::\n\n"
                         "::: {.c #r1 type=issue span=2}\nrange\n:::\n"
                         ">  four\n>  five\n"))))

(ert-deftest ygg-review-file-omits-what-position-and-front-matter-tell ()
  (let* ((diff ygg-review-file-tests--hand-diff)
         (recs (ygg-review-file--records diff))
         (line (ygg-review-file-tests--derived
                recs (ygg-review-file-tests--idx recs 'new 2) "a" nil :text "x" :range "R"
                :created 1790000000.5))
         (other (ygg-review-file-tests--derived
                 recs (ygg-review-file-tests--idx recs 'new 2) "b" nil :text "x" :range "S"
                 :quote "custom"))
         (text (ygg-review-file-print (list :range "R" :comments (list line other)) diff)))
    (should (string-search "\n::: {.c #a created=2026-09-21T14:13:20.5Z}\nx\n:::\n" text))
    (should (string-search "\n::: {.c #b range=S quote=custom}\nx\n:::\n" text))
    (dolist (word '("side=" "line=" "old-line=" "file=" "old-path=" "new-path=" "level=" "start-"))
      (should-not (string-search word text)))
    (should (equal (plist-get (ygg-review-file-tests--c (ygg-review-file-parse text) "a") :range) "R"))))

(ert-deftest ygg-review-file-created-prints-iso-or-a-float-and-round-trips-exactly ()
  (dolist (created (list 1790000000 1790000000.0 1790000000.5 1790000123.456 0 0.25
                         1790000123.4567891 1.0e22 -5 -0.5 1.0e+INF))
    (let* ((c (list :id "c" :level 'review :created created :text "x"))
           (text (ygg-review-file-print (list :comments (list c)) ""))
           (back (car (plist-get (ygg-review-file-parse text) :comments))))
      (should (equal (plist-get back :created) created))
      (should (eq (floatp (plist-get back :created)) (floatp created)))
      (if (and (numberp created) (<= 0 created 1.0e12))
          (should (string-match-p "created=[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9:]+\\(\\.[0-9]+\\)?Z[ }]" text))
        (should-not (string-match-p "created=[0-9]\\{4\\}-" text))))))

(ert-deftest ygg-review-file-infinities-and-nan-round-trip ()
  (dolist (value (list 1.0e+INF -1.0e+INF 0.0e+NaN -0.0 1.5e300))
    (let* ((c (list :id "f" :level 'review :score value :text "x"))
           (text (ygg-review-file-print (list :comments (list c) :round value) ""))
           (back (ygg-review-file-parse text)))
      (should (equal (plist-get (car (plist-get back :comments)) :score) value))
      (should (equal (plist-get back :round) value))
      (should-not (plist-get back :problems)))))

(ert-deftest ygg-review-file-ids-with-odd-characters-are-quoted ()
  (dolist (id '("a b" "a\nb" "id with space" "has#hash" "x}y" "q\"uote" "e=f" "::: {.c #z}" "é😀" ""
                "tab\tid" "a\r\nb" "{.c" "-" "remote:gh:1" "A.b_c-1:2"))
    (let* ((c (list :id id :level 'review :text "x"))
           (text (ygg-review-file-print (list :comments (list c)) ""))
           (back (ygg-review-file-parse text)))
      (should (equal (plist-get (car (plist-get back :comments)) :id) id))
      (should-not (plist-get back :problems))
      (should (equal (ygg-review-file-print (ygg-review-file-tests--without-problems back) "") text)))))

(ert-deftest ygg-review-file-print-never-signals-on-odd-values ()
  (let* ((table (make-hash-table))
         (c (list :id 7 :level 'review :text 12 :labels '("a" (1 . 2) [x "y"]) :vec [1 2]
                  :obj table :fn #'car :created "not a number"))
         (text (ygg-review-file-print (list :pr [1 2] :comments (list c (list :id 'sym :text nil)))
                                      "")))
    (let* ((back (ygg-review-file-parse text))
           (b (car (plist-get back :comments))))
      (should (equal (plist-get b :id) "7"))
      (should (equal (plist-get b :labels) '("a" (1 . 2) [x "y"])))
      (should (equal (plist-get b :vec) [1 2]))
      (should (equal (plist-get back :pr) [1 2]))
      (should (stringp (plist-get b :obj)))
      (should (equal (plist-get b :text) "12"))
      (should (equal (plist-get (cadr (plist-get back :comments)) :id) "sym"))
      (should-not (plist-member b :created))
      (should (equal (mapcar #'cdr (plist-get back :problems)) '("bad value for created, ignored"))))))

(ert-deftest ygg-review-file-an-unreadable-lisp-value-is-a-problem-not-a-signal ()
  (let ((back (ygg-review-file-parse
               "::: {.c #a labels=~\"#s(hash-table)\" more=~\"(unclosed\" ok=1}\nx\n:::\n")))
    (should (equal (plist-get (car (plist-get back :comments)) :ok) 1))
    (should (= 2 (length (plist-get back :problems))))))

(ert-deftest ygg-review-file-level-nil-round-trips-anchored-and-not ()
  (let* ((diff ygg-review-file-tests--hand-diff)
         (recs (ygg-review-file--records diff))
         (anchored (ygg-review-file-tests--derived
                    recs (ygg-review-file-tests--idx recs 'new 2) "a" nil :text "x"))
         (orphan (list :id "o" :level nil :file "zzz" :text "y"))
         (review (list :comments (list (plist-put (copy-sequence anchored) :level nil) orphan)))
         (text (ygg-review-file-print review diff)))
    (should (string-search "#a level=null" text))
    (should (string-search "#o level=null" text))
    (ygg-review-file-tests--round-trip review diff)))

(ert-deftest ygg-review-file-range-span-counts-only-the-lines-on-its-sides ()
  (let* ((diff ygg-review-file-tests--hand-diff)
         (recs (ygg-review-file--records diff))
         (new-new (ygg-review-file-tests--range-anchor
                   (aref recs (ygg-review-file-tests--idx recs 'new 2))
                   (aref recs (ygg-review-file-tests--idx recs 'new 4))))
         (old-new (ygg-review-file-tests--range-anchor
                   (aref recs (ygg-review-file-tests--idx recs 'old 3))
                   (aref recs (ygg-review-file-tests--idx recs 'new 4))))
         (old-old (list :level 'range :file "a.txt" :old-path "a.txt" :new-path "a.txt"
                        :start-side 'old :start-line 3 :side 'old :line 3))
         (one (list :level 'range :file "a.txt" :old-path "a.txt" :new-path "a.txt"
                    :start-side 'new :start-line 4 :side 'new :line 4 :old-line nil)))
    (dolist (case (list (list "nn" new-new 3 "span=3")
                        (list "on" old-new 3 "start-side=old span=3")
                        (list "oo" old-old 1 "level=range")
                        (list "one" one 1 "level=range")))
      (let* ((c (append (list :id (car case) :text "x") (nth 1 case)))
             (text (ygg-review-file-print (list :comments (list c)) diff)))
        (should (string-match-p (format "{\\.c #%s[^}]*%s[^}]*}" (car case) (nth 3 case)) text))
        (ygg-review-file-tests--round-trip (list :comments (list c)) diff)))))

(ert-deftest ygg-review-file-overlapping-ranges-and-inner-comments-keep-their-places ()
  (let* ((ygg-review-file-snip-threshold nil)
         (diff (ygg-review-file-tests--diff 20))
         (recs (ygg-review-file--records diff))
         (at (lambda (side line) (aref recs (ygg-review-file-tests--rec-at-index recs side line))))
         (range (lambda (id a b) (append (list :id id :text id)
                                         (ygg-review-file-tests--range-anchor (funcall at 'new a)
                                                                       (funcall at 'new b)))))
         (comments (list (funcall range "r1" 3 12) (funcall range "r2" 8 18) (funcall range "r3" 12 18)
                         (funcall range "r4" 3 18)
                         (ygg-review-file-tests--comment-on (funcall at 'new 10) :id "in1")
                         (ygg-review-file-tests--comment-on (funcall at 'new 18) :id "end"))))
    (ygg-review-file-tests--round-trip (list :comments comments) diff)
    (let ((text (ygg-review-file-print (list :comments comments) diff)))
      (should (equal text (ygg-review-file-print
                           (ygg-review-file-parse text) diff))))))

(defun ygg-review-file-tests--rec-at-index (recs side line)
  (cl-position (ygg-review-file-tests--rec-at recs side line) recs))

(ert-deftest ygg-review-file-a-blank-line-between-quoted-lines-means-nothing ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (base (ygg-review-file-tests--without-problems (ygg-review-file-parse text))))
    (dolist (edit '(("> -three\n> +three!" "> -three\n\n> +three!")
                    (">  two\n" "\n>  two\n")
                    ("> +three-b\n::: {.c #c2" "> +three-b\n\n\n::: {.c #c2")))
      (let ((back (ygg-review-file-parse (ygg-review-file-tests--sub text (car edit) (cadr edit)))))
        (should-not (plist-get back :problems))
        (should (equal (ygg-review-file-tests--without-problems back) base))))))

(ert-deftest ygg-review-file-hand-edit-bad-attributes-are-skipped-and-reported ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (line (ygg-review-file-tests--line-of text "{.c #c1")))
    (dolist (case '(("#c1" "#c1 bogus" "bogus")
                    ("type=nit" "type=\"unterminated" "unterminated")
                    ("type=nit" "type=" "missing value")
                    ("type=nit" "type=\"abc\\" "unterminated")
                    ("type=nit" "type=\"\\uZZ\"" "bad escape")
                    ("type=nit" "type=\"\\UFFFFFFFF\"" "bad escape")
                    ("type=nit" "extra=\"x" "unterminated")
                    ("type=nit" "line=abc" "bad value for line")
                    ("type=nit" "side=sideways" "bad value for side")))
      (let* ((edited (ygg-review-file-tests--sub text (nth 0 case) (nth 1 case)))
             (back (ygg-review-file-parse edited))
             (c (ygg-review-file-tests--c back "c1")))
        (should (ygg-review-file-tests--problem back (nth 2 case) line))
        (should (equal (ygg-review-file-diff edited) ygg-review-file-tests--hand-diff))
        (should (equal (plist-get c :text) "first"))
        (should (equal (plist-get c :line) 2))
        (should (ygg-review-file-tests--c back "c2"))
        (should (ygg-review-file-tests--c back "r1"))
        (should (or (equal (car case) "#c1") (not (plist-get c :type))))))
    (let ((back (ygg-review-file-parse (ygg-review-file-tests--sub text "#c1" "#c1 bogus"))))
      (should (eq (plist-get (ygg-review-file-tests--c back "c1") :type) 'nit)))))

(ert-deftest ygg-review-file-hand-edit-malformed-front-matter-is-skipped-and-reported ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (back (ygg-review-file-parse (ygg-review-file-tests--sub text "repo: o/r" "repo: \"unterminated"))))
    (should (ygg-review-file-tests--problem back "unterminated" 2))
    (should-not (plist-member back :repo))
    (should (= 3 (length (plist-get back :comments)))))
  (let ((back (ygg-review-file-parse "---\nfoo bar\n: x\nrepo:\npr: 12\n---\n::: {.c #a}\nhi\n:::\n")))
    (should (equal (plist-get back :pr) 12))
    (should (ygg-review-file-tests--problem back "foo bar" 2))
    (should (ygg-review-file-tests--problem back ": x" 3))
    (should (ygg-review-file-tests--c back "a")))
  (let ((back (ygg-review-file-parse "---\nrepo: o/r\npr: 12\n::: {.c #a}\nhi\n:::\n")))
    (should (ygg-review-file-tests--problem back "not closed" 1))
    (should (equal (plist-get back :pr) 12))
    (should (ygg-review-file-tests--c back "a"))))

(ert-deftest ygg-review-file-hand-edit-an-unclosed-div-closes-at-the-next-opener-or-quote ()
  (let* ((text (ygg-review-file-tests--hand-text)))
    (let* ((edited (ygg-review-file-tests--sub text "first\n:::\n" "first\n"))
           (back (ygg-review-file-parse edited)))
      (should (ygg-review-file-tests--problem back "unclosed" (ygg-review-file-tests--line-of edited "{.c #c1")))
      (should (equal (plist-get (ygg-review-file-tests--c back "c1") :text) "first"))
      (should (= 3 (length (plist-get back :comments))))
      (should (equal (ygg-review-file-diff edited) ygg-review-file-tests--hand-diff)))
    (let* ((edited (ygg-review-file-tests--sub text "second\nline2\n:::\n" "second\nline2\n"))
           (back (ygg-review-file-parse edited)))
      (should (ygg-review-file-tests--problem back "unclosed"))
      (should (equal (plist-get (ygg-review-file-tests--c back "c2") :text) "second\nline2"))
      (should (equal (plist-get (ygg-review-file-tests--c back "r1") :text) "range")))
    (let* ((back (ygg-review-file-parse (concat text "::: {.c #zz}\nunterminated\n\n"))))
      (should (ygg-review-file-tests--problem back "unclosed"))
      (should (equal (plist-get (ygg-review-file-tests--c back "zz") :text) "unterminated")))
    (let* ((edited (ygg-review-file-tests--sub text "line2\n:::\n" "line2\n::: nonsense\n"))
           (back (ygg-review-file-parse edited)))
      (should (ygg-review-file-tests--problem back "unclosed")))))

(ert-deftest ygg-review-file-a-quote-like-line-inside-a-closed-body-stays-text ()
  (let* ((text "> a\n::: {.c #x}\n> not a quote\nmore\n:::\n> b\n")
         (back (ygg-review-file-parse text)))
    (should (equal (plist-get (car (plist-get back :comments)) :text) "> not a quote\nmore"))
    (should-not (plist-get back :problems))
    (should (equal (ygg-review-file-diff text) "a\nb\n"))))

(ert-deftest ygg-review-file-hand-edit-crlf-is-tolerated ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (crlf (replace-regexp-in-string "\n" "\r\n" text))
         (back (ygg-review-file-parse crlf)))
    (should (equal (ygg-review-file-tests--without-problems back)
                   (ygg-review-file-tests--without-problems (ygg-review-file-parse text))))
    (should-not (plist-get back :problems))
    (should (equal (ygg-review-file-diff crlf) ygg-review-file-tests--hand-diff))
    (should (equal (plist-get (ygg-review-file-tests--c back "c2") :text) "second\nline2")))
  (let* ((c (list :id "r" :level 'review :text "a\r\nb\rc"))
         (text (ygg-review-file-print (list :comments (list c)) ""))
         (back (ygg-review-file-parse (replace-regexp-in-string "\n" "\r\n" text))))
    (should (string-search "text=\"a\\r\\nb\\rc\"" text))
    (should (equal (plist-get (car (plist-get back :comments)) :text) "a\r\nb\rc"))))

(ert-deftest ygg-review-file-hand-edit-indented-and-bare-class-fences-are-comments ()
  (let* ((text (ygg-review-file-tests--hand-text)))
    (let ((back (ygg-review-file-parse
                 (ygg-review-file-tests--sub
                  (ygg-review-file-tests--sub text "::: {.c #c1" "  ::: {.c #c1")
                  "first\n:::" "first\n  :::"))))
      (should-not (plist-get back :problems))
      (should (equal (plist-get (ygg-review-file-tests--c back "c1") :text) "first")))
    (dolist (opener '("::: c" ":::{.c}" "::: {.comment}" "\t::: {.c}" "::: comment"))
      (let* ((back (ygg-review-file-parse
                    (ygg-review-file-tests--sub text ">  two\n" (format ">  two\n%s\nnew\n:::\n" opener))))
             (new (seq-find (lambda (c) (equal (plist-get c :text) "new")) (plist-get back :comments))))
        (should-not (plist-get back :problems))
        (should (equal (list (plist-get new :level) (plist-get new :side) (plist-get new :line))
                       '(line new 2)))))
    (let ((back (ygg-review-file-parse (ygg-review-file-tests--sub text ">  two\n" ">  two\n::: {.note}\nhm\n:::\n"))))
      (should (ygg-review-file-tests--problem back "unknown div class note")))))

(ert-deftest ygg-review-file-stray-text-is-review-text-before-the-diff-and-a-problem-after ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (before (ygg-review-file-parse
                  (ygg-review-file-tests--sub text "> diff --git" "Overall fine.\n\nSecond para.\n> diff --git")))
         (after (ygg-review-file-parse (ygg-review-file-tests--sub text ">  two\n" ">  two\nfree text here\n"))))
    (should (equal (plist-get before :text) "Overall fine.\n\nSecond para."))
    (should-not (plist-get before :problems))
    (should-not (plist-member after :text))
    (should (ygg-review-file-tests--problem
             after "stray text.*free text here" (ygg-review-file-tests--line-of
                                                 (ygg-review-file-tests--sub text ">  two\n" ">  two\nfree text here\n")
                                                 "free text here")))
    (should (= 3 (length (plist-get after :comments)))))
  (let ((back (ygg-review-file-parse "intro\n::: {.summary}\nsum\n:::\nafter\n")))
    (should (equal (plist-get back :text) "intro\n\nsum\n\nafter"))))

(ert-deftest ygg-review-file-a-bare-comment-gets-an-id-and-the-time ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (before (float-time))
         (back (ygg-review-file-parse (ygg-review-file-tests--sub text ">  one\n" ">  one\n::: {.c}\nbrand new\n:::\n")))
         (new (seq-find (lambda (c) (equal (plist-get c :text) "brand new")) (plist-get back :comments))))
    (should (stringp (plist-get new :id)))
    (should (string-match-p "\\`[0-9a-f]+\\'" (plist-get new :id)))
    (should-not (member (plist-get new :id) '("c1" "c2" "r1")))
    (should-not (plist-get new :status))
    (should (<= before (plist-get new :created) (+ (float-time) 1)))
    (should (equal (list (plist-get new :level) (plist-get new :line)) '(line 1)))
    (should-not (plist-get back :problems))
    (let* ((again (ygg-review-file-parse (ygg-review-file-tests--sub text ">  one\n" ">  one\n::: {.c}\nA\n:::\n::: {.c}\nB\n:::\n")))
           (ids (mapcar (lambda (c) (plist-get c :id)) (plist-get again :comments))))
      (should (= (length ids) (length (delete-dups (copy-sequence ids))))))))

(ert-deftest ygg-review-file-duplicate-ids-are-reported-and-the-second-renamed ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (edited (ygg-review-file-tests--sub text "#r1" "#c1"))
         (back (ygg-review-file-parse edited))
         (ids (mapcar (lambda (c) (plist-get c :id)) (plist-get back :comments))))
    (should (ygg-review-file-tests--problem back "duplicate id c1" (ygg-review-file-tests--line-of edited "span=2")))
    (should (= 3 (length (delete-dups (copy-sequence ids)))))
    (should (equal (plist-get (ygg-review-file-tests--c back "c1") :text) "first"))
    (should (seq-find (lambda (c) (equal (plist-get c :text) "range")) (plist-get back :comments)))))

(ert-deftest ygg-review-file-a-live-diff-flags-quoted-lines-and-quotes-that-no-longer-match ()
  (let* ((text (ygg-review-file-tests--hand-text))
         (live ygg-review-file-tests--hand-diff)
         (same (ygg-review-file-parse text live))
         (edited (ygg-review-file-tests--sub live " one\n" " ONE\n"))
         (back (ygg-review-file-parse text edited))
         (short (ygg-review-file-parse text (ygg-review-file-tests--sub live " four\n" ""))))
    (should-not (plist-get same :problems))
    (should (ygg-review-file-tests--problem back "differs from the live diff.* one"
                                            (ygg-review-file-tests--line-of text ">  one")))
    (should (ygg-review-file-tests--problem back "quote of c1 no longer matches"
                                            (ygg-review-file-tests--line-of text "{.c #c1")))
    (should (ygg-review-file-tests--problem short "four\\|more lines"))
    (should (ygg-review-file-tests--problem
             (ygg-review-file-parse (ygg-review-file-tests--sub text ">  one\n" ">  one\n>  inserted\n") live)
             "inserted"))
    (should (equal (ygg-review-file-tests--without-problems back)
                   (ygg-review-file-tests--without-problems (ygg-review-file-parse text))))))

(ert-deftest ygg-review-file-a-live-diff-accepts-a-snipped-quote ()
  (let* ((diff (ygg-review-file-tests--diff 20))
         (recs (ygg-review-file--records diff))
         (text (ygg-review-file-print
                (list :comments (list (ygg-review-file-tests--derived
                                       recs (ygg-review-file-tests--rec-at-index recs 'new 38) "n" nil :text "x")))
                diff)))
    (should (string-search "[..." text))
    (should-not (plist-get (ygg-review-file-parse text diff) :problems))))

(ert-deftest ygg-review-file-a-blank-context-line-whose-space-was-stripped-is-tolerated ()
  (let* ((d (concat "diff --git a/a.txt b/a.txt\n--- a/a.txt\n+++ b/a.txt\n@@ -1,5 +1,5 @@\n"
                    " one\n \n-two\n+two!\n \n five\n"))
         (recs (ygg-review-file--records d))
         (i (cl-position-if (lambda (r) (equal (plist-get r :text) "+two!")) recs))
         (review (list :comments (list (ygg-review-file-tests--derived recs i "c1" nil :text "x"))))
         (text (ygg-review-file-print review d))
         (stripped (replace-regexp-in-string "[ \t]+$" "" text))
         (back (ygg-review-file-parse stripped)))
    (should-not (equal text stripped))
    (should (equal (ygg-review-file-diff stripped) d))
    (should (equal (plist-get (car (plist-get back :comments)) :quote) " \n-two\n+two!\n \n five"))
    (should-not (plist-get back :problems))
    (ygg-review-file-tests--round-trip review d)))

(ert-deftest ygg-review-file-comments-come-back-sorted-by-created-then-file-order ()
  (let* ((text (concat "::: {.c #a created=3}\na\n:::\n\n::: {.c #b created=1}\nb\n:::\n\n"
                       "::: {.c #c}\nc\n:::\n\n::: {.c #d created=1}\nd\n:::\n\n::: {.c #e created=3}\ne\n:::\n"))
         (ids (mapcar (lambda (c) (plist-get c :id)) (plist-get (ygg-review-file-parse text) :comments))))
    (should (equal ids '("c" "b" "d" "a" "e")))
    (let ((printed (ygg-review-file-print (ygg-review-file-parse text) "")))
      (should (equal (mapcar (lambda (c) (plist-get c :id)) (plist-get (ygg-review-file-parse printed) :comments))
                     ids)))))

(ert-deftest ygg-review-file-thread-parse-reports-what-it-cannot-use ()
  (let ((back (ygg-review-file-parse
               "> a\n:::: {.thread #t}\nstray\n::: {.reply #r1 author=a}\nx\n:::\n::: {.other}\ny\n:::\n::::\n:::: {.thread #u}\n::::\n")))
    (should (= 1 (length (plist-get back :threads))))
    (should (ygg-review-file-tests--problem back "stray text in a thread: stray" 3))
    (should (ygg-review-file-tests--problem back "not a reply" 7))
    (should (ygg-review-file-tests--problem back "without replies"))
    (should (equal (plist-get (car (plist-get back :threads)) :id) "r1"))))

(defconst ygg-review-file-tests--malformed-attributes
  '("::: {.c #c1 bogus}\nx\n:::\n" "::: {.c #c1 type=\"abc}\nx\n:::\n" "::: {.c type=}\nx\n:::\n"
    "::: {.c #c1 type=\"abc\\}\nx\n:::\n" "::: {.c #c1 type=\"\\u12\"}\nx\n:::\n"
    "::: {.c #c1 type=\"\\xZZ\"}\nx\n:::\n" "::: {.c #c1\nx\n:::\n" "::: {\nx\n:::\n"
    "::: {}\nx\n:::\n" "::: {.c =x}\nx\n:::\n" "::: {.c #}\nx\n:::\n"
    "::: {.c . # = \" \\}\nx\n:::\n" "::: {.c span=0}\nx\n:::\n" "::: {.c span=-3}\nx\n:::\n"
    "> a\n::: {.c span=5}\nx\n:::\n" "> a\n::: {.c span=\"x\"}\nx\n:::\n"
    "::: {.c labels=~\"(\"}\nx\n:::\n" "::: {.c text=}\n:::\n")
  "Divs whose attributes cannot be read.")

(defconst ygg-review-file-tests--broken-structure
  '("---\nrepo: o/r\n" "---\n---\n" "---\n---" "---" "--- \n---\t\n" "---\nrepo: \"x\n---\n> a\n"
    "::: {.c #a}\nx\n" ":::\n" ":::\n:::\n:::: {.thread #t}\n" ":::: {.thread #t}\n::: {.reply}\nx\n"
    "::: {.c #a}\n::: {.c #b}\n::: {.c #c}\n" "> a\n::: {.c #a}\n> b\n::: {.c #b}\n> c\n"
    ":::: {.thread #t}\n::: {.reply}\n::::\n:::\n" ":::: {.thread #t}\n:::::: {.reply}\n::::::\n"
    "> a\n:::: {.thread}\n:::\n::::\n" "> a\n::: {.summary}\n> b\n:::\n" "::: {.summary}\n::: {.summary}\n"
    "::: {.thread #t}\n::: {.thread #u}\n::: {.reply}\n:::\n:::\n:::\n" "text\n> a\ntext\n:::\n> b\n"
    "\n\n\n" ">" ">\n>\n>" "> [... 5 lines]\n::: {.c #a}\nx\n:::\n" "[... 5 lines]\n> [... 99999999999999999999 lines]\n")
  "Files that end early, nest wrongly or hold fences with nothing to close.")

(defconst ygg-review-file-tests--odd-whitespace
  '("::: {.c #a}\r\nx\r\n:::\r\n" "> a\r\n> b\r\n::: {.c #a}\r\nx\r\n:::\r\n" "\r\n\r\n" "\r" "\r\r\n\n"
    "---\r\nrepo: o/r\r\n---\r\n" "---\r\nrepo: o/r\r\n" "  ::: {.c #a}\n  x\n  :::\n"
    "\t:::\t{.c\t#a}\t\nx\n\t:::\t\n" "::: {.c #a}\nx\n  :::  \n" ">\r\n::: c\r\nx\r\n:::\r\n"
    "> a\n\v::: {.c #a}\nx\n:::\n" "\u00a0::: {.c #a}\nx\n:::\n" "> a\n::: {.c #a}\u2028x\n:::\n"
    ":::{.c}\n:::\n" "::: c\n:::\n" "::: {.c #a}x\n:::\n" ":::: {.c #a}\n::: {.c #b}\n::::\n:::\n")
  "Files with CRLF, indentation and unusual whitespace.")

(defun ygg-review-file-tests--tolerates (text)
  (let* ((back (ygg-review-file-parse text))
         (live (ygg-review-file-parse text "diff --git a/a b/a\n"))
         (diff (ygg-review-file-diff text)))
    (should (listp (plist-get back :comments)))
    (should (listp (plist-get back :threads)))
    (should (listp (plist-get live :problems)))
    (should (or (null diff) (stringp diff)))
    (dolist (p (plist-get back :problems))
      (should (integerp (car p)))
      (should (stringp (cdr p)))
      (should-not (string-prefix-p "internal error" (cdr p))))
    (ygg-review-file-print back ygg-review-file-tests--hand-diff)
    back))

(ert-deftest ygg-review-file-malformed-attributes-never-signal-and-are-reported ()
  (dolist (text ygg-review-file-tests--malformed-attributes)
    (ert-info (text)
      (let ((back (ygg-review-file-tests--tolerates text)))
        (should (plist-get back :problems))))))

(ert-deftest ygg-review-file-broken-structure-never-signals ()
  (dolist (text ygg-review-file-tests--broken-structure)
    (ert-info (text)
      (ygg-review-file-tests--tolerates text))))

(ert-deftest ygg-review-file-odd-whitespace-never-signals-and-keeps-the-comment ()
  (dolist (text ygg-review-file-tests--odd-whitespace)
    (ert-info (text)
      (ygg-review-file-tests--tolerates text)))
  (dolist (text '("::: {.c #a}\r\nx\r\n:::\r\n" "  ::: {.c #a}\n  x\n  :::\n" "::: {.c #a}\nx\n  :::  \n"
                  "\t:::\t{.c\t#a}\t\nx\n\t:::\t\n"))
    (let ((back (ygg-review-file-parse text)))
      (should (= 1 (length (plist-get back :comments))))
      (should (equal (plist-get (car (plist-get back :comments)) :id) "a")))))

(ert-deftest ygg-review-file-never-signals-on-non-review-strings ()
  (dolist (text (list "" "\n" "\0" "😀" (make-string 5000 ?:) (make-string 3000 ?>)
                      (mapconcat #'identity (make-list 500 "::: {.c #a}") "\n")
                      (concat "> a\n" (mapconcat #'identity (make-list 500 ":::: {.thread #t}") "\n"))
                      (apply #'string (number-sequence 1 255))))
    (ygg-review-file-tests--tolerates text)))

(defun ygg-review-file-tests--mutate (text)
  (let* ((n (length text)) (at (random (max 1 n)))
         (alphabet (string-to-list ":{}\"\\=#>\n\r -~.0aZ\u00e9\u2028~(["))
         (pick (nth (random (length alphabet)) alphabet)))
    (pcase (random 6)
      (0 (concat (substring text 0 at) (substring text (min n (1+ at)))))
      (1 (concat (substring text 0 at) (string pick) (substring text at)))
      (2 (concat (substring text 0 at) (string pick) (substring text (min n (1+ at)))))
      (3 (substring text 0 at))
      (4 (let ((to (min n (+ at (random 80))))) (concat (substring text 0 to) (substring text at))))
      (_ (let ((to (min n (+ at 1 (random 40))))) (concat (substring text 0 at) (substring text to)))))))

(ert-deftest ygg-review-file-300-byte-level-mutations-never-signal ()
  (random "ygg-review-file-fuzz")
  (let* ((diff (ygg-review-file-tests--diff 3))
         (bases (list (ygg-review-file-tests--hand-text)
                      (ygg-review-file-print ygg-review-file-tests--golden-review
                                             ygg-review-file-tests--golden-diff)
                      (ygg-review-file-print (ygg-review-file-tests--random-review diff 1) diff)
                      (ygg-review-file-print (ygg-review-file-tests--random-review diff 2) diff))))
    (dotimes (_ 300)
      (let ((text (ygg-review-file-tests--pick bases)))
        (dotimes (_ (1+ (random 4))) (setq text (ygg-review-file-tests--mutate text)))
        (ert-info (text)
          (ygg-review-file-tests--tolerates text))))))

(ert-deftest ygg-review-file-print-after-parse-is-a-fixed-point ()
  (random "ygg-review-file-fixed")
  (dolist (context '(3 20))
    (let ((diff (ygg-review-file-tests--diff context)))
      (dotimes (n 150)
        (let* ((review (ygg-review-file-tests--random-review diff n))
               (once (ygg-review-file-print review diff))
               (twice (ygg-review-file-print (ygg-review-file-parse once) diff)))
          (ert-info (once)
            (should (equal once twice))))))))

(ert-deftest ygg-review-file-print-after-parse-is-a-fixed-point-for-crowded-ranges ()
  (let* ((ygg-review-file-snip-threshold nil)
         (diff (ygg-review-file-tests--diff 20))
         (recs (ygg-review-file--records diff))
         (new (lambda (n) (aref recs (ygg-review-file-tests--rec-at-index recs 'new n))))
         (range (lambda (id a b &rest props)
                  (append (list :id id :text id) props
                          (ygg-review-file-tests--range-anchor (funcall new a) (funcall new b)))))
         (comments (list (funcall range "a" 5 10 :created 5) (funcall range "b" 5 10 :created 1)
                         (funcall range "c" 7 10 :created 3) (funcall range "d" 3 10)
                         (funcall range "e" 8 9 :quote "custom")
                         (ygg-review-file-tests--comment-on (funcall new 10) :id "f" :created 2)
                         (ygg-review-file-tests--comment-on (funcall new 7) :id "g")))
         (once (ygg-review-file-print (list :comments comments) diff))
         (twice (ygg-review-file-print (ygg-review-file-parse once) diff)))
    (should (equal once twice))
    (ygg-review-file-tests--round-trip (list :comments comments) diff)))

(defun ygg-review-file-tests--big-diff (changes)
  (let ((lines nil) (old 0) (new 0))
    (dotimes (k (* changes 50))
      (if (zerop (% (1+ k) 50))
          (progn (push (format "-old %d" k) lines) (push (format "+new %d" k) lines)
                 (cl-incf old) (cl-incf new))
        (push (format " line %d" k) lines) (cl-incf old) (cl-incf new)))
    (concat "diff --git a/big.txt b/big.txt\nindex 1..2 100644\n--- a/big.txt\n+++ b/big.txt\n"
            (format "@@ -1,%d +1,%d @@\n" old new)
            (string-join (nreverse lines) "\n") "\n")))

(ert-deftest ygg-review-file-print-time-with-crowded-ranges-on-a-big-diff ()
  (let* ((diff (ygg-review-file-tests--big-diff 100))
         (recs (ygg-review-file--records diff))
         (content (cl-loop for r across recs for i from 0
                           when (ygg-review-file--content-p r) collect i))
         (ranges (cl-loop for k below 100
                          collect (let ((a (nth (* k 40) content)) (b (nth (+ (* k 40) 200) content)))
                                    (append (list :id (format "r%d" k) :text "range"
                                                  :created (+ 1790000000 k))
                                            (ygg-review-file-tests--range-anchor (aref recs a) (aref recs b))))))
         (lines (cl-loop for k below 100
                         collect (ygg-review-file-tests--comment-on
                                  (aref recs (nth (+ 5 (* k 45)) content)) :id (format "l%d" k))))
         (review (list :comments (append ranges lines)))
         (start (float-time))
         (text (ygg-review-file-print review diff))
         (elapsed (- (float-time) start)))
    (should (>= (length (ygg-review-file--lines diff)) 5000))
    (should (< elapsed 10.0))
    (should (= 200 (length (plist-get (ygg-review-file-parse text) :comments))))
    (should-not (plist-get (ygg-review-file-parse text) :problems))))

(defun ygg-review-file-tests--sample ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (content (cl-loop for r across recs for i from 0 when (ygg-review-file--content-p r) collect i))
         (pick (lambda (n) (nth n content)))
         (range "main...feature")
         (line (lambda (n id &rest props)
                 (apply #'ygg-review-file-tests--derived recs (funcall pick n) id nil props)))
         (span (lambda (a b id &rest props)
                 (append (list :id id) props
                         (ygg-review-file-tests--range-anchor (aref recs (funcall pick a)) (aref recs (funcall pick b)))
                         (list :quote (ygg-review-file--join recs (funcall pick a) (funcall pick b)))))))
    (list
     (list :repo "github.com/o/r" :pr 12 :base "9f8e7d6" :head "1a2b3c4" :round 2
           :verdict 'request-changes :range range :text "Overall: close."
           :comments
           (list (funcall line 4 "c1" :type 'nit :author "you" :created 1790000000.5 :status 'pending
                          :range range :text "Name this constant.")
                 (funcall line 6 "c2" :type 'issue :priority 1 :author "you" :created 1790000010.5
                          :range range :text "Empty input case?")
                 (funcall line 9 "c3" :type 'fix :author "codex" :status 'pending :confidence 0.7
                          :created 1790000020.5 :range range :text "Use a helper.")
                 (funcall span 3 8 "r1" :type 'issue :author "you" :created 1790000030.5
                          :range range :text "Refactor this whole block.")
                 (funcall span 5 10 "r2" :type 'question :author "you" :created 1790000040.5
                          :range range :text "Overlaps r1.")
                 (funcall line 7 "c4" :type 'nit :author "you" :created 1790000050.5
                          :range range :text "Inside r1 and r2.")
                 (funcall line 12 "c5" :type 'nit :author "you" :created 1790000060.5
                          :range range :text "typo")
                 (funcall line 14 "c6" :type 'question :author "you" :created 1790000070.5
                          :range range :text "why?")
                 (list :id "f1" :level 'file :file "b.txt" :old-path "b.txt" :new-path "b.txt"
                       :type 'issue :author "you" :created 1790000080.5 :range range
                       :text "Add tests for b.")
                 (list :id "rv" :level 'review :type 'issue :priority 1 :author "you"
                       :created 1790000090.5 :range range :text "Overall: retry logic is close."))
           :threads
           (list (list :remote t :id "remote:gh:8812" :thread "8812" :author "alice"
                       :created 1790000100.0 :text "Why 5 here?"
                       :url "https://github.com/o/r/pull/1#discussion_r8812"
                       :diff-hunk "@@ -1,3 +1,3 @@\n 1\n+two\n+three" :file "a.txt"
                       :new-path "a.txt" :level 'range :side 'new :line 3 :start-line 2
                       :orig-line 3 :resolved nil :outdated nil)
                 (list :remote t :id "remote:gh:8813" :thread "8812" :author "bob"
                       :created 1790000200.0 :text "Matches the config default."
                       :url "https://github.com/o/r/pull/1#discussion_r8813"
                       :diff-hunk "@@ -1,3 +1,3 @@\n 1\n+two\n+three" :file "a.txt"
                       :new-path "a.txt" :level 'range :side 'new :line 3 :start-line 2
                       :orig-line 3 :resolved nil :outdated nil)
                 (list :remote t :id "remote:gl:1" :thread "d1" :author "dave"
                       :created 1790000400.0 :text "nit: rename"
                       :url "https://gitlab.com/o/r/-/merge_requests/1#note_1" :file "a.txt"
                       :new-path "a.txt" :level 'line :side 'new :line 20 :resolved t :outdated nil
                       :depth 1))))))

(ert-deftest ygg-review-file-realistic-sample-round-trips-in-a-compact-form ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (review (car (ygg-review-file-tests--sample)))
         (text (ygg-review-file-tests--round-trip review diff))
         (lines (split-string text "\n"))
         (comment-lines (seq-filter (lambda (l) (string-prefix-p "::: {.c" l)) lines))
         (thread-lines (seq-filter (lambda (l) (string-prefix-p ":::: {.thread" l)) lines))
         (lengths (sort (mapcar #'length comment-lines) #'<)))
    (should (equal text (ygg-review-file-print (ygg-review-file-parse text) diff)))
    (should (<= (car (last lengths)) 110))
    (should (string-search "\nauthor: you\n---\n" text))
    (should-not (seq-find (lambda (l) (string-search "author=you" l)) comment-lines))
    (should (cl-every (lambda (l) (or (string-search "diff-hunk=\"" l) (<= (length l) 80))) thread-lines))
    (dolist (word '("side=" "line=" "file=" "old-path=" "new-path=" "level=" "remote=" "start-"))
      (should-not (seq-find (lambda (l) (and (string-prefix-p "::: {.c" l) (string-search word l))) lines)))))

(ert-deftest ygg-review-file-range-comments-over-mixed-sides-round-trip-and-reprint-identically ()
  (let* ((d (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records d))
         (idx (cl-loop for r across recs for i from 0 when (ygg-review-file--content-p r) collect i))
         (pk (lambda (n) (nth n idx)))
         (rng (lambda (a b id)
                (append (list :id id :text id)
                        (ygg-review-file-tests--range-anchor (aref recs (funcall pk a)) (aref recs (funcall pk b)))
                        (list :quote (ygg-review-file--join recs (funcall pk a) (funcall pk b))))))
         (ln (lambda (n id) (ygg-review-file-tests--derived recs (funcall pk n) id nil :text id))))
    (dolist (comments (list (list (funcall rng 1 6 "a") (funcall rng 3 6 "b"))
                            (list (funcall rng 1 6 "a") (funcall rng 1 8 "b"))
                            (list (funcall rng 1 6 "a") (funcall rng 1 6 "b"))
                            (list (funcall rng 1 9 "a") (funcall rng 3 6 "b"))
                            (list (funcall rng 1 3 "a") (funcall rng 4 6 "b"))
                            (list (funcall rng 2 6 "a") (funcall ln 2 "l"))
                            (list (funcall rng 2 6 "a") (funcall ln 6 "l"))
                            (list (funcall rng 1 5 "a") (funcall rng 3 8 "b"))
                            (list (funcall ln 2 "a") (funcall ln 2 "b"))
                            (list (funcall rng 1 (1- (length idx)) "a"))))
      (let* ((review (list :comments comments))
             (text (ygg-review-file-tests--round-trip review d)))
        (should (equal text (ygg-review-file-print (ygg-review-file-parse text) (ygg-review-file-diff text))))))))

(ert-deftest ygg-review-file-comments-around-a-snip-marker-do-not-signal ()
  (let* ((d (ygg-review-file-tests--diff 20))
         (text (ygg-review-file-print (list :comments nil) d))
         (before (replace-regexp-in-string "^> \\[\\.\\.\\. " "::: {.c #sn}\nhi\n:::\n> [... " text))
         (after (replace-regexp-in-string "\\(^> \\[\\.\\.\\. [0-9]+ lines\\]\n\\)" "\\1::: {.c #sn}\nhi\n:::\n" text)))
    (should (ygg-review-file-tests--c (ygg-review-file-parse before) "sn"))
    (let ((back (ygg-review-file-parse after)))
      (should (ygg-review-file-tests--c back "sn"))
      (should (ygg-review-file-tests--problem back "snip marker")))))

(ert-deftest ygg-review-file-hand-edit-table ()
  (let* ((base (ygg-review-file-tests--hand-text))
         (diff ygg-review-file-tests--hand-diff)
         (plain (ygg-review-file-tests--without-problems (ygg-review-file-parse base)))
         (parse #'ygg-review-file-parse)
         (sub #'ygg-review-file-tests--sub)
         (lines (lambda (back) (mapcar (lambda (c) (list (plist-get c :id) (plist-get c :line)))
                                       (plist-get back :comments)))))
    (let* ((edited (funcall sub base ">  two\n" ">  two\n>  inserted\n"))
           (back (funcall parse edited)))
      (should (equal (mapcar #'car (plist-get back :problems))
                     (list (ygg-review-file-tests--line-of edited ">  five"))))
      (should (ygg-review-file-tests--problem back "outside any hunk"))
      (should-not (equal (ygg-review-file-diff edited) diff))
      (should (equal (funcall lines back) '(("c1" 3) ("c2" 5) ("r1" 5))))
      (should (equal (plist-get (ygg-review-file-tests--c back "r1") :start-line) 4)))
    (let* ((edited (funcall sub base "::: {.c #c1" "my note\n::: {.c #c1"))
           (back (funcall parse edited)))
      (should (ygg-review-file-tests--problem back "stray text outside a comment: my note"
                                              (ygg-review-file-tests--line-of edited "my note")))
      (should (equal (ygg-review-file-tests--without-problems back) plain)))
    (should (equal (ygg-review-file-tests--without-problems (funcall parse (string-trim-right base))) plain))
    (let ((back (funcall parse "")))
      (should-not (plist-get back :problems))
      (should-not (plist-get back :comments))
      (should-not (ygg-review-file-diff "")))
    (let ((back (funcall parse (funcall sub base "#c1" "#c1 level=file"))))
      (should (equal (list (plist-get (ygg-review-file-tests--c back "c1") :level)
                           (plist-get (ygg-review-file-tests--c back "c1") :line))
                     '(file 2))))
    (let ((back (funcall parse (funcall sub base "#c1" "#c1 level=range"))))
      (should (ygg-review-file-tests--problem back "range comment without a span")))
    (let* ((back (funcall parse (funcall sub base "#c1 " "")))
           (fresh (seq-find (lambda (c) (equal (plist-get c :text) "first")) (plist-get back :comments))))
      (should-not (plist-get back :problems))
      (should (= 3 (length (plist-get back :comments))))
      (should (equal (plist-get fresh :line) 2))
      (should (floatp (plist-get fresh :created))))
    (let ((back (funcall parse (concat base ":::\ntext\n"))))
      (should (ygg-review-file-tests--problem back "closing fence without an open div"))
      (should (ygg-review-file-tests--problem back "stray text outside a comment: text")))
    (dolist (opener '("\t::: {.c #c1" ":::{.c #c1" "   ::: {.c #c1"))
      (should (equal (ygg-review-file-tests--without-problems
                      (funcall parse (funcall sub base "::: {.c #c1" opener)))
                     plain)))
    (let* ((back (funcall parse (funcall sub base ">  two\n"
                                         ">  two\n:::: {.c #x}\n::: {.c #y}\nnested\n:::\nbody\n::::\n")))
           (x (ygg-review-file-tests--c back "x")))
      (should-not (plist-get back :problems))
      (should (equal (plist-get x :text) "::: {.c #y}\nnested\n:::\nbody"))
      (should-not (ygg-review-file-tests--c back "y")))
    (should (equal (plist-get (ygg-review-file-tests--c
                               (funcall parse (funcall sub base "first\n:::" "first\n\n\n:::")) "c1")
                              :text)
                   "first\n\n"))
    (should (equal (plist-get (ygg-review-file-tests--c
                               (funcall parse (funcall sub base "first\n:::" "  first\n   indented\n:::")) "c1")
                              :text)
                   "  first\n   indented"))
    (should (equal (ygg-review-file-tests--without-problems
                    (funcall parse (funcall sub base ">  two\n" ">  two\n\n\n")))
                   plain))
    (let ((back (funcall parse "> a\n:::: {.thread #t}\n::: {.reply}\nx\n:::\n")))
      (should (ygg-review-file-tests--problem back "unclosed" 2))
      (should (equal (plist-get (car (plist-get back :threads)) :text) "x")))
    (should (ygg-review-file-tests--problem (funcall parse ":::: {.thread #t}\n::::\n") "without replies" 1))))

(ert-deftest ygg-review-file-a-diff-line-ending-in-cr-survives-an-lf-file ()
  (let* ((d (concat "diff --git a/a b/a\n--- a/a\n+++ b/a\n@@ -1,2 +1,2 @@\n-old\r\n+new\r\n keep\r\n"))
         (recs (ygg-review-file--records d))
         (review (list :comments (list (ygg-review-file-tests--derived
                                        recs (ygg-review-file-tests--idx recs 'new 1) "c" nil :text "x"))))
         (text (ygg-review-file-print review d)))
    (should (equal (ygg-review-file-diff text) d))
    (should (equal (ygg-review-file-diff (replace-regexp-in-string "\n" "\r\n" text)) d))
    (ygg-review-file-tests--round-trip review d)))

(ert-deftest ygg-review-file-a-range-never-crosses-into-another-diff-section ()
  (let* ((d (concat "diff --git a/a.txt b/a.txt\n--- a/a.txt\n+++ b/a.txt\n@@ -1 +0,0 @@\n-gone\n"
                    "diff --git a/a.txt b/a.txt\n--- a/a.txt\n+++ b/a.txt\n@@ -0,0 +5 @@\n+here\n"))
         (c (list :id "x" :level 'range :file "a.txt" :old-path "a.txt" :new-path "a.txt"
                  :start-side 'old :start-line 1 :side 'new :line 5 :text "across"))
         (text (ygg-review-file-print (list :comments (list c)) d))
         (back (ygg-review-file-parse text)))
    (should-not (string-search "span=" text))
    (should-not (plist-get back :problems))
    (should-not (ygg-review-file-tests--differences c (car (plist-get back :comments))))))

(defun ygg-review-file-tests--created (token)
  (let ((back (ygg-review-file-parse
               (format "::: {.c #a created=%s}\nx\n:::\n::: {.c #b created=1.5}\ny\n:::\n" token))))
    (cons (ygg-review-file-tests--c back "a") (plist-get back :problems))))

(defun ygg-review-file-tests--utc (&rest fields)
  (time-convert (encode-time (append (list (nth 5 fields) (nth 4 fields) (nth 3 fields)
                                           (nth 2 fields) (nth 1 fields) (nth 0 fields))
                                     '(nil -1 t)))
                'integer))

(ert-deftest ygg-review-file-created-with-an-offset-or-no-zone-is-converted-to-utc ()
  (let ((base (ygg-review-file-tests--utc 2026 2 3 4 5 6)))
    (dolist (case (list (list "2026-02-03T04:05:06Z" base)
                        (list "2026-02-03T04:05:06" base)
                        (list "2026-02-03T07:05:06+03:00" base)
                        (list "2026-02-03T04:05:06+00:00" base)
                        (list "2026-02-02T22:35:06-05:30" base)
                        (list "2026-02-03T04:05:06.250" (+ base 0.25))
                        (list "2026-02-03T07:05:06.250+03:00" (+ base 0.25))
                        (list "2026-02-02T22:35:06.250-0530" (+ base 0.25))
                        (list "2026-02-03T07:05:06+0300" base)))
      (let ((got (ygg-review-file-tests--created (car case))))
        (should-not (cdr got))
        (should (equal (plist-get (car got) :created) (nth 1 case)))))))

(ert-deftest ygg-review-file-created-that-is-not-a-valid-time-is-a-problem-and-dropped ()
  (dolist (token '("2026-12-31T23:59:60Z" "2026-02-30T00:00:00Z" "2026-02-03T24:00:00Z" "2026-02-03"
                   "10000-01-01T00:00:00Z" "2026-02-03T04:05:06+24:00" "2026-02-03T04:05:06+03:60"
                   "2026-02-03T04:05:06+03" "2026-02-03t04:05:06z"
                   "yesterday" "\"yesterday\"" ":soon" "true"))
    (let ((got (ygg-review-file-tests--created token)))
      (should (equal (mapcar #'cdr (cdr got)) '("bad value for created, ignored")))
      (should-not (plist-member (car got) :created))
      (should (plist-get (car got) :text))))
  (let ((got (ygg-review-file-tests--created "\"2026-02-03T07:05:06+03:00\"")))
    (should-not (cdr got))
    (should (= (plist-get (car got) :created) (ygg-review-file-tests--utc 2026 2 3 4 5 6)))))

(ert-deftest ygg-review-file-created-is-never-a-string-after-parsing ()
  (let ((back (ygg-review-file-parse
               (concat "---\ncreated: nonsense\n---\n"
                       "::: {.c #a created=\"x\"}\n1\n:::\n::: {.c #b created=null}\n2\n:::\n"
                       ":::: {.thread #t}\n::: {.reply #r created=soon}\n3\n:::\n::::\n"))))
    (should-not (seq-find (lambda (c) (stringp (plist-get c :created)))
                          (append (plist-get back :comments) (plist-get back :threads))))
    (should-not (stringp (plist-get back :created)))
    (should (= 3 (length (plist-get back :problems))))))

(defun ygg-review-file-tests--body (name changed)
  (mapconcat (lambda (k) (if (and changed (= k 4)) (format "%s changed\n" name) (format "%s line %d\n" name k)))
             (number-sequence 1 7) ""))

(defvar ygg-review-file-tests--odd-diffs (make-hash-table :test 'equal))

(defun ygg-review-file-tests--odd-paths-diff (kind quotepath)
  (or (gethash (cons kind quotepath) ygg-review-file-tests--odd-diffs)
      (puthash
       (cons kind quotepath)
       (let* ((process-environment
               (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1") process-environment))
              (coding-system-for-read 'utf-8) (coding-system-for-write 'utf-8)
              (file-name-coding-system 'utf-8) (default-file-name-coding-system 'utf-8)
              (dir (file-name-as-directory (make-temp-file "ygg-review-file-odd-" t))))
         (unwind-protect
             (cl-flet ((git (&rest args) (apply #'ygg-review-file-tests--git dir args))
                       (write (file text) (with-temp-file (expand-file-name file dir) (insert text))))
               (git "init" "-q" "-b" "main")
               (git "config" "user.name" "T")
               (git "config" "user.email" "t@example.invalid")
               (git "config" "commit.gpgsign" "false")
               (dolist (name '("with space.txt" "tab\tx.txt" "é.txt" "sp ace é.txt"))
                 (write name (ygg-review-file-tests--body name nil)))
               (git "add" ".")
               (git "commit" "-q" "-m" "base")
               (if (eq kind 'edit)
                   (dolist (name '("with space.txt" "tab\tx.txt" "é.txt" "sp ace é.txt"))
                     (write name (ygg-review-file-tests--body name t)))
                 (dolist (pair '(("with space.txt" . "moved with space.txt") ("tab\tx.txt" . "tab\ty.txt")
                                 ("é.txt" . "ü é.txt") ("sp ace é.txt" . "ö.txt")))
                   (rename-file (expand-file-name (car pair) dir) (expand-file-name (cdr pair) dir))
                   (write (cdr pair) (ygg-review-file-tests--body (car pair) t))))
               (git "add" "-A")
               (git "commit" "-q" "-m" "head")
               (git "-c" (format "core.quotepath=%s" (if quotepath "true" "false"))
                    "diff" "--no-color" "--no-ext-diff" "-M" "-U1" "HEAD~1" "HEAD"))
           (delete-directory dir t)))
       ygg-review-file-tests--odd-diffs)))

(defun ygg-review-file-tests--path-pairs (diff)
  (let (pairs)
    (cl-loop for rec across (ygg-review-file--records diff)
             when (plist-get rec :new-path)
             do (cl-pushnew (cons (plist-get rec :old-path) (plist-get rec :new-path)) pairs :test #'equal))
    (sort pairs (lambda (a b) (string< (cdr a) (cdr b))))))

(ert-deftest ygg-review-file-paths-from-real-git-with-spaces-tabs-and-non-ascii-are-read-exactly ()
  (let ((edits '(("sp ace é.txt" . "sp ace é.txt") ("tab\tx.txt" . "tab\tx.txt")
                 ("with space.txt" . "with space.txt") ("é.txt" . "é.txt")))
        (renames '(("tab\tx.txt" . "tab\ty.txt") ("sp ace é.txt" . "ö.txt")
                   ("with space.txt" . "moved with space.txt") ("é.txt" . "ü é.txt"))))
    (dolist (quotepath '(t nil))
      (let ((got-edit (ygg-review-file-tests--path-pairs (ygg-review-file-tests--odd-paths-diff 'edit quotepath)))
            (got-rename (ygg-review-file-tests--path-pairs (ygg-review-file-tests--odd-paths-diff 'rename quotepath))))
        (should (equal got-edit (sort (copy-sequence edits) (lambda (a b) (string< (cdr a) (cdr b))))))
        (should (equal got-rename (sort (copy-sequence renames) (lambda (a b) (string< (cdr a) (cdr b))))))))))

(ert-deftest ygg-review-file-the-diff-header-forms-git-writes-for-odd-paths-parse-alike ()
  (dolist (case '(("diff --git a/with space.txt b/with space.txt" "--- a/with space.txt\t" "+++ b/with space.txt\t"
                   "with space.txt" "with space.txt")
                  ("diff --git \"a/tab\\tx.txt\" \"b/tab\\tx.txt\"" "--- \"a/tab\\tx.txt\"" "+++ \"b/tab\\tx.txt\""
                   "tab\tx.txt" "tab\tx.txt")
                  ("diff --git \"a/\\303\\251.txt\" \"b/\\303\\251.txt\"" "--- \"a/\\303\\251.txt\""
                   "+++ \"b/\\303\\251.txt\"" "é.txt" "é.txt")
                  ("diff --git a/é.txt b/é.txt" "--- a/é.txt" "+++ b/é.txt" "é.txt" "é.txt")
                  ("diff --git a/a b.txt \"b/q\\\"uote.txt\"" "--- a/a b.txt\t" "+++ \"b/q\\\"uote.txt\""
                   "a b.txt" "q\"uote.txt")))
    (let ((pairs (ygg-review-file-tests--path-pairs
                  (concat (string-join (list (nth 0 case) (nth 1 case) (nth 2 case) "@@ -1 +1 @@" "-a" "+b") "\n")
                          "\n"))))
      (should (equal pairs (list (cons (nth 3 case) (nth 4 case))))))))

(ert-deftest ygg-review-file-comments-on-files-with-odd-paths-stay-anchored-and-match-the-live-diff ()
  (dolist (kind '(edit rename))
    (dolist (quotepath '(t nil))
      (let* ((diff (ygg-review-file-tests--odd-paths-diff kind quotepath))
             (recs (ygg-review-file--records diff))
             (comments (cl-loop for rec across recs for i from 0
                                when (and (eq (plist-get rec :kind) 'add))
                                collect (ygg-review-file-tests--derived recs i (format "c%d" i) nil :text "x")))
             (text (ygg-review-file-print (list :comments comments) diff))
             (back (ygg-review-file-parse text diff)))
        (should (= 4 (length comments)))
        (should-not (string-search "level=" text))
        (should-not (plist-get back :problems))
        (ygg-review-file-tests--compare-lists "comment" (ygg-review-file-tests--by-id comments)
                                              (ygg-review-file-tests--by-id (plist-get back :comments)))
        (should (equal (ygg-review-file-diff text) diff))))))

(ert-deftest ygg-review-file-bare-comment-has-no-status-and-numeric-ids-become-strings ()
  (let* ((back (ygg-review-file-parse "::: {.c}\nx\n:::\n::: {.c id=42}\ny\n:::\n::: {.c #43}\nz\n:::\n"))
         (comments (plist-get back :comments)))
    (should-not (plist-get back :problems))
    (should-not (seq-find (lambda (c) (plist-member c :status)) comments))
    (should (member "42" (mapcar (lambda (c) (plist-get c :id)) comments)))
    (should (member "43" (mapcar (lambda (c) (plist-get c :id)) comments)))
    (should (seq-every-p (lambda (c) (stringp (plist-get c :id))) comments))))

(defconst ygg-review-file-tests--tiny-diff
  "diff --git a/f b/f\n--- a/f\n+++ b/f\n@@ -1 +1,2 @@\n x\n+y\n")

(defun ygg-review-file-tests--tiny (div &optional extra)
  (ygg-review-file-parse
   (concat (mapconcat (lambda (l) (concat "> " l "\n")) (ygg-review-file--lines ygg-review-file-tests--tiny-diff) "")
           div extra)))

(ert-deftest ygg-review-file-span-of-one-is-a-line-comment-and-span-wins-over-level-line ()
  (let* ((one (ygg-review-file-tests--tiny "::: {.c #a span=1}\nhi\n:::\n"))
         (a (ygg-review-file-tests--c one "a")))
    (should-not (plist-get one :problems))
    (should (eq (plist-get a :level) 'line))
    (should-not (plist-get a :start-line))
    (should (= (plist-get a :line) 2)))
  (let* ((two (ygg-review-file-tests--tiny "::: {.c #a span=2 level=line}\nhi\n:::\n"))
         (a (ygg-review-file-tests--c two "a")))
    (should (equal (mapcar #'cdr (plist-get two :problems)) '("span together with level=line, the span wins")))
    (should (eq (plist-get a :level) 'range))
    (should (= (plist-get a :start-line) 1))
    (should (= (plist-get a :line) 2)))
  (let ((ok (ygg-review-file-tests--tiny "::: {.c #a span=2}\nhi\n:::\n")))
    (should-not (plist-get ok :problems))
    (should (eq (plist-get (ygg-review-file-tests--c ok "a") :level) 'range))))

(ert-deftest ygg-review-file-a-range-that-starts-after-its-end-is-a-problem ()
  (let ((back (ygg-review-file-tests--tiny "::: {.c #a level=range start-line=5}\nhi\n:::\n")))
    (should (equal (mapcar #'cdr (plist-get back :problems)) '("range starts after its end"))))
  (let ((back (ygg-review-file-tests--tiny "::: {.c #a level=range start-line=1}\nhi\n:::\n")))
    (should-not (plist-get back :problems)))
  (let ((back (ygg-review-file-tests--tiny "::: {.c #a level=range start-side=old start-line=5}\nhi\n:::\n")))
    (should-not (plist-get back :problems))))

(ert-deftest ygg-review-file-priority-confidence-and-type-are-checked ()
  (let* ((back (ygg-review-file-tests--tiny
                (concat "::: {.c #ok priority=3 confidence=0.5 type=nit}\n1\n:::\n"
                        "::: {.c #lo priority=0 confidence=0 type=\"x\"}\n2\n:::\n"
                        "::: {.c #p1 priority=4}\n3\n:::\n"
                        "::: {.c #p2 priority=-1}\n4\n:::\n"
                        "::: {.c #p3 priority=1.0}\n5\n:::\n"
                        "::: {.c #p4 priority=high}\n6\n:::\n"
                        "::: {.c #q1 confidence=1.5}\n7\n:::\n"
                        "::: {.c #q2 confidence=.nan}\n8\n:::\n"
                        "::: {.c #q3 confidence=high}\n9\n:::\n"
                        "::: {.c #t1 type=3}\n10\n:::\n"
                        "::: {.c #t2 type=true}\n11\n:::\n"
                        "::: {.c #t3 type=:odd}\n12\n:::\n")))
         (c (lambda (id) (ygg-review-file-tests--c back id))))
    (should (equal (list (plist-get (funcall c "ok") :priority) (plist-get (funcall c "ok") :confidence)
                         (plist-get (funcall c "ok") :type))
                   '(3 0.5 nit)))
    (should (equal (list (plist-get (funcall c "lo") :priority) (plist-get (funcall c "lo") :confidence)
                         (plist-member (funcall c "lo") :type))
                   '(0 0 nil)))
    (dolist (id '("p1" "p2" "p3" "p4"))
      (should-not (plist-member (funcall c id) :priority)))
    (dolist (id '("q1" "q2" "q3"))
      (should-not (plist-member (funcall c id) :confidence)))
    (dolist (id '("t1" "t2"))
      (should-not (plist-member (funcall c id) :type)))
    (should (eq (plist-get (funcall c "t3") :type) 'odd))
    (should (= 11 (length (plist-get back :problems))))
    (should (seq-every-p (lambda (p) (string-match-p "\\`\\(bad value for \\(priority\\|confidence\\|type\\), ignored\\|unknown type odd, kept\\)\\'" (cdr p)))
                         (plist-get back :problems)))))

(defun ygg-review-file-tests--live-problems (text live)
  (seq-filter (lambda (p) (string-match-p "live diff" (cdr p)))
              (plist-get (ygg-review-file-parse text live) :problems)))

(ert-deftest ygg-review-file-live-diff-check-reports-one-problem-per-inserted-deleted-or-changed-line ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (text (ygg-review-file-print (list :comments nil) diff))
         (target "> +l3 changed\n"))
    (should-not (ygg-review-file-tests--live-problems text diff))
    (dolist (case (list (list (ygg-review-file-tests--sub text target (concat "> junk\n" target)) "not in the live diff")
                        (list (ygg-review-file-tests--sub text target "") "1 more line here")
                        (list (ygg-review-file-tests--sub text target "> +l3 other\n") "differs")
                        (list (ygg-review-file-tests--sub text target (concat "> >\n" target)) "not in the live diff")))
      (let ((found (ygg-review-file-tests--live-problems (car case) diff)))
        (should (= 1 (length found)))
        (should (string-match-p (nth 1 case) (cdr (car found))))))
    (let ((found (ygg-review-file-tests--live-problems
                  (ygg-review-file-tests--sub text target (concat "> junk 1\n> junk 2\n> junk 3\n" target)) diff)))
      (should (= 1 (length found)))
      (should (string-match-p "3 quoted lines" (cdr (car found)))))
    (let ((found (ygg-review-file-tests--live-problems
                  (ygg-review-file-tests--sub text target (concat "> junk\n" target)) diff)))
      (should (= (car (car found)) (ygg-review-file-tests--line-of
                                    (ygg-review-file-tests--sub text target (concat "> junk\n" target))
                                    "> junk"))))))

(ert-deftest ygg-review-file-live-diff-with-extra-files-or-hunks-is-one-problem ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (text (ygg-review-file-print (list :comments nil) diff))
         (more "diff --git a/z b/z\n--- a/z\n+++ b/z\n@@ -1 +1 @@\n-a\n+b\n")
         (found (ygg-review-file-tests--live-problems text (concat diff more))))
    (should (equal (mapcar #'cdr found) '("live diff has extra files or hunks beyond the quoted diff")))
    (should-not (ygg-review-file-tests--live-problems text diff))
    (should-not (ygg-review-file-tests--live-problems "" diff))))

(ert-deftest ygg-review-file-hunk-line-counts-that-do-not-match-the-header-are-problems-without-a-live-diff ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (text (ygg-review-file-print (list :comments nil) diff))
         (problems (lambda (edited) (mapcar #'cdr (plist-get (ygg-review-file-parse edited) :problems)))))
    (should-not (funcall problems text))
    (let* ((hunk (string-match "^> @@ [^\n]*\n" text))
           (edited (concat (substring text 0 hunk) (substring text (match-end 0)))))
      (should (= 1 (length (funcall problems edited))))
      (should (string-match-p "outside any hunk" (car (funcall problems edited)))))
    (let ((edited (ygg-review-file-tests--sub text "> +l3 changed\n" "")))
      (should (string-match-p "fewer lines\\|outside any hunk" (car (funcall problems edited)))))
    (let ((edited (ygg-review-file-tests--sub text "> +l3 changed\n" "> +l3 changed\n> +extra\n")))
      (should (= 1 (length (funcall problems edited))))
      (should (string-match-p "more lines\\|outside any hunk" (car (funcall problems edited)))))
    (let ((edited (concat text "> @@ -1,3 +1,3 @@\n>  a\n")))
      (should (string-match-p "fewer lines" (car (last (funcall problems edited))))))))

(defun ygg-review-file-tests--reference-span-start (recs end n sides)
  (let ((i end) (seen 0) found)
    (while (and (>= i 0) (not found))
      (let ((rec (aref recs i)))
        (cond ((eq (plist-get rec :kind) 'diff) (setq i 0))
              ((and (ygg-review-file--content-p rec) (memq (plist-get rec :side) sides))
               (cl-incf seen)
               (when (= seen n) (setq found i)))))
      (cl-decf i))
    found))

(ert-deftest ygg-review-file-span-start-agrees-with-a-plain-backward-walk ()
  (let ((recs (ygg-review-file--records (ygg-review-file-tests--diff 3))))
    (dolist (sides '((new) (old) (old new) (new old)))
      (dotimes (end (length recs))
        (when (ygg-review-file--content-p (aref recs end))
          (dolist (n '(1 2 3 5 9 40 500))
            (should (equal (ygg-review-file--span-start recs end n sides)
                           (ygg-review-file-tests--reference-span-start recs end n sides)))))))))

(ert-deftest ygg-review-file-span-lookups-over-a-huge-hunk-stay-fast ()
  (let* ((diff (concat "diff --git a/f b/f\n--- a/f\n+++ b/f\n@@ -1,50000 +1,50000 @@\n"
                       (mapconcat (lambda (i) (format " l%d" i)) (number-sequence 1 50000) "\n") "\n"))
         (recs (ygg-review-file--records diff))
         (start (float-time)))
    (dotimes (k 20000)
      (ygg-review-file--span-start recs (- (length recs) 1 (% k 100)) (+ 40000 (% k 900)) '(new)))
    (should (< (- (float-time) start) 10.0))))

(ert-deftest ygg-review-file-reading-many-attributes-takes-linear-time ()
  (let ((time (lambda (n)
                (let ((attrs (concat (mapconcat (lambda (i) (format "k%d=%d" i i)) (number-sequence 1 n) " ")
                                     " " (mapconcat (lambda (i) (format ".c%d" i)) (number-sequence 1 n) " ")))
                      (best 1.0e9))
                  (dotimes (_ 3)
                    (let ((start (float-time)))
                      (ygg-review-file--read-attrs attrs t 1)
                      (setq best (min best (- (float-time) start)))))
                  best))))
    (should (< (funcall time 40000) (max 5.0 (* 40 (funcall time 5000)))))))

(provide 'ygg-review-file-tests)

;;; ygg-review-file-tests.el ends here

(defun ygg-review-file-tests--authored (id author &rest props)
  (append (list :id id :level 'review :text id :created (+ 1790000000 (length id))) props
          (and author (list :author author))))

(ert-deftest ygg-review-file-the-most-common-author-becomes-the-front-matter-default ()
  (let* ((review (list :comments (list (ygg-review-file-tests--authored "a" "you")
                                       (ygg-review-file-tests--authored "bb" "you")
                                       (ygg-review-file-tests--authored "ccc" "codex"))))
         (text (ygg-review-file-tests--round-trip review "")))
    (should (string-search "---\nauthor: you\n---\n" text))
    (should (string-search "{.c #a created" text))
    (should-not (string-search "author=you" text))
    (should (string-search "author=codex" text))
    (should (equal (plist-get (ygg-review-file-tests--c (ygg-review-file-parse text) "a") :author) "you"))
    (should-not (plist-member (ygg-review-file-parse text) :author))))

(ert-deftest ygg-review-file-a-single-author-is-not-hoisted-to-the-front-matter ()
  (let* ((review (list :comments (list (ygg-review-file-tests--authored "a" "you")
                                       (ygg-review-file-tests--authored "bb" "codex"))))
         (text (ygg-review-file-tests--round-trip review "")))
    (should-not (string-search "author:" text))
    (should (string-search "author=you" text))
    (should (string-search "author=codex" text))))

(ert-deftest ygg-review-file-a-comment-without-an-author-prints-author-null-beside-a-default ()
  (let* ((review (list :comments (list (ygg-review-file-tests--authored "a" "you")
                                       (ygg-review-file-tests--authored "bb" "you")
                                       (ygg-review-file-tests--authored "ccc" nil)
                                       (list :id "orph" :level nil :file "zzz" :text "o" :created 1790000009))))
         (text (ygg-review-file-tests--round-trip review "")))
    (should (string-search "#ccc created=2026-09-21T14:13:23Z author=null}" text))
    (should (string-search "#orph level=null" text))
    (should (string-match-p "#orph[^}]*author=null" text))
    (should-not (plist-get (ygg-review-file-tests--c (ygg-review-file-parse text) "ccc") :author))))

(ert-deftest ygg-review-file-without-a-default-a-missing-author-stays-missing ()
  (let ((text (ygg-review-file-tests--round-trip
               (list :comments (list (ygg-review-file-tests--authored "a" nil))) "")))
    (should-not (string-search "author" text))))

(ert-deftest ygg-review-file-a-front-matter-author-fills-comments-without-one ()
  (let ((back (ygg-review-file-parse "---\nauthor: me\n---\n::: {.c #a}\nx\n:::\n::: {.c #b author=you}\ny\n:::\n")))
    (should (equal (plist-get (ygg-review-file-tests--c back "a") :author) "me"))
    (should (equal (plist-get (ygg-review-file-tests--c back "b") :author) "you"))))

(ert-deftest ygg-review-file-thread-replies-keep-their-own-authors-beside-a-default ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (thread (list (list :remote t :id "remote:gh:1" :thread "1" :author "you" :created 1.5
                             :text "a" :file "a.txt" :new-path "a.txt" :level 'line :side 'new :line 21)
                       (list :remote t :id "remote:gh:2" :thread "1" :author "bob" :created 2.5
                             :text "b" :file "a.txt" :new-path "a.txt" :level 'line :side 'new :line 21)))
         (review (list :comments (list (ygg-review-file-tests--authored "a" "you")
                                       (ygg-review-file-tests--authored "bb" "you"))
                       :threads thread))
         (text (ygg-review-file-tests--round-trip review diff)))
    (should (string-search "{.reply #remote:gh:1 created=1970-01-01T00:00:01.5Z author=you}" text))
    (should (string-search "author=bob" text))))

(defun ygg-review-file-tests--thread-on-a-line (id thread url)
  (list :remote t :id id :thread thread :author "x" :created 1.5 :text "t" :url url
        :file "a.txt" :new-path "a.txt" :level 'line :side 'new :line 21))

(ert-deftest ygg-review-file-a-gitlab-thread-prints-no-diff-hunk ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (thread (list (ygg-review-file-tests--thread-on-a-line
                        "remote:gl:1" "d1" "https://gitlab.com/o/r/-/merge_requests/1#note_1")))
         (text (ygg-review-file-tests--round-trip (list :threads thread) diff)))
    (should-not (string-search "diff-hunk" text))
    (should-not (plist-member (car (plist-get (ygg-review-file-parse text) :threads)) :diff-hunk))))

(ert-deftest ygg-review-file-a-gitlab-thread-keeps-a-hunk-it-really-has ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (thread (list (append (ygg-review-file-tests--thread-on-a-line
                                "remote:gl:1" "d1" "https://gitlab.com/o/r/-/merge_requests/1#note_1")
                               (list :diff-hunk "@@ -1 +1 @@\n-a\n+b"))))
         (text (ygg-review-file-tests--round-trip (list :threads thread) diff)))
    (should (string-search "diff-hunk=" text))))

(ert-deftest ygg-review-file-a-github-thread-omits-the-hunk-its-position-gives ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (recs (ygg-review-file--records diff))
         (hunk (ygg-review-file--hunk-text
                recs (ygg-review-file-tests--rec-at-index recs 'new 21)))
         (thread (list (append (ygg-review-file-tests--thread-on-a-line
                                "remote:gh:1" "1" "https://github.com/o/r/pull/1#discussion_r1")
                               (list :diff-hunk hunk))))
         (text (ygg-review-file-tests--round-trip (list :threads thread) diff)))
    (should hunk)
    (should-not (string-search "diff-hunk" text))
    (should (equal (plist-get (car (plist-get (ygg-review-file-parse text) :threads)) :diff-hunk) hunk))))

(ert-deftest ygg-review-file-a-github-thread-with-a-different-hunk-keeps-it ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (thread (list (append (ygg-review-file-tests--thread-on-a-line
                                "remote:gh:1" "1" "https://github.com/o/r/pull/1#discussion_r1")
                               (list :diff-hunk "@@ -1 +1 @@\n-a\n+b"))))
         (text (ygg-review-file-tests--round-trip (list :threads thread) diff)))
    (should (string-search "diff-hunk=\"@@ -1 +1 @@\\n-a\\n+b\"" text))))

(ert-deftest ygg-review-file-a-github-url-alone-marks-a-thread-as-github ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (thread (list (ygg-review-file-tests--thread-on-a-line
                        "n1" "n1" "https://github.com/o/r/pull/1#discussion_r1")))
         (text (ygg-review-file-tests--round-trip (list :threads thread) diff)))
    (should (string-search "diff-hunk=null" text))))

(defun ygg-review-file-tests--live-text (diff)
  (let ((recs (ygg-review-file--records diff)))
    (ygg-review-file-print
     (list :comments
           (list (ygg-review-file-tests--derived
                  recs (ygg-review-file-tests--rec-at-index recs 'new 3) "c1" nil
                  :type 'nit :created 1.0 :text "first")
                 (ygg-review-file-tests--derived
                  recs (ygg-review-file-tests--rec-at-index recs 'new 20) "c2" nil
                  :type 'nit :created 2.0 :text "second")))
     diff)))

(defun ygg-review-file-tests--live-messages (text live)
  (mapcar #'cdr (plist-get (ygg-review-file-parse text live) :problems)))

(ert-deftest ygg-review-file-live-diff-check-ignores-a-carriage-return-ending-the-content-lines ()
  (let* ((diff (concat "diff --git a/f b/f\n--- a/f\n+++ b/f\n@@ -1,3 +1,3 @@\n a\r\n-b\r\n+B\r\n c\r\n"))
         (recs (ygg-review-file--records diff))
         (text (ygg-review-file-print
                (list :comments (list (ygg-review-file-tests--derived
                                       recs (cl-position "+B\r" recs :test #'equal
                                                         :key (lambda (r) (plist-get r :text)))
                                       "a" nil
                                       :type 'nit :created 1.0 :text "x")))
                diff)))
    (should (string-search "\r" (plist-get (car (plist-get (ygg-review-file-parse text) :comments)) :quote)))
    (should-not (plist-get (ygg-review-file-parse text diff) :problems))
    (should-not (plist-get (ygg-review-file-parse text (replace-regexp-in-string "\r" "" diff)) :problems))))

(ert-deftest ygg-review-file-live-diff-check-resyncs-after-an-insert-of-any-size ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (text (ygg-review-file-tests--live-text diff)))
    (should-not (ygg-review-file-tests--live-messages text diff))
    (dolist (n '(1 2 20 21 25 150))
      (let* ((extra (mapconcat (lambda (k) (format "+extra%d\n" k)) (number-sequence 1 n) ""))
             (live (ygg-review-file-tests--sub diff " l2\n-l3\n" (concat " l2\n" extra "-l3\n")))
             (found (ygg-review-file-tests--live-messages text live)))
        (should (equal found (list (format "live diff has %d more line%s here" n (if (= n 1) "" "s")))))))
    (let ((found (ygg-review-file-tests--live-messages
                  text (ygg-review-file-tests--sub diff "+b1\n" (concat (make-string 30 ?+) "\n+b1\n")))))
      (should (= 1 (length found))))))

(ert-deftest ygg-review-file-live-diff-check-resyncs-on-a-moved-hunk-and-a-shift-of-every-hunk-header ()
  (let* ((diff (ygg-review-file-tests--diff 3))
         (text (ygg-review-file-tests--live-text diff))
         (h2 (string-search "@@ -18,6" diff)) (h3 (string-search "@@ -35,6" diff))
         (rest (string-search "diff --git a/b.txt" diff))
         (moved (concat (substring diff 0 h2) (substring diff h3 rest) (substring diff h2 h3)
                        (substring diff rest)))
         (shifted (replace-regexp-in-string
                   "^@@ -\\([0-9]+\\)\\(,[0-9]+\\)? \\+\\([0-9]+\\)"
                   (lambda (m)
                     (string-match "^@@ -\\([0-9]+\\)\\(,[0-9]+\\)? \\+\\([0-9]+\\)" m)
                     (format "@@ -%d%s +%d" (+ 7 (string-to-number (match-string 1 m)))
                             (or (match-string 2 m) "") (+ 7 (string-to-number (match-string 3 m)))))
                   diff t t)))
    (should (<= (length (ygg-review-file-tests--live-messages text moved)) 2))
    (should (ygg-review-file-tests--live-messages text moved))
    (let ((found (ygg-review-file-tests--live-messages text shifted)))
      (should (= 1 (length found)))
      (should (string-prefix-p "hunk headers shifted" (car found))))))

(ert-deftest ygg-review-file-duplicate-or-conflicting-attributes-are-reported-and-the-first-is-kept ()
  (dolist (case '(("{.c #a type=nit type=issue}" "duplicate attribute type" :type nit)
                  ("{.c #a #b}" "duplicate id #b" :id "a")
                  ("{.c #a line=2 line=1}" "duplicate attribute line" :line 2)
                  ("{.c #a #b id=c}" "duplicate id #b" :id "a")
                  ("{.c .c #a}" "extra class c" :id "a")))
    (let* ((back (ygg-review-file-tests--tiny (concat "::: " (car case) "\nhi\n:::\n")))
           (c (car (plist-get back :comments))))
      (should (equal (plist-get c (nth 2 case)) (nth 3 case)))
      (should (ygg-review-file-tests--problem back (nth 1 case)))))
  (let ((back (ygg-review-file-tests--tiny "::: {.c #a text=zz}\nhi\n:::\n")))
    (should (equal (plist-get (car (plist-get back :comments)) :text) "hi"))
    (should (ygg-review-file-tests--problem back "text attribute together with a body")))
  (let ((back (ygg-review-file-tests--tiny "::: {.c #a text=zz}\n:::\n")))
    (should (equal (plist-get (car (plist-get back :comments)) :text) "zz"))
    (should-not (ygg-review-file-tests--problem back "text attribute"))))

(ert-deftest ygg-review-file-created-accepts-a-basic-offset-with-a-fraction-and-a-space-for-the-t ()
  (let ((base (ygg-review-file-tests--utc 2026 2 3 4 5 6)))
    (dolist (case (list (list "\"2026-02-03 04:05:06Z\"" base)
                        (list "\"2026-02-03 07:05:06+03:00\"" base)
                        (list "\"2026-02-02T22:35:06.5-0530\"" (+ base 0.5))
                        (list "\"2026-02-02 22:35:06.5-0530\"" (+ base 0.5))))
      (let ((got (ygg-review-file-tests--created (car case))))
        (should-not (cdr got))
        (should (equal (plist-get (car got) :created) (nth 1 case)))))
    (let ((back (ygg-review-file-parse "---\ncreated: 2026-02-03 04:05:06Z\n---\n")))
      (should-not (plist-get back :problems))
      (should (equal (plist-get back :created) base)))))

(ert-deftest ygg-review-file-unknown-type-and-status-are-reported-and-kept ()
  (let* ((back (ygg-review-file-tests--tiny
                (concat "::: {.c #a type=issue status=pending}\n1\n:::\n"
                        "::: {.c #b type=todo}\n2\n:::\n"
                        "::: {.c #c type=bogus status=weird}\n3\n:::\n")))
         (c (ygg-review-file-tests--c back "c")))
    (should (eq (plist-get c :type) 'bogus))
    (should (eq (plist-get c :status) 'weird))
    (should (equal (mapcar #'cdr (plist-get back :problems))
                   '("unknown type bogus, kept" "unknown status weird, kept"))))
  (let ((ygg-git-compare-comment-types '(zap)))
    (let ((back (ygg-review-file-tests--tiny "::: {.c #a type=zap}\n1\n:::\n::: {.c #b type=nit}\n2\n:::\n")))
      (should (equal (mapcar #'cdr (plist-get back :problems)) '("unknown type nit, kept"))))))
