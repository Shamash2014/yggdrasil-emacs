;;; ygg-skill-index.el --- every skill the agents can reach, searchable -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ygg-agent-conf)

(defgroup ygg-skill-index nil
  "The index of agent skills that skill_search ranks."
  :group 'tools :prefix "ygg-skill-index-")

(defcustom ygg-skill-index-file
  (expand-file-name "skill-index.json" (locate-user-emacs-file "var/"))
  "Where the parsed skills are kept between runs."
  :type 'file :group 'ygg-skill-index)

(defcustom ygg-skill-index-claude-home "~/.claude"
  "The Claude home whose skills, synced skills, plugins and settings count."
  :type 'directory :group 'ygg-skill-index)

(defcustom ygg-skill-index-agents-dir "~/.agents/skills"
  "The shared skill directory codex reads as its user scope."
  :type 'directory :group 'ygg-skill-index)

(defconst ygg-skill-index--version 1
  "Bumped whenever a record gains or loses a field, so old files are rebuilt.")

(defvar ygg-skill-index--roots nil
  "Root directory to its record, as last built or read from the file.")

(defvar ygg-skill-index--terms (make-hash-table :test #'equal)
  "SKILL.md path to (TERM-COUNTS . LENGTH), dropped whenever a root is rebuilt.")

;;; Roots

(defun ygg-skill-index--plugin-roots ()
  "Skill directories of the plugins the Claude home has on, as (DIR . PREFIX)."
  (let* ((spec (list :home ygg-skill-index-claude-home :settings "settings.json"))
         (root (ygg-agent--plugins-root spec))
         (enabled (ygg-agent--enabled-plugins spec (expand-file-name ygg-skill-index-claude-home)))
         (installs (ygg-agent--plugin-installs root))
         out)
    (when enabled
      (pcase-dolist (`(,id . ,dirs) (ygg-agent--plugin-caches root))
        (when-let* (((eq t (gethash id enabled)))
                    (current (ygg-agent--plugin-current-dir dirs (gethash id installs))))
          (push (list (expand-file-name "skills" current) "plugin"
                      (concat (car (split-string id "@")) ":"))
                out))))
    (nreverse out)))

(defun ygg-skill-index--project-roots (dir)
  "The .claude/skills of DIR and of each directory above it in its checkout.
Then the checkout\='s own skills directory."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (top (file-name-as-directory
               (expand-file-name (or (locate-dominating-file dir ".git") dir))))
         (out (list (list (expand-file-name ".claude/skills" dir) "project" nil))))
    (while (and (not (equal dir top)) (string-prefix-p top dir))
      (setq dir (file-name-directory (directory-file-name dir)))
      (push (list (expand-file-name ".claude/skills" dir) "project" nil) out))
    (nreverse (cons (list (expand-file-name "skills" top) "project" nil) out))))

(defun ygg-skill-index-roots (&optional project)
  "Every directory of skills an agent working in PROJECT may be offered.
Each is (DIR KIND PREFIX): KIND is project, user, synced or plugin, and
PREFIX what the agent puts before a skill's name there.  A directory
reached twice, through a link, counts once, where it was first reached."
  (let* ((home (expand-file-name ygg-skill-index-claude-home))
         (synced (expand-file-name "skills/synced" home))
         (candidates
          (append
           (when project (ygg-skill-index--project-roots project))
           (list (list (expand-file-name "skills" home) "user" nil)
                 (list (expand-file-name ygg-skill-index-agents-dir) "user" nil))
           (mapcar (lambda (dir) (list dir "synced" "anthropic-skills:"))
                   (and (file-directory-p synced)
                        (seq-filter #'file-directory-p
                                    (directory-files synced t directory-files-no-dot-files-regexp))))
           (ygg-skill-index--plugin-roots)))
         seen out)
    (dolist (root candidates)
      (when (file-directory-p (car root))
        (let ((true (file-truename (car root))))
          (unless (member true seen)
            (push true seen)
            (push (cons (directory-file-name (expand-file-name (car root))) (cdr root))
                  out)))))
    (nreverse out)))

;;; Reading a SKILL.md

(defun ygg-skill-index--unquote (value)
  (cond ((string-match "\\`'\\(.*\\)'\\'" value)
         (string-replace "''" "'" (match-string 1 value)))
        ((string-match-p "\\`\".*\"\\'" value)
         (condition-case nil (read value) (error (substring value 1 -1))))
        (t value)))

(defun ygg-skill-index--continuation ()
  "The indented lines after point's line, moving over them.
A blank line between two indented ones is one of them."
  (let ((last (point)) lines)
    (while (and (zerop (forward-line 1))
                (not (eobp))
                (looking-at "^\\(?:[ \t]+\\(.*\\)\\|[ \t]*\\)$"))
      (push (or (match-string 1) "") lines)
      (when (match-string 1) (setq last (point))))
    (goto-char last)
    (nreverse (seq-drop-while #'string-empty-p lines))))

(defun ygg-skill-index--frontmatter ()
  "The top-level keys of the current buffer's frontmatter, and where it ends.
Read by line rather than as YAML: a skill whose header is not valid YAML
is still offered to the agents, so it still has to be found."
  (goto-char (point-min))
  (let (fields)
    (if (not (looking-at "---[ \t]*\n"))
        (cons nil (point-min))
      (forward-line 1)
      (let ((end (save-excursion
                   (and (re-search-forward "^---[ \t]*$" nil t) (match-beginning 0)))))
        (if (null end)
            (cons nil (point-min))
          (while (< (point) end)
            (when (looking-at "^\\([A-Za-z][A-Za-z0-9_-]*\\):[ \t]*\\(.*?\\)[ \t]*$")
              (let* ((key (match-string 1))
                     (raw (match-string 2))
                     (more (ygg-skill-index--continuation))
                     (value (cond
                             ((string-match-p "\\`[>|][-+]?\\'" raw)
                              (string-join more (if (string-prefix-p "|" raw) "\n" " ")))
                             ((string-empty-p raw) (string-join more " "))
                             (t (ygg-skill-index--unquote
                                 (string-join (cons raw more) " "))))))
                (push (cons key (string-trim value)) fields)))
            (forward-line 1))
          (goto-char end)
          (forward-line 1)
          (cons (nreverse fields) (point)))))))

(defun ygg-skill-index-read (file dir prefix)
  "FILE, the SKILL.md of DIR, as a record; PREFIX goes before its name."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
    (let* ((parsed (ygg-skill-index--frontmatter))
           (fields (car parsed))
           (field (lambda (&rest keys)
                    (seq-some (lambda (k) (cdr (assoc k fields))) keys)))
           headings)
      (goto-char (point-min))
      (while (re-search-forward "^#+ \\(.*\\)$" nil t)
        (push (match-string-no-properties 1) headings))
      (list :name (concat prefix (if prefix
                                     (file-name-nondirectory dir)
                                   (or (funcall field "name") (file-name-nondirectory dir))))
            :description (or (funcall field "description") "")
            :when_to_use (or (funcall field "when_to_use" "when-to-use") "")
            :headings (vconcat (nreverse headings))
            :dmi (if (equal (downcase (or (funcall field "disable-model-invocation") "")) "true")
                     t :false)
            :path file))))

;;; Building and keeping it

(defun ygg-skill-index--skill-files (root)
  (seq-filter #'file-regular-p
              (mapcar (lambda (dir) (expand-file-name "SKILL.md" dir))
                      (seq-filter #'file-directory-p
                                  (directory-files root t directory-files-no-dot-files-regexp)))))

(defun ygg-skill-index--mtime (file)
  (if-let* ((attrs (file-attributes file)))
      (format "%s" (float-time (file-attribute-modification-time attrs)))
    "-"))

(defun ygg-skill-index--stamp (root)
  "What ROOT looks like on disk, cheaply: its own time and each SKILL.md's.
Any difference at all is a change, which a comparison against when the
index was built would miss for an edit made within the same second."
  (mapconcat (lambda (file) (concat file "@" (ygg-skill-index--mtime file)))
             (ygg-skill-index--skill-files root)
             "\n"))

(defun ygg-skill-index--root-stamp (root)
  (concat (ygg-skill-index--mtime (file-name-as-directory (file-truename root)))
          "\n" (ygg-skill-index--stamp root)))

(defun ygg-skill-index--build-root (root kind prefix)
  (list :kind kind :prefix (or prefix "")
        :stamp (ygg-skill-index--root-stamp root)
        :skills (vconcat
                 (mapcar (lambda (file)
                           (ygg-skill-index-read file (directory-file-name
                                                         (file-name-directory file))
                                                  prefix))
                         (ygg-skill-index--skill-files root)))))

(defun ygg-skill-index--load-file ()
  "The roots recorded in `ygg-skill-index-file', or nil."
  (when (file-readable-p ygg-skill-index-file)
    (ignore-errors
      (let ((json (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8))
                      (insert-file-contents ygg-skill-index-file))
                    (json-parse-buffer :object-type 'plist :array-type 'array
                                       :null-object nil :false-object :false))))
        (when (equal (plist-get json :version) ygg-skill-index--version)
          (cl-loop for (key value) on (plist-get json :roots) by #'cddr
                   collect (cons (substring (symbol-name key) 1) value)))))))

(defun ygg-skill-index--save ()
  "Write the roots to `ygg-skill-index-file' by rename."
  (let* ((file ygg-skill-index-file)
         (dir (file-name-directory file)))
    (make-directory dir t)
    (let ((tmp (make-temp-file (expand-file-name ".skill-index-" dir)))
          (coding-system-for-write 'utf-8-unix))
      (condition-case nil
          (progn
            (with-temp-file tmp
              (json-insert (list :version ygg-skill-index--version
                             :built_at (float-time)
                             :roots (cl-loop for (root . record) in ygg-skill-index--roots
                                             append (list (intern (concat ":" root)) record)))))
            (rename-file tmp file t))
        (error (ignore-errors (delete-file tmp)))))))

(defun ygg-skill-index-refresh (&optional project)
  "The roots for PROJECT, each rebuilt only if its stamp moved."
  (unless ygg-skill-index--roots
    (setq ygg-skill-index--roots (ygg-skill-index--load-file)))
  (let (changed out)
    (pcase-dolist (`(,root ,kind ,prefix) (ygg-skill-index-roots project))
      (let ((record (cdr (assoc root ygg-skill-index--roots))))
        (unless (and record
                     (equal (plist-get record :stamp) (ygg-skill-index--root-stamp root)))
          (setq record (ygg-skill-index--build-root root kind prefix)
                changed t)
          (setf (alist-get root ygg-skill-index--roots nil nil #'equal) record))
        (push (cons root record) out)))
    (when changed
      (clrhash ygg-skill-index--terms)
      (ygg-skill-index--save))
    (nreverse out)))

(defun ygg-skill-index--hidden ()
  "Names the Claude settings take away from the model."
  (when-let* ((json (ygg-agent--read-json
                     (expand-file-name "settings.json" ygg-skill-index-claude-home)))
              (table (gethash "skillOverrides" json))
              ((hash-table-p table)))
    (let (out)
      (maphash (lambda (k v) (when (member v '("off" "user-invocable-only")) (push k out)))
               table)
      out)))

(defun ygg-skill-index-skills (&optional project all)
  "Every skill for PROJECT, first of a name winning, each with its :kind.
Without ALL, only those a model may be offered: not disabled for model
invocation, not hidden by the Claude settings."
  (let ((hidden (unless all (ygg-skill-index--hidden)))
        seen out)
    (pcase-dolist (`(,_root . ,record) (ygg-skill-index-refresh project))
      (seq-doseq (skill (plist-get record :skills))
        (let ((name (plist-get skill :name)))
          (unless (member name seen)
            (push name seen)
            (unless (and (not all)
                         (or (eq t (plist-get skill :dmi)) (member name hidden)))
              (push (append (list :kind (plist-get record :kind)) skill) out))))))
    (nreverse out)))

;;; Ranking

(defun ygg-skill-index--tokens (text)
  (let ((case-fold-search nil) out)
    (with-temp-buffer
      (insert (downcase text))
      (goto-char (point-min))
      (while (re-search-forward "[a-z0-9]+" nil t)
        (when (> (- (match-end 0) (match-beginning 0)) 1)
          (push (match-string-no-properties 0) out))))
    (nreverse out)))

(defconst ygg-skill-index--fields
  '((3 . ygg-skill-index--name-text)
    (1.5 . ygg-skill-index--purpose-text)
    (0.5 . ygg-skill-index--headings-text))
  "Each field a skill is ranked on, with what a term found there weighs.
A name is said once and means the most; the headings are many and mean
the least.  Measured on real prompts, not guessed.")

(defun ygg-skill-index--name-text (skill)
  (replace-regexp-in-string "[-:]" " " (plist-get skill :name)))

(defun ygg-skill-index--purpose-text (skill)
  (concat (plist-get skill :description) " " (plist-get skill :when_to_use)))

(defun ygg-skill-index--headings-text (skill)
  (string-join (append (plist-get skill :headings) nil) " "))

(defun ygg-skill-index--terms (skill)
  "SKILL's fields, each as (TERM-COUNTS . LENGTH)."
  (let ((path (plist-get skill :path)))
    (or (gethash path ygg-skill-index--terms)
        (puthash path
                 (mapcar (lambda (field)
                           (let ((tokens (ygg-skill-index--tokens (funcall (cdr field) skill)))
                                 (counts (make-hash-table :test #'equal)))
                             (dolist (tok tokens) (cl-incf (gethash tok counts 0)))
                             (cons counts (length tokens))))
                         ygg-skill-index--fields)
                 ygg-skill-index--terms))))

(defun ygg-skill-index-rank (query skills)
  "SKILLS scored against QUERY by BM25F, best first, as (SCORE . SKILL)."
  (let* ((terms (mapcar #'ygg-skill-index--terms skills))
         (n (length skills))
         (avgs (cl-loop for i from 0 below (length ygg-skill-index--fields)
                        collect (max 1.0 (/ (float (apply #'+ 0 (mapcar (lambda (fields)
                                                                          (cdr (nth i fields)))
                                                                        terms)))
                                            (max n 1)))))
         (df (make-hash-table :test #'equal))
         (words (ygg-skill-index--tokens query)))
    (dolist (fields terms)
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (field fields)
          (maphash (lambda (word _) (puthash word t seen)) (car field)))
        (maphash (lambda (word _) (cl-incf (gethash word df 0))) seen)))
    (sort (cl-mapcar
           (lambda (skill fields)
             (cons (cl-loop
                    for word in words
                    for tf = (cl-loop for (counts . len) in fields
                                      for (weight . _) in ygg-skill-index--fields
                                      for avg in avgs
                                      sum (/ (* weight (gethash word counts 0))
                                             (+ 0.25 (* 0.75 (/ len avg)))))
                    when (> tf 0)
                    sum (let ((d (gethash word df)))
                          (* (log (1+ (/ (+ (- n d) 0.5) (+ d 0.5))))
                             (/ (* tf 2.2) (+ tf 1.2)))))
                   skill))
           skills terms)
          (lambda (a b) (> (car a) (car b))))))

(defun ygg-skill-index-search (query &optional k project)
  "The K best skills for QUERY in PROJECT, as plists for an agent to read."
  (let ((ranked (ygg-skill-index-rank query (ygg-skill-index-skills project))))
    (mapcar (lambda (hit)
              (let ((skill (cdr hit)))
                (list :name (plist-get skill :name)
                      :description (plist-get skill :description)
                      :path (plist-get skill :path)
                      :score (/ (round (* 100 (car hit))) 100.0))))
            (seq-take (seq-filter (lambda (hit) (> (car hit) 0)) ranked) (or k 5)))))

(defun ygg-skill-index-closest (name skills &optional n)
  "The N names in SKILLS nearest NAME: containing it first, then by edit distance."
  (let ((want (downcase name)))
    (seq-take
     (mapcar #'cdr
             (sort (mapcar (lambda (skill)
                             (let ((have (downcase (plist-get skill :name))))
                               (cons (+ (if (string-search want have) 0 1000)
                                        (string-distance want have))
                                     (plist-get skill :name))))
                           skills)
                   (lambda (a b) (< (car a) (car b)))))
     (or n 5))))

(defun ygg-skill-index-find (name &optional project)
  "The skill called NAME in PROJECT, matching the part after a prefix too.
Otherwise (nil . CLOSEST), CLOSEST the five names nearest it."
  (let* ((skills (ygg-skill-index-skills project))
         (match (or (seq-find (lambda (s) (equal (plist-get s :name) name)) skills)
                    (seq-find (lambda (s)
                                (string-equal-ignore-case
                                 (car (last (split-string (plist-get s :name) ":"))) name))
                              skills))))
    (if match
        (cons match nil)
      (cons nil (ygg-skill-index-closest name skills)))))

(defun ygg-skill-index-body (skill)
  "SKILL's SKILL.md without its frontmatter."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents (plist-get skill :path)))
    (string-trim (buffer-substring-no-properties
                  (cdr (ygg-skill-index--frontmatter)) (point-max)))))

(defun ygg-skill-index-files (skill)
  "Files beside SKILL's SKILL.md, relative to its directory."
  (let ((dir (file-name-directory (plist-get skill :path))))
    (sort (seq-remove (lambda (f) (equal f "SKILL.md"))
                      (mapcar (lambda (f) (file-relative-name f dir))
                              (directory-files-recursively
                               dir "" nil
                               (lambda (d) (not (string-prefix-p "." (file-name-nondirectory d)))))))
          #'string<)))

(provide 'ygg-skill-index)
;;; ygg-skill-index.el ends here
