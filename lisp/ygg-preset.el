;;; ygg-preset.el --- The words you keep retyping, kept once -*- lexical-binding: t; -*-

;; A preset is what you would have typed at an agent, written down: a markdown
;; file with frontmatter saying how it runs and a body saying the words.
;;
;; The same name may be written in three places, and only in a presets home.
;; The config's own presets directory is the base, the owner's file comes over
;; it, and the checkout's file comes over both, each overriding fields and
;; appending to the body, because a project adds a rule to the instruction
;; rather than restating it.  Nothing under a skills directory is a preset:
;; a skill is a slash command for a prompt.
;;
;; What a preset learns while running is kept beside it, so the same correction
;; is not typed a fourth time.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defcustom ygg-preset-user-directory (expand-file-name "~/.aob/presets/")
  "Where the words live, once, for every project."
  :type 'directory :group 'ygg-preset)

(defcustom ygg-preset-home-dir "~"
  "The user's home, where their own skills and subagents are read from."
  :type 'directory :group 'ygg-preset)

(defcustom ygg-preset-own-skills-dir
  (expand-file-name "skills" user-emacs-directory)
  "Where this configuration keeps the skills its own modes name.
A mode is handed the body of every skill it names, so a skill kept here
reaches every task in every root without being installed anywhere: the
config that names it is the config that carries it.  A repository of the
owner's own still shadows one of the same name."
  :type 'directory :group 'ygg-preset)

(defvar ygg-preset-config-directory
  (expand-file-name
   "presets"
   (directory-file-name
    (file-name-directory
     (directory-file-name
      (file-name-directory (or load-file-name buffer-file-name))))))
  "The presets the config itself ships, found from this file's own place.
The config is wherever this module sits, so the words travel with it.")

(defconst ygg-preset-project-directory ".aob/presets"
  "Where a repository keeps only what differs from the global words.")

(defvar ygg-preset-old-user-directory (expand-file-name "~/.aob/directives/")
  "The home presets were kept in before they were called presets.")

(defconst ygg-preset-old-project-directory ".aob/directives"
  "The repository directory presets were kept in before the rename.")

(defconst ygg-preset-old-subdirectory "presets"
  "Where named settings lived inside whichever old directory held the words.")

(defconst ygg-preset-project-memory-directory ".aob/memory"
  "Where a repository keeps what its presets learned in it.")

(defconst ygg-preset-user-memory-subdirectory "memory"
  "Where the preset home keeps what its presets learned everywhere.")

(cl-defstruct (ygg-preset (:constructor ygg-preset--new)
                          (:conc-name ygg-preset--slot-) (:copier nil))
  name body fields files local preset)

(defalias 'ygg-preset-name 'ygg-preset--slot-name)
(defalias 'ygg-preset-fields 'ygg-preset--slot-fields)
(defalias 'ygg-preset-files 'ygg-preset--slot-files)
(defalias 'ygg-preset-preset 'ygg-preset--slot-preset)
(defalias 'ygg-preset-raw-body 'ygg-preset--slot-body)

(declare-function ygg-ex--expand "yggdrasil-ex" (string))

(defun ygg-preset-body (d)
  "D's body with the command line's expansions already run in this buffer.
A body is written where the work is, so a shell or selection expansion
reaches the worker as what it stood for."
  (let ((body (ygg-preset--slot-body d)))
    (if (fboundp 'ygg-ex--expand) (ygg-ex--expand body) body)))

(defun ygg-preset-field (d key)
  "D's KEY, or nil when it does not set one.
This is what D's own files say.  For the value it will actually run
under, which a named preset may be the one supplying, use
ygg-preset-setting."
  (plist-get (ygg-preset-fields d) key))

(defun ygg-preset-sets-p (d key)
  "Whether D sets KEY at all, which nil as a value cannot tell you."
  (and (plist-member (ygg-preset-fields d) key) t))

(defun ygg-preset--holder (d key)
  "Whichever of D and the preset it was shaped by decides KEY, strongest first."
  (cond ((ygg-preset-sets-p d key) d)
        ((and (ygg-preset-preset d)
              (ygg-preset-sets-p (ygg-preset-preset d) key))
         (ygg-preset-preset d))))

(defun ygg-preset-setting (d key)
  "D's KEY as it will run: what D says itself, else what its preset says.
Reach for this whenever the value decides how the preset runs; reach for
ygg-preset-field only to ask what D's own files carry."
  (when-let* ((holder (ygg-preset--holder d key)))
    (ygg-preset-field holder key)))

(defun ygg-preset-settles-p (d key)
  "Whether anything settles KEY for D, which nil as a value cannot tell you."
  (and (ygg-preset--holder d key) t))

(defun ygg-preset--value (raw)
  (let ((s (string-trim raw)))
    (cond
     ((string-match "\\`\\[\\(.*\\)\\]\\'" s)
      (let ((inner (string-trim (match-string 1 s))))
        (if (string-empty-p inner) nil
          (mapcar #'string-trim (split-string inner "," t)))))
     ((member s '("yes" "true")) t)
     ((member s '("no" "false")) nil)
     (t s))))

(defun ygg-preset--block-or (value)
  "VALUE itself, or the indented lines a YAML folded or literal block opens.
A folded block joins its lines with spaces; a literal one keeps newlines."
  (if (not (string-match-p "\\`[>|][-+]?[ \t]*\\'" value))
      value
    (let ((literal (string-prefix-p "|" value))
          lines)
      (while (and (zerop (forward-line 1))
                  (looking-at "^[ \t]+\\(.*\\)$"))
        (push (match-string 1) lines))
      (unless (eobp) (forward-line -1) (end-of-line))
      (string-join (nreverse lines) (if literal "\n" " ")))))

(defun ygg-preset--parse (file)
  "FILE as (FIELDS . BODY), reading the frontmatter the way a SKILL.md is."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (fields)
      (when (looking-at "---[ \t]*\n")
        (forward-line)
        (let ((start (point)))
          (if (re-search-forward "^---[ \t]*$" nil t)
              (let ((end (match-beginning 0)))
                (save-restriction
                  (narrow-to-region start end)
                  (goto-char (point-min))
                  (while (re-search-forward
                          "^\\([A-Za-z][A-Za-z0-9_-]*\\):[ \t]*\\(.*\\)$" nil t)
                    (setq fields
                          (plist-put fields
                                     (intern (concat ":" (match-string 1)))
                                     (ygg-preset--value
                                      (ygg-preset--block-or (match-string 2)))))))
                (goto-char end)
                (forward-line))
            (goto-char start))))
      (cons fields (string-trim (buffer-substring-no-properties
                                 (point) (point-max)))))))

(defun ygg-preset-names (raw)
  "RAW as the list of names a header line naming several holds."
  (cond ((consp raw) (seq-filter #'stringp raw))
        ((stringp raw) (split-string raw "[ \t,]+" t))))

(defun ygg-preset-parents (d)
  "The presets whose words come ahead of D's own."
  (ygg-preset-names (ygg-preset-field d :parents)))

(defun ygg-preset-skills (d)
  "The skills D names, by name, or nil when it names none.
A skill a mode names is part of how that mode works, so the worker is
handed it rather than told to go and read it."
  (ygg-preset-names (ygg-preset-setting d :skills)))

(defun ygg-preset-carries (d)
  "The presets D hands its subagents whole, by name, or nil when none.
A mode that orchestrates gives each of its subagents another mode's
brief word for word, so the carried body travels in D's own brief rather
than being retold from memory."
  (ygg-preset-names (ygg-preset-setting d :carries)))

(defun ygg-preset-place (d)
  "Whether D opens before the presets picked with it, or after them."
  (if (equal (ygg-preset-field d :place) "before") 'before 'after))

(defun ygg-preset-skill-files (root)
  "Every SKILL.md ROOT, home or this config carries, in Claude\='s layout.
Repository-local skills come first, so one shadows a home skill of the
same name, and the config\='s own are last: a mode that names a skill this
config ships gets it wherever it runs, and a repository that keeps one of
that name gets its own instead."
  (seq-mapcat
   (lambda (dir)
     (when (file-directory-p dir)
       (seq-filter #'file-readable-p
                   (mapcar (lambda (sub) (expand-file-name "SKILL.md" sub))
                           (seq-filter #'file-directory-p
                                       (directory-files dir t "\\`[^.]"))))))
   (list (expand-file-name ".claude/skills" root)
         (expand-file-name "skills" root)
         (expand-file-name ".claude/skills" ygg-preset-home-dir)
         ygg-preset-own-skills-dir)))

(defun ygg-preset-skill-body (name root)
  "The body of the skill called NAME in ROOT, its frontmatter dropped.
The name field answers first and the directory name after it, the
repository ahead of the home, and nil when nobody wrote such a skill."
  (seq-some
   (lambda (file)
     (let* ((parsed (ygg-preset--parse file))
            (called (or (plist-get (car parsed) :name)
                        (file-name-nondirectory
                         (directory-file-name (file-name-directory file))))))
       (when (equal called name)
         (let ((body (cdr parsed)))
           (unless (string-empty-p body) body)))))
   (ygg-preset-skill-files root)))

(defvar ygg-preset--move-announced nil
  "Whether the one line about the home having moved has been said.")

(defun ygg-preset--announce-move (dir)
  "Say once that DIR is the old home and where the new one is."
  (unless ygg-preset--move-announced
    (setq ygg-preset--move-announced t)
    (message "Presets now live in %s; %s is still read — M-x ygg-preset-migrate moves it"
             (abbreviate-file-name ygg-preset-user-directory)
             (abbreviate-file-name dir))))

(defun ygg-preset--carried-files (dir)
  "The SKILL.md of every subdirectory of DIR, for a preset carrying files."
  (when (file-directory-p dir)
    (seq-filter
     #'file-readable-p
     (mapcar (lambda (sub) (expand-file-name "SKILL.md" sub))
             (seq-filter #'file-directory-p (directory-files dir t "\\`[^.]"))))))

(defun ygg-preset--read-dir (dir &optional old local)
  "Every markdown file in DIR as (NAME FILE FIELDS BODY LOCAL).
OLD says DIR is one of the homes the words have since moved out of, and
LOCAL that the checkout rather than the owner is the one carrying it."
  (when (file-directory-p dir)
    (let ((files (append (directory-files dir t "\\.md\\'")
                         (ygg-preset--carried-files dir))))
      (when (and old files) (ygg-preset--announce-move dir))
      (mapcar (lambda (f)
                (let ((parsed (ygg-preset--parse f))
                      (name (if (equal (file-name-nondirectory f) "SKILL.md")
                                (file-name-nondirectory
                                 (directory-file-name (file-name-directory f)))
                              (file-name-base f))))
                  (list name f (car parsed) (cdr parsed) local)))
              files))))

(defcustom ygg-preset-read-old-homes nil
  "Whether the directive homes of before are still read as presets.
Off, only the presets homes count and the old files wait for
ygg-preset-migrate; on, they are read behind the presets homes."
  :type 'boolean :group 'ygg-preset)

(defun ygg-preset--sources (root)
  "Every entry offered for ROOT, weakest first.
The config's own presets, then the owner's, then the checkout's: a later
source overrides the fields of an earlier one and appends to its body.
Only a presets home is read; nothing under a skills directory is one."
  (append
   (ygg-preset--read-dir ygg-preset-config-directory)
   (when ygg-preset-read-old-homes
     (append
      (ygg-preset--read-dir ygg-preset-old-user-directory t)
      (ygg-preset--read-dir (expand-file-name ygg-preset-old-subdirectory
                                              ygg-preset-old-user-directory)
                            t)))
   (ygg-preset--read-dir ygg-preset-user-directory)
   (when root
     (let ((old (expand-file-name ygg-preset-old-project-directory root)))
       (append
        (when ygg-preset-read-old-homes
          (append
           (ygg-preset--read-dir old t t)
           (ygg-preset--read-dir (expand-file-name ygg-preset-old-subdirectory old)
                                 t t)))
        (ygg-preset--read-dir (expand-file-name ygg-preset-project-directory root)
                              nil t))))))

(defun ygg-preset--fold (name entries)
  "The one preset NAME becomes, ENTRIES folded weakest first."
  (let (fields bodies files local)
    (pcase-dolist (`(,_ ,file ,efields ,body ,elocal) entries)
      (cl-loop for (k v) on efields by #'cddr
               do (setq fields (plist-put fields k v)))
      (when (and body (not (string-empty-p body))) (push body bodies))
      (push file files)
      (when elocal (setq local t)))
    (ygg-preset--new
     :name name
     :body (string-trim (string-join (nreverse bodies) "\n\n"))
     :fields fields
     :files (nreverse files)
     :local local)))

(defun ygg-preset-list (&optional root)
  "Every preset available in ROOT, its sources folded in override order.
A preset naming another in its preset field carries it along resolved,
and one naming a preset nobody wrote is simply itself."
  (let* ((entries (ygg-preset--sources root))
         (names (delete-dups (mapcar #'car entries)))
         (ps (mapcar (lambda (n)
                       (ygg-preset--fold
                        n (seq-filter (lambda (e) (equal (car e) n)) entries)))
                     (sort names #'string<))))
    (dolist (p ps ps)
      (when-let* ((named (ygg-preset-field p :preset)))
        (setf (ygg-preset--slot-preset p)
              (seq-find (lambda (o) (and (not (eq o p))
                                         (equal (ygg-preset-name o) named)))
                        ps))))))

(defun ygg-preset-get (name &optional root)
  "The preset called NAME available in ROOT, or nil when none is."
  (seq-find (lambda (d) (equal (ygg-preset-name d) name))
            (ygg-preset-list root)))

(defun ygg-preset-local-p (d)
  "Whether D was shaped by the checkout rather than taken as written."
  (and (ygg-preset--slot-local d) t))

(defun ygg-preset-memory-file (d &optional root)
  "Where D writes down what it learned, or nil when it remembers nothing.
A project memory stays with the repository that taught it, a user one
with the words themselves, so a lesson travels only as far as it is
true."
  (pcase (ygg-preset-setting d :memory)
    ("project" (and root (expand-file-name
                          (format "%s/%s.md" ygg-preset-project-memory-directory
                                  (ygg-preset-name d))
                          root)))
    ("user" (expand-file-name
             (format "%s/%s.md" ygg-preset-user-memory-subdirectory
                     (ygg-preset-name d))
             ygg-preset-user-directory))))

(defun ygg-preset--old-memory-file (d)
  "Where D wrote down what it learned before the home moved, or nil."
  (when (equal (ygg-preset-setting d :memory) "user")
    (expand-file-name (format "%s/%s.md" ygg-preset-user-memory-subdirectory
                              (ygg-preset-name d))
                      ygg-preset-old-user-directory)))

(defun ygg-preset-memory (d &optional root)
  "What D has learned here, or nil while it has learned nothing yet."
  (when-let* ((file (seq-find #'file-readable-p
                              (delq nil (list (ygg-preset-memory-file d root)
                                              (ygg-preset--old-memory-file d)))))
              (text (with-temp-buffer (insert-file-contents file) (buffer-string)))
              ((not (string-empty-p (string-trim text)))))
    text))

(defun ygg-preset-remember (d text &optional root)
  "Add TEXT to what D knows here, dated, so it is not learned a second time."
  (when-let* ((file (ygg-preset-memory-file d root)))
    (make-directory (file-name-directory file) t)
    (write-region (format "- %s %s\n" (format-time-string "%Y-%m-%d")
                          (string-trim text))
                  nil file 'append 'silent)
    file))

(defun ygg-preset--move-into (from to)
  "Move every markdown file in FROM into TO, and say which were moved."
  (when (file-directory-p from)
    (let ((files (directory-files from t "\\.md\\'")))
      (when files (make-directory to t))
      (mapcar (lambda (f)
                (let ((dest (expand-file-name (file-name-nondirectory f) to)))
                  (rename-file f dest t)
                  dest))
              files))))

;;;###autoload
(defun ygg-preset-migrate (&optional root)
  "Move the presets out of the homes they were kept in as directives.
The owner's own files and what they remembered go to
ygg-preset-user-directory, and ROOT's to its presets directory."
  (interactive (list (and (fboundp 'ygg-task-root-here) (ygg-task-root-here))))
  (let* ((old ygg-preset-old-user-directory)
         (moved (append
                 (ygg-preset--move-into old ygg-preset-user-directory)
                 (ygg-preset--move-into
                  (expand-file-name ygg-preset-old-subdirectory old)
                  ygg-preset-user-directory)
                 (ygg-preset--move-into
                  (expand-file-name ygg-preset-user-memory-subdirectory old)
                  (expand-file-name ygg-preset-user-memory-subdirectory
                                    ygg-preset-user-directory))
                 (when root
                   (let ((oldp (expand-file-name ygg-preset-old-project-directory
                                                 root))
                         (new (expand-file-name ygg-preset-project-directory root)))
                     (append (ygg-preset--move-into oldp new)
                             (ygg-preset--move-into
                              (expand-file-name ygg-preset-old-subdirectory oldp)
                              new)))))))
    (when (called-interactively-p 'interactive)
      (message "%d preset%s moved" (length moved) (if (= 1 (length moved)) "" "s")))
    moved))

(defun ygg-preset-gradable-p (d)
  "Whether D says how it is checked, which is what decides its regime.
Without a check nothing can accept for you, so the preset goes serial
however its fanout field is written."
  (and (stringp (ygg-preset-setting d :check))
       (not (string-empty-p (ygg-preset-setting d :check)))))

(defun ygg-preset-goal (d)
  "D's goal as prose when it settles one, else nil.
A dispatch with a goal carries it as prose in its prompt."
  (let ((goal (ygg-preset-setting d :goal)))
    (when (and (stringp goal) (not (string-empty-p goal))) goal)))

(defun ygg-preset-fanout-p (d)
  "Whether D may run wide here.  A preset nothing can grade may not."
  (and (ygg-preset-gradable-p d)
       (or (not (ygg-preset-settles-p d :fanout))
           (ygg-preset-setting d :fanout))))

(defun ygg-preset-subagents (d)
  "The most helpers a shot under D may send, zero when it names none.
A preset that says nothing means none: a one turn run that spawns a
crowd nobody asked for is a run the owner cannot read."
  (let ((raw (ygg-preset-setting d :subagents)))
    (cond
     ((numberp raw) (max 0 (truncate raw)))
     ((and (stringp raw) (string-match-p "\\`[0-9]+\\'" (string-trim raw)))
      (string-to-number (string-trim raw)))
     (t 0))))

(defun ygg-preset-workers (d)
  "The worker levels D names, as plists of :name and, when given, :model
and :effort.  Written on one line, workers: build, quick=sonnet/low; a
bare name leaves the level's model and effort to whoever opens the
session, and an entry that parses as neither is dropped."
  (let ((raw (ygg-preset-setting d :workers)))
    (delq nil
          (mapcar (lambda (entry)
                    (let ((e (string-trim entry)))
                      (cond
                       ((string-match "\\`\\([[:alnum:]_-]+\\)=\\([^/ ]+\\)/\\([[:alpha:]]+\\)\\'" e)
                        (list :name (match-string 1 e) :model (match-string 2 e)
                              :effort (downcase (match-string 3 e))))
                       ((string-match-p "\\`[[:alnum:]_-]+\\'" e) (list :name e)))))
                  (cond ((stringp raw) (split-string raw "," t))
                        ((listp raw) raw))))))

(defcustom ygg-preset-worker-levels '(("quick" :preset "search" :read-only t)
                                      ("gaps" :preset "gaps")
                                      ("build" :skill "build")
                                      ("deep" :skill "build")
                                      ("review" :skill "ice-review-loop" :read-only t)
                                      ("ui" :skill "ice-ui-review" :read-only t)
                                      ("verify" :skill "regrade"))
  "What a worker level is beyond its model and effort, by level name.
:preset names the preset whose body the level runs under in place of the
carried one, :skill names a skill whose body it runs under instead, and
:read-only gives it the tools that read and none that change anything."
  :type '(alist :key-type string :value-type plist) :group 'ygg-preset)

(defun ygg-preset--worker-level (w known)
  "W with the prompt and read-only its level takes from ygg-preset-worker-levels.
The prompt is the body of the level's preset, found among KNOWN, or of
its skill, found from the default directory the way a mode finds one.
A level whose skill nobody wrote keeps the carried prompt."
  (let* ((spec (cdr (assoc (plist-get w :name) ygg-preset-worker-levels)))
         (d (and (plist-get spec :preset)
                 (seq-find (lambda (d) (equal (ygg-preset-name d) (plist-get spec :preset)))
                           known)))
         (body (cond (d (or (ygg-preset-body d) ""))
                     ((plist-get spec :skill)
                      (ygg-preset-skill-body (plist-get spec :skill) default-directory)))))
    (append w
            (and body (list :prompt (string-trim body)))
            (and (plist-get spec :read-only) (list :read-only t)))))

(defun ygg-preset-subagent-refs (presets &optional known)
  "The session refs the first of PRESETS to settle subagents asks for.
The cap is how many workers may be out at once.  A preset that carries
another to its workers orchestrates them, and each goes out with a brief.
Its worker levels go along with the body of the preset it carries, found
among KNOWN, as the prompt every level runs under, save a level that
ygg-preset-worker-levels gives a preset of its own."
  (when-let* ((p (seq-find (lambda (d) (ygg-preset-settles-p d :subagents))
                           presets)))
    (let* ((workers (mapcar (lambda (w) (ygg-preset--worker-level w known))
                            (ygg-preset-workers p)))
           (carried (seq-find (lambda (d) (equal (ygg-preset-name d)
                                                 (car (ygg-preset-carries p))))
                              known)))
      (append (list :subagent-cap (ygg-preset-subagents p))
              (and (ygg-preset-carries p) (list :subagent-briefs t))
              (and workers (list :workers workers))
              (and workers carried
                   (list :worker-prompt (string-trim (or (ygg-preset-body carried) ""))))))))

(defconst ygg-preset-modes '("one-shot" "interactive")
  "The ways the harness itself works, which a mode field may name.
One shot is one turn with the whole context supplied and a verdict at
the end of it; interactive is a talking session that runs as long as the
work takes and ends on what it leaves behind.")

(defun ygg-preset-mode (d)
  "The mode D runs as, or nil when it names none.
A mode is the harness's own way of working, one of the words
ygg-preset-modes carries, rather than a preset a task picks up.  Any
other value in that field belongs to a preset that is not a mode."
  (let ((raw (ygg-preset-setting d :mode)))
    (and (stringp raw) (car (member (string-trim raw) ygg-preset-modes)))))

(defun ygg-preset-interactive-p (d)
  "Whether D runs as a talking session rather than as one turn."
  (equal (ygg-preset-mode d) "interactive"))

(defun ygg-preset-model-for (raw agent)
  "The model RAW names for AGENT, or nil when it names none for it.
RAW is a model field: a plain name stands for every agent, and a list of
agent=model pairs answers only for the agents it names."
  (let ((cells (cond ((consp raw) (seq-filter #'stringp raw))
                     ((stringp raw) (split-string raw "," t))
                     (t nil))))
    (when cells
      (if (seq-some (lambda (cell) (string-match-p "=" cell)) cells)
          (seq-some (lambda (cell)
                      (when (string-match "\\`[ \t]*\\([^=]+?\\)[ \t]*=[ \t]*\\(.*\\)\\'"
                                          cell)
                        (and agent (equal (match-string 1 cell) agent)
                             (let ((model (string-trim (match-string 2 cell))))
                               (unless (string-empty-p model) model)))))
                    cells)
        (let ((only (string-trim (car cells))))
          (unless (string-empty-p only) only))))))

(defun ygg-preset-model (d agent)
  "The model D runs AGENT on, or nil when it names none for AGENT.
A model field written as one name stands for every agent; written as
agent=model pairs it answers only for the agents it names, so one preset
carries a model for each agent it may run on."
  (ygg-preset-model-for (ygg-preset-setting d :model) agent))

(defun ygg-preset-agent (d)
  "The ACP agent D runs on, or nil when it names none.
The name is handed back as written: whether an agent by that name exists
is the launch's question, not the reader's."
  (let ((raw (ygg-preset-setting d :agent)))
    (when (stringp raw)
      (let ((name (string-trim raw)))
        (unless (string-empty-p name) name)))))

(defun ygg-preset-tools (d)
  "The tools D lets its agent use, by name, or nil when it names none."
  (ygg-preset-names (ygg-preset-setting d :tools)))

(defconst ygg-preset-thinking-levels '("off" "low" "medium" "high" "xhigh" "max")
  "What a thinking field may say, least first.")

(defun ygg-preset-thinking (d)
  "How hard D's agent thinks, one of ygg-preset-thinking-levels, or nil."
  (let ((raw (ygg-preset-setting d :thinking)))
    (and (stringp raw)
         (car (member (downcase (string-trim raw)) ygg-preset-thinking-levels)))))

(defun ygg-preset-modes (&optional root)
  "Every preset available in ROOT that names a mode, as the reader folds it.
These are what a launch is offered first, since a mode is how the harness
works and every other preset is something added to one."
  (seq-filter #'ygg-preset-mode (ygg-preset-list root)))

(defun ygg-preset-annotation (d)
  "The one line the completion shows beside D: what it will do here."
  (string-join
   (delq nil
         (list (when-let* ((c (ygg-preset-setting d :check)))
                 (format "· %s" (if (> (length c) 28)
                                    (concat (substring c 0 27) "…")
                                  c)))
               (when-let* ((a (ygg-preset-agent d))) (format "· on %s" a))
               (when-let* ((m (ygg-preset-setting d :model))) (format "· %s" m))
               (when-let* ((w (ygg-preset-setting d :workers))) (format "· workers %s" w))
               (cond ((ygg-preset-fanout-p d) "· fanout")
                     ((ygg-preset-gradable-p d) "· serial")
                     (t "· ungraded, needs your eye"))
               (when-let* ((r (ygg-preset-setting d :resource))) (format "· %s" r))
               (when-let* ((p (ygg-preset-preset d)))
                 (format "· preset %s" (ygg-preset-name p)))
               (when (ygg-preset-setting d :memory) "· remembers")
               (when (ygg-preset-goal d) "· goal")
               (when (ygg-preset-local-p d) "· local")))
   " "))

;;; Editing — a preset is a file, and this opens the one that wins

(defvar ygg-aob--preset-cache)
(declare-function project-root "project" (project))

(defun ygg-preset--root ()
  "The checkout the presets are read for: the draft's, else this buffer's."
  (or (bound-and-true-p aob-compose--dir)
      (when-let* (((fboundp 'project-current))
                  (pr (project-current nil)))
        (project-root pr))
      default-directory))

(defun ygg-preset--scope (file root)
  "Where FILE sits, as the word for it: project, user or config."
  (let ((file (expand-file-name file)))
    (cond ((and root (string-prefix-p (expand-file-name ygg-preset-project-directory root) file))
           "project")
          ((string-prefix-p (expand-file-name ygg-preset-user-directory) file) "user")
          (t "config"))))

(defun ygg-preset--read-name (prompt root)
  "Ask for a preset name, offering ROOT's presets with where each is written."
  (let* ((ps (ygg-preset-list root))
         (ann (mapcar (lambda (d)
                        (cons (ygg-preset-name d)
                              (concat "  "
                                      (mapconcat (lambda (f) (ygg-preset--scope f root))
                                                 (ygg-preset-files d) "+")
                                      "  "
                                      (or (ygg-preset-field d :description) ""))))
                      ps)))
    (completing-read prompt
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata (annotation-function
                                       . ,(lambda (c) (cdr (assoc c ann)))))
                         (complete-with-action action (mapcar #'car ann) str pred))))))

(defun ygg-preset--open (file)
  "Visit FILE, and forget the presets compose has read once it is saved."
  (find-file file)
  (add-hook 'after-save-hook
            (lambda () (when (boundp 'ygg-aob--preset-cache)
                         (clrhash ygg-aob--preset-cache)))
            nil t))

;;;###autoload
(defun ygg-preset-edit (name)
  "Open the file preset NAME is written in, the one that wins.
A preset folded from several places opens the most particular of them,
the project's before yours before the config's.  A name no preset has
starts a new one."
  (interactive (list (ygg-preset--read-name "Edit preset: " (ygg-preset--root))))
  (let* ((root (ygg-preset--root))
         (d (seq-find (lambda (x) (equal (ygg-preset-name x) name))
                      (ygg-preset-list root))))
    (if d
        (ygg-preset--open (car (last (ygg-preset-files d))))
      (ygg-preset-new name))))

;;;###autoload
(defun ygg-preset-new (name &optional scope)
  "Start preset NAME in SCOPE: this project, your own, or the config's."
  (interactive (list (read-string "New preset: ")))
  (let* ((root (ygg-preset--root))
         (scope (or scope (completing-read "Where: " '("project" "user" "config") nil t
                                           nil nil "project")))
         (dir (pcase scope
                ("project" (expand-file-name ygg-preset-project-directory root))
                ("user" ygg-preset-user-directory)
                (_ ygg-preset-config-directory)))
         (slug (replace-regexp-in-string "[^a-z0-9-]+" "-" (downcase (string-trim name))))
         (file (expand-file-name (concat slug ".md") dir)))
    (when (string-empty-p slug) (user-error "preset: a name is needed"))
    (make-directory dir t)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "---\nname: " slug "\ndescription: \n---\n\n# " name "\n\n")))
    (ygg-preset--open file)
    (goto-char (point-min))
    (when (re-search-forward "^description: " nil t) (end-of-line))))

(provide 'ygg-preset)
;;; ygg-preset.el ends here
