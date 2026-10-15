;;; ygg-git-compare-images.el --- pictures in a pull request's description -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'image)
(require 'ygg-git-compare)
(require 'ygg-git-compare-threads)

(defcustom ygg-git-compare-image-directory
  (locate-user-emacs-file "var/ygg-git-compare-images/")
  "Where the pictures fetched for descriptions are kept.
The directory is private to you; one owned by anyone else is not used."
  :type 'directory
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-image-lines 16
  "How many lines tall a picture in a description is drawn at most."
  :type 'natnum
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-image-bytes (* 8 1024 1024)
  "The largest picture fetched for a description."
  :type 'natnum
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-image-max 20
  "The most pictures fetched for one description; later ones show their alt text."
  :type 'natnum
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-image-cache-bytes (* 200 1024 1024)
  "How large the picture directory may grow before its oldest files are removed."
  :type 'natnum
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-image-jobs 4
  "How many pictures are fetched at once."
  :type 'natnum
  :group 'ygg-git-compare)

(define-obsolete-variable-alias 'ygg-git-compare-gif-loops 'ygg-git-compare-gif-seconds "31")

(defcustom ygg-git-compare-gif-seconds 10
  "How many seconds an animated picture plays; 0 leaves it on its first frame."
  :type 'natnum
  :group 'ygg-git-compare)


(defcustom ygg-git-compare-image-retry 120
  "Seconds before a picture that could not be fetched is asked for again."
  :type 'number
  :group 'ygg-git-compare)

(defcustom ygg-git-compare-image-external nil
  "Whether a description may load pictures from hosts other than its forge's.
Nil loads only the forge's own hosts; t loads any https host.  Plain http is
never loaded."
  :type 'boolean
  :group 'ygg-git-compare)

(defvar ygg-git-compare-image-fetcher #'ygg-git-compare--image-download
  "Function of (URL PR DONE) that fetches the picture at URL in the
background and calls DONE with (:file FILE :type TYPE), or nil.")

(defvar ygg-git-compare--images (make-hash-table :test #'equal)
  "A picture's source to (:file FILE :type TYPE), (:pending) or (:failed TIME).")

(defvar ygg-git-compare--image-waiting (make-hash-table :test #'equal)
  "A picture's source to the buffers that wait for it.")

(defvar ygg-git-compare--image-queue nil)

(defvar ygg-git-compare--image-active 0)

(defvar ygg-git-compare--image-objects (make-hash-table :test #'equal)
  "FILE to (STAMP . IMAGE), the one image drawn for it.")

(defconst ygg-git-compare--image-markdown-re
  "!\\[\\([^]\n]*\\)\\](\\([^)[:space:]]+\\)\\(?:[ ]+\"[^\"]*\"\\)?)")

(defconst ygg-git-compare--image-re
  (concat ygg-git-compare--image-markdown-re
          "\\|<img\\b\\(\\(?:[^>\"']\\|\"[^\"]*\"\\|'[^']*'\\)*\\)>"))

(defconst ygg-git-compare--image-line-limit 4000
  "Longer lines are prose; they are not searched for pictures.")

(defun ygg-git-compare--image-regexp (line)
  "The pattern for the pictures in LINE; an <img> needs a closing > on it."
  (if (string-search ">" line)
      ygg-git-compare--image-re
    ygg-git-compare--image-markdown-re))

(defconst ygg-git-compare--image-entities
  '(("&quot;" . "\"") ("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&")))

(defun ygg-git-compare--image-decode (text)
  "TEXT with its HTML entities read once."
  (replace-regexp-in-string
   "&\\(?:quot\\|lt\\|gt\\|amp\\|#[0-9]+\\|#[xX][[:xdigit:]]+\\);"
   (lambda (entity)
     (or (cdr (assoc entity ygg-git-compare--image-entities))
         (let ((code (if (memq (aref entity 2) '(?x ?X))
                         (string-to-number (substring entity 3 -1) 16)
                       (string-to-number (substring entity 2 -1)))))
           (if (and (> code 0) (<= code #x10FFFF) (not (<= #xD800 code #xDFFF)))
               (string code)
             entity))))
   text t t))

(defun ygg-git-compare--image-attribute (attributes name)
  "The decoded value of the first attribute called NAME in ATTRIBUTES.
The attributes are read left to right, so text inside a value is never a name."
  (let ((pos 0)
        found)
    (while (and (not found)
                (string-match "\\([^[:space:]=\"'/]+\\)\\(?:[[:space:]]*=[[:space:]]*\\(?:\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\|\\([^[:space:]\"']+\\)\\)\\)?"
                              attributes pos))
      (setq pos (max (match-end 0) (1+ pos)))
      (when (string-equal-ignore-case (match-string 1 attributes) name)
        (setq found (or (match-string 2 attributes)
                        (match-string 3 attributes)
                        (match-string 4 attributes)
                        ""))))
    (and found (ygg-git-compare--image-decode found))))

(defun ygg-git-compare--image-item (line)
  "The picture the last match in LINE read, as (:image URL :alt ALT :width PX),
or nil when it names none."
  (let* ((attributes (match-string 3 line))
         (url (if attributes
                  (ygg-git-compare--image-attribute attributes "src")
                (match-string 2 line)))
         (alt (if attributes
                  (ygg-git-compare--image-attribute attributes "alt")
                (match-string 1 line)))
         (width (and attributes (ygg-git-compare--image-attribute attributes "width"))))
    (when (and url (not (string-empty-p url)))
      (list :image url :alt (string-trim (or alt ""))
            :width (and width (string-match-p "\\`[0-9]+\\'" width)
                        (string-to-number width))))))

(defun ygg-git-compare--image-split-text (line)
  (let ((line (replace-regexp-in-string
               (concat "\\[\\(!\\[[^]\n]*\\]" ygg-git-compare--remote-url-re "\\)\\]"
                       ygg-git-compare--remote-url-re)
               "\\1" line))
        (pos 0)
        out)
    (when (string-match-p "<img\\b" line)
      (setq line (replace-regexp-in-string "</?\\(?:a\\|p\\|picture\\|source\\)\\b[^>]*>" "" line)))
    (while (string-match (ygg-git-compare--image-regexp line) line pos)
      (let ((item (ygg-git-compare--image-item line)))
        (push (substring line pos (match-beginning 0)) out)
        (when item (push item out))
        (setq pos (match-end 0))))
    (push (substring line pos) out)
    (nreverse out)))

(defun ygg-git-compare--image-split (line)
  "LINE as the text before, between and after its pictures, which are plists.
Code spans stay text."
  (let ((pos 0)
        out)
    (cl-flet ((add (piece)
                (if (and (stringp piece) (stringp (car out)))
                    (setcar out (concat (car out) piece))
                  (push piece out))))
      (while (string-match "\\(`+\\)\\(?:[^`]\\|[^`].*?[^`]\\)\\1\\(?:[^`]\\|\\'\\)" line pos)
        (let ((end (if (= (match-end 0) (length line)) (match-end 0) (1- (match-end 0)))))
          (mapc #'add (ygg-git-compare--image-split-text (substring line pos (match-beginning 0))))
          (add (substring line (match-beginning 0) end))
          (setq pos end)))
      (mapc #'add (ygg-git-compare--image-split-text (substring line pos))))
    (nreverse out)))

(defun ygg-git-compare--image-parts (text)
  "TEXT as its prose, in strings, and its pictures, in plists, in order; the
fenced blocks are left whole."
  (let ((count 0)
        parts prose fence pieces)
    (cl-flet ((flush ()
                (when prose
                  (push (string-join (nreverse prose) "\n") parts)
                  (setq prose nil))))
      (dolist (line (split-string (ygg-git-compare--remote-uncommented text) "\n"))
        (cond
         (fence
          (push line prose)
          (when (ygg-git-compare--remote-closes-p line fence) (setq fence nil)))
         ((string-match ygg-git-compare--remote-fence-re line)
          (setq fence (match-string 1 line))
          (push line prose))
         ((string-match-p "\\`\\(?: \\{4\\}\\|\t\\)" line)
          (push line prose))
         ((and (<= (length line) ygg-git-compare--image-line-limit)
               (string-match-p (ygg-git-compare--image-regexp line) line)
               (seq-some #'consp (setq pieces (ygg-git-compare--image-split line))))
          (dolist (piece pieces)
            (if (stringp piece)
                (unless (string-blank-p piece) (push (string-trim piece) prose))
              (flush)
              (push (if (> (cl-incf count) ygg-git-compare-image-max)
                        (append piece '(:limited t))
                      piece)
                    parts))))
         (t (push line prose))))
      (flush))
    (nreverse parts)))

(defun ygg-git-compare--image-userinfo-p (url)
  (string-match-p "\\`[[:alpha:]]+://[^/?#]*@" url))

(defun ygg-git-compare--image-source (url pr)
  "URL as (remote . ABSOLUTE-URL), or nil, for the description of PR.  A GitLab
upload path is relative to the project; local files are never read, and an
address with a user name in it is refused."
  (let ((source (ygg-git-compare--image-source-1 url pr)))
    (and source
         (not (ygg-git-compare--image-userinfo-p (cdr source)))
         source)))

(defun ygg-git-compare--image-source-1 (url pr)
  (cond
   ((string-match-p "\\`https?://" url) (cons 'remote url))
   ((string-prefix-p "//" url) (cons 'remote (concat "https:" url)))
   ((and pr (string-match "\\`/?\\(uploads/.*\\)" url) (eq (plist-get pr :forge) 'gitlab))
    (cons 'remote (format "https://%s/%s/%s" (plist-get pr :host) (plist-get pr :path)
                          (match-string 1 url))))
   ((and pr (string-prefix-p "/" url) (not (string-prefix-p "//" url)))
    (cons 'remote (concat "https://" (plist-get pr :host) url)))))

(defun ygg-git-compare--image-host (url)
  (and (not (ygg-git-compare--image-userinfo-p url))
       (string-match "\\`https?://\\([[:alnum:].-]+\\)\\(?::443\\)?\\(?:[/?#]\\|\\'\\)" url)
       (downcase (match-string 1 url))))

(defun ygg-git-compare--image-trusted-p (url pr)
  "Whether URL is https on PR's forge host or that forge's own asset hosts."
  (when-let* (((string-prefix-p "https://" url))
              (host (ygg-git-compare--image-host url)))
    (or (and pr (string-equal-ignore-case host (plist-get pr :host)))
        (and pr (eq (plist-get pr :forge) 'github)
             (or (equal host "github.com")
                 (string-suffix-p ".githubusercontent.com" host)
                 (equal host "githubusercontent.com"))))))

(defun ygg-git-compare--image-allowed-p (url pr)
  (and (string-prefix-p "https://" url)
       (ygg-git-compare--image-host url)
       (or ygg-git-compare-image-external
           (ygg-git-compare--image-trusted-p url pr))))

(defun ygg-git-compare--image-token-command (url pr)
  "The command that prints the token to send with URL: the forge's own, for a
URL on PR's host, nil for any other."
  (when-let* (((and pr (string-prefix-p "https://" url)))
              (host (ygg-git-compare--image-host url))
              ((string-equal-ignore-case host (plist-get pr :host))))
    (if (eq (plist-get pr :forge) 'gitlab)
        (list "glab" "config" "get" "token" "--host" (plist-get pr :host))
      (list "gh" "auth" "token" "--hostname" (plist-get pr :host)))))

(defun ygg-git-compare--image-curl-command (url part authorized)
  "The curl command that saves URL as PART, reading the authorization header
from standard input when AUTHORIZED."
  (append (list "curl" "-q" "--globoff" "--silent" "--location" "--fail" "--max-time" "30"
                "--max-filesize" (number-to-string ygg-git-compare-image-bytes)
                "--proto" "=https" "--proto-redir" "=https")
          (and authorized (list "--config" "-"))
          (list "--output" part "--url" url)))

(defun ygg-git-compare--image-cache-file (url)
  (expand-file-name (md5 url) ygg-git-compare-image-directory))

(defun ygg-git-compare--image-found (file)
  (when-let* ((type (and (file-regular-p file) (ignore-errors (image-type-from-file-header file))))
              ((memq type '(png jpeg gif webp))))
    (list :file file :type type)))

(defun ygg-git-compare--image-part-file (cache)
  "The scratch name a fetch of CACHE downloads to, private to this Emacs."
  (format "%s.%d.part" cache (emacs-pid)))

(defvar ygg-git-compare--image-pruned nil
  "Whether the picture directory was trimmed in this session.")

(defun ygg-git-compare--image-prune ()
  "Delete the least recently read pictures until the directory fits its limit."
  (setq ygg-git-compare--image-pruned t)
  (ignore-errors
    (let ((files (seq-filter
                  (lambda (entry)
                    (and (null (file-attribute-type (cdr entry)))
                         (not (string-suffix-p ".part" (car entry)))))
                  (directory-files-and-attributes
                   ygg-git-compare-image-directory t directory-files-no-dot-files-regexp t)))
          (total 0))
      (dolist (entry files)
        (cl-incf total (file-attribute-size (cdr entry))))
      (when (> total ygg-git-compare-image-cache-bytes)
        (dolist (entry (sort files (lambda (a b)
                                     (time-less-p (file-attribute-access-time (cdr a))
                                                  (file-attribute-access-time (cdr b))))))
          (when (> total ygg-git-compare-image-cache-bytes)
            (ignore-errors (delete-file (car entry)))
            (cl-decf total (file-attribute-size (cdr entry)))))))))

(defun ygg-git-compare--image-prepare-directory ()
  "Make the picture directory private to you, and say whether it is usable."
  (let ((dir (directory-file-name ygg-git-compare-image-directory)))
    (unless (file-exists-p dir)
      (with-file-modes #o700 (make-directory dir t)))
    (let ((attributes (file-attributes dir 'integer)))
      (and (eq (file-attribute-type attributes) t)
           (= (file-attribute-user-id attributes) (user-uid))
           (progn (set-file-modes dir #o700) t)))))

(defun ygg-git-compare--image-oversize-p (part)
  (when-let* ((size (file-attribute-size (file-attributes part))))
    (> size ygg-git-compare-image-bytes)))

(defun ygg-git-compare--image-curl (url config done &optional key)
  "Fetch URL with curl, CONFIG on its standard input, then call DONE.  The file
is cached under KEY, URL by default."
  (let* ((cache (ygg-git-compare--image-cache-file (or key url)))
         (part (ygg-git-compare--image-part-file cache))
         (default-directory (if (file-remote-p default-directory)
                                temporary-file-directory
                              default-directory))
         (finished nil)
         (timer nil)
         (finish (lambda (found)
                   (unless finished
                     (setq finished t)
                     (when timer (cancel-timer timer))
                     (ignore-errors (delete-file part))
                     (funcall done found))))
         (process
          (condition-case nil
              (when (ygg-git-compare--image-prepare-directory)
                (unless ygg-git-compare--image-pruned (ygg-git-compare--image-prune))
                (make-process
                 :name "ygg-git-compare-image" :buffer nil :noquery t
                 :connection-type 'pipe
                 :command (ygg-git-compare--image-curl-command url part (and config t))
                 :sentinel (lambda (process _event)
                             (unless (or finished (process-live-p process))
                               (funcall finish
                                        (ignore-errors
                                          (and (zerop (process-exit-status process))
                                               (not (ygg-git-compare--image-oversize-p part))
                                               (file-exists-p part)
                                               (progn (rename-file part cache t)
                                                      (or (ygg-git-compare--image-found cache)
                                                          (progn (delete-file cache) nil))))))))))
            (error nil))))
    (if (null process)
        (funcall finish nil)
      (unless finished
        (setq timer (run-with-timer
                     0.5 0.5
                     (lambda ()
                       (when (ygg-git-compare--image-oversize-p part)
                         (delete-process process)
                         (funcall finish nil))))))
      (when (and config (not finished))
        (condition-case nil
            (progn (process-send-string process config)
                   (process-send-eof process))
          (error (delete-process process)
                 (funcall finish nil)))))))

(defun ygg-git-compare--image-signed-command (url pr)
  "The command that prints the rendered body of PR, where the attachment at
URL has a signed address, or nil when URL is no GitHub attachment."
  (when (and pr (eq (plist-get pr :forge) 'github)
             (string-match-p "/user-attachments/\\(?:assets\\|files\\)/" url))
    (list "gh" "api" "--hostname" (plist-get pr :host)
          "-H" "Accept: application/vnd.github.full+json" "--jq" ".body_html"
          (format "repos/%s/pulls/%s" (plist-get pr :path) (plist-get pr :number)))))

(defun ygg-git-compare--image-signed-url (html url)
  "The address in HTML of the attachment at URL on GitHub's content host, or nil."
  (let ((name (file-name-nondirectory (replace-regexp-in-string "[?#].*\\'" "" url)))
        (pos 0)
        found)
    (unless (string-empty-p name)
      (while (and (not found)
                  (string-match "\\(?:src\\|href\\)=\"\\([^\"]*\\)\"" html pos))
        (setq pos (match-end 0))
        (let* ((candidate (ygg-git-compare--image-decode (match-string 1 html)))
               (host (ygg-git-compare--image-host candidate)))
          (when (and (string-prefix-p "https://" candidate)
                     host
                     (string-suffix-p ".githubusercontent.com" host)
                     (string-search name (replace-regexp-in-string "[?#].*\\'" "" candidate)))
            (setq found candidate)))))
    found))

(defun ygg-git-compare--image-fetch-signed (url pr done)
  (if-let* ((command (ygg-git-compare--image-signed-command url pr)))
      (ygg-git-compare--forge-async
       (car command) (cdr command)
       (lambda (status text _err)
         (if-let* (((eql status 0))
                   (signed (ygg-git-compare--image-signed-url text url))
                   ((ygg-git-compare--image-trusted-p signed pr)))
             (ygg-git-compare--image-curl signed nil done url)
           (funcall done nil))))
    (funcall done nil)))

(defun ygg-git-compare--image-download (url pr done)
  "Fetch URL for PR's description into the cache, then call DONE with (:file
FILE :type TYPE), or nil.  A URL on the forge's host is fetched with its token,
a GitHub attachment that way refused at the signed address of the body."
  (let ((done (lambda (found)
                (if found
                    (funcall done found)
                  (ygg-git-compare--image-fetch-signed url pr done)))))
    (if-let* ((command (ygg-git-compare--image-token-command url pr)))
        (ygg-git-compare--forge-async
         (car command) (cdr command)
         (lambda (status text _err)
           (let ((token (and (eql status 0) (string-trim text))))
             (ygg-git-compare--image-curl
              url
              (and token (string-match-p "\\`[[:alnum:]_.-]+\\'" token)
                   (format "header = \"Authorization: Bearer %s\"\n" token))
              done))))
      (ygg-git-compare--image-curl url nil done))))

(defun ygg-git-compare--image-settle (source found)
  (puthash source (or found (list :failed (float-time))) ygg-git-compare--images)
  (let ((buffers (gethash source ygg-git-compare--image-waiting)))
    (remhash source ygg-git-compare--image-waiting)
    (dolist (buffer buffers)
      (condition-case nil
          (ygg-git-compare--image-paint buffer source)
        (error nil)))))

(defun ygg-git-compare--image-pump ()
  (while (and ygg-git-compare--image-queue
              (< ygg-git-compare--image-active ygg-git-compare-image-jobs))
    (pcase-let ((`(,source ,pr ,dir) (pop ygg-git-compare--image-queue)))
      (cl-incf ygg-git-compare--image-active)
      (let ((default-directory dir)
            (finished nil))
        (cl-flet ((finish (found)
                    (unless finished
                      (setq finished t)
                      (cl-decf ygg-git-compare--image-active)
                      (unwind-protect (ygg-git-compare--image-settle source found)
                        (ygg-git-compare--image-pump)))))
          (condition-case nil
              (funcall ygg-git-compare-image-fetcher source pr #'finish)
            (error (finish nil))))))))

(defun ygg-git-compare--image-state (source pr)
  "What is known of the picture at SOURCE, a remote URL, as a
plist, asking for it in the background when it is not."
  (let ((state (gethash source ygg-git-compare--images)))
    (when (and (plist-get state :failed)
               (> (- (float-time) (plist-get state :failed)) ygg-git-compare-image-retry))
      (setq state nil))
    (when (plist-member state :pending)
      (cl-pushnew (current-buffer) (gethash source ygg-git-compare--image-waiting)))
    (or state
        (let ((found (and (ygg-git-compare--image-prepare-directory)
                          (or (ygg-git-compare--image-found (ygg-git-compare--image-cache-file source))
                              'missing))))
          (cond
           ((null found)
            (puthash source (list :failed (float-time)) ygg-git-compare--images))
           ((consp found) (puthash source found ygg-git-compare--images))
           (t
            (push (current-buffer) (gethash source ygg-git-compare--image-waiting))
            (setq ygg-git-compare--image-queue
                  (append ygg-git-compare--image-queue (list (list source pr default-directory))))
            (puthash source (list :pending) ygg-git-compare--images)
            (ygg-git-compare--image-pump)
            (gethash source ygg-git-compare--images)))))))

(defun ygg-git-compare--image-window ()
  (or (get-buffer-window (current-buffer) t) (selected-window)))

(defun ygg-git-compare--image-object (state width)
  "STATE's picture held to WIDTH pixels at most, animated if it has frames."
  (let* ((file (plist-get state :file))
         (window (ygg-git-compare--image-window))
         (limit (max (frame-char-width) (- (window-body-width window t) (* 3 (frame-char-width)))))
         (max-width (if width (min width limit) limit))
         (max-height (* ygg-git-compare-image-lines (frame-char-height)))
         (attributes (file-attributes file))
         (stamp (list (file-attribute-modification-time attributes)
                      (file-attribute-size attributes) max-width max-height))
         (held (gethash file ygg-git-compare--image-objects)))
    (if (equal (car held) stamp)
        (cdr held)
      (when-let* ((timer (and held (ignore-errors (image-animate-timer (cdr held))))))
        (cancel-timer timer))
      (remhash file ygg-git-compare--image-objects)
      (when-let* ((image (ignore-errors
                           (create-image file (plist-get state :type) nil
                                         :max-width max-width :max-height max-height
                                         :ascent 'center))))
        (when (and (> ygg-git-compare-gif-seconds 0) (ignore-errors (image-multi-frame-p image)))
          (ignore-errors (image-animate image nil ygg-git-compare-gif-seconds)))
        (puthash file (cons stamp image) ygg-git-compare--image-objects)
        image))))

(defun ygg-git-compare--image-display (state alt width)
  "What the line of a picture shows for STATE: the picture, or ALT with why not."
  (cond ((plist-get state :file)
         (or (ygg-git-compare--image-object state width)
             (propertize (concat alt " (image unavailable)") 'face 'shadow)))
        ((plist-get state :limited)
         (propertize (concat alt " (image not loaded: too many)") 'face 'shadow))
        ((plist-get state :external)
         (propertize (format "%s (image from %s — not loaded)" alt (plist-get state :external))
                     'face 'shadow))
        ((plist-get state :failed)
         (propertize (concat alt " (image unavailable)") 'face 'shadow))
        (t (propertize (concat alt "…") 'face 'shadow))))

(defun ygg-git-compare--image-line (item pr)
  "ITEM, a picture of a description, as the line that shows it."
  (let* ((alt (if (string-empty-p (plist-get item :alt)) "image" (plist-get item :alt)))
         (width (plist-get item :width))
         (source (ygg-git-compare--image-source (plist-get item :image) pr))
         (url (cdr source))
         (state (cond ((null source) (list :failed (float-time)))
                      ((plist-get item :limited) (list :limited t))
                      ((ygg-git-compare--image-allowed-p url pr)
                       (ygg-git-compare--image-state url pr))
                      (t (list :external (or (ygg-git-compare--image-host url) "an unknown host")))))
         (text (propertize alt 'face 'shadow
                           'ygg-git-compare-image (list (cdr source) alt width)
                           'display (ygg-git-compare--image-display state alt width))))
    (concat "  " text)))

(defun ygg-git-compare--image-paint (buffer source)
  "Show the picture at SOURCE where BUFFER has drawn its line."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (with-silent-modifications
        (let ((pos (point-min))
              (state (gethash source ygg-git-compare--images)))
          (while (< pos (point-max))
            (let ((end (next-single-property-change pos 'ygg-git-compare-image nil (point-max)))
                  (spec (get-text-property pos 'ygg-git-compare-image)))
              (when (equal (car spec) source)
                (put-text-property pos end 'display
                                   (ygg-git-compare--image-display state (nth 1 spec) (nth 2 spec))))
              (setq pos end))))))))

(provide 'ygg-git-compare-images)

;;; ygg-git-compare-images.el ends here
