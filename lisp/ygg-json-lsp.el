;;; ygg-json-lsp.el --- JSON and YAML servers with the schemas they check against -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'layer-lsp)

(defvar eglot-server-programs)
(defvar eglot--servers-by-project)
(declare-function eglot--major-modes "eglot")
(declare-function eglot--managed-buffers "eglot")
(declare-function eglot-signal-didChangeConfiguration "eglot")
(declare-function ygg-rn-root "layer-react-native")
(declare-function ygg-rn-expo-p "layer-react-native")

(defconst ygg-json-lsp-modes '(json-mode json-ts-mode jsonc-mode js-json-mode)
  "Modes vscode-json-language-server serves.")

(defconst ygg-json-lsp--expo-schemas
  "https://raw.githubusercontent.com/expo/vscode-expo/schemas/schema/"
  "Where Expo publishes the schemas of its config files.")

(defconst ygg-json-lsp-schemas
  `((("package.json") . "https://json.schemastore.org/package.json")
    (("tsconfig.json" "tsconfig.*.json") . "https://json.schemastore.org/tsconfig.json")
    (("jsconfig.json" "jsconfig.*.json") . "https://json.schemastore.org/jsconfig.json")
    ((".eslintrc" ".eslintrc.json") . "https://json.schemastore.org/eslintrc.json")
    ((".prettierrc" ".prettierrc.json") . "https://json.schemastore.org/prettierrc.json")
    ((".babelrc" ".babelrc.json") . "https://json.schemastore.org/babelrc.json")
    (("eas.json") . ,(concat ygg-json-lsp--expo-schemas "eas.json"))
    (("store.config.json") . ,(concat ygg-json-lsp--expo-schemas "eas-metadata.json"))
    (("expo-module.config.json") . ,(concat ygg-json-lsp--expo-schemas "expo-module.json")))
  "File names -> the schema checking them, in every project.")

(defconst ygg-json-lsp-expo-app-files '("app.json" "app.config.json")
  "Expo's app config files; app.json is a generic name, so only in Expo projects.")

(defcustom ygg-json-lsp-expo-schema-cache (locate-user-emacs-file "var/expo-schema/")
  "Where the app config schema of each Expo SDK is kept once fetched."
  :type 'directory
  :group 'eglot)

(defun ygg-json-lsp--expo-sdk (root)
  "The Expo SDK installed at ROOT, like 53.0.0, or nil."
  (when-let* ((file (expand-file-name "node_modules/expo/package.json" root))
              ((file-exists-p file))
              (version (ignore-errors
                         (with-temp-buffer
                           (insert-file-contents file)
                           (alist-get 'version (json-parse-buffer :object-type 'alist)))))
              ((string-match "\\`\\([0-9]+\\)\\." version)))
    (concat (match-string 1 version) ".0.0")))

(defun ygg-json-lsp--expo-schema-file (root)
  (when-let* ((sdk (ygg-json-lsp--expo-sdk root)))
    (expand-file-name (concat sdk ".json") ygg-json-lsp-expo-schema-cache)))

(defun ygg-json-lsp-expo-schema-url (root)
  "The app config schema of the Expo SDK at ROOT, else the newest SDK's."
  (let ((file (ygg-json-lsp--expo-schema-file root)))
    (if (and file (file-exists-p file))
        (concat "file://" file)
      (concat ygg-json-lsp--expo-schemas "expo-xdl.json"))))

(defun ygg-json-lsp-write-expo-schema (response file)
  "Write the schema in Expo's API RESPONSE to FILE, with or without the expo key."
  (let* ((schema (gethash "schema" (gethash "data" (json-parse-string response))))
         (definitions (gethash "definitions" schema))
         (body (copy-hash-table schema)))
    (remhash "definitions" body)
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (json-insert (list :definitions definitions
                           :oneOf (vector (list :type "object" :required ["expo"]
                                                :properties (list :expo body))
                                          body)))))))

(defvar ygg-json-lsp--fetching nil
  "Schema files being fetched.")

(defun ygg-json-lsp--refresh-json-servers ()
  (dolist (servers (hash-table-values eglot--servers-by-project))
    (dolist (server servers)
      (when (cl-intersection (eglot--major-modes server) ygg-json-lsp-modes)
        (eglot-signal-didChangeConfiguration server)))))

(defun ygg-json-lsp-prefetch-expo-schema ()
  "Fetch this Expo project's app config schema in the background, once."
  (when-let* ((root (ygg-json-lsp--expo-root default-directory))
              (sdk (ygg-json-lsp--expo-sdk root))
              (file (ygg-json-lsp--expo-schema-file root))
              ((not (file-exists-p file)))
              ((not (member file ygg-json-lsp--fetching)))
              (curl (executable-find "curl")))
    (push file ygg-json-lsp--fetching)
    (let ((buffer (generate-new-buffer " *expo-schema*")))
      (make-process
       :name "expo-schema" :buffer buffer :noquery t :connection-type 'pipe
       :command (list curl "-fsS" "--max-time" "30"
                      (format "https://api.expo.dev/v2/project/configuration/schema/%s" sdk))
       :sentinel (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (setq ygg-json-lsp--fetching (delete file ygg-json-lsp--fetching))
                     (when (eql (process-exit-status process) 0)
                       (ignore-errors
                         (ygg-json-lsp-write-expo-schema
                          (with-current-buffer buffer (buffer-string)) file))
                       (when (file-exists-p file)
                         (ygg-json-lsp--refresh-json-servers)))
                     (kill-buffer buffer)))))))

(defconst ygg-json-lsp-yaml-schemas
  `((,(concat ygg-json-lsp--expo-schemas "eas-workflow.json")
     . ["**/.eas/workflows/*.yml" "**/.eas/workflows/*.yaml"]))
  "Schema -> the YAML files it checks, beside SchemaStore's own list.")

(defun ygg-json-lsp--expo-root (directory)
  (and (fboundp 'ygg-rn-root)
       (when-let* ((root (ygg-rn-root directory)))
         (and (ygg-rn-expo-p root) root))))

(defun ygg-json-lsp--app-files (root)
  "ROOT's app config files as absolute patterns, under its name and its true name."
  (vconcat (seq-uniq (mapcan (lambda (dir)
                               (mapcar (lambda (file) (expand-file-name file dir))
                                       ygg-json-lsp-expo-app-files))
                             (list root (file-truename root))))))

(defun ygg-json-lsp-json-settings (directories)
  "The json settings for a server whose buffers are in DIRECTORIES."
  (let ((roots (seq-uniq (delq nil (mapcar #'ygg-json-lsp--expo-root directories)))))
    (list :validate '(:enable t)
          :schemaDownload '(:enable t)
          :schemas (vconcat
                    (mapcar (lambda (schema)
                              (list :fileMatch (vconcat (car schema)) :url (cdr schema)))
                            ygg-json-lsp-schemas)
                    (mapcar (lambda (root)
                              (list :fileMatch (ygg-json-lsp--app-files root)
                                    :url (ygg-json-lsp-expo-schema-url root)))
                            roots)))))

(defun ygg-json-lsp-yaml-settings ()
  "The yaml settings: SchemaStore's list, and Expo's workflows."
  (list :schemaStore '(:enable t)
        :validate t
        :schemas (cl-loop for (url . globs) in ygg-json-lsp-yaml-schemas
                          append (list (intern (concat ":" url)) globs))))

(defun ygg-json-lsp-configuration (server)
  "Settings for SERVER when it serves JSON or YAML, else nil."
  (let ((modes (eglot--major-modes server)))
    (cond ((cl-intersection modes ygg-json-lsp-modes)
           (list :json (ygg-json-lsp-json-settings
                        (cons default-directory
                              (mapcar (lambda (buffer) (buffer-local-value 'default-directory buffer))
                                      (eglot--managed-buffers server))))))
          ((cl-intersection modes '(yaml-mode yaml-ts-mode))
           (list :yaml (ygg-json-lsp-yaml-settings))))))

(with-eval-after-load 'layer-lsp
  (advice-add 'ygg-lsp-workspace-configuration :after-until #'ygg-json-lsp-configuration))

(with-eval-after-load 'eglot
  (when (ygg-lsp--executable "vscode-json-language-server")
    (add-to-list 'eglot-server-programs
                 `(,(mapcar (lambda (mode)
                              (list mode :language-id (if (eq mode 'jsonc-mode) "jsonc" "json")))
                            ygg-json-lsp-modes)
                   . ("vscode-json-language-server" "--stdio")))))

(when (ygg-lsp--executable "vscode-json-language-server")
  (dolist (mode ygg-json-lsp-modes)
    (add-hook (intern (format "%s-hook" mode)) #'ygg-json-lsp-prefetch-expo-schema)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure)))

(provide 'ygg-json-lsp)
;;; ygg-json-lsp.el ends here
