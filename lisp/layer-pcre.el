;;; layer-pcre.el --- PCRE regex dialect via pcre2el -*- lexical-binding: t; -*-

;;; Code:

;; No global pcre-mode: it advises query-replace etc; Yggdrasil converts only at its own entry points.
(when (fboundp 'elpaca)
  (elpaca pcre2el))

(provide 'layer-pcre)
;;; layer-pcre.el ends here
