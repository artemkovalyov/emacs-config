;;; package --- Artem Kovalov Emacs config

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "emacs-config"))

(require 'base)
(require 'spelling)
(require 'base-functions)
(require 'base-extensions)
(require 'treemacs-config)
(require 'lsp-base)
(require 'lang-json)
;; (require 'lang-typescript)
;; (require 'lang-go)
(require 'lang-javascript)
;; (require 'lang-java)
(require 'projectile-help-functions)
;; (require 'lang-org)
(require 'lang-lisp)
(require 'base-theme)
(require 'base-global-keys)
(provide '.emacs)

;;; .emacs ends here
