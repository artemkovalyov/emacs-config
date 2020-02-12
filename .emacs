;;; package --- Artem Kovalov Emacs config

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "emacs-config"))

(require 'base-functions)
(require 'base-global-keys)
(require 'base-theme)
(require 'base)
(require 'base-extensions)
(require 'lsp-base)
;;(require 'lang-python)
;;(require 'lang-go)
(require 'lang-javascript)
;;(require 'lang-java)
(require 'projectile-help-functions)
(require 'spelling)
(require 'lang-json)
(require 'lang-org)
(require 'treemacs-config)
(provide '.emacs)

;;; .emacs ends here
