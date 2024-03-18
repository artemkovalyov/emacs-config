;; -*- lexical-binding: t; -*-
;;; package --- Artem Kovalov Emacs config

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "emacs-config"))

(require 'base)
(require 'spelling)
(require 'base-extensions)
(require 'treemacs-config)
(require 'lsp-base)
;; (require 'lsp-bridge)
(require 'lang-rust)
(require 'lang-json)
;; (require 'lang-java)
(require 'low-config-modes)
(require 'base-functions)
(require 'base-global-keys)
(require 'base-theme)
;; (require 'artem-tree-sitter)

(provide '.emacs)
;;; .emacs ends here
(put 'narrow-to-region 'disabled nil)
