;; -*- lexical-binding: t; -*-
;;; package --- Artem Kovalov Emacs config

;;; Commentary:

;;; Code:
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6
      art-gc-cons-threshold 16777216)

(add-to-list 'load-path (concat user-emacs-directory "emacs-config"))

(require 'base)
(require 'spelling)
(require 'base-functions)
(require 'base-extensions)
(require 'treemacs-config)
(require 'lsp-base)
(require 'lang-rust)
(require 'lang-json)
(require 'lang-javascript)
(require 'lang-java)
(require 'projectile-help-functions)
;; (require 'lang-lisp)
(require 'base-theme)
(require 'base-global-keys)
(provide '.emacs)

;;; .emacs ends here
