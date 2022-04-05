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
(require 'base-extensions)
(require 'treemacs-config)
(require 'lsp-base)
(require 'lang-rust)
(require 'lang-json)
(require 'lang-java)
(require 'projectile-help-functions)
(require 'low-config-modes)
(require 'base-functions)
(require 'base-global-keys)
(require 'base-theme)
(provide '.emacs)
;;; .emacs ends here
