;; -*- lexical-binding: t; -*-
;;; package --- json config
;;; Commentary:
;;; Contains my python configs

;;; Code:

(straight-use-package '(json-mode :type git :host github :repo "kiennq/json-mode"))
(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json$" . json-mode))

(provide 'lang-json)
;;; lang-json.el ends here
