;; -*- lexical-binding: t; -*-
;;; package --- json config
;;; Commentary:
;;; Contains my python configs

;;; Code:

(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode)
  :hook ((json-mode . flycheck-mode)))

(provide 'lang-json)
;;; lang-json.el ends here
