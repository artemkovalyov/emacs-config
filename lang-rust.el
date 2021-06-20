
(straight-use-package '(rustic :type git :host github :repo "brotzeit/rustic"))

(use-package rustic
  :config
  (setq rustic-format-on-save t))

(provide 'lang-rust)
