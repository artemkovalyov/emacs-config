(straight-use-package '(rust-mode :type git :host github :repo "rust-lang/rust-mode"))

(use-package rust-mode
  :init
  (setq indent-tabs-mode nil
        rust-format-on-save t))

(provide 'lang-rust)
