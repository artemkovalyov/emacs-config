
;; (straight-use-package '(rust-mode :type git :host github :repo "rust-lang/rust-mode"))

;; (use-package rust-mode
;;   :hook (rust-mode . (lambda () (setq intend-tabs-mode nil)))
;;   :config
;;   (setq rust-format-on-save t))

;; (straight-use-package '(cargo :type git :host github :repo "kwrooijen/cargo.el"))
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))


(straight-use-package '(rustic :type git :host github :repo "brotzeit/rustic"))

(use-package rustic
  :config
  (setq rustic-format-on-save t))

(provide 'lang-rust)
