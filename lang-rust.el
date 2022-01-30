
(straight-use-package '(rustic :type git :host github :repo "brotzeit/rustic"))

(use-package rustic
  :config
  (setq rustic-format-on-save t)
  :bind
  (:map rustic-mode-map
        ;; To run arbitrary command, for example `env MYVAR=1 cargo run abc use C-u A-r'
        ("A-r" . rustic-cargo-run)))

(provide 'lang-rust)
