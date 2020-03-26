;; install emmet for HTML tags templates
(straight-use-package 'emmet-mode)

(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-command "prettier-standard")
  :hook (js-mode . prettier-js-mode))

(setq js-indent-level 2)

(provide 'lang-javascript)
