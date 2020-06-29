;; install emmet for HTML tags templates
(straight-use-package 'emmet-mode)

;(add-hook 'js-mode-hook (lambda () (add-hook 'before-save-hook (lambda()(lsp-eslint-apply-all-fixes)))))

(add-hook 'js-mode-hook 'emmet-mode)
(add-hook 'typescript-mode-hook 'emmet-mode)
(add-hook 'markdown-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)

(setq js-indent-level 2)

(provide 'lang-javascript)
