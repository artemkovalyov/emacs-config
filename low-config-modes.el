;; -*- lexical-binding: t -*-

;; Gradle
(use-package gradle-mode
  :defer t
  :config
  (gradle-mode 1)
  :mode ("\\.gradle\\'"))

;; Docker
(use-package dockerfile-mode
  :defer t
  :mode "\\Dockerfile\\'")

;; YAML mode
(use-package yaml-mode
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))

(straight-use-package '(nginx-mode :host github :repo "ajc/nginx-mode"))
(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))


;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.mdx\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html?\\'" . web-mode)
  :bind
  (:map web-mode-map
        ("H-f" . web-mode-fold-or-unfold))
  :init
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\\.svelte?\\'" . svelte-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-attr-indent-offset t)
  (web-mode-javascript-indentation 2)
  (web-mode-script-padding 2))

(use-package d2-mode
  :straight (d2-mode :type git :host github :repo "andorsk/d2-mode"))


(provide 'low-config-modes)
