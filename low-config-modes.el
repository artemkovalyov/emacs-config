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

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.mdx\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'low-config-modes)
