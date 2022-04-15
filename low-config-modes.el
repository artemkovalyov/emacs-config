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

;; Svelte mode
(straight-use-package '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
(use-package svelte-mode
  :mode ("\\.svelte\\'" . svelte-mode)
  :custom
  (svelte-basic-offset 2)
  :config
  (defun svelte-mode-sgml-empty-tag-p-advice (old-function tag-name)
    "Advice function intended to wrap around `sgml-empty-tag-p
Makes case significant when checking whether tags need to be
closed or not, to not confuse elements with Svelte components."
    (if (eq major-mode 'svelte-mode)
        (assoc-string tag-name sgml-empty-tags)
      (funcall old-function tag-name)))

  (defun svelte-mode-sgml-unclosed-tag-p-advice (old-function tag-name)
    "Advice function intended to wrap around `sgml-unclosed-tag-p
Makes case significant when checking whether tags need to be
closed or not, to not confuse elements with Svelte components."
    (if (eq major-mode 'svelte-mode)
        (assoc-string tag-name sgml-unclosed-tags)
      (funcall old-function tag-name)))

  (advice-add 'sgml-empty-tag-p :around 'svelte-mode-sgml-empty-tag-p-advice)
  (advice-add 'sgml-unclosed-tag-p :around 'svelte-mode-sgml-unclosed-tag-p-advice)
  )

(use-package javascript-mode
  :mode ("\\.cjs\\'" . js-mode))

(provide 'low-config-modes)
