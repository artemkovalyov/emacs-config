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
  :after tree-sitter
  :commands (web-mode)
  :bind
  (:map web-mode-map
   ("H-f" . web-mode-fold-or-unfold))
  :init
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\\.svelte?\\'" . svelte-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(svelte-mode . html))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-attr-indent-offset t)
  (web-mode-javascript-indentation 2)
  (web-mode-script-padding 2))

;; ;; Svelte mode
;; (straight-use-package '(svelte-mode :type git :host github :repo "leafOfTree/svelte-mode"))
;; (use-package svelte-mode
;;   :mode ("\\.svelte\\'" . svelte-mode)
;;   :custom
;;   (svelte-basic-offset 2)
;;   :config
;;   (defun svelte-mode-sgml-empty-tag-p-advice (old-function tag-name)
;;     "Advice function intended to wrap around `sgml-empty-tag-p
;; Makes case significant when checking whether tags need to be
;; closed or not, to not confuse elements with Svelte components."
;;     (if (eq major-mode 'svelte-mode)
;;         (assoc-string tag-name sgml-empty-tags)
;;       (funcall old-function tag-name)))

;;   (defun svelte-mode-sgml-unclosed-tag-p-advice (old-function tag-name)
;;     "Advice function intended to wrap around `sgml-unclosed-tag-p
;; Makes case significant when checking whether tags need to be
;; closed or not, to not confuse elements with Svelte components."
;;     (if (eq major-mode 'svelte-mode)
;;         (assoc-string tag-name sgml-unclosed-tags)
;;       (funcall old-function tag-name)))

;;   (advice-add 'sgml-empty-tag-p :around 'svelte-mode-sgml-empty-tag-p-advice)
;;   (advice-add 'sgml-unclosed-tag-p :around 'svelte-mode-sgml-unclosed-tag-p-advice)
;;   )

(use-package javascript-mode
  :init (setq js-indent-level 2)
  :mode (("\\.cjs\\'" . js-mode)
         ("\\.jsx\\'" . js-mode)))

(use-package typescript-mode
  :straight (typescript-mode :type git :host github :repo "emacs-typescript/typescript.el")
  :after tree-sitter :mode (
         ("\\.tsx\\'" . typescript-mode))
  ;; :config
  ;; (define-derived-mode ts-svelte-mode typescript-mode "Typescript Svelte")
  ;; use our derived mode for tsx files
  ;; (add-to-list 'auto-mode-alist '("\\.svelte?\\'" . ts-svelte-mode))
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(ts-svelte-mode . typescript))
  )




(provide 'low-config-modes)
