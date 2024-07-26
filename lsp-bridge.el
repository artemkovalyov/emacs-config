;; -*- lexical-binding: t; -*-

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  :config
  (setq lsp-bridge-python-command "/home/artem/.emacs.d/python/env/bin/python"
        ;; lsp-bridge-user-langserver-dir "/home/artem/.emacs.d/lsp/server"
        lsp-bridge-user-multiserver-dir "/home/artem/.emacs.d/lsp/multiserver"
        lsp-bridge-enable-completion-in-string t
        acm-enable-icon nil
        acm-candidate-match-function #'orderless-flex
        ;; lsp-bridge-multi-lang-server-mode-list nil
        ;; lsp-bridge-multi-lang-server-extension-list nil
        )
  (cl-pushnew  '(("svelte") . "svelte_tailwindcss") lsp-bridge-multi-lang-server-extension-list)
  (cl-pushnew  '(("css") . "css_tailwindcss") lsp-bridge-multi-lang-server-extension-list)
  (cl-pushnew  '(("html") . "html_and_tailwindcss") lsp-bridge-multi-lang-server-extension-list)

  ;; (add-to-list 'lsp-bridge-single-lang-server-extension-list '(("svelte") . "svelteserver"))
  ;; (add-to-list 'lsp-bridge-single-lang-server-extension-list '(("svelte") . "tailwind"))
  ;; (setq lsp-bridge-enable-log t)
  (setq lsp-bridge-enable-debug nil)
  :init
  (global-lsp-bridge-mode))

(provide 'lsp-bridge)
