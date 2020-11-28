;; -*- lexical-binding: t; -*-
;; install emmet for HTML tags templates
(straight-use-package '(typescript-mode :type git :host github :repo "emacs-typescript/typescript.el"))
(straight-use-package '(emmet-mode :type git :host github :repo "smihica/emmet-mode"))

(setq
 lsp-eslint-server-command '("node" "/home/artem/.emacs.d/emacs-config/lsp/eslint/server/out/eslintServer.js" "--stdio" )
        lsp-eslint-enable t
        lsp-eslint-package-manager "npm"
        lsp-eslint-format t
        lsp-eslint-auto-fix-on-save t
        lsp-eslint-enable t
        lsp-eslint-run "onType"
        ;; lsp-eslint-trace-server "on" - can badly hit performance
        lsp-eslint-quiet nil)


;; (add-hook 'typescript-mode-hook (lambda () (add-hook 'before-save-hook (lambda()(lsp-eslint-apply-all-fixes)))))
;; (add-hook 'typescript-mode-hook (lambda () (add-hook 'before-save-hook (lambda()(lsp-eslint-apply-all-fixes)))))

 (defun art-js-mode-hook()
  (add-hook 'before-save-hook 'lsp-eslint-fix-all nil t)) ; adds the hook so that it's local to the JS and TS buffers

 ;; (remove-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)

(add-hook 'js-mode-hook 'art-js-mode-hook)
(add-hook 'typescript-mode-hook 'art-js-mode-hook)

(setq js-indent-level 2)

(add-hook 'typescript-mode-hook 'emmet-mode)
(add-hook 'js-mode-hook 'emmet-mode)
(add-hook 'markdown-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)

(provide 'lang-javascript)
