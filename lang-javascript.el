;; -*- lexical-binding: t; -*-
;; install emmet for HTML tags templates
(straight-use-package '(typescript-mode :type git :host github :repo "emacs-typescript/typescript.el"))
(straight-use-package '(js2-mode :type git :host github :repo "mooz/js2-mode"))

(add-hook 'js-mode-hook 'js2-minor-mode)

(setq
 ;; lsp-eslint-server-command '("node" "/home/artem/.emacs.d/emacs-config/lsp/eslint/server/out/eslintServer.js" "--stdio" )
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

 ;; (defun art-js-mode-hook()
  ;; (add-hook 'before-save-hook #'lsp-eslint-fix-all nil t))
 ;; (remove-hook 'before-save-hook '#lsp-eslint-apply-all-fixes)

;; (add-hook 'js-mode-hook #'art-js-mode-hook)
;; (add-hook 'typescript-mode-hook '#art-js-mode-hook)

(setq js-indent-level 2)

(provide 'lang-javascript)
