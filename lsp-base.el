;; -*- lexical-binding: t; -*-
;;; package --- lsp.el
;;; Commentary: LSP configuration
;;; Code:


;;; activate LSP mode
(use-package lsp-mode
  :straight (lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode")
  :custom
  (lsp-completion-provider :none) ;; corfu is used
  :init
  (setq lsp-keymap-prefix "s-SPC"
        lsp-enable-indentation nil)
  (setq lsp-use-plists t);; This cause my LSP setup to crash
  ;; (setq lsp-log-io t) ; enable debug log - can be a huge performance hit
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :config
  (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte"))
  ;; (define-key lsp-mode-map (kbd "s-SPC") lsp-command-map)
  (define-key lsp-mode-map (kbd "s-l") nil)
  ;; (lsp-treemacs-sync-mode 1)

  :hook
  ((typescript-mode go-mode rust-mode json-mode html-mode css-mode svelte-mode web-mode js-mode) . lsp-deferred)
  (lsp-completion-mode . my/lsp-mode-setup-completion)

  :bind-keymap ("s-SPC" . lsp-command-map)
  :bind (:map lsp-mode-map
              ("s-r"  . lsp-rename))

  :commands (lsp lsp-deferred))

;; LSP UI tools
(use-package lsp-ui
  :straight (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui")
  :defer t
  :commands
  lsp-ui-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  ;; (lsp-ui-doc-max-width 20)
  ;; (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "")
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  ;; semantic highlight
  (lsp-enable-semantic-highlighting t)
  :preface
  (defun toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
	(progn
	  (lsp-ui-doc-mode -1)
	  (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
	("H-r" . lsp-find-references)
	("H-d" . lsp-find-definition)
	("H-o"   . lsp-find-implementation)
	("H-m"   . lsp-ui-imenu)
	("C-c s"   . lsp-ui-sideline-mode)
	("C-c d"   . toggle-lsp-ui-doc))
  :hook
  (lsp-mode . lsp-ui-mode))

;; (straight-use-package '(lsp-treemacs :type git :host github :repo "emacs-lsp/lsp-treemacs"))
(use-package lsp-treemacs :after lsp-mode :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger

;; (straight-use-package '(dap-mode :type git :host github :repo "emacs-lsp/dap-mode")) ;
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode)) ;

;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package lsp-tailwindcss
  :straight (lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq lsp-tailwindcss-major-modes '(svelte-mode html-mode sgml-mode mhtml-mode web-mode css-mode)))

(provide 'lsp-base)
;;; lsp-base.el ends here
