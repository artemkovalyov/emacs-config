;;; package --- lsp.el
;;; Commentary: LSP configuration
;;; Code:


(straight-use-package '(lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode"))

;;; activate LSP mode
(use-package lsp-mode
:init
(setq lsp-keymap-prefix "s-m")
(setq lsp-prefer-capf t)
;; (setq lsp-eslint-auto-fix-on-save t)
;; (setq lsp-gopls-server-path "~/go/bin/gopls")
:commands (lsp lsp-deferred)
:hook
((js-mode typescript-mode go-mode) . lsp-deferred)
:bind
(:map lsp-mode-map
("C-c r"   . lsp-rename))
:config
(require 'lsp-clients))

(straight-use-package '(lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui"))
;; LSP UI tools
(use-package lsp-ui
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
	("C-c C-r" . lsp-ui-peek-find-references)
	("C-c C-j" . lsp-ui-peek-find-definitions)
	("C-c i"   . lsp-ui-peek-find-implementation)
	("C-c m"   . lsp-ui-imenu)
	("C-c s"   . lsp-ui-sideline-mode)
	("C-c d"   . toggle-lsp-ui-doc))
  :hook
(lsp-mode . lsp-ui-mode))

(straight-use-package 'helm-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(straight-use-package 'lsp-treemacs)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(straight-use-package 'dap-mode)
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; eslint lsp
;; (setq lsp-eslint-server-command
;;   `("node"  ,(expand-file-name "~/.emacs.d/emacs-config/eslintServer.js") "--stdio"))
(provide 'lsp-base)
;;; lsp-base.el ends here
