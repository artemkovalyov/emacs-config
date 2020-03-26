;;; package --- lang-typescript.el

;;; Commentary:

;; (straight-use-package 'web-mode)
;; (straight-use-package 'emmet-mode)

;;; Code:
(straight-use-package 'tide)
(use-package tide
  :ensure t
  :demand t
  :init
  (setq company-tooltip-align-annotations t
	typescript-indent-level 2)
  (setq tide-user-preferences '(:includeCompletionsForModuleExports t :includeCompletionsWithInsertText t :allowTextChangesInNewFiles t))
  :config
  ;; (company-mode +1)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)
	 (js-mode . tide-setup)
	 ))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

(provide 'lang-typescript)
