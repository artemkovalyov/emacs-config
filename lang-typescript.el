;;; package --- lang-tide.el

;;; Commentary:

;;; Code:
(use-package tide
  :ensure t
  :demand t
  :init
  (setq company-tooltip-align-annotations t)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
	 ))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


;; (add-hook 'js-mode-hook #'setup-tide-mode)
;;(add-hook 'js-mode-hook  #'lint-js-buffer-with-standard-js)


(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-command "prettier-standard")
  :hook (js-mode . prettier-js-mode))

(setq js-indent-level 2)

(use-package web-mode
  :mode
  (("\\.tsx\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :init
  :hook
  (web-mode . (lambda ()
    (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
	      (string-equal "jsx" (file-name-extension buffer-file-name)))
              (setup-tide-mode))))
  )

(provide 'lang-tide)
