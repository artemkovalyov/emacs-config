;; -*- lexical-binding: t; -*-
(use-package emojify
  :straight (emojify :type git :host github :repo "iqbalansari/emacs-emojify")
  :hook (after-init . global-emojify-mode))

(use-package all-the-icons
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el"))

(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "hlissner/emacs-doom-themes")
  :after (all-the-icons)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colorful") ; use the colorful treemacs theme
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :straight (doom-modeline :type git :host github :repo "seagle0128/doom-modeline")
  :ensure t
  :init (doom-modeline-mode 1))


(let ((class '((class color) (min-colors 89))))
  (custom-set-faces
   `(selectrum-current-candidate
     ((,class (:background ,(doom-color 'dark-blue)
                           :weight bold
                           :foreground ,(doom-color 'base8)))))
   `(selectrum-prescient-primary-highlight
   ((,class (:foreground ,(doom-color 'orange)))))
   `(selectrum-prescient-secondary-highlight
   ((,class (:foreground ,(doom-color 'green)))))))

(provide 'base-theme)
