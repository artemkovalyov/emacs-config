;; -*- lexical-binding: t; -*-
(use-package emojify
  :straight (emojify :type git :host github :repo "iqbalansari/emacs-emojify")
  :hook (after-init . global-emojify-mode))

(use-package all-the-icons
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el")
  :if (display-graphic-p))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "hlissner/emacs-doom-themes")
  :ensure t
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

;; custom colors for prescian (selectrum)
(let ((class '((class color) (min-colors 89))))
  (custom-set-faces
   `(selectrum-current-candidate
     ((,class (:background ,(doom-color 'dark-blue)
                           :weight bold
                           :foreground ,(doom-color 'base8)))))
   `(selectrum-prescient-primary-highlight
     ((,class (:foreground ,(doom-color 'blue)))))
   `(selectrum-prescient-secondary-highlight
     ((,class (:foreground ,(doom-color 'orange)))))))

(provide 'base-theme)
