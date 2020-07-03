 ;; (use-package spacemacs-theme
 ;;   :defer t
 ;;   :init
 ;;   (load-theme 'spacemacs-dark t))

(straight-use-package '(dracula :type git :host github :repo "dracula/emacs"))
(use-package dracula
:defer t
:init
(nyan-mode 1)
(load-theme 'dracula t))

(straight-use-package '(emojify :type git :host github :repo "iqbalansari/emacs-emojify"))
(use-package emojify  :hook (after-init . global-emojify-mode))

(straight-use-package '(all-the-icons :type git :host github :repo "domtronn/all-the-icons.el"))
(use-package all-the-icons)

(straight-use-package '(doom-modeline :type git :host github :repo "seagle0128/doom-modeline"))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(provide 'base-theme)
