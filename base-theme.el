
(straight-use-package '(emojify :type git :host github :repo "iqbalansari/emacs-emojify"))
(use-package emojify  :hook (after-init . global-emojify-mode))

(straight-use-package '(all-the-icons :type git :host github :repo "domtronn/all-the-icons.el"))
(use-package all-the-icons)

(straight-use-package 'doom-themes)

(use-package doom-themes
  :after (all-the-icons)
  :config                               ;
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(straight-use-package '(doom-modeline :type git :host github :repo "seagle0128/doom-modeline"))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(provide 'base-theme)
