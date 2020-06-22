;;; package --- base-extensions.el
;;; Commentary:

;;; Code:
(use-package ace-jump-mode
  :bind
  ("C-S-j" . ace-jump-mode))

(use-package company
  :init
  (setq company-minimum-prefix-length 3
	company-idle-delay 0)
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
	("M-k" . company-select-next)
	("M-i" . company-select-previous)
        ("M-f" . company-filter-candidates)
        ("M-s" . company-search-candidates))
  (:map company-search-map
        ("M-k" . company-select-next)
        ("M-i" . company-select-previous))
  )

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-close-button nil)
  :bind
  ("M-s-j" . centaur-tabs-backward)
  ("M-s-l" . centaur-tabs-forward))

(use-package dashboard
  :init
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5))
	dashboard-banner-logo-title "Life's Awesome!"
	dashboard-startup-banner nil)
  :config
  (dashboard-setup-startup-hook))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-diff-options "-w")
  (setq-default ediff-forward-word-function 'forward-char)
  (setq-default ediff-highlight-all-diffs 't)
  (defun artem-ediff-hook ()
    (ediff-setup-keymap)
    (define-key ediff-mode-map (kbd "M-i") 'ediff-previous-difference)
    (define-key ediff-mode-map (kbd "M-k") 'ediff-next-difference))
  :hook (ediff-mode . artem-ediff-hook)
)

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(straight-use-package '(flycheck :type git :host github :repo "flycheck/flycheck"))
(use-package flycheck
  :defer t )

(use-package comment-dwim-2
  :bind
  ("C-/" . comment-dwim-2))

(straight-use-package '(helm :type git :host github :repo "emacs-helm/helm"))
(use-package helm
  :init
  (require 'helm-config)
  :config
  (setq helm-split-window-inside-p t
        helm-split-window-default-side 'below
	helm-idle-delay 0.0
	helm-input-idle-delay 0.01
	helm-quick-update t
	helm-ff-skip-boring-files t)
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-o" . helm-find-files)
         ("s-o" . helm-projectile)
         ("s-d" . helm-projectile-find-dir)
         ("s-a" . helm-ag)
         ("s-f" . helm-projectile-find-file)
         ("M-y" . helm-show-kill-ring)
	 ("M-s M-s" . helm-projectile-switch-project)
	 ("M-s s" . helm-projectile)
	 ("C-b" . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-select-action)
	 ("M-k" . helm-next-line)
	 ("M-i" . helm-previous-line)
	 :map helm-find-files-map
	 ("M-i" . helm-previous-line)
	 ("M-k" . helm-next-line)
	 :map helm-generic-files-map
	 ("M-i" . helm-previous-line)
	 ("M-k" . helm-next-line)
	 ))

(straight-use-package '(helm-ag :type git :host github :repo "emacsorphanage/helm-ag"))
(use-package helm-ag)

(straight-use-package '(helm-projectile :type git :host github :repo "bbatsov/helm-projectile"))
(use-package helm-projectile)

(straight-use-package '(helm-swoop :type git :host github :repo "emacsorphanage/helm-swoop"))
(use-package helm-swoop
  :bind
  ("s-z" . helm-swoop)
  :config
  (setq helm-swoop-split-with-multiple-windows t
	helm-swoop-split-direction 'split-window-horizontally))

;; show line numbers
(use-package linum
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

;; highlight current line
(use-package hlinum
  :config
  (hlinum-activate))

(straight-use-package '(magit :type git :host github :repo "magit/magit"))
(use-package magit
  :config
  (setq magit-refresh-status-buffer nil)
  :bind
  ;; Magit
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive)
  (:map magit-mode-map
	("<tab>" . magit-section-toggle)))


(use-package magit-popup)

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("s-<mouse-1>" . mc/add-cursor-on-click))

(use-package projectile
  :ensure t
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'yarn '("package.json"))
  (projectile-mode 1))

;; (use-package recentf
;;   :config
;;   (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
;;   (recentf-mode 1))

(use-package smartparens)

(use-package undo-tree
  :bind
  ("M-z" . undo-tree-visualize)
  ("C-z" . undo-tree-undo)
  ("<С-S-z>" . undo-tree-redo)
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("<C-s-up>" . windmove-up)
  ("<C-s-down>" . windmove-down)
  ("<C-s-left>" . windmove-left)
  ("C-s-j" . windmove-left)
  ("<C-s-right>" . windmove-right)
  ("C-s-l" . windmove-right))

(use-package switch-window
  :bind
  ([C-tab] . switch-window)
  ("C-x o" . switch-window))

(use-package wgrep)

(straight-use-package 'yasnippet)
(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
	("<backtab>" . yas-expand))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/emacs-config/snippets"))
  ;; (yas-global-mode 1)
  (yas-reload-all)
  :hook
  ((prog-mode markdown-mode) . yas-minor-mode))

(straight-use-package 'yasnippet-snippets)
(straight-use-package 'react-snippets)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

 (use-package dimmer
  :disabled
  :custom
  (dimmer-fraction 0.3)
  (dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*NeoTree.*"
         ".*Messages.*"
         ".*Async.*"
         ".*Warnings.*"
         ".*LV.*"
         ".*Ilist.*"))
  :config
  (dimmer-mode t))

(use-package yaml-mode
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))

(use-package dockerfile-mode
  :defer t
  :mode "\\Dockerfile\\'")

(use-package nyan-mode
  :init
  ;; (setq mode-line-format
  ;; 	(list
  ;; 	 '(:eval (list (nyan-create)))))
  :custom
   (nyan-cat-face-number 4)
   (nyan-animate-nyancat t)
   (nyan-mode t))

(use-package rainbow-mode
  :diminish
  :hook
  (emacs-lisp-mode . rainbow-mode)
  (js-mode . rainbow-mode))


;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))


;; Gradle
(use-package gradle-mode
  :config
  (gradle-mode 1)
  :mode ("\\.gradle\\'"))

;;(require 'visual-regexp)
(straight-use-package '(visual-regexp :type git :host github :repo "benma/visual-regexp.el"))
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;;duplicating lines and words
(straight-use-package '(duplicate-thing :type git :host github :repo "ongaeshi/duplicate-thing"))

(provide 'base-extensions)
