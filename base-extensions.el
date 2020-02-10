;;; package --- base-extensions.el
;;; Commentary:

;;; Code:
(use-package ace-jump-mode
  :bind
  ("C-j" . ace-jump-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

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
	ediff-diff-options "-w"))
  (setq-default ediff-highlight-all-diffs 'nil)

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

(use-package flycheck
  :defer t
  :ensure t
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15)))
  :config
  (setq flycheck-javascript-standard-executable "/home/artem/.yarn/bin/standard")
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2))

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
         ("C-x C-m" . helm-M-x)
         ("C-o" . helm-find-files)
         ("s-o" . helm-projectile)
         ("C-x c o" . helm-occur)
         ("s-d" . helm-projectile-find-dir)
         ("s-g" . helm-projectile-grep)
         ("s-f" . helm-projectile-find-file)
         ("M-y" . helm-show-kill-ring)
	 ("M-s M-s" . helm-projectile-switch-project)
	 ("C-b" . helm-buffers-list)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-select-action)
	 ("M-k" . helm-next-line)
	 ("M-i" . helm-previous-line)
	 :map helm-find-files-map
	 ("M-i" . helm-previous-line)
	 ;; :map helm-find-files-map
	 ;; ("<tab>" . helm-execute-persistent-action)
	 ))


(use-package helm-ag)

(use-package helm-git-grep)

(use-package helm-projectile)

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


;; git too for Emacs
(use-package magit
  :config
  :bind
  ;; Magit
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("s-<mouse-1>" . mc/add-cursor-on-click))

(use-package neotree
  :init
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :bind
  (([f8] . neotree-toggle)
  :map neotree-mode-map
  ("k" . neotree-next-line)
  ("i" . neotree-previous-line))
  :config
  (setq neo-theme 'arrow
        neotree-smart-open t
        neo-window-fixed-size nil
	neo-show-hidden-files t
	projectile-indexing-method 'hybrid
	projectile-sort-order 'recently-active))

(use-package page-break-lines)

(use-package projectile
  :ensure t
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'yarn '("package.json"))
  (projectile-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package smartparens)

(use-package smex)

(use-package undo-tree
  :bind
  ("C-." . undo-tree-visualize)
  ("C-z" . undo-tree-undo)
  ("M-z" . undo-tree-redo)
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

(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
	("<backtab>" . yas-expand))
  :config
  (yas-global-mode 1))

(use-package awesome-tab
  :load-path
  (lambda () (concat user-emacs-directory "elpa/awesome-tab/"))
  :bind
  ("M-s-l" . awesome-tab-forward)
  ("M-s-j" . awesome-tab-backward)
  :init
  (require 'awesome-tab)
  :config
  (setq awesome-tab-style "bar")
  (awesome-tab-mode t))

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

(use-package golden-ratio
  :disabled
  :custom
  (golden-ratio-mode 1))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))

(use-package dockerfile-mode
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

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; (use-package beacon
;;   :custom
;;   (beacon-color "#f1fa8c")
;;   :hook (after-init . beacon-mode))

;; Gradle
(use-package gradle-mode
  :config
  (gradle-mode 1)
  :mode ("\\.gradle\\'"))

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)


(provide 'base-extensions)
