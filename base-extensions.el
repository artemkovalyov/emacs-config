;; -*- lexical-binding: t; -*-
;;; package --- base-extensions.el
;;; Commentary:

;;;Code:

(setq flyspell-use-meta-tab nil)

;; A more complex, more lazy-loaded config
(use-package solaire-mode
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  :hook (change-major-mode . turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  :hook (after-revert . turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  :hook (ediff-prepare-buffer . solaire-mode)
  ;; Highlight the minibuffer when it is activated:
  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  ;; The bright and dark background colors are automatically swapped the first
  ;; time solaire-mode is activated. Namely, the backgrounds of the `default` and
  ;; `solaire-default-face` faces are swapped. This is done because the colors
  ;; are usually the wrong way around. If you don't want this, you can disable it:
  (setq solaire-mode-auto-swap-bg nil)
  (solaire-global-mode +1))

 (use-package drag-stuff
   :config
   (drag-stuff-global-mode 1)
   :bind
   ("C-j" . drag-stuff-left)
   ("C-l" . drag-stuff-right)
   ("C-i" . drag-stuff-up)
   ("C-k" . drag-stuff-down))

(straight-use-package '(avy :type git :host github :repo "abo-abo/avy"))
(use-package avy
  :bind
  ("M-s l" . avy-copy-line)
  ("M-s j" . avy-goto-word-or-subword-1))

(straight-use-package 'company)
(use-package company
  :init
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0
        company-backends '((company-capf company-files company-yasnippet)))
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
	("M-k" . company-select-next-or-abort)
	("M-i" . company-select-previous-or-abort)
        ("M-s" . company-search-candidates)
        ("M-f" . company-filter-candidates)
        ("<escape>" . company-abort)
        ("TAB" . company-complete)
        ("<tab>" . company-complete)
        ("<return>" . nil)
        ("RET" . nil))
  (:map company-search-map
        ("M-k" . company-select-next)
        ("M-i" . company-select-previous)))

(straight-use-package '(centaur-tabs :type git :host github :repo "ema2159/centaur-tabs"))
(use-package centaur-tabs
  :demand
  ;; :defer t
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-close-button nil)
  :bind
  ("M-n" . centaur-tabs-backward)
  ("M-m" . centaur-tabs-forward))

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
  :defer t
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
  :defer t
  :bind
  ("C-=" . er/expand-region))

(straight-use-package '(flycheck :type git :host github :repo "flycheck/flycheck"))
(use-package flycheck
  :defer t
  :config
  (add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.21))))

(use-package comment-dwim-2
  :defer t
  :bind
    ("M-/" . comment-dwim-2))

(straight-use-package '(helm :type git :host github :repo "emacs-helm/helm"))
(use-package helm
  :init
  (progn
    (require 'helm-config)
    ;; (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
	  helm-input-idle-delay 0.01  ; this actually updates things
					; reeeelatively quickly.
	  helm-yas-display-key-on-candidate t
	  helm-quick-update t
	  helm-M-x-requires-pattern nil
	  helm-ff-skip-boring-files t
          helm-split-window-in-side-p t ; open helm buffer inside current window, not occupy whole other window
	  helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
	  helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
	  helm-scroll-amount 8)	; scroll 8 lines other window using M-<next>/M-<prior>
    (helm-mode))
  :bind (("M-x" . helm-M-x)
         ("C-o" . helm-find-files)
         ("M-s s" . helm-projectile)
         ("s-a" . helm-ag)
	 ("M-s M-s" . helm-projectile-switch-project)
	 ("M-b" . helm-mini)
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
	 ("M-k" . helm-next-line)))

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
  ("C-x g b" . magit-blame)
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

(use-package smartparens
  :init
  (setq sp-navigate-reindent-after-up-in-string nil
        sp-navigate-reindent-after-up nil)
  :bind
  ("M-s-i" . sp-backward-sexp)
  ("M-s-k" . sp-forward-sexp)
  ("M-s-l" . sp-down-sexp)
  ("M-s-j" . sp-up-sexp)
  ("M-'" . sp-mark-sexp))

(use-package undo-tree
  :bind
  ;;"C-x u" - visualize undo tree
  ("C-z" . undo-tree-undo)
  ("C-y" . undo-tree-redo)
  ;; Remember undo history
  :config
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package which-key
  :config
  (which-key-mode))

;; (use-package wgrep)

(straight-use-package '(yasnippet :type git :host github :repo "joaotavora/yasnippet"))
(use-package yasnippet
  :defer t
  :bind
  (:map yas-minor-mode-map
        ("M-s y" . yas-insert-snippet))
  :config
  (yas-reload-all)
  (yas-global-mode))
(straight-use-package '(yasnippet-snippets :type git :host github :repo "artemkovalyov/yasnippet-snippets"))
(straight-use-package 'react-snippets)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.mdx\\'" . markdown-mode)
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

;; (use-package nyan-mode

;;   :custom
;;    (nyan-cat-face-number 4)
;;    (nyan-animate-nyancat t)
;;    (nyan-mode t))

(use-package rainbow-mode
  :diminish
  :hook
  (emacs-lisp-mode . rainbow-mode)
  (js-mode . rainbow-mode))


;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(straight-use-package 'helpful)
(use-package helpful
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f" . 'helpful-callable)
         ("C-h v" . 'helpful-variable)
         ("C-h k" . 'helpful-key)
         ("C-h F" . 'helpful-function)
         ("C-h C" . 'helpful-command)))

;; Gradle
(use-package gradle-mode
  :config
  (gradle-mode 1)
  :mode ("\\.gradle\\'"))

;;(require 'visual-regexp)
(straight-use-package '(visual-regexp :type git :host github :repo "benma/visual-regexp.el"))
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;;duplicating lines and words
(straight-use-package '(duplicate-thing :type git :host github :repo "ongaeshi/duplicate-thing"))
(use-package duplicate-thing
  :bind
  ("M-d" . duplicate-thing))

(straight-use-package '(switch-window :type git :host github :repo "dimitri/switch-window"))
(use-package switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty
        switch-window-minibuffer-shortcut ?z)
  :bind
  ("s-w" . switch-window)
  ("C-w" . switch-window-then-maximize))

(straight-use-package '(rg :type git :host github :repo "dajva/rg.el"))
(use-package rg
  :defer
  :init
  (rg-enable-default-bindings))

(straight-use-package '(deadgrep :type git :host github :repo "Wilfred/deadgrep"))
(use-package deadgrep
  :defer
  :bind
  ("M-s f" . deadgrep))


(provide 'base-extensions)
