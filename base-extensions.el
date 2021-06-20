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
  ("s-l" . avy-copy-line)
  ("H-s-l" . avy-move-line)
  ("s-j" . avy-goto-word-or-subword-1)
  ("H-s-j" . avy-goto-line))

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
	("A-k" . company-select-next-or-abort)
	("A-i" . company-select-previous-or-abort)
        ("A-s" . company-search-candidates)
        ("A-f" . company-filter-candidates)
        ("<escape>" . company-abort)
        ("TAB" . company-complete)
        ("<tab>" . company-complete)
        ("<return>" . nil)
        ("RET" . nil))
  (:map company-search-map
        ("A-k" . company-select-next)
        ("A-i" . company-select-previous)))

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
  ("A-n" . centaur-tabs-backward)
  ("A-m" . centaur-tabs-forward))

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
    (define-key ediff-mode-map (kbd "A-i") 'ediff-previous-difference)
    (define-key ediff-mode-map (kbd "A-k") 'ediff-next-difference))
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
  ("A-=" . er/expand-region))

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
    ("A-/" . comment-dwim-2))

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
	  helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s"
          helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'")
          )
    (helm-mode))

  :bind (("A-x" . helm-M-x)
         ("A-o" . helm-find-files)
         ("A-s s" . helm-projectile)
	 ("A-s A-s" . helm-projectile-switch-project)
         ("s-o" . helm-occur)
	 ("A-b" . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-select-action)
	 ("A-k" . helm-next-line)
	 ("A-i" . helm-previous-line)
         ("A-h" . helm-toggle-visible-mark-forward)
	 :map helm-find-files-map
	 ("A-i" . helm-previous-line)
	 ("A-k" . helm-next-line)
	 ("<tab>" . helm-ff-TAB)
         ("A-h" . helm-toggle-visible-mark-forward)
	 :map helm-generic-files-map
	 ("A-i" . helm-previous-line)
	 ("A-k" . helm-next-line)
         ("A-h" . helm-toggle-visible-mark-forward)))

(straight-use-package '(helm-projectile :type git :host github :repo "bbatsov/helm-projectile"))
(use-package helm-projectile)

(straight-use-package '(wgrep :type git :host github :repo "mhayashi1120/Emacs-wgrep"))
(use-package wgrep
  :ensure t
  :config (use-package wgrep-helm :ensure t))


(straight-use-package '(magit :type git :host github :repo "magit/magit"))

(use-package magit
  :config
  (setq magit-refresh-status-buffer nil)
  :bind
  ;; Magit
  ("A-g s" . magit-status)
  ("A-g x" . magit-checkout)
  ("A-g c" . magit-commit)
  ("A-g p" . magit-push)
  ("A-g u" . magit-pull)
  ("A-g e" . magit-ediff-resolve)
  ("A-g r" . magit-rebase-interactive)
  ("A-g b" . magit-blame)
  (:map magit-mode-map
	("<tab>" . magit-section-toggle)))

(use-package magit-popup)

(use-package multiple-cursors
  :bind
  ("H-m" . mc/edit-lines)
  ("H-." . mc/mark-next-like-this)
  ("H-," . mc/mark-previous-like-this)
  ("C-n" . mc/mark-all-like-this)
  ("A-<mouse-1>" . mc/add-cursor-on-click))

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
  ("s-e" . sp-backward-sexp)
  ("s-d" . sp-forward-sexp)
  ("s-f" . sp-down-sexp)
  ("s-s" . sp-up-sexp))

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

(straight-use-package '(yasnippet :type git :host github :repo "joaotavora/yasnippet"))
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("A-y" . yas-insert-snippet))
  :config
  (yas-reload-all))

(straight-use-package '(yasnippet-snippets :type git :host github :repo "artemkovalyov/yasnippet-snippets"))
(straight-use-package 'react-snippets)


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

;; (use-package nyan-mode

;;   :custom
;;    (nyan-cat-face-number 4)
;;    (nyan-animate-nyancat t)
;;    (nyan-mode t))

(use-package rainbow-mode
  :diminish
  :hook
  (prog-mode . rainbow-mode))


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


;;(require 'visual-regexp)
(straight-use-package '(visual-regexp :type git :host github :repo "benma/visual-regexp.el"))
(use-package visual-regexp
  :bind
  ("A-s r" . 'vr/replace)
  ("A-s q" . 'vr/query-replace)
  ("A-s m" . 'vr/mc-mark))


;;duplicating lines and
(straight-use-package '(duplicate-thing :type git :host github :repo "artemkovalyov/duplicate-thing"))
(use-package duplicate-thing
  :bind
  ("A-d" . duplicate-thing))

(straight-use-package '(switch-window :type git :host github :repo "dimitri/switch-window"))
(use-package switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty
        switch-window-minibuffer-shortcut ?z)
  :bind
  ("M-<return>" . #'switch-window)
  ("H-<return>" . #'switch-window-then-maximize))

(straight-use-package '(rg :type git :host github :repo "dajva/rg.el"))
(use-package rg
  :defer
  :bind
  ("H-f" . rg-menu))

(straight-use-package '(deadgrep :type git :host github :repo "Wilfred/deadgrep"))
(use-package deadgrep
  :defer
  :bind
  ("A-s f" . #'deadgrep))

(straight-use-package '(helm-rg :type git :host github :repo "cosmicexplorer/helm-rg"))
(use-package helm-rg
  :defer
  :bind
  ("A-f" . helm-rg))

(straight-use-package '(emmet-mode :type git :host github :repo "smihica/emmet-mode"))
(use-package emmet-mode
  :bind
  (:map emmet-mode-keymap
        (("C-j" . nil)))
  :hook
  ((typescript-mode . emmet-mode)
   (js-mode . emmet-mode)
   (html-mode . emmet-mode)
   (css-mode . emmet-mode)
   (markdown-mode . emmet-mode)))

(straight-use-package '(restart-emacs :type git :host github :repo "raxod502/restart-emacs"))
(straight-use-package '(ctrlf :type git :host github :repo "raxod502/ctrlf"))
(use-package ctrlf
  :config
  (ctrlf-mode +1))

(straight-use-package '(apheleia :host github :repo "raxod502/apheleia"))
(use-package apheleia
  :init
  (apheleia-global-mode +1))

(straight-use-package '(nginx-mode :host github :repo "ajc/nginx-mode"))
(use-package nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))


(provide 'base-extensions)
