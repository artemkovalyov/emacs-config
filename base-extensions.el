
;;; package --- base-extensions.el
;;; Commentary:

;;;Code:

(setq flyspell-use-meta-tab nil)

;; A more complex, more lazy-loaded config
;;(use-package solaire-mode
;; Ensure solaire-mode is running in all solaire-mode buffers
;;  :hook (change-major-mode . turn-on-solaire-mode)
;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;; itself off every time Emacs reverts the file
;;  :hook (after-revert . turn-on-solaire-mode)
;; To enable solaire-mode unconditionally for certain modes:
;;  :hook (ediff-prepare-buffer . solaire-mode)
;; Highlight the minibuffer when it is activated:
;;  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
;;  :config
;; The bright and dark background colors are automatically swapped the first
;; time solaire-mode is activated. Namely, the backgrounds of the `default` and
;; `solaire-default-face` faces are swapped. This is done because the colors
;; are usually the wrong way around. If you don't want this, you can disable it:
;;  (setq solaire-mode-auto-swap-bg nil)
;;  (solaire-global-mode +1))

(use-package drag-stuff
  :straight (drag-stuff :type git :host github :repo "rejeep/drag-stuff.el")
   :config
   (drag-stuff-global-mode 1)
   :bind
   ("A-M-j" . drag-stuff-left)
   ("A-M-l" . drag-stuff-right)
   ("A-M-i" . drag-stuff-up)
   ("A-M-k" . drag-stuff-down))

(straight-use-package '(avy :type git :host github :repo "abo-abo/avy"))
(use-package avy
  :bind
  ("s-g s-c" . avy-copy-line)
  ("s-g s-m" . avy-move-line)
  ("s-g c" . avy-goto-word-or-subword-1)
  ("s-g l" . avy-goto-line))

(use-package consult
  :straight
  (consult :type git :host github :repo "minad/consult")
  :bind
  ("A-b" . consult-buffer)
  ("A-f" . consult-ripgrep)
  ("s-b" . consult-bookmark)
  ("A-s l" . consult-line))

(use-package cape
  :straight
  (cape :type git :host github :repo "minad/cape"))

(use-package corfu
  :straight
  (corfu :type git :host github :repo "minad/corfu")
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delya 0.2)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;; :bind
  ;; (:map corfu-map
  ;;        ("TAB" . corfu-next)
  ;;        ([tab] . corfu-next)
  ;;        ("S-TAB" . corfu-previous)
  ;;        ([backtab] . corfu-previous)
  ;;        ([escape] . corfu-quit))

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

;; Optionally use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides nil))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))


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
  ("A-." . er/expand-region))

(use-package flycheck
  :straight (flycheck :type git :host github :repo "flycheck/flycheck")
  :hook (lsp-mode . flycheck-mode)
  ;; :config
  ;; (add-to-list 'display-buffer-alist
  ;;            `(,(rx bos "*Flycheck errors*" eos)
  ;;             (display-buffer-reuse-window
  ;;              display-buffer-in-side-window)
  ;;             (side            . bottom)
  ;;             (reusable-frames . visible)
  ;;             (window-height   . 0.21)))
  )

(use-package comment-dwim-2
  :defer t
  :bind
    ("A-/" . comment-dwim-2))

(straight-use-package '(wgrep :type git :host github :repo "mhayashi1120/Emacs-wgrep"))
(use-package wgrep)


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


(straight-use-package '(magit-popup :type git :host github :repo "magit/magit-popup"))
(use-package magit-popup
  :after magit)

(straight-use-package '(forge :type git :host github :repo "magit/forge"))
(use-package forge
  :after magit)

(use-package multiple-cursors
  :bind
  ("s-;" . mc/edit-lines)
  ("A-s-e" . mc/unmark-next-like-this)
  ("A-s-d" . mc/unmark-previous-like-this)
  ("s-d" . mc/mark-next-like-this)
  ("s-e" . mc/mark-previous-like-this)
  ("C-n" . mc/mark-all-like-this)
  ("A-<mouse-1>" . mc/add-cursor-on-click))

(use-package projectile
  :ensure t
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
  (projectile-register-project-type 'yarn '("package.json"))
  (projectile-mode 1))

(straight-use-package '(smartparens :host github :repo "Fuco1/smartparens"))

(use-package smartparens
  :init
  (setq sp-navigate-reindent-after-up-in-string nil
        sp-navigate-reindent-after-up nil)
  :bind
  ("H-i" . sp-backward-sexp)
  ("H-k" . sp-forward-sexp)
  ("H-l" . sp-down-sexp)
  ("H-j" . sp-up-sexp)
  :hook
  (text-mode . smartparens-mode)
  (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

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


(use-package rainbow-mode
  :diminish
  :hook
  (prog-mode . rainbow-mode))


;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package helpful
  :straight (helpful :type git :host github :repo "Wilfred/helpful")
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
 (setq
  switch-window-input-style 'read-event
  switch-window-shortcut-style 'qwerty
  switch-window-minibuffer-shortcut ?z)
 :bind
 ("s-SPC" . #'switch-window)
 ("s-m" . #'switch-window-then-maximize)
 ("s-q" . #'switch-window-then-delete)
 ("s-h" . #'switch-window-then-split-horizontally)
 ("s-v" . #'switch-window-then-split-vertically))

(straight-use-package '(rg :type git :host github :repo "dajva/rg.el"))
(use-package rg
  :defer
  :bind
  ("A-s f" . rg-menu))

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

(straight-use-package 'selectrum)
(use-package selectrum
  :init
  (setq selectrum-max-window-height 20)
  (setq selectrum-display-action t)
  ;; (setq selectrum-display-action
  ;;       '(selectrum-display-full-frame))
  :bind
  (:map selectrum-minibuffer-map
        ([tab] . selectrum-insert-current-candidate))
  :config
  (selectrum-mode +1))

(straight-use-package 'prescient)

(straight-use-package 'selectrum-prescient)
(use-package selectrum-prescient
  :config
  ;; to make sorting and filtering more intelligent
  (selectrum-prescient-mode +1)

  ;; to save your command history on disk, so the sorting gets more
  ;; intelligent over time
  (prescient-persist-mode +1))

;; (setq selectrum-display-action '(display-buffer-show-in-posframe))

;; (defun display-buffer-show-in-posframe (buffer _alist)
;;   (frame-root-window
;;    (posfraime-show buffer
;;                   :min-height 20
;;                   :min-width (frame-width)
;;                   :internal-border-width 1
;;                   :left-fringe 8
;;                   :right-fringe 8
;;                   :poshandler 'posframe-poshandler-frame-bottom-left-corner)))

;; (add-hook 'minibuffer-exit-hook 'posframe-delete-all)

(use-package highlight-indent-guides
  :straight (highlight-indent-guides :host github :repo "DarthFennec/highlight-indent-guides")
  :custom (highlight-indent-guides-method 'character)
  :hook (prog-mode . highlight-indent-guides-mode))

(provide 'base-extensions)
