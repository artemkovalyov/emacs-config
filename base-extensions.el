;; -*- lexical-binding: t -*-
;;; package --- base-extensions.el
;;; Commentary:

;;;Code:


(use-package avy
  :straight (avy :type git :host github :repo "abo-abo/avy")
  :bind
  ("s-g s-c" . avy-copy-line)
  ("s-g s-m" . avy-move-line)
  ("s-g w" . avy-goto-word-or-subword-1)
  ("s-g l" . avy-goto-line)
  ("s-g c" . avy-goto-char))

;; https://github.com/minad/consult
(use-package consult
  :straight (consult :type git :host github :repo "minad/consult")
  :config
  (setq consult-preview-key nil)
  :bind
  ("A-b" . consult-buffer)
  ("M-f" . consult-ripgrep)
  ("s-b" . consult-bookmark)
  ("A-f" . consult-line)
  ("M-o" . consult-outline))

(use-package consult-dir
  :straight (consult-dir :type git :host github :repo "karthink/consult-dir")
  :bind (("C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  ;; A function that returns a list of directories
  (defun consult-dir--fasd-dirs ()
    "Return list of fasd dirs."
    (split-string (shell-command-to-string "fasd -ld") "\n" t))

  ;; A consult source that calls this function
  (defvar consult-dir--source-fasd
    `(:name     "Fasd dirs"
                :narrow   ?f
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () (executable-find "fasd"))
                :items    ,#'consult-dir--fasd-dirs)
    "Fasd directory source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-fasd t))


(use-package pcre2el
  :straight (pcre2el :type git :host github :repo "joddie/pcre2el"))


(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect-first t)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  :bind
  (:map corfu-map
        ([escape] . corfu-quit))
  :init
  (global-corfu-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :straight (embark-consult :type git :host github :repo "oantolin/embark")
  :bind
  (("M-a" . embark-act)
   ("M-d" . embark-dwim)
   ("M-s b" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight (embark-consult :type git :host github :repo "oantolin/embark")
  :config
  (setf  (alist-get 'consult-location  embark-exporters-alist) #'embark-consult-export-location-grep)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable vertico, use M-RET to chose non-existing candidates
(use-package vertico
  :straight (vertico :type git :host github :repo "minad/vertico" :files ("vertico.el" "extensions/vertico-buffer.el"))
  :init
  (vertico-mode)
  (setq vertico-count 17))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete))

(use-package cape
  :straight (cape :type git :host github :repo "minad/cape")
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'text-mode-hook (lambda ()
                              (add-to-list 'completion-at-point-functions #'cape-tex)
                              (add-to-list 'completion-at-point-functions #'cape-sgml))))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))


(use-package dashboard
  :init
  (setq dashboard-items '((recents  . 11)
                          (bookmarks . 5)
                          (registers . 5))
	dashboard-banner-logo-title "Life's Awesome!"
	dashboard-startup-banner 'official)
  :config
  (dashboard-setup-startup-hook))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-diff-options "-w")
  (setq-default ediff-forward-word-function 'forward-char)
  (setq-default ediff-highlight-all-diffs 't))


(use-package expand-region
  :bind
  ("A-s ." . er/expand-region))

(use-package comment-dwim-2
  :defer t
  :bind
  ("A-/" . comment-dwim-2))

;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :straight (wgrep :type git :host github :repo "mhayashi1120/Emacs-wgrep"))


(use-package magit
  :straight (magit :type git :host github :repo "magit/magit")
  :config
  (setq magit-refresh-status-buffer nil)
  :bind
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


(use-package forge
  :straight (forge :type git :host github :repo "magit/forge")
  :after magit)

(use-package smartparens
  :straight (smartparens :host github :repo "Fuco1/smartparens")
  :init
  (setq sp-navigate-reindent-after-up-in-string nil
        sp-navigate-reindent-after-up nil)
  :bind
  ("H-M-j" . sp-backward-delete-word)
  ("H-M-l" . sp-delete-word)
  :hook
  (text-mode . smartparens-mode)
  (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package undo-fu
  :config
  (setq undo-limit 6710886400) ;; 64mb.
  (setq undo-strong-limit 100663296) ;; 96mb.
  (setq undo-outer-limit 1006632960) ;; 960mb.
  :bind
  ("C-z" . undo-fu-only-undo)
  ("C-y" . undo-fu-only-redo))

(use-package which-key
  :config
  (which-key-mode))

(use-package yasnippet
  :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet")
  :init
  (yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("A-y" . yas-insert-snippet))
  :config
  (yas-reload-all))

(straight-use-package '(yasnippet-snippets :type git :host github :repo "artemkovalyov/yasnippet-snippets"))

(use-package rainbow-mode
  :diminish
  :hook
  (prog-mode . rainbow-mode))


(use-package helpful
  :straight (helpful :type git :host github :repo "Wilfred/helpful")
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f" . 'helpful-callable)
         ("C-h v" . 'helpful-variable)
         ("C-h k" . 'helpful-key)
         ("C-h F" . 'helpful-function)
         ("C-h C" . 'helpful-command)))


(use-package visual-regexp
  :straight (visual-regexp :type git :host github :repo "benma/visual-regexp.el")
  :bind
  ("A-s r" . 'vr/replace)
  ("A-s q" . 'vr/query-replace)
  ("A-s m" . 'vr/mc-mark))

(use-package rg
  :straight (rg :type git :host github :repo "dajva/rg.el")
  :defer
  :bind
  ("s-f" . rg-menu))

(use-package emmet-mode
  :straight (emmet-mode :type git :host github :repo "smihica/emmet-mode")
  :bind
  (:map emmet-mode-keymap
        (("C-j" . nil)))
  :hook
  ((typescript-mode . emmet-mode)
   (js-mode . emmet-mode)
   (html-mode . emmet-mode)
   (css-mode . emmet-mode)
   (markdown-mode . emmet-mode)
   (svelte-mode . emmet-mode)
   (web-mode . emmet-mode)))

(use-package restart-emacs
  :straight (restart-emacs :type git :host github :repo "raxod502/restart-emacs"))

(use-package ctrlf
  :straight (ctrlf :type git :host github :repo "raxod502/ctrlf")
  :config
  (ctrlf-mode +1))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf  (alist-get 'svelte-mode apheleia-mode-alist) '(prettier-svelte))
  (setf  (alist-get 'typescript-mode apheleia-mode-alist) '(prettier-typescript))
  (setf  (alist-get 'typescript-ts-mode apheleia-mode-alist) '(prettier-typescript))
  (setf  (alist-get 'tsx-ts-mode apheleia-mode-alist) '(prettier-typescript))
  (setf  (alist-get 'html-mode apheleia-mode-alist) '(prettier-html))
  (setf  (alist-get 'js-mode apheleia-mode-alist) '(prettier-javascript))
  (setf  (alist-get 'js-ts-mode apheleia-mode-alist) '(prettier-javascript))
  (setf  (alist-get 'web-mode apheleia-mode-alist) '(prettier-html))
  (setf  (alist-get 'markdown-mode apheleia-mode-alist) '(prettier-markdown))
  (setf  (alist-get 'markdown-ts-mode apheleia-mode-alist) '(prettier-markdown))
  :init
  (apheleia-global-mode +1))

(use-package windmove
  :bind
  ("s-o" . other-window)
  ("s-j" . windmove-left)
  ("s-l" . windmove-right)
  ("s-k" . windmove-down)
  ("s-i" . windmove-up)
  ("s-w" . delete-other-windows)
  ("s-q " . delete-window)
  ("s-M-<up>" . enlarge-window)
  ("s-M-<down>" . shrink-window)
  ("s-M-<left>" . shrink-window-horizontally)
  ("s-M-<right>" . enlarge-window-horizontally)
  ("s-h" . split-window-horizontally)
  ("s-v" . split-window-vertically))

(use-package pulsar
  :straight (pulsar :type git :host github :repo "protesilaos/pulsar")
  :init
  (setq pulsar-pulse-functions
        '(recenter-top-bottom
          move-to-window-line-top-bottom
          reposition-window
          bookmark-jump
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          org-next-visible-heading
          org-previous-visible-heading
          org-forward-heading-same-level
          org-backward-heading-same-level
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading))
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  :bind ("M-p" . #'pulsar-pulse-line)
  :config
  (pulsar-global-mode 1))

(use-package uuidgen :straight (uuidgen :type git :host github :repo "kanru/uuidgen-el"))

(use-package sudo-edit :straight (sudo-edit :type git :host github :repo "nflath/sudo-edit"))

(use-package dirvish
  :straight (dirvish :type git :host github :repo "alexluigit/dirvish")
  :bind
  ("H-d" . dirvish-side)
  (:map dirvish-mode-map
        ("TAB" . dirvish-subtree-toggle))
  :config
  (setq dired-dwim-target t)
  (setq dirvish-side-width 50)
  :init
  (dirvish-override-dired-mode))

(provide 'base-extensions)
