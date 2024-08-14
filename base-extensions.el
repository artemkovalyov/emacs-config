;; -*- lexical-binding: t -*-
;;; package --- base-extensions.el
;;; Commentary:

;;;Code:

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
  ("s-g b" . bookmark-delete)
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

;; (use-package consult-lsp
;;   :straight (consult-lsp :type git :host github :repo "gagbo/consult-lsp")
;;   ;; :bind ("s-g e" . consult-flycheck)
;;   )

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
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind
  (:map corfu-map
        ([escape] . corfu-quit))
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t
  :straight (embark-consult :type git :host github :repo "oantolin/embark" :build (:not compile))
  :bind
  (("M-a" . embark-act)         ;; pick some comfortable binding
   ("M-d" . embark-dwim)        ;; good alternative: M-.
   ("M-s b" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :straight (embark-consult :type git :host github :repo "oantolin/embark")
  ;; :straight (embark-consult :type git :host github :repo "artemkovalyov/embark" :build (:not compile))
  :config
  (setf  (alist-get 'consult-location  embark-exporters-alist) #'embark-consult-export-location-grep)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable vertico, use M-RET to chose non-existing candidates
(use-package vertico
  :straight (vertico :type git :host github :repo "minad/vertico" :files ("vertico.el" "extensions/vertico-buffer.el"))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 17)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package cape
  :straight
  (cape :type git :host github :repo "minad/cape")
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )


;; Optionally use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
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
  :demand t
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
  (setq dashboard-items '((recents  . 11)
                          (bookmarks . 5)
                          (registers . 5))
	dashboard-banner-logo-title "Life's Awesome!"
	dashboard-startup-banner 'official)
  :config
  (dashboard-setup-startup-hook))

(use-package ediff
  :bind
  ;; (:map diff-mode-menu
  :hook (ediff-keymap-setup . (lambda()
                                (define-key ediff-mode-map (kbd "A-i") #'ediff-previous-difference)
                                (define-key ediff-mode-map (kbd "A-k") #'ediff-next-difference)))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-diff-options "-w")
  (setq-default ediff-forward-word-function 'forward-char)
  (setq-default ediff-highlight-all-diffs 't))


(use-package expand-region
  :bind
  ("A-." . er/expand-region))

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


(use-package magit-popup
  :straight (magit-popup :type git :host github :repo "magit/magit-popup")
  :after magit)

(use-package forge
  :straight (forge :type git :host github :repo "magit/forge")
  :after magit)

(use-package multiple-cursors
  :bind
  ("s-;" . mc/edit-lines)
  ("A-s-e" . mc/unmark-next-like-this)
  ("A-s-d" . mc/unmark-previous-like-this)
  ("s-d" . mc/mark-next-like-this)
  ("s-e" . mc/mark-previous-like-this)
  ("C-n" . mc/mark-all-like-this)
  ("s-<mouse-1>" . mc/add-cursor-on-click))

(use-package smartparens
  :straight (smartparens :host github :repo "Fuco1/smartparens")
  :init
  (setq sp-navigate-reindent-after-up-in-string nil
        sp-navigate-reindent-after-up nil)
  :bind
  ("H-i" . sp-backward-sexp)
  ("H-k" . sp-forward-sexp)
  ("H-l" . sp-down-sexp)
  ("H-j" . sp-up-sexp)
  ("M-H-j" . sp-backward-delete-word)
  ("M-H-l" . sp-delete-word)
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
  :ensure t
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
(use-package duplicate-thing
  :straight (duplicate-thing :type git :host github :repo "artemkovalyov/duplicate-thing")
  :bind
  ("A-d" . duplicate-thing))

;; https://github.com/dajva/rg.el
(straight-use-package '(rg :type git :host github :repo "dajva/rg.el"))
(use-package rg
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
  ;; :straight (apheleia :host github :repo "raxod502/apheleia" :build (:not compile))
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  ;; (setq apheleia-log-only-errors nil)
  (add-to-list 'apheleia-formatters '(prettier-svelte . ("prettier" "--stdin-filepath" filepath "--parser=svelte")))
  (setf  (alist-get 'svelte-mode apheleia-mode-alist) '(prettier-svelte))
  (setf  (alist-get 'typescript-mode apheleia-mode-alist) '(prettier))
  (setf  (alist-get 'html-mode apheleia-mode-alist) '(prettier))
  (setf  (alist-get 'js-mode apheleia-mode-alist) '(prettier))
  (setf  (alist-get 'web-mode apheleia-mode-alist) '(prettier))
  (setf  (alist-get 'markdown-mode apheleia-mode-alist) '(prettier))
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
  ("A-s-i" . enlarge-window)
  ("A-s-k" . shrink-window)
  ("A-s-j" . shrink-window-horizontally)
  ("A-s-l" . enlarge-window-horizontally)
  ("s-h" . split-window-horizontally)
  ("s-v" . split-window-vertically))

(use-package pulsar
  :straight (pulsar :type git :host github :repo "protesilaos/pulsar")
  :init
  (setq pulsar-pulse-functions
        ;; NOTE 2022-04-09: The commented out functions are from before
        ;; the introduction of `pulsar-pulse-on-window-change'.  Try that
        ;; instead.
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
