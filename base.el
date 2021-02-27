;; -*- lexical-binding: t; -*-
;;; package --- base.el
;;; Commentary:
;;; Code:
;; (package-initialize)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq     gc-cons-threshold    art-gc-cons-threshold ;;most-positive-fixnum
                      gc-cons-percentage   0.1
                      read-process-output-max           (* 1024 1024 1024))))

(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold art-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;; straight.el package manager
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package '(el-patch :type git :host github :repo "raxod502/el-patch"))
(straight-use-package '(use-package :type git :host github :repo "jwiegley/use-package"))

(use-package el-patch
  :straight t)
(setq straight-use-package-by-default t)

(straight-use-package 'benchmark-init)

(use-package benchmark-init
  :demand t
  :hook ((after-init . benchmark-init/deactivate)))

(setq hscroll-margin                  7
      scroll-margin                   7
      hscroll-step                    7
      scroll-step                     7
      scroll-conservatively           100000
      scroll-preserve-screen-position t
      mac-mouse-wheel-smooth-scroll    nil
      scroll-error-top-bottom          t)

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories.")


;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(global-visual-line-mode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Emacs customizations
(setq-default
 confirm-kill-emacs                  'y-or-n-p
 confirm-nonexistent-file-or-buffer  t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point                 t
 require-final-newline               t
 visible-bell                        nil
 ring-bell-function                  'ignore
 custom-file                         "~/.emacs.d/.custom.el"
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

 ;; Disable non selected window highlight
 cursor-in-non-selected-windows     nil
 highlight-nonselected-windows      nil
 ;; PATH
 exec-path                          (append exec-path '("/usr/local/bin/"))
 indent-tabs-mode                   nil
 inhibit-startup-message            t
 fringes-outside-margins            t
 x-select-enable-clipboard          t
 use-package-always-ensure          t
 debug-on-error                     nil ; Activated debugging on any error in Emacs
 message-log-max                    t
 load-prefer-newer                  t
 window-combination-resize          t
 x-stretch-cursor                   t
 truncate-string-ellipsis           "…"
 ad-redefinition-action             'accept
 tramp-default-method               "ssh"
 )

;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                 t
 bookmark-default-file              (concat temp-dir "/bookmarks"))

;; Backups enabled, use nil to disable
(setq-default
 history-length                     1000
 backup-inhibited                   nil
 make-backup-files                  t
 auto-save-default                  t
 auto-save-list-file-name           (concat temp-dir "/autosave")
 make-backup-files                  t
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))

(unless (file-exists-p (concat temp-dir "/auto-save-list"))
		       (make-directory (concat temp-dir "/auto-save-list") :parents))

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)
(global-hl-line-mode)
(global-display-line-numbers-mode)
;; Autoclose brackets
(electric-pair-mode t)
(global-subword-mode t)


;; Show column number in status bar
(column-number-mode)

;; Disable toolbar & menubar
(menu-bar-mode -1)
(tab-bar-mode)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(show-paren-mode 1)

(delete-selection-mode 1)
(setq next-line-add-newlines t)

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook
;;         (lambda() (set-fill-column 120)))

(setq-default fill-column '120)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; change default size of diff region from a word to a char
(setq-default ediff-forward-word-function 'forward-char)
(setq-default ediff-highlight-all-diffs t)


(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(provide 'base)
;;; base ends here
