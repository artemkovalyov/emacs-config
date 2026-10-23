;; -*- lexical-binding: t; -*-
;;; package --- python configs
;;; Commentary:
;;; Code:

(use-package lsp-pyright
  :straight (lsp-pyright :type git :host github :repo "emacs-lsp/lsp-pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package pyenv-mode
  :if (executable-find "pyenv")
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))

(use-package f :straight (f :type git :host github :repo "rejeep/f.el"))
(use-package s :straight (s :type git :host github :repo "magnars/s.el"))

(defun pyenv-init ()
  (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
  (message (concat "Setting pyenv version to " global-pyenv))
  (pyenv-mode-set global-pyenv)
  (defvar pyenv-current-version nil global-pyenv))

(defun pyenv-activate-current-project ()
  "Automatically activate pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (message path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (progn
             (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
             (pyenv-mode-set pyenv-current-version)
             (pyvenv-workon pyenv-current-version)
             (message (concat "Setting virtualenv to " pyenv-current-version))))))))

(add-hook 'after-init-hook 'pyenv-init)

(provide 'lang-python)
;;; lang-python.el ends here
