(straight-use-package 'slime)
(straight-use-package '(helm-slime :type git :host github :repo "emacs-helm/helm-slime"))

(use-package slime
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup '(slime-fancy slime-quicklisp slime-asdf helm-slime slime-repl slime-parse slime-fuzzy slime-indentation))))

(provide 'lang-lisp)
;;; lang-list.el ends here
