(straight-use-package '(docker :type git :host github :repo "Silex/docker.el"))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
