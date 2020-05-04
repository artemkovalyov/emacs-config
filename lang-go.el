(straight-use-package '(go-mode :type git :host github :repo "dominikh/go-mode.el"))

(use-package go-mode
  :config
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  :hook
  ((before-save . gofmt-before-save)
  (go-mode . setup-go-mode-compile)
  (go-mode . smartparens-mode)
  (go-mode . (lambda ()
	      (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (go-mode . (lambda ()
	      (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
	))



(defun setup-go-mode-compile ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(provide 'lang-go)
