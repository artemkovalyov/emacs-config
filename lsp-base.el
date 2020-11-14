;; -*- lexical-binding: t; -*-
;;; package --- lsp.el
;;; Commentary: LSP configuration
;;; Code:

(straight-use-package '(lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode"))
(straight-use-package '(lsp-java :type git :host github :repo "emacs-lsp/lsp-java"))

(setq   lsp-eslint-server-command '("node" "/home/artem/server/out/eslintServer.js" "--stdio" )
        lsp-eslint-auto-fix-on-save t
        lsp-eslint-enable t
        lsp-eslint-trace-server "on"
        lsp-eslint-quiet nil)

;;; activate LSP mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-m"
        lsp-prefer-capf t
        ;; lsp-log-io t ; enable debug log - can be a huge performance hit
        lsp-disabled-clients '(eslint)
        lsp-treemacs-sync-mode 1
        )

  :commands (lsp lsp-deferred)
  :hook
  ((js-mode typescript-mode go-mode) . lsp-deferred)
  :bind
  (:map lsp-mode-map
        ("C-c r"   . lsp-rename))
  )

(straight-use-package '(lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui"))
;; LSP UI tools
(use-package lsp-ui
  :commands
  lsp-ui-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  ;; (lsp-ui-doc-max-width 20)
  ;; (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "")
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  ;; semantic highlight
  (lsp-enable-semantic-highlighting t)
  :preface
  (defun toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
	(progn
	  (lsp-ui-doc-mode -1)
	  (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
	("C-c C-r" . lsp-ui-peek-find-references)
	("C-c C-j" . lsp-ui-peek-find-definitions)
	("C-c i"   . lsp-ui-peek-find-implementation)
	("C-c m"   . lsp-ui-imenu)
	("C-c s"   . lsp-ui-sideline-mode)
	("C-c d"   . toggle-lsp-ui-doc))
  :hook
  (lsp-mode . lsp-ui-mode))

(straight-use-package '(helm-lsp :type git :host github :repo "emacs-lsp/helm-lsp"))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)


;; (straight-use-package '(lsp-treemacs :type git :host github :repo "emacs-lsp/lsp-treemacs"))
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger

;; (straight-use-package '(dap-mode :type git :host github :repo "emacs-lsp/dap-mode")) ;
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode)) ;

;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(require 'lsp-java)
(require 'dap-java)

(add-hook 'java-mode-hook #'lsp)
(setq lsp-java-vmargs
      (list
       "-noverify"
       "-Xmx2G"
       "-XX:+UseG1GC"
       "-XX:+UseStringDeduplication"
       (concat "-javaagent:" "/home/i531196/.m2/repository/org/projectlombok/lombok/1.16.20/lombok-1.16.20.jar")
       (concat "-Xbootclasspath/a:" "/home/i531196/.m2/repository/org/projectlombok/lombok/1.16.20/lombok-1.16.20.jar"))
      lsp-file-watch-ignored
      '(".idea" ".ensime_cache" ".eunit" "node_modules"
        ".git" ".hg" ".fslckout" "_FOSSIL_"
        ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
        "build")
      lsp-java-import-order '["" "java" "javax" "#"]
      ;; Don't organize imports on save
      lsp-java-save-action-organize-imports nil

      ;; ;; Formatter profile
      ;; lsp-java-format-settings-url
      ;; (concat "file://" jmi/java-format-settings-file))

      ;; lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
      )


(provide 'lsp-base)
;;; lsp-base.el ends here
