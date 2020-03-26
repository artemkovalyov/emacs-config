(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))

(defun tkj-default-code-style-hook()
  (setq c-basic-offset 2
        c-label-offset 0
        tab-width 2
        indent-tabs-mode nil
        compile-command "mvn -q -o -f ~/src/content-engine/pom.xml install"
        require-final-newline nil))
(add-hook 'java-mode-hook 'tkj-default-code-style-hook)

;;(use-package idle-highlight)

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  ;; (git-gutter+-mode)
  ;; (gtags-mode)
  ;; (idle-highlight)
  (subword-mode)
  ;; (yas-minor-mode)
  (set-fringe-style '(8 . 0))
  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; (use-package projectile :ensure t)
;; (use-package yasnippet :ensure t)

;; (use-package lsp-java
;;   :ensure t
;;   :after lsp-mode
;;   :init
;;   (setq lsp-java-vmargs
;;         (list
;;          "-noverify"
;;          "-Xmx1G"
;;          "-XX:+UseG1GC"
;;          "-XX:+UseStringDeduplication"
;;          "-javaagent:/home/torstein/.m2/repository/org/projectlombok/lombok/1.18.6/lombok-1.18.6.jar"
;;          )

;;         ;; Don't organise imports on save
;;         lsp-java-save-action-organize-imports nil

;;         ;; Currently (2019-04-24), dap-mode works best with Oracle
;;         ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
;;         ;;
;;         ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
;;         lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
;; 	lsp-java-server-install-dir "/home/artem/java/"
;;         )

;;   :config
;;   (add-hook 'java-mode-hook #'lsp))

;; (use-package dap-mode
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t)
;;   (dap-tooltip-mode 1)
;;   (tooltip-mode 1)
;;   (dap-register-debug-template
;;    "localhost:5005"
;;    (list :type "java"
;;          :request "attach"
;;          :hostName "localhost"
;;          :port 5005)))

;; (use-package dap-java
;;   :ensure nil
;;   :after (lsp-java)

;;   ;; The :bind here makes use-package fail to lead the dap-java block!
;;   ;; :bind
;;   ;; (("C-c R" . dap-java-run-test-class)
;;   ;;  ("C-c d" . dap-java-debug-test-method)
;;   ;;  ("C-c r" . dap-java-run-test-method)
;;   ;;  )

;;   :config
;;   (global-set-key (kbd "<f7>") 'dap-step-in)
;;   (global-set-key (kbd "<f8>") 'dap-next)
;;   (global-set-key (kbd "<f9>") 'dap-continue)
;;   )

(provide 'lang-java)
;;; lang-java.el ends here
