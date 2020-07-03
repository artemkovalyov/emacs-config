;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)

;; ;; (straight-use-package '(lsp-java :type git :host github :repo "emacs-lsp/lsp-java"))
;; (use-package lsp-java
;;   :ensure t
;;  ;;  (setq lsp-java-vmargs
;; ;;         (list
;; ;;          "-noverify"
;; ;;          "-Xmx2G"
;; ;;          "-XX:+UseG1GC"
;; ;;          "-XX:+UseStringDeduplication"
;; ;;          (concat "-javaagent:" jmi/lombok-jar)
;; ;;          (concat "-Xbootclasspath/a:" jmi/lombok-jar))
;; ;;         lsp-file-watch-ignored
;; ;;         '(".idea" ".ensime_cache" ".eunit" "node_modules"
;; ;;           ".git" ".hg" ".fslckout" "_FOSSIL_"
;; ;;           ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
;; ;;           "build")
;; ;;         lsp-java-import-order '["" "java" "javax" "#"]
;; ;;         ;; Don't organize imports on save
;; ;;         lsp-java-save-action-organize-imports nil

;; ;;         ;; ;; Formatter profile
;; ;;         ;; lsp-java-format-settings-url
;; ;;         ;; (concat "file://" jmi/java-format-settings-file))

;; ;; ;;         ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
;; ;;         lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
;; ;; 	lsp-java-server-install-dir "~/java/"

;;   :hook
;;   (java-mode . lsp))

;; ;;( use-package dap-mode
;; ;;   :ensure t
;; ;;   :after lsp-mode
;; ;;   :config
;; ;;   (dap-mode t)
;; ;;   (dap-ui-mode t)
;; ;;   (dap-tooltip-mode 1)
;; ;;   (tooltip-mode 1)
;; ;;   (dap-register-debug-template
;; ;;    "localhost:5005"
;; ;;    (list :type "java"
;; ;;          :request "attach"
;; ;;          :hostName "localhost"
;; ;;          :port 5005)))

;; ;; (use-package dap-java
;; ;;   :ensure nil
;; ;;   :after (lsp-java)

;; ;;   ;; The :bind here makes use-package fail to lead the dap-java block!
;; ;;   ;; :bind
;; ;;   ;; (("C-c R" . dap-java-run-test-class)
;; ;;   ;;  ("C-c d" . dap-java-debug-test-method)
;; ;;   ;;  ("C-c r" . dap-java-run-test-method)
;; ;;   ;;  )

;; ;;   :config
;; ;;   (global-set-key (kbd "<f7>") 'dap-step-in)
;; ;;   (global-set-key (kbd "<f8>") 'dap-next)
;; ;;   (global-set-key (kbd "<f9>") 'dap-continue)
;; ;;   )

;; (provide 'lang-java)
;; ;;; lang-java.el ends here
