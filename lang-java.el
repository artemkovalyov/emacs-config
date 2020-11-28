(straight-use-package '(lsp-java :type git :host github :repo "emacs-lsp/lsp-java"))

(use-package lsp-java
  :defer t
  :config
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
  :hook (java-mode lsp))

(provide 'lang-java)
