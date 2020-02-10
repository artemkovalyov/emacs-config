;;; package --- my ui config


;;; Commentary:

;;; Code:

(use-package 'emojify
  :commands emojify-mode
  :hook ((after-init . global-emojify-mode))
  :init (gsetq emojify-emoji-styles '(unicode github)
               emojify-display-style 'unicode))

(use-package all-the-icons
  :init (gsetq inhibit-compacting-font-caches t))

(provide 'ui)
;;; ui.el ends here
