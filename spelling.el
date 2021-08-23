;; -*- lexical-binding: t; -*-
;;; package --- my spellings config


;;; Commentary:

;;; Code:

(use-package flyspell-correct
  :straight (flyspell-correct :type git :host github :repo "d12frosted/flyspell-correct")
  :after flyspell
  :bind (:map flyspell-mode-map ("A-;" . flyspell-correct-wrapper)))

(setq ispell-choices-win-default-height 3)
(setq flyspell-use-meta-tab nil)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(provide 'spelling)
;;; spelling ends here
