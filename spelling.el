;;; package --- my spelling config


;;; Commentary:

;;; Code:

(setq ispell-choices-win-default-height 3)
(setq flyspell-use-meta-tab nil)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                python-mode-hook
                js-mode-hook
                tyde-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

(global-set-key (kbd "<f7>") 'ispell-word)

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f7>") 'flyspell-check-next-highlighted-word)
(provide 'spelling)
;;; spelling ends here
