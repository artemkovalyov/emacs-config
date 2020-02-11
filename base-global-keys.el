;;; package --- Add your keys here, as such

;;; Commentary:

;;; Code:
(global-unset-key (kbd "C-x h"))
(global-unset-key (kbd "C-x k"))
(global-unset-key (kbd "C-m"))
(global-unset-key (kbd "M-q"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-i"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-l"))

(global-set-key (kbd "<tab>") 'indent-for-tab-command)
(global-set-key (kbd "C-S-D") 'duplicate-thing)
(global-set-key (kbd "C-<down>") 'elpy-nav-move-line-or-region-down)
(global-set-key (kbd "C-<up>") 'elpy-nav-move-line-or-region-up)
(global-set-key (kbd "C-S-k") 'artem/kill-line-down)
(global-set-key (kbd "C-S-i") 'artem/kill-line-up)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-e") 'move-end-of-line)
(global-set-key (kbd "M-a") 'move-beginning-of-line)
(global-set-key (kbd "s-l") 'forward-word)
(global-set-key (kbd "s-j") 'backward-word)
(global-set-key (kbd "M-r") 'eval-last-sexp)
(global-set-key (kbd "M-q") 'kill-buffer)
(global-set-key (kbd "M-p") 'helm-mini)
(global-set-key (kbd "RET") 'newline)
(global-set-key (kbd "s-k") 'scroll-up-command)
(global-set-key (kbd "s-i") 'scroll-down-command)
(global-set-key (kbd "M-s-i") 'beginning-of-buffer)
(global-set-key (kbd "M-s-k") 'end-of-buffer)
(global-set-key (kbd "M-h") 'backward-list)
(global-set-key (kbd "M-n") 'forward-list)
(global-set-key (kbd "M-u") 'backward-sexp)
(global-set-key (kbd "M-m") 'forward-sexp)
(global-set-key (kbd "C-v") 'yank)




(provide 'base-global-keys)
;;; base-global-keys.el ends here
