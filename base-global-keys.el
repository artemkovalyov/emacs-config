;; -*- lexical-binding: t; -*-
;;; package --- Add your keys here, as such

;;; Commentary:

;;; Code:
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)

;; Killing lines
(global-set-key (kbd "C-s-k") 'artem/kill-line-down)
(global-set-key (kbd "C-s-i") 'artem/kill-line-up)

;; Moving across text
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)

;; Marking
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Moving beg/end of line
(global-set-key (kbd "M-w") 'artem/beginning-of-line)
(global-set-key (kbd "M-e") 'artem/end-of-line)

;;
(global-set-key (kbd "s-l") 'forward-word)
(global-set-key (kbd "s-j") 'backward-word)

;; Killing the buffer
(global-set-key (kbd "M-q") 'kill-buffer)

;; Scroll through the buffer and beg/end of the buffer
(global-set-key (kbd "M-s k") 'end-of-buffer)
(global-set-key (kbd "M-s i") 'beginning-of-buffer)
(global-set-key (kbd "s-k") 'scroll-up-command)
(global-set-key (kbd "s-i") 'scroll-down-command)

(global-set-key (kbd "M-s v") 'helm-show-kill-ring)
(global-set-key (kbd "M-s c") 'kill-region)
(global-set-key (kbd "C-x k") 'artem/kill-rest-of-line)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'kill-ring-save)
;; (global-set-key (kbd "s-s") 'hs-toggle)
(global-set-key (kbd "M-s M-w") 'fixup-whitespace)
(global-set-key (kbd "M-s l") 'copy-line)
;; Eval
(global-set-key (kbd "M-r") 'eval-last-sexp)

(provide 'base-global-keys)
;;; base-global-keys.el ends here
