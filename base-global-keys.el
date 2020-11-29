;; -*- lexical-binding: t; -*-
;;; package --- Add your keys here, as such

;;; Commentary:

;;; Code:

;;; Free keys

;;;C-SPC
;;;C-RET
;;;M-RET
;;;

;;makes M-C-i work later on
(unbind-key "C-M-i")
(unbind-key "C-M-i" emacs-lisp-mode-map)
(unbind-key "C-M-i" lisp-interaction-mode-map)
;; (unbind-key "C-M-i" emacs-lisp-mode-map)
;; (unbind-key "C-M-i" text-mode-map)

(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)

;; Killing lines
(global-set-key (kbd "H-k") 'kill-whole-line)
(global-set-key (kbd "H-i") 'artem/kill-line-up)

;; Moving across text
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)

;; Moving over words right and left
(global-set-key (kbd "s-l") 'forward-word)
(global-set-key (kbd "s-j") 'backward-word)

;; Moving beg/end of line
(global-set-key (kbd "M-w") 'artem/beginning-of-line)
(global-set-key (kbd "M-e") 'artem/end-of-line)

;; Mark the whole buffer
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Scroll through the buffer and beg/end of the buffer
(global-set-key (kbd "M-s k") 'end-of-buffer)
(global-set-key (kbd "M-s i") 'beginning-of-buffer)
(global-set-key (kbd "s-k") 'scroll-up-command)
(global-set-key (kbd "s-i") 'scroll-down-command)

;; Copy & pasting
(global-set-key (kbd "M-s v") 'helm-show-kill-ring)
(global-set-key (kbd "M-s c") 'kill-region)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'kill-ring-save)

(global-set-key (kbd "M-s M-w") 'fixup-whitespace)

;; Evaluate sexp in the buffer
(global-set-key (kbd "M-r") 'eval-last-sexp)

;; Deleting chars, characters and lines
(global-set-key (kbd "H-l") 'delete-char)
(global-set-key (kbd "H-j") 'delete-backward-char)
(global-set-key (kbd "M-H-l") 'kill-word)
(global-set-key (kbd "M-H-j") 'backward-kill-word)
(global-set-key (kbd "M-s M-d") 'artem/kill-rest-of-line)

;; Evaluate expression in the buffer
(global-set-key (kbd "M-s r") 'eval-defun)

;; Change set mark command
(global-set-key (kbd "s-SPC") 'set-mark-command)

(global-set-key (kbd "M-q") 'kill-buffer-and-window)



(provide 'base-global-keys)
;;; base-global-keys.el ends here
