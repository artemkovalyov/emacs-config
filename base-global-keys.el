;; -*- lexical-binding: t; -*-

;;; package --- Add your keys here, as such

;;; Commentary:

;;; Code:

;;; Free keys

;;; C-SPC
;;; C-RET
;;; M-RET

;; makes M-C-i work later on
;; (unbind-key "C-M-i")
;; (unbind-key "C-M-i" emacs-lisp-mode-map)
;; (unbind-key "C-M-i" lisp-interaction-mode-map)
;; (unbind-key "C-M-i" emacs-lisp-mode-map)
;; (unbind-key "C-M-i" text-mode-map)

;; remove all the Alt binding from `key-translation-map'
(cl-delete-if #'(lambda (el)
                  (and
                   (listp el)
                   (string-match-p
                    (rx bol "A-")
                    (key-description (vector (car el))))))
              key-translation-map)


;; (global-set-key (kbd "M-s RET") 'delete-other-windows)

;; Store and jump to registers
(global-set-key (kbd "s-g r") 'point-to-register)
(global-set-key (kbd "s-g j") 'jump-to-register)

;; Change window size
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)

;; Moving across text
(global-set-key (kbd "A-k") 'next-line)
(global-set-key (kbd "A-i") 'previous-line)
(global-set-key (kbd "A-j") 'backward-char)
(global-set-key (kbd "A-l") 'forward-char)

;; Moving over words right and left
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-j") 'backward-word)

;; Scroll up/down
(global-set-key (kbd "M-k") 'scroll-up-command)
(global-set-key (kbd "M-i") 'scroll-down-command)

;; Moving beg/end of line
(global-set-key (kbd "A-w") 'artem/beginning-of-line)
(global-set-key (kbd "A-e") 'artem/end-of-line)

;; Mark the whole buffer
(global-set-key (kbd "A-a") 'mark-whole-buffer)

;; Scroll beg/end of buffer
(global-set-key (kbd "A-s k") 'end-of-buffer)
(global-set-key (kbd "A-s i") 'beginning-of-buffer)

;; Copy & pasting
(global-set-key (kbd "A-c") 'kill-ring-save)
(global-set-key (kbd "A-v") 'yank)
(global-set-key (kbd "M-c") 'kill-region)
(global-set-key (kbd "M-v") 'helm-show-kill-ring)


;; Deleting chars, characters and lines
(global-set-key (kbd "A-H-l") 'delete-char)
(global-set-key (kbd "A-H-j") 'delete-backward-char)
(global-set-key (kbd "M-H-l") 'kill-word)
(global-set-key (kbd "M-H-j") 'backward-kill-word)
(global-set-key (kbd "A-H-k") 'kill-whole-line)
(global-set-key (kbd "A-H-i") 'artem/kill-line-up)
(global-set-key (kbd "M-w") 'artem/kill-line-backwards)
(global-set-key (kbd "M-e") 'kill-line)
(global-set-key (kbd "M-u") 'delete-blank-lines)
(global-set-key (kbd "H-u") 'delete-indentation)
(global-set-key (kbd "A-u") 'fixup-whitespace)


;; Eval last sexp / eval defun
(global-set-key (kbd "A-r") 'eval-last-sexp)

;; Change set mark command
(global-set-key (kbd "A-h") 'set-mark-command)

(global-set-key (kbd "A-q") 'kill-current-buffer)

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-a") 'save-some-buffers)

(global-set-key [escape] 'keyboard-escape-quit)



(provide 'base-global-keys)
;;; base-global-keys.el ends here
