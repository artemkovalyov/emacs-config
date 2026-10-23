;; -*- lexical-binding: t; -*-

;;; package --- Add your keys here, as such

;;; Commentary:

;;; Code:

;;; Free keys

;;; C-SPC
;;; C-RET
;;; M-RET

;; Remove all Alt bindings from `key-translation-map'
(cl-delete-if #'(lambda (el)
                  (and
                   (listp el)
                   (string-match-p
                    (rx bol "A-")
                    (key-description (vector (car el))))))
              key-translation-map)

;; Store and jump to registers
(global-set-key (kbd "s-g r") 'point-to-register)
(global-set-key (kbd "s-g j") 'jump-to-register)

;; Timestamps
(global-set-key (kbd "s-g t") 'insert-timestamp-default)
(global-set-key (kbd "s-g i") 'insert-timestamp-iso)

;; Change window size
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)

;; Moving across text — A- for char and line movement
(global-set-key (kbd "A-k") 'next-line)
(global-set-key (kbd "A-i") 'previous-line)
(global-set-key (kbd "A-j") 'backward-char)
(global-set-key (kbd "A-l") 'forward-char)

;; Cyrillic equivalents
(global-set-key (kbd "A-л") 'next-line)
(global-set-key (kbd "A-ш") 'previous-line)
(global-set-key (kbd "A-о") 'backward-char)
(global-set-key (kbd "A-д") 'forward-char)

;; Moving over words
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-д") 'forward-word)
(global-set-key (kbd "M-о") 'backward-word)

;; Scroll up/down
(global-set-key (kbd "M-k") 'scroll-up-command)
(global-set-key (kbd "M-i") 'scroll-down-command)
(global-set-key (kbd "M-л") 'scroll-up-command)
(global-set-key (kbd "M-ш") 'scroll-down-command)

;; Moving beg/end of line
(global-set-key (kbd "A-w") 'artem/beginning-of-line)
(global-set-key (kbd "A-e") 'artem/end-of-line)
(global-set-key (kbd "A-ц") 'artem/beginning-of-line)
(global-set-key (kbd "A-у") 'artem/end-of-line)

;; Set mark
(global-set-key (kbd "A-h") 'set-mark-command)

;; Mark the whole buffer
(global-set-key (kbd "A-a") 'mark-whole-buffer)
(global-set-key (kbd "A-ф") 'mark-whole-buffer)

;; Beg/end of buffer
(global-set-key (kbd "A-s k") 'end-of-buffer)
(global-set-key (kbd "A-s i") 'beginning-of-buffer)

;; Move by sentence
(global-set-key (kbd "A-s l") 'forward-sentence)
(global-set-key (kbd "A-s j") 'backward-sentence)

;; Copy & pasting
(global-set-key (kbd "A-c") 'kill-ring-save)
(global-set-key (kbd "A-v") 'yank)
(global-set-key (kbd "M-c") 'kill-region)
(global-set-key (kbd "M-v") 'consult-yank-from-kill-ring)

;; Whitespace
(global-set-key (kbd "A-s A-u") 'delete-blank-lines)
(global-set-key (kbd "A-s u") 'delete-indentation)
(global-set-key (kbd "A-u") 'cycle-spacing)
(global-set-key (kbd "A-і A-г") 'delete-blank-lines)
(global-set-key (kbd "A-і г") 'delete-indentation)
(global-set-key (kbd "A-г") 'cycle-spacing)

;; Deleting chars and lines
(global-set-key (kbd "A-H-l") 'delete-char)
(global-set-key (kbd "A-H-j") 'delete-backward-char)
(global-set-key (kbd "A-H-k") 'artem/delete-line-and-move-down)
(global-set-key (kbd "A-H-i") 'artem/delete-line-and-move-up)
(global-set-key (kbd "A-H-д") 'delete-char)
(global-set-key (kbd "A-H-о") 'delete-backward-char)
(global-set-key (kbd "A-H-л") 'artem/delete-line-and-move-down)
(global-set-key (kbd "A-H-ш") 'artem/delete-line-and-move-up)

(global-set-key (kbd "M-w") 'artem/delete-until-beginning-of-the-line)
(global-set-key (kbd "M-e") 'artem/delete-until-end-of-the-line)
(global-set-key (kbd "M-ц") 'artem/delete-until-beginning-of-the-line)
(global-set-key (kbd "M-у") 'artem/delete-until-end-of-the-line)

;; Eval last sexp
(global-set-key (kbd "A-r") 'eval-last-sexp)

;; Execute command
(global-set-key (kbd "A-x") 'execute-extended-command)

;; Kill buffer
(global-set-key (kbd "A-q") 'kill-current-buffer)

;; Save
(global-set-key (kbd "A-s s") 'save-buffer)
(global-set-key (kbd "A-s A-s") 'save-some-buffers)
(global-set-key (kbd "A-і і") 'save-buffer)
(global-set-key (kbd "A-і A-i") 'save-some-buffers)

;; Files and projects
(global-set-key (kbd "A-o") 'project-find-file)
(global-set-key (kbd "A-p") 'project-switch-project)
(global-set-key (kbd "A-s o") 'find-file)
(global-set-key (kbd "A-s d") 'dired)

;; Wrapping
(global-set-key (kbd "A-s '") 'wrap-singe-quote)
(global-set-key (kbd "A-s \"") 'wrap-double-quote)
(global-set-key (kbd "A-s `") 'wrap-back-quote)
(global-set-key (kbd "A-s [") 'sp-wrap-square)
(global-set-key (kbd "A-s {") 'sp-wrap-curly)
(global-set-key (kbd "A-s (") 'sp-wrap-round)
(global-set-key (kbd "A-s w") 'emmet-wrap-with-markup)

;; Misc
(global-set-key (kbd "A-s h") 'rectangle-mark-mode)
(global-set-key (kbd "A-s A-p") 'exchange-point-and-mark)
(global-set-key (kbd "A-d") 'artem/duplicate-line-or-region)

(global-set-key [escape] 'keyboard-quit)

;; Font sizes
(global-set-key (kbd "A-=")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (+ old-face-attribute 5)))))

(global-set-key (kbd "A--")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (- old-face-attribute 5)))))

(global-set-key (kbd "A-0")
                (lambda ()
                  (interactive)
                  (set-face-attribute 'default nil :height default-global-text-scale)))

(global-set-key (kbd "H-0")
                (lambda ()
                  (interactive)
                  (set-face-attribute 'default nil :height 200)))

(provide 'base-global-keys)
;;; base-global-keys.el ends here
