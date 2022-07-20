;; -*- lexical-binding: t; -*-
;;; package --- my spellings config

;;; Commentary:

;;; Code:

(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))
(setq ispell-choices-win-default-height 3)

;; find aspell and hunspell automatically
(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 (
  (executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq ispell-choices-win-default-height 3)
  (setq ispell-really-hunspell t)

  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

;; To make it work you'll have to install aspell dictionaries, it doesn't work with other dictionaries while can use hunspell as a back-end
(use-package spell-fu
  :demand t
  :config
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "de"))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "us"))
  ;; (spell-fu-dictionary-add
  ;; (spell-fu-get-personal-dictionary "de-personal" "/home/user/.aspell.de.pws"))
  ;; (spell-fu-dictionary-add
  ;; (spell-fu-get-personal-dictionary "fr-personal" "/home/user/.aspell.fr.pws"))

  (setq spell-fu-ignore-modes (list 'org-mode))
  (setq global-spell-fu-ignore-buffer (lambda (buf) (buffer-local-value 'buffer-read-only buf)))
  (global-spell-fu-mode))

(use-package flyspell-correct
  :straight (flyspell-correct :type git :host github :repo "d12frosted/flyspell-correct")
  :after spell-fu
  :config
  (ispell-set-spellchecker-params)  ;; Initializes important ispell parameters (like 'ispell-dictionary-alist)
  :bind
  ("A-;" . flyspell-auto-correct-word)
  ("A-s ;" . flyspell-correct-at-point))

(defun ispell-word-immediate ()
  "Run `ispell-word', using the first suggestion."
  (interactive)
  (cl-letf
      (((symbol-function 'ispell-command-loop)
        (lambda (miss _guess _word _start _end) (car miss))))
    (ispell-word)))


(provide 'spelling)
;;; spelling ends here
