;;; package ---  Add your custom functions
;; (defun something
;;    (do-something))
;;; Commentary:

;;; Code:
(defun kill-line-no-copy ()
  "Kill linke without putting it into 'kill-ring'."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(defun lint-js-buffer-with-standard-js ()
  "Use shell command to format buffer or region via eslint."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "standard --stdin --fix" buffer-file-name t "error"))

(provide 'base-functions)
;;; .base-functions.el ends here.
