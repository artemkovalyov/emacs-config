;;; package ---  Add your custom functions
;; (defun something
;;    (do-something))
;;; Commentary:

;;; Code:
(defun lint-js-buffer-with-standard-js ()
  "Use shell command to format buffer or region via eslint."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "standard --stdin --fix" buffer-file-name t "error"))

(defun artem/kill-line-up ()
  "remove line and move one line up"
  (interactive)
  (kill-whole-line)
  (backward-char))

(defun artem/kill-line-down ()
  "remove line and move one line up"
  (interactive)
  (kill-whole-line)
  (forward-char))

(provide 'base-functions)
;;; .base-functions.el ends here
