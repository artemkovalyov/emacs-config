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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (beginning-of-line))))

(provide 'base-functions)
;;; .base-functions.el ends here
