;;; package ---  Add your custom functions
;; (defun something
;;    (do-something))
;;; Commentary:

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

(defun artem/kill-rest-of-line ()
  "delete everything till the end of line"
  (interactive)
  (delete-region (point) (line-end-position)))

(defun artem/beginning-of-line (arg)
  "Move point back to beginning of visual line, then indentation, then beginning of logical line.

Move point to the beginning of visual line, then to the first non-whitespace character on the logical line, then to the beginning of logical line.
After beginning of logical line is reached toggle between the first non-whitespace character and the beginning of the logical line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual t))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (vertical-motion 0)
    (when (= orig-point (point))
      (back-to-indentation))))

(defun artem/end-of-line (arg)
  "Move point to the end of visual line, then move point to the end of logical line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (end-of-visual-line)
    (when (= orig-point (point))
      (end-of-line))))

(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(provide 'base-functions)
;;; .base-functions.el ends here
