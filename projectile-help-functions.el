;; -*- lexical-binding: t; -*-
;;; package --- projectile-help-functions

;;; Commentary:

;;; Code:
(require 'projectile)

(defun artem/kill-non-project-buffers (&optional kill-special)
  "Kill buffers that do not belong to a `projectile' project.

With prefix argument (`C-u'), also kill the special buffers."
  (interactive "P")
  (let ((bufs (buffer-list (selected-frame))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (let ((buf-name (buffer-name buf)))
          (when (or (null (projectile-project-p))
                    (and kill-special
                         (string-match "^\*" buf-name)))
            ;; Preserve buffers with names starting with *scratch or *Messages
            (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
              (message "Killing buffer %s" buf-name)
              (kill-buffer buf))))))))

(provide 'projectile-help-functions)
;;; projectile-help-functions.el ends here
