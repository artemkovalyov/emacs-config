;; -*- lexical-binding: t; -*-
;;; package ---  Add your custom functions
;; (defun something
;;    (do-something))
;;; Commentary:

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map (kbd "A-s p") 'exchange-point-and-mark-no-activate)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun artem/delete-line-and-move-up (&optional arg)
  "remove line and move one line up"
(interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (delete-backward-char 1))

(defun artem/delete-line-and-move-down (&optional arg)
  "remove line and move one line up"
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (delete-forward-char 1)
  )

(defun artem/delete-untill-end-of-the-line (&optional arg)
  "delete everything untill the end of line"
  (interactive "^p")
  (delete-region (point) (line-end-position)))

(defun artem/delete-untill-beginning-of-the-line (&optional arg)
  "delete everything untill the end of line"
  (interactive "^p")
  (delete-region (point) (line-beginning-position)))

(defun artem/beginning-of-line (&optional arg)
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

;;toggle hide/shoe block for hs-minor-mode
(defun hs-toggle ()
  (interactive)
  (let ((saved-point (point)))
    (end-of-line)
    (hs-toggle-hiding)
    (goto-char saved-point)))

 (defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun wrap-double-quote (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))

(defun wrap-back-quote (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "`"))

(defun wrap-singe-quote (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "'"))


(defun insert-timestamp-default ()
  "Insert the current timestamp"
  (interactive)
  (insert (current-time-string)))
(defalias 'its 'insert-timestamp-default)

(defun insert-timestamp-htmlcomment ()
  "Insert the current timestamp (HTML comment)"
  (interactive)
  (insert
   (concat
    "<!-- "
    (format-time-string "%Y-%m-%d %T ")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z"))
    " -->\n")))
(defalias 'itsh 'insert-timestamp-htmlcomment)

(defun insert-timestamp-unixtime ()
  "Insert the current Unix time"
  (interactive)
  (let ((time (current-time)))
    (let ((time1 (car time))
          (time2 (car (cdr time))))
      (insert (format "%d" (+ (* 65536 time1) time2))))))
(defalias 'itsu 'insert-timestamp-unixtime)

(defun insert-timestamp-iso ()
  "Insert the current timestamp (ISO 8601 format)"
  (interactive)
  (insert
   (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))
(defalias 'itsi 'insert-timestamp-iso)
(defalias 'itsiso 'insert-timestamp-iso)

(provide 'base-functions)
;;; .base-functions.el ends here
