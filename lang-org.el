;;; package --- Org-mode config

;;; Commentary:

;;; Code:

(setq org-export-backends nil)

(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org")
	org-archive-location "/home/artem/life/archive/archive.org::* From %s"
	org-clock-persist 'history)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

;; Org Mode redefines
(define-key org-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key org-mode-map (kbd "M-h") 'mark-whole-buffer)

;; My agenda files
(setq org-agenda-files (quote ("/home/artem/life/org/")))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
 (setq org-refile-targets (quote ((nil :maxlevel . 5)
				  (org-agenda-files :maxlevel . 5))))
;; org-refiling options
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-enforce-todo-dependencies t)
;; (setq org-completion-use-ido nil)

;; org export options
;; (use-package ox-gfm
;;   ;; :demand t
;;   ;; :commands ox-gfm-export-to-markdown
;;   )

(use-package ox-pandoc
  :config
  (setq org-pandoc-menu-entry
	'((52 "to html5 and open." org-pandoc-export-to-html5-and-open)
	  (36 "as html5." org-pandoc-export-as-html5)
	  (53 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
	  (37 "to html5-pdf." org-pandoc-export-to-html5-pdf)
	  ;; (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
	  ;; (?G "as gfm." org-pandoc-export-as-gfm)
	  (60 "to slideous and open." org-pandoc-export-to-slideous-and-open)
	  (44 "as slideous." org-pandoc-export-as-slideous)
	  (61 "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
	  (45 "to ms-pdf." org-pandoc-export-to-ms-pdf)
	  (98 "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
	  (66 "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
	  (99 "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
	  (67 "to context-pdf." org-pandoc-export-to-context-pdf)
	  (100 "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
	  (68 "as docbook5." org-pandoc-export-as-docbook5)
	  (101 "to epub3 and open." org-pandoc-export-to-epub3-and-open)
	  (69 "to epub3." org-pandoc-export-to-epub3)
	  (103 "to gfm and open." org-pandoc-export-to-gfm-and-open)
	  (71 "as gfm." org-pandoc-export-as-gfm)
	  (105 "to icml and open." org-pandoc-export-to-icml-and-open)
	  (73 "as icml." org-pandoc-export-as-icml)
	  (106 "to json and open." org-pandoc-export-to-json-and-open)
	  (74 "as json." org-pandoc-export-as-json)
	  (108 "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
	  (76 "to latex-pdf." org-pandoc-export-to-latex-pdf)
	  (109 "to man and open." org-pandoc-export-to-man-and-open)
	  (77 "as man." org-pandoc-export-as-man)
	  (110 "to native and open." org-pandoc-export-to-native-and-open)
	  (78 "as native." org-pandoc-export-as-native)
	  (111 "to odt and open." org-pandoc-export-to-odt-and-open)
	  (79 "to odt." org-pandoc-export-to-odt)
	  (112 "to pptx and open." org-pandoc-export-to-pptx-and-open)
	  (80 "to pptx." org-pandoc-export-to-pptx)
	  (114 "to rtf and open." org-pandoc-export-to-rtf-and-open)
	  (82 "as rtf." org-pandoc-export-as-rtf)
	  (117 "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
	  (85 "as dokuwiki." org-pandoc-export-as-dokuwiki)
	  (118 "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
	  (86 "as revealjs." org-pandoc-export-as-revealjs)
	  (119 "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
	  (87 "as mediawiki." org-pandoc-export-as-mediawiki)
	  (120 "to docx and open." org-pandoc-export-to-docx-and-open)
	  (88 "to docx." org-pandoc-export-to-docx)
	  (121 "to slidy and open." org-pandoc-export-to-slidy-and-open)
	  (89 "as slidy." org-pandoc-export-as-slidy)
	  (122 "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
	  (90 "as dzslides." org-pandoc-export-as-dzslides)))
  )


(setq org-todo-keywords
      '(
	(sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "DEFERRED(d)" "PROJECT(p)" "RECUR(r)" "|" "DELEGATED" "SOMEDAY" "DONE" "CANCELED(c)")
	(sequence "NOTE(n)" "IDEA(i)" "BLOG(b)" "OUTDATED(o)")
	(sequence "WATCH(w)" "LISTEN(n)" "READ(r)" "ARCHIVED(a)")
	(sequence "EPIC(e)" "FEATURE(f)" "STORY(s)" "FUNCTIONAL" "NON-FUNCTIONAL" "|" "DELIVERED")))


(setq org-todo-keyword-faces
   (quote
    (
     ("TODO" :foreground "gold" :weight bold)
     ("IN-PROGRESS" :foreground "orangered" :weight bold)
     ("EPIC" :foreground "deep sky blue" :weight bold)
     ("STORY" :foreground "royal blue" :weight bold)
     ("RECUR" :foreground "cornflowerblue" :weight bold)
     ("NOTE" :foreground "brown" :weight bold)
     ("WAITING" :foreground "red" :weight bold)
     ("DELEGATED" :foreground "dark violet" :weight bold)
     ("DEFERRED" :foreground "khaki" :weight bold)
     ("SOMEDAY" :foreground "coral" :weight bold)
     ("PROJECT" :foreground "#088e8e" :weight bold)
     ("WATCH" :foreground "gold" :weight bold)
     ("LISTEN" :foreground "deep sky blue" :weight bold)
     ("READ" :foreground "coral" :weight bold)
     ("ARCHIVED" :foreground "violet" :weight bold)
     ("BLOG" :foreground "coral" :weight bold)
     ("IDEA" :foreground "gold" :weight bold)
     ("OUTDATED" :foreground "violet" :weight bold)
     )))

(setq org-log-done 'time)
(setq org-catch-invisible-edits t)
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-log-done 'time)


(defun wicked/org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and update
them with the current numbers.  With optional prefix argument ALL,
do this for the whole buffer."
  (interactive "P")
  (save-excursion
    (let* ((beg (condition-case nil
		    (progn (outline-back-to-heading) (point))
		  (error (point-min))))
	   (end (move-marker
		 (make-marker)
		 (progn (or (outline-get-next-sibling) ;; (1)
			    (goto-char (point-max)))
			(point))))
	   (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
	   (re-box
	    "^[ \t]*\\(*+\\|[-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	   b1 e1 f1 c-on c-off lim (cstat 0))
      (when all
	(goto-char (point-min))
	(or (outline-get-next-sibling) (goto-char (point-max))) ;; (2)
	(setq beg (point) end (point-max)))
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq cstat (1+ cstat)
	      b1 (match-beginning 0)
	      e1 (match-end 0)
	      f1 (match-beginning 1)
	      lim (cond
		   ((org-on-heading-p)
		    (or (outline-get-next-sibling) ;; (3)
			(goto-char (point-max)))
		    (point))
		   ((org-at-item-p) (org-end-of-item) (point))
		   (t nil))
	      c-on 0 c-off 0)
	(goto-char e1)
	(when lim
	  (while (re-search-forward re-box lim t)
	    (if (member (match-string 2) '("[ ]" "[-]"))
		(setq c-off (1+ c-off))
	      (setq c-on (1+ c-on))))
	  (goto-char b1)
	  (insert (if f1
		      (format "[%d%%]" (/ (* 100 c-on)
					  (max 1 (+ c-on c-off))))
		    (format "[%d/%d]" c-on (+ c-on c-off))))
	  (and (looking-at "\\[.*?\\]")
	       (replace-match ""))))
      (when (interactive-p)
	(message "Checkbox statistics updated %s (%d places)"
		 (if all "in entire file" "in current outline entry")
		 cstat)))))
(defadvice org-update-checkbox-count (around wicked activate)
  "Fix the built-in checkbox count to understand headlines."
  (setq ad-return-value
	(wicked/org-update-checkbox-count (ad-get-arg 1))))

(global-set-key [f12] 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(require 'org-capture-templates)

(provide 'lang-org)
;;; lang-org.el ends here
