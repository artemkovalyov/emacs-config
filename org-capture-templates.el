;;; package --- Org-mode capture templates

;;; Commentary:

;;; Code:

(setq org-capture-templates
      ;; A TODO task

      '((
	 "a" 				; key
	 "Add Task"			; description
	 entry				; type
	 (file+headline "~/life/org/backlog.org" "Backlog") ; target destination
         "* TODO %? \n :PROPERTIES: \n :ID: \n %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:" ; template
	 :prepend t 			; props - add latest to the beginning
	 :clock-in t			; props
	 :clock-resume t)		; props

	;; Add something new to my watch list
	(
	 "w"
	 "Watch"
	 entry
	 (file+headline "~/life/org/watch.org" "Inbox")
         "* WATCH %? \n :PROPERTIES: \n :LINK: \n :ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:"
	 :prepend t)
	(
	 "r"
	 "Read"
	 entry
	 (file+headline "~/life/org/read.org" "Inbox")
         "* READ %? \n :PROPERTIES: \n :LINK: \n :ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:"
	 :prepend t)
	(
	 "l"
	 "Listen"
	 entry
	 (file+headline "~/life/org/listen.org" "Inbox")
      "* LISTEN %? \n :PROPERTIES: \n :LINK: \n :ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:"
	 :prepend t)

	;; Collect ideas for my blog
	(
	 "b"
	 "Blog"
	 entry
	 (file+headline "~/life/org/blog.org" "Inbox")
         "* BLOG %? \n :PROPERTIES: \n :ID:      %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:"
	 :prepend t)

	;; Ideas about anything
	("i"
	 "Idea"
	 entry
	 (file+headline "~/life/org/ideas.org" "Inbox")
         "* IDEA %? \n :PROPERTIES: \n :ID:      %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:"
	 :prepend t)

	;; Ideas about anything
	("s"
	 "Software"
	 entry
	 (file+headline "~/life/org/blog.org" "Inbox")
         "* IDEA %? \n :PROPERTIES: \n :ID:      %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:"
	 :prepend t)

      ;; My dear diary that I never write
	("j"
	 "Journal"
	 entry
	 (file+datetree "~/life/org/journal.org")
         "* NOTE %? \n :PROPERTIES: \n :ID:      %(shell-command-to-string \"uuidgen\"):CREATED:  %U \n :END:"
	 :prepend t)))

(provide 'org-capture-templates)
;;; org-capture-templates.el end here
