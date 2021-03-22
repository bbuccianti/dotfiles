(disable-theme 'modus-operandi)

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :bind (:map ctl-z-map
	      ("a" . org-agenda)
	      ("c" . org-capture))
  :config
  (setq org-startup-indented t
	org-startup-truncated nil
	org-modules '(ol-info ol-mhe ol-rmail)
	org-hide-leading-stars t
	org-latex-toc-command "\\tableofcontents \\clearpage"
	org-export-async-init-file "~/.emacs.d/org-init.el"
	org-src-preserve-indentation t
	org-default-notes-file "~/org/notes.org"
	org-refile-use-outline-path t
	org-outline-path-complete-in-steps nil
	org-completion-use-ido nil
	org-log-done "note"
	org-tags-column -60
	org-fast-tag-selection-single-key 'expert
	org-blank-before-new-entry '((heading . auto) (plain-list-item . auto))
	org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9))
	org-capture-templates '(("t" "Todo"
				 entry (file+headline "~/org/projects.org"
						      "Inbox")
				 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
				("n" "Note"
				 entry (file "~/org/notes.org")
				 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:"))
	org-todo-keywords '((sequence "PROJECT(p)" "TODO(t)" "|"
				      "DONE(d)" "CANCELLED(c)"))
	org-directory "~/notes/"))

(use-package org-agenda
  :config
  (setq org-agenda-files '("~/org/")
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-unavailable-files t
	org-agenda-window-setup 'current-window
	org-agenda-start-on-weekday nil
	org-agenda-compact-blocks nil
	org-agenda-tags-column -90
	org-agenda-prefix-format '((agenda . " %i %-10:c%?-10t% s")
				   (todo . "%l" )
				   (tags . "%l")
				   (search . " %i %-10:c"))))

(use-package notdeft
  :straight (:host github :repo "hasu/notdeft" :files ("*.el" "xapian"))
  :config (setq notdeft-directories '("~/notes")
		notdeft-extension "org"))

(org-agenda nil "n")
