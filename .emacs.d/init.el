;; -*- lexical-binding: t; -*-

(setq straight-use-package-by-default t
      use-package-always-defer t
      straight-check-for-modifications nil
      straight-cache-autoloads t
      file-name-handler-alist nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq user-full-name "Benjamín Buccianti"
      user-mail-address "benjamin@buccianti.dev"
      message-log-max 16384
      show-trailing-whitespace t
      cursor-in-non-selected-windows nil
      large-file-warning-threshold 100000000
      blink-matching-paren nil
      transient-mark-mode nil
      font-lock-maximum-decoration 2
      tooltip-use-echo-area t
      use-dialog-box nil
      visible-cursor nil
      make-backup-files nil
      browse-url-generic-program "/usr/local/bin/luakit"
      browse-url-browser-function 'browse-url-generic
      auto-window-vscroll nil
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-default nil
      vc-follow-symlinks t
      epg-gpg-program "gpg2"
      explicit-shell-file-name "/bin/mksh")

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
							before-init-time)))
                     gcs-done)
            (setq file-name-handler-alist old--file-name-handler-alist
                  gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)
            (garbage-collect))
          t)

;; functions
(defun yank-from-kill-ring ()
  "Uses selectrum-read to insert a string from the kill-ring."
  (interactive)
  (let* ((candidates (delete-dups kill-ring))
	 (candidates (seq-remove (lambda (s)
				   (string-empty-p (string-trim s)))
				 candidates))
	 (string (selectrum-read "Kill-ring: " candidates)))
    (when (and string (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (insert string)))

(defun vterm-switch-buffer-or-run ()
  "If a vterm buffer is opened, switch to him. Otherwise, run vterm."
  (interactive)
  (or (and (get-buffer "vterm") (switch-to-buffer "vterm"))
      (vterm)))

;; packages
;; (setq use-package-verbose t) ;; debug only
(straight-use-package 'use-package)

(use-package personal-keybindings
  :straight nil
  :init
  (global-unset-key "\C-z")
  (define-prefix-command 'ctl-z-map)
  (set-fringe-mode 1)
  :hook
  (prog-mode . prettify-symbols-mode)
  (prog-mode . global-hl-line-mode)
  :bind
  (:map global-map
	("C-z" . ctl-z-map)
	("M-SPC" . cycle-spacing)
	("M-o" . other-window)
	("M-/" . hippie-expand)
	("C-w" . backward-kill-word)
	("S-<left>" . windmove-left)
	("S-<up>" . windmove-up)
	("S-<down>" . windmove-down)
	("S-<right>" . windmove-right))
  (:map ctl-z-map
	("r" . compile)
	("C-r" . recompile)
	("y" . yank-from-kill-ring))
  (:map ctl-x-map
	("C-k" . kill-region)
	("C-b" . ibuffer)))

(use-package css-mode
  :straight nil
  :hook (css-mode . electric-pair-mode))

(use-package isearch
  :straight nil
  :commands (isearch-forward isearch-backward)
  :config
  (setq isearch-allow-scroll t
	search-whitespace-regexp ".*"))

(use-package eshell
  :straight nil
  :bind (:map ctl-z-map ("t" . eshell)))

(use-package exec-path-from-shell
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(use-package project
  :straight nil
  :commands (project-find-file project-find-regexp)
  :bind (:map ctl-z-map
	      ("p" . project-find-file)
	      ("/" . project-find-regexp))
  :config
  (dolist (folder '("node_modules" "target" "out"
		    ".cljs_node_repl" ".shadow-cljs"))
    (add-to-list 'vc-directory-exclusion-list folder)))

(use-package vterm
  :commands vterm
  :bind (:map ctl-z-map ("C-t" . vterm-switch-buffer-or-run)))

(use-package material-theme
  :init (load-theme 'material t))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :init (selectrum-mode +1))

(use-package prescient
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000
	prescient-filter-method '(literal regexp fuzzy)))

(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
		   :files ("selectrum-prescient.el"))
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (setq selectrum-current-candidate '((t (:background "#222226"
					  :weight bold
					  :foreground "gainsboro")))
	selectrum-primary-highlight '((t (:foreground "#c56ec3")))
	selectrum-secondary-highlight '((t (:foreground "#2d9574")))))

(use-package amx
  :commands amx
  :bind (:map global-map ("M-x" . amx))
  :config
  (setq amx-show-key-bindings t))

(use-package dired
  :straight nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'top
	dired-use-ls-dired nil
	dired-dwim-target t
	dired-listing-switches "-lha1v --group-directories-first")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package notmuch
  :commands notmuch
  :bind (:map ctl-z-map ("@" . notmuch))
  :config
  (setq fill-column 72
	mail-user-agent 'message-user-agent
	smtpmail-default-smtp-server "mail.buccianti.dev"
	smtpmail-smtp-server "mail.buccianti.dev"
	smtpmail-local-domain "buccianti.dev"
	message-send-mail-function 'message-smtpmail-send-it
	smtpmail-debug-info nil
	message-default-mail-headers "Cc: \nBcc: \n"
	message-auto-save-directory "~/.mail/drafts"
	message-kill-buffer-on-exit t
	message-directory "~/.mail/sent"
	notmuch-search-oldest-first nil
	notmuch-show-indent-content nil
	notmuch-show-logo nil
	notmuch-show-all-tags-lst t
	notmuch-hello-sections '(notmuch-hello-insert-header
				 notmuch-hello-insert-inbox
				 notmuch-hello-insert-footer)
	notmuch-fcc-dirs
	'(("benjamin@buccianti.dev" . "benjamin/Sent -inbox -unread +sent"))))

(use-package text-mode
  :straight nil
  :hook (text-mode . turn-off-auto-fill))

(use-package whitespace
  :straight nil
  :hook (prog-mode . whitespace-mode)
  :bind (:map ctl-z-map ("C-." . whitespace-cleanup))
  :config
  (setq whitespace-line-column 80
	whitespace-style '(face lines-tail trailing space-before-tab)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :bind (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (clojure-mode . enable-paredit-mode))

(use-package imenu
  :straight nil
  :bind (:map ctl-z-map ("i" . imenu))
  :config (setq imenu-auto-rescan t))

(use-package expand-region
  :commands er/expand-region
  :bind (:map global-map ("C-=" . er/expand-region)))

(use-package elfeed
  :commands elfeed
  :bind (:map ctl-z-map ("e" . elfeed))
  :config
  (setq elfeed-feeds '("http://planet.clojure.in/atom.xml"
		       "https://planet.emacslife.com/atom.xml"
		       "http://planet.lisp.org/rss20.xml"
		       "https://twobithistory.org/feed.xml")))

(use-package clojure-mode
  :mode (("\\.clj\\[s\\*\\'" . clojure-mode))
  :config
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'fn-traced 1))

(use-package monroe
  :commands monroe
  :hook
  (clojure-mode . clojure-enable-monroe)
  (monroe-mode . enable-paredit-mode)
  :bind (:map ctl-z-map ("m" . monroe))
  :config
  (setq monroe-detail-stacktraces t))

(use-package rust-mode
  :mode (("\\.rs\\'" . rust-mode))
  :hook (rust-mode . electric-pair-mode)
  :config (setq rust-format-on-save t
		rust-format-show-buffer nil
		rust-rustfmt-bin "/home/bbuccianti/.cargo/bin/rustfmt"
		rust-cargo-bin "/home/bbuccianti/.cargo/bin/cargo"))

(use-package fennel-mode
  :mode (("\\.fnl\\'" . fennel-mode)))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)))

(use-package ansi-color
  :straight nil
  :hook
  (compilation-filter
   . (lambda ()
       (toggle-read-only)
       (ansi-color-apply-on-region compilation-filter-start (point))
       (toggle-read-only))))

(use-package magit
  :commands (magit-status magit-list-repositories)
  :bind
  (:map ctl-x-map ("g" . magit-status))
  :config
  (setq magit-repository-directories '(("~/work" . 2) ("~/src" . 3))))

(use-package neuron-mode
  :straight (:host github :repo "bbuccianti/neuron-mode")
  :commands (neuron-edit-zettel neuron-new-zettel)
  :bind (:map ctl-z-map
	      ("z e" . neuron-edit-zettel)
	      ("z n" . neuron-new-zettel))
  :config
  (setq neuron-default-zettelkasten-directory "/home/bbuccianti/notes"
	neuron-executable "/home/bbuccianti/bin/neuron-linux-bundle"))

(use-package org
  :straight nil
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
	org-default-notes-file "/home/bbuccianti/org/notes.org"
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
				   entry (file+headline "~/org/projects.org" "Inbox")
				   "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
				  ("n" "Note"
				   entry (file "~/org/notes.org")
				   "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:"))
	org-todo-keywords '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|"
					"DONE(d)" "CANCELLED(c)"))
	org-todo-keyword-faces '(("TODO" :foreground "gold" :weight bold)
				   ("NEXT" :foreground "deep sky blue" :weight bold)
				   ("DONE" :foreground "forest green" :weight bold)
				   ("CANCELLED" :foreground "red" :weight bold))

	org-directory "/home/bbuccianti/notes/"))

(use-package org-ql
  :after org
  :commands org-ql-block)

(use-package org-agenda
  :straight nil
  :after org
  :config
  (setq org-agenda-files '("/home/bbuccianti/org/")
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-agenda-skip-scheduled-if-done t
	org-agenda-skip-unavailable-files t
	org-agenda-window-setup 'current-window
	org-agenda-start-on-weekday nil
	org-agenda-compact-blocks nil
	org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
				   (todo . "%l" )
				   (tags . "%l")
				   (search . " %i %-12:c"))
	org-agenda-custom-commands
	'(("d" "Today tasks"
	   ((agenda "" ((org-agenda-span 'day)))
	    (org-ql-block '(and (todo "TODO")
				(priority "A")
				(scheduled :on today))
			  ((org-ql-block-header "Today's priority A tasks")))
	    (org-ql-block '(and (todo "TODO")
				(priority "B")
				(scheduled :on today))
			  ((org-ql-block-header "Today's priority B tasks")))
	    (org-ql-block '(and (todo "TODO")
				(priority "C")
				(scheduled :on today))
			  ((org-ql-block-header "Today's priority C tasks")))
	    (org-ql-block '(and (todo "PROJECT" "NEXT")
				(or (descendants (todo "NEXT"))
				    (todo "NEXT")))
			  ((org-ql-block-header "Next tasks")))
	    (org-ql-block '(and (todo "PROJECT" "TODO")
				(tags "research" "reading"))
			  ((org-ql-block-header "Study time!")))))
	  ("w" "Week planning"
	   ((agenda "" ((org-agenda-span 'week)))
	    (org-ql-block '(and (todo)
				(tags "inbox"))
			  ((org-ql-block-header "Inbox")))
	    (org-ql-block '(and (todo "PROJECT" "TODO")
				(not (tags "notes"))
				(not (tags "inbox"))
				(not (ancestors (children (todo "NEXT"))))
				(not (children (todo "NEXT")))
				(not (or (scheduled) (deadline))))
			  ((org-ql-block-header "Stuck projects"))))))))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))

(use-package sdcv
  :commands sdcv-search-input
  :bind
  (:map sdcv-mode-map
	("n" . sdcv-next-line)
	("p" . sdcv-prev-line)
	("N" . sdcv-next-dictionary)
	("P" . sdcv-previous-dictionary))
  (:map ctl-z-map ("d" . sdcv-search-input)))
