;; -*- lexical-binding: t -*

(let ((file-name-handler-alist nil)
      (gc-cons-threshold 402653184)
      (gc-cons-percentage 0.6))

  (setq straight-use-package-by-default t)
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
	user-mail-address "benjamin@buccianti.dev")

  (setq bidi-display-reordering nil
	show-trailing-whitespace t
	large-file-warning-threshold 100000000
	auto-window-vscroll nil
	blink-matching-paren nil
	cursor-in-non-selected-windows nil
	inhibit-startup-screen t
	inhibit-default-init t
	inhibit-startup-message t
	initial-scratch-message nil
	default-frame-alist '((font . "Hack-10"))
	backup-directory-alist '(("." . "~/.emacs.d/backups"))
	create-lockfiles nil
	auto-save-default nil
	transient-mark-mode nil
	font-lock-maximum-decoration 2
	tooltip-use-echo-area t
	use-dialog-box nil
	visible-cursor nil
	epg-gpg-program "gpg2"
	inferior-lisp-program "/usr/bin/sbcl"
	explicit-shell-file-name "/bin/sh")

  (add-to-list 'vc-directory-exclusion-list "node_modules")
  (add-to-list 'vc-directory-exclusion-list "target")
  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  (setq mail-user-agent 'message-user-agent
	smtpmail-default-smtp-server "mail.buccianti.dev"
	smtpmail-smtp-server "mail.buccianti.dev"
	smtpmail-local-domain "buccianti.dev"
	message-send-mail-function 'message-smtpmail-send-it
	smtpmail-debug-info t
	message-default-mail-headers "Cc: \nBcc: \n"
	message-auto-save-directory "~/.mail/drafts"
	message-kill-buffer-on-exit t
	message-directory "~/.mail/sent")

  ;; packages

  (straight-use-package 'use-package)

  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize))

  (use-package monokai-theme
    :defer t
    :init (load-theme 'monokai))

  (use-package battery
    :defer t
    :config (display-battery-mode))

  (use-package ido
    :straight nil
    :init
    (setq ido-enable-flex-matching t
	  ido-enable-prefix nil
	  ido-auto-merge-work-directories-length -1
	  ido-create-new-buffer 'always
	  ido-use-filename-at-point nil
	  ido-everywhere t)
    (ido-mode 1))

  (use-package ido-completing-read+
    :init
    (ido-ubiquitous-mode 1))

  (use-package ido-vertical-mode
    :init (ido-vertical-mode 1)
    :config (setq ido-vertical-define-keys 'C-n-and-C-p-only))

  (use-package amx
    :commands amx)

  (use-package dired
    :straight nil
    :hook (dired-mode . dired-hide-details-mode)
    :config
    (setq dired-recursive-copies 'always
	  dired-recursive-deletes 'top
	  dired-use-ls-dired nil
	  dired-listing-switches "-lha1v --group-directories-f"))

  (use-package mu4e
    :straight nil
    :load-path "/usr/share/emacs/site-lisp/mu4e"
    :commands mu4e
    :config
    (setq mu4e-maildir "~/.mail"
	  mu4e-get-mail-command "mbsync -a"
	  mu4e-view-show-addresses t
	  fill-column 72))

  (use-package whitespace
    :straight nil
    :hook (prog-mode . whitespace-mode)
    :config
    (setq whitespace-line-column 80
	  whitespace-style '(face lines-tail trailing space-before-tab)))

  (use-package rainbow-delimiters
    :defer t
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package paredit
    :defer t
    :hook
    (emacs-lisp-mode . paredit-mode)
    (lisp-mode . paredit-mode)
    (lisp-interaction-mode . paredit-mode)
    (slime-mode . paredit-mode)
    (clojure-mode . paredit-mode))

  (use-package move-text
    :defer t
    :commands (move-text-up move-text-down))

  (use-package company
    :commands company-complete
    :hook (prog-mode . company-mode)
    :config (setq company-idle-delay nil))

  (use-package php-mode
    :defer t
    :mode (("\\.php\\'" . php-mode)))

  (use-package sass-mode
    :defer t)

  (use-package expand-region
    :commands er/expand-region)

  (use-package elfeed
    :defer t
    :config
    (setq elfeed-feeds '("http://planet.clojure.in/atom.xml"
			 "https://planet.emacslife.com/atom.xml"
			 "http://planet.kernel.org/rss20.xml"
			 "http://planet.lisp.org/rss20.xml"
			 "https://twobithistory.org/feed.xml")))

  (use-package clojure-mode
    :defer t)

  (use-package monroe
    :defer t
    :hook
    (clojure-mode . clojure-enable-monroe)
    (monroe-mode . paredit-mode))

  (use-package go-mode
    :defer t
    :hook
    (go-mode . (lambda ()
		 (setq indent-tabs-mode 1)
		 (setq tab-width 2)))
    :config
    (setq gofmt-command "/usr/bin/gofmt"))

  (use-package fennel-mode
    :defer t)

  (use-package lua-mode
    :defer t
    :mode (("\\.lua\\'" . lua-mode)))

  (use-package company-lua
    :defer t)

  (use-package magit
    :commands magit-status
    :custom
    (magit-repository-directories '(("/home/benja/work" . 2)
				    ("/home/benja/src" . 3)
				    ("/home/benja/projects" . 2))))

  (use-package org
    :defer t
    :hook (org-mode . auto-fill-mode)
    :config
    (setq org-startup-indented t
	  org-startup-truncated nil
	  org-agenda-window-setup 'current-window
	  org-agenda-start-on-weekday nil
	  org-agenda-compact-blocks t
	  org-hide-leading-stars t
	  org-latex-toc-command "\\tableofcontents \\clearpage"
	  org-export-async-init-file "~/.emacs.d/org-init.el"
	  org-src-preserve-indentation t
	  org-agenda-files '("~/org/projects.org" "~/org/habits.org")
	  org-default-notes-file "~/org/inbox.org"
	  org-refile-targets '(("~/org/projects.org" :maxlevel . 1))
	  org-capture-templates '(("t" "Todo"
				   entry (file "~/org/inbox.org")
				   "* TODO %?\n %u\n"))
	  org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
				     (todo . " %i %-12:c %b")
				     (tags . " %i %-12:c")
				     (search . " %i %-12:c"))))

  (use-package ox-reveal
    :after org
    :config
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0"))

  (use-package ox-latex
    :straight nil
    :after org
    :config
    (setq org-latex-create-formula-image-program 'dvipng)
    (org-babel-do-load-languages 'org-babel-load-languages '((latex . t))))

  (use-package htmlize
    :defer t)

  (use-package pug-mode
    :defer t
    :mode (("\\.pug\\'" . pug-mode))
    :hook (pug-mode . (lambda ()
                        (setq indent-tabs-mode nil)))
    :config (setq pug-tab-width 2))

  (use-package personal-keybindings
    :straight nil
    :init
    (global-hl-line-mode)
    (windmove-default-keybindings)
    (global-unset-key "\C-z")
    (line-number-mode -1)
    (column-number-mode -1)
    (setq search-whitespace-regexp ".*")
    :hook
    (prog-mode . prettify-symbols-mode)
    (css-mode . electric-pair-mode)
    :bind
    (:map mode-specific-map
	  ("C-SPC t" . eshell)
	  ("C-SPC C-t" . ansi-term)
	  ("C-SPC i" . imenu)
	  ("C-SPC C-i" . company-complete)
	  ("C-SPC @" . mu4e)
	  ("C-SPC p" . project-find-file)
	  ("C-SPC C-r" . recompile)
	  ("C-SPC r" . compile)
	  ("C-SPC l" . slime)
	  ("C-SPC m" . monroe)
	  ("C-SPC e" . elfeed)
	  ("C-SPC c" . org-capture)
	  ("C-SPC a" . org-agenda)
	  ("C-SPC x" . (lambda ()
			 (interactive)
			 (org-latex-export-to-pdf t))))
    (:map global-map
	  ("M-SPC" . cycle-spacing)
	  ("M-x" . amx)
	  ("M-/" . hippie-expand)
	  ("C-z" . call-last-kbd-macro)
	  ("C-w" . backward-kill-word)
	  ("C-=" . er/expand-region)
	  ("<up>" . move-text-up)
	  ("<down>" . move-text-down))
    (:map ctl-x-map
	  ("C-k" . kill-region)
	  ("g" . magit-status)
	  ("C-b" . ibuffer))))
