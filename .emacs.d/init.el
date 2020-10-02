;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(setq-default straight-check-for-modifications 'live
	      straight-cache-autoloads t
	      use-package-always-defer t
	      use-package-verbose t
	      use-package-expand-minimally t)

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
(load custom-file 'noerror 'nomessage)

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
      browse-url-generic-program "/usr/bin/qutebrowser"
      browse-url-browser-function 'browse-url-generic
      auto-window-vscroll nil
      auto-save-default nil
      vc-follow-symlinks t
      epg-gpg-program "gpg2"
      explicit-shell-file-name "/bin/mksh"
      uniquify-buffer-name-style 'forward)

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

;; packages
(straight-use-package 'use-package)

(use-package personal-keybindings
  :init (progn (global-unset-key "\C-z")
	       (define-prefix-command 'ctl-z-map)
	       (global-hl-line-mode))
  :bind (:map global-map
	      ("C-z" . ctl-z-map)
	      ("M-SPC" . cycle-spacing)
	      ("M-o" . other-window)
	      ("M-/" . hippie-expand)
	      ("C-w" . backward-kill-word)
	      ("S-<left>" . windmove-left)
	      ("S-<up>" . windmove-up)
	      ("S-<down>" . windmove-down)
	      ("S-<right>" . windmove-right))
  :bind (:map ctl-z-map
	      ("r" . compile)
	      ("C-r" . recompile))
  :bind (:map ctl-x-map
	      ("C-k" . kill-region)
	      ("C-b" . ibuffer)))

(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)
	 (prog-mode . whitespace-mode)))

(use-package isearch
  :config (setq isearch-allow-scroll t
		search-whitespace-regexp ".*"))

(use-package eshell
  :hook (eshell-mode . (lambda ()
			 (exec-path-from-shell-initialize)
			 (eshell-smart-initialize)))
  :bind (:map ctl-z-map ("t" . eshell))
  :config
  (progn
    (setq eshell-where-to-jump 'begin
	  eshell-review-quick-commands nil
	  eshell-smart-space-goes-to-end t)
    (dolist (mode '(eshell-smart eshell-tramp))
      (add-to-list 'eshell-modules-list mode))))

(use-package exec-path-from-shell
  :straight t
  :commands exec-path-from-shell-initialize
  :config (setq exec-path-from-shell-check-startup-files nil))

(use-package project
  :config
  (dolist (folder '("node_modules" "target" "out"
		    ".cljs_node_repl" ".shadow-cljs"))
    (add-to-list 'vc-directory-exclusion-list folder)))

(use-package tramp
  :config (setq tramp-default-method "ssh"))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :init (selectrum-mode +1))

(use-package selectrum-prescient
  :straight (:host github
	     :repo "raxod502/prescient.el"
	     :files ("selectrum-prescient.el"))
  :init (selectrum-prescient-mode +1))

(use-package prescient
  :straight t
  :init (prescient-persist-mode +1)
  :config (setq prescient-history-length 1000
		prescient-filter-method '(literal initialism regexp fuzzy)))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config (setq dired-recursive-copies 'always
		dired-recursive-deletes 'top
		dired-use-ls-dired nil
		dired-dwim-target t
		dired-listing-switches "-lha1v"))

(use-package text-mode
  :hook (text-mode . turn-off-auto-fill))

(use-package whitespace
  :bind (:map ctl-z-map ("C-." . whitespace-cleanup))
  :config (setq whitespace-line-column 80
		whitespace-style '(face lines-tail trailing indentation
				   space-before-tab space-after-tab)))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :straight t
  :bind (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook ((clojure-mode
	  lisp-interaction-mode
	  emacs-lisp-mode
	  lisp-mode) . enable-paredit-mode))

(use-package imenu
  :bind (:map ctl-z-map ("i" . imenu))
  :config (setq imenu-auto-rescan t))

(use-package expand-region
  :straight t
  :bind (:map global-map ("C-=" . er/expand-region)))

(use-package clojure-mode
  :straight t
  :mode (("\\.clj\\[s\\*\\'" . clojure-mode))
  :config (progn
	    (put-clojure-indent 'match 1)
	    (put-clojure-indent 'fn-traced 1)))

(use-package monroe
  :straight t
  :hook (clojure-mode . clojure-enable-monroe)
  :hook (monroe-mode . enable-paredit-mode)
  :bind (:map ctl-z-map ("m" . monroe))
  :config (setq monroe-detail-stacktraces t))

(use-package rust-mode
  :straight t
  :mode (("\\.rs\\'" . rust-mode))
  :hook (rust-mode . electric-pair-mode)
  :config (setq rust-format-on-save t
		rust-format-show-buffer nil
		rust-rustfmt-bin "/home/bbuccianti/.cargo/bin/rustfmt"
		rust-cargo-bin "/home/bbuccianti/.cargo/bin/cargo"))

(use-package php-mode
  :straight t
  :mode (("\\.php\\'" . php-mode)))

(use-package fennel-mode
  :straight t
  :mode (("\\.fnl\\'" . fennel-mode)))

(use-package lua-mode
  :straight t
  :mode (("\\.lua\\'" . lua-mode)))

(use-package ansi-color
  :hook (compilation-filter
	 . (lambda ()
	     (toggle-read-only)
	     (ansi-color-apply-on-region compilation-filter-start (point))
	     (toggle-read-only))))

(use-package magit
  :straight t
  :bind (:map ctl-x-map
	      ("g" . magit-status)
	      ("M-g" . magit-dispatch))
  :config
  (setq magit-repository-directories '(("~/work" . 2) ("~/src" . 3))))

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
				 entry (file+headline "~/org/projects.org"
						      "Inbox")
				 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
				("n" "Note"
				 entry (file "~/org/notes.org")
				 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:"))
	org-todo-keywords '((sequence "PROJECT(p)" "TODO(t)" "|"
				      "DONE(d)" "CANCELLED(c)"))
	org-directory "/home/bbuccianti/notes/"))

(use-package org-agenda
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
				   (search . " %i %-12:c"))))

(use-package deft
  :straight t
  :config (setq deft-directory "~/notes"
		deft-default-extension "org"
		deft-extensions '("org")))

(use-package zetteldeft
  :straight t)

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)))

(use-package simple-mpc
  :straight t
  :bind (:map ctl-z-map ("s" . simple-mpc)))

(use-package modus-operandi-theme
  :straight t
  :init (load-theme 'modus-operandi nil nil))
