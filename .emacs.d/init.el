;; -*- lexical-binding: t; -*-

(setq straight-use-package-by-default t
      straight-check-for-modifications nil
      straight-cache-autoloads t)

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
;; (load custom-file 'noerror)

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
      browse-url-browser-function 'browse-url-firefox)

(setq file-name-handler-alist nil
      auto-window-vscroll nil
      default-frame-alist '((font . "Hack-10"))
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      bidi-display-reordering nil
      inhibit-startup-screen t
      inhibit-default-init t
      inhibit-startup-message t
      initial-scratch-message nil
      frame-inhibit-implied-resize t
      create-lockfiles nil
      auto-save-default nil
      epg-gpg-program "gpg2"
      inferior-lisp-program "/usr/bin/sbcl"
      explicit-shell-file-name "/bin/sh")

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq file-name-handler-alist old--file-name-handler-alist
                  gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)
            (garbage-collect))
          t)

;; functions

(defun ido-kill-ring (choice)
  "Uses ido to insert a string from the kill-ring."
  (interactive
   (list (ido-completing-read "Kill-ring: " kill-ring)))
  (insert choice))

;; packages

;; (setq use-package-verbose t)
(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :disabled
  :config (exec-path-from-shell-initialize))

(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-hard t))

(use-package battery
  :config (display-battery-mode))

(use-package ido
  :straight nil
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  :custom
  (qido-enable-flex-matching t)
  (ido-enable-prefix nil)
  (ido-auto-merge-work-directories-length -1)
  (ido-create-new-buffer 'always)
  (ido-use-filename-at-point nil))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :after ido
  :config
  (ido-vertical-mode 1)
  :custom
  (ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package amx
  :commands amx
  :custom
  (amx-show-key-bindings nil))

(use-package dired
  :straight nil
  :commands (ido-select-text dired)
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'top
	dired-use-ls-dired nil
	dired-listing-switches "-lha1v --group-directories-f")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package gnus
  :straight nil
  :commands gnus
  :hook (gnus-group-mode . gnus-topic-mode)
  :config
  (setq gnus-select-method '(nntp "news.gmane.io")))

(use-package mu4e
  :straight nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :custom
  (mu4e-maildir "~/.mail")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-view-show-addresses t)
  (fill-column 72)
  (mail-user-agent 'message-user-agent)
  (smtpmail-default-smtp-server "mail.buccianti.dev")
  (smtpmail-smtp-server "mail.buccianti.dev")
  (smtpmail-local-domain "buccianti.dev")
  (message-send-mail-function 'message-smtpmail-send-it)
  (smtpmail-debug-info nil)
  (message-default-mail-headers "Cc: \nBcc: \n")
  (message-auto-save-directory "~/.mail/drafts")
  (message-kill-buffer-on-exit t)
  (message-directory "~/.mail/sent"))

(use-package whitespace
  :straight nil
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face lines-tail trailing space-before-tab)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :bind
  (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (clojure-mode . enable-paredit-mode))

(use-package move-text
  :commands (move-text-up move-text-down))

(use-package noccur
  :commands noccur-project)

(use-package nov
  :mode (("\\.epub\\'" . nov-mode)))

(use-package company
  :commands company-complete
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map (("C-n" . company-select-next)
				  ("C-p" . company-select-previous)))
  :config
  (setq company-idle-delay nil))

(use-package php-mode
  :mode (("\\.php\\'" . php-mode)))

(use-package sass-mode
  :disabled t)

(use-package expand-region
  :commands er/expand-region)

(use-package elfeed
  :commands elfeed
  :custom
  (elfeed-feeds '("http://planet.clojure.in/atom.xml"
		  "https://planet.emacslife.com/atom.xml"
		  "http://planet.kernel.org/rss20.xml"
		  "http://planet.lisp.org/rss20.xml"
		  "https://twobithistory.org/feed.xml")))

(use-package clojure-mode
  :mode (("\\.clj\\[s\\*\\'" . clojure-mode))
  :config
  (put-clojure-indent 'match 1))

(use-package monroe
  :commands monroe
  :hook
  (clojure-mode . clojure-enable-monroe)
  (monroe-mode . enable-paredit-mode))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  :hook
  (go-mode . (lambda () (setq indent-tabs-mode 1 tab-width 2)))
  :custom (gofmt-command "/usr/bin/gofmt"))

(use-package fennel-mode
  :mode (("\\.fnl\\'" . fennel-mode)))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)))

(use-package magit
  :commands (magit-status magit-list-repositories)
  :custom
  (magit-repository-directories '(("/home/benja/work" . 2)
				  ("/home/benja/src" . 3))))

(use-package org
  :straight nil
  :mode (("\\.org\\'" . org-mode))
  :hook (org-mode . auto-fill-mode)
  :config
  (setq org-startup-indented t
	org-startup-truncated nil
	org-agenda-window-setup 'current-window
	org-agenda-start-on-weekday nil
	org-agenda-compact-blocks t
	org-modules '(org-habit ol-info ol-mhe ol-rmail org-velocity)
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
  :disabled t
  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0"))

(use-package ox-latex
  :straight nil
  :disabled t
  :custom
  (org-latex-create-formula-image-program 'dvipng)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((latex . t))))

(use-package htmlize
  :disabled t)

(use-package pug-mode
  :mode (("\\.pug\\'" . pug-mode))
  :hook (pug-mode . (lambda () (setq indent-tabs-mode nil)))
  :custom (pug-tab-width 2))

(use-package personal-keybindings
  :straight nil
  :init
  (global-hl-line-mode)
  (windmove-default-keybindings)
  (global-unset-key "\C-z")
  (dolist (folder '("node_modules" "target" "out" ".cljs_node_repl"))
    (add-to-list 'vc-directory-exclusion-list folder))
  :custom
  (search-whitespace-regexp ".*")
  :hook
  (prog-mode . prettify-symbols-mode)
  (css-mode . electric-pair-mode)
  :bind
  (:map global-map
	("M-SPC" . cycle-spacing)
	("M-x" . amx)
	("M-/" . hippie-expand)
	("C-z" . call-last-kbd-macro)
	("C-w" . backward-kill-word)
	("C-=" . er/expand-region)
	("<up>" . move-text-up)
	("<down>" . move-text-down)
	("C-. t" . eshell)
	("C-. C-t" . shell)
	("C-. i" . imenu)
	("C-. C-i" . company-complete)
	("C-. @" . mu4e)
	("C-. p" . project-find-file)
	("C-. /" . project-find-regexp)
	("C-. n" . noccur-project)
	("C-. C-r" . recompile)
	("C-. r" . compile)
	("C-. y" . ido-kill-ring)
	("C-. m" . monroe)
	("C-. g" . magit-list-repositories)
	("C-. e" . elfeed)
	("C-. c" . org-capture)
	("C-. a" . org-agenda)
	("C-. x" . (lambda () (interactive) (org-latex-export-to-pdf t))))
  (:map ctl-x-map
	("C-k" . kill-region)
	("g" . magit-status)
	("C-b" . ibuffer)))
