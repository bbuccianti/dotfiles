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
      browse-url-browser-function 'browse-url-firefox
      file-name-handler-alist nil
      auto-window-vscroll nil
      initial-frame-alist '((font . "Hack-10"))
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      bidi-display-reordering nil
      inhibit-startup-screen t
      inhibit-default-init t
      inhibit-startup-message t
      initial-scratch-message nil
      frame-inhibit-implied-resize t
      create-lockfiles nil
      auto-save-default nil
      vc-follow-symlinks t
      epg-gpg-program "gpg2"
      inferior-lisp-program "/bin/sbcl"
      explicit-shell-file-name "/bin/mksh")

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

(defun yank-from-kill-ring (choice)
  "Uses completion-read to insert a string from the kill-ring."
  (interactive
   (list (selectrum-read "Kill-ring: " kill-ring )))
  (insert choice))

;; packages

;; (setq use-package-verbose t)
(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :after (:any shell eshell)
  :config (exec-path-from-shell-initialize))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package battery
  :defer t
  :config (display-battery-mode))

(use-package selectrum
  :init (selectrum-mode t))

(use-package prescient
  :defer t
  :config (prescient-persist-mode t))

(use-package selectrum-prescient
  :after selectrum
  :config (selectrum-prescient-mode t))

(use-package fzf
  :commands fzf
  :custom
  (fzf/args "-x --ansi --color 16 --print-query"))

(use-package amx
  :commands amx
  :custom (amx-show-key-bindings t))

(use-package dired
  :straight nil
  :commands dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'top
	dired-use-ls-dired nil
	dired-listing-switches "-lha1v --group-directories-first")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package gnus
  :straight nil
  :disabled
  :commands gnus
  :config
  (setq gnus-select-method '(nntp "news.gmane.io")))

(use-package mu4e
  :straight nil
  :load-path "~/src/github/djcb/mu/mu4e"
  :commands mu4e
  :custom
  (mu4e-maildir "~/.mail")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
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
  :bind (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (clojure-mode . enable-paredit-mode))

(use-package noccur
  :commands noccur-project)

(use-package nov
  :disabled
  :mode (("\\.epub\\'" . nov-mode)))

(use-package company
  :defer t
  :commands company-complete
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map (("C-n" . company-select-next)
				  ("C-p" . company-select-previous)))
  :custom (company-idle-delay nil))

(use-package web-mode
  :mode (("\\.php\\'" . web-mode))
  :hook (web-mode . (lambda () (setq web-mode-markup-indent-offset 2))))

(use-package expand-region
  :disabled
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
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'fn-traced 1))

(use-package monroe
  :commands monroe
  :hook
  (clojure-mode . clojure-enable-monroe)
  (monroe-mode . enable-paredit-mode))

(use-package haskell-mode
  :disabled
  :mode (("\\.hs\\'" . haskell-mode))
  :hook (haskell-mode . interactive-haskell-mode))

(use-package go-mode
  :disabled
  :mode (("\\.go\\'" . go-mode))
  :hook
  (go-mode . (lambda () (setq indent-tabs-mode 1 tab-width 2)))
  :custom (gofmt-command "/usr/bin/gofmt"))

(use-package fennel-mode
  :mode (("\\.fnl\\'" . fennel-mode)))

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode)))

(use-package ansi-color
  :straight nil
  :defer t
  :hook
  (compilation-filter
   . (lambda ()
       (toggle-read-only)
       (ansi-color-apply-on-region compilation-filter-start (point))
       (toggle-read-only))))

(use-package magit
  :commands (magit-status magit-list-repositories)
  :custom
  (magit-repository-directories '(("~/work" . 2) ("~/src" . 3))))

(straight-use-package
 `(notdeft :type git :local-repo "/home/bbuccianti/src/github/hasu/notdeft"))

(use-package notdeft
  :defer t
  :custom
  (notdeft-xapian-program "/home/bbuccianti/bin/notdeft-xapian")
  (notdeft-directories '("/home/bbuccianti/notes"))
  (notdeft-notename-function 'notdeft-default-title-to-notename))

(use-package org
  :straight nil
  :mode (("\\.org\\'" . org-mode))
  :custom
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-start-on-weekday nil)
  (org-agenda-compact-blocks nil)
  (org-modules '(ol-info ol-mhe ol-rmail))
  (org-hide-leading-stars t)
  (org-latex-toc-command "\\tableofcontents \\clearpage")
  (org-export-async-init-file "~/.emacs.d/org-init.el")
  (org-src-preserve-indentation t)
  (org-default-notes-file "/home/bbuccianti/org/notes.org")
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-completion-use-ido nil)
  (org-log-done "note")
  (org-tags-column -60)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-fast-tag-selection-single-key 'expert)
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (org-refile-targets '((nil :maxlevel . 9)
			(org-agenda-files :maxlevel . 9)))
  (org-capture-templates '(("t" "Todo"
			    entry (file+headline "~/org/projects.org" "Inbox")
			    "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
			   ("n" "Note"
			    entry (file "~/org/notes.org")
			    "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:")))
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
			      (todo . "%l" )
			      (tags . "%l")
			      (search . " %i %-12:c")))
  (org-agenda-custom-commands
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
		     ((org-ql-block-header "Next tasks")))))
     ("w" "Week planning"
      ((agenda "" ((org-agenda-span 'week)))
       (org-ql-block '(and (todo)
			   (tags "inbox"))
		     ((org-ql-block-header "Inbox")))
       (org-ql-block '(and (todo "PROJECT" "TODO")
			   (not (tags "notes"))
			   (not (ancestors (children (todo "NEXT"))))
			   (not (children (todo "NEXT")))
			   (not (or (scheduled) (deadline))))
		     ((org-ql-block-header "Stuck projects")))))
     ("u" "Study time!"
      ((agenda "" ((org-agenda-span 'week)))
       (org-ql-block '(and (tags "notes")
			   (todo)
			   (not (or (scheduled) (deadline))))
		     ((org-ql-block-header "Study time!")))))))
  (org-todo-keywords '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|"
				 "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces '(("TODO" :foreground "gold" :weight bold)
			    ("NEXT" :foreground "deep sky blue" :weight bold)
			    ("DONE" :foreground "forest green" :weight bold)
			    ("CANCELLED" :foreground "red" :weight bold)))
  (org-agenda-files '("/home/bbuccianti/org/"))
  (org-directory "/home/bbuccianti/notes/"))

(use-package org-sidebar
  :disabled
  :after org)

(use-package org-ql
  :after org)

(use-package ox-reveal
  :disabled
  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.8.0"))

(use-package ox-latex
  :straight nil
  :disabled
  :custom
  (org-latex-create-formula-image-program 'dvipng)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((latex . t))))

(use-package htmlize
  :disabled)

(use-package markdown-mode
  :disabled
  :mode (("\\.md\\'" . markdown-mode)))

(use-package pug-mode
  :disabled
  :mode (("\\.pug\\'" . pug-mode))
  :hook (pug-mode . (lambda () (setq indent-tabs-mode nil)))
  :custom (pug-tab-width 2))

(use-package personal-keybindings
  :straight nil
  :init
  (global-hl-line-mode)
  (windmove-default-keybindings)
  (global-unset-key "\C-z")
  (dolist (folder '("node_modules" "target" "out"
		    ".cljs_node_repl" ".shadow-cljs"))
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
	("C-. p" . fzf-git-files)
	("C-. /" . project-find-regexp)
	("C-. n" . noccur-project)
	("C-. C-r" . recompile)
	("C-. r" . compile)
	("C-. f" . fzf)
	("C-. y" . yank-from-kill-ring)
	("C-. m" . monroe)
	("C-. g" . magit-list-repositories)
	("C-. e" . elfeed)
	("C-. c" . org-capture)
	("C-. a" . org-agenda)
	("C-. d" . notdeft)
	("C-. x" . (lambda () (interactive) (org-latex-export-to-pdf t))))
  (:map ctl-x-map
	("C-k" . kill-region)
	("g" . magit-status)
	("C-b" . ibuffer)))
