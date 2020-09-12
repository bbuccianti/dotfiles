;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(setq straight-use-package-by-default t
      straight-check-for-modifications 'live
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
      browse-url-generic-program "/usr/local/bin/luakit"
      browse-url-browser-function 'browse-url-generic
      auto-window-vscroll nil
      auto-save-default nil
      vc-follow-symlinks t
      epg-gpg-program "gpg2"
      explicit-shell-file-name "/bin/ksh")

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

;; packages
(straight-use-package 'use-package)

(use-package use-package
  :config
  (setq use-package-always-defer t
	use-package-verbose nil))

(use-package personal-keybindings
  :straight nil
  :init (progn
	  (global-unset-key "\C-z")
	  (define-prefix-command 'ctl-z-map)
	  (set-fringe-mode 1))
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

(use-package isearch
  :straight nil
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
  :bind (:map ctl-z-map
	      ("p" . project-find-file)
	      ("/" . project-find-regexp))
  :config
  (dolist (folder '("node_modules" "target" "out"
		    ".cljs_node_repl" ".shadow-cljs"))
    (add-to-list 'vc-directory-exclusion-list folder)))

(use-package tramp
  :straight nil
  :config (setq tramp-default-method "ssh"))

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :init (selectrum-mode +1))

(use-package prescient
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000
	prescient-filter-method '(literal regexp fuzzy)))

(use-package selectrum-prescient
  :straight (:host github
	     :repo "raxod502/prescient.el"
             :files ("selectrum-prescient.el"))
  :init
  (selectrum-prescient-mode +1)
  (setq selectrum-current-candidate '((t (:background "#222226"
					  :weight bold
					  :foreground "gainsboro")))
	selectrum-primary-highlight '((t (:foreground "#c56ec3")))
	selectrum-secondary-highlight '((t (:foreground "#2d9574")))))

(use-package dired
  :straight nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'top
	dired-use-ls-dired nil
	dired-dwim-target t
	dired-listing-switches "-lha1v")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package gnus
  :straight nil
  :config (setq gnus-select-method '(nntp "news.gmane.io")))

(use-package notmuch
  :bind (:map ctl-z-map ("@" . notmuch))
  :config
  (setq smtpmail-debug-info nil ;; TOGGLE FOR DEBUG ONLY!
	mail-user-agent 'message-user-agent
	smtpmail-default-smtp-server "mail.buccianti.dev"
	smtpmail-smtp-server "mail.buccianti.dev"
	smtpmail-local-domain "buccianti.dev"
	smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
	message-send-mail-function 'smtpmail-send-it
	send-mail-function 'smtpmail-send-it
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
  :bind (:map global-map ("C-=" . er/expand-region)))

(use-package elfeed
  :bind (:map ctl-z-map ("e" . elfeed))
  :config
  (setq elfeed-feeds '("http://planet.clojure.in/atom.xml" "https://planet.emacslife.com/atom.xml"
		       "https://twobithistory.org/feed.xml" "https://drewdevault.com/blog/index.xml")))

(use-package clojure-mode
  :mode (("\\.clj\\[s\\*\\'" . clojure-mode))
  :config
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'fn-traced 1))

(use-package monroe
  :hook
  (clojure-mode . clojure-enable-monroe)
  (monroe-mode . enable-paredit-mode)
  :bind (:map ctl-z-map ("m" . monroe))
  :config (setq monroe-detail-stacktraces t))

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
  :bind (:map ctl-x-map
	      ("g" . magit-status)
	      ("M-g" . magit-dispatch))
  :config
  (setq magit-repository-directories '(("~/work" . 2) ("~/src" . 3))))

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

(use-package org-agenda
  :straight nil
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

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))

(use-package simple-mpc
  :bind (:map ctl-z-map ("s" . simple-mpc)))

(use-package zerodark-theme
  :init (load-theme 'zerodark t))
