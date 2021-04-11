;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(setq-default straight-check-for-modifications 'live
	      straight-cache-autoloads t
	      use-package-always-defer t
	      use-package-verbose nil
	      use-package-expand-minimally t
	      use-package-hook-name-suffix "")

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
      epg-gpg-program "gpg"
      explicit-shell-file-name "/bin/bash"
      uniquify-buffer-name-style 'forward
      echo-keystrokes 0.5
      line-spacing 0
      x-underline-at-descent-line t
      widget-image-enable nil)

(fringe-mode '(5 . 5))
(defface fallback '((t :family "Fira Code Light"
		       :inherit 'face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
			(make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
			(make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
			(string-to-vector " …"))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(set-face-font 'default "FiraCode 8")
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
		    '(vertical-scroll-bars . nil)
		    '(internal-border-width . 2)
		    '(font . "FiraCode 8"))))
(set-frame-parameter (selected-frame)
		     'internal-border-width 2)

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

(use-package modus-operandi-theme
  :straight t
  :hook (after-init-hook . (lambda () (load-theme 'modus-operandi t)))
  :init (setq modus-operandi-theme-parent-match 'subtle-bold
	      modus-operandi-theme-intense-paren-match t
	      modus-operandi-theme-mode-line '3d
	      modus-operandi-theme-completions 'moderate))

(use-package personal-keybindings
  :init (progn (global-unset-key "\C-z")
	       (define-prefix-command 'ctl-z-map)
	       (global-hl-line-mode))
  :hook (prog-mode-hook . prettify-symbols-mode)
  :hook (prog-mode-hook . whitespace-mode)
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

(use-package gcmh
  :straight t
  :init (gcmh-mode +1))

(use-package isearch
  :config (setq isearch-allow-scroll t
		search-whitespace-regexp ".*"))

(use-package eshell
  :hook (eshell-mode-hook . exec-path-from-shell-initialize)
  :hook (eshell-mode-hook . eshell-smart-initialize)
  :bind (:map ctl-z-map ("t" . eshell))
  :config
  (progn
    (setq eshell-where-to-jump 'begin
	  eshell-review-quick-commands nil
	  eshell-smart-space-goes-to-end t)
    (dolist (mode '(eshell-smart eshell-tramp))
      (add-to-list 'eshell-modules-list mode))))

(use-package tramp
  :config (setq tramp-default-method "ssh"
		vc-ignore-dir-regexp
		(rx (seq bos
			 (or (seq (any "/\\") (any "/\\")
				  (one-or-more (not (any "/\\")))
				  (any "/\\"))
			     (seq "/" (or "net" "afs" "...") "/")
			     ;; Ignore all tramp paths.
			     (seq "/"
				  (eval (cons 'or (mapcar #'car tramp-methods)))
				  ":"
				  (zero-or-more anything)))
			 eos))))

(use-package exec-path-from-shell
  :straight t
  :config (setq exec-path-from-shell-check-startup-files nil))

(use-package project
  :config
  (dolist (folder '("node_modules" "target" "out"
		    ".cljs_node_repl" ".shadow-cljs"))
    (add-to-list 'vc-directory-exclusion-list folder)))

(use-package vertico
  :straight (:host github :repo "minad/vertico")
  :init (vertico-mode +1)
  :config
  (setq completion-styles '(basic substring partial-completion flex)
	completion-ignore-case t
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t
	enable-recursive-minibuffers t))

(use-package marginalia
  :straight t
  :init (marginalia-mode +1)
  :config (setq marginalia-annotators '(marginalia-annotators-heavy nil)))

(use-package dired
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'top
	dired-use-ls-dired nil
	dired-dwim-target t
	dired-listing-switches "-lha1v"))

(use-package text-mode
  :hook (text-mode-hook . turn-off-auto-fill))

(use-package eldoc-mode
  :hook (emacs-lisp-mode-hook . turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.3
	eldoc-echo-area-use-multiline-p nil))

(use-package whitespace
  :bind (:map ctl-z-map ("C-." . whitespace-cleanup))
  :config
  (setq whitespace-line-column 80
	whitespace-style '(face lines-tail trailing indentation
			   space-before-tab space-after-tab)))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package format-all
  :straight t
  :hook (format-all-mode-hook . (lambda ()
				  (setq format-all-formatters
					'(("C" (clang-format))
					  ("Go" (gofmt)))))))

(use-package c-mode
  :hook ((c-mode-hook . format-all-mode)
	 (c-mode-hook . (lambda () (c-set-style "linux")))))

(use-package gdb
  :config (setq gdb-delete-out-of-scope nil))

(use-package js-mode
  :hook (js-mode-hook . electric-pair-mode)
  :config (setq js-indent-level 2))

(use-package prettier
  :straight t
  :hook (prettier-mode-hook . exec-path-from-shell-initialize)
  :commands prettier-mode)

(use-package go-mode
  :straight t
  :hook (go-mode-hook . format-all-mode))

(use-package paredit
  :straight t
  :bind (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook ((clojure-mode-hook lisp-interaction-mode-hook
	  emacs-lisp-mode-hook lisp-mode-hook) . enable-paredit-mode))

(use-package expand-region
  :straight t
  :bind (:map global-map ("C-=" . er/expand-region)))

(use-package dumb-jump
  :straight t
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :config (setq dumb-jump-force-searcher 'rg))

(use-package xref
  :config
  (setq xref-search-program 'ripgrep))

(use-package clojure-mode
  :straight t
  :mode (("\\.clj\\[s\\*\\'" . clojure-mode))
  :config
  (progn
    (put-clojure-indent 'match 1)
    (put-clojure-indent 'fn-traced 1)))

(use-package monroe
  :straight t
  :hook (clojure-mode-hook . clojure-enable-monroe)
  :hook (monroe-mode-hook . enable-paredit-mode)
  :bind (:map ctl-z-map ("m" . monroe))
  :config (setq monroe-detail-stacktraces t))

(use-package fennel-mode
  :straight t
  :mode (("\\.fnl\\'" . fennel-mode)))

(use-package lua-mode
  :straight t
  :mode (("\\.lua\\'" . lua-mode)))

(use-package magit
  :straight t
  :config
  (setq magit-repository-directories '(("~/work" . 2) ("~/src" . 3))))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-split-window-function 'split-window-horizontally))

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)))

(use-package mpc
  :commands mpc)
