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
              use-package-hook-name-suffix ""
              indent-tabs-mode nil
              user-full-name "Benjamín Buccianti"
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
              auto-window-vscroll nil
              auto-save-default nil
              vc-follow-symlinks t
              epg-gpg-program "gpg"
              explicit-shell-file-name "/bin/bash"
              uniquify-buffer-name-style 'forward
              echo-keystrokes 0.5
              line-spacing 0
              x-underline-at-descent-line t
              widget-image-enable nil
              tab-always-indent 'complete)

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

(fringe-mode '(5 . 5))
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; font config
(set-face-font 'default "Hack 8")

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

;; useful keybindings
(global-unset-key "\C-z")
(define-prefix-command 'ctl-z-map)

(define-key global-map (kbd "C-z")	#'ctl-z-map)
(define-key global-map (kbd "M-/")	#'hippie-expand)
(define-key global-map (kbd "M-SPC")	#'cycle-spacing)
(define-key global-map (kbd "C-w")	#'backward-kill-word)

(define-key ctl-x-map  (kbd "C-b")	#'ibuffer)
(define-key ctl-x-map  (kbd "C-k")	#'kill-region)
(define-key ctl-x-map  (kbd "g")	#'magit-status)

(define-key ctl-z-map  (kbd "r")	#'compile)
(define-key ctl-z-map  (kbd "C-r")	#'recompile)

;; packages
(straight-use-package 'use-package)

(use-package modus-themes
  :straight t
  :demand t
  :hook (after-init-hook . (lambda ()
                             (global-hl-line-mode)
                             (savehist-mode +1)
                             (modus-themes-load-operandi)))
  :init (setq modus-themes-paren-match 'bold
              modus-themes-mode-line 'borderless
              modus-themes-completions 'moderate
              modus-themes-bold-constructs nil
              modus-themes-scale-headings t
              modus-themes-variable-pitch-ui t
              modus-themes-variable-pitch-headings t)
  :config (modus-themes-load-themes))

(use-package windmove
  :init (windmove-default-keybindings))

(use-package tree-sitter
  :straight t
  :hook (tree-sitter-mode-hook . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :init
  (setq tree-sitter-major-mode-language-alist '((go-mode . go)
                                                (c-mode . c)
                                                (rjsx-mode . javascript)
                                                (js-mode . javascript))))

(use-package prog-mode
  :hook ((prog-mode-hook . prettify-symbols-mode)
         (prog-mode-hook . whitespace-mode)))

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
  :config
  (setq tramp-default-method "ssh"
        vc-ignore-dir-regexp "\\`\\(?:[/\\][/\\][^/\\]+[/\\]\\|/\\(?:\\.\\.\\.\\|afs\\|net\\)/\\|/\\(?:a\\(?:db\\|fp\\)\\|d\\(?:avs?\\|oas\\)\\|f\\(?:[ct]p\\)\\|gdrive\\|k\\(?:rlogin\\|su\\)\\|mtp\\|n\\(?:c\\|extcloud\\)\\|p\\(?:linkx?\\|s\\(?:\\(?:c\\|ft\\)p\\)\\)\\|r\\(?:c\\(?:lone\\|p\\)\\|em\\(?:cp\\|sh\\)\\|s\\(?:h\\|ync\\)\\)\\|s\\(?:cpx?\\|ftp\\|mb\\|sh\\(?:fs\\|x\\)?\\|udo\\(?:edit\\)?\\|[gu]\\)\\|telnet\\):[^z-a]*\\)\\'"))

(use-package exec-path-from-shell
  :straight t
  :config (setq exec-path-from-shell-check-startup-files nil))

(use-package project
  :bind (:map project-prefix-map ("m" . magit-status))
  :config
  (dolist (folder '("node_modules" "target" "out"
                    ".cljs_node_repl" ".shadow-cljs"))
    (add-to-list 'vc-directory-exclusion-list folder))
  (add-to-list 'project-switch-commands '(magit-status "Magit") t))

(use-package orderless
  :straight t
  :demand t)

(use-package marginalia
  :straight t
  :init (marginalia-mode +1)
  :config (setq marginalia-annotators '(marginalia-annotators-heavy nil)))

(use-package vertico
  :straight (:host github :repo "minad/vertico")
  :init (vertico-mode +1)
  :config
  (setq completion-styles '(orderless)
        completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        enable-recursive-minibuffers t
        resize-mini-windows nil))

(use-package corfu
  :straight (:host github :repo "minad/corfu" :files ("*.el"))
  :hook ((prog-mode-hook . corfu-mode)
         (eshell-mode-hook . corfu-mode)))

(use-package consult
  :straight t)

(use-package affe
  :straight (:host github :repo "minad/affe" :files ("*.el"))
  :bind (:map ctl-z-map
              ("f" . affe-find)
              ("g" . affe-grep))
  :config
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches
        affe-find-command "fd -HI -c never -t f"))

(use-package dired
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-use-ls-dired nil
        dired-dwim-target t
        dired-listing-switches "-lha1v"))

(use-package text-mode
  :hook (text-mode-hook . turn-off-auto-fill))

(use-package devdocs-browser
  :straight (:host github :repo "blahgeek/emacs-devdocs-browser" :file ("*.el"))
  :bind (:map ctl-z-map ("h" . devdocs-browser-open-in)))

(use-package eldoc-mode
  :hook (emacs-lisp-mode-hook . turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.3
        eldoc-echo-area-use-multiline-p nil))

(use-package whitespace
  :bind (:map ctl-z-map ("C-." . whitespace-cleanup))
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face
                           lines-tail
                           trailing
                           indentation::space
                           space-before-tab
                           space-after-tab)))

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
         (c-mode-hook . tree-sitter-mode)
         (c-mode-hook . (lambda () (c-set-style "linux")))))

(use-package gdb
  :config (setq gdb-delete-out-of-scope nil))

(use-package js-mode
  :hook ((js-mode-hook . electric-pair-mode)
         (js-mode-hook . tree-sitter-mode)
         (js-mode-hook . prettier-mode))
  :config (setq js-indent-level 2))

(use-package rjsx-mode
  :straight t
  :hook ((rjsx-mode-hook . tree-sitter-mode)
         (rjsx-mode-hook . prettier-mode))
  :mode (("\\.jsx'" . rjsx-mode)))

(use-package prettier
  :straight t
  :hook (prettier-mode-hook . exec-path-from-shell-initialize)
  :commands prettier-mode)

(use-package go-mode
  :straight t
  :hook ((go-mode-hook . tree-sitter-mode)
         (go-mode-hook . format-all-mode)))

(use-package paredit
  :straight t
  :bind (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook ((clojure-mode-hook
          lisp-interaction-mode-hook
          emacs-lisp-mode-hook
          lisp-mode-hook) . enable-paredit-mode))

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

(use-package browse-url
  :config
  (setq browse-url-generic-program "/usr/bin/qutebrowser"
        browse-url-browser-function 'browse-url-generic))

(use-package nov
  :straight t
  :mode (("\\.epub\\'" . nov-mode)))
