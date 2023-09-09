;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(load-file "~/.emacs.d/useful.el")

;; packages
(use-package modus-themes
  :ensure t
  :demand t
  :hook (after-init-hook . (lambda ()
                             (global-hl-line-mode)
                             (savehist-mode +1)
                             (load-theme 'modus-operandi t nil)))
  :custom
  (modus-themes-paren-match 'bold)
  (modus-themes-mode-line 'borderless)
  (modus-themes-bold-constructs nil)
  (modus-themes-scale-headings t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-variable-pitch-headings t))

(use-package prog-mode
  :hook ((prog-mode-hook . prettify-symbols-mode)
         (prog-mode-hook . whitespace-mode)))

(use-package whitespace
  :bind (:map ctl-z-map ("C-." . whitespace-cleanup))
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face
                      lines-tail
                      trailing
                      space-before-tab
                      space-after-tab)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package c-mode
  :hook (c-mode-hook . (lambda () (c-set-style "linux"))))

(use-package gdb
  :custom (gdb-delete-out-of-scope nil))

(use-package js-mode
  :hook (js-mode-hook . electric-pair-mode)
  :custom (js-indent-level 2))

(use-package go-mode
  :ensure t)

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook ((clojure-mode-hook
          lisp-interaction-mode-hook
          emacs-lisp-mode-hook
          lisp-mode-hook) . enable-paredit-mode))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\[s\\*\\'" . clojure-mode))
  :config
  (progn
    (put-clojure-indent 'match 1)
    (put-clojure-indent 'fn-traced 1)))

(use-package monroe
  :ensure t
  :hook (clojure-mode-hook . clojure-enable-monroe)
  :hook (monroe-mode-hook . enable-paredit-mode)
  :bind (:map ctl-z-map ("m" . monroe))
  :custom (monroe-detail-stacktraces t))

(use-package fennel-mode
  :ensure t
  :mode (("\\.fnl\\'" . fennel-mode)))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)))

(use-package nov
  :ensure t
  :hook (nov-mode-hook . (lambda ()
                           (face-remap-add-relative 'variable-pitch :family "Liberation Serif" :height 2.0)))
  :mode (("\\.epub\\'" . nov-mode)))

(use-package diff-mode
  :custom (diff-default-read-only t))

;; (use-package agitate
;;   :ensure t
;;   :init (agitate-log-edit-informative-mode))

(use-package abbrev
  :config
  (advice-add 'add-global-abbrev :after (lambda (&rest _) (write-abbrev-file))))

(use-package sly
  :ensure t)

(use-package isearch
  :custom
  (isearch-lazy-count t))

(use-package eglot
  :ensure t)

(use-package gptel
  :ensure t)
