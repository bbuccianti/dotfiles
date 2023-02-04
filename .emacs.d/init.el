;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(load-file "~/.emacs.d/useful.el")

;; packages
(use-package modus-themes
  :straight t
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
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face
                           lines-tail
                           trailing
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
                                          ("C++" (clang-format))
                                          ("Go" (gofmt)))))))

(use-package c-mode
  :hook ((c-mode-hook . format-all-mode)
         (c-mode-hook . (lambda () (c-set-style "linux")))))

(use-package c++-mode
  :hook (c++-mode-hook . format-all-mode))

(use-package gdb
  :config (setq gdb-delete-out-of-scope nil))

(use-package js-mode
  :hook (js-mode-hook . electric-pair-mode)
  :config (setq js-indent-level 2))

(use-package rjsx-mode
  :straight t
  :mode (("\\.jsx'" . rjsx-mode)))

(use-package go-mode
  :straight t
  :hook (go-mode-hook . format-all-mode))

(use-package paredit
  :straight t
  :bind (:map paredit-mode-map ("C-w" . paredit-backward-kill-word))
  :hook ((clojure-mode-hook
          lisp-interaction-mode-hook
          emacs-lisp-mode-hook
          lisp-mode-hook) . enable-paredit-mode))

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

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)))

(use-package nov
  :straight t
  :mode (("\\.epub\\'" . nov-mode)))

(use-package diff-mode
  :custom (diff-default-read-only t))

(use-package agitate
  :straight t
  :init (agitate-log-edit-informative-mode))
