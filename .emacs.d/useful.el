;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(setq-default use-package-always-defer t
              use-package-verbose nil
              use-package-expand-minimally t
              use-package-hook-name-suffix ""
              indent-tabs-mode nil
              user-full-name "Benjamín Buccianti"
              user-mail-address "bbuccianti@proton.me"
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
              tab-always-indent 'complete
              resize-mini-windows nil
              completion-ignore-case t
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t
              inferior-lisp-program "sbcl")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror 'nomessage)

;(fringe-mode '(5 . 5))
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; font config
(set-face-font 'default "Hack 8")

(menu-bar-mode 0)

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
(define-key global-map (kbd "M-o")	#'other-window)

(define-key ctl-x-map  (kbd "C-b")	#'ibuffer)
(define-key ctl-x-map  (kbd "C-k")	#'kill-region)

(define-key ctl-z-map  (kbd "r")	#'compile)
(define-key ctl-z-map  (kbd "C-r")	#'recompile)

;; packages

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package windmove
  :init (windmove-default-keybindings))

(use-package isearch
  :config (setq isearch-allow-scroll t
                search-whitespace-regexp ".*"))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :init (exec-path-from-shell-initialize)
  :hook (eshell-mode-hook . exec-path-from-shell-initialize))

(use-package eshell
  ;;  :hook (eshell-mode-hook . eshell-smart-initialize)
  :bind (:map ctl-z-map ("t" . eshell))
  :config
  (progn
    (setq eshell-where-to-jump 'begin
          eshell-review-quick-commands nil
          eshell-smart-space-goes-to-end t)
    (add-to-list 'eshell-modules-list 'eshell-tramp)))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-verbose 2
        vc-ignore-dir-regexp "\\`\\(?:[/\\][/\\][^/\\]+[/\\]\\|/\\(?:\\.\\.\\.\\|afs\\|net\\)/\\|/\\(?:a\\(?:db\\|fp\\)\\|d\\(?:avs?\\|oas\\)\\|f\\(?:[ct]p\\)\\|gdrive\\|k\\(?:rlogin\\|su\\)\\|mtp\\|n\\(?:c\\|extcloud\\)\\|p\\(?:linkx?\\|s\\(?:\\(?:c\\|ft\\)p\\)\\)\\|r\\(?:c\\(?:lone\\|p\\)\\|em\\(?:cp\\|sh\\)\\|s\\(?:h\\|ync\\)\\)\\|s\\(?:cpx?\\|ftp\\|mb\\|sh\\(?:fs\\|x\\)?\\|udo\\(?:edit\\)?\\|[gu]\\)\\|telnet\\):[^z-a]*\\)\\'"))

(use-package project
  :config
  (dolist (folder '("node_modules" "target" "out"
                    ".cljs_node_repl" ".shadow-cljs"))
    (add-to-list 'vc-directory-exclusion-list folder)))

(use-package vertico
  :ensure t
  :init (vertico-mode +1))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package dired
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-use-ls-dired nil
        dired-dwim-target t
        dired-listing-switches "-lha1v"))

(use-package rect
  :bind (:map ctl-z-map ("i" . string-insert-rectangle)))

(use-package text-mode
  :hook (text-mode-hook . turn-off-auto-fill))

(use-package eldoc-mode
  :hook (emacs-lisp-mode-hook . turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.3
        eldoc-echo-area-use-multiline-p nil))

(use-package expand-region
  :ensure t
  :bind (:map global-map ("C-=" . er/expand-region)))

(use-package xref
  :config (setq xref-search-program 'ripgrep))

(use-package browse-url
  :config
  (setq browse-url-browser-function 'eww
        eww-download-directory "~/papers"))

(defun bb/agenda (arg)
  (interactive "P")
  (if arg
      (call-interactively 'org-agenda)
    (org-agenda nil "n")))

(use-package org
  :ensure t
  :config (add-to-list 'org-modules 'org-habit t)
  :custom
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-habit-show-all-today t)
  (org-directory "/home/bex/Journal")
  (org-default-notes-file "/home/bex/Journal/todo.org"))

(use-package org-agenda
  :bind (:map ctl-z-map ("C-a" . bb/agenda))
  :custom
  (org-agenda-files '("/home/bex/Journal/")))

(use-package hl-line
  :init (global-hl-line-mode t))
