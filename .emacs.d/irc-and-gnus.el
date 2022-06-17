;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

;;(set-frame-font "Hack 8")

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
              tab-always-indent 'complete
              resize-mini-windows nil
              completion-ignore-case t
              completion-category-defaults nil
              completion-category-overrides '((file (styles . (partial-completion))))
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t)

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
(define-key ctl-x-map  (kbd "g")	#'magit-status)

(define-key ctl-z-map  (kbd "r")	#'compile)
(define-key ctl-z-map  (kbd "C-r")	#'recompile)

(menu-bar-mode 0)

(straight-use-package 'use-package)

(use-package icomplete
  :init (fido-mode))

(use-package expand-region
  :straight t
  :bind (:map global-map ("C-=" . er/expand-region)))

(use-package bbdb
  :straight t)

(use-package message
  :hook (message-mode-hook . bbdb-insinuate-message)
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
	message-auto-save-directory "~/.mail/benjamin/Drafts"
	message-kill-buffer-on-exit t))

(use-package gnus
  :bind (:map ctl-z-map ("g" . gnus))
  :hook (gnus-startup-hook . bbdb-insinuate-gnus)
  :config
  (setq gnus-select-method '(nntp "news.gmane.io")
	gnus-novice-user nil
	gnus-suppress-duplicates t
        gnus-use-full-window nil
        gnus-read-newsrc-file nil
	gnus-secondary-select-methods
	'((nnimap "benjamin"
		  (nnimap-stream network)
		  (nnimap-address "atom")
		  (nnimap-authenticator login)
		  (nnimap-user "benjamin")
		  (nnmail-expiry-target "nnimap+benjamin:Trash")
			(nnmail-expiry-wait 'immediate))
	  (nnimap "gmail"
		  (nnimap-stream network)
		  (nnimap-address "atom")
		  (nnimap-authenticator login)
		  (nnimap-user "gmail")
		  (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
		  (nnmail-expiry-wait 'immediate)))))

(use-package gnus-search
  :init (setq gnus-search-use-parsed-queries t))

(use-package gnus-sum
  :hook (gnus-summary-prepared-hook . gnus-summary-hide-all-threads)
  :bind (:map gnus-summary-mode-map
              ("F" . gnus-summary-wide-reply-with-original))
  :config (setq gnus-summary-line-format
		"%U%R%z%3t %I%(%[%4L: %-23,23f%]%) %s
"))

(use-package gnus-async
  :init (setq gnus-asynchronous t
	      gnus-use-article-prefetch 15))

(use-package gnus-topic
  :hook (gnus-group-mode-hook . gnus-topic-mode))

(use-package rcirc
  :hook ((rcirc-mode-hook . rcirc-track-minor-mode)
	 (rcirc-mode-hook . rcirc-omit-mode))
  :config
  (setq rcirc-fill-column 'window-text-width
	rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
        rcirc-time-format "%H:%M:%S "
	rcirc-server-alist
	`(("chat.sr.ht"
	   :nick "roto"
	   :port 6697
	   :user-name "bbuccianti/libera@rcirc"
	   :password ,(car (process-lines "pass" "chat.sr.ht"))
           :encryption tls))))

(rcirc nil)

;; (use-package circe
;;   :straight t
;;   :config
;;   (setq circe-network-options
;;         `(("chat.sr.ht"
;;            :tls t
;;            :nick "roto"
;;            :user "bbuccianti/libera@rcirc"
;;            :pass ,(car (process-lines "pass" "chat.sr.ht"))
;;            :host "chat.sr.ht"
;;            :port "6697"))
;;         circe-reduce-lurker-spam t
;;         circe-format-say "<{nick}> {body}"
;;         lui-time-stamp-position nil)
;;   (enable-circe-color-nicks))

;; (circe "chat.sr.ht")
