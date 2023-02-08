;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <bbuccianti@pm.me>
;;

(load-file "~/.emacs.d/useful.el")

(use-package bbdb
  :straight t)

(use-package message
  :hook (message-mode-hook . bbdb-insinuate-message)
  :config
  (setq smtpmail-debug-info nil ;; TOGGLE FOR DEBUG ONLY!
	mail-user-agent 'message-user-agent
	smtpmail-smtp-server "127.0.0.1"
	smtpmail-local-domain ""
	smtpmail-stream-type 'starttls
	smtpmail-smtp-service 1025
	message-send-mail-function 'smtpmail-send-it
	send-mail-function 'smtpmail-send-it
	message-default-mail-headers "Cc: \nBcc: \n"
	message-auto-save-directory "~/.mail/bbuccianti@pm.me/Drafts"
	message-kill-buffer-on-exit t))

(use-package gnus
  :bind (:map ctl-z-map ("g" . gnus))
  :hook (gnus-startup-hook . bbdb-insinuate-gnus)
  :config
  (setq gnus-select-method '(nntp "news.gmane.io")
	gnus-novice-user nil
	gnus-suppress-duplicates t
        gnus-use-full-window nil
        gnus-read-newsrc-file nil))

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

(use-package time
  :init (display-time-mode t)
  :config (setq display-time-24hr-format t))

(use-package rcirc
  :hook ((rcirc-mode-hook . rcirc-track-minor-mode)
	 (rcirc-mode-hook . rcirc-omit-mode))
  :config
  (setq ;;rcirc-fill-column 'window-text-width
	rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
        rcirc-time-format "%H:%M:%S "
	rcirc-server-alist
	`(("chat.sr.ht"
	   :nick "fold"
	   :port 6697
	   :user-name "bbuccianti/libera@rcirc"
	   :password ,(car (process-lines "pass" "chat.sr.ht"))
           :server-alias "libera"
           :encryption tls))))

(use-package xclip
  :straight t
  :init (xclip-mode 1))

(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (set-face-font 'default "Hack 8")
              (rcirc nil)
              (switch-to-buffer "#emacs@libera")
              (split-window-horizontally)
              (other-window 1)
              (switch-to-buffer "#argentina@libera"))))
