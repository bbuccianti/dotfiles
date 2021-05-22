;; -*- lexical-binding: t; -*-
;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(set-frame-font "Hack 8")

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
	gnus-secondary-select-methods
	'((nnimap "todo"
		  (nnimap-stream network)
		  (nnimap-address "atom")
		  (nnimap-authenticator login)
		  (nnimap-user "todo")
		  (nnmail-expiry-target "nnimap+todo:Trash")
		  (nnmail-expiry-wait 'immediate))
	  (nnimap "benjamin"
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
  :after gnus
  :init (setq gnus-search-use-parsed-queries t))

(use-package gnus-sum
  :after gnus
  :hook (gnus-summary-prepared-hook . gnus-summary-hide-all-threads)
  :bind (:map gnus-summary-mode-map
	      ("F" . gnus-summary-wide-reply-with-original))
  :config (setq gnus-summary-line-format
		"%U%R%z%3t %I%(%[%4L: %-23,23f%]%) %s
"))

(use-package gnus-art
  :after gnus
  :bind (:map gnus-article-mode-map
	      ("F" . gnus-summary-wide-reply-with-original)))

(use-package gnus-async
  :after gnus
  :config (setq gnus-asynchronous t
		gnus-use-article-prefetch 15))

(use-package gnus-group
  :after gnus)

(use-package gnus-topic
  :after gnus
  :hook (gnus-group-mode-hook . gnus-topic-mode))

(use-package rcirc
  :hook ((rcirc-mode-hook . rcirc-track-minor-mode)
	 (rcirc-mode-hook . rcirc-omit-mode)
	 (rcirc-mode-hook . (lambda ()
			      (set (make-local-variable 'scroll-conservatively)
				   8192))))
  :config
  (setq rcirc-fill-column 'window-text-width
	rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
	rcirc-server-alist
	`(("alonzo"
	   :nick "bbuccianti"
	   :port 6667
	   :user-name "bbuccianti/freenode"
	   :password ,(car (process-lines "pass" "irc/znc"))
	   :server-alias "freenode")
          ("alonzo"
	   :nick "bbuccianti"
	   :port 6667
	   :user-name "bbuccianti/libera"
	   :password ,(car (process-lines "pass" "irc/znc"))
	   :server-alias "libera"))))

(rcirc nil)

