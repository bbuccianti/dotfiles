;;
;; Personal Emacs configuration
;; Benjamín Buccianti <benjamin@buccianti.dev>
;;

(use-package message
  :straight nil
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
	message-auto-save-directory "/home/bbuccianti/.mail/benjamin/Drafts"
	message-kill-buffer-on-exit t))

(use-package gnus
  :straight nil
  :demand
  :init (setq gnus-select-method '(nntp "news.gmane.io")
	      gnus-novice-user nil
	      gnus-secondary-select-methods
	      '((nnimap "todo"
			(nnimap-stream network)
			(nnimap-address "atom")
			(nnimap-authenticator login)
			(nnimap-user "todo")
			(nnir-search-engine imap))
		(nnimap "benjamin"
			(nnimap-stream network)
			(nnimap-address "atom")
			(nnimap-authenticator login)
			(nnimap-user "benjamin")
			(nnir-search-engine imap))
		(nnimap "gmail"
			(nnimap-stream network)
			(nnimap-address "atom")
			(nnimap-authenticator login)
			(nnimap-user "gmail")
			(nnir-search-engine imap)))))

(use-package gnus-sum
  :straight nil
  :after gnus
  :hook (gnus-summary-prepared . gnus-summary-hide-all-threads)
  :bind (:map gnus-summary-mode-map
	      ("F" . gnus-summary-wide-reply-with-original))
  :config (setq gnus-summary-line-format
		"%U%R%z%3t %I%(%[%4L: %-23,23f%]%) %s
"))

(use-package gnus-art
  :straight nil
  :after gnus
  :bind (:map gnus-article-mode-map
	      ("F" . gnus-summary-wide-reply-with-original)))

(use-package gnus-async
  :straight nil
  :after gnus
  :config (setq gnus-asynchronous t
		gnus-use-article-prefetch 15))

(use-package gnus-group
  :straight nil
  :after gnus)

(use-package gnus-topic
  :straight nil
  :after gnus
  :hook (gnus-group-mode . gnus-topic-mode))

(use-package rcirc
  :hook (rcirc-mode . (lambda ()
			(rcirc-track-minor-mode 1)
			(rcirc-omit-mode 1)))
  :config
  (setq rcirc-fill-column 'window-text-width
	rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
	rcirc-server-alist
        '(("192.168.0.10"
           :nick "bbuccianti"
           :port 12345
	   :user-name "bbuccianti/freenode"
	   :password "weakpass" ;; Local network only!
	   :server-alias "freenode")
	  ("192.168.0.10"
	   :nick "bbuccianti"
	   :port 12345
	   :user-name "bbuccianti/oftc"
	   :password "weakpass" ;; Local network only!
	   :server-alias "oftc"))))

(rcirc nil)
