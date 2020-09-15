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
	message-kill-buffer-on-exit t
	message-directory "/home/bbuccianti/.mail/benjamin/Sent"))

(use-package notmuch
  :bind (:map ctl-z-map ("@" . notmuch))
  :config
  (setq	notmuch-search-oldest-first nil
	notmuch-show-indent-content nil
	notmuch-show-logo nil
	notmuch-show-all-tags-lst t
	notmuch-hello-sections '(notmuch-hello-insert-header
				 notmuch-hello-insert-inbox
				 notmuch-hello-insert-footer)
	notmuch-fcc-dirs
	'(("benjamin@buccianti.dev" . "benjamin/Sent -inbox -unread +sent"))))

(use-package gnus
  :straight nil
  :config (setq gnus-select-method '(nntp "news.gmane.io")))

(use-package rcirc
  :hook (rcirc-mode . (lambda () (set (make-local-variable 'scroll-conservatively) 8192)))
  :config
  (setq rcirc-fill-column 'window-text-width
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

(use-package init-irc
  :straight nil
  :init
  (progn
    (rcirc nil)
    (rcirc-track-minor-mode 1)))
