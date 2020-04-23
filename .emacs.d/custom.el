(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-show-key-bindings nil)
 '(company-quickhelp-color-background "#3E4452")
 '(company-quickhelp-color-foreground "#ABB2BF")
 '(custom-safe-themes
   '("4e839b24f87c529e837535d0a7880f40ac3867b6e3e73a2cf2bb40bab53d4658" "fcc707b42f328f7b842941e6a1396089262cfac9f7799176976392efb9a306c8" "ed91d4e59412defda16b551eb705213773531f30eb95b69319ecd142fab118ca" "687e997f50a47c647c5132f0671df27b8a3ff4f18e31210dc53abeaa7ea8cde3" "a67b6cb65db241e033b6aed5eeaf0805a1b62e598cedc605c71d003a1d5c00c6" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "b73a23e836b3122637563ad37ae8c7533121c2ac2c8f7c87b381dd7322714cd0" "0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" default))
 '(elfeed-feeds
   '("http://planet.clojure.in/atom.xml" "https://planet.emacslife.com/atom.xml" "http://planet.kernel.org/rss20.xml" "http://planet.lisp.org/rss20.xml" "https://twobithistory.org/feed.xml") t)
 '(exec-path-from-shell-check-startup-files nil)
 '(fill-column 72)
 '(fzf/args "-x --ansi --color 16 --print-query")
 '(gofmt-command "/usr/bin/gofmt" t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-use-filename-at-point nil)
 '(ido-vertical-define-keys 'C-n-and-C-p-only)
 '(magit-repository-directories '(("~/work" . 2) ("~/src" . 3)))
 '(mail-user-agent 'message-user-agent)
 '(message-auto-save-directory "~/.mail/drafts")
 '(message-default-mail-headers "Cc: 
Bcc: 
")
 '(message-directory "~/.mail/sent")
 '(message-kill-buffer-on-exit t)
 '(message-send-mail-function 'message-smtpmail-send-it)
 '(mu4e-get-mail-command "mbsync -a")
 '(mu4e-maildir "~/.mail" t)
 '(mu4e-user-mail-address-list '("benjamin@buccianti.dev") t)
 '(mu4e-view-show-addresses t)
 '(org-agenda-compact-blocks nil)
 '(org-agenda-files
   '("~/inspt/sistemas-computacion-2/final/todo.org" "~/src/sourcehut/bbuccianti/lambda/todo.org" "~/src/sourcehut/bbuccianti/fp/todo.org" "~/src/sourcehut/bbuccianti/buccianti.dev/todo.org"))
 '(org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . "%T: ")
     (tags . "%l")
     (search . " %i %-12:c")))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-window-setup 'current-window)
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "~/org/inbox.org")
      "* TODO %?
 %u
")) t)
 '(org-default-notes-file "~/org/inbox.org")
 '(org-export-async-init-file "~/.emacs.d/org-init.el" t)
 '(org-hide-leading-stars t)
 '(org-latex-toc-command "\\tableofcontents \\clearpage" t)
 '(org-modules '(ol-info ol-mhe ol-rmail))
 '(org-src-preserve-indentation t)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(pug-tab-width 2 t)
 '(search-whitespace-regexp ".*")
 '(smtpmail-debug-info nil)
 '(smtpmail-default-smtp-server "mail.buccianti.dev")
 '(smtpmail-local-domain "buccianti.dev")
 '(smtpmail-smtp-server "mail.buccianti.dev")
 '(whitespace-line-column 80)
 '(whitespace-style '(face lines-tail trailing space-before-tab)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-base-face ((t (:inherit nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#56B6C2" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#D19A66" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#61AFEF" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#C678DD" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#98C379" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#BE5046" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#56B6C2" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#E5C07B" :weight bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#61AFEF" :weight bold))))
 '(selectrum-current-candidate ((t (:background "#222226"
						:weight bold
						:foreground "gainsboro"))))
 '(selectrum-primary-highlight ((t (:foreground "#c56ec3"))))
 '(selectrum-secondary-highlight ((t (:foreground "#2d9574")))))
