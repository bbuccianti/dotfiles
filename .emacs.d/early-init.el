(defvar old--file-name-handler-alist file-name-handler-alist)

(setq-default gc-cons-threshold (* 100 1024 1024) ;; 100mb
	      gc-cons-percentage 0.6
	      package-enable-at-startup nil
	      package-quickstart nil
	      column-number-mode t
	      frame-inhibit-implied-resize t
	      site-run-file nil
	      initial-scratch-message nil
	      inhibit-startup-screen t
	      inhibit-default-init t
	      inhibit-startup-message t
	      bidi-display-reordering nil
	      bidi-inhibit-bpa t
	      bidi-paragraph-direction 'left-to-right
	      create-lockfiles nil
	      file-name-handler-alist nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(advice-add #'package--ensure-init-file :override #'ignore)
(advice-add #'x-apply-session-resources :override #'ignore)
