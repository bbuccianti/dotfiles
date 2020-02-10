;; -*- lexical-binding: t -*

(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      package--init-file-ensured t
      file-name-handler-alist nil
      auto-window-vscroll nil
      default-frame-alist '((font . "Hack-10"))
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      bidi-display-reordering nil
      inhibit-startup-screen t
      inhibit-default-init t
      inhibit-startup-message t
      initial-scratch-message nil
      create-lockfiles nil
      auto-save-default nil
      epg-gpg-program "gpg2"
      inferior-lisp-program "/usr/bin/sbcl"
      explicit-shell-file-name "/bin/sh")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(line-number-mode -1)
(column-number-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'emacs-startup-hook
          (let ((old-list file-name-handler-alist))
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)
              (setq file-name-handler-alist old-list
                    gc-cons-threshold 16777216 ; 16mb
                    gc-cons-percentage 0.1)
              (garbage-collect)))
          t)
