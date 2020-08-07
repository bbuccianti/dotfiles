;; -*- lexical-binding: t; -*-

(defvar old--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold (* 100 1024 1024) ;; 100mb
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      menu-bar-mode nil
      tool-bar-mode nil
      line-number-mode t
      column-number-mode t
      frame-inhibit-implied-resize t
      site-run-file nil
      initial-scratch-message nil
      inhibit-startup-screen t
      inhibit-default-init t
      inhibit-startup-message t
      bidi-display-reordering nil
      create-lockfiles nil)

(advice-add #'package--ensure-init-file :override #'ignore)
(advice-add #'x-apply-session-resources :override #'ignore)
