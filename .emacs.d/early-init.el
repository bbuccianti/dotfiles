;; -*- lexical-binding: t; -*-

(defvar old--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      horizontal-scroll-bar-mode nil
      line-number-mode nil
      column-number-mode nil
      frame-inhibit-implied-resize t
      site-run-file nil)

(advice-add #'package--ensure-init-file :override #'ignore)
(advice-add #'x-apply-session-resources :override #'ignore)
