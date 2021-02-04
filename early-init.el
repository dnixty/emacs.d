;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar package-quickstart)

;; Allow loading from the package cache
(setq package-quickstart t)

;; Do not initialise the package manager.
(setq package-enable-at-startup nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;;; early-init.el ends here
