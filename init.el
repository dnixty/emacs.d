;;; Commentary:

;;; Table of contents
;;   1. Prerequisites
;;   2. Visual settings
;;   3. Base settings
;;   4. Selection narrowing and search
;;   5. Aliases and custom commands
;;   6. Programming languages
;;   7. Applications and utilities


;;; --------------------------------------------------------------------
;;; 1. Prerequisites
;;; --------------------------------------------------------------------

;; Setup package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))
(eval-when-compile
  (require 'use-package))

;; Move user-emacs-directory
(use-package emacs
  :config
  (setq user-emacs-directory "~/.cache/emacs/"))

;; Make Emacs use shell $PATH
(use-package exec-path-from-shell
  :ensure
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))



;;; --------------------------------------------------------------------
;;; 2. Visual settings
;;; --------------------------------------------------------------------

;; Disable GUI components
(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq inhibit-startup-screen t)
  (blink-cursor-mode -1))

(use-package modus-operandi-theme
  :demand t
  :ensure)
(use-package modus-vivendi-theme
  :demand t
  :ensure)

(use-package emacs
  :after (modus-operandi-theme modus-vivendi-theme)
  :init
  (setq custom-safe-themes t)
  (setq calendar-latitude 51.508530)
  (setq calendar-longitude -0.076132)
  ;; Light at sunrise
  (run-at-time (nth 1 (split-string (sunrise-sunset)))
               (* 60 60 24)
               (lambda () (enable-theme 'modus-operandi)))
  ;; Dark at sunset
  (run-at-time (nth 4 (split-string (sunrise-sunset)))
               (* 60 60 24)
               (lambda () (enable-theme 'modus-vivendi))))



;;; --------------------------------------------------------------------
;;; 3. Base settings
;;; --------------------------------------------------------------------

(use-package emacs
  :config
  (setq-default tab-always-indent 'complete)
  (setq-default indent-tabs-mode nil)
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (setq create-lockfiles nil)
  (setq custom-file
      (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                        temporary-file-directory))
  (setq initial-scratch-message (format ";; Scratch - Started on %s\n\n"
                                        (current-time-string)))
  (setq scroll-conservatively 1)
  (setq scroll-error-top-bottom t)
  (setq scroll-margin 0)
  (setq scroll-preserve-screen-position t)
  (setq select-enable-primary t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  :hook ((before-save-hook . whitespace-cleanup)
         (before-save-hook . (lambda () (delete-trailing-whitespace)))))

;; Recentf
(use-package recentf
  :config
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  :hook (after-init-hook . recentf-mode)
  :bind ("C-x C-r" . recentf-open-files))

;; Record cursor position
(use-package saveplace
  :config
  (save-place-mode))

;; Mouse
(use-package mouse
  :config
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((control) . text-scale)))
  :hook (after-init-hook . mouse-wheel-mode))

;; Winner mode
(use-package winner
  :hook (after-init-hook . winner-mode)
  :bind (("C->" . winner-redo)
         ("C-<" . winner-undo)))



;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------


(use-package minibuffer
  :config
  (setq completion-ignore-case t)
  (setq completion-show-help nil)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq resize-mini-windows t))

;; Minibuffer history
(use-package savehist
  :config
  (setq history-length 30000)
  (savehist-mode 1))

(use-package icomplete
  :demand
  :after minibuffer
  :config
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 1)
  (fido-mode -1)
  (icomplete-mode 1)
  :bind (:map icomplete-minibuffer-map
              ("<tab>" . icomplete-force-complete)
              ("<return>" . icomplete-force-complete-and-exit)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-l" . icomplete-fido-backward-updir)
              ("C-j" . exit-minibuffer)))

(use-package icomplete-vertical
  :ensure
  :demand
  :config
  (defun dps/icomplete-yank-kill-ring ()
    (interactive)
    (let ((kills
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do (:separator 'dotted-line)
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))
  :bind (("M-y" . dps/icomplete-yank-kill-ring)))

(use-package ibuffer
  :config
  (setq ibuffer-display-summary nil)
  (setq ibuffer-show-empty-filter-groups nil)
  :bind ("C-x C-b" . ibuffer))

(use-package isearch
  :config
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited))

(use-package rg
  :ensure
  :defer
  :config
  (rg-define-search
   dps/rg-vc-or-dir
   "RipGrep in project root or present directory."
   :query ask
   :format regexp
   :files "everything"
   :dir (let ((vc (vc-root-dir)))
          (if vc
              vc
            default-directory))
   :confirm prefix
   :flags ("--hidden -g !.git"))
  (rg-define-search
   dps/rg-ref-in-dir
   "RipGrep for thing at point in present directory."
   :query point
   :format regexp
   :files "everything"
   :dir default-directory
   :confirm prefix
   :flags ("--hidden -g !.git"))

  :bind (("C-c g" . dps/rg-vc-or-dir)
         ("C-C r" . dps/rg-ref-in-dir)))



;;; --------------------------------------------------------------------
;;; 5. Aliases and custom commands
;;; --------------------------------------------------------------------

(use-package emacs
  :bind (("C-h" . delete-backward-char)
         ("C-x k" . kill-this-buffer)
         ("M-;" . comment-line)
         ("C-," . previous-buffer)
         ("C-." . next-buffer)
         ("M-RET" . eshell)))



;;; --------------------------------------------------------------------
;;; 6. Programming
;;; --------------------------------------------------------------------

;; Javascript
(use-package js
  :config
  (setq js-indent-level 2))
(use-package add-node-modules-path
  :ensure
  :hook (js-mode-hook . add-node-modules-path))
(use-package tide
  :ensure
  :mode ("\\.tsx?\\'" . typescript-mode)
  :hook ((typescript-mode-hook . tide-setup)
         (typescript-mode-hook . flycheck-mode)
         (typescript-mode-hook . tide-hl-identifier-mode)))
(use-package flow-minor-mode
  :ensure
  :hook (js-mode-hook . flow-minor-enable-automatically))

;; Haskell
(use-package haskell-mode
  :ensure)

;; Diff-hl
(use-package diff-hl
  :ensure
  :config
  (setq diff-hl-draw-borders nil)
  :hook (after-init-hook . global-diff-hl-mode))

;; Magit
(use-package magit
  :ensure
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :bind ("C-c v" . magit))

;; Flycheck
(use-package flycheck-flow
  :ensure)
(use-package flycheck
  :ensure
  :config
  ;; (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  :init (global-flycheck-mode))



;;; --------------------------------------------------------------------
;;; 7. Applicatons and utilities
;;; --------------------------------------------------------------------

;; Calendar
(use-package calendar
  :config
  (setq calendar-date-style 'iso)
  (setq calendar-week-start-day 1))

;; Dired
(use-package dired
  :config
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode))
  :bind (:map dired-mode-map
              ("C-l" . dired-up-directory)))
(use-package dired-x)

;; OpenBSD
;; (setq dired-listing-switches "-AFhl")
;; (setq ls-lisp-use-insert-directory-program nil)
;; (setq ring-bell-function 'ignore)
;; (setq vc-follow-symlinks t)
;; (require 'ls-lisp)
