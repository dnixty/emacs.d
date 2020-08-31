;;; Prerequisites
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))


;;; Visual
(require 'modus-operandi-theme)
(require 'modus-vivendi-theme)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)
(setq calendar-latitude 51.508530)
(setq calendar-longitude -0.076132)
;; Light at sunrise
(load-theme 'modus-operandi t t)
(run-at-time (nth 1 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda () (enable-theme 'modus-operandi)))
;; Dark at sunset
(load-theme 'modus-vivendi t t)
(run-at-time (nth 4 (split-string (sunrise-sunset)))
             (* 60 60 24)
             (lambda () (enable-theme 'modus-vivendi)))


;;; Base settings
(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq calendar-date-style 'iso)
(setq calendar-week-start-day 1)
(setq completion-ignore-case t)
(setq completion-show-help nil)
(setq create-lockfiles nil)
(setq custom-file
      (expand-file-name (format "emacs-custom-%s.el" (user-uid))
			temporary-file-directory))
(setq dired-listing-switches "-AFhl")
(setq history-length 30000)
(setq ibuffer-display-summary nil)
(setq ibuffer-show-empty-filter-groups nil)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 1)
(setq initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string)))
(setq ls-lisp-use-insert-directory-program nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq resize-mini-windows t)
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 1)
(setq scroll-error-top-bottom t)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq search-whitespace-regexp ".*?")
(setq select-enable-primary t)
(setq tab-always-indent 'complete)
(setq vc-follow-symlinks t)

(icomplete-mode)
(recentf-mode)
(save-place-mode)
(savehist-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)


;;; Programming languages
(setq c-default-style "bsd")


;;; Packages
(require 'ls-lisp)
(require 'dired-x)
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(when (require 'diff-hl nil t)
  (setq diff-hl-draw-borders nil)
  (add-hook 'after-init-hook 'global-diff-hl-mode))


;;; Key bindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "M-RET") 'eshell)
(global-set-key (kbd "C-c v") 'magit)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(let ((map icomplete-minibuffer-map))
  (define-key map (kbd "<tab>") 'minibuffer-force-complete)
  (define-key map (kbd "<return>") 'icomplete-force-complete-and-exit)
  (define-key map (kbd "C-n") 'icomplete-forward-completions)
  (define-key map (kbd "C-p") 'icomplete-backward-completions)
  (define-key map (kbd "C-j") 'exit-minibuffer))
(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)
