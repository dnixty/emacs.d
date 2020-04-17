;;; init.el --- Emacs configuration file.

;;; Table of contents
;;   1. Prerequisites
;;   2. Base settings
;;   3. Window manager
;;   4. Selection narrowing and search
;;   5. General interface
;;   6. Programming languages
;;   7. Applications and utilities

;;; --------------------------------------------------------------------
;;; 1. Prerequisites
;;; --------------------------------------------------------------------

;; Setup use-package
(eval-when-compile
  (require 'use-package))

;; Move user-emacs-directory
(use-package emacs
  :config
  (setq user-emacs-directory "~/.cache/emacs/"))


;;; --------------------------------------------------------------------
;;; 2. Base settings
;;; --------------------------------------------------------------------

;; Edit modeline "lighters"
(use-package delight
  :pin manual
  :after use-package)

;; Don't let `customize' clutter the config
(use-package cus-edit
  :config
  (setq custom-file
        (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                          temporary-file-directory)))

;; Base typeface configurations
(use-package emacs
  :config
  (defun dnixty/laptop-font ()
    "Font for the small laptop screen."
    (interactive)
    (when window-system
      (set-frame-font "Hack-11:hintstyle=hintslight" t t)))
  (defun dnixty/desktop-font ()
    "Font for the larger desktop screen."
    (interactive)
    (when window-system
      (set-frame-font "Hack-14:hintstyle=hintslight" t t)))
  (defun dnixty/set-font ()
    (when window-system
      (if (<= (display-pixel-width) 1366)
          (dnixty/laptop-font)
        (dnixty/desktop-font))))
  :hook (window-setup . dnixty/set-font))

;; Unique names for buffers
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Record cursor position
(use-package saveplace
  :config
  (save-place-mode)
  :hook (before-save save-place-kill-emacs-hook))

;; Backups
(use-package emacs
  :config
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq create-lockfiles nil))

;; Disable autosave features
(use-package emacs
  :init
  (setq auto-save-default nil)
  (setq auto-save-list-file-prefix nil))


;;; --------------------------------------------------------------------
;;; 3. Window manager
;;; --------------------------------------------------------------------

;; Exwm
(use-package exwm
  :config
  (defun dnixty/exwm-rename-buffer-to-title ()
    (exwm-workspace-rename-buffer exwm-title))
  (defun dnixty/capture-screen ()
    (interactive)
    (start-process "" nil "flameshot" "gui"))
  (defun dnixty/suspend-to-sleep ()
    (interactive)
    (call-process "loginctl" nil nil nil "lock-session"))
  (defun dnixty/switch-to-other ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  ;; Make sure that XF86 keys work in exwm buffers as well
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86AudioMute
               print))
  (cl-pushnew k exwm-input-prefix-keys))
  ;; Global keys
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-B") #'helm-filtered-bookmarks)
  (exwm-input-set-key (kbd "s-c") #'helm-resume)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-g") #'magit-status)
  (exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
  (exwm-input-set-key (kbd "s-k") #'kill-this-buffer)
  (exwm-input-set-key (kbd "s-o") #'other-window)
  (exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "s-P") #'helm-pass)
  (exwm-input-set-key (kbd "s-r") #'helm-run-external-command)
  (exwm-input-set-key (kbd "s-R") #'exwm-reset)
  (exwm-input-set-key (kbd "s-Z") #'dnixty/suspend-to-sleep)
  (exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)
  (exwm-input-set-key (kbd "s-<return>") #'eshell)
  (exwm-input-set-key (kbd "s-<tab>") #'dnixty/switch-to-other)
  (exwm-input-set-key (kbd "C-s-b") #'windmove-left)
  (exwm-input-set-key (kbd "C-s-n") #'windmove-down)
  (exwm-input-set-key (kbd "C-s-p") #'windmove-up)
  (exwm-input-set-key (kbd "C-s-f") #'windmove-right)
  (exwm-input-set-key (kbd "<print>") #'dnixty/capture-screen)
  ;; Simulation keys
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\M-b] . C-left)
     ([?\C-f] . right)
     ([?\M-f] . C-right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))
     ([?\C-g] . escape)
     ([?\M-n] . ?\C-n)
     ([?\M-a] . ?\C-a)
     ([?\M-k] . ?\C-w)
     ([?\C-w] . ?\C-x)
     ([?\M-w] . ?\C-c)
     ([?\C-y] . ?\C-v)
     ([?\C-s] . ?\C-f)))
  ;; Force exwm to manage specific windows
  (add-to-list 'exwm-manage-configurations
               '((string= exwm-title "Wasabi Wallet") managed t))
  :bind (("C-x C-c" . save-buffers-kill-emacs))
  :hook ((exwm-update-title . dnixty/exwm-rename-buffer-to-title)
         (exwm-floating-setup . exwm-layout-hide-mode-line)
         (exwm-floating-exit . exwm-layout-show-mode-line)))

(use-package exwm-randr
  :after exwm
  :commands exwm-randr-enable
  :demand t
  :config
  (defun dnixty/exwm-change-screen-hook ()
    (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
          (xrandr-monitor-regexp "\n .* \\([^ \n]+\\)")
          default-output)
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (re-search-forward xrandr-output-regexp nil 'noerror)
        (setq default-output (match-string 1))
        (forward-line)
        (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
            (progn
              (call-process "xrandr" nil nil nil "--output" default-output "--auto" "--primary")
              (with-temp-buffer
                (call-process "xrandr" nil t nil "--listactivemonitors")
                (goto-char (point-min))
                (while (not (eobp))
                  (when (and (re-search-forward xrandr-monitor-regexp nil 'noerror)
                             (not (string= (match-string 1) default-output)))
                    (call-process "xrandr" nil nil nil "--output" (match-string 1) "--auto")))))
          (call-process
           "xrandr" nil nil nil
           "--output" (match-string 1) "--primary" "--auto"
           "--output" default-output "--off")
          (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1))))
        (dnixty/set-font))))
  (exwm-randr-enable)
  :hook (exwm-randr-screen-change . dnixty/exwm-change-screen-hook))

(use-package helm-exwm
  :config
  (use-package helm-bookmark)
  (add-to-list 'helm-source-names-using-follow "EXWM buffers")
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found)))

;; Pulseaudio
(use-package pulseaudio-control
  :after exwm
  :config
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "2%")
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute))

(use-package frame
  :after exwm
  :config
  (setq window-divider-default-bottom-width 2)
  (setq window-divider-default-right-width 2)
  (window-divider-mode))


;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------

;; Helm
(use-package helm-config)

(use-package helm
  :delight
  :config
  (global-unset-key (kbd "C-x c"))
  (setq helm-reuse-last-window-split-state t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-grep-save-buffer-name-no-confirm t)
  (setq helm-buffers-end-truncated-string "…")
  (setq helm-buffer-max-length 22)
  (setq helm-window-show-buffers-function 'helm-window-mosaic-fn)
  (setq helm-split-window-default-side 'right)
  (setq helm-window-prefer-horizontal-split t)
  (setq helm-completion-style "helm-flex")
  :bind (("C-c h" . helm-command-prefix)
         ([remap execute-extended-command] . helm-M-x)
         ([remap find-file] . helm-find-files)
         ([remap occur] . helm-occur)
         ([remap list-buffers] . helm-mini)
         ([remap yank-pop] . helm-show-kill-ring)
         ([remap apropos-command] . helm-apropos)
         ([remap query-replace-regexp] . helm-regexp)
         ("C-h SPC" . helm-all-mark-rings)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :hook (after-init . helm-mode))

;; Helm descbinds
(use-package helm-descbinds
  :after helm
  :hook (after-init . helm-descbinds-mode))

;; Eshell
(use-package helm-eshell
  :config
  (defun dnixty/helm/eshell-set-keys ()
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
    (define-key eshell-mode-map (kbd "M-s") nil)
    (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all))
  :hook (eshell-mode . dnixty/helm/eshell-set-keys))

;; Slime
(use-package helm-slime
  :requires slime
  :config
  (defun dnixty/helm/slime-set-keys ()
    (define-key slime-repl-mode-map (kbd "M-p") 'helm-slime-repl-history)
    (define-key slime-repl-mode-map (kbd "M-s") nil)
    (define-key slime-repl-mode-map (kbd "M-s f") 'helm-comint-prompts-all)
    (define-key slime-autodoc-mode-map (kbd "C-c C-d C-a") 'helm-slime-apropos)
    (define-key slime-repl-mode-map (kbd "C-c C-x c") 'helm-slime-list-connections)
    (define-key slime-repl-mode-map (kbd "M-<tab>") 'helm-slime-complete))
  :hook (slime-repl-mode . dnixty/helm/slime-set-keys))

;; Projectile
(use-package projectile
  :delight
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (setq projectile-project-search-path '("~/src"))
  (projectile-mode))

(use-package helm-projectile
  :after projectile
  :config
  (setq projectile-switch-project-action 'helm-projectile)
  (helm-projectile-on))


;;; --------------------------------------------------------------------
;;; 5. General interface
;;; --------------------------------------------------------------------

;; Disable GUI components
(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t)
  (setq inhibit-startup-screen t)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h")))

;; Theme
(use-package color-theme-sanityinc-tomorrow
  :init
  (setq custom-safe-themes t)
  (setq-default custom-enabled-themes '(sanityinc-tomorrow-day))
  :config
  (defun dnixty/reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
  (defun dnixty/theme-toggle ()
    "Toggle between sanityinc-tomorrow themes."
    (interactive)
    (cond ((eq (car custom-enabled-themes) 'sanityinc-tomorrow-day)
           (load-theme 'sanityinc-tomorrow-night))
          ((eq (car custom-enabled-themes) 'sanityinc-tomorrow-night)
           (load-theme 'sanityinc-tomorrow-bright))
          ((eq (car custom-enabled-themes) 'sanityinc-tomorrow-bright)
           (load-theme 'sanityinc-tomorrow-day))))
  :hook (after-init . dnixty/reapply-themes)
  :bind ([f5] . dnixty/theme-toggle))

;; Initial scratch message
(use-package emacs
  :config
  (setq initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))))

;; Generic feedback
(use-package emacs
  :config
  (setq echo-keystrokes 0.25)
  (defalias 'yes-or-no-p 'y-or-n-p))

;; Line length (column count)
(use-package emacs
  :config
  (setq-default fill-column 72)
  (setq column-number-mode t)
  :hook (after-init . column-number-mode))

;; Tabs, indentation, and the TAB key
(use-package emacs
  :config
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil))

;; Auto revert mode
(use-package autorevert
  :mode ("\\.log\\'" . auto-revert-tail-mode)
  :delight
  :hook (after-init . global-auto-revert-mode))

;; Preserve contents of system clipboard
(use-package emacs
  :config
  (setq select-enable-primary t)
  (setq save-interprogram-paste-before-kill t))

;; Scrolling behaviour
(use-package emacs
  :config
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 1)
  (setq scroll-margin 0)
  (setq scroll-error-top-bottom t))

;; Comenting lines
(use-package emacs
  :bind ("M-;" . comment-line))

;; Trailing whitespace
(use-package emacs
  :hook ((before-save . whitespace-cleanup)
         (before-save . (lambda () (delete-trailing-whitespace)))))

;; Parentheses
(use-package paredit
  :delight
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))
(use-package paren
  :config
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-delay 0)
  :hook (after-init . show-paren-mode))
(use-package slime
  :config
  (defun dnixty/override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  :hook ((slime-repl-mode . dnixty/override-slime-repl-bindings-with-paredit)
         (slime-repl-mode . (lambda () (paredit-mode +1)))))
(use-package rainbow-delimiters
  :pin manual
  :init
  :hook
  ((emacs-lisp-mode-hook
    ielm-mode-hook
    lisp-mode-hook)
   . rainbow-delimiters-mode))

;; Display current time
(use-package time
  :config
  (setq display-time-format "%H:%M  %Y-%m-%d")
  (setq display-time-default-load-average nil)
  (display-time-mode))

;; Battery status
(use-package battery
  :config
  (setq battery-mode-line-format "  [%b%p%%]")
  (setq battery-mode-line-limit 99)
  (setq battery-update-interval 180)
  (setq battery-load-low 20)
  (setq battery-load-critical 10)
  (display-battery-mode))

;; Kill whole line including \n.
(use-package emacs
  :config
  (setq kill-whole-line t))

;; Newline characters for file ending
(use-package emacs
  :config
  (setq mode-require-final-newline 'visit-save))

;; Mouse behaviour
(use-package mouse
  :config
  (setq mouse-autoselect-window t)
  (setq make-pointer-invisible t))

;; Expand Region
(use-package expand-region
  :pin manual
  :config
  (setq expand-region-smart-cursor t)
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/mark-outside-pairs)
         ("C-+" . er/mark-symbol)))

;; Eldoc
(use-package eldoc
  :delight
  :config
  (global-eldoc-mode 1))

;; Collection of unpackaged commands or tweaks
(use-package emacs
  :config
  (setq split-height-threshold nil)
  (setq split-width-threshold 130)
  :bind ("C-x k" . kill-this-buffer))


;;; --------------------------------------------------------------------
;;; 6. Programming languages
;;; --------------------------------------------------------------------

;;; Nix Mode
(use-package nix-mode)


;;; --------------------------------------------------------------------
;;; 7. Applicatons and utilities
;;; --------------------------------------------------------------------

;; Calendar
(use-package calendar
  :config
  (setq calendar-week-start-day 1)
  (setq calendar-date-style 'iso)
  (setq calendar-latitude 51.508530)
  (setq calendar-longitude -0.076132)
  :hook (calendar-today-visible . calendar-mark-today))

;; Org
(use-package org
  :config
  (setq org-hide-leading-stars t))

;; Encryption
(use-package pinentry
  :config
  (setq-default epa-pinentry-mode 'loopback)
  :hook (after-init . pinentry-start))

;; Eshell
(use-package eshell
  :config
  (setq eshell-history-size 1024)
  (setq eshell-hist-ignoredups t)
  (setq eshell-destroy-buffer-when-process-dies t))
(use-package esh-module
  :after eshell
  :config
  (delq 'eshell-banner eshell-modules-list))

;; Helm Pass
(use-package helm-pass)

;; Ledger
(use-package ledger-mode
  :pin manual
  :mode "\\.ldg$"
  :init)

;; Magit
(use-package magit)

;; Slime
(use-package slime
  :pin manual
  :commands slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--noinform"))
          (clisp ("clisp"))))
  (slime-setup '(slime-fancy)))
(use-package helm-slime
  :pin manual
  :after slime
  :config
  (global-helm-slime-mode))

;;; Pdf
(use-package pdf-tools
  :pin manual)
(use-package pdf-occur
  :pin manual
  :after pdf-tools
  :config
  (pdf-tools-install))


;;; init.el ends here
