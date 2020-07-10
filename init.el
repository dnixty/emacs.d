;;; init.el --- Emacs configuration file.

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

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

;; Setup use-package
(eval-when-compile
  (require 'use-package))

;; Enable modeline diminishers
(use-package diminish)

;; Move user-emacs-directory
(use-package emacs
  :config
  (setq user-emacs-directory "~/.cache/emacs/"))



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
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (setq use-file-dialog nil)
  (setq use-dialog-box t)
  (setq inhibit-startup-screen t))

;; Font configuration
(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "Hack 11")))

;; Theme
(use-package modus-operandi-theme
  :demand t)
(use-package modus-vivendi-theme
  :demand t)
(use-package emacs
  :after (modus-operandi-theme modus-vivendi-theme)
  :init
  (setq custom-safe-themes t)
  (setq-default custom-enabled-themes '(modus-operandi))
  :config
  (defun dnixty/reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
  (defun dnixty/modus-operandi ()
    "Load Modus Operandi theme."
    (setq custom-enabled-themes '(modus-operandi))
    (dnixty/reapply-themes))
  (defun dnixty/modus-vivendi ()
    "Load Modus Vivendi theme."
    (setq custom-enabled-themes '(modus-vivendi))
    (dnixty/reapply-themes))
  (defun dnixty/theme-toggle ()
    "Toggle between Modus Vivendi and Modus Operandi themes."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (dnixty/modus-vivendi)
      (dnixty/modus-operandi)))
  :hook (after-init-hook . dnixty/modus-operandi)
  :bind ([f5] . dnixty/theme-toggle))



;;; --------------------------------------------------------------------
;;; 3. Base settings
;;; --------------------------------------------------------------------

;; Don't let `customize' clutter the config
(use-package cus-edit
  :config
  (setq custom-file
        (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                          temporary-file-directory)))

;; Unique names for buffers
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Record cursor position
(use-package saveplace
  :config
  (save-place-mode))

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

;; Mouse behaviour
(use-package mouse
  :config
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta))
          ((control) . text-scale)))
  (setq make-pointer-invisible t)
  (tooltip-mode -1))

;; Turn off large file warning threshold
(use-package emacs
  :config
  (setq large-file-warning-threshold nil))

;; Recentf
(use-package recentf
  :config
  (setq recentf-max-saved-items 200)
  (defun dnixty/recentf ()
    (interactive)
    (let* ((files (mapcar 'abbreviate-file-name recentf-list))
           (f (icomplete-vertical-do ()
                (completing-read "Open recentf entry: " files nil t))))
      (find-file f)))
  :hook (after-init-hook . recentf-mode)
  :bind ("s-r" . dnixty/recentf))

;; Bookmarks
(use-package bookmark
  :config
  (setq bookmark-save-flag 1))

;; Initial scratch message
(use-package emacs
  :config
  (setq initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))))

;; Generic feedback
(use-package emacs
  :config
  (setq echo-keystrokes 0.25)
  (defalias 'yes-or-no-p 'y-or-n-p))

;; Line length
(use-package emacs
  :config
  (setq-default fill-column 72)
  (setq column-number-mode t)
  :hook (after-init-hook . column-number-mode))

;; Tabs, indentation, and the TAB key
(use-package emacs
  :config
  (setq-default tab-always-indent 'complete)
  (setq-default indent-tabs-mode nil))

;; Auto revert mode
(use-package autorevert
  :mode ("\\.log\\'" . auto-revert-tail-mode)
  :hook (after-init-hook . global-auto-revert-mode))

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
  :hook ((before-save-hook . whitespace-cleanup)
         (before-save-hook . (lambda () (delete-trailing-whitespace)))))

;; Newline characters for file ending
(use-package emacs
  :config
  (setq mode-require-final-newline 'visit-save))

;; Expand Region
(use-package expand-region
  :config
  (setq expand-region-smart-cursor t)
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/mark-outside-pairs)
         ("C-+" . er/mark-symbol)))

;; Eldoc
(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

;; Winner mode
(use-package winner
  :hook (after-init-hook . winner-mode)
  :bind (("C-s-." . winner-redo)
         ("C-s-," . winner-undo)))

;; Delete selection
(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

;; Splits
(use-package emacs
  :config
  (setq split-height-threshold nil)
  (setq split-width-threshold 130))

;; Go to last change
(use-package goto-last-change
  :config
  (global-unset-key (kbd "C-z"))
  :bind ("C-z" . goto-last-change))

;; Hippie Expand
(use-package hippie-exp
  :config
  ;; Remove line and list expand functions from hippie-expand as they
  ;; cause problems with paredit.
  (dolist (f '(try-expand-line try-expand-list))
    (setq hippie-expand-try-functions-list
          (remq f hippie-expand-try-functions-list)))
  :bind (("M-/" . hippie-expand)))

(use-package scratch
  :config
  (defun dnixty/scratch-buffer-setup ()
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (next-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t)))
  :hook (scratch-create-buffer-hook . dnixty/scratch-buffer-setup)
  :bind ("C-c s" . scratch))



;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------

(use-package minibuffer
  :config
  (setq completion-styles
        '(basic partial-completion substring))
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)

  (minibuffer-depth-indicate-mode 1)
  (defun dnixty/focus-minibuffer ()
    "Focus the active minibuffer."
    (interactive)
    (let ((mini (active-minibuffer-window)))
      (when mini
        (select-window mini))))
  :bind (:map completion-list-mode-map
              ("M-v" . dnixty/focus-minibuffer)
              ("h" . dnixty/describe-symbol-at-point)
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . next-completion)
              ("b" . previous-completion)))

;; Minibuffer history
(use-package savehist
  :config
  (setq history-length 30000)
  (savehist-mode 1))

(use-package icomplete
  :demand
  :config
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-separator (propertize " ┆ " 'face 'shadow))
  (setq icomplete-show-matches-on-no-input nil)
  (setq icomplete-prospects-height 1)
  (setq icomplete-in-buffer t)

  (icomplete-mode 1)
  :bind (:map icomplete-minibuffer-map
              ("<tab>" . icomplete-force-complete)
              ("<return>" . icomplete-force-complete-and-exit)
              ("C-j" . exit-minibuffer)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-l" . icomplete-fido-backward-updir)))

(use-package icomplete-vertical
  :demand
  :config
  (defun dnixty/icomplete-yank-kill-ring ()
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
  :bind (("M-y" . dnixty/icomplete-yank-kill-ring)))

(use-package project
  :config
  (defun dnixty/find-project ()
    "Switch to sub-directory at the specified locations."
    (interactive)
    (let* ((dirs (list "~/src"))
           (dotless directory-files-no-dot-files-regexp)
           (cands (mapcan (lambda (d)
                            (directory-files d t dotless))
                          dirs))
           (projects (mapcar 'abbreviate-file-name cands)))
      (icomplete-vertical-do ()
        (dired
         (completing-read "Find project: " projects nil t)))))
  (defun dnixty/find-file-vc-or-dir ()
    "Find file by name that belongs to the current project or dir."
    (interactive)
    (let* ((default-directory (file-name-directory
                               (or (locate-dominating-file "." ".git" )
                                   default-directory))))
      (let* ((filenames-all
              (directory-files-recursively default-directory ".*" nil t))
             (filenames (cl-remove-if (lambda (x)
                                        (string-match-p "\\.git" x))
                                      filenames-all)))
        (icomplete-vertical-do ()
          (find-file
           (completing-read "Find file recursively: "
                            filenames nil t))))))
  :bind (("s-s p" . dnixty/find-project)
         ("s-s f" . dnixty/find-file-vc-or-dir)))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc
  :after (ibuffer vc)
  :config
  :hook (ibuffer-hook . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package isearch
  :diminish
  :config
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lazy-count t)
  (setq isearch-allow-scroll 'unlimited))

(use-package rg
  :defer
  :config
  (rg-define-search
   dnixty/rg-vc-or-dir
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
   dnixty/rg-ref-in-dir
   "RipGrep for thing at point in present directory."
   :query point
   :format regexp
   :files "everything"
   :dir default-directory
   :confirm prefix
   :flags ("--hidden -g !.git"))

  :bind (("s-s g" . dnixty/rg-vc-or-dir)
         ("s-s r" . dnixty/rg-ref-in-dir)))

(use-package wgrep)



;;; --------------------------------------------------------------------
;;; 5. Aliases and custom commands
;;; --------------------------------------------------------------------

(use-package emacs
  :config
  (defun dnixty/switch-to-other ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (defun dnixty/describe-symbol-at-point ()
    (interactive)
    (let ((symbol (symbol-at-point)))
      (when symbol
        (describe-symbol symbol))))
  :bind (("s-0" . delete-window)
         ("s-1" . delete-other-windows)
         ("s-2" . split-window-below)
         ("s-3" . split-window-right)
         ("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         ("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-h" . dnixty/describe-symbol-at-point)
         ("s-i" . imenu)
         ("s-j" . dired-jump)
         ("s-J" . dired-jump-other-window)
         ("s-k" . kill-this-buffer)
         ("s-o" . other-window)
         ("s-<tab>" . dnixty/switch-to-other)
         ("s-," . previous-buffer)
         ("s-." . next-buffer)
         ("C-s-b" . windmove-left)
         ("C-s-n" . windmove-down)
         ("C-s-p" . windmove-up)
         ("C-s-f" . windmove-right)))



;;; --------------------------------------------------------------------
;;; 6. Programming languages
;;; --------------------------------------------------------------------

;; Nix Mode
(use-package nix-mode)

;; Recognise subwords
(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

;; Parentheses
(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
         (ielm-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (lisp-interaction-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)
         (sly-mrepl-hook . enable-paredit-mode)))
(use-package paren
  :config
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-delay 0)
  :hook (after-init-hook . show-paren-mode))

;; Sly
(use-package sly
  :commands sly
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-lisp-implementations
        `((sbcl ("sbcl" "--noinform"))
          (clisp ("clisp" "--quiet")))))
(use-package sly-mrepl
  :config
  (setq sly-mrepl-history-file-name (expand-file-name "sly-mrepl-history" user-emacs-directory)))

;; Prettify symbols
(use-package prog-mode
  :hook ((emacs-lisp-mode-hook . prettify-symbols-mode)
         (eval-expression-minibuffer-setup-hook . prettify-symbols-mode)
         (ielm-mode-hook . prettify-symbols-mode)
         (lisp-mode-hook . prettify-symbols-mode)
         (lisp-interaction-mode-hook . prettify-symbols-mode)
         (scheme-mode-hook . prettify-symbols-mode)))

;; Flycheck
(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Javascript
(use-package js
  :config
  (setq js-indent-level 2))

;; Typescript
(use-package tide
  :mode ("\\.tsx?\\'" . typescript-mode)
  :hook ((typescript-mode-hook . tide-setup)
         (typescript-mode-hook . flycheck-mode)
         (typescript-mode-hook . tide-hl-identifier-mode)))

;; Prettier
(use-package prettier-js
  :hook ((js-mode-hook . prettier-js-mode)
         (typescript-mode-hook . prettier-js-mode)))

;; Diff-hl
(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  :hook (after-init-hook . global-diff-hl-mode))

;; C
(use-package cc-vars
  :config
  (setq-default c-basic-offset 4))



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
  :hook (calendar-today-visible-hook . calendar-mark-today))

;; Org
(use-package org)

;; Eshell
(use-package eshell
  :bind (("s-<return>" . eshell)))
(use-package em-term
  :config
  (setq eshell-destroy-buffer-when-process-dies t))
(use-package esh-mode
  :config
  :bind (:map eshell-mode-map
              ("M-k" . eshell-kill-input)))
(use-package esh-module
  :config
  (delq 'eshell-banner eshell-modules-list)
  (push 'eshell-tramp eshell-modules-list))
(use-package esh-autosuggest
  :config
  (setq esh-autosuggest-delay 0.5)
  :bind (:map esh-autosuggest-active-map
              ("<tab>" . company-complete-selection))
  :hook (eshell-mode-hook . esh-autosuggest-mode))
(use-package em-tramp
  :config
  (setq password-cache t)
  (setq password-cache-expiry 3600))
(use-package em-hist
  :config
  (setq eshell-history-size 4096)
  (setq eshell-hist-ignoredups t)
  (defun dnixty/eshell-complete-history ()
    (interactive)
    (let ((hist (ring-elements eshell-history-ring)))
      (insert
       (completing-read "Input history: " hist nil t))))
  :bind (:map eshell-hist-mode-map
              ("M-r" . dnixty/eshell-complete-history)))

;; Ledger
(use-package ledger-mode
  :mode "\\.ldg$"
  :init)

;; Magit
(use-package magit
  :bind (("s-v" . magit-status)))
(use-package git-commit
  :after magit
  :config
  (setq git-commit-summary-max-length 50)
  (setq git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line)))
(use-package magit-diff
  :after magit
  :config
  (setq magit-diff-refine-hunk t))

;; Password Store
(use-package password-store
  :defer
  :commands (password-store-copy
             password-store-edit
             password-store-insert)
  :config
  (setq password-store-time-before-clipboard-restore 30)
  :bind (("s-p" . password-store-copy)))
(use-package password-store-otp
  :after password-store
  :bind (("s-P" . password-store-otp-token-copy)))

;; Pdf
(use-package pdf-tools)
(use-package pdf-occur
  :after pdf-tools
  :config
  (pdf-tools-install))

;; Dired
(use-package dired
  :config
  (setq dired-auto-revert-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode))
  :bind (("s-<f1>" . (lambda () (interactive) (dired "~/")))
         ("s-<f2>" . (lambda () (interactive) (dired "~/src")))
         ("s-<f3>" . (lambda () (interactive) (dired "~/prv/notes")))
         ("s-<f4>" . (lambda () (interactive) (dired "~/tmp")))
         ("s-<f11>" . (lambda () (interactive) (dired "/mnt/archive")))
         ("s-<f12>" . (lambda () (interactive) (dired "/mnt/torrents")))
         :map dired-mode-map
              ("C-l" . dired-up-directory)))
(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  (defun contrib/cdb--bookmarked-directories ()
    (bookmark-maybe-load-default-file)
    (cl-loop for (name . props) in bookmark-alist
             for fn = (cdr (assq 'filename props))
             when (and fn (string-suffix-p "/" fn))
             collect (cons name fn)))
  (defun contrib/cd-bookmark (bm)
    "Insert the path of a bookmarked directory."
    (interactive
     (list (let ((enable-recursive-minibuffers t))
             (completing-read
              "Directory: " (contrib/cdb--bookmarked-directories) nil t))))
    (when (minibufferp)
      (delete-region (minibuffer-prompt-end) (point-max)))
    (insert (cdr (assoc bm (contrib/cdb--bookmarked-directories)))))
  :bind (:map dired-mode-map
              ("C-+" . dired-create-empty-file)
              :map minibuffer-local-filename-completion-map
              ("C-c d" . contrib/cd-bookmark)))
(use-package find-dired
  :after dired
  :config
  (setq find-ls-option
        '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
  (setq find-name-arg "-iname"))
(use-package async)
(use-package dired-async
  :after (dired async)
  :hook (dired-mode-hook . dired-async-mode))
(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t))
(use-package dired-x
  :after dired
  :config
  (setq dired-bind-man nil)
  (setq dired-bind-info nil))
(use-package diredfl
  :hook (dired-mode-hook . diredfl-mode))
(use-package trashed
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))
(use-package dired-single
  :bind (([remap dired-find-file] . dired-single-buffer)
         ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
         ([remap dired-up-directory] . dired-single-up-directory)))

;; Proced
(use-package proced
  :commands proced
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 1))


;;; init.el ends here
