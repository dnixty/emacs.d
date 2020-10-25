;;; package --- Summary
;;; Commentary:

;;; Table of contents
;;   1. Prerequisites
;;   2. Visual settings
;;   3. Base settings
;;   4. Selection narrowing and search
;;   5. Aliases and custom commands
;;   6. Programming languages
;;   7. Applications and utilities


;;; Code:

;;; --------------------------------------------------------------------
;;; 1. Prerequisites
;;; --------------------------------------------------------------------

;; Setup use-package
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

;; Font configuration
(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "Hack 11")))

;; Theme
(use-package modus-operandi-theme
  :demand)
(use-package modus-vivendi-theme
  :demand)
(use-package solar
  :config
  (setq calendar-latitude 51.508530)
  (setq calendar-longitude -0.076132))
(use-package emacs
  :after (modus-operandi-theme modus-vivendi-theme)
  :init
  (setq custom-safe-themes t)
  ;; Light at sunrise
  (run-at-time (nth 1 (split-string (sunrise-sunset)))
               (* 60 60 24)
               (lambda () (enable-theme 'modus-operandi)))
  ;; Dark at sunset
  (run-at-time (nth 4 (split-string (sunrise-sunset)))
               (* 60 60 24)
               (lambda () (enable-theme 'modus-vivendi)))
  :config
  (enable-theme 'modus-operandi))



;;; --------------------------------------------------------------------
;;; 3. Base settings
;;; --------------------------------------------------------------------

(use-package emacs
  :config
  (setq-default fill-column 72)
  (setq-default tab-always-indent 'complete)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq auto-save-default nil)
  (setq auto-save-list-file-prefix nil)
  (setq column-number-mode t)
  (setq create-lockfiles nil)
  (setq custom-file
      (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                        temporary-file-directory))
  (setq mode-require-final-newline 'visit-save)
  (setq scroll-conservatively 1)
  (setq scroll-error-top-bottom t)
  (setq scroll-margin 0)
  (setq scroll-preserve-screen-position t)
  (setq select-enable-primary t)
  (fset 'display-startup-echo-area-message 'ignore)
  (defalias 'yes-or-no-p 'y-or-n-p)
  :hook ((before-save-hook . whitespace-cleanup)
         (before-save-hook . (lambda () (delete-trailing-whitespace)))))

;; Backups
(use-package emacs
  :config
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6))

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
          ((control) . text-scale))))

;; Autorevert
(use-package autorevert
  :hook (after-init-hook . global-auto-revert-mode))

;; Bookmarks
(use-package bookmark
  :config
  (setq bookmark-save-flag 1))

;; Winner mode
(use-package winner
  :hook (after-init-hook . winner-mode)
  :bind (("C->" . winner-redo)
         ("C-<" . winner-undo)))

;; Expand Region
(use-package expand-region
  :config
  (setq expand-region-smart-cursor t)
  :bind (("C-=" . er/expand-region)
         ("C-M-=" . er/mark-outside-pairs)
         ("C-+" . er/mark-symbol)))

;; Delete selection
(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

;; Scratch
(use-package emacs
  :config
  (setq initial-scratch-message (format ";; Scratch - Started on %s\n\n"
                                        (current-time-string))))
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

;; Parentheses
(use-package paren
  :hook (after-init-hook . show-paren-mode))

;; Repeat
(use-package repeat
  :config
  (setq repeat-on-final-keystroke t))



;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------

;; Minibuffer
(use-package minibuffer
  :config
  (use-package orderless
    :config
    (setq orderless-component-separator " +")
    (setq orderless-matching-styles
          '(orderless-flex
            orderless-strict-leading-initialism
            orderless-regexp
            orderless-prefixes
            orderless-literal))
    :bind (:map minibuffer-local-completion-map
               ("SPC" . nil)))
  (setq completion-styles
        '(orderless partial-completion))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-ignore-case t)
  (setq completion-show-help nil)
  (setq completions-format 'vertical)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq enable-recursive-minibuffers t)
  (setq resize-mini-windows t)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))
(use-package savehist
  :config
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (savehist-mode 1))

;; Icomplete
(use-package icomplete
  :demand
  :after minibuffer
  :config
  (setq icomplete-delay-completions-threshold 100)
  (setq icomplete-max-delay-chars 2)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-separator (propertize " · " 'face 'shadow))
  (setq icomplete-show-matches-on-no-input nil)
  (setq icomplete-prospects-height 1)
  (setq icomplete-in-buffer t)
  (setq icomplete-tidy-shadowed-file-names t)
  (defun dnixty/icomplete-minibuffer-truncate ()
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (setq truncate-lines t)))
  (icomplete-mode 1)
  :hook (icomplete-minibuffer-setup-hook . dnixty/icomplete-minibuffer-truncate)
  :bind (:map icomplete-minibuffer-map
              ("<tab>" . icomplete-force-complete)
              ("<return>" . icomplete-force-complete-and-exit)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-l" . icomplete-fido-backward-updir)
              ("C-j" . exit-minibuffer)))
(use-package icomplete-vertical
  :demand
  :after (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (frame-height) 6))
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
  :bind (("M-y" . dps/icomplete-yank-kill-ring)
         :map icomplete-minibuffer-map
         ("C-v" . icomplete-vertical-toggle)))

;; In-buffer completions
(use-package emacs
  :config
  (defun contrib/completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
Use as a value for `completion-in-region-function'."
    (if (and (minibufferp) (not (string= (minibuffer-prompt) "Eval: ")))
        (completion--in-region start end collection predicate)
      (let* ((initial (buffer-substring-no-properties start end))
             (limit (car (completion-boundaries initial collection predicate "")))
             (all (completion-all-completions initial collection predicate
                                              (length initial)))
             (completion (cond
                          ((atom all) nil)
                          ((and (consp all) (atom (cdr all)))
                           (concat (substring initial 0 limit) (car all)))
                          (t (completing-read
                              "Completion: " collection predicate t initial)))))
        (if (null completion)
            (progn (message "No completion") nil)
          (delete-region start end)
          (insert completion)
          t))))
  (setq completion-in-region-function #'contrib/completing-read-in-region)
  :bind (:map minibuffer-local-completion-map
              ("<tab>" . minibuffer-force-complete)))

;; Projects
(use-package projectile
  :config
  (setq projectile-completion-system 'default)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

;; Dabbrev
(use-package dabbrev
  :after (minibuffer icomplete icomplete-vertical)
  :config
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t)
  :bind (("C-M-/" . dabbrev-completion)))

;; Recentf
(use-package recentf
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (defun dnixty/recentf-keep-predicate (file)
    (cond
     ((file-directory-p file) (file-readable-p file))))
  (add-to-list 'recentf-keep 'dnixty/recentf-keep-default-predicate)

  (defun dnixty/recentf (&optional input)
    (interactive)
    (let* ((files (mapcar 'abbreviate-file-name recentf-list))
           (f (icomplete-vertical-do ()
                (completing-read "Open recentf entry: " files nil t
                                 (when input input)))))
      (find-file f)))
  :hook (after-init-hook . recentf-mode)
  :bind ("C-x C-r" . dnixty/recentf))

;; Ibuffer
(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind ("C-x C-b" . ibuffer))
(use-package ibuf-ext
  :config
  (setq ibuffer-show-empty-filter-groups nil))
(use-package ibuffer-vc
  :after (ibuffer vc)
  :hook (ibuffer-hook . ibuffer-vc-set-filter-groups-by-vc-root))

;; Isearch
(use-package isearch
  :config
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited))

(use-package rg
  :defer
  :config
  (rg-define-search dnixty/rg-vc-or-dir
    "RipGrep in project root or present directory."
    :query ask
    :format regexp
    :files "everything"
    :dir (or (vc-root-dir)
             default-directory)
    :confirm prefix
    :flags ("--hidden -g !.git"))
  (rg-define-search dnixty/rg-ref-in-dir
    "RipGrep for thing at point in present directory."
    :query point
    :format regexp
    :files "everything"
    :dir default-directory
    :confirm prefix
    :flags ("--hidden -g !.git"))
  :bind (("C-c g" . dnixty/rg-vc-or-dir)
         ("C-c r" . dnixty/rg-ref-in-dir)
         :map rg-mode-map
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

;; Wgrep
(use-package wgrep
  :commands wgrep
  :config
  (setq wgrep-auto-save-buffer 1)
  (setq wgrep-change-readonly-file t))



;;; --------------------------------------------------------------------
;;; 5. Aliases and custom commands
;;; --------------------------------------------------------------------

(use-package emacs
  :bind (("C-h" . delete-backward-char)
         ("C-x k" . kill-this-buffer)
         ("M-z" . zap-up-to-char)
         ("M-;" . comment-line)
         ("C-," . previous-buffer)
         ("C-." . next-buffer)
         ("M-RET" . eshell)))



;;; --------------------------------------------------------------------
;;; 6. Programming
;;; --------------------------------------------------------------------

;; Direnv
(use-package direnv
  :config
  (direnv-mode)
  :hook (prog-mode-hook . direnv-update-environment))

;; Lsp
(use-package lsp-mode
  :after (direnv)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-snippet nil)
  :hook (js-mode-hook . lsp-deferred))

;; Javascript
(use-package js
  :after lsp-mode
  :config
  (setq js-indent-level 2))
(use-package add-node-modules-path
  :hook (js-mode-hook . add-node-modules-path))

;; Haskell
(use-package haskell-mode)

;; Nix
(use-package nix-mode)

;; Diff-hl
(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  :hook (after-init-hook . global-diff-hl-mode))

;; Magit
(use-package magit
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :bind ("C-c v" . magit))

;; Flycheck
(use-package flycheck
  :after lsp-mode
  :config
  (flycheck-add-next-checker 'lsp 'javascript-eslint)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :hook (prog-mode-hook . flycheck-mode))

;; Subword
(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

;; Rainbow Mode
(use-package rainbow-mode)

;; Highlight TODO
(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode))



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

;; Elfeed

(use-package elfeed
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (defun dnixty/elfeed-feeds ()
    (let ((feeds "~/documents/blogroll.el"))
      (if (file-exists-p feeds)
          (load-file feeds)
        (user-error "Missing feeds' file"))))
  :hook (elfeed-search-mode-hook . dnixty/elfeed-feeds)
  :bind (("C-c f" . elfeed)
         :map elfeed-search-mode-map
         ("g" . elfeed-update)
         ("G" . elfeed-search-update--force)))


;; Ledger
(use-package ledger-mode
  :mode "\\.ldg\\'")

;;; init.el ends here
