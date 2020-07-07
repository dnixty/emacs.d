;;; init.el --- Emacs configuration file.

;;; Table of contents
;;   1. Prerequisites
;;   2. Base settings
;;   3. Aliases and custom commands
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

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

;; Move user-emacs-directory
(use-package emacs
  :config
  (setq user-emacs-directory "~/.cache/emacs/"))


;;; --------------------------------------------------------------------
;;; 2. Base settings
;;; --------------------------------------------------------------------

;; Edit modeline "lighters"
(use-package diminish)

;; Don't let `customize' clutter the config
(use-package cus-edit
  :config
  (setq custom-file
        (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                          temporary-file-directory)))

;; Base typeface configurations
(use-package emacs
  :config
  (set-frame-font "Hack 11"))

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

;; Minibuffer history
(use-package savehist
  :config
  (setq history-length 30000)
  (savehist-mode 1))

;; Recentf
(use-package recentf
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
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


;;; --------------------------------------------------------------------
;;; 3. Aliases and custom commands
;;; --------------------------------------------------------------------

(use-package emacs
  :config
  (defun dnixty/switch-to-other ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
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
         ("s-i" . imenu)
         ("s-j" . dired-jump)
         ("s-J" . dired-jump-other-window)
         ("s-k" . kill-this-buffer)
         ("s-o" . other-window)
         ("s-p" . password-store-copy)
         ("s-P" . password-store-otp-token-copy)
         ("s-q" . window-toggle-side-windows)
         ("s-v" . magit-status)
         ("s-<return>" . eshell)
         ("s-<tab>" . dnixty/switch-to-other)
         ("s-," . previous-buffer)
         ("s-." . next-buffer)
         ("C-s-b" . windmove-left)
         ("C-s-n" . windmove-down)
         ("C-s-p" . windmove-up)
         ("C-s-f" . windmove-right)))



;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------

(use-package orderless
  :config
  (setq orderless-matching-styles
        '(orderless-regexp
          orderless-flex))
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)))

(use-package minibuffer
  :after orderless
  :config
  (setq completion-styles
        '(basic partial-completion orderless))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq completions-format 'horizontal)
  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)

  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  (defun dnixty/focus-minibuffer ()
    (interactive)
    (let ((mini (active-minibuffer-window)))
      (when mini
        (select-window mini))))

  :bind (:map minibuffer-local-completion-map
              ("<return>" . minibuffer-force-complete-and-exit)
              ("C-j" . exit-minibuffer)
              :map completion-list-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("f" . next-completion)
              ("b" . previous-completion)
              ("M-v" . dnixty/focus-minibuffer)))

(use-package icomplete
  :demand
  :after minibuffer
  :config
  (setq icomplete-delay-completions-threshold 100)
  (setq icomplete-max-delay-chars 2)
  (setq icomplete-compute-delay 0.2)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 1)
  (setq icomplete-separator (propertize " ┆ " 'face 'shadow))
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)

  (fido-mode -1)
  (icomplete-mode 1)

  (defun dnixty/icomplete-minibuffer-truncate ()
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (setq truncate-lines t)))

  :hook (icomplete-minibuffer-setup-hook . dnixty/icomplete-minibuffer-truncate)
  :bind (:map icomplete-minibuffer-map
              ("<tab>" . icomplete-force-complete)
              ("<return>" . icomplete-force-complete-and-exit)
              ("C-j" . exit-minibuffer)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-l" . icomplete-fido-backward-updir)))

(use-package icomplete-vertical
  :demand
  :after (minibuffer icomplete)
  :config
  (setq icomplete-vertical-prospects-height (/ (window-height) 6))
  (icomplete-vertical-mode -1)

  (defun dnixty/icomplete-yank-kill-ring ()
    (interactive)
    (let ((kills
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do
          (:separator 'dotted-line :height (/ (window-height) 4))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))

  :bind (("M-y" . dnixty/icomplete-yank-kill-ring)
         :map icomplete-minibuffer-map
         ("C-v" . icomplete-vertical-toggle)))

(use-package project
  :after (minibuffer icomplete icomplete-vertical)
  :config
  (defun dnixty/find-file-vc-or-dir (&optional arg)
    "Find file by name that belongs to the current project or dir.
With \\[universal-argument] match files by contents.  This
requires the command-line executable called 'rg' or 'ripgrep'."
    (interactive "P")
    (let* ((default-directory (file-name-directory
                               (or (locate-dominating-file "." ".git" )
                                   default-directory))))
      (if arg
          (let* ((regexp (read-regexp
                          (concat "File contents matching REGEXP in "
                                  (propertize default-directory 'face 'bold)
                                  ": ")))
                 (results (process-lines "rg" "-l" "--hidden" "-m" "1" "-M" "120" regexp)))
            (find-file
             (icomplete-vertical-do ()
               (completing-read (concat
                                 "Files with contents matching "
                                 (propertize regexp 'face 'success)
                                 (format " (%s)" (length results))
                                 ": ")
                                results nil t))))
        (let* ((filenames-all (directory-files-recursively default-directory ".*" nil t))
               (filenames (cl-remove-if (lambda (x)
                                          (string-match-p "\\.git" x))
                                        filenames-all)))
          (icomplete-vertical-do ()
            (find-file
             (completing-read "Find file recursively: " filenames nil t)))))))

  (defun dnixty/find-project (&optional arg)
    "Switch to sub-directory at the specified locations.
With \\[universal-argument] produce a `dired' buffer instead with
all the possible candidates."
    (interactive "P")
    (let* ((dirs (list "~/src"))
           (dotless directory-files-no-dot-files-regexp)
           (cands (mapcan (lambda (d)
                            (directory-files d t dotless))
                          dirs))
           (projects (mapcar 'abbreviate-file-name cands))
           (buf "*Projects Dired*"))
      (if arg
          (dired (cons (generate-new-buffer-name buf) projects))
        (icomplete-vertical-do ()
          (dired
           (completing-read "Find project: " projects nil t))))))

  :bind (("s-s p" . dnixty/find-project)
         ("s-s f" . dnixty/find-file-vc-or-dir)
         ("s-s l" . find-library)))

(use-package emacs
  :after (minibuffer icomplete icomplete-vertical)
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

(use-package dabbrev
  :after (minibuffer icomplete icomplete-vertical)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t))

(use-package ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  :hook (ibuffer-mode-hook . hl-line-mode)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("* f" . ibuffer-mark-by-file-name-regexp)
         ("* g" . ibuffer-mark-by-content-regexp)
         ("* n" . ibuffer-mark-by-name-regexp)
         ("s n" . ibuffer-do-sort-by-alphabetic)
         ("/ g" . ibuffer-filter-by-content)))

(use-package ibuffer-vc
  :ensure
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
              ("/ <deletechar>" . ibuffer-clear-filter-groups)))


(use-package isearch
  :diminish
  :config
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package rg
  :after wgrep
  :config
  (setq rg-custom-type-aliases nil)
  (rg-define-search dnixty/rg-vc-or-dir
    "RipGrep in project root or present directory."
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
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

  (defun dnixty/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind (("s-s g" . dnixty/rg-vc-or-dir)
         ("s-s r" . dnixty/rg-ref-in-dir)
         :map rg-mode-map
         ("s" . dnixty/rg-save-search-as-name)
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))


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
    "Enable some Modus Operandi variables and load the theme."
    (setq custom-enabled-themes '(modus-operandi))
    (setq modus-operandi-theme-visible-fringes t)
    (setq modus-operandi-theme-proportional-fonts t)
    (setq modus-operandi-theme-rainbow-headings t)
    (dnixty/reapply-themes))
  (defun dnixty/modus-vivendi ()
    "Enable some Modus Vivendi variables and load the theme."
    (setq custom-enabled-themes '(modus-vivendi))
    (setq modus-vivendi-theme-visible-fringes t)
    (setq modus-vivendi-theme-proportional-fonts t)
    (setq modus-vivendi-theme-rainbow-headings t)
    (dnixty/reapply-themes))
  (defun dnixty/theme-toggle ()
    "Toggle between sanityinc-tomorrow themes."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (dnixty/modus-vivendi)
      (dnixty/modus-operandi)))
  :hook (after-init-hook . dnixty/modus-operandi)
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

;; Parentheses
(use-package paredit
  :diminish
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
         (ielm-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (lisp-interaction-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)))
(use-package paren
  :config
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-delay 0)
  :hook (after-init-hook . show-paren-mode))
(use-package sly
  :config
  :hook (sly-mrepl-hook . (lambda () (paredit-mode +1))))

;; Newline characters for file ending
(use-package emacs
  :config
  (setq mode-require-final-newline 'visit-save))

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
  :bind ("C-z" . goto-last-change))



;;; --------------------------------------------------------------------
;;; 6. Programming languages
;;; --------------------------------------------------------------------

;; Nix Mode
(use-package nix-mode)

;; Recognise subwords
(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

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
  :config
  (setq typescript-indent-level 2)
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
  (setq-default c-default-style "linux")
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
  (setq eshell-history-size 1024)
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
  :pin manual
  :mode "\\.ldg$"
  :init)

;; Magit
(use-package magit)
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
(use-package magit-repos
  :after magit
  :commands magit-list-repositories
  :config
  (setq magit-repository-directories
        '(("~/src" . 1))))

;; Password Store
(use-package password-store
  :defer
  :commands (password-store-copy
             password-store-edit
             password-store-insert)
  :config
  (setq password-store-time-before-clipboard-restore 30))
(use-package password-store-otp
  :after password-store)
;; (use-package pass
;;   :defer
;;   :commands pass)

;; Pdf
(use-package pdf-tools
  :pin manual)
(use-package pdf-occur
  :pin manual
  :after pdf-tools
  :config
  (pdf-tools-install))

;; Dired
;; TODO: review
(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  :bind (:map dired-mode-map
              ("C-l" . dired-up-directory))
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))
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
(use-package peep-dired
  :after dired
  :config
  (setq peep-dired-enable-on-directories nil)
  :bind (:map dired-mode-map
              ("P" . peep-dired)))
(use-package image-dired
  :config
  (setq image-dired-external-viewer "sxiv")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  :bind (:map image-dired-thumbnail-mode-map
              ("<return>" . image-dired-thumbnail-display-external)))
(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))
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

;; Proced
(use-package proced
  :commands proced
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 1))

;; Restclient
(use-package restclient)


;;; init.el ends here
