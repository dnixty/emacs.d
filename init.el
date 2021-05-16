;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

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

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  :ensure
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))


;;; --------------------------------------------------------------------
;;; 2. Visual settings
;;; --------------------------------------------------------------------


;; Make the frame larger on desktop
(if (and window-system (> (display-pixel-width) 1366))
    (setq initial-frame-alist '((width . 106) (height . 60))))

;; Base typeface configurations
(use-package emacs
  :config
  (defun dps/laptop-font ()
    "Font for the small laptop screen."
    (interactive)
    (when window-system
      (set-face-attribute 'default nil :family "Hack" :height 110)))
  (defun dps/desktop-font ()
    "Font for the larger desktop screen."
    (interactive)
    (when window-system
      (set-face-attribute 'default nil :family "Hack" :height 180)))
  (defun dps/set-font ()
    (when window-system
      (if (<= (display-pixel-width) 1366)
          (dps/laptop-font)
        (dps/desktop-font))))
  :hook (window-setup-hook . dps/set-font))

;; Theme
(use-package modus-themes
  :ensure
  :init
  (modus-themes-load-themes)
  :config
  (let ((time (string-to-number (format-time-string "%H"))))
    (if (and (> time 5) (< time 18))
    (modus-themes-load-operandi)
      (modus-themes-load-vivendi)))
  (enable-theme 'modus-vivendi)
  :bind (("<f5>" . modus-themes-toggle)))
(use-package diminish
  :ensure)


;;; --------------------------------------------------------------------
;;; 3. Base settings
;;; --------------------------------------------------------------------

(use-package emacs
  :config
  (setq frame-title-format '("%b"))
  (setq ring-bell-function 'ignore)
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
  :ensure
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
  :ensure
  :config
  (defun dps/scratch-buffer-setup ()
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (next-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t)))
  :hook (scratch-create-buffer-hook . dps/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

;; Parentheses
(use-package paren
  :hook (after-init-hook . show-paren-mode))

;; Repeat
(use-package repeat
  :config
  (setq repeat-on-final-keystroke t))

;; Tabs
(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode -1)
  (tab-bar-history-mode -1)
  (defun dps/tab-bar-select-tab-dwim ()
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (icomplete-vertical-do ()
                                    (tab-bar-switch-to-tab
                                     (completing-read "Select tab: " tabs nil t)))))))
  (defun dps/tab-bar-toggle ()
    "Toggle `tab-bar' presentation."
    (interactive)
    (if (bound-and-true-p tab-bar-mode)
        (progn
          (setq tab-bar-show nil)
          (tab-bar-mode -1))
      (setq tab-bar-show t)
      (tab-bar-mode 1)))
  :bind (("<f8>" . dps/tab-bar-toggle)
         ("C-x t t" . dps/tab-bar-select-tab-dwim)
         ("C-x t ," . tab-next)
         ("C-x t ." . tab-previous)))

;; Comments
(use-package newcomment
  :config
  (defun dps/comment-dwim (arg)
  "Flexible, do-what-I-mean commenting.

If region is active and ARG is either a numeric argument greater
than one or a universal prefix (\\[universal-argument]), then
apply `comment-kill' on all comments in the region.

If the region is active and no ARG is supplied, or is equal to a
numeric prefix of 1, then toggle the comment status of the region.

Else toggle the comment status of the line at point.  With a
numeric prefix ARG, do so for ARGth lines (negative prefix
operates on the lines before point)."
  (interactive "p")
  (cond
   ((and (> arg 1) (use-region-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (num (count-lines beg end)))
      (save-excursion
        (goto-char beg)
        (comment-kill num))))
   ((use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end)))
   (t
    (save-excursion (comment-line (or arg 1))))))
  (setq comment-empty-lines t)
  (setq comment-multi-line t)
  ;; (setq comment-style 'multi-line)
  :bind (("C-;" . dps/comment-dwim)
         ("C-:" . comment-kill)
         ("M-;" . comment-indent)))

;; Eldoc
(use-package eldoc
  :diminish)


;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------


(use-package ivy
  :ensure t
  :diminish
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          (ivy-switch-buffer . ivy--regex-fuzzy)
          (ivy-switch-buffer-other-window . ivy--regex-fuzzy)
          (counsel-rg . ivy--regex-or-literal)
          (t . ivy--regex-plus)))
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist
        '((counsel-M-x . "^")
          (ivy-switch-buffer . "^")
          (ivy-switch-buffer-other-window . "^")
          (counsel-describe-function . "^")
          (counsel-describe-variable . "^")
          (t . "")))

  (ivy-set-occur 'counsel-fzf 'counsel-fzf-occur)
  (ivy-set-occur 'counsel-rg 'counsel-ag-occur)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  (ivy-set-occur 'swiper-multi 'counsel-ag-occur)
  :hook ((after-init-hook . ivy-mode)
         (ivy-occur-mode-hook . hl-line-mode))
  :bind (("<M-up>" . ivy-push-view)
         ("<M-down>" . ivy-switch-view)
         ("C-S-r" . ivy-resume)
         :map ivy-occur-mode-map
         ("f" . forward-char)
         ("b" . backward-char)
         ("n" . ivy-occur-next-line)
         ("p" . ivy-occur-previous-line)
         ("<C-return>" . ivy-occur-press)))

(use-package prescient
  :ensure t
  :config
  (setq prescient-history-length 200)
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :ensure t
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-switch-buffer
               ivy-switch-buffer
               swiper
               swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (ivy-prescient-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (setq counsel-yank-pop-preselect-last t)
  (setq counsel-yank-pop-separator "\n—————————\n")
  (setq counsel-rg-base-command
        "rg -SHn --no-heading --color never --no-follow --hidden --glob '!.git/' %s")
  (setq counsel-find-file-occur-cmd
        "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first")

  (defun dps/counsel-fzf-rg-files (&optional input dir)
    "Run `fzf' in tandem with `ripgrep' to find files in the
present directory.  If invoked from inside a version-controlled
repository, then the corresponding root is used instead."
    (interactive)
    (let* ((process-environment
            (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden --glob '!.git/'")
                  process-environment))
           (vc (vc-root-dir)))
      (if dir
          (counsel-fzf input dir)
        (if (eq vc nil)
            (counsel-fzf input default-directory)
          (counsel-fzf input vc)))))

  (defun dps/counsel-fzf-dir (arg)
    "Specify root directory for `counsel-fzf'."
    (prot/counsel-fzf-rg-files ivy-text
                               (read-directory-name
                                (concat (car (split-string counsel-fzf-cmd))
                                        " in directory: "))))

  (defun dps/counsel-rg-dir (arg)
    "Specify root directory for `counsel-rg'."
    (let ((current-prefix-arg '(4)))
      (counsel-rg ivy-text nil "")))

  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  (ivy-add-actions
   'counsel-fzf
   '(("r" dps/counsel-fzf-dir "change root directory")
     ("g" dps/counsel-rg-dir "use ripgrep in root directory")))

  (ivy-add-actions
   'counsel-rg
   '(("r" dps/counsel-rg-dir "change root directory")
     ("z" dps/counsel-fzf-dir "find file with fzf in root directory")))

  (ivy-add-actions
   'counsel-find-file
   '(("g" dps/counsel-rg-dir "use ripgrep in root directory")
     ("z" dps/counsel-fzf-dir "find file with fzf in root directory")))

  ;; Remove commands that only work with key bindings
  (put 'counsel-find-symbol 'no-counsel-M-x t)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-x d" . counsel-dired)
         ("C-x C-r" . counsel-recentf)
         ("C-M-y" . counsel-mark-ring)
         ("M-y" . counsel-yank-pop)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> a" . counsel-apropos)
         ("M-s r" . counsel-rg)
         ("M-s g" . counsel-git-grep)
         ("M-s l" . counsel-find-library)
         ("M-s z" . dps/counsel-fzf-rg-files)
         ("C-c c" . counsel-compile)
         :map ivy-minibuffer-map
         ("C-l" . counsel-up-directory)
         ("C-r" . counsel-minibuffer-history)
         ("C-SPC" . ivy-restrict-to-matches)))

(use-package projectile
  :ensure t
  :diminish
  :config
  (setq projectile-project-search-path '("~/src"))
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (add-to-list 'ivy-initial-inputs-alist '(counsel-projectile-switch-project . ""))
  :hook (after-init-hook . counsel-projectile-mode)
  :bind (("M-s b" . counsel-projectile-switch-to-buffer)
         ("M-s d" . counsel-projectile-find-dir)
         ("M-s p" . (lambda ()
                      (interactive)
                      (counsel-projectile-switch-project 4)))))

(use-package swiper
  :ensure t
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t)
  (setq swiper-include-line-number-in-search t)
  :bind (("C-S-s" . swiper)
         ("M-s s" . swiper-multi)
         ("M-s w" . swiper-thing-at-point)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-rich-path-style 'abbreviate)
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  :hook (after-init-hook . ivy-rich-mode))

(use-package ivy-posframe
  :ensure t
  :after ivy
  :diminish
  :config
  (setq ivy-posframe-parameters
        '((left-fringe . 2)
          (right-fringe . 2)
          (internal-border-width . 2)))
  (setq ivy-posframe-height-alist
        '((swiper . 15)
          (swiper-isearch . 15)
          (t . 10)))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . nil)
          (swiper-isearch . nil)
          (t . ivy-posframe-display-at-frame-center)))
  :hook (after-init-hook . ivy-posframe-mode))

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
  :ensure
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

(use-package wgrep
  :ensure
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
         ("C-," . previous-buffer)
         ("C-." . next-buffer)
         ("M-RET" . eshell)))


;;; --------------------------------------------------------------------
;;; 6. Programming
;;; --------------------------------------------------------------------

;; Lsp
(use-package lsp-mode
  :ensure
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-snippet nil)
  :hook (js-mode-hook . lsp))

;; Javascript
(use-package js
  :after lsp-mode
  :config
  (setq js-indent-level 2))
(use-package add-node-modules-path
  :ensure
  :hook (js-mode-hook . add-node-modules-path))

;; C
(use-package cc-vars
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "bsd"))))

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
(use-package flycheck
  :ensure
  :after lsp-mode
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :hook (prog-mode-hook . flycheck-mode))

;; Subword
(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

;; Highlight TODO
(use-package hl-todo
  :ensure
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
(use-package wdired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t))

;; Ledger
(use-package ledger-mode
  :ensure
  :mode "\\.ldg\\'")


;;; init.el ends here
