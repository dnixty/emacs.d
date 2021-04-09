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

;; Font configuration
(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "Hack 11")))

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
  (enable-theme 'modus-operandi)
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

;; Tabs
(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode -1)
  (tab-bar-history-mode -1)
  (defun dnixty/tab-bar-select-tab-dwim ()
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
  :bind (("C-x t t" . dnixty/tab-bar-select-tab-dwim)
         ("C-x t ," . tab-next)
         ("C-x t ." . tab-previous)))


;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------

;; Minibuffer
(use-package minibuffer
  :config
  (use-package orderless
    :ensure
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
  :ensure
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
  :ensure
  :diminish
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

(use-package rg
  :ensure
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
         ("M-;" . comment-line)
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
  (setq-default c-basic-offset 4))

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

;; Rainbow Mode
(use-package rainbow-mode
  :ensure)

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

;; Elfeed
(use-package elfeed
  :ensure
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (defun dnixty/elfeed-feeds ()
    (let ((feeds "~/.local/blogroll.el"))
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
  :ensure
  :mode "\\.ldg\\'")

;; Org
(use-package org
  :config
  (setq org-agenda-files
        '("~/org"
          "~/documents"))
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(D)" "CANCEL(C)")
          (sequence "MEET(m)" "|" "MET(M)")
          (sequence "STUDY(s)" "|" "STUDIED(S)")
          (sequence "WRITE(w)" "|" "WROTE(W)")))
  (setq org-todo-keyword-faces
        '(("MEET" . '(font-lock-preprocessor-face org-todo))
          ("STUDY" . '(font-lock-variable-name-face org-todo))
          ("WRITE" . '(font-lock-type-face org-todo))))
  (setq org-priority-faces
        '((?A . '(org-scheduled-today org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-tag-alist
        '((:startgroup)
          ("@work")
          ("@priv")
          (:endgroup)
          ("emacs")
          ("compsci")
          ("countryside")))
  (setq org-confirm-babel-evaluate nil)
  (setq org-log-done 'time)
  (setq org-read-date-prefer-future 'time)
  (setq org-adapt-indentation nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers t)
  (setq org-hide-macro-markers t)
  (setq org-catch-invisible-edits 'show)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-imenu-depth 7)
  (setq org-modules '(ol-info ol-eww))
  :bind (:map org-mode-map
              ("<C-return>" . nil)
              ("<C-S-return>" . nil)))
(use-package ol
  :config
  (setq org-link-keep-stored-after-insertion t)
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c S-l" . org-toggle-link-display)
         ("C-c C-S-l" . org-insert-last-stored-link)))
(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        `(("b" "Basic task for future review" entry
           (file+headline "tasks.org" "Basic tasks that need to be reviewed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l"))
          ("w" "Work")
          ("wt" "Task or assignment" entry
           (file+headline "work.org" "Tasks and assignments")
           ,(concat "* TODO [#A] %^{Title} :@work:\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))
          ("wm" "Meeting, event, appointment" entry
           (file+headline "work.org" "Meetings, events, and appointments")
           ,(concat "* MEET [#A] %^{Title} :@work:\n"
                    "SCHEDULED: %^T\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n:END:\n\n"
                    "%i%?"))
          ("t" "Task with a due date" entry
           (file+headline "tasks.org" "Task list with a date")
           ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))))
  (defun contrib/org-capture-no-delete-windows (oldfun args)
    (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
      (apply oldfun args)))
  (advice-add 'org-capture-place-template
              :around 'contrib/org-capture-no-delete-windows)
  :bind ("C-c c" . org-capture))
(use-package org-agenda
  :after org
  :config
  (setq org-agenda-span 14)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-block-separator ?—)
  (defun dnixty/org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.

Slightly tweaked version of `org-agenda-format-date-aligned' that
produces dates with a fixed length."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date t))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month t))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (weekyear (cond ((and (= month 1) (>= iso-week 52))
                            (1- year))
                           ((and (= month 12) (<= iso-week 1))
                            (1+ year))
                           (t year)))
           (weekstring (if (= day-of-week 1)
                           (format " (W%02d)" iso-week)
                         "")))
      (format "%s %2d %s %4d%s"
              dayname day monthname year weekstring)))
  (setq org-agenda-format-date #'dnixty/org-agenda-format-date-aligned)
  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-include-diary t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-current-time-string
        "Now -·-·-·-·-·-·-")
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0600 0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " ....." "-----------------"))
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-column -120)
  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-'" . nil)
         ("C-," . nil)))
(use-package org-src
  :after org
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

;;; init.el ends here
