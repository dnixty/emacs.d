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
  (defun dnixty/laptop-font ()
    "Font for the small laptop screen."
    (interactive)
    (when window-system
      (set-face-attribute 'default nil :family "Hack" :height 110)))
  (defun dnixty/desktop-font ()
    "Font for the larger desktop screen."
    (interactive)
    (when window-system
      (set-face-attribute 'default nil :family "Hack" :height 140)))
  (defun dnixty/set-font ()
    (when window-system
      (if (<= (display-pixel-width) 1366)
          (dnixty/laptop-font)
        (dnixty/desktop-font))))
  :hook (window-setup-hook . dnixty/set-font))

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
  (setq recentf-show-file-shortcuts-flag nil)
  :hook (after-init-hook . recentf-mode))


;;; --------------------------------------------------------------------
;;; 3. Window manager
;;; --------------------------------------------------------------------

(use-package window
  :init
  (setq display-buffer-alist
        '(("\\*\\(Flycheck\\|Flymake\\|Package-Lint\\|vc-git :\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|breakpoints of.*\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Output\\|Register Preview\\|input/output of\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("^\\(\\*e?shell\\|\\*gud\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 1))
          ("\\*\\(Help\\|slime-description\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.20)       ; See the :hook
           (side . left)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*rg\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0))
          ("\\*\\vc-\\(incoming\\|outgoing\\).*"
           (display-buffer-at-bottom))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  :hook (help-mode-hook . visual-line-mode)
  :bind ("C-x +" . balance-windows-area))

(use-package emacs
  :commands (dnixty/window-dired-vc-root-left)
  :config
  (defun dnixty/window-dired-vc-root-left-toggle ()
    "Toggle root directory of current version-controlled repository
or the present working directory with `dired' and bespoke window
parameters."
    (interactive)
    (if (get-buffer "*Dired-Side*")
        (kill-buffer "*Dired-Side*")
      (let ((dir (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir)))))
        (display-buffer-in-side-window
         dir `((side . left)
               (slot . -1)
               (window-width . 0.16)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)
                                     (mode-line-format . (" "
                                                          mode-line-buffer-identification))))))
        (with-current-buffer dir
          (rename-buffer "*Dired-Side*")
          (setq-local window-size-fixed 'width)))))
  :bind ("<f11>" . dnixty/window-dired-vc-root-left-toggle))

;; Exwm
(use-package exwm
  :config
  (defun dnixty/exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ": "
             (if (<= (length exwm-title) 22)
                 exwm-title
               (concat (substring exwm-title 0 21) "…")))))
  (defun dnixty/capture-screen ()
    (interactive)
    (start-process "" nil "flameshot" "gui"))
  (defun dnixty/suspend-to-sleep ()
    (interactive)
    (call-process "loginctl" nil nil nil "lock-session"))
  (defun dnixty/switch-to-other ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (defun dnixty/describe-symbol-at-point ()
    (interactive)
    (let ((symbol (symbol-at-point)))
      (when symbol
        (describe-symbol symbol))))
  (defun dnixty/describe-symbol-at-point-switch ()
    (interactive)
    (dnixty/describe-symbol-at-point)
    (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window))))))
  (defun dnixty/icomplete-recentf ()
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file
       (completing-read "Open recentf entry: " files nil t))))
  (defun dnixty/eshell-multi ()
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (name (car (last (split-string parent "/" t)))))
      (with-current-buffer (eshell)
        (rename-buffer
         (generate-new-buffer-name (concat "*eshell: " name "*"))))))
  ;; Make sure that XF86 keys work in exwm buffers as well
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86AudioMute
               print
               f5))
    (cl-pushnew k exwm-input-prefix-keys))
  ;; Global keys
  (exwm-input-set-key (kbd "s-&") #'(lambda (command)
                                      (interactive (list (read-shell-command "$ ")))
                                      (start-process-shell-command command nil command)))
  (exwm-input-set-key (kbd "s-0") #'delete-window)
  (exwm-input-set-key (kbd "s-1") #'delete-other-windows)
  (exwm-input-set-key (kbd "s-2") #'split-window-below)
  (exwm-input-set-key (kbd "s-3") #'split-window-right)
  (exwm-input-set-key (kbd "s-b") #'switch-to-buffer)
  (exwm-input-set-key (kbd "s-B") #'switch-to-buffer-other-window)
  (exwm-input-set-key (kbd "s-d") #'dired)
  (exwm-input-set-key (kbd "s-D") #'dired-other-window)
  (exwm-input-set-key (kbd "s-f") #'find-file)
  (exwm-input-set-key (kbd "s-F") #'find-file-other-window)
  (exwm-input-set-key (kbd "s-h") #'dnixty/describe-symbol-at-point)
  (exwm-input-set-key (kbd "s-H") #'dnixty/describe-symbol-at-point-switch)
  (exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
  (exwm-input-set-key (kbd "s-k") #'kill-this-buffer)
  (exwm-input-set-key (kbd "s-o") #'other-window)
  (exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "s-p") #'password-store-copy)
  (exwm-input-set-key (kbd "s-P") #'password-store-otp-token-copy)
  (exwm-input-set-key (kbd "s-q") #'window-toggle-side-windows)
  (exwm-input-set-key (kbd "s-r") #'dnixty/icomplete-recentf)
  (exwm-input-set-key (kbd "s-R") #'exwm-reset)
  (exwm-input-set-key (kbd "s-v") #'magit-status)
  (exwm-input-set-key (kbd "s-X") #'exwm-input-toggle-keyboard)
  (exwm-input-set-key (kbd "s-Z") #'dnixty/suspend-to-sleep)
  (exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)
  (exwm-input-set-key (kbd "C-s-.") #'winner-redo)
  (exwm-input-set-key (kbd "C-s-,") #'winner-undo)
  (exwm-input-set-key (kbd "s-<return>") #'eshell)
  (exwm-input-set-key (kbd "s-S-<return>") #'dnixty/eshell-multi)
  (exwm-input-set-key (kbd "s-<tab>") #'dnixty/switch-to-other)
  (exwm-input-set-key (kbd "s-,") #'previous-buffer)
  (exwm-input-set-key (kbd "s-.") #'next-buffer)
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
  ;; Allow non-floating resizing with mouse
  (setq window-divider-default-bottom-width 2)
  (setq window-divider-default-right-width 2)
  (window-divider-mode)
  :bind (("C-x C-c" . save-buffers-kill-emacs))
  :hook ((exwm-update-class-hook . dnixty/exwm-rename-buffer)
         (exwm-update-title-hook . dnixty/exwm-rename-buffer)
         (exwm-floating-setup-hook . exwm-layout-hide-mode-line)
         (exwm-floating-exit-hook . exwm-layout-show-mode-line)))

(use-package exwm-randr
  :after exwm
  :demand t
  :commands exwm-randr-enable
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
        (dnixty/set-font)
        (exwm-randr-refresh))))
  (exwm-randr-enable)
  :hook (exwm-randr-screen-change-hook . dnixty/exwm-change-screen-hook))

;; Pulseaudio
(use-package pulseaudio-control
  :after exwm
  :config
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "2%")
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute))

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


;;; --------------------------------------------------------------------
;;; 4. Selection narrowing and search
;;; --------------------------------------------------------------------

(use-package minibuffer
  :config
  (use-package orderless
    :config
    (setq orderless-component-matching-styles
          '(orderless-regexp
            orderless-flex))
    :bind (:map minibuffer-local-completion-map
                ("SPC" . nil)))
  (setq completion-styles
        '(basic partial-completion initials orderless))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq completions-format 'vertical)
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

  :bind (:map completion-list-mode-map
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
  (defun dnixty/project-or-dir-find-subdirectory-recursive ()
    (interactive)
    (let* ((project (vc-root-dir))
           (dir (if project project default-directory))
           (contents (directory-files-recursively dir ".*" t nil nil))
           (find-directories (mapcar (lambda (dir)
                                       (when (file-directory-p dir)
                                         (abbreviate-file-name dir)))
                                     contents))
           (subdirs (delete nil find-directories)))
      (icomplete-vertical-do (:height (/ (window-height) 4))
        (dired
         (completing-read "Find sub-directory: " subdirs nil t dir)))))

  (defun dnixty/find-file-from-dir-recursive ()
    (interactive)
    (let* ((dir default-directory)
           (files (directory-files-recursively dir ".*" nil t)))
      (icomplete-vertical-do (:height (/ (window-height) 4))
        (find-file
         (completing-read "Find file recursively: " files nil t dir)))))

  (defun dnixty/find-project ()
    (interactive)
    (let* ((path "~/src")
           (dotless directory-files-no-dot-files-regexp)
           (project-list (project-combine-directories
                          (directory-files path t dotless)))
           (projects (mapcar 'abbreviate-file-name project-list)))
      (icomplete-vertical-do (:height (/ (window-height) 4))
        (dired
         (completing-read "Find project: " projects nil t path)))))

  :bind (("s-s p" . dnixty/find-project)
         ("s-s f" . project-find-file)
         ("s-s z" . dnixty/find-file-from-dir-recursive)
         ("s-s d" . dnixty/project-or-dir-find-subdirectory-recursive)
         ("s-s l" . find-library)
         ("s-s C-M-%" . project-query-replace-regexp)))

(use-package emacs
  :after (minibuffer icomplete icomplete-vertical)
  :config
  (defun contrib/completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
 Use as a value for `completion-in-region-function'."
    (let* ((initial (buffer-substring-no-properties start end))
           (all (completion-all-completions initial collection predicate
                                            (length initial)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all))) (car all))
                        (t (let ((completion-in-region-function
                                  #'completion--in-region))
                             (icomplete-vertical-do (:height (/ (window-height) 5))
                               (completing-read
                                "Completion: " collection predicate t initial)))))))
      (if (null completion)
          (progn (message "No completion") nil)
        (delete-region start end)
        (insert completion)
        t)))

  (setq completion-in-region-function #'contrib/completing-read-in-region))

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
  (global-unset-key (kbd "C-h h"))
  (global-unset-key (kbd "M-i")))

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
    (setq modus-operandi-theme-slanted-constructs t)
    (setq modus-operandi-theme-bold-constructs nil)
    (setq modus-operandi-theme-scale-headings nil)
    (setq modus-operandi-theme-proportional-fonts t)
    (dnixty/reapply-themes))
  (defun dnixty/modus-vivendi ()
    "Enable some Modus Vivendi variables and load the theme."
    (setq custom-enabled-themes '(modus-vivendi))
    (setq modus-vivendi-theme-slanted-constructs t)
    (setq modus-vivendi-theme-bold-constructs nil)
    (setq modus-vivendi-theme-scale-headings nil)
    (setq modus-vivendi-theme-proportional-fonts t)
    (dnixty/reapply-themes))
  (defun dnixty/theme-toggle ()
    "Toggle between sanityinc-tomorrow themes."
    (interactive)
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (dnixty/modus-vivendi)
      (dnixty/modus-operandi)))
  :hook (after-init-hook . dnixty/reapply-themes)
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
(use-package slime
  :config
  (defun dnixty/override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  :hook ((slime-repl-mode-hook . dnixty/override-slime-repl-bindings-with-paredit)
         (slime-repl-mode-hook . (lambda () (paredit-mode +1)))))

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

(use-package flyspell
  :config
  (setq ispell-program-name "aspell"))

;; Lookup word
(use-package ispell
  :commands ispell-get-word
  :config
  (defconst dnixty/dictionary-root "https://dictionary.cambridge.org/dictionary/english/")
  (defun dnixty/lookup-word (word)
    (interactive (list (save-excursion (car (ispell-get-word nil)))))
    (browse-url (format "%s%s" dnixty/dictionary-root word)))
  :bind ("M-#" . dnixty/lookup-word))

;; Winner mode
(use-package winner
  :hook (after-init-hook . winner-mode))

;; Delete selection
(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

;; Collection of unpackaged commands or tweaks
(use-package emacs
  :config
  (setq split-height-threshold nil)
  (setq split-width-threshold 130)
  :bind (("C-x k" . kill-this-buffer)
         ("M-i" . imenu)))


;;; --------------------------------------------------------------------
;;; 6. Programming languages
;;; --------------------------------------------------------------------

;; Nix Mode
(use-package nix-mode)

;; Recognise subwords
(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))


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
(use-package org
  :config
  (setq org-hide-leading-stars t))

;; Encryption
(use-package pinentry
  :config
  (setq-default epa-pinentry-mode 'loopback)
  :hook (after-init-hook . pinentry-start))

;; Eshell
(use-package em-term
  :config
  (setq eshell-destroy-buffer-when-process-dies t))
(use-package esh-mode
  :after em-hist
  :config
  (defun dnixty/eshell-file-parent-dir ()
    "Open `dired' with the parent directory of file at point."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (dired (file-name-directory file))))
  :bind :map eshell-mode-map
  ("M-k" . eshell-kill-input)
  ("C-c C-j" . dnixty/eshell-file-parent-dir)
  ("C-c C-l" . eshell/clear-scrollback))
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
  (defun dnixty/eshell-complete-recent-dir (&optional arg)
    (interactive "P")
    (let* ((dirs (ring-elements eshell-last-dir-ring))
           (dir (icomplete-vertical-do ()
                  (completing-read "Switch to recent dir: " dirs nil t))))
      (insert dir)
      (eshell-send-input)
      (when arg
        (dired dir))))
  :bind (:map eshell-hist-mode-map
              ("M-r" . dnixty/eshell-complete-history)
              ("C-c d" . dnixty/eshell-complete-recent-dir)))

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
  (setq slime-selector-other-window nil)
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--noinform"))
          (clisp ("clisp" "--quiet"))))
  (slime-setup '(slime-fancy)))

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
(use-package pass
  :defer
  :commands pass)

;; Pdf
(use-package pdf-tools
  :pin manual)
(use-package pdf-occur
  :pin manual
  :after pdf-tools
  :config
  (pdf-tools-install))

;; Dired
(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  :bind (:map dired-mode-map
              ("C-l" . dired-up-directory)
              ("C-+" . dired-create-empty-file))
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)
         (dired-mode-hook . auto-revert-mode)))
(use-package dired-x
  :after dired)
(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

;;; init.el ends here
