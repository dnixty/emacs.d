;;; Main

;; Disable GUI components
(setq use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-h h"))

;; Place backup files in specific directory.
(setq backup-directory-alist
`(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Make questions less annoying.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Indentation
(setq-default tab-width 2
        indent-tabs-mode nil)
(defvaralias 'standard-indent 'tab-width)

;; Kill whole line including \n.
(setq kill-whole-line t)

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Remember last cursor position
(save-place-mode)
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)

;; Clipboard and primary selection
(setq select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; Scroll line by line
(setq scroll-step 1)

;; Replace not-so-useful comment-dwim binding
(global-set-key (kbd "M-;") 'comment-line)

;; Replace `kill-buffer' binding by `kill-this-buffer'.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Set default mode to text-mode
(setq-default major-mode 'text-mode)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Show matchingparenthesis
(show-paren-mode)
(setq show-paren-delay 0
      show-paren-when-point-inside-parent t)

;; Initial scratch message
(setq initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string)))

;; Calendar
(setq calendar-week-start-day 1
      calendar-date-style 'iso)

;; Zap up to char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Unique buffer names style
(setq uniquify-buffer-name-style 'forward)

;; Replace `dabbrev-expand' binding by `hippie-expand'.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Status bar
(setq display-time-24hr-format t
      display-time-default-load-average nil)
(setq battery-mode-line-format " [%p%%%b]")
(display-time)
(display-battery-mode)

;; Disable autosave features
(setq auto-save-default nil
      auto-save-list-file-prefix nil)

;; Turn on column number mode
(setq column-number-mode t)

;; Ignore selected directories on grep
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")))

;; Increase large file warning threshold
(setq large-file-warning-threshold 100000000)

;; Encryption
(setq-default epa-pinentry-mode 'loopback)
(when (require 'pinentry nil t)
  (pinentry-start))

;; Enforce horizontal splitting
;; TODO: change `split-width-threshold' to 140 after screen upgrade
(setq split-height-threshold nil
      split-width-threshold 130)

;; Save bookmarks on any change
(setq bookmark-save-flag 1)

;; Move point to top/bottom before signaling an error
(setq scroll-error-top-bottom t)

;; Show keystrokes after 0.5s
(setq echo-keystrokes 0.5)

(provide 'main)
