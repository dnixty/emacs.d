;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move user-emacs-directory so that user files don't mix with cache files.
(setq user-emacs-directory "~/.cache/emacs/")

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

;; Configure pinentry
(setq epa-pinentry-mode 'loopback)
(when (require 'pinentry nil t)
  (pinentry-start))

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

;; Replace `list-buffers' binding by `buffer-menu'.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Status bar
(setq display-time-24hr-format t
      display-time-default-load-average nil)
(setq battery-mode-line-format " [%p%%%b]")
(display-time)
(display-battery-mode)

;; Disable autosave features
(setq auto-save-default nil
      auto-save-list-file-prefix nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increase eshell history ring size
(setq eshell-hisotry-size 1024)

;; Make sure items in the eshell history are unique
(setq eshell-hist-ignoredups t)

;; Destroy eshell buffers after their processes die
(setq eshell-destroy-buffer-when-process-dies t)

(defun eshell-new ()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exwm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "s-R") 'exwm-reset)
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-r") (lambda (command)
                                    (interactive (list (read-shell-command "$ ")))
                                    (start-process-shell-command command nil command)))
  (exwm-input-set-key (kbd "s-<return>") 'eshell)
  (exwm-input-set-key (kbd "s-z") (lambda ()
                                    (interactive)
                                    (start-process "" nil "slock")))
  (setq exwm-input-global-keys
        `(,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
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
              ;; Turn off all monitors that are not DEFAULT-OUTPUT.
              ;; See "--auto" in xrandr(1) and https://github.com/ch11ng/exwm/issues/529.
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
        (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1)))))))
  (require 'exwm-randr)
  (add-hook 'exwm-randr-screen-change-hook 'dnixty/exwm-change-screen-hook)
  (exwm-randr-enable))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set font
(add-to-list 'default-frame-alist '(font . "Hack-12"))

;; Consider all themes safe to load
(setq custom-safe-themes t)

;; Theme
(when (require 'gruvbox nil t)
  (setq-default custom-enabled-themes '(gruvbox-dark-hard))
  (defun dnixty/reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
  (add-hook 'after-init-hook 'dnixty/reapply-themes))
(defun dnixty/light ()
  "Activate light color theme."
  (interactive)
  (setq custom-enabled-themes '(gruvbox-light-hard))
  (dnixty/reapply-themes))
(defun dnixty/dark ()
  "Activate dark color theme."
  (interactive)
  (setq custom-enabled-themes '(gruvbox-dark-hard))
  (dnixty/reapply-themes))
