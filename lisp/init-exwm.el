;;; Exwm

(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;; Rename buffer to window title
(defun dnixty/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'dnixty/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-o") #'other-window)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)

(if (not (fboundp 'helm-eshell-switch))
    (exwm-input-set-key (kbd "s-<return>") #'eshell)
  (exwm-input-set-key (kbd "s-<return>") #'helm-eshell-switch)
  (exwm-input-set-key (kbd "S-s-<return>") #'helm-eshell-switch-other-window))
(exwm-input-set-key (kbd "s-z") (lambda ()
                                  (interactive)
                                  (start-process "" nil "slock")))

(exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)
(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(exwm-input-set-key (kbd "s-<tab>") (lambda ()
                                      (interactive)
                                      (switch-to-buffer (other-buffer (current-buffer) 1))))

(exwm-input-set-key (kbd "s-n") #'elfeed)

(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))

(when (fboundp 'helm-pass)
  (exwm-input-set-key (kbd "s-p") #'helm-pass))

(with-eval-after-load 'helm
  (exwm-input-set-key (kbd "s-c") #'helm-resume)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-F") #'helm-locate)
  (exwm-input-set-key (kbd "s-r") #'helm-run-external-command))

;; Pulseaudio
(when (require 'pulseaudio-control nil t)
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "2%")
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute))

;; Simulation keys
(exwm-input-set-simulation-keys
 '(
   ([?\C-b] . left)
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

;; Make sure that XF86 keys work in exwm buffers as well
(dolist (k '(XF86AudioLowerVolume
             XF86AudioRaiseVolume
             XF86AudioMute
             print))
  (cl-pushnew k exwm-input-prefix-keys))

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
        (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1)))))))
(require 'exwm-randr)
(add-hook 'exwm-randr-screen-change-hook 'dnixty/exwm-change-screen-hook)
(exwm-randr-enable)

(provide 'init-exwm)
