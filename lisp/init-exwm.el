;;; Exwm

(exwm-input-set-key (kbd "s-R") 'exwm-reset)
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-&") (lambda (command)
                                  (interactive (list (read-shell-command "$ ")))
                                  (start-process-shell-command command nil command)))
(exwm-input-set-key (kbd "s-<return>") 'eshell)
(exwm-input-set-key (kbd "s-z") (lambda ()
                                  (interactive)
                                  (start-process "" nil "slock")))
(exwm-input-set-key (kbd "s-SPC") #'exwm-floating-toggle-floating)
(exwm-input-set-key (kbd "s-<tab>") (lambda ()
                                      (interactive)
                                      (switch-to-buffer (other-buffer (current-buffer) 1))))
(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
(setq exwm-input-global-keys
      `(,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(when (fboundp 'helm-pass)
  (exwm-input-set-key (kbd "s-p") #'helm-pass))

(with-eval-after-load 'helm
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files))

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
