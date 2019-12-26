;;; Visual

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

(provide 'visual)
