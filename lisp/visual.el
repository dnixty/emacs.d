;;;; Visual

;;; Set font
(add-to-list 'default-frame-alist '(font . "Hack-11"))

;;; Consider all themes safe to load
(setq custom-safe-themes t)

;;; Theme
(when (require 'color-theme-sanityinc-tomorrow nil t)
  (setq-default custom-enabled-themes '(sanityinc-tomorrow-day))
  (defun dnixty/reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
  (add-hook 'after-init-hook 'dnixty/reapply-themes))
(defun dnixty/day ()
  "Activate light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (dnixty/reapply-themes))
(defun dnixty/night ()
  "Activate dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-night))
  (dnixty/reapply-themes))

(provide 'visual)
