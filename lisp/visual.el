;;;; Visual

(setq underline-minimum-offset 0)

(defconst dnixty/fixed-pitch-font "Hack")

(defconst dnixty/fixed-pitch-params ":hintstyle=hintslight")

(defun dnixty/default-font (family size)
  (set-frame-font
   (concat family "-" (number-to-string size) dnixty/fixed-pitch-params) t t))

(defun dnixty/laptop-fonts ()
  (interactive)
  (when window-system
    (dnixty/default-font dnixty/fixed-pitch-font 11)))

(defun dnixty/desktop-fonts ()
  (interactive)
  (when window-system
    (dnixty/default-font dnixty/fixed-pitch-font 14)))

(defun dnixty/fonts-per-monitor ()
  (interactive)
  (when window-system
    (if (<= (display-pixel-width) 1366)
        (dnixty/laptop-fonts)
      (dnixty/desktop-fonts))))

(add-hook 'window-setup-hook 'dnixty/fonts-per-monitor)

;;; Consider all themes safe to load
(setq custom-safe-themes t)

;;; Theme
(when (require 'color-theme-sanityinc-tomorrow nil t)
  (setq-default custom-enabled-themes '(sanityinc-tomorrow-night))
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
(defun dnixty/bright ()
  "Activate bright color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (dnixty/reapply-themes))

(provide 'visual)
