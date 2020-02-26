;;; Dired
(setq dired-guess-shell-alist-user
      (list
       '("\\.ogg$" "mpv")
       '("\\.\\(jpe?g\\|png\\|git\\)$" "sxiv")
       '("\\.\\(mkv\\|mpe?g\\|avi\\|mp4\\|ogm\\)$" "mpv")))

(when (executable-find "sxiv")
  (setq image-dired-external-viewer "sxiv"))

(defun dnixty/image-dired-setup ()
  (add-hook 'window-configuration-change-hook 'image-dired-line-up-dynamic nil t))
(add-hook 'image-dired-thumbnail-mode-hook 'dnixty/image-dired-setup)

(provide 'init-dired)
