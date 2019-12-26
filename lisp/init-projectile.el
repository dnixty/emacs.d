;;; Projectile

(projectile-mode)
(setq projectile-completion-system 'helm
      projectile-project-search-path '("~/src"))

(when (require 'helm-projectile nil t)
  (helm-projectile-on))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'init-projectile)
