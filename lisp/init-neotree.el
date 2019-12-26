;;; Neotree

(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme 'ascii)
(setq-default neo-show-hidden-files t)
(add-hook 'neo-after-create-hook
          (lambda (&rest _) (display-line-numbers-mode -1)))

(provide 'init-neotree)
