;;;; Helpful

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h o") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h C") #'helpful-command)

(provide 'init-helpful)
