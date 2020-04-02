;;;; Scheme

(when (require 'rainbow-delimiters nil t)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

(provide 'init-scheme)
