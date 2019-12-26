;;; Pdf

(setq-default pdf-view-display-size 'fit-page)
(when (require 'pdf-occur nil t)
  (pdf-tools-install))

(provide 'init-pdf)
