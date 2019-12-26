;;; Eshell

;; Increase eshell history ring size
(setq eshell-hisotry-size 1024)

;; Make sure items in the eshell history are unique
(setq eshell-hist-ignoredups t)

;; Destroy eshell buffers after their processes die
(setq eshell-destroy-buffer-when-process-dies t)

(defun eshell-new ()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(provide 'init-eshell)
