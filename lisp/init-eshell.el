;;;; Eshell

;;; Use Tramp to use Eshell as root
(require 'em-tramp)
(setq password-cache t)
(setq passwrod-cache-expiry 3600)


;;; Increase eshell history ring size
(setq eshell-history-size 1024)

;;; Make sure items in the eshell history are unique
(setq eshell-hist-ignoredups t)

;;; Destroy eshell buffers after their processes die
(setq eshell-destroy-buffer-when-process-dies t)

;;; Auto-suggestion
(when (require 'esh-autosuggest nil t)
  (setq esh-autosuggest-delay 0.75)
  (add-hook 'eshell-mode-hook 'esh-autosuggest-mode)
  (define-key esh-autosuggest-active-map (kbd "<tab>") 'company-complete-selection)
  (when (require 'helm-config nil t)
    (define-key company-active-map (kbd "M-p") 'helm-eshell-history)))

(provide 'init-eshell)
