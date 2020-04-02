;;;; Javascript

(require 'flycheck)
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

;;; Enable JSX syntax highlight
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;;; Indentation
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'web-mode-init-hook)

;;; Prettier
(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))
(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq typescript-indent-level 2)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)

(provide 'init-javascript)
