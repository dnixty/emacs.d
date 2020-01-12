;;; Helm

(helm-mode 1)

(setq helm-reuse-last-window-split-state nil
      helm-display-header-line nil
      helm-echo-input-in-header-line t
      helm-grep-save-buffer-name-no-confirm t
      helm-mode-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-eshell-fuzzy-match t
      helm-locate-library-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-buffers-end-truncated-string "…"
      helm-buffer-max-length 22
      helm-split-window-default-side 'right
      helm-show-completion-display-function nil
      helm-window-prefer-horizontal-split t)

(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap list-buffers] 'helm-mini)
(global-set-key [remap yank-pop] 'helm-show-kill-ring)
(global-set-key [remap apropos-command] 'helm-apropos)
(global-set-key [remap query-replace-regexp] 'helm-regexp)

;; Eshell
(defun dnixty/helm/eshell-set-keys ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
  (define-key eshell-mode-map (kbd "M-s") nil)
  (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all))
(add-hook 'eshell-mode-hook 'dnixty/helm/eshell-set-keys)

(provide 'init-helm)
