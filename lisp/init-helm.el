;;; Helm

(helm-mode 1)

(setq helm-follow-mode-persistent t
      helm-reuse-last-window-split-state t
      helm-display-header-line nil
      helm-echo-input-in-header-line t
      helm-show-completion-display-function nil
      helm-grep-save-buffer-name-no-confirm t
      helm-completion-mode-string ""
      helm-dwim-target 'completion
      helm-completion-style "helm-fuzzy"
      helm-ff-skip-boring-files t

      helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-completion-in-region-fuzzy-match t
      helm-eshell-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-library-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-etags-fuzzy-match t
      helm-ff-fuzzy-matching t
      helm-file-cache-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-session-fuzzy-match t
      helm-mode-fuzzy-match t

      helm-buffers-end-truncated-string "…"
      helm-buffer-max-length 22
      helm-window-show-buffers-function 'helm-window-mosaic-fn
      helm-split-window-default-side 'right
      helm-window-prefer-horizontal-split t)

(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap list-buffers] 'helm-mini)
(global-set-key [remap yank-pop] 'helm-show-kill-ring)
(global-set-key [remap apropos-command] 'helm-apropos)
(global-set-key [remap query-replace-regexp] 'helm-regexp)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h x") 'helm-register)

;; Descbinds
(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode))

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(helm-top-poll-mode)

;; Eshell
(defun dnixty/helm/eshell-set-keys ()
  (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
  (define-key eshell-mode-map (kbd "M-s") nil)
  (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all))
(add-hook 'eshell-mode-hook 'dnixty/helm/eshell-set-keys)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(defun dnixty/helm-external-command-cleanup-dotted (old-function &optional args)
  "Remove dotted programs from `helm-run-external-command' list."
  (funcall old-function args)
  (setq helm-external-commands-list
        (cl-delete-if (lambda (p) (string-prefix-p "." p))
                      helm-external-commands-list)))
(advice-add 'helm-external-commands-list-1
            :around #'dnixty/helm-external-command-cleanup-dotted)

(provide 'init-helm)
