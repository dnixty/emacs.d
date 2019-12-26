;;; Helm

(helm-mode 1)

(setq helm-follow-mode-persistent t
      helm-reuse-last-window-split-state t
      helm-display-header-line nil
      helm-show-completion-display-function nil
      helm-completion-mode-string ""
      helm-dwim-target 'completion
      helm-echo-input-in-header-line t
      helm-use-frame-when-more-than-two-windows nil
      helm-grep-save-buffer-name-no-confirm t

      helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t

      helm-buffers-end-truncated-string "…"
      helm-buffer-max-length 22

      helm-window-show-buffers-function 'helm-window-mosaic-fn
      helm-window-prefer-horizontal-split t)

(defun dnixty/helm-split-window-combined-fn (window)
  "Helm window splitting that combined most standard features.

- With C-u, split inside. With C-u C-u, use same window.
- Else use biggest other window when available.
- Else split horizontally if width>height, vertically otherwise."
  (cond
   ((or (minibufferp helm-current-buffer)
        (and
         (not (one-window-p t))
         (not (equal current-prefix-arg '(4)))
         (not (equal current-prefix-arg '(16)))))
    ;; Find biggest window.
    (let (biggest (maxarea 0))
      (dolist (w (window-list))
        (unless (eq w (selected-window))
          (let ((area (* (window-pixel-width w) (window-pixel-height w))))
            (when (> area maxarea)
              (setq maxarea area
                    biggest w)))))
      biggest))
   ((equal current-prefix-arg '(16))
    ;; Same window.
    (selected-window))
   (t
    ;; If split inside or if unique window.
    (split-window (selected-window) nil
                  (if (> (window-pixel-width) (window-pixel-height))
                      'right
                    'below)))))
(setq helm-split-window-preferred-function 'dnixty/helm-split-window-combined-fn)

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
