;; -- Prerequisites --------------------------------------------------

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun dnixty/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'dnixty/reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun dnixty/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook 'dnixty/reset-file-name-handler-alist)

;; Store additional config in a 'lisp' subfolder
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Move user-emacs-directory so that user files don't mix with cache files.
(setq user-emacs-directory "~/.cache/emacs/")

(require 'main)
(require 'visual)

;; -- Packages -------------------------------------------------------

;; Dired
(with-eval-after-load 'dired (require 'init-dired))

;; Elfeed
(with-eval-after-load 'elfeed
  (require 'init-elfeed))

;; Emacs Lisp
(when (require 'rainbow-delimiters nil t)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; Eshell
(with-eval-after-load 'eshell
  (require 'init-eshell))

;; Expand Region
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=")'er/expand-region))

;; Exwm
(with-eval-after-load 'exwm
  (require 'init-exwm))

;; Company
(when (require 'company nil t)
  (require 'init-company))

;; Haskell
(with-eval-after-load 'haskell-mode
  (require 'init-haskell))

;; Helm
(when (require 'helm-config nil t) (require 'init-helm))

;; Helpful
(when (require 'helpful nil t) (require 'init-helpful))

;; Javascript
(when (require 'tide nil t)
  (require 'init-javascript))

;; Ledger
(add-to-list 'auto-mode-alist '("\\.ldg$" . ledger-mode))
(add-hook 'ledger-mode 'flycheck-mode)
(with-eval-after-load 'ledger-mode
  (flycheck-mode +1)
  (eval-after-load 'flycheck '(require 'flycheck-ledger)))

;; Lisp
(setq inferior-lisp-program "clisp")
(with-eval-after-load 'lisp-mode
  (require 'init-lisp))

;; Neotree
(when (require 'neotree nil t)
  (require 'init-neotree))

;; Org
(with-eval-after-load 'org
  (require 'init-org))

;; Pdf
(when (require 'pdf-tools nil t)
  (require 'init-pdf))

;; Projectile
(when (require 'projectile nil t)
  (require 'init-projectile))

;; Scheme
(with-eval-after-load 'scheme (require 'init-scheme))
