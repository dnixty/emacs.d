;; -- Prerequisites --------------------------------------------------

;; Store additional config in a 'lisp' subfolder
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Move user-emacs-directory so that user files don't mix with cache files.
(setq user-emacs-directory "~/.cache/emacs/")

(require 'main)
(require 'visual)

;; -- Packages -------------------------------------------------------

;; Eshell
(with-eval-after-load 'eshell
  (require 'init-eshell))

;; Exwm
(with-eval-after-load 'exwm
  (require 'init-exwm))

;; Haskell
(with-eval-after-load 'haskell-mode
  (require 'init-haskell))

;; Helm
(when (require 'helm-config nil t) (require 'init-helm))

;; Helpful
(when (require 'helpful nil t) (require 'init-helpful))

;; Org
(with-eval-after-load 'org
  (require 'init-org))
