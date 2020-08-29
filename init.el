;;; Prerequisites
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))


;;; Visual
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)
(setq default-frame-alist '((font . "Spleen-12")
                            (background-color . "black")
			    (foreground-color . "#aaaaaa")))
(set-face-foreground 'font-lock-comment-face "white")
(define-advice font-lock-fontify-keywords-region
   (:around (f &rest args) do-nothing))
(add-hook 'font-lock-mode-hook
         (lambda ()
           (add-function
            :filter-return (local 'font-lock-syntactic-face-function)
            (lambda (face)
              (unless (memq face '(font-lock-doc-face
                                   font-lock-string-face))
                face)))))
(defadvice set-face-attribute
    (before no-bold (face frame &rest args) activate)
  (setq args
        (mapcar (lambda(x) (if (eq x 'bold) 'normal x))
                args)))


;;; Base settings
(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq custom-file
      (expand-file-name (format "emacs-custom-%s.el" (user-uid))
			temporary-file-directory))
(setq create-lockfiles nil)
(setq initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string)))
(setq select-enable-primary t)
(setq scroll-error-top-bottom t)
(setq history-length 30000)
(setq completion-show-help nil)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq resize-mini-windows t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq ls-lisp-use-insert-directory-program nil)
(setq dired-listing-switches "-AFhl")
(setq vc-follow-symlinks t)

(save-place-mode)
(icomplete-mode)
(savehist-mode)
(recentf-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;; Programming languages
(setq c-default-style "bsd")


;;; Packages
(require 'ls-lisp)
(require 'dired-x)
(require 'magit)
(require 'diff-hl)

(setq diff-hl-draw-borders nil)
(add-hook 'after-init-hook 'global-diff-hl-mode)

;;; Key bindings
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "M-RET") 'eshell)
(global-set-key (kbd "C-c v") 'magit)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(let ((map icomplete-minibuffer-map))
  (define-key map (kbd "<return>") 'icomplete-force-complete-and-exit)
  (define-key map (kbd "C-n") 'icomplete-forward-completions)
  (define-key map (kbd "C-p") 'icomplete-backward-completions)
  (define-key map (kbd "C-j") 'exit-minibuffer))
(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)
