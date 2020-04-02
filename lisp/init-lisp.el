;;;; Lisp

(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(defun dnixty/slime-rainbow-init ()
  (font-lock-mode -1)
  (rainbow-delimiters-mode)
  (font-lock-mode))

(with-eval-after-load 'slime
  (when (require 'paredit nil t)
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--noinform"))
          (clisp ("clisp"))))
  (slime-setup '(slime-fancy))
  (add-hook 'slime-repl-mode-hook #'dnixty/slime-rainbow-init)))

(when (require 'helm-slime nil 'noerror)
  (global-helm-slime-mode)
  (add-to-list 'helm-source-names-using-follow "SLIME xrefs"))


(provide 'init-lisp)
