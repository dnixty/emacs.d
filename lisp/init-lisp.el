;;; Lisp

(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(defun dnixty/slime-switch-to-repl ()
  (interactive)
  (require 'slime)
  (pcase (length slime-net-processes)
    (0 (slime))
    (1 (if (and (eq (current-buffer) (slime-output-buffer))
                (require 'helm-slime nil 'no-error))
           (helm-slime-mini)
         (pop-to-buffer (slime-output-buffer))))
    (_ (if (require 'helm-slime nil 'noerror)
           (helm-slime-mini)
         (pop-to-buffer (slime-output-buffer))))))

(defun dnixty/slime-rainbow-init ()
  (font-lock-mode -1)
  (rainbow-delimiters-mode)
  (font-lock-mode))

(with-eval-after-load 'slime
  (when (require 'paredit nil t)
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
  (setq slime-defpackage-regexp
        "^(\\(cl:\\|common-lisp:\\|uiop:\\)?\\(defpackage\\|define-package\\)\\>[ \t']*")

  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--noinform"))
          (clisp ("clisp"))))
  (let ((slime-extra '(slime-fancy)))
    (when (ignore-errors (find-library-name "slime-company"))
      (add-to-list 'slime-extra 'slime-company))
    (slime-setup slime-extra)
    (add-hook 'slime-repl-mode-hook #'dnixty/slime-rainbow-init))))

(when (require 'helm-slime nil 'noerror)
  (global-helm-slime-mode)
  (add-to-list 'helm-source-names-using-follow "SLIME xrefs"))


(provide 'init-lisp)
