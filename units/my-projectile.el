;;; my-projectile.el --- describe
;;; Code:

;; project based operations
(use-package projectile
  :ensure t
  :config (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel)
  :bind (("C-x /" . counsel-projectile-rg)
         ("C-x f" . counsel-projectile-find-file)
         ("C-x p" . counsel-projectile-switch-project)))

;;; Commentary:
(provide 'my-projectile)
;;; my-projectile.el ends here
