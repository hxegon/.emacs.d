;;; my-paredit.el --- describe
;;; Code:

(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))

(use-package evil-paredit
  :ensure t
  :after (evil)
  :hook (paredit-mode . evil-paredit-mode)
  :config
  (evil-define-key '(normal visual) evil-paredit-mode-map
    "H" #'paredit-backward-up
    "L" #'paredit-forward-up))

;;; Commentary:
(provide 'my-paredit)
;;; my-paredit.el ends here
