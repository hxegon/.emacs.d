;;; my-smartparens.el --- describe
;;; Code:

(use-package evil-smartparens
  :ensure t
  :after (smartparens evil)
  :hook (smartparens-strict-mode . evil-smartparens-mode)
  :config
  (evil-define-key '(normal visual) evil-smartparens-mode-map
    "L" #'sp-forward-sexp
    "H" #'sp-backward-sexp))

(use-package smartparens
  :ensure t
  :hook
  (emacs-lisp-mode . smartparens-strict-mode)
  :config
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil))

;;; Commentary:
(provide 'my-smartparens)
;;; my-smartparens.el ends here
