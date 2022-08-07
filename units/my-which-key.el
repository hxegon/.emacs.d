;;; my-which-key.el --- binding discoverability hints
;;; Code:

;; Show guide for key combos
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-show-transient-maps t)
  :config
  (which-key-mode))

;;; Commentary:
(provide 'my-which-key)
;;; my-which-key.el ends here
