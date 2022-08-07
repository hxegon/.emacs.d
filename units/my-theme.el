;;; my-theme.el --- My theming config
;;; Code:

;; TODO how to get modeline to play nice with this theme
;; (use-package nano-theme
;;   :ensure t
;;   :config (nano-light))

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-dracula))
  ;; :config (load-theme 'doom-flatwhite))
  ;; :config (load-theme 'doom-nord))
  ;; :config (load-theme 'doom-zenburn))

;;; Commentary:
(provide 'my-theme)
;;; my-theme.el ends here
