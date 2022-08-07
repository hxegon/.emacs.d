;;; my-ivy.el --- lightweight generic completion/search engine
;;; Code:

;; fuzzy search todo:
;; https://oremacs.com/2016/01/06/ivy-flx/

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate
      uniquify-min-dir-content 10)
  (setq ivy-height 10)
  ;; Why did I add this? what does this do?
  (setq ivy-initial-inputs-alist nil)
  ;; Thought this was supposed to put a delay on ivy updating the results when typing but it doesn't.
  ;; Because it's the wrong thing or because it's broken?
  (setq ivy-dynamic-exhibit-delay-ms 350))

;; isearch replacement using ivy
(use-package swiper
  :ensure t
  :demand t
  :bind (("C-s" . swiper)))

(use-package ivy-hydra
  :after (hydra)
  :ensure t)

;; Smarter sorting algorithm for ivy
(use-package ivy-prescient
  :ensure t
  :after (ivy)
  :demand t
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu)
         ("C-c f" . counsel-flycheck)
         ("C-x C-r" . counsel-recentf))
  :config
  (counsel-mode))

;;; Commentary:
(provide 'my-ivy)
;;; my-ivy.el ends here
