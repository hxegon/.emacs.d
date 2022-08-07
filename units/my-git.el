;;; my-git.el --- describe
;;; Code:

;; TODO: Move git hydra into here

;; Git integration

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

;; :commands browse-repo and browse-at-remote?
(use-package browse-at-remote
  :ensure t
  :after (projectile)
  :config
  (defun browse-repo ()
    (interactive)
    (-> (projectile-acquire-root)
        browse-at-remote--file-url
        browse-url)))

;;; Commentary:
(provide 'my-git)
;;; my-git.el ends here
