;;; base.el --- describe
;;; Code:

(setq-default
 ;; Don't show default startup screen.
 inhibit-startup-message t

 ;; Don't use compiled code if there's a newer package.
 load-prefer-newer t

 enable-recursive-minibuffers t

 ;; Do not put 'custom' config in init.el; put in another file.
 custom-file "~/.emacs.d/custom-file.el"

 ;; Don't create lockfiles
 create-lockfile nil

 ;; Don't use hard tabs
 indent-tabs-mode nil

 ;; Put backup files in their own folder: ~/.emacs.d/backups
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Do not autosave
 auto-save-default nil

 ;; Replace obnoxious 'bell' sound with a visual indicator.
 visible-bell t

 ;; Try to keep 10 lines of context between cursor and end of window
 scroll-margin 8

 ;; Immediately select help window
 help-window-select t

 ;; Change elisp indentation offset from default of 4 -> 2
 lisp-indent-offset 2)


(fset 'yes-or-no-p 'y-or-n-p) ; Change all yes/no questions to y/n
(tool-bar-mode -1)            ; disable gui emacs toolbar
(menu-bar-mode -1)            ; disable menu bar
(scroll-bar-mode -1)          ; disable gui scrollbar
(line-number-mode t)          ; add line numbers to status bar
(visual-line-mode t)          ; Toggles visual line mode: wraps lines and tweaks some commands accordingly

;; initial hooks

;; Delete whitespace before a file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'text-mode-hook #'linum-mode)

(add-hook 'prog-mode-hook 'show-paren-mode)

(load-file "~/.emacs.d/custom-file.el")

;; Fallback good M-o binding
(global-set-key (kbd "M-o") 'other-window)

;;; DEFAULT PACKAGES

;; Fixes path issues present when on using a gui emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x)) ; Only run when using a graphical emacs
    (exec-path-from-shell-initialize)))

(use-package saveplace
  :config (save-place-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/.emacs.d/elpa/.*"))
  (recentf-mode t))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package diminish
  :ensure t)

;;; Commentary:
(provide 'base)
;;; base.el ends here
