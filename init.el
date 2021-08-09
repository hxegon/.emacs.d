;; -- cribbed from: https://github.com/suvratapte/dot-emacs-dot-d/blob/e7f44d981004b44bb9ae23b6b67c421404ea6b4e/init.el#L19 --

;; -------- Set up 'package' --------
(require 'package)

;; add melpa to package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; load and activate emacs packages. Done first to ensure packages are loaded before they are modified later. This also sets the load path
(package-initialize)

;; Install 'use-package' if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Early packages
(use-package no-littering ; keep emacs clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; Defaults (No packages, no custom stuff, just changing stuff from default config)

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
 scroll-margin 10

 ;; Immediately select help window
 help-window-select t)

;; Change all yes/no questions to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Rebind M-o as 'other-window'
;(global-set-key (kbd "M-o") 'other-window)

;; Delete whitespace before a file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(tool-bar-mode -1) ; disable gui emacs toolbar
(menu-bar-mode -1) ; disable menu bar
(scroll-bar-mode -1) ; disable gui scrollbar
(global-hl-line-mode t) ; highlight current line
(line-number-mode t) ; add line numbers to status bar

;; -------- Packages --------

;; Recent buffers
(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (add-to-list 'recentf-exclude (concat (getenv "HOME") "/.emacs.d/elpa/.*"))
  (recentf-mode t))

;; Better buffer management
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package diminish
  :ensure t)

;; Show guide for key combos
(use-package which-key
  :ensure t
  :config
  (which-key-mode t)
  :diminish which-key-mode)

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-initial-inputs-alist nil))

;; ivy functions replicating many common helm features
(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu)
         ("C-x C-r" . counsel-recentf))
  :config
  (counsel-mode)
  :diminish counsel-mode)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  :bind (("C-x f" . projectile-find-file)
         ("C-x p" . projectile-switch-project)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode 'rainbow-delimiters-mode))

(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker t))

(use-package powerline
  :ensure t
  :config (powerline-default-theme))

(use-package avy
  :ensure t
  :defer t
  :bind (("C-;" . avy-goto-char)))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter-fringe
  :ensure t
  :config (global-git-gutter-mode +1))
