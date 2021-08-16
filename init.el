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

(tool-bar-mode -1)                    ; disable gui emacs toolbar
(menu-bar-mode -1)                    ; disable menu bar
(scroll-bar-mode -1)                  ; disable gui scrollbar
(global-hl-line-mode t)               ; highlight current line
(line-number-mode t)                  ; add line numbers to status bar

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

;; Lightweight generic completion engine
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

;; isearch replacement using ivy
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;; Smarter sorting algorithm for ivy
(use-package ivy-prescient
  :ensure t
  :after (ivy counsel)
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

;; project based operations
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  :bind (("C-x f" . projectile-find-file)
         ("C-x p" . projectile-switch-project)))

;; keep track of delimiter nesting through layered syntax highlighting
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode #'rainbow-delimiters-mode))

;; Simple dark theme
;; (use-package gruber-darker-theme
;;   :ensure t
;;   :init (load-theme 'gruber-darker t))

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn t))

;; pretty status bar
(use-package powerline
  :ensure t
  :config (powerline-default-theme))

;; EasyMotion style code navigation
(use-package avy
  :ensure t
  :defer t
  :bind (("C-;" . avy-goto-char)))

;; Git integration
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

;; Show changed lines on the left
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))

;; group sequencable commands together
(use-package hydra
  :ensure t
  :after (cider projectile avy)
  :config
  (defhydra common (global-map "`")
    "Common actions"
    ("`" (lambda () (interactive) (insert "`")) "insert literal ` character and exit" :exit t)
    ("." enlarge-window "enlarge window vertically" :column "GUI")
    ("," shrink-window "shrink window vertically" :column "GUI")
    (">" enlarge-window-horizontally "enlarge window horizontally" :column "GUI")
    ("<" shrink-window-horizontally "shrink window horizontally" :column "GUI")
    ("+" text-scale-increase "increase text size" :column "GUI")
    ("-" text-scale-decrease "decrease text size" :column "GUI")
    ("L" linum-mode "toggle line numbers" :exit t :column "GUI")
    ("e" projectile-run-eshell "open eshell in project root" :exit t :column "RUN")
    ("TAB" avy-goto-word-0 "avy goto word 0" :exit t :column "NAV")
    ("E" (lambda () (interactive) (find-file "~/.emacs.d/init.el")) "Open emacs config" :exit t :column "NAV")
    ("C" cider-clojuredocs "Show clojuredocs" :exit t :column "LANG")
    ("S" cider-repl-set-ns "Set cider ns to that of the current buffer" :exit t :column "LANG")))

;; -------- Clojure support --------
;; syntax highlighting for .clj, .cljs, .cljc
(use-package clojure-mode
  :ensure t
  :defer t
  :after rainbow-delimiters
  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.edn\\'" . clojure-mode))
  :hook (show-paren-mode rainbow-delimiters-mode))

;; interactive clojure programming support
(use-package cider
  :ensure t
  :defer t
  :after clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  :config
  (cider-repl-toggle-pretty-printing))

(use-package parinfer-rust-mode
  :ensure t
  :defer t
  :init
  (setq parinfer-rust-auto-download t)
  :hook
  (emacs-lisp-mode
   clojure-mode))

;; Fixes path issues present when on using a gui emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x)) ; Only run when using a graphical emacs
    (exec-path-from-shell-initialize)))

;; Package for markdown files, mostly used for syntax highlighting and org-mode style folding
(use-package markdown-mode
  :ensure t
  :defer t)
