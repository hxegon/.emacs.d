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

;; Load custom file
(load-file "~/.emacs.d/custom-file.el")

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
(global-linum-mode)                   ; Line number gutter everywhere

;; ----- ERGONOMICS -----

;; More vim like binding for for forward-paragraph and backward-paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; ergonomic join-line binding
(global-set-key (kbd "C-x j") 'join-line)


;; alternative to M-x
;;(global-set-key (kbd "C-<tab>") 'counsel-M-x)

;;; Fix for mac emacs not being able to set org timer sound b/c of dbus issue

;; Terminal notifier
;; requires 'brew install terminal-notifier'
;; stolen from erc-notifier

(defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")

(defun terminal-notifier-notify (title message)
  "Show a message with terminal-notifier-command."
  (start-process "terminal-notifier"
                 "terminal-notifier"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))

(defun my-say-something (msg)
  (start-process "my-say-something"
                 "my-say-something"
                 "/usr/bin/say"
                 msg))

(defun timed-notification (time msg)
  (interactive "sNotification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (run-at-time time nil (lambda (msg) (terminal-notifier-notify "Emacs" msg)) msg)
  (run-at-time time nil (lambda (msg) (my-say-something (cadr (split-string msg ":")))) msg))

(setq org-show-notification-handler
      (lambda (msg) (timed-notification nil msg)))

;; -------- Packages --------

;; - Theme
(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-hard t))

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
  (setq ivy-virtual-abbreviate 'abbreviate
      uniquify-min-dir-content 10)
  (setq ivy-height 10)
  (setq ivy-initial-inputs-alist nil))

;; ivy functions replicating many common helm features
(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu)
         ("C-x C-r" . counsel-recentf)
         ("C-x /" . counsel-projectile-rg)
         ("C-x f" . counsel-projectile-find-file)
         ("C-x p" . counsel-projectile-switch-project))
  :demand t
  :config
  (counsel-mode)
  :diminish counsel-mode)

;; isearch replacement using ivy
(use-package swiper
  :ensure t
  :demand t
  :bind (("C-s" . swiper)))

;; Smarter sorting algorithm for ivy
(use-package ivy-prescient
  :ensure t
  :after (ivy counsel)
  :demand t
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

;; project based operations
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t))

;; FIXME: Getting 'exceeded lisp max nesting depth' or something with this :\
;; keep track of delimiter nesting through layered syntax highlighting
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :after (clojure-mode)
;;   :hook ((prog-mode . rainbow-delimiters-mode)))

;; pretty status bar
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-modal-icon t))

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

;; Visual jump hints a la Easymotion
(use-package avy
  :ensure t
  :bind (("<backtab>" . avy-goto-word-0)
         ("C-<tab>" . avy-goto-char-timer)))

;; -------- Clojure support --------
;; syntax highlighting for .clj, .cljs, .cljc
(use-package clojure-mode
  :ensure t
  :defer t
  :after (flycheck-clj-kondo)
  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode)
   ("\\.edn\\'" . clojure-mode))
  :config
  (require 'flycheck-clj-kondo))

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
  :after (clojure-mode)
  :hook (clojure-mode emacs-lisp-mode lisp-mode)
  :init (setq parinfer-rust-auto-download t)
  :config (add-hook 'parinfer-rust-mode-hook #'show-paren-mode))

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

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (("C-z" . undo)
         ("C-S-z" . redo)))

(use-package lua-mode
  :ensure t
  :defer t
  :mode (("\\.lua\\'" . lua-mode)))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)))

;; Automatic code linting
(use-package flycheck
  :ensure t
  :after (exec-path-from-shell)
  :init (global-flycheck-mode))

;; Hook clojure style checker into flycheck
(use-package flycheck-clj-kondo
  :ensure t
  :after (flycheck))

;; Magical auto completion
(use-package company
  :ensure t
  :config (global-company-mode))

;; Magical emacs terminal finally (shamelessly cribbed from derek passen)
(use-package vterm
  :ensure t
  :defer t)

;; Distributed collaboration using conflict-free replicated data types
(use-package crdt
  :ensure t)

;; Work project configuration, auto load env vars and add env switching functions
(use-package guaranteed-emacs
  :load-path "~/Code/beacon/guaranteed-emacs"
  :custom (gr-source-directory "~/Code/beacon")
  :init (use-package dash :ensure t)
        (use-package dash-functional :ensure t)
        (use-package ht :ensure t)
        (use-package s :ensure t)
  :config (gri-dev))

;; Formats work clojure projects, needs manual triggering
(use-package zprint-format
  :ensure t
  :defer t)

;; Better window navigation and swapping
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?t ?n ?l ?a ?o ?e ?u)))

;; Resize windows automatically to intelligently utilize space
(use-package zoom
  :ensure t
  :config
  (zoom-mode)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

;(use-package slime-company
;  :after (slime company))

(use-package slime
  :ensure t
  :defer t
  :mode (("\\.cl\\'" . lisp-mode))
  :init
  (setq slime-lisp-implementations '((sbcl ("sbcl")))
        slime-default-lisp 'sbcl)
  :hook (lisp-mode . slime-mode))

(use-package csv-mode
  :defer t)
