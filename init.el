;;; init.el --- Cooper LeBrun

;;; Commentary:
;; fill this out

;;; -- initially cribbed from: https://github.com/suvratapte/dot-emacs-dot-d/blob/e7f44d981004b44bb9ae23b6b67c421404ea6b4e/init.el#L19 --

;; -------- Set up 'package' --------
(require 'package)


;;; Code:

;; add melpa to package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; load and activate emacs packages. Done first to ensure packages are loaded before they are modified later. This also sets the load path
(package-initialize)

;; Install 'use-package' if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;; Early packages

(use-package no-littering ; keep emacs clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; Non-package config

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

;; Enable current line highlighting for programming and text modes
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'text-mode-hook #'linum-mode)

(tool-bar-mode -1)       ; disable gui emacs toolbar
(menu-bar-mode -1)       ; disable menu bar
(scroll-bar-mode -1)     ; disable gui scrollbar
(line-number-mode t)     ; add line numbers to status bar
(visual-line-mode t)     ; Toggles visual line mode: wraps lines and tweaks some commands accordingly

;; ----- ERGONOMICS -----

(global-set-key (kbd "M-o") 'other-window)

;; More vim like binding for for forward-paragraph and backward-paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; ergonomic join-line binding
(global-set-key (kbd "C-x j") 'join-line)

;; Binding for comment-region
(global-set-key (kbd "C-#") 'comment-region)

;; alternative to M-x
;;(global-set-key (kbd "C-<tab>") 'counsel-M-x)

;;; Fix for os x emacs not being able to set org timer sound b/c of dbus issue

;; Terminal notifier
;; requires 'brew install terminal-notifier'
;; stolen from erc-notifier

(add-hook 'prog-mode-hook 'show-paren-mode)

;;; -------- Packages --------

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-dracula))
  ;; :config (load-theme 'doom-flatwhite))
  ;; :config (load-theme 'doom-nord))
  ;; :config (load-theme 'doom-zenburn))

;; TODO how to get modeline to play nice with this theme
;; (use-package nano-theme
;;   :ensure t
;;   :config (nano-light))

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

;; ivy functions replicating many common helm features
(use-package counsel
  :ensure t
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu)
         ("C-c f" . counsel-flycheck)
         ("C-x C-r" . counsel-recentf))
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
  :after (ivy)
  :demand t
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

;; project based operations
(use-package projectile
  :ensure t
  :config (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :defer nil
  ;; :after (counsel projectile)
  :bind (("C-x /" . counsel-projectile-rg)
         ("C-x f" . counsel-projectile-find-file)
         ("C-x p" . counsel-projectile-switch-project)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :bind (("<backtab>" . avy-goto-line)
         ("C-<tab>" . avy-goto-char-timer)
         ("C-_" . avy-goto-line)
         ("M-_" . avy-goto-char-in-line)))

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
  (cider-repl-toggle-pretty-printing)
  (setq cider-enrich-classpath nil))
        ;; disable eldoc at point for cider in favor of lsp
        ;; cider-eldoc-display-for-symbol-at-point nil))


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
;; (use-package markdown-mode
;;   :ensure t
;;   :defer t)

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

;; Hook clojure linter into flycheck
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
  :defer t
  :bind (("C-x RET" . vterm-other-window)))


;; Distributed collaboration using conflict-free replicated data types
(use-package crdt
  :ensure t)

;; Formats work clojure projects, needs manual triggering
(use-package zprint-format
  :ensure t
  :defer t)

;; Better window navigation and swapping
(use-package ace-window
  :ensure t
  ; :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?t ?n ?l ?a ?o ?e ?u))
  (ace-window-display-mode 1))

;; Resize windows automatically to intelligently utilize space
;; TODO: Add fn/binding to switch between golden ration and 50/50 ratios
(use-package zoom
  :ensure t
  :config
  (zoom-mode)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package csv-mode
  :defer t)

(use-package lsp-mode
  :ensure t
  ;; Disabled until performance issues are addressed
  ;; :hook ((clojure-mode . lsp)
  ;;        (clojurescript-mode . lsp)
  ;;        (clojurec-mode . lsp))
  :config
  (lsp-enable-which-key-integration t)
  (setq gc-cons-threshold (* 100 1024 1024)
        ;; treemacs-space-between-root-nodes nil
        ;; lsp-lens-enable t
        company-minimum-prefix-length 2))

;; (use-package lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;;; --- EVIL ---

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search     ; better search mode than default
        evil-ex-complete-emacs-commands nil ; don't complete for emacs commands in ex-mode
        evil-want-keybinding nil            ; disable default evil bindings for non prog/text modes (needed for evil-collection)
        ;; v dwis. Temporary measure until my evil-stuff is more polished
        ;; evil-disable-insert-state-bindings t)
        )
  :config
  (evil-mode 1)
  (evil-set-leader `(normal visual) (kbd "SPC"))
  (evil-define-key `(normal visual) 'global
    "Y" "y$" ; Capital Y yanks to end of line
    "Q" "@q" ; Q triggers macro in q register
    "L"                 'evil-last-non-blank   ; faster beginning of line navigation
    "H"                 'evil-first-non-blank  ; faster end of line navigation
    (kbd "C-y")         'evil-paste-before    ; mimic default emacs yank binding (useful for putting in swiper)
    (kbd "<leader>l")   'avy-goto-line         ; quick line jumping
    (kbd "<leader>.")   'avy-goto-char-timer   ; char jump on timer
    (kbd "<leader>'")   'avy-goto-line        ; jump to line
    (kbd "<leader>,")   'avy-goto-symbol-1    ; 1 char symbol jump
    ;; (kbd "<leader>SPC") '
    (kbd "<leader>\\")  'evil-ex-nohighlight   ; clear highlighting from / searches
    (kbd "<leader>F")   'counsel-flycheck      ; show errors/warnings in a search minibuffer
    (kbd "<leader>A")   'align-regexp          ; align region by regular expresion
    (kbd "<leader>o")   'ace-window            ; change windows with hints
    (kbd "<leader>b")   'ivy-switch-buffer     ; ivy change buffer menu
    (kbd "<leader>x")   'counsel-M-x           ; better m-x
    (kbd "<leader>y")   'counsel-yank-pop      ; paste from a search menu of recent kills
    (kbd "<leader>L")   'counsel-load-theme    ; select and load theme
    (kbd "<leader>t")   'vterm-other-window    ; open terminal in other window
    (kbd "<leader>T")   'projectile-run-vterm ; open terminal in this window, at project root
    (kbd "<leader><")   'smerge-keep-upper    ; Keep upper merge chunk
    (kbd "<leader>>")   'smerge-keep-lower)   ; Keep lower merge chunk
  ;; Better visual indication of mode (at where I'm actually looking when editing)
  (setq evil-insert-state-cursor '((bar . 2) "red")
        evil-normal-state-cursor '(box "green")
        evil-visual-state-cursor '(box "blue")))

;; cs"' <- change surrounding double quotes to single quotes.
(use-package evil-surround
  :ensure t
  :after (evil)
  :config (global-evil-surround-mode 1))

;; f/t highlight unique letters in line for faster line-wise jumping
(use-package evil-quickscope
  :ensure t
  :after (evil)
  :config (global-evil-quickscope-always-mode 1))

(use-package evil-commentary
  :ensure t
  :after (evil)
  :config (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config
  (evil-collection-init))

;; Load vcs excluded secrets elisp file if it exists
(let ((secrets "~/.emacs.d/secrets.el"))
  (when (file-exists-p secrets)
    (load-file secrets)))

;; https://www.reddit.com/r/emacs/comments/sn0xrd/screenwriting_with_fountainmode_and_olivettimode/
;; (use-package fountain-mode
;;   :ensure t)
;; (use-package olivetti
;;   :ensure t)

;; Org-roam mode for better note-taking (Graph style??? :)
;; https://jethrokuan.github.io/org-roam-guide/

;; C-s, query string, C-c C-o to open results in buffer, wgrep and edit all at once
(use-package wgrep
  :ensure t
  :config (setq wgrep-change-readonly-file t))
