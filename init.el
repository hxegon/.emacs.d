;; Vim config migration TODOS
;; SURVIVAL LEVEL CONFIG
;; [x] Evil
;; [x] , leader
;; [x] config quick edit/resource
;; [x] mode indicator
;; [x] H and L remap
;; [x] ,w window nav
;; [x] easymotion
;; [x] zenburn or at least decent colorscheme
;; [x] surround
;; [x] line numbers
;; [x] powerline
;; [x] align command
;; [x] nohlsearch
;; [x] asynch linting/error messages
;; [x] # comment shortcut
;; [x] ruby 2 space indentation
;; [x] simple file find
;; [x] tabs
;; [x] helm
;; [x] window resize bindings
;; [x] Server workflow (TRAMP + term)
;;     - [x] term
;;     - [x] tramp
;; [x] stop making garbage files everywhere!

;; Eventually check out/learn/integrate?
;; [x] flycheck tooltips
;; [x] Spacemacs style show bindings (in some scenarios)
;; [-] Robe (Not worth, unmaintained... undocumented... :(), still better than nvim with magit, gui, use-package, elisp, tramp)
;; [x] helm bookmarks
;; [ ] helm for evil-leader
;; [x] org-mode
;; [x] magit
;; [ ] projectile
;; [x] emacs-client/daemon
;; [ ] polish client/daemon config
;; [x] gui emacs
;; [ ] better delimiter (paren) manipulation
;; [x] rainbow delimiters
;; [x] ruby-refactor
;; [ ] fix xmp binding
;; [ ] org-mode archive setup

(setq load-path (cons "~/.emacs.d/vendor" load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
			("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
			("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; always install missing packages
(setq use-package-always-ensure t)

;; THEMES
(use-package zenburn-theme)

;; EVIL
(use-package evil
  :config
  (evil-mode 1))

;; evil-leader
(use-package evil-leader
  :config
  (setq evil-leader/leader ",")
  (global-evil-leader-mode))

;; evil-powerline
(use-package powerline
  :config (powerline-evil-vim-color-theme))

;; evil-surround
(use-package evil-surround
  :defer t
  :config (global-evil-surround-mode t))

;; evil-tabs
(use-package evil-tabs
  :defer t
  :config (global-evil-tabs-mode t))

;; AVY
(use-package avy
  :defer t)

;; FLYCHECK
(use-package flycheck
  :defer t
  :config
  (global-flycheck-mode))

;; flycheck-pos-tip
(use-package flycheck-pos-tip
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;; RUBY
;;; rcodetools
(load "rcodetools")
(require 'rcodetools)
(defun auto-xmp ()
  "Add xmp comment and run xmp!"
  (interactive)
  (end-of-line)
  (insert " #=>")
  (xmp))

;; enh-ruby-mode
(use-package enh-ruby-mode
  :defer t
  :config
  :mode ("\\.rb\\'" "\\.ru\\'" "\\.rake\\'" "\\.thor\\'" "\\.jbuilder\\'" "\\.gemspec\\'" "\\.podspec\\'" "\\.Gemfile\\'" "\\.Rakefile\\'" "\\.Capfile\\'" "\\.Thorfile\\'" "\\.Vagrantfile\\'" "\\.Guardfile\\'" "\\.Podfile\\'"))

;; ruby-refactor
(use-package ruby-refactor
  :defer t
  :config
  (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch))

;; HELM
(use-package helm
  :defer t
  :config
    (require 'helm-config)
    (helm-mode 1)
    (define-key helm-find-files-map "\t" 'helm-execute-persistent-action))

;; MAGIT
(use-package magit
  :defer t
  :config
  (require 'evil-magit)
  (setq evil-magit-state 'normal)
  (add-hook 'magit-mode-hook 'evil-local-mode)
  (add-hook 'git-rebase-mode-hook 'evil-local-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :config (which-key-mode))

(use-package htmlize
  :defer t)

;; org-mode
(use-package org
  :defer t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((ruby . t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS

;; evil related:
(defun nmap (trigger action)
  (define-key evil-normal-state-map (kbd trigger) action))
(defun vmap (trigger action)
  (define-key evil-visual-state-map (kbd trigger) action))
;;; evil-leader 'lamdas', turned into functions to play nice with emacs
(defun load-dot-emacs ()
  "loads ~/.emacs.d/init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(defun launch-term ()
  "launches a zsh term"
  (interactive)
  (term "/bin/zsh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAPPINGS

;; global
(global-set-key (kbd "M-x") 'helm-M-x)
(windmove-default-keybindings)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; evil-leader bindings
;; TODO: change lamdas to custom F() so they play nice with helm/errors
;; TODO: Consolidate specific file bindings with bookmarks
(evil-leader/set-key
  "," 'save-buffer
  "." 'repeat
  "E" 'load-dot-emacs
  "z" 'suspend-emacs
  "x" 'helm-M-x
  "w" evil-window-map
  "<return>" 'align-regexp
  "b" 'helm-buffers-list
  "B" 'helm-bookmarks
  "R" 'helm-recentf
  "k" 'helm-show-kill-ring
  "f" 'helm-find-files
  "t" 'launch-term
  "T" 'text-scale-adjust
  "<up>" 'enlarge-window
  "<down>" 'shrink-window
  "<left>" 'shrink-window-horizontally
  "<right>"  'enlarge-window-horizontally
  "gs" 'magit-status
  )

(nmap "H" (kbd "g^"))
(nmap "L" (kbd "g_"))
(vmap "H" (kbd "g^"))
(vmap "L" (kbd "g_"))
(nmap "SPC" 'avy-goto-char)
(nmap "#" 'comment-line)
(vmap "#" 'comment-dwim)
(nmap "Y" (kbd "y$"))

;;; mode specific
;;;; ruby
(evil-leader/set-key-for-mode 'enh-ruby-mode
  "rm" 'ruby-refactor-extract-to-method
  "rv" 'ruby-refactor-extract-local-variable
  "rp" 'ruby-refactor-convert-post-conditional
  "#" 'auto-xmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS

(add-hook 'prog-mode-hook 'linum-mode)		; add line numbers for prog and text files
(add-hook 'text-mode-hook 'linum-mode)
(setq linum-format "%4d ")			; nicer line number format
(toggle-scroll-bar -1)				; remove gui scrollbars
(menu-bar-mode -1)				; remove gui menu bar
(tool-bar-mode -1)				; remove gui menu bar
(setq ring-bell-function 'ignore)		; Disable annoying bell sound
(setq auto-save-default nil)			; disable #auto-save-files#
(setq make-backup-files nil)			; disabel backupfiles~
(setq tramp-default-method "ssh")		; use ssh by default for tramp
(put 'dired-find-alternate-file 'disabled nil)	; re-enable a command in dired
(setq org-hide-leading-stars t)			; only show last star and good indent
(recentf-mode 1)				; enable recent file tracking
(setq recentf-max-menu-items 25)		; limit to 25
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-agenda-files '("~/Org/inbox.org"
                         "~/Org/active.org"
                         "~/Org/tickler.org"))
(setq org-refile-targets '(("~/Org/active.org" :maxlevel . 3)
                           ("~/Org/someday.org" :maxlevel . 1)
                           ("~/Org/tickler.org" :maxlevel . 2)))
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Org/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(setq org-agenda-span 14) ; change agenda display period to two weeks
(advice-add 'org-refile :after ; advise to save org buffers after refile
        (lambda (&rest _)
        (org-save-all-org-buffers)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC FIXES
;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package magit evil-magit flycheck-pos-tip helm evil-tabs enh-ruby-mode rcodetools flycheck powerline powerline-evil evil-powerline evil-surround zenburn-theme avy evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; supposed to help with passphrase hanging issue
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
