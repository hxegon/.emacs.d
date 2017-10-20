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
;; [ ] Spacemacs style show bindings (in some scenarios)
;; [-] Robe (Not worth, unmaintained... undocumented... :(), still better than nvim with magit, gui, use-package, elisp, tramp)
;; [x] helm bookmarks
;; [ ] helm for evil-leader
;; [ ] org-mode
;; [x] magit
;; [ ] projectile
;; [x] emacs-client/daemon
;; [ ] polish client/daemon config
;; [x] gui emacs
;; [ ] smartparens
;; [x] rainbow delimiters

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

(defun nmap (trigger action)
  (define-key evil-normal-state-map (kbd trigger) action))
(defun vmap (trigger action)
  (define-key evil-visual-state-map (kbd trigger) action))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAPPINGS

;; global
(global-set-key (kbd "M-x") 'helm-M-x)
(windmove-default-keybindings)

;; evil-leader bindings
;; TODO: change lamdas to custom F() so they play nice with helm/errors
;; TODO: Consolidate specific file bindings with bookmarks
(evil-leader/set-key
  "," 'save-buffer
  "." 'repeat
  "E" (lambda () (interactive) (load-file "~/.emacs.d/init.el"))
  "z" 'suspend-emacs
  "x" 'helm-M-x
  "w" evil-window-map
  "<return>" 'align-regexp
  "b" 'helm-buffers-list
  "B" 'helm-bookmarks
  "k" 'helm-show-kill-ring
  "f" 'helm-find-files
  "t" (lambda () (interactive) (term "/bin/zsh"))
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
(setq auto-make-backup nil)			; disabel backupfiles~
(setq tramp-default-method "ssh")		; use ssh by default for tramp
(put 'dired-find-alternate-file 'disabled nil)	; re-enable a command in dired

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS

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
