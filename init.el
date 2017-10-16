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
;; [ ] Robe
;; [x] helm bookmarks
;; [ ] helm for evil-leader
;; [ ] org-mode
;; [ ] magit
;; [ ] projectile
;; [x] emacs-client/daemon
;; [ ] polish client/daemon config
;; [x] gui emacs


(setq load-path (cons "~/.emacs.d/vendor" load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
			("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
			("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; THEMES
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;; EVIL
(require-package 'evil)
(evil-mode t)

(defun nmap (trigger action)
  (define-key evil-normal-state-map (kbd trigger) action))
(defun vmap (trigger action)
  (define-key evil-visual-state-map (kbd trigger) action))

;; evil-leader
(require-package 'evil-leader)
(setq evil-leader/leader ",")
(global-evil-leader-mode)
;; (setq evil-leader/in-all-states 1)

;; evil-powerline
(require-package 'powerline)
(require-package 'powerline-evil)
(powerline-evil-vim-color-theme)

;; evil-surround
(require-package 'evil-surround)
(global-evil-surround-mode t)

;; evil-tabs
(require-package 'evil-tabs)
(global-evil-tabs-mode t)

;; Disable evil in these modes
(dolist (mode '(dired-mode
		flycheck-error-list-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;; AVY
(require-package 'avy)

;; FLYCHECK
(require-package 'flycheck)
(global-flycheck-mode)
;; flycheck-pos-tip
(require-package 'flycheck-pos-tip)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

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
(require-package 'enh-ruby-mode)
(add-to-list 'auto-mode-alist
	     '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

;; HELM
(require-package 'helm)
(require 'helm-config)
(helm-mode 1)
(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAPPINGS

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
  "RET" 'align-regexp
  "b" 'helm-buffers-list
  "B" 'helm-bookmarks
  "k" 'helm-show-kill-ring
  "f" 'helm-find-files
  "T" 'text-scale-adjust
  "<up>" 'enlarge-window
  "<down>" 'shrink-window
  "<left>" 'shrink-window-horizontally
  "<right>"  'enlarge-window-horizontally
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

;;;; dired
(define-key dired-mode-map "j" 'dired-next-line)
(define-key dired-mode-map "k" 'dired-previous-line)
(define-key dired-mode-map "q" 'kill-this-buffer)
(define-key dired-mode-map "b" 'helm-buffers-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS

(global-linum-mode t)				; enable line numbers
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
    (flycheck-pos-tip helm evil-tabs enh-ruby-mode rcodetools flycheck powerline powerline-evil evil-powerline evil-surround zenburn-theme avy evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; supposed to help with passphrase hanging issue
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
