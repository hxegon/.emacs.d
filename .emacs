(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; BEGIN ENSURE PACKAGE INSTALLS
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if  (package-installed-p package)
	 nil
       (if y-or-n-p (format "Package %s is missing. Install it? " package) ; If you get a invalid Symbol variable y-or-n-p, it might mean that you don't have a package installed and the ensurer function isn't working
	 (package-install package)
	 package)))
   packages))

;; ensure archive description is downloaded
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; END ENSURE PACKAGE INSTALLS

;; (setq package-enable-at-startup nil)
(package-initialize)
(ensure-package-installed 'evil
			  'evil-leader
			  'evil-tabs
			  'helm
			  'helm-projectile
			  'which-key
			  'color-theme
			  'flycheck
			  'flycheck-pos-tip
			  'robe
			  'ruby-refactor
			  'auto-complete
			  'avy
			  'rainbow-delimiters
			  'powerline
			  'powerline-evil
			  'evil-surround) ; 'magit 'evil-magit

(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(require 'magit)
(require 'evil-magit)

(require 'helm)
(helm-mode 1)
(projectile-mode)

(which-key-mode)

;;; EVIL MODE :lightning:
(require 'evil)
(evil-mode t)
(global-evil-tabs-mode t)
(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode)
(setq evil-leader/in-all-states 1)

; leader bindings
(evil-leader/set-key
  "v" (lambda () (interactive) (find-file "~/.emacs.d/.emacs"))
  "V" (lambda () (interactive) (load-file "~/.emacs.d/.emacs"))
  "p" 'helm-projectile
  "m" 'helm-M-x ; M-x shortcut
  "b" 'helm-buffers-list
  "w" 'whwmitespace-mode
  "d" 'robe-ask ; TODO: generalize <leader>d to doc lookup key in whatever language, not just ruby.
  "gs" 'dmagit-status
  "SPC" 'avy-goto-char
  "RET" 'align-regexp
  "r" (lambda () (interactive) (execute-extended-command "ruby-refactor"))
  ) 
; add 'e' mapping for terminal ; which one? eshell?
; # mapping for commenting

; normal bindings
(defun nmap (trigger action)
  (define-key evil-normal-state-map (kbd trigger) action))
; visual bindings
(defun vmap (trigger action)
  (define-key evil-visual-state-map (kbd trigger) action))

(nmap "H" (kbd "g^"))
(vmap "H" (kbd "g^"))
(nmap "L" (kbd "g_"))
(vmap "L" (kbd "g_"))

(require 'powerline)
(require 'powerline-evil)
(powerline-evil-vim-color-theme)
(display-time-mode t)

(require 'evil-surround)
(global-evil-surround-mode t)
;;; EVIL MODE END

(global-flycheck-mode) ; syntax checking enable

(ac-config-default) ; auto-complete enable

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; RUBY STUFF
(require 'ruby-refactor)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;;; COLORSCHEMES
(load-theme 'wombat)
(global-linum-mode)

(show-paren-mode t)

;;;; BEGIN CUSTOM FUNCTIONS

;(defun align-interactive (user-input)
  ;"opens a prompt, takes a user string, turns that into a regex and aligns on it."
  ;)

;;;; END CUSTOM FUNCTIONS
