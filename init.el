;;; init.el --- Cooper LeBrun

;;; -- initially cribbed from: https://github.com/suvratapte/dot-emacs-dot-d/blob/e7f44d981004b44bb9ae23b6b67c421404ea6b4e/init.el#L19 --
;;; Further inspo from https://github.com/Crandel/home/blob/master/.config/emacs/init.el

;;; Code:

;;; USE-PACKAGE

(require 'package)

;; add melpa to package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; load and activate emacs packages. Done first to ensure packages are loaded before they are modified later. This also sets the load path
(package-initialize)

;; Install 'use-package' if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; ensure packages by default. Use :ensure nil to override this.
(setq use-package-always-ensure t)

;; Add unit files to path
(add-to-list 'load-path (expand-file-name "units/" user-emacs-directory))

;;; MY CONFIG UNITS

(require 'my-prelude) ; load utility functions / libs
(require 'base)
(require 'my-theme)
(require 'my-hydra)
(require 'my-evil)
(require 'my-git)
(require 'my-which-key)
(require 'my-ivy)
(require 'my-projectile)
(require 'my-clojure-mode)
(require 'my-parinfer)

;; (require 'my-smartparens)
;; (require 'my-lispy)
;; (require 'my-paredit)
;; (require 'my-parinfer)

;; (require 'my-yasnippet)
;; (require 'my-perspective)

;; Visual jump hints a la Easymotion
(use-package avy
  :ensure t)
  ;; :bind (("<backtab>" . avy-goto-line)
  ;;        ("C-<tab>" . avy-goto-char-timer)
  ;;        ("C-_" . avy-goto-line)
  ;;        ("M-_" . avy-goto-char-in-line)))

;; Package for markdown files, mostly used for syntax highlighting and org-mode style folding
;; (use-package markdown-mode
;;   :ensure t
;;   :defer t)

;; Automatic code linting
(use-package flycheck
  :ensure t
  :after (exec-path-from-shell)
  :init (global-flycheck-mode))

;; Magical auto completion
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-mode-minimum-prefix-length 1))

;; Magical emacs terminal finally (shamelessly cribbed from derek passen)
(use-package vterm
  :ensure t
  :defer t
  :bind (("C-x RET" . vterm-other-window)))

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
   '(zoom-size '(0.6 . 0.6))))

;; (use-package lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

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
;; Grab consul token for comcast from normal zsh env
(setenv "CONSUL_TOKEN" (shell-command-to-string ". ~/.zshrc; echo -n $CONSUL_TOKEN"))
